# ==============================================================================
# NFL Analytics Toolkit -- Season 3
# Rookie Research Module
# File: R/40_rookie_research.R
#
# PURPOSE
# -------
# Tools for reasoning about an incoming rookie class against historical college
# signal. This module is three independent components, each a plain function
# returning a data frame so it can be called from an article script for figures
# and from the Shiny rookie tab for exploration:
#
#   (1) find_lift_thresholds()   lift-first threshold / bucket finder   [THIS FILE]
#   (2) compare_hit_profiles()   hit-vs-miss median feature contrast    [THIS FILE]
#   (3) find_comparables()       nearest-neighbor comparable finder     [THIS FILE]
#
# COMPONENT 1: find_lift_thresholds()
# -----------------------------------
# The original Databricks marker search used a flat 70% HIT_THRESHOLD and found
# nothing, because no single college metric pocket reaches a 70% hit rate when
# the position base rates are QB ~19%, RB ~40%, WR ~21%, TE ~17%. The fix is
# lift-first: for each college metric, sweep candidate cutoffs, split players
# into a favorable pocket and the rest, and ask whether the pocket's hit rate
# beats the position base rate by enough that the gap survives a small-sample
# guard. We do NOT require an absolute hit-rate floor.
#
# The guard is a Wilson lower confidence bound on the favorable pocket's hit
# rate. A pocket passes only if that lower bound sits above the position base
# rate AND the pocket has at least `min_group` players. The Wilson interval
# widens automatically as the pocket shrinks, so a 3-of-3 pocket cannot pass on
# its raw 100% rate. A two-proportion test p-value is reported alongside as a
# descriptive column, but it does NOT gate anything: the cutoff sweep is a large
# multiple-comparison search and a raw p < 0.05 would overfire.
#
# DRAFT CAPITAL IS EXCLUDED ON PURPOSE
# ------------------------------------
# draft_round and draft_pick are not swept. "Early picks hit more" is true and
# circular, and would swamp the production-signal markers the article is about.
# draft_age IS included as an eligible marker (younger-at-draft is a legitimate
# production-context signal, not draft capital); the two-direction sweep handles
# its "lower is better" shape automatically.
#
# INPUTS
# ------
#   scored        R/28 final prospect scores. Either an in-memory data frame or
#                 a path to s2_week14_final_prospect_scores.csv. Must contain
#                 position, is_hit, is_training_player, and the raw college
#                 metric columns (each accompanied by a <metric>_tier column,
#                 which is how R/28 marks a metric as usable). nfl_gsis_id is
#                 required only when features_path is supplied (for the join).
#   features_path Optional path to s2_week13_feature_matrix_full.csv. Used ONLY
#                 to left-join the single column draft_age by nfl_gsis_id, since
#                 R/28 does not export draft_age. If NULL, draft_age is simply
#                 absent from the metric universe (with a message).
#
# OUTPUT (one tibble, full sweep; not written to disk by this function)
# ---------------------------------------------------------------------
#   position, metric, direction, cutoff, n_favorable, n_other,
#   rate_favorable, rate_other, base_rate, lift_abs, lift_rel,
#   wilson_lower, p_two_prop, passes_guard, is_best, n_total
#
#   direction  "high" = favorable pocket is values >= cutoff
#              "low"  = favorable pocket is values <  cutoff
#   base_rate  position hit rate over the full training pool (the reference)
#   is_best    TRUE on the single highest-lift cutoff per position+metric that
#              passes the guard; FALSE everywhere else (FALSE for all rows of a
#              metric when no cutoff qualifies)
#
# DEPENDENCIES: dplyr, readr, tibble, purrr, glue, stats. None sourced.
#
# EXAMPLE
# -------
#   source("R/40_rookie_research.R")
#   sweep <- find_lift_thresholds(
#     scored        = here::here("data/season2_cache/s2_week14_final_prospect_scores.csv"),
#     features_path = here::here("data/season2_cache/s2_week13_feature_matrix_full.csv")
#   )
#   dplyr::filter(sweep, is_best)            # one headline marker per metric
#
# SCHEMA TAG: s3_r40_v1
# Author    : Christian LeBlanc
# Created   : 2026-06
# ==============================================================================


# ==============================================================================
# LIBRARIES
# ==============================================================================

library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(glue)


# ==============================================================================
# CONFIGURATION
# ==============================================================================

R40_SCHEMA_TAG <- "s3_r40_v1"

POSITIONS_DEFAULT <- c("QB", "RB", "WR", "TE")

# Minimum complete cases (metric present AND outcome present) before a metric is
# eligible at a position. Mirrors R/28's N_TIERS * MIN_BIN_SIZE = 5 * 5.
MIN_METRIC_N <- 25L

# Columns that must never be treated as a sweepable college metric, even if they
# somehow carry a <col>_tier companion. Draft capital is excluded by design;
# the rest are scores, outcomes, flags, and identity. draft_age is deliberately
# NOT in this list.
NEVER_METRIC <- c(
  "score_v1", "score_final", "score_base", "score_enriched",
  "enriched_improvement_pct", "ppr_per_game_y13", "qualifying_seasons",
  "draft_round", "draft_pick", "is_hit", "is_training_player", "sos_imputed",
  "draft_year", "nfl_gsis_id", "gsis_id",
  # NFL career-peak descriptors: outcome-side, not pre-draft signal. Sweeping
  # them as markers is circular against is_hit. cfb_peak_ppr_per_game is the
  # pre-draft analogue and IS a legitimate sweepable college metric.
  "peak_ppr_per_game", "peak_career_year"
)


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

# ------------------------------------------------------------------------------
# .wilson_lower
#
# Lower bound of the Wilson score interval for a binomial proportion. Returns
# NA_real_ when n is zero. Clamped to [0, 1]. Conservative for small n, which is
# exactly why it is the guard: a tiny high-rate pocket gets a low lower bound.
# ------------------------------------------------------------------------------
.wilson_lower <- function(x, n, conf_level = 0.95) {
  if (is.na(n) || n <= 0L) return(NA_real_)
  z       <- stats::qnorm(1 - (1 - conf_level) / 2)
  p_hat   <- x / n
  denom   <- 1 + z^2 / n
  centre  <- (p_hat + z^2 / (2 * n)) / denom
  margin  <- (z / denom) * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))
  lower   <- centre - margin
  max(0, min(1, lower))
}

# ------------------------------------------------------------------------------
# .two_prop_p
#
# Two-sided p-value for the difference in hit rate between the favorable pocket
# and the rest. Uses prop.test; falls back to Fisher's exact test on the 2x2
# when prop.test cannot run (e.g., a degenerate margin). Descriptive only:
# never used to gate a marker. Warnings (small expected counts) are suppressed
# because the value is reported, not thresholded.
# ------------------------------------------------------------------------------
.two_prop_p <- function(hits_fav, n_fav, hits_other, n_other) {
  if (n_fav <= 0L || n_other <= 0L) return(NA_real_)
  out <- tryCatch(
    suppressWarnings(
      stats::prop.test(
        x = c(hits_fav, hits_other),
        n = c(n_fav,    n_other)
      )$p.value
    ),
    error = function(e) NA_real_
  )
  if (is.na(out)) {
    tab <- matrix(
      c(hits_fav, n_fav - hits_fav, hits_other, n_other - hits_other),
      nrow = 2, byrow = TRUE
    )
    out <- tryCatch(
      suppressWarnings(stats::fisher.test(tab)$p.value),
      error = function(e) NA_real_
    )
  }
  out
}

# ------------------------------------------------------------------------------
# .resolve_scored
#
# Accept either a data frame or a path to the R/28 CSV. Verify the columns the
# sweep depends on actually exist before any downstream code references them.
# ------------------------------------------------------------------------------
.resolve_scored <- function(scored) {
  if (is.character(scored)) {
    if (length(scored) != 1L || !file.exists(scored)) {
      stop(glue("find_lift_thresholds(): scored file not found: {scored}"),
           call. = FALSE)
    }
    scored <- readr::read_csv(scored, show_col_types = FALSE)
  }
  if (!is.data.frame(scored)) {
    stop("find_lift_thresholds(): `scored` must be a data frame or a file path.",
         call. = FALSE)
  }
  if (!"position" %in% names(scored)) {
    stop("find_lift_thresholds(): `scored` is missing required column 'position'.",
         call. = FALSE)
  }
  scored
}

# ------------------------------------------------------------------------------
# .join_draft_age
#
# Left-join the single column draft_age from the R/27 feature matrix on
# nfl_gsis_id. Returns the data frame unchanged (with a message) when the path
# is NULL or the required columns are absent, so the caller degrades gracefully
# instead of erroring.
# ------------------------------------------------------------------------------
.join_draft_age <- function(scored, features_path) {
  if (is.null(features_path)) {
    message("  draft_age: features_path not supplied -- draft_age excluded ",
            "from the metric universe.")
    return(scored)
  }
  if (!file.exists(features_path)) {
    stop(glue("find_lift_thresholds(): features_path not found: {features_path}"),
         call. = FALSE)
  }
  if (!"nfl_gsis_id" %in% names(scored)) {
    stop("find_lift_thresholds(): draft_age join needs nfl_gsis_id in `scored`, ",
         "but it is absent.", call. = FALSE)
  }
  if ("draft_age" %in% names(scored)) {
    message("  draft_age: already present in `scored` -- skipping feature-matrix join.")
    return(scored)
  }

  feats <- readr::read_csv(features_path, show_col_types = FALSE)
  need  <- c("nfl_gsis_id", "draft_age")
  miss  <- setdiff(need, names(feats))
  if (length(miss) > 0L) {
    stop(glue(
      "find_lift_thresholds(): feature matrix missing column(s): ",
      "{paste(miss, collapse = ', ')}. Cannot join draft_age."
    ), call. = FALSE)
  }

  age_lookup <- feats |>
    dplyr::select(dplyr::all_of(need)) |>
    dplyr::filter(!is.na(.data$nfl_gsis_id)) |>
    dplyr::distinct(.data$nfl_gsis_id, .keep_all = TRUE)

  out <- dplyr::left_join(scored, age_lookup, by = "nfl_gsis_id")
  n_matched <- sum(!is.na(out$draft_age))
  message(glue("  draft_age: joined from feature matrix -- ",
               "{n_matched} / {nrow(out)} rows have a value."))
  out
}

# ------------------------------------------------------------------------------
# .derive_metric_universe
#
# A column is a sweepable college metric when it is numeric and has a companion
# <col>_tier column (R/28's marker for a usable metric), excluding NEVER_METRIC.
# draft_age is added explicitly when present, since R/28 never tiers it.
# ------------------------------------------------------------------------------
.derive_metric_universe <- function(scored) {
  nm        <- names(scored)
  has_tier  <- nm[paste0(nm, "_tier") %in% nm]
  numeric_ok <- has_tier[purrr::map_lgl(has_tier, ~ is.numeric(scored[[.x]]))]
  metrics   <- setdiff(numeric_ok, NEVER_METRIC)

  if ("draft_age" %in% nm && is.numeric(scored[["draft_age"]])) {
    metrics <- union(metrics, "draft_age")
  }
  sort(metrics)
}

# ------------------------------------------------------------------------------
# .sweep_one_metric
#
# Sweep candidate cutoffs for a single metric within a single position's
# training pool. `vals` and `hits` are aligned vectors already filtered to
# complete cases (metric present, outcome present). `base_rate` and `n_total`
# are the position-level reference computed by the caller on the full pool.
#
# For each unique candidate cutoff we emit up to two rows: the high side
# (vals >= cutoff favorable) and the low side (vals < cutoff favorable). Both
# share the same partition; only the labeling of which side is "favorable"
# differs. A cutoff that leaves either side empty is skipped.
# ------------------------------------------------------------------------------
.sweep_one_metric <- function(vals, hits, position, metric, base_rate,
                              n_total, candidate_probs, min_group, conf_level) {

  cutoffs <- unique(stats::quantile(vals, probs = candidate_probs,
                                    na.rm = TRUE, names = FALSE))
  rows <- list()

  for (cut in cutoffs) {
    high_idx <- vals >= cut
    low_idx  <- !high_idx          # vals < cut

    n_high <- sum(high_idx)
    n_low  <- sum(low_idx)
    if (n_high == 0L || n_low == 0L) next  # cannot form two groups

    for (dir in c("high", "low")) {
      fav_idx <- if (dir == "high") high_idx else low_idx
      oth_idx <- !fav_idx

      n_fav   <- sum(fav_idx)
      n_oth   <- sum(oth_idx)
      h_fav   <- sum(hits[fav_idx])
      h_oth   <- sum(hits[oth_idx])

      rate_fav <- h_fav / n_fav
      rate_oth <- h_oth / n_oth
      w_lower  <- .wilson_lower(h_fav, n_fav, conf_level)

      rows[[length(rows) + 1L]] <- tibble::tibble(
        position       = position,
        metric         = metric,
        direction      = dir,
        cutoff         = cut,
        n_favorable    = n_fav,
        n_other        = n_oth,
        rate_favorable = rate_fav,
        rate_other     = rate_oth,
        base_rate      = base_rate,
        lift_abs       = rate_fav - base_rate,
        lift_rel       = if (base_rate > 0) rate_fav / base_rate else NA_real_,
        wilson_lower   = w_lower,
        p_two_prop     = .two_prop_p(h_fav, n_fav, h_oth, n_oth),
        passes_guard   = (n_fav >= min_group) &
                         !is.na(w_lower) &
                         (w_lower > base_rate),
        n_total        = n_total
      )
    }
  }

  if (length(rows) == 0L) return(NULL)
  dplyr::bind_rows(rows)
}

# ------------------------------------------------------------------------------
# .pick_best
#
# Given the guard flag and the ranking columns for the rows of one position +
# metric group, return a logical vector with exactly one TRUE on the best
# qualifying row, or all FALSE when no row passes the guard. Best = highest
# lift_abs, ties broken by higher wilson_lower, then larger favorable pocket.
# Explicit on purpose: handles the no-qualifier and tie cases without clever
# rank arithmetic.
# ------------------------------------------------------------------------------
.pick_best <- function(passes_guard, lift_abs, wilson_lower, n_favorable) {
  out  <- rep(FALSE, length(passes_guard))
  cand <- which(passes_guard)
  if (length(cand) == 0L) return(out)
  ord <- cand[order(
    -lift_abs[cand],
    -wilson_lower[cand],
    -n_favorable[cand]
  )]
  out[ord[1L]] <- TRUE
  out
}


# ==============================================================================
# EXPORT: find_lift_thresholds
# ==============================================================================

#' Lift-first college-metric threshold finder
#'
#' For each position and each eligible college metric, sweeps candidate cutoffs
#' and reports the favorable pocket's hit rate, its lift over the position base
#' rate, group sizes, a Wilson lower-bound guard, and a descriptive
#' two-proportion p-value. Returns the full sweep with an is_best flag.
#'
#' @param scored R/28 prospect scores: data frame or path to the CSV.
#' @param features_path Optional path to the R/27 feature matrix, used only to
#'   left-join draft_age. NULL drops draft_age from the universe.
#' @param positions Character vector of positions to evaluate.
#' @param metrics Optional explicit metric vector. NULL auto-derives.
#' @param hit_col Name of the 0/1 outcome column. Default "is_hit".
#' @param training_only Restrict to is_training_player == TRUE. Default TRUE.
#' @param candidate_probs Quantile grid for candidate cutoffs.
#' @param min_group Minimum favorable-pocket size to qualify. Default 10.
#' @param conf_level Confidence level for the Wilson lower bound. Default 0.95.
#' @param verbose Print per-position progress. Default TRUE.
#'
#' @return A tibble: one row per position, metric, cutoff, and direction.
#' @export
find_lift_thresholds <- function(scored,
                                 features_path   = NULL,
                                 positions       = POSITIONS_DEFAULT,
                                 metrics         = NULL,
                                 hit_col         = "is_hit",
                                 training_only   = TRUE,
                                 candidate_probs = seq(0.20, 0.80, by = 0.05),
                                 min_group       = 10L,
                                 conf_level      = 0.95,
                                 verbose         = TRUE) {

  scored <- .resolve_scored(scored)
  scored <- .join_draft_age(scored, features_path)

  if (!hit_col %in% names(scored)) {
    stop(glue("find_lift_thresholds(): hit_col '{hit_col}' not in `scored`."),
         call. = FALSE)
  }

  # Restrict to the labelled training pool with outcomes. The prediction cohort
  # has no outcome, so it can never inform a hit rate.
  pool <- scored
  if (training_only) {
    if (!"is_training_player" %in% names(pool)) {
      stop("find_lift_thresholds(): training_only = TRUE but `scored` has no ",
           "is_training_player column.", call. = FALSE)
    }
    pool <- dplyr::filter(pool, .data$is_training_player == TRUE)
  }
  pool <- dplyr::filter(pool, !is.na(.data[[hit_col]]))

  if (nrow(pool) == 0L) {
    stop("find_lift_thresholds(): no training players with outcomes after ",
         "filtering. Check is_training_player and the hit column.",
         call. = FALSE)
  }

  if (is.null(metrics)) {
    metrics <- .derive_metric_universe(pool)
  } else {
    absent <- setdiff(metrics, names(pool))
    if (length(absent) > 0L) {
      stop(glue("find_lift_thresholds(): requested metric(s) not in `scored`: ",
                "{paste(absent, collapse = ', ')}."), call. = FALSE)
    }
  }

  if (length(metrics) == 0L) {
    stop("find_lift_thresholds(): no eligible metrics found. Expected columns ",
         "with a <metric>_tier companion (and optionally draft_age).",
         call. = FALSE)
  }

  if (verbose) {
    message(glue("find_lift_thresholds() | schema {R40_SCHEMA_TAG}"))
    message(glue("  metrics in universe: {length(metrics)}"))
    message(glue("  positions: {paste(positions, collapse = ', ')}"))
  }

  all_rows <- list()

  for (pos in positions) {
    pos_pool <- dplyr::filter(pool, .data$position == pos)
    n_total  <- nrow(pos_pool)

    if (n_total < MIN_METRIC_N) {
      if (verbose) {
        message(glue("  [{pos}] {n_total} training players < {MIN_METRIC_N} ",
                     "minimum -- position skipped."))
      }
      next
    }

    base_rate <- mean(pos_pool[[hit_col]])
    if (verbose) {
      message(glue("  [{pos}] n = {n_total} | base hit rate = ",
                   "{round(base_rate * 100, 1)}%"))
    }

    for (metric in metrics) {
      if (!metric %in% names(pos_pool)) next

      keep  <- !is.na(pos_pool[[metric]])
      vals  <- pos_pool[[metric]][keep]
      hits  <- pos_pool[[hit_col]][keep]
      n_eff <- length(vals)

      if (n_eff < MIN_METRIC_N) next                 # too few complete cases
      if (length(unique(vals)) < 3L) next            # not enough spread to split

      swept <- .sweep_one_metric(
        vals            = vals,
        hits            = hits,
        position        = pos,
        metric          = metric,
        base_rate       = base_rate,
        n_total         = n_total,
        candidate_probs = candidate_probs,
        min_group       = min_group,
        conf_level      = conf_level
      )

      if (!is.null(swept)) all_rows[[length(all_rows) + 1L]] <- swept
    }
  }

  if (length(all_rows) == 0L) {
    if (verbose) message("  No sweepable metric/position combinations produced rows.")
    return(tibble::tibble(
      position = character(), metric = character(), direction = character(),
      cutoff = numeric(), n_favorable = integer(), n_other = integer(),
      rate_favorable = numeric(), rate_other = numeric(), base_rate = numeric(),
      lift_abs = numeric(), lift_rel = numeric(), wilson_lower = numeric(),
      p_two_prop = numeric(), passes_guard = logical(), is_best = logical(),
      n_total = integer()
    ))
  }

  sweep <- dplyr::bind_rows(all_rows)

  # is_best: the single highest-lift qualifying cutoff per position + metric.
  # Ties broken by a higher Wilson lower bound, then a larger favorable pocket.
  # Metrics with no qualifying cutoff get is_best = FALSE on every row.
  sweep <- sweep |>
    dplyr::group_by(.data$position, .data$metric) |>
    dplyr::mutate(
      is_best = .pick_best(
        .data$passes_guard,
        .data$lift_abs,
        .data$wilson_lower,
        .data$n_favorable
      )
    ) |>
    dplyr::ungroup()

  # Round for display; keep counts integer.
  sweep <- sweep |>
    dplyr::mutate(
      cutoff         = round(.data$cutoff, 4),
      rate_favorable = round(.data$rate_favorable, 4),
      rate_other     = round(.data$rate_other, 4),
      base_rate      = round(.data$base_rate, 4),
      lift_abs       = round(.data$lift_abs, 4),
      lift_rel       = round(.data$lift_rel, 4),
      wilson_lower   = round(.data$wilson_lower, 4),
      p_two_prop     = round(.data$p_two_prop, 4)
    ) |>
    dplyr::arrange(.data$position, dplyr::desc(.data$is_best),
                   dplyr::desc(.data$lift_abs)) |>
    dplyr::select(
      "position", "metric", "direction", "cutoff",
      "n_favorable", "n_other", "rate_favorable", "rate_other",
      "base_rate", "lift_abs", "lift_rel", "wilson_lower",
      "p_two_prop", "passes_guard", "is_best", "n_total"
    )

  if (verbose) {
    n_markers <- sum(sweep$is_best, na.rm = TRUE)
    message(glue("  qualifying markers (is_best rows): {n_markers}"))
    message(strrep("=", 70))
  }

  sweep
}


# ==============================================================================
# COMPONENT 2: compare_hit_profiles()
# ==============================================================================
#
# Component 1 asks "where do you draw a line on one metric." Component 2 asks
# the complementary question Christian raised: take the players who actually
# hit, take the players who missed, and surface which college metrics separate
# the two groups the most. It is the hit-vs-miss profile contrast.
#
# WHY MEDIANS AND CLIFF'S DELTA, NOT MEANS AND COHEN'S D
# -----------------------------------------------------
# College production metrics are skewed and have outliers (a handful of monster
# college seasons drag any mean). Medians describe the typical hit and the
# typical miss honestly. To rank "what separates" across metrics that live on
# different scales (a 4.4 forty vs a 0.95 recruiting rating vs 80 rush yds/game)
# we need a scale-free, rank-based effect size, and Cliff's delta is the natural
# partner to a median framing:
#
#   cliffs_delta = P(hit > miss) - P(hit < miss),  range [-1, 1]
#
# Positive means hits tend to carry the higher value of that metric; negative
# means hits tend lower (draft_age behaves this way: hits are younger). The
# magnitude is comparable across metrics, robust to skew and outliers, and ties
# are handled cleanly. prob_superiority = (cliffs_delta + 1) / 2 is the same
# information as the chance a random hit out-measures a random miss.
#
# A Mann-Whitney (Wilcoxon rank-sum) p-value is reported as a descriptive
# column. It is NOT a gate: this contrast runs across every metric and position,
# so a raw p-value would overfire, same multiplicity logic as component 1.
#
# Rank metrics by abs(cliffs_delta) within a position to read off the strongest
# separators. Direction lives in hit_direction and the sign of median_diff.

# ------------------------------------------------------------------------------
# .cliffs_delta
#
# Cliff's delta between two numeric vectors, computed directly as the normalized
# count of dominant pairwise comparisons. O(n_x * n_y), which is trivial at the
# group sizes here (low hundreds). Ties contribute 0 to both directions, so they
# neither inflate nor deflate the estimate. Returns NA when either group is
# empty.
# ------------------------------------------------------------------------------
.cliffs_delta <- function(x, y) {
  if (length(x) == 0L || length(y) == 0L) return(NA_real_)
  cmp     <- outer(x, y, FUN = "-")
  n_gt    <- sum(cmp > 0)
  n_lt    <- sum(cmp < 0)
  (n_gt - n_lt) / (length(x) * length(y))
}

# ------------------------------------------------------------------------------
# .cliffs_magnitude
#
# Romano et al. interpretive bands for |Cliff's delta|: negligible < 0.147,
# small < 0.33, medium < 0.474, otherwise large. NA passes through.
# ------------------------------------------------------------------------------
.cliffs_magnitude <- function(delta) {
  if (is.na(delta)) return(NA_character_)
  a <- abs(delta)
  if (a < 0.147) return("negligible")
  if (a < 0.330) return("small")
  if (a < 0.474) return("medium")
  "large"
}

# ------------------------------------------------------------------------------
# .mw_p
#
# Two-sided Mann-Whitney / Wilcoxon rank-sum p-value. Descriptive only; never
# gates a result. Warnings from ties are suppressed because the value is
# reported, not thresholded. Returns NA when a group is empty or the test fails.
# ------------------------------------------------------------------------------
.mw_p <- function(x, y) {
  if (length(x) == 0L || length(y) == 0L) return(NA_real_)
  tryCatch(
    suppressWarnings(stats::wilcox.test(x, y, exact = FALSE)$p.value),
    error = function(e) NA_real_
  )
}


# ==============================================================================
# EXPORT: compare_hit_profiles
# ==============================================================================

#' Hit-vs-miss college-metric profile contrast
#'
#' For each position, splits the training pool into hits and misses, then for
#' each eligible college metric reports the median of each group, the median
#' difference, a scale-free Cliff's delta separation measure, its interpretive
#' magnitude, and a descriptive Mann-Whitney p-value. Rank within a position by
#' abs_delta to see which metrics separate hits from misses the most.
#'
#' @param scored R/28 prospect scores: data frame or path to the CSV.
#' @param features_path Optional path to the R/27 feature matrix, used only to
#'   left-join draft_age. NULL drops draft_age from the universe.
#' @param positions Character vector of positions to evaluate.
#' @param metrics Optional explicit metric vector. NULL auto-derives the same
#'   universe as find_lift_thresholds().
#' @param hit_col Name of the 0/1 outcome column. Default "is_hit".
#' @param training_only Restrict to is_training_player == TRUE. Default TRUE.
#' @param min_group Minimum complete cases required in EACH of the hit and miss
#'   groups for a metric to be reported. Default 10.
#' @param verbose Print per-position progress. Default TRUE.
#'
#' @return A tibble: one row per position and metric.
#' @export
compare_hit_profiles <- function(scored,
                                 features_path = NULL,
                                 positions     = POSITIONS_DEFAULT,
                                 metrics       = NULL,
                                 hit_col       = "is_hit",
                                 training_only = TRUE,
                                 min_group     = 10L,
                                 verbose       = TRUE) {

  scored <- .resolve_scored(scored)
  scored <- .join_draft_age(scored, features_path)

  if (!hit_col %in% names(scored)) {
    stop(glue("compare_hit_profiles(): hit_col '{hit_col}' not in `scored`."),
         call. = FALSE)
  }

  pool <- scored
  if (training_only) {
    if (!"is_training_player" %in% names(pool)) {
      stop("compare_hit_profiles(): training_only = TRUE but `scored` has no ",
           "is_training_player column.", call. = FALSE)
    }
    pool <- dplyr::filter(pool, .data$is_training_player == TRUE)
  }
  pool <- dplyr::filter(pool, !is.na(.data[[hit_col]]))

  if (nrow(pool) == 0L) {
    stop("compare_hit_profiles(): no training players with outcomes after ",
         "filtering.", call. = FALSE)
  }

  if (is.null(metrics)) {
    metrics <- .derive_metric_universe(pool)
  } else {
    absent <- setdiff(metrics, names(pool))
    if (length(absent) > 0L) {
      stop(glue("compare_hit_profiles(): requested metric(s) not in `scored`: ",
                "{paste(absent, collapse = ', ')}."), call. = FALSE)
    }
  }

  if (length(metrics) == 0L) {
    stop("compare_hit_profiles(): no eligible metrics found.", call. = FALSE)
  }

  if (verbose) {
    message(glue("compare_hit_profiles() | schema {R40_SCHEMA_TAG}"))
    message(glue("  metrics in universe: {length(metrics)}"))
  }

  all_rows <- list()

  for (pos in positions) {
    pos_pool <- dplyr::filter(pool, .data$position == pos)
    if (nrow(pos_pool) < MIN_METRIC_N) {
      if (verbose) {
        message(glue("  [{pos}] {nrow(pos_pool)} training players < ",
                     "{MIN_METRIC_N} minimum -- position skipped."))
      }
      next
    }

    is_hit_vec <- pos_pool[[hit_col]] == 1
    if (verbose) {
      message(glue("  [{pos}] hits = {sum(is_hit_vec)} | ",
                   "misses = {sum(!is_hit_vec)}"))
    }

    for (metric in metrics) {
      if (!metric %in% names(pos_pool)) next

      vals <- pos_pool[[metric]]
      x <- vals[is_hit_vec  & !is.na(vals)]   # hits, metric present
      y <- vals[!is_hit_vec & !is.na(vals)]   # misses, metric present

      if (length(x) < min_group || length(y) < min_group) next

      delta      <- .cliffs_delta(x, y)
      med_hit    <- stats::median(x)
      med_miss   <- stats::median(y)
      med_diff   <- med_hit - med_miss

      hit_dir <- if (is.na(delta) || delta == 0) {
        "equal"
      } else if (delta > 0) {
        "higher"
      } else {
        "lower"
      }

      all_rows[[length(all_rows) + 1L]] <- tibble::tibble(
        position         = pos,
        metric           = metric,
        n_hits           = length(x),
        n_miss           = length(y),
        median_hit       = med_hit,
        median_miss      = med_miss,
        median_diff      = med_diff,
        hit_direction    = hit_dir,
        cliffs_delta     = delta,
        prob_superiority = (delta + 1) / 2,
        magnitude        = .cliffs_magnitude(delta),
        mw_p             = .mw_p(x, y),
        abs_delta        = abs(delta)
      )
    }
  }

  if (length(all_rows) == 0L) {
    if (verbose) message("  No position/metric pair met the group-size floor.")
    return(tibble::tibble(
      position = character(), metric = character(),
      n_hits = integer(), n_miss = integer(),
      median_hit = numeric(), median_miss = numeric(), median_diff = numeric(),
      hit_direction = character(), cliffs_delta = numeric(),
      prob_superiority = numeric(), magnitude = character(),
      mw_p = numeric(), abs_delta = numeric()
    ))
  }

  profiles <- dplyr::bind_rows(all_rows) |>
    dplyr::mutate(
      median_hit       = round(.data$median_hit, 4),
      median_miss      = round(.data$median_miss, 4),
      median_diff      = round(.data$median_diff, 4),
      cliffs_delta     = round(.data$cliffs_delta, 4),
      prob_superiority = round(.data$prob_superiority, 4),
      mw_p             = round(.data$mw_p, 4),
      abs_delta        = round(.data$abs_delta, 4)
    ) |>
    dplyr::arrange(.data$position, dplyr::desc(.data$abs_delta)) |>
    dplyr::select(
      "position", "metric", "n_hits", "n_miss",
      "median_hit", "median_miss", "median_diff", "hit_direction",
      "cliffs_delta", "prob_superiority", "magnitude", "mw_p", "abs_delta"
    )

  if (verbose) {
    n_large <- sum(profiles$magnitude %in% c("medium", "large"), na.rm = TRUE)
    message(glue("  metric/position rows: {nrow(profiles)} | ",
                 "medium-or-large separators: {n_large}"))
    message(strrep("=", 70))
  }

  profiles
}


# ==============================================================================
# COMPONENT 3: find_comparables()
# ==============================================================================
#
# The Shiny rookie tab today finds a player's nearest historical neighbors by
# score_final, the single 0-100 enriched tier score. That collapses production
# and draft capital into one number, so it cannot answer the question the rookie
# article is built on: who does this player look like on PRODUCTION ALONE, before
# draft slot is folded in? find_comparables() generalizes the nearest-N idea to
# three keys, using the raw PPR/game predictions R/39 preserves:
#
#   key = "base"     pred_base       production-only model, draft capital absent
#   key = "enriched" pred_enriched   production plus draft capital
#   key = "gap"      pred_enriched - pred_base
#
# THE GAP IS THE ARTICLE SIGNAL
# -----------------------------
#   gap = pred_enriched - pred_base
#   gap >> 0  adding draft capital RAISES the projection: the draft slot is more
#             bullish than the college production alone (the market likes him
#             more than the tape does).
#   gap <= 0  production already carries the projection; draft slot adds little
#             or drags (the tape likes him more than the market does).
# Finding comparables by similar gap surfaces "players the model read the same
# way relative to where they were drafted," which is exactly the
# market-vs-production story.
#
# DATA SOURCES (the R/28-joined-to-R/39 contract)
# -----------------------------------------------
#   preds   R/39 augmented predictions (s3_r39_translation_preds_augmented).
#           Carries the raw pred_base / pred_enriched for both the training
#           (pred_type "loco", with outcomes) and the rookie cohort
#           (pred_type "final_model", outcomes NA). Keyed by nfl_gsis_id.
#   scored  R/28 prospect scores, optional but recommended. R/39 has no player
#           name; scored supplies cfb_player_name (and lets you name a target).
#
# Comparables are drawn from the historical pool (pred_type "loco") by default,
# because those are the players whose NFL outcome is known, so the punchy
# output is "N of the 10 nearest comparables hit." The target is excluded from
# its own neighbor set.
#
# KNOWN, DOCUMENTED BIAS (inherited from R/39)
# --------------------------------------------
# A rookie target is a final_model prediction matched against loco predictions.
# The two come from the same model fit on slightly different data (final vs
# leave-one-class-out). The resulting bias is small but real; it is named here,
# not engineered around. See the R/39 header.

# ------------------------------------------------------------------------------
# .resolve_preds
#
# Accept the R/39 augmented predictions as a data frame, an .rds path, or a .csv
# path. Verify the columns the comparable finder depends on before use.
# ------------------------------------------------------------------------------
.resolve_preds <- function(preds) {
  if (is.character(preds)) {
    if (length(preds) != 1L || !file.exists(preds)) {
      stop(glue("find_comparables(): preds file not found: {preds}"),
           call. = FALSE)
    }
    preds <- if (grepl("\\.rds$", preds, ignore.case = TRUE)) {
      readRDS(preds)
    } else {
      readr::read_csv(preds, show_col_types = FALSE)
    }
  }
  if (!is.data.frame(preds)) {
    stop("find_comparables(): `preds` must be a data frame or a file path.",
         call. = FALSE)
  }
  required <- c("nfl_gsis_id", "draft_position", "pred_base", "pred_enriched",
                "pred_type", "is_hit")
  missing  <- setdiff(required, names(preds))
  if (length(missing) > 0L) {
    stop(glue(
      "find_comparables(): `preds` missing required column(s): ",
      "{paste(missing, collapse = ', ')}. Expected the R/39 augmented file."
    ), call. = FALSE)
  }
  preds
}

# ------------------------------------------------------------------------------
# .resolve_target
#
# Return a single nfl_gsis_id for the target. Matches a literal nfl_gsis_id
# first, then a cfb_player_name (case-insensitive) when a name lookup is
# available. Errors clearly on no match or an ambiguous name.
# ------------------------------------------------------------------------------
.resolve_target <- function(target, preds, name_lookup) {
  target <- as.character(target)
  if (length(target) != 1L) {
    stop("find_comparables(): `target` must be a single id or name.",
         call. = FALSE)
  }
  if (target %in% preds$nfl_gsis_id) return(target)

  if (!is.null(name_lookup)) {
    hits <- name_lookup$nfl_gsis_id[
      tolower(name_lookup$cfb_player_name) == tolower(target)
    ]
    hits <- unique(hits[!is.na(hits)])
    if (length(hits) == 1L) return(hits)
    if (length(hits) > 1L) {
      stop(glue(
        "find_comparables(): target name '{target}' matches {length(hits)} ",
        "players. Pass the nfl_gsis_id instead."
      ), call. = FALSE)
    }
  }
  stop(glue("find_comparables(): target '{target}' not found by id or name."),
       call. = FALSE)
}

# ------------------------------------------------------------------------------
# .key_values
#
# Vector of key values for the chosen key. gap = enriched - base.
# ------------------------------------------------------------------------------
.key_values <- function(df, key) {
  switch(key,
    base     = df$pred_base,
    enriched = df$pred_enriched,
    gap      = df$pred_enriched - df$pred_base,
    stop(glue("find_comparables(): unknown key '{key}'."), call. = FALSE)
  )
}


# ==============================================================================
# EXPORT: find_comparables
# ==============================================================================

#' Nearest historical comparables by production, draft-adjusted, or the gap
#'
#' Given a target player, returns the n nearest historical players (those with a
#' known NFL outcome) on a chosen prediction key: production-only (base),
#' production plus draft capital (enriched), or the gap between them. The gap
#' view is the market-vs-production signal the Shiny tab cannot produce today.
#'
#' @param preds R/39 augmented predictions: data frame, .rds path, or .csv path.
#' @param target A single nfl_gsis_id, or a cfb_player_name when `scored` is
#'   supplied for the name lookup.
#' @param scored Optional R/28 prospect scores (data frame or path). Supplies
#'   cfb_player_name for display and for resolving a target by name.
#' @param key One of "gap" (default), "base", "enriched".
#' @param n Number of comparables to return. Default 10.
#' @param same_position Restrict comparables to the target's position. Default TRUE.
#' @param pool "loco" (default, historical players with outcomes) or "all".
#' @param verbose Print the target line and the comparable hit rate. Default TRUE.
#'
#' @return A tibble of comparables, nearest first, with the comparable hit rate
#'   attached as attr(result, "comp_hit_rate").
#' @export
find_comparables <- function(preds,
                             target,
                             scored        = NULL,
                             key           = c("gap", "base", "enriched"),
                             n             = 10L,
                             same_position = TRUE,
                             pool          = c("loco", "all"),
                             verbose       = TRUE) {

  key  <- match.arg(key)
  pool <- match.arg(pool)
  preds <- .resolve_preds(preds)

  # Optional name lookup from R/28, used for both display and target resolution.
  name_lookup <- NULL
  if (!is.null(scored)) {
    scored <- .resolve_scored(scored)
    if (all(c("nfl_gsis_id", "cfb_player_name") %in% names(scored))) {
      name_lookup <- scored |>
        dplyr::select(dplyr::all_of(c("nfl_gsis_id", "cfb_player_name"))) |>
        dplyr::filter(!is.na(.data$nfl_gsis_id)) |>
        dplyr::distinct(.data$nfl_gsis_id, .keep_all = TRUE)
    } else {
      message("  find_comparables(): `scored` lacks nfl_gsis_id/cfb_player_name ",
              "-- comparables shown by id only.")
    }
  }

  target_id <- .resolve_target(target, preds, name_lookup)

  # Target row and its key value. filter() drops rows with an NA id instead of
  # the phantom all-NA rows base `[` inserts when nfl_gsis_id contains NAs (the
  # 8 id-less rookies). That phantom-row insertion was the original bug.
  target_row <- dplyr::filter(preds, .data$nfl_gsis_id == target_id)
  if (nrow(target_row) == 0L) {
    stop(glue("find_comparables(): target resolved to id '{target_id}', but ",
              "that id is not present in `preds`."), call. = FALSE)
  }
  if (nrow(target_row) > 1L) {
    target_row <- target_row[1L, , drop = FALSE]
  }
  target_pos <- target_row$draft_position[1]
  target_key <- .key_values(target_row, key)[1]

  if (is.na(target_key)) {
    stop(glue("find_comparables(): target '{target_id}' has a missing ",
              "pred_base or pred_enriched, so key '{key}' is NA."),
         call. = FALSE)
  }

  # Candidate pool: historical outcomes by default, target always excluded.
  # filter() treats an NA condition as FALSE, so the id-less rookie rows never
  # leak in as phantom matches the way base `[` would insert them.
  cand <- dplyr::filter(preds, .data$nfl_gsis_id != target_id)
  if (pool == "loco") {
    cand <- dplyr::filter(cand, .data$pred_type == "loco")
  }
  if (same_position) {
    cand <- dplyr::filter(cand, .data$draft_position == target_pos)
  }
  if (nrow(cand) == 0L) {
    stop("find_comparables(): no candidate comparables after filtering. ",
         "Check pool and same_position.", call. = FALSE)
  }

  # 1-D nearest neighbor on the chosen key.
  cand$gap        <- cand$pred_enriched - cand$pred_base
  cand$key_value  <- .key_values(cand, key)
  cand$distance   <- abs(cand$key_value - target_key)
  cand <- cand[!is.na(cand$distance), , drop = FALSE]

  if (nrow(cand) == 0L) {
    stop(glue("find_comparables(): no candidates have a non-NA '{key}' value."),
         call. = FALSE)
  }

  n_return <- min(n, nrow(cand))
  if (n_return < n && verbose) {
    message(glue("  Only {nrow(cand)} candidates available; returning all."))
  }

  ord  <- order(cand$distance)
  cand <- cand[ord[seq_len(n_return)], , drop = FALSE]

  # Attach names if available.
  if (!is.null(name_lookup)) {
    cand <- dplyr::left_join(cand, name_lookup, by = "nfl_gsis_id")
  } else {
    cand$cfb_player_name <- NA_character_
  }

  has_year <- "draft_year"        %in% names(cand)
  has_ppr  <- "ppr_per_game_y13"  %in% names(cand)
  has_peak <- "peak_ppr_per_game" %in% names(cand)
  has_pkyr <- "peak_career_year"  %in% names(cand)

  out <- tibble::tibble(
    rank             = seq_len(nrow(cand)),
    nfl_gsis_id      = cand$nfl_gsis_id,
    cfb_player_name  = cand$cfb_player_name,
    position         = cand$draft_position,
    draft_year       = if (has_year) cand$draft_year else NA_integer_,
    key              = key,
    key_value        = round(cand$key_value, 3),
    target_value     = round(target_key, 3),
    distance         = round(cand$distance, 3),
    pred_base        = round(cand$pred_base, 3),
    pred_enriched    = round(cand$pred_enriched, 3),
    gap              = round(cand$gap, 3),
    is_hit           = cand$is_hit,
    ppr_per_game_y13  = if (has_ppr)  round(cand$ppr_per_game_y13, 2) else NA_real_,
    peak_ppr_per_game = if (has_peak) round(cand$peak_ppr_per_game, 2) else NA_real_,
    peak_career_year  = if (has_pkyr) cand$peak_career_year else NA_integer_
  )

  comp_hit_rate <- if (all(is.na(out$is_hit))) {
    NA_real_
  } else {
    mean(out$is_hit, na.rm = TRUE)
  }

  if (verbose) {
    tname <- if (!is.null(name_lookup)) {
      nm <- name_lookup$cfb_player_name[name_lookup$nfl_gsis_id == target_id]
      if (length(nm) > 0L && !is.na(nm[1])) nm[1] else target_id
    } else {
      target_id
    }
    message(glue("find_comparables() | schema {R40_SCHEMA_TAG}"))
    message(glue("  target: {tname} ({target_pos}) | key = {key} | ",
                 "target {key} = {round(target_key, 3)}"))
    if (!is.na(comp_hit_rate)) {
      message(glue("  {sum(out$is_hit, na.rm = TRUE)} of {nrow(out)} nearest ",
                   "comparables hit ({round(comp_hit_rate * 100, 1)}%)"))
    }
    message(strrep("=", 70))
  }

  attr(out, "comp_hit_rate") <- comp_hit_rate
  attr(out, "target_id")     <- target_id
  out
}
