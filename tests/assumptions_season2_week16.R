# ==============================================================================
# SEASON 2 WEEK 16: DEF/ST PROJECTION -- ASSUMPTIONS / REGRESSION JUSTIFICATION
# File: tests/assumptions_season2_week16.R
# ==============================================================================
#
# WHY THIS FILE LOOKS DIFFERENT FROM OTHER ASSUMPTIONS SCRIPTS
# -----------------------------------------------------------
# R/34's scoring is deterministic arithmetic and its in-season decay is R/29's
# compute_prior_weight(), already validated in the Season 1 R/14 predictive-
# validity study and reused unchanged. Neither has a distributional assumption
# to test. The ONE judgment call in R/34 is REGRESSION_STRENGTH = 0.35, chosen
# by assertion. This script validates that single decision empirically, which
# is the assumption testing that actually earns its keep here.
#
# TWO QUESTIONS
# -------------
#   1. STICKINESS: does team DEF PPG regress season to season? If year N barely
#      predicts year N+1, shrinkage toward the mean is justified; if defenses
#      are sticky, 0.35 is too aggressive and discards signal.
#   2. CALIBRATION: is 0.35 near the out-of-sample optimum? Leave-one-season-out
#      backtest sweeping the shrinkage strength and minimizing prediction error.
#
# Run order (project standard): example -> assumptions -> tests -> visuals.
# Run example_season2_week16.R first. This script loads pbp for every available
# season, so it may take a couple of minutes on a cold cache.
#
# All numbers in the verdicts are computed from data, never hardcoded.
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

library(dplyr)
library(here)
library(glue)

source(here::here("R", "34_def_st_projection.R"))

# Candidate seasons to pull. Anything not in the cache is skipped with a note,
# so this works whether you have the full 2010-2025 normalized cache or just a
# few recent seasons. More seasons = a richer backtest.
CANDIDATE_SEASONS <- 2010:2025

# Shrinkage strengths to sweep. 0 = no regression (pure historical mean),
# 1 = fully the league mean.
STRENGTH_GRID <- seq(0, 0.70, by = 0.05)

# ==============================================================================
# PURE HELPERS (unit-verifiable -- no I/O)
# ==============================================================================

# ------------------------------------------------------------------------------
# .yoy_stickiness
# ------------------------------------------------------------------------------

#' Pooled year-over-year correlation of team DEF PPG
#'
#' Joins each team's DEF PPG in consecutive seasons and returns the pooled
#' Pearson correlation across all (season N, season N+1) pairs. Low correlation
#' means DEF PPG is noisy season to season and shrinkage is warranted.
#'
#' @param season_ppg Tibble: season, team, def_ppg.
#' @return List: r (correlation), n_pairs (team-season pairs used).
#' @keywords internal
.yoy_stickiness <- function(season_ppg) {
  nxt <- season_ppg %>%
    dplyr::mutate(season = .data$season - 1L) %>%
    dplyr::rename(def_ppg_next = def_ppg)

  paired <- season_ppg %>%
    dplyr::inner_join(nxt, by = c("season", "team"))

  if (nrow(paired) < 3L) {
    return(list(r = NA_real_, n_pairs = nrow(paired)))
  }

  list(
    r = stats::cor(paired$def_ppg, paired$def_ppg_next, use = "complete.obs"),
    n_pairs = nrow(paired)
  )
}

# ------------------------------------------------------------------------------
# .shrinkage_backtest
# ------------------------------------------------------------------------------

#' Leave-one-season-out backtest of the shrinkage strength
#'
#' For each target season with at least one prior season available, builds the
#' prior exactly as R/34 does (equal-weight mean of up to `window` prior
#' seasons, regressed toward the prior-window league mean), then measures
#' prediction error against the target season's actual DEF PPG, at each
#' strength in `strengths`. Errors are pooled across all target seasons.
#'
#' @param season_ppg Tibble: season, team, def_ppg.
#' @param strengths Numeric vector of shrinkage strengths to test.
#' @param window Integer. Max prior seasons in the prior (R/34's HISTORICAL_WINDOW).
#' @return Tibble: strength, rmse, mae, n (pooled team-season predictions).
#' @keywords internal
.shrinkage_backtest <- function(season_ppg, strengths, window = 3L) {
  seasons <- sort(unique(season_ppg$season))

  # Build the (prior, actual) prediction set once, then score each strength.
  preds <- purrr::map_dfr(seasons, function(t) {
    prior_seasons <- seasons[seasons < t]
    prior_seasons <- utils::tail(prior_seasons, window)
    if (length(prior_seasons) == 0L) return(NULL)

    prior_tbl <- season_ppg %>%
      dplyr::filter(.data$season %in% prior_seasons) %>%
      dplyr::group_by(.data$team) %>%
      dplyr::summarise(prior_raw = mean(.data$def_ppg, na.rm = TRUE),
                       .groups = "drop")

    # League mean of the prior window, computed as a scalar BEFORE the mutate
    # so the name resolves to the data frame, not a masked column.
    prior_league_mean <- mean(prior_tbl$prior_raw, na.rm = TRUE)

    actual <- season_ppg %>%
      dplyr::filter(.data$season == t) %>%
      dplyr::select(team, actual = def_ppg)

    prior_tbl %>%
      dplyr::inner_join(actual, by = "team") %>%
      dplyr::mutate(target_season = t, prior_mean = prior_league_mean)
  })

  if (is.null(preds) || nrow(preds) == 0L) {
    return(tibble::tibble(strength = numeric(), rmse = numeric(),
                          mae = numeric(), n = integer()))
  }

  purrr::map_dfr(strengths, function(s) {
    regressed <- (1 - s) * preds$prior_raw + s * preds$prior_mean
    err <- regressed - preds$actual
    tibble::tibble(
      strength = s,
      rmse     = sqrt(mean(err^2, na.rm = TRUE)),
      mae      = mean(abs(err), na.rm = TRUE),
      n        = sum(!is.na(err))
    )
  })
}

# ==============================================================================
# RUN
# ==============================================================================

message(glue("\n{strrep('=', 70)}"))
message("R/34 ASSUMPTIONS: DEF PPG regression justification")
message(glue("{strrep('=', 70)}"))

# Step 1: per-team DEF PPG for every available season -----------------------
message("\nSTEP 1: Computing per-team DEF PPG for available seasons")
scoring <- DEF_SCORING_DEFAULT

season_ppg <- purrr::map_dfr(CANDIDATE_SEASONS, function(s) {
  pbp <- tryCatch(
    load_normalized_season(s, cache_dir = CACHE_DIR_DEFAULT),
    error = function(e) NULL
  )
  if (is.null(pbp)) {
    return(NULL)
  }
  .compute_team_def_ppg(pbp, scoring) %>% dplyr::mutate(season = s)
})

available <- sort(unique(season_ppg$season))
message(glue("  Seasons available: {paste(available, collapse = ', ')} ",
             "({length(available)} total)"))

if (length(available) < 2L) {
  stop("Need at least 2 cached seasons to assess stickiness or backtest.",
       call. = FALSE)
}

# Step 2: stickiness --------------------------------------------------------
message("\nSTEP 2: Year-over-year stickiness")
stick <- .yoy_stickiness(season_ppg)
message(glue("  Pooled YoY correlation r = ",
             "{format(round(stick$r, 3), nsmall = 3)} ",
             "across {stick$n_pairs} team-season pairs"))

# Step 3: shrinkage sweep ---------------------------------------------------
message("\nSTEP 3: Leave-one-season-out shrinkage backtest")
sweep <- .shrinkage_backtest(season_ppg, STRENGTH_GRID,
                             window = HISTORICAL_WINDOW)

if (nrow(sweep) == 0L) {
  message("  Not enough prior-season depth to backtest (need a target season ",
          "with at least one prior season).")
} else {
  best <- sweep[which.min(sweep$rmse), ]
  rmse_at_0    <- sweep$rmse[which.min(abs(sweep$strength - 0.00))]
  rmse_at_035  <- sweep$rmse[which.min(abs(sweep$strength - 0.35))]
  pct_off_opt  <- 100 * (rmse_at_035 - best$rmse) / best$rmse

  message("\n  RMSE by shrinkage strength:")
  print(sweep)

  message(glue("\n  No-regression RMSE (strength 0):  ",
               "{format(round(rmse_at_0, 3), nsmall = 3)}"))
  message(glue("  Current setting RMSE (0.35):      ",
               "{format(round(rmse_at_035, 3), nsmall = 3)}"))
  message(glue("  Best RMSE (strength {format(best$strength, nsmall = 2)}): ",
               "{format(round(best$rmse, 3), nsmall = 3)}"))
}

# Step 4: verdicts ----------------------------------------------------------
message(glue("\n{strrep('=', 70)}"))
message("VERDICTS")
message(glue("{strrep('=', 70)}"))

if (!is.na(stick$r)) {
  stick_verdict <- if (stick$r < 0.40) {
    "WEAK stickiness -- DEF PPG is noisy year to year, shrinkage justified."
  } else if (stick$r < 0.65) {
    "MODERATE stickiness -- some signal carries, moderate shrinkage sensible."
  } else {
    "STRONG stickiness -- DEF PPG is fairly repeatable, heavy shrinkage may discard signal."
  }
  message(glue("  Stickiness: r = {format(round(stick$r, 3), nsmall = 3)} -- {stick_verdict}"))
}

if (exists("best") && nrow(sweep) > 0L) {
  calib_verdict <- if (abs(pct_off_opt) <= 5) {
    glue("0.35 is within {format(round(abs(pct_off_opt), 1), nsmall = 1)}% of ",
         "optimal RMSE -- DEFENSIBLE, keep it.")
  } else {
    glue("0.35 is {format(round(abs(pct_off_opt), 1), nsmall = 1)}% off optimal ",
         "(best at {format(best$strength, nsmall = 2)}) -- consider changing ",
         "REGRESSION_STRENGTH to {format(best$strength, nsmall = 2)}.")
  }
  message(glue("  Calibration: {calib_verdict}"))
}

message(glue("{strrep('=', 70)}\n"))
