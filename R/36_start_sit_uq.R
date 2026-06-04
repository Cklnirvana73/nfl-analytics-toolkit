# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 16
# Start/Sit Uncertainty Quantification
# File: R/36_start_sit_uq.R
#
# PURPOSE
# -------
# A thin uncertainty layer on top of R/35. R/35 returns the best legal starting
# lineup and a binary confidence_flag (intervals overlap or they do not). R/36
# answers the questions that flag cannot:
#
#   1. How likely is each start to actually be correct?  (p_start_correct)
#   2. When a call goes wrong, how much does it typically cost? (avg_miss)
#   3. How much expected value is at risk across the whole lineup?
#      (week_risk_score)
#
# It does NOT re-solve the lineup. It enriches the lineup R/35 already produced.
# R/35 is consumed, never modified.
#
# THE MODEL (all analytical, no simulation)
# -----------------------------------------
#   - Each player's weekly score is treated as an independent Normal. The mean
#     is the matchup-adjusted projection (adj_proj). The spread is recovered
#     from the R/32 80% interval:
#         sigma = (upper_80 - lower_80) / (2 * qnorm(0.9))
#   - Interval scale: the R/32 interval is centered on the raw projection, but
#     the lineup decision uses adj_proj. To keep the mean and the spread on the
#     same scale, the interval is scaled by matchup_factor (multiplicative), so
#     a favorable matchup widens the spread in proportion to the lift. This also
#     resolves a latent inconsistency in R/35, where .flag_confidence compares
#     raw intervals but picks the best alternative by adj_proj.
#   - For starter S and best alternative A, both Normal and independent:
#         p_start_correct = pnorm( (mu_s - mu_a) / sqrt(sigma_s^2 + sigma_a^2) )
#         expected_regret = E[max(A - S, 0)]            (probability-weighted)
#         avg_miss        = expected_regret / (1 - p_start_correct)  (conditional)
#     expected_regret folds in both the odds and the magnitude, so it is the
#     quantity summed into week_risk_score. avg_miss is the human-readable
#     "if this goes wrong, you lose about X points."
#
# INDEPENDENCE ASSUMPTION
# -----------------------
# Player scores are treated as independent. Same-game correlation (a QB and his
# WR move together) is real and is NOT modeled here. It is a documented v2
# limitation, not a bug.
#
# DEF / ST (the streaming case)
# -----------------------------
# R/34 produces a DEF projected PPG but no interval, and R/35's
# suggest_waiver_adds() excludes DEF from waiver candidates entirely, so nothing
# upstream answers "drop my DEF for one on waivers?". R/36 fills that gap:
#   - A pooled, league-wide weekly DEF scoring sigma is computed empirically from
#     calculate_def_st_points() (R/29) over def_seasons. Pooled rather than
#     per-team because one defense plays only ~17 games a season, too few for a
#     stable per-team spread.
#   - That sigma puts every DEF on the same Normal footing as offense, so the
#     same probability and regret formulas apply.
#   - For the DEF slot, the alternative set includes AVAILABLE (waiver) defenses,
#     not just benched ones. That is what makes the streaming comparison work:
#     P(waiver DEF outscores my DEF) and the expected miss, directly.
#   - When the DEF slot is OPEN (no DEF in the optimal lineup, the streamer's
#     usual state), there is no started DEF to compare against, so R/36 instead
#     returns a ranked streaming table (lineup$def_streaming): the top available
#     defenses by matchup-adjusted projection, each with P(it beats the
#     next-ranked option) and a clarity label (clear / slight edge / tossup).
#     Clarity reads off that probability, not off regret: DEF variance is large
#     enough that the absolute point swing between near-equal defenses is big
#     even when the choice barely matters, so probability of separation is the
#     honest signal for a streaming pick.
#   - The empirical sigma uses Sleeper-standard DEF scoring (what
#     calculate_def_st_points() emits). A league with custom DEF scoring would
#     want it rescaled; documented as a v1 limitation.
#
# OUTPUTS (only when save_output = TRUE)
# --------------------------------------
#   data/season2_cache/s2_week16_r36_start_sit_uq.rds / .csv
#
# SOURCE DEPENDENCIES (sourced below if not already loaded)
# ---------------------------------------------------------
#   R/35_lineup_optimizer.R  -- optimize_lineup(), assemble_player_pool(),
#                               compute_dvp_factors(), compute_def_matchup_factors(),
#                               .apply_matchup(), .load_week_matchups(),
#                               SLOT_ELIGIBILITY, LINEUP_OFFENSE_POSITIONS
#                               (R/35 in turn sources R/33/R/32/R/29/R/19/R/15)
#   R/29_projection_engine.R -- calculate_def_st_points()
#   R/15_multi_season_pbp.R  -- load_normalized_season()
#
# Consumed as data (passed in by the caller, not sourced):
#   lineup      -- the list returned by optimize_lineup() (R/35)
#   reconciled  -- R/32 reconciled projections (recovers starter intervals)
#   vorp        -- R/33 VORP rankings (only for the waiver-DEF pool)
#   def_proj    -- R/34 DEF projections   (only for the waiver-DEF pool)
#
# RUN
# ---
#   source(here::here("R", "36_start_sit_uq.R"))
#   # offense-only enrichment (no waiver DEF comparison):
#   uq <- analyze_start_sit(lineup, reconciled)
#
#   # full enrichment with the rostered-vs-waiver DEF comparison, week 6:
#   uq <- analyze_start_sit(lineup, reconciled,
#                           source = src, vorp = vorp, def_proj = def_proj,
#                           week = 6L)
#   uq$starters          # enriched lineup with the UQ columns
#   uq$week_risk_score   # one number: expected points at risk this week
#
# SCHEMA TAG: s2_w16_uq_v1
# Author: Christian K. LeBlanc
# Version: 1.0
# ==============================================================================

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(here)
library(glue)

# Null-coalescing infix (defined in R/19; redefine defensively so this file is
# self-contained if the R/19 chain has not been sourced yet).
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Source dependencies only if their entry points are not already in the session.
# Sourcing R/35 pulls in the whole projection/optimizer chain it depends on.
if (!exists("optimize_lineup") || !exists("assemble_player_pool")) {
  source(here::here("R", "35_lineup_optimizer.R"))
}
if (!exists("calculate_def_st_points")) {
  source(here::here("R", "29_projection_engine.R"))
}
if (!exists("load_normalized_season")) {
  source(here::here("R", "15_multi_season_pbp.R"))
}

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

SEASON <- 2026L
SCHEMA_TAG_UQ <- "s2_w16_uq_v1"

# z multiplier for the 80% central interval: the interval spans the 10th to the
# 90th percentile, so half-width = qnorm(0.9) * sigma.
Z80 <- stats::qnorm(0.9)

# Stakes label thresholds, in points of expected regret (tunable). A slot below
# STAKES_LOW_MAX is "low" stakes, at or above STAKES_HIGH_MIN is "high".
STAKES_LOW_MAX  <- 1.0
STAKES_HIGH_MIN <- 3.0

# DEF streaming pick-clarity thresholds, on P(the top defense beats the next
# option). DEF weekly variance is large, so adjacent defenses are usually a
# tossup; these label how separated the top pick actually is (tunable).
PICK_CLEAR_MIN  <- 0.60
PICK_SLIGHT_MIN <- 0.53

# Fallback weekly DEF scoring sigma used only if the empirical history pull
# returns too little to estimate one.
DEF_SIGMA_FALLBACK <- 4.0

# Below this wrong-call probability, the conditional avg_miss is undefined
# (a near-lock has no meaningful "when wrong" loss); report NA.
P_WRONG_FLOOR <- 1e-6

CACHE_DIR_DEFAULT <- here::here("data", "season2_cache")
OUTPUT_RDS_PATH_UQ <- here::here("data", "season2_cache",
                                 "s2_week16_r36_start_sit_uq.rds")
OUTPUT_CSV_PATH_UQ <- here::here("data", "season2_cache",
                                 "s2_week16_r36_start_sit_uq.csv")
OUTPUT_RDS_PATH_DEF <- here::here("data", "season2_cache",
                                  "s2_week16_r36_def_streaming.rds")
OUTPUT_CSV_PATH_DEF <- here::here("data", "season2_cache",
                                  "s2_week16_r36_def_streaming.csv")

# ------------------------------------------------------------------------------
# NSE DECLARATIONS
# ------------------------------------------------------------------------------

utils::globalVariables(c(
  ".data", "nfl_gsis_id", "player_name", "team", "position", "opponent",
  "base_proj", "matchup_factor", "adj_proj", "adjusted_vorp", "confidence_flag",
  "schema_tag", "r32_projection_lower_80", "r32_projection_upper_80",
  "lower_80", "upper_80", "is_available", "def_st_points", "slot",
  "mu_a", "sigma_a", "mu_s", "sigma_s", "alt_player", "alt_position",
  "alt_source", "alt_adj_proj", "p_start_correct", "avg_miss",
  "expected_regret", "stakes", "uq_schema_tag",
  "def_team", "p_vs_next", "pick_clarity", "rank", "sigma"
))

# ==============================================================================
# SECTION 1: DISTRIBUTION HELPERS (scalar; use base if/else, not dplyr::if_else)
# ==============================================================================

# ------------------------------------------------------------------------------
# .recover_sigma
# ------------------------------------------------------------------------------

#' Recover a Normal sigma from an 80% central interval
#'
#' The 80% interval runs from the 10th to the 90th percentile, so the
#' half-width equals qnorm(0.9) * sigma. Vectorized; NA in either bound
#' propagates to NA.
#'
#' @param lower_80,upper_80 Numeric vectors of interval bounds.
#' @return Numeric vector of recovered sigmas.
#' @keywords internal
.recover_sigma <- function(lower_80, upper_80) {
  (upper_80 - lower_80) / (2 * Z80)
}

# ------------------------------------------------------------------------------
# .prob_start_correct
# ------------------------------------------------------------------------------

#' P(starter outscores alternative) for two independent Normals
#'
#' @param mu_s,sd_s Starter mean and sigma (scalars).
#' @param mu_a,sd_a Alternative mean and sigma (scalars).
#' @return Probability in [0,1], or NA if a mean is missing.
#' @keywords internal
.prob_start_correct <- function(mu_s, sd_s, mu_a, sd_a) {
  if (is.na(mu_s) || is.na(mu_a)) return(NA_real_)
  sd_diff <- sqrt((sd_s %||% 0)^2 + (sd_a %||% 0)^2)
  if (is.na(sd_diff) || sd_diff == 0) {
    return(if (mu_s > mu_a) 1 else if (mu_s == mu_a) 0.5 else 0)
  }
  stats::pnorm((mu_s - mu_a) / sd_diff)
}

# ------------------------------------------------------------------------------
# .expected_regret
# ------------------------------------------------------------------------------

#' Unconditional expected regret E[max(A - S, 0)] for two independent Normals
#'
#' The probability-weighted points left on the bench: it is large only when the
#' alternative is both plausibly better AND the gap could be material. Uses the
#' closed-form mean of the positive part of a Normal:
#'   E[Y+] = m * pnorm(m / s) + s * dnorm(m / s),  m = mu_a - mu_s, s = sd_diff.
#'
#' @param mu_s,sd_s Starter mean and sigma (scalars).
#' @param mu_a,sd_a Alternative mean and sigma (scalars).
#' @return Expected regret in points, or NA if a mean is missing.
#' @keywords internal
.expected_regret <- function(mu_s, sd_s, mu_a, sd_a) {
  if (is.na(mu_s) || is.na(mu_a)) return(NA_real_)
  sd_diff <- sqrt((sd_s %||% 0)^2 + (sd_a %||% 0)^2)
  if (is.na(sd_diff) || sd_diff == 0) {
    return(max(mu_a - mu_s, 0))
  }
  m <- mu_a - mu_s
  m * stats::pnorm(m / sd_diff) + sd_diff * stats::dnorm(m / sd_diff)
}

# ------------------------------------------------------------------------------
# .stakes_label
# ------------------------------------------------------------------------------

#' Bucket expected regret into a stakes label
#' @keywords internal
.stakes_label <- function(expected_regret) {
  if (is.na(expected_regret)) return(NA_character_)
  if (expected_regret < STAKES_LOW_MAX) return("low")
  if (expected_regret >= STAKES_HIGH_MIN) return("high")
  "medium"
}

# ------------------------------------------------------------------------------
# .pick_clarity
# ------------------------------------------------------------------------------

#' Label how clearly the top streaming defense beats the next option
#'
#' Based on P(top beats next), not on regret: DEF variance makes the absolute
#' point swing large even between interchangeable defenses, so probability of
#' separation is the honest signal for a streaming pick.
#' @keywords internal
.pick_clarity <- function(p_vs_next) {
  if (is.na(p_vs_next)) return(NA_character_)
  if (p_vs_next >= PICK_CLEAR_MIN)  return("clear")
  if (p_vs_next >= PICK_SLIGHT_MIN) return("slight edge")
  "tossup"
}

# ------------------------------------------------------------------------------
# .eligible_positions_for_slot
# ------------------------------------------------------------------------------

#' Positions that may legally fill a given lineup slot (reverse of SLOT_ELIGIBILITY)
#' @keywords internal
.eligible_positions_for_slot <- function(slot) {
  names(SLOT_ELIGIBILITY)[
    vapply(SLOT_ELIGIBILITY, function(v) slot %in% v, logical(1))
  ]
}

# ==============================================================================
# SECTION 2: EMPIRICAL DEF SIGMA
# ==============================================================================

# ------------------------------------------------------------------------------
# .compute_def_sigma
# ------------------------------------------------------------------------------

#' Pooled league-wide weekly DEF scoring sigma from history
#'
#' Loads each season's pbp, scores team defenses per game with
#' calculate_def_st_points() (R/29; Sleeper-standard DEF scoring), pools every
#' team-week observation, and returns the standard deviation. Pooled rather than
#' per-team because a single defense plays only ~17 games a season.
#'
#' @param seasons Integer vector of prior seasons (default SEASON - 1).
#' @param cache_dir Character. R/15 cache directory.
#' @return A single numeric sigma (falls back to DEF_SIGMA_FALLBACK if history
#'   is unavailable).
#' @seealso calculate_def_st_points, load_normalized_season
#' @keywords internal
.compute_def_sigma <- function(seasons = SEASON - 1L,
                               cache_dir = CACHE_DIR_DEFAULT) {
  scores <- purrr::map(seasons, function(s) {
    pbp <- tryCatch(load_normalized_season(s, cache_dir = cache_dir),
                    error = function(e) NULL)
    if (is.null(pbp) || nrow(pbp) == 0L) return(numeric(0))
    dg <- tryCatch(calculate_def_st_points(pbp), error = function(e) NULL)
    if (is.null(dg) || nrow(dg) == 0L || !"def_st_points" %in% names(dg)) {
      return(numeric(0))
    }
    dg$def_st_points
  })

  all_scores <- unlist(scores, use.names = FALSE)
  all_scores <- all_scores[is.finite(all_scores)]

  if (length(all_scores) < 2L) {
    message(glue("  .compute_def_sigma(): insufficient DEF history; ",
                 "using fallback sigma {DEF_SIGMA_FALLBACK}."))
    return(DEF_SIGMA_FALLBACK)
  }
  stats::sd(all_scores)
}

# ------------------------------------------------------------------------------
# .assemble_waiver_def
# ------------------------------------------------------------------------------

#' Available (waiver) team defenses, matchup-adjusted to the lineup's scale
#'
#' Re-assembles the player pool to recover every DEF row, keeps the ones no team
#' rosters, and applies the same matchup adjustment the started DEF received so
#' the comparison is like-for-like.
#'
#' @param source league_source from build_league_source() (R/35).
#' @param reconciled R/32 reconciled projections.
#' @param vorp R/33 VORP rankings.
#' @param def_proj R/34 DEF projections.
#' @param week Integer or NULL. NULL = neutral matchup.
#' @param dvp,def_factors Optional precomputed matchup factors.
#' @param season Integer NFL season.
#' @param cache_dir Character. R/15 cache directory.
#' @return Tibble of available DEF rows with adj_proj and matchup_factor; empty
#'   tibble if none.
#' @keywords internal
.assemble_waiver_def <- function(source, reconciled, vorp, def_proj,
                                 week, dvp, def_factors, season, cache_dir) {
  pool <- assemble_player_pool(source, reconciled, vorp, def_proj)
  def_av <- pool %>%
    dplyr::filter(.data$position == "DEF", .data$is_available %in% TRUE)
  if (nrow(def_av) == 0L) return(def_av)

  if (!is.null(week)) {
    if (is.null(dvp)) {
      dvp <- compute_dvp_factors(seasons = season - 1L, cache_dir = cache_dir)
    }
    if (is.null(def_factors)) {
      def_factors <- compute_def_matchup_factors(seasons = season - 1L,
                                                 cache_dir = cache_dir)
    }
    week_matchups <- .load_week_matchups(season, week)
  } else {
    week_matchups <- NULL
  }

  .apply_matchup(def_av, week_matchups, dvp, def_factors)
}

# ------------------------------------------------------------------------------
# .recommend_def_stream
# ------------------------------------------------------------------------------

#' Rank available defenses to stream into an open DEF slot
#'
#' For a roster with no DEF in the optimal lineup (a streamer's open slot), this
#' ranks the available defenses by matchup-adjusted projection and, for each,
#' reports the probability and expected regret of taking it over the next-ranked
#' option. The top row is the recommended stream; the p_vs_next on that row says
#' how clear-cut the choice is over the second-best defense.
#'
#' @param source,reconciled,vorp,def_proj Inputs for the available-DEF pool.
#' @param def_sigma Pooled empirical weekly DEF sigma (from .compute_def_sigma).
#' @param week,dvp,def_factors,season,cache_dir Matchup inputs (see optimize_lineup).
#' @param max_suggestions Integer. Top-N defenses to return.
#' @return Tibble: rank, def_team, opponent, adj_proj, p_vs_next, pick_clarity,
#'   uq_schema_tag; or NULL if none available. pick_clarity labels how clearly
#'   the top pick separates from the next option (clear / slight edge / tossup).
#' @seealso .assemble_waiver_def, .compute_def_sigma
#' @keywords internal
.recommend_def_stream <- function(source, reconciled, vorp, def_proj,
                                  def_sigma, week, dvp, def_factors,
                                  season, cache_dir, max_suggestions) {

  wd <- .assemble_waiver_def(source, reconciled, vorp, def_proj,
                             week = week, dvp = dvp, def_factors = def_factors,
                             season = season, cache_dir = cache_dir)
  if (is.null(wd) || nrow(wd) == 0L) return(NULL)

  ranked <- wd %>%
    dplyr::transmute(
      def_team = .data$team,
      opponent = .data$opponent,
      adj_proj = .data$adj_proj,
      sigma    = def_sigma * dplyr::coalesce(.data$matchup_factor, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$adj_proj)) %>%
    dplyr::slice_head(n = max_suggestions) %>%
    dplyr::mutate(rank = dplyr::row_number())

  # P(this defense outscores the next-ranked one). The absolute point swing is
  # dominated by DEF variance and is not decision-relevant for near-ties, so it
  # is intentionally not reported; the clarity label reads off this probability.
  n <- nrow(ranked)
  p_vs_next <- rep(NA_real_, n)
  if (n >= 2L) {
    for (i in seq_len(n - 1L)) {
      p_vs_next[i] <- .prob_start_correct(ranked$adj_proj[i], ranked$sigma[i],
                                          ranked$adj_proj[i + 1L],
                                          ranked$sigma[i + 1L])
    }
  }
  clarity <- vapply(p_vs_next, .pick_clarity, character(1))

  ranked %>%
    dplyr::transmute(
      rank,
      def_team,
      opponent,
      adj_proj = round(.data$adj_proj, 2),
      p_vs_next = round(p_vs_next, 3),
      pick_clarity = clarity,
      uq_schema_tag = SCHEMA_TAG_UQ
    )
}

# ==============================================================================
# SECTION 3: MAIN ENTRY POINT
# ==============================================================================

# ------------------------------------------------------------------------------
# analyze_start_sit
# ------------------------------------------------------------------------------

#' Enrich an optimized lineup with start/sit uncertainty quantification
#'
#' Takes the list optimize_lineup() returned and, for every started slot,
#' computes the probability the start is correct, the conditional average miss,
#' the probability-weighted expected regret, and a stakes label, plus a single
#' week-level risk score. Offense uses the R/32 80% interval; DEF uses a pooled
#' empirical sigma and compares against available (waiver) defenses.
#'
#' The offense comparison needs only lineup and reconciled (the bench already
#' carries intervals; starters are re-joined to reconciled to recover theirs).
#' The rostered-vs-waiver DEF comparison additionally needs source, vorp, and
#' def_proj. When include_waiver_def is TRUE but those are not all supplied, the
#' DEF alternatives fall back to the bench with a one-line message.
#'
#' @param lineup List from optimize_lineup(): starters, bench, unavailable,
#'   all_close.
#' @param reconciled R/32 reconciled projections (for starter intervals).
#' @param source league_source, or NULL. Needed for the waiver-DEF comparison.
#' @param vorp R/33 VORP rankings, or NULL. Needed for the waiver-DEF pool.
#' @param def_proj R/34 DEF projections, or NULL. Needed for the waiver-DEF pool.
#' @param week Integer or NULL. The week the lineup was solved for; used to put
#'   waiver defenses on the same matchup-adjusted scale as the started DEF.
#' @param dvp,def_factors Optional precomputed matchup factors for the waiver DEF.
#' @param def_seasons Integer vector. History window for the empirical DEF sigma.
#' @param include_waiver_def Logical. Compare the started DEF against available
#'   defenses, not just benched ones; and, when the DEF slot is open, produce a
#'   ranked streaming recommendation.
#' @param def_max_suggestions Integer. Top-N defenses in the streaming table.
#' @param season Integer or NULL (default = source season, else SEASON).
#' @param cache_dir Character. R/15 cache directory.
#' @param save_output Logical. Write the enriched starters table to RDS + CSV.
#' @return The input lineup list with starters enriched (adds alt_player,
#'   alt_position, alt_source, alt_adj_proj, p_start_correct, avg_miss,
#'   expected_regret, stakes, uq_schema_tag), a week_risk_score element, and a
#'   def_streaming element (a ranked available-DEF table when the DEF slot is
#'   open and the waiver inputs are supplied, otherwise NULL).
#' @seealso optimize_lineup, assemble_player_pool, calculate_def_st_points
#' @export
analyze_start_sit <- function(lineup, reconciled,
                              source = NULL, vorp = NULL, def_proj = NULL,
                              week = NULL, dvp = NULL, def_factors = NULL,
                              def_seasons = SEASON - 1L,
                              include_waiver_def = TRUE,
                              def_max_suggestions = 5L,
                              season = NULL,
                              cache_dir = CACHE_DIR_DEFAULT,
                              save_output = FALSE) {

  # ---- validate inputs ----
  if (!is.list(lineup) || is.null(lineup$starters) || is.null(lineup$bench)) {
    stop("lineup must be the list returned by optimize_lineup() ",
         "(with $starters and $bench).", call. = FALSE)
  }
  recon_need <- c("nfl_gsis_id", "r32_projection_lower_80",
                  "r32_projection_upper_80")
  recon_miss <- setdiff(recon_need, names(reconciled))
  if (length(recon_miss) > 0L) {
    stop(glue("reconciled missing columns: ",
              "{paste(recon_miss, collapse = ', ')}"), call. = FALSE)
  }

  starters <- lineup$starters
  bench    <- lineup$bench
  if (nrow(starters) == 0L) {
    message("analyze_start_sit(): lineup has no starters; nothing to enrich.")
    lineup$week_risk_score <- 0
    return(lineup)
  }
  season <- season %||% (if (!is.null(source)) source$season else NULL) %||% SEASON

  # ---- empirical DEF sigma (computed when a DEF is started; the streaming
  #      path below computes it on demand when the DEF slot is open instead) ----
  has_def_starter <- "DEF" %in% starters$position
  def_sigma <- if (has_def_starter) {
    .compute_def_sigma(seasons = def_seasons, cache_dir = cache_dir)
  } else {
    NA_real_
  }

  # ---- recover starter means and sigmas on the adjusted scale ----
  recon_iv <- reconciled %>%
    dplyr::select(nfl_gsis_id,
                  r32_projection_lower_80, r32_projection_upper_80)

  starters2 <- starters %>%
    dplyr::left_join(recon_iv, by = "nfl_gsis_id") %>%
    dplyr::mutate(
      mu_s = .data$adj_proj,
      # Condition is row-varying (position), so dplyr::if_else is correct here.
      sigma_s = dplyr::if_else(
        .data$position == "DEF",
        def_sigma * dplyr::coalesce(.data$matchup_factor, 1),
        .recover_sigma(.data$r32_projection_lower_80,
                       .data$r32_projection_upper_80) *
          dplyr::coalesce(.data$matchup_factor, 1)
      )
    )

  # ---- build the alternative pool ----
  # Offense alternatives: benched offensive players (bench retains the raw R/32
  # intervals and its own matchup_factor / adj_proj from R/35).
  off_alts <- bench %>%
    dplyr::filter(.data$position %in% LINEUP_OFFENSE_POSITIONS) %>%
    dplyr::transmute(
      position,
      alt_player = .data$player_name,
      alt_source = "bench",
      mu_a    = .data$adj_proj,
      sigma_a = .recover_sigma(.data$lower_80, .data$upper_80) *
        dplyr::coalesce(.data$matchup_factor, 1)
    )

  # DEF alternatives: benched DEFs, plus available waiver DEFs when requested
  # and the inputs to assemble them are present.
  def_alt_raw <- bench %>%
    dplyr::filter(.data$position == "DEF") %>%
    dplyr::transmute(position,
                     alt_player = .data$player_name,
                     alt_source = "bench",
                     mu_a = .data$adj_proj,
                     matchup_factor)

  if (has_def_starter && include_waiver_def) {
    have_waiver_inputs <- !is.null(source) && !is.null(vorp) &&
      !is.null(def_proj)
    if (have_waiver_inputs) {
      wd <- .assemble_waiver_def(source, reconciled, vorp, def_proj,
                                 week = week, dvp = dvp,
                                 def_factors = def_factors,
                                 season = season, cache_dir = cache_dir)
      if (nrow(wd) > 0L) {
        def_alt_raw <- dplyr::bind_rows(
          def_alt_raw,
          wd %>% dplyr::transmute(position,
                                  alt_player = .data$player_name,
                                  alt_source = "waiver",
                                  mu_a = .data$adj_proj,
                                  matchup_factor)
        )
      }
    } else {
      message("  include_waiver_def = TRUE but source/vorp/def_proj not all ",
              "supplied; DEF alternatives limited to the bench.")
    }
  }

  def_alts <- def_alt_raw %>%
    dplyr::mutate(
      sigma_a = def_sigma * dplyr::coalesce(.data$matchup_factor, 1)
    ) %>%
    dplyr::select(position, alt_player, alt_source, mu_a, sigma_a)

  alternatives <- dplyr::bind_rows(off_alts, def_alts)

  # ---- per-slot metrics ----
  metric_rows <- purrr::map_dfr(seq_len(nrow(starters2)), function(i) {
    row  <- starters2[i, ]
    elig <- .eligible_positions_for_slot(row$slot)
    cand <- alternatives %>% dplyr::filter(.data$position %in% elig)

    if (nrow(cand) == 0L) {
      return(tibble::tibble(
        alt_player = NA_character_, alt_position = NA_character_,
        alt_source = NA_character_, alt_adj_proj = NA_real_,
        p_start_correct = NA_real_, avg_miss = NA_real_,
        expected_regret = NA_real_, stakes = NA_character_
      ))
    }

    best <- cand %>%
      dplyr::slice_max(.data$mu_a, n = 1, with_ties = FALSE)

    p_corr <- .prob_start_correct(row$mu_s, row$sigma_s, best$mu_a, best$sigma_a)
    e_reg  <- .expected_regret(row$mu_s, row$sigma_s, best$mu_a, best$sigma_a)
    p_wrong <- 1 - p_corr
    a_miss <- if (is.na(p_wrong) || p_wrong < P_WRONG_FLOOR) {
      NA_real_
    } else {
      e_reg / p_wrong
    }

    tibble::tibble(
      alt_player      = best$alt_player,
      alt_position    = best$position,
      alt_source      = best$alt_source,
      alt_adj_proj    = round(best$mu_a, 2),
      p_start_correct = round(p_corr, 3),
      avg_miss        = round(a_miss, 2),
      expected_regret = round(e_reg, 2),
      stakes          = .stakes_label(e_reg)
    )
  })

  enriched <- dplyr::bind_cols(starters, metric_rows) %>%
    dplyr::mutate(uq_schema_tag = SCHEMA_TAG_UQ)

  week_risk_score <- sum(enriched$expected_regret, na.rm = TRUE)

  # ---- DEF streaming recommendation (open DEF slot) ----
  # When no DEF is in the optimal lineup, the actionable question is which
  # available defense to stream. The started-DEF path above cannot answer that
  # (there is no started DEF), so rank the available defenses here.
  def_streaming <- NULL
  if (!has_def_starter && include_waiver_def) {
    have_waiver_inputs <- !is.null(source) && !is.null(vorp) &&
      !is.null(def_proj)
    if (have_waiver_inputs) {
      if (is.na(def_sigma)) {
        def_sigma <- .compute_def_sigma(seasons = def_seasons,
                                        cache_dir = cache_dir)
      }
      def_streaming <- .recommend_def_stream(
        source, reconciled, vorp, def_proj, def_sigma,
        week = week, dvp = dvp, def_factors = def_factors,
        season = season, cache_dir = cache_dir,
        max_suggestions = def_max_suggestions)
    } else {
      message("  DEF slot is open but source/vorp/def_proj not all supplied; ",
              "skipping the DEF streaming recommendation.")
    }
  }

  # ---- optional save ----
  if (save_output) {
    dir.create(dirname(OUTPUT_RDS_PATH_UQ), recursive = TRUE,
               showWarnings = FALSE)
    saveRDS(enriched, OUTPUT_RDS_PATH_UQ)
    readr::write_csv(enriched, OUTPUT_CSV_PATH_UQ)
    message(glue("  Saved: {OUTPUT_RDS_PATH_UQ}"))
    message(glue("  Saved: {OUTPUT_CSV_PATH_UQ}"))
    if (!is.null(def_streaming) && nrow(def_streaming) > 0L) {
      saveRDS(def_streaming, OUTPUT_RDS_PATH_DEF)
      readr::write_csv(def_streaming, OUTPUT_CSV_PATH_DEF)
      message(glue("  Saved: {OUTPUT_RDS_PATH_DEF}"))
      message(glue("  Saved: {OUTPUT_CSV_PATH_DEF}"))
    }
  }

  # ---- KEY INSIGHTS (computed from the enriched output, never hardcoded) ----
  n_high  <- sum(enriched$stakes == "high", na.rm = TRUE)
  n_scored <- sum(!is.na(enriched$p_start_correct))
  wk_label <- if (is.null(week)) "preseason" else glue("week {week}")

  riskiest <- enriched %>%
    dplyr::filter(!is.na(.data$p_start_correct)) %>%
    dplyr::arrange(.data$p_start_correct) %>%
    dplyr::slice(1)

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/36: Start/sit uncertainty ({wk_label})"))
  message(glue("{strrep('=', 70)}"))
  message(glue("  Slots scored:        {n_scored} of {nrow(enriched)}"))
  message(glue("  High-stakes slots:   {n_high}"))
  message(glue("  Week risk score:     ",
               "{format(round(week_risk_score, 1), nsmall = 1)} pts at risk"))
  if (nrow(riskiest) == 1L) {
    message(glue("  Closest call:        {riskiest$player_name} ",
                 "({riskiest$slot}) at p={riskiest$p_start_correct} ",
                 "vs {riskiest$alt_player %||% 'NA'}"))
  }
  if (!is.null(def_streaming) && nrow(def_streaming) > 0L) {
    top <- def_streaming[1, ]
    opp_txt <- if (is.na(top$opponent)) "neutral" else glue("vs {top$opponent}")
    clarity_txt <- if (is.na(top$pick_clarity)) "only option" else
      top$pick_clarity
    message(glue("  DEF stream pick:     {top$def_team} {opp_txt} ",
                 "({format(round(top$adj_proj, 1), nsmall = 1)} pts, ",
                 "{clarity_txt})"))
  }
  message(glue("{strrep('=', 70)}\n"))

  lineup$starters        <- enriched
  lineup$week_risk_score <- week_risk_score
  lineup$def_streaming   <- def_streaming
  lineup
}
