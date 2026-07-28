# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Team Volume Projections
# File: R/30_team_volume_projections.R
#
# PURPOSE
# -------
# First foundation layer for team-aware projections (Option 3 architecture).
# Produces per-team 2026 projections for pass volume, rush volume, pace, and
# scoring rate. Output feeds R/31 player allocation, which then feeds R/32
# reconciliation back to R/29 priors.
#
# WHY THIS EXISTS
# ---------------
# R/29 projects players in isolation -- it gives Tai Felton the same 10.2 PPG
# whether he plays for MIN (Jefferson + Addison already eating the target pie)
# or for a team with no entrenched WRs. The model has no concept of team
# constraints. R/30 starts the fix by anchoring every projection to a team
# total that finite resources can be allocated against.
#
# THREE-PART ARCHITECTURE
# -----------------------
#   1. TEAM PATTERN -- each team's own 3-year historical PROE, pace, and
#      volume. The default signal source. Weight: 70%.
#
#   2. COACH PRIOR -- for teams with a 2026 HC change, pull the new HC's
#      prior team's PROE/pace. Captures scheme tendency that travels with
#      the coach. Weight: 30%. If no HC change, weight is 0 and team pattern
#      gets 100%.
#
#   3. QB QUALITY INDEX -- composite of CPOE and EPA per dropback from the
#      team's projected starting QB. Adjusts both volume (good QBs throw
#      more) and efficiency (good QBs convert at higher rates).
#
# DESIGN DECISIONS (confirmed in scoping)
# ---------------------------------------
#   - Historical window  : 3 prior seasons (2023-2025)
#   - Team/coach blend   : 70/30 if HC changed, else 100/0
#   - QB quality inputs  : CPOE z-score + EPA/dropback z-score, equal weight
#   - QB starter rule    : 2026 starter resolved from depth charts (primary),
#                          rosters + prior-season dropbacks (fallback), and an
#                          optional manual CSV override; prior-season CPOE/EPA
#                          are keyed by qb_id so a QB who changed teams brings
#                          his real signal to his new team
#   - Min dropbacks      : 200 per season for QB quality measurement
#   - Min games          : 14 per season for team historical inclusion
#   - Kneels/spikes      : Excluded from all volume and efficiency metrics
#   - Regular season     : Filter season_type == "REG" before any aggregation
#   - Coaching source    : Auto-detected from nflreadr::load_schedules()
#                          with optional manual CSV override
#
# COACHING CHANGES AUTO-DETECTION
# -------------------------------
# Primary source: nflreadr::load_schedules() exposes home_coach and away_coach
# columns (sourced from Pro-Football-Reference). The pipeline:
#   1. Pulls target season schedule (2026) -- each team's Week 1 HC
#   2. Pulls prior season schedule (2025) -- each team's final-week HC
#   3. Flags name mismatches as coaching changes
#   4. For each change, searches the 6 prior seasons for that HC's most
#      recent team with 8+ games coached -- that becomes prior_team
#
# If the target season schedule has not yet been ingested by nflverse (typical
# in early offseason), auto-detect returns empty and the pipeline falls back
# to the manual CSV.
#
# COACHING CHANGES CSV OVERRIDE (data/ref/coaching_changes_2026.csv)
# ------------------------------------------------------------------
# Optional. Use for:
#   - First-time NFL HCs (auto-detect produces NA prior_team)
#   - Cases where auto-detection picked the wrong prior team
#   - Interim-to-permanent promotions (no name change to detect)
#
# Required columns: team, new_hc, prior_team
#   team       = new team in 2026 (the team adopting the new HC)
#   new_hc     = HC's name (informational, not used in computation)
#   prior_team = team the HC was at in 2025 (or most recent prior team
#                with 8+ games coached)
#
# CSV entries override auto-detected entries for the same team. Teams not
# in the CSV keep their auto-detected entry (or have none if auto-detect
# found no change). An empty or missing file is valid.
#
# QB STARTER RESOLUTION (depth charts + rosters + optional CSV)
# ------------------------------------------------------------
# QB quality is anchored to each team's projected 2026 starter, resolved in
# three layers (mirroring the coaching-change pattern):
#   1. nflreadr::load_depth_charts(2026) -- top-ranked QB per team (primary)
#   2. nflreadr::load_rosters(2026) + prior-season dropbacks -- the roster QB
#      with the most prior dropbacks is the presumed starter (fallback, also
#      used to gap-fill teams the depth chart does not cover)
#   3. Optional manual CSV at data/ref/qb_changes_2026.csv (override)
#
# Prior-season CPOE/EPA are aggregated by qb_id across all teams the QB played
# for, so a quarterback who changed teams carries his real prior-season signal
# to his new team rather than the team being anchored to last year's starter.
#
# QB CHANGES CSV OVERRIDE (data/ref/qb_changes_2026.csv)
# ------------------------------------------------------
# Optional. All columns optional except team:
#   team                = 2026 team code (required)
#   qb_id               = gsis_id of the 2026 starter (reference-QB mode:
#                         the named QB's prior-season CPOE/EPA are z-scored)
#   qb_quality_override = direct z-score (direct-score mode: used verbatim,
#                         e.g. for a rookie #1 pick with no usable NFL signal)
#   qb_name             = optional, human readability only
#   source              = optional, documentation only
# If both qb_id and qb_quality_override are given, the direct score wins.
# CSV entries override the auto-detected starter for the same team. An empty
# or missing file is valid.
#
# OUTPUTS
# -------
#   data/season2_cache/s2_week15_team_volumes.rds
#   data/season2_cache/s2_week15_team_volumes.csv
#
# 32 rows, one per active 2026 NFL team. Schema:
#   team                       chr   3-letter team code
#   season                     int   2026
#   games_observed             int   Historical games used (varies by team)
#   coach_change_flag          lgl   TRUE if 2026 HC changed
#   prior_hc_team              chr   Prior team for the HC (NA if no change)
#   historical_pass_pg         dbl   Team's own 3-year pass attempts per game
#   historical_rush_pg         dbl   Team's own 3-year rush attempts per game
#   historical_plays_pg        dbl   Team's own 3-year plays per game
#   historical_proe            dbl   Team's own 3-year PROE
#   historical_pass_yds_pg     dbl   Team's own 3-year passing yards per game
#   historical_rush_yds_pg     dbl   Team's own 3-year rushing yards per game
#   historical_pass_tds_pg     dbl   Team's own 3-year passing TDs per game
#   historical_rush_tds_pg     dbl   Team's own 3-year rushing TDs per game
#   coach_pass_pg              dbl   HC prior team's pass attempts per game (NA if none)
#   coach_rush_pg              dbl   HC prior team's rush attempts per game (NA if none)
#   coach_proe                 dbl   HC prior team's PROE (NA if none)
#   blended_pass_pg            dbl   70/30 blend with coach pattern
#   blended_rush_pg            dbl   70/30 blend with coach pattern
#   blended_plays_pg           dbl   70/30 blend with coach pattern
#   blended_proe               dbl   70/30 blend with coach pattern
#   qb_quality_score           dbl   Z-score composite of CPOE + EPA/dropback
#   pass_sos_factor            dbl   Pass strength-of-schedule multiplier
#                                      (>1 = easier pass schedule, applied to
#                                       pass volume and efficiency)
#   rush_sos_factor            dbl   Rush strength-of-schedule multiplier
#                                      (>1 = easier rush schedule, applied to
#                                       rush volume and efficiency)
#   projected_pass_pg          dbl   FINAL projection: blended * qb adj * SOS
#   projected_rush_pg          dbl   FINAL projection: blended * (1 - qb adjustment)
#   projected_plays_pg         dbl   FINAL projection: pace
#   projected_pass_yds_pg      dbl   FINAL projection: historical scaled by qb_quality
#   projected_rush_yds_pg      dbl   FINAL projection: historical (no qb adj)
#   projected_pass_tds_pg      dbl   FINAL projection: historical scaled by qb_quality
#   projected_rush_tds_pg      dbl   FINAL projection: historical
#   schema_tag                 chr   "s2_w15_team_vol_v2"
#
# SOURCE DEPENDENCIES
# -------------------
#   R/15_multi_season_pbp.R -- load_normalized_season()
#   R/29_projection_engine.R -- compute_prior_weight() (in-season SOS taper)
#
# RUN
# ---
#   source(here::here("R", "30_team_volume_projections.R"))
#   team_vols <- project_team_volumes()                  # preseason
#   team_vols <- project_team_volumes(as_of_week = 6)    # in-season (with
#                                                          current_season_sos)
#
# Author: Christian K. LeBlanc
# Version: 2.0
#
# CHANGELOG
# ---------
# 2.0  Strength-of-schedule (SOS) v2 added. Defensive quality measured as EPA
#      allowed per play type (pass/rush) relative to league average, following
#      R/10's EPA-not-yards principle. Two preseason components: retrospective
#      (recency-weighted quality of opponents actually faced over the 3 prior
#      seasons) and forward (recency-weighted quality of the published 2026
#      opponents). New optional as_of_week argument tapers the preseason SOS
#      signal toward observed current-season opponent quality using R/29's
#      compute_prior_weight() decay curve, so the SOS leg updates in-season on
#      the same schedule as the rest of the engine. SOS adjusts both volume
#      (smaller sensitivity) and efficiency (larger sensitivity), with separate
#      pass/rush factors bounded to 0.80-1.20. The sensitivities converting
#      the SOS signal into factors are FITTED FROM DATA each run by
#      .calibrate_sos_sensitivity(): team-game relative efficiency and volume
#      regressed on leave-one-out opponent defensive EPA with team-season
#      fixed effects (within transformation), four separate slopes
#      (pass/rush x efficiency/volume). Fallback constants exist only for
#      calibration failure and warn loudly. Two new output columns:
#      pass_sos_factor, rush_sos_factor. Schema tag -> s2_w15_team_vol_v2.
# 1.1  QB quality made player-centric and starter-aware. Prior-season CPOE/EPA
#      now keyed by qb_id; each 2026 team mapped to its resolved starter via
#      depth charts (primary), rosters + dropbacks (fallback), and an optional
#      qb_changes_2026.csv override. Fixes offseason QB-movement bug where a
#      team was anchored to the prior season's top-snap QB regardless of who
#      actually starts in 2026. project_team_volumes() gains qb_changes_path.
# 1.0  Initial release.
# ==============================================================================

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(here)
library(glue)
library(nflfastR)
library(nflreadr)

source(here::here("R", "15_multi_season_pbp.R"))

# compute_prior_weight() is the project-wide validated in-season decay curve
# (defined in R/29, reused by R/34). It is the single source of truth for how
# prior-season information tapers toward live current-season data: week 1 gives
# the prior full weight, the crossover is ~week 9, and the prior never falls
# below a 0.05 floor. R/30 reuses the identical function so the SOS in-season
# blend tapers on exactly the same schedule as the rest of the engine -- no
# divergence. Guarded source mirrors R/29's own idempotent pattern: in a full
# chain run R/29 is already loaded and this is a no-op; a standalone R/30 run
# triggers the source and fails loudly if R/29 cannot load (preferable to a
# silently drifting local copy of the curve).
if (!exists("compute_prior_weight")) {
  source(here::here("R", "29_projection_engine.R"))
}

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

SEASON <- 2026L
HISTORICAL_WINDOW <- 3L
HISTORICAL_SEASONS <- (SEASON - HISTORICAL_WINDOW):(SEASON - 1L)

# Blend weights
TEAM_PATTERN_WEIGHT <- 1.00
COACH_PATTERN_WEIGHT <- 0.00

# Quality thresholds
MIN_GAMES_TEAM_SEASON <- 14L   # Of 17 regular season games, allow some slack
MIN_DROPBACKS_QB <- 200L       # Roughly 14 games at modern dropback rates

# QB quality adjustment magnitude
# A QB at +2 sigma on both CPOE and EPA gets the team a +6% pass volume
# bump and a +6% pass efficiency bump. At -2 sigma, symmetric -6%.
QB_VOLUME_SENSITIVITY <- 0.015
QB_EFFICIENCY_SENSITIVITY <- 0.015

# ------------------------------------------------------------------------------
# STRENGTH OF SCHEDULE (SOS) -- v2
# ------------------------------------------------------------------------------
# SOS adjusts projected volume and efficiency for the quality of defenses a
# team faces. Defensive quality is measured as EPA allowed per play, split by
# play type (pass vs rush), expressed relative to league average. This metric
# choice follows R/10's classify_defensive_style(): EPA allowed, not yards
# allowed, because yards conflate defensive quality with game script.
#
# Two components, blended by the in-season decay curve:
#   - HISTORICAL (retrospective): for each team, the recency-weighted average
#     defensive quality of the opponents it actually faced over the 3 prior
#     seasons. Corrects the historical volume base for past schedule luck.
#   - FORWARD: for each team, the average defensive quality of its 2026
#     opponents (from the published schedule), each opponent scored by that
#     opponent's own recency-weighted historical defense. Answers "is the
#     2026 slate hard or easy?"
#
# In-season blend (as_of_week): before the season (as_of_week = NULL) the SOS
# factor is 100% historical+forward preseason signal. Once games are played,
# compute_prior_weight(as_of_week) tapers the preseason SOS toward the team's
# observed current-season opponent quality, on the same curve R/29 and R/34
# use. The current-season opponent quality is supplied by the caller via
# current_season_def (optional); absent it, the preseason signal is retained.
#
# SENSITIVITY CALIBRATION: the sensitivities that convert the SOS signal
# (opponent EPA allowed vs league) into multiplicative factors are FITTED FROM
# DATA at run time by .calibrate_sos_sensitivity(), not hardcoded. The
# calibration regresses team-game relative efficiency (yards per play vs
# league) and relative volume (plays per game vs league) on leave-one-out
# opponent defensive EPA, with team-season fixed effects (within
# transformation) so the slope is identified from how the SAME offense
# performs against varying defenses -- not from comparing different offenses.
# Separate slopes are fitted for pass/rush and efficiency/volume (4 total).

# FALLBACK sensitivities, used ONLY if the empirical calibration fails (cache
# unavailable, insufficient sample). These are unvalidated reasoned priors,
# not fitted values -- the same status as the QB2/QB3 depth multipliers in
# R/32. A run that falls back warns loudly.
SOS_VOLUME_SENSITIVITY_FALLBACK     <- 0.15
SOS_EFFICIENCY_SENSITIVITY_FALLBACK <- 0.40

# Minimum plays of a given type in a team-game for that game to enter the
# sensitivity calibration (ratio outcomes are unstable on tiny denominators),
# and minimum qualifying team-games for a fitted slope to be trusted.
SOS_CALIB_MIN_PLAYS_PER_GAME <- 10L
SOS_CALIB_MIN_GAMES          <- 100L

# Recency weights for the 3 prior seasons when averaging a defense's quality
# or a team's faced-schedule quality. Derived from the project in-season decay
# curve applied to season age (most recent season = age 1): normalized
# compute_prior_weight(1), (2), (3). Computed at load (see below) so the
# weights track the canonical curve rather than hardcoding 1.00/0.94/0.88.
SOS_SEASON_RECENCY_WEIGHTS <- local({
  raw <- vapply(seq_len(HISTORICAL_WINDOW), compute_prior_weight, numeric(1))
  raw / sum(raw)
})

# Minimum defensive plays (of a given type) for a team-season to be a valid
# SOS input. Below this the team-season is dropped from the opponent-quality
# average. Mirrors R/10's min_def_plays guard.
SOS_MIN_DEF_PLAYS <- 100L

# Bounds on the final SOS multipliers so an extreme schedule cannot dominate
# the projection. Mirrors the capped schedule multiplier used in the R/14
# capstone (0.80-1.20).
SOS_FACTOR_FLOOR <- 0.80
SOS_FACTOR_CEILING <- 1.20

# Paths
CACHE_DIR_DEFAULT <- here::here("data", "season2_cache")
COACHING_CHANGES_PATH <- here::here("data", "ref", "coaching_changes_2026.csv")
QB_CHANGES_PATH <- here::here("data", "ref", "qb_changes_2026.csv")

# Committee QB config (uncertain rooms). Season-guarded: only rows whose season
# matches the run season are applied, so 2026 overrides can never touch 2027.
QB_COMMITTEES_PATH <- here::here("data", "ref", "qb_committees_2026.csv")
# NAMESPACE GUARD  [2026-07-15]
# R/30 and R/34 both defined OUTPUT_RDS_PATH, OUTPUT_CSV_PATH and SCHEMA_TAG at
# top level with different values. R/42 sources R/34, so sourcing R/42 after R/30
# silently repointed R/30's writer at the def/ST filename: project_team_volumes()
# wrote 2026 team volumes to s2_week16_def_st_projections.rds and left
# s2_week15_team_volumes.rds stale, which every downstream board then read.
# Correctness must not depend on source order, so these are now R30_-prefixed.
# R/34 keeps the unprefixed names; nothing else defines them, so the collision
# is gone regardless of load order. R/29 already did this with R29_SCHEMA_TAG.
R30_OUTPUT_RDS_PATH <- here::here("data", "season2_cache",
                              "s2_week15_team_volumes.rds")
R30_OUTPUT_CSV_PATH <- here::here("data", "season2_cache",
                              "s2_week15_team_volumes.csv")

R30_SCHEMA_TAG <- "s2_w15_team_vol_v2"

# Active 2026 NFL teams. Used to filter the output to only active teams
# (excludes historical codes like STL, OAK, SD that may appear in older pbp).
ACTIVE_TEAMS_2026 <- c(
  "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
  "DAL", "DEN", "DET", "GB",  "HOU", "IND", "JAX", "KC",
  "LA",  "LAC", "LV",  "MIA", "MIN", "NE",  "NO",  "NYG",
  "NYJ", "PHI", "PIT", "SEA", "SF",  "TB",  "TEN", "WAS"
)

# Legacy team code mapping. Older pbp uses STL/OAK/SD; we map to LA/LV/LAC.
TEAM_CODE_MAP <- c(
  "STL" = "LA",
  "OAK" = "LV",
  "SD"  = "LAC"
)

# ------------------------------------------------------------------------------
# NSE DECLARATIONS
# ------------------------------------------------------------------------------

utils::globalVariables(c(
  "season", "season_type", "week", "game_id", "posteam", "defteam",
  "play_type", "pass", "rush", "qb_dropback", "qb_kneel", "qb_spike",
  "xpass", "epa", "cpoe", "passing_yards", "rushing_yards",
  "pass_touchdown", "rush_touchdown", "passer_player_id",
  "passer_player_name", "two_point_attempt", "team",
  "n_pass_attempts", "n_rush_attempts", "n_plays", "n_dropbacks",
  "pass_yards_total", "rush_yards_total", "pass_tds_total",
  "rush_tds_total", "proe_sum", "n_proe_eligible", "games",
  "pass_attempts_pg", "rush_attempts_pg", "plays_pg", "proe",
  "pass_yds_pg", "rush_yds_pg", "pass_tds_pg", "rush_tds_pg",
  "n_seasons", "historical_pass_pg", "historical_rush_pg",
  "historical_plays_pg", "historical_proe", "historical_pass_yds_pg",
  "historical_rush_yds_pg", "historical_pass_tds_pg",
  "historical_rush_tds_pg", "prior_team", "new_hc", "coach_pass_pg",
  "coach_rush_pg", "coach_plays_pg", "coach_proe", "coach_change_flag",
  "prior_hc_team", "blended_pass_pg", "blended_rush_pg",
  "blended_plays_pg", "blended_proe", "qb_id", "team_dropbacks",
  "qb_dropbacks_share", "qb_cpoe_mean", "qb_epa_per_db",
  "cpoe_z", "epa_z", "qb_quality_score", "qb_quality_adj_pass",
  "qb_quality_adj_eff", "projected_pass_pg", "projected_rush_pg",
  "projected_plays_pg", "projected_pass_yds_pg", "projected_rush_yds_pg",
  "projected_pass_tds_pg", "projected_rush_tds_pg",
  "schema_tag", "games_observed", "season_count",
  "home_team", "away_team", "home_coach", "away_coach",
  "coach", "coach_target", "coach_target_n", "coach_prior",
  "coach_prior_n", "coach_n", "n_games",
  "def_pass_epa_allowed", "def_rush_epa_allowed",
  "def_pass_plays", "def_rush_plays",
  "pass_epa_vs_league", "rush_epa_vs_league",
  "opponent", "opp_pass_quality", "opp_rush_quality",
  "hist_pass_sos", "hist_rush_sos",
  "fwd_pass_sos", "fwd_rush_sos",
  "cur_pass_sos", "cur_rush_sos",
  "pass_sos_signal", "rush_sos_signal",
  "pass_sos_factor", "rush_sos_factor",
  "pass_vol_factor", "rush_vol_factor",
  "recency_weight", "season_age",
  "pass_w", "rush_w", "pass_weight_sum", "rush_weight_sum",
  "yards_gained", "epa_sum", "n_plays", "yds", "opponent_team",
  "opp_total_epa", "opp_total_plays",
  "league_mean_epa", "league_ypp", "league_plays_per_tg",
  "x", "y", "y_eff", "y_vol", "xd", "yd", "team_season",
  ".data", "passer_id"
))

# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

# ------------------------------------------------------------------------------
# .normalize_team_codes
# ------------------------------------------------------------------------------

#' Map legacy team codes to current codes
#'
#' nflfastR pbp uses historical codes (STL, OAK, SD) for relocated franchises.
#' We map them to current codes so historical data joins cleanly to 2026
#' roster. Idempotent: codes already in the active set pass through.
#'
#' @param team_vec Character vector of team codes.
#' @return Character vector with legacy codes remapped.
#' @keywords internal
.normalize_team_codes <- function(team_vec) {
  out <- team_vec
  for (old_code in names(TEAM_CODE_MAP)) {
    out[out == old_code] <- TEAM_CODE_MAP[[old_code]]
  }
  out
}

# ------------------------------------------------------------------------------
# .normalize_coach_name
# ------------------------------------------------------------------------------

#' Normalize coach name for fuzzy joining across schedule pulls
#'
#' Strips middle initials with periods (e.g., "Sean P. Payton" -> "sean payton"),
#' trailing or embedded suffixes (Jr/Sr/II/III/IV), and all remaining
#' punctuation. Lowercases and collapses whitespace. Used to match coach
#' names between 2025 final-week and 2026 first-week schedule rows where
#' formatting may vary slightly.
#'
#' @param x Character vector of coach names.
#' @return Character vector of normalized names.
#' @keywords internal
.normalize_coach_name <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("\\b[a-z]\\.\\s+", "", x)
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("\\b(jr|sr|ii|iii|iv)\\b", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# ------------------------------------------------------------------------------
# .derive_coaching_changes_from_schedules
# ------------------------------------------------------------------------------

#' Auto-detect 2026 coaching changes from nflreadr schedule data
#'
#' Pulls schedules for the target season (default SEASON=2026) and the
#' prior season. Identifies each team's first-week coach in target season
#' and last-week coach in prior season, normalizes names, and flags
#' mismatches as coaching changes. For each change, searches the prior
#' history_seasons range for the new HC's most recent prior team with
#' at least min_games_prior games coached.
#'
#' If the target season schedule has not been ingested by nflverse yet
#' (typical in early offseason), returns empty and the pipeline falls
#' back to the manual CSV.
#'
#' First-time NFL HCs (no prior team in the data) are filtered out and
#' reported; they need a manual CSV entry to participate in the blend.
#'
#' @param season Integer. Target season (default SEASON).
#' @param history_seasons Integer vector. Range to search for prior HC
#'   tenures (default 6 seasons back from target).
#' @param min_games_prior Integer. Minimum games at prior team to qualify
#'   as the HC's prior_team (default 8).
#' @return Tibble: team, new_hc, prior_team. One row per detected change
#'   with a discoverable prior team.
#' @keywords internal
.derive_coaching_changes_from_schedules <- function(
    season = SEASON,
    history_seasons = (season - 6L):(season - 1L),
    min_games_prior = 8L) {

  empty_result <- tibble::tibble(
    team       = character(),
    new_hc     = character(),
    prior_team = character()
  )

  # Pull target season schedule
  current <- tryCatch(
    nflreadr::load_schedules(seasons = season),
    error = function(e) {
      message(glue("    Season {season} schedule load failed: {e$message}"))
      NULL
    }
  )

  if (is.null(current) || nrow(current) == 0L) {
    message(glue("    Season {season} schedule not yet available -- ",
                 "auto-detect skipped"))
    return(empty_result)
  }

  required_cols <- c("season", "week", "home_team", "away_team",
                      "home_coach", "away_coach")
  missing_cols <- setdiff(required_cols, names(current))
  if (length(missing_cols) > 0L) {
    message(glue("    Schedule missing required columns: ",
                 "{paste(missing_cols, collapse = ', ')} -- auto-detect skipped"))
    return(empty_result)
  }

  # season_type may be absent in pre-release schedule data
  has_season_type <- "season_type" %in% names(current)

  pivot_to_team_coach <- function(df) {
    df_filtered <- if (has_season_type) {
      df %>% dplyr::filter(.data$season_type == "REG" |
                             is.na(.data$season_type))
    } else {
      df
    }
    dplyr::bind_rows(
      df_filtered %>%
        dplyr::transmute(
          season = .data$season, week = .data$week,
          team = .data$home_team, coach = .data$home_coach
        ),
      df_filtered %>%
        dplyr::transmute(
          season = .data$season, week = .data$week,
          team = .data$away_team, coach = .data$away_coach
        )
    ) %>%
      dplyr::filter(!is.na(.data$team), !is.na(.data$coach),
                     nchar(.data$coach) > 0) %>%
      dplyr::mutate(team = .normalize_team_codes(.data$team))
  }

  # Target season first-week coach per team
  current_long <- pivot_to_team_coach(current)
  current_coach <- current_long %>%
    dplyr::group_by(.data$team) %>%
    dplyr::slice_min(.data$week, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$team, coach_target = .data$coach) %>%
    dplyr::mutate(coach_target_n = .normalize_coach_name(.data$coach_target))

  # Prior season last-week coach per team
  prior <- tryCatch(
    nflreadr::load_schedules(seasons = season - 1L),
    error = function(e) NULL
  )

  if (is.null(prior) || nrow(prior) == 0L) {
    message(glue("    Prior season ({season - 1L}) schedule unavailable -- ",
                 "cannot compare"))
    return(empty_result)
  }

  prior_long <- pivot_to_team_coach(prior)
  prior_coach <- prior_long %>%
    dplyr::group_by(.data$team) %>%
    dplyr::slice_max(.data$week, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$team, coach_prior = .data$coach) %>%
    dplyr::mutate(coach_prior_n = .normalize_coach_name(.data$coach_prior))

  # Identify name mismatches
  changes <- current_coach %>%
    dplyr::left_join(prior_coach, by = "team") %>%
    dplyr::filter(
      .data$team %in% ACTIVE_TEAMS_2026,
      !is.na(.data$coach_target), !is.na(.data$coach_prior),
      .data$coach_target_n != .data$coach_prior_n
    )

  if (nrow(changes) == 0L) {
    message("    No coaching changes detected between prior and target seasons")
    return(empty_result)
  }

  # Look up prior team for each changed HC from historical schedules
  history <- tryCatch(
    nflreadr::load_schedules(seasons = history_seasons),
    error = function(e) NULL
  )

  if (is.null(history) || nrow(history) == 0L) {
    message("    Historical schedules unavailable -- prior_team will be NA")
    return(changes %>%
             dplyr::transmute(
               team       = .data$team,
               new_hc     = .data$coach_target,
               prior_team = NA_character_
             ))
  }

  history_long <- pivot_to_team_coach(history) %>%
    dplyr::mutate(coach_n = .normalize_coach_name(.data$coach))

  # Coach-team tenure counts; filter to qualifying tenures
  coach_tenure <- history_long %>%
    dplyr::group_by(.data$coach_n, .data$team, .data$season) %>%
    dplyr::summarise(n_games = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(.data$n_games >= min_games_prior)

  # Most recent prior team for each changed coach
  prior_team_map <- coach_tenure %>%
    dplyr::filter(.data$coach_n %in% changes$coach_target_n) %>%
    dplyr::group_by(.data$coach_n) %>%
    dplyr::slice_max(.data$season, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$coach_n, prior_team = .data$team)

  result <- changes %>%
    dplyr::left_join(prior_team_map,
                      by = c("coach_target_n" = "coach_n")) %>%
    dplyr::transmute(
      team       = .data$team,
      new_hc     = .data$coach_target,
      prior_team = .data$prior_team
    )

  # Report first-time HCs (no prior team found)
  first_time <- result %>% dplyr::filter(is.na(.data$prior_team))
  if (nrow(first_time) > 0L) {
    message(glue("    First-time NFL HCs (need manual CSV entry for blend): ",
                 "{paste(first_time$new_hc, collapse = ', ')}"))
  }

  result <- result %>%
    dplyr::filter(.data$prior_team %in% ACTIVE_TEAMS_2026,
                   !is.na(.data$prior_team))

  message(glue("    Auto-detected {nrow(result)} coaching change(s) ",
               "with prior team resolved"))

  result
}

# ------------------------------------------------------------------------------
# .compute_team_season_aggregates
# ------------------------------------------------------------------------------

#' Compute per-team per-season volume aggregates from a single season's pbp
#'
#' Filters to regular season, drops kneels/spikes/two-point attempts, then
#' aggregates pass attempts, rush attempts, plays, yards, and TDs at the
#' team-season level.
#'
#' @param pbp Tibble. Output of load_normalized_season() for one season.
#' @param target_season Integer. The season number (used as the season column).
#' @return Tibble with one row per team for this season. Columns:
#'   team, season, games, n_pass_attempts, n_rush_attempts, n_plays,
#'   pass_yards_total, rush_yards_total, pass_tds_total, rush_tds_total,
#'   proe_sum, n_proe_eligible.
#' @keywords internal
.compute_team_season_aggregates <- function(pbp, target_season) {

  if (is.null(pbp) || nrow(pbp) == 0L) {
    return(tibble::tibble(
      team               = character(),
      season             = integer(),
      games              = integer(),
      n_pass_attempts    = integer(),
      n_rush_attempts    = integer(),
      n_plays            = integer(),
      pass_yards_total   = numeric(),
      rush_yards_total   = numeric(),
      pass_tds_total     = numeric(),
      rush_tds_total     = numeric(),
      proe_sum           = numeric(),
      n_proe_eligible    = integer()
    ))
  }

  # Filter: regular season only, exclude kneels/spikes/two-point attempts
  pbp_use <- pbp %>%
    dplyr::filter(
      .data$season_type == "REG",
      !is.na(.data$posteam),
      nchar(.data$posteam) > 0
    ) %>%
    dplyr::mutate(
      posteam = .normalize_team_codes(.data$posteam),
      qb_kneel = dplyr::coalesce(.data$qb_kneel, 0L),
      qb_spike = dplyr::coalesce(.data$qb_spike, 0L),
      two_point_attempt = dplyr::coalesce(.data$two_point_attempt, 0L),
      pass = dplyr::coalesce(.data$pass, 0L),
      rush = dplyr::coalesce(.data$rush, 0L)
    ) %>%
    dplyr::filter(
      .data$qb_kneel == 0L,
      .data$qb_spike == 0L,
      .data$two_point_attempt == 0L
    )

  if (nrow(pbp_use) == 0L) {
    return(tibble::tibble(
      team = character(), season = integer(), games = integer(),
      n_pass_attempts = integer(), n_rush_attempts = integer(),
      n_plays = integer(), pass_yards_total = numeric(),
      rush_yards_total = numeric(), pass_tds_total = numeric(),
      rush_tds_total = numeric(), proe_sum = numeric(),
      n_proe_eligible = integer()
    ))
  }

  # Detect xpass availability: in some older seasons this column may be NA
  # for many plays. We compute PROE only on plays where xpass is non-NA.
  has_xpass <- "xpass" %in% names(pbp_use)
  if (!has_xpass) {
    pbp_use$xpass <- NA_real_
  }

  # Aggregate at team-season level
  pbp_use %>%
    dplyr::group_by(team = .data$posteam) %>%
    dplyr::summarise(
      season             = target_season,
      games              = dplyr::n_distinct(.data$game_id),
      n_pass_attempts    = sum(.data$pass == 1L, na.rm = TRUE),
      n_rush_attempts    = sum(.data$rush == 1L, na.rm = TRUE),
      n_plays            = dplyr::n(),
      pass_yards_total   = sum(dplyr::coalesce(.data$passing_yards, 0),
                                na.rm = TRUE),
      rush_yards_total   = sum(dplyr::coalesce(.data$rushing_yards, 0),
                                na.rm = TRUE),
      pass_tds_total     = sum(dplyr::coalesce(.data$pass_touchdown, 0L),
                                na.rm = TRUE),
      rush_tds_total     = sum(dplyr::coalesce(.data$rush_touchdown, 0L),
                                na.rm = TRUE),
      # PROE: mean of (pass_actual - pass_expected) over qualifying plays.
      # We sum the differences and count eligible plays separately so we
      # can compute the team's true mean when joining across seasons.
      proe_sum           = sum(
        dplyr::if_else(
          !is.na(.data$xpass) & (.data$pass == 1L | .data$rush == 1L),
          .data$pass - .data$xpass,
          NA_real_
        ),
        na.rm = TRUE
      ),
      n_proe_eligible    = sum(
        !is.na(.data$xpass) & (.data$pass == 1L | .data$rush == 1L),
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

# ------------------------------------------------------------------------------
# .compute_team_historical_volume
# ------------------------------------------------------------------------------

#' Aggregate team-season aggregates across the 3-year window
#'
#' Loads each season via load_normalized_season(), computes season-level
#' team aggregates, then averages per-game rates across qualifying seasons
#' (games >= MIN_GAMES_TEAM_SEASON in that season).
#'
#' Memory management: each season is loaded, aggregated, then dropped from
#' memory before the next season loads.
#'
#' @param cache_dir Character. R/15 cache directory path.
#' @param seasons Integer vector. Seasons to include (default HISTORICAL_SEASONS).
#' @return Tibble with one row per team. Per-game rate columns plus the
#'   number of qualifying seasons and total games observed.
#' @keywords internal
.compute_team_historical_volume <- function(cache_dir = CACHE_DIR_DEFAULT,
                                             seasons = HISTORICAL_SEASONS) {

  message(glue("  Loading {length(seasons)} historical seasons: ",
               "{paste(seasons, collapse = ', ')}"))

  all_aggs <- purrr::map_dfr(seasons, function(s) {

    pbp <- tryCatch(
      load_normalized_season(s, cache_dir = cache_dir),
      error = function(e) {
        message(glue("    Season {s}: load failed -- {e$message}"))
        NULL
      }
    )

    if (is.null(pbp)) return(NULL)

    agg <- .compute_team_season_aggregates(pbp, target_season = s)

    # Free memory before next season
    rm(pbp)
    invisible(gc(verbose = FALSE))

    agg
  })

  if (nrow(all_aggs) == 0L) {
    stop(".compute_team_historical_volume(): no seasons loaded successfully.")
  }

  # Filter to qualifying team-seasons, then aggregate to team level
  team_hist <- all_aggs %>%
    dplyr::filter(.data$games >= MIN_GAMES_TEAM_SEASON) %>%
    dplyr::mutate(
      pass_attempts_pg = .data$n_pass_attempts / .data$games,
      rush_attempts_pg = .data$n_rush_attempts / .data$games,
      plays_pg         = .data$n_plays / .data$games,
      pass_yds_pg      = .data$pass_yards_total / .data$games,
      rush_yds_pg      = .data$rush_yards_total / .data$games,
      pass_tds_pg      = .data$pass_tds_total / .data$games,
      rush_tds_pg      = .data$rush_tds_total / .data$games,
      proe             = dplyr::if_else(
        .data$n_proe_eligible > 0L,
        .data$proe_sum / .data$n_proe_eligible,
        NA_real_
      )
    ) %>%
    dplyr::group_by(.data$team) %>%
    dplyr::summarise(
      season_count           = dplyr::n(),
      games_observed         = sum(.data$games),
      historical_pass_pg     = mean(.data$pass_attempts_pg, na.rm = TRUE),
      historical_rush_pg     = mean(.data$rush_attempts_pg, na.rm = TRUE),
      historical_plays_pg    = mean(.data$plays_pg, na.rm = TRUE),
      historical_proe        = mean(.data$proe, na.rm = TRUE),
      historical_pass_yds_pg = mean(.data$pass_yds_pg, na.rm = TRUE),
      historical_rush_yds_pg = mean(.data$rush_yds_pg, na.rm = TRUE),
      historical_pass_tds_pg = mean(.data$pass_tds_pg, na.rm = TRUE),
      historical_rush_tds_pg = mean(.data$rush_tds_pg, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$team %in% ACTIVE_TEAMS_2026)

  message(glue("  Team historical: {nrow(team_hist)} active teams aggregated"))

  team_hist
}

# ------------------------------------------------------------------------------
# .load_coaching_changes
# ------------------------------------------------------------------------------

#' Load 2026 coaching changes (auto-detect with optional manual override)
#'
#' Combines two sources:
#'   1. Auto-detection from nflreadr::load_schedules() comparing target
#'      season's first-week coach to prior season's last-week coach
#'   2. Optional manual CSV at COACHING_CHANGES_PATH for overrides
#'
#' CSV entries (matched by team) replace auto-detected entries for the
#' same team. The CSV is the right place to:
#'   - Add first-time NFL HCs (auto-detect produces NA prior_team)
#'   - Correct an auto-detected prior team that picked the wrong tenure
#'   - Handle interim-to-permanent promotions (no name change to detect)
#'
#' If both sources are empty, the pipeline degrades to team-pattern-only
#' blending.
#'
#' @param path Character. Path to the optional CSV override file.
#' @param season Integer. Target season for auto-detection (default SEASON).
#' @return Tibble: team, new_hc, prior_team.
#' @keywords internal
.load_coaching_changes <- function(path = COACHING_CHANGES_PATH,
                                    season = SEASON) {

  # Step 1: Auto-detect from schedules
  message("  Auto-detecting coaching changes from nflreadr::load_schedules()")
  auto_changes <- tryCatch(
    .derive_coaching_changes_from_schedules(season = season),
    error = function(e) {
      message(glue("    Auto-detect error: {e$message}"))
      tibble::tibble(team = character(), new_hc = character(),
                      prior_team = character())
    }
  )

  # Step 2: Load manual CSV overrides (optional)
  manual <- tibble::tibble(team = character(), new_hc = character(),
                           prior_team = character())

  if (file.exists(path)) {
    raw_csv <- tryCatch(
      readr::read_csv(path, show_col_types = FALSE),
      error = function(e) {
        message(glue("  CSV read failed: {e$message}"))
        NULL
      }
    )

    if (!is.null(raw_csv) && nrow(raw_csv) > 0L) {
      required <- c("team", "new_hc", "prior_team")
      missing_cols <- setdiff(required, names(raw_csv))
      if (length(missing_cols) > 0L) {
        message(glue("  CSV missing columns: ",
                     "{paste(missing_cols, collapse = ', ')} -- ignoring CSV"))
      } else {
        manual <- raw_csv %>%
          dplyr::select(dplyr::all_of(required)) %>%
          dplyr::mutate(
            team       = .normalize_team_codes(as.character(.data$team)),
            prior_team = .normalize_team_codes(as.character(.data$prior_team))
          ) %>%
          dplyr::filter(
            .data$team %in% ACTIVE_TEAMS_2026,
            .data$prior_team %in% ACTIVE_TEAMS_2026,
            !is.na(.data$team), !is.na(.data$prior_team)
          )
      }
    }
  }

  # Step 3: Merge -- CSV entries override auto-detected ones for same team
  if (nrow(manual) > 0L) {
    result <- auto_changes %>%
      dplyr::filter(!.data$team %in% manual$team) %>%
      dplyr::bind_rows(manual)
  } else {
    result <- auto_changes
  }

  message(glue("  Coaching changes: {nrow(auto_changes)} auto-detected, ",
               "{nrow(manual)} manual override(s), {nrow(result)} total"))

  if (nrow(result) == 0L) {
    message("  No coaching changes from either source -- ",
            "team-pattern-only blending will apply")
  }

  result
}

# ------------------------------------------------------------------------------
# .compute_coach_prior_pattern
# ------------------------------------------------------------------------------

#' Join coaching changes to prior team historical patterns
#'
#' For each team with a 2026 HC change, pulls the HC's prior team's
#' historical pass/rush/proe pattern. Returns a tibble suitable for
#' joining to the team historical pattern by the new team's code.
#'
#' @param coaching_changes Tibble. Output of .load_coaching_changes().
#' @param team_history Tibble. Output of .compute_team_historical_volume().
#' @return Tibble: team, coach_pass_pg, coach_rush_pg, coach_plays_pg,
#'   coach_proe. One row per coaching change.
#' @keywords internal
.compute_coach_prior_pattern <- function(coaching_changes, team_history) {

  if (nrow(coaching_changes) == 0L) {
    return(tibble::tibble(
      team            = character(),
      coach_pass_pg   = numeric(),
      coach_rush_pg   = numeric(),
      coach_plays_pg  = numeric(),
      coach_proe      = numeric()
    ))
  }

  coaching_changes %>%
    dplyr::left_join(
      team_history %>%
        dplyr::select(team, historical_pass_pg, historical_rush_pg,
                       historical_plays_pg, historical_proe) %>%
        dplyr::rename(
          prior_team             = .data$team,
          coach_pass_pg          = .data$historical_pass_pg,
          coach_rush_pg          = .data$historical_rush_pg,
          coach_plays_pg         = .data$historical_plays_pg,
          coach_proe             = .data$historical_proe
        ),
      by = "prior_team"
    ) %>%
    dplyr::select(.data$team, .data$coach_pass_pg, .data$coach_rush_pg,
                   .data$coach_plays_pg, .data$coach_proe)
}

# ------------------------------------------------------------------------------
# .blend_team_and_coach_pattern
# ------------------------------------------------------------------------------

#' Apply 70/30 team/coach blend to volume metrics
#'
#' For teams with a coach prior pattern, blend at 70% team + 30% coach.
#' For teams without a coach change, blended = historical (100% team).
#' Uses dplyr::if_else() row-wise on the coach_change_flag.
#'
#' @param team_history Tibble from .compute_team_historical_volume().
#' @param coach_prior Tibble from .compute_coach_prior_pattern().
#' @return Tibble with original team_history columns plus blended_*
#'   columns and coach_change_flag.
#' @keywords internal
.blend_team_and_coach_pattern <- function(team_history, coach_prior) {

  blended <- team_history %>%
    dplyr::left_join(coach_prior, by = "team") %>%
    dplyr::mutate(
      coach_change_flag = !is.na(.data$coach_pass_pg),
      blended_pass_pg = dplyr::if_else(
        .data$coach_change_flag,
        TEAM_PATTERN_WEIGHT * .data$historical_pass_pg +
          COACH_PATTERN_WEIGHT * .data$coach_pass_pg,
        .data$historical_pass_pg
      ),
      blended_rush_pg = dplyr::if_else(
        .data$coach_change_flag,
        TEAM_PATTERN_WEIGHT * .data$historical_rush_pg +
          COACH_PATTERN_WEIGHT * .data$coach_rush_pg,
        .data$historical_rush_pg
      ),
      blended_plays_pg = dplyr::if_else(
        .data$coach_change_flag,
        TEAM_PATTERN_WEIGHT * .data$historical_plays_pg +
          COACH_PATTERN_WEIGHT * .data$coach_plays_pg,
        .data$historical_plays_pg
      ),
      blended_proe = dplyr::if_else(
        .data$coach_change_flag,
        TEAM_PATTERN_WEIGHT * .data$historical_proe +
          COACH_PATTERN_WEIGHT * .data$coach_proe,
        .data$historical_proe
      )
    )

  blended
}

# ------------------------------------------------------------------------------
# .pick_first_present
# ------------------------------------------------------------------------------

#' Return the first candidate name present in a character vector
#'
#' Defensive column resolution helper for nflreadr sources whose schemas
#' differ across versions and seasons. Returns NA_character_ if no candidate
#' is present.
#'
#' @param candidates Character. Candidate column names in priority order.
#' @param available Character. Names actually present (e.g., names(df)).
#' @return Character scalar (first match) or NA_character_.
#' @keywords internal
.pick_first_present <- function(candidates, available) {
  hit <- candidates[candidates %in% available]
  if (length(hit) == 0L) NA_character_ else hit[1]
}

# ------------------------------------------------------------------------------
# .pick_starter_by_dropbacks
# ------------------------------------------------------------------------------

#' Pick one starter per team as the QB with the most prior-season dropbacks
#'
#' Pure selection helper (no I/O) used by the roster fallback. Given the QBs
#' assigned to each team plus their prior-season dropback counts, returns the
#' single highest-dropback QB per team. Ties are broken deterministically
#' (with_ties = FALSE keeps the first after ordering), and QBs with no
#' prior-season dropbacks score 0 so a team of all-new QBs still yields a
#' (neutral-signal) pick rather than dropping out.
#'
#' @param qbs Tibble with columns team, qb_id, n_dropbacks.
#' @return Tibble: team, starter_qb_id (one row per team).
#' @keywords internal
.pick_starter_by_dropbacks <- function(qbs) {
  if (is.null(qbs) || nrow(qbs) == 0L) {
    return(tibble::tibble(team = character(), starter_qb_id = character()))
  }
  qbs %>%
    dplyr::mutate(n_dropbacks = dplyr::coalesce(.data$n_dropbacks, 0L)) %>%
    dplyr::group_by(.data$team) %>%
    dplyr::slice_max(.data$n_dropbacks, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(team = .data$team, starter_qb_id = .data$qb_id)
}

# ------------------------------------------------------------------------------
# .merge_qb_starter_override
# ------------------------------------------------------------------------------

#' Apply CSV starter overrides on top of auto-detected starters
#'
#' Pure merge helper (no I/O). CSV rows replace the auto-detected starter for
#' the same team, mirroring the coaching-change override semantics. Optional
#' CSV columns qb_id and qb_quality_override are tolerated when absent. Rows
#' with a team outside ACTIVE_TEAMS_2026 (or a missing team) are dropped.
#'
#' @param starters Tibble: team, starter_qb_id, starter_source,
#'   qb_quality_override.
#' @param raw_csv Tibble or NULL. Parsed CSV; must contain a team column to
#'   take effect.
#' @return Tibble with the same schema as starters, overrides applied.
#' @keywords internal
.merge_qb_starter_override <- function(starters, raw_csv) {
  if (is.null(raw_csv) || nrow(raw_csv) == 0L ||
      !("team" %in% names(raw_csv))) {
    return(starters)
  }

  if (!"qb_id" %in% names(raw_csv)) raw_csv$qb_id <- NA_character_
  if (!"qb_quality_override" %in% names(raw_csv)) {
    raw_csv$qb_quality_override <- NA_real_
  }

  ov <- raw_csv %>%
    dplyr::transmute(
      team                = .normalize_team_codes(as.character(.data$team)),
      starter_qb_id       = as.character(.data$qb_id),
      starter_source      = "csv_override",
      qb_quality_override = suppressWarnings(as.numeric(.data$qb_quality_override))
    ) %>%
    dplyr::filter(.data$team %in% ACTIVE_TEAMS_2026, !is.na(.data$team))

  if (nrow(ov) == 0L) return(starters)

  starters %>%
    dplyr::filter(!.data$team %in% ov$team) %>%
    dplyr::bind_rows(ov)
}

# ------------------------------------------------------------------------------
# .score_qb_starters
# ------------------------------------------------------------------------------

#' Convert resolved starters + prior-season stats into the QB quality table
#'
#' Pure statistical core (no I/O). Z-scores each qualifying starter's CPOE and
#' EPA against the distribution of qualifying starters and resolves the final
#' quality score with this precedence:
#'   1. Direct override (qb_quality_override non-NA) used verbatim
#'   2. Computed z-score mean if the starter qualifies (>= min_dropbacks)
#'   3. Neutral 0 otherwise
#'
#' A starter "qualifies" for z-scoring only when it has >= min_dropbacks
#' prior-season dropbacks AND no direct override. The qualifies flag is built
#' from is.na()/comparison operators so it never contains NA, which keeps the
#' row-varying dplyr::if_else() calls valid.
#'
#' @param starter_stats Tibble: team, starter_qb_id, qb_quality_override,
#'   n_dropbacks, qb_cpoe_mean, qb_epa_per_db.
#' @param min_dropbacks Integer. Qualification threshold (default MIN_DROPBACKS_QB).
#' @return Tibble: team, qb_id, qb_dropbacks_share, qb_cpoe_mean, qb_epa_per_db,
#'   cpoe_z, epa_z, qb_quality_score.
#' @keywords internal
.score_qb_starters <- function(starter_stats,
                               min_dropbacks = MIN_DROPBACKS_QB) {

  pool <- starter_stats %>%
    dplyr::filter(
      is.na(.data$qb_quality_override),
      !is.na(.data$n_dropbacks),
      .data$n_dropbacks >= min_dropbacks
    )

  cpoe_mean <- mean(pool$qb_cpoe_mean, na.rm = TRUE)
  cpoe_sd   <- stats::sd(pool$qb_cpoe_mean, na.rm = TRUE)
  epa_mean  <- mean(pool$qb_epa_per_db, na.rm = TRUE)
  epa_sd    <- stats::sd(pool$qb_epa_per_db, na.rm = TRUE)

  if (is.na(cpoe_mean)) cpoe_mean <- 0
  if (is.na(epa_mean))  epa_mean  <- 0
  if (is.na(cpoe_sd) || cpoe_sd <= 0) cpoe_sd <- 1
  if (is.na(epa_sd)  || epa_sd  <= 0) epa_sd  <- 1

  starter_stats %>%
    dplyr::mutate(
      qualifies = !is.na(.data$n_dropbacks) &
        .data$n_dropbacks >= min_dropbacks &
        is.na(.data$qb_quality_override),
      cpoe_z = dplyr::if_else(
        .data$qualifies, (.data$qb_cpoe_mean - cpoe_mean) / cpoe_sd, 0
      ),
      epa_z = dplyr::if_else(
        .data$qualifies, (.data$qb_epa_per_db - epa_mean) / epa_sd, 0
      ),
      qb_quality_score = dplyr::case_when(
        !is.na(.data$qb_quality_override) ~ .data$qb_quality_override,
        .data$qualifies                   ~ (.data$cpoe_z + .data$epa_z) / 2,
        TRUE                              ~ 0
      ),
      qb_id              = .data$starter_qb_id,
      qb_dropbacks_share = NA_real_
    ) %>%
    dplyr::select(.data$team, .data$qb_id, .data$qb_dropbacks_share,
                   .data$qb_cpoe_mean, .data$qb_epa_per_db,
                   .data$cpoe_z, .data$epa_z, .data$qb_quality_score)
}

# ------------------------------------------------------------------------------
# .detect_starters_from_depth_charts
# ------------------------------------------------------------------------------

#' Detect each team's QB1 from nflreadr depth charts (primary source)
#'
#' Pulls \code{nflreadr::load_depth_charts(season)} and identifies the
#' top-ranked QB per team. Depth chart schemas vary across nflreadr versions
#' and seasons, so all column references are resolved defensively at runtime.
#' If the source is unavailable, empty, or missing the columns needed to
#' identify a starter, returns NULL so the caller can fall back to rosters.
#'
#' Depth charts are often sparse in the offseason (e.g., May) and fully
#' populated in-season, which is exactly when this tool is used weekly.
#'
#' @param season Integer. Target season (default SEASON).
#' @return Tibble (team, starter_qb_id, starter_source) or NULL on failure.
#' @keywords internal
.detect_starters_from_depth_charts <- function(season = SEASON) {

  dc <- tryCatch(
    nflreadr::load_depth_charts(seasons = season),
    error = function(e) {
      message(glue("    load_depth_charts() failed: {e$message}"))
      NULL
    }
  )

  if (is.null(dc) || nrow(dc) == 0L) return(NULL)

  nm <- names(dc)
  team_col <- .pick_first_present(c("team", "club_code", "club"), nm)
  id_col   <- .pick_first_present(c("gsis_id", "gsis_it_id", "player_id"), nm)
  pos_col  <- .pick_first_present(c("position", "pos_abb", "pos_grp"), nm)
  rank_col <- .pick_first_present(
    c("depth_team", "pos_rank", "depth_position", "rank"), nm
  )

  # Cannot proceed without a team, an id, and a position to filter on
  if (is.na(team_col) || is.na(id_col) || is.na(pos_col)) {
    message("    Depth charts present but missing team/id/position columns -- ",
            "deferring to roster fallback")
    return(NULL)
  }

  df <- dc %>%
    dplyr::mutate(
      .team = .normalize_team_codes(as.character(.data[[team_col]])),
      .qbid = as.character(.data[[id_col]]),
      .pos  = toupper(as.character(.data[[pos_col]]))
    ) %>%
    dplyr::filter(.data$.pos == "QB",
                  .data$.team %in% ACTIVE_TEAMS_2026,
                  !is.na(.data$.qbid))

  if (nrow(df) == 0L) return(NULL)

  # If a week column exists, keep only the most recent week per team so the
  # latest known depth chart wins.
  if ("week" %in% nm) {
    df <- df %>%
      dplyr::group_by(.data$.team) %>%
      dplyr::filter(.data$week == max(.data$week, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }

  # Rank handling: lower rank value = higher on depth chart (1 = starter).
  # If no usable rank column, fall back to the first listed QB per team.
  if (!is.na(rank_col)) {
    df <- df %>%
      dplyr::mutate(.rank = suppressWarnings(as.numeric(.data[[rank_col]]))) %>%
      dplyr::mutate(.rank = dplyr::coalesce(.data$.rank, 99))
  } else {
    df <- df %>% dplyr::mutate(.rank = dplyr::row_number())
  }

  starters <- df %>%
    dplyr::group_by(.data$.team) %>%
    dplyr::slice_min(.data$.rank, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      team           = .data$.team,
      starter_qb_id  = .data$.qbid,
      starter_source = "depth_chart"
    )

  if (nrow(starters) == 0L) return(NULL)
  starters
}

# ------------------------------------------------------------------------------
# .detect_starters_from_rosters
# ------------------------------------------------------------------------------

#' Detect each team's likely starter from rosters + 2025 dropbacks (fallback)
#'
#' Uses the established \code{nflreadr::load_rosters(season)} pattern to find
#' the QBs assigned to each 2026 team, then selects the one with the most
#' prior-season dropbacks as the presumed starter. This naturally promotes an
#' incoming veteran (who carries NFL dropbacks) over a backup or rookie. A
#' rookie presumed starter with no prior-season dropbacks will not win here by
#' design -- that case is handled by the optional CSV override (direct-score
#' mode), and absent an override the team defaults to neutral QB quality.
#'
#' @param season Integer. Target season (default SEASON).
#' @param qb_stats_2025 Tibble or NULL. Prior-season QB stats keyed by qb_id
#'   with an n_dropbacks column. When NULL, all roster QBs score 0 dropbacks
#'   and the first listed QB per team is chosen.
#' @return Tibble (team, starter_qb_id, starter_source). Empty tibble if
#'   rosters are unavailable.
#' @keywords internal
.detect_starters_from_rosters <- function(season = SEASON,
                                          qb_stats_2025 = NULL) {

  ros <- tryCatch(
    nflreadr::load_rosters(seasons = season),
    error = function(e) {
      message(glue("    load_rosters() failed: {e$message}"))
      NULL
    }
  )

  empty <- tibble::tibble(team = character(), starter_qb_id = character(),
                          starter_source = character())

  if (is.null(ros) || nrow(ros) == 0L) return(empty)

  nm <- names(ros)
  team_col <- .pick_first_present(c("team", "club_code", "club"), nm)
  id_col   <- .pick_first_present(c("gsis_id", "player_id"), nm)
  pos_col  <- .pick_first_present(c("position", "depth_chart_position"), nm)

  if (is.na(team_col) || is.na(id_col) || is.na(pos_col)) {
    message("    Rosters missing team/id/position columns -- ",
            "QB starter resolution unavailable")
    return(empty)
  }

  qbs <- ros %>%
    dplyr::transmute(
      team     = .normalize_team_codes(as.character(.data[[team_col]])),
      qb_id    = as.character(.data[[id_col]]),
      position = toupper(as.character(.data[[pos_col]]))
    ) %>%
    dplyr::filter(.data$position == "QB",
                  .data$team %in% ACTIVE_TEAMS_2026,
                  !is.na(.data$qb_id))

  if (nrow(qbs) == 0L) return(empty)

  if (!is.null(qb_stats_2025) && "n_dropbacks" %in% names(qb_stats_2025)) {
    qbs <- qbs %>%
      dplyr::left_join(
        qb_stats_2025 %>% dplyr::select(.data$qb_id, .data$n_dropbacks),
        by = "qb_id"
      ) %>%
      dplyr::mutate(n_dropbacks = dplyr::coalesce(.data$n_dropbacks, 0L))
  } else {
    qbs <- qbs %>% dplyr::mutate(n_dropbacks = 0L)
  }

  .pick_starter_by_dropbacks(qbs) %>%
    dplyr::mutate(starter_source = "roster_fallback")
}

# ------------------------------------------------------------------------------
# .resolve_qb_starters
# ------------------------------------------------------------------------------

#' Resolve each 2026 team's starting QB (auto-detect + optional CSV override)
#'
#' Three-layer resolution mirroring \code{.load_coaching_changes()}:
#'   1. Primary  : depth charts (\code{.detect_starters_from_depth_charts})
#'   2. Fallback : rosters + prior-season dropbacks
#'                 (\code{.detect_starters_from_rosters}), used when depth
#'                 charts are unavailable or do not cover all active teams
#'   3. Override : optional CSV at \code{qb_changes_path}. CSV rows replace
#'                 the auto-detected starter for the same team. The CSV is the
#'                 right place to force a rookie starter or settle a camp
#'                 battle, and supports a direct quality-score override.
#'
#' CSV schema (data/ref/qb_changes_2026.csv), all columns optional except team:
#'   team                 chr  2026 team code (required)
#'   qb_id                chr  gsis_id of the 2026 starter (reference-QB mode)
#'   qb_quality_override  dbl  direct z-score quality (direct-score mode, e.g.
#'                              for a rookie #1 pick with no usable NFL signal)
#'   qb_name              chr  optional, for human readability only
#'   source               chr  optional, for documentation only
#'
#' Reference-QB mode (qb_id given): the named QB's prior-season CPOE/EPA are
#' pulled by the caller and z-scored normally. Direct-score mode
#' (qb_quality_override given): the supplied score is used verbatim, bypassing
#' the prior-season lookup. If both are given, the direct score wins.
#'
#' @param season Integer. Target season (default SEASON).
#' @param qb_stats_2025 Tibble or NULL. Prior-season QB stats keyed by qb_id
#'   (must include n_dropbacks) used by the roster fallback.
#' @param qb_changes_path Character. Path to the optional CSV override.
#' @return Tibble: team, starter_qb_id, starter_source, qb_quality_override.
#' @seealso .load_coaching_changes, .compute_qb_quality_index
#' @keywords internal
.resolve_qb_starters <- function(season = SEASON,
                                 qb_stats_2025 = NULL,
                                 qb_changes_path = QB_CHANGES_PATH) {

  # Layer 1: depth charts (primary)
  starters <- .detect_starters_from_depth_charts(season = season)

  # Layer 2: roster fallback (whole-league or gap-fill)
  if (is.null(starters) || nrow(starters) == 0L) {
    message("    QB starters: depth charts unavailable -- using roster fallback")
    starters <- .detect_starters_from_rosters(
      season = season, qb_stats_2025 = qb_stats_2025
    )
  } else {
    missing <- setdiff(ACTIVE_TEAMS_2026, starters$team)
    if (length(missing) > 0L) {
      gap <- .detect_starters_from_rosters(
        season = season, qb_stats_2025 = qb_stats_2025
      ) %>%
        dplyr::filter(.data$team %in% missing)
      if (nrow(gap) > 0L) starters <- dplyr::bind_rows(starters, gap)
    }
  }

  starters <- starters %>% dplyr::mutate(qb_quality_override = NA_real_)

  # Layer 3: optional CSV override
  if (file.exists(qb_changes_path)) {
    raw_csv <- tryCatch(
      readr::read_csv(qb_changes_path, show_col_types = FALSE),
      error = function(e) {
        message(glue("    QB changes CSV read failed: {e$message}"))
        NULL
      }
    )

    if (!is.null(raw_csv) && nrow(raw_csv) > 0L && "team" %in% names(raw_csv)) {
      starters <- .merge_qb_starter_override(starters, raw_csv)
    } else if (!is.null(raw_csv)) {
      message("    QB changes CSV present but missing 'team' column -- ignored")
    }
  }

  n_dc  <- sum(starters$starter_source == "depth_chart", na.rm = TRUE)
  n_ros <- sum(starters$starter_source == "roster_fallback", na.rm = TRUE)
  n_csv <- sum(starters$starter_source == "csv_override", na.rm = TRUE)
  message(glue("    QB starters resolved: {nrow(starters)} teams ",
               "({n_dc} depth chart, {n_ros} roster fallback, ",
               "{n_csv} CSV override)"))

  starters
}

# ------------------------------------------------------------------------------
# .compute_qb_quality_index
# ------------------------------------------------------------------------------

#' Compute QB quality z-score per team for the target season
#'
#' Player-centric design (v1.1). Prior-season (SEASON - 1) CPOE and EPA per
#' dropback are aggregated by \code{qb_id} across every team a QB played for,
#' then each target-season team is mapped to its resolved starter
#' (\code{.resolve_qb_starters}) and assigned that starter's prior-season
#' signal. This correctly handles offseason QB movement: a quarterback who
#' changed teams brings his real prior-season efficiency to his new team,
#' rather than the team being anchored to whoever took the most snaps there
#' last season.
#'
#' Each qualifying starter (>= MIN_DROPBACKS_QB prior-season dropbacks, no
#' direct override) is z-scored against the distribution of qualifying
#' starters:
#'
#'   qb_quality_score = mean(cpoe_z, epa_per_db_z)
#'
#' Resolution precedence for a team's score:
#'   1. Direct CSV override (qb_quality_override) wins verbatim.
#'   2. Otherwise, if the resolved starter qualifies, the computed z-score.
#'   3. Otherwise neutral (0) -- e.g., a rookie starter with no prior-season
#'      dropbacks and no override.
#'
#' @param cache_dir Character. R/15 cache directory.
#' @param prior_season Integer. Season to compute QB quality from
#'   (default SEASON - 1).
#' @param qb_changes_path Character. Optional CSV override path passed through
#'   to \code{.resolve_qb_starters}.
#' @param season Integer. Target season whose starters are resolved
#'   (default SEASON).
#' @return Tibble: team, qb_id, qb_dropbacks_share, qb_cpoe_mean,
#'   qb_epa_per_db, cpoe_z, epa_z, qb_quality_score. (qb_dropbacks_share is
#'   retained for schema compatibility and is NA under the player-centric
#'   design; it is not consumed downstream.)
#' @seealso .resolve_qb_starters, .apply_qb_quality_adjustment
#' @keywords internal
.compute_qb_quality_index <- function(cache_dir = CACHE_DIR_DEFAULT,
                                       prior_season = SEASON - 1L,
                                       qb_changes_path = QB_CHANGES_PATH,
                                       season = SEASON) {

  message(glue("  Computing QB quality from season {prior_season} pbp"))

  # Neutral table for every active team -- used on any data failure so the
  # pipeline degrades to QB-neutral rather than dropping teams.
  neutral_all <- function() {
    tibble::tibble(
      team               = ACTIVE_TEAMS_2026,
      qb_id              = NA_character_,
      qb_dropbacks_share = NA_real_,
      qb_cpoe_mean       = NA_real_,
      qb_epa_per_db      = NA_real_,
      cpoe_z             = 0,
      epa_z              = 0,
      qb_quality_score   = 0
    )
  }

  pbp <- tryCatch(
    load_normalized_season(prior_season, cache_dir = cache_dir),
    error = function(e) {
      message(glue("    QB quality: prior season load failed -- {e$message}"))
      NULL
    }
  )

  if (is.null(pbp)) return(neutral_all())

  # Filter to regular season dropbacks (excludes kneels, spikes, 2pt)
  pbp_qb <- pbp %>%
    dplyr::filter(
      .data$season_type == "REG",
      !is.na(.data$posteam),
      !is.na(.data$passer_player_id),
      dplyr::coalesce(.data$qb_dropback, 0L) == 1L,
      dplyr::coalesce(.data$qb_kneel, 0L) == 0L,
      dplyr::coalesce(.data$qb_spike, 0L) == 0L,
      dplyr::coalesce(.data$two_point_attempt, 0L) == 0L
    ) %>%
    dplyr::mutate(posteam = .normalize_team_codes(.data$posteam))

  rm(pbp)
  invisible(gc(verbose = FALSE))

  if (nrow(pbp_qb) == 0L) return(neutral_all())

  # PLAYER-LEVEL prior-season stats keyed by qb_id, aggregated across any
  # teams the QB played for. This is the core of the player-centric fix.
  qb_stats_2025 <- pbp_qb %>%
    dplyr::group_by(qb_id = .data$passer_player_id) %>%
    dplyr::summarise(
      n_dropbacks   = dplyr::n(),
      qb_cpoe_mean  = mean(.data$cpoe, na.rm = TRUE),
      qb_epa_per_db = mean(.data$epa, na.rm = TRUE),
      .groups       = "drop"
    )

  # Resolve each target-season team's starter (depth chart -> roster -> CSV)
  starters <- .resolve_qb_starters(
    season          = season,
    qb_stats_2025   = qb_stats_2025,
    qb_changes_path = qb_changes_path
  )

  # If starter resolution returned nothing usable, degrade to neutral
  if (is.null(starters) || nrow(starters) == 0L) {
    message("  QB quality: no starters resolved -- all teams neutral")
    return(neutral_all())
  }

  # Attach each starter's prior-season stats by qb_id
  starter_stats <- starters %>%
    dplyr::left_join(qb_stats_2025, by = c("starter_qb_id" = "qb_id"))

  # Z-score and resolve final quality (pure core: override > computed > neutral)
  qb_quality <- .score_qb_starters(starter_stats, min_dropbacks = MIN_DROPBACKS_QB)

  # Ensure all active teams are represented; any team with no resolved
  # starter gets neutral quality (z = 0)
  missing_teams <- setdiff(ACTIVE_TEAMS_2026, qb_quality$team)
  if (length(missing_teams) > 0L) {
    qb_quality <- dplyr::bind_rows(
      qb_quality,
      tibble::tibble(
        team               = missing_teams,
        qb_id              = NA_character_,
        qb_dropbacks_share = NA_real_,
        qb_cpoe_mean       = NA_real_,
        qb_epa_per_db      = NA_real_,
        cpoe_z             = 0,
        epa_z              = 0,
        qb_quality_score   = 0
      )
    )
  }

  n_pool <- sum(
    is.na(starter_stats$qb_quality_override) &
      !is.na(starter_stats$n_dropbacks) &
      starter_stats$n_dropbacks >= MIN_DROPBACKS_QB,
    na.rm = TRUE
  )
  n_override <- sum(!is.na(starter_stats$qb_quality_override), na.rm = TRUE)
  # Resolved starters that fall below the dropback threshold (or have no prior
  # stats) and carry no override -> scored neutral via the case_when, distinct
  # from teams with no resolved starter at all.
  n_below <- sum(
    is.na(starter_stats$qb_quality_override) &
      (is.na(starter_stats$n_dropbacks) |
         starter_stats$n_dropbacks < MIN_DROPBACKS_QB),
    na.rm = TRUE
  )
  message(glue("  QB quality: {n_pool} z-scored starters, ",
               "{n_override} direct override(s), ",
               "{n_below} below-threshold neutral, ",
               "{length(missing_teams)} unresolved (also neutral)"))

  attr(qb_quality, "qb_stats_2025") <- qb_stats_2025
  qb_quality
}

# ------------------------------------------------------------------------------
# .apply_qb_quality_adjustment
# ------------------------------------------------------------------------------

#' Apply QB quality adjustment to blended volume and efficiency
#'
#' Two effects per the design:
#'   1. Volume: better QB -> team passes more, runs less.
#'      pass_pg_adj = blended_pass_pg * (1 + qb_score * QB_VOLUME_SENSITIVITY)
#'      rush_pg_adj = blended_rush_pg * (1 - qb_score * QB_VOLUME_SENSITIVITY)
#'
#'   2. Efficiency: better QB -> more yards per attempt, more TDs.
#'      pass_yds_pg_adj = historical_pass_yds_pg * (1 + qb_score * QB_EFFICIENCY_SENSITIVITY)
#'      pass_tds_pg_adj = historical_pass_tds_pg * (1 + qb_score * QB_EFFICIENCY_SENSITIVITY)
#'
#' Rush metrics get no QB adjustment (assumes RB efficiency is independent
#' of QB quality, which is a defensible simplification).
#'
#' @param blended Tibble. Output of .blend_team_and_coach_pattern().
#' @param qb_quality Tibble. Output of .compute_qb_quality_index().
#' @return Tibble with projected_* columns added.
#' @keywords internal
.apply_qb_quality_adjustment <- function(blended, qb_quality) {

  blended %>%
    dplyr::left_join(
      qb_quality %>% dplyr::select(.data$team, .data$qb_quality_score),
      by = "team"
    ) %>%
    dplyr::mutate(
      qb_quality_score = dplyr::coalesce(.data$qb_quality_score, 0),
      qb_quality_adj_pass = 1 + .data$qb_quality_score * QB_VOLUME_SENSITIVITY,
      qb_quality_adj_eff  = 1 + .data$qb_quality_score * QB_EFFICIENCY_SENSITIVITY,
      projected_pass_pg = .data$blended_pass_pg * .data$qb_quality_adj_pass,
      projected_rush_pg = .data$blended_rush_pg *
                            (2 - .data$qb_quality_adj_pass),
      projected_plays_pg = .data$blended_plays_pg,
      projected_pass_yds_pg = .data$historical_pass_yds_pg *
                                .data$qb_quality_adj_eff,
      projected_rush_yds_pg = .data$historical_rush_yds_pg,
      projected_pass_tds_pg = .data$historical_pass_tds_pg *
                                .data$qb_quality_adj_eff,
      projected_rush_tds_pg = .data$historical_rush_tds_pg
    )
}

# ------------------------------------------------------------------------------
# .compute_team_defense_allowed
# ------------------------------------------------------------------------------

#' Compute per-team-season defensive EPA allowed, split by play type
#'
#' For each season in \code{seasons}, loads the R/15 normalized pbp, filters to
#' regular-season pass/rush plays (excluding kneels, spikes, two-point
#' attempts), and aggregates EPA allowed at the \code{defteam}-season level,
#' separately for pass and rush. This is the raw material for every SOS
#' computation: a defense's quality is how much EPA it allows relative to the
#' league.
#'
#' Memory: each season is loaded, aggregated, and dropped before the next.
#'
#' @param seasons Integer vector. Seasons to aggregate.
#' @param cache_dir Character. R/15 cache directory.
#' @return Tibble: team, season, def_pass_epa_allowed, def_rush_epa_allowed,
#'   def_pass_plays, def_rush_plays. One row per defensive team-season.
#'   Empty tibble if no seasons load.
#' @keywords internal
.compute_team_defense_allowed <- function(seasons, cache_dir = CACHE_DIR_DEFAULT) {

  empty <- tibble::tibble(
    team = character(), season = integer(),
    def_pass_epa_allowed = numeric(), def_rush_epa_allowed = numeric(),
    def_pass_plays = integer(), def_rush_plays = integer()
  )

  agg <- purrr::map_dfr(seasons, function(s) {

    pbp <- tryCatch(
      load_normalized_season(s, cache_dir = cache_dir),
      error = function(e) {
        message(glue("    SOS: season {s} load failed -- {e$message}"))
        NULL
      }
    )
    if (is.null(pbp)) return(NULL)

    pbp_use <- pbp %>%
      dplyr::filter(
        .data$season_type == "REG",
        !is.na(.data$defteam),
        nchar(.data$defteam) > 0,
        !is.na(.data$epa),
        .data$play_type %in% c("pass", "run")
      ) %>%
      dplyr::mutate(
        defteam  = .normalize_team_codes(.data$defteam),
        qb_kneel = dplyr::coalesce(.data$qb_kneel, 0L),
        qb_spike = dplyr::coalesce(.data$qb_spike, 0L),
        two_point_attempt = dplyr::coalesce(.data$two_point_attempt, 0L)
      ) %>%
      dplyr::filter(
        .data$qb_kneel == 0L,
        .data$qb_spike == 0L,
        .data$two_point_attempt == 0L
      )

    rm(pbp)
    invisible(gc(verbose = FALSE))

    if (nrow(pbp_use) == 0L) return(NULL)

    pbp_use %>%
      dplyr::group_by(team = .data$defteam) %>%
      dplyr::summarise(
        season               = s,
        def_pass_epa_allowed = mean(.data$epa[.data$play_type == "pass"],
                                    na.rm = TRUE),
        def_rush_epa_allowed = mean(.data$epa[.data$play_type == "run"],
                                    na.rm = TRUE),
        def_pass_plays       = sum(.data$play_type == "pass", na.rm = TRUE),
        def_rush_plays       = sum(.data$play_type == "run",  na.rm = TRUE),
        .groups              = "drop"
      )
  })

  if (is.null(agg) || nrow(agg) == 0L) return(empty)
  agg
}

# ------------------------------------------------------------------------------
# .compute_defense_quality_by_team
# ------------------------------------------------------------------------------

#' Collapse team-season defense to one recency-weighted quality per team
#'
#' Takes the team-season defensive EPA-allowed table and produces, for each
#' team, a single recency-weighted average pass and rush EPA allowed (weighting
#' recent seasons more via SOS_SEASON_RECENCY_WEIGHTS), then expresses each
#' relative to the league average across teams. Positive values mean the
#' defense allows MORE EPA than average (a weaker defense = an easier matchup).
#'
#' Team-seasons below SOS_MIN_DEF_PLAYS (for the relevant play type) are
#' dropped from that play type's average.
#'
#' @param def_team_season Tibble. Output of .compute_team_defense_allowed().
#' @return Tibble: team, pass_epa_vs_league, rush_epa_vs_league. Empty tibble
#'   if input is empty.
#' @keywords internal
.compute_defense_quality_by_team <- function(def_team_season) {

  empty <- tibble::tibble(
    team = character(),
    pass_epa_vs_league = numeric(),
    rush_epa_vs_league = numeric()
  )
  if (is.null(def_team_season) || nrow(def_team_season) == 0L) return(empty)

  # Attach a recency weight by season age (most recent season = age 1).
  max_season <- max(def_team_season$season, na.rm = TRUE)
  weighted <- def_team_season %>%
    dplyr::mutate(
      season_age = max_season - .data$season + 1L,
      recency_weight = dplyr::if_else(
        .data$season_age >= 1L & .data$season_age <= HISTORICAL_WINDOW,
        SOS_SEASON_RECENCY_WEIGHTS[pmin(.data$season_age, HISTORICAL_WINDOW)],
        0
      ),
      # Per-play-type effective weight: zeroed when the team-season is below
      # the minimum-plays guard for that play type. A weighted.mean with these
      # weights then naturally excludes the under-sampled team-seasons.
      pass_w = dplyr::if_else(
        .data$def_pass_plays >= SOS_MIN_DEF_PLAYS, .data$recency_weight, 0
      ),
      rush_w = dplyr::if_else(
        .data$def_rush_plays >= SOS_MIN_DEF_PLAYS, .data$recency_weight, 0
      )
    ) %>%
    dplyr::filter(.data$recency_weight > 0)

  # Recency-weighted mean EPA allowed per team, per play type. Teams whose
  # qualifying weight sums to zero (all team-seasons under the plays guard)
  # yield NaN from weighted.mean; converted to NA below.
  per_team <- weighted %>%
    dplyr::group_by(.data$team) %>%
    dplyr::summarise(
      pass_weight_sum = sum(.data$pass_w, na.rm = TRUE),
      rush_weight_sum = sum(.data$rush_w, na.rm = TRUE),
      def_pass_epa_allowed = stats::weighted.mean(
        .data$def_pass_epa_allowed, .data$pass_w, na.rm = TRUE
      ),
      def_rush_epa_allowed = stats::weighted.mean(
        .data$def_rush_epa_allowed, .data$rush_w, na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      def_pass_epa_allowed = dplyr::if_else(
        .data$pass_weight_sum > 0, .data$def_pass_epa_allowed, NA_real_
      ),
      def_rush_epa_allowed = dplyr::if_else(
        .data$rush_weight_sum > 0, .data$def_rush_epa_allowed, NA_real_
      )
    )

  # League averages across teams, then express each team relative to league.
  league_pass <- mean(per_team$def_pass_epa_allowed, na.rm = TRUE)
  league_rush <- mean(per_team$def_rush_epa_allowed, na.rm = TRUE)

  per_team %>%
    dplyr::mutate(
      pass_epa_vs_league = .data$def_pass_epa_allowed - league_pass,
      rush_epa_vs_league = .data$def_rush_epa_allowed - league_rush
    ) %>%
    dplyr::select(.data$team, .data$pass_epa_vs_league,
                  .data$rush_epa_vs_league)
}

# ------------------------------------------------------------------------------
# .compute_historical_sos
# ------------------------------------------------------------------------------

#' Retrospective SOS: quality of opponents each team actually faced
#'
#' For each prior season, reconstructs each team's opponent list from the pbp
#' (distinct posteam-defteam-game), then averages the opponents' defensive
#' quality (recency-weighted defense table), recency-weighting the seasons
#' themselves. The result is a per-team relative schedule-quality signal that
#' corrects the historical volume base for past schedule luck.
#'
#' Positive pass value = the team historically faced weaker-than-average pass
#' defenses (its historical pass volume was inflated by an easy slate).
#'
#' @param seasons Integer vector. Prior seasons (HISTORICAL_SEASONS).
#' @param cache_dir Character. R/15 cache directory.
#' @param def_quality Tibble. Output of .compute_defense_quality_by_team()
#'   over the same seasons -- the opponent-quality lookup.
#' @return Tibble: team, hist_pass_sos, hist_rush_sos. Empty tibble if no
#'   schedule could be reconstructed.
#' @keywords internal
.compute_historical_sos <- function(seasons, cache_dir = CACHE_DIR_DEFAULT,
                                    def_quality) {

  empty <- tibble::tibble(
    team = character(), hist_pass_sos = numeric(), hist_rush_sos = numeric()
  )
  if (is.null(def_quality) || nrow(def_quality) == 0L) return(empty)

  # Reconstruct opponent pairings per season (team faced opponent).
  pairings <- purrr::map_dfr(seasons, function(s) {
    pbp <- tryCatch(
      load_normalized_season(s, cache_dir = cache_dir),
      error = function(e) NULL
    )
    if (is.null(pbp)) return(NULL)

    pr <- pbp %>%
      dplyr::filter(
        .data$season_type == "REG",
        !is.na(.data$posteam), nchar(.data$posteam) > 0,
        !is.na(.data$defteam), nchar(.data$defteam) > 0
      ) %>%
      dplyr::mutate(
        team     = .normalize_team_codes(.data$posteam),
        opponent = .normalize_team_codes(.data$defteam)
      ) %>%
      dplyr::distinct(.data$team, .data$opponent, .data$game_id) %>%
      dplyr::mutate(season = s) %>%
      dplyr::select(.data$team, .data$opponent, .data$season)

    rm(pbp)
    invisible(gc(verbose = FALSE))
    pr
  })

  if (is.null(pairings) || nrow(pairings) == 0L) return(empty)

  # Attach each opponent's defensive quality and a season recency weight, then
  # average per team (each game counts once; recent seasons weighted higher).
  max_season <- max(pairings$season, na.rm = TRUE)
  pairings %>%
    dplyr::left_join(
      def_quality %>%
        dplyr::rename(opp_pass_quality = .data$pass_epa_vs_league,
                      opp_rush_quality = .data$rush_epa_vs_league),
      by = c("opponent" = "team")
    ) %>%
    dplyr::mutate(
      season_age = max_season - .data$season + 1L,
      recency_weight = dplyr::if_else(
        .data$season_age >= 1L & .data$season_age <= HISTORICAL_WINDOW,
        SOS_SEASON_RECENCY_WEIGHTS[pmin(.data$season_age, HISTORICAL_WINDOW)],
        0
      ),
      # Zero the weight where the opponent has no quality score so weighted.mean
      # naturally drops it; track per-play-type so each can be NA independently.
      pass_w = dplyr::if_else(
        !is.na(.data$opp_pass_quality) & .data$recency_weight > 0,
        .data$recency_weight, 0
      ),
      rush_w = dplyr::if_else(
        !is.na(.data$opp_rush_quality) & .data$recency_weight > 0,
        .data$recency_weight, 0
      ),
      # weighted.mean errors on NA values even at zero weight, so neutralize
      # the NA quality entries to 0 (their weight is already 0).
      opp_pass_quality = dplyr::coalesce(.data$opp_pass_quality, 0),
      opp_rush_quality = dplyr::coalesce(.data$opp_rush_quality, 0)
    ) %>%
    dplyr::filter(.data$team %in% ACTIVE_TEAMS_2026) %>%
    dplyr::group_by(.data$team) %>%
    dplyr::summarise(
      pass_weight_sum = sum(.data$pass_w, na.rm = TRUE),
      rush_weight_sum = sum(.data$rush_w, na.rm = TRUE),
      hist_pass_sos = stats::weighted.mean(.data$opp_pass_quality,
                                           .data$pass_w, na.rm = TRUE),
      hist_rush_sos = stats::weighted.mean(.data$opp_rush_quality,
                                           .data$rush_w, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      hist_pass_sos = dplyr::if_else(.data$pass_weight_sum > 0,
                                     .data$hist_pass_sos, NA_real_),
      hist_rush_sos = dplyr::if_else(.data$rush_weight_sum > 0,
                                     .data$hist_rush_sos, NA_real_)
    ) %>%
    dplyr::select(.data$team, .data$hist_pass_sos, .data$hist_rush_sos)
}

# ------------------------------------------------------------------------------
# .compute_forward_sos
# ------------------------------------------------------------------------------

#' Forward SOS: quality of each team's scheduled opponents in the target season
#'
#' Pulls the target-season schedule from nflreadr::load_schedules() and, for
#' each team, averages the defensive quality of its scheduled opponents (each
#' opponent scored by its own recency-weighted historical defense). Answers
#' "is the 2026 slate hard or easy?"
#'
#' If the target-season schedule is not yet ingested by nflverse (typical in
#' early offseason), returns an empty tibble and the caller falls back to
#' historical-only SOS.
#'
#' @param season Integer. Target season.
#' @param def_quality Tibble. Output of .compute_defense_quality_by_team().
#' @return Tibble: team, fwd_pass_sos, fwd_rush_sos. Empty tibble if the
#'   schedule is unavailable.
#' @keywords internal
.compute_forward_sos <- function(season = SEASON, def_quality) {

  empty <- tibble::tibble(
    team = character(), fwd_pass_sos = numeric(), fwd_rush_sos = numeric()
  )
  if (is.null(def_quality) || nrow(def_quality) == 0L) return(empty)

  sched <- tryCatch(
    nflreadr::load_schedules(seasons = season),
    error = function(e) {
      message(glue("    Forward SOS: schedule load failed -- {e$message}"))
      NULL
    }
  )
  if (is.null(sched) || nrow(sched) == 0L) {
    message(glue("    Forward SOS: season {season} schedule unavailable -- ",
                 "historical-only SOS will apply"))
    return(empty)
  }

  req <- c("season", "week", "home_team", "away_team")
  if (length(setdiff(req, names(sched))) > 0L) {
    message("    Forward SOS: schedule missing home/away columns -- skipped")
    return(empty)
  }

  # Long form: one row per team per game, with the opponent named.
  team_opp <- dplyr::bind_rows(
    sched %>% dplyr::transmute(
      team = .normalize_team_codes(.data$home_team),
      opponent = .normalize_team_codes(.data$away_team)
    ),
    sched %>% dplyr::transmute(
      team = .normalize_team_codes(.data$away_team),
      opponent = .normalize_team_codes(.data$home_team)
    )
  ) %>%
    dplyr::filter(!is.na(.data$team), !is.na(.data$opponent),
                  .data$team %in% ACTIVE_TEAMS_2026)

  if (nrow(team_opp) == 0L) return(empty)

  team_opp %>%
    dplyr::left_join(
      def_quality %>%
        dplyr::rename(opp_pass_quality = .data$pass_epa_vs_league,
                      opp_rush_quality = .data$rush_epa_vs_league),
      by = c("opponent" = "team")
    ) %>%
    dplyr::group_by(.data$team) %>%
    dplyr::summarise(
      fwd_pass_sos = mean(.data$opp_pass_quality, na.rm = TRUE),
      fwd_rush_sos = mean(.data$opp_rush_quality, na.rm = TRUE),
      .groups = "drop"
    )
}

# ------------------------------------------------------------------------------
# .calibrate_sos_sensitivity
# ------------------------------------------------------------------------------

#' Fit SOS sensitivities empirically from team-game data
#'
#' Estimates how much team offensive efficiency and volume actually move with
#' opponent defensive quality, so the SOS multipliers are earned from data
#' rather than guessed.
#'
#' Design (locked in session):
#'   - Unit of observation: team-game (one row per team per game per play
#'     type). The opponent effect is a per-game phenomenon; season-level
#'     averaging washes it out.
#'   - Identification: team-season fixed effects via the within transformation
#'     (demean outcome and regressor inside each team-season, then fit the
#'     pooled slope). The slope is identified from how the SAME offense
#'     performs against varying defenses, removing offense-quality confounding.
#'   - Regressor: leave-one-out opponent defensive EPA per play (the opponent's
#'     season EPA allowed EXCLUDING the game being predicted, so the outcome
#'     never appears on both sides), expressed vs league average.
#'   - Outcomes (both relative, so slopes are directly the unitless
#'     sensitivities the factor mechanism needs):
#'       efficiency: (team-game yards per play / league yards per play) - 1
#'       volume:     (team-game plays / league plays per team-game) - 1
#'
#' Sign handling: a negative EFFICIENCY slope (easier defense, worse output)
#' is causally implausible and treated as noise -- clamped to 0 (neutral)
#' with a message. A negative VOLUME slope is a real phenomenon (teams ahead
#' against weak defenses run more / pass less) and is retained as fitted.
#'
#' @param seasons Integer vector. Seasons to calibrate on (HISTORICAL_SEASONS).
#' @param cache_dir Character. R/15 cache directory.
#' @return Named list: pass_eff, rush_eff, pass_vol, rush_vol (numeric
#'   sensitivities), plus calibrated (lgl) and n_games (int). Falls back to
#'   the *_FALLBACK constants with calibrated = FALSE if data is insufficient.
#' @keywords internal
.calibrate_sos_sensitivity <- function(seasons, cache_dir = CACHE_DIR_DEFAULT) {

  fallback <- list(
    pass_eff = SOS_EFFICIENCY_SENSITIVITY_FALLBACK,
    rush_eff = SOS_EFFICIENCY_SENSITIVITY_FALLBACK,
    pass_vol = SOS_VOLUME_SENSITIVITY_FALLBACK,
    rush_vol = SOS_VOLUME_SENSITIVITY_FALLBACK,
    calibrated = FALSE,
    n_games = 0L
  )

  # Team-game aggregates per play type, one pass over each season.
  team_games <- purrr::map_dfr(seasons, function(s) {
    pbp <- tryCatch(
      load_normalized_season(s, cache_dir = cache_dir),
      error = function(e) NULL
    )
    if (is.null(pbp)) return(NULL)

    tg <- pbp %>%
      dplyr::filter(
        .data$season_type == "REG",
        .data$play_type %in% c("pass", "run"),
        !is.na(.data$epa),
        !is.na(.data$posteam), nchar(.data$posteam) > 0,
        !is.na(.data$defteam), nchar(.data$defteam) > 0
      ) %>%
      dplyr::mutate(
        qb_kneel = dplyr::coalesce(.data$qb_kneel, 0L),
        qb_spike = dplyr::coalesce(.data$qb_spike, 0L),
        two_point_attempt = dplyr::coalesce(.data$two_point_attempt, 0L)
      ) %>%
      dplyr::filter(
        .data$qb_kneel == 0L, .data$qb_spike == 0L,
        .data$two_point_attempt == 0L
      ) %>%
      dplyr::mutate(
        team     = .normalize_team_codes(.data$posteam),
        opponent = .normalize_team_codes(.data$defteam)
      ) %>%
      dplyr::group_by(.data$game_id, .data$team, .data$opponent,
                      .data$play_type) %>%
      dplyr::summarise(
        n_plays = dplyr::n(),
        yds     = sum(.data$yards_gained, na.rm = TRUE),
        epa_sum = sum(.data$epa, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(season = s)

    rm(pbp)
    invisible(gc(verbose = FALSE))
    tg
  })

  if (is.null(team_games) || nrow(team_games) == 0L) {
    message("    SOS calibration: no team-game data -- using FALLBACK ",
            "sensitivities (unvalidated priors)")
    return(fallback)
  }

  # Fit one slope per play_type x outcome. All league reference quantities are
  # season-specific so era drift does not contaminate the relative outcomes.
  fit_slope <- function(pt, outcome) {

    d <- team_games %>% dplyr::filter(.data$play_type == pt)

    # Opponent season defensive totals (their EPA allowed = the epa_sum the
    # offenses they faced produced), for the leave-one-out regressor.
    opp_totals <- d %>%
      dplyr::group_by(opponent_team = .data$opponent, .data$season) %>%
      dplyr::summarise(
        opp_total_epa   = sum(.data$epa_sum),
        opp_total_plays = sum(.data$n_plays),
        .groups = "drop"
      )

    d <- d %>%
      dplyr::left_join(
        opp_totals,
        by = c("opponent" = "opponent_team", "season" = "season")
      ) %>%
      dplyr::group_by(.data$season) %>%
      dplyr::mutate(
        league_mean_epa     = sum(.data$epa_sum) / sum(.data$n_plays),
        league_ypp          = sum(.data$yds) / sum(.data$n_plays),
        league_plays_per_tg = sum(.data$n_plays) / dplyr::n()
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(
        .data$n_plays >= SOS_CALIB_MIN_PLAYS_PER_GAME,
        (.data$opp_total_plays - .data$n_plays) > 0
      ) %>%
      dplyr::mutate(
        # Leave-one-out opponent defensive EPA vs league: this game's own
        # plays are removed from the opponent's season totals.
        x = (.data$opp_total_epa - .data$epa_sum) /
            (.data$opp_total_plays - .data$n_plays) - .data$league_mean_epa,
        y_eff = (.data$yds / .data$n_plays) / .data$league_ypp - 1,
        y_vol = .data$n_plays / .data$league_plays_per_tg - 1,
        team_season = paste(.data$team, .data$season, sep = "_")
      )

    # outcome is a scalar -- base if/else per house convention.
    d$y <- if (outcome == "eff") d$y_eff else d$y_vol

    if (nrow(d) < SOS_CALIB_MIN_GAMES) return(NA_real_)

    # Within transformation: demean x and y inside each team-season, then fit
    # the pooled slope. Equivalent to OLS with team-season fixed effects.
    d <- d %>%
      dplyr::group_by(.data$team_season) %>%
      dplyr::mutate(
        xd = .data$x - mean(.data$x),
        yd = .data$y - mean(.data$y)
      ) %>%
      dplyr::ungroup()

    sxx <- sum(d$xd^2)
    if (sxx <= 0) return(NA_real_)

    sum(d$xd * d$yd) / sxx
  }

  slopes <- list(
    pass_eff = fit_slope("pass", "eff"),
    rush_eff = fit_slope("run",  "eff"),
    pass_vol = fit_slope("pass", "vol"),
    rush_vol = fit_slope("run",  "vol")
  )

  if (any(vapply(slopes, is.na, logical(1)))) {
    message("    SOS calibration: insufficient sample for at least one slope ",
            "-- using FALLBACK sensitivities (unvalidated priors)")
    return(fallback)
  }

  # Negative efficiency slopes are causally implausible (easier defense should
  # not reduce output) -- treat as noise and neutralize. Negative volume slopes
  # are real game-script effects and are retained.
  for (nm in c("pass_eff", "rush_eff")) {
    if (slopes[[nm]] < 0) {
      message(glue("    SOS calibration: fitted {nm} slope negative ",
                   "({format(round(slopes[[nm]], 3), nsmall = 3)}) -- ",
                   "clamped to 0 (neutral)"))
      slopes[[nm]] <- 0
    }
  }

  n_games <- team_games %>%
    dplyr::distinct(.data$game_id, .data$team) %>%
    nrow()

  c(slopes, list(calibrated = TRUE, n_games = as.integer(n_games)))
}



#' Blend historical + forward (+ optional in-season) SOS and apply to projections
#'
#' Combines the retrospective and forward SOS signals into a preseason SOS
#' signal per team and play type. When \code{as_of_week} is supplied and a
#' \code{current_season_def} opponent-quality signal is available, the
#' preseason signal is tapered toward the team's observed current-season
#' opponent quality using compute_prior_weight(as_of_week) -- the same in-season
#' decay curve used across the engine (week 1 = full preseason weight, ~week 9
#' crossover, 0.05 floor on the preseason signal).
#'
#' The blended relative-EPA signal is converted to a multiplier:
#'   factor = 1 + signal * sensitivity
#' then bounded to [SOS_FACTOR_FLOOR, SOS_FACTOR_CEILING]. A positive signal
#' means easier defenses faced, so volume and efficiency tick up.
#'
#' Volume: pass scales up with the pass factor; rush scales reciprocally so a
#' tougher pass schedule does not also inflate rush attempts. Efficiency:
#' pass yards and TDs scale with the pass factor; rush yards and TDs with the
#' rush factor.
#'
#' @param projected Tibble. Output of .apply_qb_quality_adjustment().
#' @param hist_sos Tibble. Output of .compute_historical_sos().
#' @param fwd_sos Tibble. Output of .compute_forward_sos().
#' @param as_of_week Integer or NULL. Current week for in-season taper.
#' @param current_season_sos Tibble or NULL. Observed current-season schedule
#'   quality per team (team, cur_pass_sos, cur_rush_sos). Only used when
#'   as_of_week is non-NULL.
#' @param sens Named list from .calibrate_sos_sensitivity(): pass_eff,
#'   rush_eff, pass_vol, rush_vol. Fitted slopes converting the SOS signal
#'   into multiplicative factors.
#' @return Tibble with pass_sos_factor, rush_sos_factor, and SOS-adjusted
#'   projected_* columns.
#' @keywords internal
.apply_sos_adjustment <- function(projected, hist_sos, fwd_sos,
                                  as_of_week = NULL,
                                  current_season_sos = NULL,
                                  sens) {

  # Assemble per-team preseason SOS signal. Historical and forward are averaged
  # when both present; whichever is present is used alone otherwise; absent
  # both, the signal is 0 (neutral).
  sos <- projected %>%
    dplyr::select(.data$team) %>%
    dplyr::left_join(hist_sos, by = "team") %>%
    dplyr::left_join(fwd_sos, by = "team") %>%
    dplyr::mutate(
      pass_sos_signal = dplyr::case_when(
        !is.na(.data$hist_pass_sos) & !is.na(.data$fwd_pass_sos) ~
          (.data$hist_pass_sos + .data$fwd_pass_sos) / 2,
        !is.na(.data$fwd_pass_sos)  ~ .data$fwd_pass_sos,
        !is.na(.data$hist_pass_sos) ~ .data$hist_pass_sos,
        TRUE ~ 0
      ),
      rush_sos_signal = dplyr::case_when(
        !is.na(.data$hist_rush_sos) & !is.na(.data$fwd_rush_sos) ~
          (.data$hist_rush_sos + .data$fwd_rush_sos) / 2,
        !is.na(.data$fwd_rush_sos)  ~ .data$fwd_rush_sos,
        !is.na(.data$hist_rush_sos) ~ .data$hist_rush_sos,
        TRUE ~ 0
      )
    )

  # In-season taper toward observed current-season opponent quality.
  if (!is.null(as_of_week) && !is.null(current_season_sos) &&
      nrow(current_season_sos) > 0L) {
    pw <- compute_prior_weight(as_of_week)  # preseason-signal weight
    message(glue("    SOS in-season blend at week {as_of_week}: ",
                 "preseason weight {format(round(pw, 3), nsmall = 3)}, ",
                 "observed weight {format(round(1 - pw, 3), nsmall = 3)}"))
    sos <- sos %>%
      dplyr::left_join(current_season_sos, by = "team") %>%
      dplyr::mutate(
        pass_sos_signal = dplyr::if_else(
          is.na(.data$cur_pass_sos),
          .data$pass_sos_signal,
          pw * .data$pass_sos_signal + (1 - pw) * .data$cur_pass_sos
        ),
        rush_sos_signal = dplyr::if_else(
          is.na(.data$cur_rush_sos),
          .data$rush_sos_signal,
          pw * .data$rush_sos_signal + (1 - pw) * .data$cur_rush_sos
        )
      )
  }

  # Build bounded multipliers and apply, using the fitted sensitivities.
  sos <- sos %>%
    dplyr::mutate(
      pass_sos_factor = pmin(pmax(
        1 + .data$pass_sos_signal * sens$pass_eff,
        SOS_FACTOR_FLOOR), SOS_FACTOR_CEILING),
      rush_sos_factor = pmin(pmax(
        1 + .data$rush_sos_signal * sens$rush_eff,
        SOS_FACTOR_FLOOR), SOS_FACTOR_CEILING),
      # Volume multipliers use the separately fitted volume slopes (volume
      # responds to schedule differently than efficiency, including possible
      # game-script sign reversal).
      pass_vol_factor = pmin(pmax(
        1 + .data$pass_sos_signal * sens$pass_vol,
        SOS_FACTOR_FLOOR), SOS_FACTOR_CEILING),
      rush_vol_factor = pmin(pmax(
        1 + .data$rush_sos_signal * sens$rush_vol,
        SOS_FACTOR_FLOOR), SOS_FACTOR_CEILING)
    ) %>%
    dplyr::select(.data$team, .data$pass_sos_factor, .data$rush_sos_factor,
                  pass_vol_factor, rush_vol_factor)

  projected %>%
    dplyr::left_join(sos, by = "team") %>%
    dplyr::mutate(
      # Neutral fallback for any unmatched team.
      pass_sos_factor = dplyr::coalesce(.data$pass_sos_factor, 1),
      rush_sos_factor = dplyr::coalesce(.data$rush_sos_factor, 1),
      pass_vol_factor = dplyr::coalesce(.data$pass_vol_factor, 1),
      rush_vol_factor = dplyr::coalesce(.data$rush_vol_factor, 1),
      # Volume (separate pass/rush volume factors).
      projected_pass_pg     = .data$projected_pass_pg * .data$pass_vol_factor,
      projected_rush_pg     = .data$projected_rush_pg * .data$rush_vol_factor,
      # Efficiency (yards and TDs by play type).
      projected_pass_yds_pg = .data$projected_pass_yds_pg * .data$pass_sos_factor,
      projected_rush_yds_pg = .data$projected_rush_yds_pg * .data$rush_sos_factor,
      projected_pass_tds_pg = .data$projected_pass_tds_pg * .data$pass_sos_factor,
      projected_rush_tds_pg = .data$projected_rush_tds_pg * .data$rush_sos_factor
    ) %>%
    dplyr::select(-pass_vol_factor, -rush_vol_factor)
}

# ==============================================================================
# PUBLIC ENTRY POINT
# ==============================================================================

# ------------------------------------------------------------------------------
# QB committee support (uncertain QB rooms)
# ------------------------------------------------------------------------------

#' Load the season-guarded QB committee config
#'
#' Flagged teams (uncertain QB rooms) get every listed QB projected as a
#' full-time starter, so the board shows each candidate's ceiling. The CSV is
#' hard-scoped to the run season: a 2026 row can only ever apply to a 2026 run.
#'
#' @param path Character. Path to the committee CSV.
#' @param season Integer. Run season; only rows matching it are applied.
#' @return Tibble (team, qb_id, committee_group) for this season, else empty.
#' @keywords internal
.load_qb_committees <- function(path = QB_COMMITTEES_PATH, season = SEASON) {

  empty <- tibble::tibble(team = character(), qb_id = character(),
                          committee_group = character())

  if (!file.exists(path)) return(empty)

  raw <- tryCatch(readr::read_csv(path, show_col_types = FALSE),
                  error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0L) return(empty)

  req <- c("season", "team", "qb_id", "committee_group")
  if (length(setdiff(req, names(raw))) > 0L) {
    warning(glue("QB committees CSV missing columns: ",
                 "{paste(setdiff(req, names(raw)), collapse = ', ')}. Ignored."),
            call. = FALSE)
    return(empty)
  }

  out <- raw %>%
    dplyr::mutate(.season = suppressWarnings(as.integer(.data$season))) %>%
    dplyr::filter(.data$.season == as.integer(season)) %>%
    dplyr::transmute(
      team            = .normalize_team_codes(as.character(.data$team)),
      qb_id           = as.character(.data$qb_id),
      committee_group = as.character(.data$committee_group)
    ) %>%
    dplyr::filter(.data$team %in% ACTIVE_TEAMS_2026, !is.na(.data$qb_id)) %>%
    dplyr::distinct(.data$team, .data$qb_id, .keep_all = TRUE)

  if (nrow(out) > 0L) {
    message(glue("  QB committees: {nrow(out)} candidate(s) across ",
                 "{dplyr::n_distinct(out$team)} team(s) for season {season}"))
  }
  out
}

#' Score committee candidates on the league-starter z-scale
#'
#' Recovers the cpoe/epa distribution from the resolved-starter pool (so the
#' scale is identical to .score_qb_starters and non-committee output is
#' untouched), then z-scores each candidate. Candidates with fewer than
#' min_dropbacks prior-season dropbacks (e.g. rookies) land neutral (0).
#'
#' @param committee Tibble from .load_qb_committees().
#' @param qb_stats_2025 Per-qb_id prior-season stats attached to the quality
#'   table (qb_cpoe_mean, qb_epa_per_db, n_dropbacks). May be NULL.
#' @param main_quality The resolved-starter quality table (defines the pool).
#' @param min_dropbacks Integer qualification threshold.
#' @return committee with an added qb_quality_score column.
#' @keywords internal
.score_committee_candidates <- function(committee, qb_stats_2025, main_quality,
                                        min_dropbacks = MIN_DROPBACKS_QB) {

  if (nrow(committee) == 0L) {
    return(committee %>% dplyr::mutate(qb_quality_score = numeric(0)))
  }
  if (is.null(qb_stats_2025) || nrow(qb_stats_2025) == 0L) {
    message("  Committee scoring: no prior-season QB stats -- all neutral")
    return(committee %>% dplyr::mutate(qb_quality_score = 0))
  }

  pool <- main_quality %>%
    dplyr::select(.data$qb_id) %>%
    dplyr::inner_join(qb_stats_2025, by = "qb_id") %>%
    dplyr::filter(!is.na(.data$n_dropbacks), .data$n_dropbacks >= min_dropbacks)

  cpoe_mean <- mean(pool$qb_cpoe_mean, na.rm = TRUE)
  cpoe_sd   <- stats::sd(pool$qb_cpoe_mean, na.rm = TRUE)
  epa_mean  <- mean(pool$qb_epa_per_db, na.rm = TRUE)
  epa_sd    <- stats::sd(pool$qb_epa_per_db, na.rm = TRUE)
  if (is.na(cpoe_mean)) cpoe_mean <- 0
  if (is.na(epa_mean))  epa_mean  <- 0
  if (is.na(cpoe_sd) || cpoe_sd <= 0) cpoe_sd <- 1
  if (is.na(epa_sd)  || epa_sd  <= 0) epa_sd  <- 1

  committee %>%
    dplyr::left_join(
      qb_stats_2025 %>% dplyr::select(.data$qb_id, .data$qb_cpoe_mean,
                                      .data$qb_epa_per_db, .data$n_dropbacks),
      by = "qb_id"
    ) %>%
    dplyr::mutate(
      .qualifies = !is.na(.data$n_dropbacks) &
        .data$n_dropbacks >= min_dropbacks,
      .cpoe_z = dplyr::if_else(.data$.qualifies,
                               (.data$qb_cpoe_mean - cpoe_mean) / cpoe_sd, 0),
      .epa_z  = dplyr::if_else(.data$.qualifies,
                               (.data$qb_epa_per_db - epa_mean) / epa_sd, 0),
      qb_quality_score = dplyr::if_else(.data$.qualifies,
                                        (.data$.cpoe_z + .data$.epa_z) / 2, 0)
    ) %>%
    dplyr::select(.data$team, .data$qb_id, .data$committee_group,
                  .data$qb_quality_score)
}

#' Fan committee teams into one row per listed candidate (Option A)
#'
#' Replaces each committee team's single resolved-starter row with exactly the
#' listed candidates. Each candidate's passing is the resolved starter's passing
#' scaled by the ratio of quality adjustments; team volume and SOS are shared,
#' so the ratio is exact and no re-run is needed. If a committee team's resolved
#' starter is not on the list, a loud warning names him and he is dropped.
#'
#' @param output The finished per-team output (has qb_id, qb_quality_score,
#'   projected_* columns, one row per team).
#' @param committee_scored Tibble (team, qb_id, committee_group,
#'   qb_quality_score) from .score_committee_candidates().
#' @return output with committee teams fanned and a committee_group column.
#' @keywords internal
.append_committee_qb_rows <- function(output, committee_scored) {

  output <- output %>% dplyr::mutate(committee_group = NA_character_)
  if (nrow(committee_scored) == 0L) return(output)

  comm_teams <- unique(committee_scored$team)

  for (tm in comm_teams) {
    resolved <- output$qb_id[output$team == tm]
    listed   <- committee_scored$qb_id[committee_scored$team == tm]
    miss <- setdiff(resolved[!is.na(resolved)], listed)
    if (length(miss) > 0L) {
      warning(glue("Committee team {tm}: R/30-resolved starter(s) ",
                   "{paste(miss, collapse = ', ')} not in the committee list; ",
                   "dropped (Option A)."), call. = FALSE)
    }
  }

  keep <- output %>% dplyr::filter(!.data$team %in% comm_teams)
  base <- output %>%
    dplyr::filter(.data$team %in% comm_teams) %>%
    dplyr::select(-committee_group) %>%
    dplyr::rename(start_qb_id = qb_id, start_q = qb_quality_score)

  eff <- QB_EFFICIENCY_SENSITIVITY
  vol <- QB_VOLUME_SENSITIVITY

  fanned <- committee_scored %>%
    dplyr::rename(cand_q = qb_quality_score) %>%
    dplyr::left_join(base, by = "team") %>%
    dplyr::mutate(
      ratio_eff  = (1 + .data$cand_q * eff) / (1 + .data$start_q * eff),
      ratio_pass = (1 + .data$cand_q * vol) / (1 + .data$start_q * vol),
      ratio_rush = (2 - (1 + .data$cand_q * vol)) /
                   (2 - (1 + .data$start_q * vol)),
      qb_quality_score      = .data$cand_q,
      projected_pass_pg     = .data$projected_pass_pg * .data$ratio_pass,
      projected_rush_pg     = .data$projected_rush_pg * .data$ratio_rush,
      projected_pass_yds_pg = .data$projected_pass_yds_pg * .data$ratio_eff,
      projected_pass_tds_pg = .data$projected_pass_tds_pg * .data$ratio_eff
    ) %>%
    dplyr::select(dplyr::any_of(names(output)))

  dplyr::bind_rows(keep, fanned) %>%
    dplyr::arrange(.data$team, dplyr::desc(.data$qb_quality_score))
}

# ------------------------------------------------------------------------------
# project_team_volumes
# ------------------------------------------------------------------------------

#' Project 2026 team-level volume, pace, and scoring rate
#'
#' Top-level orchestrator. Loads 3 historical seasons of pbp, computes
#' per-team volume aggregates, blends with 2026 HC prior team patterns
#' (70/30), then applies a QB quality adjustment.
#'
#' Output is the foundation for R/31 player allocation and R/32 projection
#' reconciliation. Every per-player projection downstream is constrained
#' against these team totals.
#'
#' @param cache_dir Character. R/15 cache directory.
#' @param coaching_changes_path Character. Path to coaching changes CSV.
#' @param qb_changes_path Character. Path to optional QB changes CSV override.
#' @param as_of_week Integer 1-18 or NULL. When NULL (default), SOS is the
#'   preseason historical+forward signal. When a week is supplied, the SOS
#'   signal tapers toward observed current-season opponent quality via
#'   compute_prior_weight(as_of_week). Requires current_season_sos to have an
#'   effect; without it the preseason signal is retained.
#' @param current_season_sos Tibble or NULL. Observed current-season schedule
#'   quality per team (columns: team, cur_pass_sos, cur_rush_sos, expressed as
#'   EPA allowed relative to league average, same convention as the preseason
#'   signal). Only consulted when as_of_week is non-NULL.
#' @param save_output Logical. If TRUE, write RDS + CSV to OUTPUT_*_PATH.
#' @return Tibble with 32 rows (one per active 2026 team) and the full
#'   set of historical, blended, and projected columns documented at the
#'   top of this file.
#'
#' @seealso load_normalized_season (R/15), compute_prior_weight (R/29)
#' @export
project_team_volumes <- function(cache_dir = CACHE_DIR_DEFAULT,
                                  coaching_changes_path = COACHING_CHANGES_PATH,
                                  qb_changes_path = QB_CHANGES_PATH,
                                  as_of_week = NULL,
                                  current_season_sos = NULL,
                                  save_output = TRUE) {

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/30: Projecting team volumes for season {SEASON}"))
  message(glue("Historical window: {paste(HISTORICAL_SEASONS, collapse = ', ')}"))
  message(glue("Blend weights: team {TEAM_PATTERN_WEIGHT}, ",
               "coach {COACH_PATTERN_WEIGHT}"))
  if (is.null(as_of_week)) {
    message("SOS mode: preseason (historical + forward schedule)")
  } else {
    message(glue("SOS mode: in-season at week {as_of_week} ",
                 "(preseason signal tapers toward observed)"))
  }
  message(glue("{strrep('=', 70)}"))

  # STEP 1: Team historical volume aggregation
  message("\nSTEP 1/6: Aggregating team historical volume")
  team_history <- .compute_team_historical_volume(
    cache_dir = cache_dir,
    seasons   = HISTORICAL_SEASONS
  )

  # STEP 2: Load coaching changes (auto-detect from schedules + CSV override)
  message("\nSTEP 2/6: Loading 2026 coaching changes")
  coaching_changes <- .load_coaching_changes(
    path   = coaching_changes_path,
    season = SEASON
  )

  # STEP 3: Coach prior pattern
  message("\nSTEP 3/6: Computing coach prior team patterns")
  coach_prior <- .compute_coach_prior_pattern(
    coaching_changes = coaching_changes,
    team_history     = team_history
  )

  # STEP 4: Blend team + coach
  message("\nSTEP 4/6: Blending team and coach patterns (70/30)")
  blended <- .blend_team_and_coach_pattern(
    team_history = team_history,
    coach_prior  = coach_prior
  )

  blended <- blended %>%
    dplyr::left_join(
      coaching_changes %>% dplyr::select(.data$team, .data$prior_team) %>%
        dplyr::rename(prior_hc_team = .data$prior_team),
      by = "team"
    )

  # STEP 5: QB quality adjustment
  message("\nSTEP 5/6: Computing QB quality index and applying adjustment")
  qb_quality <- .compute_qb_quality_index(
    cache_dir       = cache_dir,
    qb_changes_path = qb_changes_path,
    season          = SEASON
  )
  projected <- .apply_qb_quality_adjustment(
    blended    = blended,
    qb_quality = qb_quality
  )

  # STEP 6: Strength-of-schedule adjustment (v2)
  message("\nSTEP 6/6: Computing strength-of-schedule and applying adjustment")

  # Fit the SOS sensitivities empirically from team-game data (see
  # .calibrate_sos_sensitivity for the design). Computed from live data every
  # run -- never hardcoded.
  sens <- .calibrate_sos_sensitivity(
    seasons = HISTORICAL_SEASONS, cache_dir = cache_dir
  )
  if (sens$calibrated) {
    message(glue(
      "    SOS sensitivities fitted from ",
      "{format(sens$n_games, big.mark = ',')} team-games: ",
      "pass eff {format(round(sens$pass_eff, 3), nsmall = 3)}, ",
      "rush eff {format(round(sens$rush_eff, 3), nsmall = 3)}, ",
      "pass vol {format(round(sens$pass_vol, 3), nsmall = 3)}, ",
      "rush vol {format(round(sens$rush_vol, 3), nsmall = 3)}"
    ))
  } else {
    warning("SOS sensitivities NOT calibrated -- fallback priors in use. ",
            "Factors this run are based on unvalidated constants.",
            call. = FALSE)
  }

  # Defensive quality per team over the prior-season window (shared lookup).
  def_team_season <- .compute_team_defense_allowed(
    seasons = HISTORICAL_SEASONS, cache_dir = cache_dir
  )
  def_quality <- .compute_defense_quality_by_team(def_team_season)

  hist_sos <- .compute_historical_sos(
    seasons = HISTORICAL_SEASONS, cache_dir = cache_dir,
    def_quality = def_quality
  )
  fwd_sos <- .compute_forward_sos(season = SEASON, def_quality = def_quality)

  message(glue("    SOS inputs: {nrow(def_quality)} defenses scored, ",
               "{nrow(hist_sos)} teams with historical SOS, ",
               "{nrow(fwd_sos)} teams with forward SOS"))

  projected <- .apply_sos_adjustment(
    projected          = projected,
    hist_sos           = hist_sos,
    fwd_sos            = fwd_sos,
    as_of_week         = as_of_week,
    current_season_sos = current_season_sos,
    sens               = sens
  )

  # Final output columns in documented order
  output <- projected %>%
    dplyr::mutate(
      season     = SEASON,
      schema_tag = R30_SCHEMA_TAG
    ) %>%
    # Carry the resolved 2026 starter gsis into the output. .compute_qb_quality
    # _index() resolves it (STEP 5) but .apply_qb_quality_adjustment() only
    # pulled qb_quality_score across, so qb_id was computed and then dropped
    # before output. Downstream R/32 uses qb_id as a QB1 starter consistency
    # check. One row per team, so this join does not fan out.
    dplyr::left_join(
      qb_quality %>% dplyr::select(.data$team, .data$qb_id),
      by = "team"
    ) %>%
    dplyr::select(
      .data$team, .data$season, .data$games_observed,
      .data$coach_change_flag, .data$prior_hc_team,
      .data$historical_pass_pg, .data$historical_rush_pg,
      .data$historical_plays_pg, .data$historical_proe,
      .data$historical_pass_yds_pg, .data$historical_rush_yds_pg,
      .data$historical_pass_tds_pg, .data$historical_rush_tds_pg,
      .data$coach_pass_pg, .data$coach_rush_pg, .data$coach_proe,
      .data$blended_pass_pg, .data$blended_rush_pg,
      .data$blended_plays_pg, .data$blended_proe,
      .data$qb_quality_score, .data$qb_id,
      .data$pass_sos_factor, .data$rush_sos_factor,
      .data$projected_pass_pg, .data$projected_rush_pg,
      .data$projected_plays_pg,
      .data$projected_pass_yds_pg, .data$projected_rush_yds_pg,
      .data$projected_pass_tds_pg, .data$projected_rush_tds_pg,
      .data$schema_tag
    ) %>%
    dplyr::arrange(.data$team)

  # Committee QB rooms: fan flagged teams into one row per listed candidate,
  # each valued as a full-time starter (adds committee_group; NA elsewhere).
  committee <- .load_qb_committees(season = SEASON)
  committee_scored <- .score_committee_candidates(
    committee      = committee,
    qb_stats_2025  = attr(qb_quality, "qb_stats_2025"),
    main_quality   = qb_quality
  )
  output <- .append_committee_qb_rows(output, committee_scored)

  # Save outputs
  if (save_output) {
    dir.create(dirname(R30_OUTPUT_RDS_PATH), recursive = TRUE, showWarnings = FALSE)
    saveRDS(output, R30_OUTPUT_RDS_PATH)
    readr::write_csv(output, R30_OUTPUT_CSV_PATH)
    message(glue("\n  Saved: {R30_OUTPUT_RDS_PATH}"))
    message(glue("  Saved: {R30_OUTPUT_CSV_PATH}"))
  }

  # KEY INSIGHTS (computed from output, never hardcoded)
  n_teams <- nrow(output)
  n_coach_changes <- sum(output$coach_change_flag, na.rm = TRUE)
  league_avg_pass <- mean(output$projected_pass_pg, na.rm = TRUE)
  league_avg_rush <- mean(output$projected_rush_pg, na.rm = TRUE)
  top_pass_team <- output$team[which.max(output$projected_pass_pg)]
  top_pass_pg   <- max(output$projected_pass_pg, na.rm = TRUE)
  top_rush_team <- output$team[which.max(output$projected_rush_pg)]
  top_rush_pg   <- max(output$projected_rush_pg, na.rm = TRUE)
  top_qb_team   <- output$team[which.max(output$qb_quality_score)]
  top_qb_score  <- max(output$qb_quality_score, na.rm = TRUE)

  # SOS: most favorable and toughest pass schedules (computed from output)
  easiest_pass_team <- output$team[which.max(output$pass_sos_factor)]
  easiest_pass_fac  <- max(output$pass_sos_factor, na.rm = TRUE)
  hardest_pass_team <- output$team[which.min(output$pass_sos_factor)]
  hardest_pass_fac  <- min(output$pass_sos_factor, na.rm = TRUE)
  n_sos_adjusted    <- sum(
    abs(output$pass_sos_factor - 1) > 0.001 |
      abs(output$rush_sos_factor - 1) > 0.001,
    na.rm = TRUE
  )

  message(glue("\n{strrep('=', 70)}"))
  message("KEY INSIGHTS")
  message(glue("{strrep('=', 70)}"))
  message(glue("  Active teams projected:   {n_teams}"))
  message(glue("  Coaching changes applied: {n_coach_changes}"))
  message(glue("  League avg pass attempts: {format(round(league_avg_pass, 1), nsmall = 1)} per game"))
  message(glue("  League avg rush attempts: {format(round(league_avg_rush, 1), nsmall = 1)} per game"))
  message(glue("  Highest projected pass:   {top_pass_team} at ",
               "{format(round(top_pass_pg, 1), nsmall = 1)} attempts/game"))
  message(glue("  Highest projected rush:   {top_rush_team} at ",
               "{format(round(top_rush_pg, 1), nsmall = 1)} attempts/game"))
  message(glue("  Highest QB quality:       {top_qb_team} at z = ",
               "{format(round(top_qb_score, 2), nsmall = 2)}"))
  message(glue("  Teams SOS-adjusted:       {n_sos_adjusted}"))
  message(glue("  Easiest pass schedule:    {easiest_pass_team} ",
               "(factor {format(round(easiest_pass_fac, 3), nsmall = 3)})"))
  message(glue("  Toughest pass schedule:   {hardest_pass_team} ",
               "(factor {format(round(hardest_pass_fac, 3), nsmall = 3)})"))
  message(glue("{strrep('=', 70)}\n"))

  output
}
