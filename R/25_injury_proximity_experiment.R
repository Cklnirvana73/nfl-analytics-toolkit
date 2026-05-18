# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 11
# Injury Proximity Natural Experiment
# File: R/25_injury_proximity_experiment.R
#
# Purpose: Design and execute a natural experiment measuring the causal effect
#          of early-season injury return timing on second-half fantasy output
#          and projection accuracy. Uses absence-based injury inference from
#          nflfastR play-by-play across the full multi-season panel.
#
# Design (confirmed in handoff, 2026-05-07):
#   - Data source    : Multi-season nflfastR PBP (2010-2025) via
#                      load_normalized_season() from R/15
#   - Scoring        : Weekly PPR via calculate_fantasy_points_ext() from R/17
#   - Treatment      : Players who returned from injury absence in Weeks 1-4
#                      of a given season (return_week <= TREATMENT_RETURN_MAX)
#   - Control        : Players healthy all 8 training weeks (no absence)
#   - Outcome        : PPR fantasy points Weeks 9-18 of the same season
#   - Prediction     : Prior-season weekly PPG (projection proxy)
#   - Analysis range : 2011-2025 (2010 excluded -- no prior-year baseline)
#
# Injury inference methodology:
#   nflfastR contains no explicit injury flag. Absence is inferred when:
#     (a) A skill-position player has zero offensive plays in a given week,
#     (b) Their team played that week (not a bye), AND
#     (c) They had >= MIN_PRIOR_GAMES appearances before the absence.
#   Known limitation: inferred absence may reflect benching, roster deactivation
#   for non-injury reasons, or mid-season trade timing. Validated via
#   validate_injury_assumptions() and tested in injury_robustness_check().
#
# Navigation:
#   Line  ~100 : Libraries
#   Line  ~120 : Source guards
#   Line  ~180 : Constants
#   Line  ~240 : NSE declarations
#   Line  ~270 : Internal helpers
#                  .build_team_schedule()
#                  .extract_player_participation()
#                  .compute_weekly_fantasy_season()
#                  .bootstrap_mean_diff_ci()
#   Line  ~520 : identify_returning_players()
#   Line  ~680 : classify_treatment_control_injury()
#   Line  ~820 : check_balance_injury_groups()
#   Line  ~990 : validate_injury_assumptions()
#   Line ~1150 : design_power_analysis_injury()
#   Line ~1260 : create_injury_experiment_specification()
#   Line ~1380 : calculate_injury_effect()
#   Line ~1610 : analyze_injury_heterogeneous_effects()
#   Line ~1780 : injury_robustness_check()
#   Line ~1960 : run_week11_pipeline()
#
# Source dependencies:
#   R/15_multi_season_pbp.R  -- load_normalized_season()
#   R/17_extended_scoring.R  -- calculate_fantasy_points_ext()
#
# Outputs (written by run_week11_pipeline()):
#   data/season2_cache/s2_week11_participation_records.rds
#   data/season2_cache/s2_week11_team_schedule.rds
#   data/season2_cache/s2_week11_weekly_fantasy.rds
#   data/season2_cache/s2_week11_groups.rds
#   data/season2_cache/s2_week11_effects.rds
#   data/season2_cache/s2_week11_experiment_spec.txt
#
# Season 2 output prefix : s2_week11_
# Schema tag             : s2_w11_v1
# Author                 : Christian LeBlanc
# Created                : 2026-05
# ==============================================================================


# ==============================================================================
# LIBRARIES
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(here)
library(nflreadr)


# ==============================================================================
# SOURCE GUARDS
# ==============================================================================

# R/15: load_normalized_season() -- primary PBP cache loader
if (!exists("load_normalized_season", mode = "function")) {
  week15_path <- here::here("R", "15_multi_season_pbp.R")
  if (!file.exists(week15_path)) {
    stop(glue(
      "R/15_multi_season_pbp.R not found at: {week15_path}\n",
      "Required for load_normalized_season()."
    ), call. = FALSE)
  }
  source(week15_path)
}

# R/17: calculate_fantasy_points_ext() -- weekly PPR scoring
if (!exists("calculate_fantasy_points_ext", mode = "function")) {
  week17_path <- here::here("R", "17_extended_scoring.R")
  if (!file.exists(week17_path)) {
    stop(glue(
      "R/17_extended_scoring.R not found at: {week17_path}\n",
      "Required for calculate_fantasy_points_ext()."
    ), call. = FALSE)
  }
  source(week17_path)
}


# ==============================================================================
# CONSTANTS
# ==============================================================================

# Full multi-season panel range (matches R/15 SEASONS_DEFAULT)
SEASONS_W11 <- 2010L:2025L

# Analysis seasons: exclude 2010. Prior-year (2009) data is outside the R/15
# cache floor. Every analysis season requires a prior-year baseline computed
# from SEASONS_W11 data. First usable analysis season = 2011 (prior = 2010).
ANALYSIS_SEASONS_W11 <- 2011L:2025L

# Training window: Weeks 1-8 per season.
# Absence classification and group assignment are confined to this window.
TRAINING_WEEKS_W11 <- 1L:8L

# Outcome window: Weeks 9-18 per season.
# Reflects the second half of the regular season across all schedule formats
# (16-game era through 17-game era). Week 18 exists from 2021 onward; earlier
# seasons naturally produce fewer outcome weeks with no imputation applied.
OUTCOME_WEEKS_W11 <- 9L:18L

# Treatment return threshold (outline spec: players who returned in Weeks 1-4).
# Players whose first post-absence participation week is <= this value are
# classified as treatment. Robustness checks sweep this value.
TREATMENT_RETURN_MAX_W11 <- 4L

# Minimum games played in training weeks BEFORE the first inferred absence.
# Guards against classifying players who never participated as returners.
# Two games required: one to establish presence, one to confirm it.
MIN_PRIOR_GAMES_W11 <- 2L

# Minimum training-window games for inclusion in either group.
# Treatment: games played after return_week within training window.
# Control: total training-window games.
MIN_TRAINING_GAMES_W11 <- 3L

# Minimum outcome-window games required to compute the player's Weeks 9-18
# summary. Players below this threshold are excluded from the effect analysis.
MIN_OUTCOME_GAMES_W11 <- 3L

# Minimum prior-season games required to use prior-season PPG as a prediction.
# Players below this threshold have unreliable baselines and are excluded.
MIN_PRIOR_SEASON_GAMES_W11 <- 4L

# Skill positions included in the analysis.
SKILL_POSITIONS_W11 <- c("QB", "RB", "WR", "TE")

# Era breakpoint for heterogeneous effects analysis.
# "Early" = 2011:(ERA_BREAKPOINT - 1); "Modern" = ERA_BREAKPOINT:2025.
# 2017 chosen: CBA/safety rule changes materially altered injury management
# and roster strategy.
ERA_BREAKPOINT_W11 <- 2017L

# Offensive play types used for participation inference.
# Matches R/17 calculate_fantasy_points_ext() filtering behavior.
OFFENSIVE_PLAY_TYPES_W11 <- c("pass", "run")

# Cache directory (inherits from R/15 convention)
CACHE_DIR_W11 <- here::here("data", "season2_cache")

# Bootstrap resamples for CI estimation
BOOTSTRAP_B_W11 <- 1000L

# Schema version tag
SCHEMA_TAG_W11 <- "s2_w11_v1"


# ==============================================================================
# NSE DECLARATIONS
# ==============================================================================

utils::globalVariables(c(
  # PBP columns (verified against R/15 normalize_schema output)
  "season", "week", "game_id", "play_type", "posteam",
  "sack", "qb_scramble",
  "passer_player_id", "passer_player_name",
  "rusher_player_id", "rusher_player_name",
  "receiver_player_id", "receiver_player_name",
  # Roster columns (verified against R/16 ROSTER_COLS_REQUIRED)
  "gsis_id", "full_name", "position",
  # Derived participation columns
  "player_id", "player_name", "team", "team_played",
  "participated", "absent",
  # Derived experiment columns
  "first_absent_week", "last_absent_week", "return_week", "n_absent_weeks",
  "n_prior_games", "n_training_games_post_return",
  "group", "era", "weeks_post_return", "return_week_category",
  "n_outcome_games", "n_prior_season_games",
  # Scoring columns (verified against R/17 @return documentation)
  "total_fantasy_points",
  # Analysis columns
  "prior_ppg", "outcome_ppg", "prediction_error", "abs_error",
  "mean_ppg", "mean_prior_ppg", "mean_abs_error",
  "effect_est", "ci_lower", "ci_upper", "p_value", "cohens_d",
  "n_players", "sd_ppg",
  # Misc
  "games_in_window", "smd"
))


# ==============================================================================
# INTERNAL HELPERS (not exported; prefix with .)
# ==============================================================================

# ------------------------------------------------------------------------------
# .build_team_schedule
# ------------------------------------------------------------------------------
# Build a lookup of which teams played each (season, week).
# Used to distinguish true absences from bye weeks.
#
# @param pbp Data frame. Single-season normalized PBP from
#   load_normalized_season(). Must contain: season, week, posteam.
# @return Tibble with one row per (team, season, week) combination where that
#   team appeared as posteam in at least one play. A team absent from a given
#   week's posteam values was on bye.
# @noRd
.build_team_schedule <- function(pbp) {

  required <- c("season", "week", "posteam")
  missing  <- setdiff(required, names(pbp))
  if (length(missing) > 0L) {
    stop(glue(
      ".build_team_schedule(): pbp missing columns: ",
      "{paste(missing, collapse = ', ')}"
    ), call. = FALSE)
  }

  pbp %>%
    dplyr::filter(!is.na(posteam)) %>%
    dplyr::distinct(season, week, team = posteam) %>%
    dplyr::arrange(season, week, team)
}


# ------------------------------------------------------------------------------
# .extract_player_participation
# ------------------------------------------------------------------------------
# Extract all (player_id, season, week, team) combinations where a skill-
# position player had at least one offensive play.
#
# Attribution rules (consistent with R/17 scoring):
#   passer_player_id  : QBs on pass attempts, sacks, scrambles
#   rusher_player_id  : Designed rushers only (excludes sacks and QB scrambles
#                       -- QBs are already captured via passer_player_id)
#   receiver_player_id: Any targeted receiver (caught or incomplete)
#
# sack and qb_scramble are coerced to integer before comparison to handle NAs
# in older seasons. coalesce() fills NA with 0L (no sack / not a scramble).
#
# @param pbp Data frame. Single-season normalized PBP.
# @return Tibble with one row per (player_id, season, week). Columns:
#   player_id, player_name, season, week, team (posteam). One row per
#   (player_id, season, week); multiple-team trades handled via last(team).
# @noRd
.extract_player_participation <- function(pbp) {

  required <- c(
    "season", "week", "play_type", "posteam",
    "passer_player_id", "passer_player_name",
    "rusher_player_id", "rusher_player_name",
    "receiver_player_id", "receiver_player_name"
  )
  missing <- setdiff(required, names(pbp))
  if (length(missing) > 0L) {
    stop(glue(
      ".extract_player_participation(): pbp missing columns: ",
      "{paste(missing, collapse = ', ')}"
    ), call. = FALSE)
  }

  # Filter to offensive play types: "pass" and "run".
  # Excludes kickoffs, punts, field goals, no-plays, penalty-only rows.
  pbp_off <- pbp %>%
    dplyr::filter(play_type %in% OFFENSIVE_PLAY_TYPES_W11)

  # Passers: QB on pass attempts, sacks, scrambles.
  # passer_player_id is set whenever the QB was the primary actor.
  passers <- pbp_off %>%
    dplyr::filter(!is.na(passer_player_id)) %>%
    dplyr::select(
      player_id   = passer_player_id,
      player_name = passer_player_name,
      season, week, team = posteam
    )

  # Rushers: designed rushing plays only.
  # Exclude sacks (sack == 1) and QB scrambles (qb_scramble == 1) because
  # both are already captured through passer_player_id. NA-safe via coalesce.
  rushers <- pbp_off %>%
    dplyr::filter(
      !is.na(rusher_player_id),
      dplyr::coalesce(as.integer(sack),        0L) != 1L,
      dplyr::coalesce(as.integer(qb_scramble), 0L) != 1L
    ) %>%
    dplyr::select(
      player_id   = rusher_player_id,
      player_name = rusher_player_name,
      season, week, team = posteam
    )

  # Receivers: all targeted receivers regardless of catch outcome.
  receivers <- pbp_off %>%
    dplyr::filter(!is.na(receiver_player_id)) %>%
    dplyr::select(
      player_id   = receiver_player_id,
      player_name = receiver_player_name,
      season, week, team = posteam
    )

  # Union all three sources, drop any rows with NA player_id.
  # Deduplicate to one row per (player_id, season, week).
  # last(player_name) and last(team) handle rare mid-game data inconsistencies.
  dplyr::bind_rows(passers, rushers, receivers) %>%
    dplyr::filter(!is.na(player_id)) %>%
    dplyr::group_by(player_id, season, week) %>%
    dplyr::summarise(
      player_name = dplyr::last(player_name),
      team        = dplyr::last(team),
      .groups     = "drop"
    )
}


# ------------------------------------------------------------------------------
# .compute_weekly_fantasy_season
# ------------------------------------------------------------------------------
# Thin wrapper: calls calculate_fantasy_points_ext() for one season's PBP and
# returns only the columns needed downstream, keeping weekly_fantasy lean.
#
# @param pbp Data frame. Single-season PBP.
# @param season_val Integer. Season year (for calculate_fantasy_points_ext()).
# @param roster_data Data frame or NULL. Passed to calculate_fantasy_points_ext
#   for position anchoring. If NULL, position is inferred from play data.
# @return Tibble. Columns: season, week, game_id, player_id, player_name,
#   position, team, total_fantasy_points. One row per (player_id, season, week).
#   Column names verified against R/17 @return documentation.
# @noRd
.compute_weekly_fantasy_season <- function(pbp, season_val, roster_data = NULL) {

  scores <- calculate_fantasy_points_ext(
    pbp_data    = pbp,
    roster_data = roster_data,
    season      = season_val
  )

  # Retain only columns needed for the effect analysis. Column list verified
  # against R/17 calculate_fantasy_points_ext() @return documentation.
  # The function returns one row per player per game (season, week, game_id,
  # player_id). total_fantasy_points is the full PPR sum across all components.
  scores %>%
    dplyr::select(
      season, week, game_id, player_id, player_name,
      position, team, total_fantasy_points
    )
}


# ------------------------------------------------------------------------------
# .bootstrap_mean_diff_ci
# ------------------------------------------------------------------------------
# Parametric-free CI for the difference in means (group_a - group_b).
# Uses the basic (reverse-percentile) bootstrap.
# Falls back to Welch t-interval when min(n_a, n_b) < 10 to avoid degenerate
# bootstrap distributions on small samples.
#
# @param a Numeric vector. Group A values (treatment).
# @param b Numeric vector. Group B values (control).
# @param B Integer. Bootstrap resamples. Default BOOTSTRAP_B_W11.
# @param conf_level Numeric. Confidence level. Default 0.95.
# @return Named list: estimate, ci_lower, ci_upper, n_a, n_b, method.
# @noRd
.bootstrap_mean_diff_ci <- function(a, b, B = BOOTSTRAP_B_W11,
                                    conf_level = 0.95) {

  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  n_a <- length(a)
  n_b <- length(b)
  obs_diff <- mean(a) - mean(b)

  # Welch t-interval fallback for very small groups
  if (min(n_a, n_b) < 10L) {
    tt <- tryCatch(
      stats::t.test(a, b, conf.level = conf_level),
      error = function(e) NULL
    )
    if (is.null(tt)) {
      return(list(
        estimate = obs_diff, ci_lower = NA_real_, ci_upper = NA_real_,
        n_a = n_a, n_b = n_b, method = "welch_t_failed"
      ))
    }
    return(list(
      estimate = obs_diff,
      ci_lower = tt$conf.int[[1L]],
      ci_upper = tt$conf.int[[2L]],
      n_a = n_a, n_b = n_b, method = "welch_t"
    ))
  }

  # Basic bootstrap: resample each group independently
  set.seed(42L)
  boot_diffs <- replicate(B, {
    mean(sample(a, n_a, replace = TRUE)) -
      mean(sample(b, n_b, replace = TRUE))
  })

  alpha    <- 1 - conf_level
  ci_lower <- 2 * obs_diff - stats::quantile(boot_diffs, 1 - alpha / 2)
  ci_upper <- 2 * obs_diff - stats::quantile(boot_diffs, alpha / 2)

  list(
    estimate = obs_diff,
    ci_lower = unname(ci_lower),
    ci_upper = unname(ci_upper),
    n_a = n_a, n_b = n_b, method = "bootstrap"
  )
}


# ==============================================================================
# FUNCTION: identify_returning_players
# ==============================================================================

#' Identify Players Who Returned from Injury in the Training Window
#'
#' Scans multi-season participation records to find skill-position players who
#' had at least one inferred absence in the training window (Weeks 1-8 by
#' default) and returned to play before the training window closed.
#'
#' Injury absence is inferred when: (a) a player had zero offensive plays in
#' a week where their team played (not a bye), AND (b) they had played in at
#' least \code{min_prior_games} prior weeks of the same season.
#'
#' @param participation_records Tibble. Output of the internal
#'   \code{.extract_player_participation()} loop in
#'   \code{run_week11_pipeline()}. Columns: player_id (chr), player_name (chr),
#'   season (int), week (int), team (chr). One row per (player_id, season, week)
#'   where the player had at least one offensive play.
#' @param team_schedule Tibble. Output of the internal
#'   \code{.build_team_schedule()} loop. Columns: season (int), week (int),
#'   team (chr). One row per (team, season, week) where that team played.
#' @param roster_positions Tibble. Position lookup. Columns: player_id (chr),
#'   season (int), position (chr). Typically derived from
#'   \code{nflreadr::load_rosters()} with gsis_id renamed to player_id.
#' @param seasons Integer vector. Seasons to analyze.
#'   Default: \code{ANALYSIS_SEASONS_W11}.
#' @param training_weeks Integer vector. Weeks that define the training window.
#'   Default: \code{TRAINING_WEEKS_W11} (1L:8L).
#' @param min_prior_games Integer. Minimum training-week appearances before the
#'   first absence. Default: \code{MIN_PRIOR_GAMES_W11} (2L).
#' @param skill_positions Character vector. Positions to include.
#'   Default: \code{SKILL_POSITIONS_W11}.
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return Tibble with one row per returning player-season. Columns:
#'   \describe{
#'     \item{player_id}{chr: GSIS ID}
#'     \item{player_name}{chr}
#'     \item{season}{int}
#'     \item{team}{chr: last known team in training window}
#'     \item{position}{chr: from roster_positions}
#'     \item{first_absent_week}{int: first training week with inferred absence}
#'     \item{return_week}{int: first training week with participation after
#'       first_absent_week}
#'     \item{n_absent_weeks}{int: total absent training weeks before return_week}
#'     \item{n_prior_games}{int: training-week games before first_absent_week}
#'     \item{n_training_games_post_return}{int: training-week games after
#'       return_week (inclusive)}
#'   }
#'
#' @details
#' A player must satisfy ALL of the following to be returned:
#' \enumerate{
#'   \item Position is in \code{skill_positions}.
#'   \item Had at least \code{min_prior_games} training-week participations
#'     before their first inferred absence.
#'   \item Had at least one inferred absence in the training window.
#'   \item Returned (participated in at least one training week after their
#'     first absence), confirming the injury was not season-ending.
#' }
#'
#' @seealso \code{\link{classify_treatment_control_injury}},
#'   \code{\link{validate_injury_assumptions}}
#' @export
identify_returning_players <- function(
    participation_records,
    team_schedule,
    roster_positions,
    seasons        = ANALYSIS_SEASONS_W11,
    training_weeks = TRAINING_WEEKS_W11,
    min_prior_games = MIN_PRIOR_GAMES_W11,
    skill_positions = SKILL_POSITIONS_W11,
    verbose        = TRUE
) {

  # --- Input validation -------------------------------------------------------

  stopifnot(
    is.data.frame(participation_records),
    is.data.frame(team_schedule),
    is.data.frame(roster_positions),
    is.integer(seasons) || is.numeric(seasons),
    is.integer(training_weeks) || is.numeric(training_weeks),
    is.integer(min_prior_games) || is.numeric(min_prior_games),
    is.character(skill_positions),
    is.logical(verbose)
  )

  seasons        <- sort(as.integer(seasons))
  training_weeks <- sort(as.integer(training_weeks))
  min_prior_games <- as.integer(min_prior_games)

  required_part <- c("player_id", "player_name", "season", "week", "team")
  required_sched <- c("season", "week", "team")
  required_pos  <- c("player_id", "season", "position")

  miss_part  <- setdiff(required_part,  names(participation_records))
  miss_sched <- setdiff(required_sched, names(team_schedule))
  miss_pos   <- setdiff(required_pos,   names(roster_positions))

  if (length(miss_part)  > 0L)
    stop(glue("participation_records missing: {paste(miss_part, collapse=', ')}"),
         call. = FALSE)
  if (length(miss_sched) > 0L)
    stop(glue("team_schedule missing: {paste(miss_sched, collapse=', ')}"),
         call. = FALSE)
  if (length(miss_pos)   > 0L)
    stop(glue("roster_positions missing: {paste(miss_pos, collapse=', ')}"),
         call. = FALSE)

  # --- Position filter --------------------------------------------------------

  # Restrict to skill positions. Players not in roster_positions (no roster
  # match) are dropped -- prevents OL/DL participation noise from entering
  # the experiment.
  skilled_ids <- roster_positions %>%
    dplyr::filter(
      season %in% seasons,
      position %in% skill_positions
    ) %>%
    dplyr::distinct(player_id, season, position)

  if (verbose) {
    message(glue(
      "identify_returning_players(): {format(nrow(skilled_ids), big.mark=',')} ",
      "skill-position player-seasons in [{min(seasons)}-{max(seasons)}]"
    ))
  }

  # --- Build full training-week grid ------------------------------------------

  # For each player-season in skilled_ids, create one row per training week.
  # Then join team_schedule to determine team_played (not a bye), and join
  # participation_records to determine whether they actually played.

  # Step 1: Last known team and player_name per player-season within training
  # window. player_name must be carried here so it is available in grid and
  # in the downstream summarise(player_name = dplyr::last(player_name), ...).
  player_team <- participation_records %>%
    dplyr::filter(season %in% seasons, week %in% training_weeks) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      team        = dplyr::last(team),
      player_name = dplyr::last(player_name),
      .groups     = "drop"
    )

  # Step 2: Cross-join each player-season with training weeks
  grid <- skilled_ids %>%
    dplyr::left_join(player_team, by = c("player_id", "season")) %>%
    dplyr::filter(!is.na(team)) %>%          # drop if never appeared in training
    dplyr::mutate(week = list(training_weeks)) %>%
    tidyr::unnest(week) %>%
    dplyr::mutate(week = as.integer(week))

  # Step 3: Mark team_played: did the player's team play this week?
  schedule_flag <- team_schedule %>%
    dplyr::filter(season %in% seasons, week %in% training_weeks) %>%
    dplyr::mutate(team_played = TRUE)

  grid <- grid %>%
    dplyr::left_join(
      schedule_flag,
      by = c("season", "week", "team")
    ) %>%
    dplyr::mutate(team_played = dplyr::coalesce(team_played, FALSE))

  # Step 4: Mark participated
  played_flag <- participation_records %>%
    dplyr::filter(season %in% seasons, week %in% training_weeks) %>%
    dplyr::select(player_id, season, week) %>%
    dplyr::mutate(participated = TRUE)

  grid <- grid %>%
    dplyr::left_join(played_flag, by = c("player_id", "season", "week")) %>%
    dplyr::mutate(
      participated = dplyr::coalesce(participated, FALSE),
      absent       = team_played & !participated
    )

  # --- Identify returning players ---------------------------------------------

  # For each player-season, find the first absent week and the return week.
  returners <- grid %>%
    dplyr::arrange(player_id, season, week) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      player_name = dplyr::last(player_name),
      team        = dplyr::last(team),
      position    = dplyr::last(position),

      # Count prior games: participations BEFORE the first absent team-played week
      # Logic: cumulative participation up to (but not including) first absence.
      # We walk the ordered weeks: as long as there is no absence yet,
      # count participating weeks.
      first_absent_week = {
        weeks_w_absence <- week[absent]
        if (length(weeks_w_absence) == 0L) NA_integer_
        else min(weeks_w_absence)
      },

      n_prior_games = {
        faw <- week[absent]
        faw <- if (length(faw) == 0L) Inf else min(faw)
        sum(participated[week < faw & team_played])
      },

      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(first_absent_week)) %>%
    dplyr::filter(n_prior_games >= min_prior_games)

  if (nrow(returners) == 0L) {
    if (verbose) message("identify_returning_players(): no returning players found.")
    return(dplyr::tibble(
      player_id = character(), player_name = character(),
      season = integer(), team = character(), position = character(),
      first_absent_week = integer(), return_week = integer(),
      n_absent_weeks = integer(), n_prior_games = integer(),
      n_training_games_post_return = integer()
    ))
  }

  # Now find return_week and n_absent_weeks for confirmed returners.
  # Re-join with grid to get week-level detail per returning player-season.
  returner_detail <- returners %>%
    dplyr::select(player_id, season, first_absent_week, n_prior_games) %>%
    dplyr::left_join(
      grid %>% dplyr::select(player_id, season, week, participated,
                             absent, team_played),
      by = c("player_id", "season")
    ) %>%
    dplyr::filter(week >= first_absent_week) %>%
    dplyr::group_by(player_id, season, first_absent_week, n_prior_games) %>%
    dplyr::summarise(
      # return_week: first training week AFTER first_absent_week with participation
      return_week = {
        ret_weeks <- week[participated & week > first_absent_week[1L]]
        if (length(ret_weeks) == 0L) NA_integer_
        else min(ret_weeks)
      },
      # n_absent_weeks: team-played weeks between first_absent_week and return_week
      # where player did not participate
      n_absent_weeks = {
        rw <- {
          ret_weeks <- week[participated & week > first_absent_week[1L]]
          if (length(ret_weeks) == 0L) NA_integer_ else min(ret_weeks)
        }
        if (is.na(rw)) NA_integer_
        else sum(absent[week >= first_absent_week[1L] & week < rw])
      },
      n_training_games_post_return = {
        rw <- {
          ret_weeks <- week[participated & week > first_absent_week[1L]]
          if (length(ret_weeks) == 0L) NA_integer_ else min(ret_weeks)
        }
        if (is.na(rw)) NA_integer_
        else sum(participated[week >= rw])
      },
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(return_week))

  # Assemble final output
  result <- returners %>%
    dplyr::left_join(
      returner_detail %>%
        dplyr::select(player_id, season, return_week, n_absent_weeks,
                      n_training_games_post_return),
      by = c("player_id", "season")
    ) %>%
    dplyr::filter(!is.na(return_week)) %>%
    dplyr::select(
      player_id, player_name, season, team, position,
      first_absent_week, return_week, n_absent_weeks,
      n_prior_games, n_training_games_post_return
    ) %>%
    dplyr::arrange(season, position, player_name)

  if (verbose) {
    message(glue(
      "identify_returning_players(): {nrow(result)} returning player-seasons ",
      "across {length(unique(result$season))} seasons"
    ))
  }

  return(result)
}


# ==============================================================================
# FUNCTION: classify_treatment_control_injury
# ==============================================================================

#' Assign Treatment and Control Groups for the Injury Proximity Experiment
#'
#' Classifies skill-position player-seasons into three categories:
#' \strong{treatment} (returned from inferred injury in Weeks 1-4),
#' \strong{control} (healthy all 8 training weeks), or
#' \strong{excluded} (all other cases).
#'
#' @param returning_players Tibble. Output of
#'   \code{\link{identify_returning_players}}.
#' @param participation_records Tibble. Same object passed to
#'   \code{identify_returning_players}. Used to build the healthy-all-8 control
#'   pool.
#' @param team_schedule Tibble. Same object passed to
#'   \code{identify_returning_players}.
#' @param roster_positions Tibble. Same position lookup. Columns: player_id,
#'   season, position.
#' @param treatment_return_max Integer. Return weeks <= this value define the
#'   treatment group. Default: \code{TREATMENT_RETURN_MAX_W11} (4L).
#' @param min_training_games Integer. Minimum post-return training-window games
#'   for treatment, or total training-window games for control.
#'   Default: \code{MIN_TRAINING_GAMES_W11} (3L).
#' @param seasons Integer vector. Default: \code{ANALYSIS_SEASONS_W11}.
#' @param training_weeks Integer vector. Default: \code{TRAINING_WEEKS_W11}.
#' @param skill_positions Character vector. Default: \code{SKILL_POSITIONS_W11}.
#' @param verbose Logical. Default TRUE.
#'
#' @return Tibble with one row per player-season. Columns:
#'   \describe{
#'     \item{player_id}{chr}
#'     \item{player_name}{chr}
#'     \item{season}{int}
#'     \item{team}{chr}
#'     \item{position}{chr}
#'     \item{group}{chr: "treatment", "control", or "excluded"}
#'     \item{return_week}{int: NA for control and excluded}
#'     \item{first_absent_week}{int: NA for control and excluded}
#'     \item{n_absent_weeks}{int: NA for control and excluded}
#'     \item{n_prior_games}{int: NA for excluded}
#'     \item{era}{chr: "Early" or "Modern" based on ERA_BREAKPOINT_W11}
#'   }
#'
#' @seealso \code{\link{identify_returning_players}},
#'   \code{\link{check_balance_injury_groups}}
#' @export
classify_treatment_control_injury <- function(
    returning_players,
    participation_records,
    team_schedule,
    roster_positions,
    treatment_return_max = TREATMENT_RETURN_MAX_W11,
    min_training_games   = MIN_TRAINING_GAMES_W11,
    seasons              = ANALYSIS_SEASONS_W11,
    training_weeks       = TRAINING_WEEKS_W11,
    skill_positions      = SKILL_POSITIONS_W11,
    verbose              = TRUE
) {

  stopifnot(
    is.data.frame(returning_players),
    is.data.frame(participation_records),
    is.data.frame(team_schedule),
    is.data.frame(roster_positions),
    is.logical(verbose)
  )

  seasons        <- sort(as.integer(seasons))
  training_weeks <- sort(as.integer(training_weeks))

  # --- Treatment: return_week <= treatment_return_max -------------------------

  treatment <- returning_players %>%
    dplyr::filter(
      return_week      <= treatment_return_max,
      n_training_games_post_return >= min_training_games
    ) %>%
    dplyr::mutate(group = "treatment") %>%
    dplyr::select(player_id, player_name, season, team, position, group,
                  return_week, first_absent_week, n_absent_weeks, n_prior_games)

  # --- Control: no absence in all training weeks ------------------------------

  # For each skill-position player-season, build training-week participation
  # summary. Control = 0 absent team-played weeks across all training weeks.

  skilled_ids <- roster_positions %>%
    dplyr::filter(season %in% seasons, position %in% skill_positions) %>%
    dplyr::distinct(player_id, season, position)

  player_team <- participation_records %>%
    dplyr::filter(season %in% seasons, week %in% training_weeks) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(team = dplyr::last(team), .groups = "drop")

  schedule_flag <- team_schedule %>%
    dplyr::filter(season %in% seasons, week %in% training_weeks) %>%
    dplyr::mutate(team_played = TRUE)

  played_flag <- participation_records %>%
    dplyr::filter(season %in% seasons, week %in% training_weeks) %>%
    dplyr::select(player_id, season, week) %>%
    dplyr::mutate(participated = TRUE)

  ctrl_grid <- skilled_ids %>%
    dplyr::left_join(player_team, by = c("player_id", "season")) %>%
    dplyr::filter(!is.na(team)) %>%
    dplyr::mutate(week = list(training_weeks)) %>%
    tidyr::unnest(week) %>%
    dplyr::mutate(week = as.integer(week)) %>%
    dplyr::left_join(schedule_flag, by = c("season", "week", "team")) %>%
    dplyr::mutate(team_played = dplyr::coalesce(team_played, FALSE)) %>%
    dplyr::left_join(played_flag, by = c("player_id", "season", "week")) %>%
    dplyr::mutate(
      participated = dplyr::coalesce(participated, FALSE),
      absent       = team_played & !participated
    )

  # Player-season summary for control candidate pool
  player_names <- participation_records %>%
    dplyr::filter(season %in% seasons, week %in% training_weeks) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(player_name = dplyr::last(player_name), .groups = "drop")

  ctrl_summary <- ctrl_grid %>%
    dplyr::group_by(player_id, season, position, team) %>%
    dplyr::summarise(
      n_absent_training = sum(absent),
      n_training_games  = sum(participated),
      .groups = "drop"
    ) %>%
    dplyr::left_join(player_names, by = c("player_id", "season")) %>%
    dplyr::filter(
      n_absent_training == 0L,          # no absences in training window
      n_training_games  >= min_training_games
    ) %>%
    # Exclude any player-season already in treatment or returning_players
    dplyr::anti_join(
      returning_players %>% dplyr::select(player_id, season),
      by = c("player_id", "season")
    ) %>%
    dplyr::mutate(
      group             = "control",
      return_week       = NA_integer_,
      first_absent_week = NA_integer_,
      n_absent_weeks    = NA_integer_,
      n_prior_games     = n_training_games
    ) %>%
    dplyr::select(player_id, player_name, season, team, position, group,
                  return_week, first_absent_week, n_absent_weeks, n_prior_games)

  # --- Combine and add era label ----------------------------------------------

  groups <- dplyr::bind_rows(treatment, ctrl_summary) %>%
    dplyr::mutate(
      era = dplyr::if_else(
        season < ERA_BREAKPOINT_W11, "Early", "Modern"
      )
    ) %>%
    dplyr::arrange(season, position, group, player_name)

  if (verbose) {
    n_trt <- sum(groups$group == "treatment")
    n_ctl <- sum(groups$group == "control")
    message(glue(
      "classify_treatment_control_injury(): ",
      "{n_trt} treatment, {n_ctl} control player-seasons"
    ))
  }

  return(groups)
}


# ==============================================================================
# FUNCTION: check_balance_injury_groups
# ==============================================================================

#' Check Covariate Balance Between Treatment and Control Groups
#'
#' Compares treatment and control groups on observed pre-treatment covariates:
#' position distribution, era, and prior-season fantasy production. Reports
#' standardized mean differences (SMDs) to flag potential confounding.
#'
#' An SMD > 0.20 on any continuous covariate is flagged as a material
#' imbalance warranting interpretation caution (Austin 2011 threshold for
#' PS-matched studies; applied conservatively here as a pre-matching check).
#'
#' @param groups Tibble. Output of
#'   \code{\link{classify_treatment_control_injury}}.
#' @param weekly_fantasy Tibble. Player-week PPR scoring across all seasons.
#'   Columns: player_id (chr), season (int), week (int),
#'   total_fantasy_points (dbl), position (chr). Built in
#'   \code{run_week11_pipeline()}.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{balance_table}{Tibble. Group-level summary per position and era.}
#'     \item{smd_table}{Tibble. Standardized mean differences for continuous
#'       covariates (prior_ppg, n_absent_weeks). Columns: covariate, smd,
#'       flagged (lgl: TRUE if abs(smd) > 0.20).}
#'     \item{position_dist}{Tibble. Position distribution by group.}
#'     \item{era_dist}{Tibble. Era distribution by group.}
#'     \item{balance_summary}{chr. Human-readable summary string.}
#'   }
#'
#' @seealso \code{\link{classify_treatment_control_injury}},
#'   \code{\link{validate_injury_assumptions}}
#' @export
check_balance_injury_groups <- function(groups, weekly_fantasy, verbose = TRUE) {

  stopifnot(is.data.frame(groups), is.data.frame(weekly_fantasy))

  required_groups  <- c("player_id", "season", "group", "position", "era")
  required_fantasy <- c("player_id", "season", "week", "total_fantasy_points")

  miss_g <- setdiff(required_groups,  names(groups))
  miss_f <- setdiff(required_fantasy, names(weekly_fantasy))
  if (length(miss_g) > 0L)
    stop(glue("groups missing: {paste(miss_g, collapse=', ')}"), call. = FALSE)
  if (length(miss_f) > 0L)
    stop(glue("weekly_fantasy missing: {paste(miss_f, collapse=', ')}"),
         call. = FALSE)

  trt_ctrl <- groups %>% dplyr::filter(group %in% c("treatment", "control"))

  if (nrow(trt_ctrl) == 0L) {
    stop("check_balance_injury_groups(): groups contains no treatment or control rows.",
         call. = FALSE)
  }

  # --- Prior-season PPG as continuous covariate -------------------------------
  # Prior season = season - 1 for each analysis season.

  prior_ppg_tbl <- weekly_fantasy %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      prior_ppg_all_weeks = mean(total_fantasy_points, na.rm = TRUE),
      n_weeks             = dplyr::n(),
      .groups             = "drop"
    ) %>%
    dplyr::mutate(season_join = season + 1L) %>%
    dplyr::rename(prior_season = season) %>%
    dplyr::select(player_id, season_join, prior_ppg_all_weeks, n_weeks) %>%
    dplyr::rename(season = season_join)

  groups_with_ppg <- trt_ctrl %>%
    dplyr::left_join(prior_ppg_tbl, by = c("player_id", "season"))

  # --- SMD computation --------------------------------------------------------
  # SMD = (mean_treatment - mean_control) / sqrt((var_trt + var_ctrl) / 2)
  # Using pooled SD denominator (Cohen's convention).

  compute_smd <- function(tbl, var_name) {
    trt_sub  <- tbl[tbl$group == "treatment", ]
    ctl_sub  <- tbl[tbl$group == "control",   ]
    trt_vals <- trt_sub[[var_name]]
    ctl_vals <- ctl_sub[[var_name]]

    trt_vals <- trt_vals[!is.na(trt_vals)]
    ctl_vals <- ctl_vals[!is.na(ctl_vals)]

    if (length(trt_vals) < 2L || length(ctl_vals) < 2L) {
      return(NA_real_)
    }

    pooled_sd <- sqrt((stats::var(trt_vals) + stats::var(ctl_vals)) / 2)
    if (pooled_sd == 0) return(0)
    (mean(trt_vals) - mean(ctl_vals)) / pooled_sd
  }

  smd_ppg    <- compute_smd(groups_with_ppg, "prior_ppg_all_weeks")
  smd_absent <- compute_smd(
    groups %>% dplyr::filter(group %in% c("treatment", "control")),
    "n_prior_games"
  )

  smd_table <- dplyr::tibble(
    covariate = c("prior_season_ppg", "n_prior_games"),
    smd       = c(smd_ppg, smd_absent),
    flagged   = abs(c(smd_ppg, smd_absent)) > 0.20
  )

  # --- Group-level summary stats ----------------------------------------------

  balance_table <- groups_with_ppg %>%
    dplyr::group_by(group, position) %>%
    dplyr::summarise(
      n_player_seasons = dplyr::n(),
      mean_prior_ppg   = mean(prior_ppg_all_weeks, na.rm = TRUE),
      sd_prior_ppg     = stats::sd(prior_ppg_all_weeks, na.rm = TRUE),
      median_prior_ppg = stats::median(prior_ppg_all_weeks, na.rm = TRUE),
      .groups          = "drop"
    )

  position_dist <- trt_ctrl %>%
    dplyr::count(group, position) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
    dplyr::ungroup()

  era_dist <- trt_ctrl %>%
    dplyr::count(group, era) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
    dplyr::ungroup()

  # --- Summary string ---------------------------------------------------------

  n_flagged <- sum(smd_table$flagged, na.rm = TRUE)
  balance_summary <- glue(
    "Balance check: {n_flagged} of {nrow(smd_table)} covariates flagged ",
    "(abs(SMD) > 0.20). ",
    "Prior-season PPG SMD = {round(smd_ppg, 3)}. ",
    "N prior games SMD = {round(smd_absent, 3)}."
  )

  if (verbose) message(balance_summary)

  list(
    balance_table    = balance_table,
    smd_table        = smd_table,
    position_dist    = position_dist,
    era_dist         = era_dist,
    balance_summary  = as.character(balance_summary)
  )
}


# ==============================================================================
# FUNCTION: validate_injury_assumptions
# ==============================================================================

#' Validate Core Assumptions of the Injury Proximity Natural Experiment
#'
#' Runs five assumption checks:
#' \enumerate{
#'   \item \strong{Absence rate plausibility}: Inferred absence rates per
#'     position and era are within plausible injury-epidemiology ranges.
#'   \item \strong{Return timing distribution}: Confirms return weeks follow
#'     a reasonable distribution and are not dominated by a single week.
#'   \item \strong{Control group game-count consistency}: Controls play in a
#'     similar number of training-window games season over season.
#'   \item \strong{Pre-return performance trend}: Checks that treatment players
#'     show no systematic pre-absence performance difference from controls in
#'     the same prior season (parallel-trends proxy).
#'   \item \strong{Position composition stability}: Treatment and control share
#'     comparable position mixes across eras, guarding against era-confounded
#'     position shifts.
#' }
#'
#' @param groups Tibble. Output of
#'   \code{\link{classify_treatment_control_injury}}.
#' @param weekly_fantasy Tibble. Player-week PPR scores.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{absence_rate}{Tibble. Inferred absence rates by position and era.}
#'     \item{return_timing}{Tibble. Distribution of return weeks.}
#'     \item{control_consistency}{Tibble. Control group season-over-season
#'       training game counts.}
#'     \item{parallel_trends}{Tibble. Prior-season PPG comparison by group
#'       (parallel-trends proxy).}
#'     \item{position_stability}{Tibble. Position mix by group and era.}
#'     \item{checks_passed}{Named logical vector. One element per check.}
#'     \item{report}{chr. Human-readable assumption report.}
#'   }
#'
#' @seealso \code{\link{check_balance_injury_groups}},
#'   \code{\link{calculate_injury_effect}}
#' @export
validate_injury_assumptions <- function(groups, weekly_fantasy, verbose = TRUE) {

  stopifnot(is.data.frame(groups), is.data.frame(weekly_fantasy))

  trt <- groups %>% dplyr::filter(group == "treatment")
  ctl <- groups %>% dplyr::filter(group == "control")

  # --- Check A: Absence rate plausibility -------------------------------------
  # NFL positional injury rates 8%-15% per game-week (rough literature range).
  # We expect inferred absence rates in the 5%-20% range per position.
  # Values outside this suggest data quality issues.

  absence_rate <- trt %>%
    dplyr::group_by(position) %>%
    dplyr::summarise(
      n_player_seasons = dplyr::n(),
      mean_absent_weeks = mean(n_absent_weeks, na.rm = TRUE),
      sd_absent_weeks   = stats::sd(n_absent_weeks, na.rm = TRUE),
      .groups = "drop"
    )

  check_a <- all(
    absence_rate$mean_absent_weeks >= 0.5 &
      absence_rate$mean_absent_weeks <= 5.0,
    na.rm = TRUE
  )

  # --- Check B: Return timing distribution ------------------------------------
  # No single return week should represent > 60% of treatment players.
  # Over-concentration suggests a data artifact rather than real injury returns.

  return_timing <- trt %>%
    dplyr::count(return_week) %>%
    dplyr::mutate(pct = round(100 * n / sum(n), 1))

  max_return_pct <- if (nrow(return_timing) > 0L) max(return_timing$pct) else 0
  check_b <- max_return_pct <= 60

  # --- Check C: Control group consistency ------------------------------------
  # Control group season-level sample sizes should not fluctuate by more than
  # 3x between any two adjacent seasons. Large swings suggest inconsistent
  # absence detection across seasons.

  ctrl_by_season <- ctl %>%
    dplyr::count(season, name = "n_control") %>%
    dplyr::arrange(season) %>%
    dplyr::mutate(
      n_prev  = dplyr::lag(n_control),
      ratio   = n_control / dplyr::if_else(is.na(n_prev) | n_prev == 0,
                                            NA_real_, as.numeric(n_prev))
    )

  check_c <- all(ctrl_by_season$ratio <= 3, na.rm = TRUE)

  control_consistency <- ctrl_by_season %>%
    dplyr::select(season, n_control, ratio)

  # --- Check D: Parallel trends proxy ----------------------------------------
  # Prior-season PPG should not differ systematically between treatment and
  # control. A large gap (>2 PPG) suggests selection into injury is correlated
  # with baseline talent, threatening validity.

  prior_ppg_tbl <- weekly_fantasy %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(ppg = mean(total_fantasy_points, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(season = season + 1L)    # shift: prior season -> current season

  groups_ppg <- groups %>%
    dplyr::filter(group %in% c("treatment", "control")) %>%
    dplyr::left_join(prior_ppg_tbl, by = c("player_id", "season"))

  parallel_trends <- groups_ppg %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      n           = dplyr::n(),
      mean_ppg    = mean(ppg, na.rm = TRUE),
      sd_ppg      = stats::sd(ppg, na.rm = TRUE),
      median_ppg  = stats::median(ppg, na.rm = TRUE),
      .groups     = "drop"
    )

  trt_mean <- parallel_trends$mean_ppg[parallel_trends$group == "treatment"]
  ctl_mean <- parallel_trends$mean_ppg[parallel_trends$group == "control"]
  ppg_gap  <- if (length(trt_mean) > 0L && length(ctl_mean) > 0L) {
    abs(trt_mean - ctl_mean)
  } else NA_real_

  check_d <- if (is.na(ppg_gap)) FALSE else ppg_gap <= 2.0

  # --- Check E: Position composition stability --------------------------------
  # Position mix within each group should not shift dramatically between eras.
  # We check that no position group goes from < 5% to > 40% (or vice versa)
  # between eras within the same experiment group.

  position_stability <- groups %>%
    dplyr::filter(group %in% c("treatment", "control")) %>%
    dplyr::count(group, era, position) %>%
    dplyr::group_by(group, era) %>%
    dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
    dplyr::ungroup()

  pos_range <- position_stability %>%
    dplyr::group_by(group, position) %>%
    dplyr::summarise(pct_range = max(pct) - min(pct), .groups = "drop")

  check_e <- all(pos_range$pct_range <= 25, na.rm = TRUE)

  # --- Assemble report --------------------------------------------------------

  checks_passed <- c(
    absence_rate_plausible   = check_a,
    return_timing_distributed = check_b,
    control_consistency       = check_c,
    parallel_trends_proxy     = check_d,
    position_stability        = check_e
  )

  n_passed <- sum(checks_passed)
  n_total  <- length(checks_passed)

  report_lines <- c(
    glue("Assumption validation: {n_passed}/{n_total} checks passed"),
    glue("  A. Absence rate plausibility   : {if(check_a) 'PASS' else 'FAIL'}"),
    glue("  B. Return timing distribution  : {if(check_b) 'PASS' else 'FAIL'} ",
         "(max single-week concentration: {round(max_return_pct, 1)}%)"),
    glue("  C. Control group consistency   : {if(check_c) 'PASS' else 'FAIL'}"),
    glue("  D. Parallel trends proxy       : {if(check_d) 'PASS' else 'FAIL'} ",
         "(prior PPG gap: {round(ppg_gap, 2)})"),
    glue("  E. Position stability          : {if(check_e) 'PASS' else 'FAIL'}")
  )

  report <- paste(report_lines, collapse = "\n")
  if (verbose) message(report)

  list(
    absence_rate         = absence_rate,
    return_timing        = return_timing,
    control_consistency  = control_consistency,
    parallel_trends      = parallel_trends,
    position_stability   = position_stability,
    checks_passed        = checks_passed,
    report               = report
  )
}


# ==============================================================================
# FUNCTION: design_power_analysis_injury
# ==============================================================================

#' Power Analysis for the Injury Proximity Experiment
#'
#' Computes statistical power given observed group sizes and a target effect
#' size. Uses \code{stats::power.t.test()} for an independent two-sample
#' t-test (conservative; bootstrap is used for inference but power is
#' summarized in the t-test framework for interpretability).
#'
#' Reports detectable effect sizes at 80% and 90% power for the observed group
#' sizes, in addition to power at a user-specified target effect size.
#'
#' @param groups Tibble. Output of
#'   \code{\link{classify_treatment_control_injury}}.
#' @param effect_size Numeric. Cohen's d for the primary power calculation.
#'   Default 0.20 (small-to-medium; roughly 1 PPG difference on a 7-PPG mean
#'   with pooled SD around 5).
#' @param conf_level Numeric. 1 - alpha. Default 0.95.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{power_at_target}{Numeric. Power at \code{effect_size}.}
#'     \item{detectable_at_80}{Numeric. Minimum detectable Cohen's d at 80\%
#'       power.}
#'     \item{detectable_at_90}{Numeric. Minimum detectable Cohen's d at 90\%
#'       power.}
#'     \item{power_table}{Tibble. Power across a range of effect sizes (0.10
#'       to 0.50 in steps of 0.05).}
#'     \item{group_sizes}{Named integer. n_treatment and n_control.}
#'     \item{summary}{chr. Human-readable power summary.}
#'   }
#'
#' @seealso \code{\link{classify_treatment_control_injury}},
#'   \code{\link{create_injury_experiment_specification}}
#' @export
design_power_analysis_injury <- function(
    groups,
    effect_size = 0.20,
    conf_level  = 0.95,
    verbose     = TRUE
) {

  stopifnot(is.data.frame(groups), is.numeric(effect_size), is.numeric(conf_level))

  n_trt <- sum(groups$group == "treatment", na.rm = TRUE)
  n_ctl <- sum(groups$group == "control",   na.rm = TRUE)

  if (n_trt == 0L || n_ctl == 0L) {
    stop("design_power_analysis_injury(): treatment or control group is empty.",
         call. = FALSE)
  }

  alpha   <- 1 - conf_level
  n_harmonic <- 2 / (1/n_trt + 1/n_ctl)    # harmonic mean sample size

  # Power at target effect size
  pw_target <- tryCatch(
    stats::power.t.test(
      n    = n_harmonic,
      delta = effect_size,
      sd   = 1,            # standardized (delta = Cohen's d when sd = 1)
      sig.level = alpha,
      type      = "two.sample",
      alternative = "two.sided"
    )$power,
    error = function(e) NA_real_
  )

  # Minimum detectable d at 80% and 90% power
  mde <- function(target_power) {
    tryCatch(
      stats::power.t.test(
        n    = n_harmonic,
        power    = target_power,
        sd   = 1,
        sig.level = alpha,
        type      = "two.sample",
        alternative = "two.sided"
      )$delta,
      error = function(e) NA_real_
    )
  }

  detectable_80 <- mde(0.80)
  detectable_90 <- mde(0.90)

  # Power curve across a grid of effect sizes
  effect_grid <- seq(0.10, 0.50, by = 0.05)
  power_vec   <- purrr::map_dbl(effect_grid, function(d) {
    tryCatch(
      stats::power.t.test(
        n = n_harmonic, delta = d, sd = 1,
        sig.level = alpha, type = "two.sample", alternative = "two.sided"
      )$power,
      error = function(e) NA_real_
    )
  })

  power_table <- dplyr::tibble(
    cohens_d = effect_grid,
    power    = round(power_vec, 3)
  )

  summary_str <- glue(
    "Power analysis (two-sample t-test, alpha = {alpha}):\n",
    "  Treatment N = {n_trt}; Control N = {n_ctl}; ",
    "Harmonic mean N = {round(n_harmonic, 1)}\n",
    "  Power at d = {effect_size}: {round(pw_target, 3)}\n",
    "  Min detectable d at 80% power: {round(detectable_80, 3)}\n",
    "  Min detectable d at 90% power: {round(detectable_90, 3)}"
  )

  if (verbose) message(summary_str)

  list(
    power_at_target   = pw_target,
    detectable_at_80  = detectable_80,
    detectable_at_90  = detectable_90,
    power_table       = power_table,
    group_sizes       = c(n_treatment = n_trt, n_control = n_ctl),
    summary           = as.character(summary_str)
  )
}


# ==============================================================================
# FUNCTION: create_injury_experiment_specification
# ==============================================================================

#' Generate a Formal Specification Document for the Injury Proximity Experiment
#'
#' Assembles a human-readable specification document summarizing the experiment
#' design, group sizes, balance diagnostics, and power. Returns a character
#' string suitable for writing to disk or embedding in a report.
#'
#' @param groups Tibble. Output of
#'   \code{\link{classify_treatment_control_injury}}.
#' @param balance List. Output of \code{\link{check_balance_injury_groups}}.
#' @param power_results List. Output of
#'   \code{\link{design_power_analysis_injury}}.
#' @param training_weeks Integer vector. Default \code{TRAINING_WEEKS_W11}.
#' @param treatment_return_max Integer. Default \code{TREATMENT_RETURN_MAX_W11}.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W11}.
#'
#' @return Character scalar. Formatted specification document.
#'
#' @seealso \code{\link{classify_treatment_control_injury}},
#'   \code{\link{design_power_analysis_injury}}
#' @export
create_injury_experiment_specification <- function(
    groups,
    balance,
    power_results,
    training_weeks       = TRAINING_WEEKS_W11,
    treatment_return_max = TREATMENT_RETURN_MAX_W11,
    outcome_weeks        = OUTCOME_WEEKS_W11
) {

  stopifnot(
    is.data.frame(groups), is.list(balance), is.list(power_results)
  )

  n_trt      <- sum(groups$group == "treatment", na.rm = TRUE)
  n_ctl      <- sum(groups$group == "control",   na.rm = TRUE)
  n_seasons  <- dplyr::n_distinct(groups$season)
  min_season <- min(groups$season, na.rm = TRUE)
  max_season <- max(groups$season, na.rm = TRUE)

  smd_ppg_row  <- balance$smd_table[balance$smd_table$covariate == "prior_season_ppg", ]
  smd_ppg_val  <- if (nrow(smd_ppg_row) > 0L) round(smd_ppg_row$smd[[1L]], 3) else NA_real_
  n_flagged    <- sum(balance$smd_table$flagged, na.rm = TRUE)

  pw_target <- round(power_results$power_at_target, 3)
  mde_80    <- round(power_results$detectable_at_80, 3)

  spec <- glue(
    "============================================================\n",
    "INJURY PROXIMITY EXPERIMENT SPECIFICATION\n",
    "NFL Analytics Toolkit -- Season 2, Week 11\n",
    "Schema tag: {SCHEMA_TAG_W11}\n",
    "============================================================\n\n",

    "RESEARCH QUESTION\n",
    "-----------------\n",
    "How long after returning from injury does a player's productivity\n",
    "stabilize? Does early-season injury recovery affect the accuracy\n",
    "of prior-season stats as a projection baseline for Weeks 9-18?\n\n",

    "DESIGN\n",
    "------\n",
    "Type            : Natural experiment (observational; no randomization)\n",
    "Data source     : nflfastR PBP via load_normalized_season() (R/15)\n",
    "Scoring         : PPR via calculate_fantasy_points_ext() (R/17)\n",
    "Analysis seasons: {min_season}-{max_season} ({n_seasons} seasons)\n",
    "Positions       : QB, RB, WR, TE\n\n",

    "TRAINING WINDOW : Weeks {min(training_weeks)}-{max(training_weeks)}\n",
    "  Absence classification and group assignment confined to this window.\n\n",

    "OUTCOME WINDOW  : Weeks {min(outcome_weeks)}-{max(outcome_weeks)}\n",
    "  Fantasy PPR points in this window are the primary outcome.\n\n",

    "INJURY INFERENCE\n",
    "  Absence inferred from zero offensive plays in a week where:\n",
    "    (a) The player's team played (not a bye), AND\n",
    "    (b) The player had >= {MIN_PRIOR_GAMES_W11} prior training-week appearances.\n",
    "  Limitation: inferred absence may reflect benching or roster deactivation\n",
    "  for non-injury reasons.\n\n",

    "GROUP DEFINITIONS\n",
    "-----------------\n",
    "Treatment (N = {n_trt}):\n",
    "  Players whose first return-from-absence week is <= Week {treatment_return_max}.\n",
    "  Require >= {MIN_TRAINING_GAMES_W11} training-window games post-return.\n\n",

    "Control (N = {n_ctl}):\n",
    "  Players with zero inferred absences across all training weeks.\n",
    "  Require >= {MIN_TRAINING_GAMES_W11} total training-window appearances.\n\n",

    "PRIMARY OUTCOMES\n",
    "----------------\n",
    "1. Mean absolute prediction error (MAE): |outcome_ppg - prior_season_ppg|\n",
    "   Primary test: is MAE higher in treatment than control?\n",
    "2. Mean outcome PPG (Weeks 9-18): does treatment show lower fantasy output\n",
    "   after controlling for prior-season production?\n",
    "3. Within-treatment trajectory: how many weeks post-return until performance\n",
    "   is statistically indistinguishable from the control mean?\n\n",

    "PREDICTION PROXY\n",
    "----------------\n",
    "Prior-season weekly PPG (all weeks of season-1). Requires >= ",
    "{MIN_PRIOR_SEASON_GAMES_W11} prior-season\n",
    "games. Players without a qualifying prior season are excluded.\n\n",

    "BALANCE DIAGNOSTICS\n",
    "-------------------\n",
    "{n_flagged} of {nrow(balance$smd_table)} covariates flagged (abs(SMD) > 0.20).\n",
    "Prior-season PPG SMD = {smd_ppg_val}.\n",
    "{balance$balance_summary}\n\n",

    "POWER ANALYSIS\n",
    "--------------\n",
    "N treatment = {n_trt}; N control = {n_ctl}\n",
    "Power at d = 0.20: {pw_target}\n",
    "Min detectable d at 80% power: {mde_80}\n\n",

    "INFERENCE METHOD\n",
    "----------------\n",
    "Primary CI: basic bootstrap ({BOOTSTRAP_B_W11} resamples, alpha = 0.05).\n",
    "Falls back to Welch t-interval when min(n) < 10.\n",
    "Welch two-sample t-test for p-values.\n",
    "Effect size: Cohen's d (pooled SD denominator).\n\n",

    "HETEROGENEOUS EFFECTS\n",
    "---------------------\n",
    "Analyzed by: position (QB / RB / WR / TE), time post-return (Weeks 1-2,\n",
    "3-4, 5+), and era (Early <{ERA_BREAKPOINT_W11} / Modern >={ERA_BREAKPOINT_W11}).\n\n",

    "ROBUSTNESS CHECKS\n",
    "-----------------\n",
    "1. Return window: treatment_return_max swept over 3, 4, 5.\n",
    "2. Min prior games: swept over 1, 2, 3.\n",
    "3. Outcome metric: outcome PPG vs outcome MAE vs outcome percentile rank.\n",
    "============================================================\n"
  )

  as.character(spec)
}


# ==============================================================================
# FUNCTION: calculate_injury_effect
# ==============================================================================

#' Estimate the Effect of Early Injury Return on Fantasy Projection Accuracy
#'
#' Primary analysis for the injury proximity experiment. Compares treatment
#' and control groups on two outcome measures in the Weeks 9-18 window:
#' \enumerate{
#'   \item \strong{MAE} (mean absolute prediction error):
#'     \code{abs(outcome_ppg - prior_season_ppg)}. Tests whether returned
#'     players are harder to predict.
#'   \item \strong{Outcome PPG}: mean weekly fantasy points in Weeks 9-18.
#'     Tests whether returned players produce less, net of baseline talent.
#' }
#'
#' Within-treatment trajectory analysis estimates when performance stabilizes
#' by modeling outcome PPG as a function of weeks post-return.
#'
#' @param weekly_fantasy Tibble. Player-week PPR scores across ALL analysis
#'   seasons (including the season-1 prior-year data). Columns: player_id,
#'   season, week, total_fantasy_points, position.
#' @param groups Tibble. Output of
#'   \code{\link{classify_treatment_control_injury}}.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W11}.
#' @param B Integer. Bootstrap resamples. Default \code{BOOTSTRAP_B_W11}.
#' @param conf_level Numeric. Default 0.95.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{group_summary}{Tibble. Per-group summary: n, mean_prior_ppg,
#'       mean_outcome_ppg, mean_mae, sd_mae.}
#'     \item{effect_mae}{List. Bootstrap result for MAE difference
#'       (treatment - control): estimate, ci_lower, ci_upper, n_a, n_b, method.}
#'     \item{effect_ppg}{List. Bootstrap result for outcome PPG difference.}
#'     \item{cohens_d_mae}{Numeric. Cohen's d for the MAE comparison.}
#'     \item{t_test_mae}{htest. Welch t-test result for MAE comparison.}
#'     \item{t_test_ppg}{htest. Welch t-test result for outcome PPG comparison.}
#'     \item{trajectory}{Tibble. Within-treatment weekly mean PPG in
#'       outcome_weeks. Columns: week, n_players, mean_ppg, sd_ppg.}
#'     \item{player_level}{Tibble. Player-season-level prediction errors for
#'       downstream diagnostics.}
#'   }
#'
#' @seealso \code{\link{validate_injury_assumptions}},
#'   \code{\link{analyze_injury_heterogeneous_effects}}
#' @export
calculate_injury_effect <- function(
    weekly_fantasy,
    groups,
    outcome_weeks = OUTCOME_WEEKS_W11,
    B             = BOOTSTRAP_B_W11,
    conf_level    = 0.95,
    verbose       = TRUE
) {

  stopifnot(
    is.data.frame(weekly_fantasy), is.data.frame(groups),
    is.numeric(outcome_weeks), is.numeric(conf_level)
  )

  required_fantasy <- c("player_id", "season", "week", "total_fantasy_points")
  required_groups  <- c("player_id", "season", "group", "return_week")
  miss_f <- setdiff(required_fantasy, names(weekly_fantasy))
  miss_g <- setdiff(required_groups, names(groups))
  if (length(miss_f) > 0L)
    stop(glue("weekly_fantasy missing: {paste(miss_f, collapse=', ')}"), call. = FALSE)
  if (length(miss_g) > 0L)
    stop(glue("groups missing: {paste(miss_g, collapse=', ')}"), call. = FALSE)

  trt_ctrl_ids <- groups %>%
    dplyr::filter(group %in% c("treatment", "control"))

  # --- Prior-season PPG -------------------------------------------------------
  # Compute mean PPG across ALL weeks of season-1 (the projection proxy).
  # Requires >= MIN_PRIOR_SEASON_GAMES_W11 games in the prior season.

  prior_ppg <- weekly_fantasy %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      prior_ppg           = mean(total_fantasy_points, na.rm = TRUE),
      n_prior_season_games = dplyr::n(),
      .groups              = "drop"
    ) %>%
    dplyr::filter(n_prior_season_games >= MIN_PRIOR_SEASON_GAMES_W11) %>%
    dplyr::mutate(season = season + 1L) %>%    # shift to match current season
    dplyr::select(player_id, season, prior_ppg, n_prior_season_games)

  # --- Outcome PPG in weeks 9-18 ----------------------------------------------

  outcome_ppg <- weekly_fantasy %>%
    dplyr::filter(week %in% outcome_weeks) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      outcome_ppg    = mean(total_fantasy_points, na.rm = TRUE),
      n_outcome_games = dplyr::n(),
      .groups         = "drop"
    ) %>%
    dplyr::filter(n_outcome_games >= MIN_OUTCOME_GAMES_W11)

  # --- Player-level prediction errors ----------------------------------------

  player_level <- trt_ctrl_ids %>%
    dplyr::select(player_id, player_name, season, group, position,
                  era, return_week) %>%
    dplyr::inner_join(prior_ppg,  by = c("player_id", "season")) %>%
    dplyr::inner_join(outcome_ppg, by = c("player_id", "season")) %>%
    dplyr::mutate(
      prediction_error = outcome_ppg - prior_ppg,
      abs_error        = abs(prediction_error)
    )

  if (nrow(player_level) == 0L) {
    stop(glue(
      "calculate_injury_effect(): no players survived the prior-season and ",
      "outcome-window filters. Check MIN_PRIOR_SEASON_GAMES_W11 = ",
      "{MIN_PRIOR_SEASON_GAMES_W11} and MIN_OUTCOME_GAMES_W11 = ",
      "{MIN_OUTCOME_GAMES_W11}."
    ), call. = FALSE)
  }

  trt_mae <- player_level %>%
    dplyr::filter(group == "treatment") %>%
    dplyr::pull(abs_error)

  ctl_mae <- player_level %>%
    dplyr::filter(group == "control") %>%
    dplyr::pull(abs_error)

  trt_ppg <- player_level %>%
    dplyr::filter(group == "treatment") %>%
    dplyr::pull(outcome_ppg)

  ctl_ppg <- player_level %>%
    dplyr::filter(group == "control") %>%
    dplyr::pull(outcome_ppg)

  # --- Bootstrap CIs and t-tests ----------------------------------------------

  effect_mae <- .bootstrap_mean_diff_ci(trt_mae, ctl_mae, B = B,
                                        conf_level = conf_level)
  effect_ppg <- .bootstrap_mean_diff_ci(trt_ppg, ctl_ppg, B = B,
                                        conf_level = conf_level)

  t_test_mae <- tryCatch(
    stats::t.test(trt_mae, ctl_mae),
    error = function(e) NULL
  )
  t_test_ppg <- tryCatch(
    stats::t.test(trt_ppg, ctl_ppg),
    error = function(e) NULL
  )

  # Cohen's d for MAE (pooled SD denominator)
  pooled_sd_mae <- sqrt(
    (stats::var(trt_mae, na.rm = TRUE) + stats::var(ctl_mae, na.rm = TRUE)) / 2
  )
  # Guard against NA: stats::var() returns NA when length(x) < 2.
  # Treatment N=1 produces pooled_sd_mae = NA. Base if() cannot evaluate NA > 0.
  cohens_d_mae <- if (!is.na(pooled_sd_mae) && pooled_sd_mae > 0) {
    (mean(trt_mae, na.rm = TRUE) - mean(ctl_mae, na.rm = TRUE)) / pooled_sd_mae
  } else NA_real_

  # --- Group-level summary ----------------------------------------------------

  group_summary <- player_level %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      n_players       = dplyr::n(),
      mean_prior_ppg  = mean(prior_ppg,    na.rm = TRUE),
      mean_outcome_ppg = mean(outcome_ppg, na.rm = TRUE),
      sd_outcome_ppg  = stats::sd(outcome_ppg, na.rm = TRUE),
      mean_mae        = mean(abs_error,    na.rm = TRUE),
      sd_mae          = stats::sd(abs_error, na.rm = TRUE),
      median_mae      = stats::median(abs_error, na.rm = TRUE),
      .groups         = "drop"
    )

  # --- Within-treatment trajectory --------------------------------------------
  # For treatment players, compute per-week mean PPG in outcome_weeks.
  # This shows whether performance trends upward (stabilizes) across the window.

  trt_player_seasons <- player_level %>%
    dplyr::filter(group == "treatment") %>%
    dplyr::select(player_id, season)

  trajectory <- weekly_fantasy %>%
    dplyr::inner_join(trt_player_seasons, by = c("player_id", "season")) %>%
    dplyr::filter(week %in% outcome_weeks) %>%
    dplyr::group_by(week) %>%
    dplyr::summarise(
      n_players = dplyr::n(),
      mean_ppg  = mean(total_fantasy_points, na.rm = TRUE),
      sd_ppg    = stats::sd(total_fantasy_points, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    dplyr::arrange(week)

  if (verbose) {
    n_trt_final <- sum(player_level$group == "treatment")
    n_ctl_final <- sum(player_level$group == "control")
    mae_diff    <- round(effect_mae$estimate, 2)
    ppg_diff    <- round(effect_ppg$estimate, 2)
    message(glue(
      "calculate_injury_effect(): {n_trt_final} treatment, ",
      "{n_ctl_final} control player-seasons after filtering.\n",
      "  MAE difference (trt - ctl): {mae_diff} PPG ",
      "[{round(effect_mae$ci_lower,2)}, {round(effect_mae$ci_upper,2)}]\n",
      "  Outcome PPG difference: {ppg_diff} PPG ",
      "[{round(effect_ppg$ci_lower,2)}, {round(effect_ppg$ci_upper,2)}]"
    ))
  }

  list(
    group_summary   = group_summary,
    effect_mae      = effect_mae,
    effect_ppg      = effect_ppg,
    cohens_d_mae    = cohens_d_mae,
    t_test_mae      = t_test_mae,
    t_test_ppg      = t_test_ppg,
    trajectory      = trajectory,
    player_level    = player_level
  )
}


# ==============================================================================
# FUNCTION: analyze_injury_heterogeneous_effects
# ==============================================================================

#' Analyze Heterogeneous Effects of Injury Return by Position, Timing, and Era
#'
#' Runs the primary effect analysis (\code{\link{calculate_injury_effect}})
#' within three stratification dimensions:
#' \enumerate{
#'   \item \strong{Position}: QB, RB, WR, TE separately.
#'   \item \strong{Time post-return}: categories based on return_week (early
#'     return Weeks 1-2 vs. mid return Weeks 3-4 vs. late return Weeks 5-8
#'     -- all within the treatment group).
#'   \item \strong{Era}: "Early" (<\code{ERA_BREAKPOINT_W11}) vs.
#'     "Modern" (>=\code{ERA_BREAKPOINT_W11}).
#' }
#'
#' @param weekly_fantasy Tibble. Same object passed to
#'   \code{calculate_injury_effect}.
#' @param groups Tibble. Output of
#'   \code{\link{classify_treatment_control_injury}}.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W11}.
#' @param min_group_n Integer. Minimum treatment AND control N required to run
#'   the effect analysis for a stratum. Strata below this threshold are
#'   reported as NA. Default 10L.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{by_position}{Tibble. One row per position: n_treatment,
#'       n_control, mae_diff, mae_ci_lower, mae_ci_upper, ppg_diff,
#'       cohens_d.}
#'     \item{by_return_timing}{Tibble. One row per return-week category
#'       within the treatment group vs. full control.}
#'     \item{by_era}{Tibble. One row per era.}
#'     \item{narrative}{chr. Key heterogeneous effect findings.}
#'   }
#'
#' @seealso \code{\link{calculate_injury_effect}},
#'   \code{\link{injury_robustness_check}}
#' @export
analyze_injury_heterogeneous_effects <- function(
    weekly_fantasy,
    groups,
    outcome_weeks = OUTCOME_WEEKS_W11,
    min_group_n   = 10L,
    verbose       = TRUE
) {

  stopifnot(
    is.data.frame(weekly_fantasy), is.data.frame(groups),
    is.numeric(outcome_weeks)
  )

  # Helper: run calculate_injury_effect on a subgroups object and extract
  # the key summary row. Returns NA row if either group is too small.
  .stratum_effect <- function(sub_groups, stratum_label) {

    n_trt <- sum(sub_groups$group == "treatment", na.rm = TRUE)
    n_ctl <- sum(sub_groups$group == "control",   na.rm = TRUE)

    if (n_trt < min_group_n || n_ctl < min_group_n) {
      return(dplyr::tibble(
        stratum      = stratum_label,
        n_treatment  = n_trt,
        n_control    = n_ctl,
        mae_diff     = NA_real_,
        mae_ci_lower = NA_real_,
        mae_ci_upper = NA_real_,
        ppg_diff     = NA_real_,
        cohens_d     = NA_real_,
        note         = glue("Insufficient N (min {min_group_n} per group)")
      ))
    }

    result <- tryCatch(
      calculate_injury_effect(
        weekly_fantasy = weekly_fantasy,
        groups         = sub_groups,
        outcome_weeks  = outcome_weeks,
        B              = 500L,          # fewer resamples for strata speed
        verbose        = FALSE
      ),
      error = function(e) NULL
    )

    if (is.null(result)) {
      return(dplyr::tibble(
        stratum = stratum_label, n_treatment = n_trt, n_control = n_ctl,
        mae_diff = NA_real_, mae_ci_lower = NA_real_, mae_ci_upper = NA_real_,
        ppg_diff = NA_real_, cohens_d = NA_real_, note = "calculation_error"
      ))
    }

    dplyr::tibble(
      stratum      = stratum_label,
      n_treatment  = result$effect_mae$n_a,
      n_control    = result$effect_mae$n_b,
      mae_diff     = result$effect_mae$estimate,
      mae_ci_lower = result$effect_mae$ci_lower,
      mae_ci_upper = result$effect_mae$ci_upper,
      ppg_diff     = result$effect_ppg$estimate,
      cohens_d     = result$cohens_d_mae,
      note         = NA_character_
    )
  }

  # --- By position ------------------------------------------------------------

  if (verbose) message("analyze_injury_heterogeneous_effects(): by position...")

  positions <- SKILL_POSITIONS_W11
  by_position <- purrr::map_dfr(positions, function(pos) {
    sub <- groups %>% dplyr::filter(position == pos)
    .stratum_effect(sub, pos)
  })

  # --- By return timing (treatment only vs full control) ----------------------

  if (verbose) message("analyze_injury_heterogeneous_effects(): by return timing...")

  timing_labels <- c("Weeks 1-2", "Weeks 3-4")
  timing_ranges <- list(1L:2L, 3L:4L)

  full_control <- groups %>% dplyr::filter(group == "control")

  by_return_timing <- purrr::map2_dfr(
    timing_ranges, timing_labels,
    function(rng, lbl) {
      sub_trt <- groups %>%
        dplyr::filter(group == "treatment", return_week %in% rng)
      sub <- dplyr::bind_rows(sub_trt, full_control)
      .stratum_effect(sub, lbl)
    }
  )

  # --- By era -----------------------------------------------------------------

  if (verbose) message("analyze_injury_heterogeneous_effects(): by era...")

  eras <- c("Early", "Modern")
  by_era <- purrr::map_dfr(eras, function(e) {
    sub <- groups %>% dplyr::filter(era == e)
    .stratum_effect(sub, e)
  })

  # --- Narrative --------------------------------------------------------------

  find_max_stratum <- function(tbl) {
    if (all(is.na(tbl$mae_diff))) return("insufficient data")
    tbl$stratum[which.max(abs(tbl$mae_diff))]
  }

  pos_max  <- find_max_stratum(by_position)
  era_diff <- tryCatch({
    early_d  <- by_era$mae_diff[by_era$stratum == "Early"]
    modern_d <- by_era$mae_diff[by_era$stratum == "Modern"]
    if (!is.na(early_d) && !is.na(modern_d)) round(modern_d - early_d, 2)
    else NA_real_
  }, error = function(e) NA_real_)

  narrative <- glue(
    "Heterogeneous effects summary:\n",
    "  By position: largest MAE difference at '{pos_max}'.\n",
    "  By era: Modern vs Early MAE-diff change = {era_diff} PPG ",
    "(positive = larger effect in Modern era).\n",
    "  By return timing: see by_return_timing table."
  )

  if (verbose) message(narrative)

  list(
    by_position      = by_position,
    by_return_timing = by_return_timing,
    by_era           = by_era,
    narrative        = as.character(narrative)
  )
}


# ==============================================================================
# FUNCTION: injury_robustness_check
# ==============================================================================

#' Sensitivity Analysis: Test Robustness to Experiment Design Choices
#'
#' Re-runs the classification and primary effect analysis under alternative
#' specifications:
#' \enumerate{
#'   \item \strong{Return window}: treatment_return_max in {3, 4, 5} -- tests
#'     sensitivity to how "early" an early return must be.
#'   \item \strong{Min prior games}: swept over {1, 2, 3} -- tests sensitivity
#'     to the absence-inference threshold.
#'   \item \strong{Outcome metric}: MAE vs raw outcome PPG vs outcome PPG
#'     percentile rank within position-season.
#' }
#'
#' @param participation_records Tibble. Same object from pipeline.
#' @param team_schedule Tibble. Same object from pipeline.
#' @param roster_positions Tibble. Same position lookup.
#' @param weekly_fantasy Tibble. Same object from pipeline.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W11}.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{results_table}{Tibble. One row per specification combination.
#'       Columns: treatment_return_max, min_prior_games, n_treatment,
#'       n_control, mae_diff, mae_ci_lower, mae_ci_upper, ppg_diff.}
#'     \item{outcome_metric_comparison}{Tibble. MAE vs raw PPG vs percentile
#'       rank comparison at the base specification.}
#'     \item{summary}{chr. Key robustness finding.}
#'   }
#'
#' @seealso \code{\link{calculate_injury_effect}},
#'   \code{\link{analyze_injury_heterogeneous_effects}}
#' @export
injury_robustness_check <- function(
    participation_records,
    team_schedule,
    roster_positions,
    weekly_fantasy,
    outcome_weeks = OUTCOME_WEEKS_W11,
    verbose       = TRUE
) {

  stopifnot(
    is.data.frame(participation_records),
    is.data.frame(team_schedule),
    is.data.frame(roster_positions),
    is.data.frame(weekly_fantasy)
  )

  if (verbose) message("injury_robustness_check(): sweeping return window and prior-game thresholds...")

  return_max_grid  <- c(3L, 4L, 5L)
  prior_games_grid <- c(1L, 2L, 3L)

  spec_grid <- expand.grid(
    treatment_return_max = return_max_grid,
    min_prior_games      = prior_games_grid,
    stringsAsFactors     = FALSE
  )

  results_rows <- purrr::map_dfr(
    seq_len(nrow(spec_grid)),
    function(i) {
      rmax <- as.integer(spec_grid$treatment_return_max[[i]])
      mpg  <- as.integer(spec_grid$min_prior_games[[i]])

      if (verbose) {
        message(glue(
          "  return_max={rmax}, min_prior_games={mpg}"
        ))
      }

      returners <- tryCatch(
        identify_returning_players(
          participation_records = participation_records,
          team_schedule         = team_schedule,
          roster_positions      = roster_positions,
          seasons               = ANALYSIS_SEASONS_W11,
          training_weeks        = TRAINING_WEEKS_W11,
          min_prior_games       = mpg,
          verbose               = FALSE
        ),
        error = function(e) NULL
      )

      if (is.null(returners) || nrow(returners) == 0L) {
        return(dplyr::tibble(
          treatment_return_max = rmax, min_prior_games = mpg,
          n_treatment = 0L, n_control = NA_integer_,
          mae_diff = NA_real_, mae_ci_lower = NA_real_,
          mae_ci_upper = NA_real_, ppg_diff = NA_real_
        ))
      }

      groups_spec <- tryCatch(
        classify_treatment_control_injury(
          returning_players    = returners,
          participation_records = participation_records,
          team_schedule        = team_schedule,
          roster_positions     = roster_positions,
          treatment_return_max = rmax,
          verbose              = FALSE
        ),
        error = function(e) NULL
      )

      if (is.null(groups_spec)) {
        return(dplyr::tibble(
          treatment_return_max = rmax, min_prior_games = mpg,
          n_treatment = NA_integer_, n_control = NA_integer_,
          mae_diff = NA_real_, mae_ci_lower = NA_real_,
          mae_ci_upper = NA_real_, ppg_diff = NA_real_
        ))
      }

      n_trt <- sum(groups_spec$group == "treatment", na.rm = TRUE)
      n_ctl <- sum(groups_spec$group == "control",   na.rm = TRUE)

      if (n_trt < 5L || n_ctl < 5L) {
        return(dplyr::tibble(
          treatment_return_max = rmax, min_prior_games = mpg,
          n_treatment = n_trt, n_control = n_ctl,
          mae_diff = NA_real_, mae_ci_lower = NA_real_,
          mae_ci_upper = NA_real_, ppg_diff = NA_real_
        ))
      }

      effect_spec <- tryCatch(
        calculate_injury_effect(
          weekly_fantasy = weekly_fantasy,
          groups         = groups_spec,
          outcome_weeks  = outcome_weeks,
          B              = 500L,
          verbose        = FALSE
        ),
        error = function(e) NULL
      )

      if (is.null(effect_spec)) {
        return(dplyr::tibble(
          treatment_return_max = rmax, min_prior_games = mpg,
          n_treatment = n_trt, n_control = n_ctl,
          mae_diff = NA_real_, mae_ci_lower = NA_real_,
          mae_ci_upper = NA_real_, ppg_diff = NA_real_
        ))
      }

      dplyr::tibble(
        treatment_return_max = rmax,
        min_prior_games      = mpg,
        n_treatment          = effect_spec$effect_mae$n_a,
        n_control            = effect_spec$effect_mae$n_b,
        mae_diff             = effect_spec$effect_mae$estimate,
        mae_ci_lower         = effect_spec$effect_mae$ci_lower,
        mae_ci_upper         = effect_spec$effect_mae$ci_upper,
        ppg_diff             = effect_spec$effect_ppg$estimate
      )
    }
  )

  # --- Outcome metric comparison at base spec ---------------------------------
  if (verbose) message("injury_robustness_check(): outcome metric comparison...")

  base_returners <- tryCatch(
    identify_returning_players(
      participation_records = participation_records,
      team_schedule         = team_schedule,
      roster_positions      = roster_positions,
      verbose               = FALSE
    ),
    error = function(e) NULL
  )

  outcome_metric_comparison <- dplyr::tibble(
    metric  = character(),
    mae_diff = numeric(),
    note     = character()
  )

  if (!is.null(base_returners) && nrow(base_returners) > 0L) {
    base_groups <- tryCatch(
      classify_treatment_control_injury(
        returning_players    = base_returners,
        participation_records = participation_records,
        team_schedule        = team_schedule,
        roster_positions     = roster_positions,
        verbose              = FALSE
      ),
      error = function(e) NULL
    )

    if (!is.null(base_groups)) {
      base_effect <- tryCatch(
        calculate_injury_effect(
          weekly_fantasy = weekly_fantasy,
          groups         = base_groups,
          outcome_weeks  = outcome_weeks,
          B              = 500L,
          verbose        = FALSE
        ),
        error = function(e) NULL
      )

      if (!is.null(base_effect)) {
        # Percentile rank outcome: rank within position-season in outcome window
        outcome_pctl <- weekly_fantasy %>%
          dplyr::filter(week %in% outcome_weeks) %>%
          dplyr::group_by(player_id, season) %>%
          dplyr::summarise(
            outcome_ppg     = mean(total_fantasy_points, na.rm = TRUE),
            n_outcome_games = dplyr::n(),
            .groups         = "drop"
          ) %>%
          dplyr::inner_join(
            weekly_fantasy %>%
              dplyr::filter(week %in% outcome_weeks) %>%
              dplyr::select(player_id, season, position) %>%
              dplyr::distinct(),
            by = c("player_id", "season")
          ) %>%
          dplyr::group_by(season, position) %>%
          dplyr::mutate(
            outcome_pctl = dplyr::percent_rank(outcome_ppg)
          ) %>%
          dplyr::ungroup()

        pctl_trt <- base_effect$player_level %>%
          dplyr::filter(group == "treatment") %>%
          dplyr::left_join(
            outcome_pctl %>% dplyr::select(player_id, season, outcome_pctl),
            by = c("player_id", "season")
          ) %>%
          dplyr::pull(outcome_pctl)

        pctl_ctl <- base_effect$player_level %>%
          dplyr::filter(group == "control") %>%
          dplyr::left_join(
            outcome_pctl %>% dplyr::select(player_id, season, outcome_pctl),
            by = c("player_id", "season")
          ) %>%
          dplyr::pull(outcome_pctl)

        pctl_diff <- mean(pctl_trt, na.rm = TRUE) - mean(pctl_ctl, na.rm = TRUE)

        outcome_metric_comparison <- dplyr::tibble(
          metric   = c("MAE (abs prediction error)",
                       "Raw outcome PPG",
                       "Outcome percentile rank (within position-season)"),
          estimate = c(base_effect$effect_mae$estimate,
                       base_effect$effect_ppg$estimate,
                       pctl_diff),
          note     = c("Primary outcome", "Secondary outcome",
                       "Rank-based alternative")
        )
      }
    }
  }

  # --- Robustness summary -----------------------------------------------------

  valid_rows <- results_rows %>% dplyr::filter(!is.na(mae_diff))
  summary_str <- if (nrow(valid_rows) == 0L) {
    "No valid specifications produced estimable effects."
  } else {
    direction_consistent <- all(
      sign(valid_rows$mae_diff) == sign(valid_rows$mae_diff[[1L]]),
      na.rm = TRUE
    )
    glue(
      "Robustness check: {nrow(valid_rows)}/{nrow(results_rows)} specs ",
      "produced estimable effects. Direction consistent across specs: ",
      "{direction_consistent}. MAE difference range: ",
      "[{round(min(valid_rows$mae_diff, na.rm=TRUE), 2)}, ",
      "{round(max(valid_rows$mae_diff, na.rm=TRUE), 2)}]."
    )
  }

  if (verbose) message(summary_str)

  list(
    results_table              = results_rows,
    outcome_metric_comparison  = outcome_metric_comparison,
    summary                    = as.character(summary_str)
  )
}


# ==============================================================================
# FUNCTION: run_week11_pipeline
# ==============================================================================

#' Run the Complete Week 11 Injury Proximity Experiment Pipeline
#'
#' End-to-end wrapper that loads all season data, builds the core data
#' structures, and runs all nine experiment functions in sequence. Returns
#' a named list with all intermediate and final results.
#'
#' Season-by-season loading is used throughout to constrain peak memory.
#' Each season's PBP is loaded, processed, and freed before the next is
#' loaded. gc() is called after each season to release memory.
#'
#' @param seasons Integer vector. Seasons to load for PBP and fantasy scoring.
#'   Must include both analysis seasons AND their prior years (so that the
#'   prior-season baseline can be computed). Default includes \code{SEASONS_W11}
#'   which covers 2010-2025. To compute baselines for analysis season 2011,
#'   2010 data is required. Do not narrow this range.
#' @param analysis_seasons Integer vector. Seasons to include in the experiment.
#'   Default: \code{ANALYSIS_SEASONS_W11} (2011-2025).
#' @param cache_dir Character. Cache directory for load_normalized_season().
#'   Default: \code{CACHE_DIR_W11}.
#' @param training_weeks Integer vector. Default \code{TRAINING_WEEKS_W11}.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W11}.
#' @param treatment_return_max Integer. Default \code{TREATMENT_RETURN_MAX_W11}.
#' @param save_outputs Logical. Write result RDS files to cache_dir.
#'   Default TRUE.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{participation_records}{Tibble. Player-week participation.}
#'     \item{team_schedule}{Tibble. Team-week schedule.}
#'     \item{weekly_fantasy}{Tibble. Player-week PPR scoring.}
#'     \item{roster_positions}{Tibble. Player-season position lookup.}
#'     \item{returning_players}{Tibble. Output of identify_returning_players().}
#'     \item{groups}{Tibble. Output of classify_treatment_control_injury().}
#'     \item{balance}{List. Output of check_balance_injury_groups().}
#'     \item{assumptions}{List. Output of validate_injury_assumptions().}
#'     \item{power}{List. Output of design_power_analysis_injury().}
#'     \item{spec}{chr. Output of create_injury_experiment_specification().}
#'     \item{effects}{List. Output of calculate_injury_effect().}
#'     \item{heterogeneous}{List. Output of analyze_injury_heterogeneous_effects().}
#'     \item{robustness}{List. Output of injury_robustness_check().}
#'   }
#'
#' @seealso \code{\link{identify_returning_players}},
#'   \code{\link{classify_treatment_control_injury}},
#'   \code{\link{calculate_injury_effect}}
#' @export
run_week11_pipeline <- function(
    seasons              = SEASONS_W11,
    analysis_seasons     = ANALYSIS_SEASONS_W11,
    cache_dir            = CACHE_DIR_W11,
    training_weeks       = TRAINING_WEEKS_W11,
    outcome_weeks        = OUTCOME_WEEKS_W11,
    treatment_return_max = TREATMENT_RETURN_MAX_W11,
    save_outputs         = TRUE,
    verbose              = TRUE
) {

  seasons          <- sort(as.integer(seasons))
  analysis_seasons <- sort(as.integer(analysis_seasons))

  sep <- strrep("=", 60)
  if (verbose) {
    message(glue(
      "\n{sep}\n",
      "Week 11 Pipeline: Injury Proximity Experiment\n",
      "Seasons: {min(seasons)}-{max(seasons)} (PBP load)\n",
      "Analysis seasons: {min(analysis_seasons)}-{max(analysis_seasons)}\n",
      "Training window: Weeks {min(training_weeks)}-{max(training_weeks)}\n",
      "Outcome window:  Weeks {min(outcome_weeks)}-{max(outcome_weeks)}\n",
      "Treatment return max: Week {treatment_return_max}\n",
      "{sep}"
    ))
  }

  # --------------------------------------------------------------------------
  # Step 1: Load rosters for position classification
  # --------------------------------------------------------------------------
  if (verbose) message("\n[1/8] Loading roster data for position classification...")

  roster_raw <- tryCatch(
    nflreadr::load_rosters(seasons = analysis_seasons),
    error = function(e) {
      stop(glue(
        "nflreadr::load_rosters() failed: {conditionMessage(e)}\n",
        "Check network connection and nflreadr version."
      ), call. = FALSE)
    }
  )

  # Verified required columns: gsis_id, position, season (R/16 ROSTER_COLS_REQUIRED)
  if (!all(c("gsis_id", "position", "season") %in% names(roster_raw))) {
    stop("nflreadr::load_rosters() missing required columns: gsis_id, position, season.",
         call. = FALSE)
  }

  roster_positions <- roster_raw %>%
    dplyr::filter(!is.na(gsis_id), !is.na(position)) %>%
    dplyr::select(player_id = gsis_id, season, position) %>%
    dplyr::filter(position %in% SKILL_POSITIONS_W11) %>%
    dplyr::distinct(player_id, season, position)

  if (verbose) {
    message(glue(
      "  {format(nrow(roster_positions), big.mark=',')} skill-position ",
      "player-seasons loaded."
    ))
  }

  # --------------------------------------------------------------------------
  # Step 2: Season-by-season PBP loop
  #   Builds participation_records, team_schedule, weekly_fantasy
  # --------------------------------------------------------------------------
  if (verbose) message(glue("\n[2/8] Loading PBP and scoring for {length(seasons)} seasons..."))

  participation_list <- vector("list", length(seasons))
  schedule_list      <- vector("list", length(seasons))
  fantasy_list       <- vector("list", length(seasons))

  for (i in seq_along(seasons)) {
    s <- seasons[[i]]
    if (verbose) message(glue("  Season {s} ({i}/{length(seasons)})..."))

    pbp <- tryCatch(
      load_normalized_season(s, cache_dir = cache_dir),
      error = function(e) {
        message(glue("  WARNING: Could not load season {s}: {conditionMessage(e)}"))
        NULL
      }
    )

    if (is.null(pbp)) {
      gc()
      next
    }

    # Participation records
    participation_list[[i]] <- tryCatch(
      .extract_player_participation(pbp),
      error = function(e) {
        message(glue("  WARNING: participation extraction failed for {s}: {conditionMessage(e)}"))
        NULL
      }
    )

    # Team schedule
    schedule_list[[i]] <- tryCatch(
      .build_team_schedule(pbp),
      error = function(e) NULL
    )

    # Weekly fantasy: roster_data = NULL -- R/17 infers position from play data.
    # Position filtering in the experiment logic uses roster_positions (loaded
    # in Step 1 from nflreadr), which is the authoritative position source.
    # Passing roster_data = NULL avoids a second per-season network call and
    # keeps peak memory low.
    fantasy_list[[i]] <- tryCatch(
      .compute_weekly_fantasy_season(pbp, season_val = s,
                                     roster_data = NULL),
      error = function(e) {
        message(glue("  WARNING: fantasy scoring failed for {s}: {conditionMessage(e)}"))
        NULL
      }
    )

    rm(pbp)
    gc()
  }

  # Combine season-level lists into single tibbles
  participation_records <- dplyr::bind_rows(
    purrr::compact(participation_list)
  )
  team_schedule <- dplyr::bind_rows(
    purrr::compact(schedule_list)
  )
  weekly_fantasy <- dplyr::bind_rows(
    purrr::compact(fantasy_list)
  )

  rm(participation_list, schedule_list, fantasy_list)
  gc()

  if (verbose) {
    message(glue(
      "  Built:\n",
      "    participation_records: {format(nrow(participation_records), big.mark=',')} rows\n",
      "    team_schedule: {format(nrow(team_schedule), big.mark=',')} rows\n",
      "    weekly_fantasy: {format(nrow(weekly_fantasy), big.mark=',')} rows"
    ))
  }

  # --------------------------------------------------------------------------
  # Step 3: Identify returning players
  # --------------------------------------------------------------------------
  if (verbose) message("\n[3/8] Identifying returning players...")

  returning_players <- identify_returning_players(
    participation_records = participation_records,
    team_schedule         = team_schedule,
    roster_positions      = roster_positions,
    seasons               = analysis_seasons,
    training_weeks        = training_weeks,
    verbose               = verbose
  )

  # --------------------------------------------------------------------------
  # Step 4: Classify treatment / control
  # --------------------------------------------------------------------------
  if (verbose) message("\n[4/8] Classifying treatment and control groups...")

  groups <- classify_treatment_control_injury(
    returning_players     = returning_players,
    participation_records = participation_records,
    team_schedule         = team_schedule,
    roster_positions      = roster_positions,
    treatment_return_max  = treatment_return_max,
    training_weeks        = training_weeks,
    seasons               = analysis_seasons,
    verbose               = verbose
  )

  # --------------------------------------------------------------------------
  # Step 5: Balance check + assumption validation
  # --------------------------------------------------------------------------
  if (verbose) message("\n[5/8] Checking balance and validating assumptions...")

  balance     <- check_balance_injury_groups(groups, weekly_fantasy,
                                              verbose = verbose)
  assumptions <- validate_injury_assumptions(groups, weekly_fantasy,
                                              verbose = verbose)

  # --------------------------------------------------------------------------
  # Step 6: Power analysis + experiment specification
  # --------------------------------------------------------------------------
  if (verbose) message("\n[6/8] Power analysis and experiment specification...")

  power <- design_power_analysis_injury(groups, verbose = verbose)
  spec  <- create_injury_experiment_specification(
    groups               = groups,
    balance              = balance,
    power_results        = power,
    training_weeks       = training_weeks,
    treatment_return_max = treatment_return_max,
    outcome_weeks        = outcome_weeks
  )
  if (verbose) cat(spec)

  # --------------------------------------------------------------------------
  # Step 7: Primary effect analysis + heterogeneous effects
  # --------------------------------------------------------------------------
  if (verbose) message("\n[7/8] Calculating primary effect and heterogeneous effects...")

  effects       <- calculate_injury_effect(
    weekly_fantasy = weekly_fantasy,
    groups         = groups,
    outcome_weeks  = outcome_weeks,
    verbose        = verbose
  )

  heterogeneous <- analyze_injury_heterogeneous_effects(
    weekly_fantasy = weekly_fantasy,
    groups         = groups,
    outcome_weeks  = outcome_weeks,
    verbose        = verbose
  )

  # --------------------------------------------------------------------------
  # Step 8: Robustness checks
  # --------------------------------------------------------------------------
  if (verbose) message("\n[8/8] Running robustness checks...")

  robustness <- injury_robustness_check(
    participation_records = participation_records,
    team_schedule         = team_schedule,
    roster_positions      = roster_positions,
    weekly_fantasy        = weekly_fantasy,
    outcome_weeks         = outcome_weeks,
    verbose               = verbose
  )

  # --------------------------------------------------------------------------
  # Save outputs
  # --------------------------------------------------------------------------
  if (save_outputs) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

    saveRDS(participation_records,
            file.path(cache_dir, "s2_week11_participation_records.rds"))
    saveRDS(team_schedule,
            file.path(cache_dir, "s2_week11_team_schedule.rds"))
    saveRDS(weekly_fantasy,
            file.path(cache_dir, "s2_week11_weekly_fantasy.rds"))
    saveRDS(groups,
            file.path(cache_dir, "s2_week11_groups.rds"))
    saveRDS(effects,
            file.path(cache_dir, "s2_week11_effects.rds"))
    writeLines(spec,
               file.path(cache_dir, "s2_week11_experiment_spec.txt"))

    if (verbose) {
      message(glue("\nOutputs saved to: {cache_dir}"))
    }
  }

  # --------------------------------------------------------------------------
  # KEY INSIGHTS (computed from live results)
  # --------------------------------------------------------------------------
  n_trt <- sum(groups$group == "treatment", na.rm = TRUE)
  n_ctl <- sum(groups$group == "control",   na.rm = TRUE)

  mae_diff_val  <- round(effects$effect_mae$estimate, 2)
  mae_ci_low    <- round(effects$effect_mae$ci_lower, 2)
  mae_ci_high   <- round(effects$effect_mae$ci_upper, 2)
  ppg_diff_val  <- round(effects$effect_ppg$estimate, 2)
  d_val         <- round(effects$cohens_d_mae, 3)

  mae_direction <- if (!is.na(mae_diff_val) && mae_diff_val > 0) {
    "higher (harder to predict)"
  } else if (!is.na(mae_diff_val) && mae_diff_val < 0) {
    "lower (easier to predict)"
  } else "indeterminate"

  trt_mean_ppg <- effects$group_summary %>%
    dplyr::filter(group == "treatment") %>%
    dplyr::pull(mean_outcome_ppg)
  ctl_mean_ppg <- effects$group_summary %>%
    dplyr::filter(group == "control") %>%
    dplyr::pull(mean_outcome_ppg)

  n_checks_passed <- sum(assumptions$checks_passed, na.rm = TRUE)

  trt_mean_ppg_fmt <- if (length(trt_mean_ppg) > 0L) round(trt_mean_ppg, 2) else NA_real_
  ctl_mean_ppg_fmt <- if (length(ctl_mean_ppg) > 0L) round(ctl_mean_ppg, 2) else NA_real_

  if (verbose) {
    cat(glue(
      "\n{sep}\n",
      "KEY INSIGHTS -- Week 11 Injury Proximity Experiment\n",
      "{sep}\n",
      "Treatment N = {format(n_trt, big.mark=',')} | ",
      "Control N = {format(n_ctl, big.mark=',')}\n",
      "Assumption checks passed: {n_checks_passed}/5\n\n",

      "PRIMARY EFFECT (MAE comparison):\n",
      "  Treatment MAE - Control MAE = {mae_diff_val} PPG\n",
      "  95% CI: [{mae_ci_low}, {mae_ci_high}]\n",
      "  Direction: Treatment prediction error is {mae_direction}\n",
      "  Cohen's d = {d_val}\n\n",

      "OUTCOME PPG:\n",
      "  Treatment Weeks 9-18 mean = {trt_mean_ppg_fmt} PPG\n",
      "  Control   Weeks 9-18 mean = {ctl_mean_ppg_fmt} PPG\n",
      "  Difference = {ppg_diff_val} PPG\n\n",

      "ROBUSTNESS: {robustness$summary}\n",
      "{sep}\n"
    ), "\n")
  }

  invisible(list(
    participation_records = participation_records,
    team_schedule         = team_schedule,
    weekly_fantasy        = weekly_fantasy,
    roster_positions      = roster_positions,
    returning_players     = returning_players,
    groups                = groups,
    balance               = balance,
    assumptions           = assumptions,
    power                 = power,
    spec                  = spec,
    effects               = effects,
    heterogeneous         = heterogeneous,
    robustness            = robustness
  ))
}
