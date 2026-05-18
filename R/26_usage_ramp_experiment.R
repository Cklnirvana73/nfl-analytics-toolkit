# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 12
# Usage Ramp Experiment + Rookie Subgroup Analysis
# File: R/26_usage_ramp_experiment.R
#
# Purpose: Design and execute a natural experiment testing whether increasing
#          usage in the first half of a season predicts sustained role expansion
#          and improved fantasy output in the second half. Includes a rookie
#          subgroup analysis comparing ramp effects for first-year vs veteran
#          players.
#
# Research Question:
#   Is an increasing usage trend (Weeks 1-8) a leading indicator of sustained
#   role changes? Do ramping players maintain elevated usage and higher fantasy
#   output in Weeks 9-18?
#
# Design (confirmed in handoff, 2026-05-11):
#   - Data source     : Multi-season nflfastR PBP (2010-2025) via
#                       load_normalized_season() from R/15
#   - Scoring         : Weekly PPR via calculate_fantasy_points_ext() from R/17
#   - Usage metric    : Target share for WR/TE; touch share for RB
#                       (touch = carries + receptions per team carries + receptions)
#   - Treatment def   : Usage ramp > 10% relative from Week 1-4 avg to
#                       Week 5-8 avg, with >= 3 of 4 active weeks per sub-window
#   - Control def     : Usage change within +/-10%, >= 3 of 4 active weeks
#                       per sub-window (stable role)
#   - Declining def   : Usage drop > 10% relative, same floor (retained for
#                       heterogeneous effects analysis, not primary contrast)
#   - Excluded def    : < 3 of 4 active weeks in either sub-window, or
#                       early-window average usage < MIN_USAGE_FLOOR_W12
#   - Outcome window  : Weeks 9-18 of the same season
#   - Robustness      : Method sweep (sub_window vs full_split) and threshold
#                       sweep (5%, 10%, 15%)
#   - Rookie def      : years_exp == 0 from nflreadr::load_rosters()
#   - Analysis range  : 2010-2025 (16 seasons)
#
# Navigation:
#   Line  ~100 : Libraries
#   Line  ~120 : Source guards
#   Line  ~185 : Constants
#   Line  ~260 : NSE declarations
#   Line  ~310 : Internal helpers
#                  .compute_weekly_usage_season()
#                  .bootstrap_ramp_diff_ci()
#   Line  ~510 : calculate_weekly_usage_share()
#   Line  ~650 : identify_usage_ramps()
#   Line  ~820 : classify_treatment_control_ramp()
#   Line  ~990 : check_balance_ramp_groups()
#   Line ~1140 : validate_ramp_assumptions()
#   Line ~1280 : create_ramp_experiment_specification()
#   Line ~1390 : identify_rookies()
#   Line ~1490 : stratify_by_rookie_status()
#   Line ~1580 : analyze_ramp_by_rookie_status()
#   Line ~1720 : rookie_ramp_heterogeneous_effects()
#   Line ~1840 : calculate_usage_persistence()
#   Line ~1960 : run_ramp_experiment()
#   Line ~2130 : analyze_ramp_heterogeneous_effects()
#   Line ~2280 : ramp_robustness_check()
#   Line ~2440 : create_ramp_experiment_report()
#   Line ~2520 : run_week12_pipeline()
#
# Source dependencies:
#   R/15_multi_season_pbp.R  -- load_normalized_season()
#   R/17_extended_scoring.R  -- calculate_fantasy_points_ext()
#
# Outputs (written by run_week12_pipeline()):
#   data/season2_cache/s2_week12_weekly_usage.rds
#   data/season2_cache/s2_week12_weekly_fantasy.rds
#   data/season2_cache/s2_week12_ramp_flags.rds
#   data/season2_cache/s2_week12_groups.rds
#   data/season2_cache/s2_week12_effects.rds
#   data/season2_cache/s2_week12_rookie_effects.rds
#   data/season2_cache/s2_week12_experiment_spec.txt
#
# Season 2 output prefix : s2_week12_
# Schema tag             : s2_w12_v1
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
SEASONS_W12 <- 2010L:2025L

# Analysis seasons: all seasons in range (usage ramp requires no prior-year
# baseline, unlike the injury experiment in R/25)
ANALYSIS_SEASONS_W12 <- 2010L:2025L

# Training window: Weeks 1-8 per season.
# Usage ramp detection is confined to this window.
TRAINING_WEEKS_W12 <- 1L:8L

# Sub-windows within the training window (primary ramp detection method).
# Early: Weeks 1-4 average. Late: Weeks 5-8 average.
# A ramp is detected when late_avg exceeds early_avg by > RAMP_THRESHOLD_W12.
SUBWINDOW_EARLY_W12 <- 1L:4L
SUBWINDOW_LATE_W12  <- 5L:8L

# Outcome window: Weeks 9-18 per season.
OUTCOME_WEEKS_W12 <- 9L:18L

# Minimum active weeks per sub-window (primary method).
# A player must have >= 3 of 4 sub-window weeks active to be classified as
# treatment, control, or declining. Fewer active weeks = excluded.
# Prevents injured players' flat averages from contaminating the control group.
MIN_ACTIVE_WEEKS_SUBWINDOW_W12 <- 3L

# Minimum usage floor for early sub-window average.
# Players with early-window average below this threshold are excluded.
# A player at 0% usage in weeks 1-4 is not a meaningful ramp candidate --
# they were either not rostered, injured, or inactive the entire window.
MIN_USAGE_FLOOR_W12 <- 0.01

# Ramp threshold: relative change required to classify as treatment or declining.
# Default: 10% relative increase/decrease.
# Robustness checks sweep c(0.05, 0.10, 0.15).
RAMP_THRESHOLD_W12 <- 0.10

# Minimum outcome-window games to include a player in the effect analysis.
MIN_OUTCOME_GAMES_W12 <- 3L

# Skill positions included. QB excluded: usage ramp is not a meaningful
# construct for quarterbacks in the context of fantasy target/touch share.
SKILL_POSITIONS_W12 <- c("RB", "WR", "TE")

# Era breakpoint for heterogeneous effects analysis.
# "Early" = 2010:(ERA_BREAKPOINT - 1); "Modern" = ERA_BREAKPOINT:2025.
# 2017 chosen: CBA and safety rule changes materially altered usage patterns
# and roster strategy (consistent with R/25 ERA_BREAKPOINT_W11).
ERA_BREAKPOINT_W12 <- 2017L

# Minimum team plays per week for the usage denominator quality filter.
# Guards against computing shares in games with very few offensive plays
# (e.g., severely weather-delayed or truncated games).
MIN_TEAM_PLAYS_W12 <- 15L

# Bootstrap resamples for CI estimation
BOOTSTRAP_B_W12 <- 1000L

# Cache directory (inherits from R/15 convention)
CACHE_DIR_W12 <- here::here("data", "season2_cache")

# Schema version tag
SCHEMA_TAG_W12 <- "s2_w12_v1"


# ==============================================================================
# NSE DECLARATIONS
# ==============================================================================

utils::globalVariables(c(
  # PBP columns (verified against R/15 normalize_schema output)
  "season", "week", "game_id", "play_type", "posteam",
  "sack", "qb_scramble", "complete_pass",
  "rusher_player_id", "rusher_player_name",
  "receiver_player_id", "receiver_player_name",
  # Roster columns (verified against R/16 ROSTER_COLS_REQUIRED)
  "gsis_id", "full_name", "position", "years_exp", "entry_year",
  # Derived usage columns
  "player_id", "player_name", "team",
  "usage_share", "usage_numerator", "team_usage_denominator",
  "n_team_plays", "active",
  "carries", "receptions", "targets",
  "team_carries", "team_receptions", "team_targets_pass",
  # Derived ramp columns
  "early_avg", "late_avg", "n_early_active", "n_late_active",
  "relative_change", "ramp_method",
  # Derived classification columns
  "group", "era", "is_rookie",
  "rookie_group", "vet_group",
  # Scoring columns (verified against R/17 @return documentation)
  "total_fantasy_points",
  # Analysis columns
  "outcome_ppg", "outcome_usage",
  "mean_ppg", "sd_ppg", "mean_usage", "sd_usage",
  "effect_est", "ci_lower", "ci_upper", "p_value", "cohens_d",
  "n_players", "n_outcome_games",
  "position_group",
  # Misc
  "ramp_size_quintile", "smd"
))


# ==============================================================================
# INTERNAL HELPERS (not exported; prefix with .)
# ==============================================================================

# ------------------------------------------------------------------------------
# .compute_weekly_usage_season
# ------------------------------------------------------------------------------
# Extract per-player, per-week usage records from a single season's PBP.
# Returns target share for WR/TE (targets / team targets) and touch share for
# RB ((carries + receptions) / (team carries + team receptions)).
#
# Attribution:
#   carries   : rush plays where rusher_player_id is not NA, excluding sacks
#               and QB scrambles (consistent with R/25 participation inference)
#   targets   : pass plays where receiver_player_id is not NA (complete or
#               incomplete -- any target)
#   receptions: pass plays where receiver_player_id is not NA AND
#               complete_pass == 1
#
#   Team carries    = sum of non-sack, non-scramble rush plays
#   Team receptions = sum of completed passes
#   Team targets    = sum of pass plays with any receiver target
#
# @param pbp Data frame. Single-season normalized PBP from
#   load_normalized_season(). Must contain: season, week, game_id, play_type,
#   posteam, rusher_player_id, rusher_player_name, receiver_player_id,
#   receiver_player_name, complete_pass, sack, qb_scramble.
# @param roster_positions Tibble. Position lookup. Columns: player_id (chr),
#   season (int), position (chr). From nflreadr::load_rosters().
# @param skill_positions Character vector. Default SKILL_POSITIONS_W12.
# @param min_team_plays Integer. Minimum team plays per week for quality filter.
#   Default MIN_TEAM_PLAYS_W12.
# @return Tibble with one row per (player_id, season, week). Columns:
#   player_id, player_name, season, week, team, position, usage_share,
#   usage_numerator, team_usage_denominator, n_team_plays, active.
# @noRd
.compute_weekly_usage_season <- function(pbp,
                                          roster_positions,
                                          skill_positions = SKILL_POSITIONS_W12,
                                          min_team_plays  = MIN_TEAM_PLAYS_W12) {

  required_pbp <- c(
    "season", "week", "game_id", "play_type", "posteam",
    "rusher_player_id", "rusher_player_name",
    "receiver_player_id", "receiver_player_name",
    "complete_pass", "sack", "qb_scramble"
  )
  missing_pbp <- setdiff(required_pbp, names(pbp))
  if (length(missing_pbp) > 0L) {
    stop(glue(
      ".compute_weekly_usage_season(): pbp missing columns: ",
      "{paste(missing_pbp, collapse = ', ')}"
    ), call. = FALSE)
  }

  if (nrow(pbp) == 0L) return(dplyr::tibble())

  s <- unique(pbp$season)
  if (length(s) > 1L) {
    stop(".compute_weekly_usage_season(): pbp must contain exactly one season.",
         call. = FALSE)
  }

  # --- Coerce flag columns to integer (handles NAs in older seasons) ----------
  pbp <- pbp %>%
    dplyr::mutate(
      sack         = coalesce(as.integer(sack),         0L),
      qb_scramble  = coalesce(as.integer(qb_scramble),  0L),
      complete_pass = coalesce(as.integer(complete_pass), 0L)
    )

  # --- Carry plays (non-sack, non-scramble rush plays) -----------------------
  carries_raw <- pbp %>%
    dplyr::filter(
      play_type == "run",
      sack        == 0L,
      qb_scramble == 0L,
      !is.na(rusher_player_id)
    ) %>%
    dplyr::select(
      season, week, game_id,
      player_id   = rusher_player_id,
      player_name = rusher_player_name,
      team        = posteam
    ) %>%
    dplyr::mutate(event_type = "carry")

  # --- Team carry denominator per (team, season, week) -----------------------
  team_carries_raw <- pbp %>%
    dplyr::filter(
      play_type == "run",
      sack        == 0L,
      qb_scramble == 0L
    ) %>%
    dplyr::group_by(season, week, team = posteam) %>%
    dplyr::summarise(team_carries = dplyr::n(), .groups = "drop")

  # --- Target plays (any targeted receiver, complete or incomplete) ----------
  targets_raw <- pbp %>%
    dplyr::filter(
      play_type == "pass",
      !is.na(receiver_player_id)
    ) %>%
    dplyr::select(
      season, week, game_id,
      player_id    = receiver_player_id,
      player_name  = receiver_player_name,
      team         = posteam,
      complete_pass
    )

  # --- Team target denominator per (team, season, week) ---------------------
  team_targets_raw <- pbp %>%
    dplyr::filter(
      play_type == "pass",
      !is.na(receiver_player_id)
    ) %>%
    dplyr::group_by(season, week, team = posteam) %>%
    dplyr::summarise(team_targets_pass = dplyr::n(), .groups = "drop")

  # --- Team reception denominator (for RB touch share) ----------------------
  team_receptions_raw <- pbp %>%
    dplyr::filter(
      play_type == "pass",
      !is.na(receiver_player_id),
      complete_pass == 1L
    ) %>%
    dplyr::group_by(season, week, team = posteam) %>%
    dplyr::summarise(team_receptions = dplyr::n(), .groups = "drop")

  # --- Per-player carry counts per (player_id, season, week, team) ----------
  player_carries <- carries_raw %>%
    dplyr::group_by(player_id, player_name, season, week, team) %>%
    dplyr::summarise(carries = dplyr::n(), .groups = "drop")

  # --- Per-player target and reception counts --------------------------------
  player_pass <- targets_raw %>%
    dplyr::group_by(player_id, player_name, season, week, team) %>%
    dplyr::summarise(
      targets    = dplyr::n(),
      receptions = sum(complete_pass == 1L, na.rm = TRUE),
      .groups    = "drop"
    )

  # --- Team total plays for quality filter ----------------------------------
  team_total_plays <- pbp %>%
    dplyr::filter(play_type %in% c("pass", "run")) %>%
    dplyr::group_by(season, week, team = posteam) %>%
    dplyr::summarise(n_team_plays = dplyr::n(), .groups = "drop")

  # --- Join position from roster_positions ----------------------------------
  # One player can appear as both carrier and receiver in a season.
  # Merge carry and pass records first, then join position.
  roster_season <- roster_positions %>%
    dplyr::filter(season == s, position %in% skill_positions) %>%
    dplyr::select(player_id, position) %>%
    dplyr::distinct(player_id, .keep_all = TRUE)

  # All players active in this season/week with at least one carry or target
  all_players <- dplyr::bind_rows(
    player_carries %>%
      dplyr::select(player_id, player_name, season, week, team),
    player_pass %>%
      dplyr::select(player_id, player_name, season, week, team)
  ) %>%
    dplyr::distinct(player_id, season, week, .keep_all = TRUE)

  # Attach stats (NAs fill to 0 after join)
  player_week <- all_players %>%
    dplyr::left_join(
      player_carries %>% dplyr::select(player_id, season, week, carries),
      by = c("player_id", "season", "week")
    ) %>%
    dplyr::left_join(
      player_pass %>% dplyr::select(player_id, season, week, targets, receptions),
      by = c("player_id", "season", "week")
    ) %>%
    dplyr::mutate(
      carries    = coalesce(carries,    0L),
      targets    = coalesce(targets,    0L),
      receptions = coalesce(receptions, 0L)
    ) %>%
    # Join position -- filter to skill positions only
    dplyr::inner_join(roster_season, by = "player_id")

  if (nrow(player_week) == 0L) return(dplyr::tibble())

  # Traded players: keep last known team per (player_id, season, week)
  # (in case a player switched teams mid-week -- rare but guard it)
  player_week <- player_week %>%
    dplyr::group_by(player_id, season, week) %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::ungroup()

  # --- Attach team denominators and compute usage share ----------------------
  player_week <- player_week %>%
    dplyr::left_join(team_carries_raw,    by = c("season", "week", "team")) %>%
    dplyr::left_join(team_targets_raw,    by = c("season", "week", "team")) %>%
    dplyr::left_join(team_receptions_raw, by = c("season", "week", "team")) %>%
    dplyr::left_join(team_total_plays,    by = c("season", "week", "team")) %>%
    dplyr::mutate(
      team_carries     = coalesce(team_carries,     0L),
      team_targets_pass = coalesce(team_targets_pass, 0L),
      team_receptions  = coalesce(team_receptions,  0L),
      n_team_plays     = coalesce(n_team_plays,     0L)
    )

  # Apply quality filter: exclude team-weeks with too few plays
  player_week <- player_week %>%
    dplyr::filter(n_team_plays >= min_team_plays)

  # Compute position-appropriate usage share:
  #   WR/TE: target share = targets / team_targets_pass
  #   RB:    touch share  = (carries + receptions) / (team_carries + team_receptions)
  player_week <- player_week %>%
    dplyr::mutate(
      usage_numerator = dplyr::if_else(
        position %in% c("WR", "TE"),
        as.integer(targets),
        as.integer(carries + receptions)
      ),
      team_usage_denominator = dplyr::if_else(
        position %in% c("WR", "TE"),
        as.integer(team_targets_pass),
        as.integer(team_carries + team_receptions)
      ),
      usage_share = dplyr::if_else(
        team_usage_denominator > 0L,
        usage_numerator / team_usage_denominator,
        NA_real_
      ),
      active = usage_numerator > 0L
    ) %>%
    dplyr::select(
      player_id, player_name, season, week, team, position,
      usage_share, usage_numerator, team_usage_denominator,
      n_team_plays, active
    ) %>%
    dplyr::arrange(season, week, position, dplyr::desc(usage_share))

  player_week
}


# ------------------------------------------------------------------------------
# .bootstrap_ramp_diff_ci
# ------------------------------------------------------------------------------
# Parametric-free CI for the difference in means (group_a - group_b).
# Uses the basic (reverse-percentile) bootstrap.
# Falls back to Welch t-interval when min(n_a, n_b) < 10.
# Pattern mirrors R/25 .bootstrap_mean_diff_ci().
#
# @param a Numeric vector. Group A values (treatment).
# @param b Numeric vector. Group B values (control).
# @param B Integer. Bootstrap resamples. Default BOOTSTRAP_B_W12.
# @param conf_level Numeric. Confidence level. Default 0.95.
# @return Named list: estimate, ci_lower, ci_upper, n_a, n_b, method.
# @noRd
.bootstrap_ramp_diff_ci <- function(a, b,
                                     B          = BOOTSTRAP_B_W12,
                                     conf_level = 0.95) {

  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  n_a <- length(a)
  n_b <- length(b)
  obs_diff <- mean(a) - mean(b)

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
# FUNCTION: calculate_weekly_usage_share
# ==============================================================================

#' Calculate Per-Player, Per-Week Usage Share From Multi-Season PBP
#'
#' @description
#' Exports the \code{.compute_weekly_usage_season()} internal helper across
#' all requested seasons, handling the season-by-season memory loop and
#' combining results into a single tibble.
#'
#' Usage metric by position:
#' \itemize{
#'   \item \strong{WR/TE}: target share = player targets / team total targets
#'   \item \strong{RB}: touch share = (carries + receptions) /
#'     (team carries + team receptions)
#' }
#'
#' Touch share for RB was chosen over carry share to capture pass-catching
#' backs (e.g., CMC, Kamara, Gainwell) whose fantasy value derives from
#' receiving work as much as rushing. A carry-only metric would misclassify
#' role expansions driven by increased receiving work.
#'
#' @param seasons Integer vector. Seasons to process.
#'   Default: \code{ANALYSIS_SEASONS_W12} (2010-2025).
#' @param cache_dir Character. Cache directory for \code{load_normalized_season()}.
#'   Default: \code{CACHE_DIR_W12}.
#' @param roster_positions Tibble. Position lookup with columns: player_id (chr),
#'   season (int), position (chr). Typically from \code{nflreadr::load_rosters()}
#'   with gsis_id renamed to player_id. Must cover all requested seasons.
#' @param skill_positions Character vector. Positions to include.
#'   Default: \code{SKILL_POSITIONS_W12} (RB, WR, TE).
#' @param weeks_filter Integer vector or NULL. If provided, only include these
#'   weeks in the output. Default NULL (all weeks).
#' @param verbose Logical. Default TRUE.
#'
#' @return Tibble with one row per (player_id, season, week). Columns:
#'   \describe{
#'     \item{player_id}{chr: GSIS ID}
#'     \item{player_name}{chr}
#'     \item{season}{int}
#'     \item{week}{int}
#'     \item{team}{chr: team for this week (last known if mid-week trade)}
#'     \item{position}{chr: from roster_positions}
#'     \item{usage_share}{dbl: target share (WR/TE) or touch share (RB)}
#'     \item{usage_numerator}{int: targets (WR/TE) or carries+receptions (RB)}
#'     \item{team_usage_denominator}{int: team targets or team carries+receptions}
#'     \item{n_team_plays}{int: total team offensive plays (quality filter)}
#'     \item{active}{lgl: TRUE if usage_numerator > 0}
#'   }
#'
#' @details
#' Only weeks where the player's team had at least \code{MIN_TEAM_PLAYS_W12}
#' offensive plays are included. Filters to \code{play_type \%in\% c("pass", "run")}.
#'
#' @seealso \code{\link{identify_usage_ramps}}, \code{\link{classify_treatment_control_ramp}}
#' @export
calculate_weekly_usage_share <- function(
    seasons          = ANALYSIS_SEASONS_W12,
    cache_dir        = CACHE_DIR_W12,
    roster_positions,
    skill_positions  = SKILL_POSITIONS_W12,
    weeks_filter     = NULL,
    verbose          = TRUE
) {

  stopifnot(
    is.integer(seasons) || is.numeric(seasons),
    is.data.frame(roster_positions),
    is.character(skill_positions),
    is.logical(verbose)
  )

  required_roster <- c("player_id", "season", "position")
  missing_roster  <- setdiff(required_roster, names(roster_positions))
  if (length(missing_roster) > 0L) {
    stop(glue(
      "calculate_weekly_usage_share(): roster_positions missing columns: ",
      "{paste(missing_roster, collapse = ', ')}"
    ), call. = FALSE)
  }

  seasons <- sort(as.integer(seasons))

  if (verbose) {
    message(glue(
      "calculate_weekly_usage_share(): processing {length(seasons)} seasons ",
      "({min(seasons)}-{max(seasons)})"
    ))
  }

  usage_list <- vector("list", length(seasons))

  for (i in seq_along(seasons)) {
    s <- seasons[[i]]
    if (verbose) message(glue("  Season {s} ({i}/{length(seasons)})..."))

    roster_s <- roster_positions %>%
      dplyr::filter(season == s)

    if (nrow(roster_s) == 0L) {
      if (verbose) message(glue("    No roster data for season {s}, skipping."))
      next
    }

    pbp <- tryCatch(
      load_normalized_season(s, cache_dir = cache_dir),
      error = function(e) {
        message(glue("    WARNING: Could not load season {s}: {conditionMessage(e)}"))
        NULL
      }
    )

    if (is.null(pbp)) {
      gc()
      next
    }

    usage_s <- tryCatch(
      .compute_weekly_usage_season(
        pbp              = pbp,
        roster_positions = roster_s,
        skill_positions  = skill_positions
      ),
      error = function(e) {
        message(glue("    WARNING: usage computation failed for {s}: {conditionMessage(e)}"))
        NULL
      }
    )

    usage_list[[i]] <- usage_s

    rm(pbp)
    gc()
  }

  weekly_usage <- dplyr::bind_rows(purrr::compact(usage_list))

  if (!is.null(weeks_filter)) {
    weekly_usage <- weekly_usage %>%
      dplyr::filter(week %in% as.integer(weeks_filter))
  }

  if (verbose) {
    n_players  <- dplyr::n_distinct(weekly_usage$player_id)
    n_seasons  <- dplyr::n_distinct(weekly_usage$season)
    n_rows     <- nrow(weekly_usage)
    message(glue(
      "  Done: {format(n_rows, big.mark=',')} player-week rows | ",
      "{format(n_players, big.mark=',')} unique players | ",
      "{n_seasons} seasons"
    ))
  }

  weekly_usage
}


# ==============================================================================
# FUNCTION: identify_usage_ramps
# ==============================================================================

#' Identify Players With an Increasing Usage Trajectory in the Training Window
#'
#' @description
#' For each player-season, computes average usage share in the early sub-window
#' (Weeks 1-4) and late sub-window (Weeks 5-8) and flags players whose usage
#' increased by more than \code{ramp_threshold} relative.
#'
#' Two methods are supported via the \code{method} argument:
#' \describe{
#'   \item{\code{"sub_window"} (primary)}{Requires \code{min_active_weeks} of
#'     4 weeks active in each sub-window. A player missing 2+ weeks in either
#'     window is flagged as excluded. Prevents injured players' flat averages
#'     from contaminating the control group.}
#'   \item{\code{"full_split"} (robustness)}{Same window comparison (W1-4 vs
#'     W5-8) but with a relaxed floor of 1 active week minimum per sub-window.
#'     Tests whether the \code{min_active_weeks} floor is driving results.}
#' }
#'
#' @param weekly_usage Tibble. Output of \code{calculate_weekly_usage_share()}.
#'   Must contain: player_id, player_name, season, week, team, position,
#'   usage_share, active.
#' @param method Character. One of \code{"sub_window"} (default) or
#'   \code{"full_split"}. See Details.
#' @param ramp_threshold Numeric. Minimum relative change to classify as
#'   treatment or declining. Default: \code{RAMP_THRESHOLD_W12} (0.10 = 10\%).
#' @param subwindow_early Integer vector. Early sub-window weeks.
#'   Default: \code{SUBWINDOW_EARLY_W12} (1:4).
#' @param subwindow_late Integer vector. Late sub-window weeks.
#'   Default: \code{SUBWINDOW_LATE_W12} (5:8).
#' @param min_active_weeks Integer. Minimum active weeks required per
#'   sub-window for the primary \code{"sub_window"} method.
#'   Default: \code{MIN_ACTIVE_WEEKS_SUBWINDOW_W12} (3).
#' @param min_usage_floor Numeric. Minimum early-window average usage required
#'   to be eligible (not excluded). Default: \code{MIN_USAGE_FLOOR_W12} (0.01).
#' @param verbose Logical. Default TRUE.
#'
#' @return Tibble with one row per (player_id, season). Columns:
#'   \describe{
#'     \item{player_id}{chr}
#'     \item{player_name}{chr}
#'     \item{season}{int}
#'     \item{team}{chr: most recent team in training window}
#'     \item{position}{chr}
#'     \item{early_avg}{dbl: mean usage share in early sub-window}
#'     \item{late_avg}{dbl: mean usage share in late sub-window}
#'     \item{n_early_active}{int: active weeks in early sub-window}
#'     \item{n_late_active}{int: active weeks in late sub-window}
#'     \item{relative_change}{dbl: (late_avg - early_avg) / early_avg}
#'     \item{ramp_flag}{lgl: TRUE if relative_change > ramp_threshold}
#'     \item{ramp_method}{chr: method used}
#'   }
#'
#' @seealso \code{\link{classify_treatment_control_ramp}},
#'   \code{\link{ramp_robustness_check}}
#' @export
identify_usage_ramps <- function(
    weekly_usage,
    method           = c("sub_window", "full_split"),
    ramp_threshold   = RAMP_THRESHOLD_W12,
    subwindow_early  = SUBWINDOW_EARLY_W12,
    subwindow_late   = SUBWINDOW_LATE_W12,
    min_active_weeks = MIN_ACTIVE_WEEKS_SUBWINDOW_W12,
    min_usage_floor  = MIN_USAGE_FLOOR_W12,
    verbose          = TRUE
) {

  method <- match.arg(method)

  stopifnot(
    is.data.frame(weekly_usage),
    is.numeric(ramp_threshold), ramp_threshold > 0,
    is.integer(subwindow_early) || is.numeric(subwindow_early),
    is.integer(subwindow_late)  || is.numeric(subwindow_late),
    is.integer(min_active_weeks) || is.numeric(min_active_weeks),
    is.numeric(min_usage_floor),
    is.logical(verbose)
  )

  required_cols <- c("player_id", "player_name", "season", "week",
                     "team", "position", "usage_share", "active")
  missing_cols  <- setdiff(required_cols, names(weekly_usage))
  if (length(missing_cols) > 0L) {
    stop(glue(
      "identify_usage_ramps(): weekly_usage missing columns: ",
      "{paste(missing_cols, collapse = ', ')}"
    ), call. = FALSE)
  }

  if (nrow(weekly_usage) == 0L) {
    message("identify_usage_ramps(): weekly_usage is empty. Returning empty tibble.")
    return(dplyr::tibble())
  }

  # Effective games floor per sub-window based on method
  floor_per_window <- if (method == "sub_window") {
    as.integer(min_active_weeks)
  } else {
    1L  # full_split: just need 1 active week per sub-window
  }

  subwindow_early <- as.integer(subwindow_early)
  subwindow_late  <- as.integer(subwindow_late)

  # --- Compute sub-window summaries per player-season ------------------------
  ramp_data <- weekly_usage %>%
    dplyr::filter(week %in% c(subwindow_early, subwindow_late)) %>%
    dplyr::mutate(
      sub_window = dplyr::if_else(week %in% subwindow_early, "early", "late")
    ) %>%
    dplyr::group_by(player_id, player_name, season, position, sub_window) %>%
    dplyr::summarise(
      window_avg    = mean(usage_share[active], na.rm = TRUE),
      n_active      = sum(active, na.rm = TRUE),
      last_team     = dplyr::last(team),
      .groups       = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = sub_window,
      values_from = c(window_avg, n_active, last_team)
    )

  # Normalize column names after pivot_wider
  ramp_data <- ramp_data %>%
    dplyr::rename(
      early_avg       = window_avg_early,
      late_avg        = window_avg_late,
      n_early_active  = n_active_early,
      n_late_active   = n_active_late
    ) %>%
    dplyr::mutate(
      # Resolve team: prefer late sub-window team (more recent)
      team = dplyr::coalesce(last_team_late, last_team_early)
    ) %>%
    dplyr::select(-last_team_early, -last_team_late)

  # Fill NAs for players completely absent from a sub-window
  ramp_data <- ramp_data %>%
    dplyr::mutate(
      early_avg      = dplyr::if_else(is.nan(early_avg),      NA_real_, early_avg),
      late_avg       = dplyr::if_else(is.nan(late_avg),       NA_real_, late_avg),
      n_early_active = coalesce(n_early_active, 0L),
      n_late_active  = coalesce(n_late_active,  0L)
    )

  # --- Compute relative change and apply floors ------------------------------
  ramp_data <- ramp_data %>%
    dplyr::mutate(
      # Relative change: (late - early) / early
      # NA if early_avg is NA or below the usage floor (undefined ramp)
      relative_change = dplyr::case_when(
        is.na(early_avg)                      ~ NA_real_,
        early_avg < min_usage_floor           ~ NA_real_,
        TRUE ~ (late_avg - early_avg) / early_avg
      ),
      ramp_flag   = !is.na(relative_change) & relative_change > ramp_threshold,
      ramp_method = method
    )

  # Apply active-weeks floor for exclusion flag
  ramp_data <- ramp_data %>%
    dplyr::mutate(
      games_floor_met = (
        n_early_active >= floor_per_window &
        n_late_active  >= floor_per_window &
        !is.na(early_avg) &
        early_avg >= min_usage_floor
      )
    )

  if (verbose) {
    n_total   <- nrow(ramp_data)
    n_ramp    <- sum(ramp_data$ramp_flag & ramp_data$games_floor_met, na.rm = TRUE)
    n_floor   <- sum(!ramp_data$games_floor_met, na.rm = TRUE)
    message(glue(
      "  identify_usage_ramps() [{method}]: ",
      "{format(n_total, big.mark=',')} player-seasons evaluated | ",
      "{format(n_ramp, big.mark=',')} ramp flagged | ",
      "{format(n_floor, big.mark=',')} excluded (games floor)"
    ))
  }

  ramp_data %>%
    dplyr::select(
      player_id, player_name, season, team, position,
      early_avg, late_avg, n_early_active, n_late_active,
      relative_change, ramp_flag, ramp_method, games_floor_met
    ) %>%
    dplyr::arrange(season, position, dplyr::desc(relative_change))
}


# ==============================================================================
# FUNCTION: classify_treatment_control_ramp
# ==============================================================================

#' Classify Players Into Treatment, Control, Declining, and Excluded Groups
#'
#' @description
#' Applies the four-category group assignment to ramp-flagged player-seasons:
#' \itemize{
#'   \item \code{"treatment"}: Usage increased > \code{ramp_threshold} relative,
#'     games floor met.
#'   \item \code{"control"}: Usage changed within +/- \code{ramp_threshold},
#'     games floor met. Primary comparison group.
#'   \item \code{"declining"}: Usage decreased > \code{ramp_threshold} relative,
#'     games floor met. Retained for heterogeneous effects; not in primary
#'     contrast.
#'   \item \code{"excluded"}: Games floor not met, or early-window average below
#'     \code{min_usage_floor}.
#' }
#'
#' @param ramp_flags Tibble. Output of \code{identify_usage_ramps()}.
#' @param ramp_threshold Numeric. Default: \code{RAMP_THRESHOLD_W12} (0.10).
#' @param seasons Integer vector. Restrict to these seasons.
#'   Default: \code{ANALYSIS_SEASONS_W12}.
#' @param verbose Logical. Default TRUE.
#'
#' @return Tibble with one row per (player_id, season). Columns from
#'   \code{ramp_flags} plus:
#'   \describe{
#'     \item{group}{chr: "treatment", "control", "declining", or "excluded"}
#'     \item{era}{chr: "early" (< ERA_BREAKPOINT_W12) or "modern"}
#'   }
#'
#' @seealso \code{\link{identify_usage_ramps}},
#'   \code{\link{check_balance_ramp_groups}}
#' @export
classify_treatment_control_ramp <- function(
    ramp_flags,
    ramp_threshold = RAMP_THRESHOLD_W12,
    seasons        = ANALYSIS_SEASONS_W12,
    verbose        = TRUE
) {

  stopifnot(
    is.data.frame(ramp_flags),
    is.numeric(ramp_threshold),
    is.logical(verbose)
  )

  required_cols <- c("player_id", "player_name", "season", "team", "position",
                     "early_avg", "late_avg", "relative_change",
                     "ramp_flag", "games_floor_met")
  missing_cols  <- setdiff(required_cols, names(ramp_flags))
  if (length(missing_cols) > 0L) {
    stop(glue(
      "classify_treatment_control_ramp(): ramp_flags missing columns: ",
      "{paste(missing_cols, collapse = ', ')}"
    ), call. = FALSE)
  }

  classified <- ramp_flags %>%
    dplyr::filter(season %in% as.integer(seasons)) %>%
    dplyr::mutate(
      group = dplyr::case_when(
        !games_floor_met                                         ~ "excluded",
        !is.na(relative_change) & relative_change >  ramp_threshold ~ "treatment",
        !is.na(relative_change) & relative_change < -ramp_threshold ~ "declining",
        !is.na(relative_change)                                  ~ "control",
        TRUE                                                     ~ "excluded"
      ),
      era = dplyr::if_else(
        season < ERA_BREAKPOINT_W12, "early", "modern"
      )
    )

  if (verbose) {
    counts <- classified %>%
      dplyr::count(group) %>%
      dplyr::arrange(group)
    message("  classify_treatment_control_ramp() group counts:")
    for (i in seq_len(nrow(counts))) {
      message(glue("    {counts$group[[i]]}: {format(counts$n[[i]], big.mark=',')}"))
    }
  }

  classified
}


# ==============================================================================
# FUNCTION: check_balance_ramp_groups
# ==============================================================================

#' Check Pre-Treatment Balance Between Treatment and Control Groups
#'
#' @description
#' Compares treatment and control groups on key pre-treatment covariates to
#' assess the validity of the ramp experiment's quasi-experimental design.
#' Imbalanced groups suggest confounding that may bias the effect estimate.
#'
#' Covariates checked: position distribution, era distribution, early-window
#' average usage, season distribution.
#'
#' Standardized mean difference (SMD) is reported for continuous covariates.
#' SMD > 0.25 is flagged as a potential imbalance concern.
#'
#' @param groups Tibble. Output of \code{classify_treatment_control_ramp()}.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{position_table}{Tibble. Position distribution by group.}
#'     \item{era_table}{Tibble. Era distribution by group.}
#'     \item{usage_smd}{Tibble. SMD for early-window usage by group pair.}
#'     \item{season_table}{Tibble. Season distribution by group.}
#'     \item{n_by_group}{Tibble. N per group.}
#'     \item{balance_flags}{Tibble. Which covariates show potential imbalance.}
#'     \item{summary}{chr. Human-readable balance summary.}
#'   }
#'
#' @seealso \code{\link{classify_treatment_control_ramp}},
#'   \code{\link{validate_ramp_assumptions}}
#' @export
check_balance_ramp_groups <- function(groups, verbose = TRUE) {

  stopifnot(is.data.frame(groups), is.logical(verbose))

  required_cols <- c("player_id", "season", "position", "group",
                     "early_avg", "era")
  missing_cols  <- setdiff(required_cols, names(groups))
  if (length(missing_cols) > 0L) {
    stop(glue(
      "check_balance_ramp_groups(): groups missing columns: ",
      "{paste(missing_cols, collapse = ', ')}"
    ), call. = FALSE)
  }

  trt_ctl <- groups %>%
    dplyr::filter(group %in% c("treatment", "control"))

  if (nrow(trt_ctl) == 0L) {
    message("check_balance_ramp_groups(): no treatment/control observations.")
    return(list(summary = "No treatment/control observations."))
  }

  # --- N by group ------------------------------------------------------------
  n_by_group <- groups %>%
    dplyr::count(group) %>%
    dplyr::arrange(group)

  # --- Position distribution -------------------------------------------------
  position_table <- trt_ctl %>%
    dplyr::count(group, position) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(pct = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(group, position)

  # --- Era distribution ------------------------------------------------------
  era_table <- trt_ctl %>%
    dplyr::count(group, era) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(pct = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(group, era)

  # --- Season distribution ---------------------------------------------------
  season_table <- trt_ctl %>%
    dplyr::count(group, season) %>%
    dplyr::arrange(group, season)

  # --- SMD for early-window usage share -------------------------------------
  trt_vals <- trt_ctl$early_avg[trt_ctl$group == "treatment" &
                                  !is.na(trt_ctl$early_avg)]
  ctl_vals <- trt_ctl$early_avg[trt_ctl$group == "control" &
                                  !is.na(trt_ctl$early_avg)]

  pooled_sd <- if (length(c(trt_vals, ctl_vals)) > 1L) {
    sd_t <- if (length(trt_vals) > 1L) stats::sd(trt_vals) else NA_real_
    sd_c <- if (length(ctl_vals) > 1L) stats::sd(ctl_vals) else NA_real_
    if (!is.na(sd_t) && !is.na(sd_c) && (sd_t + sd_c) > 0) {
      sqrt((sd_t^2 + sd_c^2) / 2)
    } else NA_real_
  } else NA_real_

  early_usage_smd <- if (!is.na(pooled_sd) && pooled_sd > 0) {
    abs(mean(trt_vals, na.rm = TRUE) - mean(ctl_vals, na.rm = TRUE)) / pooled_sd
  } else NA_real_

  usage_smd <- dplyr::tibble(
    covariate    = "early_window_usage",
    mean_trt     = mean(trt_vals, na.rm = TRUE),
    mean_ctl     = mean(ctl_vals, na.rm = TRUE),
    smd          = early_usage_smd,
    flag_concern = !is.na(early_usage_smd) && early_usage_smd > 0.25
  )

  # --- Balance flags ---------------------------------------------------------
  balance_flags <- dplyr::tibble(
    covariate = "early_window_usage",
    smd       = early_usage_smd,
    concern   = !is.na(early_usage_smd) && early_usage_smd > 0.25,
    note      = dplyr::if_else(
      !is.na(early_usage_smd) && early_usage_smd > 0.25,
      "SMD > 0.25 -- potential baseline usage imbalance",
      "OK"
    )
  )

  # --- Summary string --------------------------------------------------------
  n_trt <- sum(n_by_group$n[n_by_group$group == "treatment"], na.rm = TRUE)
  n_ctl <- sum(n_by_group$n[n_by_group$group == "control"],   na.rm = TRUE)
  smd_val <- round(early_usage_smd, 3)
  smd_str <- if (!is.na(smd_val)) as.character(smd_val) else "N/A"
  n_concerns <- sum(balance_flags$concern, na.rm = TRUE)

  summary_str <- glue(
    "Balance check: N treatment = {format(n_trt, big.mark=',')}, ",
    "N control = {format(n_ctl, big.mark=',')}. ",
    "Early-usage SMD = {smd_str}. ",
    "{n_concerns} covariate(s) flagged for potential imbalance."
  )

  if (verbose) message(summary_str)

  list(
    position_table = position_table,
    era_table      = era_table,
    usage_smd      = usage_smd,
    season_table   = season_table,
    n_by_group     = n_by_group,
    balance_flags  = balance_flags,
    summary        = as.character(summary_str)
  )
}


# ==============================================================================
# FUNCTION: validate_ramp_assumptions
# ==============================================================================

#' Validate Key Assumptions Underlying the Usage Ramp Experiment
#'
#' @description
#' Runs five assumption checks specific to the ramp experiment design:
#' \enumerate{
#'   \item \strong{Usage floor coverage}: All treatment players have early-window
#'     average above \code{MIN_USAGE_FLOOR_W12}. Players at zero early usage
#'     have undefined ramp direction.
#'   \item \strong{Ramp magnitude distribution}: Distribution of relative changes
#'     in treatment group. Extremely large values (> 5x) may indicate data
#'     quality issues.
#'   \item \strong{Season coverage}: Treatment and control groups are spread
#'     across multiple seasons (not concentrated in one or two).
#'   \item \strong{Position coverage}: All three positions (RB, WR, TE) are
#'     represented in both groups at reasonable sample sizes.
#'   \item \strong{Denominator stability}: Team usage denominators are plausible
#'     (no team with 0 denominators in multiple weeks).
#' }
#'
#' @param groups Tibble. Output of \code{classify_treatment_control_ramp()}.
#' @param weekly_usage Tibble. Output of \code{calculate_weekly_usage_share()}.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list with elements: \code{checks} (tibble), \code{details}
#'   (named list per check), \code{n_passed} (int), \code{summary} (chr).
#'
#' @seealso \code{\link{check_balance_ramp_groups}}
#' @export
validate_ramp_assumptions <- function(groups, weekly_usage, verbose = TRUE) {

  stopifnot(
    is.data.frame(groups),
    is.data.frame(weekly_usage),
    is.logical(verbose)
  )

  checks <- list()

  # Check 1: Usage floor coverage
  trt_early_ok <- groups %>%
    dplyr::filter(group == "treatment") %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_above_floor = sum(early_avg >= MIN_USAGE_FLOOR_W12, na.rm = TRUE)
    )
  floor_pct <- if (trt_early_ok$n_total > 0L) {
    trt_early_ok$n_above_floor / trt_early_ok$n_total
  } else 1.0
  checks[["usage_floor"]] <- dplyr::tibble(
    check = "All treatment players above usage floor",
    result = floor_pct >= 0.99,
    note   = glue(
      "{trt_early_ok$n_above_floor}/{trt_early_ok$n_total} treatment players ",
      "above MIN_USAGE_FLOOR = {MIN_USAGE_FLOOR_W12}"
    )
  )

  # Check 2: Ramp magnitude -- flag extreme outliers (> 10x early usage)
  trt_ramps <- groups %>%
    dplyr::filter(group == "treatment", !is.na(relative_change))
  n_extreme <- sum(trt_ramps$relative_change > 10.0, na.rm = TRUE)
  checks[["ramp_magnitude"]] <- dplyr::tibble(
    check = "No extreme ramp outliers (relative_change > 10x)",
    result = n_extreme == 0L,
    note   = glue("{n_extreme} treatment player-seasons with relative_change > 10x")
  )

  # Check 3: Season spread
  n_seasons_trt <- dplyr::n_distinct(
    groups$season[groups$group == "treatment"]
  )
  n_seasons_ctl <- dplyr::n_distinct(
    groups$season[groups$group == "control"]
  )
  checks[["season_spread"]] <- dplyr::tibble(
    check = "Both groups span >= 5 seasons",
    result = n_seasons_trt >= 5L && n_seasons_ctl >= 5L,
    note   = glue(
      "Treatment: {n_seasons_trt} seasons | Control: {n_seasons_ctl} seasons"
    )
  )

  # Check 4: Position coverage (at least 10 per position per group)
  pos_counts <- groups %>%
    dplyr::filter(group %in% c("treatment", "control")) %>%
    dplyr::count(group, position)
  min_pos_n <- min(pos_counts$n, na.rm = TRUE)
  checks[["position_coverage"]] <- dplyr::tibble(
    check = "All positions have >= 10 players per group",
    result = min_pos_n >= 10L,
    note   = glue(
      "Minimum position-group cell size = {min_pos_n}"
    )
  )

  # Check 5: Denominator plausibility (no NA usage shares in training weeks)
  training_usage <- weekly_usage %>%
    dplyr::filter(week %in% TRAINING_WEEKS_W12)
  n_na_share <- sum(is.na(training_usage$usage_share), na.rm = FALSE)
  n_total_training <- nrow(training_usage)
  na_pct <- if (n_total_training > 0L) n_na_share / n_total_training else 0
  checks[["denominator_stability"]] <- dplyr::tibble(
    check = "NA usage share rate < 5% in training weeks",
    result = na_pct < 0.05,
    note   = glue(
      "{n_na_share}/{n_total_training} training-week rows have NA usage_share ",
      "({round(na_pct * 100, 1)}%)"
    )
  )

  checks_tbl <- dplyr::bind_rows(checks)
  n_passed   <- sum(checks_tbl$result, na.rm = TRUE)

  summary_str <- glue(
    "validate_ramp_assumptions(): {n_passed}/{nrow(checks_tbl)} checks passed."
  )

  if (verbose) {
    message(summary_str)
    for (i in seq_len(nrow(checks_tbl))) {
      icon <- if (checks_tbl$result[[i]]) "[PASS]" else "[WARN]"
      message(glue("  {icon} {checks_tbl$check[[i]]}: {checks_tbl$note[[i]]}"))
    }
  }

  list(
    checks   = checks_tbl,
    n_passed = as.integer(n_passed),
    summary  = as.character(summary_str)
  )
}


# ==============================================================================
# FUNCTION: create_ramp_experiment_specification
# ==============================================================================

#' Generate a Formal Design Document for the Usage Ramp Experiment
#'
#' @description
#' Produces a human-readable specification document summarizing the experiment
#' design, group sizes, balance results, power analysis, and key assumptions.
#' Returned as a character string suitable for writing to disk.
#'
#' @param groups Tibble. Output of \code{classify_treatment_control_ramp()}.
#' @param balance List. Output of \code{check_balance_ramp_groups()}.
#' @param assumptions List. Output of \code{validate_ramp_assumptions()}.
#' @param training_weeks Integer vector. Default \code{TRAINING_WEEKS_W12}.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W12}.
#' @param ramp_threshold Numeric. Default \code{RAMP_THRESHOLD_W12}.
#' @param method Character. Ramp detection method used. Default "sub_window".
#'
#' @return Character string. Multi-line formatted experiment specification.
#'
#' @seealso \code{\link{check_balance_ramp_groups}},
#'   \code{\link{validate_ramp_assumptions}}
#' @export
create_ramp_experiment_specification <- function(
    groups,
    balance,
    assumptions,
    training_weeks  = TRAINING_WEEKS_W12,
    outcome_weeks   = OUTCOME_WEEKS_W12,
    ramp_threshold  = RAMP_THRESHOLD_W12,
    method          = "sub_window"
) {

  stopifnot(is.data.frame(groups), is.list(balance), is.list(assumptions))

  n_trt  <- sum(groups$group == "treatment", na.rm = TRUE)
  n_ctl  <- sum(groups$group == "control",   na.rm = TRUE)
  n_dec  <- sum(groups$group == "declining", na.rm = TRUE)
  n_excl <- sum(groups$group == "excluded",  na.rm = TRUE)

  sep  <- strrep("=", 60)
  sep2 <- strrep("-", 60)

  spec <- glue(
    "{sep}\n",
    "USAGE RAMP EXPERIMENT -- DESIGN SPECIFICATION\n",
    "Season 2, Week 12\n",
    "{sep}\n\n",

    "RESEARCH QUESTION\n",
    "{sep2}\n",
    "Is an increasing usage trend in the first half of a season a leading\n",
    "indicator of sustained role changes and improved fantasy output\n",
    "in the second half?\n\n",

    "DESIGN PARAMETERS\n",
    "{sep2}\n",
    "Usage metric : Target share (WR/TE), touch share (RB)\n",
    "               Touch = (carries + receptions) / (team carries + receptions)\n",
    "Ramp method  : {method}\n",
    "Sub-windows  : Early = Weeks {min(SUBWINDOW_EARLY_W12)}-{max(SUBWINDOW_EARLY_W12)}, ",
    "Late = Weeks {min(SUBWINDOW_LATE_W12)}-{max(SUBWINDOW_LATE_W12)}\n",
    "Games floor  : {MIN_ACTIVE_WEEKS_SUBWINDOW_W12} of {length(SUBWINDOW_EARLY_W12)} ",
    "weeks per sub-window (primary method)\n",
    "Ramp threshold: {ramp_threshold * 100}% relative increase\n",
    "Training window: Weeks {min(training_weeks)}-{max(training_weeks)}\n",
    "Outcome window : Weeks {min(outcome_weeks)}-{max(outcome_weeks)}\n",
    "Positions    : {paste(SKILL_POSITIONS_W12, collapse=', ')}\n",
    "Seasons      : {min(groups$season)}-{max(groups$season)}\n",
    "Min usage floor: {MIN_USAGE_FLOOR_W12} (early-window avg; below = excluded)\n\n",

    "GROUP DEFINITIONS\n",
    "{sep2}\n",
    "Treatment : Ramp > {ramp_threshold * 100}% relative, >= {MIN_ACTIVE_WEEKS_SUBWINDOW_W12}",
    " active weeks per sub-window\n",
    "Control   : Change within +/-{ramp_threshold * 100}%, same floor (stable role)\n",
    "Declining : Drop > {ramp_threshold * 100}% relative, same floor (retained for ",
    "heterogeneous analysis)\n",
    "Excluded  : Games floor not met OR early-window average < {MIN_USAGE_FLOOR_W12}\n\n",

    "GROUP SIZES\n",
    "{sep2}\n",
    "Treatment  : {format(n_trt,  big.mark=',')}\n",
    "Control    : {format(n_ctl,  big.mark=',')}\n",
    "Declining  : {format(n_dec,  big.mark=',')}\n",
    "Excluded   : {format(n_excl, big.mark=',')}\n",
    "Total      : {format(n_trt + n_ctl + n_dec + n_excl, big.mark=',')}\n\n",

    "BALANCE SUMMARY\n",
    "{sep2}\n",
    "{balance$summary}\n\n",

    "ASSUMPTION CHECKS\n",
    "{sep2}\n",
    "{assumptions$summary}\n",
    paste(
      glue_data(assumptions$checks,
        "  {ifelse(result, '[PASS]', '[WARN]')} {check}: {note}"),
      collapse = "\n"
    ),
    "\n\n",

    "PRIMARY OUTCOME\n",
    "{sep2}\n",
    "Mean PPR fantasy points per game in Weeks {min(outcome_weeks)}-{max(outcome_weeks)} ",
    "(treatment vs control)\n\n",

    "SECONDARY OUTCOME\n",
    "{sep2}\n",
    "Mean usage share in Weeks {min(outcome_weeks)}-{max(outcome_weeks)} ",
    "(treatment vs control)\n\n",

    "ROBUSTNESS CHECKS\n",
    "{sep2}\n",
    "1. Method sweep: sub_window vs full_split (relaxed games floor)\n",
    "2. Threshold sweep: 5%, 10%, 15% ramp threshold\n",
    "3. Rookie vs veteran subgroup comparison\n\n",

    "LIMITATIONS\n",
    "{sep2}\n",
    "1. Ramp detection uses two 4-week windows. Players who ramp and then\n",
    "   regress before Week 5 may be misclassified as non-ramp.\n",
    "2. Quasi-experimental design: no random assignment. Coaches who expand\n",
    "   player roles may be responding to unmeasured performance signals.\n",
    "   Ramp could be effect of role expansion rather than cause of persistence.\n",
    "3. Rookie flag uses years_exp from nflreadr; players with redshirt seasons\n",
    "   may be misclassified. Second-year effects documented as extension.\n",
    "4. Touch share denominator for RB uses team receptions (completions), not\n",
    "   team targets (attempts). This excludes incomplete targets from the\n",
    "   denominator, which understates touch share when completion rate is low.\n\n",

    "{sep}\n"
  )

  as.character(spec)
}


# ==============================================================================
# FUNCTION: identify_rookies
# ==============================================================================

#' Flag Players in Their First NFL Season
#'
#' @description
#' Joins roster data to the classification output and adds an \code{is_rookie}
#' flag for players in their first NFL season (\code{years_exp == 0}).
#'
#' If \code{years_exp} is not available in \code{roster_with_exp}, the function
#' attempts to derive it from \code{entry_year} (entry_year == season implies
#' first-year player). If neither column is present, the function stops with a
#' diagnostic message.
#'
#' Rookie definition: first NFL season only (\code{years_exp == 0}).
#' Second-year players are not included in the rookie flag. The \code{years_exp}
#' column is retained in the output for post-hoc Year 2 analysis in
#' \code{rookie_ramp_heterogeneous_effects()}.
#'
#' @param groups Tibble. Output of \code{classify_treatment_control_ramp()}.
#' @param roster_with_exp Tibble. nflreadr roster data including experience
#'   information. Must contain: gsis_id (or player_id), season, and either
#'   years_exp or entry_year.
#' @param verbose Logical. Default TRUE.
#'
#' @return \code{groups} tibble with two additional columns:
#'   \describe{
#'     \item{is_rookie}{lgl: TRUE if years_exp == 0}
#'     \item{years_exp}{int: years of NFL experience (0 = first season)}
#'   }
#'
#' @seealso \code{\link{stratify_by_rookie_status}},
#'   \code{\link{rookie_ramp_heterogeneous_effects}}
#' @export
identify_rookies <- function(groups, roster_with_exp, verbose = TRUE) {

  stopifnot(
    is.data.frame(groups),
    is.data.frame(roster_with_exp),
    is.logical(verbose)
  )

  # Normalize player_id column name in roster
  if ("gsis_id" %in% names(roster_with_exp) &&
      !"player_id" %in% names(roster_with_exp)) {
    roster_with_exp <- roster_with_exp %>%
      dplyr::rename(player_id = gsis_id)
  }

  if (!"player_id" %in% names(roster_with_exp)) {
    stop(
      "identify_rookies(): roster_with_exp must contain 'player_id' or 'gsis_id'.",
      call. = FALSE
    )
  }

  if (!"season" %in% names(roster_with_exp)) {
    stop("identify_rookies(): roster_with_exp must contain 'season'.",
         call. = FALSE)
  }

  # Determine experience column
  has_years_exp  <- "years_exp"  %in% names(roster_with_exp)
  has_entry_year <- "entry_year" %in% names(roster_with_exp)

  if (!has_years_exp && !has_entry_year) {
    stop(glue(
      "identify_rookies(): roster_with_exp must contain 'years_exp' or ",
      "'entry_year'. Neither column found.\n",
      "Available columns: {paste(names(roster_with_exp), collapse=', ')}"
    ), call. = FALSE)
  }

  # If years_exp not present, derive from entry_year
  if (!has_years_exp) {
    if (verbose) {
      message("  identify_rookies(): years_exp not found, deriving from entry_year.")
    }
    roster_with_exp <- roster_with_exp %>%
      dplyr::mutate(
        years_exp = as.integer(season - entry_year)
      )
  }

  exp_lookup <- roster_with_exp %>%
    dplyr::filter(!is.na(player_id), !is.na(years_exp)) %>%
    dplyr::select(player_id, season, years_exp) %>%
    dplyr::mutate(years_exp = as.integer(years_exp)) %>%
    dplyr::distinct(player_id, season, .keep_all = TRUE)

  groups_out <- groups %>%
    dplyr::left_join(exp_lookup, by = c("player_id", "season")) %>%
    dplyr::mutate(
      is_rookie = !is.na(years_exp) & years_exp == 0L
    )

  n_rookies  <- sum(groups_out$is_rookie & groups_out$group %in%
                      c("treatment", "control"), na.rm = TRUE)
  n_total_tc <- sum(groups_out$group %in% c("treatment", "control"), na.rm = TRUE)
  n_missing  <- sum(is.na(groups_out$years_exp) &
                      groups_out$group %in% c("treatment", "control"))

  if (verbose) {
    message(glue(
      "  identify_rookies(): {format(n_rookies, big.mark=',')} rookies in ",
      "treatment+control | {n_missing} players missing experience data"
    ))
  }

  groups_out
}


# ==============================================================================
# FUNCTION: stratify_by_rookie_status
# ==============================================================================

#' Stratify Players Into Four Groups by Ramp Status and Rookie Status
#'
#' @description
#' Combines the ramp classification (treatment/control) with the rookie flag
#' (\code{is_rookie}) to produce four analytical strata: rookie_ramp,
#' rookie_flat, vet_ramp, vet_flat.
#'
#' @param groups Tibble. Output of \code{identify_rookies()} -- must contain
#'   \code{group}, \code{is_rookie}, \code{years_exp}.
#' @param verbose Logical. Default TRUE.
#'
#' @return \code{groups} tibble with one additional column:
#'   \describe{
#'     \item{rookie_group}{chr: "rookie_ramp", "rookie_flat", "vet_ramp",
#'       "vet_flat", or "excluded" (for declining and excluded group members)}
#'   }
#'
#' @seealso \code{\link{identify_rookies}}, \code{\link{analyze_ramp_by_rookie_status}}
#' @export
stratify_by_rookie_status <- function(groups, verbose = TRUE) {

  stopifnot(is.data.frame(groups), is.logical(verbose))

  required_cols <- c("group", "is_rookie")
  missing_cols  <- setdiff(required_cols, names(groups))
  if (length(missing_cols) > 0L) {
    stop(glue(
      "stratify_by_rookie_status(): groups missing columns: ",
      "{paste(missing_cols, collapse = ', ')}. ",
      "Run identify_rookies() first."
    ), call. = FALSE)
  }

  groups_out <- groups %>%
    dplyr::mutate(
      rookie_group = dplyr::case_when(
        group == "treatment" &  is_rookie ~ "rookie_ramp",
        group == "treatment" & !is_rookie ~ "vet_ramp",
        group == "control"   &  is_rookie ~ "rookie_flat",
        group == "control"   & !is_rookie ~ "vet_flat",
        TRUE                              ~ "excluded"
      )
    )

  if (verbose) {
    counts <- groups_out %>%
      dplyr::filter(rookie_group != "excluded") %>%
      dplyr::count(rookie_group) %>%
      dplyr::arrange(rookie_group)
    message("  stratify_by_rookie_status() counts:")
    for (i in seq_len(nrow(counts))) {
      message(glue("    {counts$rookie_group[[i]]}: ",
                   "{format(counts$n[[i]], big.mark=',')}"))
    }
  }

  groups_out
}


# ==============================================================================
# FUNCTION: analyze_ramp_by_rookie_status
# ==============================================================================

#' Run Primary Ramp Analysis Separately for Rookies and Veterans
#'
#' @description
#' Computes the ramp effect (treatment vs control) within each experience
#' stratum (rookie and veteran). Tests whether the usage ramp signal is stronger
#' or weaker for first-year players compared to established veterans.
#'
#' Primary outcome: mean Weeks 9-18 PPR fantasy points per game.
#' Secondary outcome: mean Weeks 9-18 usage share.
#'
#' @param groups Tibble. Output of \code{stratify_by_rookie_status()}.
#' @param weekly_fantasy Tibble. Player-week PPR scoring from
#'   \code{.compute_weekly_fantasy_season()} aggregated across seasons.
#'   Must contain: player_id, season, week, total_fantasy_points.
#' @param weekly_usage Tibble. Output of \code{calculate_weekly_usage_share()}.
#'   Must contain: player_id, season, week, usage_share.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W12}.
#' @param min_outcome_games Integer. Minimum outcome-window games.
#'   Default \code{MIN_OUTCOME_GAMES_W12}.
#' @param B Integer. Bootstrap resamples. Default \code{BOOTSTRAP_B_W12}.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{rookie_effect}{List. Bootstrap CI result for rookie ramp vs rookie flat.}
#'     \item{vet_effect}{List. Bootstrap CI result for vet ramp vs vet flat.}
#'     \item{stratum_summary}{Tibble. N, mean PPG, SD per rookie_group.}
#'     \item{interaction_test}{List. Welch t-test on (ramp - flat) difference
#'       between rookie and vet strata.}
#'     \item{summary}{chr.}
#'   }
#'
#' @seealso \code{\link{stratify_by_rookie_status}},
#'   \code{\link{rookie_ramp_heterogeneous_effects}}
#' @export
analyze_ramp_by_rookie_status <- function(
    groups,
    weekly_fantasy,
    weekly_usage,
    outcome_weeks    = OUTCOME_WEEKS_W12,
    min_outcome_games = MIN_OUTCOME_GAMES_W12,
    B                = BOOTSTRAP_B_W12,
    verbose          = TRUE
) {

  stopifnot(
    is.data.frame(groups),
    is.data.frame(weekly_fantasy),
    is.data.frame(weekly_usage),
    is.logical(verbose)
  )

  if (!"rookie_group" %in% names(groups)) {
    stop("analyze_ramp_by_rookie_status(): run stratify_by_rookie_status() first.",
         call. = FALSE)
  }

  # --- Build outcome summaries per player-season ---------------------------
  outcome_ppg <- weekly_fantasy %>%
    dplyr::filter(week %in% outcome_weeks) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      outcome_ppg       = mean(total_fantasy_points, na.rm = TRUE),
      n_outcome_games   = dplyr::n(),
      .groups           = "drop"
    ) %>%
    dplyr::filter(n_outcome_games >= min_outcome_games)

  outcome_usage <- weekly_usage %>%
    dplyr::filter(week %in% outcome_weeks, active) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      outcome_usage = mean(usage_share, na.rm = TRUE),
      .groups       = "drop"
    )

  # Join to groups
  analysis_data <- groups %>%
    dplyr::filter(rookie_group != "excluded") %>%
    dplyr::left_join(outcome_ppg,   by = c("player_id", "season")) %>%
    dplyr::left_join(outcome_usage, by = c("player_id", "season")) %>%
    dplyr::filter(!is.na(outcome_ppg))

  # --- Stratum summary -------------------------------------------------------
  stratum_summary <- analysis_data %>%
    dplyr::group_by(rookie_group) %>%
    dplyr::summarise(
      n_players  = dplyr::n(),
      mean_ppg   = mean(outcome_ppg,   na.rm = TRUE),
      sd_ppg     = stats::sd(outcome_ppg,   na.rm = TRUE),
      mean_usage = mean(outcome_usage, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    dplyr::arrange(rookie_group)

  # --- Rookie effect (rookie_ramp vs rookie_flat) ----------------------------
  rookie_trt <- analysis_data$outcome_ppg[
    analysis_data$rookie_group == "rookie_ramp"
  ]
  rookie_ctl <- analysis_data$outcome_ppg[
    analysis_data$rookie_group == "rookie_flat"
  ]

  rookie_effect <- .bootstrap_ramp_diff_ci(rookie_trt, rookie_ctl, B = B)

  # --- Veteran effect (vet_ramp vs vet_flat) ---------------------------------
  vet_trt <- analysis_data$outcome_ppg[
    analysis_data$rookie_group == "vet_ramp"
  ]
  vet_ctl <- analysis_data$outcome_ppg[
    analysis_data$rookie_group == "vet_flat"
  ]

  vet_effect <- .bootstrap_ramp_diff_ci(vet_trt, vet_ctl, B = B)

  # --- Interaction test: is the ramp effect different for rookies vs vets? ---
  # Compute individual player-level difference: ramp - flat within stratum
  # Approximation: compare ramp effect estimates using two-sample t on PPG
  # (treatment within rookie vs treatment within vet)
  r_t <- analysis_data$outcome_ppg[analysis_data$rookie_group == "rookie_ramp"]
  v_t <- analysis_data$outcome_ppg[analysis_data$rookie_group == "vet_ramp"]

  interaction_test <- tryCatch(
    stats::t.test(r_t, v_t),
    error = function(e) NULL
  )

  # --- Summary ---------------------------------------------------------------
  rook_est <- round(rookie_effect$estimate, 2)
  vet_est  <- round(vet_effect$estimate,   2)

  summary_str <- glue(
    "Rookie ramp vs flat: {rook_est} PPG difference ",
    "[{round(rookie_effect$ci_lower,2)}, {round(rookie_effect$ci_upper,2)}]. ",
    "Vet ramp vs flat: {vet_est} PPG difference ",
    "[{round(vet_effect$ci_lower,2)}, {round(vet_effect$ci_upper,2)}]."
  )

  if (verbose) message(summary_str)

  list(
    rookie_effect     = rookie_effect,
    vet_effect        = vet_effect,
    stratum_summary   = stratum_summary,
    interaction_test  = interaction_test,
    summary           = as.character(summary_str)
  )
}


# ==============================================================================
# FUNCTION: rookie_ramp_heterogeneous_effects
# ==============================================================================

#' Explore Within-Rookie Variation in Ramp Effects
#'
#' @description
#' Within the rookie ramp stratum, tests whether the ramp-to-persistence
#' relationship varies by position and era. Also surfaces Year 2 players
#' for a documented extension note.
#'
#' @param groups Tibble. Output of \code{stratify_by_rookie_status()}.
#' @param weekly_fantasy Tibble. Player-week PPR scoring. Must contain:
#'   player_id, season, week, total_fantasy_points.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W12}.
#' @param min_outcome_games Integer. Default \code{MIN_OUTCOME_GAMES_W12}.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{by_position}{Tibble. Mean outcome PPG by position within rookies.}
#'     \item{by_era}{Tibble. Mean outcome PPG by era within rookies.}
#'     \item{year2_note}{chr. Documents Year 2 extension opportunity.}
#'     \item{summary}{chr.}
#'   }
#'
#' @seealso \code{\link{analyze_ramp_by_rookie_status}}
#' @export
rookie_ramp_heterogeneous_effects <- function(
    groups,
    weekly_fantasy,
    outcome_weeks    = OUTCOME_WEEKS_W12,
    min_outcome_games = MIN_OUTCOME_GAMES_W12,
    verbose          = TRUE
) {

  stopifnot(
    is.data.frame(groups),
    is.data.frame(weekly_fantasy),
    is.logical(verbose)
  )

  if (!"rookie_group" %in% names(groups)) {
    stop("rookie_ramp_heterogeneous_effects(): run stratify_by_rookie_status() first.",
         call. = FALSE)
  }

  outcome_ppg <- weekly_fantasy %>%
    dplyr::filter(week %in% outcome_weeks) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      outcome_ppg     = mean(total_fantasy_points, na.rm = TRUE),
      n_outcome_games = dplyr::n(),
      .groups         = "drop"
    ) %>%
    dplyr::filter(n_outcome_games >= min_outcome_games)

  rookie_data <- groups %>%
    dplyr::filter(rookie_group %in% c("rookie_ramp", "rookie_flat")) %>%
    dplyr::left_join(outcome_ppg, by = c("player_id", "season")) %>%
    dplyr::filter(!is.na(outcome_ppg))

  # By position
  by_position <- rookie_data %>%
    dplyr::group_by(rookie_group, position) %>%
    dplyr::summarise(
      n_players = dplyr::n(),
      mean_ppg  = mean(outcome_ppg, na.rm = TRUE),
      sd_ppg    = stats::sd(outcome_ppg, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    dplyr::arrange(position, rookie_group)

  # By era
  by_era <- rookie_data %>%
    dplyr::group_by(rookie_group, era) %>%
    dplyr::summarise(
      n_players = dplyr::n(),
      mean_ppg  = mean(outcome_ppg, na.rm = TRUE),
      sd_ppg    = stats::sd(outcome_ppg, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    dplyr::arrange(era, rookie_group)

  # Year 2 extension note
  n_year2 <- if ("years_exp" %in% names(groups)) {
    sum(groups$years_exp == 1L & groups$group %in% c("treatment", "control"),
        na.rm = TRUE)
  } else 0L

  year2_note <- glue(
    "Extension (not in primary analysis): {format(n_year2, big.mark=',')} ",
    "second-year players in treatment+control. Year 2 breakout analysis can ",
    "be run by filtering years_exp == 1L from the groups output and repeating ",
    "the primary ramp analysis."
  )

  summary_str <- glue(
    "Rookie heterogeneous effects: {nrow(by_position)} position-group cells | ",
    "{nrow(by_era)} era-group cells. {year2_note}"
  )

  if (verbose) message(summary_str)

  list(
    by_position = by_position,
    by_era      = by_era,
    year2_note  = as.character(year2_note),
    summary     = as.character(summary_str)
  )
}


# ==============================================================================
# FUNCTION: calculate_usage_persistence
# ==============================================================================

#' Measure Whether Ramping Players Maintain Elevated Usage in Weeks 9-18
#'
#' @description
#' Computes mean weekly usage share in the outcome window (Weeks 9-18) for
#' treatment and control groups. Tests whether the usage ramp from the training
#' window persists into the second half of the season.
#'
#' This is a mechanism check -- it confirms whether the ramp is a sustained
#' role change (which should predict fantasy output) or a transient fluctuation
#' (which should not).
#'
#' @param groups Tibble. Output of \code{classify_treatment_control_ramp()} or
#'   \code{stratify_by_rookie_status()}.
#' @param weekly_usage Tibble. Output of \code{calculate_weekly_usage_share()}.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W12}.
#' @param min_outcome_games Integer. Default \code{MIN_OUTCOME_GAMES_W12}.
#' @param B Integer. Bootstrap resamples. Default \code{BOOTSTRAP_B_W12}.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{persistence_summary}{Tibble. N, mean outcome usage, SD per group.}
#'     \item{persistence_effect}{List. Bootstrap CI for treatment - control
#'       mean outcome usage share.}
#'     \item{cohen_d_usage}{dbl. Cohen's d for the usage persistence difference.}
#'     \item{summary}{chr.}
#'   }
#'
#' @seealso \code{\link{run_ramp_experiment}}
#' @export
calculate_usage_persistence <- function(
    groups,
    weekly_usage,
    outcome_weeks    = OUTCOME_WEEKS_W12,
    min_outcome_games = MIN_OUTCOME_GAMES_W12,
    B                = BOOTSTRAP_B_W12,
    verbose          = TRUE
) {

  stopifnot(
    is.data.frame(groups),
    is.data.frame(weekly_usage),
    is.logical(verbose)
  )

  # Outcome-window usage per player-season
  outcome_usage <- weekly_usage %>%
    dplyr::filter(week %in% outcome_weeks) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      outcome_usage   = mean(usage_share[active], na.rm = TRUE),
      n_active_out    = sum(active, na.rm = TRUE),
      .groups         = "drop"
    ) %>%
    dplyr::filter(n_active_out >= min_outcome_games)

  analysis_data <- groups %>%
    dplyr::filter(group %in% c("treatment", "control")) %>%
    dplyr::left_join(outcome_usage, by = c("player_id", "season")) %>%
    dplyr::filter(!is.na(outcome_usage))

  persistence_summary <- analysis_data %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      n_players    = dplyr::n(),
      mean_usage   = mean(outcome_usage, na.rm = TRUE),
      sd_usage     = stats::sd(outcome_usage, na.rm = TRUE),
      .groups      = "drop"
    ) %>%
    dplyr::arrange(group)

  trt_usage <- analysis_data$outcome_usage[analysis_data$group == "treatment"]
  ctl_usage <- analysis_data$outcome_usage[analysis_data$group == "control"]

  persistence_effect <- .bootstrap_ramp_diff_ci(trt_usage, ctl_usage, B = B)

  # Cohen's d for usage persistence
  sd_t <- stats::sd(trt_usage, na.rm = TRUE)
  sd_c <- stats::sd(ctl_usage, na.rm = TRUE)
  pooled_sd_val <- if (!is.na(sd_t) && !is.na(sd_c) && (sd_t + sd_c) > 0) {
    sqrt((sd_t^2 + sd_c^2) / 2)
  } else NA_real_

  cohen_d_usage <- if (!is.na(pooled_sd_val) && pooled_sd_val > 0) {
    persistence_effect$estimate / pooled_sd_val
  } else NA_real_

  pe  <- round(persistence_effect$estimate, 4)
  ci1 <- round(persistence_effect$ci_lower, 4)
  ci2 <- round(persistence_effect$ci_upper, 4)
  d_r <- round(cohen_d_usage, 3)

  summary_str <- glue(
    "Usage persistence: treatment - control = {pe} share points ",
    "95% CI [{ci1}, {ci2}]. Cohen's d = {d_r}."
  )

  if (verbose) message(summary_str)

  list(
    persistence_summary = persistence_summary,
    persistence_effect  = persistence_effect,
    cohen_d_usage       = cohen_d_usage,
    summary             = as.character(summary_str)
  )
}


# ==============================================================================
# FUNCTION: run_ramp_experiment
# ==============================================================================

#' Run the Primary Usage Ramp Effect Analysis
#'
#' @description
#' Computes the primary treatment effect: do players who ramped usage in
#' Weeks 1-8 score more PPR fantasy points in Weeks 9-18 than stable-usage
#' players?
#'
#' Reports: group-level PPR summaries, bootstrap CI for the mean difference,
#' Cohen's d effect size, a Welch t-test for reference, and usage persistence.
#'
#' @param groups Tibble. Output of \code{classify_treatment_control_ramp()} or
#'   \code{stratify_by_rookie_status()}.
#' @param weekly_fantasy Tibble. Player-week PPR scoring. Must contain:
#'   player_id, season, week, total_fantasy_points.
#' @param weekly_usage Tibble. Output of \code{calculate_weekly_usage_share()}.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W12}.
#' @param min_outcome_games Integer. Default \code{MIN_OUTCOME_GAMES_W12}.
#' @param B Integer. Bootstrap resamples. Default \code{BOOTSTRAP_B_W12}.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{group_summary}{Tibble. N, mean PPG, SD PPG per group.}
#'     \item{effect_ppg}{List. Bootstrap CI for treatment - control mean PPG.}
#'     \item{cohens_d_ppg}{dbl. Cohen's d for PPG difference.}
#'     \item{t_test_ppg}{htest. Welch t-test result.}
#'     \item{usage_persistence}{List. Output of calculate_usage_persistence().}
#'     \item{summary}{chr.}
#'   }
#'
#' @seealso \code{\link{calculate_usage_persistence}},
#'   \code{\link{analyze_ramp_heterogeneous_effects}}
#' @export
run_ramp_experiment <- function(
    groups,
    weekly_fantasy,
    weekly_usage,
    outcome_weeks    = OUTCOME_WEEKS_W12,
    min_outcome_games = MIN_OUTCOME_GAMES_W12,
    B                = BOOTSTRAP_B_W12,
    verbose          = TRUE
) {

  stopifnot(
    is.data.frame(groups),
    is.data.frame(weekly_fantasy),
    is.data.frame(weekly_usage),
    is.logical(verbose)
  )

  required_fantasy <- c("player_id", "season", "week", "total_fantasy_points")
  missing_f <- setdiff(required_fantasy, names(weekly_fantasy))
  if (length(missing_f) > 0L) {
    stop(glue(
      "run_ramp_experiment(): weekly_fantasy missing columns: ",
      "{paste(missing_f, collapse=', ')}"
    ), call. = FALSE)
  }

  if (verbose) message("run_ramp_experiment(): computing outcome-window PPR by group...")

  # --- Outcome-window PPG per player-season ----------------------------------
  outcome_ppg <- weekly_fantasy %>%
    dplyr::filter(week %in% outcome_weeks) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      outcome_ppg     = mean(total_fantasy_points, na.rm = TRUE),
      n_outcome_games = dplyr::n(),
      .groups         = "drop"
    ) %>%
    dplyr::filter(n_outcome_games >= min_outcome_games)

  analysis_data <- groups %>%
    dplyr::filter(group %in% c("treatment", "control")) %>%
    dplyr::left_join(outcome_ppg, by = c("player_id", "season")) %>%
    dplyr::filter(!is.na(outcome_ppg))

  if (nrow(analysis_data) == 0L) {
    message("run_ramp_experiment(): no players met outcome criteria.")
    return(list(summary = "No players met outcome criteria."))
  }

  # --- Group summary ---------------------------------------------------------
  group_summary <- analysis_data %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      n_players  = dplyr::n(),
      mean_ppg   = mean(outcome_ppg, na.rm = TRUE),
      sd_ppg     = stats::sd(outcome_ppg, na.rm = TRUE),
      median_ppg = stats::median(outcome_ppg, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    dplyr::arrange(group)

  # --- Bootstrap CI for treatment - control mean PPG ------------------------
  trt_ppg <- analysis_data$outcome_ppg[analysis_data$group == "treatment"]
  ctl_ppg <- analysis_data$outcome_ppg[analysis_data$group == "control"]

  effect_ppg <- .bootstrap_ramp_diff_ci(trt_ppg, ctl_ppg, B = B)

  # --- Cohen's d ------------------------------------------------------------
  sd_t <- stats::sd(trt_ppg, na.rm = TRUE)
  sd_c <- stats::sd(ctl_ppg, na.rm = TRUE)
  pooled_sd_val <- if (!is.na(sd_t) && !is.na(sd_c) && (sd_t + sd_c) > 0) {
    sqrt((sd_t^2 + sd_c^2) / 2)
  } else NA_real_

  cohens_d_ppg <- if (!is.na(pooled_sd_val) && pooled_sd_val > 0) {
    effect_ppg$estimate / pooled_sd_val
  } else NA_real_

  # --- Welch t-test reference -----------------------------------------------
  t_test_ppg <- tryCatch(
    stats::t.test(trt_ppg, ctl_ppg),
    error = function(e) NULL
  )

  # --- Usage persistence (mechanism check) ----------------------------------
  usage_persistence <- calculate_usage_persistence(
    groups         = groups,
    weekly_usage   = weekly_usage,
    outcome_weeks  = outcome_weeks,
    min_outcome_games = min_outcome_games,
    B              = B,
    verbose        = verbose
  )

  # --- Summary ---------------------------------------------------------------
  est   <- round(effect_ppg$estimate, 2)
  ci1   <- round(effect_ppg$ci_lower, 2)
  ci2   <- round(effect_ppg$ci_upper, 2)
  d_r   <- round(cohens_d_ppg, 3)
  n_trt <- effect_ppg$n_a
  n_ctl <- effect_ppg$n_b

  pval_str <- if (!is.null(t_test_ppg)) {
    round(t_test_ppg$p.value, 4)
  } else "N/A"

  direction <- if (!is.na(est) && est > 0) {
    "treatment scored more"
  } else if (!is.na(est) && est < 0) {
    "control scored more"
  } else "no direction"

  summary_str <- glue(
    "Primary effect: treatment - control = {est} PPG ",
    "95% CI [{ci1}, {ci2}]. p = {pval_str}. ",
    "Cohen's d = {d_r}. N: {n_trt} treatment, {n_ctl} control. ",
    "Direction: {direction}."
  )

  if (verbose) message(summary_str)

  list(
    group_summary     = group_summary,
    effect_ppg        = effect_ppg,
    cohens_d_ppg      = cohens_d_ppg,
    t_test_ppg        = t_test_ppg,
    usage_persistence = usage_persistence,
    summary           = as.character(summary_str)
  )
}


# ==============================================================================
# FUNCTION: analyze_ramp_heterogeneous_effects
# ==============================================================================

#' Analyze Variation in the Ramp Effect by Position, Ramp Size, and Era
#'
#' @description
#' Tests whether the treatment effect (treatment - control PPG) is larger for
#' certain positions, ramp magnitudes, or historical eras.
#'
#' Subgroup analyses:
#' \itemize{
#'   \item By position (RB, WR, TE)
#'   \item By ramp size quintile (Q1-Q5 of relative_change in treatment group)
#'   \item By era (early vs modern, split at \code{ERA_BREAKPOINT_W12})
#' }
#'
#' All subgroup effects use bootstrap CI. BH FDR correction applied across
#' subgroup t-test p-values to account for multiple comparisons.
#'
#' @param groups Tibble. Output of \code{classify_treatment_control_ramp()}.
#' @param weekly_fantasy Tibble. Player-week PPR scoring.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W12}.
#' @param min_outcome_games Integer. Default \code{MIN_OUTCOME_GAMES_W12}.
#' @param B Integer. Bootstrap resamples. Default \code{BOOTSTRAP_B_W12}.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{by_position}{Tibble. Effect estimate per position.}
#'     \item{by_ramp_size}{Tibble. Effect by ramp quintile (treatment only).}
#'     \item{by_era}{Tibble. Effect per era.}
#'     \item{p_adjusted}{Tibble. BH-corrected p-values across all subgroups.}
#'     \item{summary}{chr.}
#'   }
#'
#' @seealso \code{\link{run_ramp_experiment}}, \code{\link{ramp_robustness_check}}
#' @export
analyze_ramp_heterogeneous_effects <- function(
    groups,
    weekly_fantasy,
    outcome_weeks    = OUTCOME_WEEKS_W12,
    min_outcome_games = MIN_OUTCOME_GAMES_W12,
    B                = BOOTSTRAP_B_W12,
    verbose          = TRUE
) {

  stopifnot(
    is.data.frame(groups),
    is.data.frame(weekly_fantasy),
    is.logical(verbose)
  )

  # Outcome PPG per player-season
  outcome_ppg <- weekly_fantasy %>%
    dplyr::filter(week %in% outcome_weeks) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      outcome_ppg     = mean(total_fantasy_points, na.rm = TRUE),
      n_outcome_games = dplyr::n(),
      .groups         = "drop"
    ) %>%
    dplyr::filter(n_outcome_games >= min_outcome_games)

  base_data <- groups %>%
    dplyr::filter(group %in% c("treatment", "control")) %>%
    dplyr::left_join(outcome_ppg, by = c("player_id", "season")) %>%
    dplyr::filter(!is.na(outcome_ppg))

  # --- Helper: compute effect for a subgroup ---------------------------------
  compute_subgroup_effect <- function(df, label) {
    trt <- df$outcome_ppg[df$group == "treatment"]
    ctl <- df$outcome_ppg[df$group == "control"]
    if (length(trt) < 3L || length(ctl) < 3L) {
      return(dplyr::tibble(
        subgroup = label, n_trt = length(trt), n_ctl = length(ctl),
        estimate = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
        p_value = NA_real_
      ))
    }
    res  <- .bootstrap_ramp_diff_ci(trt, ctl, B = B)
    tt   <- tryCatch(stats::t.test(trt, ctl), error = function(e) NULL)
    pval <- if (!is.null(tt)) tt$p.value else NA_real_
    dplyr::tibble(
      subgroup = label,
      n_trt    = length(trt),
      n_ctl    = length(ctl),
      estimate = res$estimate,
      ci_lower = res$ci_lower,
      ci_upper = res$ci_upper,
      p_value  = pval
    )
  }

  # --- By position -----------------------------------------------------------
  positions <- sort(unique(base_data$position))
  by_position <- purrr::map_dfr(positions, function(pos) {
    compute_subgroup_effect(
      df    = dplyr::filter(base_data, position == pos),
      label = pos
    )
  })

  # --- By era ----------------------------------------------------------------
  eras <- sort(unique(base_data$era))
  by_era <- purrr::map_dfr(eras, function(e) {
    compute_subgroup_effect(
      df    = dplyr::filter(base_data, era == e),
      label = e
    )
  })

  # --- By ramp size quintile (treatment group only) --------------------------
  trt_data <- base_data %>%
    dplyr::filter(group == "treatment", !is.na(relative_change)) %>%
    dplyr::mutate(
      ramp_size_quintile = dplyr::ntile(relative_change, 5L)
    )

  by_ramp_size <- purrr::map_dfr(1L:5L, function(q) {
    players_in_q <- trt_data$player_id[trt_data$ramp_size_quintile == q]
    df_q <- base_data %>%
      dplyr::filter(
        (group == "treatment" & player_id %in% players_in_q) |
        group == "control"
      )
    compute_subgroup_effect(
      df    = df_q,
      label = paste0("ramp_Q", q)
    )
  })

  # --- BH FDR correction across all subgroup p-values ----------------------
  all_p <- c(
    stats::setNames(by_position$p_value, paste0("pos_", by_position$subgroup)),
    stats::setNames(by_era$p_value,      paste0("era_", by_era$subgroup)),
    stats::setNames(by_ramp_size$p_value, by_ramp_size$subgroup)
  )

  valid_p   <- all_p[!is.na(all_p)]
  p_adj_vec <- stats::p.adjust(valid_p, method = "BH")

  p_adjusted <- dplyr::tibble(
    subgroup  = names(all_p),
    p_raw     = as.numeric(all_p),
    p_bh      = stats::p.adjust(as.numeric(all_p), method = "BH")
  )

  n_sig <- sum(p_adjusted$p_bh < 0.10, na.rm = TRUE)
  summary_str <- glue(
    "Heterogeneous effects: {nrow(by_position)} positions | ",
    "{nrow(by_era)} eras | {nrow(by_ramp_size)} ramp quintiles. ",
    "{n_sig} subgroups significant at BH q < 0.10."
  )

  if (verbose) message(summary_str)

  list(
    by_position = by_position,
    by_ramp_size = by_ramp_size,
    by_era      = by_era,
    p_adjusted  = p_adjusted,
    summary     = as.character(summary_str)
  )
}


# ==============================================================================
# FUNCTION: ramp_robustness_check
# ==============================================================================

#' Sensitivity Analysis for the Ramp Experiment
#'
#' @description
#' Sweeps two robustness dimensions:
#' \enumerate{
#'   \item \strong{Method}: \code{"sub_window"} (primary, 3-of-4 games floor)
#'     vs \code{"full_split"} (relaxed 1-of-4 floor).
#'   \item \strong{Threshold}: 5\%, 10\%, 15\% relative change required for
#'     treatment classification.
#' }
#'
#' For each combination, re-classifies groups and re-runs the primary effect
#' analysis. Returns a grid of results showing whether the finding is robust
#' across operationalizations.
#'
#' @param weekly_usage Tibble. Output of \code{calculate_weekly_usage_share()}.
#' @param weekly_fantasy Tibble. Player-week PPR scoring.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W12}.
#' @param analysis_seasons Integer vector. Default \code{ANALYSIS_SEASONS_W12}.
#' @param B Integer. Bootstrap resamples per spec. Default 500L (reduced for
#'   grid search efficiency).
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{results_table}{Tibble. One row per (method, threshold) combination.
#'       Columns: method, threshold, n_treatment, n_control, effect_estimate,
#'       ci_lower, ci_upper, cohens_d, direction_consistent.}
#'     \item{summary}{chr. Whether findings are consistent across specs.}
#'   }
#'
#' @seealso \code{\link{identify_usage_ramps}},
#'   \code{\link{classify_treatment_control_ramp}}
#' @export
ramp_robustness_check <- function(
    weekly_usage,
    weekly_fantasy,
    outcome_weeks   = OUTCOME_WEEKS_W12,
    analysis_seasons = ANALYSIS_SEASONS_W12,
    B               = 500L,
    verbose         = TRUE
) {

  stopifnot(
    is.data.frame(weekly_usage),
    is.data.frame(weekly_fantasy),
    is.logical(verbose)
  )

  methods     <- c("sub_window", "full_split")
  thresholds  <- c(0.05, 0.10, 0.15)
  spec_grid   <- expand.grid(
    method    = methods,
    threshold = thresholds,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    message(glue(
      "ramp_robustness_check(): sweeping {nrow(spec_grid)} method-threshold combinations..."
    ))
  }

  outcome_ppg <- weekly_fantasy %>%
    dplyr::filter(week %in% outcome_weeks) %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      outcome_ppg     = mean(total_fantasy_points, na.rm = TRUE),
      n_outcome_games = dplyr::n(),
      .groups         = "drop"
    ) %>%
    dplyr::filter(n_outcome_games >= MIN_OUTCOME_GAMES_W12)

  results_rows <- purrr::map_dfr(
    seq_len(nrow(spec_grid)),
    function(i) {
      meth  <- spec_grid$method[[i]]
      thresh <- spec_grid$threshold[[i]]

      if (verbose) {
        message(glue("  method={meth}, threshold={thresh * 100}%"))
      }

      ramp_flags <- tryCatch(
        identify_usage_ramps(
          weekly_usage   = weekly_usage,
          method         = meth,
          ramp_threshold = thresh,
          verbose        = FALSE
        ),
        error = function(e) NULL
      )

      if (is.null(ramp_flags) || nrow(ramp_flags) == 0L) {
        return(dplyr::tibble(
          method = meth, threshold = thresh,
          n_treatment = 0L, n_control = NA_integer_,
          effect_estimate = NA_real_, ci_lower = NA_real_,
          ci_upper = NA_real_, cohens_d = NA_real_,
          direction_consistent = NA
        ))
      }

      classified <- tryCatch(
        classify_treatment_control_ramp(
          ramp_flags     = ramp_flags,
          ramp_threshold = thresh,
          seasons        = analysis_seasons,
          verbose        = FALSE
        ),
        error = function(e) NULL
      )

      if (is.null(classified)) {
        return(dplyr::tibble(
          method = meth, threshold = thresh,
          n_treatment = NA_integer_, n_control = NA_integer_,
          effect_estimate = NA_real_, ci_lower = NA_real_,
          ci_upper = NA_real_, cohens_d = NA_real_,
          direction_consistent = NA
        ))
      }

      n_trt <- sum(classified$group == "treatment", na.rm = TRUE)
      n_ctl <- sum(classified$group == "control",   na.rm = TRUE)

      if (n_trt < 5L || n_ctl < 5L) {
        return(dplyr::tibble(
          method = meth, threshold = thresh,
          n_treatment = n_trt, n_control = n_ctl,
          effect_estimate = NA_real_, ci_lower = NA_real_,
          ci_upper = NA_real_, cohens_d = NA_real_,
          direction_consistent = NA
        ))
      }

      analysis_data <- classified %>%
        dplyr::filter(group %in% c("treatment", "control")) %>%
        dplyr::left_join(outcome_ppg, by = c("player_id", "season")) %>%
        dplyr::filter(!is.na(outcome_ppg))

      trt_ppg <- analysis_data$outcome_ppg[analysis_data$group == "treatment"]
      ctl_ppg <- analysis_data$outcome_ppg[analysis_data$group == "control"]

      if (length(trt_ppg) < 3L || length(ctl_ppg) < 3L) {
        return(dplyr::tibble(
          method = meth, threshold = thresh,
          n_treatment = length(trt_ppg), n_control = length(ctl_ppg),
          effect_estimate = NA_real_, ci_lower = NA_real_,
          ci_upper = NA_real_, cohens_d = NA_real_,
          direction_consistent = NA
        ))
      }

      res <- tryCatch(
        .bootstrap_ramp_diff_ci(trt_ppg, ctl_ppg, B = B),
        error = function(e) NULL
      )

      if (is.null(res)) {
        return(dplyr::tibble(
          method = meth, threshold = thresh,
          n_treatment = length(trt_ppg), n_control = length(ctl_ppg),
          effect_estimate = NA_real_, ci_lower = NA_real_,
          ci_upper = NA_real_, cohens_d = NA_real_,
          direction_consistent = NA
        ))
      }

      sd_t <- stats::sd(trt_ppg, na.rm = TRUE)
      sd_c <- stats::sd(ctl_ppg, na.rm = TRUE)
      pooled_sd_val <- if (!is.na(sd_t) && !is.na(sd_c) && (sd_t + sd_c) > 0) {
        sqrt((sd_t^2 + sd_c^2) / 2)
      } else NA_real_

      d_val <- if (!is.na(pooled_sd_val) && pooled_sd_val > 0) {
        res$estimate / pooled_sd_val
      } else NA_real_

      dplyr::tibble(
        method          = meth,
        threshold       = thresh,
        n_treatment     = as.integer(res$n_a),
        n_control       = as.integer(res$n_b),
        effect_estimate = res$estimate,
        ci_lower        = res$ci_lower,
        ci_upper        = res$ci_upper,
        cohens_d        = d_val,
        direction_consistent = NA  # filled below
      )
    }
  )

  # Tag direction consistency (positive vs negative vs null)
  valid_rows <- results_rows %>% dplyr::filter(!is.na(effect_estimate))
  if (nrow(valid_rows) > 0L) {
    dominant_direction <- if (mean(valid_rows$effect_estimate > 0) > 0.7) {
      "positive"
    } else if (mean(valid_rows$effect_estimate < 0) > 0.7) {
      "negative"
    } else {
      "mixed"
    }
    results_rows <- results_rows %>%
      dplyr::mutate(
        direction_consistent = dplyr::if_else(
          !is.na(effect_estimate),
          (dominant_direction == "positive" & effect_estimate > 0) |
            (dominant_direction == "negative" & effect_estimate < 0),
          NA
        )
      )
  }

  n_consistent <- sum(results_rows$direction_consistent, na.rm = TRUE)
  n_valid      <- sum(!is.na(results_rows$effect_estimate))

  summary_str <- glue(
    "Robustness check: {n_valid}/{nrow(results_rows)} specs produced estimable effects. ",
    "{n_consistent}/{n_valid} specs consistent in direction. ",
    "Effect range: [{round(min(valid_rows$effect_estimate, na.rm=TRUE),2)}, ",
    "{round(max(valid_rows$effect_estimate, na.rm=TRUE),2)}] PPG."
  )

  if (verbose) message(summary_str)

  list(
    results_table = results_rows,
    summary       = as.character(summary_str)
  )
}


# ==============================================================================
# FUNCTION: create_ramp_experiment_report
# ==============================================================================

#' Generate a Formatted Results Report for the Usage Ramp Experiment
#'
#' @description
#' Summarizes the full pipeline output into a human-readable character string
#' suitable for writing to disk, sharing, or incorporating into the docs
#' session deliverables.
#'
#' @param groups Tibble. Output of \code{stratify_by_rookie_status()}.
#' @param effects List. Output of \code{run_ramp_experiment()}.
#' @param rookie_effects List. Output of \code{analyze_ramp_by_rookie_status()}.
#' @param heterogeneous List. Output of \code{analyze_ramp_heterogeneous_effects()}.
#' @param robustness List. Output of \code{ramp_robustness_check()}.
#' @param spec Character. Output of \code{create_ramp_experiment_specification()}.
#'
#' @return Character string. Multi-line formatted results report.
#'
#' @seealso \code{\link{run_week12_pipeline}}
#' @export
create_ramp_experiment_report <- function(
    groups,
    effects,
    rookie_effects,
    heterogeneous,
    robustness,
    spec
) {

  stopifnot(
    is.data.frame(groups),
    is.list(effects),
    is.list(rookie_effects),
    is.list(heterogeneous),
    is.list(robustness)
  )

  sep  <- strrep("=", 60)
  sep2 <- strrep("-", 60)

  est <- round(effects$effect_ppg$estimate, 2)
  ci1 <- round(effects$effect_ppg$ci_lower, 2)
  ci2 <- round(effects$effect_ppg$ci_upper, 2)
  d_r <- round(effects$cohens_d_ppg, 3)
  n_t <- effects$effect_ppg$n_a
  n_c <- effects$effect_ppg$n_b

  rook_est <- round(rookie_effects$rookie_effect$estimate, 2)
  vet_est  <- round(rookie_effects$vet_effect$estimate,   2)

  persist_est <- round(effects$usage_persistence$persistence_effect$estimate, 4)

  report <- glue(
    "{sep}\n",
    "USAGE RAMP EXPERIMENT -- RESULTS REPORT\n",
    "Season 2, Week 12\n",
    "Schema: {SCHEMA_TAG_W12}\n",
    "{sep}\n\n",

    "PRIMARY EFFECT (PPR Fantasy Points)\n",
    "{sep2}\n",
    "Treatment - Control mean PPG = {est}\n",
    "95% CI: [{ci1}, {ci2}]\n",
    "Cohen's d = {d_r}\n",
    "N: {format(n_t, big.mark=',')} treatment | ",
    "{format(n_c, big.mark=',')} control\n\n",

    "USAGE PERSISTENCE (mechanism check)\n",
    "{sep2}\n",
    "Treatment - Control mean Wks 9-18 usage = {persist_est} share points\n",
    "{effects$usage_persistence$summary}\n\n",

    "ROOKIE vs VETERAN SUBGROUP\n",
    "{sep2}\n",
    "Rookie ramp vs flat: {rook_est} PPG\n",
    "Veteran ramp vs flat: {vet_est} PPG\n",
    "{rookie_effects$summary}\n\n",

    "HETEROGENEOUS EFFECTS\n",
    "{sep2}\n",
    "{heterogeneous$summary}\n\n",

    "BY POSITION:\n",
    paste(
      glue_data(
        heterogeneous$by_position,
        "  {subgroup}: {round(estimate,2)} PPG ",
        "(N trt={n_trt}, N ctl={n_ctl})"
      ),
      collapse = "\n"
    ),
    "\n\n",

    "BY ERA:\n",
    paste(
      glue_data(
        heterogeneous$by_era,
        "  {subgroup}: {round(estimate,2)} PPG ",
        "(N trt={n_trt}, N ctl={n_ctl})"
      ),
      collapse = "\n"
    ),
    "\n\n",

    "ROBUSTNESS\n",
    "{sep2}\n",
    "{robustness$summary}\n\n",

    "{sep}\n"
  )

  as.character(report)
}


# ==============================================================================
# FUNCTION: run_week12_pipeline
# ==============================================================================

#' Run the Complete Week 12 Usage Ramp Experiment Pipeline
#'
#' @description
#' End-to-end wrapper that loads all season data, builds the weekly usage and
#' fantasy scoring records, runs all experiment functions in sequence, and
#' returns a named list with all intermediate and final results.
#'
#' Season-by-season loading is used throughout to constrain peak memory.
#' Each season's PBP is loaded, processed, and freed before the next is
#' loaded. \code{gc()} is called after each season.
#'
#' @param seasons Integer vector. Seasons to load for PBP data.
#'   Default: \code{SEASONS_W12} (2010-2025).
#' @param analysis_seasons Integer vector. Seasons to include in the experiment.
#'   Default: \code{ANALYSIS_SEASONS_W12} (2010-2025).
#' @param cache_dir Character. Cache directory for \code{load_normalized_season()}.
#'   Default: \code{CACHE_DIR_W12}.
#' @param training_weeks Integer vector. Default \code{TRAINING_WEEKS_W12}.
#' @param outcome_weeks Integer vector. Default \code{OUTCOME_WEEKS_W12}.
#' @param ramp_threshold Numeric. Default \code{RAMP_THRESHOLD_W12} (0.10).
#' @param save_outputs Logical. Write result RDS files to cache_dir.
#'   Default TRUE.
#' @param verbose Logical. Default TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{weekly_usage}{Tibble. Per-player, per-week usage share.}
#'     \item{weekly_fantasy}{Tibble. Per-player, per-week PPR scoring.}
#'     \item{roster_positions}{Tibble. Player-season position lookup.}
#'     \item{ramp_flags}{Tibble. Output of identify_usage_ramps().}
#'     \item{groups}{Tibble. Output of stratify_by_rookie_status().}
#'     \item{balance}{List. Output of check_balance_ramp_groups().}
#'     \item{assumptions}{List. Output of validate_ramp_assumptions().}
#'     \item{spec}{chr. Output of create_ramp_experiment_specification().}
#'     \item{effects}{List. Output of run_ramp_experiment().}
#'     \item{rookie_effects}{List. Output of analyze_ramp_by_rookie_status().}
#'     \item{heterogeneous}{List. Output of analyze_ramp_heterogeneous_effects().}
#'     \item{robustness}{List. Output of ramp_robustness_check().}
#'     \item{report}{chr. Output of create_ramp_experiment_report().}
#'   }
#'
#' @seealso \code{\link{calculate_weekly_usage_share}},
#'   \code{\link{run_ramp_experiment}}
#' @export
run_week12_pipeline <- function(
    seasons          = SEASONS_W12,
    analysis_seasons = ANALYSIS_SEASONS_W12,
    cache_dir        = CACHE_DIR_W12,
    training_weeks   = TRAINING_WEEKS_W12,
    outcome_weeks    = OUTCOME_WEEKS_W12,
    ramp_threshold   = RAMP_THRESHOLD_W12,
    save_outputs     = TRUE,
    verbose          = TRUE
) {

  seasons          <- sort(as.integer(seasons))
  analysis_seasons <- sort(as.integer(analysis_seasons))

  sep <- strrep("=", 60)

  if (verbose) {
    message(glue(
      "\n{sep}\n",
      "Week 12 Pipeline: Usage Ramp Experiment + Rookie Subgroup Analysis\n",
      "Seasons: {min(seasons)}-{max(seasons)} (PBP load)\n",
      "Analysis seasons: {min(analysis_seasons)}-{max(analysis_seasons)}\n",
      "Training window: Weeks {min(training_weeks)}-{max(training_weeks)}\n",
      "Outcome window:  Weeks {min(outcome_weeks)}-{max(outcome_weeks)}\n",
      "Ramp threshold:  {ramp_threshold * 100}% relative\n",
      "Games floor:     {MIN_ACTIVE_WEEKS_SUBWINDOW_W12} of 4 weeks per sub-window\n",
      "{sep}"
    ))
  }

  # --------------------------------------------------------------------------
  # Step 1: Load rosters for position and experience classification
  # --------------------------------------------------------------------------
  if (verbose) message("\n[1/9] Loading roster data (position + experience)...")

  roster_raw <- tryCatch(
    nflreadr::load_rosters(seasons = analysis_seasons),
    error = function(e) {
      stop(glue(
        "nflreadr::load_rosters() failed: {conditionMessage(e)}\n",
        "Check network connection and nflreadr version."
      ), call. = FALSE)
    }
  )

  # Verify minimum required columns
  required_roster_cols <- c("gsis_id", "position", "season")
  missing_roster_cols  <- setdiff(required_roster_cols, names(roster_raw))
  if (length(missing_roster_cols) > 0L) {
    stop(glue(
      "nflreadr::load_rosters() missing required columns: ",
      "{paste(missing_roster_cols, collapse=', ')}"
    ), call. = FALSE)
  }

  # Check for experience column (years_exp preferred; entry_year as fallback)
  has_years_exp  <- "years_exp"  %in% names(roster_raw)
  has_entry_year <- "entry_year" %in% names(roster_raw)

  if (!has_years_exp && !has_entry_year) {
    stop(glue(
      "nflreadr::load_rosters() does not contain 'years_exp' or 'entry_year'.\n",
      "Available columns: {paste(names(roster_raw), collapse=', ')}"
    ), call. = FALSE)
  }

  # Build position lookup (for usage computation)
  roster_positions <- roster_raw %>%
    dplyr::filter(!is.na(gsis_id), !is.na(position)) %>%
    dplyr::select(player_id = gsis_id, season, position) %>%
    dplyr::filter(position %in% SKILL_POSITIONS_W12) %>%
    dplyr::distinct(player_id, season, position)

  # Build experience lookup (for rookie classification)
  exp_cols <- c("gsis_id", "season",
                if (has_years_exp)  "years_exp"  else NULL,
                if (has_entry_year) "entry_year" else NULL)

  roster_with_exp <- roster_raw %>%
    dplyr::filter(!is.na(gsis_id)) %>%
    dplyr::select(dplyr::all_of(exp_cols)) %>%
    dplyr::distinct(gsis_id, season, .keep_all = TRUE)

  if (verbose) {
    message(glue(
      "  {format(nrow(roster_positions), big.mark=',')} skill-position ",
      "player-seasons loaded. Experience column: ",
      "{if (has_years_exp) 'years_exp' else 'entry_year (derived)'}"
    ))
  }

  # --------------------------------------------------------------------------
  # Step 2: Season-by-season PBP loop
  #   Builds weekly_usage and weekly_fantasy across all seasons
  # --------------------------------------------------------------------------
  if (verbose) {
    message(glue("\n[2/9] Loading PBP and computing usage + scoring ",
                 "for {length(seasons)} seasons..."))
  }

  usage_list   <- vector("list", length(seasons))
  fantasy_list <- vector("list", length(seasons))

  for (i in seq_along(seasons)) {
    s <- seasons[[i]]
    if (verbose) message(glue("  Season {s} ({i}/{length(seasons)})..."))

    roster_s <- roster_positions %>%
      dplyr::filter(season == s)

    if (nrow(roster_s) == 0L) {
      if (verbose) message(glue("    No roster data for {s}, skipping."))
      gc()
      next
    }

    pbp <- tryCatch(
      load_normalized_season(s, cache_dir = cache_dir),
      error = function(e) {
        message(glue("    WARNING: Could not load season {s}: {conditionMessage(e)}"))
        NULL
      }
    )

    if (is.null(pbp)) {
      gc()
      next
    }

    usage_list[[i]] <- tryCatch(
      .compute_weekly_usage_season(
        pbp              = pbp,
        roster_positions = roster_s,
        skill_positions  = SKILL_POSITIONS_W12
      ),
      error = function(e) {
        message(glue("    WARNING: usage computation failed for {s}: {conditionMessage(e)}"))
        NULL
      }
    )

    # Weekly fantasy: roster_data = NULL -- R/17 infers position from play data.
    # Position filtering uses roster_positions from Step 1. This avoids a
    # second per-season network call and keeps peak memory low.
    fantasy_s <- tryCatch({
      scores_raw <- calculate_fantasy_points_ext(pbp, roster_data = NULL)
      scores_raw %>%
        dplyr::select(
          season, week, game_id, player_id, player_name,
          position, team, total_fantasy_points
        )
    }, error = function(e) {
      message(glue("    WARNING: fantasy scoring failed for {s}: {conditionMessage(e)}"))
      NULL
    })

    fantasy_list[[i]] <- fantasy_s

    rm(pbp)
    gc()
  }

  weekly_usage   <- dplyr::bind_rows(purrr::compact(usage_list))
  weekly_fantasy <- dplyr::bind_rows(purrr::compact(fantasy_list))

  rm(usage_list, fantasy_list)
  gc()

  if (verbose) {
    message(glue(
      "  Built:\n",
      "    weekly_usage:   {format(nrow(weekly_usage),   big.mark=',')} rows\n",
      "    weekly_fantasy: {format(nrow(weekly_fantasy), big.mark=',')} rows"
    ))
  }

  # --------------------------------------------------------------------------
  # Step 3: Identify usage ramps (primary method)
  # --------------------------------------------------------------------------
  if (verbose) message("\n[3/9] Identifying usage ramps (sub_window method)...")

  training_usage <- weekly_usage %>%
    dplyr::filter(week %in% training_weeks,
                  season %in% analysis_seasons)

  ramp_flags <- identify_usage_ramps(
    weekly_usage   = training_usage,
    method         = "sub_window",
    ramp_threshold = ramp_threshold,
    verbose        = verbose
  )

  # --------------------------------------------------------------------------
  # Step 4: Classify treatment / control / declining / excluded
  # --------------------------------------------------------------------------
  if (verbose) message("\n[4/9] Classifying treatment, control, declining, excluded...")

  classified <- classify_treatment_control_ramp(
    ramp_flags     = ramp_flags,
    ramp_threshold = ramp_threshold,
    seasons        = analysis_seasons,
    verbose        = verbose
  )

  # --------------------------------------------------------------------------
  # Step 5: Identify rookies and stratify
  # --------------------------------------------------------------------------
  if (verbose) message("\n[5/9] Identifying rookies and stratifying by experience...")

  with_rookies <- identify_rookies(
    groups          = classified,
    roster_with_exp = roster_with_exp,
    verbose         = verbose
  )

  groups <- stratify_by_rookie_status(with_rookies, verbose = verbose)

  # --------------------------------------------------------------------------
  # Step 6: Balance check + assumption validation
  # --------------------------------------------------------------------------
  if (verbose) message("\n[6/9] Balance check and assumption validation...")

  balance     <- check_balance_ramp_groups(groups, verbose = verbose)
  assumptions <- validate_ramp_assumptions(
    groups       = groups,
    weekly_usage = weekly_usage,
    verbose      = verbose
  )

  # Experiment specification
  spec <- create_ramp_experiment_specification(
    groups         = groups,
    balance        = balance,
    assumptions    = assumptions,
    training_weeks = training_weeks,
    outcome_weeks  = outcome_weeks,
    ramp_threshold = ramp_threshold,
    method         = "sub_window"
  )
  if (verbose) cat(spec)

  # --------------------------------------------------------------------------
  # Step 7: Primary effect analysis
  # --------------------------------------------------------------------------
  if (verbose) message("\n[7/9] Running primary effect analysis...")

  effects <- run_ramp_experiment(
    groups         = groups,
    weekly_fantasy = weekly_fantasy,
    weekly_usage   = weekly_usage,
    outcome_weeks  = outcome_weeks,
    verbose        = verbose
  )

  # --------------------------------------------------------------------------
  # Step 8: Rookie subgroup + heterogeneous effects
  # --------------------------------------------------------------------------
  if (verbose) message("\n[8/9] Rookie subgroup and heterogeneous effects...")

  rookie_effects <- analyze_ramp_by_rookie_status(
    groups         = groups,
    weekly_fantasy = weekly_fantasy,
    weekly_usage   = weekly_usage,
    outcome_weeks  = outcome_weeks,
    verbose        = verbose
  )

  rookie_het <- rookie_ramp_heterogeneous_effects(
    groups         = groups,
    weekly_fantasy = weekly_fantasy,
    outcome_weeks  = outcome_weeks,
    verbose        = verbose
  )

  heterogeneous <- analyze_ramp_heterogeneous_effects(
    groups         = groups,
    weekly_fantasy = weekly_fantasy,
    outcome_weeks  = outcome_weeks,
    verbose        = verbose
  )

  # --------------------------------------------------------------------------
  # Step 9: Robustness checks
  # --------------------------------------------------------------------------
  if (verbose) message("\n[9/9] Running robustness checks...")

  robustness <- ramp_robustness_check(
    weekly_usage     = training_usage,
    weekly_fantasy   = weekly_fantasy,
    outcome_weeks    = outcome_weeks,
    analysis_seasons = analysis_seasons,
    B                = 500L,
    verbose          = verbose
  )

  # Final report
  report <- create_ramp_experiment_report(
    groups         = groups,
    effects        = effects,
    rookie_effects = rookie_effects,
    heterogeneous  = heterogeneous,
    robustness     = robustness,
    spec           = spec
  )

  # --------------------------------------------------------------------------
  # Save outputs
  # --------------------------------------------------------------------------
  if (save_outputs) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

    saveRDS(weekly_usage,
            file.path(cache_dir, "s2_week12_weekly_usage.rds"))
    saveRDS(weekly_fantasy,
            file.path(cache_dir, "s2_week12_weekly_fantasy.rds"))
    saveRDS(ramp_flags,
            file.path(cache_dir, "s2_week12_ramp_flags.rds"))
    saveRDS(groups,
            file.path(cache_dir, "s2_week12_groups.rds"))
    saveRDS(effects,
            file.path(cache_dir, "s2_week12_effects.rds"))
    saveRDS(rookie_effects,
            file.path(cache_dir, "s2_week12_rookie_effects.rds"))
    writeLines(spec,
               file.path(cache_dir, "s2_week12_experiment_spec.txt"))
    writeLines(report,
               file.path(cache_dir, "s2_week12_results_report.txt"))

    if (verbose) message(glue("\nOutputs saved to: {cache_dir}"))
  }

  # --------------------------------------------------------------------------
  # KEY INSIGHTS (computed from live results -- never hardcoded)
  # --------------------------------------------------------------------------
  n_trt  <- sum(groups$group == "treatment", na.rm = TRUE)
  n_ctl  <- sum(groups$group == "control",   na.rm = TRUE)
  n_rook <- sum(groups$is_rookie & groups$group %in% c("treatment", "control"),
                na.rm = TRUE)

  ppg_est  <- round(effects$effect_ppg$estimate, 2)
  ppg_ci1  <- round(effects$effect_ppg$ci_lower, 2)
  ppg_ci2  <- round(effects$effect_ppg$ci_upper, 2)
  d_val    <- round(effects$cohens_d_ppg, 3)
  persist  <- round(
    effects$usage_persistence$persistence_effect$estimate, 4
  )

  trt_mean_ppg <- effects$group_summary$mean_ppg[
    effects$group_summary$group == "treatment"
  ]
  ctl_mean_ppg <- effects$group_summary$mean_ppg[
    effects$group_summary$group == "control"
  ]
  trt_ppg_fmt <- if (length(trt_mean_ppg) > 0L) round(trt_mean_ppg, 2) else NA_real_
  ctl_ppg_fmt <- if (length(ctl_mean_ppg) > 0L) round(ctl_mean_ppg, 2) else NA_real_

  rook_est_fmt <- round(rookie_effects$rookie_effect$estimate, 2)
  vet_est_fmt  <- round(rookie_effects$vet_effect$estimate,   2)

  direction <- if (!is.na(ppg_est) && ppg_est > 0) {
    "treatment scored higher"
  } else if (!is.na(ppg_est) && ppg_est < 0) {
    "control scored higher"
  } else "indeterminate"

  n_checks_passed <- assumptions$n_passed

  if (verbose) {
    cat(glue(
      "\n{sep}\n",
      "KEY INSIGHTS -- Week 12 Usage Ramp Experiment\n",
      "{sep}\n",
      "Treatment N = {format(n_trt, big.mark=',')} | ",
      "Control N = {format(n_ctl, big.mark=',')} | ",
      "Rookies in T+C = {format(n_rook, big.mark=',')}\n",
      "Assumption checks passed: {n_checks_passed}/5\n\n",

      "PRIMARY EFFECT (PPR fantasy points):\n",
      "  Treatment Wks 9-18 mean = {trt_ppg_fmt} PPG\n",
      "  Control   Wks 9-18 mean = {ctl_ppg_fmt} PPG\n",
      "  Difference = {ppg_est} PPG | 95% CI [{ppg_ci1}, {ppg_ci2}]\n",
      "  Direction: {direction}\n",
      "  Cohen's d = {d_val}\n\n",

      "USAGE PERSISTENCE:\n",
      "  Treatment - Control Wks 9-18 usage = {persist} share points\n\n",

      "ROOKIE vs VETERAN:\n",
      "  Rookie ramp vs flat = {rook_est_fmt} PPG\n",
      "  Veteran ramp vs flat = {vet_est_fmt} PPG\n\n",

      "HETEROGENEOUS EFFECTS: {heterogeneous$summary}\n\n",

      "ROBUSTNESS: {robustness$summary}\n",
      "{sep}\n"
    ), "\n")
  }

  invisible(list(
    weekly_usage   = weekly_usage,
    weekly_fantasy = weekly_fantasy,
    roster_positions = roster_positions,
    ramp_flags     = ramp_flags,
    groups         = groups,
    balance        = balance,
    assumptions    = assumptions,
    spec           = spec,
    effects        = effects,
    rookie_effects = rookie_effects,
    rookie_het     = rookie_het,
    heterogeneous  = heterogeneous,
    robustness     = robustness,
    report         = report
  ))
}
