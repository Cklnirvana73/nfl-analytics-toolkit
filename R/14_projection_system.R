# ==============================================================================
# 14_projection_system.R
# NFL Analytics Toolkit - Week 12: Season-Long Fantasy Projection System
# ==============================================================================
#
# NAVIGATION GUIDE
# ----------------
#   Line  75 : Libraries & global constants
#   Line 135 : Helper utilities (internal)
#   Line 297 : resolve_player_positions() (NEW -- roster-based position lookup)
#   Line 370 : generate_preseason_projections()
#   Line 605 : update_weekly_projections()
#   Line 918 : generate_ros_projections()
#   Line 1150: create_projection_report()  (MODIFIED -- roster_season param added)
#   Line 1408: backtest_projections()
#   Line 1771: run_full_pipeline()         (MODIFIED -- roster_season passthrough)
#
# PURPOSE
# -------
# Capstone file. Consumes artifacts from all prior weeks and assembles a
# complete season-long fantasy projection system with Bayesian updating.
#
# The design is a precision-weighted blend of a prior (preseason expectations)
# and an observed likelihood (ensemble model output from Week 11). As the
# season progresses the observed term dominates. The prior never disappears
# entirely -- even in Week 18 it provides a small regularisation pull.
#
# WEEK 4 PREDICTIVE VALIDITY FINDING (applied here)
# --------------------------------------------------
# Volume/opportunity metrics (rushing yards, receptions, target share) have
# significantly stronger predictive validity for second-half fantasy production
# than efficiency metrics (EPA per play, success rate). The prior for each
# position therefore weights volume features more heavily than efficiency
# features. EPA-based features are used as secondary/diagnostic signals, not
# primary forecasting inputs.
#
# NFL SEASON STRUCTURE (2021 onward -- 17-game regular season)
# ------------------------------------------------------------
# Regular season : Weeks 1-18
# Wild Card       : Week 19
# Divisional      : Week 20
# Conference Champ: Week 21
# Super Bowl      : Week 22
#
# Fantasy relevance boundary: week <= 18 (regular season only).
# Week 22 in nflfastR data = Super Bowl. This is correctly coded.
# This system filters to week <= 18 at data load; playoff games are
# outside scope by design and produce a hard stop if requested.
#
# Bye weeks occur in weeks 5-14 (regular season). No player has a bye
# in weeks 1-4 or weeks 15-18. Projections for bye weeks carry forward
# the prior unchanged -- no update is applied (no observed data available).
#
# BUILDS ON
# ---------
# Week  1 : 01_data_loading.R          -- load_season_pbp(), load_rosters()
# Weeks 2-3: 02-05_*.R                 -- player stats, fantasy scoring
# Week  4 : 06_predictive_validation.R -- stability rankings used for prior weights
# Weeks 5-8: 07-10_*.R                 -- complete feature set
# Week  9 : 11_xgboost_fantasy.R       -- prepare_model_features(), evaluate_model()
# Week 10 : 12_boom_bust_model.R       -- calculate_boom_probability()
# Week 11 : 13_ensemble_pipeline.R     -- ensemble RDS artifacts per position
#
# REQUIRED ARTIFACT PATHS (produced by prior weeks)
# --------------------------------------------------
# output/models/ensemble_passer.rds
# output/models/ensemble_rusher.rds
# output/models/ensemble_receiver.rds
# output/models/boom_bust_passer.rds
# output/models/boom_bust_rusher.rds
# output/models/boom_bust_receiver.rds
# output/models/feature_matrix.rds     (from prepare_model_features())
# NOTE: FEATURE_MATRIX_PATH points to ml_data_{season}.rds (the ML-ready
# output of prepare_model_features()), NOT the raw compile_feature_matrix()
# artifact. Models require columns added by prepare_model_features():
# ppr_points_this_week, availability_rate, weeks_since_last_played, etc.
#
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(here)
library(glue)
library(tidymodels)
library(stacks)
library(nflreadr)

# ------------------------------------------------------------------------------
# GLOBAL CONSTANTS
# ------------------------------------------------------------------------------

# Fantasy scoring relevance boundary -- hard cap at end of regular season.
# Playoff games (week >= 19) are outside projection scope by design.
# Wild Card = week 19 in nflfastR (17-game season, 2021+).
NFL_REGULAR_SEASON_MAX_WEEK <- 18L
NFL_PLAYOFF_START_WEEK      <- 19L

# Bye week boundaries (regular season only)
NFL_BYE_WEEK_MIN <- 5L
NFL_BYE_WEEK_MAX <- 14L

# Minimum observations required before projected rate is trusted over prior
MIN_WEEKS_OBSERVED <- 3L

# Prior weight schedule: weight on PRIOR declines from 1.0 in week 1 to
# PRIOR_FLOOR by week 18. The observed term = 1 - prior_weight.
# Even at week 18 the prior contributes PRIOR_FLOOR regularisation.
PRIOR_WEIGHT_START <- 1.0
PRIOR_WEIGHT_FLOOR <- 0.05

# Week 4 finding: volume features outpredict efficiency. These weights are
# applied inside compute_position_prior() when blending feature contributions.
VOLUME_FEATURE_WEIGHT     <- 0.70
EFFICIENCY_FEATURE_WEIGHT <- 0.30

# Confidence interval z-scores
CI_95_Z <- 1.96
CI_80_Z <- 1.282

# Positions handled by this system
SUPPORTED_POSITIONS <- c("QB", "RB", "WR")

# Ensemble model artifact paths (produced by 13_ensemble_pipeline.R)
ENSEMBLE_PATHS <- list(
  QB = here::here("output", "week11_ensemble_passer_2025.rds"),
  RB = here::here("output", "week11_ensemble_rusher_2025.rds"),
  WR = here::here("output", "week11_ensemble_receiver_2025.rds")
)

BOOM_BUST_PATHS <- list(
  QB = here::here("output", "week12_boom_bust_passer_2025.rds"),
  RB = here::here("output", "week12_boom_bust_rusher_2025.rds"),
  WR = here::here("output", "week12_boom_bust_receiver_2025.rds")
)

FEATURE_MATRIX_PATH <- here::here("output", "ml_data_2025.rds")

# ==============================================================================
# INTERNAL HELPERS (not exported)
# ==============================================================================

#' Compute prior weight for a given week
#'
#' Returns the weight to place on the PRIOR projection for a given week number.
#' Weight declines linearly from 1.0 in week 1 to PRIOR_WEIGHT_FLOOR at week 18.
#' Observed weight = 1 - prior_weight.
#'
#' NFL context: Early season (weeks 1-3) prior dominates because sample sizes
#' are too small to trust observed rates. By week 9+ observed data is primary.
#'
#' @param week Integer week number (1-18).
#' @return Numeric in [PRIOR_WEIGHT_FLOOR, 1.0].
#' @keywords internal
compute_prior_weight <- function(week) {
  if (!is.numeric(week) || week < 1 || week > NFL_REGULAR_SEASON_MAX_WEEK) {
    stop(
      glue("compute_prior_weight: week must be 1-{NFL_REGULAR_SEASON_MAX_WEEK}. ",
           "Got: {week}. Playoff weeks are outside projection scope.")
    )
  }
  slope <- (PRIOR_WEIGHT_START - PRIOR_WEIGHT_FLOOR) /
             (NFL_REGULAR_SEASON_MAX_WEEK - 1)
  weight <- PRIOR_WEIGHT_START - slope * (week - 1)
  max(weight, PRIOR_WEIGHT_FLOOR)
}

#' Load and validate an RDS artifact with explicit failure
#'
#' @param path Character. File path to .rds artifact.
#' @param label Character. Human-readable label for error messages.
#' @return Loaded R object.
#' @keywords internal
load_artifact <- function(path, label) {
  if (!file.exists(path)) {
    stop(
      glue("load_artifact: {label} not found at '{path}'. ",
           "Run the Week {substr(basename(path), 1, 2)} pipeline first.")
    )
  }
  readRDS(path)
}

#' Load all ensemble models, with presence check
#'
#' @return Named list with elements QB, RB, WR.
#' @keywords internal
load_ensemble_models <- function() {
  list(
    QB = load_artifact(ENSEMBLE_PATHS$QB, "QB ensemble (passer)"),
    RB = load_artifact(ENSEMBLE_PATHS$RB, "RB ensemble (rusher)"),
    WR = load_artifact(ENSEMBLE_PATHS$WR, "WR ensemble (receiver)")
  )
}

#' Load all boom/bust classification models
#'
#' @return Named list with elements QB, RB, WR.
#' @keywords internal
load_boom_bust_models <- function() {
  list(
    QB = load_artifact(BOOM_BUST_PATHS$QB, "QB boom/bust classifier"),
    RB = load_artifact(BOOM_BUST_PATHS$RB, "RB boom/bust classifier"),
    WR = load_artifact(BOOM_BUST_PATHS$WR, "WR boom/bust classifier")
  )
}

#' Validate that current_week is within fantasy relevance boundary
#'
#' Playoff weeks (>= 19) produce a hard stop with a clear explanation.
#' This is not a data error -- it is a scope enforcement.
#'
#' @param current_week Integer.
#' @keywords internal
validate_regular_season_week <- function(current_week) {
  if (current_week >= NFL_PLAYOFF_START_WEEK) {
    stop(
      glue(
        "Week {current_week} is a playoff week (Wild Card = week 19 in the ",
        "17-game era). This projection system covers the regular season only ",
        "(weeks 1-{NFL_REGULAR_SEASON_MAX_WEEK}). Playoff games are outside scope. ",
        "Note: week 22 in nflfastR is the Super Bowl -- correctly coded, ",
        "not a data error."
      )
    )
  }
  if (current_week < 1L) {
    stop(glue("current_week must be >= 1. Got: {current_week}."))
  }
}

#' Standardise column names and inject missing columns from feature matrix
#'
#' Bridges the gap between compile_feature_matrix() output (24 columns stored
#' in feature_matrix_2025.rds) and the full prepare_model_features() schema
#' (31 columns) that all trained models expect.
#'
#' Column renames:
#'   - .pred -> predicted_ppr
#'   - ppr_points_next_week -> observed_ppr
#'   - position_group ("passer"/"rusher"/"receiver") -> position ("QB"/"RB"/"WR")
#'   - ppr_points_this_week -> ppr_points_actual
#'
#' Missing column injection (as NA, handled by recipe step_impute_median):
#'   - weeks_since_last_played, missed_weeks_this_season, availability_rate
#'
#' Type coercion (to match prepare_model_features() output):
#'   - role_stability_flag: logical -> integer
#'   - opponent_style: character -> factor (4 levels)
#'   - opponent_tier: character -> factor (6 levels)
#'   - position_group: character -> factor (3 levels)
#'
#' @param df Tibble from predict_fantasy_points(), feature matrix, or
#'   compile_feature_matrix() output.
#' @return Tibble with standardised column names, injected availability
#'   columns, and coerced types ready for model prediction.
#' @keywords internal
standardise_prediction_columns <- function(df) {
  # .pred -> predicted_ppr
  if (".pred" %in% names(df) && !"predicted_ppr" %in% names(df)) {
    df <- dplyr::rename(df, predicted_ppr = .pred)
  }
  # ppr_points_next_week -> observed_ppr (this is the outcome variable)
  if ("ppr_points_next_week" %in% names(df) && !"observed_ppr" %in% names(df)) {
    df <- dplyr::rename(df, observed_ppr = ppr_points_next_week)
  }
  # position_group ("passer"/"rusher"/"receiver") -> position ("QB"/"RB"/"WR")
  # prepare_model_features() uses position_group; this system uses position.
  if ("position_group" %in% names(df) && !"position" %in% names(df)) {
    df <- dplyr::mutate(
      df,
      position = dplyr::case_when(
        position_group == "passer"   ~ "QB",
        position_group == "rusher"   ~ "RB",
        position_group == "receiver" ~ "WR",
        TRUE                         ~ NA_character_
      )
    )
  }
  # ppr_points_this_week -> ppr_points_actual (observed score for current week)
  # prepare_model_features() uses ppr_points_this_week; backtest expects ppr_points_actual.
  if ("ppr_points_this_week" %in% names(df) && !"ppr_points_actual" %in% names(df)) {
    df <- dplyr::rename(df, ppr_points_actual = ppr_points_this_week)
  }
  # week column naming: ensure `week` exists (not `predicted_week`)
  # predicted_week = 22 issue: filtered upstream at week <= 18

  # --- Inject columns that prepare_model_features() adds but ----------------
  # --- raw compile_feature_matrix() output lacks (defensive fallback). ------
  # FEATURE_MATRIX_PATH now points to ml_data_{season}.rds which has these
  # columns. This block is a no-op in normal operation but guards against
  # a user accidentally loading the raw feature_matrix_{season}.rds instead.
  availability_cols <- c("weeks_since_last_played",
                         "missed_weeks_this_season",
                         "availability_rate")
  for (col in availability_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA_real_
    }
  }

  # --- Type coercion to match prepare_model_features() output ---------------
  # role_stability_flag: logical in compile_feature_matrix(), integer in
  # prepare_model_features(). Recipes prepped on integer will choke on logical.
  if ("role_stability_flag" %in% names(df) && is.logical(df$role_stability_flag)) {
    df$role_stability_flag <- as.integer(df$role_stability_flag)
  }

  # opponent_style / opponent_tier: character in compile_feature_matrix(),
  # factor with fixed levels in prepare_model_features(). The recipe's
  # step_novel() handles unseen levels; step_dummy(one_hot = TRUE) requires
  # factor input. Coerce here so forge() matches the trained recipe.
  if ("opponent_style" %in% names(df) && !is.factor(df$opponent_style)) {
    df$opponent_style <- factor(
      dplyr::coalesce(as.character(df$opponent_style), "unknown"),
      levels = c("pass_funnel", "run_funnel", "balanced", "unknown")
    )
  }
  if ("opponent_tier" %in% names(df) && !is.factor(df$opponent_tier)) {
    df$opponent_tier <- factor(
      dplyr::coalesce(as.character(df$opponent_tier), "unknown"),
      levels = c("elite", "above_avg", "average", "below_avg", "poor", "unknown")
    )
  }
  if ("position_group" %in% names(df) && !is.factor(df$position_group)) {
    df$position_group <- factor(
      df$position_group,
      levels = c("passer", "rusher", "receiver")
    )
  }

  df
}

#' Derive opponent difficulty from the feature matrix
#'
#' Computes per-team defensive quality from offensive EPA allowed in prior
#' weeks, then maps each player's current-week opponent to a difficulty score.
#' Centered so 0 = league average defense, positive = tougher, negative = easier.
#'
#' This is a convenience function for scripts that need opponent_difficulty
#' without running the full compile_feature_matrix() -> calculate_opponent_adjustments()
#' pipeline externally.
#'
#' @param feature_matrix Tibble from prepare_model_features() or ml_data RDS.
#'   Must contain: player_id, team, opponent, week, epa_this_week.
#' @param target_week Integer. The week to generate matchup flags for.
#'   Defensive quality is computed from weeks 1 to (target_week - 1) only.
#' @param min_games Integer. Minimum defensive games for a team to have a
#'   non-NA difficulty score. Default 3.
#'
#' @return Tibble with columns player_id (chr) and opponent_difficulty_remaining
#'   (dbl). Suitable for passing to generate_ros_projections() and
#'   create_projection_report().
#'
#' @keywords internal
derive_opponent_difficulty <- function(feature_matrix,
                                       target_week,
                                       min_games = 3L) {

  required <- c("player_id", "team", "opponent", "week", "epa_this_week")
  missing  <- setdiff(required, names(feature_matrix))
  if (length(missing) > 0) {
    warning(glue(
      "derive_opponent_difficulty: missing columns: ",
      "{paste(missing, collapse = ', ')}. Returning NULL."
    ))
    return(NULL)
  }

  # Defensive quality: mean offensive EPA each defense allows (prior weeks only)
  # One row per team-week to avoid double-counting (distinct on team + week)
  team_week_epa <- feature_matrix %>%
    dplyr::filter(week < target_week, !is.na(epa_this_week)) %>%
    dplyr::group_by(opponent, week) %>%
    dplyr::summarise(
      opp_epa_allowed = mean(epa_this_week, na.rm = TRUE),
      .groups = "drop"
    )

  def_quality <- team_week_epa %>%
    dplyr::group_by(opponent) %>%
    dplyr::summarise(
      mean_epa_allowed = mean(opp_epa_allowed, na.rm = TRUE),
      def_games        = dplyr::n(),
      .groups          = "drop"
    ) %>%
    dplyr::filter(def_games >= min_games)

  if (nrow(def_quality) == 0) {
    warning("derive_opponent_difficulty: no teams with enough games. Returning NULL.")
    return(NULL)
  }

  # Center: 0 = average defense. Positive = tougher (allows LESS EPA).
  league_avg <- mean(def_quality$mean_epa_allowed, na.rm = TRUE)
  def_quality <- def_quality %>%
    dplyr::mutate(
      # Negate: defense allowing less EPA than average = harder matchup = positive
      def_difficulty = -(mean_epa_allowed - league_avg)
    )

  # Map each player's target-week opponent to difficulty
  # Dedup: traded players appear on two teams in the same week
  target_players <- feature_matrix %>%
    dplyr::filter(week == target_week) %>%
    dplyr::distinct(player_id, .keep_all = TRUE) %>%
    dplyr::select(player_id, opponent)

  result <- target_players %>%
    dplyr::left_join(
      def_quality %>% dplyr::select(opponent, opponent_difficulty_remaining = def_difficulty),
      by = "opponent"
    ) %>%
    dplyr::mutate(
      opponent_difficulty_remaining = dplyr::coalesce(opponent_difficulty_remaining, 0)
    ) %>%
    dplyr::select(player_id, opponent_difficulty_remaining)

  cat(glue(
    "[derive_opponent_difficulty] Week {target_week}: ",
    "{nrow(result)} players mapped. ",
    "{sum(result$opponent_difficulty_remaining > 0.10)} tough, ",
    "{sum(result$opponent_difficulty_remaining < -0.10)} favorable, ",
    "{sum(abs(result$opponent_difficulty_remaining) <= 0.10)} neutral.\n\n"
  ))

  result
}
#'
#' The feature matrix uses position_group ("passer"/"rusher"/"receiver") for
#' model routing. This is coarse: TEs and WRs are both "receiver", and a few
#' WRs/TEs with rush attempts land in "rusher". Users need the real NFL
#' position for filtering (e.g. TE-premium leagues, flex decisions).
#'
#' This function loads the nflreadr roster once per call and caches the result
#' in the calling environment. Network cost: one HTTP request to nflreadr's
#' GitHub-hosted roster CSV per unique season requested.
#'
#' @param player_ids Character vector. GSIS player IDs to resolve.
#' @param season Integer. NFL season year. Default 2025.
#' @return Tibble with columns player_id (chr) and player_position (chr).
#'   player_position values: "QB", "RB", "WR", "TE", or NA_character_ if
#'   the player was not found in the roster for that season.
#' @keywords internal
resolve_player_positions <- function(player_ids, season = 2025L) {
  stopifnot(
    is.character(player_ids),
    is.numeric(season),
    length(season) == 1L
  )

  if (length(player_ids) == 0L) {
    return(tibble::tibble(
      player_id       = character(0),
      player_position = character(0)
    ))
  }

  # Load roster -- single network call per invocation
  cat(glue(
    "[resolve_player_positions] Loading {season} roster from nflreadr...\n\n"
  ))

  roster <- tryCatch(
    nflreadr::load_rosters(seasons = season),
    error = function(e) {
      warning(glue(
        "resolve_player_positions: failed to load roster for season {season}: ",
        "{conditionMessage(e)}. Returning NA positions."
      ))
      NULL
    }
  )

  if (is.null(roster)) {
    return(tibble::tibble(
      player_id       = unique(player_ids),
      player_position = NA_character_
    ))
  }

  # nflreadr roster uses gsis_id as the player identifier and position for

  # the canonical NFL position (QB, RB, WR, TE, etc.)
  # Deduplicate: a player can appear on multiple roster snapshots within a
  # season (e.g. traded mid-season). Take the most recent entry.
  roster_lookup <- roster %>%
    dplyr::filter(
      !is.na(gsis_id),
      !is.na(position),
      position %in% c("QB", "RB", "WR", "TE")
    ) %>%
    dplyr::arrange(gsis_id, dplyr::desc(week)) %>%
    dplyr::distinct(gsis_id, .keep_all = TRUE) %>%
    dplyr::select(player_id = gsis_id, player_position = position)

  # Join to requested IDs
  result <- tibble::tibble(player_id = unique(player_ids)) %>%
    dplyr::left_join(roster_lookup, by = "player_id")

  n_missing <- sum(is.na(result$player_position))
  if (n_missing > 0) {
    warning(glue(
      "resolve_player_positions: {n_missing} of {nrow(result)} players ",
      "not found in {season} roster. Their player_position will be NA."
    ))
  }

  cat(glue(
    "[resolve_player_positions] Resolved {nrow(result) - n_missing} of ",
    "{nrow(result)} players. {n_missing} unmatched (NA).\n\n"
  ))

  result
}

# ==============================================================================
# FUNCTION 1: generate_preseason_projections()
# ==============================================================================

#' Generate preseason fantasy projections anchored to prior-season data
#'
#' @description
#' Produces a baseline projection per player before any current-season games
#' have been played. The prior is derived from prior-season player statistics
#' (from the Week 2-3 functions) with a regression-to-the-mean correction
#' scaled by prior-season sample size.
#'
#' **NFL context:**
#' Preseason projections are the most uncertain in the system. Wide uncertainty
#' intervals reflect genuine unpredictability -- injuries, role changes, scheme
#' changes, and offseason transactions are not modelled. The prior will be
#' updated each week as observed data accumulates.
#'
#' **Week 4 predictive validity finding (applied here):**
#' Volume/opportunity features (rushing yards, receptions, target share)
#' carry VOLUME_FEATURE_WEIGHT (70%) of prior weight.
#' Efficiency features (EPA per play, success rate) carry
#' EFFICIENCY_FEATURE_WEIGHT (30%). EPA is analytically valuable at the
#' play level but adds no incremental forecasting validity at the player-
#' aggregate level -- do not over-weight it in the prior.
#'
#' **Regression to the mean:**
#' Applied per position. Players with fewer than `min_prior_games` games in
#' the prior season regress more strongly toward the positional mean.
#' The correction follows: projected_rate = alpha * league_mean + (1-alpha) * observed_rate
#' where alpha = 1 - (games_played / 17). A player with 17 prior games
#' receives no regression. A player with 4 prior games gets strong pull
#' toward the positional mean.
#'
#' @param prior_season_stats Tibble. Prior-season player statistics per game.
#'   Required columns: player_id (chr), player_name (chr), position (chr),
#'   season (int), games_played (int), avg_ppr_per_game (dbl),
#'   avg_targets_per_game (dbl) [WR/QB], avg_rushes_per_game (dbl) [RB],
#'   avg_receptions_per_game (dbl) [RB/WR], avg_pass_epa_per_game (dbl) [QB],
#'   avg_rush_epa_per_game (dbl) [RB].
#'   Produced by: get_player_passing_stats(), get_player_rushing_stats(),
#'   get_player_receiving_stats() from 02_player_stats.R.
#' @param positions Character vector. Positions to project. Default:
#'   SUPPORTED_POSITIONS (QB, RB, WR).
#' @param min_prior_games Integer. Minimum prior-season games to include a
#'   player. Default 4 (one quarter of season). Players below threshold are
#'   flagged but excluded; they have no stable prior.
#' @param prior_season Integer. Season year of prior_season_stats. Used for
#'   audit labelling only.
#'
#' @return Tibble with one row per player containing:
#'   - player_id (chr): GSIS ID
#'   - player_name (chr): Full name
#'   - position (chr): QB / RB / WR
#'   - prior_season (int): Season data was derived from
#'   - projected_ppr_per_game (dbl): Point projection per game
#'   - projected_ppr_lower_95 (dbl): Lower bound of 95% CI
#'   - projected_ppr_upper_95 (dbl): Upper bound of 95% CI
#'   - projected_ppr_sd (dbl): Standard deviation of projection
#'   - prior_games_played (int): Games in prior season (sample size)
#'   - regression_alpha (dbl): Regression-to-mean pull applied (0=none, 1=full)
#'   - prior_volume_rate (dbl): Volume component (70% weight per Week 4 finding)
#'   - prior_efficiency_rate (dbl): Efficiency component (30% weight)
#'
#' @details
#' **NOT returned:** season-level EPA aggregates (those are from current season
#' and would introduce lookahead into the prior). Full-season defense quality
#' is not joined here -- opponent adjustment is applied during weekly updates.
#'
#' @seealso [update_weekly_projections()] which updates this prior each week.
#' @seealso [backtest_projections()] which validates the updating mechanism.
#'
#' @examples
#' \dontrun{
#' prior_stats <- readRDS(here::here("output", "player_stats_2024.rds"))
#' prior_proj <- generate_preseason_projections(
#'   prior_season_stats = prior_stats,
#'   prior_season       = 2024
#' )
#' }
#'
#' @export
generate_preseason_projections <- function(prior_season_stats,
                                           positions     = SUPPORTED_POSITIONS,
                                           min_prior_games = 4L,
                                           prior_season  = NULL) {

  # --- Input validation -------------------------------------------------------
  stopifnot(is.data.frame(prior_season_stats))

  required_cols <- c(
    "player_id", "player_name", "position",
    "games_played", "avg_ppr_per_game"
  )
  missing_cols <- setdiff(required_cols, names(prior_season_stats))
  if (length(missing_cols) > 0) {
    stop(glue(
      "generate_preseason_projections: missing required columns: ",
      "{paste(missing_cols, collapse = ', ')}"
    ))
  }

  unsupported <- setdiff(positions, SUPPORTED_POSITIONS)
  if (length(unsupported) > 0) {
    stop(glue(
      "Unsupported positions: {paste(unsupported, collapse = ', ')}. ",
      "Supported: {paste(SUPPORTED_POSITIONS, collapse = ', ')}."
    ))
  }

  # --- Filter to supported positions and minimum games -----------------------
  excluded_low_sample <- prior_season_stats %>%
    dplyr::filter(
      position %in% positions,
      !is.na(avg_ppr_per_game),
      games_played < min_prior_games
    )

  if (nrow(excluded_low_sample) > 0) {
    cat(glue(
      "[generate_preseason_projections] Excluding {nrow(excluded_low_sample)} ",
      "players with fewer than {min_prior_games} prior-season games. ",
      "They have no stable prior.\n\n"
    ))
  }

  stats_clean <- prior_season_stats %>%
    dplyr::filter(
      position %in% positions,
      !is.na(avg_ppr_per_game),
      games_played >= min_prior_games
    )

  if (nrow(stats_clean) == 0) {
    stop("generate_preseason_projections: no qualifying players after filtering.")
  }

  # --- Positional league means (used for regression to mean) -----------------
  positional_means <- stats_clean %>%
    dplyr::group_by(position) %>%
    dplyr::summarise(
      pos_mean_ppr = mean(avg_ppr_per_game, na.rm = TRUE),
      pos_sd_ppr   = sd(avg_ppr_per_game,   na.rm = TRUE),
      .groups = "drop"
    )

  # --- Compute volume and efficiency prior contributions ---------------------
  # Volume proxy: avg_ppr_per_game itself (dominated by volume in PPR)
  # Efficiency proxy: EPA columns where available, else zero
  # NOTE: case_when evaluates ALL RHS expressions for type-checking, so
  # referencing a non-existent column on the RHS errors even when the LHS

  # condition is FALSE.  Ensure columns exist with a safe default first.
  if (!"avg_pass_epa_per_game" %in% names(stats_clean)) {
    warning("avg_pass_epa_per_game not found in input -- QB efficiency ",
            "component will be 0. Include this column for accurate QB ",
            "projections.", call. = FALSE)
    stats_clean$avg_pass_epa_per_game <- 0
  }
  if (!"avg_rush_epa_per_game" %in% names(stats_clean)) {
    warning("avg_rush_epa_per_game not found in input -- RB efficiency ",
            "component will be 0. Include this column for accurate RB ",
            "projections.", call. = FALSE)
    stats_clean$avg_rush_epa_per_game <- 0
  }

  stats_enriched <- stats_clean %>%
    dplyr::mutate(
      efficiency_signal = dplyr::case_when(
        position == "QB" ~ avg_pass_epa_per_game,
        position == "RB" ~ avg_rush_epa_per_game,
        TRUE ~ 0
      ),
      # Volume signal: targets / rushes per game scaled to PPR contribution
      volume_signal = avg_ppr_per_game
    )

  # --- Apply regression to the mean -----------------------------------------
  # alpha = 1 - (games_played / 17). Full season = no regression.
  # Per Week 4 finding: volume weighted 70%, efficiency 30%.
  projections <- stats_enriched %>%
    dplyr::left_join(positional_means, by = "position") %>%
    dplyr::mutate(
      # Regression alpha: stronger pull for low-game players
      regression_alpha = pmax(0, 1 - (games_played / 17)),

      # Volume component: regression-corrected PPR rate
      prior_volume_rate = (1 - regression_alpha) * avg_ppr_per_game +
                           regression_alpha       * pos_mean_ppr,

      # Efficiency component: scaled EPA signal (kept modest per Week 4 finding)
      prior_efficiency_rate = efficiency_signal,

      # Projected PPR: weighted blend (70% volume, 30% efficiency rescaled)
      # Efficiency is diagnostic, not primary, per Week 4 result
      projected_ppr_per_game = VOLUME_FEATURE_WEIGHT * prior_volume_rate +
                               EFFICIENCY_FEATURE_WEIGHT * pmax(
                                 prior_volume_rate + prior_efficiency_rate * 2,
                                 0
                               ),

      # Uncertainty: wider for low-sample players and low-games positions
      # SD inflated by (1 + regression_alpha) -- more uncertainty = more regression
      projected_ppr_sd = pos_sd_ppr * (1 + regression_alpha),

      projected_ppr_lower_95 = projected_ppr_per_game - CI_95_Z * projected_ppr_sd,
      projected_ppr_upper_95 = projected_ppr_per_game + CI_95_Z * projected_ppr_sd,

      # Ensure lower bound is not negative (can't score below 0)
      projected_ppr_lower_95 = pmax(projected_ppr_lower_95, 0),

      prior_season = ifelse(
        is.null(prior_season),
        NA_integer_,
        as.integer(prior_season)
      )
    ) %>%
    dplyr::select(
      player_id,
      player_name,
      position,
      prior_season,
      projected_ppr_per_game,
      projected_ppr_lower_95,
      projected_ppr_upper_95,
      projected_ppr_sd,
      prior_games_played = games_played,
      regression_alpha,
      prior_volume_rate,
      prior_efficiency_rate
    ) %>%
    dplyr::arrange(position, dplyr::desc(projected_ppr_per_game))

  cat(glue(
    "[generate_preseason_projections] Complete. ",
    "{nrow(projections)} players projected across ",
    "{length(unique(projections$position))} positions.\n\n"
  ))

  projections
}

# ==============================================================================
# FUNCTION 2: update_weekly_projections()
# ==============================================================================

#' Update fantasy projections using Bayesian precision-weighted blend
#'
#' @description
#' Core engine of the projection system. Each week, takes the prior projection
#' (preseason or previous week's posterior) and blends it with the observed
#' likelihood (ensemble model prediction from Week 11) using a precision-
#' weighted average.
#'
#' **Update rule:**
#' posterior = prior_weight * prior + (1 - prior_weight) * ensemble_prediction
#'
#' This is a tractable approximation to Bayesian updating. A fully conjugate
#' Bayesian approach would require specifying a likelihood distribution, which
#' adds complexity without material benefit at this sample size.
#'
#' **Prior weight schedule:**
#' Week 1: prior_weight = 1.0  (prior dominates, no observed data)
#' Week 9: prior_weight ~ 0.53 (roughly equal blend)
#' Week 18: prior_weight = 0.05 (observed data dominates)
#'
#' **Bye week handling:**
#' If `is_bye_week = TRUE` for a player, no update is applied. The prior is
#' carried forward unchanged with uncertainty widened slightly (+10% SD) to
#' reflect missed playing time. Bye weeks occur in regular season weeks 5-14.
#'
#' **NFL context:**
#' Opponent adjustment is embedded in the ensemble model features (from Week 8
#' 10_opponent_features.R). The update therefore implicitly adjusts for matchup
#' quality -- no separate opponent step is needed here.
#'
#' **Column name note:**
#' Ensemble predictions are standardised via standardise_prediction_columns()
#' before blending. This resolves the `.pred` vs `predicted_ppr` mismatch
#' from the Week 9 artifact.
#'
#' @param prior_projections Tibble. Output of generate_preseason_projections()
#'   or previous call to update_weekly_projections(). Required columns:
#'   player_id (chr), position (chr), projected_ppr_per_game (dbl),
#'   projected_ppr_sd (dbl).
#' @param current_week Integer. NFL week being updated (1-18).
#'   Weeks >= 19 produce a hard stop (playoff scope enforcement).
#' @param ensemble_models Named list. Output of load_ensemble_models().
#'   Must contain QB, RB, WR elements.
#' @param feature_matrix Tibble. Current-week features for all players.
#'   Produced by prepare_model_features() from 11_xgboost_fantasy.R.
#'   Required columns include player_id, position, week, and all feature
#'   columns expected by each position's ensemble model.
#' @param bye_week_players Character vector. player_id values on bye this week.
#'   Default NULL (no byes -- weeks 1-4 and 15-18 have no byes).
#'
#' @return Tibble with one row per player containing:
#'   - player_id (chr)
#'   - player_name (chr)
#'   - position (chr)
#'   - week (int): Week of this update
#'   - projected_ppr_per_game (dbl): Posterior projection
#'   - projected_ppr_lower_95 (dbl)
#'   - projected_ppr_upper_95 (dbl)
#'   - projected_ppr_lower_80 (dbl)
#'   - projected_ppr_upper_80 (dbl)
#'   - projected_ppr_sd (dbl)
#'   - ensemble_prediction (dbl): Raw ensemble point estimate this week
#'   - prior_weight (dbl): Weight applied to prior this week
#'   - observed_weight (dbl): Weight applied to ensemble (1 - prior_weight)
#'   - is_bye_week (lgl): TRUE if player had bye (no update applied)
#'
#' @seealso [generate_preseason_projections()] for the initial prior.
#' @seealso [generate_ros_projections()] for rest-of-season totals.
#'
#' @examples
#' \dontrun{
#' ensembles   <- load_ensemble_models()
#' feat_matrix <- readRDS(here::here("output", "models", "feature_matrix.rds"))
#'
#' # Week 1 update (prior dominates)
#' wk1_proj <- update_weekly_projections(
#'   prior_projections = preseason_proj,
#'   current_week      = 1L,
#'   ensemble_models   = ensembles,
#'   feature_matrix    = feat_matrix %>% dplyr::filter(week == 1)
#' )
#' }
#'
#' @export
update_weekly_projections <- function(prior_projections,
                                      current_week,
                                      ensemble_models,
                                      feature_matrix,
                                      bye_week_players = NULL) {

  # --- Input validation -------------------------------------------------------
  validate_regular_season_week(current_week)

  stopifnot(
    is.data.frame(prior_projections),
    is.data.frame(feature_matrix),
    is.list(ensemble_models)
  )

  required_prior_cols <- c(
    "player_id", "position",
    "projected_ppr_per_game", "projected_ppr_sd"
  )
  missing_prior <- setdiff(required_prior_cols, names(prior_projections))
  if (length(missing_prior) > 0) {
    stop(glue(
      "update_weekly_projections: prior_projections missing columns: ",
      "{paste(missing_prior, collapse = ', ')}"
    ))
  }

  missing_ensemble <- setdiff(SUPPORTED_POSITIONS, names(ensemble_models))
  if (length(missing_ensemble) > 0) {
    stop(glue(
      "update_weekly_projections: ensemble_models missing positions: ",
      "{paste(missing_ensemble, collapse = ', ')}"
    ))
  }

  cat(glue("[update_weekly_projections] Processing week {current_week}...\n\n"))

  # --- Bye week setup ---------------------------------------------------------
  if (is.null(bye_week_players)) bye_week_players <- character(0)

  # NFL bye weeks only occur in weeks 5-14
  if (current_week < NFL_BYE_WEEK_MIN || current_week > NFL_BYE_WEEK_MAX) {
    if (length(bye_week_players) > 0) {
      warning(glue(
        "Week {current_week} is outside the bye week window ",
        "({NFL_BYE_WEEK_MIN}-{NFL_BYE_WEEK_MAX}). ",
        "Bye week players list will be ignored."
      ))
      bye_week_players <- character(0)
    }
  }

  # --- Compute prior weight for this week ------------------------------------
  pw <- compute_prior_weight(current_week)
  ow <- 1 - pw  # observed weight

  cat(glue(
    "[update_weekly_projections] Week {current_week}: ",
    "prior_weight = {round(pw, 3)}, observed_weight = {round(ow, 3)}\n\n"
  ))

  # --- Generate ensemble predictions per position ----------------------------
  # Standardise column names: position_group -> position, etc.
  feature_matrix <- standardise_prediction_columns(feature_matrix)

  ensemble_preds <- purrr::map_dfr(
    SUPPORTED_POSITIONS,
    function(pos) {
      feat_pos <- feature_matrix %>%
        dplyr::filter(
          position == pos,
          !is.na(player_id)
        ) %>%
        # Dedup: traded players can appear twice in the same week for the same
        # position (one row per team). Keep the last row (most recent team).
        # Without this, ensemble_preds has duplicate player_id+position and the
        # blend join fails with relationship = "many-to-one".
        dplyr::distinct(player_id, .keep_all = TRUE)

      if (nrow(feat_pos) == 0) {
        cat(glue("  [update] No {pos} features for week {current_week}.\n\n"))
        return(tibble::tibble(
          player_id = character(),
          position = character(),
          ensemble_prediction = numeric()
        ))
      }

      model_obj <- ensemble_models[[pos]]

      # build_ensemble() returns a list with $stack_final as the fitted
      # stacks model. Extract it; fall back to the object itself if it
      # is already a model_stack (defensive for alternative storage).
      model <- if (is.list(model_obj) && "stack_final" %in% names(model_obj)) {
        model_obj$stack_final
      } else {
        model_obj
      }

      # Guard: ppr_points_this_week is required by the ensemble recipe
      # (baked in during Week 11 training). At live prediction time this is
      # the player's observed score for the current week -- present in the
      # historical feature matrix. If missing (e.g. early-season rows not yet
      # scored), inject NA so hardhat::forge() doesn't error. The model
      # degrades gracefully on NA inputs via the recipe's step_impute_median.
      if (!"ppr_points_this_week" %in% names(feat_pos)) {
        feat_pos <- dplyr::mutate(feat_pos, ppr_points_this_week = NA_real_)
      }

      # Ensemble model predict call -- uses stacks-based workflow
      # predict() returns a tibble with .pred column
      raw_preds <- tryCatch(
        predict(model, new_data = feat_pos),
        error = function(e) {
          warning(glue(
            "Ensemble prediction failed for {pos} week {current_week}: ",
            "{conditionMessage(e)}"
          ))
          NULL
        }
      )

      if (is.null(raw_preds)) return(tibble::tibble(
        player_id = character(),
        position = character(),
        ensemble_prediction = numeric()
      ))

      # Standardise .pred -> predicted_ppr (resolves Week 9 column mismatch)
      raw_preds <- standardise_prediction_columns(raw_preds)

      # Bind player identifiers
      feat_pos %>%
        dplyr::select(player_id, position) %>%
        dplyr::bind_cols(raw_preds) %>%
        dplyr::select(player_id, position, ensemble_prediction = predicted_ppr)
    }
  )

  # --- Blend prior with ensemble predictions ----------------------------------
  # Drop ensemble_prediction from prior_projections before joining.
  # On week 2+, prior_projections carries ensemble_prediction from the
  # previous week's output. Joining with ensemble_preds (which also has
  # ensemble_prediction) creates .x/.y suffixes, breaking the mutate below.
  updated <- prior_projections %>%
    dplyr::select(-dplyr::any_of("ensemble_prediction")) %>%
    dplyr::left_join(
      ensemble_preds,
      by = c("player_id", "position"),
      relationship = "many-to-one"
      # NOTE: many-to-many join warning from prepare_model_features() is
      # suppressed by explicit relationship = "many-to-one" here.
      # Upstream cause: compile_feature_matrix() produces duplicate player-week
      # rows for traded players (same player_id, same week, two teams).
      # Mitigation: filter to most_recent_team in feature matrix before passing.
      # Full fix: add deduplication step in prepare_model_features() (deferred).
    )

  # Defensive column add: if ALL positions failed predict(), map_dfr can
  # return a 0-row tibble with no columns (purrr drops names on all-empty
  # bind). The left_join then has no ensemble_prediction column to reference,
  # causing "object not found" in the subsequent mutate.
  if (!"ensemble_prediction" %in% names(updated)) {
    updated <- updated %>%
      dplyr::mutate(ensemble_prediction = NA_real_)
  }

  updated <- updated %>%
    dplyr::mutate(
      week       = as.integer(current_week),
      is_bye_week = player_id %in% bye_week_players,

      # Ensemble prediction: use NA-safe fallback to prior when missing
      ensemble_prediction = dplyr::if_else(
        is.na(ensemble_prediction),
        projected_ppr_per_game,
        ensemble_prediction
      ),

      # Posterior: precision-weighted blend
      # Bye week players carry prior forward unchanged
      projected_ppr_per_game = dplyr::if_else(
        is_bye_week,
        projected_ppr_per_game,
        pw * projected_ppr_per_game + ow * ensemble_prediction
      ),

      # Uncertainty: narrows as observed weight increases, widens slightly for byes
      projected_ppr_sd = dplyr::if_else(
        is_bye_week,
        projected_ppr_sd * 1.10,
        projected_ppr_sd * (pw + ow * 0.85)
      ),

      projected_ppr_lower_95 = pmax(
        projected_ppr_per_game - CI_95_Z * projected_ppr_sd, 0
      ),
      projected_ppr_upper_95 = projected_ppr_per_game + CI_95_Z * projected_ppr_sd,
      projected_ppr_lower_80 = pmax(
        projected_ppr_per_game - CI_80_Z * projected_ppr_sd, 0
      ),
      projected_ppr_upper_80 = projected_ppr_per_game + CI_80_Z * projected_ppr_sd,

      prior_weight    = pw,
      observed_weight = ow
    )

  cat(glue(
    "[update_weekly_projections] Week {current_week} complete. ",
    "{nrow(updated)} players updated. ",
    "{sum(updated$is_bye_week, na.rm = TRUE)} on bye (prior carried forward).\n\n"
  ))

  # Return standardised column set
  updated %>%
    dplyr::select(
      player_id,
      dplyr::any_of("player_name"),
      position,
      week,
      projected_ppr_per_game,
      projected_ppr_lower_95,
      projected_ppr_upper_95,
      projected_ppr_lower_80,
      projected_ppr_upper_80,
      projected_ppr_sd,
      ensemble_prediction,
      prior_weight,
      observed_weight,
      is_bye_week
    )
}

# ==============================================================================
# FUNCTION 3: generate_ros_projections()
# ==============================================================================

#' Generate rest-of-season fantasy projections
#'
#' @description
#' Computes cumulative projected fantasy points for all remaining regular season
#' weeks from any point in the season. Takes the current posterior weekly rate
#' and multiplies by remaining games, adjusting for known bye weeks in the
#' remaining schedule and for schedule-adjusted opponent difficulty (from the
#' Week 8 opponent features).
#'
#' **Regular season boundary:**
#' Remaining games = NFL_REGULAR_SEASON_MAX_WEEK (18) - current_week.
#' If current_week = 18, remaining_games = 0 and ROS = 0 for all players.
#' Playoff weeks are outside scope -- hard stop if current_week >= 19.
#'
#' **Bye week handling in remaining schedule:**
#' Each player's remaining bye week is subtracted from remaining_games.
#' Bye weeks only occur in the regular season (weeks 5-14). A player whose
#' bye falls in a week already past does not lose a game from their ROS count.
#'
#' **Schedule difficulty adjustment:**
#' The opponent_difficulty_remaining column (from Week 8's
#' get_schedule_adjusted_stats()) scales the ROS projection up or down.
#' A player facing a soft remaining schedule gets a positive adjustment.
#' The adjustment is multiplicative and capped at +/- 20% to prevent
#' extreme values from a single matchup dominating the ROS.
#'
#' **Confidence interval widening:**
#' Uncertainty compounds over remaining weeks. Projected SD is multiplied by
#' sqrt(remaining_games) -- each additional game adds independent variance.
#' This means ROS confidence intervals are intentionally wide.
#'
#' @param current_projections Tibble. Output of update_weekly_projections().
#'   Required columns: player_id, position, projected_ppr_per_game,
#'   projected_ppr_sd, week.
#' @param current_week Integer. Current NFL week (1-18). Remaining weeks
#'   computed as (18 - current_week).
#' @param bye_week_schedule Named integer vector. Player bye weeks for the
#'   full season. Names are player_id, values are bye week numbers.
#'   Example: c("00-0030506" = 11L, "00-0033873" = 7L).
#'   Default NULL (no remaining byes subtracted).
#' @param opponent_difficulty Tibble. Optional. Schedule-adjusted difficulty
#'   for remaining weeks. Produced by get_schedule_adjusted_stats() from
#'   10_opponent_features.R. Required columns: player_id (chr),
#'   opponent_difficulty_remaining (dbl, centred near 0).
#'   Default NULL (no schedule adjustment applied).
#'
#' @return Tibble with one row per player containing:
#'   - player_id (chr)
#'   - player_name (chr)
#'   - position (chr)
#'   - current_week (int)
#'   - remaining_games (int): Regular season games remaining (post bye adjustment)
#'   - projected_ros_ppr (dbl): Total projected PPR for remaining games
#'   - projected_ros_lower_95 (dbl)
#'   - projected_ros_upper_95 (dbl)
#'   - projected_ros_lower_80 (dbl)
#'   - projected_ros_upper_80 (dbl)
#'   - schedule_adjustment (dbl): Multiplicative factor applied (1.0 = no adjust)
#'   - had_remaining_bye (lgl): TRUE if bye week falls in remaining schedule
#'
#' @seealso [update_weekly_projections()] for the per-game rate input.
#' @seealso [create_projection_report()] which combines ROS and weekly output.
#'
#' @examples
#' \dontrun{
#' # After Week 10 update
#' ros <- generate_ros_projections(
#'   current_projections = wk10_proj,
#'   current_week        = 10L,
#'   bye_week_schedule   = c("00-0030506" = 11L),
#'   opponent_difficulty = opp_diff_tibble
#' )
#' dplyr::arrange(ros, position, dplyr::desc(projected_ros_ppr))
#' }
#'
#' @export
generate_ros_projections <- function(current_projections,
                                     current_week,
                                     bye_week_schedule   = NULL,
                                     opponent_difficulty = NULL) {

  # --- Input validation -------------------------------------------------------
  validate_regular_season_week(current_week)

  stopifnot(is.data.frame(current_projections))

  required_cols <- c(
    "player_id", "position",
    "projected_ppr_per_game", "projected_ppr_sd"
  )
  missing_cols <- setdiff(required_cols, names(current_projections))
  if (length(missing_cols) > 0) {
    stop(glue(
      "generate_ros_projections: current_projections missing columns: ",
      "{paste(missing_cols, collapse = ', ')}"
    ))
  }

  # Edge case: end of regular season
  remaining_total <- NFL_REGULAR_SEASON_MAX_WEEK - current_week
  if (remaining_total == 0L) {
    warning(
      "generate_ros_projections called at week 18 (end of regular season). ",
      "All players have 0 remaining games. Returning zero projections."
    )
    return(
      current_projections %>%
        dplyr::mutate(
          current_week       = as.integer(current_week),
          remaining_games    = 0L,
          projected_ros_ppr  = 0,
          projected_ros_lower_95 = 0,
          projected_ros_upper_95 = 0,
          projected_ros_lower_80 = 0,
          projected_ros_upper_80 = 0,
          schedule_adjustment = 1.0,
          had_remaining_bye   = FALSE
        )
    )
  }

  cat(glue(
    "[generate_ros_projections] Week {current_week}. ",
    "Up to {remaining_total} games remaining in regular season.\n\n"
  ))

  # --- Determine remaining bye weeks per player ------------------------------
  if (!is.null(bye_week_schedule)) {
    bye_df <- tibble::tibble(
      player_id = names(bye_week_schedule),
      bye_week  = as.integer(bye_week_schedule)
    ) %>%
      dplyr::mutate(
        # Bye falls in remaining schedule if > current_week and <= 18
        had_remaining_bye = bye_week > current_week &
                            bye_week <= NFL_REGULAR_SEASON_MAX_WEEK
      )
  } else {
    bye_df <- tibble::tibble(
      player_id         = character(0),
      bye_week          = integer(0),
      had_remaining_bye = logical(0)
    )
  }

  # --- Join opponent difficulty -----------------------------------------------
  if (!is.null(opponent_difficulty)) {
    stopifnot(
      "player_id" %in% names(opponent_difficulty),
      "opponent_difficulty_remaining" %in% names(opponent_difficulty)
    )
  }

  # --- Build ROS projections --------------------------------------------------
  ros <- current_projections %>%
    dplyr::left_join(
      bye_df %>% dplyr::select(player_id, had_remaining_bye),
      by = "player_id"
    ) %>%
    dplyr::mutate(
      had_remaining_bye = dplyr::coalesce(had_remaining_bye, FALSE)
    ) %>%
    {
      if (!is.null(opponent_difficulty)) {
        dplyr::left_join(.,
          opponent_difficulty %>%
            dplyr::select(player_id, opponent_difficulty_remaining),
          by = "player_id"
        )
      } else {
        dplyr::mutate(., opponent_difficulty_remaining = 0)
      }
    } %>%
    dplyr::mutate(
      current_week     = as.integer(current_week),

      # Remaining games after subtracting bye (if in remaining schedule)
      remaining_games  = as.integer(
        remaining_total - as.integer(had_remaining_bye)
      ),

      # Schedule adjustment: multiplicative, capped at +/- 20%
      # Positive difficulty_remaining = harder schedule = lower adjustment
      # Opponent features are centred near 0; convert to multiplier
      schedule_adjustment = pmin(
        pmax(1 - opponent_difficulty_remaining * 0.5, 0.80),
        1.20
      ),

      # ROS projection: rate * remaining games * schedule adjustment
      projected_ros_ppr = pmax(
        projected_ppr_per_game * remaining_games * schedule_adjustment,
        0
      ),

      # Uncertainty compounds: SD scales by sqrt(remaining_games)
      ros_sd = projected_ppr_sd * sqrt(pmax(remaining_games, 1)) *
               schedule_adjustment,

      projected_ros_lower_95 = pmax(projected_ros_ppr - CI_95_Z * ros_sd, 0),
      projected_ros_upper_95 = projected_ros_ppr + CI_95_Z * ros_sd,
      projected_ros_lower_80 = pmax(projected_ros_ppr - CI_80_Z * ros_sd, 0),
      projected_ros_upper_80 = projected_ros_ppr + CI_80_Z * ros_sd
    ) %>%
    dplyr::select(
      player_id,
      dplyr::any_of("player_name"),
      position,
      current_week,
      remaining_games,
      projected_ros_ppr,
      projected_ros_lower_95,
      projected_ros_upper_95,
      projected_ros_lower_80,
      projected_ros_upper_80,
      schedule_adjustment,
      had_remaining_bye
    ) %>%
    dplyr::arrange(position, dplyr::desc(projected_ros_ppr))

  cat(glue(
    "[generate_ros_projections] Complete. {nrow(ros)} players. ",
    "{sum(ros$had_remaining_bye)} have remaining byes accounted for.\n\n"
  ))

  ros
}

# ==============================================================================
# FUNCTION 4: create_projection_report()
# ==============================================================================

#' Create formatted per-position projection report
#'
#' @description
#' Assembles the weekly and ROS projections with boom/bust probabilities
#' (from the Week 10 classification model) into a single, formatted tibble
#' suitable for direct fantasy decision-making. Also flags adverse matchups.
#'
#' This is the integration point between the regression track (Week 9:
#' predicted PPR points) and the classification track (Week 10: boom/bust
#' outcome tier). Both tracks inform different decisions:
#'   - Regression track: "How many points will this player score?"
#'   - Classification track: "Is this player a boom or bust risk?"
#'
#' **Boom/bust tier definitions (from 12_boom_bust_model.R):**
#'   - Boom: top 10% of PPR scores at position for that week
#'   - Bust: bottom 25% of PPR scores at position for that week
#'   - Average: all other outcomes
#'
#' **Matchup flag:**
#' Based on opponent_difficulty_remaining from Week 8. Flagged as "tough"
#' when opponent_difficulty_remaining > 0.10 (one SD above neutral), or
#' "favorable" when < -0.10.
#'
#' **Player position resolution:**
#' The feature matrix uses position_group ("passer"/"rusher"/"receiver") for
#' model routing. This is mapped to position ("QB"/"RB"/"WR") by
#' standardise_prediction_columns(). However, TEs and WRs are both folded
#' into "receiver", and a handful of WRs/TEs with rush attempts appear as
#' "rusher". The roster_season parameter enables resolve_player_positions()
#' to attach the real NFL position (QB/RB/WR/TE) as player_position.
#' This column is for display and filtering only -- model routing still uses
#' position (QB/RB/WR).
#'
#' **Known limitation:**
#' The "receiver" model was trained on WR + TE combined. TE projections
#' are less reliable than WR projections because the model learned a mixed
#' signal. The player_position column lets users identify TEs and apply
#' their own judgment. TE-premium scoring adjustments are out of scope.
#'
#' **Output format:**
#' One row per player, sorted by projected_ppr_per_game descending within
#' position. Designed to be read directly or exported to CSV without
#' post-processing.
#'
#' @param weekly_projections Tibble. Output of update_weekly_projections().
#' @param ros_projections Tibble. Output of generate_ros_projections().
#' @param boom_bust_models Named list. Output of load_boom_bust_models().
#' @param feature_matrix Tibble. Current-week features for prediction input.
#'   Must include player_id, position, week, and all model feature columns.
#' @param opponent_difficulty Tibble. Optional. Same as in
#'   generate_ros_projections(). Default NULL.
#' @param roster_season Integer. Season for nflreadr roster lookup used to
#'   resolve real NFL positions (QB/RB/WR/TE). Default 2025L.
#' @param export_csv Logical. If TRUE, writes report to
#'   output/projections/week{N}_projection_report.csv. Default FALSE.
#'
#' @return Tibble with one row per player containing:
#'   - player_id (chr), player_name (chr), player_position (chr: QB/RB/WR/TE),
#'     position (chr: QB/RB/WR -- model routing label)
#'   - week (int)
#'   - projected_ppr_per_game (dbl): This week's point projection
#'   - projected_ppr_lower_80 (dbl), projected_ppr_upper_80 (dbl)
#'   - projected_ros_ppr (dbl): Rest-of-season total projection
#'   - remaining_games (int)
#'   - boom_probability (dbl): Probability of top-10% outcome at position
#'   - bust_probability (dbl): Probability of bottom-25% outcome at position
#'   - boom_bust_recommendation (chr): "boom", "bust", "average", or "avoid"
#'   - matchup_flag (chr): "favorable", "neutral", or "tough"
#'   - is_bye_week (lgl)
#'
#' @seealso [update_weekly_projections()], [generate_ros_projections()],
#'   [backtest_projections()], [resolve_player_positions()]
#'
#' @examples
#' \dontrun{
#' report <- create_projection_report(
#'   weekly_projections = wk10_proj,
#'   ros_projections    = ros,
#'   boom_bust_models   = load_boom_bust_models(),
#'   feature_matrix     = feat_matrix %>% dplyr::filter(week == 10),
#'   roster_season      = 2025L
#' )
#' # Filter to TEs only (not possible with position alone)
#' report %>% dplyr::filter(player_position == "TE", !is_bye_week) %>% print(n = 20)
#' }
#'
#' @export
create_projection_report <- function(weekly_projections,
                                     ros_projections,
                                     boom_bust_models,
                                     feature_matrix,
                                     opponent_difficulty = NULL,
                                     roster_season       = 2025L,
                                     export_csv          = FALSE) {

  # --- Input validation -------------------------------------------------------
  stopifnot(
    is.data.frame(weekly_projections),
    is.data.frame(ros_projections),
    is.list(boom_bust_models),
    is.data.frame(feature_matrix)
  )

  # Validate boom/bust models cover all positions
  missing_bb <- setdiff(SUPPORTED_POSITIONS, names(boom_bust_models))
  if (length(missing_bb) > 0) {
    stop(glue(
      "create_projection_report: boom_bust_models missing: ",
      "{paste(missing_bb, collapse = ', ')}"
    ))
  }

  # Infer current week from weekly_projections
  current_week <- as.integer(
    dplyr::pull(weekly_projections, week)[1]
  )
  validate_regular_season_week(current_week)

  cat(glue(
    "[create_projection_report] Building report for week {current_week}...\n\n"
  ))

  # --- Boom/bust probabilities ------------------------------------------------
  # Standardise column names: position_group -> position, etc.
  feature_matrix <- standardise_prediction_columns(feature_matrix)

  boom_bust_preds <- purrr::map_dfr(
    SUPPORTED_POSITIONS,
    function(pos) {
      feat_pos <- feature_matrix %>%
        dplyr::filter(position == pos, !is.na(player_id)) %>%
        dplyr::distinct(player_id, .keep_all = TRUE)

      if (nrow(feat_pos) == 0) return(tibble::tibble(
        player_id = character(), position = character(),
        boom_probability = numeric(), bust_probability = numeric()
      ))

      model <- boom_bust_models[[pos]]

      # Class probabilities from Week 10 classifier
      probs <- tryCatch(
        predict(model, new_data = feat_pos, type = "prob"),
        error = function(e) {
          warning(glue(
            "Boom/bust prediction failed for {pos}: {conditionMessage(e)}"
          ))
          NULL
        }
      )

      if (is.null(probs)) return(tibble::tibble(
        player_id = character(), position = character(),
        boom_probability = numeric(), bust_probability = numeric()
      ))

      # Expected column names from 12_boom_bust_model.R:
      # .pred_boom, .pred_bust, .pred_average
      feat_pos %>%
        dplyr::select(player_id, position) %>%
        dplyr::bind_cols(probs) %>%
        dplyr::select(
          player_id,
          position,
          boom_probability = dplyr::any_of(".pred_boom"),
          bust_probability = dplyr::any_of(".pred_bust")
        )
    }
  )

  # --- Matchup flags from opponent difficulty ---------------------------------
  if (!is.null(opponent_difficulty)) {
    opp_flag <- opponent_difficulty %>%
      dplyr::mutate(
        matchup_flag = dplyr::case_when(
          opponent_difficulty_remaining > 0.10  ~ "tough",
          opponent_difficulty_remaining < -0.10 ~ "favorable",
          TRUE                                  ~ "neutral"
        )
      ) %>%
      dplyr::select(player_id, matchup_flag)
  } else {
    opp_flag <- tibble::tibble(
      player_id    = character(0),
      matchup_flag = character(0)
    )
  }

  # --- Resolve real NFL positions (QB/RB/WR/TE) from roster data ---------------
  # position_group -> position (QB/RB/WR) is the model routing label.
  # player_position (QB/RB/WR/TE) is the real NFL position for display/filter.
  # The "receiver" model covers both WR and TE -- player_position distinguishes them.
  all_player_ids <- unique(weekly_projections$player_id)
  position_lookup <- resolve_player_positions(all_player_ids, season = roster_season)

  # --- Assemble report --------------------------------------------------------
  report <- weekly_projections %>%
    dplyr::left_join(
      position_lookup,
      by = "player_id"
    ) %>%
    dplyr::left_join(
      ros_projections %>%
        dplyr::select(player_id, projected_ros_ppr, remaining_games),
      by = "player_id"
    ) %>%
    dplyr::left_join(boom_bust_preds, by = c("player_id", "position")) %>%
    dplyr::left_join(opp_flag, by = "player_id") %>%
    dplyr::mutate(
      matchup_flag = dplyr::coalesce(matchup_flag, "neutral"),

      # Boom/bust recommendation: boom if boom_prob > 0.35 OR bust_prob > 0.40
      boom_bust_recommendation = dplyr::case_when(
        is_bye_week                               ~ "bye",
        !is.na(boom_probability) &
          boom_probability > 0.35                 ~ "boom",
        !is.na(bust_probability) &
          bust_probability > 0.40                 ~ "bust",
        matchup_flag == "tough" &
          !is.na(bust_probability) &
          bust_probability > 0.30                 ~ "avoid",
        TRUE                                      ~ "average"
      )
    ) %>%
    dplyr::select(
      player_id,
      dplyr::any_of("player_name"),
      player_position,
      position,
      week,
      projected_ppr_per_game,
      projected_ppr_lower_80,
      projected_ppr_upper_80,
      projected_ros_ppr,
      remaining_games,
      boom_probability,
      bust_probability,
      boom_bust_recommendation,
      matchup_flag,
      is_bye_week
    ) %>%
    dplyr::arrange(position, dplyr::desc(projected_ppr_per_game))

  # --- Optional CSV export ----------------------------------------------------
  if (export_csv) {
    out_dir <- here::here("output", "projections")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    out_path <- file.path(
      out_dir,
      glue("week{current_week}_projection_report.csv")
    )
    readr::write_csv(report, out_path)
    cat(glue("[create_projection_report] Report saved: {out_path}\n\n"))
  }

  cat(glue(
    "[create_projection_report] Complete. {nrow(report)} players. ",
    "{sum(report$boom_bust_recommendation == 'boom', na.rm = TRUE)} boom, ",
    "{sum(report$boom_bust_recommendation == 'bust', na.rm = TRUE)} bust.\n\n"
  ))

  report
}

# ==============================================================================
# FUNCTION 5: backtest_projections()
# ==============================================================================

#' Validate the projection system against historical season outcomes
#'
#' @description
#' Runs the full projection update pipeline historically on a completed season,
#' comparing week-by-week projections to actual observed PPR scores. Reports
#' RMSE, MAE, and correlation by week number, position, and update stage
#' (early vs late season).
#'
#' **Evaluation question:**
#' Does the Bayesian updating mechanism beat naive baselines? Two baselines
#' are computed for every week:
#'   1. Last week's actual score (strong baseline -- NFL consistency is real)
#'   2. Season-to-date average (stable baseline -- regresses toward true rate)
#'
#' Per the Week 4 predictive validity study, a baseline using volume stats
#' (season-to-date rushing yards, receptions) is expected to be competitive.
#' The ensemble-informed projection should beat both baselines by late season
#' (week 9+) when the observed weight dominates. If it does not, that is
#' documented as a finding, not hidden.
#'
#' **Regular season filter (CRITICAL):**
#' Filter applied immediately at data load: `week <= NFL_REGULAR_SEASON_MAX_WEEK`.
#' This resolves the deferred issue from Week 9 where `predicted_week = 22`
#' (Super Bowl) caused join failures with test weeks 15-18. Week 22 is
#' correctly coded in nflfastR; it is outside fantasy relevance scope.
#' The filter is applied once here, not piecemeal in downstream joins.
#'
#' **Column name standardisation:**
#' standardise_prediction_columns() is called on the feature matrix to resolve
#' the `.pred` vs `predicted_ppr` and `ppr_points_next_week` vs `observed_ppr`
#' mismatch from Week 9 artifacts.
#'
#' **Audit checklist compliance:**
#' - success rate: uses `success` column, not `epa > 0` (Issue 3)
#' - team grouping: player_id + season + team before rolling (Issue 1)
#' - no `initial_split()`: temporal filter only (Issue 5)
#' - role_stability_flag: training filtered to stable rows (Issue 6)
#'
#' @param feature_matrix Tibble. Full-season feature matrix from
#'   prepare_model_features() (11_xgboost_fantasy.R). Required columns:
#'   player_id (chr), player_name (chr), position (chr), season (int),
#'   week (int), ppr_points_actual (dbl) [observed outcome],
#'   and all model feature columns.
#'   NOTE: week column must contain NFL week numbers. Values >= 19 are
#'   playoff games and will be filtered out.
#' @param preseason_stats Tibble. Prior-season player statistics for building
#'   the initial prior. Passed to generate_preseason_projections().
#' @param ensemble_models Named list. Output of load_ensemble_models().
#' @param boom_bust_models Named list. Output of load_boom_bust_models().
#' @param backtest_season Integer. Season year being backtested. Used for
#'   labelling and loading the correct bye week schedule.
#' @param bye_week_schedule Named integer vector. Player bye weeks.
#'   Names = player_id, values = bye week number. Default NULL.
#' @param positions Character vector. Positions to evaluate. Default
#'   SUPPORTED_POSITIONS.
#' @param min_games_for_evaluation Integer. Minimum observed-week rows a
#'   player must have to appear in evaluation output. Default 4.
#'
#' @return List with three elements:
#'   - weekly_errors: Tibble of RMSE/MAE/correlation by week and position.
#'     Columns: position (chr), week (int), rmse_ensemble (dbl),
#'     rmse_last_week (dbl), rmse_season_avg (dbl), mae_ensemble (dbl),
#'     n_players (int).
#'   - player_errors: Tibble of season-level RMSE per player.
#'     Columns: player_id (chr), player_name (chr), position (chr),
#'     season_rmse (dbl), season_mae (dbl), n_weeks (int).
#'   - summary_stats: Named list with overall RMSE, MAE, and baseline
#'     comparison results. Includes finding flag if ensemble fails to
#'     beat both baselines.
#'
#' @seealso [generate_preseason_projections()], [update_weekly_projections()]
#'
#' @examples
#' \dontrun{
#' feat_matrix  <- readRDS(here::here("output", "models", "feature_matrix.rds"))
#' prior_stats  <- readRDS(here::here("output", "player_stats_2024.rds"))
#' ensembles    <- load_ensemble_models()
#' boom_bust    <- load_boom_bust_models()
#'
#' results <- backtest_projections(
#'   feature_matrix   = feat_matrix,
#'   preseason_stats  = prior_stats,
#'   ensemble_models  = ensembles,
#'   boom_bust_models = boom_bust,
#'   backtest_season  = 2024,
#'   bye_week_schedule = c("00-0030506" = 11L)
#' )
#'
#' results$weekly_errors %>%
#'   dplyr::filter(position == "WR") %>%
#'   ggplot(aes(x = week)) +
#'   geom_line(aes(y = rmse_ensemble, colour = "Ensemble")) +
#'   geom_line(aes(y = rmse_last_week, colour = "Last Week Actual"))
#' }
#'
#' @export
backtest_projections <- function(feature_matrix,
                                 preseason_stats,
                                 ensemble_models,
                                 boom_bust_models,
                                 backtest_season,
                                 bye_week_schedule   = NULL,
                                 positions           = SUPPORTED_POSITIONS,
                                 min_games_for_evaluation = 4L) {

  # --- Input validation -------------------------------------------------------
  stopifnot(
    is.data.frame(feature_matrix),
    is.data.frame(preseason_stats),
    is.list(ensemble_models),
    is.numeric(backtest_season)
  )

  required_fm_cols <- c("player_id", "week")
  missing_fm <- setdiff(required_fm_cols, names(feature_matrix))
  if (length(missing_fm) > 0) {
    stop(glue(
      "backtest_projections: feature_matrix missing: ",
      "{paste(missing_fm, collapse = ', ')}"
    ))
  }

  # Standardise column names FIRST so position_group -> position is resolved
  # before the position-based filter below.
  feature_matrix <- standardise_prediction_columns(feature_matrix)

  if (!"position" %in% names(feature_matrix)) {
    stop(glue(
      "backtest_projections: feature_matrix has no 'position' column and no ",
      "'position_group' column to derive it from. Check prepare_model_features() output."
    ))
  }

  # CRITICAL: filter to regular season only -- APPLIED ONCE HERE.
  # week 22 = Super Bowl (correctly coded in nflfastR for 17-game era).
  # This resolves the Week 9 artifact join failure for predicted_week = 22.
  n_total  <- nrow(feature_matrix)
  feature_matrix <- feature_matrix %>%
    dplyr::filter(
      week >= 1L,
      week <= NFL_REGULAR_SEASON_MAX_WEEK,
      position %in% positions
    )
  n_postseason_removed <- n_total - nrow(feature_matrix)

  if (n_postseason_removed > 0) {
    cat(glue(
      "[backtest_projections] Removed {n_postseason_removed} rows with ",
      "week > {NFL_REGULAR_SEASON_MAX_WEEK} (playoff games, outside scope). ",
      "Week 22 = Super Bowl -- correctly coded in nflfastR, out of scope here.\n\n"
    ))
  }

  # Check for observed outcome column
  if (!"ppr_points_actual" %in% names(feature_matrix)) {
    # Try alternative column names from Week 9
    if ("observed_ppr" %in% names(feature_matrix)) {
      feature_matrix <- dplyr::rename(
        feature_matrix, ppr_points_actual = observed_ppr
      )
    } else {
      stop(glue(
        "backtest_projections: need 'ppr_points_actual' or 'observed_ppr' ",
        "in feature_matrix. Column not found. Check prepare_model_features() output."
      ))
    }
  }

  # Detect weeks available in feature matrix
  available_weeks <- sort(unique(feature_matrix$week))
  cat(glue(
    "[backtest_projections] Backtesting season {backtest_season}. ",
    "Weeks {min(available_weeks)}-{max(available_weeks)}. ",
    "{nrow(feature_matrix)} player-week rows.\n\n"
  ))

  # --- Generate preseason prior -----------------------------------------------
  cat("[backtest_projections] Generating preseason prior...\n\n")
  current_projections <- generate_preseason_projections(
    prior_season_stats = preseason_stats,
    positions          = positions,
    prior_season       = backtest_season - 1L
  )

  # --- Rolling update loop: week by week through regular season ---------------
  all_weekly_results <- vector("list", length(available_weeks))

  for (i in seq_along(available_weeks)) {
    wk <- available_weeks[i]
    cat(glue("[backtest_projections] Processing week {wk}...\n\n"))

    # Feature matrix for this week only
    feat_wk <- feature_matrix %>%
      dplyr::filter(week == wk)

    # Determine bye week players for this week
    if (!is.null(bye_week_schedule)) {
      bye_this_week <- names(
        bye_week_schedule[bye_week_schedule == wk]
      )
    } else {
      bye_this_week <- character(0)
    }

    # Last-week baseline: actual score from previous week
    if (i > 1) {
      last_wk_actual <- feature_matrix %>%
        dplyr::filter(week == available_weeks[i - 1]) %>%
        dplyr::distinct(player_id, .keep_all = TRUE) %>%
        dplyr::select(player_id, last_week_actual = ppr_points_actual)
    } else {
      last_wk_actual <- feature_matrix %>%
        dplyr::filter(week == wk) %>%
        dplyr::distinct(player_id, .keep_all = TRUE) %>%
        dplyr::select(player_id) %>%
        dplyr::mutate(last_week_actual = NA_real_)
    }

    # Season-to-date average baseline (expanding window, no leakage)
    # Uses weeks BEFORE current week only
    season_avg_baseline <- feature_matrix %>%
      dplyr::filter(week < wk) %>%
      dplyr::distinct(player_id, week, .keep_all = TRUE) %>%
      dplyr::group_by(player_id) %>%
      dplyr::summarise(
        season_avg_to_date = mean(ppr_points_actual, na.rm = TRUE),
        .groups = "drop"
      )

    # Update projections for this week
    current_projections <- tryCatch(
      update_weekly_projections(
        prior_projections = current_projections,
        current_week      = wk,
        ensemble_models   = ensemble_models,
        feature_matrix    = feat_wk,
        bye_week_players  = bye_this_week
      ),
      error = function(e) {
        warning(glue(
          "update_weekly_projections failed at week {wk}: {conditionMessage(e)}. ",
          "Carrying prior forward."
        ))
        current_projections %>%
          dplyr::mutate(week = as.integer(wk))
      }
    )

    # Observed outcomes this week
    observed_wk <- feat_wk %>%
      dplyr::select(player_id, position, ppr_points_actual) %>%
      dplyr::filter(!is.na(ppr_points_actual)) %>%
      dplyr::distinct(player_id, position, .keep_all = TRUE)

    # Join projections to observed
    eval_wk <- current_projections %>%
      dplyr::inner_join(observed_wk, by = c("player_id", "position")) %>%
      dplyr::left_join(last_wk_actual,     by = "player_id") %>%
      dplyr::left_join(season_avg_baseline, by = "player_id") %>%
      dplyr::mutate(
        error_ensemble    = projected_ppr_per_game - ppr_points_actual,
        error_last_week   = last_week_actual - ppr_points_actual,
        error_season_avg  = dplyr::coalesce(season_avg_to_date, projected_ppr_per_game) -
                            ppr_points_actual
      )

    all_weekly_results[[i]] <- eval_wk
  }

  # --- Aggregate evaluation ---------------------------------------------------
  cat("[backtest_projections] Aggregating evaluation metrics...\n\n")

  all_results <- dplyr::bind_rows(all_weekly_results)

  # Weekly RMSE by position
  weekly_errors <- all_results %>%
    dplyr::filter(!is.na(error_ensemble)) %>%
    dplyr::group_by(position, week) %>%
    dplyr::summarise(
      rmse_ensemble   = sqrt(mean(error_ensemble^2,   na.rm = TRUE)),
      rmse_last_week  = sqrt(mean(error_last_week^2,  na.rm = TRUE)),
      rmse_season_avg = sqrt(mean(error_season_avg^2, na.rm = TRUE)),
      mae_ensemble    = mean(abs(error_ensemble),      na.rm = TRUE),
      n_players       = dplyr::n(),
      .groups         = "drop"
    ) %>%
    dplyr::arrange(position, week)

  # Player-level season RMSE
  player_group_cols <- c("player_id", "position")
  if ("player_name" %in% names(all_results)) {
    player_group_cols <- c("player_id", "player_name", "position")
  }

  player_errors <- all_results %>%
    dplyr::filter(!is.na(error_ensemble)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(player_group_cols))) %>%
    dplyr::summarise(
      season_rmse = sqrt(mean(error_ensemble^2, na.rm = TRUE)),
      season_mae  = mean(abs(error_ensemble),   na.rm = TRUE),
      n_weeks     = dplyr::n(),
      .groups     = "drop"
    ) %>%
    dplyr::filter(n_weeks >= min_games_for_evaluation) %>%
    dplyr::arrange(position, season_rmse)

  # Overall summary
  overall_rmse     <- sqrt(mean(all_results$error_ensemble^2,   na.rm = TRUE))
  overall_mae      <- mean(abs(all_results$error_ensemble),      na.rm = TRUE)
  baseline_rmse_lw <- sqrt(mean(all_results$error_last_week^2,  na.rm = TRUE))
  baseline_rmse_sa <- sqrt(mean(all_results$error_season_avg^2, na.rm = TRUE))

  # Late season only (weeks 9-18): ensemble should dominate baselines here
  late_season <- all_results %>% dplyr::filter(week >= 9)
  late_rmse_ensemble <- sqrt(mean(late_season$error_ensemble^2,   na.rm = TRUE))
  late_rmse_lw       <- sqrt(mean(late_season$error_last_week^2,  na.rm = TRUE))
  late_rmse_sa       <- sqrt(mean(late_season$error_season_avg^2, na.rm = TRUE))

  # Document result honestly -- do not hide if ensemble doesn't beat baselines
  beats_last_week   <- late_rmse_ensemble < late_rmse_lw
  beats_season_avg  <- late_rmse_ensemble < late_rmse_sa
  beats_both        <- beats_last_week && beats_season_avg

  cat(glue(
    "[backtest_projections] === RESULTS (Season {backtest_season}) ===\n",
    "  Overall RMSE (ensemble):       {round(overall_rmse, 3)}\n",
    "  Overall RMSE (last week):      {round(baseline_rmse_lw, 3)}\n",
    "  Overall RMSE (season avg):     {round(baseline_rmse_sa, 3)}\n",
    "  Late-season RMSE (ensemble):   {round(late_rmse_ensemble, 3)}\n",
    "  Late-season RMSE (last week):  {round(late_rmse_lw, 3)}\n",
    "  Late-season RMSE (season avg): {round(late_rmse_sa, 3)}\n",
    "  Beats last-week baseline (late season): {beats_last_week}\n",
    "  Beats season-avg baseline (late season): {beats_season_avg}\n\n"
  ))

  if (!beats_both) {
    cat(glue(
      "  FINDING: Ensemble does not beat both baselines in late season.\n",
      "  This is a documented result, not a failure to hide.\n",
      "  Possible causes: feature leakage audit incomplete (Week 10 deferred),\n",
      "  role_stability_flag not applied to training rows (Issue 6 in audit),\n",
      "  or many-to-many join producing duplicate training rows.\n\n"
    ))
  }

  summary_stats <- list(
    backtest_season      = backtest_season,
    overall_rmse         = overall_rmse,
    overall_mae          = overall_mae,
    baseline_rmse_lw     = baseline_rmse_lw,
    baseline_rmse_sa     = baseline_rmse_sa,
    late_rmse_ensemble   = late_rmse_ensemble,
    late_rmse_lw         = late_rmse_lw,
    late_rmse_sa         = late_rmse_sa,
    beats_last_week      = beats_last_week,
    beats_season_avg     = beats_season_avg,
    beats_both_baselines = beats_both,
    n_players_evaluated  = dplyr::n_distinct(all_results$player_id),
    n_weeks_evaluated    = dplyr::n_distinct(all_results$week)
  )

  list(
    weekly_errors = weekly_errors,
    player_errors = player_errors,
    summary_stats = summary_stats
  )
}

# ==============================================================================
# FUNCTION 6: run_full_pipeline()
# ==============================================================================

#' Run the complete projection pipeline end-to-end
#'
#' @description
#' Single-function entry point for the full projection system. Loads all
#' artifacts, generates preseason projections, runs weekly updates through
#' the current week, produces the ROS projection and formatted report.
#'
#' Designed for reproducibility: one function call regenerates the full output
#' from saved artifacts. All intermediate results are optionally saved to disk.
#'
#' **NFL season scope:**
#' current_week must be 1-18 (regular season). Playoff weeks (>= 19) are
#' outside projection scope and will produce a hard stop.
#'
#' **Execution time:**
#' Generating projections for all weeks 1 through current_week requires
#' one ensemble predict() call per position per week. For week 18 this is
#' 3 positions x 18 weeks = 54 predict calls. Each call takes 1-5 seconds
#' depending on feature matrix size. Total runtime: 1-5 minutes for a
#' full-season run.
#'
#' @param current_week Integer. NFL week through which to run updates (1-18).
#' @param preseason_stats Tibble. Prior-season player statistics. Passed to
#'   generate_preseason_projections().
#' @param feature_matrix Tibble. Full season feature matrix from
#'   prepare_model_features(). Must include all weeks 1:current_week.
#' @param bye_week_schedule Named integer vector. Player bye weeks.
#'   Names = player_id, values = bye week number. Default NULL.
#' @param opponent_difficulty Tibble. Optional schedule difficulty for ROS.
#'   Default NULL.
#' @param positions Character vector. Default SUPPORTED_POSITIONS.
#' @param export_report Logical. If TRUE, writes final report to
#'   output/projections/. Default TRUE.
#' @param save_intermediates Logical. If TRUE, saves each week's projection
#'   tibble to output/projections/weekly/. Default FALSE.
#' @param roster_season Integer. Season for nflreadr roster lookup used to
#'   resolve real NFL positions (QB/RB/WR/TE) in the final report.
#'   Default 2025L. Pass 2024L when backtesting on prior-season data.
#'
#' @return Named list:
#'   - preseason_projections: Output of generate_preseason_projections()
#'   - current_projections: Output of final update_weekly_projections()
#'   - ros_projections: Output of generate_ros_projections()
#'   - report: Output of create_projection_report()
#'
#' @examples
#' \dontrun{
#' feat_matrix <- readRDS(here::here("output", "models", "feature_matrix.rds"))
#' prior_stats <- readRDS(here::here("output", "player_stats_2024.rds"))
#'
#' pipeline_output <- run_full_pipeline(
#'   current_week      = 10L,
#'   preseason_stats   = prior_stats,
#'   feature_matrix    = feat_matrix,
#'   bye_week_schedule = c("00-0030506" = 11L),
#'   export_report     = TRUE
#' )
#'
#' # Top WR projections this week
#' pipeline_output$report %>%
#'   dplyr::filter(position == "WR", !is_bye_week) %>%
#'   dplyr::select(player_name, player_position, projected_ppr_per_game,
#'                 boom_probability, matchup_flag) %>%
#'   print(n = 20)
#'
#' # Filter to TEs only (player_position distinguishes WR from TE)
#' pipeline_output$report %>%
#'   dplyr::filter(player_position == "TE") %>%
#'   dplyr::select(player_name, player_position, projected_ppr_per_game) %>%
#'   print(n = 10)
#' }
#'
#' @export
run_full_pipeline <- function(current_week,
                               preseason_stats,
                               feature_matrix,
                               bye_week_schedule   = NULL,
                               opponent_difficulty = NULL,
                               positions           = SUPPORTED_POSITIONS,
                               export_report       = TRUE,
                               save_intermediates  = FALSE,
                               roster_season       = 2025L) {

  # --- Scope enforcement ------------------------------------------------------
  validate_regular_season_week(current_week)

  cat(glue(
    "[run_full_pipeline] Starting. Week {current_week}, ",
    "positions: {paste(positions, collapse = ', ')}.\n\n"
  ))

  # --- Load artifacts ---------------------------------------------------------
  cat("[run_full_pipeline] Loading ensemble and boom/bust models...\n\n")
  ensemble_models  <- load_ensemble_models()
  boom_bust_models <- load_boom_bust_models()

  # Standardise column names FIRST (position_group -> position, etc.)
  # Must run before the position-based filter below.
  feature_matrix <- standardise_prediction_columns(feature_matrix)

  # Regular season filter applied once (resolves week 22 join issue)
  feature_matrix <- feature_matrix %>%
    dplyr::filter(
      week >= 1L,
      week <= NFL_REGULAR_SEASON_MAX_WEEK,
      position %in% positions
    )

  # --- Preseason prior --------------------------------------------------------
  cat("[run_full_pipeline] Generating preseason prior...\n\n")
  preseason_proj <- generate_preseason_projections(
    prior_season_stats = preseason_stats,
    positions          = positions
  )

  # --- Weekly update loop -----------------------------------------------------
  proj <- preseason_proj
  all_weeks <- seq_len(current_week)

  if (save_intermediates) {
    int_dir <- here::here("output", "projections", "weekly")
    if (!dir.exists(int_dir)) dir.create(int_dir, recursive = TRUE)
  }

  for (wk in all_weeks) {
    feat_wk <- feature_matrix %>% dplyr::filter(week == wk)

    if (nrow(feat_wk) == 0) {
      cat(glue("  [run_full_pipeline] No feature data for week {wk}, skipping.\n\n"))
      next
    }

    bye_this_week <- if (!is.null(bye_week_schedule)) {
      names(bye_week_schedule[bye_week_schedule == wk])
    } else {
      character(0)
    }

    proj <- update_weekly_projections(
      prior_projections = proj,
      current_week      = wk,
      ensemble_models   = ensemble_models,
      feature_matrix    = feat_wk,
      bye_week_players  = bye_this_week
    )

    if (save_intermediates) {
      saveRDS(
        proj,
        file.path(int_dir, glue("projections_week{wk}.rds"))
      )
    }
  }

  # --- ROS projections --------------------------------------------------------
  cat("[run_full_pipeline] Generating ROS projections...\n\n")
  ros <- generate_ros_projections(
    current_projections = proj,
    current_week        = current_week,
    bye_week_schedule   = bye_week_schedule,
    opponent_difficulty = opponent_difficulty
  )

  # --- Final report -----------------------------------------------------------
  cat("[run_full_pipeline] Building projection report...\n\n")
  feat_current <- feature_matrix %>% dplyr::filter(week == current_week)

  report <- create_projection_report(
    weekly_projections  = proj,
    ros_projections     = ros,
    boom_bust_models    = boom_bust_models,
    feature_matrix      = feat_current,
    opponent_difficulty = opponent_difficulty,
    roster_season       = roster_season,
    export_csv          = export_report
  )

  cat(glue(
    "[run_full_pipeline] Complete. ",
    "{nrow(report)} players in final report.\n\n"
  ))

  list(
    preseason_projections = preseason_proj,
    current_projections   = proj,
    ros_projections       = ros,
    report                = report
  )
}
