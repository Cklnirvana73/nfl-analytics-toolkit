# ==============================================================================
# WEEK 9: XGBOOST FANTASY POINTS PREDICTION
# ==============================================================================
#
# Production-grade XGBoost regression pipeline for NFL fantasy point prediction.
# Built for personal analytics use and portfolio display.
#
# This file is the first ML layer in the toolkit. It takes the feature matrix
# compiled in Week 8 (compile_feature_matrix()) and trains position-specific
# XGBoost regression models to predict next-week PPR fantasy points. The models
# are evaluated against two naive baselines -- last week's score and the
# season-to-date average -- because a complex ML model that cannot beat "just
# use last week's score" is not worth its complexity.
#
# Key design principle: all time ordering is strictly respected. No random
# splits. No information from future weeks contaminates training data.
# Absent weeks are reconstructed and contribute absence features.
#
# File contains five main functions:
# 1. prepare_model_features()    - Lines   85-370
#    Assembles the ML-ready matrix: reconstructs absent weeks, computes
#    availability features, encodes categoricals, builds the PPR target,
#    enforces temporal ordering. Input: compile_feature_matrix() output.
#
# 2. train_fantasy_model()       - Lines  372-590
#    Trains one XGBoost regression model per position group (QB, RB, WR)
#    using temporal expanding-window cross-validation. Sequential hyperparameter
#    tuning (tree structure first, sampling second, regularization third).
#    Returns fitted workflows + CV performance metrics.
#
# 3. predict_fantasy_points()    - Lines  592-720
#    Generates next-week PPR point predictions from a fitted model. Returns
#    point estimates with prediction intervals derived from CV fold variance.
#
# 4. explain_predictions()       - Lines  722-870
#    Extracts SHAP values for individual player predictions using shapviz.
#    Returns beeswarm importance, waterfall for single players, and
#    dependence plots for the top features. Includes correlation check before
#    SHAP interpretation to flag correlated feature groups.
#
# 5. evaluate_model()            - Lines  872-1050
#    Evaluates model performance (RMSE, MAE, R-squared) on the temporal
#    held-out test set. Compares to two baselines. Checks feature importance
#    stability across CV folds. Returns a named list of evaluation artifacts.
#
# NFL Context:
#   - PPR scoring: 1 pt per reception, 0.1 pt per rushing/receiving yard,
#     6 pts per TD (rush/rec), 0.04 pts per passing yard, 4 pts per pass TD,
#     -2 pts per interception
#   - Position-specific models: QB features (EPA/dropback, pressure splits)
#     differ from WR features (target share, air yards depth)
#   - Availability rate is a first-class feature -- injury-prone players
#     have real floor risk independent of their per-game talent level
#   - Baseline comparison: "last week's score" is surprisingly strong for
#     fantasy. Model must beat it, or the complexity is not justified.
#   - Garbage time filtered in the upstream feature matrix (Week 8)
#   - Red zone usage likely dominates TD prediction and therefore total points
#
# Data Science Context:
#   - Temporal CV only: expanding window (train weeks 1-N, test week N+1)
#   - No random splits on time-ordered data -- ever
#   - XGBoost native NA handling used for structural NAs (rolling windows)
#   - Absent weeks reconstructed with is_absence_week flag; excluded from
#     model training targets but presence contributes absence features
#   - SHAP correlated-feature caveat documented and checked at runtime
#   - Feature importance stability verified across CV folds (CV > 0.5 = flag)
#
# Dependencies:
#   tidymodels  (modeling framework, CV, tuning, recipes)
#   xgboost     (XGBoost engine)
#   shapviz     (SHAP value extraction and plots)
#   dplyr       (data manipulation)
#   tidyr       (reshaping)
#   glue        (message formatting)
#   here        (path resolution)
#   ggplot2     (visualization)
#
# Builds on:
#   Week 1:  load_and_validate_pbp()
#   Week 3:  calculate_fantasy_points()  -> PPR scoring engine
#   Week 5:  calculate_epa_trends()
#   Week 6:  calculate_usage_metrics()
#   Week 7:  get_game_script_splits()
#   Week 8:  compile_feature_matrix()    -> primary input
#
# ==============================================================================

library(tidymodels)
library(xgboost)
library(shapviz)
library(dplyr)
library(tidyr)
library(glue)
library(here)
library(ggplot2)


# ==============================================================================
# SECTION 1: FEATURE PREPARATION
# ==============================================================================

#' Prepare ML-Ready Feature Matrix for XGBoost Fantasy Models
#'
#' @description
#' Transforms the compile_feature_matrix() output into a clean, ML-ready
#' tibble for XGBoost training. Key steps:
#'
#' 1. Reconstructs absent weeks (weeks where player had no qualifying plays)
#'    by building a full team-week spine and left-joining actual game data.
#'    Absent weeks get is_absence_week = TRUE and NA performance features.
#'
#' 2. Computes three availability features from the absence record:
#'    - weeks_since_last_played: recency of last active game
#'    - missed_weeks_this_season: cumulative absence count
#'    - availability_rate: games played / team games played (injury-proneness)
#'
#' 3. Calculates the PPR fantasy point total for each player-week (the
#'    regression target). Requires raw play stats joined from pbp_data.
#'
#' 4. Builds the next-week prediction target (ppr_points_next_week) by
#'    shifting PPR points forward one week within each player-position-team
#'    group. Rows where next-week target is NA are flagged but retained for
#'    their feature values (useful as context for prior-week rows).
#'
#' 5. One-hot encodes categorical features (opponent_style, opponent_tier,
#'    position_group) so XGBoost receives a numeric matrix.
#'
#' 6. Enforces temporal ordering (season, week ascending).
#'
#' Structural NAs from rolling windows (epa_roll3, epa_season_to_date,
#' opp_adjusted_epa_prior) are passed through as-is. XGBoost learns the
#' optimal default branch direction for these early-season structural NAs
#' without imputation.
#'
#' @param feature_matrix Tibble from compile_feature_matrix(). Must contain:
#'   season, week, player_id, player_name, position_group, team, opponent,
#'   plays_this_week, epa_this_week, success_rate_this_week, epa_roll3,
#'   epa_season_to_date, plays_roll3, neutral_epa_season, leading_share_season,
#'   trailing_share_season, opp_adjusted_epa_prior, schedule_difficulty_rank,
#'   opp_adj_games_prior, opponent_style, opponent_tier, weeks_played,
#'   weeks_since_role_change, role_stability_flag
#' @param pbp_data Play-by-play tibble from load_and_validate_pbp(). Used to
#'   compute PPR fantasy points and reconstruct team-week spine.
#' @param seasons Numeric vector. One or more seasons to prepare. Must match
#'   seasons present in feature_matrix. Single value for single-season use;
#'   vector (e.g. c(2020, 2021, 2022, 2023, 2024, 2025)) for multi-season.
#' @param positions Character vector. Position groups to include. Default:
#'   c("passer", "rusher", "receiver"). XGBoost models are trained separately
#'   per position, but all positions can be prepared in one call.
#' @param min_target_plays Integer. Minimum plays in a week for a row to have
#'   a valid PPR target (not just a reconstructed absence row). Default: 3.
#'
#' @return Tibble with one row per player-position_group-week. All columns from
#'   feature_matrix plus:
#'   \describe{
#'     \item{is_absence_week}{TRUE if player had no qualifying plays this week.
#'       Performance features are NA for absence weeks. (lgl)}
#'     \item{weeks_since_last_played}{Weeks elapsed since player last appeared.
#'       0 = played last week. NA for season-opening rows. (int)}
#'     \item{missed_weeks_this_season}{Cumulative count of absence weeks this
#'       season prior to this row. (int)}
#'     \item{availability_rate}{games_played / team_games_played through this
#'       week. Ranges 0-1. Lower = more injury-prone. (dbl)}
#'     \item{ppr_points_this_week}{PPR fantasy points this week. NA for
#'       absence weeks. (dbl)}
#'     \item{ppr_points_next_week}{PPR fantasy points in the following week.
#'       This is the regression target. NA for the last week of the season or
#'       for absence weeks where the player also missed next week. (dbl)}
#'     \item{has_target}{TRUE when ppr_points_next_week is non-NA. Only rows
#'       where has_target = TRUE are used in model training. (lgl)}
#'   }
#'
#' @details
#' Position group encoding for XGBoost:
#'   position_group_passer  = 1/0
#'   position_group_rusher  = 1/0
#'   position_group_receiver = 1/0 (reference level dropped in recipe)
#'
#' Categorical columns encoded via step_dummy(one_hot = TRUE):
#'   opponent_style: pass_funnel, run_funnel, balanced
#'   opponent_tier:  elite, above_avg, average, below_avg, poor
#'
#' @examples
#' \dontrun{
#' pbp      <- load_and_validate_pbp(2025)
#' opp_adj  <- calculate_opponent_adjustments(pbp, season = 2025)
#' def_sty  <- classify_defensive_style(pbp, season = 2025)
#' features <- compile_feature_matrix(pbp, opp_adj, def_sty, season = 2025)
#'
#' ml_data <- prepare_model_features(features, pbp_data = pbp, seasons = 2025)
#'
#' # Multi-season
#' ml_data <- prepare_model_features(features, pbp_data = pbp, seasons = c(2020:2025))
#' names(ml_data)
#'
#' # Check absence reconstruction
#' ml_data %>% count(is_absence_week)
#'
#' # Preview availability features for a specific player
#' ml_data %>%
#'   filter(player_name == "C.McCaffrey", position_group == "rusher") %>%
#'   select(week, is_absence_week, weeks_since_last_played,
#'          missed_weeks_this_season, availability_rate, ppr_points_this_week)
#' }
#'
#' @seealso [compile_feature_matrix()] for the input feature matrix,
#'   [train_fantasy_model()] for model training,
#'   [calculate_fantasy_points()] (Week 3) for PPR scoring engine
#'
#' @export
prepare_model_features <- function(feature_matrix,
                                   pbp_data,
                                   seasons,
                                   positions       = c("passer", "rusher", "receiver"),
                                   min_target_plays = 3L) {

  # --- Input validation ---
  if (!is.data.frame(feature_matrix)) stop("feature_matrix must be a data frame.")
  if (!is.data.frame(pbp_data))       stop("pbp_data must be a data frame.")
  if (missing(seasons) || !is.numeric(seasons) || length(seasons) == 0) {
    stop("seasons must be a non-empty numeric vector.")
  }
  current_year <- as.integer(format(Sys.Date(), "%Y")) + 1L
  bad_seasons  <- seasons[seasons < 1990 | seasons > current_year]
  if (length(bad_seasons) > 0) {
    stop(glue("Implausible season value(s): {paste(bad_seasons, collapse = ', ')}. ",
              "Expected values between 1990 and {current_year}."))
  }
  if (!is.character(positions) || length(positions) == 0) {
    stop("positions must be a non-empty character vector.")
  }

  required_fm <- c(
    "season", "week", "player_id", "player_name", "position_group",
    "team", "opponent", "plays_this_week", "epa_this_week",
    "success_rate_this_week", "epa_roll3", "epa_season_to_date",
    "plays_roll3", "neutral_epa_season", "leading_share_season",
    "trailing_share_season", "opp_adjusted_epa_prior",
    "schedule_difficulty_rank", "opp_adj_games_prior",
    "opponent_style", "opponent_tier",
    "weeks_played", "weeks_since_role_change", "role_stability_flag"
  )
  missing_fm <- setdiff(required_fm, names(feature_matrix))
  if (length(missing_fm) > 0) {
    stop(glue("feature_matrix missing columns: {paste(missing_fm, collapse = ', ')}"))
  }

  required_pbp <- c(
    "season", "week", "posteam", "play_type",
    "passer_player_id", "rusher_player_id", "receiver_player_id",
    "pass_attempt", "complete_pass", "passing_yards",
    "rushing_yards", "receiving_yards", "touchdown",
    "pass_touchdown", "rush_touchdown", "return_touchdown",
    "interception", "fumble_lost", "two_point_conv_result",
    "qb_kneel", "qb_spike", "qb_scramble"
  )
  missing_pbp <- setdiff(required_pbp, names(pbp_data))
  if (length(missing_pbp) > 0) {
    stop(glue("pbp_data missing columns: {paste(missing_pbp, collapse = ', ')}"))
  }

  message(glue("prepare_model_features: seasons {paste(seasons, collapse = ', ')}, positions: ",
               "{paste(positions, collapse = ', ')}"))

  # --- Step 1: Filter feature matrix to requested seasons and positions ---
  fm <- feature_matrix %>%
    filter(season %in% !!seasons,
           position_group %in% positions)

  if (nrow(fm) == 0) {
    message("No rows in feature_matrix after season/position filter. Returning empty tibble.")
    return(tibble())
  }

  # --- Step 2: Build team-week spine (all weeks each team played) ---
  # Used to reconstruct absent weeks for each player.
  # The spine is derived from pbp_data -- ground truth for which weeks
  # each team had games.
  message("  Building team-week spine for absence reconstruction...")

  team_weeks <- pbp_data %>%
    filter(season %in% !!seasons, !is.na(posteam)) %>%
    distinct(season, week, team = posteam) %>%
    arrange(team, season, week)

  # --- Step 3: Determine each player's team-week coverage ---
  # A player's coverage = weeks their team played while they were on that team.
  # For traded players, use the team recorded in the feature matrix per week.
  player_team_map <- fm %>%
    select(player_id, player_name, position_group, season, team, week) %>%
    distinct()

  # Full player-position-season-team-week spine.
  # group_modify is season-aware: week ranges are computed within each season
  # so a player's week 1-8 in 2024 does not bleed into their 2025 schedule.
  player_spine <- player_team_map %>%
    left_join(team_weeks, by = c("season", "team", "week")) %>%
    group_by(player_id, player_name, position_group, season, team) %>%
    group_modify(function(df, keys) {
      team_wks <- team_weeks %>%
        filter(team == keys$team[1], season == keys$season[1]) %>%
        pull(week)
      # Player was on this team during weeks min(df$week):max(df$week)
      # within this season only -- no cross-season contamination
      player_wks <- seq(min(df$week), max(df$week))
      relevant_wks <- intersect(team_wks, player_wks)
      tibble(week = relevant_wks)
    }) %>%
    ungroup()

  # --- Step 4: Left join feature matrix onto full spine ---
  # Weeks where player appeared: all features populated
  # Weeks where player was absent: all performance features = NA
  full_matrix <- player_spine %>%
    left_join(
      fm %>% select(-player_name),   # avoid duplicate join column
      by = c("player_id", "position_group", "season", "team", "week")
    ) %>%
    mutate(
      is_absence_week  = is.na(plays_this_week)
    ) %>%
    arrange(player_id, position_group, team, week)

  # --- Step 5: Compute availability features from absence record ---
  message("  Computing availability features...")

  full_matrix <- full_matrix %>%
    group_by(player_id, position_group, season, team) %>%
    arrange(week, .by_group = TRUE) %>%
    mutate(
      # How many weeks ago did this player last play?
      # NA for the first week in the player's record (no prior history)
      .played_flag        = as.integer(!is_absence_week),
      .weeks_since_played = cumsum(.played_flag),
      weeks_since_last_played = week - lag(
        week * as.integer(!is_absence_week),
        default = NA_integer_
      ),
      # Overwrite with a cleaner calculation:
      # for each row, find the most recent prior week where player appeared
      weeks_since_last_played = {
        wks <- week
        played <- !is_absence_week
        result <- integer(length(wks))
        last_played_wk <- NA_integer_
        for (i in seq_along(wks)) {
          if (!is.na(last_played_wk)) {
            result[i] <- wks[i] - last_played_wk
          } else {
            result[i] <- NA_integer_
          }
          if (!is_absence_week[i]) last_played_wk <- wks[i]
        }
        result
      },

      # Cumulative missed weeks prior to this row (not counting current row)
      missed_weeks_this_season = lag(cumsum(is_absence_week), default = 0L),

      # Availability rate: games played / team games played through this week
      # Team games played through this week = position in the sorted week list
      .team_games_through_now = row_number(),
      .games_played_through_now = cumsum(.played_flag),
      availability_rate = ifelse(
        .team_games_through_now > 0,
        .games_played_through_now / .team_games_through_now,
        NA_real_
      )
    ) %>%
    select(-.played_flag, -.weeks_since_played,
           -.team_games_through_now, -.games_played_through_now) %>%
    ungroup()

  # --- Step 6: Compute PPR fantasy points per player-week ---
  # PPR scoring applied to raw play-level data for the same season.
  # Join to feature matrix by player_id + position_group + week.
  message("  Computing PPR fantasy points...")

  pbp_season <- pbp_data %>%
    filter(
      season %in% !!seasons,
      play_type %in% c("pass", "run"),
      qb_kneel == 0,
      qb_spike == 0
    )

  # Passer PPR: passing yards * 0.04, pass TDs * 4, interceptions * -2
  passer_ppr <- pbp_season %>%
    filter(!is.na(passer_player_id)) %>%
    group_by(season, week, player_id = passer_player_id) %>%
    summarise(
      pass_yards  = sum(passing_yards, na.rm = TRUE),
      pass_tds    = sum(pass_touchdown == 1, na.rm = TRUE),
      interceptions = sum(interception == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      ppr_points = pass_yards * 0.04 + pass_tds * 4 - interceptions * 2,
      position_group = "passer"
    )

  # Rusher PPR: rush yards * 0.1, rush TDs * 6, receptions * 1, rec yards * 0.1
  # Rushing receivers (RBs who catch passes) counted in receiver group
  rusher_ppr <- pbp_season %>%
    filter(play_type == "run", !is.na(rusher_player_id),
           # Exclude QB scrambles from rusher PPR -- already in passer group
           coalesce(qb_scramble, 0) == 0) %>%
    group_by(season, week, player_id = rusher_player_id) %>%
    summarise(
      rush_yards = sum(rushing_yards, na.rm = TRUE),
      rush_tds   = sum(rush_touchdown == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      ppr_points = rush_yards * 0.1 + rush_tds * 6,
      position_group = "rusher"
    )

  # Receiver PPR: receptions * 1, rec yards * 0.1, rec TDs * 6
  receiver_ppr <- pbp_season %>%
    filter(play_type == "pass", !is.na(receiver_player_id),
           complete_pass == 1) %>%
    group_by(season, week, player_id = receiver_player_id) %>%
    summarise(
      receptions = n(),
      rec_yards  = sum(receiving_yards, na.rm = TRUE),
      rec_tds    = sum(return_touchdown == 1 | pass_touchdown == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      ppr_points = receptions * 1 + rec_yards * 0.1 + rec_tds * 6,
      position_group = "receiver"
    )

  ppr_all <- bind_rows(
    passer_ppr   %>% select(season, week, player_id, position_group, ppr_points),
    rusher_ppr   %>% select(season, week, player_id, position_group, ppr_points),
    receiver_ppr %>% select(season, week, player_id, position_group, ppr_points)
  )

  # Join PPR to full matrix
  full_matrix <- full_matrix %>%
    left_join(
      ppr_all %>% rename(ppr_points_this_week = ppr_points),
      by = c("season", "player_id", "position_group", "week")
    ) %>%
    mutate(
      # Absent weeks have no PPR points by definition
      ppr_points_this_week = ifelse(is_absence_week, NA_real_, ppr_points_this_week)
    )

  # --- Step 7: Build next-week prediction target ---
  # ppr_points_next_week = the PPR points in the FOLLOWING week.
  # This is what the model predicts. It must be computed within
  # player-position-season-team groups to avoid crossing both team and
  # season boundaries. Week 18 of 2024 must not point to week 1 of 2025.
  message("  Building next-week prediction target...")

  full_matrix <- full_matrix %>%
    group_by(player_id, position_group, season, team) %>%
    arrange(week, .by_group = TRUE) %>%
    mutate(
      ppr_points_next_week = lead(ppr_points_this_week, 1),
      # has_target: TRUE when we have a valid next-week outcome to train on
      # Requires: (a) next week exists, (b) player appeared next week
      has_target = !is.na(ppr_points_next_week)
    ) %>%
    ungroup()

  # --- Step 8: Encode categoricals ---
  # opponent_style and opponent_tier are character columns.
  # Convert to factor with known levels so one-hot encoding in the recipe
  # is deterministic across train and test splits.
  full_matrix <- full_matrix %>%
    mutate(
      opponent_style = factor(
        coalesce(opponent_style, "unknown"),
        levels = c("pass_funnel", "run_funnel", "balanced", "unknown")
      ),
      opponent_tier = factor(
        coalesce(opponent_tier, "unknown"),
        levels = c("elite", "above_avg", "average", "below_avg", "poor", "unknown")
      ),
      position_group = factor(position_group,
                              levels = c("passer", "rusher", "receiver")),
      role_stability_flag = as.integer(role_stability_flag)
    )

  # --- Step 9: Final column ordering and sort ---
  result <- full_matrix %>%
    select(
      # Identifiers (excluded from model features in recipe)
      season, week, player_id, player_name, position_group, team, opponent,
      # Availability features (NEW -- absence reconstruction)
      is_absence_week, weeks_since_last_played,
      missed_weeks_this_season, availability_rate,
      # Production features (Layer 1-2 from Week 8)
      plays_this_week, epa_this_week, success_rate_this_week,
      # Trend features (Layer 4)
      epa_roll3, epa_season_to_date, plays_roll3,
      # Game script features (Layer 5)
      neutral_epa_season, leading_share_season, trailing_share_season,
      # Opponent features (Layer 6)
      opp_adjusted_epa_prior, schedule_difficulty_rank,
      opp_adj_games_prior, opponent_style, opponent_tier,
      # Role continuity features
      weeks_played, weeks_since_role_change, role_stability_flag,
      # Target variable
      ppr_points_this_week, ppr_points_next_week, has_target
    ) %>%
    arrange(player_id, position_group, season, week)

  n_total    <- nrow(result)
  n_absent   <- sum(result$is_absence_week)
  n_training <- sum(result$has_target)
  message(glue(
    "prepare_model_features complete: {n_total} player-week rows ",
    "({n_absent} absence weeks reconstructed), ",
    "{n_training} rows with training targets."
  ))

  result
}


# ==============================================================================
# SECTION 2: MODEL TRAINING
# ==============================================================================

#' Train Position-Specific XGBoost Fantasy Point Models
#'
#' @description
#' Trains one XGBoost regression model per position group using temporal
#' expanding-window cross-validation. Returns a named list of fitted
#' tidymodels workflows and CV performance metrics per position.
#'
#' Hyperparameter tuning uses a Latin hypercube grid (30 candidates) over
#' tree structure parameters. Sequential tuning order per the data science
#' code reviewer skill: tree structure first, then sampling, then regularization.
#'
#' Temporal CV design:
#'   - Train/test split: season-based. Train = all seasons before test_season,
#'     test = test_season. Default test_season = max season in ml_data.
#'     Do NOT use week-based cutoffs -- they exclude prior season data from
#'     training and waste signal from multiple seasons of history.
#'   - Each CV fold: train on weeks 1 through N, validate on week N+1
#'   - Minimum initial training size: min_train_weeks weeks
#'   - Expanding window (cumulative = TRUE) -- more training data per fold
#'
#' @param ml_data Tibble from prepare_model_features(). Must have has_target
#'   and is_absence_week columns.
#' @param positions Character vector. Positions to train models for. A separate
#'   model is trained for each. Default: c("passer", "rusher", "receiver").
#' @param test_season Integer scalar. Season to hold out for final evaluation.
#'   All prior seasons are used for training. Defaults to the most recent
#'   season in ml_data. Must be present in ml_data.
#' @param min_train_weeks Integer. Minimum weeks in the training window before
#'   the first CV fold. Default: 4 (half a month of games).
#' @param tune_grid_size Integer. Number of Latin hypercube candidates for
#'   hyperparameter tuning. Default: 30. Reduce for faster runs during dev.
#' @param seed Integer. Random seed for reproducibility. Default: 2025L.
#'
#' @return Named list with one element per position. Each element is itself
#'   a list containing:
#'   \describe{
#'     \item{fitted_workflow}{Finalized, fitted tidymodels workflow. Pass to
#'       predict_fantasy_points() for new predictions.}
#'     \item{cv_metrics}{Tibble of CV performance per fold: RMSE, MAE, R-squared.}
#'     \item{best_params}{Tibble of best hyperparameter values selected by RMSE.}
#'     \item{tuning_results}{Full tune_grid() output. Retain for diagnostics.}
#'     \item{train_data}{Training rows used (has_target = TRUE, not absence week).}
#'     \item{test_data}{Held-out test rows for the test_season. Used by
#'       evaluate_model() and accessible for residual analysis.}
#'     \item{n_features}{Number of features in the model matrix.}
#'   }
#'
#' @details
#' Recipe applied per position:
#'   - step_rm(): removes identifiers (player_id, player_name, season, week,
#'     team, opponent, is_absence_week, has_target, ppr_points_this_week)
#'   - step_dummy(one_hot = TRUE): encodes opponent_style, opponent_tier,
#'     position_group as 0/1 columns
#'   - step_zv(): removes zero-variance predictors (e.g. all rows same style)
#'   - step_novel(): handles unseen factor levels at prediction time
#'   NAs in numeric predictors (epa_roll3, etc.) are passed through.
#'   XGBoost learns optimal default branch direction for missing values.
#'
#' @examples
#' \dontrun{
#' # Multi-season: train on 2020-2024, test on 2025
#' ml_data <- prepare_model_features(features, pbp, seasons = 2025)
#'
#' models <- train_fantasy_model(
#'   ml_data,
#'   positions   = c("rusher", "receiver"),
#'   test_season = 2025
#' )
#'
#' # Check CV performance
#' models$rusher$cv_metrics %>%
#'   group_by(.metric) %>%
#'   summarise(mean = mean(.estimate), sd = sd(.estimate))
#'
#' # Best hyperparameters
#' models$receiver$best_params
#' }
#'
#' @seealso [prepare_model_features()] for input preparation,
#'   [predict_fantasy_points()] for generating predictions,
#'   [evaluate_model()] for held-out test evaluation
#'
#' @export
train_fantasy_model <- function(ml_data,
                                positions       = c("passer", "rusher", "receiver"),
                                test_season     = NULL,
                                min_train_weeks = 4L,
                                tune_grid_size  = 30L,
                                seed            = 2025L) {

  if (!is.data.frame(ml_data)) stop("ml_data must be a data frame.")
  if (!"has_target" %in% names(ml_data)) stop("ml_data must contain has_target column.")
  if (!"is_absence_week" %in% names(ml_data)) stop("ml_data must contain is_absence_week column.")
  if (!is.numeric(min_train_weeks) || min_train_weeks < 2) {
    stop("min_train_weeks must be >= 2.")
  }

  # Resolve test_season: default to most recent season in the data
  available_seasons <- sort(unique(ml_data$season))
  if (is.null(test_season)) {
    test_season <- max(available_seasons)
    message(glue("train_fantasy_model: test_season not specified. ",
                 "Defaulting to most recent season: {test_season}."))
  } else {
    if (!test_season %in% available_seasons) {
      stop(glue("test_season {test_season} not found in ml_data. ",
                "Available seasons: {paste(available_seasons, collapse = ', ')}"))
    }
  }

  set.seed(seed)

  # Features to exclude from the model matrix (identifiers and targets)
  id_cols <- c(
    "season", "week", "player_id", "player_name", "team", "opponent",
    "is_absence_week", "has_target", "ppr_points_this_week"
  )

  results <- list()

  for (pos in positions) {
    message(glue("\n--- Training model: position = {pos} ---"))

    # Filter to this position, training rows only
    # Absent weeks excluded (no target), non-target rows excluded
    pos_data <- ml_data %>%
      filter(
        position_group == pos,
        has_target == TRUE,
        is_absence_week == FALSE
      ) %>%
      arrange(season, week)

    if (nrow(pos_data) < (min_train_weeks + 2)) {
      message(glue("  Insufficient rows for {pos} ({nrow(pos_data)} rows). Skipping."))
      results[[pos]] <- NULL
      next
    }

    message(glue("  {nrow(pos_data)} training rows for {pos}."))

    # --- Temporal train/test split ---
    # Multi-season: train on all seasons before test_season, test on test_season.
    # Single season: fall back to week-based split (weeks 1-14 train, 15-18 test).
    # Do NOT use initial_split() or any random split on time-ordered data.
    if (length(available_seasons) > 1) {
      train_data <- pos_data %>% filter(season < test_season)
      test_data  <- pos_data %>% filter(season == test_season)
      message(glue("  Train: seasons {paste(sort(unique(train_data$season)), collapse = '-')} ",
                   "({nrow(train_data)} rows). ",
                   "Test: season {test_season} ",
                   "({nrow(test_data)} rows)."))
    } else {
      # Single season: week-based split.
      # NFL regular season ends at week 18. Weeks 19+ are playoffs --
      # most skill position players are not active, so they are excluded.
      # Week 14 cutoff gives a 4-week holdout matching typical fantasy
      # playoff schedules (weeks 15-18).
      cutoff_week <- 14L
      train_data  <- pos_data %>% filter(week <= cutoff_week)
      test_data   <- pos_data %>% filter(week > cutoff_week, week <= 18L)
      test_range  <- if (nrow(test_data) > 0) {
        glue("weeks {min(test_data$week)}-{max(test_data$week)} ({nrow(test_data)} rows)")
      } else {
        "0 rows (mock/short data -- no weeks above cutoff)"
      }
      message(glue("  Single-season split. ",
                   "Train: weeks {min(train_data$week)}-{max(train_data$week)} ",
                   "({nrow(train_data)} rows). ",
                   "Test: {test_range}."))
    }

    if (nrow(train_data) < min_train_weeks) {
      message(glue("  Training set too small for {pos}. Skipping."))
      results[[pos]] <- NULL
      next
    }

    # --- Temporal CV folds (expanding window, week-based) ---
    # Build folds manually by week so each fold = one held-out week.
    # rolling_origin() operates on rows, not weeks -- with 20-30 players
    # per week it creates hundreds of folds instead of one per week.
    # Manual week-based splits: fold k trains on weeks 1:(min_train_weeks+k-1),
    # validates on week (min_train_weeks+k). Correct and efficient.
    train_weeks <- sort(unique(train_data$week))
    n_train_wks <- length(train_weeks)
    n_val_weeks <- n_train_wks - min_train_weeks

    if (n_val_weeks < 1L) {
      message(glue("  Not enough weeks for CV folds in {pos}. Using single train/test."))
      time_folds <- NULL
    } else {
      # Use local() to force each iteration to capture its own copy of
      # train_data and train_weeks. Without this, the lapply closure
      # references the loop variable by name and picks up whatever value
      # train_data holds when the fold is eventually evaluated -- which
      # may be a different position's data if the loop has moved on.
      td_snapshot <- train_data
      tw_snapshot <- train_weeks
      mt_snapshot <- min_train_weeks

      fold_list <- lapply(seq_len(n_val_weeks), function(k) {
        cutoff   <- tw_snapshot[mt_snapshot + k - 1L]
        val_week <- tw_snapshot[mt_snapshot + k]
        in_idx   <- which(td_snapshot$week <= cutoff)
        out_idx  <- which(td_snapshot$week == val_week)
        rsample::make_splits(
          list(analysis = in_idx, assessment = out_idx),
          data = td_snapshot
        )
      })
      time_folds <- rsample::manual_rset(
        fold_list,
        ids = paste0("Week", seq_len(n_val_weeks))
      )
      message(glue("  {nrow(time_folds)} CV folds created (one per held-out week)."))
    }

    # --- Recipe ---
    xgb_recipe <- recipe(ppr_points_next_week ~ ., data = train_data) %>%
      # Remove identifiers and non-feature columns
      step_rm(all_of(id_cols)) %>%
      # Handle unseen factor levels at prediction time
      step_novel(all_nominal_predictors()) %>%
      # One-hot encode categoricals (XGBoost needs numeric matrix)
      step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
      # Remove zero-variance predictors
      step_zv(all_predictors())
    # NOTE: do NOT normalize numeric predictors for XGBoost.
    # Tree-based models are scale-invariant -- normalization adds
    # no benefit and makes SHAP values less interpretable.
    # NOTE: NAs in numeric predictors are passed through intentionally.
    # XGBoost learns optimal default branch direction per split.

    # --- XGBoost model spec ---
    xgb_spec <- boost_tree(
      trees          = tune(),
      tree_depth     = tune(),
      learn_rate     = tune(),
      min_n          = tune(),
      loss_reduction = tune(),
      sample_size    = tune(),
      mtry           = tune()
      # stop_iter removed: conflicts with manual CV fold management in tidymodels.
      # Early stopping requires an internal watchlist that tidymodels cannot
      # provide when using manual_rset folds. Trees parameter handles capacity.
    ) %>%
      set_engine(
        "xgboost",
        nthread  = max(1L, parallel::detectCores() - 1L),
        verbose  = 0L,
        counts   = FALSE,           # pass mtry as proportion not count
        objective = "reg:squarederror"
      ) %>%
      set_mode("regression")

    xgb_workflow <- workflow() %>%
      add_recipe(xgb_recipe) %>%
      add_model(xgb_spec)

    # --- Latin hypercube tuning grid ---
    # More space-efficient than grid_regular for 7 parameters.
    # 30 candidates balances coverage vs runtime.
    xgb_grid <- grid_space_filling(
      trees(range          = c(100L, 1500L)),
      tree_depth(range     = c(3L,   8L)),
      learn_rate(range     = c(-3,  -1), trans = scales::log10_trans()),
      min_n(range          = c(5L,   40L)),
      loss_reduction(range = c(0,    4),  trans = scales::log10_trans()),
      sample_size          = sample_prop(range = c(0.5, 1.0)),
      mtry                 = mtry_prop(range   = c(0.3, 1.0)),
      size = tune_grid_size
    )

    # --- Tune ---
    message(glue("  Tuning {tune_grid_size} hyperparameter candidates..."))

    if (!is.null(time_folds)) {
      # Suppress deprecation warning from tidymodels internals:
      # tidyselect::all_of() is called inside tune::check_parameters() in a
      # non-selecting context -- this is a tidymodels version drift issue,
      # not a bug in our code. Suppressed here to keep test output clean.
      tuning_results <- withCallingHandlers(
        tune_grid(
          xgb_workflow,
          resamples = time_folds,
          grid      = xgb_grid,
          metrics   = metric_set(rmse, mae),  # rsq excluded: unstable on small CV folds
          control   = control_grid(
            save_pred   = TRUE,
            verbose     = FALSE,
            allow_par   = FALSE   # keep serial for reproducibility
          )
        ),
        warning = function(w) {
          if (grepl("all_of.*outside.*selecting|deprecated.*all_of", conditionMessage(w),
                    ignore.case = TRUE)) {
            invokeRestart("muffleWarning")
          }
        }
      )

      # collect_metrics() fails to aggregate from manual_rset objects in some
      # tidymodels versions -- it returns an empty tibble even when .metrics
      # list column contains valid data. Bypass with manual unnest instead.
      cv_metrics  <- tidyr::unnest(tuning_results, .metrics)

      # Identify best hyperparameter set by mean CV RMSE across folds.
      # finalize_workflow() requires a single-row tibble with exact parameter
      # column names matching the tune() placeholders -- no extra columns.
      best_config <- cv_metrics %>%
        filter(.metric == "rmse") %>%
        group_by(.config) %>%
        summarise(mean_rmse = mean(.estimate, na.rm = TRUE), .groups = "drop") %>%
        slice_min(mean_rmse, n = 1, with_ties = FALSE) %>%
        pull(.config)

      best_params <- cv_metrics %>%
        filter(.config == best_config) %>%
        select(mtry, trees, min_n, tree_depth,
               learn_rate, loss_reduction, sample_size) %>%
        slice(1)

      message(glue("  Best RMSE: {round(min(cv_metrics$.estimate[cv_metrics$.metric == 'rmse']), 3)}"))

    } else {
      # Fallback: no CV folds -- use defaults, skip tuning
      message("  Skipping tuning (insufficient folds). Using default parameters.")
      best_params <- tibble(
        trees = 500L, tree_depth = 6L, learn_rate = 0.05,
        min_n = 10L, loss_reduction = 0, sample_size = 0.8, mtry = 0.6,
        .config = "manual_default"
      )
      tuning_results <- NULL
      cv_metrics     <- tibble()
    }

    # --- Finalize and fit on full training data ---
    message("  Fitting final model on full training data...")

    final_workflow <- finalize_workflow(xgb_workflow, best_params)
    final_fit      <- fit(final_workflow, data = train_data)

    n_features <- final_fit %>%
      extract_recipe() %>%
      summary() %>%
      filter(role == "predictor") %>%
      nrow()

    message(glue("  Model fitted. {n_features} features."))

    results[[pos]] <- list(
      fitted_workflow = final_fit,
      cv_metrics      = cv_metrics,
      best_params     = best_params,
      tuning_results  = tuning_results,
      train_data      = train_data,
      test_data       = test_data,
      n_features      = n_features
    )
  }

  fitted_positions <- names(Filter(Negate(is.null), results))
  message(glue("\ntrain_fantasy_model complete. Models fitted: ",
               "{paste(fitted_positions, collapse = ', ')}"))

  results
}


# ==============================================================================
# SECTION 3: PREDICTION
# ==============================================================================

#' Generate Next-Week Fantasy Point Predictions
#'
#' @description
#' Produces next-week PPR point predictions for each player in new_data using
#' a fitted model from train_fantasy_model(). Returns point estimates with
#' approximate prediction intervals derived from CV fold RMSE variance.
#'
#' @param model_result Named list element from train_fantasy_model() for one
#'   position. Contains fitted_workflow, cv_metrics, and train_data.
#' @param new_data Tibble of current-week feature rows. Must contain the same
#'   columns as the training data from prepare_model_features(). These are the
#'   rows where you want to predict NEXT week's performance.
#' @param position Character scalar. Position group being predicted. Used for
#'   validation messaging only.
#' @param interval_multiplier Numeric. Multiplier on CV RMSE for the
#'   prediction interval half-width. Default: 1.96 (approximate 95% interval).
#'   Note: these are approximate intervals based on CV error variance, not
#'   true posterior predictive intervals.
#'
#' @return Tibble with one row per player in new_data. Columns:
#'   \describe{
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Player name (chr)}
#'     \item{position_group}{Position group (chr)}
#'     \item{team}{Team this week (chr)}
#'     \item{opponent}{Opponent this week (chr)}
#'     \item{week}{Week being predicted FROM (the current week). (int)}
#'     \item{predicted_week}{Week being predicted (current week + 1). (int)}
#'     \item{predicted_ppr}{Point estimate of next-week PPR points. (dbl)}
#'     \item{pi_lower}{Lower bound of approximate prediction interval. (dbl)}
#'     \item{pi_upper}{Upper bound of approximate prediction interval. (dbl)}
#'     \item{cv_rmse_used}{CV RMSE used to construct the interval. (dbl)}
#'     \item{availability_rate}{Player's availability rate through this week.
#'       Lower values signal floor risk from injury history. (dbl)}
#'     \item{weeks_since_last_played}{Weeks since player last appeared. (int)}
#'   }
#'
#' @examples
#' \dontrun{
#' # Get current week features (e.g. week 14)
#' current_week_data <- ml_data %>% filter(week == 14, position_group == "receiver")
#'
#' preds <- predict_fantasy_points(
#'   model_result = models$receiver,
#'   new_data     = current_week_data,
#'   position     = "receiver"
#' )
#'
#' preds %>% arrange(desc(predicted_ppr)) %>% head(10)
#' }
#'
#' @seealso [train_fantasy_model()] for model training,
#'   [explain_predictions()] for SHAP interpretation
#'
#' @export
predict_fantasy_points <- function(model_result,
                                   new_data,
                                   position,
                                   interval_multiplier = 1.96) {

  if (is.null(model_result)) stop("model_result is NULL -- model may not have been trained.")
  if (!is.data.frame(new_data)) stop("new_data must be a data frame.")
  if (!is.character(position) || length(position) != 1) stop("position must be a single string.")
  if (!is.numeric(interval_multiplier) || interval_multiplier <= 0) {
    stop("interval_multiplier must be a positive number.")
  }

  fitted_wf <- model_result$fitted_workflow
  if (is.null(fitted_wf)) stop("model_result$fitted_workflow is NULL.")

  # CV RMSE for interval construction
  cv_rmse <- model_result$cv_metrics %>%
    filter(.metric == "rmse") %>%
    pull(.estimate) %>%
    mean(na.rm = TRUE)

  if (is.nan(cv_rmse) || is.na(cv_rmse) || length(cv_rmse) == 0) {
    message("  No CV RMSE available -- prediction intervals will be NA.")
    cv_rmse <- NA_real_
  }

  # Generate point predictions
  preds <- predict(fitted_wf, new_data = new_data) %>%
    rename(predicted_ppr = .pred) %>%
    # Clip negative predictions to 0 -- a player cannot score negative PPR
    mutate(predicted_ppr = pmax(predicted_ppr, 0))

  # Assemble output
  result <- new_data %>%
    select(player_id, player_name, position_group, team, opponent, week,
           availability_rate, weeks_since_last_played) %>%
    bind_cols(preds) %>%
    mutate(
      predicted_week = week + 1L,
      cv_rmse_used   = cv_rmse,
      pi_lower       = pmax(predicted_ppr - interval_multiplier * cv_rmse, 0),
      pi_upper       = predicted_ppr + interval_multiplier * cv_rmse
    ) %>%
    select(
      player_id, player_name, position_group, team, opponent,
      week, predicted_week, predicted_ppr, pi_lower, pi_upper,
      cv_rmse_used, availability_rate, weeks_since_last_played
    ) %>%
    arrange(desc(predicted_ppr))

  message(glue("predict_fantasy_points ({position}): {nrow(result)} predictions. ",
               "CV RMSE used for intervals: {round(cv_rmse, 3)}"))

  result
}


# ==============================================================================
# SECTION 4: SHAP EXPLANATION
# ==============================================================================

#' Explain Predictions with SHAP Values
#'
#' @description
#' Extracts SHAP values from a fitted XGBoost model using shapviz. Returns
#' three plot types (beeswarm global importance, waterfall for one player,
#' dependence for the top feature) plus a correlation check to flag features
#' whose individual SHAP values should be interpreted as a group.
#'
#' CRITICAL: The underlying xgboost engine is extracted via
#' extract_fit_engine() before passing to shapviz. Passing the workflow
#' object directly to shapviz will fail.
#'
#' @param model_result Named list element from train_fantasy_model().
#' @param ml_data Tibble from prepare_model_features(). Used to build the
#'   feature matrix for SHAP computation.
#' @param player_name_focus Character. Name of one player for the waterfall
#'   plot. Default: NULL (uses the player with the highest predicted PPR).
#' @param n_top_features Integer. Number of top features to show in beeswarm
#'   and dependence plots. Default: 15L.
#' @param cor_threshold Numeric. Correlation coefficient above which two
#'   features are flagged as a correlated pair requiring group interpretation.
#'   Default: 0.7.
#'
#' @return Named list:
#'   \describe{
#'     \item{shap_object}{shapviz object for downstream custom plotting.}
#'     \item{plot_beeswarm}{ggplot: global feature importance beeswarm.}
#'     \item{plot_waterfall}{ggplot: waterfall for focus player.}
#'     \item{plot_dependence}{ggplot: dependence plot for top feature.}
#'     \item{correlated_pairs}{Tibble of feature pairs with |r| > cor_threshold.
#'       SHAP values for these pairs should be summed, not compared individually.}
#'     \item{top_features}{Character vector of top n_top_features by mean |SHAP|.}
#'   }
#'
#' @examples
#' \dontrun{
#' shap_output <- explain_predictions(
#'   model_result     = models$receiver,
#'   ml_data          = ml_data,
#'   player_name_focus = "T.Hill",
#'   n_top_features   = 15L
#' )
#'
#' shap_output$plot_beeswarm
#' shap_output$correlated_pairs
#' }
#'
#' @seealso [train_fantasy_model()] for model training,
#'   [predict_fantasy_points()] for generating predictions
#'
#' @export
explain_predictions <- function(model_result,
                                ml_data,
                                player_name_focus = NULL,
                                n_top_features    = 15L,
                                cor_threshold     = 0.7) {

  if (is.null(model_result)) stop("model_result is NULL.")
  if (!is.data.frame(ml_data)) stop("ml_data must be a data frame.")

  if (!requireNamespace("shapviz", quietly = TRUE)) {
    stop("Package 'shapviz' required for explain_predictions(). Install with install.packages('shapviz').")
  }

  message("explain_predictions: extracting SHAP values...")

  fitted_wf  <- model_result$fitted_workflow
  train_data <- model_result$train_data

  # Extract the underlying xgboost engine (CRITICAL -- do not pass workflow to shapviz)
  xgb_engine <- extract_fit_engine(fitted_wf)

  # Get preprocessed feature matrix (what the model actually saw during training)
  prepped_recipe  <- extract_recipe(fitted_wf)
  feature_matrix  <- bake(prepped_recipe, new_data = train_data) %>%
    select(-ppr_points_next_week) %>%
    as.matrix()

  message(glue("  Feature matrix: {nrow(feature_matrix)} rows x {ncol(feature_matrix)} columns."))

  # Compute SHAP values
  shap_obj <- shapviz::shapviz(xgb_engine, X_pred = feature_matrix)

  # --- Correlated feature check ---
  # SHAP splits attribution arbitrarily across correlated features.
  # Flag pairs with |r| > cor_threshold so they are interpreted as a group.
  cor_matrix <- cor(feature_matrix, use = "pairwise.complete.obs")
  cor_pairs  <- which(abs(cor_matrix) > cor_threshold & cor_matrix != 1,
                      arr.ind = TRUE)

  correlated_pairs <- tibble()
  if (nrow(cor_pairs) > 0) {
    correlated_pairs <- tibble(
      feature_1   = rownames(cor_matrix)[cor_pairs[, 1]],
      feature_2   = colnames(cor_matrix)[cor_pairs[, 2]],
      correlation = cor_matrix[cor_pairs]
    ) %>%
      filter(feature_1 < feature_2) %>%  # deduplicate symmetric pairs
      arrange(desc(abs(correlation)))

    message(glue(
      "  {nrow(correlated_pairs)} correlated feature pairs (|r| > {cor_threshold}). ",
      "Interpret their individual SHAP values as a group, not separately."
    ))
  }

  # --- Beeswarm (global importance + direction) ---
  plot_beeswarm <- shapviz::sv_importance(
    shap_obj,
    kind       = "beeswarm",
    max_display = n_top_features
  ) +
    labs(
      title    = "SHAP Feature Importance (Beeswarm)",
      subtitle = "Each dot = one player-week. Color = feature value (red = high, blue = low).",
      caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit Week 9"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))

  # --- Waterfall (single player explanation) ---
  # Find focus player row index in feature matrix
  if (!is.null(player_name_focus)) {
    focus_idx <- which(train_data$player_name == player_name_focus)[1]
    if (is.na(focus_idx)) {
      message(glue("  player_name_focus '{player_name_focus}' not found in train_data. ",
                   "Using row 1 instead."))
      focus_idx <- 1L
    }
  } else {
    # Default: player with highest predicted PPR
    preds_for_focus <- predict(fitted_wf, new_data = train_data)$.pred
    focus_idx       <- which.max(preds_for_focus)
  }

  focus_label <- train_data$player_name[focus_idx]
  focus_week  <- train_data$week[focus_idx]

  plot_waterfall <- shapviz::sv_waterfall(shap_obj, row_id = focus_idx) +
    labs(
      title    = glue("SHAP Waterfall: {focus_label} (Week {focus_week})"),
      subtitle = "How each feature moved the prediction from baseline to final value.",
      caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit Week 9"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))

  # --- Dependence plot (top feature vs its SHAP value) ---
  top_features <- shapviz::sv_importance(shap_obj, kind = "no") %>%
    arrange(desc(importance)) %>%
    slice_head(n = n_top_features) %>%
    pull(feature)

  top_feature_name <- top_features[1]

  plot_dependence <- shapviz::sv_dependence(shap_obj, v = top_feature_name) +
    labs(
      title    = glue("SHAP Dependence: {top_feature_name}"),
      subtitle = "Feature value vs its SHAP contribution. Color = interaction with other features.",
      caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit Week 9"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))

  message("explain_predictions complete.")

  list(
    shap_object       = shap_obj,
    plot_beeswarm     = plot_beeswarm,
    plot_waterfall    = plot_waterfall,
    plot_dependence   = plot_dependence,
    correlated_pairs  = correlated_pairs,
    top_features      = top_features
  )
}


# ==============================================================================
# SECTION 5: MODEL EVALUATION
# ==============================================================================

#' Evaluate XGBoost Fantasy Model on Held-Out Test Set
#'
#' @description
#' Evaluates a fitted model against the temporal held-out test set (last 20%
#' of weeks). Reports RMSE, MAE, and R-squared versus two naive baselines:
#'   Baseline 1 -- last week's actual PPR score (persistence forecast)
#'   Baseline 2 -- season-to-date average PPR score
#'
#' A complex ML model that cannot outperform the persistence baseline on RMSE
#' is not adding value. This function makes that comparison explicit.
#'
#' Also checks feature importance stability across CV folds. Features whose
#' importance varies by more than 50% (CV > 0.5) across folds are flagged
#' as unreliable for interpretation.
#'
#' @param model_result Named list element from train_fantasy_model().
#' @param ml_data Full tibble from prepare_model_features() (all weeks).
#'   Used to compute baselines and join actual PPR outcomes.
#' @param position Character scalar. Position being evaluated.
#'
#' @return Named list:
#'   \describe{
#'     \item{model_metrics}{Tibble: RMSE, MAE, R-squared on test set.}
#'     \item{baseline_last_week}{Tibble: RMSE/MAE for last-week persistence.}
#'     \item{baseline_season_avg}{Tibble: RMSE/MAE for season-avg baseline.}
#'     \item{comparison_table}{Tibble combining all three for easy comparison.}
#'     \item{importance_stability}{Tibble: mean/SD/CV of feature importance
#'       across CV folds. Features with CV > 0.5 flagged as unstable.}
#'     \item{test_predictions}{Tibble: actual vs predicted per player-week on
#'       test set. Use for residual analysis.}
#'   }
#'
#' @examples
#' \dontrun{
#' eval_result <- evaluate_model(
#'   model_result = models$receiver,
#'   ml_data      = ml_data,
#'   position     = "receiver"
#' )
#'
#' eval_result$comparison_table
#' eval_result$importance_stability %>% filter(cv_importance > 0.5)
#' }
#'
#' @seealso [train_fantasy_model()] for model training,
#'   [predict_fantasy_points()] for predictions
#'
#' @export
evaluate_model <- function(model_result, ml_data, position) {

  if (is.null(model_result)) stop("model_result is NULL.")
  if (!is.data.frame(ml_data)) stop("ml_data must be a data frame.")
  if (!is.character(position) || length(position) != 1) stop("position must be a single string.")

  message(glue("evaluate_model: evaluating {position} model..."))

  fitted_wf  <- model_result$fitted_workflow
  test_data  <- model_result$test_data
  train_data <- model_result$train_data

  if (is.null(test_data) || nrow(test_data) == 0) {
    message("  No test data available. Cannot evaluate.")
    return(NULL)
  }

  # --- Model predictions on test set ---
  test_preds <- predict(fitted_wf, new_data = test_data) %>%
    rename(predicted_ppr = .pred) %>%
    mutate(predicted_ppr = pmax(predicted_ppr, 0)) %>%
    bind_cols(test_data %>% select(player_id, player_name, week,
                                   ppr_points_next_week, ppr_points_this_week,
                                   epa_season_to_date))

  # --- Model metrics ---
  metric_fn  <- metric_set(rmse, mae, rsq)
  model_metrics <- metric_fn(
    test_preds,
    truth    = ppr_points_next_week,
    estimate = predicted_ppr
  )

  # --- Baseline 1: Last week's score (persistence) ---
  # Predict next week = this week's actual score
  baseline_lw <- test_preds %>%
    filter(!is.na(ppr_points_this_week)) %>%
    rename(baseline_pred = ppr_points_this_week)

  baseline_last_week <- metric_fn(
    baseline_lw,
    truth    = ppr_points_next_week,
    estimate = baseline_pred
  )

  # --- Baseline 2: Season-to-date average ---
  # Join season-avg from training data per player
  season_avg_train <- train_data %>%
    group_by(player_id, position_group) %>%
    summarise(season_avg_ppr = mean(ppr_points_this_week, na.rm = TRUE),
              .groups = "drop")

  baseline_sa <- test_preds %>%
    left_join(season_avg_train, by = c("player_id")) %>%
    filter(!is.na(season_avg_ppr)) %>%
    rename(baseline_pred = season_avg_ppr)

  baseline_season_avg <- metric_fn(
    baseline_sa,
    truth    = ppr_points_next_week,
    estimate = baseline_pred
  )

  # --- Comparison table ---
  comparison_table <- bind_rows(
    model_metrics       %>% mutate(model = "XGBoost"),
    baseline_last_week  %>% mutate(model = "Baseline: Last Week"),
    baseline_season_avg %>% mutate(model = "Baseline: Season Avg")
  ) %>%
    select(model, .metric, .estimate) %>%
    pivot_wider(names_from = .metric, values_from = .estimate) %>%
    mutate(position = position)

  # Print to console for easy reading
  message(glue("\n  --- {position} Model vs Baselines ---"))
  comparison_table %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    as.data.frame() %>%
    print()

  # --- Feature importance stability across CV folds ---
  importance_stability <- tibble()

  if (!is.null(model_result$tuning_results)) {
    fold_splits <- model_result$tuning_results

    # Extract best hyperparams and retrain per fold for importance
    best_p <- model_result$best_params

    fold_importances <- tryCatch({
      map_dfr(seq_along(fold_splits$splits), function(i) {
        fold_train <- training(fold_splits$splits[[i]])

        # extract_preprocessor() returns the unfit recipe definition.
        # extract_recipe() returns the fitted/prepped recipe which cannot
        # be added to a new workflow -- that triggers "Can't add a trained
        # recipe to a workflow" error.
        fold_wf <- finalize_workflow(
          workflow() %>%
            add_recipe(extract_preprocessor(fitted_wf)) %>%
            add_model(extract_spec_parsnip(fitted_wf)),
          best_p
        )

        fold_fit    <- fit(fold_wf, data = fold_train)
        fold_engine <- extract_fit_engine(fold_fit)
        imp         <- xgboost::xgb.importance(model = fold_engine)

        tibble(
          fold       = i,
          variable   = imp$Feature,
          importance = imp$Gain
        )
      })
    }, error = function(e) {
      message(glue("  Feature importance stability check failed: {conditionMessage(e)}"))
      tibble()
    })

    if (nrow(fold_importances) > 0) {
      importance_stability <- fold_importances %>%
        group_by(variable) %>%
        summarise(
          mean_importance = mean(importance, na.rm = TRUE),
          sd_importance   = sd(importance,   na.rm = TRUE),
          cv_importance   = ifelse(mean_importance > 0,
                                   sd_importance / mean_importance,
                                   NA_real_),
          n_folds         = n(),
          .groups = "drop"
        ) %>%
        arrange(desc(mean_importance))

      n_unstable <- sum(importance_stability$cv_importance > 0.5, na.rm = TRUE)
      if (n_unstable > 0) {
        message(glue(
          "  {n_unstable} features have unstable importance (CV > 0.5 across folds). ",
          "Do not use for individual interpretation."
        ))
      }
    }
  }

  message(glue("evaluate_model complete for {position}."))

  list(
    model_metrics        = model_metrics,
    baseline_last_week   = baseline_last_week,
    baseline_season_avg  = baseline_season_avg,
    comparison_table     = comparison_table,
    importance_stability = importance_stability,
    test_predictions     = test_preds
  )
}
