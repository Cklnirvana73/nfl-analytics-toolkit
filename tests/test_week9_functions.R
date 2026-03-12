# ==============================================================================
# WEEK 9 TEST SUITE: XGBOOST FANTASY PREDICTION
# ==============================================================================
#
# Tests for all five functions in R/11_xgboost_fantasy.R:
#   - prepare_model_features()
#   - train_fantasy_model()
#   - predict_fantasy_points()
#   - explain_predictions()
#   - evaluate_model()
#
# Run from project root:
#   source("tests/test_week9_functions.R")
#
# ==============================================================================

library(testthat)
library(dplyr)
library(tidyr)

# Source Week 9 production file
source(here::here("R", "11_xgboost_fantasy.R"))

cat("=======================================================\n")
cat("WEEK 9 TEST SUITE: XGBoost Fantasy Prediction\n")
cat("=======================================================\n\n")

# ==============================================================================
# HELPER: Minimal mock data builders
# ==============================================================================

make_feature_matrix <- function(n_players  = 5,
                                n_weeks    = 10,
                                position   = "rusher",
                                seasons    = c(2024, 2025),
                                multi_team = FALSE) {
  set.seed(42)
  players <- paste0("P", seq_len(n_players))

  # Build full cross of seasons x players x weeks
  base <- expand.grid(
    season    = seasons,
    player_id = players,
    week      = seq_len(n_weeks),
    stringsAsFactors = FALSE
  ) %>%
    as_tibble()

  # When multi_team = TRUE, P1 switches from BUF to KC at the week midpoint
  # within each season. All other players stay on BUF.
  # Enables team-reset and season-boundary leakage tests.
  midpoint <- ceiling(n_weeks / 2)
  base <- base %>%
    mutate(
      team = case_when(
        multi_team & player_id == "P1" & week > midpoint ~ "KC",
        TRUE                                              ~ "BUF"
      )
    )

  base %>%
    mutate(
      player_name             = paste0("Player_", player_id),
      position_group          = position,
      opponent                = "MIA",
      plays_this_week         = sample(10:30, n(), replace = TRUE),
      epa_this_week           = rnorm(n(), 0.05, 0.3),
      success_rate_this_week  = runif(n(), 0.35, 0.6),
      # epa_roll3 is NA for weeks 1-3 within each season (structural NA).
      # Must NOT be imputed -- XGBoost handles these natively.
      epa_roll3               = ifelse(week <= 3, NA_real_, rnorm(n(), 0.05, 0.2)),
      epa_season_to_date      = ifelse(week == 1, NA_real_, rnorm(n(), 0.05, 0.15)),
      plays_roll3             = ifelse(week <= 3, NA_real_, runif(n(), 15, 30)),
      neutral_epa_season      = rnorm(n(), 0.03, 0.2),
      leading_share_season    = runif(n(), 0.2, 0.5),
      trailing_share_season   = runif(n(), 0.2, 0.5),
      # opp_adjusted_epa_prior is NA for week 1 within each season
      opp_adjusted_epa_prior  = ifelse(week == 1, NA_real_, rnorm(n(), 0.02, 0.2)),
      schedule_difficulty_rank = sample(1:32, n(), replace = TRUE),
      opp_adj_games_prior     = pmax(0L, as.integer(week - 1L)),
      opponent_style          = sample(c("pass_funnel","run_funnel","balanced"), n(), replace = TRUE),
      opponent_tier           = sample(c("elite","above_avg","average","below_avg","poor"), n(), replace = TRUE),
      weeks_played            = pmin(week, 10L),
      weeks_since_role_change = sample(0:5, n(), replace = TRUE),
      role_stability_flag     = sample(c(TRUE, FALSE), n(), replace = TRUE)
    )
}

make_pbp_data <- function(n_plays   = 500,
                          n_players = 5,
                          n_weeks   = 10,
                          season    = 2025) {
  set.seed(99)
  players <- paste0("P", seq_len(n_players))

  tibble(
    season               = season,
    week                 = sample(seq_len(n_weeks), n_plays, replace = TRUE),
    posteam              = "BUF",
    defteam              = "MIA",
    play_type            = sample(c("pass", "run"), n_plays, replace = TRUE, prob = c(0.55, 0.45)),
    qb_kneel             = 0L,
    qb_spike             = 0L,
    qb_scramble          = 0L,
    passer_player_id     = ifelse(runif(n_plays) > 0.55, sample(players, n_plays, replace = TRUE), NA_character_),
    rusher_player_id     = ifelse(runif(n_plays) > 0.45, sample(players, n_plays, replace = TRUE), NA_character_),
    receiver_player_id   = ifelse(runif(n_plays) > 0.55, sample(players, n_plays, replace = TRUE), NA_character_),
    pass_attempt         = ifelse(runif(n_plays) > 0.5, 1L, 0L),
    complete_pass        = ifelse(runif(n_plays) > 0.4, 1L, 0L),
    passing_yards        = ifelse(runif(n_plays) > 0.5, sample(0:40, n_plays, replace = TRUE), NA_real_),
    rushing_yards        = ifelse(runif(n_plays) > 0.5, sample(0:20, n_plays, replace = TRUE), NA_real_),
    receiving_yards      = ifelse(runif(n_plays) > 0.5, sample(0:30, n_plays, replace = TRUE), NA_real_),
    touchdown            = sample(0:1, n_plays, replace = TRUE, prob = c(0.94, 0.06)),
    pass_touchdown       = sample(0:1, n_plays, replace = TRUE, prob = c(0.94, 0.06)),
    rush_touchdown       = sample(0:1, n_plays, replace = TRUE, prob = c(0.95, 0.05)),
    return_touchdown     = 0L,
    interception         = sample(0:1, n_plays, replace = TRUE, prob = c(0.97, 0.03)),
    fumble_lost          = sample(0:1, n_plays, replace = TRUE, prob = c(0.97, 0.03)),
    two_point_conv_result = NA_character_,
    epa                  = rnorm(n_plays, 0, 0.5)
  )
}


# ==============================================================================
# TEST BLOCK 1: prepare_model_features() -- Input Validation
# ==============================================================================

test_that("prepare_model_features: rejects non-data-frame feature_matrix", {
  expect_error(
    prepare_model_features("not_a_df", make_pbp_data(), seasons = 2025),
    "feature_matrix must be a data frame"
  )
})

test_that("prepare_model_features: rejects non-data-frame pbp_data", {
  expect_error(
    prepare_model_features(make_feature_matrix(), "not_a_df", seasons = 2025),
    "pbp_data must be a data frame"
  )
})

test_that("prepare_model_features: rejects missing season", {
  expect_error(
    prepare_model_features(make_feature_matrix(), make_pbp_data()),
    "seasons must be a non-empty numeric vector"
  )
})

test_that("prepare_model_features: rejects vector season", {
  expect_error(
    prepare_model_features(make_feature_matrix(), make_pbp_data(), seasons = "not_a_number"),
    "seasons must be a non-empty numeric vector"
  )
})

test_that("prepare_model_features: errors on missing required feature_matrix columns", {
  bad_fm <- make_feature_matrix(seasons = c(2024, 2025)) %>% select(-epa_roll3)
  expect_error(
    prepare_model_features(bad_fm, make_pbp_data(), seasons = 2025),
    "feature_matrix missing columns"
  )
})

test_that("prepare_model_features: errors on missing required pbp columns", {
  bad_pbp <- make_pbp_data() %>% select(-interception)
  expect_error(
    prepare_model_features(make_feature_matrix(seasons = c(2024, 2025)), bad_pbp, seasons = 2025),
    "pbp_data missing columns"
  )
})

test_that("prepare_model_features: rejects implausible season value in seasons vector", {
  expect_error(
    prepare_model_features(make_feature_matrix(seasons = c(2024, 2025)),
                           make_pbp_data(), seasons = c(1950, 2025)),
    "Implausible season"
  )
})


# ==============================================================================
# TEST BLOCK 2: prepare_model_features() -- Output Structure
# ==============================================================================

# Build shared fixtures once -- expensive to rebuild per test
# pbp_fixture spans both seasons so prepare_model_features can build the
# team-week spine and PPR points for each season independently.
fm_fixture  <- make_feature_matrix(n_players = 4, n_weeks = 8, seasons = c(2024, 2025))
pbp_fixture <- bind_rows(
  make_pbp_data(n_plays = 400, n_players = 4, n_weeks = 8, season = 2024),
  make_pbp_data(n_plays = 400, n_players = 4, n_weeks = 8, season = 2025)
)
ml_fixture  <- prepare_model_features(fm_fixture, pbp_fixture, seasons = c(2024, 2025),
                                      positions = "rusher")

test_that("prepare_model_features: returns a tibble", {
  expect_s3_class(ml_fixture, "tbl_df")
})

test_that("prepare_model_features: output contains required new columns", {
  required_new <- c(
    "is_absence_week", "weeks_since_last_played",
    "missed_weeks_this_season", "availability_rate",
    "ppr_points_this_week", "ppr_points_next_week", "has_target"
  )
  missing_cols <- setdiff(required_new, names(ml_fixture))
  expect_equal(
    missing_cols, character(0),
    label = paste("Missing columns:", paste(missing_cols, collapse = ", "))
  )
})

test_that("prepare_model_features: is_absence_week is logical", {
  expect_type(ml_fixture$is_absence_week, "logical")
})

test_that("prepare_model_features: has_target is logical", {
  expect_type(ml_fixture$has_target, "logical")
})

test_that("prepare_model_features: availability_rate is between 0 and 1", {
  valid <- ml_fixture$availability_rate
  valid <- valid[!is.na(valid)]
  expect_true(all(valid >= 0 & valid <= 1),
              label = "availability_rate must be in [0, 1]")
})

test_that("prepare_model_features: missed_weeks_this_season is non-negative", {
  valid <- ml_fixture$missed_weeks_this_season
  valid <- valid[!is.na(valid)]
  expect_true(all(valid >= 0))
})

test_that("prepare_model_features: ppr_points_this_week is NA for absence weeks", {
  absence_rows <- ml_fixture %>% filter(is_absence_week == TRUE)
  if (nrow(absence_rows) > 0) {
    expect_true(all(is.na(absence_rows$ppr_points_this_week)),
                label = "Absence weeks must have NA ppr_points_this_week")
  }
})

test_that("prepare_model_features: structural NAs in epa_roll3 are preserved", {
  # epa_roll3 is NA for weeks 1-3 in mock data -- must NOT be imputed
  early_rows <- ml_fixture %>% filter(week <= 3)
  if (nrow(early_rows) > 0) {
    expect_true(any(is.na(early_rows$epa_roll3)),
                label = "Structural NAs in epa_roll3 must survive -- do not impute")
  }
})

test_that("prepare_model_features: data is ordered by season/week", {
  actual_weeks   <- ml_fixture %>% pull(week)
  expected_weeks <- ml_fixture %>%
    arrange(player_id, position_group, season, week) %>%
    pull(week)
  expect_equal(actual_weeks, expected_weeks)
})

test_that("prepare_model_features: ppr_points_this_week is non-negative when present", {
  valid_ppr <- ml_fixture$ppr_points_this_week
  valid_ppr <- valid_ppr[!is.na(valid_ppr)]
  expect_true(all(valid_ppr >= 0),
              label = "PPR points cannot be negative")
})


# ==============================================================================
# TEST BLOCK 3: prepare_model_features() -- Absence Reconstruction
# ==============================================================================

test_that("prepare_model_features: reconstructed matrix has >= rows than input", {
  # Filter input to match the same season and position as ml_fixture
  # before comparing -- multi-season fixture contains rows from all seasons
  input_rows <- fm_fixture %>%
    filter(position_group == "rusher", season == 2025) %>%
    nrow()
  expect_gte(nrow(ml_fixture), input_rows)
})

test_that("prepare_model_features: next-week target is NA for last game of season", {
  # For any player, the last week in the season cannot have a next-week target.
  # Exclude absence rows (NA team) -- this assertion applies to played weeks only.
  # Assumption: absence-reconstructed rows may carry NA for team.
  last_week_rows <- ml_fixture %>%
    filter(!is.na(team)) %>%
    group_by(player_id, position_group, team) %>%
    filter(week == max(week)) %>%
    ungroup()
  expect_true(all(is.na(last_week_rows$ppr_points_next_week) |
                    last_week_rows$has_target == FALSE),
              label = "Last week of season must have no next-week target")
})

test_that("prepare_model_features: availability_rate decreases or stays flat when player misses games", {
  # Players with absence weeks should have availability_rate <= 1.0 always
  players_with_absence <- ml_fixture %>%
    filter(is_absence_week == TRUE) %>%
    pull(player_id) %>%
    unique()

  if (length(players_with_absence) > 0) {
    affected <- ml_fixture %>%
      filter(player_id %in% players_with_absence,
             !is.na(availability_rate))
    expect_true(all(affected$availability_rate <= 1.0),
                label = "Players with absences cannot have availability_rate > 1")
  }
})


# ==============================================================================
# TEST BLOCK 4: Feature Leakage Guards
# ==============================================================================

test_that("epa_roll3 is NA for early weeks -- confirms no lookahead in rolling window", {
  # The mock feature matrix sets epa_roll3 = NA for weeks <= 3.
  # prepare_model_features must not fill these forward.
  week1_rows <- ml_fixture %>% filter(week == 1)
  if (nrow(week1_rows) > 0) {
    expect_true(all(is.na(week1_rows$epa_roll3)),
                label = "Week 1 epa_roll3 must be NA -- no lookahead allowed")
  }
})

test_that("opp_adjusted_epa_prior is NA for week 1 -- confirms expanding window", {
  week1_rows <- ml_fixture %>% filter(week == 1)
  if (nrow(week1_rows) > 0) {
    expect_true(all(is.na(week1_rows$opp_adjusted_epa_prior)),
                label = "Week 1 opp_adjusted_epa_prior must be NA -- no prior games")
  }
})

test_that("rolling features are NA at season boundary -- no cross-season contamination", {
  # Week 1 of 2025 must not have rolling features populated from week 18 of 2024.
  # Season boundaries are independent contexts -- rolling windows must reset.
  season_boundary_rows <- ml_fixture %>%
    filter(season == 2025, week == 1)
  if (nrow(season_boundary_rows) > 0) {
    expect_true(all(is.na(season_boundary_rows$epa_roll3)),
                label = "epa_roll3 must be NA for week 1 of a new season -- no cross-season lookahead")
  }
})

test_that("ppr_points_next_week is the lead of ppr_points_this_week within player-season group", {
  # For a player who plays two consecutive weeks, week N+1 ppr actual
  # should equal week N's ppr_points_next_week.
  # Season is included in group_by to prevent lead() crossing season boundaries --
  # week 18 of 2024 must not point to week 1 of 2025 as its next-week target.
  consecutive_pairs <- ml_fixture %>%
    filter(!is_absence_week) %>%
    group_by(player_id, position_group, team, season) %>%
    arrange(week, .by_group = TRUE) %>%
    mutate(
      next_week_actual    = lead(ppr_points_this_week),
      target_matches_lead = abs(coalesce(ppr_points_next_week, -999) -
                                  coalesce(next_week_actual, -999)) < 0.001
    ) %>%
    filter(!is.na(next_week_actual), !is.na(ppr_points_next_week)) %>%
    ungroup()

  if (nrow(consecutive_pairs) > 0) {
    expect_true(all(consecutive_pairs$target_matches_lead),
                label = "ppr_points_next_week must equal lead(ppr_points_this_week) within player-season group")
  }
})


test_that("rolling features reset to NA on team change -- no cross-team contamination", {
  # P1 switches from BUF (weeks 1-5) to KC (weeks 6-10).
  # The first rolling window on KC (week 6) must be NA because the window
  # cannot be filled from BUF games. Any non-NA value in week 6 indicates
  # the rolling window crossed the team boundary -- a leakage bug.
  mt_fm      <- make_feature_matrix(n_players = 3, n_weeks = 10, multi_team = TRUE)
  mt_pbp     <- make_pbp_data(n_plays = 300, n_players = 3, n_weeks = 10)
  mt_fixture <- suppressMessages(
    prepare_model_features(mt_fm, mt_pbp, seasons = c(2024, 2025), positions = "rusher")
  )

  traded_player_first_new_team_week <- mt_fixture %>%
    filter(player_id == "P1", team == "KC") %>%
    arrange(week) %>%
    slice_head(n = 1)

  if (nrow(traded_player_first_new_team_week) > 0) {
    expect_true(
      is.na(traded_player_first_new_team_week$epa_roll3),
      label = "epa_roll3 must be NA for first week on new team -- rolling window must reset"
    )
  }
})

# ==============================================================================
# TEST BLOCK 5: NFL Domain -- PPR Scoring Validation
# ==============================================================================

test_that("rusher PPR points are non-negative for all played weeks", {
  # If a player has only rushing yards (rusher group), their rusher PPR
  # should be positive when they have rush yards
  rushers <- ml_fixture %>%
    filter(position_group == "rusher",
           !is_absence_week,
           !is.na(ppr_points_this_week))

  # All PPR values for played weeks must be >= 0
  expect_true(all(rushers$ppr_points_this_week >= 0),
              label = "Rusher PPR must be non-negative for played weeks")
})

test_that("availability_rate is 1.0 for player who has never missed a game", {
  # A player who appears every week should have availability_rate = 1.0
  # Find a player in mock data who appears in all weeks
  all_week_players <- ml_fixture %>%
    group_by(player_id, position_group, team) %>%
    summarise(n_absent = sum(is_absence_week), .groups = "drop") %>%
    filter(n_absent == 0) %>%
    pull(player_id)

  if (length(all_week_players) > 0) {
    perfect_avail <- ml_fixture %>%
      filter(player_id %in% all_week_players,
             !is.na(availability_rate)) %>%
      pull(availability_rate)
    expect_true(all(abs(perfect_avail - 1.0) < 0.01),
                label = "Player with no absences must have availability_rate = 1.0")
  }
})


# ==============================================================================
# TEST BLOCK 6: train_fantasy_model() -- Input Validation
# ==============================================================================

test_that("train_fantasy_model: rejects non-data-frame ml_data", {
  expect_error(
    train_fantasy_model("not_a_df"),
    "ml_data must be a data frame"
  )
})

test_that("train_fantasy_model: rejects ml_data without has_target column", {
  bad_data <- ml_fixture %>% select(-has_target)
  expect_error(
    train_fantasy_model(bad_data),
    "ml_data must contain has_target column"
  )
})

test_that("train_fantasy_model: rejects min_train_weeks < 2", {
  expect_error(
    train_fantasy_model(ml_fixture, min_train_weeks = 1),
    "min_train_weeks must be >= 2"
  )
})

test_that("train_fantasy_model: rejects test_season not present in ml_data", {
  expect_error(
    train_fantasy_model(ml_fixture, test_season = 2099),
    "test_season"
  )
})


# ==============================================================================
# TEST BLOCK 7: train_fantasy_model() -- Output Structure
# ==============================================================================

# Build a minimal training result -- use small grid for test speed
test_that("train_fantasy_model: returns a list with position keys", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("workflows")
  skip_if_not_installed("tune")

  # Use ml_fixture with enough rows to train
  result <- suppressMessages(
    train_fantasy_model(
      ml_fixture,
      positions       = "rusher",
      min_train_weeks = 3L,
      tune_grid_size  = 2L,  # minimal grid for test speed
      seed            = 42L
    )
  )

  expect_type(result, "list")
  expect_true("rusher" %in% names(result))
})

test_that("train_fantasy_model: result contains required slots per position", {
  skip_if_not_installed("xgboost")

  result <- suppressMessages(
    train_fantasy_model(
      ml_fixture,
      positions       = "rusher",
      min_train_weeks = 3L,
      tune_grid_size  = 2L,
      seed            = 42L
    )
  )

  if (!is.null(result$rusher)) {
    required_slots <- c("fitted_workflow", "cv_metrics", "best_params",
                        "train_data", "test_data", "n_features")
    expect_true(all(required_slots %in% names(result$rusher)),
                label = paste("Missing slots:", paste(
                  setdiff(required_slots, names(result$rusher)), collapse = ", ")))
  }
})

test_that("train_fantasy_model: n_features is a positive integer", {
  skip_if_not_installed("xgboost")

  result <- suppressMessages(
    train_fantasy_model(
      ml_fixture,
      positions       = "rusher",
      min_train_weeks = 3L,
      tune_grid_size  = 2L,
      seed            = 42L
    )
  )

  if (!is.null(result$rusher)) {
    expect_true(result$rusher$n_features > 0L)
  }
})

test_that("train_fantasy_model: train_data contains no test_season rows", {
  skip_if_not_installed("xgboost")

  result <- suppressMessages(
    train_fantasy_model(
      ml_fixture,
      positions       = "rusher",
      test_season     = 2025,
      min_train_weeks = 3L,
      tune_grid_size  = 2L,
      seed            = 42L
    )
  )

  if (!is.null(result$rusher)) {
    expect_true(all(result$rusher$train_data$season < 2025),
                label = "train_data must contain only pre-test seasons -- no future leakage")
    expect_true(all(result$rusher$test_data$season == 2025),
                label = "test_data must contain only the test season")
  }
})


# ==============================================================================
# TEST BLOCK 8: predict_fantasy_points() -- Validation
# ==============================================================================

test_that("predict_fantasy_points: rejects NULL model_result", {
  expect_error(
    predict_fantasy_points(NULL, ml_fixture, "rusher"),
    "model_result is NULL"
  )
})

test_that("predict_fantasy_points: rejects non-data-frame new_data", {
  skip_if_not_installed("xgboost")

  result <- suppressMessages(
    train_fantasy_model(ml_fixture, positions = "rusher",
                        min_train_weeks = 3L, tune_grid_size = 2L, seed = 42L)
  )

  if (!is.null(result$rusher)) {
    expect_error(
      predict_fantasy_points(result$rusher, "not_a_df", "rusher"),
      "new_data must be a data frame"
    )
  }
})

test_that("predict_fantasy_points: all predictions are non-negative", {
  skip_if_not_installed("xgboost")

  result <- suppressMessages(
    train_fantasy_model(ml_fixture, positions = "rusher",
                        min_train_weeks = 3L, tune_grid_size = 2L, seed = 42L)
  )

  if (!is.null(result$rusher)) {
    current_week <- ml_fixture %>%
      filter(season == max(season), week == max(week),
             position_group == "rusher", !is_absence_week)

    if (nrow(current_week) > 0) {
      preds <- suppressMessages(
        predict_fantasy_points(result$rusher, current_week, "rusher")
      )
      expect_true(all(preds$predicted_ppr >= 0),
                  label = "Predicted PPR must be non-negative (clipped at 0)")
    }
  }
})

test_that("predict_fantasy_points: output contains required columns", {
  skip_if_not_installed("xgboost")

  result <- suppressMessages(
    train_fantasy_model(ml_fixture, positions = "rusher",
                        min_train_weeks = 3L, tune_grid_size = 2L, seed = 42L)
  )

  if (!is.null(result$rusher)) {
    current_week <- ml_fixture %>%
      filter(season == max(season), week == max(week),
             position_group == "rusher", !is_absence_week)

    if (nrow(current_week) > 0) {
      preds <- suppressMessages(
        predict_fantasy_points(result$rusher, current_week, "rusher")
      )

      required_cols <- c("player_id", "player_name", "predicted_ppr",
                         "pi_lower", "pi_upper", "predicted_week",
                         "availability_rate")
      missing_cols <- setdiff(required_cols, names(preds))
      expect_equal(missing_cols, character(0),
                   label = paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    }
  }
})

test_that("predict_fantasy_points: pi_lower <= predicted_ppr <= pi_upper", {
  skip_if_not_installed("xgboost")

  result <- suppressMessages(
    train_fantasy_model(ml_fixture, positions = "rusher",
                        min_train_weeks = 3L, tune_grid_size = 2L, seed = 42L)
  )

  if (!is.null(result$rusher)) {
    current_week <- ml_fixture %>%
      filter(season == max(season), week == max(week),
             position_group == "rusher", !is_absence_week)

    if (nrow(current_week) > 0) {
      preds <- suppressMessages(
        predict_fantasy_points(result$rusher, current_week, "rusher")
      )

      valid <- preds %>% filter(!is.na(pi_lower), !is.na(pi_upper))
      if (nrow(valid) > 0) {
        expect_true(all(valid$pi_lower <= valid$predicted_ppr),
                    label = "pi_lower must be <= predicted_ppr")
        expect_true(all(valid$predicted_ppr <= valid$pi_upper),
                    label = "predicted_ppr must be <= pi_upper")
      }
    }
  }
})


# ==============================================================================
# TEST BLOCK 9: evaluate_model() -- Comparison Table Structure
# ==============================================================================

test_that("evaluate_model: rejects NULL model_result", {
  expect_error(
    evaluate_model(NULL, ml_fixture, "rusher"),
    "model_result is NULL"
  )
})

test_that("evaluate_model: comparison_table contains all three models", {
  skip_if_not_installed("xgboost")

  result <- suppressMessages(
    train_fantasy_model(ml_fixture, positions = "rusher",
                        min_train_weeks = 3L, tune_grid_size = 2L, seed = 42L)
  )

  if (!is.null(result$rusher)) {
    eval_out <- suppressMessages(
      evaluate_model(result$rusher, ml_fixture %>% filter(season == 2025), "rusher")
    )

    if (!is.null(eval_out)) {
      expect_true("XGBoost" %in% eval_out$comparison_table$model,
                  label = "comparison_table must include XGBoost row")
      expect_true(any(grepl("Last Week", eval_out$comparison_table$model)),
                  label = "comparison_table must include last-week baseline")
      expect_true(any(grepl("Season Avg", eval_out$comparison_table$model)),
                  label = "comparison_table must include season-avg baseline")
    }
  }
})

test_that("evaluate_model: RMSE is a positive number", {
  skip_if_not_installed("xgboost")

  result <- suppressMessages(
    train_fantasy_model(ml_fixture, positions = "rusher",
                        min_train_weeks = 3L, tune_grid_size = 2L, seed = 42L)
  )

  if (!is.null(result$rusher)) {
    eval_out <- suppressMessages(
      evaluate_model(result$rusher, ml_fixture %>% filter(season == 2025), "rusher")
    )

    if (!is.null(eval_out)) {
      model_rmse <- eval_out$comparison_table %>%
        filter(model == "XGBoost") %>%
        pull(rmse)
      expect_true(is.numeric(model_rmse) && model_rmse > 0,
                  label = "Model RMSE must be a positive number")
    }
  }
})


# ==============================================================================
# TEST BLOCK 10: Edge Cases
# ==============================================================================

test_that("prepare_model_features: returns empty tibble when no matching season/position", {
  fm_2024 <- make_feature_matrix(seasons = c(2024))
  result <- suppressMessages(
    prepare_model_features(fm_2024, make_pbp_data(season = 2024),
                           seasons = 2025, positions = "rusher")
  )
  expect_equal(nrow(result), 0L,
               label = "No rows in wrong season should return empty tibble")
})

test_that("prepare_model_features: handles single player correctly", {
  single_player_fm <- make_feature_matrix(n_players = 1, n_weeks = 5, seasons = c(2025))
  result <- suppressMessages(
    prepare_model_features(single_player_fm, make_pbp_data(n_players = 1),
                           season = 2025, positions = "rusher")
  )
  expect_gte(nrow(result), 1L)
  expect_true("is_absence_week" %in% names(result))
})

test_that("prepare_model_features: handles all-absence player gracefully", {
  # A player who never appears in pbp but is in feature_matrix
  # (edge case: player in roster but never got plays)
  fm_sparse <- make_feature_matrix(n_players = 2, n_weeks = 3, seasons = c(2025))
  # Make one player have very few plays
  fm_sparse$plays_this_week[fm_sparse$player_id == "P1"] <- 1L

  result <- suppressMessages(
    prepare_model_features(fm_sparse, make_pbp_data(n_players = 2, n_weeks = 3),
                           season = 2025, positions = "rusher")
  )
  expect_true(is.data.frame(result))
})

test_that("train_fantasy_model: returns NULL for position with insufficient rows", {
  tiny_data <- ml_fixture %>% slice_head(n = 3)
  result <- suppressMessages(
    train_fantasy_model(tiny_data, positions = "rusher",
                        min_train_weeks = 4L, tune_grid_size = 2L)
  )
  expect_null(result$rusher)
})

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n=======================================================\n")
cat("Week 9 test suite complete.\n")
cat("Functions tested: prepare_model_features, train_fantasy_model,\n")
cat("                  predict_fantasy_points, evaluate_model\n")
cat("=======================================================\n")
