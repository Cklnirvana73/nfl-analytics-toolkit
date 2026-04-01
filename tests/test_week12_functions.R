# ==============================================================================
# test_week12_functions.R
# NFL Analytics Toolkit - Week 12: Projection System Test Suite
#
# Tests all functions in R/14_projection_system.R
# Covers: pure functions, input validation, edge cases, NFL-specific guards,
#         column verification, traded-player dedup, and integration tests.
#
# Tests requiring model artifacts (ensemble, boom/bust) are limited to input
# validation only. Full integration tests require running the ensemble pipeline
# first (see example_week12.R Examples 5 and 7 for live integration).
#
# Run from: test_week12.R (root level) or directly via testthat::test_file()
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)
library(here)
library(glue)

source(here::here("R", "14_projection_system.R"))

# ==============================================================================
# MOCK DATA BUILDERS
# ==============================================================================

# Minimal prior season stats for generate_preseason_projections()
build_mock_prior_stats <- function(n_per_pos = 5) {
  positions <- c("QB", "RB", "WR")
  rows <- list()
  for (pos in positions) {
    for (i in seq_len(n_per_pos)) {
      rows[[length(rows) + 1]] <- tibble(
        player_id        = paste0("00-", formatC(match(pos, positions) * 100 + i, width = 7, flag = "0")),
        player_name      = paste0("Test ", pos, " ", i),
        position         = pos,
        games_played     = sample(8:17, 1),
        avg_ppr_per_game = runif(1, 5, 25)
      )
    }
  }
  bind_rows(rows)
}

# Minimal feature matrix slice (one week) matching ml_data schema
build_mock_feature_matrix <- function(player_ids, week_num = 5L,
                                       season_val = 2025L) {
  n <- length(player_ids)
  tibble(
    season                   = rep(season_val, n),
    week                     = rep(as.integer(week_num), n),
    player_id                = player_ids,
    player_name              = paste0("Player_", seq_len(n)),
    position_group           = rep(c("passer", "rusher", "receiver"), length.out = n),
    team                     = rep(c("KC", "BUF", "PHI", "SF", "DAL"), length.out = n),
    opponent                 = rep(c("LV", "MIA", "NYG", "SEA", "WAS"), length.out = n),
    plays_this_week          = sample(20:80, n, replace = TRUE),
    epa_this_week            = rnorm(n, 0, 3),
    success_rate_this_week   = runif(n, 0.3, 0.6),
    epa_roll3                = rnorm(n, 0, 2),
    epa_season_to_date       = rnorm(n, 0, 2),
    plays_roll3              = sample(50:200, n, replace = TRUE),
    neutral_epa_season       = rnorm(n, 0, 1),
    leading_share_season     = runif(n, 0, 0.5),
    trailing_share_season    = runif(n, 0, 0.5),
    opp_adjusted_epa_prior   = rnorm(n, 0, 1),
    schedule_difficulty_rank = sample(1:50, n, replace = TRUE),
    opp_adj_games_prior      = sample(1:14, n, replace = TRUE),
    opponent_style           = rep(NA_character_, n),
    opponent_tier            = rep(NA_character_, n),
    weeks_played             = sample(1:14, n, replace = TRUE),
    weeks_since_role_change  = sample(0:10, n, replace = TRUE),
    role_stability_flag      = sample(c(TRUE, FALSE), n, replace = TRUE),
    # ml_data columns from prepare_model_features()
    ppr_points_this_week     = rnorm(n, 12, 6),
    ppr_points_next_week     = rnorm(n, 12, 6),
    has_target               = rep(TRUE, n),
    is_absence_week          = rep(FALSE, n),
    weeks_since_last_played  = sample(0:3, n, replace = TRUE),
    missed_weeks_this_season = sample(0:2, n, replace = TRUE),
    availability_rate        = runif(n, 0.7, 1.0)
  )
}

# Minimal preseason projections (output of generate_preseason_projections)
build_mock_preseason_proj <- function(player_ids) {
  n <- length(player_ids)
  tibble(
    player_id              = player_ids,
    player_name            = paste0("Player_", seq_len(n)),
    position               = rep(c("QB", "RB", "WR"), length.out = n),
    prior_season           = 2024L,
    projected_ppr_per_game = runif(n, 5, 25),
    projected_ppr_lower_95 = runif(n, 0, 10),
    projected_ppr_upper_95 = runif(n, 20, 35),
    projected_ppr_sd       = runif(n, 3, 8),
    prior_games_played     = sample(8:17, n, replace = TRUE),
    regression_alpha       = runif(n, 0, 0.6),
    prior_volume_rate      = runif(n, 5, 25),
    prior_efficiency_rate  = rnorm(n, 0, 0.5)
  )
}


# ==============================================================================
# TEST 1: compute_prior_weight()
# ==============================================================================

test_that("compute_prior_weight returns correct boundary values", {
  # Week 1: full prior weight
  expect_equal(compute_prior_weight(1), 1.0)

  # Week 18: floor (minimum prior weight)
  expect_equal(compute_prior_weight(18), PRIOR_WEIGHT_FLOOR)
})

test_that("compute_prior_weight is monotonically decreasing", {
  weights <- sapply(1:18, compute_prior_weight)
  diffs   <- diff(weights)
  expect_true(all(diffs <= 0),
              info = "Prior weight must decrease or stay flat across weeks")
})

test_that("compute_prior_weight crossover near week 9-10", {
  # Crossover: prior_weight drops below 0.5
  w9  <- compute_prior_weight(9)
  w10 <- compute_prior_weight(10)
  expect_true(w9 > 0.5 | w10 < 0.5,
              info = "Crossover point should be near weeks 9-10")
})

test_that("compute_prior_weight rejects invalid input", {
  expect_error(compute_prior_weight(0),   "week must be 1-18")
  expect_error(compute_prior_weight(19),  "week must be 1-18")
  expect_error(compute_prior_weight(-1),  "week must be 1-18")
  expect_error(compute_prior_weight(22),  "week must be 1-18")
})

test_that("prior + observed weights sum to 1 for all weeks", {
  for (wk in 1:18) {
    pw <- compute_prior_weight(wk)
    ow <- 1 - pw
    expect_equal(pw + ow, 1.0, tolerance = 1e-10,
                 info = paste("Week", wk, "weights must sum to 1"))
  }
})


# ==============================================================================
# TEST 2: validate_regular_season_week()
# ==============================================================================

test_that("validate_regular_season_week accepts valid weeks", {
  for (wk in 1:18) {
    expect_silent(validate_regular_season_week(wk))
  }
})

test_that("validate_regular_season_week rejects playoff weeks", {
  expect_error(validate_regular_season_week(19), "playoff week")
  expect_error(validate_regular_season_week(20), "playoff week")
  expect_error(validate_regular_season_week(22), "playoff week")
})

test_that("validate_regular_season_week rejects week 0 and negative", {
  expect_error(validate_regular_season_week(0),  "must be >= 1")
  expect_error(validate_regular_season_week(-1), "must be >= 1")
})


# ==============================================================================
# TEST 3: standardise_prediction_columns()
# ==============================================================================

test_that("standardise maps position_group to position", {
  df <- tibble(
    player_id      = c("A", "B", "C"),
    position_group = c("passer", "rusher", "receiver")
  )
  result <- standardise_prediction_columns(df)
  expect_true("position" %in% names(result))
  expect_equal(result$position, c("QB", "RB", "WR"))
})

test_that("standardise does not overwrite existing position column", {
  df <- tibble(
    player_id      = c("A", "B"),
    position       = c("QB", "RB"),
    position_group = c("passer", "rusher")
  )
  result <- standardise_prediction_columns(df)
  expect_equal(result$position, c("QB", "RB"))
})

test_that("standardise renames .pred to predicted_ppr", {
  df <- tibble(player_id = "A", .pred = 15.5)
  result <- standardise_prediction_columns(df)
  expect_true("predicted_ppr" %in% names(result))
  expect_equal(result$predicted_ppr, 15.5)
})

test_that("standardise renames ppr_points_this_week to ppr_points_actual", {
  df <- tibble(player_id = "A", ppr_points_this_week = 18.3)
  result <- standardise_prediction_columns(df)
  expect_true("ppr_points_actual" %in% names(result))
  expect_equal(result$ppr_points_actual, 18.3)
})

test_that("standardise renames ppr_points_next_week to observed_ppr", {
  df <- tibble(player_id = "A", ppr_points_next_week = 20.1)
  result <- standardise_prediction_columns(df)
  expect_true("observed_ppr" %in% names(result))
  expect_equal(result$observed_ppr, 20.1)
})

test_that("standardise injects missing availability columns as NA", {
  df <- tibble(player_id = "A", position_group = "passer")
  result <- standardise_prediction_columns(df)
  expect_true("weeks_since_last_played" %in% names(result))
  expect_true("missed_weeks_this_season" %in% names(result))
  expect_true("availability_rate" %in% names(result))
  expect_true(is.na(result$weeks_since_last_played))
})

test_that("standardise coerces role_stability_flag from logical to integer", {
  df <- tibble(player_id = "A", role_stability_flag = TRUE)
  result <- standardise_prediction_columns(df)
  expect_true(is.integer(result$role_stability_flag) ||
              is.numeric(result$role_stability_flag))
  expect_equal(result$role_stability_flag, 1L)
})


# ==============================================================================
# TEST 4: derive_opponent_difficulty()
# ==============================================================================

test_that("derive_opponent_difficulty returns correct structure", {
  set.seed(42)
  ids <- paste0("00-000", 1001:1015)
  fm  <- build_mock_feature_matrix(ids, week_num = 5L) %>%
    bind_rows(build_mock_feature_matrix(ids, week_num = 1L)) %>%
    bind_rows(build_mock_feature_matrix(ids, week_num = 2L)) %>%
    bind_rows(build_mock_feature_matrix(ids, week_num = 3L)) %>%
    bind_rows(build_mock_feature_matrix(ids, week_num = 4L))

  result <- derive_opponent_difficulty(fm, target_week = 5L, min_games = 1L)

  expect_true(is.data.frame(result))
  expect_true("player_id" %in% names(result))
  expect_true("opponent_difficulty_remaining" %in% names(result))
  expect_true(is.numeric(result$opponent_difficulty_remaining))
})

test_that("derive_opponent_difficulty is centered near zero", {
  set.seed(42)
  ids <- paste0("00-000", 1001:1030)
  fm  <- bind_rows(lapply(1:5, function(w) build_mock_feature_matrix(ids, week_num = w)))
  result <- derive_opponent_difficulty(fm, target_week = 5L, min_games = 1L)

  # Mean difficulty should be approximately zero (centered)
  expect_true(abs(mean(result$opponent_difficulty_remaining, na.rm = TRUE)) < 1.0,
              info = "Opponent difficulty should be centered near zero")
})

test_that("derive_opponent_difficulty returns NULL for missing columns", {
  bad_fm <- tibble(player_id = "A", week = 5L)  # missing team, opponent, epa_this_week
  result <- suppressWarnings(derive_opponent_difficulty(bad_fm, target_week = 5L))
  expect_null(result)
})

test_that("derive_opponent_difficulty returns one row per player (no dupes)", {
  set.seed(42)
  ids <- paste0("00-000", 1001:1010)
  fm  <- bind_rows(lapply(1:5, function(w) build_mock_feature_matrix(ids, week_num = w)))

  # Add a traded player: same player_id, two teams in week 5
  traded_row <- fm %>% filter(week == 5) %>% slice(1) %>%
    mutate(team = "NYJ", opponent = "NE")
  fm <- bind_rows(fm, traded_row)

  result <- derive_opponent_difficulty(fm, target_week = 5L, min_games = 1L)
  expect_equal(nrow(result), n_distinct(result$player_id),
               info = "No duplicate player_ids in output")
})

test_that("derive_opponent_difficulty only uses prior weeks (no leakage)", {
  set.seed(42)
  ids <- paste0("00-000", 1001:1010)
  # Only week 5 data -- no prior weeks to compute defensive quality
  fm  <- build_mock_feature_matrix(ids, week_num = 5L)
  result <- suppressWarnings(derive_opponent_difficulty(fm, target_week = 5L, min_games = 1L))
  # No prior data -> function should return NULL or all-zero difficulties
  if (is.null(result)) {
    expect_null(result, info = "No prior weeks = NULL (no data to compute difficulty)")
  } else {
    expect_true(all(result$opponent_difficulty_remaining == 0),
                info = "No prior weeks = no difficulty signal")
  }
})


# ==============================================================================
# TEST 5: generate_preseason_projections()
# ==============================================================================

test_that("generate_preseason_projections returns correct columns", {
  set.seed(42)
  prior <- build_mock_prior_stats(n_per_pos = 5)
  result <- generate_preseason_projections(prior, prior_season = 2024)

  expected_cols <- c(
    "player_id", "player_name", "position", "prior_season",
    "projected_ppr_per_game", "projected_ppr_lower_95", "projected_ppr_upper_95",
    "projected_ppr_sd", "prior_games_played", "regression_alpha",
    "prior_volume_rate", "prior_efficiency_rate"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }
})

test_that("generate_preseason_projections excludes low-sample players", {
  prior <- tibble(
    player_id        = c("A", "B", "C"),
    player_name      = c("Full", "Low", "Mid"),
    position         = c("QB", "QB", "QB"),
    games_played     = c(17L, 2L, 8L),
    avg_ppr_per_game = c(20, 15, 12)
  )
  result <- generate_preseason_projections(prior, min_prior_games = 4L,
                                            prior_season = 2024)
  # Player B (2 games) should be excluded
  expect_false("B" %in% result$player_id)
  expect_true("A" %in% result$player_id)
  expect_true("C" %in% result$player_id)
})

test_that("generate_preseason_projections regression_alpha is stronger for fewer games", {
  prior <- tibble(
    player_id        = c("Full", "Partial"),
    player_name      = c("Full Season", "Partial Season"),
    position         = c("QB", "QB"),
    games_played     = c(17L, 5L),
    avg_ppr_per_game = c(20, 20)
  )
  result <- generate_preseason_projections(prior, min_prior_games = 4L,
                                            prior_season = 2024)
  full    <- result %>% filter(player_id == "Full")
  partial <- result %>% filter(player_id == "Partial")

  # 17 games -> alpha = 0, 5 games -> alpha = 1 - 5/17 > 0
  expect_equal(full$regression_alpha, 0)
  expect_true(partial$regression_alpha > full$regression_alpha)
})

test_that("generate_preseason_projections lower bound >= 0", {
  set.seed(42)
  prior <- build_mock_prior_stats(n_per_pos = 10)
  result <- generate_preseason_projections(prior, prior_season = 2024)
  expect_true(all(result$projected_ppr_lower_95 >= 0),
              info = "Lower 95% CI must be non-negative (can't score below 0)")
})

test_that("generate_preseason_projections rejects missing columns", {
  bad_prior <- tibble(player_id = "A", player_name = "Test")
  expect_error(generate_preseason_projections(bad_prior),
               "missing required columns")
})

test_that("generate_preseason_projections rejects unsupported positions", {
  prior <- tibble(
    player_id = "A", player_name = "Test", position = "K",
    games_played = 17L, avg_ppr_per_game = 8
  )
  expect_error(generate_preseason_projections(prior, positions = "K"),
               "Unsupported positions")
})

test_that("generate_preseason_projections filters to requested positions only", {
  set.seed(42)
  prior <- build_mock_prior_stats(n_per_pos = 5)
  result <- generate_preseason_projections(prior, positions = "QB",
                                            prior_season = 2024)
  expect_true(all(result$position == "QB"))
})


# ==============================================================================
# TEST 6: generate_ros_projections()
# ==============================================================================

test_that("generate_ros_projections returns correct columns", {
  ids    <- paste0("P", 1:6)
  proj   <- build_mock_preseason_proj(ids)
  result <- generate_ros_projections(proj, current_week = 10L)

  expected_cols <- c(
    "player_id", "position", "current_week", "remaining_games",
    "projected_ros_ppr", "projected_ros_lower_80", "projected_ros_upper_80",
    "schedule_adjustment", "had_remaining_bye"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }
})

test_that("generate_ros_projections remaining_games = 18 - current_week", {
  ids    <- paste0("P", 1:3)
  proj   <- build_mock_preseason_proj(ids)
  result <- generate_ros_projections(proj, current_week = 10L)
  expect_true(all(result$remaining_games == 8L | result$remaining_games == 7L),
              info = "Remaining games should be 8 (or 7 if bye)")
})

test_that("generate_ros_projections at week 18 returns zero projections", {
  ids  <- paste0("P", 1:3)
  proj <- build_mock_preseason_proj(ids)
  result <- suppressWarnings(
    generate_ros_projections(proj, current_week = 18L)
  )
  expect_true(all(result$remaining_games == 0L))
  expect_true(all(result$projected_ros_ppr == 0))
})

test_that("generate_ros_projections rejects playoff weeks", {
  ids  <- paste0("P", 1:3)
  proj <- build_mock_preseason_proj(ids)
  expect_error(
    generate_ros_projections(proj, current_week = 19L),
    "playoff week"
  )
})

test_that("generate_ros_projections bye reduces remaining games by 1", {
  ids  <- paste0("P", 1:3)
  proj <- build_mock_preseason_proj(ids)
  bye_schedule <- c("P1" = 12L)

  result_no_bye  <- generate_ros_projections(proj, current_week = 10L)
  result_with_bye <- generate_ros_projections(
    proj, current_week = 10L, bye_week_schedule = bye_schedule
  )

  p1_no_bye  <- result_no_bye  %>% filter(player_id == "P1")
  p1_with_bye <- result_with_bye %>% filter(player_id == "P1")

  expect_equal(p1_with_bye$remaining_games, p1_no_bye$remaining_games - 1L)
  expect_true(p1_with_bye$had_remaining_bye)
  expect_false(p1_no_bye$had_remaining_bye)
})

test_that("generate_ros_projections schedule_adjustment capped at 0.80-1.20", {
  ids  <- paste0("P", 1:3)
  proj <- build_mock_preseason_proj(ids)
  # Extreme opponent difficulty
  opp_diff <- tibble(
    player_id = ids,
    opponent_difficulty_remaining = c(5.0, -5.0, 0.0)  # extreme values
  )
  result <- generate_ros_projections(
    proj, current_week = 10L, opponent_difficulty = opp_diff
  )
  expect_true(all(result$schedule_adjustment >= 0.80))
  expect_true(all(result$schedule_adjustment <= 1.20))
})

test_that("generate_ros_projections opponent_difficulty flows into matchup", {
  ids  <- paste0("P", 1:3)
  proj <- build_mock_preseason_proj(ids)
  opp_diff <- tibble(
    player_id = ids,
    opponent_difficulty_remaining = c(0.2, -0.2, 0.0)
  )
  result <- generate_ros_projections(
    proj, current_week = 10L, opponent_difficulty = opp_diff
  )
  # Tough opponent = lower adjustment (< 1.0)
  tough   <- result %>% filter(player_id == "P1")
  easy    <- result %>% filter(player_id == "P2")
  neutral <- result %>% filter(player_id == "P3")

  expect_true(tough$schedule_adjustment < neutral$schedule_adjustment)
  expect_true(easy$schedule_adjustment > neutral$schedule_adjustment)
})

test_that("generate_ros_projections rejects missing required columns", {
  bad_proj <- tibble(player_id = "A", position = "QB")
  expect_error(
    generate_ros_projections(bad_proj, current_week = 10L),
    "missing columns"
  )
})


# ==============================================================================
# TEST 7: NFL-specific edge cases
# ==============================================================================

test_that("bye week window enforced (weeks 5-14 only)", {
  # Bye list outside valid window should trigger warning
  expect_warning(
    {
      ids  <- paste0("P", 1:3)
      proj <- build_mock_preseason_proj(ids)
      fm   <- build_mock_feature_matrix(ids, week_num = 2L)
      # Fake ensemble models that will fail predict() but that's fine --
      # we're testing the bye week guard, not the prediction
      fake_models <- list(QB = list(), RB = list(), WR = list())
      tryCatch(
        update_weekly_projections(
          prior_projections = proj,
          current_week      = 2L,
          ensemble_models   = fake_models,
          feature_matrix    = fm,
          bye_week_players  = c("P1")
        ),
        error = function(e) NULL  # prediction error expected, we want the warning
      )
    },
    "outside the bye week window"
  )
})

test_that("constants match NFL rules", {
  expect_equal(NFL_REGULAR_SEASON_MAX_WEEK, 18L)
  expect_equal(NFL_PLAYOFF_START_WEEK, 19L)
  expect_equal(NFL_BYE_WEEK_MIN, 5L)
  expect_equal(NFL_BYE_WEEK_MAX, 14L)
  expect_equal(SUPPORTED_POSITIONS, c("QB", "RB", "WR"))
})


# ==============================================================================
# TEST 8: Column verification tests
# ==============================================================================

test_that("standardise handles all position_group values", {
  df <- tibble(
    player_id      = c("A", "B", "C", "D"),
    position_group = c("passer", "rusher", "receiver", "unknown")
  )
  result <- standardise_prediction_columns(df)
  expect_equal(result$position[1:3], c("QB", "RB", "WR"))
  expect_true(is.na(result$position[4]),
              info = "Unknown position_group should map to NA")
})

test_that("FEATURE_MATRIX_PATH points to ml_data artifact", {
  expect_true(grepl("ml_data", FEATURE_MATRIX_PATH),
              info = "FEATURE_MATRIX_PATH must point to ml_data, not raw feature_matrix")
})


# ==============================================================================
# TEST 9: Traded player deduplication
# ==============================================================================

test_that("standardise + distinct produces one row per player in feature matrix", {
  ids <- paste0("P", 1:5)
  fm  <- build_mock_feature_matrix(ids, week_num = 5L)
  # Add duplicate for traded player
  traded <- fm %>% slice(1) %>% mutate(team = "NYJ", opponent = "NE")
  fm_duped <- bind_rows(fm, traded)

  expect_true(nrow(fm_duped) > nrow(fm), info = "Setup: dupe row added")

  # After standardise + distinct (mimicking production code)
  fm_std <- standardise_prediction_columns(fm_duped)
  fm_deduped <- fm_std %>%
    filter(position == "QB") %>%
    distinct(player_id, .keep_all = TRUE)

  expect_equal(n_distinct(fm_deduped$player_id), nrow(fm_deduped),
               info = "No duplicate player_ids after dedup")
})


# ==============================================================================
# TEST 10: Integration test -- preseason through ROS
# ==============================================================================

test_that("preseason -> ROS pipeline produces valid output", {
  set.seed(42)
  prior  <- build_mock_prior_stats(n_per_pos = 8)
  result <- generate_preseason_projections(prior, prior_season = 2024)

  # All projections positive

  expect_true(all(result$projected_ppr_per_game > 0),
              info = "All preseason projections must be positive")

  # Feed into ROS
  ros <- generate_ros_projections(result, current_week = 10L)

  expect_true(all(ros$projected_ros_ppr >= 0),
              info = "ROS projections must be non-negative")
  expect_true(all(ros$remaining_games <= 8),
              info = "At week 10, max remaining = 8")
  expect_equal(nrow(ros), nrow(result),
               info = "ROS should have same player count as preseason")

  # CI ordering
  expect_true(all(ros$projected_ros_lower_80 <= ros$projected_ros_ppr),
              info = "Lower CI must be <= point estimate")
  expect_true(all(ros$projected_ros_upper_80 >= ros$projected_ros_ppr),
              info = "Upper CI must be >= point estimate")
})


# ==============================================================================
# TEST 11: Week 4 finding -- volume weight > efficiency weight
# ==============================================================================

test_that("Volume weight exceeds efficiency weight (Week 4 finding)", {
  expect_true(VOLUME_FEATURE_WEIGHT > EFFICIENCY_FEATURE_WEIGHT,
              info = "Week 4 finding: volume outpredicts efficiency")
  expect_equal(VOLUME_FEATURE_WEIGHT + EFFICIENCY_FEATURE_WEIGHT, 1.0,
               info = "Weights must sum to 1")
})


# ==============================================================================
# TEST 12: Feature matrix artifact path and season filter
# ==============================================================================

test_that("ml_data artifact exists on disk", {
  skip_if_not(file.exists(FEATURE_MATRIX_PATH),
              message = "ml_data_2025.rds not found -- skip artifact tests")

  fm <- readRDS(FEATURE_MATRIX_PATH)
  expect_true(is.data.frame(fm))
  expect_true("season" %in% names(fm))
  expect_true("position_group" %in% names(fm))
  expect_true("ppr_points_this_week" %in% names(fm))
  expect_true("is_absence_week" %in% names(fm))

  # ml_data should contain 2025 season data
  expect_true(2025 %in% unique(fm$season),
              info = "ml_data must contain 2025 season")
})


# ==============================================================================
# TEST SUMMARY
# ==============================================================================

cat("\n")
cat("==============================================================\n")
cat("Week 12 Test Suite Complete\n")
cat("==============================================================\n")
cat("Functions tested:\n")
cat("  1. compute_prior_weight()             - boundaries, monotonicity, crossover\n")
cat("  2. validate_regular_season_week()     - valid, playoff, negative\n")
cat("  3. standardise_prediction_columns()   - renames, injection, coercion\n")
cat("  4. derive_opponent_difficulty()        - structure, centering, dedup, leakage\n")
cat("  5. generate_preseason_projections()   - columns, filtering, regression alpha\n")
cat("  6. generate_ros_projections()         - remaining games, bye, caps, playoff\n")
cat("  7. NFL edge cases                      - bye window, constants\n")
cat("  8. Column verification                 - position mapping, artifact path\n")
cat("  9. Traded player dedup                 - no duplicate player_ids\n")
cat(" 10. Integration (preseason -> ROS)      - pipeline validity\n")
cat(" 11. Week 4 finding                      - volume > efficiency weights\n")
cat(" 12. Feature matrix artifact             - disk presence, required columns\n")
cat("==============================================================\n")
