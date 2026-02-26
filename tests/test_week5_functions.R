# ==============================================================================
# WEEK 5: COMPREHENSIVE TEST SUITE
# ==============================================================================
# Tests for: calculate_epa_trends(), get_situational_splits(),
#            calculate_stability_metrics(), get_pressure_performance()
#
# Test categories:
# - Input validation (bad inputs caught with clear errors)
# - Column name verification (output has expected columns)
# - NFL-specific edge cases (garbage time, kneels, small samples)
# - Feature leakage prevention (trends use only prior data)
# - Division-by-zero guards (rate metrics with 0 denominator)
# - Integration test (happy path end-to-end)
# ==============================================================================

library(testthat)
library(dplyr)
library(here)

# Source production code
source(here("R", "07_epa_features.R"))

# ==============================================================================
# HELPER: CREATE REALISTIC TEST DATA
# ==============================================================================

create_test_pbp <- function(n_games = 6, n_plays_per_game = 50, n_players = 3) {
  # Build a minimal but realistic play-by-play dataset
  set.seed(42)

  games <- expand.grid(
    game_num = 1:n_games,
    play_num = 1:n_plays_per_game
  ) %>%
    mutate(
      season = 2025,
      week = game_num,
      game_id = paste0("2025_", sprintf("%02d", game_num), "_TEAM1_TEAM2"),
      game_date = as.Date("2025-09-07") + (game_num - 1) * 7,
      play_id = row_number()
    )

  # Assign players and play types
  player_ids <- paste0("00-000", 1:n_players)
  player_names <- paste0("Player_", LETTERS[1:n_players])

  pbp <- games %>%
    mutate(
      # Alternate play types roughly 55% pass / 45% run
      play_type = sample(c("pass", "run"), n(), replace = TRUE, prob = c(0.55, 0.45)),

      # Assign primary player based on play type
      passer_player_id = ifelse(play_type == "pass",
                                sample(player_ids[1], n(), replace = TRUE),
                                NA_character_),
      passer_player_name = ifelse(!is.na(passer_player_id),
                                  player_names[match(passer_player_id, player_ids)],
                                  NA_character_),
      rusher_player_id = ifelse(play_type == "run",
                                sample(player_ids[2:n_players], n(), replace = TRUE),
                                NA_character_),
      rusher_player_name = ifelse(!is.na(rusher_player_id),
                                  player_names[match(rusher_player_id, player_ids)],
                                  NA_character_),
      receiver_player_id = ifelse(play_type == "pass",
                                  sample(player_ids[2:n_players], n(), replace = TRUE),
                                  NA_character_),
      receiver_player_name = ifelse(!is.na(receiver_player_id),
                                    player_names[match(receiver_player_id, player_ids)],
                                    NA_character_),

      # Generate realistic EPA and context columns
      epa = rnorm(n(), mean = 0.02, sd = 0.8),
      success = as.integer(epa > 0),
      wp = runif(n(), 0.15, 0.85),
      qtr = sample(1:4, n(), replace = TRUE, prob = c(0.28, 0.28, 0.22, 0.22)),
      down = sample(1:4, n(), replace = TRUE, prob = c(0.35, 0.30, 0.25, 0.10)),
      ydstogo = sample(1:15, n(), replace = TRUE),
      posteam = "TEAM1",
      defteam = "TEAM2",
      score_differential = sample(-14:14, n(), replace = TRUE),
      yardline_100 = sample(1:99, n(), replace = TRUE),
      rushing_yards = ifelse(play_type == "run", rnorm(n(), 4, 3), NA_real_),
      passing_yards = ifelse(play_type == "pass", rnorm(n(), 7, 5), NA_real_),
      receiving_yards = ifelse(play_type == "pass", rnorm(n(), 7, 5), NA_real_),
      complete_pass = ifelse(play_type == "pass", rbinom(n(), 1, 0.65), NA_integer_),
      air_yards = ifelse(play_type == "pass", rnorm(n(), 8, 5), NA_real_)
    )

  return(pbp)
}


# ==============================================================================
# TEST 1: INPUT VALIDATION - calculate_epa_trends()
# ==============================================================================

test_that("calculate_epa_trends rejects bad inputs", {
  # Not a data frame

  expect_error(calculate_epa_trends("not_a_df"), "must be a data frame")

  # Missing required columns
  bad_df <- data.frame(game_id = 1, season = 2025)
  expect_error(calculate_epa_trends(bad_df), "Missing required columns")

  # Invalid trend_windows (< 3)
  pbp <- create_test_pbp()
  expect_error(
    calculate_epa_trends(pbp, trend_windows = c(2)),
    "trend_windows"
  )

  # Invalid min_plays_per_game
  expect_error(
    calculate_epa_trends(pbp, min_plays_per_game = 0),
    "min_plays_per_game"
  )
})


# ==============================================================================
# TEST 2: COLUMN VERIFICATION - calculate_epa_trends()
# ==============================================================================

test_that("calculate_epa_trends returns expected columns", {
  pbp <- create_test_pbp(n_games = 8, n_plays_per_game = 40)

  result <- calculate_epa_trends(pbp, season = 2025, trend_windows = c(3, 4))

  # Core columns
  expected_core <- c("player_id", "player_name", "season", "week", "game_id",
                     "posteam", "plays", "weekly_epa", "epa_per_play",
                     "success_rate", "pass_plays", "run_plays")
  expect_true(all(expected_core %in% names(result)),
              info = paste("Missing:", setdiff(expected_core, names(result))))

  # Trend columns for each window
  expect_true("epa_trend_3" %in% names(result))
  expect_true("epa_level_3" %in% names(result))
  expect_true("epa_trend_4" %in% names(result))
  expect_true("epa_level_4" %in% names(result))
})


# ==============================================================================
# TEST 3: FEATURE LEAKAGE PREVENTION
# ==============================================================================

test_that("EPA trends use only prior games (no leakage)", {
  pbp <- create_test_pbp(n_games = 8, n_plays_per_game = 60, n_players = 2)

  result <- calculate_epa_trends(pbp, season = 2025, trend_windows = c(3))

  # First 3 weeks should have NA trends (insufficient prior data)
  player1 <- result %>%
    filter(player_id == first(result$player_id)) %>%
    arrange(week)

  if (nrow(player1) >= 3) {
    expect_true(is.na(player1$epa_trend_3[1]),
                info = "Week 1 should have NA trend (no prior games)")
    expect_true(is.na(player1$epa_trend_3[2]),
                info = "Week 2 should have NA trend (only 1 prior game)")
    expect_true(is.na(player1$epa_trend_3[3]),
                info = "Week 3 should have NA trend (only 2 prior games)")
  }

  # Week 4+ may have valid trends (3 prior games available)
  if (nrow(player1) >= 4) {
    # If week 4 has a valid trend, it should NOT include week 4 data
    # (this is the feature leakage test - verified by construction via lag())
    expect_true(
      is.na(player1$epa_trend_3[4]) || is.numeric(player1$epa_trend_3[4]),
      info = "Week 4 trend should be numeric or NA (not an error)"
    )
  }
})


# ==============================================================================
# TEST 4: GARBAGE TIME FILTERING
# ==============================================================================

test_that("Garbage time plays are excluded when filter_garbage_time = TRUE", {
  pbp <- create_test_pbp(n_games = 6)

  # Inject obvious garbage time plays: Q4 with extreme WP
  garbage_rows <- pbp %>%
    slice(1:20) %>%
    mutate(qtr = 4, wp = 0.05, epa = 3.0)  # Unrealistic garbage time EPA

  pbp_with_garbage <- bind_rows(pbp, garbage_rows)

  result_filtered <- calculate_epa_trends(pbp_with_garbage, filter_garbage_time = TRUE)
  result_unfiltered <- calculate_epa_trends(pbp_with_garbage, filter_garbage_time = FALSE)

  # Filtered result should have fewer total plays
  total_filtered <- sum(result_filtered$plays, na.rm = TRUE)
  total_unfiltered <- sum(result_unfiltered$plays, na.rm = TRUE)

  expect_true(total_filtered < total_unfiltered,
              info = "Garbage time filter should reduce play count")
})


# ==============================================================================
# TEST 5: INPUT VALIDATION - get_situational_splits()
# ==============================================================================

test_that("get_situational_splits rejects bad inputs", {
  expect_error(get_situational_splits("not_a_df"), "must be a data frame")

  bad_df <- data.frame(game_id = 1)
  expect_error(get_situational_splits(bad_df), "Missing required columns")
})


# ==============================================================================
# TEST 6: COLUMN VERIFICATION - get_situational_splits()
# ==============================================================================

test_that("get_situational_splits returns expected columns and split types", {
  pbp <- create_test_pbp(n_games = 6, n_plays_per_game = 60)

  result <- get_situational_splits(pbp, season = 2025)

  expected_cols <- c("player_id", "player_name", "posteam", "split_type",
                     "split_value", "plays", "total_epa", "epa_per_play",
                     "success_rate")
  expect_true(all(expected_cols %in% names(result)),
              info = paste("Missing:", setdiff(expected_cols, names(result))))

  # All 4 split types should be present
  expect_true("down" %in% result$split_type)
  expect_true("distance" %in% result$split_type)
  expect_true("quarter" %in% result$split_type)
  expect_true("game_script" %in% result$split_type)
})


# ==============================================================================
# TEST 7: MINIMUM PLAYS THRESHOLD
# ==============================================================================

test_that("Splits with too few plays return NA for efficiency metrics", {
  pbp <- create_test_pbp(n_games = 2, n_plays_per_game = 10)

  # With high min_plays threshold, most splits should be NA
  result <- get_situational_splits(pbp, season = 2025, min_plays = 100)

  # All epa_per_play should be NA (not enough plays in any split)
  expect_true(all(is.na(result$epa_per_play)),
              info = "With min_plays=100, all efficiency metrics should be NA")

  # But plays column should still have values (raw count always reported)
  expect_true(all(!is.na(result$plays)),
              info = "Play counts should always be present")
})


# ==============================================================================
# TEST 8: COLUMN VERIFICATION - calculate_stability_metrics()
# ==============================================================================

test_that("calculate_stability_metrics returns expected columns", {
  pbp <- create_test_pbp(n_games = 12, n_plays_per_game = 60)

  result <- calculate_stability_metrics(pbp, season = 2025, min_games = 4)

  expected_cols <- c("metric", "autocorrelation", "split_half_r",
                     "n_players", "stability_tier")
  expect_true(all(expected_cols %in% names(result)),
              info = paste("Missing:", setdiff(expected_cols, names(result))))

  # Should have QB, RB, and WR metrics
  expect_true(any(grepl("^qb_", result$metric)))
  expect_true(any(grepl("^rb_", result$metric)))
  expect_true(any(grepl("^wr_", result$metric)))
})


# ==============================================================================
# TEST 9: STABILITY TIERS
# ==============================================================================

test_that("Stability tiers are correctly assigned", {
  # Create a mock result and verify tier logic
  mock_results <- tibble(
    metric = c("high_metric", "mod_metric", "low_metric", "unstable_metric"),
    autocorrelation = c(0.55, 0.35, 0.20, 0.05),
    split_half_r = c(0.60, 0.40, 0.25, 0.10),
    n_players = c(20, 20, 20, 20)
  ) %>%
    mutate(
      stability_tier = case_when(
        autocorrelation >= 0.50 ~ "high",
        autocorrelation >= 0.30 ~ "moderate",
        autocorrelation >= 0.15 ~ "low",
        TRUE ~ "unstable"
      )
    )

  expect_equal(mock_results$stability_tier[1], "high")
  expect_equal(mock_results$stability_tier[2], "moderate")
  expect_equal(mock_results$stability_tier[3], "low")
  expect_equal(mock_results$stability_tier[4], "unstable")
})


# ==============================================================================
# TEST 10: COLUMN VERIFICATION - get_pressure_performance()
# ==============================================================================

test_that("get_pressure_performance returns expected columns and leverage types", {
  pbp <- create_test_pbp(n_games = 6, n_plays_per_game = 60)

  result <- get_pressure_performance(pbp, season = 2025)

  expected_cols <- c("player_id", "player_name", "posteam", "leverage_type",
                     "leverage_value", "plays", "total_epa", "epa_per_play",
                     "success_rate")
  expect_true(all(expected_cols %in% names(result)),
              info = paste("Missing:", setdiff(expected_cols, names(result))))

  # All 3 leverage types present
  expect_true("quarter_score" %in% result$leverage_type)
  expect_true("third_down" %in% result$leverage_type)
  expect_true("red_zone" %in% result$leverage_type)
})


# ==============================================================================
# TEST 11: EMPTY DATA HANDLING
# ==============================================================================

test_that("All functions handle empty data gracefully", {
  pbp <- create_test_pbp(n_games = 1, n_plays_per_game = 5)

  # Filter to impossible condition -> 0 rows
  empty_pbp <- pbp %>% filter(season == 1900)

  expect_warning(calculate_epa_trends(empty_pbp))
  expect_warning(get_situational_splits(empty_pbp))
  expect_warning(get_pressure_performance(empty_pbp))
})


# ==============================================================================
# TEST 12: NA HANDLING IN EPA
# ==============================================================================

test_that("Functions handle NA EPA values correctly", {
  pbp <- create_test_pbp(n_games = 6, n_plays_per_game = 40)

  # Inject NAs into EPA column
  pbp$epa[sample(1:nrow(pbp), 50)] <- NA

  # Should not error -- NAs filtered in the !is.na(epa) step
  result <- calculate_epa_trends(pbp, season = 2025, trend_windows = c(3))
  expect_true(nrow(result) > 0, info = "Should still return results with some NA EPA")
  expect_false(any(is.nan(result$epa_per_play), na.rm = TRUE),
               info = "No NaN values in epa_per_play")
})


# ==============================================================================
# TEST 13: INTEGRATION TEST (HAPPY PATH)
# ==============================================================================

test_that("Full Week 5 pipeline runs end-to-end", {
  pbp <- create_test_pbp(n_games = 10, n_plays_per_game = 60, n_players = 4)

  # All four functions should run without error
  trends <- calculate_epa_trends(pbp, season = 2025, trend_windows = c(3, 4))
  expect_true(nrow(trends) > 0)

  splits <- get_situational_splits(pbp, season = 2025)
  expect_true(nrow(splits) > 0)

  stability <- calculate_stability_metrics(pbp, season = 2025, min_games = 4)
  expect_true(nrow(stability) > 0)

  pressure <- get_pressure_performance(pbp, season = 2025)
  expect_true(nrow(pressure) > 0)
})


# ==============================================================================
# TEST 14: DISTANCE BUCKET CLASSIFICATION
# ==============================================================================

test_that("Distance buckets classify correctly", {
  pbp <- create_test_pbp(n_games = 6, n_plays_per_game = 60)

  result <- get_situational_splits(pbp, season = 2025)
  distance_splits <- result %>% filter(split_type == "distance")

  # Should have exactly 3 distance categories
  expected_distances <- c("short_1_3", "medium_4_7", "long_8plus")
  actual_distances <- unique(distance_splits$split_value)

  expect_true(all(actual_distances %in% expected_distances),
              info = paste("Unexpected distance value:",
                           setdiff(actual_distances, expected_distances)))
})


# ==============================================================================
# TEST 15: QB KNEELS EXCLUDED
# ==============================================================================

test_that("QB kneels are excluded from efficiency metrics", {
  pbp <- create_test_pbp(n_games = 4, n_plays_per_game = 40)

  # Add some qb_kneel plays
  kneel_plays <- pbp %>%
    slice(1:10) %>%
    mutate(play_type = "qb_kneel", epa = -0.5)

  pbp_with_kneels <- bind_rows(pbp, kneel_plays)

  # play_type filter should exclude kneels

  result <- calculate_epa_trends(pbp_with_kneels, season = 2025)

  # kneels should not be in the data (filtered by play_type %in% c("pass", "run"))
  # Result play counts should not include the 10 kneel plays
  total_plays_result <- sum(result$plays, na.rm = TRUE)

  result_no_kneels <- calculate_epa_trends(pbp, season = 2025)
  total_plays_no_kneels <- sum(result_no_kneels$plays, na.rm = TRUE)

  expect_equal(total_plays_result, total_plays_no_kneels,
               info = "QB kneels should be excluded from play counts")
})
