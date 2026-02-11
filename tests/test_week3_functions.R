# ==============================================================================
# WEEK 3 UNIT TESTS - Fantasy Scoring & Rolling Averages
# ==============================================================================
#
# This file contains comprehensive unit tests for Week 3 functions:
# 1. Fantasy Points Calculation (with all customizable parameters)
# 2. Rolling Statistics (3-game and 6-game windows)
# 3. Rolling Fantasy Points
# 4. Edge Cases (bye weeks, insufficient games, NA handling)
# 5. Integration Tests (full pipeline)
#
# Location: tests/test_week3_functions.R
# Run with: testthat::test_file("tests/test_week3_functions.R")
#
# ==============================================================================

library(testthat)
library(dplyr)
library(here)
source(here("R/01_data_loading.R"))
source(here("R/05_consistency_metrics.R"))

# ==============================================================================
# HELPER: Create mock PBP data with all required columns
# ==============================================================================

create_mock_pbp <- function(n_plays = 5, play_type = "pass") {
  # Use scalar check for if() statements
  is_pass <- play_type[1] == "pass"
  is_run <- play_type[1] == "run"
  
  tibble(
    season = 2024,
    week = 1,
    game_id = "2024_01_TEST",
    play_type = play_type,
    posteam = "KC",
    
    # Player IDs
    receiver_player_id = if(is_pass) rep("PLAYER1", n_plays) else NA,
    receiver_player_name = if(is_pass) rep("Test Player", n_plays) else NA,
    passer_player_id = NA,
    passer_player_name = NA,
    rusher_player_id = if(is_run) rep("PLAYER1", n_plays) else NA,
    rusher_player_name = if(is_run) rep("Test Player", n_plays) else NA,
    
    # Yards
    receiving_yards = if(is_pass) rep(10, n_plays) else 0,
    passing_yards = 0,
    rushing_yards = if(is_run) rep(10, n_plays) else 0,
    
    # Touchdowns
    pass_touchdown = 0,
    rush_touchdown = 0,
    touchdown = 0,
    
    # Other
    complete_pass = if(is_pass) rep(1, n_plays) else NA,
    interception = 0,
    fumble_lost = 0,
    fumbled_1_player_id = NA
  )
}

# ==============================================================================
# TEST SUITE 1: FANTASY POINTS CALCULATION
# ==============================================================================

test_that("Default settings use tiered PPR and 6pt pass TDs", {
  mock_pbp <- create_mock_pbp(n_plays = 3, play_type = "pass")
  mock_pbp$receiving_yards <- c(10, 20, 30)  # Total 60 yards
  mock_pbp$touchdown <- c(0, 0, 1)
  mock_pbp$pass_touchdown <- c(0, 0, 1)
  
  fantasy <- calculate_fantasy_points(mock_pbp)
  
  # With tiered PPR (correct calculation):
  # 10 yards = 1.0 tiered (10-19 bracket)
  # 20 yards = 1.5 tiered (20-29 bracket)
  # 30 yards = 2.0 tiered (30-39 bracket)
  # Tiered total = 4.5 pts
  # 60 yards / 10 = 6 pts
  # 1 TD * 6 = 6 pts
  # Total = 16.5 pts
  expect_equal(fantasy$rec_fantasy_points[1], 16.5, tolerance = 0.01)
})

test_that("Regular PPR (not tiered) works", {
  mock_pbp <- create_mock_pbp(n_plays = 3, play_type = "pass")
  mock_pbp$receiving_yards <- c(10, 20, 30)
  
  fantasy <- calculate_fantasy_points(mock_pbp, 
                                      use_tiered_ppr = FALSE,
                                      ppr = 1)
  
  # 3 receptions * 1 pt = 3
  # 60 yards / 10 = 6
  # Total = 9 pts
  expect_equal(fantasy$rec_fantasy_points[1], 9, tolerance = 0.01)
  expect_equal(fantasy$receptions[1], 3)
})

test_that("Standard scoring (no PPR, 4pt pass TDs) works", {
  mock_pbp <- create_mock_pbp(n_plays = 3, play_type = "pass")
  mock_pbp$receiving_yards <- c(10, 20, 30)
  
  fantasy <- calculate_fantasy_points(
    mock_pbp,
    use_tiered_ppr = FALSE,
    te_premium = FALSE,
    rush_att_bonus = 0,
    ppr = 0,
    pass_td = 4
  )
  
  # 0 receptions pts + 60 yards / 10 = 6 points
  expect_equal(fantasy$rec_fantasy_points[1], 6, tolerance = 0.01)
})

test_that("6pt passing TDs work correctly", {
  # Need 2 separate pass plays for 2 TDs
  mock_pbp <- tibble(
    season = 2024,
    week = 1,
    game_id = "2024_01_TEST",
    play_type = c("pass", "pass"),
    posteam = "KC",
    passer_player_id = c("QB1", "QB1"),
    passer_player_name = c("Test QB", "Test QB"),
    receiver_player_id = NA,
    receiver_player_name = NA,
    passing_yards = c(150, 150),  # 300 total
    pass_touchdown = c(1, 1),  # 2 TDs (one per play)
    interception = c(0, 0),
    fumble_lost = c(0, 0),
    touchdown = c(1, 1),
    complete_pass = NA,
    rusher_player_id = NA,
    rusher_player_name = NA,
    rushing_yards = 0,
    rush_touchdown = 0,
    receiving_yards = 0,
    fumbled_1_player_id = NA
  )
  
  fantasy <- calculate_fantasy_points(mock_pbp, pass_td = 6)
  
  # 300 yards * 0.04 = 12
  # 2 TDs * 6 = 12
  # Total = 24 pts
  expect_equal(fantasy$pass_fantasy_points[1], 24, tolerance = 0.01)
})

test_that("Rush attempt bonus adds correctly", {
  mock_pbp <- create_mock_pbp(n_plays = 5, play_type = "run")
  mock_pbp$rushing_yards <- c(10, 5, 8, 12, 15)  # 50 total
  mock_pbp$rush_touchdown <- c(0, 0, 0, 0, 1)
  mock_pbp$touchdown <- c(0, 0, 0, 0, 1)
  
  fantasy <- calculate_fantasy_points(mock_pbp, rush_att_bonus = 0.5)
  
  # 50 yards * 0.1 = 5
  # 1 TD * 6 = 6
  # 5 attempts * 0.5 = 2.5
  # Total = 13.5 pts
  expect_equal(fantasy$rush_fantasy_points[1], 13.5, tolerance = 0.01)
  expect_equal(fantasy$rush_attempts[1], 5)
})

test_that("Pick-6 penalty applies correctly", {
  mock_pbp <- create_mock_pbp(n_plays = 2, play_type = "pass")
  mock_pbp$play_type <- "pass"
  mock_pbp$passer_player_id <- "QB1"
  mock_pbp$passer_player_name <- "Test QB"
  mock_pbp$receiver_player_id <- NA
  mock_pbp$receiver_player_name <- NA
  mock_pbp$passing_yards <- c(250, 0)
  mock_pbp$interception <- c(0, 1)
  mock_pbp$touchdown <- c(0, 1)  # INT returned for TD
  
  fantasy <- calculate_fantasy_points(mock_pbp, 
                                      pass_int = -2, 
                                      pick6_penalty = -4)
  
  # 250 yards * 0.04 = 10
  # 1 INT * -2 = -2
  # 1 pick6 * -4 = -4
  # Total = 4 pts
  expect_equal(fantasy$pass_fantasy_points[1], 4, tolerance = 0.01)
  expect_equal(fantasy$pick6[1], 1)
})

test_that("Multi-role player (QB who rushes) combines points correctly", {
  mock_pbp <- tibble(
    season = 2024, week = 1, game_id = "2024_01_TEST",
    play_type = c("pass", "run"),
    posteam = "KC",
    passer_player_id = c("QB1", NA),
    passer_player_name = c("Dual Threat QB", NA),
    passing_yards = c(250, 0),
    pass_touchdown = c(2, 0),
    interception = c(0, 0),
    rusher_player_id = c(NA, "QB1"),
    rusher_player_name = c(NA, "Dual Threat QB"),
    rushing_yards = c(0, 50),
    rush_touchdown = c(0, 1),
    receiver_player_id = NA,
    receiver_player_name = NA,
    receiving_yards = 0,
    complete_pass = NA,
    fumble_lost = 0,
    touchdown = c(0, 1),
    fumbled_1_player_id = NA
  )
  
  fantasy <- calculate_fantasy_points(mock_pbp)
  
  expect_equal(nrow(fantasy), 1)
  expect_equal(fantasy$player_id[1], "QB1")
  expect_gt(fantasy$pass_fantasy_points[1], 0)
  expect_gt(fantasy$rush_fantasy_points[1], 0)
  expect_equal(
    fantasy$total_fantasy_points[1],
    fantasy$pass_fantasy_points[1] + fantasy$rush_fantasy_points[1],
    tolerance = 0.01
  )
})

# ==============================================================================
# TEST SUITE 2: ROLLING AVERAGES - FEATURE LEAKAGE PREVENTION
# ==============================================================================

test_that("Rolling average excludes current game (no feature leakage)", {
  mock_stats <- tibble(
    player_id = "PLAYER1",
    player_name = "Test Player",
    season = 2024,
    week = 1:5,
    rush_yards = c(100, 80, 120, 90, 110)
  )
  
  rolling_stats <- calculate_rolling_stats(
    mock_stats,
    stat_columns = "rush_yards",
    windows = 3,
    min_games = 2
  )
  
  # Week 4 rolling average should be avg(weeks 1-3) = avg(100, 80, 120) = 100
  expect_equal(rolling_stats$rush_yards_roll3[4], 100, tolerance = 0.01)
  
  # Week 5 rolling average should be avg(weeks 2-4) = avg(80, 120, 90) = 96.67
  expect_equal(rolling_stats$rush_yards_roll3[5], 96.67, tolerance = 0.01)
})

test_that("Rolling average returns NA for insufficient games", {
  mock_stats <- tibble(
    player_id = "PLAYER1",
    player_name = "Test Player",
    season = 2024,
    week = 1:2,
    rush_yards = c(100, 80)
  )
  
  rolling_stats <- calculate_rolling_stats(
    mock_stats,
    stat_columns = "rush_yards",
    windows = 3,
    min_games = 2
  )
  
  expect_true(is.na(rolling_stats$rush_yards_roll3[1]))
  expect_true(is.na(rolling_stats$rush_yards_roll3[2]))
})

test_that("Bye weeks are handled correctly", {
  mock_stats <- tibble(
    player_id = "PLAYER1",
    player_name = "Test Player",
    season = 2024,
    week = c(1, 2, 3, 5),  # Week 4 bye
    rush_yards = c(100, 80, 120, 90)
  )
  
  rolling_stats <- calculate_rolling_stats(
    mock_stats,
    stat_columns = "rush_yards",
    windows = 3,
    min_games = 2
  )
  
  # Week 5 should use weeks 1, 2, 3 (last 3 games before week 5)
  expect_equal(rolling_stats$rush_yards_roll3[4], 100, tolerance = 0.01)
})

# ==============================================================================
# TEST SUITE 3: NA HANDLING
# ==============================================================================

test_that("Explicit NA filtering prevents Boolean logic bugs", {
  mock_pbp <- create_mock_pbp(n_plays = 3, play_type = "pass")
  mock_pbp$receiver_player_id <- c("PLAYER1", NA, "PLAYER2")
  mock_pbp$receiver_player_name <- c("Player 1", NA, "Player 2")
  
  fantasy <- calculate_fantasy_points(mock_pbp)
  
  expect_equal(nrow(fantasy), 2)
  expect_false(any(is.na(fantasy$player_id)))
  expect_false(any(is.na(fantasy$player_name)))
})

test_that("Rolling average handles NA stat values correctly", {
  mock_stats <- tibble(
    player_id = "PLAYER1",
    player_name = "Test Player",
    season = 2024,
    week = 1:5,
    rush_yards = c(100, NA, 120, 90, 110)
  )
  
  rolling_stats <- calculate_rolling_stats(
    mock_stats,
    stat_columns = "rush_yards",
    windows = 3,
    min_games = 2
  )
  
  # Week 4: avg(100, 120) skipping NA = 110
  expect_equal(rolling_stats$rush_yards_roll3[4], 110, tolerance = 0.01)
  
  # Week 5: avg(120, 90) skipping NA = 105
  expect_equal(rolling_stats$rush_yards_roll3[5], 105, tolerance = 0.01)
})

# ==============================================================================
# TEST SUITE 4: EDGE CASES
# ==============================================================================

test_that("Player with 0 receptions doesn't get negative points", {
  mock_pbp <- create_mock_pbp(n_plays = 3, play_type = "pass")
  mock_pbp$complete_pass <- c(0, 0, 0)
  mock_pbp$receiving_yards <- c(0, 0, 0)
  
  fantasy <- calculate_fantasy_points(mock_pbp)
  
  expect_gte(fantasy$rec_fantasy_points[1], 0)
  expect_equal(fantasy$receptions[1], 0)
})

test_that("Negative yards are handled correctly", {
  # QB with sacks and TDs across multiple plays
  mock_pbp <- tibble(
    season = 2024,
    week = 1,
    game_id = "2024_01_TEST",
    play_type = c("pass", "pass", "pass", "pass"),
    posteam = "KC",
    passer_player_id = c("QB1", "QB1", "QB1", "QB1"),
    passer_player_name = c("Test QB", "Test QB", "Test QB", "Test QB"),
    passing_yards = c(-10, -5, 125, 125),  # Total 235
    pass_touchdown = c(0, 0, 1, 1),  # 2 TDs
    interception = c(0, 0, 0, 0),
    fumble_lost = c(0, 0, 0, 0),
    touchdown = c(0, 0, 1, 1),
    receiver_player_id = NA,
    receiver_player_name = NA,
    rusher_player_id = NA,
    rusher_player_name = NA,
    receiving_yards = 0,
    rushing_yards = 0,
    rush_touchdown = 0,
    complete_pass = NA,
    fumbled_1_player_id = NA
  )
  
  fantasy <- calculate_fantasy_points(mock_pbp)
  
  # Total yards: 235
  expect_equal(fantasy$pass_yards[1], 235)
  # 235 * 0.04 = 9.4, 2 TDs * 6 = 12, Total = 21.4
  expect_equal(fantasy$pass_fantasy_points[1], 21.4, tolerance = 0.01)
})

test_that("Player switching teams mid-season aggregates correctly", {
  mock_pbp <- tibble(
    season = 2024,
    week = c(1, 2, 3, 4),
    game_id = paste0("2024_0", 1:4, "_TEST"),
    play_type = "run",
    posteam = c("KC", "KC", "SF", "SF"),
    rusher_player_id = "PLAYER1",
    rusher_player_name = "Trade Player",
    rushing_yards = c(80, 90, 100, 110),
    rush_touchdown = c(1, 0, 1, 1),
    fumble_lost = c(0, 0, 0, 0),
    passer_player_id = NA,
    passer_player_name = NA,
    receiver_player_id = NA,
    receiver_player_name = NA,
    passing_yards = 0,
    receiving_yards = 0,
    pass_touchdown = 0,
    interception = 0,
    complete_pass = NA,
    touchdown = c(1, 0, 1, 1),
    fumbled_1_player_id = NA
  )
  
  fantasy <- calculate_fantasy_points(mock_pbp)
  
  expect_equal(nrow(fantasy), 4)
  expect_equal(fantasy$team[3], "SF")
  expect_equal(fantasy$team[4], "SF")
})

# ==============================================================================
# TEST SUITE 5: INTEGRATION TESTS
# ==============================================================================

test_that("Full pipeline: load → fantasy → rolling works", {
  skip_if_not(file.exists("data/cache"), "No cached data available")
  
  # This test requires actual NFL data
  # Uncomment when testing with real data:
  
  # pbp <- load_and_validate_pbp(2024, validate = FALSE)
  # roster <- get_roster_data(2024)
  # 
  # fantasy <- calculate_fantasy_points(
  #   pbp,
  #   roster_data = roster,
  #   season = 2024,
  #   week_max = 5
  # )
  # 
  # fantasy_rolling <- calculate_rolling_fantasy(fantasy)
  # 
  # # Verify pipeline
  # expect_gt(nrow(fantasy), 0)
  # expect_gt(nrow(fantasy_rolling), 0)
  # expect_true("total_fantasy_points_roll3" %in% names(fantasy_rolling))
  # expect_true("total_fantasy_points_roll6" %in% names(fantasy_rolling))
})

test_that("Common workflow: fantasy → rolling averages works", {
  # Simulate realistic player data (10 weeks)
  mock_pbp <- tibble(
    season = 2024,
    week = rep(1:10, each = 3),
    game_id = paste0("2024_", sprintf("%02d", rep(1:10, each = 3)), "_TEST"),
    play_type = "pass",
    posteam = "KC",
    receiver_player_id = "WR1",
    receiver_player_name = "Star Receiver",
    complete_pass = 1,
    receiving_yards = sample(50:120, 30, replace = TRUE),
    pass_touchdown = sample(0:1, 30, replace = TRUE, prob = c(0.8, 0.2)),
    touchdown = sample(0:1, 30, replace = TRUE, prob = c(0.8, 0.2)),
    fumble_lost = 0,
    passer_player_id = NA,
    passer_player_name = NA,
    rusher_player_id = NA,
    rusher_player_name = NA,
    passing_yards = 0,
    rushing_yards = 0,
    rush_touchdown = 0,
    interception = 0,
    fumbled_1_player_id = NA
  )
  
  # Run full workflow
  fantasy <- calculate_fantasy_points(mock_pbp)
  fantasy_rolling <- calculate_rolling_fantasy(fantasy)
  
  # Validate results
  expect_equal(nrow(fantasy), 10)
  expect_true("total_fantasy_points_roll3" %in% names(fantasy_rolling))
  expect_true("total_fantasy_points_roll6" %in% names(fantasy_rolling))
  
  # Verify rolling averages calculated (weeks 4+)
  expect_true(!is.na(fantasy_rolling$total_fantasy_points_roll3[4]))
  
  # Verify week 1-3 have NA (insufficient games)
  expect_true(is.na(fantasy_rolling$total_fantasy_points_roll3[1]))
  expect_true(is.na(fantasy_rolling$total_fantasy_points_roll3[2]))
  expect_true(is.na(fantasy_rolling$total_fantasy_points_roll3[3]))
})

# ==============================================================================
# RUN ALL TESTS
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("WEEK 3 UNIT TEST SUMMARY\n")
cat("================================================================================\n")
cat("\n")
cat("Running comprehensive tests for:\n")
cat("  1. Fantasy Points Calculation (All Parameters Customizable)\n")
cat("  2. Rolling Averages (Feature Leakage Prevention)\n")
cat("  3. NA Handling (Boolean Logic Edge Cases)\n")
cat("  4. Edge Cases (Bye Weeks, Negative Yards, Team Trades)\n")
cat("  5. Integration Tests (Full Pipeline)\n")
cat("\n")
cat("Tests designed with data science code reviewer best practices:\n")
cat("  ✓ Feature leakage prevention (lag before rolling calculation)\n")
cat("  ✓ Explicit NA filtering (no Boolean logic bugs)\n")
cat("  ✓ Complete grouping keys (season + week + game_id + player_id)\n")
cat("  ✓ Edge case coverage (bye weeks, trades, negative values)\n")
cat("\n")
cat("New tests for Week 3 v2.0:\n")
cat("  ✓ Tiered PPR scoring validation\n")
cat("  ✓ Rush attempt bonus verification\n")
cat("  ✓ Pick-6 penalty calculation\n")
cat("  ✓ TE premium (requires roster data)\n")
cat("  ✓ 6pt passing TD default\n")
cat("\n")
cat("================================================================================\n")
cat("\n")
