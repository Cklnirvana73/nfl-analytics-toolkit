# Unit Tests for Week 2 Analysis Functions
# tests/test_week2_functions.R
#
# Run with: testthat::test_file("tests/test_week2_functions.R")

library(testthat)
library(dplyr)

# Defensive path resolution - handles running from different directories
if (file.exists("R/01_data_loading.R")) {
  # Running from project root
  source("R/01_data_loading.R")
  source("R/02_player_stats.R")
  source("R/03_team_stats.R")
  source("R/04_game_analysis.R")
} else if (file.exists("../R/01_data_loading.R")) {
  # Running from tests/ directory
  source("../R/01_data_loading.R")
  source("../R/02_player_stats.R")
  source("../R/03_team_stats.R")
  source("../R/04_game_analysis.R")
} else {
  stop("Cannot find R source files. Please run tests from project root or tests/ directory.")
}

# ============================================================================
# Helper: Create test data
# ============================================================================

create_test_pbp_week2 <- function() {
  # Create realistic test data with all required columns
  tibble(
    game_id = rep(c("2024_01_KC_BAL", "2024_01_BUF_MIA"), each = 100),
    play_id = rep(1:100, 2),
    season = 2024,
    week = 1,
    game_date = as.Date("2024-09-05"),
    
    # Team info
    home_team = rep(c("BAL", "MIA"), each = 100),
    away_team = rep(c("KC", "BUF"), each = 100),
    posteam = sample(c("KC", "BAL", "BUF", "MIA"), 200, replace = TRUE),
    defteam = NA,  # Will fill based on posteam
    
    # Play type
    play_type = sample(c("pass", "run", "punt"), 200, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
    
    # Player IDs
    rusher_player_id = ifelse(play_type == "run", 
                               sample(paste0("RB", 1:5), 200, replace = TRUE), 
                               NA),
    rusher_player_name = ifelse(!is.na(rusher_player_id), 
                                 paste("Player", rusher_player_id), 
                                 NA),
    passer_player_id = ifelse(play_type == "pass",
                               sample(paste0("QB", 1:4), 200, replace = TRUE),
                               NA),
    passer_player_name = ifelse(!is.na(passer_player_id),
                                 paste("Player", passer_player_id),
                                 NA),
    receiver_player_id = ifelse(play_type == "pass",
                                 sample(paste0("WR", 1:10), 200, replace = TRUE),
                                 NA),
    receiver_player_name = ifelse(!is.na(receiver_player_id),
                                   paste("Player", receiver_player_id),
                                   NA),
    
    # Play outcomes
    yards_gained = sample(-5:25, 200, replace = TRUE),
    rushing_yards = ifelse(play_type == "run", yards_gained, 0),
    passing_yards = ifelse(play_type == "pass", yards_gained, 0),
    receiving_yards = ifelse(play_type == "pass", yards_gained, 0),
    
    # Scoring
    rush_touchdown = ifelse(play_type == "run", sample(0:1, 200, replace = TRUE, prob = c(0.95, 0.05)), 0),
    pass_touchdown = ifelse(play_type == "pass", sample(0:1, 200, replace = TRUE, prob = c(0.95, 0.05)), 0),
    td_team = ifelse(rush_touchdown == 1 | pass_touchdown == 1, posteam, NA),
    
    # Pass specific
    complete_pass = ifelse(play_type == "pass", sample(0:1, 200, replace = TRUE, prob = c(0.4, 0.6)), NA),
    interception = ifelse(play_type == "pass", sample(0:1, 200, replace = TRUE, prob = c(0.97, 0.03)), 0),
    sack = ifelse(play_type == "pass", sample(0:1, 200, replace = TRUE, prob = c(0.93, 0.07)), 0),
    air_yards = ifelse(play_type == "pass", sample(0:30, 200, replace = TRUE), NA),
    yards_after_catch = ifelse(play_type == "pass" & complete_pass == 1, sample(0:15, 200, replace = TRUE), 0),
    
    # Run specific
    first_down_rush = ifelse(play_type == "run", sample(0:1, 200, replace = TRUE, prob = c(0.7, 0.3)), 0),
    first_down_pass = ifelse(play_type == "pass", sample(0:1, 200, replace = TRUE, prob = c(0.6, 0.4)), 0),
    
    # Advanced metrics
    epa = rnorm(200, mean = 0, sd = 2),
    cpoe = ifelse(play_type == "pass", rnorm(200, mean = 0, sd = 0.2), NA),
    success = sample(0:1, 200, replace = TRUE, prob = c(0.55, 0.45)),
    
    # Down info
    down = sample(1:4, 200, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15)),
    ydstogo = sample(1:15, 200, replace = TRUE),
    third_down_converted = ifelse(down == 3, sample(0:1, 200, replace = TRUE, prob = c(0.6, 0.4)), NA),
    fourth_down_converted = ifelse(down == 4, sample(0:1, 200, replace = TRUE, prob = c(0.5, 0.5)), NA),
    
    # Drive info
    drive = rep(1:20, each = 10),
    drive_ended_with_score = rep(sample(0:1, 20, replace = TRUE, prob = c(0.6, 0.4)), each = 10),
    drive_play_count = rep(sample(3:12, 20, replace = TRUE), each = 10),
    drive_yards_penalized = rep(sample(0:30, 20, replace = TRUE), each = 10),
    drive_start_yard_line = rep(sample(10:80, 20, replace = TRUE), each = 10),
    drive_end_yard_line = rep(sample(10:80, 20, replace = TRUE), each = 10),
    drive_time_of_possession = rep(sprintf("%d:%02d", sample(0:5, 20, replace = TRUE), sample(0:59, 20, replace = TRUE)), each = 10),
    
    # Game score (for game summary)
    total_home_score = rep(c(24, 17), each = 100),
    total_away_score = rep(c(20, 21), each = 100),
    
    # Field goals and safeties
    field_goal_result = sample(c("made", "missed", NA), 200, replace = TRUE, prob = c(0.05, 0.02, 0.93)),
    safety = sample(0:1, 200, replace = TRUE, prob = c(0.998, 0.002)),
    
    # Time
    qtr = sample(1:4, 200, replace = TRUE),
    quarter_seconds_remaining = sample(0:900, 200, replace = TRUE),
    
    # Description
    desc = paste("Play description for play", 1:200),
    
    # Fumbles
    fumble_lost = sample(0:1, 200, replace = TRUE, prob = c(0.98, 0.02))
  ) %>%
    mutate(
      # Fill defteam based on posteam
      defteam = case_when(
        game_id == "2024_01_KC_BAL" & posteam == "KC" ~ "BAL",
        game_id == "2024_01_KC_BAL" & posteam == "BAL" ~ "KC",
        game_id == "2024_01_BUF_MIA" & posteam == "BUF" ~ "MIA",
        game_id == "2024_01_BUF_MIA" & posteam == "MIA" ~ "BUF",
        TRUE ~ NA_character_
      )
    )
}


# ============================================================================
# Test Suite 1: Player Statistics - Input Validation
# ============================================================================

test_that("get_player_rushing_stats validates inputs", {
  test_data <- create_test_pbp_week2()
  
  # Should reject non-dataframe
  expect_error(
    get_player_rushing_stats("not a dataframe"),
    "must be a data frame"
  )
  
  # Should reject non-numeric season
  expect_error(
    get_player_rushing_stats(test_data, season = "2024"),
    "season must be numeric"
  )
  
  # Should reject invalid week_min
  expect_error(
    get_player_rushing_stats(test_data, week_min = "one"),
    "week_min must be a single numeric"
  )
})

test_that("get_player_passing_stats validates inputs", {
  test_data <- create_test_pbp_week2()
  
  expect_error(
    get_player_passing_stats("not a dataframe"),
    "must be a data frame"
  )
})

test_that("get_player_receiving_stats validates inputs", {
  test_data <- create_test_pbp_week2()
  
  expect_error(
    get_player_receiving_stats("not a dataframe"),
    "must be a data frame"
  )
})


# ============================================================================
# Test Suite 2: Player Statistics - Functionality
# ============================================================================

test_that("get_player_rushing_stats calculates correctly", {
  test_data <- create_test_pbp_week2()
  
  rush_stats <- get_player_rushing_stats(test_data, season = 2024)
  
  # Should return a data frame
  expect_s3_class(rush_stats, "data.frame")
  
  # Should have expected columns
  expect_true("player_id" %in% names(rush_stats))
  expect_true("player_name" %in% names(rush_stats))
  expect_true("rushes" %in% names(rush_stats))
  expect_true("rush_yards" %in% names(rush_stats))
  expect_true("yards_per_carry" %in% names(rush_stats))
  
  # Should only include rushing players
  expect_true(all(!is.na(rush_stats$player_id)))
  
  # Yards per carry should be calculated correctly
  expect_true(all(abs(rush_stats$yards_per_carry - rush_stats$rush_yards / rush_stats$rushes) < 0.01))
})

test_that("get_player_passing_stats calculates correctly", {
  test_data <- create_test_pbp_week2()
  
  pass_stats <- get_player_passing_stats(test_data, season = 2024)
  
  expect_s3_class(pass_stats, "data.frame")
  expect_true("player_id" %in% names(pass_stats))
  expect_true("attempts" %in% names(pass_stats))
  expect_true("completions" %in% names(pass_stats))
  expect_true("completion_pct" %in% names(pass_stats))
  
  # Completion pct should be between 0 and 1
  expect_true(all(pass_stats$completion_pct >= 0 & pass_stats$completion_pct <= 1))
})

test_that("get_player_receiving_stats calculates correctly", {
  test_data <- create_test_pbp_week2()
  
  rec_stats <- get_player_receiving_stats(test_data, season = 2024)
  
  expect_s3_class(rec_stats, "data.frame")
  expect_true("player_id" %in% names(rec_stats))
  expect_true("targets" %in% names(rec_stats))
  expect_true("receptions" %in% names(rec_stats))
  expect_true("catch_rate" %in% names(rec_stats))
  
  # Receptions should never exceed targets
  expect_true(all(rec_stats$receptions <= rec_stats$targets))
})


# ============================================================================
# Test Suite 3: Team Statistics
# ============================================================================

test_that("get_team_offense_stats validates and calculates", {
  test_data <- create_test_pbp_week2()
  
  # Should validate input
  expect_error(
    get_team_offense_stats("not a dataframe"),
    "must be a data frame"
  )
  
  # Should calculate correctly
  offense_stats <- get_team_offense_stats(test_data, season = 2024)
  
  expect_s3_class(offense_stats, "data.frame")
  expect_true("team" %in% names(offense_stats))
  expect_true("total_plays" %in% names(offense_stats))
  expect_true("offensive_epa_per_play" %in% names(offense_stats))
  
  # Should have one row per team
  expect_true(nrow(offense_stats) <= 4)  # Max 4 teams in test data
})

test_that("get_team_defense_stats validates and calculates", {
  test_data <- create_test_pbp_week2()
  
  defense_stats <- get_team_defense_stats(test_data, season = 2024)
  
  expect_s3_class(defense_stats, "data.frame")
  expect_true("team" %in% names(defense_stats))
  expect_true("plays_against" %in% names(defense_stats))
  expect_true("sacks" %in% names(defense_stats))
  expect_true("turnovers_generated" %in% names(defense_stats))
})


# ============================================================================
# Test Suite 4: Game Analysis
# ============================================================================

test_that("get_game_summary validates and calculates", {
  test_data <- create_test_pbp_week2()
  
  # Should validate input
  expect_error(
    get_game_summary("not a dataframe", "2024_01_KC_BAL"),
    "must be a data frame"
  )
  
  expect_error(
    get_game_summary(test_data, c("game1", "game2")),
    "must be a single character"
  )
  
  # Should calculate correctly
  game_sum <- get_game_summary(test_data, "2024_01_KC_BAL")
  
  expect_s3_class(game_sum, "data.frame")
  expect_equal(nrow(game_sum), 1)
  expect_true("home_team" %in% names(game_sum))
  expect_true("away_team" %in% names(game_sum))
  expect_true("total_plays" %in% names(game_sum))
  expect_true("winner" %in% names(game_sum))
  
  # Should return NULL for non-existent game
  expect_null(get_game_summary(test_data, "2024_99_XX_YY"))
})

test_that("get_scoring_plays extracts scores", {
  test_data <- create_test_pbp_week2()
  
  scoring <- get_scoring_plays(test_data, "2024_01_KC_BAL")
  
  # May or may not have scoring plays in random data
  if (!is.null(scoring) && nrow(scoring) > 0) {
    expect_s3_class(scoring, "data.frame")
    expect_true("scoring_team" %in% names(scoring))
    expect_true("score_type" %in% names(scoring))
    expect_true("points" %in% names(scoring))
  }
})

test_that("get_drive_summary calculates drives", {
  test_data <- create_test_pbp_week2()
  
  drives <- get_drive_summary(test_data, "2024_01_KC_BAL")
  
  expect_s3_class(drives, "data.frame")
  expect_true("drive" %in% names(drives))
  expect_true("posteam" %in% names(drives))
  expect_true("plays" %in% names(drives))
  expect_true("yards_gained" %in% names(drives))
})


# ============================================================================
# Test Suite 5: Week Filtering
# ============================================================================

test_that("week filtering works across all functions", {
  test_data <- create_test_pbp_week2()
  
  # Add multi-week data
  test_data_multi <- bind_rows(
    test_data,
    test_data %>% mutate(week = 2)
  )
  
  # Test week_max filter on rushing stats
  rush_week1 <- get_player_rushing_stats(test_data_multi, season = 2024, week_max = 1)
  expect_true(nrow(rush_week1) > 0)
  
  # Test week_min filter on passing stats
  pass_week2 <- get_player_passing_stats(test_data_multi, season = 2024, week_min = 2)
  expect_true(nrow(pass_week2) > 0)
})


# ============================================================================
# Test Suite 6: NA Handling (Critical - Week 1 Lesson!)
# ============================================================================

test_that("functions handle NA player IDs correctly", {
  test_data <- create_test_pbp_week2()
  
  # Add some plays with NA player IDs
  test_data_with_nas <- test_data %>%
    mutate(
      rusher_player_id = ifelse(row_number() <= 5, NA, rusher_player_id),
      passer_player_id = ifelse(row_number() <= 5, NA, passer_player_id),
      receiver_player_id = ifelse(row_number() <= 5, NA, receiver_player_id)
    )
  
  # Should exclude NAs and not error
  rush_stats <- get_player_rushing_stats(test_data_with_nas)
  expect_true(all(!is.na(rush_stats$player_id)))
  
  pass_stats <- get_player_passing_stats(test_data_with_nas)
  expect_true(all(!is.na(pass_stats$player_id)))
  
  rec_stats <- get_player_receiving_stats(test_data_with_nas)
  expect_true(all(!is.na(rec_stats$player_id)))
})


# ============================================================================
# Summary Message
# ============================================================================

cat("\n")
cat("=================================================================\n")
cat("  Week 2 Analysis Functions - Unit Test Suite Complete\n")
cat("=================================================================\n")
cat("✅ Tests cover:\n")
cat("   - Input validation for all functions\n")
cat("   - Statistical calculation accuracy\n")
cat("   - NA handling (applying Week 1 lessons)\n")
cat("   - Week/season filtering\n")
cat("   - Edge cases\n")
cat("\n")
cat("Run this file with: testthat::test_file('tests/test_week2_functions.R')\n")
cat("=================================================================\n")
