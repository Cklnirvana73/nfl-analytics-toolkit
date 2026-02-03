# Unit Tests for Data Loading Functions
# tests/test_data_loading.R
#
# Run with: testthat::test_file("tests/test_data_loading.R")
# Or: devtools::test()

library(testthat)
library(dplyr)

# Source the functions
# Load here package for robust path resolution
if (!requireNamespace("here", quietly = TRUE)) {
  message("Installing 'here' package for robust path resolution...")
  install.packages("here")
}
library(here)

# Source the functions using here::here() for robust paths
source(here("R", "01_data_loading.R"))

# ============================================================================
# Test Suite 1: Input Validation
# ============================================================================

test_that("load_and_validate_pbp validates season range", {
  # Should reject seasons before 1999
  expect_error(
    load_and_validate_pbp(1998),
    "between 1999 and"
  )
  
  # Should reject future seasons beyond current year
  future_year <- as.numeric(format(Sys.Date(), "%Y")) + 2
  expect_error(
    load_and_validate_pbp(future_year),
    "between 1999 and"
  )
  
  # Should reject non-numeric input
  expect_error(
    load_and_validate_pbp("2023"),
    "seasons must be numeric"
  )
})

test_that("get_roster_data validates input", {
  # Should reject non-numeric input
  expect_error(
    get_roster_data("2023"),
    "seasons must be numeric"
  )
})


# ============================================================================
# Test Suite 2: Validation Function - Critical Issues
# ============================================================================

test_that("validate_pbp_quality handles NA play_ids correctly (CRITICAL FIX #1)", {
  # This tests the duplicate detection bug fix
  test_pbp <- data.frame(
    game_id = c("2023_01_BUF_NYJ", "2023_01_BUF_NYJ", "2023_01_BUF_NYJ", "2023_01_BUF_NYJ"),
    play_id = c(1, 1, NA, NA),  # Two real duplicates, two NAs
    season = 2023,
    week = 1,
    game_date = as.Date("2023-09-10"),
    posteam = "BUF",
    defteam = "NYJ",
    desc = "test play",
    play_type = "pass"
  )
  
  results <- validate_pbp_quality(test_pbp)
  
  # Should only count the real duplicate (play_id = 1), not the NAs
  expect_equal(results$duplicate_plays, 1)
  
  # Should NOT group NA play_ids together as duplicates
  expect_true(results$duplicate_plays < 2)
})

test_that("validate_pbp_quality detects season completeness (CRITICAL FIX #2)", {
  # Create test data with incomplete current season
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  test_pbp <- data.frame(
    game_id = paste0(current_year, "_01_BUF_NYJ"),
    play_id = 1:100,
    season = current_year,
    week = 5,  # Only week 5, not complete season
    game_date = as.Date(paste0(current_year, "-10-01")),
    posteam = "BUF",
    defteam = "NYJ",
    desc = "test",
    play_type = "pass"
  )
  
  # Capture messages
  expect_message(
    results <- validate_pbp_quality(test_pbp),
    "incomplete"
  )
  
  # Should flag incomplete season
  expect_true("incomplete_seasons" %in% names(results))
  expect_equal(results$incomplete_seasons$season, current_year)
  expect_equal(results$incomplete_seasons$weeks_available, 5)
})

test_that("validate_pbp_quality detects low/high play count games", {
  # Create test data with abnormal game lengths
  test_pbp <- rbind(
    # Normal game
    data.frame(
      game_id = "2023_01_BUF_NYJ",
      play_id = 1:150,
      season = 2023,
      week = 1,
      game_date = as.Date("2023-09-10"),
      posteam = "BUF",
      defteam = "NYJ",
      desc = "normal game",
      play_type = "pass"
    ),
    # Suspiciously low play count (cancelled game?)
    data.frame(
      game_id = "2023_01_KC_DET",
      play_id = 1:50,  # Only 50 plays
      season = 2023,
      week = 1,
      game_date = as.Date("2023-09-10"),
      posteam = "KC",
      defteam = "DET",
      desc = "low play game",
      play_type = "pass"
    ),
    # Overtime game
    data.frame(
      game_id = "2023_01_SF_DAL",
      play_id = 1:220,  # 220 plays (OT)
      season = 2023,
      week = 1,
      game_date = as.Date("2023-09-10"),
      posteam = "SF",
      defteam = "DAL",
      desc = "OT game",
      play_type = "pass"
    )
  )
  
  expect_message(
    results <- validate_pbp_quality(test_pbp),
    "100 plays"
  )
  
  expect_true("low_play_games" %in% names(results))
  expect_true("high_play_games" %in% names(results))
  expect_equal(nrow(results$low_play_games), 1)
  expect_equal(nrow(results$high_play_games), 1)
})


# ============================================================================
# Test Suite 3: Data Quality Checks
# ============================================================================

test_that("validate_pbp_quality detects missing required columns", {
  # Create incomplete test data
  incomplete_pbp <- data.frame(
    game_id = "2023_01_BUF_NYJ",
    season = 2023
    # Missing: play_id, posteam, week, game_date, desc, play_type
  )
  
  expect_warning(
    results <- validate_pbp_quality(incomplete_pbp),
    "Missing required columns"
  )
  
  expect_true(results$has_critical_issues)
  expect_true(length(results$missing_columns) > 0)
})

test_that("validate_pbp_quality detects high NA rates in key columns", {
  # Create data with excessive NAs
  test_pbp <- data.frame(
    game_id = rep("2023_01_BUF_NYJ", 100),
    play_id = 1:100,
    season = 2023,
    week = 1,
    game_date = as.Date("2023-09-10"),
    posteam = rep(NA, 100),  # 100% NA
    defteam = "NYJ",
    desc = "test",
    play_type = "pass"
  )
  
  expect_warning(
    results <- validate_pbp_quality(test_pbp),
    ">50% missing"
  )
  
  expect_true(results$has_critical_issues)
  expect_true("posteam" %in% results$high_na_columns)
})

test_that("validate_pbp_quality detects low season play counts", {
  # Create data with suspiciously few plays
  test_pbp <- data.frame(
    game_id = "2023_01_BUF_NYJ",
    play_id = 1:100,  # Only 100 plays for entire season
    season = 2023,
    week = 1,
    game_date = as.Date("2023-09-10"),
    posteam = "BUF",
    defteam = "NYJ",
    desc = "test",
    play_type = "pass"
  )
  
  expect_warning(
    results <- validate_pbp_quality(test_pbp),
    "unexpectedly low play counts"
  )
  
  expect_true(results$has_critical_issues)
  expect_true("low_play_count" %in% names(results))
})

test_that("validate_pbp_quality detects invalid dates", {
  # Create data with pre-1999 dates
  test_pbp <- data.frame(
    game_id = "1998_01_BUF_NYJ",
    play_id = 1:100,
    season = 1998,
    week = 1,
    game_date = as.Date("1998-09-10"),  # Before nflfastR era
    posteam = "BUF",
    defteam = "NYJ",
    desc = "test",
    play_type = "pass"
  )
  
  expect_warning(
    results <- validate_pbp_quality(test_pbp),
    "before 1999"
  )
  
  expect_true(results$has_critical_issues)
  expect_true(results$invalid_dates)
})


# ============================================================================
# Test Suite 4: Helper Functions
# ============================================================================

test_that("create_test_pbp generates valid structure", {
  test_data <- create_test_pbp(n_games = 2, n_plays_per_game = 100)
  
  # Check dimensions
  expect_equal(nrow(test_data), 200)  # 2 games × 100 plays
  
  # Check required columns exist
  required_cols <- c("game_id", "play_id", "season", "week", "game_date",
                     "posteam", "defteam", "play_type", "desc")
  expect_true(all(required_cols %in% names(test_data)))
  
  # Check data types
  expect_type(test_data$season, "double")
  expect_type(test_data$week, "double")
  expect_s3_class(test_data$game_date, "Date")
  
  # Validation should pass on generated data
  results <- validate_pbp_quality(test_data)
  expect_false(results$has_critical_issues)
})

test_that("create_test_pbp varies play types realistically", {
  test_data <- create_test_pbp(n_games = 1, n_plays_per_game = 1000)
  
  play_type_dist <- table(test_data$play_type) / nrow(test_data)
  
  # Pass plays should be most common (around 40%)
  expect_true(play_type_dist["pass"] > 0.35)
  expect_true(play_type_dist["pass"] < 0.45)
  
  # Run plays should be second (around 30%)
  expect_true(play_type_dist["run"] > 0.25)
  expect_true(play_type_dist["run"] < 0.35)
})


# ============================================================================
# Test Suite 5: Integration with Real Structure
# ============================================================================

test_that("validation results can be accessed as attributes", {
  test_data <- create_test_pbp(n_games = 1, n_plays_per_game = 100)
  
  # Mock the load_and_validate_pbp behavior
  attr(test_data, "validation_results") <- validate_pbp_quality(test_data)
  
  # Should be able to extract validation results
  validation <- attr(test_data, "validation_results")
  
  expect_true("summary" %in% names(validation))
  expect_true("has_critical_issues" %in% names(validation))
})


# ============================================================================
# Test Suite 6: Edge Cases
# ============================================================================

test_that("functions handle empty data gracefully", {
  empty_pbp <- data.frame(
    game_id = character(0),
    play_id = numeric(0),
    season = numeric(0),
    week = numeric(0),
    game_date = as.Date(character(0)),
    posteam = character(0),
    defteam = character(0),
    desc = character(0),
    play_type = character(0)
  )
  
  # Should not crash, but should flag issues
  results <- validate_pbp_quality(empty_pbp)
  expect_true(results$has_critical_issues)
})

test_that("validation handles all NA columns", {
  test_pbp <- data.frame(
    game_id = rep(NA_character_, 100),
    play_id = rep(NA_real_, 100),
    season = 2023,
    week = 1,
    game_date = as.Date("2023-09-10"),
    posteam = rep(NA_character_, 100),
    defteam = "NYJ",
    desc = "test",
    play_type = "pass"
  )
  
  expect_warning(
    results <- validate_pbp_quality(test_pbp),
    "50% missing"
  )
  
  expect_true(results$has_critical_issues)
})


# ============================================================================
# Summary Message
# ============================================================================

cat("\n")
cat("=================================================================\n")
cat("  Week 1 Data Loading - Unit Test Suite Complete\n")
cat("=================================================================\n")
cat("✅ Tests cover:\n")
cat("   - Input validation\n")
cat("   - Critical bug fixes (NA handling, season completeness)\n")
cat("   - Data quality checks\n")
cat("   - Edge cases and error handling\n")
cat("\n")
cat("Run this file with: testthat::test_file('tests/test_data_loading.R')\n")
cat("=================================================================\n")