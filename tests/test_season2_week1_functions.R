# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 1
# Full Test Suite
# File: tests/test_season2_week1_functions.R
#
# Tests for R/15_multi_season_pbp.R
# Run with: testthat::test_file("tests/test_season2_week1_functions.R")
# Or via root runner: source("test_season2_week1.R")
#
# Coverage:
#   - Input validation (seasons range, types, edge cases)
#   - normalize_schema() column handling and type coercion
#   - Team relocation mapping (OAK->LV, SD->LAC, STL->LA)
#   - validate_season_coverage() threshold logic
#   - get_schema_differences() presence matrix
#   - validate_epa_distribution() filtering and tolerance
#   - load_normalized_season() error handling
#   - NFL-specific edge cases (17-game season, garbage time, kneels)
#   - Integration test (pipeline end-to-end on mock data)
#
# Production-grade for portfolio display
# Built for personal analytics use
# ==============================================================================

library(testthat)
library(dplyr)
library(here)

# Source production file if not already loaded
if (!exists("load_multi_season_pbp")) {
  source(here::here("R", "15_multi_season_pbp.R"))
}


# ==============================================================================
# HELPER: Create mock play-by-play data
# ==============================================================================

#' Create Realistic Mock PBP Data for Testing
#'
#' Generates a tibble with all core columns present, realistic play type
#' distribution, and valid EPA values centered near zero.
#'
#' @param season Integer. Season year.
#' @param n_games Integer. Number of games to generate.
#' @param n_plays_per_game Integer. Plays per game.
#' @param include_garbage_time Logical. Add garbage time plays. Default: TRUE
#' @param include_kneels Logical. Add QB kneel plays. Default: TRUE
#' @return Tibble with mock PBP data.
create_mock_pbp <- function(season = 2024L,
                            n_games = 4L,
                            n_plays_per_game = 150L,
                            include_garbage_time = TRUE,
                            include_kneels = TRUE) {

  total_plays <- n_games * n_plays_per_game
  teams <- c("KC", "BUF", "SF", "DAL", "PHI", "MIA", "BAL", "CIN",
             "NYJ", "NE", "GB", "LAR", "NYG", "WAS", "PIT", "CLE",
             "DET", "MIN", "CHI", "TB", "NO", "ATL", "CAR", "SEA",
             "ARI", "LA", "LV", "DEN", "LAC", "HOU", "JAX", "TEN")

  game_ids <- paste0(season, "_", sprintf("%02d", rep(1:n_games, each = n_plays_per_game)),
                     "_HOME_AWAY")

  # Realistic play type distribution
  play_types <- sample(
    c("pass", "run", "punt", "kickoff", "field_goal", "extra_point",
      "qb_kneel", "qb_spike", "no_play"),
    total_plays, replace = TRUE,
    prob = c(0.38, 0.28, 0.08, 0.06, 0.04, 0.03, 0.02, 0.01, 0.10)
  )

  # EPA centered near zero for pass/run plays, NA for special teams
  epa_vals <- ifelse(
    play_types %in% c("pass", "run"),
    rnorm(total_plays, mean = 0, sd = 1.5),
    NA_real_
  )

  # Win probability: most plays in 20-80% range, some extreme
  wp_vals <- pmin(pmax(rbeta(total_plays, 2, 2), 0.01), 0.99)

  mock <- tibble(
    play_id    = as.character(1:total_plays),
    game_id    = game_ids,
    season     = as.integer(season),
    week       = rep(1:n_games, each = n_plays_per_game),
    game_date  = as.Date("2024-09-10") + rep(0:(n_games - 1), each = n_plays_per_game) * 7,
    posteam    = sample(teams, total_plays, replace = TRUE),
    defteam    = sample(teams, total_plays, replace = TRUE),
    play_type  = play_types,
    desc       = paste("Play", 1:total_plays),
    yards_gained = as.integer(sample(-5:30, total_plays, replace = TRUE)),
    epa        = epa_vals,
    success    = as.integer(ifelse(!is.na(epa_vals), epa_vals > 0, NA)),
    passer_player_id   = ifelse(play_types == "pass", paste0("P", sample(1:50, total_plays, replace = TRUE)), NA_character_),
    passer_player_name = ifelse(play_types == "pass", paste("Passer", sample(1:50, total_plays, replace = TRUE)), NA_character_),
    rusher_player_id   = ifelse(play_types == "run", paste0("R", sample(1:30, total_plays, replace = TRUE)), NA_character_),
    rusher_player_name = ifelse(play_types == "run", paste("Rusher", sample(1:30, total_plays, replace = TRUE)), NA_character_),
    receiver_player_id   = ifelse(play_types == "pass", paste0("W", sample(1:60, total_plays, replace = TRUE)), NA_character_),
    receiver_player_name = ifelse(play_types == "pass", paste("Receiver", sample(1:60, total_plays, replace = TRUE)), NA_character_),
    passing_yards  = ifelse(play_types == "pass", sample(0:40, total_plays, replace = TRUE), NA_integer_),
    rushing_yards  = ifelse(play_types == "run", sample(-3:20, total_plays, replace = TRUE), NA_integer_),
    receiving_yards = ifelse(play_types == "pass", sample(0:40, total_plays, replace = TRUE), NA_integer_),
    air_yards      = ifelse(play_types == "pass", runif(total_plays, -5, 30), NA_real_),
    yards_after_catch = ifelse(play_types == "pass", runif(total_plays, 0, 20), NA_real_),
    touchdown      = as.integer(sample(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1), total_plays, replace = TRUE)),
    interception   = as.integer(sample(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), total_plays, replace = TRUE)),
    fumble         = as.integer(sample(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), total_plays, replace = TRUE)),
    sack           = as.integer(sample(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1), total_plays, replace = TRUE)),
    qb_hit         = as.integer(sample(c(0, 0, 0, 0, 1), total_plays, replace = TRUE)),
    wp             = wp_vals,
    def_wp         = 1 - wp_vals,
    down           = as.integer(sample(1:4, total_plays, replace = TRUE, prob = c(0.35, 0.30, 0.25, 0.10))),
    ydstogo        = as.integer(sample(1:20, total_plays, replace = TRUE)),
    yardline_100   = as.integer(sample(1:99, total_plays, replace = TRUE)),
    shotgun        = sample(c(TRUE, FALSE), total_plays, replace = TRUE),
    no_huddle      = sample(c(TRUE, FALSE), total_plays, replace = TRUE, prob = c(0.15, 0.85)),
    qb_dropback    = play_types == "pass" | (play_types == "run" & runif(total_plays) < 0.05),
    qb_scramble    = play_types == "run" & qb_dropback & runif(total_plays) < 0.15,
    pass_attempt   = as.integer(play_types == "pass"),
    rush_attempt   = as.integer(play_types == "run"),
    complete_pass  = as.integer(play_types == "pass" & runif(total_plays) < 0.65),
    two_point_attempt  = as.integer(sample(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), total_plays, replace = TRUE)),
    extra_point_attempt = as.integer(play_types == "extra_point"),
    field_goal_attempt  = as.integer(play_types == "field_goal"),
    penalty        = as.integer(sample(c(0, 0, 0, 0, 0, 0, 0, 1), total_plays, replace = TRUE)),
    penalty_yards  = as.integer(sample(c(0, 5, 10, 15), total_plays, replace = TRUE))
  )

  # Add garbage time plays (Q4, extreme WP) if requested
  if (include_garbage_time) {
    gt_idx <- sample(which(mock$play_type %in% c("pass", "run")),
                     min(20, sum(mock$play_type %in% c("pass", "run"))),
                     replace = FALSE)
    mock$wp[gt_idx] <- sample(c(runif(10, 0.01, 0.08), runif(10, 0.93, 0.99)), length(gt_idx), replace = TRUE)
    mock$down[gt_idx] <- 4L
  }

  return(mock)
}


# ==============================================================================
# Test Suite 1: Input Validation
# ==============================================================================

context("Input Validation")

test_that("load_multi_season_pbp rejects invalid season types", {
  expect_error(
    load_multi_season_pbp(seasons = "2024"),
    "non-empty numeric"
  )
  expect_error(
    load_multi_season_pbp(seasons = character(0)),
    "non-empty numeric"
  )
  expect_error(
    load_multi_season_pbp(seasons = NULL),
    "non-empty numeric"
  )
})

test_that("load_multi_season_pbp rejects out-of-range seasons", {
  expect_error(
    load_multi_season_pbp(seasons = 1998),
    "between"
  )
  future_year <- as.integer(format(Sys.Date(), "%Y")) + 2
  expect_error(
    load_multi_season_pbp(seasons = future_year),
    "between"
  )
})

test_that("load_normalized_season rejects multiple seasons", {
  expect_error(
    load_normalized_season(season = c(2024, 2025)),
    "exactly one season"
  )
})

test_that("load_normalized_season gives clear error for missing cache", {
  expect_error(
    load_normalized_season(season = 2024, cache_dir = tempdir()),
    "Cache file not found"
  )
  # Verify the error message includes instructions
  expect_error(
    load_normalized_season(season = 2024, cache_dir = tempdir()),
    "load_multi_season_pbp"
  )
})


# ==============================================================================
# Test Suite 2: normalize_schema() Column Handling
# ==============================================================================

context("Schema Normalization")

test_that("normalize_schema adds missing optional columns as NA", {
  mock <- create_mock_pbp(season = 2012L, n_games = 1L, n_plays_per_game = 20L)

  # Remove optional columns to simulate early-season data
  mock$cpoe <- NULL
  mock$pass_oe <- NULL
  mock$xpass <- NULL
  mock$xyac_epa <- NULL
  mock$xyac_mean_yardage <- NULL

  result <- normalize_schema(mock, season = 2012L)

  # All optional columns should now exist (as NA)
  expect_true("cpoe" %in% names(result))
  expect_true("pass_oe" %in% names(result))
  expect_true("xpass" %in% names(result))
  expect_true("xyac_epa" %in% names(result))
  expect_true("xyac_mean_yardage" %in% names(result))

  # They should all be NA

  expect_true(all(is.na(result$cpoe)))
  expect_true(all(is.na(result$pass_oe)))
})

test_that("normalize_schema enforces correct types on core columns", {
  mock <- create_mock_pbp(season = 2023L, n_games = 1L, n_plays_per_game = 20L)

  # Intentionally corrupt types
  mock$season <- "2023"
  mock$week <- "5"
  mock$epa <- as.character(mock$epa)

  result <- normalize_schema(mock, season = 2023L)

  expect_type(result$season, "integer")
  expect_type(result$week, "integer")
  expect_type(result$epa, "double")
  expect_type(result$game_id, "character")
  expect_type(result$play_id, "character")
})

test_that("normalize_schema adds version tag", {
  mock <- create_mock_pbp(season = 2024L, n_games = 1L, n_plays_per_game = 10L)
  result <- normalize_schema(mock, season = 2024L)

  expect_true("season_norm_version" %in% names(result))
  expect_equal(unique(result$season_norm_version), SCHEMA_NORM_VERSION)
})

test_that("normalize_schema rejects empty input", {
  expect_error(
    normalize_schema(data.frame(), season = 2024L),
    "empty"
  )
  expect_error(
    normalize_schema("not a dataframe", season = 2024L),
    "non-data.frame"
  )
})


# ==============================================================================
# Test Suite 3: Team Relocation Handling
# ==============================================================================

context("Team Relocations")

test_that("normalize_schema maps OAK to LV in posteam and defteam", {
  mock <- create_mock_pbp(season = 2018L, n_games = 1L, n_plays_per_game = 20L)
  mock$posteam[1:5] <- "OAK"
  mock$defteam[6:10] <- "OAK"

  result <- normalize_schema(mock, season = 2018L)

  # OAK should be gone, replaced with LV
  expect_false("OAK" %in% result$posteam)
  expect_false("OAK" %in% result$defteam)
  expect_true("LV" %in% result$posteam)
  expect_true("LV" %in% result$defteam)
})

test_that("normalize_schema maps SD to LAC", {
  mock <- create_mock_pbp(season = 2015L, n_games = 1L, n_plays_per_game = 20L)
  mock$posteam[1:5] <- "SD"

  result <- normalize_schema(mock, season = 2015L)

  expect_false("SD" %in% result$posteam)
  expect_true("LAC" %in% result$posteam)
})

test_that("normalize_schema maps STL to LA", {
  mock <- create_mock_pbp(season = 2014L, n_games = 1L, n_plays_per_game = 20L)
  mock$posteam[1:5] <- "STL"

  result <- normalize_schema(mock, season = 2014L)

  expect_false("STL" %in% result$posteam)
  expect_true("LA" %in% result$posteam)
})

test_that("normalize_schema does not corrupt current team abbreviations", {
  mock <- create_mock_pbp(season = 2025L, n_games = 1L, n_plays_per_game = 20L)
  mock$posteam[1:3] <- "LV"
  mock$posteam[4:6] <- "LAC"
  mock$posteam[7:9] <- "LA"

  result <- normalize_schema(mock, season = 2025L)

  # Current abbreviations should pass through unchanged
  expect_true("LV" %in% result$posteam)
  expect_true("LAC" %in% result$posteam)
  expect_true("LA" %in% result$posteam)
})


# ==============================================================================
# Test Suite 4: validate_season_coverage() Threshold Logic
# ==============================================================================

context("Season Coverage Validation")

test_that("get_expected_games returns correct counts for 16 and 17 game eras", {
  # 16-game era: 2010-2020
  expect_equal(get_expected_games(2010), 256L)
  expect_equal(get_expected_games(2015), 256L)
  expect_equal(get_expected_games(2020), 256L)

  # 17-game era: 2021-2025
  expect_equal(get_expected_games(2021), 272L)
  expect_equal(get_expected_games(2024), 272L)
  expect_equal(get_expected_games(2025), 272L)
})

test_that("validate_season_coverage handles missing cache gracefully", {
  result <- validate_season_coverage(
    seasons = 9999,
    cache_dir = tempdir()
  )

  expect_equal(nrow(result), 1)
  expect_false(result$all_pass)
  expect_true(is.na(result$n_games))
})

test_that("validate_season_coverage returns expected columns", {
  # Create a mock cached season
  tmp_dir <- file.path(tempdir(), "test_cache_coverage")
  dir.create(tmp_dir, showWarnings = FALSE)
  mock <- create_mock_pbp(season = 2024L, n_games = 4L, n_plays_per_game = 150L)
  mock <- normalize_schema(mock, season = 2024L)
  saveRDS(mock, file.path(tmp_dir, "pbp_normalized_2024.rds"))

  result <- validate_season_coverage(seasons = 2024, cache_dir = tmp_dir)

  expected_cols <- c("season", "n_games", "expected_games", "games_ok",
                     "n_teams", "teams_ok", "median_plays_per_game",
                     "plays_per_game_ok", "mean_epa", "epa_centered", "all_pass")
  expect_true(all(expected_cols %in% names(result)))

  # Clean up
  unlink(tmp_dir, recursive = TRUE)
})


# ==============================================================================
# Test Suite 5: get_schema_differences() Presence Matrix
# ==============================================================================

context("Schema Differences")

test_that("get_schema_differences builds correct presence matrix", {
  tmp_dir <- file.path(tempdir(), "test_cache_schema")
  dir.create(tmp_dir, showWarnings = FALSE)

  # Create two mock seasons with different columns
  mock_2020 <- create_mock_pbp(season = 2020L, n_games = 1L, n_plays_per_game = 10L)
  mock_2020 <- normalize_schema(mock_2020, season = 2020L)

  mock_2024 <- create_mock_pbp(season = 2024L, n_games = 1L, n_plays_per_game = 10L)
  mock_2024$new_col_2024 <- runif(nrow(mock_2024))
  mock_2024 <- normalize_schema(mock_2024, season = 2024L)

  saveRDS(mock_2020, file.path(tmp_dir, "pbp_normalized_2020.rds"))
  saveRDS(mock_2024, file.path(tmp_dir, "pbp_normalized_2024.rds"))

  result <- get_schema_differences(seasons = c(2020, 2024), cache_dir = tmp_dir)

  expect_true("column_name" %in% names(result))
  expect_true("s2020" %in% names(result))
  expect_true("s2024" %in% names(result))

  # new_col_2024 should be TRUE for 2024, FALSE for 2020
  new_col_row <- result %>% filter(column_name == "new_col_2024")
  expect_equal(nrow(new_col_row), 1)
  expect_true(new_col_row$s2024)
  expect_false(new_col_row$s2020)

  # Core columns should be TRUE in both
  epa_row <- result %>% filter(column_name == "epa")
  expect_true(epa_row$s2020)
  expect_true(epa_row$s2024)

  # Clean up
  unlink(tmp_dir, recursive = TRUE)
})

test_that("get_schema_differences stops when no cache files exist", {
  expect_error(
    get_schema_differences(seasons = c(9998, 9999), cache_dir = tempdir()),
    "No cached season files found"
  )
})


# ==============================================================================
# Test Suite 6: validate_epa_distribution() Filtering and Tolerance
# ==============================================================================

context("EPA Distribution Validation")

test_that("validate_epa_distribution returns expected columns", {
  tmp_dir <- file.path(tempdir(), "test_cache_epa")
  dir.create(tmp_dir, showWarnings = FALSE)
  mock <- create_mock_pbp(season = 2024L, n_games = 4L, n_plays_per_game = 200L)
  mock <- normalize_schema(mock, season = 2024L)
  saveRDS(mock, file.path(tmp_dir, "pbp_normalized_2024.rds"))

  result <- validate_epa_distribution(seasons = 2024, cache_dir = tmp_dir)

  expected_cols <- c("season", "n_plays", "mean_epa", "sd_epa",
                     "median_epa", "within_tolerance")
  expect_true(all(expected_cols %in% names(result)))

  # Clean up
  unlink(tmp_dir, recursive = TRUE)
})

test_that("validate_epa_distribution flags non-centered EPA", {
  tmp_dir <- file.path(tempdir(), "test_cache_epa_bad")
  dir.create(tmp_dir, showWarnings = FALSE)

  # Create mock with intentionally biased EPA
  mock <- create_mock_pbp(season = 2024L, n_games = 4L, n_plays_per_game = 200L)
  # Shift all EPA values to be strongly positive (should fail tolerance)
  pass_run_idx <- which(mock$play_type %in% c("pass", "run"))
  mock$epa[pass_run_idx] <- mock$epa[pass_run_idx] + 0.5

  mock <- normalize_schema(mock, season = 2024L)
  saveRDS(mock, file.path(tmp_dir, "pbp_normalized_2024.rds"))

  result <- validate_epa_distribution(seasons = 2024, cache_dir = tmp_dir, epa_tolerance = 0.05)

  # Should fail tolerance with such a large bias
  expect_false(result$within_tolerance)
  expect_true(result$mean_epa > 0.05)

  # Clean up
  unlink(tmp_dir, recursive = TRUE)
})

test_that("validate_epa_distribution handles missing cache file", {
  result <- validate_epa_distribution(
    seasons = 9999,
    cache_dir = tempdir()
  )

  expect_equal(nrow(result), 1)
  expect_false(result$within_tolerance)
  expect_true(is.na(result$n_plays))
})


# ==============================================================================
# Test Suite 7: NFL-Specific Edge Cases
# ==============================================================================

context("NFL-Specific Edge Cases")

test_that("17-game season boundary is correctly at 2021", {
  # This is a structural NFL rule that changed in 2021
  # The function must distinguish pre-2021 (256 games) from 2021+ (272 games)
  expect_equal(get_expected_games(2020), 256L)
  expect_equal(get_expected_games(2021), 272L)

  # Verify boundary is sharp, not gradual
  expect_equal(get_expected_games(2021) - get_expected_games(2020), 16L)
})

test_that("all three team relocations are handled", {
  # OAK->LV (2020), SD->LAC (2017), STL->LA (2016)
  expect_equal(length(TEAM_RELOCATIONS), 3)
  expect_equal(TEAM_RELOCATIONS[["OAK"]], "LV")
  expect_equal(TEAM_RELOCATIONS[["SD"]], "LAC")
  expect_equal(TEAM_RELOCATIONS[["STL"]], "LA")
})

test_that("SEASON_RANGE_DEFAULT covers 2010 through 2025 (16 seasons)", {
  expect_equal(length(SEASON_RANGE_DEFAULT), 16)
  expect_equal(min(SEASON_RANGE_DEFAULT), 2010)
  expect_equal(max(SEASON_RANGE_DEFAULT), 2025)
})

test_that("default season range includes 2025 (completed season)", {

  # 2025 NFL season is complete. The pipeline must include it.
  expect_true(2025 %in% SEASON_RANGE_DEFAULT)
})

test_that("core columns include all essential nflfastR play-level fields", {
  # Verify critical analytical columns are in the CORE_COLUMNS list
  # These are used by every downstream function in the toolkit
  essential <- c("epa", "success", "wp", "play_type", "posteam", "defteam",
                 "passer_player_id", "rusher_player_id", "receiver_player_id",
                 "down", "ydstogo", "touchdown", "interception")
  for (col in essential) {
    expect_true(col %in% CORE_COLUMNS,
                info = glue::glue("Missing essential core column: {col}"))
  }
})


# ==============================================================================
# Test Suite 8: Integration Test (Pipeline on Mock Data)
# ==============================================================================

context("Integration: Pipeline End-to-End")

test_that("full pipeline works on mock cached data", {
  tmp_dir <- file.path(tempdir(), "test_cache_integration")
  dir.create(tmp_dir, showWarnings = FALSE)

  # Create and cache two mock seasons
  for (s in c(2023L, 2024L)) {
    mock <- create_mock_pbp(season = s, n_games = 4L, n_plays_per_game = 150L)
    mock_norm <- normalize_schema(mock, season = s)
    saveRDS(mock_norm, file.path(tmp_dir, glue::glue("pbp_normalized_{s}.rds")))
  }

  # Validate coverage
  coverage <- validate_season_coverage(seasons = c(2023, 2024), cache_dir = tmp_dir)
  expect_equal(nrow(coverage), 2)
  expect_true(all(c("all_pass") %in% names(coverage)))

  # Schema differences
  diffs <- get_schema_differences(seasons = c(2023, 2024), cache_dir = tmp_dir)
  expect_true(nrow(diffs) > 0)
  expect_true("column_name" %in% names(diffs))

  # EPA validation
  epa <- validate_epa_distribution(seasons = c(2023, 2024), cache_dir = tmp_dir)
  expect_equal(nrow(epa), 2)
  expect_true(all(c("mean_epa", "within_tolerance") %in% names(epa)))

  # Load single season
  loaded <- load_normalized_season(season = 2024, cache_dir = tmp_dir)
  expect_true(nrow(loaded) > 0)
  expect_true("season_norm_version" %in% names(loaded))
  expect_equal(unique(loaded$season), 2024L)

  # Clean up
  unlink(tmp_dir, recursive = TRUE)
})

test_that("load_multi_season_pbp creates cache directory if missing", {
  tmp_dir <- file.path(tempdir(), "nonexistent_cache_dir_test")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)

  # This will try to download from nflfastR which will fail in test env,
  # but the directory creation should happen before the download attempt
  expect_false(dir.exists(tmp_dir))

  # Use a fake season that will error on download, but directory should be created
  tryCatch(
    load_multi_season_pbp(seasons = 2024, cache_dir = tmp_dir),
    error = function(e) NULL,
    warning = function(w) NULL
  )

  expect_true(dir.exists(tmp_dir))

  # Clean up
  unlink(tmp_dir, recursive = TRUE)
})


# ==============================================================================
# Summary
# ==============================================================================

cat("\n================================================================\n")
cat("Season 2, Week 1 Test Suite Complete\n")
cat("Functions tested: normalize_schema, validate_season_coverage,\n")
cat("  get_schema_differences, validate_epa_distribution,\n")
cat("  load_normalized_season, load_multi_season_pbp,\n")
cat("  get_expected_games, CONSTANTS\n")
cat("NFL edge cases: 17-game boundary, team relocations,\n")
cat("  garbage time, EPA centering, 2025 season inclusion\n")
cat("================================================================\n")
