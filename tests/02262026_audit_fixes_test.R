# test_audit_fixes.R
# Targeted tests for the six issues fixed in the February 2026 audit.
# Run with: testthat::test_file("test_audit_fixes.R")

library(testthat)
library(dplyr)
library(zoo)

source("R/05_consistency_metrics.R")
source("R/07_epa_features.R")
source("R/08_usage_features.R")
source("R/10_opponent_features.R")


# ==============================================================================
# ISSUE 1: Rolling window resets on team change
# ==============================================================================

test_that("calculate_rolling_stats resets window on team change", {
  mock <- tibble(
    player_id   = "P1",
    player_name = "Test Player",
    season      = 2025L,
    team        = c(rep("KC", 6), rep("NE", 6)),
    week        = c(1:6, 8:13),   # week 7 = bye / trade week
    rush_yards  = c(80, 90, 70, 100, 85, 95, 60, 70, 75, 80, 65, 90)
  )
  
  result <- calculate_rolling_stats(mock, stat_columns = "rush_yards",
                                    windows = 3, min_games = 2)
  
  # First valid roll3 on team NE (week 8 in NE, row 7 overall) must be NA
  # because NE has no prior games yet -- window cannot borrow from KC
  ne_rows <- result %>% filter(team == "NE")
  expect_true(is.na(ne_rows$rush_yards_roll3[1]),
              info = "First NE row roll3 should be NA -- window must not borrow from KC")
  expect_true(is.na(ne_rows$rush_yards_roll3[2]),
              info = "Second NE row roll3 should be NA -- only 1 prior NE game")
  # Third NE row should have a valid value (2 prior NE games meets min_games=2)
  expect_false(is.na(ne_rows$rush_yards_roll3[3]),
               info = "Third NE row roll3 should be valid (2 prior NE games)")
})


# ==============================================================================
# ISSUE 3: success_rate uses success column not epa > 0
# ==============================================================================

test_that("compile_feature_matrix requires success column", {
  # Convention enforcement: function must error if success column is absent.
  # This is the correct test for Issue 3 -- on valid nflfastR data, success is
  # definitionally as.integer(epa > 0), so output values cannot diverge.
  # The meaningful guarantee is that the column is required and validated.
  mock_plays <- tibble(
    play_type            = "pass",
    posteam              = "KC",
    defteam              = "NE",
    season               = 2025L,
    week                 = 1L,
    game_id              = "2025_01_KC_NE",
    epa                  = c(0.5, -0.3, 0.2),
    # success column intentionally omitted
    wp                   = 0.5,
    qtr                  = 2L,
    passer_player_id     = "QB1",
    passer_player_name   = "A.Smith",
    rusher_player_id     = NA_character_,
    rusher_player_name   = NA_character_,
    receiver_player_id   = NA_character_,
    receiver_player_name = NA_character_,
    qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
  )
  
  expect_error(
    compile_feature_matrix(
      pbp_data           = mock_plays,
      opp_adjusted       = NULL,
      def_styles         = NULL,
      season             = 2025L,
      min_plays_per_week = 1L
    ),
    regexp = "success",
    info   = "compile_feature_matrix must error when success column is missing"
  )
})

test_that("success_rate_this_week equals mean of success column", {
  # Verify the output value directly. success = c(1, 0, 1) -> mean = 0.667.
  # Do not attempt to compare against epa > 0 -- they are identical on valid data.
  mock_plays <- tibble(
    play_type            = "pass",
    posteam              = "KC",
    defteam              = "NE",
    season               = 2025L,
    week                 = 1L,
    game_id              = "2025_01_KC_NE",
    epa                  = c(0.5, -0.3, 0.2),
    success              = c(1L,   0L,  1L),
    wp                   = 0.5,
    qtr                  = 2L,
    passer_player_id     = "QB1",
    passer_player_name   = "A.Smith",
    rusher_player_id     = NA_character_,
    rusher_player_name   = NA_character_,
    receiver_player_id   = NA_character_,
    receiver_player_name = NA_character_,
    qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
  )
  
  result <- compile_feature_matrix(
    pbp_data           = mock_plays,
    opp_adjusted       = NULL,
    def_styles         = NULL,
    season             = 2025L,
    min_plays_per_week = 1L
  )
  
  expect_true(nrow(result) > 0,
              info = "compile_feature_matrix returned empty -- check min_plays_per_week or play filters")
  expect_equal(result$success_rate_this_week[1], 2/3, tolerance = 0.01,
               info = "success_rate_this_week should be mean(c(1,0,1)) = 0.667")
})


# ==============================================================================
# ISSUE 4: No lookahead in epa_season_to_date
# ==============================================================================

test_that("epa_season_to_date excludes current week (no lookahead)", {
  mock_plays <- tibble(
    play_type            = "pass",
    posteam              = "KC",
    defteam              = "NE",
    season               = 2025L,
    week                 = rep(1:6, each = 10),
    game_id              = paste0("2025_0", rep(1:6, each = 10), "_KC_NE"),
    epa                  = rep(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), each = 10),
    success              = rep(1L, 60),
    wp                   = 0.5,
    qtr                  = 2L,
    passer_player_id     = "QB1",
    passer_player_name   = "A.Smith",
    rusher_player_id     = NA_character_,
    rusher_player_name   = NA_character_,
    receiver_player_id   = NA_character_,
    receiver_player_name = NA_character_,
    qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
  )
  
  result <- compile_feature_matrix(
    pbp_data = mock_plays, opp_adjusted = NULL, def_styles = NULL,
    season = 2025L, min_plays_per_week = 1L
  ) %>% arrange(week)
  
  # Week 1: no prior games -- epa_season_to_date must be NA
  expect_true(is.na(result$epa_season_to_date[1]),
              info = "Week 1 epa_season_to_date must be NA (no prior weeks)")
  
  # Week 3 epa_season_to_date must equal mean of weeks 1-2 (not include week 3)
  if (nrow(result) >= 3) {
    week1_epa <- mean(mock_plays$epa[mock_plays$week == 1])  # 0.1
    week2_epa <- mean(mock_plays$epa[mock_plays$week == 2])  # 0.2
    expected_w3 <- mean(c(week1_epa, week2_epa))              # 0.15
    expect_equal(result$epa_season_to_date[3], expected_w3, tolerance = 0.01,
                 info = "Week 3 epa_season_to_date should be mean of weeks 1-2 only")
  }
})


# ==============================================================================
# ISSUE 6: role_stability_flag is FALSE early, TRUE once window is clean
# ==============================================================================

test_that("role_stability_flag is FALSE until rolling_window prior stable weeks", {
  mock_plays <- tibble(
    play_type            = "pass",
    posteam              = "KC",
    defteam              = "NE",
    season               = 2025L,
    week                 = rep(1:8, each = 10),
    game_id              = paste0("2025_0", rep(1:8, each = 10), "_KC_NE"),
    epa                  = 0.1,
    success              = 1L,
    wp                   = 0.5,
    qtr                  = 2L,
    passer_player_id     = "QB1",
    passer_player_name   = "A.Smith",
    rusher_player_id     = NA_character_,
    rusher_player_name   = NA_character_,
    receiver_player_id   = NA_character_,
    receiver_player_name = NA_character_,
    qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
  )
  
  result <- compile_feature_matrix(
    pbp_data = mock_plays, opp_adjusted = NULL, def_styles = NULL,
    season = 2025L, rolling_window = 3L, min_plays_per_week = 1L
  ) %>% arrange(week)
  
  if (nrow(result) >= 4) {
    # Weeks 1-3: fewer than rolling_window stable prior weeks -- flag should be FALSE
    expect_false(result$role_stability_flag[1],
                 info = "Week 1 role_stability_flag should be FALSE")
    expect_false(result$role_stability_flag[3],
                 info = "Week 3 role_stability_flag should be FALSE (need >3 stable weeks)")
    # Week 4+: 3+ stable prior weeks -- flag should be TRUE
    expect_true(result$role_stability_flag[4],
                info = "Week 4 role_stability_flag should be TRUE (3 stable prior weeks)")
  }
})


# ==============================================================================
# Run summary
# ==============================================================================

cat("\n--- Audit Fix Tests Complete ---\n")
cat("5 tests covering: team window reset, success_rate column enforcement,\n")
cat("success_rate output value, lookahead in epa_season_to_date, role_stability_flag.\n\n")