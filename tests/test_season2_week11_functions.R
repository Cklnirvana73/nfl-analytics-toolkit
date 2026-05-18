# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 11
# Test Suite: Injury Proximity Experiment Functions
# File: tests/test_season2_week11_functions.R
#
# Tests (62 total):
#   Section A: identify_returning_players()        -- 12 tests
#   Section B: classify_treatment_control_injury() --  9 tests
#   Section C: check_balance_injury_groups()       --  7 tests
#   Section D: validate_injury_assumptions()       --  6 tests
#   Section E: design_power_analysis_injury()      --  5 tests
#   Section F: create_injury_experiment_specification() -- 4 tests
#   Section G: calculate_injury_effect()           --  9 tests
#   Section H: analyze_injury_heterogeneous_effects() -- 6 tests
#   Section I: injury_robustness_check()           --  4 tests
#
# No network calls. No live nflfastR downloads.
# All tests use synthetic in-memory fixtures.
# load_normalized_season() and calculate_fantasy_points_ext() are sourced
# from R/15 and R/17 but never called in this test file.
#
# Run:
#   testthat::test_file(here::here("tests", "test_season2_week11_functions.R"))
# ==============================================================================

library(testthat)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(here)

source(here::here("R", "25_injury_proximity_experiment.R"))


# ==============================================================================
# SHARED FIXTURES
# ==============================================================================
#
# Fixture design (hand-verifiable expected outputs documented inline):
#
#   TEST_SEASON = 2020L  (analysis season)
#   TEST_PRIOR  = 2019L  (prior-season PPG baseline)
#
#   Players:
#     TEST-P001 (WR, KC): participates wks 1,2,4,5,6,7,8 -- absent wk 3
#       -> first_absent_week=3, n_prior_games=2, return_week=4
#       -> TREATMENT (return_week=4 <= TREATMENT_RETURN_MAX_W11=4)
#
#     TEST-P002 (RB, KC): participates all 8 training weeks
#       -> CONTROL
#
#     TEST-P003 (TE, SF): participates wks 1,2,3,6,7,8 -- absent wks 4,5
#       -> first_absent_week=4, n_prior_games=3, return_week=6
#       -> EXCLUDED (return_week=6 > 4)
#
#     TEST-P004 (QB, SF): participates all 8 training weeks
#       -> CONTROL
#
#   Team schedule: KC and SF both play all 18 weeks (no byes in training wks)
#
#   Fantasy scores (prior season 2019 / outcome 2020 Wks 9-18):
#     P001: prior_ppg=8,  outcome_ppg=6  -> abs_error=|6-8|=2
#     P002: prior_ppg=12, outcome_ppg=12 -> abs_error=|12-12|=0
#     P003: prior_ppg=5,  outcome_ppg=5  (not in groups, not used in effect)
#     P004: prior_ppg=20, outcome_ppg=22 -> abs_error=|22-20|=2
#
#   Expected MAE treatment = 2.0
#   Expected MAE control   = mean(0, 2) = 1.0
#   Expected MAE diff (trt - ctl) = +1.0 (treatment harder to predict)
# ==============================================================================

TEST_SEASON <- 2020L
TEST_PRIOR  <- 2019L

make_participation <- function() {
  dplyr::bind_rows(
    # P001: absent week 3 -- treatment candidate
    dplyr::tibble(
      player_id   = "TEST-P001",
      player_name = "T.Player",
      season      = TEST_SEASON,
      week        = c(1L, 2L, 4L, 5L, 6L, 7L, 8L),
      team        = "KC"
    ),
    # P002: all 8 weeks -- control
    dplyr::tibble(
      player_id   = "TEST-P002",
      player_name = "C.Player1",
      season      = TEST_SEASON,
      week        = 1L:8L,
      team        = "KC"
    ),
    # P003: absent weeks 4,5, return week 6 -- excluded (return > 4)
    dplyr::tibble(
      player_id   = "TEST-P003",
      player_name = "E.Player",
      season      = TEST_SEASON,
      week        = c(1L, 2L, 3L, 6L, 7L, 8L),
      team        = "SF"
    ),
    # P004: all 8 weeks -- control
    dplyr::tibble(
      player_id   = "TEST-P004",
      player_name = "C.Player2",
      season      = TEST_SEASON,
      week        = 1L:8L,
      team        = "SF"
    )
  )
}

make_schedule <- function() {
  # Both teams play all 18 weeks. No byes in training window.
  tidyr::expand_grid(
    season = TEST_SEASON,
    week   = 1L:18L,
    team   = c("KC", "SF")
  )
}

make_roster_positions <- function() {
  dplyr::tibble(
    player_id = c("TEST-P001", "TEST-P002", "TEST-P003", "TEST-P004"),
    season    = TEST_SEASON,
    position  = c("WR", "RB", "TE", "QB")
  )
}

make_weekly_fantasy <- function() {
  # Returns weekly fantasy for both TEST_PRIOR (2019) and TEST_SEASON (2020).
  # Prior season: all 17 weeks at prior_ppg.
  # Analysis season: weeks 1-8 at prior_ppg (training), weeks 9-18 at outcome_ppg.
  player_specs <- list(
    list(id = "TEST-P001", name = "T.Player",  pos = "WR", team = "KC",
         prior_ppg = 8,  outcome_ppg = 6),
    list(id = "TEST-P002", name = "C.Player1", pos = "RB", team = "KC",
         prior_ppg = 12, outcome_ppg = 12),
    list(id = "TEST-P003", name = "E.Player",  pos = "TE", team = "SF",
         prior_ppg = 5,  outcome_ppg = 5),
    list(id = "TEST-P004", name = "C.Player2", pos = "QB", team = "SF",
         prior_ppg = 20, outcome_ppg = 22)
  )

  purrr::map_dfr(player_specs, function(p) {
    dplyr::bind_rows(
      # Prior season: 17 weeks
      dplyr::tibble(
        player_id            = p$id,
        player_name          = p$name,
        season               = TEST_PRIOR,
        week                 = 1L:17L,
        game_id              = paste0("gm_", TEST_PRIOR, "_", 1L:17L),
        position             = p$pos,
        team                 = p$team,
        total_fantasy_points = as.double(p$prior_ppg)
      ),
      # Analysis season: weeks 1-18
      dplyr::tibble(
        player_id            = p$id,
        player_name          = p$name,
        season               = TEST_SEASON,
        week                 = 1L:18L,
        game_id              = paste0("gm_", TEST_SEASON, "_", 1L:18L),
        position             = p$pos,
        team                 = p$team,
        total_fantasy_points = c(
          rep(as.double(p$prior_ppg),   8L),  # wks 1-8
          rep(as.double(p$outcome_ppg), 10L)   # wks 9-18
        )
      )
    )
  })
}

# Build shared objects used across all sections
participation_fx  <- make_participation()
team_schedule_fx  <- make_schedule()
roster_pos_fx     <- make_roster_positions()
weekly_fantasy_fx <- make_weekly_fantasy()


# ==============================================================================
# SECTION A: identify_returning_players()
# ==============================================================================

test_that("A01: rejects non-data-frame participation_records", {
  expect_error(
    identify_returning_players(
      participation_records = "not a df",
      team_schedule         = team_schedule_fx,
      roster_positions      = roster_pos_fx,
      seasons               = TEST_SEASON,
      verbose               = FALSE
    )
  )
})

test_that("A02: rejects participation_records missing required columns", {
  bad <- participation_fx %>% dplyr::select(-player_name)
  expect_error(
    identify_returning_players(
      participation_records = bad,
      team_schedule         = team_schedule_fx,
      roster_positions      = roster_pos_fx,
      seasons               = TEST_SEASON,
      verbose               = FALSE
    ),
    regexp = "player_name"
  )
})

test_that("A03: rejects non-data-frame team_schedule", {
  expect_error(
    identify_returning_players(
      participation_records = participation_fx,
      team_schedule         = list(a = 1),
      roster_positions      = roster_pos_fx,
      seasons               = TEST_SEASON,
      verbose               = FALSE
    )
  )
})

test_that("A04: returns a tibble", {
  result <- identify_returning_players(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    training_weeks        = 1L:8L,
    verbose               = FALSE
  )
  expect_s3_class(result, "tbl_df")
})

test_that("A05: returns correct column names", {
  result <- identify_returning_players(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  expected_cols <- c(
    "player_id", "player_name", "season", "team", "position",
    "first_absent_week", "return_week", "n_absent_weeks",
    "n_prior_games", "n_training_games_post_return"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("A06: P001 identified as returning player (absent wk 3, return wk 4)", {
  result <- identify_returning_players(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  expect_true(nrow(result) > 0L)
  p001 <- result %>% dplyr::filter(player_id == "TEST-P001")
  expect_equal(nrow(p001), 1L)
})

test_that("A07: P002 (healthy all 8 weeks) not in returning players", {
  result <- identify_returning_players(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  p002 <- result %>% dplyr::filter(player_id == "TEST-P002")
  expect_equal(nrow(p002), 0L)
})

test_that("A08: P001 first_absent_week = 3", {
  result <- identify_returning_players(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  p001 <- result %>% dplyr::filter(player_id == "TEST-P001")
  expect_equal(p001$first_absent_week, 3L)
})

test_that("A09: P001 return_week = 4", {
  result <- identify_returning_players(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  p001 <- result %>% dplyr::filter(player_id == "TEST-P001")
  expect_equal(p001$return_week, 4L)
})

test_that("A10: P001 n_absent_weeks = 1 (only week 3)", {
  result <- identify_returning_players(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  p001 <- result %>% dplyr::filter(player_id == "TEST-P001")
  expect_equal(p001$n_absent_weeks, 1L)
})

test_that("A11: P001 n_prior_games = 2 (weeks 1 and 2 before absence)", {
  result <- identify_returning_players(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  p001 <- result %>% dplyr::filter(player_id == "TEST-P001")
  expect_equal(p001$n_prior_games, 2L)
})

test_that("A12: min_prior_games filter excludes player with only 1 prior game", {
  # P001 has 2 prior games. Setting min_prior_games = 3 should exclude it.
  result <- identify_returning_players(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    min_prior_games       = 3L,
    verbose               = FALSE
  )
  p001 <- result %>% dplyr::filter(player_id == "TEST-P001")
  expect_equal(nrow(p001), 0L)
})


# ==============================================================================
# SECTION B: classify_treatment_control_injury()
# ==============================================================================

# Shared: build returners from fixture
returners_fx <- identify_returning_players(
  participation_records = participation_fx,
  team_schedule         = team_schedule_fx,
  roster_positions      = roster_pos_fx,
  seasons               = TEST_SEASON,
  verbose               = FALSE
)

test_that("B01: returns a tibble", {
  result <- classify_treatment_control_injury(
    returning_players     = returners_fx,
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  expect_s3_class(result, "tbl_df")
})

test_that("B02: contains required columns including group and era", {
  result <- classify_treatment_control_injury(
    returning_players     = returners_fx,
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  expect_true(all(c("player_id", "season", "group", "era") %in% names(result)))
})

test_that("B03: P001 classified as treatment (return_week = 4)", {
  result <- classify_treatment_control_injury(
    returning_players     = returners_fx,
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  p001_group <- result %>%
    dplyr::filter(player_id == "TEST-P001") %>%
    dplyr::pull(group)
  expect_equal(p001_group, "treatment")
})

test_that("B04: P002 classified as control (healthy all 8 weeks)", {
  result <- classify_treatment_control_injury(
    returning_players     = returners_fx,
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  p002_group <- result %>%
    dplyr::filter(player_id == "TEST-P002") %>%
    dplyr::pull(group)
  expect_equal(p002_group, "control")
})

test_that("B05: P004 classified as control (healthy all 8 weeks)", {
  result <- classify_treatment_control_injury(
    returning_players     = returners_fx,
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  p004_group <- result %>%
    dplyr::filter(player_id == "TEST-P004") %>%
    dplyr::pull(group)
  expect_equal(p004_group, "control")
})

test_that("B06: P003 excluded (return_week = 6 > treatment_return_max = 4)", {
  result <- classify_treatment_control_injury(
    returning_players     = returners_fx,
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  p003 <- result %>% dplyr::filter(player_id == "TEST-P003")
  expect_equal(nrow(p003), 0L)
})

test_that("B07: group column contains only 'treatment' and 'control' values", {
  result <- classify_treatment_control_injury(
    returning_players     = returners_fx,
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  expect_true(all(result$group %in% c("treatment", "control")))
})

test_that("B08: era column uses 'Early' or 'Modern' labels only", {
  result <- classify_treatment_control_injury(
    returning_players     = returners_fx,
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  expect_true(all(result$era %in% c("Early", "Modern")))
})

test_that("B09: era is 'Early' for season 2020 (< ERA_BREAKPOINT_W11 = 2017 is False, 2020 >= 2017 -> Modern)", {
  result <- classify_treatment_control_injury(
    returning_players     = returners_fx,
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    seasons               = TEST_SEASON,
    verbose               = FALSE
  )
  # TEST_SEASON = 2020 >= ERA_BREAKPOINT_W11 = 2017 -> Modern
  expect_true(all(result$era == "Modern"))
})


# ==============================================================================
# SECTION C: check_balance_injury_groups()
# ==============================================================================

groups_fx <- classify_treatment_control_injury(
  returning_players     = returners_fx,
  participation_records = participation_fx,
  team_schedule         = team_schedule_fx,
  roster_positions      = roster_pos_fx,
  seasons               = TEST_SEASON,
  verbose               = FALSE
)

test_that("C01: returns a list", {
  result <- check_balance_injury_groups(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  expect_type(result, "list")
})

test_that("C02: result contains smd_table slot", {
  result <- check_balance_injury_groups(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  expect_true("smd_table" %in% names(result))
})

test_that("C03: smd_table has required columns", {
  result <- check_balance_injury_groups(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  expect_true(all(c("covariate", "smd", "flagged") %in% names(result$smd_table)))
})

test_that("C04: smd_table flagged column is logical", {
  result <- check_balance_injury_groups(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  expect_true(is.logical(result$smd_table$flagged))
})

test_that("C05: balance_table slot present and is a data frame", {
  result <- check_balance_injury_groups(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  expect_true("balance_table" %in% names(result))
  expect_s3_class(result$balance_table, "data.frame")
})

test_that("C06: position_dist slot present", {
  result <- check_balance_injury_groups(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  expect_true("position_dist" %in% names(result))
})

test_that("C07: balance_summary is a character scalar", {
  result <- check_balance_injury_groups(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  expect_true("balance_summary" %in% names(result))
  expect_type(result$balance_summary, "character")
  expect_length(result$balance_summary, 1L)
})


# ==============================================================================
# SECTION D: validate_injury_assumptions()
# ==============================================================================

test_that("D01: returns a list", {
  result <- validate_injury_assumptions(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  expect_type(result, "list")
})

test_that("D02: checks_passed is a named logical vector with 5 elements", {
  result <- validate_injury_assumptions(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  expect_true("checks_passed" %in% names(result))
  expect_true(is.logical(result$checks_passed))
  expect_length(result$checks_passed, 5L)
  expect_false(is.null(names(result$checks_passed)))
})

test_that("D03: all required slots present", {
  result <- validate_injury_assumptions(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  required_slots <- c("absence_rate", "return_timing", "control_consistency",
                      "parallel_trends", "position_stability",
                      "checks_passed", "report")
  expect_true(all(required_slots %in% names(result)))
})

test_that("D04: report is a character scalar", {
  result <- validate_injury_assumptions(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  expect_type(result$report, "character")
  expect_length(result$report, 1L)
})

test_that("D05: absence_rate has position column", {
  result <- validate_injury_assumptions(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  expect_true("position" %in% names(result$absence_rate))
})

test_that("D06: return_timing has return_week and pct columns", {
  result <- validate_injury_assumptions(groups_fx, weekly_fantasy_fx,
                                        verbose = FALSE)
  expect_true(all(c("return_week", "pct") %in% names(result$return_timing)))
})


# ==============================================================================
# SECTION E: design_power_analysis_injury()
# ==============================================================================

test_that("E01: returns a list with all required slots", {
  result <- design_power_analysis_injury(groups_fx, verbose = FALSE)
  required_slots <- c("power_at_target", "detectable_at_80", "detectable_at_90",
                      "power_table", "group_sizes", "summary")
  expect_true(all(required_slots %in% names(result)))
})

test_that("E02: power_at_target is numeric or NA (small N may fail power.t.test)", {
  result <- design_power_analysis_injury(groups_fx, verbose = FALSE)
  expect_true(is.numeric(result$power_at_target) || is.na(result$power_at_target))
})

test_that("E03: power_table has cohens_d and power columns", {
  result <- design_power_analysis_injury(groups_fx, verbose = FALSE)
  expect_true(all(c("cohens_d", "power") %in% names(result$power_table)))
})

test_that("E04: group_sizes is a named numeric vector with n_treatment and n_control", {
  result <- design_power_analysis_injury(groups_fx, verbose = FALSE)
  expect_true(all(c("n_treatment", "n_control") %in% names(result$group_sizes)))
  expect_true(is.numeric(result$group_sizes))
})

test_that("E05: group_sizes reflects fixture counts (1 treatment, 2 control)", {
  result <- design_power_analysis_injury(groups_fx, verbose = FALSE)
  expect_equal(unname(result$group_sizes["n_treatment"]), 1L)
  expect_equal(unname(result$group_sizes["n_control"]),   2L)
})


# ==============================================================================
# SECTION F: create_injury_experiment_specification()
# ==============================================================================

balance_fx   <- check_balance_injury_groups(groups_fx, weekly_fantasy_fx,
                                            verbose = FALSE)
power_fx     <- design_power_analysis_injury(groups_fx, verbose = FALSE)

test_that("F01: returns a character scalar", {
  result <- create_injury_experiment_specification(
    groups        = groups_fx,
    balance       = balance_fx,
    power_results = power_fx
  )
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that("F02: spec contains RESEARCH QUESTION section", {
  result <- create_injury_experiment_specification(
    groups        = groups_fx,
    balance       = balance_fx,
    power_results = power_fx
  )
  expect_true(grepl("RESEARCH QUESTION", result, fixed = TRUE))
})

test_that("F03: spec contains GROUP DEFINITIONS section", {
  result <- create_injury_experiment_specification(
    groups        = groups_fx,
    balance       = balance_fx,
    power_results = power_fx
  )
  expect_true(grepl("GROUP DEFINITIONS", result, fixed = TRUE))
})

test_that("F04: spec contains schema tag", {
  result <- create_injury_experiment_specification(
    groups        = groups_fx,
    balance       = balance_fx,
    power_results = power_fx
  )
  expect_true(grepl(SCHEMA_TAG_W11, result, fixed = TRUE))
})


# ==============================================================================
# SECTION G: calculate_injury_effect()
# ==============================================================================

test_that("G01: returns a list with all required slots", {
  result <- calculate_injury_effect(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    B              = 100L,      # fewer resamples for test speed
    verbose        = FALSE
  )
  required_slots <- c("group_summary", "effect_mae", "effect_ppg",
                      "cohens_d_mae", "t_test_mae", "t_test_ppg",
                      "trajectory", "player_level")
  expect_true(all(required_slots %in% names(result)))
})

test_that("G02: group_summary has 2 rows (treatment and control)", {
  result <- calculate_injury_effect(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    B              = 100L,
    verbose        = FALSE
  )
  expect_equal(nrow(result$group_summary), 2L)
  expect_true(all(c("treatment", "control") %in% result$group_summary$group))
})

test_that("G03: group_summary has required columns", {
  result <- calculate_injury_effect(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    B              = 100L,
    verbose        = FALSE
  )
  required_cols <- c("group", "n_players", "mean_prior_ppg",
                     "mean_outcome_ppg", "mean_mae")
  expect_true(all(required_cols %in% names(result$group_summary)))
})

test_that("G04: P001 treatment mean_outcome_ppg = 6.0 (fixture value)", {
  result <- calculate_injury_effect(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    B              = 100L,
    verbose        = FALSE
  )
  trt_ppg <- result$group_summary %>%
    dplyr::filter(group == "treatment") %>%
    dplyr::pull(mean_outcome_ppg)
  expect_equal(trt_ppg, 6.0, tolerance = 0.01)
})

test_that("G05: treatment mean_mae > 0 (P001 underpredicted: outcome < prior)", {
  result <- calculate_injury_effect(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    B              = 100L,
    verbose        = FALSE
  )
  trt_mae <- result$group_summary %>%
    dplyr::filter(group == "treatment") %>%
    dplyr::pull(mean_mae)
  expect_gt(trt_mae, 0)
})

test_that("G06: effect_mae is a list with estimate, ci_lower, ci_upper", {
  result <- calculate_injury_effect(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    B              = 100L,
    verbose        = FALSE
  )
  expect_true(all(c("estimate", "ci_lower", "ci_upper") %in%
                    names(result$effect_mae)))
})

test_that("G07: player_level has prediction_error and abs_error columns", {
  result <- calculate_injury_effect(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    B              = 100L,
    verbose        = FALSE
  )
  expect_true(all(c("prediction_error", "abs_error") %in%
                    names(result$player_level)))
})

test_that("G08: cohens_d_mae is numeric or NA (NA expected when treatment N=1)", {
  result <- calculate_injury_effect(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    B              = 100L,
    verbose        = FALSE
  )
  # With only 1 treatment player, var() = NA -> cohens_d_mae = NA by design.
  # Real data has N > 1. Test verifies the guard returns NA rather than crashing.
  expect_true(is.numeric(result$cohens_d_mae) || is.na(result$cohens_d_mae))
})

test_that("G08b: P001 prediction_error = -2.0 (outcome 6 - prior 8)", {
  result <- calculate_injury_effect(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    B              = 100L,
    verbose        = FALSE
  )
  p001_err <- result$player_level %>%
    dplyr::filter(player_id == "TEST-P001") %>%
    dplyr::pull(prediction_error)
  expect_equal(p001_err, -2.0, tolerance = 0.01)
})

test_that("G09: trajectory has week and mean_ppg columns with 10 rows (wks 9-18)", {
  result <- calculate_injury_effect(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    B              = 100L,
    verbose        = FALSE
  )
  expect_true(all(c("week", "mean_ppg") %in% names(result$trajectory)))
  expect_equal(nrow(result$trajectory), 10L)
})


# ==============================================================================
# SECTION H: analyze_injury_heterogeneous_effects()
# ==============================================================================

effects_fx <- calculate_injury_effect(
  weekly_fantasy = weekly_fantasy_fx,
  groups         = groups_fx,
  outcome_weeks  = 9L:18L,
  B              = 100L,
  verbose        = FALSE
)

test_that("H01: returns a list with by_position, by_return_timing, by_era, narrative", {
  result <- analyze_injury_heterogeneous_effects(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    verbose        = FALSE
  )
  expect_true(all(c("by_position", "by_return_timing", "by_era", "narrative") %in%
                    names(result)))
})

test_that("H02: by_position has stratum, n_treatment, n_control, mae_diff columns", {
  result <- analyze_injury_heterogeneous_effects(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    verbose        = FALSE
  )
  expect_true(all(c("stratum", "n_treatment", "n_control", "mae_diff") %in%
                    names(result$by_position)))
})

test_that("H03: by_position has at most 4 rows (one per skill position)", {
  result <- analyze_injury_heterogeneous_effects(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    verbose        = FALSE
  )
  expect_lte(nrow(result$by_position), 4L)
})

test_that("H04: by_era has 2 rows (Early and Modern)", {
  result <- analyze_injury_heterogeneous_effects(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    verbose        = FALSE
  )
  expect_equal(nrow(result$by_era), 2L)
  expect_true(all(c("Early", "Modern") %in% result$by_era$stratum))
})

test_that("H05: narrative is a character scalar", {
  result <- analyze_injury_heterogeneous_effects(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    verbose        = FALSE
  )
  expect_type(result$narrative, "character")
  expect_length(result$narrative, 1L)
})

test_that("H06: by_return_timing has stratum column", {
  result <- analyze_injury_heterogeneous_effects(
    weekly_fantasy = weekly_fantasy_fx,
    groups         = groups_fx,
    outcome_weeks  = 9L:18L,
    verbose        = FALSE
  )
  expect_true("stratum" %in% names(result$by_return_timing))
})


# ==============================================================================
# SECTION I: injury_robustness_check()
# ==============================================================================

test_that("I01: returns a list", {
  result <- injury_robustness_check(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    weekly_fantasy        = weekly_fantasy_fx,
    outcome_weeks         = 9L:18L,
    verbose               = FALSE
  )
  expect_type(result, "list")
})

test_that("I02: results_table has exactly 9 rows (3 return_max x 3 min_prior)", {
  result <- injury_robustness_check(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    weekly_fantasy        = weekly_fantasy_fx,
    outcome_weeks         = 9L:18L,
    verbose               = FALSE
  )
  expect_equal(nrow(result$results_table), 9L)
})

test_that("I03: results_table has treatment_return_max and min_prior_games columns", {
  result <- injury_robustness_check(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    weekly_fantasy        = weekly_fantasy_fx,
    outcome_weeks         = 9L:18L,
    verbose               = FALSE
  )
  expect_true(all(c("treatment_return_max", "min_prior_games") %in%
                    names(result$results_table)))
})

test_that("I04: summary is a character scalar", {
  result <- injury_robustness_check(
    participation_records = participation_fx,
    team_schedule         = team_schedule_fx,
    roster_positions      = roster_pos_fx,
    weekly_fantasy        = weekly_fantasy_fx,
    outcome_weeks         = 9L:18L,
    verbose               = FALSE
  )
  expect_type(result$summary, "character")
  expect_length(result$summary, 1L)
})


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("Week 11 Test Suite Summary\n")
cat(strrep("=", 60), "\n")
cat("Sections: A (12) B (9) C (7) D (6) E (5) F (4) G (9) H (6) I (4)\n")
cat("Target total: 62 tests\n")
cat("Fixtures: 4 players, 2 teams, seasons 2019-2020\n")
cat("No network calls. No live nflfastR downloads.\n")
cat(strrep("=", 60), "\n")
