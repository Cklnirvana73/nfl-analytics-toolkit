# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 12
# Test Suite: Usage Ramp Experiment + Rookie Subgroup Analysis
# File: tests/test_season2_week12_functions.R
#
# Run from project root:
#   testthat::test_file(here::here("tests", "test_season2_week12_functions.R"))
#
# SCOPE: 13 sections, 63 tests. No live nflfastR downloads. All tests use
# synthetic in-memory fixtures. No network dependency.
#
# KNOWN R/26 FIX (identified from example run output):
#   The two left_join calls in .compute_weekly_usage_season() that attach
#   player_carries and player_pass to all_players can produce a many-to-many
#   warning when a player appears on two teams in the same week (mid-season
#   trade, data artifact). The fix: deduplicate player_carries and player_pass
#   to one row per (player_id, season, week) by summing stats before joining.
#   Test B8 verifies no duplicate player-week rows in the output. Apply the
#   fix to .compute_weekly_usage_season() if test B8 fires.
#
# SECTIONS:
#   A. Shared fixtures (setup, no tests)
#   B. .compute_weekly_usage_season()        -- 10 tests
#   C. .bootstrap_ramp_diff_ci()             --  5 tests
#   D. identify_usage_ramps()                -- 12 tests
#   E. classify_treatment_control_ramp()     --  8 tests
#   F. check_balance_ramp_groups()           --  5 tests
#   G. validate_ramp_assumptions()           --  5 tests
#   H. identify_rookies()                    --  7 tests
#   I. stratify_by_rookie_status()           --  5 tests
#   J. calculate_usage_persistence()         --  3 tests
#   K. run_ramp_experiment()                 --  3 tests
#   L. Statistical assumption tests          --  5 tests (non-negotiable)
#   Total: 68 tests
#
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)
library(tidyr)
library(glue)
library(here)
library(purrr)

source(here::here("R", "26_usage_ramp_experiment.R"))


# ==============================================================================
# SECTION A: SHARED FIXTURES
# ==============================================================================
# All synthetic data is hand-verified against known expected values.
# See inline comments for the math behind each expected result.

# ------------------------------------------------------------------------------
# make_pbp_w12()
#
# One team ("KC"), one week, three skill players + QB.
# Per week:
#   WR1 (P001): 10 targets, 7 complete
#   TE1 (P002):  5 targets, 3 complete
#   RB1 (P003):  8 carries (rush), 5 targets, 4 complete (pass)
#   QB1 (Q001):  1 scramble (run, qb_scramble=1 -- NOT a carry)
#                1 sack    (run, sack=1          -- NOT a carry)
#
# Expected team totals:
#   team_targets_pass = 10 + 5 + 5 = 20
#   team_carries (non-sack, non-scramble) = 8 (RB1 only; QB scramble excluded)
#   team_receptions = 7 + 3 + 4 = 14
#   n_team_plays (pass + run) = 20 + 8 + 1 + 1 = 30
#
# Expected usage shares:
#   WR1: target share = 10/20 = 0.500
#   TE1: target share = 5/20  = 0.250
#   RB1: touch share  = (8+4) / (8+14) = 12/22 = 0.5455
# ------------------------------------------------------------------------------
make_pbp_w12 <- function(season = 2023L, week = 1L, team = "KC") {

  # WR1 targets
  wr_targets <- tibble::tibble(
    season              = season, week = week,
    game_id             = paste0(season, "_WK", week),
    play_type           = "pass",  posteam = team,
    rusher_player_id    = NA_character_, rusher_player_name  = NA_character_,
    receiver_player_id  = "P001",        receiver_player_name = "WR1",
    complete_pass       = c(rep(1L, 7L), rep(0L, 3L)),
    sack = 0L, qb_scramble = 0L
  )

  # TE1 targets
  te_targets <- tibble::tibble(
    season              = season, week = week,
    game_id             = paste0(season, "_WK", week),
    play_type           = "pass",  posteam = team,
    rusher_player_id    = NA_character_, rusher_player_name  = NA_character_,
    receiver_player_id  = "P002",        receiver_player_name = "TE1",
    complete_pass       = c(rep(1L, 3L), rep(0L, 2L)),
    sack = 0L, qb_scramble = 0L
  )

  # RB1 pass targets
  rb_targets <- tibble::tibble(
    season              = season, week = week,
    game_id             = paste0(season, "_WK", week),
    play_type           = "pass",  posteam = team,
    rusher_player_id    = NA_character_, rusher_player_name  = NA_character_,
    receiver_player_id  = "P003",        receiver_player_name = "RB1",
    complete_pass       = c(rep(1L, 4L), rep(0L, 1L)),
    sack = 0L, qb_scramble = 0L
  )

  # RB1 carries (8 run plays)
  rb_carries <- tibble::tibble(
    season              = season, week = week,
    game_id             = paste0(season, "_WK", week),
    play_type           = "run",   posteam = team,
    rusher_player_id    = "P003",  rusher_player_name  = "RB1",
    receiver_player_id  = NA_character_, receiver_player_name = NA_character_,
    complete_pass       = 0L,
    sack = 0L, qb_scramble = 0L
  )[rep(1L, 8L), ]

  # QB scramble (play_type = "run", qb_scramble = 1 -- NOT a carry)
  qb_scramble <- tibble::tibble(
    season              = season, week = week,
    game_id             = paste0(season, "_WK", week),
    play_type           = "run",   posteam = team,
    rusher_player_id    = "Q001",  rusher_player_name  = "QB1",
    receiver_player_id  = NA_character_, receiver_player_name = NA_character_,
    complete_pass       = 0L,
    sack = 0L, qb_scramble = 1L
  )

  # Sack play (play_type = "run", sack = 1 -- NOT a carry; nflfastR domain fact)
  sack_play <- tibble::tibble(
    season              = season, week = week,
    game_id             = paste0(season, "_WK", week),
    play_type           = "run",   posteam = team,
    rusher_player_id    = NA_character_, rusher_player_name  = NA_character_,
    receiver_player_id  = NA_character_, receiver_player_name = NA_character_,
    complete_pass       = 0L,
    sack = 1L, qb_scramble = 0L
  )

  dplyr::bind_rows(wr_targets, te_targets, rb_targets,
                   rb_carries, qb_scramble, sack_play)
}

# Roster with 3 skill players for make_pbp_w12()
make_roster_w12 <- function(season = 2023L) {
  tibble::tibble(
    player_id = c("P001", "P002", "P003"),
    season    = season,
    position  = c("WR", "TE", "RB")
  )
}

# ------------------------------------------------------------------------------
# make_usage_w12()
#
# Pre-built weekly_usage tibble covering 6 known player scenarios across
# season 2023, weeks 1-8.
#
# Expected identify_usage_ramps() results (threshold = 0.10, floor = 3/4):
#   P_RAMP : early=0.10, late=0.18, rel_change=+0.80 → treatment
#   P_FLAT : early=0.20, late=0.20, rel_change= 0.00 → control
#   P_DEC  : early=0.25, late=0.15, rel_change=-0.40 → declining
#   P_EXCL : n_early_active=2 (weeks 1,4 only) < floor 3  → excluded
#   P_FLOOR: early_avg=0.005 < MIN_USAGE_FLOOR_W12=0.01   → excluded
#   P_RB   : early=0.15, late=0.28, rel_change=+0.867     → treatment
# ------------------------------------------------------------------------------
make_usage_w12 <- function() {

  s <- 2023L

  make_weeks <- function(pid, pname, pos, team, weeks, shares) {
    tibble::tibble(
      player_id              = pid,
      player_name            = pname,
      season                 = s,
      week                   = as.integer(weeks),
      team                   = team,
      position               = pos,
      usage_share            = shares,
      usage_numerator        = as.integer(round(shares * 20)),
      team_usage_denominator = 20L,
      n_team_plays           = 30L,
      active                 = shares > 0
    )
  }

  dplyr::bind_rows(
    make_weeks("P_RAMP",  "Adams",    "WR", "KC", 1:8,
      c(0.10, 0.10, 0.10, 0.10, 0.18, 0.18, 0.18, 0.18)),
    make_weeks("P_FLAT",  "Hill",     "WR", "KC", 1:8,
      rep(0.20, 8)),
    make_weeks("P_DEC",   "Smith",    "WR", "SF", 1:8,
      c(0.25, 0.25, 0.25, 0.25, 0.15, 0.15, 0.15, 0.15)),
    # P_EXCL: absent in weeks 2 and 3 (only 2 active in early window)
    make_weeks("P_EXCL",  "Jones",    "WR", "SF", c(1L, 4L, 5L, 6L, 7L, 8L),
      c(0.10, 0.12, 0.15, 0.15, 0.15, 0.15)),
    # P_FLOOR: early_avg = 0.005, below MIN_USAGE_FLOOR_W12
    make_weeks("P_FLOOR", "Brown",    "TE", "KC", 1:8,
      c(0.005, 0.005, 0.005, 0.005, 0.012, 0.012, 0.012, 0.012)),
    make_weeks("P_RB",    "Williams", "RB", "KC", 1:8,
      c(0.15, 0.15, 0.15, 0.15, 0.28, 0.28, 0.28, 0.28))
  )
}

# ------------------------------------------------------------------------------
# make_groups_w12()
#
# Pre-built classify_treatment_control_ramp() output for effect-analysis tests.
# 20 treatment + 20 control, season 2023.
# ------------------------------------------------------------------------------
make_groups_w12 <- function() {

  trt <- tibble::tibble(
    player_id      = paste0("TRT_", 1:20),
    player_name    = paste0("TRT_P", 1:20),
    season         = 2023L,
    team           = "KC",
    position       = "WR",
    early_avg      = 0.12,
    late_avg       = 0.18,
    n_early_active = 3L,
    n_late_active  = 3L,
    relative_change = 0.50,
    ramp_flag      = TRUE,
    ramp_method    = "sub_window",
    games_floor_met = TRUE,
    group          = "treatment",
    era            = "modern"
  )

  ctl <- tibble::tibble(
    player_id      = paste0("CTL_", 1:20),
    player_name    = paste0("CTL_P", 1:20),
    season         = 2023L,
    team           = "SF",
    position       = "WR",
    early_avg      = 0.22,
    late_avg       = 0.22,
    n_early_active = 3L,
    n_late_active  = 3L,
    relative_change = 0.00,
    ramp_flag      = FALSE,
    ramp_method    = "sub_window",
    games_floor_met = TRUE,
    group          = "control",
    era            = "modern"
  )

  dplyr::bind_rows(trt, ctl)
}

# ------------------------------------------------------------------------------
# make_weekly_fantasy_w12()
#
# Pre-built weekly fantasy scoring for effect tests.
# Treatment: outcome weeks PPG seeded at 9.5 (lower)
# Control:   outcome weeks PPG seeded at 12.0 (higher)
# Deterministic via set.seed(42L).
# ------------------------------------------------------------------------------
make_weekly_fantasy_w12 <- function() {
  set.seed(42L)

  trt_fantasy <- purrr::map_dfr(paste0("TRT_", 1:20), function(pid) {
    tibble::tibble(
      player_id           = pid,
      season              = 2023L,
      week                = 9L:13L,
      game_id             = paste0("G_", 9L:13L),
      player_name         = pid,
      position            = "WR",
      team                = "KC",
      total_fantasy_points = stats::rnorm(5L, 9.5, 5.0)
    )
  })

  ctl_fantasy <- purrr::map_dfr(paste0("CTL_", 1:20), function(pid) {
    tibble::tibble(
      player_id           = pid,
      season              = 2023L,
      week                = 9L:13L,
      game_id             = paste0("G_", 9L:13L),
      player_name         = pid,
      position            = "WR",
      team                = "SF",
      total_fantasy_points = stats::rnorm(5L, 12.0, 5.5)
    )
  })

  dplyr::bind_rows(trt_fantasy, ctl_fantasy)
}

# ------------------------------------------------------------------------------
# make_weekly_usage_outcome_w12()
#
# Pre-built outcome-window (weeks 9-18) usage for persistence tests.
# Treatment: outcome usage seeded at 0.17 (lower than early start of 0.22)
# Control:   outcome usage seeded at 0.22 (stable)
# ------------------------------------------------------------------------------
make_weekly_usage_outcome_w12 <- function() {
  set.seed(99L)

  trt_usage <- purrr::map_dfr(paste0("TRT_", 1:20), function(pid) {
    tibble::tibble(
      player_id              = pid,
      player_name            = pid,
      season                 = 2023L,
      week                   = 9L:13L,
      team                   = "KC",
      position               = "WR",
      usage_share            = pmax(0, stats::rnorm(5L, 0.17, 0.05)),
      usage_numerator        = 5L,
      team_usage_denominator = 30L,
      n_team_plays           = 40L,
      active                 = TRUE
    )
  })

  ctl_usage <- purrr::map_dfr(paste0("CTL_", 1:20), function(pid) {
    tibble::tibble(
      player_id              = pid,
      player_name            = pid,
      season                 = 2023L,
      week                   = 9L:13L,
      team                   = "SF",
      position               = "WR",
      usage_share            = pmax(0, stats::rnorm(5L, 0.22, 0.05)),
      usage_numerator        = 6L,
      team_usage_denominator = 30L,
      n_team_plays           = 40L,
      active                 = TRUE
    )
  })

  dplyr::bind_rows(trt_usage, ctl_usage)
}

# ------------------------------------------------------------------------------
# make_roster_exp_w12()
#
# Roster with years_exp column for identify_rookies() tests.
# P_ROOK (years_exp=0): rookie, is_rookie should be TRUE
# P_SOPH (years_exp=1): sophomore, is_rookie should be FALSE
# P_VET  (years_exp=5): veteran, is_rookie should be FALSE
# ------------------------------------------------------------------------------
make_roster_exp_w12 <- function() {
  tibble::tibble(
    gsis_id   = c("P_ROOK", "P_SOPH", "P_VET"),
    season    = 2023L,
    years_exp = c(0L, 1L, 5L)
  )
}


# ==============================================================================
# SECTION B: .compute_weekly_usage_season()
# ==============================================================================

test_that("B1: returns tibble with required columns", {
  pbp     <- make_pbp_w12()
  roster  <- make_roster_w12()
  result  <- .compute_weekly_usage_season(pbp, roster)
  expect_s3_class(result, "tbl_df")
  expected_cols <- c("player_id", "player_name", "season", "week", "team",
                     "position", "usage_share", "usage_numerator",
                     "team_usage_denominator", "n_team_plays", "active")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("B2: WR target share = player targets / team targets (10/20 = 0.50)", {
  result  <- .compute_weekly_usage_season(make_pbp_w12(), make_roster_w12())
  wr1     <- result[result$player_id == "P001", ]
  expect_equal(nrow(wr1), 1L)
  expect_equal(wr1$usage_numerator,        10L)
  expect_equal(wr1$team_usage_denominator, 20L)
  expect_equal(wr1$usage_share,            0.5, tolerance = 1e-6)
})

test_that("B3: TE target share = 5/20 = 0.25", {
  result <- .compute_weekly_usage_season(make_pbp_w12(), make_roster_w12())
  te1    <- result[result$player_id == "P002", ]
  expect_equal(te1$usage_share, 0.25, tolerance = 1e-6)
})

test_that("B4: RB touch share = (carries + receptions) / (team_carries + team_receptions) = 12/22", {
  # team_carries = 8 (non-sack, non-scramble), team_receptions = 7+3+4 = 14
  # RB1: carries=8, receptions=4, numerator=12, denominator=22
  result <- .compute_weekly_usage_season(make_pbp_w12(), make_roster_w12())
  rb1    <- result[result$player_id == "P003", ]
  expect_equal(rb1$usage_numerator,        12L)
  expect_equal(rb1$team_usage_denominator, 22L)
  expect_equal(rb1$usage_share, 12 / 22, tolerance = 1e-6)
})

test_that("B5: sack plays excluded from team carries (sack=1 is NOT a carry)", {
  # If sack counted: team_carries would be 9 instead of 8, changing RB touch share
  result    <- .compute_weekly_usage_season(make_pbp_w12(), make_roster_w12())
  rb1       <- result[result$player_id == "P003", ]
  # If sack were included, denominator would be (9+14)=23, share=12/23
  expect_false(isTRUE(all.equal(rb1$usage_share, 12 / 23, tolerance = 1e-4)))
  expect_equal(rb1$team_usage_denominator, 22L)
})

test_that("B6: QB scrambles excluded from team carries (qb_scramble=1 is NOT a carry)", {
  # If scramble counted: team_carries would be 9, denominator=23
  result <- .compute_weekly_usage_season(make_pbp_w12(), make_roster_w12())
  rb1    <- result[result$player_id == "P003", ]
  expect_equal(rb1$team_usage_denominator, 22L)
})

test_that("B7: quality filter excludes team-weeks with too few plays", {
  # Build a minimal PBP with fewer than MIN_TEAM_PLAYS_W12 (15) plays
  pbp_small <- make_pbp_w12() %>%
    dplyr::slice_head(n = 10L)  # Only 10 plays, below floor of 15
  result <- .compute_weekly_usage_season(pbp_small, make_roster_w12())
  expect_equal(nrow(result), 0L)
})

test_that("B8: no duplicate player-week rows in output (many-to-many guard)", {
  result <- .compute_weekly_usage_season(make_pbp_w12(), make_roster_w12())
  dupes  <- result %>%
    dplyr::group_by(player_id, season, week) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1L)
  expect_equal(nrow(dupes), 0L,
    info = "Duplicate player-week rows found. Apply the player_carries/player_pass deduplication fix to .compute_weekly_usage_season().")
})

test_that("B9: empty PBP returns empty tibble", {
  empty_pbp <- make_pbp_w12()[0L, ]
  result    <- .compute_weekly_usage_season(empty_pbp, make_roster_w12())
  expect_equal(nrow(result), 0L)
})

test_that("B10: missing required PBP column stops with informative message", {
  bad_pbp <- make_pbp_w12() %>% dplyr::select(-sack)
  expect_error(
    .compute_weekly_usage_season(bad_pbp, make_roster_w12()),
    regexp = "sack"
  )
})


# ==============================================================================
# SECTION C: .bootstrap_ramp_diff_ci()
# ==============================================================================

test_that("C1: returns named list with required elements", {
  result <- .bootstrap_ramp_diff_ci(rnorm(30, 5), rnorm(30, 3), B = 100L)
  expect_type(result, "list")
  expect_true(all(c("estimate", "ci_lower", "ci_upper", "n_a", "n_b", "method")
                  %in% names(result)))
})

test_that("C2: estimate equals mean(a) - mean(b)", {
  set.seed(1L)
  a <- c(10.0, 12.0, 14.0, 11.0, 13.0)
  b <- c(7.0,   8.0,  9.0,  6.0,  7.0)
  result <- .bootstrap_ramp_diff_ci(a, b, B = 200L)
  expect_equal(result$estimate, mean(a) - mean(b), tolerance = 1e-10)
})

test_that("C3: 95% CI contains the estimate", {
  set.seed(42L)
  result <- .bootstrap_ramp_diff_ci(rnorm(50, 10, 2), rnorm(50, 8, 2), B = 500L)
  expect_true(result$ci_lower <= result$estimate)
  expect_true(result$ci_upper >= result$estimate)
})

test_that("C4: falls back to Welch t for very small groups (n < 10)", {
  result <- .bootstrap_ramp_diff_ci(c(5.0, 7.0, 6.0), c(3.0, 4.0, 3.5),
                                     B = 200L)
  expect_true(result$method %in% c("welch_t", "welch_t_failed"))
})

test_that("C5: NA values in inputs are excluded from computation", {
  a <- c(10.0, NA, 12.0, 11.0)
  b <- c(8.0, 9.0, NA, 7.0)
  result <- .bootstrap_ramp_diff_ci(a, b, B = 100L)
  expect_equal(result$n_a, 3L)
  expect_equal(result$n_b, 3L)
  expect_false(is.na(result$estimate))
})


# ==============================================================================
# SECTION D: identify_usage_ramps()
# ==============================================================================

test_that("D1: returns tibble with required columns", {
  usage  <- make_usage_w12()
  result <- identify_usage_ramps(usage, verbose = FALSE)
  expect_s3_class(result, "tbl_df")
  expected_cols <- c("player_id", "player_name", "season", "team", "position",
                     "early_avg", "late_avg", "n_early_active", "n_late_active",
                     "relative_change", "ramp_flag", "ramp_method",
                     "games_floor_met")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("D2: P_RAMP correctly flagged as ramp (rel_change = 0.80 > 0.10)", {
  result <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  ramp_row <- result[result$player_id == "P_RAMP", ]
  expect_true(ramp_row$ramp_flag)
  expect_true(ramp_row$games_floor_met)
  expect_equal(ramp_row$early_avg, 0.10, tolerance = 1e-6)
  expect_equal(ramp_row$late_avg,  0.18, tolerance = 1e-6)
  expect_equal(ramp_row$relative_change, 0.80, tolerance = 1e-6)
})

test_that("D3: P_FLAT not flagged as ramp (rel_change = 0.0)", {
  result   <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  flat_row <- result[result$player_id == "P_FLAT", ]
  expect_false(flat_row$ramp_flag)
  expect_true(flat_row$games_floor_met)
  expect_equal(flat_row$relative_change, 0.0, tolerance = 1e-6)
})

test_that("D4: P_DEC not flagged as ramp (rel_change = -0.40)", {
  result  <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  dec_row <- result[result$player_id == "P_DEC", ]
  expect_false(dec_row$ramp_flag)
  expect_true(dec_row$games_floor_met)
  expect_equal(dec_row$relative_change, -0.40, tolerance = 1e-6)
})

test_that("D5: P_EXCL has games_floor_met = FALSE (only 2 active early weeks)", {
  result   <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  excl_row <- result[result$player_id == "P_EXCL", ]
  expect_false(excl_row$games_floor_met)
  expect_equal(excl_row$n_early_active, 2L)
})

test_that("D6: P_FLOOR excluded due to early_avg < MIN_USAGE_FLOOR_W12", {
  result    <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  floor_row <- result[result$player_id == "P_FLOOR", ]
  expect_false(floor_row$games_floor_met)
  expect_true(is.na(floor_row$relative_change))
})

test_that("D7: relative_change computed correctly as (late - early) / early", {
  result  <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  rb_row  <- result[result$player_id == "P_RB", ]
  # P_RB: early=0.15, late=0.28, rel_change = (0.28-0.15)/0.15 = 0.8667
  expect_equal(rb_row$relative_change, (0.28 - 0.15) / 0.15, tolerance = 1e-4)
  expect_true(rb_row$ramp_flag)
})

test_that("D8: full_split method includes more players than sub_window (relaxed floor)", {
  usage      <- make_usage_w12()
  sub_result <- identify_usage_ramps(usage, method = "sub_window", verbose = FALSE)
  full_result <- identify_usage_ramps(usage, method = "full_split", verbose = FALSE)
  # full_split has floor=1, so P_EXCL should meet the floor
  excl_sub  <- sub_result[sub_result$player_id   == "P_EXCL", ]$games_floor_met
  excl_full <- full_result[full_result$player_id == "P_EXCL", ]$games_floor_met
  expect_false(excl_sub)
  expect_true(excl_full)
})

test_that("D9: empty usage input returns empty tibble", {
  result <- identify_usage_ramps(make_usage_w12()[0L, ], verbose = FALSE)
  expect_equal(nrow(result), 0L)
})

test_that("D10: missing required column stops with informative message", {
  bad_usage <- make_usage_w12() %>% dplyr::select(-usage_share)
  expect_error(
    identify_usage_ramps(bad_usage, verbose = FALSE),
    regexp = "usage_share"
  )
})

test_that("D11: ramp_method column reflects method argument", {
  sub_result  <- identify_usage_ramps(make_usage_w12(),
                                       method = "sub_window", verbose = FALSE)
  full_result <- identify_usage_ramps(make_usage_w12(),
                                       method = "full_split", verbose = FALSE)
  expect_true(all(sub_result$ramp_method  == "sub_window"))
  expect_true(all(full_result$ramp_method == "full_split"))
})

test_that("D12: custom threshold changes group assignments", {
  usage   <- make_usage_w12()
  # P_FLAT has relative_change = 0. With threshold = 0 it should be treatment (0 > 0 is FALSE).
  # P_RAMP has relative_change = 0.80. With threshold = 0.90 it should NOT be flagged.
  strict  <- identify_usage_ramps(usage, ramp_threshold = 0.90, verbose = FALSE)
  ramp_row <- strict[strict$player_id == "P_RAMP", ]
  expect_false(ramp_row$ramp_flag)  # 0.80 < 0.90 threshold, not flagged
})


# ==============================================================================
# SECTION E: classify_treatment_control_ramp()
# ==============================================================================

test_that("E1: treatment group assigned to ramp players meeting floor", {
  ramp_flags <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  result     <- classify_treatment_control_ramp(ramp_flags, verbose = FALSE)
  trt_ids    <- result$player_id[result$group == "treatment"]
  expect_true("P_RAMP" %in% trt_ids)
  expect_true("P_RB"   %in% trt_ids)
})

test_that("E2: control group assigned to flat players meeting floor", {
  ramp_flags <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  result     <- classify_treatment_control_ramp(ramp_flags, verbose = FALSE)
  expect_true("P_FLAT" %in% result$player_id[result$group == "control"])
})

test_that("E3: declining group assigned to declining players meeting floor", {
  ramp_flags <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  result     <- classify_treatment_control_ramp(ramp_flags, verbose = FALSE)
  expect_true("P_DEC" %in% result$player_id[result$group == "declining"])
})

test_that("E4: excluded group assigned to players not meeting floor", {
  ramp_flags <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  result     <- classify_treatment_control_ramp(ramp_flags, verbose = FALSE)
  excl_ids   <- result$player_id[result$group == "excluded"]
  expect_true("P_EXCL"  %in% excl_ids)
  expect_true("P_FLOOR" %in% excl_ids)
})

test_that("E5: era column set correctly (season < ERA_BREAKPOINT_W12 = early)", {
  ramp_flags <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  # season 2023 >= 2017 ERA_BREAKPOINT_W12 → modern
  result <- classify_treatment_control_ramp(ramp_flags, verbose = FALSE)
  expect_true(all(result$era == "modern"))
  # Verify early era with old season
  old_flags  <- ramp_flags %>% dplyr::mutate(season = 2014L)
  old_result <- classify_treatment_control_ramp(old_flags, verbose = FALSE)
  expect_true(all(old_result$era == "early"))
})

test_that("E6: group counts sum to total input rows", {
  ramp_flags <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  result     <- classify_treatment_control_ramp(ramp_flags, verbose = FALSE)
  expect_equal(nrow(result), nrow(ramp_flags))
})

test_that("E7: custom threshold changes treatment/control boundary", {
  ramp_flags <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  # With threshold = 0.05, P_RAMP (0.80) is still treatment but boundary shifts
  # P_DEC (rel_change = -0.40) is still declining
  result <- classify_treatment_control_ramp(ramp_flags,
                                             ramp_threshold = 0.05,
                                             verbose = FALSE)
  expect_true("P_RAMP" %in% result$player_id[result$group == "treatment"])
})

test_that("E8: empty ramp_flags input returns empty tibble", {
  empty_flags <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)[0L, ]
  result      <- classify_treatment_control_ramp(empty_flags, verbose = FALSE)
  expect_equal(nrow(result), 0L)
})


# ==============================================================================
# SECTION F: check_balance_ramp_groups()
# ==============================================================================

test_that("F1: returns list with required elements", {
  groups <- make_groups_w12()
  result <- check_balance_ramp_groups(groups, verbose = FALSE)
  expect_type(result, "list")
  expected_elements <- c("position_table", "era_table", "usage_smd",
                         "season_table", "n_by_group", "balance_flags",
                         "summary")
  expect_true(all(expected_elements %in% names(result)))
})

test_that("F2: SMD is non-negative", {
  result <- check_balance_ramp_groups(make_groups_w12(), verbose = FALSE)
  smd_val <- result$usage_smd$smd
  expect_true(is.na(smd_val) || smd_val >= 0)
})

test_that("F3: position_table has group and position columns", {
  result <- check_balance_ramp_groups(make_groups_w12(), verbose = FALSE)
  expect_true("group"    %in% names(result$position_table))
  expect_true("position" %in% names(result$position_table))
  expect_true("pct"      %in% names(result$position_table))
})

test_that("F4: balance_flags concern flag fires when SMD > 0.25", {
  # make_groups_w12() uses constant early_avg per group (SD = 0, pooled_sd = NA).
  # Add variance so pooled_sd is computable. Means remain far apart (0.12 vs 0.22)
  # guaranteeing SMD >> 0.25.
  groups <- make_groups_w12() %>%
    dplyr::mutate(
      early_avg = dplyr::if_else(
        group == "treatment",
        0.12 + seq(-0.03, 0.03, length.out = dplyr::n()),
        0.22 + seq(-0.03, 0.03, length.out = dplyr::n())
      )
    )
  result  <- check_balance_ramp_groups(groups, verbose = FALSE)
  concern <- result$balance_flags$concern
  expect_true(any(concern, na.rm = TRUE))
})

test_that("F5: groups with no treatment/control rows handled gracefully", {
  excl_only <- tibble::tibble(
    player_id = "X1", season = 2023L, position = "WR",
    group = "excluded", era = "modern", early_avg = 0.10
  )
  result <- check_balance_ramp_groups(excl_only, verbose = FALSE)
  expect_type(result, "list")
})


# ==============================================================================
# SECTION G: validate_ramp_assumptions()
# ==============================================================================

test_that("G1: returns list with checks tibble and n_passed", {
  groups <- make_groups_w12() %>%
    dplyr::mutate(relative_change = ifelse(group == "treatment", 0.5, 0.0),
                  is_rookie = FALSE, years_exp = 3L, rookie_group = "vet_flat")
  usage  <- make_usage_w12()
  result <- validate_ramp_assumptions(groups, usage, verbose = FALSE)
  expect_type(result, "list")
  expect_true("checks"   %in% names(result))
  expect_true("n_passed" %in% names(result))
  expect_s3_class(result$checks, "tbl_df")
  expect_true("result" %in% names(result$checks))
})

test_that("G2: all-passing scenario returns n_passed = 5", {
  groups <- make_groups_w12() %>%
    dplyr::mutate(relative_change = ifelse(group == "treatment", 0.5, 0.0))
  # Add years for multi-season spread
  groups <- dplyr::bind_rows(
    lapply(2010L:2023L, function(s) dplyr::mutate(groups, season = s))
  ) %>% dplyr::mutate(era = ifelse(season < 2017L, "early", "modern"))

  usage <- dplyr::bind_rows(
    lapply(2010L:2023L, function(s) dplyr::mutate(make_usage_w12(), season = s))
  )
  result <- validate_ramp_assumptions(groups, usage, verbose = FALSE)
  expect_equal(result$n_passed, 5L)
})

test_that("G3: check results tibble has 5 rows (one per check)", {
  result <- validate_ramp_assumptions(make_groups_w12(), make_usage_w12(),
                                       verbose = FALSE)
  expect_equal(nrow(result$checks), 5L)
})

test_that("G4: season spread check fails when only one season represented", {
  single_season_groups <- make_groups_w12()
  # Groups only have season 2023 -- span = 1 season < 5 required
  result <- validate_ramp_assumptions(
    single_season_groups, make_usage_w12(), verbose = FALSE
  )
  season_check <- result$checks[result$checks$check ==
                                  "Both groups span >= 5 seasons", ]
  expect_equal(nrow(season_check), 1L)
  # Season check should fail (only 1 season)
  expect_false(season_check$result)
})

test_that("G5: n_passed is integer", {
  result <- validate_ramp_assumptions(make_groups_w12(), make_usage_w12(),
                                       verbose = FALSE)
  expect_type(result$n_passed, "integer")
})


# ==============================================================================
# SECTION H: identify_rookies()
# ==============================================================================

test_that("H1: years_exp == 0 sets is_rookie = TRUE", {
  groups  <- make_groups_w12() %>%
    dplyr::mutate(
      player_id = c(paste0("TRT_", 1:20), paste0("CTL_", 1:20))
    )
  roster  <- tibble::tibble(
    gsis_id   = groups$player_id,
    season    = 2023L,
    years_exp = c(rep(0L, 20L), rep(3L, 20L))   # TRT are all rookies
  )
  result <- identify_rookies(groups, roster, verbose = FALSE)
  trt_rookies <- result$is_rookie[result$group == "treatment"]
  ctl_rookies <- result$is_rookie[result$group == "control"]
  expect_true(all(trt_rookies))
  expect_false(any(ctl_rookies))
})

test_that("H2: years_exp > 0 sets is_rookie = FALSE", {
  groups <- make_groups_w12() %>% dplyr::slice_head(n = 1L)
  roster <- tibble::tibble(gsis_id = groups$player_id,
                            season = 2023L, years_exp = 4L)
  result <- identify_rookies(groups, roster, verbose = FALSE)
  expect_false(result$is_rookie[[1L]])
})

test_that("H3: gsis_id column renamed to player_id automatically", {
  groups <- make_groups_w12()
  roster <- make_roster_exp_w12()   # has gsis_id column
  # Only use players that appear in groups
  roster_for_groups <- tibble::tibble(
    gsis_id   = groups$player_id,
    season    = 2023L,
    years_exp = rep(0L, nrow(groups))
  )
  # Should not error -- gsis_id renamed internally
  expect_no_error(
    identify_rookies(groups, roster_for_groups, verbose = FALSE)
  )
})

test_that("H4: player missing from roster gets is_rookie = FALSE (not NA)", {
  # When years_exp is NA after the left join, the expression
  # !is.na(NA) & NA == 0L evaluates to FALSE & NA = FALSE.
  # Unknown experience is conservatively treated as not a rookie.
  groups <- make_groups_w12() %>% dplyr::slice_head(n = 2L)
  roster <- tibble::tibble(
    gsis_id   = groups$player_id[[1L]],
    season    = 2023L,
    years_exp = 0L
  )
  result <- identify_rookies(groups, roster, verbose = FALSE)
  expect_false(result$is_rookie[[2L]])
})

test_that("H5: entry_year fallback works when years_exp absent", {
  groups <- make_groups_w12() %>% dplyr::slice_head(n = 1L)
  roster <- tibble::tibble(
    gsis_id    = groups$player_id,
    season     = 2023L,
    entry_year = 2023L   # season - entry_year = 0 → rookie
  )
  result <- identify_rookies(groups, roster, verbose = FALSE)
  expect_true(result$is_rookie[[1L]])
})

test_that("H6: roster with neither years_exp nor entry_year stops with message", {
  groups <- make_groups_w12() %>% dplyr::slice_head(n = 1L)
  roster <- tibble::tibble(gsis_id = groups$player_id, season = 2023L)
  expect_error(
    identify_rookies(groups, roster, verbose = FALSE),
    regexp = "years_exp"
  )
})

test_that("H7: years_exp column retained in output", {
  groups <- make_groups_w12()
  roster <- tibble::tibble(
    gsis_id   = groups$player_id,
    season    = 2023L,
    years_exp = 0L
  )
  result <- identify_rookies(groups, roster, verbose = FALSE)
  expect_true("years_exp" %in% names(result))
})


# ==============================================================================
# SECTION I: stratify_by_rookie_status()
# ==============================================================================

# Build a groups tibble with is_rookie added
make_groups_with_rookies <- function() {
  groups <- make_groups_w12()
  groups$is_rookie  <- c(rep(TRUE, 5L), rep(FALSE, 15L),
                          rep(TRUE, 5L), rep(FALSE, 15L))
  groups$years_exp  <- c(rep(0L, 5L), rep(3L, 15L),
                          rep(0L, 5L), rep(3L, 15L))
  groups
}

test_that("I1: treatment + rookie → rookie_ramp", {
  result    <- stratify_by_rookie_status(make_groups_with_rookies(),
                                          verbose = FALSE)
  rook_ramp <- result$player_id[result$rookie_group == "rookie_ramp"]
  expect_true(all(result$group[result$player_id %in% rook_ramp] == "treatment"))
  expect_true(all(result$is_rookie[result$player_id %in% rook_ramp]))
})

test_that("I2: treatment + not rookie → vet_ramp", {
  result   <- stratify_by_rookie_status(make_groups_with_rookies(),
                                         verbose = FALSE)
  vet_ramp <- result$player_id[result$rookie_group == "vet_ramp"]
  expect_true(all(result$group[result$player_id %in% vet_ramp] == "treatment"))
  expect_false(any(result$is_rookie[result$player_id %in% vet_ramp]))
})

test_that("I3: control + rookie → rookie_flat", {
  result    <- stratify_by_rookie_status(make_groups_with_rookies(),
                                          verbose = FALSE)
  rook_flat <- result$player_id[result$rookie_group == "rookie_flat"]
  expect_true(all(result$group[result$player_id %in% rook_flat] == "control"))
  expect_true(all(result$is_rookie[result$player_id %in% rook_flat]))
})

test_that("I4: control + not rookie → vet_flat", {
  result   <- stratify_by_rookie_status(make_groups_with_rookies(),
                                         verbose = FALSE)
  vet_flat <- result$player_id[result$rookie_group == "vet_flat"]
  expect_true(all(result$group[result$player_id %in% vet_flat] == "control"))
  expect_false(any(result$is_rookie[result$player_id %in% vet_flat]))
})

test_that("I5: declining group → excluded in rookie_group", {
  groups_with_dec <- make_groups_with_rookies() %>%
    dplyr::mutate(
      group     = dplyr::if_else(player_id == "TRT_1", "declining", group),
      is_rookie = dplyr::if_else(player_id == "TRT_1", TRUE, is_rookie)
    )
  result    <- stratify_by_rookie_status(groups_with_dec, verbose = FALSE)
  dec_row   <- result[result$player_id == "TRT_1", ]
  expect_equal(dec_row$rookie_group, "excluded")
})


# ==============================================================================
# SECTION J: calculate_usage_persistence()
# ==============================================================================

test_that("J1: returns list with required elements", {
  groups <- make_groups_w12()
  usage  <- make_weekly_usage_outcome_w12()
  result <- calculate_usage_persistence(groups, usage,
                                         outcome_weeks = 9L:13L,
                                         verbose = FALSE)
  expect_type(result, "list")
  expect_true(all(c("persistence_summary", "persistence_effect",
                    "cohen_d_usage", "summary") %in% names(result)))
})

test_that("J2: persistence_summary has one row per group (treatment and control)", {
  groups <- make_groups_w12()
  usage  <- make_weekly_usage_outcome_w12()
  result <- calculate_usage_persistence(groups, usage,
                                         outcome_weeks = 9L:13L,
                                         verbose = FALSE)
  expect_equal(nrow(result$persistence_summary), 2L)
  expect_true(all(c("treatment", "control") %in%
                    result$persistence_summary$group))
})

test_that("J3: persistence effect estimate sign matches mean_usage difference", {
  groups <- make_groups_w12()
  usage  <- make_weekly_usage_outcome_w12()
  result <- calculate_usage_persistence(groups, usage,
                                         outcome_weeks = 9L:13L,
                                         verbose = FALSE)
  trt_mean <- result$persistence_summary$mean_usage[
    result$persistence_summary$group == "treatment"]
  ctl_mean <- result$persistence_summary$mean_usage[
    result$persistence_summary$group == "control"]
  expected_sign <- sign(trt_mean - ctl_mean)
  actual_sign   <- sign(result$persistence_effect$estimate)
  expect_equal(actual_sign, expected_sign)
})


# ==============================================================================
# SECTION K: run_ramp_experiment()
# ==============================================================================

test_that("K1: returns list with required elements", {
  groups  <- make_groups_w12()
  fantasy <- make_weekly_fantasy_w12()
  usage   <- make_weekly_usage_outcome_w12()
  result  <- run_ramp_experiment(groups, fantasy, usage,
                                  outcome_weeks = 9L:13L,
                                  verbose = FALSE)
  expect_type(result, "list")
  expect_true(all(c("group_summary", "effect_ppg", "cohens_d_ppg",
                    "t_test_ppg", "usage_persistence", "summary") %in%
                    names(result)))
})

test_that("K2: group_summary has two rows (treatment and control)", {
  groups  <- make_groups_w12()
  fantasy <- make_weekly_fantasy_w12()
  usage   <- make_weekly_usage_outcome_w12()
  result  <- run_ramp_experiment(groups, fantasy, usage,
                                  outcome_weeks = 9L:13L,
                                  verbose = FALSE)
  expect_equal(nrow(result$group_summary), 2L)
})

test_that("K3: Cohen's d sign matches effect_ppg estimate sign", {
  groups  <- make_groups_w12()
  fantasy <- make_weekly_fantasy_w12()
  usage   <- make_weekly_usage_outcome_w12()
  result  <- run_ramp_experiment(groups, fantasy, usage,
                                  outcome_weeks = 9L:13L,
                                  verbose = FALSE)
  if (!is.na(result$effect_ppg$estimate) && !is.na(result$cohens_d_ppg)) {
    expect_equal(sign(result$cohens_d_ppg),
                 sign(result$effect_ppg$estimate))
  }
})


# ==============================================================================
# SECTION L: STATISTICAL ASSUMPTION TESTS
# (Non-negotiable per Season 2 testing standards)
# ==============================================================================

test_that("L1: usage_share in [0, 1] for all non-NA values", {
  result <- .compute_weekly_usage_season(make_pbp_w12(), make_roster_w12())
  valid  <- result$usage_share[!is.na(result$usage_share)]
  expect_true(all(valid >= 0))
  expect_true(all(valid <= 1))
})

test_that("L2: active flag equals (usage_numerator > 0) for all rows", {
  result <- .compute_weekly_usage_season(make_pbp_w12(), make_roster_w12())
  expect_equal(result$active, result$usage_numerator > 0L)
})

test_that("L3: team_usage_denominator >= usage_numerator for all non-NA usage_share rows", {
  result <- .compute_weekly_usage_season(make_pbp_w12(), make_roster_w12())
  valid  <- result[!is.na(result$usage_share), ]
  expect_true(all(valid$team_usage_denominator >= valid$usage_numerator))
})

test_that("L4: relative_change = (late_avg - early_avg) / early_avg for floor-met players", {
  result <- identify_usage_ramps(make_usage_w12(), verbose = FALSE)
  valid  <- result[result$games_floor_met & !is.na(result$relative_change), ]
  expected_rc <- (valid$late_avg - valid$early_avg) / valid$early_avg
  expect_equal(valid$relative_change, expected_rc, tolerance = 1e-6)
})

test_that("L5: bootstrap CI is wider for higher-variance data (statistical property)", {
  set.seed(42L)
  # Low variance data
  lo_var <- .bootstrap_ramp_diff_ci(rnorm(50, 10, 1), rnorm(50, 8, 1),
                                     B = 500L)
  # High variance data (same means, 5x the SD)
  hi_var <- .bootstrap_ramp_diff_ci(rnorm(50, 10, 5), rnorm(50, 8, 5),
                                     B = 500L)
  lo_width <- lo_var$ci_upper - lo_var$ci_lower
  hi_width <- hi_var$ci_upper - hi_var$ci_lower
  expect_true(hi_width > lo_width)
})
