# ==============================================================================
# tests/test_season2_week4_functions.R
# Season 2, Week 4: Recency Bias A/B Test -- Unit Test Suite
# NFL Analytics Toolkit
#
# PURPOSE
# Unit tests for all exported functions in R/18_predictive_validity_ab_testing.R.
# Uses synthetic data fixtures -- no live nflfastR downloads. Deterministic
# and network-independent. Run with:
#   testthat::test_file(here::here("tests", "test_season2_week4_functions.R"))
#
# FUNCTIONS TESTED
#   calculate_season_to_date_avg()     Tests T01-T06
#   build_ab_panel()                   Tests T07-T09  (input validation only)
#   compare_prediction_methods()       Tests T10-T16
#   calculate_effect_size_ab()         Tests T17-T19
#   power_analysis_correlation_diff()  Tests T20-T22
#   apply_fdr_correction()             Tests T23-T26
#   run_ab_test_pipeline()             Tests T27      (input validation only)
#
# SYNTHETIC DATA FIXTURES
#   make_fantasy_data()   Minimal fantasy scoring tibble for STD tests
#   make_panel()          Minimal A/B panel for comparison tests
#   make_results()        Minimal raw results tibble for FDR tests
#
# COVERAGE FOCUS
#   - cummean(lag()) NA propagation bug (the bug we fixed -- must stay fixed)
#   - Feature leakage: week N predictor never sees week N actual score
#   - STD expanding mean: first game of each stint is NA, rest non-NA
#   - p_value_log10 column present and non-zero
#   - Direction summary computed from data, not hardcoded
#   - BH correction: adjusted p >= raw p, significance flags correct
#   - Cohen's q: always non-negative, correct Fisher z math
#   - Power analysis: achieved power increases with n
#
# SCHEMA TAG: s2_ab_v1
# ==============================================================================

library(testthat)
library(dplyr)
library(here)

source(here::here("R", "15_multi_season_pbp.R"))
source(here::here("R", "05_consistency_metrics.R"))
source(here::here("R", "18_predictive_validity_ab_testing.R"))


# ==============================================================================
# SYNTHETIC DATA FIXTURES
# ==============================================================================

#' Minimal fantasy scoring tibble for calculate_season_to_date_avg() tests.
#' Three players, one season, clean weekly data -- no NA in total_fantasy_points.
#' Player C has two team stints (traded mid-season) to test window reset.
make_fantasy_data <- function() {
  tibble::tibble(
    player_id            = c(
      # Player A: 5 games, one team
      rep("PA", 5),
      # Player B: 3 games, one team
      rep("PB", 3),
      # Player C: traded -- 3 games TEN then 3 games BUF
      rep("PC", 3), rep("PC", 3)
    ),
    player_name          = c(rep("Player A", 5), rep("Player B", 3),
                             rep("Player C", 3), rep("Player C", 3)),
    season               = 2024L,
    week                 = c(1:5, 1:3, 1:3, 5:7),
    team                 = c(rep("GB", 5), rep("KC", 3),
                             rep("TEN", 3), rep("BUF", 3)),
    total_fantasy_points = c(
      10, 20, 15, 25, 30,   # Player A
      5,  8, 12,             # Player B
      18, 22, 16,            # Player C TEN
      9, 14, 20              # Player C BUF
    )
  )
}

#' Minimal A/B panel for compare_prediction_methods() tests.
#' 300 rows per position, predictors and outcomes drawn from correlated normals.
#' Seed fixed for reproducibility.
make_panel <- function(seed = 42L, n_per_pos = 300L) {
  set.seed(seed)
  positions <- c("QB", "RB", "WR")
  purrr::map_dfr(positions, function(pos) {
    # True outcome
    y        <- rnorm(n_per_pos, mean = 15, sd = 8)
    # roll3: moderate correlation with outcome (r ~ 0.45)
    x_roll3  <- 0.45 * y + rnorm(n_per_pos, sd = sqrt(1 - 0.45^2) * 8)
    # std: slightly higher correlation with outcome (r ~ 0.52)
    x_std    <- 0.52 * y + rnorm(n_per_pos, sd = sqrt(1 - 0.52^2) * 8)
    tibble::tibble(
      season               = rep(2024L, n_per_pos),
      week                 = sample(4:17, n_per_pos, replace = TRUE),
      player_id            = paste0(pos, "_", seq_len(n_per_pos)),
      player_name          = paste0(pos, "_player_", seq_len(n_per_pos)),
      position             = pos,
      team                 = sample(c("GB", "KC", "SF"), n_per_pos, replace = TRUE),
      fantasy_pts_actual   = y,
      fantasy_pts_roll3    = x_roll3,
      fantasy_pts_std      = x_std,
      games_played_prior   = sample(3L:16L, n_per_pos, replace = TRUE)
    )
  })
}

#' Minimal raw results tibble for apply_fdr_correction() tests.
make_results <- function() {
  tibble::tibble(
    position        = c("QB", "RB", "WR"),
    n_player_weeks  = c(1242L, 8123L, 44691L),
    n_seasons       = c(16L, 16L, 16L),
    r_roll3         = c(0.3647, 0.6227, 0.5080),
    r_std           = c(0.4311, 0.6520, 0.5425),
    r_roll3_ci_lo   = c(0.3155, 0.6081, 0.5002),
    r_roll3_ci_hi   = c(0.4145, 0.6359, 0.5149),
    r_std_ci_lo     = c(0.3852, 0.6381, 0.5351),
    r_std_ci_hi     = c(0.4776, 0.6639, 0.5492),
    z_stat          = c(-5.1234, -8.9012, -15.3456),
    p_value_raw     = c(1.5e-7, 2.3e-19, 9.1e-53),
    p_value_log10   = c(-6.8, -18.6, -52.0),
    cohens_q        = c(0.0789, 0.0493, 0.0477),
    better_predictor = c("std", "std", "std")
  )
}


# ==============================================================================
# TESTS: calculate_season_to_date_avg()
# ==============================================================================

test_that("T01: STD first game of each player-season-team stint is NA", {
  # The cummean(lag()) bug produced ALL-NA. After fix, only the first row
  # of each group should be NA -- all subsequent rows must be non-NA.
  d    <- make_fantasy_data()
  out  <- calculate_season_to_date_avg(d)

  first_rows <- out %>%
    arrange(player_id, season, team, week) %>%
    group_by(player_id, season, team) %>%
    slice(1) %>%
    ungroup()

  expect_true(
    all(is.na(first_rows$fantasy_pts_std)),
    label = "First row of each player-season-team group must be NA"
  )
})

test_that("T02: STD subsequent rows after first are non-NA", {
  d    <- make_fantasy_data()
  out  <- calculate_season_to_date_avg(d)

  non_first_rows <- out %>%
    arrange(player_id, season, team, week) %>%
    group_by(player_id, season, team) %>%
    slice(-1) %>%
    ungroup()

  expect_true(
    all(!is.na(non_first_rows$fantasy_pts_std)),
    label = "All rows after first in each group must have non-NA STD"
  )
})

test_that("T03: STD expanding mean is mathematically correct", {
  # Player A weeks 1-5: scores 10, 20, 15, 25, 30
  # Week 2 STD should = mean(10) = 10
  # Week 3 STD should = mean(10, 20) = 15
  # Week 4 STD should = mean(10, 20, 15) = 15
  # Week 5 STD should = mean(10, 20, 15, 25) = 17.5
  d   <- make_fantasy_data()
  out <- calculate_season_to_date_avg(d) %>%
    filter(player_id == "PA") %>%
    arrange(week)

  expect_equal(out$fantasy_pts_std[2], 10,   tolerance = 1e-6)
  expect_equal(out$fantasy_pts_std[3], 15,   tolerance = 1e-6)
  expect_equal(out$fantasy_pts_std[4], 15,   tolerance = 1e-6)
  expect_equal(out$fantasy_pts_std[5], 17.5, tolerance = 1e-6)
})

test_that("T04: STD resets at team change (traded player)", {
  # Player C: TEN weeks 1-3 (18, 22, 16), then BUF weeks 5-7 (9, 14, 20)
  # BUF week 5 (first BUF game) should be NA -- new stint resets
  # BUF week 6 STD should = 9 (only first BUF game in prior)
  d   <- make_fantasy_data()
  out <- calculate_season_to_date_avg(d) %>%
    filter(player_id == "PC") %>%
    arrange(team, week)

  buf_rows <- out %>% filter(team == "BUF") %>% arrange(week)

  expect_true(is.na(buf_rows$fantasy_pts_std[1]),
              label = "First BUF game of traded player must be NA")
  expect_equal(buf_rows$fantasy_pts_std[2], 9, tolerance = 1e-6,
               label = "Second BUF game STD should equal first BUF game score")
})

test_that("T05: STD does not leak current week into predictor", {
  # Leakage check: STD for week N must NOT equal total_fantasy_points for week N
  # (unless coincidental). We verify by checking week N score is excluded.
  # Player A week 5 actual = 30. STD at week 5 = mean(10,20,15,25) = 17.5, not 30.
  d   <- make_fantasy_data()
  out <- calculate_season_to_date_avg(d) %>%
    filter(player_id == "PA") %>%
    arrange(week)

  # STD at any week must not equal that week's actual score
  # (test the week-5 case explicitly where actual = 30)
  week5 <- out %>% filter(week == 5)
  expect_false(
    isTRUE(all.equal(week5$fantasy_pts_std, week5$total_fantasy_points)),
    label = "STD must not equal current week's actual score (leakage check)"
  )
})

test_that("T06: calculate_season_to_date_avg() row count unchanged", {
  d   <- make_fantasy_data()
  out <- calculate_season_to_date_avg(d)
  expect_equal(nrow(out), nrow(d),
               label = "Row count must be unchanged after STD calculation")
})


# ==============================================================================
# TESTS: build_ab_panel() -- input validation only (no live data)
# ==============================================================================

test_that("T07: build_ab_panel() rejects non-numeric seasons", {
  expect_error(
    build_ab_panel(seasons = "2024"),
    label = "Non-numeric seasons should error"
  )
})

test_that("T08: build_ab_panel() rejects empty seasons vector", {
  expect_error(
    build_ab_panel(seasons = integer(0)),
    label = "Empty seasons vector should error"
  )
})

test_that("T09: build_ab_panel() rejects invalid min_prior_games", {
  expect_error(
    build_ab_panel(seasons = 2024L, min_prior_games = 0L),
    label = "min_prior_games < 1 should error"
  )
})


# ==============================================================================
# TESTS: compare_prediction_methods()
# ==============================================================================
# NOTE: compare_prediction_methods() calls below use suppressWarnings().
# The BCa bootstrap CI (boot::boot.ci) warns "extreme order statistics used
# as endpoints" when n_bootstrap is low (100) and the synthetic dataset is
# small (n=300). This warning is expected with small synthetic fixtures and
# does not occur in production (n=44,691, bootstrap=2000). The underlying
# test assertions are unaffected -- the CI fallback still returns valid values.

test_that("T10: compare_prediction_methods() returns one row per position", {
  panel  <- make_panel()
  result <- suppressWarnings(
    compare_prediction_methods(panel, n_bootstrap = 100L, seed = 42L)
  )
  expect_equal(nrow(result), length(unique(panel$position)))
})

test_that("T11: compare_prediction_methods() output has required columns", {
  panel    <- make_panel()
  result   <- suppressWarnings(
    compare_prediction_methods(panel, n_bootstrap = 100L, seed = 42L)
  )
  required <- c("position", "n_player_weeks", "n_seasons",
                "r_roll3", "r_std", "r_roll3_ci_lo", "r_roll3_ci_hi",
                "r_std_ci_lo", "r_std_ci_hi", "z_stat", "p_value_raw",
                "p_value_log10", "cohens_q", "better_predictor")
  missing  <- setdiff(required, names(result))
  expect_equal(length(missing), 0L,
               label = paste("Missing columns:", paste(missing, collapse = ", ")))
})

test_that("T12: p_value_log10 is present and non-zero for all positions", {
  panel  <- make_panel()
  result <- suppressWarnings(
    compare_prediction_methods(panel, n_bootstrap = 100L, seed = 42L)
  )
  expect_true(all(!is.na(result$p_value_log10)),
              label = "p_value_log10 must be non-NA for all positions")
  expect_true(all(result$p_value_log10 < 0),
              label = "p_value_log10 must be negative (log10 of p < 1)")
})

test_that("T13: better_predictor is computed from data not hardcoded", {
  # Build a panel where roll3 is deliberately the better predictor
  set.seed(99L)
  n <- 300L
  y       <- rnorm(n, 15, 8)
  x_roll3 <- 0.65 * y + rnorm(n, sd = sqrt(1 - 0.65^2) * 8)  # higher r
  x_std   <- 0.40 * y + rnorm(n, sd = sqrt(1 - 0.40^2) * 8)  # lower r

  panel_roll3_wins <- tibble::tibble(
    season = 2024L, week = sample(4:17, n, replace = TRUE),
    player_id = paste0("P", seq_len(n)), player_name = paste0("P", seq_len(n)),
    position = "WR", team = "GB",
    fantasy_pts_actual = y, fantasy_pts_roll3 = x_roll3,
    fantasy_pts_std = x_std, games_played_prior = 5L
  )

  result <- suppressWarnings(
    compare_prediction_methods(
      panel_roll3_wins, n_bootstrap = 100L, seed = 42L
    )
  )
  expect_equal(result$better_predictor[result$position == "WR"], "roll3",
               label = "better_predictor must reflect actual data, not assume 'std'")
})

test_that("T14: correlations are in valid range (-1, 1)", {
  panel  <- make_panel()
  result <- suppressWarnings(
    compare_prediction_methods(panel, n_bootstrap = 100L, seed = 42L)
  )
  expect_true(all(result$r_roll3 > -1 & result$r_roll3 < 1))
  expect_true(all(result$r_std   > -1 & result$r_std   < 1))
})

test_that("T15: n_player_weeks matches input panel row count per position", {
  panel  <- make_panel(n_per_pos = 300L)
  result <- suppressWarnings(
    compare_prediction_methods(panel, n_bootstrap = 100L, seed = 42L)
  )
  for (pos in unique(panel$position)) {
    expected_n <- nrow(panel %>% filter(position == pos))
    actual_n   <- result %>% filter(position == pos) %>% pull(n_player_weeks)
    expect_equal(actual_n, as.integer(expected_n),
                 label = glue::glue("{pos}: n_player_weeks mismatch"))
  }
})

test_that("T16: compare_prediction_methods() errors on missing required columns", {
  panel <- make_panel() %>% select(-fantasy_pts_roll3)
  expect_error(
    compare_prediction_methods(panel, n_bootstrap = 100L),
    label = "Missing fantasy_pts_roll3 should error"
  )
})


# ==============================================================================
# TESTS: calculate_effect_size_ab()
# ==============================================================================

test_that("T17: Cohen's q is non-negative", {
  expect_gte(calculate_effect_size_ab(0.50, 0.40), 0)
  expect_gte(calculate_effect_size_ab(0.40, 0.50), 0)  # order should not matter
})

test_that("T18: Cohen's q is zero when correlations are equal", {
  expect_equal(calculate_effect_size_ab(0.50, 0.50), 0, tolerance = 1e-10)
})

test_that("T19: Cohen's q rejects invalid correlation values", {
  expect_error(calculate_effect_size_ab(1.0,  0.5), label = "r1 = 1.0 should error")
  expect_error(calculate_effect_size_ab(0.5, -1.0), label = "r2 = -1.0 should error")
  expect_error(calculate_effect_size_ab(NA,   0.5), label = "NA r1 should error")
})


# ==============================================================================
# TESTS: power_analysis_correlation_diff()
# ==============================================================================

test_that("T20: power_analysis_correlation_diff() returns required slots", {
  panel  <- make_panel()
  result <- power_analysis_correlation_diff(panel, mde_q = 0.10)
  expect_true("required_n"     %in% names(result))
  expect_true("achieved_power" %in% names(result))
})

test_that("T21: achieved power increases with larger n", {
  # Build two panels -- small n and large n -- and verify power is higher for large n
  panel_small <- make_panel(n_per_pos = 50L)
  panel_large <- make_panel(n_per_pos = 500L)

  power_small <- power_analysis_correlation_diff(panel_small, mde_q = 0.10)
  power_large <- power_analysis_correlation_diff(panel_large, mde_q = 0.10)

  # For each position, larger panel should have higher achieved power
  for (pos in unique(panel_small$position)) {
    p_small <- power_small$achieved_power %>%
      filter(position == pos) %>% pull(achieved_power)
    p_large <- power_large$achieved_power %>%
      filter(position == pos) %>% pull(achieved_power)
    expect_lt(p_small, p_large,
              label = glue::glue("{pos}: power should increase with larger n"))
  }
})

test_that("T22: required_n is a single positive number", {
  panel  <- make_panel()
  result <- power_analysis_correlation_diff(panel, mde_q = 0.10)
  req_n  <- result$required_n$required_n_per_group
  expect_length(req_n, 1L)
  expect_gt(req_n, 0)
})


# ==============================================================================
# TESTS: apply_fdr_correction()
# ==============================================================================

test_that("T23: apply_fdr_correction() adds p_value_adj and significant_adj columns", {
  result <- make_results()
  out    <- apply_fdr_correction(result)
  expect_true("p_value_adj"     %in% names(out))
  expect_true("significant_adj" %in% names(out))
})

test_that("T24: BH-adjusted p-values are >= raw p-values", {
  # BH correction can only increase p-values, never decrease them
  result <- make_results()
  out    <- apply_fdr_correction(result)
  expect_true(
    all(out$p_value_adj >= out$p_value_raw),
    label = "BH adjusted p must be >= raw p for all positions"
  )
})

test_that("T25: significant_adj flags are logical and consistent with p_value_adj", {
  result <- make_results()
  out    <- apply_fdr_correction(result)
  # significant_adj should be TRUE iff p_value_adj < 0.05
  expect_equal(
    out$significant_adj,
    out$p_value_adj < 0.05,
    label = "significant_adj must equal p_value_adj < 0.05"
  )
})

test_that("T26: apply_fdr_correction() errors on missing p_value_raw column", {
  result <- make_results() %>% select(-p_value_raw)
  expect_error(
    apply_fdr_correction(result),
    label = "Missing p_value_raw should error"
  )
})


# ==============================================================================
# TESTS: run_ab_test_pipeline() -- input validation only
# ==============================================================================

test_that("T27: run_ab_test_pipeline() errors when cache_dir does not exist", {
  expect_error(
    run_ab_test_pipeline(
      seasons   = 2024L,
      cache_dir = here::here("data", "nonexistent_cache_dir_xyz")
    ),
    label = "Non-existent cache_dir should error at build_ab_panel()"
  )
})


# ==============================================================================
# TEST SUMMARY
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("  Season 2 Week 4 -- Test Suite Complete\n")
cat(strrep("=", 60), "\n")
cat("  Functions tested:\n")
cat("    calculate_season_to_date_avg()    T01-T06\n")
cat("    build_ab_panel()                  T07-T09 (validation)\n")
cat("    compare_prediction_methods()      T10-T16\n")
cat("    calculate_effect_size_ab()        T17-T19\n")
cat("    power_analysis_correlation_diff() T20-T22\n")
cat("    apply_fdr_correction()            T23-T26\n")
cat("    run_ab_test_pipeline()            T27     (validation)\n")
cat(strrep("=", 60), "\n\n")
