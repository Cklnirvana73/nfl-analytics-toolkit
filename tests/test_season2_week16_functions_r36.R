# ==============================================================================
# Season 2 Week 16 -- Start/Sit Uncertainty Quantification (R/36) Tests
# File: tests/test_season2_week16_functions_r36.R
# ==============================================================================
#
# PURPOSE
# -------
# Tests for R/36_start_sit_uq.R. Covers the distribution helpers, the label
# functions, slot eligibility, and an end-to-end analyze_start_sit() check on a
# synthetic lineup. All tests are deterministic and network-free: the
# integration fixture has no DEF starter and passes no league source, so the
# offense-only path runs without any Sleeper call or cache read.
#
# COVERAGE
# --------
#   .recover_sigma()             : sigma from an 80% interval
#   .prob_start_correct()        : two-Normal probability, deterministic edges, NA
#   .expected_regret()           : positive-part expectation, deterministic edges
#   .stakes_label()              : low / medium / high boundaries, NA
#   .pick_clarity()              : clear / slight edge / tossup boundaries, NA
#   .eligible_positions_for_slot(): FLEX, QB, SUPER_FLEX, DEF, TE
#   analyze_start_sit()          : enriched schema, predicted probabilities,
#                                  regret/avg_miss consistency, no_alt handling,
#                                  week_risk_score, empty-lineup guard
#
# RUN WITH:
#   testthat::test_file(here::here("tests",
#                                  "test_season2_week16_functions_r36.R"))
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)
library(here)

# Sourcing R/36 pulls in R/35 and the rest of the chain via its guarded
# source() calls; the helper definitions then live at the top level.
source(here::here("R", "36_start_sit_uq.R"))

# ==============================================================================
# FIXTURES (synthetic; no network, no cache)
# ==============================================================================

# A two-bound interval width of 10 recovers sigma = 10 / (2 * qnorm(0.9)).
sigma_from_10 <- 10 / (2 * stats::qnorm(0.9))

# Minimal optimize_lineup()-shaped result. Starters: QB, WR, TE. The TE has no
# eligible bench alternative (no bench TE), exercising the no_alt path.
make_lineup <- function() {
  starters <- tibble::tibble(
    slot           = c("QB", "WR", "TE"),
    nfl_gsis_id    = c("00-TEST-QB", "00-TEST-WR", "00-TEST-TE"),
    player_name    = c("Test QB", "Test WR1", "Test TE1"),
    team           = c("AAA", "BBB", "GGG"),
    position       = c("QB", "WR", "TE"),
    opponent       = c("CCC", "DDD", "HHH"),
    base_proj      = c(20, 15, 10),
    matchup_factor = c(1, 1, 1),
    adj_proj       = c(20, 15, 10),
    adjusted_vorp  = c(5, 4, 3),
    confidence_flag = c("close", "close", "close"),
    schema_tag     = "s2_w16_lineup_v1"
  )
  bench <- tibble::tibble(
    nfl_gsis_id    = c("00-TEST-QB2", "00-TEST-WR2"),
    player_name    = c("Test QB2", "Test WR2"),
    team           = c("EEE", "FFF"),
    position       = c("QB", "WR"),
    base_proj      = c(16, 12),
    lower_80       = c(11, 8),
    upper_80       = c(21, 16),
    matchup_factor = c(1, 1),
    adj_proj       = c(16, 12)
  )
  list(starters = starters, bench = bench,
       unavailable = tibble::tibble(), all_close = tibble::tibble())
}

make_reconciled <- function() {
  tibble::tibble(
    nfl_gsis_id = c("00-TEST-QB", "00-TEST-WR", "00-TEST-TE"),
    r32_projection_lower_80 = c(15, 10, 5),
    r32_projection_upper_80 = c(25, 20, 15)
  )
}

# ==============================================================================
# .recover_sigma
# ==============================================================================

test_that(".recover_sigma inverts the 80% interval half-width", {
  expect_equal(.recover_sigma(10, 20), sigma_from_10)
  expect_equal(.recover_sigma(0, 0), 0)
  expect_true(is.na(.recover_sigma(NA_real_, 20)))
})

# ==============================================================================
# .prob_start_correct
# ==============================================================================

test_that(".prob_start_correct equals 0.5 for equal means", {
  expect_equal(.prob_start_correct(10, 2, 10, 2), 0.5)
})

test_that(".prob_start_correct handles the deterministic (zero-sigma) case", {
  expect_equal(.prob_start_correct(20, 0, 10, 0), 1)
  expect_equal(.prob_start_correct(10, 0, 10, 0), 0.5)
  expect_equal(.prob_start_correct(5, 0, 10, 0), 0)
})

test_that(".prob_start_correct returns NA when a mean is missing", {
  expect_true(is.na(.prob_start_correct(NA_real_, 2, 10, 2)))
  expect_true(is.na(.prob_start_correct(10, 2, NA_real_, 2)))
})

test_that(".prob_start_correct rises above 0.5 when the starter leads", {
  p <- .prob_start_correct(15, 3, 12, 3)
  expect_gt(p, 0.5)
  expect_lt(p, 1)
})

# ==============================================================================
# .expected_regret
# ==============================================================================

test_that(".expected_regret at equal means equals sd_diff * dnorm(0)", {
  expect_equal(.expected_regret(10, 3, 10, 3),
               sqrt(3^2 + 3^2) * stats::dnorm(0))
})

test_that(".expected_regret collapses toward 0 when the starter dominates", {
  expect_lt(.expected_regret(40, 2, 10, 2), 0.001)
})

test_that(".expected_regret deterministic case is max(mu_a - mu_s, 0)", {
  expect_equal(.expected_regret(10, 0, 14, 0), 4)
  expect_equal(.expected_regret(10, 0, 8, 0), 0)
})

test_that(".expected_regret returns NA when a mean is missing", {
  expect_true(is.na(.expected_regret(NA_real_, 2, 10, 2)))
})

# ==============================================================================
# .stakes_label
# ==============================================================================

test_that(".stakes_label buckets on the regret thresholds", {
  expect_equal(.stakes_label(0.5), "low")
  expect_equal(.stakes_label(0.99), "low")
  expect_equal(.stakes_label(1.0), "medium")   # 1.0 is not < STAKES_LOW_MAX
  expect_equal(.stakes_label(2.0), "medium")
  expect_equal(.stakes_label(3.0), "high")     # >= STAKES_HIGH_MIN
  expect_equal(.stakes_label(5.0), "high")
  expect_true(is.na(.stakes_label(NA_real_)))
})

# ==============================================================================
# .pick_clarity
# ==============================================================================

test_that(".pick_clarity buckets on P(top beats next)", {
  expect_equal(.pick_clarity(0.60), "clear")
  expect_equal(.pick_clarity(0.75), "clear")
  expect_equal(.pick_clarity(0.59), "slight edge")
  expect_equal(.pick_clarity(0.53), "slight edge")
  expect_equal(.pick_clarity(0.52), "tossup")
  expect_equal(.pick_clarity(0.50), "tossup")
  expect_true(is.na(.pick_clarity(NA_real_)))
})

# ==============================================================================
# .eligible_positions_for_slot
# ==============================================================================

test_that(".eligible_positions_for_slot reverses SLOT_ELIGIBILITY", {
  expect_setequal(.eligible_positions_for_slot("QB"), c("QB"))
  expect_setequal(.eligible_positions_for_slot("TE"), c("TE"))
  expect_setequal(.eligible_positions_for_slot("DEF"), c("DEF"))
  expect_setequal(.eligible_positions_for_slot("FLEX"), c("RB", "WR", "TE"))
  expect_setequal(.eligible_positions_for_slot("SUPER_FLEX"),
                  c("QB", "RB", "WR", "TE"))
})

# ==============================================================================
# analyze_start_sit (integration, offense-only, no network)
# ==============================================================================

test_that("analyze_start_sit returns the enriched schema and week score", {
  uq <- analyze_start_sit(make_lineup(), make_reconciled(), week = 6L)

  expect_true(is.list(uq))
  expect_true(nrow(uq$starters) == 3)

  added <- c("alt_player", "alt_position", "alt_source", "alt_adj_proj",
             "p_start_correct", "avg_miss", "expected_regret", "stakes",
             "uq_schema_tag")
  expect_true(all(added %in% names(uq$starters)))
  expect_true(all(uq$starters$uq_schema_tag == "s2_w16_uq_v1"))

  # week_risk_score is the NA-safe sum of expected_regret.
  expect_equal(uq$week_risk_score,
               sum(uq$starters$expected_regret, na.rm = TRUE))

  # No league source supplied, so no DEF streaming table.
  expect_null(uq$def_streaming)
})

test_that("analyze_start_sit computes the predicted probabilities", {
  uq <- analyze_start_sit(make_lineup(), make_reconciled(), week = 6L)
  s  <- uq$starters

  # WR: mu_s = 15, sigma = sigma_from_10; alt WR2 mu = 12,
  # sigma = .recover_sigma(8, 16). p = pnorm((15-12)/sqrt(sum of squares)).
  sd_wr  <- sqrt(sigma_from_10^2 + .recover_sigma(8, 16)^2)
  p_wr   <- stats::pnorm((15 - 12) / sd_wr)
  expect_equal(s$p_start_correct[s$slot == "WR"], round(p_wr, 3))
  expect_equal(s$alt_player[s$slot == "WR"], "Test WR2")

  # QB: mu_s = 20, sigma = sigma_from_10; alt QB2 mu = 16,
  # sigma = .recover_sigma(11, 21) = sigma_from_10.
  sd_qb <- sqrt(sigma_from_10^2 + sigma_from_10^2)
  p_qb  <- stats::pnorm((20 - 16) / sd_qb)
  expect_equal(s$p_start_correct[s$slot == "QB"], round(p_qb, 3))
})

test_that("analyze_start_sit keeps avg_miss = expected_regret / (1 - p)", {
  uq <- analyze_start_sit(make_lineup(), make_reconciled(), week = 6L)
  wr <- uq$starters[uq$starters$slot == "WR", ]
  # Relationship holds up to rounding of the stored columns.
  expect_lt(abs(wr$avg_miss * (1 - wr$p_start_correct) - wr$expected_regret),
            0.05)
})

test_that("analyze_start_sit marks a slot with no eligible alternative as no_alt", {
  uq <- analyze_start_sit(make_lineup(), make_reconciled(), week = 6L)
  te <- uq$starters[uq$starters$slot == "TE", ]
  expect_true(is.na(te$p_start_correct))
  expect_true(is.na(te$expected_regret))
  expect_true(is.na(te$stakes))
  expect_true(is.na(te$alt_player))
})

test_that("analyze_start_sit handles an empty lineup", {
  empty <- list(starters = make_lineup()$starters[0, ],
                bench = make_lineup()$bench,
                unavailable = tibble::tibble(),
                all_close = tibble::tibble())
  uq <- analyze_start_sit(empty, make_reconciled())
  expect_equal(uq$week_risk_score, 0)
})

test_that("analyze_start_sit errors on a malformed lineup", {
  expect_error(analyze_start_sit(list(foo = 1), make_reconciled()),
               regexp = "optimize_lineup")
  expect_error(
    analyze_start_sit(make_lineup(),
                      tibble::tibble(nfl_gsis_id = "x")),
    regexp = "missing columns")
})

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat("==============================================================\n")
cat("  Season 2 Week 16 Test Suite (R/36 start/sit UQ)\n")
cat("==============================================================\n")
cat("  Functions tested: .recover_sigma, .prob_start_correct,\n")
cat("                    .expected_regret, .stakes_label,\n")
cat("                    .pick_clarity, .eligible_positions_for_slot,\n")
cat("                    analyze_start_sit\n")
cat("  Categories: sigma recovery, two-Normal probability,\n")
cat("              expected regret, label boundaries, slot\n")
cat("              eligibility, enriched schema, predicted values,\n")
cat("              regret/avg_miss consistency, no_alt handling,\n")
cat("              empty-lineup guard, input validation\n")
cat("  All tests synthetic and network-free.\n")
cat("==============================================================\n")
