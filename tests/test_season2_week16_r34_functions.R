# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 16
# Test Suite: R/34 DEF/ST Forward Projection
# File: tests/test_season2_week16_r34_functions.R
#
# COVERAGE
# --------
#   .def_pts_allowed_to_points -- tier boundary mapping, NA passthrough, type
#   .score_def_games           -- schema, standard-scoring hand calc, league
#                                 config changes the score, type consistency
#   .parse_def_scoring         -- league overrides applied, missing keys keep
#                                 default, multiple Sleeper key spellings,
#                                 NULL/empty returns default
#   .regress_to_mean           -- strength 0 / 1 / midpoint
#   .blend_prior_observed      -- has_observed gate, week-weighted blend,
#                                 preseason pure prior, NA-observed safety
#
# OUT OF SCOPE
# -----------
#   .load_league_def_scoring   -- requires connect_sleeper_league (network)
#   .compute_team_def_ppg      -- requires calculate_def_st_points + pbp
#   .compute_def_prior         -- loads pbp from R/15 cache
#   .compute_def_observed      -- loads pbp from R/15 cache
#   project_def_st             -- top-level orchestrator
#
# DEPENDENCIES
# ------------
#   R/34_def_st_projection.R (sources R/15, R/19, R/29 at load time)
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
})

suppressPackageStartupMessages({
  source(here::here("R", "34_def_st_projection.R"))
})


# ==============================================================================
# FIXTURE BUILDERS
# ==============================================================================

#' Build a minimal per-game DEF counts tibble for .score_def_games tests.
make_def_counts <- function(opponent_pts_allowed,
                            sacks         = 0,
                            def_ints      = 0,
                            fum_recs      = 0,
                            def_tds       = 0,
                            safeties      = 0,
                            blocked_kicks = 0) {
  n <- length(opponent_pts_allowed)
  tibble::tibble(
    opponent_pts_allowed = opponent_pts_allowed,
    sacks         = rep(sacks, length.out = n),
    def_ints      = rep(def_ints, length.out = n),
    fum_recs      = rep(fum_recs, length.out = n),
    def_tds       = rep(def_tds, length.out = n),
    safeties      = rep(safeties, length.out = n),
    blocked_kicks = rep(blocked_kicks, length.out = n)
  )
}


# ==============================================================================
# .def_pts_allowed_to_points
# ==============================================================================

test_that("[R/34] .def_pts_allowed_to_points maps tier boundaries under standard scoring", {
  tiers <- DEF_SCORING_DEFAULT$pts_allow
  out <- .def_pts_allowed_to_points(c(0, 6, 7, 13, 20, 27, 34, 35), tiers)
  expect_equal(out, c(10, 7, 4, 4, 1, 0, -1, -4))
})

test_that("[R/34] .def_pts_allowed_to_points passes NA through as NA", {
  tiers <- DEF_SCORING_DEFAULT$pts_allow
  expect_true(is.na(.def_pts_allowed_to_points(NA_real_, tiers)))
})

test_that("[R/34] .def_pts_allowed_to_points returns numeric", {
  tiers <- DEF_SCORING_DEFAULT$pts_allow
  expect_type(.def_pts_allowed_to_points(c(3, 40), tiers), "double")
})


# ==============================================================================
# .score_def_games
# ==============================================================================

test_that("[R/34] .score_def_games returns the scoring columns", {
  counts <- make_def_counts(opponent_pts_allowed = 14, sacks = 2)
  out <- .score_def_games(counts, DEF_SCORING_DEFAULT)
  expect_true(all(c("pts_allow_points", "def_event_points", "def_st_points")
                  %in% names(out)))
})

test_that("[R/34] .score_def_games standard scoring matches hand calculation", {
  # shutout (10) + 3 sacks (3) + 2 INT (4) + 1 fum_rec (2) + 1 def_td (6) = 25
  counts <- make_def_counts(opponent_pts_allowed = 0, sacks = 3, def_ints = 2,
                            fum_recs = 1, def_tds = 1)
  out <- .score_def_games(counts, DEF_SCORING_DEFAULT)
  expect_equal(out$def_st_points, 25)
})

test_that("[R/34] .score_def_games applies a league config that differs from standard", {
  # league pays 3 per sack instead of 1
  league <- DEF_SCORING_DEFAULT
  league$events[["sack"]] <- 3
  counts <- make_def_counts(opponent_pts_allowed = 30, sacks = 4)
  out <- .score_def_games(counts, league)
  # pts_allow 30 -> tier 28_34 = -1; events 4 sacks * 3 = 12; total 11
  expect_equal(out$def_st_points, 11)
})

test_that("[R/34] .score_def_games returns numeric def_st_points", {
  out <- .score_def_games(make_def_counts(7), DEF_SCORING_DEFAULT)
  expect_type(out$def_st_points, "double")
})


# ==============================================================================
# .parse_def_scoring
# ==============================================================================

test_that("[R/34] .parse_def_scoring applies league overrides and keeps defaults for missing keys", {
  ss <- list(pts_allow_0 = 12, def_sack = 2)
  cfg <- .parse_def_scoring(ss)
  expect_equal(cfg$pts_allow[["pts_allow_0"]], 12)     # overridden
  expect_equal(cfg$pts_allow[["pts_allow_1_6"]], 7)    # default retained
  expect_equal(cfg$events[["sack"]], 2)                # overridden via def_sack
  expect_equal(cfg$events[["safety"]], 2)              # default retained
})

test_that("[R/34] .parse_def_scoring resolves multiple Sleeper key spellings", {
  # int (not def_int), def_st_td (not def_td) should still map
  ss <- list(int = 3, def_st_td = 7)
  cfg <- .parse_def_scoring(ss)
  expect_equal(cfg$events[["def_int"]], 3)
  expect_equal(cfg$events[["def_td"]], 7)
})

test_that("[R/34] .parse_def_scoring returns the default for NULL or empty settings", {
  expect_identical(.parse_def_scoring(NULL), DEF_SCORING_DEFAULT)
  expect_identical(.parse_def_scoring(list()), DEF_SCORING_DEFAULT)
})


# ==============================================================================
# .regress_to_mean
# ==============================================================================

test_that("[R/34] .regress_to_mean strength 0 leaves values unchanged", {
  expect_equal(.regress_to_mean(c(10, 2), 6, 0), c(10, 2))
})

test_that("[R/34] .regress_to_mean strength 1 returns the target", {
  expect_equal(.regress_to_mean(c(10, 2), 6, 1), c(6, 6))
})

test_that("[R/34] .regress_to_mean midpoint shrinkage matches the formula", {
  expect_equal(.regress_to_mean(10, 6, 0.35), 0.65 * 10 + 0.35 * 6,
               tolerance = 1e-12)
})


# ==============================================================================
# .blend_prior_observed
# ==============================================================================

test_that("[R/34] .blend_prior_observed blends observed teams by the prior weight", {
  prior    <- c(8, 8)
  observed <- c(12, 4)
  out <- .blend_prior_observed(prior, observed, 0.5, c(TRUE, TRUE))
  expect_equal(out, c(10, 6))   # 0.5*8 + 0.5*12 ; 0.5*8 + 0.5*4
})

test_that("[R/34] .blend_prior_observed keeps the prior when has_observed is FALSE", {
  out <- .blend_prior_observed(c(8, 8), c(12, NA), 0.5, c(TRUE, FALSE))
  expect_equal(out[2], 8)        # no observed -> pure prior, NA does not leak
})

test_that("[R/34] .blend_prior_observed preseason weight 1.0 returns pure prior", {
  out <- .blend_prior_observed(c(8, 5), c(12, 20), 1.0, c(TRUE, TRUE))
  expect_equal(out, c(8, 5))
})
