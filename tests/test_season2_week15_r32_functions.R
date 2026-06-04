# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Test Suite: R/32 Projection Reconciliation
# File: tests/test_season2_week15_r32_functions.R
#
# COVERAGE
# --------
#   .compute_volume_implied_ppg  -- schema, sack adjustment, QB gets NA,
#                                   WR formula correctness
#   .determine_blend_weights     -- QB always 0, known prior_source mapping,
#                                   unrecognized falls back to DEFAULT
#   .blend_projections           -- blend equation, NA volume -> weight 0,
#                                   negative clamp, interval shift width
#   .apply_qb_depth_discount     -- QB1=1.0 no change, QB2=0.15 uses r29_mu,
#                                   QB3=0.05, non-QB NA, NA depth_position NA,
#                                   depth_position column removed
#
# ALL FUNCTIONS ARE PURE -- no mocking needed.
#
# FIXTURE NOTES
# -------------
#   .blend_projections expects columns already renamed from R/29 conventions:
#     posterior_mu -> r29_posterior_mu (done by orchestrator before calling blend)
#   .apply_qb_depth_discount receives depth_position pre-joined into reconciled.
#   The r31 argument to .apply_qb_depth_discount is unused in the function body
#   (kept for signature symmetry) so tests pass an empty tibble.
#
# DEPENDENCIES
# ------------
#   R/32_projection_reconciliation.R
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
})

suppressPackageStartupMessages({
  source(here::here("R", "32_projection_reconciliation.R"))
})


# ==============================================================================
# FIXTURE BUILDERS
# ==============================================================================

#' Build a minimal R/31 alloc tibble for .compute_volume_implied_ppg.
make_alloc_r32 <- function(gsis_ids             = c("00-wr", "00-rb", "00-qb"),
                            positions            = c("WR", "RB", "QB"),
                            expected_targets_pg  = c(8.0, 3.0, 0.0),
                            expected_carries_pg  = c(0.0, 15.0, 0.0)) {
  tibble::tibble(
    nfl_gsis_id         = gsis_ids,
    position            = positions,
    expected_targets_pg = expected_targets_pg,
    expected_carries_pg = expected_carries_pg
  )
}

#' Build a minimal R/29 tibble for .determine_blend_weights.
make_r29_r32 <- function(gsis_ids     = c("00-001", "00-002", "00-003"),
                          positions    = c("WR", "RB", "QB"),
                          prior_source = c("blended", "translation_only", "blended")) {
  tibble::tibble(
    nfl_gsis_id  = gsis_ids,
    position     = positions,
    prior_source = prior_source
  )
}

#' Build a merged tibble for .blend_projections.
#' All R/29 columns are already renamed (posterior_mu -> r29_posterior_mu).
make_merged_r32 <- function(gsis_ids             = "00-001",
                             position             = "WR",
                             r29_posterior_mu     = 14.0,
                             volume_implied_ppg_v2 = 12.0,
                             blend_weight_r31     = 0.30,
                             r29_lower_80         = 10.0,
                             r29_upper_80         = 18.0,
                             r29_lower_95         = 7.0,
                             r29_upper_95         = 21.0) {
  n <- length(gsis_ids)
  tibble::tibble(
    nfl_gsis_id             = gsis_ids,
    position                = rep(position, n),
    r29_posterior_mu        = rep(r29_posterior_mu, n),
    volume_implied_ppg_v2   = rep(volume_implied_ppg_v2, n),
    blend_weight_r31        = rep(blend_weight_r31, n),
    r29_projection_lower_80 = rep(r29_lower_80, n),
    r29_projection_upper_80 = rep(r29_upper_80, n),
    r29_projection_lower_95 = rep(r29_lower_95, n),
    r29_projection_upper_95 = rep(r29_upper_95, n)
  )
}

#' Build a reconciled tibble for .apply_qb_depth_discount.
#' Includes depth_position (pre-joined from R/31 by orchestrator).
make_reconciled_r32 <- function(gsis_ids        = "00-qb1",
                                 position        = "QB",
                                 depth_position  = "QB1",
                                 r29_mu          = 20.0,
                                 r32_mu          = 20.0,
                                 r29_lower_80    = 15.0,
                                 r29_upper_80    = 25.0,
                                 r29_lower_95    = 12.0,
                                 r29_upper_95    = 28.0,
                                 r32_lower_80    = 15.0,
                                 r32_upper_80    = 25.0,
                                 r32_lower_95    = 12.0,
                                 r32_upper_95    = 28.0) {
  n <- length(gsis_ids)
  br <- function(x) rep(x, n)
  tibble::tibble(
    nfl_gsis_id             = gsis_ids,
    position                = br(position),
    depth_position          = br(depth_position),
    r29_posterior_mu        = br(r29_mu),
    r32_posterior_mu        = br(r32_mu),
    r32_delta_from_r29      = br(r32_mu - r29_mu),
    r29_projection_lower_80 = br(r29_lower_80),
    r29_projection_upper_80 = br(r29_upper_80),
    r29_projection_lower_95 = br(r29_lower_95),
    r29_projection_upper_95 = br(r29_upper_95),
    r32_projection_lower_80 = br(r32_lower_80),
    r32_projection_upper_80 = br(r32_upper_80),
    r32_projection_lower_95 = br(r32_lower_95),
    r32_projection_upper_95 = br(r32_upper_95)
  )
}


# ==============================================================================
# .compute_volume_implied_ppg
# ==============================================================================

test_that("[R/32] .compute_volume_implied_ppg returns required schema columns", {
  alloc <- make_alloc_r32()
  out   <- .compute_volume_implied_ppg(alloc)
  expect_true(all(c("nfl_gsis_id", "r31_true_targets_pg",
                     "volume_implied_ppg_v2") %in% names(out)),
    info = paste("Missing:", setdiff(c("nfl_gsis_id", "r31_true_targets_pg",
                                        "volume_implied_ppg_v2"), names(out))))
})

test_that("[R/32] .compute_volume_implied_ppg r31_true_targets_pg applies SACK_ADJUSTMENT to expected_targets_pg", {
  alloc <- make_alloc_r32(gsis_ids = "00-wr", positions = "WR",
                           expected_targets_pg = 8.0, expected_carries_pg = 0.0)
  out   <- .compute_volume_implied_ppg(alloc)
  expect_equal(out$r31_true_targets_pg, 8.0 * SACK_ADJUSTMENT, tolerance = 1e-10)
})

test_that("[R/32] .compute_volume_implied_ppg QB gets NA volume_implied_ppg_v2 (not in RECON_POSITIONS)", {
  alloc <- make_alloc_r32(gsis_ids = "00-qb", positions = "QB",
                           expected_targets_pg = 0.0, expected_carries_pg = 0.0)
  out   <- .compute_volume_implied_ppg(alloc)
  expect_true(is.na(out$volume_implied_ppg_v2),
    info = "QB is excluded from RECON_POSITIONS -- volume_implied must be NA")
})

test_that("[R/32] .compute_volume_implied_ppg WR implied PPG matches ppr_per_target formula", {
  # WR ppr_per_target = catch_rate * (yards_per_catch * 0.1 + 1.0) + td_per_target * 6
  # = 0.65 * (12.5 * 0.1 + 1.0) + 0.05 * 6 = 0.65 * 2.25 + 0.30 = 1.7625
  # volume_implied = expected_targets * SACK_ADJUSTMENT * 1.7625
  wr_eff <- POSITION_EFFICIENCY[["WR"]]
  ppr_per_target <- wr_eff$catch_rate * (wr_eff$yards_per_catch * 0.1 + 1.0) +
                      wr_eff$td_per_target * 6
  expected_implied <- 8.0 * SACK_ADJUSTMENT * ppr_per_target

  alloc <- make_alloc_r32(gsis_ids = "00-wr", positions = "WR",
                           expected_targets_pg = 8.0, expected_carries_pg = 0.0)
  out   <- .compute_volume_implied_ppg(alloc)
  expect_equal(out$volume_implied_ppg_v2, expected_implied, tolerance = 1e-10)
})


# ==============================================================================
# .determine_blend_weights
# ==============================================================================

test_that("[R/32] .determine_blend_weights QB position always gets weight 0 regardless of prior_source", {
  r29 <- make_r29_r32(gsis_ids = c("00-q1", "00-q2"),
                       positions = c("QB", "QB"),
                       prior_source = c("blended", "translation_only"))
  out <- .determine_blend_weights(r29)
  expect_true(all(out$blend_weight_r31 == 0),
    info = "QBs must always get blend_weight_r31 = 0")
})

test_that("[R/32] .determine_blend_weights maps recognized prior_source values correctly", {
  r29 <- make_r29_r32(
    gsis_ids     = c("00-a", "00-b", "00-c", "00-d", "00-e"),
    positions    = c("WR", "WR", "RB", "TE", "WR"),
    prior_source = c("blended", "veteran_history_only", "history_only_fallback",
                     "translation_only", "score_final_fallback")
  )
  out <- .determine_blend_weights(r29)
  expect_equal(unname(out %>% dplyr::filter(nfl_gsis_id == "00-a") %>%
                 dplyr::pull(blend_weight_r31)), 0.30)
  expect_equal(unname(out %>% dplyr::filter(nfl_gsis_id == "00-b") %>%
                 dplyr::pull(blend_weight_r31)), 0.45)
  expect_equal(unname(out %>% dplyr::filter(nfl_gsis_id == "00-c") %>%
                 dplyr::pull(blend_weight_r31)), 0.55)
  expect_equal(unname(out %>% dplyr::filter(nfl_gsis_id == "00-d") %>%
                 dplyr::pull(blend_weight_r31)), 0.65)
  expect_equal(unname(out %>% dplyr::filter(nfl_gsis_id == "00-e") %>%
                 dplyr::pull(blend_weight_r31)), 0.85)
})

test_that("[R/32] .determine_blend_weights unrecognized prior_source falls back to DEFAULT_R31_WEIGHT", {
  r29 <- make_r29_r32(gsis_ids = "00-x", positions = "WR",
                       prior_source = "completely_unknown_source")
  out <- .determine_blend_weights(r29)
  expect_equal(unname(out$blend_weight_r31), DEFAULT_R31_WEIGHT,
    info = "Unrecognized prior_source should fall back to DEFAULT_R31_WEIGHT (0.50)")
})


# ==============================================================================
# .blend_projections
# ==============================================================================

test_that("[R/32] .blend_projections applies blend formula: r32_mu = w * vol + (1-w) * r29_mu", {
  # w = 0.30, vol = 12.0, r29 = 14.0
  # expected r32 = 0.30 * 12.0 + 0.70 * 14.0 = 3.6 + 9.8 = 13.4
  merged <- make_merged_r32(r29_posterior_mu = 14.0, volume_implied_ppg_v2 = 12.0,
                              blend_weight_r31 = 0.30)
  out <- .blend_projections(merged)
  expect_equal(out$r32_posterior_mu, 0.30 * 12.0 + 0.70 * 14.0, tolerance = 1e-10)
})

test_that("[R/32] .blend_projections when volume_implied_ppg_v2 is NA blend_weight_effective is 0 and r32_mu equals r29_mu", {
  merged <- make_merged_r32(r29_posterior_mu = 14.0,
                              volume_implied_ppg_v2 = NA_real_,
                              blend_weight_r31 = 0.30)
  out <- .blend_projections(merged)
  expect_equal(out$blend_weight_effective, 0,
    info = "NA volume should force blend_weight_effective to 0")
  expect_equal(out$r32_posterior_mu, 14.0, tolerance = 1e-10,
    info = "No volume signal -> r32_mu should equal r29_mu")
})

test_that("[R/32] .blend_projections negative r32_posterior_mu is clamped to 0", {
  # r29_mu = 2.0, vol = -20.0 (edge case), w = 0.90
  # raw r32 = 0.90 * (-20) + 0.10 * 2 = -18 + 0.2 = -17.8 -> clamp -> 0
  merged <- make_merged_r32(r29_posterior_mu = 2.0,
                              volume_implied_ppg_v2 = -20.0,
                              blend_weight_r31 = 0.90)
  out <- .blend_projections(merged)
  expect_equal(out$r32_posterior_mu, 0,
    info = "Negative projected PPG must be clamped to 0")
})

test_that("[R/32] .blend_projections intervals shift by r32_delta and preserve width", {
  # r29_mu = 14, vol = 10, w = 0.30 -> r32_mu = 0.30*10 + 0.70*14 = 12.8
  # delta = 12.8 - 14.0 = -1.2
  # r29_lower_80 = 10.0 -> r32_lower_80 = 10.0 + (-1.2) = 8.8
  # r29_upper_80 = 18.0 -> r32_upper_80 = 18.0 + (-1.2) = 16.8
  merged <- make_merged_r32(r29_posterior_mu = 14.0,
                              volume_implied_ppg_v2 = 10.0,
                              blend_weight_r31 = 0.30,
                              r29_lower_80 = 10.0, r29_upper_80 = 18.0)
  out   <- .blend_projections(merged)
  delta <- out$r32_posterior_mu - 14.0
  expect_equal(out$r32_projection_lower_80, 10.0 + delta, tolerance = 1e-10)
  expect_equal(out$r32_projection_upper_80, 18.0 + delta, tolerance = 1e-10)
  # Interval width should be unchanged
  r29_width <- 18.0 - 10.0
  r32_width <- out$r32_projection_upper_80 - out$r32_projection_lower_80
  expect_equal(r32_width, r29_width, tolerance = 1e-10,
    info = "Interval shift must preserve width (delta added to both ends)")
})


# ==============================================================================
# .apply_qb_depth_discount
# ==============================================================================

test_that("[R/32] .apply_qb_depth_discount QB1 gets factor 1.0 and r32_mu is unchanged", {
  rec <- make_reconciled_r32(position = "QB", depth_position = "QB1",
                               r29_mu = 25.0, r32_mu = 25.0)
  out <- .apply_qb_depth_discount(rec, tibble::tibble())
  expect_equal(out$qb_discount_factor, QB_DEPTH_DISCOUNT_MULT[["QB1"]])
  expect_equal(out$r32_posterior_mu, 25.0, tolerance = 1e-10,
    info = "QB1 factor = 1.0 means no change to r32_posterior_mu")
})

test_that("[R/32] .apply_qb_depth_discount QB2 gets factor 0.15 applied to r29_posterior_mu", {
  rec <- make_reconciled_r32(position = "QB", depth_position = "QB2",
                               r29_mu = 24.0, r32_mu = 24.0)
  out <- .apply_qb_depth_discount(rec, tibble::tibble())
  expect_equal(out$qb_discount_factor, QB_DEPTH_DISCOUNT_MULT[["QB2"]])
  expect_equal(out$r32_posterior_mu,
    24.0 * QB_DEPTH_DISCOUNT_MULT[["QB2"]],
    tolerance = 1e-10,
    info = "QB2 discount applied to r29_posterior_mu (not r32)")
})

test_that("[R/32] .apply_qb_depth_discount QB3 gets factor 0.05", {
  rec <- make_reconciled_r32(position = "QB", depth_position = "QB3",
                               r29_mu = 22.0, r32_mu = 22.0)
  out <- .apply_qb_depth_discount(rec, tibble::tibble())
  expect_equal(out$qb_discount_factor, QB_DEPTH_DISCOUNT_MULT[["QB3"]])
  expect_equal(out$r32_posterior_mu,
    22.0 * QB_DEPTH_DISCOUNT_MULT[["QB3"]],
    tolerance = 1e-10)
})

test_that("[R/32] .apply_qb_depth_discount non-QB position gets NA factor and r32_mu unchanged", {
  rec <- make_reconciled_r32(position = "WR", depth_position = "WR1",
                               r29_mu = 16.0, r32_mu = 15.0)
  out <- .apply_qb_depth_discount(rec, tibble::tibble())
  expect_true(is.na(out$qb_discount_factor),
    info = "Non-QB players must get qb_discount_factor = NA")
  expect_equal(out$r32_posterior_mu, 15.0, tolerance = 1e-10,
    info = "r32_posterior_mu must be unchanged for non-QB players")
})

test_that("[R/32] .apply_qb_depth_discount QB with NA depth_position gets NA factor (no R/31 record)", {
  rec <- make_reconciled_r32(position = "QB", depth_position = NA_character_,
                               r29_mu = 20.0, r32_mu = 20.0)
  out <- .apply_qb_depth_discount(rec, tibble::tibble())
  expect_true(is.na(out$qb_discount_factor),
    info = "QB with no R/31 depth record should get NA factor (kept at R/29)")
  expect_equal(out$r32_posterior_mu, 20.0, tolerance = 1e-10)
})

test_that("[R/32] .apply_qb_depth_discount removes depth_position column from output", {
  rec <- make_reconciled_r32(position = "QB", depth_position = "QB1")
  out <- .apply_qb_depth_discount(rec, tibble::tibble())
  expect_false("depth_position" %in% names(out),
    info = "depth_position is a working join column and must be removed from output")
})
