# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Test Suite: R/33 VORP Rankings
# File: tests/test_season2_week15_r33_functions.R
#
# COVERAGE
# --------
#   build_league_config           -- format defaults applied, custom override
#                                    wins, class set correctly, invalid format
#                                    errors
#   .compute_replacement_levels   -- starters assigned before bench, replacement
#                                    PPG is first bench player per position
#   .compute_player_vorp          -- vorp_base = r32_mu - replacement_ppg,
#                                    boom_modifier = boom_weight * boom_prob
#   compute_vorp_rankings         -- contiguous overall_rank and position_rank,
#                                    errors on invalid config class
#
# FIXTURE NOTES
# -------------
#   Tests use num_teams=1 to minimise fixture size while still exercising
#   starter/bench role logic. With num_teams=1 and starters=list(QB=1,RB=1,
#   WR=1,TE=1), exactly 1 player per position is a starter and all others
#   are bench. This keeps each fixture to 2-3 players per position.
#
# DEPENDENCIES
# ------------
#   R/33_vorp_rankings.R
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
})

suppressPackageStartupMessages({
  source(here::here("R", "33_vorp_rankings.R"))
})


# ==============================================================================
# FIXTURE BUILDERS
# ==============================================================================

#' Build a minimal projections tibble for R/33 tests.
#' Columns are the minimum required by .compute_replacement_levels,
#' .compute_player_vorp, and compute_vorp_rankings.
make_projections_r33 <- function(gsis_ids    = paste0("00-", sprintf("%02d", 1:8)),
                                   positions   = c("QB","QB","RB","RB","WR","WR","TE","TE"),
                                   r32_mu      = c(22,8,16,12,18,14,11,7),
                                   boom_prob   = 0.20,
                                   bust_prob   = 0.15,
                                   upper_80    = NULL) {
  n <- length(gsis_ids)
  if (length(boom_prob) == 1L) boom_prob <- rep(boom_prob, n)
  if (length(bust_prob) == 1L) bust_prob <- rep(bust_prob, n)
  if (is.null(upper_80)) upper_80 <- r32_mu + 4
  tibble::tibble(
    nfl_gsis_id              = gsis_ids,
    player_name              = paste("Player", seq_len(n)),
    team                     = rep("KC", n),
    position                 = positions,
    r32_posterior_mu         = r32_mu,
    r32_projection_upper_80  = upper_80,
    boom_probability         = boom_prob,
    bust_probability         = bust_prob
  )
}

#' Build a 1-team PPR config for straightforward fixture sizing.
make_config_1team <- function(league_name = "Test League",
                               format      = "ppr") {
  build_league_config(
    league_name = league_name,
    num_teams   = 1L,
    starters    = list(QB = 1L, RB = 1L, WR = 1L, TE = 1L),
    flex        = 0L,
    superflex   = 0L,
    format      = format
  )
}


# ==============================================================================
# build_league_config
# ==============================================================================

test_that("[R/33] build_league_config applies LEAGUE_FORMAT_DEFAULTS for best_ball format", {
  cfg <- build_league_config("Test BB", format = "best_ball")
  defaults <- LEAGUE_FORMAT_DEFAULTS[["best_ball"]]
  expect_equal(cfg$boom_weight,    defaults$boom_weight)
  expect_equal(cfg$bust_weight,    defaults$bust_weight)
  expect_equal(cfg$ceiling_factor, defaults$ceiling_factor)
})

test_that("[R/33] build_league_config custom boom/bust weights override format defaults", {
  cfg <- build_league_config("Test Custom",
                              format      = "ppr",
                              boom_weight = 9.9,
                              bust_weight = 0.1)
  expect_equal(cfg$boom_weight, 9.9)
  expect_equal(cfg$bust_weight, 0.1)
  # ceiling_factor not overridden -- should still come from ppr default
  expect_equal(cfg$ceiling_factor, LEAGUE_FORMAT_DEFAULTS[["ppr"]]$ceiling_factor)
})

test_that("[R/33] build_league_config returns a league_config object", {
  cfg <- make_config_1team()
  expect_true(inherits(cfg, "league_config"))
})

test_that("[R/33] build_league_config errors on unknown format string", {
  expect_error(
    build_league_config("Bad", format = "not_a_format"),
    regexp = "Unknown format"
  )
})


# ==============================================================================
# .compute_replacement_levels
# ==============================================================================

test_that("[R/33] .compute_replacement_levels marks top-N players per position as starters", {
  # With num_teams=1 and 1 QB starter: the QB with highest r32_mu gets role=starter.
  proj   <- make_projections_r33()
  config <- make_config_1team()

  rep_result  <- .compute_replacement_levels(proj, config)
  role_assign <- rep_result$role_assignments

  top_qb_gsis <- proj %>%
    dplyr::filter(position == "QB") %>%
    dplyr::slice_max(r32_posterior_mu, n = 1L, with_ties = FALSE) %>%
    dplyr::pull(nfl_gsis_id)

  top_qb_role <- role_assign %>%
    dplyr::filter(nfl_gsis_id == top_qb_gsis) %>%
    dplyr::pull(role)

  expect_equal(top_qb_role, "starter",
    info = "QB with highest r32_mu should be assigned role=starter")
})

test_that("[R/33] .compute_replacement_levels replacement PPG is the first bench player per position", {
  # With 2 QBs and 1 starter slot: highest QB = starter, lower QB = bench.
  # Replacement PPG for QB = the bench QB's r32_mu.
  proj <- make_projections_r33(
    gsis_ids  = c("00-qb1", "00-qb2"),
    positions = c("QB", "QB"),
    r32_mu    = c(25.0, 8.0)
  )
  config <- make_config_1team()

  rep_result <- .compute_replacement_levels(proj, config)
  qb_rep     <- rep_result$replacement_ppg[["QB"]]

  expect_equal(qb_rep, 8.0, tolerance = 1e-10,
    info = "QB replacement PPG should be the first bench player's r32_mu")
})


# ==============================================================================
# .compute_player_vorp
# ==============================================================================

test_that("[R/33] .compute_player_vorp vorp_base equals r32_posterior_mu minus replacement_ppg", {
  proj <- make_projections_r33(
    gsis_ids  = c("00-wr1", "00-wr2"),
    positions = c("WR", "WR"),
    r32_mu    = c(18.0, 10.0),
    boom_prob = c(0.20, 0.10),
    bust_prob = c(0.10, 0.20)
  )
  config     <- make_config_1team()
  rep_result <- .compute_replacement_levels(proj, config)
  rep_ppg    <- rep_result$replacement_ppg

  out <- .compute_player_vorp(proj, rep_ppg, config)

  wr1 <- out %>% dplyr::filter(nfl_gsis_id == "00-wr1")
  expect_equal(wr1$vorp_base,
    wr1$r32_posterior_mu - wr1$replacement_ppg,
    tolerance = 1e-10)
})

test_that("[R/33] .compute_player_vorp boom_modifier equals boom_weight * boom_probability", {
  proj <- make_projections_r33(
    gsis_ids  = c("00-wr1", "00-wr2"),
    positions = c("WR", "WR"),
    r32_mu    = c(18.0, 10.0),
    boom_prob = c(0.25, 0.10)
  )
  config     <- make_config_1team()
  rep_result <- .compute_replacement_levels(proj, config)

  out  <- .compute_player_vorp(proj, rep_result$replacement_ppg, config)
  wr1  <- out %>% dplyr::filter(nfl_gsis_id == "00-wr1")

  expect_equal(wr1$boom_modifier,
    config$boom_weight * 0.25,
    tolerance = 1e-10)
})


# ==============================================================================
# compute_vorp_rankings
# ==============================================================================

test_that("[R/33] compute_vorp_rankings produces contiguous overall_rank and position_rank", {
  proj   <- make_projections_r33()
  config <- make_config_1team()
  out    <- compute_vorp_rankings(proj, config)

  overall_ranks <- sort(out$overall_rank)
  expect_equal(overall_ranks, seq_len(nrow(out)),
    info = "overall_rank must be contiguous integers 1..N")

  for (pos in unique(out$position)) {
    pos_ranks <- out %>%
      dplyr::filter(position == pos) %>%
      dplyr::pull(position_rank) %>%
      sort()
    n_pos <- length(pos_ranks)
    expect_equal(pos_ranks, seq_len(n_pos),
      info = glue::glue("position_rank for {pos} must be contiguous integers 1..{n_pos}"))
  }
})

test_that("[R/33] compute_vorp_rankings errors when config is not a league_config object", {
  proj       <- make_projections_r33()
  bad_config <- list(league_name = "bad", num_teams = 12)  # no league_config class
  expect_error(
    compute_vorp_rankings(proj, bad_config),
    regexp = "league_config"
  )
})
