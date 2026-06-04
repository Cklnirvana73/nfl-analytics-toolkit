# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Test Suite: R/30 Team Volume Projections
# File: tests/test_season2_week15_r30_functions.R
#
# COVERAGE
# --------
#   .normalize_team_codes          -- legacy code remapping (STL/OAK/SD),
#                                     current codes unchanged, NA passthrough
#   .normalize_coach_name          -- middle initial strips, suffix strips,
#                                     punctuation, whitespace collapse
#   .blend_team_and_coach_pattern  -- no-change path (100% historical),
#                                     coach-change 70/30 blend math,
#                                     coach_change_flag derivation
#   .apply_qb_quality_adjustment   -- above-average QB raises pass, lowers rush;
#                                     below-average QB reverses; neutral (0)
#                                     leaves volumes unchanged; NA quality
#                                     coalesces to 0
#   .pick_first_present            -- first present candidate, NA when none
#   .pick_starter_by_dropbacks     -- most-dropbacks QB per team, NA->0 coalesce
#   .merge_qb_starter_override     -- CSV replaces auto-detected starter
#                                     (reference-QB + direct-score modes),
#                                     NULL / missing-team CSV is a no-op
#   .score_qb_starters             -- override > computed > neutral precedence,
#                                     z-score ordering and sign, rookie/no-data
#                                     neutral, single-member pool sd guard
#
# OUT OF SCOPE
# -----------
#   .derive_coaching_changes_from_schedules  -- requires nflreadr::load_schedules
#   .detect_starters_from_depth_charts       -- requires nflreadr::load_depth_charts
#   .detect_starters_from_rosters            -- requires nflreadr::load_rosters
#   .resolve_qb_starters                     -- orchestrates the above + CSV I/O
#   .compute_team_season_aggregates          -- requires live pbp tibble
#   .compute_team_historical_volume          -- loads pbp from R/15 cache
#   .compute_qb_quality_index                -- loads pbp from R/15 cache
#   project_team_volumes                     -- top-level orchestrator
#
# DEPENDENCIES
# ------------
#   R/15_multi_season_pbp.R  (sourced automatically by R/30 at load time)
#   R/30_team_volume_projections.R
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
})

suppressPackageStartupMessages({
  source(here::here("R", "30_team_volume_projections.R"))
})


# ==============================================================================
# FIXTURE BUILDERS
# ==============================================================================

#' Build a minimal team_history tibble for .blend_team_and_coach_pattern tests.
#' All numeric columns default to round numbers for easy mental arithmetic.
make_team_history <- function(teams             = "KC",
                               historical_pass_pg  = 36.0,
                               historical_rush_pg  = 25.0,
                               historical_plays_pg = 61.0,
                               historical_proe     = 0.05,
                               historical_pass_yds_pg = 240.0,
                               historical_rush_yds_pg = 110.0,
                               historical_pass_tds_pg = 1.8,
                               historical_rush_tds_pg = 0.8) {
  n <- length(teams)
  tibble::tibble(
    team                   = teams,
    historical_pass_pg     = rep(historical_pass_pg, n),
    historical_rush_pg     = rep(historical_rush_pg, n),
    historical_plays_pg    = rep(historical_plays_pg, n),
    historical_proe        = rep(historical_proe, n),
    historical_pass_yds_pg = rep(historical_pass_yds_pg, n),
    historical_rush_yds_pg = rep(historical_rush_yds_pg, n),
    historical_pass_tds_pg = rep(historical_pass_tds_pg, n),
    historical_rush_tds_pg = rep(historical_rush_tds_pg, n)
  )
}

#' Build a coach_prior tibble for .blend_team_and_coach_pattern tests.
make_coach_prior <- function(teams         = "KC",
                              coach_pass_pg  = 40.0,
                              coach_rush_pg  = 22.0,
                              coach_plays_pg = 62.0,
                              coach_proe     = 0.08) {
  n <- length(teams)
  tibble::tibble(
    team          = teams,
    coach_pass_pg  = rep(coach_pass_pg, n),
    coach_rush_pg  = rep(coach_rush_pg, n),
    coach_plays_pg = rep(coach_plays_pg, n),
    coach_proe     = rep(coach_proe, n)
  )
}

#' Build a blended tibble for .apply_qb_quality_adjustment tests.
#' Includes all columns the function reads.
make_blended <- function(teams             = "KC",
                          blended_pass_pg    = 36.0,
                          blended_rush_pg    = 25.0,
                          blended_plays_pg   = 61.0,
                          historical_pass_yds_pg = 240.0,
                          historical_rush_yds_pg = 110.0,
                          historical_pass_tds_pg = 1.8,
                          historical_rush_tds_pg = 0.8) {
  n <- length(teams)
  tibble::tibble(
    team                   = teams,
    blended_pass_pg        = rep(blended_pass_pg, n),
    blended_rush_pg        = rep(blended_rush_pg, n),
    blended_plays_pg       = rep(blended_plays_pg, n),
    historical_pass_yds_pg = rep(historical_pass_yds_pg, n),
    historical_rush_yds_pg = rep(historical_rush_yds_pg, n),
    historical_pass_tds_pg = rep(historical_pass_tds_pg, n),
    historical_rush_tds_pg = rep(historical_rush_tds_pg, n)
  )
}

#' Build a qb_quality tibble for .apply_qb_quality_adjustment tests.
make_qb_quality <- function(teams = "KC", qb_quality_score = 0.0) {
  n <- length(teams)
  tibble::tibble(
    team             = teams,
    qb_quality_score = rep(qb_quality_score, n)
  )
}


# ==============================================================================
# .normalize_team_codes
# ==============================================================================

test_that("[R/30] .normalize_team_codes maps all three legacy codes", {
  input  <- c("STL", "OAK", "SD")
  output <- .normalize_team_codes(input)
  expect_equal(output, c("LA", "LV", "LAC"))
})

test_that("[R/30] .normalize_team_codes leaves current codes and NA unchanged", {
  input  <- c("KC", "BUF", "ARI", "LA", NA_character_)
  output <- .normalize_team_codes(input)
  # NA is tricky: the loop does out[out == old_code] which won't match NA
  expect_equal(output[1:4], c("KC", "BUF", "ARI", "LA"))
})


# ==============================================================================
# .normalize_coach_name
# ==============================================================================

test_that("[R/30] .normalize_coach_name strips single-letter middle initials with periods", {
  # "Sean P. Payton" -> strips "P. " -> "Sean Payton" -> lowercase -> "sean payton"
  input  <- c("Sean P. Payton", "Mike D. McDaniel", "Dan Q. Campbell")
  output <- .normalize_coach_name(input)
  expect_equal(output, c("sean payton", "mike mcdaniel", "dan campbell"))
})

test_that("[R/30] .normalize_coach_name strips Jr, Sr, II, III, IV suffixes", {
  input  <- c("Andy Reid Jr", "Bill Belichick II", "Jim Harbaugh III")
  output <- .normalize_coach_name(input)
  expect_equal(output, c("andy reid", "bill belichick", "jim harbaugh"))
})

test_that("[R/30] .normalize_coach_name lowercases and collapses whitespace", {
  input  <- c("KYLE  SHANAHAN", "  Robert   Saleh  ")
  output <- .normalize_coach_name(input)
  expect_equal(output, c("kyle shanahan", "robert saleh"))
  expect_false(any(grepl("  ", output)),
    info = "Double spaces should be collapsed to single")
})


# ==============================================================================
# .blend_team_and_coach_pattern
# ==============================================================================

test_that("[R/30] .blend_team_and_coach_pattern with no coach change uses historical values unchanged", {
  team_history <- make_team_history(teams = "KC")
  coach_prior  <- make_coach_prior(teams = character())  # empty -- no match

  out <- .blend_team_and_coach_pattern(team_history, coach_prior)

  expect_equal(out$coach_change_flag, FALSE)
  expect_equal(out$blended_pass_pg,  team_history$historical_pass_pg)
  expect_equal(out$blended_rush_pg,  team_history$historical_rush_pg)
  expect_equal(out$blended_plays_pg, team_history$historical_plays_pg)
  expect_equal(out$blended_proe,     team_history$historical_proe)
})

test_that("[R/30] .blend_team_and_coach_pattern coach_change_flag is TRUE when coach_pass_pg is not NA", {
  team_history <- make_team_history(teams = "NO")
  coach_prior  <- make_coach_prior(teams = "NO")

  out <- .blend_team_and_coach_pattern(team_history, coach_prior)
  expect_equal(out$coach_change_flag, TRUE)
})

test_that("[R/30] .blend_team_and_coach_pattern applies 70/30 blend when coach change is present", {
  # historical_pass_pg = 36.0, coach_pass_pg = 40.0
  # expected: 0.70 * 36 + 0.30 * 40 = 25.2 + 12.0 = 37.2
  team_history <- make_team_history(teams = "NO",
                                     historical_pass_pg  = 36.0,
                                     historical_rush_pg  = 25.0,
                                     historical_plays_pg = 61.0,
                                     historical_proe     = 0.04)
  coach_prior  <- make_coach_prior(teams = "NO",
                                    coach_pass_pg  = 40.0,
                                    coach_rush_pg  = 20.0,
                                    coach_plays_pg = 60.0,
                                    coach_proe     = 0.10)

  out <- .blend_team_and_coach_pattern(team_history, coach_prior)

  expect_equal(out$blended_pass_pg,
    0.70 * 36.0 + 0.30 * 40.0,
    tolerance = 1e-10)
  expect_equal(out$blended_rush_pg,
    0.70 * 25.0 + 0.30 * 20.0,
    tolerance = 1e-10)
  expect_equal(out$blended_plays_pg,
    0.70 * 61.0 + 0.30 * 60.0,
    tolerance = 1e-10)
  expect_equal(out$blended_proe,
    0.70 * 0.04 + 0.30 * 0.10,
    tolerance = 1e-10)
})


# ==============================================================================
# .apply_qb_quality_adjustment
# Constants:
#   QB_VOLUME_SENSITIVITY     = 0.015
#   QB_EFFICIENCY_SENSITIVITY = 0.015
# Formulas:
#   adj_pass  = 1 + qb_score * 0.015
#   adj_eff   = 1 + qb_score * 0.015
#   proj_pass = blended_pass_pg * adj_pass
#   proj_rush = blended_rush_pg * (2 - adj_pass)
#   proj_pass_yds = historical_pass_yds_pg * adj_eff
#   proj_pass_tds = historical_pass_tds_pg * adj_eff
# ==============================================================================

test_that("[R/30] .apply_qb_quality_adjustment above-average QB increases pass and decreases rush volume", {
  blended    <- make_blended(blended_pass_pg = 35.0, blended_rush_pg = 25.0)
  qb_quality <- make_qb_quality(qb_quality_score = 1.0)

  out <- .apply_qb_quality_adjustment(blended, qb_quality)

  expect_gt(out$projected_pass_pg, blended$blended_pass_pg,
    label = "Above-average QB should increase projected pass volume")
  expect_lt(out$projected_rush_pg, blended$blended_rush_pg,
    label = "Above-average QB should decrease projected rush volume")
})

test_that("[R/30] .apply_qb_quality_adjustment neutral QB score=0 leaves pass and rush volumes unchanged", {
  blended    <- make_blended(blended_pass_pg = 35.0, blended_rush_pg = 25.0)
  qb_quality <- make_qb_quality(qb_quality_score = 0.0)

  out <- .apply_qb_quality_adjustment(blended, qb_quality)

  expect_equal(out$projected_pass_pg, 35.0, tolerance = 1e-10)
  expect_equal(out$projected_rush_pg, 25.0, tolerance = 1e-10)
})

test_that("[R/30] .apply_qb_quality_adjustment pass volume math matches formula: blended * (1 + score * 0.015)", {
  blended    <- make_blended(blended_pass_pg = 35.0)
  qb_quality <- make_qb_quality(qb_quality_score = 2.0)

  out <- .apply_qb_quality_adjustment(blended, qb_quality)

  expected_pass <- 35.0 * (1 + 2.0 * QB_VOLUME_SENSITIVITY)
  expect_equal(out$projected_pass_pg, expected_pass, tolerance = 1e-10)
})

test_that("[R/30] .apply_qb_quality_adjustment NA QB quality coalesces to 0 and leaves volumes unchanged", {
  blended    <- make_blended(blended_pass_pg = 35.0, blended_rush_pg = 25.0)
  # qb_quality table has no row for KC -> left join produces NA -> coalesces to 0
  qb_quality <- make_qb_quality(teams = "BUF", qb_quality_score = 1.5)

  out <- .apply_qb_quality_adjustment(blended, qb_quality)

  expect_equal(out$projected_pass_pg, 35.0, tolerance = 1e-10,
    info = "Missing QB quality should coalesce to 0 -- no adjustment applied")
  expect_equal(out$projected_rush_pg, 25.0, tolerance = 1e-10)
})

test_that("[R/30] .apply_qb_quality_adjustment efficiency metrics scale by (1 + score * 0.015)", {
  blended    <- make_blended(historical_pass_yds_pg = 240.0,
                              historical_pass_tds_pg = 1.8)
  qb_quality <- make_qb_quality(qb_quality_score = 1.0)

  out <- .apply_qb_quality_adjustment(blended, qb_quality)

  expected_yds <- 240.0 * (1 + 1.0 * QB_EFFICIENCY_SENSITIVITY)
  expected_tds <- 1.8   * (1 + 1.0 * QB_EFFICIENCY_SENSITIVITY)
  expect_equal(out$projected_pass_yds_pg, expected_yds, tolerance = 1e-10)
  expect_equal(out$projected_pass_tds_pg, expected_tds, tolerance = 1e-10)
})

# ==============================================================================
# QB STARTER RESOLUTION -- PURE HELPER FIXTURES
# ==============================================================================

#' Build a resolved-starter-with-stats tibble for .score_qb_starters tests.
#' One row per team; columns match what .compute_qb_quality_index passes in.
make_starter_stats <- function(team,
                                starter_qb_id,
                                qb_quality_override,
                                n_dropbacks,
                                qb_cpoe_mean,
                                qb_epa_per_db,
                                starter_source = "depth_chart") {
  tibble::tibble(
    team                = team,
    starter_qb_id       = starter_qb_id,
    starter_source      = rep(starter_source, length(team)),
    qb_quality_override = qb_quality_override,
    n_dropbacks         = n_dropbacks,
    qb_cpoe_mean        = qb_cpoe_mean,
    qb_epa_per_db       = qb_epa_per_db
  )
}


# ==============================================================================
# .pick_first_present
# ==============================================================================

test_that("[R/30] .pick_first_present returns the first candidate present", {
  expect_equal(.pick_first_present(c("a", "b"), c("z", "b")), "b")
  expect_equal(
    .pick_first_present(c("team", "club_code"), c("club_code", "x")),
    "club_code"
  )
})

test_that("[R/30] .pick_first_present returns NA when no candidate present", {
  expect_true(is.na(.pick_first_present(c("a", "b"), c("z"))))
})

test_that("[R/30] .pick_first_present honors candidate priority order", {
  # both present -> first candidate wins
  expect_equal(
    .pick_first_present(c("gsis_id", "player_id"), c("player_id", "gsis_id")),
    "gsis_id"
  )
})


# ==============================================================================
# .pick_starter_by_dropbacks
# ==============================================================================

test_that("[R/30] .pick_starter_by_dropbacks picks the most-dropback QB per team", {
  qbs <- tibble::tibble(
    team        = c("KC", "KC", "BUF"),
    qb_id       = c("mahomes", "kc_backup", "allen"),
    n_dropbacks = c(600, 40, 580)
  )
  out <- .pick_starter_by_dropbacks(qbs)
  expect_equal(nrow(out), 2L)
  expect_equal(out$starter_qb_id[out$team == "KC"], "mahomes")
  expect_equal(out$starter_qb_id[out$team == "BUF"], "allen")
})

test_that("[R/30] .pick_starter_by_dropbacks coalesces NA dropbacks to 0", {
  # BUF has one QB with NA dropbacks -> still selected (no other option)
  qbs <- tibble::tibble(
    team        = c("BUF", "BUF"),
    qb_id       = c("allen", "buf_rookie"),
    n_dropbacks = c(580, NA)
  )
  out <- .pick_starter_by_dropbacks(qbs)
  expect_equal(out$starter_qb_id[out$team == "BUF"], "allen")
})

test_that("[R/30] .pick_starter_by_dropbacks returns empty on empty input", {
  out <- .pick_starter_by_dropbacks(
    tibble::tibble(team = character(), qb_id = character(),
                   n_dropbacks = numeric())
  )
  expect_equal(nrow(out), 0L)
  expect_true(all(c("team", "starter_qb_id") %in% names(out)))
})


# ==============================================================================
# .merge_qb_starter_override
# ==============================================================================

test_that("[R/30] .merge_qb_starter_override replaces auto-detected starter (reference-QB mode)", {
  starters <- tibble::tibble(
    team                = c("KC", "NYG"),
    starter_qb_id       = c("mahomes", "auto_nyg"),
    starter_source      = c("depth_chart", "roster_fallback"),
    qb_quality_override = c(NA_real_, NA_real_)
  )
  csv <- tibble::tibble(team = "NYG", qb_id = "traded_in_qb")

  out <- .merge_qb_starter_override(starters, csv)
  nyg <- out[out$team == "NYG", ]
  kc  <- out[out$team == "KC", ]

  expect_equal(nrow(out), 2L)
  expect_equal(nyg$starter_qb_id, "traded_in_qb")
  expect_equal(nyg$starter_source, "csv_override")
  expect_true(is.na(nyg$qb_quality_override))
  # untouched team retains its auto-detected entry
  expect_equal(kc$starter_source, "depth_chart")
})

test_that("[R/30] .merge_qb_starter_override carries a direct-score override", {
  starters <- tibble::tibble(
    team                = "LV",
    starter_qb_id       = "auto_lv",
    starter_source      = "depth_chart",
    qb_quality_override = NA_real_
  )
  csv <- tibble::tibble(team = "LV", qb_quality_override = 0.8)

  out <- .merge_qb_starter_override(starters, csv)
  expect_equal(out$starter_source, "csv_override")
  expect_equal(out$qb_quality_override, 0.8, tolerance = 1e-10)
})

test_that("[R/30] .merge_qb_starter_override is a no-op for NULL or missing-team CSV", {
  starters <- tibble::tibble(
    team                = "KC",
    starter_qb_id       = "mahomes",
    starter_source      = "depth_chart",
    qb_quality_override = NA_real_
  )
  expect_identical(.merge_qb_starter_override(starters, NULL), starters)
  expect_identical(
    .merge_qb_starter_override(starters, tibble::tibble(foo = 1)),
    starters
  )
})

test_that("[R/30] .merge_qb_starter_override drops override rows for inactive teams", {
  starters <- tibble::tibble(
    team                = "KC",
    starter_qb_id       = "mahomes",
    starter_source      = "depth_chart",
    qb_quality_override = NA_real_
  )
  # "ZZZ" is not in ACTIVE_TEAMS_2026 -> dropped, KC left intact
  csv <- tibble::tibble(team = "ZZZ", qb_id = "ghost")
  out <- .merge_qb_starter_override(starters, csv)
  expect_equal(out$team, "KC")
  expect_equal(out$starter_source, "depth_chart")
})


# ==============================================================================
# .score_qb_starters
# ==============================================================================

test_that("[R/30] .score_qb_starters returns the documented schema", {
  ss <- make_starter_stats(
    team = "KC", starter_qb_id = "a", qb_quality_override = NA_real_,
    n_dropbacks = 600, qb_cpoe_mean = 3, qb_epa_per_db = 0.15
  )
  out <- .score_qb_starters(ss)
  expect_true(all(c("team", "qb_id", "qb_dropbacks_share", "qb_cpoe_mean",
                    "qb_epa_per_db", "cpoe_z", "epa_z", "qb_quality_score")
                  %in% names(out)))
  # qb_id is mapped from the resolved starter; share is NA under player-centric
  expect_equal(out$qb_id, "a")
  expect_true(is.na(out$qb_dropbacks_share))
})

test_that("[R/30] .score_qb_starters uses a direct override verbatim", {
  ss <- make_starter_stats(
    team = "LV", starter_qb_id = "ovr", qb_quality_override = 1.5,
    n_dropbacks = NA_real_, qb_cpoe_mean = NA_real_, qb_epa_per_db = NA_real_,
    starter_source = "csv_override"
  )
  out <- .score_qb_starters(ss)
  expect_equal(out$qb_quality_score, 1.5, tolerance = 1e-10)
})

test_that("[R/30] .score_qb_starters gives a rookie with no dropbacks neutral 0", {
  ss <- make_starter_stats(
    team = "NYG", starter_qb_id = "rookie", qb_quality_override = NA_real_,
    n_dropbacks = 0, qb_cpoe_mean = NA_real_, qb_epa_per_db = NA_real_,
    starter_source = "roster_fallback"
  )
  out <- .score_qb_starters(ss)
  expect_equal(out$qb_quality_score, 0)
  expect_equal(out$cpoe_z, 0)
  expect_equal(out$epa_z, 0)
})

test_that("[R/30] .score_qb_starters z-scores qualifying starters in the right order", {
  # three qualifying starters spread above/at/below the mean
  ss <- make_starter_stats(
    team          = c("KC", "BUF", "LA"),
    starter_qb_id = c("a", "b", "c"),
    qb_quality_override = c(NA_real_, NA_real_, NA_real_),
    n_dropbacks   = c(600, 550, 500),
    qb_cpoe_mean  = c(4, 0, -4),
    qb_epa_per_db = c(0.2, 0.0, -0.2)
  )
  out <- .score_qb_starters(ss)
  kc  <- out$qb_quality_score[out$team == "KC"]
  buf <- out$qb_quality_score[out$team == "BUF"]
  la  <- out$qb_quality_score[out$team == "LA"]
  expect_gt(kc, buf)
  expect_gt(buf, la)
  expect_gt(kc, 0)
  expect_lt(la, 0)
})

test_that("[R/30] .score_qb_starters below-threshold starter does not enter the z pool", {
  # 'sub' has only 50 dropbacks (< MIN_DROPBACKS_QB) -> neutral, and is
  # excluded from the mean/sd so it cannot distort the qualifying QB's z-score
  ss <- make_starter_stats(
    team          = c("KC", "LV"),
    starter_qb_id = c("a", "sub"),
    qb_quality_override = c(NA_real_, NA_real_),
    n_dropbacks   = c(600, 50),
    qb_cpoe_mean  = c(2, 99),
    qb_epa_per_db = c(0.1, 99)
  )
  out <- .score_qb_starters(ss)
  expect_equal(out$qb_quality_score[out$team == "LV"], 0)
  # KC is the only pool member -> sd guard -> z = 0, score finite
  expect_true(is.finite(out$qb_quality_score[out$team == "KC"]))
})

test_that("[R/30] .score_qb_starters single-member pool is finite via sd guard", {
  ss <- make_starter_stats(
    team = "KC", starter_qb_id = "a", qb_quality_override = NA_real_,
    n_dropbacks = 300, qb_cpoe_mean = 2, qb_epa_per_db = 0.1
  )
  out <- .score_qb_starters(ss)
  expect_true(is.finite(out$qb_quality_score))
  expect_equal(out$qb_quality_score, 0)  # lone member sits at its own mean
})
