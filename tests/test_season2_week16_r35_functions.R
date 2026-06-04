# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 16
# Test Suite: R/35 Lineup + Waiver Optimizer
# File: tests/test_season2_week16_r35_functions.R
#
# COVERAGE
# --------
#   .norm_name                   -- lowercase/punctuation strip, suffix removal,
#                                   NA and empty input, idempotent on clean input
#   .pct_rank_within             -- single-group monotonicity, two groups are
#                                   independent, NA-safe, tie averaging
#   .aging_trajectory_multiplier -- below-peak > 1, above-peak < 1, at-peak ~1,
#                                   NA age returns 1, unknown position uses fallback,
#                                   R/23 curve overrides when supplied, bounds clamp
#   .build_slot_caps             -- returns only positive slots, DEF included, FLEX
#                                   absent when zero
#   .score_def_games_standard    -- shutout + event hand calculation, NA passthrough,
#                                   adds def_st_points column
#   .game_opponent_map           -- two rows per game, home/away are inverses,
#                                   errors on missing columns
#   compute_player_values        -- redraft: value = ros_value, forward_score NA;
#                                   out_for_season collapses ros_value to 0;
#                                   dynasty: forward_vorp present and finite for offense
#   load_availability            -- all-active baseline, IR status -> out_for_season,
#                                   injury_status Out -> out_week, manual CSV overrides
#   .solve_lineup_ip             -- optimal player fills correct slot, FLEX filled
#                                   by highest-value eligible, one player per row,
#                                   infeasible returns empty tibble
#   .flag_confidence             -- non-overlapping intervals -> confident, overlapping
#                                   -> close, no alternatives -> no_alt
#   assemble_player_pool         -- DEF pseudo-player created, gsis and name-aware
#                                   roster flags, free agent is_available, roll3 blend
#
# OUT OF SCOPE
# -----------
#   build_league_source          -- requires Sleeper network (connect_sleeper_league,
#                                   get_sleeper_rosters, get_all_sleeper_players,
#                                   match_sleeper_players)
#   compute_dvp_factors          -- requires R/15 pbp cache + nflreadr::load_rosters
#   compute_def_matchup_factors  -- requires R/15 pbp cache
#   .load_week_matchups          -- requires nflreadr::load_schedules (network)
#   .apply_matchup               -- depends on .load_week_matchups
#   optimize_lineup              -- top-level orchestrator; covered by the example
#   suggest_waiver_adds          -- top-level orchestrator; covered by the example
#
# FIXTURE NOTES
# -------------
#   Tests use a 2-team synthetic league_source (make_test_source) with 4 players
#   on the user roster (gsis 1-4) and 4 on a second roster (gsis 5-8) to keep
#   fixtures small while exercising both rostered and free-agent paths. gsis 9
#   is an unrostered free agent. All gsis values are synthetic strings prefixed
#   "gsis_" so no live data is needed. def_slots = 0 in make_test_source to
#   avoid DEF slot complexity in valuation tests; .solve_lineup_ip and
#   .build_slot_caps tests use inline fixtures sized to their specific needs.
#
# DEPENDENCIES
# ------------
#   R/35_lineup_optimizer.R  (sources R/05, R/15, R/19, R/29, R/33 at load time)
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
  library(here)
})

suppressPackageStartupMessages({
  source(here::here("R", "35_lineup_optimizer.R"))
})


# ==============================================================================
# FIXTURE BUILDERS
# ==============================================================================

# Canonical synthetic gsis ids used throughout.
.TEST_GSIS <- c(
  "gsis_qb1", "gsis_rb1", "gsis_wr1", "gsis_te1",   # user roster
  "gsis_qb2", "gsis_rb2", "gsis_wr2", "gsis_te2",   # second roster
  "gsis_fa1"                                          # free agent
)

#' Minimal reconciled projections tibble (R/32 output schema).
make_test_reconciled <- function() {
  tibble::tibble(
    nfl_gsis_id             = .TEST_GSIS[1:9],
    player_name             = c("QB One","RB One","WR One","TE One",
                                "QB Two","RB Two","WR Two","TE Two","FA WR"),
    team                    = c(rep("TST", 8), "FA"),
    position                = c("QB","RB","WR","TE","QB","RB","WR","TE","WR"),
    r32_posterior_mu        = c(20, 14, 12, 10, 8, 6, 5, 4, 3),
    r32_projection_lower_80 = c(16, 10,  8,  6, 4, 3, 3, 2, 1),
    r32_projection_upper_80 = c(24, 18, 16, 14,12, 9, 7, 6, 5),
    boom_probability        = 0.20,
    bust_probability        = 0.15
  )
}

#' Minimal DEF projection tibble (R/34 output schema).
make_test_def_proj <- function() {
  tibble::tibble(
    team             = "TST",
    def_proj_ppg     = 8.5,
    def_prior_ppg    = 8.5,
    def_observed_ppg = NA_real_,
    n_observed_games = 0L,
    prior_weight     = 1,
    scoring_source   = "standard",
    schema_tag       = "test"
  )
}

#' Minimal VORP tibble (nfl_gsis_id + adjusted_vorp subset used by assemble).
make_test_vorp <- function() {
  tibble::tibble(
    nfl_gsis_id   = .TEST_GSIS[1:9],
    adjusted_vorp = c(4, 3, 2, 1, -1, -2, -3, -4, -5)
  )
}

#' Minimal pool tibble for compute_player_values tests, bypassing assemble.
make_test_pool <- function() {
  tibble::tibble(
    nfl_gsis_id      = .TEST_GSIS[1:5],
    player_name      = c("QB One","RB One","WR One","TE One","RB Inj"),
    team             = "TST",
    position         = c("QB","RB","WR","TE","RB"),
    base_proj        = c(20, 14, 12, 10, 8),
    lower_80         = c(16, 10,  8,  6, 4),
    upper_80         = c(24, 18, 16, 14,12),
    boom_probability = 0.20,
    bust_probability = 0.15,
    adjusted_vorp    = c(4, 3, 2, 1, -1),
    on_user_roster   = TRUE,
    is_available     = FALSE
  )
}

#' Minimal 2-team league_source.
#' User roster = gsis 1-4; other roster = gsis 5-8; FA = gsis 9 (unrostered).
make_test_source <- function(format = "redraft") {
  players <- tibble::tibble(
    sleeper_player_id = .TEST_GSIS[1:9],
    nfl_gsis_id       = .TEST_GSIS[1:9],
    player_name       = c("QB One","RB One","WR One","TE One",
                          "QB Two","RB Two","WR Two","TE Two","FA WR"),
    position          = c("QB","RB","WR","TE","QB","RB","WR","TE","WR"),
    team              = c(rep("TST", 8), "FA"),
    status            = "Active",
    injury_status     = NA_character_,
    age               = NA_integer_,
    is_free_agent     = c(rep(FALSE, 8), TRUE)
  )
  rosters_resolved <- tibble::tibble(
    league_id        = "test",
    roster_id        = c(rep(1L, 4), rep(2L, 4)),
    owner_id         = c(rep("own_1", 4), rep("own_2", 4)),
    player_id        = .TEST_GSIS[1:8],
    is_starter       = FALSE,
    is_reserve       = FALSE,
    on_bench         = TRUE,
    nfl_gsis_id      = .TEST_GSIS[1:8],
    match_method     = "test",
    match_confidence = "high",
    status           = "Active",
    injury_status    = NA_character_,
    age              = NA_integer_
  )
  config <- build_league_config(
    "Test League", num_teams = 2L,
    starters = list(QB = 1L, RB = 1L, WR = 1L, TE = 1L),
    flex = 0L, superflex = 0L, format = "ppr"
  )
  structure(list(
    platform          = "test",
    league_id         = "test",
    season            = 2026L,
    config            = config,
    format            = format,
    league_type       = if (format == "dynasty") 2L else 0L,
    def_slots         = 0L,
    roster_positions  = c("QB","RB","WR","TE"),
    rosters_resolved  = rosters_resolved,
    players           = players,
    all_rostered_gsis = .TEST_GSIS[1:8],
    user_roster_id    = 1L,
    user_roster_gsis  = .TEST_GSIS[1:4],
    source            = "test"
  ), class = c("league_source", "list"))
}


# ==============================================================================
# .norm_name
# ==============================================================================

test_that("[R/35] .norm_name lowercases and strips punctuation", {
  expect_equal(.norm_name("Ja'Marr Chase"), "jamarrchase")
})

test_that("[R/35] .norm_name removes common name suffixes", {
  expect_equal(.norm_name("Travis Hunter Jr"),  "travishunter")
  expect_equal(.norm_name("Deebo Samuel Sr"),   "deebosamuel")
  expect_equal(.norm_name("Travis Kelce II"),   "traviskelce")
})

test_that("[R/35] .norm_name returns empty string for NA and empty input", {
  expect_equal(.norm_name(NA_character_), "")
  expect_equal(.norm_name(""), "")
})

test_that("[R/35] .norm_name is idempotent on already-normalized input", {
  clean <- "travishunter"
  expect_equal(.norm_name(clean), clean)
})


# ==============================================================================
# .pct_rank_within
# ==============================================================================

test_that("[R/35] .pct_rank_within maps a sorted vector monotonically onto 0-1", {
  out <- .pct_rank_within(c(1, 2, 3, 4), rep("QB", 4))
  expect_equal(out, c(0, 1/3, 2/3, 1), tolerance = 1e-12)
})

test_that("[R/35] .pct_rank_within computes each group independently", {
  out <- .pct_rank_within(c(10, 8, 3, 1), c("QB","QB","WR","WR"))
  expect_equal(out[1:2], c(1, 0), tolerance = 1e-12)
  expect_equal(out[3:4], c(1, 0), tolerance = 1e-12)
})

test_that("[R/35] .pct_rank_within returns NA for NA inputs without poisoning others", {
  out <- .pct_rank_within(c(1, NA, 3), rep("RB", 3))
  expect_true(is.na(out[2]))
  expect_false(is.na(out[1]))
})

test_that("[R/35] .pct_rank_within averages tied values", {
  out <- .pct_rank_within(c(5, 5, 1), rep("WR", 3))
  expect_equal(out[1], out[2])
})


# ==============================================================================
# .aging_trajectory_multiplier
# ==============================================================================

test_that("[R/35] .aging_trajectory_multiplier returns > 1 below positional peak", {
  expect_gt(.aging_trajectory_multiplier(24, "QB"), 1)  # QB peak 29
})

test_that("[R/35] .aging_trajectory_multiplier returns < 1 above positional peak", {
  expect_lt(.aging_trajectory_multiplier(32, "RB"), 1)  # RB peak 25
})

test_that("[R/35] .aging_trajectory_multiplier returns 1 for NA age", {
  expect_equal(unname(.aging_trajectory_multiplier(NA_real_, "WR")), 1)
})

test_that("[R/35] .aging_trajectory_multiplier uses fallback for unknown position", {
  out <- .aging_trajectory_multiplier(27, "DEF")
  expect_type(out, "double")
  expect_false(is.na(out))
})

test_that("[R/35] .aging_trajectory_multiplier clamps output to AGING_MULT_BOUNDS", {
  expect_lte(.aging_trajectory_multiplier(18, "RB"), AGING_MULT_BOUNDS[2])
  expect_gte(.aging_trajectory_multiplier(45, "RB"), AGING_MULT_BOUNDS[1])
})

test_that("[R/35] .aging_trajectory_multiplier uses R/23 curve peak_age_quad when supplied", {
  fake_curves <- list(WR = list(peak_age_quad = 30L))
  out_curve   <- .aging_trajectory_multiplier(25, "WR", aging_curves = fake_curves)
  out_default <- .aging_trajectory_multiplier(25, "WR")
  # peak 30 vs default 27: age 25 is further from peak 30, so larger upside mult
  expect_gt(out_curve, out_default)
})


# ==============================================================================
# .build_slot_caps
# ==============================================================================

test_that("[R/35] .build_slot_caps returns only slots with positive capacity", {
  src  <- make_test_source()
  caps <- .build_slot_caps(src)
  expect_true(all(caps > 0))
  expect_false("FLEX"       %in% names(caps))
  expect_false("SUPER_FLEX" %in% names(caps))
  expect_false("DEF"        %in% names(caps))
})

test_that("[R/35] .build_slot_caps includes DEF when def_slots > 0", {
  src           <- make_test_source()
  src$def_slots <- 1L
  caps          <- .build_slot_caps(src)
  expect_true("DEF" %in% names(caps))
  expect_equal(unname(caps[["DEF"]]), 1)
})


# ==============================================================================
# .score_def_games_standard
# ==============================================================================

test_that("[R/35] .score_def_games_standard hand calculation: shutout + events", {
  # shutout 10 + 3 sacks 3 + 2 INT 4 + 1 fum_rec 2 + 1 def_td 6 = 25
  counts <- tibble::tibble(opponent_pts_allowed = 0, sacks = 3, def_ints = 2,
                           fum_recs = 1, def_tds = 1, safeties = 0,
                           blocked_kicks = 0)
  expect_equal(.score_def_games_standard(counts)$def_st_points, 25)
})

test_that("[R/35] .score_def_games_standard passes NA pts_allowed through as NA", {
  counts <- tibble::tibble(opponent_pts_allowed = NA_real_, sacks = 0,
                           def_ints = 0, fum_recs = 0, def_tds = 0,
                           safeties = 0, blocked_kicks = 0)
  expect_true(is.na(.score_def_games_standard(counts)$def_st_points))
})

test_that("[R/35] .score_def_games_standard adds def_st_points numeric column", {
  counts <- tibble::tibble(opponent_pts_allowed = 14, sacks = 1,
                           def_ints = 0, fum_recs = 0, def_tds = 0,
                           safeties = 0, blocked_kicks = 0)
  out <- .score_def_games_standard(counts)
  expect_true("def_st_points" %in% names(out))
  expect_type(out$def_st_points, "double")
})


# ==============================================================================
# .game_opponent_map
# ==============================================================================

test_that("[R/35] .game_opponent_map returns two rows per game", {
  pbp <- tibble::tibble(game_id = "G1", home_team = "KC", away_team = "LV",
                        season = 2025L, week = 1L)
  expect_equal(nrow(.game_opponent_map(pbp)), 2L)
})

test_that("[R/35] .game_opponent_map home and away teams are each other's opponent", {
  pbp <- tibble::tibble(game_id = "G1", home_team = "KC", away_team = "LV",
                        season = 2025L, week = 1L)
  out <- .game_opponent_map(pbp)
  expect_equal(out$opponent[out$team == "KC"], "LV")
  expect_equal(out$opponent[out$team == "LV"], "KC")
})

test_that("[R/35] .game_opponent_map errors when home_team or away_team is absent", {
  pbp <- tibble::tibble(game_id = "G1", season = 2025L, week = 1L)
  expect_error(.game_opponent_map(pbp))
})


# ==============================================================================
# compute_player_values
# ==============================================================================

test_that("[R/35] compute_player_values redraft: value equals ros_value", {
  pool <- make_test_pool()
  pool$availability_status <- "active"
  out  <- compute_player_values(pool, format = "redraft")
  expect_equal(out$value, out$ros_value)
})

test_that("[R/35] compute_player_values redraft: forward_score is NA for all rows", {
  pool <- make_test_pool()
  out  <- compute_player_values(pool, format = "redraft")
  expect_true(all(is.na(out$forward_score)))
})

test_that("[R/35] compute_player_values out_for_season collapses ros_value to 0", {
  pool  <- make_test_pool()
  avail <- tibble::tibble(
    nfl_gsis_id         = .TEST_GSIS[3],   # WR One
    availability_status = "out_for_season"
  )
  out <- compute_player_values(pool, format = "redraft", availability = avail)
  expect_equal(out$ros_value[out$nfl_gsis_id == .TEST_GSIS[3]], 0)
  expect_gt(  out$ros_value[out$nfl_gsis_id == .TEST_GSIS[1]], 0)
})

test_that("[R/35] compute_player_values dynasty: forward_vorp column is present", {
  pool <- make_test_pool()
  out  <- compute_player_values(pool, format = "dynasty")
  expect_true("forward_vorp" %in% names(out))
})

test_that("[R/35] compute_player_values dynasty: offense forward_vorp is finite", {
  pool <- make_test_pool()
  pool$availability_status <- "active"
  out  <- compute_player_values(pool, format = "dynasty")
  off  <- out[out$position %in% c("QB","RB","WR","TE"), ]
  expect_true(all(is.finite(off$forward_vorp)))
})


# ==============================================================================
# load_availability
# ==============================================================================

test_that("[R/35] load_availability defaults all active when no IR or injuries", {
  out <- load_availability(make_test_source())
  expect_true(all(out$availability_status == "active"))
})

test_that("[R/35] load_availability marks Sleeper IR status as out_for_season", {
  src <- make_test_source()
  src$players$status[1] <- "Injured Reserve"
  out <- load_availability(src)
  expect_equal(
    out$availability_status[out$nfl_gsis_id == .TEST_GSIS[1]],
    "out_for_season"
  )
})

test_that("[R/35] load_availability marks injury_status Out as out_week", {
  src <- make_test_source()
  src$players$injury_status[2] <- "Out"
  out <- load_availability(src)
  expect_equal(
    out$availability_status[out$nfl_gsis_id == .TEST_GSIS[2]],
    "out_week"
  )
})

test_that("[R/35] load_availability manual CSV override wins over inferred status", {
  src      <- make_test_source()
  ovr_path <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(gsis_id = .TEST_GSIS[1], status = "out_for_season",
                   return_week = NA_integer_),
    ovr_path
  )
  out <- load_availability(src, availability_path = ovr_path)
  expect_equal(
    out$availability_status[out$nfl_gsis_id == .TEST_GSIS[1]],
    "out_for_season"
  )
  expect_equal(
    out$avail_source[out$nfl_gsis_id == .TEST_GSIS[1]],
    "manual_override"
  )
})


# ==============================================================================
# .solve_lineup_ip
# ==============================================================================

test_that("[R/35] .solve_lineup_ip assigns the best-value player to the slot", {
  players <- tibble::tibble(nfl_gsis_id = c("qb_a","qb_b"),
                            position    = c("QB","QB"),
                            opt_value   = c(20, 10))
  out <- .solve_lineup_ip(players, c(QB = 1))
  expect_equal(nrow(out), 1L)
  expect_equal(out$nfl_gsis_id, "qb_a")
})

test_that("[R/35] .solve_lineup_ip fills FLEX with highest-value eligible player", {
  players <- tibble::tibble(nfl_gsis_id = c("rb1","rb2","wr1"),
                            position    = c("RB","RB","WR"),
                            opt_value   = c(15, 10, 12))
  out <- .solve_lineup_ip(players, c(RB = 1, FLEX = 1))
  # RB1(15) fills RB; WR1(12) fills FLEX over RB2(10)
  expect_equal(out$nfl_gsis_id[out$slot == "RB"],   "rb1")
  expect_equal(out$nfl_gsis_id[out$slot == "FLEX"],  "wr1")
})

test_that("[R/35] .solve_lineup_ip uses each player at most once", {
  players <- tibble::tibble(nfl_gsis_id = c("rb1","rb2"),
                            position    = c("RB","RB"),
                            opt_value   = c(15, 12))
  out <- .solve_lineup_ip(players, c(RB = 2))
  expect_equal(length(unique(out$nfl_gsis_id)), nrow(out))
})

test_that("[R/35] .solve_lineup_ip returns empty tibble when no eligible player exists", {
  players <- tibble::tibble(nfl_gsis_id = "qb1", position = "QB",
                            opt_value   = 20)
  out <- .solve_lineup_ip(players, c(RB = 1))
  expect_equal(nrow(out), 0L)
})


# ==============================================================================
# .flag_confidence
# ==============================================================================

test_that("[R/35] .flag_confidence confident when starter lower_80 >= alt upper_80", {
  starter <- tibble::tibble(slot = "WR", position = "WR", player_name = "S1",
                            lower_80 = 14, upper_80 = 20, adj_proj = 17,
                            adjusted_vorp = 2, confidence_flag = NA_character_)
  alt     <- tibble::tibble(position = "WR", player_name = "A1",
                            lower_80 = 6,  upper_80 = 12, adj_proj = 9,
                            adjusted_vorp = 0)
  expect_equal(.flag_confidence(starter, alt)$confidence_flag, "confident")
})

test_that("[R/35] .flag_confidence close when 80pct intervals overlap", {
  starter <- tibble::tibble(slot = "WR", position = "WR", player_name = "S1",
                            lower_80 = 12, upper_80 = 20, adj_proj = 16,
                            adjusted_vorp = 2, confidence_flag = NA_character_)
  alt     <- tibble::tibble(position = "WR", player_name = "A1",
                            lower_80 = 10, upper_80 = 18, adj_proj = 14,
                            adjusted_vorp = 0)
  expect_equal(.flag_confidence(starter, alt)$confidence_flag, "close")
})

test_that("[R/35] .flag_confidence no_alt when alternatives tibble is empty", {
  starter <- tibble::tibble(slot = "TE", position = "TE", player_name = "S1",
                            lower_80 = 8,  upper_80 = 14, adj_proj = 11,
                            adjusted_vorp = 1, confidence_flag = NA_character_)
  empty   <- tibble::tibble(position = character(), lower_80 = numeric(),
                            upper_80 = numeric(), adj_proj = numeric(),
                            player_name = character())
  expect_equal(.flag_confidence(starter, empty)$confidence_flag, "no_alt")
})


# ==============================================================================
# assemble_player_pool
# ==============================================================================

test_that("[R/35] assemble_player_pool creates a DEF pseudo-player with id DEF_<team>", {
  pool <- assemble_player_pool(make_test_source(), make_test_reconciled(),
                               make_test_vorp(), make_test_def_proj())
  def_row <- pool[pool$position == "DEF", ]
  expect_equal(nrow(def_row), 1L)
  expect_equal(def_row$nfl_gsis_id, "DEF_TST")
})

test_that("[R/35] assemble_player_pool sets on_user_roster TRUE for user players", {
  pool <- assemble_player_pool(make_test_source(), make_test_reconciled(),
                               make_test_vorp(), make_test_def_proj())
  expect_true( all(pool$on_user_roster[pool$nfl_gsis_id %in% .TEST_GSIS[1:4]]))
  expect_false(any(pool$on_user_roster[pool$nfl_gsis_id %in% .TEST_GSIS[5:8]]))
})

test_that("[R/35] assemble_player_pool marks free-agent player as is_available", {
  pool <- assemble_player_pool(make_test_source(), make_test_reconciled(),
                               make_test_vorp(), make_test_def_proj())
  expect_true( pool$is_available[pool$nfl_gsis_id == .TEST_GSIS[9]])
  expect_false(pool$is_available[pool$nfl_gsis_id == .TEST_GSIS[1]])
})

test_that("[R/35] assemble_player_pool roll3 blend shifts base_proj by weight", {
  r3   <- tibble::tibble(nfl_gsis_id = .TEST_GSIS[1], fantasy_pts_roll3 = 30)
  pool_plain <- assemble_player_pool(make_test_source(), make_test_reconciled(),
                                     make_test_vorp(), make_test_def_proj())
  pool_r3    <- assemble_player_pool(make_test_source(), make_test_reconciled(),
                                     make_test_vorp(), make_test_def_proj(),
                                     roll3 = r3, roll3_weight = 0.5)
  base_plain <- pool_plain$base_proj[pool_plain$nfl_gsis_id == .TEST_GSIS[1]]
  base_r3    <- pool_r3$base_proj[pool_r3$nfl_gsis_id    == .TEST_GSIS[1]]
  # base 20, roll3 30, weight 0.5 -> 25
  expect_equal(base_r3, 25, tolerance = 1e-10)
  expect_gt(base_r3, base_plain)
})
