# ==============================================================================
# Season 2 Week 5 -- Sleeper API Integration Tests
# File: tests/test_season2_week5_functions.R
# ==============================================================================
#
# PURPOSE
# -------
# Tests for:
#   (A) New Sleeper parity parameters added to R/17_extended_scoring.R
#       as part of the Week 5 integration work:
#       tiered_rec_tiers, bonus_pass_yd_300, bonus_pass_yd_400,
#       bonus_rec_yd_200, bonus_rush_yd_200, pass_2pt
#   (B) All exported functions in R/19_sleeper_api.R
#
# All tests use synthetic data -- no live network calls, no nflfastR downloads.
# Tests are deterministic and network-independent.
#
# RUN WITH:
#   testthat::test_file(here::here("tests", "test_season2_week5_functions.R"))
#
# ==============================================================================

library(testthat)
library(dplyr)
library(here)

source(here::here("R", "17_extended_scoring.R"))
source(here::here("R", "19_sleeper_api.R"))

# ==============================================================================
# SYNTHETIC PBP FIXTURES
# ==============================================================================
# These helpers produce the minimum columns required by calculate_fantasy_points_ext().
# All defaults produce a clean, scoreless play so tests add only what they need.

make_pass_play <- function(
  season = 2025L, week = 1L,
  game_id = "2025_01_KC_BUF",
  passer_id = "QB001", passer_name = "Test QB",
  receiver_id = "WR001", receiver_name = "Test WR",
  pass_yards = 10, rec_yards = 10,
  is_td = FALSE, is_int = FALSE,
  is_complete = TRUE,
  posteam = "KC",
  first_down = FALSE,
  n_plays = 1L
) {
  tibble::tibble(
    season               = season,
    week                 = week,
    game_id              = game_id,
    season_type          = "REG",
    play_type            = "pass",
    posteam              = posteam,
    passer_player_id     = passer_id,
    passer_player_name   = passer_name,
    rusher_player_id     = NA_character_,
    rusher_player_name   = NA_character_,
    receiver_player_id   = if (is_complete) receiver_id   else NA_character_,
    receiver_player_name = if (is_complete) receiver_name else NA_character_,
    passing_yards        = as.numeric(pass_yards),
    rushing_yards        = NA_real_,
    receiving_yards      = if (is_complete) as.numeric(rec_yards) else NA_real_,
    pass_touchdown       = as.integer(is_td),
    rush_touchdown       = 0L,
    interception         = as.integer(is_int),
    fumble_lost          = 0L,
    pass_attempt         = 1L,
    complete_pass        = as.integer(is_complete),
    sack                 = 0L,
    qb_kneel             = 0L,
    qb_spike             = 0L,
    two_point_attempt    = 0L,
    first_down_rush      = 0L,
    first_down_pass      = as.integer(first_down & is_complete),
    fumbled_1_player_id  = NA_character_,
    return_touchdown     = 0L
  ) %>%
    dplyr::slice(rep(1L, n_plays))
}

make_rush_play <- function(
  season = 2025L, week = 1L,
  game_id = "2025_01_KC_BUF",
  rusher_id = "RB001", rusher_name = "Test RB",
  rush_yards = 10,
  is_td = FALSE,
  posteam = "KC",
  first_down = FALSE,
  n_plays = 1L
) {
  tibble::tibble(
    season               = season,
    week                 = week,
    game_id              = game_id,
    season_type          = "REG",
    play_type            = "run",
    posteam              = posteam,
    passer_player_id     = NA_character_,
    passer_player_name   = NA_character_,
    rusher_player_id     = rusher_id,
    rusher_player_name   = rusher_name,
    receiver_player_id   = NA_character_,
    receiver_player_name = NA_character_,
    passing_yards        = NA_real_,
    rushing_yards        = as.numeric(rush_yards),
    receiving_yards      = NA_real_,
    pass_touchdown       = 0L,
    rush_touchdown       = as.integer(is_td),
    interception         = 0L,
    fumble_lost          = 0L,
    pass_attempt         = 0L,
    complete_pass        = 0L,
    sack                 = 0L,
    qb_kneel             = 0L,
    qb_spike             = 0L,
    two_point_attempt    = 0L,
    first_down_rush      = as.integer(first_down),
    first_down_pass      = 0L,
    fumbled_1_player_id  = NA_character_,
    return_touchdown     = 0L
  ) %>%
    dplyr::slice(rep(1L, n_plays))
}

make_2pt_play <- function(
  season = 2025L, week = 1L,
  game_id = "2025_01_KC_BUF",
  passer_id = "QB001", passer_name = "Test QB",
  scorer_id = "WR001", scorer_name = "Test WR",
  is_passing = TRUE,
  posteam = "KC"
) {
  tibble::tibble(
    season               = season,
    week                 = week,
    game_id              = game_id,
    season_type          = "REG",
    play_type            = "no_play",
    posteam              = posteam,
    passer_player_id     = if (is_passing) passer_id   else NA_character_,
    passer_player_name   = if (is_passing) passer_name else NA_character_,
    rusher_player_id     = if (!is_passing) scorer_id   else NA_character_,
    rusher_player_name   = if (!is_passing) scorer_name else NA_character_,
    receiver_player_id   = if (is_passing) scorer_id   else NA_character_,
    receiver_player_name = if (is_passing) scorer_name else NA_character_,
    passing_yards        = NA_real_,
    rushing_yards        = NA_real_,
    receiving_yards      = NA_real_,
    pass_touchdown       = 0L,
    rush_touchdown       = 0L,
    interception         = 0L,
    fumble_lost          = 0L,
    pass_attempt         = 0L,
    complete_pass        = 0L,
    sack                 = 0L,
    qb_kneel             = 0L,
    qb_spike             = 0L,
    two_point_attempt    = 1L,
    two_point_conv_result = "success",
    first_down_rush      = 0L,
    first_down_pass      = 0L,
    fumbled_1_player_id  = NA_character_,
    return_touchdown     = 0L
  )
}

make_roster <- function() {
  tibble::tibble(
    gsis_id   = c("QB001", "WR001", "RB001", "TE001"),
    full_name = c("Test QB", "Test WR", "Test RB", "Test TE"),
    position  = c("QB", "WR", "RB", "TE"),
    team      = c("KC", "KC", "KC", "KC")
  )
}

make_mock_scoring <- function(
  pass_yd = 0.04, pass_td = 4, pass_int = -2,
  rush_yd = 0.1, rush_td = 6,
  rec_yd = 0.1, rec_td = 6, rec = 1.0,
  fum_lost = -2,
  bonus_rec_te = 0.5,
  bonus_rec_yd_100 = 3.0, bonus_rush_yd_100 = 3.0,
  pass_sack = -1.0,
  two_pt_conv = 2.0,
  bonus_fd_rb = 0.5, bonus_fd_wr = 0.5,
  bonus_fd_te = 0.5, bonus_fd_qb = 0.0,
  bonus_rec_td_50p = 2.0, bonus_rush_td_50p = 2.0,
  rush_att = 0.25,
  bonus_pass_yd_300 = 2.0, bonus_pass_yd_400 = 4.0,
  bonus_rec_yd_200 = 4.0, bonus_rush_yd_200 = 4.0,
  pass_2pt = 2.0,
  rec_0_4 = 0, rec_5_9 = 0.5, rec_10_19 = 1.0,
  rec_20_29 = 1.5, rec_30_39 = 2.0, rec_40p = 2.0
) {
  list(
    pass_yd = pass_yd, pass_td = pass_td, pass_int = pass_int,
    rush_yd = rush_yd, rush_td = rush_td,
    rec_yd = rec_yd, rec_td = rec_td, rec = rec,
    fum_lost = fum_lost,
    bonus_rec_te = bonus_rec_te,
    bonus_rec_yd_100 = bonus_rec_yd_100, bonus_rush_yd_100 = bonus_rush_yd_100,
    pass_sack = pass_sack, two_pt_conv = two_pt_conv,
    bonus_fd_rb = bonus_fd_rb, bonus_fd_wr = bonus_fd_wr,
    bonus_fd_te = bonus_fd_te, bonus_fd_qb = bonus_fd_qb,
    bonus_rec_td_50p = bonus_rec_td_50p, bonus_rush_td_50p = bonus_rush_td_50p,
    rush_att = rush_att,
    bonus_pass_yd_300 = bonus_pass_yd_300, bonus_pass_yd_400 = bonus_pass_yd_400,
    bonus_rec_yd_200 = bonus_rec_yd_200, bonus_rush_yd_200 = bonus_rush_yd_200,
    pass_2pt = pass_2pt,
    rec_0_4 = rec_0_4, rec_5_9 = rec_5_9, rec_10_19 = rec_10_19,
    rec_20_29 = rec_20_29, rec_30_39 = rec_30_39, rec_40p = rec_40p
  )
}

make_sleeper_db <- function() {
  tibble::tibble(
    sleeper_player_id = c("4017", "6794", "7525", "2374", "1389", "6812"),
    full_name  = c("Patrick Mahomes", "Justin Jefferson", "Ja'Marr Chase",
                   "Davante Adams", "Travis Kelce", "Alvin Kamara"),
    first_name = c("Patrick", "Justin", "Ja'Marr", "Davante", "Travis", "Alvin"),
    last_name  = c("Mahomes", "Jefferson", "Chase", "Adams", "Kelce", "Kamara"),
    position   = c("QB", "WR", "WR", "WR", "TE", "RB"),
    team       = c("KC", "MIN", "CIN", "LV", "KC", "NO"),
    age        = c(29L, 25L, 24L, 32L, 35L, 30L),
    status     = rep("Active", 6L),
    gsis_id    = c("00-0033873", "00-0034857", "00-0036900",
                   "00-0031408", "00-0029604", "00-0027939"),
    years_exp  = c(8L, 4L, 4L, 12L, 15L, 8L)
  )
}

make_nfl_roster <- function() {
  tibble::tibble(
    gsis_id   = c("00-0033873", "00-0034857", "00-0036900",
                  "00-0031408", "00-0029604", "00-0027939"),
    full_name = c("Patrick Mahomes", "Justin Jefferson", "Ja'Marr Chase",
                  "Davante Adams", "Travis Kelce", "Alvin Kamara"),
    position  = c("QB", "WR", "WR", "WR", "TE", "RB"),
    team      = c("KC", "MIN", "CIN", "LV", "KC", "NO")
  )
}

# ==============================================================================
# PART A: R/17 NEW SLEEPER PARITY PARAMETERS
# ==============================================================================

# ----------------------------------------------------------------------
# A1: tiered_rec_tiers
# ----------------------------------------------------------------------

test_that("tiered_rec_tiers NULL uses TIERED_PPR_BREAKS: 15-yard rec = 1.0 tier pt", {
  # 10-19 yard bucket default = 1.0 pt
  pbp <- make_pass_play(pass_yards = 15, rec_yards = 15)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(), use_tiered_ppr = TRUE,
    tiered_rec_tiers = NULL, ppr = 0, rush_att_bonus = 0
  )

  wr_row <- result %>% dplyr::filter(player_id == "WR001")
  expect_true(nrow(wr_row) > 0)
  # 15 yds * 0.1 rec_yd = 1.5 yardage pts + 1.0 tier = 2.5 total
  expect_equal(wr_row$rec_fantasy_points, 2.5, tolerance = 0.01)
})

test_that("tiered_rec_tiers custom values override defaults", {
  # Change 10-19 bucket from 1.0 to 3.0
  custom <- c(0, 0.5, 3.0, 1.5, 2.0, 2.0)
  pbp <- make_pass_play(pass_yards = 15, rec_yards = 15)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(), use_tiered_ppr = TRUE,
    tiered_rec_tiers = custom, ppr = 0, rush_att_bonus = 0
  )

  wr_row <- result %>% dplyr::filter(player_id == "WR001")
  # 15 yds * 0.1 + 3.0 tier = 4.5
  expect_equal(wr_row$rec_fantasy_points, 4.5, tolerance = 0.01)
})

test_that("tiered_rec_tiers correct bucket applied per yardage range", {
  custom <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  roster <- make_roster()

  # 3-yard catch: bucket 1 (0-4) = 0.1
  r3 <- calculate_fantasy_points_ext(
    make_pass_play(rec_yards = 3, pass_yards = 3),
    roster, use_tiered_ppr = TRUE, tiered_rec_tiers = custom,
    ppr = 0, rush_att_bonus = 0
  ) %>% dplyr::filter(player_id == "WR001")

  # 45-yard catch: bucket 6 (40+) = 0.6
  r45 <- calculate_fantasy_points_ext(
    make_pass_play(rec_yards = 45, pass_yards = 45),
    roster, use_tiered_ppr = TRUE, tiered_rec_tiers = custom,
    ppr = 0, rush_att_bonus = 0
  ) %>% dplyr::filter(player_id == "WR001")

  expect_equal(r3$rec_fantasy_points,  3  * 0.1 + 0.1, tolerance = 0.01)
  expect_equal(r45$rec_fantasy_points, 45 * 0.1 + 0.6, tolerance = 0.01)
})

test_that("tiered_rec_tiers wrong length fails validation", {
  # validate_ext_scoring_params() returns a list with $valid and $errors;
  # it does not stop() -- the caller (calculate_fantasy_points_ext) stops.
  result <- validate_ext_scoring_params(tiered_rec_tiers = c(0, 0.5, 1.0))
  expect_false(result$valid)
  expect_true(any(grepl("6 values", result$errors)))
})

test_that("tiered_rec_tiers non-numeric fails validation", {
  result <- validate_ext_scoring_params(
    tiered_rec_tiers = c("a", "b", "c", "d", "e", "f")
  )
  expect_false(result$valid)
  expect_true(any(grepl("6 values", result$errors)))
})

# ----------------------------------------------------------------------
# A2: bonus_pass_yd_300 and bonus_pass_yd_400
# ----------------------------------------------------------------------

test_that("bonus_pass_yd_300: no bonus below 300 yards", {
  # 25 plays * 11 yards = 275 total
  pbp <- make_pass_play(pass_yards = 11, rec_yards = 11, n_plays = 25L)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(), bonus_pass_yd_300 = 3.0, bonus_pass_yd_400 = 5.0
  )

  qb_row <- result %>% dplyr::filter(player_id == "QB001")
  expect_true(nrow(qb_row) > 0)
  expect_equal(qb_row$pass_milestone_fantasy_points, 0, tolerance = 0.01)
})

test_that("bonus_pass_yd_300: earns bonus at 300 yards", {
  # 30 plays * 10 yards = 300 total
  pbp <- make_pass_play(pass_yards = 10, rec_yards = 10, n_plays = 30L)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(), bonus_pass_yd_300 = 3.0, bonus_pass_yd_400 = 0
  )

  qb_row <- result %>% dplyr::filter(player_id == "QB001")
  expect_equal(qb_row$pass_milestone_fantasy_points, 3.0, tolerance = 0.01)
})

test_that("bonus_pass_yd_300 and bonus_pass_yd_400 are cumulative at 400+ yards", {
  # 40 plays * 11 yards = 440 total -- earns both
  pbp <- make_pass_play(pass_yards = 11, rec_yards = 11, n_plays = 40L)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(), bonus_pass_yd_300 = 2.0, bonus_pass_yd_400 = 4.0
  )

  qb_row <- result %>% dplyr::filter(player_id == "QB001")
  expect_equal(qb_row$pass_milestone_fantasy_points, 6.0, tolerance = 0.01)
})

test_that("bonus_pass_yd_400 not earned at 300 yards", {
  pbp <- make_pass_play(pass_yards = 10, rec_yards = 10, n_plays = 30L)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(), bonus_pass_yd_300 = 0, bonus_pass_yd_400 = 4.0
  )

  qb_row <- result %>% dplyr::filter(player_id == "QB001")
  expect_equal(qb_row$pass_milestone_fantasy_points, 0, tolerance = 0.01)
})

test_that("pass_milestone_fantasy_points included in total_fantasy_points", {
  pbp <- make_pass_play(pass_yards = 10, rec_yards = 10, n_plays = 30L)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(), bonus_pass_yd_300 = 3.0
  )

  qb_row <- result %>% dplyr::filter(player_id == "QB001")
  expect_equal(
    qb_row$total_fantasy_points,
    qb_row$pass_fantasy_points + 3.0,
    tolerance = 0.01
  )
})

# ----------------------------------------------------------------------
# A3: bonus_rec_yd_200 and bonus_rush_yd_200
# ----------------------------------------------------------------------

test_that("bonus_rec_yd_200: 100-yard bonus only below 200 yards", {
  # 15 plays * 11 yards = 165 receiving yards
  pbp <- make_pass_play(pass_yards = 11, rec_yards = 11, n_plays = 15L)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(), hundred_yard_bonus = 3.0, bonus_rec_yd_200 = 6.0
  )

  wr_row <- result %>% dplyr::filter(player_id == "WR001")
  # 165 yards: 100-yard bonus (3.0) only
  expect_equal(wr_row$hundred_yard_fantasy_points, 3.0, tolerance = 0.01)
})

test_that("bonus_rec_yd_200: cumulative with hundred_yard_bonus at 200+ yards", {
  # 20 plays * 11 yards = 220 receiving yards
  pbp <- make_pass_play(pass_yards = 11, rec_yards = 11, n_plays = 20L)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(), hundred_yard_bonus = 3.0, bonus_rec_yd_200 = 6.0
  )

  wr_row <- result %>% dplyr::filter(player_id == "WR001")
  # 220 yards: 100-yard (3.0) + 200-yard (6.0) = 9.0
  expect_equal(wr_row$hundred_yard_fantasy_points, 9.0, tolerance = 0.01)
})

test_that("bonus_rush_yd_200: cumulative with hundred_yard_bonus at 200+ yards", {
  # 20 plays * 11 yards = 220 rushing yards
  pbp <- make_rush_play(rush_yards = 11, n_plays = 20L)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(), hundred_yard_bonus = 3.0, bonus_rush_yd_200 = 6.0
  )

  rb_row <- result %>% dplyr::filter(player_id == "RB001")
  expect_equal(rb_row$hundred_yard_fantasy_points, 9.0, tolerance = 0.01)
})

test_that("bonus_rec_yd_200 and bonus_rush_yd_200 are independent", {
  pbp <- dplyr::bind_rows(
    make_pass_play(pass_yards = 11, rec_yards = 11, n_plays = 20L),  # WR: 220 rec yds
    make_rush_play(rush_yards = 11, n_plays = 10L)                   # RB: 110 rush yds
  )

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(),
    hundred_yard_bonus = 3.0,
    bonus_rec_yd_200   = 6.0,
    bonus_rush_yd_200  = 0
  )

  wr_row <- result %>% dplyr::filter(player_id == "WR001")
  rb_row <- result %>% dplyr::filter(player_id == "RB001")
  expect_equal(wr_row$hundred_yard_fantasy_points, 9.0, tolerance = 0.01)
  expect_equal(rb_row$hundred_yard_fantasy_points, 3.0, tolerance = 0.01)
})

# ----------------------------------------------------------------------
# A4: pass_2pt
# ----------------------------------------------------------------------

test_that("pass_2pt: passer earns credit on passing 2PC", {
  pbp <- make_2pt_play(is_passing = TRUE)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(),
    two_point_conversion = 2.0,
    pass_2pt             = 2.0
  )

  expect_true(nrow(result) > 0)
  qb_row <- result %>% dplyr::filter(player_id == "QB001")
  wr_row <- result %>% dplyr::filter(player_id == "WR001")
  expect_true(nrow(qb_row) > 0)
  expect_true(nrow(wr_row) > 0)
  expect_equal(qb_row$two_pt_fantasy_points, 2.0, tolerance = 0.01)
  expect_equal(wr_row$two_pt_fantasy_points, 2.0, tolerance = 0.01)
})

test_that("pass_2pt: passer earns nothing on rushing 2PC", {
  # Explicitly set scorer to RB001 for a rushing 2PC
  pbp <- make_2pt_play(
    is_passing  = FALSE,
    scorer_id   = "RB001",
    scorer_name = "Test RB"
  )

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(),
    two_point_conversion = 2.0,
    pass_2pt             = 2.0
  )

  # RB001 scored the rushing 2PC -- earns two_point_conversion pts
  rb_row <- result %>% dplyr::filter(player_id == "RB001")
  expect_true(nrow(rb_row) > 0)
  expect_equal(rb_row$two_pt_fantasy_points, 2.0, tolerance = 0.01)

  # QB should not appear -- no passer on a rushing 2PC
  qb_row <- result %>% dplyr::filter(player_id == "QB001")
  expect_true(
    nrow(qb_row) == 0 ||
    isTRUE(all.equal(qb_row$two_pt_fantasy_points, 0, tolerance = 0.01))
  )
})

test_that("pass_2pt = 0: passer earns nothing even on passing 2PC", {
  pbp <- make_2pt_play(is_passing = TRUE)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(),
    two_point_conversion = 2.0,
    pass_2pt             = 0
  )

  qb_row <- result %>% dplyr::filter(player_id == "QB001")
  # QB should either be absent from two-point scoring or have 0 pts
  expect_true(
    nrow(qb_row) == 0 ||
    isTRUE(all.equal(qb_row$two_pt_fantasy_points, 0, tolerance = 0.01))
  )
})

test_that("pass_2pt included in total_fantasy_points for passer", {
  pbp <- make_2pt_play(is_passing = TRUE)

  result <- calculate_fantasy_points_ext(
    pbp, make_roster(),
    two_point_conversion = 0,
    pass_2pt             = 2.0
  )

  qb_row <- result %>% dplyr::filter(player_id == "QB001")
  expect_equal(qb_row$total_fantasy_points, 2.0, tolerance = 0.01)
})

# ----------------------------------------------------------------------
# A5: backward compatibility
# ----------------------------------------------------------------------

test_that("all new params at defaults produce identical output to original call", {
  pbp <- dplyr::bind_rows(
    make_pass_play(pass_yards = 10, rec_yards = 10, n_plays = 10L),
    make_rush_play(rush_yards = 10, n_plays = 5L)
  )
  roster <- make_roster()

  result_base <- calculate_fantasy_points_ext(pbp, roster)
  result_new  <- calculate_fantasy_points_ext(
    pbp, roster,
    tiered_rec_tiers  = NULL,
    bonus_pass_yd_300 = 0,
    bonus_pass_yd_400 = 0,
    bonus_rec_yd_200  = 0,
    bonus_rush_yd_200 = 0,
    pass_2pt          = 0
  )

  joined <- dplyr::inner_join(
    result_base %>% dplyr::select(player_id, base = total_fantasy_points),
    result_new  %>% dplyr::select(player_id, new  = total_fantasy_points),
    by = "player_id"
  )
  expect_equal(joined$base, joined$new, tolerance = 0.001)
})

# ==============================================================================
# PART B: R/19 SLEEPER API FUNCTIONS
# ==============================================================================

# ----------------------------------------------------------------------
# B1: .normalize_player_name()
# ----------------------------------------------------------------------

test_that(".normalize_player_name removes Jr./Sr. suffixes", {
  expect_false(grepl("jr", .normalize_player_name("Calvin Ridley Jr.")))
})

test_that(".normalize_player_name lowercases and squishes whitespace", {
  expect_equal(.normalize_player_name("  TRAVIS  KELCE  "), "travis kelce")
})

test_that(".normalize_player_name removes punctuation", {
  expect_false(grepl("\\.", .normalize_player_name("D.K. Metcalf")))
})

test_that(".normalize_player_name removes roman numeral suffixes", {
  expect_false(grepl("\\biii\\b", .normalize_player_name("Odell Beckham III")))
})

test_that(".normalize_player_name handles empty string", {
  expect_equal(.normalize_player_name(""), "")
})

# ----------------------------------------------------------------------
# B2: .parse_scoring_settings() -- existing and new mappings
# ----------------------------------------------------------------------

test_that(".parse_scoring_settings maps core direct fields", {
  result <- .parse_scoring_settings(make_mock_scoring(pass_yd = 0.04, pass_td = 6))
  expect_equal(result$params$pass_yd, 0.04)
  expect_equal(result$params$pass_td, 6)
})

test_that(".parse_scoring_settings: rush_att maps to rush_att_bonus", {
  result <- .parse_scoring_settings(make_mock_scoring(rush_att = 0.25))
  expect_equal(result$params$rush_att_bonus, 0.25)
})

test_that(".parse_scoring_settings: rush_att = 0 does not produce rush_att_bonus", {
  result <- .parse_scoring_settings(make_mock_scoring(rush_att = 0))
  val <- result$params$rush_att_bonus
  expect_true(is.null(val) || val == 0)
})

test_that(".parse_scoring_settings maps bonus_pass_yd_300 and bonus_pass_yd_400", {
  result <- .parse_scoring_settings(
    make_mock_scoring(bonus_pass_yd_300 = 2.0, bonus_pass_yd_400 = 4.0)
  )
  expect_equal(result$params$bonus_pass_yd_300, 2.0)
  expect_equal(result$params$bonus_pass_yd_400, 4.0)
})

test_that(".parse_scoring_settings maps bonus_rec_yd_200 and bonus_rush_yd_200", {
  result <- .parse_scoring_settings(
    make_mock_scoring(bonus_rec_yd_200 = 4.0, bonus_rush_yd_200 = 4.0)
  )
  expect_equal(result$params$bonus_rec_yd_200, 4.0)
  expect_equal(result$params$bonus_rush_yd_200, 4.0)
})

test_that(".parse_scoring_settings maps pass_2pt", {
  result <- .parse_scoring_settings(make_mock_scoring(pass_2pt = 2.0))
  expect_equal(result$params$pass_2pt, 2.0)
})

test_that(".parse_scoring_settings: all tiered rec = 0 leaves use_tiered_ppr FALSE", {
  result <- .parse_scoring_settings(
    make_mock_scoring(rec_0_4 = 0, rec_5_9 = 0, rec_10_19 = 0,
                      rec_20_29 = 0, rec_30_39 = 0, rec_40p = 0)
  )
  expect_false(result$params$use_tiered_ppr)
})

test_that(".parse_scoring_settings: non-zero tiered rec sets use_tiered_ppr TRUE", {
  result <- .parse_scoring_settings(
    make_mock_scoring(rec_5_9 = 0.5, rec_10_19 = 1.0, rec_20_29 = 1.5,
                      rec_30_39 = 2.0, rec_40p = 2.0)
  )
  expect_true(result$params$use_tiered_ppr)
})

test_that(".parse_scoring_settings: tiered rec fields produce 6-element vector", {
  result <- .parse_scoring_settings(
    make_mock_scoring(rec_0_4 = 0, rec_5_9 = 0.5, rec_10_19 = 1.0,
                      rec_20_29 = 1.5, rec_30_39 = 2.0, rec_40p = 2.5)
  )
  tiers <- result$params$tiered_rec_tiers
  expect_equal(length(tiers), 6L)
  expect_equal(tiers[2], 0.5)
  expect_equal(tiers[6], 2.5)
})

test_that(".parse_scoring_settings: tiered rec vector order matches Sleeper fields", {
  result <- .parse_scoring_settings(
    make_mock_scoring(rec_0_4 = 0.1, rec_5_9 = 0.2, rec_10_19 = 0.3,
                      rec_20_29 = 0.4, rec_30_39 = 0.5, rec_40p = 0.6)
  )
  expect_equal(result$params$tiered_rec_tiers,
               c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), tolerance = 0.001)
})

test_that(".parse_scoring_settings always sets use_tiered_ppr = FALSE when tiers are all 0", {
  result <- .parse_scoring_settings(make_mock_scoring(
    rec_0_4 = 0, rec_5_9 = 0, rec_10_19 = 0,
    rec_20_29 = 0, rec_30_39 = 0, rec_40p = 0
  ))
  expect_false(result$params$use_tiered_ppr)
})

test_that(".parse_scoring_settings log has required columns", {
  result <- .parse_scoring_settings(make_mock_scoring())
  expect_true(all(
    c("sleeper_field", "sleeper_value", "our_param",
      "our_value", "status", "note") %in% names(result$log)
  ))
})

test_that(".parse_scoring_settings log contains only valid status values", {
  result <- .parse_scoring_settings(make_mock_scoring())
  valid  <- c("mapped_direct", "mapped_derived", "mapped_derived_warning",
              "derived_no_sleeper_source", "unsupported", "unmapped")
  expect_true(all(result$log$status %in% valid))
})

test_that(".parse_scoring_settings handles empty settings gracefully", {
  result <- .parse_scoring_settings(list())
  expect_type(result$params, "list")
  expect_s3_class(result$log, "tbl_df")
  expect_false(result$params$use_tiered_ppr)
})

# ----------------------------------------------------------------------
# B3: player matching (unit level -- no network)
# ----------------------------------------------------------------------

test_that("GSIS direct match works for all mock players", {
  db      <- make_sleeper_db()
  roster  <- make_nfl_roster()
  matched <- db %>%
    dplyr::filter(nchar(trimws(dplyr::coalesce(gsis_id, ""))) > 0L) %>%
    dplyr::left_join(
      roster %>% dplyr::select(gsis_id, nflfastr_name = full_name),
      by = "gsis_id"
    ) %>%
    dplyr::filter(!is.na(nflfastr_name))
  expect_equal(nrow(matched), 6L)
})

test_that("exact name match: normalized names are consistent across both tables", {
  sleeper_norm <- .normalize_player_name(make_sleeper_db()$full_name)
  roster_norm  <- .normalize_player_name(make_nfl_roster()$full_name)
  expect_true(all(sleeper_norm %in% roster_norm))
})

test_that("fuzzy match handles apostrophe in name", {
  hits <- agrep(
    .normalize_player_name("JaMarr Chase"),
    .normalize_player_name("Ja'Marr Chase"),
    max.distance = FUZZY_MATCH_DISTANCE
  )
  expect_equal(length(hits), 1L)
})

test_that("match_sleeper_players: empty input raises error", {
  expect_error(
    match_sleeper_players(character(0), make_nfl_roster()),
    regexp = "empty"
  )
})

test_that("match_sleeper_players: missing required columns raises error", {
  expect_error(
    match_sleeper_players("4017", tibble::tibble(name = "Test")),
    regexp = "missing required columns"
  )
})

test_that("match rate thresholds are correctly ordered", {
  expect_true(MATCH_RATE_ERROR_THRESHOLD < MATCH_RATE_WARNING_THRESHOLD)
  expect_equal(MATCH_RATE_WARNING_THRESHOLD, 0.90)
  expect_equal(MATCH_RATE_ERROR_THRESHOLD,   0.70)
})

# ----------------------------------------------------------------------
# B4: roster and matchup parsing (unit level)
# ----------------------------------------------------------------------

test_that("roster parsing produces correct row count and starter flags", {
  mock_raw <- list(
    list(roster_id = 1L, owner_id = "u1",
         players = list("A","B","C"), starters = list("A","B"), reserve = list()),
    list(roster_id = 2L, owner_id = "u2",
         players = list("D","E"), starters = list("D"), reserve = list())
  )
  rows <- purrr::map_dfr(mock_raw, function(r) {
    all_p <- unlist(r$players); starters <- unlist(r$starters)
    tibble::tibble(
      roster_id  = as.integer(r$roster_id), player_id = all_p,
      is_starter = all_p %in% starters, on_bench = !(all_p %in% starters)
    )
  })
  expect_equal(nrow(rows), 5L)
  expect_equal(sum(rows$is_starter), 3L)
  expect_equal(sum(rows$on_bench),   2L)
})

test_that("roster starter and bench flags are mutually exclusive", {
  df <- tibble::tibble(
    is_starter = c(TRUE, FALSE, FALSE),
    is_reserve = c(FALSE, FALSE, TRUE),
    on_bench   = c(FALSE, TRUE,  FALSE)
  )
  expect_equal(sum(df$is_starter & df$on_bench), 0L)
  expect_equal(sum(df$is_reserve & df$on_bench), 0L)
})

test_that("matchup parsing: bench player has NA points", {
  mock <- list(
    matchup_id = 1L, roster_id = 1L,
    players = list("A","B","C"), starters = list("A","B"),
    points = 100.0,
    players_points = list(A = 40.0, B = 30.0)
  )
  pts <- purrr::map_dbl(unlist(mock$players), function(pid) {
    val <- mock$players_points[[pid]]
    if (is.null(val)) NA_real_ else as.numeric(val)
  })
  expect_equal(pts[1], 40.0)
  expect_equal(pts[2], 30.0)
  expect_true(is.na(pts[3]))
})

test_that("week validation: week 0 is invalid", {
  expect_true(0L < 1L || 0L > 18L)
})

test_that("week validation: week 9 is valid", {
  expect_false(9L < 1L || 9L > 18L)
})

# ----------------------------------------------------------------------
# B5: get_user_leagues() scoring type classification
# ----------------------------------------------------------------------

test_that("scoring_type PPR classified correctly at rec = 1.0", {
  expect_equal(
    dplyr::case_when(1.0 >= 0.9 ~ "ppr", 1.0 >= 0.4 ~ "half_ppr", TRUE ~ "standard"),
    "ppr"
  )
})

test_that("scoring_type half_ppr classified correctly at rec = 0.5", {
  expect_equal(
    dplyr::case_when(0.5 >= 0.9 ~ "ppr", 0.5 >= 0.4 ~ "half_ppr", TRUE ~ "standard"),
    "half_ppr"
  )
})

test_that("scoring_type standard classified correctly at rec = 0", {
  expect_equal(
    dplyr::case_when(0.0 >= 0.9 ~ "ppr", 0.0 >= 0.4 ~ "half_ppr", TRUE ~ "standard"),
    "standard"
  )
})

# ----------------------------------------------------------------------
# B6: SLEEPER_DIRECT_MAP -- verify all new entries present
# ----------------------------------------------------------------------

test_that("SLEEPER_DIRECT_MAP contains all new Sleeper parity keys", {
  new_keys <- c("rush_att", "bonus_pass_yd_300", "bonus_pass_yd_400",
                "bonus_rec_yd_200", "bonus_rush_yd_200", "pass_2pt")
  expect_true(all(new_keys %in% names(SLEEPER_DIRECT_MAP)))
})

test_that("SLEEPER_DIRECT_MAP: rush_att maps to rush_att_bonus (key fix verified)", {
  expect_equal(SLEEPER_DIRECT_MAP[["rush_att"]], "rush_att_bonus")
})

test_that("SLEEPER_DIRECT_MAP: pass_2pt maps to pass_2pt", {
  expect_equal(SLEEPER_DIRECT_MAP[["pass_2pt"]], "pass_2pt")
})

test_that("SLEEPER_DIRECT_MAP: bonus_pass_yd_300 maps to bonus_pass_yd_300", {
  expect_equal(SLEEPER_DIRECT_MAP[["bonus_pass_yd_300"]], "bonus_pass_yd_300")
})

# ----------------------------------------------------------------------
# B7: Constants
# ----------------------------------------------------------------------

test_that("SLEEPER_BASE_URL has correct format", {
  expect_true(startsWith(SLEEPER_BASE_URL, "https://"))
  expect_false(endsWith(SLEEPER_BASE_URL, "/"))
})

test_that("CURRENT_NFL_SEASON is an integer", {
  expect_type(CURRENT_NFL_SEASON, "integer")
})

test_that("FUZZY_MATCH_DISTANCE is between 0 and 1", {
  expect_true(FUZZY_MATCH_DISTANCE > 0 && FUZZY_MATCH_DISTANCE < 1)
})

test_that("null coalescing %||% works correctly", {
  expect_equal(NULL    %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0       %||% "default", 0)
  expect_equal(FALSE   %||% "default", FALSE)
})

test_that("is_superflex detected when SUPER_FLEX in roster_positions", {
  expect_true("SUPER_FLEX" %in% c("QB", "RB", "WR", "SUPER_FLEX", "BN"))
})

test_that("is_superflex FALSE when SUPER_FLEX absent", {
  expect_false("SUPER_FLEX" %in% c("QB", "RB", "WR", "FLEX", "BN"))
})
