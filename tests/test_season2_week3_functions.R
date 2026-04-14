# ==============================================================================
# SEASON 2 WEEK 3: EXTENDED FANTASY SCORING -- TEST SUITE
# File: tests/test_season2_week3_functions.R
# ==============================================================================
#
# Tests for R/17_extended_scoring.R
# Functions covered:
#   - calculate_fantasy_points_ext()
#   - validate_ext_scoring_params()
#   - compare_scoring_systems()
#   - get_ext_scoring_defaults()
#
# Test categories:
#   1.  Output schema -- all required columns present, correct types
#   2.  Backward compatibility -- default call matches Season 1 behavior
#   3.  New component columns -- all zero at defaults
#   4.  First down bonus -- correct attribution to rusher/receiver not passer
#   5.  Long TD bonus -- fires only above threshold, correct attribution
#   6.  Hundred-yard bonus -- fires at 100+, not below
#   7.  Two-point conversion -- scorer earns points, failed attempt earns zero
#   8.  Sack penalty -- applied to QB, zero for non-QB
#   9.  Superflex parameter -- overrides pass_td when non-zero
#   10. Input validation -- bad pbp_data, missing columns, bad season filter
#   11. validate_ext_scoring_params() -- errors, warnings, valid config
#   12. get_ext_scoring_defaults() -- correct structure and values
#   13. Edge cases -- empty filtered data, no two-point attempts in data
#   14. compare_scoring_systems() -- multiple systems, rank shift direction
#
# ==============================================================================

library(testthat)
library(dplyr)
library(here)

source(here::here("R", "17_extended_scoring.R"))

# ==============================================================================
# TEST FIXTURE: Minimal synthetic play-by-play data
# ==============================================================================
# Building synthetic data avoids dependency on live nflfastR downloads
# and makes tests deterministic. Column names verified against CORE_COLUMNS
# in R/15_multi_season_pbp.R and the nflfastR data dictionary.

make_pbp <- function() {
  tibble::tibble(
    # Identifiers
    season      = 2024L,
    week        = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
    game_id     = "2024_01_BAL_KC",
    season_type = "REG",
    play_id     = 1:10L,
    posteam     = "BAL",
    defteam     = "KC",

    # Play type
    play_type   = c("pass", "pass", "run", "run", "pass", "pass",
                    "run", "no_play", "no_play", "run"),

    # Situation
    down        = c(1L, 2L, 1L, 2L, 3L, 1L, 1L, NA_integer_, NA_integer_, 1L),
    ydstogo     = c(10L, 7L, 10L, 3L, 5L, 10L, 10L, NA_integer_, NA_integer_, 10L),

    # EPA / success
    epa         = c(0.5, -0.2, 0.3, 1.2, 2.1, 0.8, 0.1, NA_real_, NA_real_, 0.6),
    success     = c(1L, 0L, 1L, 1L, 1L, 1L, 1L, NA_integer_, NA_integer_, 1L),
    wp          = rep(0.55, 10),

    # Passer columns (rows 1,2,5,6 are pass plays)
    passer_player_id   = c("QB1", "QB1", NA, NA, "QB1", "QB1",
                            NA, NA, NA, NA),
    passer_player_name = c("T.Brady", "T.Brady", NA, NA, "T.Brady", "T.Brady",
                            NA, NA, NA, NA),
    passing_yards      = c(12, 5, NA, NA, 45, 8, NA, NA, NA, NA),
    pass_attempt       = c(1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L),
    complete_pass      = c(1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L),
    pass_touchdown     = c(0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L),
    interception       = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    qb_spike           = rep(0L, 10),
    qb_kneel           = rep(0L, 10),
    sack               = rep(0L, 10),

    # Rusher columns (rows 3,4,7,10 are run plays)
    rusher_player_id   = c(NA, NA, "RB1", "RB1", NA, NA, "RB1", NA, NA, "QB1"),
    rusher_player_name = c(NA, NA, "D.Henry", "D.Henry", NA, NA, "D.Henry",
                            NA, NA, "T.Brady"),
    rushing_yards      = c(NA, NA, 8, 25, NA, NA, 4, NA, NA, 6),
    rush_attempt       = c(0L, 0L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 1L),
    rush_touchdown     = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    first_down_rush    = c(0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L),

    # Receiver columns (rows 1,2,5,6 are complete passes)
    receiver_player_id   = c("WR1", "WR1", NA, NA, "WR1", "TE1",
                              NA, NA, NA, NA),
    receiver_player_name = c("J.Chase", "J.Chase", NA, NA, "J.Chase", "T.Kelce",
                              NA, NA, NA, NA),
    receiving_yards      = c(12, 5, NA, NA, 45, 8, NA, NA, NA, NA),
    first_down_pass      = c(1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L),

    # Fumbles
    fumble             = rep(0L, 10),
    fumble_lost        = rep(0L, 10),
    fumbled_1_player_id = rep(NA_character_, 10),

    # Two-point columns (rows 8,9 are two-point attempts)
    two_point_attempt      = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L),
    two_point_conv_result  = c(NA, NA, NA, NA, NA, NA, NA,
                                "success", "failure", NA),

    # Extra columns required by the filter logic
    yards_gained       = c(12L, 5L, 8L, 25L, 45L, 8L, 4L, 0L, 0L, 6L),
    touchdown          = c(0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L)
  )
}

# Row 8: QB1 rushes 2PC successfully (receiver_player_id drives who scored)
# Row 9: WR1 receives 2PC that fails

# Minimal roster for position anchoring
make_roster <- function() {
  tibble::tibble(
    gsis_id  = c("QB1", "RB1", "WR1", "TE1"),
    position = c("QB",  "RB",  "WR",  "TE"),
    full_name = c("T.Brady", "D.Henry", "J.Chase", "T.Kelce")
  )
}

pbp     <- make_pbp()
roster  <- make_roster()

# ==============================================================================
# CATEGORY 1: Output schema
# ==============================================================================

test_that("output has all required columns", {
  result <- calculate_fantasy_points_ext(pbp_data = pbp)

  required_cols <- c(
    "season", "week", "game_id", "player_id", "player_name",
    "position", "team",
    "pass_fantasy_points", "rush_fantasy_points", "rec_fantasy_points",
    "two_pt_fantasy_points", "first_down_fantasy_points",
    "long_td_fantasy_points", "hundred_yard_fantasy_points",
    "total_fantasy_points"
  )
  missing <- setdiff(required_cols, names(result))
  expect_equal(missing, character(0),
    info = paste("Missing columns:", paste(missing, collapse = ", ")))
})

test_that("output has correct column types", {
  result <- calculate_fantasy_points_ext(pbp_data = pbp)

  expect_type(result$season,                  "integer")
  expect_type(result$week,                    "integer")
  expect_type(result$game_id,                 "character")
  expect_type(result$player_id,               "character")
  expect_type(result$total_fantasy_points,    "double")
  expect_type(result$pass_fantasy_points,     "double")
  expect_type(result$first_down_fantasy_points, "double")
})

test_that("output has one row per player per game", {
  result <- calculate_fantasy_points_ext(pbp_data = pbp)

  dups <- result %>%
    dplyr::count(season, week, game_id, player_id) %>%
    dplyr::filter(n > 1)
  expect_equal(nrow(dups), 0L,
    info = "Duplicate player-game rows found")
})

test_that("total_fantasy_points equals sum of components", {
  result <- calculate_fantasy_points_ext(
    pbp_data          = pbp,
    first_down_points = 0.5,
    hundred_yard_bonus = 3.0,
    long_td_bonus      = 2.0,
    two_point_conversion = 2.0
  )

  computed_total <- result %>%
    dplyr::mutate(
      check_total = pass_fantasy_points + rush_fantasy_points +
                    rec_fantasy_points + two_pt_fantasy_points +
                    first_down_fantasy_points + long_td_fantasy_points +
                    hundred_yard_fantasy_points
    ) %>%
    dplyr::summarise(max_diff = max(abs(total_fantasy_points - check_total),
                                    na.rm = TRUE)) %>%
    dplyr::pull(max_diff)

  expect_lt(computed_total, 0.001,
    label = "total_fantasy_points does not equal sum of components")
})

# ==============================================================================
# CATEGORY 2: Backward compatibility
# ==============================================================================

test_that("default call produces non-zero total points for known players", {
  result <- calculate_fantasy_points_ext(pbp_data = pbp)

  # QB1 threw passes -- should have positive pass_fantasy_points
  qb_pts <- result %>%
    dplyr::filter(player_id == "QB1") %>%
    dplyr::pull(pass_fantasy_points)
  expect_true(length(qb_pts) > 0)
  expect_gt(sum(qb_pts, na.rm = TRUE), 0)
})

test_that("season filter restricts output to requested season", {
  result <- calculate_fantasy_points_ext(pbp_data = pbp, season = 2024L)
  expect_true(all(result$season == 2024L))
})

test_that("week_max filter restricts output correctly", {
  result_all  <- calculate_fantasy_points_ext(pbp_data = pbp)
  result_w1   <- calculate_fantasy_points_ext(pbp_data = pbp, week_max = 1L)
  expect_lte(nrow(result_w1), nrow(result_all))
  expect_true(all(result_w1$week <= 1L))
})

test_that("invalid season type raises error", {
  expect_error(
    calculate_fantasy_points_ext(pbp_data = pbp, season = "twenty-twenty-four"),
    regexp = "numeric"
  )
})

test_that("non-data-frame pbp_data raises error", {
  expect_error(
    calculate_fantasy_points_ext(pbp_data = list(a = 1)),
    regexp = "data frame"
  )
})

test_that("missing required column raises informative error", {
  pbp_bad <- pbp %>% dplyr::select(-passing_yards)
  expect_error(
    calculate_fantasy_points_ext(pbp_data = pbp_bad),
    regexp = "passing_yards"
  )
})

# ==============================================================================
# CATEGORY 3: New component columns all zero at defaults
# ==============================================================================

test_that("new component columns are all zero at default parameters", {
  result <- calculate_fantasy_points_ext(pbp_data = pbp)

  new_cols <- c(
    "two_pt_fantasy_points", "first_down_fantasy_points",
    "long_td_fantasy_points", "hundred_yard_fantasy_points"
  )
  col_sums <- result %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(new_cols), sum)) %>%
    unlist()

  expect_true(all(col_sums == 0),
    info = paste("Non-zero new component at defaults:",
                 paste(names(col_sums[col_sums != 0]), collapse = ", ")))
})

# ==============================================================================
# CATEGORY 4: First down bonus attribution
# ==============================================================================

test_that("first_down_points awards points to rusher on rushing first down", {
  result <- calculate_fantasy_points_ext(
    pbp_data          = pbp,
    first_down_points = 1.0
  )

  # Row 4: RB1 gains rushing first down (first_down_rush == 1)
  rb_fd <- result %>%
    dplyr::filter(player_id == "RB1") %>%
    dplyr::pull(first_down_fantasy_points)
  expect_gt(sum(rb_fd, na.rm = TRUE), 0,
    label = "RB should have first_down_fantasy_points > 0")
})

test_that("first_down_points awards points to receiver on receiving first down", {
  result <- calculate_fantasy_points_ext(
    pbp_data          = pbp,
    first_down_points = 1.0
  )

  # Row 1: WR1 catches a pass and gains a first down (first_down_pass == 1)
  wr_fd <- result %>%
    dplyr::filter(player_id == "WR1") %>%
    dplyr::pull(first_down_fantasy_points)
  expect_gt(sum(wr_fd, na.rm = TRUE), 0,
    label = "WR should have first_down_fantasy_points > 0")
})

test_that("passer earns zero first_down_fantasy_points", {
  result <- calculate_fantasy_points_ext(
    pbp_data          = pbp,
    first_down_points = 1.0
  )

  qb_fd <- result %>%
    dplyr::filter(player_id == "QB1") %>%
    dplyr::pull(first_down_fantasy_points)
  expect_equal(sum(qb_fd, na.rm = TRUE), 0,
    label = "QB pass_fantasy_points should not include first_down bonus")
})

test_that("first_down_fantasy_points is zero when first_down_points = 0", {
  result <- calculate_fantasy_points_ext(
    pbp_data          = pbp,
    first_down_points = 0
  )
  expect_equal(sum(result$first_down_fantasy_points, na.rm = TRUE), 0)
})

# ==============================================================================
# CATEGORY 5: Long TD bonus
# ==============================================================================

test_that("long_td_bonus fires for pass TD exceeding threshold", {
  # Row 5: WR1 catches a 45-yard TD (pass_touchdown == 1, receiving_yards == 45)
  # threshold = 40 -- should fire
  result <- calculate_fantasy_points_ext(
    pbp_data          = pbp,
    long_td_bonus     = 5.0,
    long_td_threshold = 40L
  )

  wr_long <- result %>%
    dplyr::filter(player_id == "WR1") %>%
    dplyr::pull(long_td_fantasy_points)
  expect_gt(sum(wr_long, na.rm = TRUE), 0,
    label = "WR1 45-yard TD should trigger long_td_bonus at threshold=40")
})

test_that("long_td_bonus does not fire below threshold", {
  # threshold = 50 -- 45-yard TD should NOT fire
  result <- calculate_fantasy_points_ext(
    pbp_data          = pbp,
    long_td_bonus     = 5.0,
    long_td_threshold = 50L
  )

  total_long <- sum(result$long_td_fantasy_points, na.rm = TRUE)
  expect_equal(total_long, 0,
    label = "No TDs exceed 50 yards in test data -- bonus should be zero")
})

test_that("passer earns zero long_td_fantasy_points", {
  result <- calculate_fantasy_points_ext(
    pbp_data          = pbp,
    long_td_bonus     = 5.0,
    long_td_threshold = 40L
  )

  qb_long <- result %>%
    dplyr::filter(player_id == "QB1") %>%
    dplyr::pull(long_td_fantasy_points)
  expect_equal(sum(qb_long, na.rm = TRUE), 0,
    label = "QB should not earn long_td_fantasy_points")
})

test_that("long_td_fantasy_points is zero when long_td_bonus = 0", {
  result <- calculate_fantasy_points_ext(pbp_data = pbp, long_td_bonus = 0)
  expect_equal(sum(result$long_td_fantasy_points, na.rm = TRUE), 0)
})

# ==============================================================================
# CATEGORY 6: Hundred-yard bonus
# ==============================================================================

test_that("hundred_yard_bonus fires for player with 100+ rush yards in a game", {
  # Build pbp where RB1 gains 100+ yards in a single game
  pbp_100 <- pbp %>%
    dplyr::mutate(
      rushing_yards = dplyr::if_else(
        !is.na(rusher_player_id) & rusher_player_id == "RB1",
        60,   # each of the 2 RB1 rush plays gets 60 yards = 120 total
        rushing_yards
      )
    )

  result <- calculate_fantasy_points_ext(
    pbp_data           = pbp_100,
    hundred_yard_bonus = 3.0
  )

  rb_bonus <- result %>%
    dplyr::filter(player_id == "RB1") %>%
    dplyr::pull(hundred_yard_fantasy_points)
  expect_equal(sum(rb_bonus, na.rm = TRUE), 3.0,
    label = "RB1 should earn exactly one hundred_yard_bonus")
})

test_that("hundred_yard_bonus does not fire below 100 yards", {
  # Default pbp: RB1 has 8 + 4 = 12 rush yards -- well below 100
  result <- calculate_fantasy_points_ext(
    pbp_data           = pbp,
    hundred_yard_bonus = 3.0
  )

  rb_bonus <- result %>%
    dplyr::filter(player_id == "RB1") %>%
    dplyr::pull(hundred_yard_fantasy_points)
  expect_equal(sum(rb_bonus, na.rm = TRUE), 0,
    label = "RB1 has <100 yards -- no bonus should fire")
})

test_that("hundred_yard_fantasy_points is zero when hundred_yard_bonus = 0", {
  result <- calculate_fantasy_points_ext(pbp_data = pbp, hundred_yard_bonus = 0)
  expect_equal(sum(result$hundred_yard_fantasy_points, na.rm = TRUE), 0)
})

# ==============================================================================
# CATEGORY 7: Two-point conversion
# ==============================================================================

test_that("two_pt_fantasy_points awarded on successful 2PC", {
  # Row 8: two_point_conv_result == "success", receiver_player_id is NA
  # (rush 2PC), rusher_player_id would drive attribution -- but in our
  # synthetic data row 8 has no rusher either; test with a row that has one
  pbp_2pc <- pbp
  pbp_2pc$rusher_player_id[8]   <- "RB1"
  pbp_2pc$rusher_player_name[8] <- "D.Henry"

  result <- calculate_fantasy_points_ext(
    pbp_data             = pbp_2pc,
    two_point_conversion = 2.0
  )

  rb_2pc <- result %>%
    dplyr::filter(player_id == "RB1") %>%
    dplyr::pull(two_pt_fantasy_points)
  expect_gt(sum(rb_2pc, na.rm = TRUE), 0,
    label = "RB1 should earn two_pt_fantasy_points on successful 2PC")
})

test_that("failed 2PC earns zero two_pt_fantasy_points", {
  # Row 9: two_point_conv_result == "failure"
  pbp_fail <- pbp
  pbp_fail$receiver_player_id[9]   <- "WR1"
  pbp_fail$receiver_player_name[9] <- "J.Chase"

  result <- calculate_fantasy_points_ext(
    pbp_data             = pbp_fail,
    two_point_conversion = 2.0
  )

  # WR1 only appears in row 9 (failed 2PC) -- two_pt_fantasy_points should be 0
  wr_2pc <- result %>%
    dplyr::filter(player_id == "WR1") %>%
    dplyr::pull(two_pt_fantasy_points)
  expect_equal(sum(wr_2pc, na.rm = TRUE), 0,
    label = "Failed 2PC should not award points")
})

test_that("two_pt_fantasy_points is zero when two_point_conversion = 0", {
  result <- calculate_fantasy_points_ext(pbp_data = pbp, two_point_conversion = 0)
  expect_equal(sum(result$two_pt_fantasy_points, na.rm = TRUE), 0)
})

# ==============================================================================
# CATEGORY 8: Sack penalty
# ==============================================================================

test_that("sack_penalty reduces QB pass_fantasy_points", {
  # Build pbp with a sack play attributed to QB1
  pbp_sack <- pbp
  pbp_sack$sack[2]            <- 1L
  pbp_sack$rush_attempt[2]    <- 0L  # sacks coded as rush in nflfastR but
  pbp_sack$pass_attempt[2]    <- 1L  # keep pass_attempt=1 for our filter

  result_no_penalty <- calculate_fantasy_points_ext(
    pbp_data     = pbp_sack,
    sack_penalty = 0
  )
  result_with_penalty <- calculate_fantasy_points_ext(
    pbp_data     = pbp_sack,
    sack_penalty = -1.0
  )

  qb_no  <- result_no_penalty   %>% dplyr::filter(player_id == "QB1") %>%
    dplyr::pull(pass_fantasy_points) %>% sum(na.rm = TRUE)
  qb_pen <- result_with_penalty %>% dplyr::filter(player_id == "QB1") %>%
    dplyr::pull(pass_fantasy_points) %>% sum(na.rm = TRUE)

  expect_lt(qb_pen, qb_no,
    label = "QB pass_fantasy_points should be lower with sack_penalty = -1.0")
})

test_that("sack_penalty does not affect non-QB players", {
  pbp_sack <- pbp
  pbp_sack$sack[2] <- 1L

  result_no  <- calculate_fantasy_points_ext(pbp_data = pbp_sack, sack_penalty = 0)
  result_yes <- calculate_fantasy_points_ext(pbp_data = pbp_sack, sack_penalty = -1.0)

  rb_no  <- result_no  %>% dplyr::filter(player_id == "RB1") %>%
    dplyr::pull(total_fantasy_points) %>% sum(na.rm = TRUE)
  rb_yes <- result_yes %>% dplyr::filter(player_id == "RB1") %>%
    dplyr::pull(total_fantasy_points) %>% sum(na.rm = TRUE)

  expect_equal(rb_no, rb_yes,
    label = "RB total_fantasy_points should be unaffected by sack_penalty")
})

# ==============================================================================
# CATEGORY 9: Superflex parameter
# ==============================================================================

test_that("superflex_pass_td overrides pass_td when non-zero", {
  result_4pt <- calculate_fantasy_points_ext(
    pbp_data          = pbp,
    superflex_pass_td = 4.0
  )
  result_6pt <- calculate_fantasy_points_ext(
    pbp_data          = pbp,
    superflex_pass_td = 6.0
  )

  qb_4 <- result_4pt %>% dplyr::filter(player_id == "QB1") %>%
    dplyr::pull(pass_fantasy_points) %>% sum(na.rm = TRUE)
  qb_6 <- result_6pt %>% dplyr::filter(player_id == "QB1") %>%
    dplyr::pull(pass_fantasy_points) %>% sum(na.rm = TRUE)

  # QB1 has one passing TD in test data -- 6pt system should score higher
  expect_gt(qb_6, qb_4,
    label = "6pt superflex_pass_td should produce more QB points than 4pt")
  expect_equal(qb_6 - qb_4, 2.0,   # one TD * (6 - 4) = 2 pts
    tolerance = 0.001,
    label = "gap should be exactly 2 pts per passing TD")
})

test_that("superflex_pass_td = 0 leaves pass_td unchanged", {
  result_default <- calculate_fantasy_points_ext(pbp_data = pbp)
  result_zero_sf <- calculate_fantasy_points_ext(pbp_data = pbp, superflex_pass_td = 0)

  qb_default <- result_default %>% dplyr::filter(player_id == "QB1") %>%
    dplyr::pull(pass_fantasy_points) %>% sum(na.rm = TRUE)
  qb_zero_sf <- result_zero_sf %>% dplyr::filter(player_id == "QB1") %>%
    dplyr::pull(pass_fantasy_points) %>% sum(na.rm = TRUE)

  expect_equal(qb_default, qb_zero_sf, tolerance = 0.001)
})

# ==============================================================================
# CATEGORY 10: Input validation
# ==============================================================================

test_that("empty pbp after season filter returns empty tibble with correct columns", {
  result <- calculate_fantasy_points_ext(pbp_data = pbp, season = 1999L)
  expect_equal(nrow(result), 0L)
  expect_true("total_fantasy_points" %in% names(result))
})

test_that("week_min > week_max produces empty result", {
  result <- calculate_fantasy_points_ext(
    pbp_data = pbp, week_min = 10L, week_max = 5L
  )
  expect_equal(nrow(result), 0L)
})

# ==============================================================================
# CATEGORY 11: validate_ext_scoring_params()
# ==============================================================================

test_that("valid config returns valid = TRUE with no errors", {
  check <- validate_ext_scoring_params(
    pass_yd = 0.04, pass_td = 6, pass_int = -2, pick6_penalty = -4,
    rush_yd = 0.1,  rush_td = 6,
    rec_yd  = 0.1,  rec_td  = 6, ppr = 1, fumbles = -2,
    rush_att_bonus = 0.25, first_down_points = 0.5,
    long_td_bonus = 2.0, long_td_threshold = 40L,
    hundred_yard_bonus = 3.0, superflex_pass_td = 0,
    two_point_conversion = 2.0, sack_penalty = -1.0
  )
  expect_true(check$valid)
  expect_equal(length(check$errors), 0L)
})

test_that("non-numeric parameter returns valid = FALSE with error message", {
  check <- validate_ext_scoring_params(pass_yd = "not a number")
  expect_false(check$valid)
  expect_true(length(check$errors) > 0)
  expect_true(grepl("pass_yd", check$errors[1]))
})

test_that("positive pass_int triggers advisory warning", {
  check <- validate_ext_scoring_params(pass_int = 1.0)
  expect_true(check$valid)   # valid but unusual
  expect_true(length(check$warnings) > 0)
  expect_true(any(grepl("pass_int", check$warnings)))
})

test_that("positive sack_penalty triggers advisory warning", {
  check <- validate_ext_scoring_params(sack_penalty = 2.0)
  expect_true(check$valid)
  expect_true(any(grepl("sack_penalty", check$warnings)))
})

test_that("low long_td_threshold triggers advisory warning", {
  check <- validate_ext_scoring_params(long_td_threshold = 5L)
  expect_true(check$valid)
  expect_true(any(grepl("long_td_threshold", check$warnings)))
})

test_that("superflex_pass_td equal to pass_td triggers advisory warning", {
  check <- validate_ext_scoring_params(pass_td = 6, superflex_pass_td = 6)
  expect_true(check$valid)
  expect_true(any(grepl("superflex_pass_td", check$warnings)))
})

test_that("NA parameter returns valid = FALSE", {
  check <- validate_ext_scoring_params(pass_yd = NA_real_)
  expect_false(check$valid)
})

# ==============================================================================
# CATEGORY 12: get_ext_scoring_defaults()
# ==============================================================================

test_that("get_ext_scoring_defaults returns a named list", {
  defaults <- get_ext_scoring_defaults()
  expect_type(defaults, "list")
  expect_true(length(names(defaults)) > 0)
})

test_that("get_ext_scoring_defaults includes all Season 1 params", {
  defaults <- get_ext_scoring_defaults(show_new_only = FALSE)
  s1_params <- c("pass_yd", "pass_td", "pass_int", "pick6_penalty",
                 "rush_yd", "rush_td", "rec_yd", "rec_td",
                 "ppr", "fumbles", "use_tiered_ppr", "te_premium",
                 "rush_att_bonus")
  expect_true(all(s1_params %in% names(defaults)))
})

test_that("get_ext_scoring_defaults show_new_only = TRUE returns only Season 2 params", {
  new_only <- get_ext_scoring_defaults(show_new_only = TRUE)
  s1_params <- c("pass_yd", "pass_td", "pass_int")
  expect_false(any(s1_params %in% names(new_only)))

  s2_params <- c("first_down_points", "long_td_bonus", "hundred_yard_bonus",
                 "superflex_pass_td", "two_point_conversion", "sack_penalty")
  expect_true(all(s2_params %in% names(new_only)))
})

test_that("Season 2 new params default to 0", {
  new_only <- get_ext_scoring_defaults(show_new_only = TRUE)
  numeric_defaults <- unlist(new_only[sapply(new_only, is.numeric)])
  non_threshold    <- numeric_defaults[names(numeric_defaults) != "long_td_threshold"]
  expect_true(all(non_threshold == 0),
    info = "All new numeric params except long_td_threshold should default to 0")
})

# ==============================================================================
# CATEGORY 13: Edge cases
# ==============================================================================

test_that("pbp with no passing plays returns zero pass_fantasy_points", {
  pbp_no_pass <- pbp %>%
    dplyr::filter(play_type != "pass") %>%
    dplyr::mutate(passer_player_id = NA_character_)

  result <- calculate_fantasy_points_ext(pbp_data = pbp_no_pass)
  expect_equal(sum(result$pass_fantasy_points, na.rm = TRUE), 0)
})

test_that("pbp with no two-point attempts produces all-zero two_pt_fantasy_points", {
  pbp_no_2pc <- pbp %>%
    dplyr::mutate(two_point_attempt = 0L)

  result <- calculate_fantasy_points_ext(
    pbp_data             = pbp_no_2pc,
    two_point_conversion = 2.0
  )
  expect_equal(sum(result$two_pt_fantasy_points, na.rm = TRUE), 0)
})

test_that("position column is populated for all rows", {
  result <- calculate_fantasy_points_ext(pbp_data = pbp, roster_data = roster)
  expect_false(any(is.na(result$position)))
})

# ==============================================================================
# CATEGORY 14: compare_scoring_systems()
# ==============================================================================

test_that("compare_scoring_systems returns one row per player with all system columns", {
  result <- compare_scoring_systems(
    pbp_data = pbp,
    systems  = list(
      Base   = list(),
      Sleeper = list(first_down_points = 0.5)
    ),
    min_games = 1L
  )

  expect_true("Base_total"    %in% names(result))
  expect_true("Sleeper_total" %in% names(result))
  expect_true("Base_ppg"      %in% names(result))
  expect_true("Sleeper_ppg"   %in% names(result))
})

test_that("Sleeper system scores higher than Base for players with first downs", {
  result <- compare_scoring_systems(
    pbp_data = pbp,
    systems  = list(
      Base    = list(first_down_points = 0),
      Sleeper = list(first_down_points = 1.0)
    ),
    min_games = 1L
  )

  # At least one player should have more points in Sleeper
  has_gain <- result %>%
    dplyr::filter(!is.na(Base_total), !is.na(Sleeper_total)) %>%
    dplyr::summarise(any_gain = any(Sleeper_total > Base_total)) %>%
    dplyr::pull(any_gain)

  expect_true(has_gain,
    label = "Some players should score more in Sleeper with first_down_points=1")
})

test_that("compare_scoring_systems errors on unnamed systems list", {
  expect_error(
    compare_scoring_systems(
      pbp_data = pbp,
      systems  = list(list(), list())  # no names
    ),
    regexp = "names"
  )
})

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("Season 2 Week 3 Test Suite Complete\n")
cat("==============================================================================\n")
cat(glue::glue("Functions tested: calculate_fantasy_points_ext, validate_ext_scoring_params,\n"))
cat(glue::glue("                  compare_scoring_systems, get_ext_scoring_defaults\n"))
cat("Test categories: output schema, backward compat, new components,\n")
cat("                 first down, long TD, 100-yd bonus, 2PC, sack penalty,\n")
cat("                 superflex, input validation, validator, defaults, edge cases,\n")
cat("                 compare_scoring_systems\n")
cat("==============================================================================\n\n")
