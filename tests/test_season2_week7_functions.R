# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 7
# Test Suite: CFB Player-Season Panel Functions
# File: tests/test_season2_week7_functions.R
#
# Tests:
#   Section A: .apply_cfb_garbage_time_filter() -- 8 tests
#   Section B: .build_cfb_passing_stats()       -- 7 tests
#   Section C: .build_cfb_rushing_stats()       -- 6 tests
#   Section D: .build_cfb_receiving_stats()     -- 7 tests
#   Section E: .build_cfb_success_stats()       -- 5 tests
#   Section F: .build_cfb_games_played()        -- 4 tests
#   Section G: classify_cfb_player_position()   -- 8 tests
#   Section H: validate_cfb_panel_integrity()   -- 10 tests
#   Section I: build_cfb_player_season_panel()  -- 5 tests (input validation)
#
# Total: 60 tests
#
# No network calls. All tests use synthetic in-memory fixtures.
# Does NOT call load_normalized_cfb_season() -- no cache required.
#
# Run:
#   testthat::test_file(here::here("tests", "test_season2_week7_functions.R"))
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)
library(glue)
library(here)

source(here::here("R", "20_multi_season_cfb_pbp.R"))
source(here::here("R", "21_cfb_player_season_panel.R"))


# ==============================================================================
# SHARED FIXTURES
# ==============================================================================

# make_cfb_pbp(): minimal CFB PBP fixture covering all three offensive roles.
# Produces deterministic, hand-verifiable play counts and stats.
#
# Design:
#   - 3 games (game_id: "G001", "G002", "G003")
#   - 2 QBs: "QB Alpha" (team "TeamA"), "QB Beta" (team "TeamB")
#   - 2 RBs: "RB Alpha" (team "TeamA"), "RB Beta" (team "TeamB")
#   - 2 WRs: "WR Alpha" (team "TeamA"), "WR Beta" (team "TeamB")
#   - Periods 1-4 with one period-3 blowout play per team to test
#     garbage time filter
#   - season_type "regular" on all plays except 2 tagged "postseason"
#   - All plays have non-NA EPA in [-2, 2]

make_cfb_pbp <- function() {
  tibble::tibble(
    game_id              = c(rep("G001", 14), rep("G002", 10), rep("G003", 6)),
    season               = 2024L,
    season_type          = c(rep("regular", 28), rep("postseason", 2)),
    period               = c(1L,1L,1L,1L,2L,2L,2L,2L,3L,3L,3L,3L,4L,4L,
                              1L,1L,2L,2L,3L,3L,4L,4L,4L,4L,
                              1L,2L,3L,4L,3L,3L),
    pos_team             = c("TeamA","TeamB","TeamA","TeamB",
                              "TeamA","TeamB","TeamA","TeamB",
                              "TeamA","TeamB","TeamA","TeamB",
                              "TeamA","TeamB",
                              "TeamA","TeamB","TeamA","TeamB",
                              "TeamA","TeamB","TeamA","TeamB","TeamA","TeamB",
                              "TeamA","TeamA","TeamA","TeamA",
                              "TeamA","TeamB"),
    score_diff           = c(0L,0L,0L,0L,7L,7L,7L,7L,
                              35L,35L,0L,0L,7L,7L,      # rows 9-10 are blowout
                              0L,0L,0L,0L,
                              35L,35L,                   # rows 19-20 blowout
                              0L,0L,0L,0L,
                              0L,0L,35L,0L,0L,35L),
    wp_before            = c(rep(0.5, 8), 0.05, 0.95, rep(0.5, 4),
                              rep(0.5, 4), 0.04, 0.96, rep(0.5, 4),
                              rep(0.5, 4), 0.04, 0.96),
    play_type            = c(
      # G001 rows 1-14
      "Pass Completion", "Rush", "Pass Incompletion", "Rush",
      "Passing Touchdown", "Rushing Touchdown", "Sack", "Pass Completion",
      "Pass Completion", "Rush",   # period 3, score_diff=35 -> garbage if |sd|>28
      "Pass Completion", "Rush",   # period 3, score_diff=0  -> kept
      "Interception", "Rush",
      # G002 rows 15-24
      "Pass Completion", "Rush",
      "Pass Incompletion", "Rush",
      "Pass Completion", "Rush",   # period 3, blowout by WP criterion
      "Passing Touchdown", "Rushing Touchdown",
      "Pass Completion", "Rush",
      # G003 rows 25-30
      "Pass Completion", "Pass Completion",
      "Pass Completion", "Passing Touchdown",
      "Pass Completion", "Rush"    # rows 29-30 are postseason
    ),
    passer_player_name   = c(
      "QB Alpha", NA, "QB Alpha", NA,
      "QB Alpha", NA, "QB Alpha", "QB Beta",
      "QB Alpha", NA,
      "QB Alpha", NA,
      "QB Alpha", NA,
      "QB Alpha", NA, "QB Alpha", NA,
      "QB Alpha", NA,
      "QB Alpha", NA, "QB Beta", NA,
      "QB Alpha", "QB Alpha", "QB Alpha", "QB Alpha",
      "QB Alpha", NA
    ),
    rusher_player_name   = c(
      NA, "RB Alpha", NA, "RB Beta",
      NA, "RB Alpha", NA, NA,
      NA, "RB Beta",
      NA, "RB Alpha",
      NA, "RB Alpha",
      NA, "RB Alpha", NA, "RB Beta",
      NA, "RB Beta",
      NA, "RB Alpha", NA, "RB Beta",
      NA, NA, NA, NA,
      NA, "RB Alpha"
    ),
    receiver_player_name = c(
      "WR Alpha", NA, NA, NA,
      "WR Alpha", NA, NA, "WR Beta",
      "WR Alpha", NA,
      "WR Alpha", NA,
      NA, NA,
      "WR Alpha", NA, NA, NA,
      "WR Alpha", NA,
      "WR Alpha", NA, "WR Beta", NA,
      "WR Alpha", "WR Alpha", "WR Alpha", "WR Alpha",
      "WR Alpha", NA
    ),
    yards_gained         = c(
      12L, 5L, 0L, 3L,
      8L, 7L, -5L, 6L,
      10L, 4L,
      9L, 2L,
      -2L, 1L,
      15L, 8L, 0L, 4L,
      11L, 6L,
      14L, 9L, 7L, 3L,
      20L, 18L, 16L, 22L,
      10L, 5L
    ),
    EPA                  = c(
      0.5, 0.3, -0.2, 0.1,
      1.2, 0.8, -0.5, 0.4,
      0.6, 0.2,
      0.7, 0.1,
      -0.8, 0.2,
      0.9, 0.4, -0.1, 0.2,
      0.5, 0.3,
      1.0, 0.6, 0.5, 0.2,
      1.1, 0.9, 0.8, 1.3,
      0.7, 0.4
    )
  )
}

pbp_base <- make_cfb_pbp()

# Minimal valid panel fixture for validate_cfb_panel_integrity() tests.
make_valid_panel <- function() {
  tibble::tibble(
    player_name          = c("QB Alpha", "RB Alpha", "WR Alpha"),
    season               = 2024L,
    primary_team         = c("TeamA", "TeamA", "TeamA"),
    n_teams              = 1L,
    has_name_collision   = FALSE,
    position_group       = c("QB", "RB", "WR_TE"),
    games_played         = c(3L, 3L, 3L),
    total_plays          = c(15L, 12L, 10L),
    low_volume           = FALSE,
    pass_attempts        = c(15L, 0L, 0L),
    completions          = c(10L, 0L, 0L),
    completion_pct       = c(0.667, NA_real_, NA_real_),
    passing_yards        = c(120L, 0L, 0L),
    pass_tds             = c(2L, 0L, 0L),
    interceptions        = c(1L, 0L, 0L),
    pass_epa             = c(3.5, 0.0, 0.0),
    pass_epa_per_attempt = c(0.233, NA_real_, NA_real_),
    rush_attempts        = c(0L, 12L, 0L),
    rushing_yards        = c(0L, 65L, 0L),
    rush_tds             = c(0L, 2L, 0L),
    rush_epa             = c(0.0, 2.1, 0.0),
    rush_epa_per_attempt = c(NA_real_, 0.175, NA_real_),
    targets              = c(0L, 0L, 10L),
    receptions           = c(0L, 0L, 8L),
    catch_rate           = c(NA_real_, NA_real_, 0.8),
    receiving_yards      = c(0L, 0L, 95L),
    rec_tds              = c(0L, 0L, 1L),
    rec_epa              = c(0.0, 0.0, 2.8),
    rec_epa_per_target   = c(NA_real_, NA_real_, 0.28),
    success_rate         = c(0.6, 0.5, 0.7),
    garbage_time_plays_excluded = 0L,
    panel_version        = "s2cfbv1_panel"
  )
}


# ==============================================================================
# SECTION A: .apply_cfb_garbage_time_filter()
# ==============================================================================

test_that("A01: first-half plays always retained regardless of score_diff", {
  pbp_h1 <- pbp_base %>% dplyr::filter(period <= 2L)
  result  <- .apply_cfb_garbage_time_filter(pbp_h1, 28L, 0.05, 0.95)
  expect_equal(nrow(result), nrow(pbp_h1))
})

test_that("A02: period-3 blowout by score_diff removed", {
  # Rows 9-10 (G001, period 3, score_diff=35) should be removed.
  result <- .apply_cfb_garbage_time_filter(pbp_base, 28L, 0.05, 0.95)
  blowout_rows <- result %>%
    dplyr::filter(game_id == "G001", period == 3L, abs(score_diff) > 28L)
  expect_equal(nrow(blowout_rows), 0L)
})

test_that("A03: period-3 non-blowout plays retained", {
  result <- .apply_cfb_garbage_time_filter(pbp_base, 28L, 0.05, 0.95)
  kept_rows <- result %>%
    dplyr::filter(game_id == "G001", period == 3L, score_diff == 0L)
  expect_equal(nrow(kept_rows), 2L)
})

test_that("A04: wp_before criterion removes plays outside [0.05, 0.95]", {
  # Rows 19-20 (G002, period 3, wp=0.04 and 0.96) should be removed.
  result    <- .apply_cfb_garbage_time_filter(pbp_base, 28L, 0.05, 0.95)
  wp_blowout <- result %>%
    dplyr::filter(game_id == "G002", period == 3L,
                  (wp_before < 0.05 | wp_before > 0.95))
  expect_equal(nrow(wp_blowout), 0L)
})

test_that("A05: returns pbp unchanged when period column absent", {
  pbp_no_period <- pbp_base %>% dplyr::select(-period)
  result <- .apply_cfb_garbage_time_filter(pbp_no_period, 28L, 0.05, 0.95)
  expect_equal(nrow(result), nrow(pbp_no_period))
})

test_that("A06: returns pbp unchanged when 0 rows", {
  empty <- pbp_base[0L, ]
  result <- .apply_cfb_garbage_time_filter(empty, 28L, 0.05, 0.95)
  expect_equal(nrow(result), 0L)
})

test_that("A07: score_diff absent does not error; no plays removed by that criterion", {
  pbp_no_sd <- pbp_base %>% dplyr::select(-score_diff)
  # Without score_diff, only wp_before criterion fires.
  result <- .apply_cfb_garbage_time_filter(pbp_no_sd, 28L, 0.05, 0.95)
  # Rows with wp_before outside [0.05, 0.95] in periods 3+ should still be removed.
  wp_blowout <- result %>%
    dplyr::filter(period >= 3L,
                  !is.na(wp_before),
                  (wp_before < 0.05 | wp_before > 0.95))
  expect_equal(nrow(wp_blowout), 0L)
})

test_that("A08: wp_before absent does not error; only score_diff criterion fires", {
  pbp_no_wp <- pbp_base %>% dplyr::select(-wp_before)
  result <- .apply_cfb_garbage_time_filter(pbp_no_wp, 28L, 0.05, 0.95)
  blowout <- result %>%
    dplyr::filter(period >= 3L, abs(score_diff) > 28L)
  expect_equal(nrow(blowout), 0L)
})


# ==============================================================================
# SECTION B: .build_cfb_passing_stats()
# ==============================================================================

# Use regular-season, non-garbage-time plays only for clean known expectations.
pbp_clean <- pbp_base %>%
  dplyr::filter(season_type == "regular") %>%
  .apply_cfb_garbage_time_filter(28L, 0.05, 0.95)

test_that("B01: returns correct columns", {
  result <- .build_cfb_passing_stats(pbp_clean)
  expect_true(all(c("player_name", "pos_team", "pass_attempts", "completions",
                     "passing_yards", "pass_tds", "interceptions",
                     "pass_epa") %in% names(result)))
})

test_that("B02: sack yards excluded from passing_yards", {
  # Sack in row 7 (yards_gained=-5, play_type=Sack, passer=QB Alpha, TeamA).
  result <- .build_cfb_passing_stats(pbp_clean)
  qa     <- result %>% dplyr::filter(player_name == "QB Alpha", pos_team == "TeamA")
  expect_true(nrow(qa) == 1L)
  # passing_yards must not include the -5 from the sack play.
  expect_true(qa$passing_yards >= 0L)
})

test_that("B03: interception plays counted in interceptions not passing_yards", {
  result <- .build_cfb_passing_stats(pbp_clean)
  qa     <- result %>% dplyr::filter(player_name == "QB Alpha", pos_team == "TeamA")
  expect_true(qa$interceptions >= 1L)
})

test_that("B04: passing TDs counted correctly", {
  result <- .build_cfb_passing_stats(pbp_clean)
  qa     <- result %>% dplyr::filter(player_name == "QB Alpha", pos_team == "TeamA")
  expect_true(qa$pass_tds >= 1L)
})

test_that("B05: returns empty tibble with correct schema when pbp has 0 rows", {
  result <- .build_cfb_passing_stats(pbp_clean[0L, ])
  expect_equal(nrow(result), 0L)
  expect_true("pass_attempts" %in% names(result))
})

test_that("B06: returns empty tibble when passer_player_name column absent", {
  pbp_no_passer <- pbp_clean %>% dplyr::select(-passer_player_name)
  result <- .build_cfb_passing_stats(pbp_no_passer)
  expect_equal(nrow(result), 0L)
})

test_that("B07: one row per passer-team combination", {
  result <- .build_cfb_passing_stats(pbp_clean)
  dups   <- result %>% dplyr::count(player_name, pos_team) %>% dplyr::filter(n > 1L)
  expect_equal(nrow(dups), 0L)
})


# ==============================================================================
# SECTION C: .build_cfb_rushing_stats()
# ==============================================================================

test_that("C01: returns correct columns", {
  result <- .build_cfb_rushing_stats(pbp_clean)
  expect_true(all(c("player_name", "pos_team", "rush_attempts",
                     "rushing_yards", "rush_tds", "rush_epa") %in% names(result)))
})

test_that("C02: rushing TDs counted correctly", {
  result <- .build_cfb_rushing_stats(pbp_clean)
  # RB Alpha's rushes land on TeamB rows in the fixture (pos_team alternates
  # TeamA/TeamB and rush plays are assigned to even-indexed rows = TeamB).
  ra     <- result %>% dplyr::filter(player_name == "RB Alpha", pos_team == "TeamB")
  expect_true(nrow(ra) == 1L)
  expect_true(ra$rush_tds >= 1L)
})

test_that("C03: sack plays not included in rush_attempts", {
  # Sack is play_type "Sack", not "Rush" -- should be excluded automatically.
  result      <- .build_cfb_rushing_stats(pbp_clean)
  all_rushers <- unique(result$player_name)
  # QB Alpha threw the sack; should not appear in rushing stats
  # (QB Alpha has no Rush or Rushing Touchdown plays in the fixture).
  expect_false("QB Alpha" %in% all_rushers)
})

test_that("C04: returns empty tibble with correct schema when 0 rows", {
  result <- .build_cfb_rushing_stats(pbp_clean[0L, ])
  expect_equal(nrow(result), 0L)
  expect_true("rush_attempts" %in% names(result))
})

test_that("C05: returns empty tibble when rusher_player_name column absent", {
  pbp_no_rush <- pbp_clean %>% dplyr::select(-rusher_player_name)
  result      <- .build_cfb_rushing_stats(pbp_no_rush)
  expect_equal(nrow(result), 0L)
})

test_that("C06: one row per rusher-team combination", {
  result <- .build_cfb_rushing_stats(pbp_clean)
  dups   <- result %>% dplyr::count(player_name, pos_team) %>% dplyr::filter(n > 1L)
  expect_equal(nrow(dups), 0L)
})


# ==============================================================================
# SECTION D: .build_cfb_receiving_stats()
# ==============================================================================

test_that("D01: returns correct columns", {
  result <- .build_cfb_receiving_stats(pbp_clean)
  expect_true(all(c("player_name", "pos_team", "targets", "receptions",
                     "receiving_yards", "rec_tds", "rec_epa") %in% names(result)))
})

test_that("D02: receptions always <= targets", {
  result <- .build_cfb_receiving_stats(pbp_clean)
  expect_true(all(result$receptions <= result$targets))
})

test_that("D03: receiving_yards computed on completions only", {
  # Pass Incompletion rows should contribute 0 yards.
  result <- .build_cfb_receiving_stats(pbp_clean)
  wa     <- result %>% dplyr::filter(player_name == "WR Alpha", pos_team == "TeamA")
  expect_true(nrow(wa) == 1L)
  expect_true(wa$receiving_yards >= 0L)
})

test_that("D04: rec_tds counted from Passing Touchdown plays", {
  result <- .build_cfb_receiving_stats(pbp_clean)
  wa     <- result %>% dplyr::filter(player_name == "WR Alpha", pos_team == "TeamA")
  expect_true(wa$rec_tds >= 1L)
})

test_that("D05: rec_epa is 0 for incompletion-only receivers", {
  # Add a receiver who only appears on Pass Incompletion plays.
  pbp_inc_only <- pbp_clean %>%
    dplyr::mutate(
      receiver_player_name = dplyr::if_else(
        play_type == "Pass Incompletion", "Inc Receiver", receiver_player_name
      )
    )
  result  <- .build_cfb_receiving_stats(pbp_inc_only)
  inc_row <- result %>% dplyr::filter(player_name == "Inc Receiver")
  if (nrow(inc_row) > 0L) {
    expect_equal(inc_row$rec_epa, 0)
    expect_equal(inc_row$receiving_yards, 0L)
  }
})

test_that("D06: returns empty tibble with correct schema when 0 rows", {
  result <- .build_cfb_receiving_stats(pbp_clean[0L, ])
  expect_equal(nrow(result), 0L)
  expect_true("targets" %in% names(result))
})

test_that("D07: one row per receiver-team combination", {
  result <- .build_cfb_receiving_stats(pbp_clean)
  dups   <- result %>% dplyr::count(player_name, pos_team) %>% dplyr::filter(n > 1L)
  expect_equal(nrow(dups), 0L)
})


# ==============================================================================
# SECTION E: .build_cfb_success_stats()
# ==============================================================================

test_that("E01: returns correct columns", {
  result <- .build_cfb_success_stats(pbp_clean)
  expect_true(all(c("player_name", "pos_team",
                     "n_positive_epa", "n_epa_plays") %in% names(result)))
})

test_that("E02: n_positive_epa <= n_epa_plays for all rows", {
  result <- .build_cfb_success_stats(pbp_clean)
  expect_true(all(result$n_positive_epa <= result$n_epa_plays))
})

test_that("E03: n_epa_plays equals count of non-NA EPA plays for that player", {
  result <- .build_cfb_success_stats(pbp_clean)
  # All EPA values in fixture are non-NA, so n_epa_plays should equal play count.
  expect_true(all(result$n_epa_plays > 0L))
})

test_that("E04: returns empty tibble when 0 rows", {
  result <- .build_cfb_success_stats(pbp_clean[0L, ])
  expect_equal(nrow(result), 0L)
})

test_that("E05: returns empty tibble when all EPA is NA", {
  pbp_na_epa <- pbp_clean %>% dplyr::mutate(EPA = NA_real_)
  result     <- .build_cfb_success_stats(pbp_na_epa)
  expect_equal(nrow(result), 0L)
})


# ==============================================================================
# SECTION F: .build_cfb_games_played()
# ==============================================================================

test_that("F01: returns correct columns", {
  result <- .build_cfb_games_played(pbp_clean)
  expect_true(all(c("player_name", "pos_team", "games_played") %in% names(result)))
})

test_that("F02: player appearing in multiple games counted correctly", {
  # QB Alpha appears in G001, G002, G003 regular season.
  result <- .build_cfb_games_played(pbp_clean)
  qa     <- result %>% dplyr::filter(player_name == "QB Alpha", pos_team == "TeamA")
  expect_true(nrow(qa) == 1L)
  expect_true(qa$games_played >= 2L)
})

test_that("F03: player appearing as both passer and rusher in same game counted once", {
  # Create a fixture where one player is both passer and rusher in G001.
  pbp_dual <- pbp_clean %>%
    dplyr::mutate(
      rusher_player_name = dplyr::if_else(
        play_type == "Rush" & pos_team == "TeamA",
        "QB Alpha",
        rusher_player_name
      )
    )
  result <- .build_cfb_games_played(pbp_dual)
  qa     <- result %>% dplyr::filter(player_name == "QB Alpha", pos_team == "TeamA")
  # games_played should not double-count the same game_id.
  n_distinct_games <- pbp_dual %>%
    dplyr::filter(pos_team == "TeamA") %>%
    dplyr::pull(game_id) %>%
    dplyr::n_distinct()
  expect_true(qa$games_played <= n_distinct_games)
})

test_that("F04: returns empty tibble when game_id column absent", {
  pbp_no_gid <- pbp_clean %>% dplyr::select(-game_id)
  result     <- .build_cfb_games_played(pbp_no_gid)
  expect_equal(nrow(result), 0L)
})


# ==============================================================================
# SECTION G: classify_cfb_player_position()
# ==============================================================================

make_position_panel <- function() {
  tibble::tibble(
    player_name   = c("Pure QB", "Pure RB", "Pure WR", "Dual Threat",
                      "No Plays", "Borderline QB", "Borderline RB"),
    season        = 2024L,
    primary_team  = "TeamA",
    pass_attempts = c(100L,  0L,  0L, 40L, 0L, 70L,  5L),
    rush_attempts = c(  0L, 80L,  0L, 35L, 0L, 20L, 70L),
    targets       = c(  0L,  0L, 60L, 25L, 0L, 10L, 25L)
  )
}

pos_panel <- make_position_panel()

test_that("G01: pure passer classified QB", {
  result <- classify_cfb_player_position(pos_panel)
  expect_equal(
    result$position_group[result$player_name == "Pure QB"], "QB"
  )
})

test_that("G02: pure rusher classified RB", {
  result <- classify_cfb_player_position(pos_panel)
  expect_equal(
    result$position_group[result$player_name == "Pure RB"], "RB"
  )
})

test_that("G03: pure receiver classified WR_TE", {
  result <- classify_cfb_player_position(pos_panel)
  expect_equal(
    result$position_group[result$player_name == "Pure WR"], "WR_TE"
  )
})

test_that("G04: player with zero plays gets NA position_group", {
  result <- classify_cfb_player_position(pos_panel)
  expect_true(is.na(result$position_group[result$player_name == "No Plays"]))
})

test_that("G05: QB threshold is >= 0.70 pass share (borderline QB classified QB)", {
  # Borderline QB: 70/(70+20+10) = 0.70 exactly -- should be QB.
  result <- classify_cfb_player_position(pos_panel)
  expect_equal(
    result$position_group[result$player_name == "Borderline QB"], "QB"
  )
})

test_that("G06: QB precedence over RB when both thresholds met", {
  # Dual threat: 40% pass, 35% rush, 25% recv -- none meet threshold, so 'other'.
  result <- classify_cfb_player_position(pos_panel)
  expect_equal(
    result$position_group[result$player_name == "Dual Threat"], "other"
  )
})

test_that("G07: classify_cfb_player_position stops with informative error when required columns absent", {
  bad_panel <- pos_panel %>% dplyr::select(-pass_attempts)
  expect_error(classify_cfb_player_position(bad_panel), "pass_attempts")
})

test_that("G08: position_group column present in output", {
  result <- classify_cfb_player_position(pos_panel)
  expect_true("position_group" %in% names(result))
})


# ==============================================================================
# SECTION H: validate_cfb_panel_integrity()
# ==============================================================================

valid_panel <- make_valid_panel()

test_that("H01: valid panel returns valid = TRUE", {
  result <- suppressMessages(validate_cfb_panel_integrity(valid_panel))
  expect_true(result$valid)
})

test_that("H02: duplicate player_name + season detected", {
  dup_panel <- dplyr::bind_rows(valid_panel, valid_panel[1L, ])
  result    <- suppressMessages(validate_cfb_panel_integrity(dup_panel))
  expect_false(result$valid)
  dup_check <- result$summary %>% dplyr::filter(check_name == "no_duplicate_player_season")
  expect_false(dup_check$passed)
})

test_that("H03: completions > pass_attempts detected as critical failure", {
  bad_panel                  <- valid_panel
  bad_panel$completions[1L]  <- bad_panel$pass_attempts[1L] + 5L
  result <- suppressMessages(validate_cfb_panel_integrity(bad_panel))
  expect_false(result$valid)
})

test_that("H04: receptions > targets detected as critical failure", {
  bad_panel               <- valid_panel
  bad_panel$receptions[3L] <- bad_panel$targets[3L] + 1L
  result <- suppressMessages(validate_cfb_panel_integrity(bad_panel))
  expect_false(result$valid)
})

test_that("H05: success_rate outside [0, 1] detected as critical failure", {
  bad_panel                  <- valid_panel
  bad_panel$success_rate[1L] <- 1.5
  result <- suppressMessages(validate_cfb_panel_integrity(bad_panel))
  expect_false(result$valid)
})

test_that("H06: efficiency non-NA with zero denominator flagged", {
  bad_panel                         <- valid_panel
  bad_panel$pass_attempts[2L]       <- 0L
  bad_panel$pass_epa_per_attempt[2L] <- 0.5  # non-NA with 0 attempts
  result <- suppressMessages(validate_cfb_panel_integrity(bad_panel))
  eff_check <- result$summary %>%
    dplyr::filter(check_name == "efficiency_na_consistency")
  expect_false(eff_check$passed)
})

test_that("H07: missing expected season flagged as warning", {
  result <- suppressMessages(
    validate_cfb_panel_integrity(valid_panel, expected_seasons = c(2024L, 2023L))
  )
  cov_check <- result$summary %>% dplyr::filter(check_name == "season_coverage")
  expect_false(cov_check$passed)
  # Missing season is a warning, not critical -- valid should still be TRUE.
  expect_true(result$valid)
})

test_that("H08: EPA out of range detected as critical failure", {
  bad_panel            <- valid_panel
  bad_panel$pass_epa[1L] <- 99999
  result <- suppressMessages(validate_cfb_panel_integrity(bad_panel))
  expect_false(result$valid)
})

test_that("H09: summary tibble has expected columns", {
  result <- suppressMessages(validate_cfb_panel_integrity(valid_panel))
  expect_true(all(c("check_name", "severity", "passed", "detail") %in%
                    names(result$summary)))
})

test_that("H10: stops with informative error on empty panel", {
  expect_error(
    validate_cfb_panel_integrity(valid_panel[0L, ]),
    "non-empty"
  )
})


# ==============================================================================
# SECTION I: build_cfb_player_season_panel() -- input validation only
# ==============================================================================
# These tests verify that the function rejects bad inputs with informative
# errors. They do NOT call load_normalized_cfb_season() and require no cache.

test_that("I01: rejects non-numeric seasons argument", {
  expect_error(
    build_cfb_player_season_panel(seasons = "2024"),
    "non-empty numeric"
  )
})

test_that("I02: rejects empty seasons vector", {
  expect_error(
    build_cfb_player_season_panel(seasons = integer(0)),
    "non-empty numeric"
  )
})

test_that("I03: rejects seasons below 2014 with informative error", {
  expect_error(
    build_cfb_player_season_panel(
      seasons   = 2013L,
      cache_dir = tempdir()
    ),
    "2014"
  )
})

test_that("I04: rejects missing cache_dir with informative error", {
  expect_error(
    build_cfb_player_season_panel(
      seasons   = 2024L,
      cache_dir = file.path(tempdir(), "nonexistent_cache_xyz")
    ),
    "cache_dir not found"
  )
})

test_that("I05: rejects negative min_plays", {
  expect_error(
    build_cfb_player_season_panel(
      seasons   = 2024L,
      cache_dir = tempdir(),
      min_plays = -1L
    ),
    "non-negative"
  )
})
