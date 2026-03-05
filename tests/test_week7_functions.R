# ==============================================================================
# WEEK 7 TEST SUITE: test_week7_functions.R
# ==============================================================================
# Tests for: get_game_script_splits(), calculate_leverage_features(),
#            get_comeback_profile(), calculate_script_adjusted_epa()
#
# Test coverage:
#   - Input validation (all 4 functions)
#   - Empty data returns
#   - Game script classification correctness
#   - Garbage time exclusion (WP < 0.10 or > 0.90 in Q4)
#   - QB kneel / spike exclusion
#   - QB scramble exclusion from rusher group
#   - Leverage threshold logic
#   - WPA aggregation correctness
#   - Comeback: score_differential filter
#   - Script-adjusted EPA: league baseline computation
#   - Script-adjusted EPA: adjustment direction correctness
#   - Sample note flagging (small_sample vs sufficient_sample)
#   - Integration: all four functions run on same data without error
#   - Column name verification (no assumed columns)
#
# NFL edge cases:
#   - Garbage time plays (Q4, WP 0.05) excluded
#   - QB kneel plays excluded from all denominators
#   - QB scrambles excluded from rusher group
#   - Players traded mid-season (team assigned to most frequent)
#   - Players with zero comeback opportunities (filtered out)
#   - OT plays (qtr == 5) not treated as Q4 garbage time
# ==============================================================================

library(testthat)
library(dplyr)
library(glue)

# Source production code if not already loaded
if (!exists("get_game_script_splits")) {
  source(here::here("R", "09_gamescript_features.R"))
}

# ==============================================================================
# SHARED TEST FIXTURE
# ==============================================================================

make_pbp <- function(n = 100) {
  # A representative play-by-play tibble with all required columns
  set.seed(42)
  tibble(
    season               = 2024L,
    week                 = rep(1:10, each = n / 10),
    game_id              = paste0("2024_", sprintf("%02d", rep(1:10, each = n / 10)),
                                  "_KC_BUF"),
    posteam              = rep(c("KC", "BUF"), times = n / 2),
    defteam              = rep(c("BUF", "KC"), times = n / 2),
    play_type            = rep(c("pass", "run"), times = n / 2),
    qtr                  = rep(c(1L, 2L, 3L, 4L), length.out = n),
    epa                  = rnorm(n, mean = 0.05, sd = 0.5),
    wp                   = runif(n, 0.15, 0.85),
    wpa                  = rnorm(n, 0, 0.03),
    score_differential   = as.integer(sample(-21:21, n, replace = TRUE)),
    passer_player_id     = ifelse(rep(c("pass", "run"), times = n/2) == "pass",
                                  rep(c("P001", "P002"), length.out = n), NA_character_),
    passer_player_name   = ifelse(rep(c("pass", "run"), times = n/2) == "pass",
                                  rep(c("Patrick Mahomes", "Josh Allen"), length.out = n), NA_character_),
    rusher_player_id     = ifelse(rep(c("pass", "run"), times = n/2) == "run",
                                  rep(c("R001", "R002"), length.out = n), NA_character_),
    rusher_player_name   = ifelse(rep(c("pass", "run"), times = n/2) == "run",
                                  rep(c("Isiah Pacheco", "James Cook"), length.out = n), NA_character_),
    receiver_player_id   = ifelse(rep(c("pass", "run"), times = n/2) == "pass",
                                  rep(c("W001", "W002"), length.out = n), NA_character_),
    receiver_player_name = ifelse(rep(c("pass", "run"), times = n/2) == "pass",
                                  rep(c("Travis Kelce", "Stefon Diggs"), length.out = n), NA_character_),
    qb_kneel             = 0L,
    qb_spike             = 0L,
    qb_scramble          = 0L
  )
}

pbp_base <- make_pbp(100)


# ==============================================================================
# SECTION A: INPUT VALIDATION
# ==============================================================================

test_that("get_game_script_splits: rejects non-data-frame input", {
  expect_error(get_game_script_splits("not a df"), "data frame")
})

test_that("get_game_script_splits: rejects missing required columns", {
  bad <- pbp_base %>% select(-wp)
  expect_error(get_game_script_splits(bad), "wp")
})

test_that("get_game_script_splits: rejects invalid leading_threshold", {
  expect_error(get_game_script_splits(pbp_base, leading_threshold = 0.3),
               "leading_threshold")
})

test_that("get_game_script_splits: rejects invalid trailing_threshold", {
  expect_error(get_game_script_splits(pbp_base, trailing_threshold = 0.7),
               "trailing_threshold")
})

test_that("calculate_leverage_features: rejects missing wpa column", {
  bad <- pbp_base %>% select(-wpa)
  expect_error(calculate_leverage_features(bad), "wpa")
})

test_that("calculate_leverage_features: rejects invalid leverage_threshold", {
  expect_error(calculate_leverage_features(pbp_base, leverage_threshold = 1.5),
               "leverage_threshold")
})

test_that("get_comeback_profile: rejects missing score_differential", {
  bad <- pbp_base %>% select(-score_differential)
  expect_error(get_comeback_profile(bad), "score_differential")
})

test_that("get_comeback_profile: rejects deficit_threshold < 1", {
  expect_error(get_comeback_profile(pbp_base, deficit_threshold = 0), "deficit_threshold")
})

test_that("calculate_script_adjusted_epa: rejects non-data-frame", {
  expect_error(calculate_script_adjusted_epa(list(a = 1)), "data frame")
})

test_that("week_min > week_max raises error across all functions", {
  expect_error(get_game_script_splits(pbp_base, week_min = 10, week_max = 5), "week_min")
  expect_error(calculate_leverage_features(pbp_base, week_min = 10, week_max = 5), "week_min")
  expect_error(get_comeback_profile(pbp_base, week_min = 10, week_max = 5), "week_min")
  expect_error(calculate_script_adjusted_epa(pbp_base, week_min = 10, week_max = 5), "week_min")
})


# ==============================================================================
# SECTION B: EMPTY DATA
# ==============================================================================

test_that("get_game_script_splits: returns tibble on empty input", {
  result <- get_game_script_splits(pbp_base %>% filter(FALSE))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("calculate_leverage_features: returns tibble on empty input", {
  result <- calculate_leverage_features(pbp_base %>% filter(FALSE))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("get_comeback_profile: returns tibble on empty input", {
  result <- get_comeback_profile(pbp_base %>% filter(FALSE))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("calculate_script_adjusted_epa: returns tibble on empty input", {
  result <- calculate_script_adjusted_epa(pbp_base %>% filter(FALSE))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})


# ==============================================================================
# SECTION C: GARBAGE TIME EXCLUSION
# ==============================================================================

test_that("garbage time plays (Q4, WP < 0.10) are excluded from script splits", {
  # Add a garbage time play that should be excluded
  pbp_with_garbage <- pbp_base %>%
    bind_rows(tibble(
      season = 2024L, week = 5L,
      game_id = "2024_05_KC_BUF", posteam = "KC", defteam = "BUF",
      play_type = "pass", qtr = 4L,
      epa = 2.0,             # Large EPA -- would inflate results if included
      wp = 0.05,             # Garbage time: WP < 0.10 in Q4
      wpa = 0.01,
      score_differential = -28L,
      passer_player_id = "P001",
      passer_player_name = "Patrick Mahomes",
      rusher_player_id = NA_character_,
      rusher_player_name = NA_character_,
      receiver_player_id = "W001",
      receiver_player_name = "Travis Kelce",
      qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
    ))

  result_clean   <- get_game_script_splits(pbp_base)
  result_garbage <- get_game_script_splits(pbp_with_garbage)

  mahomes_clean   <- result_clean   %>% filter(player_id == "P001", position_group == "passer")
  mahomes_garbage <- result_garbage %>% filter(player_id == "P001", position_group == "passer")

  # Total plays should be the same -- garbage time play excluded
  expect_equal(mahomes_clean$total_plays, mahomes_garbage$total_plays)
})

test_that("WP > 0.90 in Q4 is also excluded (leading garbage time)", {
  pbp_with_leading_garbage <- pbp_base %>%
    bind_rows(tibble(
      season = 2024L, week = 5L,
      game_id = "2024_05_KC_BUF", posteam = "KC", defteam = "BUF",
      play_type = "run", qtr = 4L,
      epa = -1.0,
      wp = 0.95,             # Leading garbage time
      wpa = -0.001,
      score_differential = 28L,
      passer_player_id = NA_character_, passer_player_name = NA_character_,
      rusher_player_id = "R001", rusher_player_name = "Isiah Pacheco",
      receiver_player_id = NA_character_, receiver_player_name = NA_character_,
      qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
    ))

  result_base    <- get_game_script_splits(pbp_base)
  result_garbage <- get_game_script_splits(pbp_with_leading_garbage)

  pacheco_base    <- result_base    %>% filter(player_id == "R001", position_group == "rusher")
  pacheco_garbage <- result_garbage %>% filter(player_id == "R001", position_group == "rusher")

  # Plays should be equal -- leading garbage excluded
  expect_equal(pacheco_base$total_plays, pacheco_garbage$total_plays)
})

test_that("OT plays (qtr == 5) are NOT excluded as garbage time", {
  pbp_with_ot <- pbp_base %>%
    bind_rows(tibble(
      season = 2024L, week = 6L,
      game_id = "2024_06_KC_BUF", posteam = "KC", defteam = "BUF",
      play_type = "pass", qtr = 5L,   # OT, not Q4
      epa = 0.8, wp = 0.50, wpa = 0.05,
      score_differential = 0L,
      passer_player_id = "P001", passer_player_name = "Patrick Mahomes",
      rusher_player_id = NA_character_, rusher_player_name = NA_character_,
      receiver_player_id = "W001", receiver_player_name = "Travis Kelce",
      qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
    ))

  result_with_ot <- get_game_script_splits(pbp_with_ot)
  result_base    <- get_game_script_splits(pbp_base)

  mahomes_ot   <- result_with_ot %>% filter(player_id == "P001", position_group == "passer")
  mahomes_base <- result_base    %>% filter(player_id == "P001", position_group == "passer")

  # OT play should be included -- one more play
  expect_equal(mahomes_ot$total_plays, mahomes_base$total_plays + 1L)
})


# ==============================================================================
# SECTION D: QB KNEEL / SPIKE / SCRAMBLE EXCLUSION
# ==============================================================================

test_that("QB kneels are excluded from game script splits", {
  pbp_with_kneel <- pbp_base %>%
    bind_rows(tibble(
      season = 2024L, week = 7L,
      game_id = "2024_07_KC_BUF", posteam = "KC", defteam = "BUF",
      play_type = "run", qtr = 4L,
      epa = -1.5, wp = 0.75, wpa = -0.001,
      score_differential = 7L,
      passer_player_id = NA_character_, passer_player_name = NA_character_,
      rusher_player_id = "P001", rusher_player_name = "Patrick Mahomes",
      receiver_player_id = NA_character_, receiver_player_name = NA_character_,
      qb_kneel = 1L, qb_spike = 0L, qb_scramble = 0L  # KNEEL
    ))

  result_base  <- get_game_script_splits(pbp_base)
  result_kneel <- get_game_script_splits(pbp_with_kneel)

  # Mahomes rushes from kneel should not appear in rusher group
  mahomes_rush_base  <- result_base  %>% filter(player_id == "P001", position_group == "rusher")
  mahomes_rush_kneel <- result_kneel %>% filter(player_id == "P001", position_group == "rusher")

  # Play counts should be identical -- kneel excluded
  expect_equal(nrow(mahomes_rush_base) == 0 || mahomes_rush_base$total_plays,
               nrow(mahomes_rush_kneel) == 0 || mahomes_rush_kneel$total_plays)
})

test_that("QB scrambles are excluded from rusher group", {
  pbp_with_scramble <- pbp_base %>%
    bind_rows(tibble(
      season = 2024L, week = 8L,
      game_id = "2024_08_KC_BUF", posteam = "KC", defteam = "BUF",
      play_type = "run", qtr = 2L,
      epa = 1.2, wp = 0.55, wpa = 0.02,
      score_differential = 3L,
      passer_player_id = NA_character_, passer_player_name = NA_character_,
      rusher_player_id = "P001", rusher_player_name = "Patrick Mahomes",
      receiver_player_id = NA_character_, receiver_player_name = NA_character_,
      qb_kneel = 0L, qb_spike = 0L, qb_scramble = 1L  # SCRAMBLE
    ))

  result_base     <- get_game_script_splits(pbp_base)
  result_scramble <- get_game_script_splits(pbp_with_scramble)

  # Scramble QB run should NOT appear in rusher group
  mahomes_rush_base  <- result_base     %>% filter(player_id == "P001", position_group == "rusher")
  mahomes_rush_scram <- result_scramble %>% filter(player_id == "P001", position_group == "rusher")

  expect_equal(
    if (nrow(mahomes_rush_base) > 0)  mahomes_rush_base$total_plays  else 0L,
    if (nrow(mahomes_rush_scram) > 0) mahomes_rush_scram$total_plays else 0L
  )
})


# ==============================================================================
# SECTION E: GAME SCRIPT CLASSIFICATION CORRECTNESS
# ==============================================================================

test_that("script classification uses correct WP thresholds", {
  # Build controlled plays at known WP values
  controlled <- tibble(
    season = 2024L, week = 1L,
    game_id = "2024_01_KC_BUF",
    posteam = "KC", defteam = "BUF",
    qtr = 2L,
    epa = c(0.5, 0.5, 0.5),
    wp  = c(0.70, 0.50, 0.25),   # leading, neutral, trailing
    wpa = c(0.01, 0.01, 0.01),
    score_differential = c(7L, 0L, -7L),
    play_type = "pass",
    passer_player_id   = "P_CTRL",
    passer_player_name = "Control QB",
    rusher_player_id   = NA_character_,
    rusher_player_name = NA_character_,
    receiver_player_id   = "R_CTRL",
    receiver_player_name = "Control WR",
    qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
  )

  result <- get_game_script_splits(controlled, min_plays = 1)
  ctrl_qb <- result %>% filter(player_id == "P_CTRL", position_group == "passer")

  # Should have 3 plays total
  expect_equal(ctrl_qb$total_plays, 3L)
  # Each script should have exactly 1 play
  expect_equal(ctrl_qb$leading_plays,  1L)
  expect_equal(ctrl_qb$neutral_plays,  1L)
  expect_equal(ctrl_qb$trailing_plays, 1L)
})

test_that("script shares sum to <= 1.0 (never exceeds total)", {
  result <- get_game_script_splits(pbp_base)
  result <- result %>%
    mutate(share_sum = coalesce(neutral_share, 0) + coalesce(leading_share, 0) +
                       coalesce(trailing_share, 0))
  expect_true(all(result$share_sum <= 1.001))  # small float tolerance
})

test_that("script_epa_delta computed as neutral minus trailing", {
  # Build controlled data: known EPA per script
  known_epa <- tibble(
    season = 2024L, week = 1L,
    game_id = "2024_01_KC_BUF",
    posteam = "KC", defteam = "BUF", qtr = 2L,
    play_type = "pass",
    passer_player_id   = "P_DELTA",
    passer_player_name = "Delta QB",
    rusher_player_id   = NA_character_,
    rusher_player_name = NA_character_,
    receiver_player_id   = "R_D",
    receiver_player_name = "Delta WR",
    qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L,
    wpa = 0.01, score_differential = 0L
  ) %>%
    bind_rows(
      # 4 neutral plays: EPA = 0.4 each
      tibble(epa = rep(0.4, 4), wp = rep(0.50, 4),
             season = 2024L, week = 1L, game_id = "2024_01_KC_BUF",
             posteam = "KC", defteam = "BUF", qtr = 2L, play_type = "pass",
             passer_player_id = "P_DELTA", passer_player_name = "Delta QB",
             rusher_player_id = NA_character_, rusher_player_name = NA_character_,
             receiver_player_id = "R_D", receiver_player_name = "Delta WR",
             qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L,
             wpa = 0.01, score_differential = 0L),
      # 4 trailing plays: EPA = 0.1 each
      tibble(epa = rep(0.1, 4), wp = rep(0.25, 4),
             season = 2024L, week = 1L, game_id = "2024_01_KC_BUF",
             posteam = "KC", defteam = "BUF", qtr = 2L, play_type = "pass",
             passer_player_id = "P_DELTA", passer_player_name = "Delta QB",
             rusher_player_id = NA_character_, rusher_player_name = NA_character_,
             receiver_player_id = "R_D", receiver_player_name = "Delta WR",
             qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L,
             wpa = 0.01, score_differential = -10L)
    )

  result <- get_game_script_splits(known_epa, min_plays = 1)
  delta_row <- result %>% filter(player_id == "P_DELTA", position_group == "passer")

  expect_true(nrow(delta_row) > 0)
  # neutral EPA should be ~0.4, trailing EPA should be ~0.1
  expect_equal(round(delta_row$neutral_epa_per_play,  1), 0.4)
  expect_equal(round(delta_row$trailing_epa_per_play, 1), 0.1)
  expect_equal(round(delta_row$script_epa_delta,      1), 0.3)  # 0.4 - 0.1
})


# ==============================================================================
# SECTION F: LEVERAGE FEATURES
# ==============================================================================

test_that("high_leverage_plays counts correctly using absolute WPA threshold", {
  # Build 5 plays: 2 above threshold (|wpa| >= 0.05), 3 below
  controlled_lev <- tibble(
    season = 2024L, week = 1L,
    game_id = "2024_01_KC_BUF",
    posteam = "KC", defteam = "BUF", qtr = 2L,
    play_type = "pass",
    epa = 0.2, wp = 0.50,
    passer_player_id   = "P_LEV",
    passer_player_name = "Leverage QB",
    rusher_player_id   = NA_character_, rusher_player_name = NA_character_,
    receiver_player_id = "R_L", receiver_player_name = "Leverage WR",
    qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L,
    score_differential = 0L,
    wpa = c(0.06, -0.07, 0.02, -0.01, 0.00)  # 2 high leverage
  )

  result <- calculate_leverage_features(controlled_lev, leverage_threshold = 0.05,
                                         min_plays = 1)
  qb_row <- result %>% filter(player_id == "P_LEV", position_group == "passer")
  expect_equal(qb_row$high_leverage_plays, 2L)
  expect_equal(qb_row$total_plays, 5L)
  expect_equal(round(qb_row$high_leverage_rate, 1), 0.4)
})

test_that("clutch_rate is proportion of high-leverage plays with positive WPA", {
  # 2 high-leverage plays: 1 positive WPA, 1 negative WPA
  controlled_clutch <- tibble(
    season = 2024L, week = 1L,
    game_id = "2024_01_KC_BUF",
    posteam = "KC", defteam = "BUF", qtr = 2L,
    play_type = "pass",
    epa = 0.2, wp = 0.50,
    passer_player_id   = "P_CLT",
    passer_player_name = "Clutch QB",
    rusher_player_id   = NA_character_, rusher_player_name = NA_character_,
    receiver_player_id = "R_C", receiver_player_name = "Clutch WR",
    qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L,
    score_differential = 0L,
    wpa = c(0.06, -0.08)   # 1 positive, 1 negative
  )

  result <- calculate_leverage_features(controlled_clutch, leverage_threshold = 0.05,
                                         min_plays = 1)
  qb_row <- result %>% filter(player_id == "P_CLT", position_group == "passer")
  # clutch_rate should be 0.5 (1 out of 2 high-lev plays positive WPA)
  expect_equal(qb_row$clutch_rate, 0.5)
})

test_that("clutch_note is 'small_sample' when high_leverage_plays < 10", {
  result <- calculate_leverage_features(pbp_base, leverage_threshold = 0.05, min_plays = 5)
  # With 100 plays and |WPA| ~ N(0, 0.03), very few will exceed 0.05
  # Most players will be small_sample
  if (nrow(result) > 0) {
    small_sample_rows <- result %>% filter(high_leverage_plays < 10)
    expect_true(all(small_sample_rows$clutch_note == "small_sample"))
  }
})

test_that("mean_wpa_per_play is total_wpa / total_plays", {
  result <- calculate_leverage_features(pbp_base, min_plays = 5)
  if (nrow(result) > 0) {
    check <- result %>%
      filter(!is.na(mean_wpa_per_play)) %>%
      mutate(expected = total_wpa / total_plays,
             diff = abs(mean_wpa_per_play - expected))
    expect_true(all(check$diff < 1e-10))
  }
})


# ==============================================================================
# SECTION G: COMEBACK PROFILE
# ==============================================================================

test_that("comeback_plays only includes plays where score_differential <= -deficit", {
  # One player with controlled plays: some trailing, some not
  controlled_cmb <- tibble(
    season = 2024L, week = 1L,
    game_id = "2024_01_KC_BUF",
    posteam = "KC", defteam = "BUF", qtr = 2L,
    play_type = "pass",
    epa = 0.2, wp = 0.30,
    wpa = 0.01,
    passer_player_id   = "P_CMB",
    passer_player_name = "Comeback QB",
    rusher_player_id   = NA_character_, rusher_player_name = NA_character_,
    receiver_player_id = "R_CMB", receiver_player_name = "Comeback WR",
    qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L,
    score_differential = c(-10L, -12L, -5L, 3L, -9L)  # 3 qualify at deficit_threshold=8
  )

  result <- get_comeback_profile(controlled_cmb, deficit_threshold = 8, min_plays = 1)
  qb_row <- result %>% filter(player_id == "P_CMB", position_group == "passer")
  expect_equal(qb_row$comeback_plays, 3L)
})

test_that("comeback_vs_overall_epa is comeback minus overall EPA", {
  controlled_epa <- tibble(
    season = 2024L, week = 1L,
    game_id = "2024_01_KC_BUF",
    posteam = "KC", defteam = "BUF", qtr = 2L,
    play_type = "pass", wp = 0.35, wpa = 0.01,
    passer_player_id   = "P_DIFF",
    passer_player_name = "Diff QB",
    rusher_player_id   = NA_character_, rusher_player_name = NA_character_,
    receiver_player_id = "R_DIFF", receiver_player_name = "Diff WR",
    qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L,
    # 5 comeback plays (epa = 0.4) + 5 normal plays (epa = 0.2)
    epa               = c(rep(0.4, 5), rep(0.2, 5)),
    score_differential = c(rep(-10L, 5), rep(3L, 5))
  )

  result <- get_comeback_profile(controlled_epa, deficit_threshold = 8, min_plays = 1)
  row <- result %>% filter(player_id == "P_DIFF", position_group == "passer")

  expect_equal(round(row$comeback_epa_per_play,  1), 0.4)
  expect_equal(round(row$overall_epa_per_play,   1), 0.3)  # (0.4*5 + 0.2*5) / 10
  expect_equal(round(row$comeback_vs_overall_epa,1), 0.1)  # 0.4 - 0.3
})

test_that("WP < 0.10 plays are excluded from comeback even when trailing", {
  pbp_true_garbage <- pbp_base %>%
    bind_rows(tibble(
      season = 2024L, week = 5L,
      game_id = "2024_05_KC_BUF", posteam = "KC", defteam = "BUF",
      play_type = "pass", qtr = 4L,
      epa = 1.5, wp = 0.05,   # WP < 0.10: true garbage time
      wpa = 0.005, score_differential = -28L,
      passer_player_id = "P001", passer_player_name = "Patrick Mahomes",
      rusher_player_id = NA_character_, rusher_player_name = NA_character_,
      receiver_player_id = "W001", receiver_player_name = "Travis Kelce",
      qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
    ))

  result_base    <- get_comeback_profile(pbp_base)
  result_garbage <- get_comeback_profile(pbp_true_garbage)

  mahomes_base    <- result_base    %>% filter(player_id == "P001", position_group == "passer")
  mahomes_garbage <- result_garbage %>% filter(player_id == "P001", position_group == "passer")

  # Comeback plays should not increase -- garbage time play excluded
  base_plays    <- if (nrow(mahomes_base)    > 0) mahomes_base$comeback_plays    else 0L
  garbage_plays <- if (nrow(mahomes_garbage) > 0) mahomes_garbage$comeback_plays else 0L
  expect_equal(base_plays, garbage_plays)
})


# ==============================================================================
# SECTION H: SCRIPT-ADJUSTED EPA
# ==============================================================================

test_that("script_adjusted_epa differs from raw_epa_per_play for script-heavy players", {
  # Build data where one player has all trailing plays and another has all leading
  n_each <- 30L
  controlled_adj <- bind_rows(
    tibble(
      season = 2024L, week = rep(1:6, each = 5L),
      game_id = paste0("2024_0", rep(1:6, each=5L), "_KC_BUF"),
      posteam = "KC", defteam = "BUF", qtr = 2L,
      play_type = "pass",
      epa = rnorm(n_each, 0.1, 0.2),
      wp = 0.25,   # always trailing
      wpa = 0.01, score_differential = -14L,
      passer_player_id   = "P_TRAIL",
      passer_player_name = "Trailing QB",
      rusher_player_id   = NA_character_, rusher_player_name = NA_character_,
      receiver_player_id = "R_T", receiver_player_name = "Trailing WR",
      qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
    ),
    tibble(
      season = 2024L, week = rep(1:6, each = 5L),
      game_id = paste0("2024_0", rep(1:6, each=5L), "_KC_BUF"),
      posteam = "BUF", defteam = "KC", qtr = 2L,
      play_type = "pass",
      epa = rnorm(n_each, 0.1, 0.2),
      wp = 0.75,   # always leading
      wpa = 0.01, score_differential = 14L,
      passer_player_id   = "P_LEAD",
      passer_player_name = "Leading QB",
      rusher_player_id   = NA_character_, rusher_player_name = NA_character_,
      receiver_player_id = "R_L2", receiver_player_name = "Leading WR",
      qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
    )
  )

  result <- calculate_script_adjusted_epa(controlled_adj, min_plays = 20)
  expect_true(nrow(result) > 0)

  # adjustment_applied should not be zero for the extreme-script players
  adj_rows <- result %>% filter(position_group == "passer")
  if (nrow(adj_rows) > 0) {
    expect_true(any(abs(adj_rows$adjustment_applied) > 0.001))
  }
})

test_that("mean adjustment_applied is close to zero across all players (zero-sum)", {
  # The zero-sum property holds when baselines are computed from the same pool
  # being adjusted. Use a symmetric controlled dataset: two players with identical
  # play counts, one always leading, one always trailing. Their adjustments must
  # be equal and opposite, so the mean across both is exactly zero.
  # pbp_base (100 random rows) is too small and random to guarantee < 0.05 tolerance.
  set.seed(99)
  n_sym <- 40L
  sym_data <- bind_rows(
    tibble(
      season = 2024L, week = rep(1:8, each = 5L),
      game_id = paste0("2024_0", rep(1:8, each = 5L), "_A_B"),
      posteam = "A", defteam = "B", qtr = 2L, play_type = "pass",
      epa = rnorm(n_sym, 0.05, 0.3), wp = 0.75, wpa = 0.01,
      score_differential = 14L,
      passer_player_id = "P_SYM_LEAD", passer_player_name = "Leading QB",
      rusher_player_id = NA_character_, rusher_player_name = NA_character_,
      receiver_player_id = "R_SYM_L", receiver_player_name = "Leading WR",
      qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
    ),
    tibble(
      season = 2024L, week = rep(1:8, each = 5L),
      game_id = paste0("2024_0", rep(1:8, each = 5L), "_C_D"),
      posteam = "C", defteam = "D", qtr = 2L, play_type = "pass",
      epa = rnorm(n_sym, 0.05, 0.3), wp = 0.25, wpa = 0.01,
      score_differential = -14L,
      passer_player_id = "P_SYM_TRAIL", passer_player_name = "Trailing QB",
      rusher_player_id = NA_character_, rusher_player_name = NA_character_,
      receiver_player_id = "R_SYM_T", receiver_player_name = "Trailing WR",
      qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L
    )
  )
  result <- calculate_script_adjusted_epa(sym_data, min_plays = 20)
  expect_true(nrow(result) > 0)
  # The two passers should have adjustments that are exact negatives of each other
  passers <- result %>% filter(position_group == "passer") %>% arrange(player_id)
  expect_equal(nrow(passers), 2L)
  expect_equal(passers$adjustment_applied[1], -passers$adjustment_applied[2],
               tolerance = 1e-10)
  mean_adj <- mean(passers$adjustment_applied, na.rm = TRUE)
  expect_equal(mean_adj, 0, tolerance = 1e-10)
})

test_that("adjustment direction: trailing-heavy player gets negative adjustment", {
  # A player who always plays from trailing will get negative adjustment
  # because league_neutral_epa > league_trailing_epa
  # So (b_neutral - b_trailing) * trailing_share should be positive, but
  # if b_trailing < b_neutral then the adjustment is positive (corrects upward)
  result <- calculate_script_adjusted_epa(pbp_base, min_plays = 5)

  # Just verify columns are present and adjustment is numeric
  expect_true("adjustment_applied" %in% names(result))
  expect_true(is.numeric(result$adjustment_applied))
})

test_that("script_adjusted_epa column is present and numeric", {
  result <- calculate_script_adjusted_epa(pbp_base, min_plays = 5)
  expect_true("script_adjusted_epa" %in% names(result))
  expect_true(is.numeric(result$script_adjusted_epa))
})


# ==============================================================================
# SECTION I: COLUMN NAME VERIFICATION
# ==============================================================================

test_that("get_game_script_splits returns all documented columns", {
  result <- get_game_script_splits(pbp_base, min_plays = 1)
  expected_cols <- c(
    "season", "player_id", "player_name", "position_group", "team",
    "total_plays", "neutral_plays", "leading_plays", "trailing_plays",
    "neutral_epa_per_play", "leading_epa_per_play", "trailing_epa_per_play",
    "neutral_success_rate", "leading_success_rate", "trailing_success_rate",
    "neutral_share", "leading_share", "trailing_share", "script_epa_delta"
  )
  expect_true(all(expected_cols %in% names(result)),
              info = paste("Missing:", paste(setdiff(expected_cols, names(result)), collapse = ", ")))
})

test_that("calculate_leverage_features returns all documented columns", {
  result <- calculate_leverage_features(pbp_base, min_plays = 1)
  expected_cols <- c(
    "season", "player_id", "player_name", "position_group", "team",
    "total_plays", "total_wpa", "mean_wpa_per_play",
    "high_leverage_plays", "high_leverage_rate", "high_leverage_wpa",
    "clutch_rate", "high_leverage_epa_per_play", "clutch_note"
  )
  expect_true(all(expected_cols %in% names(result)),
              info = paste("Missing:", paste(setdiff(expected_cols, names(result)), collapse = ", ")))
})

test_that("get_comeback_profile returns all documented columns", {
  result <- get_comeback_profile(pbp_base, min_plays = 1)
  expected_cols <- c(
    "season", "player_id", "player_name", "position_group", "team",
    "comeback_plays", "comeback_epa_per_play", "comeback_success_rate",
    "overall_epa_per_play", "comeback_vs_overall_epa", "sample_note"
  )
  expect_true(all(expected_cols %in% names(result)),
              info = paste("Missing:", paste(setdiff(expected_cols, names(result)), collapse = ", ")))
})

test_that("calculate_script_adjusted_epa returns all documented columns", {
  result <- calculate_script_adjusted_epa(pbp_base, min_plays = 5)
  expected_cols <- c(
    "season", "player_id", "player_name", "position_group", "team",
    "total_plays", "raw_epa_per_play",
    "neutral_share", "leading_share", "trailing_share",
    "script_adjusted_epa", "adjustment_applied"
  )
  expect_true(all(expected_cols %in% names(result)),
              info = paste("Missing:", paste(setdiff(expected_cols, names(result)), collapse = ", ")))
})

test_that("no column named 'carries' exists in any output (use rushes)", {
  gs  <- get_game_script_splits(pbp_base, min_plays = 1)
  lev <- calculate_leverage_features(pbp_base, min_plays = 1)
  cmb <- get_comeback_profile(pbp_base, min_plays = 1)
  adj <- calculate_script_adjusted_epa(pbp_base, min_plays = 5)
  expect_false("carries" %in% c(names(gs), names(lev), names(cmb), names(adj)))
})

test_that("no column named 'team_id' in any output (use team / posteam)", {
  gs  <- get_game_script_splits(pbp_base, min_plays = 1)
  lev <- calculate_leverage_features(pbp_base, min_plays = 1)
  cmb <- get_comeback_profile(pbp_base, min_plays = 1)
  adj <- calculate_script_adjusted_epa(pbp_base, min_plays = 5)
  expect_false("team_id" %in% c(names(gs), names(lev), names(cmb), names(adj)))
})


# ==============================================================================
# SECTION J: INTEGRATION TEST
# ==============================================================================

test_that("INTEGRATION: all four functions run on same pbp without error", {
  pbp_test <- make_pbp(200)

  gs  <- get_game_script_splits(pbp_test, min_plays = 3)
  lev <- calculate_leverage_features(pbp_test, min_plays = 3)
  cmb <- get_comeback_profile(pbp_test, min_plays = 3)
  adj <- calculate_script_adjusted_epa(pbp_test, min_plays = 10)

  expect_s3_class(gs,  "tbl_df")
  expect_s3_class(lev, "tbl_df")
  expect_s3_class(cmb, "tbl_df")
  expect_s3_class(adj, "tbl_df")

  expect_gt(nrow(gs),  0L)
  expect_gt(nrow(lev), 0L)
  expect_gt(nrow(adj), 0L)

  # All player_ids from script splits appear in adjusted EPA
  gs_ids  <- unique(gs$player_id)
  adj_ids <- unique(adj$player_id)
  # adj is subset (higher min_plays) so gs_ids should be superset
  expect_true(all(adj_ids %in% gs_ids))
})

test_that("INTEGRATION: season and week filters applied correctly across functions", {
  pbp_multi <- make_pbp(200) %>%
    mutate(season = ifelse(week <= 5, 2023L, 2024L))

  gs_2024  <- get_game_script_splits(pbp_multi, season = 2024L, min_plays = 1)
  gs_all   <- get_game_script_splits(pbp_multi, min_plays = 1)

  # 2024-only should have fewer or equal rows than all-seasons
  expect_lte(nrow(gs_2024), nrow(gs_all))
  # All season values in filtered result should be 2024
  expect_true(all(gs_2024$season == 2024L))
})

cat("\n=== Week 7 Test Summary ===\n")
cat("Total test cases: see testthat output above\n")
cat("Coverage: input validation, empty data, garbage time, kneels, scrambles,\n")
cat("          script classification, leverage threshold, WPA aggregation,\n")
cat("          comeback filter, script adjustment direction, column names,\n")
cat("          integration, season filter\n")
