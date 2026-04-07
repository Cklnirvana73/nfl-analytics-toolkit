# =============================================================================
# tests/test_season2_week2_functions.R
# =============================================================================
# Season 2, Week 2: Player-Season Panel -- Full Test Suite
# NFL Analytics Toolkit
# =============================================================================
#
# RUNNING THESE TESTS
# -------------------
# From the project root:
#   testthat::test_file("tests/test_season2_week2_functions.R")
#
# SCOPE
# -----
# Tests are organized into 7 contexts:
#   1. Configuration constants
#   2. .mode_chr() internal helper
#   3. classify_player_position() -- input validation (no network calls)
#   4. Internal stat helpers on mock PBP data
#   5. build_player_season_panel() -- input validation (no network calls)
#   6. validate_panel_integrity() -- all six checks
#   7. Integration: full pipeline on cached data (skipped if cache absent)
#
# MOCK DATA STRATEGY
# ------------------
# nflreadr::load_rosters() and load_normalized_season() require network
# access and a populated cache. All tests that touch those paths are wrapped
# in skip_if_not() checking for the 2024 cache file. All other tests operate
# on synthetic mock data built inline and require no external dependencies.
#
# =============================================================================

library(testthat)
library(dplyr)
library(here)
library(glue)

source(here::here("R", "16_player_season_panel.R"))


# =============================================================================
# MOCK DATA BUILDERS
# =============================================================================
# Self-contained helpers used across multiple test contexts.
# Build minimal valid PBP and panel tibbles without touching the cache.

.make_mock_pbp <- function(n_passers       = 3L,
                            n_rushers       = 3L,
                            n_receivers     = 3L,
                            include_kneels  = FALSE,
                            include_spikes  = FALSE,
                            include_2pt     = FALSE,
                            include_scramble = TRUE) {

  set.seed(42L)

  pass_plays <- tibble::tibble(
    play_id              = seq_len(n_passers * 50L),
    game_id              = rep(paste0("2024_01_NE_BUF_", seq_len(n_passers)), each = 50L),
    season               = 2024L,
    week                 = 1L,
    season_type          = "REG",
    play_type            = "pass",
    passer_player_id     = rep(paste0("QB_", seq_len(n_passers)), each = 50L),
    passer_player_name   = rep(paste0("QB Player ", seq_len(n_passers)), each = 50L),
    rusher_player_id     = NA_character_,
    rusher_player_name   = NA_character_,
    receiver_player_id   = rep(paste0("WR_", seq_len(n_receivers)),
                               length.out = n_passers * 50L),
    receiver_player_name = rep(paste0("WR Player ", seq_len(n_receivers)),
                               length.out = n_passers * 50L),
    pass_attempt         = 1L,
    rush_attempt         = 0L,
    complete_pass        = rep(c(1L, 1L, 0L, 1L, 1L), n_passers * 10L),
    sack                 = 0L,
    qb_dropback          = 1L,
    qb_scramble          = 0L,
    qb_spike             = 0L,
    qb_kneel             = 0L,
    two_point_attempt    = 0L,
    passing_yards        = rep(c(8L, 15L, 0L, 22L, 6L), n_passers * 10L),
    rushing_yards        = 0L,
    receiving_yards      = rep(c(8L, 15L, 0L, 22L, 6L), n_passers * 10L),
    air_yards            = rep(c(5L, 10L, 8L, 18L, 4L), n_passers * 10L),
    yards_after_catch    = rep(c(3L, 5L, 0L, 4L, 2L), n_passers * 10L),
    epa                  = rep(c(0.3, 0.8, -0.5, 1.2, 0.1), n_passers * 10L),
    success              = rep(c(1L, 1L, 0L, 1L, 1L), n_passers * 10L),
    wp                   = 0.5,
    down                 = 1L,
    ydstogo              = 10L,
    pass_touchdown       = 0L,
    rush_touchdown       = 0L,
    interception         = 0L,
    fumble               = 0L,
    penalty              = 0L,
    # cpoe is an OPTIONAL_COLUMN in the normalized schema (available 2016+).
    # Must exist in mock PBP so .build_passing_stats() does not error on
    # the 'cpoe' %in% names(pbp) guard. Set to NA to simulate pre-2016 seasons.
    cpoe                 = NA_real_
  )

  rush_plays <- tibble::tibble(
    play_id              = seq_len(n_rushers * 30L) + nrow(pass_plays),
    game_id              = rep(paste0("2024_01_NE_BUF_", seq_len(n_rushers)), each = 30L),
    season               = 2024L,
    week                 = 1L,
    season_type          = "REG",
    play_type            = "run",
    passer_player_id     = NA_character_,
    passer_player_name   = NA_character_,
    rusher_player_id     = rep(paste0("RB_", seq_len(n_rushers)), each = 30L),
    rusher_player_name   = rep(paste0("RB Player ", seq_len(n_rushers)), each = 30L),
    receiver_player_id   = NA_character_,
    receiver_player_name = NA_character_,
    pass_attempt         = 0L,
    rush_attempt         = 1L,
    complete_pass        = 0L,
    sack                 = 0L,
    qb_dropback          = 0L,
    qb_scramble          = 0L,
    qb_spike             = 0L,
    qb_kneel             = 0L,
    two_point_attempt    = 0L,
    passing_yards        = 0L,
    rushing_yards        = rep(c(4L, 7L, 2L, -1L, 6L), n_rushers * 6L),
    receiving_yards      = 0L,
    air_yards            = NA_real_,
    yards_after_catch    = NA_real_,
    epa                  = rep(c(0.1, 0.4, -0.2, -0.4, 0.2), n_rushers * 6L),
    success              = rep(c(1L, 1L, 0L, 0L, 1L), n_rushers * 6L),
    wp                   = 0.5,
    down                 = 1L,
    ydstogo              = 10L,
    pass_touchdown       = 0L,
    rush_touchdown       = 0L,
    interception         = 0L,
    fumble               = 0L,
    penalty              = 0L
  )

  pbp      <- dplyr::bind_rows(pass_plays, rush_plays)
  next_id  <- nrow(pbp) + 1L

  if (include_kneels) {
    pbp <- dplyr::bind_rows(pbp, tibble::tibble(
      play_id = next_id, game_id = "2024_01_NE_BUF_1", season = 2024L,
      week = 1L, season_type = "REG", play_type = "run",
      passer_player_id = NA_character_, passer_player_name = NA_character_,
      rusher_player_id = "QB_1", rusher_player_name = "QB Player 1",
      receiver_player_id = NA_character_, receiver_player_name = NA_character_,
      pass_attempt = 0L, rush_attempt = 1L, complete_pass = 0L,
      sack = 0L, qb_dropback = 0L, qb_scramble = 0L,
      qb_spike = 0L, qb_kneel = 1L, two_point_attempt = 0L,
      passing_yards = 0L, rushing_yards = -1L, receiving_yards = 0L,
      air_yards = NA_real_, yards_after_catch = NA_real_,
      epa = -0.5, success = 0L, wp = 0.95, down = 4L, ydstogo = 1L,
      pass_touchdown = 0L, rush_touchdown = 0L, interception = 0L,
      fumble = 0L, penalty = 0L
    ))
    next_id <- next_id + 1L
  }

  if (include_spikes) {
    pbp <- dplyr::bind_rows(pbp, tibble::tibble(
      play_id = next_id, game_id = "2024_01_NE_BUF_1", season = 2024L,
      week = 1L, season_type = "REG", play_type = "pass",
      passer_player_id = "QB_1", passer_player_name = "QB Player 1",
      rusher_player_id = NA_character_, rusher_player_name = NA_character_,
      receiver_player_id = NA_character_, receiver_player_name = NA_character_,
      pass_attempt = 1L, rush_attempt = 0L, complete_pass = 0L,
      sack = 0L, qb_dropback = 1L, qb_scramble = 0L,
      qb_spike = 1L, qb_kneel = 0L, two_point_attempt = 0L,
      passing_yards = 0L, rushing_yards = 0L, receiving_yards = 0L,
      air_yards = 0L, yards_after_catch = 0L,
      epa = -0.1, success = 0L, wp = 0.5, down = 2L, ydstogo = 10L,
      pass_touchdown = 0L, rush_touchdown = 0L, interception = 0L,
      fumble = 0L, penalty = 0L
    ))
    next_id <- next_id + 1L
  }

  if (include_2pt) {
    pbp <- dplyr::bind_rows(pbp, tibble::tibble(
      play_id = next_id, game_id = "2024_01_NE_BUF_1", season = 2024L,
      week = 1L, season_type = "REG", play_type = "pass",
      passer_player_id = "QB_1", passer_player_name = "QB Player 1",
      rusher_player_id = NA_character_, rusher_player_name = NA_character_,
      receiver_player_id = "WR_1", receiver_player_name = "WR Player 1",
      pass_attempt = 1L, rush_attempt = 0L, complete_pass = 1L,
      sack = 0L, qb_dropback = 1L, qb_scramble = 0L,
      qb_spike = 0L, qb_kneel = 0L, two_point_attempt = 1L,
      passing_yards = 2L, rushing_yards = 0L, receiving_yards = 2L,
      air_yards = 2L, yards_after_catch = 0L,
      epa = 1.0, success = 1L, wp = 0.5, down = 1L, ydstogo = 2L,
      pass_touchdown = 0L, rush_touchdown = 0L, interception = 0L,
      fumble = 0L, penalty = 0L
    ))
    next_id <- next_id + 1L
  }

  if (include_scramble) {
    pbp <- dplyr::bind_rows(pbp, tibble::tibble(
      play_id = next_id, game_id = "2024_01_NE_BUF_1", season = 2024L,
      week = 1L, season_type = "REG", play_type = "run",
      passer_player_id = "QB_1", passer_player_name = "QB Player 1",
      rusher_player_id = "QB_1", rusher_player_name = "QB Player 1",
      receiver_player_id = NA_character_, receiver_player_name = NA_character_,
      pass_attempt = 0L, rush_attempt = 0L, complete_pass = 0L,
      sack = 0L, qb_dropback = 1L, qb_scramble = 1L,
      qb_spike = 0L, qb_kneel = 0L, two_point_attempt = 0L,
      passing_yards = 0L, rushing_yards = 12L, receiving_yards = 0L,
      air_yards = NA_real_, yards_after_catch = NA_real_,
      epa = 0.6, success = 1L, wp = 0.5, down = 3L, ydstogo = 8L,
      pass_touchdown = 0L, rush_touchdown = 0L, interception = 0L,
      fumble = 0L, penalty = 0L
    ))
  }

  return(pbp)
}


.make_mock_panel <- function(n_players = 10L, seasons = 2024L) {
  set.seed(99L)
  tidyr::crossing(
    player_id = paste0("P_", seq_len(n_players)),
    season    = seasons
  ) %>%
    dplyr::mutate(
      player_name          = paste0("Player ", player_id),
      team                 = sample(c("BAL", "BUF", "SF", "KC"), n(), replace = TRUE),
      position             = sample(c("QB", "RB", "WR", "TE"), n(), replace = TRUE),
      depth_chart_position = position,
      position_group       = dplyr::recode(
        position, "QB" = "QB", "RB" = "RB", "WR" = "WR_TE", "TE" = "WR_TE"
      ),
      has_name_collision   = FALSE,
      games_played         = sample(10L:17L, n(), replace = TRUE),
      total_plays          = sample(20L:150L, n(), replace = TRUE),
      low_volume           = total_plays < 10L,
      attempts             = sample(0L:400L, n(), replace = TRUE),
      completions          = as.integer(attempts * runif(n(), 0.55, 0.72)),
      passing_yards        = as.integer(completions * runif(n(), 7, 12)),
      pass_tds             = sample(0L:35L, n(), replace = TRUE),
      interceptions_thrown = sample(0L:15L, n(), replace = TRUE),
      sacks_taken          = sample(0L:45L, n(), replace = TRUE),
      qb_dropbacks         = attempts + sacks_taken + sample(0L:30L, n(), replace = TRUE),
      pass_epa             = runif(n(), -30, 80),
      pass_epa_per_dropback = pass_epa / pmax(qb_dropbacks, 1L),
      pass_success_rate    = runif(n(), 0.35, 0.60),
      mean_cpoe            = runif(n(), -5, 8),
      rush_attempts        = sample(0L:300L, n(), replace = TRUE),
      scrambles            = sample(0L:50L, n(), replace = TRUE),
      rushing_yards        = as.integer(rush_attempts * runif(n(), 3.5, 5.5)),
      rush_tds             = sample(0L:18L, n(), replace = TRUE),
      rush_epa             = runif(n(), -20, 40),
      rush_epa_per_attempt = rush_epa / pmax(rush_attempts, 1L),
      rush_success_rate    = runif(n(), 0.35, 0.55),
      yards_per_carry      = rushing_yards / pmax(rush_attempts, 1L),
      targets              = sample(0L:180L, n(), replace = TRUE),
      receptions           = as.integer(targets * runif(n(), 0.60, 0.80)),
      receiving_yards      = as.integer(receptions * runif(n(), 7, 14)),
      rec_tds              = sample(0L:15L, n(), replace = TRUE),
      rec_epa              = runif(n(), -10, 50),
      rec_epa_per_target   = rec_epa / pmax(targets, 1L),
      rec_success_rate     = runif(n(), 0.40, 0.60),
      catch_rate           = receptions / pmax(targets, 1L),
      rec_air_yards        = as.integer(targets * runif(n(), 4, 12)),
      rec_yac              = as.integer(receptions * runif(n(), 2, 6)),
      total_tds            = pass_tds + rush_tds + rec_tds,
      total_yards          = passing_yards + rushing_yards + receiving_yards,
      total_epa            = pass_epa + rush_epa + rec_epa,
      panel_version        = "s2panel_v1"
    )
}


# =============================================================================
# Context 1: Configuration Constants
# =============================================================================

test_that("PANEL_SEASONS_DEFAULT covers 2010 through 2025", {
  expect_equal(min(PANEL_SEASONS_DEFAULT), 2010L)
  expect_equal(max(PANEL_SEASONS_DEFAULT), 2025L)
  expect_equal(length(PANEL_SEASONS_DEFAULT), 16L)
})

test_that("PANEL_MIN_PLAYS is a positive integer", {
  expect_true(is.numeric(PANEL_MIN_PLAYS))
  expect_gt(PANEL_MIN_PLAYS, 0L)
})

test_that("PANEL_VERSION is the expected schema tag", {
  expect_equal(PANEL_VERSION, "s2panel_v1")
})

test_that("POSITION_GROUP_MAP routes skill positions correctly", {
  expect_equal(POSITION_GROUP_MAP[["QB"]], "QB")
  expect_equal(POSITION_GROUP_MAP[["RB"]], "RB")
  expect_equal(POSITION_GROUP_MAP[["FB"]], "RB")
  expect_equal(POSITION_GROUP_MAP[["HB"]], "RB")
  expect_equal(POSITION_GROUP_MAP[["WR"]], "WR_TE")
  expect_equal(POSITION_GROUP_MAP[["TE"]], "WR_TE")
})

test_that(".max_games_for_season returns 16 for pre-2021 and 17 for 2021+", {
  expect_equal(.max_games_for_season(2020L), 16L)
  expect_equal(.max_games_for_season(2021L), 17L)
  expect_equal(.max_games_for_season(2025L), 17L)
  expect_equal(.max_games_for_season(2010L), 16L)
})


# =============================================================================
# Context 2: .mode_chr() Internal Helper
# =============================================================================

test_that(".mode_chr returns the most frequent non-NA value", {
  expect_equal(.mode_chr(c("QB", "QB", "RB")), "QB")
  expect_equal(.mode_chr(c("WR", "TE", "WR", "WR")), "WR")
})

test_that(".mode_chr returns NA_character_ on empty input", {
  expect_equal(.mode_chr(character(0L)), NA_character_)
})

test_that(".mode_chr returns NA_character_ on all-NA input", {
  expect_equal(.mode_chr(c(NA_character_, NA_character_)), NA_character_)
})

test_that(".mode_chr ignores NAs when non-NA values present", {
  expect_equal(.mode_chr(c(NA_character_, "QB", "QB", NA_character_)), "QB")
})

test_that(".mode_chr handles a single value", {
  expect_equal(.mode_chr("RB"), "RB")
})


# =============================================================================
# Context 3: classify_player_position() -- Input Validation
# =============================================================================

test_that("classify_player_position() rejects empty seasons vector", {
  expect_error(
    classify_player_position(seasons = integer(0L)),
    regexp = "non-empty"
  )
})

test_that("classify_player_position() rejects non-numeric seasons", {
  expect_error(
    classify_player_position(seasons = "2024"),
    regexp = "non-empty numeric"
  )
})

test_that("classify_player_position() rejects empty player_ids vector", {
  expect_error(
    classify_player_position(player_ids = character(0L), seasons = 2024L),
    regexp = "non-empty character"
  )
})

test_that("classify_player_position() rejects non-character player_ids", {
  expect_error(
    classify_player_position(player_ids = 12345L, seasons = 2024L),
    regexp = "non-empty character"
  )
})


# =============================================================================
# Context 4: Internal Stat Helpers on Mock PBP
# =============================================================================

test_that(".build_passing_stats returns one row per passer", {
  pbp    <- .make_mock_pbp(n_passers = 3L, include_scramble = FALSE)
  result <- .build_passing_stats(pbp)

  expect_equal(nrow(result), 3L)
  expect_true("player_id" %in% names(result))
  expect_true(all(grepl("^QB_", result$player_id)))
})

test_that(".build_passing_stats excludes qb_spike plays from attempts", {
  pbp_clean <- .make_mock_pbp(include_spikes = FALSE, include_scramble = FALSE)
  pbp_spike <- .make_mock_pbp(include_spikes = TRUE,  include_scramble = FALSE)

  att_clean <- .build_passing_stats(pbp_clean) %>%
    filter(player_id == "QB_1") %>% pull(attempts)
  att_spike <- .build_passing_stats(pbp_spike) %>%
    filter(player_id == "QB_1") %>% pull(attempts)

  # Spike row has pass_attempt = 1 but must be excluded -- counts must match
  expect_equal(att_clean, att_spike)
})

test_that(".build_passing_stats excludes two_point_attempt plays", {
  pbp_clean <- .make_mock_pbp(include_2pt = FALSE, include_scramble = FALSE)
  pbp_2pt   <- .make_mock_pbp(include_2pt = TRUE,  include_scramble = FALSE)

  yards_clean <- .build_passing_stats(pbp_clean) %>%
    filter(player_id == "QB_1") %>% pull(passing_yards)
  yards_2pt   <- .build_passing_stats(pbp_2pt) %>%
    filter(player_id == "QB_1") %>% pull(passing_yards)

  expect_equal(yards_clean, yards_2pt)
})

test_that(".build_passing_stats qb_dropbacks excludes scramble rows", {
  # .build_passing_stats() filters to pass_attempt == 1 | sack == 1 before
  # summarising. Scramble rows have pass_attempt = 0 and sack = 0 so they
  # are excluded entirely. qb_dropbacks counts only pass attempts and sacks.
  # The scramble's qb_dropback = 1 flag never reaches the sum.
  pbp_clean  <- .make_mock_pbp(include_scramble = FALSE)
  pbp_scram  <- .make_mock_pbp(include_scramble = TRUE)

  db_clean <- .build_passing_stats(pbp_clean) %>%
    filter(player_id == "QB_1") %>% pull(qb_dropbacks)
  db_scram <- .build_passing_stats(pbp_scram) %>%
    filter(player_id == "QB_1") %>% pull(qb_dropbacks)

  expect_equal(db_scram, db_clean)
})

test_that(".build_passing_stats pass_epa does NOT include scramble EPA", {
  pbp_clean <- .make_mock_pbp(include_scramble = FALSE)
  pbp_scram <- .make_mock_pbp(include_scramble = TRUE)

  epa_clean <- .build_passing_stats(pbp_clean) %>%
    filter(player_id == "QB_1") %>% pull(pass_epa)
  epa_scram <- .build_passing_stats(pbp_scram) %>%
    filter(player_id == "QB_1") %>% pull(pass_epa)

  # Scramble EPA must go to rush_epa only -- pass_epa must be identical
  expect_equal(epa_clean, epa_scram)
})

test_that(".build_rushing_stats returns one row per rusher", {
  pbp    <- .make_mock_pbp(n_rushers = 3L, include_scramble = FALSE)
  result <- .build_rushing_stats(pbp)

  expect_equal(nrow(result), 3L)
  expect_true(all(grepl("^RB_", result$player_id)))
})

test_that(".build_rushing_stats excludes qb_kneel plays", {
  pbp_kneel  <- .make_mock_pbp(include_kneels = TRUE, include_scramble = FALSE)
  result     <- .build_rushing_stats(pbp_kneel)

  # Kneel is attributed to QB_1 as rusher -- after exclusion QB_1 absent
  expect_false("QB_1" %in% result$player_id)
})

test_that(".build_rushing_stats scramble yards appear in rushing_yards", {
  pbp_clean <- .make_mock_pbp(n_rushers = 1L, include_scramble = FALSE)
  pbp_scram <- .make_mock_pbp(n_rushers = 1L, include_scramble = TRUE)

  # Without scramble: QB_1 has no designed rush plays, so absent from result
  result_clean <- .build_rushing_stats(pbp_clean)
  result_scram <- .build_rushing_stats(pbp_scram)

  # With scramble: QB_1 must appear with positive rushing yards
  expect_false("QB_1" %in% result_clean$player_id)
  expect_true("QB_1"  %in% result_scram$player_id)

  yards_scram <- result_scram %>% filter(player_id == "QB_1") %>% pull(rushing_yards)
  expect_equal(length(yards_scram), 1L)
  expect_gt(yards_scram, 0)
})

test_that(".build_rushing_stats rush_epa_per_attempt is not Inf or NaN", {
  pbp    <- .make_mock_pbp(n_rushers = 1L, include_scramble = TRUE)
  result <- .build_rushing_stats(pbp) %>% filter(player_id == "RB_1")

  expect_false(is.na(result$rush_epa_per_attempt))
  expect_false(is.infinite(result$rush_epa_per_attempt))
})

test_that(".build_receiving_stats returns one row per receiver", {
  pbp    <- .make_mock_pbp(n_receivers = 3L)
  result <- .build_receiving_stats(pbp)

  expect_equal(nrow(result), 3L)
  expect_true(all(grepl("^WR_", result$player_id)))
})

test_that(".build_receiving_stats excludes two_point_attempt targets", {
  pbp_clean <- .make_mock_pbp(include_2pt = FALSE)
  pbp_2pt   <- .make_mock_pbp(include_2pt = TRUE)

  tgt_clean <- .build_receiving_stats(pbp_clean) %>%
    filter(player_id == "WR_1") %>% pull(targets)
  tgt_2pt   <- .build_receiving_stats(pbp_2pt) %>%
    filter(player_id == "WR_1") %>% pull(targets)

  expect_equal(tgt_clean, tgt_2pt)
})

test_that(".build_receiving_stats catch_rate is between 0 and 1", {
  pbp    <- .make_mock_pbp()
  result <- .build_receiving_stats(pbp)

  expect_true(all(result$catch_rate >= 0 & result$catch_rate <= 1, na.rm = TRUE))
})

test_that(".build_games_played counts distinct games per player", {
  pbp    <- .make_mock_pbp(n_passers = 2L, n_rushers = 2L, n_receivers = 2L)
  result <- .build_games_played(pbp)

  expect_true("player_id"    %in% names(result))
  expect_true("games_played" %in% names(result))
  expect_true(all(result$games_played >= 1L))
})


# =============================================================================
# Context 5: build_player_season_panel() -- Input Validation
# =============================================================================

test_that("build_player_season_panel() rejects empty seasons vector", {
  expect_error(
    build_player_season_panel(seasons = integer(0L)),
    regexp = "non-empty"
  )
})

test_that("build_player_season_panel() rejects non-numeric seasons", {
  expect_error(
    build_player_season_panel(seasons = "2024"),
    regexp = "non-empty numeric"
  )
})

test_that("build_player_season_panel() rejects negative min_plays", {
  expect_error(
    build_player_season_panel(seasons = 2024L, min_plays = -1L),
    regexp = "non-negative"
  )
})

test_that("build_player_season_panel() stops when cache_dir is missing", {
  expect_error(
    build_player_season_panel(
      seasons   = 2024L,
      cache_dir = "/nonexistent/path/to/cache"
    ),
    regexp = "cache_dir not found"
  )
})


# =============================================================================
# Context 6: validate_panel_integrity() -- All Six Checks
# =============================================================================

test_that("validate_panel_integrity() rejects empty data frame", {
  expect_error(
    validate_panel_integrity(data.frame()),
    regexp = "non-empty"
  )
})

test_that("validate_panel_integrity() rejects missing required columns", {
  bad_panel <- tibble::tibble(player_id = "P1", season = 2024L)
  expect_error(
    validate_panel_integrity(bad_panel),
    regexp = "missing required columns"
  )
})

test_that("validate_panel_integrity() returns valid=TRUE on clean panel", {
  panel  <- .make_mock_panel(n_players = 5L, seasons = 2024L)
  result <- validate_panel_integrity(panel, verbose = FALSE)
  expect_true(result$valid)
})

test_that("validate_panel_integrity() detects duplicate player-season rows", {
  panel       <- .make_mock_panel(n_players = 3L, seasons = 2024L)
  panel_duped <- dplyr::bind_rows(panel, panel[1L, ])

  result <- validate_panel_integrity(panel_duped, verbose = FALSE)
  expect_false(result$valid)
  expect_gt(nrow(result$duplicate_rows), 0L)
})

test_that("validate_panel_integrity() detects implausible negative yardage", {
  panel                   <- .make_mock_panel(n_players = 3L, seasons = 2024L)
  panel$rushing_yards[1L] <- -100L

  result <- validate_panel_integrity(panel, verbose = FALSE)
  expect_false(result$valid)
  expect_gt(nrow(result$negative_stats), 0L)
})

test_that("validate_panel_integrity() does NOT flag small negative yardage", {
  # -6 rushing yards (a jet sweep loss) is a legitimate season total
  panel                    <- .make_mock_panel(n_players = 3L, seasons = 2024L)
  panel$rushing_yards[1L]  <- -6L
  panel$receiving_yards[2L] <- -3L

  result <- validate_panel_integrity(panel, verbose = FALSE)
  expect_equal(nrow(result$negative_stats), 0L)
})

test_that("validate_panel_integrity() detects games_played above 2024 era cap", {
  panel                   <- .make_mock_panel(n_players = 3L, seasons = 2024L)
  panel$games_played[1L]  <- 18L  # Above 17-game cap for 2024

  result <- validate_panel_integrity(panel, verbose = FALSE)
  expect_false(result$valid)
  expect_gt(nrow(result$games_exceeded), 0L)
})

test_that("validate_panel_integrity() applies 16-game cap for pre-2021 seasons", {
  panel                  <- .make_mock_panel(n_players = 3L, seasons = 2020L)
  panel$games_played[1L] <- 17L  # Exceeds 16-game cap for 2020

  result <- validate_panel_integrity(panel, verbose = FALSE)
  expect_false(result$valid)
  expect_gt(nrow(result$games_exceeded), 0L)
})

test_that("validate_panel_integrity() flags unmatched players as informational only", {
  panel                <- .make_mock_panel(n_players = 3L, seasons = 2024L)
  panel$position[1L]   <- NA_character_

  result      <- validate_panel_integrity(panel, verbose = FALSE)
  check_row   <- result$summary %>%
    filter(check == "All players matched to nflreadr roster")

  expect_gt(nrow(result$unmatched_players), 0L)
  expect_false(check_row$critical)
})

test_that("validate_panel_integrity() detects position changes as informational", {
  panel <- .make_mock_panel(n_players = 2L, seasons = 2022L:2024L)
  panel$position[panel$player_id == "P_1" & panel$season == 2022L] <- "WR"
  panel$position[panel$player_id == "P_1" & panel$season == 2023L] <- "TE"
  panel$position[panel$player_id == "P_1" & panel$season == 2024L] <- "WR"

  result    <- validate_panel_integrity(panel, verbose = FALSE)
  check_row <- result$summary %>%
    filter(check == "Position changes flagged (informational)")

  expect_gt(nrow(result$position_changes), 0L)
  expect_false(check_row$critical)
})

test_that("validate_panel_integrity() detects name collisions as informational", {
  panel <- .make_mock_panel(n_players = 4L, seasons = 2024L)
  panel$player_name[panel$player_id == "P_1"] <- "Shared Name"
  panel$player_name[panel$player_id == "P_2"] <- "Shared Name"
  panel$has_name_collision[panel$player_name == "Shared Name"] <- TRUE

  result    <- validate_panel_integrity(panel, verbose = FALSE)
  check_row <- result$summary %>%
    filter(check == "Name collisions flagged (informational)")

  expect_gt(nrow(result$name_collisions), 0L)
  expect_false(check_row$critical)
})

test_that("validate_panel_integrity() summary has exactly six checks", {
  panel  <- .make_mock_panel()
  result <- validate_panel_integrity(panel, verbose = FALSE)
  expect_equal(nrow(result$summary), 6L)
})

test_that("validate_panel_integrity() returns all expected list elements", {
  panel  <- .make_mock_panel()
  result <- validate_panel_integrity(panel, verbose = FALSE)

  expected_names <- c("valid", "summary", "duplicate_rows", "negative_stats",
                      "games_exceeded", "unmatched_players",
                      "position_changes", "name_collisions")
  expect_true(all(expected_names %in% names(result)))
})

test_that("validate_panel_integrity() valid is a scalar logical", {
  panel  <- .make_mock_panel()
  result <- validate_panel_integrity(panel, verbose = FALSE)
  expect_true(is.logical(result$valid))
  expect_equal(length(result$valid), 1L)
})


# =============================================================================
# Context 7: Integration -- Full Pipeline on Cached Data
# =============================================================================

.cache_2024 <- here::here("data", "season2_cache", "pbp_normalized_2024.rds")

test_that("classify_player_position() returns expected columns for 2024", {
  skip_if_not(
    file.exists(.cache_2024),
    message = "season2_cache not populated -- skipping cache-dependent test"
  )

  result        <- classify_player_position(seasons = 2024L, verbose = FALSE)
  expected_cols <- c("player_id", "season", "player_name", "team",
                     "position", "depth_chart_position",
                     "position_group", "has_name_collision")

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(unique(result$season), 2024L)
  expect_true(is.logical(result$has_name_collision))
})

test_that("classify_player_position() has_name_collision is never NA", {
  skip_if_not(file.exists(.cache_2024),
    message = "season2_cache not populated -- skipping cache-dependent test")

  result <- classify_player_position(seasons = 2024L, verbose = FALSE)
  expect_false(any(is.na(result$has_name_collision)))
})

test_that("classify_player_position() QB position_group is always QB", {
  skip_if_not(file.exists(.cache_2024),
    message = "season2_cache not populated -- skipping cache-dependent test")

  result <- classify_player_position(seasons = 2024L, verbose = FALSE)
  qbs    <- result %>% filter(position == "QB")
  expect_true(all(qbs$position_group == "QB"))
})

test_that("classify_player_position() TE position_group is always WR_TE", {
  skip_if_not(file.exists(.cache_2024),
    message = "season2_cache not populated -- skipping cache-dependent test")

  result <- classify_player_position(seasons = 2024L, verbose = FALSE)
  tes    <- result %>% filter(position == "TE")
  expect_true(all(tes$position_group == "WR_TE"))
})

test_that("build_player_season_panel() produces one row per player for 2024", {
  skip_if_not(file.exists(.cache_2024),
    message = "season2_cache not populated -- skipping cache-dependent test")

  panel <- build_player_season_panel(seasons = 2024L, verbose = FALSE)
  expect_equal(nrow(panel), dplyr::n_distinct(panel$player_id))
  expect_true(all(panel$season == 2024L))
})

test_that("build_player_season_panel() panel_version tag is correct", {
  skip_if_not(file.exists(.cache_2024),
    message = "season2_cache not populated -- skipping cache-dependent test")

  panel <- build_player_season_panel(seasons = 2024L, verbose = FALSE)
  expect_true(all(panel$panel_version == "s2panel_v1"))
})

test_that("build_player_season_panel() total_plays is non-negative for all rows", {
  skip_if_not(file.exists(.cache_2024),
    message = "season2_cache not populated -- skipping cache-dependent test")

  panel <- build_player_season_panel(seasons = 2024L, verbose = FALSE)
  expect_true(all(panel$total_plays >= 0L, na.rm = TRUE))
})

test_that("build_player_season_panel() low_volume is consistent with total_plays", {
  skip_if_not(file.exists(.cache_2024),
    message = "season2_cache not populated -- skipping cache-dependent test")

  panel <- build_player_season_panel(seasons = 2024L, min_plays = 10L,
                                     verbose = FALSE)
  expect_true(all(panel$low_volume[panel$total_plays < 10L]))
  expect_false(any(panel$low_volume[panel$total_plays >= 10L]))
})

test_that("build_player_season_panel() passes full integrity check on 2024", {
  skip_if_not(file.exists(.cache_2024),
    message = "season2_cache not populated -- skipping cache-dependent test")

  panel     <- build_player_season_panel(seasons = 2024L, verbose = FALSE)
  integrity <- validate_panel_integrity(panel, verbose = FALSE)
  expect_true(integrity$valid)
})

test_that("build_player_season_panel() Lamar Jackson (BAL) is QB with rushing yards", {
  skip_if_not(file.exists(.cache_2024),
    message = "season2_cache not populated -- skipping cache-dependent test")

  panel <- build_player_season_panel(seasons = 2024L, verbose = FALSE)
  lamar <- panel %>% filter(player_id == "00-0034796")

  expect_equal(nrow(lamar), 1L)
  expect_equal(lamar$position, "QB")
  expect_equal(lamar$position_group, "QB")
  # He must have rushing yards despite position = QB
  expect_gt(dplyr::coalesce(lamar$rushing_yards, 0), 0)
})

test_that("build_player_season_panel() has_name_collision is logical and present", {
  skip_if_not(file.exists(.cache_2024),
    message = "season2_cache not populated -- skipping cache-dependent test")

  panel <- build_player_season_panel(seasons = 2024L, verbose = FALSE)
  expect_true("has_name_collision" %in% names(panel))
  expect_true(is.logical(panel$has_name_collision))
  expect_false(any(is.na(panel$has_name_collision)))
})
