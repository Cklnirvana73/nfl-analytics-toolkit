# ==============================================================================
# tests/test_season2_week8_functions.R
# Test suite for R/22_sos_reconciliation.R
#
# Standards:
#   - Single test file per week (two-file standard retired after Week 1)
#   - Synthetic fixtures only -- no live cfbfastR downloads, no Sleeper API
#   - testthat::test_file() called directly, no root-level runner
#   - All fixtures defined inline via make_* helpers
#   - Assert non-empty results before testing downstream behavior
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)
library(glue)
library(here)

# Source R/22 (which sources R/19 and R/20 via its guards).
# R/19 and R/20 must exist at the expected paths.
source(here::here("R", "22_sos_reconciliation.R"))


# ==============================================================================
# SYNTHETIC FIXTURE FACTORIES
# ==============================================================================

#' Build a minimal synthetic CFB PBP tibble for testing
#' Returns a tibble that mimics the normalized R/20 cache output.
make_cfb_pbp <- function(season       = 2023L,
                          season_type  = "regular",
                          n_plays      = 100L) {

  teams_home <- c("Alabama", "Georgia", "Ohio State", "Michigan", "Texas")
  teams_away <- c("Auburn", "Tennessee", "Penn State", "Michigan State", "Oklahoma")

  set.seed(42L + season)

  tibble::tibble(
    game_id      = rep(paste0(season, "_", seq_len(10L)), each = n_plays %/% 10L),
    season       = season,
    season_type  = season_type,
    week         = rep(seq_len(10L), each = n_plays %/% 10L),
    period       = sample(1L:4L, n_plays, replace = TRUE),
    pos_team     = rep(teams_home, each = n_plays %/% 5L),
    def_pos_team = rep(teams_away, each = n_plays %/% 5L),
    posteam      = rep(teams_home, each = n_plays %/% 5L),
    defteam      = rep(teams_away, each = n_plays %/% 5L),
    play_type    = sample(
      c("Rush", "Pass Completion", "Pass Incompletion", "Sack",
        "Rushing Touchdown", "Passing Touchdown"),
      n_plays, replace = TRUE
    ),
    EPA          = rnorm(n_plays, mean = 0.02, sd = 0.25),
    score_diff   = sample(-14L:14L, n_plays, replace = TRUE),
    wp_before    = runif(n_plays, 0.1, 0.9),
    passer_player_name   = ifelse(
      runif(n_plays) > 0.6, paste0("QB_", rep(teams_home, each = n_plays %/% 5L)),
      NA_character_
    ),
    rusher_player_name   = ifelse(
      runif(n_plays) > 0.6, paste0("RB_", rep(teams_home, each = n_plays %/% 5L)),
      NA_character_
    ),
    receiver_player_name = ifelse(
      runif(n_plays) > 0.7, paste0("WR_", rep(teams_home, each = n_plays %/% 5L)),
      NA_character_
    )
  )
}


#' Build a minimal synthetic CFB player-season panel
#' Mirrors R/21 output schema (31 columns; uses subset for testing)
make_cfb_panel <- function(n_players = 20L,
                            seasons   = c(2022L, 2023L)) {

  positions <- c("QB", "RB", "WR_TE", "other")
  teams     <- c("Alabama", "Georgia", "Ohio State", "Michigan", "Texas")

  purrr::map_dfr(seasons, function(s) {
    set.seed(s)
    tibble::tibble(
      player_name                  = paste0("Player_", seq_len(n_players), "_s", s),
      season                       = s,
      primary_team                 = sample(teams, n_players, replace = TRUE),
      n_teams                      = sample(1L:2L, n_players, replace = TRUE),
      has_name_collision           = rep(FALSE, n_players),
      position_group               = sample(positions, n_players, replace = TRUE),
      games_played                 = sample(8L:13L, n_players, replace = TRUE),
      total_plays                  = sample(20L:200L, n_players, replace = TRUE),
      low_volume                   = sample(c(TRUE, FALSE), n_players,
                                            replace = TRUE, prob = c(0.2, 0.8)),
      pass_attempts                = sample(0L:400L, n_players, replace = TRUE),
      completions                  = sample(0L:300L, n_players, replace = TRUE),
      completion_pct               = runif(n_players, 0.5, 0.75),
      passing_yards                = sample(0L:4000L, n_players, replace = TRUE),
      pass_tds                     = sample(0L:40L, n_players, replace = TRUE),
      interceptions                = sample(0L:20L, n_players, replace = TRUE),
      pass_epa                     = rnorm(n_players, 5, 20),
      pass_epa_per_attempt         = runif(n_players, -0.3, 0.5),
      rush_attempts                = sample(0L:300L, n_players, replace = TRUE),
      rushing_yards                = sample(0L:1500L, n_players, replace = TRUE),
      rush_tds                     = sample(0L:20L, n_players, replace = TRUE),
      rush_epa                     = rnorm(n_players, 2, 10),
      rush_epa_per_attempt         = runif(n_players, -0.3, 0.4),
      targets                      = sample(0L:150L, n_players, replace = TRUE),
      receptions                   = sample(0L:120L, n_players, replace = TRUE),
      catch_rate                   = runif(n_players, 0.5, 0.85),
      receiving_yards              = sample(0L:1500L, n_players, replace = TRUE),
      rec_tds                      = sample(0L:15L, n_players, replace = TRUE),
      rec_epa                      = rnorm(n_players, 3, 10),
      rec_epa_per_target           = runif(n_players, -0.3, 0.5),
      success_rate                 = runif(n_players, 0.35, 0.65),
      garbage_time_plays_excluded  = sample(0L:50L, n_players, replace = TRUE),
      panel_version                = "s2cfbv1_panel"
    )
  })
}


#' Build a synthetic team defense tibble
#' Simulates output of .build_cfb_defense_and_schedule()$team_defense
make_team_defense <- function(seasons = c(2022L, 2023L)) {
  teams <- c("Auburn", "Tennessee", "Penn State", "Michigan State",
             "Oklahoma", "LSU", "Notre Dame", "Clemson")
  purrr::map_dfr(seasons, function(s) {
    set.seed(s + 1000L)
    tibble::tibble(
      defteam                  = teams,
      season                   = s,
      def_epa_per_play         = rnorm(length(teams), mean = 0.00, sd = 0.08),
      def_success_rate_allowed = runif(length(teams), 0.40, 0.55),
      def_n_plays              = sample(400L:800L, length(teams), replace = TRUE)
    )
  })
}


#' Build a synthetic schedule tibble
#' Simulates output of .build_cfb_defense_and_schedule()$schedule
make_schedule <- function(seasons = c(2022L, 2023L)) {
  home_teams <- c("Alabama", "Georgia", "Ohio State", "Michigan", "Texas")
  away_teams <- c("Auburn", "Tennessee", "Penn State", "Michigan State", "Oklahoma")

  purrr::map_dfr(seasons, function(s) {
    tibble::tibble(
      posteam = c(home_teams, away_teams),
      defteam = c(away_teams, home_teams),
      season  = s
    ) %>%
      dplyr::distinct()
  })
}


#' Build a synthetic Sleeper crosswalk tibble
#' Simulates output of match_sleeper_players()
make_crosswalk <- function(n_players = 30L) {
  set.seed(99L)
  positions <- c("QB", "RB", "WR", "TE", "K", "DEF")
  methods   <- c("gsis", "gsis", "exact_name", "fuzzy_name", "unmatched")
  teams     <- c("KC", "BUF", "PHI", "DAL", "SF", "LAR")

  n_matched  <- round(n_players * 0.85)
  n_unmatched <- n_players - n_matched

  tibble::tibble(
    sleeper_player_id = paste0("SLP_", seq_len(n_players)),
    sleeper_name      = paste0("Player_", seq_len(n_players)),
    sleeper_position  = sample(positions, n_players, replace = TRUE),
    sleeper_team      = sample(teams, n_players, replace = TRUE),
    gsis_id           = c(
      paste0("00-", sprintf("%07d", seq_len(n_matched))),
      rep(NA_character_, n_unmatched)
    ),
    nflfastr_name     = c(
      paste0("NFL_Player_", seq_len(n_matched)),
      rep(NA_character_, n_unmatched)
    ),
    match_method      = c(
      sample(c("gsis", "exact_name", "fuzzy_name"),
             n_matched, replace = TRUE, prob = c(0.6, 0.25, 0.15)),
      rep("unmatched", n_unmatched)
    ),
    match_confidence  = c(
      sample(c("high", "high", "medium"),
             n_matched, replace = TRUE),
      rep("none", n_unmatched)
    )
  )
}


# ==============================================================================
# TESTS: build_cfb_sos_features() -- input validation
# ==============================================================================

test_that("build_cfb_sos_features stops on non-data-frame panel", {
  expect_error(
    build_cfb_sos_features(panel = list(a = 1)),
    regexp = "non-empty data frame"
  )
})

test_that("build_cfb_sos_features stops on empty panel", {
  empty_panel <- tibble::tibble(player_name = character(),
                                 season = integer(),
                                 primary_team = character())
  expect_error(
    build_cfb_sos_features(panel = empty_panel),
    regexp = "non-empty data frame"
  )
})

test_that("build_cfb_sos_features stops on missing panel columns", {
  bad_panel <- tibble::tibble(player_name = "X", season = 2023L)
  # Missing primary_team
  expect_error(
    build_cfb_sos_features(panel = bad_panel),
    regexp = "missing required columns"
  )
})

test_that("build_cfb_sos_features stops on malformed opponent_defense", {
  panel <- make_cfb_panel(n_players = 5L, seasons = 2023L)
  bad_def <- tibble::tibble(defteam = "Alabama", season = 2023L)
  # Missing def_epa_per_play, def_success_rate_allowed, def_n_plays
  expect_error(
    build_cfb_sos_features(panel = panel, opponent_defense = bad_def),
    regexp = "missing required columns"
  )
})


# ==============================================================================
# TESTS: build_cfb_sos_features() -- output structure with pre-built inputs
# ==============================================================================

test_that("build_cfb_sos_features returns a tibble with correct columns", {
  panel      <- make_cfb_panel(n_players = 10L, seasons = 2023L)
  team_def   <- make_team_defense(seasons = 2023L)
  schedule   <- make_schedule(seasons = 2023L)

  # Inject pre-built schedule and defense directly via internal function
  # by calling the exported function with opponent_defense provided.
  # The schedule will still be built from cache; we test with opponent_defense
  # provided and a cache path that won't exist, expecting a warning but
  # relying on the function to handle NA gracefully.

  # Test structural output using opponent_defense provided but no cache
  # (schedule extraction will fail gracefully, SOS will be NA)
  result <- suppressWarnings(
    build_cfb_sos_features(
      panel            = panel,
      opponent_defense = team_def,
      seasons          = 2023L,
      cache_dir        = tempdir(),  # empty dir; schedule extraction fails
      verbose          = FALSE
    )
  )

  expect_true(is.data.frame(result))
  expect_true(nrow(result) == nrow(panel))

  # SOS columns should be added even if all NA
  expect_true("sos_opp_def_epa_per_play" %in% names(result))
  expect_true("sos_opp_def_success_rate_allowed" %in% names(result))
  expect_true("sos_n_opponents" %in% names(result))
  expect_true("sos_computed" %in% names(result))
})

test_that("build_cfb_sos_features does not change row count", {
  panel <- make_cfb_panel(n_players = 15L, seasons = c(2022L, 2023L))
  n_before <- nrow(panel)

  result <- suppressWarnings(
    build_cfb_sos_features(
      panel            = panel,
      opponent_defense = make_team_defense(seasons = c(2022L, 2023L)),
      seasons          = c(2022L, 2023L),
      cache_dir        = tempdir(),
      verbose          = FALSE
    )
  )

  expect_equal(nrow(result), n_before)
})

test_that("build_cfb_sos_features sos_computed is logical", {
  panel <- make_cfb_panel(n_players = 8L, seasons = 2023L)
  result <- suppressWarnings(
    build_cfb_sos_features(
      panel            = panel,
      opponent_defense = make_team_defense(seasons = 2023L),
      seasons          = 2023L,
      cache_dir        = tempdir(),
      verbose          = FALSE
    )
  )

  expect_true(is.logical(result$sos_computed))
})

test_that("build_cfb_sos_features sos_n_opponents is non-negative integer", {
  panel <- make_cfb_panel(n_players = 8L, seasons = 2023L)
  result <- suppressWarnings(
    build_cfb_sos_features(
      panel            = panel,
      opponent_defense = make_team_defense(seasons = 2023L),
      seasons          = 2023L,
      cache_dir        = tempdir(),
      verbose          = FALSE
    )
  )

  expect_true(is.integer(result$sos_n_opponents))
  expect_true(all(result$sos_n_opponents >= 0L, na.rm = TRUE))
})

test_that("build_cfb_sos_features overwrites pre-existing SOS columns with warning", {
  panel <- make_cfb_panel(n_players = 5L, seasons = 2023L) %>%
    dplyr::mutate(sos_computed = TRUE, sos_n_opponents = 99L,
                  sos_opp_def_epa_per_play = 0.99,
                  sos_opp_def_success_rate_allowed = 0.99)

  expect_warning(
    build_cfb_sos_features(
      panel            = panel,
      opponent_defense = make_team_defense(seasons = 2023L),
      seasons          = 2023L,
      cache_dir        = tempdir(),
      verbose          = FALSE
    ),
    regexp = "Overwriting"
  )
})

test_that("build_cfb_sos_features seasons derived from panel when NULL", {
  panel <- make_cfb_panel(n_players = 5L, seasons = c(2021L, 2022L))

  # seasons = NULL should derive 2021:2022 from panel, not error
  result <- suppressWarnings(
    build_cfb_sos_features(
      panel            = panel,
      opponent_defense = make_team_defense(seasons = c(2021L, 2022L)),
      seasons          = NULL,   # Derived from panel
      cache_dir        = tempdir(),
      verbose          = FALSE
    )
  )

  expect_equal(nrow(result), nrow(panel))
})


# ==============================================================================
# TESTS: validate_sos_crosswalk() -- input validation
# ==============================================================================

test_that("validate_sos_crosswalk stops when both inputs are NULL", {
  expect_error(
    validate_sos_crosswalk(sos_panel = NULL, crosswalk = NULL),
    regexp = "At least one"
  )
})

test_that("validate_sos_crosswalk accepts sos_panel only", {
  panel <- make_cfb_panel(n_players = 10L, seasons = 2023L) %>%
    dplyr::mutate(
      sos_opp_def_epa_per_play         = runif(10L, -0.2, 0.2),
      sos_opp_def_success_rate_allowed = runif(10L, 0.4, 0.6),
      sos_n_opponents                  = sample(6L:12L, 10L, replace = TRUE),
      sos_computed                     = TRUE
    )

  result <- validate_sos_crosswalk(sos_panel = panel, crosswalk = NULL)
  expect_true(is.list(result))
  expect_true("valid" %in% names(result))
  expect_true("summary" %in% names(result))
})

test_that("validate_sos_crosswalk accepts crosswalk only", {
  cw <- make_crosswalk(n_players = 20L)
  result <- validate_sos_crosswalk(sos_panel = NULL, crosswalk = cw)
  expect_true(is.list(result))
  expect_true("valid" %in% names(result))
})


# ==============================================================================
# TESTS: validate_sos_crosswalk() -- SOS panel checks
# ==============================================================================

test_that("validate_sos_crosswalk PASS: all SOS cols present and in range", {
  panel <- make_cfb_panel(n_players = 10L, seasons = 2023L) %>%
    dplyr::mutate(
      sos_opp_def_epa_per_play         = runif(10L, -0.2, 0.2),
      sos_opp_def_success_rate_allowed = runif(10L, 0.4, 0.6),
      sos_n_opponents                  = sample(6L:12L, 10L, replace = TRUE),
      sos_computed                     = TRUE
    )

  result <- validate_sos_crosswalk(sos_panel = panel, crosswalk = NULL)
  sos_checks <- result$summary[grepl("^sos_cols|^sos_epa|^sos_sr", result$summary$check_name), ]
  expect_true(all(sos_checks$passed))
})

test_that("validate_sos_crosswalk FAIL: EPA out of range triggers critical failure", {
  panel <- make_cfb_panel(n_players = 5L, seasons = 2023L) %>%
    dplyr::mutate(
      sos_opp_def_epa_per_play         = c(0.1, -0.6, 0.2, 0.8, -0.1),  # two OOR
      sos_opp_def_success_rate_allowed = runif(5L, 0.4, 0.6),
      sos_n_opponents                  = sample(6L:12L, 5L, replace = TRUE),
      sos_computed                     = TRUE
    )

  result <- validate_sos_crosswalk(sos_panel = panel, crosswalk = NULL)
  epa_check <- result$summary[result$summary$check_name == "sos_epa_range", ]
  expect_false(epa_check$passed)
  expect_false(result$valid)
})

test_that("validate_sos_crosswalk FAIL: missing SOS cols fails critical check", {
  panel <- make_cfb_panel(n_players = 5L, seasons = 2023L)
  # No SOS columns added

  result <- validate_sos_crosswalk(sos_panel = panel, crosswalk = NULL)
  cols_check <- result$summary[result$summary$check_name == "sos_cols_present", ]
  expect_false(cols_check$passed)
  expect_false(result$valid)
})

test_that("validate_sos_crosswalk WARN: low sos_computed rate triggers warning not critical", {
  n <- 20L
  panel <- make_cfb_panel(n_players = n, seasons = 2023L) %>%
    dplyr::mutate(
      sos_opp_def_epa_per_play         = runif(n, -0.2, 0.2),
      sos_opp_def_success_rate_allowed = runif(n, 0.4, 0.6),
      sos_n_opponents                  = sample(6L:12L, n, replace = TRUE),
      # Only 50% computed -- below 70% threshold
      sos_computed                     = c(rep(TRUE, n %/% 2L),
                                           rep(FALSE, n - n %/% 2L))
    )

  result <- validate_sos_crosswalk(sos_panel = panel, crosswalk = NULL)
  rate_check <- result$summary[result$summary$check_name == "sos_computed_rate", ]
  expect_false(rate_check$passed)
  expect_equal(as.character(rate_check$severity), "warning")
  # Warning should not flip valid to FALSE (no critical failures)
  critical_checks <- result$summary[result$summary$severity == "critical", ]
  expect_true(all(critical_checks$passed))
})


# ==============================================================================
# TESTS: validate_sos_crosswalk() -- crosswalk checks
# ==============================================================================

test_that("validate_sos_crosswalk PASS: clean crosswalk passes all checks", {
  cw <- make_crosswalk(n_players = 30L)
  result <- validate_sos_crosswalk(sos_panel = NULL, crosswalk = cw)

  # The synthetic crosswalk has unique IDs and > 70% match rate
  uid_check   <- result$summary[result$summary$check_name == "crosswalk_unique_ids", ]
  match_check <- result$summary[result$summary$check_name == "crosswalk_match_rate", ]

  expect_true(nrow(uid_check) == 1L)
  expect_true(uid_check$passed)
  expect_true(nrow(match_check) == 1L)
  expect_true(match_check$passed)
})

test_that("validate_sos_crosswalk FAIL: duplicate sleeper_player_id fails critical", {
  cw <- make_crosswalk(n_players = 10L)
  # Introduce a duplicate
  cw <- dplyr::bind_rows(cw, cw[1L, ])

  result <- validate_sos_crosswalk(sos_panel = NULL, crosswalk = cw)
  uid_check <- result$summary[result$summary$check_name == "crosswalk_unique_ids", ]
  expect_false(uid_check$passed)
  expect_false(result$valid)
})

test_that("validate_sos_crosswalk FAIL: low match rate fails critical", {
  # Build crosswalk with only 50% match rate (below 70% threshold)
  cw <- make_crosswalk(n_players = 40L)
  n_unmatch <- round(nrow(cw) * 0.5)
  cw$match_method[seq_len(n_unmatch)]      <- "unmatched"
  cw$gsis_id[seq_len(n_unmatch)]           <- NA_character_
  cw$nflfastr_name[seq_len(n_unmatch)]     <- NA_character_
  cw$match_confidence[seq_len(n_unmatch)]  <- "none"

  result <- validate_sos_crosswalk(sos_panel = NULL, crosswalk = cw)
  match_check <- result$summary[result$summary$check_name == "crosswalk_match_rate", ]
  expect_false(match_check$passed)
  expect_false(result$valid)
})

test_that("validate_sos_crosswalk WARN: duplicate GSIS IDs trigger warning not critical", {
  cw <- make_crosswalk(n_players = 20L)
  # Force a duplicate GSIS ID among matched rows
  cw$gsis_id[1L]  <- cw$gsis_id[2L]

  result <- validate_sos_crosswalk(sos_panel = NULL, crosswalk = cw)
  gsis_check <- result$summary[result$summary$check_name == "crosswalk_gsis_unique", ]
  expect_true(nrow(gsis_check) == 1L)
  expect_false(gsis_check$passed)
  expect_equal(as.character(gsis_check$severity), "warning")
})

test_that("validate_sos_crosswalk result contains required list elements", {
  cw <- make_crosswalk(n_players = 15L)
  result <- validate_sos_crosswalk(sos_panel = NULL, crosswalk = cw)

  expect_true("valid" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("crosswalk_duplicates" %in% names(result))
  expect_true("gsis_duplicates" %in% names(result))
  expect_true(is.logical(result$valid))
  expect_true(is.data.frame(result$summary))
})

test_that("validate_sos_crosswalk summary tibble has required columns", {
  cw <- make_crosswalk(n_players = 10L)
  result <- validate_sos_crosswalk(sos_panel = NULL, crosswalk = cw)

  expect_true(all(c("check_name", "severity", "passed", "detail") %in%
                    names(result$summary)))
})


# ==============================================================================
# TESTS: .build_cfb_defense_and_schedule() -- internal helper via integration
# ==============================================================================

test_that(".build_cfb_defense_and_schedule stops on empty cache dir", {
  # Point to a temp dir with no RDS files
  expect_error(
    .build_cfb_defense_and_schedule(
      seasons   = 2023L,
      cache_dir = tempdir(),
      verbose   = FALSE
    ),
    regexp = "No team defensive quality data could be built"
  )
})


# ==============================================================================
# TESTS: build_sleeper_gsis_crosswalk() -- input validation
# ==============================================================================

test_that("build_sleeper_gsis_crosswalk stops on empty league_ids", {
  expect_error(
    build_sleeper_gsis_crosswalk(league_ids = character(0)),
    regexp = "non-empty character vector"
  )
})

test_that("build_sleeper_gsis_crosswalk stops on blank league_ids", {
  expect_error(
    build_sleeper_gsis_crosswalk(league_ids = c("", "  ")),
    regexp = "non-empty character vector"
  )
})

test_that("build_sleeper_gsis_crosswalk stops on non-integer nfl_season", {
  expect_error(
    build_sleeper_gsis_crosswalk(
      league_ids = "123",
      nfl_season = NA
    ),
    regexp = "single integer"
  )
})


# ==============================================================================
# TESTS: W8_SOS_COLS constant
# ==============================================================================

test_that("W8_SOS_COLS contains expected column names", {
  expect_true("sos_opp_def_epa_per_play" %in% W8_SOS_COLS)
  expect_true("sos_opp_def_success_rate_allowed" %in% W8_SOS_COLS)
  expect_true("sos_n_opponents" %in% W8_SOS_COLS)
  expect_true("sos_computed" %in% W8_SOS_COLS)
  expect_equal(length(W8_SOS_COLS), 4L)
})


# ==============================================================================
# TESTS: Constants integrity
# ==============================================================================

test_that("CFB_SOS_SEASONS_DEFAULT starts at 2014 and does not exceed 2025", {
  expect_equal(min(CFB_SOS_SEASONS_DEFAULT), 2014L)
  expect_true(max(CFB_SOS_SEASONS_DEFAULT) <= 2025L)
})

test_that("CFB_SOS_MIN_DEF_PLAYS is a positive integer", {
  expect_true(is.numeric(CFB_SOS_MIN_DEF_PLAYS))
  expect_true(CFB_SOS_MIN_DEF_PLAYS > 0L)
})

test_that("W8_GARBAGE_SCORE_DIFF matches R/21 constant value", {
  expect_equal(W8_GARBAGE_SCORE_DIFF, 28L)
})

test_that("W8_GARBAGE_WP_LO and WP_HI are valid probability bounds", {
  expect_true(W8_GARBAGE_WP_LO >= 0 && W8_GARBAGE_WP_LO < 0.5)
  expect_true(W8_GARBAGE_WP_HI > 0.5 && W8_GARBAGE_WP_HI <= 1)
})

test_that("W8_SCRIMMAGE_TYPES includes all play types from R/21 CFB_SCRIMMAGE_TYPES", {
  # Core types that must be present
  expected_core <- c("Rush", "Rushing Touchdown",
                     "Pass Completion", "Pass Incompletion",
                     "Passing Touchdown", "Sack")
  expect_true(all(expected_core %in% W8_SCRIMMAGE_TYPES))
})


# ==============================================================================
# TESTS: Source guard behavior
# ==============================================================================

test_that("match_sleeper_players is available after sourcing R/22", {
  expect_true(exists("match_sleeper_players", mode = "function"))
})

test_that("load_normalized_cfb_season is available after sourcing R/22", {
  expect_true(exists("load_normalized_cfb_season", mode = "function"))
})

test_that("build_cfb_sos_features is available as a function", {
  expect_true(exists("build_cfb_sos_features", mode = "function"))
})

test_that("build_sleeper_gsis_crosswalk is available as a function", {
  expect_true(exists("build_sleeper_gsis_crosswalk", mode = "function"))
})

test_that("validate_sos_crosswalk is available as a function", {
  expect_true(exists("validate_sos_crosswalk", mode = "function"))
})

test_that("run_week8_pipeline is available as a function", {
  expect_true(exists("run_week8_pipeline", mode = "function"))
})
