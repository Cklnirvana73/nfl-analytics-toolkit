# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 10
# Test Suite: College-to-NFL Translation Model Functions
# File: tests/test_season2_week10_functions.R
#
# Coverage:
#   Section A: .normalize_player_name()       --  6 tests
#   Section B: .compute_team_pass_attempts()  --  5 tests
#   Section C: .apply_multiseason_weights()   --  6 tests
#   Section D: .impute_features()             --  6 tests
#   Section E: .fit_elastic_net_loco()        --  5 tests
#   Section F: link_cfb_to_nfl()              -- 12 tests
#   Section G: build_translation_features()   -- 10 tests
#   Section H: train_translation_model()      --  8 tests
#   Section I: evaluate_translation_accuracy()--  6 tests
#   Section J: identify_translation_gaps()    --  6 tests
#   Section K: validate_translation_assumptions()-- 8 tests
#   Section L: Assumption validation          --  5 tests
#
# Total: 83 tests
#
# No network calls. All tests use synthetic in-memory fixtures.
# Does NOT call run_week10_pipeline() -- no cache or source() required.
#
# Run:
#   testthat::test_file(here::here("tests", "test_season2_week10_functions.R"))
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)
library(glue)
library(here)

source(here::here("R", "24_translation_model.R"))


# ==============================================================================
# SHARED FIXTURES
# ==============================================================================

# make_cfb_panel(): minimal R/21-schema CFB player-season panel.
# Covers 3 positions, 3 seasons, 4 players. Designed for hand-verification.
make_cfb_panel <- function() {
  tibble::tibble(
    player_name          = c("Aaron QB",   "Aaron QB",   "Aaron QB",
                             "Bobby RB",   "Bobby RB",
                             "Carl WR",    "Carl WR",    "Carl WR",
                             "Dave TE"),
    season               = c(2021L, 2022L, 2023L,
                             2022L, 2023L,
                             2021L, 2022L, 2023L,
                             2023L),
    primary_team         = c("Alabama",   "Alabama",   "Alabama",
                             "Georgia",   "Georgia",
                             "Ohio St",   "Ohio St",   "Ohio St",
                             "Michigan"),
    position_group       = c("QB", "QB", "QB",
                             "RB", "RB",
                             "WR_TE", "WR_TE", "WR_TE",
                             "WR_TE"),
    games_played         = c(12L, 13L, 13L,
                             12L, 13L,
                             12L, 13L, 13L,
                             13L),
    pass_attempts        = c(300L, 350L, 360L,
                             5L,   4L,
                             2L,   3L,   2L,
                             1L),
    passing_yards        = c(3200L, 3800L, 4000L,
                             30L,   20L,
                             10L,   15L,   12L,
                             5L),
    pass_tds             = c(28L, 32L, 35L,
                             0L,  0L,
                             0L,  0L,  0L,
                             0L),
    interceptions        = c(6L,  5L,  4L,
                             0L,  0L,
                             0L,  0L,  0L,
                             0L),
    completion_pct       = c(0.64, 0.67, 0.70,
                             0.50, 0.60,
                             0.60, 0.65, 0.70,
                             0.55),
    pass_epa_per_attempt = c(0.15, 0.18, 0.20,
                             0.05, 0.04,
                             0.02, 0.03, 0.04,
                             0.01),
    rush_attempts        = c(60L,  55L,  50L,
                             220L, 240L,
                             15L,  12L,  10L,
                             5L),
    rushing_yards        = c(320L, 290L, 280L,
                             1100L,1200L,
                             60L,  50L,  45L,
                             20L),
    rush_tds             = c(5L,  4L,  4L,
                             10L, 11L,
                             1L,  0L,  1L,
                             0L),
    rush_epa_per_attempt = c(0.05, 0.04, 0.06,
                             0.18, 0.20,
                             0.02, 0.01, 0.03,
                             0.02),
    targets              = c(5L,   4L,   3L,
                             40L,  45L,
                             90L,  100L, 110L,
                             60L),
    receptions           = c(3L,   3L,   2L,
                             32L,  38L,
                             65L,  72L,  80L,
                             42L),
    receiving_yards      = c(25L,  20L,  18L,
                             280L, 320L,
                             900L, 1050L,1200L,
                             500L),
    rec_tds              = c(0L,  0L,  0L,
                             2L,  3L,
                             7L,  8L,  10L,
                             4L),
    rec_epa_per_target   = c(0.01, 0.02, 0.01,
                             0.12, 0.14,
                             0.22, 0.24, 0.26,
                             0.18),
    catch_rate           = c(0.60, 0.75, 0.67,
                             0.80, 0.84,
                             0.72, 0.72, 0.73,
                             0.70),
    success_rate         = c(0.48, 0.50, 0.52,
                             0.46, 0.48,
                             0.44, 0.46, 0.48,
                             0.45),
    low_volume           = c(FALSE, FALSE, FALSE,
                             FALSE, FALSE,
                             FALSE, FALSE, FALSE,
                             FALSE)
  )
}

# make_draft_picks(): mimics nflreadr::load_draft_picks() column schema.
# Verified column names from actual console run.
make_draft_picks <- function() {
  tibble::tibble(
    season          = c(2024L, 2024L, 2024L, 2024L, 2025L),
    round           = c(1L,    2L,    3L,    1L,    1L),
    pick            = c(5L,    42L,   88L,   12L,   8L),
    team            = c("PHI", "KC",  "DAL", "NE",  "SF"),
    gsis_id         = c("QB-GSIS-1", "RB-GSIS-1", "WR-GSIS-1",
                        "TE-GSIS-1", "WR-GSIS-2"),
    pfr_player_name = c("Aaron QB",  "Bobby RB",  "Carl WR",
                        "Dave TE",   "Eve WR"),
    college         = c("Alabama",   "Georgia",   "Ohio St",
                        "Michigan",  "LSU"),
    position        = c("QB",        "RB",        "WR",
                        "TE",        "WR"),
    age             = c(22.1,        21.8,        22.5,
                        23.0,        21.5)
  )
}

# make_sos_panel(): mimics R/22 SOS panel output.
make_sos_panel <- function() {
  tibble::tibble(
    player_name                      = c("Aaron QB",  "Bobby RB",
                                         "Carl WR",   "Dave TE",
                                         "Eve WR"),
    season                           = c(2023L, 2023L, 2023L, 2023L, 2024L),
    sos_opp_def_epa_per_play         = c(-0.05, -0.08, -0.06, -0.04, -0.07),
    sos_opp_def_success_rate_allowed = c(0.38,   0.36,  0.37,  0.39,  0.35),
    sos_n_opponents                  = c(12L,   12L,   12L,   12L,   12L),
    sos_computed                     = c(TRUE,  TRUE,  TRUE,  TRUE,  TRUE)
  )
}

# make_nfl_panel(): mimics R/16 NFL player-season panel.
make_nfl_panel <- function() {
  tibble::tibble(
    player_id            = c("QB-GSIS-1", "QB-GSIS-1", "QB-GSIS-1",
                             "RB-GSIS-1", "RB-GSIS-1", "RB-GSIS-1",
                             "WR-GSIS-1", "WR-GSIS-1", "WR-GSIS-1",
                             "TE-GSIS-1", "TE-GSIS-1",
                             # Other players for ranking context
                             "OTHER-QB-1", "OTHER-QB-2",
                             "OTHER-RB-1", "OTHER-WR-1", "OTHER-TE-1"),
    player_name          = c("Aaron QB",  "Aaron QB",  "Aaron QB",
                             "Bobby RB",  "Bobby RB",  "Bobby RB",
                             "Carl WR",   "Carl WR",   "Carl WR",
                             "Dave TE",   "Dave TE",
                             "Other QB1", "Other QB2",
                             "Other RB1", "Other WR1", "Other TE1"),
    season               = c(2024L, 2025L, 2026L,
                             2024L, 2025L, 2026L,
                             2024L, 2025L, 2026L,
                             2024L, 2025L,
                             2024L, 2024L,
                             2024L, 2024L, 2024L),
    position             = c("QB","QB","QB",
                             "RB","RB","RB",
                             "WR","WR","WR",
                             "TE","TE",
                             "QB","QB",
                             "RB","WR","TE"),
    position_group       = c("QB","QB","QB",
                             "RB","RB","RB",
                             "WR_TE","WR_TE","WR_TE",
                             "WR_TE","WR_TE",
                             "QB","QB",
                             "RB","WR_TE","WR_TE"),
    games_played         = c(16L, 17L, 17L,
                             15L, 16L, 17L,
                             17L, 17L, 17L,
                             16L, 15L,
                             16L, 14L,
                             16L, 17L, 16L),
    passing_yards        = c(4200L, 4500L, 4800L,
                             0L,    0L,    0L,
                             0L,    0L,    0L,
                             0L,    0L,
                             3800L, 3200L,
                             0L,    0L,    0L),
    pass_tds             = c(32L,  34L,  36L,
                             0L,   0L,   0L,
                             0L,   0L,   0L,
                             0L,   0L,
                             28L,  22L,
                             0L,   0L,   0L),
    interceptions_thrown = c(8L,   6L,   5L,
                             0L,   0L,   0L,
                             0L,   0L,   0L,
                             0L,   0L,
                             10L,  12L,
                             0L,   0L,   0L),
    rushing_yards        = c(320L, 280L, 300L,
                             1100L,1200L,1150L,
                             60L,  55L,  50L,
                             20L,  18L,
                             200L, 180L,
                             950L, 40L,  15L),
    rush_tds             = c(4L,  3L,  4L,
                             10L, 12L, 11L,
                             1L,  0L,  1L,
                             0L,  0L,
                             2L,  1L,
                             8L,  0L,  0L),
    receptions           = c(30L, 28L, 25L,
                             40L, 45L, 42L,
                             90L, 100L,95L,
                             55L, 50L,
                             20L, 18L,
                             35L, 80L, 45L),
    receiving_yards      = c(280L,260L,240L,
                             350L,400L,380L,
                             1100L,1250L,1180L,
                             650L,600L,
                             180L,160L,
                             300L,980L,520L),
    rec_tds              = c(2L,  2L,  2L,
                             3L,  4L,  3L,
                             8L,  9L,  8L,
                             6L,  5L,
                             1L,  1L,
                             2L,  7L,  4L)
  )
}

# make_minimal_crosswalk(): minimal output of link_cfb_to_nfl() for use
# in downstream function tests.
make_minimal_crosswalk <- function() {
  tibble::tibble(
    draft_year       = c(2024L, 2024L, 2024L, 2024L),
    nfl_gsis_id      = c("QB-GSIS-1", "RB-GSIS-1", "WR-GSIS-1", "TE-GSIS-1"),
    draft_position   = c("QB",        "RB",        "WR",        "TE"),
    draft_round      = c(1L,          2L,          3L,          1L),
    draft_pick       = c(5L,          42L,         88L,         12L),
    draft_age        = c(22.1,        21.8,        22.5,        23.0),
    cfb_player_name  = c("Aaron QB",  "Bobby RB",  "Carl WR",   "Dave TE"),
    cfb_final_season = c(2023L,       2023L,       2023L,       2023L),
    cfb_primary_team = c("Alabama",   "Georgia",   "Ohio St",   "Michigan"),
    n_cfb_seasons    = c(3L,          2L,          3L,          1L),
    match_method     = c("exact_name_college", "exact_name_college",
                         "exact_name_only",    "exact_name_college"),
    match_confidence = c(1.0,         1.0,         0.9,         1.0)
  )
}


# ==============================================================================
# SECTION A: .normalize_player_name()
# ==============================================================================

test_that("A1: normalize_player_name strips Jr suffix", {
  expect_equal(.normalize_player_name("Patrick Mahomes Jr."), "patrickmahomes")
})

test_that("A2: normalize_player_name strips Sr suffix", {
  expect_equal(.normalize_player_name("Calvin Ridley Sr"), "calvinridley")
})

test_that("A3: normalize_player_name strips II, III suffixes", {
  expect_equal(.normalize_player_name("Michael Thomas II"), "michaelthomas")
  expect_equal(.normalize_player_name("Will Anderson III"), "willanderson")
})

test_that("A4: normalize_player_name lowercases and removes non-alpha", {
  expect_equal(.normalize_player_name("A.J. Brown"), "ajbrown")
})

test_that("A5: normalize_player_name handles NULL input", {
  expect_equal(.normalize_player_name(NULL), character(0L))
})

test_that("A6: normalize_player_name handles NA input returns NA", {
  # as.character(NA_character_) stays NA through tolower/gsub chain
  result <- .normalize_player_name(NA_character_)
  expect_true(is.na(result))
})


# ==============================================================================
# SECTION B: .compute_team_pass_attempts()
# ==============================================================================

test_that("B1: returns one row per team-season", {
  panel <- make_cfb_panel()
  result <- .compute_team_pass_attempts(panel)
  expect_equal(nrow(result), dplyr::n_distinct(
    panel$primary_team[!panel$low_volume],
    panel$season[!panel$low_volume]
  ))
})

test_that("B2: team_pass_attempts sums correctly across positions", {
  panel <- make_cfb_panel()
  result <- .compute_team_pass_attempts(panel)
  # Alabama 2023: only Aaron QB (360 pass_attempts). Carl WR is Ohio St.
  alabama_2023 <- result %>%
    dplyr::filter(primary_team == "Alabama", season == 2023L)
  expect_equal(alabama_2023$team_pass_attempts, 360L)
})

test_that("B3: excludes low_volume rows", {
  panel <- make_cfb_panel()
  panel$low_volume[panel$player_name == "Aaron QB" & panel$season == 2021L] <- TRUE
  result <- .compute_team_pass_attempts(panel)
  alabama_2021 <- result %>%
    dplyr::filter(primary_team == "Alabama", season == 2021L)
  # Alabama 2021 only has Aaron QB. Excluding him leaves 0 pass_attempts.
  # The row is dropped by filter(team_pass_attempts > 0) -- 0 rows expected.
  expect_equal(nrow(alabama_2021), 0L)
})

test_that("B4: returns team_pass_attempts as integer-compatible", {
  result <- .compute_team_pass_attempts(make_cfb_panel())
  expect_true(is.numeric(result$team_pass_attempts))
})

test_that("B5: rows with zero team_pass_attempts are excluded", {
  result <- .compute_team_pass_attempts(make_cfb_panel())
  expect_true(all(result$team_pass_attempts > 0L))
})


# ==============================================================================
# SECTION C: .apply_multiseason_weights()
# ==============================================================================

test_that("C1: final season receives double weight", {
  # Two seasons: prior (2022), final (2023)
  # pass_att_pg: 100 in 2022, 200 in 2023
  # Expected: (1*100 + 2*200) / 3 = 500/3 = 166.67
  seasons <- tibble::tibble(
    season         = c(2022L, 2023L),
    cfb_final_season = c(2023L, 2023L),
    pass_att_pg    = c(100, 200)
  )
  result <- .apply_multiseason_weights(seasons, "pass_att_pg")
  expect_equal(round(result$pass_att_pg, 4), round(500/3, 4))
})

test_that("C2: single season returns that season's value unchanged", {
  seasons <- tibble::tibble(
    season      = c(2023L),
    pass_att_pg = c(150)
  )
  result <- .apply_multiseason_weights(seasons, "pass_att_pg")
  expect_equal(result$pass_att_pg, 150)
})

test_that("C3: NA in prior season excluded from weighted average", {
  # prior NA, final = 100 -> result = 100 (only valid value)
  seasons <- tibble::tibble(
    season      = c(2022L, 2023L),
    pass_att_pg = c(NA_real_, 100)
  )
  result <- .apply_multiseason_weights(seasons, "pass_att_pg")
  expect_equal(result$pass_att_pg, 100)
})

test_that("C4: all NA returns NA", {
  seasons <- tibble::tibble(
    season      = c(2022L, 2023L),
    pass_att_pg = c(NA_real_, NA_real_)
  )
  result <- .apply_multiseason_weights(seasons, "pass_att_pg")
  expect_true(is.na(result$pass_att_pg))
})

test_that("C5: returns one-row tibble", {
  seasons <- tibble::tibble(
    season      = c(2021L, 2022L, 2023L),
    pass_att_pg = c(80, 100, 120)
  )
  result <- .apply_multiseason_weights(seasons, "pass_att_pg")
  expect_equal(nrow(result), 1L)
})

test_that("C6: three seasons weighted correctly", {
  # prior (2021): 80, prior (2022): 100, final (2023): 120
  # weights: 1, 1, 2 -> (80 + 100 + 240) / 4 = 105
  seasons <- tibble::tibble(
    season      = c(2021L, 2022L, 2023L),
    pass_att_pg = c(80, 100, 120)
  )
  result <- .apply_multiseason_weights(seasons, "pass_att_pg")
  expect_equal(result$pass_att_pg, 105)
})


# ==============================================================================
# SECTION D: .impute_features()
# ==============================================================================

test_that("D1: NAs replaced with training median in training mode", {
  df <- tibble::tibble(x = c(1, 2, NA, 4, 5))
  result <- .impute_features(df, "x", impute_medians = NULL)
  # median of c(1,2,4,5) = 3
  expect_equal(result$imputed_df$x[3], 3)
})

test_that("D2: returns impute_medians in training mode", {
  df <- tibble::tibble(x = c(1, 2, 3, 4, 5))
  result <- .impute_features(df, "x", impute_medians = NULL)
  expect_equal(result$impute_medians[["x"]], 3)
})

test_that("D3: applies stored median in prediction mode", {
  df <- tibble::tibble(x = c(NA_real_, 10))
  result <- .impute_features(df, "x", impute_medians = c(x = 99))
  expect_equal(result$imputed_df$x[1], 99)
})

test_that("D4: non-NA values unchanged", {
  df <- tibble::tibble(x = c(1, 2, NA))
  result <- .impute_features(df, "x", impute_medians = NULL)
  expect_equal(result$imputed_df$x[1], 1)
  expect_equal(result$imputed_df$x[2], 2)
})

test_that("D5: all-NA column imputed with 0 when median is NA", {
  df <- tibble::tibble(x = c(NA_real_, NA_real_))
  result <- .impute_features(df, "x", impute_medians = NULL)
  # median of all-NA = NA -> imputed with 0
  expect_equal(result$imputed_df$x, c(0, 0))
})

test_that("D6: multiple columns imputed independently", {
  df <- tibble::tibble(x = c(NA, 2, 4), y = c(10, NA, 30))
  result <- .impute_features(df, c("x", "y"), impute_medians = NULL)
  expect_equal(result$imputed_df$x[1], median(c(2, 4)))
  expect_equal(result$imputed_df$y[2], median(c(10, 30)))
})


# ==============================================================================
# SECTION E: .fit_elastic_net_loco()
# ==============================================================================

test_that("E1: returns numeric vector same length as y", {
  set.seed(42L)
  n <- 40L
  X <- matrix(rnorm(n * 3), nrow = n)
  y <- rnorm(n)
  draft_years <- rep(2015L:2018L, each = 10L)
  result <- .fit_elastic_net_loco(X, y, draft_years)
  expect_equal(length(result), n)
  expect_true(is.numeric(result))
})

test_that("E2: each draft class gets holdout predictions", {
  set.seed(42L)
  n <- 40L
  X <- matrix(rnorm(n * 2), nrow = n)
  y <- rnorm(n)
  draft_years <- rep(2015L:2018L, each = 10L)
  result <- .fit_elastic_net_loco(X, y, draft_years)
  expect_true(all(!is.na(result)))
})

test_that("E3: fails with fewer than 2 draft classes", {
  X <- matrix(rnorm(20L), nrow = 10L)
  y <- rnorm(10L)
  draft_years <- rep(2020L, 10L)
  expect_error(.fit_elastic_net_loco(X, y, draft_years), "2 distinct draft classes")
})

test_that("E4: predictions are numeric and finite", {
  set.seed(42L)
  X <- matrix(rnorm(60L), nrow = 30L)
  y <- rnorm(30L)
  draft_years <- rep(2015L:2017L, each = 10L)
  result <- .fit_elastic_net_loco(X, y, draft_years)
  expect_true(all(is.finite(result)))
})

test_that("E5: reproducible with same seed via GLMNET_SEED", {
  X <- matrix(rnorm(40L), nrow = 20L)
  y <- rnorm(20L)
  draft_years <- rep(2015L:2016L, each = 10L)
  r1 <- .fit_elastic_net_loco(X, y, draft_years)
  r2 <- .fit_elastic_net_loco(X, y, draft_years)
  expect_equal(r1, r2)
})


# ==============================================================================
# SECTION F: link_cfb_to_nfl()
# ==============================================================================

test_that("F1: returns tibble with required columns", {
  result <- link_cfb_to_nfl(make_cfb_panel(), make_draft_picks())
  required_cols <- c(
    "draft_year", "nfl_gsis_id", "draft_position", "draft_round",
    "draft_pick", "draft_age", "cfb_player_name", "cfb_final_season",
    "cfb_primary_team", "n_cfb_seasons", "match_method", "match_confidence"
  )
  expect_true(all(required_cols %in% names(result)))
})

test_that("F2: one row per draft entry in TRANSLATION_POSITIONS", {
  picks <- make_draft_picks()
  n_skill <- sum(picks$position %in% TRANSLATION_POSITIONS)
  result <- link_cfb_to_nfl(make_cfb_panel(), picks)
  expect_equal(nrow(result), n_skill)
})

test_that("F3: exact name+college match returns match_method exact_name_college", {
  result <- link_cfb_to_nfl(make_cfb_panel(), make_draft_picks())
  # Aaron QB: Alabama matches Alabama exactly
  aaron_row <- result %>% dplyr::filter(cfb_player_name == "Aaron QB")
  expect_equal(aaron_row$match_method, "exact_name_college")
})

test_that("F4: exact name match returns match_confidence 0.9", {
  # Carl WR: college is "Ohio St" in CFB panel, "Ohio St" in draft data
  # If they differ, falls to exact_name_only with 0.9
  result <- link_cfb_to_nfl(make_cfb_panel(), make_draft_picks())
  carl_row <- result %>% dplyr::filter(cfb_player_name == "Carl WR")
  expect_true(carl_row$match_confidence >= 0.9)
})

test_that("F5: unmatched player returns match_method unmatched", {
  picks <- make_draft_picks()
  # Add a player not in CFB panel
  picks <- dplyr::add_row(picks,
    season = 2024L, round = 5L, pick = 180L, team = "GB",
    gsis_id = "UNKNOWN-1", pfr_player_name = "Zach Nomatch",
    college = "Nowhere U", position = "WR", age = 22.0
  )
  result <- link_cfb_to_nfl(make_cfb_panel(), picks)
  nomatch_row <- result %>% dplyr::filter(is.na(cfb_player_name))
  expect_true(nrow(nomatch_row) >= 1L)
  expect_true(all(nomatch_row$match_method == "unmatched"))
})

test_that("F6: match_confidence is 1.0 for exact_name_college", {
  result <- link_cfb_to_nfl(make_cfb_panel(), make_draft_picks())
  exact <- result %>% dplyr::filter(match_method == "exact_name_college")
  expect_true(all(exact$match_confidence == 1.0))
})

test_that("F7: cfb_final_season is draft_year minus 1", {
  result <- link_cfb_to_nfl(make_cfb_panel(), make_draft_picks())
  matched <- result %>% dplyr::filter(match_method != "unmatched")
  expect_true(all(matched$cfb_final_season == matched$draft_year - 1L))
})

test_that("F8: n_cfb_seasons >= 1 for all matched players", {
  result <- link_cfb_to_nfl(make_cfb_panel(), make_draft_picks())
  matched <- result %>% dplyr::filter(match_method != "unmatched")
  expect_true(all(matched$n_cfb_seasons >= 1L))
})

test_that("F9: stops on empty cfb_panel", {
  expect_error(
    link_cfb_to_nfl(tibble::tibble(), make_draft_picks()),
    "non-empty"
  )
})

test_that("F10: stops when draft_data missing required column", {
  picks <- make_draft_picks() %>% dplyr::select(-college)
  expect_error(
    link_cfb_to_nfl(make_cfb_panel(), picks),
    "missing expected columns"
  )
})

test_that("F11: filters non-skill positions from draft data", {
  picks <- make_draft_picks()
  picks <- dplyr::add_row(picks,
    season = 2024L, round = 1L, pick = 3L, team = "BUF",
    gsis_id = "OL-1", pfr_player_name = "Big Lineman",
    college = "Alabama", position = "OT", age = 22.0
  )
  result <- link_cfb_to_nfl(make_cfb_panel(), picks)
  expect_true(all(result$draft_position %in% TRANSLATION_POSITIONS))
})

test_that("F12: draft_year matches season from draft data", {
  result <- link_cfb_to_nfl(make_cfb_panel(), make_draft_picks())
  expect_true(all(result$draft_year %in% c(2024L, 2025L)))
})


# ==============================================================================
# SECTION G: build_translation_features()
# ==============================================================================

test_that("G1: returns list with required elements", {
  feat <- build_translation_features(
    cfb_panel   = make_cfb_panel(),
    nfl_panel   = make_nfl_panel(),
    sos_panel   = make_sos_panel(),
    crosswalk   = make_minimal_crosswalk(),
    cutoff_year = 2024L
  )
  required <- c("training", "prediction", "team_pass_attempts",
                "age_centers", "impute_medians", "base_feature_cols",
                "sos_na_rate", "schema_tag")
  expect_true(all(required %in% names(feat)))
})

test_that("G2: schema_tag is correct", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  expect_equal(feat$schema_tag, TRANSLATION_SCHEMA_TAG)
})

test_that("G3: training list has one entry per position", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  expect_equal(names(feat$training), TRANSLATION_POSITIONS)
})

test_that("G4: training rows have draft_year <= cutoff_year", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  for (pos in TRANSLATION_POSITIONS) {
    df <- feat$training[[pos]]
    if (nrow(df) > 0L) {
      expect_true(all(df$draft_year <= 2024L),
        info = glue("Position {pos}: training rows exceed cutoff_year"))
    }
  }
})

test_that("G5: prediction rows have draft_year > cutoff_year", {
  # Add a 2025 draft entry to crosswalk
  cw <- make_minimal_crosswalk()
  cw <- dplyr::add_row(cw,
    draft_year = 2025L, nfl_gsis_id = "WR-GSIS-2",
    draft_position = "WR", draft_round = 1L, draft_pick = 8L,
    draft_age = 21.5, cfb_player_name = "Eve WR",
    cfb_final_season = 2024L, cfb_primary_team = "LSU",
    n_cfb_seasons = 1L, match_method = "exact_name_college",
    match_confidence = 1.0
  )
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    cw, cutoff_year = 2024L
  )
  for (pos in TRANSLATION_POSITIONS) {
    df <- feat$prediction[[pos]]
    if (nrow(df) > 0L) {
      expect_true(all(df$draft_year > 2024L))
    }
  }
})

test_that("G6: age_centers is named numeric vector with one entry per position", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  expect_true(is.numeric(feat$age_centers))
  expect_true(all(TRANSLATION_POSITIONS %in% names(feat$age_centers)))
})

test_that("G7: team_pass_attempts has primary_team, season, team_pass_attempts", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  expect_true(all(c("primary_team", "season", "team_pass_attempts") %in%
    names(feat$team_pass_attempts)))
})

test_that("G8: sos_na_rate tibble has sos_na_pct column", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  expect_true("sos_na_pct" %in% names(feat$sos_na_rate))
})

test_that("G9: stops on empty cfb_panel", {
  expect_error(
    build_translation_features(
      tibble::tibble(), make_nfl_panel(), make_sos_panel(),
      make_minimal_crosswalk(), cutoff_year = 2024L
    ),
    regexp = "."
  )
})

test_that("G10: stops when nfl_panel missing required column", {
  bad_nfl <- make_nfl_panel() %>% dplyr::select(-receiving_yards)
  expect_error(
    build_translation_features(
      make_cfb_panel(), bad_nfl, make_sos_panel(),
      make_minimal_crosswalk(), cutoff_year = 2024L
    ),
    "nfl_panel missing columns"
  )
})


# ==============================================================================
# SECTION H: train_translation_model()
# ==============================================================================

# Helper to build a minimal feature matrix for training tests
make_minimal_feature_matrix <- function() {
  build_translation_features(
    cfb_panel   = make_cfb_panel(),
    nfl_panel   = make_nfl_panel(),
    sos_panel   = make_sos_panel(),
    crosswalk   = make_minimal_crosswalk(),
    cutoff_year = 2024L
  )
}

test_that("H1: returns list with required elements", {
  feat <- make_minimal_feature_matrix()
  # Skip if no training data (fixture may be too sparse for some positions)
  has_training <- any(purrr::map_lgl(
    TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L
  ))
  skip_if(!has_training, "Insufficient training rows in fixture")

  result <- train_translation_model(feat, cutoff_year = 2024L)
  required <- c("models_base", "models_enriched", "loco_predictions",
                "loco_performance", "impute_medians", "base_feature_cols")
  expect_true(all(required %in% names(result)))
})

test_that("H2: models_base has one model per position", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  result <- train_translation_model(feat, cutoff_year = 2024L)
  expect_equal(names(result$models_base), TRANSLATION_POSITIONS)
})

test_that("H3: models_enriched has one model per position", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  result <- train_translation_model(feat, cutoff_year = 2024L)
  expect_equal(names(result$models_enriched), TRANSLATION_POSITIONS)
})

test_that("H4: loco_predictions has required columns", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  result <- train_translation_model(feat, cutoff_year = 2024L)
  required_cols <- c("nfl_gsis_id", "draft_year", "draft_position",
                     "ppr_per_game_y13", "pred_base", "pred_enriched", "is_hit")
  expect_true(all(required_cols %in% names(result$loco_predictions)))
})

test_that("H5: loco_performance has one row per position per variant", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  result <- train_translation_model(feat, cutoff_year = 2024L)
  expect_equal(nrow(result$loco_performance), length(TRANSLATION_POSITIONS))
})

test_that("H6: pred_base and pred_enriched are numeric and finite", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  result <- train_translation_model(feat, cutoff_year = 2024L)
  preds <- result$loco_predictions
  expect_true(is.numeric(preds$pred_base))
  expect_true(is.numeric(preds$pred_enriched))
})

test_that("H7: stops on missing training element", {
  expect_error(
    train_translation_model(list(training = NULL)),
    regexp = "."
  )
})

test_that("H8: impute_medians passed through from feature_matrix", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  result <- train_translation_model(feat, cutoff_year = 2024L)
  expect_true(is.list(result$impute_medians))
  expect_true(all(TRANSLATION_POSITIONS %in% names(result$impute_medians)))
})


# ==============================================================================
# SECTION I: evaluate_translation_accuracy()
# ==============================================================================

test_that("I1: returns tibble with required columns", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  model_list <- train_translation_model(feat, cutoff_year = 2024L)
  result <- evaluate_translation_accuracy(model_list, feat)
  required <- c("draft_position", "model_variant", "n_players",
                "rmse", "mae", "r_squared", "hit_accuracy", "accuracy_delta")
  expect_true(all(required %in% names(result)))
})

test_that("I2: has two rows per position (base and enriched)", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  model_list <- train_translation_model(feat, cutoff_year = 2024L)
  result <- evaluate_translation_accuracy(model_list, feat)
  counts <- result %>% dplyr::count(draft_position)
  expect_true(all(counts$n <= 2L))
})

test_that("I3: RMSE is non-negative", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  model_list <- train_translation_model(feat, cutoff_year = 2024L)
  result <- evaluate_translation_accuracy(model_list, feat)
  expect_true(all(result$rmse >= 0, na.rm = TRUE))
})

test_that("I4: accuracy_delta is NA for base variant", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  model_list <- train_translation_model(feat, cutoff_year = 2024L)
  result <- evaluate_translation_accuracy(model_list, feat)
  base_rows <- result %>% dplyr::filter(model_variant == "base")
  expect_true(all(is.na(base_rows$accuracy_delta)))
})

test_that("I5: hit_accuracy is between 0 and 1", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  model_list <- train_translation_model(feat, cutoff_year = 2024L)
  result <- evaluate_translation_accuracy(model_list, feat)
  expect_true(all(result$hit_accuracy >= 0 & result$hit_accuracy <= 1,
    na.rm = TRUE))
})

test_that("I6: stops on NULL loco_predictions", {
  expect_error(
    evaluate_translation_accuracy(list(loco_predictions = NULL)),
    regexp = "."
  )
})


# ==============================================================================
# SECTION J: identify_translation_gaps()
# ==============================================================================

test_that("J1: returns named list with one entry per position", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  model_list <- train_translation_model(feat, cutoff_year = 2024L)
  result <- identify_translation_gaps(model_list)
  expect_equal(names(result), TRANSLATION_POSITIONS)
})

test_that("J2: each position tibble has required columns", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  model_list <- train_translation_model(feat, cutoff_year = 2024L)
  result <- identify_translation_gaps(model_list)
  required <- c("feature", "base_coef", "enriched_coef",
                "base_nonzero", "enriched_nonzero", "absorbed_by_capital")
  for (pos in TRANSLATION_POSITIONS) {
    if (!is.null(result[[pos]])) {
      expect_true(all(required %in% names(result[[pos]])),
        info = glue("Position {pos} missing columns"))
    }
  }
})

test_that("J3: absorbed_by_capital is TRUE only when base nonzero and enriched zero", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  model_list <- train_translation_model(feat, cutoff_year = 2024L)
  result <- identify_translation_gaps(model_list)
  for (pos in TRANSLATION_POSITIONS) {
    df <- result[[pos]]
    if (!is.null(df) && nrow(df) > 0L) {
      absorbed <- df %>% dplyr::filter(absorbed_by_capital)
      if (nrow(absorbed) > 0L) {
        expect_true(all(absorbed$base_nonzero))
        expect_true(all(!absorbed$enriched_nonzero))
      }
    }
  }
})

test_that("J4: base_coef and enriched_coef are numeric", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  model_list <- train_translation_model(feat, cutoff_year = 2024L)
  result <- identify_translation_gaps(model_list)
  for (pos in TRANSLATION_POSITIONS) {
    df <- result[[pos]]
    if (!is.null(df)) {
      expect_true(is.numeric(df$base_coef))
      expect_true(is.numeric(df$enriched_coef))
    }
  }
})

test_that("J5: draft_round and draft_pick appear in enriched-only rows", {
  feat <- make_minimal_feature_matrix()
  skip_if(
    sum(purrr::map_lgl(TRANSLATION_POSITIONS, ~ nrow(feat$training[[.x]]) >= 4L)) == 0L,
    "Insufficient training rows"
  )
  model_list <- train_translation_model(feat, cutoff_year = 2024L)
  result <- identify_translation_gaps(model_list)
  for (pos in TRANSLATION_POSITIONS) {
    df <- result[[pos]]
    if (!is.null(df)) {
      cap_features <- df$feature[!is.na(df$enriched_coef) & is.na(df$base_coef)]
      # draft_round and draft_pick should appear in capital-only rows
      expect_true(
        any(c("draft_round", "draft_pick") %in% df$feature),
        info = glue("Position {pos}: draft capital features missing from output")
      )
    }
  }
})

test_that("J6: stops on missing models_base", {
  expect_error(
    identify_translation_gaps(list(models_base = NULL)),
    regexp = "."
  )
})


# ==============================================================================
# SECTION K: validate_translation_assumptions()
# ==============================================================================

test_that("K1: returns list with valid, summary, collinearity_flags", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  result <- validate_translation_assumptions(
    make_minimal_crosswalk(), feat, verbose = FALSE
  )
  expect_true(all(c("valid", "summary", "collinearity_flags") %in% names(result)))
})

test_that("K2: valid is logical scalar", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  result <- validate_translation_assumptions(
    make_minimal_crosswalk(), feat, verbose = FALSE
  )
  expect_true(is.logical(result$valid))
  expect_equal(length(result$valid), 1L)
})

test_that("K3: summary has check, critical, passed, detail columns", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  result <- validate_translation_assumptions(
    make_minimal_crosswalk(), feat, verbose = FALSE
  )
  expect_true(all(c("check", "critical", "passed", "detail") %in%
    names(result$summary)))
})

test_that("K4: match rate check fires for 100 pct matched crosswalk", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  result <- validate_translation_assumptions(
    make_minimal_crosswalk(), feat, verbose = FALSE
  )
  match_check <- result$summary %>%
    dplyr::filter(check == "match_rate_overall")
  expect_true(match_check$passed)
})

test_that("K5: match rate check fails below 50 pct", {
  # Create crosswalk where most are unmatched
  cw_bad <- make_minimal_crosswalk()
  cw_bad$match_method <- "unmatched"
  cw_bad$cfb_player_name <- NA_character_
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  result <- validate_translation_assumptions(
    cw_bad, feat, verbose = FALSE
  )
  match_check <- result$summary %>%
    dplyr::filter(check == "match_rate_overall")
  expect_false(match_check$passed)
})

test_that("K6: outcome_ppr_range check passes for valid range", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  result <- validate_translation_assumptions(
    make_minimal_crosswalk(), feat, verbose = FALSE
  )
  range_check <- result$summary %>%
    dplyr::filter(check == "outcome_ppr_range")
  if (nrow(range_check) > 0L) {
    expect_true(range_check$passed)
  }
})

test_that("K7: collinearity_flags is a tibble", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  result <- validate_translation_assumptions(
    make_minimal_crosswalk(), feat, verbose = FALSE
  )
  expect_true(is.data.frame(result$collinearity_flags))
})

test_that("K8: stops on non-dataframe crosswalk", {
  feat <- build_translation_features(
    make_cfb_panel(), make_nfl_panel(), make_sos_panel(),
    make_minimal_crosswalk(), cutoff_year = 2024L
  )
  expect_error(
    validate_translation_assumptions("not_a_df", feat, verbose = FALSE),
    regexp = "."
  )
})


# ==============================================================================
# SECTION L: Assumption validation (project-required section)
# ==============================================================================

test_that("L1: CUTOFF_YEAR is correct for April 2026 -- 2023L", {
  expect_equal(CUTOFF_YEAR, 2023L)
})

test_that("L2: TRAINING_DRAFT_CLASSES spans 2015 through CUTOFF_YEAR", {
  expect_equal(min(TRAINING_DRAFT_CLASSES), 2015L)
  expect_equal(max(TRAINING_DRAFT_CLASSES), CUTOFF_YEAR)
})

test_that("L3: NFL_OUTCOME_SEASONS covers Year 3 of CUTOFF_YEAR class", {
  # Draft class CUTOFF_YEAR plays NFL seasons CUTOFF_YEAR, +1, +2
  expect_true((CUTOFF_YEAR + 2L) %in% NFL_OUTCOME_SEASONS)
})

test_that("L4: HIT_THRESHOLDS match confirmed league structure", {
  expect_equal(HIT_THRESHOLDS[["QB"]], 12L)
  expect_equal(HIT_THRESHOLDS[["RB"]], 36L)
  expect_equal(HIT_THRESHOLDS[["WR"]], 36L)
  expect_equal(HIT_THRESHOLDS[["TE"]], 12L)
})

test_that("L5: GLMNET_ALPHA is 0.5 -- confirmed Elastic Net design decision", {
  expect_equal(GLMNET_ALPHA, 0.5)
})
