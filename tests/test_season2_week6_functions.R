# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 6
# Test Suite: Multi-Season CFB Play-by-Play Functions
# File: tests/test_season2_week6_functions.R
#
# Tests: normalize_cfb_schema, .filter_cfb_by_division,
#        load_multi_season_cfb_pbp (input validation only),
#        validate_cfb_season_coverage, get_cfb_schema_differences,
#        load_normalized_cfb_season
#
# No network calls. All tests use synthetic fixtures written to tempdir().
#
# Run: testthat::test_file(here::here("tests", "test_season2_week6_functions.R"))
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)
library(glue)
library(here)

source(here::here("R", "20_multi_season_cfb_pbp.R"))


# ==============================================================================
# FIXTURES
# ==============================================================================

# Single-game CFB PBP fixture. Plays alternate pos_team between home and away
# so both branches of .filter_cfb_by_division() are exercised.
make_cfb_pbp <- function(n            = 20L,
                          season       = 2024L,
                          home_team    = "Alabama",
                          away_team    = "Georgia",
                          home_div     = "fbs",
                          away_div     = "fbs") {
  stopifnot(n >= 2L)
  tibble::tibble(
    game_id            = rep(1L, n),
    season             = rep(as.integer(season), n),
    week               = rep(1L, n),
    season_type        = rep("regular", n),
    pos_team           = rep(c(home_team, away_team), length.out = n),
    def_pos_team       = rep(c(away_team, home_team), length.out = n),
    home               = rep(home_team, n),
    away               = rep(away_team, n),
    home_team_division = rep(home_div, n),
    away_team_division = rep(away_div, n),
    play_type          = rep("Rush", n),
    play_text          = rep("test run", n),
    yards_gained       = rep(5L, n),
    down               = rep(1L, n),
    distance           = rep(10L, n),
    id_play            = seq_len(n),
    EPA                = rep(0.1, n)
  )
}

# Multi-team, multi-game fixture for validate_cfb_season_coverage.
# Each of n_teams teams appears as pos_team = home in n_games_per_team
# distinct game_ids. All plays for a given team are in that team's "home"
# games so the FBS filter (Strategy C) retains them when home_div = "fbs".
make_cfb_season_pbp <- function(n_teams          = 128L,
                                 n_games_per_team = 12L,
                                 plays_per_game   = 25L,
                                 season           = 2024L,
                                 home_div         = "fbs",
                                 away_div         = "fbs") {
  teams <- paste0("Team", formatC(seq_len(n_teams), width = 3L, flag = "0"))

  rows <- lapply(seq_len(n_teams), function(t) {
    home_team <- teams[t]
    away_team <- teams[(t %% n_teams) + 1L]

    lapply(seq_len(n_games_per_team), function(g) {
      gid <- as.integer((t - 1L) * n_games_per_team + g)
      tibble::tibble(
        game_id            = rep(gid, plays_per_game),
        season             = rep(as.integer(season), plays_per_game),
        week               = rep(as.integer(g), plays_per_game),
        season_type        = rep("regular", plays_per_game),
        pos_team           = rep(home_team, plays_per_game),
        def_pos_team       = rep(away_team, plays_per_game),
        home               = rep(home_team, plays_per_game),
        away               = rep(away_team, plays_per_game),
        home_team_division = rep(home_div, plays_per_game),
        away_team_division = rep(away_div, plays_per_game),
        play_type          = rep("Rush", plays_per_game),
        play_text          = rep("test run", plays_per_game),
        yards_gained       = rep(5L, plays_per_game),
        down               = rep(1L, plays_per_game),
        distance           = rep(10L, plays_per_game),
        id_play            = seq_len(plays_per_game),
        EPA                = rep(0.1, plays_per_game)
      )
    })
  })

  dplyr::bind_rows(unlist(rows, recursive = FALSE))
}

# Write a synthetic season RDS file to cache_dir. Returns the file path.
write_cfb_cache <- function(cache_dir, season, pbp) {
  cache_file <- file.path(cache_dir,
                          glue("cfb_pbp_normalized_{season}.rds"))
  saveRDS(pbp, cache_file)
  invisible(cache_file)
}


# ==============================================================================
# SECTION 1: normalize_cfb_schema
# ==============================================================================

test_that("normalize_cfb_schema: errors on non-data-frame input", {
  expect_error(normalize_cfb_schema("not_a_df", season = 2024L),
               regexp = "data frame")
})

test_that("normalize_cfb_schema: errors on 0-row input", {
  empty <- make_cfb_pbp(n = 2L)[0, ]
  expect_error(normalize_cfb_schema(empty, season = 2024L),
               regexp = "empty")
})

test_that("normalize_cfb_schema: errors on non-numeric season", {
  pbp <- make_cfb_pbp()
  expect_error(normalize_cfb_schema(pbp, season = "2024"),
               regexp = "single integer")
})

test_that("normalize_cfb_schema: errors on vector season", {
  pbp <- make_cfb_pbp()
  expect_error(normalize_cfb_schema(pbp, season = c(2024L, 2023L)),
               regexp = "single integer")
})

test_that("normalize_cfb_schema: creates posteam alias from pos_team", {
  pbp  <- make_cfb_pbp()
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  expect_true("posteam" %in% names(norm))
  expect_equal(norm$posteam, norm$pos_team)
})

test_that("normalize_cfb_schema: creates defteam alias from def_pos_team", {
  pbp  <- make_cfb_pbp()
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  expect_true("defteam" %in% names(norm))
  expect_equal(norm$defteam, norm$def_pos_team)
})

test_that("normalize_cfb_schema: creates epa alias from EPA", {
  pbp       <- make_cfb_pbp()
  # Rename EPA to uppercase to simulate cfbfastR column name
  names(pbp)[names(pbp) == "EPA"] <- "EPA"
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  expect_true("epa" %in% names(norm))
  expect_equal(norm$epa, norm$EPA)
})

test_that("normalize_cfb_schema: does not create alias when source column absent", {
  pbp <- make_cfb_pbp()
  pbp$pos_team <- NULL  # remove source column
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  expect_false("posteam" %in% names(norm))
})

test_that("normalize_cfb_schema: does not overwrite existing alias column", {
  pbp             <- make_cfb_pbp()
  pbp$posteam     <- "EXISTING"
  norm            <- normalize_cfb_schema(pbp, season = 2024L)
  expect_equal(norm$posteam[1], "EXISTING")
})

test_that("normalize_cfb_schema: adds missing optional columns as NA", {
  pbp  <- make_cfb_pbp()
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  for (col in CFB_OPTIONAL_COLUMNS) {
    if (!(col %in% names(make_cfb_pbp()))) {
      expect_true(col %in% names(norm),
                  label = glue("optional column '{col}' should be added"))
    }
  }
})

test_that("normalize_cfb_schema: sets schema_norm_version attribute", {
  pbp  <- make_cfb_pbp()
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  expect_equal(attr(norm, "schema_norm_version"), "s2cfbv1")
})

test_that("normalize_cfb_schema: sets season attribute", {
  pbp  <- make_cfb_pbp()
  norm <- normalize_cfb_schema(pbp, season = 2022L)
  expect_equal(attr(norm, "season"), 2022L)
})

test_that("normalize_cfb_schema: coerces season column to integer", {
  pbp        <- make_cfb_pbp()
  pbp$season <- as.numeric(pbp$season)  # simulate non-integer
  norm       <- normalize_cfb_schema(pbp, season = 2024L)
  expect_true(is.integer(norm$season))
})


# ==============================================================================
# SECTION 2: .filter_cfb_by_division
# ==============================================================================

test_that(".filter_cfb_by_division: division = 'all' returns input unchanged", {
  pbp      <- make_cfb_pbp(n = 20L)
  filtered <- .filter_cfb_by_division(pbp, division = "all")
  expect_equal(nrow(filtered), nrow(pbp))
  expect_equal(names(filtered), names(pbp))
})

test_that(".filter_cfb_by_division: fbs filter keeps only FBS offensive plays", {
  # Home = FBS, Away = FBS. All 20 plays kept since both teams are FBS.
  pbp      <- make_cfb_pbp(n = 20L, home_div = "fbs", away_div = "fbs")
  filtered <- .filter_cfb_by_division(pbp, division = "fbs")
  expect_equal(nrow(filtered), 20L)
  expect_true(all(unique(filtered$pos_team) %in% c("Alabama", "Georgia")))
})

test_that(".filter_cfb_by_division: fbs filter excludes FCS offensive plays", {
  # Home = FCS, Away = FBS. Plays alternate pos_team.
  # FBS filter: away plays (pos_team = "Alabama") kept; home plays dropped.
  pbp      <- make_cfb_pbp(n = 20L,
                             home_team = "NDState", away_team = "Alabama",
                             home_div  = "fcs",     away_div  = "fbs")
  filtered <- .filter_cfb_by_division(pbp, division = "fbs")
  # pos_team alternates: NDState, Alabama, NDState, Alabama...
  # FBS keeps only Alabama (away_div = "fbs" and pos_team == away)
  expect_true(nrow(filtered) < nrow(pbp))
  expect_true(all(filtered$pos_team == "Alabama"))
})

test_that(".filter_cfb_by_division: fcs filter keeps only FCS offensive plays", {
  # Home = FCS, Away = FBS. FCS filter keeps home (NDState) plays only.
  pbp      <- make_cfb_pbp(n = 20L,
                             home_team = "NDState", away_team = "Alabama",
                             home_div  = "fcs",     away_div  = "fbs")
  filtered <- .filter_cfb_by_division(pbp, division = "fcs")
  expect_true(nrow(filtered) < nrow(pbp))
  expect_true(all(filtered$pos_team == "NDState"))
})

test_that(".filter_cfb_by_division: fbs and fcs filtered counts sum to <= total", {
  # With mixed divisions, fbs + fcs <= total (some plays may match neither
  # if division data is NA, but in clean data they should sum to total)
  pbp     <- make_cfb_pbp(n = 20L, home_div = "fbs", away_div = "fcs")
  n_fbs   <- nrow(.filter_cfb_by_division(pbp, "fbs"))
  n_fcs   <- nrow(.filter_cfb_by_division(pbp, "fcs"))
  n_total <- nrow(pbp)
  expect_lte(n_fbs + n_fcs, n_total)
})

test_that(".filter_cfb_by_division: missing required column triggers warning and returns unfiltered", {
  pbp          <- make_cfb_pbp(n = 10L)
  pbp$home     <- NULL   # remove required column
  expect_warning(
    result <- .filter_cfb_by_division(pbp, division = "fbs"),
    regexp = "missing columns"
  )
  expect_equal(nrow(result), 10L)  # unfiltered returned
})


# ==============================================================================
# SECTION 3: load_multi_season_cfb_pbp input validation
# (no network calls -- only testing argument guards)
# ==============================================================================

test_that("load_multi_season_cfb_pbp: errors on non-numeric seasons", {
  expect_error(load_multi_season_cfb_pbp(seasons = "2024"),
               regexp = "non-empty numeric")
})

test_that("load_multi_season_cfb_pbp: errors on empty seasons vector", {
  expect_error(load_multi_season_cfb_pbp(seasons = integer(0)),
               regexp = "non-empty numeric")
})

test_that("load_multi_season_cfb_pbp: errors on season below CFB_SEASON_MIN", {
  expect_error(load_multi_season_cfb_pbp(seasons = 2013L,
                                          cache_dir = tempdir()),
               regexp = "2014")
})

test_that("load_multi_season_cfb_pbp: errors on season well above CFB_SEASON_MAX", {
  expect_error(load_multi_season_cfb_pbp(seasons = 2099L,
                                          cache_dir = tempdir()),
               regexp = "2099")
})

test_that("load_multi_season_cfb_pbp: errors on non-logical force_reload", {
  expect_error(load_multi_season_cfb_pbp(seasons = 2024L,
                                          cache_dir = tempdir(),
                                          force_reload = "yes"),
               regexp = "logical")
})

test_that("load_multi_season_cfb_pbp: cached season returns status = 'cached'", {
  cache_dir <- tempfile("cfb_test_cache")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # Pre-populate cache with synthetic data
  pbp  <- make_cfb_pbp(n = 20L)
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  write_cfb_cache(cache_dir, 2024L, norm)

  result <- load_multi_season_cfb_pbp(seasons = 2024L, cache_dir = cache_dir,
                                       verbose = FALSE)
  expect_equal(result$status, "cached")
  expect_equal(result$season, 2024L)
  expect_equal(result$n_plays, nrow(norm))
})


# ==============================================================================
# SECTION 4: validate_cfb_season_coverage
# ==============================================================================

test_that("validate_cfb_season_coverage: passing season returns all_pass = TRUE", {
  cache_dir <- tempfile("cfb_validate_pass")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # 128 FBS teams, 12 games, 25 plays = 38,400 rows. Passes all thresholds.
  pbp  <- make_cfb_season_pbp(n_teams = 128L, n_games_per_team = 12L,
                               plays_per_game = 25L, season = 2024L)
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  write_cfb_cache(cache_dir, 2024L, norm)

  coverage <- validate_cfb_season_coverage(seasons = 2024L,
                                            cache_dir = cache_dir,
                                            division = "fbs")

  expect_equal(nrow(coverage), 1L)
  expect_true(coverage$all_pass[1])
  expect_equal(coverage$failures[1], "")
})

test_that("validate_cfb_season_coverage: play count below threshold fails", {
  cache_dir <- tempfile("cfb_validate_plays")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # 128 teams, 12 games, 5 plays = 7,680 rows -- below 30k threshold
  pbp  <- make_cfb_season_pbp(n_teams = 128L, n_games_per_team = 12L,
                               plays_per_game = 5L, season = 2024L)
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  write_cfb_cache(cache_dir, 2024L, norm)

  coverage <- validate_cfb_season_coverage(seasons = 2024L,
                                            cache_dir = cache_dir,
                                            division = "fbs")

  expect_false(coverage$all_pass[1])
  expect_true(grepl("play_count_too_low", coverage$failures[1]))
})

test_that("validate_cfb_season_coverage: team count below threshold fails (fbs)", {
  cache_dir <- tempfile("cfb_validate_teams")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # Only 100 teams -- below CFB_FBS_TEAMS_MIN (115). Use enough plays per game
  # to clear the 30k row threshold: 100 * 12 * 26 = 31,200.
  pbp  <- make_cfb_season_pbp(n_teams = 100L, n_games_per_team = 12L,
                               plays_per_game = 26L, season = 2024L)
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  write_cfb_cache(cache_dir, 2024L, norm)

  coverage <- validate_cfb_season_coverage(seasons = 2024L,
                                            cache_dir = cache_dir,
                                            division = "fbs")

  expect_false(coverage$all_pass[1])
  expect_true(grepl("team_count_too_low", coverage$failures[1]))
})

test_that("validate_cfb_season_coverage: 2020 with mean ~8 games passes COVID floor", {
  cache_dir <- tempfile("cfb_validate_covid")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # 128 teams, 8 games, 30 plays = 30,720 rows.
  # mean_games_per_team = 8: above COVID floor (6), below normal floor (10).
  pbp  <- make_cfb_season_pbp(n_teams = 128L, n_games_per_team = 8L,
                               plays_per_game = 30L, season = 2020L)
  norm <- normalize_cfb_schema(pbp, season = 2020L)
  write_cfb_cache(cache_dir, 2020L, norm)

  coverage <- validate_cfb_season_coverage(seasons = 2020L,
                                            cache_dir = cache_dir,
                                            division = "fbs")

  expect_true(coverage$all_pass[1],
              label = "2020 with 8 games should pass COVID floor of 6")
  expect_equal(coverage$failures[1], "")
})

test_that("validate_cfb_season_coverage: non-2020 season with mean 8 games fails", {
  cache_dir <- tempfile("cfb_validate_non_covid")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # Same structure as COVID test but season = 2021 -- uses normal floor of 10.
  pbp  <- make_cfb_season_pbp(n_teams = 128L, n_games_per_team = 8L,
                               plays_per_game = 30L, season = 2021L)
  norm <- normalize_cfb_schema(pbp, season = 2021L)
  write_cfb_cache(cache_dir, 2021L, norm)

  coverage <- validate_cfb_season_coverage(seasons = 2021L,
                                            cache_dir = cache_dir,
                                            division = "fbs")

  expect_false(coverage$all_pass[1],
               label = "2021 with 8 games should fail normal floor of 10")
  expect_true(grepl("mean_games_per_team_low", coverage$failures[1]))
})

test_that("validate_cfb_season_coverage: output tibble contains division column", {
  cache_dir <- tempfile("cfb_validate_division_col")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  pbp  <- make_cfb_season_pbp(n_teams = 128L, n_games_per_team = 12L,
                               plays_per_game = 25L, season = 2023L)
  norm <- normalize_cfb_schema(pbp, season = 2023L)
  write_cfb_cache(cache_dir, 2023L, norm)

  coverage <- validate_cfb_season_coverage(seasons = 2023L,
                                            cache_dir = cache_dir,
                                            division = "fbs")
  expect_true("division" %in% names(coverage))
  expect_equal(coverage$division[1], "fbs")
})

test_that("validate_cfb_season_coverage: errors when no cached files found", {
  empty_dir <- tempfile("cfb_empty")
  dir.create(empty_dir)
  on.exit(unlink(empty_dir, recursive = TRUE))

  expect_error(validate_cfb_season_coverage(cache_dir = empty_dir),
               regexp = "No cached CFB seasons")
})


# ==============================================================================
# SECTION 5: get_cfb_schema_differences
# ==============================================================================

test_that("get_cfb_schema_differences: universal column has pct_seasons_present = 1", {
  cache_dir <- tempfile("cfb_schema_diff")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # Both seasons have the same columns
  for (s in c(2023L, 2024L)) {
    pbp  <- make_cfb_pbp(n = 10L, season = s)
    norm <- normalize_cfb_schema(pbp, season = s)
    write_cfb_cache(cache_dir, s, norm)
  }

  diffs <- get_cfb_schema_differences(cache_dir = cache_dir)

  expect_true(nrow(diffs) > 0)
  # game_id is in both seasons so pct_seasons_present = 1
  game_id_row <- diffs[diffs$column == "game_id", ]
  expect_equal(nrow(game_id_row), 1L)
  expect_equal(game_id_row$pct_seasons_present, 1)
})

test_that("get_cfb_schema_differences: column absent from one season has pct < 1", {
  cache_dir <- tempfile("cfb_schema_partial")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # Season 2023: standard columns
  pbp_2023  <- make_cfb_pbp(n = 10L, season = 2023L)
  norm_2023 <- normalize_cfb_schema(pbp_2023, season = 2023L)
  write_cfb_cache(cache_dir, 2023L, norm_2023)

  # Season 2024: add an extra column absent from 2023
  pbp_2024           <- make_cfb_pbp(n = 10L, season = 2024L)
  pbp_2024$new_col   <- "only_in_2024"
  norm_2024          <- normalize_cfb_schema(pbp_2024, season = 2024L)
  write_cfb_cache(cache_dir, 2024L, norm_2024)

  diffs <- get_cfb_schema_differences(cache_dir = cache_dir)

  new_col_row <- diffs[diffs$column == "new_col", ]
  expect_equal(nrow(new_col_row), 1L)
  expect_lt(new_col_row$pct_seasons_present, 1)
})

test_that("get_cfb_schema_differences: output contains required summary columns", {
  cache_dir <- tempfile("cfb_schema_cols")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  pbp  <- make_cfb_pbp(n = 10L, season = 2024L)
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  write_cfb_cache(cache_dir, 2024L, norm)

  diffs <- get_cfb_schema_differences(cache_dir = cache_dir)

  expect_true(all(c("column", "n_seasons_present", "pct_seasons_present") %in%
                    names(diffs)))
})

test_that("get_cfb_schema_differences: errors when no cached files found", {
  empty_dir <- tempfile("cfb_diff_empty")
  dir.create(empty_dir)
  on.exit(unlink(empty_dir, recursive = TRUE))

  expect_error(get_cfb_schema_differences(cache_dir = empty_dir),
               regexp = "No cached CFB seasons")
})


# ==============================================================================
# SECTION 6: load_normalized_cfb_season
# ==============================================================================

test_that("load_normalized_cfb_season: errors on non-numeric season", {
  expect_error(load_normalized_cfb_season("2024"),
               regexp = "single integer")
})

test_that("load_normalized_cfb_season: errors on vector season", {
  expect_error(load_normalized_cfb_season(c(2024L, 2023L)),
               regexp = "single integer")
})

test_that("load_normalized_cfb_season: errors when cache file does not exist", {
  empty_dir <- tempfile("cfb_load_missing")
  dir.create(empty_dir)
  on.exit(unlink(empty_dir, recursive = TRUE))

  expect_error(load_normalized_cfb_season(2024L, cache_dir = empty_dir),
               regexp = "not cached")
})

test_that("load_normalized_cfb_season: division = 'all' returns all rows", {
  cache_dir <- tempfile("cfb_load_all")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  pbp  <- make_cfb_pbp(n = 20L)
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  write_cfb_cache(cache_dir, 2024L, norm)

  result <- load_normalized_cfb_season(2024L, cache_dir = cache_dir,
                                        division = "all")
  expect_equal(nrow(result), 20L)
})

test_that("load_normalized_cfb_season: fbs filter reduces row count for mixed-division data", {
  cache_dir <- tempfile("cfb_load_fbs")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # Home = FCS, Away = FBS. FBS filter keeps only away plays (~half).
  pbp  <- make_cfb_pbp(n = 20L, home_team = "NDState", away_team = "Alabama",
                        home_div = "fcs", away_div = "fbs")
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  write_cfb_cache(cache_dir, 2024L, norm)

  result_all <- load_normalized_cfb_season(2024L, cache_dir = cache_dir,
                                            division = "all")
  result_fbs <- load_normalized_cfb_season(2024L, cache_dir = cache_dir,
                                            division = "fbs")

  expect_lt(nrow(result_fbs), nrow(result_all))
  expect_true(all(result_fbs$pos_team == "Alabama"))
})

test_that("load_normalized_cfb_season: preserves schema_norm_version attribute", {
  cache_dir <- tempfile("cfb_load_attr")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  pbp  <- make_cfb_pbp(n = 10L)
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  write_cfb_cache(cache_dir, 2024L, norm)

  result <- load_normalized_cfb_season(2024L, cache_dir = cache_dir,
                                        division = "all")
  expect_equal(attr(result, "schema_norm_version"), "s2cfbv1")
})

test_that("load_normalized_cfb_season: sets division_filter attribute", {
  cache_dir <- tempfile("cfb_load_divattr")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  pbp  <- make_cfb_pbp(n = 10L)
  norm <- normalize_cfb_schema(pbp, season = 2024L)
  write_cfb_cache(cache_dir, 2024L, norm)

  result_fbs <- load_normalized_cfb_season(2024L, cache_dir = cache_dir,
                                            division = "fbs")
  expect_equal(attr(result_fbs, "division_filter"), "fbs")

  result_all <- load_normalized_cfb_season(2024L, cache_dir = cache_dir,
                                            division = "all")
  expect_equal(attr(result_all, "division_filter"), "all")
})
