# Shared setup for scoring-grain tests (added 2026-09-17).
suppressMessages({
  library(dplyr)
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  source(file.path(root, "R", "05_consistency_metrics.R"))
  source(file.path(root, "R", "17_extended_scoring.R"))
})

fixture_pbp    <- function() readRDS(testthat::test_path("fixtures", "pbp_fixture.rds"))
fixture_roster <- function() readRDS(testthat::test_path("fixtures", "roster_fixture.rds"))

quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# Perturb name spellings on every other play for every player, in every role
# column, so any residual name-keyed grouping would split rows.
perturb_names <- function(pbp) {
  idx <- seq_len(nrow(pbp)) %% 2 == 0
  for (col in c("passer_player_name", "rusher_player_name", "receiver_player_name")) {
    v <- pbp[[col]]
    v[idx & !is.na(v)] <- paste0(v[idx & !is.na(v)], "_ALT")
    pbp[[col]] <- v
  }
  pbp
}

COUNT_COLS_05 <- c("pass_yards", "pass_tds", "pass_ints", "pick6", "rush_yards",
                   "rush_tds", "rush_attempts", "rec_yards", "rec_tds",
                   "receptions", "two_point_conversions")
COUNT_COLS_17 <- c("pass_yards", "pass_tds", "pass_ints", "sacks_taken",
                   "rush_yards", "rush_tds", "rush_attempts", "rush_first_downs",
                   "rec_yards", "rec_tds", "receptions", "targets",
                   "rec_first_downs", "two_point_successes")
KEY <- c("season", "week", "game_id", "player_id")
