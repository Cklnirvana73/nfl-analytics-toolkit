# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 21 (R/41)
# Test suite: Tab 3 Trade Evaluator + Tab 4 Rookie Tracker, plus get_league_users
# File: tests/test_season2_week21_functions.R
#
# PURPOSE
# -------
# Unit tests for shiny_app/tabs/trade.R and shiny_app/tabs/rookie.R, plus the
# R/19 helper get_league_users(). Module servers are exercised with
# shiny::testServer() against synthetic fixtures in the Season 2 deterministic
# no-live-data style. The pure Sleeper helper is tested with a temporary stub of
# the internal .sleeper_get() (assign + on.exit, no extra package dependency).
#
# What is intentionally NOT covered:
#   - The live Sleeper calls inside the trade Load button and the rookie Player
#     League Lookup (network; exercised manually with a real username).
#   - evaluate_trade()'s internal arithmetic (covered by the week18 suite). The
#     trade tests here check only that the server wires the button to a result.
#   - The comps distribution plot rendering (its logic lives in comps(), tested).
#
# RUN
# ---
#   From the project root, in a fresh R session:
#     testthat::test_file(here::here("tests", "test_season2_week21_functions.R"))
# ==============================================================================

library(testthat)
library(shiny)
library(dplyr)
library(here)
library(glue)
library(tibble)
library(purrr)

# Dependencies under test. Sourced with project-root-anchored paths, because the
# test runs from the project root (not inside the shiny_app bundle). R/19 defines
# get_league_users(), .sleeper_get(), and the `%||%` helper the modules rely on.
source(here::here("R", "19_sleeper_api.R"))
source(here::here("shiny_app", "R", "trade_helpers.R"))
source(here::here("shiny_app", "tabs", "trade.R"))
source(here::here("shiny_app", "tabs", "rookie.R"))

# ------------------------------------------------------------------------------
# Synthetic fixtures
# ------------------------------------------------------------------------------

# Trade values: two leagues, mixed positions. Columns match the R/37
# build_trade_value_table() output that trade.R reads.
make_trade_values <- function() {
  tibble::tibble(
    league_name = c(rep("King of Kings", 4L), rep("Dynasty Warriors", 2L)),
    nfl_gsis_id = c("p1", "p2", "p3", "p4", "p5", "p6"),
    player_name = c("Player A", "Player B", "Player C", "Player D",
                    "Player E", "Player F"),
    position    = c("QB", "RB", "WR", "TE", "RB", "WR"),
    trade_value = c(90, 70, 50, 30, 60, 40),
    tv_tier     = c("Elite", "Great", "Good", "Solid", "Great", "Good")
  )
}

# Prospect scores: prediction cohort (current rookies) + training cohort
# (historical players with known outcomes). Eleven WR training rows carry a
# known is_hit; one WR training row has is_hit = NA and must be excluded from
# comps even though its score is the nearest of all.
make_prospects <- function() {
  prediction <- tibble::tibble(
    cfb_player_name = c("Rookie WR1", "Rookie WR2", "Rookie RB1"),
    position        = c("WR", "WR", "RB"),
    draft_year      = c(2026L, 2026L, 2026L),
    draft_round     = c(1L, 2L, 1L),
    draft_pick      = c(5L, 40L, 10L),
    draft_class_type = "prediction",
    score_final     = c(0.80, 0.60, 0.75),
    is_hit          = NA_real_,
    ppr_per_game_y13 = NA_real_
  )

  wr_scores <- c(0.82, 0.78, 0.85, 0.74, 0.88, 0.70, 0.90, 0.66, 0.79, 0.81, 0.60)
  wr_train <- tibble::tibble(
    cfb_player_name  = paste0("WR Train ", seq_along(wr_scores)),
    position         = "WR",
    draft_year       = 2015L + seq_along(wr_scores),
    draft_round      = 2L,
    draft_pick       = 50L,
    draft_class_type = "training",
    score_final      = wr_scores,
    is_hit           = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0),
    ppr_per_game_y13 = seq(6, 16, length.out = length(wr_scores))
  )

  wr_train_na <- tibble::tibble(
    cfb_player_name  = "WR Train NA",
    position         = "WR",
    draft_year       = 2014L,
    draft_round      = 3L,
    draft_pick       = 70L,
    draft_class_type = "training",
    score_final      = 0.805,          # nearest of all to Rookie WR1's 0.80
    is_hit           = NA_real_,        # unknown outcome -> excluded from comps
    ppr_per_game_y13 = NA_real_
  )

  rb_train <- tibble::tibble(
    cfb_player_name  = c("RB Train 1", "RB Train 2"),
    position         = "RB",
    draft_year       = c(2017L, 2018L),
    draft_round      = c(1L, 4L),
    draft_pick       = c(8L, 110L),
    draft_class_type = "training",
    score_final      = c(0.72, 0.55),
    is_hit           = c(1, 0),
    ppr_per_game_y13 = c(14, 7)
  )

  dplyr::bind_rows(prediction, wr_train, wr_train_na, rb_train)
}

# ==============================================================================
# tradeServer
# ==============================================================================

test_that("matched_leagues is empty before any leagues are loaded", {
  shiny::testServer(
    tradeServer,
    args = list(current_trade_values = shiny::reactive(make_trade_values()),
                app_season = 2026L),
    {
      expect_identical(matched_leagues(), character(0))
    }
  )
})

test_that("matched_leagues matches the cache case- and whitespace-insensitively", {
  shiny::testServer(
    tradeServer,
    args = list(current_trade_values = shiny::reactive(make_trade_values()),
                app_season = 2026L),
    {
      # The user's Sleeper leagues, spelled differently from the cache.
      rv$leagues <- tibble::tibble(
        name      = c("  king of kings ", "Some Other League"),
        league_id = c("L1", "L2")
      )
      out <- matched_leagues()
      # Returns the CACHE spelling, only for the league that matches.
      expect_identical(out, "King of Kings")
    }
  )
})

test_that("matched_leagues is empty when trade values are NULL", {
  shiny::testServer(
    tradeServer,
    args = list(current_trade_values = shiny::reactive(NULL),
                app_season = 2026L),
    {
      rv$leagues <- tibble::tibble(name = "King of Kings", league_id = "L1")
      expect_identical(matched_leagues(), character(0))
    }
  )
})

test_that("filtered_league_choices narrows by position; empty filter returns all", {
  shiny::testServer(
    tradeServer,
    args = list(current_trade_values = shiny::reactive(make_trade_values()),
                app_season = 2026L),
    {
      session$setInputs(league = "King of Kings")
      # No position filter set yet -> all four King of Kings players.
      all_choices <- filtered_league_choices()
      expect_equal(length(all_choices), 4L)
      expect_setequal(unname(all_choices), c("p1", "p2", "p3", "p4"))

      # Filter to RB -> only Player B.
      session$setInputs(pos_filter = "RB")
      rb_only <- filtered_league_choices()
      expect_equal(length(rb_only), 1L)
      expect_identical(unname(rb_only), "p2")
      expect_identical(names(rb_only), "Player B (RB)")
    }
  )
})

test_that("the evaluate button wires inputs through to a result with a verdict", {
  shiny::testServer(
    tradeServer,
    args = list(current_trade_values = shiny::reactive(make_trade_values()),
                app_season = 2026L),
    {
      session$setInputs(league = "King of Kings")
      session$setInputs(give = "p1", receive = c("p2", "p3"))
      session$setInputs(evaluate = 1)

      res <- result()
      expect_equal(res$give_value, 90)
      expect_equal(res$receive_value, 120)   # 70 + 50
      expect_equal(res$net_delta, 30)         # receive - give
      expect_identical(res$verdict, "WIN")    # 30 > TRADE_VERDICT_BAND
    }
  )
})

# ==============================================================================
# rookieServer
# ==============================================================================

test_that("rookies_all and training_all split the prospect cohorts", {
  shiny::testServer(
    rookieServer,
    args = list(current_prospects = shiny::reactive(make_prospects()),
                app_season = 2026L),
    {
      pred <- rookies_all()
      expect_true(all(pred$draft_class_type == "prediction"))
      expect_equal(nrow(pred), 3L)

      train <- training_all()
      expect_true(all(train$draft_class_type == "training"))
    }
  )
})

test_that("rookie_table_data filters by position and ranks within position", {
  shiny::testServer(
    rookieServer,
    args = list(current_prospects = shiny::reactive(make_prospects()),
                app_season = 2026L),
    {
      # No filter -> all prediction rows.
      full <- rookie_table_data()
      expect_equal(nrow(full), 3L)
      wr1 <- dplyr::filter(full, .data$cfb_player_name == "Rookie WR1")
      wr2 <- dplyr::filter(full, .data$cfb_player_name == "Rookie WR2")
      expect_equal(wr1$pos_rank, 1L)   # 0.80 is the top WR
      expect_equal(wr2$pos_rank, 2L)   # 0.60 is the second WR

      # Filter to WR -> only the two WR rookies.
      session$setInputs(pos_filter = "WR")
      wr_only <- rookie_table_data()
      expect_equal(nrow(wr_only), 2L)
      expect_true(all(wr_only$position == "WR"))
    }
  )
})

test_that("selected_rookie maps the row, and comps returns nearest known outcomes", {
  shiny::testServer(
    rookieServer,
    args = list(current_prospects = shiny::reactive(make_prospects()),
                app_season = 2026L),
    {
      # rookie_table_data is arranged desc score_final overall: WR1 (0.80) is row 1.
      session$setInputs(rookie_tbl_rows_selected = 1L)
      r <- selected_rookie()
      expect_equal(nrow(r), 1L)
      expect_identical(r$cfb_player_name[1], "Rookie WR1")

      cp <- comps()
      expect_equal(nrow(cp), 10L)                     # capped at ten
      expect_true(all(cp$position == "WR"))           # same position only
      expect_true(all(!is.na(cp$is_hit)))             # known outcomes only
      # The nearest-by-score row has an unknown outcome and must be excluded.
      expect_false("WR Train NA" %in% cp$cfb_player_name)
    }
  )
})

test_that("the league lookup rejects a blank player or username without a live call", {
  shiny::testServer(
    rookieServer,
    args = list(current_prospects = shiny::reactive(make_prospects()),
                app_season = 2026L),
    {
      # Blank name, non-blank user -> early return, no Sleeper call.
      # The lookup observer is observeEvent(..., ignoreInit = TRUE), which skips
      # its first invocation; in testServer that first set is the skipped init,
      # so we set the button twice to land a genuine post-init change that fires.
      session$setInputs(lookup_name = "", lookup_user = "someuser")
      session$setInputs(lookup_search = 1)
      session$setInputs(lookup_search = 2)
      expect_null(lk$result)
      expect_identical(lk$msg,
                       "Enter both a player name and your Sleeper username.")
      expect_identical(lk$msg_class, "alert alert-warning")
    }
  )
})

# ==============================================================================
# get_league_users (R/19) -- pure parse, .sleeper_get stubbed
# ==============================================================================

test_that("get_league_users returns the empty 3-column tibble on a NULL response", {
  env <- environment(get_league_users)
  orig <- get(".sleeper_get", envir = env)
  assign(".sleeper_get", function(endpoint, ...) NULL, envir = env)
  on.exit(assign(".sleeper_get", orig, envir = env), add = TRUE)

  expect_warning(out <- get_league_users("123"))
  expect_identical(names(out),
                   c("owner_id", "team_name", "display_name"))
  expect_equal(nrow(out), 0L)
})

test_that("get_league_users parses users, falling back to display_name for team", {
  env <- environment(get_league_users)
  orig <- get(".sleeper_get", envir = env)
  fixture <- list(
    list(user_id = "u1", display_name = "alice",
         metadata = list(team_name = "Alice's Squad")),
    list(user_id = "u2", display_name = "bob",
         metadata = list(team_name = "")),       # empty -> fall back to display
    list(user_id = "u3", display_name = "carol")  # no metadata -> fall back
  )
  assign(".sleeper_get", function(endpoint, ...) fixture, envir = env)
  on.exit(assign(".sleeper_get", orig, envir = env), add = TRUE)

  out <- get_league_users("123")
  expect_equal(nrow(out), 3L)
  expect_identical(out$owner_id, c("u1", "u2", "u3"))
  expect_identical(out$display_name, c("alice", "bob", "carol"))
  expect_identical(out$team_name, c("Alice's Squad", "bob", "carol"))
})
