# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 18
# Test suite: Shiny architecture and data layer
# File: tests/test_season2_week18_functions.R
#
# COVERS
# ------
#   load_shiny_data()  (shiny_app/R/data_layer.R)
#     - returns all six named slots regardless of which cache files exist
#     - missing file -> slot is NULL but PRESERVED (regression for the
#       out[[nm]] <- NULL slot-removal bug)
#     - present file -> slot is the loaded data frame
#     - does not error when files are missing (graceful degradation)
#
#   evaluate_trade()   (shiny_app/R/trade_helpers.R)
#     - give/receive value sums and net_delta
#     - verdict thresholds against TRADE_VERDICT_BAND (WIN / LOSS / EVEN)
#     - league filtering and NULL-league default
#     - missing player IDs warned and excluded, not fatal
#     - error when target league has no rows
#     - return structure and detail-tibble columns
#
# RUN
# ---
#   testthat::test_file(here::here("tests", "test_season2_week18_functions.R"))
#
# NOTES
# -----
#   Synthetic fixtures only -- no live Sleeper or nflfastR calls.
#   load_shiny_data() path tests use manual assign() + on.exit() mocking
#   targeting globalenv(), per the documented project pattern (testthat
#   local_mocked_bindings() requires a package context this project lacks).
# ==============================================================================

library(testthat)
library(dplyr)
library(glue)
library(tibble)

# Helpers are pure and sourceable in isolation; no app launch is triggered.
source(here::here("shiny_app", "R", "data_layer.R"))
source(here::here("shiny_app", "R", "trade_helpers.R"))

# ------------------------------------------------------------------------------
# FIXTURE
# ------------------------------------------------------------------------------

#' Minimal synthetic trade_values table spanning two leagues
#'
#' League A and League B both contain player "00-001" at different values, so
#' tests can confirm league scoping picks the right row.
make_trade_values <- function() {
  tibble::tibble(
    league_name = c(rep("League A", 4), rep("League B", 2)),
    nfl_gsis_id = c("00-001", "00-002", "00-003", "00-004", "00-001", "00-005"),
    player_name = c("Alpha", "Bravo", "Charlie", "Delta", "Alpha", "Echo"),
    position    = c("RB", "WR", "QB", "TE", "RB", "WR"),
    trade_value = c(100, 80, 60, 40, 50, 30),
    tv_tier     = c("Elite", "Tier 1", "Tier 2", "Depth", "Tier 2", "Depth")
  )
}

# ==============================================================================
# evaluate_trade()
# ==============================================================================

test_that("give and receive values sum correctly and net_delta is right", {
  tv  <- make_trade_values()
  res <- evaluate_trade(give_ids = "00-001",
                        receive_ids = c("00-002", "00-003"),
                        trade_values = tv,
                        league_name = "League A")
  expect_equal(res$give_value, 100)
  expect_equal(res$receive_value, 140)   # 80 + 60
  expect_equal(res$net_delta, 40)        # 140 - 100
})

test_that("verdict is WIN when receiving more value", {
  tv  <- make_trade_values()
  res <- evaluate_trade("00-001", c("00-002", "00-003"), tv, "League A")
  expect_equal(res$verdict, "WIN")
})

test_that("verdict is LOSS when giving more value", {
  tv  <- make_trade_values()
  res <- evaluate_trade(c("00-002", "00-003"), "00-001", tv, "League A")
  expect_equal(res$net_delta, -40)
  expect_equal(res$verdict, "LOSS")
})

test_that("verdict is ROUGHLY EVEN when net_delta is within the band", {
  tv  <- make_trade_values()
  # give Charlie + Delta (60 + 40 = 100), receive Alpha (100) -> net 0
  res <- evaluate_trade(c("00-003", "00-004"), "00-001", tv, "League A")
  expect_equal(res$net_delta, 0)
  expect_equal(res$verdict, "ROUGHLY EVEN")
})

test_that("verdict respects the TRADE_VERDICT_BAND boundary precisely", {
  tv <- tibble::tibble(
    league_name = "L",
    nfl_gsis_id = c("g", "r"),
    player_name = c("G", "R"),
    position    = c("RB", "WR"),
    trade_value = c(10.0, 10.4),   # net_delta = 0.4, inside the 0.5 band
    tv_tier     = c("a", "b")
  )
  expect_equal(evaluate_trade("g", "r", tv)$verdict, "ROUGHLY EVEN")

  tv$trade_value <- c(10.0, 10.6)  # net_delta = 0.6, above the band
  expect_equal(evaluate_trade("g", "r", tv)$verdict, "WIN")
})

test_that("league_name = NULL defaults to the first league in the table", {
  tv  <- make_trade_values()
  # Alpha is 100 in League A (first) and 50 in League B. NULL must use League A.
  res <- evaluate_trade("00-001", character(0), tv, league_name = NULL)
  expect_equal(res$give_value, 100)
})

test_that("league filtering uses the target league's values, not another's", {
  tv  <- make_trade_values()
  # In League B, Alpha is worth 50, Echo 30.
  res <- evaluate_trade("00-001", "00-005", tv, league_name = "League B")
  expect_equal(res$give_value, 50)
  expect_equal(res$receive_value, 30)
  expect_equal(res$verdict, "LOSS")
})

test_that("unknown player IDs warn and are excluded, not fatal", {
  tv <- make_trade_values()
  expect_warning(
    res <- evaluate_trade(c("00-001", "99-999"), "00-002", tv, "League A"),
    regexp = "not found"
  )
  # 99-999 excluded; give_value is Alpha only.
  expect_equal(res$give_value, 100)
})

test_that("error is raised when target league has no rows", {
  tv <- make_trade_values()
  expect_error(
    evaluate_trade("00-001", "00-002", tv, league_name = "Nonexistent"),
    regexp = "No trade values found"
  )
})

test_that("return object has the expected named structure", {
  tv  <- make_trade_values()
  res <- evaluate_trade("00-001", "00-002", tv, "League A")
  expect_named(
    res,
    c("give_value", "receive_value", "net_delta",
      "give_detail", "receive_detail", "verdict"),
    ignore.order = TRUE
  )
  expect_type(res$give_value, "double")
  expect_type(res$verdict, "character")
})

test_that("detail tibbles carry the expected columns", {
  tv  <- make_trade_values()
  res <- evaluate_trade("00-001", "00-002", tv, "League A")
  expected_cols <- c("player_name", "position", "trade_value", "tv_tier")
  expect_true(all(expected_cols %in% names(res$give_detail)),
              info = paste("Missing:",
                           setdiff(expected_cols, names(res$give_detail))))
  expect_s3_class(res$give_detail, "tbl_df")
})

# ==============================================================================
# load_shiny_data()
# ==============================================================================

test_that("load_shiny_data returns all six named slots", {
  result <- suppressWarnings(load_shiny_data())
  expect_length(result, 6)
  expect_named(
    result,
    c("vorp", "start_sit_uq", "def_streaming",
      "optimal_lineup", "prospects", "trade_values"),
    ignore.order = TRUE
  )
})

test_that("load_shiny_data does not error when cache files are missing", {
  expect_no_error(suppressWarnings(load_shiny_data()))
})

test_that("every slot is either NULL or a data frame", {
  result <- suppressWarnings(load_shiny_data())
  for (nm in names(result)) {
    slot <- result[[nm]]
    expect_true(is.null(slot) || is.data.frame(slot),
                info = paste("Slot", nm, "is neither NULL nor a data frame"))
  }
})

test_that("a missing file yields a PRESERVED NULL slot (regression)", {
  # Force one path to a guaranteed-missing file and confirm its slot survives
  # as NULL rather than being dropped from the list. This is the regression
  # guard for the out[[nm]] <- NULL bug (which would shrink the list to 5).
  missing_path <- file.path(tempdir(), "definitely_missing_week18_xyz.rds")
  old <- PATH_TRADE_VALUES
  assign("PATH_TRADE_VALUES", missing_path, envir = globalenv())
  on.exit(assign("PATH_TRADE_VALUES", old, envir = globalenv()), add = TRUE)

  result <- suppressWarnings(load_shiny_data())
  expect_length(result, 6)
  expect_true("trade_values" %in% names(result))
  expect_null(result$trade_values)
})

test_that("a present RDS file is loaded into its slot", {
  tmp <- tempfile(fileext = ".rds")
  saveRDS(tibble::tibble(x = 1:3, y = letters[1:3]), tmp)
  old <- PATH_VORP
  assign("PATH_VORP", tmp, envir = globalenv())
  on.exit({
    assign("PATH_VORP", old, envir = globalenv())
    unlink(tmp)
  }, add = TRUE)

  result <- suppressWarnings(load_shiny_data())
  expect_s3_class(result$vorp, "data.frame")
  expect_equal(nrow(result$vorp), 3)
})

test_that("a present CSV file is loaded into its slot", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:2, b = c("x", "y")), tmp, row.names = FALSE)
  old <- PATH_PROSPECTS
  assign("PATH_PROSPECTS", tmp, envir = globalenv())
  on.exit({
    assign("PATH_PROSPECTS", old, envir = globalenv())
    unlink(tmp)
  }, add = TRUE)

  result <- suppressWarnings(load_shiny_data())
  expect_s3_class(result$prospects, "data.frame")
  expect_equal(nrow(result$prospects), 2)
})

# ==============================================================================
# NULL-assignment idiom (documents the R gotcha the data layer guards against)
# ==============================================================================

test_that("single-bracket list(NULL) preserves a slot; [[<- NULL removes it", {
  buggy <- vector("list", 3)
  names(buggy) <- c("a", "b", "c")
  buggy[["b"]] <- NULL              # WRONG idiom: removes the slot
  expect_length(buggy, 2)

  fixed <- vector("list", 3)
  names(fixed) <- c("a", "b", "c")
  fixed["b"] <- list(NULL)          # CORRECT idiom: preserves the slot as NULL
  expect_length(fixed, 3)
  expect_true("b" %in% names(fixed))
  expect_null(fixed$b)
})
