# ==============================================================================
# NFL Analytics Toolkit | Season 2, Phase 5
# Tests: Week 20 (R/40) Shiny Tab 2 Waiver / Start-Sit
# File: tests/test_season2_week20_functions.R
#
# SCOPE
# -----
# Tab 2 is a cache-read module: waiverUI() / waiverServer() in
# shiny_app/tabs/waiver.R. These tests cover the module contract (existence,
# signatures, output namespacing), the server behavior against a synthetic
# fixture (renders, the week-risk summary arithmetic, and the NULL guard), and
# the data contract on the R/36 caches the module reads. The cache-contract
# tests skip cleanly when a cache file is absent, so the suite passes in an
# environment without the Phase 4 outputs and verifies columns where present.
#
# SELF-CONTAINED
# --------------
# Attaches every package the module relies on (shiny, bslib, DT, dplyr), plus
# testthat and tibble for the fixtures, and sources the module and the data
# layer through here::here() so the file runs from the project root with no
# prior session state. No root-level runner: run with
# testthat::test_file("tests/test_season2_week20_functions.R").
# ==============================================================================

library(testthat)
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tibble)

# Module under test (pure definitions: defines waiverUI / waiverServer).
source(here::here("shiny_app", "tabs", "waiver.R"))

# Data layer for the cache path constants (PATH_START_SIT_UQ, PATH_DEF_STREAMING).
source(here::here("shiny_app", "R", "data_layer.R"))

# ------------------------------------------------------------------------------
# Columns the module reads from each cache. Kept here so a future column rename
# upstream fails these tests loudly rather than silently blanking a table.
# ------------------------------------------------------------------------------

START_SIT_REQUIRED <- c(
  "slot", "player_name", "team", "position", "opponent", "adj_proj",
  "confidence_flag", "p_start_correct", "alt_player", "alt_source",
  "expected_regret", "stakes"
)

DEF_STREAMING_REQUIRED <- c(
  "rank", "def_team", "opponent", "adj_proj", "p_vs_next", "pick_clarity"
)

# ------------------------------------------------------------------------------
# Fixtures. Minimal tibbles carrying exactly the columns the module reads, so
# the behavioral tests do not depend on the real caches being present.
# expected_regret sums to 4.0 so the week-risk assertion has a known target.
# ------------------------------------------------------------------------------

fixture_start_sit <- tibble::tibble(
  slot            = c("QB", "RB", "WR"),
  nfl_gsis_id     = c("00-0000001", "00-0000002", "00-0000003"),
  player_name     = c("Test QB", "Test RB", "Test WR"),
  team            = c("CHI", "BUF", "SF"),
  position        = c("QB", "RB", "WR"),
  opponent        = c("ATL", "LV", "WAS"),
  base_proj       = c(16.0, 12.0, 13.0),
  matchup_factor  = c(1.00, 1.00, 1.00),
  adj_proj        = c(16.9, 12.2, 13.0),
  adjusted_vorp   = c(2.1, 1.0, 1.4),
  confidence_flag = c("clear", "close", "close"),
  schema_tag      = "s2_w16_lineup_v1",
  alt_player      = c("Alt QB", "Alt RB", "Alt WR"),
  alt_position    = c("QB", "RB", "WR"),
  alt_source      = c("bench", "bench", "bench"),
  alt_adj_proj    = c(15.0, 11.0, 12.0),
  p_start_correct = c(0.75, 0.65, 0.64),
  avg_miss        = c(2.0, 2.0, 2.0),
  expected_regret = c(0.8, 1.0, 2.2),   # sum = 4.0
  stakes          = c("low", "medium", "medium"),
  uq_schema_tag   = "s2_w16_uq_v1"
)

fixture_def_streaming <- tibble::tibble(
  rank          = c(1L, 2L),
  def_team      = c("BUF", "BAL"),
  opponent      = c("LV", "CLE"),
  adj_proj      = c(9.0, 8.9),
  p_vs_next     = c(0.51, NA_real_),       # last row has no next to beat
  pick_clarity  = c("tossup", NA_character_),
  uq_schema_tag = "s2_w16_uq_v1"
)

# ==============================================================================
# 1. Module contract
# ==============================================================================

test_that("waiverUI and waiverServer are defined functions", {
  expect_true(is.function(waiverUI))
  expect_true(is.function(waiverServer))
})

test_that("waiverUI has the expected signature", {
  expect_identical(names(formals(waiverUI)), "id")
})

test_that("waiverServer has the expected signature", {
  expect_identical(
    names(formals(waiverServer)),
    c("id", "current_start_sit", "current_def_streaming")
  )
})

test_that("waiverUI namespaces its outputs under the module id", {
  html <- as.character(waiverUI("waiver"))
  expect_match(html, "waiver-risk_summary", fixed = TRUE, all = FALSE)
  expect_match(html, "waiver-lineup_table", fixed = TRUE, all = FALSE)
  expect_match(html, "waiver-def_table",    fixed = TRUE, all = FALSE)
})

# ==============================================================================
# 2. Server behavior (synthetic fixture, no real cache required)
# ==============================================================================

test_that("waiverServer renders both tables with a valid fixture", {
  shiny::testServer(
    waiverServer,
    args = list(
      current_start_sit     = shiny::reactive(fixture_start_sit),
      current_def_streaming = shiny::reactive(fixture_def_streaming)
    ),
    {
      expect_false(is.null(output$lineup_table))
      expect_false(is.null(output$def_table))
    }
  )
})

test_that("week risk score reports the sum of expected_regret", {
  shiny::testServer(
    waiverServer,
    args = list(
      current_start_sit     = shiny::reactive(fixture_start_sit),
      current_def_streaming = shiny::reactive(fixture_def_streaming)
    ),
    {
      rendered <- paste(unlist(output$risk_summary), collapse = " ")
      expect_match(rendered, "4.0 expected points at risk", fixed = TRUE)
    }
  )
})

test_that("risk summary is empty when start/sit data is NULL", {
  shiny::testServer(
    waiverServer,
    args = list(
      current_start_sit     = shiny::reactive(NULL),
      current_def_streaming = shiny::reactive(NULL)
    ),
    {
      # The renderUI guard returns NULL for a missing cache; the lineup and DEF
      # tables hit their validate() messages on access, so they are not probed
      # here (a validation stop is the intended behavior, not a test target).
      expect_null(output$risk_summary)
    }
  )
})

# ==============================================================================
# 3. Data contract (real caches; skipped when absent)
# ==============================================================================

test_that("start_sit_uq cache carries the columns the module reads", {
  skip_if_not(
    file.exists(PATH_START_SIT_UQ),
    "start_sit_uq cache not present; run R/36 with save_output = TRUE."
  )
  df <- readRDS(PATH_START_SIT_UQ)
  missing <- setdiff(START_SIT_REQUIRED, names(df))
  expect_identical(missing, character(0))
  expect_gt(nrow(df), 0L)
})

test_that("def_streaming cache carries the columns the module reads", {
  skip_if_not(
    file.exists(PATH_DEF_STREAMING),
    "def_streaming cache not present (legitimate when the lineup starts a DEF)."
  )
  df <- readRDS(PATH_DEF_STREAMING)
  # The slot can be NULL even when a file exists only if it was written empty;
  # treat a NULL or empty table as nothing to verify rather than a failure.
  skip_if(is.null(df) || nrow(df) == 0L,
          "def_streaming cache is empty; no columns to verify.")
  missing <- setdiff(DEF_STREAMING_REQUIRED, names(df))
  expect_identical(missing, character(0))
})
