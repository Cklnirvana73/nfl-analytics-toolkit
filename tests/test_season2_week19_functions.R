# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 19 (R/39)
# Test suite: Tab 1 Projection Dashboard module
# File: tests/test_season2_week19_functions.R
#
# PURPOSE
# -------
# Unit tests for shiny_app/tabs/projections.R. Exercises projectionsServer()
# via shiny::testServer() against a synthetic vorp fixture (no live data, in the
# Season 2 deterministic-fixture style). Covers the module's real logic:
#   - league filtering
#   - position filtering ("All" passes everything)
#   - NA-projection rows dropped
#   - graceful halt when the vorp accessor returns NULL
#   - render produces output without error when data is present
#
# The league-selector population (updateSelectInput inside an observe) is UI
# plumbing exercised end-to-end in examples/verify_season2_week19_shiny.R; here
# we drive inputs directly and assert on the filtered reactive.
#
# RUN
# ---
#   testthat::test_file(here::here("tests", "test_season2_week19_functions.R"))
# ==============================================================================

library(testthat)
library(shiny)
library(dplyr)
library(DT)
library(here)

# Module under test. Sourcing defines projectionsUI() and projectionsServer();
# no top-level side effects.
source(here::here("shiny_app", "tabs", "projections.R"))

# ------------------------------------------------------------------------------
# Synthetic fixture
# ------------------------------------------------------------------------------
# Two leagues, mixed positions, one row with NA r32_posterior_mu (must be
# dropped). Carries exactly the columns projections.R reads from the vorp object
# (R/33 schema s2_w15_vorp_v2).
#
#   League One : 5 rows (QB, RB, WR, TE[NA mu], WR)  -> 4 after NA drop
#   League Two : 3 rows (RB, QB, WR)                 -> 3 after NA drop

make_vorp_fixture <- function() {
  tibble::tibble(
    nfl_gsis_id             = sprintf("00-%07d", 1:8),
    player_name             = c("Alpha QB", "Bravo RB", "Charlie WR",
                                "Delta TE", "Echo WR", "Foxtrot RB",
                                "Golf QB", "Hotel WR"),
    team                    = c("AAA", "BBB", "CCC", "DDD",
                                "EEE", "FFF", "GGG", "HHH"),
    position                = c("QB", "RB", "WR", "TE",
                                "WR", "RB", "QB", "WR"),
    league_name             = c(rep("League One", 5), rep("League Two", 3)),
    league_format           = "ppr",
    league_teams            = 12L,
    r32_posterior_mu        = c(22.0, 15.5, 18.2, NA_real_,
                                9.1, 12.0, 20.5, 14.0),
    r32_projection_lower_80 = c(16.0, 10.0, 12.0, 5.0,
                                5.0, 7.0, 14.0, 9.0),
    r32_projection_upper_80 = c(28.0, 21.0, 24.0, 13.0,
                                13.0, 17.0, 27.0, 19.0),
    boom_probability        = c(0.30, 0.25, 0.28, 0.10,
                                0.12, 0.20, 0.27, 0.22),
    bust_probability        = c(0.10, 0.20, 0.15, 0.40,
                                0.35, 0.25, 0.12, 0.18),
    replacement_ppg         = 9.0,
    vorp_base               = c(13.0, 6.5, 9.2, NA_real_,
                                0.1, 3.0, 11.5, 5.0),
    boom_modifier           = 0.5,
    bust_modifier           = -0.3,
    ceiling_modifier        = 0.0,
    adjusted_vorp           = c(8.0, 4.0, 6.0, 1.0,
                                0.5, 2.0, 7.0, 3.0),
    overall_rank            = c(1L, 4L, 3L, 8L, 7L, 5L, 2L, 6L),
    position_rank           = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L),
    schema_tag              = "s2_w15_vorp_v2"
  )
}

# ==============================================================================
# TESTS
# ==============================================================================

test_that("projectionsUI returns a Shiny UI object", {
  ui <- projectionsUI("proj")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("league filter scopes filtered() to the selected league", {
  fx <- make_vorp_fixture()
  shiny::testServer(
    projectionsServer,
    args = list(current_vorp      = shiny::reactive(fx),
                current_start_sit = shiny::reactive(NULL)),
    {
      session$setInputs(league = "League One", position = "All")
      f <- filtered()
      expect_true(all(f$league_name == "League One"))

      session$setInputs(league = "League Two", position = "All")
      f2 <- filtered()
      expect_true(all(f2$league_name == "League Two"))
      expect_equal(nrow(f2), 3L)
    }
  )
})

test_that("NA-projection rows are dropped", {
  fx <- make_vorp_fixture()
  shiny::testServer(
    projectionsServer,
    args = list(current_vorp      = shiny::reactive(fx),
                current_start_sit = shiny::reactive(NULL)),
    {
      session$setInputs(league = "League One", position = "All")
      f <- filtered()
      # League One has 5 rows; Delta TE has NA mu -> 4 remain
      expect_equal(nrow(f), 4L)
      expect_false(any(is.na(f$r32_posterior_mu)))
      expect_false("Delta TE" %in% f$player_name)
    }
  )
})

test_that("position = 'All' applies no position filter", {
  fx <- make_vorp_fixture()
  shiny::testServer(
    projectionsServer,
    args = list(current_vorp      = shiny::reactive(fx),
                current_start_sit = shiny::reactive(NULL)),
    {
      session$setInputs(league = "League One", position = "All")
      f <- filtered()
      expect_setequal(unique(f$position), c("QB", "RB", "WR"))
    }
  )
})

test_that("position filter restricts to the chosen position", {
  fx <- make_vorp_fixture()
  shiny::testServer(
    projectionsServer,
    args = list(current_vorp      = shiny::reactive(fx),
                current_start_sit = shiny::reactive(NULL)),
    {
      session$setInputs(league = "League One", position = "WR")
      f <- filtered()
      expect_true(all(f$position == "WR"))
      # League One WRs with non-NA mu: Charlie, Echo
      expect_equal(nrow(f), 2L)
    }
  )
})

test_that("position filter combined with NA drop yields empty set", {
  fx <- make_vorp_fixture()
  shiny::testServer(
    projectionsServer,
    args = list(current_vorp      = shiny::reactive(fx),
                current_start_sit = shiny::reactive(NULL)),
    {
      # The only League One TE (Delta) has NA mu, so the filtered set is empty.
      session$setInputs(league = "League One", position = "TE")
      f <- filtered()
      expect_equal(nrow(f), 0L)
    }
  )
})

test_that("filtered() halts gracefully when the vorp accessor is NULL", {
  shiny::testServer(
    projectionsServer,
    args = list(current_vorp      = shiny::reactive(NULL),
                current_start_sit = shiny::reactive(NULL)),
    {
      session$setInputs(league = "anything", position = "All")
      # req(df) on a NULL accessor raises a silent shiny error (no crash).
      expect_error(filtered(), class = "shiny.silent.error")
    }
  )
})

test_that("the table renders without error when data is present", {
  fx <- make_vorp_fixture()
  shiny::testServer(
    projectionsServer,
    args = list(current_vorp      = shiny::reactive(fx),
                current_start_sit = shiny::reactive(NULL)),
    {
      session$setInputs(league = "League One", position = "All")
      expect_error(output$table, NA)
    }
  )
})

test_that("unused current_start_sit accessor does not break the module", {
  fx <- make_vorp_fixture()
  # Pass a populated start_sit accessor to confirm the seam argument is inert.
  ss <- tibble::tibble(nfl_gsis_id = "00-0000001", stakes = "high",
                       p_start_correct = 0.61)
  shiny::testServer(
    projectionsServer,
    args = list(current_vorp      = shiny::reactive(fx),
                current_start_sit = shiny::reactive(ss)),
    {
      session$setInputs(league = "League One", position = "All")
      f <- filtered()
      # start_sit is not joined in Tab 1; row count must be unaffected by it.
      expect_equal(nrow(f), 4L)
    }
  )
})
