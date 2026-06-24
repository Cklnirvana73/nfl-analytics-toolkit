# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 19 (R/39)
# Verification smoke test: Tab 1 Projection Dashboard module
# File: examples/verify_season2_week19_shiny.R
#
# PURPOSE
# -------
# Build-verification deliverable for the Shiny Tab 1 module. A Shiny week
# produces interactive UI, not a static PNG, so this browser-free smoke test
# stands in for the usual visualization script (same pattern as
# examples/verify_season2_week18_shiny.R). It confirms the module builds, wires
# against a synthetic data layer, and filters correctly -- without launching a
# browser.
#
# The visualization reference (data-science-code-reviewer skill) was consulted:
# it covers analytical charts (ggplot2, EPA distributions, heatmaps), none of
# which apply to a UI tab. The tab itself is the visual; this script verifies it.
#
# RUN
# ---
#   source(here::here("examples", "verify_season2_week19_shiny.R"))
#
# Prints one PASS/FAIL line per check and a final summary. Exits non-zero-style
# (stop()) only if any check fails, so it can gate a build.
# ==============================================================================

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(here)

# Module under test.
source(here::here("shiny_app", "tabs", "projections.R"))

# ------------------------------------------------------------------------------
# Tiny check harness
# ------------------------------------------------------------------------------

.checks <- list()

check <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  .checks[[length(.checks) + 1L]] <<- list(label = label, ok = ok)
  status <- if (ok) "PASS" else "FAIL"
  message(sprintf("  [%s] %s", status, label))
  invisible(ok)
}

# ------------------------------------------------------------------------------
# Synthetic data layer (mirrors the columns projections.R reads from vorp)
# ------------------------------------------------------------------------------

make_vorp_fixture <- function() {
  tibble::tibble(
    nfl_gsis_id             = sprintf("00-%07d", 1:6),
    player_name             = c("Alpha QB", "Bravo RB", "Charlie WR",
                                "Delta TE", "Echo WR", "Foxtrot RB"),
    team                    = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF"),
    position                = c("QB", "RB", "WR", "TE", "WR", "RB"),
    league_name             = c(rep("League One", 4), rep("League Two", 2)),
    league_format           = "ppr",
    league_teams            = 12L,
    r32_posterior_mu        = c(22.0, 15.5, 18.2, NA_real_, 9.1, 12.0),
    r32_projection_lower_80 = c(16.0, 10.0, 12.0, 5.0, 5.0, 7.0),
    r32_projection_upper_80 = c(28.0, 21.0, 24.0, 13.0, 13.0, 17.0),
    boom_probability        = c(0.30, 0.25, 0.28, 0.10, 0.12, 0.20),
    bust_probability        = c(0.10, 0.20, 0.15, 0.40, 0.35, 0.25),
    replacement_ppg         = 9.0,
    vorp_base               = c(13.0, 6.5, 9.2, NA_real_, 0.1, 3.0),
    boom_modifier           = 0.5,
    bust_modifier           = -0.3,
    ceiling_modifier        = 0.0,
    adjusted_vorp           = c(8.0, 4.0, 6.0, 1.0, 0.5, 2.0),
    overall_rank            = c(1L, 4L, 3L, 6L, 5L, 2L),
    position_rank           = c(1L, 1L, 1L, 1L, 2L, 2L),
    schema_tag              = "s2_w15_vorp_v2"
  )
}

# ==============================================================================
# CHECKS
# ==============================================================================

message("\n", strrep("=", 60))
message("R/39 verification: Tab 1 Projection Dashboard module")
message(strrep("=", 60))

fx <- make_vorp_fixture()

# 1. UI builds and is a Shiny tag.
ui_obj <- projectionsUI("proj")
check("projectionsUI() returns a Shiny UI tag",
      inherits(ui_obj, "shiny.tag") || inherits(ui_obj, "shiny.tag.list"))

# 2. The calibration banner is present in the rendered UI HTML.
ui_html <- as.character(ui_obj)
check("calibration banner is present in the UI",
      grepl("being calibrated", ui_html, fixed = TRUE) &&
        grepl("alert-warning", ui_html, fixed = TRUE))

# 3. UI inputs are namespaced (league + position carry the module id).
check("league and position inputs are namespaced under 'proj'",
      grepl("proj-league", ui_html, fixed = TRUE) &&
        grepl("proj-position", ui_html, fixed = TRUE))

# 4. Server is a function with the expected accessor signature.
check("projectionsServer() has (id, current_vorp, current_start_sit) args",
      identical(names(formals(projectionsServer)),
                c("id", "current_vorp", "current_start_sit")))

# 5. Module filters by league via testServer (the core data path).
res_league <- tryCatch({
  ok <- FALSE
  shiny::testServer(
    projectionsServer,
    args = list(current_vorp      = shiny::reactive(fx),
                current_start_sit = shiny::reactive(NULL)),
    {
      session$setInputs(league = "League Two", position = "All")
      f <- filtered()
      ok <<- all(f$league_name == "League Two") && nrow(f) == 2L
    }
  )
  ok
}, error = function(e) FALSE)
check("league filter scopes the table to one league", res_league)

# 6. NA-projection rows are dropped (Delta TE has NA mu).
res_na <- tryCatch({
  ok <- FALSE
  shiny::testServer(
    projectionsServer,
    args = list(current_vorp      = shiny::reactive(fx),
                current_start_sit = shiny::reactive(NULL)),
    {
      session$setInputs(league = "League One", position = "All")
      f <- filtered()
      ok <<- nrow(f) == 3L && !any(is.na(f$r32_posterior_mu))
    }
  )
  ok
}, error = function(e) FALSE)
check("rows with no projection are excluded", res_na)

# 7. The full app object assembles against the synthetic data layer.
res_app <- tryCatch({
  app_data <- list(vorp = fx, start_sit_uq = NULL)
  ui <- bslib::page_navbar(
    title = "smoke",
    bslib::nav_panel(title = "Projections", projectionsUI("proj"))
  )
  server <- function(input, output, session) {
    current_vorp      <- shiny::reactive(app_data$vorp)
    current_start_sit <- shiny::reactive(app_data$start_sit_uq)
    projectionsServer("proj", current_vorp, current_start_sit)
  }
  app <- shiny::shinyApp(ui, server)
  inherits(app, "shiny.appobj")
}, error = function(e) FALSE)
check("app assembles with the tab wired to a synthetic data layer", res_app)

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

n_total <- length(.checks)
n_pass  <- sum(vapply(.checks, function(c) isTRUE(c$ok), logical(1)))

message(strrep("=", 60))
message(sprintf("R/39 verification: %d of %d checks passed", n_pass, n_total))
message(strrep("=", 60), "\n")

if (n_pass < n_total) {
  failed <- vapply(.checks[!vapply(.checks, function(c) isTRUE(c$ok),
                                   logical(1))],
                   function(c) c$label, character(1))
  stop("Verification FAILED: ", paste(failed, collapse = "; "), call. = FALSE)
}

invisible(TRUE)
