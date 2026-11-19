# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 21 (R/41)
# Verify: Tab 3 Trade Evaluator + Tab 4 Rookie Tracker (browser-free smoke test)
# File: examples/verify_season2_week21_shiny.R
#
# PURPOSE
# -------
# Confirms the two Week 21 tab modules build and wire correctly without launching
# a browser. This replaces the static PNG visualization deliverable for a Shiny
# week (the postable visual is the running app itself). Each check prints a
# [PASS]/[FAIL] line and the script ends with an "N of N checks passed" summary.
#
# Checks:
#   1. tradeUI() returns a Shiny UI tag
#   2. rookieUI() returns a Shiny UI tag
#   3. both tabs carry the calibration alert
#   4. tab inputs are namespaced under the module id
#   5. tradeServer() has (id, current_trade_values, app_season) args
#   6. rookieServer() has (id, current_prospects, app_season) args
#   7. tradeServer wires through to a verdict on a synthetic data layer
#   8. rookieServer ranks rookies within position on a synthetic data layer
#
# RUN
# ---
#   From the project root, in a fresh R session:
#     source(here::here("examples", "verify_season2_week21_shiny.R"))
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(dplyr)
  library(here)
  library(glue)
  library(tibble)
  library(purrr)
})

# Modules + helpers under test (project-root-anchored, like the test suite).
source(here::here("R", "19_sleeper_api.R"))
source(here::here("shiny_app", "R", "trade_helpers.R"))
source(here::here("shiny_app", "tabs", "trade.R"))
source(here::here("shiny_app", "tabs", "rookie.R"))

# ------------------------------------------------------------------------------
# Tiny check harness
# ------------------------------------------------------------------------------

.checks <- list()
check <- function(label, ok) {
  ok <- isTRUE(ok)
  .checks[[length(.checks) + 1L]] <<- list(label = label, ok = ok)
  cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", label))
  invisible(ok)
}

# A UI tag rendered to one HTML string, for substring assertions.
ui_html <- function(ui) paste(as.character(ui), collapse = " ")

# ------------------------------------------------------------------------------
# Synthetic fixtures (match the data the tabs read)
# ------------------------------------------------------------------------------

tv_fixture <- tibble::tibble(
  league_name = rep("King of Kings", 4L),
  nfl_gsis_id = c("p1", "p2", "p3", "p4"),
  player_name = c("Player A", "Player B", "Player C", "Player D"),
  position    = c("QB", "RB", "WR", "TE"),
  trade_value = c(90, 70, 50, 30),
  tv_tier     = c("Elite", "Great", "Good", "Solid")
)

prospects_fixture <- dplyr::bind_rows(
  tibble::tibble(
    cfb_player_name  = c("Rookie WR1", "Rookie WR2"),
    position         = "WR",
    draft_year       = 2026L,
    draft_round      = c(1L, 2L),
    draft_pick       = c(5L, 40L),
    draft_class_type = "prediction",
    score_final      = c(0.80, 0.60),
    is_hit           = NA_real_,
    ppr_per_game_y13 = NA_real_
  ),
  tibble::tibble(
    cfb_player_name  = paste0("WR Train ", 1:3),
    position         = "WR",
    draft_year       = c(2018L, 2019L, 2020L),
    draft_round      = 2L,
    draft_pick       = 50L,
    draft_class_type = "training",
    score_final      = c(0.78, 0.82, 0.70),
    is_hit           = c(1, 0, 1),
    ppr_per_game_y13 = c(12, 8, 14)
  )
)

# ------------------------------------------------------------------------------
# Run checks
# ------------------------------------------------------------------------------

cat(strrep("=", 60), "\n", sep = "")
cat("R/41 verification: Tab 3 Trade Evaluator + Tab 4 Rookie Tracker\n")
cat(strrep("=", 60), "\n", sep = "")

trade_ui  <- tradeUI("trade")
rookie_ui <- rookieUI("rookie")

check("tradeUI() returns a Shiny UI tag",
      inherits(trade_ui, "shiny.tag.list") || inherits(trade_ui, "shiny.tag"))

check("rookieUI() returns a Shiny UI tag",
      inherits(rookie_ui, "shiny.tag.list") || inherits(rookie_ui, "shiny.tag"))

check("both tabs carry the calibration alert",
      grepl("being calibrated", ui_html(trade_ui)) &&
        grepl("being calibrated", ui_html(rookie_ui)))

# Namespacing: a known input id appears prefixed with the module id.
check("tab inputs are namespaced under the module id",
      grepl("trade-username", ui_html(trade_ui), fixed = TRUE) &&
        grepl("rookie-lookup_name", ui_html(rookie_ui), fixed = TRUE))

check("tradeServer() has (id, current_trade_values, app_season) args",
      identical(names(formals(tradeServer)),
                c("id", "current_trade_values", "app_season")))

check("rookieServer() has (id, current_prospects, app_season) args",
      identical(names(formals(rookieServer)),
                c("id", "current_prospects", "app_season")))

# Capture env at top level. A testServer expr resolves free names against the
# module's scope (and up to the global env where the modules were sourced), not
# against the caller's local frame, so the capture target must be global. Env
# mutation is by reference, so writes inside the expr persist out here.
.cap <- new.env()

# Synthetic round-trip: the trade server reaches a verdict.
trade_ok <- tryCatch({
  .cap$verdict <- NULL
  shiny::testServer(
    tradeServer,
    args = list(current_trade_values = shiny::reactive(tv_fixture),
                app_season = 2026L),
    {
      session$setInputs(league = "King of Kings")
      session$setInputs(give = "p1", receive = c("p2", "p3"))
      session$setInputs(evaluate = 1)
      .cap$verdict <- result()$verdict
    }
  )
  identical(.cap$verdict, "WIN")
}, error = function(e) FALSE)
check("tradeServer wires through to a verdict on a synthetic data layer",
      trade_ok)

# Synthetic round-trip: the rookie server ranks within position.
rookie_ok <- tryCatch({
  .cap$top_rank <- NULL
  shiny::testServer(
    rookieServer,
    args = list(current_prospects = shiny::reactive(prospects_fixture),
                app_season = 2026L),
    {
      tbl <- rookie_table_data()
      .cap$top_rank <- tbl$pos_rank[tbl$cfb_player_name == "Rookie WR1"]
    }
  )
  identical(.cap$top_rank, 1L)
}, error = function(e) FALSE)
check("rookieServer ranks rookies within position on a synthetic data layer",
      rookie_ok)

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

n_pass <- sum(vapply(.checks, function(c) c$ok, logical(1)))
n_tot  <- length(.checks)
cat(strrep("=", 60), "\n", sep = "")
cat(sprintf("R/41 verification: %d of %d checks passed\n", n_pass, n_tot))
cat(strrep("=", 60), "\n", sep = "")

if (n_pass < n_tot) {
  failed <- vapply(.checks[!vapply(.checks, function(c) c$ok, logical(1))],
                   function(c) c$label, character(1))
  warning("Verify finished with failing checks: ",
          paste(failed, collapse = "; "), call. = FALSE)
}
