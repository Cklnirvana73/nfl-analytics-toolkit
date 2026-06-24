# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 18
# Build verification: Shiny architecture and data layer
# File: examples/verify_season2_week18_shiny.R
#
# PURPOSE
# -------
# Stands in for the usual visualization deliverable. A Shiny architecture week
# produces infrastructure, not analytical findings, so there is no data chart
# to make. Instead this script verifies that the week's actual output works:
# the data layer loads its six-slot structure, evaluate_trade() runs
# end-to-end on real cached data, and the full app object constructs. It does
# all of this WITHOUT launching a browser, and doubles as the examples script
# by demonstrating both functions on real data.
#
# RUN
# ---
#   source(here::here("examples", "verify_season2_week18_shiny.R"))
#
# OUTPUT
# ------
# Prints a PASS/FAIL line per check and a summary. Stops with an error if any
# check fails, so it can gate a deploy. Produces no files.
# ==============================================================================

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(glue)
library(here)
library(httr)

# Data layer + trade helper, sourced directly (no app launch).
source(here::here("shiny_app", "R", "data_layer.R"))
source(here::here("shiny_app", "R", "trade_helpers.R"))

# ------------------------------------------------------------------------------
# PASS/FAIL HARNESS
# ------------------------------------------------------------------------------

pass_count <- 0L
fail_count <- 0L

check <- function(label, ok, detail = "") {
  status <- if (isTRUE(ok)) "PASS" else "FAIL"
  if (isTRUE(ok)) {
    pass_count <<- pass_count + 1L
  } else {
    fail_count <<- fail_count + 1L
  }
  cat(sprintf("  [%s] %s%s\n", status, label,
              if (nzchar(detail)) paste0(" (", detail, ")") else ""))
}

cat(strrep("=", 64), "\n", sep = "")
cat("Week 18 Shiny build verification\n")
cat(strrep("=", 64), "\n", sep = "")

# ------------------------------------------------------------------------------
# GROUP 1: DATA LAYER
# ------------------------------------------------------------------------------

cat("\nData layer:\n")

app_data <- suppressWarnings(load_shiny_data())

check("load_shiny_data() returns a list", is.list(app_data))
check("six named slots present", length(app_data) == 6L,
      sprintf("got %d", length(app_data)))

expected_slots <- c("vorp", "start_sit_uq", "def_streaming",
                    "optimal_lineup", "prospects", "trade_values")
check("slot names correct", setequal(names(app_data), expected_slots))

slot_ok <- vapply(app_data,
                  function(x) is.null(x) || is.data.frame(x),
                  logical(1))
check("every slot is NULL or a data frame", all(slot_ok))

loaded  <- names(app_data)[!vapply(app_data, is.null, logical(1))]
missing <- names(app_data)[vapply(app_data, is.null, logical(1))]
cat(sprintf("  loaded:  %s\n",
            if (length(loaded))  paste(loaded, collapse = ", ")  else "none"))
cat(sprintf("  missing: %s\n",
            if (length(missing)) paste(missing, collapse = ", ") else "none"))

# ------------------------------------------------------------------------------
# GROUP 2: TRADE EVALUATION (on real cache if present, else synthetic)
# ------------------------------------------------------------------------------

cat("\nTrade evaluation:\n")

tv <- app_data$trade_values
if (is.null(tv)) {
  cat("  trade_values cache absent; exercising function on a synthetic table\n")
  tv <- tibble::tibble(
    league_name = "Demo League",
    nfl_gsis_id = c("00-0000001", "00-0000002", "00-0000003"),
    player_name = c("Alpha", "Bravo", "Charlie"),
    position    = c("RB", "WR", "QB"),
    trade_value = c(100, 60, 40),
    tv_tier     = c("Elite", "Tier 2", "Depth")
  )
}

first_league <- tv$league_name[1]
league_rows  <- dplyr::filter(tv, .data$league_name == first_league)

res <- NULL
trade_ok <- tryCatch({
  res <- evaluate_trade(
    give_ids     = league_rows$nfl_gsis_id[1],
    receive_ids  = league_rows$nfl_gsis_id[2],
    trade_values = tv,
    league_name  = first_league
  )
  is.list(res) &&
    all(c("give_value", "receive_value", "net_delta", "verdict") %in% names(res)) &&
    res$verdict %in% c("WIN", "LOSS", "ROUGHLY EVEN")
}, error = function(e) {
  cat("  error:", conditionMessage(e), "\n")
  FALSE
})

check("evaluate_trade() runs end-to-end and returns a valid verdict", trade_ok)

if (isTRUE(trade_ok)) {
  cat(sprintf("  example: give %s, receive %s in '%s' => %s (net %.1f)\n",
              league_rows$player_name[1],
              league_rows$player_name[2],
              first_league,
              res$verdict,
              res$net_delta))
}

# ------------------------------------------------------------------------------
# GROUP 3: APP OBJECT CONSTRUCTION (no browser launch)
# ------------------------------------------------------------------------------

cat("\nApp construction:\n")

# Sourcing app.R evaluates shiny::shinyApp(ui, server) as its last expression
# and returns the app object. shinyApp() builds the object but does NOT launch;
# runApp() (in R/38) is what launches, and it is not called here. Startup
# messages and cache warnings are suppressed to keep this report clean.
app_obj <- tryCatch(
  suppressMessages(suppressWarnings(
    source(here::here("shiny_app", "app.R"))$value
  )),
  error = function(e) {
    cat("  build error:", conditionMessage(e), "\n")
    NULL
  }
)

check("app.R builds a shiny.appobj without launching",
      inherits(app_obj, "shiny.appobj"))

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------

cat("\n", strrep("=", 64), "\n", sep = "")
cat(sprintf("RESULT: %d passed, %d failed\n", pass_count, fail_count))
cat(strrep("=", 64), "\n", sep = "")

if (fail_count > 0L) {
  stop(sprintf("Build verification FAILED (%d check(s) failed).", fail_count),
       call. = FALSE)
}

cat("All build checks passed. App is wired and ready to launch.\n")
