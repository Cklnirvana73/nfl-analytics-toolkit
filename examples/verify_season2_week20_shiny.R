# ==============================================================================
# NFL Analytics Toolkit | Season 2, Phase 5
# Smoke test: Week 20 (R/40) Shiny Tab 2 Waiver / Start-Sit
# File: examples/verify_season2_week20_shiny.R
#
# PURPOSE
# -------
# Browser-free verification for the Tab 2 build, the Shiny-week substitute for a
# PNG visual. Sources the data layer and the module, loads the real R/36 caches,
# and runs a fixed set of checks that the module is wired correctly and the data
# it reads is present and well-formed. Prints a numbered PASS/FAIL line per check
# and a final summary, and stops with a non-zero condition if any check fails.
#
# RUN
# ---
#   source(here::here("examples", "verify_season2_week20_shiny.R"))
#
# This does NOT launch the app or open a browser. Visual confirmation of the
# rendered tab is done by launching the app (R/38) separately.
# ==============================================================================

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(DT)
library(dplyr)

# Module under test and the data layer (for the cache path constants and
# load_shiny_data()). Both are pure-definition sources.
source(here::here("shiny_app", "tabs", "waiver.R"))
source(here::here("shiny_app", "R", "data_layer.R"))

# Columns the module reads from each cache. A failure here means an upstream
# rename would silently blank a table in the running app.
START_SIT_REQUIRED <- c(
  "slot", "player_name", "team", "position", "opponent", "adj_proj",
  "confidence_flag", "p_start_correct", "alt_player", "alt_source",
  "expected_regret", "stakes"
)
DEF_STREAMING_REQUIRED <- c(
  "rank", "def_team", "opponent", "adj_proj", "p_vs_next", "pick_clarity"
)

# ------------------------------------------------------------------------------
# CHECK HARNESS
# ------------------------------------------------------------------------------

results <- list()

check <- function(label, passed, detail = "") {
  n <- length(results) + 1L
  status <- if (isTRUE(passed)) "PASS" else "FAIL"
  results[[n]] <<- list(label = label, passed = isTRUE(passed))
  cat(sprintf(
    "  [%s] %d. %s%s\n",
    status, n, label,
    if (nzchar(detail)) sprintf("  (%s)", detail) else ""
  ))
  invisible(isTRUE(passed))
}

cat(sprintf("\n%s\n", strrep("=", 70)))
cat("Week 20 smoke test: Shiny Tab 2 Waiver / Start-Sit\n")
cat(sprintf("%s\n", strrep("=", 70)))

# ------------------------------------------------------------------------------
# 1. Module definitions
# ------------------------------------------------------------------------------
check(
  "waiverUI and waiverServer are defined functions",
  is.function(waiverUI) && is.function(waiverServer)
)

# ------------------------------------------------------------------------------
# 2. Module signatures
# ------------------------------------------------------------------------------
ui_sig_ok <- identical(names(formals(waiverUI)), "id")
srv_sig_ok <- identical(
  names(formals(waiverServer)),
  c("id", "current_start_sit", "current_def_streaming")
)
check(
  "Module signatures match the accessor contract",
  ui_sig_ok && srv_sig_ok,
  sprintf("server(%s)", paste(names(formals(waiverServer)), collapse = ", "))
)

# ------------------------------------------------------------------------------
# 3. UI namespacing
# ------------------------------------------------------------------------------
ui_html <- as.character(waiverUI("waiver"))
ns_ids  <- c("waiver-risk_summary", "waiver-lineup_table", "waiver-def_table")
ns_ok   <- all(vapply(ns_ids, function(x) grepl(x, ui_html, fixed = TRUE),
                      logical(1)))
check(
  "UI namespaces risk_summary, lineup_table, and def_table under the module id",
  ns_ok
)

# ------------------------------------------------------------------------------
# 4. start_sit_uq cache present and carrying the read columns
# ------------------------------------------------------------------------------
start_sit <- if (file.exists(PATH_START_SIT_UQ)) {
  readRDS(PATH_START_SIT_UQ)
} else {
  NULL
}
ss_missing <- if (is.null(start_sit)) {
  START_SIT_REQUIRED
} else {
  setdiff(START_SIT_REQUIRED, names(start_sit))
}
ss_ok <- !is.null(start_sit) && length(ss_missing) == 0L && nrow(start_sit) > 0L
check(
  "start_sit_uq cache loads with the columns the lineup table reads",
  ss_ok,
  if (is.null(start_sit)) {
    "cache missing: run R/36 with save_output = TRUE"
  } else if (length(ss_missing) > 0L) {
    sprintf("missing: %s", paste(ss_missing, collapse = ", "))
  } else {
    sprintf("%d rows", nrow(start_sit))
  }
)

# ------------------------------------------------------------------------------
# 5. def_streaming cache in a valid state (present + columns, or legitimately
#    absent when the optimal lineup already starts a defense)
# ------------------------------------------------------------------------------
def_present <- file.exists(PATH_DEF_STREAMING)
def_stream  <- if (def_present) readRDS(PATH_DEF_STREAMING) else NULL
def_state_ok <- if (!def_present || is.null(def_stream) ||
                    nrow(def_stream) == 0L) {
  TRUE  # no open DEF slot to stream into; the tab shows its note, not a table
} else {
  length(setdiff(DEF_STREAMING_REQUIRED, names(def_stream))) == 0L
}
check(
  "def_streaming cache is in a valid state (present with columns, or absent)",
  def_state_ok,
  if (!def_present) {
    "absent: lineup starts a DEF, no open slot"
  } else if (is.null(def_stream) || nrow(def_stream) == 0L) {
    "empty"
  } else {
    sprintf("present, %d defenses", nrow(def_stream))
  }
)

# ------------------------------------------------------------------------------
# 6. Week-risk sum computes (same arithmetic the risk summary line uses)
# ------------------------------------------------------------------------------
risk_val <- if (!is.null(start_sit) && "expected_regret" %in% names(start_sit)) {
  sum(start_sit$expected_regret, na.rm = TRUE)
} else {
  NA_real_
}
check(
  "Week-risk sum of expected_regret is a finite, non-negative number",
  is.finite(risk_val) && risk_val >= 0,
  if (is.finite(risk_val)) {
    sprintf("%.1f expected points at risk", risk_val)
  } else {
    "expected_regret unavailable"
  }
)

# ------------------------------------------------------------------------------
# 7. Data layer exposes the slots the Tab 2 accessors read
# ------------------------------------------------------------------------------
app_data <- suppressWarnings(load_shiny_data())
slots_ok <- all(c("start_sit_uq", "def_streaming") %in% names(app_data))
check(
  "load_shiny_data() exposes the start_sit_uq and def_streaming slots",
  slots_ok,
  sprintf("slots: %s", paste(names(app_data), collapse = ", "))
)

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------

n_pass <- sum(vapply(results, function(x) x$passed, logical(1)))
n_tot  <- length(results)

cat(sprintf("\n%s\n", strrep("=", 70)))
cat(sprintf("Result: %d of %d checks passed.\n", n_pass, n_tot))
cat(sprintf("%s\n\n", strrep("=", 70)))

if (n_pass < n_tot) {
  failed <- vapply(results, function(x) if (!x$passed) x$label else NA_character_,
                   character(1))
  failed <- failed[!is.na(failed)]
  stop(
    sprintf("Week 20 smoke test failed %d of %d checks:\n  - %s",
            n_tot - n_pass, n_tot, paste(failed, collapse = "\n  - ")),
    call. = FALSE
  )
}

invisible(TRUE)
