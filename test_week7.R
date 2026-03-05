# ==============================================================================
# TEST RUNNER: WEEK 7 - GAME SCRIPT & LEVERAGE FEATURES
# ==============================================================================
# Run this file from the project root to execute all Week 7 tests.
# Delegates to tests/test_week7_functions.R for the full test suite.
#
# Usage:
#   source(here::here("test_week7.R"))
#
# Expected output: All tests pass, summary printed to console.
# ==============================================================================

library(here)
library(testthat)

cat("=============================================================\n")
cat("WEEK 7 TEST SUITE: Game Script & Leverage Features\n")
cat("=============================================================\n\n")

# Source production code
source(here::here("R", "09_gamescript_features.R"))

# Run full test suite
test_results <- test_file(
  here::here("tests", "test_week7_functions.R"),
  reporter = "summary"
)

cat("\n=============================================================\n")
cat("Week 7 tests complete.\n")
cat("=============================================================\n")
