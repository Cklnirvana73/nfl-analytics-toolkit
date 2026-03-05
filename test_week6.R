# ==============================================================================
# WEEK 6: TEST RUNNER (Root Level)
# ==============================================================================
# Quick test runner for Week 6 usage features.
# For full test suite: source("tests/test_week6_functions.R")
#
# Usage:
#   source("test_week6.R")
#
# ==============================================================================

library(testthat)
library(here)

source(here("R", "08_usage_features.R"))

cat("Running Week 6 tests...\n\n")

test_results <- tryCatch(
  test_file(here("tests", "test_week6_functions.R"), reporter = "summary"),
  error = function(e) { cat("Test runner error:", conditionMessage(e), "\n"); NULL }
)

cat("\n=== Week 6 Test Summary ===\n")

if (!is.null(test_results)) {
  # test_file returns a data frame of results in testthat >= 3.0
  results_df <- as.data.frame(test_results)
  n_passed   <- sum(results_df$passed,  na.rm = TRUE)
  n_failed   <- sum(results_df$failed,  na.rm = TRUE)
  n_warnings <- sum(results_df$warning, na.rm = TRUE)
  n_skipped  <- sum(results_df$skipped, na.rm = TRUE)

  cat(glue::glue(
    "Passed:   {n_passed}\n",
    "Failed:   {n_failed}\n",
    "Warnings: {n_warnings}\n",
    "Skipped:  {n_skipped}\n"
  ))

  if (n_failed == 0) {
    cat("\nAll Week 6 tests passed. Production-ready.\n")
  } else {
    cat(glue::glue("\n{n_failed} test(s) failed. Review output above.\n"))
  }
} else {
  cat("Could not retrieve test results. Check error output above.\n")
}
