# ==============================================================================
# test_week8.R -- Root-level test runner for Week 8
# ==============================================================================
# Run this file to execute all Week 8 tests.
# Usage: source(here::here("test_week8.R"))
# ==============================================================================

library(testthat)
library(here)

cat("Running Week 8 tests...\n")
cat(strrep("-", 50), "\n")

result <- tryCatch(
  testthat::test_file(
    here("tests", "test_week8_functions.R"),
    reporter = "summary"
  ),
  error = function(e) {
    cat("Runner error:", conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(result)) {
  results_df <- as.data.frame(result)
  n_passed <- sum(results_df$passed, na.rm = TRUE)
  n_failed <- sum(results_df$failed, na.rm = TRUE)
  n_warn   <- sum(results_df$warning, na.rm = TRUE)
  n_skip   <- sum(results_df$skipped, na.rm = TRUE)

  cat(strrep("-", 50), "\n")
  cat(glue::glue("Week 8 results: {n_passed} passed | {n_failed} failed | ",
                 "{n_warn} warnings | {n_skip} skipped\n"))

  if (n_failed == 0) {
    cat("All tests passed.\n")
  } else {
    cat("FAILURES DETECTED -- review output above.\n")
  }
}
