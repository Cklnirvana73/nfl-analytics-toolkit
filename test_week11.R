# =============================================================================
# NFL Analytics Toolkit - Week 11 Root Test Runner
# =============================================================================
# File:    test_week11.R
# Purpose: Execute the full Week 11 test suite from the project root.
# Usage:   source("test_week11.R")
# =============================================================================

library(testthat)
library(here)

cat("Running Week 11 test suite...\n\n")

results <- testthat::test_file(
  here::here("tests", "test_week11_functions.R"),
  reporter = testthat::default_reporter()
)

passed   <- sum(as.data.frame(results)$passed)
failed   <- sum(as.data.frame(results)$failed)
warnings <- sum(as.data.frame(results)$warning)
skipped  <- sum(as.data.frame(results)$skipped)

cat("\n")
cat("===================================================================\n")
cat("Week 11 test results\n")
cat("===================================================================\n")
cat(glue::glue("Passed  : {passed}\n"))
cat(glue::glue("Failed  : {failed}\n"))
cat(glue::glue("Warnings: {warnings}\n"))
cat(glue::glue("Skipped : {skipped}\n"))
cat("===================================================================\n")

if (failed > 0L) {
  stop(glue::glue("{failed} test(s) failed. Review output above."), call. = FALSE)
} else {
  cat("All tests passed.\n")
}
