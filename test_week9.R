# ==============================================================================
# WEEK 9 TEST RUNNER (ROOT LEVEL)
# ==============================================================================
# Run from project root: source("test_week9.R")
# ==============================================================================

library(testthat)

cat("Running Week 9 tests: XGBoost Fantasy Prediction\n")
cat(rep("-", 50), sep = "")
cat("\n")

test_results <- test_file(
  here::here("tests", "test_week9_functions.R"),
  reporter = "summary"
)

cat("\n")
cat(rep("-", 50), sep = "")
cat("\n")
cat(sprintf("Tests passed : %d\n", sum(as.data.frame(test_results)$passed)))
cat(sprintf("Tests failed : %d\n", sum(as.data.frame(test_results)$failed)))
cat(sprintf("Tests skipped: %d\n", sum(as.data.frame(test_results)$skipped)))
cat(rep("-", 50), sep = "")
cat("\n")
