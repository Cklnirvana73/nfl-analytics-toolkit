# ==============================================================================
# WEEK 5: ROOT-LEVEL TEST RUNNER
# ==============================================================================
# Run all Week 5 tests and display summary
#
# Usage: source("test_week5.R")
# ==============================================================================

library(testthat)
library(here)

cat("\n")
cat("================================================================\n")
cat("  NFL Analytics Toolkit - Week 5 Test Suite\n")
cat("  EPA Trend Features & Situational Splits\n")
cat("================================================================\n\n")

# Source production code
source(here("R", "07_epa_features.R"))

# Run test file
test_results <- test_file(
  here("tests", "test_week5_functions.R"),
  reporter = "summary"
)

cat("\n")
cat("================================================================\n")
cat("  Week 5 Test Summary\n")
cat("================================================================\n")
cat("  Total tests: ", length(test_results), "\n")
cat("  Passed:      ", sum(as.data.frame(test_results)$passed > 0), "\n")
cat("  Failed:      ", sum(as.data.frame(test_results)$failed > 0), "\n")
cat("================================================================\n\n")
