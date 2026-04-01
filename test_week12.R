# ==============================================================================
# test_week12.R
# NFL Analytics Toolkit - Week 12: Projection System Test Runner
#
# Root-level test runner. Sources the full test suite in tests/ directory.
# Run this file to execute all Week 12 tests.
#
# Usage:
#   source("test_week12.R")        # from R console
#   Ctrl+Shift+Enter               # from RStudio
#
# Prerequisites:
#   R/14_projection_system.R       (production code)
#   output/ml_data_2025.rds        (optional -- some tests skipped if absent)
#
# Test coverage:
#   - compute_prior_weight()              5 tests
#   - validate_regular_season_week()      3 tests
#   - standardise_prediction_columns()    7 tests
#   - derive_opponent_difficulty()         5 tests
#   - generate_preseason_projections()     7 tests
#   - generate_ros_projections()           7 tests
#   - NFL edge cases                       2 tests
#   - Column verification                  2 tests
#   - Traded player dedup                  1 test
#   - Integration (preseason -> ROS)       1 test
#   - Week 4 finding validation            1 test
#   - Feature matrix artifact              1 test
#   Total: 42 tests across 12 groups
# ==============================================================================

cat("\n")
cat("##############################################################\n")
cat("# NFL Analytics Toolkit - Week 12 Test Suite                 #\n")
cat("# Projection System: 14_projection_system.R                  #\n")
cat("##############################################################\n\n")

library(testthat)
library(here)

test_file <- here::here("tests", "test_week12_functions.R")

if (!file.exists(test_file)) {
  stop("Test file not found: ", test_file,
       "\nExpected at: tests/test_week12_functions.R")
}

cat("Running test suite...\n\n")

results <- testthat::test_file(
  test_file,
  reporter = testthat::SummaryReporter$new()
)

cat("\n")

# Extract pass/fail counts
n_tests  <- length(results)
n_passed <- sum(sapply(results, function(r) length(r$results) > 0 &&
                          all(sapply(r$results, inherits, "expectation_success"))))
n_failed <- n_tests - n_passed

cat(glue::glue(
  "RESULTS: {n_passed} passed / {n_failed} failed / {n_tests} total\n\n"
))

if (n_failed > 0) {
  cat("FAILED TESTS:\n")
  for (r in results) {
    failures <- Filter(function(x) !inherits(x, "expectation_success"), r$results)
    if (length(failures) > 0) {
      cat(paste0("  - ", r$test, "\n"))
      for (f in failures) {
        cat(paste0("    ", conditionMessage(f), "\n"))
      }
    }
  }
}

cat("##############################################################\n")
