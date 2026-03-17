# ==============================================================================
# test_week10.R
# Week 10: Boom / Bust Classification -- Root-Level Test Runner
# NFL Analytics Toolkit
#
# USAGE
# Open in RStudio. Source with Ctrl+Shift+Enter.
# Runs the full test suite in tests/test_week10_functions.R and prints summary.
#
# PREREQUISITES
# output/feature_matrix_2025.rds   -- from R/10_opponent_features.R
# output/week9_predictions_2025.rds -- from R/11_xgboost_fantasy.R
# data/cache/pbp_2025_2025.rds     -- from load_and_validate_pbp(2025)
# ==============================================================================

library(here)
library(testthat)

cat("=======================================================\n")
cat("Week 10 Test Suite: Boom / Bust Classification\n")
cat("=======================================================\n\n")

start_time <- proc.time()

test_results <- test_file(
  here::here("tests", "test_week10_functions.R"),
  reporter = "progress"
)

elapsed <- round((proc.time() - start_time)[["elapsed"]], 1)

cat("\n=======================================================\n")
cat("Week 10 Test Suite Complete\n")
cat(sprintf("Elapsed: %s seconds\n", elapsed))
cat("=======================================================\n")
