# Week 1 Testing Script - Updated for 2025
# Tests the data loading functions

# Load packages
library(nflfastR)
library(tidyverse)

# Source our functions
# Load here package for robust path resolution
if (!requireNamespace("here", quietly = TRUE)) {
  message("Installing 'here' package for robust path resolution...")
  install.packages("here")
}
library(here)

# Source our functions using here::here() for robust paths
source(here("R", "01_data_loading.R"))

# Test 1: Load single season (most recent)
cat("\n=== Test 1: Load 2025 season ===\n")
pbp_2025 <- load_and_validate_pbp(2025)

# Verify it worked
cat("\nVerification:\n")
cat(glue::glue("Rows: {nrow(pbp_2025)}"), "\n")
cat(glue::glue("Columns: {ncol(pbp_2025)}"), "\n")
cat(glue::glue("Seasons: {paste(unique(pbp_2025$season), collapse=', ')}"), "\n")

# Test 2: Load from cache (should be fast)
cat("\n=== Test 2: Load from cache ===\n")
pbp_2025_cached <- load_and_validate_pbp(2025, force_reload = FALSE)

# Test 3: Load multiple recent seasons
cat("\n=== Test 3: Load multiple seasons ===\n")
pbp_recent <- load_and_validate_pbp(2024:2025)

# Verify
cat("\nVerification:\n")
cat(glue::glue("Rows: {nrow(pbp_recent)}"), "\n")
cat(glue::glue("Seasons: {paste(unique(pbp_recent$season), collapse=', ')}"), "\n")

# Test 4: Get roster data
cat("\n=== Test 4: Load roster data ===\n")
roster_2025 <- get_roster_data(2025)

# Verify
cat("\nVerification:\n")
cat(glue::glue("Players: {nrow(roster_2025)}"), "\n")
cat(glue::glue("Teams: {length(unique(roster_2025$team))}"), "\n")
cat(glue::glue("Positions: {paste(unique(roster_2025$position), collapse=', ')}"), "\n")

# Test 5: Validation function standalone
cat("\n=== Test 5: Run validation standalone ===\n")
validation <- validate_pbp_quality(pbp_2025)

cat("\nValidation Results:\n")
cat(glue::glue("Has critical issues: {validation$has_critical_issues}"), "\n")
cat(glue::glue("Total plays: {validation$summary$total_plays}"), "\n")
cat(glue::glue("Games: {validation$summary$games}"), "\n")

cat("\n=== All tests complete! ===\n")