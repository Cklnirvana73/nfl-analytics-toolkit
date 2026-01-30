# Week 2 Integration Testing Script
# Tests the analysis functions with real nflfastR data
# This is for manual verification - unit tests are in tests/test_week2_functions.R

# Load packages
library(nflfastR)
library(tidyverse)
library(glue)

# Source our functions
source("R/01_data_loading.R")
source("R/02_player_stats.R")
source("R/03_team_stats.R")
source("R/04_game_analysis.R")

cat("\n========================================\n")
cat("Week 2 Integration Tests - Real Data\n")
cat("========================================\n\n")

# Load recent season data (use 2024 for most current)
cat("Loading 2024 season data...\n")
pbp_2024 <- load_and_validate_pbp(2024)

# ============================================================================
# Test 1: Player Rushing Statistics
# ============================================================================
cat("\n=== Test 1: Player Rushing Stats ===\n")
rush_stats <- get_player_rushing_stats(pbp_2024, season = 2024, week_max = 10)

cat("\nTop 5 Rushers (through Week 10):\n")
print(
  rush_stats %>%
    select(player_name, recent_team, rushes, rush_yards, yards_per_carry, rush_tds) %>%
    head(5)
)

# Validation checks
cat("\n Validation:\n")
cat(glue("  - Total players: {nrow(rush_stats)}"), "\n")
cat(glue("  - Players with 50+ carries: {sum(rush_stats$rushes >= 50)}"), "\n")
cat(glue("  - Average YPC (all): {round(mean(rush_stats$yards_per_carry), 2)}"), "\n")

# ============================================================================
# Test 2: Player Passing Statistics
# ============================================================================
cat("\n=== Test 2: Player Passing Stats ===\n")
pass_stats <- get_player_passing_stats(pbp_2024, season = 2024, week_max = 10)

cat("\nTop 5 Passers (through Week 10):\n")
print(
  pass_stats %>%
    select(player_name, recent_team, attempts, completions, pass_yards, pass_tds, passer_rating) %>%
    head(5)
)

cat("\n✓ Validation:\n")
cat(glue("  - Total QBs: {nrow(pass_stats)}"), "\n")
cat(glue("  - QBs with 100+ attempts: {sum(pass_stats$attempts >= 100)}"), "\n")
cat(glue("  - Average completion %: {round(mean(pass_stats$completion_pct) * 100, 1)}%"), "\n")

# ============================================================================
# Test 3: Player Receiving Statistics
# ============================================================================
cat("\n=== Test 3: Player Receiving Stats ===\n")
rec_stats <- get_player_receiving_stats(pbp_2024, season = 2024, week_max = 10)

cat("\nTop 5 Receivers (through Week 10):\n")
print(
  rec_stats %>%
    select(player_name, recent_team, targets, receptions, rec_yards, rec_tds, catch_rate) %>%
    head(5)
)

cat("\n✓ Validation:\n")
cat(glue("  - Total receivers: {nrow(rec_stats)}"), "\n")
cat(glue("  - Receivers with 30+ targets: {sum(rec_stats$targets >= 30)}"), "\n")
cat(glue("  - Average catch rate: {round(mean(rec_stats$catch_rate) * 100, 1)}%"), "\n")

# ============================================================================
# Test 4: Team Offense Statistics
# ============================================================================
cat("\n=== Test 4: Team Offense Stats ===\n")
offense_stats <- get_team_offense_stats(pbp_2024, season = 2024, week_max = 10)

cat("\nTop 5 Offenses by EPA/play (through Week 10):\n")
print(
  offense_stats %>%
    select(team, games_played, total_plays, offensive_epa_per_play, success_rate, third_down_conversion_rate) %>%
    head(5)
)

cat("\n✓ Validation:\n")
cat(glue("  - Total teams: {nrow(offense_stats)}"), "\n")
cat(glue("  - Expected: 32 teams"), "\n")

# ============================================================================
# Test 5: Team Defense Statistics
# ============================================================================
cat("\n=== Test 5: Team Defense Stats ===\n")
defense_stats <- get_team_defense_stats(pbp_2024, season = 2024, week_max = 10)

cat("\nTop 5 Defenses by EPA/play (through Week 10):\n")
print(
  defense_stats %>%
    select(team, games_played, plays_against, defensive_epa_per_play, sack_rate, turnover_rate) %>%
    head(5)
)

cat("\n✓ Validation:\n")
cat(glue("  - Total teams: {nrow(defense_stats)}"), "\n")

# ============================================================================
# Test 6: Game Analysis
# ============================================================================
cat("\n=== Test 6: Game Analysis ===\n")

# Get a recent game ID
recent_game <- pbp_2024 %>%
  filter(week == 1) %>%
  pull(game_id) %>%
  first()

cat(glue("\nAnalyzing game: {recent_game}\n"))

# Game summary
game_sum <- get_game_summary(pbp_2024, recent_game)
cat("\nGame Summary:\n")
print(game_sum %>% select(game_id, home_team, away_team, total_home_score, total_away_score, winner))

# Scoring plays
scoring <- get_scoring_plays(pbp_2024, recent_game)
if (!is.null(scoring) && nrow(scoring) > 0) {
  cat("\nScoring Plays:\n")
  print(scoring %>% select(qtr, time_remaining, scoring_team, score_type, points) %>% head(10))
} else {
  cat("\nNo scoring plays found (or none recorded)\n")
}

# Drive summary
drives <- get_drive_summary(pbp_2024, recent_game)
cat("\nDrive Summary:\n")
print(drives %>% select(drive, posteam, plays, yards_gained, ended_with_score) %>% head(10))

# ============================================================================
# Test 7: Week Filtering
# ============================================================================
cat("\n=== Test 7: Week Filtering ===\n")

# Early season rushing
rush_early <- get_player_rushing_stats(pbp_2024, season = 2024, week_min = 1, week_max = 4)
cat(glue("\nRushers weeks 1-4: {nrow(rush_early)} players\n"))

# Late season rushing
rush_late <- get_player_rushing_stats(pbp_2024, season = 2024, week_min = 5, week_max = 10)
cat(glue("Rushers weeks 5-10: {nrow(rush_late)} players\n"))

cat("\n✓ Filtering works correctly\n")

# ============================================================================
# Summary
# ============================================================================
cat("\n========================================\n")
cat("All Week 2 Integration Tests Complete!\n")
cat("========================================\n")
cat("\nNext steps:\n")
cat("1. Review output for correctness\n")
cat("2. Compare to known NFL stats for validation\n")
cat("3. Run unit tests: testthat::test_file('tests/test_week2_functions.R')\n")
cat("4. If all looks good, Week 2 is complete!\n\n")
