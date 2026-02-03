################################################################################
# NFL Analytics Toolkit - Week 2 Examples
# Educational examples demonstrating player stats, team stats, and game analysis
################################################################################

# This file contains 7 self-contained examples showing how to use Week 2 functions.
# Each example is runnable independently and demonstrates specific use cases.

# Setup: Load required packages and functions
library(dplyr)
library(nflfastR)

# Source Week 1 and Week 2 functions
# Note: Adjust paths if running from different directory
source("R/01_data_loading.R")
source("R/02_player_stats.R")
source("R/03_team_stats.R")
source("R/04_game_analysis.R")

# Load 2024 season data for all examples
cat("Loading 2024 NFL play-by-play data...\n")
pbp_2024 <- load_and_validate_pbp(seasons = 2024)
cat("Data loaded successfully!\n\n")

################################################################################
# Example 1: Basic Player Stats - Finding Rushing Leaders
################################################################################
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("EXAMPLE 1: Basic Player Stats - Top 10 Rushers Through Week 10\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Get rushing stats for first 10 weeks of 2024 season
rush_stats <- get_player_rushing_stats(
  pbp_data = pbp_2024,
  season = 2024,
  week_min = 1,
  week_max = 10
)

# Display top 10 rushers by total yards
top_rushers <- rush_stats %>%
  arrange(desc(rush_yards)) %>%
  select(player_name, recent_team, rushes, rush_yards, rush_tds, yards_per_carry) %>%
  head(10)

print(top_rushers)

cat("\n💡 KEY INSIGHT:\n")
cat("The get_player_rushing_stats() function aggregates all rushing plays\n")
cat("for each player across the specified weeks. The yards_per_carry metric\n")
cat("shows efficiency - it's the total rush_yards divided by rushes.\n")
cat("This helps distinguish high-volume runners from explosive ones.\n\n")

################################################################################
# Example 2: Multi-Position Analysis - Comparing Offensive Leaders
################################################################################
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("EXAMPLE 2: Multi-Position Analysis - Top 5 at Each Position\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Get stats for all three offensive positions
pass_stats <- get_player_passing_stats(pbp_2024, season = 2024, week_max = 10)
recv_stats <- get_player_receiving_stats(pbp_2024, season = 2024, week_max = 10)

# Top 5 quarterbacks by passing yards
cat("TOP 5 QUARTERBACKS (by passing yards):\n")
top_qbs <- pass_stats %>%
  arrange(desc(pass_yards)) %>%
  select(player_name, recent_team, completions, attempts, pass_yards, pass_tds, interceptions) %>%
  head(5)
print(top_qbs)

cat("\n")

# Top 5 receivers by receiving yards
cat("TOP 5 RECEIVERS (by receiving yards):\n")
top_receivers <- recv_stats %>%
  arrange(desc(rec_yards)) %>%
  select(player_name, recent_team, targets, receptions, rec_yards, rec_tds, yards_per_reception) %>%
  head(5)
print(top_receivers)

cat("\n💡 KEY INSIGHT:\n")
cat("Each position function follows the same parameter structure but returns\n")
cat("position-specific metrics. This consistency makes it easy to compare\n")
cat("players across different positions using the same workflow.\n\n")

################################################################################
# Example 3: Team Comparison - Offensive vs Defensive Efficiency
################################################################################
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("EXAMPLE 3: Team Analysis - Offensive vs Defensive Efficiency\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Get team-level stats
offense_stats <- get_team_offense_stats(pbp_2024, season = 2024, week_max = 10)
defense_stats <- get_team_defense_stats(pbp_2024, season = 2024, week_max = 10)

# Top 5 offenses by EPA per play
cat("TOP 5 OFFENSES (by EPA per play):\n")
top_offenses <- offense_stats %>%
  arrange(desc(offensive_epa_per_play)) %>%
  select(team, total_plays, offensive_epa_per_play, success_rate, 
         yards_per_play, third_down_conversion_rate) %>%
  head(5)
print(top_offenses)

cat("\n")

# Top 5 defenses by EPA per play (most negative = best)
cat("TOP 5 DEFENSES (by EPA per play - most negative is best):\n")
top_defenses <- defense_stats %>%
  arrange(defensive_epa_per_play) %>%
  select(team, plays_against, defensive_epa_per_play, 
         sack_rate, turnover_rate) %>%
  head(5)
print(top_defenses)

cat("\n💡 KEY INSIGHT:\n")
cat("EPA (Expected Points Added) per play measures how much a team changes\n")
cat("their scoring expectation on each play. For offenses, positive is good.\n")
cat("For defenses, negative is good (they're preventing expected points).\n")
cat("This metric accounts for down, distance, and field position context.\n\n")

################################################################################
# Example 4: Single Game Deep Dive - Analyzing One Complete Game
################################################################################
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("EXAMPLE 4: Game Analysis - Complete Breakdown of a Single Game\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Find a game ID from the data
available_games <- pbp_2024 %>%
  filter(!is.na(game_id)) %>%
  distinct(game_id, home_team, away_team) %>%
  head(1)

if (nrow(available_games) > 0) {
  example_game_id <- available_games$game_id[1]
  
  cat(sprintf("Analyzing game: %s vs %s (game_id: %s)\n\n", 
              available_games$away_team[1], 
              available_games$home_team[1],
              example_game_id))
  
  # Get game summary
  cat("GAME SUMMARY:\n")
  game_summary <- get_game_summary(pbp_2024, example_game_id)
  print(game_summary)
  
  cat("\n")
  
  # Get scoring plays
  cat("SCORING PLAYS:\n")
  scoring <- get_scoring_plays(pbp_2024, example_game_id)
  print(scoring %>% select(qtr, time_remaining, scoring_team, score_type, points, desc))
  
  cat("\n")
  
  # Get drive summary
  cat("DRIVE SUMMARY (first 5 drives):\n")
  drives <- get_drive_summary(pbp_2024, example_game_id)
  print(drives %>% head(5) %>% select(drive, posteam, plays, yards_gained, ended_with_score))
  
} else {
  cat("No games available in dataset for analysis.\n")
}

cat("\n💡 KEY INSIGHT:\n")
cat("Game analysis functions break down a single game into multiple views:\n")
cat("- get_game_summary() provides high-level statistics\n")
cat("- get_scoring_plays() shows the scoring timeline\n")
cat("- get_drive_summary() reveals offensive efficiency drive-by-drive\n")
cat("Together, these tell the complete story of how a game unfolded.\n\n")

################################################################################
# Example 5: Time-Period Filtering - Early vs Late Season Performance
################################################################################
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("EXAMPLE 5: Time-Period Filtering - Early Season vs Late Season\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Compare early season (weeks 1-4) vs late season (weeks 11-14) for rushing
early_season_rush <- get_player_rushing_stats(
  pbp_data = pbp_2024,
  season = 2024,
  week_min = 1,
  week_max = 4
)

late_season_rush <- get_player_rushing_stats(
  pbp_data = pbp_2024,
  season = 2024,
  week_min = 11,
  week_max = 14
)

# Find players with data in both periods (minimum 20 rushes per period)
early_qualified <- early_season_rush %>%
  filter(rushes >= 20) %>%
  select(player_id, early_ypc = yards_per_carry, early_rushes = rushes)

late_qualified <- late_season_rush %>%
  filter(rushes >= 20) %>%
  select(player_id, late_ypc = yards_per_carry, late_rushes = rushes)

# Join and calculate difference
ypc_comparison <- inner_join(early_qualified, late_qualified, by = "player_id") %>%
  mutate(ypc_change = late_ypc - early_ypc) %>%
  arrange(desc(ypc_change)) %>%
  head(10)

# Add player names back from original data for display
ypc_comparison <- ypc_comparison %>%
  left_join(
    early_season_rush %>% select(player_id, player_name),
    by = "player_id"
  ) %>%
  select(player_name, early_ypc, late_ypc, ypc_change, early_rushes, late_rushes)

cat("TOP 10 RUSHERS WHO IMPROVED YARDS PER CARRY (early season to late season):\n")
print(ypc_comparison)

cat("\n💡 KEY INSIGHT:\n")
cat("The week_min and week_max parameters let you analyze specific time periods.\n")
cat("This is useful for identifying trends - do players improve as the season\n")
cat("progresses? Do teams change their offensive approach? The parameters give\n")
cat("you flexibility to slice the data however you need.\n\n")

################################################################################
# Example 6: Multi-Function Workflow - Player Performance in Team Context
################################################################################
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("EXAMPLE 6: Integration - Player Stats Within Team Context\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Get top offensive teams by total EPA
top_offenses_for_context <- offense_stats %>%
  arrange(desc(offensive_epa_per_play)) %>%
  select(team, offensive_epa_per_play, total_plays, rush_attempts, pass_attempts) %>%
  head(5)

cat("TOP 5 OFFENSES (by EPA per play):\n")
print(top_offenses_for_context)

cat("\n")

# For the top offense, find their leading rusher
if (nrow(top_offenses_for_context) > 0) {
  top_offense_team <- top_offenses_for_context$team[1]
  
  team_rushers <- rush_stats %>%
    filter(recent_team == top_offense_team) %>%
    arrange(desc(rush_yards)) %>%
    select(player_name, rushes, rush_yards, rush_tds, yards_per_carry) %>%
    head(3)
  
  cat(sprintf("TOP 3 RUSHERS FOR %s:\n", top_offense_team))
  print(team_rushers)
}

cat("\n💡 KEY INSIGHT:\n")
cat("Combining team-level and player-level functions provides context.\n")
cat("A high-performing rushing offense might be driven by one star RB\n")
cat("or distributed across multiple players. Understanding both the team's\n")
cat("aggregate performance and individual contributions tells a richer story.\n\n")

################################################################################
# Example 7: Edge Case Handling - Data Validation and Empty Results
################################################################################
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("EXAMPLE 7: Edge Cases - Handling Missing Data and Bye Weeks\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Test with a week range that might have no data (future weeks)
future_week_test <- get_player_rushing_stats(
  pbp_data = pbp_2024,
  season = 2024,
  week_min = 50,
  week_max = 52
)

cat("Testing with future weeks (50-52) that have no data:\n")
cat(sprintf("Number of rows returned: %d\n", nrow(future_week_test)))
cat(sprintf("Number of columns: %d\n", ncol(future_week_test)))

cat("\n")

# Test with single week
single_week_test <- get_player_rushing_stats(
  pbp_data = pbp_2024,
  season = 2024,
  week_min = 1,
  week_max = 1
)

cat("Testing with single week (Week 1 only):\n")
cat(sprintf("Number of players with rushing attempts: %d\n", nrow(single_week_test)))

if (nrow(single_week_test) > 0) {
  cat("\nTop 3 Week 1 rushers:\n")
  print(single_week_test %>% 
          arrange(desc(rush_yards)) %>%
          select(player_name, recent_team, rushes, rush_yards) %>%
          head(3))
}

cat("\n💡 KEY INSIGHT:\n")
cat("All Week 2 functions handle edge cases gracefully:\n")
cat("- Empty date ranges return empty data frames (not errors)\n")
cat("- Single week analysis works the same as multi-week\n")
cat("- NA values in player IDs or team names are filtered out\n")
cat("This defensive programming ensures your analysis code won't break\n")
cat("when encountering unusual data situations.\n\n")

################################################################################
# SUMMARY: Best Practices for Using Week 2 Functions
################################################################################
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY: Best Practices\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("1. ALWAYS specify season and week parameters explicitly\n")
cat("   - Don't rely on defaults; be clear about your time period\n\n")

cat("2. Filter for minimum thresholds to avoid small sample noise\n")
cat("   - Example: filter(rushes >= 20) for meaningful rushing stats\n\n")

cat("3. Combine functions to tell complete stories\n")
cat("   - Use player + team functions together for context\n")
cat("   - Use game functions to validate season-long trends\n\n")

cat("4. Check for NA values after filtering\n")
cat("   - Functions remove NAs during aggregation, but always verify\n\n")

cat("5. Use arrange() and head() to focus on top performers\n")
cat("   - Large datasets are overwhelming; filter to what matters\n\n")

cat("6. Leverage week_min/week_max for time-based analysis\n")
cat("   - Compare early vs late season, pre-injury vs post-injury, etc.\n\n")

cat("7. Validate your results against known outcomes\n")
cat("   - Cross-check with NFL.com stats or other sources\n\n")

cat("\nThese functions provide the foundation for Week 3 (rolling averages)\n")
cat("and Week 4 (fantasy scoring). Master these patterns now to build more\n")
cat("complex analyses later.\n\n")

cat("End of examples. All code above is runnable and self-contained.\n")