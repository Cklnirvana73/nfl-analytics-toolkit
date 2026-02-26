# ==============================================================================
# WEEK 5: EXAMPLES - EPA TREND FEATURES & SITUATIONAL SPLITS
# ==============================================================================
# 7 self-contained examples demonstrating Week 5 functions
#
# Examples:
# 1. Basic EPA trends with trend windows
# 2. Identifying improving vs declining players
# 3. Situational splits by game script
# 4. Down/distance EPA analysis
# 5. Metric stability for feature selection
# 6. Pressure performance (high-leverage situations)
# 7. Combining trends + splits for player profiles
#
# Dependencies: R/01_data_loading.R, R/07_epa_features.R
# ==============================================================================

library(here)
library(dplyr)

# Source required files
source(here("R", "01_data_loading.R"))
source(here("R", "07_epa_features.R"))

# Load data
pbp <- load_and_validate_pbp(2025)


# ==============================================================================
# EXAMPLE 1: Basic EPA Trends
# ==============================================================================

cat("\n=== Example 1: Basic EPA Trends ===\n\n")

# Calculate EPA trends with 3-game and 4-game windows
# filter_garbage_time = TRUE (default) excludes WP <10% or >90% in Q4
epa_trends <- calculate_epa_trends(
  pbp,
  season = 2025,
  trend_windows = c(3, 4),
  min_plays_per_game = 5
)

cat("Output columns:\n")
cat(paste(names(epa_trends), collapse = ", "), "\n\n")

# Verify column names before using them
stopifnot("epa_trend_3" %in% names(epa_trends))
stopifnot("epa_trend_4" %in% names(epa_trends))
stopifnot("epa_level_3" %in% names(epa_trends))

cat("Dimensions:", nrow(epa_trends), "rows x", ncol(epa_trends), "columns\n")
cat("Players:", n_distinct(epa_trends$player_id), "\n")
cat("Weeks:", paste(range(epa_trends$week), collapse = " to "), "\n\n")

# NFL Context: epa_trend is the OLS slope of EPA/play over previous N games.
# Positive = improving, negative = declining.
# Units: EPA/play change per game-week.


# ==============================================================================
# EXAMPLE 2: Identifying Improving vs Declining Players
# ==============================================================================

cat("\n=== Example 2: Top Improving & Declining Players ===\n\n")

# Get the most recent valid trend for each player
latest <- epa_trends %>%
  filter(!is.na(epa_trend_4)) %>%
  group_by(player_id, player_name) %>%
  filter(week == max(week)) %>%
  ungroup()

cat("Top 5 Improving (4-game EPA trend):\n")
latest %>%
  arrange(desc(epa_trend_4)) %>%
  select(player_name, week, epa_per_play, epa_trend_4, epa_level_4) %>%
  head(5) %>%
  print()

cat("\nTop 5 Declining (4-game EPA trend):\n")
latest %>%
  arrange(epa_trend_4) %>%
  select(player_name, week, epa_per_play, epa_trend_4, epa_level_4) %>%
  head(5) %>%
  print()

# Key insight: A player with positive epa_trend_4 but negative epa_level_4
# is improving FROM a bad baseline -- interesting buy-low candidate.


# ==============================================================================
# EXAMPLE 3: Game Script Splits
# ==============================================================================

cat("\n=== Example 3: Game Script Splits ===\n\n")

splits <- get_situational_splits(pbp, season = 2025)

# Verify columns
stopifnot(all(c("split_type", "split_value", "epa_per_play") %in% names(splits)))

# Game script analysis: Who performs differently when leading vs trailing?
script_splits <- splits %>%
  filter(split_type == "game_script", plays >= 20)

cat("Game Script EPA (min 20 plays per split):\n\n")

# Show players with biggest leading-vs-trailing gap
# NOTE: A player may have multiple rows per split_value (e.g., traded mid-season,
# or attributed plays on multiple teams). Aggregate first to avoid list-columns.
wide_script <- script_splits %>%
  group_by(player_id, player_name, split_value) %>%
  summarise(epa_per_play = mean(epa_per_play, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = split_value, values_from = epa_per_play) %>%
  filter(!is.na(leading), !is.na(trailing)) %>%
  mutate(script_gap = leading - trailing) %>%
  arrange(desc(abs(script_gap)))

cat("Biggest game script dependency (leading - trailing EPA):\n")
wide_script %>%
  select(player_name, leading, neutral, trailing, script_gap) %>%
  head(10) %>%
  print()

# NFL Context: Leading teams run 55-60% vs 40-45% when trailing.
# RBs inflate stats when ahead; WRs may pad stats in garbage time.


# ==============================================================================
# EXAMPLE 4: Down/Distance EPA Analysis
# ==============================================================================

cat("\n=== Example 4: Down/Distance EPA ===\n\n")

down_splits <- splits %>%
  filter(split_type == "down", plays >= 20)

cat("League-average EPA by down (all qualified players):\n")
down_splits %>%
  group_by(split_value) %>%
  summarise(
    avg_epa = round(mean(epa_per_play, na.rm = TRUE), 4),
    avg_success = round(mean(success_rate, na.rm = TRUE), 3),
    players = n_distinct(player_id),
    .groups = "drop"
  ) %>%
  print()

distance_splits <- splits %>%
  filter(split_type == "distance", plays >= 15)

cat("\nLeague-average EPA by distance bucket:\n")
distance_splits %>%
  group_by(split_value) %>%
  summarise(
    avg_epa = round(mean(epa_per_play, na.rm = TRUE), 4),
    avg_success = round(mean(success_rate, na.rm = TRUE), 3),
    players = n_distinct(player_id),
    .groups = "drop"
  ) %>%
  print()

# NFL Context: 3rd down is the highest-leverage regular-down situation.
# Distance buckets: short (1-3) = likely run, long (8+) = obvious passing.


# ==============================================================================
# EXAMPLE 5: Metric Stability for Feature Selection
# ==============================================================================

cat("\n=== Example 5: Metric Stability Analysis ===\n\n")

stability <- calculate_stability_metrics(pbp, season = 2025, min_games = 6)

cat("Metric stability rankings (week-to-week autocorrelation):\n\n")
stability %>%
  select(metric, autocorrelation, split_half_r, stability_tier) %>%
  print(n = 20)

cat("\nStability tier summary:\n")
stability %>%
  group_by(stability_tier) %>%
  summarise(count = n(), .groups = "drop") %>%
  print()

# Key insight from Week 4 findings: volume metrics (yards, targets) are
# more stable than efficiency metrics (EPA/play). This means volume features
# should receive more weight in ML models.
# Experimental: stability rankings may differ with different sample sizes.


# ==============================================================================
# EXAMPLE 6: Pressure Performance
# ==============================================================================

cat("\n=== Example 6: Pressure Performance ===\n\n")

pressure <- get_pressure_performance(pbp, season = 2025)

# Verify columns
stopifnot(all(c("leverage_type", "leverage_value", "epa_per_play") %in% names(pressure)))

# Q4 close games: who steps up?
q4_close <- pressure %>%
  filter(leverage_type == "quarter_score", leverage_value == "q4_close",
         plays >= 15) %>%
  arrange(desc(epa_per_play))

cat("Top performers in Q4 close games (score diff <= 8, min 15 plays):\n")
q4_close %>%
  select(player_name, plays, epa_per_play, success_rate) %>%
  head(10) %>%
  print()

# Red zone comparison
cat("\nRed zone vs outside red zone (league average):\n")
pressure %>%
  filter(leverage_type == "red_zone") %>%
  group_by(leverage_value) %>%
  summarise(
    avg_epa = round(mean(epa_per_play, na.rm = TRUE), 4),
    avg_success = round(mean(success_rate, na.rm = TRUE), 3),
    total_players = n_distinct(player_id),
    .groups = "drop"
  ) %>%
  print()

# Sample size caution: leverage splits have small samples by definition.
# A player with 15-20 plays has wide confidence intervals.
# These features are experimental for ML models.


# ==============================================================================
# EXAMPLE 7: Combined Player Profile
# ==============================================================================

cat("\n=== Example 7: Combined Player Profile ===\n\n")

# Build a comprehensive view by combining trends + splits + pressure
# This demonstrates how Week 5 features create a richer player picture

# Step 1: Get player's latest trend
player_trend <- epa_trends %>%
  filter(!is.na(epa_trend_4)) %>%
  group_by(player_id, player_name) %>%
  filter(week == max(week)) %>%
  ungroup() %>%
  select(player_id, player_name, posteam, epa_per_play, epa_trend_4, epa_level_4)

# Step 2: Get their game script dependency
script_wide <- splits %>%
  filter(split_type == "game_script", plays >= 10) %>%
  group_by(player_id, split_value) %>%
  summarise(epa_per_play = mean(epa_per_play, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = split_value, values_from = epa_per_play,
                     names_prefix = "script_")

# Step 3: Get their 3rd down performance
third_down <- pressure %>%
  filter(leverage_type == "third_down", leverage_value == "third_down",
         plays >= 10) %>%
  select(player_id, third_down_epa = epa_per_play, third_down_plays = plays)

# Combine
profile <- player_trend %>%
  left_join(script_wide, by = "player_id") %>%
  left_join(third_down, by = "player_id") %>%
  filter(!is.na(script_neutral) | !is.na(third_down_epa))

cat("Top 10 comprehensive player profiles:\n")
profile %>%
  arrange(desc(epa_per_play)) %>%
  head(10) %>%
  print(width = 120)

cat("\nThis combined view shows:\n")
cat("  - Current EPA level and trend direction\n")
cat("  - Game script sensitivity (leading vs trailing EPA)\n")
cat("  - 3rd down performance (clutch situations)\n")
cat("  - All features are EXPERIMENTAL until validated in ML models (Week 9+)\n\n")


# ==============================================================================
# BEST PRACTICES SUMMARY
# ==============================================================================

cat("\n=== Week 5 Best Practices ===\n\n")
cat("1. ALWAYS verify column names before using: names(result)\n")
cat("2. Filter garbage time for efficiency metrics (default in all functions)\n")
cat("3. Check play counts in splits -- small samples mean wide confidence intervals\n")
cat("4. Trend slopes need >= 3 games; use 4+ for more stability\n")
cat("5. EPA trends use lagged data only (no feature leakage)\n")
cat("6. Game script splits reveal script-dependent players (especially RBs)\n")
cat("7. Stability metrics guide feature selection: use stable metrics for prediction\n")
cat("8. Pressure performance has inherently small samples -- use cautiously\n")
cat("9. All features are EXPERIMENTAL until validated against actual outcomes\n")
cat("10. Compare to benchmarks: league avg EPA ~0.0 rush, ~+0.08 pass\n\n")
