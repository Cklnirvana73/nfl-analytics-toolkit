# ==============================================================================
# WEEK 6: EXAMPLES
# examples/example_week6.R
# ==============================================================================
# 7 self-contained examples demonstrating Week 6 usage features.
#
# Each example is independent and runnable in isolation.
# Column names are verified against function source before use.
#
# Dependencies: dplyr, glue, tidyr (for pivot_wider in depth profile)
# Data: nflfastR 2025 play-by-play (or substitute any season)
#
# ==============================================================================

library(dplyr)
library(glue)
library(here)

source(here("R", "01_data_loading.R"))
source(here("R", "08_usage_features.R"))

message("Loading 2025 play-by-play...")
pbp <- load_and_validate_pbp(seasons = 2025)


# ==============================================================================
# EXAMPLE 1: BASIC USAGE METRICS
# Target share, rush share, air yards share per player per game-week
#
# NFL Context: Target share is an opportunity metric, not an efficiency metric.
# A receiver with 25% target share is getting their fair share of looks
# regardless of whether they're catching the ball. This drives production
# in the same way volume drives results for running backs.
# ==============================================================================
cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 1: Basic Usage Metrics (Season Snapshot)\n")
cat(strrep("=", 60), "\n\n")

usage <- calculate_usage_metrics(pbp, season = 2025, week_min = 1, week_max = 10)

# Available columns (verified):
# season, week, game_id, player_id, player_name, position_group, team,
# targets, team_targets, target_share,
# rushes, team_rushes, rush_share,
# air_yards, team_air_yards, air_yards_share,
# redzone_targets, redzone_rushes, redzone_opportunities,
# team_redzone_plays, redzone_share

# Top receivers by average target share, Weeks 1-10
top_receivers <- usage %>%
  filter(position_group == "receiver", !is.na(target_share)) %>%
  group_by(player_id, player_name, team) %>%
  summarise(
    games            = n_distinct(game_id),
    avg_target_share = mean(target_share, na.rm = TRUE),
    avg_air_yards_share = mean(air_yards_share, na.rm = TRUE),
    total_targets    = sum(targets, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(games >= 5) %>%
  arrange(desc(avg_target_share))

cat("Top 10 Receivers by Average Target Share (Weeks 1-10, min 5 games):\n\n")
print(
  top_receivers %>%
    select(player_name, team, games, total_targets,
           avg_target_share, avg_air_yards_share) %>%
    mutate(across(c(avg_target_share, avg_air_yards_share),
                  ~ paste0(round(. * 100, 1), "%"))) %>%
    head(10)
)

cat(glue("\nKey Insight: A target share above 20% is typically a WR1 workload.\n",
         "Air yards share > target share = downfield role (bigger plays when targeted).\n"))


# ==============================================================================
# EXAMPLE 2: RUSH SHARE LEADERS
# Identifying backs with workhorse volume
#
# NFL Context: Rush share predicts RB fantasy ceiling more than efficiency.
# A back with 65%+ rush share on a quality offense is the most reliable
# RB fantasy asset -- the Week 4 predictive validity study confirmed this.
# ==============================================================================
cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 2: Rush Share Leaders\n")
cat(strrep("=", 60), "\n\n")

rush_leaders <- usage %>%
  filter(position_group == "rusher", !is.na(rush_share)) %>%
  group_by(player_id, player_name, team) %>%
  summarise(
    games          = n_distinct(game_id),
    avg_rush_share = mean(rush_share, na.rm = TRUE),
    total_rushes   = sum(rushes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(games >= 5, total_rushes >= 30) %>%
  arrange(desc(avg_rush_share))

cat("Top 10 Running Backs by Rush Share (Weeks 1-10, min 30 rushes):\n\n")
print(
  rush_leaders %>%
    select(player_name, team, games, total_rushes, avg_rush_share) %>%
    mutate(avg_rush_share = paste0(round(avg_rush_share * 100, 1), "%")) %>%
    head(10)
)

cat(glue("\nKey Insight: Rush share >= 60% within a game = 'three-down back' role.\n",
         "Below 40% with a committee backfield suppresses weekly ceiling.\n"))


# ==============================================================================
# EXAMPLE 3: USAGE TRENDS -- IDENTIFYING EXPANDING ROLES
# Rolling target share change over recent weeks
#
# NFL Context: A receiver whose target share is trending up from Week 7-9
# is often a better buy-low candidate than one with a stable high share,
# because the market (waiver wire, trade value) may not yet reflect the
# emerging role.
# ==============================================================================
cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 3: Usage Trends -- Identifying Expanding Roles\n")
cat(strrep("=", 60), "\n\n")

trends <- calculate_usage_trends(usage)

# Available columns (verified):
# season, week, player_id, player_name, position_group, team,
# target_share, rush_share, air_yards_share,
# target_share_roll3, target_share_roll4,
# rush_share_roll3, rush_share_roll4,
# target_share_delta1, rush_share_delta1, air_yards_share_delta1,
# role_trend

expanding_receivers <- trends %>%
  filter(
    position_group == "receiver",
    role_trend == "expanding",
    week >= 7,                          # enough history to trust the trend
    !is.na(target_share_roll4)
  ) %>%
  group_by(player_id, player_name, team) %>%
  filter(week == max(week)) %>%         # most recent week only
  ungroup() %>%
  arrange(desc(target_share_roll4))

cat("Receivers with Expanding Roles (most recent week, min Week 7):\n\n")
print(
  expanding_receivers %>%
    select(player_name, team, week, target_share, target_share_roll4,
           target_share_delta1, role_trend) %>%
    mutate(across(c(target_share, target_share_roll4, target_share_delta1),
                  ~ paste0(round(. * 100, 1), "%"))) %>%
    head(10)
)

cat(glue("\nKey Insight: role_trend is based on 4-week rolling average vs prior 4 weeks.\n",
         "Expanding = current 4-wk avg > prior 4-wk avg by >2 percentage points.\n",
         "All rolling calculations use lagged data (no feature leakage).\n"))


# ==============================================================================
# EXAMPLE 4: RED ZONE PROFILE -- SEASONAL SUMMARY
# Who are the true red zone focal points?
#
# NFL Context: TDs score 6 points vs 0.5 pts/yard (PPR). A player with
# heavy inside-10 usage is worth 3-4x more per opportunity than one
# targeted at the 18-yard line. The rz5_td_rate identifies true goal-line
# backs and receivers.
# ==============================================================================
cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 4: Red Zone Profile -- Seasonal Summary\n")
cat(strrep("=", 60), "\n\n")

rz <- get_redzone_profile(pbp, season = 2025, week_min = 1, week_max = 10,
                           min_redzone_opps = 5)

# Available columns (verified):
# season, player_id, player_name, position_group, team, games_played,
# rz20_targets, rz20_rushes, rz20_tds, rz20_td_rate, rz20_epa,
# rz10_targets, rz10_rushes, rz10_tds, rz10_td_rate,
# rz5_targets, rz5_rushes, rz5_tds, rz5_td_rate

cat("Top Red Zone Receivers (Inside-20, min 5 opportunities):\n\n")
print(
  rz %>%
    filter(position_group == "receiver") %>%
    arrange(desc(rz20_targets)) %>%
    select(player_name, team,
           rz20_targets, rz10_targets, rz5_targets,
           rz20_tds, rz20_td_rate) %>%
    mutate(rz20_td_rate = paste0(round(rz20_td_rate * 100, 1), "%")) %>%
    head(10)
)

cat("\nTop Red Zone Rushers (Inside-20, min 5 opportunities):\n\n")
print(
  rz %>%
    filter(position_group == "rusher") %>%
    arrange(desc(rz20_rushes)) %>%
    select(player_name, team,
           rz20_rushes, rz10_rushes, rz5_rushes,
           rz20_tds, rz20_td_rate) %>%
    mutate(rz20_td_rate = paste0(round(rz20_td_rate * 100, 1), "%")) %>%
    head(10)
)

cat(glue("\nKey Insight: Compare rz10_targets vs rz20_targets -- a player with\n",
         "high rz20 but low rz10 is getting perimeter red zone targets (fades),\n",
         "not the high-value goal-line looks.\n"))


# ==============================================================================
# EXAMPLE 5: RECEIVING DEPTH PROFILE -- SLOT VS DEEP THREATS
#
# NFL Context: Slot-heavy receivers (short_pct >= 55%) are more stable
# week-to-week because short routes succeed regardless of coverage type.
# Deep threats (deep_pct >= 25%) have higher boom/bust variance -- they're
# matchup-dependent and weather-dependent.
# ==============================================================================
cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 5: Receiving Depth Profile -- Role Characterization\n")
cat(strrep("=", 60), "\n\n")

depth <- get_receiving_depth_profile(pbp, season = 2025,
                                      week_min = 1, week_max = 10,
                                      min_targets = 25)

# Available columns (verified):
# season, player_id, player_name, team, total_targets, avg_air_yards,
# short_targets, medium_targets, deep_targets,
# short_pct, medium_pct, deep_pct,
# short_catch_rate, medium_catch_rate, deep_catch_rate,
# short_epa_per_target, medium_epa_per_target, deep_epa_per_target,
# depth_profile

cat("Profile Distribution:\n")
print(depth %>% count(depth_profile, name = "n_receivers"))

cat("\nTop Slot-Heavy Receivers (short_pct >= 55%):\n\n")
print(
  depth %>%
    filter(depth_profile == "slot_heavy") %>%
    arrange(desc(total_targets)) %>%
    select(player_name, team, total_targets, avg_air_yards,
           short_pct, medium_pct, deep_pct, depth_profile) %>%
    mutate(across(c(short_pct, medium_pct, deep_pct),
                  ~ paste0(round(. * 100, 0), "%")),
           avg_air_yards = round(avg_air_yards, 1)) %>%
    head(8)
)

cat("\nTop Deep Threats (deep_pct >= 25%):\n\n")
print(
  depth %>%
    filter(depth_profile == "deep_threat") %>%
    arrange(desc(deep_targets)) %>%
    select(player_name, team, total_targets, avg_air_yards,
           deep_targets, deep_pct, deep_catch_rate, deep_epa_per_target) %>%
    mutate(
      deep_pct = paste0(round(deep_pct * 100, 0), "%"),
      deep_catch_rate = paste0(round(deep_catch_rate * 100, 1), "%"),
      avg_air_yards = round(avg_air_yards, 1),
      deep_epa_per_target = round(deep_epa_per_target, 3)
    ) %>%
    head(8)
)


# ==============================================================================
# EXAMPLE 6: RED ZONE OPPORTUNITY VOLATILITY
# Week-to-week consistency of red zone usage
#
# NFL Context: Red zone opportunities are highly volatile week-to-week
# (small sample, play-calling dependent). A player with 15 red zone targets
# over 10 weeks might have gotten 8 in two games and 1 in six others.
# Consistency of usage matters for weekly fantasy decisions.
# ==============================================================================
cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 6: Red Zone Opportunity Volatility\n")
cat(strrep("=", 60), "\n\n")

rz_weekly <- usage %>%
  filter(position_group == "receiver",
         redzone_targets > 0 | redzone_rushes > 0) %>%
  group_by(player_id, player_name, team) %>%
  summarise(
    games_played        = n_distinct(game_id),
    total_rz_opps       = sum(redzone_opportunities, na.rm = TRUE),
    avg_rz_opps         = mean(redzone_opportunities, na.rm = TRUE),
    sd_rz_opps          = sd(redzone_opportunities, na.rm = TRUE),
    games_with_rz       = sum(redzone_opportunities > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(games_played >= 5, total_rz_opps >= 5) %>%
  mutate(
    rz_cv              = ifelse(avg_rz_opps > 0, sd_rz_opps / avg_rz_opps, NA_real_),
    rz_game_rate       = games_with_rz / games_played
  ) %>%
  arrange(desc(avg_rz_opps))

cat("Red Zone Consistency Leaders (min 5 games, min 5 total RZ opps):\n")
cat("Lower CV = more consistent. Higher game_rate = RZ target in more games.\n\n")
print(
  rz_weekly %>%
    select(player_name, team, games_played, total_rz_opps,
           avg_rz_opps, rz_cv, rz_game_rate) %>%
    mutate(
      avg_rz_opps  = round(avg_rz_opps, 2),
      rz_cv        = round(rz_cv, 2),
      rz_game_rate = paste0(round(rz_game_rate * 100, 0), "%")
    ) %>%
    head(12)
)


# ==============================================================================
# EXAMPLE 7: COMBINED USAGE PROFILE -- FANTASY PRIORITY SCORE
# Experimental metric combining target share, air yards share, red zone share
#
# NFL Context: This is an EXPERIMENTAL ranking metric, not a validated
# predictor. It combines three opportunity dimensions into a single score
# for quick sorting. Requires validation against actual fantasy outcomes
# before using in production models (see Week 4 methodology).
# ==============================================================================
cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 7: Combined Usage Profile (EXPERIMENTAL)\n")
cat(strrep("=", 60), "\n\n")

# Combined opportunity score for WR/TE fantasy priority ranking
# Hypothesis: Linear combination of three opportunity metrics captures
# most of the variance in WR fantasy production
# Weights based on Week 4 finding: volume > efficiency
# Requires: Minimum 5 games for stability

opportunity_score <- usage %>%
  filter(position_group == "receiver") %>%
  group_by(player_id, player_name, team) %>%
  summarise(
    games              = n_distinct(game_id),
    avg_target_share   = mean(target_share, na.rm = TRUE),
    avg_air_yards_share = mean(air_yards_share, na.rm = TRUE),
    avg_redzone_share  = mean(redzone_share, na.rm = TRUE),
    total_targets      = sum(targets, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(games >= 5) %>%
  mutate(
    # Experimental: weighted opportunity score
    # Weights are informed by Week 4 correlations but require formal validation
    # target_share has ~0.60 correlation with late-season production (Week 4)
    # air_yards_share captures quality of opportunity (not just volume)
    # redzone_share is high variance but highest per-play value
    opportunity_score = (avg_target_share    * 0.50 +
                          avg_air_yards_share * 0.30 +
                          avg_redzone_share   * 0.20) * 100
  ) %>%
  arrange(desc(opportunity_score))

cat("EXPERIMENTAL: Receiver Opportunity Score (Weeks 1-10, min 5 games)\n")
cat("Formula: 50% target share + 30% air yards share + 20% RZ share\n")
cat("VALIDATION REQUIRED before use in prediction models.\n\n")

print(
  opportunity_score %>%
    select(player_name, team, games, total_targets,
           avg_target_share, avg_air_yards_share, avg_redzone_share,
           opportunity_score) %>%
    mutate(
      avg_target_share    = paste0(round(avg_target_share    * 100, 1), "%"),
      avg_air_yards_share = paste0(round(avg_air_yards_share * 100, 1), "%"),
      avg_redzone_share   = paste0(round(avg_redzone_share   * 100, 1), "%"),
      opportunity_score   = round(opportunity_score, 2)
    ) %>%
    head(15)
)

cat(glue("\nKey Insight: This score is an EXPERIMENTAL hypothesis, not a validated\n",
         "predictor. The Week 4 study showed target share (weight 0.50) has the\n",
         "strongest correlation with late-season production. Weights should be\n",
         "validated in the ML phase (Weeks 9-12) against actual outcomes.\n\n"))

cat(glue("\nBest Practices Summary:\n",
         "1. All usage metrics are RATES (share of team totals), not raw counts\n",
         "2. Multi-team players grouped by team-week -- trades handled automatically\n",
         "3. QB kneels, spikes, and scrambles excluded from all denominators\n",
         "4. Division guards protect all share calculations from zero-denominator error\n",
         "5. Rolling trends use lag() to prevent feature leakage\n",
         "6. Snap count data is NOT available in nflfastR -- documented limitation\n",
         "7. Red zone opportunity is volatile week-to-week -- use season totals for stability\n"))
