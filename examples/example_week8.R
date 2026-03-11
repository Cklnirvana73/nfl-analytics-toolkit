# ==============================================================================
# WEEK 8 EXAMPLES: OPPONENT-ADJUSTED METRICS & MATCHUP FEATURES
# ==============================================================================
#
# 5 self-contained examples demonstrating Week 8 functions.
# Run interactively after sourcing Week 1 (data loading) and Week 8 functions.
#
# Prerequisites:
#   source(here("R", "01_data_loading.R"))
#   source(here("R", "10_opponent_features.R"))
#
# Update season parameter when running in subsequent seasons.
# ==============================================================================

library(dplyr)
library(tidyr)
library(glue)
library(here)

source(here("R", "01_data_loading.R"))
source(here("R", "10_opponent_features.R"))

SEASON <- 2025L

cat(strrep("=", 70), "\n")
cat(glue("WEEK 8 EXAMPLES -- {SEASON} NFL Season\n"))
cat(strrep("=", 70), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# EXAMPLE 1: OPPONENT ADJUSTMENTS
# Who looks better or worse after accounting for schedule difficulty?
# ─────────────────────────────────────────────────────────────────────────────

cat("EXAMPLE 1: Opponent-Adjusted EPA\n")
cat(strrep("-", 50), "\n")

pbp <- load_and_validate_pbp(seasons = SEASON)

opp_adj <- calculate_opponent_adjustments(
  pbp,
  season    = SEASON,
  min_plays = 50
)

cat("Output columns:\n")
print(names(opp_adj))
cat("\n")

# QBs with the biggest upward adjustment (faced hardest schedules)
# opp_adjustment is NEGATIVE for tough schedules, so opp_adjusted > raw
cat("QBs rewarded most by opponent adjustment (toughest schedules):\n")
opp_adj %>%
  filter(position_group == "passer", total_plays >= 200) %>%
  arrange(opp_adjustment) %>%   # most negative = toughest schedule
  select(player_name, team, raw_epa_per_play, opp_adjusted_epa,
         opp_adjustment, schedule_difficulty_rank, games_played) %>%
  head(8) %>%
  mutate(across(where(is.double), ~ round(.x, 4))) %>%
  print()

cat("\n")
cat("QBs penalized most (easiest schedules):\n")
opp_adj %>%
  filter(position_group == "passer", total_plays >= 200) %>%
  arrange(desc(opp_adjustment)) %>%  # most positive = easiest schedule
  select(player_name, team, raw_epa_per_play, opp_adjusted_epa,
         opp_adjustment, schedule_difficulty_rank) %>%
  head(8) %>%
  mutate(across(where(is.double), ~ round(.x, 4))) %>%
  print()

# KEY INSIGHT
cat("\nKEY INSIGHT (Example 1):\n")
cat("  NFL schedule difficulty can differ by 0.05+ EPA/play between extremes.\n")
cat("  A QB with identical raw EPA but harder schedule may be significantly better.\n")
cat("  opp_adjustment < 0 = player rewarded (faced tougher defense than avg).\n")
cat("  Always report both raw_epa_per_play and opp_adjusted_epa side-by-side.\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# EXAMPLE 2: DEFENSIVE STYLE CLASSIFICATION
# Which defenses are pass funnels or run funnels?
# ─────────────────────────────────────────────────────────────────────────────

cat("EXAMPLE 2: Defensive Style Classification\n")
cat(strrep("-", 50), "\n")

def_styles <- classify_defensive_style(
  pbp,
  season        = SEASON,
  min_def_plays = 100
)

cat("Output columns:\n")
print(names(def_styles))
cat("\n")

cat("Defensive style breakdown:\n")
def_styles %>%
  count(defensive_style, overall_tier) %>%
  arrange(defensive_style, overall_tier) %>%
  print()

cat("\nStrongest pass-funnel defenses (toughest QB matchup, best for RBs):\n")
def_styles %>%
  filter(defensive_style == "pass_funnel") %>%
  arrange(desc(style_strength)) %>%
  select(team, pass_epa_allowed, rush_epa_allowed,
         pass_epa_vs_league, rush_epa_vs_league,
         style_strength, overall_tier) %>%
  head(6) %>%
  mutate(across(where(is.double), ~ round(.x, 4))) %>%
  print()

cat("\nStrongest run-funnel defenses (toughest RB matchup, best for QBs/WRs):\n")
def_styles %>%
  filter(defensive_style == "run_funnel") %>%
  arrange(desc(style_strength)) %>%
  select(team, pass_epa_allowed, rush_epa_allowed,
         pass_epa_vs_league, rush_epa_vs_league,
         style_strength, overall_tier) %>%
  head(6) %>%
  mutate(across(where(is.double), ~ round(.x, 4))) %>%
  print()

cat("\nElite balanced defenses (tough for everyone):\n")
def_styles %>%
  filter(overall_tier == "elite", defensive_style == "balanced") %>%
  arrange(overall_epa_allowed) %>%
  select(team, overall_epa_allowed, defensive_style, style_strength) %>%
  mutate(across(where(is.double), ~ round(.x, 4))) %>%
  print()

cat("\nKEY INSIGHT (Example 2):\n")
cat("  EPA allowed (not yards) is the correct metric for defensive style.\n")
cat("  Yards conflate performance with game script; EPA accounts for situation.\n")
cat("  Pass-funnel defenses are good RB matchups in fantasy -- RBs see more\n")
cat("  opportunity in neutral/leading scripts against these defenses.\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# EXAMPLE 3: MATCHUP HISTORY BY DEFENSIVE ARCHETYPE
# Which players are most and least matchup-sensitive?
# ─────────────────────────────────────────────────────────────────────────────

cat("EXAMPLE 3: Matchup History by Defensive Archetype\n")
cat(strrep("-", 50), "\n")

arch_history <- calculate_matchup_history(
  pbp, def_styles,
  season                  = SEASON,
  min_plays_per_archetype = 15
)

cat("Output columns:\n")
print(names(arch_history))
cat("\n")

cat("WRs most sensitive to defensive archetype (largest archetype_epa_range):\n")
arch_history %>%
  filter(position_group == "receiver", !is.na(archetype_epa_range)) %>%
  arrange(desc(archetype_epa_range)) %>%
  select(player_name, team,
         vs_pass_funnel_epa, vs_run_funnel_epa, vs_balanced_epa,
         archetype_epa_range, best_archetype, worst_archetype) %>%
  head(8) %>%
  mutate(across(where(is.double), ~ round(.x, 4))) %>%
  print()

cat("\nRBs by performance vs pass-funnel defenses (good RB matchup):\n")
arch_history %>%
  filter(position_group == "rusher",
         !is.na(vs_pass_funnel_epa),
         vs_pass_funnel_plays >= 15) %>%
  arrange(desc(vs_pass_funnel_epa)) %>%
  select(player_name, team, vs_pass_funnel_epa, vs_pass_funnel_plays,
         overall_epa_per_play) %>%
  head(8) %>%
  mutate(across(where(is.double), ~ round(.x, 4))) %>%
  print()

cat("\nKEY INSIGHT (Example 3):\n")
cat("  Individual matchup history is too sparse (1-2 games per opponent per\n")
cat("  season). Archetype matching provides 15-50+ plays per player-archetype.\n")
cat("  NA means < min_plays_per_archetype -- do not substitute 0.\n")
cat("  Large archetype_epa_range = matchup-dependent player (roster management\n")
cat("  opportunity in fantasy).\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# EXAMPLE 4: COMPILE MASTER FEATURE MATRIX
# One row per player-week for Week 9 ML models
# ─────────────────────────────────────────────────────────────────────────────

cat("EXAMPLE 4: Master Feature Matrix\n")
cat(strrep("-", 50), "\n")

features <- compile_feature_matrix(
  pbp_data          = pbp,
  opp_adjusted      = opp_adj,
  def_styles        = def_styles,
  season            = SEASON,
  rolling_window    = 3L,
  min_plays_per_week = 3L
)

cat("Feature matrix dimensions:\n")
cat(glue("  Rows: {nrow(features)}\n"))
cat(glue("  Columns: {ncol(features)}\n"))
cat(glue("  Unique players: {n_distinct(features$player_id)}\n"))
cat(glue("  Weeks covered: {min(features$week, na.rm = TRUE)} - ",
         "{max(features$week, na.rm = TRUE)}\n\n"))

cat("Column names:\n")
print(names(features))
cat("\n")

# Preview for a WR (receiving position group)
example_receiver <- features %>%
  filter(position_group == "receiver") %>%
  group_by(player_id) %>%
  filter(n() >= 8) %>%        # at least 8 qualifying weeks
  ungroup() %>%
  slice_max(order_by = epa_this_week, n = 1) %>%
  pull(player_name) %>%
  first()

if (!is.na(example_receiver)) {
  cat(glue("Preview: {example_receiver} week-by-week features\n"))
  features %>%
    filter(player_name == example_receiver, position_group == "receiver") %>%
    select(week, epa_this_week, epa_roll3, epa_season_to_date,
           opponent, opponent_tier, opponent_style,
           opp_adjusted_epa_season) %>%
    mutate(across(where(is.double), ~ round(.x, 4))) %>%
    print(n = 18)
}

cat("\nKEY INSIGHT (Example 4):\n")
cat("  season-level features (neutral_epa_season, opp_adjusted_epa_season)\n")
cat("  use the FULL season calculation -- appropriate for within-season use.\n")
cat("  For cross-season prediction, generate these from prior-season data.\n")
cat("  Rolling features use lag() before rollmean() -- no leakage.\n")
cat("  epa_roll3 and epa_season_to_date are NA for a player's first week.\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# EXAMPLE 5: INTEGRATED ANALYSIS
# Combining all layers for actionable fantasy insight
# ─────────────────────────────────────────────────────────────────────────────

cat("EXAMPLE 5: Integrated Analysis -- Fantasy Context\n")
cat(strrep("-", 50), "\n")

# Identify RBs who:
#   1. Have above-average opp-adjusted EPA (good at the position)
#   2. Have upcoming game vs a pass-funnel defense (favorable matchup)
#   3. Are not garbage-time dependent (neutral_share > 0.50)

cat("RB profiles combining opponent adjustment + matchup + game script:\n")
rb_opp <- opp_adj %>%
  filter(position_group == "rusher", total_plays >= 40)

rb_script <- tryCatch({
  source(here("R", "09_gamescript_features.R"), local = TRUE)
  get_game_script_splits(pbp, season = SEASON) %>%
    filter(position_group == "rusher") %>%
    select(player_id, neutral_share, leading_share, trailing_share,
           neutral_epa_per_play, script_epa_delta)
}, error = function(e) {
  message(glue("Could not load Week 7 script features: {conditionMessage(e)}"))
  NULL
})

rb_arch <- arch_history %>%
  filter(position_group == "rusher") %>%
  select(player_id, vs_pass_funnel_epa, vs_pass_funnel_plays,
         archetype_epa_range, best_archetype)

integrated <- rb_opp %>%
  left_join(rb_arch, by = "player_id")

if (!is.null(rb_script)) {
  integrated <- integrated %>%
    left_join(rb_script, by = "player_id")
}

integrated <- integrated %>%
  mutate(
    above_avg_efficiency = opp_adjusted_epa > 0,
    pass_funnel_threat   = !is.na(vs_pass_funnel_epa) & vs_pass_funnel_epa > 0
  )

cat("RBs with positive opp-adjusted EPA AND positive vs pass-funnel EPA:\n")
integrated %>%
  filter(above_avg_efficiency, pass_funnel_threat) %>%
  arrange(desc(opp_adjusted_epa)) %>%
  select(player_name, team,
         raw_epa_per_play, opp_adjusted_epa,
         vs_pass_funnel_epa,
         any_of(c("neutral_share", "neutral_epa_per_play"))) %>%
  mutate(across(where(is.double), ~ round(.x, 4))) %>%
  head(10) %>%
  print()

cat("\nKEY INSIGHT (Example 5):\n")
cat("  Opponent adjustment + defensive style + game script together form the\n")
cat("  cleanest picture of player value. A RB who:\n")
cat("    - Has positive opp-adjusted EPA (good against tough defenses)\n")
cat("    - Thrives vs pass-funnel defenses (scheme-independent)\n")
cat("    - Has high neutral_share (not garbage-time dependent)\n")
cat("  ...is the most projectable fantasy asset.\n")
cat("  These metrics feed directly into the Week 9 XGBoost feature matrix.\n\n")

cat(strrep("=", 70), "\n")
cat("Week 8 examples complete.\n")
cat(strrep("=", 70), "\n")
