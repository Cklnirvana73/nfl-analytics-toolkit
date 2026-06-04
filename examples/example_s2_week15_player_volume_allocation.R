# ==============================================================================
# Example Usage: R/31 Player Volume Allocation
# File: examples/example_s2_week15_player_volume_allocation.R
#
# Demonstrates the public API of R/31 and inspects how team volumes are
# distributed among players. The MIN example (Example 6) is the Felton
# diagnosis -- showing how the WR target pool is split.
# ==============================================================================

library(dplyr)
library(tidyr)
library(here)

# Source the module
source(here::here("R", "31_player_volume_allocation.R"))

# ------------------------------------------------------------------------------
# EXAMPLE 1: Run the allocation pipeline end to end
# ------------------------------------------------------------------------------
# Loads R/30 team volumes, depth charts, draft picks, R/28 dynasty scores.
# Allocates target and rush shares per player, enforces soft constraints,
# multiplies by team volumes to get expected targets/carries per game.

alloc <- allocate_player_volumes()

message("\n--- Output structure ---")
str(alloc, max.level = 1)

message("\n--- First 10 rows (alphabetical by team) ---")
alloc %>%
  dplyr::select(team, player_name, position, depth_position,
                 target_share, expected_targets_pg,
                 rush_share, expected_carries_pg) %>%
  head(10) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 2: Top 20 highest projected target volumes league-wide
# ------------------------------------------------------------------------------
# Who the model expects to lead the league in target volume per game.

message("\n--- Top 20 expected targets per game ---")
alloc %>%
  dplyr::arrange(dplyr::desc(expected_targets_pg)) %>%
  dplyr::select(player_name, team, position, depth_position,
                 target_share, expected_targets_pg, talent_multiplier) %>%
  head(20) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 3: Top 20 highest projected carry volumes league-wide
# ------------------------------------------------------------------------------
# Who the model expects to lead the league in carries per game.

message("\n--- Top 20 expected carries per game ---")
alloc %>%
  dplyr::arrange(dplyr::desc(expected_carries_pg)) %>%
  dplyr::select(player_name, team, position, depth_position,
                 rush_share, expected_carries_pg, talent_multiplier) %>%
  head(20) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 4: Rookies receiving meaningful projected volume
# ------------------------------------------------------------------------------
# Filter to drafted rookies with non-zero target or carry projection. Sort
# by expected fantasy contribution.

message("\n--- Rookies with notable projected volume ---")
alloc %>%
  dplyr::filter(is_rookie,
                 expected_targets_pg > 2 | expected_carries_pg > 2) %>%
  dplyr::mutate(
    combined_touches_pg = expected_targets_pg + expected_carries_pg
  ) %>%
  dplyr::arrange(dplyr::desc(combined_touches_pg)) %>%
  dplyr::select(player_name, team, position, depth_position, draft_round,
                 rookie_capital_mult, expected_targets_pg,
                 expected_carries_pg) %>%
  head(20) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 5: Soft constraint diagnostics
# ------------------------------------------------------------------------------
# Show per-team target and rush share sums BEFORE constraint enforcement.
# Teams outside [0.95, 1.05] were rescaled; teams inside were not.

message("\n--- Per-team share sums (pre-constraint) ---")
alloc %>%
  dplyr::group_by(team) %>%
  dplyr::summarise(
    target_sum_pre   = sum(target_share_adjusted, na.rm = TRUE),
    target_sum_post  = sum(target_share, na.rm = TRUE),
    rush_sum_pre     = sum(rush_share_adjusted, na.rm = TRUE),
    rush_sum_post    = sum(rush_share, na.rm = TRUE),
    rescaled_targets = abs(target_sum_pre - 1.0) > 0.05,
    rescaled_rushes  = abs(rush_sum_pre - 1.0) > 0.05,
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(abs(target_sum_pre - 1.0))) %>%
  head(10) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 6: MIN WR pool breakdown -- the Tai Felton diagnostic
# ------------------------------------------------------------------------------
# The original motivation for R/30 + R/31. Shows how MIN's WR target pool
# is split among Jefferson, Addison, Felton, and other receivers.

message("\n--- MIN WR target allocation (the Felton diagnostic) ---")
alloc %>%
  dplyr::filter(team == "MIN", position == "WR") %>%
  dplyr::arrange(depth_rank) %>%
  dplyr::select(player_name, depth_position, is_rookie, talent_multiplier,
                 target_share_base, target_share_adjusted, target_share,
                 expected_targets_pg) %>%
  print()

message("\n--- MIN full skill position allocation ---")
alloc %>%
  dplyr::filter(team == "MIN", position %in% c("RB", "WR", "TE")) %>%
  dplyr::arrange(position, depth_rank) %>%
  dplyr::select(player_name, position, depth_position,
                 target_share, expected_targets_pg,
                 rush_share, expected_carries_pg) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 7: Per-position summary statistics
# ------------------------------------------------------------------------------
# Mean and max projected volumes per position. Useful for sanity check
# against your intuition: WR1s should average ~7-9 targets/game, RB1s
# should average ~14-18 carries/game.

message("\n--- Position-level summary statistics ---")
alloc %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(
    n_players          = dplyr::n(),
    mean_target_pg     = mean(expected_targets_pg, na.rm = TRUE),
    max_target_pg      = max(expected_targets_pg, na.rm = TRUE),
    mean_carry_pg      = mean(expected_carries_pg, na.rm = TRUE),
    max_carry_pg       = max(expected_carries_pg, na.rm = TRUE),
    .groups            = "drop"
  ) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 8: Talent multiplier distribution
# ------------------------------------------------------------------------------
# Sanity check the talent z-score system. Players at extreme z-scores
# (capped at the multiplier floor/ceiling) are worth inspecting -- they
# may be the model thinking very strongly about them.

message("\n--- Players hitting the talent multiplier ceiling (1.30) ---")
alloc %>%
  dplyr::filter(talent_multiplier >= 1.30 - 0.001) %>%
  dplyr::arrange(dplyr::desc(score_final)) %>%
  dplyr::select(player_name, team, position, score_final, talent_z,
                 talent_multiplier, expected_targets_pg) %>%
  head(15) %>%
  print()

message("\n--- Players hitting the talent multiplier floor (0.70) ---")
alloc %>%
  dplyr::filter(talent_multiplier <= 0.70 + 0.001) %>%
  dplyr::arrange(score_final) %>%
  dplyr::select(player_name, team, position, score_final, talent_z,
                 talent_multiplier, expected_targets_pg) %>%
  head(15) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 9: Single-team deep dive
# ------------------------------------------------------------------------------
# Pull every player for one team. Useful for verifying allocation logic
# manually. Change "KC" to any team code you want to inspect.

inspect_team <- "KC"

message(glue::glue("\n--- {inspect_team} complete offensive allocation ---"))
alloc %>%
  dplyr::filter(team == inspect_team) %>%
  dplyr::arrange(position, depth_rank) %>%
  dplyr::select(player_name, position, depth_position, is_rookie,
                 target_share, expected_targets_pg,
                 rush_share, expected_carries_pg) %>%
  print(n = Inf)

# ------------------------------------------------------------------------------
# EXAMPLE 10: League-wide distribution summary
# ------------------------------------------------------------------------------

message("\n--- League-wide allocation summary ---")
alloc %>%
  dplyr::summarise(
    n_players          = dplyr::n(),
    n_teams            = dplyr::n_distinct(team),
    n_rookies          = sum(is_rookie, na.rm = TRUE),
    n_with_r28_score   = sum(!is.na(score_final), na.rm = TRUE),
    mean_target_share  = mean(target_share, na.rm = TRUE),
    sd_target_share    = stats::sd(target_share, na.rm = TRUE),
    mean_rush_share    = mean(rush_share, na.rm = TRUE),
    sd_rush_share      = stats::sd(rush_share, na.rm = TRUE)
  ) %>%
  print()

message("\nDone.")
