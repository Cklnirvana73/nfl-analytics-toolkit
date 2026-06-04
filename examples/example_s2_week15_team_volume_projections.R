# ==============================================================================
# Example Usage: R/30 Team Volume Projections
# File: examples/example_30_team_volume_projections.R
#
# Demonstrates the public API of R/30 and inspects the output structure.
# Each example is independent and runnable from a fresh R session after
# sourcing R/30.
# ==============================================================================

library(dplyr)
library(here)

# Source the module
source(here::here("R", "30_team_volume_projections.R"))

# ------------------------------------------------------------------------------
# EXAMPLE 1: Run the projection pipeline end to end
# ------------------------------------------------------------------------------
# Loads 3 historical seasons, blends with 2026 coaching changes (if CSV
# exists), applies QB quality adjustment, returns 32-row tibble.

team_vols <- project_team_volumes()

message("\n--- Output structure ---")
str(team_vols, max.level = 1)

message("\n--- First 5 teams alphabetical ---")
team_vols %>%
  dplyr::select(team, projected_pass_pg, projected_rush_pg,
                 projected_plays_pg, qb_quality_score, coach_change_flag) %>%
  head(5) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 2: Top 10 highest projected pass volume teams
# ------------------------------------------------------------------------------
# These are the teams whose receivers will see the most opportunity.
# Useful for identifying WR/TE-friendly offenses in 2026.

message("\n--- Top 10 projected pass volume ---")
team_vols %>%
  dplyr::arrange(dplyr::desc(projected_pass_pg)) %>%
  dplyr::select(team, projected_pass_pg, historical_pass_pg, blended_pass_pg,
                 qb_quality_score, coach_change_flag) %>%
  head(10) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 3: Top 10 highest projected rush volume teams
# ------------------------------------------------------------------------------
# Run-heavy offenses where RBs project to see the most carries.

message("\n--- Top 10 projected rush volume ---")
team_vols %>%
  dplyr::arrange(dplyr::desc(projected_rush_pg)) %>%
  dplyr::select(team, projected_rush_pg, historical_rush_pg,
                 blended_rush_pg, qb_quality_score, coach_change_flag) %>%
  head(10) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 4: Teams with coaching changes -- before vs after blend
# ------------------------------------------------------------------------------
# Shows how the 70/30 coach blend shifts each affected team's projected
# pass volume away from its own history and toward the new HC's prior
# team pattern.

message("\n--- Coaching change impact on pass volume ---")
team_vols %>%
  dplyr::filter(coach_change_flag) %>%
  dplyr::mutate(
    delta_pass_pg = blended_pass_pg - historical_pass_pg
  ) %>%
  dplyr::select(team, prior_hc_team, historical_pass_pg, coach_pass_pg,
                 blended_pass_pg, delta_pass_pg) %>%
  dplyr::arrange(dplyr::desc(abs(delta_pass_pg))) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 5: QB quality leaders and how they affect their team
# ------------------------------------------------------------------------------
# Teams whose 2025 starting QB scored highest on the CPOE + EPA z-score
# composite. Higher quality bumps both pass volume and pass efficiency.

message("\n--- Top 10 QB quality scores ---")
team_vols %>%
  dplyr::arrange(dplyr::desc(qb_quality_score)) %>%
  dplyr::select(team, qb_quality_score, blended_pass_pg, projected_pass_pg,
                 historical_pass_yds_pg, projected_pass_yds_pg) %>%
  head(10) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 6: Bottom 10 QB quality and the volume penalty they incur
# ------------------------------------------------------------------------------
# Teams whose starter scored below league average on CPOE + EPA. These
# teams get a small pass volume haircut and a small efficiency haircut.

message("\n--- Bottom 10 QB quality scores ---")
team_vols %>%
  dplyr::arrange(qb_quality_score) %>%
  dplyr::select(team, qb_quality_score, blended_pass_pg, projected_pass_pg,
                 historical_pass_yds_pg, projected_pass_yds_pg) %>%
  head(10) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 7: Direct sanity check on a single team
# ------------------------------------------------------------------------------
# Pulls every column for one team so you can verify the blending math
# manually. Useful for debugging unexpected projections.

message("\n--- MIN full projection breakdown ---")
team_vols %>%
  dplyr::filter(team == "MIN") %>%
  tidyr::pivot_longer(
    cols = -team,
    names_to = "metric",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>%
  print(n = Inf)

# ------------------------------------------------------------------------------
# EXAMPLE 8: League distribution summary
# ------------------------------------------------------------------------------
# Quick statistical summary of the 32-team projection distribution.

message("\n--- League distribution summary ---")
team_vols %>%
  dplyr::summarise(
    n_teams = dplyr::n(),
    n_coach_changes = sum(coach_change_flag, na.rm = TRUE),
    pass_pg_mean = mean(projected_pass_pg, na.rm = TRUE),
    pass_pg_sd   = stats::sd(projected_pass_pg, na.rm = TRUE),
    pass_pg_min  = min(projected_pass_pg, na.rm = TRUE),
    pass_pg_max  = max(projected_pass_pg, na.rm = TRUE),
    rush_pg_mean = mean(projected_rush_pg, na.rm = TRUE),
    rush_pg_sd   = stats::sd(projected_rush_pg, na.rm = TRUE),
    qb_q_mean    = mean(qb_quality_score, na.rm = TRUE),
    qb_q_sd      = stats::sd(qb_quality_score, na.rm = TRUE)
  ) %>%
  print()

message("\nDone.")
