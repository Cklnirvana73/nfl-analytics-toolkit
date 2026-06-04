# ==============================================================================
# Example Usage: R/32 Projection Reconciliation
# File: examples/example_s2_week15_projection_reconciliation.R
#
# Demonstrates the reconciliation layer. The headline result is in Example 5
# (biggest movers) and Example 6 (the Felton deep dive).
# ==============================================================================

library(dplyr)
library(tidyr)
library(here)

# Source the module
source(here::here("R", "32_projection_reconciliation.R"))

# ------------------------------------------------------------------------------
# EXAMPLE 1: Run reconciliation end to end
# ------------------------------------------------------------------------------
# Loads R/29 projections and R/31 allocations. Computes volume-implied PPG.
# Blends per player based on prior_source. Outputs corrected projections.

reconciled <- reconcile_projections()

message("\n--- Output structure ---")
str(reconciled, max.level = 1)

# ------------------------------------------------------------------------------
# EXAMPLE 2: Top 20 reconciled projections league-wide
# ------------------------------------------------------------------------------
# Sorted by the new r32_posterior_mu, showing how the order has shifted
# from R/29's posterior_mu.

message("\n--- Top 20 reconciled PPG ---")
reconciled %>%
  dplyr::arrange(dplyr::desc(r32_posterior_mu)) %>%
  dplyr::select(player_name, team, position, r29_posterior_mu,
                 r32_posterior_mu, r32_delta_from_r29, blend_weight_r31,
                 prior_source) %>%
  head(20) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 3: Top 20 by position -- WRs only
# ------------------------------------------------------------------------------
# WR rankings after team-constraint reconciliation. Compare against R/29's
# ranking to see who moved.

message("\n--- Top 20 WRs after reconciliation ---")
reconciled %>%
  dplyr::filter(position == "WR") %>%
  dplyr::arrange(dplyr::desc(r32_posterior_mu)) %>%
  dplyr::select(player_name, team, r29_posterior_mu, r32_posterior_mu,
                 r32_delta_from_r29, r31_expected_targets_pg,
                 volume_implied_ppg_v2, prior_source) %>%
  head(20) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 4: Top 20 RBs after reconciliation
# ------------------------------------------------------------------------------

message("\n--- Top 20 RBs after reconciliation ---")
reconciled %>%
  dplyr::filter(position == "RB") %>%
  dplyr::arrange(dplyr::desc(r32_posterior_mu)) %>%
  dplyr::select(player_name, team, r29_posterior_mu, r32_posterior_mu,
                 r32_delta_from_r29, r31_expected_carries_pg,
                 r31_expected_targets_pg, prior_source) %>%
  head(20) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 5: Biggest movers (positive and negative deltas)
# ------------------------------------------------------------------------------
# Which players had their projections shifted most by team constraints.
# Downward movers are typically WR3/WR4s the model previously over-projected
# because R/29 didn't know about WR1/WR2 ahead of them.

message("\n--- Top 20 downward movers ---")
reconciled %>%
  dplyr::filter(position %in% c("RB", "WR", "TE"),
                 !is.na(r32_delta_from_r29)) %>%
  dplyr::arrange(r32_delta_from_r29) %>%
  dplyr::select(player_name, team, position, r29_posterior_mu,
                 r32_posterior_mu, r32_delta_from_r29, blend_weight_r31,
                 prior_source) %>%
  head(20) %>%
  print()

message("\n--- Top 20 upward movers ---")
reconciled %>%
  dplyr::filter(position %in% c("RB", "WR", "TE"),
                 !is.na(r32_delta_from_r29)) %>%
  dplyr::arrange(dplyr::desc(r32_delta_from_r29)) %>%
  dplyr::select(player_name, team, position, r29_posterior_mu,
                 r32_posterior_mu, r32_delta_from_r29, blend_weight_r31,
                 prior_source) %>%
  head(20) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 6: Tai Felton deep dive -- the headline result
# ------------------------------------------------------------------------------
# Show the full chain for Felton: R/29 said one thing, R/31 said another,
# R/32 brought him back to reality.

message("\n--- Tai Felton full diagnostic ---")
reconciled %>%
  dplyr::filter(grepl("Felton", player_name, ignore.case = TRUE)) %>%
  tidyr::pivot_longer(
    cols = -player_name,
    names_to = "metric",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  print(n = Inf)

message("\n--- MIN WR room after reconciliation ---")
reconciled %>%
  dplyr::filter(team == "MIN", position == "WR") %>%
  dplyr::arrange(dplyr::desc(r32_posterior_mu)) %>%
  dplyr::select(player_name, r29_posterior_mu, r32_posterior_mu,
                 r32_delta_from_r29, r31_expected_targets_pg,
                 blend_weight_r31, prior_source) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 7: Effect by prior_source category
# ------------------------------------------------------------------------------
# Average shift per prior_source. Should show score_final_fallback players
# moving the most (they get the highest R/31 weight).

message("\n--- Mean shift by prior_source ---")
reconciled %>%
  dplyr::filter(position %in% c("RB", "WR", "TE"),
                 !is.na(r32_delta_from_r29)) %>%
  dplyr::group_by(prior_source) %>%
  dplyr::summarise(
    n_players              = dplyr::n(),
    mean_blend_weight      = mean(blend_weight_r31, na.rm = TRUE),
    mean_r29               = mean(r29_posterior_mu, na.rm = TRUE),
    mean_r32               = mean(r32_posterior_mu, na.rm = TRUE),
    mean_abs_delta         = mean(abs(r32_delta_from_r29), na.rm = TRUE),
    .groups                = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(mean_blend_weight)) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 8: QB pass-through verification
# ------------------------------------------------------------------------------
# QBs should have r32_posterior_mu equal to r29_posterior_mu exactly,
# blend_weight_r31 = 0, and volume_implied_ppg_v2 NA.

message("\n--- QB pass-through check (top 10 QBs) ---")
reconciled %>%
  dplyr::filter(position == "QB") %>%
  dplyr::arrange(dplyr::desc(r29_posterior_mu)) %>%
  dplyr::select(player_name, team, r29_posterior_mu, r32_posterior_mu,
                 r32_delta_from_r29, blend_weight_r31,
                 volume_implied_ppg_v2) %>%
  head(10) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 9: Per-position summary statistics
# ------------------------------------------------------------------------------

message("\n--- Per-position summary ---")
reconciled %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(
    n_players       = dplyr::n(),
    mean_r29        = mean(r29_posterior_mu, na.rm = TRUE),
    mean_r32        = mean(r32_posterior_mu, na.rm = TRUE),
    median_delta    = stats::median(r32_delta_from_r29, na.rm = TRUE),
    max_drop        = min(r32_delta_from_r29, na.rm = TRUE),
    max_gain        = max(r32_delta_from_r29, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 10: Single team deep dive
# ------------------------------------------------------------------------------
# Pull every reconciled projection for one team. Useful for verifying that
# the team-level constraints look sensible across the full offense.

inspect_team <- "MIN"

message(glue::glue("\n--- {inspect_team} full reconciled offense ---"))
reconciled %>%
  dplyr::filter(team == inspect_team,
                 position %in% c("QB", "RB", "WR", "TE")) %>%
  dplyr::arrange(position, dplyr::desc(r32_posterior_mu)) %>%
  dplyr::select(player_name, position, r29_posterior_mu, r32_posterior_mu,
                 r32_delta_from_r29, r31_expected_targets_pg,
                 r31_expected_carries_pg, blend_weight_r31) %>%
  print(n = Inf)

message("\nDone.")
