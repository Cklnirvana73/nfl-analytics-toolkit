# ==============================================================================
# 2026 Preseason Projections
# Uses 2025 as the prior season
# TEs routed through receiver model, real position restored after
# ==============================================================================

library(dplyr)
library(here)
library(glue)
library(nflreadr)

source(here::here("R", "14_projection_system.R"))

# ------------------------------------------------------------------------------
# STEP 1: Build the prior stats from the 2025 season
# ------------------------------------------------------------------------------

prior_stats_path <- here::here("output", "player_stats_2025.rds")

if (file.exists(prior_stats_path)) {
  prior_stats <- readRDS(prior_stats_path)
  cat("Loaded prior season stats from cache.\n")
} else {
  cat("Building 2025 prior stats from nflreadr...\n")
  weekly_2025 <- nflreadr::load_player_stats(seasons = 2025) %>%
    filter(
      position %in% c("QB", "RB", "WR", "TE"),
      season_type == "REG"
    )
  
  prior_stats <- weekly_2025 %>%
    group_by(player_id, player_display_name, position) %>%
    summarise(
      games_played          = n_distinct(week),
      total_ppr             = sum(fantasy_points_ppr, na.rm = TRUE),
      avg_ppr_per_game      = total_ppr / games_played,
      avg_pass_epa_per_game = sum(passing_epa, na.rm = TRUE) / games_played,
      avg_rush_epa_per_game = sum(rushing_epa, na.rm = TRUE) / games_played,
      .groups = "drop"
    ) %>%
    rename(player_name = player_display_name) %>%
    select(player_id, player_name, position, games_played,
           avg_ppr_per_game, avg_pass_epa_per_game, avg_rush_epa_per_game)
  
  saveRDS(prior_stats, prior_stats_path)
  cat(glue("Saved {nrow(prior_stats)} players to {prior_stats_path}\n"))
}

cat(glue("\nPrior stats: {nrow(prior_stats)} players\n"))
cat("Position breakdown:\n")
print(table(prior_stats$position))

# ------------------------------------------------------------------------------
# STEP 2: Generate 2026 preseason projections (TEs routed as receivers)
# ------------------------------------------------------------------------------

# Save the real position before remapping
real_positions <- prior_stats %>%
  select(player_id, real_position = position)

# Remap TE -> WR so they flow through the receiver model
prior_stats_mapped <- prior_stats %>%
  mutate(position = if_else(position == "TE", "WR", position))

projections_2026 <- generate_preseason_projections(
  prior_stats_mapped,
  prior_season    = 2025,
  min_prior_games = 4L
)

# Restore the real position via join
projections_2026 <- projections_2026 %>%
  left_join(real_positions, by = "player_id") %>%
  mutate(
    model_position = position,
    position       = real_position
  ) %>%
  select(-real_position)

cat(glue("\n2026 Preseason projections: {nrow(projections_2026)} qualified players\n"))
cat("Position breakdown (real positions):\n")
print(table(projections_2026$position))

# ------------------------------------------------------------------------------
# STEP 3: View results by position
# ------------------------------------------------------------------------------

cat("\n=== TOP 15 QBs ===\n")
projections_2026 %>%
  filter(position == "QB") %>%
  arrange(desc(projected_ppr_per_game)) %>%
  select(player_name, projected_ppr_per_game, projected_ppr_lower_95,
         projected_ppr_upper_95, prior_games_played, regression_alpha,
         prior_volume_rate, prior_efficiency_rate) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  head(15) %>%
  print()

cat("\n=== TOP 20 RBs ===\n")
projections_2026 %>%
  filter(position == "RB") %>%
  arrange(desc(projected_ppr_per_game)) %>%
  select(player_name, projected_ppr_per_game, projected_ppr_lower_95,
         projected_ppr_upper_95, prior_games_played, regression_alpha,
         prior_volume_rate, prior_efficiency_rate) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  head(20) %>%
  print()

cat("\n=== TOP 20 WRs ===\n")
projections_2026 %>%
  filter(position == "WR") %>%
  arrange(desc(projected_ppr_per_game)) %>%
  select(player_name, projected_ppr_per_game, projected_ppr_lower_95,
         projected_ppr_upper_95, prior_games_played, regression_alpha,
         prior_volume_rate, prior_efficiency_rate) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  head(20) %>%
  print()

cat("\n=== TOP 10 TEs ===\n")
projections_2026 %>%
  filter(position == "TE") %>%
  arrange(desc(projected_ppr_per_game)) %>%
  select(player_name, projected_ppr_per_game, projected_ppr_lower_95,
         projected_ppr_upper_95, prior_games_played, regression_alpha,
         prior_volume_rate, prior_efficiency_rate) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  head(10) %>%
  print()

# ------------------------------------------------------------------------------
# STEP 4: Save the 2026 projections
# ------------------------------------------------------------------------------

saveRDS(projections_2026, here::here("output", "preseason_projections_2026.rds"))
cat("\nSaved to output/preseason_projections_2026.rds\n")