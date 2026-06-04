# ==============================================================================
# Example Usage: R/33 Cross-Position VORP Rankings
# File: examples/example_s2_week15_vorp_rankings.R
#
# Demonstrates the cross-position ranking layer. Each example builds a
# different league configuration and shows how the rankings shift. The
# headline result is in Example 5 (format comparison) and Example 6
# (Felton/Hunter sanity checks).
# ==============================================================================

library(dplyr)
library(tidyr)
library(here)

source(here::here("R", "33_vorp_rankings.R"))

# ------------------------------------------------------------------------------
# LOAD R/32 RECONCILED PROJECTIONS
# ------------------------------------------------------------------------------

reconciled <- readRDS(here::here("data", "season2_cache",
                                  "s2_week15_reconciled_projections.rds"))

message(glue::glue("\nLoaded {nrow(reconciled)} reconciled projections from R/32"))

# ------------------------------------------------------------------------------
# EXAMPLE 1: Build a standard 12-team PPR config and inspect it
# ------------------------------------------------------------------------------
# This is the bog-standard format most home leagues use.

config_standard <- build_league_config(
  league_name = "Standard 12-team PPR",
  num_teams   = 12L,
  starters    = list(QB = 1, RB = 2, WR = 2, TE = 1),
  flex        = 1L,
  superflex   = 0L,
  format      = "ppr"
)

print(config_standard)

# ------------------------------------------------------------------------------
# EXAMPLE 2: Compute rankings for this standard league
# ------------------------------------------------------------------------------
# Top 30 cross-position. This is what your draft board should look like.

rankings_std <- compute_vorp_rankings(reconciled, config_standard)

message("\n--- Top 30 overall for Standard 12-team PPR ---")
rankings_std %>%
  dplyr::select(overall_rank, player_name, team, position, position_rank,
                 r32_posterior_mu, replacement_ppg, vorp_base,
                 adjusted_vorp) %>%
  head(30) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 3: Position-by-position top 15 within standard PPR
# ------------------------------------------------------------------------------

for (pos in c("QB", "RB", "WR", "TE")) {
  message(glue::glue("\n--- Top 15 {pos} in standard PPR ---"))
  rankings_std %>%
    dplyr::filter(position == pos) %>%
    dplyr::arrange(position_rank) %>%
    dplyr::select(position_rank, overall_rank, player_name, team,
                   r32_posterior_mu, vorp_base, adjusted_vorp) %>%
    head(15) %>%
    print()
}

# ------------------------------------------------------------------------------
# EXAMPLE 4: Build a best ball config and compute rankings
# ------------------------------------------------------------------------------
# Best ball: no lineup decisions, platform auto-picks highest scorers.
# Ceiling matters far more, floor matters far less.

config_bestball <- build_league_config(
  league_name = "Best Ball 12-team",
  num_teams   = 12L,
  starters    = list(QB = 1, RB = 2, WR = 3, TE = 1),
  flex        = 1L,
  superflex   = 0L,
  format      = "best_ball"
)

print(config_bestball)

rankings_bb <- compute_vorp_rankings(reconciled, config_bestball)

message("\n--- Top 30 overall for Best Ball ---")
rankings_bb %>%
  dplyr::select(overall_rank, player_name, team, position, position_rank,
                 r32_posterior_mu, boom_probability, ceiling_modifier,
                 adjusted_vorp) %>%
  head(30) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 5: Build a superflex config and see how QB rankings change
# ------------------------------------------------------------------------------
# Superflex makes elite QBs the most valuable position because QB
# replacement level effectively drops to QB25 in a 12-team league.

config_sflex <- build_league_config(
  league_name = "Superflex 12-team PPR",
  num_teams   = 12L,
  starters    = list(QB = 1, RB = 2, WR = 2, TE = 1),
  flex        = 1L,
  superflex   = 1L,
  format      = "ppr"
)

print(config_sflex)

rankings_sf <- compute_vorp_rankings(reconciled, config_sflex)

message("\n--- Top 30 overall for Superflex (watch QBs rise) ---")
rankings_sf %>%
  dplyr::select(overall_rank, player_name, team, position, position_rank,
                 r32_posterior_mu, replacement_ppg, vorp_base,
                 adjusted_vorp) %>%
  head(30) %>%
  print()

message("\n--- QB replacement level: standard vs superflex ---")
qb_rep_std <- rankings_std %>% dplyr::filter(position == "QB") %>%
  dplyr::slice_head(n = 1) %>% dplyr::pull(replacement_ppg)
qb_rep_sf <- rankings_sf %>% dplyr::filter(position == "QB") %>%
  dplyr::slice_head(n = 1) %>% dplyr::pull(replacement_ppg)
message(glue::glue("  Standard:  {format(round(qb_rep_std, 1), nsmall = 1)} PPG"))
message(glue::glue("  Superflex: {format(round(qb_rep_sf, 1), nsmall = 1)} PPG"))
message(glue::glue("  Delta:     {format(round(qb_rep_std - qb_rep_sf, 1), nsmall = 1)} PPG drop"))

# ------------------------------------------------------------------------------
# EXAMPLE 6: Compute all three formats at once -- the headline product
# ------------------------------------------------------------------------------
# This is how you'd actually use it for draft prep. Stack all your league
# configs, run them in one shot, get back a long table you can pivot.

all_configs <- list(
  standard = config_standard,
  bestball = config_bestball,
  superflex = config_sflex
)

multi_rankings <- compute_multi_league_rankings(reconciled, all_configs)

message("\n--- Multi-league output shape ---")
str(multi_rankings, max.level = 1)

# ------------------------------------------------------------------------------
# EXAMPLE 7: Compare same player across formats
# ------------------------------------------------------------------------------
# Pick a few headline players and show how their rank shifts by format.

check_players <- c("Patrick Mahomes", "Christian McCaffrey", "Justin Jefferson",
                    "Travis Hunter", "Tai Felton")

message("\n--- Player rank comparison across formats ---")
multi_rankings %>%
  dplyr::filter(player_name %in% check_players) %>%
  dplyr::select(player_name, position, league_name, overall_rank,
                 position_rank, adjusted_vorp) %>%
  tidyr::pivot_wider(
    names_from  = league_name,
    values_from = c(overall_rank, position_rank, adjusted_vorp),
    names_glue  = "{league_name}_{.value}"
  ) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 8: Felton sanity check across formats
# ------------------------------------------------------------------------------
# Show where the team-aware projection lands Felton in each league.

message("\n--- Tai Felton across formats ---")
multi_rankings %>%
  dplyr::filter(grepl("Felton", player_name, ignore.case = TRUE)) %>%
  dplyr::select(league_name, league_format, overall_rank, position_rank,
                 r32_posterior_mu, replacement_ppg, vorp_base,
                 adjusted_vorp) %>%
  print()

message("\n--- Travis Hunter across formats (manual override candidate) ---")
multi_rankings %>%
  dplyr::filter(grepl("Hunter", player_name, ignore.case = TRUE),
                 grepl("Travis", player_name, ignore.case = TRUE)) %>%
  dplyr::select(league_name, league_format, overall_rank, position_rank,
                 r32_posterior_mu, replacement_ppg, vorp_base,
                 adjusted_vorp) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 9: Replacement levels by league
# ------------------------------------------------------------------------------
# See what the replacement floor is at each position in each format.

message("\n--- Replacement PPG by position and league ---")
multi_rankings %>%
  dplyr::group_by(league_name, league_format, position) %>%
  dplyr::summarise(
    replacement_ppg = dplyr::first(replacement_ppg),
    starter_count   = sum(position_rank <=
                            dplyr::n_distinct(position_rank[
                              r32_posterior_mu > replacement_ppg
                            ])),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = position,
    values_from = c(replacement_ppg, starter_count),
    names_glue  = "{position}_{.value}"
  ) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 10: SLEEPER INTEGRATION (commented out -- requires real league ID)
# ------------------------------------------------------------------------------
# Uncomment and replace with your actual Sleeper league ID or username.

# # Single league from Sleeper
# my_sleeper_config <- build_config_from_sleeper("YOUR_LEAGUE_ID_HERE")
# print(my_sleeper_config)
# my_sleeper_rankings <- compute_vorp_rankings(reconciled, my_sleeper_config)
#
# # All leagues for a Sleeper user
# all_my_configs <- build_configs_from_sleeper_user("YOUR_USERNAME")
# all_my_rankings <- compute_multi_league_rankings(reconciled, all_my_configs)

message("\nDone.")
