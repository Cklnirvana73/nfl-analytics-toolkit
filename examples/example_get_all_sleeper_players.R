# ==============================================================================
# Example/Verification: get_all_sleeper_players()
# File: examples/example_get_all_sleeper_players.R
#
# Quick standalone verification that the new R/19 function pulls the full
# Sleeper player database, caches it, and produces actionable output.
# Run this BEFORE wiring the function into R/29's roster refresh step.
#
# What you should see:
#   - First call: hits the endpoint (~10MB download, takes 10-30 seconds)
#   - Subsequent calls within 24h: instant load from cache
#   - Headline test: Tyreek Hill shows team = NA (free agent), Jaylen Waddle
#     shows team = DEN (the trade we know happened)
# ==============================================================================

library(dplyr)
library(here)

source(here::here("R", "19_sleeper_api.R"))

# ------------------------------------------------------------------------------
# EXAMPLE 1: Pull all players (first call hits endpoint, caches)
# ------------------------------------------------------------------------------

players <- get_all_sleeper_players()

message("\n--- Output shape ---")
str(players, max.level = 1)

# ------------------------------------------------------------------------------
# EXAMPLE 2: Headline tests -- did this fix what we thought it would?
# ------------------------------------------------------------------------------

message("\n--- Tyreek Hill (should show team = NA, is_free_agent = TRUE) ---")
players %>%
  dplyr::filter(grepl("Tyreek Hill", player_name, ignore.case = TRUE)) %>%
  dplyr::select(player_name, position, team, status, is_free_agent,
                 depth_chart_position, depth_chart_order) %>%
  print()

message("\n--- Jaylen Waddle (should show team = DEN, not MIA) ---")
players %>%
  dplyr::filter(grepl("Jaylen Waddle", player_name, ignore.case = TRUE)) %>%
  dplyr::select(player_name, position, team, status, is_free_agent,
                 depth_chart_position, depth_chart_order) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 3: All Active skill-position free agents
# ------------------------------------------------------------------------------

message("\n--- All Active QB/RB/WR/TE free agents ---")
players %>%
  dplyr::filter(is_free_agent,
                 status == "Active",
                 position %in% c("QB", "RB", "WR", "TE")) %>%
  dplyr::select(player_name, position, age, years_exp, injury_status) %>%
  dplyr::arrange(position, dplyr::desc(years_exp)) %>%
  print(n = 30)

# ------------------------------------------------------------------------------
# EXAMPLE 4: Compare Sleeper to current R/32 reconciled projections
# ------------------------------------------------------------------------------
# This is the diagnostic that will drive the R/29 fix. Where does Sleeper
# disagree with our current team assignments?

message("\n--- Team-assignment discrepancies vs current R/32 output ---")
reconciled <- readRDS(here::here("data", "season2_cache",
                                  "s2_week15_reconciled_projections.rds"))

discrepancies <- reconciled %>%
  dplyr::select(nfl_gsis_id, player_name, position,
                 team_r32 = team, r32_posterior_mu) %>%
  dplyr::inner_join(
    players %>% dplyr::select(nfl_gsis_id, team_sleeper = team,
                                is_free_agent),
    by = "nfl_gsis_id"
  ) %>%
  dplyr::filter(team_r32 != team_sleeper | (is_free_agent & !is.na(team_r32))) %>%
  dplyr::arrange(dplyr::desc(r32_posterior_mu))

message(glue::glue("\nFound {nrow(discrepancies)} players with stale team assignments"))
message("Top 30 (by R/32 projection) -- these are the ones that will move:")
discrepancies %>%
  dplyr::select(player_name, position, team_r32, team_sleeper,
                 is_free_agent, r32_posterior_mu) %>%
  head(30) %>%
  print()

# ------------------------------------------------------------------------------
# EXAMPLE 5: Cache behavior verification
# ------------------------------------------------------------------------------

message("\n--- Second call should load from cache (instant) ---")
t1 <- Sys.time()
players_cached <- get_all_sleeper_players()
t2 <- Sys.time()
message(glue::glue("  Cached load took: ",
                    "{format(round(as.numeric(difftime(t2, t1, units = 'secs')), 3), nsmall = 3)}s"))
message(glue::glue("  Identical to first call: ",
                    "{identical(players, players_cached)}"))

message("\nDone.")
