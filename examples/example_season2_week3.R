# ==============================================================================
# SEASON 2 WEEK 3: EXTENDED FANTASY SCORING -- USAGE EXAMPLES
# File: examples/example_season2_week3.R
# ==============================================================================
#
# PURPOSE
# -------
# Seven self-contained examples demonstrating calculate_fantasy_points_ext()
# and the supporting functions in R/17_extended_scoring.R.
#
# Each example is fully runnable independently. Run the SETUP section first,
# then run any example in any order.
#
# EXAMPLES
# --------
#   Example 1: Default scoring -- backward compatibility with Season 1
#   Example 2: Sleeper scoring -- all new parameters active
#   Example 3: Superflex league -- QB value under 6-point passing TDs
#   Example 4: Score decomposition -- isolating each point component
#   Example 5: compare_scoring_systems() -- how scoring changes rankings
#   Example 6: validate_ext_scoring_params() -- catching bad parameters
#   Example 7: Sack penalty and first down points -- positional impact
#
# NFL CONTEXT
# -----------
#   All analyses use 2025 regular season play-by-play data.
#   Garbage time is NOT filtered at the scoring level -- these are raw
#   fantasy totals as Sleeper would compute them (includes garbage time).
#   If you want efficiency-adjusted analysis, filter the pbp before calling.
#
# DEPENDENCIES
# ------------
#   nflfastR, nflreadr, dplyr, tidyr, purrr, glue, here
#   R/15_multi_season_pbp.R  (load_normalized_season)
#   R/17_extended_scoring.R  (all scoring functions)
#
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(here)

# Source production files -- exact names required
source(here::here("R", "15_multi_season_pbp.R"))
source(here::here("R", "17_extended_scoring.R"))

# ==============================================================================
# CONFIGURATION -- change this one value to switch seasons
# ==============================================================================
# Default: most recently completed NFL regular season.
# As of April 2026, that is 2025. Update to 2026 when the 2026 season completes.
SEASON <- 2025L

# ==============================================================================
# SETUP: Load 2025 data (required by all examples)
# ==============================================================================

cat("==============================================================================\n")
cat(glue("SETUP: Loading {SEASON} Play-by-Play and Roster Data\n"))
cat("==============================================================================\n\n")

# Load normalized 2025 season from Season 2 cache
# load_normalized_season() reads the cached RDS -- no re-download if cached
pbp_2025 <- load_normalized_season(
  season    = 2025L,
  cache_dir = here::here("data", "season2_cache")
)

# Verify core columns are present before any example runs
required_example_cols <- c(
  "season", "week", "game_id", "posteam",
  "passer_player_id", "passer_player_name",
  "rusher_player_id",  "rusher_player_name",
  "receiver_player_id", "receiver_player_name",
  "passing_yards", "rushing_yards", "receiving_yards",
  "pass_touchdown", "rush_touchdown",
  "interception", "fumble_lost"
)

missing_setup_cols <- setdiff(required_example_cols, names(pbp_2025))
if (length(missing_setup_cols) > 0) {
  stop(glue(
    "pbp_2025 is missing required columns: {paste(missing_setup_cols, collapse = ', ')}\n",
    "Re-run load_normalized_season() or check the cache file."
  ))
}

cat(glue("pbp_2025 loaded: {format(nrow(pbp_2025), big.mark=',')} rows | {n_distinct(pbp_2025$game_id)} games\n\n"))

# Load roster data for position anchoring (TE premium, position labels)
# nflreadr::load_rosters() returns gsis_id, full_name, position, team, season
roster_2025 <- nflreadr::load_rosters(seasons = 2025L)

cat(glue("roster_2025 loaded: {format(nrow(roster_2025), big.mark=',')} players\n\n"))

cat("Setup complete. Run any example below.\n\n")

# ==============================================================================
# EXAMPLE 1: Default scoring -- backward compatibility with Season 1
# ==============================================================================
#
# PURPOSE: Confirm that calling calculate_fantasy_points_ext() with no new
# arguments produces the same results as Season 1 calculate_fantasy_points().
# The new columns (two_pt_fantasy_points, first_down_fantasy_points, etc.)
# are present but all zero when new parameters are at their defaults.
#
# NFL CONTEXT: The Season 1 defaults are a Tiered PPR league with TE premium,
# 6-point passing TDs, and a 0.25-point rush attempt bonus. This is a
# common format in competitive fantasy leagues.
# ==============================================================================

cat("==============================================================================\n")
cat("EXAMPLE 1: Default Scoring -- Backward Compatibility with Season 1\n")
cat("==============================================================================\n\n")

fp_default <- calculate_fantasy_points_ext(
  pbp_data    = pbp_2025,
  roster_data = roster_2025,
  season      = 2025L
  # All new parameters default to 0 -- identical to Season 1 output
)

# Verify column names before any downstream use
cat("Output columns:\n")
cat(paste(names(fp_default), collapse = ", "), "\n\n")

# Confirm new component columns are all zero (backward compat check)
new_component_cols <- c(
  "two_pt_fantasy_points", "first_down_fantasy_points",
  "long_td_fantasy_points", "hundred_yard_fantasy_points"
)
missing_new_cols <- setdiff(new_component_cols, names(fp_default))
if (length(missing_new_cols) > 0) {
  warning(glue("Missing new component columns: {paste(missing_new_cols, collapse = ', ')}"))
} else {
  all_zero <- fp_default %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(new_component_cols), sum)) %>%
    dplyr::summarise(total = rowSums(.)) %>%
    dplyr::pull(total)

  cat(glue(
    "Backward compat check: sum of all new component columns = {all_zero}\n",
    "(should be 0 when new parameters are at their defaults)\n\n"
  ))
}

# Top 10 season scorers
top_10_default <- fp_default %>%
  dplyr::group_by(player_id, player_name, position, team) %>%
  dplyr::summarise(
    games        = dplyr::n_distinct(game_id),
    total_pts    = sum(total_fantasy_points, na.rm = TRUE),
    ppg          = total_pts / games,
    .groups      = "drop"
  ) %>%
  dplyr::filter(games >= 8L) %>%          # At least half a season
  dplyr::arrange(desc(total_pts)) %>%
  dplyr::slice_head(n = 10L)

cat(glue("Top 10 Fantasy Scorers -- {SEASON} Season (Default Settings, Min 8 Games):\n"))
print(top_10_default %>% dplyr::select(player_name, position, team, games, total_pts, ppg))

cat("\nKEY INSIGHT:\n")
cat("Default settings = Tiered PPR, TE premium, 6-pt pass TDs, 0.25 rush attempt bonus.\n")
cat("This is the Season 1 scoring baseline. All Season 2 extensions build on top of it.\n\n")

# ==============================================================================
# EXAMPLE 2: Sleeper scoring -- all new parameters active
# ==============================================================================
#
# PURPOSE: Apply a realistic Sleeper league scoring configuration with all
# Season 2 new parameters enabled. This is the primary use case for Week 3.
#
# NFL CONTEXT: First down bonuses reward players who move the chains, not
# just score. Long TD bonuses reward explosiveness over efficiency.
# The 100-yard game bonus shifts value toward volume players.
# Sack penalty makes QB selection more nuanced in superflex formats.
# ==============================================================================

cat("==============================================================================\n")
cat("EXAMPLE 2: Sleeper Scoring -- All New Parameters Active\n")
cat("==============================================================================\n\n")

# Common Sleeper league scoring configuration
fp_sleeper <- calculate_fantasy_points_ext(
  pbp_data             = pbp_2025,
  roster_data          = roster_2025,
  season               = 2025L,
  # Season 1 base (unchanged)
  use_tiered_ppr       = TRUE,
  te_premium           = TRUE,
  rush_att_bonus       = 0.25,
  pass_td              = 6,
  # New Season 2 parameters
  first_down_points    = 0.5,    # 0.5 pts per rush/rec first down
  long_td_bonus        = 2.0,    # 2 bonus pts for TD > 40 yards
  long_td_threshold    = 40L,    # Must exceed 40 yards to qualify
  hundred_yard_bonus   = 3.0,    # 3 pts for 100+ rush or rec yards in a game
  two_point_conversion = 2.0,    # 2 pts for scoring a 2-point conversion
  sack_penalty         = -1.0    # -1 pt per sack taken by QB
)

# Verify output columns exist before using them
stopifnot("first_down_fantasy_points" %in% names(fp_sleeper))
stopifnot("two_pt_fantasy_points"     %in% names(fp_sleeper))

# Season totals by player
sleeper_season <- fp_sleeper %>%
  dplyr::group_by(player_id, player_name, position, team) %>%
  dplyr::summarise(
    games                  = dplyr::n_distinct(game_id),
    total_pts              = sum(total_fantasy_points, na.rm = TRUE),
    base_pts               = sum(pass_fantasy_points + rush_fantasy_points +
                                   rec_fantasy_points, na.rm = TRUE),
    first_down_pts         = sum(first_down_fantasy_points, na.rm = TRUE),
    long_td_pts            = sum(long_td_fantasy_points, na.rm = TRUE),
    hundred_yd_pts         = sum(hundred_yard_fantasy_points, na.rm = TRUE),
    two_pt_pts             = sum(two_pt_fantasy_points, na.rm = TRUE),
    ppg                    = total_pts / dplyr::if_else(games > 0, games, 1L),
    .groups                = "drop"
  ) %>%
  dplyr::filter(games >= 8L) %>%
  dplyr::arrange(desc(total_pts))

cat("Top 10 Scorers -- Sleeper Settings (Min 8 Games):\n")
print(
  sleeper_season %>%
    dplyr::slice_head(n = 10L) %>%
    dplyr::select(player_name, position, team, games, total_pts, ppg,
                  first_down_pts, long_td_pts, hundred_yd_pts)
)

# How much do new parameters add per position?
cat("\nAverage New Points Per Game by Position (Sleeper add-ons only):\n")
pos_impact <- sleeper_season %>%
  dplyr::filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(
    n_players              = dplyr::n(),
    avg_first_down_pg      = mean(first_down_pts / games, na.rm = TRUE),
    avg_long_td_pg         = mean(long_td_pts / games, na.rm = TRUE),
    avg_hundred_yd_pg      = mean(hundred_yd_pts / games, na.rm = TRUE),
    avg_total_addon_pg     = mean(
      (first_down_pts + long_td_pts + hundred_yd_pts) / games,
      na.rm = TRUE
    ),
    .groups = "drop"
  )
print(pos_impact)

cat("\nKEY INSIGHT:\n")
cat("RBs and WRs benefit most from first down bonuses (they earn the first down).\n")
cat("QBs do NOT earn first down points -- only the rusher/receiver earns them.\n")
cat("100-yard bonuses disproportionately lift volume workhorses over committee backs.\n\n")

# ==============================================================================
# EXAMPLE 3: Superflex league -- QB value under 6-point passing TDs
# ==============================================================================
#
# PURPOSE: Demonstrate superflex_pass_td parameter and show how it shifts
# QB rankings relative to non-superflex.
#
# NFL CONTEXT: In superflex leagues, QBs start in the FLEX slot, effectively
# doubling QB scarcity. Passing TD value (4 vs 6 pts) dramatically affects
# how QBs rank relative to RBs and WRs. This example quantifies that gap.
# ==============================================================================

cat("==============================================================================\n")
cat("EXAMPLE 3: Superflex League -- QB Value Under 6-Point Passing TDs\n")
cat("==============================================================================\n\n")

# Superflex config: 6-pt pass TDs (superflex_pass_td overrides pass_td),
# first down bonus included (common in superflex leagues)
fp_superflex <- calculate_fantasy_points_ext(
  pbp_data          = pbp_2025,
  roster_data       = roster_2025,
  season            = 2025L,
  superflex_pass_td = 6.0,       # Overrides pass_td = 6 (same here, demo purpose)
  first_down_points = 0.5,
  use_tiered_ppr    = TRUE,
  te_premium        = FALSE       # Many superflex leagues drop TE premium
)

# Superflex with 4-pt pass TDs for comparison
fp_fourpt <- calculate_fantasy_points_ext(
  pbp_data          = pbp_2025,
  roster_data       = roster_2025,
  season            = 2025L,
  superflex_pass_td = 4.0,       # 4-point passing TDs
  first_down_points = 0.5,
  use_tiered_ppr    = TRUE,
  te_premium        = FALSE
)

# Summarize to season level for both systems
summarise_season <- function(fp_data, label) {
  fp_data %>%
    dplyr::group_by(player_id, player_name, position) %>%
    dplyr::summarise(
      team      = dplyr::last(team),   # last team if traded mid-season
      games     = dplyr::n_distinct(game_id),
      total_pts = sum(total_fantasy_points, na.rm = TRUE),
      ppg       = total_pts / dplyr::if_else(games > 0, games, 1L),
      .groups   = "drop"
    ) %>%
    dplyr::filter(games >= 8L) %>%
    dplyr::mutate(system = label)
}

sf_6pt <- summarise_season(fp_superflex, "6pt_pass_TD")
sf_4pt <- summarise_season(fp_fourpt,    "4pt_pass_TD")

# Join and compute the gap
sf_comparison <- sf_6pt %>%
  dplyr::inner_join(
    sf_4pt %>% dplyr::select(player_id, total_pts, ppg) %>%
      dplyr::rename(total_pts_4pt = total_pts, ppg_4pt = ppg),
    by = "player_id"
  ) %>%
  dplyr::mutate(
    td_value_gap     = total_pts - total_pts_4pt,   # How much 6pt is worth
    td_value_gap_ppg = ppg - ppg_4pt
  ) %>%
  dplyr::arrange(desc(td_value_gap))

cat("Players Who Benefit Most from 6-Point vs 4-Point Passing TDs:\n")
print(
  sf_comparison %>%
    dplyr::slice_head(n = 10L) %>%
    dplyr::select(player_name, position, team, games,
                  total_pts, total_pts_4pt, td_value_gap, td_value_gap_ppg)
)

# QB vs non-QB average gap
cat("\nAverage Season Point Gap by Position (6pt minus 4pt):\n")
sf_comparison %>%
  dplyr::filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(
    n_players       = dplyr::n(),
    avg_gap         = mean(td_value_gap, na.rm = TRUE),
    avg_gap_ppg     = mean(td_value_gap_ppg, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  dplyr::arrange(desc(avg_gap)) %>%
  print()

cat("\nKEY INSIGHT:\n")
# Compute insight from actual data rather than hardcoding
qb_avg_gap <- sf_comparison %>%
  dplyr::filter(position == "QB") %>%
  dplyr::summarise(avg = mean(td_value_gap, na.rm = TRUE)) %>%
  dplyr::pull(avg)
top_qb_gap <- sf_comparison %>%
  dplyr::filter(position == "QB") %>%
  dplyr::arrange(desc(td_value_gap)) %>%
  dplyr::slice_head(n = 1L)
cat(glue(
  "QBs average +{round(qb_avg_gap, 0)} season points moving from 4pt to 6pt passing TDs.\n"
))
cat(glue(
  "Top gap: {top_qb_gap$player_name} (+{round(top_qb_gap$td_value_gap, 0)} pts, ",
  "{top_qb_gap$games} games).\n"
))
cat("This is why superflex drafts see QBs taken 2-3 rounds earlier than standard.\n\n")

# ==============================================================================
# EXAMPLE 4: Score decomposition -- isolating each point component
# ==============================================================================
#
# PURPOSE: Show how total fantasy points break down by component for specific
# player archetypes. Helps validate that each new parameter is working
# correctly and provides insight into what drives value in Sleeper leagues.
#
# NFL CONTEXT: Understanding the scoring decomposition reveals which player
# types are being repriced by Sleeper's scoring options. A high first_down_pts
# value relative to total identifies a possession receiver. A high
# hundred_yd_pts value identifies a volume workhorse.
# ==============================================================================

cat("==============================================================================\n")
cat("EXAMPLE 4: Score Decomposition -- Isolating Each Point Component\n")
cat("==============================================================================\n\n")

# Use Sleeper settings from Example 2 (re-run rather than assume fp_sleeper exists)
fp_decomp <- calculate_fantasy_points_ext(
  pbp_data             = pbp_2025,
  roster_data          = roster_2025,
  season               = 2025L,
  first_down_points    = 0.5,
  long_td_bonus        = 2.0,
  long_td_threshold    = 40L,
  hundred_yard_bonus   = 3.0,
  two_point_conversion = 2.0,
  sack_penalty         = -1.0
)

# Season-level decomposition
decomp_season <- fp_decomp %>%
  dplyr::group_by(player_id, player_name, position, team) %>%
  dplyr::summarise(
    games              = dplyr::n_distinct(game_id),
    total_pts          = sum(total_fantasy_points, na.rm = TRUE),
    pass_pts           = sum(pass_fantasy_points,         na.rm = TRUE),
    rush_pts           = sum(rush_fantasy_points,         na.rm = TRUE),
    rec_pts            = sum(rec_fantasy_points,          na.rm = TRUE),
    two_pt_pts         = sum(two_pt_fantasy_points,       na.rm = TRUE),
    first_down_pts     = sum(first_down_fantasy_points,   na.rm = TRUE),
    long_td_pts        = sum(long_td_fantasy_points,      na.rm = TRUE),
    hundred_yd_pts     = sum(hundred_yard_fantasy_points, na.rm = TRUE),
    .groups            = "drop"
  ) %>%
  dplyr::filter(games >= 8L) %>%
  dplyr::mutate(
    # Percentage of total from each new component
    pct_first_down = round(100 * first_down_pts / total_pts, 1),
    pct_long_td    = round(100 * long_td_pts    / total_pts, 1),
    pct_hundred_yd = round(100 * hundred_yd_pts / total_pts, 1),
    pct_new_total  = pct_first_down + pct_long_td + pct_hundred_yd
  )

# Top RBs: who benefits most from new parameters?
cat("Top RBs by New Parameter Contribution (Sleeper Settings):\n")
decomp_season %>%
  dplyr::filter(position == "RB") %>%
  dplyr::arrange(desc(first_down_pts + hundred_yd_pts)) %>%
  dplyr::slice_head(n = 8L) %>%
  dplyr::select(
    player_name, team, games, total_pts,
    first_down_pts, hundred_yd_pts, long_td_pts, pct_new_total
  ) %>%
  print()

# Top WRs: how does first_down + long_td reshape receiver rankings?
cat("\nTop WRs by New Parameter Contribution (Sleeper Settings):\n")
decomp_season %>%
  dplyr::filter(position == "WR") %>%
  dplyr::arrange(desc(first_down_pts + long_td_pts)) %>%
  dplyr::slice_head(n = 8L) %>%
  dplyr::select(
    player_name, team, games, total_pts,
    first_down_pts, long_td_pts, hundred_yd_pts, pct_new_total
  ) %>%
  print()

# QBs: sack penalty impact
cat("\nQBs with Largest Sack Penalty (Sleeper Settings, -1pt per sack):\n")
fp_decomp %>%
  dplyr::filter(!is.na(sacks_taken)) %>%
  dplyr::group_by(player_id, player_name, team) %>%
  dplyr::summarise(
    games        = dplyr::n_distinct(game_id),
    total_sacks  = sum(sacks_taken, na.rm = TRUE),
    sack_penalty_pts = total_sacks * (-1.0),
    .groups      = "drop"
  ) %>%
  dplyr::filter(games >= 8L, total_sacks > 0L) %>%
  dplyr::arrange(sack_penalty_pts) %>%   # Most negative first
  dplyr::slice_head(n = 8L) %>%
  print()

cat("\nKEY INSIGHT:\n")
cat("First down bonuses reward possession receivers over splash-play specialists.\n")
cat("100-yard bonuses create a nonlinear reward -- the workhorse earns a step change\n")
cat("in value the moment they cross 100 yards, regardless of how they got there.\n\n")

# ==============================================================================
# EXAMPLE 5: compare_scoring_systems() -- how scoring changes rankings
# ==============================================================================
#
# PURPOSE: Use compare_scoring_systems() to run three scoring configs in one
# call and see which players gain or lose rank between systems.
#
# NFL CONTEXT: The rank shift between systems reveals which player archetypes
# are being mispriced by any given scoring format. A player who gains 10+
# ranks moving from Standard to Sleeper is being undervalued in Standard
# leagues that use similar settings.
# ==============================================================================

cat("==============================================================================\n")
cat("EXAMPLE 5: compare_scoring_systems() -- How Scoring Changes Rankings\n")
cat("==============================================================================\n\n")

system_comparison <- compare_scoring_systems(
  pbp_data    = pbp_2025,
  roster_data = roster_2025,
  season      = 2025L,
  min_games   = 8L,
  systems     = list(
    Standard = list(
      use_tiered_ppr = FALSE,
      te_premium     = FALSE,
      rush_att_bonus = 0,
      ppr            = 0,
      pass_td        = 4
    ),
    PPR = list(
      use_tiered_ppr = FALSE,
      te_premium     = FALSE,
      ppr            = 1.0
    ),
    Sleeper = list(
      use_tiered_ppr       = TRUE,
      te_premium           = TRUE,
      rush_att_bonus       = 0.25,
      first_down_points    = 0.5,
      long_td_bonus        = 2.0,
      long_td_threshold    = 40L,
      hundred_yard_bonus   = 3.0,
      two_point_conversion = 2.0,
      sack_penalty         = -1.0
    )
  )
)

# Verify output columns exist before using them
stopifnot("Standard_total" %in% names(system_comparison))
stopifnot("Sleeper_total"  %in% names(system_comparison))

# Add overall rank per system
system_comparison <- system_comparison %>%
  dplyr::filter(!is.na(Standard_total), !is.na(Sleeper_total)) %>%
  dplyr::mutate(
    rank_standard = dplyr::min_rank(desc(Standard_total)),
    rank_sleeper  = dplyr::min_rank(desc(Sleeper_total)),
    rank_shift    = rank_standard - rank_sleeper   # Positive = moved up in Sleeper
  )

# Overall top 15 under Sleeper settings
cat("Top 15 Overall -- Sleeper Settings (with cross-system comparison):\n")
system_comparison %>%
  dplyr::arrange(rank_sleeper) %>%
  dplyr::slice_head(n = 15L) %>%
  dplyr::select(
    player_name, position, team,
    Standard_total, PPR_total, Sleeper_total,
    rank_standard, rank_sleeper, rank_shift
  ) %>%
  print()

# Biggest winners moving from Standard to Sleeper
cat("\nBiggest Winners: Standard -> Sleeper (Largest Rank Improvement):\n")
system_comparison %>%
  dplyr::filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  dplyr::arrange(desc(rank_shift)) %>%
  dplyr::slice_head(n = 8L) %>%
  dplyr::select(player_name, position, team, Standard_total, Sleeper_total, rank_shift) %>%
  print()

# Biggest losers
cat("\nBiggest Losers: Standard -> Sleeper (Largest Rank Drop):\n")
system_comparison %>%
  dplyr::filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  dplyr::arrange(rank_shift) %>%
  dplyr::slice_head(n = 8L) %>%
  dplyr::select(player_name, position, team, Standard_total, Sleeper_total, rank_shift) %>%
  print()

cat("\nKEY INSIGHT:\n")
cat("Players who gain rank in Sleeper vs Standard are being undervalued by ADP set\n")
cat("against Standard leagues. Volume workhorses (high first-down and 100-yd rates)\n")
cat("are consistently underpriced in Standard formats that ignore these bonuses.\n\n")

# ==============================================================================
# EXAMPLE 6: Parameter name validation -- the Week 5 Sleeper API safety pattern
# ==============================================================================
#
# PURPOSE: Demonstrate how to safely pass API-sourced scoring parameters into
# calculate_fantasy_points_ext(). The function has a strict signature (no ...),
# so a misspelled parameter name causes an immediate error. The risk for the
# Week 5 Sleeper integration is that a mapping error between Sleeper's field
# names and this function's parameter names produces that error at runtime
# rather than being caught before the call.
#
# THE PATTERN:
#   Step 1 -- Check incoming names against get_ext_scoring_defaults() to catch
#             any mismatches BEFORE calling the function.
#   Step 2 -- Run validate_ext_scoring_params() to catch wrong types/values.
#   Step 3 -- Only then call calculate_fantasy_points_ext() with do.call().
#
# IMPORTANT: R functions WITHOUT ... error immediately on unknown arguments.
# R functions WITH ... silently swallow unknown arguments. calculate_fantasy_
# points_ext() has no ..., so typos error loudly -- which is actually safer
# than silent drops. But a runtime error mid-pipeline is still a failure mode
# to prevent with pre-call name validation.
# ==============================================================================

cat("==============================================================================\n")
cat("EXAMPLE 6: Parameter Name Validation -- The Week 5 Sleeper API Safety Pattern\n")
cat("==============================================================================\n\n")

# --- Case 1: Correct spelling -- hundred_yard_bonus is applied ---
cat("Case 1: Correct parameter name -- hundred_yard_bonus = 3.0\n")
fp_correct <- calculate_fantasy_points_ext(
  pbp_data           = pbp_2025,
  roster_data        = roster_2025,
  season             = 2025L,
  hundred_yard_bonus = 3.0
)

hundred_correct <- fp_correct %>%
  dplyr::summarise(
    total_hundred_yd_pts = sum(hundred_yard_fantasy_points, na.rm = TRUE),
    games_with_bonus     = sum(hundred_yard_fantasy_points > 0, na.rm = TRUE)
  )

cat(glue(
  "  Total hundred-yard bonus points awarded: {round(hundred_correct$total_hundred_yd_pts, 1)}\n",
  "  Player-games with bonus applied:         {hundred_correct$games_with_bonus}\n\n"
))

# --- Case 2: Misspelled -- hundred_year_bonus causes an immediate error ---
cat("Case 2: Misspelled parameter name -- hundred_year_bonus = 3.0 (wrong)\n")
cat("  calculate_fantasy_points_ext() has no ..., so R errors immediately.\n")
cat("  Using tryCatch() to demonstrate the error message:\n\n")

result <- tryCatch(
  calculate_fantasy_points_ext(
    pbp_data           = pbp_2025,
    roster_data        = roster_2025,
    season             = 2025L,
    hundred_year_bonus = 3.0    # Typo -- will error, not silently drop
  ),
  error = function(e) {
    cat(glue("  Error caught: {conditionMessage(e)}\n\n"))
    NULL
  }
)

cat("  The error fires before any data is processed -- no partial results.\n")
cat("  This is better than a silent drop but still fails the pipeline.\n\n")

# --- Case 3: The safe pattern for API-sourced parameters ---
cat("Case 3: Safe pattern -- validate names before calling\n\n")

# Simulate what the Week 5 Sleeper API integration will receive:
# a named list of scoring settings from an external source
sleeper_api_response <- list(
  pass_td              = 6,
  rush_att_bonus       = 0.25,
  first_down_points    = 0.5,
  hundred_year_bonus   = 3.0,   # Typo from a mapping error
  long_td_bonus        = 2.0,
  long_td_threshold    = 40L,
  two_point_conversion = 2.0,
  sack_penalty         = -1.0
)

# Step 1: compare incoming names against the known valid parameter set
valid_param_names <- names(get_ext_scoring_defaults(show_new_only = FALSE))
incoming_names    <- names(sleeper_api_response)
unknown_names     <- setdiff(incoming_names, valid_param_names)

cat("  Incoming names from simulated Sleeper API response:\n")
cat(paste("   ", incoming_names, collapse = "\n"), "\n\n")

if (length(unknown_names) > 0) {
  cat("  UNKNOWN parameter names detected -- fix these before calling:\n")
  purrr::walk(unknown_names, ~ cat(glue("    - {.x}\n")))
  cat("\n  Removing unknown names from the parameter list...\n\n")
  sleeper_api_clean <- sleeper_api_response[
    intersect(incoming_names, valid_param_names)
  ]
} else {
  cat("  All parameter names are valid.\n\n")
  sleeper_api_clean <- sleeper_api_response
}

# Step 2: validate types and values on the cleaned list
check <- do.call(validate_ext_scoring_params, sleeper_api_clean)
cat(glue("  validate_ext_scoring_params() result: valid = {check$valid}\n"))
if (length(check$warnings) > 0) {
  cat("  Warnings:\n")
  purrr::walk(check$warnings, ~ cat(glue("    - {.x}\n")))
}
cat("\n")

# Step 3: only call calculate_fantasy_points_ext() after both checks pass
if (check$valid && length(unknown_names) == 0) {
  cat("  Both checks passed -- safe to call.\n")
} else {
  cat("  Checks failed -- unknown names removed, calling with cleaned parameters.\n")
  fp_api_safe <- do.call(
    calculate_fantasy_points_ext,
    c(list(pbp_data = pbp_2025, roster_data = roster_2025, season = 2025L),
      sleeper_api_clean)
  )
  cat(glue(
    "  Safe call succeeded: {format(nrow(fp_api_safe), big.mark=',')} player-games\n\n"
  ))
}

# --- Case 4: validate_ext_scoring_params() on a bad type ---
cat("Case 4: validate_ext_scoring_params() -- catches wrong types\n")
check_bad <- validate_ext_scoring_params(
  pass_yd = "zero point oh four"   # String instead of numeric
)
cat(glue("  Valid: {check_bad$valid}\n"))
cat("  Errors:\n")
purrr::walk(check_bad$errors, ~ cat(glue("    - {.x}\n")))

cat("\nKEY INSIGHT:\n")
cat("calculate_fantasy_points_ext() errors immediately on unknown argument names.\n")
cat("This is safer than a silent drop but still breaks the pipeline at runtime.\n")
cat("The two-step Week 5 safety pattern:\n")
cat("  Step 1: setdiff(incoming_names, valid_param_names) -- catch name mismatches.\n")
cat("  Step 2: validate_ext_scoring_params() -- catch bad types and values.\n")
cat("Run both before every do.call() with API-sourced parameters.\n\n")

# ==============================================================================
# EXAMPLE 7: Sack penalty and first down points -- positional impact by week
# ==============================================================================
#
# PURPOSE: Isolate the weekly impact of sack_penalty on QB rankings and
# first_down_points on RB/WR rankings. Shows which weeks a parameter
# matters most and which player types are most sensitive.
#
# NFL CONTEXT: Sack rate is not stable week-to-week -- it spikes in bad
# weather, against elite pass rushes, and when QBs scramble more under
# pressure. The first down bonus is more stable because first down rates
# are tied to target share and role, which are consistent week-to-week.
# ==============================================================================

cat("==============================================================================\n")
cat("EXAMPLE 7: Sack Penalty and First Down Points -- Weekly Positional Impact\n")
cat("==============================================================================\n\n")

# Run two configs: one with sack penalty and first down, one without
fp_with_addons <- calculate_fantasy_points_ext(
  pbp_data          = pbp_2025,
  roster_data       = roster_2025,
  season            = 2025L,
  first_down_points = 0.5,
  sack_penalty      = -1.0
)

fp_without_addons <- calculate_fantasy_points_ext(
  pbp_data    = pbp_2025,
  roster_data = roster_2025,
  season      = 2025L
  # first_down_points = 0 (default), sack_penalty = 0 (default)
)

# Verify columns before joining
stopifnot(all(c("season", "week", "game_id", "player_id", "total_fantasy_points") %in%
               names(fp_with_addons)))
stopifnot(all(c("season", "week", "game_id", "player_id", "total_fantasy_points") %in%
               names(fp_without_addons)))

# Join on player-game key and compute the delta.
# Aggregate both sides to one row per player-game before joining to prevent
# the many-to-many warning that fires when a player appears in both the
# passing and receiving component tables for the same game.
weekly_delta <- fp_with_addons %>%
  dplyr::group_by(season, week, game_id, player_id, player_name, position, team) %>%
  dplyr::summarise(
    total_fantasy_points      = sum(total_fantasy_points,      na.rm = TRUE),
    first_down_fantasy_points = sum(first_down_fantasy_points, na.rm = TRUE),
    sacks_taken               = sum(dplyr::coalesce(sacks_taken, 0L), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    fp_without_addons %>%
      dplyr::group_by(season, week, game_id, player_id) %>%
      dplyr::summarise(
        total_pts_base = sum(total_fantasy_points, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("season", "week", "game_id", "player_id")
  ) %>%
  dplyr::mutate(
    addon_pts = total_fantasy_points - total_pts_base
  )

# Weekly QB sack penalty range
cat(glue("QB Sack Penalty Distribution by Week ({SEASON}, Weeks 1-18):\n"))
weekly_delta %>%
  dplyr::filter(position == "QB", !is.na(sacks_taken)) %>%
  dplyr::group_by(week) %>%
  dplyr::summarise(
    n_qbs          = dplyr::n(),
    avg_sacks       = mean(sacks_taken, na.rm = TRUE),
    max_sacks       = max(sacks_taken, na.rm = TRUE),
    avg_penalty_pts = mean(addon_pts, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  dplyr::arrange(week) %>%
  print(n = 18L)

# RB and WR first down bonus -- season summary by player
cat("\nTop RBs by First Down Bonus Points (full season):\n")
weekly_delta %>%
  dplyr::filter(position == "RB") %>%
  dplyr::group_by(player_id, player_name, team) %>%
  dplyr::summarise(
    games              = dplyr::n_distinct(game_id),
    total_first_dn_pts = sum(first_down_fantasy_points, na.rm = TRUE),
    first_dn_ppg       = total_first_dn_pts / dplyr::if_else(games > 0, games, 1L),
    .groups            = "drop"
  ) %>%
  dplyr::filter(games >= 8L) %>%
  dplyr::arrange(desc(total_first_dn_pts)) %>%
  dplyr::slice_head(n = 8L) %>%
  print()

cat("\nTop WRs by First Down Bonus Points (full season):\n")
weekly_delta %>%
  dplyr::filter(position == "WR") %>%
  dplyr::group_by(player_id, player_name, team) %>%
  dplyr::summarise(
    games              = dplyr::n_distinct(game_id),
    total_first_dn_pts = sum(first_down_fantasy_points, na.rm = TRUE),
    first_dn_ppg       = total_first_dn_pts / dplyr::if_else(games > 0, games, 1L),
    .groups            = "drop"
  ) %>%
  dplyr::filter(games >= 8L) %>%
  dplyr::arrange(desc(total_first_dn_pts)) %>%
  dplyr::slice_head(n = 8L) %>%
  print()

cat("\nKEY INSIGHT:\n")
cat("First down bonus points are highly stable week-to-week because they track\n")
cat("target share and touch volume. Sack penalty is volatile -- it spikes in\n")
cat("cold weather weeks and against elite pass rush units. When building\n")
cat("projections in Week 5, first_down_points should be treated as a stable\n")
cat("feature while sack_penalty should be game-context-weighted.\n\n")

# ==============================================================================
# BEST PRACTICES SUMMARY
# ==============================================================================

cat("==============================================================================\n")
cat("BEST PRACTICES SUMMARY\n")
cat("==============================================================================\n\n")

cat("1. COLUMN VERIFICATION\n")
cat("   Always run names() on function output before writing downstream code.\n")
cat("   Output columns: season, week, game_id, player_id, player_name,\n")
cat("   position, team, pass_fantasy_points, rush_fantasy_points,\n")
cat("   rec_fantasy_points, two_pt_fantasy_points, first_down_fantasy_points,\n")
cat("   long_td_fantasy_points, hundred_yard_fantasy_points, total_fantasy_points,\n")
cat("   plus raw stat columns.\n\n")

cat("2. BACKWARD COMPATIBILITY\n")
cat("   Calling calculate_fantasy_points_ext() with no new arguments produces\n")
cat("   output identical to Season 1 calculate_fantasy_points(). The new\n")
cat("   component columns are present but all zero.\n\n")

cat("3. POSITION ANCHORING\n")
cat("   Always pass roster_data when te_premium = TRUE. Without it, TE premium\n")
cat("   has no effect because position cannot be confirmed from play-level data.\n\n")

cat("4. PARAMETER VALIDATION\n")
cat("   Run validate_ext_scoring_params() before any call where parameters come\n")
cat("   from an external source. This is mandatory before the Week 5 Sleeper\n")
cat("   API integration.\n\n")

cat("5. FIRST DOWN ATTRIBUTION\n")
cat("   The RUSHER earns first_down_points on rushing first downs.\n")
cat("   The RECEIVER earns first_down_points on receiving first downs.\n")
cat("   The PASSER earns no first down bonus.\n\n")

cat("6. LONG TD ATTRIBUTION\n")
cat("   The RECEIVER earns long_td_bonus on passing plays.\n")
cat("   The RUSHER earns long_td_bonus on rushing plays.\n")
cat("   The PASSER earns no long TD bonus.\n\n")

cat("7. TWO-POINT CONVERSIONS\n")
cat("   Points go to the scorer (rusher or receiver) only.\n")
cat("   Passer does not receive two_point_conversion points.\n")
cat("   This matches default Sleeper behavior.\n\n")

cat("8. SUPERFLEX PARAMETER\n")
cat("   superflex_pass_td replaces pass_td when non-zero.\n")
cat("   Set superflex_pass_td = 0 (default) and pass_td directly for\n")
cat("   non-superflex leagues.\n\n")

cat("Example script complete.\n")
