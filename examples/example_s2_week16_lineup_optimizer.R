# ==============================================================================
# SEASON 2 WEEK 16: LINEUP + WAIVER OPTIMIZER -- USAGE EXAMPLES
# File: examples/example_s2_week16_lineup_optimizer.R
# ==============================================================================
#
# PURPOSE
# -------
# Self-contained examples for R/35_lineup_optimizer.R: the matchup engine,
# player valuation, the integer-program lineup, and both waiver modes. Every
# example uses save_output = FALSE so nothing here overwrites the real
# s2_week16_optimal_lineup output.
#
# R/35 normally builds its league source from Sleeper (build_league_source),
# which needs a live league id. To keep these examples runnable with no network
# call, the SETUP block defines make_demo_source(): it assembles an equivalent
# league_source by sampling real projected players into a synthetic 12-team
# league. This is the same idea as R/33's examples building configs inline.
# Example 9 shows the real Sleeper workflow (commented, fill in your league id).
#
# EXAMPLES
#   1. Defense-vs-position matchup factors (compute_dvp_factors)
#   2. Opponent-offense matchup factors for DEF (compute_def_matchup_factors)
#   3. The player pool and format-aware value (assemble + compute_player_values)
#   4. Optimal lineup, preseason / neutral matchup
#   5. Optimal lineup, in-season with the matchup adjustment applied
#   6. Waiver adds, lineup mode (immediate this-week lineup gain)
#   7. Waiver adds, stash mode (forward-looking dynasty value)
#   8. Availability gating with a manual override CSV
#   9. The real Sleeper workflow (commented)
#
# PREREQUISITES
# -------------
# Normalized pbp in data/season2_cache/ for the matchup examples:
#   pbp_normalized_2024.rds, pbp_normalized_2025.rds
# Projection inputs from earlier in the pipeline:
#   s2_week15_reconciled_projections.rds   (R/32)
#   s2_week16_def_st_projections.rds        (R/34)
# R/35 sources R/05, R/15, R/19, R/29, and R/33 automatically if not loaded.
#
# NFL SEASON STRUCTURE
# --------------------
# Regular season weeks 1-18. A NULL week means preseason (neutral matchup, every
# factor 1.0). Examples 5 uses a COMPLETED season (2025) as a stand-in live
# season so the matchup adjustment is visible and runnable today.
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP (run once before any example)
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(here)
library(glue)

source(here::here("R", "35_lineup_optimizer.R"))

reconciled <- readRDS(here::here("data", "season2_cache",
                                 "s2_week15_reconciled_projections.rds"))
def_proj   <- readRDS(here::here("data", "season2_cache",
                                 "s2_week16_def_st_projections.rds"))

message(glue("Loaded {nrow(reconciled)} reconciled projections (R/32) and ",
             "{nrow(def_proj)} DEF projections (R/34)."))

# ------------------------------------------------------------------------------
# HELPER: build a synthetic league_source from the projection files
# ------------------------------------------------------------------------------
# Samples real projected players into a 12-team league: the top players are
# rostered (round-robin across teams), the rest are free agents, and the user is
# roster 1 with a realistic spread plus the top-projected DEF. Ages are NA here
# because the projection files carry no age (real Sleeper sources do), so the
# dynasty age trajectory is neutral in these examples; prospect scores still
# flow if you pass them.
make_demo_source <- function(reconciled, def_proj, format = "dynasty",
                             n_teams = 12L) {

  off <- reconciled %>%
    dplyr::filter(position %in% c("QB", "RB", "WR", "TE"),
                  !is.na(r32_posterior_mu)) %>%
    dplyr::arrange(desc(r32_posterior_mu)) %>%
    dplyr::mutate(rk = dplyr::row_number())

  # Roster the top n_teams * 15 offensive players, round-robin across teams.
  n_roster <- n_teams * 15L
  rostered <- off %>%
    dplyr::slice(1:min(n_roster, nrow(off))) %>%
    dplyr::mutate(roster_id = ((rk - 1L) %% n_teams) + 1L)

  players <- rostered %>%
    dplyr::transmute(
      sleeper_player_id = nfl_gsis_id,
      nfl_gsis_id,
      player_name, position, team,
      status        = "Active",
      injury_status = NA_character_,
      age           = NA_integer_,
      is_free_agent = FALSE
    )

  rosters_resolved <- rostered %>%
    dplyr::transmute(
      league_id = "demo", roster_id,
      owner_id  = paste0("owner_", roster_id),
      player_id = nfl_gsis_id,
      is_starter = FALSE, is_reserve = FALSE, on_bench = TRUE,
      nfl_gsis_id,
      match_method = "demo", match_confidence = "high",
      status = "Active", injury_status = NA_character_, age = NA_integer_
    )

  # Give the user (roster 1) the top-projected DEF so the DEF slot fills.
  top_def <- def_proj %>% dplyr::arrange(desc(def_proj_ppg)) %>%
    dplyr::slice(1) %>% dplyr::pull(team)
  players <- dplyr::bind_rows(players, tibble::tibble(
    sleeper_player_id = top_def, nfl_gsis_id = NA_character_,
    player_name = paste(top_def, "DEF"), position = "DEF", team = top_def,
    status = "Active", injury_status = NA_character_, age = NA_integer_,
    is_free_agent = FALSE))
  rosters_resolved <- dplyr::bind_rows(rosters_resolved, tibble::tibble(
    league_id = "demo", roster_id = 1L, owner_id = "owner_1",
    player_id = top_def, is_starter = FALSE, is_reserve = FALSE,
    on_bench = TRUE, nfl_gsis_id = NA_character_, match_method = "demo",
    match_confidence = "high", status = "Active",
    injury_status = NA_character_, age = NA_integer_))

  config <- build_league_config(
    league_name = "Demo 12-team", num_teams = n_teams,
    starters = list(QB = 1, RB = 2, WR = 2, TE = 1),
    flex = 1L, superflex = 0L, format = "ppr")

  user_gsis <- rosters_resolved %>%
    dplyr::filter(roster_id == 1L, !is.na(nfl_gsis_id)) %>%
    dplyr::pull(nfl_gsis_id)

  structure(list(
    platform = "demo", league_id = "demo", season = SEASON,
    config = config, format = format,
    league_type = if (format == "dynasty") 2L else 0L,
    def_slots = 1L,
    roster_positions = c("QB", "RB", "WR", "TE", "FLEX", "DEF"),
    rosters_resolved = rosters_resolved, players = players,
    all_rostered_gsis = unique(stats::na.omit(rosters_resolved$nfl_gsis_id)),
    user_roster_id = 1L, user_roster_gsis = user_gsis, source = "demo"
  ), class = c("league_source", "list"))
}

demo <- make_demo_source(reconciled, def_proj, format = "dynasty")
vorp <- compute_vorp_rankings(reconciled, demo$config)
print(demo)


# ==============================================================================
# EXAMPLE 1: Defense-vs-position matchup factors
# ==============================================================================
# Purpose: compute_dvp_factors() turns each defense into a per-position
# multiplier from fantasy points allowed last season, normalized to the league
# average and scoring-neutral so league quirks cancel.
#
# Key insight: a factor above 1 is a favorable matchup (the defense gives up more
# than average to that position), below 1 is a tough matchup. These are the
# numbers the optimizer multiplies a player's projection by.

dvp <- compute_dvp_factors(seasons = 2025L)

cat("\nEasiest WR matchups (defenses that gave up the most to WRs):\n")
dvp %>%
  dplyr::filter(position == "WR") %>%
  dplyr::arrange(desc(dvp_factor)) %>%
  dplyr::select(def_team, position, dvp_allowed_ppg, dvp_factor) %>%
  head(5) %>% print()

cat("\nToughest RB matchups:\n")
dvp %>%
  dplyr::filter(position == "RB") %>%
  dplyr::arrange(dvp_factor) %>%
  dplyr::select(def_team, position, dvp_allowed_ppg, dvp_factor) %>%
  head(5) %>% print()


# ==============================================================================
# EXAMPLE 2: Opponent-offense matchup factors for the DEF slot
# ==============================================================================
# Purpose: compute_def_matchup_factors() is the DEF analogue. For each offense it
# measures how many DEF/ST fantasy points that offense concedes to opposing
# defenses, normalized to league average.
#
# Key insight: a defense facing a turnover-prone, low-scoring offense gets a
# factor above 1. This is how a streaming DEF decision accounts for the matchup.

def_factors <- compute_def_matchup_factors(seasons = 2025L)

cat("\nOffenses that concede the most to opposing defenses (best DEF streams):\n")
def_factors %>%
  dplyr::arrange(desc(def_matchup_factor)) %>%
  head(5) %>% print()


# ==============================================================================
# EXAMPLE 3: The player pool and format-aware value
# ==============================================================================
# Purpose: assemble_player_pool() stacks offense (R/32) and DEF (R/34) and tags
# roster membership; compute_player_values() adds the format value. In dynasty,
# value blends rest-of-season with a forward score, and forward_vorp is the
# cross-position forward value used by the stash waiver.
#
# Key insight: on_user_roster and is_available are resolved by gsis AND by name,
# so a rostered rookie with no gsis mapping is still flagged correctly.

pool <- assemble_player_pool(demo, reconciled, vorp, def_proj)
pool <- compute_player_values(pool, format = "dynasty")

cat("\nYour roster, by dynasty value:\n")
pool %>%
  dplyr::filter(on_user_roster) %>%
  dplyr::arrange(desc(value)) %>%
  dplyr::select(player_name, position, team, base_proj, adjusted_vorp,
                forward_vorp, value) %>%
  head(10) %>% print()


# ==============================================================================
# EXAMPLE 4: Optimal lineup, preseason (neutral matchup)
# ==============================================================================
# Purpose: the default lineup call. No week means preseason, so every matchup
# factor is 1 and the optimizer maximizes raw projected points across the legal
# slot structure (QB, RB, RB, WR, WR, TE, FLEX, DEF for this demo league).
#
# Key insight: this is a true full-lineup integer program. FLEX is filled with
# whichever leftover RB/WR/TE adds the most, decided jointly with every other
# slot rather than by a position-by-position sort.

lineup_pre <- optimize_lineup(demo, reconciled, vorp, def_proj)

cat("\nOptimal preseason lineup:\n")
print(lineup_pre$starters)


# ==============================================================================
# EXAMPLE 5: Optimal lineup, in-season with the matchup adjustment
# ==============================================================================
# Purpose: demonstrate the matchup layer against real data by treating 2025 week
# 6 as a stand-in live week. We pass the factors from Examples 1-2 and a week, so
# each player's projection is multiplied by his opponent matchup.
#
# Key insight: adj_proj now differs from base_proj, and matchup_factor and
# opponent are populated. A player in a great matchup can leapfrog a higher-
# projected teammate in a tough one. Compare to Example 4's neutral lineup.

lineup_wk <- optimize_lineup(demo, reconciled, vorp, def_proj,
                             week = 6L, season = 2025L,
                             dvp = dvp, def_factors = def_factors)

cat("\nMatchup-adjusted lineup (2025 week 6 stand-in):\n")
lineup_wk$starters %>%
  dplyr::select(slot, player_name, team, opponent, base_proj,
                matchup_factor, adj_proj, confidence_flag) %>%
  print()


# ==============================================================================
# EXAMPLE 6: Waiver adds, lineup mode (this-week lineup gain)
# ==============================================================================
# Purpose: lineup mode ranks available free agents by how many points they add
# to your optimal starting lineup, holding your lowest-value player as the drop.
# This is the redraft view, so we force format = "redraft", mode = "lineup".
#
# Key insight: only a free agent who out-projects your worst starter can show a
# gain. In a deep league this list is often short or empty, which is correct.

adds_lineup <- suggest_waiver_adds(demo, reconciled, vorp, def_proj,
                                   format = "redraft", mode = "lineup")

cat("\nLineup-improvement waiver adds:\n")
print(adds_lineup)


# ==============================================================================
# EXAMPLE 7: Waiver adds, stash mode (forward-looking dynasty value)
# ==============================================================================
# Purpose: stash mode skips the lineup solve and ranks available players by
# forward_vorp, a cross-position value (VORP nudged up for youth and prospect
# pedigree). This surfaces future assets a this-week check would never show.
# Dynasty sources select this automatically; here it is explicit.
#
# Key insight: forward_vorp is comparable across positions, unlike a within-
# position percentile, so a buried young WR can rank above a near-replacement
# veteran TE. Pass prospect_scores (R/28) to sharpen the pedigree signal.

adds_stash <- suggest_waiver_adds(demo, reconciled, vorp, def_proj,
                                  mode = "stash")

cat("\nStash waiver adds (forward value):\n")
print(adds_stash)

# With R/28 prospect pedigree folded in (uncomment if the file is present):
# prospects <- readr::read_csv(
#   here::here("data","season2_cache","s2_week14_final_prospect_scores.csv"),
#   show_col_types = FALSE)
# if (!"nfl_gsis_id" %in% names(prospects) && "gsis_id" %in% names(prospects)) {
#   prospects <- dplyr::rename(prospects, nfl_gsis_id = gsis_id)
# }
# adds_stash_p <- suggest_waiver_adds(demo, reconciled, vorp, def_proj,
#                                     mode = "stash", prospect_scores = prospects)
# print(adds_stash_p)


# ==============================================================================
# EXAMPLE 8: Availability gating with a manual override
# ==============================================================================
# Purpose: load_availability() infers status from Sleeper, and a manual override
# CSV (gsis_id, status, return_week) gets the final word. Here we write a tiny
# override that rules one of your starters out for the season and confirm the
# optimizer benches him.
#
# Key insight: an out_for_season player is removed from the startable set and
# returned under $unavailable, and the optimizer fills his slot from the bench.

ruled_out <- demo$user_roster_gsis[1]
ovr_path  <- tempfile(fileext = ".csv")
readr::write_csv(
  tibble::tibble(gsis_id = ruled_out, status = "out_for_season",
                 return_week = NA_integer_),
  ovr_path)

avail <- load_availability(demo, availability_path = ovr_path)
lineup_inj <- optimize_lineup(demo, reconciled, vorp, def_proj,
                              availability = avail)

cat(glue("\nRuled out (gsis {ruled_out}); held under unavailable:\n"))
lineup_inj$unavailable %>%
  dplyr::select(player_name, position, team, availability_status) %>%
  print()

cat("\nLineup with that player removed (slot refilled from bench):\n")
lineup_inj$starters %>%
  dplyr::select(slot, player_name, position, adj_proj) %>% print()


# ==============================================================================
# EXAMPLE 9: THE REAL SLEEPER WORKFLOW (commented -- needs a live league id)
# ==============================================================================
# Replace the placeholders with your Sleeper league id and roster slot. Find the
# current season's league id via get_user_leagues("your_username", 2026L). For a
# dynasty league, use the CURRENT season's instance, not a completed prior year.

# src <- build_league_source(league_id = "YOUR_LEAGUE_ID",
#                            user_roster_id = YOUR_ROSTER_ID)
# vorp_real <- compute_vorp_rankings(reconciled, src$config)
#
# # preseason lineup
# optimize_lineup(src, reconciled, vorp_real, def_proj)
#
# # in-season, matchup-adjusted
# optimize_lineup(src, reconciled, vorp_real, def_proj, week = 6L,
#                 dvp = dvp, def_factors = def_factors)
#
# # waiver adds (mode auto-selects: stash for dynasty, lineup for redraft)
# suggest_waiver_adds(src, reconciled, vorp_real, def_proj)

message("\nDone.")
