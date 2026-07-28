# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Cross-Position VORP Ranking Engine
# File: R/33_vorp_rankings.R
#
# PURPOSE
# -------
# Final layer of the team-aware projection stack. Takes R/32 reconciled
# projections and converts them into draft-usable cross-position rankings
# using Value Over Replacement Player (VORP), with league-specific
# replacement levels, boom/bust adjustments, and best-ball ceiling bonuses.
#
# WHY VORP MATTERS
# ----------------
# R/32 gives every player a corrected PPG projection, but you can't draft
# from a PPG list alone. A QB at 16 PPG might be worth less than an RB at
# 14 PPG if QB is a shallow position (every team has one good QB) and RB
# is a deep position (the dropoff after the top tier is steep). VORP
# answers: "How much better is this player than the worst startable player
# at their position in my league?"
#
# Higher VORP = bigger advantage from drafting this player versus
# whatever else you could grab at the same position.
#
# THE ITERATIVE FLEX ALGORITHM (Christian's design)
# -------------------------------------------------
# Standard fantasy leagues have FLEX slots that any RB/WR/TE can fill.
# Superflex adds a slot QB/RB/WR/TE can fill. The replacement level for
# each position depends on how many flex slots its players occupy. This
# requires an iterative computation:
#
#   1. Allocate dedicated starter slots first (top N QB, top M RB, etc.)
#   2. Allocate flex slots, each type from its own eligible pool sorted by
#      PPG: REC_FLEX (WR/TE) and WRRB_FLEX (RB/WR) first, then generic
#      FLEX (RB/WR/TE)
#   3. Allocate SUPERFLEX slots similarly, including QB
#   4. Replacement PPG at each position = PPG of the next player below
#      everyone who got a starter/flex/superflex slot
#
# Example (12-team 1QB/2RB/2WR/1TE/1FLEX league):
#   - QB starters: 12, replacement = PPG of QB13
#   - Dedicated RB slots: 24, dedicated WR: 24, dedicated TE: 12
#   - FLEX 12 spots get filled by best RB/WR/TE not already starting.
#     If FLEX ends up 8 RB + 3 WR + 1 TE, then replacement levels are:
#       RB replacement = PPG of RB33 (24 starters + 8 flex + 1)
#       WR replacement = PPG of WR28 (24 starters + 3 flex + 1)
#       TE replacement = PPG of TE14 (12 starters + 1 flex + 1)
#
# This is what makes a stud RB more valuable than a stud WR in most leagues:
# RBs eat more flex spots, which makes the RB pool effectively deeper, but
# the talent dropoff above replacement is steeper. The iterative algorithm
# captures this naturally.
#
# COMPOSITE FORMULA
# -----------------
#   vorp_base       = r32_posterior_mu - replacement_ppg_at_position
#   boom_modifier   = boom_weight * boom_probability
#   bust_modifier   = -bust_weight * bust_probability
#   ceiling_modifier = ceiling_factor * pmax(upper_80 - posterior_mu, 0)
#   adjusted_vorp   = vorp_base + boom_modifier + bust_modifier
#                                + ceiling_modifier
#   overall_rank    = rank by adjusted_vorp DESC across all positions
#
# LEAGUE FORMAT DEFAULTS
# ----------------------
# Standard:   boom_weight = 1.5, bust_weight = 2.0, ceiling_factor = 0.0
# Best ball:  boom_weight = 4.0, bust_weight = 0.5, ceiling_factor = 0.3
#
# Loss aversion baked into standard formats (bust weighted more than boom).
# Best ball inverts: ceiling matters most, floor barely matters because the
# platform auto-sets the highest-scoring lineup every week.
#
# CONFIGURATION SOURCES
# ---------------------
# Three ways to build a league_config:
#   1. build_league_config()                manual entry
#   2. build_config_from_sleeper(league_id) auto-pull from one league
#   3. build_configs_from_sleeper_user()    pulls all the user's leagues
#
# All three return the same schema. The downstream ranking engine doesn't
# care which path populated the config.
#
# OUTPUTS
# -------
#   data/season2_cache/s2_week15_vorp_rankings.rds
#   data/season2_cache/s2_week15_vorp_rankings.csv
#
# Long format: one row per (player, league) combination. Schema:
#   nfl_gsis_id              chr   gsis ID
#   player_name              chr
#   team                     chr
#   position                 chr   QB/RB/WR/TE
#   league_name              chr   from config
#   league_format            chr   "standard"/"ppr"/"best_ball"/"superflex"
#   league_teams             int
#   r32_posterior_mu         dbl   from R/32
#   r32_projection_lower_80  dbl   from R/32 (80% interval lower bound)
#   r32_projection_upper_80  dbl   from R/32 (80% interval upper bound)
#   boom_probability         dbl
#   bust_probability         dbl
#   replacement_ppg          dbl   for this position in this league
#   vorp_base                dbl   r32_posterior_mu - replacement_ppg
#   boom_modifier            dbl
#   bust_modifier            dbl
#   ceiling_modifier         dbl   non-zero only in best ball
#   adjusted_vorp            dbl
#   overall_rank             int   1 = best in this league
#   position_rank            int   within position
#   schema_tag               chr   "s2_w15_vorp_v2"
#
# SOURCE DEPENDENCIES
# -------------------
#   R/19_sleeper_api.R              connect_sleeper_league(), get_user_leagues()
#   R/32_projection_reconciliation.R reconcile_projections() output
#
# RUN
# ---
#   source(here::here("R", "33_vorp_rankings.R"))
#   reconciled <- readRDS(here::here("data", "season2_cache",
#                                    "s2_week15_reconciled_projections.rds"))
#   config <- build_league_config(
#     league_name = "Home League",
#     num_teams = 12,
#     starters = list(QB = 1, RB = 2, WR = 2, TE = 1),
#     flex = 1,
#     superflex = 0,
#     format = "ppr"
#   )
#   rankings <- compute_vorp_rankings(reconciled, config)
#
# Author: Christian K. LeBlanc
# Version: 1.0
# ==============================================================================

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(here)
library(glue)

source(here::here("R", "19_sleeper_api.R"))
source(here::here("R", "32_projection_reconciliation.R"))

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

SEASON_VORP <- 2026L

# Positions ranked (DEF/ST handled separately downstream)
VORP_POSITIONS <- c("QB", "RB", "WR", "TE")

# Positions eligible for standard FLEX
FLEX_ELIGIBLE <- c("RB", "WR", "TE")

# Sleeper's restricted flex variants each have their own eligibility:
# REC_FLEX = WR/TE only, WRRB_FLEX = RB/WR only. Tracked separately from
# generic FLEX so replacement levels reflect the actual eligible pools.
REC_FLEX_ELIGIBLE  <- c("WR", "TE")
WRRB_FLEX_ELIGIBLE <- c("RB", "WR")

# Positions eligible for SUPER_FLEX
SUPERFLEX_ELIGIBLE <- c("QB", "RB", "WR", "TE")

# League format defaults (boom/bust/ceiling weights)
LEAGUE_FORMAT_DEFAULTS <- list(
  "standard" = list(
    boom_weight    = 1.5,
    bust_weight    = 2.0,
    ceiling_factor = 0.0
  ),
  "half_ppr" = list(
    boom_weight    = 1.5,
    bust_weight    = 2.0,
    ceiling_factor = 0.0
  ),
  "ppr" = list(
    boom_weight    = 1.5,
    bust_weight    = 2.0,
    ceiling_factor = 0.0
  ),
  "best_ball" = list(
    boom_weight    = 4.0,
    bust_weight    = 0.5,
    ceiling_factor = 0.3
  )
)

# Paths
R32_RECON_RDS_VORP <- here::here(
  "data", "season2_cache", "s2_week15_reconciled_projections.rds"
)
OUTPUT_RDS_PATH_VORP <- here::here(
  "data", "season2_cache", "s2_week15_vorp_rankings.rds"
)
OUTPUT_CSV_PATH_VORP <- here::here(
  "data", "season2_cache", "s2_week15_vorp_rankings.csv"
)

SCHEMA_TAG_VORP <- "s2_w15_vorp_v2"

# ------------------------------------------------------------------------------
# NSE DECLARATIONS
# ------------------------------------------------------------------------------

utils::globalVariables(c(
  "league_name", "league_format", "league_teams",
  "r32_projection_upper_80", "r32_projection_lower_80",
  "replacement_ppg", "vorp_base", "boom_modifier", "bust_modifier",
  "ceiling_modifier", "adjusted_vorp", "overall_rank", "position_rank",
  "pos_rank", "role", "n_used",
  "FLEX", "SUPER_FLEX", "BN", "IR", "TAXI", "K", "DEF",
  "roster_position"
))

# ==============================================================================
# CONFIG BUILDERS
# ==============================================================================

# ------------------------------------------------------------------------------
# build_league_config
# ------------------------------------------------------------------------------

#' Manually build a league configuration
#'
#' Use for non-Sleeper leagues (best ball platforms like Underdog or
#' DraftKings, custom leagues, hypothetical scenarios).
#'
#' @param league_name Character. Display name for the league.
#' @param num_teams Integer. Number of teams (default 12).
#' @param starters Named list. Number of dedicated starter slots per
#'   position: list(QB = 1, RB = 2, WR = 2, TE = 1). DEF/K/etc. ignored.
#' @param flex Integer. Number of FLEX slots (RB/WR/TE eligible). Default 1.
#' @param rec_flex Integer. Number of REC_FLEX slots (WR/TE eligible).
#'   Default 0.
#' @param wrrb_flex Integer. Number of WRRB_FLEX slots (RB/WR eligible).
#'   Default 0.
#' @param superflex Integer. Number of SUPER_FLEX slots (QB/RB/WR/TE
#'   eligible). Default 0.
#' @param format Character. One of "standard", "half_ppr", "ppr",
#'   "best_ball". Drives boom/bust/ceiling weight defaults.
#' @param boom_weight Numeric. Override the format default if non-NULL.
#' @param bust_weight Numeric. Override the format default if non-NULL.
#' @param ceiling_factor Numeric. Override the format default if non-NULL.
#' @return A league_config list.
#' @export
build_league_config <- function(
    league_name,
    num_teams = 12L,
    starters = list(QB = 1L, RB = 2L, WR = 2L, TE = 1L),
    flex = 1L,
    rec_flex = 0L,
    wrrb_flex = 0L,
    superflex = 0L,
    format = "ppr",
    boom_weight = NULL,
    bust_weight = NULL,
    ceiling_factor = NULL) {

  # Validate format
  if (!format %in% names(LEAGUE_FORMAT_DEFAULTS)) {
    stop(glue("Unknown format '{format}'. Valid: ",
              "{paste(names(LEAGUE_FORMAT_DEFAULTS), collapse = ', ')}"))
  }

  # Ensure all positions present with default 0
  full_starters <- list(QB = 0L, RB = 0L, WR = 0L, TE = 0L)
  for (pos in names(starters)) {
    if (pos %in% names(full_starters)) {
      full_starters[[pos]] <- as.integer(starters[[pos]])
    }
  }

  # Apply format defaults, override if user supplied
  defaults <- LEAGUE_FORMAT_DEFAULTS[[format]]
  bw <- boom_weight %||% defaults$boom_weight
  busw <- bust_weight %||% defaults$bust_weight
  cf <- ceiling_factor %||% defaults$ceiling_factor

  config <- list(
    league_name    = as.character(league_name),
    num_teams      = as.integer(num_teams),
    starters       = full_starters,
    flex           = as.integer(flex),
    rec_flex       = as.integer(rec_flex),
    wrrb_flex      = as.integer(wrrb_flex),
    superflex      = as.integer(superflex),
    format         = format,
    boom_weight    = bw,
    bust_weight    = busw,
    ceiling_factor = cf,
    source         = "manual"
  )

  class(config) <- c("league_config", "list")
  config
}

# ------------------------------------------------------------------------------
# .parse_roster_positions
# ------------------------------------------------------------------------------

#' Parse Sleeper's roster_positions vector into starter counts
#'
#' Sleeper returns roster_positions like:
#'   c("QB","RB","RB","WR","WR","TE","FLEX","BN","BN","BN","BN","IR")
#'
#' This counts starter positions and flex/superflex slots. Ignores BN/IR/
#' TAXI/K/DEF.
#'
#' @param roster_positions Character vector from Sleeper league meta.
#' @return List: starters, flex, rec_flex, wrrb_flex, superflex counts.
#' @keywords internal
.parse_roster_positions <- function(roster_positions) {

  counts <- table(roster_positions)

  .count_slot <- function(slot) {
    n <- as.integer(counts[slot] %||% 0L)
    if (is.na(n)) 0L else n
  }

  starters <- list(
    QB = .count_slot("QB"),
    RB = .count_slot("RB"),
    WR = .count_slot("WR"),
    TE = .count_slot("TE")
  )

  # Each Sleeper flex variant is tracked with its own count because each has
  # its own eligibility set (FLEX = RB/WR/TE, REC_FLEX = WR/TE,
  # WRRB_FLEX = RB/WR). Pooling them into one generic flex would overstate
  # the eligible pool for the restricted variants.
  list(
    starters  = starters,
    flex      = .count_slot("FLEX"),
    rec_flex  = .count_slot("REC_FLEX"),
    wrrb_flex = .count_slot("WRRB_FLEX"),
    superflex = .count_slot("SUPER_FLEX")
  )
}

# ------------------------------------------------------------------------------
# .detect_scoring_format
# ------------------------------------------------------------------------------

#' Detect scoring format from Sleeper scoring_settings
#'
#' Looks at the receptions value: 1.0 = PPR, 0.5 = half PPR, 0 = standard.
#' Best ball is detected from best_ball flag in settings (not always
#' present); defaults to PPR-equivalent if missing.
#'
#' @param scoring_settings List from Sleeper.
#' @param settings List from Sleeper (top-level).
#' @return Character. One of "standard", "half_ppr", "ppr", "best_ball".
#' @keywords internal
.detect_scoring_format <- function(scoring_settings, settings = list()) {

  # Sleeper best_ball flag (when present)
  if (isTRUE(as.logical(settings$best_ball %||% FALSE))) {
    return("best_ball")
  }
  if (!is.null(settings$type) && grepl("best.?ball",
                                          tolower(as.character(settings$type)))) {
    return("best_ball")
  }

  rec_val <- as.numeric(scoring_settings[["rec"]] %||% 0)

  if (rec_val >= 0.9) {
    "ppr"
  } else if (rec_val >= 0.4) {
    "half_ppr"
  } else {
    "standard"
  }
}

# ------------------------------------------------------------------------------
# build_config_from_sleeper
# ------------------------------------------------------------------------------

#' Build a league config by fetching from Sleeper API
#'
#' Uses R/19's connect_sleeper_league() to pull league metadata, then
#' parses roster_positions and scoring_settings into the standard config
#' schema.
#'
#' @param league_id Character. Sleeper league ID.
#' @param boom_weight Numeric. Optional override.
#' @param bust_weight Numeric. Optional override.
#' @param ceiling_factor Numeric. Optional override.
#' @return A league_config list.
#' @export
build_config_from_sleeper <- function(
    league_id,
    boom_weight = NULL,
    bust_weight = NULL,
    ceiling_factor = NULL) {

  meta <- connect_sleeper_league(league_id)
  if (is.null(meta)) {
    stop(glue("Could not connect to Sleeper league {league_id}"))
  }

  parsed <- .parse_roster_positions(meta$roster_positions)
  format_detected <- .detect_scoring_format(meta$scoring_settings,
                                             meta$settings)

  build_league_config(
    league_name    = meta$name,
    num_teams      = meta$total_rosters,
    starters       = parsed$starters,
    flex           = parsed$flex,
    rec_flex       = parsed$rec_flex,
    wrrb_flex      = parsed$wrrb_flex,
    superflex      = parsed$superflex,
    format         = format_detected,
    boom_weight    = boom_weight,
    bust_weight    = bust_weight,
    ceiling_factor = ceiling_factor
  ) -> config

  config$source     <- "sleeper"
  config$league_id  <- as.character(league_id)
  config
}

# ------------------------------------------------------------------------------
# build_configs_from_sleeper_user
# ------------------------------------------------------------------------------

#' Build league configs for every league a Sleeper user belongs to
#'
#' Uses R/19's get_user_leagues() to enumerate the user's leagues, then
#' calls build_config_from_sleeper() for each. Returns a named list of
#' league_config objects.
#'
#' @param username Character. Sleeper username.
#' @param season Integer. NFL season (default SEASON_VORP).
#' @return Named list of league_config objects, keyed by league_name.
#' @export
build_configs_from_sleeper_user <- function(username,
                                              season = SEASON_VORP) {

  leagues <- get_user_leagues(username, season = season)
  if (is.null(leagues) || nrow(leagues) == 0L) {
    message(glue("No leagues found for {username} in season {season}"))
    return(list())
  }

  configs <- purrr::map(leagues$league_id, function(lid) {
    tryCatch(
      build_config_from_sleeper(lid),
      error = function(e) {
        message(glue("  Skipping league {lid}: {e$message}"))
        NULL
      }
    )
  })

  configs <- purrr::compact(configs)
  names(configs) <- purrr::map_chr(configs, "league_name")

  message(glue("\n  Built {length(configs)} league config(s) for {username}"))
  configs
}

# ==============================================================================
# VORP COMPUTATION
# ==============================================================================

# ------------------------------------------------------------------------------
# .compute_replacement_levels
# ------------------------------------------------------------------------------

#' Iteratively determine replacement PPG for each position
#'
#' Implements the iterative flex algorithm. Allocates dedicated starter
#' slots first, then each flex type from its own eligible pool (REC_FLEX =
#' WR/TE, WRRB_FLEX = RB/WR, then generic FLEX = RB/WR/TE), then SUPER_FLEX
#' from remaining QB/RB/WR/TE. Replacement PPG at each position = PPG of the
#' next-best player below everyone allocated.
#'
#' @param projections Tibble. Must contain nfl_gsis_id, position, and
#'   r32_posterior_mu.
#' @param config League_config list.
#' @return Named list with two elements:
#'   replacement_ppg: named numeric (QB, RB, WR, TE)
#'   role_assignments: tibble with nfl_gsis_id, role
#'   (starter/flex/rec_flex/wrrb_flex/superflex/bench)
#' @keywords internal
.compute_replacement_levels <- function(projections, config) {

  proj <- projections %>%
    dplyr::filter(.data$position %in% VORP_POSITIONS,
                   !is.na(.data$r32_posterior_mu)) %>%
    dplyr::group_by(.data$position) %>%
    dplyr::arrange(dplyr::desc(.data$r32_posterior_mu), .by_group = TRUE) %>%
    dplyr::mutate(pos_rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(role = "bench")

  # Step 1: dedicated starters per position
  for (pos in VORP_POSITIONS) {
    n_starters <- config$starters[[pos]] * config$num_teams
    if (n_starters > 0L) {
      proj$role[proj$position == pos & proj$pos_rank <= n_starters] <-
        "starter"
    }
  }

  # Step 2: flex slots. Each flex type has its own eligibility set and is
  # allocated from the best remaining eligible players. Restricted variants
  # (REC_FLEX, WRRB_FLEX -- 2-position pools) go first so the generic FLEX
  # (3-position pool) absorbs whoever the narrower slots could not take.
  flex_specs <- list(
    list(role = "rec_flex",  eligible = REC_FLEX_ELIGIBLE,
         n = as.integer(config$rec_flex %||% 0L)),
    list(role = "wrrb_flex", eligible = WRRB_FLEX_ELIGIBLE,
         n = as.integer(config$wrrb_flex %||% 0L)),
    list(role = "flex",      eligible = FLEX_ELIGIBLE,
         n = as.integer(config$flex %||% 0L))
  )

  for (spec in flex_specs) {
    n_slot_total <- spec$n * config$num_teams
    if (n_slot_total > 0L) {
      slot_candidates <- proj %>%
        dplyr::filter(.data$position %in% spec$eligible,
                       .data$role == "bench") %>%
        dplyr::arrange(dplyr::desc(.data$r32_posterior_mu)) %>%
        dplyr::slice_head(n = n_slot_total)

      proj$role[proj$nfl_gsis_id %in% slot_candidates$nfl_gsis_id] <-
        spec$role
    }
  }

  # Step 3: SUPER_FLEX -- best QB/RB/WR/TE not yet starting/flexing
  n_sflex_total <- config$superflex * config$num_teams
  if (n_sflex_total > 0L) {
    sflex_candidates <- proj %>%
      dplyr::filter(.data$position %in% SUPERFLEX_ELIGIBLE,
                     .data$role == "bench") %>%
      dplyr::arrange(dplyr::desc(.data$r32_posterior_mu)) %>%
      dplyr::slice_head(n = n_sflex_total)

    proj$role[proj$nfl_gsis_id %in% sflex_candidates$nfl_gsis_id] <-
      "superflex"
  }

  # Step 4: replacement PPG per position = PPG of first bench player
  replacement_ppg <- purrr::map_dbl(VORP_POSITIONS, function(pos) {
    pos_bench <- proj %>%
      dplyr::filter(.data$position == pos, .data$role == "bench") %>%
      dplyr::arrange(dplyr::desc(.data$r32_posterior_mu))

    if (nrow(pos_bench) == 0L) {
      # All players at this position are used -- use last used player's PPG
      pos_used <- proj %>%
        dplyr::filter(.data$position == pos, .data$role != "bench")
      if (nrow(pos_used) == 0L) return(0)
      min(pos_used$r32_posterior_mu, na.rm = TRUE)
    } else {
      pos_bench$r32_posterior_mu[1]
    }
  })
  names(replacement_ppg) <- VORP_POSITIONS

  list(
    replacement_ppg  = replacement_ppg,
    role_assignments = proj %>% dplyr::select(nfl_gsis_id, role, pos_rank)
  )
}

# ------------------------------------------------------------------------------
# .compute_player_vorp
# ------------------------------------------------------------------------------

#' Compute VORP and adjusted VORP per player
#'
#' Joins replacement levels to projections, computes vorp_base, applies
#' boom/bust/ceiling modifiers per the config.
#'
#' @param projections Tibble with r32_posterior_mu, position,
#'   boom_probability, bust_probability, r32_projection_upper_80.
#' @param replacement_levels Named numeric from .compute_replacement_levels().
#' @param config League_config.
#' @return Tibble with vorp_base, modifiers, adjusted_vorp added.
#' @keywords internal
.compute_player_vorp <- function(projections, replacement_levels, config) {

  rep_df <- tibble::tibble(
    position        = names(replacement_levels),
    replacement_ppg = as.numeric(replacement_levels)
  )

  projections %>%
    dplyr::filter(.data$position %in% VORP_POSITIONS,
                   !is.na(.data$r32_posterior_mu)) %>%
    dplyr::left_join(rep_df, by = "position") %>%
    dplyr::mutate(
      vorp_base = .data$r32_posterior_mu - .data$replacement_ppg,

      boom_modifier = config$boom_weight *
                        dplyr::coalesce(.data$boom_probability, 0),

      bust_modifier = -config$bust_weight *
                        dplyr::coalesce(.data$bust_probability, 0),

      ceiling_modifier = config$ceiling_factor *
                           pmax(
                             dplyr::coalesce(.data$r32_projection_upper_80, 0) -
                               .data$r32_posterior_mu,
                             0
                           ),

      adjusted_vorp = .data$vorp_base + .data$boom_modifier +
                        .data$bust_modifier + .data$ceiling_modifier
    )
}

# ==============================================================================
# PUBLIC ENTRY POINTS
# ==============================================================================

# ------------------------------------------------------------------------------
# compute_vorp_rankings
# ------------------------------------------------------------------------------

#' Compute VORP rankings for a single league configuration
#'
#' Computes replacement levels iteratively, then VORP per player, then
#' ranks by adjusted_vorp.
#'
#' @param projections Tibble. R/32 reconciled projections. Required columns:
#'   nfl_gsis_id, player_name, team, position, r32_posterior_mu,
#'   boom_probability, bust_probability, r32_projection_upper_80.
#' @param config league_config list.
#' @return Tibble with one row per player, sorted by overall_rank.
#' @export
compute_vorp_rankings <- function(projections, config) {

  if (!inherits(config, "league_config")) {
    stop("config must be a league_config object (use build_league_config()).")
  }

  # Compute replacement levels
  rep_result <- .compute_replacement_levels(projections, config)

  # Compute per-player VORP
  vorp_df <- .compute_player_vorp(
    projections        = projections,
    replacement_levels = rep_result$replacement_ppg,
    config             = config
  )

  # Rank
  vorp_df <- vorp_df %>%
    dplyr::arrange(dplyr::desc(.data$adjusted_vorp)) %>%
    dplyr::mutate(overall_rank = dplyr::row_number()) %>%
    dplyr::group_by(.data$position) %>%
    dplyr::arrange(dplyr::desc(.data$adjusted_vorp),
                    .by_group = TRUE) %>%
    dplyr::mutate(position_rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$overall_rank) %>%
    dplyr::mutate(
      league_name   = config$league_name,
      league_format = config$format,
      league_teams  = config$num_teams,
      schema_tag    = SCHEMA_TAG_VORP
    )

  # Select final schema columns
  vorp_df %>%
    dplyr::select(
      nfl_gsis_id, player_name, team, position,
      league_name, league_format, league_teams,
      r32_posterior_mu, r32_projection_lower_80, r32_projection_upper_80,
      boom_probability, bust_probability,
      replacement_ppg, vorp_base,
      boom_modifier, bust_modifier, ceiling_modifier,
      adjusted_vorp, overall_rank, position_rank,
      schema_tag
    )
}

# ------------------------------------------------------------------------------
# compute_multi_league_rankings
# ------------------------------------------------------------------------------

#' Compute VORP rankings across multiple league configurations
#'
#' Runs compute_vorp_rankings() for each config and stacks the results
#' into a long-format tibble. Easy to filter by league_name downstream.
#'
#' @param projections Tibble. R/32 reconciled projections.
#' @param configs Named list of league_config objects, or a single config.
#' @param save_output Logical. Write RDS + CSV outputs.
#' @return Long-format tibble with one row per (player, league).
#' @export
compute_multi_league_rankings <- function(projections,
                                            configs,
                                            save_output = TRUE) {

  # Normalize to list
  if (inherits(configs, "league_config")) {
    configs <- list(configs)
  }

  if (length(configs) == 0L) {
    stop("No configs supplied.")
  }

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/33: Computing VORP rankings for {length(configs)} league(s)"))
  message(glue("{strrep('=', 70)}"))

  all_rankings <- purrr::map_dfr(configs, function(cfg) {
    message(glue("\n  League: {cfg$league_name} ({cfg$format}, ",
                 "{cfg$num_teams}T, ",
                 "QB:{cfg$starters$QB}/RB:{cfg$starters$RB}/",
                 "WR:{cfg$starters$WR}/TE:{cfg$starters$TE}/",
                 "FLEX:{cfg$flex}/RECF:{cfg$rec_flex %||% 0L}/",
                 "WRRBF:{cfg$wrrb_flex %||% 0L}/SF:{cfg$superflex})"))

    rankings <- compute_vorp_rankings(projections, cfg)

    # Diagnostic print
    rep_levels <- rankings %>%
      dplyr::group_by(.data$position) %>%
      dplyr::summarise(rep = dplyr::first(.data$replacement_ppg),
                        .groups = "drop")
    rep_str <- paste(
      glue("{rep_levels$position}: {format(round(rep_levels$rep, 1), nsmall=1)}"),
      collapse = ", "
    )
    message(glue("    Replacement PPG -- {rep_str}"))

    rankings
  })

  # Save outputs
  if (save_output) {
    dir.create(dirname(OUTPUT_RDS_PATH_VORP), recursive = TRUE,
                showWarnings = FALSE)
    saveRDS(all_rankings, OUTPUT_RDS_PATH_VORP)
    readr::write_csv(all_rankings, OUTPUT_CSV_PATH_VORP)
    message(glue("\n  Saved: {OUTPUT_RDS_PATH_VORP}"))
    message(glue("  Saved: {OUTPUT_CSV_PATH_VORP}"))
  }

  # KEY INSIGHTS
  n_leagues <- dplyr::n_distinct(all_rankings$league_name)
  n_players <- dplyr::n_distinct(all_rankings$nfl_gsis_id)
  n_rows    <- nrow(all_rankings)

  message(glue("\n{strrep('=', 70)}"))
  message("KEY INSIGHTS")
  message(glue("{strrep('=', 70)}"))
  message(glue("  Leagues processed:        {n_leagues}"))
  message(glue("  Unique players ranked:    {n_players}"))
  message(glue("  Total (player, league):   {n_rows}"))

  # Per-league #1 overall
  top_per_league <- all_rankings %>%
    dplyr::filter(.data$overall_rank == 1L) %>%
    dplyr::select(league_name, league_format, player_name, team, position,
                   adjusted_vorp)
  if (nrow(top_per_league) > 0L) {
    message("\n  #1 overall pick per league:")
    for (i in seq_len(nrow(top_per_league))) {
      r <- top_per_league[i, ]
      message(glue("    {r$league_name}: {r$player_name} ({r$team} {r$position}) ",
                   "VORP {format(round(r$adjusted_vorp, 1), nsmall = 1)}"))
    }
  }
  message(glue("{strrep('=', 70)}\n"))

  all_rankings
}

# ------------------------------------------------------------------------------
# print method for league_config
# ------------------------------------------------------------------------------

#' @export
print.league_config <- function(x, ...) {
  cat(glue("League Configuration: {x$league_name}\n"))
  cat(glue("  Source:         {x$source}\n"))
  cat(glue("  Teams:          {x$num_teams}\n"))
  cat(glue("  Format:         {x$format}\n"))
  cat(glue("  Starters:       QB={x$starters$QB} RB={x$starters$RB} ",
           "WR={x$starters$WR} TE={x$starters$TE}\n"))
  cat(glue("  FLEX slots:     {x$flex}\n"))
  cat(glue("  REC_FLEX:       {x$rec_flex %||% 0L}\n"))
  cat(glue("  WRRB_FLEX:      {x$wrrb_flex %||% 0L}\n"))
  cat(glue("  SUPER_FLEX:     {x$superflex}\n"))
  cat(glue("  Boom weight:    {x$boom_weight}\n"))
  cat(glue("  Bust weight:    {x$bust_weight}\n"))
  cat(glue("  Ceiling factor: {x$ceiling_factor}\n"))
  invisible(x)
}
