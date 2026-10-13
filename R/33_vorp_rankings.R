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
# ALL WEIGHTS ARE 0.00.  [Wave A2 R6, 2026-08-07]
#
# The former defaults were:
#   Standard:   boom_weight = 1.5, bust_weight = 2.0, ceiling_factor = 0.0
#   Best ball:  boom_weight = 4.0, bust_weight = 0.5, ceiling_factor = 0.3
# Five non-zero constants, none of them fitted or gated out of sample. The
# stated rationale was loss aversion in standard formats and ceiling-seeking in
# best ball. Both are plausible claims about football; neither was ever tested,
# and the ceiling term is not even a separate signal: under a symmetric
# predictive distribution (upper_80 - mu) is a fixed multiple of sigma, so
# ceiling_modifier is a rescaled sigma presented as new information.
#
# With every weight at 0.00, adjusted_vorp == vorp_base exactly. The modifier
# columns still emit, as zeros, so the neutralization is auditable rather than
# invisible. See LEAGUE_FORMAT_DEFAULTS for the measured effect and the
# conditions for reinstating a fitted weight set.
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
#
# ALL FIVE WEIGHTS NEUTRALIZED TO 0.00  [Wave A2 R6]
#
# The three modifiers below add a quantity to vorp_base that no gate ever
# supported. Each of the five constants (boom 1.5/4.0, bust 2.0/0.5, ceiling
# 0.3) was chosen by assertion, not fitted or validated out of sample, and the
# ceiling term is worse than merely unsupported: ceiling_factor * (upper_80 -
# mu) is a fixed multiple of sigma under a symmetric predictive distribution,
# so it carries NO information beyond sigma while presenting itself as a
# separate signal. It is a rescaled sigma wearing the name of a ceiling.
#
# Measured effect before neutralization, on a 12-team redraft board: the extra
# term ranged -0.4 to +1.45 PPG, mean absolute rank shift 3.33 places, max 13.
# That is enough to reorder real draft decisions on the strength of five
# constants nobody had tested.
#
# With every weight at 0.00, adjusted_vorp is arithmetically identical to
# vorp_base, and vorp_base is r32_posterior_mu minus positional replacement:
# a quantity that means something. The machinery is retained so a fitted
# replacement can be dropped in behind the same interface once the best-ball
# simulation supplies weights EARNED from simulated lineup value rather than
# assumed. Do not restore a non-zero value without an out-of-sample gate.
# QB availability weighting: see compute_vorp_rankings() for the full argument.
# TRUE multiplies backup QB mu by qb_discount_factor before ranking, so a
# high-PPG backup does not outrank a starter on a draft board.
VORP_APPLY_QB_AVAILABILITY <- TRUE

LEAGUE_FORMAT_DEFAULTS <- list(
  "standard" = list(
    boom_weight    = 0.0,
    bust_weight    = 0.0,
    ceiling_factor = 0.0
  ),
  "half_ppr" = list(
    boom_weight    = 0.0,
    bust_weight    = 0.0,
    ceiling_factor = 0.0
  ),
  "ppr" = list(
    boom_weight    = 0.0,
    bust_weight    = 0.0,
    ceiling_factor = 0.0
  ),
  "best_ball" = list(
    boom_weight    = 0.0,
    bust_weight    = 0.0,
    ceiling_factor = 0.0
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
  "qb_discount_factor", "vorp_availability", "r32_posterior_mu_ppg",
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
#' @param projections Tibble. Must contain player_uid, nfl_gsis_id, position,
#'   and r32_posterior_mu. player_uid is the identity key; nfl_gsis_id is NA
#'   for the draftable rookie cohort and must not be used to identify rows.
#' @param config League_config list.
#' @return Named list with two elements:
#'   replacement_ppg: named numeric (QB, RB, WR, TE)
#'   role_assignments: tibble with player_uid, nfl_gsis_id, role
#'   (starter/flex/rec_flex/wrrb_flex/superflex/bench)
#' @keywords internal
.compute_replacement_levels <- function(projections, config) {

  # WAVE A2 IDENTITY FIX.
  #
  # Role assignment below previously matched rows with
  #   proj$role[proj$nfl_gsis_id %in% candidates$nfl_gsis_id] <- ...
  # nfl_gsis_id is NA for the draftable rookie cohort, and in R
  # NA %in% c(NA) is TRUE. So if a single id-less rookie earned a flex slot,
  # EVERY id-less rookie was flagged into that slot and removed from the bench
  # pool. Step 4 reads replacement_ppg off the first bench player, and
  # vorp_base = r32_posterior_mu - replacement_ppg, so the corruption did not
  # stay with the rookies -- it moved the replacement level and therefore
  # every VORP number at that position.
  #
  # The fix does not swap one key for another. It assigns by row position via
  # .vorp_row_id, which is correct no matter what the identity column holds.
  # player_uid is still required and validated, because it is what makes the
  # id-less cohort addressable at all downstream.
  if (!"player_uid" %in% names(projections)) {
    stop(
      ".compute_replacement_levels(): projections carry no player_uid. ",
      "Regenerate R/29 and R/32 -- nfl_gsis_id is NA for draftable rookies ",
      "and cannot serve as the identity key.",
      call. = FALSE
    )
  }

  proj <- projections %>%
    dplyr::filter(.data$position %in% VORP_POSITIONS,
                   !is.na(.data$r32_posterior_mu)) %>%
    dplyr::mutate(.vorp_row_id = dplyr::row_number()) %>%
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

      proj$role[proj$.vorp_row_id %in% slot_candidates$.vorp_row_id] <-
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

    proj$role[proj$.vorp_row_id %in% sflex_candidates$.vorp_row_id] <-
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
    role_assignments = proj %>%
      dplyr::select(player_uid, nfl_gsis_id, role, pos_rank)
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

      # With LEAGUE_FORMAT_DEFAULTS all at 0.00 every modifier is exactly 0 and
      # this reduces to vorp_base. Kept as a sum rather than collapsed so the
      # modifier columns still emit (all zeros, auditable) and so a fitted
      # weight set can be reinstated without touching this line.
      adjusted_vorp = .data$vorp_base + .data$boom_modifier +
                        .data$bust_modifier + .data$ceiling_modifier
    )
}

# ==============================================================================
# BEST-BALL SIMULATION  [2026-07-30, Wave A2 R6]
#
# WHAT THIS REPLACES
# The composite formula above prices upside with three hand-set numbers:
#   boom_modifier    = boom_weight   * boom_probability     (4.0 in best ball)
#   bust_modifier    = -bust_weight  * bust_probability     (0.5)
#   ceiling_modifier = ceiling_factor * (upper_80 - mu)     (0.3)
# None of the five weights across the two formats ever had OOS gate evidence,
# and R/49 showed ceiling_modifier was exactly 0.3 * 1.282 * sigma, carrying no
# information beyond sigma itself.
#
# They exist only because the board holds a POINT estimate and upside had to be
# stapled on afterwards. Given a weekly predictive distribution there is nothing
# left for them to do: best-ball value is what the lineup actually scores, and
# the lineup keeps good weeks and discards bad ones by itself.
#
# WHY BEST BALL PAYS FOR VARIANCE
# No in-season management means the optimal lineup is chosen after the results
# are known. A player's bad weeks never enter the lineup, and his good weeks
# enter in full. His downside is truncated and his upside is not, so his value
# is an option on his weeks rather than his average. Two players with identical
# projections are not equally valuable if one delivers 12 every week and the
# other alternates 3 and 21: the 21s are kept and the 3s are thrown away.
#
# THE MARGINAL-VALUE IDENTITY
# Adding one player of position p, scoring x, to a roster changes the weekly
# lineup total by exactly
#     max(0, x - bar_p)
# where bar_p is the score he must beat. For a flex-eligible position,
#     bar_p = min(A[s_p], F)
# with A[s_p] the s_p-th best score at p on the baseline roster and F the best
# remaining flex-eligible score. Derivation: if x clears A[s_p] he takes a
# position slot and displaces A[s_p], who then competes for flex, so the flex
# gains max(0, A[s_p] - F); summing gives x - F when A[s_p] >= F and
# x - A[s_p] otherwise, which is x - min(A[s_p], F) in both branches. For a
# non-flex position, bar_p = A[s_p].
#
# That identity is what makes this tractable: the baseline roster is simulated
# ONCE, and every player is then priced against the same bar rather than
# re-optimising a lineup 586 times per simulated week.
#
# TWO-LEVEL DRAW  [2026-08-03]
# WHAT WAS WRONG. Every week used to be drawn as
#     week ~ shape(mu, sigma_week),  sigma_week = sqrt(tau^2 + omega^2)
# seventeen independent times. tau and omega are not the same kind of quantity
# and cannot be consumed the same way:
#   tau   is uncertainty about the player's SEASON MEAN. It is settled once.
#         Whether a first-year starter is a 12-point player or a 22-point
#         player is decided for the year, not re-decided every Sunday.
#   omega is WEEK-TO-WEEK dispersion around whatever that mean turns out to be.
#         It genuinely does resample every week.
# Replicating sigma_week across the season and drawing independently gave a
# player seventeen separate chances to be his own optimistic case. Best ball
# keeps the maximum, so uncertainty about the LEVEL was harvested as weekly
# CEILING. The players the model knew least about got the largest bonus.
#
# MEASURED CONSEQUENCE (2026-08-03, DK best ball board, 586 players):
#   Malik Willis    mu 17.31  tau 6.944  sigma_week 11.69  ranked QB9
#   Deshaun Watson  mu 17.05  tau 6.944  sigma_week 11.94  ranked QB10
#   Kyler Murray    mu 19.76  tau 1.084  sigma_week  6.05  ranked QB12
#   Patrick Mahomes mu 18.93  tau 0.959  sigma_week  6.77  ranked QB15
# Willis outranked Murray while projecting 2.45 PPG lower. The entire gap was
# tau. Both flagged players are starters with no usable NFL history, so their
# 6.944 is the QB translation RMSE and is an HONEST number. It was being spent
# in the wrong place.
#
# TWO CORRECTIONS TO THE PARAGRAPH ABOVE  [2026-08-10]
#
# 1. ATTRIBUTION. 6.944 does NOT come from .load_translation_sigmas(). That
#    loader is retired (R/29:3562) and returns 6.667. The live value comes from
#    .compute_translation_sigmas_scored(), which recomputes the RMSE under the
#    TARGET SCORING rather than reading a fixed reference-scoring number. 6.944
#    is that function's output under DK best ball. Verified live:
#      legacy 6.667 | scoring-aware PPR 6.663 | scoring-aware DK 6.944
#    QB is the only position where the two paths differ materially; RB, WR and
#    TE agree to three decimals. The number was always right; the citation was
#    not.
#
# 2. THE PEER RANGE IS SUPERSEDED. Contemporary notes compared 6.944 against a
#    "measured-QB range of 0.96 to 2.58". That range was measured BEFORE the
#    change_sigma term was added to the history arm on 2026-07-30 (R/29:2155).
#    Without it, a long career drove tau toward zero, which R/49 measured as
#    3.7x overconfident. On the same board rebuilt after that fix:
#      measured QB median tau 2.337 -> 3.768
#      Willis / Watson         6.944 -> 6.944  (no history arm, unchanged)
#      ratio                    2.97x -> 1.84x
#    The gap is roughly 40 percent smaller than the older figures imply. Do not
#    quote 0.96 to 2.58 as a current benchmark.
#
# The DEFECT this block documents is real and the fix is correct. Only these
# two supporting details were wrong.
#
# WHAT IT IS NOW
#     theta ~ Normal(mu, tau)        ONCE per simulated season
#     week  ~ shape(theta, omega)    weeks times, around that season's theta
# An uncertain player still earns real option value: some simulated seasons
# draw him high and in those seasons he is excellent. He no longer collects
# seventeen independent lottery tickets inside one season.
#
# WHY LEVEL 1 IS NORMAL AND NOT THE R/50 SHAPE
# tau is constructed in R/29 .blend_prior_components() as a precision-weighted
# Gaussian standard error. Normal is the distribution it already is; nothing
# new is assumed. The R/50 shape table was fitted on standardized WEEKLY
# residuals and does not transfer to a season mean. Applying it at level 1
# would be using a fitted object outside the population it was fitted on.
# A right-skewed season-level form belongs to queue item R5, once there is
# evidence for one.
#
# SCOPE. Only the best-ball SIMULATION changes. r32_sigma_week itself is
# untouched and remains correct for what it is: the marginal SD of one future
# week in isolation. R/32's boom_probability_week / bust_probability_week are
# single-week marginal questions and stay on sqrt(tau^2 + omega^2).
#
# LIMITS, stated here rather than discovered later
#  - The baseline roster is replacement-level players on unrelated NFL teams,
#    so a candidate's correlation with the bar is ~0. That is right for a
#    marginal-value board and WRONG for any stacking question, which is what
#    simulate_roster_season() below is for.
#  - Shape calibration is weakest in the low-projection tercile (R/50 measured
#    dev 3.0-4.5 against 1.4-2.3 elsewhere), which is exactly where cheap
#    high-variance players live. Their values carry more error than the
#    expensive ones.
#  - Availability is not modelled. omega is measured over games PLAYED, so a
#    missed week is absent rather than zero.
#  - theta is NOT floored at zero. The floor stays where it already was, on
#    the weekly score, exactly as predictive_draw() applies it. Flooring the
#    season mean would bias every uncertain player upward and break the
#    marginal-variance identity.
#  - Level 1 is drawn independently across players, including team-mates, in
#    simulate_roster_season(). The R/50 copula was calibrated on weekly
#    residuals; there is no evidence for a season-level correlation and
#    borrowing the weekly one would repeat the mistake this block fixes.
# ==============================================================================

# 17-game regular season, 2021 onward.
BB_SEASON_WEEKS <- 17L

# Simulated seasons for the marginal-value board. The bar is shared across all
# players, so its Monte Carlo error is common rather than per-player, and
# rankings stabilise well before individual values do.
BB_DEFAULT_SIMS <- 2000L

# Two-level draw. TRUE is the corrected behaviour and the production default.
# The FALSE path is kept, not deleted, so the pre-2026-08-03 construction stays
# runnable for A/B verification and so any figure produced under it can be
# reproduced. Same pattern as every other neutralised path in this build:
# machinery intact, gate evidence written into the file above.
BB_HIERARCHICAL <- TRUE

utils::globalVariables(c(
  "r32_sigma_week", "r32_omega", "r32_posterior_sigma",
  "bb_value", "bb_rank", "bb_starts",
  "sim", "wk", "bar", "pos_slot", "tau", "omega"
))

# ------------------------------------------------------------------------------
# .best_ball_week_score
# ------------------------------------------------------------------------------

#' Optimal best-ball lineup score for one week
#'
#' Exact for a slot structure where every slot is position-exclusive apart from
#' flex slots drawn from a single eligible set: take the top s_p at each
#' position, then fill flex from whatever remains. Greedy is optimal here
#' because no earlier choice can ever be improved by a later one.
#'
#' This is deliberately NOT a general optimizer. R/35 remains the general one,
#' with league rules, availability and matchups. Calling R/35 inside a
#' simulation loop would be both wrong for this structure and far too slow.
#'
#' @param points Numeric vector of weekly scores.
#' @param positions Character vector, same length.
#' @param starters Named list, e.g. list(QB = 1, RB = 2, WR = 3, TE = 1).
#' @param flex_n Integer. Number of generic flex slots.
#' @param flex_eligible Character vector.
#' @return Numeric scalar.
#' @keywords internal
.best_ball_week_score <- function(points, positions, starters, flex_n,
                                   flex_eligible = FLEX_ELIGIBLE) {

  total <- 0
  used  <- rep(FALSE, length(points))

  for (pos in names(starters)) {
    n_slot <- as.integer(starters[[pos]])
    if (n_slot <= 0L) next
    idx <- which(positions == pos)
    if (length(idx) == 0L) next
    ord <- idx[order(points[idx], decreasing = TRUE)]
    take <- ord[seq_len(min(n_slot, length(ord)))]
    total <- total + sum(points[take])
    used[take] <- TRUE
  }

  if (flex_n > 0L) {
    idx <- which(!used & positions %in% flex_eligible)
    if (length(idx) > 0L) {
      ord <- idx[order(points[idx], decreasing = TRUE)]
      take <- ord[seq_len(min(flex_n, length(ord)))]
      total <- total + sum(points[take])
    }
  }

  total
}

# ------------------------------------------------------------------------------
# .hier_draw
# ------------------------------------------------------------------------------

#' Draw a full simulated season panel for ONE player, two levels
#'
#' Returns n_sims * weeks weekly scores, ordered SEASON-MAJOR: rows
#' 1..weeks are season 1, rows (weeks+1)..(2*weeks) are season 2, and so on.
#' That ordering is required by every consumer in this file and by
#' \code{matrix(x, nrow = weeks, ncol = n_sims)}, which fills column-major.
#'
#' Level 1 draws the season mean once from Normal(mu, tau). Level 2 draws each
#' week around that season's mean with dispersion omega, through the R/50
#' predictive machinery so the fitted weekly shape is preserved.
#'
#' The gamma_points route is handled by delegating each season to
#' \code{predictive_draw()} rather than reimplementing the parameterisation.
#' That also preserves R/50's guard fallback for seasons whose drawn mean is
#' too small for a valid gamma.
#'
#' @param position Character. One position.
#' @param n_sims Integer. Simulated seasons.
#' @param weeks Integer. Weeks per season.
#' @param mu Numeric. Projected points per game.
#' @param tau Numeric. SD of the season mean (R/32 r32_posterior_sigma).
#' @param omega Numeric. Week-to-week dispersion (R/32 r32_omega).
#' @param shape_table R/50 shape table.
#' @param hierarchical Logical. FALSE reproduces the pre-2026-08-03 draw.
#' @return Numeric vector of length n_sims * weeks.
#' @keywords internal
.hier_draw <- function(position, n_sims, weeks, mu, tau, omega, shape_table,
                        hierarchical = BB_HIERARCHICAL) {

  n_draw <- as.integer(n_sims) * as.integer(weeks)

  tau   <- if (is.na(tau)) 0 else max(tau, 0)
  omega <- if (is.na(omega)) 0 else max(omega, 0)

  if (!isTRUE(hierarchical)) {
    # Pre-fix construction, retained for A/B only.
    return(predictive_draw(position, n_draw, mu,
                            sqrt(tau^2 + omega^2), shape_table))
  }

  if (omega <= 0) {
    stop(".hier_draw(): omega must be > 0 for ", position,
         " (mu = ", mu, "). A player with no weekly dispersion cannot be ",
         "simulated. Regenerate R/32 with the omega columns.", call. = FALSE)
  }

  # LEVEL 1: the season mean, settled once per simulated season.
  theta <- if (tau > 0) stats::rnorm(n_sims, mean = mu, sd = tau) else rep(mu, n_sims)

  ent <- shape_table[[as.character(position)]]

  if (!is.null(ent) && identical(ent$method, "gamma_points")) {
    # LEVEL 2, points-space gamma. Parameters depend on the season's own mean,
    # so this cannot be vectorised across seasons.
    out <- numeric(n_draw)
    for (s in seq_len(n_sims)) {
      idx <- ((s - 1L) * weeks + 1L):(s * weeks)
      out[idx] <- predictive_draw(position, weeks, theta[s], omega, shape_table)
    }
    return(out)
  }

  # LEVEL 2, standardized route. One shape_draw() call for the whole panel.
  # The zero floor sits on the WEEK, matching predictive_draw() exactly.
  pmax(rep(theta, each = weeks) +
         omega * shape_draw(position, n_draw, shape_table), 0)
}

# ------------------------------------------------------------------------------
# .build_baseline_roster
# ------------------------------------------------------------------------------

#' Replacement-level roster the marginal value is measured against
#'
#' Uses the replacement levels the existing VORP path already computes, so
#' bb_value and vorp_base are anchored to the SAME definition of replacement.
#' Inventing a second one would make the two columns incomparable and every
#' before/after rank comparison meaningless.
#'
#' tau and omega are carried SEPARATELY rather than pre-combined into
#' sigma_week, because the bar must be drawn by the same two-level process as
#' the players it is compared against. A bar drawn the old way against players
#' drawn the new way would make every comparison a comparison of two different
#' models. [2026-08-03]
#'
#' @param projections Tibble with position, r32_posterior_mu,
#'   r32_posterior_sigma, r32_omega.
#' @param replacement_ppg Named numeric from .compute_replacement_levels().
#' @param config league_config.
#' @return Tibble: position, mu, tau, omega, sigma_week.
#' @keywords internal
.build_baseline_roster <- function(projections, replacement_ppg, config) {

  # Dispersion for a replacement player: taken from the real players nearest
  # the replacement line at that position, not assumed. Medians are taken on
  # tau and omega independently, then sigma_week is derived, so the identity
  # sigma_week^2 = tau^2 + omega^2 holds for the baseline as it does for a
  # real player.
  nearest <- function(pos) {
    projections %>%
      dplyr::filter(.data$position == pos,
                     !is.na(.data$r32_omega), .data$r32_omega > 0,
                     !is.na(.data$r32_posterior_sigma)) %>%
      dplyr::mutate(gap = abs(.data$r32_posterior_mu - replacement_ppg[[pos]])) %>%
      dplyr::arrange(gap) %>%
      dplyr::slice_head(n = 10)
  }

  tau_v <- purrr::map_dbl(VORP_POSITIONS, function(pos) {
    d <- nearest(pos)
    if (nrow(d) == 0L) return(NA_real_)
    stats::median(d$r32_posterior_sigma, na.rm = TRUE)
  })
  omega_v <- purrr::map_dbl(VORP_POSITIONS, function(pos) {
    d <- nearest(pos)
    if (nrow(d) == 0L) return(NA_real_)
    stats::median(d$r32_omega, na.rm = TRUE)
  })
  names(tau_v)   <- VORP_POSITIONS
  names(omega_v) <- VORP_POSITIONS

  bad <- !is.finite(omega_v) | omega_v <= 0 | !is.finite(tau_v)
  if (any(bad)) {
    stop(".build_baseline_roster(): no usable tau/omega for ",
         paste(VORP_POSITIONS[bad], collapse = ", "),
         ". Regenerate R/32 with the omega columns.", call. = FALSE)
  }

  rows <- list()
  for (pos in VORP_POSITIONS) {
    n_slot <- as.integer(config$starters[[pos]])
    # One spare at each flex-eligible position so the flex slot has a real
    # contest rather than being filled by whoever is left over.
    n_extra <- if (pos %in% FLEX_ELIGIBLE) as.integer(config$flex %||% 0L) else 0L
    n_tot <- n_slot + n_extra
    if (n_tot <= 0L) next
    rows[[pos]] <- tibble::tibble(
      position   = pos,
      mu         = replacement_ppg[[pos]],
      tau        = tau_v[[pos]],
      omega      = omega_v[[pos]],
      sigma_week = sqrt(tau_v[[pos]]^2 + omega_v[[pos]]^2)
    ) %>% dplyr::slice(rep(1L, n_tot))
  }
  dplyr::bind_rows(rows)
}

# ------------------------------------------------------------------------------
# .simulate_bars
# ------------------------------------------------------------------------------

#' The score a new player must beat, per position, per simulated week
#'
#' @param baseline Tibble from .build_baseline_roster().
#' @param config league_config.
#' @param shape_table R/50 shape table.
#' @param n_sims Integer.
#' @param hierarchical Logical. Must match the value used for the candidates.
#' @return Numeric matrix, (n_sims * BB_SEASON_WEEKS) rows by position columns.
#' @keywords internal
.simulate_bars <- function(baseline, config, shape_table,
                            n_sims = BB_DEFAULT_SIMS,
                            hierarchical = BB_HIERARCHICAL) {

  n_draw <- n_sims * BB_SEASON_WEEKS

  # Baseline players sit on unrelated NFL teams, so they are drawn
  # independently. This is the assumption that makes the bar a property of the
  # FIELD rather than of any one offense.
  #
  # Each baseline player gets his OWN season mean per simulated season, drawn
  # at level 1, so the bar itself carries season-to-season variation rather
  # than being a fixed line with weekly noise on it. [2026-08-03]
  pts <- vapply(seq_len(nrow(baseline)), function(i) {
    .hier_draw(baseline$position[i], n_sims, BB_SEASON_WEEKS,
                baseline$mu[i], baseline$tau[i], baseline$omega[i],
                shape_table, hierarchical = hierarchical)
  }, numeric(n_draw))
  if (is.null(dim(pts))) pts <- matrix(pts, nrow = n_draw)

  pos_vec <- baseline$position
  flex_n  <- as.integer(config$flex %||% 0L)

  bars <- matrix(NA_real_, nrow = n_draw, ncol = length(VORP_POSITIONS),
                  dimnames = list(NULL, VORP_POSITIONS))

  for (r in seq_len(n_draw)) {
    row_pts <- pts[r, ]
    used <- rep(FALSE, length(row_pts))

    # A[s_p] per position, and mark starters used
    a_sp <- stats::setNames(rep(-Inf, length(VORP_POSITIONS)), VORP_POSITIONS)
    for (pos in VORP_POSITIONS) {
      n_slot <- as.integer(config$starters[[pos]])
      if (n_slot <= 0L) next
      idx <- which(pos_vec == pos)
      if (length(idx) == 0L) next
      ord <- idx[order(row_pts[idx], decreasing = TRUE)]
      take <- ord[seq_len(min(n_slot, length(ord)))]
      a_sp[pos] <- row_pts[take[length(take)]]
      used[take] <- TRUE
    }

    # F: best remaining flex-eligible score
    fidx <- which(!used & pos_vec %in% FLEX_ELIGIBLE)
    F_val <- if (flex_n > 0L && length(fidx) > 0L) max(row_pts[fidx]) else -Inf

    for (pos in VORP_POSITIONS) {
      bars[r, pos] <- if (pos %in% FLEX_ELIGIBLE && flex_n > 0L) {
        min(a_sp[pos], F_val)
      } else {
        a_sp[pos]
      }
    }
  }

  bars
}

# ------------------------------------------------------------------------------
# simulate_best_ball_value
# ------------------------------------------------------------------------------

#' Best-ball value over replacement, simulated from the weekly distribution
#'
#' Returns season points added over a replacement-level roster. Directly
#' comparable to vorp_base in units (points), but earned rather than assumed:
#' upside is priced by the lineup mechanics instead of by boom/bust/ceiling
#' weights.
#'
#' @param projections Tibble. R/32 output carrying r32_posterior_mu,
#'   r32_posterior_sigma (tau) and r32_omega.
#' @param config league_config.
#' @param shape_table R/50 shape table.
#' @param n_sims Integer.
#' @param seed Integer or NULL.
#' @param hierarchical Logical. TRUE is the corrected two-level draw. FALSE
#'   reproduces the pre-2026-08-03 construction and is for A/B only.
#' @return projections plus bb_value, bb_starts, bb_rank.
#' @export
simulate_best_ball_value <- function(projections, config, shape_table,
                                      n_sims = BB_DEFAULT_SIMS,
                                      seed = NULL,
                                      hierarchical = BB_HIERARCHICAL) {

  need <- c("r32_posterior_mu", "r32_posterior_sigma", "r32_omega")
  miss <- setdiff(need, names(projections))
  if (length(miss) > 0L) {
    stop("simulate_best_ball_value(): projections missing ",
         paste(miss, collapse = ", "),
         ". The two-level draw needs tau and omega separately, not a ",
         "pre-combined sigma_week. Regenerate R/32 with the omega columns.",
         call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  draw_label <- if (isTRUE(hierarchical)) {
    "two-level: theta once per season, weeks drawn around it"
  } else {
    "FLAT pre-2026-08-03 construction. A/B ONLY, not a production board."
  }
  message(glue("  draw: {draw_label}"))

  rep_result <- .compute_replacement_levels(projections, config)
  baseline <- .build_baseline_roster(projections, rep_result$replacement_ppg,
                                      config)

  message(glue("  simulating baseline bar: {n_sims} seasons x ",
               "{BB_SEASON_WEEKS} weeks"))
  bars <- .simulate_bars(baseline, config, shape_table, n_sims = n_sims,
                          hierarchical = hierarchical)
  n_draw <- nrow(bars)

  cand <- projections %>%
    dplyr::filter(.data$position %in% VORP_POSITIONS,
                   !is.na(.data$r32_posterior_mu),
                   !is.na(.data$r32_omega),
                   .data$r32_omega > 0)

  message(glue("  pricing {nrow(cand)} players against the bar"))

  vals <- vapply(seq_len(nrow(cand)), function(i) {
    pos <- cand$position[i]
    x <- .hier_draw(pos, n_sims, BB_SEASON_WEEKS,
                     cand$r32_posterior_mu[i],
                     cand$r32_posterior_sigma[i],
                     cand$r32_omega[i],
                     shape_table, hierarchical = hierarchical)
    inc <- pmax(0, x - bars[, pos])
    c(value = mean(inc) * BB_SEASON_WEEKS,
      starts = mean(inc > 0))
  }, numeric(2))

  cand %>%
    dplyr::mutate(
      bb_value  = as.numeric(vals["value", ]),
      bb_starts = as.numeric(vals["starts", ])
    ) %>%
    dplyr::arrange(dplyr::desc(bb_value)) %>%
    dplyr::mutate(bb_rank = dplyr::row_number())
}

# ------------------------------------------------------------------------------
# simulate_roster_season
# ------------------------------------------------------------------------------

#' Simulate a full roster's season with team correlation
#'
#' Unlike simulate_best_ball_value(), this draws players on the SAME NFL team
#' together through the R/50 copula, so it can answer stacking questions. Use
#' it for roster-construction and stacking analysis; use the marginal-value
#' function for a draft board.
#'
#' The roster must carry tau and omega separately. A roster built with only
#' sigma_week is REJECTED rather than silently simulated the old way, because
#' a stale analysis script producing pre-2026-08-03 numbers without saying so
#' is worse than one that fails. [2026-08-03]
#'
#' @param roster Tibble: position, team, mu, tau, omega. One row per player.
#' @param config league_config.
#' @param shape_table R/50 shape table.
#' @param pc POSxPOS correlation matrix (calibrated Gaussian inputs).
#' @param n_sims Integer.
#' @param weeks Integer. Defaults to the full regular season. Set to 3 to
#'   simulate only a best-ball tournament playoff window.
#' @param seed Integer or NULL.
#' @param hierarchical Logical. FALSE reproduces the pre-fix draw and requires
#'   sigma_week instead of tau/omega. A/B only.
#' @return Numeric vector of simulated totals, length n_sims.
#' @export
simulate_roster_season <- function(roster, config, shape_table, pc,
                                    n_sims = 2000L, weeks = BB_SEASON_WEEKS,
                                    seed = NULL,
                                    hierarchical = BB_HIERARCHICAL) {

  need <- if (isTRUE(hierarchical)) {
    c("position", "team", "mu", "tau", "omega")
  } else {
    c("position", "team", "mu", "sigma_week")
  }
  miss <- setdiff(need, names(roster))
  if (length(miss) > 0) {
    stop("simulate_roster_season(): roster missing ",
         paste(miss, collapse = ", "),
         if (isTRUE(hierarchical))
           ". The two-level draw needs tau and omega separately. Add
  tau = r32_posterior_sigma, omega = r32_omega
to the roster build." else "",
         call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  teams <- unique(roster$team)
  # One prepared copula per NFL team represented on the roster. Players on
  # different NFL teams are independent, so only within-team blocks need it.
  preps <- purrr::map(teams, function(tm) {
    idx <- which(roster$team == tm)
    list(idx = idx, prep = prepare_copula(roster$position[idx], pc))
  })

  flex_n <- as.integer(config$flex %||% 0L)
  n_week_total <- n_sims * weeks

  # Draw every team-week at once per NFL team. Drawing one week at a time was
  # correct but unusably slow: it made one copula call per team per week per
  # simulation. The correlation structure is identical across weeks, so the
  # prepared factorisation is reused and the whole panel comes out in one call.
  z <- matrix(NA_real_, nrow = n_week_total, ncol = nrow(roster))
  for (tp in preps) {
    z[, tp$idx] <- draw_copula_z(tp$prep, shape_table, n_draws = n_week_total)
  }

  if (isTRUE(hierarchical)) {
    # LEVEL 1: one season mean per player per simulated season. Drawn
    # independently across players, team-mates included: the copula below was
    # calibrated on WEEKLY residuals and there is no season-level estimate to
    # justify correlating theta. Stated as a limit in the header block.
    n_p <- nrow(roster)
    theta <- matrix(
      stats::rnorm(n_sims * n_p,
                    mean = rep(roster$mu,  each = n_sims),
                    sd   = rep(pmax(dplyr::coalesce(roster$tau, 0), 0),
                                each = n_sims)),
      nrow = n_sims, ncol = n_p
    )
    # Expand season-major: rows 1..weeks are season 1, and so on.
    theta_rep <- theta[rep(seq_len(n_sims), each = weeks), , drop = FALSE]

    # LEVEL 2: correlated weekly draws around that season's mean.
    pts <- pmax(
      theta_rep +
        matrix(roster$omega, nrow = n_week_total, ncol = n_p, byrow = TRUE) * z,
      0
    )
  } else {
    # Pre-2026-08-03 construction. A/B only.
    pts <- pmax(
      matrix(roster$mu, nrow = n_week_total, ncol = nrow(roster), byrow = TRUE) +
        matrix(roster$sigma_week, nrow = n_week_total, ncol = nrow(roster),
                byrow = TRUE) * z,
      0
    )
  }

  wk_scores <- vapply(seq_len(n_week_total), function(r) {
    .best_ball_week_score(pts[r, ], roster$position, config$starters, flex_n)
  }, numeric(1))

  colSums(matrix(wk_scores, nrow = weeks, ncol = n_sims))
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
#'   player_uid, nfl_gsis_id, player_name, team, position, r32_posterior_mu,
#'   boom_probability, bust_probability, r32_projection_upper_80.
#' @param config league_config list.
#' @return Tibble with one row per player, sorted by overall_rank.
#' @export
compute_vorp_rankings <- function(projections, config) {

  if (!inherits(config, "league_config")) {
    stop("config must be a league_config object (use build_league_config()).")
  }

  # ---------------------------------------------------------------------------
  # QB AVAILABILITY WEIGHTING  [2026-08-09]
  # ---------------------------------------------------------------------------
  # r32_posterior_mu is PPG WHEN PLAYING. R/32 used to multiply backup QBs by a
  # depth-chart availability factor, which mixed two quantities: it compared a
  # season-contribution number against a per-game realized rate. Measured on the
  # 2018-2025 holdout, that made backup projections more than twice as wrong
  # (MAE 13.05 discounted vs 5.87 undiscounted, n = 44). R/32 no longer applies
  # it; see QB_DEPTH_DISCOUNT_APPLY_TO_MU there.
  #
  # But a DRAFT BOARD does ask the season-contribution question: a QB2 who
  # scores 20 PPG in the three games he plays is not worth a roster spot ahead
  # of a starter. VORP is where that question lives, so the factor is applied
  # HERE instead.
  #
  # It is applied BEFORE replacement levels are computed, not after, because
  # replacement is the first bench player's mu. Leaving backups at starter-level
  # mu through the replacement calculation would let a QB2 displace a real
  # starter and move the replacement bar itself.
  #
  # Non-QBs and QBs with no depth record carry a factor of 1 and are unchanged.
  # Set VORP_APPLY_QB_AVAILABILITY to FALSE to neutralize with machinery intact.
  if (isTRUE(VORP_APPLY_QB_AVAILABILITY) &&
      "qb_discount_factor" %in% names(projections)) {

    n_adj <- sum(!is.na(projections$qb_discount_factor) &
                   projections$qb_discount_factor < 1, na.rm = TRUE)

    projections <- projections %>%
      dplyr::mutate(
        vorp_availability = dplyr::coalesce(.data$qb_discount_factor, 1),
        r32_posterior_mu_ppg = .data$r32_posterior_mu,
        r32_posterior_mu = .data$r32_posterior_mu * .data$vorp_availability
      )

    message(glue::glue(
      "  QB availability weighting applied to {n_adj} backup(s); ",
      "per-game mu retained as r32_posterior_mu_ppg."))
  } else if ("qb_discount_factor" %in% names(projections)) {
    projections <- projections %>%
      dplyr::mutate(vorp_availability = 1,
                    r32_posterior_mu_ppg = .data$r32_posterior_mu)
    message("  QB availability weighting DISABLED (VORP_APPLY_QB_AVAILABILITY).")
  } else {
    projections <- projections %>%
      dplyr::mutate(vorp_availability = 1,
                    r32_posterior_mu_ppg = .data$r32_posterior_mu)
    warning("compute_vorp_rankings(): no qb_discount_factor column; QB backups ",
            "will be ranked on per-game PPG with no availability weighting.",
            call. = FALSE)
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

  # Select final schema columns.
  #
  # NOTE: this whitelist silently drops any column not named here. If a new
  # quantity is computed above and does not appear in this list, it will not
  # reach the caller and the omission produces no error. [2026-08-09]
  #
  # r32_posterior_mu in this output is the AVAILABILITY-WEIGHTED value used for
  # ranking, not the per-game rate. r32_posterior_mu_ppg carries the per-game
  # number, and vorp_availability is the factor applied. For every non-QB and
  # every QB1 the two mu columns are identical and the factor is 1.
  vorp_df %>%
    dplyr::select(
      player_uid, nfl_gsis_id, player_name, team, position,
      league_name, league_format, league_teams,
      r32_posterior_mu, r32_posterior_mu_ppg, vorp_availability,
      r32_projection_lower_80, r32_projection_upper_80,
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
  # n_distinct on player_uid, not nfl_gsis_id: the latter is NA for the
  # id-less cohort and counts all of them as one player.
  n_players <- dplyr::n_distinct(all_rankings$player_uid)
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
