# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Player Volume Allocation Engine
# File: R/31_player_volume_allocation.R
#
# PURPOSE
# -------
# Second foundation layer for team-aware projections (Option 3 architecture).
# Distributes R/30 team-level volumes (pass attempts, rush attempts) among
# individual players using:
#   1. Depth chart position from nflreadr::load_depth_charts()
#   2. Rookie draft capital from nflreadr::load_draft_picks()
#   3. Talent multiplier from R/28 dynasty score_final
#   4. Soft constraint enforcement -- per-team target/rush shares rescaled
#      to sum to ~1.0 only when total drifts outside [0.95, 1.05]
#
# WHY THIS EXISTS
# ---------------
# R/29 projects players in isolation. Tai Felton gets ~10 PPG whether or not
# Jefferson and Addison exist on his team. R/31 fixes this by allocating a
# finite pool: the MIN WR target pool is ~60% of MIN's team passes (rest go
# to TE/RB), and the entrenched WR1/WR2 already claim ~43% of that pool.
# Whatever's left is what's actually available for Felton to compete for.
#
# THREE-STAGE ALLOCATION
# ----------------------
#   1. BASE PRIOR     : Depth chart position drives the starting share
#                       (WR1 = 25%, WR2 = 17%, etc. -- see TARGET_SHARE_PRIORS)
#
#   2. ROOKIE ADJUST  : Rookies get a draft-capital multiplier on their
#                       base prior. 1st round = 1.15x, 7th round = 0.55x.
#                       Reflects that high-capital rookies outperform their
#                       initial depth chart position.
#
#   3. TALENT ADJUST  : Non-rookies get a z-score-based multiplier from
#                       their R/28 score_final relative to their position.
#                       Capped at [0.70, 1.30] to prevent extreme rescaling.
#
# After all three stages, per-team shares are summed per role group. If the
# sum falls outside [0.95, 1.05] the team's shares get rescaled. Within
# tolerance, the modeled differences are preserved (a team summing to 1.04
# means the model thinks that team has slightly more talent than baseline,
# which is worth keeping as signal).
#
# DESIGN DECISIONS (confirmed in scoping)
# ---------------------------------------
#   - Depth chart      : nflreadr most recent week, accepted as-is
#   - Constraint type  : Soft. Rescale only outside [0.95, 1.05]
#   - Rookie priors    : Draft capital multiplier on depth-position prior
#   - Talent signal    : R/28 score_final, z-scored by position
#   - Injury logic     : NOT in R/31 (separate weekly function in R/32+)
#   - Positions        : QB, RB, WR, TE only (DEF/ST handled separately)
#   - Sack adjustment  : R/30 team_pass_pg includes sacks; downstream may
#                        apply a 0.93 multiplier to convert to thrown passes
#
# DEPTH POSITION CLASSIFICATION
# -----------------------------
# nflreadr's depth_position column uses strings like "WR", "RB", "TE", "QB"
# with a depth_team rank. We collapse to:
#   QB1, QB2 (and QB3+ as needed)
#   RB1, RB2, RB3 (others lumped as RB4)
#   WR1, WR2, WR3, WR4, WR5 (others lumped as WR5)
#   TE1, TE2 (others lumped as TE3)
#
# TARGET SHARE PRIORS (historical league averages)
# ------------------------------------------------
# Sum across WR + TE + RB roles approximates 1.0 (typical NFL distribution):
#   WR1 = 0.250, WR2 = 0.170, WR3 = 0.110, WR4 = 0.040, WR5 = 0.015
#   TE1 = 0.150, TE2 = 0.050, TE3 = 0.010
#   RB1 = 0.140, RB2 = 0.050, RB3 = 0.010
#   Total = 1.000
#
# RUSH SHARE PRIORS (historical league averages)
# ----------------------------------------------
#   RB1 = 0.650, RB2 = 0.250, RB3 = 0.080
#   QB1 = 0.015 (scramble baseline; mobile QBs adjusted via talent mult.)
#   WR/TE end-arounds: minimal, not modeled
#   Total = 0.995 (small remainder for incidental carries)
#
# OUTPUTS
# -------
#   data/season2_cache/s2_week15_player_volume_allocation.rds
#   data/season2_cache/s2_week15_player_volume_allocation.csv
#
# One row per active 2026 offensive player. Schema:
#   nfl_gsis_id                chr   gsis ID (join key)
#   player_name                chr   display name
#   team                       chr   2026 team
#   position                   chr   QB/RB/WR/TE
#   depth_position             chr   QB1/RB1/WR3/TE2 etc.
#   depth_rank                 int   numeric rank within position
#   is_rookie                  lgl   2026 first NFL season
#   draft_round                int   1-7 for drafted, NA for UDFA/veteran
#   rookie_capital_mult        dbl   1.15 for R1 rookie down to 0.45 UDFA
#                                    1.0 for non-rookies
#   score_final                dbl   R/28 dynasty score (NA if not present)
#   talent_z                   dbl   z-score within position from score_final
#   talent_multiplier          dbl   1 + 0.10 * talent_z, capped [0.70, 1.30]
#   target_share_base          dbl   depth-position prior
#   target_share_adjusted      dbl   after rookie + talent multipliers
#   target_share               dbl   after soft constraint per team
#   rush_share_base            dbl   depth-position prior
#   rush_share_adjusted        dbl   after rookie + talent multipliers
#   rush_share                 dbl   after soft constraint per team
#   projected_team_pass_pg     dbl   from R/30
#   projected_team_rush_pg     dbl   from R/30
#   expected_targets_pg        dbl   target_share * team_pass_pg
#   expected_carries_pg        dbl   rush_share * team_rush_pg
#   schema_tag                 chr   "s2_w15_player_alloc_v1"
#
# SOURCE DEPENDENCIES
# -------------------
#   R/30_team_volume_projections.R -- project_team_volumes() output
#
# INPUT ARTIFACTS
# ---------------
#   data/season2_cache/s2_week15_team_volumes.rds (or .csv)
#     From R/30. Required.
#
#   data/season2_cache/s2_week14_final_prospect_scores.csv
#     From R/28. Required for talent multiplier. Players missing from this
#     file get talent_multiplier = 1.0 (neutral).
#
# RUN
# ---
#   source(here::here("R", "31_player_volume_allocation.R"))
#   alloc <- allocate_player_volumes()
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
library(nflreadr)

source(here::here("R", "30_team_volume_projections.R"))
source(here::here("R", "19_sleeper_api.R"))   # for get_all_sleeper_players()

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

SEASON_ALLOC <- 2026L

# Supported positions for allocation
ALLOC_POSITIONS <- c("QB", "RB", "WR", "TE")

# Soft constraint bounds. If per-team summed share falls outside this range,
# rescale to 1.0. Inside this range, preserve modeled differences as signal.
CONSTRAINT_LOWER <- 0.95
CONSTRAINT_UPPER <- 1.05

# Talent multiplier sensitivity: 1 + 0.10 * z-score, capped
TALENT_MULT_SENSITIVITY <- 0.10
TALENT_MULT_FLOOR <- 0.70
TALENT_MULT_CEILING <- 1.30

# Target share priors by depth position (sum across all positions ~= 1.0)
TARGET_SHARE_PRIORS <- c(
  "QB1" = 0.000, "QB2" = 0.000, "QB3" = 0.000,
  "RB1" = 0.140, "RB2" = 0.050, "RB3" = 0.010, "RB4" = 0.005,
  "WR1" = 0.250, "WR2" = 0.170, "WR3" = 0.110, "WR4" = 0.040,
  "WR5" = 0.015,
  "TE1" = 0.150, "TE2" = 0.050, "TE3" = 0.010
)

# Rush share priors by depth position (sum ~= 1.0)
RUSH_SHARE_PRIORS <- c(
  "QB1" = 0.015, "QB2" = 0.000, "QB3" = 0.000,
  "RB1" = 0.650, "RB2" = 0.250, "RB3" = 0.080, "RB4" = 0.005,
  "WR1" = 0.000, "WR2" = 0.000, "WR3" = 0.000, "WR4" = 0.000,
  "WR5" = 0.000,
  "TE1" = 0.000, "TE2" = 0.000, "TE3" = 0.000
)

# Rookie draft capital multipliers (applied to base prior for rookies only)
ROOKIE_CAPITAL_MULTIPLIERS <- c(
  "1"  = 1.15,
  "2"  = 1.05,
  "3"  = 0.95,
  "4"  = 0.85,
  "5"  = 0.75,
  "6"  = 0.65,
  "7"  = 0.55,
  "U"  = 0.45    # undrafted free agent
)

# Position-specific max depth rank to keep (deeper players get folded into max)
MAX_DEPTH_RANK <- c("QB" = 3L, "RB" = 4L, "WR" = 5L, "TE" = 3L)

# Paths
TEAM_VOLUMES_RDS  <- here::here("data", "season2_cache",
                                 "s2_week15_team_volumes.rds")
DYNASTY_SCORES_CSV <- here::here("data", "season2_cache",
                                  "s2_week14_final_prospect_scores.csv")
OUTPUT_RDS_PATH_ALLOC <- here::here("data", "season2_cache",
                                     "s2_week15_player_volume_allocation.rds")
OUTPUT_CSV_PATH_ALLOC <- here::here("data", "season2_cache",
                                     "s2_week15_player_volume_allocation.csv")

SCHEMA_TAG_ALLOC <- "s2_w15_player_alloc_v1"

# ------------------------------------------------------------------------------
# NSE DECLARATIONS
# ------------------------------------------------------------------------------

utils::globalVariables(c(
  "nfl_gsis_id", "player_name", "team", "position", "depth_position",
  "depth_rank", "is_rookie", "draft_round", "rookie_capital_mult",
  "score_final", "talent_z", "talent_multiplier",
  "target_share_base", "target_share_adjusted", "target_share",
  "rush_share_base", "rush_share_adjusted", "rush_share",
  "projected_team_pass_pg", "projected_team_rush_pg",
  "expected_targets_pg", "expected_carries_pg",
  "gsis_id", "full_name", "first_name", "last_name",
  "club_code", "depth_chart_position", "depth_team",
  "pos_grp", "pos_abb", "pos_name", "pos_rank", "pos_slot",
  "pos_grp_id", "pos_id", "dt", "espn_id",
  "rookie_year", "round", "pos", "side", "season", "week",
  "depth_pos_clean", "depth_rank_raw", "key",
  "cfb_player_name", "position_score", "position_mean", "position_sd",
  "team_target_sum", "team_rush_sum", "team_target_rescale",
  "team_rush_rescale", "schema_tag"
))

# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

# ------------------------------------------------------------------------------
# SLEEPER DEPTH CHART OVERRIDES (Phase 4 fix, Week 15)
# ------------------------------------------------------------------------------
#
# nflreadr::load_depth_charts() depends on PFR which lags 1-4 weeks behind
# actual NFL roster moves during the offseason. Sleeper updates its player
# database within hours. The helpers below merge Sleeper team assignments
# and depth_chart_order into the nflreadr depth chart to capture trades and
# signings that nflreadr hasn't picked up yet.
#
# Mirrors the team code crosswalk and normalizer used in R/29. Future
# refactor: lift both into a shared utility.

# Sleeper-to-nflreadr team code crosswalk. Only entries that DIFFER.
SLEEPER_TO_NFLREADR_TEAM_MAP_R31 <- c(
  "LAR" = "LA",     # Los Angeles Rams
  "AZ"  = "ARI",    # Arizona Cardinals (Sleeper uses AZ, nflfastR uses ARI)
  "JAC" = "JAX",    # Jacksonville Jaguars (defensive)
  "WSH" = "WAS"     # Washington Commanders (defensive)
)


#' Normalize Sleeper team codes to nflreadr conventions
#' @keywords internal
.normalize_sleeper_team_codes_r31 <- function(team_vec) {
  out <- team_vec
  for (sleeper_code in names(SLEEPER_TO_NFLREADR_TEAM_MAP_R31)) {
    out[!is.na(out) & out == sleeper_code] <-
      SLEEPER_TO_NFLREADR_TEAM_MAP_R31[[sleeper_code]]
  }
  out
}


#' Normalize player name for fallback matching
#'
#' Strips apostrophes, periods, suffixes (Jr/Sr/II/III/IV), non-letters,
#' and collapses whitespace. Used to match Sleeper records that lack
#' gsis_id back to nflreadr depth chart entries when both refer to the
#' same player despite typographic differences.
#'
#' Examples:
#'   "Ja'Marr Chase"     -> "jamarr chase"
#'   "D.J. Chark Jr."    -> "dj chark"
#'   "Marvin Harrison Jr." -> "marvin harrison"
#'
#' @keywords internal
.normalize_name_for_match_r31 <- function(name_vec) {
  out <- tolower(as.character(name_vec))
  out <- gsub("[.']", "", out)
  out <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", out, ignore.case = TRUE)
  out <- gsub("[^a-z ]", "", out)
  out <- gsub("\\s+", " ", out)
  trimws(out)
}


#' Merge Sleeper depth chart data into nflreadr depth chart
#'
#' Two-pass match against Sleeper:
#'   1. By gsis_id -- direct match for Sleeper records that have it
#'   2. By normalized name + position -- recovers nflreadr's gsis_id for
#'      Sleeper records where Sleeper's gsis_id field is NA
#'
#' For matched players, Sleeper's team and depth_chart_order replace
#' nflreadr's values. nflreadr-only players (not in Sleeper at all) keep
#' their nflreadr assignment. After merge, depth ranks are re-numbered
#' within team + position so they remain contiguous integers starting at 1.
#'
#' The name-fallback pass is critical for veterans Sleeper has but whose
#' gsis_id mapping is missing in Sleeper's data (e.g., Jaylen Waddle's
#' DEN reassignment in 2026).
#'
#' @param nflreadr_dc Tibble from .load_2026_depth_charts() pre-merge.
#'   Must have nfl_gsis_id, player_name, team, position, depth_rank_raw.
#' @return Same schema. Player rows replaced with Sleeper data where
#'   available (via gsis_id or name match), re-ranked within team + position.
#' @keywords internal
.merge_sleeper_depth_overrides <- function(nflreadr_dc) {

  message("    Applying Sleeper depth chart overrides...")

  sleeper <- tryCatch(
    get_all_sleeper_players(),
    error = function(e) {
      message(glue(
        "    Sleeper data unavailable: {e$message}. ",
        "Continuing with nflreadr depth chart only (may be stale)."
      ))
      NULL
    }
  )

  if (is.null(sleeper) || nrow(sleeper) == 0L) {
    return(nflreadr_dc)
  }

  # Common filters: skill position + Sleeper team that passes ACTIVE_TEAMS_2026
  sleeper_filtered <- sleeper %>%
    dplyr::filter(
      !is.na(.data$team),
      .data$position %in% ALLOC_POSITIONS
    ) %>%
    dplyr::mutate(team = .normalize_sleeper_team_codes_r31(.data$team)) %>%
    dplyr::filter(.data$team %in% ACTIVE_TEAMS_2026)

  # ----- Pass 1: direct gsis_id match -----
  sleeper_with_gsis <- sleeper_filtered %>%
    dplyr::filter(!is.na(.data$nfl_gsis_id)) %>%
    dplyr::transmute(
      nfl_gsis_id       = .data$nfl_gsis_id,
      player_name       = .data$player_name,
      team              = .data$team,
      position          = .data$position,
      depth_rank_raw    = as.integer(dplyr::coalesce(
        .data$depth_chart_order, 999L
      )),
      depth_was_missing = is.na(.data$depth_chart_order)
    ) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)

  # ----- Pass 2: name fallback match for Sleeper records missing gsis_id -----
  # Build nflreadr lookup keyed by normalized name + position, restricted to
  # unique matches (a name+position appearing only once in nflreadr's pool)
  # to avoid false-positive joins to similarly-named players.
  nflreadr_lookup <- nflreadr_dc %>%
    dplyr::mutate(
      name_norm = .normalize_name_for_match_r31(.data$player_name)
    ) %>%
    dplyr::group_by(.data$name_norm, .data$position) %>%
    dplyr::filter(dplyr::n() == 1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(name_norm, position, recovered_gsis_id = nfl_gsis_id)

  sleeper_no_gsis <- sleeper_filtered %>%
    dplyr::filter(is.na(.data$nfl_gsis_id)) %>%
    dplyr::mutate(
      name_norm = .normalize_name_for_match_r31(.data$player_name)
    )

  sleeper_recovered <- sleeper_no_gsis %>%
    dplyr::inner_join(nflreadr_lookup,
                       by = c("name_norm", "position")) %>%
    # Don't recover a gsis_id already covered by Pass 1
    dplyr::anti_join(sleeper_with_gsis, by = c("recovered_gsis_id" = "nfl_gsis_id")) %>%
    dplyr::transmute(
      nfl_gsis_id       = .data$recovered_gsis_id,
      player_name       = .data$player_name,
      team              = .data$team,
      position          = .data$position,
      depth_rank_raw    = as.integer(dplyr::coalesce(
        .data$depth_chart_order, 999L
      )),
      depth_was_missing = is.na(.data$depth_chart_order)
    ) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)

  # Combine both passes
  sleeper_rows <- dplyr::bind_rows(sleeper_with_gsis, sleeper_recovered)

  # nflreadr rows for players NOT covered by either Sleeper pass
  # depth_was_missing = FALSE for all nflreadr rows (their depth rank comes
  # from actual depth chart data, not Sleeper's often-incomplete pre-season
  # depth_chart_order field).
  sleeper_gsis <- sleeper_rows$nfl_gsis_id
  nflreadr_only <- nflreadr_dc %>%
    dplyr::filter(!.data$nfl_gsis_id %in% sleeper_gsis) %>%
    dplyr::mutate(depth_was_missing = FALSE)

  # Combine and re-rank within team + position
  unified <- dplyr::bind_rows(nflreadr_only, sleeper_rows) %>%
    dplyr::group_by(.data$team, .data$position) %>%
    dplyr::arrange(.data$depth_rank_raw, .data$player_name,
                    .by_group = TRUE) %>%
    dplyr::mutate(depth_rank_raw = dplyr::row_number()) %>%
    dplyr::ungroup()

  # Diagnostic counts
  team_lookup <- nflreadr_dc %>%
    dplyr::select(nfl_gsis_id, team_old = team)
  n_team_moved <- sleeper_rows %>%
    dplyr::inner_join(team_lookup, by = "nfl_gsis_id") %>%
    dplyr::filter(.data$team != .data$team_old) %>%
    nrow()
  n_recovered <- nrow(sleeper_recovered)
  n_added <- nrow(sleeper_rows) -
    nrow(dplyr::inner_join(
      sleeper_rows %>% dplyr::select(nfl_gsis_id),
      nflreadr_dc %>% dplyr::select(nfl_gsis_id),
      by = "nfl_gsis_id"
    ))

  message(glue(
    "    Depth chart Sleeper merge: ",
    "{n_team_moved} team change(s), ",
    "{n_recovered} recovered via name fallback, ",
    "{n_added} new rookie(s)/signing(s) added, ",
    "{nrow(unified)} total rows after re-rank"
  ))

  unified
}


# ------------------------------------------------------------------------------
# .load_2026_depth_charts
# ------------------------------------------------------------------------------

#' Load most recent available depth charts for the target season
#'
#' Tries the target season first (e.g., 2026 pre-season depth charts). If
#' empty, falls back to the final week of the prior season. Returns a tibble
#' with one row per player-team-position with the latest depth rank.
#'
#' @param season Integer. Target season (default SEASON_ALLOC).
#' @return Tibble: gsis_id, full_name, team, position, depth_position,
#'   depth_rank.
#' @keywords internal
.load_2026_depth_charts <- function(season = SEASON_ALLOC) {

  message(glue("  Loading depth charts for season {season}"))

  dc <- tryCatch(
    nflreadr::load_depth_charts(seasons = season),
    error = function(e) {
      message(glue("    Season {season} depth charts unavailable: ",
                   "{e$message}"))
      NULL
    }
  )

  if (is.null(dc) || nrow(dc) == 0L) {
    message(glue("    Falling back to season {season - 1L} final week"))
    dc <- tryCatch(
      nflreadr::load_depth_charts(seasons = season - 1L),
      error = function(e) NULL
    )

    if (is.null(dc) || nrow(dc) == 0L) {
      stop(".load_2026_depth_charts(): no depth chart data available")
    }
  }

  # Identify the latest week available.
  # Pre-season depth charts may not have a week column at all -- in that
  # case the full table is already the most recent snapshot.
  has_week_col <- "week" %in% names(dc) &&
                    any(!is.na(dc[["week"]]))

  if (has_week_col) {
    latest_week <- max(dc[["week"]], na.rm = TRUE)
    message(glue("    Filtering to week {latest_week}"))
    dc_latest <- dc %>%
      dplyr::filter(.data$week == latest_week) %>%
      dplyr::filter(!is.na(.data$gsis_id), nchar(.data$gsis_id) > 0)
  } else {
    message("    No week column -- using full depth chart snapshot")
    dc_latest <- dc %>%
      dplyr::filter(!is.na(.data$gsis_id), nchar(.data$gsis_id) > 0)
  }

  # Column name handling: depth_position vs position vs depth_chart_position
  # varies across nflreadr versions
  # pos_abb = player position abbreviation (WR/QB/RB/TE)
  # pos_grp = formation name (Base 4-3 D, 3WR 1TE) -- NOT a position column
  pos_col <- dplyr::case_when(
    "pos_abb"              %in% names(dc_latest) ~ "pos_abb",
    "depth_position"       %in% names(dc_latest) ~ "depth_position",
    "position"             %in% names(dc_latest) ~ "position",
    "depth_chart_position" %in% names(dc_latest) ~ "depth_chart_position",
    "pos_name"             %in% names(dc_latest) ~ "pos_name",
    TRUE                                          ~ NA_character_
  )[1]

  if (is.na(pos_col)) {
    stop(glue(".load_2026_depth_charts(): no position column found. ",
              "Available columns: {paste(names(dc_latest), collapse = ', ')}"))
  }

  team_col <- dplyr::case_when(
    "team"      %in% names(dc_latest) ~ "team",
    "club_code" %in% names(dc_latest) ~ "club_code",
    TRUE                               ~ NA_character_
  )[1]

  if (is.na(team_col)) {
    stop(".load_2026_depth_charts(): no team column found in depth charts")
  }

  # Rank column: pos_rank in newer nflreadr, depth_team in older
  rank_col <- dplyr::case_when(
    "pos_rank"           %in% names(dc_latest) ~ "pos_rank",
    "depth_team"         %in% names(dc_latest) ~ "depth_team",
    "depth_position_rank" %in% names(dc_latest) ~ "depth_position_rank",
    TRUE                                        ~ NA_character_
  )[1]

  # Name column: full_name preferred
  name_col <- dplyr::case_when(
    "full_name" %in% names(dc_latest) ~ "full_name",
    "player_name" %in% names(dc_latest) ~ "player_name",
    "first_name" %in% names(dc_latest) ~ "first_name",
    TRUE ~ NA_character_
  )[1]

  if (is.na(name_col)) {
    dc_latest$player_name_synth <- dc_latest$gsis_id
    name_col <- "player_name_synth"
  }

  result <- dc_latest %>%
    dplyr::transmute(
      nfl_gsis_id    = .data$gsis_id,
      player_name    = .data[[name_col]],
      # Apply both normalizers: .normalize_team_codes handles legacy NFL
      # codes (STL/OAK/SD); .normalize_sleeper_team_codes_r31 then maps
      # alternate-spelling codes (AZ, LAR, JAC, WSH) to canonical form.
      # Both passes needed so nflreadr's "AZ" rows aren't silently dropped
      # by the ACTIVE_TEAMS_2026 filter below.
      team           = .normalize_sleeper_team_codes_r31(
                          .normalize_team_codes(.data[[team_col]])
                       ),
      position       = .data[[pos_col]],
      depth_rank_raw = if (!is.na(rank_col)) as.integer(.data[[rank_col]]) else 1L
    ) %>%
    dplyr::filter(
      .data$position %in% ALLOC_POSITIONS,
      .data$team %in% ACTIVE_TEAMS_2026,
      !is.na(.data$nfl_gsis_id)
    ) %>%
    # One row per player: this data has one record per player-formation
    # combination (167K+ rows total). Keep the row with the lowest depth rank
    # per player-team-position, which represents their primary role.
    dplyr::group_by(.data$nfl_gsis_id, .data$team, .data$position) %>%
    dplyr::slice_min(.data$depth_rank_raw, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup()

  week_label <- if (exists("latest_week", inherits = FALSE)) {
    paste0("week ", latest_week)
  } else {
    "no week column"
  }
  message(glue("    Depth chart loaded: {nrow(result)} player-positions ",
               "({week_label})"))

  # Apply Sleeper overrides to catch offseason moves nflreadr/PFR has missed
  result <- .merge_sleeper_depth_overrides(result)

  result
}

# ------------------------------------------------------------------------------
# .classify_depth_position
# ------------------------------------------------------------------------------

#' Collapse raw depth_team rank into bucketed depth position string
#'
#' Caps the rank per position per MAX_DEPTH_RANK (e.g., 5 WRs per team kept
#' as WR1-WR5; deeper WRs all become WR5). Produces a normalized string
#' like "WR3" or "TE1" suitable for joining to TARGET_SHARE_PRIORS.
#'
#' @param positions Character vector of position codes (QB, RB, WR, TE).
#' @param ranks Integer vector of raw depth ranks.
#' @return Character vector of bucketed depth_position strings.
#' @keywords internal
.classify_depth_position <- function(positions, ranks) {
  max_ranks <- MAX_DEPTH_RANK[positions]
  capped <- pmin(ranks, max_ranks)
  capped <- pmax(capped, 1L)  # any 0 or negative becomes 1
  paste0(positions, capped)
}

# ------------------------------------------------------------------------------
# .load_rookie_draft_capital
# ------------------------------------------------------------------------------

#' Load 2026 rookie draft picks and assign capital multipliers
#'
#' Pulls nflreadr::load_draft_picks(season) and joins on gsis_id. Returns
#' the round for each rookie, plus the multiplier from
#' ROOKIE_CAPITAL_MULTIPLIERS. UDFAs (not in draft pool) get round NA
#' and the "U" multiplier (0.45).
#'
#' @param season Integer. Rookie class season (default SEASON_ALLOC).
#' @return Tibble: nfl_gsis_id, draft_round, rookie_capital_mult.
#' @keywords internal
.load_rookie_draft_capital <- function(season = SEASON_ALLOC) {

  message(glue("  Loading {season} draft picks"))

  picks <- tryCatch(
    nflreadr::load_draft_picks(seasons = season),
    error = function(e) {
      message(glue("    Draft picks unavailable: {e$message}"))
      NULL
    }
  )

  if (is.null(picks) || nrow(picks) == 0L) {
    message(glue("    No {season} draft data -- all rookies treated as UDFA"))
    return(tibble::tibble(
      nfl_gsis_id          = character(),
      draft_round          = integer(),
      rookie_capital_mult  = numeric()
    ))
  }

  # gsis_id column varies in nflreadr drafts table
  id_col <- dplyr::case_when(
    "gsis_id" %in% names(picks)    ~ "gsis_id",
    "player_id" %in% names(picks)  ~ "player_id",
    TRUE                            ~ NA_character_
  )[1]

  if (is.na(id_col)) {
    message("    Draft picks table has no gsis_id column -- skipping")
    return(tibble::tibble(
      nfl_gsis_id          = character(),
      draft_round          = integer(),
      rookie_capital_mult  = numeric()
    ))
  }

  result <- picks %>%
    dplyr::filter(!is.na(.data[[id_col]]), nchar(.data[[id_col]]) > 0) %>%
    dplyr::transmute(
      nfl_gsis_id  = .data[[id_col]],
      draft_round  = as.integer(.data$round)
    ) %>%
    dplyr::filter(!is.na(.data$draft_round)) %>%
    dplyr::mutate(
      round_key = as.character(.data$draft_round),
      rookie_capital_mult = ROOKIE_CAPITAL_MULTIPLIERS[.data$round_key],
      rookie_capital_mult = dplyr::coalesce(.data$rookie_capital_mult, 0.55)
    ) %>%
    dplyr::select(nfl_gsis_id, draft_round, rookie_capital_mult)

  message(glue("    Loaded {nrow(result)} drafted rookies"))

  result
}

# ------------------------------------------------------------------------------
# .load_dynasty_scores
# ------------------------------------------------------------------------------

#' Load R/28 dynasty score_final per player
#'
#' Used for talent multiplier computation. Players missing from R/28 output
#' get talent_multiplier = 1.0 (neutral) downstream.
#'
#' @param path Character. Path to R/28 CSV output.
#' @return Tibble: nfl_gsis_id, position, score_final.
#' @keywords internal
.load_dynasty_scores <- function(path = DYNASTY_SCORES_CSV) {

  if (!file.exists(path)) {
    message(glue("  R/28 dynasty scores not found at: {path}"))
    message("  All talent multipliers will default to 1.0 (neutral)")
    return(tibble::tibble(
      nfl_gsis_id = character(),
      position    = character(),
      score_final = numeric()
    ))
  }

  scores <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) {
      message(glue("  R/28 read failed: {e$message}"))
      NULL
    }
  )

  if (is.null(scores) || nrow(scores) == 0L) {
    return(tibble::tibble(
      nfl_gsis_id = character(),
      position    = character(),
      score_final = numeric()
    ))
  }

  required <- c("nfl_gsis_id", "position", "score_final")
  missing_cols <- setdiff(required, names(scores))
  if (length(missing_cols) > 0L) {
    message(glue("  R/28 CSV missing columns: ",
                 "{paste(missing_cols, collapse = ', ')}"))
    return(tibble::tibble(
      nfl_gsis_id = character(),
      position    = character(),
      score_final = numeric()
    ))
  }

  result <- scores %>%
    dplyr::select(nfl_gsis_id, position, score_final) %>%
    dplyr::filter(
      !is.na(.data$nfl_gsis_id), nchar(.data$nfl_gsis_id) > 0,
      !is.na(.data$score_final)
    )

  message(glue("  Loaded {nrow(result)} R/28 dynasty scores"))

  result
}

# ------------------------------------------------------------------------------
# .compute_talent_multipliers
# ------------------------------------------------------------------------------

#' Compute talent multiplier per player from R/28 score_final
#'
#' For each position, computes the league mean and SD of score_final.
#' Each player gets talent_z = (score - mean) / sd, then
#' talent_multiplier = 1 + TALENT_MULT_SENSITIVITY * talent_z, capped at
#' [TALENT_MULT_FLOOR, TALENT_MULT_CEILING].
#'
#' Players missing from R/28 get talent_z = 0 and multiplier = 1.0.
#'
#' @param scores Tibble from .load_dynasty_scores().
#' @return Tibble: nfl_gsis_id, score_final, talent_z, talent_multiplier.
#' @keywords internal
.compute_talent_multipliers <- function(scores) {

  if (nrow(scores) == 0L) {
    return(tibble::tibble(
      nfl_gsis_id       = character(),
      score_final       = numeric(),
      talent_z          = numeric(),
      talent_multiplier = numeric()
    ))
  }

  pos_stats <- scores %>%
    dplyr::group_by(.data$position) %>%
    dplyr::summarise(
      position_mean = mean(.data$score_final, na.rm = TRUE),
      position_sd   = stats::sd(.data$score_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      position_sd = dplyr::if_else(
        is.na(.data$position_sd) | .data$position_sd <= 0,
        1.0, .data$position_sd
      )
    )

  scores %>%
    dplyr::left_join(pos_stats, by = "position") %>%
    dplyr::mutate(
      talent_z = (.data$score_final - .data$position_mean) /
                 .data$position_sd,
      talent_multiplier = 1 + TALENT_MULT_SENSITIVITY * .data$talent_z,
      talent_multiplier = pmin(
        pmax(.data$talent_multiplier, TALENT_MULT_FLOOR),
        TALENT_MULT_CEILING
      )
    ) %>%
    dplyr::select(nfl_gsis_id, score_final, talent_z, talent_multiplier)
}

# ------------------------------------------------------------------------------
# .build_player_allocation_table
# ------------------------------------------------------------------------------

#' Assemble per-player allocation inputs by joining all sources
#'
#' Joins depth chart, rookie capital, and talent multipliers into a single
#' tibble. Players without R/28 scores keep talent_multiplier = 1.0.
#' Non-rookies keep rookie_capital_mult = 1.0 (no draft capital adjustment).
#'
#' @param depth_chart Tibble from .load_2026_depth_charts().
#' @param rookies Tibble from .load_rookie_draft_capital().
#' @param talent Tibble from .compute_talent_multipliers().
#' @return Tibble with one row per player containing all allocation inputs.
#' @keywords internal
.build_player_allocation_table <- function(depth_chart, rookies, talent) {

  rookie_ids <- rookies$nfl_gsis_id

  base <- depth_chart %>%
    dplyr::mutate(
      depth_rank     = pmax(.data$depth_rank_raw, 1L),
      depth_position = .classify_depth_position(.data$position,
                                                  .data$depth_rank),
      is_rookie      = .data$nfl_gsis_id %in% rookie_ids
    )

  with_rookie <- base %>%
    dplyr::left_join(
      rookies %>% dplyr::select(nfl_gsis_id, draft_round,
                                  rookie_capital_mult),
      by = "nfl_gsis_id"
    ) %>%
    dplyr::mutate(
      # Non-rookies get 1.0 (no capital adjustment); rookies without draft
      # entry get the UDFA multiplier
      rookie_capital_mult = dplyr::case_when(
        !.data$is_rookie ~ 1.0,
        is.na(.data$rookie_capital_mult) ~ ROOKIE_CAPITAL_MULTIPLIERS[["U"]],
        TRUE ~ .data$rookie_capital_mult
      ),
      draft_round = dplyr::if_else(
        .data$is_rookie, .data$draft_round, NA_integer_
      )
    )

  with_talent <- with_rookie %>%
    dplyr::left_join(
      talent %>% dplyr::select(nfl_gsis_id, score_final, talent_z,
                                 talent_multiplier),
      by = "nfl_gsis_id"
    ) %>%
    dplyr::mutate(
      talent_multiplier = dplyr::coalesce(.data$talent_multiplier, 1.0),
      talent_z          = dplyr::coalesce(.data$talent_z, 0.0)
    )

  with_talent
}

# ------------------------------------------------------------------------------
# .compute_initial_shares
# ------------------------------------------------------------------------------

#' Compute pre-constraint target and rush shares for each player
#'
#' Applies the three-stage formula:
#'   base    = lookup TARGET_SHARE_PRIORS[depth_position]
#'   adjusted = base * rookie_capital_mult * talent_multiplier
#'
#' Same for rush shares using RUSH_SHARE_PRIORS.
#'
#' @param alloc_table Tibble from .build_player_allocation_table().
#' @return Tibble with target_share_base, target_share_adjusted,
#'   rush_share_base, rush_share_adjusted columns added.
#' @keywords internal
.compute_initial_shares <- function(alloc_table) {

  alloc_table %>%
    dplyr::mutate(
      target_share_base = dplyr::coalesce(
        TARGET_SHARE_PRIORS[.data$depth_position], 0
      ),
      rush_share_base = dplyr::coalesce(
        RUSH_SHARE_PRIORS[.data$depth_position], 0
      ),
      target_share_adjusted = .data$target_share_base *
                                .data$rookie_capital_mult *
                                .data$talent_multiplier,
      rush_share_adjusted   = .data$rush_share_base *
                                .data$rookie_capital_mult *
                                .data$talent_multiplier
    )
}

# ------------------------------------------------------------------------------
# .enforce_soft_constraints
# ------------------------------------------------------------------------------

#' Rescale per-team shares only when outside [CONSTRAINT_LOWER, CONSTRAINT_UPPER]
#'
#' For each team, sums adjusted target shares and adjusted rush shares.
#' If sum falls outside the constraint bounds, rescales all players on
#' that team proportionally so the sum equals 1.0. If within bounds,
#' leaves the shares untouched (preserves modeled signal).
#'
#' @param alloc_table Tibble with target_share_adjusted and
#'   rush_share_adjusted columns.
#' @return Same tibble with target_share and rush_share columns added
#'   (post-constraint values).
#' @keywords internal
.enforce_soft_constraints <- function(alloc_table) {

  team_sums <- alloc_table %>%
    dplyr::group_by(.data$team) %>%
    dplyr::summarise(
      team_target_sum = sum(.data$target_share_adjusted, na.rm = TRUE),
      team_rush_sum   = sum(.data$rush_share_adjusted, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      team_target_rescale = dplyr::case_when(
        .data$team_target_sum < CONSTRAINT_LOWER |
          .data$team_target_sum > CONSTRAINT_UPPER ~
          1.0 / .data$team_target_sum,
        TRUE ~ 1.0
      ),
      team_rush_rescale = dplyr::case_when(
        .data$team_rush_sum < CONSTRAINT_LOWER |
          .data$team_rush_sum > CONSTRAINT_UPPER ~
          1.0 / .data$team_rush_sum,
        TRUE ~ 1.0
      )
    )

  alloc_table %>%
    dplyr::left_join(team_sums, by = "team") %>%
    dplyr::mutate(
      target_share = .data$target_share_adjusted *
                       dplyr::coalesce(.data$team_target_rescale, 1.0),
      rush_share   = .data$rush_share_adjusted *
                       dplyr::coalesce(.data$team_rush_rescale, 1.0)
    )
}

# ------------------------------------------------------------------------------
# .apply_team_volume
# ------------------------------------------------------------------------------

#' Multiply per-player shares by R/30 team volumes to get expected PPG inputs
#'
#' Joins R/30 output by team. expected_targets_pg = target_share * pass_pg,
#' expected_carries_pg = rush_share * rush_pg.
#'
#' @param alloc_table Tibble with target_share and rush_share columns.
#' @param team_volumes Tibble from R/30 project_team_volumes().
#' @return Same tibble with expected_targets_pg and expected_carries_pg
#'   columns added.
#' @keywords internal
.apply_team_volume <- function(alloc_table, team_volumes) {

  alloc_table %>%
    dplyr::left_join(
      team_volumes %>%
        dplyr::select(team, projected_pass_pg, projected_rush_pg) %>%
        dplyr::rename(
          projected_team_pass_pg = projected_pass_pg,
          projected_team_rush_pg = projected_rush_pg
        ),
      by = "team"
    ) %>%
    dplyr::mutate(
      expected_targets_pg = .data$target_share *
                              dplyr::coalesce(
                                .data$projected_team_pass_pg, 0
                              ),
      expected_carries_pg = .data$rush_share *
                              dplyr::coalesce(
                                .data$projected_team_rush_pg, 0
                              )
    )
}

# ==============================================================================
# PUBLIC ENTRY POINT
# ==============================================================================

# ------------------------------------------------------------------------------
# .correct_rookie_depth_ranks
# ------------------------------------------------------------------------------

#' Replace 999-defaulted depth ranks for 2026 draftees using draft capital
#'
#' Sleeper's pre-season depth_chart_order for recently drafted rookies is
#' frequently NULL -- the depth chart hasn't been set yet. When the Sleeper
#' merge coalesces NULL to 999, those players sort to the bottom of their
#' position group after re-ranking and receive near-zero target/carry share.
#' For high-dynasty-score rookies (Carnell Tate, Jordyn Tyson, Kenyon Sadiq,
#' Jeremiyah Love, etc.) this produces wildly incorrect allocations.
#'
#' This function detects players where depth_was_missing is TRUE (Sleeper
#' had no depth_chart_order), cross-references the 2026 draft pick data, and
#' replaces their depth_rank_raw with a round-based default:
#'   Round 1 -> 1  (day-1 starter / immediate contributor)
#'   Round 2 -> 2
#'   Round 3 -> 3
#'   Round 4+ -> 4
#'
#' After patching, all team + position groups are re-ranked so adjacent
#' incumbents shift accordingly.
#'
#' @param depth_chart Tibble from .load_2026_depth_charts(), must include
#'   the depth_was_missing column added by .merge_sleeper_depth_overrides().
#' @param rookies Tibble from .load_rookie_draft_capital().
#' @return depth_chart with corrected depth_rank_raw and depth_was_missing
#'   column removed.
#' @keywords internal
.correct_rookie_depth_ranks <- function(depth_chart, rookies) {

  eligible <- depth_chart %>%
    dplyr::filter(dplyr::coalesce(.data$depth_was_missing, FALSE)) %>%
    dplyr::inner_join(
      rookies %>%
        dplyr::filter(!is.na(.data$draft_round)) %>%
        dplyr::select(nfl_gsis_id, draft_round),
      by = "nfl_gsis_id"
    )

  if (nrow(eligible) == 0L) {
    message("  Rookie depth rank correction: no eligible players (all have Sleeper depth data)")
    return(depth_chart %>% dplyr::select(-depth_was_missing))
  }

  rank_overrides <- eligible %>%
    dplyr::mutate(
      new_rank = dplyr::case_when(
        .data$draft_round == 1L ~ 1L,
        .data$draft_round == 2L ~ 2L,
        .data$draft_round == 3L ~ 3L,
        .data$draft_round >= 4L ~ 4L,
        TRUE                    ~ 4L
      )
    ) %>%
    dplyr::select(nfl_gsis_id, new_rank)

  patched <- depth_chart %>%
    dplyr::left_join(rank_overrides, by = "nfl_gsis_id") %>%
    dplyr::mutate(
      depth_rank_raw = dplyr::if_else(
        !is.na(.data$new_rank),
        .data$new_rank,
        .data$depth_rank_raw
      )
    ) %>%
    dplyr::select(-new_rank, -depth_was_missing) %>%
    dplyr::group_by(.data$team, .data$position) %>%
    dplyr::arrange(.data$depth_rank_raw, .data$player_name, .by_group = TRUE) %>%
    dplyr::mutate(depth_rank_raw = dplyr::row_number()) %>%
    dplyr::ungroup()

  message(glue(
    "  Rookie depth rank correction: ",
    "{nrow(eligible)} players corrected using draft capital ",
    "(R1->rank 1, R2->rank 2, R3->rank 3, R4+->rank 4)"
  ))

  patched
}



#' Allocate 2026 team volumes to individual players
#'
#' Top-level orchestrator. Pulls depth charts, draft picks, R/28 dynasty
#' scores, and R/30 team volumes. Computes depth-position-based priors,
#' adjusts for rookie draft capital and player talent, enforces soft
#' per-team constraints, and multiplies shares by team volumes to produce
#' per-player expected targets and carries per game.
#'
#' Output is the foundation for R/32 reconciliation, which will use these
#' expected volumes to correct the player projections in R/29 priors.
#'
#' @param team_volumes_path Character. Path to R/30 output RDS.
#' @param dynasty_scores_path Character. Path to R/28 output CSV.
#' @param season Integer. Target season (default SEASON_ALLOC).
#' @param save_output Logical. Write RDS + CSV outputs.
#' @return Tibble with one row per active 2026 offensive player.
#'
#' @seealso project_team_volumes (R/30)
#' @export
allocate_player_volumes <- function(team_volumes_path  = TEAM_VOLUMES_RDS,
                                      dynasty_scores_path = DYNASTY_SCORES_CSV,
                                      season             = SEASON_ALLOC,
                                      save_output        = TRUE) {

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/31: Allocating player volumes for season {season}"))
  message(glue("Soft constraint bounds: [{CONSTRAINT_LOWER}, ",
               "{CONSTRAINT_UPPER}]"))
  message(glue("{strrep('=', 70)}"))

  # STEP 1: Load R/30 team volumes
  message("\nSTEP 1/7: Loading R/30 team volumes")
  if (!file.exists(team_volumes_path)) {
    stop(glue("R/30 output not found at {team_volumes_path}. ",
              "Run project_team_volumes() first."))
  }
  team_volumes <- readRDS(team_volumes_path)
  message(glue("  Loaded {nrow(team_volumes)} team volume projections"))

  # STEP 2: Load 2026 depth charts
  message("\nSTEP 2/7: Loading depth charts")
  depth_chart <- .load_2026_depth_charts(season = season)

  # STEP 3: Load rookie draft capital
  message("\nSTEP 3/7: Loading rookie draft capital")
  rookies <- .load_rookie_draft_capital(season = season)

  # STEP 3.5: Correct depth ranks for 2026 draftees missing Sleeper depth data
  # Must run AFTER both depth chart and rookie capital are loaded. Players whose
  # Sleeper depth_chart_order was NULL (flagged as depth_was_missing) get their
  # depth_rank_raw replaced with a draft-round-based default before allocation
  # shares are computed.
  message("\nSTEP 3.5/7: Correcting rookie depth ranks via draft capital")
  depth_chart <- .correct_rookie_depth_ranks(depth_chart, rookies)

  # STEP 4: Load R/28 dynasty scores and compute talent multipliers
  message("\nSTEP 4/7: Loading dynasty scores and computing talent multipliers")
  scores <- .load_dynasty_scores(path = dynasty_scores_path)
  talent <- .compute_talent_multipliers(scores)

  # STEP 5: Build the per-player allocation table
  message("\nSTEP 5/7: Assembling per-player allocation inputs")
  alloc_table <- .build_player_allocation_table(
    depth_chart = depth_chart,
    rookies     = rookies,
    talent      = talent
  )

  # STEP 6: Compute initial shares and enforce soft constraints
  message("\nSTEP 6/7: Computing shares and enforcing soft constraints")
  alloc_with_shares <- .compute_initial_shares(alloc_table)
  alloc_constrained <- .enforce_soft_constraints(alloc_with_shares)

  # STEP 7: Apply team volume to get expected volumes per player
  message("\nSTEP 7/7: Applying team volumes to get expected per-player volumes")
  alloc_final <- .apply_team_volume(
    alloc_table   = alloc_constrained,
    team_volumes  = team_volumes
  )

  # Final output schema
  output <- alloc_final %>%
    dplyr::mutate(schema_tag = SCHEMA_TAG_ALLOC) %>%
    dplyr::select(
      nfl_gsis_id, player_name, team, position, depth_position, depth_rank,
      is_rookie, draft_round, rookie_capital_mult,
      score_final, talent_z, talent_multiplier,
      target_share_base, target_share_adjusted, target_share,
      rush_share_base, rush_share_adjusted, rush_share,
      projected_team_pass_pg, projected_team_rush_pg,
      expected_targets_pg, expected_carries_pg,
      schema_tag
    ) %>%
    dplyr::arrange(team, position, depth_rank)

  # Save outputs
  if (save_output) {
    dir.create(dirname(OUTPUT_RDS_PATH_ALLOC), recursive = TRUE,
               showWarnings = FALSE)
    saveRDS(output, OUTPUT_RDS_PATH_ALLOC)
    readr::write_csv(output, OUTPUT_CSV_PATH_ALLOC)
    message(glue("\n  Saved: {OUTPUT_RDS_PATH_ALLOC}"))
    message(glue("  Saved: {OUTPUT_CSV_PATH_ALLOC}"))
  }

  # KEY INSIGHTS (computed from data, never hardcoded)
  n_players <- nrow(output)
  n_rookies <- sum(output$is_rookie, na.rm = TRUE)
  n_with_scores <- sum(!is.na(output$score_final), na.rm = TRUE)
  n_teams <- dplyr::n_distinct(output$team)

  # Soft constraint diagnostics: how many teams were rescaled?
  team_check <- output %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      target_sum_raw = sum(target_share_adjusted, na.rm = TRUE),
      rush_sum_raw   = sum(rush_share_adjusted, na.rm = TRUE),
      .groups        = "drop"
    )
  n_target_rescaled <- sum(
    team_check$target_sum_raw < CONSTRAINT_LOWER |
      team_check$target_sum_raw > CONSTRAINT_UPPER,
    na.rm = TRUE
  )
  n_rush_rescaled <- sum(
    team_check$rush_sum_raw < CONSTRAINT_LOWER |
      team_check$rush_sum_raw > CONSTRAINT_UPPER,
    na.rm = TRUE
  )

  # Top expected targets and carries
  top_target_player <- output$player_name[which.max(output$expected_targets_pg)]
  top_target_team   <- output$team[which.max(output$expected_targets_pg)]
  top_target_pg     <- max(output$expected_targets_pg, na.rm = TRUE)
  top_carry_player  <- output$player_name[which.max(output$expected_carries_pg)]
  top_carry_team    <- output$team[which.max(output$expected_carries_pg)]
  top_carry_pg      <- max(output$expected_carries_pg, na.rm = TRUE)

  message(glue("\n{strrep('=', 70)}"))
  message("KEY INSIGHTS")
  message(glue("{strrep('=', 70)}"))
  message(glue("  Players allocated:        {n_players}"))
  message(glue("  Active teams covered:     {n_teams}"))
  message(glue("  Rookies in pool:          {n_rookies}"))
  message(glue("  Players with R/28 score:  {n_with_scores}"))
  message(glue("  Teams rescaled (targets): {n_target_rescaled} of {n_teams}"))
  message(glue("  Teams rescaled (rushes):  {n_rush_rescaled} of {n_teams}"))
  message(glue("  Highest expected targets: {top_target_player} ",
               "({top_target_team}) at ",
               "{format(round(top_target_pg, 1), nsmall = 1)} per game"))
  message(glue("  Highest expected carries: {top_carry_player} ",
               "({top_carry_team}) at ",
               "{format(round(top_carry_pg, 1), nsmall = 1)} per game"))
  message(glue("{strrep('=', 70)}\n"))

  output
}
