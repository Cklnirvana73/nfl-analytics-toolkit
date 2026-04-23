# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 7
# CFB Player-Season Panel Construction
# File: R/21_cfb_player_season_panel.R
#
# Purpose: Aggregate 12 seasons of cfbfastR FBS play-by-play data (2014-2025)
#          to a player-season panel with one row per player per season.
#          Produces college player profiles that feed the Phase 3
#          college-to-NFL translation model (Week 10). Mirrors the structural
#          pattern of R/16_player_season_panel.R with CFB-appropriate
#          modifications documented below.
#
# Key CFB vs NFL panel differences:
#   - No stable player ID: cfbfastR PBP does not carry a numeric player ID
#     equivalent to nflfastR's GSIS ID. Join key is player_name + season.
#     Name collisions (two different players with the same name) are flagged
#     via has_name_collision. Analyst review is required for flagged rows
#     before any individual player comparison across data sources.
#   - No roster anchor: Position is inferred from play-level role distribution
#     (share of plays as passer / rusher / receiver) using threshold rules.
#     This is a documented heuristic; there is no nflreadr equivalent.
#   - Garbage time more severe: CFB blowout margins are wider. Filter is
#     applied to periods 3+ using |score_diff| > 28 as primary criterion,
#     with wp_before outside [0.05, 0.95] as secondary when available.
#   - EPA column is uppercase "EPA" (cfbfastR native). The lowercase alias
#     "epa" exists in the normalized cache but EPA is the correct column.
#   - season_type == "regular" denotes regular season (not "REG" as in NFL).
#   - Counting stats default to 0L (not NA) for players who did not
#     accumulate them. Efficiency per-play stats use NA when denominator = 0.
#
# Navigation:
#   Line  ~90 : Constants and configuration
#   Line ~165 : NSE declarations
#   Line ~185 : Internal helpers (5 functions):
#                 .apply_cfb_garbage_time_filter()
#                 .build_cfb_passing_stats()
#                 .build_cfb_rushing_stats()
#                 .build_cfb_receiving_stats()
#                 .build_cfb_success_stats()
#                 .build_cfb_games_played()
#   Line ~520 : build_cfb_player_season_panel()
#   Line ~790 : classify_cfb_player_position()
#   Line ~870 : validate_cfb_panel_integrity()
#
# Dependencies: cfbfastR, dplyr, tibble, glue, here
#               Sources R/20_multi_season_cfb_pbp.R
# Schema tag: s2cfbv1_panel
# ==============================================================================

library(cfbfastR)
library(dplyr)
library(tibble)
library(glue)
library(here)

# Source Week 6 functions. load_normalized_cfb_season() must be available
# before any panel-building code runs. Guard prevents double-sourcing.
if (!exists("load_normalized_cfb_season", mode = "function")) {
  week6_path <- here::here("R", "20_multi_season_cfb_pbp.R")
  if (!file.exists(week6_path)) {
    stop(glue(
      "R/20_multi_season_cfb_pbp.R not found at: {week6_path}\n",
      "This file is required for load_normalized_cfb_season()."
    ), call. = FALSE)
  }
  source(week6_path)
}


# ==============================================================================
# CONSTANTS
# ==============================================================================

# Season range: matches Week 6 CFB cache (2014-2025).
# CFB data floor is 2014L -- EPA coverage and schema consistency are
# unreliable before 2014 in cfbfastR. Never set seasons below 2014L.
CFB_PANEL_SEASONS_DEFAULT <- 2014:2025

# Default cache directory: must match Week 6 cache location exactly.
CFB_PANEL_CACHE_DIR_DEFAULT <- here::here("data", "season2_cfb_cache")

# Minimum plays for low_volume flag. Players below threshold are retained
# in the panel (not dropped) but flagged. Downstream models apply their
# own cutoffs per analytical need.
CFB_PANEL_MIN_PLAYS <- 10L

# Schema version tag for the panel. Kept separate from CFB_SCHEMA_NORM_VERSION
# (which tags the PBP cache) to allow independent evolution of each layer.
CFB_PANEL_VERSION <- "s2cfbv1_panel"

# Garbage time filter defaults.
# Applied to periods 3+ only (first half always retained).
#   Primary criterion: |score_diff| > CFB_GARBAGE_TIME_SCORE_DIFF
#   Secondary criterion: wp_before outside [LO, HI] when wp_before non-NA
CFB_GARBAGE_TIME_SCORE_DIFF <- 28L
CFB_GARBAGE_TIME_WP_LO      <- 0.05
CFB_GARBAGE_TIME_WP_HI      <- 0.95

# Position classification thresholds (heuristic; no roster anchor available).
# Precedence order: QB > RB > WR_TE > other.
#   QB:    pass_attempts / total_role_plays >= 0.70
#   RB:    rush_attempts / total_role_plays >= 0.70
#   WR_TE: targets       / total_role_plays >= 0.50
#   other: none of the above
#   NA:    total_role_plays == 0
CFB_POSITION_QB_THRESHOLD   <- 0.70
CFB_POSITION_RB_THRESHOLD   <- 0.70
CFB_POSITION_WR_THRESHOLD   <- 0.50

# Play type sets used across aggregation helpers.
# Pass attempt: all plays attributed to the passer (includes sacks and
#   interceptions). Sack and interception yardage excluded from passing_yards.
CFB_PASS_ATTEMPT_TYPES <- c(
  "Pass Completion", "Pass Incompletion", "Passing Touchdown",
  "Pass Reception",                         # alternate coding in some seasons
  "Sack",
  "Interception", "Interception Return", "Interception Return Touchdown"
)

# Completion types (passer and receiver perspective).
CFB_COMPLETION_TYPES <- c(
  "Pass Completion", "Pass Reception", "Passing Touchdown"
)

# Interception types (used to exclude from passing_yards).
CFB_INTERCEPTION_TYPES <- c(
  "Interception", "Interception Return", "Interception Return Touchdown"
)

# Rush types: only plays coded as rushes by cfbfastR. Sacks are play_type
# "Sack" in cfbfastR PBP, not "Rush", so they are excluded automatically.
CFB_RUSH_TYPES <- c("Rush", "Rushing Touchdown")

# Scrimmage play types: used for games played and EPA success rate aggregation.
CFB_SCRIMMAGE_TYPES <- c(
  "Rush", "Rushing Touchdown",
  "Pass Completion", "Pass Incompletion", "Passing Touchdown",
  "Pass Reception", "Sack",
  "Interception", "Interception Return", "Interception Return Touchdown",
  "Fumble Recovery (Opponent)", "Fumble Recovery (Own)"
)

# Name collision heuristic: player_name on >= 3 distinct teams in the same
# season is more likely a name collision than a transfer. True transfers
# rarely play for 3+ programs in a single season.
CFB_NAME_COLLISION_TEAM_THRESHOLD <- 3L


# ==============================================================================
# NSE COLUMN DECLARATIONS (suppress R CMD check "no visible binding" notes)
# ==============================================================================

utils::globalVariables(c(
  "player_name", "pos_team", "EPA", "yards_gained", "play_type", "game_id",
  "passer_player_name", "rusher_player_name", "receiver_player_name",
  "season_type", "score_diff", "wp_before", "period",
  "n_positive_epa", "n_epa_plays", "n_teams", "plays_this_team",
  "pass_attempts", "completions", "passing_yards", "pass_tds", "interceptions",
  "pass_epa", "rush_attempts", "rushing_yards", "rush_tds", "rush_epa",
  "targets", "receptions", "receiving_yards", "rec_tds", "rec_epa",
  "games_played", "total_plays", "position_group",
  "pass_attempts_int", "rush_attempts_int", "targets_int",
  "total_role_plays", "pass_pct", "rush_pct", "recv_pct",
  "primary_team", "has_name_collision", "low_volume", "completion_pct",
  "pass_epa_per_attempt", "rush_epa_per_attempt", "catch_rate",
  "rec_epa_per_target", "success_rate", "garbage_time_plays_excluded",
  "panel_version"
))


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

#' Apply CFB Garbage Time Filter (Internal Helper)
#'
#' Removes plays in periods 3+ that satisfy at least one garbage time
#' criterion. First-half plays (periods 1-2) are always retained.
#'
#' Primary criterion (always applied when score_diff column exists):
#'   |score_diff| > score_diff_threshold
#' Secondary criterion (applied only where wp_before is non-NA):
#'   wp_before < wp_lo OR wp_before > wp_hi
#'
#' @param pbp Normalized CFB play-by-play tibble (already FBS-filtered).
#' @param score_diff_threshold Integer. Score difference threshold.
#' @param wp_lo Numeric. Lower win probability bound.
#' @param wp_hi Numeric. Upper win probability bound.
#' @return Filtered tibble. Returned unchanged if pbp has 0 rows or lacks
#'   the period column.
.apply_cfb_garbage_time_filter <- function(pbp,
                                            score_diff_threshold,
                                            wp_lo,
                                            wp_hi) {
  if (nrow(pbp) == 0L || !"period" %in% names(pbp)) return(pbp)

  is_late <- pbp$period >= 3L

  # Score differential criterion (primary). Default to 0 if column absent
  # so that is_blowout_score is always FALSE when score_diff is unavailable.
  if ("score_diff" %in% names(pbp)) {
    score_diff_col <- pbp$score_diff
  } else {
    score_diff_col <- rep(0L, nrow(pbp))
  }
  is_blowout_score <- is_late & !is.na(score_diff_col) &
    (abs(score_diff_col) > score_diff_threshold)

  # Win probability criterion (secondary; only where wp_before non-NA).
  if ("wp_before" %in% names(pbp)) {
    wp_col        <- pbp$wp_before
    wp_available  <- !is.na(wp_col)
    is_blowout_wp <- is_late & wp_available &
      (wp_col < wp_lo | wp_col > wp_hi)
  } else {
    is_blowout_wp <- rep(FALSE, nrow(pbp))
  }

  is_garbage <- is_blowout_score | is_blowout_wp
  pbp[!is_garbage, , drop = FALSE]
}


#' Aggregate Passing Statistics per Player-Team (Internal Helper)
#'
#' Filters plays where passer_player_name is non-NA and play_type is a
#' CFB pass attempt type, then aggregates to (player_name, pos_team).
#'
#' Sack and interception yardage are excluded from passing_yards per
#' standard convention. EPA is summed for all pass-attempt plays including
#' sacks and interceptions (those plays affect passer EPA).
#'
#' @param pbp Garbage-time-filtered, season-type-filtered PBP tibble.
#' @return Tibble with one row per passer-team combination.
.build_cfb_passing_stats <- function(pbp) {

  empty_result <- tibble::tibble(
    player_name   = character(0),
    pos_team      = character(0),
    pass_attempts = integer(0),
    completions   = integer(0),
    passing_yards = integer(0),
    pass_tds      = integer(0),
    interceptions = integer(0),
    pass_epa      = double(0)
  )

  if (nrow(pbp) == 0L) return(empty_result)
  if (!"passer_player_name" %in% names(pbp)) return(empty_result)

  pbp_pass <- pbp[
    !is.na(pbp$passer_player_name) &
      pbp$play_type %in% CFB_PASS_ATTEMPT_TYPES,
  ]

  if (nrow(pbp_pass) == 0L) return(empty_result)

  # Rename for clean group_by usage below.
  pbp_pass$player_name <- pbp_pass$passer_player_name

  pbp_pass %>%
    dplyr::group_by(player_name, pos_team) %>%
    dplyr::summarise(
      pass_attempts = dplyr::n(),
      completions   = as.integer(sum(
        play_type %in% CFB_COMPLETION_TYPES, na.rm = TRUE
      )),
      # Sack and interception yards excluded from passing_yards.
      passing_yards = as.integer(sum(
        dplyr::if_else(
          play_type %in% c("Sack", CFB_INTERCEPTION_TYPES),
          0L,
          as.integer(dplyr::coalesce(yards_gained, 0L))
        ),
        na.rm = TRUE
      )),
      pass_tds      = as.integer(sum(
        play_type == "Passing Touchdown", na.rm = TRUE
      )),
      interceptions = as.integer(sum(
        play_type %in% CFB_INTERCEPTION_TYPES, na.rm = TRUE
      )),
      pass_epa      = sum(dplyr::coalesce(EPA, 0), na.rm = TRUE),
      .groups = "drop"
    )
}


#' Aggregate Rushing Statistics per Player-Team (Internal Helper)
#'
#' Filters plays where rusher_player_name is non-NA and play_type is in
#' CFB_RUSH_TYPES, then aggregates to (player_name, pos_team).
#'
#' Sack plays are play_type "Sack" in cfbfastR, not "Rush", so they are
#' excluded automatically by the play_type filter. No additional sack
#' guard is needed.
#'
#' @param pbp Filtered PBP tibble.
#' @return Tibble with one row per rusher-team combination.
.build_cfb_rushing_stats <- function(pbp) {

  empty_result <- tibble::tibble(
    player_name   = character(0),
    pos_team      = character(0),
    rush_attempts = integer(0),
    rushing_yards = integer(0),
    rush_tds      = integer(0),
    rush_epa      = double(0)
  )

  if (nrow(pbp) == 0L) return(empty_result)
  if (!"rusher_player_name" %in% names(pbp)) return(empty_result)

  pbp_rush <- pbp[
    !is.na(pbp$rusher_player_name) &
      pbp$play_type %in% CFB_RUSH_TYPES,
  ]

  if (nrow(pbp_rush) == 0L) return(empty_result)

  pbp_rush$player_name <- pbp_rush$rusher_player_name

  pbp_rush %>%
    dplyr::group_by(player_name, pos_team) %>%
    dplyr::summarise(
      rush_attempts = dplyr::n(),
      rushing_yards = as.integer(sum(
        dplyr::coalesce(yards_gained, 0L), na.rm = TRUE
      )),
      rush_tds      = as.integer(sum(
        play_type == "Rushing Touchdown", na.rm = TRUE
      )),
      rush_epa      = sum(dplyr::coalesce(EPA, 0), na.rm = TRUE),
      .groups = "drop"
    )
}


#' Aggregate Receiving Statistics per Player-Team (Internal Helper)
#'
#' Targets are defined as any play where receiver_player_name is non-NA on a
#' pass play type. In cfbfastR PBP, receiver_player_name is reliably
#' populated on completions and Passing Touchdowns. Population on
#' Pass Incompletion is inconsistent across seasons and data sources.
#' As a result, targets in this panel may be under-counted relative to
#' official box-score targets. This limitation is documented in the
#' build_cfb_player_season_panel() return value description.
#'
#' Receiving yards and rec_epa are computed only on completion plays.
#' EPA on incompletions where the receiver is named is excluded from
#' rec_epa because that EPA is attributed to the passer in the pass stats.
#'
#' @param pbp Filtered PBP tibble.
#' @return Tibble with one row per receiver-team combination.
.build_cfb_receiving_stats <- function(pbp) {

  empty_result <- tibble::tibble(
    player_name     = character(0),
    pos_team        = character(0),
    targets         = integer(0),
    receptions      = integer(0),
    receiving_yards = integer(0),
    rec_tds         = integer(0),
    rec_epa         = double(0)
  )

  if (nrow(pbp) == 0L) return(empty_result)
  if (!"receiver_player_name" %in% names(pbp)) return(empty_result)

  TARGET_PLAY_TYPES <- c(CFB_COMPLETION_TYPES, "Pass Incompletion")

  pbp_recv <- pbp[
    !is.na(pbp$receiver_player_name) &
      pbp$play_type %in% TARGET_PLAY_TYPES,
  ]

  if (nrow(pbp_recv) == 0L) return(empty_result)

  pbp_recv$player_name <- pbp_recv$receiver_player_name

  pbp_recv %>%
    dplyr::group_by(player_name, pos_team) %>%
    dplyr::summarise(
      targets         = dplyr::n(),
      receptions      = as.integer(sum(
        play_type %in% CFB_COMPLETION_TYPES, na.rm = TRUE
      )),
      # Yards and EPA only on completions.
      receiving_yards = as.integer(sum(
        dplyr::if_else(
          play_type %in% CFB_COMPLETION_TYPES,
          as.integer(dplyr::coalesce(yards_gained, 0L)),
          0L
        ),
        na.rm = TRUE
      )),
      rec_tds         = as.integer(sum(
        play_type == "Passing Touchdown", na.rm = TRUE
      )),
      rec_epa         = sum(
        dplyr::if_else(
          play_type %in% CFB_COMPLETION_TYPES,
          dplyr::coalesce(EPA, 0),
          0
        ),
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}


#' Aggregate EPA Success Stats per Player-Team (Internal Helper)
#'
#' Returns n_positive_epa and n_epa_plays per (player_name, pos_team) across
#' all three roles (passer, rusher, receiver on completions). These raw
#' numerator and denominator counts are summed during the multi-team collapse
#' and converted to success_rate = n_positive_epa / n_epa_plays.
#'
#' Computing from numerator + denominator rather than averaging percentages
#' gives the exact rate for players who appeared on multiple teams.
#'
#' @param pbp Filtered PBP tibble.
#' @return Tibble with one row per player-team combination.
.build_cfb_success_stats <- function(pbp) {

  empty_result <- tibble::tibble(
    player_name    = character(0),
    pos_team       = character(0),
    n_positive_epa = integer(0),
    n_epa_plays    = integer(0)
  )

  if (nrow(pbp) == 0L) return(empty_result)

  pbp_scrimmage <- pbp[
    pbp$play_type %in% CFB_SCRIMMAGE_TYPES & !is.na(pbp$EPA),
  ]

  if (nrow(pbp_scrimmage) == 0L) return(empty_result)

  rows_list <- list()

  # Passer plays.
  if ("passer_player_name" %in% names(pbp_scrimmage)) {
    rp <- pbp_scrimmage[
      !is.na(pbp_scrimmage$passer_player_name) &
        pbp_scrimmage$play_type %in% CFB_PASS_ATTEMPT_TYPES,
      c("passer_player_name", "pos_team", "EPA"),
      drop = FALSE
    ]
    if (nrow(rp) > 0L) {
      names(rp)[names(rp) == "passer_player_name"] <- "player_name"
      rows_list[["passer"]] <- rp
    }
  }

  # Rusher plays.
  if ("rusher_player_name" %in% names(pbp_scrimmage)) {
    rr <- pbp_scrimmage[
      !is.na(pbp_scrimmage$rusher_player_name) &
        pbp_scrimmage$play_type %in% CFB_RUSH_TYPES,
      c("rusher_player_name", "pos_team", "EPA"),
      drop = FALSE
    ]
    if (nrow(rr) > 0L) {
      names(rr)[names(rr) == "rusher_player_name"] <- "player_name"
      rows_list[["rusher"]] <- rr
    }
  }

  # Receiver plays (completions only to avoid double-counting EPA).
  if ("receiver_player_name" %in% names(pbp_scrimmage)) {
    rv <- pbp_scrimmage[
      !is.na(pbp_scrimmage$receiver_player_name) &
        pbp_scrimmage$play_type %in% CFB_COMPLETION_TYPES,
      c("receiver_player_name", "pos_team", "EPA"),
      drop = FALSE
    ]
    if (nrow(rv) > 0L) {
      names(rv)[names(rv) == "receiver_player_name"] <- "player_name"
      rows_list[["receiver"]] <- rv
    }
  }

  if (length(rows_list) == 0L) return(empty_result)

  all_plays <- do.call(rbind, rows_list)
  rownames(all_plays) <- NULL

  all_plays %>%
    dplyr::group_by(player_name, pos_team) %>%
    dplyr::summarise(
      n_positive_epa = as.integer(sum(EPA > 0, na.rm = TRUE)),
      n_epa_plays    = as.integer(sum(!is.na(EPA))),
      .groups = "drop"
    )
}


#' Aggregate Games Played per Player-Team (Internal Helper)
#'
#' Counts distinct game_id values per (player_name, pos_team) across all
#' three roles. A player who appeared in a game as both passer and rusher
#' is counted once for that game.
#'
#' @param pbp Filtered PBP tibble.
#' @return Tibble with one row per player-team combination.
.build_cfb_games_played <- function(pbp) {

  empty_result <- tibble::tibble(
    player_name  = character(0),
    pos_team     = character(0),
    games_played = integer(0)
  )

  if (nrow(pbp) == 0L || !"game_id" %in% names(pbp)) return(empty_result)

  pbp_scrimmage <- pbp[pbp$play_type %in% CFB_SCRIMMAGE_TYPES, ]
  if (nrow(pbp_scrimmage) == 0L) return(empty_result)

  rows_list <- list()

  if ("passer_player_name" %in% names(pbp_scrimmage)) {
    rp <- unique(pbp_scrimmage[
      !is.na(pbp_scrimmage$passer_player_name) &
        pbp_scrimmage$play_type %in% CFB_PASS_ATTEMPT_TYPES,
      c("passer_player_name", "pos_team", "game_id"),
      drop = FALSE
    ])
    if (nrow(rp) > 0L) {
      names(rp)[names(rp) == "passer_player_name"] <- "player_name"
      rows_list[["passer"]] <- rp
    }
  }

  if ("rusher_player_name" %in% names(pbp_scrimmage)) {
    rr <- unique(pbp_scrimmage[
      !is.na(pbp_scrimmage$rusher_player_name) &
        pbp_scrimmage$play_type %in% CFB_RUSH_TYPES,
      c("rusher_player_name", "pos_team", "game_id"),
      drop = FALSE
    ])
    if (nrow(rr) > 0L) {
      names(rr)[names(rr) == "rusher_player_name"] <- "player_name"
      rows_list[["rusher"]] <- rr
    }
  }

  if ("receiver_player_name" %in% names(pbp_scrimmage)) {
    rv <- unique(pbp_scrimmage[
      !is.na(pbp_scrimmage$receiver_player_name) &
        pbp_scrimmage$play_type %in% CFB_COMPLETION_TYPES,
      c("receiver_player_name", "pos_team", "game_id"),
      drop = FALSE
    ])
    if (nrow(rv) > 0L) {
      names(rv)[names(rv) == "receiver_player_name"] <- "player_name"
      rows_list[["receiver"]] <- rv
    }
  }

  if (length(rows_list) == 0L) return(empty_result)

  all_games <- unique(do.call(rbind, rows_list))
  rownames(all_games) <- NULL

  all_games %>%
    dplyr::group_by(player_name, pos_team) %>%
    dplyr::summarise(
      games_played = dplyr::n_distinct(game_id),
      .groups = "drop"
    )
}


# ==============================================================================
# FUNCTION: build_cfb_player_season_panel
# ==============================================================================

#' Build CFB Player-Season Panel
#'
#' @description
#' Aggregates 12 seasons of cfbfastR FBS play-by-play data to a player-season
#' panel with one row per player per season. Produces the college player
#' profiles required by the Phase 3 college-to-NFL translation model (Week 10).
#'
#' \strong{No stable player ID.} cfbfastR PBP does not carry a numeric player
#' ID equivalent to nflfastR's GSIS ID. The join key is
#' \code{player_name + season}. \code{has_name_collision} flags rows where
#' the same player_name appeared on three or more distinct teams in one season,
#' which is more likely a name collision than a transfer. All individual player
#' comparisons across data sources must verify via position_group and team.
#'
#' \strong{Position is a heuristic.} Position group is inferred from the share
#' of a player's plays in each role (passer / rusher / receiver) using the
#' threshold rules in the constants section. There is no roster anchor
#' equivalent to nflreadr. Position error rates are higher than in the NFL
#' panel (R/16) and must be disclosed in downstream analysis.
#'
#' \strong{Counting stats default to 0, not NA.} A player with no passing
#' activity has \code{pass_attempts = 0L}, not \code{NA}. Efficiency columns
#' (\code{pass_epa_per_attempt}, etc.) use \code{NA_real_} when the
#' denominator is 0 to correctly signal "undefined" vs "zero".
#'
#' \strong{Targets are under-counted.} cfbfastR does not consistently populate
#' \code{receiver_player_name} on Pass Incompletion plays. Targets in this
#' panel count named-receiver plays only and will be lower than official
#' box-score target totals. Do not compare to NFL target share without
#' acknowledging this limitation.
#'
#' @param seasons Integer vector of seasons to process. Must be >= 2014.
#'   Default: 2014:2025 (all 12 cached seasons).
#' @param cache_dir Character path to the Week 6 CFB cache directory.
#' @param min_plays Integer. Total plays threshold for low_volume flag.
#'   Default: 10L.
#' @param include_postseason Logical. If FALSE (default), only regular-season
#'   plays (\code{season_type == "regular"}) are included.
#' @param garbage_time_filter Logical. If TRUE (default), apply garbage time
#'   filter to periods 3+ before aggregation.
#' @param garbage_score_diff Integer. Primary garbage time threshold:
#'   |score_diff| above this value in period 3+ triggers exclusion.
#' @param garbage_wp_lo Numeric. Secondary garbage time lower WP bound.
#' @param garbage_wp_hi Numeric. Secondary garbage time upper WP bound.
#' @param verbose Logical. Print season-by-season progress. Default: TRUE.
#'
#' @return A tibble with one row per player-season. Columns:
#'   \describe{
#'     \item{player_name}{chr: Player name from cfbfastR PBP. Primary key
#'       component. Verify with position_group and primary_team before use.}
#'     \item{season}{int: Season year.}
#'     \item{primary_team}{chr: Team where player had the most plays this
#'       season. For most players this is their only team.}
#'     \item{n_teams}{int: Distinct teams played for this season. > 1
#'       indicates transfer or possible name collision.}
#'     \item{has_name_collision}{lgl: TRUE if n_teams >=
#'       CFB_NAME_COLLISION_TEAM_THRESHOLD (3). Heuristic flag -- analyst
#'       review recommended before using these rows.}
#'     \item{position_group}{chr: QB / RB / WR_TE / other / NA. Heuristic
#'       from play role distribution. See classify_cfb_player_position().}
#'     \item{games_played}{int: Distinct games as passer, rusher, or receiver.}
#'     \item{total_plays}{int: pass_attempts + rush_attempts + targets.}
#'     \item{low_volume}{lgl: total_plays < min_plays.}
#'     \item{pass_attempts}{int: Pass attempts including sacks and INTs.}
#'     \item{completions}{int: Completed passes.}
#'     \item{completion_pct}{dbl: completions / pass_attempts. NA if 0.}
#'     \item{passing_yards}{int: Yards on completions only (sack and INT
#'       yardage excluded).}
#'     \item{pass_tds}{int: Passing touchdowns.}
#'     \item{interceptions}{int: Interceptions thrown.}
#'     \item{pass_epa}{dbl: Summed EPA as passer (all pass attempt types).}
#'     \item{pass_epa_per_attempt}{dbl: pass_epa / pass_attempts. NA if 0.}
#'     \item{rush_attempts}{int: Rush attempts (sacks excluded).}
#'     \item{rushing_yards}{int: Rushing yards.}
#'     \item{rush_tds}{int: Rushing touchdowns.}
#'     \item{rush_epa}{dbl: Summed EPA as rusher.}
#'     \item{rush_epa_per_attempt}{dbl: rush_epa / rush_attempts. NA if 0.}
#'     \item{targets}{int: Named-receiver plays on pass plays. See note
#'       above re: under-counting on incompletions.}
#'     \item{receptions}{int: Completions caught.}
#'     \item{catch_rate}{dbl: receptions / targets. NA if 0 targets.}
#'     \item{receiving_yards}{int: Receiving yards on completions.}
#'     \item{rec_tds}{int: Receiving touchdowns.}
#'     \item{rec_epa}{dbl: Summed EPA as receiver on completions.}
#'     \item{rec_epa_per_target}{dbl: rec_epa / targets. NA if 0 targets.}
#'     \item{success_rate}{dbl: Proportion of plays (all roles) with EPA > 0.
#'       NA if no plays with non-NA EPA.}
#'     \item{garbage_time_plays_excluded}{int: Plays excluded by garbage time
#'       filter this season. 0 when garbage_time_filter = FALSE.}
#'     \item{panel_version}{chr: Schema tag "s2cfbv1_panel".}
#'   }
#'
#' @examples
#' # Full 12-season panel
#' panel <- build_cfb_player_season_panel()
#'
#' # Single season for development
#' panel_2024 <- build_cfb_player_season_panel(seasons = 2024L)
#'
#' # Include bowl and playoff games
#' panel_full <- build_cfb_player_season_panel(include_postseason = TRUE)
#'
#' # Receivers with volume
#' receivers <- panel %>%
#'   filter(position_group == "WR_TE", !low_volume, season >= 2020) %>%
#'   arrange(desc(rec_epa_per_target))
#'
#' @seealso classify_cfb_player_position, validate_cfb_panel_integrity,
#'   load_normalized_cfb_season
#' @export
build_cfb_player_season_panel <- function(
    seasons             = CFB_PANEL_SEASONS_DEFAULT,
    cache_dir           = CFB_PANEL_CACHE_DIR_DEFAULT,
    min_plays           = CFB_PANEL_MIN_PLAYS,
    include_postseason  = FALSE,
    garbage_time_filter = TRUE,
    garbage_score_diff  = CFB_GARBAGE_TIME_SCORE_DIFF,
    garbage_wp_lo       = CFB_GARBAGE_TIME_WP_LO,
    garbage_wp_hi       = CFB_GARBAGE_TIME_WP_HI,
    verbose             = TRUE
) {

  # --- Input validation ---
  if (!is.numeric(seasons) || length(seasons) == 0L) {
    stop("'seasons' must be a non-empty numeric vector.", call. = FALSE)
  }
  seasons <- sort(as.integer(seasons))

  bad_seasons <- seasons[seasons < 2014L]
  if (length(bad_seasons) > 0L) {
    stop(glue(
      "CFB data floor is 2014. Remove season(s): ",
      "{paste(bad_seasons, collapse = ', ')}."
    ), call. = FALSE)
  }

  if (!dir.exists(cache_dir)) {
    stop(glue(
      "cache_dir not found: {cache_dir}\n",
      "Run load_multi_season_cfb_pbp() from R/20_multi_season_cfb_pbp.R first."
    ), call. = FALSE)
  }

  if (!is.numeric(min_plays) || length(min_plays) != 1L || min_plays < 0L) {
    stop("'min_plays' must be a single non-negative number.", call. = FALSE)
  }

  if (verbose) {
    message(glue(
      "build_cfb_player_season_panel(): ",
      "{length(seasons)} season(s) [{min(seasons)}-{max(seasons)}]"
    ))
    if (!include_postseason) {
      message("  Season type : regular season only (season_type == 'regular')")
    } else {
      message("  Season type : regular + postseason")
    }
    if (garbage_time_filter) {
      message(glue(
        "  Garbage filter : ON  |score_diff| > {garbage_score_diff}; ",
        "WP outside [{garbage_wp_lo}, {garbage_wp_hi}]"
      ))
    } else {
      message("  Garbage filter : OFF")
    }
  }

  # Pre-allocate result list to avoid repeated copying.
  season_results <- vector("list", length(seasons))

  for (i in seq_along(seasons)) {
    s <- seasons[i]

    if (verbose) message(glue("  Season {s}: loading FBS plays..."))

    pbp <- tryCatch(
      load_normalized_cfb_season(
        season    = s,
        cache_dir = cache_dir,
        division  = "fbs"
      ),
      error = function(e) {
        warning(
          glue("Season {s}: load failed -- {conditionMessage(e)}. Skipping."),
          call. = FALSE
        )
        NULL
      }
    )

    if (is.null(pbp) || nrow(pbp) == 0L) {
      if (verbose) message(glue("  Season {s}: skipped (no data returned)."))
      next
    }

    # --- Season type filter ---
    if (!include_postseason && "season_type" %in% names(pbp)) {
      n_before <- nrow(pbp)
      pbp      <- pbp[pbp$season_type == "regular", , drop = FALSE]
      if (verbose && nrow(pbp) < n_before) {
        message(glue(
          "  Season {s}: {format(n_before - nrow(pbp), big.mark = ',')} ",
          "postseason plays excluded."
        ))
      }
    }

    if (nrow(pbp) == 0L) {
      if (verbose) message(glue("  Season {s}: no regular season plays. Skipping."))
      rm(pbp); gc(verbose = FALSE)
      next
    }

    # --- Garbage time filter ---
    n_pre_garbage <- nrow(pbp)
    if (garbage_time_filter) {
      pbp <- .apply_cfb_garbage_time_filter(
        pbp, garbage_score_diff, garbage_wp_lo, garbage_wp_hi
      )
    }
    n_plays_excluded <- n_pre_garbage - nrow(pbp)

    if (verbose) {
      message(glue(
        "  Season {s}: {format(nrow(pbp), big.mark = ',')} plays ",
        "({format(n_plays_excluded, big.mark = ',')} garbage time removed)"
      ))
    }

    # --- Per-role aggregations at player_name + pos_team level ---
    pass_stats  <- .build_cfb_passing_stats(pbp)
    rush_stats  <- .build_cfb_rushing_stats(pbp)
    recv_stats  <- .build_cfb_receiving_stats(pbp)
    succ_stats  <- .build_cfb_success_stats(pbp)
    games_stats <- .build_cfb_games_played(pbp)

    rm(pbp)
    gc(verbose = FALSE)

    # --- Collect all (player_name, pos_team) combinations ---
    name_team_list <- list()
    if (nrow(pass_stats) > 0L) {
      name_team_list[["pass"]] <- pass_stats[, c("player_name", "pos_team")]
    }
    if (nrow(rush_stats) > 0L) {
      name_team_list[["rush"]] <- rush_stats[, c("player_name", "pos_team")]
    }
    if (nrow(recv_stats) > 0L) {
      name_team_list[["recv"]] <- recv_stats[, c("player_name", "pos_team")]
    }

    if (length(name_team_list) == 0L) {
      if (verbose) message(glue("  Season {s}: no player stats found. Skipping."))
      rm(pass_stats, rush_stats, recv_stats, succ_stats, games_stats)
      gc(verbose = FALSE)
      next
    }

    all_combos <- unique(do.call(rbind, name_team_list))
    rownames(all_combos) <- NULL

    # --- Join all stat tables at (player_name, pos_team) level ---
    player_team <- tibble::as_tibble(all_combos) %>%
      dplyr::left_join(pass_stats,  by = c("player_name", "pos_team")) %>%
      dplyr::left_join(rush_stats,  by = c("player_name", "pos_team")) %>%
      dplyr::left_join(recv_stats,  by = c("player_name", "pos_team")) %>%
      dplyr::left_join(succ_stats,  by = c("player_name", "pos_team")) %>%
      dplyr::left_join(games_stats, by = c("player_name", "pos_team")) %>%
      dplyr::mutate(
        plays_this_team = dplyr::coalesce(pass_attempts, 0L) +
          dplyr::coalesce(rush_attempts, 0L) +
          dplyr::coalesce(targets, 0L)
      )

    rm(pass_stats, rush_stats, recv_stats, succ_stats, games_stats,
       all_combos, name_team_list)
    gc(verbose = FALSE)

    # --- Collapse multi-team players to one row per player_name ---
    # Stats are summed across teams. primary_team is the team with the most
    # plays. n_teams flags transfers and potential name collisions.
    player_season <- player_team %>%
      dplyr::group_by(player_name) %>%
      dplyr::summarise(
        season                      = s,
        primary_team                = pos_team[which.max(plays_this_team)],
        n_teams                     = dplyr::n_distinct(pos_team),
        games_played                = as.integer(sum(
          dplyr::coalesce(games_played, 0L))),
        pass_attempts               = as.integer(sum(
          dplyr::coalesce(pass_attempts, 0L))),
        completions                 = as.integer(sum(
          dplyr::coalesce(completions, 0L))),
        passing_yards               = as.integer(sum(
          dplyr::coalesce(passing_yards, 0L))),
        pass_tds                    = as.integer(sum(
          dplyr::coalesce(pass_tds, 0L))),
        interceptions               = as.integer(sum(
          dplyr::coalesce(interceptions, 0L))),
        pass_epa                    = sum(dplyr::coalesce(pass_epa, 0)),
        rush_attempts               = as.integer(sum(
          dplyr::coalesce(rush_attempts, 0L))),
        rushing_yards               = as.integer(sum(
          dplyr::coalesce(rushing_yards, 0L))),
        rush_tds                    = as.integer(sum(
          dplyr::coalesce(rush_tds, 0L))),
        rush_epa                    = sum(dplyr::coalesce(rush_epa, 0)),
        targets                     = as.integer(sum(
          dplyr::coalesce(targets, 0L))),
        receptions                  = as.integer(sum(
          dplyr::coalesce(receptions, 0L))),
        receiving_yards             = as.integer(sum(
          dplyr::coalesce(receiving_yards, 0L))),
        rec_tds                     = as.integer(sum(
          dplyr::coalesce(rec_tds, 0L))),
        rec_epa                     = sum(dplyr::coalesce(rec_epa, 0)),
        # Success numerator and denominator summed so that the final rate
        # is exact rather than an average of team-level rates.
        n_positive_epa              = as.integer(sum(
          dplyr::coalesce(n_positive_epa, 0L))),
        n_epa_plays                 = as.integer(sum(
          dplyr::coalesce(n_epa_plays, 0L))),
        garbage_time_plays_excluded = as.integer(n_plays_excluded),
        .groups = "drop"
      ) %>%
      # Derived efficiency columns. Zero-denominator guard on every division.
      dplyr::mutate(
        has_name_collision   = n_teams >= CFB_NAME_COLLISION_TEAM_THRESHOLD,
        total_plays          = pass_attempts + rush_attempts + targets,
        low_volume           = total_plays < as.integer(min_plays),
        completion_pct       = dplyr::if_else(
          pass_attempts > 0L,
          completions / pass_attempts,
          NA_real_
        ),
        pass_epa_per_attempt = dplyr::if_else(
          pass_attempts > 0L,
          pass_epa / pass_attempts,
          NA_real_
        ),
        rush_epa_per_attempt = dplyr::if_else(
          rush_attempts > 0L,
          rush_epa / rush_attempts,
          NA_real_
        ),
        catch_rate           = dplyr::if_else(
          targets > 0L,
          receptions / targets,
          NA_real_
        ),
        rec_epa_per_target   = dplyr::if_else(
          targets > 0L,
          rec_epa / targets,
          NA_real_
        ),
        success_rate         = dplyr::if_else(
          n_epa_plays > 0L,
          n_positive_epa / n_epa_plays,
          NA_real_
        ),
        panel_version        = CFB_PANEL_VERSION
      ) %>%
      # Drop intermediate success rate components from final schema.
      dplyr::select(-n_positive_epa, -n_epa_plays)

    season_results[[i]] <- player_season

    rm(player_team, player_season)
    gc(verbose = FALSE)

    if (verbose) {
      message(glue(
        "  Season {s}: {format(nrow(season_results[[i]]), big.mark = ',')} ",
        "player-seasons complete."
      ))
    }
  }

  # --- Bind all seasons ---
  non_null <- !vapply(season_results, is.null, logical(1))
  panel    <- dplyr::bind_rows(season_results[non_null])
  rm(season_results)
  gc(verbose = FALSE)

  if (nrow(panel) == 0L) {
    stop(paste(
      "build_cfb_player_season_panel() returned an empty panel.",
      "Verify the CFB cache is populated for the requested seasons via",
      "load_multi_season_cfb_pbp() from R/20_multi_season_cfb_pbp.R."
    ), call. = FALSE)
  }

  # --- Apply position classification ---
  panel <- classify_cfb_player_position(panel)

  # --- Reorder to canonical schema ---
  schema_cols <- c(
    "player_name", "season", "primary_team", "n_teams", "has_name_collision",
    "position_group",
    "games_played", "total_plays", "low_volume",
    "pass_attempts", "completions", "completion_pct",
    "passing_yards", "pass_tds", "interceptions",
    "pass_epa", "pass_epa_per_attempt",
    "rush_attempts", "rushing_yards", "rush_tds",
    "rush_epa", "rush_epa_per_attempt",
    "targets", "receptions", "catch_rate",
    "receiving_yards", "rec_tds",
    "rec_epa", "rec_epa_per_target",
    "success_rate",
    "garbage_time_plays_excluded",
    "panel_version"
  )
  # Guard against future schema additions by keeping only present columns.
  panel <- panel[, schema_cols[schema_cols %in% names(panel)]]

  if (verbose) {
    n_qb    <- sum(panel$position_group == "QB",    na.rm = TRUE)
    n_rb    <- sum(panel$position_group == "RB",    na.rm = TRUE)
    n_wrte  <- sum(panel$position_group == "WR_TE", na.rm = TRUE)
    n_other <- sum(panel$position_group == "other", na.rm = TRUE)
    n_na_pg <- sum(is.na(panel$position_group))
    n_coll  <- sum(panel$has_name_collision, na.rm = TRUE)

    message(glue("\n--- Panel complete ---"))
    message(glue(
      "Rows             : {format(nrow(panel), big.mark = ',')}"
    ))
    message(glue(
      "Seasons          : {min(panel$season, na.rm = TRUE)}-",
      "{max(panel$season, na.rm = TRUE)}"
    ))
    message(glue(
      "Unique players   : ",
      "{format(dplyr::n_distinct(panel$player_name), big.mark = ',')}"
    ))
    message(glue(
      "Low volume       : {format(sum(panel$low_volume, na.rm = TRUE), big.mark = ',')}"
    ))
    message(glue("Position QB      : {format(n_qb, big.mark = ',')}"))
    message(glue("Position RB      : {format(n_rb, big.mark = ',')}"))
    message(glue("Position WR_TE   : {format(n_wrte, big.mark = ',')}"))
    message(glue("Position other   : {format(n_other, big.mark = ',')}"))
    message(glue("Position NA      : {format(n_na_pg, big.mark = ',')}"))
    message(glue(
      "Name collisions  : {format(n_coll, big.mark = ',')} ",
      "(n_teams >= {CFB_NAME_COLLISION_TEAM_THRESHOLD})"
    ))
  }

  panel
}


# ==============================================================================
# FUNCTION: classify_cfb_player_position
# ==============================================================================

#' Classify CFB Player Position Group from Play Role Distribution
#'
#' @description
#' Assigns a \code{position_group} label (QB / RB / WR_TE / other / NA) to
#' each row in the panel based on the proportion of that player's plays in
#' each role. This is a heuristic: there is no cfbfastR roster position anchor
#' equivalent to the nflreadr data used in the NFL panel (R/16).
#'
#' \strong{Classification rules (applied in priority order):}
#' \enumerate{
#'   \item QB: pass_attempts / total_role_plays >= CFB_POSITION_QB_THRESHOLD
#'   \item RB: rush_attempts / total_role_plays >= CFB_POSITION_RB_THRESHOLD
#'   \item WR_TE: targets / total_role_plays >= CFB_POSITION_WR_THRESHOLD
#'   \item other: none of the above thresholds met
#'   \item NA: total_role_plays == 0
#' }
#'
#' \strong{Known edge cases:} A rushing QB (e.g., pass_pct < 0.70,
#' rush_pct >= 0.70) will be classified RB. A dual-threat with no dominant
#' role may appear as "other". A wildcat rusher with no passing and only
#' incidental receiving will be classified RB. Analysts should inspect
#' "other" rows and use positional context when interpreting results.
#'
#' This function is called automatically by \code{build_cfb_player_season_panel}
#' but is exported for standalone use on any compatible tibble.
#'
#' @param panel A tibble with numeric columns pass_attempts, rush_attempts,
#'   and targets. NA values are coerced to 0 before classification.
#'
#' @return The panel tibble with a \code{position_group} column added or
#'   replaced. Intermediate computation columns are removed.
#'
#' @examples
#' panel <- build_cfb_player_season_panel(seasons = 2024L)
#' panel <- classify_cfb_player_position(panel)
#' table(panel$position_group, useNA = "ifany")
#'
#' @seealso build_cfb_player_season_panel, validate_cfb_panel_integrity
#' @export
classify_cfb_player_position <- function(panel) {

  required <- c("pass_attempts", "rush_attempts", "targets")
  missing  <- setdiff(required, names(panel))
  if (length(missing) > 0L) {
    stop(
      glue("classify_cfb_player_position() requires columns: ",
           "{paste(missing, collapse = ', ')}."),
      call. = FALSE
    )
  }

  panel <- panel %>%
    dplyr::mutate(
      pass_attempts_int = as.integer(dplyr::coalesce(pass_attempts, 0L)),
      rush_attempts_int = as.integer(dplyr::coalesce(rush_attempts, 0L)),
      targets_int       = as.integer(dplyr::coalesce(targets, 0L)),
      total_role_plays  = pass_attempts_int + rush_attempts_int + targets_int,

      # Role proportions. Zero-denominator guard produces NA proportions
      # which the case_when handles via the total_role_plays == 0 branch.
      pass_pct = dplyr::if_else(
        total_role_plays > 0L,
        pass_attempts_int / total_role_plays,
        NA_real_
      ),
      rush_pct = dplyr::if_else(
        total_role_plays > 0L,
        rush_attempts_int / total_role_plays,
        NA_real_
      ),
      recv_pct = dplyr::if_else(
        total_role_plays > 0L,
        targets_int / total_role_plays,
        NA_real_
      ),

      # Classification in precedence order.
      position_group = dplyr::case_when(
        total_role_plays == 0L                        ~ NA_character_,
        pass_pct >= CFB_POSITION_QB_THRESHOLD         ~ "QB",
        rush_pct >= CFB_POSITION_RB_THRESHOLD         ~ "RB",
        recv_pct >= CFB_POSITION_WR_THRESHOLD         ~ "WR_TE",
        TRUE                                          ~ "other"
      )
    ) %>%
    dplyr::select(
      -pass_attempts_int, -rush_attempts_int, -targets_int,
      -total_role_plays, -pass_pct, -rush_pct, -recv_pct
    )

  panel
}


# ==============================================================================
# FUNCTION: validate_cfb_panel_integrity
# ==============================================================================

#' Validate CFB Player-Season Panel Integrity
#'
#' @description
#' Runs a battery of integrity checks on the panel from
#' \code{\link{build_cfb_player_season_panel}}. Returns a named list with
#' per-check detail tibbles, a summary tibble, and a scalar \code{valid} flag.
#'
#' \strong{Checks performed:}
#' \enumerate{
#'   \item \strong{No duplicate rows} [critical]: Each player_name + season
#'     must appear exactly once.
#'   \item \strong{EPA range} [critical]: pass_epa, rush_epa, rec_epa within
#'     [-10000, 10000]. Values outside this range indicate aggregation error.
#'   \item \strong{Completions <= pass_attempts} [critical]: Logical constraint.
#'   \item \strong{Receptions <= targets} [critical]: Logical constraint.
#'   \item \strong{Success rate in [0, 1]} [critical]: Proportion must be
#'     bounded.
#'   \item \strong{Efficiency NA consistency} [warning]: Per-play efficiency
#'     columns must be NA when their denominator is 0.
#'   \item \strong{Season coverage} [warning]: All expected seasons present in
#'     the panel output (no season silently dropped).
#'   \item \strong{Position coverage} [info]: Distribution of position_group
#'     values including NA and "other" rates.
#' }
#'
#' @param panel Tibble from build_cfb_player_season_panel().
#' @param expected_seasons Integer vector. Seasons that should be present in
#'   the panel. Default: unique values from panel$season.
#'
#' @return Named list:
#'   \describe{
#'     \item{valid}{Logical scalar. TRUE only if all critical checks pass.}
#'     \item{summary}{Tibble: check_name, severity, passed, detail.}
#'     \item{duplicates}{Tibble of duplicate player_name+season combos.
#'       Empty tibble if check passes.}
#'     \item{completions_exceed_attempts}{Tibble of violating rows.
#'       Empty tibble if check passes.}
#'     \item{receptions_exceed_targets}{Tibble of violating rows.
#'       Empty tibble if check passes.}
#'   }
#'
#' @examples
#' panel <- build_cfb_player_season_panel(seasons = 2024L)
#' vr    <- validate_cfb_panel_integrity(panel)
#' vr$valid    # TRUE if all critical checks pass
#' vr$summary  # Per-check detail
#'
#' @seealso build_cfb_player_season_panel, classify_cfb_player_position
#' @export
validate_cfb_panel_integrity <- function(panel,
                                          expected_seasons = NULL) {

  if (!is.data.frame(panel) || nrow(panel) == 0L) {
    stop("'panel' must be a non-empty data frame.", call. = FALSE)
  }

  if (is.null(expected_seasons)) {
    expected_seasons <- sort(unique(panel$season))
  }

  checks  <- list()
  results <- list()

  # ---- Check 1: No duplicate player_name + season ----
  if (all(c("player_name", "season") %in% names(panel))) {
    dup_counts <- panel %>%
      dplyr::count(player_name, season) %>%
      dplyr::filter(n > 1L)

    chk1_pass <- nrow(dup_counts) == 0L
    checks[["duplicates"]] <- list(
      check_name = "no_duplicate_player_season",
      severity   = "critical",
      passed     = chk1_pass,
      detail     = if (chk1_pass) {
        "OK: no duplicate player_name + season rows"
      } else {
        glue("{nrow(dup_counts)} player_name+season combinations appear more than once")
      }
    )
    results[["duplicates"]] <- if (!chk1_pass) dup_counts else tibble::tibble()
  }

  # ---- Check 2: EPA range plausibility ----
  epa_cols    <- intersect(c("pass_epa", "rush_epa", "rec_epa"), names(panel))
  chk2_pass   <- TRUE
  chk2_detail <- "OK: all EPA columns within [-10000, 10000]"

  for (col in epa_cols) {
    vals_nonNA <- panel[[col]][!is.na(panel[[col]])]
    if (length(vals_nonNA) > 0L &&
        any(vals_nonNA < -10000 | vals_nonNA > 10000)) {
      chk2_pass   <- FALSE
      chk2_detail <- glue(
        "Column {col} has values outside [-10000, 10000]. Possible aggregation error."
      )
      break
    }
  }
  checks[["epa_range"]] <- list(
    check_name = "epa_range_plausible",
    severity   = "critical",
    passed     = chk2_pass,
    detail     = chk2_detail
  )

  # ---- Check 3: Completions <= pass_attempts ----
  if (all(c("completions", "pass_attempts") %in% names(panel))) {
    comp_exceed <- panel %>%
      dplyr::filter(
        !is.na(completions), !is.na(pass_attempts),
        completions > pass_attempts
      )
    chk3_pass <- nrow(comp_exceed) == 0L
    checks[["completions_vs_attempts"]] <- list(
      check_name = "completions_le_attempts",
      severity   = "critical",
      passed     = chk3_pass,
      detail     = if (chk3_pass) {
        "OK: completions <= pass_attempts for all rows"
      } else {
        glue("{nrow(comp_exceed)} rows have completions > pass_attempts")
      }
    )
    results[["completions_exceed_attempts"]] <-
      if (!chk3_pass) comp_exceed else tibble::tibble()
  }

  # ---- Check 4: Receptions <= targets ----
  if (all(c("receptions", "targets") %in% names(panel))) {
    recv_exceed <- panel %>%
      dplyr::filter(
        !is.na(receptions), !is.na(targets),
        receptions > targets
      )
    chk4_pass <- nrow(recv_exceed) == 0L
    checks[["receptions_vs_targets"]] <- list(
      check_name = "receptions_le_targets",
      severity   = "critical",
      passed     = chk4_pass,
      detail     = if (chk4_pass) {
        "OK: receptions <= targets for all rows"
      } else {
        glue("{nrow(recv_exceed)} rows have receptions > targets")
      }
    )
    results[["receptions_exceed_targets"]] <-
      if (!chk4_pass) recv_exceed else tibble::tibble()
  }

  # ---- Check 5: Success rate in [0, 1] ----
  if ("success_rate" %in% names(panel)) {
    sr_nonNA  <- panel$success_rate[!is.na(panel$success_rate)]
    chk5_pass <- length(sr_nonNA) == 0L || all(sr_nonNA >= 0 & sr_nonNA <= 1)
    n_sr_bad  <- sum(sr_nonNA < 0 | sr_nonNA > 1, na.rm = TRUE)
    checks[["success_rate_bounds"]] <- list(
      check_name = "success_rate_in_01",
      severity   = "critical",
      passed     = chk5_pass,
      detail     = if (chk5_pass) {
        "OK: success_rate in [0, 1] for all non-NA rows"
      } else {
        glue("{n_sr_bad} rows have success_rate outside [0, 1]")
      }
    )
  }

  # ---- Check 6: Efficiency column NA consistency ----
  chk6_pass   <- TRUE
  chk6_detail <- "OK: efficiency per-play columns are NA when denominator is 0"

  eff_pairs <- list(
    list(eff = "pass_epa_per_attempt", denom = "pass_attempts"),
    list(eff = "rush_epa_per_attempt", denom = "rush_attempts"),
    list(eff = "rec_epa_per_target",   denom = "targets"),
    list(eff = "completion_pct",       denom = "pass_attempts"),
    list(eff = "catch_rate",           denom = "targets")
  )

  for (pair in eff_pairs) {
    if (!all(c(pair$eff, pair$denom) %in% names(panel))) next
    bad_rows <- panel %>%
      dplyr::filter(
        .data[[pair$denom]] == 0L & !is.na(.data[[pair$eff]])
      )
    if (nrow(bad_rows) > 0L && chk6_pass) {
      chk6_pass   <- FALSE
      chk6_detail <- glue(
        "{nrow(bad_rows)} rows have {pair$eff} non-NA with {pair$denom} == 0"
      )
    }
  }

  checks[["efficiency_na_consistency"]] <- list(
    check_name = "efficiency_na_consistency",
    severity   = "warning",
    passed     = chk6_pass,
    detail     = chk6_detail
  )

  # ---- Check 7: Season coverage ----
  actual_seasons  <- sort(unique(panel$season))
  missing_seasons <- setdiff(as.integer(expected_seasons), actual_seasons)
  chk7_pass       <- length(missing_seasons) == 0L
  checks[["season_coverage"]] <- list(
    check_name = "season_coverage",
    severity   = "warning",
    passed     = chk7_pass,
    detail     = if (chk7_pass) {
      glue("OK: all {length(actual_seasons)} expected season(s) present")
    } else {
      glue("Missing season(s): {paste(missing_seasons, collapse = ', ')}")
    }
  )

  # ---- Check 8: Position coverage (info) ----
  if ("position_group" %in% names(panel)) {
    n_total   <- nrow(panel)
    n_qb      <- sum(panel$position_group == "QB",    na.rm = TRUE)
    n_rb      <- sum(panel$position_group == "RB",    na.rm = TRUE)
    n_wrte    <- sum(panel$position_group == "WR_TE", na.rm = TRUE)
    n_other   <- sum(panel$position_group == "other", na.rm = TRUE)
    n_na_pg   <- sum(is.na(panel$position_group))
    na_pct    <- round(n_na_pg  / n_total * 100, 1)
    other_pct <- round(n_other  / n_total * 100, 1)

    checks[["position_coverage"]] <- list(
      check_name = "position_coverage",
      severity   = "info",
      passed     = TRUE,
      detail     = glue(
        "QB={format(n_qb, big.mark=',')}, ",
        "RB={format(n_rb, big.mark=',')}, ",
        "WR_TE={format(n_wrte, big.mark=',')}, ",
        "other={format(n_other, big.mark=',')} ({other_pct}%), ",
        "NA={format(n_na_pg, big.mark=',')} ({na_pct}%)"
      )
    )
  }

  # ---- Compile summary tibble ----
  summary_tbl <- tibble::tibble(
    check_name = vapply(checks, `[[`, character(1), "check_name"),
    severity   = vapply(checks, `[[`, character(1), "severity"),
    passed     = vapply(checks, `[[`, logical(1),   "passed"),
    detail     = vapply(checks, `[[`, character(1), "detail")
  )

  critical_rows    <- summary_tbl[summary_tbl$severity == "critical", ]
  all_critical_ok  <- nrow(critical_rows) == 0L || all(critical_rows$passed)

  message("\n--- validate_cfb_panel_integrity() ---")
  for (j in seq_len(nrow(summary_tbl))) {
    row  <- summary_tbl[j, ]
    icon <- if (row$passed) "[PASS]" else if (row$severity == "critical") "[FAIL]" else "[WARN]"
    message(glue(
      "  {icon} [{row$severity}] {row$check_name}: {row$detail}"
    ))
  }
  message(glue("  Overall valid: {all_critical_ok}"))

  c(list(valid = all_critical_ok, summary = summary_tbl), results)
}
