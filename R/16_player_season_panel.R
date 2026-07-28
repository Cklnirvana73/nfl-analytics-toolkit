# =============================================================================
# R/16_player_season_panel.R
# =============================================================================
# Season 2, Week 2: Player-Season Panel Construction
# NFL Analytics Toolkit | Production-grade for portfolio display
# =============================================================================
#
# PURPOSE
# -------
# Builds a player-season panel with one row per player per season,
# covering 2010-2025. This is the analytical backbone for aging curves,
# college-to-NFL translation models, and all longitudinal analyses
# in Phases 3-4 of Season 2.
#
# DEPENDENCY
# ----------
# Requires R/15_multi_season_pbp.R to be sourced and the season2_cache/
# directory populated by load_multi_season_pbp(). If the cache does not
# exist, run that pipeline first:
#   source(here::here("R", "15_multi_season_pbp.R"))
#   load_multi_season_pbp(seasons = 2010:2025)
#
# FUNCTIONS IN THIS FILE
# ----------------------
#   Line  130: classify_player_position()
#   Line  330: build_player_season_panel()
#   Line  700: validate_panel_integrity()
#
# KEY DESIGN DECISIONS
# --------------------
# 1. POSITION ANCHOR
#    Position comes from nflreadr roster data, not play-level role.
#    Lamar Jackson = QB. McCaffrey = RB. This is correct and intentional.
#    The play-level columns (rusher_player_id, receiver_player_id) are used
#    only for stat aggregation, never for position classification.
#
# 2. SCRAMBLE PARTITION (no double-counting of EPA)
#    Scramble plays count toward rushing yards and rush_epa.
#    pass_epa covers pass_attempt and sack plays only (NOT scrambles).
#    rush_epa covers rush_attempt AND qb_scramble plays.
#    qb_dropbacks counts all three (pass + sack + scramble) as a denominator.
#
# 3. PLAY EXCLUSIONS (applied every season before aggregation)
#    - QB kneels excluded from rushing stats (coalesce guard for NA safety)
#    - QB spikes excluded from passing stats (coalesce guard for NA safety)
#    - Two-point conversion plays excluded from all stats
#    - Pre-season and post-season excluded (season_type == "REG" only)
#
# 4. MULTI-TEAM PLAYERS
#    Players traded mid-season get ONE row per season.
#    team = last team per nflreadr roster (end-of-season assignment).
#    Stats are summed across all teams they played for.
#
# 5. MINIMUM PLAYS THRESHOLD
#    PANEL_MIN_PLAYS = 10 (default). Players below threshold are NOT dropped.
#    They are retained with low_volume = TRUE. Downstream models apply
#    their own filtering per analytical need.
#
# 6. MEMORY MANAGEMENT
#    PBP data loads one season at a time via load_normalized_season().
#    Each season is freed with rm() + gc() before the next loads.
#    Peak memory stays under ~400 MB throughout.
#
# NFL CONTEXT
# -----------
# EPA (Expected Points Added) = EP_after - EP_before.
#   Positive EPA = play increased scoring probability.
#   Negative EPA = play decreased scoring probability.
#   Success rate = proportion of plays with EPA > 0.
#
# CPOE (Completion Percentage Over Expected): available 2016+ via tracking
#   data. Returns NA for 2010-2015 seasons. See OPTIONAL_COLUMNS in R/15.
#
# Garbage time (WP < 10% or > 90% in Q4) is NOT filtered from this panel.
# The panel stores raw season aggregates. Downstream analyses apply
# situational filters appropriate to their specific question.
#
# COLUMN REFERENCE
# ----------------
# See docs/Week2_Column_Reference.txt for the full output schema.
#
# =============================================================================

library(nflfastR)
library(nflreadr)
library(dplyr)
library(tidyr)
library(here)
library(glue)


# =============================================================================
# CONFIGURATION
# =============================================================================

# Minimum total plays (attempts + rush_attempts + targets) to flag low_volume.
# Players below this are retained in the panel with low_volume = TRUE.
PANEL_MIN_PLAYS <- 10L

# Season range for the panel. Must match seasons available in season2_cache/.
PANEL_SEASONS_DEFAULT <- 2010:2025

# Panel schema version. Increment if output column structure changes.
PANEL_VERSION <- "s2panel_v1"

# Position group routing for the model layer.
# TEs and WRs share the WR_TE group per project design.
# FB and HB fold into RB.
# All other positions (OL, DL, DB, K, P, etc.) map to "other".
POSITION_GROUP_MAP <- c(
  "QB" = "QB",
  "RB" = "RB",
  "FB" = "RB",
  "HB" = "RB",
  "WR" = "WR_TE",
  "TE" = "WR_TE"
)

# nflreadr column names. Documented here so API changes surface as clear errors.
ROSTER_COLS_REQUIRED <- c("gsis_id", "full_name", "position", "team", "season")
ROSTER_COL_DEPTH     <- "depth_chart_position"

# Maximum games per era (for validate_panel_integrity games-cap check).
# 2021+: 17-game regular season (272 total games).
# Pre-2021: 16-game regular season (256 total games).
.max_games_for_season <- function(season) {
  dplyr::if_else(as.integer(season) >= 2021L, 17L, 16L)
}


# =============================================================================
# INTERNAL HELPER: .mode_chr()
# =============================================================================
# Returns the most frequent non-NA string value in a character vector.
# Used to resolve position when a player has multiple roster entries
# (e.g., traded mid-season, listed at two positions early in camp).
# Returns NA_character_ on empty or all-NA input.

.mode_chr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1L]
}


# =============================================================================
# classify_player_position()
# =============================================================================

#' Classify Player Position from nflreadr Roster Data
#'
#' @description
#' Resolves the official roster position for a set of player IDs and seasons
#' using \code{nflreadr::load_rosters()}. This is the authoritative position
#' source for the Season 2 toolkit. Position is never inferred from play-level
#' role -- a rushing QB is always QB, a pass-catching RB is always RB.
#'
#' For players traded mid-season, the most common position across all roster
#' entries is used. The \code{team} column reflects the LAST team the player
#' was rostered on in that season.
#'
#' \strong{Known complexity cases (handled automatically):}
#' \itemize{
#'   \item Lamar Jackson, Jalen Hurts: nflreadr position = QB. Rushing stats
#'     appear in rush_* columns in the panel, but position_group = "QB".
#'   \item Christian McCaffrey, Alvin Kamara: nflreadr position = RB. Receiving
#'     stats appear in rec_* columns but position_group = "RB".
#' }
#'
#' \strong{Position group mapping:}
#' \itemize{
#'   \item QB -> QB
#'   \item RB / FB / HB -> RB
#'   \item WR / TE -> WR_TE (model routing group)
#'   \item All others (OL, DL, DB, etc.) -> other
#' }
#'
#' @param player_ids Character vector of GSIS player IDs, or NULL to return
#'   all players found in nflreadr rosters for the given seasons.
#' @param seasons Integer vector of season years (e.g., \code{2010:2025}).
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return A tibble with one row per player-season:
#'   \describe{
#'     \item{player_id}{chr: GSIS ID -- matches passer/rusher/receiver_player_id
#'       in nflfastR play-by-play data}
#'     \item{season}{int: Season year}
#'     \item{player_name}{chr: Full name from nflreadr}
#'     \item{team}{chr: Last team during the season (end-of-season assignment)}
#'     \item{position}{chr: Official roster position (QB, RB, WR, TE, etc.)}
#'     \item{depth_chart_position}{chr: Depth chart position if available in
#'       nflreadr; NA otherwise}
#'     \item{position_group}{chr: Model routing group (QB / RB / WR_TE / other)}
#'     \item{has_name_collision}{lgl: TRUE when this player_name maps to more
#'       than one player_id in the same season (e.g., two players named
#'       Michael Thomas active in 2019-2020). player_id is always the correct
#'       join key regardless of this flag.}
#'   }
#'
#'   NOT returned: play-level role columns (passer/rusher/receiver). Use
#'   \code{\link{build_player_season_panel}} for aggregated stats.
#'
#' @seealso \code{\link{build_player_season_panel}},
#'   \code{\link{validate_panel_integrity}}
#'
#' @examples
#' # All players across two seasons
#' positions <- classify_player_position(seasons = 2023:2024)
#'
#' # Verify Lamar Jackson is classified as QB
#' lamar <- positions %>%
#'   filter(player_name == "Lamar Jackson", season == 2023)
#' # Expect: position = "QB", position_group = "QB"
#'
#' # Verify McCaffrey is classified as RB (not WR despite high target share)
#' cmc <- positions %>%
#'   filter(player_name == "Christian McCaffrey", season == 2023)
#' # Expect: position = "RB", position_group = "RB"
#'
#' @export
classify_player_position <- function(player_ids = NULL,
                                     seasons    = PANEL_SEASONS_DEFAULT,
                                     verbose    = TRUE) {

  # Input validation
  if (!is.numeric(seasons) || length(seasons) == 0L) {
    stop("seasons must be a non-empty numeric vector (e.g., 2010:2025).")
  }
  seasons <- sort(as.integer(seasons))

  if (!is.null(player_ids)) {
    if (!is.character(player_ids) || length(player_ids) == 0L) {
      stop("player_ids must be a non-empty character vector or NULL.")
    }
  }

  if (verbose) {
    message(glue::glue(
      "classify_player_position(): loading rosters for ",
      "{length(seasons)} season(s) [{min(seasons)}-{max(seasons)}]..."
    ))
  }

  # Load roster data from nflreadr
  roster_raw <- tryCatch(
    nflreadr::load_rosters(seasons = seasons),
    error = function(e) {
      stop(glue::glue(
        "nflreadr::load_rosters() failed: {conditionMessage(e)}\n",
        "Check network connection and nflreadr version."
      ))
    }
  )

  # Verify required columns are present (catches nflreadr API changes)
  missing_required <- setdiff(ROSTER_COLS_REQUIRED, names(roster_raw))
  if (length(missing_required) > 0L) {
    stop(glue::glue(
      "nflreadr::load_rosters() is missing expected columns: ",
      "{paste(missing_required, collapse = ', ')}.\n",
      "This may indicate an nflreadr API change. ",
      "Update ROSTER_COLS_REQUIRED in R/16_player_season_panel.R."
    ))
  }

  # Add depth_chart_position as NA column if not returned by this nflreadr version
  if (!ROSTER_COL_DEPTH %in% names(roster_raw)) {
    roster_raw[[ROSTER_COL_DEPTH]] <- NA_character_
  }

  # Filter to requested player_ids if provided
  if (!is.null(player_ids)) {
    roster_raw <- roster_raw %>%
      dplyr::filter(gsis_id %in% player_ids)

    unmatched <- setdiff(player_ids, roster_raw$gsis_id)
    if (length(unmatched) > 0L && verbose) {
      warning(glue::glue(
        "{length(unmatched)} player_id(s) not found in nflreadr roster data. ",
        "These may be specialists, historical edge cases, or data gaps.\n",
        "Examples: {paste(head(unmatched, 3L), collapse = ', ')}"
      ))
    }
  }

  if (verbose) {
    message(glue::glue(
      "  Raw roster rows: {format(nrow(roster_raw), big.mark = ',')}. ",
      "Resolving to one row per player-season..."
    ))
  }

  # Resolve to one row per player-season.
  # For traded players (multiple teams, multiple roster entries):
  #   position = most common position across all entries
  #   team     = last team listed (end-of-season assignment)
  #   name     = first non-NA full_name found
  roster_clean <- roster_raw %>%
    dplyr::filter(!is.na(gsis_id), !is.na(position)) %>%
    dplyr::group_by(gsis_id, season) %>%
    dplyr::summarise(
      player_name          = dplyr::first(full_name[!is.na(full_name)]),
      position             = .mode_chr(position),
      depth_chart_position = .mode_chr(.data[[ROSTER_COL_DEPTH]]),
      team                 = dplyr::last(team[!is.na(team)]),
      .groups              = "drop"
    ) %>%
    dplyr::rename(player_id = gsis_id) %>%
    # Apply position group mapping. recode() with !!!POSITION_GROUP_MAP
    # passes the named vector as individual arguments. .default handles
    # all positions not in the map (OL, DL, K, P, etc.).
    dplyr::mutate(
      position_group = dplyr::recode(
        position,
        !!!POSITION_GROUP_MAP,
        .default = "other"
      )
    )

  # Detect name collisions: player_name values that map to more than one
  # player_id within the same season. These are genuinely different people
  # who share an identical display name (e.g., two WRs named Michael Thomas
  # active in 2019-2020; two players named Lamar Jackson in 2023).
  # has_name_collision = TRUE flags these rows. player_id remains the only
  # safe join key regardless of this flag.
  name_collision_index <- roster_clean %>%
    dplyr::group_by(season, player_name) %>%
    dplyr::summarise(
      n_player_ids = dplyr::n_distinct(player_id),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_player_ids > 1L) %>%
    dplyr::select(season, player_name) %>%
    dplyr::mutate(has_name_collision = TRUE)

  roster_clean <- roster_clean %>%
    dplyr::left_join(name_collision_index, by = c("season", "player_name")) %>%
    dplyr::mutate(
      has_name_collision = dplyr::coalesce(has_name_collision, FALSE)
    )

  if (verbose) {
    pos_dist     <- sort(table(roster_clean$position_group), decreasing = TRUE)
    n_collisions <- sum(roster_clean$has_name_collision)
    message(glue::glue(
      "  Resolved: {format(nrow(roster_clean), big.mark = ',')} player-seasons.\n",
      "  Position groups: ",
      paste(names(pos_dist), pos_dist, sep = "=", collapse = ", "), "\n",
      "  Name collisions flagged: {n_collisions} player-seasons ",
      "(has_name_collision = TRUE)"
    ))
  }

  return(roster_clean)
}


# =============================================================================
# INTERNAL HELPERS: Season-level stat builders
# =============================================================================
# These four functions are not exported. Each is called once per season
# inside build_player_season_panel() to keep the main function readable.
# All accept a pre-filtered PBP tibble (regular season, pass/run only).
#
# COLUMN DEPENDENCIES (verified at call site in build_player_season_panel):
#   Required (from CORE_COLUMNS in R/15): epa, success, pass_attempt,
#     rush_attempt, complete_pass, passer_player_id, rusher_player_id,
#     receiver_player_id, passing_yards, rushing_yards, receiving_yards,
#     air_yards, yards_after_catch, qb_dropback, qb_scramble, interception,
#     sack, two_point_attempt
#   Guarded (not in CORE_COLUMNS, present in raw nflfastR, retained by
#     normalize_schema()): qb_kneel, qb_spike, pass_touchdown,
#     rush_touchdown, cpoe
# =============================================================================


# -----------------------------------------------------------------------------
# .build_passing_stats(pbp)
# -----------------------------------------------------------------------------
# Aggregates passing statistics for all passers in the season.
# Scope: plays where passer_player_id is not NA and (pass_attempt OR sack).
# Scrambles are excluded from pass_epa here -- their EPA goes to rush_epa.
# qb_spike plays are excluded (intentional incompletions, not real attempts).
# two_point_attempt plays are excluded (different scoring context).
# -----------------------------------------------------------------------------
.build_passing_stats <- function(pbp) {

  # qb_spike and qb_kneel are guaranteed to exist by build_player_season_panel(),
  # which adds them as 0L if absent in the normalized data. Use coalesce
  # defensively against any residual NAs.
  pbp %>%
    dplyr::filter(
      !is.na(passer_player_id),
      # Include: clean pass attempts and sacks
      # Exclude: scrambles (qb_dropback but no pass_attempt -- handled in rush)
      pass_attempt == 1L | sack == 1L,
      dplyr::coalesce(qb_spike, 0L) != 1L,
      dplyr::coalesce(two_point_attempt, 0L) != 1L
    ) %>%
    dplyr::group_by(player_id = passer_player_id) %>%
    dplyr::summarise(
      # Attempt and completion counts
      attempts     = sum(pass_attempt, na.rm = TRUE),
      completions  = sum(complete_pass, na.rm = TRUE),
      passing_yards = sum(passing_yards, na.rm = TRUE),

      # pass_touchdown is not in CORE_COLUMNS but is standard nflfastR.
      # normalize_schema() retains it. Guard for robustness.
      pass_tds = if ("pass_touchdown" %in% names(pbp))
        as.integer(sum(pass_touchdown, na.rm = TRUE))
      else
        NA_integer_,

      interceptions_thrown = sum(interception, na.rm = TRUE),
      sacks_taken          = sum(sack, na.rm = TRUE),

      # qb_dropbacks: counts pass attempts and sacks only.
      # Scramble rows are excluded by the filter above (pass_attempt == 1 | sack == 1)
      # so their qb_dropback = 1 flag never reaches this sum.
      # Scramble dropbacks are counted separately in .build_rushing_stats().
      qb_dropbacks = sum(qb_dropback, na.rm = TRUE),

      # pass_epa: sum of EPA on pass_attempt and sack rows only.
      # Scramble EPA is intentionally excluded here (see .build_rushing_stats).
      pass_epa = sum(epa[pass_attempt == 1L | sack == 1L], na.rm = TRUE),

      # pass_epa_per_dropback: use full dropback count as denominator.
      # Division guard: if_else prevents NA/NaN from zero denominators.
      pass_epa_per_dropback = dplyr::if_else(
        sum(qb_dropback, na.rm = TRUE) > 0L,
        sum(epa[pass_attempt == 1L | sack == 1L], na.rm = TRUE) /
          sum(qb_dropback, na.rm = TRUE),
        NA_real_
      ),

      # Success rate: proportion of pass attempts with EPA > 0.
      # Denominator restricted to pass_attempt rows (excludes sacks).
      pass_success_rate = dplyr::if_else(
        sum(pass_attempt, na.rm = TRUE) > 0L,
        mean(success[pass_attempt == 1L], na.rm = TRUE),
        NA_real_
      ),

      # CPOE: completion percentage over expected.
      # Available ~2016+ via tracking data. Returns NA for earlier seasons.
      # The column exists in normalized data (OPTIONAL_COLUMN) but is NA
      # for seasons before tracking data was available.
      # Base if/else (not dplyr::if_else): if_else evaluates both branches
      # eagerly, which errors with object-not-found when cpoe is absent.
      # Short-circuit && guards the cpoe reference.
      mean_cpoe = if ("cpoe" %in% names(pbp) &&
                        sum(!is.na(cpoe[pass_attempt == 1L])) > 0L)
        mean(cpoe[pass_attempt == 1L], na.rm = TRUE)
      else
        NA_real_,

      .groups = "drop"
    )
}


# -----------------------------------------------------------------------------
# .build_rushing_stats(pbp)
# -----------------------------------------------------------------------------
# Aggregates rushing statistics including QB scrambles.
# Scope: plays where rusher_player_id is not NA and (rush_attempt OR scramble).
# Kneels excluded (not real rushing efficiency).
# two_point_attempt plays excluded.
# rush_epa captures scramble EPA (which is excluded from pass_epa).
# -----------------------------------------------------------------------------
.build_rushing_stats <- function(pbp) {

  # Guard: qb_kneel should be in raw nflfastR for all seasons but is not
  # in Season 2 CORE_COLUMNS. Coalesce to 0 if somehow absent.
  pbp %>%
    dplyr::filter(
      !is.na(rusher_player_id),
      rush_attempt == 1L | dplyr::coalesce(qb_scramble, 0L) == 1L,
      dplyr::coalesce(qb_kneel, 0L)          != 1L,
      dplyr::coalesce(two_point_attempt, 0L) != 1L
    ) %>%
    dplyr::group_by(player_id = rusher_player_id) %>%
    dplyr::summarise(
      # Designed rushes only (excludes scrambles for play-count clarity)
      rush_attempts = sum(rush_attempt, na.rm = TRUE),
      # Scrambles counted separately so analysts can split designed vs scramble
      scrambles     = sum(dplyr::coalesce(qb_scramble, 0L), na.rm = TRUE),
      # Rushing yards: includes both designed runs and scramble yards
      rushing_yards = sum(rushing_yards, na.rm = TRUE),

      # rush_touchdown: not in CORE_COLUMNS but standard nflfastR column.
      rush_tds = if ("rush_touchdown" %in% names(pbp))
        as.integer(sum(rush_touchdown, na.rm = TRUE))
      else
        NA_integer_,

      # rush_epa: EPA from rush_attempt AND scramble rows.
      # This is the complement of pass_epa (no overlap).
      rush_epa = sum(epa, na.rm = TRUE),

      # rush_epa_per_attempt: designed runs only in denominator.
      # Scrambles have a different decision structure and would skew this.
      rush_epa_per_attempt = dplyr::if_else(
        sum(rush_attempt, na.rm = TRUE) > 0L,
        sum(epa[rush_attempt == 1L], na.rm = TRUE) /
          sum(rush_attempt, na.rm = TRUE),
        NA_real_
      ),

      rush_success_rate = dplyr::if_else(
        sum(rush_attempt, na.rm = TRUE) > 0L,
        mean(success[rush_attempt == 1L], na.rm = TRUE),
        NA_real_
      ),

      # yards_per_carry: designed runs only (standard NFL definition excludes scrambles)
      yards_per_carry = dplyr::if_else(
        sum(rush_attempt, na.rm = TRUE) > 0L,
        sum(rushing_yards[rush_attempt == 1L], na.rm = TRUE) /
          sum(rush_attempt, na.rm = TRUE),
        NA_real_
      ),

      .groups = "drop"
    )
}


# -----------------------------------------------------------------------------
# .build_receiving_stats(pbp)
# -----------------------------------------------------------------------------
# Aggregates receiving statistics for all targeted receivers.
# Scope: plays where receiver_player_id is not NA and pass_attempt == 1.
# two_point_attempt plays excluded.
# rec_epa: EPA on all target rows (complete and incomplete).
# -----------------------------------------------------------------------------
.build_receiving_stats <- function(pbp) {

  pbp %>%
    dplyr::filter(
      !is.na(receiver_player_id),
      pass_attempt == 1L,
      dplyr::coalesce(two_point_attempt, 0L) != 1L
    ) %>%
    dplyr::group_by(player_id = receiver_player_id) %>%
    dplyr::summarise(
      targets         = dplyr::n(),
      receptions      = sum(complete_pass, na.rm = TRUE),
      receiving_yards = sum(receiving_yards, na.rm = TRUE),

      # Receiving TDs: sum pass_touchdown on target rows.
      # pass_touchdown = 1 when the pass was a TD, attributed to the receiver.
      rec_tds = if ("pass_touchdown" %in% names(pbp))
        as.integer(sum(pass_touchdown, na.rm = TRUE))
      else
        NA_integer_,

      # rec_epa: EPA on ALL target rows (includes incompletions).
      # This is opportunity-weighted -- a player who draws PI on a target gets
      # EPA credit even though they didn't record a reception.
      rec_epa = sum(epa, na.rm = TRUE),

      rec_epa_per_target = dplyr::if_else(
        dplyr::n() > 0L,
        sum(epa, na.rm = TRUE) / dplyr::n(),
        NA_real_
      ),

      rec_success_rate = dplyr::if_else(
        dplyr::n() > 0L,
        mean(success, na.rm = TRUE),
        NA_real_
      ),

      catch_rate = dplyr::if_else(
        dplyr::n() > 0L,
        sum(complete_pass, na.rm = TRUE) / dplyr::n(),
        NA_real_
      ),

      # rec_air_yards: air yards on all targets (not just completions).
      # Measures the depth of target opportunity regardless of outcome.
      rec_air_yards = sum(air_yards, na.rm = TRUE),
      rec_yac       = sum(yards_after_catch, na.rm = TRUE),

      .groups = "drop"
    )
}


# -----------------------------------------------------------------------------
# .build_games_played(pbp)
# -----------------------------------------------------------------------------
# Counts distinct games each player appeared in (any offensive role).
# A player "appeared" if they had at least one play as passer, rusher,
# or receiver. Pre-filtered to regular season by the caller.
# -----------------------------------------------------------------------------
.build_games_played <- function(pbp) {

  passer_games <- pbp %>%
    dplyr::filter(!is.na(passer_player_id)) %>%
    dplyr::distinct(player_id = passer_player_id, game_id)

  rusher_games <- pbp %>%
    dplyr::filter(!is.na(rusher_player_id)) %>%
    dplyr::distinct(player_id = rusher_player_id, game_id)

  receiver_games <- pbp %>%
    dplyr::filter(!is.na(receiver_player_id)) %>%
    dplyr::distinct(player_id = receiver_player_id, game_id)

  dplyr::bind_rows(passer_games, rusher_games, receiver_games) %>%
    dplyr::distinct(player_id, game_id) %>%
    dplyr::count(player_id, name = "games_played")
}


# =============================================================================
# build_player_season_panel()
# =============================================================================

#' Build Player-Season Panel from 16 Seasons of Play-by-Play Data
#'
#' @description
#' Aggregates normalized play-by-play data from the Season 2 cache into a
#' player-season panel with one row per player per season (2010-2025).
#' Position classification is anchored to \code{nflreadr} roster data via
#' \code{\link{classify_player_position}}. Stats from all play roles (passing,
#' rushing, receiving) are aggregated independently and joined to the same row.
#'
#' This panel is the analytical backbone for aging curves (Week 9), the
#' college-to-NFL translation model (Week 10), the projection engine (Week 14),
#' and the trade value model (Week 16).
#'
#' \strong{Memory management:} Each season's PBP is loaded, aggregated,
#' and freed before the next season loads. Peak memory stays under ~400 MB.
#'
#' @param seasons Integer vector. Seasons to include. Must be a subset of
#'   seasons available in \code{cache_dir}. Default: \code{2010:2025}.
#' @param cache_dir Character. Path to the season2_cache/ directory produced
#'   by \code{load_multi_season_pbp()} in \code{R/15_multi_season_pbp.R}.
#'   Default: \code{here::here("data", "season2_cache")}.
#' @param min_plays Integer. Minimum total offensive plays (attempts +
#'   rush_attempts + targets) for a player-season to be considered above the
#'   low_volume threshold. Default: 10. Players below threshold are retained
#'   with \code{low_volume = TRUE}.
#' @param verbose Logical. Print progress messages. Default: TRUE.
#'
#' @return A tibble with one row per player per season. Columns:
#'   \describe{
#'     \item{player_id}{chr: GSIS ID (matches nflfastR player ID columns)}
#'     \item{player_name}{chr: Full name from nflreadr; falls back to play-level
#'       name if no roster match}
#'     \item{season}{int: Season year (2010-2025)}
#'     \item{team}{chr: Last team of season per nflreadr (end-of-season)}
#'     \item{position}{chr: Official roster position (QB, RB, WR, TE, etc.)}
#'     \item{depth_chart_position}{chr: Depth chart position if available}
#'     \item{position_group}{chr: QB / RB / WR_TE / other}
#'     \item{games_played}{int: Distinct games with at least one offensive play}
#'     \item{total_plays}{int: attempts + rush_attempts + targets}
#'     \item{low_volume}{lgl: TRUE if total_plays < min_plays}
#'     \item{attempts}{int: Pass attempts (excludes sacks, scrambles, spikes)}
#'     \item{completions}{int: Completed passes}
#'     \item{passing_yards}{int: Passing yards on completions}
#'     \item{pass_tds}{int: Passing touchdowns (NA if column absent in source)}
#'     \item{interceptions_thrown}{int: Interceptions thrown}
#'     \item{sacks_taken}{int: Times sacked}
#'     \item{qb_dropbacks}{int: Total dropbacks (pass + sack + scramble)}
#'     \item{pass_epa}{dbl: Total EPA on pass_attempt and sack plays}
#'     \item{pass_epa_per_dropback}{dbl: pass_epa / qb_dropbacks}
#'     \item{pass_success_rate}{dbl: Proportion of pass attempts with EPA > 0}
#'     \item{mean_cpoe}{dbl: Mean CPOE on pass attempts; NA for 2010-2015}
#'     \item{rush_attempts}{int: Designed rushing attempts (excludes kneels)}
#'     \item{scrambles}{int: QB scrambles (included in rushing stats)}
#'     \item{rushing_yards}{int: Total rushing yards (designed + scrambles)}
#'     \item{rush_tds}{int: Rushing touchdowns (NA if column absent)}
#'     \item{rush_epa}{dbl: Total EPA on rush_attempt and qb_scramble plays}
#'     \item{rush_epa_per_attempt}{dbl: EPA per designed rush attempt}
#'     \item{rush_success_rate}{dbl: Proportion of designed rushes with EPA > 0}
#'     \item{yards_per_carry}{dbl: rushing_yards / rush_attempts (designed only)}
#'     \item{targets}{int: Times targeted as a receiver on pass plays}
#'     \item{receptions}{int: Receptions}
#'     \item{receiving_yards}{int: Receiving yards}
#'     \item{rec_tds}{int: Receiving touchdowns (NA if column absent)}
#'     \item{rec_epa}{dbl: Total EPA on all target rows}
#'     \item{rec_epa_per_target}{dbl: rec_epa / targets}
#'     \item{rec_success_rate}{dbl: Proportion of targets with EPA > 0}
#'     \item{catch_rate}{dbl: receptions / targets}
#'     \item{rec_air_yards}{int: Air yards on all targets}
#'     \item{rec_yac}{int: Yards after catch on receptions}
#'     \item{total_tds}{int: pass_tds + rush_tds + rec_tds (NA if any component NA)}
#'     \item{total_yards}{int: passing_yards + rushing_yards + receiving_yards}
#'     \item{total_epa}{dbl: pass_epa + rush_epa + rec_epa}
#'     \item{panel_version}{chr: Schema version tag ("s2panel_v1")}
#'   }
#'
#'   NOT returned: play_id, game_id, play_type, down, ydstogo, or any other
#'   play-level columns. These remain in the cached PBP files.
#'
#' @details
#' \strong{Scramble accounting:}
#' A QB scramble has both \code{passer_player_id} and \code{rusher_player_id}
#' set to the QB's ID. The play counts toward \code{qb_dropbacks} (passing
#' denominator) but its yards and EPA go to rushing stats only. This prevents
#' double-counting of EPA while correctly attributing the rushing production.
#'
#' \strong{Total stats with NA components:}
#' When any of \code{pass_tds}, \code{rush_tds}, or \code{rec_tds} is NA
#' (due to missing source column in older seasons), \code{total_tds} will
#' also be NA. Use \code{total_yards} and \code{total_epa} (which guard NAs
#' with coalesce) for cross-season comparisons.
#'
#' @seealso \code{\link{classify_player_position}},
#'   \code{\link{validate_panel_integrity}}
#'
#' @examples
#' # Full 16-season panel (15-25 min first run, faster on cache hit)
#' panel <- build_player_season_panel()
#'
#' # Single season for development and testing
#' panel_2024 <- build_player_season_panel(seasons = 2024)
#'
#' # Check that Lamar Jackson's position is QB despite high rush volume
#' lamar <- panel %>% filter(player_name == "Lamar Jackson", season == 2023)
#' stopifnot(lamar$position == "QB", lamar$position_group == "QB")
#' # rush_attempts and rushing_yards will be populated (not zero)
#' # because those columns reflect what he DID, not his roster position
#'
#' # Check McCaffrey's receiving stats are captured under RB
#' cmc <- panel %>%
#'   filter(player_name == "Christian McCaffrey", season == 2023)
#' stopifnot(cmc$position_group == "RB")
#' # targets and receiving_yards will be populated despite position = "RB"
#'
#' # Summary stats
#' qb_panel <- panel %>%
#'   filter(position_group == "QB", !low_volume, season >= 2020) %>%
#'   arrange(desc(pass_epa_per_dropback))
#'
#' @export
build_player_season_panel <- function(
    seasons   = PANEL_SEASONS_DEFAULT,
    cache_dir = here::here("data", "season2_cache"),
    min_plays = PANEL_MIN_PLAYS,
    verbose   = TRUE
) {

  # Input validation
  if (!is.numeric(seasons) || length(seasons) == 0L) {
    stop("seasons must be a non-empty numeric vector (e.g., 2010:2025).")
  }
  seasons <- sort(as.integer(seasons))

  if (!dir.exists(cache_dir)) {
    stop(glue::glue(
      "cache_dir not found: {cache_dir}\n",
      "Run load_multi_season_pbp() from R/15_multi_season_pbp.R first ",
      "to populate the cache."
    ))
  }

  if (!is.numeric(min_plays) || min_plays < 0L) {
    stop("min_plays must be a non-negative integer.")
  }

  # Source Week 1 functions if load_normalized_season() is not yet available.
  # This makes R/16 self-contained when sourced fresh.
  if (!exists("load_normalized_season", mode = "function")) {
    week1_path <- here::here("R", "15_multi_season_pbp.R")
    if (!file.exists(week1_path)) {
      stop(glue::glue(
        "R/15_multi_season_pbp.R not found at: {week1_path}\n",
        "This file is required for load_normalized_season()."
      ))
    }
    if (verbose) message("Sourcing R/15_multi_season_pbp.R...")
    source(week1_path)
  }

  if (verbose) {
    message(glue::glue(
      "\n--- build_player_season_panel() ---",
      "\nSeasons : {min(seasons)}-{max(seasons)} ({length(seasons)} seasons)",
      "\nCache   : {cache_dir}",
      "\nMin plays threshold: {min_plays}",
      "\nProcessing one season at a time (memory-safe)..."
    ))
  }

  # Step 1: Load position classifications for all seasons in one nflreadr call.
  # More efficient than calling load_rosters() once per season in the loop.
  if (verbose) message("\nStep 1: Loading position classifications from nflreadr...")
  positions <- classify_player_position(
    player_ids = NULL,
    seasons    = seasons,
    verbose    = verbose
  )

  # Step 2: Process each season. Accumulate one tibble per season.
  season_results <- vector("list", length(seasons))

  for (i in seq_along(seasons)) {
    yr <- seasons[i]
    if (verbose) message(glue::glue("\nStep 2.{i}/{length(seasons)}: Season {yr}..."))

    # Load normalized PBP from season2_cache/
    pbp_raw <- tryCatch(
      load_normalized_season(season = yr, cache_dir = cache_dir),
      error = function(e) {
        warning(glue::glue(
          "Season {yr}: Could not load from cache. Skipping.\n",
          "Error: {conditionMessage(e)}\n",
          "Run load_multi_season_pbp(seasons = {yr}) to generate the cache."
        ))
        return(NULL)
      }
    )

    if (is.null(pbp_raw)) {
      gc()
      next
    }

    # Build player name lookup from play data BEFORE freeing pbp_raw.
    # Used as a fallback for players with no nflreadr roster match.
    name_lookup <- dplyr::bind_rows(
      pbp_raw %>%
        dplyr::filter(!is.na(passer_player_id)) %>%
        dplyr::distinct(player_id = passer_player_id,
                        player_name_pbp = passer_player_name),
      pbp_raw %>%
        dplyr::filter(!is.na(rusher_player_id)) %>%
        dplyr::distinct(player_id = rusher_player_id,
                        player_name_pbp = rusher_player_name),
      pbp_raw %>%
        dplyr::filter(!is.na(receiver_player_id)) %>%
        dplyr::distinct(player_id = receiver_player_id,
                        player_name_pbp = receiver_player_name)
    ) %>%
      dplyr::filter(!is.na(player_id), !is.na(player_name_pbp)) %>%
      dplyr::distinct(player_id, .keep_all = TRUE)

    # Filter to regular season offensive plays.
    # season_type column exists in nflfastR but is not in Season 2 CORE_COLUMNS.
    # Guard for the (unlikely) case it's missing.
    #
    # TWO-POINT PLAYS ARE RETAINED [2026-07-15]. Previously this filter kept
    # only play_type %in% c("pass","run"), which silently dropped two-point
    # conversions that nflfastR tags "no_play" because a defensive penalty was
    # enforced between downs. The conversion still counted; only the penalty
    # moved to the ensuing kickoff. Three real cases across 2010-2025:
    #   2015_13_IND_PIT  Roethlisberger -> W.Johnson, succeeds, def. holding
    #   2019_06_SEA_CLE  Mayfield -> D.Harris, succeeds, def. holding
    #   2020_04_CLE_DAL  S.Carlson rush on a blocked-XP recovery, def. offside
    # A player whose only snap in a game was one of these was credited with zero
    # games_played for it.
    #
    # This changes ONLY games_played. .build_passing_stats(), .build_rushing_
    # stats() and .build_receiving_stats() each already exclude
    # two_point_attempt == 1L (lines 398, 478, 541), which is correct: by NFL
    # convention two-point yards and TDs do not count toward passing, rushing or
    # receiving totals. So the extra rows are invisible to every counting column
    # and to the min_plays / low_volume threshold, and visible only to
    # .build_games_played(), which is the thing that was wrong.
    #
    # Two-point plays already tagged "pass" or "run" (the large majority: 107 of
    # 113 in 2016) were always retained by this filter and already counted
    # toward games_played. Only the "no_play"-tagged ones were being lost.
    if ("season_type" %in% names(pbp_raw)) {
      pbp <- pbp_raw %>%
        dplyr::filter(
          season_type == "REG",
          play_type %in% c("pass", "run") |
            dplyr::coalesce(two_point_attempt, 0L) == 1L
        )
    } else {
      warning(glue::glue(
        "Season {yr}: season_type column not found. ",
        "Including all season types (pre-season + playoffs may be included)."
      ))
      pbp <- pbp_raw %>%
        dplyr::filter(
          play_type %in% c("pass", "run") |
            dplyr::coalesce(two_point_attempt, 0L) == 1L
        )
    }

    rm(pbp_raw)
    gc()

    if (nrow(pbp) == 0L) {
      warning(glue::glue(
        "Season {yr}: No regular season pass/run plays found after filter. ",
        "Check cache for this season."
      ))
      rm(pbp)
      gc()
      next
    }

    if (verbose) {
      message(glue::glue("  {yr}: {format(nrow(pbp), big.mark = ',')} plays loaded."))
    }

    # Guarantee non-CORE_COLUMN guard columns exist with safe defaults.
    # normalize_schema() retains these from raw nflfastR but does not verify
    # them. Adding here ensures all helper functions can use coalesce() safely.
    if (!"qb_spike" %in% names(pbp)) pbp[["qb_spike"]] <- 0L
    if (!"qb_kneel" %in% names(pbp)) pbp[["qb_kneel"]] <- 0L

    # Aggregate stats by role
    if (verbose) message(glue::glue("  {yr}: Aggregating passing stats..."))
    pass_stats <- .build_passing_stats(pbp)

    if (verbose) message(glue::glue("  {yr}: Aggregating rushing stats..."))
    rush_stats <- .build_rushing_stats(pbp)

    if (verbose) message(glue::glue("  {yr}: Aggregating receiving stats..."))
    rec_stats  <- .build_receiving_stats(pbp)

    if (verbose) message(glue::glue("  {yr}: Counting games played..."))
    gp_stats   <- .build_games_played(pbp)

    rm(pbp)
    gc()

    # All unique player IDs with any offensive activity this season
    all_ids <- unique(c(pass_stats$player_id,
                         rush_stats$player_id,
                         rec_stats$player_id))

    if (verbose) {
      message(glue::glue("  {yr}: {length(all_ids)} unique player IDs. Joining..."))
    }

    # Join all stat types into one row per player
    player_season <- tibble::tibble(player_id = all_ids, season = yr) %>%
      dplyr::left_join(pass_stats,  by = "player_id") %>%
      dplyr::left_join(rush_stats,  by = "player_id") %>%
      dplyr::left_join(rec_stats,   by = "player_id") %>%
      dplyr::left_join(gp_stats,    by = "player_id") %>%
      dplyr::left_join(name_lookup, by = "player_id")

    # Join position from nflreadr (pre-loaded in Step 1)
    season_positions <- positions %>%
      dplyr::filter(season == yr) %>%
      dplyr::select(player_id, player_name, team, position,
                    depth_chart_position, position_group, has_name_collision)

    player_season <- player_season %>%
      dplyr::left_join(season_positions, by = "player_id") %>%
      # Use nflreadr name as primary. Fall back to play-level name if no match.
      dplyr::mutate(
        player_name = dplyr::coalesce(player_name, player_name_pbp)
      ) %>%
      dplyr::select(-player_name_pbp)

    # Compute panel-level summary columns and flags
    player_season <- player_season %>%
      dplyr::mutate(
        # total_plays: denominator for low_volume flag
        total_plays = dplyr::coalesce(as.integer(attempts), 0L) +
                      dplyr::coalesce(as.integer(rush_attempts), 0L) +
                      dplyr::coalesce(as.integer(targets), 0L),

        low_volume = total_plays < as.integer(min_plays),

        # total_tds: sum of all scoring plays.
        # NA-propagating intentionally -- if the source column was absent
        # in older nflfastR data, the NA propagates as a signal.
        total_tds = pass_tds + rush_tds + rec_tds,

        # total_yards: coalesce NA stats to 0 before summing.
        # A player with only receiving stats has 0 passing yards, not NA.
        total_yards = dplyr::coalesce(as.integer(passing_yards),  0L) +
                      dplyr::coalesce(as.integer(rushing_yards),  0L) +
                      dplyr::coalesce(as.integer(receiving_yards), 0L),

        # total_epa: sum across all play types. Coalesce to 0.
        total_epa = dplyr::coalesce(pass_epa, 0) +
                    dplyr::coalesce(rush_epa, 0) +
                    dplyr::coalesce(rec_epa, 0),

        panel_version = PANEL_VERSION
      )

    season_results[[i]] <- player_season

    if (verbose) {
      n_above <- sum(!player_season$low_volume, na.rm = TRUE)
      n_positioned <- sum(!is.na(player_season$position))
      message(glue::glue(
        "  {yr}: {nrow(player_season)} player-seasons | ",
        "{n_above} above min_plays | ",
        "{n_positioned} with roster position"
      ))
    }

    rm(pass_stats, rush_stats, rec_stats, gp_stats,
       player_season, season_positions, all_ids, name_lookup)
    gc()
  }

  # Bind all seasons into the final panel
  panel <- dplyr::bind_rows(season_results)
  rm(season_results)
  gc()

  if (nrow(panel) == 0L) {
    stop(
      "build_player_season_panel() returned an empty panel. ",
      "Check that the season2_cache/ contains data for the requested seasons."
    )
  }

  if (verbose) {
    message(glue::glue(
      "\n--- Panel complete ---",
      "\nRows           : {format(nrow(panel), big.mark = ',')}",
      "\nSeasons        : {min(panel$season, na.rm = TRUE)}-",
      "{max(panel$season, na.rm = TRUE)}",
      "\nUnique players : {format(dplyr::n_distinct(panel$player_id), big.mark = ',')}",
      "\nWith position  : {sum(!is.na(panel$position), na.rm = TRUE)}",
      "\nLow volume     : {sum(panel$low_volume, na.rm = TRUE)}"
    ))
  }

  return(panel)
}


# =============================================================================
# validate_panel_integrity()
# =============================================================================

#' Validate Player-Season Panel Integrity
#'
#' @description
#' Runs a battery of integrity checks on the player-season panel from
#' \code{\link{build_player_season_panel}}. Returns a named list with
#' per-check result tibbles, an overall \code{summary} tibble, and a
#' scalar \code{valid} flag.
#'
#' \strong{Checks performed:}
#' \enumerate{
#'   \item \strong{No duplicate rows} (critical): Each player_id + season
#'     combination must appear exactly once.
#'   \item \strong{No implausible negative yardage} (critical): passing_yards,
#'     rushing_yards, receiving_yards must be >= -50 where non-NA. Small
#'     negatives (e.g., -6 rushing yards from a jet sweep loss) are legitimate
#'     season totals. Values below -50 indicate an aggregation error.
#'   \item \strong{Games within era cap} (critical): games_played must not
#'     exceed 16 (pre-2021) or 17 (2021+).
#'   \item \strong{All players have position} (informational): Players with
#'     NA position have no nflreadr roster match. Common for specialists
#'     and historical edge cases.
#'   \item \strong{Position changes across seasons} (informational): Players
#'     who changed positions (e.g., WR to TE, RB to FB) across seasons.
#'     This is expected behavior, not an error.
#' }
#'
#' @param panel Tibble. Output of \code{\link{build_player_season_panel}}.
#' @param verbose Logical. Print summary to console. Default TRUE.
#'
#' @return A named list:
#'   \describe{
#'     \item{valid}{lgl: TRUE if all critical checks passed}
#'     \item{summary}{tibble: One row per check (check, critical, n_flagged, passed)}
#'     \item{duplicate_rows}{tibble: Duplicate player_id + season pairs}
#'     \item{negative_stats}{tibble: Rows with negative yardage values}
#'     \item{games_exceeded}{tibble: Players with games_played > era maximum}
#'     \item{unmatched_players}{tibble: Players with position == NA}
#'     \item{position_changes}{tibble: Players whose position changed across seasons}
#'   }
#'
#' @seealso \code{\link{build_player_season_panel}}
#'
#' @examples
#' panel <- build_player_season_panel(seasons = 2023:2024)
#' integrity <- validate_panel_integrity(panel)
#'
#' # Check overall validity
#' integrity$valid
#'
#' # Review position changes (informational)
#' integrity$position_changes
#'
#' # Any players with no roster match?
#' integrity$unmatched_players %>% select(player_id, player_name, season)
#'
#' @export
validate_panel_integrity <- function(panel, verbose = TRUE) {

  # Input validation
  if (!is.data.frame(panel) || nrow(panel) == 0L) {
    stop("panel must be a non-empty data frame. Run build_player_season_panel() first.")
  }

  required_cols <- c(
    "player_id", "season", "position", "position_group",
    "games_played", "total_plays", "passing_yards",
    "rushing_yards", "receiving_yards", "low_volume"
  )
  missing_cols <- setdiff(required_cols, names(panel))
  if (length(missing_cols) > 0L) {
    stop(glue::glue(
      "panel is missing required columns: {paste(missing_cols, collapse = ', ')}.\n",
      "Ensure this was produced by build_player_season_panel()."
    ))
  }

  results <- list()

  # ------------------------------------------------------------------
  # Check 1: Duplicate rows
  # Each player_id + season must be unique (one row per player-season).
  # ------------------------------------------------------------------
  dup_rows <- panel %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::filter(dplyr::n() > 1L) %>%
    dplyr::ungroup()

  results[["duplicate_rows"]] <- dup_rows

  # ------------------------------------------------------------------
  # Check 2: Implausible negative yardage
  # Small negative season totals are legitimate: a receiver whose only play
  # was a jet sweep loss will have negative rushing_yards. The threshold of
  # -50 flags only values that indicate an aggregation error, not real football.
  # EPA can be arbitrarily negative and is not checked here.
  # ------------------------------------------------------------------
  NEG_YARDS_THRESHOLD <- -50L

  neg_stats <- panel %>%
    dplyr::filter(
      dplyr::coalesce(passing_yards,  0L) < NEG_YARDS_THRESHOLD |
      dplyr::coalesce(rushing_yards,  0L) < NEG_YARDS_THRESHOLD |
      dplyr::coalesce(receiving_yards, 0L) < NEG_YARDS_THRESHOLD
    ) %>%
    dplyr::select(player_id, player_name, season, position,
                  passing_yards, rushing_yards, receiving_yards)

  results[["negative_stats"]] <- neg_stats

  # ------------------------------------------------------------------
  # Check 3: Games within era cap
  # 2021+: max 17 regular season games. Pre-2021: max 16.
  # ------------------------------------------------------------------
  games_exceeded <- panel %>%
    dplyr::filter(!is.na(games_played)) %>%
    dplyr::mutate(
      season_max = vapply(season, .max_games_for_season, integer(1L))
    ) %>%
    dplyr::filter(games_played > season_max) %>%
    dplyr::select(player_id, player_name, season, games_played, season_max)

  results[["games_exceeded"]] <- games_exceeded

  # ------------------------------------------------------------------
  # Check 4: Players with no roster position (informational)
  # These are typically specialists (long snappers appearing on trick plays),
  # players with old IDs not in nflreadr, or minor data gaps.
  # ------------------------------------------------------------------
  unmatched <- panel %>%
    dplyr::filter(is.na(position)) %>%
    dplyr::select(player_id, player_name, season, total_plays)

  results[["unmatched_players"]] <- unmatched

  # ------------------------------------------------------------------
  # Check 5: Position changes across seasons (informational)
  # Expected behavior for genuine position changes (RB -> FB, WR -> TE).
  # Surfaced here for documentation, not as a failure.
  # ------------------------------------------------------------------
  position_changes <- panel %>%
    dplyr::filter(!is.na(position)) %>%
    dplyr::group_by(player_id, player_name) %>%
    dplyr::summarise(
      seasons_active  = dplyr::n(),
      positions_seen  = paste(
        sort(unique(position)), collapse = " -> "
      ),
      n_distinct_positions = dplyr::n_distinct(position),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_distinct_positions > 1L) %>%
    dplyr::arrange(dplyr::desc(n_distinct_positions), player_name)

  results[["position_changes"]] <- position_changes

  # ------------------------------------------------------------------
  # Check 6: Name collisions (informational)
  # Players whose display name is shared by at least one other player_id
  # in the same season. player_id is always the correct join key.
  # ------------------------------------------------------------------
  if ("has_name_collision" %in% names(panel)) {
    name_collisions <- panel %>%
      dplyr::filter(has_name_collision == TRUE) %>%
      dplyr::select(player_id, player_name, season, position, team)
  } else {
    name_collisions <- tibble::tibble()
  }

  results[["name_collisions"]] <- name_collisions

  # ------------------------------------------------------------------
  # Summary tibble
  # ------------------------------------------------------------------
  summary_tbl <- tibble::tibble(
    check = c(
      "No duplicate player-season rows",
      "No implausible negative yardage (< -50)",
      "games_played within era cap",
      "All players matched to nflreadr roster",
      "Position changes flagged (informational)",
      "Name collisions flagged (informational)"
    ),
    critical  = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    n_flagged = c(
      nrow(dup_rows),
      nrow(neg_stats),
      nrow(games_exceeded),
      nrow(unmatched),
      nrow(position_changes),
      nrow(name_collisions)
    ),
    passed = c(
      nrow(dup_rows)       == 0L,
      nrow(neg_stats)      == 0L,
      nrow(games_exceeded) == 0L,
      nrow(unmatched)      == 0L,
      TRUE,
      TRUE
    )
  )

  results[["summary"]] <- summary_tbl

  # Overall validity: all CRITICAL checks must pass
  critical_rows <- summary_tbl %>% dplyr::filter(critical)
  results[["valid"]] <- all(critical_rows$passed)

  if (verbose) {
    message("\n--- validate_panel_integrity() ---")
    message(glue::glue("Panel rows     : {format(nrow(panel), big.mark = ',')}"))
    message(glue::glue(
      "Unique players : {format(dplyr::n_distinct(panel$player_id), big.mark = ',')}"
    ))
    message(glue::glue(
      "Seasons        : {min(panel$season, na.rm = TRUE)}-",
      "{max(panel$season, na.rm = TRUE)}"
    ))
    message("")

    for (i in seq_len(nrow(summary_tbl))) {
      row    <- summary_tbl[i, ]
      status <- dplyr::case_when(
        row$passed && row$critical  ~ "[PASS]",
        !row$passed && row$critical ~ "[FAIL]",
        row$passed && !row$critical ~ "[PASS]",
        TRUE                        ~ "[INFO]"
      )
      message(glue::glue(
        "{status} {row$check}: {row$n_flagged} flagged"
      ))
    }

    message("")
    overall_msg <- if (results$valid) {
      "VALID -- all critical checks passed."
    } else {
      "INVALID -- see $summary and individual check tibbles for details."
    }
    message(glue::glue("Overall: {overall_msg}"))

    if (nrow(position_changes) > 0L) {
      message(glue::glue(
        "\nPosition changes (top 5 by distinct positions):"
      ))
      print(head(position_changes, 5L))
    }
  }

  return(results)
}


# ==============================================================================
# SECTION: GAME-LEVEL SCORED PANEL  [2026-07-15, A2-b / B1]
# ==============================================================================
#
# WHY THIS EXISTS
# ---------------
# R/23_aging_curves.R historically computed fantasy points from the SEASON-level
# panel via its own compute_season_ppg(), which hardcoded eight scoring terms.
# That hardcode happened to match DK Best Ball on every term the season panel
# could support, but it was structurally scoring-blind: it could not represent
# fumbles, per-game threshold bonuses, tiered PPR, TE premium, rush attempt
# bonuses, sack penalties, superflex, or first-down points.
#
# Three of those are not a granularity problem, they are a data problem: the
# season panel never carried fumbles, first downs, or two-point conversions.
# The rest (100-yard games, 300-yard passing games, per-reception yardage tiers)
# are irreducibly GAME- or PLAY-level and cannot be recovered from season totals.
#
# The fix is to score at the play level through R/17's engine, which already
# implements every one of those terms, and roll up to a season fp_per_game.
#
# DESIGN
# ------
# Additive. build_player_season_panel() is untouched, as are all its consumers.
# attach_scored_ppg() takes an existing panel and OVERWRITES ONLY the fantasy
# points columns, deliberately reusing the panel's own games_played so that the
# only thing that changes versus the legacy path is the numerator. That isolates
# any downstream rank diff to the scoring change alone.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# Internal: stable cache key for a scoring settings list
# ------------------------------------------------------------------------------
.scoring_settings_key <- function(scoring_settings) {
  # Sort by name so that list order never changes the key. NULL-valued entries
  # (e.g. tiered_rec_tiers) are serialized as the literal "NULL" so that
  # absence and presence produce different keys.
  if (length(scoring_settings) == 0L) return("noscoring")

  nms <- sort(names(scoring_settings))
  flat <- vapply(nms, function(n) {
    v <- scoring_settings[[n]]
    if (is.null(v)) {
      paste0(n, "=NULL")
    } else {
      # format() drops names from named vectors, so configs differing only in
      # element names (e.g. long_td_tiers thresholds) would hash identically.
      # Serialize names alongside values explicitly.
      vals <- format(v, trim = TRUE, digits = 10)
      if (!is.null(names(v))) {
        vals <- paste0(names(v), "=", vals)
      }
      paste0(n, "=", paste(vals, collapse = ","))
    }
  }, character(1))

  # rlang::hash() is used rather than digest::digest() so that no new package
  # dependency is introduced: rlang is already required by dplyr.
  substr(rlang::hash(paste(flat, collapse = "|")), 1, 10)
}


# ------------------------------------------------------------------------------
# Internal: merge caller scoring against R/17 defaults and reject unknown keys
# ------------------------------------------------------------------------------
.resolve_scoring_settings <- function(scoring_settings) {

  if (!exists("get_ext_scoring_defaults", mode = "function")) {
    r17_path <- here::here("R", "17_extended_scoring.R")
    if (!file.exists(r17_path)) {
      stop(glue::glue(
        "R/17_extended_scoring.R not found at: {r17_path}\n",
        "This file is required for get_ext_scoring_defaults() and ",
        "calculate_fantasy_points_ext()."
      ))
    }
    source(r17_path)
  }

  defaults <- get_ext_scoring_defaults()

  if (is.null(scoring_settings)) return(defaults)

  if (!is.list(scoring_settings)) {
    stop("scoring_settings must be a named list, or NULL to use R/17 defaults.")
  }

  unknown <- setdiff(names(scoring_settings), names(defaults))
  if (length(unknown) > 0L) {
    stop(glue::glue(
      "scoring_settings contains keys R/17 does not implement: ",
      "{paste(unknown, collapse = ', ')}.\n",
      "Valid keys: {paste(sort(names(defaults)), collapse = ', ')}"
    ))
  }

  # utils::modifyList drops entries whose value is NULL, which would silently
  # revert an explicit tiered_rec_tiers = NULL to the default. Assign directly.
  out <- defaults
  for (n in names(scoring_settings)) out[n] <- list(scoring_settings[[n]])
  out
}


# ------------------------------------------------------------------------------
# Internal: hard-fail on scoring terms that the loaded PBP cannot support
# ------------------------------------------------------------------------------
# R/17 soft-checks several columns: if they are absent it silently scores them
# as zero (first_down_rush at R/17:859, first_down_pass at R/17:991), and a NULL
# roster_data silently disables te_premium. A scoring setting that is quietly
# ignored is worse than one that is honestly hardcoded, because the output looks
# parameterized and is not. Fail loudly instead.
# ------------------------------------------------------------------------------
.assert_pbp_supports_scoring <- function(pbp, scoring, season, has_roster) {

  problems <- character(0)

  # Numeric comparison, not identical(): a caller passing first_down_points = 0L
  # would fail identical(0L, 0) and trip the guard spuriously.
  .is_active <- function(x) !is.null(x) && !is.na(x) && x != 0

  if (.is_active(scoring$first_down_points) &&
      !all(c("first_down_rush", "first_down_pass") %in% names(pbp))) {
    problems <- c(problems, glue::glue(
      "first_down_points = {scoring$first_down_points} but season {season} PBP ",
      "is missing first_down_rush and/or first_down_pass. R/17 would silently ",
      "score these as zero."
    ))
  }

  if (isTRUE(scoring$te_premium) && !has_roster) {
    problems <- c(problems, glue::glue(
      "te_premium = TRUE but no roster_data was resolved. R/17 would silently ",
      "skip the TE premium."
    ))
  }

  if (.is_active(scoring$two_point_conversion) &&
      !"two_point_attempt" %in% names(pbp)) {
    problems <- c(problems, glue::glue(
      "two_point_conversion = {scoring$two_point_conversion} but season ",
      "{season} PBP is missing two_point_attempt."
    ))
  }

  if (length(problems) > 0L) {
    stop(glue::glue(
      "Scoring settings cannot be honored by the loaded play-by-play:\n  - ",
      "{paste(problems, collapse = '\n  - ')}"
    ), call. = FALSE)
  }

  invisible(TRUE)
}


#' Build a Player-Game Panel Scored Under Arbitrary League Settings
#'
#' @description
#' Loads normalized play-by-play one season at a time and scores it through
#' R/17's \code{calculate_fantasy_points_ext()}, returning player-GAME rows.
#' Unlike the season panel, this carries every scoring term R/17 implements,
#' including per-game threshold bonuses, tiered PPR, and first downs.
#'
#' Results are cached per season per scoring key. Entries are invalidated
#' automatically when the source PBP changes; see \code{force_refresh}.
#'
#' @param seasons Integer vector. Seasons to score. Default
#'   \code{PANEL_SEASONS_DEFAULT}.
#' @param scoring_settings Named list, or NULL for R/17 defaults. Keys must be a
#'   subset of \code{names(get_ext_scoring_defaults())}; unknown keys are an
#'   error.
#' @param cache_dir Character. Directory holding normalized PBP from R/15.
#' @param game_cache_dir Character. Directory for the scored game panel cache.
#'   Default \code{data/season3_cache}. Entries are named
#'   \code{s3_r16_gamepanel_<season>_<scoring_key>.rds}.
#' @param roster_data Data frame or NULL. Passed to R/17 for its position
#'   lookup. Supplying this explicitly DISABLES the cache, because the cache key
#'   cannot capture custom rosters. Prefer \code{use_roster} where possible.
#' @param use_roster Logical or NULL. Auto-load rosters via nflreadr for
#'   \code{seasons} and pass them to R/17. Affects both the TE premium and
#'   whether R/17's \code{position} column is roster-anchored or play-inferred.
#'   Auto-loaded rosters are deterministic given the season, so this IS folded
#'   into the cache key. NULL (default) resolves to TRUE only when
#'   \code{te_premium} is TRUE.
#' @param use_cache Logical. Read and write the game panel cache. Default TRUE.
#' @param force_refresh Logical. Rebuild every season and overwrite its cache
#'   entry, ignoring any existing one. Default FALSE.
#' @param verbose Logical. Print progress. Default TRUE.
#'
#' @return A tibble of player-game rows: the full column set returned by
#'   \code{calculate_fantasy_points_ext()}, stacked across seasons.
#'
#' @seealso \code{\link{attach_scored_ppg}}, \code{\link{build_player_season_panel}}


# ------------------------------------------------------------------------------
# Internal: game panel cache path and staleness-checked read/write
# ------------------------------------------------------------------------------
# Cached per SEASON rather than per season-range, so that a 2010:2025 run and a
# 2023:2024 run share entries.
#
# STALENESS. This project's recurring operational failure is a cache that no
# longer matches its source. The scoring key alone does not protect against it:
# if R/15's normalized PBP is regenerated, every game panel built from it is
# silently wrong. Each entry therefore records the size and mtime of the exact
# pbp_normalized_<season>.rds it was built from, and is rejected on read if
# either has moved. A rejected entry rebuilds; it never falls through as valid.
# ------------------------------------------------------------------------------

GAME_PANEL_CACHE_VERSION <- "s3_r16_gp_v1"

.game_panel_cache_path <- function(game_cache_dir, season, scoring_key) {
  file.path(
    game_cache_dir,
    sprintf("s3_r16_gamepanel_%d_%s.rds", as.integer(season), scoring_key)
  )
}

.pbp_source_fingerprint <- function(cache_dir, season) {
  src <- file.path(cache_dir, sprintf("pbp_normalized_%d.rds", as.integer(season)))
  if (!file.exists(src)) {
    return(list(path = src, exists = FALSE, size = NA_real_, mtime = NA))
  }
  info <- file.info(src)
  list(
    path   = src,
    exists = TRUE,
    size   = as.numeric(info$size),
    mtime  = as.character(info$mtime)
  )
}

.read_game_panel_cache <- function(path, scoring_key, fingerprint, verbose) {

  if (!file.exists(path)) return(NULL)

  entry <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(entry) || !is.list(entry) ||
      !all(c("meta", "data") %in% names(entry))) {
    if (verbose) message("    Cache entry unreadable or malformed. Rebuilding.")
    return(NULL)
  }

  m <- entry$meta

  if (!identical(m$cache_version, GAME_PANEL_CACHE_VERSION)) {
    if (verbose) message(glue::glue(
      "    Cache entry version '{m$cache_version}' != ",
      "'{GAME_PANEL_CACHE_VERSION}'. Rebuilding."
    ))
    return(NULL)
  }

  if (!identical(m$scoring_key, scoring_key)) {
    if (verbose) message("    Cache entry scoring key mismatch. Rebuilding.")
    return(NULL)
  }

  if (!identical(m$pbp_size, fingerprint$size) ||
      !identical(m$pbp_mtime, fingerprint$mtime)) {
    if (verbose) message(glue::glue(
      "    STALE: source PBP has changed since this entry was built ",
      "(built against size {m$pbp_size} / {m$pbp_mtime}; ",
      "source is now {fingerprint$size} / {fingerprint$mtime}). Rebuilding."
    ))
    return(NULL)
  }

  entry$data
}

.write_game_panel_cache <- function(path, data, scoring, scoring_key,
                                    season, fingerprint) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(
    list(
      meta = list(
        cache_version = GAME_PANEL_CACHE_VERSION,
        season        = as.integer(season),
        scoring_key   = scoring_key,
        scoring       = scoring,
        pbp_path      = fingerprint$path,
        pbp_size      = fingerprint$size,
        pbp_mtime     = fingerprint$mtime,
        n_rows        = nrow(data),
        built_at      = Sys.time()
      ),
      data = data
    ),
    file = path
  )
  invisible(path)
}


#' @export
build_player_game_panel <- function(
    seasons          = PANEL_SEASONS_DEFAULT,
    scoring_settings = NULL,
    cache_dir        = here::here("data", "season2_cache"),
    game_cache_dir   = here::here("data", "season3_cache"),
    roster_data      = NULL,
    use_roster       = NULL,
    use_cache        = TRUE,
    force_refresh    = FALSE,
    verbose          = TRUE
) {

  if (!is.numeric(seasons) || length(seasons) == 0L) {
    stop("seasons must be a non-empty numeric vector (e.g., 2010:2025).")
  }
  seasons <- sort(as.integer(seasons))

  # A caller-supplied roster changes R/17's position lookup but is not captured
  # by the scoring key, so a cache entry built from auto-loaded rosters would be
  # silently reused for it. Bypass the cache rather than risk that.
  #
  # use_roster is DIFFERENT: it auto-loads rosters via nflreadr for the requested
  # seasons, which is deterministic given the season, so it IS cacheable and is
  # folded into the cache key below.
  caller_supplied_roster <- !is.null(roster_data)
  if (caller_supplied_roster && use_cache) {
    if (verbose) message(
      "  roster_data supplied by caller: bypassing the game panel cache ",
      "(the cache key does not capture custom rosters)."
    )
    use_cache <- FALSE
  }

  if (!dir.exists(cache_dir)) {
    stop(glue::glue(
      "cache_dir not found: {cache_dir}\n",
      "Run load_multi_season_pbp() from R/15_multi_season_pbp.R first."
    ))
  }

  scoring <- .resolve_scoring_settings(scoring_settings)

  if (!exists("load_normalized_season", mode = "function")) {
    r15_path <- here::here("R", "15_multi_season_pbp.R")
    if (!file.exists(r15_path)) {
      stop(glue::glue("R/15_multi_season_pbp.R not found at: {r15_path}"))
    }
    if (verbose) message("Sourcing R/15_multi_season_pbp.R...")
    source(r15_path)
  }

  # use_roster default: TRUE only when the scoring actually needs a position
  # lookup. Callers that want R/17's `position` column roster-anchored rather
  # than play-inferred (R/45's build_actual_ppg does) pass TRUE explicitly.
  if (is.null(use_roster)) use_roster <- isTRUE(scoring$te_premium)

  if (isTRUE(scoring$te_premium) && !use_roster && !caller_supplied_roster) {
    stop(
      "te_premium = TRUE requires a position lookup, but use_roster = FALSE ",
      "and no roster_data was supplied. R/17 would silently skip the premium.",
      call. = FALSE
    )
  }

  # The roster choice changes R/17's output, so it must be part of the key or a
  # roster-anchored run would collide with a play-inferred one.
  scoring_key <- .scoring_settings_key(
    c(scoring, list(.use_roster = use_roster))
  )

  if (verbose) {
    message(glue::glue(
      "\n--- build_player_game_panel() ---",
      "\nSeasons     : {min(seasons)}-{max(seasons)} ({length(seasons)} seasons)",
      "\nScoring key : {scoring_key}",
      "\nRosters     : {if (caller_supplied_roster) 'caller-supplied' else if (use_roster) 'auto-loaded' else 'none (play-inferred position)'}",
      "\nPBP cache   : {cache_dir}",
      "\nPanel cache : {if (use_cache) game_cache_dir else '(disabled)'}",
      "{if (force_refresh) '\\nforce_refresh = TRUE: every season will be rebuilt.' else ''}"
    ))
  }

  # Roster resolution is DEFERRED until a cache miss actually needs it, so that
  # a fully cached run does not pay for an nflreadr call it will not use.
  roster_resolved <- caller_supplied_roster
  has_roster      <- !is.null(roster_data) && nrow(roster_data) > 0L

  .ensure_roster <- function() {
    if (roster_resolved) return(invisible(NULL))
    if (isTRUE(use_roster)) {
      if (verbose) message("    Loading rosters for R/17 position lookup...")
      roster_data <<- nflreadr::load_rosters(seasons = seasons)
      has_roster  <<- !is.null(roster_data) && nrow(roster_data) > 0L
    }
    roster_resolved <<- TRUE
    invisible(NULL)
  }

  season_results <- vector("list", length(seasons))
  n_hit <- 0L
  n_built <- 0L

  for (i in seq_along(seasons)) {
    yr <- seasons[i]
    if (verbose) message(glue::glue("\n  {i}/{length(seasons)}: Season {yr}..."))

    fingerprint <- .pbp_source_fingerprint(cache_dir, yr)
    cache_path  <- .game_panel_cache_path(game_cache_dir, yr, scoring_key)

    # --- Try the cache ---
    if (use_cache && !force_refresh) {
      cached <- .read_game_panel_cache(
        path        = cache_path,
        scoring_key = scoring_key,
        fingerprint = fingerprint,
        verbose     = verbose
      )
      if (!is.null(cached)) {
        if (verbose) message(glue::glue(
          "    Cache hit: {format(nrow(cached), big.mark = ',')} player-games."
        ))
        season_results[[i]] <- cached
        n_hit <- n_hit + 1L
        next
      }
    }

    # --- Miss: build it ---
    .ensure_roster()

    pbp_raw <- load_normalized_season(season = yr, cache_dir = cache_dir)

    # NOTE [2026-07-15]: do NOT add a season_type == "REG" filter here.
    # calculate_fantasy_points_ext() already applies one (R/17:410-414), so the
    # postseason never reaches the scoring components. Verified: season 2023
    # loads 285 games (272 regular + 13 playoff) and R/17 filters 49,665 plays
    # to 47,399 unmodified, producing the identical 5,372 player-games with or
    # without a filter here.
    #
    # A filter here would also be actively harmful in the edge case where
    # season_type is absent: R/17's filter is conditional on the column
    # existing, and build_player_season_panel() likewise warns and includes all
    # season types (R/16:850-857). In that case games_played and the scored
    # points BOTH include the postseason and remain consistent. Erroring out
    # here would break a path that works.
    #
    # The numerator/denominator invariant is asserted directly in
    # attach_scored_ppg() instead, which is the layer that actually couples them.

    .assert_pbp_supports_scoring(
      pbp        = pbp_raw,
      scoring    = scoring,
      season     = yr,
      has_roster = has_roster
    )

    # Subset rosters to THIS season before handing them to R/17. R/17 builds its
    # position lookup with distinct(player_id, .keep_all = TRUE) (R/17:463),
    # which keeps one arbitrary row per player. Passing all 16 seasons at once
    # would let a player who changed position (e.g. WR to TE) be scored under
    # the wrong position for every season. Subsetting confines that collision to
    # within-season, where it is far rarer.
    roster_yr <- roster_data
    if (has_roster && "season" %in% names(roster_data)) {
      roster_yr <- roster_data %>% dplyr::filter(season == yr)
      if (nrow(roster_yr) == 0L) {
        stop(glue::glue(
          "Season {yr}: roster_data contains no rows for this season, but ",
          "te_premium requires a position lookup."
        ), call. = FALSE)
      }
    }

    scored_yr <- do.call(
      calculate_fantasy_points_ext,
      c(
        list(
          pbp_data    = pbp_raw,
          roster_data = roster_yr,
          season      = yr
        ),
        scoring
      )
    )

    if (use_cache) {
      .write_game_panel_cache(
        path        = cache_path,
        data        = scored_yr,
        scoring     = scoring,
        scoring_key = scoring_key,
        season      = yr,
        fingerprint = fingerprint
      )
      if (verbose) message(glue::glue("    Cached -> {basename(cache_path)}"))
    }

    season_results[[i]] <- scored_yr
    n_built <- n_built + 1L

    rm(pbp_raw, scored_yr)
    gc()
  }

  out <- dplyr::bind_rows(season_results)

  if (nrow(out) == 0L) {
    stop("build_player_game_panel(): no player-games produced. Check cache_dir.")
  }

  if (verbose) {
    message(glue::glue(
      "\n  Complete. {format(nrow(out), big.mark = ',')} player-games across ",
      "{length(seasons)} seasons. ",
      "({n_hit} from cache, {n_built} rebuilt)"
    ))
  }

  out
}


#' Attach Correctly Scored Points Per Game to a Season Panel
#'
#' @description
#' Aggregates a player-game panel to season totals and joins the result onto an
#' existing season panel, replacing \code{season_fp} and \code{fp_per_game}.
#'
#' The panel's OWN \code{games_played} is used as the denominator, not a count
#' derived from the game panel. This is deliberate: it holds the denominator
#' fixed so that a diff against the legacy \code{compute_season_ppg()} path
#' isolates the scoring change and nothing else.
#'
#' @param panel Tibble. Output of \code{\link{build_player_season_panel}}. Must
#'   carry \code{player_id}, \code{season}, and \code{games_played}.
#' @param game_panel Tibble or NULL. Output of
#'   \code{\link{build_player_game_panel}}. If NULL it is built from
#'   \code{seasons} and \code{scoring_settings}.
#' @param seasons Integer vector. Only used when \code{game_panel} is NULL.
#'   Defaults to the seasons present in \code{panel}.
#' @param scoring_settings Named list or NULL. Only used when \code{game_panel}
#'   is NULL.
#' @param cache_dir Character. Only used when \code{game_panel} is NULL.
#' @param verbose Logical. Print progress and match diagnostics. Default TRUE.
#'
#' @return \code{panel} with \code{season_fp} and \code{fp_per_game} columns
#'   set from the scored game panel. Players present in the panel but absent
#'   from the game panel receive \code{season_fp = 0}.
#'
#' @seealso \code{\link{build_player_game_panel}}
#'
#' @export
attach_scored_ppg <- function(
    panel,
    game_panel       = NULL,
    seasons          = NULL,
    scoring_settings = NULL,
    cache_dir        = here::here("data", "season2_cache"),
    game_cache_dir   = here::here("data", "season3_cache"),
    use_cache        = TRUE,
    force_refresh    = FALSE,
    verbose          = TRUE
) {

  if (!is.data.frame(panel) || nrow(panel) == 0L) {
    stop("panel must be a non-empty data frame.")
  }
  required <- c("player_id", "season", "games_played")
  missing_cols <- setdiff(required, names(panel))
  if (length(missing_cols) > 0L) {
    stop(glue::glue(
      "panel is missing columns: {paste(missing_cols, collapse = ', ')}."
    ))
  }

  if (is.null(game_panel)) {
    if (is.null(seasons)) seasons <- sort(unique(as.integer(panel$season)))
    game_panel <- build_player_game_panel(
      seasons          = seasons,
      scoring_settings = scoring_settings,
      cache_dir        = cache_dir,
      game_cache_dir   = game_cache_dir,
      use_cache        = use_cache,
      force_refresh    = force_refresh,
      verbose          = verbose
    )
  }

  if (!"total_fantasy_points" %in% names(game_panel)) {
    stop(
      "game_panel is missing total_fantasy_points. ",
      "Expected the output of build_player_game_panel()."
    )
  }

  season_fp_tbl <- game_panel %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      season_fp_scored = sum(total_fantasy_points, na.rm = TRUE),
      n_games_scored   = dplyr::n_distinct(game_id),
      .groups = "drop"
    ) %>%
    dplyr::mutate(season = as.integer(season))

  out <- panel %>%
    dplyr::mutate(season = as.integer(season)) %>%
    dplyr::left_join(season_fp_tbl, by = c("player_id", "season"))

  # --------------------------------------------------------------------------
  # DENOMINATOR CONSISTENCY ASSERTION  [2026-07-15]
  # --------------------------------------------------------------------------
  # The numerator comes from the game panel; the denominator is the season
  # panel's own games_played. Those two only agree if both were built from the
  # same filters. On 2026-07-15 they were not: build_player_game_panel() scored
  # the postseason while the season panel is season_type == "REG" only, which
  # inflated fp_per_game for playoff participants in an age-correlated way.
  #
  # A player can legitimately be scored in FEWER games than games_played (he
  # played but recorded no scoring-relevant plays). He can never legitimately be
  # scored in MORE games than he played. Any such row means the two paths
  # disagree about which games count.
  # --------------------------------------------------------------------------
  over_scored <- out %>%
    dplyr::filter(
      !is.na(n_games_scored),
      !is.na(games_played),
      n_games_scored > games_played
    )

  if (nrow(over_scored) > 0L) {
    ex <- over_scored %>%
      dplyr::arrange(dplyr::desc(n_games_scored - games_played)) %>%
      dplyr::slice_head(n = 5)
    stop(glue::glue(
      "attach_scored_ppg(): {nrow(over_scored)} player-season(s) were scored in ",
      "MORE games than the panel says they played. The game panel and the season ",
      "panel disagree about which games count (check the season_type / week ",
      "filters on both sides).\n",
      "Worst offenders:\n",
      "{paste(sprintf('  %s (%s): scored %d games, panel says %d', ex$player_id, ex$season, ex$n_games_scored, ex$games_played), collapse = '\n')}"
    ), call. = FALSE)
  }

  out <- out %>% dplyr::select(-n_games_scored)

  n_unmatched <- sum(is.na(out$season_fp_scored))
  pct_unmatched <- n_unmatched / nrow(out)

  # Coalescing an unmatched join to zero is exactly the failure that produced
  # the R/32 volume bug on 2026-07-15: an NA turned into a real zero and the
  # wrongness looked like data. A handful of unmatched rows is expected (panel
  # players with no scoring-relevant plays). A systematic player_id mismatch
  # between R/16 and R/17 would show up as MASS unmatching, so refuse to
  # coalesce past a threshold rather than quietly zeroing out the panel.
  if (pct_unmatched > 0.05) {
    stop(glue::glue(
      "attach_scored_ppg(): {round(100 * pct_unmatched, 1)}% of panel rows ",
      "({format(n_unmatched, big.mark = ',')} of {format(nrow(out), big.mark = ',')}) ",
      "found no match in the scored game panel. That is too high to be players ",
      "with no scoring plays, and points at a player_id or season-type mismatch ",
      "between R/16 and R/17. Refusing to coalesce these to zero. ",
      "Inspect the join before proceeding."
    ), call. = FALSE)
  }

  out <- out %>%
    dplyr::mutate(
      season_fp = dplyr::coalesce(season_fp_scored, 0),
      fp_per_game = dplyr::if_else(
        dplyr::coalesce(games_played, 0L) > 0L,
        season_fp / games_played,
        NA_real_
      )
    ) %>%
    dplyr::select(-season_fp_scored)

  if (verbose) {
    message(glue::glue(
      "\n  attach_scored_ppg(): {format(nrow(out) - n_unmatched, big.mark = ',')}",
      " of {format(nrow(out), big.mark = ',')} panel rows matched a scored ",
      "player-season."
    ))
    if (n_unmatched > 0L) {
      message(glue::glue(
        "  {format(n_unmatched, big.mark = ',')} unmatched rows set to ",
        "season_fp = 0. These are players in the panel with no scoring-relevant ",
        "plays (e.g. blockers, special teams only)."
      ))
    }
  }

  out
}
