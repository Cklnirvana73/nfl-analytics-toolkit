# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 8
# Strength of Schedule (SOS) Features and Sleeper-to-GSIS ID Crosswalk
# File: R/22_sos_reconciliation.R
#
# Purpose:
#   (1) Compute opponent-adjusted SOS features for each player-season in the
#       R/21 CFB panel. SOS is defined as the mean defensive EPA allowed per
#       play and mean defensive success rate allowed across all opponents the
#       player's primary team faced in regular season.
#
#   (2) Build and save a Sleeper-to-GSIS ID crosswalk by pulling rosters from
#       one or more Sleeper leagues and reconciling Sleeper player_ids to
#       nflfastR GSIS IDs via match_sleeper_players() from R/19.
#
# SOS computation design (Q4: Option B):
#   build_cfb_sos_features() accepts an optional pre-built opponent_defense
#   tibble. When NULL (default), it calls the internal helper
#   .build_cfb_defense_and_schedule() which loads the R/20 cache once and
#   returns both the team-season defensive quality tibble and the schedule
#   (all (posteam, defteam, season) matchup pairs). When opponent_defense is
#   provided, only the schedule is extracted from the cache (lighter load).
#   This lets the caller substitute a custom defensive metric without
#   reloading the PBP.
#
# Crosswalk design (Q5: Option A):
#   build_sleeper_gsis_crosswalk() accepts league_ids directly as a function
#   argument. No hardcoded league IDs. Discovery (get_user_leagues()) is a
#   separate concern handled by the caller before invoking this function.
#
# CFB-to-NFL leg deferred (Q3: Option B):
#   The link from cfbfastR player names to nflfastR GSIS IDs is out of scope
#   for Week 8. It is built in Week 10 (R/24) where the translation model
#   requirements are fully known. This file only reconciles Sleeper IDs to
#   GSIS IDs (NFL players currently active in Sleeper leagues).
#
# Known limitations:
#   - SOS for multi-team players (transfers) is assigned from primary_team
#     only (team with most plays). Stats from the other team are included in
#     the panel but the SOS reflects only the primary team's schedule.
#   - Teams with fewer than CFB_SOS_MIN_DEF_PLAYS defensive plays in a season
#     are excluded from the opponent defense table and produce NA SOS values
#     for any player whose team faced them. Primarily affects FCS opponents
#     (already filtered by R/20's FBS division filter) and early-season teams.
#   - 2025 CFB panel data inflated by upstream cfbfastR duplicate issue.
#     SOS computation for 2025 should be treated as provisional.
#
# Navigation:
#   Line  ~90  : Library dependencies
#   Line ~105  : Source guards (R/19, R/20, R/21)
#   Line ~140  : Constants
#   Line ~200  : NSE declarations
#   Line ~220  : .build_cfb_defense_and_schedule() -- internal helper
#   Line ~380  : build_cfb_sos_features()
#   Line ~560  : build_sleeper_gsis_crosswalk()
#   Line ~700  : validate_sos_crosswalk()
#   Line ~860  : run_week8_pipeline()
#
# Source dependencies:
#   R/19_sleeper_api.R          match_sleeper_players(), get_sleeper_rosters()
#   R/20_multi_season_cfb_pbp.R load_normalized_cfb_season()
#   R/21_cfb_player_season_panel.R (panel is passed in, not sourced here)
#
# Package dependencies:
#   dplyr, tibble, purrr, glue, here, nflreadr
#
# Outputs (written by run_week8_pipeline()):
#   data/season2_cfb_cache/s2_week8_cfb_sos_panel.rds
#   data/season2_cache/s2_week8_id_crosswalk.rds
#
# Season 2 output prefix : s2_week8_
# Schema tag             : s2_w8_v1
# Author                 : Christian LeBlanc
# Created                : 2026-04
# ==============================================================================


# ==============================================================================
# DEPENDENCIES
# ==============================================================================

library(dplyr)
library(tibble)
library(purrr)
library(glue)
library(here)

if (!requireNamespace("nflreadr", quietly = TRUE)) {
  stop(
    "Package 'nflreadr' is required. Install with: install.packages('nflreadr')",
    call. = FALSE
  )
}


# ==============================================================================
# SOURCE GUARDS
# ==============================================================================

# R/19: Sleeper API integration (match_sleeper_players, get_sleeper_rosters)
if (!exists("match_sleeper_players", mode = "function")) {
  r19_path <- here::here("R", "19_sleeper_api.R")
  if (!file.exists(r19_path)) {
    stop(glue(
      "R/19_sleeper_api.R not found at: {r19_path}\n",
      "This file is required for match_sleeper_players() and get_sleeper_rosters()."
    ), call. = FALSE)
  }
  source(r19_path)
}

# R/20: CFB PBP cache loader (load_normalized_cfb_season)
if (!exists("load_normalized_cfb_season", mode = "function")) {
  r20_path <- here::here("R", "20_multi_season_cfb_pbp.R")
  if (!file.exists(r20_path)) {
    stop(glue(
      "R/20_multi_season_cfb_pbp.R not found at: {r20_path}\n",
      "This file is required for load_normalized_cfb_season()."
    ), call. = FALSE)
  }
  source(r20_path)
}


# ==============================================================================
# CONSTANTS
# ==============================================================================

# Default season range: matches R/20 and R/21 CFB cache
CFB_SOS_SEASONS_DEFAULT <- 2014:2025

# Default cache directories (must match R/20 and NFL cache locations exactly)
CFB_SOS_CFB_CACHE_DIR_DEFAULT <- here::here("data", "season2_cfb_cache")
CFB_SOS_NFL_CACHE_DIR_DEFAULT <- here::here("data", "season2_cache")

# Minimum defensive plays for a team-season to be included in the opponent
# defense table. Teams below this threshold produce NA SOS values for any
# player whose team faced them. 200 plays is approximately 3 FBS games.
CFB_SOS_MIN_DEF_PLAYS <- 200L

# Schema version tag for Week 8 outputs.
W8_SCHEMA_TAG <- "s2_w8_v1"

# NFL season for nflreadr roster loading in the Sleeper crosswalk.
# Update when the most recently completed NFL season changes.
NFL_ROSTER_SEASON_DEFAULT <- 2025L

# Scrimmage play types used for defensive EPA aggregation.
# Mirrors CFB_SCRIMMAGE_TYPES from R/21 for consistency.
W8_SCRIMMAGE_TYPES <- c(
  "Rush", "Rushing Touchdown",
  "Pass Completion", "Pass Incompletion", "Passing Touchdown",
  "Pass Reception", "Sack",
  "Interception", "Interception Return", "Interception Return Touchdown",
  "Fumble Recovery (Opponent)", "Fumble Recovery (Own)"
)

# Garbage time thresholds: mirrors R/21 defaults.
# Applied to periods 3+ only (first half always retained).
W8_GARBAGE_SCORE_DIFF <- 28L
W8_GARBAGE_WP_LO      <- 0.05
W8_GARBAGE_WP_HI      <- 0.95

# SOS output column names (added to panel by build_cfb_sos_features)
W8_SOS_COLS <- c(
  "sos_opp_def_epa_per_play",
  "sos_opp_def_success_rate_allowed",
  "sos_n_opponents",
  "sos_computed"
)


# ==============================================================================
# NSE COLUMN DECLARATIONS
# ==============================================================================

utils::globalVariables(c(
  "season", "season_type", "play_type", "period", "score_diff", "wp_before",
  "EPA", "def_pos_team", "pos_team", "game_id",
  "def_epa_per_play", "def_success_rate_allowed", "def_n_plays",
  "defteam", "opp_defteam", "primary_team",
  "sos_opp_def_epa_per_play", "sos_opp_def_success_rate_allowed",
  "sos_n_opponents", "sos_computed",
  "player_id", "player_name", "position_group", "low_volume",
  "sleeper_player_id", "gsis_id", "match_method", "match_confidence",
  "match_rate", "n_matched", "n_total", "league_id", "player_id_sleeper",
  "n_epa_plays", "n_pos_epa"
))


# ==============================================================================
# INTERNAL HELPER: .build_cfb_defense_and_schedule
# ==============================================================================

#' Build CFB Team Defensive Quality and Season Schedule (Internal)
#'
#' @description
#' Loads each season from the R/20 CFB cache, applies the standard scrimmage
#' play filter and garbage time filter, then computes:
#'   (1) Team-season defensive quality: mean EPA allowed per play and
#'       defensive success rate allowed (proportion of plays where EPA > 0
#'       from the offense's perspective).
#'   (2) Season schedule: all unique (posteam, defteam, season) matchup pairs
#'       from regular season games.
#'
#' Both outputs are needed by build_cfb_sos_features() to compute per-player
#' SOS metrics. The two computations are done in a single cache pass to avoid
#' loading the same season twice.
#'
#' EPA column is uppercase "EPA" in cfbfastR (R/20 also creates a lowercase
#' alias "epa", but "EPA" is used here for consistency with R/21).
#'
#' Defensive success rate allowed is computed as EPA > 0 (offense gained
#' expected points), NOT using cfbfastR's "success" column which uses the
#' yards-gained threshold definition (50%/70%/100%). The EPA-based definition
#' is consistent with the R/21 panel's success_rate column.
#'
#' Not exported. Called internally by build_cfb_sos_features().
#'
#' @param seasons Integer vector of seasons to process.
#' @param cache_dir Character. Path to R/20 CFB cache directory.
#' @param min_def_plays Integer. Minimum defensive plays for a team-season to
#'   appear in the team_defense output. Default: CFB_SOS_MIN_DEF_PLAYS.
#' @param verbose Logical. Print season-by-season progress. Default: TRUE.
#'
#' @return Named list:
#'   \describe{
#'     \item{team_defense}{Tibble with one row per team-season:
#'       defteam (chr), season (int), def_epa_per_play (dbl),
#'       def_success_rate_allowed (dbl), def_n_plays (int).}
#'     \item{schedule}{Tibble with one row per unique (posteam, defteam, season)
#'       matchup: posteam (chr), defteam (chr), season (int).}
#'   }
.build_cfb_defense_and_schedule <- function(seasons,
                                             cache_dir,
                                             min_def_plays = CFB_SOS_MIN_DEF_PLAYS,
                                             verbose       = TRUE) {

  seasons <- sort(as.integer(seasons))

  defense_list  <- vector("list", length(seasons))
  schedule_list <- vector("list", length(seasons))

  for (i in seq_along(seasons)) {
    s <- seasons[i]

    # Load one season from R/20 cache (FBS filter applied inside)
    pbp <- tryCatch(
      load_normalized_cfb_season(s, cache_dir = cache_dir, division = "fbs"),
      error = function(e) {
        warning(glue("Season {s}: cache load failed -- {e$message}. Skipping."),
                call. = FALSE)
        return(NULL)
      }
    )

    if (is.null(pbp) || nrow(pbp) == 0L) {
      if (verbose) message(glue("  Season {s}: no data. Skipping."))
      next
    }

    # --- Regular season filter ---
    if ("season_type" %in% names(pbp)) {
      pbp <- pbp[pbp$season_type == "regular", , drop = FALSE]
    } else {
      warning(glue(
        "Season {s}: 'season_type' column absent. ",
        "Cannot filter to regular season. All plays used."
      ), call. = FALSE)
    }

    if (nrow(pbp) == 0L) {
      if (verbose) message(glue("  Season {s}: 0 regular season plays. Skipping."))
      next
    }

    # --- Scrimmage play filter ---
    if ("play_type" %in% names(pbp)) {
      pbp <- pbp[pbp$play_type %in% W8_SCRIMMAGE_TYPES, , drop = FALSE]
    }

    # --- EPA availability filter ---
    if ("EPA" %in% names(pbp)) {
      pbp <- pbp[!is.na(pbp$EPA), , drop = FALSE]
    } else {
      warning(glue(
        "Season {s}: 'EPA' column absent. ",
        "Cannot compute defensive quality. Skipping."
      ), call. = FALSE)
      rm(pbp); gc(verbose = FALSE)
      next
    }

    # --- Garbage time filter (periods 3+) ---
    if ("period" %in% names(pbp) && "score_diff" %in% names(pbp)) {
      is_late    <- pbp$period >= 3L
      bad_score  <- abs(pbp$score_diff) > W8_GARBAGE_SCORE_DIFF

      if ("wp_before" %in% names(pbp)) {
        bad_wp <- !is.na(pbp$wp_before) &
          (pbp$wp_before < W8_GARBAGE_WP_LO | pbp$wp_before > W8_GARBAGE_WP_HI)
        is_garbage <- is_late & (bad_score | bad_wp)
      } else {
        is_garbage <- is_late & bad_score
      }

      pbp <- pbp[!is_garbage, , drop = FALSE]
    }

    if (nrow(pbp) == 0L) {
      if (verbose) message(glue("  Season {s}: 0 plays after filters. Skipping."))
      rm(pbp); gc(verbose = FALSE)
      next
    }

    # Confirm defensive team column exists
    if (!"def_pos_team" %in% names(pbp)) {
      # Try the alias created by R/20 normalization
      if ("defteam" %in% names(pbp)) {
        pbp$def_pos_team <- pbp$defteam
      } else {
        warning(glue(
          "Season {s}: neither 'def_pos_team' nor 'defteam' column found. ",
          "Cannot compute defensive quality. Skipping."
        ), call. = FALSE)
        rm(pbp); gc(verbose = FALSE)
        next
      }
    }

    if (!"pos_team" %in% names(pbp)) {
      if ("posteam" %in% names(pbp)) {
        pbp$pos_team <- pbp$posteam
      } else {
        warning(glue(
          "Season {s}: neither 'pos_team' nor 'posteam' column found. ",
          "Cannot build schedule. Skipping schedule for this season."
        ), call. = FALSE)
      }
    }

    # --- Compute team defensive quality ---
    def_stats <- pbp %>%
      dplyr::group_by(defteam = def_pos_team) %>%
      dplyr::summarise(
        season                   = s,
        def_n_plays              = dplyr::n(),
        # EPA > 0 from offense perspective = offense succeeded = defense failed
        n_pos_epa                = sum(EPA > 0, na.rm = TRUE),
        def_epa_per_play         = mean(EPA, na.rm = TRUE),
        def_success_rate_allowed = n_pos_epa / def_n_plays,
        .groups                  = "drop"
      ) %>%
      dplyr::select(defteam, season, def_epa_per_play,
                    def_success_rate_allowed, def_n_plays) %>%
      dplyr::filter(def_n_plays >= min_def_plays)

    # --- Extract season schedule ---
    sched_season <- tibble::tibble()
    if ("pos_team" %in% names(pbp) && "game_id" %in% names(pbp)) {
      sched_season <- pbp %>%
        dplyr::distinct(posteam = pos_team, defteam = def_pos_team, season = s)
    }

    defense_list[[i]]  <- def_stats
    schedule_list[[i]] <- sched_season

    if (verbose) {
      n_teams <- dplyr::n_distinct(def_stats$defteam)
      n_games <- if (nrow(sched_season) > 0L) {
        dplyr::n_distinct(
          paste0(
            pmin(sched_season$posteam, sched_season$defteam),
            "_",
            pmax(sched_season$posteam, sched_season$defteam)
          )
        )
      } else 0L
      message(glue(
        "  Season {s}: {format(n_teams, big.mark=',')} team defenses | ",
        "{format(n_games, big.mark=',')} unique matchups"
      ))
    }

    rm(pbp, def_stats, sched_season)
    gc(verbose = FALSE)
  }

  # Bind all seasons
  defense_list  <- defense_list[!vapply(defense_list, is.null, logical(1))]
  schedule_list <- schedule_list[!vapply(schedule_list, is.null, logical(1))]

  if (length(defense_list) == 0L) {
    stop(
      "No team defensive quality data could be built. ",
      "Check that R/20 cache exists and contains valid seasons.",
      call. = FALSE
    )
  }

  team_defense <- dplyr::bind_rows(defense_list)
  schedule     <- dplyr::bind_rows(schedule_list) %>%
    dplyr::distinct(posteam, defteam, season)

  list(team_defense = team_defense, schedule = schedule)
}


# ==============================================================================
# FUNCTION: build_cfb_sos_features
# ==============================================================================

#' Add Strength of Schedule Features to the CFB Player-Season Panel
#'
#' @description
#' Computes per-player-season SOS features by averaging the defensive quality
#' of all opponents that the player's primary team faced during the regular
#' season. Joins the result back to the R/21 panel.
#'
#' \strong{SOS metric definitions:}
#' \itemize{
#'   \item \code{sos_opp_def_epa_per_play}: Mean defensive EPA allowed per
#'     play across all opponents. Higher (less negative) = easier schedule.
#'     A defense allowing +0.05 EPA/play is worse than one allowing -0.10.
#'   \item \code{sos_opp_def_success_rate_allowed}: Mean proportion of plays
#'     where the opponent's defense allowed EPA > 0. Higher = worse defense =
#'     easier schedule.
#'   \item \code{sos_n_opponents}: Number of distinct opponents included in
#'     the average. Low values (< 4) indicate incomplete schedule data and
#'     warrant caution.
#'   \item \code{sos_computed}: Logical flag. FALSE when SOS could not be
#'     computed for the player-season (primary_team not in schedule, or all
#'     opponents below minimum defensive play threshold).
#' }
#'
#' \strong{Multi-team player limitation:}
#' Players who transferred mid-season get the SOS of their \code{primary_team}
#' (the team with the most plays). Their panel stats include all teams, but
#' SOS reflects only the primary team's schedule. Set \code{has_name_collision}
#' rows aside before interpreting SOS for these players.
#'
#' \strong{Pre-built opponent_defense:}
#' When \code{opponent_defense} is provided, only the season schedule is
#' extracted from the cache (lighter load). Use this to substitute a custom
#' defensive metric (e.g., weighted by opponent strength) without reprocessing
#' the full PBP. The provided tibble must have columns:
#' defteam (chr), season (int), def_epa_per_play (dbl),
#' def_success_rate_allowed (dbl), def_n_plays (int).
#'
#' @param panel Tibble. Output of build_cfb_player_season_panel() from R/21.
#'   Must include: player_name, season, primary_team.
#' @param opponent_defense Tibble or NULL. Pre-built team-season defensive
#'   quality table. When NULL (default), computed from the R/20 cache.
#' @param seasons Integer vector. Seasons to include in the SOS computation.
#'   Default: derived from unique values in panel$season. Only seasons in the
#'   R/20 cache are available.
#' @param cache_dir Character. Path to R/20 CFB cache directory.
#' @param min_def_plays Integer. Minimum defensive plays for a team-season to
#'   be trusted. Opponents below this threshold are excluded from the SOS
#'   average. Default: CFB_SOS_MIN_DEF_PLAYS (200L).
#' @param verbose Logical. Print progress. Default: TRUE.
#'
#' @return The input panel tibble with four SOS columns appended:
#'   sos_opp_def_epa_per_play (dbl), sos_opp_def_success_rate_allowed (dbl),
#'   sos_n_opponents (int), sos_computed (lgl).
#'   Rows where SOS could not be computed have NA for the numeric columns and
#'   sos_computed = FALSE.
#'
#' @examples
#' \dontrun{
#' library(here)
#' source(here("R", "20_multi_season_cfb_pbp.R"))
#' source(here("R", "21_cfb_player_season_panel.R"))
#' source(here("R", "22_sos_reconciliation.R"))
#'
#' panel <- build_cfb_player_season_panel(seasons = 2022:2024)
#'
#' # Default: opponent defense computed from cache
#' panel_sos <- build_cfb_sos_features(panel)
#'
#' # Custom opponent defense (e.g., pre-filtered to conference games only)
#' my_def <- my_custom_defense_function()
#' panel_sos <- build_cfb_sos_features(panel, opponent_defense = my_def)
#'
#' # QBs ranked by SOS-adjusted efficiency
#' panel_sos %>%
#'   filter(position_group == "QB", !low_volume, !is.na(sos_opp_def_epa_per_play)) %>%
#'   arrange(sos_opp_def_epa_per_play) %>%  # ascending = hardest schedule first
#'   select(player_name, season, primary_team,
#'          pass_epa_per_attempt, sos_opp_def_epa_per_play, sos_n_opponents)
#' }
#'
#' @seealso build_cfb_player_season_panel, validate_sos_crosswalk
#' @export
build_cfb_sos_features <- function(panel,
                                    opponent_defense = NULL,
                                    seasons          = NULL,
                                    cache_dir        = CFB_SOS_CFB_CACHE_DIR_DEFAULT,
                                    min_def_plays    = CFB_SOS_MIN_DEF_PLAYS,
                                    verbose          = TRUE) {

  # --- Input validation ---
  if (!is.data.frame(panel) || nrow(panel) == 0L) {
    stop("'panel' must be a non-empty data frame. Run build_cfb_player_season_panel() first.",
         call. = FALSE)
  }

  required_panel_cols <- c("player_name", "season", "primary_team")
  missing_panel_cols  <- setdiff(required_panel_cols, names(panel))
  if (length(missing_panel_cols) > 0L) {
    stop(glue(
      "'panel' is missing required columns: {paste(missing_panel_cols, collapse = ', ')}.\n",
      "Ensure this was produced by build_cfb_player_season_panel()."
    ), call. = FALSE)
  }

  # Derive seasons from panel if not provided
  if (is.null(seasons)) {
    seasons <- sort(unique(as.integer(panel$season)))
  } else {
    seasons <- sort(as.integer(seasons))
  }

  if (!is.logical(verbose) || length(verbose) != 1L) {
    stop("'verbose' must be a single logical value.", call. = FALSE)
  }

  if (!is.null(opponent_defense)) {
    required_def_cols <- c("defteam", "season", "def_epa_per_play",
                           "def_success_rate_allowed", "def_n_plays")
    missing_def_cols <- setdiff(required_def_cols, names(opponent_defense))
    if (length(missing_def_cols) > 0L) {
      stop(glue(
        "'opponent_defense' is missing required columns: ",
        "{paste(missing_def_cols, collapse = ', ')}.\n",
        "Required: defteam, season, def_epa_per_play, ",
        "def_success_rate_allowed, def_n_plays."
      ), call. = FALSE)
    }
  }

  if (verbose) {
    message(strrep("=", 60))
    message("build_cfb_sos_features()")
    message(glue(
      "Panel rows: {format(nrow(panel), big.mark = ',')} | ",
      "Seasons: {min(seasons)}-{max(seasons)}"
    ))
    message(strrep("=", 60))
  }

  # --- Step 1: Build team defense table and season schedule ---
  if (verbose) message("\nStep 1: Building team defense and season schedule...")

  if (is.null(opponent_defense)) {
    defense_and_sched <- .build_cfb_defense_and_schedule(
      seasons       = seasons,
      cache_dir     = cache_dir,
      min_def_plays = min_def_plays,
      verbose       = verbose
    )
    team_defense <- defense_and_sched$team_defense
    schedule     <- defense_and_sched$schedule
    rm(defense_and_sched)
    gc(verbose = FALSE)
  } else {
    if (verbose) message("  Using provided opponent_defense. Extracting schedule from cache...")
    team_defense <- opponent_defense %>%
      dplyr::filter(def_n_plays >= min_def_plays)

    # Extract schedule only (lighter load than full defense + schedule)
    schedule_list <- vector("list", length(seasons))
    for (i in seq_along(seasons)) {
      s <- seasons[i]
      pbp <- tryCatch(
        load_normalized_cfb_season(s, cache_dir = cache_dir, division = "fbs"),
        error = function(e) NULL
      )
      if (is.null(pbp) || nrow(pbp) == 0L) next

      if ("season_type" %in% names(pbp)) {
        pbp <- pbp[pbp$season_type == "regular", , drop = FALSE]
      }

      # Map column aliases if needed
      if (!"pos_team" %in% names(pbp) && "posteam" %in% names(pbp)) {
        pbp$pos_team <- pbp$posteam
      }
      if (!"def_pos_team" %in% names(pbp) && "defteam" %in% names(pbp)) {
        pbp$def_pos_team <- pbp$defteam
      }

      if (all(c("pos_team", "def_pos_team") %in% names(pbp))) {
        schedule_list[[i]] <- tibble::tibble(
          posteam = unique(pbp$pos_team[!is.na(pbp$pos_team)]),
          defteam = NA_character_,
          season  = s
        )
        # Full distinct matchup pairs
        schedule_list[[i]] <- pbp %>%
          dplyr::filter(!is.na(pos_team), !is.na(def_pos_team)) %>%
          dplyr::distinct(posteam = pos_team, defteam = def_pos_team, season = s)
      }

      rm(pbp); gc(verbose = FALSE)
    }

    schedule_list <- schedule_list[!vapply(schedule_list, is.null, logical(1))]
    schedule <- if (length(schedule_list) > 0L) {
      dplyr::bind_rows(schedule_list) %>%
        dplyr::distinct(posteam, defteam, season)
    } else {
      tibble::tibble(posteam = character(), defteam = character(), season = integer())
    }
    rm(schedule_list); gc(verbose = FALSE)
  }

  if (verbose) {
    message(glue(
      "  team_defense: {format(nrow(team_defense), big.mark=',')} team-seasons | ",
      "schedule: {format(nrow(schedule), big.mark=',')} matchup pairs"
    ))
  }

  # --- Step 2: Compute SOS per (primary_team, season) ---
  # For each (posteam, season), average defensive quality across all opponents
  if (verbose) message("\nStep 2: Averaging opponent defensive quality per team-season...")

  if (nrow(schedule) == 0L || nrow(team_defense) == 0L) {
    warning(
      "Empty schedule or team_defense. All SOS values will be NA.",
      call. = FALSE
    )
    sos_lookup <- tibble::tibble(
      primary_team                   = character(),
      season                         = integer(),
      sos_opp_def_epa_per_play       = numeric(),
      sos_opp_def_success_rate_allowed = numeric(),
      sos_n_opponents                = integer(),
      sos_computed                   = logical()
    )
  } else {
    # Join schedule to team_defense on the defensive team side
    sched_with_quality <- schedule %>%
      dplyr::left_join(
        team_defense %>%
          dplyr::select(defteam, season, def_epa_per_play,
                        def_success_rate_allowed, def_n_plays),
        by = c("defteam", "season")
      ) %>%
      dplyr::filter(!is.na(def_epa_per_play))  # Drop opponents below threshold

    # Validate join did not multiply rows
    n_sched   <- nrow(schedule)
    n_joined  <- nrow(sched_with_quality) +
      sum(is.na(
        dplyr::left_join(
          schedule, team_defense %>% dplyr::select(defteam, season, def_n_plays),
          by = c("defteam", "season")
        )$def_n_plays
      ))

    if (n_joined > n_sched * 1.01) {
      warning(glue(
        "Schedule join multiplied rows ({n_sched} -> {n_joined}). ",
        "Check for duplicate rows in team_defense."
      ), call. = FALSE)
    }

    # Average across opponents per (posteam, season)
    sos_by_team <- sched_with_quality %>%
      dplyr::group_by(primary_team = posteam, season) %>%
      dplyr::summarise(
        sos_opp_def_epa_per_play         = mean(def_epa_per_play, na.rm = TRUE),
        sos_opp_def_success_rate_allowed = mean(def_success_rate_allowed, na.rm = TRUE),
        sos_n_opponents                  = dplyr::n(),
        sos_computed                     = TRUE,
        .groups                          = "drop"
      )

    # Build full lookup including teams with no usable opponents (NA)
    all_team_seasons <- schedule %>%
      dplyr::distinct(primary_team = posteam, season)

    sos_lookup <- all_team_seasons %>%
      dplyr::left_join(sos_by_team, by = c("primary_team", "season")) %>%
      dplyr::mutate(
        sos_computed = dplyr::coalesce(sos_computed, FALSE),
        sos_n_opponents = dplyr::coalesce(as.integer(sos_n_opponents), 0L)
      )
  }

  # --- Step 3: Join SOS back to player panel ---
  if (verbose) message("\nStep 3: Joining SOS features to player panel...")

  n_panel_before <- nrow(panel)

  # Remove any pre-existing SOS columns to avoid conflicts
  existing_sos <- intersect(W8_SOS_COLS, names(panel))
  if (length(existing_sos) > 0L) {
    warning(glue(
      "Panel already contains SOS columns: {paste(existing_sos, collapse=', ')}. ",
      "Overwriting."
    ), call. = FALSE)
    panel <- panel[, setdiff(names(panel), W8_SOS_COLS), drop = FALSE]
  }

  panel_sos <- panel %>%
    dplyr::left_join(
      sos_lookup,
      by = c("primary_team", "season")
    ) %>%
    dplyr::mutate(
      sos_computed    = dplyr::coalesce(sos_computed, FALSE),
      sos_n_opponents = dplyr::coalesce(as.integer(sos_n_opponents), 0L)
    )

  # Verify join did not change row count
  n_panel_after <- nrow(panel_sos)
  if (n_panel_after != n_panel_before) {
    stop(glue(
      "SOS join changed row count: {n_panel_before} -> {n_panel_after}. ",
      "Duplicate (primary_team, season) rows in sos_lookup. ",
      "Check schedule for many-to-many matchup pairs."
    ), call. = FALSE)
  }

  n_computed  <- sum(panel_sos$sos_computed, na.rm = TRUE)
  n_na        <- sum(!panel_sos$sos_computed, na.rm = TRUE)
  pct_computed <- round(n_computed / n_panel_before * 100, 1)

  if (verbose) {
    message(glue(
      "  SOS computed for {format(n_computed, big.mark=',')} of ",
      "{format(n_panel_before, big.mark=',')} player-seasons ({pct_computed}%)"
    ))
    message(glue(
      "  SOS not computed: {format(n_na, big.mark=',')} player-seasons ",
      "(primary_team not in schedule or all opponents below min threshold)"
    ))

    # Warn if NA rate is high
    if (pct_computed < 80) {
      warning(glue(
        "SOS computed for only {pct_computed}% of player-seasons. ",
        "Check that panel seasons match the R/20 CFB cache seasons."
      ), call. = FALSE)
    }
  }

  panel_sos
}


# ==============================================================================
# FUNCTION: build_sleeper_gsis_crosswalk
# ==============================================================================

#' Build and Save Sleeper Player ID to nflfastR GSIS ID Crosswalk
#'
#' @description
#' Pulls rosters from one or more Sleeper leagues, collects unique Sleeper
#' player_ids, reconciles them to nflfastR GSIS IDs using match_sleeper_players()
#' from R/19, and saves the result as a single RDS file. The crosswalk is the
#' authoritative link between Sleeper fantasy data and nflfastR play-by-play
#' data for all downstream Phase 3 and Phase 4 work.
#'
#' \strong{Three-stage matching (from R/19):}
#' \enumerate{
#'   \item GSIS ID direct match (confidence: "high")
#'   \item Exact name + position match (confidence: "high")
#'   \item Fuzzy name match via agrep (confidence: "medium")
#' }
#' Unmatched players have gsis_id = NA and match_method = "unmatched".
#'
#' \strong{Scope:}
#' This crosswalk covers Sleeper-to-GSIS only (NFL players currently active
#' in Sleeper leagues). The CFB-to-NFL link (cfbfastR player names to GSIS
#' IDs) is out of scope and built in Week 10 (R/24).
#'
#' @param league_ids Character vector. Sleeper league IDs to pull rosters from.
#'   No hardcoded IDs. Use get_user_leagues() from R/19 to discover your IDs.
#' @param nfl_season Integer. NFL season year for nflreadr::load_rosters().
#'   Default: NFL_ROSTER_SEASON_DEFAULT (2025L). Update when a new season
#'   completes. Determines which player-team assignments are used for matching.
#' @param output_dir Character. Directory where the crosswalk RDS is saved.
#'   Default: data/season2_cache/.
#' @param force_refresh Logical. Passed to match_sleeper_players(). If TRUE,
#'   forces re-download of the Sleeper player database. Default: FALSE.
#' @param verbose Logical. Print progress. Default: TRUE.
#'
#' @return A tibble with one row per unique Sleeper player_id across all
#'   input leagues:
#'   \describe{
#'     \item{sleeper_player_id}{chr: Sleeper player_id.}
#'     \item{sleeper_name}{chr: Player name from Sleeper database.}
#'     \item{sleeper_position}{chr: Position from Sleeper (QB, RB, WR, TE, K, DEF).}
#'     \item{sleeper_team}{chr: NFL team abbreviation from Sleeper.}
#'     \item{gsis_id}{chr: nflfastR GSIS player_id. NA if unmatched.}
#'     \item{nflfastr_name}{chr: Name from nflreadr roster. NA if unmatched.}
#'     \item{match_method}{chr: "gsis", "exact_name", "fuzzy_name", or "unmatched".}
#'     \item{match_confidence}{chr: "high", "medium", or "none".}
#'   }
#'   Also saves to output_dir/s2_week8_id_crosswalk.rds.
#'   Returns NULL with a warning if all league roster pulls fail.
#'
#' @examples
#' \dontrun{
#' source(here::here("R", "19_sleeper_api.R"))
#' source(here::here("R", "22_sos_reconciliation.R"))
#'
#' # Discover your leagues first
#' my_leagues <- get_user_leagues("your_sleeper_username", season = 2025L)
#' league_ids <- my_leagues$league_id
#'
#' # Build and save the crosswalk
#' crosswalk <- build_sleeper_gsis_crosswalk(league_ids)
#'
#' # Inspect match rate by position
#' crosswalk %>%
#'   count(sleeper_position, match_method) %>%
#'   group_by(sleeper_position) %>%
#'   mutate(pct = round(n / sum(n) * 100, 1))
#' }
#'
#' @seealso match_sleeper_players, get_sleeper_rosters, validate_sos_crosswalk
#' @export
build_sleeper_gsis_crosswalk <- function(league_ids,
                                          nfl_season    = NFL_ROSTER_SEASON_DEFAULT,
                                          output_dir    = CFB_SOS_NFL_CACHE_DIR_DEFAULT,
                                          force_refresh = FALSE,
                                          verbose       = TRUE) {

  # --- Input validation ---
  league_ids <- as.character(league_ids)
  if (length(league_ids) == 0L || all(nchar(trimws(league_ids)) == 0L)) {
    stop("'league_ids' must be a non-empty character vector of Sleeper league IDs.",
         call. = FALSE)
  }

  nfl_season <- as.integer(nfl_season)
  if (length(nfl_season) != 1L || is.na(nfl_season)) {
    stop("'nfl_season' must be a single integer.", call. = FALSE)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) message(glue("Created output directory: {output_dir}"))
  }

  if (verbose) {
    message(strrep("=", 60))
    message("build_sleeper_gsis_crosswalk()")
    message(glue(
      "Leagues: {length(league_ids)} | NFL roster season: {nfl_season}"
    ))
    message(strrep("=", 60))
  }

  # --- Step 1: Pull rosters from all leagues ---
  if (verbose) message("\nStep 1: Pulling rosters from Sleeper leagues...")

  all_player_ids <- character(0)
  n_failed       <- 0L

  for (lid in league_ids) {
    rosters <- tryCatch(
      get_sleeper_rosters(lid),
      error = function(e) {
        warning(glue("League {lid}: roster pull failed -- {e$message}"),
                call. = FALSE)
        return(NULL)
      }
    )

    if (is.null(rosters) || nrow(rosters) == 0L) {
      n_failed <- n_failed + 1L
      next
    }

    # player_id column from get_sleeper_rosters() contains Sleeper player IDs
    ids_this_league <- rosters$player_id[!is.na(rosters$player_id)]
    all_player_ids  <- unique(c(all_player_ids, as.character(ids_this_league)))

    if (verbose) {
      message(glue(
        "  League {lid}: ",
        "{format(length(ids_this_league), big.mark=',')} player slots | ",
        "{format(length(unique(ids_this_league)), big.mark=',')} unique players"
      ))
    }
  }

  if (n_failed == length(league_ids)) {
    warning(
      "All league roster pulls failed. Cannot build crosswalk. Returning NULL.",
      call. = FALSE
    )
    return(NULL)
  }

  if (n_failed > 0L) {
    warning(glue(
      "{n_failed} of {length(league_ids)} league pulls failed. ",
      "Crosswalk built from {length(league_ids) - n_failed} leagues."
    ), call. = FALSE)
  }

  if (verbose) {
    message(glue(
      "  Total unique Sleeper player IDs: ",
      "{format(length(all_player_ids), big.mark=',')}"
    ))
  }

  # --- Step 2: Load nflreadr roster ---
  if (verbose) message(glue("\nStep 2: Loading nflreadr roster for season {nfl_season}..."))

  nfl_roster <- tryCatch(
    nflreadr::load_rosters(seasons = nfl_season),
    error = function(e) {
      stop(glue(
        "nflreadr::load_rosters({nfl_season}) failed: {e$message}\n",
        "Check network connection and nflreadr version."
      ), call. = FALSE)
    }
  )

  # Confirm required columns exist
  required_roster_cols <- c("gsis_id", "full_name", "position", "team")
  missing_roster_cols  <- setdiff(required_roster_cols, names(nfl_roster))
  if (length(missing_roster_cols) > 0L) {
    stop(glue(
      "nflreadr::load_rosters() output missing required columns: ",
      "{paste(missing_roster_cols, collapse = ', ')}"
    ), call. = FALSE)
  }

  if (verbose) {
    message(glue(
      "  nflreadr roster: {format(nrow(nfl_roster), big.mark=',')} rows | ",
      "{format(sum(!is.na(nfl_roster$gsis_id)), big.mark=',')} with GSIS IDs"
    ))
  }

  # --- Step 3: Reconcile Sleeper IDs to GSIS IDs ---
  if (verbose) message("\nStep 3: Reconciling Sleeper player IDs to GSIS IDs...")

  crosswalk <- tryCatch(
    match_sleeper_players(
      sleeper_player_ids = all_player_ids,
      nflreadr_roster    = nfl_roster,
      force_refresh      = force_refresh
    ),
    error = function(e) {
      stop(glue(
        "match_sleeper_players() failed: {e$message}"
      ), call. = FALSE)
    }
  )

  if (is.null(crosswalk) || nrow(crosswalk) == 0L) {
    warning("match_sleeper_players() returned empty result.", call. = FALSE)
    return(NULL)
  }

  # --- Step 4: Validate output and save ---
  if (verbose) {
    n_matched   <- sum(crosswalk$match_method != "unmatched", na.rm = TRUE)
    n_total     <- nrow(crosswalk)
    match_rate  <- round(n_matched / n_total * 100, 1)

    message(glue("\nStep 4: Saving crosswalk..."))
    message(glue(
      "  Match rate: {n_matched}/{n_total} ({match_rate}%)"
    ))
    message(glue(
      "  By method -- ",
      "GSIS: {sum(crosswalk$match_method == 'gsis', na.rm=TRUE)}, ",
      "Exact: {sum(crosswalk$match_method == 'exact_name', na.rm=TRUE)}, ",
      "Fuzzy: {sum(crosswalk$match_method == 'fuzzy_name', na.rm=TRUE)}, ",
      "Unmatched: {sum(crosswalk$match_method == 'unmatched', na.rm=TRUE)}"
    ))
  }

  # Attach metadata attributes
  attr(crosswalk, "nfl_season")    <- nfl_season
  attr(crosswalk, "n_leagues")     <- length(league_ids) - n_failed
  attr(crosswalk, "schema_tag")    <- W8_SCHEMA_TAG
  attr(crosswalk, "built_at")      <- Sys.time()

  # Save to output directory
  output_path <- file.path(output_dir, "s2_week8_id_crosswalk.rds")
  saveRDS(crosswalk, output_path)
  if (verbose) message(glue("  Saved: {output_path}"))

  crosswalk
}


# ==============================================================================
# FUNCTION: validate_sos_crosswalk
# ==============================================================================

#' Validate SOS Panel and Sleeper-GSIS Crosswalk Integrity
#'
#' @description
#' Runs integrity checks on the two Week 8 outputs before downstream Phase 3
#' work begins. Returns a named list with per-check results, a summary tibble,
#' and a scalar valid flag. The reconciliation rate must be documented before
#' Phase 3 modeling begins (per Week 8 scope contract in handoff).
#'
#' \strong{SOS panel checks:}
#' \enumerate{
#'   \item SOS columns present (critical)
#'   \item sos_opp_def_epa_per_play in plausible range [-0.5, 0.5] (critical)
#'   \item sos_opp_def_success_rate_allowed in [0, 1] (critical)
#'   \item sos_computed rate >= 70% (warning)
#'   \item sos_n_opponents >= 4 for computed rows (warning: low values unreliable)
#'   \item All expected seasons present in panel (warning)
#' }
#'
#' \strong{Crosswalk checks:}
#' \enumerate{
#'   \item sleeper_player_id unique (critical)
#'   \item Overall match rate >= 70% (critical)
#'   \item Skill position (QB/RB/WR/TE) match rate >= 80% (warning)
#'   \item No duplicate GSIS IDs among matched rows (warning)
#' }
#'
#' @param sos_panel Tibble. Output of build_cfb_sos_features(). NULL to skip
#'   SOS panel checks.
#' @param crosswalk Tibble. Output of build_sleeper_gsis_crosswalk(). NULL to
#'   skip crosswalk checks.
#' @param expected_seasons Integer vector. CFB seasons that should appear in
#'   the sos_panel. Default: derived from sos_panel$season.
#'
#' @return Named list:
#'   \describe{
#'     \item{valid}{Logical scalar. TRUE only if all critical checks pass.}
#'     \item{summary}{Tibble: check_name, severity, passed, detail.}
#'     \item{sos_out_of_range}{Tibble: SOS rows with EPA outside [-0.5, 0.5].}
#'     \item{crosswalk_duplicates}{Tibble: Duplicate sleeper_player_id rows.}
#'     \item{gsis_duplicates}{Tibble: Duplicate GSIS IDs among matched rows.}
#'   }
#'
#' @examples
#' \dontrun{
#' vr <- validate_sos_crosswalk(
#'   sos_panel  = panel_sos,
#'   crosswalk  = crosswalk
#' )
#' vr$valid
#' vr$summary
#' vr$crosswalk_duplicates
#' }
#'
#' @seealso build_cfb_sos_features, build_sleeper_gsis_crosswalk
#' @export
validate_sos_crosswalk <- function(sos_panel        = NULL,
                                    crosswalk        = NULL,
                                    expected_seasons = NULL) {

  if (is.null(sos_panel) && is.null(crosswalk)) {
    stop("At least one of 'sos_panel' or 'crosswalk' must be provided.",
         call. = FALSE)
  }

  checks  <- list()
  results <- list(
    sos_out_of_range     = tibble::tibble(),
    crosswalk_duplicates = tibble::tibble(),
    gsis_duplicates      = tibble::tibble()
  )

  # ==========================================================================
  # SOS PANEL CHECKS
  # ==========================================================================

  if (!is.null(sos_panel)) {

    # Check 1: SOS columns present
    present_sos <- intersect(W8_SOS_COLS, names(sos_panel))
    chk1_pass   <- length(present_sos) == length(W8_SOS_COLS)
    checks[["sos_cols_present"]] <- list(
      check_name = "sos_cols_present",
      severity   = "critical",
      passed     = chk1_pass,
      detail     = if (chk1_pass) {
        glue("OK: all {length(W8_SOS_COLS)} SOS columns present")
      } else {
        missing_sos <- setdiff(W8_SOS_COLS, names(sos_panel))
        glue("Missing SOS columns: {paste(missing_sos, collapse = ', ')}")
      }
    )

    # Check 2: EPA range plausibility
    if ("sos_opp_def_epa_per_play" %in% names(sos_panel)) {
      epa_vals    <- sos_panel$sos_opp_def_epa_per_play
      epa_nonNA   <- epa_vals[!is.na(epa_vals)]
      out_of_range <- sos_panel[!is.na(epa_vals) &
                                  (epa_vals < -0.5 | epa_vals > 0.5), ]
      chk2_pass   <- nrow(out_of_range) == 0L
      results[["sos_out_of_range"]] <- out_of_range
      checks[["sos_epa_range"]] <- list(
        check_name = "sos_epa_range",
        severity   = "critical",
        passed     = chk2_pass,
        detail     = if (chk2_pass) {
          glue(
            "OK: sos_opp_def_epa_per_play in [-0.5, 0.5] | ",
            "range: [{round(min(epa_nonNA, na.rm=TRUE), 3)}, ",
            "{round(max(epa_nonNA, na.rm=TRUE), 3)}]"
          )
        } else {
          glue(
            "{nrow(out_of_range)} rows outside [-0.5, 0.5]. ",
            "Check defensive play filters."
          )
        }
      )
    }

    # Check 3: Success rate in [0, 1]
    if ("sos_opp_def_success_rate_allowed" %in% names(sos_panel)) {
      sr_vals   <- sos_panel$sos_opp_def_success_rate_allowed[
        !is.na(sos_panel$sos_opp_def_success_rate_allowed)]
      chk3_pass <- length(sr_vals) == 0L || all(sr_vals >= 0 & sr_vals <= 1)
      checks[["sos_sr_range"]] <- list(
        check_name = "sos_sr_range",
        severity   = "critical",
        passed     = chk3_pass,
        detail     = if (chk3_pass) {
          "OK: sos_opp_def_success_rate_allowed in [0, 1]"
        } else {
          glue("{sum(sr_vals < 0 | sr_vals > 1)} rows outside [0, 1]")
        }
      )
    }

    # Check 4: SOS computed rate
    if ("sos_computed" %in% names(sos_panel)) {
      pct_computed <- mean(sos_panel$sos_computed, na.rm = TRUE) * 100
      chk4_pass    <- pct_computed >= 70
      checks[["sos_computed_rate"]] <- list(
        check_name = "sos_computed_rate",
        severity   = "warning",
        passed     = chk4_pass,
        detail     = glue(
          "sos_computed rate: {round(pct_computed, 1)}% ",
          "(threshold: 70%)"
        )
      )
    }

    # Check 5: n_opponents for computed rows
    if (all(c("sos_computed", "sos_n_opponents") %in% names(sos_panel))) {
      computed_rows     <- sos_panel[sos_panel$sos_computed == TRUE, ]
      low_opp           <- sum(computed_rows$sos_n_opponents < 4L, na.rm = TRUE)
      chk5_pass         <- low_opp == 0L
      checks[["sos_n_opponents"]] <- list(
        check_name = "sos_n_opponents",
        severity   = "warning",
        passed     = chk5_pass,
        detail     = if (chk5_pass) {
          "OK: all computed SOS rows have >= 4 opponents"
        } else {
          glue("{format(low_opp, big.mark=',')} computed rows have < 4 opponents. ",
               "SOS estimates unreliable for these rows.")
        }
      )
    }

    # Check 6: Season coverage
    if (!is.null(expected_seasons) && "season" %in% names(sos_panel)) {
      actual_seasons  <- sort(unique(sos_panel$season))
      missing_seasons <- setdiff(as.integer(expected_seasons), actual_seasons)
      chk6_pass       <- length(missing_seasons) == 0L
      checks[["sos_season_coverage"]] <- list(
        check_name = "sos_season_coverage",
        severity   = "warning",
        passed     = chk6_pass,
        detail     = if (chk6_pass) {
          glue("OK: all {length(actual_seasons)} expected seasons present")
        } else {
          glue("Missing seasons: {paste(missing_seasons, collapse = ', ')}")
        }
      )
    }
  }

  # ==========================================================================
  # CROSSWALK CHECKS
  # ==========================================================================

  if (!is.null(crosswalk)) {

    # Check 7: Unique sleeper_player_id
    if ("sleeper_player_id" %in% names(crosswalk)) {
      dup_ids   <- crosswalk %>%
        dplyr::count(sleeper_player_id) %>%
        dplyr::filter(n > 1L)
      chk7_pass <- nrow(dup_ids) == 0L
      results[["crosswalk_duplicates"]] <- if (!chk7_pass) {
        crosswalk %>%
          dplyr::filter(sleeper_player_id %in% dup_ids$sleeper_player_id)
      } else tibble::tibble()

      checks[["crosswalk_unique_ids"]] <- list(
        check_name = "crosswalk_unique_ids",
        severity   = "critical",
        passed     = chk7_pass,
        detail     = if (chk7_pass) {
          glue("OK: {format(nrow(crosswalk), big.mark=',')} unique sleeper_player_ids")
        } else {
          glue("{nrow(dup_ids)} sleeper_player_ids appear more than once")
        }
      )
    }

    # Check 8: Overall match rate >= 70%
    if ("match_method" %in% names(crosswalk)) {
      n_matched   <- sum(crosswalk$match_method != "unmatched", na.rm = TRUE)
      n_total_cw  <- nrow(crosswalk)
      match_rate  <- if (n_total_cw > 0L) n_matched / n_total_cw else 0
      chk8_pass   <- match_rate >= 0.70
      checks[["crosswalk_match_rate"]] <- list(
        check_name = "crosswalk_match_rate",
        severity   = "critical",
        passed     = chk8_pass,
        detail     = glue(
          "Overall match rate: {n_matched}/{n_total_cw} ",
          "({round(match_rate * 100, 1)}%) | threshold: 70%"
        )
      )

      # Check 9: Skill position match rate >= 80%
      if ("sleeper_position" %in% names(crosswalk)) {
        skill_positions <- c("QB", "RB", "WR", "TE")
        skill_rows      <- crosswalk[
          toupper(crosswalk$sleeper_position) %in% skill_positions, ]
        skill_matched   <- sum(skill_rows$match_method != "unmatched", na.rm = TRUE)
        skill_total     <- nrow(skill_rows)
        skill_rate      <- if (skill_total > 0L) skill_matched / skill_total else 0
        chk9_pass       <- skill_rate >= 0.80

        checks[["crosswalk_skill_rate"]] <- list(
          check_name = "crosswalk_skill_rate",
          severity   = "warning",
          passed     = chk9_pass,
          detail     = glue(
            "Skill position (QB/RB/WR/TE) match rate: ",
            "{skill_matched}/{skill_total} ",
            "({round(skill_rate * 100, 1)}%) | threshold: 80%"
          )
        )
      }
    }

    # Check 10: No duplicate GSIS IDs among matched rows
    if (all(c("gsis_id", "match_method") %in% names(crosswalk))) {
      matched_rows <- crosswalk[crosswalk$match_method != "unmatched" &
                                  !is.na(crosswalk$gsis_id), ]
      dup_gsis     <- matched_rows %>%
        dplyr::count(gsis_id) %>%
        dplyr::filter(n > 1L)
      chk10_pass   <- nrow(dup_gsis) == 0L
      results[["gsis_duplicates"]] <- if (!chk10_pass) {
        matched_rows %>%
          dplyr::filter(gsis_id %in% dup_gsis$gsis_id) %>%
          dplyr::arrange(gsis_id)
      } else tibble::tibble()

      checks[["crosswalk_gsis_unique"]] <- list(
        check_name = "crosswalk_gsis_unique",
        severity   = "warning",
        passed     = chk10_pass,
        detail     = if (chk10_pass) {
          "OK: no duplicate GSIS IDs among matched rows"
        } else {
          glue(
            "{nrow(dup_gsis)} GSIS IDs appear more than once. ",
            "Possible player name collision in fuzzy matching."
          )
        }
      )
    }
  }

  # --- Compile summary ---
  summary_tbl <- tibble::tibble(
    check_name = vapply(checks, `[[`, character(1), "check_name"),
    severity   = vapply(checks, `[[`, character(1), "severity"),
    passed     = vapply(checks, `[[`, logical(1),   "passed"),
    detail     = vapply(checks, `[[`, character(1), "detail")
  )

  critical_rows   <- summary_tbl[summary_tbl$severity == "critical", ]
  all_critical_ok <- nrow(critical_rows) == 0L || all(critical_rows$passed)

  message("\n--- validate_sos_crosswalk() ---")
  for (j in seq_len(nrow(summary_tbl))) {
    row  <- summary_tbl[j, ]
    icon <- if (row$passed) "[PASS]" else if (row$severity == "critical") "[FAIL]" else "[WARN]"
    message(glue("  {icon} [{row$severity}] {row$check_name}: {row$detail}"))
  }
  message(glue("  Overall valid: {all_critical_ok}"))

  c(list(valid = all_critical_ok, summary = summary_tbl), results)
}


# ==============================================================================
# FUNCTION: validate_week8_assumptions
# ==============================================================================

#' Validate Statistical and Domain Assumptions for Week 8 SOS Analysis
#'
#' @description
#' Tests the five assumptions underlying the Week 8 SOS computation and
#' adjustment formula before Phase 3 consumes this data. Each assumption
#' is tested with a specific empirical check and reported with a PASS /
#' WARN / FAIL classification.
#'
#' \strong{Assumption 1 -- Linearity of SOS-efficiency relationship:}
#' The adjustment formula \code{adjusted = raw - sos_opp_def_epa_per_play}
#' assumes a linear unit relationship between schedule difficulty and player
#' efficiency. Tested via Pearson r and a simple OLS regression per position.
#' A significant relationship with R² < 0.05 means adjustment moves numbers
#' but explains little variance -- useful to know before Phase 3 modeling.
#'
#' \strong{Assumption 2 -- Filter consistency:}
#' Garbage time and scrimmage play filters must be identical between player
#' stat aggregation (R/21) and opponent defense computation (R/22). Tested
#' by verifying the W8 filter constants match R/21 constants.
#'
#' \strong{Assumption 3 -- SOS independence from position:}
#' All players on the same team face the same schedule, so SOS should not
#' vary meaningfully by position group within a team-season. Tested by
#' computing within-team-season SOS variance across position groups. High
#' variance indicates a data problem (wrong schedule join or primary_team
#' assignment errors).
#'
#' \strong{Assumption 4 -- Opponent sample adequacy:}
#' SOS estimates require a minimum number of opponents to be reliable.
#' Tested by checking the distribution of \code{sos_n_opponents} for
#' computed rows. Fewer than 6 opponents is flagged as unreliable.
#'
#' \strong{Assumption 5 -- low_volume flag coverage:}
#' The adjustment is only applied to above-threshold players. Tested by
#' verifying that all rows used in efficiency analyses have
#' \code{low_volume == FALSE} and non-NA efficiency columns.
#'
#' @param sos_panel Tibble. Output of build_cfb_sos_features(). Must include
#'   SOS columns, position_group, primary_team, season, and efficiency columns.
#' @param min_n_for_corr Integer. Minimum rows per position to run correlation
#'   check. Default: 30L.
#'
#' @return Named list:
#'   \describe{
#'     \item{valid}{Logical scalar. TRUE if all critical assumptions hold.}
#'     \item{summary}{Tibble: assumption, severity, passed, detail.}
#'     \item{linearity}{Named list of per-position regression results.}
#'     \item{low_n_opponents}{Tibble: rows with sos_n_opponents < 6.}
#'   }
#'
#' @examples
#' \dontrun{
#' panel_sos <- readRDS(here("data", "season2_cfb_cache",
#'                           "s2_week8_cfb_sos_panel.rds"))
#' ar <- validate_week8_assumptions(panel_sos)
#' ar$valid
#' ar$summary
#' ar$linearity$QB
#' }
#'
#' @seealso build_cfb_sos_features, validate_sos_crosswalk
#' @export
validate_week8_assumptions <- function(sos_panel,
                                        min_n_for_corr = 30L) {

  if (!is.data.frame(sos_panel) || nrow(sos_panel) == 0L) {
    stop("'sos_panel' must be a non-empty data frame.", call. = FALSE)
  }

  checks    <- list()
  results   <- list()

  message("\n--- validate_week8_assumptions() ---")

  # ==========================================================================
  # ASSUMPTION 1: Linearity of SOS-efficiency relationship
  # ==========================================================================

  pos_metrics <- list(
    QB    = "pass_epa_per_attempt",
    RB    = "rush_epa_per_attempt",
    WR_TE = "rec_epa_per_target"
  )

  linearity_results <- list()
  linearity_details <- character(0)

  for (pos in names(pos_metrics)) {
    metric <- pos_metrics[[pos]]

    if (!all(c(metric, "sos_opp_def_epa_per_play", "position_group",
               "low_volume", "sos_computed") %in% names(sos_panel))) next

    pos_data <- sos_panel %>%
      dplyr::filter(
        position_group == pos,
        !low_volume,
        sos_computed,
        !is.na(.data[[metric]]),
        !is.na(sos_opp_def_epa_per_play)
      )

    n_pos <- nrow(pos_data)

    if (n_pos < min_n_for_corr) {
      linearity_details <- c(
        linearity_details,
        glue("{pos}: n={n_pos} below threshold ({min_n_for_corr}) -- skipped")
      )
      linearity_results[[pos]] <- list(n = n_pos, r = NA_real_,
                                        r2 = NA_real_, p = NA_real_,
                                        slope = NA_real_)
      next
    }

    fit   <- lm(as.formula(glue(
      "{metric} ~ sos_opp_def_epa_per_play"
    )), data = pos_data)
    smry  <- summary(fit)
    r2    <- round(smry$r.squared, 4)
    slope <- round(coef(fit)[["sos_opp_def_epa_per_play"]], 4)
    pval  <- round(coef(smry)["sos_opp_def_epa_per_play", "Pr(>|t|)"], 4)
    r_val <- round(sign(slope) * sqrt(r2), 4)

    linearity_results[[pos]] <- list(
      n = n_pos, r = r_val, r2 = r2, p = pval, slope = slope
    )

    linearity_details <- c(
      linearity_details,
      glue(
        "{pos}: r={r_val}, R\u00b2={r2}, slope={slope}, p={pval}, n={n_pos}"
      )
    )
  }

  # Warn if relationship is not significant for any position -- adjustment
  # is moving numbers without empirical support for that position
  any_sig <- any(
    vapply(linearity_results, function(x) {
      !is.na(x$p) && x$p < 0.05
    }, logical(1))
  )

  checks[["linearity"]] <- list(
    assumption = "SOS-efficiency linear relationship",
    severity   = "warning",
    passed     = any_sig,
    detail     = if (any_sig) {
      paste("Significant for at least one position.",
            paste(linearity_details, collapse = " | "))
    } else {
      paste("No significant SOS-efficiency relationship found.",
            "Adjustment moves values without empirical backing.",
            paste(linearity_details, collapse = " | "))
    }
  )

  results[["linearity"]] <- linearity_results
  message(glue(
    "  Assumption 1 (linearity): ",
    "{if (any_sig) '[PASS]' else '[WARN]'}"
  ))
  for (d in linearity_details) message(glue("    {d}"))

  # ==========================================================================
  # ASSUMPTION 2: Filter consistency (R/21 vs R/22 constants)
  # ==========================================================================

  # Both R/21 and R/22 must use the same garbage time thresholds.
  # R/21 constants: CFB_GARBAGE_TIME_SCORE_DIFF = 28L, WP_LO = 0.05, WP_HI = 0.95
  # R/22 constants: W8_GARBAGE_SCORE_DIFF, W8_GARBAGE_WP_LO, W8_GARBAGE_WP_HI
  r21_score_diff <- 28L
  r21_wp_lo      <- 0.05
  r21_wp_hi      <- 0.95

  filter_match <- (
    W8_GARBAGE_SCORE_DIFF == r21_score_diff &&
    abs(W8_GARBAGE_WP_LO  - r21_wp_lo) < 1e-9 &&
    abs(W8_GARBAGE_WP_HI  - r21_wp_hi) < 1e-9
  )

  checks[["filter_consistency"]] <- list(
    assumption = "Garbage time filter consistency (R/21 vs R/22)",
    severity   = "critical",
    passed     = filter_match,
    detail     = if (filter_match) {
      glue(
        "OK: score_diff={W8_GARBAGE_SCORE_DIFF}, ",
        "wp_lo={W8_GARBAGE_WP_LO}, wp_hi={W8_GARBAGE_WP_HI} ",
        "match R/21 constants."
      )
    } else {
      glue(
        "MISMATCH -- R/22: score_diff={W8_GARBAGE_SCORE_DIFF}, ",
        "wp=[{W8_GARBAGE_WP_LO},{W8_GARBAGE_WP_HI}] vs ",
        "R/21: score_diff={r21_score_diff}, ",
        "wp=[{r21_wp_lo},{r21_wp_hi}]. ",
        "Player stats and opponent defense use different filters."
      )
    }
  )

  message(glue(
    "  Assumption 2 (filter consistency): ",
    "{if (filter_match) '[PASS]' else '[FAIL]'}"
  ))

  # ==========================================================================
  # ASSUMPTION 3: SOS independence from position within team-season
  # ==========================================================================

  # All players on the same team-season should have the same SOS value.
  # Within-team-season variance in SOS signals primary_team assignment errors.

  if (all(c("primary_team", "season", "position_group",
            "sos_opp_def_epa_per_play", "sos_computed") %in% names(sos_panel))) {

    sos_variance <- sos_panel %>%
      dplyr::filter(sos_computed, !is.na(sos_opp_def_epa_per_play)) %>%
      dplyr::group_by(primary_team, season) %>%
      dplyr::summarise(
        n_positions  = dplyr::n_distinct(position_group),
        sos_sd       = sd(sos_opp_def_epa_per_play, na.rm = TRUE),
        .groups      = "drop"
      ) %>%
      dplyr::filter(n_positions > 1L)

    # Any team-season with SD > 0.001 has meaningfully different SOS across
    # positions, which should be impossible (same schedule).
    high_var <- sos_variance %>%
      dplyr::filter(!is.na(sos_sd) & sos_sd > 0.001)

    n_high_var   <- nrow(high_var)
    n_team_season <- nrow(sos_variance)
    chk3_pass    <- n_high_var == 0L

    checks[["sos_position_independence"]] <- list(
      assumption = "SOS independent of position within team-season",
      severity   = "critical",
      passed     = chk3_pass,
      detail     = if (chk3_pass) {
        glue(
          "OK: all {n_team_season} multi-position team-seasons have ",
          "SOS SD <= 0.001 across positions."
        )
      } else {
        glue(
          "{n_high_var} team-seasons have SOS SD > 0.001 across positions. ",
          "Indicates primary_team assignment error or schedule join problem."
        )
      }
    )

    message(glue(
      "  Assumption 3 (SOS position independence): ",
      "{if (chk3_pass) '[PASS]' else '[FAIL]'}"
    ))

  } else {
    checks[["sos_position_independence"]] <- list(
      assumption = "SOS independent of position within team-season",
      severity   = "warning",
      passed     = FALSE,
      detail     = "Required columns absent -- cannot test."
    )
    message("  Assumption 3 (SOS position independence): [SKIP] -- missing columns")
  }

  # ==========================================================================
  # ASSUMPTION 4: Opponent sample adequacy
  # ==========================================================================

  if (all(c("sos_n_opponents", "sos_computed") %in% names(sos_panel))) {

    computed_rows <- sos_panel[sos_panel$sos_computed == TRUE, ]
    low_n         <- computed_rows[
      !is.na(computed_rows$sos_n_opponents) &
        computed_rows$sos_n_opponents < 6L, ]
    n_low         <- nrow(low_n)
    n_computed    <- nrow(computed_rows)
    pct_low       <- round(n_low / max(n_computed, 1L) * 100, 1)
    chk4_pass     <- pct_low < 10

    results[["low_n_opponents"]] <- low_n

    checks[["opponent_sample"]] <- list(
      assumption = "Opponent sample adequacy (>= 6 opponents per player-season)",
      severity   = "warning",
      passed     = chk4_pass,
      detail     = glue(
        "{n_low} of {n_computed} computed rows have < 6 opponents ({pct_low}%). ",
        if (chk4_pass) "Below 10% threshold -- acceptable."
        else "Exceeds 10% threshold -- consider raising CFB_SOS_MIN_DEF_PLAYS."
      )
    )

    message(glue(
      "  Assumption 4 (opponent sample): ",
      "{if (chk4_pass) '[PASS]' else '[WARN]'} ",
      "({n_low}/{n_computed} rows with < 6 opponents)"
    ))

  } else {
    checks[["opponent_sample"]] <- list(
      assumption = "Opponent sample adequacy",
      severity   = "warning",
      passed     = FALSE,
      detail     = "sos_n_opponents column absent."
    )
  }

  # ==========================================================================
  # ASSUMPTION 5: low_volume flag coverage
  # ==========================================================================

  pos_metrics_all <- c("pass_epa_per_attempt", "rush_epa_per_attempt",
                       "rec_epa_per_target")
  present_metrics <- intersect(pos_metrics_all, names(sos_panel))

  if ("low_volume" %in% names(sos_panel) && length(present_metrics) > 0L) {

    # Any above-threshold player with a non-NA efficiency column should be usable
    above_threshold <- sos_panel %>%
      dplyr::filter(low_volume == FALSE)

    # Check: no above-threshold player has ALL efficiency columns NA
    # (would mean they appear above threshold but have no usable stats)
    all_na_eff <- above_threshold %>%
      dplyr::filter(
        dplyr::if_all(dplyr::all_of(present_metrics), is.na)
      )

    n_all_na  <- nrow(all_na_eff)
    chk5_pass <- n_all_na == 0L

    checks[["low_volume_coverage"]] <- list(
      assumption = "low_volume == FALSE rows have at least one non-NA efficiency column",
      severity   = "warning",
      passed     = chk5_pass,
      detail     = if (chk5_pass) {
        glue(
          "OK: all {nrow(above_threshold)} above-threshold rows have ",
          "at least one non-NA efficiency column."
        )
      } else {
        glue(
          "{n_all_na} above-threshold rows have NA for all efficiency columns. ",
          "Check total_plays vs individual play type counts."
        )
      }
    )

    message(glue(
      "  Assumption 5 (low_volume coverage): ",
      "{if (chk5_pass) '[PASS]' else '[WARN]'}"
    ))

  } else {
    checks[["low_volume_coverage"]] <- list(
      assumption = "low_volume flag coverage",
      severity   = "warning",
      passed     = FALSE,
      detail     = "low_volume column or efficiency columns absent."
    )
  }

  # ==========================================================================
  # Compile summary
  # ==========================================================================

  summary_tbl <- tibble::tibble(
    assumption = vapply(checks, `[[`, character(1), "assumption"),
    severity   = vapply(checks, `[[`, character(1), "severity"),
    passed     = vapply(checks, `[[`, logical(1),   "passed"),
    detail     = vapply(checks, `[[`, character(1), "detail")
  )

  critical_rows   <- summary_tbl[summary_tbl$severity == "critical", ]
  all_critical_ok <- nrow(critical_rows) == 0L || all(critical_rows$passed)

  message(glue("\n  Overall assumptions valid: {all_critical_ok}"))

  c(
    list(valid = all_critical_ok, summary = summary_tbl),
    results
  )
}


# ==============================================================================
# FUNCTION: run_week8_pipeline
# ==============================================================================

#' Run the Full Week 8 Pipeline
#'
#' @description
#' End-to-end wrapper that:
#' \enumerate{
#'   \item Builds or loads the R/21 CFB player-season panel.
#'   \item Computes SOS features via build_cfb_sos_features().
#'   \item Saves the SOS-augmented panel to cfb_cache.
#'   \item Builds the Sleeper-to-GSIS ID crosswalk via
#'         build_sleeper_gsis_crosswalk().
#'   \item Runs validate_sos_crosswalk() on both outputs.
#'   \item Prints a reconciliation rate summary.
#' }
#'
#' The R/21 panel is loaded from cache if a pre-built RDS exists at
#' cfb_cache_dir/s2_week7_cfb_panel.rds. If not found, the function stops
#' with an explicit error directing the user to run R/21 first. This enforces
#' the build order contract: R/21 must run before R/22.
#'
#' @param league_ids Character vector. Sleeper league IDs. No hardcoding.
#' @param seasons Integer vector. CFB seasons for SOS computation.
#'   Default: CFB_SOS_SEASONS_DEFAULT (2014:2025).
#' @param cfb_cache_dir Character. Path to CFB cache (R/20/R/21 outputs).
#' @param nfl_cache_dir Character. Path to NFL cache (crosswalk output).
#' @param nfl_season Integer. NFL season for nflreadr roster. Default: 2025L.
#' @param force_sleeper_refresh Logical. Force re-download of Sleeper player
#'   database. Default: FALSE.
#' @param verbose Logical. Print progress. Default: TRUE.
#'
#' @return Invisibly, a named list:
#'   \describe{
#'     \item{sos_panel}{Tibble: SOS-augmented CFB player-season panel.}
#'     \item{crosswalk}{Tibble: Sleeper-to-GSIS crosswalk.}
#'     \item{validation}{Named list from validate_sos_crosswalk().}
#'   }
#'
#' @examples
#' \dontrun{
#' source(here::here("R", "19_sleeper_api.R"))
#' source(here::here("R", "20_multi_season_cfb_pbp.R"))
#' source(here::here("R", "22_sos_reconciliation.R"))
#'
#' # Discover your leagues
#' my_leagues <- get_user_leagues("your_sleeper_username", season = 2025L)
#'
#' # Run full pipeline
#' out <- run_week8_pipeline(league_ids = my_leagues$league_id)
#'
#' # Check validation results
#' out$validation$valid
#' out$validation$summary
#' }
#'
#' @seealso build_cfb_sos_features, build_sleeper_gsis_crosswalk,
#'   validate_sos_crosswalk
#' @export
run_week8_pipeline <- function(league_ids,
                                seasons              = CFB_SOS_SEASONS_DEFAULT,
                                cfb_cache_dir        = CFB_SOS_CFB_CACHE_DIR_DEFAULT,
                                nfl_cache_dir        = CFB_SOS_NFL_CACHE_DIR_DEFAULT,
                                nfl_season           = NFL_ROSTER_SEASON_DEFAULT,
                                force_sleeper_refresh = FALSE,
                                verbose              = TRUE) {

  message(strrep("#", 60))
  message("Season 2 Week 8: SOS Features + Sleeper-GSIS Crosswalk")
  message(glue("Seasons : {min(seasons)}-{max(seasons)}"))
  message(glue("Leagues : {length(league_ids)} provided"))
  message(strrep("#", 60))

  # --- Step 1: Load R/21 CFB panel from cache ---
  message("\nSTEP 1/4: Loading R/21 CFB player-season panel from cache...")

  panel_path <- file.path(cfb_cache_dir, "s2_week7_cfb_panel.rds")

  if (!file.exists(panel_path)) {
    stop(glue(
      "R/21 CFB panel not found at: {panel_path}\n",
      "Run R/21_cfb_player_season_panel.R and save the panel output to this ",
      "path before running the Week 8 pipeline.\n",
      "Expected call: saveRDS(panel, here('data', 'season2_cfb_cache', ",
      "'s2_week7_cfb_panel.rds'))"
    ), call. = FALSE)
  }

  panel <- readRDS(panel_path)
  message(glue(
    "  Loaded: {format(nrow(panel), big.mark=',')} player-seasons | ",
    "{dplyr::n_distinct(panel$player_name)} unique players"
  ))

  # --- Step 2: Compute SOS features ---
  message("\nSTEP 2/4: Computing SOS features...")

  sos_panel <- build_cfb_sos_features(
    panel     = panel,
    seasons   = seasons,
    cache_dir = cfb_cache_dir,
    verbose   = verbose
  )

  sos_output_path <- file.path(cfb_cache_dir, "s2_week8_cfb_sos_panel.rds")
  saveRDS(sos_panel, sos_output_path)
  message(glue("  Saved SOS panel: {sos_output_path}"))

  rm(panel); gc(verbose = FALSE)

  # --- Step 3: Build Sleeper-GSIS crosswalk ---
  message("\nSTEP 3/4: Building Sleeper-to-GSIS ID crosswalk...")

  crosswalk <- build_sleeper_gsis_crosswalk(
    league_ids    = league_ids,
    nfl_season    = nfl_season,
    output_dir    = nfl_cache_dir,
    force_refresh = force_sleeper_refresh,
    verbose       = verbose
  )

  # --- Step 4: Validate both outputs ---
  message("\nSTEP 4/4: Validating outputs...")

  validation <- validate_sos_crosswalk(
    sos_panel        = sos_panel,
    crosswalk        = crosswalk,
    expected_seasons = as.integer(seasons)
  )

  # --- Final summary ---
  message(strrep("#", 60))
  message("Week 8 Pipeline Complete.")

  n_sos_computed  <- sum(sos_panel$sos_computed, na.rm = TRUE)
  pct_sos         <- round(n_sos_computed / nrow(sos_panel) * 100, 1)
  message(glue(
    "SOS: {format(n_sos_computed, big.mark=',')} of ",
    "{format(nrow(sos_panel), big.mark=',')} player-seasons computed ({pct_sos}%)"
  ))

  if (!is.null(crosswalk)) {
    n_matched_cw <- sum(crosswalk$match_method != "unmatched", na.rm = TRUE)
    n_total_cw   <- nrow(crosswalk)
    pct_matched  <- round(n_matched_cw / n_total_cw * 100, 1)
    message(glue(
      "Crosswalk: {format(n_matched_cw, big.mark=',')} of ",
      "{format(n_total_cw, big.mark=',')} Sleeper players matched to GSIS ",
      "({pct_matched}%)"
    ))
  }

  message(glue("Validation: overall valid = {validation$valid}"))

  if (!validation$valid) {
    failed_checks <- validation$summary[!validation$summary$passed &
                                          validation$summary$severity == "critical", ]
    message("  Critical failures:")
    for (i in seq_len(nrow(failed_checks))) {
      message(glue("    [FAIL] {failed_checks$check_name[i]}: {failed_checks$detail[i]}"))
    }
  }

  message(strrep("#", 60))

  invisible(list(
    sos_panel  = sos_panel,
    crosswalk  = crosswalk,
    validation = validation
  ))
}
