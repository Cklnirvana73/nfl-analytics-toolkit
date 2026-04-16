# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 5
# Sleeper Fantasy Football API Integration
# File: R/19_sleeper_api.R
# ==============================================================================
#
# PURPOSE
# -------
# Connect to the Sleeper Fantasy Football API to pull league settings, rosters,
# matchups, and weekly outcomes. Maps Sleeper scoring settings directly to the
# calculate_fantasy_points_ext() parameter set from R/17_extended_scoring.R.
# Multi-league design: no hardcoded league IDs anywhere in this file.
#
# NAVIGATION
# ----------
#   Lines   55-140  : Constants, scoring field map
#   Lines  142-240  : .sleeper_get()              -- internal HTTP helper
#   Lines  242-360  : connect_sleeper_league()    -- league metadata
#   Lines  362-470  : get_sleeper_rosters()       -- roster data
#   Lines  472-580  : get_sleeper_matchups()      -- weekly matchup data
#   Lines  582-180  : .parse_scoring_settings()   -- internal scoring parser
#   Lines  780-930  : map_sleeper_scoring()       -- scoring -> function params
#   Lines  932-1100 : match_sleeper_players()     -- player ID reconciliation
#   Lines 1102-1180 : get_user_leagues()          -- convenience: user's leagues
#
# SLEEPER API OVERVIEW
# --------------------
#   Base URL  : https://api.sleeper.app/v1
#   Auth      : None required (public, read-only for league data)
#   Rate limit: Approximately 1,000 requests/hour -- unenforced but respected
#   No API key or OAuth required.
#
#   Key endpoints used:
#     GET /league/{league_id}                  -- league metadata + scoring
#     GET /league/{league_id}/rosters          -- all rosters
#     GET /league/{league_id}/matchups/{week}  -- weekly matchup scores
#     GET /players/nfl                         -- all NFL player profiles (large)
#     GET /user/{username}                     -- user profile lookup
#     GET /user/{user_id}/leagues/nfl/{season} -- user's leagues in a season
#
# CROSS-FILE INTERFACES
# ---------------------
#   READS FROM:
#     R/17_extended_scoring.R  : calculate_fantasy_points_ext() parameter names
#       map_sleeper_scoring()$params is a named list ready for do.call() into
#       calculate_fantasy_points_ext(). See examples section of map_sleeper_scoring().
#
#   OUTPUTS:
#     match_sleeper_players() returns a reconciliation tibble with columns:
#       sleeper_player_id, sleeper_name, sleeper_position, sleeper_team,
#       gsis_id, nflfastr_name, match_method, match_confidence
#     This tibble enables joining Sleeper weekly scores to nflfastR play-by-play.
#
# DEPENDENCIES
# ------------
#   httr     : HTTP GET, response status handling
#   dplyr    : Tibble manipulation and joins
#   purrr    : List iteration (map, map_dfr)
#   glue     : String interpolation in messages
#   stringr  : Name normalization for fuzzy matching
#   here     : Portable path resolution
#   jsonlite : JSON parsing (used via httr::content())
#
# ==============================================================================

library(httr)
library(dplyr)
library(purrr)
library(glue)
library(stringr)
library(here)

# ==============================================================================
# SECTION 1: CONSTANTS
# ==============================================================================

# Sleeper API base URL -- no trailing slash
SLEEPER_BASE_URL <- "https://api.sleeper.app/v1"

# Most recently completed NFL regular season.
# Update to 2026 when the 2026 season completes.
CURRENT_NFL_SEASON <- 2025L

# Cache directory -- shared with R/15 and R/16 pipeline artifacts
SLEEPER_CACHE_DIR <- here::here("data", "season2_cache")

# Sleeper player list cache file.
# The /players/nfl endpoint returns ~5MB of JSON. Cache locally and refresh
# only when force_refresh = TRUE. File is season-independent (player database
# is cumulative, not season-specific).
SLEEPER_PLAYERS_CACHE <- file.path(SLEEPER_CACHE_DIR, "sleeper_players_nfl.rds")

# Player name fuzzy match distance passed to agrep(max.distance).
# 0.1 = allow ~10% character-level distance (e.g., "D.K. Metcalf" ~ "DK Metcalf")
FUZZY_MATCH_DISTANCE <- 0.1

# Match rate thresholds
# Below WARNING: log a warning but proceed
# Below ERROR: stop with an informative error
MATCH_RATE_WARNING_THRESHOLD <- 0.90
MATCH_RATE_ERROR_THRESHOLD   <- 0.70

# ==============================================================================
# SECTION 2: SLEEPER SCORING FIELD MAP
# ==============================================================================
#
# Maps Sleeper scoring_settings JSON keys to calculate_fantasy_points_ext()
# parameter names from R/17_extended_scoring.R.
#
# Three categories:
#   DIRECT_MAP  -- 1:1 numeric mapping, no transformation
#   DERIVED     -- multi-field or type-conversion logic (handled in parser)
#   UNSUPPORTED -- Sleeper features with no equivalent in our function
#
# The parser in .parse_scoring_settings() walks DIRECT_MAP first, then
# handles DERIVED fields explicitly, then records UNSUPPORTED fields in
# the mapping log so the user can see what was left on the table.

SLEEPER_DIRECT_MAP <- list(
  # Sleeper field = calculate_fantasy_points_ext() parameter
  pass_yd        = "pass_yd",
  pass_td        = "pass_td",
  pass_int       = "pass_int",
  rush_yd        = "rush_yd",
  rush_td        = "rush_td",
  rec_yd         = "rec_yd",
  rec_td         = "rec_td",
  fum_lost       = "fumbles",
  rec            = "ppr",
  two_pt_conv    = "two_point_conversion",
  rush_att       = "rush_att_bonus",
  # Passing yardage milestone bonuses
  bonus_pass_yd_300 = "bonus_pass_yd_300",
  bonus_pass_yd_400 = "bonus_pass_yd_400",
  # Yardage milestone bonuses (rec/rush 200+)
  bonus_rec_yd_200  = "bonus_rec_yd_200",
  bonus_rush_yd_200 = "bonus_rush_yd_200",
  # Passer 2-point conversion credit
  pass_2pt          = "pass_2pt"
)

# Fields requiring special handling -- documented here for readability.
# Logic lives in .parse_scoring_settings().
#   pass_sack          -> sack_penalty (Sleeper stores as negative; our func too)
#   bonus_rec_te       -> te_premium = TRUE if > 0; warn if value != 0.5
#   bonus_rec_yd_100   -> hundred_yard_bonus (receiving component)
#   bonus_rush_yd_100  -> hundred_yard_bonus (rushing component; merged with rec)
#   bonus_fd_rb / _wr / _te / _qb -> first_down_points (merged across positions)
#   bonus_rec_td_50p   -> long_td_bonus, long_td_threshold = 49L (>49 = 50+)
#   bonus_rush_td_50p  -> long_td_bonus (merged with receiving; same threshold)
#   roster SUPER_FLEX  -> superflex_pass_td flag (detected at league level)

# Sleeper fields known to exist but having no mapping in our function.
# Logged in mapping_log$status = "unsupported" so users know what was skipped.
SLEEPER_KNOWN_UNSUPPORTED <- c(
  # Defensive / special teams (not modeled in our player-level function)
  "def_td", "def_st_td", "kret_td", "def_st_ff", "def_st_fum_rec",
  "def_sack", "def_int", "pts_allow_0", "pts_allow_1_6",
  # Kicker scoring
  "xpmiss", "fgm", "fgm_0_19", "fgm_20_29", "fgm_30_39", "fgm_40_49",
  # Pick-6 (Sleeper has pass_int_td for this; mapped to a single INT penalty)
  "pass_int_td",
  # Incomplete pass penalty
  "pass_inc",
  # Bonuses that vary by position in a way our function cannot distinguish
  "bonus_rec_rb", "bonus_rec_wr",
  # Scorer-specific 2PT fields (two_pt_conv already covers scorer;
  # rush_2pt/rec_2pt only needed if league differentiates scorer role)
  "rush_2pt", "rec_2pt"
)


# ==============================================================================
# SECTION 3: INTERNAL HTTP HELPER
# ==============================================================================

#' Internal: Execute a Sleeper API GET Request
#'
#' @description
#' Single-responsibility HTTP helper. Handles status checking, error messaging,
#' and JSON parsing. All public functions route their requests through here.
#' Not exported -- do not call directly.
#'
#' @param endpoint Character. Path after the base URL, starting with "/".
#'   Example: "/league/123456789"
#' @param timeout_sec Integer. Request timeout in seconds. Default 30.
#'
#' @return Parsed R list from the Sleeper JSON response.
#'   Returns NULL (with warning) on HTTP errors or timeouts.
#'
#' @keywords internal
.sleeper_get <- function(endpoint, timeout_sec = 30L) {

  url <- paste0(SLEEPER_BASE_URL, endpoint)

  response <- tryCatch(
    httr::GET(url, httr::timeout(timeout_sec)),
    error = function(e) {
      warning(glue("Sleeper API request failed: {e$message}\nURL: {url}"),
              call. = FALSE)
      return(NULL)
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  status <- httr::status_code(response)

  if (status == 404L) {
    warning(glue(
      "Sleeper API returned 404 (Not Found) for:\n  {url}\n",
      "Check that the league_id or endpoint path is correct."
    ), call. = FALSE)
    return(NULL)
  }

  if (status == 429L) {
    warning(glue(
      "Sleeper API returned 429 (Rate Limited).\n",
      "Wait a few minutes before retrying. No API key is needed but requests\n",
      "should stay under approximately 1,000 per hour."
    ), call. = FALSE)
    return(NULL)
  }

  if (status != 200L) {
    warning(glue(
      "Sleeper API returned HTTP {status} for:\n  {url}"
    ), call. = FALSE)
    return(NULL)
  }

  parsed <- tryCatch(
    httr::content(response, as = "parsed", type = "application/json",
                  encoding = "UTF-8"),
    error = function(e) {
      warning(glue("Failed to parse Sleeper API response: {e$message}"),
              call. = FALSE)
      return(NULL)
    }
  )

  return(parsed)
}


# ==============================================================================
# SECTION 4: CONNECT TO SLEEPER LEAGUE
# ==============================================================================

#' Connect to a Sleeper League and Return Metadata
#'
#' @description
#' Fetches league metadata from the Sleeper API, including name, season, roster
#' settings, scoring settings, and roster positions. This is the primary entry
#' point -- most downstream functions accept a league_id and call this
#' internally, or you can call it first to inspect the league before proceeding.
#'
#' @param league_id Character. Sleeper league ID (numeric string, e.g.,
#'   "1048402504890023936"). Find yours at sleeper.app/leagues or via
#'   get_user_leagues().
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{league_id}{Character. The league ID passed in.}
#'     \item{name}{Character. League display name.}
#'     \item{season}{Integer. The NFL season year.}
#'     \item{season_type}{Character. "regular", "pre", or "post".}
#'     \item{status}{Character. "drafting", "in_season", "complete", etc.}
#'     \item{total_rosters}{Integer. Number of teams in the league.}
#'     \item{roster_positions}{Character vector. Roster slot configuration,
#'       e.g., c("QB", "RB", "RB", "WR", "WR", "TE", "FLEX", "SUPER_FLEX",
#'       "BN", "BN", ...).}
#'     \item{is_superflex}{Logical. TRUE if "SUPER_FLEX" is in roster_positions.}
#'     \item{scoring_settings}{Named list. Raw Sleeper scoring settings JSON.
#'       Pass to map_sleeper_scoring() or inspect directly.}
#'     \item{settings}{Named list. Other league settings (taxi, reserve, etc.).}
#'     \item{raw}{The full parsed API response for debugging.}
#'   }
#'   Returns NULL with a warning if the request fails.
#'
#' @examples
#' \dontrun{
#' # Connect to a league (replace with your actual league_id)
#' league <- connect_sleeper_league("1048402504890023936")
#' league$name
#' league$total_rosters
#' league$is_superflex
#' names(league$scoring_settings)  # inspect raw scoring fields
#' }
#'
#' @seealso map_sleeper_scoring, get_sleeper_rosters, get_sleeper_matchups
#' @export
connect_sleeper_league <- function(league_id) {

  league_id <- as.character(league_id)

  if (nchar(trimws(league_id)) == 0L) {
    stop("league_id cannot be empty.", call. = FALSE)
  }

  message(glue("Connecting to Sleeper league {league_id}..."))

  raw <- .sleeper_get(glue("/league/{league_id}"))

  if (is.null(raw)) {
    return(NULL)
  }

  roster_positions <- unlist(raw$roster_positions) %||% character(0)
  is_superflex     <- "SUPER_FLEX" %in% roster_positions

  league_meta <- list(
    league_id        = league_id,
    name             = raw$name %||% "Unknown",
    season           = as.integer(raw$season %||% NA_integer_),
    season_type      = raw$season_type %||% "unknown",
    status           = raw$status %||% "unknown",
    total_rosters    = as.integer(raw$total_rosters %||% NA_integer_),
    roster_positions = roster_positions,
    is_superflex     = is_superflex,
    scoring_settings = raw$scoring_settings %||% list(),
    settings         = raw$settings %||% list(),
    raw              = raw
  )

  message(glue(
    "  League: {league_meta$name} | ",
    "Season: {league_meta$season} | ",
    "Teams: {league_meta$total_rosters} | ",
    "Superflex: {league_meta$is_superflex}"
  ))

  return(league_meta)
}

# Null-coalescing operator (base R doesn't include %||%; define locally)
`%||%` <- function(x, y) if (!is.null(x)) x else y


# ==============================================================================
# SECTION 5: GET SLEEPER ROSTERS
# ==============================================================================

#' Pull All Rosters for a Sleeper League
#'
#' @description
#' Fetches all rosters for a league, returning a tidy tibble with one row per
#' player-roster combination. Each row links a Sleeper player_id to the roster
#' (team) it belongs to. Use match_sleeper_players() to join these Sleeper
#' player_ids to nflfastR gsis_ids.
#'
#' @param league_id Character. Sleeper league ID.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{league_id}{Character.}
#'     \item{roster_id}{Integer. Sleeper's internal team roster ID.}
#'     \item{owner_id}{Character. Sleeper user_id of the roster owner.}
#'     \item{player_id}{Character. Sleeper player_id for each rostered player.}
#'     \item{on_bench}{Logical. TRUE if player is on the bench/reserve.}
#'     \item{is_starter}{Logical. TRUE if player is in a starter slot.}
#'     \item{is_reserve}{Logical. TRUE if player is on IR/taxi.}
#'   }
#'   Returns NULL with a warning if the request fails.
#'
#' @examples
#' \dontrun{
#' rosters <- get_sleeper_rosters("1048402504890023936")
#'
#' # How many players per team?
#' rosters %>% count(roster_id)
#'
#' # Who owns each roster?
#' rosters %>% distinct(roster_id, owner_id)
#' }
#'
#' @seealso connect_sleeper_league, match_sleeper_players
#' @export
get_sleeper_rosters <- function(league_id) {

  league_id <- as.character(league_id)

  message(glue("Fetching rosters for league {league_id}..."))

  raw <- .sleeper_get(glue("/league/{league_id}/rosters"))

  if (is.null(raw) || length(raw) == 0L) {
    warning(glue("No roster data returned for league {league_id}."), call. = FALSE)
    return(NULL)
  }

  roster_rows <- purrr::map_dfr(raw, function(r) {

    all_players   <- unlist(r$players) %||% character(0)
    starters      <- unlist(r$starters) %||% character(0)
    reserve       <- unlist(r$reserve) %||% character(0)

    if (length(all_players) == 0L) {
      return(tibble::tibble())
    }

    tibble::tibble(
      league_id  = league_id,
      roster_id  = as.integer(r$roster_id),
      owner_id   = r$owner_id %||% NA_character_,
      player_id  = all_players,
      is_starter = all_players %in% starters,
      is_reserve = all_players %in% reserve,
      on_bench   = !(all_players %in% starters) & !(all_players %in% reserve)
    )
  })

  if (nrow(roster_rows) == 0L) {
    warning(glue("Rosters parsed but returned 0 players for league {league_id}."),
            call. = FALSE)
    return(NULL)
  }

  n_teams   <- length(unique(roster_rows$roster_id))
  n_players <- nrow(roster_rows)
  message(glue("  {n_teams} teams | {n_players} total rostered player-slots"))

  return(roster_rows)
}


# ==============================================================================
# SECTION 6: GET SLEEPER MATCHUPS
# ==============================================================================

#' Pull Weekly Matchup Data for a Sleeper League
#'
#' @description
#' Fetches matchup data for a given week, returning a tidy tibble with one row
#' per player-matchup combination. Each row shows what a player scored in
#' Sleeper's own scoring system for that week.
#'
#' @param league_id Character. Sleeper league ID.
#' @param week Integer. NFL regular season week (1-18).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{league_id}{Character.}
#'     \item{week}{Integer. The week number.}
#'     \item{matchup_id}{Integer. Identifies which two teams played each other.
#'       Both rosters in the same matchup share the same matchup_id.}
#'     \item{roster_id}{Integer.}
#'     \item{player_id}{Character. Sleeper player_id.}
#'     \item{player_points}{Numeric. Points this player scored in Sleeper's
#'       system for this week. NA if player is on bench (Sleeper only tracks
#'       starter points for some leagues).}
#'     \item{is_starter}{Logical. TRUE if player was a starter this week.}
#'     \item{team_points}{Numeric. Total points for this roster in this matchup.}
#'   }
#'   Returns NULL with a warning if the request fails or no data exists for
#'   the requested week.
#'
#' @examples
#' \dontrun{
#' # Pull Week 9 matchups
#' matchups <- get_sleeper_matchups("1048402504890023936", week = 9L)
#'
#' # Which teams won?
#' matchups %>%
#'   distinct(matchup_id, roster_id, team_points) %>%
#'   group_by(matchup_id) %>%
#'   mutate(winner = roster_id[which.max(team_points)])
#' }
#'
#' @seealso connect_sleeper_league, get_sleeper_rosters
#' @export
get_sleeper_matchups <- function(league_id, week) {

  league_id <- as.character(league_id)
  week      <- as.integer(week)

  if (is.na(week) || week < 1L || week > 18L) {
    stop(glue("week must be an integer between 1 and 18. Got: {week}"),
         call. = FALSE)
  }

  message(glue("Fetching Week {week} matchups for league {league_id}..."))

  raw <- .sleeper_get(glue("/league/{league_id}/matchups/{week}"))

  if (is.null(raw) || length(raw) == 0L) {
    warning(glue(
      "No matchup data returned for league {league_id}, Week {week}.\n",
      "This is expected if the season has not started or the week has not occurred."
    ), call. = FALSE)
    return(NULL)
  }

  matchup_rows <- purrr::map_dfr(raw, function(m) {

    all_players  <- unlist(m$players) %||% character(0)
    starters     <- unlist(m$starters) %||% character(0)
    pts_by_player <- m$players_points %||% list()

    if (length(all_players) == 0L) {
      return(tibble::tibble())
    }

    player_pts <- purrr::map_dbl(
      all_players,
      function(pid) {
        val <- pts_by_player[[pid]]
        if (is.null(val)) NA_real_ else as.numeric(val)
      }
    )

    tibble::tibble(
      league_id    = league_id,
      week         = week,
      matchup_id   = as.integer(m$matchup_id %||% NA_integer_),
      roster_id    = as.integer(m$roster_id),
      player_id    = all_players,
      player_points = player_pts,
      is_starter   = all_players %in% starters,
      team_points  = as.numeric(m$points %||% NA_real_)
    )
  })

  if (nrow(matchup_rows) == 0L) {
    warning(glue("Matchups parsed but returned 0 player rows for Week {week}."),
            call. = FALSE)
    return(NULL)
  }

  n_matchups <- length(unique(matchup_rows$matchup_id[!is.na(matchup_rows$matchup_id)]))
  message(glue("  {n_matchups} matchups | {nrow(matchup_rows)} player-week rows"))

  return(matchup_rows)
}


# ==============================================================================
# SECTION 7: INTERNAL SCORING SETTINGS PARSER
# ==============================================================================

#' Internal: Parse Sleeper Scoring Settings to Function Parameters
#'
#' @description
#' Converts a Sleeper scoring_settings list (raw API response) into a named
#' list of parameters for calculate_fantasy_points_ext(). Called by
#' map_sleeper_scoring() -- not intended for direct use.
#'
#' @param scoring_settings Named list. Raw scoring_settings from Sleeper API.
#' @param is_superflex Logical. Whether the league uses a SUPER_FLEX roster slot.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{params}{Named list ready for do.call(calculate_fantasy_points_ext).}
#'     \item{log}{Tibble documenting each field's mapping status.}
#'   }
#'
#' @keywords internal
.parse_scoring_settings <- function(scoring_settings, is_superflex = FALSE) {

  params  <- list()
  log_rows <- list()

  # --- STEP 1: Direct 1:1 mappings ---
  for (sleeper_field in names(SLEEPER_DIRECT_MAP)) {
    our_param <- SLEEPER_DIRECT_MAP[[sleeper_field]]
    raw_val   <- scoring_settings[[sleeper_field]]

    if (!is.null(raw_val)) {
      params[[our_param]] <- as.numeric(raw_val)
      log_rows[[length(log_rows) + 1L]] <- list(
        sleeper_field = sleeper_field,
        sleeper_value = as.numeric(raw_val),
        our_param     = our_param,
        our_value     = as.numeric(raw_val),
        status        = "mapped_direct",
        note          = NA_character_
      )
    }
  }

  # --- STEP 2: te_premium ---
  # Sleeper: bonus_rec_te (numeric, e.g., 0.5)
  # Our func: te_premium (logical TRUE/FALSE), fixed value = 0.5 (TE_PREMIUM_PTS)
  bonus_rec_te <- scoring_settings[["bonus_rec_te"]]
  if (!is.null(bonus_rec_te) && as.numeric(bonus_rec_te) > 0) {
    params[["te_premium"]] <- TRUE
    note_te <- if (abs(as.numeric(bonus_rec_te) - 0.5) > 0.001) {
      glue("Sleeper value {bonus_rec_te} differs from our fixed TE_PREMIUM_PTS = 0.5. ",
           "Our function awards exactly 0.5 per TE reception regardless of this value.")
    } else {
      NA_character_
    }
    log_rows[[length(log_rows) + 1L]] <- list(
      sleeper_field = "bonus_rec_te",
      sleeper_value = as.numeric(bonus_rec_te),
      our_param     = "te_premium",
      our_value     = TRUE,
      status        = if (is.na(note_te)) "mapped_derived" else "mapped_derived_warning",
      note          = note_te
    )
  } else if (!is.null(bonus_rec_te) && as.numeric(bonus_rec_te) == 0) {
    params[["te_premium"]] <- FALSE
    log_rows[[length(log_rows) + 1L]] <- list(
      sleeper_field = "bonus_rec_te", sleeper_value = 0,
      our_param = "te_premium", our_value = FALSE,
      status = "mapped_derived", note = NA_character_
    )
  }

  # --- STEP 3: hundred_yard_bonus ---
  # Sleeper has separate rec (bonus_rec_yd_100) and rush (bonus_rush_yd_100) bonuses.
  # Our function uses a single hundred_yard_bonus applied to both categories.
  # Strategy: use the larger of the two; warn if they differ.
  rec_100  <- as.numeric(scoring_settings[["bonus_rec_yd_100"]] %||% 0)
  rush_100 <- as.numeric(scoring_settings[["bonus_rush_yd_100"]] %||% 0)

  if (rec_100 > 0 || rush_100 > 0) {
    hundred_val <- max(rec_100, rush_100)
    params[["hundred_yard_bonus"]] <- hundred_val
    note_100 <- if (rec_100 != rush_100 && rec_100 > 0 && rush_100 > 0) {
      glue(
        "Sleeper has different bonuses: receiving={rec_100}, rushing={rush_100}. ",
        "Our function uses a single hundred_yard_bonus applied to both. ",
        "Using max value ({hundred_val}). If leagues differ by category, consider ",
        "separate calculate_fantasy_points_ext() calls."
      )
    } else {
      NA_character_
    }
    log_rows[[length(log_rows) + 1L]] <- list(
      sleeper_field = "bonus_rec_yd_100 / bonus_rush_yd_100",
      sleeper_value = glue("rec={rec_100}, rush={rush_100}"),
      our_param     = "hundred_yard_bonus",
      our_value     = hundred_val,
      status        = if (is.na(note_100)) "mapped_derived" else "mapped_derived_warning",
      note          = note_100
    )
  }

  # --- STEP 4: sack_penalty ---
  # Sleeper: pass_sack (stored as a negative number, e.g., -1)
  # Our func: sack_penalty (also negative, e.g., -1)
  pass_sack <- scoring_settings[["pass_sack"]]
  if (!is.null(pass_sack)) {
    params[["sack_penalty"]] <- as.numeric(pass_sack)
    log_rows[[length(log_rows) + 1L]] <- list(
      sleeper_field = "pass_sack", sleeper_value = as.numeric(pass_sack),
      our_param = "sack_penalty", our_value = as.numeric(pass_sack),
      status = "mapped_derived", note = NA_character_
    )
  }

  # --- STEP 5: first_down_points ---
  # Sleeper has position-specific first-down bonuses.
  # Our function uses a single first_down_points applied to all rushers/receivers.
  # Merge strategy: if all skill position values are equal, use that value.
  # If they differ, use the average and warn.
  fd_fields <- c(
    bonus_fd_rb = "bonus_fd_rb",
    bonus_fd_wr = "bonus_fd_wr",
    bonus_fd_te = "bonus_fd_te",
    bonus_fd_qb = "bonus_fd_qb"
  )
  fd_vals <- purrr::map_dbl(
    fd_fields,
    function(f) as.numeric(scoring_settings[[f]] %||% 0)
  )
  fd_nonzero <- fd_vals[fd_vals != 0]

  if (length(fd_nonzero) > 0) {
    fd_unique <- unique(fd_nonzero)
    if (length(fd_unique) == 1L) {
      params[["first_down_points"]] <- fd_unique
      log_rows[[length(log_rows) + 1L]] <- list(
        sleeper_field = "bonus_fd_rb/wr/te/qb",
        sleeper_value = fd_unique,
        our_param     = "first_down_points",
        our_value     = fd_unique,
        status        = "mapped_derived",
        note          = NA_character_
      )
    } else {
      fd_mean <- mean(fd_nonzero)
      params[["first_down_points"]] <- fd_mean
      log_rows[[length(log_rows) + 1L]] <- list(
        sleeper_field = "bonus_fd_rb/wr/te/qb",
        sleeper_value = paste(fd_vals, collapse = "/"),
        our_param     = "first_down_points",
        our_value     = fd_mean,
        status        = "mapped_derived_warning",
        note          = glue(
          "Position-specific first-down bonuses differ: ",
          "{paste(names(fd_nonzero), fd_nonzero, sep='=', collapse=', ')}. ",
          "Our function uses a single first_down_points value. Using mean ({round(fd_mean,3)})."
        )
      )
    }
  }

  # --- STEP 6: long_td_bonus ---
  # Sleeper uses bonus_rec_td_50p (receiving TDs of 50+ yards) and
  # bonus_rush_td_50p (rushing TDs of 50+ yards).
  # Our function: long_td_bonus + long_td_threshold.
  # Threshold: 49L (strictly greater than 49 = 50+ yards).
  rec_ltd  <- as.numeric(scoring_settings[["bonus_rec_td_50p"]] %||% 0)
  rush_ltd <- as.numeric(scoring_settings[["bonus_rush_td_50p"]] %||% 0)

  if (rec_ltd > 0 || rush_ltd > 0) {
    ltd_val <- max(rec_ltd, rush_ltd)
    params[["long_td_bonus"]]     <- ltd_val
    params[["long_td_threshold"]] <- 49L  # strictly greater than 49 = 50+ yards

    note_ltd <- if (rec_ltd != rush_ltd && rec_ltd > 0 && rush_ltd > 0) {
      glue(
        "Sleeper has different long TD bonuses: receiving={rec_ltd}, rushing={rush_ltd}. ",
        "Using max ({ltd_val}) with threshold = 49 (covers 50+ yards)."
      )
    } else {
      "Sleeper 50+ yard TD bonus mapped to long_td_bonus with long_td_threshold = 49."
    }
    log_rows[[length(log_rows) + 1L]] <- list(
      sleeper_field = "bonus_rec_td_50p / bonus_rush_td_50p",
      sleeper_value = glue("rec={rec_ltd}, rush={rush_ltd}"),
      our_param     = "long_td_bonus (threshold=49)",
      our_value     = ltd_val,
      status        = if (rec_ltd != rush_ltd && rec_ltd > 0 && rush_ltd > 0)
        "mapped_derived_warning" else "mapped_derived",
      note          = note_ltd
    )
  }

  # --- STEP 7: superflex_pass_td ---
  # If the league uses SUPER_FLEX, QBs are eligible at a flex position.
  # Many superflex leagues use 6-point passing TDs (vs standard 4-point).
  # Our function: superflex_pass_td overrides pass_td when non-zero.
  # Detection: if is_superflex AND pass_td >= 6, flag superflex_pass_td.
  # The user's pass_td from Sleeper settings already has the correct value;
  # we set superflex_pass_td to the same value so the override is active.
  pass_td_val <- params[["pass_td"]] %||% 4
  if (is_superflex && pass_td_val >= 6) {
    params[["superflex_pass_td"]] <- pass_td_val
    log_rows[[length(log_rows) + 1L]] <- list(
      sleeper_field = "SUPER_FLEX (roster position)",
      sleeper_value = glue("is_superflex=TRUE, pass_td={pass_td_val}"),
      our_param     = "superflex_pass_td",
      our_value     = pass_td_val,
      status        = "mapped_derived",
      note          = glue(
        "SUPER_FLEX roster slot detected. Setting superflex_pass_td = {pass_td_val} ",
        "so the passing TD override is active in calculate_fantasy_points_ext()."
      )
    )
  }

  # --- STEP 8: use_tiered_ppr ---
  # Sleeper does not support tiered PPR natively.
  # Our function's tiered PPR is a custom extension.
  # Always set use_tiered_ppr = FALSE when mapping from Sleeper.
  params[["use_tiered_ppr"]] <- FALSE
  log_rows[[length(log_rows) + 1L]] <- list(
    sleeper_field = "(no Sleeper equivalent)",
    sleeper_value = NA_real_,
    our_param     = "use_tiered_ppr",
    our_value     = FALSE,
    status        = "derived_no_sleeper_source",
    note          = paste(
      "Sleeper does not support tiered PPR. Set to FALSE.",
      "If you want tiered PPR, override this in the returned params list manually."
    )
  )

  # --- STEP 8b: Sleeper tiered reception fields ---
  # Sleeper supports per-yardage-bucket reception points.
  # These map directly to tiered_rec_tiers in calculate_fantasy_points_ext().
  # Fields: rec_0_4, rec_5_9, rec_10_19, rec_20_29, rec_30_39, rec_40p
  # Order must match the 6-element vector expected by calculate_fantasy_points_ext().
  tiered_fields <- c("rec_0_4", "rec_5_9", "rec_10_19",
                     "rec_20_29", "rec_30_39", "rec_40p")
  tiered_vals <- purrr::map_dbl(
    tiered_fields,
    function(f) as.numeric(scoring_settings[[f]] %||% 0)
  )

  # Only map tiered fields if at least one is non-zero
  if (any(tiered_vals != 0)) {
    params[["tiered_rec_tiers"]] <- tiered_vals
    params[["use_tiered_ppr"]]   <- TRUE  # override the FALSE set in STEP 8
    log_rows[[length(log_rows) + 1L]] <- list(
      sleeper_field = "rec_0_4/rec_5_9/rec_10_19/rec_20_29/rec_30_39/rec_40p",
      sleeper_value = paste(tiered_vals, collapse = "/"),
      our_param     = "tiered_rec_tiers + use_tiered_ppr",
      our_value     = paste(tiered_vals, collapse = "/"),
      status        = "mapped_derived",
      note          = paste(
        "Mapped to tiered_rec_tiers vector.",
        "use_tiered_ppr set to TRUE.",
        "If rec (flat PPR) is also non-zero, tiered_rec_tiers takes precedence."
      )
    )
  }

  # --- STEP 9: Log known-unsupported fields ---
  for (uf in SLEEPER_KNOWN_UNSUPPORTED) {
    raw_val <- scoring_settings[[uf]]
    if (!is.null(raw_val) && as.numeric(raw_val) != 0) {
      log_rows[[length(log_rows) + 1L]] <- list(
        sleeper_field = uf,
        sleeper_value = as.numeric(raw_val),
        our_param     = NA_character_,
        our_value     = NA_real_,
        status        = "unsupported",
        note          = "No equivalent parameter in calculate_fantasy_points_ext()."
      )
    }
  }

  # --- STEP 10: Log remaining unmapped fields ---
  all_logged   <- purrr::map_chr(log_rows, "sleeper_field")
  all_known    <- c(names(SLEEPER_DIRECT_MAP), SLEEPER_KNOWN_UNSUPPORTED,
                    "bonus_rec_te", "bonus_rec_yd_100", "bonus_rush_yd_100",
                    "pass_sack", "bonus_fd_rb", "bonus_fd_wr", "bonus_fd_te",
                    "bonus_fd_qb", "bonus_rec_td_50p", "bonus_rush_td_50p",
                    # Tiered reception fields handled in STEP 8b
                    "rec_0_4", "rec_5_9", "rec_10_19",
                    "rec_20_29", "rec_30_39", "rec_40p")
  remaining    <- setdiff(names(scoring_settings), all_known)

  for (rf in remaining) {
    raw_val <- scoring_settings[[rf]]
    if (!is.null(raw_val) && !is.na(as.numeric(raw_val)) && as.numeric(raw_val) != 0) {
      log_rows[[length(log_rows) + 1L]] <- list(
        sleeper_field = rf,
        sleeper_value = as.numeric(raw_val),
        our_param     = NA_character_,
        our_value     = NA_real_,
        status        = "unmapped",
        note          = glue("'{rf}' = {as.numeric(raw_val)}: no mapping in calculate_fantasy_points_ext().")
      )
    }
  }

  # Build log tibble
  log_tbl <- dplyr::bind_rows(
    purrr::map(log_rows, function(r) {
      tibble::tibble(
        sleeper_field = as.character(r$sleeper_field),
        sleeper_value = as.character(r$sleeper_value),
        our_param     = as.character(r$our_param %||% NA_character_),
        our_value     = as.character(r$our_value %||% NA_real_),
        status        = as.character(r$status),
        note          = as.character(r$note %||% NA_character_)
      )
    })
  )

  return(list(params = params, log = log_tbl))
}


# ==============================================================================
# SECTION 8: MAP SLEEPER SCORING TO FUNCTION PARAMETERS
# ==============================================================================

#' Map Sleeper League Scoring Settings to calculate_fantasy_points_ext() Parameters
#'
#' @description
#' Fetches a league's scoring settings and converts them to the named parameter
#' list expected by calculate_fantasy_points_ext() from R/17_extended_scoring.R.
#' The returned params list can be passed directly via do.call().
#'
#' Provides full transparency: the mapping_log tibble shows what was mapped,
#' what was derived (with any approximations), what was unsupported, and what
#' was left unmapped.
#'
#' @param league_id Character. Sleeper league ID.
#'
#' @return A named list with four elements:
#'   \describe{
#'     \item{params}{Named list of calculate_fantasy_points_ext() parameters.
#'       Ready for do.call(). Does NOT include pbp_data or roster_data --
#'       add those before calling. See examples.}
#'     \item{mapping_log}{Tibble. Row per Sleeper field showing:
#'       sleeper_field, sleeper_value, our_param, our_value, status, note.
#'       Status values: "mapped_direct", "mapped_derived",
#'       "mapped_derived_warning", "derived_no_sleeper_source",
#'       "unsupported", "unmapped".}
#'     \item{warnings}{Character vector of mapping issues requiring attention.
#'       Empty character(0) if no issues.}
#'     \item{league_meta}{The connect_sleeper_league() output for reference.}
#'   }
#'   Returns NULL with a warning if the API request fails.
#'
#' @details
#' **Parameter coverage:** All Season 1 and Season 2 parameters of
#' calculate_fantasy_points_ext() are either mapped from Sleeper settings or
#' set to appropriate defaults. The one exception is pick6_penalty -- Sleeper
#' does not expose this separately from pass_int, so it remains at the default
#' of -4 (total pick-6 cost = pass_int + pick6_penalty = -6).
#'
#' **Tiered PPR:** Our function's tiered PPR is a custom extension not present
#' in Sleeper. Map always sets use_tiered_ppr = FALSE. If you want tiered PPR,
#' modify params$use_tiered_ppr manually after calling this function.
#'
#' **100-yard bonus:** Sleeper can set different bonuses for rushing and receiving
#' 100-yard games. Our function applies one value to both. If they differ, the
#' larger value is used and a warning is recorded.
#'
#' @examples
#' \dontrun{
#' library(here)
#' source(here::here("R", "15_multi_season_pbp.R"))
#' source(here::here("R", "17_extended_scoring.R"))
#' source(here::here("R", "19_sleeper_api.R"))
#'
#' # Step 1: Map your league's scoring settings
#' scoring_map <- map_sleeper_scoring("1048402504890023936")
#'
#' # Step 2: Inspect the mapping
#' scoring_map$mapping_log
#' scoring_map$warnings  # should be character(0) for most standard leagues
#'
#' # Step 3: Load data
#' pbp    <- load_normalized_season(2025)
#' roster <- nflreadr::load_rosters(seasons = 2025)
#'
#' # Step 4: Calculate fantasy points with your exact league settings
#' fp <- do.call(
#'   calculate_fantasy_points_ext,
#'   c(list(pbp_data = pbp, roster_data = roster, season = 2025L),
#'     scoring_map$params)
#' )
#'
#' head(fp[, c("player_name", "position", "total_fantasy_points")])
#' }
#'
#' @seealso connect_sleeper_league, calculate_fantasy_points_ext
#' @export
map_sleeper_scoring <- function(league_id) {

  league_id <- as.character(league_id)

  # Fetch league metadata (includes scoring_settings and roster_positions)
  league_meta <- connect_sleeper_league(league_id)

  if (is.null(league_meta)) {
    return(NULL)
  }

  scoring_settings <- league_meta$scoring_settings

  if (length(scoring_settings) == 0L) {
    warning(glue(
      "League {league_id} has no scoring_settings in the API response.\n",
      "This may occur for pre-draft leagues or leagues in setup. ",
      "Returning default parameters."
    ), call. = FALSE)
    return(list(
      params      = get_ext_scoring_defaults(),
      mapping_log = tibble::tibble(),
      warnings    = "No scoring settings available; defaults used.",
      league_meta = league_meta
    ))
  }

  message(glue(
    "  Parsing {length(scoring_settings)} Sleeper scoring fields..."
  ))

  # Parse settings into params + log
  parsed <- .parse_scoring_settings(
    scoring_settings = scoring_settings,
    is_superflex     = league_meta$is_superflex
  )

  params      <- parsed$params
  mapping_log <- parsed$log

  # Collect warnings from log
  warning_rows <- mapping_log[
    grepl("warning|unmapped", mapping_log$status, ignore.case = TRUE) &
    !is.na(mapping_log$note),
  ]
  warnings <- if (nrow(warning_rows) > 0L) {
    warning_rows$note[!is.na(warning_rows$note)]
  } else {
    character(0)
  }

  # Surface any warnings to the console
  if (length(warnings) > 0L) {
    purrr::walk(warnings, function(w) {
      warning(glue("[map_sleeper_scoring] {w}"), call. = FALSE)
    })
  }

  # Summary counts
  n_mapped     <- sum(grepl("^mapped", mapping_log$status))
  n_unsupported <- sum(mapping_log$status == "unsupported")
  n_unmapped   <- sum(mapping_log$status == "unmapped")

  message(glue(
    "  Mapping complete: {n_mapped} mapped | ",
    "{n_unsupported} unsupported (DEF/ST/K) | ",
    "{n_unmapped} unmapped"
  ))

  if (n_unmapped > 0L) {
    unmapped_fields <- mapping_log$sleeper_field[mapping_log$status == "unmapped"]
    message(glue(
      "  Unmapped non-zero fields: {paste(unmapped_fields, collapse=', ')}"
    ))
  }

  return(list(
    params      = params,
    mapping_log = mapping_log,
    warnings    = warnings,
    league_meta = league_meta
  ))
}


# ==============================================================================
# SECTION 9: PLAYER ID RECONCILIATION
# ==============================================================================

#' Normalize a Player Name for Fuzzy Matching
#'
#' @description
#' Strips suffixes, punctuation, and spacing inconsistencies for string
#' distance matching. Used internally by match_sleeper_players().
#'
#' @param name Character vector of player names.
#' @return Character vector of normalized names.
#' @keywords internal
.normalize_player_name <- function(name) {
  name %>%
    stringr::str_to_lower() %>%
    # Remove generational suffixes
    stringr::str_remove_all("\\b(jr\\.?|sr\\.?|ii|iii|iv|v)\\b") %>%
    # Remove punctuation except spaces
    stringr::str_remove_all("[^a-z ]") %>%
    # Collapse multiple spaces
    stringr::str_squish()
}


#' Fetch and Cache the Sleeper NFL Player Database
#'
#' @description
#' Internal. Fetches all NFL players from Sleeper's /players/nfl endpoint
#' and caches as RDS. The response is large (~5MB JSON, ~4,000+ players).
#' Only re-fetches if force_refresh = TRUE.
#'
#' The player objects include gsis_id which maps directly to nflfastR player_id,
#' enabling high-confidence reconciliation without fuzzy matching for most players.
#'
#' @param force_refresh Logical. Force re-download even if cache exists. Default FALSE.
#' @return A tibble with Sleeper player profile data.
#' @keywords internal
.get_sleeper_players <- function(force_refresh = FALSE) {

  if (!force_refresh && file.exists(SLEEPER_PLAYERS_CACHE)) {
    message("Loading Sleeper player database from cache...")
    players <- readRDS(SLEEPER_PLAYERS_CACHE)
    message(glue("  {format(nrow(players), big.mark=',')} players loaded from cache."))
    return(players)
  }

  message("Fetching Sleeper NFL player database (large request, ~5MB)...")
  raw <- .sleeper_get("/players/nfl")

  if (is.null(raw)) {
    stop("Failed to fetch Sleeper player database. Cannot perform player matching.",
         call. = FALSE)
  }

  # raw is a named list: names are Sleeper player_ids, values are player objects
  player_tbl <- purrr::map_dfr(
    names(raw),
    function(pid) {
      p <- raw[[pid]]
      tibble::tibble(
        sleeper_player_id = pid,
        full_name         = p$full_name  %||% NA_character_,
        first_name        = p$first_name %||% NA_character_,
        last_name         = p$last_name  %||% NA_character_,
        position          = p$position   %||% NA_character_,
        team              = p$team        %||% NA_character_,
        age               = as.integer(p$age %||% NA_integer_),
        status            = p$status     %||% NA_character_,
        gsis_id           = p$gsis_id    %||% NA_character_,
        years_exp         = as.integer(p$years_exp %||% NA_integer_)
      )
    }
  )

  # Cache locally
  if (!dir.exists(SLEEPER_CACHE_DIR)) {
    dir.create(SLEEPER_CACHE_DIR, recursive = TRUE)
  }
  saveRDS(player_tbl, SLEEPER_PLAYERS_CACHE)
  message(glue(
    "  {format(nrow(player_tbl), big.mark=',')} players fetched and cached at:\n",
    "  {SLEEPER_PLAYERS_CACHE}"
  ))

  return(player_tbl)
}


#' Reconcile Sleeper Player IDs with nflfastR GSIS IDs
#'
#' @description
#' Links Sleeper's internal player_ids to nflfastR GSIS IDs so that Sleeper
#' roster and matchup data can be joined to nflfastR play-by-play.
#'
#' Matching proceeds in three stages:
#'   1. **GSIS match** -- Sleeper player objects contain a gsis_id field that
#'      directly matches nflfastR player_id. Highest confidence.
#'   2. **Exact name match** -- normalized full name + position + team match.
#'   3. **Fuzzy name match** -- normalized name within FUZZY_MATCH_DISTANCE
#'      (see constants), same position. Used for name variations (e.g.,
#'      "D.K. Metcalf" vs "DK Metcalf").
#'
#' Reports match rate by position and stops if the overall rate falls below
#' MATCH_RATE_ERROR_THRESHOLD (default 0.70).
#'
#' @param sleeper_player_ids Character vector. Sleeper player_ids to reconcile.
#'   These are the values in the player_id column from get_sleeper_rosters().
#' @param nflreadr_roster Tibble. nflreadr roster data from
#'   nflreadr::load_rosters(). Must include: gsis_id, full_name, position, team.
#' @param force_refresh Logical. Force re-download of Sleeper player database.
#'   Default FALSE.
#' @param min_match_rate Numeric. Minimum acceptable match rate (0-1). Stops
#'   with an error if the overall match rate falls below this. Default 0.70.
#'
#' @return A tibble with one row per input Sleeper player_id:
#'   \describe{
#'     \item{sleeper_player_id}{Character. Input Sleeper player_id.}
#'     \item{sleeper_name}{Character. Player name from Sleeper database.}
#'     \item{sleeper_position}{Character.}
#'     \item{sleeper_team}{Character.}
#'     \item{gsis_id}{Character. nflfastR player_id. NA if not matched.}
#'     \item{nflfastr_name}{Character. Name from nflreadr roster. NA if unmatched.}
#'     \item{match_method}{Character. "gsis", "exact_name", "fuzzy_name", or "unmatched".}
#'     \item{match_confidence}{Character. "high", "medium", "low", or "none".}
#'   }
#'
#' @examples
#' \dontrun{
#' library(nflreadr)
#' source(here::here("R", "19_sleeper_api.R"))
#'
#' rosters     <- get_sleeper_rosters("1048402504890023936")
#' nfl_roster  <- nflreadr::load_rosters(seasons = 2025)
#' player_ids  <- unique(rosters$player_id)
#'
#' reconciled <- match_sleeper_players(player_ids, nfl_roster)
#'
#' # Check match rate by position
#' reconciled %>%
#'   count(sleeper_position, match_method) %>%
#'   group_by(sleeper_position) %>%
#'   mutate(pct = n / sum(n))
#' }
#'
#' @seealso get_sleeper_rosters, connect_sleeper_league
#' @export
match_sleeper_players <- function(sleeper_player_ids,
                                  nflreadr_roster,
                                  force_refresh  = FALSE,
                                  min_match_rate = MATCH_RATE_ERROR_THRESHOLD) {

  sleeper_player_ids <- as.character(unique(sleeper_player_ids))
  n_input <- length(sleeper_player_ids)

  if (n_input == 0L) {
    stop("sleeper_player_ids is empty.", call. = FALSE)
  }

  # Validate nflreadr roster columns
  required_cols <- c("gsis_id", "full_name", "position", "team")
  missing_cols  <- setdiff(required_cols, names(nflreadr_roster))
  if (length(missing_cols) > 0L) {
    stop(glue(
      "nflreadr_roster is missing required columns: {paste(missing_cols, collapse=', ')}\n",
      "Ensure you are using nflreadr::load_rosters() output."
    ), call. = FALSE)
  }

  message(glue("Reconciling {n_input} Sleeper player IDs to nflfastR GSIS IDs..."))

  # Load Sleeper player database
  sleeper_db <- .get_sleeper_players(force_refresh = force_refresh)

  # Subset to the requested player_ids
  sleeper_subset <- sleeper_db %>%
    dplyr::filter(sleeper_player_id %in% sleeper_player_ids) %>%
    dplyr::distinct(sleeper_player_id, .keep_all = TRUE)

  # Players in input but not in Sleeper database (very rare: invalid IDs)
  unrecognized <- setdiff(sleeper_player_ids, sleeper_subset$sleeper_player_id)
  if (length(unrecognized) > 0L) {
    warning(glue(
      "{length(unrecognized)} Sleeper player_ids not found in Sleeper player database:\n",
      "  {paste(head(unrecognized, 10), collapse=', ')}",
      if (length(unrecognized) > 10) glue(" ... and {length(unrecognized)-10} more") else ""
    ), call. = FALSE)
  }

  # Prepare nflreadr roster for matching: normalize names, deduplicate on gsis_id
  nflfastr_lookup <- nflreadr_roster %>%
    dplyr::filter(!is.na(gsis_id), !is.na(full_name)) %>%
    dplyr::distinct(gsis_id, .keep_all = TRUE) %>%
    dplyr::mutate(
      name_norm = .normalize_player_name(full_name),
      team_std  = toupper(trimws(team)),
      pos_std   = toupper(trimws(position))
    )

  sleeper_subset <- sleeper_subset %>%
    dplyr::mutate(
      name_norm    = .normalize_player_name(dplyr::coalesce(full_name, "")),
      gsis_id_raw  = trimws(dplyr::coalesce(gsis_id, "")),
      team_std     = toupper(trimws(dplyr::coalesce(team, ""))),
      pos_std      = toupper(trimws(dplyr::coalesce(position, "")))
    )

  # --- STAGE 1: GSIS ID direct match ---
  message("  Stage 1: GSIS ID direct match...")

  gsis_matched <- sleeper_subset %>%
    dplyr::filter(nchar(gsis_id_raw) > 0L) %>%
    dplyr::left_join(
      nflfastr_lookup %>%
        dplyr::select(gsis_id, nflfastr_name = full_name),
      by = c("gsis_id_raw" = "gsis_id")
    ) %>%
    dplyr::filter(!is.na(nflfastr_name)) %>%
    dplyr::mutate(
      gsis_id          = gsis_id_raw,
      match_method     = "gsis",
      match_confidence = "high"
    )

  matched_ids <- gsis_matched$sleeper_player_id
  message(glue("    GSIS matches: {nrow(gsis_matched)} of {n_input}"))

  # --- STAGE 2: Exact name + position match ---
  remaining_2 <- sleeper_subset %>%
    dplyr::filter(!sleeper_player_id %in% matched_ids)

  message(glue("  Stage 2: Exact name + position match ({nrow(remaining_2)} remaining)..."))

  name_matched <- tibble::tibble()
  if (nrow(remaining_2) > 0L) {
    name_matched <- remaining_2 %>%
      dplyr::left_join(
        nflfastr_lookup %>%
          dplyr::select(matched_gsis = gsis_id, nflfastr_name = full_name,
                        name_norm, pos_std),
        by = c("name_norm", "pos_std")
      ) %>%
      dplyr::filter(!is.na(matched_gsis)) %>%
      dplyr::mutate(
        gsis_id          = matched_gsis,
        match_method     = "exact_name",
        match_confidence = "high"
      ) %>%
      dplyr::select(-matched_gsis)
    matched_ids <- c(matched_ids, name_matched$sleeper_player_id)
    message(glue("    Exact name matches: {nrow(name_matched)}"))
  }

  # --- STAGE 3: Fuzzy name match ---
  remaining_3 <- sleeper_subset %>%
    dplyr::filter(!sleeper_player_id %in% matched_ids)

  message(glue("  Stage 3: Fuzzy name match ({nrow(remaining_3)} remaining)..."))

  fuzzy_matched <- tibble::tibble()
  if (nrow(remaining_3) > 0L) {
    fuzzy_rows <- purrr::map_dfr(seq_len(nrow(remaining_3)), function(i) {
      row     <- remaining_3[i, ]
      s_name  <- row$name_norm
      s_pos   <- row$pos_std

      # Restrict fuzzy search to same position to avoid false positives
      candidates <- nflfastr_lookup %>%
        dplyr::filter(pos_std == s_pos)

      if (nrow(candidates) == 0L || nchar(s_name) == 0L) {
        return(tibble::tibble(
          sleeper_player_id = row$sleeper_player_id,
          gsis_id           = NA_character_,
          nflfastr_name     = NA_character_,
          match_method      = "unmatched",
          match_confidence  = "none"
        ))
      }

      hits <- agrep(s_name, candidates$name_norm,
                    max.distance = FUZZY_MATCH_DISTANCE,
                    value = FALSE)

      if (length(hits) == 0L) {
        return(tibble::tibble(
          sleeper_player_id = row$sleeper_player_id,
          gsis_id           = NA_character_,
          nflfastr_name     = NA_character_,
          match_method      = "unmatched",
          match_confidence  = "none"
        ))
      }

      # If multiple fuzzy hits, take the one with the closest string distance
      hit_candidates <- candidates[hits, ]
      distances <- stringr::str_length(hit_candidates$name_norm) -
        stringr::str_length(s_name)
      best_idx <- which.min(abs(distances))
      best     <- hit_candidates[best_idx, ]

      tibble::tibble(
        sleeper_player_id = row$sleeper_player_id,
        gsis_id           = best$gsis_id,
        nflfastr_name     = best$full_name,
        match_method      = "fuzzy_name",
        match_confidence  = "medium"
      )
    })

    fuzzy_matched <- fuzzy_rows %>%
      dplyr::filter(match_method == "fuzzy_name")
    matched_ids <- c(matched_ids, fuzzy_matched$sleeper_player_id)
    message(glue("    Fuzzy name matches: {nrow(fuzzy_matched)}"))
  }

  # --- Combine all results ---
  # Unmatched players
  unmatched_ids <- setdiff(sleeper_player_ids, matched_ids)
  unmatched_tbl <- sleeper_subset %>%
    dplyr::filter(sleeper_player_id %in% unmatched_ids) %>%
    dplyr::mutate(
      gsis_id          = NA_character_,
      nflfastr_name    = NA_character_,
      match_method     = "unmatched",
      match_confidence = "none"
    ) %>%
    dplyr::select(sleeper_player_id, sleeper_name = full_name,
                  sleeper_position = position, sleeper_team = team,
                  gsis_id, nflfastr_name, match_method, match_confidence)

  # Stack all matched results
  output_cols <- c("sleeper_player_id", "sleeper_name", "sleeper_position",
                   "sleeper_team", "gsis_id", "nflfastr_name",
                   "match_method", "match_confidence")

  gsis_out <- if (nrow(gsis_matched) > 0L) {
    gsis_matched %>%
      dplyr::mutate(sleeper_name = full_name, sleeper_position = position,
                    sleeper_team = team) %>%
      dplyr::select(dplyr::all_of(output_cols))
  } else tibble::tibble()

  name_out <- if (nrow(name_matched) > 0L) {
    name_matched %>%
      dplyr::mutate(sleeper_name = full_name, sleeper_position = position,
                    sleeper_team = team) %>%
      dplyr::select(dplyr::all_of(output_cols))
  } else tibble::tibble()

  fuzzy_out <- if (nrow(fuzzy_matched) > 0L) {
    remaining_3 %>%
      dplyr::inner_join(
        fuzzy_matched %>% dplyr::select(sleeper_player_id,
                                        matched_gsis = gsis_id,
                                        nflfastr_name, match_method,
                                        match_confidence),
        by = "sleeper_player_id"
      ) %>%
      dplyr::mutate(
        gsis_id          = matched_gsis,
        sleeper_name     = full_name,
        sleeper_position = position,
        sleeper_team     = team
      ) %>%
      dplyr::select(dplyr::all_of(output_cols))
  } else tibble::tibble()

  result <- dplyr::bind_rows(gsis_out, name_out, fuzzy_out, unmatched_tbl) %>%
    dplyr::arrange(sleeper_player_id)

  # --- Match rate summary ---
  n_matched    <- sum(result$match_method != "unmatched", na.rm = TRUE)
  match_rate   <- n_matched / n_input

  # Position-level summary
  pos_summary <- result %>%
    dplyr::count(sleeper_position, match_method) %>%
    dplyr::group_by(sleeper_position) %>%
    dplyr::mutate(pct = n / sum(n)) %>%
    dplyr::ungroup()

  message(glue(
    "\n  Match summary: {n_matched}/{n_input} players matched ",
    "({round(match_rate * 100, 1)}%)\n",
    "  by method -- ",
    "GSIS: {sum(result$match_method=='gsis')}, ",
    "Exact: {sum(result$match_method=='exact_name')}, ",
    "Fuzzy: {sum(result$match_method=='fuzzy_name')}, ",
    "Unmatched: {sum(result$match_method=='unmatched')}"
  ))

  if (match_rate < MATCH_RATE_WARNING_THRESHOLD) {
    unmatched_players <- result %>%
      dplyr::filter(match_method == "unmatched") %>%
      dplyr::pull(sleeper_name)
    warning(glue(
      "Player match rate {round(match_rate*100,1)}% is below the warning threshold ",
      "({MATCH_RATE_WARNING_THRESHOLD*100}%).\n",
      "Unmatched players: {paste(head(unmatched_players, 10), collapse=', ')}",
      if (length(unmatched_players) > 10L)
        glue(" ... and {length(unmatched_players)-10} more") else ""
    ), call. = FALSE)
  }

  if (match_rate < min_match_rate) {
    stop(glue(
      "Player match rate {round(match_rate*100,1)}% is below the error threshold ",
      "({min_match_rate*100}%). ",
      "Check that nflreadr_roster matches the correct NFL season and that the ",
      "Sleeper player database is up to date (force_refresh = TRUE to re-fetch)."
    ), call. = FALSE)
  }

  attr(result, "match_rate")    <- match_rate
  attr(result, "pos_summary")   <- pos_summary
  attr(result, "n_input")       <- n_input

  return(result)
}


# ==============================================================================
# SECTION 10: CONVENIENCE -- GET USER LEAGUES
# ==============================================================================

#' Look Up All Sleeper Leagues for a User in a Given Season
#'
#' @description
#' Convenience function to find your league_ids without opening the Sleeper app.
#' Takes a Sleeper username (or user_id), returns a tibble of all leagues for
#' that user in the specified season.
#'
#' @param username Character. Sleeper username (not display name). The username
#'   is visible in your Sleeper profile URL: sleeper.app/user/{username}.
#' @param season Integer. NFL season year. Default: CURRENT_NFL_SEASON (2025).
#'
#' @return A tibble with one row per league:
#'   \describe{
#'     \item{league_id}{Character. Use this to call other functions.}
#'     \item{name}{Character. League display name.}
#'     \item{season}{Integer.}
#'     \item{status}{Character.}
#'     \item{total_rosters}{Integer.}
#'     \item{is_superflex}{Logical.}
#'     \item{scoring_type}{Character. "ppr", "half_ppr", or "standard".}
#'   }
#'   Returns NULL with a warning if the user is not found.
#'
#' @examples
#' \dontrun{
#' # Find all your leagues for 2025
#' my_leagues <- get_user_leagues("your_sleeper_username")
#' my_leagues[, c("name", "league_id", "scoring_type", "is_superflex")]
#'
#' # Then connect to the one you want
#' league <- connect_sleeper_league(my_leagues$league_id[1])
#' }
#'
#' @seealso connect_sleeper_league, map_sleeper_scoring
#' @export
get_user_leagues <- function(username, season = CURRENT_NFL_SEASON) {

  username <- trimws(as.character(username))
  season   <- as.integer(season)

  if (nchar(username) == 0L) {
    stop("username cannot be empty.", call. = FALSE)
  }

  # Step 1: resolve username to user_id
  message(glue("Looking up Sleeper user: {username}..."))
  user_raw <- .sleeper_get(glue("/user/{username}"))

  if (is.null(user_raw)) {
    warning(glue(
      "Could not find Sleeper user '{username}'.\n",
      "Check that the username is correct (not the display name)."
    ), call. = FALSE)
    return(NULL)
  }

  user_id      <- user_raw$user_id %||% NA_character_
  display_name <- user_raw$display_name %||% username

  message(glue("  Found user: {display_name} (user_id: {user_id})"))

  # Step 2: fetch leagues
  leagues_raw <- .sleeper_get(glue("/user/{user_id}/leagues/nfl/{season}"))

  if (is.null(leagues_raw) || length(leagues_raw) == 0L) {
    warning(glue(
      "No leagues found for user '{username}' in season {season}."
    ), call. = FALSE)
    return(NULL)
  }

  # Parse into tidy tibble
  leagues_tbl <- purrr::map_dfr(leagues_raw, function(lg) {
    roster_positions <- unlist(lg$roster_positions) %||% character(0)
    ppr_val <- as.numeric(lg$scoring_settings[["rec"]] %||% 0)
    scoring_type <- dplyr::case_when(
      ppr_val >= 0.9 ~ "ppr",
      ppr_val >= 0.4 ~ "half_ppr",
      TRUE           ~ "standard"
    )
    tibble::tibble(
      league_id     = lg$league_id  %||% NA_character_,
      name          = lg$name       %||% "Unknown",
      season        = as.integer(lg$season %||% NA_integer_),
      status        = lg$status     %||% "unknown",
      total_rosters = as.integer(lg$total_rosters %||% NA_integer_),
      is_superflex  = "SUPER_FLEX" %in% roster_positions,
      scoring_type  = scoring_type
    )
  })

  message(glue("  {nrow(leagues_tbl)} league(s) found for season {season}."))
  return(leagues_tbl)
}
