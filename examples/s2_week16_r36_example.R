# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 16
# EXAMPLE: Start/Sit Uncertainty Quantification (R/36)
# File: examples/s2_week16_r36_example.R
#
# WHAT THIS DOES
# --------------
# End-to-end runnable example for analyze_start_sit() (R/36). It builds a league
# source, solves the weekly lineup with R/35, then enriches that lineup with the
# R/36 uncertainty layer: the probability each start is correct, the typical
# miss when a call goes wrong, a stakes label per slot, and one week-level risk
# score. The DEF section demonstrates the rostered-vs-waiver streaming check.
#
# BEFORE RUNNING
# --------------
#   1. Identifiers: leave SLEEPER_USERNAME NULL to be prompted for your Sleeper
#      username at runtime. The script then lists your leagues, lets you pick
#      one, and matches your roster automatically. You can hardcode
#      SLEEPER_USERNAME / LEAGUE_ID / USER_ROSTER_ID to skip any prompt; a
#      non-interactive caller (the future Shiny app) supplies them directly.
#   2. Confirm the cached inputs exist (the script checks and stops if not):
#        data/season2_cache/s2_week15_reconciled_projections.rds   (R/32)
#        data/season2_cache/s2_week16_def_st_projections.rds        (R/34)
#      If the DEF file is absent, the script builds it with project_def_st().
#   3. A live Sleeper connection is required (the lookups and build_league_source
#      hit the API).
#
# RUN
# ---
#   source(here::here("examples", "s2_week16_r36_example.R"))
# ==============================================================================

library(here)
library(dplyr)
library(glue)

# Source R/36. This pulls in R/35 and the full R/33 -> R/32 -> R/29 -> R/19/R/15
# chain through R/35's own guarded source() calls (which include R/19's Sleeper
# helpers: get_user_leagues(), get_sleeper_rosters(), build_league_source()).
source(here::here("R", "36_start_sit_uq.R"))

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

SEASON         <- 2026L
PRIOR_SEASON   <- SEASON - 1L
WEEK           <- 6L                 # the week to solve and score; NULL = preseason

# Identifiers. Leave SLEEPER_USERNAME NULL to be prompted; you type the same
# username that appears in your Sleeper profile URL (sleeper.app/user/NAME), not
# a numeric id. LEAGUE_ID and USER_ROSTER_ID are then resolved for you: the
# league is picked from a list of your leagues, and your roster is matched
# automatically from your username. Set any of these directly (or have the
# future Shiny app set them) to skip the corresponding prompt.
SLEEPER_USERNAME <- NULL
LEAGUE_ID        <- NULL
USER_ROSTER_ID   <- NULL

CACHE_DIR        <- here::here("data", "season2_cache")
RECONCILED_PATH  <- file.path(CACHE_DIR, "s2_week15_reconciled_projections.rds")
DEF_PROJ_PATH    <- file.path(CACHE_DIR, "s2_week16_def_st_projections.rds")

# ------------------------------------------------------------------------------
# RESOLVE IDENTIFIERS (username -> league pick-list -> auto roster)
# ------------------------------------------------------------------------------
# This whole section is entry-layer plumbing. In the Shiny app a text box and a
# dropdown replace the prompts; the resolved league_id and roster_id then flow
# into build_league_source() exactly as they do here. The library functions
# never prompt on their own.

# Returns a supplied value, else prompts (interactive), else errors / defaults.
.resolve_value <- function(current, prompt, required = TRUE,
                           as_integer = FALSE, default = NULL) {
  is_blank <- is.null(current) ||
    (length(current) == 1L &&
       (is.na(current) || !nzchar(trimws(as.character(current)))))

  if (is_blank) {
    if (!interactive()) {
      if (!is.null(default)) return(default)
      stop(glue("{trimws(prompt)} is required, but this session is not ",
                "interactive and no value was supplied."), call. = FALSE)
    }
    repeat {
      ans <- trimws(readline(prompt = prompt))
      if (!nzchar(ans) && !is.null(default)) ans <- as.character(default)
      if (nzchar(ans) || !required) break
      cat("  A value is required.\n")
    }
    current <- ans
  }

  if (as_integer) {
    iv <- suppressWarnings(as.integer(current))
    if (is.na(iv)) {
      stop(glue("Expected a whole number for: {trimws(prompt)}"),
           call. = FALSE)
    }
    return(iv)
  }
  as.character(current)
}

# Resolve a Sleeper username to its user_id (NA if not found). Reuses R/19's
# HTTP layer rather than re-implementing the request.
.sleeper_user_id <- function(username) {
  user_raw <- tryCatch(.sleeper_get(glue("/user/{username}")),
                       error = function(e) NULL)
  if (is.null(user_raw)) return(NA_character_)
  user_raw$user_id %||% NA_character_
}

# Show the user's leagues as a numbered list and return the chosen row. Falls
# back one season if the requested season has none yet (common in the offseason).
.choose_league <- function(username, season) {
  leagues <- get_user_leagues(username, season = season)
  if (is.null(leagues) || nrow(leagues) == 0L) {
    message(glue("  No leagues for season {season}; trying {season - 1L}."))
    season  <- season - 1L
    leagues <- get_user_leagues(username, season = season)
  }
  if (is.null(leagues) || nrow(leagues) == 0L) {
    stop(glue("No NFL leagues found for Sleeper user '{username}'. ",
              "Check that the username is correct."), call. = FALSE)
  }

  cat(glue("\nLeagues for {username} (season {season}):"), "\n")
  for (i in seq_len(nrow(leagues))) {
    sf <- if (isTRUE(leagues$is_superflex[i])) ", superflex" else ""
    cat(glue("  [{i}] {leagues$name[i]} ",
             "(id {leagues$league_id[i]}, {leagues$scoring_type[i]}{sf}, ",
             "{leagues$total_rosters[i]} teams)"), "\n")
  }

  if (nrow(leagues) == 1L) {
    cat("  Only one league found; selecting it.\n")
    return(leagues[1, , drop = FALSE])
  }

  repeat {
    sel <- suppressWarnings(as.integer(trimws(
      readline(prompt = glue("Pick a league [1-{nrow(leagues)}]: ")))))
    if (!is.na(sel) && sel >= 1L && sel <= nrow(leagues)) break
    cat("  Please enter a number from the list.\n")
  }
  leagues[sel, , drop = FALSE]
}

# Match the user's roster_id in a league by owner_id (the Sleeper user_id on each
# roster). NA if no match (co-owner-only or not in the league).
.resolve_my_roster <- function(league_id, user_id) {
  if (is.na(user_id)) return(NA_integer_)
  rosters <- tryCatch(get_sleeper_rosters(league_id), error = function(e) NULL)
  if (is.null(rosters) || nrow(rosters) == 0L) return(NA_integer_)
  mine <- rosters %>%
    dplyr::filter(.data$owner_id == user_id) %>%
    dplyr::distinct(roster_id) %>%
    dplyr::pull(roster_id)
  if (length(mine) == 0L) return(NA_integer_)
  as.integer(mine[1])
}

# 1) username
SLEEPER_USERNAME <- .resolve_value(SLEEPER_USERNAME,
                                   prompt = "Enter your Sleeper username: ",
                                   required = TRUE)
user_id <- .sleeper_user_id(SLEEPER_USERNAME)
if (is.na(user_id)) {
  stop(glue("Could not find Sleeper user '{SLEEPER_USERNAME}'. ",
            "Use the username from your profile URL, not the display name."),
       call. = FALSE)
}

# 2) league: pick from the list (unless one was hardcoded above)
if (is.null(LEAGUE_ID)) {
  chosen    <- .choose_league(SLEEPER_USERNAME, season = SEASON)
  LEAGUE_ID <- chosen$league_id
  cat(glue("  Using league: {chosen$name} ({LEAGUE_ID})"), "\n")
}

# 3) roster: auto-match from the username, prompt only if that fails
if (is.null(USER_ROSTER_ID)) {
  USER_ROSTER_ID <- .resolve_my_roster(LEAGUE_ID, user_id)
  if (is.na(USER_ROSTER_ID)) {
    cat("  Could not auto-detect your roster from your username.\n")
    USER_ROSTER_ID <- .resolve_value(
      NULL, prompt = "Enter your roster_id manually: ",
      required = TRUE, as_integer = TRUE)
  } else {
    cat(glue("  Matched your roster_id: {USER_ROSTER_ID}"), "\n")
  }
}

# ------------------------------------------------------------------------------
# LOAD INPUTS
# ------------------------------------------------------------------------------

if (!file.exists(RECONCILED_PATH)) {
  stop(glue("Missing R/32 input: {RECONCILED_PATH}. ",
            "Run reconcile_projections() (R/32) first."), call. = FALSE)
}
reconciled <- readRDS(RECONCILED_PATH)

# DEF projections: load the R/34 cache if present, otherwise build it.
if (file.exists(DEF_PROJ_PATH)) {
  def_proj <- readRDS(DEF_PROJ_PATH)
} else {
  message(glue("DEF cache not found at {DEF_PROJ_PATH}; building with ",
               "project_def_st()."))
  def_proj <- project_def_st(season = SEASON, as_of_week = WEEK,
                             league_id = LEAGUE_ID)
}

# ------------------------------------------------------------------------------
# BUILD LEAGUE SOURCE + VORP + LINEUP (R/35)
# ------------------------------------------------------------------------------

src  <- build_league_source(league_id = LEAGUE_ID,
                            user_roster_id = USER_ROSTER_ID,
                            season = SEASON)
print(src)

vorp <- compute_vorp_rankings(reconciled, src$config)

# Matchup factors for the target week (shared by the optimizer and the waiver
# DEF comparison so both sit on the same adjusted scale).
dvp     <- compute_dvp_factors(seasons = PRIOR_SEASON)
defmtch <- compute_def_matchup_factors(seasons = PRIOR_SEASON)

lineup <- optimize_lineup(src, reconciled, vorp, def_proj,
                          week = WEEK, dvp = dvp, def_factors = defmtch)

cat("\n--- R/35 starting lineup (pre-UQ) ---\n")
print(lineup$starters)

# ------------------------------------------------------------------------------
# EXAMPLE A: offense-only enrichment (no waiver DEF comparison)
# ------------------------------------------------------------------------------
# Minimal call: only the lineup and the R/32 reconciled projections. DEF
# alternatives fall back to the bench (a one-line message says so).

uq_offense <- analyze_start_sit(lineup, reconciled, week = WEEK)

cat("\n--- Example A: offense-only UQ ---\n")
print(uq_offense$starters[, c("slot", "player_name", "adj_proj", "alt_player",
                              "p_start_correct", "avg_miss", "expected_regret",
                              "stakes")])
cat(glue("Week risk score (A): ",
         "{format(round(uq_offense$week_risk_score, 1), nsmall = 1)} pts\n"))

# ------------------------------------------------------------------------------
# EXAMPLE B: full enrichment, including the rostered-vs-waiver DEF check
# ------------------------------------------------------------------------------
# Passing source, vorp, and def_proj turns on the streaming comparison: the
# started DEF is scored against the best AVAILABLE defense, not just the bench.
# save_output = TRUE writes s2_week16_r36_start_sit_uq.{rds,csv}.

uq <- analyze_start_sit(lineup, reconciled,
                        source = src, vorp = vorp, def_proj = def_proj,
                        week = WEEK, dvp = dvp, def_factors = defmtch,
                        def_seasons = PRIOR_SEASON,
                        include_waiver_def = TRUE,
                        save_output = TRUE)

cat("\n--- Example B: full UQ (with waiver DEF streaming check) ---\n")
print(uq$starters[, c("slot", "player_name", "adj_proj", "alt_player",
                      "alt_source", "alt_adj_proj", "p_start_correct",
                      "avg_miss", "expected_regret", "stakes")])

cat("\n--- High-stakes slots (worth a second look) ---\n")
high <- uq$starters[!is.na(uq$starters$stakes) & uq$starters$stakes == "high", ]
if (nrow(high) == 0L) {
  cat("None. No started slot carries high decision risk this week.\n")
} else {
  print(high[, c("slot", "player_name", "alt_player", "p_start_correct",
                 "avg_miss", "expected_regret")])
}

# DEF read. Two cases:
#   (1) You roster a DEF that got started: compare it to the best alternative
#       (which may be a waiver pickup) right in the starters table.
#   (2) Your DEF slot is open (you stream): uq$def_streaming ranks the available
#       defenses to add.
def_row <- uq$starters[uq$starters$slot == "DEF", ]
if (nrow(def_row) >= 1L && !is.na(def_row$alt_source[1]) &&
    def_row$alt_source[1] == "waiver") {
  cat(glue("\nDEF (rostered): best alternative is a WAIVER defense ",
           "({def_row$alt_player[1]}). P(your DEF outscores it) = ",
           "{def_row$p_start_correct[1]}; ",
           "expected miss if you keep yours = {def_row$avg_miss[1]} pts.\n"))
}

if (!is.null(uq$def_streaming) && nrow(uq$def_streaming) > 0L) {
  cat("\n--- DEF streaming: best available defenses to add ---\n")
  print(uq$def_streaming[, c("rank", "def_team", "opponent", "adj_proj",
                             "p_vs_next", "pick_clarity")])
  top <- uq$def_streaming[1, ]
  opp_txt <- if (is.na(top$opponent)) "a neutral matchup" else
    glue("vs {top$opponent}")
  clarity_txt <- if (is.na(top$pick_clarity)) "the only option" else
    top$pick_clarity
  cat(glue("\nRecommended stream: {top$def_team} {opp_txt}, projected ",
           "{format(round(top$adj_proj, 1), nsmall = 1)} pts. ",
           "P(it beats the next option) = {top$p_vs_next} ({clarity_txt}).\n"))
}

cat(glue("\nWeek risk score (B): ",
         "{format(round(uq$week_risk_score, 1), nsmall = 1)} pts at risk\n"))
