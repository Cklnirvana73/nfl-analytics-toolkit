# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 5
# Example: Sleeper API Integration (Interactive)
# File: examples/example_season2_week5.R
# ==============================================================================
#
# PURPOSE
# -------
# Walk through all four major functions in R/19_sleeper_api.R using your
# actual Sleeper account. The script prompts for your Sleeper username at
# runtime -- no hardcoded IDs anywhere.
#
# WHAT THIS SCRIPT DOES
# ---------------------
#   Step 1  : Find your leagues via get_user_leagues()
#   Step 2  : Let you pick which league to analyze
#   Step 3  : Connect and inspect league metadata via connect_sleeper_league()
#   Step 4  : Pull rosters via get_sleeper_rosters()
#   Step 5  : Map scoring settings to calculate_fantasy_points_ext() params
#   Step 6  : Reconcile Sleeper player IDs to nflfastR GSIS IDs
#   Step 7  : (Optional) Pull a specific week's matchups
#
# HOW TO RUN
# ----------
#   1. Open this file in RStudio
#   2. Run the entire script (Ctrl+Shift+Enter) or step through section by section
#   3. You will be prompted for your Sleeper username in the console
#   4. Follow the numbered prompts
#
# REQUIREMENTS
# ------------
#   - R/19_sleeper_api.R must be sourced (done below)
#   - nflreadr must be installed: install.packages("nflreadr")
#   - An active internet connection for Sleeper API calls
#   - Your Sleeper username (visible at sleeper.app -- NOT your display name)
#
# NOTE ON RATE LIMITS
# -------------------
#   The Sleeper API is public and requires no API key. Rate limits are
#   approximately 1,000 requests/hour. This script makes fewer than 10 calls.
#
# ==============================================================================

library(dplyr)
library(here)
library(nflreadr)

source(here::here("R", "19_sleeper_api.R"))

cat("\n")
cat(strrep("=", 60), "\n")
cat("NFL Analytics Toolkit -- Season 2, Week 5\n")
cat("Sleeper API Integration -- Interactive Example\n")
cat(strrep("=", 60), "\n\n")

# ==============================================================================
# STEP 1: GET YOUR SLEEPER USERNAME
# ==============================================================================

cat("STEP 1: Identify your Sleeper account\n")
cat(strrep("-", 40), "\n")
cat("Your Sleeper username is NOT your display name.\n")
cat("Find it at: https://sleeper.app  (visible in your profile URL)\n\n")

sleeper_username <- readline(prompt = "Enter your Sleeper username: ")
sleeper_username <- trimws(sleeper_username)

if (nchar(sleeper_username) == 0L) {
  stop("Username cannot be empty. Re-run and enter your Sleeper username.",
       call. = FALSE)
}

cat(glue::glue("\nLooking up leagues for: {sleeper_username}\n\n"))

# ==============================================================================
# STEP 2: FIND YOUR LEAGUES
# ==============================================================================

cat("STEP 2: Fetch your leagues\n")
cat(strrep("-", 40), "\n")

my_leagues <- get_user_leagues(
  username = sleeper_username,
  season   = CURRENT_NFL_SEASON
)

if (is.null(my_leagues) || nrow(my_leagues) == 0L) {
  stop(glue::glue(
    "No leagues found for '{sleeper_username}' in the {CURRENT_NFL_SEASON} season.\n",
    "Check that the username is correct and that you were in leagues this season."
  ), call. = FALSE)
}

cat("\nYour leagues:\n\n")
print(my_leagues[, c("name", "league_id", "scoring_type", "is_superflex",
                     "total_rosters", "status")])
cat("\n")

# ==============================================================================
# STEP 3: PICK A LEAGUE
# ==============================================================================

cat("STEP 3: Select a league to analyze\n")
cat(strrep("-", 40), "\n")

if (nrow(my_leagues) == 1L) {
  cat("Only one league found -- selecting it automatically.\n")
  selected_idx <- 1L
} else {
  cat("Enter the row number of the league you want to analyze (1 to",
      nrow(my_leagues), "): ")
  selected_idx <- as.integer(readline(prompt = ""))

  if (is.na(selected_idx) ||
      selected_idx < 1L  ||
      selected_idx > nrow(my_leagues)) {
    stop(glue::glue(
      "Invalid selection: {selected_idx}. ",
      "Enter a number between 1 and {nrow(my_leagues)}."
    ), call. = FALSE)
  }
}

selected_league_id   <- my_leagues$league_id[selected_idx]
selected_league_name <- my_leagues$name[selected_idx]

cat(glue::glue(
  "\nSelected: {selected_league_name}\n",
  "League ID: {selected_league_id}\n\n"
))

# ==============================================================================
# STEP 4: CONNECT AND INSPECT LEAGUE METADATA
# ==============================================================================

cat("STEP 4: Connect to league and inspect metadata\n")
cat(strrep("-", 40), "\n")

league <- connect_sleeper_league(selected_league_id)

if (is.null(league)) {
  stop("Failed to connect to league. Check your internet connection and try again.",
       call. = FALSE)
}

cat(glue::glue(
  "\nLeague overview:\n",
  "  Name         : {league$name}\n",
  "  Season       : {league$season}\n",
  "  Status       : {league$status}\n",
  "  Teams        : {league$total_rosters}\n",
  "  Superflex    : {league$is_superflex}\n",
  "  Roster slots : {paste(league$roster_positions, collapse = ', ')}\n\n"
))

cat("Raw scoring fields from Sleeper (first 10):\n")
scoring_preview <- head(names(league$scoring_settings), 10L)
for (field in scoring_preview) {
  cat(glue::glue("  {field}: {league$scoring_settings[[field]]}\n"))
}
if (length(league$scoring_settings) > 10L) {
  cat(glue::glue("  ... and {length(league$scoring_settings) - 10L} more fields\n"))
}
cat("\n")

# ==============================================================================
# STEP 5: MAP SCORING SETTINGS TO FUNCTION PARAMETERS
# ==============================================================================

cat("STEP 5: Map Sleeper scoring to calculate_fantasy_points_ext() parameters\n")
cat(strrep("-", 40), "\n\n")

scoring_map <- map_sleeper_scoring(selected_league_id)

if (is.null(scoring_map)) {
  stop("Failed to map scoring settings.", call. = FALSE)
}

cat("\nMapped parameters:\n")
params <- scoring_map$params
param_names <- names(params)
for (p in param_names) {
  cat(glue::glue("  {p}: {params[[p]]}\n"))
}

cat("\nMapping log (mapped fields only):\n")
mapped_rows <- scoring_map$mapping_log %>%
  dplyr::filter(grepl("^mapped", status))
print(mapped_rows[, c("sleeper_field", "sleeper_value", "our_param", "our_value")])

if (length(scoring_map$warnings) > 0L) {
  cat("\nMapping warnings:\n")
  for (w in scoring_map$warnings) {
    cat(glue::glue("  [!] {w}\n"))
  }
}

cat("\nTo calculate fantasy points with your exact league settings:\n\n")
cat("  pbp    <- load_normalized_season(", league$season, ")\n")
cat("  roster <- nflreadr::load_rosters(seasons =", league$season, ")\n")
cat("  fp     <- do.call(\n")
cat("    calculate_fantasy_points_ext,\n")
cat("    c(list(pbp_data = pbp, roster_data = roster,\n")
cat("           season =", as.integer(league$season), "L),\n")
cat("      scoring_map$params)\n")
cat("  )\n\n")

# ==============================================================================
# STEP 6: PULL ROSTERS AND RECONCILE PLAYERS
# ==============================================================================

cat("STEP 6: Pull rosters and reconcile players to nflfastR\n")
cat(strrep("-", 40), "\n\n")

rosters <- get_sleeper_rosters(selected_league_id)

if (is.null(rosters)) {
  cat("[!] Could not pull rosters. Skipping player reconciliation.\n\n")
} else {

  cat(glue::glue(
    "Roster summary: {length(unique(rosters$roster_id))} teams, ",
    "{nrow(rosters)} total player-slots\n\n"
  ))

  cat("Starter breakdown by roster:\n")
  starter_summary <- rosters %>%
    dplyr::group_by(roster_id) %>%
    dplyr::summarise(
      total_players = dplyr::n(),
      starters      = sum(is_starter),
      bench         = sum(on_bench),
      reserve       = sum(is_reserve),
      .groups       = "drop"
    )
  print(starter_summary)
  cat("\n")

  # Reconcile players to nflfastR
  cat("Reconciling Sleeper player IDs to nflfastR GSIS IDs...\n")
  cat("(Loading nflreadr roster for season", league$season, ")\n\n")

  nfl_roster <- nflreadr::load_rosters(seasons = as.integer(league$season))

  player_ids <- unique(rosters$player_id)

  reconciled <- tryCatch(
    match_sleeper_players(
      sleeper_player_ids = player_ids,
      nflreadr_roster    = nfl_roster,
      force_refresh      = FALSE
    ),
    error = function(e) {
      cat(glue::glue("[!] Player reconciliation error: {e$message}\n\n"))
      return(NULL)
    }
  )

  if (!is.null(reconciled)) {

    match_rate <- attr(reconciled, "match_rate")

    cat(glue::glue(
      "\nOverall match rate: {round(match_rate * 100, 1)}% ",
      "({sum(reconciled$match_method != 'unmatched')}/",
      "{nrow(reconciled)} players)\n\n"
    ))

    # Match method breakdown
    method_summary <- reconciled %>%
      dplyr::count(match_method, match_confidence) %>%
      dplyr::arrange(dplyr::desc(n))
    cat("Match method breakdown:\n")
    print(method_summary)
    cat("\n")

    # Show any unmatched players
    unmatched <- reconciled %>%
      dplyr::filter(match_method == "unmatched") %>%
      dplyr::select(sleeper_player_id, sleeper_name, sleeper_position, sleeper_team)

    if (nrow(unmatched) > 0L) {
      cat("Unmatched players (may be retired, IR, or name variants):\n")
      print(unmatched)
    } else {
      cat("All players matched successfully.\n")
    }

    cat("\nSample of reconciled players:\n")
    sample_rows <- reconciled %>%
      dplyr::filter(match_method != "unmatched") %>%
      dplyr::select(sleeper_name, sleeper_position, sleeper_team,
                    gsis_id, match_method) %>%
      head(10L)
    print(sample_rows)
    cat("\n")
  }
}

# ==============================================================================
# STEP 7: (OPTIONAL) PULL A WEEK'S MATCHUPS
# ==============================================================================

cat("STEP 7: Pull matchup data for a specific week (optional)\n")
cat(strrep("-", 40), "\n")
cat("Enter a week number (1-18) to pull matchup data, or press Enter to skip: ")
week_input <- trimws(readline(prompt = ""))

if (nchar(week_input) > 0L) {
  week_num <- suppressWarnings(as.integer(week_input))

  if (is.na(week_num) || week_num < 1L || week_num > 18L) {
    cat("[!] Invalid week. Skipping matchup pull.\n\n")
  } else {
    matchups <- get_sleeper_matchups(selected_league_id, week = week_num)

    if (!is.null(matchups)) {
      cat(glue::glue(
        "\nWeek {week_num} matchup summary:\n",
        "  {length(unique(matchups$matchup_id))} matchups | ",
        "{nrow(matchups)} player-week rows\n\n"
      ))

      # Which teams scored the most?
      team_scores <- matchups %>%
        dplyr::distinct(matchup_id, roster_id, team_points) %>%
        dplyr::filter(!is.na(team_points)) %>%
        dplyr::arrange(dplyr::desc(team_points))

      cat("Team scores (Week", week_num, "):\n")
      print(team_scores)

      # Top individual scorers
      top_players <- matchups %>%
        dplyr::filter(!is.na(player_points), is_starter) %>%
        dplyr::arrange(dplyr::desc(player_points)) %>%
        dplyr::select(roster_id, player_id, player_points, is_starter) %>%
        head(10L)

      cat(glue::glue("\nTop 10 starters by points (Week {week_num}):\n"))
      print(top_players)
      cat("\n")

      cat("Note: player_id values above are Sleeper IDs. Use reconciled$gsis_id\n")
      cat("from Step 6 to join these back to nflfastR player names.\n\n")
    }
  }
} else {
  cat("Skipped matchup pull.\n\n")
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("Example complete.\n\n")
cat("Key objects available in your session:\n")
cat("  my_leagues   -- all your Sleeper leagues\n")
cat("  league       -- metadata for selected league\n")
cat("  scoring_map  -- scoring params for calculate_fantasy_points_ext()\n")
if (exists("rosters"))    cat("  rosters      -- all rostered players\n")
if (exists("reconciled") && !is.null(reconciled))
  cat("  reconciled   -- Sleeper player_id <-> nflfastR gsis_id crosswalk\n")
if (exists("matchups") && !is.null(matchups))
  cat("  matchups     -- weekly matchup data\n")

cat("\nNext step: calculate fantasy points with your league settings:\n\n")
cat("  pbp    <- load_normalized_season(", league$season, ")\n")
cat("  roster <- nflreadr::load_rosters(seasons =", league$season, ")\n")
cat("  fp     <- do.call(\n")
cat("    calculate_fantasy_points_ext,\n")
cat("    c(list(pbp_data = pbp, roster_data = roster,\n")
cat("           season =", as.integer(league$season), "L),\n")
cat("      scoring_map$params)\n")
cat("  )\n\n")
cat(strrep("=", 60), "\n")
