# ==============================================================================
# WEEK 6: USAGE & OPPORTUNITY ENGINEERING
# ==============================================================================
#
# Production-grade usage and opportunity metrics for NFL player evaluation.
# Built for personal analytics use and portfolio display.
#
# Usage metrics come BEFORE efficiency adjustments because opportunity drives
# production. A player cannot score if they do not touch the ball.
#
# This file contains four main functions:
# 1. calculate_usage_metrics()       - Lines  60-330
#    Target share, rush share, air yards share, red zone opportunity rate.
# 2. calculate_usage_trends()        - Lines 332-560
#    Rolling usage changes: expanding/shrinking role detection.
# 3. get_redzone_profile()           - Lines 562-780
#    Red zone targets/rushes/TDs inside 20, 10, and 5-yard lines.
# 4. get_receiving_depth_profile()   - Lines 782-1000
#    Short/medium/deep target distribution per receiver.
#
# Dependencies:
#   Existing: dplyr, glue, zoo, here
#   New this week: (none -- all dependencies already present)
#
# Builds on:
#   Week 1: load_and_validate_pbp() -> pbp tibble
#   Week 2: get_player_rushing_stats(), get_player_receiving_stats()
#   Week 5: calculate_epa_trends(), get_situational_splits() patterns
#
# NFL Context:
#   - All usage metrics are RATES (share of team total), not raw counts
#   - Team-level denominators required for all share calculations
#   - Multi-team players (traded mid-season) grouped by team-week
#   - Target share = single strongest predictor of WR fantasy output
#   - Red zone usage disproportionately valuable (TD probability spikes)
#   - Air yards share captures opportunity quality, not just volume
#   - Snap count data not available in nflfastR -- documented limitation
#
# nflfastR column reference (verified):
#   epa               -- Expected Points Added per play
#   wp                -- Win probability (0 to 1)
#   play_type         -- "pass", "run", "qb_kneel", "qb_spike", etc.
#   posteam           -- Possessing (offensive) team abbreviation
#   down              -- Current down (1-4)
#   ydstogo           -- Yards needed for first down
#   yardline_100      -- Yards from opponent end zone (0 = end zone, 100 = own end zone)
#   passer_player_id  -- QB GSIS ID on pass plays
#   rusher_player_id  -- Ball carrier GSIS ID on run plays
#   receiver_player_id-- Target GSIS ID on pass plays
#   passer_player_name  -- QB name
#   rusher_player_name  -- Ball carrier name
#   receiver_player_name-- Target name
#   air_yards         -- Distance ball travels in air on pass plays
#   yards_after_catch -- YAC on completed passes
#   complete_pass     -- 1 if completion, 0 otherwise
#   touchdown         -- 1 if TD scored on this play
#   qb_kneel          -- 1 if QB kneel play
#   qb_spike          -- 1 if spike play
#   qb_scramble       -- 1 if QB scramble
#   season, week, game_id, game_date
#
# ==============================================================================

library(dplyr)
library(glue)
library(zoo)
library(tidyr)
library(here)


# ==============================================================================
# SECTION 1: CALCULATE USAGE METRICS
# ==============================================================================

#' Calculate Player Usage and Opportunity Metrics
#'
#' @description
#' Computes per-player, per-week opportunity metrics as shares of team totals.
#' All metrics are rates (not raw counts) to enable cross-team and cross-game
#' comparisons. This is the foundational usage layer -- efficiency adjustments
#' (game script, opponent) are applied in later weeks.
#'
#' Target share is computed as receiver targets / team pass attempts (including
#' incompletions). Air yards share uses total air yards on all targets, not
#' just completions, to capture true opportunity quality.
#'
#' Multi-team players are handled by team-week grouping. A player traded from
#' KC to BUF in Week 9 will have separate rows for each team's games.
#'
#' Limitation: Snap count data is not available in nflfastR play-by-play.
#' Snap share is a known gap -- document when this matters analytically.
#'
#' @param pbp_data Play-by-play tibble from load_and_validate_pbp().
#'   Must contain: play_type, posteam, season, week, game_id, game_date,
#'   passer_player_id, rusher_player_id, receiver_player_id,
#'   passer_player_name, rusher_player_name, receiver_player_name,
#'   air_yards, complete_pass, touchdown, qb_kneel, qb_spike, qb_scramble,
#'   down, yardline_100, epa
#' @param season Optional numeric scalar or vector. Filter to specific season(s).
#'   If NULL, all seasons in data are used.
#' @param week_min Optional numeric. Minimum week (inclusive). Default: 1.
#' @param week_max Optional numeric. Maximum week (inclusive). Default: 22.
#' @param min_team_plays Minimum team plays in a game-week to include that
#'   team-week in the denominator. Guards against incomplete data.
#'   Default: 20.
#'
#' @return Tibble with one row per player-team-week (one row per team if
#'   player was traded mid-season). Columns:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{week}{Week number (int)}
#'     \item{game_id}{Game identifier (chr)}
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Player name (chr)}
#'     \item{position_group}{Derived position group: "passer", "rusher", "receiver" (chr)}
#'     \item{team}{Team abbreviation for this game-week (chr)}
#'     \item{targets}{Targets received (int, receivers only)}
#'     \item{team_targets}{Total team targets (int)}
#'     \item{target_share}{targets / team_targets (dbl, NA if team_targets == 0)}
#'     \item{rushes}{Rush attempts (int, rushers only)}
#'     \item{team_rushes}{Total team rushes (int)}
#'     \item{rush_share}{rushes / team_rushes (dbl, NA if team_rushes == 0)}
#'     \item{air_yards}{Total air yards on targets (dbl, receivers/passers)}
#'     \item{team_air_yards}{Total team air yards (dbl)}
#'     \item{air_yards_share}{air_yards / team_air_yards (dbl, NA if team_air_yards == 0)}
#'     \item{redzone_targets}{Targets inside opponent 20-yard line (int)}
#'     \item{redzone_rushes}{Rush attempts inside opponent 20-yard line (int)}
#'     \item{redzone_opportunities}{redzone_targets + redzone_rushes (int)}
#'     \item{team_redzone_plays}{Total team red zone plays (int)}
#'     \item{redzone_share}{redzone_opportunities / team_redzone_plays (dbl)}
#'   }
#'
#' @details
#' Position group assignment:
#' - "passer": passer_player_id identified (QB, including scrambles excluded)
#' - "receiver": receiver_player_id identified (WR/TE/RB targeted on pass)
#' - "rusher": rusher_player_id identified, qb_scramble == 0 (RB/FB rushes)
#' A player may appear in multiple groups (RB targeted as receiver AND as rusher).
#'
#' QB kneels and spikes are excluded from all denominators.
#' QB scrambles are excluded from rusher analysis (treated as pass plays).
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' usage <- calculate_usage_metrics(pbp, season = 2025)
#' names(usage)
#'
#' # Top target shares, Week 9
#' usage %>%
#'   filter(week == 9, position_group == "receiver") %>%
#'   arrange(desc(target_share)) %>%
#'   select(player_name, team, targets, team_targets, target_share) %>%
#'   head(10)
#' }
#'
#' @seealso [calculate_usage_trends()] for rolling usage changes,
#'   [get_redzone_profile()] for detailed red zone breakdown,
#'   [get_receiving_depth_profile()] for target depth distribution
#'
#' @export
calculate_usage_metrics <- function(pbp_data,
                                    season    = NULL,
                                    week_min  = 1,
                                    week_max  = 22,
                                    min_team_plays = 20) {

  # --- Input validation ---
  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame or tibble.")
  }

  required_cols <- c(
    "play_type", "posteam", "season", "week", "game_id",
    "passer_player_id", "rusher_player_id", "receiver_player_id",
    "passer_player_name", "rusher_player_name", "receiver_player_name",
    "air_yards", "complete_pass", "touchdown",
    "qb_kneel", "qb_spike", "qb_scramble",
    "down", "yardline_100", "epa"
  )
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  if (!is.null(season) && !is.numeric(season)) {
    stop("season must be numeric or NULL.")
  }
  if (!is.numeric(week_min) || !is.numeric(week_max)) {
    stop("week_min and week_max must be numeric.")
  }
  if (week_min > week_max) {
    stop(glue("week_min ({week_min}) cannot exceed week_max ({week_max})."))
  }
  if (!is.numeric(min_team_plays) || min_team_plays < 1) {
    stop("min_team_plays must be a positive integer.")
  }

  if (nrow(pbp_data) == 0) {
    message("pbp_data is empty. Returning empty usage metrics tibble.")
    return(tibble(
      season = integer(), week = integer(), game_id = character(),
      player_id = character(), player_name = character(),
      position_group = character(), team = character(),
      targets = integer(), team_targets = integer(), target_share = double(),
      rushes = integer(), team_rushes = integer(), rush_share = double(),
      air_yards = double(), team_air_yards = double(), air_yards_share = double(),
      redzone_targets = integer(), redzone_rushes = integer(),
      redzone_opportunities = integer(), team_redzone_plays = integer(),
      redzone_share = double()
    ))
  }

  # --- Filter to scope ---
  if (!is.null(season)) {
    pbp_data <- pbp_data %>% filter(season %in% !!season)
  }

  pbp_data <- pbp_data %>%
    filter(week >= week_min, week <= week_max)

  if (nrow(pbp_data) == 0) {
    message(glue("No plays found for specified season/week range. Returning empty tibble."))
    return(tibble(
      season = integer(), week = integer(), game_id = character(),
      player_id = character(), player_name = character(),
      position_group = character(), team = character(),
      targets = integer(), team_targets = integer(), target_share = double(),
      rushes = integer(), team_rushes = integer(), rush_share = double(),
      air_yards = double(), team_air_yards = double(), air_yards_share = double(),
      redzone_targets = integer(), redzone_rushes = integer(),
      redzone_opportunities = integer(), team_redzone_plays = integer(),
      redzone_share = double()
    ))
  }

  # --- Clean plays: exclude non-meaningful plays ---
  # QB kneels and spikes excluded from all denominators
  pbp_clean <- pbp_data %>%
    filter(
      !is.na(posteam),
      play_type %in% c("pass", "run"),
      qb_kneel == 0,
      qb_spike == 0
    )

  message(glue("Processing usage metrics: {nrow(pbp_clean)} clean plays across ",
               "{n_distinct(pbp_clean$game_id)} games"))

  # --- Team-level denominators per game-week ---
  # These are the denominators for all share calculations

  team_pass_denominator <- pbp_clean %>%
    filter(play_type == "pass", !is.na(posteam)) %>%
    group_by(season, week, game_id, posteam) %>%
    summarise(
      team_targets   = n(),
      team_air_yards = sum(air_yards, na.rm = TRUE),
      .groups = "drop"
    )

  team_rush_denominator <- pbp_clean %>%
    filter(play_type == "run", qb_scramble == 0) %>%
    group_by(season, week, game_id, posteam) %>%
    summarise(
      team_rushes = n(),
      .groups = "drop"
    )

  team_redzone_denominator <- pbp_clean %>%
    filter(!is.na(yardline_100), yardline_100 <= 20) %>%
    group_by(season, week, game_id, posteam) %>%
    summarise(
      team_redzone_plays = n(),
      .groups = "drop"
    )

  # --- Receiver stats per game-week ---
  receiver_stats <- pbp_clean %>%
    filter(
      play_type == "pass",
      !is.na(receiver_player_id),
      !is.na(receiver_player_name)
    ) %>%
    group_by(season, week, game_id, posteam,
             player_id = receiver_player_id,
             player_name = receiver_player_name) %>%
    summarise(
      targets         = n(),
      air_yards       = sum(air_yards, na.rm = TRUE),
      redzone_targets = sum(!is.na(yardline_100) & yardline_100 <= 20, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(position_group = "receiver")

  # --- Rusher stats per game-week ---
  # Exclude QB scrambles -- those are pass plays attributed to the QB
  rusher_stats <- pbp_clean %>%
    filter(
      play_type == "run",
      qb_scramble == 0,
      !is.na(rusher_player_id),
      !is.na(rusher_player_name)
    ) %>%
    group_by(season, week, game_id, posteam,
             player_id = rusher_player_id,
             player_name = rusher_player_name) %>%
    summarise(
      rushes         = n(),
      redzone_rushes = sum(!is.na(yardline_100) & yardline_100 <= 20, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(position_group = "rusher")

  # --- Join denominators and compute shares ---

  # Receiver shares
  receiver_with_shares <- receiver_stats %>%
    left_join(team_pass_denominator,
              by = c("season", "week", "game_id", "posteam")) %>%
    left_join(team_rush_denominator,
              by = c("season", "week", "game_id", "posteam")) %>%
    left_join(team_redzone_denominator,
              by = c("season", "week", "game_id", "posteam")) %>%
    mutate(
      team           = posteam,
      rushes         = 0L,
      redzone_rushes = 0L,
      # Division guards: NA when denominator is 0 or missing
      target_share   = ifelse(!is.na(team_targets) & team_targets > 0,
                              targets / team_targets, NA_real_),
      rush_share     = NA_real_,
      air_yards_share = ifelse(!is.na(team_air_yards) & team_air_yards > 0,
                               air_yards / team_air_yards, NA_real_),
      redzone_opportunities = redzone_targets + redzone_rushes,
      redzone_share  = ifelse(!is.na(team_redzone_plays) & team_redzone_plays > 0,
                              redzone_opportunities / team_redzone_plays, NA_real_),
      team_rushes    = coalesce(team_rushes, 0L)
    ) %>%
    select(season, week, game_id, player_id, player_name, position_group, team,
           targets, team_targets, target_share,
           rushes, team_rushes, rush_share,
           air_yards, team_air_yards, air_yards_share,
           redzone_targets, redzone_rushes, redzone_opportunities,
           team_redzone_plays, redzone_share)

  # Rusher shares
  rusher_with_shares <- rusher_stats %>%
    left_join(team_pass_denominator,
              by = c("season", "week", "game_id", "posteam")) %>%
    left_join(team_rush_denominator,
              by = c("season", "week", "game_id", "posteam")) %>%
    left_join(team_redzone_denominator,
              by = c("season", "week", "game_id", "posteam")) %>%
    mutate(
      team            = posteam,
      targets         = 0L,
      redzone_targets = 0L,
      air_yards       = 0.0,
      target_share    = NA_real_,
      rush_share      = ifelse(!is.na(team_rushes) & team_rushes > 0,
                               rushes / team_rushes, NA_real_),
      air_yards_share = NA_real_,
      team_targets    = coalesce(team_targets, 0L),
      team_air_yards  = coalesce(team_air_yards, 0.0),
      redzone_opportunities = redzone_targets + redzone_rushes,
      redzone_share   = ifelse(!is.na(team_redzone_plays) & team_redzone_plays > 0,
                               redzone_opportunities / team_redzone_plays, NA_real_)
    ) %>%
    select(season, week, game_id, player_id, player_name, position_group, team,
           targets, team_targets, target_share,
           rushes, team_rushes, rush_share,
           air_yards, team_air_yards, air_yards_share,
           redzone_targets, redzone_rushes, redzone_opportunities,
           team_redzone_plays, redzone_share)

  # Combine
  usage_metrics <- bind_rows(receiver_with_shares, rusher_with_shares) %>%
    arrange(season, week, team, player_name, position_group)

  # Filter out team-weeks with insufficient plays (data quality guard)
  games_before <- n_distinct(paste(usage_metrics$game_id, usage_metrics$team))
  usage_metrics <- usage_metrics %>%
    filter(
      (position_group == "receiver" & coalesce(team_targets, 0L) >= min_team_plays) |
      (position_group == "rusher"   & coalesce(team_rushes,  0L) >= (min_team_plays / 2))
    )
  games_after <- n_distinct(paste(usage_metrics$game_id, usage_metrics$team))

  if (games_before > games_after) {
    message(glue("Filtered {games_before - games_after} team-games with fewer than ",
                 "{min_team_plays} plays (incomplete data guard)."))
  }

  n_players   <- n_distinct(usage_metrics$player_id)
  n_game_weeks <- n_distinct(paste(usage_metrics$season, usage_metrics$week,
                                   usage_metrics$game_id))
  message(glue("Usage metrics complete: {n_players} players across ",
               "{n_game_weeks} game-weeks"))

  return(usage_metrics)
}


# ==============================================================================
# SECTION 2: CALCULATE USAGE TRENDS
# ==============================================================================

#' Calculate Rolling Usage Trend Features
#'
#' @description
#' Computes rolling changes in usage metrics to identify expanding or shrinking
#' roles. Uses lagged data to prevent feature leakage -- the current week's
#' usage is not included in the trend calculation.
#'
#' A player with increasing target share over the past 4 weeks is a different
#' asset than one with decreasing share, even if their current share is identical.
#' This captures role trajectory for predictive modeling (Weeks 9-12).
#'
#' @param usage_data Tibble from calculate_usage_metrics(). Must contain:
#'   season, week, game_id, player_id, player_name, position_group, team,
#'   target_share, rush_share, air_yards_share
#' @param trend_windows Numeric vector of window sizes for rolling calculations.
#'   Default: c(3, 4). Minimum 3 required for meaningful trend.
#' @param min_games Minimum non-NA observations required in window.
#'   Default: 2.
#'
#' @return Tibble with one row per player-position_group-team-week. Columns:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{week}{Week number (int)}
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Player name (chr)}
#'     \item{position_group}{"receiver" or "rusher" (chr)}
#'     \item{team}{Current team (chr)}
#'     \item{target_share}{Current week target share (dbl)}
#'     \item{rush_share}{Current week rush share (dbl)}
#'     \item{air_yards_share}{Current week air yards share (dbl)}
#'     \item{target_share_roll3}{Rolling mean target share, 3-week lagged window (dbl)}
#'     \item{target_share_roll4}{Rolling mean target share, 4-week lagged window (dbl)}
#'     \item{rush_share_roll3}{Rolling mean rush share, 3-week lagged window (dbl)}
#'     \item{rush_share_roll4}{Rolling mean rush share, 4-week lagged window (dbl)}
#'     \item{target_share_delta1}{Week-over-week change in target share (dbl)}
#'     \item{rush_share_delta1}{Week-over-week change in rush share (dbl)}
#'     \item{air_yards_share_delta1}{Week-over-week change in air yards share (dbl)}
#'     \item{role_trend}{"expanding", "stable", or "shrinking" (chr)}
#'   }
#'
#' @details
#' Feature leakage prevention: All rolling windows use lag(1) before the
#' window, so roll3 for Week 10 = mean of Weeks 7-9 (not 8-10).
#'
#' role_trend classification (based on 4-week rolling average vs prior 4 weeks):
#' - "expanding": latest 4-week avg > prior 4-week avg by more than 0.02 (2 pct points)
#' - "shrinking": latest 4-week avg < prior 4-week avg by more than 0.02
#' - "stable": within 0.02 threshold in either direction
#' Role trend uses target_share for receivers and rush_share for rushers.
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' usage <- calculate_usage_metrics(pbp, season = 2025)
#' trends <- calculate_usage_trends(usage)
#'
#' # Players with expanding receiver roles
#' trends %>%
#'   filter(position_group == "receiver", role_trend == "expanding") %>%
#'   arrange(desc(target_share)) %>%
#'   select(player_name, team, week, target_share, target_share_roll4, role_trend)
#' }
#'
#' @seealso [calculate_usage_metrics()] for the underlying usage data,
#'   [calculate_epa_trends()] (Week 5) for EPA trajectory features
#'
#' @export
calculate_usage_trends <- function(usage_data,
                                   trend_windows = c(3, 4),
                                   min_games     = 2) {

  # --- Input validation ---
  if (!is.data.frame(usage_data)) {
    stop("usage_data must be a data frame or tibble.")
  }

  required_cols <- c("season", "week", "player_id", "player_name",
                     "position_group", "team",
                     "target_share", "rush_share", "air_yards_share")
  missing_cols <- setdiff(required_cols, names(usage_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  if (!is.numeric(trend_windows) || length(trend_windows) < 1) {
    stop("trend_windows must be a non-empty numeric vector.")
  }
  if (any(trend_windows < 3)) {
    stop("All trend windows must be >= 3 for meaningful rolling averages.")
  }
  if (!is.numeric(min_games) || min_games < 1) {
    stop("min_games must be a positive integer.")
  }

  if (nrow(usage_data) == 0) {
    message("usage_data is empty. Returning empty trends tibble.")
    return(tibble())
  }

  # --- Sort chronologically before any lag/roll operations ---
  # Include team in sort: rolling windows must reset on team change.
  usage_sorted <- usage_data %>%
    arrange(season, week, player_id, position_group, team)

  # --- Build rolling features per player-position_group-team ---
  # Group by player + position_group to handle receivers who also rush

  roll_fn <- function(x, window, min_obs) {
    # Apply lag(1) first to prevent current week from entering the window
    x_lagged <- lag(x, 1)
    zoo::rollapply(x_lagged, width = window, FUN = mean,
                   fill = NA, align = "right", na.rm = TRUE,
                   partial = FALSE)
  }

  # For partial windows with min_games requirement
  roll_partial_fn <- function(x, window, min_obs) {
    x_lagged <- lag(x, 1)
    zoo::rollapply(x_lagged, width = window, FUN = function(vals) {
      non_na <- vals[!is.na(vals)]
      if (length(non_na) >= min_obs) mean(non_na) else NA_real_
    }, fill = NA, align = "right")
  }

  result <- usage_sorted %>%
    # Group by player + position_group + team: rolling window resets on team
    # change. A player traded mid-season has two independent rolling windows.
    # This prevents cross-team contamination in target_share_roll* features.
    group_by(player_id, position_group, team) %>%
    arrange(season, week, .by_group = TRUE) %>%
    mutate(
      # Rolling means (lagged -- no leakage)
      target_share_roll3 = roll_partial_fn(target_share,   3, min_games),
      target_share_roll4 = roll_partial_fn(target_share,   4, min_games),
      rush_share_roll3   = roll_partial_fn(rush_share,     3, min_games),
      rush_share_roll4   = roll_partial_fn(rush_share,     4, min_games),

      # Week-over-week delta (current minus lagged-1)
      target_share_delta1    = target_share    - lag(target_share, 1),
      rush_share_delta1      = rush_share      - lag(rush_share, 1),
      air_yards_share_delta1 = air_yards_share - lag(air_yards_share, 1),

      # Role trend: compare most recent 4-week rolling avg vs prior 4-week avg
      # Uses position-appropriate share metric
      role_primary_share = case_when(
        position_group == "receiver" ~ target_share,
        position_group == "rusher"   ~ rush_share,
        TRUE                         ~ NA_real_
      ),
      # Prior 4-week average: roll4 already uses lag(1), so this is weeks -(5:2)
      # We approximate prior period with lag(4) of the 4-week rolling avg
      prior_roll4 = lag(roll_partial_fn(role_primary_share, 4, min_games), 4),
      current_roll4 = roll_partial_fn(role_primary_share, 4, min_games),
      role_trend = case_when(
        is.na(current_roll4) | is.na(prior_roll4) ~ NA_character_,
        current_roll4 - prior_roll4 >  0.02       ~ "expanding",
        current_roll4 - prior_roll4 < -0.02       ~ "shrinking",
        TRUE                                       ~ "stable"
      )
    ) %>%
    ungroup() %>%
    select(season, week, player_id, player_name, position_group, team,
           target_share, rush_share, air_yards_share,
           target_share_roll3, target_share_roll4,
           rush_share_roll3,   rush_share_roll4,
           target_share_delta1, rush_share_delta1, air_yards_share_delta1,
           role_trend)

  n_expanding <- sum(result$role_trend == "expanding", na.rm = TRUE)
  n_shrinking <- sum(result$role_trend == "shrinking", na.rm = TRUE)
  message(glue("Usage trends complete: {n_expanding} expanding, ",
               "{n_shrinking} shrinking player-weeks identified"))

  return(result)
}


# ==============================================================================
# SECTION 3: RED ZONE PROFILE
# ==============================================================================

#' Get Player Red Zone Profile
#'
#' @description
#' Computes red zone opportunity and efficiency metrics at three distance
#' thresholds: inside the 20, inside the 10, and inside the 5. Red zone
#' usage is disproportionately valuable in fantasy football because TD
#' probability increases sharply as distance to the goal decreases.
#'
#' This function provides both volume (opportunities, share) and efficiency
#' (TD conversion rate, EPA) metrics to distinguish volume red zone users
#' from efficient converters.
#'
#' @param pbp_data Play-by-play tibble from load_and_validate_pbp().
#' @param season Optional numeric scalar or vector. Filter to specific season(s).
#' @param week_min Optional numeric. Minimum week (inclusive). Default: 1.
#' @param week_max Optional numeric. Maximum week (inclusive). Default: 22.
#' @param min_redzone_opps Minimum total red zone opportunities (inside 20)
#'   for player to be included. Default: 5.
#'
#' @return Tibble with one row per player-team-season. Columns:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Player name (chr)}
#'     \item{position_group}{"receiver" or "rusher" (chr)}
#'     \item{team}{Most recent team (chr)}
#'     \item{games_played}{Distinct game-weeks with red zone opportunity (int)}
#'     \item{rz20_targets}{Targets inside opponent 20-yard line (int)}
#'     \item{rz20_rushes}{Rushes inside opponent 20-yard line (int)}
#'     \item{rz20_tds}{Touchdowns inside 20 (int)}
#'     \item{rz20_td_rate}{TDs per opportunity inside 20 (dbl)}
#'     \item{rz20_epa}{Total EPA inside 20 (dbl)}
#'     \item{rz10_targets}{Targets inside opponent 10-yard line (int)}
#'     \item{rz10_rushes}{Rushes inside opponent 10-yard line (int)}
#'     \item{rz10_tds}{Touchdowns inside 10 (int)}
#'     \item{rz10_td_rate}{TDs per opportunity inside 10 (dbl)}
#'     \item{rz5_targets}{Targets inside opponent 5-yard line (int)}
#'     \item{rz5_rushes}{Rushes inside opponent 5-yard line (int)}
#'     \item{rz5_tds}{Touchdowns inside 5 (int)}
#'     \item{rz5_td_rate}{TDs per opportunity inside 5 (dbl)}
#'   }
#'
#' @details
#' yardline_100 is the nflfastR column representing yards from the opponent's
#' end zone. yardline_100 = 20 means the ball is on the offense's 20 (wrong --
#' it means 20 yards from the opponent's end zone, i.e., inside the red zone).
#' yardline_100 = 5 means 5 yards from the end zone.
#'
#' TD rate uses total opportunities (targets + rushes) as denominator.
#' Minimum 5 opportunities enforced to avoid noise from single red zone plays.
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' rz <- get_redzone_profile(pbp, season = 2025, min_redzone_opps = 5)
#'
#' # Top red zone receivers inside the 10
#' rz %>%
#'   filter(position_group == "receiver") %>%
#'   arrange(desc(rz10_targets)) %>%
#'   select(player_name, team, rz10_targets, rz10_tds, rz10_td_rate) %>%
#'   head(10)
#' }
#'
#' @seealso [calculate_usage_metrics()] for game-week level red zone share,
#'   [get_receiving_depth_profile()] for full-field depth distribution
#'
#' @export
get_redzone_profile <- function(pbp_data,
                                season          = NULL,
                                week_min        = 1,
                                week_max        = 22,
                                min_redzone_opps = 5) {

  # --- Input validation ---
  if (!is.data.frame(pbp_data)) stop("pbp_data must be a data frame or tibble.")

  required_cols <- c(
    "play_type", "posteam", "season", "week", "game_id",
    "receiver_player_id", "receiver_player_name",
    "rusher_player_id", "rusher_player_name",
    "yardline_100", "touchdown", "epa",
    "qb_kneel", "qb_spike", "qb_scramble"
  )
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  if (!is.null(season) && !is.numeric(season)) stop("season must be numeric or NULL.")
  if (!is.numeric(min_redzone_opps) || min_redzone_opps < 1) {
    stop("min_redzone_opps must be a positive integer.")
  }

  if (nrow(pbp_data) == 0) {
    message("pbp_data is empty. Returning empty red zone profile tibble.")
    return(tibble())
  }

  # --- Filter ---
  if (!is.null(season)) pbp_data <- pbp_data %>% filter(season %in% !!season)
  pbp_data <- pbp_data %>% filter(week >= week_min, week <= week_max)

  # Clean plays
  pbp_clean <- pbp_data %>%
    filter(
      !is.na(posteam),
      play_type %in% c("pass", "run"),
      qb_kneel == 0,
      qb_spike == 0,
      !is.na(yardline_100)
    )

  message(glue("Computing red zone profile from {nrow(pbp_clean)} clean plays"))

  # --- Helper to build red zone stats at given threshold ---
  rz_player_stats <- function(data, threshold, prefix_recv, prefix_rush) {
    # Receivers
    recv <- data %>%
      filter(play_type == "pass",
             !is.na(receiver_player_id),
             yardline_100 <= threshold) %>%
      group_by(season,
               player_id = receiver_player_id,
               player_name = receiver_player_name,
               team = posteam) %>%
      summarise(
        !!paste0(prefix_recv, "_targets") := n(),
        !!paste0(prefix_recv, "_tds")     := sum(touchdown == 1, na.rm = TRUE),
        !!paste0(prefix_recv, "_epa")     := sum(epa, na.rm = TRUE),
        games_played = n_distinct(game_id),
        .groups = "drop"
      ) %>%
      mutate(position_group = "receiver",
             !!paste0(prefix_recv, "_rushes") := 0L)

    # Rushers (exclude QB scrambles)
    rush <- data %>%
      filter(play_type == "run",
             qb_scramble == 0,
             !is.na(rusher_player_id),
             yardline_100 <= threshold) %>%
      group_by(season,
               player_id = rusher_player_id,
               player_name = rusher_player_name,
               team = posteam) %>%
      summarise(
        !!paste0(prefix_rush, "_rushes") := n(),
        !!paste0(prefix_rush, "_tds")    := sum(touchdown == 1, na.rm = TRUE),
        games_played = n_distinct(game_id),
        .groups = "drop"
      ) %>%
      mutate(position_group = "rusher",
             !!paste0(prefix_rush, "_targets") := 0L,
             !!paste0(prefix_rush, "_epa")     := NA_real_)

    bind_rows(recv, rush)
  }

  rz20 <- rz_player_stats(pbp_clean, 20, "rz20", "rz20")
  rz10 <- rz_player_stats(pbp_clean, 10, "rz10", "rz10")
  rz5  <- rz_player_stats(pbp_clean,  5, "rz5",  "rz5")

  # Join all thresholds
  rz_profile <- rz20 %>%
    left_join(rz10 %>% select(season, player_id, position_group,
                               rz10_targets, rz10_rushes, rz10_tds),
              by = c("season", "player_id", "position_group")) %>%
    left_join(rz5 %>% select(season, player_id, position_group,
                              rz5_targets, rz5_rushes, rz5_tds),
              by = c("season", "player_id", "position_group")) %>%
    mutate(
      # Fill NAs with 0 for players with rz20 ops but none at rz10/rz5
      across(c(rz10_targets, rz10_rushes, rz10_tds,
               rz5_targets, rz5_rushes, rz5_tds),
             ~ replace_na(.x, 0L)),
      # Total opportunities per threshold
      rz20_opps = rz20_targets + rz20_rushes,
      rz10_opps = rz10_targets + rz10_rushes,
      rz5_opps  = rz5_targets  + rz5_rushes,
      # TD rate per opportunity -- division guards
      rz20_td_rate = ifelse(rz20_opps > 0, rz20_tds / rz20_opps, NA_real_),
      rz10_td_rate = ifelse(rz10_opps > 0, rz10_tds / rz10_opps, NA_real_),
      rz5_td_rate  = ifelse(rz5_opps  > 0, rz5_tds  / rz5_opps,  NA_real_)
    ) %>%
    filter(rz20_opps >= min_redzone_opps) %>%
    select(season, player_id, player_name, position_group, team, games_played,
           rz20_targets, rz20_rushes, rz20_tds, rz20_td_rate, rz20_epa,
           rz10_targets, rz10_rushes, rz10_tds, rz10_td_rate,
           rz5_targets,  rz5_rushes,  rz5_tds,  rz5_td_rate) %>%
    arrange(position_group, desc(rz20_targets + rz20_rushes))

  message(glue("Red zone profile: {nrow(rz_profile)} players with >= {min_redzone_opps} ",
               "red zone opportunities"))

  return(rz_profile)
}


# ==============================================================================
# SECTION 4: RECEIVING DEPTH PROFILE
# ==============================================================================

#' Get Receiver Target Depth Profile
#'
#' @description
#' Characterizes each receiver's target distribution by air yards depth:
#' short (<10 air yards), medium (10-19 air yards), and deep (20+ air yards).
#' Distinguishes slot receivers (short-heavy) from intermediate route runners
#' and deep threats. Air yards on targets (not just completions) capture the
#' intended depth of the route, not the actual result.
#'
#' @param pbp_data Play-by-play tibble from load_and_validate_pbp().
#' @param season Optional numeric scalar or vector.
#' @param week_min Optional numeric. Default: 1.
#' @param week_max Optional numeric. Default: 22.
#' @param min_targets Minimum total targets for player inclusion. Default: 20.
#'   Below 20 targets, depth profile percentages are too noisy to interpret.
#'
#' @return Tibble with one row per player-team-season. Columns:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Player name (chr)}
#'     \item{team}{Most recent team (chr)}
#'     \item{total_targets}{Total targets in date range (int)}
#'     \item{avg_air_yards}{Mean air yards per target (dbl)}
#'     \item{short_targets}{Targets with air_yards < 10 (int)}
#'     \item{medium_targets}{Targets with air_yards 10-19 (int)}
#'     \item{deep_targets}{Targets with air_yards >= 20 (int)}
#'     \item{short_pct}{short_targets / total_targets (dbl)}
#'     \item{medium_pct}{medium_targets / total_targets (dbl)}
#'     \item{deep_pct}{deep_targets / total_targets (dbl)}
#'     \item{short_catch_rate}{Completions / targets on short routes (dbl)}
#'     \item{medium_catch_rate}{Completions / targets on medium routes (dbl)}
#'     \item{deep_catch_rate}{Completions / targets on deep routes (dbl)}
#'     \item{short_epa_per_target}{EPA per short target (dbl)}
#'     \item{medium_epa_per_target}{EPA per medium target (dbl)}
#'     \item{deep_epa_per_target}{EPA per deep target (dbl)}
#'     \item{depth_profile}{"slot_heavy", "balanced", or "deep_threat" (chr)}
#'   }
#'
#' @details
#' Air yards depth buckets:
#' - Short: air_yards < 10 (screens, slants, quick outs -- slot-heavy)
#' - Medium: air_yards 10-19 (crossing routes, curls, digs)
#' - Deep: air_yards >= 20 (go routes, post routes, deep crosses)
#'
#' Depth profile classification:
#' - "slot_heavy": short_pct >= 0.55 (majority of targets < 10 air yards)
#' - "deep_threat": deep_pct >= 0.25 (meaningful portion of targets 20+ air yards)
#' - "balanced": everything else
#'
#' Plays with missing air_yards are excluded from depth bucketing but counted
#' in total_targets. This can occur on scramble-adjacent plays.
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' depth <- get_receiving_depth_profile(pbp, season = 2025, min_targets = 20)
#'
#' # Deep threats
#' depth %>%
#'   filter(depth_profile == "deep_threat") %>%
#'   arrange(desc(deep_targets)) %>%
#'   select(player_name, team, deep_targets, deep_pct, avg_air_yards)
#' }
#'
#' @seealso [calculate_usage_metrics()] for target share,
#'   [get_redzone_profile()] for inside-20 usage,
#'   [get_situational_splits()] (Week 5) for situational context
#'
#' @export
get_receiving_depth_profile <- function(pbp_data,
                                        season      = NULL,
                                        week_min    = 1,
                                        week_max    = 22,
                                        min_targets = 20) {

  # --- Input validation ---
  if (!is.data.frame(pbp_data)) stop("pbp_data must be a data frame or tibble.")

  required_cols <- c(
    "play_type", "posteam", "season", "week", "game_id",
    "receiver_player_id", "receiver_player_name",
    "air_yards", "complete_pass", "epa",
    "qb_kneel", "qb_spike"
  )
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  if (!is.null(season) && !is.numeric(season)) stop("season must be numeric or NULL.")
  if (!is.numeric(min_targets) || min_targets < 1) {
    stop("min_targets must be a positive integer.")
  }

  if (nrow(pbp_data) == 0) {
    message("pbp_data is empty. Returning empty depth profile tibble.")
    return(tibble())
  }

  # --- Filter ---
  if (!is.null(season)) pbp_data <- pbp_data %>% filter(season %in% !!season)
  pbp_data <- pbp_data %>% filter(week >= week_min, week <= week_max)

  # Pass plays only, with a receiver identified
  pass_plays <- pbp_data %>%
    filter(
      play_type == "pass",
      qb_kneel == 0,
      qb_spike == 0,
      !is.na(receiver_player_id),
      !is.na(receiver_player_name)
    )

  message(glue("Computing depth profile from {nrow(pass_plays)} pass plays targeting ",
               "{n_distinct(pass_plays$receiver_player_id)} receivers"))

  # --- Classify plays by air yards depth ---
  pass_classified <- pass_plays %>%
    mutate(
      depth_bucket = case_when(
        is.na(air_yards)     ~ NA_character_,
        air_yards < 10       ~ "short",
        air_yards < 20       ~ "medium",
        TRUE                 ~ "deep"
      )
    )

  # --- Overall totals per player-season ---
  overall <- pass_classified %>%
    group_by(season,
             player_id = receiver_player_id,
             player_name = receiver_player_name,
             team = posteam) %>%
    summarise(
      total_targets = n(),
      avg_air_yards = mean(air_yards, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Use most recent team for players traded mid-season
    group_by(season, player_id) %>%
    slice_max(total_targets, n = 1, with_ties = FALSE) %>%
    ungroup()

  # --- Depth bucket stats per player-season ---
  depth_stats <- pass_classified %>%
    filter(!is.na(depth_bucket)) %>%
    group_by(season,
             player_id = receiver_player_id,
             depth_bucket) %>%
    summarise(
      n_targets  = n(),
      n_catches  = sum(complete_pass == 1, na.rm = TRUE),
      total_epa  = sum(epa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      catch_rate      = ifelse(n_targets > 0, n_catches / n_targets, NA_real_),
      epa_per_target  = ifelse(n_targets > 0, total_epa / n_targets, NA_real_)
    )

  # Pivot to wide format
  depth_wide <- depth_stats %>%
    pivot_wider_safe(
      id_cols    = c(season, player_id),
      names_from = depth_bucket,
      values_from = c(n_targets, catch_rate, epa_per_target),
      values_fill = list(n_targets = 0L, catch_rate = NA_real_, epa_per_target = NA_real_)
    )

  # --- Join and compute shares ---
  depth_profile <- overall %>%
    left_join(depth_wide, by = c("season", "player_id")) %>%
    mutate(
      # NA-safe rename: use coalesce to handle players with 0 targets in a bucket
      short_targets  = coalesce(n_targets_short,  0L),
      medium_targets = coalesce(n_targets_medium, 0L),
      deep_targets   = coalesce(n_targets_deep,   0L),
      classified_targets = short_targets + medium_targets + deep_targets,
      # Share of classified (air_yards-available) targets
      short_pct  = ifelse(classified_targets > 0, short_targets  / classified_targets, NA_real_),
      medium_pct = ifelse(classified_targets > 0, medium_targets / classified_targets, NA_real_),
      deep_pct   = ifelse(classified_targets > 0, deep_targets   / classified_targets, NA_real_),
      # Per-bucket efficiency
      short_catch_rate      = catch_rate_short,
      medium_catch_rate     = catch_rate_medium,
      deep_catch_rate       = catch_rate_deep,
      short_epa_per_target  = epa_per_target_short,
      medium_epa_per_target = epa_per_target_medium,
      deep_epa_per_target   = epa_per_target_deep,
      # Profile classification
      depth_profile = case_when(
        !is.na(short_pct)  & short_pct >= 0.55             ~ "slot_heavy",
        !is.na(deep_pct)   & deep_pct  >= 0.25             ~ "deep_threat",
        !is.na(classified_targets) & classified_targets > 0 ~ "balanced",
        TRUE                                                 ~ NA_character_
      )
    ) %>%
    filter(total_targets >= min_targets) %>%
    select(season, player_id, player_name, team, total_targets, avg_air_yards,
           short_targets, medium_targets, deep_targets,
           short_pct, medium_pct, deep_pct,
           short_catch_rate, medium_catch_rate, deep_catch_rate,
           short_epa_per_target, medium_epa_per_target, deep_epa_per_target,
           depth_profile) %>%
    arrange(desc(total_targets))

  # Profile breakdown
  profile_counts <- depth_profile %>%
    count(depth_profile, name = "n") %>%
    arrange(depth_profile)
  message(glue("Depth profiles: {paste(paste(profile_counts$depth_profile, profile_counts$n, sep = '='), collapse = ', ')}"))
  message(glue("Excluded {sum(overall$total_targets < min_targets)} players with < {min_targets} targets"))

  return(depth_profile)
}


# ==============================================================================
# INTERNAL HELPER: Safe pivot_wider wrapper
# ==============================================================================

#' @noRd
pivot_wider_safe <- function(data, id_cols, names_from, values_from, values_fill = list()) {
  tidyr::pivot_wider(data,
                     id_cols     = {{ id_cols }},
                     names_from  = {{ names_from }},
                     values_from = {{ values_from }},
                     values_fill = values_fill)
}
