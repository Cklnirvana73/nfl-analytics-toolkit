#' Get Game Summary Statistics
#'
#' @description
#' Summarizes a single game's key statistics including score, volume, efficiency,
#' and key events for both teams. Now includes win probability context.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param game_id Single game ID (e.g., "2024_01_KC_BAL")
#'
#' @return Single-row tibble with comprehensive game statistics, or NULL if game not found
#'
#' @details
#' **NFL Context:**
#' - EPA (Expected Points Added): Positive = offense gained expected points
#' - Success rate: Proportion of plays with positive EPA
#' - Turnovers include interceptions and fumbles lost
#' - Sacks counted from defensive perspective (allowed by offense)
#'
#' **Win Probability Context:**
#' - If available, includes biggest win probability swing in game
#' - Large swings typically occur on: turnovers, 4th down conversions, long TDs
#' - Leverage situations = close games where WP can swing dramatically
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2024)
#' game_summary <- get_game_summary(pbp, "2024_01_KC_BAL")
#' 
#' # View key stats
#' game_summary %>%
#'   select(home_team, away_team, winner, home_epa, away_epa)
#' }
#'
#' @seealso
#' \code{\link{get_scoring_plays}} for play-by-play scoring timeline
#' \code{\link{get_drive_summary}} for drive-level efficiency
#'
#' @export
get_game_summary <- function(pbp_data, game_id) {
  
  library(dplyr)
  library(glue)
  
  # Input validation
  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }
  
  if (!is.character(game_id) || length(game_id) != 1) {
    stop("game_id must be a single character string")
  }
  
  required_cols <- c("game_id", "home_team", "away_team", "game_date", "week",
                     "total_home_score", "total_away_score", "posteam", "defteam",
                     "play_type", "epa", "success", "sack", "interception",
                     "fumble_lost")
  
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse=', ')}"))
  }
  
  # Filter to specific game
  game_plays <- pbp_data %>%
    filter(game_id == !!game_id)
  
  if (nrow(game_plays) == 0) {
    warning(glue("No plays found for game_id: {game_id}"))
    return(NULL)
  }
  
  # Get basic game info
  game_info <- game_plays %>%
    slice(1) %>%
    select(game_id, home_team, away_team, game_date, week, 
           total_home_score, total_away_score)
  
  # Filter to offensive plays only for stats
  offensive_plays <- game_plays %>%
    filter(play_type %in% c("pass", "run"))
  
  # Calculate home team stats
  home_stats <- offensive_plays %>%
    filter(posteam == game_info$home_team) %>%
    summarise(
      home_plays = n(),
      home_epa = sum(epa, na.rm = TRUE),
      home_success_rate = mean(success, na.rm = TRUE),
      home_sacks_allowed = sum(sack == 1, na.rm = TRUE),
      home_turnovers = sum(
        (interception == 1 | fumble_lost == 1),
        na.rm = TRUE
      )
    )
  
  # Calculate away team stats
  away_stats <- offensive_plays %>%
    filter(posteam == game_info$away_team) %>%
    summarise(
      away_plays = n(),
      away_epa = sum(epa, na.rm = TRUE),
      away_success_rate = mean(success, na.rm = TRUE),
      away_sacks_allowed = sum(sack == 1, na.rm = TRUE),
      away_turnovers = sum(
        (interception == 1 | fumble_lost == 1),
        na.rm = TRUE
      )
    )
  
  # Combine all stats
  game_summary <- game_info %>%
    bind_cols(home_stats, away_stats) %>%
    mutate(
      total_plays = home_plays + away_plays,
      score_margin = total_home_score - total_away_score,
      winner = case_when(
        score_margin > 0 ~ home_team,
        score_margin < 0 ~ away_team,
        TRUE ~ "TIE"
      )
    )
  
  # NFL ENHANCEMENT: Add win probability swing if available
  if ("wpa" %in% names(game_plays)) {
    biggest_swing <- max(abs(game_plays$wpa), na.rm = TRUE)
    if (is.finite(biggest_swing)) {
      game_summary <- game_summary %>%
        mutate(biggest_wp_swing = biggest_swing)
    }
  }
  
  return(game_summary)
}


#' Get Scoring Plays from a Game
#'
#' @description
#' Extracts all scoring plays from a specific game, including touchdowns,
#' field goals, and safeties with context about game situation.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param game_id Single game ID (e.g., "2024_01_KC_BAL")
#'
#' @return Tibble with one row per scoring play, or NULL if game not found
#'
#' @details
#' **NFL Context:**
#' - Touchdown = 6 points (extra point/2-pt conversion tracked separately)
#' - Field Goal = 3 points
#' - Safety = 2 points (defensive score)
#' - Score differential shows game flow and momentum shifts
#' - Time remaining crucial for understanding comeback attempts
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2024)
#' scoring <- get_scoring_plays(pbp, "2024_01_KC_BAL")
#' 
#' # View scoring timeline
#' scoring %>%
#'   select(qtr, time_remaining, scoring_team, score_type, points)
#' }
#'
#' @seealso
#' \code{\link{get_game_summary}} for overall game statistics
#' \code{\link{get_drive_summary}} for drive-level analysis
#'
#' @export
get_scoring_plays <- function(pbp_data, game_id) {
  
  library(dplyr)
  library(glue)
  
  # Input validation
  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }
  
  if (!is.character(game_id) || length(game_id) != 1) {
    stop("game_id must be a single character string")
  }
  
  required_cols <- c("game_id", "play_id", "quarter_seconds_remaining", "qtr",
                     "down", "ydstogo", "posteam", "defteam", "desc",
                     "td_team", "field_goal_result", "safety",
                     "total_home_score", "total_away_score")
  
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse=', ')}"))
  }
  
  # Filter to specific game
  game_plays <- pbp_data %>%
    filter(game_id == !!game_id)
  
  if (nrow(game_plays) == 0) {
    warning(glue("No plays found for game_id: {game_id}"))
    return(NULL)
  }
  
  # Filter to scoring plays only
  scoring_plays <- game_plays %>%
    filter(
      !is.na(td_team) | 
      field_goal_result == "made" | 
      safety == 1
    ) %>%
    mutate(
      # Determine score type and points
      score_type = case_when(
        !is.na(td_team) ~ "Touchdown",
        field_goal_result == "made" ~ "Field Goal",
        safety == 1 ~ "Safety",
        TRUE ~ "Unknown"
      ),
      points = case_when(
        !is.na(td_team) ~ 6L,  # Base TD points (extra point/2pt conversion separate)
        field_goal_result == "made" ~ 3L,
        safety == 1 ~ 2L,
        TRUE ~ 0L
      ),
      scoring_team = case_when(
        !is.na(td_team) ~ td_team,
        field_goal_result == "made" ~ posteam,
        safety == 1 ~ defteam,
        TRUE ~ NA_character_
      ),
      # Calculate time remaining in quarter
      time_remaining = sprintf(
        "%d:%02d", 
        floor(quarter_seconds_remaining / 60),
        as.integer(quarter_seconds_remaining %% 60)
      )
    ) %>%
    # Calculate score differential before this play
    arrange(play_id) %>%
    mutate(
      score_differential_before = lag(total_home_score - total_away_score, default = 0)
    ) %>%
    select(
      game_id, play_id, qtr, time_remaining, down, ydstogo,
      scoring_team, score_type, points, posteam, defteam,
      score_differential_before, desc
    ) %>%
    arrange(play_id)
  
  if (nrow(scoring_plays) == 0) {
    message(glue("No scoring plays found for game_id: {game_id}"))
    return(tibble())
  }
  
  message(glue("Found {nrow(scoring_plays)} scoring plays in game {game_id}"))
  
  return(scoring_plays)
}


#' Get Drive Summary Statistics
#'
#' @description
#' Summarizes each drive in a game including outcome, plays, yards, and time.
#' Useful for analyzing offensive efficiency and game flow.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param game_id Single game ID (e.g., "2024_01_KC_BAL")
#'
#' @return Tibble with one row per drive, or NULL if game not found
#'
#' @details
#' **NFL Context:**
#' - Drive = series of plays from one team's possession until change
#' - Ended with score = TD or FG (successful drive)
#' - Drive EPA = total expected points added across all plays in drive
#' - Positive drive EPA = offense moved into better scoring position
#' - Time of possession affects game strategy (clock management)
#'
#' **Typical Drive Statistics:**
#' - Average plays per drive: 5-6
#' - Average yards per drive: 25-35
#' - Drive success rate: 30-40% (end in score)
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2024)
#' drives <- get_drive_summary(pbp, "2024_01_KC_BAL")
#' 
#' # View scoring drives
#' drives %>%
#'   filter(ended_with_score == 1) %>%
#'   select(drive, posteam, plays, yards_gained, drive_epa)
#' }
#'
#' @seealso
#' \code{\link{get_game_summary}} for overall game statistics
#' \code{\link{get_scoring_plays}} for scoring timeline
#'
#' @export
get_drive_summary <- function(pbp_data, game_id) {
  
  library(dplyr)
  library(glue)
  
  # Input validation
  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }
  
  if (!is.character(game_id) || length(game_id) != 1) {
    stop("game_id must be a single character string")
  }
  
  required_cols <- c("game_id", "drive", "posteam", "defteam", 
                     "drive_ended_with_score", "drive_play_count",
                     "drive_yards_penalized", "drive_start_yard_line",
                     "drive_end_yard_line", "drive_time_of_possession")
  
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse=', ')}"))
  }
  
  # Filter to specific game
  game_plays <- pbp_data %>%
    filter(game_id == !!game_id)
  
  if (nrow(game_plays) == 0) {
    warning(glue("No plays found for game_id: {game_id}"))
    return(NULL)
  }
  
  # Get drive-level summary
  drive_summary <- game_plays %>%
    filter(!is.na(drive)) %>%
    group_by(game_id, drive, posteam) %>%
    summarise(
      defteam = first(defteam),
      plays = first(drive_play_count),
      yards_gained = sum(yards_gained, na.rm = TRUE),
      yards_penalized = first(drive_yards_penalized),
      start_yard_line = first(drive_start_yard_line),
      end_yard_line = first(drive_end_yard_line),
      time_of_possession = first(drive_time_of_possession),
      ended_with_score = first(drive_ended_with_score),
      drive_epa = sum(epa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(drive)
  
  if (nrow(drive_summary) == 0) {
    message(glue("No drives found for game_id: {game_id}"))
    return(tibble())
  }
  
  message(glue("Found {nrow(drive_summary)} drives in game {game_id}"))
  
  return(drive_summary)
}
