#' Get Team Offensive Statistics
#'
#' @description
#' Aggregates play-by-play data to calculate offensive statistics for each team.
#' Returns volume, production, efficiency, and situational metrics.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric vector of season(s) to filter
#' @param week_min Optional. Minimum week number (inclusive)
#' @param week_max Optional. Maximum week number (inclusive)
#'
#' @return Tibble with one row per team containing offensive statistics
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2024)
#' offense_stats <- get_team_offense_stats(pbp, season = 2024)
#' }
#'
#' @export
get_team_offense_stats <- function(pbp_data, 
                                   season = NULL, 
                                   week_min = NULL, 
                                   week_max = NULL) {
  
  require(dplyr)
  require(glue)
  
  # Input validation
  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }
  
  required_cols <- c("posteam", "play_type", "yards_gained", "epa", "success",
                     "down", "third_down_converted", "fourth_down_converted",
                     "drive_ended_with_score", "game_id", "season")
  
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse=', ')}"))
  }
  
  # Filter by season/week if provided
  if (!is.null(season)) {
    if (!is.numeric(season)) {
      stop("season must be numeric")
    }
    pbp_data <- pbp_data %>% filter(season %in% !!season)
  }
  
  if (!is.null(week_min)) {
    if (!is.numeric(week_min) || length(week_min) != 1) {
      stop("week_min must be a single numeric value")
    }
    pbp_data <- pbp_data %>% filter(week >= !!week_min)
  }
  
  if (!is.null(week_max)) {
    if (!is.numeric(week_max) || length(week_max) != 1) {
      stop("week_max must be a single numeric value")
    }
    pbp_data <- pbp_data %>% filter(week <= !!week_max)
  }
  
  # Filter to offensive plays (pass/run) and exclude NAs
  offensive_plays <- pbp_data %>%
    filter(
      play_type %in% c("pass", "run"),
      !is.na(posteam)
    )
  
  if (nrow(offensive_plays) == 0) {
    warning("No offensive plays found with given filters")
    return(tibble())
  }
  
  # Calculate team-level statistics
  offense_stats <- offensive_plays %>%
    group_by(posteam) %>%
    summarise(
      # Season context
      season = paste(unique(season), collapse = ", "),
      games_played = n_distinct(game_id),
      
      # Volume metrics
      total_plays = n(),
      pass_attempts = sum(play_type == "pass", na.rm = TRUE),
      rush_attempts = sum(play_type == "run", na.rm = TRUE),
      pass_play_pct = pass_attempts / total_plays,
      
      # Production metrics
      total_yards = sum(yards_gained, na.rm = TRUE),
      yards_per_play = mean(yards_gained, na.rm = TRUE),
      
      # Efficiency metrics
      offensive_epa = sum(epa, na.rm = TRUE),
      offensive_epa_per_play = mean(epa, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      
      # Situational metrics - third down
      third_down_attempts = sum(down == 3, na.rm = TRUE),
      third_down_conversions = sum(third_down_converted == 1, na.rm = TRUE),
      third_down_conversion_rate = third_down_conversions / third_down_attempts,
      
      # Situational metrics - fourth down
      fourth_down_attempts = sum(down == 4, na.rm = TRUE),
      fourth_down_conversions = sum(fourth_down_converted == 1, na.rm = TRUE),
      fourth_down_conversion_rate = fourth_down_conversions / fourth_down_attempts,
      
      # Context metrics
      explosive_play_rate = mean(yards_gained >= 20, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    # Rename for clarity
    rename(team = posteam) %>%
    # Sort by offensive EPA (descending)
    arrange(desc(offensive_epa_per_play))
  
  message(glue("Calculated offensive stats for {nrow(offense_stats)} teams"))
  
  return(offense_stats)
}


#' Get Team Defensive Statistics
#'
#' @description
#' Aggregates play-by-play data to calculate defensive statistics for each team.
#' Returns volume, production allowed, efficiency, and situational metrics.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric vector of season(s) to filter
#' @param week_min Optional. Minimum week number (inclusive)
#' @param week_max Optional. Maximum week number (inclusive)
#'
#' @return Tibble with one row per team containing defensive statistics
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2024)
#' defense_stats <- get_team_defense_stats(pbp, season = 2024)
#' }
#'
#' @export
get_team_defense_stats <- function(pbp_data, 
                                   season = NULL, 
                                   week_min = NULL, 
                                   week_max = NULL) {
  
  require(dplyr)
  require(glue)
  
  # Input validation
  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }
  
  required_cols <- c("defteam", "play_type", "yards_gained", "epa", "success",
                     "down", "third_down_converted", "fourth_down_converted",
                     "sack", "interception", "fumble_lost", "game_id", "season")
  
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse=', ')}"))
  }
  
  # Filter by season/week if provided
  if (!is.null(season)) {
    if (!is.numeric(season)) {
      stop("season must be numeric")
    }
    pbp_data <- pbp_data %>% filter(season %in% !!season)
  }
  
  if (!is.null(week_min)) {
    if (!is.numeric(week_min) || length(week_min) != 1) {
      stop("week_min must be a single numeric value")
    }
    pbp_data <- pbp_data %>% filter(week >= !!week_min)
  }
  
  if (!is.null(week_max)) {
    if (!is.numeric(week_max) || length(week_max) != 1) {
      stop("week_max must be a single numeric value")
    }
    pbp_data <- pbp_data %>% filter(week <= !!week_max)
  }
  
  # Filter to defensive plays (pass/run against) and exclude NAs
  defensive_plays <- pbp_data %>%
    filter(
      play_type %in% c("pass", "run"),
      !is.na(defteam)
    )
  
  if (nrow(defensive_plays) == 0) {
    warning("No defensive plays found with given filters")
    return(tibble())
  }
  
  # Calculate team-level statistics
  defense_stats <- defensive_plays %>%
    group_by(defteam) %>%
    summarise(
      # Season context
      season = paste(unique(season), collapse = ", "),
      games_played = n_distinct(game_id),
      
      # Volume metrics
      plays_against = n(),
      pass_attempts_against = sum(play_type == "pass", na.rm = TRUE),
      rush_attempts_against = sum(play_type == "run", na.rm = TRUE),
      
      # Production metrics (from defensive perspective)
      yards_allowed = sum(yards_gained, na.rm = TRUE),
      yards_per_play_allowed = mean(yards_gained, na.rm = TRUE),
      
      # Efficiency metrics (negative EPA is good for defense)
      defensive_epa = sum(epa, na.rm = TRUE),
      defensive_epa_per_play = mean(epa, na.rm = TRUE),
      success_rate_allowed = mean(success, na.rm = TRUE),
      
      # Situational metrics - third down stops
      third_down_attempts_against = sum(down == 3, na.rm = TRUE),
      third_down_stops = sum(down == 3 & third_down_converted == 0, na.rm = TRUE),
      third_down_stop_rate = third_down_stops / third_down_attempts_against,
      
      # Situational metrics - fourth down stops
      fourth_down_attempts_against = sum(down == 4, na.rm = TRUE),
      fourth_down_stops = sum(down == 4 & fourth_down_converted == 0, na.rm = TRUE),
      fourth_down_stop_rate = fourth_down_stops / fourth_down_attempts_against,
      
      # Playmaking metrics
      sacks = sum(sack == 1, na.rm = TRUE),
      sack_rate = sacks / pass_attempts_against,
      interceptions = sum(interception == 1, na.rm = TRUE),
      fumbles_recovered = sum(fumble_lost == 1, na.rm = TRUE),
      turnovers_generated = interceptions + fumbles_recovered,
      turnover_rate = turnovers_generated / plays_against,
      
      # Context metrics
      explosive_plays_allowed = sum(yards_gained >= 20, na.rm = TRUE),
      explosive_play_rate_allowed = explosive_plays_allowed / plays_against,
      
      .groups = "drop"
    ) %>%
    # Rename for clarity
    rename(team = defteam) %>%
    # Sort by defensive EPA (ascending - lower is better for defense)
    arrange(defensive_epa_per_play)
  
  message(glue("Calculated defensive stats for {nrow(defense_stats)} teams"))
  
  return(defense_stats)
}
