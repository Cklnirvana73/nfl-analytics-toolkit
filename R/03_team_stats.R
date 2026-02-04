#' Get Team Offensive Statistics
#'
#' @description
#' Aggregates play-by-play data to calculate offensive statistics for each team.
#' Returns volume, production, efficiency, and situational metrics. Now includes
#' optional garbage time filtering and QB kneel/spike exclusion for accurate
#' efficiency metrics.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric vector of season(s) to filter
#' @param week_min Optional. Minimum week number (inclusive)
#' @param week_max Optional. Maximum week number (inclusive)
#' @param exclude_garbage_time Logical. If TRUE (default), excludes plays when win probability
#'   <10% or >90% in 4th quarter. This provides cleaner efficiency metrics by removing
#'   situations where teams play conservatively or aggressively due to game being decided.
#'
#' @return Tibble with one row per team containing offensive statistics
#'
#' @details
#' **NFL Context:**
#' - Excludes QB kneels and spikes (intentional negative/zero-yard plays)
#' - Garbage time = Win probability <10% or >90% in 4th quarter
#' - EPA (Expected Points Added): Measures play value in expected points
#' - Success rate: Proportion of plays with positive EPA
#' - League average offensive EPA/play: ~0.0 to +0.05
#'
#' **Why Filter Garbage Time:**
#' - Teams down big pass more (inflates passing volume, deflates efficiency)
#' - Teams up big run more (inflates rushing volume, deflates efficiency)
#' - Defenses play prevent coverage (inflates offensive stats artificially)
#' - Standard practice in NFL analytics for evaluating "true" team efficiency
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2024)
#' 
#' # With garbage time filtering (default - recommended)
#' offense_stats <- get_team_offense_stats(pbp, season = 2024)
#' 
#' # Without garbage time filtering (includes all plays)
#' offense_all <- get_team_offense_stats(pbp, season = 2024, 
#'                                       exclude_garbage_time = FALSE)
#' }
#'
#' @seealso
#' \code{\link{get_team_defense_stats}} for defensive statistics
#'
#' @export
get_team_offense_stats <- function(pbp_data, 
                                   season = NULL, 
                                   week_min = NULL, 
                                   week_max = NULL,
                                   exclude_garbage_time = TRUE) {
  
  library(dplyr)
  library(glue)
  
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
  # CRITICAL NFL FIX: Exclude QB kneels and spikes (intentional negative/zero plays)
  offensive_plays <- pbp_data %>%
    filter(
      play_type %in% c("pass", "run"),
      !is.na(posteam)
    )
  
  # Remove QB kneels and spikes if they exist as play types
  if (any(c("qb_kneel", "qb_spike") %in% unique(offensive_plays$play_type))) {
    rows_before <- nrow(offensive_plays)
    offensive_plays <- offensive_plays %>%
      filter(!(play_type %in% c("qb_kneel", "qb_spike")))
    rows_removed <- rows_before - nrow(offensive_plays)
    if (rows_removed > 0) {
      message(glue("Excluded {rows_removed} QB kneels/spikes from efficiency metrics"))
    }
  }
  
  # CRITICAL NFL FIX: Optionally exclude garbage time
  # Garbage time = Win probability <10% or >90% in 4th quarter
  if (exclude_garbage_time) {
    if ("wp" %in% names(offensive_plays) && "qtr" %in% names(offensive_plays)) {
      rows_before <- nrow(offensive_plays)
      offensive_plays <- offensive_plays %>%
        filter(
          # Keep plays that are NOT garbage time
          !(qtr == 4 & (wp < 0.10 | wp > 0.90))
        )
      rows_removed <- rows_before - nrow(offensive_plays)
      if (rows_removed > 0) {
        message(glue("Excluded {rows_removed} garbage time plays (Q4, WP <10% or >90%)"))
      }
    } else {
      warning("Cannot filter garbage time: 'wp' or 'qtr' column not found in data")
    }
  }
  
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
      third_down_conversion_rate = ifelse(third_down_attempts > 0, 
                                          third_down_conversions / third_down_attempts, 
                                          NA_real_),
      
      # Situational metrics - fourth down
      fourth_down_attempts = sum(down == 4, na.rm = TRUE),
      fourth_down_conversions = sum(fourth_down_converted == 1, na.rm = TRUE),
      fourth_down_conversion_rate = ifelse(fourth_down_attempts > 0,
                                           fourth_down_conversions / fourth_down_attempts,
                                           NA_real_),      
      # Context metrics (NFL standard: explosive play = 20+ yards)
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
#' Returns volume, production allowed, efficiency, and situational metrics. Now includes
#' optional garbage time filtering and QB kneel/spike exclusion for accurate
#' efficiency metrics.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric vector of season(s) to filter
#' @param week_min Optional. Minimum week number (inclusive)
#' @param week_max Optional. Maximum week number (inclusive)
#' @param exclude_garbage_time Logical. If TRUE (default), excludes plays when win probability
#'   <10% or >90% in 4th quarter. Provides cleaner defensive efficiency metrics.
#'
#' @return Tibble with one row per team containing defensive statistics
#'
#' @details
#' **NFL Context:**
#' - Defensive EPA: Negative is GOOD (preventing expected points)
#' - Success rate allowed: Lower is better (fewer offensive successes)
#' - League average defensive EPA/play: ~-0.05 to 0.0
#' - Sack rate: Sacks per pass attempt against
#' - Turnover rate: Turnovers generated per play
#'
#' **Elite Defense Benchmarks:**
#' - Defensive EPA/play: < -0.10 (significantly negative)
#' - Success rate allowed: < 40%
#' - Sack rate: > 8%
#' - Turnover rate: > 2%
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2024)
#' defense_stats <- get_team_defense_stats(pbp, season = 2024)
#' 
#' # View best defenses by EPA
#' defense_stats %>%
#'   arrange(defensive_epa_per_play) %>%  # Most negative = best
#'   head(10)
#' }
#'
#' @seealso
#' \code{\link{get_team_offense_stats}} for offensive statistics
#'
#' @export
get_team_defense_stats <- function(pbp_data, 
                                   season = NULL, 
                                   week_min = NULL, 
                                   week_max = NULL,
                                   exclude_garbage_time = TRUE) {
  
  library(dplyr)
  library(glue)
  
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
  # CRITICAL NFL FIX: Exclude QB kneels and spikes
  defensive_plays <- pbp_data %>%
    filter(
      play_type %in% c("pass", "run"),
      !is.na(defteam)
    )
  
  # Remove QB kneels and spikes if they exist as play types
  if (any(c("qb_kneel", "qb_spike") %in% unique(defensive_plays$play_type))) {
    rows_before <- nrow(defensive_plays)
    defensive_plays <- defensive_plays %>%
      filter(!(play_type %in% c("qb_kneel", "qb_spike")))
    rows_removed <- rows_before - nrow(defensive_plays)
    if (rows_removed > 0) {
      message(glue("Excluded {rows_removed} QB kneels/spikes from defensive metrics"))
    }
  }
  
  # CRITICAL NFL FIX: Optionally exclude garbage time
  if (exclude_garbage_time) {
    if ("wp" %in% names(defensive_plays) && "qtr" %in% names(defensive_plays)) {
      rows_before <- nrow(defensive_plays)
      defensive_plays <- defensive_plays %>%
        filter(
          !(qtr == 4 & (wp < 0.10 | wp > 0.90))
        )
      rows_removed <- rows_before - nrow(defensive_plays)
      if (rows_removed > 0) {
        message(glue("Excluded {rows_removed} garbage time plays from defensive stats"))
      }
    } else {
      warning("Cannot filter garbage time: 'wp' or 'qtr' column not found in data")
    }
  }
  
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
      third_down_stop_rate = ifelse(third_down_attempts_against > 0,
                                    third_down_stops / third_down_attempts_against,
                                    NA_real_),
      
      # Situational metrics - fourth down stops
      fourth_down_attempts_against = sum(down == 4, na.rm = TRUE),
      fourth_down_stops = sum(down == 4 & fourth_down_converted == 0, na.rm = TRUE),
      fourth_down_stop_rate = ifelse(fourth_down_attempts_against > 0,
                                     fourth_down_stops / fourth_down_attempts_against,
                                     NA_real_),
      
      # Playmaking metrics
      sacks = sum(sack == 1, na.rm = TRUE),
      sack_rate = ifelse(pass_attempts_against > 0,
                         sacks / pass_attempts_against,
                         NA_real_),
      interceptions = sum(interception == 1, na.rm = TRUE),
      fumbles_recovered = sum(fumble_lost == 1, na.rm = TRUE),
      turnovers_generated = interceptions + fumbles_recovered,
      turnover_rate = ifelse(plays_against > 0,
                             turnovers_generated / plays_against,
                             NA_real_),      
      # Context metrics (NFL standard: explosive play = 20+ yards)
      explosive_plays_allowed = sum(yards_gained >= 20, na.rm = TRUE),
      explosive_play_rate_allowed = explosive_plays_allowed / plays_against,
      
      .groups = "drop"
    ) %>%
    # Rename for clarity
    rename(team = defteam) %>%
    # Sort by defensive EPA (ascending - lower/more negative is better for defense)
    arrange(defensive_epa_per_play)
  
  message(glue("Calculated defensive stats for {nrow(defense_stats)} teams"))
  
  return(defense_stats)
}
