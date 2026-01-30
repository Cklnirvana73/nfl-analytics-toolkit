#' Get Player Rushing Statistics
#'
#' @description
#' Aggregates play-by-play data to calculate rushing statistics for each player.
#' Returns volume, production, and efficiency metrics.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric vector of season(s) to filter (e.g., 2024 or 2023:2024)
#' @param week_min Optional. Minimum week number (inclusive)
#' @param week_max Optional. Maximum week number (inclusive)
#'
#' @return Tibble with one row per player containing rushing statistics
#'
#' @examples
#' \dontrun{
#' # Load data
#' pbp <- load_and_validate_pbp(2024)
#' 
#' # Get full season stats
#' rush_stats <- get_player_rushing_stats(pbp, season = 2024)
#' 
#' # Get stats for specific week range
#' rush_stats_early <- get_player_rushing_stats(pbp, season = 2024, week_max = 10)
#' }
#'
#' @export
get_player_rushing_stats <- function(pbp_data, 
                                     season = NULL, 
                                     week_min = NULL, 
                                     week_max = NULL) {
  
  require(dplyr)
  require(glue)
  
  # Input validation
  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }
  
  required_cols <- c("rusher_player_id", "rusher_player_name", "posteam", 
                     "play_type", "rushing_yards", "rush_touchdown", 
                     "first_down_rush", "epa", "success", "game_id")
  
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
  
  # Filter to rushing plays only and exclude NAs (CRITICAL - Week 1 lesson!)
  rushing_plays <- pbp_data %>%
    filter(
      play_type == "run",
      !is.na(rusher_player_id),
      !is.na(rusher_player_name)
    )
  
  if (nrow(rushing_plays) == 0) {
    warning("No rushing plays found with given filters")
    return(tibble())
  }
  
  # Calculate per-player statistics
  rush_stats <- rushing_plays %>%
    group_by(rusher_player_id, rusher_player_name) %>%
    summarise(
      # Get most recent team (last team they played for)
      recent_team = last(posteam),
      
      # Volume metrics
      rushes = n(),
      games_played = n_distinct(game_id),
      
      # Production metrics
      rush_yards = sum(rushing_yards, na.rm = TRUE),
      rush_tds = sum(rush_touchdown, na.rm = TRUE),
      rush_first_downs = sum(first_down_rush, na.rm = TRUE),
      
      # Efficiency metrics
      yards_per_carry = rush_yards / rushes,
      rush_epa = sum(epa, na.rm = TRUE),
      rush_epa_per_play = mean(epa, na.rm = TRUE),
      rush_success_rate = mean(success, na.rm = TRUE),
      
      # Context metrics
      explosive_rush_rate = mean(rushing_yards >= 15, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    # Rename for clarity
    rename(
      player_id = rusher_player_id,
      player_name = rusher_player_name
    ) %>%
    # Sort by total yards (descending)
    arrange(desc(rush_yards))
  
  message(glue("Calculated rushing stats for {nrow(rush_stats)} players"))
  
  return(rush_stats)
}


#' Get Player Passing Statistics
#'
#' @description
#' Aggregates play-by-play data to calculate passing statistics for each QB.
#' Returns volume, production, efficiency, and advanced metrics.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric vector of season(s) to filter
#' @param week_min Optional. Minimum week number (inclusive)
#' @param week_max Optional. Maximum week number (inclusive)
#'
#' @return Tibble with one row per QB containing passing statistics
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2024)
#' pass_stats <- get_player_passing_stats(pbp, season = 2024)
#' }
#'
#' @export
get_player_passing_stats <- function(pbp_data, 
                                     season = NULL, 
                                     week_min = NULL, 
                                     week_max = NULL) {
  
  require(dplyr)
  require(glue)
  
  # Input validation
  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }
  
  required_cols <- c("passer_player_id", "passer_player_name", "posteam",
                     "play_type", "complete_pass", "passing_yards", 
                     "pass_touchdown", "interception", "epa", "cpoe",
                     "air_yards", "sack", "game_id")
  
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
  
  # Filter to passing plays (including sacks, spikes, kneels) and exclude NAs
  passing_plays <- pbp_data %>%
    filter(
      play_type %in% c("pass", "qb_spike", "qb_kneel"),
      !is.na(passer_player_id),
      !is.na(passer_player_name)
    )
  
  if (nrow(passing_plays) == 0) {
    warning("No passing plays found with given filters")
    return(tibble())
  }
  
  # Calculate per-player statistics
  pass_stats <- passing_plays %>%
    group_by(passer_player_id, passer_player_name) %>%
    summarise(
      # Get most recent team
      recent_team = last(posteam),
      
      # Volume metrics
      attempts = n(),
      completions = sum(complete_pass == 1, na.rm = TRUE),
      games_played = n_distinct(game_id),
      
      # Production metrics
      pass_yards = sum(passing_yards, na.rm = TRUE),
      pass_tds = sum(pass_touchdown, na.rm = TRUE),
      interceptions = sum(interception, na.rm = TRUE),
      
      # Efficiency metrics
      completion_pct = completions / attempts,
      yards_per_attempt = pass_yards / attempts,
      
      # Advanced metrics
      pass_epa = sum(epa, na.rm = TRUE),
      pass_epa_per_play = mean(epa, na.rm = TRUE),
      cpoe = mean(cpoe, na.rm = TRUE),  # Completion % over expected
      air_yards_per_attempt = mean(air_yards, na.rm = TRUE),
      
      # Context metrics
      sack_rate = sum(sack == 1, na.rm = TRUE) / attempts,
      
      .groups = "drop"
    ) %>%
    # Calculate passer rating (simplified NFL formula)
    mutate(
      passer_rating = pmin(
        pmax((completion_pct - 0.3) * 5, 0) * 100 +
        pmax((yards_per_attempt - 3) * 0.25, 0) * 100 +
        pmax(pass_tds / attempts * 20, 0) * 100 +
        pmax(2.375 - (interceptions / attempts * 25), 0) * 100,
        158.3
      ) / 6
    ) %>%
    # Rename for clarity
    rename(
      player_id = passer_player_id,
      player_name = passer_player_name
    ) %>%
    # Sort by pass yards (descending)
    arrange(desc(pass_yards))
  
  message(glue("Calculated passing stats for {nrow(pass_stats)} players"))
  
  return(pass_stats)
}


#' Get Player Receiving Statistics
#'
#' @description
#' Aggregates play-by-play data to calculate receiving statistics for each receiver.
#' Returns volume, production, efficiency, and target metrics.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric vector of season(s) to filter
#' @param week_min Optional. Minimum week number (inclusive)
#' @param week_max Optional. Maximum week number (inclusive)
#'
#' @return Tibble with one row per receiver containing receiving statistics
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2024)
#' rec_stats <- get_player_receiving_stats(pbp, season = 2024)
#' }
#'
#' @export
get_player_receiving_stats <- function(pbp_data, 
                                       season = NULL, 
                                       week_min = NULL, 
                                       week_max = NULL) {
  
  require(dplyr)
  require(glue)
  
  # Input validation
  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }
  
  required_cols <- c("receiver_player_id", "receiver_player_name", "posteam",
                     "play_type", "complete_pass", "receiving_yards", 
                     "pass_touchdown", "epa", "air_yards", "yards_after_catch",
                     "first_down_pass", "game_id")
  
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
  
  # Filter to pass plays with a receiver and exclude NAs
  receiving_plays <- pbp_data %>%
    filter(
      play_type == "pass",
      !is.na(receiver_player_id),
      !is.na(receiver_player_name)
    )
  
  if (nrow(receiving_plays) == 0) {
    warning("No receiving plays found with given filters")
    return(tibble())
  }
  
  # Calculate team-level target totals for target share
  team_targets <- receiving_plays %>%
    group_by(posteam, game_id) %>%
    summarise(team_targets = n(), .groups = "drop")
  
  # Calculate per-player statistics
  rec_stats <- receiving_plays %>%
    # Join team targets for calculating share
    left_join(team_targets, by = c("posteam", "game_id")) %>%
    group_by(receiver_player_id, receiver_player_name) %>%
    summarise(
      # Get most recent team
      recent_team = last(posteam),
      
      # Volume metrics
      targets = n(),
      receptions = sum(complete_pass == 1, na.rm = TRUE),
      games_played = n_distinct(game_id),
      
      # Production metrics
      rec_yards = sum(receiving_yards, na.rm = TRUE),
      rec_tds = sum(pass_touchdown, na.rm = TRUE),
      rec_first_downs = sum(first_down_pass, na.rm = TRUE),
      
      # Efficiency metrics
      catch_rate = receptions / targets,
      yards_per_reception = rec_yards / receptions,
      yards_per_target = rec_yards / targets,
      
      # Advanced metrics
      rec_epa = sum(epa, na.rm = TRUE),
      rec_epa_per_target = mean(epa, na.rm = TRUE),
      avg_yac = mean(yards_after_catch, na.rm = TRUE),
      avg_air_yards = mean(air_yards, na.rm = TRUE),
      
      # Context metrics
      target_share = targets / sum(team_targets, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    # Rename for clarity
    rename(
      player_id = receiver_player_id,
      player_name = receiver_player_name
    ) %>%
    # Sort by receiving yards (descending)
    arrange(desc(rec_yards))
  
  message(glue("Calculated receiving stats for {nrow(rec_stats)} players"))
  
  return(rec_stats)
}
