#' Get Player Rushing Statistics
#'
#' @description
#' Aggregates play-by-play data to calculate rushing statistics for each player.
#' Returns volume, production, and efficiency metrics. Excludes QB kneels by default
#' for accurate efficiency metrics.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric vector of season(s) to filter (e.g., 2024 or 2023:2024)
#' @param week_min Optional. Minimum week number (inclusive)
#' @param week_max Optional. Maximum week number (inclusive)
#' @param include_qbs Logical. If FALSE (default), filters out QBs from results. Set to TRUE
#'   to include QB rushing stats (useful for mobile QBs like Lamar Jackson). Default: FALSE
#'
#' @return Tibble with one row per player containing rushing statistics
#'
#' @details
#' **NFL Context:**
#' - Excludes QB kneels (victory formation) from all statistics as they are intentional negative plays
#' - By default, filters out QBs to provide clean RB rankings
#' - Set `include_qbs = TRUE` to analyze QB rushing (Lamar Jackson, Josh Allen, etc.)
#' - Explosive rush rate uses 15+ yards threshold (standard NFL analytics definition)
#'
#' **Dependencies:**
#' Requires 'nflreadr' package for position filtering when include_qbs = FALSE.
#' Install with: install.packages("nflreadr")
#'
#' @examples
#' \dontrun{
#' # Load data
#' pbp <- load_and_validate_pbp(2024)
#' 
#' # Get full season stats (excludes QBs)
#' rush_stats <- get_player_rushing_stats(pbp, season = 2024)
#' 
#' # Get stats for specific week range
#' rush_stats_early <- get_player_rushing_stats(pbp, season = 2024, week_max = 10)
#' 
#' # Include QB rushing (for mobile QB analysis)
#' rush_with_qbs <- get_player_rushing_stats(pbp, season = 2024, include_qbs = TRUE)
#' }
#'
#' @seealso
#' \code{\link{get_player_passing_stats}} for QB passing statistics
#' \code{\link{get_player_receiving_stats}} for receiver statistics
#'
#' @export
get_player_rushing_stats <- function(pbp_data, 
                                     season = NULL, 
                                     week_min = NULL, 
                                     week_max = NULL,
                                     include_qbs = FALSE) {
  
  library(dplyr)
  library(glue)
  
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
  
  # Filter to rushing plays only and exclude NAs
  # CRITICAL NFL FIX: Exclude QB kneels (victory formation - intentional negative plays)
  rushing_plays <- pbp_data %>%
    filter(
      play_type == "run",
      !is.na(rusher_player_id),
      !is.na(rusher_player_name)
    )
  
  # Additional NFL filter: Remove QB kneels if play_type column has that value
  if ("qb_kneel" %in% unique(rushing_plays$play_type)) {
    rushing_plays <- rushing_plays %>%
      filter(play_type != "qb_kneel")
    message("Filtered out QB kneel downs (victory formation)")
  }
  
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
      
      # Context metrics (NFL standard: explosive rush = 15+ yards)
      explosive_rush_rate = mean(rushing_yards >= 15, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    # Rename for clarity
    rename(
      player_id = rusher_player_id,
      player_name = rusher_player_name
    )
  
  # CRITICAL FIX: Filter out QBs unless explicitly requested
  if (!include_qbs) {
    if (!requireNamespace("nflreadr", quietly = TRUE)) {
      warning(paste(
        "nflreadr package not available - cannot filter QBs.",
        "Install with: install.packages('nflreadr')",
        "QB rushing stats will be included in results."
      ))
    } else {
      # Get roster data for position filtering
      seasons_to_load <- if (!is.null(season)) season else unique(pbp_data$season)
      
      tryCatch({
        roster <- nflreadr::load_rosters(seasons_to_load)
        
        rows_before <- nrow(rush_stats)
        
        # Join and filter out QBs
        rush_stats <- rush_stats %>%
          left_join(
            roster %>% 
              filter(!is.na(gsis_id), !is.na(position)) %>%
              select(gsis_id, position) %>%
              distinct(gsis_id, .keep_all = TRUE),
            by = c("player_id" = "gsis_id")
          ) %>%
          # Keep only non-QBs (or rows with missing position - edge cases like defensive TDs)
          filter(position != "QB" | is.na(position)) %>%
          select(-position)  # Remove position column after filtering
        
        rows_after <- nrow(rush_stats)
        qbs_filtered <- rows_before - rows_after
        
        if (qbs_filtered > 0) {
          message(glue("Filtered out {qbs_filtered} QBs from rushing statistics"))
        }
      }, error = function(e) {
        warning(glue("Could not load roster data: {e$message}. QBs will be included in results."))
      })
    }
  } else {
    message("Including QB rushing statistics (include_qbs = TRUE)")
  }
  
  # Sort by total yards (descending)
  rush_stats <- rush_stats %>%
    arrange(desc(rush_yards))
  
  message(glue("Calculated rushing stats for {nrow(rush_stats)} players"))
  
  return(rush_stats)
}


#' Get Player Passing Statistics
#'
#' @description
#' Aggregates play-by-play data to calculate passing statistics for each QB.
#' Returns volume, production, efficiency, and advanced metrics including CORRECTED
#' passer rating calculation.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric vector of season(s) to filter
#' @param week_min Optional. Minimum week number (inclusive)
#' @param week_max Optional. Maximum week number (inclusive)
#'
#' @return Tibble with one row per QB containing passing statistics
#'
#' @details
#' **NFL Context:**
#' - Passer rating ranges from 0.0 to 158.3 (maximum possible)
#' - League average typically 85-95
#' - Below 70 = poor, 70-85 = below average, 85-95 = average, 95-105 = above average, 105+ = elite
#' - Includes QB spikes and kneels in attempt count (per NFL official stats)
#' - CPOE (Completion Percentage Over Expected) separates QB accuracy from receiver performance
#'
#' **Passer Rating Formula:**
#' Each component capped at 2.375, then: ((A + B + C + D) / 6) * 100
#' - Component A: (Comp% - 30%) * 5
#' - Component B: (YPA - 3) * 0.25
#' - Component C: (TD% * 20)
#' - Component D: 2.375 - (INT% * 25)
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2024)
#' pass_stats <- get_player_passing_stats(pbp, season = 2024)
#' 
#' # View top QBs by passer rating
#' pass_stats %>%
#'   filter(attempts >= 100) %>%
#'   arrange(desc(passer_rating)) %>%
#'   head(10)
#' }
#'
#' @seealso
#' \code{\link{get_player_rushing_stats}} for rushing statistics
#' \code{\link{get_player_receiving_stats}} for receiver statistics
#'
#' @export
get_player_passing_stats <- function(pbp_data, 
                                     season = NULL, 
                                     week_min = NULL, 
                                     week_max = NULL) {
  
  library(dplyr)
  library(glue)
  
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
      pass_tds = sum(pass_touchdown == 1, na.rm = TRUE),
      interceptions = sum(interception == 1, na.rm = TRUE),
      
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
    # CRITICAL FIX: Correct NFL Passer Rating Formula
    mutate(
      passer_rating = ifelse(
        attempts > 0,
        {
          # Calculate each component individually (each capped at 2.375)
          # Component A: Completion percentage
          a <- pmin(pmax((completion_pct - 0.3) * 5, 0), 2.375)
          
          # Component B: Yards per attempt
          b <- pmin(pmax((yards_per_attempt - 3) * 0.25, 0), 2.375)
          
          # Component C: Touchdown percentage
          c <- pmin(pmax((pass_tds / attempts) * 20, 0), 2.375)
          
          # Component D: Interception percentage (inverted - fewer is better)
          d <- pmin(pmax(2.375 - (interceptions / attempts * 25), 0), 2.375)
          
          # Sum components, divide by 6, multiply by 100
          # Final rating capped at 158.3 (perfect rating)
          rating <- ((a + b + c + d) / 6) * 100
          pmin(rating, 158.3)
        },
        NA_real_
      )
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
#' Returns volume, production, efficiency, and CORRECTED target share metrics.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric vector of season(s) to filter
#' @param week_min Optional. Minimum week number (inclusive)
#' @param week_max Optional. Maximum week number (inclusive)
#'
#' @return Tibble with one row per receiver containing receiving statistics
#'
#' @details
#' **NFL Context:**
#' - Target share = player targets / total team targets (correctly calculated at season level)
#' - Typical WR1: 20-28% target share
#' - Typical WR2: 12-18% target share
#' - Typical TE1: 15-22% target share
#' - Catch rate (receptions/targets) league average ~65%
#' - YPRR (Yards Per Route Run) is gold standard but requires tracking data
#'
#' **Metrics Included:**
#' - Volume: targets, receptions, games played
#' - Production: receiving yards, TDs, first downs
#' - Efficiency: catch rate, yards per reception, yards per target
#' - Advanced: EPA per target, YAC, air yards
#' - Context: target share (% of team's total targets)
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2024)
#' rec_stats <- get_player_receiving_stats(pbp, season = 2024)
#' 
#' # View WR1s by target share
#' rec_stats %>%
#'   filter(targets >= 50) %>%
#'   arrange(desc(target_share)) %>%
#'   head(10)
#' }
#'
#' @seealso
#' \code{\link{get_player_passing_stats}} for QB statistics
#' \code{\link{get_player_rushing_stats}} for rushing statistics
#'
#' @export
get_player_receiving_stats <- function(pbp_data, 
                                       season = NULL, 
                                       week_min = NULL, 
                                       week_max = NULL) {
  
  library(dplyr)
  library(glue)
  
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
  
  # CRITICAL FIX: Calculate team-level target totals FIRST (season-level, not game-level)
  team_season_targets <- receiving_plays %>%
    group_by(posteam) %>%
    summarise(team_total_targets = n(), .groups = "drop")
  
  # Calculate per-player statistics
  rec_stats <- receiving_plays %>%
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
      rec_tds = sum(pass_touchdown == 1, na.rm = TRUE),
      rec_first_downs = sum(first_down_pass == 1, na.rm = TRUE),
      
      # Efficiency metrics
      catch_rate = receptions / targets,
      yards_per_reception = ifelse(receptions > 0, rec_yards / receptions, NA_real_),
      yards_per_target = ifelse(targets > 0, rec_yards / targets, NA_real_),
      
      # Advanced metrics
      rec_epa = sum(epa, na.rm = TRUE),
      rec_epa_per_target = mean(epa, na.rm = TRUE),
      avg_yac = mean(yards_after_catch, na.rm = TRUE),
      avg_air_yards = mean(air_yards, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    # CRITICAL FIX: Join season-level team totals and calculate correct target share
    left_join(team_season_targets, by = c("recent_team" = "posteam")) %>%
    mutate(
      # Now correctly: player targets / team season total targets
      target_share = targets / team_total_targets
    ) %>%
    select(-team_total_targets) %>%  # Remove helper column
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
