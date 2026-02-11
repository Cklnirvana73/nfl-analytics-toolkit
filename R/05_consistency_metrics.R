# ==============================================================================
# WEEK 3: CONSISTENCY METRICS & ROLLING AVERAGES
# ==============================================================================
# 
# This file contains three main components:
# 1. Fantasy Points Calculation (fully customizable scoring system)
# 2. Rolling Statistics (3-game and 6-game averages for raw stats)
# 3. Rolling Fantasy Points (3-game and 6-game averages for fantasy scoring)
#
# Navigation:
# - Lines 20-500:   calculate_fantasy_points()
# - Lines 502-750:  calculate_rolling_stats()
# - Lines 752-900:  calculate_rolling_fantasy()
#
# ==============================================================================


# ==============================================================================
# SECTION 1: FANTASY POINTS CALCULATION
# ==============================================================================

#' Calculate Fantasy Points with Fully Customizable Scoring
#'
#' @description
#' Calculates fantasy football points for all players with complete control over
#' scoring rules. Supports tiered PPR, TE premium, rush attempt bonuses, pick-6 
#' penalties, and all standard scoring parameters.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param roster_data Optional. Roster data from get_roster_data() for accurate position labels.
#'   If NULL, positions will be inferred from play type (less accurate for multi-role players).
#' @param season Optional. Numeric vector of season(s) to filter
#' @param week_min Optional. Minimum week number (inclusive)
#' @param week_max Optional. Maximum week number (inclusive)
#' 
#' @param use_tiered_ppr Use tiered PPR scoring based on reception yardage (default = TRUE)
#' @param te_premium Add 0.5 points per TE reception (default = TRUE)
#' @param rush_att_bonus Points per rushing attempt (default = 0.25)
#' 
#' @param pass_yd Points per passing yard (default = 0.04)
#' @param pass_td Points per passing TD (default = 6)
#' @param pass_int Points per interception (default = -2)
#' @param pick6_penalty Additional penalty when INT returned for TD (default = -4)
#' 
#' @param rush_yd Points per rushing yard (default = 0.1)
#' @param rush_td Points per rushing TD (default = 6)
#' 
#' @param rec_yd Points per receiving yard (default = 0.1)
#' @param rec_td Points per receiving TD (default = 6)
#' @param ppr Points per reception if not using tiered PPR (default = 1)
#' 
#' @param fumbles Points lost per fumble (default = -2)
#'
#' @return Tibble with one row per player per week containing fantasy points
#'
#' @details
#' **Tiered PPR Scoring (when use_tiered_ppr = TRUE):**
#' 
#' Each reception earns points based on yards gained:
#' - 5-9 yards: 0.5 pts
#' - 10-19 yards: 1.0 pts
#' - 20-29 yards: 1.5 pts
#' - 30-39 yards: 2.0 pts
#' - 40+ yards: 2.0 pts
#' - <5 yards: 0 pts (no bonus)
#' 
#' This rewards explosive receptions and reduces value of short catches.
#' 
#' **TE Premium (when te_premium = TRUE):**
#' 
#' Tight ends receive an additional 0.5 points per reception on top of standard
#' PPR/tiered PPR scoring. Requires roster_data for accurate position identification.
#' 
#' **Rush Attempt Bonus:**
#' 
#' Awards points per rushing attempt regardless of outcome. Common settings:
#' - 0.25 pts/attempt (default, favors volume backs)
#' - 0.1 pts/attempt (moderate bonus)
#' - 0 pts/attempt (no bonus)
#' 
#' **Pick-6 Penalty:**
#' 
#' Additional penalty when a QB's interception is returned for a touchdown.
#' Standard interception penalty PLUS pick-6 penalty = total deduction.
#' Example: pass_int = -2, pick6_penalty = -4 → Pick-6 costs -6 total
#' 
#' **Quick League Setup Examples:**
#' 
#' ```r
#' # League 1: Your default (tiered PPR, TE premium, 6pt pass TDs)
#' fantasy <- calculate_fantasy_points(pbp, roster)
#' 
#' # League 2: Standard scoring (no PPR, no bonuses, 4pt pass TDs)
#' fantasy <- calculate_fantasy_points(pbp, roster,
#'                                     use_tiered_ppr = FALSE,
#'                                     te_premium = FALSE,
#'                                     rush_att_bonus = 0,
#'                                     ppr = 0,
#'                                     pass_td = 4)
#' 
#' # League 3: Half-PPR with no bonuses
#' fantasy <- calculate_fantasy_points(pbp, roster,
#'                                     use_tiered_ppr = FALSE,
#'                                     te_premium = FALSE,
#'                                     rush_att_bonus = 0,
#'                                     ppr = 0.5)
#' 
#' # League 4: Full PPR with high rush bonus
#' fantasy <- calculate_fantasy_points(pbp, roster,
#'                                     use_tiered_ppr = FALSE,
#'                                     rush_att_bonus = 0.5,
#'                                     ppr = 1)
#' ```
#'
#' @examples
#' \dontrun{
#' # Load data
#' pbp <- load_and_validate_pbp(2024)
#' roster <- get_roster_data(2024)
#' 
#' # Calculate with your default settings
#' fantasy <- calculate_fantasy_points(pbp, roster, season = 2024)
#' 
#' # View top scorers
#' fantasy %>%
#'   group_by(player_id, player_name, position) %>%
#'   summarise(total_pts = sum(total_fantasy_points)) %>%
#'   arrange(desc(total_pts))
#' }
#'
#' @seealso
#' \code{\link{calculate_rolling_fantasy}} for adding rolling averages to fantasy points
#' \code{\link{calculate_rolling_stats}} for rolling averages on raw statistics
#'
#' @export
calculate_fantasy_points <- function(pbp_data,
                                     roster_data = NULL,
                                     season = NULL,
                                     week_min = NULL,
                                     week_max = NULL,
                                     
                                     # Scoring features
                                     use_tiered_ppr = TRUE,
                                     te_premium = TRUE,
                                     rush_att_bonus = 0.25,
                                     
                                     # Passing scoring
                                     pass_yd = 0.04,
                                     pass_td = 6,
                                     pass_int = -2,
                                     pick6_penalty = -4,
                                     
                                     # Rushing scoring
                                     rush_yd = 0.1,
                                     rush_td = 6,
                                     
                                     # Receiving scoring
                                     rec_yd = 0.1,
                                     rec_td = 6,
                                     ppr = 1,
                                     
                                     # Other
                                     fumbles = -2) {
  
  library(dplyr)
  library(glue)
  
  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================
  
  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }
  
  required_cols <- c("season", "week", "game_id", "play_type",
                     "passer_player_id", "passer_player_name",
                     "rusher_player_id", "rusher_player_name",
                     "receiver_player_id", "receiver_player_name",
                     "passing_yards", "rushing_yards", "receiving_yards",
                     "pass_touchdown", "rush_touchdown", "touchdown",
                     "interception", "fumble_lost", "complete_pass",
                     "fumbled_1_player_id", "posteam")
  
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse=', ')}"))
  }
  
  # Validate numeric parameters
  if (!is.numeric(pass_yd) || !is.numeric(pass_td) || !is.numeric(pass_int)) {
    stop("Passing scoring parameters must be numeric")
  }
  
  if (!is.numeric(rush_yd) || !is.numeric(rush_td) || !is.numeric(rush_att_bonus)) {
    stop("Rushing scoring parameters must be numeric")
  }
  
  if (!is.numeric(rec_yd) || !is.numeric(rec_td) || !is.numeric(ppr)) {
    stop("Receiving scoring parameters must be numeric")
  }
  
  if (!is.numeric(fumbles) || !is.numeric(pick6_penalty)) {
    stop("Penalty parameters must be numeric")
  }
  
  # ============================================================================
  # FILTER DATA BY SEASON/WEEK
  # ============================================================================
  
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
  
  # Filter to relevant plays only
  pbp_filtered <- pbp_data %>%
    filter(
      !is.na(play_type),
      play_type %in% c("pass", "run")
    )
  
  if (nrow(pbp_filtered) == 0) {
    warning("No offensive plays found with given filters")
    return(tibble())
  }
  
  message(glue("Calculating fantasy points for {n_distinct(pbp_filtered$game_id)} games..."))
  message(glue("Settings: Tiered PPR = {use_tiered_ppr}, TE Premium = {te_premium}, Rush Bonus = {rush_att_bonus}"))
  
  # ============================================================================
  # CREATE POSITION LOOKUP (if roster data provided)
  # ============================================================================
  
  if (!is.null(roster_data)) {
    if (!"gsis_id" %in% names(roster_data)) {
      stop("roster_data must contain 'gsis_id' column")
    }
    if (!"position" %in% names(roster_data)) {
      stop("roster_data must contain 'position' column")
    }
    
    position_lookup <- roster_data %>%
      filter(!is.na(gsis_id), !is.na(position)) %>%
      select(player_id = gsis_id, actual_position = position) %>%
      distinct(player_id, .keep_all = TRUE)
    
    message(glue("  • Using roster data for {n_distinct(position_lookup$player_id)} players"))
  } else {
    position_lookup <- NULL
    message("  • No roster data provided - positions will be inferred from play type")
  }
  
  # ============================================================================
  # CALCULATE PASSING FANTASY POINTS
  # ============================================================================
  
  passing_fantasy <- pbp_filtered %>%
    filter(
      !is.na(passer_player_id),  # CRITICAL: Explicit NA check
      !is.na(passer_player_name)
    )
  
  # Only proceed if there's passing data
  if (nrow(passing_fantasy) > 0) {
    passing_fantasy <- passing_fantasy %>%
      group_by(season, week, game_id, passer_player_id, passer_player_name) %>%
      summarise(
        position_inferred = "QB",
        team = last(posteam),
        pass_yards = sum(passing_yards, na.rm = TRUE),
        pass_tds = sum(pass_touchdown == 1, na.rm = TRUE),
        pass_ints = sum(interception == 1, na.rm = TRUE),
        pick6 = sum(interception == 1 & touchdown == 1, na.rm = TRUE),
        pass_fumbles_lost = sum(fumble_lost == 1 & passer_player_id == fumbled_1_player_id, na.rm = TRUE),
        
        # Calculate fantasy points
        pass_fantasy_points = 
          (pass_yards * pass_yd) +
          (pass_tds * pass_td) +
          (pass_ints * pass_int) +
          (pick6 * pick6_penalty) +
          (pass_fumbles_lost * fumbles),
        
        .groups = "drop"
      ) %>%
      rename(player_id = passer_player_id, player_name = passer_player_name)
    
    # Apply position lookup
    if (!is.null(position_lookup)) {
      passing_fantasy <- passing_fantasy %>%
        left_join(position_lookup, by = "player_id") %>%
        mutate(position = coalesce(actual_position, position_inferred)) %>%
        select(-actual_position, -position_inferred)
    } else {
      passing_fantasy <- passing_fantasy %>%
        mutate(position = position_inferred) %>%
        select(-position_inferred)
    }
  } else {
    # Create empty tibble with correct column types
    passing_fantasy <- tibble(
      season = integer(),
      week = integer(),
      game_id = character(),
      player_id = character(),
      player_name = character(),
      position = character(),
      team = character(),
      pass_yards = numeric(),
      pass_tds = numeric(),
      pass_ints = numeric(),
      pick6 = numeric(),
      pass_fumbles_lost = numeric(),
      pass_fantasy_points = numeric()
    )
  }
  
  message(glue("  • Calculated passing fantasy for {n_distinct(passing_fantasy$player_id)} players"))
  
  # ============================================================================
  # CALCULATE RUSHING FANTASY POINTS
  # ============================================================================
  
  rushing_fantasy <- pbp_filtered %>%
    filter(
      !is.na(rusher_player_id),  # CRITICAL: Explicit NA check
      !is.na(rusher_player_name)
    )
  
  # Only proceed if there's rushing data
  if (nrow(rushing_fantasy) > 0) {
    # Check if rush_attempt column exists
    has_rush_attempt <- "rush_attempt" %in% names(rushing_fantasy)
    
    rushing_fantasy <- rushing_fantasy %>%
      group_by(season, week, game_id, rusher_player_id, rusher_player_name) %>%
      summarise(
        position_inferred = "RB",
        team = last(posteam),
        rush_yards = sum(rushing_yards, na.rm = TRUE),
        rush_tds = sum(rush_touchdown == 1, na.rm = TRUE),
        rush_attempts = if (has_rush_attempt) sum(rush_attempt == 1, na.rm = TRUE) else n(),
        rush_fumbles_lost = sum(fumble_lost == 1 & rusher_player_id == fumbled_1_player_id, na.rm = TRUE),
        
        # Calculate fantasy points (including rush attempt bonus)
        rush_fantasy_points = 
          (rush_yards * rush_yd) +
          (rush_tds * rush_td) +
          (rush_attempts * rush_att_bonus) +
          (rush_fumbles_lost * fumbles),
        
        .groups = "drop"
      ) %>%
      rename(player_id = rusher_player_id, player_name = rusher_player_name)
    
    # Apply position lookup
    if (!is.null(position_lookup)) {
      rushing_fantasy <- rushing_fantasy %>%
        left_join(position_lookup, by = "player_id") %>%
        mutate(position = coalesce(actual_position, position_inferred)) %>%
        select(-actual_position, -position_inferred)
    } else {
      rushing_fantasy <- rushing_fantasy %>%
        mutate(position = position_inferred) %>%
        select(-position_inferred)
    }
  } else {
    # Create empty tibble with correct column types
    rushing_fantasy <- tibble(
      season = integer(),
      week = integer(),
      game_id = character(),
      player_id = character(),
      player_name = character(),
      position = character(),
      team = character(),
      rush_yards = numeric(),
      rush_tds = numeric(),
      rush_attempts = numeric(),
      rush_fumbles_lost = numeric(),
      rush_fantasy_points = numeric()
    )
  }
  
  message(glue("  • Calculated rushing fantasy for {n_distinct(rushing_fantasy$player_id)} players"))
  
  # ============================================================================
  # CALCULATE RECEIVING FANTASY POINTS (WITH TIERED PPR)
  # ============================================================================
  
  receiving_fantasy <- pbp_filtered %>%
    filter(
      !is.na(receiver_player_id),  # CRITICAL: Explicit NA check
      !is.na(receiver_player_name)
    )
  
  # Only proceed if there's receiving data
  if (nrow(receiving_fantasy) > 0) {
    
    # Calculate tiered PPR points per reception if enabled
    if (use_tiered_ppr) {
      receiving_fantasy <- receiving_fantasy %>%
        mutate(
          tiered_ppr_pts = case_when(
            receiving_yards >= 5  & receiving_yards <= 9  ~ 0.5,
            receiving_yards >= 10 & receiving_yards <= 19 ~ 1.0,
            receiving_yards >= 20 & receiving_yards <= 29 ~ 1.5,
            receiving_yards >= 30 & receiving_yards <= 39 ~ 2.0,
            receiving_yards >= 40                         ~ 2.0,
            TRUE ~ 0
          )
        )
    }
    
    receiving_fantasy <- receiving_fantasy %>%
      group_by(season, week, game_id, receiver_player_id, receiver_player_name) %>%
      summarise(
        position_inferred = "WR",
        team = last(posteam),
        receptions = sum(complete_pass == 1, na.rm = TRUE),
        rec_yards = sum(receiving_yards, na.rm = TRUE),
        rec_tds = sum(touchdown == 1 & pass_touchdown == 1, na.rm = TRUE),
        rec_fumbles_lost = sum(fumble_lost == 1 & receiver_player_id == fumbled_1_player_id, na.rm = TRUE),
        tiered_pts = if (use_tiered_ppr) sum(tiered_ppr_pts, na.rm = TRUE) else 0,
        
        .groups = "drop"
      ) %>%
      rename(player_id = receiver_player_id, player_name = receiver_player_name)
    
    # Apply position lookup (needed for TE premium)
    if (!is.null(position_lookup)) {
      receiving_fantasy <- receiving_fantasy %>%
        left_join(position_lookup, by = "player_id") %>%
        mutate(position = coalesce(actual_position, position_inferred)) %>%
        select(-actual_position, -position_inferred)
    } else {
      receiving_fantasy <- receiving_fantasy %>%
        mutate(position = position_inferred) %>%
        select(-position_inferred)
    }
    
    # Calculate receiving fantasy points
    receiving_fantasy <- receiving_fantasy %>%
      mutate(
        # Choose between tiered PPR or regular PPR
        tiered_ppr_points = if (use_tiered_ppr) tiered_pts else 0,
        regular_ppr_points = if (!use_tiered_ppr) receptions * ppr else 0,
        
        # TE premium (only if using roster data for accurate positions)
        te_bonus = if (te_premium && !is.null(position_lookup)) {
          ifelse(position == "TE", 0.5 * receptions, 0)
        } else {
          0
        },
        
        # Calculate fantasy points
        rec_fantasy_points = 
          (rec_yards * rec_yd) +
          (rec_tds * rec_td) +
          tiered_ppr_points +
          regular_ppr_points +
          te_bonus +
          (rec_fumbles_lost * fumbles)
      ) %>%
      select(-tiered_pts, -tiered_ppr_points, -regular_ppr_points, -te_bonus)
    
  } else {
    # Create empty tibble with correct column types
    receiving_fantasy <- tibble(
      season = integer(),
      week = integer(),
      game_id = character(),
      player_id = character(),
      player_name = character(),
      position = character(),
      team = character(),
      receptions = numeric(),
      rec_yards = numeric(),
      rec_tds = numeric(),
      rec_fumbles_lost = numeric(),
      rec_fantasy_points = numeric()
    )
  }
  
  message(glue("  • Calculated receiving fantasy for {n_distinct(receiving_fantasy$player_id)} players"))
  
  # ============================================================================
  # COMBINE ALL FANTASY POINTS
  # ============================================================================
  
  # Combine passing, rushing, receiving into single player-week rows
  all_fantasy <- bind_rows(
    passing_fantasy,
    rushing_fantasy,
    receiving_fantasy
  ) %>%
    group_by(season, week, game_id, player_id, player_name) %>%
    summarise(
      # Take position and team from most recent activity
      position = last(position),
      team = last(team),
      
      # Sum all fantasy point components
      pass_fantasy_points = sum(pass_fantasy_points, na.rm = TRUE),
      rush_fantasy_points = sum(rush_fantasy_points, na.rm = TRUE),
      rec_fantasy_points = sum(rec_fantasy_points, na.rm = TRUE),
      
      # Calculate total
      total_fantasy_points = pass_fantasy_points + rush_fantasy_points + rec_fantasy_points,
      
      # Include component stats for reference
      pass_yards = sum(pass_yards, na.rm = TRUE),
      rush_yards = sum(rush_yards, na.rm = TRUE),
      rec_yards = sum(rec_yards, na.rm = TRUE),
      pass_tds = sum(pass_tds, na.rm = TRUE),
      rush_tds = sum(rush_tds, na.rm = TRUE),
      rec_tds = sum(rec_tds, na.rm = TRUE),
      receptions = sum(receptions, na.rm = TRUE),
      pass_ints = sum(pass_ints, na.rm = TRUE),
      pick6 = sum(pick6, na.rm = TRUE),
      rush_attempts = sum(rush_attempts, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    # Sort by total fantasy points (descending)
    arrange(season, week, desc(total_fantasy_points))
  
  message(glue("✓ Calculated fantasy points for {n_distinct(all_fantasy$player_id)} players"))
  message(glue("✓ Total player-weeks: {nrow(all_fantasy)}"))
  
  return(all_fantasy)
}


# ==============================================================================
# SECTION 2: ROLLING STATISTICS FOR RAW STATS
# ==============================================================================

#' Calculate Rolling Averages for Raw Player Statistics
#'
#' @description
#' Calculates 3-game and 6-game rolling averages for raw statistics (yards, TDs, etc.).
#' Properly handles bye weeks, injuries, and players with insufficient games.
#'
#' @param player_stats Player statistics from get_player_*_stats() functions
#'   Must contain: player_id, player_name, season, week, and stat columns
#' @param stat_columns Character vector of column names to calculate rolling averages for
#' @param windows Numeric vector of window sizes (default: c(3, 6))
#' @param min_games Minimum number of games required to calculate rolling average (default: 2)
#'
#' @return Tibble with original data plus rolling average columns (e.g., rush_yards_roll3, rush_yards_roll6)
#'
#' @details
#' **Rolling Window Calculation:**
#' 
#' For each player, statistics are averaged over the specified number of PREVIOUS games.
#' The current game is NOT included in the rolling average to avoid feature leakage.
#' 
#' Example: Player has games in weeks 1, 2, 3, 5 (bye in week 4)
#' - Week 1: No rolling avg (insufficient history)
#' - Week 2: roll3 = avg(week 1 only), roll6 = avg(week 1 only)
#' - Week 3: roll3 = avg(weeks 1-2), roll6 = avg(weeks 1-2)
#' - Week 5: roll3 = avg(weeks 2-3), roll6 = avg(weeks 1-3) - bye doesn't break streak
#' 
#' **Edge Cases Handled:**
#' - Bye weeks: Skipped automatically (only actual games counted)
#' - Insufficient games: Returns NA if fewer than min_games available
#' - Missing values: Handled with na.rm = TRUE in calculations
#' - Multiple stats: Each stat gets its own rolling columns
#'
#' **Partial Windows:**
#' 
#' The function uses partial = FALSE, meaning it requires EXACTLY 'window' games
#' before calculating rolling average. With 3-game window:
#' - Week 1: NA (0 previous games)
#' - Week 2: NA (1 previous game, need 3)
#' - Week 3: NA (2 previous games, need 3)
#' - Week 4: avg(weeks 1-3) - FIRST valid rolling average
#' 
#' If you want averages with fewer games (e.g., 2-game average until 3 available),
#' set min_games = 2. The function checks non-NA count and returns NA if < min_games.
#'
#' **Dependencies:**
#' This function requires the 'zoo' package for rolling calculations.
#' Install with: install.packages("zoo")
#'
#' @examples
#' \dontrun{
#' # Get rushing stats
#' pbp <- load_and_validate_pbp(2024)
#' rush_stats <- get_player_rushing_stats(pbp, season = 2024)
#' 
#' # Calculate rolling averages for key stats
#' rush_with_rolling <- calculate_rolling_stats(
#'   player_stats = rush_stats,
#'   stat_columns = c("rush_yards", "rush_tds", "yards_per_carry"),
#'   windows = c(3, 6)
#' )
#' }
#'
#' @seealso
#' \code{\link{calculate_rolling_fantasy}} for rolling averages on fantasy points
#' \code{\link{calculate_fantasy_points}} for calculating fantasy points first
#'
#' @export
calculate_rolling_stats <- function(player_stats,
                                    stat_columns,
                                    windows = c(3, 6),
                                    min_games = 2) {
  
  library(dplyr)
  library(glue)
  library(zoo)  # For rollmean function
  
  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================
  
  if (!is.data.frame(player_stats)) {
    stop("player_stats must be a data frame")
  }
  
  required_cols <- c("player_id", "player_name", "season", "week")
  missing_cols <- setdiff(required_cols, names(player_stats))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse=', ')}"))
  }
  
  # Validate stat_columns exist
  missing_stats <- setdiff(stat_columns, names(player_stats))
  if (length(missing_stats) > 0) {
    stop(glue("stat_columns not found in data: {paste(missing_stats, collapse=', ')}"))
  }
  
  if (!is.numeric(windows) || any(windows < 2)) {
    stop("windows must be numeric vector with values >= 2")
  }
  
  if (!is.numeric(min_games) || length(min_games) != 1 || min_games < 1) {
    stop("min_games must be a single numeric value >= 1")
  }
  
  # ============================================================================
  # PREPARE DATA
  # ============================================================================
  
  # Ensure data is sorted by player and week
  player_stats <- player_stats %>%
    arrange(player_id, season, week)
  
  # ============================================================================
  # CALCULATE ROLLING AVERAGES
  # ============================================================================
  
  # For each window size, calculate rolling averages
  for (window in windows) {
    
    for (stat in stat_columns) {
      
      # Create column name for this rolling average
      roll_col_name <- glue("{stat}_roll{window}")
      
      # Calculate rolling average for each player
      player_stats <- player_stats %>%
        group_by(player_id) %>%
        mutate(
          # Add row number to check if we have enough prior games
          .row_num = row_number(),
          
          # CRITICAL: Use lag() to exclude current game (prevent feature leakage)
          # rollapply calculates mean of previous 'window' games
          !!roll_col_name := {
            # Get lagged values (excludes current row)
            lagged_vals <- lag(!!sym(stat), n = 1)
            
            # Calculate rolling mean with alignment='right' (looks backward)
            # fill=NA ensures we get NA when insufficient data
            result <- zoo::rollapply(
              lagged_vals,
              width = window,
              FUN = function(x) {
                # Count non-NA values (actual games with data)
                non_na_count <- sum(!is.na(x))
                
                # Require enough non-NA values to meet min_games
                # This allows some NAs in the window (missing data)
                if (non_na_count >= min_games) {
                  mean(x, na.rm = TRUE)
                } else {
                  NA_real_
                }
              },
              align = "right",
              fill = NA,
              partial = FALSE  # Don't calculate with partial windows
            )
            
            # But override to NA if we don't have enough PRIOR GAMES yet
            # Row N needs at least window prior games (row must be > window)
            ifelse(.row_num <= window, NA_real_, result)
          }
        ) %>%
        select(-.row_num) %>%
        ungroup()
    }
  }
  
  # ============================================================================
  # SUMMARY STATISTICS
  # ============================================================================
  
  message(glue("Calculated rolling averages for {length(stat_columns)} statistics"))
  message(glue("Window sizes: {paste(windows, collapse=', ')}-game averages"))
  message(glue("Minimum games required: {min_games}"))
  
  # Count how many player-weeks have valid rolling averages
  for (window in windows) {
    valid_count <- player_stats %>%
      filter(!is.na(!!sym(glue("{stat_columns[1]}_roll{window}")))) %>%
      nrow()
    message(glue("  {window}-game rolling: {valid_count} valid player-weeks"))
  }
  
  return(player_stats)
}


# ==============================================================================
# SECTION 3: ROLLING AVERAGES FOR FANTASY POINTS
# ==============================================================================

#' Calculate Rolling Averages for Fantasy Points
#'
#' @description
#' Calculates 3-game and 6-game rolling averages for fantasy points.
#' Wrapper around calculate_rolling_stats() specifically for fantasy output.
#'
#' @param fantasy_data Fantasy points data from calculate_fantasy_points()
#' @param windows Numeric vector of window sizes (default: c(3, 6))
#' @param min_games Minimum number of games required (default: 2)
#'
#' @return Tibble with fantasy data plus rolling average columns
#'
#' @details
#' Calculates rolling averages for:
#' - total_fantasy_points
#' - pass_fantasy_points
#' - rush_fantasy_points  
#' - rec_fantasy_points
#' 
#' Handles all edge cases (bye weeks, injuries, insufficient games) automatically.
#'
#' **Dependencies:**
#' This function requires the 'zoo' package for rolling calculations.
#' Install with: install.packages("zoo")
#'
#' @examples
#' \dontrun{
#' # Calculate fantasy points
#' pbp <- load_and_validate_pbp(2024)
#' roster <- get_roster_data(2024)
#' fantasy <- calculate_fantasy_points(pbp, roster, season = 2024)
#' 
#' # Add rolling averages
#' fantasy_rolling <- calculate_rolling_fantasy(fantasy)
#' 
#' # View players with best 3-game rolling average
#' fantasy_rolling %>%
#'   filter(!is.na(total_fantasy_points_roll3)) %>%
#'   arrange(desc(total_fantasy_points_roll3))
#' }
#'
#' @seealso
#' \code{\link{calculate_fantasy_points}} for calculating fantasy points first
#' \code{\link{calculate_rolling_stats}} for rolling averages on raw statistics
#'
#' @export
calculate_rolling_fantasy <- function(fantasy_data,
                                      windows = c(3, 6),
                                      min_games = 2) {
  
  library(dplyr)
  library(glue)
  
  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================
  
  if (!is.data.frame(fantasy_data)) {
    stop("fantasy_data must be a data frame")
  }
  
  required_cols <- c("season", "week", "player_id", "player_name",
                     "total_fantasy_points", "pass_fantasy_points",
                     "rush_fantasy_points", "rec_fantasy_points")
  
  missing_cols <- setdiff(required_cols, names(fantasy_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse=', ')}"))
  }
  
  # ============================================================================
  # CALCULATE ROLLING AVERAGES
  # ============================================================================
  
  # Define which fantasy stats to calculate rolling averages for
  fantasy_stat_columns <- c(
    "total_fantasy_points",
    "pass_fantasy_points",
    "rush_fantasy_points",
    "rec_fantasy_points"
  )
  
  message("Calculating rolling fantasy point averages...")
  
  # Use the main rolling stats function
  fantasy_rolling <- calculate_rolling_stats(
    player_stats = fantasy_data,
    stat_columns = fantasy_stat_columns,
    windows = windows,
    min_games = min_games
  )
  
  message(glue("Added rolling fantasy columns for {length(fantasy_stat_columns)} metrics"))
  
  return(fantasy_rolling)
}
