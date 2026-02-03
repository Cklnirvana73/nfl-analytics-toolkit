#' Load and Validate NFL Play-by-Play Data
#'
#' @description
#' Loads nflfastR play-by-play data with error handling, validation, 
#' and local caching for improved performance.
#'
#' @param seasons Numeric vector of seasons to load (e.g., 2018:2023)
#' @param cache_dir Directory path for caching data (default: "data/cache")
#' @param force_reload Logical. If TRUE, downloads fresh data. If FALSE, uses cache. (default: FALSE)
#' @param validate Logical. If TRUE, runs data quality checks. (default: TRUE)
#'
#' @return A tibble containing play-by-play data for the specified seasons
#'
#' @examples
#' \dontrun{
#' # Load single season
#' pbp_2023 <- load_and_validate_pbp(2023)
#' 
#' # Load multiple seasons with caching
#' pbp_recent <- load_and_validate_pbp(2021:2023)
#' 
#' # Force fresh download
#' pbp_fresh <- load_and_validate_pbp(2023, force_reload = TRUE)
#' }
#'
#' @export
load_and_validate_pbp <- function(seasons, 
                                  cache_dir = "data/cache",
                                  force_reload = FALSE,
                                  validate = TRUE) {
  
  # Load required packages
  library(nflfastR)
  library(dplyr)
  library(glue)
  
  # Input validation
  if (!is.numeric(seasons)) {
    stop("seasons must be numeric (e.g., 2023 or 2018:2023)")
  }
  
  if (any(seasons < 1999 | seasons > as.numeric(format(Sys.Date(), "%Y")))) {
    stop(glue("seasons must be between 1999 and {format(Sys.Date(), '%Y')}"))
  }
  
  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    message(glue("Created cache directory: {cache_dir}"))
  }
  
  # Define cache file path
  cache_file <- file.path(cache_dir, glue("pbp_{min(seasons)}_{max(seasons)}.rds"))
  
  # Check if cached data exists and should be used
  if (file.exists(cache_file) && !force_reload) {
    message(glue("Loading cached data from: {cache_file}"))
    pbp <- readRDS(cache_file)
    message(glue("Loaded {nrow(pbp)} plays from cache"))
  } else {
    # Download fresh data
    message(glue("Downloading play-by-play data for seasons: {paste(seasons, collapse=', ')}"))
    message("This may take 30-60 seconds per season...")
    
    # Use try-catch for error handling
    pbp <- tryCatch({
      nflfastR::load_pbp(seasons)
    }, error = function(e) {
      stop(glue("Failed to download data: {e$message}"))
    })
    
    # Save to cache
    saveRDS(pbp, cache_file)
    message(glue("Cached data to: {cache_file}"))
    message(glue("Downloaded {nrow(pbp)} plays"))
  }
  
  # Run validation if requested
  if (validate) {
    message("Running data quality checks...")
    validation_results <- validate_pbp_quality(pbp)
    
    # Print validation summary
    cat("\n--- Data Quality Report ---\n")
    cat(glue("Total plays: {format(nrow(pbp), big.mark=',')}"), "\n")
    cat(glue("Seasons: {paste(unique(pbp$season), collapse=', ')}"), "\n")
    cat(glue("Weeks: {min(pbp$week, na.rm=TRUE)} to {max(pbp$week, na.rm=TRUE)}"), "\n")
    cat(glue("Date range: {min(pbp$game_date, na.rm=TRUE)} to {max(pbp$game_date, na.rm=TRUE)}"), "\n")
    cat("---------------------------\n\n")
    
    # Check for critical issues
    if (validation_results$has_critical_issues) {
      warning("Data quality issues detected! Review validation_results for details.")
    }
  }
  
  return(pbp)
}


#' Validate Play-by-Play Data Quality
#'
#' @description
#' Performs comprehensive data quality checks on play-by-play data.
#' Checks for missing values, unexpected data types, outliers, and anomalies.
#'
#' @param pbp Play-by-play dataframe from load_and_validate_pbp() or nflfastR::load_pbp()
#'
#' @return List containing validation results and flags
#'
#' @examples
#' \dontrun{
#' pbp <- load_pbp(2023)
#' validation <- validate_pbp_quality(pbp)
#' print(validation$summary)
#' }
#'
#' @export
validate_pbp_quality <- function(pbp) {
  
  library(dplyr)
  library(glue)
  
  # Initialize results list
  results <- list()
  results$has_critical_issues <- FALSE
  
  # Check 0: Empty data (PRIORITY 3 FIX)
  if (nrow(pbp) == 0) {
    results$has_critical_issues <- TRUE
    results$empty_data <- TRUE
    warning("Data is empty (0 rows)")
    
    # Return minimal summary
    results$summary <- list(
      total_plays = 0,
      seasons = numeric(0),
      weeks = c(NA, NA),
      date_range = c(NA, NA),
      teams = 0,
      games = 0
    )
    
    return(results)
  }
  
  # Check 1: Required columns exist
  required_cols <- c("game_id", "play_id", "posteam", "week", "season", 
                     "game_date", "desc", "play_type")
  
  missing_cols <- setdiff(required_cols, names(pbp))
  
  if (length(missing_cols) > 0) {
    results$has_critical_issues <- TRUE
    results$missing_columns <- missing_cols
    warning(glue("Missing required columns: {paste(missing_cols, collapse=', ')}"))
  } else {
    results$missing_columns <- NULL
  }
  
  # Check 2: Row count is reasonable
  min_plays_per_season <- 30000  # Typical season has 40,000+ plays
  seasons <- unique(pbp$season)
  plays_per_season <- pbp %>%
    group_by(season) %>%
    summarise(plays = n(), .groups = "drop")
  
  low_count_seasons <- plays_per_season %>%
    filter(plays < min_plays_per_season)
  
  if (nrow(low_count_seasons) > 0) {
    results$has_critical_issues <- TRUE
    results$low_play_count <- low_count_seasons
    warning(glue("Some seasons have unexpectedly low play counts: {paste(low_count_seasons$season, collapse=', ')}"))
  } else {
    results$low_play_count <- NULL
  }
  
  # Check 3: NA patterns in key columns (PRIORITY 2 FIX)
  key_cols <- c("posteam", "game_id", "play_id", "season", "week")
  existing_key_cols <- intersect(key_cols, names(pbp))  # Only use existing columns
  
  if (length(existing_key_cols) > 0) {
    na_counts <- sapply(pbp[existing_key_cols], function(x) sum(is.na(x)))
    
    high_na_cols <- names(na_counts[na_counts > nrow(pbp) * 0.5])  # More than 50% NA
    
    if (length(high_na_cols) > 0) {
      results$has_critical_issues <- TRUE
      results$high_na_columns <- high_na_cols
      warning(glue("Key columns with >50% missing data: {paste(high_na_cols, collapse=', ')}"))
    } else {
      results$high_na_columns <- NULL
    }
  } else {
    results$has_critical_issues <- TRUE
    results$high_na_columns <- key_cols  # All key columns missing
    warning("All key columns are missing from data")
  }
  
  # Check 4: Date range is valid
  min_date <- min(pbp$game_date, na.rm = TRUE)
  max_date <- max(pbp$game_date, na.rm = TRUE)
  
  if (min_date < as.Date("1999-01-01")) {
    results$has_critical_issues <- TRUE
    results$invalid_dates <- TRUE
    warning("Data contains dates before 1999 (pre-nflfastR era)")
  } else {
    results$invalid_dates <- FALSE
  }
  
  # Check 5: Duplicate plays (PRIORITY 1 FIX: Count pairs not rows)
  # Only check duplicates if both game_id and play_id columns exist
  if ("game_id" %in% names(pbp) && "play_id" %in% names(pbp)) {
    duplicate_pairs <- pbp %>%
      filter(!is.na(game_id) & !is.na(play_id)) %>%  # Explicit NA exclusion
      group_by(game_id, play_id) %>%
      summarise(n = n(), .groups = "drop") %>%
      filter(n > 1)  # Find duplicate pairs
    
    duplicate_plays <- nrow(duplicate_pairs)  # Count PAIRS not individual rows
    
    if (duplicate_plays > 0) {
      results$has_critical_issues <- TRUE
      results$duplicate_plays <- duplicate_plays
      warning(glue("Found {duplicate_plays} duplicate plays (same game_id + play_id)"))
    } else {
      results$duplicate_plays <- 0
    }
  } else {
    # Can't check duplicates without required columns
    results$duplicate_plays <- NA
    if (!"game_id" %in% names(pbp) || !"play_id" %in% names(pbp)) {
      message("Skipping duplicate check: game_id or play_id column missing")
    }
  }
  # Check 6: Season completeness
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  current_season_data <- pbp %>% filter(season == current_year)
  
  if (nrow(current_season_data) > 0) {
    current_week <- max(current_season_data$week, na.rm = TRUE)
    
    if (current_week < 18) {
      results$incomplete_seasons <- data.frame(
        season = current_year, 
        weeks_available = current_week
      )
      message(glue("ℹ️  Note: {current_year} season is incomplete (data through week {current_week} of 18)"))
    }
  }
  
  # Summary statistics
  results$summary <- list(
    total_plays = nrow(pbp),
    seasons = unique(pbp$season),
    weeks = range(pbp$week, na.rm = TRUE),
    date_range = c(min_date, max_date),
    teams = length(unique(c(pbp$posteam, pbp$defteam))),
    games = length(unique(pbp$game_id))
  )
  
  return(results)
}


#' Get Roster Data with Position Information
#'
#' @description
#' Loads roster data from nflfastR for specified seasons.
#' Useful for getting accurate position information for players.
#'
#' @param seasons Numeric vector of seasons to load (e.g., 2018:2023)
#'
#' @return A tibble containing roster data with player positions
#'
#' @examples
#' \dontrun{
#' roster_2023 <- get_roster_data(2023)
#' roster_recent <- get_roster_data(2021:2023)
#' }
#'
#' @export
get_roster_data <- function(seasons) {
  
  library(dplyr)
  library(glue)
  
  # Check nflreadr package availability
  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    stop(
      "nflreadr package required but not installed.\n",
      "Fix: install.packages('nflreadr')\n",
      "Note: nflfastR moved roster functions to nflreadr package."
    )
  }
  
  # Input validation
  if (!is.numeric(seasons)) {
    stop("seasons must be numeric (e.g., 2023 or 2018:2023)")
  }
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (any(seasons < 1999 | seasons > current_year)) {
    stop(glue("Invalid seasons: {paste(seasons, collapse=', ')}. Must be between 1999 and {current_year}."))
  }
  
  message(glue("Loading roster data for seasons: {paste(seasons, collapse=', ')}"))
  
  # Load rosters with enhanced error handling
  rosters <- tryCatch({
    nflreadr::load_rosters(seasons)
  }, error = function(e) {
    stop(
      "Failed to download roster data.\n",
      "Error: ", e$message, "\n",
      "Troubleshooting:\n",
      "  1. Check internet connection\n",
      "  2. Verify seasons are valid: ", paste(seasons, collapse=", "), "\n",
      "  3. Try updating nflreadr: install.packages('nflreadr')\n",
      "  4. Check https://github.com/nflverse/nflreadr for known issues"
    )
  })
  
  # Select key columns and clean (improved robustness)
  expected_cols <- c("season", "team", "gsis_id", "full_name", "position", 
                     "depth_chart_position", "status")
  missing_cols <- setdiff(expected_cols, names(rosters))
  
  if (length(missing_cols) > 0) {
    warning(glue("Roster data missing columns: {paste(missing_cols, collapse=', ')}. These will be omitted."))
  }
  
  rows_before <- nrow(rosters)
  
  rosters_clean <- rosters %>%
    select(any_of(expected_cols)) %>%  # CHANGED: use any_of() instead of select()
    filter(!is.na(gsis_id))  # Remove rows without player ID
  
  rows_removed <- rows_before - nrow(rosters_clean)
  
  message(glue("Loaded {nrow(rosters_clean)} player records"))
  
  if (rows_removed > 0) {
    message(glue("ℹ️  Removed {rows_removed} roster records without player IDs"))
  }
  
  return(rosters_clean)
}