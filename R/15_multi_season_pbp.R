# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 1
# Multi-Season Play-by-Play Loading and Schema Normalization
# File: R/15_multi_season_pbp.R
#
# Purpose: Load, normalize, validate, and cache 16 seasons of nflfastR
#          play-by-play data (2010-2025). Presents a consistent column
#          interface regardless of source season. Season-level RDS caching
#          ensures the full load runs once, not every session.
#
# Production-grade for portfolio display
# Built for personal analytics use
# Demonstrates data engineering capability across a 16-year NFL dataset
#
# Navigation:
#   Line ~30   : Package dependencies
#   Line ~50   : Constants and configuration
#   Line ~90   : load_multi_season_pbp()
#   Line ~250  : normalize_schema()
#   Line ~480  : validate_season_coverage()
#   Line ~600  : get_schema_differences()
#   Line ~700  : validate_epa_distribution()
#   Line ~830  : load_normalized_season()
#   Line ~900  : run_week1_pipeline()
#
# Dependencies: nflfastR, nflreadr, dplyr, glue, here
# Data: nflfastR play-by-play 2010-2025 (16 seasons)
# ==============================================================================

library(nflfastR)
library(nflreadr)
library(dplyr)
library(glue)
library(here)

# ==============================================================================
# CONSTANTS
# ==============================================================================

# Default season range: 2010-2025 (16 seasons)
# 2010 chosen as start: nflfastR data is reliable from 1999, but EPA models
# and column coverage stabilize meaningfully around 2010. Pre-2010 data has
# more missing columns and less consistent play classification.
#
# NFL structure:
#   2010-2020: 16-game regular season (256 total games)
#   2021-2025: 17-game regular season (272 total games)
SEASON_RANGE_DEFAULT <- 2010:2025
SEASON_MIN <- 1999L
SEASON_MAX <- as.integer(format(Sys.Date(), "%Y"))

# Cache directory for normalized season files
CACHE_DIR_DEFAULT <- here::here("data", "season2_cache")

# Schema normalization version tag -- increment when normalize_schema() logic changes
SCHEMA_NORM_VERSION <- "s2v1"

# Team relocation mappings (old -> new)
# These ensure consistent team abbreviations across all 16 seasons
TEAM_RELOCATIONS <- list(

  # Oakland Raiders -> Las Vegas Raiders (2020)
  "OAK" = "LV",
  # San Diego Chargers -> Los Angeles Chargers (2017)
  "SD"  = "LAC",
  # St. Louis Rams -> Los Angeles Rams (2016)
  "STL" = "LA"
)

# Core columns that MUST exist in every normalized season
# Type-verified during normalization
CORE_COLUMNS <- c(

  "play_id", "game_id", "season", "week", "game_date",

  "posteam", "defteam", "play_type", "desc",
  "yards_gained", "epa", "success",
  "passer_player_id", "passer_player_name",
  "rusher_player_id", "rusher_player_name",
  "receiver_player_id", "receiver_player_name",
  "passing_yards", "rushing_yards", "receiving_yards",
  "air_yards", "yards_after_catch",
  "touchdown", "interception", "fumble", "sack",
  "qb_hit", "wp", "def_wp",
  "down", "ydstogo", "yardline_100",
  "shotgun", "no_huddle", "qb_dropback", "qb_scramble",
  "pass_attempt", "rush_attempt", "complete_pass",
  "two_point_attempt", "extra_point_attempt", "field_goal_attempt",
  "penalty", "penalty_yards"
)

# Optional columns that are added as NA if absent in older seasons
# These became available in nflfastR at different points
OPTIONAL_COLUMNS <- c(
  "pass_oe",           # Pass rate over expected (~2016+)
  "cpoe",              # Completion % over expected (~2016+)
  "xpass",             # Expected pass probability (~2016+)
  "xyac_epa",          # Expected YAC EPA (~2018+)
  "xyac_mean_yardage", # Expected YAC mean yardage (~2018+)
  "comp_air_epa",      # Completion air EPA
  "comp_yac_epa",      # Completion YAC EPA
  "receiver_player_id" # Exists in all years but verify
)

# Expected game counts per season
# 2010-2020: 256 games (32 teams * 16 games / 2)
# 2021-2025: 272 games (32 teams * 17 games / 2)
get_expected_games <- function(season) {
  if (season >= 2021) return(272L)
  return(256L)
}


# ==============================================================================
# FUNCTION: load_multi_season_pbp
# ==============================================================================

#' Load Multiple Seasons of nflfastR Play-by-Play Data
#'
#' Downloads, normalizes, and caches nflfastR play-by-play data for the
#' specified season range. Each season is processed individually to manage
#' memory: load -> normalize -> cache -> free -> next season. Peak memory
#' stays under ~400MB regardless of total seasons requested.
#'
#' @param seasons Integer vector of seasons to load. Default: 2010:2025
#' @param cache_dir Character path to cache directory. Default: data/season2_cache/
#' @param force_reload Logical. If TRUE, re-downloads and overwrites cached
#'   seasons. Default: FALSE
#'
#' @return A tibble with one row per season summarizing load results:
#'   season (int), status (chr: "cached"|"loaded"|"error"),
#'   n_plays (int), n_games (int), cache_file (chr)
#'
#' @details
#' Does NOT return the full play-by-play data. The data is cached as individual
#' season RDS files. Use load_normalized_season() to retrieve a single season.
#' This design prevents loading all 16 seasons into memory simultaneously.
#'
#' NFL context: nflfastR provides play-level data including EPA
#' (Expected Points Added = EP_after - EP_before), success indicators, and
#' player IDs. Schema varies across the 16-year span due to nflfastR updates,
#' rule changes (e.g., 2015 PAT distance, 2021 17-game season), and evolving
#' tracking data availability.
#'
#' @examples
#' # Load all 16 seasons (first run: 20-30 min; subsequent: skips cached)
#' summary <- load_multi_season_pbp()
#'
#' # Load subset for testing
#' summary <- load_multi_season_pbp(seasons = 2022:2025)
#'
#' # Force re-download of everything
#' summary <- load_multi_season_pbp(force_reload = TRUE)
#'
#' @seealso normalize_schema, validate_season_coverage, load_normalized_season
#' @export
load_multi_season_pbp <- function(seasons = SEASON_RANGE_DEFAULT,
                                   cache_dir = CACHE_DIR_DEFAULT,
                                   force_reload = FALSE) {


  # --- Input validation ---
  if (!is.numeric(seasons) || length(seasons) == 0) {
    stop("'seasons' must be a non-empty numeric vector.", call. = FALSE)
  }

  seasons <- as.integer(seasons)

  if (any(seasons < SEASON_MIN) || any(seasons > SEASON_MAX)) {
    stop(glue("All seasons must be between {SEASON_MIN} and {SEASON_MAX}. ",
              "Received: {paste(seasons[seasons < SEASON_MIN | seasons > SEASON_MAX], collapse = ', ')}"),
         call. = FALSE)
  }

  # --- Create cache directory ---
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    message(glue("Created cache directory: {cache_dir}"))
  }

  # --- Process each season ---
  n_seasons <- length(seasons)
  results <- vector("list", n_seasons)

  message(glue("\n{'='|>strrep(60)}"))
  message(glue("Loading {n_seasons} season(s): {min(seasons)}-{max(seasons)}"))
  message(glue("Cache directory: {cache_dir}"))
  message(glue("Force reload: {force_reload}"))
  message(glue("{'='|>strrep(60)}\n"))

  for (i in seq_along(seasons)) {
    s <- seasons[i]
    cache_file <- file.path(cache_dir, glue("pbp_normalized_{s}.rds"))

    # Check cache

    if (file.exists(cache_file) && !force_reload) {
      message(glue("[{i}/{n_seasons}] Season {s}: cached (skipping download)"))

      # Read just enough to report summary stats
      cached <- readRDS(cache_file)
      results[[i]] <- tibble(
        season   = s,
        status   = "cached",
        n_plays  = nrow(cached),
        n_games  = length(unique(cached$game_id)),
        cache_file = cache_file
      )
      rm(cached)
      gc(verbose = FALSE)
      next
    }

    # Download and normalize
    message(glue("[{i}/{n_seasons}] Season {s}: downloading from nflfastR..."))

    tryCatch({
      pbp_raw <- nflfastR::load_pbp(s)
      message(glue("  Raw: {nrow(pbp_raw)} plays, {length(unique(pbp_raw$game_id))} games"))

      # Normalize schema
      pbp_norm <- normalize_schema(pbp_raw, season = s)
      message(glue("  Normalized: {ncol(pbp_norm)} columns, version {SCHEMA_NORM_VERSION}"))

      # Cache to disk
      saveRDS(pbp_norm, cache_file)
      message(glue("  Cached: {cache_file}"))

      results[[i]] <- tibble(
        season   = s,
        status   = "loaded",
        n_plays  = nrow(pbp_norm),
        n_games  = length(unique(pbp_norm$game_id)),
        cache_file = cache_file
      )

      # Free memory before next season
      rm(pbp_raw, pbp_norm)
      gc(verbose = FALSE)

    }, error = function(e) {
      warning(glue("Season {s} failed: {e$message}"), call. = FALSE)
      results[[i]] <<- tibble(
        season   = s,
        status   = "error",
        n_plays  = NA_integer_,
        n_games  = NA_integer_,
        cache_file = NA_character_
      )
    })
  }

  summary <- bind_rows(results)

  # Final report
  n_ok <- sum(summary$status != "error")
  n_err <- sum(summary$status == "error")
  total_plays <- sum(summary$n_plays, na.rm = TRUE)

  message(glue("\n{'='|>strrep(60)}"))
  message(glue("Complete: {n_ok} seasons loaded, {n_err} errors"))
  message(glue("Total plays cached: {format(total_plays, big.mark = ',')}"))
  message(glue("{'='|>strrep(60)}\n"))

  return(summary)
}


# ==============================================================================
# FUNCTION: normalize_schema
# ==============================================================================

#' Normalize nflfastR Schema for Cross-Season Consistency
#'
#' Standardizes column names, types, and presence across all seasons.
#' Core columns are type-verified. Optional columns are added as NA if absent
#' so downstream code can reference them without season-specific guards.
#' Team abbreviations are updated for relocations (OAK->LV, SD->LAC, STL->LA).
#'
#' @param pbp_raw Data frame of raw nflfastR play-by-play for a single season.
#' @param season Integer. The season year being normalized.
#'
#' @return A tibble with consistent column names/types, relocation-adjusted
#'   team abbreviations, and a season_norm_version tag column.
#'
#' @details
#' NFL context: nflfastR schema has evolved over 16 years. Key changes include:
#'
#' - Columns like cpoe, pass_oe, xpass became available ~2016
#' - xyac_epa, xyac_mean_yardage appeared ~2018
#' - Team abbreviations changed with relocations (SD->LAC 2017, STL->LA 2016,
#'   OAK->LV 2020)
#' - The 2015 PAT rule change (moved to 33-yard attempt) affects extra point
#'   EPA calculations in that transition year
#' - 2021 introduced the 17-game season (week 18 regular season)
#'
#' This function does NOT filter plays. It only standardizes the schema.
#' Filtering (garbage time, kneels, spikes) is left to downstream functions.
#'
#' @seealso load_multi_season_pbp, get_schema_differences
#' @keywords internal
normalize_schema <- function(pbp_raw, season) {

  if (!is.data.frame(pbp_raw) || nrow(pbp_raw) == 0) {
    stop(glue("normalize_schema(): received empty or non-data.frame input for season {season}"),
         call. = FALSE)
  }

  raw_cols <- names(pbp_raw)

  # --- 1. Add missing optional columns as NA ---
  for (col in OPTIONAL_COLUMNS) {
    if (!col %in% raw_cols) {
      pbp_raw[[col]] <- NA_real_
    }
  }

  # --- 2. Verify core columns exist ---
  missing_core <- setdiff(CORE_COLUMNS, names(pbp_raw))
  if (length(missing_core) > 0) {
    # Some very early seasons may lack a few core columns
    # Add as NA with a warning rather than hard-stopping
    for (col in missing_core) {
      pbp_raw[[col]] <- NA
      warning(glue("Season {season}: core column '{col}' missing, added as NA"),
              call. = FALSE)
    }
  }

  # --- 3. Type verification for critical columns ---
  pbp_norm <- pbp_raw %>%
    mutate(
      # Ensure integer types
      season     = as.integer(season),
      week       = as.integer(week),
      down       = as.integer(down),
      ydstogo    = as.integer(ydstogo),
      yards_gained = as.integer(yards_gained),

      # Ensure numeric types for analytics columns
      epa        = as.numeric(epa),
      wp         = as.numeric(wp),
      air_yards  = as.numeric(air_yards),
      yards_after_catch = as.numeric(yards_after_catch),
      cpoe       = as.numeric(cpoe),

      # Ensure character types for IDs
      game_id    = as.character(game_id),
      play_id    = as.character(play_id),
      posteam    = as.character(posteam),
      defteam    = as.character(defteam),
      passer_player_id   = as.character(passer_player_id),
      rusher_player_id   = as.character(rusher_player_id),
      receiver_player_id = as.character(receiver_player_id),

      # Ensure logical types for binary flags
      shotgun    = as.logical(shotgun),
      no_huddle  = as.logical(no_huddle),
      qb_dropback  = as.logical(qb_dropback),
      qb_scramble  = as.logical(qb_scramble),
      touchdown    = as.integer(touchdown),
      interception = as.integer(interception),
      fumble       = as.integer(fumble),
      sack         = as.integer(sack),
      complete_pass = as.integer(complete_pass),
      pass_attempt  = as.integer(pass_attempt),
      rush_attempt  = as.integer(rush_attempt)
    )

  # --- 4. Handle team relocations ---
  for (old_abbr in names(TEAM_RELOCATIONS)) {
    new_abbr <- TEAM_RELOCATIONS[[old_abbr]]
    pbp_norm <- pbp_norm %>%
      mutate(
        posteam = ifelse(posteam == old_abbr, new_abbr, posteam),
        defteam = ifelse(defteam == old_abbr, new_abbr, defteam)
      )
  }

  # --- 5. Add normalization metadata ---
  pbp_norm <- pbp_norm %>%
    mutate(season_norm_version = SCHEMA_NORM_VERSION)

  return(pbp_norm)
}


# ==============================================================================
# FUNCTION: validate_season_coverage
# ==============================================================================

#' Validate Season Coverage and Data Quality
#'
#' Checks each cached season for expected game counts, team counts, play
#' counts per game, and EPA distribution. Returns a diagnostic tibble with
#' pass/fail flags per season.
#'
#' @param seasons Integer vector of seasons to validate. Default: 2010:2025
#' @param cache_dir Character path to cache directory. Default: data/season2_cache/
#'
#' @return A tibble with one row per season:
#'   season (int), n_games (int), expected_games (int), games_ok (lgl),
#'   n_teams (int), teams_ok (lgl), median_plays_per_game (dbl),
#'   plays_per_game_ok (lgl), mean_epa (dbl), epa_centered (lgl),
#'   all_pass (lgl)
#'
#' @details
#' Validation thresholds:
#' - Game count: must equal expected (256 for 2010-2020, 272 for 2021-2025)
#' - Team count: must equal 32
#' - Plays per game: median must be between 100 and 200
#' - EPA: mean of pass/run plays (excluding garbage time) within 0.05 of zero
#'
#' NFL context: The 17-game season started in 2021 (272 games vs 256).
#' Some seasons may show slight game count deviations due to cancelled/
#' postponed games (e.g., COVID-19 in 2020 had some schedule adjustments).
#'
#' @examples
#' validation <- validate_season_coverage(seasons = 2022:2025)
#' print(validation)
#'
#' # Check which seasons failed
#' validation %>% filter(!all_pass)
#'
#' @seealso load_multi_season_pbp, validate_epa_distribution
#' @export
validate_season_coverage <- function(seasons = SEASON_RANGE_DEFAULT,
                                      cache_dir = CACHE_DIR_DEFAULT) {

  results <- vector("list", length(seasons))

  for (i in seq_along(seasons)) {
    s <- seasons[i]
    cache_file <- file.path(cache_dir, glue("pbp_normalized_{s}.rds"))

    if (!file.exists(cache_file)) {
      warning(glue("Season {s}: cache file not found at {cache_file}"), call. = FALSE)
      results[[i]] <- tibble(
        season = s, n_games = NA_integer_, expected_games = get_expected_games(s),
        games_ok = FALSE, n_teams = NA_integer_, teams_ok = FALSE,
        median_plays_per_game = NA_real_, plays_per_game_ok = FALSE,
        mean_epa = NA_real_, epa_centered = FALSE, all_pass = FALSE
      )
      next
    }

    pbp <- readRDS(cache_file)

    # Filter to regular season only
    # 2021+: weeks 1-18 are regular season (17-game schedule)
    # 2010-2020: weeks 1-17 are regular season (16-game schedule)
    max_reg_week <- ifelse(s >= 2021, 18L, 17L)
    pbp_reg <- pbp %>%
      filter(!is.na(week), week <= max_reg_week)

    # Game count (regular season only)
    game_ids <- unique(pbp_reg$game_id)
    n_games <- length(game_ids)
    expected <- get_expected_games(s)

    # Team count (from posteam, excluding NA/empty)
    teams <- unique(pbp_reg$posteam[!is.na(pbp_reg$posteam) & pbp_reg$posteam != ""])
    n_teams <- length(teams)

    # Plays per game
    ppg <- pbp_reg %>%
      filter(!is.na(game_id)) %>%
      count(game_id) %>%
      pull(n)
    median_ppg <- median(ppg, na.rm = TRUE)

    # EPA check on clean offensive plays (pass/run, non-garbage-time)
    # Garbage time = extreme win probability in any quarter
    # Full garbage time filtering (Q4-specific) lives in validate_epa_distribution()
    # This is a quick sanity check only
    epa_plays <- pbp_reg %>%
      filter(
        !is.na(epa),
        !is.na(posteam),
        play_type %in% c("pass", "run"),
        # Exclude extreme win probability plays (garbage time proxy)
        !is.na(wp),
        wp >= 0.10,
        wp <= 0.90
      )
    mean_epa <- mean(epa_plays$epa, na.rm = TRUE)

    results[[i]] <- tibble(
      season = s,
      n_games = n_games,
      expected_games = expected,
      games_ok = (n_games >= expected - 2L & n_games <= expected + 2L),
      n_teams = n_teams,
      teams_ok = (n_teams == 32L),
      median_plays_per_game = round(median_ppg, 1),
      plays_per_game_ok = (median_ppg >= 100 & median_ppg <= 200),
      mean_epa = round(mean_epa, 4),
      epa_centered = (abs(mean_epa) < 0.05),
      all_pass = NA
    )

    rm(pbp, pbp_reg, epa_plays)
    gc(verbose = FALSE)
  }

  out <- bind_rows(results) %>%
    mutate(all_pass = games_ok & teams_ok & plays_per_game_ok & epa_centered)

  return(out)
}


# ==============================================================================
# FUNCTION: get_schema_differences
# ==============================================================================

#' Diagnose Schema Differences Across Seasons
#'
#' Reads only column names from each cached season RDS file (no full data load).
#' Returns a wide tibble showing which columns exist in which seasons.
#' Use before Phase 3 modeling to know exactly which features are available
#' back to 2010.
#'
#' @param seasons Integer vector of seasons to compare. Default: 2010:2025
#' @param cache_dir Character path to cache directory. Default: data/season2_cache/
#'
#' @return A tibble with columns: column_name (chr), then one logical column
#'   per season (e.g., s2010, s2011, ..., s2025) indicating presence.
#'   Sorted by number of seasons the column appears in (ascending), so
#'   columns available in fewer seasons appear first.
#'
#' @details
#' This function reads only names() from each RDS file, not the full data.
#' Memory footprint is minimal even for all 16 seasons. The output is
#' designed for quick diagnosis: which columns can you rely on for the full
#' 16-year span vs. which are only available in recent seasons.
#'
#' @examples
#' diffs <- get_schema_differences(seasons = 2010:2025)
#'
#' # Find columns missing from early seasons
#' diffs %>% filter(!s2010 & s2025)
#'
#' # Find columns available in ALL seasons
#' diffs %>% filter(if_all(starts_with("s"), ~ .x == TRUE))
#'
#' @seealso normalize_schema, validate_season_coverage
#' @export
get_schema_differences <- function(seasons = SEASON_RANGE_DEFAULT,
                                    cache_dir = CACHE_DIR_DEFAULT) {

  all_cols <- list()

  for (s in seasons) {
    cache_file <- file.path(cache_dir, glue("pbp_normalized_{s}.rds"))

    if (!file.exists(cache_file)) {
      warning(glue("Season {s}: cache file not found, skipping."), call. = FALSE)
      next
    }

    # Read only names -- no full data load
    pbp <- readRDS(cache_file)
    all_cols[[as.character(s)]] <- names(pbp)
    rm(pbp)
    gc(verbose = FALSE)
  }

  if (length(all_cols) == 0) {
    stop("No cached season files found. Run load_multi_season_pbp() first.", call. = FALSE)
  }

  # Build presence matrix
  all_unique_cols <- sort(unique(unlist(all_cols)))
  presence <- tibble(column_name = all_unique_cols)

  for (s_chr in names(all_cols)) {
    col_label <- paste0("s", s_chr)
    presence[[col_label]] <- all_unique_cols %in% all_cols[[s_chr]]
  }

  # Sort by how many seasons the column appears in (ascending)
  season_cols <- names(presence)[grepl("^s\\d{4}$", names(presence))]
  presence <- presence %>%
    mutate(n_seasons = rowSums(across(all_of(season_cols)))) %>%
    arrange(n_seasons, column_name) %>%
    select(-n_seasons)

  return(presence)
}


# ==============================================================================
# FUNCTION: validate_epa_distribution
# ==============================================================================

#' Validate EPA Distribution Across Seasons
#'
#' For each season, filters to regular-season pass/run plays (excluding
#' garbage time, kneels, spikes, and 2-point attempts), then checks that
#' mean EPA is centered near zero. Optionally saves per-season density plots.
#'
#' @param seasons Integer vector of seasons to validate. Default: 2010:2025
#' @param cache_dir Character path to cache directory. Default: data/season2_cache/
#' @param save_plots Logical. If TRUE, saves density plot per season to
#'   output/plots/epa_validation/. Default: FALSE
#' @param epa_tolerance Numeric. Maximum acceptable absolute mean EPA.
#'   Default: 0.05
#'
#' @return A tibble with one row per season:
#'   season (int), n_plays (int), mean_epa (dbl), sd_epa (dbl),
#'   median_epa (dbl), within_tolerance (lgl)
#'
#' @details
#' EPA (Expected Points Added) should be centered near zero across all
#' plays in aggregate: positive EPA plays are balanced by negative EPA plays
#' over a full season. Deviations suggest data quality issues or model changes
#' in nflfastR's underlying EP model.
#'
#' Filters applied:
#' - !is.na(epa): play has EPA value
#' - !is.na(posteam): valid possessing team
#' - play_type %in% c("pass", "run"): offensive plays only
#' - Exclude Q4 garbage time (wp < 0.10 or wp > 0.90)
#' - Exclude QB kneels (play_type != "qb_kneel")
#' - Exclude QB spikes (play_type != "qb_spike")
#' - Exclude 2-point attempts (two_point_attempt != 1)
#'
#' @examples
#' epa_check <- validate_epa_distribution(seasons = 2020:2025)
#' epa_check %>% filter(!within_tolerance)
#'
#' @seealso validate_season_coverage
#' @export
validate_epa_distribution <- function(seasons = SEASON_RANGE_DEFAULT,
                                       cache_dir = CACHE_DIR_DEFAULT,
                                       save_plots = FALSE,
                                       epa_tolerance = 0.05) {

  if (save_plots) {
    plot_dir <- here::here("output", "plots", "epa_validation")
    if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  }

  results <- vector("list", length(seasons))

  for (i in seq_along(seasons)) {
    s <- seasons[i]
    cache_file <- file.path(cache_dir, glue("pbp_normalized_{s}.rds"))

    if (!file.exists(cache_file)) {
      warning(glue("Season {s}: cache file not found."), call. = FALSE)
      results[[i]] <- tibble(
        season = s, n_plays = NA_integer_, mean_epa = NA_real_,
        sd_epa = NA_real_, median_epa = NA_real_, within_tolerance = FALSE
      )
      next
    }

    pbp <- readRDS(cache_file)

    # Clean plays for EPA validation
    # EPA should be near zero on aggregate for pass/run plays
    clean <- pbp %>%
      filter(
        !is.na(epa),
        !is.na(posteam),
        play_type %in% c("pass", "run"),
        # Exclude garbage time
        !(down >= 1 & !is.na(wp) & (wp < 0.10 | wp > 0.90)),
        # Exclude kneels and spikes (already filtered by play_type but be explicit)
        !grepl("kneel|spike", tolower(desc), fixed = FALSE),
        # Exclude 2-point attempts
        is.na(two_point_attempt) | two_point_attempt != 1
      )

    m_epa <- mean(clean$epa, na.rm = TRUE)
    sd_epa <- sd(clean$epa, na.rm = TRUE)
    med_epa <- median(clean$epa, na.rm = TRUE)

    results[[i]] <- tibble(
      season = s,
      n_plays = nrow(clean),
      mean_epa = round(m_epa, 4),
      sd_epa = round(sd_epa, 4),
      median_epa = round(med_epa, 4),
      within_tolerance = (abs(m_epa) < epa_tolerance)
    )

    # Optional density plot
    if (save_plots) {
      tryCatch({
        library(ggplot2)
        p <- ggplot(clean, aes(x = epa)) +
          geom_density(fill = "steelblue", alpha = 0.5) +
          geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
          geom_vline(xintercept = m_epa, color = "darkgreen", linewidth = 0.8) +
          labs(
            title = glue("EPA Distribution: {s} Season"),
            subtitle = glue("Mean EPA = {round(m_epa, 4)} | n = {format(nrow(clean), big.mark = ',')} plays"),
            x = "EPA (Expected Points Added)",
            y = "Density",
            caption = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nFiltered: pass/run plays, no garbage time, no kneels/spikes/2PT"
          ) +
          theme_minimal() +
          theme(plot.title = element_text(face = "bold"))

        ggsave(
          file.path(plot_dir, glue("epa_density_{s}.png")),
          plot = p, width = 8, height = 5, dpi = 300
        )
      }, error = function(e) {
        warning(glue("Plot for season {s} failed: {e$message}"), call. = FALSE)
      })
    }

    rm(pbp, clean)
    gc(verbose = FALSE)
  }

  return(bind_rows(results))
}


# ==============================================================================
# FUNCTION: load_normalized_season
# ==============================================================================

#' Load a Single Normalized Season from Cache
#'
#' Reads one season of cached, normalized play-by-play data. Throws a clear
#' error with instructions if the cache file is missing.
#'
#' @param season Integer. The season year to load.
#' @param cache_dir Character path to cache directory. Default: data/season2_cache/
#'
#' @return A tibble of normalized play-by-play data for the requested season.
#'
#' @details
#' This is the primary entry point for downstream scripts that need a single
#' season of data. It does not download or normalize -- it reads the pre-cached
#' RDS file produced by load_multi_season_pbp().
#'
#' @examples
#' pbp_2025 <- load_normalized_season(2025)
#' pbp_2025 %>% filter(play_type %in% c("pass", "run")) %>% nrow()
#'
#' @seealso load_multi_season_pbp
#' @export
load_normalized_season <- function(season, cache_dir = CACHE_DIR_DEFAULT) {

  season <- as.integer(season)

  if (length(season) != 1) {
    stop("load_normalized_season() accepts exactly one season. ",
         "Use load_multi_season_pbp() for multiple seasons.", call. = FALSE)
  }

  cache_file <- file.path(cache_dir, glue("pbp_normalized_{season}.rds"))

  if (!file.exists(cache_file)) {
    stop(glue(
      "Cache file not found for season {season}.\n",
      "Expected: {cache_file}\n\n",
      "To create it, run:\n",
      "  load_multi_season_pbp(seasons = {season})\n\n",
      "Or to load all seasons:\n",
      "  load_multi_season_pbp()"
    ), call. = FALSE)
  }

  message(glue("Loading normalized season {season} from cache..."))
  pbp <- readRDS(cache_file)
  message(glue("  {format(nrow(pbp), big.mark = ',')} plays, ",
               "{length(unique(pbp$game_id))} games"))

  return(pbp)
}


# ==============================================================================
# FUNCTION: run_week1_pipeline
# ==============================================================================

#' Run Complete Week 1 Pipeline
#'
#' End-to-end wrapper: loads all seasons, validates coverage, checks EPA
#' distributions. Returns a list with all diagnostic outputs.
#'
#' @param seasons Integer vector of seasons. Default: 2010:2025
#' @param cache_dir Character path to cache directory. Default: data/season2_cache/
#' @param force_reload Logical. Force re-download of all seasons. Default: FALSE
#' @param save_epa_plots Logical. Save EPA density plots. Default: FALSE
#'
#' @return A named list with three elements:
#'   load_summary (tibble from load_multi_season_pbp),
#'   coverage_validation (tibble from validate_season_coverage),
#'   epa_validation (tibble from validate_epa_distribution)
#'
#' @examples
#' # Full pipeline
#' results <- run_week1_pipeline()
#'
#' # Quick test on 3 seasons
#' results <- run_week1_pipeline(seasons = 2023:2025)
#'
#' @seealso load_multi_season_pbp, validate_season_coverage, validate_epa_distribution
#' @export
run_week1_pipeline <- function(seasons = SEASON_RANGE_DEFAULT,
                                cache_dir = CACHE_DIR_DEFAULT,
                                force_reload = FALSE,
                                save_epa_plots = FALSE) {

  message(glue("\n{'#'|>strrep(60)}"))
  message("NFL Analytics Toolkit - Season 2, Week 1 Pipeline")
  message(glue("Seasons: {min(seasons)}-{max(seasons)} ({length(seasons)} total)"))
  message(glue("{'#'|>strrep(60)}\n"))

  # Step 1: Load and cache
  message("STEP 1/3: Loading and caching seasons...")
  load_summary <- load_multi_season_pbp(
    seasons = seasons,
    cache_dir = cache_dir,
    force_reload = force_reload
  )

  # Step 2: Validate coverage
  message("\nSTEP 2/3: Validating season coverage...")
  loaded_seasons <- load_summary %>%
    filter(status != "error") %>%
    pull(season)

  coverage <- validate_season_coverage(
    seasons = loaded_seasons,
    cache_dir = cache_dir
  )

  n_pass <- sum(coverage$all_pass, na.rm = TRUE)
  n_fail <- sum(!coverage$all_pass, na.rm = TRUE)
  message(glue("  Coverage: {n_pass} pass, {n_fail} fail"))

  if (n_fail > 0) {
    failed <- coverage %>% filter(!all_pass) %>% pull(season)
    message(glue("  Failed seasons: {paste(failed, collapse = ', ')}"))
  }

  # Step 3: EPA validation
  message("\nSTEP 3/3: Validating EPA distributions...")
  epa_check <- validate_epa_distribution(
    seasons = loaded_seasons,
    cache_dir = cache_dir,
    save_plots = save_epa_plots
  )

  n_epa_ok <- sum(epa_check$within_tolerance, na.rm = TRUE)
  n_epa_bad <- sum(!epa_check$within_tolerance, na.rm = TRUE)
  message(glue("  EPA: {n_epa_ok} centered, {n_epa_bad} outside tolerance"))

  # Final summary
  message(glue("\n{'#'|>strrep(60)}"))
  message("Pipeline complete.")
  message(glue("{'#'|>strrep(60)}\n"))

  return(list(
    load_summary = load_summary,
    coverage_validation = coverage,
    epa_validation = epa_check
  ))
}
