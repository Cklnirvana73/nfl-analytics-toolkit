# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 6
# Multi-Season College Football Play-by-Play Loading and Schema Normalization
# File: R/20_multi_season_cfb_pbp.R
#
# Purpose: Load, normalize, validate, and cache 12 seasons of cfbfastR
#          college football play-by-play data (2014-2025). Mirrors the
#          structural pattern established in R/15 for nflfastR data, with
#          college-appropriate validation thresholds and schema handling.
#          Season-level RDS caching ensures the full load runs once, not
#          every session.
#
# Design choices (see Season 2 Week 6 plan):
#   - CFB data floor is 2014L. cfbfastR RDS files exist back to 2004 but
#     EPA coverage and schema consistency are unreliable before 2014.
#     Season range 2014-2025 (12 seasons) is the reliable window and
#     aligns with the Phase 3 college-to-pro translation target (NFL 2014+).
#   - Schema normalization preserves CFB native column names (pos_team,
#     def_pos_team) and adds NFL-style aliases (posteam, defteam) for
#     downstream consistency with NFL modules. Both columns preserved.
#   - FBS filter applied at load_normalized_cfb_season() call site via
#     a division parameter, not at cache-write time. Cache contains all
#     divisions; FCS data remains available for Week 8 ID reconciliation.
#   - FBS filter uses the embedded home_team_division / away_team_division
#     columns that cfbfastR includes in PBP data. No CFBD API call
#     required. Single source of truth via .filter_cfb_by_division().
#     Strategy: keep plays where pos_team is the home team AND
#     home_team_division == "fbs", OR pos_team is the away team AND
#     away_team_division == "fbs". This is the offensive-team-first
#     strategy confirmed by diagnostic_s2w6_division_check.R (Check 4,
#     Strategy C): 134 FBS teams, 143,929 plays in 2024.
#   - Validation thresholds relaxed vs NFL: FBS team count varies by
#     season (120-134 historically), games per team include bowls and
#     playoff, so ranges are wider than the NFL's fixed 32-team structure.
#   - 2022 NOTE: cfbfastR expanded FCS play-by-play coverage starting in
#     2022. Raw play counts jump ~54% from 2021 to 2022 (163k -> 252k).
#     This is expected data behavior driven by FCS coverage expansion,
#     not a load error. The FBS filter removes FCS plays when
#     division = "fbs". Validated by diagnostic Check 5: 2021 had only
#     162 FCS home plays; 2022 had 91,013.
#
# Navigation:
#   Line ~60   : Package dependencies
#   Line ~75   : Constants and configuration
#   Line ~150  : Internal helpers (.filter_cfb_by_division, %||%)
#   Line ~220  : load_multi_season_cfb_pbp()
#   Line ~385  : normalize_cfb_schema()
#   Line ~490  : validate_cfb_season_coverage()
#   Line ~650  : get_cfb_schema_differences()
#   Line ~755  : load_normalized_cfb_season()
#   Line ~845  : run_week6_pipeline()
#
# Dependencies: cfbfastR, dplyr, tibble, glue, here
# Data: cfbfastR play-by-play 2014-2025 (12 seasons)
# Schema tag: s2cfbv1
# ==============================================================================

library(cfbfastR)
library(dplyr)
library(tibble)
library(glue)
library(here)

# ==============================================================================
# CONSTANTS
# ==============================================================================

# Default season range: 2014-2025 (12 seasons).
# Floor is 2014L -- cfbfastR RDS files exist earlier but EPA coverage and
# schema consistency are unreliable before 2014. This is the established
# cfbfastR data floor and matches the NFL 2014+ translation target.
CFB_SEASON_RANGE_DEFAULT <- 2014:2025
CFB_SEASON_MIN <- 2014L
CFB_SEASON_MAX <- as.integer(format(Sys.Date(), "%Y"))

# Cache directory for normalized college season files.
# Kept separate from NFL cache (data/season2_cache/) so the two datasets
# can be managed independently.
CFB_CACHE_DIR_DEFAULT <- here::here("data", "season2_cfb_cache")

# Schema normalization version tag -- increment when normalize_cfb_schema()
# logic changes. Distinct from NFL schema tag ("s2v1") to prevent confusion.
CFB_SCHEMA_NORM_VERSION <- "s2cfbv1"

# NFL-style alias mapping. For each entry, the alias column (LHS) is created
# as a copy of the CFB native column (RHS) during normalization, if the
# native column exists. Both columns are preserved in the output.
CFB_NFL_ALIASES <- list(
  posteam = "pos_team",
  defteam = "def_pos_team",
  epa     = "EPA",     # cfbfastR uses uppercase EPA
  pass    = "pass",
  rush    = "rush"
)

# Core columns that SHOULD exist in every normalized season.
CFB_CORE_COLUMNS <- c(
  "game_id", "season", "week", "season_type",
  "pos_team", "def_pos_team",
  "play_type", "play_text",
  "yards_gained", "down", "distance",
  "id_play",
  "home", "away"
)

# Optional columns that became available at different points in the
# cfbfastR data history. Added as NA if absent so downstream code can
# reference them safely.
CFB_OPTIONAL_COLUMNS <- c(
  # Division identifiers (present in all 2014-2025 seasons per diagnostic):
  "home_team_division", "away_team_division",
  # EPA/WPA variants:
  "EPA", "ppa", "wpa", "wp_before", "wp_after",
  # Player identifiers:
  "passer_player_name", "rusher_player_name", "receiver_player_name"
)

# Expected FBS team counts by season era. Validation range is deliberately
# wide to accommodate conference realignment and FBS additions.
#   2014-2016: ~128 FBS teams
#   2017-2020: ~130 FBS teams
#   2021-2025: 130-134 FBS teams
CFB_FBS_TEAMS_MIN <- 115L
CFB_FBS_TEAMS_MAX <- 140L

# Expected games-per-team range. Regular season is 12-13 games, plus
# conference championship for some teams, plus bowl or playoff for a subset.
CFB_GAMES_PER_TEAM_MIN <- 10
CFB_GAMES_PER_TEAM_MAX <- 16

# COVID season exception: 2020 was severely shortened. Several conferences
# opted out entirely; others played partial schedules. Mean games per team
# was ~8 vs the normal ~12. A separate floor of 6 is used for 2020 only
# so that validate_cfb_season_coverage() does not flag a known structural
# event as a data error.
CFB_GAMES_PER_TEAM_MIN_COVID <- 6L
CFB_COVID_SEASON <- 2020L

# Regular season week threshold.
CFB_REGULAR_SEASON_MAX_WEEK <- 15L


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

# Null-coalescing operator: returns b if a is NULL, else a.
`%||%` <- function(a, b) if (is.null(a)) b else a

# Declare NSE column names to suppress R CMD check "no visible binding" notes.
utils::globalVariables(c(
  "pos_team", "game_id", "status", "season", "all_pass",
  "home", "away", "home_team_division", "away_team_division"
))


#' Filter CFB Play-by-Play by Division (Internal Helper)
#'
#' Single source of truth for the FBS/FCS division filter. Keeps plays where
#' the offensive team (pos_team) belongs to the requested division, using
#' the embedded home_team_division / away_team_division columns present in
#' cfbfastR PBP data for all seasons 2014-2025.
#'
#' Strategy (Strategy C from diagnostic Check 4): keep a play when
#' (pos_team == home AND tolower(home_team_division) == target) OR
#' (pos_team == away AND tolower(away_team_division) == target).
#' This yields 134 FBS teams and 143,929 plays for 2024 -- consistent
#' with known FBS enrollment.
#'
#' Not exported. Called by load_normalized_cfb_season() and
#' validate_cfb_season_coverage().
#'
#' @param pbp A normalized CFB play-by-play tibble.
#' @param division Character. One of "fbs", "fcs", or "all".
#' @return Filtered tibble, or pbp unchanged if division == "all" or the
#'   required columns are absent (warning issued in that case).
.filter_cfb_by_division <- function(pbp, division = c("fbs", "fcs", "all")) {

  division <- match.arg(division)

  if (division == "all") {
    return(pbp)
  }

  required_cols <- c("pos_team", "home", "away",
                     "home_team_division", "away_team_division")
  missing_cols <- setdiff(required_cols, names(pbp))

  if (length(missing_cols) > 0) {
    warning(
      glue("Cannot apply division filter '{division}': missing columns: ",
           "{paste(missing_cols, collapse = ', ')}. ",
           "Returning unfiltered data."),
      call. = FALSE
    )
    return(pbp)
  }

  target <- tolower(division)

  pbp_filtered <- pbp[
    (pbp$pos_team == pbp$home &
       tolower(pbp$home_team_division) == target) |
      (pbp$pos_team == pbp$away &
         tolower(pbp$away_team_division) == target),
    ,
    drop = FALSE
  ]

  pbp_filtered
}


# ==============================================================================
# FUNCTION: load_multi_season_cfb_pbp
# ==============================================================================

#' Load Multiple Seasons of cfbfastR Play-by-Play Data
#'
#' Downloads, normalizes, and caches cfbfastR play-by-play data for the
#' specified season range. Each season is processed individually to manage
#' memory: load -> normalize -> cache -> free -> next season.
#'
#' Mirrors the structural pattern of R/15's load_multi_season_pbp() for
#' nflfastR data, with college-appropriate defaults.
#'
#' @param seasons Integer vector of seasons to load. Default: 2014:2025.
#'   Minimum supported season: 2014L (cfbfastR EPA/schema floor).
#' @param cache_dir Character path to cache directory.
#'   Default: data/season2_cfb_cache/
#' @param force_reload Logical. If TRUE, re-downloads and overwrites cached
#'   seasons. Default: FALSE
#' @param verbose Logical. If TRUE (default), print progress messages.
#'
#' @return Invisibly, a tibble with one row per season summarizing load
#'   results: season (int), status (chr: "cached"|"loaded"|"error"),
#'   n_plays (int), n_games (int), cache_file (chr).
#'
#' @details
#' Does NOT return the full play-by-play data. Data is cached as individual
#' season RDS files. Use load_normalized_cfb_season() to retrieve a single
#' season.
#'
#' cfbfastR context: load_cfb_pbp() pulls RDS files from the
#' sportsdataverse-data GitHub releases repository. This does NOT consume
#' CFBD API calls. Season loads are therefore not rate-limited.
#'
#' 2022 play count note: cfbfastR expanded FCS coverage in 2022. Raw play
#' counts jump ~54% from 2021 to 2022. This is expected. The FBS filter
#' in load_normalized_cfb_season() removes FCS plays downstream.
#'
#' @examples
#' # Load all 12 seasons (first run: network download; subsequent: skips cached)
#' summary <- load_multi_season_cfb_pbp()
#'
#' # Load subset for testing
#' summary <- load_multi_season_cfb_pbp(seasons = 2023:2025)
#'
#' # Force re-download of everything
#' summary <- load_multi_season_cfb_pbp(force_reload = TRUE)
#'
#' @seealso normalize_cfb_schema, validate_cfb_season_coverage,
#'   load_normalized_cfb_season
#' @export
load_multi_season_cfb_pbp <- function(seasons = CFB_SEASON_RANGE_DEFAULT,
                                      cache_dir = CFB_CACHE_DIR_DEFAULT,
                                      force_reload = FALSE,
                                      verbose = TRUE) {

  # --- Input validation ---
  if (!is.numeric(seasons) || length(seasons) == 0) {
    stop("'seasons' must be a non-empty numeric vector.", call. = FALSE)
  }
  seasons <- as.integer(seasons)

  if (any(seasons < CFB_SEASON_MIN) || any(seasons > CFB_SEASON_MAX)) {
    bad <- seasons[seasons < CFB_SEASON_MIN | seasons > CFB_SEASON_MAX]
    stop(
      glue("All seasons must be between {CFB_SEASON_MIN} and {CFB_SEASON_MAX}. ",
           "Received: {paste(bad, collapse = ', ')}"),
      call. = FALSE
    )
  }

  if (!is.logical(force_reload) || length(force_reload) != 1) {
    stop("'force_reload' must be a single logical value.", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value.", call. = FALSE)
  }

  # --- Create cache directory ---
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    if (verbose) message(glue("Created cache directory: {cache_dir}"))
  }

  # --- Process each season ---
  n_seasons <- length(seasons)
  results   <- vector("list", n_seasons)

  if (verbose) {
    message(strrep("=", 60))
    message(glue("Loading {n_seasons} CFB season(s): {min(seasons)}-{max(seasons)}"))
    message(glue("Cache directory: {cache_dir}"))
    message(glue("Force reload: {force_reload}"))
    message(strrep("=", 60))
  }

  for (i in seq_along(seasons)) {
    s          <- seasons[i]
    cache_file <- file.path(cache_dir, glue("cfb_pbp_normalized_{s}.rds"))

    # Check cache
    if (file.exists(cache_file) && !force_reload) {
      if (verbose) {
        message(glue("[{i}/{n_seasons}] Season {s}: cached (skipping download)"))
      }

      cached     <- readRDS(cache_file)
      results[[i]] <- tibble::tibble(
        season     = s,
        status     = "cached",
        n_plays    = nrow(cached),
        n_games    = length(unique(cached$game_id)),
        cache_file = cache_file
      )
      rm(cached)
      gc(verbose = FALSE)
      next
    }

    # Download and normalize
    if (verbose) {
      message(glue("[{i}/{n_seasons}] Season {s}: downloading from cfbfastR..."))
    }

    download_result <- tryCatch({
      data <- cfbfastR::load_cfb_pbp(seasons = s)
      list(ok = TRUE, data = data, err_msg = NULL)
    }, error = function(e) {
      list(ok = FALSE, data = NULL, err_msg = e$message)
    })

    pbp_raw <- download_result$data

    if (!download_result$ok || is.null(pbp_raw) || nrow(pbp_raw) == 0) {
      err_msg <- download_result$err_msg %||% "empty data"
      warning(glue("Season {s} load failed: {err_msg}"), call. = FALSE)
      results[[i]] <- tibble::tibble(
        season     = s,
        status     = "error",
        n_plays    = NA_integer_,
        n_games    = NA_integer_,
        cache_file = NA_character_
      )
      next
    }

    if (verbose) {
      message(glue("  Raw: {nrow(pbp_raw)} plays, ",
                   "{length(unique(pbp_raw$game_id))} games"))
    }

    # Normalize schema
    pbp_norm <- tryCatch({
      normalize_cfb_schema(pbp_raw, season = s)
    }, error = function(e) {
      warning(glue("Season {s} normalization failed: {e$message}"),
              call. = FALSE)
      NULL
    })

    if (is.null(pbp_norm)) {
      results[[i]] <- tibble::tibble(
        season     = s,
        status     = "error",
        n_plays    = NA_integer_,
        n_games    = NA_integer_,
        cache_file = NA_character_
      )
      rm(pbp_raw)
      gc(verbose = FALSE)
      next
    }

    if (verbose) {
      message(glue("  Normalized: {ncol(pbp_norm)} columns, ",
                   "version {CFB_SCHEMA_NORM_VERSION}"))
    }

    # Cache to disk
    saveRDS(pbp_norm, cache_file)
    if (verbose) message(glue("  Cached: {cache_file}"))

    results[[i]] <- tibble::tibble(
      season     = s,
      status     = "loaded",
      n_plays    = nrow(pbp_norm),
      n_games    = length(unique(pbp_norm$game_id)),
      cache_file = cache_file
    )

    # Free memory before next season
    rm(pbp_raw, pbp_norm)
    gc(verbose = FALSE)
  }

  summary_df <- dplyr::bind_rows(results)

  # Final report
  if (verbose) {
    n_ok        <- sum(summary_df$status %in% c("cached", "loaded"))
    n_err       <- sum(summary_df$status == "error")
    total_plays <- sum(summary_df$n_plays, na.rm = TRUE)

    message(strrep("=", 60))
    message(glue("Load complete: {n_ok} season(s) ok, {n_err} error(s)"))
    message(glue("Total plays across loaded seasons: ",
                 "{format(total_plays, big.mark = ',')}"))
    message(strrep("=", 60))
  }

  invisible(summary_df)
}


# ==============================================================================
# FUNCTION: normalize_cfb_schema
# ==============================================================================

#' Normalize cfbfastR Schema Across Seasons
#'
#' Standardizes column presence and types across seasons. Adds NFL-style
#' alias columns (posteam, defteam) as copies of CFB native columns
#' (pos_team, def_pos_team) to enable downstream consistency with NFL
#' modules. Does NOT rename native columns or filter divisions.
#'
#' @param pbp A tibble returned by cfbfastR::load_cfb_pbp() for a single
#'   season.
#' @param season Integer. The season being normalized (used for logging
#'   and attribute tagging).
#'
#' @return The input tibble with:
#'   - NFL-style alias columns added where the source column exists
#'   - Missing CFB_OPTIONAL_COLUMNS added as NA
#'   - Schema version attribute set: attr(result, "schema_norm_version")
#'   - Season attribute set: attr(result, "season")
#'
#' @details
#' Aliasing rules (both columns preserved):
#'   posteam := pos_team
#'   defteam := def_pos_team
#'   epa     := EPA (cfbfastR uses uppercase EPA)
#'
#' If a source column is absent, the alias is not created.
#'
#' @examples
#' raw  <- cfbfastR::load_cfb_pbp(2024)
#' norm <- normalize_cfb_schema(raw, season = 2024L)
#' attr(norm, "schema_norm_version")  # "s2cfbv1"
#'
#' @seealso load_multi_season_cfb_pbp, get_cfb_schema_differences
#' @export
normalize_cfb_schema <- function(pbp, season) {

  # --- Input validation ---
  if (!is.data.frame(pbp)) {
    stop("'pbp' must be a data frame or tibble.", call. = FALSE)
  }
  if (nrow(pbp) == 0) {
    stop("'pbp' is empty. Cannot normalize a 0-row tibble.", call. = FALSE)
  }
  if (!is.numeric(season) || length(season) != 1) {
    stop("'season' must be a single integer.", call. = FALSE)
  }
  season <- as.integer(season)

  # --- Add NFL-style alias columns where source column exists ---
  for (alias_name in names(CFB_NFL_ALIASES)) {
    source_name <- CFB_NFL_ALIASES[[alias_name]]

    if (alias_name %in% names(pbp)) next       # preserve existing
    if (!(source_name %in% names(pbp))) next   # skip if source absent

    pbp[[alias_name]] <- pbp[[source_name]]
  }

  # --- Add missing optional columns as NA ---
  missing_optional <- setdiff(CFB_OPTIONAL_COLUMNS, names(pbp))
  for (col in missing_optional) {
    pbp[[col]] <- NA
  }

  # --- Minimal type coercions ---
  if ("season" %in% names(pbp) && !is.integer(pbp$season)) {
    pbp$season <- as.integer(pbp$season)
  }

  if ("week" %in% names(pbp) && !is.integer(pbp$week)) {
    suppressWarnings({
      coerced <- as.integer(pbp$week)
    })
    if (sum(is.na(coerced)) == sum(is.na(pbp$week))) {
      pbp$week <- coerced
    }
  }

  # --- Attach attributes ---
  attr(pbp, "schema_norm_version") <- CFB_SCHEMA_NORM_VERSION
  attr(pbp, "season")              <- season
  attr(pbp, "normalized_at")       <- Sys.time()

  pbp
}


# ==============================================================================
# FUNCTION: validate_cfb_season_coverage
# ==============================================================================

#' Validate CFB Season Coverage
#'
#' Runs college-appropriate sanity checks on cached CFB play-by-play data.
#' Validation thresholds are relaxed vs NFL because FBS team counts and
#' games-per-team counts vary more than the NFL's fixed structure.
#'
#' Checks performed per season:
#'   - FBS team count in range [CFB_FBS_TEAMS_MIN, CFB_FBS_TEAMS_MAX]
#'     (only applied when division == "fbs")
#'   - Mean games per team in range [CFB_GAMES_PER_TEAM_MIN,
#'     CFB_GAMES_PER_TEAM_MAX]
#'   - Row count >= 30,000 plays
#'
#' @param seasons Integer vector of seasons to validate. Default: all
#'   cached seasons in cache_dir.
#' @param cache_dir Character path to cache directory.
#' @param division Character. One of "fbs" (default), "fcs", or "all".
#'   Validation is run against the filtered subset, so team and game
#'   counts reflect the requested division only.
#'
#' @return A tibble with one row per season and columns:
#'   season, division, n_teams, n_games, max_games_per_team,
#'   mean_games_per_team, min_week, max_week, n_plays, all_pass (logical),
#'   failures (chr).
#'
#' @examples
#' coverage <- validate_cfb_season_coverage()
#' coverage %>% dplyr::filter(!all_pass)
#'
#' # Validate FBS-only subset explicitly
#' coverage_fbs <- validate_cfb_season_coverage(division = "fbs")
#'
#' @seealso load_multi_season_cfb_pbp, get_cfb_schema_differences
#' @export
validate_cfb_season_coverage <- function(seasons = NULL,
                                         cache_dir = CFB_CACHE_DIR_DEFAULT,
                                         division = c("fbs", "fcs", "all")) {

  division <- match.arg(division)

  # --- Resolve seasons ---
  if (is.null(seasons)) {
    cached_files <- list.files(cache_dir,
                               pattern = "^cfb_pbp_normalized_\\d{4}\\.rds$")
    if (length(cached_files) == 0) {
      stop(glue("No cached CFB seasons found in {cache_dir}. ",
                "Run load_multi_season_cfb_pbp() first."),
           call. = FALSE)
    }
    seasons <- as.integer(gsub("cfb_pbp_normalized_|\\.rds", "", cached_files))
  } else {
    seasons <- as.integer(seasons)
  }

  # --- Iterate seasons ---
  results <- vector("list", length(seasons))

  for (i in seq_along(seasons)) {
    s          <- seasons[i]
    cache_file <- file.path(cache_dir, glue("cfb_pbp_normalized_{s}.rds"))

    if (!file.exists(cache_file)) {
      warning(glue("Season {s}: no cache file. Skipping."), call. = FALSE)
      next
    }

    pbp_raw <- readRDS(cache_file)

    # Apply division filter via single helper call
    pbp <- .filter_cfb_by_division(pbp_raw, division = division)
    rm(pbp_raw)

    # Team count
    n_teams <- if ("pos_team" %in% names(pbp)) {
      length(unique(pbp$pos_team[!is.na(pbp$pos_team)]))
    } else {
      NA_integer_
    }

    # Game count
    n_games <- length(unique(pbp$game_id[!is.na(pbp$game_id)]))

    # Games per team
    if ("pos_team" %in% names(pbp)) {
      team_game_counts <- pbp %>%
        dplyr::filter(!is.na(pos_team), !is.na(game_id)) %>%
        dplyr::distinct(pos_team, game_id) %>%
        dplyr::count(pos_team)

      max_games_per_team <- if (nrow(team_game_counts) > 0) {
        max(team_game_counts$n)
      } else {
        NA_integer_
      }
      mean_games_per_team <- if (nrow(team_game_counts) > 0) {
        mean(team_game_counts$n)
      } else {
        NA_real_
      }
    } else {
      max_games_per_team  <- NA_integer_
      mean_games_per_team <- NA_real_
    }

    # Week range
    min_week <- if ("week" %in% names(pbp)) {
      suppressWarnings(min(pbp$week, na.rm = TRUE))
    } else {
      NA_integer_
    }
    max_week <- if ("week" %in% names(pbp)) {
      suppressWarnings(max(pbp$week, na.rm = TRUE))
    } else {
      NA_integer_
    }

    if (is.infinite(min_week)) min_week <- NA_integer_
    if (is.infinite(max_week)) max_week <- NA_integer_

    n_plays <- nrow(pbp)

    # --- Check status ---
    # FBS team count thresholds only apply when filtering to FBS
    failures <- character(0)

    if (division == "fbs") {
      if (!is.na(n_teams)) {
        if (n_teams < CFB_FBS_TEAMS_MIN) {
          failures <- c(failures,
                        glue("team_count_too_low ({n_teams} < {CFB_FBS_TEAMS_MIN})"))
        }
        if (n_teams > CFB_FBS_TEAMS_MAX) {
          failures <- c(failures,
                        glue("team_count_too_high ({n_teams} > {CFB_FBS_TEAMS_MAX})"))
        }
      } else {
        failures <- c(failures, "no_pos_team_column")
      }
    }

    # 2020 COVID exception: use the lower floor for the shortened season
    games_min_this_season <- if (s == CFB_COVID_SEASON) {
      CFB_GAMES_PER_TEAM_MIN_COVID
    } else {
      CFB_GAMES_PER_TEAM_MIN
    }

    if (!is.na(mean_games_per_team)) {
      if (mean_games_per_team < games_min_this_season) {
        failures <- c(failures,
                      glue("mean_games_per_team_low ",
                           "({round(mean_games_per_team, 1)} < ",
                           "{games_min_this_season})"))
      }
      if (mean_games_per_team > CFB_GAMES_PER_TEAM_MAX) {
        failures <- c(failures,
                      glue("mean_games_per_team_high ",
                           "({round(mean_games_per_team, 1)} > ",
                           "{CFB_GAMES_PER_TEAM_MAX})"))
      }
    }

    if (!is.na(n_plays) && n_plays < 30000L) {
      failures <- c(failures,
                    glue("play_count_too_low ",
                         "({format(n_plays, big.mark = ',')} < 30,000)"))
    }

    all_pass <- length(failures) == 0

    results[[i]] <- tibble::tibble(
      season              = s,
      division            = division,
      n_teams             = n_teams,
      n_games             = n_games,
      max_games_per_team  = max_games_per_team,
      mean_games_per_team = round(mean_games_per_team, 2),
      min_week            = min_week,
      max_week            = max_week,
      n_plays             = n_plays,
      all_pass            = all_pass,
      failures            = if (length(failures) == 0) "" else
        paste(failures, collapse = "; ")
    )

    rm(pbp)
    gc(verbose = FALSE)
  }

  dplyr::bind_rows(results)
}


# ==============================================================================
# FUNCTION: get_cfb_schema_differences
# ==============================================================================

#' Compare Column Presence Across Cached CFB Seasons
#'
#' Scans cached RDS files and returns a wide tibble indicating which
#' columns exist in which seasons. Essential for Week 7+ aggregation
#' code to know which columns are safe to use across the full time
#' range vs which are season-dependent.
#'
#' @param cache_dir Character path to cache directory.
#' @param seasons Integer vector of seasons to compare. Default: all
#'   cached seasons.
#'
#' @return A tibble with column "column" and one logical column per
#'   season indicating presence (TRUE = column exists in that season,
#'   FALSE = absent). Also includes "n_seasons_present" and
#'   "pct_seasons_present" summary columns.
#'
#' @examples
#' diffs <- get_cfb_schema_differences()
#' # Columns present in all seasons:
#' diffs %>% dplyr::filter(pct_seasons_present == 1)
#' # Schema drift columns:
#' diffs %>% dplyr::filter(pct_seasons_present < 1, pct_seasons_present > 0)
#'
#' @seealso load_multi_season_cfb_pbp, normalize_cfb_schema
#' @export
get_cfb_schema_differences <- function(cache_dir = CFB_CACHE_DIR_DEFAULT,
                                       seasons = NULL) {

  # --- Resolve seasons ---
  if (is.null(seasons)) {
    cached_files <- list.files(cache_dir,
                               pattern = "^cfb_pbp_normalized_\\d{4}\\.rds$")
    if (length(cached_files) == 0) {
      stop(glue("No cached CFB seasons found in {cache_dir}. ",
                "Run load_multi_season_cfb_pbp() first."),
           call. = FALSE)
    }
    seasons <- sort(as.integer(gsub("cfb_pbp_normalized_|\\.rds", "",
                                    cached_files)))
  } else {
    seasons <- sort(as.integer(seasons))
  }

  # --- Collect column sets per season ---
  # Memory management: only store column NAMES, never full pbp
  col_sets <- vector("list", length(seasons))
  names(col_sets) <- as.character(seasons)

  for (i in seq_along(seasons)) {
    s          <- seasons[i]
    cache_file <- file.path(cache_dir, glue("cfb_pbp_normalized_{s}.rds"))

    if (!file.exists(cache_file)) {
      warning(glue("Season {s}: no cache file. Skipping."), call. = FALSE)
      next
    }

    pbp                             <- readRDS(cache_file)
    col_sets[[as.character(s)]]     <- names(pbp)
    rm(pbp)
    gc(verbose = FALSE)
  }

  # Drop null entries (seasons with missing files)
  col_sets <- col_sets[!vapply(col_sets, is.null, logical(1))]

  if (length(col_sets) == 0) {
    stop("No usable cached seasons found.", call. = FALSE)
  }

  # --- Build presence matrix ---
  all_columns <- sort(unique(unlist(col_sets)))
  presence    <- tibble::tibble(column = all_columns)

  for (season_key in names(col_sets)) {
    presence[[paste0("s", season_key)]] <- all_columns %in% col_sets[[season_key]]
  }

  # --- Summary columns ---
  season_cols              <- paste0("s", names(col_sets))
  presence$n_seasons_present  <- rowSums(presence[season_cols])
  presence$pct_seasons_present <- presence$n_seasons_present / length(col_sets)

  # Order by presence (most universal first)
  presence <- presence[order(-presence$pct_seasons_present, presence$column), ]

  presence
}


# ==============================================================================
# FUNCTION: load_normalized_cfb_season
# ==============================================================================

#' Load a Single Normalized CFB Season from Cache
#'
#' Reads one cached RDS file of normalized CFB play-by-play data. This is
#' the downstream workhorse function that Week 7+ code uses to access CFB
#' data without re-downloading.
#'
#' The FBS filter is applied at THIS call site (not at cache-write time)
#' so that the cache contains all divisions and can serve FCS queries
#' when needed (e.g., for Week 8 ID reconciliation across transfers).
#'
#' @param season Integer. The season to load.
#' @param cache_dir Character path to cache directory.
#' @param division Character. One of "fbs" (default), "fcs", or "all".
#'   Filtering delegates to .filter_cfb_by_division().
#'
#' @return A tibble of normalized play-by-play data for the specified
#'   season and division subset. Preserves schema_norm_version attribute.
#'
#' @examples
#' # Load FBS-only plays from 2024
#' pbp_2024 <- load_normalized_cfb_season(2024)
#'
#' # Load all divisions
#' pbp_2024_all <- load_normalized_cfb_season(2024, division = "all")
#'
#' @seealso load_multi_season_cfb_pbp, normalize_cfb_schema
#' @export
load_normalized_cfb_season <- function(season,
                                       cache_dir = CFB_CACHE_DIR_DEFAULT,
                                       division = c("fbs", "fcs", "all")) {

  # --- Input validation ---
  if (!is.numeric(season) || length(season) != 1) {
    stop("'season' must be a single integer.", call. = FALSE)
  }
  season <- as.integer(season)

  division   <- match.arg(division)
  cache_file <- file.path(cache_dir, glue("cfb_pbp_normalized_{season}.rds"))

  if (!file.exists(cache_file)) {
    stop(glue("Season {season} not cached at {cache_file}. ",
              "Run load_multi_season_cfb_pbp(seasons = {season}) first."),
         call. = FALSE)
  }

  pbp <- readRDS(cache_file)

  # --- Deduplication guard ---
  # cfbfastR cached files for actively-updated seasons (e.g. 2025) can
  # contain duplicate rows from double-loading during mid-season cache builds.
  # Confirmed in 2025: 23,800 duplicate game_id + id_play rows, concentrated
  # in Week 1, inflating player-season row counts ~5x vs expected.
  # Deduplication is applied at read time so all callers receive clean data
  # without requiring a cache rebuild. If duplicates are found, a warning
  # directs the user to fix the cache with force_reload = TRUE.
  # id_play is the cfbfastR play identifier; game_id scopes it to a game.
  if (all(c("game_id", "id_play") %in% names(pbp))) {
    n_before_dedup <- nrow(pbp)
    pbp <- pbp[!duplicated(pbp[, c("game_id", "id_play")]), , drop = FALSE]
    n_removed <- n_before_dedup - nrow(pbp)
    if (n_removed > 0L) {
      warning(
        glue("Season {season}: removed {format(n_removed, big.mark = ',')} ",
             "duplicate game_id + id_play rows at read time. ",
             "Run load_multi_season_cfb_pbp(seasons = {season}, ",
             "force_reload = TRUE) to rebuild a clean cache."),
        call. = FALSE
      )
    }
  }

  # Apply division filter via single helper call
  pbp_filtered <- .filter_cfb_by_division(pbp, division = division)

  # Preserve schema attributes
  attr(pbp_filtered, "schema_norm_version") <- attr(pbp, "schema_norm_version")
  attr(pbp_filtered, "season")              <- attr(pbp, "season")
  attr(pbp_filtered, "division_filter")     <- division
  attr(pbp_filtered, "normalized_at")       <- attr(pbp, "normalized_at")

  pbp_filtered
}


# ==============================================================================
# FUNCTION: run_week6_pipeline
# ==============================================================================

#' Orchestrate the Full Week 6 Pipeline
#'
#' Convenience function that runs load + validate + schema diff for the
#' specified season range. Prints a summary and returns the validation
#' results. Intended for first-time setup and for re-running after cache
#' corruption or schema version bumps.
#'
#' @param seasons Integer vector. Default: 2014:2025.
#' @param cache_dir Character path to cache directory.
#' @param force_reload Logical. If TRUE, re-downloads all seasons.
#'
#' @return Invisibly, a list with:
#'   - load_summary: tibble from load_multi_season_cfb_pbp()
#'   - coverage: tibble from validate_cfb_season_coverage()
#'   - schema_diffs: tibble from get_cfb_schema_differences()
#'
#' @examples
#' # Full pipeline (first run: long; subsequent: short)
#' pipeline_out <- run_week6_pipeline()
#'
#' # Check failures
#' pipeline_out$coverage %>% dplyr::filter(!all_pass)
#'
#' @seealso load_multi_season_cfb_pbp, validate_cfb_season_coverage,
#'   get_cfb_schema_differences
#' @export
run_week6_pipeline <- function(seasons = CFB_SEASON_RANGE_DEFAULT,
                               cache_dir = CFB_CACHE_DIR_DEFAULT,
                               force_reload = FALSE) {

  message(strrep("#", 60))
  message("Season 2 Week 6: CFB Multi-Season Pipeline")
  message(glue("Seasons: {min(seasons)}-{max(seasons)}"))
  message(glue("Cache:   {cache_dir}"))
  message(strrep("#", 60))

  # Step 1: Load
  message("\nSTEP 1/3: Loading and caching CFB seasons...")
  load_summary <- load_multi_season_cfb_pbp(
    seasons      = seasons,
    cache_dir    = cache_dir,
    force_reload = force_reload,
    verbose      = TRUE
  )

  # Step 2: Validate (FBS filter applied inside validate_cfb_season_coverage)
  message("\nSTEP 2/3: Validating season coverage (FBS filter)...")
  loaded_seasons <- load_summary %>%
    dplyr::filter(status != "error") %>%
    dplyr::pull(season)

  if (length(loaded_seasons) == 0) {
    stop("No seasons loaded successfully. Cannot continue.", call. = FALSE)
  }

  coverage <- validate_cfb_season_coverage(
    seasons   = loaded_seasons,
    cache_dir = cache_dir,
    division  = "fbs"
  )

  n_pass <- sum(coverage$all_pass, na.rm = TRUE)
  n_fail <- sum(!coverage$all_pass, na.rm = TRUE)
  message(glue("  Coverage: {n_pass} pass, {n_fail} fail"))

  if (n_fail > 0) {
    failed <- coverage %>%
      dplyr::filter(!all_pass) %>%
      dplyr::pull(season)
    message(glue("  Failed seasons: {paste(failed, collapse = ', ')}"))
  }

  # Step 3: Schema differences
  message("\nSTEP 3/3: Computing schema differences across seasons...")
  schema_diffs <- get_cfb_schema_differences(
    cache_dir = cache_dir,
    seasons   = loaded_seasons
  )

  n_universal <- sum(schema_diffs$pct_seasons_present == 1, na.rm = TRUE)
  n_partial   <- sum(schema_diffs$pct_seasons_present < 1 &
                       schema_diffs$pct_seasons_present > 0, na.rm = TRUE)
  message(glue("  Columns: {n_universal} universal, {n_partial} partial coverage"))

  # Final summary
  message(strrep("#", 60))
  message("Pipeline complete.")
  message(strrep("#", 60))

  invisible(list(
    load_summary = load_summary,
    coverage     = coverage,
    schema_diffs = schema_diffs
  ))
}
