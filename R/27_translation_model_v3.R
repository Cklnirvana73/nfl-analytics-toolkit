# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 13
# College-to-NFL Translation Model v3
# File: R/27_translation_model_v3.R
#
# Purpose: Extends the Week 10 translation model (R/24) with six new feature
#          groups identified as structural gaps in the v1 architecture:
#
#   1. Combine athleticism   -- ht, wt, forty, vertical, broad_jump
#                               (nflreadr::load_combine(); cone excluded,
#                               vertical/broad_jump excluded for QB)
#   2. Production slope      -- OLS slope of efficiency metric across CFB
#                               seasons (rec_epa_slope, rush_epa_slope,
#                               pass_epa_slope). Min 2 seasons required.
#   3. Age x competition     -- draft_age * sos_opp_def_epa_per_play
#                               interaction. Age-adjusted SOS signal.
#   4. Recruiting composite  -- 247Sports rating + stars from
#                               cfbfastR::cfbd_recruiting_player().
#                               Fallback: try draft_year-3, -4, -5 in order.
#                               Cache-first: s2_week13_recruiting.rds.
#   5. Receiving role proxy  -- rec_role_share, rush_to_rec_ratio.
#                               Derived from R/21 panel; no new data.
#   6. Breakout age          -- age at first season above position efficiency
#                               median (training-set median, computed per pos).
#                               seasons_since_breakout also included.
#   7. Teammate talent density -- judges production relative to the position
#                               opportunity environment: pos_recruiting_density
#                               (recruited talent stacked at the position),
#                               pos_volume_concentration and
#                               pos_scoring_concentration (teammate share of
#                               touches / points -- high = opportunity
#                               suppressed), plus density_x_role interaction.
#                               Same team, same position group, any career
#                               overlap. WR/TE share the receiver pool.
#
# Architecture:
#   Sources R/24_translation_model.R, inheriting all internal helpers:
#     .normalize_player_name(), .compute_team_pass_attempts(),
#     .apply_multiseason_weights(), .impute_features(),
#     .fit_elastic_net_loco(), .compute_nfl_outcomes()
#   Adds five new internal helpers:
#     .normalize_team_name(), .load_combine_features(),
#     .load_recruiting_features(), .compute_production_slopes(),
#     .compute_breakout_age()
#   Plus two for teammate talent density (v3.1):
#     .load_recruiting_board(), .compute_teammate_density()
#   Exports two new functions:
#     build_translation_features_v3()
#     run_week13_pipeline()
#
# Key decisions (confirmed in brainstorm session):
#   - LOCO CV strategy retained from R/24 (leave-one-draft-class-out)
#   - Elastic Net model retained for R-side baseline
#   - cone excluded: coverage collapses to near-zero in 2022-2023
#   - vertical/broad_jump excluded for QB: no established translation signal
#   - Recruiting fallback: draft_year-3, draft_year-4, draft_year-5 in order
#   - All NA features: median imputation (training-set medians)
#   - CSV export as final step: feeds Week 13 Python/MLflow notebook
#   - R/24 v1 is the correct baseline (v2 produced worse results)
#
# Source dependencies:
#   R/24_translation_model.R    -- all v1 helpers and exported functions
#   R/20_multi_season_cfb_pbp.R -- load_normalized_cfb_season() (via R/24)
#   R/21_cfb_player_season_panel.R -- build_cfb_player_season_panel() (via R/24)
#   R/16_player_season_panel.R  -- build_player_season_panel() (via R/24)
#   data/season2_cfb_cache/s2_week8_cfb_sos_panel.rds (via R/24)
#
# Outputs (written by run_week13_pipeline()):
#   data/season2_cache/s2_week13_crosswalk.rds
#   data/season2_cache/s2_week13_feature_matrix.rds
#   data/season2_cache/s2_week13_models.rds
#   data/season2_cache/s2_week13_performance.rds
#   data/season2_cache/s2_week13_predictions.rds
#   data/season2_cache/s2_week13_feature_matrix.csv   <-- Python notebook input
#   data/season2_cfb_cache/s2_week13_recruiting.rds   <-- recruiting cache
#
# Navigation:
#   Line  ~90  : Libraries
#   Line ~120  : Source guard (R/24)
#   Line ~150  : Constants
#   Line ~210  : NSE declarations (v3 additions)
#   Line ~240  : Internal helpers
#                  .normalize_team_name()
#                  .load_combine_features()
#                  .load_recruiting_features()
#                  .compute_production_slopes()
#                  .compute_breakout_age()
#   Line ~620  : build_translation_features_v3()
#   Line ~1050 : run_week13_pipeline()
#
# Season 2 output prefix : s2_week13_
# Schema tag             : s2_w13_v3
# Author                 : Christian LeBlanc
# Created                : 2026-05
# ==============================================================================


# ==============================================================================
# LIBRARIES
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(here)
library(nflreadr)
library(cfbfastR)

if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop(
    "Package 'glmnet' is required. Install with: install.packages('glmnet')",
    call. = FALSE
  )
}


# ==============================================================================
# SOURCE GUARD: R/24_translation_model.R
# ==============================================================================

# R/24 provides all v1 infrastructure: internal helpers, link_cfb_to_nfl(),
# build_translation_features(), train_translation_model(),
# evaluate_translation_accuracy(), identify_translation_gaps(),
# validate_translation_assumptions(), and all constants.
if (!exists("link_cfb_to_nfl", mode = "function")) {
  week24_path <- here::here("R", "24_translation_model.R")
  if (!file.exists(week24_path)) {
    stop(glue(
      "R/24_translation_model.R not found at: {week24_path}\n",
      "R/27 requires R/24 for all v1 infrastructure."
    ), call. = FALSE)
  }
  source(week24_path)
}


# ==============================================================================
# CONSTANTS (v3 additions -- R/24 constants remain in effect)
# ==============================================================================

# Schema tag for v3 outputs
V3_SCHEMA_TAG <- "s2_w13_v3_1"

# Output prefix for all v3 files
V3_OUTPUT_PREFIX <- "s2_week13_"

# Combine columns to use. cone excluded: coverage near-zero in 2022-2023.
# vertical and broad_jump excluded for QB (see COMBINE_COLS_BY_POS below).
COMBINE_COLS_ALL <- c("ht", "wt", "forty", "vertical", "broad_jump")

# Position-specific combine feature inclusion (for documentation).
# QB: ht, wt, forty only -- vertical/broad_jump excluded (no established signal).
# RB/WR/TE: all five.
# Enforced via v3_cols_QB/RB/WR/TE in build_translation_features_v3().
COMBINE_COLS_BY_POS <- list(
  QB = c("ht", "wt", "forty"),
  RB = c("ht", "wt", "forty", "vertical", "broad_jump"),
  WR = c("ht", "wt", "forty", "vertical", "broad_jump"),
  TE = c("ht", "wt", "forty", "vertical", "broad_jump")
)

# Recruiting cache path
RECRUITING_CACHE_PATH <- here::here(
  "data", "season2_cfb_cache", "s2_week13_recruiting.rds"
)

# Recruiting year offsets to try in order (early entrant, standard, redshirt)
RECRUITING_YEAR_OFFSETS <- c(-3L, -4L, -5L)

# Minimum college seasons required to compute production slope.
# Players with only one CFB season get NA (imputed at median).
MIN_SEASONS_FOR_SLOPE <- 2L

# ------------------------------------------------------------------------------
# TEAMMATE TALENT DENSITY (v3.1 feature group)
# ------------------------------------------------------------------------------
# Judges college production relative to the opportunity environment, not in a
# vacuum. Three measures over each player's overlap window at his college team,
# at his position group, plus an interaction:
#   pos_recruiting_density   -- sum of overlapping same-position teammates' full
#                               247Sports recruiting composite (talent stacked
#                               at the position; full rating, not overlap-
#                               weighted)
#   pos_volume_concentration -- avg over the player's seasons of the share of
#                               same-position-group TOUCHES captured by teammates
#                               (high = player's opportunity was suppressed)
#   pos_scoring_concentration-- same, for a simple yards+TD points composite
#   density_x_role           -- player's own role share x volume concentration
#                               (held a role despite a dense, productive room)
#
# Teammate set: same primary_team, same position_group, ANY career-window
# overlap (a one-season brush counts; full recruiting rating regardless of
# overlap length). Position group is the CFB panel's QB/RB/WR_TE, so WR and TE
# share the receiver opportunity pool by design (they compete for targets and
# the panel groups them as WR_TE).
#
# All four enter the feature matrix; the Elastic Net learns the weights. No
# hand-tuned multiplier on score_final downstream (R/28 inherits via the model).
#
# Recruiting density requires the NATIONAL recruiting board (all recruits at the
# position, not just future-NFL ones), a broader pull than the matched-only
# .load_recruiting_features(). Cached separately.
RECRUITING_BOARD_CACHE_PATH <- here::here(
  "data", "season2_cfb_cache", "s2_week13_recruiting_board.rds"
)

# Points composite weights for scoring concentration (position-agnostic, simple
# and stable: yards contribute at 0.1/yd, TDs at 6). Used only for the relative
# concentration ratio, so absolute scale is irrelevant -- only the split between
# a player and his teammates matters.
DENSITY_POINTS_PER_YARD <- 0.1
DENSITY_POINTS_PER_TD   <- 6.0

# Percentile threshold for breakout detection. 0.50 = first season above
# position median on primary efficiency metric.
BREAKOUT_THRESHOLD_PERCENTILE <- 0.50

# --- R/24 v1 constant overrides ---
# R/24 v1 sets CUTOFF_YEAR <- 2022L. As of 2026 the 2023 draft class has
# 3 complete NFL seasons (2023, 2024, 2025) and must be included in training.
# These three constants are redefined here to override the v1 values.
# All downstream functions that reference CUTOFF_YEAR use this value.
CUTOFF_YEAR            <- 2023L
# Floor matches R/24 v1 (2015L). R/27 originally used 2017L, replicating
# v2's training-window narrowing -- a documented reason v2 performed worse.
# Restored to 2015L to retain the full v1 training population.
TRAINING_DRAFT_CLASSES <- 2015L:CUTOFF_YEAR
NFL_OUTCOME_SEASONS    <- 2015L:(CUTOFF_YEAR + 2L)

# Minimum average PPR per game for a player to be included in training outcome.
# 0 = all contributors included (default, matches original R/24 v1 behavior).
# Defined here because R/24 v1 does not include this constant; it was added
# in v2. R/27 defines it independently to remain self-contained.
MIN_PPR_OUTCOME <- 0

# CSV export path for Python notebook
V3_CSV_EXPORT_PATH_FN <- function(output_dir) {
  file.path(output_dir, "s2_week13_feature_matrix.csv")
}


# ==============================================================================
# NSE DECLARATIONS (v3 additions)
# ==============================================================================

utils::globalVariables(c(
  # combine
  "ht", "wt", "forty", "vertical", "broad_jump", "pos",
  # recruiting
  "name", "committed_to", "stars", "rating", "recruit_year",
  "norm_committed_to", "norm_cfb_team",
  "recruiting_rating", "recruiting_stars",
  # slopes
  "rec_epa_slope", "rush_epa_slope", "pass_epa_slope",
  "season_index", "metric_val", "slope_val",
  # breakout
  "breakout_age", "seasons_since_breakout", "breakout_season",
  "pos_median", "above_median",
  # receiving role
  "rec_role_share", "rush_to_rec_ratio", "total_plays",
  # interaction
  "sos_x_age",
  # teammate density (v3.1)
  "pos_recruiting_density", "pos_volume_concentration",
  "pos_scoring_concentration", "density_x_role",
  "position_group", "primary_team", "norm_primary_team",
  "team_pos_recruiting", "teammate_recruiting", "own_recruiting",
  "pos_touches", "pos_points", "team_pos_touches", "team_pos_points",
  "teammate_touches_share", "teammate_points_share",
  "first_season", "last_season", "overlap_start", "overlap_end",
  "board_rating", "norm_board_name", "norm_board_team",
  # misc v3
  "offset", "rec_year", "norm_recruit_name", "norm_player_name_v3",
  "combine_join_season", "pfr_player_name"
))


# ==============================================================================
# INTERNAL HELPERS (v3)
# ==============================================================================

# ------------------------------------------------------------------------------
# .normalize_team_name
#
# Strips punctuation, converts to lowercase, removes common university
# suffixes for matching between cfbfastR team names and recruiting
# committed_to school names.
# Not exported.
# ------------------------------------------------------------------------------
.normalize_team_name <- function(name) {
  if (is.null(name)) return(character(0L))
  name <- tolower(as.character(name))
  # Remove punctuation and special characters
  name <- gsub("[^a-z0-9 ]", "", name)
  # Remove common university tokens that vary between sources
  name <- gsub(
    "\\b(university|college|state|the|of|at|a&m|tech|&)\\b", "", name
  )
  # Collapse multiple spaces
  name <- gsub("\\s+", " ", name)
  trimws(name)
}


# ------------------------------------------------------------------------------
# .load_combine_features
#
# Loads nflreadr::load_combine(), filters to draft classes in crosswalk,
# joins on pfr_player_name + draft_year (season in combine data).
# Returns ht, wt, forty, vertical, broad_jump keyed on nfl_gsis_id.
# NA rate reported per column per position.
# Not exported.
# ------------------------------------------------------------------------------
.load_combine_features <- function(crosswalk, verbose = TRUE) {

  stopifnot(is.data.frame(crosswalk))

  required_cw_cols <- c("nfl_gsis_id", "draft_year", "draft_position",
                        "pfr_player_name")
  missing_cw <- setdiff(required_cw_cols, names(crosswalk))
  if (length(missing_cw) > 0L) {
    stop(glue(
      ".load_combine_features(): crosswalk missing columns: ",
      "{paste(missing_cw, collapse = ', ')}"
    ), call. = FALSE)
  }

  if (verbose) message("Loading combine data from nflreadr...")

  combine_raw <- tryCatch(
    nflreadr::load_combine(),
    error = function(e) {
      stop(glue(
        ".load_combine_features(): nflreadr::load_combine() failed.\n",
        "Error: {conditionMessage(e)}"
      ), call. = FALSE)
    }
  )

  if (verbose) message(glue(
    "  Combine rows loaded: {format(nrow(combine_raw), big.mark = ',')}"
  ))

  # load_combine() uses 'player_name' (not 'pfr_player_name').
  # 'pos' is the position column; 'season' is the combine/draft year.
  required_combine_cols <- c("season", "player_name", COMBINE_COLS_ALL)
  missing_combine <- setdiff(required_combine_cols, names(combine_raw))
  if (length(missing_combine) > 0L) {
    stop(glue(
      ".load_combine_features(): load_combine() missing columns: ",
      "{paste(missing_combine, collapse = ', ')}\n",
      "Available: {paste(names(combine_raw), collapse = ', ')}"
    ), call. = FALSE)
  }

  # Filter combine to draft years present in crosswalk
  cw_years <- unique(crosswalk$draft_year[!is.na(crosswalk$draft_year)])

  combine_filtered <- combine_raw %>%
    dplyr::filter(season %in% cw_years) %>%
    dplyr::select(
      season, player_name,
      dplyr::all_of(COMBINE_COLS_ALL)
    ) %>%
    dplyr::rename(draft_year = season) %>%
    dplyr::mutate(
      # Parse ht from character "6-4" format to numeric inches.
      # nflreadr::load_combine() returns ht as character (e.g., "6-4").
      # grepl guard ensures non-conforming values become NA rather than erroring.
      ht = dplyr::if_else(
        !is.na(ht) & grepl("^\\d+-\\d+$", ht),
        as.numeric(sub("-.", "", ht)) * 12L + as.numeric(sub(".-", "", ht)),
        NA_real_
      ),
      # Normalize combine player_name to match crosswalk's pfr_player_name
      norm_pfr_name = .normalize_player_name(player_name)
    )

  # Prepare crosswalk side -- join using pfr_player_name from draft_data
  cw_for_join <- crosswalk %>%
    dplyr::filter(!is.na(nfl_gsis_id), !is.na(pfr_player_name)) %>%
    dplyr::select(nfl_gsis_id, draft_year, draft_position, pfr_player_name) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      norm_pfr_name = .normalize_player_name(pfr_player_name)
    )

  # Check for duplicates in combine before joining
  combine_dups <- combine_filtered %>%
    dplyr::count(draft_year, norm_pfr_name) %>%
    dplyr::filter(n > 1L)

  if (nrow(combine_dups) > 0L && verbose) {
    message(glue(
      "  WARNING: {nrow(combine_dups)} duplicate rows in combine data ",
      "(same name + draft_year). Taking first row per player."
    ))
    combine_filtered <- combine_filtered %>%
      dplyr::distinct(draft_year, norm_pfr_name, .keep_all = TRUE)
  }

  # Left join on normalized name + draft_year
  result <- cw_for_join %>%
    dplyr::left_join(
      combine_filtered %>%
        dplyr::select(draft_year, norm_pfr_name,
                      dplyr::all_of(COMBINE_COLS_ALL)),
      by = c("draft_year", "norm_pfr_name")
    ) %>%
    dplyr::select(nfl_gsis_id, draft_position,
                  dplyr::all_of(COMBINE_COLS_ALL))

  # Report NA rates per column per position
  if (verbose) {
    message("  Combine NA rates by position:")
    for (pos in c("QB", "RB", "WR", "TE")) {
      pos_rows <- result %>% dplyr::filter(draft_position == pos)
      if (nrow(pos_rows) == 0L) next
      na_rates <- purrr::map_chr(COMBINE_COLS_ALL, function(col) {
        pct <- round(100 * mean(is.na(pos_rows[[col]])), 1)
        glue("{col}: {pct}%")
      })
      message(glue("    {pos}: {paste(na_rates, collapse = ' | ')}"))
    }
  }

  result
}


# ------------------------------------------------------------------------------
# .load_recruiting_features
#
# Cache-first pull of cfbd_recruiting_player() across all relevant recruiting
# years. For each player in crosswalk, tries draft_year + offset for offsets
# in RECRUITING_YEAR_OFFSETS (-3, -4, -5) and takes the first match on
# normalized name + normalized school.
# Returns recruiting_rating and recruiting_stars keyed on cfb_player_name +
# cfb_primary_team.
# Not exported.
# ------------------------------------------------------------------------------
.load_recruiting_features <- function(crosswalk,
                                       cache_path = RECRUITING_CACHE_PATH,
                                       verbose    = TRUE) {

  stopifnot(is.data.frame(crosswalk))

  required_cw_cols <- c("cfb_player_name", "cfb_primary_team",
                        "draft_year", "draft_position")
  missing_cw <- setdiff(required_cw_cols, names(crosswalk))
  if (length(missing_cw) > 0L) {
    stop(glue(
      ".load_recruiting_features(): crosswalk missing columns: ",
      "{paste(missing_cw, collapse = ', ')}"
    ), call. = FALSE)
  }

  # Cache-first: load if exists
  if (file.exists(cache_path)) {
    if (verbose) message(glue(
      "Loading recruiting cache from: {cache_path}"
    ))
    return(readRDS(cache_path))
  }

  if (verbose) message(
    "Recruiting cache not found. Pulling from cfbfastR API..."
  )

  # Determine all recruiting years needed
  cw_matched <- crosswalk %>%
    dplyr::filter(
      !is.na(cfb_player_name),
      !is.na(cfb_primary_team),
      !is.na(draft_year)
    ) %>%
    dplyr::select(cfb_player_name, cfb_primary_team,
                  draft_year, draft_position) %>%
    dplyr::distinct()

  all_recruit_years <- unique(as.vector(outer(
    unique(cw_matched$draft_year),
    RECRUITING_YEAR_OFFSETS,
    `+`
  )))
  all_recruit_years <- sort(all_recruit_years[all_recruit_years >= 2010L])

  if (verbose) message(glue(
    "  Pulling {length(all_recruit_years)} recruiting years: ",
    "{min(all_recruit_years)}-{max(all_recruit_years)}"
  ))

  # Pull all positions for each year.
  # cfbd_recruiting_player() does not accept "QB".
  # QBs split into "PRO" (pocket) and "DUAL" (dual-threat); both pulled.
  # Match to QB draft position happens downstream via name normalization.
  positions_to_pull <- c("PRO", "DUAL", "RB", "WR", "TE")

  recruit_raw_list <- list()

  for (yr in all_recruit_years) {
    for (pos in positions_to_pull) {
      key <- glue("{yr}_{pos}")
      result <- tryCatch({
        dat <- cfbfastR::cfbd_recruiting_player(
          year     = yr,
          position = pos
        )
        if (!is.null(dat) && nrow(dat) > 0L) dat else NULL
      }, error = function(e) {
        if (verbose) message(glue(
          "  WARNING: API call failed for year={yr}, pos={pos}: ",
          "{conditionMessage(e)}"
        ))
        NULL
      })
      if (!is.null(result)) recruit_raw_list[[key]] <- result
    }

    if (verbose && yr %% 3L == 0L) {
      message(glue("  Pulled through year {yr}..."))
    }
  }

  if (length(recruit_raw_list) == 0L) {
    if (verbose) message(
      "  WARNING: No recruiting data returned. All recruiting features will be NA."
    )
    result_empty <- cw_matched %>%
      dplyr::mutate(
        recruiting_rating = NA_real_,
        recruiting_stars  = NA_integer_
      ) %>%
      dplyr::select(cfb_player_name, cfb_primary_team,
                    recruiting_rating, recruiting_stars)
    saveRDS(result_empty, cache_path)
    return(result_empty)
  }

  recruit_all <- dplyr::bind_rows(recruit_raw_list)

  # Validate columns
  required_rec_cols <- c("name", "committed_to", "year", "stars", "rating")
  missing_rec <- setdiff(required_rec_cols, names(recruit_all))
  if (length(missing_rec) > 0L) {
    stop(glue(
      ".load_recruiting_features(): cfbd_recruiting_player() missing columns: ",
      "{paste(missing_rec, collapse = ', ')}"
    ), call. = FALSE)
  }

  recruit_all <- recruit_all %>%
    dplyr::select(name, committed_to, year, stars, rating) %>%
    dplyr::rename(recruit_year = year) %>%
    dplyr::mutate(
      norm_recruit_name  = .normalize_player_name(name),
      norm_committed_to  = .normalize_team_name(committed_to)
    )

  # For each crosswalk player, try offsets in order
  cw_matched <- cw_matched %>%
    dplyr::mutate(
      norm_player_name_v3 = .normalize_player_name(cfb_player_name),
      norm_cfb_team       = .normalize_team_name(cfb_primary_team)
    )

  match_results <- purrr::map_dfr(seq_len(nrow(cw_matched)), function(i) {
    p_norm  <- cw_matched$norm_player_name_v3[i]
    t_norm  <- cw_matched$norm_cfb_team[i]
    d_year  <- cw_matched$draft_year[i]
    p_name  <- cw_matched$cfb_player_name[i]
    p_team  <- cw_matched$cfb_primary_team[i]

    matched <- NULL
    for (offset in RECRUITING_YEAR_OFFSETS) {
      rec_yr <- d_year + offset
      candidates <- recruit_all %>%
        dplyr::filter(
          recruit_year == rec_yr,
          norm_recruit_name == p_norm
        )

      if (nrow(candidates) == 0L) next

      # Try name + school match first
      school_match <- candidates %>%
        dplyr::filter(norm_committed_to == t_norm)

      if (nrow(school_match) >= 1L) {
        matched <- school_match[1L, ]
        break
      }

      # Name-only fallback if school match fails
      if (nrow(candidates) == 1L) {
        matched <- candidates[1L, ]
        break
      }
    }

    if (is.null(matched)) {
      return(tibble::tibble(
        cfb_player_name   = p_name,
        cfb_primary_team  = p_team,
        recruiting_rating = NA_real_,
        recruiting_stars  = NA_integer_
      ))
    }

    tibble::tibble(
      cfb_player_name   = p_name,
      cfb_primary_team  = p_team,
      recruiting_rating = as.numeric(matched$rating[1L]),
      recruiting_stars  = as.integer(matched$stars[1L])
    )
  })

  # Report match rate
  if (verbose) {
    n_matched <- sum(!is.na(match_results$recruiting_rating))
    n_total   <- nrow(match_results)
    message(glue(
      "  Recruiting match rate: {n_matched} / {n_total} ",
      "({round(100 * n_matched / n_total, 1)}%)"
    ))
  }

  # Cache result
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(match_results, cache_path)
  if (verbose) message(glue("  Recruiting cache saved to: {cache_path}"))

  match_results
}


# ------------------------------------------------------------------------------
# .compute_production_slopes
#
# For each matched player in crosswalk, fits an OLS slope of their primary
# efficiency metric across college seasons (season_index = 1, 2, 3 ...).
# Requires MIN_SEASONS_FOR_SLOPE seasons; single-season players return NA.
# Uses base R lm() via purrr::map_dfr to stay dependency-light.
#
# Returns per-player tibble with rec_epa_slope, rush_epa_slope, pass_epa_slope
# keyed on cfb_player_name.
# Not exported.
# ------------------------------------------------------------------------------
.compute_production_slopes <- function(cfb_panel, crosswalk, verbose = TRUE) {

  stopifnot(is.data.frame(cfb_panel), is.data.frame(crosswalk))

  required_panel <- c("player_name", "season", "rec_epa_per_target",
                      "rush_epa_per_attempt", "pass_epa_per_attempt")
  missing_panel <- setdiff(required_panel, names(cfb_panel))
  if (length(missing_panel) > 0L) {
    stop(glue(
      ".compute_production_slopes(): cfb_panel missing columns: ",
      "{paste(missing_panel, collapse = ', ')}"
    ), call. = FALSE)
  }

  if (verbose) message("Computing production slopes across college seasons...")

  matched_players <- crosswalk %>%
    dplyr::filter(!is.na(cfb_player_name)) %>%
    dplyr::select(cfb_player_name, cfb_final_season,
                  draft_position) %>%
    dplyr::distinct()

  slopes <- purrr::map_dfr(seq_len(nrow(matched_players)), function(i) {
    p_name   <- matched_players$cfb_player_name[i]
    final_s  <- matched_players$cfb_final_season[i]
    d_pos    <- matched_players$draft_position[i]

    player_seasons <- cfb_panel %>%
      dplyr::filter(
        player_name == p_name,
        season <= final_s
      ) %>%
      dplyr::arrange(season) %>%
      dplyr::mutate(season_index = seq_len(dplyr::n()))

    # Compute OLS slope helper -- returns NA if fewer than MIN_SEASONS_FOR_SLOPE
    .slope <- function(y, x) {
      valid <- !is.na(y) & !is.na(x)
      if (sum(valid) < MIN_SEASONS_FOR_SLOPE) return(NA_real_)
      coef(lm(y[valid] ~ x[valid]))[2L]
    }

    rec_slope  <- .slope(
      player_seasons$rec_epa_per_target,
      player_seasons$season_index
    )
    rush_slope <- .slope(
      player_seasons$rush_epa_per_attempt,
      player_seasons$season_index
    )
    pass_slope <- .slope(
      player_seasons$pass_epa_per_attempt,
      player_seasons$season_index
    )

    tibble::tibble(
      cfb_player_name = p_name,
      rec_epa_slope   = rec_slope,
      rush_epa_slope  = rush_slope,
      pass_epa_slope  = pass_slope
    )
  })

  if (verbose) {
    n_rec  <- sum(!is.na(slopes$rec_epa_slope))
    n_rush <- sum(!is.na(slopes$rush_epa_slope))
    n_pass <- sum(!is.na(slopes$pass_epa_slope))
    n_tot  <- nrow(slopes)
    message(glue(
      "  Slopes computed: rec={n_rec}/{n_tot}, ",
      "rush={n_rush}/{n_tot}, pass={n_pass}/{n_tot}"
    ))
  }

  slopes
}


# ------------------------------------------------------------------------------
# .compute_breakout_age
#
# For each matched player, finds the first college season where their primary
# efficiency metric crossed the position median (computed on training set only
# to prevent leakage). Returns breakout_age and seasons_since_breakout.
#
# Position-metric mapping:
#   QB  -> pass_epa_per_attempt
#   RB  -> rush_epa_per_attempt
#   WR  -> rec_epa_per_target
#   TE  -> rec_epa_per_target
#
# breakout_age: approximated as (draft_age - (draft_year - breakout_season))
# seasons_since_breakout: draft_year - 1 - breakout_season (years before draft)
#
# Players who never exceeded the median return NA for both.
# Not exported.
# ------------------------------------------------------------------------------
.compute_breakout_age <- function(cfb_panel, crosswalk,
                                   cutoff_year = CUTOFF_YEAR,
                                   verbose     = TRUE) {

  stopifnot(is.data.frame(cfb_panel), is.data.frame(crosswalk))

  pos_metric_map <- list(
    QB = "pass_epa_per_attempt",
    RB = "rush_epa_per_attempt",
    WR = "rec_epa_per_target",
    TE = "rec_epa_per_target"
  )

  required_panel <- c("player_name", "season",
                      "pass_epa_per_attempt",
                      "rush_epa_per_attempt",
                      "rec_epa_per_target")
  missing_panel <- setdiff(required_panel, names(cfb_panel))
  if (length(missing_panel) > 0L) {
    stop(glue(
      ".compute_breakout_age(): cfb_panel missing columns: ",
      "{paste(missing_panel, collapse = ', ')}"
    ), call. = FALSE)
  }

  if (verbose) message("Computing breakout ages...")

  # Compute position medians from training set only (draft_year <= cutoff_year)
  training_players <- crosswalk %>%
    dplyr::filter(
      !is.na(cfb_player_name),
      draft_year <= cutoff_year,
      draft_position %in% c("QB", "RB", "WR", "TE")
    ) %>%
    dplyr::select(cfb_player_name, cfb_final_season, draft_position) %>%
    dplyr::distinct()

  pos_medians <- purrr::map(
    c("QB", "RB", "WR", "TE"),
    function(pos) {
      metric <- pos_metric_map[[pos]]
      pos_players <- training_players %>%
        dplyr::filter(draft_position == pos)

      all_vals <- cfb_panel %>%
        dplyr::filter(
          player_name %in% pos_players$cfb_player_name
        ) %>%
        dplyr::pull(!!metric)

      median(all_vals, na.rm = TRUE)
    }
  ) %>%
  stats::setNames(c("QB", "RB", "WR", "TE"))

  if (verbose) {
    message("  Position efficiency medians (training set):")
    for (pos in c("QB", "RB", "WR", "TE")) {
      message(glue(
        "    {pos} ({pos_metric_map[[pos]]}): ",
        "{round(pos_medians[[pos]], 4)}"
      ))
    }
  }

  # All matched players (training + prediction)
  all_matched <- crosswalk %>%
    dplyr::filter(
      !is.na(cfb_player_name),
      draft_position %in% c("QB", "RB", "WR", "TE")
    ) %>%
    dplyr::select(cfb_player_name, cfb_final_season,
                  draft_year, draft_age, draft_position) %>%
    dplyr::distinct()

  breakout_results <- purrr::map_dfr(
    seq_len(nrow(all_matched)),
    function(i) {
      p_name   <- all_matched$cfb_player_name[i]
      final_s  <- all_matched$cfb_final_season[i]
      d_year   <- all_matched$draft_year[i]
      d_age    <- all_matched$draft_age[i]
      d_pos    <- all_matched$draft_position[i]
      metric   <- pos_metric_map[[d_pos]]
      thresh   <- pos_medians[[d_pos]]

      player_seasons <- cfb_panel %>%
        dplyr::filter(
          player_name == p_name,
          season <= final_s
        ) %>%
        dplyr::arrange(season)

      if (nrow(player_seasons) == 0L || is.na(thresh)) {
        return(tibble::tibble(
          cfb_player_name      = p_name,
          breakout_age         = NA_real_,
          seasons_since_breakout = NA_integer_
        ))
      }

      # Find first season above position median
      metric_vals <- player_seasons[[metric]]
      above       <- !is.na(metric_vals) & metric_vals > thresh
      first_above <- which(above)[1L]

      if (is.na(first_above)) {
        return(tibble::tibble(
          cfb_player_name      = p_name,
          breakout_age         = NA_real_,
          seasons_since_breakout = NA_integer_
        ))
      }

      breakout_s <- player_seasons$season[first_above]

      # breakout_age: draft_age minus years between breakout season and draft
      # draft_year - 1 is the final CFB season; breakout_season may be earlier
      years_before_draft <- (d_year - 1L) - breakout_s
      b_age <- if (!is.na(d_age)) d_age - years_before_draft else NA_real_

      tibble::tibble(
        cfb_player_name        = p_name,
        breakout_age           = b_age,
        seasons_since_breakout = as.integer(years_before_draft)
      )
    }
  )

  if (verbose) {
    n_breakout <- sum(!is.na(breakout_results$breakout_age))
    n_total    <- nrow(breakout_results)
    message(glue(
      "  Breakout age computed: {n_breakout} / {n_total} players ",
      "({round(100 * n_breakout / n_total, 1)}%)"
    ))
  }

  breakout_results
}


# ------------------------------------------------------------------------------
# .load_recruiting_board
#
# National recruiting board: all recruits at QB/RB/WR/TE across the relevant
# year range, aggregated to a normalized name + normalized school + recruit
# year key with their 247Sports composite rating. Unlike
# .load_recruiting_features() (which keeps only matched/future-NFL players),
# this retains EVERYONE so a player's non-NFL teammates carry ratings for the
# pos_recruiting_density computation. Cache-first.
#
# Returns: tibble(norm_board_name, norm_board_team, recruit_year, board_rating).
# Not exported.
# ------------------------------------------------------------------------------
.load_recruiting_board <- function(years,
                                   positions_to_pull = c("PRO", "DUAL", "RB",
                                                         "WR", "TE"),
                                   cache_path = RECRUITING_BOARD_CACHE_PATH,
                                   verbose = TRUE) {

  # NOTE: cfbd_recruiting_player() does not accept "QB". Quarterbacks are split
  # into "PRO" (pro-style) and "DUAL" (dual-threat) in the 247 recruiting
  # taxonomy. Both map to the panel's QB position_group. RB/WR/TE match
  # directly. This is why positions_to_pull defaults to the recruiting codes,
  # not the panel labels.
  if (file.exists(cache_path)) {
    if (verbose) message(glue("  Recruiting board cache hit: {cache_path}"))
    return(readRDS(cache_path))
  }

  if (verbose) message(glue(
    "  Building national recruiting board for years ",
    "{min(years)}-{max(years)} (no cache)..."
  ))

  board_list <- list()
  for (yr in years) {
    for (pos in positions_to_pull) {
      key <- glue("{yr}_{pos}")
      result <- tryCatch({
        dat <- cfbfastR::cfbd_recruiting_player(year = yr, position = pos)
        if (!is.null(dat) && nrow(dat) > 0L) dat else NULL
      }, error = function(e) {
        if (verbose) message(glue(
          "    WARNING: board pull failed year={yr} pos={pos}: ",
          "{conditionMessage(e)}"
        ))
        NULL
      })
      if (!is.null(result)) board_list[[key]] <- result
    }
  }

  if (length(board_list) == 0L) {
    if (verbose) message(
      "  WARNING: recruiting board empty. pos_recruiting_density will be NA."
    )
    empty <- tibble::tibble(
      norm_board_name = character(0L),
      norm_board_team = character(0L),
      recruit_year    = integer(0L),
      board_rating    = numeric(0L)
    )
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(empty, cache_path)
    return(empty)
  }

  board_all <- dplyr::bind_rows(board_list)

  required_cols <- c("name", "committed_to", "year", "rating")
  missing_cols  <- setdiff(required_cols, names(board_all))
  if (length(missing_cols) > 0L) {
    stop(glue(
      ".load_recruiting_board(): cfbd_recruiting_player() missing columns: ",
      "{paste(missing_cols, collapse = ', ')}"
    ), call. = FALSE)
  }

  board <- board_all %>%
    dplyr::transmute(
      norm_board_name = .normalize_player_name(.data$name),
      norm_board_team = .normalize_team_name(.data$committed_to),
      recruit_year    = as.integer(.data$year),
      board_rating    = as.numeric(.data$rating)
    ) %>%
    dplyr::filter(!is.na(.data$board_rating), nchar(.data$norm_board_name) > 0) %>%
    # One rating per name-team-year (recruiting sources occasionally duplicate).
    dplyr::distinct(.data$norm_board_name, .data$norm_board_team,
                    .data$recruit_year, .keep_all = TRUE)

  if (verbose) message(glue(
    "  Recruiting board: {format(nrow(board), big.mark = ',')} rated recruits"
  ))

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(board, cache_path)
  board
}


# ------------------------------------------------------------------------------
# .compute_teammate_density
#
# Per-matched-player teammate talent density features. For each player in the
# crosswalk, finds same-team, same-position-group teammates with ANY career
# overlap and computes:
#   pos_recruiting_density   -- sum of those teammates' board_rating
#   pos_volume_concentration -- avg over the player's seasons of teammate share
#                               of position-group touches
#   pos_scoring_concentration-- same for a yards+TD points composite
# The density_x_role interaction is built later in build_translation_features_v3
# (it needs the player's role share, joined there).
#
# Position-group touch/scoring definitions (CFB panel position_group values
# are QB / RB / WR_TE; WR_TE is the combined receiver pool by design):
#   QB    : touches = pass_attempts; points = passing_yards*0.1 + pass_tds*6
#   RB    : touches = rush_attempts; points = rushing_yards*0.1 + rush_tds*6
#   WR_TE : touches = targets;       points = receiving_yards*0.1 + rec_tds*6
#
# Returns: tibble(cfb_player_name, pos_recruiting_density,
#   pos_volume_concentration, pos_scoring_concentration). Not exported.
# ------------------------------------------------------------------------------
.compute_teammate_density <- function(cfb_panel, crosswalk, recruiting_board,
                                      verbose = TRUE) {

  stopifnot(is.data.frame(cfb_panel), is.data.frame(crosswalk),
            is.data.frame(recruiting_board))

  required_panel <- c("player_name", "season", "primary_team",
                      "position_group",
                      "pass_attempts", "rush_attempts", "targets",
                      "passing_yards", "rushing_yards", "receiving_yards",
                      "pass_tds", "rush_tds", "rec_tds")
  missing_panel <- setdiff(required_panel, names(cfb_panel))
  if (length(missing_panel) > 0L) {
    stop(glue(
      ".compute_teammate_density(): cfb_panel missing columns: ",
      "{paste(missing_panel, collapse = ', ')}"
    ), call. = FALSE)
  }

  if (verbose) message("Computing teammate talent density...")

  # Per player-season touches and points by position group, with a normalized
  # team key. One row per player-season (panel is already collapsed to
  # primary_team per player-season).
  panel_pos <- cfb_panel %>%
    dplyr::filter(
      !is.na(.data$position_group),
      !is.na(.data$primary_team),
      nchar(.data$primary_team) > 0
    ) %>%
    dplyr::mutate(
      norm_primary_team = .normalize_team_name(.data$primary_team),
      pos_touches = dplyr::case_when(
        .data$position_group == "QB"    ~ dplyr::coalesce(.data$pass_attempts, 0),
        .data$position_group == "RB"    ~ dplyr::coalesce(.data$rush_attempts, 0),
        .data$position_group == "WR_TE" ~ dplyr::coalesce(.data$targets, 0),
        TRUE ~ 0
      ),
      pos_points = dplyr::case_when(
        .data$position_group == "QB" ~
          dplyr::coalesce(.data$passing_yards, 0) * DENSITY_POINTS_PER_YARD +
          dplyr::coalesce(.data$pass_tds, 0) * DENSITY_POINTS_PER_TD,
        .data$position_group == "RB" ~
          dplyr::coalesce(.data$rushing_yards, 0) * DENSITY_POINTS_PER_YARD +
          dplyr::coalesce(.data$rush_tds, 0) * DENSITY_POINTS_PER_TD,
        .data$position_group == "WR_TE" ~
          dplyr::coalesce(.data$receiving_yards, 0) * DENSITY_POINTS_PER_YARD +
          dplyr::coalesce(.data$rec_tds, 0) * DENSITY_POINTS_PER_TD,
        TRUE ~ 0
      )
    ) %>%
    dplyr::select(.data$player_name, .data$season, .data$norm_primary_team,
                  .data$position_group, .data$pos_touches, .data$pos_points)

  # Team-position-season totals for the concentration denominators.
  team_pos_season <- panel_pos %>%
    dplyr::group_by(.data$norm_primary_team, .data$position_group,
                    .data$season) %>%
    dplyr::summarise(
      team_pos_touches = sum(.data$pos_touches, na.rm = TRUE),
      team_pos_points  = sum(.data$pos_points,  na.rm = TRUE),
      .groups = "drop"
    )

  # Matched players to score, with their college career window and team.
  matched <- crosswalk %>%
    dplyr::filter(!is.na(.data$cfb_player_name),
                  !is.na(.data$cfb_primary_team)) %>%
    dplyr::select(.data$cfb_player_name, .data$cfb_primary_team) %>%
    dplyr::distinct()

  # Each matched player's panel seasons (their actual career rows).
  player_career <- panel_pos %>%
    dplyr::rename(cfb_player_name = .data$player_name)

  density <- purrr::map_dfr(seq_len(nrow(matched)), function(i) {
    p_name <- matched$cfb_player_name[i]
    p_team_norm <- .normalize_team_name(matched$cfb_primary_team[i])

    # The player's own seasons at his primary team.
    own <- player_career %>%
      dplyr::filter(.data$cfb_player_name == p_name,
                    .data$norm_primary_team == p_team_norm)

    if (nrow(own) == 0L) {
      return(tibble::tibble(
        cfb_player_name = p_name,
        pos_recruiting_density   = NA_real_,
        pos_volume_concentration = NA_real_,
        pos_scoring_concentration = NA_real_
      ))
    }

    p_pos    <- own$position_group[1L]
    p_seasons <- sort(unique(own$season))

    # --- Volume / scoring concentration ---
    # For each of the player's seasons, teammate share = (team total - own) /
    # team total at that team-position-season. Average across the player's
    # seasons. Teammate share, not player share, so high = suppressed.
    own_by_season <- own %>%
      dplyr::group_by(.data$season) %>%
      dplyr::summarise(
        own_touches = sum(.data$pos_touches, na.rm = TRUE),
        own_points  = sum(.data$pos_points,  na.rm = TRUE),
        .groups = "drop"
      )

    conc <- own_by_season %>%
      dplyr::left_join(
        team_pos_season %>%
          dplyr::filter(.data$norm_primary_team == p_team_norm,
                        .data$position_group == p_pos) %>%
          dplyr::select(.data$season, .data$team_pos_touches,
                        .data$team_pos_points),
        by = "season"
      ) %>%
      dplyr::mutate(
        teammate_touches_share = dplyr::if_else(
          !is.na(.data$team_pos_touches) & .data$team_pos_touches > 0,
          (.data$team_pos_touches - .data$own_touches) / .data$team_pos_touches,
          NA_real_
        ),
        teammate_points_share = dplyr::if_else(
          !is.na(.data$team_pos_points) & .data$team_pos_points > 0,
          (.data$team_pos_points - .data$own_points) / .data$team_pos_points,
          NA_real_
        )
      )

    vol_conc   <- mean(conc$teammate_touches_share, na.rm = TRUE)
    score_conc <- mean(conc$teammate_points_share,  na.rm = TRUE)

    # --- Recruiting density ---
    # Same-team, same-position-group teammates with ANY season overlap. Their
    # full board rating summed. Exclude the player himself. The board is keyed
    # by recruit class year, not panel season, so a teammate is anyone who
    # appears in the panel at the same team+position-group in any of the
    # player's seasons (career-window overlap), then matched to the board by
    # normalized name + team for their rating.
    teammates <- panel_pos %>%
      dplyr::filter(
        .data$norm_primary_team == p_team_norm,
        .data$position_group == p_pos,
        .data$season %in% p_seasons,
        .data$player_name != p_name
      ) %>%
      dplyr::distinct(.data$player_name)

    if (nrow(teammates) == 0L) {
      rec_density <- 0
    } else {
      # Board is pre-filtered to non-NA ratings at load. Collapse to one rating
      # per teammate name via mean (avoids max()'s empty-group -Inf edge), then
      # sum. Unmatched teammates join to NA and are dropped by na.rm; the final
      # guard ensures no non-finite value can flow into the feature.
      board_team <- recruiting_board %>%
        dplyr::filter(.data$norm_board_team == p_team_norm,
                      is.finite(.data$board_rating)) %>%
        dplyr::group_by(.data$norm_board_name) %>%
        dplyr::summarise(
          board_rating = mean(.data$board_rating, na.rm = TRUE),
          .groups = "drop"
        )

      tm_ratings <- teammates %>%
        dplyr::mutate(
          norm_board_name = .normalize_player_name(.data$player_name)
        ) %>%
        dplyr::left_join(board_team, by = "norm_board_name")

      matched_ratings <- tm_ratings$board_rating[
        is.finite(tm_ratings$board_rating)
      ]
      rec_density <- if (length(matched_ratings) == 0L) {
        0
      } else {
        sum(matched_ratings)
      }
    }

    tibble::tibble(
      cfb_player_name = p_name,
      pos_recruiting_density    = rec_density,
      pos_volume_concentration  = if (is.nan(vol_conc))   NA_real_ else vol_conc,
      pos_scoring_concentration = if (is.nan(score_conc)) NA_real_ else score_conc
    )
  })

  if (verbose) {
    n_rec <- sum(!is.na(density$pos_recruiting_density) &
                   density$pos_recruiting_density > 0)
    n_vol <- sum(!is.na(density$pos_volume_concentration))
    message(glue(
      "  Density computed: {nrow(density)} players, ",
      "{n_rec} with recruiting density > 0, ",
      "{n_vol} with volume concentration"
    ))
  }

  density
}


# ==============================================================================
# FUNCTION: build_translation_features_v3
# ==============================================================================

#' Build v3 Translation Feature Matrix
#'
#' @description
#' Extends \code{build_translation_features()} from R/24 with six new feature
#' groups: combine athleticism, production slope, age-adjusted competition
#' interaction, recruiting composite, receiving role proxy, and breakout age.
#'
#' All new features with NA values receive median imputation (training-set
#' medians), consistent with the SOS imputation pattern in R/24.
#'
#' \strong{Feature inclusion by position:}
#' \itemize{
#'   \item QB: ht, wt, forty, pass_epa_slope,
#'     recruiting_rating, breakout_age, seasons_since_breakout
#'   \item RB: ht, wt, forty, vertical, broad_jump, rec_epa_slope,
#'     rush_epa_slope, recruiting_rating,
#'     rec_role_share, rush_to_rec_ratio, breakout_age,
#'     seasons_since_breakout
#'   \item WR: ht, wt, forty, vertical, broad_jump, rec_epa_slope,
#'     recruiting_rating, rec_role_share,
#'     breakout_age, seasons_since_breakout
#'   \item TE: same as WR
#' }
#'
#' \strong{Dropped after collinearity analysis:}
#' sos_x_age (r=0.998 with sos_opp_def_epa_per_play -- no independent signal);
#' recruiting_stars (r=0.907 with recruiting_rating -- coarse binning only).
#'
#' @param cfb_panel tibble. R/21 CFB player-season panel.
#' @param nfl_panel tibble. R/16 NFL player-season panel.
#' @param sos_panel tibble. R/22 SOS panel.
#' @param crosswalk tibble. Output of \code{link_cfb_to_nfl()}.
#' @param combine_features tibble. Output of \code{.load_combine_features()}.
#' @param recruiting_features tibble. Output of
#'   \code{.load_recruiting_features()}.
#' @param production_slopes tibble. Output of
#'   \code{.compute_production_slopes()}.
#' @param breakout_features tibble. Output of \code{.compute_breakout_age()}.
#' @param cutoff_year int. Last training draft class. Default: CUTOFF_YEAR.
#' @param min_ppr_outcome numeric. Minimum PPR outcome to include in training.
#'   Default: 0 (all contributors).
#' @param verbose logical. Progress output. Default: TRUE.
#'
#' @return A named list extending the R/24 feature_matrix structure with
#'   additional v3 columns in each position tibble and updated
#'   \code{base_feature_cols} / \code{enriched_feature_cols}.
#'   \code{schema_tag} set to \code{"s2_w13_v3"}.
#'
#' @seealso build_translation_features, run_week13_pipeline
#' @export
build_translation_features_v3 <- function(cfb_panel,
                                            nfl_panel,
                                            sos_panel,
                                            crosswalk,
                                            combine_features,
                                            recruiting_features,
                                            production_slopes,
                                            breakout_features,
                                            teammate_density,
                                            cutoff_year    = CUTOFF_YEAR,
                                            min_ppr_outcome = MIN_PPR_OUTCOME,
                                            verbose        = TRUE) {

  # --- Input validation ---
  stopifnot(
    is.data.frame(cfb_panel),     nrow(cfb_panel)     > 0L,
    is.data.frame(nfl_panel),     nrow(nfl_panel)     > 0L,
    is.data.frame(sos_panel),     nrow(sos_panel)     > 0L,
    is.data.frame(crosswalk),     nrow(crosswalk)     > 0L,
    is.data.frame(combine_features),
    is.data.frame(recruiting_features),
    is.data.frame(production_slopes),
    is.data.frame(breakout_features),
    is.data.frame(teammate_density),
    is.numeric(cutoff_year), length(cutoff_year) == 1L
  )
  cutoff_year <- as.integer(cutoff_year)

  message(strrep("=", 60))
  message("build_translation_features_v3()")
  message(glue("Cutoff year: {cutoff_year}"))
  message(strrep("=", 60))

  # --- Step 1: Build v1 feature matrix as base ---
  # Note: R/24 v1 build_translation_features() does not accept min_ppr_outcome.
  # That parameter was introduced in v2. Outcome filtering at the
  # run_week13_pipeline level uses the training split filter
  # (!is.na(ppr_per_game_y13)) which is equivalent to min_ppr_outcome = 0.
  message("\nStep 1: Building v1 base feature matrix via R/24...")
  v1_features <- build_translation_features(
    cfb_panel   = cfb_panel,
    nfl_panel   = nfl_panel,
    sos_panel   = sos_panel,
    crosswalk   = crosswalk,
    cutoff_year = cutoff_year
  )

  # Reconstruct the full player_features tibble from v1 outputs
  # (training + prediction rows combined for joining)
  training_rows   <- dplyr::bind_rows(v1_features$training)
  prediction_rows <- dplyr::bind_rows(v1_features$prediction)
  all_players     <- dplyr::bind_rows(training_rows, prediction_rows)

  message(glue(
    "  v1 base: {format(nrow(training_rows), big.mark = ',')} training + ",
    "{format(nrow(prediction_rows), big.mark = ',')} prediction rows"
  ))

  # --- Step 2: Join combine features ---
  message("\nStep 2: Joining combine athleticism features...")

  n_before <- nrow(all_players)

  combine_keyed <- combine_features %>%
    dplyr::select(nfl_gsis_id, dplyr::all_of(COMBINE_COLS_ALL)) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)

  all_players <- all_players %>%
    dplyr::left_join(combine_keyed, by = "nfl_gsis_id")

  stopifnot(nrow(all_players) == n_before)

  message(glue(
    "  Joined. Combine NA rate (forty): ",
    "{round(100 * mean(is.na(all_players$forty)), 1)}%"
  ))

  # --- Step 3: Join recruiting features ---
  message("\nStep 3: Joining recruiting composite features...")

  n_before <- nrow(all_players)

  recruiting_keyed <- recruiting_features %>%
    dplyr::distinct(cfb_player_name, cfb_primary_team, .keep_all = TRUE)

  all_players <- all_players %>%
    dplyr::left_join(
      recruiting_keyed,
      by = c("cfb_player_name", "cfb_primary_team")
    )

  stopifnot(nrow(all_players) == n_before)

  n_rec_matched <- sum(!is.na(all_players$recruiting_rating))
  message(glue(
    "  Recruiting match: {n_rec_matched} / {nrow(all_players)} players ",
    "({round(100 * n_rec_matched / nrow(all_players), 1)}%)"
  ))

  # --- Step 4: Join production slopes ---
  message("\nStep 4: Joining production slopes...")

  n_before <- nrow(all_players)

  slopes_keyed <- production_slopes %>%
    dplyr::distinct(cfb_player_name, .keep_all = TRUE)

  all_players <- all_players %>%
    dplyr::left_join(slopes_keyed, by = "cfb_player_name")

  stopifnot(nrow(all_players) == n_before)

  # --- Step 5: Join breakout features ---
  message("\nStep 5: Joining breakout age features...")

  n_before <- nrow(all_players)

  breakout_keyed <- breakout_features %>%
    dplyr::distinct(cfb_player_name, .keep_all = TRUE)

  all_players <- all_players %>%
    dplyr::left_join(breakout_keyed, by = "cfb_player_name")

  stopifnot(nrow(all_players) == n_before)

  n_breakout <- sum(!is.na(all_players$breakout_age))
  message(glue(
    "  Breakout age available: {n_breakout} / {nrow(all_players)} players"
  ))

  # --- Step 5.5: Join teammate talent density ---
  message("\nStep 5.5: Joining teammate talent density features...")

  n_before <- nrow(all_players)

  density_keyed <- teammate_density %>%
    dplyr::distinct(cfb_player_name, .keep_all = TRUE)

  all_players <- all_players %>%
    dplyr::left_join(density_keyed, by = "cfb_player_name")

  stopifnot(nrow(all_players) == n_before)

  n_density <- sum(!is.na(all_players$pos_volume_concentration))
  message(glue(
    "  Teammate density available: {n_density} / {nrow(all_players)} players"
  ))

  # --- Step 6: Compute derived features ---
  message("\nStep 6: Computing derived features (role proxy, interaction)...")

  # total_plays may not be in all_players if R/24 dropped it; recompute safely
  # rec_role_share and rush_to_rec_ratio are computed from cfb_panel via
  # the final college season of each player, then joined.
  role_features <- crosswalk %>%
    dplyr::filter(!is.na(cfb_player_name)) %>%
    dplyr::select(cfb_player_name, cfb_final_season) %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      cfb_panel %>%
        dplyr::select(player_name, season,
                      targets, rush_attempts, receptions, total_plays) %>%
        dplyr::rename(
          cfb_player_name  = player_name,
          cfb_final_season = season
        ),
      by = c("cfb_player_name", "cfb_final_season")
    ) %>%
    dplyr::mutate(
      rec_role_share = dplyr::if_else(
        !is.na(total_plays) & total_plays > 0L,
        (dplyr::coalesce(targets, 0L) +
           dplyr::coalesce(receptions, 0L)) / total_plays,
        NA_real_
      ),
      rush_to_rec_ratio = dplyr::if_else(
        !is.na(targets) & (targets + 1L) > 0L,
        dplyr::coalesce(rush_attempts, 0L) / (targets + 1L),
        NA_real_
      )
    ) %>%
    # Deduplicate on cfb_player_name -- transfer players may appear on
    # multiple teams in their final season, producing duplicate rows.
    # Keep the row with the most total_plays (primary team).
    dplyr::arrange(cfb_player_name, dplyr::desc(dplyr::coalesce(total_plays, 0L))) %>%
    dplyr::distinct(cfb_player_name, .keep_all = TRUE) %>%
    dplyr::select(cfb_player_name, rec_role_share, rush_to_rec_ratio)

  n_before <- nrow(all_players)
  all_players <- all_players %>%
    dplyr::left_join(role_features, by = "cfb_player_name")
  stopifnot(nrow(all_players) == n_before)

  # Age x competition interaction
  all_players <- all_players %>%
    dplyr::mutate(
      sos_x_age = dplyr::if_else(
        !is.na(sos_opp_def_epa_per_play) & !is.na(draft_age),
        sos_opp_def_epa_per_play * draft_age,
        NA_real_
      ),
      # Density x role: held a role despite a productive, crowded position room.
      # rec_role_share is the player's own opportunity share; multiplied by the
      # teammate volume concentration so the model can reward "produced while
      # buried behind productive teammates".
      density_x_role = dplyr::if_else(
        !is.na(rec_role_share) & !is.na(pos_volume_concentration),
        rec_role_share * pos_volume_concentration,
        NA_real_
      )
    )

  message(glue(
    "  rec_role_share non-NA: {sum(!is.na(all_players$rec_role_share))} | ",
    "sos_x_age non-NA: {sum(!is.na(all_players$sos_x_age))} | ",
    "density_x_role non-NA: {sum(!is.na(all_players$density_x_role))}"
  ))

  # --- Step 7: Define v3 feature columns per position ---
  message("\nStep 7: Building v3 feature column sets...")

  # v3 additions by position
  # sos_x_age dropped: r=0.998 with sos_opp_def_epa_per_play -- no independent
  # signal. Elastic Net splits coefficient unpredictably; pure noise.
  # recruiting_stars dropped: r=0.907 with recruiting_rating -- stars is a
  # coarse binning of rating. recruiting_rating carries all the information.
  v3_cols_QB <- c(
    "ht", "wt", "forty",
    "pass_epa_slope",
    "recruiting_rating",
    "breakout_age", "seasons_since_breakout",
    "pos_recruiting_density", "pos_volume_concentration",
    "pos_scoring_concentration"
  )
  v3_cols_RB <- c(
    "ht", "wt", "forty", "vertical", "broad_jump",
    "rec_epa_slope", "rush_epa_slope",
    "recruiting_rating",
    "rec_role_share", "rush_to_rec_ratio",
    "breakout_age", "seasons_since_breakout",
    "pos_recruiting_density", "pos_volume_concentration",
    "pos_scoring_concentration", "density_x_role"
  )
  v3_cols_WR <- c(
    "ht", "wt", "forty", "vertical", "broad_jump",
    "rec_epa_slope",
    "recruiting_rating",
    "rec_role_share",
    "breakout_age", "seasons_since_breakout",
    "pos_recruiting_density", "pos_volume_concentration",
    "pos_scoring_concentration", "density_x_role"
  )
  v3_cols_TE <- v3_cols_WR  # same as WR

  v3_additions <- list(QB = v3_cols_QB, RB = v3_cols_RB,
                       WR = v3_cols_WR, TE = v3_cols_TE)

  # --- Step 8: Impute NA values in v3 features ---
  # Training-set medians only; applied to prediction rows using same medians.
  message("\nStep 8: Imputing NA values in v3 features (training medians)...")

  # Force-coerce all expected numeric v3 columns before imputation.
  # Join chains can promote integer or logical columns to list type when
  # many-to-many fan-outs occur silently. This pass corrects all of them.
  v3_numeric_cols_all <- unique(c(
    COMBINE_COLS_ALL,
    "rec_epa_slope", "rush_epa_slope", "pass_epa_slope",
    "sos_x_age",
    "recruiting_rating", "recruiting_stars",
    "rec_role_share", "rush_to_rec_ratio",
    "breakout_age", "seasons_since_breakout",
    "pos_recruiting_density", "pos_volume_concentration",
    "pos_scoring_concentration", "density_x_role"
  ))
  for (.col in v3_numeric_cols_all) {
    if (.col %in% names(all_players)) {
      col_data <- all_players[[.col]]
      if (is.list(col_data)) {
        # List column: flatten before numeric coercion
        all_players[[.col]] <- suppressWarnings(
          as.numeric(vapply(col_data, function(x) {
            if (length(x) == 0L || is.null(x)) NA_real_
            else as.numeric(x[[1L]])
          }, numeric(1L)))
        )
      } else if (!is.numeric(col_data)) {
        all_players[[.col]] <- suppressWarnings(as.numeric(col_data))
      }
    }
  }

  training_all_v3   <- all_players %>%
    dplyr::filter(draft_year <= cutoff_year, !is.na(ppr_per_game_y13))
  prediction_all_v3 <- all_players %>%
    dplyr::filter(draft_year > cutoff_year)

  impute_medians_v3 <- list()

  for (pos in TRANSLATION_POSITIONS) {
    all_v3_cols <- v3_additions[[pos]]
    train_pos   <- training_all_v3 %>%
      dplyr::filter(draft_position == pos)

    impute_medians_v3[[pos]] <- purrr::map_dbl(all_v3_cols, function(col) {
      if (!col %in% names(train_pos)) return(NA_real_)
      col_vals <- suppressWarnings(as.numeric(train_pos[[col]]))
      if (all(is.na(col_vals))) return(NA_real_)
      median(col_vals, na.rm = TRUE)
    }) %>%
      stats::setNames(all_v3_cols)
  }

  # Apply imputation to combined all_players
  for (pos in TRANSLATION_POSITIONS) {
    pos_idx    <- all_players$draft_position == pos
    cols_to_impute <- v3_additions[[pos]]
    for (col in cols_to_impute) {
      if (!col %in% names(all_players)) next
      # After the coercion pass above, columns should be numeric.
      # Guard again here in case any were missed.
      if (is.list(all_players[[col]])) next
      med <- impute_medians_v3[[pos]][[col]]
      na_rows <- pos_idx & is.na(all_players[[col]])
      if (any(na_rows, na.rm = TRUE) && !is.na(med)) {
        all_players[[col]][na_rows] <- med
      }
    }
  }

  # --- Step 9: Rebuild position-stratified training and prediction sets ---
  message("\nStep 9: Rebuilding position-stratified splits...")

  training_v3   <- all_players %>%
    dplyr::filter(draft_year <= cutoff_year, !is.na(ppr_per_game_y13))
  prediction_v3 <- all_players %>%
    dplyr::filter(draft_year > cutoff_year)

  split_by_pos <- function(df) {
    purrr::map(TRANSLATION_POSITIONS, function(pos) {
      df %>% dplyr::filter(draft_position == pos)
    }) %>%
      stats::setNames(TRANSLATION_POSITIONS)
  }

  training_by_pos_v3   <- split_by_pos(training_v3)
  prediction_by_pos_v3 <- split_by_pos(prediction_v3)

  for (pos in TRANSLATION_POSITIONS) {
    message(glue(
      "  {pos}: {format(nrow(training_by_pos_v3[[pos]]), big.mark = ',')} ",
      "training | ",
      "{format(nrow(prediction_by_pos_v3[[pos]]), big.mark = ',')} prediction"
    ))
  }

  # --- Step 10: Build v3 feature column lists ---
  # v1 base feature cols from R/24's build_translation_features() output
  v1_base_cols <- v1_features$base_feature_cols

  base_feature_cols_v3 <- purrr::map(TRANSLATION_POSITIONS, function(pos) {
    v1_cols <- v1_base_cols[[pos]]
    new_v3  <- v3_additions[[pos]]
    # Only include columns that actually exist in the data
    all_candidate <- c(v1_cols, new_v3)
    intersect(all_candidate, names(training_by_pos_v3[[pos]]))
  }) %>%
    stats::setNames(TRANSLATION_POSITIONS)

  # Enriched = base + draft capital (same as v1 enriched, now also includes v3).
  # age_centered is already in v1 base cols; only draft_round and draft_pick
  # are added by the enriched model (matches R/24 v1 enr_cols construction).
  enriched_feature_cols_v3 <- purrr::map(TRANSLATION_POSITIONS, function(pos) {
    base <- base_feature_cols_v3[[pos]]
    enrich_extras <- c("draft_round", "draft_pick")
    c(base, intersect(enrich_extras, names(training_by_pos_v3[[pos]])))
  }) %>%
    stats::setNames(TRANSLATION_POSITIONS)

  message(strrep("-", 60))
  message("build_translation_features_v3() complete.")
  for (pos in TRANSLATION_POSITIONS) {
    message(glue(
      "  {pos}: {length(base_feature_cols_v3[[pos]])} base features, ",
      "{length(enriched_feature_cols_v3[[pos]])} enriched features"
    ))
  }

  # Merge v3 medians into impute_medians so train_translation_model()
  # can subscript by any v3 column name without subscript-out-of-bounds.
  # train_translation_model() does imp_med[base_cols] -- base_cols now
  # contains v3 columns, so imp_med must include them.
  impute_medians_merged <- purrr::map(
    TRANSLATION_POSITIONS,
    function(pos) {
      v1_med  <- v1_features$impute_medians[[pos]]
      v3_med  <- impute_medians_v3[[pos]]
      # c() on named numeric vectors concatenates; duplicate names kept
      # from v3 (v3 values take precedence for any overlapping names).
      merged <- c(v1_med, v3_med)
      merged[!duplicated(names(merged), fromLast = TRUE)]
    }
  ) %>%
    stats::setNames(TRANSLATION_POSITIONS)

  list(
    training            = training_by_pos_v3,
    prediction          = prediction_by_pos_v3,
    base_feature_cols   = base_feature_cols_v3,
    enriched_feature_cols = enriched_feature_cols_v3,
    team_pass_attempts  = v1_features$team_pass_attempts,
    age_centers         = v1_features$age_centers,
    impute_medians      = impute_medians_merged,
    impute_medians_v3   = impute_medians_v3,
    sos_na_rate         = v1_features$sos_na_rate,
    schema_tag          = V3_SCHEMA_TAG
  )
}


# ==============================================================================
# FUNCTION: run_week13_pipeline
# ==============================================================================

#' Run the Full Week 13 v3 Translation Pipeline
#'
#' @description
#' End-to-end execution of the v3 translation model. Loads all data sources,
#' builds the v3 feature matrix, trains the Elastic Net baseline with LOCO CV,
#' evaluates accuracy vs R/24 v1 baseline, and exports the feature matrix as
#' CSV for the Week 13 Python/MLflow notebook.
#'
#' \strong{New data sources loaded (vs R/24):}
#' \itemize{
#'   \item \code{nflreadr::load_combine()} for athleticism features
#'   \item \code{cfbfastR::cfbd_recruiting_player()} for recruiting composite
#'     (cache-first; pulls API if cache absent)
#' }
#'
#' \strong{Outputs saved:}
#' \itemize{
#'   \item \code{s2_week13_crosswalk.rds}
#'   \item \code{s2_week13_feature_matrix.rds}
#'   \item \code{s2_week13_models.rds}
#'   \item \code{s2_week13_performance.rds}
#'   \item \code{s2_week13_predictions.rds}
#'   \item \code{s2_week13_feature_matrix.csv} -- Python notebook input
#'   \item \code{s2_week13_recruiting.rds} -- recruiting cache (cfb cache dir)
#' }
#'
#' @param cfb_panel tibble or NULL. Pre-built R/21 CFB panel. Built from
#'   cache if NULL.
#' @param nfl_panel tibble or NULL. Pre-built R/16 NFL panel. Built from
#'   cache if NULL.
#' @param sos_rds_path chr. Path to R/22 SOS panel RDS.
#'   Default: \code{SOS_PANEL_DEFAULT_PATH}.
#' @param cutoff_year int. Last training draft class. Default: CUTOFF_YEAR.
#' @param min_ppr_outcome numeric. Minimum PPR outcome for training inclusion.
#'   Default: 0.
#' @param output_dir chr. Directory for output RDS/CSV files.
#'   Default: \code{TRANSLATION_CACHE_DIR}.
#' @param recruiting_cache_path chr. Path for recruiting cache RDS.
#'   Default: \code{RECRUITING_CACHE_PATH}.
#' @param verbose logical. Progress output. Default: TRUE.
#'
#' @return Named list: crosswalk, feature_matrix_v3, assumption_checks,
#'   model_list, performance, translation_gaps, csv_path.
#'
#' @examples
#' # Full run from cache (first time: pulls recruiting API, slow)
#' results <- run_week13_pipeline()
#' results$performance
#'
#' # With pre-built panels (faster for re-runs after first pull)
#' results <- run_week13_pipeline(
#'   cfb_panel = my_cfb_panel,
#'   nfl_panel = my_nfl_panel
#' )
#'
#' # Force recruiting re-pull (delete cache first)
#' file.remove(RECRUITING_CACHE_PATH)
#' results <- run_week13_pipeline()
#'
#' @seealso build_translation_features_v3, link_cfb_to_nfl,
#'   train_translation_model
#' @export
run_week13_pipeline <- function(cfb_panel             = NULL,
                                 nfl_panel             = NULL,
                                 sos_rds_path          = SOS_PANEL_DEFAULT_PATH,
                                 cutoff_year           = CUTOFF_YEAR,
                                 min_ppr_outcome       = MIN_PPR_OUTCOME,
                                 output_dir            = TRANSLATION_CACHE_DIR,
                                 recruiting_cache_path = RECRUITING_CACHE_PATH,
                                 verbose               = TRUE) {

  cutoff_year     <- as.integer(cutoff_year)
  min_ppr_outcome <- as.numeric(min_ppr_outcome)

  t_start <- proc.time()

  if (verbose) {
    message(strrep("=", 70))
    message("run_week13_pipeline() -- College-to-NFL Translation Model v3")
    message(glue("Schema tag: {V3_SCHEMA_TAG}"))
    message(glue("Cutoff year: {cutoff_year}"))
    message(glue(
      "Training classes: {min(TRAINING_DRAFT_CLASSES)}-{cutoff_year}"
    ))
    message(glue("Min PPR outcome: {min_ppr_outcome}"))
    message(glue(
      "New features: combine athleticism, production slope, ",
      "age x competition, recruiting, receiving role, breakout age"
    ))
    message(strrep("=", 70))
  }

  # --- Validate output directory ---
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) message(glue("Created output dir: {output_dir}"))
  }

  # --- Load CFB panel ---
  cfb_max_season <- min(cutoff_year, 2025L)

  if (is.null(cfb_panel)) {
    if (verbose) message("\nLoading CFB panel from R/21 cache...")
    cfb_panel <- build_cfb_player_season_panel(
      seasons = CFB_DATA_FLOOR:cfb_max_season,
      verbose = verbose
    )
    if (verbose) message(glue(
      "  CFB panel: {format(nrow(cfb_panel), big.mark = ',')} rows"
    ))
  }

  # --- Load NFL panel ---
  if (is.null(nfl_panel)) {
    if (verbose) message("\nLoading NFL panel from R/16 cache...")
    nfl_panel <- build_player_season_panel(
      seasons = NFL_OUTCOME_SEASONS,
      verbose = verbose
    )
    if (verbose) message(glue(
      "  NFL panel: {format(nrow(nfl_panel), big.mark = ',')} rows"
    ))
  }
  gc(verbose = FALSE)

  # --- Load SOS panel ---
  if (verbose) message(glue("\nLoading SOS panel from: {sos_rds_path}"))
  if (!file.exists(sos_rds_path)) {
    stop(glue(
      "SOS panel not found at: {sos_rds_path}\n",
      "Run the R/22 pipeline first."
    ), call. = FALSE)
  }
  sos_panel <- readRDS(sos_rds_path)

  # --- Load draft picks ---
  if (verbose) message("\nLoading draft picks from nflreadr...")
  draft_data <- nflreadr::load_draft_picks()

  # --- Step 1: ID linkage ---
  if (verbose) message("\n--- Step 1: ID Linkage (R/24 link_cfb_to_nfl) ---")
  crosswalk <- link_cfb_to_nfl(cfb_panel, draft_data)

  # Enrich crosswalk with pfr_player_name from draft_data for combine join.
  # link_cfb_to_nfl() does not include pfr_player_name. Join on gsis_id
  # (unique per player) to avoid the many-to-many fan-out that results
  # from joining on season alone.
  draft_pfr <- draft_data %>%
    dplyr::filter(!is.na(gsis_id)) %>%
    dplyr::select(gsis_id, pfr_player_name) %>%
    dplyr::distinct(gsis_id, .keep_all = TRUE)

  crosswalk <- crosswalk %>%
    dplyr::left_join(
      draft_pfr,
      by = c("nfl_gsis_id" = "gsis_id"),
      relationship = "many-to-one"
    )

  # --- Step 2: Load new data sources ---
  if (verbose) message("\n--- Step 2: Loading v3 Data Sources ---")

  combine_features <- .load_combine_features(crosswalk, verbose = verbose)
  gc(verbose = FALSE)

  recruiting_features <- .load_recruiting_features(
    crosswalk,
    cache_path = recruiting_cache_path,
    verbose    = verbose
  )
  gc(verbose = FALSE)

  # National recruiting board for teammate density (all recruits, not just
  # future-NFL). Recruit classes precede college play by ~3-5 years, so pull a
  # window starting 5 years before the earliest panel season.
  panel_seasons <- sort(unique(cfb_panel$season))
  board_years <- (min(panel_seasons) - 5L):max(panel_seasons)
  if (verbose) message("\n--- Step 2b: Loading National Recruiting Board ---")
  recruiting_board <- .load_recruiting_board(
    years   = board_years,
    verbose = verbose
  )
  gc(verbose = FALSE)

  # --- Step 3: Compute derived features ---
  if (verbose) message("\n--- Step 3: Computing Production Slopes ---")
  production_slopes <- .compute_production_slopes(
    cfb_panel, crosswalk, verbose = verbose
  )

  if (verbose) message("\n--- Step 4: Computing Breakout Ages ---")
  breakout_features <- .compute_breakout_age(
    cfb_panel, crosswalk, cutoff_year = cutoff_year, verbose = verbose
  )

  if (verbose) message("\n--- Step 4b: Computing Teammate Talent Density ---")
  teammate_density <- .compute_teammate_density(
    cfb_panel, crosswalk, recruiting_board, verbose = verbose
  )

  # --- Step 5: Build v3 feature matrix ---
  if (verbose) message("\n--- Step 5: Building v3 Feature Matrix ---")
  feature_matrix_v3 <- build_translation_features_v3(
    cfb_panel           = cfb_panel,
    nfl_panel           = nfl_panel,
    sos_panel           = sos_panel,
    crosswalk           = crosswalk,
    combine_features    = combine_features,
    recruiting_features = recruiting_features,
    production_slopes   = production_slopes,
    breakout_features   = breakout_features,
    teammate_density    = teammate_density,
    cutoff_year         = cutoff_year,
    min_ppr_outcome     = min_ppr_outcome,
    verbose             = verbose
  )
  gc(verbose = FALSE)

  # --- Step 6: Assumption validation ---
  if (verbose) message("\n--- Step 6: Assumption Validation ---")
  assumption_checks <- validate_translation_assumptions(
    crosswalk      = crosswalk,
    feature_matrix = feature_matrix_v3,
    verbose        = verbose
  )

  if (!assumption_checks$valid) {
    warning(
      "One or more critical assumption checks failed. ",
      "Review validate_translation_assumptions() output before ",
      "interpreting model results.",
      call. = FALSE
    )
  }

  # --- Step 7: Train v3 Elastic Net (R-side baseline) ---
  if (verbose) message("\n--- Step 7: Training v3 Elastic Net (LOCO CV) ---")
  model_list <- train_translation_model(
    feature_matrix = feature_matrix_v3,
    cutoff_year    = cutoff_year,
    alpha          = GLMNET_ALPHA
  )

  # --- Step 8: Evaluate and compare vs v1 baseline ---
  if (verbose) message("\n--- Step 8: Evaluating v3 vs v1 Accuracy ---")
  performance <- evaluate_translation_accuracy(model_list, feature_matrix_v3)

  if (verbose) {
    message("\nv3 Performance (LOCO RMSE):")
    perf_summary <- performance %>%
      dplyr::select(draft_position, model_variant, n_players, rmse, r_squared)
    print(as.data.frame(perf_summary))
  }

  # --- Step 9: Translation gaps ---
  if (verbose) message("\n--- Step 9: Translation Gaps ---")
  translation_gaps <- identify_translation_gaps(model_list)

  # --- Step 10: Save RDS outputs ---
  if (verbose) message(glue("\n--- Step 10: Saving Outputs to {output_dir} ---"))

  saveRDS(crosswalk,
    file.path(output_dir, "s2_week13_crosswalk.rds"))
  saveRDS(feature_matrix_v3,
    file.path(output_dir, "s2_week13_feature_matrix.rds"))
  saveRDS(model_list,
    file.path(output_dir, "s2_week13_models.rds"))
  saveRDS(performance,
    file.path(output_dir, "s2_week13_performance.rds"))
  saveRDS(model_list$loco_predictions,
    file.path(output_dir, "s2_week13_predictions.rds"))

  # --- Step 11: Export feature matrix as CSV for Python notebook ---
  if (verbose) message("\n--- Step 11: Exporting CSV for Python/MLflow Notebook ---")

  # Combine training rows from all positions into one flat CSV
  # Include all v3 feature columns plus metadata and outcome
  csv_rows <- dplyr::bind_rows(
    lapply(TRANSLATION_POSITIONS, function(pos) {
      df <- feature_matrix_v3$training[[pos]]
      df$position <- pos
      df
    })
  )

  # Coerce recruiting_stars to numeric for CSV compatibility
  if ("recruiting_stars" %in% names(csv_rows)) {
    csv_rows$recruiting_stars <- as.numeric(csv_rows$recruiting_stars)
  }

  csv_path <- V3_CSV_EXPORT_PATH_FN(output_dir)
  utils::write.csv(csv_rows, csv_path, row.names = FALSE, na = "")

  if (verbose) message(glue(
    "  CSV exported: {format(nrow(csv_rows), big.mark = ',')} rows, ",
    "{ncol(csv_rows)} columns\n",
    "  Path: {csv_path}"
  ))

  t_elapsed <- round((proc.time() - t_start)[["elapsed"]])

  if (verbose) {
    message(strrep("=", 70))
    message(glue(
      "run_week13_pipeline() complete in {t_elapsed} seconds."
    ))
    message(glue(
      "Training classes: {min(TRAINING_DRAFT_CLASSES)}-{cutoff_year} | ",
      "Schema: {V3_SCHEMA_TAG}"
    ))
    message(glue("CSV for Python notebook: {csv_path}"))
    message(strrep("=", 70))
  }

  list(
    crosswalk         = crosswalk,
    feature_matrix_v3 = feature_matrix_v3,
    assumption_checks = assumption_checks,
    model_list        = model_list,
    performance       = performance,
    translation_gaps  = translation_gaps,
    csv_path          = csv_path
  )
}
