# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 10
# College-to-NFL Translation Model
# File: R/24_translation_model.R
#
# Purpose: Build a model that translates college production metrics to NFL
#          production expectations using cfbfastR panel data (Phase 2) linked
#          to NFL outcomes via nflreadr draft pick data.
#
# Design decisions (confirmed in brainstorm session):
#   - ID linkage     : nflreadr::load_draft_picks() primary bridge (name +
#                      college team), normalized name fuzzy match as fallback.
#                      CFB-to-NFL leg deferred from R/22 is built here.
#   - Outcome        : Average PPR per game across NFL Years 1-3 (regression).
#                      Hit flag derived post-fitting: top-N at position at
#                      least once in Years 1-3 (QB: 12, RB: 36, WR: 36,
#                      TE: 12). Not a separate classification model.
#   - Model class    : Elastic Net (glmnet). No XGBoost -- small-N dataset
#                      (draft classes) favors regularization over tree depth.
#   - Model variants : base (college production only) + enriched (adds draft
#                      capital). 8 total model objects. Accuracy delta
#                      quantifies value of draft capital beyond production.
#   - CV strategy    : Leave-one-draft-class-out (LOCO). True prospective
#                      holdout. 8 folds (draft classes 2015-2022).
#   - Positions      : QB, RB, WR, TE -- four stratified models per variant.
#                      WR/TE split uses draft position from load_draft_picks().
#   - Multi-year     : Final college season double-weighted, prior seasons
#                      single-weighted.
#   - Context norm   : Receiving yards / team pass attempts per season for
#                      WR, TE, and RB receiving role. Computed at query time
#                      from R/21 panel -- not stored in R/21 output.
#   - Age            : Position-centered at draft entry. Centering computed on
#                      training set; prediction set uses training-set mean.
#   - SOS            : sos_opp_def_epa_per_play and
#                      sos_opp_def_success_rate_allowed as standalone features.
#                      NA rate reported. Median imputation applied before
#                      glmnet matrix construction.
#
# Future-proofing:
#   Set CUTOFF_YEAR to the most recent draft class with 3 complete NFL seasons.
#   As of April 2026: 2022L (NFL seasons 2022, 2023, 2024 complete).
#   Next year: change to 2023L after the 2026 NFL regular season completes.
#   All training/prediction splits derive from this single constant.
#
# Navigation:
#   Line  ~90  : Libraries
#   Line ~125  : Source guards
#   Line ~195  : Constants
#   Line ~330  : NSE declarations
#   Line ~360  : Internal helpers
#                  .normalize_player_name()
#                  .compute_team_pass_attempts()
#                  .apply_multiseason_weights()
#                  .impute_features()
#                  .fit_elastic_net_loco()
#                  .compute_nfl_outcomes()
#   Line ~700  : link_cfb_to_nfl()
#   Line ~960  : build_translation_features()
#   Line ~1310 : train_translation_model()
#   Line ~1570 : evaluate_translation_accuracy()
#   Line ~1700 : identify_translation_gaps()
#   Line ~1820 : validate_translation_assumptions()
#   Line ~2000 : run_week10_pipeline()
#
# Source dependencies:
#   R/20_multi_season_cfb_pbp.R    -- load_normalized_cfb_season()
#   R/21_cfb_player_season_panel.R -- build_cfb_player_season_panel()
#   R/16_player_season_panel.R     -- build_player_season_panel()
#   R/22 SOS panel loaded from:
#     data/season2_cfb_cache/s2_week8_cfb_sos_panel.rds
#
# Outputs (written by run_week10_pipeline()):
#   data/season2_cache/s2_week10_crosswalk.rds
#   data/season2_cache/s2_week10_feature_matrix.rds
#   data/season2_cache/s2_week10_models.rds
#   data/season2_cache/s2_week10_performance.rds
#   data/season2_cache/s2_week10_predictions.rds
#
# Season 2 output prefix : s2_week10_
# Schema tag             : s2_w10_v1
# Author                 : Christian LeBlanc
# Created                : 2026-04
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

if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop(
    "Package 'glmnet' is required. Install with: install.packages('glmnet')",
    call. = FALSE
  )
}


# ==============================================================================
# SOURCE GUARDS
# ==============================================================================

# R/20: load_normalized_cfb_season() -- required by R/21 at source time
if (!exists("load_normalized_cfb_season", mode = "function")) {
  week20_path <- here::here("R", "20_multi_season_cfb_pbp.R")
  if (!file.exists(week20_path)) {
    stop(glue(
      "R/20_multi_season_cfb_pbp.R not found at: {week20_path}\n",
      "Required for load_normalized_cfb_season()."
    ), call. = FALSE)
  }
  source(week20_path)
}

# R/21: build_cfb_player_season_panel() -- used in run_week10_pipeline()
if (!exists("build_cfb_player_season_panel", mode = "function")) {
  week21_path <- here::here("R", "21_cfb_player_season_panel.R")
  if (!file.exists(week21_path)) {
    stop(glue(
      "R/21_cfb_player_season_panel.R not found at: {week21_path}\n",
      "Required for build_cfb_player_season_panel()."
    ), call. = FALSE)
  }
  source(week21_path)
}

# R/16: build_player_season_panel() -- used in run_week10_pipeline()
if (!exists("build_player_season_panel", mode = "function")) {
  week16_path <- here::here("R", "16_player_season_panel.R")
  if (!file.exists(week16_path)) {
    stop(glue(
      "R/16_player_season_panel.R not found at: {week16_path}\n",
      "Required for build_player_season_panel()."
    ), call. = FALSE)
  }
  source(week16_path)
}


# ==============================================================================
# CONSTANTS
# ==============================================================================

# --- Future-proofing anchor ---
# Change this to the most recent draft class with 3 complete NFL seasons.
# Draft class CUTOFF_YEAR played NFL seasons CUTOFF_YEAR, CUTOFF_YEAR+1,
# CUTOFF_YEAR+2. As of April 2026, the 2025 NFL season (Sept 2025 - Feb 2026)
# is complete, so CUTOFF_YEAR+2 = 2025 -> CUTOFF_YEAR = 2023.
# Next update: set to 2024L after the 2026 NFL regular season ends.
CUTOFF_YEAR <- 2023L

# CFB data floor -- matches R/21 and R/20 cache floor
CFB_DATA_FLOOR <- 2014L

# Effective training draft classes: first class with CFB data is 2015
# (their last college season = 2014 = CFB data floor). Classes through
# CUTOFF_YEAR form the training set. 2015:CUTOFF_YEAR = 9 classes.
TRAINING_DRAFT_CLASSES <- 2015L:CUTOFF_YEAR

# NFL seasons to load for outcome computation. Must cover Year 3 of the
# earliest training class (draft 2015 -> NFL seasons 2015, 2016, 2017)
# through Year 3 of CUTOFF_YEAR class (2023, 2024, 2025).
NFL_OUTCOME_SEASONS    <- 2015L:(CUTOFF_YEAR + 2L)

# --- Hit thresholds ---
# Top-N at position at least once in NFL Years 1-3.
# Reflects standard 12-team fantasy league starting lineup scarcity.
HIT_THRESHOLD_QB <- 12L
HIT_THRESHOLD_RB <- 36L
HIT_THRESHOLD_WR <- 36L
HIT_THRESHOLD_TE <- 12L

# Named lookup for programmatic access
HIT_THRESHOLDS <- list(
  QB = HIT_THRESHOLD_QB,
  RB = HIT_THRESHOLD_RB,
  WR = HIT_THRESHOLD_WR,
  TE = HIT_THRESHOLD_TE
)

# --- Outcome thresholds ---
# Minimum games played in an NFL season for that season to count toward the
# 3-year average. Players below this threshold in a given season are excluded
# from the average for that season (injury / depth chart exclusion).
MIN_NFL_GAMES_PER_SEASON <- 4L

# Minimum seasons with >= MIN_NFL_GAMES_PER_SEASON to compute average.
# Players with 0 qualifying seasons are treated as non-contributors (PPR = 0).
MIN_QUALIFYING_NFL_SEASONS <- 1L

# Minimum college games in any season for that season to enter the weighted
# average. Prevents injury-shortened semesters from distorting features.
MIN_CFB_GAMES_PER_SEASON <- 4L

# --- Multi-year weighting ---
FINAL_SEASON_WEIGHT <- 2L   # Weight applied to last college season
PRIOR_SEASON_WEIGHT <- 1L   # Weight applied to all prior college seasons

# --- Model ---
GLMNET_ALPHA      <- 0.5    # Elastic Net blend: 0 = Ridge, 1 = Lasso
GLMNET_NFOLDS_MIN <- 5L     # Minimum inner CV folds for lambda selection
GLMNET_SEED       <- 42L    # Reproducibility seed for cv.glmnet

# --- PPR scoring constants for NFL outcome computation from R/16 panel ---
# Standard PPR scoring applied to R/16 season-level raw stats.
# fumbles_lost and two_pt_conversions are not present in R/16; omitted.
PPR_PASS_YD   <-  0.04
PPR_PASS_TD   <-  4.00
PPR_INT       <- -2.00
PPR_RUSH_YD   <-  0.10
PPR_RUSH_TD   <-  6.00
PPR_RECEPTION <-  1.00
PPR_REC_YD    <-  0.10
PPR_REC_TD    <-  6.00

# --- Season 3 Wave A2: scoring-agnostic stat-line outcome -------------------
# The legacy outcome (ppr_per_game_y13) is a POINTS quantity computed under the
# fixed constants above. Any consumer that receives it is locked to that one
# scoring, which is the root of the scoring-blindness defect: a model fit on a
# points target cannot answer "what is this player worth under a different
# ruleset" without a refit.
#
# A2 replaces the single points target with the eight raw per-game stat
# components. Points under ANY linear scoring are then recovered downstream by
# score_stat_line(). This is exact, not an approximation: the outcome averages
# per-game rates across qualifying seasons, and scoring is linear in the
# components, so
#     mean_s( sum_c w_c * comp_c(s) )  ==  sum_c w_c * mean_s( comp_c(s) )
# The scored average component line equals the averaged scored line for every
# linear scoring. Nothing is lost by decomposing.
#
# KNOWN GAPS (inherited from the legacy outcome, NOT introduced here):
#   - fumbles_lost and two_pt_conversions are absent from the R/16 panel.
#   - Threshold bonuses (100-yard game, 300-yard passing game) are NON-LINEAR
#     in per-game stats and cannot be recovered from a season-average line.
#     They were never in the legacy outcome either. Do not add a bonus term to
#     score_stat_line() expecting it to work on y13 averages; it will not.
#
# NAMING: the y13_ prefix is deliberate. Bare names like pass_yd_pg / rec_td_pg
# are ALREADY IN USE as CFB feature columns in the same feature matrix. Reusing
# them for NFL outcomes would silently collide on join.
STAT_COMPONENTS <- c(
  "y13_pass_yd", "y13_pass_td", "y13_int",
  "y13_rush_yd", "y13_rush_td",
  "y13_rec",     "y13_rec_yd",  "y13_rec_td"
)

# Panel source column for each component. Order matches STAT_COMPONENTS.
STAT_COMPONENT_SOURCE <- c(
  y13_pass_yd = "passing_yards",
  y13_pass_td = "pass_tds",
  y13_int     = "interceptions_thrown",
  y13_rush_yd = "rushing_yards",
  y13_rush_td = "rush_tds",
  y13_rec     = "receptions",
  y13_rec_yd  = "receiving_yards",
  y13_rec_td  = "rec_tds"
)

# Scoring-list key that weights each component. Keys match the R/17 scoring
# vocabulary (DK_BEST_BALL_SCORING, Sleeper resolver output) so any scoring list
# already in the pipeline can be passed straight to score_stat_line().
STAT_COMPONENT_SCORING_KEY <- c(
  y13_pass_yd = "pass_yd",
  y13_pass_td = "pass_td",
  y13_int     = "pass_int",
  y13_rush_yd = "rush_yd",
  y13_rush_td = "rush_td",
  y13_rec     = "ppr",
  y13_rec_yd  = "rec_yd",
  y13_rec_td  = "rec_td"
)

# Reference scoring, used ONLY for (a) the legacy ppr_per_game_y13 column kept
# for backward compatibility and the A1 benchmark, and (b) is_hit / peak ranking,
# which need one fixed yardstick to be comparable across players and seasons.
# These values reproduce the legacy constants above exactly, so the legacy
# column is unchanged by this rewrite.
REFERENCE_SCORING <- list(
  pass_yd = PPR_PASS_YD, pass_td = PPR_PASS_TD, pass_int = PPR_INT,
  rush_yd = PPR_RUSH_YD, rush_td = PPR_RUSH_TD,
  ppr     = PPR_RECEPTION, rec_yd = PPR_REC_YD, rec_td = PPR_REC_TD
)

# Minimum fraction of training rows that must carry a NON-ZERO value before a
# stat component is modelled rather than treated as a constant. Components
# below this line (RB passing yards, WR passing TDs, QB receptions) are
# structurally absent for the position: variance is technically non-zero
# because one or two players registered a value, but every LOCO fold and the
# final glmnet standardization collapse on them. The constant is the honest
# prediction, and it costs nothing, because the true value is ~0 for everyone.
MIN_COMPONENT_NONZERO_FRAC <- 0.05

# --- Linkage ---
# Maximum normalized edit distance for fuzzy name match (0 = exact, 1 = total)
FUZZY_NAME_THRESHOLD <- 0.20

# --- Cache paths ---
TRANSLATION_CACHE_DIR    <- here::here("data", "season2_cache")
SOS_PANEL_DEFAULT_PATH   <- here::here(
  "data", "season2_cfb_cache", "s2_week8_cfb_sos_panel.rds"
)
CFB_PANEL_CACHE_DIR      <- here::here("data", "season2_cfb_cache")
NFL_PANEL_CACHE_DIR      <- here::here("data", "season2_cache")

# --- Schema tag ---
TRANSLATION_SCHEMA_TAG <- "s2_w10_v1"

# --- nflreadr::load_draft_picks() expected columns ---
# Validated at runtime inside link_cfb_to_nfl(). Listed here for
# documentation. Column names verified against nflreadr as of 2026.
DRAFT_COLS_REQUIRED <- c(
  "season",           # draft year (int)
  "round",            # draft round (int)
  "pick",             # overall pick number (int)
  "team",             # NFL team abbreviation (chr)
  "gsis_id",          # NFL player ID, may be NA for historical picks (chr)
  "pfr_player_name",  # player name from Pro Football Reference (chr)
  "college",          # college attended (chr)
  "position",         # draft position (chr: QB, RB, WR, TE, ...)
  "age"               # age at draft (dbl)
)

# Positions covered by this model
TRANSLATION_POSITIONS <- c("QB", "RB", "WR", "TE")

# --- R/21 panel required columns (verified from R/21 roxygen) ---
CFB_PANEL_COLS_REQUIRED <- c(
  "player_name", "season", "primary_team", "position_group",
  "games_played", "pass_attempts", "passing_yards", "pass_tds",
  "interceptions", "completion_pct", "pass_epa_per_attempt",
  "rush_attempts", "rushing_yards", "rush_tds", "rush_epa_per_attempt",
  "targets", "receptions", "receiving_yards", "rec_tds",
  "rec_epa_per_target", "catch_rate", "success_rate", "low_volume"
)

# --- R/22 SOS panel required columns (verified from R/22 roxygen) ---
SOS_COLS_REQUIRED <- c(
  "player_name", "season",
  "sos_opp_def_epa_per_play",
  "sos_opp_def_success_rate_allowed",
  "sos_n_opponents",
  "sos_computed"
)

# --- R/16 NFL panel required columns (verified from R/16 roxygen) ---
NFL_PANEL_COLS_REQUIRED <- c(
  "player_id", "player_name", "season", "position", "position_group",
  "games_played", "passing_yards", "pass_tds", "interceptions_thrown",
  "rushing_yards", "rush_tds", "receptions", "receiving_yards", "rec_tds"
)


# ==============================================================================
# NSE DECLARATIONS
# ==============================================================================

utils::globalVariables(c(
  # shared
  "player_name", "season", "primary_team", "position_group",
  "games_played", "low_volume", "has_name_collision",
  # R/21 CFB columns
  "pass_attempts", "passing_yards", "pass_tds", "interceptions",
  "completion_pct", "pass_epa_per_attempt",
  "rush_attempts", "rushing_yards", "rush_tds", "rush_epa_per_attempt",
  "targets", "receptions", "receiving_yards", "rec_tds",
  "rec_epa_per_target", "catch_rate", "success_rate",
  # SOS columns
  "sos_opp_def_epa_per_play", "sos_opp_def_success_rate_allowed",
  "sos_n_opponents", "sos_computed",
  # R/16 NFL columns
  "player_id", "position", "pass_tds", "interceptions_thrown",
  "rush_tds", "rec_tds", "rec_epa_per_target",
  # derived / feature columns
  "draft_year", "draft_round", "draft_pick", "draft_age", "draft_position",
  "nfl_gsis_id", "cfb_player_name", "cfb_primary_team",
  "match_method", "match_confidence", "n_cfb_seasons",
  "team_pass_attempts", "weight", "weighted_sum",
  "pass_att_pg", "pass_yd_pg", "pass_td_pg", "int_pg",
  "rush_att_pg", "rush_yd_pg", "rush_td_pg",
  "rec_yd_per_team_pass_att", "rec_yd_pg", "tgt_pg", "rec_td_pg",
  "age_centered",
  "ppr_season", "ppr_per_game", "ppr_per_game_y13", "is_hit",
  # Season 3 Wave A2 stat-line outcome components
  "y13_pass_yd", "y13_pass_td", "y13_int",
  "y13_rush_yd", "y13_rush_td",
  "y13_rec", "y13_rec_yd", "y13_rec_td",
  "component", "pred_component", "scoring_label",
  "year_in_nfl", "qualifying_seasons",
  "peak_ppr_per_game", "peak_career_year",
  "season_rank", "ever_top_n",
  # glmnet / model output
  "feature", "base_coef", "enriched_coef",
  "base_nonzero", "enriched_nonzero",
  "position_label", "model_variant",
  "rmse", "mae", "r_squared", "n_players", "accuracy_delta",
  # misc
  "cfb_final_season", "norm_name", "norm_college",
  "draft_college", "panel_version"
))


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

# ------------------------------------------------------------------------------
# .normalize_player_name
#
# Strips common suffixes (Jr., Sr., II, III, IV), converts to lowercase, and
# removes non-alphabetic characters. Used for name matching across cfbfastR
# and nflreadr where naming conventions differ.
# Not exported.
# ------------------------------------------------------------------------------
.normalize_player_name <- function(name) {
  if (is.null(name)) return(character(0L))
  name <- tolower(as.character(name))
  # Strip generational suffixes
  name <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b\\.?", "", name)
  # Remove non-alphabetic characters
  name <- gsub("[^a-z]", "", name)
  trimws(name)
}


# ------------------------------------------------------------------------------
# .compute_team_pass_attempts
#
# Computes total team pass attempts per college team per season by summing
# pass_attempts across all players on each primary_team in the R/21 panel.
# Used as denominator for context-normalized receiving yards.
#
# Called once inside build_translation_features() and the result is reused
# across all receiver rows via a join.
#
# Args:
#   cfb_panel: tibble from build_cfb_player_season_panel()
#
# Returns: tibble with columns primary_team, season, team_pass_attempts (int)
# Not exported.
# ------------------------------------------------------------------------------
.compute_team_pass_attempts <- function(cfb_panel) {
  cfb_panel %>%
    dplyr::filter(!low_volume) %>%
    dplyr::group_by(primary_team, season) %>%
    dplyr::summarise(
      team_pass_attempts = sum(pass_attempts, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(team_pass_attempts > 0L)
}


# ------------------------------------------------------------------------------
# .apply_multiseason_weights
#
# Applies multi-year weighting to a player's college season rows before
# averaging features. Final season receives FINAL_SEASON_WEIGHT; all prior
# seasons receive PRIOR_SEASON_WEIGHT.
#
# Args:
#   player_seasons: tibble of one player's college season rows, sorted by
#                   season ascending. Must contain a `season` column.
#   feature_cols:   character vector of column names to average.
#   final_season:   optional integer. The player's true final CFB season from
#                   the crosswalk (cfb_final_season). When supplied, the
#                   FINAL_SEASON_WEIGHT is anchored to it; otherwise it falls
#                   back to max(season) of the rows passed in. The fallback is
#                   wrong when the true final season was filtered out (e.g. an
#                   injury-shortened low-volume year), which would shift the 2x
#                   weight onto an earlier season.
#
# Returns: one-row tibble with weighted-average values for each feature_col.
# Not exported.
# ------------------------------------------------------------------------------
.apply_multiseason_weights <- function(player_seasons, feature_cols,
                                       final_season = NULL) {
  # Assign weights: final season = FINAL_SEASON_WEIGHT, prior = PRIOR_SEASON_WEIGHT
  final_s <- if (!is.null(final_season) && !is.na(final_season)) {
    final_season
  } else {
    max(player_seasons$season)
  }
  weights  <- dplyr::if_else(
    player_seasons$season == final_s,
    as.numeric(FINAL_SEASON_WEIGHT),
    as.numeric(PRIOR_SEASON_WEIGHT)
  )

  total_weight <- sum(weights)

  # Weighted average for each feature column
  result <- purrr::map_dfc(feature_cols, function(col) {
    vals <- player_seasons[[col]]
    # Weighted mean, ignoring NA seasons for this column
    valid <- !is.na(vals)
    if (!any(valid)) {
      tibble::tibble(!!col := NA_real_)
    } else {
      wt_sum <- sum(weights[valid] * vals[valid])
      wt_tot <- sum(weights[valid])
      tibble::tibble(!!col := wt_sum / wt_tot)
    }
  })

  result
}


# ------------------------------------------------------------------------------
# .impute_features
#
# Applies median imputation to NA values in a numeric feature matrix tibble.
# glmnet does not accept NA values; this must be called before matrix
# conversion. Imputation medians are computed on the training set and must be
# applied to prediction rows using training-set medians (passed in).
#
# Args:
#   df:              tibble with numeric feature columns
#   feature_cols:    character vector of column names to impute
#   impute_medians:  named numeric vector of medians (from training set).
#                    If NULL, medians are computed from df (training mode).
#
# Returns: list(imputed_df, impute_medians)
# Not exported.
# ------------------------------------------------------------------------------
.impute_features <- function(df, feature_cols, impute_medians = NULL) {
  training_mode <- is.null(impute_medians)

  if (training_mode) {
    impute_medians <- purrr::map_dbl(feature_cols, function(col) {
      median(df[[col]], na.rm = TRUE)
    })
    names(impute_medians) <- feature_cols
  }

  for (col in feature_cols) {
    med <- impute_medians[[col]]
    na_rows <- is.na(df[[col]])
    if (any(na_rows)) {
      df[[col]][na_rows] <- if (is.na(med)) 0 else med
    }
  }

  list(imputed_df = df, impute_medians = impute_medians)
}


# ------------------------------------------------------------------------------
# .fit_elastic_net_loco
#
# Implements leave-one-draft-class-out (LOCO) cross-validation for an
# Elastic Net model. For each held-out draft class:
#   1. Run cv.glmnet on all other classes to select lambda.min.
#   2. Predict the held-out class at lambda.min.
# Returns out-of-sample predictions for all training rows.
#
# Args:
#   X:            numeric matrix (n x p), no NAs allowed
#   y:            numeric vector of outcomes (length n)
#   draft_years:  integer vector of draft class for each row (length n)
#   alpha:        Elastic Net mixing parameter (default GLMNET_ALPHA)
#
# Returns: numeric vector of LOCO predictions (same length as y)
# Not exported.
# ------------------------------------------------------------------------------
.fit_elastic_net_loco <- function(X, y, draft_years,
                                   alpha = GLMNET_ALPHA) {
  unique_years <- sort(unique(draft_years))
  n_folds      <- length(unique_years)

  if (n_folds < 2L) {
    stop(glue(
      ".fit_elastic_net_loco(): need >= 2 distinct draft classes. ",
      "Found: {n_folds}."
    ), call. = FALSE)
  }

  predictions <- numeric(length(y))

  for (hold_year in unique_years) {
    train_idx <- draft_years != hold_year
    test_idx  <- draft_years == hold_year

    n_train <- sum(train_idx)
    n_test  <- sum(test_idx)

    if (n_train < 10L) {
      warning(glue(
        "LOCO fold {hold_year}: only {n_train} training rows. ",
        "Predictions for this fold may be unreliable."
      ), call. = FALSE)
    }

    X_train <- X[train_idx, , drop = FALSE]
    y_train <- y[train_idx]
    X_test  <- X[test_idx,  , drop = FALSE]

    # Inner CV on training data to select optimal lambda
    inner_folds <- max(GLMNET_NFOLDS_MIN, min(10L, n_train))

    set.seed(GLMNET_SEED)
    cv_fit <- tryCatch(
      glmnet::cv.glmnet(
        x      = X_train,
        y      = y_train,
        alpha  = alpha,
        nfolds = inner_folds
      ),
      error = function(e) {
        warning(glue(
          "LOCO fold {hold_year}: cv.glmnet failed -- {conditionMessage(e)}. ",
          "Predictions set to training mean."
        ), call. = FALSE)
        NULL
      }
    )

    if (is.null(cv_fit)) {
      predictions[test_idx] <- mean(y_train, na.rm = TRUE)
    } else {
      predictions[test_idx] <- as.vector(
        predict(cv_fit, newx = X_test, s = "lambda.min")
      )
    }
  }

  predictions
}


# ------------------------------------------------------------------------------
# .compute_nfl_outcomes
#
# Computes PPR per game averaged across NFL Years 1-3 for each drafted player,
# and derives the hit flag (ever top-N at position in Years 1-3).
#
# Uses the full R/16 NFL panel to rank ALL players each season (not just
# players in the training set) for hit flag accuracy.
#
# PPR formula applied to R/16 season-level columns:
#   passing_yards * PPR_PASS_YD + pass_tds * PPR_PASS_TD +
#   interceptions_thrown * PPR_INT + rushing_yards * PPR_RUSH_YD +
#   rush_tds * PPR_RUSH_TD + receptions * PPR_RECEPTION +
#   receiving_yards * PPR_REC_YD + rec_tds * PPR_REC_TD
# Divided by games_played.
#
# Note: fumbles_lost and two_pt_conversions are absent from R/16 panel.
#       These represent small contributions at the season level.
#
# Args:
#   crosswalk:     output of link_cfb_to_nfl() -- needs nfl_gsis_id, draft_year
#   nfl_panel:     R/16 panel tibble filtered to NFL_OUTCOME_SEASONS
#   hit_thresholds: named list (QB, RB, WR, TE) of top-N thresholds
#
# Returns: tibble with nfl_gsis_id, ppr_per_game_y13, qualifying_seasons,
#          is_hit, draft_position (for position routing)
# Not exported.
# ------------------------------------------------------------------------------
#' Score a stat line under an arbitrary scoring list
#'
#' @description
#' Single source of truth for turning stat components into fantasy points.
#' Accepts any data frame carrying the STAT_COMPONENTS columns (per-game rates
#' or season totals; the arithmetic is the same) and any scoring list using the
#' R/17 scoring vocabulary, and returns a numeric vector of points.
#'
#' Missing scoring keys default to 0, so a partial scoring list scores only the
#' components it names. Missing component columns are an error, not a silent
#' zero: a stat line that cannot be scored should fail loudly rather than
#' quietly return an understated total.
#'
#' Only LINEAR per-component terms are supported. Threshold bonuses cannot be
#' recovered from season-average rates. See the STAT_COMPONENTS comment block.
#'
#' @param stat_df Data frame containing the STAT_COMPONENTS columns.
#' @param scoring Named list of scoring values (R/17 vocabulary). Defaults to
#'   REFERENCE_SCORING, which reproduces the legacy PPR constants exactly.
#'
#' @return Numeric vector of points, length nrow(stat_df).
#'
#' @examples
#' score_stat_line(outcomes, DK_BEST_BALL_SCORING)
#'
#' @export
score_stat_line <- function(stat_df, scoring = REFERENCE_SCORING) {

  missing_cols <- setdiff(STAT_COMPONENTS, names(stat_df))
  if (length(missing_cols) > 0L) {
    stop(glue(
      "score_stat_line(): stat_df is missing required component column(s): ",
      "{paste(missing_cols, collapse = ', ')}. Refusing to score a partial ",
      "stat line."
    ), call. = FALSE)
  }
  if (!is.list(scoring)) {
    stop("score_stat_line(): `scoring` must be a named list.", call. = FALSE)
  }

  pts <- rep(0, nrow(stat_df))
  for (comp in STAT_COMPONENTS) {
    key <- STAT_COMPONENT_SCORING_KEY[[comp]]
    w   <- scoring[[key]]
    if (is.null(w) || is.na(w)) w <- 0
    pts <- pts + dplyr::coalesce(as.numeric(stat_df[[comp]]), 0) * as.numeric(w)
  }
  pts
}


.compute_nfl_outcomes <- function(crosswalk, nfl_panel, hit_thresholds) {

  # Per-game rate for every stat component, for every qualifying player-season.
  # ppr_per_game is retained under REFERENCE_SCORING for backward compatibility
  # and for is_hit / peak ranking, which need one fixed yardstick.
  nfl_ppr <- nfl_panel %>%
    dplyr::filter(
      !is.na(player_id),
      games_played >= MIN_NFL_GAMES_PER_SEASON,
      position %in% c("QB", "RB", "WR", "TE")
    ) %>%
    dplyr::mutate(
      y13_pass_yd = dplyr::coalesce(passing_yards,        0L) / games_played,
      y13_pass_td = dplyr::coalesce(pass_tds,             0L) / games_played,
      y13_int     = dplyr::coalesce(interceptions_thrown, 0L) / games_played,
      y13_rush_yd = dplyr::coalesce(rushing_yards,        0L) / games_played,
      y13_rush_td = dplyr::coalesce(rush_tds,             0L) / games_played,
      y13_rec     = dplyr::coalesce(receptions,           0L) / games_played,
      y13_rec_yd  = dplyr::coalesce(receiving_yards,      0L) / games_played,
      y13_rec_td  = dplyr::coalesce(rec_tds,              0L) / games_played
    )

  # Reference-scoring points, computed through the same single scoring path as
  # every other consumer. Assigned outside the pipe so this does not depend on
  # dplyr::pick() (dplyr >= 1.1.0).
  nfl_ppr$ppr_per_game <- score_stat_line(nfl_ppr, REFERENCE_SCORING)

  nfl_ppr <- nfl_ppr %>%
    dplyr::select(
      player_id, season, position, ppr_per_game, games_played,
      dplyr::all_of(STAT_COMPONENTS)
    )

  # Compute hit flag: rank ALL players at each position each season
  nfl_hits <- nfl_ppr %>%
    dplyr::group_by(season, position) %>%
    dplyr::mutate(
      season_rank = rank(-ppr_per_game, ties.method = "min")
    ) %>%
    dplyr::ungroup()

  # For each drafted player, find Years 1-3 and compute outcomes
  crosswalk_outcomes <- crosswalk %>%
    dplyr::filter(!is.na(nfl_gsis_id)) %>%
    dplyr::select(nfl_gsis_id, draft_year, draft_position) %>%
    dplyr::distinct()

  results <- purrr::map_dfr(seq_len(nrow(crosswalk_outcomes)), function(i) {
    gsis_id      <- crosswalk_outcomes$nfl_gsis_id[i]
    d_year       <- crosswalk_outcomes$draft_year[i]
    d_pos        <- crosswalk_outcomes$draft_position[i]
    nfl_seasons  <- d_year:(d_year + 2L)
    threshold    <- hit_thresholds[[d_pos]]
    if (is.null(threshold)) threshold <- 36L

    # Best-observed career season across the player's ENTIRE observed career
    # (draft year onward), using the same min-games gate that nfl_ppr already
    # applied. Computed independently of the Years 1-3 window so a late bloomer
    # who never qualified in Years 1-3 still records a real peak. peak_career_year
    # is years into the career, rookie season = 1. For recent classes this is a
    # best-OBSERVED peak, since only a few seasons are visible yet.
    career_seasons <- nfl_ppr %>%
      dplyr::filter(player_id == gsis_id, season >= d_year)
    if (nrow(career_seasons) == 0L) {
      peak_ppg <- NA_real_
      peak_yr  <- NA_integer_
    } else {
      bi       <- which.max(career_seasons$ppr_per_game)
      peak_ppg <- career_seasons$ppr_per_game[bi]
      peak_yr  <- as.integer(career_seasons$season[bi] - d_year + 1L)
    }

    player_seasons <- nfl_hits %>%
      dplyr::filter(
        player_id == gsis_id,
        season %in% nfl_seasons,
        games_played >= MIN_NFL_GAMES_PER_SEASON
      )

    if (nrow(player_seasons) == 0L) {
      zero_components <- tibble::as_tibble(
        stats::setNames(as.list(rep(0, length(STAT_COMPONENTS))),
                        STAT_COMPONENTS)
      )
      return(dplyr::bind_cols(
        tibble::tibble(
          nfl_gsis_id        = gsis_id,
          ppr_per_game_y13   = 0,
          qualifying_seasons = 0L,
          is_hit             = FALSE,
          peak_ppr_per_game  = peak_ppg,
          peak_career_year   = peak_yr,
          draft_position     = d_pos
        ),
        zero_components
      ))
    }

    # Legacy points outcome: unchanged computation, preserved exactly.
    avg_ppr <- mean(player_seasons$ppr_per_game, na.rm = TRUE)
    ever_top_n <- any(player_seasons$season_rank <= threshold, na.rm = TRUE)

    # A2 stat-line outcome: the SAME aggregation applied per component. By
    # linearity of scoring, score_stat_line() on this averaged line reproduces
    # avg_ppr under REFERENCE_SCORING, and yields the correct points under any
    # other linear scoring without a refit.
    avg_components <- tibble::as_tibble(
      stats::setNames(
        lapply(STAT_COMPONENTS, function(cc) {
          mean(player_seasons[[cc]], na.rm = TRUE)
        }),
        STAT_COMPONENTS
      )
    )

    dplyr::bind_cols(
      tibble::tibble(
        nfl_gsis_id        = gsis_id,
        ppr_per_game_y13   = avg_ppr,
        qualifying_seasons = nrow(player_seasons),
        is_hit             = ever_top_n,
        peak_ppr_per_game  = peak_ppg,
        peak_career_year   = peak_yr,
        draft_position     = d_pos
      ),
      avg_components
    )
  })

  results
}


# ==============================================================================
# FUNCTION: link_cfb_to_nfl
# ==============================================================================

#' Link CFB Panel to NFL Draft Data
#'
#' @description
#' Builds a crosswalk connecting college player records in the R/21 CFB panel
#' to NFL player records via \code{nflreadr::load_draft_picks()}. This is the
#' CFB-to-NFL linkage leg deferred from R/22 (Week 8).
#'
#' \strong{Primary match:} Normalized player name + college team name match
#' between the R/21 panel (player_name + primary_team in the player's final
#' college season = draft_year - 1) and the draft data (pfr_player_name +
#' college).
#'
#' \strong{Fallback match:} Normalized name-only fuzzy match using base R
#' \code{adist()} (restricted Damerau-Levenshtein). Threshold controlled by
#' FUZZY_NAME_THRESHOLD. Fuzzy matches are flagged and carry lower confidence.
#'
#' \strong{Coverage:} Only covers players who appear in both data sources.
#' Undrafted free agents appear in the draft data only if nflreadr includes
#' them, which is inconsistent. Players with no cfbfastR data for their final
#' college season (draft classes predating CFB_DATA_FLOOR = 2014) are excluded.
#'
#' @param cfb_panel tibble. Output of
#'   \code{build_cfb_player_season_panel()} from R/21.
#' @param draft_data tibble. Output of
#'   \code{nflreadr::load_draft_picks()}. Must contain columns:
#'   season, round, pick, team, gsis_id, pfr_player_name, college,
#'   position, age.
#'
#' @return A tibble with one row per matched or attempted draft entry:
#'   \describe{
#'     \item{draft_year}{int: NFL draft year.}
#'     \item{nfl_gsis_id}{chr: NFL player GSIS ID from nflreadr. NA if
#'       unmatched or if draft data has no gsis_id for this player.}
#'     \item{draft_position}{chr: Position drafted at (QB/RB/WR/TE/...).}
#'     \item{draft_round}{int: Draft round.}
#'     \item{draft_pick}{int: Overall pick number.}
#'     \item{draft_age}{dbl: Age at draft.}
#'     \item{cfb_player_name}{chr: Matched player name from R/21 panel.
#'       NA if unmatched.}
#'     \item{cfb_final_season}{int: Player's last college season (draft_year
#'       - 1). The season used as the linkage anchor.}
#'     \item{cfb_primary_team}{chr: College team in the final CFB season.}
#'     \item{n_cfb_seasons}{int: Total CFB seasons available for this player
#'       in R/21 (including seasons before the final).}
#'     \item{match_method}{chr: "exact_name_college", "exact_name_only",
#'       "fuzzy_name", or "unmatched".}
#'     \item{match_confidence}{dbl: 1.0 for exact, (1 - norm_distance) for
#'       fuzzy, 0.0 for unmatched.}
#'   }
#'
#' @examples
#' draft_data <- nflreadr::load_draft_picks()
#' crosswalk  <- link_cfb_to_nfl(cfb_panel, draft_data)
#' table(crosswalk$match_method)
#'
#' @seealso build_translation_features, validate_translation_assumptions
#' @export
link_cfb_to_nfl <- function(cfb_panel, draft_data) {

  # --- Input validation ---
  if (!is.data.frame(cfb_panel) || nrow(cfb_panel) == 0L) {
    stop("'cfb_panel' must be a non-empty data frame.", call. = FALSE)
  }

  missing_cfb <- setdiff(
    c("player_name", "season", "primary_team"), names(cfb_panel)
  )
  if (length(missing_cfb) > 0L) {
    stop(glue(
      "'cfb_panel' is missing required columns: ",
      "{paste(missing_cfb, collapse = ', ')}.\n",
      "Run build_cfb_player_season_panel() from R/21 first."
    ), call. = FALSE)
  }

  if (!is.data.frame(draft_data) || nrow(draft_data) == 0L) {
    stop("'draft_data' must be a non-empty data frame.", call. = FALSE)
  }

  missing_draft <- setdiff(DRAFT_COLS_REQUIRED, names(draft_data))
  if (length(missing_draft) > 0L) {
    stop(glue(
      "'draft_data' is missing expected columns: ",
      "{paste(missing_draft, collapse = ', ')}.\n",
      "Available columns: {paste(names(draft_data), collapse = ', ')}.\n",
      "Verify nflreadr::load_draft_picks() column names have not changed."
    ), call. = FALSE)
  }

  message(strrep("=", 60))
  message("link_cfb_to_nfl()")
  message(glue(
    "CFB panel: {format(nrow(cfb_panel), big.mark = ',')} rows | ",
    "Draft data: {format(nrow(draft_data), big.mark = ',')} rows"
  ))
  message(strrep("=", 60))

  # --- Preprocess CFB panel ---
  # Only keep rows from seasons within CFB data range
  cfb_clean <- cfb_panel %>%
    dplyr::filter(
      season >= CFB_DATA_FLOOR,
      !low_volume,
      games_played >= MIN_CFB_GAMES_PER_SEASON,
      !is.na(player_name)
    ) %>%
    dplyr::mutate(
      norm_name   = .normalize_player_name(player_name),
      norm_college = tolower(trimws(gsub("[^a-zA-Z0-9 ]", "", primary_team)))
    )

  # Count available CFB seasons per player (for n_cfb_seasons)
  cfb_season_counts <- cfb_clean %>%
    dplyr::group_by(player_name, norm_name) %>%
    dplyr::summarise(
      n_cfb_seasons_total = dplyr::n_distinct(season),
      .groups = "drop"
    )

  # --- Preprocess draft data ---
  # Filter to relevant positions and draft classes with CFB coverage
  # (draft class 2015 -> last CFB season 2014 = data floor)
  draft_clean <- draft_data %>%
    dplyr::filter(
      season >= min(TRAINING_DRAFT_CLASSES),
      position %in% TRANSLATION_POSITIONS
    ) %>%
    dplyr::mutate(
      cfb_final_season = as.integer(season) - 1L,
      norm_draft_name  = .normalize_player_name(pfr_player_name),
      norm_draft_coll  = tolower(trimws(
        gsub("[^a-zA-Z0-9 ]", "", dplyr::coalesce(college, ""))
      ))
    )

  message(glue(
    "Draft entries to match: {format(nrow(draft_clean), big.mark = ',')} | ",
    "Positions: {paste(TRANSLATION_POSITIONS, collapse = '/')}"
  ))

  # --- Match each draft entry to CFB panel ---
  results <- purrr::map_dfr(seq_len(nrow(draft_clean)), function(i) {

    d       <- draft_clean[i, ]
    fin_s   <- d$cfb_final_season
    d_norm  <- d$norm_draft_name
    d_coll  <- d$norm_draft_coll

    # CFB rows for the player's final college season
    cfb_final <- cfb_clean %>%
      dplyr::filter(season == fin_s)

    # --- Attempt 1: exact name + college match ---
    exact_both <- cfb_final %>%
      dplyr::filter(norm_name == d_norm, norm_college == d_coll)

    if (nrow(exact_both) >= 1L) {
      row <- exact_both[1L, ]
      n_seas <- cfb_season_counts$n_cfb_seasons_total[
        cfb_season_counts$norm_name == d_norm
      ]
      n_seas <- if (length(n_seas) == 0L) 1L else n_seas[1L]

      return(tibble::tibble(
        draft_year        = as.integer(d$season),
        nfl_gsis_id       = d$gsis_id,
        draft_position    = d$position,
        draft_round       = as.integer(d$round),
        draft_pick        = as.integer(d$pick),
        draft_age         = as.numeric(d$age),
        cfb_player_name   = row$player_name,
        cfb_final_season  = fin_s,
        cfb_primary_team  = row$primary_team,
        n_cfb_seasons     = n_seas,
        match_method      = "exact_name_college",
        match_confidence  = 1.0
      ))
    }

    # --- Attempt 2: exact name match only (college may differ in spelling) ---
    exact_name <- cfb_final %>%
      dplyr::filter(norm_name == d_norm)

    if (nrow(exact_name) >= 1L) {
      row <- exact_name[1L, ]
      n_seas <- cfb_season_counts$n_cfb_seasons_total[
        cfb_season_counts$norm_name == d_norm
      ]
      n_seas <- if (length(n_seas) == 0L) 1L else n_seas[1L]

      return(tibble::tibble(
        draft_year        = as.integer(d$season),
        nfl_gsis_id       = d$gsis_id,
        draft_position    = d$position,
        draft_round       = as.integer(d$round),
        draft_pick        = as.integer(d$pick),
        draft_age         = as.numeric(d$age),
        cfb_player_name   = row$player_name,
        cfb_final_season  = fin_s,
        cfb_primary_team  = row$primary_team,
        n_cfb_seasons     = n_seas,
        match_method      = "exact_name_only",
        match_confidence  = 0.9
      ))
    }

    # --- Attempt 3: fuzzy name match via normalized edit distance ---
    if (nrow(cfb_final) > 0L) {
      cfb_names <- cfb_final$norm_name
      cfb_names[is.na(cfb_names)] <- ""

      if (nchar(d_norm) > 0L) {
        distances   <- adist(d_norm, cfb_names)[1L, ]
        max_len     <- pmax(nchar(d_norm), nchar(cfb_names))
        max_len[max_len == 0L] <- 1L
        norm_dist   <- distances / max_len
        best_idx    <- which.min(norm_dist)
        best_dist   <- norm_dist[best_idx]

        if (best_dist <= FUZZY_NAME_THRESHOLD) {
          row <- cfb_final[best_idx, ]
          n_seas <- cfb_season_counts$n_cfb_seasons_total[
            cfb_season_counts$norm_name == cfb_names[best_idx]
          ]
          n_seas <- if (length(n_seas) == 0L) 1L else n_seas[1L]

          return(tibble::tibble(
            draft_year        = as.integer(d$season),
            nfl_gsis_id       = d$gsis_id,
            draft_position    = d$position,
            draft_round       = as.integer(d$round),
            draft_pick        = as.integer(d$pick),
            draft_age         = as.numeric(d$age),
            cfb_player_name   = row$player_name,
            cfb_final_season  = fin_s,
            cfb_primary_team  = row$primary_team,
            n_cfb_seasons     = n_seas,
            match_method      = "fuzzy_name",
            match_confidence  = round(1.0 - best_dist, 3L)
          ))
        }
      }
    }

    # --- No match ---
    tibble::tibble(
      draft_year        = as.integer(d$season),
      nfl_gsis_id       = d$gsis_id,
      draft_position    = d$position,
      draft_round       = as.integer(d$round),
      draft_pick        = as.integer(d$pick),
      draft_age         = as.numeric(d$age),
      cfb_player_name   = NA_character_,
      cfb_final_season  = fin_s,
      cfb_primary_team  = NA_character_,
      n_cfb_seasons     = 0L,
      match_method      = "unmatched",
      match_confidence  = 0.0
    )
  })

  # --- Match rate summary ---
  match_summary <- results %>%
    dplyr::count(match_method) %>%
    dplyr::mutate(pct = round(100 * n / sum(n), 1))

  message("\nMatch rate by method:")
  for (k in seq_len(nrow(match_summary))) {
    message(glue(
      "  {match_summary$match_method[k]}: ",
      "{format(match_summary$n[k], big.mark = ',')} ",
      "({match_summary$pct[k]}%)"
    ))
  }

  n_unmatched <- sum(results$match_method == "unmatched")
  match_rate  <- round(100 * (1 - n_unmatched / nrow(results)), 1)
  message(glue("\nOverall match rate: {match_rate}%"))

  if (match_rate < 50) {
    warning(glue(
      "Match rate is {match_rate}%, below 50%. ",
      "Review player name normalization and college name alignment between ",
      "nflreadr and cfbfastR before proceeding with feature engineering."
    ), call. = FALSE)
  }

  results
}


# ==============================================================================
# FUNCTION: build_translation_features
# ==============================================================================

#' Build College-to-NFL Translation Feature Matrix
#'
#' @description
#' Engineers the feature matrix used by \code{train_translation_model()}.
#' Performs multi-year weighted averaging of college production, adds context-
#' normalized receiving stats, joins SOS features, centers draft age within
#' position, and computes NFL outcome variables (PPR per game Years 1-3, hit
#' flag).
#'
#' \strong{Context normalization:} Receiving yards per team pass attempts is
#' computed at query time from the CFB panel. It is NOT stored in R/21 output.
#' Applied to WR, TE, and RB receiving yards only.
#'
#' \strong{Multi-year weighting:} Final college season receives weight
#' FINAL_SEASON_WEIGHT (2); all prior seasons receive PRIOR_SEASON_WEIGHT (1).
#' Seasons with fewer than MIN_CFB_GAMES_PER_SEASON games are excluded from
#' the weighted average for that player.
#'
#' \strong{Split:} Returns separate training and prediction tibbles.
#' Training rows: draft_year <= cutoff_year (outcomes computable).
#' Prediction rows: draft_year > cutoff_year (no outcome yet).
#'
#' @param cfb_panel tibble. R/21 CFB player-season panel.
#' @param nfl_panel tibble. R/16 NFL player-season panel.
#' @param sos_panel tibble. R/22 SOS panel loaded from
#'   \code{data/season2_cfb_cache/s2_week8_cfb_sos_panel.rds}.
#' @param crosswalk tibble. Output of \code{link_cfb_to_nfl()}.
#' @param cutoff_year int. Last draft class with 3 full NFL seasons.
#'   Default: CUTOFF_YEAR.
#'
#' @return A named list:
#'   \describe{
#'     \item{training}{Named list of 4 tibbles (QB, RB, WR, TE). Each tibble
#'       contains feature columns, draft_year (for LOCO folds),
#'       ppr_per_game_y13 (regression outcome), is_hit (hit flag), and
#'       qualifying_seasons.}
#'     \item{prediction}{Named list of 4 tibbles (QB, RB, WR, TE). Feature
#'       columns and player metadata only. No outcome columns.}
#'     \item{team_pass_attempts}{tibble (primary_team, season,
#'       team_pass_attempts). Documented for audit trail.}
#'     \item{age_centers}{Named numeric vector: mean draft age per position
#'       computed on training set. Applied to prediction rows.}
#'     \item{impute_medians}{Named list of imputation medians per position,
#'       per model variant. Apply to new data before predicting.}
#'     \item{sos_na_rate}{tibble: SOS NA rate by position in training set.}
#'     \item{schema_tag}{chr: "s2_w10_v1".}
#'   }
#'
#' @examples
#' draft_data  <- nflreadr::load_draft_picks()
#' crosswalk   <- link_cfb_to_nfl(cfb_panel, draft_data)
#' sos_panel   <- readRDS(here::here(
#'   "data", "season2_cfb_cache", "s2_week8_cfb_sos_panel.rds"
#' ))
#' feat <- build_translation_features(
#'   cfb_panel, nfl_panel, sos_panel, crosswalk
#' )
#' nrow(feat$training$WR)
#' names(feat$training$QB)
#'
#' @seealso link_cfb_to_nfl, train_translation_model,
#'   validate_translation_assumptions
#' @export
build_translation_features <- function(cfb_panel,
                                        nfl_panel,
                                        sos_panel,
                                        crosswalk,
                                        cutoff_year = CUTOFF_YEAR) {

  # --- Input validation ---
  stopifnot(
    is.data.frame(cfb_panel), nrow(cfb_panel) > 0L,
    is.data.frame(nfl_panel), nrow(nfl_panel) > 0L,
    is.data.frame(sos_panel), nrow(sos_panel) > 0L,
    is.data.frame(crosswalk), nrow(crosswalk) > 0L,
    is.numeric(cutoff_year), length(cutoff_year) == 1L
  )
  cutoff_year <- as.integer(cutoff_year)

  missing_cfb <- setdiff(CFB_PANEL_COLS_REQUIRED, names(cfb_panel))
  if (length(missing_cfb) > 0L) {
    stop(glue(
      "cfb_panel missing columns: {paste(missing_cfb, collapse = ', ')}"
    ), call. = FALSE)
  }

  missing_nfl <- setdiff(NFL_PANEL_COLS_REQUIRED, names(nfl_panel))
  if (length(missing_nfl) > 0L) {
    stop(glue(
      "nfl_panel missing columns: {paste(missing_nfl, collapse = ', ')}"
    ), call. = FALSE)
  }

  missing_sos <- setdiff(SOS_COLS_REQUIRED, names(sos_panel))
  if (length(missing_sos) > 0L) {
    stop(glue(
      "sos_panel missing columns: {paste(missing_sos, collapse = ', ')}"
    ), call. = FALSE)
  }

  message(strrep("=", 60))
  message("build_translation_features()")
  message(glue("Cutoff year: {cutoff_year}"))
  message(strrep("=", 60))

  # --- Step 1: Team pass attempts per season from CFB panel ---
  message("\nStep 1: Computing team pass attempts (context normalization)...")
  team_pa <- .compute_team_pass_attempts(cfb_panel)
  message(glue(
    "  {format(nrow(team_pa), big.mark = ',')} team-season rows computed."
  ))

  # --- Step 2: Prepare CFB panel for feature computation ---
  # Filter to matched players only; exclude low-volume and collision rows
  matched_cw <- crosswalk %>%
    dplyr::filter(match_method != "unmatched", !is.na(cfb_player_name))

  cfb_filtered <- cfb_panel %>%
    dplyr::filter(
      !low_volume,
      games_played >= MIN_CFB_GAMES_PER_SEASON,
      !is.na(player_name)
    ) %>%
    dplyr::left_join(team_pa, by = c("primary_team", "season")) %>%
    dplyr::mutate(
      # Per-game volume stats
      pass_att_pg    = dplyr::if_else(games_played > 0L,
        pass_attempts  / games_played, NA_real_),
      pass_yd_pg     = dplyr::if_else(games_played > 0L,
        passing_yards  / games_played, NA_real_),
      pass_td_pg     = dplyr::if_else(games_played > 0L,
        pass_tds       / games_played, NA_real_),
      int_pg         = dplyr::if_else(games_played > 0L,
        interceptions  / games_played, NA_real_),
      rush_att_pg    = dplyr::if_else(games_played > 0L,
        rush_attempts  / games_played, NA_real_),
      rush_yd_pg     = dplyr::if_else(games_played > 0L,
        rushing_yards  / games_played, NA_real_),
      rush_td_pg     = dplyr::if_else(games_played > 0L,
        rush_tds       / games_played, NA_real_),
      rec_yd_pg      = dplyr::if_else(games_played > 0L,
        receiving_yards / games_played, NA_real_),
      tgt_pg         = dplyr::if_else(games_played > 0L,
        targets        / games_played, NA_real_),
      rec_td_pg      = dplyr::if_else(games_played > 0L,
        rec_tds        / games_played, NA_real_),
      # Context-normalized receiving: WR, TE, RB receiving role
      # team_pass_attempts may be NA if team had no pass_attempts in panel
      rec_yd_per_team_pass_att = dplyr::if_else(
        !is.na(team_pass_attempts) & team_pass_attempts > 0L,
        receiving_yards / team_pass_attempts,
        NA_real_
      )
    )

  # Feature columns to average across seasons
  qb_feature_cols <- c(
    "pass_att_pg", "pass_yd_pg", "pass_td_pg", "int_pg",
    "completion_pct", "pass_epa_per_attempt", "success_rate", "games_played"
  )
  rb_feature_cols <- c(
    "rush_att_pg", "rush_yd_pg", "rush_td_pg", "rush_epa_per_attempt",
    "rec_yd_per_team_pass_att", "rec_epa_per_target",
    "success_rate", "games_played"
  )
  wr_te_feature_cols <- c(
    "rec_yd_per_team_pass_att", "rec_yd_pg", "tgt_pg", "rec_td_pg",
    "catch_rate", "rec_epa_per_target", "success_rate", "games_played"
  )

  feature_cols_by_pos <- list(
    QB = qb_feature_cols,
    RB = rb_feature_cols,
    WR = wr_te_feature_cols,
    TE = wr_te_feature_cols
  )

  # --- Step 3: Compute weighted multi-year averages per player ---
  message("\nStep 3: Computing weighted multi-year feature averages...")

  player_features <- purrr::map_dfr(
    seq_len(nrow(matched_cw)), function(i) {
      cw_row     <- matched_cw[i, ]
      p_name     <- cw_row$cfb_player_name
      final_s    <- cw_row$cfb_final_season
      d_pos      <- cw_row$draft_position
      d_year     <- cw_row$draft_year
      p_team     <- cw_row$cfb_primary_team

      # All seasons for this player up to and including final college season.
      # Strict branch: name + primary team, so two different humans sharing a
      # name are never pooled. NOTE: transfers keep only their primary-team
      # seasons under the strict branch -- acceptable versus pooling different
      # players. Fall back to name-only when the strict filter finds nothing
      # (e.g. a transfer whose crosswalk team differs from earlier panel
      # rows), warning if R/21 flagged the name as a collision.
      player_cfb <- cfb_filtered %>%
        dplyr::filter(
          player_name == p_name,
          primary_team == p_team,
          season <= final_s
        ) %>%
        dplyr::arrange(season)

      if (nrow(player_cfb) == 0L) {
        player_cfb <- cfb_filtered %>%
          dplyr::filter(
            player_name == p_name,
            season <= final_s
          ) %>%
          dplyr::arrange(season)

        if (nrow(player_cfb) > 0L &&
            "has_name_collision" %in% names(player_cfb) &&
            any(player_cfb$has_name_collision, na.rm = TRUE)) {
          warning(glue(
            "Name-only season match for '{p_name}' ({d_pos}, draft {d_year}): ",
            "R/21 flags has_name_collision for this name, so these rows may ",
            "mix different players."
          ), call. = FALSE)
        }
      }

      if (nrow(player_cfb) == 0L) return(NULL)

      # Determine feature columns for this position. All four positions map to
      # themselves; the list lookup below handles WR/TE sharing a column set.
      pos_key   <- d_pos
      feat_cols <- feature_cols_by_pos[[pos_key]]
      if (is.null(feat_cols)) feat_cols <- wr_te_feature_cols

      # Weighted average, anchored on the crosswalk's true final season so an
      # injury-shortened (filtered) final year cannot shift the 2x weight.
      weighted_feats <- .apply_multiseason_weights(player_cfb, feat_cols,
                                                   final_season = final_s)

      dplyr::bind_cols(
        tibble::tibble(
          draft_year       = d_year,
          draft_position   = d_pos,
          draft_round      = cw_row$draft_round,
          draft_pick       = cw_row$draft_pick,
          draft_age        = cw_row$draft_age,
          nfl_gsis_id      = cw_row$nfl_gsis_id,
          cfb_player_name  = p_name,
          cfb_final_season = final_s,
          cfb_primary_team = cw_row$cfb_primary_team,
          match_method      = cw_row$match_method,
          match_confidence  = cw_row$match_confidence,
          n_cfb_seasons     = cw_row$n_cfb_seasons
        ),
        weighted_feats
      )
    }
  )

  n_built <- nrow(player_features)
  message(glue(
    "  {format(n_built, big.mark = ',')} player feature rows built."
  ))

  # --- Step 4: Join SOS features ---
  message("\nStep 4: Joining SOS features...")

  sos_select <- sos_panel %>%
    dplyr::select(
      player_name, season,
      sos_opp_def_epa_per_play,
      sos_opp_def_success_rate_allowed,
      sos_n_opponents,
      sos_computed
    ) %>%
    dplyr::rename(
      cfb_player_name  = player_name,
      cfb_final_season = season
    )

  player_features <- player_features %>%
    dplyr::left_join(
      sos_select,
      by = c("cfb_player_name", "cfb_final_season")
    )

  sos_na_rate <- player_features %>%
    dplyr::group_by(draft_position) %>%
    dplyr::summarise(
      n_total  = dplyr::n(),
      n_sos_na = sum(is.na(sos_opp_def_epa_per_play)),
      sos_na_pct = round(100 * n_sos_na / n_total, 1),
      .groups = "drop"
    )

  message("  SOS NA rate by position:")
  for (k in seq_len(nrow(sos_na_rate))) {
    message(glue(
      "    {sos_na_rate$draft_position[k]}: ",
      "{sos_na_rate$n_sos_na[k]} / {sos_na_rate$n_total[k]} ",
      "({sos_na_rate$sos_na_pct[k]}% NA)"
    ))
  }

  # --- Step 5: Compute NFL outcomes (training rows only) ---
  message("\nStep 5: Computing NFL outcomes (Years 1-3 PPR average)...")

  nfl_outcomes <- .compute_nfl_outcomes(
    crosswalk      = player_features %>%
      dplyr::filter(!is.na(nfl_gsis_id), draft_year <= cutoff_year),
    nfl_panel      = nfl_panel,
    hit_thresholds = HIT_THRESHOLDS
  )

  player_features <- player_features %>%
    dplyr::left_join(
      nfl_outcomes %>%
        dplyr::select(
          nfl_gsis_id, ppr_per_game_y13, qualifying_seasons, is_hit,
          peak_ppr_per_game, peak_career_year,
          # Season 3 Wave A2: the stat-line outcome must survive this join or
          # train_translation_model() cannot fit component models.
          dplyr::all_of(STAT_COMPONENTS)
        ),
      by = "nfl_gsis_id"
    )

  # --- Step 5b: KEEP THE BUSTS ---
  # Drafted players in the training classes with an NA gsis_id (or no outcome
  # row from the join above) previously carried NA ppr_per_game_y13 and were
  # silently dropped by the !is.na() training filter in Step 7, truncating the
  # outcome distribution: the players the model most needs to see as failures
  # never reached the zero-PPG branch of .compute_nfl_outcomes(). Give them
  # the same zero treatment that branch applies to gsis-matched players with
  # no qualifying NFL seasons.
  zero_fill <- player_features$draft_year <= cutoff_year &
    is.na(player_features$ppr_per_game_y13)
  n_zero_filled <- sum(zero_fill, na.rm = TRUE)
  if (n_zero_filled > 0L) {
    player_features$ppr_per_game_y13[zero_fill]   <- 0
    player_features$qualifying_seasons[zero_fill] <- 0L
    player_features$is_hit[zero_fill]             <- FALSE
    for (cc in STAT_COMPONENTS) {
      player_features[[cc]][zero_fill] <- 0
    }
    # peak_ppr_per_game / peak_career_year stay NA, matching the zero branch.
    message(glue(
      "  Step 5b: retained {n_zero_filled} zero-outcome training players ",
      "(NA gsis_id or no NFL outcome rows) with ppr_per_game_y13 = 0."
    ))
  }

  # --- Step 6: Center draft age within position on training set ---
  message("\nStep 6: Centering draft age within position...")

  training_age_means <- player_features %>%
    dplyr::filter(draft_year <= cutoff_year, !is.na(draft_age)) %>%
    dplyr::group_by(draft_position) %>%
    dplyr::summarise(mean_age = mean(draft_age, na.rm = TRUE), .groups = "drop")

  age_centers <- stats::setNames(
    training_age_means$mean_age,
    training_age_means$draft_position
  )

  player_features <- player_features %>%
    dplyr::mutate(
      age_centered = draft_age - dplyr::coalesce(
        age_centers[draft_position], mean(draft_age, na.rm = TRUE)
      )
    )

  message("  Age centers by position:")
  for (pos in names(age_centers)) {
    message(glue(
      "    {pos}: mean draft age = {round(age_centers[[pos]], 2)}"
    ))
  }

  # --- Step 7: Split into training and prediction sets ---
  training_all   <- player_features %>%
    dplyr::filter(draft_year <= cutoff_year, !is.na(ppr_per_game_y13))
  prediction_all <- player_features %>%
    dplyr::filter(draft_year > cutoff_year)

  message(glue(
    "\nTraining rows: {format(nrow(training_all), big.mark = ',')} | ",
    "Prediction rows: {format(nrow(prediction_all), big.mark = ',')}"
  ))

  # --- Step 8: Split by position ---
  split_by_pos <- function(df) {
    purrr::map(TRANSLATION_POSITIONS, function(pos) {
      df %>% dplyr::filter(draft_position == pos)
    }) %>%
      stats::setNames(TRANSLATION_POSITIONS)
  }

  training_by_pos   <- split_by_pos(training_all)
  prediction_by_pos <- split_by_pos(prediction_all)

  for (pos in TRANSLATION_POSITIONS) {
    message(glue(
      "  {pos}: {format(nrow(training_by_pos[[pos]]), big.mark = ',')} ",
      "training | ",
      "{format(nrow(prediction_by_pos[[pos]]), big.mark = ',')} prediction"
    ))
  }

  # --- Step 9: Compute imputation medians on training set ---
  message("\nStep 9: Computing imputation medians on training set...")

  base_feature_cols_by_pos <- list(
    QB = c(qb_feature_cols,  "age_centered",
           "sos_opp_def_epa_per_play", "sos_opp_def_success_rate_allowed"),
    RB = c(rb_feature_cols,  "age_centered",
           "sos_opp_def_epa_per_play", "sos_opp_def_success_rate_allowed"),
    WR = c(wr_te_feature_cols, "age_centered",
           "sos_opp_def_epa_per_play", "sos_opp_def_success_rate_allowed"),
    TE = c(wr_te_feature_cols, "age_centered",
           "sos_opp_def_epa_per_play", "sos_opp_def_success_rate_allowed")
  )

  enriched_only_cols <- c("draft_round", "draft_pick")

  impute_medians_list <- list()

  for (pos in TRANSLATION_POSITIONS) {
    all_feat_cols <- c(
      base_feature_cols_by_pos[[pos]], enriched_only_cols
    )
    res <- .impute_features(
      df           = training_by_pos[[pos]],
      feature_cols = all_feat_cols,
      impute_medians = NULL
    )
    impute_medians_list[[pos]] <- res$impute_medians
  }

  list(
    training          = training_by_pos,
    prediction        = prediction_by_pos,
    team_pass_attempts  = team_pa,
    age_centers       = age_centers,
    impute_medians    = impute_medians_list,
    base_feature_cols = base_feature_cols_by_pos,
    sos_na_rate       = sos_na_rate,
    schema_tag        = TRANSLATION_SCHEMA_TAG
  )
}


# ==============================================================================
# FUNCTION: train_translation_model
# ==============================================================================

#' Train College-to-NFL Translation Models
#'
#' @description
#' Fits Elastic Net regression models predicting average PPR per game across
#' NFL Years 1-3 from college production features. Produces two model variants
#' per position: base (college production only) and enriched (adds draft
#' capital). LOCO cross-validation evaluates out-of-sample performance on
#' held-out draft classes.
#'
#' \strong{Model variants:}
#' \itemize{
#'   \item \strong{base}: College production features + SOS + age only.
#'     Answers: "What does college production alone predict?"
#'   \item \strong{enriched}: Base features + draft_round + draft_pick.
#'     Answers: "What does production predict after controlling for draft
#'     capital?" Accuracy delta (enriched - base RMSE) quantifies the
#'     contribution of NFL opportunity beyond college production signal.
#' }
#'
#' \strong{LOCO CV:} Each draft class is held out once. Inner
#' cv.glmnet selects lambda.min from the training classes. Final model fitted
#' on all training data using the same alpha.
#'
#' @param feature_matrix list. Output of \code{build_translation_features()}.
#' @param cutoff_year int. Last training draft class. Default: CUTOFF_YEAR.
#' @param alpha numeric. Elastic Net mixing parameter. Default: GLMNET_ALPHA.
#'
#' @return A named list:
#'   \describe{
#'     \item{models_base}{Named list of 4 cv.glmnet objects (QB, RB, WR, TE).
#'       Final models fitted on all training data.}
#'     \item{models_enriched}{Named list of 4 cv.glmnet objects.}
#'     \item{loco_predictions}{tibble: player-level LOCO predictions with
#'       columns nfl_gsis_id, draft_year, draft_position, ppr_per_game_y13,
#'       pred_base, pred_enriched, is_hit.}
#'     \item{loco_performance}{tibble: RMSE, MAE, R-squared per position per
#'       model variant.}
#'     \item{impute_medians}{Passed through from feature_matrix for use in
#'       prediction.}
#'     \item{base_feature_cols}{Passed through from feature_matrix.}
#'   }
#'
#' @examples
#' models <- train_translation_model(feat)
#' models$loco_performance
#'
#' @seealso build_translation_features, evaluate_translation_accuracy
#' @export
train_translation_model <- function(feature_matrix,
                                     cutoff_year = CUTOFF_YEAR,
                                     alpha       = GLMNET_ALPHA) {

  stopifnot(
    is.list(feature_matrix),
    !is.null(feature_matrix$training),
    !is.null(feature_matrix$base_feature_cols),
    !is.null(feature_matrix$impute_medians)
  )
  cutoff_year <- as.integer(cutoff_year)

  message(strrep("=", 60))
  message("train_translation_model()")
  message(glue(
    "Alpha: {alpha} | Cutoff year: {cutoff_year}"
  ))
  message(strrep("=", 60))

  models_base     <- list()
  models_enriched <- list()
  loco_pred_list  <- list()

  # Season 3 Wave A2: per-component model containers (stat-line models)
  models_components_base     <- list()
  models_components_enriched <- list()

  for (pos in TRANSLATION_POSITIONS) {
    message(glue("\n--- Position: {pos} ---"))

    train_df <- feature_matrix$training[[pos]]
    base_cols <- feature_matrix$base_feature_cols[[pos]]
    enr_cols  <- c(base_cols, "draft_round", "draft_pick")
    imp_med   <- feature_matrix$impute_medians[[pos]]

    if (nrow(train_df) < 10L) {
      warning(glue(
        "Position {pos}: only {nrow(train_df)} training rows. ",
        "Model results may be unreliable."
      ), call. = FALSE)
    }

    # Impute training data
    train_base_imp <- .impute_features(
      df             = train_df,
      feature_cols   = base_cols,
      impute_medians = imp_med[base_cols]
    )$imputed_df

    train_enr_imp <- .impute_features(
      df             = train_df,
      feature_cols   = enr_cols,
      impute_medians = imp_med[enr_cols]
    )$imputed_df

    y          <- train_df$ppr_per_game_y13
    draft_yrs  <- train_df$draft_year

    # Validate LOCO folds
    n_folds <- dplyr::n_distinct(draft_yrs)
    message(glue(
      "  Training rows: {format(nrow(train_df), big.mark = ',')} | ",
      "LOCO folds: {n_folds}"
    ))

    # --- Base model: LOCO predictions ---
    X_base <- as.matrix(train_base_imp[, base_cols, drop = FALSE])
    loco_pred_base <- tryCatch(
      .fit_elastic_net_loco(X_base, y, draft_yrs, alpha),
      error = function(e) {
        warning(glue(
          "Position {pos} base LOCO failed: {conditionMessage(e)}"
        ), call. = FALSE)
        rep(mean(y, na.rm = TRUE), length(y))
      }
    )

    # --- Enriched model: LOCO predictions ---
    X_enr <- as.matrix(train_enr_imp[, enr_cols, drop = FALSE])
    loco_pred_enr <- tryCatch(
      .fit_elastic_net_loco(X_enr, y, draft_yrs, alpha),
      error = function(e) {
        warning(glue(
          "Position {pos} enriched LOCO failed: {conditionMessage(e)}"
        ), call. = FALSE)
        rep(mean(y, na.rm = TRUE), length(y))
      }
    )

    # --- Season 3 Wave A2: per-component (stat-line) models -----------------
    # One model per stat component, per feature variant. Predicting the stat
    # line instead of a points total is what makes the output scoring-agnostic:
    # points under any linear scoring are recovered downstream by
    # score_stat_line(), with no refit per format.
    #
    # ZERO-VARIANCE GUARD: several components are structurally absent for a
    # position (QB receptions, WR passing yards). cv.glmnet errors on a
    # constant target, so those are short-circuited to the constant itself
    # rather than allowed to fail the run. A constant prediction is the correct
    # answer for a component the position never accumulates.
    comp_loco_base <- list()
    comp_loco_enr  <- list()
    comp_models_base <- list()
    comp_models_enr  <- list()

    for (comp in STAT_COMPONENTS) {
      y_c <- train_df[[comp]]

      if (is.null(y_c)) {
        stop(glue(
          "train_translation_model(): component column '{comp}' missing from ",
          "training data for position {pos}. Rebuild the feature matrix with ",
          "the Wave A2 outcome (.compute_nfl_outcomes)."
        ), call. = FALSE)
      }

      # DEGENERACY TEST. An exact-constant check is not enough: components like
      # RB passing yards are zero for all but one or two players, so variance is
      # non-zero overall while every LOCO fold and the final standardization
      # still collapse. Elastic net cannot learn anything from a target that is
      # ~entirely zero on 95-250 rows, and the honest prediction is the constant.
      # MIN_COMPONENT_NONZERO_FRAC is a judgment call, stated here to be visible
      # and tunable rather than buried.
      y_nonzero_frac <- mean(!is.na(y_c) & y_c != 0)
      const_target <- isTRUE(stats::sd(y_c, na.rm = TRUE) == 0) ||
        all(is.na(y_c)) ||
        dplyr::n_distinct(y_c[!is.na(y_c)]) <= 1L ||
        y_nonzero_frac < MIN_COMPONENT_NONZERO_FRAC

      if (const_target) {
        const_val <- if (all(is.na(y_c))) 0 else stats::na.omit(y_c)[1]
        comp_loco_base[[comp]] <- rep(const_val, length(y_c))
        comp_loco_enr[[comp]]  <- rep(const_val, length(y_c))
        comp_models_base[[comp]] <- list(constant = const_val)
        comp_models_enr[[comp]]  <- list(constant = const_val)
        next
      }

      comp_loco_base[[comp]] <- tryCatch(
        .fit_elastic_net_loco(X_base, y_c, draft_yrs, alpha),
        error = function(e) {
          warning(glue(
            "Position {pos} component {comp} base LOCO failed: ",
            "{conditionMessage(e)}"
          ), call. = FALSE)
          rep(mean(y_c, na.rm = TRUE), length(y_c))
        }
      )

      comp_loco_enr[[comp]] <- tryCatch(
        .fit_elastic_net_loco(X_enr, y_c, draft_yrs, alpha),
        error = function(e) {
          warning(glue(
            "Position {pos} component {comp} enriched LOCO failed: ",
            "{conditionMessage(e)}"
          ), call. = FALSE)
          rep(mean(y_c, na.rm = TRUE), length(y_c))
        }
      )

      # Final fits. Wrapped: even after the degeneracy test, a component can be
      # sparse enough that glmnet's standardization fails. Degrade to the
      # constant rather than killing a multi-hour run.
      set.seed(GLMNET_SEED)
      comp_models_base[[comp]] <- tryCatch(
        glmnet::cv.glmnet(
          x = X_base, y = y_c, alpha = alpha,
          nfolds = min(10L, nrow(train_df))
        ),
        error = function(e) {
          warning(glue(
            "Position {pos} component {comp} base FINAL fit failed: ",
            "{conditionMessage(e)}. Falling back to constant."
          ), call. = FALSE)
          list(constant = mean(y_c, na.rm = TRUE))
        }
      )

      set.seed(GLMNET_SEED)
      comp_models_enr[[comp]] <- tryCatch(
        glmnet::cv.glmnet(
          x = X_enr, y = y_c, alpha = alpha,
          nfolds = min(10L, nrow(train_df))
        ),
        error = function(e) {
          warning(glue(
            "Position {pos} component {comp} enriched FINAL fit failed: ",
            "{conditionMessage(e)}. Falling back to constant."
          ), call. = FALSE)
          list(constant = mean(y_c, na.rm = TRUE))
        }
      )
    }

    models_components_base[[pos]]     <- comp_models_base
    models_components_enriched[[pos]] <- comp_models_enr

    # --- Final models: fit on all training data ---
    set.seed(GLMNET_SEED)
    final_base <- glmnet::cv.glmnet(
      x      = X_base,
      y      = y,
      alpha  = alpha,
      nfolds = min(10L, nrow(train_df))
    )

    set.seed(GLMNET_SEED)
    final_enr <- glmnet::cv.glmnet(
      x      = X_enr,
      y      = y,
      alpha  = alpha,
      nfolds = min(10L, nrow(train_df))
    )

    models_base[[pos]]     <- final_base
    models_enriched[[pos]] <- final_enr

    # Store LOCO predictions
    # Season 3 Wave A2: carry BOTH the observed component line and the LOCO
    # component predictions. Together these let any downstream consumer compute
    # honest out-of-sample points error under ANY scoring, with no refit:
    # score the predicted line and the observed line under that scoring and
    # compare. This is what replaces the per-format sigma calibration.
    comp_truth_cols <- stats::setNames(
      lapply(STAT_COMPONENTS, function(cc) train_df[[cc]]),
      STAT_COMPONENTS
    )
    comp_pred_base_cols <- stats::setNames(
      lapply(STAT_COMPONENTS, function(cc) comp_loco_base[[cc]]),
      paste0("pred_base_", STAT_COMPONENTS)
    )
    comp_pred_enr_cols <- stats::setNames(
      lapply(STAT_COMPONENTS, function(cc) comp_loco_enr[[cc]]),
      paste0("pred_enriched_", STAT_COMPONENTS)
    )

    loco_pred_list[[pos]] <- dplyr::bind_cols(
      tibble::tibble(
        nfl_gsis_id      = train_df$nfl_gsis_id,
        draft_year       = train_df$draft_year,
        draft_position   = pos,
        ppr_per_game_y13 = y,
        pred_base        = loco_pred_base,
        pred_enriched    = loco_pred_enr,
        is_hit           = train_df$is_hit,
        peak_ppr_per_game = train_df$peak_ppr_per_game,
        peak_career_year  = train_df$peak_career_year
      ),
      tibble::as_tibble(comp_truth_cols),
      tibble::as_tibble(comp_pred_base_cols),
      tibble::as_tibble(comp_pred_enr_cols)
    )

    base_rmse <- sqrt(mean((y - loco_pred_base)^2, na.rm = TRUE))
    enr_rmse  <- sqrt(mean((y - loco_pred_enr)^2,  na.rm = TRUE))
    message(glue(
      "  LOCO RMSE -- base: {round(base_rmse, 3)} | ",
      "enriched: {round(enr_rmse, 3)} | ",
      "delta: {round(enr_rmse - base_rmse, 3)}"
    ))
  }

  loco_predictions <- dplyr::bind_rows(loco_pred_list)

  # Quick LOCO performance table
  loco_performance <- loco_predictions %>%
    dplyr::group_by(draft_position) %>%
    dplyr::summarise(
      n_players      = dplyr::n(),
      rmse_base      = sqrt(mean((ppr_per_game_y13 - pred_base)^2,    na.rm = TRUE)),
      rmse_enriched  = sqrt(mean((ppr_per_game_y13 - pred_enriched)^2, na.rm = TRUE)),
      mae_base       = mean(abs(ppr_per_game_y13 - pred_base),         na.rm = TRUE),
      mae_enriched   = mean(abs(ppr_per_game_y13 - pred_enriched),     na.rm = TRUE),
      r2_base        = 1 - sum((ppr_per_game_y13 - pred_base)^2, na.rm = TRUE) /
        sum((ppr_per_game_y13 - mean(ppr_per_game_y13, na.rm = TRUE))^2, na.rm = TRUE),
      r2_enriched    = 1 - sum((ppr_per_game_y13 - pred_enriched)^2, na.rm = TRUE) /
        sum((ppr_per_game_y13 - mean(ppr_per_game_y13, na.rm = TRUE))^2, na.rm = TRUE),
      accuracy_delta = rmse_enriched - rmse_base,
      .groups        = "drop"
    )

  list(
    models_base       = models_base,
    models_enriched   = models_enriched,
    loco_predictions  = loco_predictions,
    loco_performance  = loco_performance,
    impute_medians    = feature_matrix$impute_medians,
    base_feature_cols = feature_matrix$base_feature_cols,
    # Season 3 Wave A2
    models_components_base     = models_components_base,
    models_components_enriched = models_components_enriched,
    stat_components            = STAT_COMPONENTS
  )
}


# ==============================================================================
# FUNCTION: compare_points_vs_statline (Season 3 Wave A2 backtest)
# ==============================================================================

#' Out-of-sample comparison: direct points model (A1) vs stat-line model (A2)
#'
#' @description
#' Settles which target earns production, on evidence rather than architecture
#' preference. Both arms are evaluated on the SAME leave-one-class-out
#' predictions already produced by \code{train_translation_model()}, so no
#' refitting occurs here and the comparison is honestly out-of-sample.
#'
#' \strong{A1 (direct points):} the model fit on \code{ppr_per_game_y13}. Its
#' prediction is a points number on the REFERENCE_SCORING scale. To evaluate it
#' under a different scoring it must be RESCALED, because it cannot know the
#' stat mix underneath. The rescale used here is the position-level ratio of
#' mean observed points under the target scoring to mean observed points under
#' the reference. This is deliberately the most favourable simple treatment of
#' A1 under a foreign scoring, and it is exactly the band-aid rejected as the
#' production fix. Including it here makes the comparison fair rather than
#' rigged.
#'
#' \strong{A2 (stat line):} the per-component predictions, scored directly under
#' the target scoring by \code{score_stat_line()}. No rescale, no refit.
#'
#' Truth in both arms is the observed component line scored under the target
#' scoring, so the two arms are compared against an identical target.
#'
#' Under REFERENCE_SCORING the A1 rescale factor is 1 by construction, so that
#' row is the clean like-for-like test of whether decomposing into components
#' costs aggregate accuracy.
#'
#' @param model_list Output of \code{train_translation_model()}.
#' @param scoring_list Named list of scoring lists to evaluate, e.g.
#'   \code{list(reference = REFERENCE_SCORING, dk = DK_BEST_BALL_SCORING)}.
#' @param variant Character, "base" or "enriched". Which feature variant to
#'   compare. Default "base", matching what R/29 uses for its prior point
#'   estimate.
#'
#' @return tibble: scoring_label, draft_position, n_players, rmse_a1_points,
#'   rmse_a2_statline, rmse_delta (negative favours A2), mae_a1_points,
#'   mae_a2_statline, a1_rescale_factor.
#'
#' @export
compare_points_vs_statline <- function(model_list,
                                       scoring_list = list(
                                         reference = REFERENCE_SCORING
                                       ),
                                       variant = c("base", "enriched")) {

  variant <- match.arg(variant)
  loco <- model_list$loco_predictions

  if (is.null(loco)) {
    stop("compare_points_vs_statline(): model_list has no loco_predictions.",
         call. = FALSE)
  }

  pred_points_col <- if (variant == "base") "pred_base" else "pred_enriched"
  comp_prefix     <- if (variant == "base") "pred_base_" else "pred_enriched_"
  comp_pred_cols  <- paste0(comp_prefix, STAT_COMPONENTS)

  missing_cols <- setdiff(c(comp_pred_cols, STAT_COMPONENTS), names(loco))
  if (length(missing_cols) > 0L) {
    stop(glue(
      "compare_points_vs_statline(): loco_predictions is missing ",
      "{length(missing_cols)} required column(s), first: {missing_cols[1]}. ",
      "This model object predates Wave A2; retrain before comparing."
    ), call. = FALSE)
  }

  # Observed component line, used as truth under every scoring.
  truth_line <- loco[, STAT_COMPONENTS, drop = FALSE]

  # Predicted component line, renamed to the canonical component names so
  # score_stat_line() can consume it directly.
  pred_line <- loco[, comp_pred_cols, drop = FALSE]
  names(pred_line) <- STAT_COMPONENTS

  purrr::map_dfr(names(scoring_list), function(lbl) {

    sc <- scoring_list[[lbl]]

    truth_pts <- score_stat_line(truth_line, sc)
    a2_pts    <- score_stat_line(pred_line,  sc)
    ref_pts   <- score_stat_line(truth_line, REFERENCE_SCORING)

    tibble::tibble(
      draft_position = loco$draft_position,
      truth_pts      = truth_pts,
      a2_pts         = a2_pts,
      ref_pts        = ref_pts,
      a1_raw         = loco[[pred_points_col]]
    ) %>%
      dplyr::group_by(draft_position) %>%
      dplyr::mutate(
        # Position-level rescale of the A1 points prediction onto the target
        # scoring. Exactly 1 when the target IS the reference scoring.
        a1_rescale_factor = dplyr::if_else(
          mean(ref_pts, na.rm = TRUE) == 0, 1,
          mean(truth_pts, na.rm = TRUE) / mean(ref_pts, na.rm = TRUE)
        ),
        a1_pts = a1_raw * a1_rescale_factor
      ) %>%
      dplyr::summarise(
        scoring_label     = lbl,
        n_players         = dplyr::n(),
        rmse_a1_points    = sqrt(mean((truth_pts - a1_pts)^2, na.rm = TRUE)),
        rmse_a2_statline  = sqrt(mean((truth_pts - a2_pts)^2, na.rm = TRUE)),
        mae_a1_points     = mean(abs(truth_pts - a1_pts), na.rm = TRUE),
        mae_a2_statline   = mean(abs(truth_pts - a2_pts), na.rm = TRUE),
        a1_rescale_factor = dplyr::first(a1_rescale_factor),
        .groups           = "drop"
      ) %>%
      dplyr::mutate(rmse_delta = rmse_a2_statline - rmse_a1_points) %>%
      dplyr::select(
        scoring_label, draft_position, n_players,
        rmse_a1_points, rmse_a2_statline, rmse_delta,
        mae_a1_points, mae_a2_statline, a1_rescale_factor
      )
  })
}


# ==============================================================================
# FUNCTION: evaluate_translation_accuracy
# ==============================================================================

#' Evaluate Out-of-Sample Translation Model Accuracy
#'
#' @description
#' Computes RMSE, MAE, and R-squared from LOCO cross-validation predictions.
#' Reports both regression accuracy (continuous PPR per game) and
#' classification accuracy (hit/bust flag derived from predictions).
#' Also computes the accuracy delta: how much adding draft capital to the
#' enriched model changes RMSE relative to the base model.
#'
#' A negative accuracy_delta means the enriched model is BETTER (lower RMSE).
#' A positive delta means draft capital hurt -- indicating the production
#' features alone were sufficient or that draft capital added noise.
#'
#' @param model_list list. Output of \code{train_translation_model()}.
#' @param feature_matrix list. Output of \code{build_translation_features()}.
#'
#' @return A tibble with one row per position per model variant:
#'   \describe{
#'     \item{draft_position}{chr: QB, RB, WR, or TE.}
#'     \item{model_variant}{chr: "base" or "enriched".}
#'     \item{n_players}{int: number of LOCO players evaluated.}
#'     \item{rmse}{dbl: root mean squared error on LOCO predictions.}
#'     \item{mae}{dbl: mean absolute error.}
#'     \item{r_squared}{dbl: R-squared on LOCO predictions.}
#'     \item{hit_accuracy}{dbl: fraction correct on hit/bust classification
#'       using a threshold-free approach (predicted top-half = hit).}
#'     \item{accuracy_delta}{dbl: enriched RMSE - base RMSE. Negative =
#'       enriched improves over base.}
#'   }
#'
#' @examples
#' perf <- evaluate_translation_accuracy(models, feat)
#' perf %>% dplyr::arrange(draft_position, model_variant)
#'
#' @seealso train_translation_model, identify_translation_gaps
#' @export
evaluate_translation_accuracy <- function(model_list, feature_matrix) {

  stopifnot(
    is.list(model_list),
    !is.null(model_list$loco_predictions),
    !is.null(model_list$loco_performance)
  )

  message(strrep("=", 60))
  message("evaluate_translation_accuracy()")
  message(strrep("=", 60))

  loco <- model_list$loco_predictions

  results <- purrr::map_dfr(TRANSLATION_POSITIONS, function(pos) {
    pos_loco <- loco %>% dplyr::filter(draft_position == pos)

    if (nrow(pos_loco) == 0L) {
      return(NULL)
    }

    y         <- pos_loco$ppr_per_game_y13
    y_mean    <- mean(y, na.rm = TRUE)
    ss_tot    <- sum((y - y_mean)^2, na.rm = TRUE)

    purrr::map_dfr(c("base", "enriched"), function(variant) {
      pred_col <- if (variant == "base") "pred_base" else "pred_enriched"
      pred     <- pos_loco[[pred_col]]

      rmse_v   <- sqrt(mean((y - pred)^2, na.rm = TRUE))
      mae_v    <- mean(abs(y - pred), na.rm = TRUE)
      ss_res   <- sum((y - pred)^2, na.rm = TRUE)
      r2_v     <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_

      # Hit classification: predict top-half by predicted PPR as "hit"
      hit_thresh  <- stats::median(pred, na.rm = TRUE)
      pred_hit    <- pred >= hit_thresh
      actual_hit  <- pos_loco$is_hit
      hit_acc     <- mean(pred_hit == actual_hit, na.rm = TRUE)

      # Accuracy delta computed vs base within same position
      delta <- if (variant == "enriched") {
        rmse_v - sqrt(mean(
          (y - pos_loco$pred_base)^2, na.rm = TRUE
        ))
      } else {
        NA_real_
      }

      tibble::tibble(
        draft_position = pos,
        model_variant  = variant,
        n_players      = nrow(pos_loco),
        rmse           = round(rmse_v,  3),
        mae            = round(mae_v,   3),
        r_squared      = round(r2_v,    3),
        hit_accuracy   = round(hit_acc, 3),
        accuracy_delta = round(delta,   3)
      )
    })
  })

  message("\nLOCO performance summary:")
  print(as.data.frame(results))

  results
}


# ==============================================================================
# FUNCTION: identify_translation_gaps
# ==============================================================================

#' Identify Which College Metrics Translate to NFL Success
#'
#' @description
#' Extracts non-zero Elastic Net coefficients from the final fitted models
#' (all training data, not LOCO) for each position and model variant.
#' Identifies which college features survive regularization and how their
#' coefficients change when draft capital is added to the enriched model.
#'
#' A feature with non-zero coefficient in the base model but zero in the
#' enriched model is one where draft capital acts as a proxy -- draft capital
#' absorbs the signal. A feature surviving in both models has genuine
#' independent predictive value beyond opportunity.
#'
#' @param model_list list. Output of \code{train_translation_model()}.
#'
#' @return A named list of tibbles, one per position (QB, RB, WR, TE):
#'   \describe{
#'     \item{feature}{chr: Feature name.}
#'     \item{base_coef}{dbl: Coefficient in the base model at lambda.min.
#'       0 means regularized out.}
#'     \item{enriched_coef}{dbl: Coefficient in the enriched model.}
#'     \item{base_nonzero}{lgl: TRUE if |base_coef| > 0.}
#'     \item{enriched_nonzero}{lgl: TRUE if |enriched_coef| > 0.}
#'     \item{absorbed_by_capital}{lgl: TRUE if base_nonzero but not
#'       enriched_nonzero -- draft capital proxied this feature.}
#'   }
#'   Rows sorted by |base_coef| descending.
#'
#' @examples
#' gaps <- identify_translation_gaps(models)
#' gaps$WR
#' gaps$QB %>% dplyr::filter(base_nonzero)
#'
#' @seealso train_translation_model, evaluate_translation_accuracy
#' @export
identify_translation_gaps <- function(model_list) {

  stopifnot(
    is.list(model_list),
    !is.null(model_list$models_base),
    !is.null(model_list$models_enriched),
    !is.null(model_list$base_feature_cols)
  )

  message(strrep("=", 60))
  message("identify_translation_gaps()")
  message(strrep("=", 60))

  purrr::map(TRANSLATION_POSITIONS, function(pos) {
    base_model <- model_list$models_base[[pos]]
    enr_model  <- model_list$models_enriched[[pos]]
    base_cols  <- model_list$base_feature_cols[[pos]]
    enr_cols   <- c(base_cols, "draft_round", "draft_pick")

    if (is.null(base_model) || is.null(enr_model)) {
      warning(glue("Position {pos}: model is NULL. Skipping."), call. = FALSE)
      return(NULL)
    }

    # Extract coefficients at lambda.min, excluding intercept
    base_coefs <- as.vector(
      coef(base_model, s = "lambda.min")
    )[-1L]  # drop intercept

    enr_coefs <- as.vector(
      coef(enr_model, s = "lambda.min")
    )[-1L]

    # Align enriched coefficients to base cols (enriched has extra cols at end)
    enr_base_coefs <- enr_coefs[seq_along(base_cols)]
    enr_cap_coefs  <- enr_coefs[
      (length(base_cols) + 1L):length(enr_cols)
    ]

    result <- tibble::tibble(
      feature      = base_cols,
      base_coef    = round(base_coefs,      4),
      enriched_coef = round(enr_base_coefs, 4)
    ) %>%
      dplyr::mutate(
        base_nonzero          = abs(base_coef)    > 0,
        enriched_nonzero      = abs(enriched_coef) > 0,
        absorbed_by_capital   = base_nonzero & !enriched_nonzero
      ) %>%
      dplyr::arrange(dplyr::desc(abs(base_coef)))

    # Report draft capital coefficients
    cap_summary <- tibble::tibble(
      feature      = c("draft_round", "draft_pick")[
        seq_along(enr_cap_coefs)
      ],
      base_coef    = NA_real_,
      enriched_coef = round(enr_cap_coefs, 4),
      base_nonzero = FALSE,
      enriched_nonzero = abs(enr_cap_coefs) > 0,
      absorbed_by_capital = FALSE
    )

    result_full <- dplyr::bind_rows(result, cap_summary)

    message(glue("\n{pos} translation gaps:"))
    message(glue(
      "  Features surviving base regularization: ",
      "{sum(result$base_nonzero)} / {nrow(result)}"
    ))
    message(glue(
      "  Features absorbed by draft capital: ",
      "{sum(result$absorbed_by_capital)}"
    ))

    result_full
  }) %>%
    stats::setNames(TRANSLATION_POSITIONS)
}


# ==============================================================================
# FUNCTION: validate_translation_assumptions
# ==============================================================================

#' Validate Translation Model Assumptions
#'
#' @description
#' Runs a structured set of assumption checks before model training:
#' \enumerate{
#'   \item Match rate check: overall and by position (>= 50% required).
#'   \item Outcome distribution: PPR per game range and outlier check.
#'   \item LOCO fold size: each fold must have >= 5 players per position.
#'   \item Feature distribution: flag columns with > 5% extreme values
#'     (> 5 SD from mean).
#'   \item SOS NA rate: report (does not fail -- NA imputation handles it).
#'   \item Draft year coverage: confirm expected training classes present.
#'   \item Feature collinearity: flag feature pairs with |r| > 0.90.
#' }
#'
#' @param crosswalk tibble. Output of \code{link_cfb_to_nfl()}.
#' @param feature_matrix list. Output of \code{build_translation_features()}.
#' @param verbose logical. Print detailed output. Default: TRUE.
#'
#' @return A named list:
#'   \describe{
#'     \item{valid}{lgl: TRUE if all critical checks passed.}
#'     \item{summary}{tibble: one row per check (check, critical, passed,
#'       detail).}
#'     \item{collinearity_flags}{tibble: feature pairs with |r| > 0.90,
#'       per position.}
#'   }
#'
#' @examples
#' checks <- validate_translation_assumptions(crosswalk, feat)
#' checks$valid
#' checks$summary
#'
#' @seealso build_translation_features, train_translation_model
#' @export
validate_translation_assumptions <- function(crosswalk,
                                              feature_matrix,
                                              verbose = TRUE) {

  stopifnot(
    is.data.frame(crosswalk),
    is.list(feature_matrix),
    !is.null(feature_matrix$training),
    is.logical(verbose), length(verbose) == 1L
  )

  if (verbose) {
    message(strrep("=", 60))
    message("validate_translation_assumptions()")
    message(strrep("=", 60))
  }

  checks      <- list()
  collin_list <- list()

  # --- Check 1: Match rate ---
  total_entries <- nrow(crosswalk)
  n_matched     <- sum(crosswalk$match_method != "unmatched")
  match_rate    <- if (total_entries > 0L) n_matched / total_entries else 0

  checks[["match_rate"]] <- tibble::tibble(
    check    = "match_rate_overall",
    critical = TRUE,
    passed   = match_rate >= 0.50,
    detail   = glue("{round(100 * match_rate, 1)}% matched")
  )

  if (verbose) {
    flag <- if (match_rate >= 0.50) "PASS" else "FAIL (CRITICAL)"
    message(glue(
      "\nCheck 1 -- Match rate: {round(100 * match_rate, 1)}% [{flag}]"
    ))
  }

  # --- Check 2: Outcome distribution ---
  all_outcomes <- purrr::map_dfr(
    TRANSLATION_POSITIONS,
    ~ feature_matrix$training[[.x]] %>%
      dplyr::select(draft_position, ppr_per_game_y13) %>%
      dplyr::filter(!is.na(ppr_per_game_y13))
  )

  ppr_min  <- min(all_outcomes$ppr_per_game_y13, na.rm = TRUE)
  ppr_max  <- max(all_outcomes$ppr_per_game_y13, na.rm = TRUE)
  ppr_ok   <- ppr_min >= 0 && ppr_max <= 60

  checks[["outcome_range"]] <- tibble::tibble(
    check    = "outcome_ppr_range",
    critical = TRUE,
    passed   = ppr_ok,
    detail   = glue(
      "PPR per game range: [{round(ppr_min, 2)}, {round(ppr_max, 2)}]"
    )
  )

  if (verbose) {
    flag <- if (ppr_ok) "PASS" else "FAIL (review outliers)"
    message(glue(
      "\nCheck 2 -- Outcome range: [{round(ppr_min, 2)}, ",
      "{round(ppr_max, 2)}] [{flag}]"
    ))
  }

  # --- Check 3: LOCO fold size (>= 5 players per position per class) ---
  loco_sizes <- purrr::map_dfr(TRANSLATION_POSITIONS, function(pos) {
    df <- feature_matrix$training[[pos]]
    if (nrow(df) == 0L) {
      return(tibble::tibble(
        draft_position = pos, draft_year = NA_integer_,
        n_players = 0L
      ))
    }
    df %>%
      dplyr::count(draft_year) %>%
      dplyr::mutate(draft_position = pos) %>%
      dplyr::rename(n_players = n)
  })

  min_fold_size <- if (nrow(loco_sizes) > 0L) min(loco_sizes$n_players) else 0L
  loco_ok       <- min_fold_size >= 5L

  checks[["loco_fold_size"]] <- tibble::tibble(
    check    = "loco_fold_min_size",
    critical = FALSE,
    passed   = loco_ok,
    detail   = glue("Minimum players per LOCO fold: {min_fold_size}")
  )

  if (verbose) {
    flag <- if (loco_ok) "PASS" else "WARNING (small fold)"
    message(glue(
      "\nCheck 3 -- LOCO fold size: min = {min_fold_size} [{flag}]"
    ))
  }

  # --- Check 4: Feature extremes (> 5 SD) ---
  extreme_flags <- purrr::map_dfr(TRANSLATION_POSITIONS, function(pos) {
    df        <- feature_matrix$training[[pos]]
    feat_cols <- feature_matrix$base_feature_cols[[pos]]
    feat_cols <- intersect(feat_cols, names(df))

    purrr::map_dfr(feat_cols, function(col) {
      vals <- df[[col]]
      if (all(is.na(vals))) return(NULL)
      m  <- mean(vals, na.rm = TRUE)
      s  <- stats::sd(vals, na.rm = TRUE)
      if (is.na(s) || s == 0) return(NULL)
      pct_extreme <- mean(abs(vals - m) > 5 * s, na.rm = TRUE)
      tibble::tibble(
        draft_position = pos,
        feature        = col,
        pct_extreme    = round(100 * pct_extreme, 2)
      )
    })
  })

  extreme_count <- if (
    nrow(extreme_flags) > 0L && "pct_extreme" %in% names(extreme_flags)
  ) {
    sum(extreme_flags$pct_extreme > 5, na.rm = TRUE)
  } else {
    0L
  }

  checks[["feature_extremes"]] <- tibble::tibble(
    check    = "feature_extreme_values",
    critical = FALSE,
    passed   = extreme_count == 0L,
    detail   = glue(
      "{extreme_count} feature(s) with >5% values beyond 5 SD"
    )
  )

  if (verbose) {
    flag <- if (extreme_count == 0L) "PASS" else "WARNING"
    message(glue(
      "\nCheck 4 -- Feature extremes: {extreme_count} flagged [{flag}]"
    ))
    if (extreme_count > 0L && verbose) {
      extreme_flags %>%
        dplyr::filter(pct_extreme > 5) %>%
        dplyr::arrange(dplyr::desc(pct_extreme)) %>%
        as.data.frame() %>%
        print()
    }
  }

  # --- Check 5: Draft year coverage ---
  expected_classes <- TRAINING_DRAFT_CLASSES
  found_classes <- sort(unique(
    purrr::map(TRANSLATION_POSITIONS, function(pos) {
      feature_matrix$training[[pos]]$draft_year
    }) %>%
      unlist()
  ))
  missing_classes <- setdiff(expected_classes, found_classes)

  checks[["draft_year_coverage"]] <- tibble::tibble(
    check    = "draft_year_coverage",
    critical = FALSE,
    passed   = length(missing_classes) == 0L,
    detail   = glue(
      "Found {length(found_classes)} classes. Missing: ",
      "{if (length(missing_classes) == 0L) 'none' else paste(missing_classes, collapse = ', ')}"
    )
  )

  if (verbose) {
    flag <- if (length(missing_classes) == 0L) "PASS" else "WARNING"
    message(glue(
      "\nCheck 5 -- Draft year coverage: {length(found_classes)} classes ",
      "found [{flag}]"
    ))
  }

  # --- Check 6: SOS NA rate (informational) ---
  sos_na_rate <- feature_matrix$sos_na_rate
  max_sos_na  <- max(sos_na_rate$sos_na_pct, na.rm = TRUE)

  checks[["sos_na_rate"]] <- tibble::tibble(
    check    = "sos_na_rate",
    critical = FALSE,
    passed   = TRUE,  # NA handled by imputation; always passes
    detail   = glue("Max SOS NA rate: {max_sos_na}% (imputed)")
  )

  if (verbose) {
    message(glue(
      "\nCheck 6 -- SOS NA rate: max {max_sos_na}% (median imputed) [INFO]"
    ))
  }

  # --- Check 7: Collinearity (|r| > 0.90 pairs) ---
  for (pos in TRANSLATION_POSITIONS) {
    df        <- feature_matrix$training[[pos]]
    feat_cols <- feature_matrix$base_feature_cols[[pos]]
    feat_cols <- intersect(feat_cols, names(df))
    df_num    <- df[, feat_cols, drop = FALSE]

    # Drop constant columns before correlation.
    # Guard: sd() returns NA for single-row positions; isTRUE() prevents
    # NA propagation into the logical vector used for subsetting.
    non_const <- purrr::map_lgl(feat_cols, function(col) {
      v <- df_num[[col]]
      s <- stats::sd(v, na.rm = TRUE)
      !all(is.na(v)) && isTRUE(s > 0)
    })
    feat_cols_nc <- feat_cols[non_const]

    if (length(feat_cols_nc) < 2L) next

    cor_mat <- stats::cor(
      df_num[, feat_cols_nc, drop = FALSE],
      use = "pairwise.complete.obs"
    )

    # Upper triangle pairs only
    pairs <- which(abs(cor_mat) > 0.90 & upper.tri(cor_mat), arr.ind = TRUE)

    if (nrow(pairs) > 0L) {
      collin_list[[pos]] <- tibble::tibble(
        position  = pos,
        feature_1 = feat_cols_nc[pairs[, 1L]],
        feature_2 = feat_cols_nc[pairs[, 2L]],
        correlation = round(cor_mat[pairs], 3)
      )
    }
  }

  collin_all    <- dplyr::bind_rows(collin_list)
  n_collin_pairs <- nrow(collin_all)

  checks[["collinearity"]] <- tibble::tibble(
    check    = "feature_collinearity",
    critical = FALSE,
    passed   = n_collin_pairs == 0L,
    detail   = glue(
      "{n_collin_pairs} feature pair(s) with |r| > 0.90 ",
      "(Elastic Net regularization handles collinearity)"
    )
  )

  if (verbose) {
    flag <- if (n_collin_pairs == 0L) "PASS" else "INFO (Elastic Net robust)"
    message(glue(
      "\nCheck 7 -- Collinearity: {n_collin_pairs} pairs flagged [{flag}]"
    ))
    if (n_collin_pairs > 0L && verbose) print(as.data.frame(collin_all))
  }

  # --- Summary ---
  summary_tbl <- dplyr::bind_rows(checks)
  n_critical   <- sum(summary_tbl$critical & !summary_tbl$passed)
  all_valid    <- n_critical == 0L

  if (verbose) {
    message(strrep("-", 60))
    status <- if (all_valid) "ALL CRITICAL CHECKS PASSED" else
      glue("{n_critical} CRITICAL CHECK(S) FAILED")
    message(glue("\nAssumption check result: {status}"))
    message(strrep("=", 60))
  }

  list(
    valid              = all_valid,
    summary            = summary_tbl,
    collinearity_flags = collin_all,
    loco_fold_sizes    = loco_sizes
  )
}


# ==============================================================================
# FUNCTION: run_week10_pipeline
# ==============================================================================

#' Run the Full Week 10 College-to-NFL Translation Pipeline
#'
#' @description
#' End-to-end execution: loads all required data sources, runs assumption
#' validation, builds feature matrices, trains models, evaluates accuracy,
#' and saves all outputs to disk.
#'
#' \strong{Data loading:} If \code{cfb_panel} or \code{nfl_panel} are NULL,
#' they are built from cache using \code{build_cfb_player_season_panel()} and
#' \code{build_player_season_panel()} respectively. This requires the R/20,
#' R/21, and R/16 caches to be populated.
#'
#' \strong{Outputs saved:}
#' \itemize{
#'   \item \code{s2_week10_crosswalk.rds}
#'   \item \code{s2_week10_feature_matrix.rds}
#'   \item \code{s2_week10_models.rds}
#'   \item \code{s2_week10_performance.rds}
#'   \item \code{s2_week10_predictions.rds}
#' }
#'
#' @param cfb_panel tibble or NULL. Pre-built R/21 CFB panel. If NULL,
#'   built from cache using default seasons.
#' @param nfl_panel tibble or NULL. Pre-built R/16 NFL panel. If NULL,
#'   built from cache covering NFL_OUTCOME_SEASONS.
#' @param sos_rds_path chr. Path to R/22 SOS panel RDS. Default:
#'   \code{SOS_PANEL_DEFAULT_PATH}.
#' @param cutoff_year int. Last training draft class. Default: CUTOFF_YEAR.
#' @param output_dir chr. Directory for output RDS files. Default:
#'   TRANSLATION_CACHE_DIR.
#' @param verbose logical. Progress output. Default: TRUE.
#'
#' @return Named list with all pipeline outputs:
#'   crosswalk, feature_matrix, assumption_checks, model_list,
#'   performance, translation_gaps.
#'
#' @examples
#' # Full run from cache (first time: loads all data, then builds)
#' results <- run_week10_pipeline()
#' results$performance
#' results$translation_gaps$WR
#'
#' # With pre-built panels (faster for re-runs)
#' results <- run_week10_pipeline(
#'   cfb_panel = my_cfb_panel,
#'   nfl_panel = my_nfl_panel
#' )
#'
#' @seealso link_cfb_to_nfl, build_translation_features, train_translation_model
#' @export
run_week10_pipeline <- function(cfb_panel        = NULL,
                                 nfl_panel        = NULL,
                                 sos_rds_path     = SOS_PANEL_DEFAULT_PATH,
                                 cutoff_year      = CUTOFF_YEAR,
                                 prediction_cutoff = 2026L,
                                 output_dir       = TRANSLATION_CACHE_DIR,
                                 verbose          = TRUE) {

  cutoff_year       <- as.integer(cutoff_year)
  prediction_cutoff <- as.integer(prediction_cutoff)
  t_start           <- proc.time()

  # CFB panel must cover the last college season for all prediction classes.
  # For draft class YEAR, last college season = YEAR - 1.
  # So we need CFB seasons through prediction_cutoff - 1.
  cfb_max_season <- min(
    prediction_cutoff - 1L,
    2025L   # hard cap at current R/21 cache ceiling
  )

  if (verbose) {
    message(strrep("=", 70))
    message("run_week10_pipeline() -- College-to-NFL Translation Model")
    message(glue("Cutoff year: {cutoff_year}"))
    message(glue(
      "Training classes: {min(TRAINING_DRAFT_CLASSES)}-{cutoff_year}"
    ))
    message(glue(
      "CFB seasons to load: {CFB_DATA_FLOOR}-{cfb_max_season} ",
      "(covers prediction classes through {prediction_cutoff})"
    ))
    message(strrep("=", 70))
  }

  # --- Validate output directory ---
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) message(glue("Created output dir: {output_dir}"))
  }

  # --- Load CFB panel ---
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
      seasons  = NFL_OUTCOME_SEASONS,
      verbose  = verbose
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
      "Run run_week8_pipeline() from R/22_sos_reconciliation.R first."
    ), call. = FALSE)
  }
  sos_panel <- readRDS(sos_rds_path)
  if (verbose) message(glue(
    "  SOS panel: {format(nrow(sos_panel), big.mark = ',')} rows"
  ))

  # --- Load draft picks ---
  if (verbose) message("\nLoading draft picks from nflreadr...")
  draft_data <- nflreadr::load_draft_picks()
  if (verbose) message(glue(
    "  Draft picks: {format(nrow(draft_data), big.mark = ',')} rows"
  ))

  # --- Step 1: ID linkage ---
  if (verbose) message("\n--- Step 1: ID Linkage ---")
  crosswalk <- link_cfb_to_nfl(cfb_panel, draft_data)

  # --- Step 2: Feature engineering ---
  if (verbose) message("\n--- Step 2: Feature Engineering ---")
  feature_matrix <- build_translation_features(
    cfb_panel   = cfb_panel,
    nfl_panel   = nfl_panel,
    sos_panel   = sos_panel,
    crosswalk   = crosswalk,
    cutoff_year = cutoff_year
  )
  gc(verbose = FALSE)

  # --- Step 3: Assumption validation ---
  if (verbose) message("\n--- Step 3: Assumption Validation ---")
  assumption_checks <- validate_translation_assumptions(
    crosswalk      = crosswalk,
    feature_matrix = feature_matrix,
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

  # --- Step 4: Model training ---
  if (verbose) message("\n--- Step 4: Model Training ---")
  model_list <- train_translation_model(
    feature_matrix = feature_matrix,
    cutoff_year    = cutoff_year,
    alpha          = GLMNET_ALPHA
  )

  # --- Step 5: Evaluation ---
  if (verbose) message("\n--- Step 5: Evaluation ---")
  performance <- evaluate_translation_accuracy(model_list, feature_matrix)

  # --- Step 6: Translation gaps ---
  if (verbose) message("\n--- Step 6: Translation Gaps ---")
  translation_gaps <- identify_translation_gaps(model_list)

  # --- Save outputs ---
  if (verbose) message(glue("\nSaving outputs to: {output_dir}"))

  # Every run writes the canonical file (for downstream readers) AND a
  # date-stamped copy (YYYYMMDD, date only) into output_dir/backups/, so each
  # rebuild is recoverable with no manual step. A same-day rerun overwrites
  # that day's snapshot by design (date only, no time).
  .version_dir <- file.path(output_dir, "backups")
  if (!dir.exists(.version_dir)) dir.create(.version_dir, recursive = TRUE)
  .date_tag <- format(Sys.Date(), "%Y%m%d")
  save_dated <- function(obj, name) {
    saveRDS(obj, file.path(output_dir,   paste0(name, ".rds")))
    saveRDS(obj, file.path(.version_dir, paste0(name, "_", .date_tag, ".rds")))
  }

  save_dated(crosswalk,                   "s2_week10_crosswalk")
  save_dated(feature_matrix,              "s2_week10_feature_matrix")
  save_dated(model_list,                  "s2_week10_models")
  save_dated(performance,                 "s2_week10_performance")
  save_dated(model_list$loco_predictions, "s2_week10_predictions")

  t_elapsed <- round((proc.time() - t_start)[["elapsed"]])

  if (verbose) {
    message(strrep("=", 70))
    message(glue(
      "run_week10_pipeline() complete in {t_elapsed} seconds."
    ))
    message(glue(
      "Training classes: {min(TRAINING_DRAFT_CLASSES)}-{cutoff_year} | ",
      "Cutoff: {cutoff_year}"
    ))
    message("\nPerformance summary (LOCO RMSE):")
    perf_summary <- performance %>%
      dplyr::select(
        draft_position, model_variant, n_players, rmse, r_squared
      )
    print(as.data.frame(perf_summary))
    message(strrep("=", 70))
  }

  list(
    crosswalk         = crosswalk,
    feature_matrix    = feature_matrix,
    assumption_checks = assumption_checks,
    model_list        = model_list,
    performance       = performance,
    translation_gaps  = translation_gaps
  )
}
