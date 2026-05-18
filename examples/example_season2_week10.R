# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 10
# Example Script: College-to-NFL Translation Model
# File: examples/example_season2_week10.R
#
# Purpose: Run the full college-to-NFL translation pipeline on the complete
#          available data span (CFB 2014-2025, draft classes 2015-2026) and
#          score every draft class including the just-drafted 2026 class.
#
# What this script does:
#   1. Loads all required data sources (CFB panel, NFL panel, SOS, draft picks)
#   2. Stops explicitly if 2026 draft picks are not yet in nflreadr
#   3. Runs the full pipeline: ID linkage, feature engineering, assumption
#      validation, model training (LOCO CV, 9 folds), evaluation
#   4. Scores ALL draft classes:
#      - 2015-2023 training classes: LOCO holdout predictions (the model
#        never saw each class when predicting it)
#      - 2024 class: model prediction, 2 NFL seasons observable but out of
#        training scope
#      - 2025 class: model prediction, 1 NFL season observable
#      - 2026 class: pure college-to-NFL projection, no NFL games yet
#   5. Extracts coefficient tables with plain-English feature labels
#   6. Writes 8 CSV files to output/
#
# Outputs:
#   output/s2_week10_predictions_QB.csv
#   output/s2_week10_predictions_RB.csv
#   output/s2_week10_predictions_WR.csv
#   output/s2_week10_predictions_TE.csv
#   output/s2_week10_coefficients_QB.csv
#   output/s2_week10_coefficients_RB.csv
#   output/s2_week10_coefficients_WR.csv
#   output/s2_week10_coefficients_TE.csv
#
# Runtime note: This script loads and processes 12 CFB seasons and 11 NFL
#   seasons. Expect 10-20 minutes on first run depending on cache state.
#   Subsequent runs are faster if caches are already populated.
#
# Dependencies:
#   R/24_translation_model.R (sources R/16, R/20, R/21 automatically)
#   data/season2_cfb_cache/s2_week8_cfb_sos_panel.rds (from R/22)
#   nflreadr::load_draft_picks() -- must include 2026 picks
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


# ==============================================================================
# SOURCE
# ==============================================================================

source(here::here("R", "24_translation_model.R"))


# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Output directory for all CSV files
OUTPUT_DIR <- here::here("output")

# SOS panel from R/22
SOS_PATH <- here::here(
  "data", "season2_cfb_cache", "s2_week8_cfb_sos_panel.rds"
)

# Most recent draft class to include as a prediction target.
# Change this value if you want to limit the prediction window.
PREDICTION_CUTOFF <- 2026L

# Elastic Net alpha -- must match the constant in R/24
EXAMPLE_ALPHA <- GLMNET_ALPHA


# ==============================================================================
# STEP 1: LOAD DRAFT DATA AND VALIDATE 2026 CLASS IS PRESENT
# ==============================================================================

message(strrep("=", 70))
message("Step 1: Loading draft picks and validating 2026 class")
message(strrep("=", 70))

draft_data <- nflreadr::load_draft_picks()

n_2026 <- sum(
  draft_data$season == PREDICTION_CUTOFF &
  draft_data$position %in% TRANSLATION_POSITIONS,
  na.rm = TRUE
)

if (n_2026 == 0L) {
  stop(glue(
    "\n\n2026 draft picks are not yet available in nflreadr.\n",
    "Update nflreadr with: install.packages('nflreadr')\n",
    "or: nflreadr::update_nflreadr()\n",
    "Then re-run this script.\n",
    "If the draft has not yet occurred, set PREDICTION_CUTOFF <- 2025L ",
    "at the top of this file."
  ), call. = FALSE)
}

message(glue(
  "  2026 draft picks found: {format(n_2026, big.mark = ',')} ",
  "skill-position players (QB/RB/WR/TE)"
))
message(glue(
  "  Total draft picks loaded: {format(nrow(draft_data), big.mark = ',')} ",
  "| Seasons: {min(draft_data$season)}-{max(draft_data$season)}"
))


# ==============================================================================
# STEP 2: RUN FULL PIPELINE
# ==============================================================================

message(strrep("=", 70))
message("Step 2: Running full translation pipeline")
message(glue(
  "CFB seasons: {CFB_DATA_FLOOR}-{PREDICTION_CUTOFF - 1L} | ",
  "Training classes: {min(TRAINING_DRAFT_CLASSES)}-{CUTOFF_YEAR} ",
  "({length(TRAINING_DRAFT_CLASSES)} folds) | ",
  "Prediction through: {PREDICTION_CUTOFF}"
))
message(strrep("=", 70))

pipeline_results <- run_week10_pipeline(
  cfb_panel         = NULL,       # loaded from R/21 cache
  nfl_panel         = NULL,       # loaded from R/16 cache
  sos_rds_path      = SOS_PATH,
  cutoff_year       = CUTOFF_YEAR,
  prediction_cutoff = PREDICTION_CUTOFF,
  output_dir        = here::here("data", "season2_cache"),
  verbose           = TRUE
)

# Unpack pipeline components
crosswalk      <- pipeline_results$crosswalk
feature_matrix <- pipeline_results$feature_matrix
model_list     <- pipeline_results$model_list
performance    <- pipeline_results$performance
translation_gaps <- pipeline_results$translation_gaps


# ==============================================================================
# STEP 3: SCORE ALL DRAFT CLASSES (TRAINING + PREDICTION)
# ==============================================================================

message(strrep("=", 70))
message("Step 3: Scoring all draft classes (2015-2026)")
message(strrep("=", 70))

# Helper: apply stored imputation medians to a new data frame.
# Replaces NAs with the training-set median for each feature column.
# Used to prepare prediction rows before applying final glmnet models.
.apply_stored_medians <- function(df, feature_cols, medians) {
  for (col in feature_cols) {
    if (!col %in% names(df)) next
    med     <- medians[[col]]
    na_rows <- is.na(df[[col]])
    if (any(na_rows) && !is.na(med)) {
      df[[col]][na_rows] <- med
    }
  }
  df
}

# Helper: classify draft class type for labeling
.classify_draft_type <- function(draft_year, cutoff_year) {
  dplyr::case_when(
    draft_year <= cutoff_year                  ~ "training",
    draft_year == cutoff_year + 1L             ~ "2yr_partial",
    draft_year == cutoff_year + 2L             ~ "1yr_partial",
    TRUE                                       ~ "no_nfl_seasons"
  )
}

# Build scored table for each position
scored_list <- purrr::map(TRANSLATION_POSITIONS, function(pos) {

  message(glue("  Scoring position: {pos}"))

  base_cols <- feature_matrix$base_feature_cols[[pos]]
  enr_cols  <- c(base_cols, "draft_round", "draft_pick")
  imp_med   <- feature_matrix$impute_medians[[pos]]

  # --- Training rows: use LOCO holdout predictions ---
  train_loco <- model_list$loco_predictions %>%
    dplyr::filter(draft_position == pos) %>%
    dplyr::select(
      nfl_gsis_id, draft_year, pred_base, pred_enriched,
      ppr_per_game_y13, is_hit
    )

  train_meta <- feature_matrix$training[[pos]] %>%
    dplyr::select(
      nfl_gsis_id, draft_year, draft_round, draft_pick, draft_age,
      cfb_player_name, cfb_primary_team, match_method, match_confidence,
      n_cfb_seasons, qualifying_seasons,
      dplyr::all_of(base_cols)   # observed feature values (pre-imputation)
    )

  train_scored <- train_loco %>%
    dplyr::left_join(train_meta, by = c("nfl_gsis_id", "draft_year")) %>%
    dplyr::mutate(
      draft_class_type = "training",
      actual_ppr_y13   = ppr_per_game_y13,
      actual_is_hit    = is_hit
    ) %>%
    dplyr::select(-ppr_per_game_y13, -is_hit)

  # --- Prediction rows: apply final models ---
  pred_df <- feature_matrix$prediction[[pos]]

  if (nrow(pred_df) == 0L) {
    message(glue("    No prediction rows for {pos}."))
    return(train_scored %>% dplyr::mutate(
      draft_class_type = .classify_draft_type(draft_year, CUTOFF_YEAR)
    ))
  }

  # Impute using training-set medians
  pred_base_imp <- .apply_stored_medians(pred_df, base_cols, imp_med)
  pred_enr_imp  <- .apply_stored_medians(pred_df, enr_cols, imp_med)

  X_base <- as.matrix(pred_base_imp[, base_cols, drop = FALSE])
  X_enr  <- as.matrix(pred_enr_imp[, enr_cols,  drop = FALSE])

  pred_base_scores <- as.vector(predict(
    model_list$models_base[[pos]],
    newx = X_base,
    s    = "lambda.min"
  ))

  pred_enr_scores <- as.vector(predict(
    model_list$models_enriched[[pos]],
    newx = X_enr,
    s    = "lambda.min"
  ))

  pred_scored <- pred_df %>%
    dplyr::select(
      nfl_gsis_id, draft_year, draft_round, draft_pick, draft_age,
      cfb_player_name, cfb_primary_team, match_method, match_confidence,
      n_cfb_seasons,
      dplyr::all_of(base_cols)   # observed feature values (pre-imputation)
    ) %>%
    dplyr::mutate(
      pred_base          = pred_base_scores,
      pred_enriched      = pred_enr_scores,
      actual_ppr_y13     = NA_real_,
      actual_is_hit      = NA,
      qualifying_seasons = NA_integer_,
      draft_class_type   = .classify_draft_type(draft_year, CUTOFF_YEAR)
    )

  dplyr::bind_rows(train_scored, pred_scored)
}) %>%
  stats::setNames(TRANSLATION_POSITIONS)


# ==============================================================================
# STEP 3B: COMPUTE TRANSLATION SCORE (0-100 PERCENTILE)
# ==============================================================================
# Percentile rank of pred_ppr_enriched within position, computed across ALL
# draft classes combined (training + prediction). A score of 85 means this
# player is projected better than 85% of all players at their position in
# the full dataset.
#
# Future-proof: every re-run recalculates percentiles from the full scored
# population at that time. As CUTOFF_YEAR advances and more training classes
# enter, scores recalibrate automatically. No hardcoded reference point.
#
# Computation: (rank - 1) / (n - 1) * 100, giving 0 to the lowest scorer
# and 100 to the highest. ties.method = "average" handles tied predictions.
# NA pred_ppr_enriched produces NA score (unmatched players).

scored_list <- purrr::map(TRANSLATION_POSITIONS, function(pos) {
  df <- scored_list[[pos]]
  n  <- sum(!is.na(df$pred_enriched))

  if (n <= 1L) {
    return(dplyr::mutate(df, translation_score = NA_real_))
  }

  dplyr::mutate(df,
    translation_score = dplyr::if_else(
      !is.na(pred_enriched),
      round(
        100 * (rank(pred_enriched, ties.method = "average", na.last = "keep") - 1) /
          (n - 1),
        1
      ),
      NA_real_
    )
  )
}) %>%
  stats::setNames(TRANSLATION_POSITIONS)


# ==============================================================================
# STEP 4: BUILD COEFFICIENT TABLES WITH PLAIN-ENGLISH LABELS
# ==============================================================================

message(strrep("=", 70))
message("Step 4: Building coefficient tables")
message(strrep("=", 70))

# Master feature label lookup -- covers all features across all positions.
# Annotated with the weighting scheme applied and any documented limitations.
FEATURE_LABELS <- c(
  # --- Volume features: per game ---
  pass_att_pg    = "Pass attempts per game (final-season double-weighted avg)",
  pass_yd_pg     = "Passing yards per game (final-season double-weighted avg)",
  pass_td_pg     = "Passing touchdowns per game (final-season double-weighted avg)",
  int_pg         = "Interceptions thrown per game (final-season double-weighted avg)",
  rush_att_pg    = "Rush attempts per game (final-season double-weighted avg)",
  rush_yd_pg     = "Rushing yards per game (final-season double-weighted avg)",
  rush_td_pg     = "Rushing touchdowns per game (final-season double-weighted avg)",
  rec_yd_pg      = "Receiving yards per game -- raw, un-normalized (double-weighted avg)",
  tgt_pg         = paste0(
    "Targets per game -- NOTE: under-counted in cfbfastR; ",
    "incompletions often lack named receiver (double-weighted avg)"
  ),
  rec_td_pg      = "Receiving touchdowns per game (final-season double-weighted avg)",

  # --- Efficiency features ---
  completion_pct        = "Completion percentage (final-season double-weighted avg)",
  pass_epa_per_attempt  = "EPA per pass attempt -- value added per dropback (double-weighted avg)",
  rush_epa_per_attempt  = "EPA per rush attempt -- value added per carry (double-weighted avg)",
  rec_epa_per_target    = "EPA per target as receiver -- value added per look (double-weighted avg)",
  catch_rate            = "Catch rate: receptions / targets (double-weighted avg)",

  # --- Context-normalized volume ---
  rec_yd_per_team_pass_att = paste0(
    "Receiving yards per team pass attempt -- context-normalized receiving role; ",
    "adjusts for pass-heavy vs run-heavy offenses (double-weighted avg)"
  ),

  # --- General efficiency ---
  success_rate = "Proportion of plays with positive EPA -- overall play quality (double-weighted avg)",

  # --- Context / sample size ---
  games_played = "Weighted average games played per college season",

  # --- Player characteristics ---
  age_centered = paste0(
    "Draft age relative to position-specific mean -- positive = older than avg; ",
    "centering computed on training set (2015-", CUTOFF_YEAR, " classes)"
  ),

  # --- Strength of schedule (from R/22) ---
  sos_opp_def_epa_per_play = paste0(
    "Mean opponent defensive EPA allowed per play across all opponents faced; ",
    "lower (more negative) = harder schedule (R/22 SOS feature)"
  ),
  sos_opp_def_success_rate_allowed = paste0(
    "Mean opponent defensive success rate allowed across all opponents faced; ",
    "lower = harder schedule (R/22 SOS feature)"
  ),

  # --- Draft capital (enriched model only) ---
  draft_round = paste0(
    "NFL draft round [enriched model only] -- ",
    "proxy for NFL team evaluation and guaranteed opportunity"
  ),
  draft_pick  = paste0(
    "Overall NFL draft pick number [enriched model only] -- ",
    "finer-grained opportunity signal within round; ",
    "negative coefficient expected (lower pick = better)"
  )
)

# Build coefficient CSV for each position
coef_list <- purrr::map(TRANSLATION_POSITIONS, function(pos) {

  gaps <- translation_gaps[[pos]]
  if (is.null(gaps) || nrow(gaps) == 0L) return(NULL)

  gaps %>%
    dplyr::mutate(
      plain_english_label = dplyr::coalesce(
        FEATURE_LABELS[feature],
        paste0(feature, " (no label defined)")
      )
    ) %>%
    dplyr::select(
      feature,
      plain_english_label,
      base_coef,
      enriched_coef,
      base_nonzero,
      enriched_nonzero,
      absorbed_by_capital
    )
}) %>%
  stats::setNames(TRANSLATION_POSITIONS)


# ==============================================================================
# STEP 5: ASSEMBLE FINAL PREDICTION CSVS AND WRITE ALL OUTPUTS
# ==============================================================================

message(strrep("=", 70))
message("Step 5: Writing output files")
message(strrep("=", 70))

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

for (pos in TRANSLATION_POSITIONS) {

  # --- Predictions CSV ---
  pred_out <- scored_list[[pos]] %>%
    dplyr::arrange(draft_year, dplyr::desc(translation_score)) %>%
    dplyr::mutate(draft_position = pos) %>%
    dplyr::select(
      draft_year,
      draft_class_type,
      player_name         = cfb_player_name,
      nfl_gsis_id,
      draft_position,
      draft_round,
      draft_pick,
      draft_age,
      college             = cfb_primary_team,
      match_method,
      match_confidence,
      n_cfb_seasons,
      translation_score,
      pred_ppr_base       = pred_base,
      pred_ppr_enriched   = pred_enriched,
      actual_ppr_y13,
      actual_is_hit,
      qualifying_seasons,
      # Feature values used by the model -- one column per feature.
      # NA indicates the value was missing and imputed with position median
      # for prediction. See s2_week10_coefficients_{pos}.csv for
      # plain-English labels and coefficient values for each feature.
      dplyr::any_of(feature_matrix$base_feature_cols[[pos]])
    )

  pred_path <- file.path(
    OUTPUT_DIR,
    glue("s2_week10_predictions_{pos}.csv")
  )
  utils::write.csv(pred_out, pred_path, row.names = FALSE)
  message(glue(
    "  Saved: s2_week10_predictions_{pos}.csv ",
    "({format(nrow(pred_out), big.mark = ',')} rows)"
  ))

  # --- Coefficients CSV ---
  coef_out  <- coef_list[[pos]]
  coef_path <- file.path(
    OUTPUT_DIR,
    glue("s2_week10_coefficients_{pos}.csv")
  )
  utils::write.csv(coef_out, coef_path, row.names = FALSE)
  message(glue(
    "  Saved: s2_week10_coefficients_{pos}.csv ",
    "({format(nrow(coef_out), big.mark = ',')} rows)"
  ))
}


# ==============================================================================
# SUMMARY
# ==============================================================================

message(strrep("=", 70))
message("example_season2_week10.R complete")
message(strrep("=", 70))

message(glue("\nModel scope:"))
message(glue(
  "  CFB seasons       : {CFB_DATA_FLOOR}-{PREDICTION_CUTOFF - 1L}"
))
message(glue(
  "  Training classes  : {min(TRAINING_DRAFT_CLASSES)}-{CUTOFF_YEAR} ",
  "({length(TRAINING_DRAFT_CLASSES)} LOCO folds)"
))
message(glue(
  "  Prediction classes: {CUTOFF_YEAR + 1L}-{PREDICTION_CUTOFF}"
))
message(glue(
  "  2026 picks scored : {n_2026} skill-position players"
))

message(glue("\nLOCO performance by position (enriched model):"))
performance %>%
  dplyr::filter(model_variant == "enriched") %>%
  dplyr::select(draft_position, n_players, rmse, r_squared) %>%
  dplyr::rename(
    position  = draft_position,
    n         = n_players
  ) %>%
  as.data.frame() %>%
  print()

message(glue("\nOutput files written to: {OUTPUT_DIR}"))
message(glue(
  "  Predictions : s2_week10_predictions_{{QB,RB,WR,TE}}.csv"
))
message(glue(
  "  Coefficients: s2_week10_coefficients_{{QB,RB,WR,TE}}.csv"
))
message(strrep("=", 70))
