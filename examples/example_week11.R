# =============================================================================
# NFL Analytics Toolkit - Week 11 Usage Examples
# =============================================================================
# File:    examples/example_week11.R
# Purpose: Guided walkthrough of R/13_ensemble_pipeline.R with 6 self-contained
#          examples covering the full pipeline, individual function usage,
#          the Week 10 postseason join fix, and feature selection for Week 12.
#
# Prerequisites:
#   - nflfastR, nflreadr, dplyr, tidymodels, xgboost, ranger, glmnet, stacks
#   - R/01_data_loading.R, R/10_opponent_features.R, R/11_xgboost_fantasy.R
#   - R/13_ensemble_pipeline.R
#   - output/week9_models_2025.rds and output/week9_predictions_2025.rds
#     (produced by the Week 9 walkthrough)
#   - output/ml_data_2025.rds  OR  pbp + feature matrix available to build it
#
# NFL Context:
#   Ensemble models are used here because three model families capture
#   different kinds of signal in NFL data:
#   - XGBoost learns non-linear interactions (e.g., WRs on pass-funnel
#     defenses with high target share have disproportionate upside)
#   - Random Forest handles correlated features robustly via bagging
#     (usage metrics and EPA trend metrics are typically r > 0.6)
#   - Elastic Net recovers stable linear patterns (target share trends
#     for established receivers have near-linear PPR relationships)
#   Stacking the three via a ridge meta-learner captures the parts each
#   model does well without dramatically overfitting.
# =============================================================================

library(here)
library(dplyr)
library(glue)
library(tidyr)

# Source required modules (in dependency order)
source(here::here("R", "01_data_loading.R"))
source(here::here("R", "10_opponent_features.R"))
source(here::here("R", "11_xgboost_fantasy.R"))
source(here::here("R", "13_ensemble_pipeline.R"))

SEASON      <- 2025L
OUTPUT_DIR  <- here::here("output")


# =============================================================================
# EXAMPLE 1: Full end-to-end pipeline (recommended starting point)
# =============================================================================
# run_full_pipeline() handles all data loading, feature engineering, model
# training, stacking, and artifact saving in a single call.
# Use this when you want the final predictions without manual steps.
#
# Checkpoint: After running, output/ should contain:
#   week11_suite_passer_2025.rds
#   week11_suite_rusher_2025.rds
#   week11_suite_receiver_2025.rds
#   week11_ensemble_passer_2025.rds
#   week11_ensemble_rusher_2025.rds
#   week11_ensemble_receiver_2025.rds
#   week11_full_results_2025.rds

cat("====================================================\n")
cat("EXAMPLE 1: Full pipeline\n")
cat("====================================================\n")

# If ml_data is already built from a previous run, run_full_pipeline
# detects it automatically and skips to model training.
# Set force_rerun = TRUE to rebuild from scratch.

results <- run_full_pipeline(
  predict_season = SEASON,
  train_seasons  = 2023:2025,
  train_weeks    = 1:14,
  test_weeks     = 15:18,
  positions      = c("passer", "rusher", "receiver"),
  force_rerun    = FALSE,   # use existing artifacts if present
  output_dir     = OUTPUT_DIR
)

cat("\nCross-position summary (RMSE on test weeks 15-18):\n")
results$summary %>%
  dplyr::filter(!grepl("baseline", model)) %>%
  dplyr::select(position, model, rmse, mae, rsq) %>%
  dplyr::arrange(position, rmse) %>%
  print()

cat("\nKey insight: Does the ensemble beat the best single model for each position?\n")
model_wins <- results$summary %>%
  dplyr::filter(!grepl("baseline", model)) %>%
  dplyr::group_by(position) %>%
  dplyr::slice_min(rmse, n = 1L) %>%
  dplyr::select(position, model, rmse)
print(model_wins)


# =============================================================================
# EXAMPLE 2: Train a single-position suite and inspect CV results
# =============================================================================
# Use train_model_suite() directly when you want to inspect the tuning results
# for a specific position before committing to the full pipeline.
# This is also useful for iterating on hyperparameter grids.
#
# NFL context: Running backs have the most variance in PPR scoring because
# touchdown rate is highly situational. The RF model often performs well for
# rushers because it handles the noisy touchdown binary more robustly than
# the linear terms in Elastic Net.

cat("\n====================================================\n")
cat("EXAMPLE 2: Single-position suite with CV inspection\n")
cat("====================================================\n")

# Load pre-built ml_data if it exists, otherwise build it
ml_data_path <- file.path(OUTPUT_DIR, glue("ml_data_{SEASON}.rds"))
if (!file.exists(ml_data_path)) {
  stop(glue("ml_data artifact not found: {ml_data_path}\n",
            "Run Example 1 first to build the ml_data artifact."),
       call. = FALSE)
}
ml_data <- readRDS(ml_data_path)
cat(glue("Loaded ml_data: {nrow(ml_data)} rows\n"))

# Train just the rusher suite
suite_rb <- train_model_suite(
  ml_data,
  position    = "rusher",
  train_weeks = 1:14,
  test_weeks  = 15:18,
  n_folds     = 5L
)

cat("\nCV metrics summary for rusher (best candidates per model):\n")

# XGBoost best params
xgb_best <- tune::select_best(suite_rb$xgb_res, metric = "rmse")
cat("XGBoost best parameters:\n")
print(xgb_best %>% dplyr::select(-c(.config)))

# RF best params
rf_best  <- tune::select_best(suite_rb$rf_res, metric = "rmse")
cat("\nRandom Forest best parameters:\n")
print(rf_best %>% dplyr::select(-c(.config)))

# EN best params
en_best  <- tune::select_best(suite_rb$en_res, metric = "rmse")
cat("\nElastic Net best parameters (penalty, mixture):\n")
print(en_best %>% dplyr::select(-c(.config)))

cat("\nPrediction correlation between base models (rusher):\n")
print(suite_rb$cv_correlation)
cat("Interpretation: r < 0.90 means stacking is likely to add value.\n")
cat("                r > 0.90 means models make similar errors; ensemble gain is marginal.\n")


# =============================================================================
# EXAMPLE 3: Head-to-head model comparison including Week 10 fix
# =============================================================================
# compare_models() evaluates all three models on held-out test weeks and runs
# Wilcoxon tests for statistical significance of performance differences.
# The Week 10 known limitation is resolved here: predicted_week = 22 in the
# Week 9 predictions artifact is detected, logged, and dropped before joining.
#
# NFL context: The Wilcoxon test uses per-week absolute errors rather than
# a t-test because fantasy point error distributions are right-skewed. A
# handful of bust weeks (player is projected 15, scores 2) inflate the mean
# but the Wilcoxon median comparison is more robust to these outliers.

cat("\n====================================================\n")
cat("EXAMPLE 3: Model comparison with postseason join fix\n")
cat("====================================================\n")

comp_rb <- compare_models(
  suite_rb,
  week9_preds_path = file.path(OUTPUT_DIR, glue("week9_predictions_{SEASON}.rds")),
  baselines        = TRUE
)

cat("\nMetrics table (sorted best to worst RMSE):\n")
print(comp_rb$metrics_table)

cat("\nWilcoxon test results (paired per-week absolute errors):\n")
print(comp_rb$wilcoxon_tests)
cat("p < 0.05 = statistically significant difference in median error.\n")

# Week 10 postseason fix log
if (nrow(comp_rb$week9_remap_log) > 0L) {
  cat("\nWeek 10 postseason join fix log:\n")
  print(comp_rb$week9_remap_log)
  cat("These postseason week codes were detected in week9_predictions_2025.rds.\n")
  cat("They were dropped before the regular-season join (predicted_week > 18).\n")
} else {
  cat("\nNo postseason week codes detected in Week 9 artifact.\n")
}

# Week 9 comparison (if available)
if (!is.null(comp_rb$week9_comparison)) {
  cat("\nWeek 9 regression vs Week 11 ensemble comparison:\n")
  print(comp_rb$week9_comparison)
  cat("ensemble_better = TRUE means stacking the three models beat the",
      "Week 9 single-model XGBoost on the overlapping rows.\n")
}


# =============================================================================
# EXAMPLE 4: Build ensemble and inspect blend weights
# =============================================================================
# build_ensemble() stacks the three models using stacks::blend_predictions().
# The blend weights reveal how much the meta-learner trusts each model.
# Near-equal weights suggest the models have different error profiles and all
# contribute to the ensemble.
#
# NFL context: If the meta-learner zeros out Elastic Net for rushers, it means
# the non-linear interactions from XGBoost and RF dominate -- which makes sense
# for RBs where touchdown proximity and game script interactions matter more
# than stable linear trends.

cat("\n====================================================\n")
cat("EXAMPLE 4: Ensemble construction and blend weights\n")
cat("====================================================\n")

ensemble_rb <- build_ensemble(suite_rb)

cat("\nBlend weights (higher = more influence in final prediction):\n")
print(ensemble_rb$member_weights)

cat("\nEnsemble vs best single-model comparison:\n")
best_single_rmse <- min(comp_rb$metrics_table %>%
                          dplyr::filter(!grepl("baseline", model)) %>%
                          dplyr::filter(model != "ensemble") %>%
                          dplyr::pull(rmse))

cat(glue("Best single model RMSE:  {round(best_single_rmse, 3)}\n"))
cat(glue("Ensemble RMSE:           {round(ensemble_rb$test_metrics$rmse, 3)}\n"))
pct_improvement <- (best_single_rmse - ensemble_rb$test_metrics$rmse) / best_single_rmse * 100
cat(glue("Improvement:             {round(pct_improvement, 1)}%\n"))
cat("Expected range from NFL ensemble literature: 3-8% improvement over best single model.\n")

cat("\nFirst 10 ensemble predictions (test weeks 15-18):\n")
print(ensemble_rb$test_preds %>%
        dplyr::slice_head(n = 10L) %>%
        dplyr::select(player_id, week, actual, predicted, residual))


# =============================================================================
# EXAMPLE 5: Feature selection for Week 12 simplification
# =============================================================================
# select_features() identifies which of the 40-60 engineered features from
# Weeks 5-8 are most important across all three models. The consensus ranking
# and reduced-model test confirm whether 15-20 features capture the same
# predictive power as the full set.
#
# NFL context: Target share and EPA trend features typically dominate for
# receivers; usage rate and red zone opportunity for rushers; clean-pocket
# EPA and opponent CPOE allowed for passers. Features that appear in the
# top 20 for all three models are the most robustly predictive across
# different statistical approaches.
#
# NFL practical use: The top_features vector from this example is the right
# input to Week 12's generate_preseason_projections() -- a simpler model
# with 15-20 features is more stable and generalizes better to future seasons.

cat("\n====================================================\n")
cat("EXAMPLE 5: Feature selection for Week 12\n")
cat("====================================================\n")

feat_rb <- select_features(
  suite_rb,
  n_top                  = 20L,
  reduced_rmse_tolerance = 0.5
)

cat("\nTop 10 consensus features (rusher):\n")
feat_rb$importance_table %>%
  dplyr::slice_head(n = 10L) %>%
  dplyr::select(feature, mean_importance, is_consensus, consensus_rank) %>%
  print()

cat(glue("\nFull-feature RMSE:   {round(feat_rb$full_rmse, 3)}\n"))
cat(glue("Reduced (top 20) RMSE: {round(feat_rb$reduced_rmse, 3)}\n"))
cat(glue("Within tolerance (< +0.5 RMSE): {feat_rb$within_tolerance}\n"))

if (feat_rb$within_tolerance) {
  cat("\nConclusion: Top 20 features are sufficient for Week 12.\n")
  cat("Use feat_rb$top_features to subset the feature matrix.\n")
} else {
  cat("\nConclusion: Full feature set needed. Top 20 features lose predictive power.\n")
  cat("Consider increasing n_top or investigating which features are critical.\n")
}

cat("\nTop 20 feature names (copy to Week 12 feature matrix):\n")
cat(paste(feat_rb$top_features, collapse = ", "), "\n")


# =============================================================================
# EXAMPLE 6: Applying ensemble to a new prediction week
# =============================================================================
# Once the ensemble is fit, predict_fantasy_points()-style predictions can be
# generated for any new week by passing new feature data through the fitted
# stacks object. This is the forward-looking use case -- what does the model
# predict for the upcoming week?
#
# NFL context: For mid-season use, the feature matrix needs to be rebuilt with
# current-week data as the "next week." The ensemble stack_final object handles
# all preprocessing internally (recipes were fit on training data inside
# stacks::fit_members()), so new data just needs to have the same column names.

cat("\n====================================================\n")
cat("EXAMPLE 6: Forward-looking predictions for a new week\n")
cat("====================================================\n")

# Simulate a new week's feature data (same structure as test data)
# In production use, replace this with actual current-week features
# from compile_feature_matrix() + prepare_model_features()

set.seed(2025L)
new_week_data <- suite_rb$test %>%
  dplyr::slice_sample(n = 10L) %>%
  dplyr::select(-ppr_points_next_week)  # exclude target (not known for future weeks)

cat("Generating ensemble predictions for 10 players (simulated new week)...\n")
new_preds_raw <- predict(ensemble_rb$stack_final, new_data = new_week_data)
new_preds     <- tibble::tibble(
  player_id = suite_rb$test_raw %>%
    dplyr::slice(as.integer(attr(suite_rb$test, "row.names")[1:10])) %>%
    dplyr::pull(player_id),
  predicted_ppr = pmax(new_preds_raw$.pred, 0)
) %>%
  dplyr::arrange(dplyr::desc(predicted_ppr))

cat("\nEnsemble predictions (descending by projected PPR):\n")
print(new_preds)
cat("\nThese predictions combine XGBoost, Random Forest, and Elastic Net\n")
cat("via the ridge meta-learner blend weights shown in Example 4.\n")
cat("For production use, replace new_week_data with current-season features\n")
cat("from compile_feature_matrix() + prepare_model_features().\n")


# =============================================================================
# Best practices summary
# =============================================================================

cat("\n====================================================\n")
cat("BEST PRACTICES: Week 11 Ensemble Pipeline\n")
cat("====================================================\n")
cat("1. ALWAYS use run_full_pipeline() for reproducibility.\n")
cat("   It saves artifacts so you can resume without re-training.\n")
cat("2. Check cv_correlation in suite output before stacking.\n")
cat("   r > 0.90 means stacking adds marginal value; consider simple averaging.\n")
cat("3. Verify the Week 10 postseason fix log in compare_models() output.\n")
cat("   week9_remap_log shows which postseason week codes were dropped.\n")
cat("4. Use select_features() top_features for Week 12 projections.\n")
cat("   20 consensus features typically capture 90%+ of predictive power.\n")
cat("5. The stacks stack_final object handles all preprocessing internally.\n")
cat("   Pass new data with same column names; do not pre-process manually.\n")
cat("6. Ensemble RMSE improvement of 3-8% is typical for NFL fantasy data.\n")
cat("   Larger improvements suggest base models had very different error profiles.\n")
cat("   Smaller improvements are expected when base model predictions are correlated.\n")
