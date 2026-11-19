# ==============================================================================
# WEEK 9 WALKTHROUGH SCRIPT
# ==============================================================================
# Run this file section by section (Ctrl+Enter line by line or by block).
# Do NOT source the entire file at once -- each section has a checkpoint
# where you should review console output before continuing.
# ==============================================================================

library(here)
library(dplyr)


# ==============================================================================
# STEP 1 & 2: Load data infrastructure and pbp data
# Expected: cache hit on pbp_2025_2025.rds -- should complete in a few seconds.
# ==============================================================================

source(here::here("R", "01_data_loading.R"))

pbp <- load_and_validate_pbp(seasons = 2025)

# CHECKPOINT 1: Verify pbp loaded correctly before continuing.
cat("pbp rows:", nrow(pbp), "\n")
cat("pbp weeks:", paste(sort(unique(pbp$week)), collapse = ", "), "\n")
cat("pbp seasons:", paste(unique(pbp$season), collapse = ", "), "\n")


# ==============================================================================
# STEP 3: Source all function files (no computation yet -- definitions only)
# ==============================================================================

source(here::here("R", "02_player_stats.R"))
source(here::here("R", "03_team_stats.R"))
source(here::here("R", "04_game_analysis.R"))
source(here::here("R", "05_consistency_metrics.R"))
source(here::here("R", "06_predictive_validation.R"))
source(here::here("R", "07_epa_features.R"))
source(here::here("R", "08_usage_features.R"))
source(here::here("R", "09_gamescript_features.R"))
source(here::here("R", "10_opponent_features.R"))

cat("All function files sourced.\n")


# ==============================================================================
# STEP 4: Build the feature matrix (this is the real work -- a few minutes)
# Watch the progress messages in the console as each layer builds.
# ==============================================================================

opp_adj  <- calculate_opponent_adjustments(pbp, season = 2025)
def_sty  <- classify_defensive_style(pbp, season = 2025)
features <- compile_feature_matrix(pbp, opp_adj, def_sty, season = 2025)

# CHECKPOINT 2: Verify feature matrix looks right before saving.
cat("feature matrix rows:   ", nrow(features), "\n")
cat("feature matrix columns:", ncol(features), "\n")
cat("weeks covered:         ", paste(sort(unique(features$week)), collapse = ", "), "\n")
cat("positions:             ", paste(sort(unique(features$position_group)), collapse = ", "), "\n")


# ==============================================================================
# STEP 5: Save feature matrix -- skip Steps 3-4 in all future sessions
# ==============================================================================

saveRDS(features, here::here("output", "feature_matrix_2025.rds"))
cat("Feature matrix saved to output/feature_matrix_2025.rds\n")
cat("Future sessions: features <- readRDS(here::here('output', 'feature_matrix_2025.rds'))\n")


# ==============================================================================
# STEP 6: Source Week 9 and prepare the ML-ready matrix
# This adds absence reconstruction and availability features.
# ==============================================================================

source(here::here("R", "11_xgboost_fantasy.R"))

ml_data <- prepare_model_features(features, pbp, season = 2025)

# CHECKPOINT 3: Review before training -- this is the most important stop.
# Confirm absence reconstruction worked, row counts make sense,
# and all three positions are present before committing to training.
cat("\n--- ML data summary ---\n")
glimpse(ml_data)

cat("\nAbsence weeks reconstructed:\n")
print(ml_data %>% count(is_absence_week))

cat("\nRows with training targets:\n")
print(ml_data %>% count(has_target))

cat("\nRows by position:\n")
print(ml_data %>% count(position_group))

cat("\nAvailability rate distribution:\n")
print(summary(ml_data$availability_rate))


# ==============================================================================
# STEP 7: Train models (10-15 minutes -- only run after Checkpoint 3 looks good)
# tune_grid_size = 30 is production setting.
# Drop to 5-10 if you want a faster first run to verify it works end-to-end.
# ==============================================================================
source(here::here("R", "11_xgboost_fantasy.R"))

models <- train_fantasy_model(
  ml_data,
  positions       = c("passer", "rusher", "receiver"),
  min_train_weeks = 4L,
  tune_grid_size  = 30L,
  seed            = 2025L
)
# CHECKPOINT 4: Check training completed for all three positions.
cat("\nModels trained:\n")
for (pos in names(models)) {
  if (!is.null(models[[pos]])) {
    cat(sprintf("  %s: %d features, %d train rows\n",
                pos,
                models[[pos]]$n_features,
                nrow(models[[pos]]$train_data)))
  } else {
    cat(sprintf("  %s: NULL (insufficient data)\n", pos))
  }
}


# ==============================================================================
# STEP 8: Evaluate each model against baselines
# ==============================================================================

eval_passer   <- evaluate_model(models$passer,   ml_data, "passer")
eval_rusher   <- evaluate_model(models$rusher,   ml_data, "rusher")
eval_receiver <- evaluate_model(models$receiver, ml_data, "receiver")

# CHECKPOINT 5: Does XGBoost beat last-week baseline?
cat("\n--- Final comparison across all positions ---\n")
bind_rows(
  eval_passer$comparison_table,
  eval_rusher$comparison_table,
  eval_receiver$comparison_table
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print()


# ==============================================================================
# STEP 9: Generate predictions for the most recent week
# ==============================================================================

most_recent_week <- max(ml_data$week)
cat(sprintf("\nGenerating predictions: week %d -> week %d\n",
            most_recent_week, most_recent_week + 1))

current_data <- ml_data %>%
  filter(week == most_recent_week, !is_absence_week)

preds_passer   <- predict_fantasy_points(models$passer,   current_data %>% filter(position_group == "passer"),   "passer")
preds_rusher   <- predict_fantasy_points(models$rusher,   current_data %>% filter(position_group == "rusher"),   "rusher")
preds_receiver <- predict_fantasy_points(models$receiver, current_data %>% filter(position_group == "receiver"), "receiver")

all_preds <- bind_rows(preds_passer, preds_rusher, preds_receiver)

cat("\nTop 15 projections (next week):\n")
all_preds %>%
  arrange(desc(predicted_ppr)) %>%
  select(player_name, position_group, predicted_ppr, pi_lower, pi_upper, availability_rate) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  head(15) %>%
  print()


# ==============================================================================
# STEP 10: Save models and predictions for the Docs conversation
# ==============================================================================

saveRDS(models,    here::here("output", "week9_models_2025.rds"))
saveRDS(all_preds, here::here("output", "week9_predictions_2025.rds"))
cat("\nModels and predictions saved to output/\n")
cat("Week 9 Code walkthrough complete.\n")