# ==============================================================================
# example_week10.R
# Week 10: Boom / Bust Classification -- Usage Examples
# NFL Analytics Toolkit
# examples/example_week10.R
#
# EXAMPLES
#   Example 1: Assign boom/bust/average tiers to player-weeks
#   Example 2: Train a position-specific classification model
#   Example 3: Extract boom probabilities for a specific week
#   Example 4: Evaluate classifier performance (recall, precision, F1)
#   Example 5: Identify high-upside vs safe-floor players for a target week
#   Example 6: Tier distribution audit -- verify thresholds and class balance
#   Example 7: Full pipeline -- tiers to predictions in one block
#
# PREREQUISITES
# Requires feature_matrix_2025.rds and pbp_2025_2025.rds in output/ and
# data/cache/ respectively. All objects are built from scratch if not present.
#
# DEPENDENCIES
# library(dplyr), library(tidyr), library(purrr), library(here), library(glue)
# Upstream: all R/01 through R/12 scripts sourced automatically
# ==============================================================================

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)

source(here::here("R", "01_data_loading.R"))
source(here::here("R", "02_player_stats.R"))
source(here::here("R", "03_team_stats.R"))
source(here::here("R", "04_game_analysis.R"))
source(here::here("R", "05_consistency_metrics.R"))
source(here::here("R", "06_predictive_validation.R"))
source(here::here("R", "07_epa_features.R"))
source(here::here("R", "08_usage_features.R"))
source(here::here("R", "09_gamescript_features.R"))
source(here::here("R", "10_opponent_features.R"))
source(here::here("R", "11_xgboost_fantasy.R"))
source(here::here("R", "12_boom_bust_model.R"))

# ------------------------------------------------------------------------------
# Shared setup: load or build ml_data_tiered and boom_bust_results
# ------------------------------------------------------------------------------

if (!exists("ml_data_tiered")) {
  message("Building ml_data_tiered from artifacts...")
  features       <- readRDS(here::here("output", "feature_matrix_2025.rds"))
  pbp            <- readRDS(here::here("data", "cache", "pbp_2025_2025.rds"))
  ml_data_raw    <- prepare_model_features(features, pbp, seasons = 2025)
  rm(pbp); gc()
  ml_data_tiered <- define_outcome_tiers(ml_data_raw, train_weeks = 1:14,
                                          bust_pct = 0.25, boom_pct = 0.75)
  rm(ml_data_raw); gc()
}

if (!exists("boom_bust_results")) {
  message("Training classification models (this takes a few minutes)...")
  boom_bust_results <- list(
    passer   = train_classification_model(ml_data_tiered, "passer"),
    rusher   = train_classification_model(ml_data_tiered, "rusher"),
    receiver = train_classification_model(ml_data_tiered, "receiver")
  )
  gc()
}

# ==============================================================================
# EXAMPLE 1: Assign boom/bust/average tiers to player-weeks
#
# Why this matters: Standard regression outputs a point estimate. Tiers convert
# that continuous distribution into a decision-relevant classification.
# A rusher scoring 12 PPR points is a "boom" in a weak week and "average" in
# a strong one -- thresholds must be derived from actual score distributions,
# not arbitrary round numbers.
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 1: Tier assignment and threshold inspection\n")
cat(strrep("=", 60), "\n\n")

# Inspect position-specific thresholds (derived from training weeks 1-14 only)
thresholds <- ml_data_tiered %>%
  filter(!is.na(outcome_tier), week <= 14) %>%
  group_by(position_group) %>%
  summarise(
    bust_ceiling  = round(first(bust_threshold), 2),
    boom_floor    = round(first(boom_threshold), 2),
    n_train_rows  = n(),
    pct_boom      = round(mean(outcome_tier == "boom",    na.rm = TRUE), 3),
    pct_average   = round(mean(outcome_tier == "average", na.rm = TRUE), 3),
    pct_bust      = round(mean(outcome_tier == "bust",    na.rm = TRUE), 3),
    .groups = "drop"
  )

cat("Position-specific thresholds (derived from weeks 1-14 only):\n\n")
print(thresholds)

cat("\nKey insight: thresholds are NOT symmetric around a fixed value.\n")
cat("Passers need >", thresholds$boom_floor[thresholds$position_group == "passer"],
    "PPR pts to boom; rushers need >",
    thresholds$boom_floor[thresholds$position_group == "rusher"], "pts.\n")
cat("Using a single threshold across positions would misclassify most players.\n")

# ==============================================================================
# EXAMPLE 2: Train a single position model and inspect CV results
#
# Why this matters: The week-based CV folds respect temporal ordering --
# fold 1 trains on week 1, tests on week 2; fold 2 trains on weeks 1-2,
# tests on week 3, etc. This mirrors real-world deployment where you never
# have future data at prediction time.
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 2: Model training and CV fold inspection\n")
cat(strrep("=", 60), "\n\n")

passer_model <- boom_bust_results$passer

cat("Passer model summary:\n")
cat("  Position:         passer\n")
cat("  Train rows:      ", passer_model$train_n, "\n")
cat("  Test rows:       ", passer_model$test_n,  "\n")
cat("  CV log-loss:     ", round(passer_model$cv_logloss,   4), "\n")
cat("  Test log-loss:   ", round(passer_model$test_logloss, 4), "\n")

cat("\nTier thresholds used:\n")
print(passer_model$tier_thresholds)

cat("\nBest hyperparameters:\n")
print(passer_model$cv_metrics %>% head(3))

cat("\nNFL context: mlogloss penalizes confident wrong predictions more than\n")
cat("uncertain wrong ones. A model that says 90% boom on a bust player is\n")
cat("punished far more than one that says 40% boom on the same player.\n")

# ==============================================================================
# EXAMPLE 3: Extract boom probabilities for a specific week
#
# Why this matters: p_boom is the decision variable for fantasy lineup choices.
# A player with a 15-point projection and p_boom = 0.55 is a better tournament
# play than a player with a 16-point projection and p_boom = 0.12.
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 3: Boom probabilities for week 16 receivers\n")
cat(strrep("=", 60), "\n\n")

TARGET_WEEK <- 16

# Pull test predictions for week 16 receivers, ranked by p_boom
week16_receivers <- boom_bust_results$receiver$test_predictions %>%
  filter(week == TARGET_WEEK, !is.na(outcome_tier)) %>%
  arrange(desc(p_boom)) %>%
  select(player_name, week, outcome_tier, .pred_class,
         p_boom, p_average, p_bust) %>%
  mutate(across(c(p_boom, p_average, p_bust), ~ round(.x, 3)))

if (nrow(week16_receivers) == 0) {
  cat("No week 16 receiver test predictions found.\n")
  cat("Test weeks are 15-18. Try TARGET_WEEK <- 15 or 17.\n")
} else {
  cat(glue("Top 10 receivers by boom probability, week {TARGET_WEEK}:\n\n"))
  print(head(week16_receivers, 10))

  # Compare model prediction vs actual outcome for top boom calls
  top_boom_calls <- week16_receivers %>%
    filter(p_boom >= 0.40) %>%
    nrow()

  correct_boom <- week16_receivers %>%
    filter(p_boom >= 0.40, outcome_tier == "boom") %>%
    nrow()

  cat(glue("\nPlayers with p_boom >= 40%: {top_boom_calls}\n"))
  cat(glue("Of those, actual booms:     {correct_boom}",
           " ({round(100*correct_boom/max(top_boom_calls,1), 1)}%)\n"))
  cat("\nKey insight: p_boom is a probability, not a guarantee.\n")
  cat("Use it to tilt toward upside, not as a binary yes/no signal.\n")
}

# ==============================================================================
# EXAMPLE 4: Evaluate classifier -- recall, precision, F1
#
# Why this matters: Accuracy alone is misleading for imbalanced classes.
# The naive baseline (predict 'average' for everyone) achieves ~45-50%
# accuracy with zero boom recall. A useful model must beat naive on boom recall
# -- if it can't identify boom weeks, it adds no fantasy value.
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 4: Classifier evaluation across all three positions\n")
cat(strrep("=", 60), "\n\n")

eval_summary <- map_dfr(names(boom_bust_results), function(pos) {
  ev <- evaluate_classifier(boom_bust_results[[pos]], verbose = FALSE)

  boom_f1 <- if (!is.na(ev$boom_recall) && !is.na(ev$boom_precision) &&
                   (ev$boom_recall + ev$boom_precision) > 0) {
    round(2 * ev$boom_recall * ev$boom_precision /
            (ev$boom_recall + ev$boom_precision), 3)
  } else {
    NA_real_
  }

  tibble(
    position        = pos,
    boom_recall     = round(ev$boom_recall,     3),
    boom_precision  = round(ev$boom_precision,  3),
    boom_f1         = boom_f1,
    overall_acc     = round(ev$class_metrics %>%
                              filter(.metric == "accuracy") %>%
                              pull(.estimate), 3),
    naive_baseline  = round(ev$baseline_accuracy, 3)
  )
})

cat("Performance vs naive baseline (predict 'average' for everyone):\n\n")
print(eval_summary)

cat("\nNaive baseline always has boom_recall = 0.\n")
cat("Any positive boom_recall represents real predictive signal.\n")

best_pos <- eval_summary %>%
  filter(boom_recall == max(boom_recall, na.rm = TRUE)) %>%
  pull(position)
cat(glue("\nStrongest boom detection: {best_pos} position\n"))

# ==============================================================================
# EXAMPLE 5: High-upside vs safe-floor player targeting
#
# Why this matters: Fantasy strategy differs by contest type. Cash games
# (50/50s, head-to-head) reward consistency -- target players with low p_bust.
# Tournament (GPP) play rewards ceiling -- target players with high p_boom.
# This example shows how to split the same player pool for different contexts.
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 5: Cash game vs tournament targeting by position\n")
cat(strrep("=", 60), "\n\n")

TARGET_WEEK_EX5 <- 15

for (pos in names(boom_bust_results)) {
  week_preds <- boom_bust_results[[pos]]$test_predictions %>%
    filter(week == TARGET_WEEK_EX5, !is.na(outcome_tier))

  if (nrow(week_preds) < 5) next

  # Tournament targets: high p_boom (ceiling plays)
  gpp_targets <- week_preds %>%
    arrange(desc(p_boom)) %>%
    slice_head(n = 5) %>%
    select(player_name, p_boom, p_bust, outcome_tier) %>%
    mutate(across(c(p_boom, p_bust), ~ round(.x, 3)))

  # Cash targets: low p_bust (floor plays)
  cash_targets <- week_preds %>%
    arrange(p_bust) %>%
    slice_head(n = 5) %>%
    select(player_name, p_boom, p_bust, outcome_tier) %>%
    mutate(across(c(p_boom, p_bust), ~ round(.x, 3)))

  cat(glue("--- {toupper(pos)} | Week {TARGET_WEEK_EX5} ---\n\n"))
  cat("Tournament targets (highest p_boom):\n")
  print(gpp_targets)
  cat("\nCash game targets (lowest p_bust):\n")
  print(cash_targets)
  cat("\n")
}

cat("Key insight: The same player can appear in both lists (high boom, low bust)\n")
cat("-- that is the ideal profile. Divergence between the two lists reveals\n")
cat("the boom-or-bust vs safe-floor tradeoff for each position.\n")

# ==============================================================================
# EXAMPLE 6: Tier distribution audit across train and test splits
#
# Why this matters: Class imbalance can silently degrade model performance.
# SMOTE inside the recipe rebalances training data, but the test distribution
# must remain natural (unsmoted) to reflect real-world conditions.
# This audit verifies the split is clean.
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 6: Tier distribution audit -- train vs test split\n")
cat(strrep("=", 60), "\n\n")

split_audit <- ml_data_tiered %>%
  filter(!is.na(outcome_tier), has_target, !is_absence_week) %>%
  mutate(split = if_else(week <= 14, "train (wks 1-14)", "test (wks 15-18)")) %>%
  group_by(position_group, split, outcome_tier) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(position_group, split) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  arrange(position_group, split, outcome_tier)

cat("Class distribution by position and split:\n\n")
print(split_audit, n = Inf)

cat("\nWhat to look for:\n")
cat("  - Train and test distributions should be similar (no temporal drift)\n")
cat("  - Boom and bust ~25% each is expected (thresholds set at 25th/75th pct)\n")
cat("  - Large deviations between train and test suggest distribution shift\n")
cat("    which would explain test performance being worse than CV performance\n")

# Check for distribution shift
shift_check <- split_audit %>%
  select(position_group, split, outcome_tier, pct) %>%
  pivot_wider(names_from = split, values_from = pct) %>%
  mutate(
    across(where(is.numeric), ~ replace_na(.x, 0)),
    drift = abs(`train (wks 1-14)` - `test (wks 15-18)`)
  ) %>%
  filter(drift > 5)

if (nrow(shift_check) > 0) {
  cat("\nDistribution shift detected (>5 pct point difference):\n")
  print(shift_check)
} else {
  cat("\nNo significant distribution shift detected (all tiers within 5 pct points).\n")
}

# ==============================================================================
# EXAMPLE 7: Full pipeline -- tiers to predictions in one block
#
# Why this matters: Shows the complete Week 10 workflow as a single reproducible
# block. This is the pattern to use when re-running from scratch at the start
# of a new season.
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("EXAMPLE 7: Full pipeline demonstration\n")
cat(strrep("=", 60), "\n\n")

cat("Step 1: Load feature matrix and play-by-play\n")
cat("  features <- readRDS(here::here('output', 'feature_matrix_2025.rds'))\n")
cat("  pbp      <- readRDS(here::here('data', 'cache', 'pbp_2025_2025.rds'))\n\n")

cat("Step 2: Build model-ready feature matrix\n")
cat("  ml_data_raw <- prepare_model_features(features, pbp, seasons = 2025)\n\n")

cat("Step 3: Assign tiers (thresholds from training weeks only)\n")
cat("  ml_data_tiered <- define_outcome_tiers(\n")
cat("    ml_data_raw,\n")
cat("    train_weeks = 1:14,\n")
cat("    bust_pct    = 0.25,\n")
cat("    boom_pct    = 0.75\n")
cat("  )\n\n")

cat("Step 4: Train classification models per position\n")
cat("  boom_bust_results <- list(\n")
cat("    passer   = train_classification_model(ml_data_tiered, 'passer'),\n")
cat("    rusher   = train_classification_model(ml_data_tiered, 'rusher'),\n")
cat("    receiver = train_classification_model(ml_data_tiered, 'receiver')\n")
cat("  )\n\n")

cat("Step 5: Extract boom probabilities for any week\n")
cat("  boom_probs <- calculate_boom_probability(\n")
cat("    boom_bust_results$receiver,\n")
cat("    new_data = ml_data_tiered %>% filter(week == 16)\n")
cat("  )\n\n")

cat("Step 6: Evaluate on held-out test weeks\n")
cat("  eval_result <- evaluate_classifier(boom_bust_results$passer)\n\n")

# Show actual live summary using existing objects
cat("Live summary using objects already in session:\n\n")
live_summary <- map_dfr(names(boom_bust_results), function(pos) {
  ev    <- evaluate_classifier(boom_bust_results[[pos]], verbose = FALSE)
  preds <- boom_bust_results[[pos]]$test_predictions

  tibble(
    position       = pos,
    train_n        = boom_bust_results[[pos]]$train_n,
    test_n         = boom_bust_results[[pos]]$test_n,
    boom_recall    = round(ev$boom_recall,    3),
    boom_precision = round(ev$boom_precision, 3),
    test_logloss   = round(boom_bust_results[[pos]]$test_logloss, 4)
  )
})
print(live_summary)

cat("\nBest practices summary:\n")
cat("  1. Always derive thresholds from training rows only\n")
cat("  2. Use week-based CV folds -- never random splits on temporal data\n")
cat("  3. Evaluate boom recall first -- accuracy alone is misleading\n")
cat("  4. p_boom is a probability signal, not a binary decision rule\n")
cat("  5. Tournament play: sort by p_boom | Cash play: sort by p_bust ascending\n")

cat("\n", strrep("=", 60), "\n")
cat("Week 10 examples complete\n")
cat(strrep("=", 60), "\n")
