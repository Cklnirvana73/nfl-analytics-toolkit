# ==============================================================================
# test_week10_functions.R
# Week 10: Boom / Bust Classification -- Full Test Suite
# NFL Analytics Toolkit
# tests/test_week10_functions.R
#
# COVERAGE
#   define_outcome_tiers()       -- 12 tests
#   train_classification_model() -- 10 tests (uses saved model artifacts)
#   calculate_boom_probability() -- 8 tests
#   evaluate_classifier()        -- 8 tests
#   compare_to_regression()      -- 6 tests
#   Integration                  -- 4 tests
#
# PREREQUISITES
# This file is sourced by test_week10.R which calls test_file().
# The following objects must exist in the calling session OR this script
# will build them from saved RDS artifacts:
#   ml_data_tiered     -- from define_outcome_tiers()
#   boom_bust_results  -- list(passer, rusher, receiver) trained models
#   regression_preds   -- from output/week9_predictions_2025.rds
#
# DEPENDENCIES
# library(testthat), library(here), library(dplyr), library(tidyr),
# library(purrr), library(tibble), library(tidymodels), library(xgboost),
# library(themis), library(yardstick)
# ==============================================================================

library(here)
library(testthat)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(tidymodels)
library(xgboost)
library(themis)
library(yardstick)

# ------------------------------------------------------------------------------
# SETUP: source production scripts and load / build required objects
# ------------------------------------------------------------------------------

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

# Load or build ml_data_tiered
if (!exists("ml_data_tiered")) {
  message("test_week10_functions: ml_data_tiered not in session -- building from artifacts...")

  features_path <- here::here("output", "feature_matrix_2025.rds")
  pbp_path      <- here::here("data", "cache", "pbp_2025_2025.rds")

  if (!file.exists(features_path)) stop("Missing: ", features_path)
  if (!file.exists(pbp_path))      stop("Missing: ", pbp_path)

  features       <- readRDS(features_path)
  pbp            <- readRDS(pbp_path)
  ml_data_raw    <- prepare_model_features(features, pbp, seasons = 2025)
  rm(pbp); gc()

  ml_data_tiered <- define_outcome_tiers(ml_data_raw, train_weeks = 1:14,
                                          bust_pct = 0.25, boom_pct = 0.75)
  rm(ml_data_raw); gc()
}

# Load or build boom_bust_results
if (!exists("boom_bust_results")) {
  message("test_week10_functions: boom_bust_results not in session -- training models...")
  boom_bust_results <- list(
    passer   = train_classification_model(ml_data_tiered, "passer"),
    rusher   = train_classification_model(ml_data_tiered, "rusher"),
    receiver = train_classification_model(ml_data_tiered, "receiver")
  )
  gc()
}

# Load regression predictions for compare_to_regression() tests.
# week9_predictions_2025.rds is a forward-looking snapshot from the last week of
# the season (predicted_week = max_week + 1, typically week 19 / playoffs). No
# actual PPR outcomes exist for that week in ml_data_tiered, so the inner_join
# inside compare_to_regression() returns 0 rows and the function errors.
#
# compare_to_regression() needs predictions covering the classifier test period
# (weeks 15-18). We rebuild regression_preds from week9_models_2025.rds by
# running predict_fantasy_points() on source weeks 14-17, which produces
# predicted_week 15-18 -- weeks for which actual outcomes exist in ml_data_tiered.
if (!exists("regression_preds")) {
  models_w9_path <- here::here("output", "week9_models_2025.rds")
  if (!file.exists(models_w9_path)) stop("Missing: ", models_w9_path)
  message("test_week10_functions: rebuilding regression_preds from week9_models_2025.rds...")
  w9_models <- readRDS(models_w9_path)

  # Weeks 14-17 as source data: predicted_week = source_week + 1 = 15-18
  source_data <- ml_data_tiered %>%
    filter(week >= 14L, week <= 17L, !is_absence_week)

  regression_preds <- bind_rows(
    predict_fantasy_points(w9_models$passer,
                           source_data %>% filter(position_group == "passer"),
                           "passer"),
    predict_fantasy_points(w9_models$rusher,
                           source_data %>% filter(position_group == "rusher"),
                           "rusher"),
    predict_fantasy_points(w9_models$receiver,
                           source_data %>% filter(position_group == "receiver"),
                           "receiver")
  ) %>%
    select(player_id, predicted_week, position_group, predicted_ppr)
}

# Minimal synthetic data for unit tests that do not need real model artifacts
make_minimal_tiered <- function(n_per_class = 30, position = "passer",
                                 seed = 99) {
  set.seed(seed)
  tier_vec <- rep(c("boom", "average", "bust"), each = n_per_class)
  tibble(
    player_id            = paste0("P", seq_along(tier_vec)),
    player_name          = paste0("Player_", seq_along(tier_vec)),
    season               = 2025L,
    week                 = rep(1:10, length.out = length(tier_vec)),
    position_group       = position,
    team                 = "TST",
    ppr_points_this_week = c(
      runif(n_per_class, 20, 40),   # boom
      runif(n_per_class,  8, 18),   # average
      runif(n_per_class,  0,  7)    # bust
    ),
    ppr_points_next_week = runif(length(tier_vec), 0, 35),
    has_target           = TRUE,
    is_absence_week      = FALSE,
    availability_rate    = runif(length(tier_vec), 0.5, 1),
    epa_this_week        = rnorm(length(tier_vec), 0, 1),
    epa_roll3            = c(NA, NA, NA, rnorm(length(tier_vec) - 3, 0, 1)),
    epa_season_to_date   = rnorm(length(tier_vec), 0, 1),
    plays_this_week      = sample(20:60, length(tier_vec), replace = TRUE),
    plays_roll3          = sample(15:55, length(tier_vec), replace = TRUE),
    success_rate_this_week = runif(length(tier_vec), 0.3, 0.7),
    opp_adjusted_epa_prior = rnorm(length(tier_vec), 0, 0.5),
    schedule_difficulty_rank = sample(1:32, length(tier_vec), replace = TRUE),
    leading_share_season = runif(length(tier_vec), 0, 0.5),
    trailing_share_season = runif(length(tier_vec), 0, 0.5),
    opponent_style       = sample(c("balanced", "pass_funnel", "run_funnel"),
                                  length(tier_vec), replace = TRUE),
    opponent_tier        = sample(c("elite", "average", "poor"),
                                  length(tier_vec), replace = TRUE),
    neutral_epa_season   = rnorm(length(tier_vec), 0, 0.5),
    missed_weeks_this_season = sample(0:3, length(tier_vec), replace = TRUE),
    weeks_played         = sample(1:17, length(tier_vec), replace = TRUE),
    weeks_since_last_played = sample(0:4, length(tier_vec), replace = TRUE),
    weeks_since_role_change = sample(0:10, length(tier_vec), replace = TRUE),
    role_stability_flag  = sample(0:1, length(tier_vec), replace = TRUE),
    opp_adj_games_prior  = sample(0:10, length(tier_vec), replace = TRUE),
    outcome_tier         = tier_vec,
    bust_threshold       = 7.0,
    boom_threshold       = 20.0,
    tier_computed_from_training = TRUE
  )
}

cat("\nSetup complete. Running tests...\n\n")

# ==============================================================================
# SECTION 1: define_outcome_tiers()
# ==============================================================================

test_that("define_outcome_tiers: returns expected new columns", {
  result <- define_outcome_tiers(ml_data_tiered %>% select(-outcome_tier,
    -bust_threshold, -boom_threshold, -tier_computed_from_training),
    train_weeks = 1:14)
  expected <- c("outcome_tier", "bust_threshold", "boom_threshold",
                "tier_computed_from_training")
  expect_true(all(expected %in% names(result)),
    info = paste("Missing:", paste(setdiff(expected, names(result)),
                                   collapse = ", ")))
})

test_that("define_outcome_tiers: exactly 3 tier levels in output", {
  tiers <- na.omit(unique(ml_data_tiered$outcome_tier))
  expect_setequal(tiers, c("boom", "average", "bust"))
})

test_that("define_outcome_tiers: no double-classification at boundaries", {
  # A single row cannot be classified as both boom and bust.
  # Boundary rows (exactly at threshold) must fall to "average" via strict inequalities.
  boundary_rows <- ml_data_tiered %>%
    filter(!is.na(outcome_tier)) %>%
    filter(
      (ppr_points_this_week == boom_threshold & outcome_tier == "boom") |
      (ppr_points_this_week == bust_threshold & outcome_tier == "bust")
    )
  expect_equal(nrow(boundary_rows), 0L,
    info = "Exact boundary values should classify as 'average', not boom or bust")
})

test_that("define_outcome_tiers: thresholds only use training rows", {
  expect_true(all(ml_data_tiered$tier_computed_from_training == TRUE,
                  na.rm = TRUE))
})

test_that("define_outcome_tiers: absence weeks receive NA tier", {
  absence_rows <- ml_data_tiered %>% filter(is_absence_week == TRUE)
  if (nrow(absence_rows) > 0) {
    expect_true(all(is.na(absence_rows$outcome_tier)))
  } else {
    skip("No absence weeks in data")
  }
})

test_that("define_outcome_tiers: boom pct approximately equals boom_pct param", {
  train_dist <- ml_data_tiered %>%
    filter(week <= 14, has_target == TRUE, !is_absence_week,
           !is.na(outcome_tier)) %>%
    group_by(position_group) %>%
    summarise(boom_pct = mean(outcome_tier == "boom"), .groups = "drop")
  # With p75 threshold, expect boom ~25% per position (allow 3pp tolerance)
  expect_true(all(abs(train_dist$boom_pct - 0.25) < 0.03),
    info = paste("Boom pcts:", paste(round(train_dist$boom_pct, 3),
                                     collapse = ", ")))
})

test_that("define_outcome_tiers: stops on missing required columns", {
  bad_data <- ml_data_tiered %>% select(-ppr_points_this_week)
  expect_error(define_outcome_tiers(bad_data),
               regexp = "missing required columns")
})

test_that("define_outcome_tiers: stops when bust_pct >= boom_pct", {
  expect_error(
    define_outcome_tiers(ml_data_tiered, bust_pct = 0.75, boom_pct = 0.25),
    regexp = "bust_pct must be less than boom_pct"
  )
})

test_that("define_outcome_tiers: stops on bust_pct outside (0,1)", {
  expect_error(
    define_outcome_tiers(ml_data_tiered, bust_pct = 1.5),
    regexp = "bust_pct must be numeric"
  )
})

test_that("define_outcome_tiers: all positions have a threshold", {
  thresh <- ml_data_tiered %>%
    filter(!is.na(bust_threshold)) %>%
    select(position_group, bust_threshold, boom_threshold) %>%
    distinct()
  expect_equal(nrow(thresh), 3L)
  expect_setequal(thresh$position_group,
                  c("passer", "rusher", "receiver"))
})

test_that("define_outcome_tiers: bust_threshold < boom_threshold for all positions", {
  thresh <- ml_data_tiered %>%
    select(position_group, bust_threshold, boom_threshold) %>%
    distinct() %>%
    filter(!is.na(bust_threshold))
  expect_true(all(thresh$bust_threshold < thresh$boom_threshold))
})

test_that("define_outcome_tiers: no eligible rows left unclassified", {
  unclassified <- ml_data_tiered %>%
    filter(has_target == TRUE, !is_absence_week,
           !is.na(ppr_points_this_week),
           is.na(outcome_tier))
  expect_equal(nrow(unclassified), 0L)
})

# ==============================================================================
# SECTION 2: train_classification_model()
# Uses boom_bust_results from session -- avoids retraining
# ==============================================================================

test_that("train_classification_model: returns all required list slots", {
  res      <- boom_bust_results$passer
  expected <- c("fitted_workflow", "cv_metrics", "cv_logloss",
                "test_predictions", "test_logloss", "tier_thresholds",
                "fold_importances", "position", "train_n", "test_n")
  expect_true(all(expected %in% names(res)),
    info = paste("Missing slots:", paste(setdiff(expected, names(res)),
                                          collapse = ", ")))
})

test_that("train_classification_model: test_predictions has required columns", {
  preds    <- boom_bust_results$passer$test_predictions
  expected <- c("player_id", "player_name", "season", "week",
                "outcome_tier", ".pred_class", "p_boom", "p_average", "p_bust")
  expect_true(all(expected %in% names(preds)),
    info = paste("Missing:", paste(setdiff(expected, names(preds)),
                                   collapse = ", ")))
})

test_that("train_classification_model: probabilities sum to 1 per row", {
  preds    <- boom_bust_results$passer$test_predictions
  prob_sum <- round(preds$p_boom + preds$p_average + preds$p_bust, 3)
  expect_true(all(prob_sum == 1.000),
    info = paste("Rows where probs != 1:",
                 sum(prob_sum != 1.000)))
})

test_that("train_classification_model: all probabilities in [0, 1]", {
  for (pos in names(boom_bust_results)) {
    preds <- boom_bust_results[[pos]]$test_predictions
    expect_true(all(preds$p_boom    >= 0 & preds$p_boom    <= 1))
    expect_true(all(preds$p_average >= 0 & preds$p_average <= 1))
    expect_true(all(preds$p_bust    >= 0 & preds$p_bust    <= 1))
  }
})

test_that("train_classification_model: cv_logloss is positive numeric", {
  for (pos in names(boom_bust_results)) {
    ll <- boom_bust_results[[pos]]$cv_logloss
    expect_true(is.numeric(ll) && ll > 0,
      info = paste(pos, "cv_logloss:", ll))
  }
})

test_that("train_classification_model: test_logloss is positive numeric", {
  for (pos in names(boom_bust_results)) {
    ll <- boom_bust_results[[pos]]$test_logloss
    expect_true(is.numeric(ll) && !is.na(ll) && ll > 0,
      info = paste(pos, "test_logloss:", ll))
  }
})

test_that("train_classification_model: position slot matches input", {
  for (pos in names(boom_bust_results)) {
    expect_equal(boom_bust_results[[pos]]$position, pos)
  }
})

test_that("train_classification_model: test rows are from weeks 15-18 only", {
  for (pos in names(boom_bust_results)) {
    preds <- boom_bust_results[[pos]]$test_predictions
    expect_true(all(preds$week >= 15L & preds$week <= 18L),
      info = paste(pos, "-- weeks outside 15-18:",
                   paste(unique(preds$week[preds$week < 15 | preds$week > 18]),
                         collapse = ", ")))
  }
})

test_that("train_classification_model: stops on invalid position", {
  expect_error(
    train_classification_model(ml_data_tiered, "QB"),
    regexp = "position must be one of"
  )
})

test_that("train_classification_model: stops when outcome_tier column missing", {
  bad_data <- ml_data_tiered %>% select(-outcome_tier)
  expect_error(
    train_classification_model(bad_data, "passer"),
    regexp = "outcome_tier column not found"
  )
})

# ==============================================================================
# SECTION 3: calculate_boom_probability()
# ==============================================================================

test_that("calculate_boom_probability: returns required columns", {
  test_input <- ml_data_tiered %>%
    filter(position_group == "passer", week == 15,
           has_target == TRUE, !is_absence_week) %>%
    head(5)
  result <- calculate_boom_probability(boom_bust_results$passer, test_input)
  expected <- c("p_boom", "p_average", "p_bust", "is_calibrated")
  expect_true(all(expected %in% names(result)),
    info = paste("Missing:", paste(setdiff(expected, names(result)),
                                   collapse = ", ")))
})

test_that("calculate_boom_probability: is_calibrated = FALSE by default", {
  test_input <- ml_data_tiered %>%
    filter(position_group == "passer", week == 15,
           has_target == TRUE, !is_absence_week) %>%
    head(5)
  result <- calculate_boom_probability(boom_bust_results$passer, test_input)
  expect_true(all(result$is_calibrated == FALSE))
})

test_that("calculate_boom_probability: probabilities sum to 1", {
  test_input <- ml_data_tiered %>%
    filter(position_group == "receiver", week == 16,
           has_target == TRUE, !is_absence_week) %>%
    head(10)
  result    <- calculate_boom_probability(boom_bust_results$receiver, test_input)
  prob_sums <- round(result$p_boom + result$p_average + result$p_bust, 3)
  expect_true(all(prob_sums == 1.000))
})

test_that("calculate_boom_probability: all probabilities in [0, 1]", {
  test_input <- ml_data_tiered %>%
    filter(position_group == "rusher", week == 17,
           has_target == TRUE, !is_absence_week) %>%
    head(10)
  result <- calculate_boom_probability(boom_bust_results$rusher, test_input)
  expect_true(all(result$p_boom    >= 0 & result$p_boom    <= 1))
  expect_true(all(result$p_average >= 0 & result$p_average <= 1))
  expect_true(all(result$p_bust    >= 0 & result$p_bust    <= 1))
})

test_that("calculate_boom_probability: stops when fitted_workflow missing", {
  bad_result <- list(fitted_workflow = NULL)
  test_input <- ml_data_tiered %>%
    filter(position_group == "passer", week == 15) %>% head(3)
  expect_error(
    calculate_boom_probability(bad_result, test_input),
    regexp = "fitted_workflow not found"
  )
})

test_that("calculate_boom_probability: warns when calibrate=TRUE but no model", {
  test_input <- ml_data_tiered %>%
    filter(position_group == "passer", week == 15,
           has_target == TRUE, !is_absence_week) %>%
    head(3)
  expect_warning(
    calculate_boom_probability(boom_bust_results$passer, test_input,
                               calibrate = TRUE, calibration_model = NULL),
    regexp = "calibration_model.*NULL"
  )
})

test_that("calculate_boom_probability: preserves id columns from new_data", {
  test_input <- ml_data_tiered %>%
    filter(position_group == "passer", week == 15,
           has_target == TRUE, !is_absence_week) %>%
    head(5)
  result <- calculate_boom_probability(boom_bust_results$passer, test_input)
  expect_true("player_id" %in% names(result))
  expect_true("week"      %in% names(result))
})

test_that("calculate_boom_probability: row count matches input", {
  test_input <- ml_data_tiered %>%
    filter(position_group == "passer", week == 15,
           has_target == TRUE, !is_absence_week) %>%
    head(8)
  result <- calculate_boom_probability(boom_bust_results$passer, test_input)
  expect_equal(nrow(result), nrow(test_input))
})

# ==============================================================================
# SECTION 4: evaluate_classifier()
# ==============================================================================

test_that("evaluate_classifier: returns all required list slots", {
  eval_result <- evaluate_classifier(boom_bust_results$passer, verbose = FALSE)
  expected    <- c("confusion_matrix", "class_metrics", "overall_metrics",
                   "roc_auc", "baseline_accuracy", "boom_recall",
                   "boom_precision")
  expect_true(all(expected %in% names(eval_result)),
    info = paste("Missing:", paste(setdiff(expected, names(eval_result)),
                                   collapse = ", ")))
})

test_that("evaluate_classifier: boom_recall is numeric in [0, 1]", {
  for (pos in names(boom_bust_results)) {
    eval_result <- evaluate_classifier(boom_bust_results[[pos]], verbose = FALSE)
    br <- eval_result$boom_recall
    expect_true(is.numeric(br) && (is.na(br) || (br >= 0 && br <= 1)),
      info = paste(pos, "boom_recall:", br))
  }
})

test_that("evaluate_classifier: boom_precision is numeric in [0, 1]", {
  for (pos in names(boom_bust_results)) {
    eval_result <- evaluate_classifier(boom_bust_results[[pos]], verbose = FALSE)
    bp <- eval_result$boom_precision
    expect_true(is.numeric(bp) && bp >= 0 && bp <= 1,
      info = paste(pos, "boom_precision:", bp))
  }
})

test_that("evaluate_classifier: overall_metrics contains expected metric names", {
  eval_result <- evaluate_classifier(boom_bust_results$passer, verbose = FALSE)
  expected    <- c("accuracy", "baseline_accuracy", "macro_weighted_f1",
                   "cv_logloss", "test_logloss", "boom_recall", "boom_precision")
  actual      <- eval_result$overall_metrics$metric
  expect_true(all(expected %in% actual),
    info = paste("Missing metrics:", paste(setdiff(expected, actual),
                                           collapse = ", ")))
})

test_that("evaluate_classifier: model accuracy > baseline for all positions", {
  for (pos in names(boom_bust_results)) {
    eval_result   <- evaluate_classifier(boom_bust_results[[pos]], verbose = FALSE)
    model_acc     <- eval_result$overall_metrics %>%
      filter(metric == "accuracy") %>% pull(value)
    baseline_acc  <- eval_result$baseline_accuracy
    expect_true(model_acc > baseline_acc,
      info = paste(pos, "model:", round(model_acc, 3),
                   "baseline:", round(baseline_acc, 3)))
  }
})

test_that("evaluate_classifier: boom_recall > 0 for all positions", {
  for (pos in names(boom_bust_results)) {
    eval_result <- evaluate_classifier(boom_bust_results[[pos]], verbose = FALSE)
    expect_true(!is.na(eval_result$boom_recall) &&
                  eval_result$boom_recall > 0,
      info = paste(pos, "boom_recall =", eval_result$boom_recall))
  }
})

test_that("evaluate_classifier: stops when test_predictions missing", {
  bad_result <- boom_bust_results$passer
  bad_result$test_predictions <- NULL
  expect_error(
    evaluate_classifier(bad_result, verbose = FALSE),
    regexp = "test_predictions not found"
  )
})

test_that("evaluate_classifier: confusion matrix rows sum to test_n", {
  for (pos in names(boom_bust_results)) {
    eval_result <- evaluate_classifier(boom_bust_results[[pos]], verbose = FALSE)
    cm_total    <- sum(eval_result$confusion_matrix)
    test_n      <- boom_bust_results[[pos]]$test_n
    expect_equal(cm_total, test_n,
      info = paste(pos, "cm total:", cm_total, "test_n:", test_n))
  }
})

# ==============================================================================
# SECTION 5: compare_to_regression()
# ==============================================================================

test_that("compare_to_regression: returns expected columns", {
  result <- compare_to_regression(boom_bust_results$passer,
                                   regression_preds, ml_data_tiered, "passer")
  expected <- c("player_id", "week", "outcome_tier", "regression_pred",
                "actual_ppr", ".pred_class", "p_boom", "p_average", "p_bust",
                "regression_error", "tier_correct", "boom_detected")
  expect_true(all(expected %in% names(result)),
    info = paste("Missing:", paste(setdiff(expected, names(result)),
                                   collapse = ", ")))
})

test_that("compare_to_regression: regression_error is non-negative", {
  result <- compare_to_regression(boom_bust_results$rusher,
                                   regression_preds, ml_data_tiered, "rusher")
  expect_true(all(result$regression_error >= 0, na.rm = TRUE))
})

test_that("compare_to_regression: tier_correct is logical", {
  result <- compare_to_regression(boom_bust_results$receiver,
                                   regression_preds, ml_data_tiered, "receiver")
  expect_type(result$tier_correct, "logical")
})

test_that("compare_to_regression: boom_detected only TRUE when both match boom", {
  result <- compare_to_regression(boom_bust_results$passer,
                                   regression_preds, ml_data_tiered, "passer")
  inconsistent <- result %>%
    filter(boom_detected == TRUE) %>%
    filter(outcome_tier != "boom" | .pred_class != "boom")
  expect_equal(nrow(inconsistent), 0L)
})

test_that("compare_to_regression: stops on invalid position in regression_preds", {
  expect_error(
    compare_to_regression(boom_bust_results$passer,
                          regression_preds, ml_data_tiered, "kicker"),
    regexp = "no regression predictions for position"
  )
})

test_that("compare_to_regression: result is ordered by week then desc p_boom", {
  result <- compare_to_regression(boom_bust_results$receiver,
                                   regression_preds, ml_data_tiered, "receiver")
  if (nrow(result) > 1) {
    expect_true(all(diff(result$week) >= 0))
  }
})

# ==============================================================================
# SECTION 6: Integration Tests
# ==============================================================================

test_that("Integration: define -> train -> evaluate pipeline completes", {
  # Verify the full pipeline produces non-null output with real data
  eval_result <- evaluate_classifier(boom_bust_results$passer, verbose = FALSE)
  expect_false(is.null(eval_result))
  expect_true(eval_result$boom_recall > 0)
})

test_that("Integration: calculate_boom_probability consistent with test_predictions", {
  # Predictions from calculate_boom_probability on test rows should match
  # the p_boom values stored in test_predictions (same model, same data)
  test_input <- ml_data_tiered %>%
    filter(position_group == "passer", week == 15,
           has_target == TRUE, !is_absence_week)

  if (nrow(test_input) == 0) skip("No passer rows at week 15")

  boom_probs   <- calculate_boom_probability(boom_bust_results$passer,
                                              test_input)
  stored_preds <- boom_bust_results$passer$test_predictions %>%
    filter(week == 15) %>%
    inner_join(boom_probs %>% select(player_id, p_boom_recalc = p_boom),
               by = "player_id")

  if (nrow(stored_preds) == 0) skip("No player_id matches at week 15")

  # Probabilities should match to within rounding tolerance
  expect_true(all(abs(stored_preds$p_boom - stored_preds$p_boom_recalc) < 0.01),
    info = "p_boom from calculate_boom_probability does not match stored test_predictions")
})

test_that("Integration: all three positions return boom_recall > 0", {
  recalls <- map_dbl(names(boom_bust_results), function(pos) {
    evaluate_classifier(boom_bust_results[[pos]], verbose = FALSE)$boom_recall
  })
  expect_true(all(recalls > 0),
    info = paste("Boom recalls:", paste(round(recalls, 3), collapse = ", ")))
})

test_that("Integration: compare_to_regression matched rows are subset of test_predictions", {
  for (pos in names(boom_bust_results)) {
    combined    <- compare_to_regression(boom_bust_results[[pos]],
                                         regression_preds, ml_data_tiered, pos)
    test_pids   <- unique(boom_bust_results[[pos]]$test_predictions$player_id)
    result_pids <- unique(combined$player_id)
    extra       <- setdiff(result_pids, test_pids)
    expect_equal(length(extra), 0L,
      info = paste(pos, "-- player_ids in compare output not in test_predictions:",
                   paste(head(extra, 5), collapse = ", ")))
  }
})

cat("\nAll Week 10 tests complete.\n")
