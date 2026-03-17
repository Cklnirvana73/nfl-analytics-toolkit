# ==============================================================================
# 12_boom_bust_model.R
# Week 10: Boom / Bust Fantasy Classification
# NFL Analytics Toolkit
#
# NAVIGATION GUIDE
# Line  65:  define_outcome_tiers()        -- assign boom/bust/average labels
# Line 185:  train_classification_model()  -- XGBoost multiclass per position
# Line 410:  calculate_boom_probability()  -- extract class probabilities
# Line 510:  evaluate_classifier()         -- precision / recall / F1 / ROC
# Line 625:  compare_to_regression()       -- classification vs regression output
#
# DESCRIPTION
# Builds a three-class XGBoost classifier (boom / average / bust) on top of
# the Week 9 feature matrix. Uses the same temporal CV architecture as
# train_fantasy_model() in R/11_xgboost_fantasy.R: manual week-based folds,
# hard train/test cutoff at week 14.
#
# KEY DESIGN DECISIONS (all carried forward from Week 9 post-mortems)
# - collect_metrics() broken on manual_rset: use tidyr::unnest(.metrics)
# - stop_iter removed from boost_tree(): incompatible with manual CV folds
# - extract_preprocessor() not extract_recipe() for fold retraining
# - SMOTE via step_smote() inside recipe only, never before CV folding
# - mtry_prop(counts = FALSE) required to avoid silent proportion/count mismatch
# - Structural NAs (epa_roll3 weeks 1-3) passed through: XGBoost handles natively
# - Tier thresholds computed from training rows ONLY, fixed, applied to test
# - position_group values: "passer", "rusher", "receiver" (NOT "QB", "RB", "WR")
#
# NFL CONTEXT
# Boom/bust classification addresses a real fantasy decision problem: point
# projections capture expected outcome, but fantasy is won on variance. A
# 15-point projection with 40% boom probability differs fundamentally from a
# 15-point projection with 5% boom probability. Standard regression cannot
# distinguish these cases.
#
# DEPENDENCIES
# library(tidymodels), library(xgboost), library(themis), library(yardstick)
# library(dplyr), library(tidyr), library(purrr), library(tibble), library(here)
# Upstream: R/11_xgboost_fantasy.R must be sourced for prepare_model_features()
# ==============================================================================

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(tidymodels)
library(xgboost)
library(themis)
library(yardstick)


# ==============================================================================
# define_outcome_tiers()
#
# Assigns boom / average / bust labels to every player-week row.
# Thresholds are derived from the TRAINING window only and then applied
# to all rows including test. Never recompute thresholds on test data.
#
# @param ml_data      tibble from prepare_model_features(). Required columns:
#                     player_id, season, week, position_group,
#                     ppr_points_this_week, has_target (lgl), is_absence_week (lgl)
# @param train_weeks  integer vector. Weeks used to compute thresholds.
#                     Default 1:14 mirrors the Week 9 train/test cutoff.
# @param bust_pct     numeric (0,1). Quantile below which = bust. Default 0.25.
# @param boom_pct     numeric (0,1). Quantile above which = boom. Default 0.75.
#
# @return ml_data with four added columns:
#   outcome_tier (chr):                "boom" | "average" | "bust" | NA
#   bust_threshold (dbl):              position-specific bust ceiling
#   boom_threshold (dbl):              position-specific boom floor
#   tier_computed_from_training (lgl): always TRUE, documents threshold origin
#
# @details
# Strict inequalities at both boundaries prevent double-classification at
# exact quantile boundary rows:
#   bust:    ppr_points_this_week <  bust_threshold
#   boom:    ppr_points_this_week >  boom_threshold
#   average: all remaining rows (including exact boundary values)
#
# Absence weeks and rows with NA ppr_points_this_week receive NA tier.
# Model training uses only rows where has_target == TRUE and
# is_absence_week == FALSE.
#
# PPR context (approximate 2025 values, data-driven thresholds override):
#   Passer boom:   > ~22 PPR pts | bust: < ~8 PPR pts
#   Rusher boom:   > ~14 PPR pts | bust: < ~4 PPR pts
#   Receiver boom: > ~14 PPR pts | bust: < ~4 PPR pts
#
# @seealso train_classification_model(), calculate_boom_probability()
# ==============================================================================

define_outcome_tiers <- function(ml_data,
                                  train_weeks = 1:14,
                                  bust_pct    = 0.25,
                                  boom_pct    = 0.75) {

  required_cols <- c("player_id", "season", "week", "position_group",
                     "ppr_points_this_week", "has_target", "is_absence_week")
  missing <- setdiff(required_cols, names(ml_data))
  if (length(missing) > 0) {
    stop("define_outcome_tiers: missing required columns: ",
         paste(missing, collapse = ", "))
  }
  if (!is.numeric(bust_pct) || bust_pct <= 0 || bust_pct >= 1) {
    stop("define_outcome_tiers: bust_pct must be numeric in (0, 1)")
  }
  if (!is.numeric(boom_pct) || boom_pct <= 0 || boom_pct >= 1) {
    stop("define_outcome_tiers: boom_pct must be numeric in (0, 1)")
  }
  if (bust_pct >= boom_pct) {
    stop("define_outcome_tiers: bust_pct must be less than boom_pct")
  }

  message("define_outcome_tiers: computing thresholds from weeks ",
          min(train_weeks), "-", max(train_weeks), " only...")

  thresholds <- ml_data %>%
    filter(
      week %in% train_weeks,
      has_target      == TRUE,
      !is_absence_week,
      !is.na(ppr_points_this_week)
    ) %>%
    group_by(position_group) %>%
    summarise(
      bust_threshold = quantile(ppr_points_this_week, bust_pct, na.rm = TRUE),
      boom_threshold = quantile(ppr_points_this_week, boom_pct, na.rm = TRUE),
      n_train        = n(),
      .groups        = "drop"
    )

  message("define_outcome_tiers: thresholds by position:")
  for (i in seq_len(nrow(thresholds))) {
    message(sprintf("  %-10s  bust < %.2f  |  boom > %.2f  (n_train = %d)",
                    thresholds$position_group[i],
                    thresholds$bust_threshold[i],
                    thresholds$boom_threshold[i],
                    thresholds$n_train[i]))
  }

  result <- ml_data %>%
    left_join(
      thresholds %>% select(position_group, bust_threshold, boom_threshold),
      by = "position_group"
    ) %>%
    mutate(
      outcome_tier = case_when(
        is_absence_week                        ~ NA_character_,
        is.na(ppr_points_this_week)            ~ NA_character_,
        ppr_points_this_week > boom_threshold  ~ "boom",
        ppr_points_this_week < bust_threshold  ~ "bust",
        TRUE                                   ~ "average"
      ),
      tier_computed_from_training = TRUE
    )

  tier_summary <- result %>%
    filter(!is.na(outcome_tier)) %>%
    count(position_group, outcome_tier) %>%
    group_by(position_group) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup()

  message("define_outcome_tiers: tier distribution:")
  print(tier_summary)

  result
}


# ==============================================================================
# train_classification_model()
#
# Trains a position-specific XGBoost multiclass classifier (boom/average/bust).
# Mirrors train_fantasy_model() architecture from R/11_xgboost_fantasy.R.
#
# @param ml_data_tiered  tibble from define_outcome_tiers(). Must contain
#                        outcome_tier column plus all feature columns.
# @param position        chr. One of: "passer", "rusher", "receiver".
# @param train_week_max  int. Last week in training window. Default: 14.
# @param cv_week_min     int. First week eligible as CV validation fold.
#                        Default: 5. Weeks 1-4 excluded: structural NAs
#                        dominate early rolling features.
# @param seed            int. Random seed. Default: 42.
#
# @return named list:
#   fitted_workflow    (workflow): final model fitted on all training data
#   cv_metrics         (tibble):  per-fold CV metrics (mlogloss, accuracy)
#   cv_logloss         (dbl):     mean CV log-loss across folds
#   test_predictions   (tibble):  test set rows with truth and probabilities
#   test_logloss       (dbl):     held-out log-loss on test weeks
#   tier_thresholds    (tibble):  boom/bust thresholds used
#   fold_importances   (tibble):  per-fold variable importance
#   position           (chr):     position group label
#   train_n            (int):     training row count
#   test_n             (int):     test row count
#
# @details
# SMOTE is applied via step_smote() inside the recipe so oversampling only
# affects training data within each CV fold. Applying SMOTE before folding
# contaminates validation folds with synthetic minority samples.
#
# Objective: multi:softprob returns per-class probabilities. Tuning metric
# is mlogloss (proper scoring rule for multiclass).
#
# @seealso define_outcome_tiers(), calculate_boom_probability(), evaluate_classifier()
# ==============================================================================

train_classification_model <- function(ml_data_tiered,
                                        position,
                                        train_week_max = 14L,
                                        cv_week_min    = 5L,
                                        seed           = 42L) {

  valid_positions <- c("passer", "rusher", "receiver")
  if (!position %in% valid_positions) {
    stop("train_classification_model: position must be one of: ",
         paste(valid_positions, collapse = ", "))
  }
  if (!"outcome_tier" %in% names(ml_data_tiered)) {
    stop("train_classification_model: outcome_tier column not found. ",
         "Run define_outcome_tiers() first.")
  }

  message("\n", strrep("-", 60))
  message("train_classification_model: ", position)
  message(strrep("-", 60))

  id_cols <- intersect(
    c("player_id", "player_name", "season", "team", "opponent",
      "position_group", "is_absence_week", "has_target",
      "ppr_points_this_week", "ppr_points_next_week",
      "bust_threshold", "boom_threshold", "tier_computed_from_training"),
    names(ml_data_tiered)
  )

  pos_data <- ml_data_tiered %>%
    filter(
      position_group == position,
      !is.na(outcome_tier),
      has_target       == TRUE,
      !is_absence_week
    ) %>%
    mutate(
      outcome_tier = factor(outcome_tier, levels = c("boom", "average", "bust"))
    )

  if (nrow(pos_data) == 0) {
    stop("train_classification_model: no eligible rows for position = ", position)
  }

  train_data <- pos_data %>% filter(week <= train_week_max)
  test_data  <- pos_data %>% filter(week  > train_week_max, week <= 18L)

  message("  Train rows: ", nrow(train_data),
          " | Test rows: ", nrow(test_data))
  message("  Class distribution (train):")
  print(table(train_data$outcome_tier))

  if (nrow(train_data) < 50) {
    stop("train_classification_model: insufficient training rows (",
         nrow(train_data), ") for position = ", position)
  }
  if (nrow(test_data) < 10) {
    warning("train_classification_model: only ", nrow(test_data),
            " test rows -- evaluation metrics will be unreliable")
  }

  cv_weeks  <- seq(cv_week_min, train_week_max)
  fold_list <- compact(map(cv_weeks, function(val_week) {
    train_idx <- which(train_data$week < val_week)
    val_idx   <- which(train_data$week == val_week)
    if (length(train_idx) == 0 || length(val_idx) == 0) return(NULL)
    make_splits(
      list(analysis = train_idx, assessment = val_idx),
      data = train_data
    )
  }))

  if (length(fold_list) == 0) {
    stop("train_classification_model: no valid CV folds produced. ",
         "Check cv_week_min (", cv_week_min, ") vs train data weeks.")
  }

  cv_folds <- manual_rset(
    fold_list,
    ids = paste0("week_", cv_weeks[seq_along(fold_list)])
  )
  message("  CV folds: ", length(fold_list))

  set.seed(seed)
  cls_recipe <- recipe(
    outcome_tier ~ .,
    data = train_data %>% select(-all_of(id_cols))
  ) %>%
    step_rm(any_of("week")) %>%
    step_zv(all_predictors()) %>%
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_smote(outcome_tier, over_ratio = 0.5, seed = seed)

  xgb_spec <- boost_tree(
    trees          = tune(),
    tree_depth     = tune(),
    learn_rate     = tune(),
    min_n          = tune(),
    loss_reduction = tune(),
    sample_size    = tune(),
    mtry           = tune()
  ) %>%
    set_engine(
      "xgboost",
      objective   = "multi:softprob",
      eval_metric = "mlogloss",
      counts      = FALSE,
      nthread     = max(1L, parallel::detectCores() - 1L),
      verbose     = 0
    ) %>%
    set_mode("classification")

  cls_workflow <- workflow() %>%
    add_recipe(cls_recipe) %>%
    add_model(xgb_spec)

  set.seed(seed)
  tune_grid <- grid_latin_hypercube(
    trees(range          = c(100L, 800L)),
    tree_depth(range     = c(3L, 7L)),
    learn_rate(range     = c(-2.5, -0.7), trans = scales::log10_trans()),
    min_n(range          = c(5L, 30L)),
    loss_reduction(range = c(0, 3), trans = scales::log10_trans()),
    sample_size          = sample_prop(range = c(0.5, 1.0)),
    mtry                 = mtry_prop(range = c(0.3, 0.9)),
    size = 25
  )

  message("  Tuning ", nrow(tune_grid), " combinations across ",
          length(fold_list), " folds...")

  tune_results <- tune_grid(
    cls_workflow,
    resamples = cv_folds,
    grid      = tune_grid,
    metrics   = metric_set(mn_log_loss, accuracy),
    control   = control_grid(save_pred = TRUE, verbose = FALSE, allow_par = FALSE)
  )

  cv_metrics_raw <- tidyr::unnest(tune_results, .metrics)

  best_config <- cv_metrics_raw %>%
    filter(.metric == "mn_log_loss") %>%
    group_by(across(starts_with(".config")), trees, tree_depth, learn_rate,
             min_n, loss_reduction, sample_size, mtry) %>%
    summarise(mean_logloss = mean(.estimate, na.rm = TRUE), .groups = "drop") %>%
    slice_min(mean_logloss, n = 1, with_ties = FALSE)

  cv_logloss  <- round(best_config$mean_logloss, 5)
  best_params <- best_config %>%
    select(trees, tree_depth, learn_rate, min_n, loss_reduction, sample_size, mtry)

  message("  Best CV log-loss: ", cv_logloss)

  message("  Extracting per-fold feature importance...")
  fold_importances <- map_dfr(seq_along(fold_list), function(i) {
    fold_train <- analysis(fold_list[[i]])
    fold_fit   <- fit(finalize_workflow(cls_workflow, best_params), data = fold_train)
    tryCatch({
      xgboost::xgb.importance(model = extract_fit_engine(fold_fit)) %>%
        as_tibble() %>%
        select(variable = Feature, importance = Gain) %>%
        mutate(fold = paste0("week_", cv_weeks[i]))
    }, error = function(e) {
      tibble(variable = character(0), importance = numeric(0), fold = character(0))
    })
  })

  message("  Fitting final model on all training data...")
  final_fit <- fit(
    finalize_workflow(cls_workflow, best_params),
    data = train_data %>% select(-all_of(id_cols))
  )

  message("  Generating test predictions...")
  test_pred_class <- predict(final_fit,
                             new_data = test_data %>% select(-all_of(id_cols)),
                             type = "class")
  test_pred_prob  <- predict(final_fit,
                             new_data = test_data %>% select(-all_of(id_cols)),
                             type = "prob")

  test_predictions <- bind_cols(
    test_data %>% select(player_id, player_name, season, week, outcome_tier),
    test_pred_class,
    test_pred_prob
  ) %>%
    rename(p_boom = .pred_boom, p_average = .pred_average, p_bust = .pred_bust)

  test_logloss <- tryCatch({
    test_predictions %>%
      mutate(outcome_tier = factor(outcome_tier,
                                   levels = c("boom", "average", "bust"))) %>%
      mn_log_loss(truth = outcome_tier, p_boom, p_average, p_bust) %>%
      pull(.estimate) %>%
      round(5)
  }, error = function(e) {
    warning("train_classification_model: could not compute test log-loss: ",
            e$message)
    NA_real_
  })

  message("  Test log-loss: ", test_logloss)
  message("  Test confusion matrix:")
  print(table(Predicted = test_predictions$.pred_class,
              Actual    = test_predictions$outcome_tier))

  tier_thresholds <- ml_data_tiered %>%
    filter(position_group == position) %>%
    select(position_group, bust_threshold, boom_threshold) %>%
    distinct() %>%
    slice(1)

  list(
    fitted_workflow  = final_fit,
    cv_metrics       = cv_metrics_raw,
    cv_logloss       = cv_logloss,
    test_predictions = test_predictions,
    test_logloss     = test_logloss,
    tier_thresholds  = tier_thresholds,
    fold_importances = fold_importances,
    position         = position,
    train_n          = nrow(train_data),
    test_n           = nrow(test_data)
  )
}


# ==============================================================================
# calculate_boom_probability()
#
# Extracts boom / average / bust probabilities for new player-week data.
# Optionally applies Platt scaling if a calibration model is provided.
#
# @param model_result       list from train_classification_model()
# @param new_data           tibble of player-week features (same columns as
#                           training data, without outcome_tier)
# @param calibrate          lgl. Apply Platt scaling if TRUE. Default FALSE.
# @param calibration_model  list with boom_model, average_model, bust_model
#                           (glm objects). Required when calibrate = TRUE.
#
# @return tibble:
#   player_id, player_name, season, week (if present in new_data)
#   p_boom (dbl), p_average (dbl), p_bust (dbl)
#   is_calibrated (lgl)
#
# @details
# Raw XGBoost multiclass probabilities are produced by softmax applied to tree
# leaf scores. A predicted p_boom of 0.30 does NOT mean 30% of players with
# those features will have a boom week. Calibration corrects for this.
# The ECE check in tests/assumptions_week10.R quantifies miscalibration magnitude.
#
# PPR context: compare p_boom against the base rate (~25% of weeks are boom
# by construction with p75 threshold). A player with p_boom = 0.50 is roughly
# 2x more likely to boom than the average player in the pool.
#
# @seealso train_classification_model(), evaluate_classifier()
# ==============================================================================

calculate_boom_probability <- function(model_result,
                                        new_data,
                                        calibrate         = FALSE,
                                        calibration_model = NULL) {

  if (is.null(model_result$fitted_workflow)) {
    stop("calculate_boom_probability: fitted_workflow not found. ",
         "Run train_classification_model() first.")
  }
  if (calibrate && is.null(calibration_model)) {
    warning("calculate_boom_probability: calibrate = TRUE but calibration_model ",
            "is NULL. Returning raw XGBoost probabilities. These are NOT true ",
            "probabilities -- use for ranking only, not absolute thresholds.")
  }

  id_cols_present <- intersect(
    c("player_id", "player_name", "season", "week", "team", "position_group"),
    names(new_data)
  )
  id_data   <- new_data %>% select(all_of(id_cols_present))
  raw_probs <- predict(model_result$fitted_workflow,
                       new_data = new_data, type = "prob")

  expected <- c(".pred_boom", ".pred_average", ".pred_bust")
  missing  <- setdiff(expected, names(raw_probs))
  if (length(missing) > 0) {
    stop("calculate_boom_probability: missing probability columns from predict(): ",
         paste(missing, collapse = ", "))
  }

  result <- bind_cols(id_data, raw_probs) %>%
    rename(p_boom = .pred_boom, p_average = .pred_average, p_bust = .pred_bust) %>%
    mutate(is_calibrated = FALSE)

  if (calibrate && !is.null(calibration_model)) {
    result <- result %>%
      mutate(
        p_boom    = predict(calibration_model$boom_model,
                            newdata = data.frame(raw_prob = p_boom),
                            type = "response"),
        p_average = predict(calibration_model$average_model,
                            newdata = data.frame(raw_prob = p_average),
                            type = "response"),
        p_bust    = predict(calibration_model$bust_model,
                            newdata = data.frame(raw_prob = p_bust),
                            type = "response"),
        is_calibrated = TRUE,
        prob_sum  = p_boom + p_average + p_bust,
        p_boom    = round(p_boom    / prob_sum, 4),
        p_average = round(p_average / prob_sum, 4),
        p_bust    = round(p_bust    / prob_sum, 4)
      ) %>%
      select(-prob_sum)
  } else {
    result <- result %>%
      mutate(across(c(p_boom, p_average, p_bust), ~ round(.x, 4)))
  }

  result
}


# ==============================================================================
# evaluate_classifier()
#
# Full evaluation report: confusion matrix, per-class precision/recall/F1,
# ROC AUC, and comparison against the majority-class naive baseline.
#
# @param model_result  list from train_classification_model()
# @param verbose       lgl. Print to console. Default TRUE.
#
# @return named list:
#   confusion_matrix  (tbl):  confusion matrix counts
#   class_metrics     (tbl):  precision, recall, F1
#   overall_metrics   (tbl):  accuracy, macro F1, log-loss, boom recall
#   roc_auc           (tbl):  one-vs-rest ROC AUC
#   baseline_accuracy (dbl):  majority-class naive accuracy
#   boom_recall       (dbl):  fraction of actual boom weeks correctly identified
#   boom_precision    (dbl):  fraction of predicted boom weeks that were boom
#
# @details
# Primary metric is boom recall. A model with boom recall = 0 is functionally
# identical to predicting "average" for every player. Accuracy alone is
# misleading: the average class is ~50% of rows so a naive model achieves
# ~50% accuracy with zero boom recall.
#
# @seealso train_classification_model(), compare_to_regression()
# ==============================================================================

evaluate_classifier <- function(model_result, verbose = TRUE) {

  preds <- model_result$test_predictions
  if (is.null(preds)) {
    stop("evaluate_classifier: test_predictions not found in model_result.")
  }

  required <- c("outcome_tier", ".pred_class", "p_boom", "p_average", "p_bust")
  missing  <- setdiff(required, names(preds))
  if (length(missing) > 0) {
    stop("evaluate_classifier: missing columns in test_predictions: ",
         paste(missing, collapse = ", "))
  }

  tier_levels <- c("boom", "average", "bust")
  preds_f <- preds %>%
    mutate(
      outcome_tier = factor(outcome_tier, levels = tier_levels),
      .pred_class  = factor(.pred_class,  levels = tier_levels)
    )

  cm <- conf_mat(preds_f, truth = outcome_tier, estimate = .pred_class)

  class_metrics <- bind_rows(
    precision(preds_f, truth = outcome_tier, estimate = .pred_class,
              estimator = "macro_weighted"),
    recall(preds_f,    truth = outcome_tier, estimate = .pred_class,
           estimator = "macro_weighted"),
    f_meas(preds_f,    truth = outcome_tier, estimate = .pred_class,
           estimator = "macro_weighted"),
    accuracy(preds_f,  truth = outcome_tier, estimate = .pred_class)
  ) %>%
    mutate(.estimate = round(.estimate, 4))

  boom_rows      <- preds_f %>% filter(outcome_tier == "boom")
  predicted_boom <- preds_f %>% filter(.pred_class  == "boom")

  boom_recall <- if (nrow(boom_rows) > 0) {
    round(mean(boom_rows$.pred_class == "boom"), 4)
  } else {
    NA_real_
  }
  boom_precision <- if (nrow(predicted_boom) > 0) {
    round(mean(predicted_boom$outcome_tier == "boom"), 4)
  } else {
    0
  }

  baseline_accuracy <- round(mean(preds_f$outcome_tier == "average"), 4)
  model_accuracy    <- round(mean(preds_f$outcome_tier == preds_f$.pred_class), 4)

  roc_auc_result <- tryCatch(
    roc_auc(preds_f, truth = outcome_tier, p_boom, p_average, p_bust,
            estimator = "macro_weighted") %>%
      mutate(.estimate = round(.estimate, 4)),
    error = function(e) {
      tibble(.metric = "roc_auc", .estimator = "macro_weighted", .estimate = NA_real_)
    }
  )

  overall_metrics <- tibble(
    metric = c("accuracy", "baseline_accuracy", "macro_weighted_f1",
               "cv_logloss", "test_logloss", "boom_recall", "boom_precision"),
    value  = c(model_accuracy, baseline_accuracy,
               class_metrics %>% filter(.metric == "f_meas") %>% pull(.estimate),
               model_result$cv_logloss, model_result$test_logloss,
               boom_recall, boom_precision)
  ) %>%
    mutate(value = round(value, 4))

  if (verbose) {
    cat("\n", strrep("=", 60), "\n")
    cat("Classifier Evaluation -- position:", model_result$position, "\n")
    cat(strrep("=", 60), "\n\n")
    cat("Confusion Matrix:\n")
    print(cm$table)
    cat("\nOverall Metrics:\n")
    print(overall_metrics)
    cat("\nClass Metrics:\n")
    print(class_metrics %>% select(.metric, .estimator, .estimate))
    cat("\nROC AUC:\n")
    print(roc_auc_result)
    cat(sprintf("\nModel beats naive baseline: %s  (%.1f%% vs %.1f%%)\n",
                toupper(model_accuracy > baseline_accuracy),
                100 * model_accuracy, 100 * baseline_accuracy))
    if (!is.na(boom_recall) && boom_recall == 0) {
      cat("\n[FAIL] Boom recall = 0 -- model never predicts boom.\n")
    }
  }

  list(
    confusion_matrix  = cm$table,
    class_metrics     = class_metrics,
    overall_metrics   = overall_metrics,
    roc_auc           = roc_auc_result,
    baseline_accuracy = baseline_accuracy,
    boom_recall       = boom_recall,
    boom_precision    = boom_precision
  )
}


# ==============================================================================
# compare_to_regression()
#
# Side-by-side comparison of Week 9 regression output (point projections) and
# Week 10 classification output (boom/bust probabilities) on the same test rows.
# Central question: does boom/bust probability add decision value beyond the
# point estimate alone?
#
# @param model_result      list from train_classification_model()
# @param regression_preds  tibble of Week 9 predictions. Required columns:
#                          player_id, predicted_week, position_group,
#                          predicted_ppr.
# @param ml_data_tiered    tibble from define_outcome_tiers(). Used to join
#                          actual ppr_points_this_week on player_id + week.
# @param position          chr. "passer", "rusher", or "receiver"
#
# @return tibble with one row per matched player-week:
#   player_id, player_name, week, outcome_tier (actual),
#   regression_pred, actual_ppr, .pred_class, p_boom, p_average, p_bust,
#   regression_error (abs error), tier_correct (lgl), boom_detected (lgl)
#
# @details
# regression_preds uses predicted_ppr as the prediction column and
# predicted_week as the target week. Actual PPR values are joined from
# ml_data_tiered$ppr_points_this_week on player_id + week = predicted_week.
#
# A player ranked 5th by regression with p_boom = 0.45 may be a better start
# than the player ranked 3rd with p_boom = 0.12. Regression captures expected
# value; classification captures upside risk. Both together are more useful
# than either alone.
#
# @seealso evaluate_classifier(), calculate_boom_probability()
# ==============================================================================

compare_to_regression <- function(model_result, regression_preds,
                                   ml_data_tiered, position) {

  cls_preds <- model_result$test_predictions
  if (is.null(cls_preds)) {
    stop("compare_to_regression: test_predictions not found in model_result.")
  }

  required_reg <- c("player_id", "predicted_week", "position_group", "predicted_ppr")
  missing_reg  <- setdiff(required_reg, names(regression_preds))
  if (length(missing_reg) > 0) {
    stop("compare_to_regression: missing columns in regression_preds: ",
         paste(missing_reg, collapse = ", "))
  }

  # Actual PPR values live in ml_data_tiered as ppr_points_this_week
  actuals <- ml_data_tiered %>%
    filter(position_group == position) %>%
    select(player_id, week, actual_ppr = ppr_points_this_week)

  reg_pos <- regression_preds %>%
    filter(position_group == position) %>%
    select(player_id,
           week            = predicted_week,
           regression_pred = predicted_ppr) %>%
    inner_join(actuals, by = c("player_id", "week")) %>%
    filter(!is.na(actual_ppr))

  if (nrow(reg_pos) == 0) {
    stop("compare_to_regression: no regression predictions for position = ",
         position, ". Verify position_group values in regression_preds.")
  }

  combined <- cls_preds %>%
    inner_join(reg_pos, by = c("player_id", "week")) %>%
    mutate(
      regression_error = round(abs(regression_pred - actual_ppr), 3),
      tier_correct     = (as.character(.pred_class) == as.character(outcome_tier)),
      boom_detected    = (outcome_tier == "boom" & .pred_class == "boom"),
      .pred_class      = as.character(.pred_class)
    ) %>%
    arrange(week, desc(p_boom))

  if (nrow(combined) == 0) {
    warning("compare_to_regression: no rows matched on player_id + week. ",
            "Check season/week alignment.")
    return(combined)
  }

  boom_rows <- combined %>% filter(outcome_tier == "boom")

  cat("\n", strrep("-", 60), "\n")
  cat("Regression vs Classification -- position:", position, "\n")
  cat(strrep("-", 60), "\n\n")
  cat("Matched rows:                 ", nrow(combined), "\n")
  cat("Actual boom weeks:            ", nrow(boom_rows), "\n")
  cat("Boom weeks detected:          ", sum(combined$boom_detected, na.rm = TRUE), "\n")
  cat("Mean regression MAE:          ",
      round(mean(combined$regression_error, na.rm = TRUE), 3), "\n")
  cat("Tier classification accuracy: ",
      round(100 * mean(combined$tier_correct, na.rm = TRUE), 1), "%\n")

  if (nrow(boom_rows) > 0) {
    cat("\nAmong actual boom weeks:\n")
    cat("  Mean regression prediction:",
        round(mean(boom_rows$regression_pred, na.rm = TRUE), 2), "PPR pts\n")
    cat("  Mean actual PPR:           ",
        round(mean(boom_rows$actual_ppr, na.rm = TRUE), 2), "PPR pts\n")
    cat("  Mean p_boom:               ",
        round(mean(boom_rows$p_boom, na.rm = TRUE), 3), "\n")
    cat("  Boom weeks with p_boom > 0.30:",
        sum(boom_rows$p_boom > 0.30, na.rm = TRUE), "of", nrow(boom_rows), "\n")
  }

  combined
}
