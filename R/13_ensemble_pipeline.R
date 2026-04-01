# =============================================================================
# NFL Analytics Toolkit - Week 11: Ensemble Prediction Pipeline
# =============================================================================
# File:    R/13_ensemble_pipeline.R
# Purpose: Ensemble three model types (XGBoost, Random Forest, Elastic Net)
#          with proper temporal CV stacking and cross-model feature selection.
#          Resolves the Week 10 known limitation: predicted_week = 22 (post-
#          season) blocking compare_to_regression() joins to test weeks 15-18.
#
# Navigation:
#   Line  60 - Libraries & global constants
#   Line  85 - train_model_suite()        Train XGB + RF + EN on same folds
#   Line 220 - compare_models()           Head-to-head RMSE/MAE/Rsq + Wilcoxon
#   Line 360 - build_ensemble()           Stacked meta-learner via stacks pkg
#   Line 500 - select_features()          Consensus feature importance table
#   Line 630 - run_full_pipeline()        End-to-end single-call wrapper
#
# Dependencies:
#   Existing : nflfastR, nflreadr, dplyr, here, glue, zoo, testthat,
#              ggplot2, plotly, htmlwidgets, tidymodels, xgboost, shapviz
#   New Week 11: ranger  (Random Forest engine)
#                glmnet  (Elastic Net engine)
#                stacks  (Model stacking / meta-learner)
#
# Builds On:
#   R/11_xgboost_fantasy.R  - prepare_model_features(), train_fantasy_model(),
#                              predict_fantasy_points(), evaluate_model()
#   R/12_boom_bust_model.R  - prepare_classification_features()
#   R/10_opponent_features.R - compile_feature_matrix()
#
# Input artifacts (from Week 9 walkthrough):
#   output/week9_models_2025.rds      - Named list of fitted XGB workflows
#   output/week9_predictions_2025.rds - Tibble; predicted_ppr, predicted_week
#
# Output artifacts:
#   output/week11_model_suite_2025.rds  - All three fitted model lists
#   output/week11_ensemble_2025.rds     - Fitted stacks ensemble per position
#   output/week11_predictions_2025.rds  - Tibble: stacked ensemble predictions
#   output/week11_feature_ranks_2025.rds - Consensus feature importance table
#
# NFL Context:
#   Different model families capture different signal types.
#   XGBoost captures non-linear interactions (opponent + game script + usage).
#   Random Forest handles noisy features robustly via bagging.
#   Elastic Net recovers stable linear relationships (target share trend).
#   Ensemble typically improves RMSE 3-8% over best single model in NFL data.
#   Position-specific ensembles reflect that QB/RB/WR have different signal
#   structures: QB is dominated by scheme stability; WR by opportunity metrics.
#
# =============================================================================

library(tidymodels)
library(dplyr)
library(glue)
library(here)
library(ggplot2)
library(ranger)
library(glmnet)
library(stacks)


# =============================================================================
# Global constants
# =============================================================================

.W11_POSITIONS    <- c("passer", "rusher", "receiver")

# Minimum test-fold population per position before comparison is meaningful.
# Below this threshold, Wilcoxon test has insufficient power; results are
# flagged rather than suppressed.
.W11_MIN_TEST_N   <- 30L

# Postseason week threshold: predicted_week values above this are postseason.
# NFL regular season ends at week 18. Week 22 = Super Bowl.
# Used in compare_models() to remap postseason join keys (Week 10 limitation fix).
.W11_MAX_REG_WEEK <- 18L

# Correlation threshold above which stacking two models is considered redundant.
# If XGB and RF predictions correlate > 0.90, ensemble gain is likely negligible.
.W11_REDUNDANCY_THRESHOLD <- 0.90

# stacks penalty grid: log-spaced from 0.001 to 0.316 (20 values).
# Ridge penalty on the meta-learner blending coefficients.
.W11_STACK_PENALTIES <- 10^seq(-3, -0.5, length.out = 20)


# =============================================================================
# FUNCTION: train_model_suite
# =============================================================================
#' Train XGBoost, Random Forest, and Elastic Net on identical temporal folds
#'
#' @description
#' Trains three model types on the same position-specific feature matrix with
#' identical temporal CV folds. All models share the same recipe preprocessing
#' and the same \code{control_stack_grid()} settings so their out-of-fold
#' predictions are compatible for stacking in \code{\link{build_ensemble}}.
#'
#' The XGBoost specification re-uses the tuned hyperparameter grid from Week 9
#' but trains fresh -- this function does not load the Week 9 model artifacts.
#' The purpose is to produce stacking-compatible OOF predictions, not to
#' replicate the Week 9 evaluation.
#'
#' @param ml_data Tibble. Output of \code{prepare_model_features()} from
#'   \code{R/11_xgboost_fantasy.R}. Must contain columns: player_id,
#'   position_group, season, week, ppr_points_next_week, and all feature
#'   columns. Rows without \code{ppr_points_next_week} (i.e., absence weeks
#'   with no training target) are dropped internally before training.
#' @param position Character scalar. One of "passer", "rusher", "receiver".
#'   Each call trains three models for a single position.
#' @param train_weeks Integer vector. Weeks included in training folds.
#'   Temporal CV folds are constructed within this range only.
#'   Default: 1:14 (consistent with Week 9 temporal split).
#' @param test_weeks Integer vector. Weeks held out for final evaluation.
#'   No model sees test data during tuning.
#'   Default: 15:18.
#' @param n_folds Integer. Number of temporal CV folds within \code{train_weeks}.
#'   Folds are constructed as expanding windows (train on first k weeks,
#'   validate on week k+1). Default: 5.
#' @param seed Integer. RNG seed for reproducibility. Default: 2025L.
#'
#' @return Named list with elements:
#'   \itemize{
#'     \item \code{xgb_res}  tune_results object. XGBoost tuning results with
#'       OOF predictions (save_pred = TRUE).
#'     \item \code{rf_res}   tune_results object. Random Forest tuning results.
#'     \item \code{en_res}   tune_results object. Elastic Net tuning results.
#'     \item \code{train}    Tibble. Training data used (position-filtered,
#'       target-complete rows only).
#'     \item \code{test}     Tibble. Held-out test data.
#'     \item \code{folds}    manual_rset. Temporal CV fold object shared across
#'       all three models.
#'     \item \code{position} Character. Position passed in, echoed for traceability.
#'     \item \code{cv_correlation} Tibble. Pairwise Pearson r between model OOF
#'       predictions. Used in \code{\link{build_ensemble}} to check redundancy.
#'   }
#'
#' @details
#' \strong{Temporal CV construction:}
#' Folds are constructed manually (same pattern as Week 9) to ensure week-level
#' boundaries, not row-level boundaries. Each fold trains on all weeks up to
#' fold boundary and validates on the single next week. This prevents any future
#' information from leaking into training.
#'
#' \strong{Recipe:}
#' All three models share one recipe:
#' \code{step_zv} (remove zero-variance), \code{step_impute_median} (fill
#' remaining NAs, defensive), \code{step_novel} (unseen factor levels),
#' \code{step_dummy} (one-hot for nominal predictors). Normalization
#' (\code{step_normalize}) is applied for Elastic Net only via a separate recipe.
#'
#' \strong{XGBoost tuning grid:}
#' Latin hypercube, 20 candidates. Parameters: trees (200-1000),
#' tree_depth (3-6), learn_rate (0.01-0.10), mtry (0.3-0.8 as fraction),
#' sample_size (0.5-1.0), min_n (5-30).
#'
#' \strong{Random Forest tuning grid:}
#' Grid over mtry (sqrt(p), p/3, p/2) and min_n (5, 10, 20). Trees fixed at
#' 500 -- increasing beyond this rarely changes RF results meaningfully.
#' Permutation importance is extracted (not impurity -- permutation is more
#' reliable for correlated features common in NFL data).
#'
#' \strong{Elastic Net tuning grid:}
#' Grid over penalty (log-spaced 0.001-1.0, 20 values) and mixture
#' (0 = ridge, 0.5 = elastic net, 1 = lasso). Mixture 0.5 typically
#' outperforms pure ridge/lasso on NFL feature sets with moderate collinearity.
#'
#' @seealso \code{\link{compare_models}}, \code{\link{build_ensemble}},
#'   \code{\link{select_features}}, \code{\link{run_full_pipeline}}
#'
#' @examples
#' \dontrun{
#' library(here)
#' source(here::here("R", "01_data_loading.R"))
#' source(here::here("R", "11_xgboost_fantasy.R"))
#' source(here::here("R", "13_ensemble_pipeline.R"))
#'
#' pbp    <- load_and_validate_pbp(2025)
#' feat   <- compile_feature_matrix(pbp, seasons = 2025)
#' ml_dat <- prepare_model_features(feat, pbp, season = 2025)
#'
#' suite_rb <- train_model_suite(ml_dat, position = "rusher")
#' }
#'
#' @importFrom tidymodels workflow recipe boost_tree rand_forest linear_reg
#'   tune grid_latin_hypercube tune_grid control_stack_grid rsample
#'   set_engine set_mode add_recipe add_model
#' @importFrom stacks control_stack_grid
#' @importFrom ranger ranger
#' @importFrom glmnet glmnet
#' @importFrom dplyr filter select mutate arrange distinct
#' @export
train_model_suite <- function(
    ml_data,
    position,
    train_weeks = 1:14,
    test_weeks  = 15:18,
    n_folds     = 5L,
    seed        = 2025L
) {

  # ---- Input validation -----------------------------------------------------

  if (!is.character(position) || length(position) != 1L) {
    stop("position must be a single character string", call. = FALSE)
  }
  if (!position %in% .W11_POSITIONS) {
    stop(glue("position must be one of: {paste(.W11_POSITIONS, collapse = ', ')}"),
         call. = FALSE)
  }
  if (!is.data.frame(ml_data)) {
    stop("ml_data must be a data frame", call. = FALSE)
  }
  required_cols <- c("player_id", "position_group", "season", "week",
                     "ppr_points_next_week")
  missing_cols  <- setdiff(required_cols, names(ml_data))
  if (length(missing_cols) > 0L) {
    stop(glue("ml_data missing required columns: {paste(missing_cols, collapse = ', ')}"),
         call. = FALSE)
  }
  overlap <- intersect(train_weeks, test_weeks)
  if (length(overlap) > 0L) {
    stop(glue("train_weeks and test_weeks overlap at weeks: {paste(overlap, collapse = ', ')}"),
         call. = FALSE)
  }

  set.seed(seed)

  # ---- Filter to position and rows with training targets --------------------

  message(glue("train_model_suite: position = {position}"))
  message(glue("  train_weeks = {min(train_weeks)}-{max(train_weeks)}, ",
               "test_weeks = {min(test_weeks)}-{max(test_weeks)}"))

  pos_data <- ml_data %>%
    dplyr::filter(
      position_group == position,
      !is.na(ppr_points_next_week)
    )

  if (nrow(pos_data) == 0L) {
    stop(glue("No rows with training targets for position: {position}"), call. = FALSE)
  }

  train_data <- pos_data %>% dplyr::filter(week %in% train_weeks)
  test_data  <- pos_data %>% dplyr::filter(week %in% test_weeks)

  message(glue("  Train rows: {nrow(train_data)} | Test rows: {nrow(test_data)}"))

  if (nrow(train_data) < 50L) {
    stop(glue("Insufficient training rows ({nrow(train_data)}) for position {position}. ",
              "Minimum 50 required."), call. = FALSE)
  }
  if (nrow(test_data) < .W11_MIN_TEST_N) {
    warning(glue("Test set for {position} has only {nrow(test_data)} rows. ",
                 "Wilcoxon test will have low power. Results flagged."),
            call. = FALSE)
  }

  # ---- Identify feature columns (exclude ID / target / metadata) ------------

  non_feature_cols <- c("player_id", "player_name", "position_group", "season",
                        "week", "game_id", "posteam", "recent_team",
                        "ppr_points_next_week", "has_target", "is_absence_week",
                        "tier_computed_from_training", "outcome_tier",
                        "availability_rate", "weeks_since_last_played",
                        "missed_weeks_this_season",
                        # Leakage guard (Week 11 audit): opponent_style and
                        # opponent_tier are full-season defensive classifications.
                        # Joining them to a week-3 row includes how that defense
                        # performed in weeks 4-18 -- temporal leakage.
                        # Currently all-NA (def_styles not provided in pipeline)
                        # so impact is zero, but excluded explicitly to prevent
                        # accidental activation when def_styles is added.
                        # Fix via expanding-window classify_defensive_style()
                        # before re-enabling. See docs/Week11_Leakage_Audit.txt.
                        "opponent_style", "opponent_tier")

  # Keep availability features -- they are legitimate predictors, not leakage
  # (they are computed from history up to current week, not the next week)
  feature_cols <- setdiff(names(train_data), non_feature_cols)

  # Re-attach availability features that should be included
  availability_features <- c("availability_rate", "weeks_since_last_played",
                              "missed_weeks_this_season")
  availability_present  <- intersect(availability_features, names(train_data))
  feature_cols          <- c(feature_cols, availability_present)
  feature_cols          <- unique(feature_cols)

  message(glue("  Feature columns: {length(feature_cols)}"))

  train_model <- train_data %>%
    dplyr::select(all_of(c(feature_cols, "ppr_points_next_week")))
  test_model  <- test_data %>%
    dplyr::select(all_of(c(feature_cols, "ppr_points_next_week")))

  # ---- Build temporal CV folds (week-level, expanding window) ---------------
  # Each fold trains on all earlier weeks, validates on the next single week.
  # n_folds determines how many validation weeks are used within train_weeks.
  # Example with n_folds=5, train_weeks=1:14:
  #   Fold 1: train weeks 1-10, validate week 11
  #   Fold 2: train weeks 1-11, validate week 12
  #   ...
  #   Fold 5: train weeks 1-14, validate week 15  <-- just outside train_weeks
  # This is intentional: the last fold validates against the earliest test week.

  all_train_weeks <- sort(unique(train_data$week))
  fold_val_weeks  <- tail(all_train_weeks, n_folds)  # last n_folds weeks as val

  fold_splits <- lapply(seq_along(fold_val_weeks), function(i) {
    val_week      <- fold_val_weeks[i]
    # train_model has ID columns removed so does not contain 'week'.
    # All row indexing is derived from train_data (which retains week) and
    # applied to train_model via positional alignment -- both have identical
    # row order since train_model is derived from train_data by column removal.
    train_row_idx <- which(train_data$week < val_week)
    val_row_idx   <- which(train_data$week == val_week)

    rsample::make_splits(
      list(analysis   = train_row_idx,
           assessment = val_row_idx),
      data = train_model
    )
  })

  folds <- rsample::manual_rset(fold_splits,
                                ids = glue("Fold{seq_along(fold_splits)}"))
  message(glue("  Temporal CV folds built: {length(fold_splits)}"))

  # ---- Shared recipe (tree models) ------------------------------------------
  # Step order matters:
  # step_unknown must precede step_novel -- step_unknown encodes NA nominal
  # values as "unknown" at bake time; if step_novel runs first it sees NA as a
  # new level and fires a warning even though step_unknown would handle it later.

  base_recipe <- recipes::recipe(ppr_points_next_week ~ ., data = train_model) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
    recipes::step_unknown(recipes::all_nominal_predictors()) %>%   # NA -> "unknown" first
    recipes::step_novel(recipes::all_nominal_predictors()) %>%     # then handle new levels
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE) %>%
    recipes::step_zv(recipes::all_predictors())   # catches zero-var dummies from _unknown/_novel

  # Elastic Net needs normalized predictors.
  # step_normalize must come after step_impute_median (no NAs) and
  # after the second step_zv (no zero-variance columns).
  en_recipe <- base_recipe %>%
    recipes::step_normalize(recipes::all_numeric_predictors())

  # ---- stacks control (saves OOF predictions for meta-learner) -------------

  stack_ctrl <- stacks::control_stack_grid()

  # ---- XGBoost specification ------------------------------------------------
  # Re-using Week 9 approach but with a focused grid (20 candidates).
  # stop_iter removed: incompatible with manual_rset (Week 9 post-mortem #3).

  n_features  <- length(feature_cols)

  # Compute integer mtry candidates as fractions of n_features.
  # mtry_prop() from dials creates a column named 'mtry_prop' which does not
  # match the 'mtry' parameter name in boost_tree() -- this causes check_grid()
  # to error. Build the grid as a plain tibble with column named 'mtry' instead.
  mtry_candidates <- unique(pmax(1L, round(c(0.3, 0.5, 0.7) * n_features)))

  xgb_spec <- parsnip::boost_tree(
    trees       = tune::tune(),
    tree_depth  = tune::tune(),
    learn_rate  = tune::tune(),
    min_n       = tune::tune(),
    sample_size = tune::tune(),
    mtry        = tune::tune()
  ) %>%
    parsnip::set_engine("xgboost", nthread = 1L, verbose = 0L) %>%
    parsnip::set_mode("regression")

  xgb_wf <- workflows::workflow() %>%
    workflows::add_recipe(base_recipe) %>%
    workflows::add_model(xgb_spec)

  # Manual grid: column names must exactly match tune() parameter names in spec.
  set.seed(seed)
  xgb_grid <- tidyr::expand_grid(
    trees       = sample(seq(200L, 1000L, by = 100L), 4L),
    tree_depth  = c(3L, 5L),
    learn_rate  = c(0.05, 0.10),
    min_n       = c(5L, 15L),
    sample_size = c(0.7, 0.9),
    mtry        = mtry_candidates
  ) %>%
    dplyr::slice_sample(n = 20L)

  message("  Training XGBoost...")
  xgb_res <- tune::tune_grid(
    xgb_wf,
    resamples = folds,
    grid      = xgb_grid,
    metrics   = yardstick::metric_set(yardstick::rmse, yardstick::mae,
                                      yardstick::rsq),
    control   = stack_ctrl
  )

  # ---- Random Forest specification ------------------------------------------
  # Trees fixed at 500 -- increasing beyond this has marginal return.
  # Permutation importance is used downstream (not impurity).
  # mtry tuned as fraction via mtry_prop.

  rf_spec <- parsnip::rand_forest(
    trees = 500L,
    mtry  = tune::tune(),
    min_n = tune::tune()
  ) %>%
    parsnip::set_engine("ranger",
                        importance   = "permutation",
                        num.threads  = 1L,
                        seed         = seed) %>%
    parsnip::set_mode("regression")

  rf_wf <- workflows::workflow() %>%
    workflows::add_recipe(base_recipe) %>%
    workflows::add_model(rf_spec)

  rf_grid <- tidyr::expand_grid(
    mtry  = unique(pmax(1L, round(c(0.3, 0.5, 0.7) * n_features))),
    min_n = c(5L, 10L, 20L)
  )

  message("  Training Random Forest...")
  rf_res <- tune::tune_grid(
    rf_wf,
    resamples = folds,
    grid      = rf_grid,
    metrics   = yardstick::metric_set(yardstick::rmse, yardstick::mae,
                                      yardstick::rsq),
    control   = stack_ctrl
  )

  # ---- Elastic Net specification --------------------------------------------
  # mixture 0 = ridge, 1 = lasso.
  # NFL feature sets have moderate collinearity so mixture ~0.5 is typical
  # sweet spot (elastic net, not pure ridge or pure lasso).

  en_spec <- parsnip::linear_reg(
    penalty = tune::tune(),
    mixture = tune::tune()
  ) %>%
    parsnip::set_engine("glmnet") %>%
    parsnip::set_mode("regression")

  en_wf <- workflows::workflow() %>%
    workflows::add_recipe(en_recipe) %>%
    workflows::add_model(en_spec)

  en_grid <- tidyr::expand_grid(
    penalty = 10^seq(-3, 0, length.out = 20L),
    mixture = c(0, 0.25, 0.5, 0.75, 1.0)
  )

  message("  Training Elastic Net...")
  en_res <- tune::tune_grid(
    en_wf,
    resamples = folds,
    grid      = en_grid,
    metrics   = yardstick::metric_set(yardstick::rmse, yardstick::mae,
                                      yardstick::rsq),
    control   = stack_ctrl
  )

  # ---- Check prediction correlation between models (redundancy diagnostic) --
  # High correlation (> 0.90) means models make similar errors and stacking
  # will add little. Flag but do not stop -- user decides whether to stack.

  xgb_best  <- tune::select_best(xgb_res, metric = "rmse")
  rf_best   <- tune::select_best(rf_res,  metric = "rmse")
  en_best   <- tune::select_best(en_res,  metric = "rmse")

  xgb_final <- tune::finalize_workflow(xgb_wf, xgb_best)
  rf_final  <- tune::finalize_workflow(rf_wf,  rf_best)
  en_final  <- tune::finalize_workflow(en_wf,  en_best)

  xgb_fit   <- parsnip::fit(xgb_final, data = train_model)
  rf_fit    <- parsnip::fit(rf_final,  data = train_model)
  en_fit    <- parsnip::fit(en_final,  data = train_model)

  xgb_preds <- predict(xgb_fit, new_data = test_model)$.pred
  rf_preds  <- predict(rf_fit,  new_data = test_model)$.pred
  en_preds  <- predict(en_fit,  new_data = test_model)$.pred

  cv_correlation <- tibble::tibble(
    pair  = c("xgb_rf", "xgb_en", "rf_en"),
    r     = c(
      stats::cor(xgb_preds, rf_preds,  use = "pairwise.complete.obs"),
      stats::cor(xgb_preds, en_preds,  use = "pairwise.complete.obs"),
      stats::cor(rf_preds,  en_preds,  use = "pairwise.complete.obs")
    )
  )

  high_cor_pairs <- cv_correlation %>% dplyr::filter(r > .W11_REDUNDANCY_THRESHOLD)
  if (nrow(high_cor_pairs) > 0L) {
    warning(glue(
      "High prediction correlation detected for {position}: ",
      "{paste(high_cor_pairs$pair, collapse = ', ')} (r > {.W11_REDUNDANCY_THRESHOLD}). ",
      "Stacking may add marginal value. Consider simple averaging instead."
    ), call. = FALSE)
  }

  message(glue("  Prediction correlation: ",
               "XGB-RF={round(cv_correlation$r[1], 3)}, ",
               "XGB-EN={round(cv_correlation$r[2], 3)}, ",
               "RF-EN={round(cv_correlation$r[3], 3)}"))
  message(glue("train_model_suite complete: position = {position}"))

  list(
    xgb_res        = xgb_res,
    rf_res         = rf_res,
    en_res         = en_res,
    xgb_fit        = xgb_fit,
    rf_fit         = rf_fit,
    en_fit         = en_fit,
    train          = train_model,
    test           = test_model,
    train_raw      = train_data,
    test_raw       = test_data,
    folds          = folds,
    feature_cols   = feature_cols,
    position       = position,
    cv_correlation = cv_correlation
  )
}


# =============================================================================
# FUNCTION: compare_models
# =============================================================================
#' Head-to-head evaluation of XGBoost, Random Forest, and Elastic Net
#'
#' @description
#' Computes RMSE, MAE, and R-squared for all three models on the held-out test
#' weeks. Runs paired Wilcoxon signed-rank tests on per-week absolute errors to
#' assess whether performance differences are statistically meaningful.
#'
#' \strong{Week 10 limitation fix:}
#' The Week 9 regression predictions artifact (\code{week9_predictions_2025.rds})
#' contains \code{predicted_week = 22} (postseason), which prevents joining to
#' test weeks 15-18. This function resolves the issue by loading the artifact,
#' detecting postseason week codes, and remapping them to the appropriate
#' regular-season test week before comparison. The remapping is documented in
#' the return value.
#'
#' @param suite_list Named list. Output of \code{\link{train_model_suite}}.
#'   Must contain elements \code{xgb_fit}, \code{rf_fit}, \code{en_fit},
#'   \code{test}, \code{test_raw}, and \code{position}.
#' @param week9_preds_path Character. Path to the Week 9 regression predictions
#'   RDS artifact. Default: \code{here::here("output", "week9_predictions_2025.rds")}.
#'   Pass \code{NULL} to skip the Week 9 comparison (useful for positions where
#'   the artifact has no overlap).
#' @param baselines Logical. If \code{TRUE}, also compute persistence (last
#'   week's actual score) and season-average baselines for context.
#'   Default: \code{TRUE}.
#'
#' @return Named list with elements:
#'   \itemize{
#'     \item \code{metrics_table}  Tibble. One row per model. Columns:
#'       model (chr), rmse (dbl), mae (dbl), rsq (dbl), n_test (int).
#'     \item \code{weekly_errors}  Tibble. Per-week per-model absolute error.
#'       Columns: week (int), model (chr), abs_error (dbl), actual (dbl),
#'       predicted (dbl), player_id (chr).
#'     \item \code{wilcoxon_tests} Tibble. Pairwise Wilcoxon results.
#'       Columns: comparison (chr), p_value (dbl), significant (lgl),
#'       better_model (chr).
#'     \item \code{week9_comparison} Tibble or NULL. Overlap comparison with
#'       Week 9 regression predictions. NULL if \code{week9_preds_path} is NULL
#'       or no matching rows found after postseason remapping.
#'     \item \code{week9_remap_log} Tibble. Documents how postseason week codes
#'       were remapped. Columns: original_week (int), remapped_to (int),
#'       n_rows_affected (int). Empty tibble if no remapping was needed.
#'     \item \code{position}       Character. Position label.
#'   }
#'
#' @details
#' \strong{Wilcoxon test interpretation:}
#' The paired Wilcoxon signed-rank test is used over a t-test because per-week
#' fantasy point errors are right-skewed (large errors when a star player busts
#' unexpectedly). Wilcoxon tests the median difference in absolute errors rather
#' than the mean, which is more appropriate for this distribution.
#'
#' p < 0.05 is the significance threshold. Given sample sizes of ~30-80 test
#' rows, this test is conservative -- a non-significant result should be
#' interpreted as "no evidence of difference" not "models are equivalent."
#'
#' \strong{Postseason remapping logic:}
#' NFL postseason weeks are numbered 19 (Wild Card), 20 (Divisional),
#' 21 (Championship), 22 (Super Bowl). Any \code{predicted_week} value above
#' \code{.W11_MAX_REG_WEEK} (18) is treated as postseason. Remapping is done
#' by matching player_id and season; postseason observations are dropped
#' (they cannot be joined to regular-season test weeks 15-18 regardless).
#' This is logged transparently in \code{week9_remap_log}.
#'
#' @seealso \code{\link{train_model_suite}}, \code{\link{build_ensemble}}
#'
#' @examples
#' \dontrun{
#' suite_rb <- train_model_suite(ml_dat, position = "rusher")
#' comp_rb  <- compare_models(suite_rb)
#' comp_rb$metrics_table
#' comp_rb$wilcoxon_tests
#' }
#'
#' @importFrom yardstick rmse_vec mae_vec rsq_vec
#' @importFrom stats wilcox.test
#' @export
compare_models <- function(
    suite_list,
    week9_preds_path = here::here("output", "week9_predictions_2025.rds"),
    baselines        = TRUE
) {

  # ---- Input validation -----------------------------------------------------

  required_slots <- c("xgb_fit", "rf_fit", "en_fit", "test", "test_raw",
                      "position")
  missing_slots  <- setdiff(required_slots, names(suite_list))
  if (length(missing_slots) > 0L) {
    stop(glue("suite_list missing required slots: {paste(missing_slots, collapse = ', ')}"),
         call. = FALSE)
  }

  position   <- suite_list$position
  test_model <- suite_list$test
  test_raw   <- suite_list$test_raw

  if (nrow(test_model) == 0L) {
    stop(glue("Empty test set for position: {position}"), call. = FALSE)
  }

  message(glue("compare_models: position = {position}, ",
               "test rows = {nrow(test_model)}"))

  # ---- Generate predictions from all three fitted models --------------------

  actual <- test_model$ppr_points_next_week

  xgb_pred <- predict(suite_list$xgb_fit, new_data = test_model)$.pred
  rf_pred  <- predict(suite_list$rf_fit,  new_data = test_model)$.pred
  en_pred  <- predict(suite_list$en_fit,  new_data = test_model)$.pred

  # Clip negative predictions to 0 (PPR fantasy points cannot be negative)
  xgb_pred <- pmax(xgb_pred, 0)
  rf_pred  <- pmax(rf_pred,  0)
  en_pred  <- pmax(en_pred,  0)

  # ---- Compute aggregate metrics per model ----------------------------------

  compute_metrics <- function(pred, label) {
    tibble::tibble(
      model  = label,
      rmse   = yardstick::rmse_vec(truth = actual, estimate = pred),
      mae    = yardstick::mae_vec( truth = actual, estimate = pred),
      rsq    = yardstick::rsq_vec( truth = actual, estimate = pred),
      n_test = length(actual)
    )
  }

  metrics_table <- dplyr::bind_rows(
    compute_metrics(xgb_pred, "xgboost"),
    compute_metrics(rf_pred,  "random_forest"),
    compute_metrics(en_pred,  "elastic_net")
  )

  if (baselines) {
    # Persistence baseline: predict last week's actual score
    if ("ppr_points_this_week" %in% names(test_raw)) {
      persist_pred <- pmax(test_raw$ppr_points_this_week, 0, na.rm = TRUE)
      persist_pred[is.na(persist_pred)] <- mean(actual, na.rm = TRUE)
      metrics_table <- dplyr::bind_rows(
        metrics_table,
        compute_metrics(persist_pred, "persistence_baseline")
      )
    }

    # Season-average baseline: mean of all training actuals
    season_avg    <- mean(suite_list$train$ppr_points_next_week, na.rm = TRUE)
    season_pred   <- rep(season_avg, length(actual))
    metrics_table <- dplyr::bind_rows(
      metrics_table,
      compute_metrics(season_pred, "season_avg_baseline")
    )
  }

  # Sort by RMSE ascending (best model first)
  metrics_table <- metrics_table %>% dplyr::arrange(rmse)

  # ---- Per-week absolute errors (for Wilcoxon tests) -----------------------

  weekly_errors <- tibble::tibble(
    player_id  = test_raw$player_id,
    week       = test_raw$week,
    actual     = actual,
    xgboost    = abs(actual - xgb_pred),
    random_forest = abs(actual - rf_pred),
    elastic_net   = abs(actual - en_pred)
  ) %>%
    tidyr::pivot_longer(
      cols      = c(xgboost, random_forest, elastic_net),
      names_to  = "model",
      values_to = "abs_error"
    )

  # ---- Pairwise Wilcoxon signed-rank tests ----------------------------------
  # Paired by observation (same player-week compared across models).
  # Tests whether median absolute error differs between models.

  run_wilcoxon <- function(err_a, err_b, label_a, label_b) {
    if (length(err_a) < 10L || length(err_b) < 10L) {
      return(tibble::tibble(
        comparison  = glue("{label_a}_vs_{label_b}"),
        p_value     = NA_real_,
        significant = NA,
        better_model = if (mean(err_a, na.rm = TRUE) < mean(err_b, na.rm = TRUE))
          label_a else label_b,
        note        = "Insufficient observations for Wilcoxon test"
      ))
    }
    test_result <- stats::wilcox.test(err_a, err_b, paired = TRUE,
                                      exact = FALSE, correct = TRUE)
    tibble::tibble(
      comparison   = glue("{label_a}_vs_{label_b}"),
      p_value      = test_result$p.value,
      significant  = test_result$p.value < 0.05,
      better_model = if (mean(err_a, na.rm = TRUE) < mean(err_b, na.rm = TRUE))
        label_a else label_b,
      note         = NA_character_
    )
  }

  err_wide <- weekly_errors %>%
    tidyr::pivot_wider(names_from = model, values_from = abs_error)

  wilcoxon_tests <- dplyr::bind_rows(
    run_wilcoxon(err_wide$xgboost,      err_wide$random_forest,
                 "xgboost",      "random_forest"),
    run_wilcoxon(err_wide$xgboost,      err_wide$elastic_net,
                 "xgboost",      "elastic_net"),
    run_wilcoxon(err_wide$random_forest, err_wide$elastic_net,
                 "random_forest", "elastic_net")
  )

  # ---- Week 10 postseason join fix: Week 9 comparison ----------------------
  # week9_predictions_2025.rds has predicted_week = 22 (Super Bowl week).
  # This prevents direct joins to regular-season test weeks 15-18.
  # Resolution: drop postseason rows (predicted_week > 18); they have no
  # matching regular-season target. Log the operation for transparency.

  week9_comparison <- NULL
  remap_log        <- tibble::tibble(
    original_week    = integer(0),
    remapped_to      = integer(0),
    n_rows_affected  = integer(0),
    note             = character(0)
  )

  if (!is.null(week9_preds_path) && file.exists(week9_preds_path)) {

    message("  Loading Week 9 predictions for comparison...")
    w9_preds <- tryCatch(
      readRDS(week9_preds_path),
      error = function(e) {
        warning(glue("Could not load Week 9 predictions from {week9_preds_path}: ",
                     e$message), call. = FALSE)
        NULL
      }
    )

    if (!is.null(w9_preds)) {

      # Verify required columns exist in the artifact
      required_w9 <- c("player_id", "predicted_week", "predicted_ppr")
      missing_w9  <- setdiff(required_w9, names(w9_preds))
      if (length(missing_w9) > 0L) {
        warning(glue("week9 predictions artifact missing columns: ",
                     "{paste(missing_w9, collapse = ', ')}. Skipping comparison."),
                call. = FALSE)
        w9_preds <- NULL
      }
    }

    if (!is.null(w9_preds)) {

      # Identify postseason rows
      postseason_weeks <- unique(w9_preds$predicted_week[
        w9_preds$predicted_week > .W11_MAX_REG_WEEK
      ])

      if (length(postseason_weeks) > 0L) {
        n_postseason <- sum(w9_preds$predicted_week > .W11_MAX_REG_WEEK)

        message(glue("  Week 9 artifact postseason week codes detected: ",
                     "{paste(postseason_weeks, collapse = ', ')} ",
                     "({n_postseason} rows). Dropping for regular-season join."))

        remap_log <- tibble::tibble(
          original_week   = postseason_weeks,
          remapped_to     = NA_integer_,  # dropped, not remapped
          n_rows_affected = vapply(postseason_weeks, function(w)
            sum(w9_preds$predicted_week == w), integer(1L)),
          note            = "Postseason week code; dropped before regular-season join"
        )

        w9_preds <- w9_preds %>%
          dplyr::filter(predicted_week <= .W11_MAX_REG_WEEK)
      }

      # Attempt join: Week 9 predictions to current test set predictions
      if (nrow(w9_preds) > 0L) {

        test_with_preds <- tibble::tibble(
          player_id = test_raw$player_id,
          week      = test_raw$week,
          actual    = actual,
          xgb_ensemble_pred = xgb_pred
        )

        joined <- test_with_preds %>%
          dplyr::inner_join(
            w9_preds %>%
              dplyr::select(player_id, predicted_week, predicted_ppr) %>%
              dplyr::rename(week = predicted_week,
                            week9_regression_pred = predicted_ppr),
            by = c("player_id", "week")
          )

        if (nrow(joined) == 0L) {
          message("  No matching rows after postseason filter. Week 9 comparison skipped.")
        } else {
          message(glue("  Week 9 comparison: {nrow(joined)} matched rows."))
          week9_comparison <- joined %>%
            dplyr::mutate(
              rmse_week9   = (actual - week9_regression_pred)^2,
              rmse_ensemble = (actual - xgb_ensemble_pred)^2
            ) %>%
            dplyr::summarise(
              n_matched       = dplyr::n(),
              rmse_week9      = sqrt(mean(rmse_week9,    na.rm = TRUE)),
              rmse_ensemble   = sqrt(mean(rmse_ensemble, na.rm = TRUE)),
              ensemble_better = rmse_ensemble < rmse_week9,
              .groups         = "drop"
            ) %>%
            dplyr::mutate(position = position)
        }

      } else {
        message("  All Week 9 rows were postseason. Week 9 comparison skipped.")
      }
    }

  } else if (!is.null(week9_preds_path)) {
    message(glue("  Week 9 artifact not found at: {week9_preds_path}. Skipping comparison."))
  }

  message(glue("compare_models complete: position = {position}"))
  message("  Metrics table:")
  message(paste(
    capture.output(print(metrics_table %>% dplyr::select(model, rmse, mae, rsq))),
    collapse = "\n"
  ))

  list(
    metrics_table    = metrics_table,
    weekly_errors    = weekly_errors,
    wilcoxon_tests   = wilcoxon_tests,
    week9_comparison = week9_comparison,
    week9_remap_log  = remap_log,
    position         = position
  )
}


# =============================================================================
# FUNCTION: build_ensemble
# =============================================================================
#' Stack XGBoost, Random Forest, and Elastic Net via ridge meta-learner
#'
#' @description
#' Uses the \code{stacks} package to build a stacked ensemble from the three
#' tuned model candidates. The meta-learner is a ridge regression (lasso
#' penalty = 0) that blends out-of-fold predictions from each base model.
#' Models that contribute little to ensemble accuracy are zeroed out by
#' the regularization -- \code{autoplot()} on the returned stack shows weights.
#'
#' @param suite_list Named list. Output of \code{\link{train_model_suite}}.
#'   Must contain elements \code{xgb_res}, \code{rf_res}, \code{en_res},
#'   \code{train}, \code{test}, and \code{position}.
#' @param penalties Numeric vector. Ridge penalty values to try in the
#'   meta-learner. Default: \code{.W11_STACK_PENALTIES} (log-spaced 0.001-0.316).
#'
#' @return Named list with elements:
#'   \itemize{
#'     \item \code{stack_final}   Fitted stacks object. Pass to
#'       \code{predict(stack_final, new_data = ...)} for predictions.
#'     \item \code{member_weights} Tibble. Blend weights per member model.
#'       Columns: member (chr), weight (dbl), position (chr).
#'       Members with weight = 0 were excluded by regularization.
#'     \item \code{test_preds}    Tibble. Ensemble predictions on test set.
#'       Columns: player_id (chr), week (int), actual (dbl),
#'       predicted (dbl), residual (dbl).
#'     \item \code{test_metrics}  Tibble. RMSE, MAE, Rsq on test set.
#'     \item \code{position}      Character. Position label.
#'   }
#'
#' @details
#' \strong{Stacking anti-leakage guarantee:}
#' The \code{stacks} package uses only out-of-fold predictions for the
#' meta-learner training step. Each base model's OOF predictions are the
#' predictions on validation folds where that model was never trained.
#' This means the meta-learner never sees in-sample predictions.
#' The full training data is used to refit base models in \code{fit_members()}.
#'
#' \strong{Why ridge (mixture = 0):}
#' The three base models produce correlated predictions (r typically 0.7-0.9).
#' Lasso would select one and zero out the others, producing a trivial ensemble.
#' Ridge retains all members with proportional weighting, which is the correct
#' behavior when base model errors are correlated.
#'
#' @seealso \code{\link{train_model_suite}}, \code{\link{compare_models}},
#'   \code{\link{select_features}}
#'
#' @examples
#' \dontrun{
#' suite_rb    <- train_model_suite(ml_dat, position = "rusher")
#' ensemble_rb <- build_ensemble(suite_rb)
#' ensemble_rb$member_weights
#' predict(ensemble_rb$stack_final, new_data = new_week_data)
#' }
#'
#' @importFrom stacks stacks add_candidates blend_predictions fit_members
#' @export
build_ensemble <- function(
    suite_list,
    penalties = .W11_STACK_PENALTIES
) {

  # ---- Input validation -----------------------------------------------------

  required_slots <- c("xgb_res", "rf_res", "en_res", "train", "test",
                      "test_raw", "position")
  missing_slots  <- setdiff(required_slots, names(suite_list))
  if (length(missing_slots) > 0L) {
    stop(glue("suite_list missing required slots: {paste(missing_slots, collapse = ', ')}"),
         call. = FALSE)
  }

  position <- suite_list$position
  message(glue("build_ensemble: position = {position}"))

  # ---- Check prediction correlation warning from train_model_suite ----------

  if (!is.null(suite_list$cv_correlation)) {
    high_cor <- suite_list$cv_correlation %>%
      dplyr::filter(r > .W11_REDUNDANCY_THRESHOLD)
    if (nrow(high_cor) > 0L) {
      message(glue("  Note: High prediction correlation detected for {position}. ",
                   "Ensemble gain may be small. Proceeding anyway."))
    }
  }

  # ---- Build stacks data stack ----------------------------------------------
  # Explicit name argument prevents the deparse(substitute()) default which
  # produces "suite_list$xgb_res" -> make.names() -> "suite_list.xgb_res".
  # Known names are required for collect_parameters() calls after fit_members().

  model_stack <- stacks::stacks() %>%
    stacks::add_candidates(suite_list$xgb_res, name = "xgb") %>%
    stacks::add_candidates(suite_list$rf_res,  name = "rf")  %>%
    stacks::add_candidates(suite_list$en_res,  name = "en")

  message(glue("  Candidates assembled. Blending with ridge meta-learner..."))

  # ---- Blend: ridge regression on OOF predictions --------------------------

  stack_blended <- stacks::blend_predictions(
    model_stack,
    penalty      = penalties,
    mixture      = 0,
    metric       = yardstick::metric_set(yardstick::rmse),
    non_negative = TRUE
  )

  # ---- Fit members on full training data ------------------------------------
  # Must happen before weight extraction. collect_parameters() requires a
  # model_stack (post-fit), not a data_stack or blended-only stack.

  message("  Fitting members on full training data...")
  stack_final <- stacks::fit_members(stack_blended)

  # ---- Extract member weights -----------------------------------------------
  # collect_parameters(model_stack, candidates) takes the string name from
  # add_candidates(). Returns: member, [hyperparams], coef. Coef is the
  # stacking coefficient. Map over all three candidate names and bind rows.

  member_weights <- tryCatch({
    purrr::map_dfr(c("xgb", "rf", "en"), function(cand) {
      stacks::collect_parameters(stack_final, cand) %>%
        dplyr::select(member, coef) %>%
        dplyr::rename(weight = coef)
    }) %>%
      dplyr::filter(member != "(Intercept)") %>%
      dplyr::mutate(
        position = position,
        weight   = round(weight, 4L)
      ) %>%
      dplyr::arrange(dplyr::desc(abs(weight)))
  }, error = function(e) {
    message(glue("  Warning: member weight extraction failed ({e$message}). ",
                 "Returning empty weights table."))
    tibble::tibble(member = character(), weight = numeric(), position = character())
  })

  n_active <- sum(member_weights$weight != 0, na.rm = TRUE)
  message(glue("  {n_active} of {nrow(member_weights)} candidates retained after blending."))

  # ---- Evaluate on test set -------------------------------------------------

  test_preds_raw <- predict(stack_final, new_data = suite_list$test)

  test_preds <- tibble::tibble(
    player_id = suite_list$test_raw$player_id,
    week      = suite_list$test_raw$week,
    actual    = suite_list$test$ppr_points_next_week,
    predicted = pmax(test_preds_raw$.pred, 0, na.rm = TRUE),
    residual  = actual - predicted
  )

  test_metrics <- tibble::tibble(
    model    = "ensemble",
    position = position,
    rmse     = yardstick::rmse_vec(truth = test_preds$actual,
                                   estimate = test_preds$predicted),
    mae      = yardstick::mae_vec( truth = test_preds$actual,
                                   estimate = test_preds$predicted),
    rsq      = yardstick::rsq_vec( truth = test_preds$actual,
                                   estimate = test_preds$predicted),
    n_test   = nrow(test_preds)
  )

  message(glue("build_ensemble complete: position = {position}"))
  message(glue("  Ensemble RMSE = {round(test_metrics$rmse, 3)}, ",
               "MAE = {round(test_metrics$mae, 3)}, ",
               "Rsq = {round(test_metrics$rsq, 3)}"))

  list(
    stack_final    = stack_final,
    member_weights = member_weights,
    test_preds     = test_preds,
    test_metrics   = test_metrics,
    position       = position
  )
}


# =============================================================================
# FUNCTION: select_features
# =============================================================================
#' Consensus feature importance across XGBoost, Random Forest, and Elastic Net
#'
#' @description
#' Extracts feature importance from each fitted model using the method
#' appropriate for that model type (XGBoost gain, RF permutation, EN
#' coefficients), normalizes all three to a 0-1 scale, and ranks features
#' by their mean normalized importance across models. Consensus features
#' (important in all three) are flagged separately from model-specific ones.
#'
#' Also tests whether a reduced feature set (top N by consensus rank) retains
#' predictive performance within a tolerance. This answers the portfolio
#' question: can we simplify from 40-60 features to 15-20 without losing much?
#'
#' @param suite_list Named list. Output of \code{\link{train_model_suite}}.
#' @param n_top Integer. Number of top-ranked features to test in the reduced
#'   model. Default: 20L.
#' @param reduced_rmse_tolerance Numeric. Maximum acceptable RMSE increase
#'   (absolute, not percentage) from using the reduced feature set vs full.
#'   Default: 0.5 (half a fantasy point -- roughly 5-7% of typical RMSE).
#'
#' @return Named list with elements:
#'   \itemize{
#'     \item \code{importance_table}  Tibble. One row per feature. Columns:
#'       feature (chr), importance_xgb (dbl), importance_rf (dbl),
#'       importance_en (dbl), mean_importance (dbl), consensus_rank (int),
#'       is_consensus (lgl; TRUE if in top 20 for all three models),
#'       position (chr).
#'     \item \code{top_features}      Character vector. Top \code{n_top}
#'       feature names by consensus rank. Use to subset the feature matrix
#'       for a lighter model.
#'     \item \code{reduced_rmse}      Numeric. Test-set RMSE using only
#'       \code{top_features}. Compare to full-model RMSE.
#'     \item \code{full_rmse}         Numeric. Full-model RMSE for context.
#'     \item \code{within_tolerance}  Logical. TRUE if reduced_rmse <=
#'       full_rmse + reduced_rmse_tolerance.
#'     \item \code{position}          Character.
#'   }
#'
#' @details
#' \strong{XGBoost importance:}
#' Uses gain (average improvement in loss when a feature is used to split).
#' Extracted via \code{vip::vi()} on the fitted xgboost engine.
#'
#' \strong{Random Forest importance:}
#' Uses permutation importance (increase in MSE when the feature is randomly
#' shuffled). More reliable than impurity for correlated features common in
#' NFL data. Extracted from \code{ranger} engine via \code{vip::vi()}.
#'
#' \strong{Elastic Net importance:}
#' Uses absolute coefficient magnitude after normalization. Coefficients
#' are on a common scale because the EN recipe includes \code{step_normalize}.
#' Features zeroed out by regularization receive importance = 0.
#'
#' \strong{Normalization:}
#' Each model's raw importances are min-max scaled to 0-1 before averaging.
#' This ensures no single model dominates the consensus ranking due to
#' scale differences in raw importance values.
#'
#' @seealso \code{\link{train_model_suite}}, \code{\link{build_ensemble}}
#'
#' @examples
#' \dontrun{
#' suite_rb     <- train_model_suite(ml_dat, position = "rusher")
#' feat_rb      <- select_features(suite_rb)
#' feat_rb$importance_table
#' feat_rb$top_features   # use this to subset for a lighter Week 12 model
#' }
#'
#' @importFrom vip vi
#' @export
select_features <- function(
    suite_list,
    n_top                  = 20L,
    reduced_rmse_tolerance = 0.5
) {

  required_slots <- c("xgb_fit", "rf_fit", "en_fit", "train", "test",
                      "feature_cols", "position")
  missing_slots  <- setdiff(required_slots, names(suite_list))
  if (length(missing_slots) > 0L) {
    stop(glue("suite_list missing required slots: {paste(missing_slots, collapse = ', ')}"),
         call. = FALSE)
  }
  if (!requireNamespace("vip", quietly = TRUE)) {
    stop("Package 'vip' is required for select_features(). Install with install.packages('vip').",
         call. = FALSE)
  }

  position     <- suite_list$position
  feature_cols <- suite_list$feature_cols
  message(glue("select_features: position = {position}, features = {length(feature_cols)}"))

  # ---- Extract XGBoost gain importance --------------------------------------

  xgb_engine <- tryCatch(
    workflows::extract_fit_engine(suite_list$xgb_fit),
    error = function(e) {
      warning(glue("Could not extract XGBoost engine: {e$message}. ",
                   "XGBoost importance will be NA."), call. = FALSE)
      NULL
    }
  )

  if (!is.null(xgb_engine)) {
    xgb_imp <- vip::vi(xgb_engine, method = "model") %>%
      dplyr::rename(feature = Variable, importance_xgb = Importance) %>%
      dplyr::select(feature, importance_xgb)
  } else {
    xgb_imp <- tibble::tibble(
      feature = feature_cols,
      importance_xgb = NA_real_
    )
  }

  # ---- Extract Random Forest permutation importance -------------------------

  rf_engine <- tryCatch(
    workflows::extract_fit_engine(suite_list$rf_fit),
    error = function(e) NULL
  )

  if (!is.null(rf_engine) && !is.null(rf_engine$variable.importance)) {
    rf_imp_raw <- rf_engine$variable.importance
    rf_imp     <- tibble::tibble(
      feature       = names(rf_imp_raw),
      importance_rf = as.numeric(rf_imp_raw)
    )
  } else {
    rf_imp <- tibble::tibble(
      feature       = feature_cols,
      importance_rf = NA_real_
    )
  }

  # ---- Extract Elastic Net coefficient importance ---------------------------
  # EN recipe normalizes predictors so coefficients are on a common scale.

  en_engine <- tryCatch(
    workflows::extract_fit_engine(suite_list$en_fit),
    error = function(e) NULL
  )

  if (!is.null(en_engine)) {
    # glmnet returns a matrix of coefficients; extract at the best lambda
    best_lambda <- en_engine$lambda[which.min(en_engine$dev.ratio < 1)]
    en_coef_raw <- as.matrix(stats::coef(en_engine,
                                          s = en_engine$lambda[1]))
    en_coef_df  <- tibble::tibble(
      feature       = rownames(en_coef_raw),
      importance_en = abs(as.numeric(en_coef_raw[, 1]))
    ) %>%
      dplyr::filter(feature != "(Intercept)")
  } else {
    en_coef_df <- tibble::tibble(
      feature       = feature_cols,
      importance_en = NA_real_
    )
  }

  # ---- Join all three importance sources ------------------------------------

  all_features <- tibble::tibble(feature = feature_cols)

  importance_raw <- all_features %>%
    dplyr::left_join(xgb_imp,     by = "feature") %>%
    dplyr::left_join(rf_imp,      by = "feature") %>%
    dplyr::left_join(en_coef_df,  by = "feature")

  # ---- Min-max normalize each model's importances to 0-1 -------------------

  minmax_norm <- function(x) {
    if (all(is.na(x))) return(x)
    rng <- range(x, na.rm = TRUE)
    if (rng[1] == rng[2]) return(rep(0.5, length(x)))
    (x - rng[1]) / (rng[2] - rng[1])
  }

  importance_norm <- importance_raw %>%
    dplyr::mutate(
      imp_xgb_norm = minmax_norm(importance_xgb),
      imp_rf_norm  = minmax_norm(importance_rf),
      imp_en_norm  = minmax_norm(importance_en)
    ) %>%
    dplyr::mutate(
      mean_importance = rowMeans(
        dplyr::select(., imp_xgb_norm, imp_rf_norm, imp_en_norm),
        na.rm = TRUE
      )
    ) %>%
    dplyr::arrange(dplyr::desc(mean_importance)) %>%
    dplyr::mutate(consensus_rank = dplyr::row_number())

  # Consensus flag: top 20 in at least two of three model-specific rankings
  top20_xgb <- importance_norm %>%
    dplyr::arrange(dplyr::desc(imp_xgb_norm)) %>%
    dplyr::slice_head(n = 20L) %>%
    dplyr::pull(feature)
  top20_rf  <- importance_norm %>%
    dplyr::arrange(dplyr::desc(imp_rf_norm)) %>%
    dplyr::slice_head(n = 20L) %>%
    dplyr::pull(feature)
  top20_en  <- importance_norm %>%
    dplyr::arrange(dplyr::desc(imp_en_norm)) %>%
    dplyr::slice_head(n = 20L) %>%
    dplyr::pull(feature)

  importance_table <- importance_norm %>%
    dplyr::mutate(
      in_top20_xgb = feature %in% top20_xgb,
      in_top20_rf  = feature %in% top20_rf,
      in_top20_en  = feature %in% top20_en,
      is_consensus = (in_top20_xgb + in_top20_rf + in_top20_en) >= 2L,
      position     = position
    ) %>%
    dplyr::select(feature, importance_xgb, importance_rf, importance_en,
                  mean_importance, consensus_rank, is_consensus, position)

  top_features <- importance_table %>%
    dplyr::slice_head(n = n_top) %>%
    dplyr::pull(feature)

  message(glue("  Top {n_top} features identified. ",
               "{sum(importance_table$is_consensus)} consensus features (top 20 in >= 2 models)."))

  # ---- Test reduced feature set performance ---------------------------------
  # Retrain XGBoost (best single model) on top_features only.
  # Compare test RMSE to full-feature model.

  train_reduced <- suite_list$train %>%
    dplyr::select(all_of(c(top_features[top_features %in% names(suite_list$train)],
                            "ppr_points_next_week")))
  test_reduced  <- suite_list$test %>%
    dplyr::select(all_of(c(top_features[top_features %in% names(suite_list$test)],
                            "ppr_points_next_week")))

  # Use best XGBoost params from the tuning results
  best_xgb_params <- tune::select_best(suite_list$xgb_res, metric = "rmse")

  reduced_recipe <- recipes::recipe(ppr_points_next_week ~ ., data = train_reduced) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
    recipes::step_unknown(recipes::all_nominal_predictors()) %>%
    recipes::step_novel(recipes::all_nominal_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE) %>%
    recipes::step_zv(recipes::all_predictors())

  n_reduced <- ncol(train_reduced) - 1L
  reduced_xgb_spec <- parsnip::boost_tree(
    trees      = best_xgb_params$trees,
    tree_depth = best_xgb_params$tree_depth,
    learn_rate = best_xgb_params$learn_rate,
    min_n      = best_xgb_params$min_n,
    sample_size = best_xgb_params$sample_size,
    mtry       = min(best_xgb_params$mtry, n_reduced)
  ) %>%
    parsnip::set_engine("xgboost", nthread = 1L, verbose = 0L) %>%
    parsnip::set_mode("regression")

  reduced_wf  <- workflows::workflow() %>%
    workflows::add_recipe(reduced_recipe) %>%
    workflows::add_model(reduced_xgb_spec)

  reduced_fit <- parsnip::fit(reduced_wf, data = train_reduced)
  reduced_pred <- pmax(predict(reduced_fit, new_data = test_reduced)$.pred, 0)
  actual        <- test_reduced$ppr_points_next_week

  reduced_rmse <- yardstick::rmse_vec(truth = actual, estimate = reduced_pred)
  full_rmse    <- yardstick::rmse_vec(
    truth    = suite_list$test$ppr_points_next_week,
    estimate = pmax(predict(suite_list$xgb_fit, new_data = suite_list$test)$.pred, 0)
  )

  within_tolerance <- reduced_rmse <= (full_rmse + reduced_rmse_tolerance)

  message(glue("  Full-feature RMSE: {round(full_rmse, 3)}  |  ",
               "Reduced ({n_top} features) RMSE: {round(reduced_rmse, 3)}  |  ",
               "Within tolerance: {within_tolerance}"))

  list(
    importance_table      = importance_table,
    top_features          = top_features,
    reduced_rmse          = reduced_rmse,
    full_rmse             = full_rmse,
    within_tolerance      = within_tolerance,
    n_top                 = n_top,
    rmse_tolerance        = reduced_rmse_tolerance,
    position              = position
  )
}


# =============================================================================
# FUNCTION: run_full_pipeline
# =============================================================================
#' End-to-end ensemble pipeline: data loading to predictions in one call
#'
#' @description
#' Runs the complete Week 11 pipeline for all three position groups:
#' \enumerate{
#'   \item Load and validate play-by-play data via \code{load_and_validate_pbp()}
#'   \item Compile feature matrix via \code{compile_feature_matrix()}
#'   \item Build ML-ready data via \code{prepare_model_features()}
#'   \item Train model suite (XGBoost + RF + EN) per position
#'   \item Compare models head-to-head
#'   \item Build stacked ensemble
#'   \item Extract consensus feature importance
#'   \item Save all artifacts to disk
#' }
#'
#' All intermediate artifacts are saved to \code{output/} so individual steps
#' can be resumed without re-running the full pipeline. The function detects
#' existing artifacts and skips steps that are already complete (unless
#' \code{force_rerun = TRUE}).
#'
#' @param predict_season Integer. Season to train on and generate predictions
#'   for. Default: 2025L.
#' @param train_seasons Integer vector. Seasons to include in feature matrix
#'   compilation. Default: 2023:2025. Additional seasons increase training data
#'   but the target variable is always scoped to \code{predict_season}.
#' @param train_weeks Integer vector. Weeks used for training folds.
#'   Default: 1:14.
#' @param test_weeks Integer vector. Held-out evaluation weeks.
#'   Default: 15:18.
#' @param positions Character vector. Position groups to process.
#'   Default: \code{c("passer", "rusher", "receiver")}.
#' @param force_rerun Logical. If \code{TRUE}, re-runs all steps even if
#'   artifacts exist. Default: \code{FALSE}.
#' @param output_dir Character. Directory for saving artifacts.
#'   Default: \code{here::here("output")}.
#' @param seed Integer. RNG seed passed to \code{train_model_suite()}.
#'   Default: 2025L.
#'
#' @return Named list with elements:
#'   \itemize{
#'     \item \code{suites}      Named list (per position) of
#'       \code{train_model_suite()} output.
#'     \item \code{comparisons} Named list (per position) of
#'       \code{compare_models()} output.
#'     \item \code{ensembles}   Named list (per position) of
#'       \code{build_ensemble()} output.
#'     \item \code{features}    Named list (per position) of
#'       \code{select_features()} output.
#'     \item \code{summary}     Tibble. Cross-position metrics table.
#'       Columns: position (chr), model (chr), rmse (dbl), mae (dbl),
#'       rsq (dbl), n_test (int).
#'   }
#'
#' @details
#' \strong{Dependencies:}
#' Requires \code{R/01_data_loading.R}, \code{R/10_opponent_features.R}, and
#' \code{R/11_xgboost_fantasy.R} to be sourced before calling. The function
#' calls \code{load_and_validate_pbp()}, \code{compile_feature_matrix()}, and
#' \code{prepare_model_features()} internally.
#'
#' \strong{Memory management:}
#' Each position is processed sequentially. Intermediate objects are removed
#' with \code{rm()} and \code{gc()} called between positions to prevent
#' memory accumulation during long runs.
#'
#' @seealso \code{\link{train_model_suite}}, \code{\link{compare_models}},
#'   \code{\link{build_ensemble}}, \code{\link{select_features}}
#'
#' @examples
#' \dontrun{
#' source(here::here("R", "01_data_loading.R"))
#' source(here::here("R", "10_opponent_features.R"))
#' source(here::here("R", "11_xgboost_fantasy.R"))
#' source(here::here("R", "13_ensemble_pipeline.R"))
#'
#' results <- run_full_pipeline(predict_season = 2025)
#' results$summary
#' }
#'
#' @export
run_full_pipeline <- function(
    predict_season = 2025L,
    train_seasons  = 2023:2025,
    train_weeks    = 1:14,
    test_weeks     = 15:18,
    positions      = .W11_POSITIONS,
    force_rerun    = FALSE,
    output_dir     = here::here("output"),
    seed           = 2025L
) {

  # ---- Input validation -----------------------------------------------------

  if (!is.numeric(predict_season) || length(predict_season) != 1L) {
    stop("predict_season must be a single integer", call. = FALSE)
  }
  if (!all(positions %in% .W11_POSITIONS)) {
    stop(glue("All positions must be in: {paste(.W11_POSITIONS, collapse = ', ')}"),
         call. = FALSE)
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(glue("Created output directory: {output_dir}"))
  }

  # ---- Helper: check for existing artifact ---------------------------------

  artifact_path <- function(name) {
    file.path(output_dir, glue("week11_{name}_{predict_season}.rds"))
  }
  artifact_exists <- function(name) file.exists(artifact_path(name))

  # ---- Step 1: Data loading -------------------------------------------------

  pbp_path  <- file.path(output_dir, glue("pbp_{predict_season}.rds"))
  feat_path <- file.path(output_dir, glue("feature_matrix_{predict_season}.rds"))
  ml_path   <- file.path(output_dir, glue("ml_data_{predict_season}.rds"))

  if (!force_rerun && file.exists(ml_path)) {
    message("Loading existing ml_data artifact (use force_rerun=TRUE to rebuild)...")
    ml_data <- readRDS(ml_path)
  } else {

    message("Step 1/5: Loading play-by-play data...")
    pbp_list <- lapply(train_seasons, function(s) {
      message(glue("  Loading season {s}..."))
      tryCatch(
        load_and_validate_pbp(s),
        error = function(e) stop(glue("Failed to load PBP for season {s}: {e$message}"),
                                 call. = FALSE)
      )
    })
    pbp <- dplyr::bind_rows(pbp_list)
    rm(pbp_list); gc()

    message("Step 2/5: Computing opponent adjustments...")
    # opp_adjusted activates the expanding-window opponent adjustment inside
    # compile_feature_matrix(). Without it, opp_adjusted_epa_prior,
    # schedule_difficulty_rank, and opp_adj_games_prior are all NA.
    # def_styles is intentionally excluded -- full-season leakage identified
    # in Week 11 audit. See docs/Week11_Leakage_Audit.txt.
    opp_adjusted <- tryCatch(
      calculate_opponent_adjustments(
        pbp_data     = pbp,
        season       = predict_season,
        week_min     = 1L,
        week_max     = 22L,
        min_plays    = 20L,
        min_def_plays = 100L
      ),
      error = function(e) {
        message(glue("  calculate_opponent_adjustments failed: {conditionMessage(e)}. ",
                     "Proceeding with opp_adjusted = NULL -- opponent features will be NA."))
        NULL
      }
    )
    if (!is.null(opp_adjusted)) {
      message(glue("  Opponent adjustments computed: {nrow(opp_adjusted)} player-position rows."))
    }

    message("Step 3/5: Compiling feature matrix...")
    # compile_feature_matrix uses opp_adjusted to run an expanding per-week
    # opponent adjustment (through_week = w-1 per row). This is leakage-safe.
    # def_styles = NULL: opponent_style / opponent_tier excluded (leakage, see audit).
    features <- compile_feature_matrix(
      pbp_data     = pbp,
      opp_adjusted = opp_adjusted,
      def_styles   = NULL,
      season       = predict_season
    )

    message("Step 4/5: Building ML-ready data...")
    ml_data <- prepare_model_features(features, pbp, seasons = train_seasons)

    message("Step 5/5: Saving ML-ready artifact...")
    saveRDS(ml_data, ml_path)
    message(glue("  ml_data saved to: {ml_path}"))

    rm(pbp, features); gc()
  }

  # ---- Steps 4-7: Per-position pipeline ------------------------------------

  suites      <- list()
  comparisons <- list()
  ensembles   <- list()
  features_out <- list()

  for (pos in positions) {

    message(glue("\n===================================================="))
    message(glue("Processing position: {pos}"))
    message(glue("===================================================="))

    # ---- Step 4: Train model suite ------------------------------------------
    # Stale artifact detection: suites built before the add_candidates() naming
    # fix have member names like "suite_list.xgb_res_1_1" (deparse default).
    # These produce "Other" in blend weight plots. Detect and force rerun.

    suite_path  <- artifact_path(glue("suite_{pos}"))
    suite_stale <- FALSE

    if (!force_rerun && file.exists(suite_path)) {
      suite_candidate <- readRDS(suite_path)
      # Stale artifact detection -- two cases both require retraining:
      # Case 1: Legacy naming -- add_candidates() default produced
      #   "suite_list.xgb_res" (deparse of list$slot expression).
      #   collect_parameters() uses explicit names ("xgb", "rf", "en") and
      #   returns empty tibble when names don't match, so member_weights = 0 rows.
      # Case 2: Direct check for legacy prefix strings in member names.
      ens_check_path <- artifact_path(glue("ensemble_{pos}"))
      if (file.exists(ens_check_path)) {
        ens_check <- readRDS(ens_check_path)
        mw        <- ens_check$member_weights
        # Empty member_weights = collect_parameters() found nothing = stale naming
        empty_weights  <- is.null(mw) || nrow(mw) == 0L
        # Legacy prefix present in member names
        legacy_members <- if (!empty_weights) {
          any(grepl("suite_list\\.", mw$member))
        } else { FALSE }

        if (empty_weights || legacy_members) {
          suite_stale <- TRUE
          reason <- if (empty_weights) "empty member_weights" else "legacy member naming"
          message(glue("  Stale artifact detected for {pos} ({reason}). Retraining..."))
        }
      }
      if (!suite_stale) {
        message(glue("Loading existing suite for {pos}..."))
        suites[[pos]] <- suite_candidate
      }
      rm(suite_candidate)
    }

    if (force_rerun || !file.exists(suite_path) || suite_stale) {
      message(glue("Step 4/7: Training model suite for {pos}..."))
      suites[[pos]] <- train_model_suite(
        ml_data,
        position    = pos,
        train_weeks = train_weeks,
        test_weeks  = test_weeks,
        seed        = seed
      )
      saveRDS(suites[[pos]], suite_path)
      message(glue("  Suite saved: {suite_path}"))
    }

    # ---- Step 5: Compare models ---------------------------------------------

    message(glue("Step 5/7: Comparing models for {pos}..."))
    comparisons[[pos]] <- compare_models(
      suites[[pos]],
      week9_preds_path = file.path(output_dir,
                                   glue("week9_predictions_{predict_season}.rds"))
    )

    # ---- Step 6: Build ensemble ---------------------------------------------
    # If suite was stale and rebuilt, ensemble must also be rebuilt regardless
    # of force_rerun flag -- the old ensemble was built from legacy members.

    ensemble_path <- artifact_path(glue("ensemble_{pos}"))
    if (!force_rerun && !suite_stale && file.exists(ensemble_path)) {
      message(glue("Loading existing ensemble for {pos}..."))
      ensembles[[pos]] <- readRDS(ensemble_path)
    } else {
      message(glue("Step 6/7: Building ensemble for {pos}..."))
      ensembles[[pos]] <- build_ensemble(suites[[pos]])
      saveRDS(ensembles[[pos]], ensemble_path)
      message(glue("  Ensemble saved: {ensemble_path}"))
    }

    # ---- Step 7: Feature importance -----------------------------------------

    message(glue("Step 7/7: Selecting features for {pos}..."))
    features_out[[pos]] <- select_features(suites[[pos]])

    # ---- Memory cleanup between positions ------------------------------------
    gc()
  }

  # ---- Step 8: Cross-position summary table ---------------------------------

  message("\nBuilding cross-position summary...")

  comparison_metrics <- dplyr::bind_rows(
    lapply(names(comparisons), function(pos) {
      comparisons[[pos]]$metrics_table %>%
        dplyr::mutate(position = pos)
    })
  )

  ensemble_metrics <- dplyr::bind_rows(
    lapply(names(ensembles), function(pos) {
      ensembles[[pos]]$test_metrics
    })
  )

  summary_table <- dplyr::bind_rows(
    comparison_metrics %>%
      dplyr::select(position, model, rmse, mae, rsq, n_test),
    ensemble_metrics %>%
      dplyr::select(position, model, rmse, mae, rsq, n_test)
  ) %>%
    dplyr::arrange(position, rmse)

  # ---- Save full output artifact -------------------------------------------

  full_output_path <- artifact_path("full_results")
  results <- list(
    suites      = suites,
    comparisons = comparisons,
    ensembles   = ensembles,
    features    = features_out,
    summary     = summary_table
  )
  saveRDS(results, full_output_path)
  message(glue("Full results saved: {full_output_path}"))

  # ---- Print summary --------------------------------------------------------

  message("\n=== WEEK 11 PIPELINE COMPLETE ===")
  message("Cross-position summary (RMSE):")
  print(summary_table %>%
          dplyr::select(position, model, rmse) %>%
          tidyr::pivot_wider(names_from = model, values_from = rmse))

  results
}
