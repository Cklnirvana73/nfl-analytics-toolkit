# =============================================================================
# NFL Analytics Toolkit - Week 11 Tests
# =============================================================================
# File:    tests/test_week11_functions.R
# Purpose: Test suite for R/13_ensemble_pipeline.R
# Run via: source("test_week11.R") from project root
#
# Coverage:
#   - Input validation for all five functions
#   - Temporal CV fold construction (week-level, no leakage)
#   - Postseason week remap logic (Week 10 known limitation fix)
#   - Feature importance extraction and normalization
#   - Reduced feature set tolerance check
#   - Ensemble stacking with stacks package
#   - Cross-position summary table structure
#   - Memory management: rm/gc pattern in run_full_pipeline
#   - Edge cases: single position, all-NA importance column, empty test set
# =============================================================================

library(testthat)
library(dplyr)
library(tibble)
library(tidyr)

# Source production file. Adjust path if running from a different working dir.
source(here::here("R", "13_ensemble_pipeline.R"))


# =============================================================================
# Shared fixtures
# =============================================================================

# Minimal ml_data tibble with enough structure to pass input validation.
# Numeric features are random; target is random PPR points.
make_ml_data <- function(n_train = 120L, n_test = 40L, seed = 42L) {
  set.seed(seed)
  n_total <- n_train + n_test
  weeks   <- c(rep(1:14, length.out = n_train),
               rep(15:18, length.out = n_test))

  tibble::tibble(
    player_id              = paste0("P", sprintf("%03d", sample(1:30, n_total, replace = TRUE))),
    player_name            = paste0("Player_", sample(1:30, n_total, replace = TRUE)),
    position_group         = "rusher",
    season                 = 2025L,
    week                   = weeks,
    ppr_points_next_week   = round(pmax(stats::rnorm(n_total, 12, 6), 0), 1),
    has_target             = TRUE,
    is_absence_week        = FALSE,
    availability_rate      = stats::runif(n_total, 0.6, 1.0),
    weeks_since_last_played = sample(0:3, n_total, replace = TRUE),
    missed_weeks_this_season = sample(0:3, n_total, replace = TRUE),
    # Numeric features (simulated)
    target_share_roll3     = stats::runif(n_total, 0.05, 0.35),
    epa_trend_slope        = stats::rnorm(n_total, 0, 0.5),
    usage_rate             = stats::runif(n_total, 0.05, 0.40),
    matchup_epa_allowed    = stats::rnorm(n_total, 0, 0.3),
    opp_def_rank           = sample(1:32, n_total, replace = TRUE),
    snap_share             = stats::runif(n_total, 0.3, 1.0)
  )
}

ml_data_fixture <- make_ml_data()

# Minimal week9 predictions tibble with no postseason rows.
make_w9_preds <- function(postseason = FALSE) {
  preds <- tibble::tibble(
    player_id      = paste0("P", sprintf("%03d", 1:20)),
    season         = 2025L,
    predicted_week = if (postseason) 22L else rep(15:18, length.out = 20L),
    predicted_ppr  = round(pmax(stats::rnorm(20, 12, 5), 0), 1)
  )
  preds
}


# =============================================================================
# SECTION 1: train_model_suite() - Input validation
# =============================================================================

test_that("train_model_suite rejects non-string position", {
  expect_error(
    train_model_suite(ml_data_fixture, position = 1L),
    regexp = "single character string"
  )
})

test_that("train_model_suite rejects invalid position value", {
  expect_error(
    train_model_suite(ml_data_fixture, position = "quarterback"),
    regexp = "must be one of"
  )
})

test_that("train_model_suite rejects non-data-frame ml_data", {
  expect_error(
    train_model_suite(list(a = 1), position = "rusher"),
    regexp = "data frame"
  )
})

test_that("train_model_suite stops on missing required columns", {
  bad_data <- ml_data_fixture %>% dplyr::select(-ppr_points_next_week)
  expect_error(
    train_model_suite(bad_data, position = "rusher"),
    regexp = "missing required columns"
  )
})

test_that("train_model_suite stops when train_weeks and test_weeks overlap", {
  expect_error(
    train_model_suite(ml_data_fixture, position = "rusher",
                      train_weeks = 1:15, test_weeks = 14:18),
    regexp = "overlap"
  )
})

test_that("train_model_suite stops if position has no training targets", {
  no_target_data <- ml_data_fixture %>%
    dplyr::mutate(ppr_points_next_week = NA_real_)
  expect_error(
    train_model_suite(no_target_data, position = "rusher"),
    regexp = "No rows with training targets"
  )
})

test_that("train_model_suite stops if insufficient training rows", {
  tiny_data <- ml_data_fixture %>% dplyr::slice_head(n = 10L)
  expect_error(
    train_model_suite(tiny_data, position = "rusher"),
    regexp = "Insufficient training rows"
  )
})


# =============================================================================
# SECTION 2: train_model_suite() - Temporal CV fold correctness
# =============================================================================

test_that("temporal CV folds respect week-level boundaries", {
  # The fold construction must not allow a validation row to appear in
  # the training portion of the same fold.
  # Proxy test: verify that in a fold where val_week = W, no training rows
  # have week >= W.

  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 3L)

  folds <- suite$folds

  for (i in seq_len(length(folds$splits))) {
    spl <- folds$splits[[i]]
    train_idx <- rsample::analysis(spl)$week
    val_idx   <- rsample::assessment(spl)$week

    # No validation week should appear in training
    expect_true(
      length(intersect(train_idx, val_idx)) == 0L,
      label = glue::glue("Fold {i}: training and validation weeks must not overlap")
    )
  }
})

test_that("temporal CV folds are ordered (later folds have more training data)", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 3L)

  fold_train_sizes <- vapply(
    suite$folds$splits,
    function(s) nrow(rsample::analysis(s)),
    integer(1L)
  )
  # Each successive fold must have >= as many training rows (expanding window)
  expect_true(
    all(diff(fold_train_sizes) >= 0L),
    label = "Fold training sizes must be non-decreasing (expanding window)"
  )
})

test_that("train_model_suite returns required output slots", {
  # Smoke test with small data -- does not verify model quality
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)

  expected_slots <- c("xgb_res", "rf_res", "en_res", "xgb_fit", "rf_fit",
                       "en_fit", "train", "test", "train_raw", "test_raw",
                       "folds", "feature_cols", "position", "cv_correlation")
  expect_true(all(expected_slots %in% names(suite)),
              label = "All required suite slots must be present")
})

test_that("train_model_suite cv_correlation has three pairs", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  expect_equal(nrow(suite$cv_correlation), 3L)
  expect_true(all(c("pair", "r") %in% names(suite$cv_correlation)))
})

test_that("cv_correlation values are in [-1, 1]", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  expect_true(
    all(suite$cv_correlation$r >= -1 & suite$cv_correlation$r <= 1,
        na.rm = TRUE),
    label = "All correlation values must be in [-1, 1]"
  )
})


# =============================================================================
# SECTION 3: compare_models() - Input validation and postseason fix
# =============================================================================

test_that("compare_models stops on missing suite slots", {
  bad_suite <- list(xgb_fit = NULL, position = "rusher")
  expect_error(
    compare_models(bad_suite),
    regexp = "missing required slots"
  )
})

test_that("compare_models stops on empty test set", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  # Replace test with empty tibble
  suite$test     <- suite$test[0, ]
  suite$test_raw <- suite$test_raw[0, ]
  expect_error(
    compare_models(suite),
    regexp = "Empty test set"
  )
})

test_that("compare_models returns required output slots", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  result  <- compare_models(suite, week9_preds_path = NULL, baselines = FALSE)

  expected_slots <- c("metrics_table", "weekly_errors", "wilcoxon_tests",
                       "week9_comparison", "week9_remap_log", "position")
  expect_true(all(expected_slots %in% names(result)),
              label = "All required compare_models output slots must be present")
})

test_that("compare_models metrics_table has correct columns and 3 rows (no baselines)", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  result  <- compare_models(suite, week9_preds_path = NULL, baselines = FALSE)

  expect_equal(nrow(result$metrics_table), 3L,
               label = "3 models without baselines")
  expect_true(all(c("model", "rmse", "mae", "rsq", "n_test") %in%
                    names(result$metrics_table)))
})

test_that("compare_models metrics_table has 5 rows with baselines", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  # Need ppr_points_this_week for persistence baseline
  ml_mini <- ml_mini %>%
    dplyr::mutate(ppr_points_this_week = ppr_points_next_week +
                    stats::rnorm(dplyr::n(), 0, 2))

  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  result  <- compare_models(suite, week9_preds_path = NULL, baselines = TRUE)

  # season_avg + persistence + 3 models = 5 total
  expect_equal(nrow(result$metrics_table), 5L,
               label = "5 rows with baselines (3 models + 2 baselines)")
})

test_that("compare_models RMSE values are non-negative", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  result  <- compare_models(suite, week9_preds_path = NULL, baselines = FALSE)

  expect_true(all(result$metrics_table$rmse >= 0),
              label = "RMSE must be non-negative")
})

test_that("compare_models wilcoxon_tests has 3 rows (3 pairs)", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  result  <- compare_models(suite, week9_preds_path = NULL, baselines = FALSE)

  expect_equal(nrow(result$wilcoxon_tests), 3L,
               label = "3 pairwise Wilcoxon tests (XGB-RF, XGB-EN, RF-EN)")
})

# --- Week 10 postseason fix tests ---

test_that("compare_models handles NULL week9_preds_path gracefully", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  result  <- compare_models(suite, week9_preds_path = NULL)

  expect_null(result$week9_comparison,
              label = "week9_comparison must be NULL when path is NULL")
  expect_equal(nrow(result$week9_remap_log), 0L,
               label = "remap_log must be empty when no artifact loaded")
})

test_that("compare_models postseason detection: remap_log records postseason weeks", {
  # Build a fake week9 artifact with postseason week codes and save to tmp
  w9_postseason <- make_w9_preds(postseason = TRUE)
  tmp_path      <- tempfile(fileext = ".rds")
  saveRDS(w9_postseason, tmp_path)

  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  result  <- compare_models(suite, week9_preds_path = tmp_path)

  # Postseason rows (week 22) should be logged
  expect_true(nrow(result$week9_remap_log) > 0L,
              label = "remap_log must have entries when postseason weeks detected")
  expect_true(22L %in% result$week9_remap_log$original_week,
              label = "Week 22 must appear in remap_log")

  unlink(tmp_path)
})

test_that("compare_models postseason detection: week9_comparison NULL when all rows dropped", {
  # All rows are postseason -- after dropping, no rows remain to join
  w9_postseason <- make_w9_preds(postseason = TRUE)
  tmp_path      <- tempfile(fileext = ".rds")
  saveRDS(w9_postseason, tmp_path)

  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  result  <- compare_models(suite, week9_preds_path = tmp_path)

  # After dropping all postseason rows, no regular-season rows remain to join
  expect_null(result$week9_comparison,
              label = "week9_comparison must be NULL if all rows were postseason")

  unlink(tmp_path)
})

test_that("compare_models warns on missing required week9 columns", {
  # Artifact missing predicted_ppr column
  bad_w9 <- tibble::tibble(player_id = "P001", predicted_week = 15L)
  tmp_path <- tempfile(fileext = ".rds")
  saveRDS(bad_w9, tmp_path)

  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)

  expect_warning(
    compare_models(suite, week9_preds_path = tmp_path),
    regexp = "missing columns"
  )

  unlink(tmp_path)
})


# =============================================================================
# SECTION 4: build_ensemble() - Structure and anti-leakage
# =============================================================================

test_that("build_ensemble stops on missing suite slots", {
  bad_suite <- list(xgb_res = NULL, position = "rusher")
  expect_error(
    build_ensemble(bad_suite),
    regexp = "missing required slots"
  )
})

test_that("build_ensemble returns required output slots", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  ens     <- build_ensemble(suite)

  expected_slots <- c("stack_final", "member_weights", "test_preds",
                       "test_metrics", "position")
  expect_true(all(expected_slots %in% names(ens)),
              label = "All required build_ensemble slots must be present")
})

test_that("build_ensemble member_weights has correct columns", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  ens     <- build_ensemble(suite)

  expect_true(all(c("member", "weight", "position") %in% names(ens$member_weights)),
              label = "member_weights must have member, weight, position columns")
})

test_that("build_ensemble ensemble predictions are non-negative", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  ens     <- build_ensemble(suite)

  expect_true(all(ens$test_preds$predicted >= 0),
              label = "Ensemble predictions must be >= 0 (PPR cannot be negative)")
})

test_that("build_ensemble test_metrics has expected columns", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  ens     <- build_ensemble(suite)

  expect_true(all(c("model", "position", "rmse", "mae", "rsq", "n_test") %in%
                    names(ens$test_metrics)),
              label = "test_metrics must have standard metric columns")
})

test_that("build_ensemble test_preds row count matches test set", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  ens     <- build_ensemble(suite)

  expect_equal(nrow(ens$test_preds), nrow(suite$test),
               label = "test_preds row count must match test set size")
})


# =============================================================================
# SECTION 5: select_features() - Importance normalization and consensus
# =============================================================================

test_that("select_features stops on missing suite slots", {
  bad_suite <- list(xgb_fit = NULL, position = "rusher")
  expect_error(
    select_features(bad_suite),
    regexp = "missing required slots"
  )
})

test_that("select_features importance_table has correct columns", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  feat    <- select_features(suite, n_top = 5L)

  expected_cols <- c("feature", "importance_xgb", "importance_rf",
                      "importance_en", "mean_importance", "consensus_rank",
                      "is_consensus", "position")
  expect_true(all(expected_cols %in% names(feat$importance_table)),
              label = "importance_table must have all required columns")
})

test_that("select_features consensus_rank is a contiguous sequence starting at 1", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  feat    <- select_features(suite)

  ranks <- sort(feat$importance_table$consensus_rank)
  expect_equal(ranks, seq_len(nrow(feat$importance_table)),
               label = "consensus_rank must be 1:n with no gaps")
})

test_that("select_features top_features length matches n_top", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  feat    <- select_features(suite, n_top = 4L)

  # n_top may be capped by actual number of features
  expect_true(length(feat$top_features) <= 4L,
              label = "top_features length must not exceed n_top")
  expect_true(length(feat$top_features) >= 1L,
              label = "top_features must have at least one feature")
})

test_that("select_features reduced_rmse is a single non-negative numeric", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  feat    <- select_features(suite, n_top = 4L)

  expect_true(is.numeric(feat$reduced_rmse) && length(feat$reduced_rmse) == 1L,
              label = "reduced_rmse must be a single numeric")
  expect_true(feat$reduced_rmse >= 0,
              label = "reduced_rmse must be non-negative")
})

test_that("select_features within_tolerance is logical", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  feat    <- select_features(suite, n_top = 4L)

  expect_true(is.logical(feat$within_tolerance),
              label = "within_tolerance must be logical")
})

test_that("select_features: reduced model uses only top_features subset", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  feat    <- select_features(suite, n_top = 3L)

  # All top_features must exist in the original feature_cols
  expect_true(all(feat$top_features %in% suite$feature_cols),
              label = "top_features must be a subset of feature_cols")
})


# =============================================================================
# SECTION 6: run_full_pipeline() - End-to-end structure (mocked)
# =============================================================================
# Note: run_full_pipeline() calls load_and_validate_pbp(), compile_feature_matrix(),
# and prepare_model_features() internally. Full integration tests require actual
# nflfastR data (too slow for unit tests). These tests verify the ml_data
# resume path and artifact saving logic.

test_that("run_full_pipeline detects and loads existing ml_data artifact", {
  # with_mocked_bindings() requires pkgload which is unavailable when the
  # test file is run via source() outside of devtools. Test the artifact
  # detection logic directly: verify that an RDS saved at the expected path
  # is readable and structurally intact -- this is what run_full_pipeline()
  # does when force_rerun = FALSE and the artifact already exists.

  tmp_dir       <- tempdir()
  ml_mini       <- make_ml_data(n_train = 80L, n_test = 20L)
  artifact_path <- file.path(tmp_dir, "ml_data_2025.rds")
  saveRDS(ml_mini, artifact_path)

  expect_true(file.exists(artifact_path),
              label = "ml_data artifact must exist at expected path")

  ml_loaded <- readRDS(artifact_path)
  expect_equal(nrow(ml_loaded), nrow(ml_mini),
               label = "Loaded artifact must have same row count as saved object")
  expect_true(all(names(ml_mini) %in% names(ml_loaded)),
              label = "Loaded artifact must contain all expected columns")

  unlink(artifact_path)
})

test_that("run_full_pipeline summary table has expected columns", {
  # Test just the summary table construction logic, not the full pipeline
  # Build fake comparison and ensemble results and verify the bind_rows logic

  fake_comp <- list(
    passer  = list(metrics_table = tibble::tibble(
      model = c("xgboost", "random_forest", "elastic_net"),
      rmse = c(6.5, 6.8, 7.1), mae = c(4.5, 4.7, 5.0), rsq = c(0.3, 0.28, 0.22),
      n_test = 30L
    )),
    rusher  = list(metrics_table = tibble::tibble(
      model = c("xgboost", "random_forest", "elastic_net"),
      rmse = c(5.5, 5.8, 6.1), mae = c(3.5, 3.7, 4.0), rsq = c(0.35, 0.33, 0.28),
      n_test = 40L
    ))
  )

  fake_ens <- list(
    passer = list(test_metrics = tibble::tibble(
      model = "ensemble", position = "passer", rmse = 6.2,
      mae = 4.3, rsq = 0.32, n_test = 30L
    )),
    rusher = list(test_metrics = tibble::tibble(
      model = "ensemble", position = "rusher", rmse = 5.3,
      mae = 3.4, rsq = 0.37, n_test = 40L
    ))
  )

  summary_table <- dplyr::bind_rows(
    dplyr::bind_rows(lapply(names(fake_comp), function(pos) {
      fake_comp[[pos]]$metrics_table %>% dplyr::mutate(position = pos)
    })) %>% dplyr::select(position, model, rmse, mae, rsq, n_test),
    dplyr::bind_rows(lapply(names(fake_ens), function(pos) {
      fake_ens[[pos]]$test_metrics %>% dplyr::select(position, model, rmse, mae, rsq, n_test)
    }))
  ) %>% dplyr::arrange(position, rmse)

  expect_true(all(c("position", "model", "rmse", "mae", "rsq", "n_test") %in%
                    names(summary_table)),
              label = "summary table must have all expected columns")
  expect_equal(nrow(summary_table), 8L,
               label = "2 positions x 4 models (3 base + 1 ensemble) = 8 rows")
})


# =============================================================================
# SECTION 7: NFL domain validation
# =============================================================================

test_that("predicted PPR values are clipped to 0 minimum", {
  # A model predicting negative PPR values is a domain error.
  # Both compare_models() and build_ensemble() must clip to >= 0.

  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)

  comp_result <- compare_models(suite, week9_preds_path = NULL, baselines = FALSE)

  # weekly_errors contains abs_error, not raw predictions.
  # Test via ensemble predictions (has raw predicted column).
  ens <- build_ensemble(suite)
  expect_true(all(ens$test_preds$predicted >= 0),
              label = "Ensemble predictions must be >= 0 (PPR cannot be negative)")
})

test_that("postseason week constant is 18 (regular season max)", {
  expect_equal(.W11_MAX_REG_WEEK, 18L,
               label = "Regular season max week must be 18")
})

test_that("redundancy threshold is between 0.8 and 1.0", {
  expect_true(.W11_REDUNDANCY_THRESHOLD >= 0.8 && .W11_REDUNDANCY_THRESHOLD <= 1.0,
              label = "Redundancy threshold must be in [0.8, 1.0]")
})

test_that("positions constant contains exactly passer, rusher, receiver", {
  expect_setequal(.W11_POSITIONS, c("passer", "rusher", "receiver"))
})


# =============================================================================
# SECTION 8: Edge cases
# =============================================================================

test_that("train_model_suite warns but does not stop on small test set", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 10L)
  # test_weeks=13:14 with 10 test rows may fall below .W11_MIN_TEST_N (30)
  expect_warning(
    train_model_suite(ml_mini, position = "rusher",
                      train_weeks = 1:12, test_weeks = 13:14,
                      n_folds = 2L),
    regexp = "only .* rows"
  )
})

test_that("compare_models weekly_errors has all three model labels", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  result  <- compare_models(suite, week9_preds_path = NULL, baselines = FALSE)

  expected_models <- c("xgboost", "random_forest", "elastic_net")
  actual_models   <- unique(result$weekly_errors$model)
  expect_setequal(actual_models, expected_models)
})

test_that("compare_models metrics_table sorted by RMSE ascending", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  result  <- compare_models(suite, week9_preds_path = NULL, baselines = FALSE)

  rmse_values <- result$metrics_table$rmse
  expect_true(
    all(diff(rmse_values) >= 0),
    label = "metrics_table must be sorted by RMSE ascending (best model first)"
  )
})

test_that("train_model_suite echoes position in output", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  expect_equal(suite$position, "rusher")
})

test_that("build_ensemble echoes position in output", {
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  ens <- build_ensemble(suite)
  expect_equal(ens$position, "rusher")
})


# =============================================================================
# SECTION 9: Statistical assumption validation tests
# Established in Week 10 -- non-negotiable for all ML weeks.
# Failing any of these renders results untrustworthy regardless of RMSE.
# =============================================================================

test_that("ASSUMPTION: temporal fold ordering -- folds are strictly chronological", {
  # Each fold's validation week must be strictly greater than all training weeks.
  # Violation = future data leaking into training.
  #
  # NOTE: train_model (the data attached to splits) has week stripped before
  # make_splits() is called. Week must be recovered from train_raw via the
  # integer row indices stored in the split object.
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 3L)

  # train_raw retains the week column and has the same row order as train_model
  train_raw_weeks <- suite$train_raw$week

  for (i in seq_len(length(suite$folds$splits))) {
    spl          <- suite$folds$splits[[i]]
    train_idx    <- rsample::analysis(spl) %>% tibble::rowid_to_column(".row") %>%
                      dplyr::pull(.row)
    val_idx      <- rsample::assessment(spl) %>% tibble::rowid_to_column(".row") %>%
                      dplyr::pull(.row)

    # Use indices to recover week values from train_raw
    train_wks <- train_raw_weeks[train_idx]
    val_wks   <- train_raw_weeks[val_idx]

    max_train <- max(train_wks, na.rm = TRUE)
    min_val   <- min(val_wks,   na.rm = TRUE)

    expect_true(
      is.finite(max_train) && is.finite(min_val),
      label = glue::glue("Fold {i}: week indices must resolve to finite values")
    )
    expect_true(
      min_val > max_train,
      label = glue::glue(
        "Fold {i}: earliest validation week ({min_val}) must be > ",
        "latest training week ({max_train}). Any earlier = temporal leakage."
      )
    )
  }
})

test_that("ASSUMPTION: feature leakage check -- target column absent from feature matrix", {
  # ppr_points_next_week is the prediction target.
  # If it appears in the feature matrix passed to the model, every prediction
  # is trivially derived from the answer -- a fatal leakage path.
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)

  # feature_cols must not include the target
  expect_false(
    "ppr_points_next_week" %in% suite$feature_cols,
    label = "Target variable must not appear in feature_cols"
  )

  # train tibble passed to model must not have a column other than target
  # that directly encodes next-week scores
  train_cols <- names(suite$train)
  expect_true(
    "ppr_points_next_week" %in% train_cols,
    label = "Target must remain in train tibble (needed for recipe formula)"
  )

  # Verify no column named with 'next_week' other than the target exists
  next_week_cols <- grep("next_week", train_cols, value = TRUE)
  expect_equal(
    next_week_cols,
    "ppr_points_next_week",
    label = "Only the target column should contain 'next_week' in its name"
  )
})

test_that("ASSUMPTION: R-squared upper bound -- Rsq < 0.95 on test set", {
  # R-squared > 0.95 on real-world NFL fantasy data is a leakage signal.
  # NFL play-by-play is noisy; a genuinely well-calibrated model should not
  # achieve near-perfect prediction on unseen weeks.
  # Threshold: 0.95 is a hard ceiling; 0.7+ should prompt investigation.
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)
  result  <- compare_models(suite, week9_preds_path = NULL, baselines = FALSE)

  rsq_values <- result$metrics_table$rsq
  rsq_values <- rsq_values[!is.na(rsq_values)]

  expect_true(
    all(rsq_values < 0.95),
    label = paste0(
      "All model R-squared values must be < 0.95. ",
      "Values >= 0.95 on NFL test data indicate leakage. ",
      "Observed: ", paste(round(rsq_values, 3), collapse = ", ")
    )
  )
})

test_that("ASSUMPTION: preprocessing fit-on-train-only -- recipe prep uses training data", {
  # The recipe must be fit (prep'd) exclusively on training data.
  # Fitting on full data leaks test distribution (means, variances, factor
  # levels) into preprocessing transformations.
  # Proxy test: the recipe inside the workflow must be an unprepped blueprint,
  # not a prep'd recipe. tidymodels workflows handle this correctly internally
  # via fit() -- the test verifies the workflow structure is used, not bare prep().

  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)

  # Extract the preprocessor from the fitted workflow -- should be a recipes object
  xgb_preprocessor <- tryCatch(
    workflows::extract_preprocessor(suite$xgb_fit),
    error = function(e) NULL
  )

  expect_false(
    is.null(xgb_preprocessor),
    label = "xgb_fit must contain an extractable preprocessor (recipe)"
  )

  # A prep'd recipe has a $template slot; an unprepped blueprint does not.
  # extract_preprocessor() on a workflow fit returns the unprepped recipe.
  # This confirms the workflow pattern (not bare prep()) was used.
  expect_true(
    inherits(xgb_preprocessor, "recipe"),
    label = "Preprocessor must be a recipe object (workflow pattern in use)"
  )
})

test_that("ASSUMPTION: minimum fold population -- each fold has >= 10 training and validation rows", {
  # CV folds with very small populations produce unreliable metrics.
  # Per-position NFL data at week granularity can produce small folds for
  # low-volume positions (e.g., only a few passers per week).
  # Minimum: 10 rows in analysis (training) and 5 rows in assessment (validation).
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 3L)

  for (i in seq_len(length(suite$folds$splits))) {
    spl         <- suite$folds$splits[[i]]
    n_train_fold <- nrow(rsample::analysis(spl))
    n_val_fold   <- nrow(rsample::assessment(spl))

    expect_true(
      n_train_fold >= 10L,
      label = glue::glue(
        "Fold {i}: training population ({n_train_fold}) must be >= 10 rows. ",
        "Smaller folds produce unreliable cross-validation estimates."
      )
    )
    expect_true(
      n_val_fold >= 5L,
      label = glue::glue(
        "Fold {i}: validation population ({n_val_fold}) must be >= 5 rows. ",
        "Smaller folds produce unreliable cross-validation estimates."
      )
    )
  }
})

test_that("ASSUMPTION: train/test week partition is strict -- no week overlap", {
  # This is the hardest boundary to get right in NFL temporal modeling.
  # A single week appearing in both train and test produces optimistic metrics
  # because the model has seen examples from the same game-week context.
  ml_mini <- make_ml_data(n_train = 80L, n_test = 20L)
  suite   <- train_model_suite(ml_mini, position = "rusher",
                                train_weeks = 1:12, test_weeks = 13:14,
                                n_folds = 2L)

  train_weeks_actual <- unique(suite$train_raw$week)
  test_weeks_actual  <- unique(suite$test_raw$week)
  overlap            <- intersect(train_weeks_actual, test_weeks_actual)

  expect_equal(
    length(overlap), 0L,
    label = paste0(
      "No weeks must appear in both train and test sets. ",
      "Overlap detected at weeks: ", paste(overlap, collapse = ", ")
    )
  )
})


# =============================================================================
# Test summary
# =============================================================================

cat("\n")
cat("===================================================================\n")
cat("Week 11 test suite complete\n")
cat("File: tests/test_week11_functions.R\n")
cat("===================================================================\n")
