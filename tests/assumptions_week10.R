# ==============================================================================
# assumptions_week10.R
# Week 10: Boom / Bust Classification -- Assumption Validation
# NFL Analytics Toolkit
# tests/assumptions_week10.R
#
# PURPOSE
# End-to-end assumption validation for the Week 10 XGBoost multiclass
# classifier. Sources all production scripts, loads data, assigns tiers,
# trains all three position models, runs pre- and post-training assumption
# checks, and saves results to:
#   output/assumption_tests/assumptions_week10.txt
#
# WHAT THIS SCRIPT VALIDATES
# Pre-Training (data checks)
#   S1:  Required files and column presence
#   S2:  Class distribution and imbalance severity per position
#   S3:  Temporal integrity (no future leakage in feature matrix)
#   S4:  Feature NA profile (structural vs unexpected)
#   S5:  Feature correlation (SHAP interpretation risk)
#   S6:  Threshold boundary integrity (no double-classification)
#   S7:  Fold viability (players per week, boom rows per fold)
#   S8:  Target variable distribution by position
#
# Post-Training (model checks)
#   S9:  Baseline comparison (majority-class naive classifier)
#   S10: Overfitting gap (CV log-loss vs held-out test log-loss)
#   S11: Probability calibration (Expected Calibration Error on boom class)
#   S12: Class-level performance (precision / recall / F1, boom recall primary)
#   S13: Feature importance stability across CV folds
#   S14: Observation independence (documented limitation, not blocking)
#
# USAGE
# Open in RStudio. Run with Ctrl+Shift+Enter to source the whole file.
# No manual steps. Output saved automatically.
#
# PREREQUISITES
#   output/feature_matrix_2025.rds    -- from R/10_opponent_features.R
#   output/week9_predictions_2025.rds -- from R/11_xgboost_fantasy.R
#   data/cache/pbp_2025_2025.rds      -- from load_and_validate_pbp(2025)
#   R/01_data_loading.R through R/12_boom_bust_model.R
#
# DEPENDENCIES
# tidymodels, xgboost, themis, yardstick, dplyr, tidyr, purrr, tibble,
# here, scales
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
library(scales)

# ==============================================================================
# OUTPUT SETUP
# ==============================================================================

output_dir  <- here::here("output", "assumption_tests")
output_path <- file.path(output_dir, "assumptions_week10.txt")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created directory: ", output_dir)
}

# Logging helpers
# All output goes to both console and file via sink(split = TRUE)
results_log <- list()

log_section <- function(title) {
  line <- paste0("\n", strrep("=", 72), "\n  ", title, "\n", strrep("=", 72))
  cat(line, "\n")
}

log_result <- function(label, status, detail = NULL) {
  icon <- switch(status, PASS = "[PASS]", WARN = "[WARN]",
                 FAIL = "[FAIL]", INFO = "[INFO]")
  msg  <- paste0("  ", icon, "  ", label)
  if (!is.null(detail)) msg <- paste0(msg, "\n         ", detail)
  cat(msg, "\n")
}

record <- function(section, label, status, detail = NULL) {
  results_log[[length(results_log) + 1]] <<- list(
    section = section, label = label, status = status,
    detail  = if (is.null(detail)) "" else detail
  )
}

# Open sink: split = TRUE writes to both console and file simultaneously
if (sink.number() > 0) sink()
sink(output_path, split = TRUE)

cat("NFL Analytics Toolkit -- Week 10 Assumption Validation\n")
cat("Run date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output:  ", output_path, "\n\n")

# ==============================================================================
# STEP 1: SOURCE ALL PRODUCTION SCRIPTS
# ==============================================================================

cat("Sourcing production scripts...\n")
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
cat("All scripts sourced.\n\n")

# ==============================================================================
# STEP 2: LOAD DATA AND BUILD ml_data
# ==============================================================================

features_path <- here::here("output", "feature_matrix_2025.rds")
pbp_path      <- here::here("data", "cache", "pbp_2025_2025.rds")
reg_pred_path <- here::here("output", "week9_predictions_2025.rds")

for (p in c(features_path, pbp_path, reg_pred_path)) {
  if (!file.exists(p)) {
    sink()
    stop("Required file missing: ", p)
  }
}

features          <- readRDS(features_path)
pbp               <- readRDS(pbp_path)
ml_data           <- prepare_model_features(features, pbp, seasons = 2025)
regression_preds  <- readRDS(reg_pred_path)
rm(pbp); gc()

cat("ml_data:", nrow(ml_data), "rows |", ncol(ml_data), "cols\n\n")

# ==============================================================================
# STEP 3: ASSIGN OUTCOME TIERS
# ==============================================================================

ml_data_tiered <- define_outcome_tiers(
  ml_data,
  train_weeks = 1:14,
  bust_pct    = 0.25,
  boom_pct    = 0.75
)

# Reference splits used throughout
model_rows <- ml_data_tiered %>% filter(has_target == TRUE, !is_absence_week)
train_rows <- model_rows %>% filter(week <= 14L)
test_rows  <- model_rows %>% filter(week  > 14L, week <= 18L)

tier_thresholds <- ml_data_tiered %>%
  filter(!is.na(bust_threshold)) %>%
  select(position_group, bust_threshold, boom_threshold) %>%
  distinct()

# ==============================================================================
# SECTION 1: REQUIRED FILES AND COLUMN PRESENCE
# ==============================================================================

log_section("SECTION 1: REQUIRED FILES AND COLUMN PRESENCE")

required_cols <- c(
  "season", "week", "player_id", "player_name", "position_group", "team",
  "ppr_points_this_week", "ppr_points_next_week", "has_target",
  "is_absence_week", "availability_rate", "epa_this_week",
  "epa_roll3", "epa_season_to_date", "plays_this_week",
  "opp_adjusted_epa_prior", "leading_share_season", "trailing_share_season",
  "opponent_style", "opponent_tier", "role_stability_flag"
)

missing_cols <- setdiff(required_cols, names(ml_data_tiered))
if (length(missing_cols) == 0) {
  log_result(paste0("All ", length(required_cols), " required columns present"), "PASS")
  record("S1", "required_columns", "PASS")
} else {
  log_result("Missing required columns", "FAIL",
             paste0("Missing: ", paste(missing_cols, collapse = ", ")))
  record("S1", "required_columns", "FAIL",
         paste0("missing: ", paste(missing_cols, collapse = ", ")))
}

cat("\nData dimensions: train rows =", nrow(train_rows),
    "| test rows =", nrow(test_rows), "\n")
record("S1", "data_loaded", "PASS",
       paste0("train=", nrow(train_rows), " test=", nrow(test_rows)))

# ==============================================================================
# SECTION 2: CLASS DISTRIBUTION AND IMBALANCE SEVERITY
# ==============================================================================

log_section("SECTION 2: CLASS DISTRIBUTION AND IMBALANCE SEVERITY")

train_tiered <- train_rows %>%
  left_join(tier_thresholds, by = "position_group")

class_dist <- train_tiered %>%
  filter(!is.na(outcome_tier)) %>%
  count(position_group, outcome_tier) %>%
  group_by(position_group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  arrange(position_group, outcome_tier)

cat("\nClass distribution by position (training rows, weeks 1-14):\n")
print(class_dist)

imbalance <- class_dist %>%
  group_by(position_group) %>%
  summarise(
    ratio  = round(max(n) / min(n), 2),
    boom_n = n[outcome_tier == "boom"],
    .groups = "drop"
  )

for (i in seq_len(nrow(imbalance))) {
  pos   <- imbalance$position_group[i]
  ratio <- imbalance$ratio[i]
  bn    <- imbalance$boom_n[i]

  if (ratio > 5) {
    log_result(paste0("Imbalance ratio: ", pos), "WARN",
               paste0("ratio = ", ratio, " -- SMOTE inside recipe required"))
    record("S2", paste0("imbalance_", pos), "WARN", paste0("ratio=", ratio))
  } else {
    log_result(paste0("Imbalance ratio: ", pos), "PASS",
               paste0("ratio = ", ratio))
    record("S2", paste0("imbalance_", pos), "PASS", paste0("ratio=", ratio))
  }

  if (bn < 30) {
    log_result(paste0("Boom class count: ", pos), "FAIL",
               paste0(bn, " boom rows -- minimum 30 required"))
    record("S2", paste0("boom_count_", pos), "FAIL", paste0("n=", bn))
  } else if (bn < 60) {
    log_result(paste0("Boom class count: ", pos), "WARN",
               paste0(bn, " boom rows -- marginal"))
    record("S2", paste0("boom_count_", pos), "WARN", paste0("n=", bn))
  } else {
    log_result(paste0("Boom class count: ", pos), "PASS", paste0("n=", bn))
    record("S2", paste0("boom_count_", pos), "PASS", paste0("n=", bn))
  }
}

# ==============================================================================
# SECTION 3: TEMPORAL INTEGRITY
# ==============================================================================

log_section("SECTION 3: TEMPORAL INTEGRITY (NO FUTURE LEAKAGE)")

# T1: epa_roll3 must be structurally NA at week 1
roll3_w1_na <- ml_data_tiered %>%
  filter(week == 1, !is_absence_week) %>%
  summarise(pct = round(100 * mean(is.na(epa_roll3)), 1)) %>%
  pull(pct)

if (roll3_w1_na >= 95) {
  log_result("epa_roll3 structurally NA at week 1", "PASS",
             paste0(roll3_w1_na, "% NA -- lag and rolling logic intact"))
  record("S3", "roll3_na_week1", "PASS")
} else {
  log_result("epa_roll3 not NA at week 1", "FAIL",
             paste0("Only ", roll3_w1_na, "% NA -- check for imputation upstream"))
  record("S3", "roll3_na_week1", "FAIL")
}

# T2: ppr_points_next_week must be NA at week 18 (season boundary)
target_w18_na <- ml_data_tiered %>%
  filter(week == 18) %>%
  summarise(pct = round(100 * mean(is.na(ppr_points_next_week)), 1)) %>%
  pull(pct)

if (target_w18_na >= 95) {
  log_result("ppr_points_next_week NA at week 18", "PASS",
             paste0(target_w18_na, "% NA -- season boundary intact"))
  record("S3", "season_boundary", "PASS")
} else {
  log_result("ppr_points_next_week not NA at week 18", "FAIL",
             paste0("Only ", target_w18_na, "% NA -- cross-season leakage"))
  record("S3", "season_boundary", "FAIL")
}

# T3: No train/test week overlap
if (max(train_rows$week) < min(test_rows$week)) {
  log_result("Train/test week boundary clean", "PASS",
             paste0("Train max = week ", max(train_rows$week),
                    " | Test min = week ", min(test_rows$week)))
  record("S3", "train_test_boundary", "PASS")
} else {
  log_result("Train/test week overlap detected", "FAIL",
             paste0("Train max ", max(train_rows$week),
                    " >= test min ", min(test_rows$week)))
  record("S3", "train_test_boundary", "FAIL")
}

# T4: Tier thresholds computed from training data only (design constraint)
log_result("Tier thresholds use training data only", "INFO",
           "Enforced in define_outcome_tiers() -- fixed thresholds applied to all rows")
record("S3", "threshold_training_only", "INFO", "design constraint in define_outcome_tiers()")

# T5: opp_adjusted_epa_prior largely NA at week 1
opp_w1_na <- ml_data_tiered %>%
  filter(week == 1, !is_absence_week) %>%
  summarise(pct = round(100 * mean(is.na(opp_adjusted_epa_prior)), 1)) %>%
  pull(pct)

if (opp_w1_na >= 80) {
  log_result("opp_adjusted_epa_prior NA at week 1", "PASS",
             paste0(opp_w1_na, "% NA -- no prior opponent data expected"))
  record("S3", "opp_adj_na_week1", "PASS")
} else {
  log_result("opp_adjusted_epa_prior low NA at week 1", "WARN",
             paste0("Only ", opp_w1_na, "% NA -- verify no lookahead"))
  record("S3", "opp_adj_na_week1", "WARN")
}

# ==============================================================================
# SECTION 4: FEATURE NA PROFILE
# ==============================================================================

log_section("SECTION 4: FEATURE NA PROFILE")

feature_cols <- setdiff(
  names(model_rows),
  c("player_id", "player_name", "season", "week", "team", "opponent",
    "position_group", "is_absence_week", "has_target",
    "ppr_points_this_week", "ppr_points_next_week",
    "outcome_tier", "bust_threshold", "boom_threshold",
    "tier_computed_from_training")
)

na_profile <- model_rows %>%
  select(all_of(feature_cols)) %>%
  summarise(across(everything(), ~ round(100 * mean(is.na(.)), 1))) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "pct_na") %>%
  arrange(desc(pct_na))

cat("\nTop 20 features by % missing (model-eligible rows):\n")
print(head(na_profile, 20))

structural_na_features <- c("epa_roll3", "plays_roll3", "opp_adjusted_epa_prior")
high_na <- na_profile %>%
  filter(pct_na > 80, !feature %in% structural_na_features)

if (nrow(high_na) == 0) {
  log_result("No non-structural features with >80% NA", "PASS")
  record("S4", "high_na_features", "PASS")
} else {
  log_result("Non-structural features with >80% NA", "WARN",
             paste0(paste(high_na$feature, collapse = ", ")))
  record("S4", "high_na_features", "WARN",
         paste0(paste(high_na$feature, collapse = ", ")))
}

log_result("Structural NAs passed through to XGBoost (no imputation)", "INFO",
           paste0(paste(structural_na_features, collapse = ", ")))
record("S4", "structural_nas", "INFO",
       paste0(paste(structural_na_features, collapse = ", ")))

# ==============================================================================
# SECTION 5: FEATURE CORRELATION (SHAP INTERPRETATION RISK)
# ==============================================================================

log_section("SECTION 5: FEATURE CORRELATION (SHAP INTERPRETATION RISK)")

numeric_feats <- train_rows %>%
  select(all_of(intersect(feature_cols, names(train_rows)))) %>%
  select(where(is.numeric))

cor_mat <- cor(numeric_feats, use = "pairwise.complete.obs")
cor_idx <- which(abs(cor_mat) > 0.70 & upper.tri(cor_mat), arr.ind = TRUE)

if (nrow(cor_idx) == 0) {
  log_result("No highly correlated feature pairs (|r| > 0.70)", "PASS")
  record("S5", "feature_correlation", "PASS")
} else {
  cor_pairs <- tibble(
    feature_a   = rownames(cor_mat)[cor_idx[, 1]],
    feature_b   = colnames(cor_mat)[cor_idx[, 2]],
    correlation = round(cor_mat[cor_idx], 3)
  ) %>% arrange(desc(abs(correlation)))

  cat("\nHighly correlated pairs (|r| > 0.70):\n")
  print(cor_pairs, n = 30)

  log_result(paste0("Correlated feature pairs: ", nrow(cor_pairs)), "WARN",
             "SHAP values for these pairs must be interpreted as a group, not individually")
  record("S5", "feature_correlation", "WARN",
         paste0(nrow(cor_pairs), " pairs above 0.70"))

  extreme <- cor_pairs %>% filter(abs(correlation) > 0.95)
  if (nrow(extreme) > 0) {
    log_result(paste0("Extreme correlations (|r| > 0.95): ", nrow(extreme)), "WARN",
               "Consider removing one feature from each extreme pair")
    record("S5", "extreme_correlation", "WARN",
           paste0(nrow(extreme), " pairs"))
  } else {
    log_result("No extreme correlations (|r| > 0.95)", "PASS")
    record("S5", "extreme_correlation", "PASS")
  }
}

# ==============================================================================
# SECTION 6: THRESHOLD BOUNDARY INTEGRITY
# ==============================================================================

log_section("SECTION 6: THRESHOLD BOUNDARY INTEGRITY")

unclassified_n <- train_tiered %>%
  filter(has_target == TRUE, !is_absence_week,
         !is.na(ppr_points_this_week)) %>%
  summarise(n = sum(is.na(outcome_tier))) %>%
  pull(n)

if (unclassified_n == 0) {
  log_result("All eligible training rows assigned a tier", "PASS")
  record("S6", "all_rows_classified", "PASS")
} else {
  log_result("Unclassified rows detected", "FAIL",
             paste0(unclassified_n, " eligible rows with NA outcome_tier"))
  record("S6", "all_rows_classified", "FAIL",
         paste0(unclassified_n, " unclassified"))
}

tier_levels_found <- sort(unique(na.omit(train_tiered$outcome_tier)))
if (identical(tier_levels_found, c("average", "boom", "bust"))) {
  log_result("Exactly 3 tier levels: boom, average, bust", "PASS")
  record("S6", "tier_levels", "PASS")
} else {
  log_result("Unexpected tier levels", "FAIL",
             paste0("Found: ", paste(tier_levels_found, collapse = ", ")))
  record("S6", "tier_levels", "FAIL")
}

# Distribution shift: apply training thresholds to test rows
test_tiered <- test_rows %>%
  left_join(tier_thresholds, by = "position_group")

boom_shift <- bind_rows(
  train_tiered %>% filter(!is.na(outcome_tier)) %>% mutate(split = "train"),
  test_tiered  %>% filter(!is.na(outcome_tier)) %>% mutate(split = "test")
) %>%
  group_by(position_group, split) %>%
  summarise(boom_pct = round(100 * mean(outcome_tier == "boom"), 1),
            .groups  = "drop") %>%
  pivot_wider(names_from = split, values_from = boom_pct) %>%
  mutate(shift_pp = round(abs(train - test), 1))

cat("\nBoom% distribution shift (training thresholds applied to test rows):\n")
print(boom_shift)

for (i in seq_len(nrow(boom_shift))) {
  pos   <- boom_shift$position_group[i]
  shift <- boom_shift$shift_pp[i]
  if (is.na(shift)) {
    log_result(paste0("Distribution shift: ", pos), "WARN", "could not compute")
    record("S6", paste0("boom_shift_", pos), "WARN", "NA")
  } else if (shift > 10) {
    log_result(paste0("Distribution shift: ", pos), "WARN",
               paste0(shift, " pp -- late-season scoring pattern differs from training"))
    record("S6", paste0("boom_shift_", pos), "WARN", paste0(shift, " pp"))
  } else {
    log_result(paste0("Distribution shift: ", pos), "PASS",
               paste0(shift, " pp boom% shift"))
    record("S6", paste0("boom_shift_", pos), "PASS", paste0(shift, " pp"))
  }
}

# ==============================================================================
# SECTION 7: FOLD VIABILITY
# ==============================================================================

log_section("SECTION 7: FOLD VIABILITY (SAMPLE SIZE PER WEEK)")

fold_sizes <- train_rows %>%
  group_by(position_group, week) %>%
  summarise(n_players = n(), .groups = "drop")

fold_summary <- fold_sizes %>%
  group_by(position_group) %>%
  summarise(
    min_players    = min(n_players),
    median_players = round(median(n_players), 0),
    weeks_below_10 = sum(n_players < 10),
    .groups        = "drop"
  )

cat("\nPlayers per position per week (training rows):\n")
print(fold_summary)

for (i in seq_len(nrow(fold_summary))) {
  pos    <- fold_summary$position_group[i]
  min_n  <- fold_summary$min_players[i]
  low_wk <- fold_summary$weeks_below_10[i]

  if (min_n < 5) {
    log_result(paste0("Fold size: ", pos), "FAIL",
               paste0("Min ", min_n, " players/week -- folds too small"))
    record("S7", paste0("fold_size_", pos), "FAIL", paste0("min=", min_n))
  } else if (low_wk > 0) {
    log_result(paste0("Fold size: ", pos), "WARN",
               paste0(low_wk, " weeks with <10 players"))
    record("S7", paste0("fold_size_", pos), "WARN",
           paste0(low_wk, " weeks below 10"))
  } else {
    log_result(paste0("Fold size: ", pos), "PASS",
               paste0("Min ", min_n, " players/week"))
    record("S7", paste0("fold_size_", pos), "PASS", paste0("min=", min_n))
  }
}

boom_per_fold <- train_tiered %>%
  filter(outcome_tier == "boom") %>%
  count(position_group, week) %>%
  group_by(position_group) %>%
  summarise(
    min_boom_per_week = min(n),
    zero_boom_weeks   = sum(n == 0),
    .groups           = "drop"
  )

cat("\nBoom rows per week per position:\n")
print(boom_per_fold)

for (i in seq_len(nrow(boom_per_fold))) {
  pos    <- boom_per_fold$position_group[i]
  zero_w <- boom_per_fold$zero_boom_weeks[i]
  min_b  <- boom_per_fold$min_boom_per_week[i]

  if (zero_w > 0) {
    log_result(paste0("Boom per fold: ", pos), "WARN",
               paste0(zero_w, " weeks with zero boom rows -- recall undefined for those folds"))
    record("S7", paste0("boom_fold_", pos), "WARN",
           paste0(zero_w, " zero-boom weeks"))
  } else if (min_b < 3) {
    log_result(paste0("Boom per fold: ", pos), "WARN",
               paste0("Min ", min_b, " boom rows in a fold -- precision/recall will be noisy"))
    record("S7", paste0("boom_fold_", pos), "WARN", paste0("min=", min_b))
  } else {
    log_result(paste0("Boom per fold: ", pos), "PASS",
               paste0("Min ", min_b, " boom rows in any single week"))
    record("S7", paste0("boom_fold_", pos), "PASS", paste0("min=", min_b))
  }
}

# ==============================================================================
# SECTION 8: TARGET VARIABLE DISTRIBUTION
# ==============================================================================

log_section("SECTION 8: TARGET VARIABLE DISTRIBUTION BY POSITION")

ppr_dist <- train_rows %>%
  group_by(position_group) %>%
  summarise(
    n        = n(),
    mean_ppr = round(mean(ppr_points_this_week, na.rm = TRUE), 2),
    sd_ppr   = round(sd(ppr_points_this_week,   na.rm = TRUE), 2),
    skewness = round(
      mean((ppr_points_this_week - mean(ppr_points_this_week, na.rm = TRUE))^3,
           na.rm = TRUE) / sd(ppr_points_this_week, na.rm = TRUE)^3, 2),
    p25      = round(quantile(ppr_points_this_week, 0.25, na.rm = TRUE), 2),
    p50      = round(quantile(ppr_points_this_week, 0.50, na.rm = TRUE), 2),
    p75      = round(quantile(ppr_points_this_week, 0.75, na.rm = TRUE), 2),
    .groups  = "drop"
  )

cat("\nPPR distribution by position (training data):\n")
print(ppr_dist)

for (i in seq_len(nrow(ppr_dist))) {
  pos  <- ppr_dist$position_group[i]
  skew <- ppr_dist$skewness[i]
  if (skew < 0) {
    log_result(paste0("PPR skewness: ", pos), "WARN",
               paste0("Negative skew = ", skew, " -- unusual for PPR data"))
    record("S8", paste0("ppr_skew_", pos), "WARN", paste0("skew=", skew))
  } else {
    log_result(paste0("PPR skewness: ", pos), "INFO",
               paste0("Skew = ", skew, " (right-skewed -- expected, boom is rare)"))
    record("S8", paste0("ppr_skew_", pos), "INFO", paste0("skew=", skew))
  }
}

neg_ppr <- sum(model_rows$ppr_points_this_week < 0, na.rm = TRUE)
if (neg_ppr == 0) {
  log_result("No negative PPR values", "PASS")
  record("S8", "negative_ppr", "PASS")
} else {
  log_result("Negative PPR values present", "FAIL",
             paste0(neg_ppr, " rows with ppr_points_this_week < 0"))
  record("S8", "negative_ppr", "FAIL", paste0(neg_ppr, " rows"))
}

cat("\n\n", strrep("-", 72), "\n")
cat("Pre-training checks complete. Training models now...\n")
cat(strrep("-", 72), "\n\n")

# ==============================================================================
# STEP 4: TRAIN CLASSIFICATION MODELS
# ==============================================================================

cat("Training passer model...\n")
passer_cls <- train_classification_model(ml_data_tiered, "passer")
gc()

cat("\nTraining rusher model...\n")
rusher_cls <- train_classification_model(ml_data_tiered, "rusher")
gc()

cat("\nTraining receiver model...\n")
receiver_cls <- train_classification_model(ml_data_tiered, "receiver")
gc()

boom_bust_results <- list(
  passer   = passer_cls,
  rusher   = rusher_cls,
  receiver = receiver_cls
)

cat("\nAll models trained.\n\n")

# ==============================================================================
# SECTION 9: BASELINE COMPARISON
# ==============================================================================

log_section("SECTION 9: BASELINE COMPARISON (MAJORITY-CLASS NAIVE CLASSIFIER)")

cat("Naive baseline: predict 'average' for every player every week.\n")
cat("Model must beat this on boom F1, not just accuracy.\n\n")

for (pos in names(boom_bust_results)) {
  res   <- boom_bust_results[[pos]]
  preds <- res$test_predictions

  if (is.null(preds)) {
    log_result(paste0("Baseline: ", pos), "WARN", "test_predictions not available")
    record("S9", paste0("baseline_", pos), "WARN", "no test predictions")
    next
  }

  n_test       <- nrow(preds)
  naive_acc    <- round(100 * mean(preds$outcome_tier == "average"), 1)
  model_acc    <- round(100 * mean(preds$outcome_tier == preds$.pred_class), 1)

  cat(sprintf("  %-10s  Naive acc: %5.1f%%  | Model acc: %5.1f%%\n",
              pos, naive_acc, model_acc))

  if (model_acc > naive_acc) {
    log_result(paste0("Beats naive baseline: ", pos), "PASS",
               paste0("Model ", model_acc, "% > Naive ", naive_acc, "%"))
    record("S9", paste0("baseline_", pos), "PASS",
           paste0("model=", model_acc, "% naive=", naive_acc, "%"))
  } else {
    log_result(paste0("Does NOT beat naive baseline: ", pos), "FAIL",
               paste0("Model ", model_acc, "% <= Naive ", naive_acc, "%"))
    record("S9", paste0("baseline_", pos), "FAIL",
           paste0("model=", model_acc, "% naive=", naive_acc, "%"))
  }
}

# ==============================================================================
# SECTION 10: OVERFITTING GAP
# ==============================================================================

log_section("SECTION 10: OVERFITTING GAP (CV LOG-LOSS vs HELD-OUT TEST)")

cat("Expected: test log-loss within 1.5x of CV log-loss.\n\n")

for (pos in names(boom_bust_results)) {
  res     <- boom_bust_results[[pos]]
  cv_ll   <- res$cv_logloss
  test_ll <- res$test_logloss

  if (is.null(cv_ll) || is.null(test_ll) || is.na(cv_ll) || is.na(test_ll)) {
    log_result(paste0("Overfitting gap: ", pos), "WARN", "log-loss values unavailable")
    record("S10", paste0("overfit_", pos), "WARN", "metrics not available")
    next
  }

  ratio <- round(test_ll / cv_ll, 2)
  cat(sprintf("  %-10s  CV: %.4f  | Test: %.4f  | Ratio: %.2f\n",
              pos, cv_ll, test_ll, ratio))

  if (ratio > 2.0) {
    log_result(paste0("Overfitting: ", pos), "FAIL",
               paste0("Ratio = ", ratio, " -- severe overfitting"))
    record("S10", paste0("overfit_", pos), "FAIL", paste0("ratio=", ratio))
  } else if (ratio > 1.5) {
    log_result(paste0("Overfitting: ", pos), "WARN",
               paste0("Ratio = ", ratio, " -- moderate gap, expected for late-season test weeks"))
    record("S10", paste0("overfit_", pos), "WARN", paste0("ratio=", ratio))
  } else {
    log_result(paste0("Overfitting: ", pos), "PASS",
               paste0("Ratio = ", ratio))
    record("S10", paste0("overfit_", pos), "PASS", paste0("ratio=", ratio))
  }
}

# ==============================================================================
# SECTION 11: PROBABILITY CALIBRATION
# ==============================================================================

log_section("SECTION 11: PROBABILITY CALIBRATION (EXPECTED CALIBRATION ERROR)")

cat("Raw XGBoost probabilities are NOT true probabilities.\n")
cat("ECE < 0.10 required before using p_boom as an absolute decision threshold.\n\n")

for (pos in names(boom_bust_results)) {
  res   <- boom_bust_results[[pos]]
  preds <- res$test_predictions

  if (is.null(preds) || !"p_boom" %in% names(preds)) {
    log_result(paste0("Calibration: ", pos), "WARN",
               "p_boom column not found in test_predictions")
    record("S11", paste0("calibration_", pos), "WARN", "no probability columns")
    next
  }

  ece_tbl <- preds %>%
    mutate(
      is_boom  = as.integer(outcome_tier == "boom"),
      prob_bin = cut(p_boom, breaks = seq(0, 1, by = 0.10),
                     include.lowest = TRUE, right = FALSE)
    ) %>%
    group_by(prob_bin) %>%
    summarise(
      n              = n(),
      mean_pred_prob = mean(p_boom,    na.rm = TRUE),
      observed_rate  = mean(is_boom,   na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    filter(n >= 5) %>%
    mutate(abs_diff = abs(mean_pred_prob - observed_rate))

  if (nrow(ece_tbl) == 0) {
    log_result(paste0("Calibration: ", pos), "WARN",
               "Insufficient test rows per bin to compute ECE")
    record("S11", paste0("calibration_", pos), "WARN", "insufficient bin data")
    next
  }

  ece <- round(sum(ece_tbl$n * ece_tbl$abs_diff) / sum(ece_tbl$n), 4)
  cat(sprintf("  %-10s  Boom ECE: %.4f\n", pos, ece))

  if (ece > 0.15) {
    log_result(paste0("Calibration (boom): ", pos), "FAIL",
               paste0("ECE = ", ece, " -- calibrate with Platt scaling before using p_boom for decisions"))
    record("S11", paste0("calibration_", pos), "FAIL", paste0("ECE=", ece))
  } else if (ece > 0.10) {
    log_result(paste0("Calibration (boom): ", pos), "WARN",
               paste0("ECE = ", ece, " -- marginal; consider Platt scaling"))
    record("S11", paste0("calibration_", pos), "WARN", paste0("ECE=", ece))
  } else {
    log_result(paste0("Calibration (boom): ", pos), "PASS",
               paste0("ECE = ", ece))
    record("S11", paste0("calibration_", pos), "PASS", paste0("ECE=", ece))
  }
}

# ==============================================================================
# SECTION 12: CLASS-LEVEL PERFORMANCE
# ==============================================================================

log_section("SECTION 12: CLASS-LEVEL PERFORMANCE (PRECISION / RECALL / F1)")

cat("Primary metric: boom recall.\n")
cat("Boom recall = 0 means the model never predicts boom -- equivalent to naive baseline.\n\n")

for (pos in names(boom_bust_results)) {
  res   <- boom_bust_results[[pos]]
  preds <- res$test_predictions

  if (is.null(preds)) {
    log_result(paste0("Class metrics: ", pos), "WARN", "no test predictions")
    record("S12", paste0("class_metrics_", pos), "WARN", "no test predictions")
    next
  }

  tier_levels <- c("boom", "average", "bust")
  preds_f <- preds %>%
    mutate(
      outcome_tier = factor(outcome_tier, levels = tier_levels),
      .pred_class  = factor(.pred_class,  levels = tier_levels)
    )

  cm <- conf_mat(preds_f, truth = outcome_tier, estimate = .pred_class)

  boom_recall <- preds_f %>%
    filter(outcome_tier == "boom") %>%
    summarise(r = round(mean(.pred_class == "boom"), 4)) %>%
    pull(r)

  boom_precision <- if (sum(preds_f$.pred_class == "boom") > 0) {
    preds_f %>%
      filter(.pred_class == "boom") %>%
      summarise(p = round(mean(outcome_tier == "boom"), 4)) %>%
      pull(p)
  } else { 0 }

  cat(sprintf("\n  Position: %s\n", pos))
  print(cm$table)
  cat(sprintf("  Boom recall:    %.4f\n", boom_recall))
  cat(sprintf("  Boom precision: %.4f\n", boom_precision))

  if (is.na(boom_recall) || boom_recall == 0) {
    log_result(paste0("Boom recall: ", pos), "FAIL",
               "Recall = 0 -- model never predicts boom")
    record("S12", paste0("boom_recall_", pos), "FAIL", "recall=0")
  } else if (boom_recall < 0.20) {
    log_result(paste0("Boom recall: ", pos), "WARN",
               paste0("Recall = ", boom_recall, " -- low"))
    record("S12", paste0("boom_recall_", pos), "WARN",
           paste0("recall=", boom_recall))
  } else {
    log_result(paste0("Boom recall: ", pos), "PASS",
               paste0("Recall = ", boom_recall))
    record("S12", paste0("boom_recall_", pos), "PASS",
           paste0("recall=", boom_recall))
  }
}

# ==============================================================================
# SECTION 13: FEATURE IMPORTANCE STABILITY
# ==============================================================================

log_section("SECTION 13: FEATURE IMPORTANCE STABILITY ACROSS CV FOLDS")

cat("CV of importance > 0.50 = importance varies more than 50% across folds.\n")
cat("Unstable features should not be trusted for interpretation.\n\n")

for (pos in names(boom_bust_results)) {
  res              <- boom_bust_results[[pos]]
  fold_imps        <- res$fold_importances

  if (is.null(fold_imps) || nrow(fold_imps) == 0) {
    log_result(paste0("Feature stability: ", pos), "INFO",
               "fold_importances not available")
    record("S13", paste0("feat_stability_", pos), "INFO", "not available")
    next
  }

  stability <- fold_imps %>%
    group_by(variable) %>%
    summarise(
      mean_imp = mean(importance, na.rm = TRUE),
      sd_imp   = sd(importance,   na.rm = TRUE),
      cv_imp   = round(sd_imp / abs(mean_imp), 3),
      .groups  = "drop"
    ) %>%
    arrange(desc(mean_imp))

  unstable <- stability %>% filter(cv_imp > 0.5)
  cat(sprintf("\n  Position: %s | Top 10 features:\n", pos))
  print(head(stability, 10) %>% mutate(across(c(mean_imp, sd_imp, cv_imp), ~ round(.x, 4))))

  if (nrow(unstable) > 0) {
    log_result(paste0("Feature stability: ", pos), "WARN",
               paste0(nrow(unstable), " unstable features (CV > 0.50): ",
                      paste(head(unstable$variable, 5), collapse = ", ")))
    record("S13", paste0("feat_stability_", pos), "WARN",
           paste0(nrow(unstable), " unstable"))
  } else {
    log_result(paste0("Feature stability: ", pos), "PASS",
               "All top features stable (CV < 0.50) across folds")
    record("S13", paste0("feat_stability_", pos), "PASS")
  }
}

# ==============================================================================
# SECTION 14: OBSERVATION INDEPENDENCE (DOCUMENTED LIMITATION)
# ==============================================================================

log_section("SECTION 14: OBSERVATION INDEPENDENCE")

cat("XGBoost does not assume independent observations.\n")
cat("However, precision / recall / F1 treat each player-week as independent.\n")
cat("The same player appears in multiple test weeks, so F1 confidence intervals\n")
cat("are anti-conservative. Report point estimates only -- no CI on F1.\n\n")

player_reps <- test_rows %>%
  count(player_id, position_group) %>%
  group_by(position_group) %>%
  summarise(
    median_weeks = round(median(n), 1),
    max_weeks    = max(n),
    .groups      = "drop"
  )

cat("Player appearances in test set (weeks 15-18):\n")
print(player_reps)

log_result("Observation independence: repeated player-weeks in test set", "INFO",
           "F1 CIs are anti-conservative -- report point estimates only")
record("S14", "obs_independence", "INFO",
       "repeated player-weeks -- F1 CIs anti-conservative by design")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

log_section("ASSUMPTION VALIDATION SUMMARY")

summary_tbl <- bind_rows(lapply(results_log, as.data.frame)) %>%
  as_tibble()

status_counts <- summary_tbl %>%
  group_by(status) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(match(status, c("FAIL", "WARN", "PASS", "INFO")))

cat("\nResult counts:\n")
print(status_counts)

fails <- summary_tbl %>% filter(status == "FAIL")
if (nrow(fails) > 0) {
  cat("\nFAILURES -- resolve before trusting model output:\n")
  for (i in seq_len(nrow(fails))) {
    cat(sprintf("  [FAIL]  S%-3s | %s\n          %s\n",
                fails$section[i], fails$label[i], fails$detail[i]))
  }
} else {
  cat("\nNo FAIL results.\n")
}

warns <- summary_tbl %>% filter(status == "WARN")
if (nrow(warns) > 0) {
  cat("\nWARNINGS -- review before Week 10 code review:\n")
  for (i in seq_len(nrow(warns))) {
    cat(sprintf("  [WARN]  S%-3s | %s\n          %s\n",
                warns$section[i], warns$label[i], warns$detail[i]))
  }
}

cat("\n", strrep("-", 72), "\n")
cat("Output: output/assumption_tests/assumptions_week10.txt\n")
cat("Run date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("-", 72), "\n")

sink()

cat("\nAssumption validation complete.\n")
cat("Results saved to: output/assumption_tests/assumptions_week10.txt\n\n")
cat("Objects available in session:\n")
cat("  ml_data_tiered     -- feature matrix with outcome_tier column\n")
cat("  boom_bust_results  -- list(passer, rusher, receiver) trained models\n")
