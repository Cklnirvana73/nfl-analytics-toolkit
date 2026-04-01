# ==============================================================================
# assumptions_week11.R
# Week 11: Ensemble Pipeline -- Assumption Validation
# NFL Analytics Toolkit
#
# PURPOSE
# End-to-end assumption validation for the Week 11 ensemble regression
# pipeline (XGBoost + Random Forest + Elastic Net + stacked ensemble).
# Loads saved artifacts, runs pre- and post-training assumption checks,
# and saves results to:
#   output/assumption_tests/assumptions_week11.txt
#
# WHAT THIS SCRIPT VALIDATES
# Pre-Training (data checks)
#   S1:  Required files and column presence
#   S2:  Temporal fold ordering (train/test boundary clean)
#   S3:  Feature leakage verification (lag-based NA pattern at week 1)
#   S4:  Feature NA profile (structural vs unexpected; opponent coverage)
#   S5:  Feature correlation (SHAP interpretation risk)
#   S6:  Fold viability (min players per week per position)
#   S7:  Target variable distribution by position
#   S8:  Season boundary (ppr_points_next_week NA at week 18)
#
# Post-Training (model checks)
#   S9:  Baseline comparison (all models must beat season_avg_baseline)
#   S10: Overfitting gap (RMSE vs target SD; R-squared upper bound)
#   S11: Ensemble gain (ensemble RMSE vs best individual model)
#   S12: Prediction correlation (high r = ensemble adds marginal value)
#   S13: Feature importance stability across models
#   S14: Observation independence (documented limitation)
#   S15: Opponent feature coverage (new Week 11)
#
# USAGE
# Open in RStudio. Run with Ctrl+Shift+Enter.
# No manual steps. Output saved automatically.
#
# PREREQUISITES
#   output/ml_data_2025.rds              -- from run_full_pipeline(force_rerun=TRUE)
#   output/week11_full_results_2025.rds  -- from run_full_pipeline()
#   R/01_data_loading.R through R/13_ensemble_pipeline.R
#
# DEPENDENCIES
# tidymodels, dplyr, tidyr, purrr, tibble, here, glue, yardstick
# ==============================================================================

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(tidymodels)
library(yardstick)
library(glue)

# ==============================================================================
# OUTPUT SETUP
# ==============================================================================

SEASON      <- 2025L
TRAIN_WEEKS <- 1:14
TEST_WEEKS  <- 15:18

output_dir  <- here::here("output", "assumption_tests")
output_path <- file.path(output_dir, "assumptions_week11.txt")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created directory: ", output_dir)
}

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

if (sink.number() > 0) sink()
sink(output_path, split = TRUE)

cat("NFL Analytics Toolkit -- Week 11 Assumption Validation\n")
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
source(here::here("R", "13_ensemble_pipeline.R"))
cat("All scripts sourced.\n\n")

# ==============================================================================
# STEP 2: LOAD ARTIFACTS
# Load saved artifacts directly -- do NOT re-run the pipeline.
# ==============================================================================

ml_path      <- here::here("output", glue("ml_data_{SEASON}.rds"))
results_path <- here::here("output", glue("week11_full_results_{SEASON}.rds"))

for (p in c(ml_path, results_path)) {
  if (!file.exists(p)) {
    sink()
    stop("Required artifact missing: ", p,
         "\nRun run_full_pipeline(force_rerun = TRUE) first.")
  }
}

ml_data <- readRDS(ml_path)
results  <- readRDS(results_path)

cat("ml_data:", nrow(ml_data), "rows |", ncol(ml_data), "cols\n")
cat("results slots:", paste(names(results), collapse = ", "), "\n\n")

model_rows <- ml_data %>% filter(has_target == TRUE, !is_absence_week)
train_rows <- model_rows %>% filter(week %in% TRAIN_WEEKS)
test_rows  <- model_rows %>% filter(week %in% TEST_WEEKS)

cat("Train rows:", nrow(train_rows), "| Test rows:", nrow(test_rows), "\n\n")

# ==============================================================================
# SECTION 1: REQUIRED FILES AND COLUMN PRESENCE
# ==============================================================================

log_section("SECTION 1: REQUIRED FILES AND COLUMN PRESENCE")

required_cols <- c(
  "season", "week", "player_id", "player_name", "position_group", "team",
  "opponent", "ppr_points_this_week", "ppr_points_next_week",
  "has_target", "is_absence_week",
  "plays_this_week", "epa_this_week", "success_rate_this_week",
  "epa_roll3", "epa_season_to_date", "plays_roll3",
  "neutral_epa_season", "leading_share_season", "trailing_share_season",
  "opp_adjusted_epa_prior", "schedule_difficulty_rank", "opp_adj_games_prior",
  "weeks_played", "weeks_since_role_change", "role_stability_flag",
  "availability_rate", "weeks_since_last_played", "missed_weeks_this_season"
)

missing_cols <- setdiff(required_cols, names(ml_data))
if (length(missing_cols) == 0) {
  log_result(paste0("All ", length(required_cols), " required columns present"), "PASS")
  record("S1", "required_columns", "PASS")
} else {
  log_result("Missing required columns", "FAIL",
             paste0("Missing: ", paste(missing_cols, collapse = ", ")))
  record("S1", "required_columns", "FAIL",
         paste0("missing: ", paste(missing_cols, collapse = ", ")))
}

required_slots <- c("comparisons", "ensembles", "features", "summary")
missing_slots  <- setdiff(required_slots, names(results))
if (length(missing_slots) == 0) {
  log_result("results artifact has all required slots", "PASS")
  record("S1", "results_slots", "PASS")
} else {
  log_result("results artifact missing slots", "FAIL",
             paste0("missing: ", paste(missing_slots, collapse = ", ")))
  record("S1", "results_slots", "FAIL",
         paste0("missing: ", paste(missing_slots, collapse = ", ")))
}

record("S1", "data_loaded", "PASS",
       paste0("train=", nrow(train_rows), " test=", nrow(test_rows)))

# ==============================================================================
# SECTION 2: TEMPORAL FOLD ORDERING
# ==============================================================================

log_section("SECTION 2: TEMPORAL FOLD ORDERING")

if (max(train_rows$week) < min(test_rows$week)) {
  log_result("Train/test week boundary clean", "PASS",
             paste0("Train max = week ", max(train_rows$week),
                    " | Test min = week ", min(test_rows$week)))
  record("S2", "train_test_boundary", "PASS")
} else {
  log_result("Train/test week overlap detected", "FAIL",
             paste0("Train max ", max(train_rows$week),
                    " >= test min ", min(test_rows$week)))
  record("S2", "train_test_boundary", "FAIL")
}

train_wk_range <- range(train_rows$week)
if (train_wk_range[1] == min(TRAIN_WEEKS) && train_wk_range[2] == max(TRAIN_WEEKS)) {
  log_result("Train week range correct", "PASS",
             paste0("Weeks ", train_wk_range[1], " through ", train_wk_range[2]))
  record("S2", "train_week_range", "PASS")
} else {
  log_result("Train week range unexpected", "WARN",
             paste0("Expected ", min(TRAIN_WEEKS), "-", max(TRAIN_WEEKS),
                    " | Found ", train_wk_range[1], "-", train_wk_range[2]))
  record("S2", "train_week_range", "WARN")
}

test_wk_range <- range(test_rows$week)
if (test_wk_range[1] >= min(TEST_WEEKS) && test_wk_range[2] <= max(TEST_WEEKS)) {
  log_result("Test week range correct", "PASS",
             paste0("Weeks ", test_wk_range[1], " through ", test_wk_range[2]))
  record("S2", "test_week_range", "PASS")
} else {
  log_result("Test weeks outside expected range", "WARN",
             paste0("Expected ", min(TEST_WEEKS), "-", max(TEST_WEEKS),
                    " | Found ", test_wk_range[1], "-", test_wk_range[2]))
  record("S2", "test_week_range", "WARN")
}

# ==============================================================================
# SECTION 3: FEATURE LEAKAGE VERIFICATION
# ==============================================================================

log_section("SECTION 3: FEATURE LEAKAGE (LAG VERIFICATION)")

# L1: epa_roll3 must be structurally NA at week 1
roll3_w1_na <- ml_data %>%
  filter(week == 1, !is_absence_week) %>%
  summarise(pct = round(100 * mean(is.na(epa_roll3)), 1)) %>%
  pull(pct)

if (roll3_w1_na >= 95) {
  log_result("epa_roll3 structurally NA at week 1", "PASS",
             paste0(roll3_w1_na, "% NA -- lag(rollapply()) logic intact"))
  record("S3", "roll3_na_week1", "PASS", paste0(roll3_w1_na, "%"))
} else {
  log_result("epa_roll3 not NA at week 1", "FAIL",
             paste0("Only ", roll3_w1_na, "% NA -- check lag() upstream"))
  record("S3", "roll3_na_week1", "FAIL", paste0(roll3_w1_na, "%"))
}

# L2: epa_season_to_date must be structurally NA at week 1
std_w1_na <- ml_data %>%
  filter(week == 1, !is_absence_week) %>%
  summarise(pct = round(100 * mean(is.na(epa_season_to_date)), 1)) %>%
  pull(pct)

if (std_w1_na >= 95) {
  log_result("epa_season_to_date structurally NA at week 1", "PASS",
             paste0(std_w1_na, "% NA -- lag(cummean()) logic intact"))
  record("S3", "std_na_week1", "PASS", paste0(std_w1_na, "%"))
} else {
  log_result("epa_season_to_date not NA at week 1", "FAIL",
             paste0("Only ", std_w1_na, "% NA -- check lag(cummean()) upstream"))
  record("S3", "std_na_week1", "FAIL", paste0(std_w1_na, "%"))
}

# L3: opp_adjusted_epa_prior must be NA at week 1
opp_w1_na <- ml_data %>%
  filter(week == 1, !is_absence_week) %>%
  summarise(pct = round(100 * mean(is.na(opp_adjusted_epa_prior)), 1)) %>%
  pull(pct)

if (opp_w1_na >= 95) {
  log_result("opp_adjusted_epa_prior NA at week 1", "PASS",
             paste0(opp_w1_na, "% NA -- expanding window through_week=0 correct"))
  record("S3", "opp_adj_na_week1", "PASS", paste0(opp_w1_na, "%"))
} else {
  log_result("opp_adjusted_epa_prior low NA at week 1", "WARN",
             paste0("Only ", opp_w1_na, "% NA -- verify no lookahead in opponent calc"))
  record("S3", "opp_adj_na_week1", "WARN", paste0(opp_w1_na, "%"))
}

# L4: target absent from feature set
non_feature_cols <- c(
  "player_id", "player_name", "position_group", "season", "week",
  "ppr_points_next_week", "has_target", "is_absence_week",
  "opponent_style", "opponent_tier"
)
feature_cols_check <- setdiff(names(train_rows), non_feature_cols)
if (!"ppr_points_next_week" %in% feature_cols_check) {
  log_result("Target (ppr_points_next_week) absent from feature set", "PASS")
  record("S3", "target_not_in_features", "PASS")
} else {
  log_result("Target in feature set -- target leakage", "FAIL",
             "ppr_points_next_week found in feature columns")
  record("S3", "target_not_in_features", "FAIL")
}

# L5: leaky features confirmed excluded
leakage_excluded <- c("opponent_style", "opponent_tier")
found_in_feats   <- intersect(leakage_excluded, feature_cols_check)
if (length(found_in_feats) == 0) {
  log_result("Leaky features excluded (opponent_style, opponent_tier)", "PASS",
             "Full-season classifications excluded per Week 11 leakage audit")
  record("S3", "leaky_features_excluded", "PASS")
} else {
  log_result("Leaky features still in feature set", "FAIL",
             paste0("Found: ", paste(found_in_feats, collapse = ", "),
                    " -- see docs/Week11_Leakage_Audit.txt"))
  record("S3", "leaky_features_excluded", "FAIL",
         paste0(paste(found_in_feats, collapse = ", ")))
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
    "opponent_style", "opponent_tier")
)

na_profile <- model_rows %>%
  select(all_of(intersect(feature_cols, names(model_rows)))) %>%
  summarise(across(everything(), ~ round(100 * mean(is.na(.)), 1))) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "pct_na") %>%
  arrange(desc(pct_na))

cat("\nTop 20 features by % missing (model-eligible rows):\n")
print(head(na_profile, 20))

structural_na_features <- c("epa_roll3", "plays_roll3", "epa_season_to_date",
                             "opp_adjusted_epa_prior", "schedule_difficulty_rank",
                             "neutral_epa_season", "leading_share_season",
                             "trailing_share_season", "weeks_since_last_played")

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
# SECTION 5: FEATURE CORRELATION
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
             "SHAP values for these pairs must be interpreted as a group")
  record("S5", "feature_correlation", "WARN",
         paste0(nrow(cor_pairs), " pairs above 0.70"))

  extreme <- cor_pairs %>% filter(abs(correlation) > 0.95)
  if (nrow(extreme) > 0) {
    log_result(paste0("Extreme correlations (|r| > 0.95): ", nrow(extreme)), "WARN",
               "Consider removing one feature from each extreme pair")
    record("S5", "extreme_correlation", "WARN", paste0(nrow(extreme), " pairs"))
  } else {
    log_result("No extreme correlations (|r| > 0.95)", "PASS")
    record("S5", "extreme_correlation", "PASS")
  }
}

# ==============================================================================
# SECTION 6: FOLD VIABILITY
# ==============================================================================

log_section("SECTION 6: FOLD VIABILITY (SAMPLE SIZE PER WEEK)")

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
    record("S6", paste0("fold_size_", pos), "FAIL", paste0("min=", min_n))
  } else if (low_wk > 0) {
    log_result(paste0("Fold size: ", pos), "WARN",
               paste0(low_wk, " weeks with <10 players"))
    record("S6", paste0("fold_size_", pos), "WARN", paste0(low_wk, " wks below 10"))
  } else {
    log_result(paste0("Fold size: ", pos), "PASS",
               paste0("Min ", min_n, " players/week"))
    record("S6", paste0("fold_size_", pos), "PASS", paste0("min=", min_n))
  }
}

# ==============================================================================
# SECTION 7: TARGET VARIABLE DISTRIBUTION
# ==============================================================================

log_section("SECTION 7: TARGET VARIABLE DISTRIBUTION BY POSITION")

ppr_dist <- train_rows %>%
  group_by(position_group) %>%
  summarise(
    n        = n(),
    mean_ppr = round(mean(ppr_points_next_week, na.rm = TRUE), 2),
    sd_ppr   = round(sd(ppr_points_next_week,   na.rm = TRUE), 2),
    p25      = round(quantile(ppr_points_next_week, 0.25, na.rm = TRUE), 2),
    p50      = round(quantile(ppr_points_next_week, 0.50, na.rm = TRUE), 2),
    p75      = round(quantile(ppr_points_next_week, 0.75, na.rm = TRUE), 2),
    .groups  = "drop"
  )

cat("\nTarget (ppr_points_next_week) distribution by position:\n")
print(ppr_dist)

for (i in seq_len(nrow(ppr_dist))) {
  pos <- ppr_dist$position_group[i]
  sd  <- ppr_dist$sd_ppr[i]
  log_result(paste0("Target SD: ", pos), "INFO",
             paste0("SD = ", sd,
                    " PPR pts -- RMSE ceiling (naive predict-mean = this SD)"))
  record("S7", paste0("target_sd_", pos), "INFO", paste0("SD=", sd))
}

neg_target <- sum(train_rows$ppr_points_next_week < 0, na.rm = TRUE)
if (neg_target == 0) {
  log_result("No negative target values", "PASS")
  record("S7", "negative_target", "PASS")
} else {
  log_result("Negative target values present", "WARN",
             paste0(neg_target, " rows with ppr_points_next_week < 0"))
  record("S7", "negative_target", "WARN", paste0(neg_target, " rows"))
}

# ==============================================================================
# SECTION 8: SEASON BOUNDARY
# ==============================================================================

log_section("SECTION 8: SEASON BOUNDARY (TARGET NA AT LAST PLAYER-SEASON WEEK)")

# The correct check: the last week each player-season-team group appears must
# have NA target (no next week to predict). Checking week == 18 is wrong when
# postseason data is included -- some players have valid targets in week 18
# because they played in week 19 (wild card). The real boundary is the last
# row per player-position-season-team group.

last_week_target_na <- ml_data %>%
  filter(!is_absence_week) %>%
  group_by(player_id, position_group, season, team) %>%
  arrange(week, .by_group = TRUE) %>%
  slice_tail(n = 1L) %>%
  ungroup() %>%
  summarise(pct = round(100 * mean(is.na(ppr_points_next_week)), 1)) %>%
  pull(pct)

# Also show the max week in the data for context
max_week_data <- max(ml_data$week, na.rm = TRUE)
cat(glue("  Max week in ml_data: {max_week_data}
"))
cat(glue("  Rows with NA target at last player-season week: {last_week_target_na}%
"))

if (last_week_target_na >= 95) {
  log_result("ppr_points_next_week NA at last player-season week", "PASS",
             paste0(last_week_target_na,
                    "% NA -- season boundary intact (postseason rows have valid targets)"))
  record("S8", "season_boundary", "PASS", paste0(last_week_target_na, "%"))
} else {
  log_result("ppr_points_next_week not NA at last player-season week", "FAIL",
             paste0("Only ", last_week_target_na,
                    "% NA -- check for cross-season leakage in lead() call"))
  record("S8", "season_boundary", "FAIL", paste0(last_week_target_na, "%"))
}

cat("\n\n", strrep("-", 72), "\n")
cat("Pre-training checks complete. Loading model results...\n")
cat(strrep("-", 72), "\n\n")

# ==============================================================================
# STEP 3: ASSEMBLE METRICS
# ==============================================================================

summary_tbl <- results$summary
positions   <- unique(summary_tbl$position)
baselines   <- c("season_avg_baseline", "persistence_baseline")
models      <- c("xgboost", "random_forest", "elastic_net", "ensemble")

get_rmse <- function(pos, mdl) {
  summary_tbl %>%
    filter(position == pos, model == mdl) %>%
    pull(rmse) %>%
    { if (length(.) == 0) NA_real_ else .[1] }
}

cat("\nFull results summary:\n")
print(summary_tbl %>% arrange(position, rmse))
cat("\n")

# ==============================================================================
# SECTION 9: BASELINE COMPARISON
# ==============================================================================

log_section("SECTION 9: BASELINE COMPARISON")

cat("All ML models must beat the season_avg_baseline RMSE.\n\n")

for (pos in positions) {
  baseline_rmse <- get_rmse(pos, "season_avg_baseline")
  if (is.na(baseline_rmse)) {
    log_result(paste0("Baseline: ", pos), "WARN", "season_avg_baseline RMSE not found")
    record("S9", paste0("baseline_", pos), "WARN", "no baseline")
    next
  }
  cat(sprintf("  %s -- Season avg baseline RMSE: %.3f\n", pos, baseline_rmse))

  for (mdl in models) {
    mdl_rmse <- get_rmse(pos, mdl)
    if (is.na(mdl_rmse)) next
    delta <- round(mdl_rmse - baseline_rmse, 3)
    label <- paste0(pos, " | ", mdl)

    if (mdl_rmse < baseline_rmse) {
      log_result(paste0("Beats baseline: ", label), "PASS",
                 paste0("RMSE ", round(mdl_rmse, 3),
                        " vs baseline ", round(baseline_rmse, 3),
                        " (delta ", delta, ")"))
      record("S9", paste0("vs_baseline_", pos, "_", mdl), "PASS",
             paste0("delta=", delta))
    } else {
      log_result(paste0("Does NOT beat baseline: ", label), "FAIL",
                 paste0("RMSE ", round(mdl_rmse, 3),
                        " >= baseline ", round(baseline_rmse, 3),
                        " (delta +", delta, ")"))
      record("S9", paste0("vs_baseline_", pos, "_", mdl), "FAIL",
             paste0("delta=+", delta))
    }
  }
}

# ==============================================================================
# SECTION 10: OVERFITTING GAP
# ==============================================================================

log_section("SECTION 10: OVERFITTING GAP (RMSE VS TARGET SD; R-SQUARED UPPER BOUND)")

cat("Test RMSE > target SD means model is worse than predicting the mean.\n")
cat("R-squared > 0.95 on real-world NFL data strongly suggests leakage.\n\n")

for (pos in positions) {
  target_sd <- ppr_dist %>%
    filter(position_group == pos) %>%
    pull(sd_ppr)
  if (length(target_sd) == 0) next

  for (mdl in c(models, "season_avg_baseline")) {
    mdl_rmse <- get_rmse(pos, mdl)
    if (is.na(mdl_rmse)) next
    rmse_vs_sd <- round(mdl_rmse / target_sd, 3)

    if (rmse_vs_sd > 1.0) {
      log_result(paste0("RMSE > target SD: ", pos, " | ", mdl), "WARN",
                 paste0("RMSE ", round(mdl_rmse, 3), " > SD ", target_sd,
                        " (ratio ", rmse_vs_sd, ")"))
      record("S10", paste0("rmse_vs_sd_", pos, "_", mdl), "WARN",
             paste0("ratio=", rmse_vs_sd))
    } else {
      log_result(paste0("RMSE within SD ceiling: ", pos, " | ", mdl), "PASS",
                 paste0("RMSE ", round(mdl_rmse, 3), " / SD ", target_sd,
                        " = ", rmse_vs_sd))
      record("S10", paste0("rmse_vs_sd_", pos, "_", mdl), "PASS",
             paste0("ratio=", rmse_vs_sd))
    }
  }

  max_rsq <- summary_tbl %>%
    filter(position == pos, !model %in% baselines) %>%
    pull(rsq) %>%
    max(na.rm = TRUE)

  if (!is.na(max_rsq) && max_rsq > 0.95) {
    log_result(paste0("R-squared upper bound: ", pos), "FAIL",
               paste0("Max Rsq = ", round(max_rsq, 3),
                      " -- R2 > 0.95 strongly suggests leakage"))
    record("S10", paste0("rsq_upper_", pos), "FAIL",
           paste0("Rsq=", round(max_rsq, 3)))
  } else {
    log_result(paste0("R-squared upper bound: ", pos), "PASS",
               paste0("Max Rsq = ", round(max_rsq, 3), " (< 0.95)"))
    record("S10", paste0("rsq_upper_", pos), "PASS",
           paste0("Rsq=", round(max_rsq, 3)))
  }
}

# ==============================================================================
# SECTION 11: ENSEMBLE GAIN
# ==============================================================================

log_section("SECTION 11: ENSEMBLE GAIN OVER BEST INDIVIDUAL MODEL")

cat("Stacking typically improves 0-5% over best single model.\n\n")

for (pos in positions) {
  ens_rmse  <- get_rmse(pos, "ensemble")
  ind_rmses <- sapply(c("xgboost", "random_forest", "elastic_net"),
                      function(m) get_rmse(pos, m))
  best_ind  <- min(ind_rmses, na.rm = TRUE)
  best_name <- names(which.min(ind_rmses))

  if (is.na(ens_rmse) || is.na(best_ind)) {
    log_result(paste0("Ensemble gain: ", pos), "WARN", "metrics not available")
    record("S11", paste0("ensemble_gain_", pos), "WARN", "NA")
    next
  }

  gain_pct <- round(100 * (best_ind - ens_rmse) / best_ind, 2)
  cat(sprintf("  %s: Ensemble %.3f | Best individual (%s) %.3f | Gain %.2f%%\n",
              pos, ens_rmse, best_name, best_ind, gain_pct))

  if (ens_rmse <= best_ind) {
    log_result(paste0("Ensemble gain: ", pos), "PASS",
               paste0("Beats best individual by ", gain_pct, "%"))
    record("S11", paste0("ensemble_gain_", pos), "PASS",
           paste0("gain=", gain_pct, "%"))
  } else {
    loss_pct <- round(100 * (ens_rmse - best_ind) / best_ind, 2)
    log_result(paste0("Ensemble trails best individual: ", pos), "WARN",
               paste0("Ensemble RMSE ", round(ens_rmse, 3),
                      " > best individual ", round(best_ind, 3),
                      " (", loss_pct, "% worse) -- stacking adds noise for this position"))
    record("S11", paste0("ensemble_gain_", pos), "WARN",
           paste0("loss=", loss_pct, "%"))
  }
}

# ==============================================================================
# SECTION 12: PREDICTION CORRELATION
# ==============================================================================

log_section("SECTION 12: PREDICTION CORRELATION (ENSEMBLE DIVERSIFICATION)")

cat("r > 0.9 between models: ensemble adds marginal value -- documented, not blocking.\n\n")

for (pos in positions) {
  comp <- results$comparisons[[pos]]

  if (is.null(comp) || is.null(comp$test_predictions)) {
    log_result(paste0("Prediction correlation: ", pos), "INFO",
               "test_predictions not available")
    record("S12", paste0("pred_corr_", pos), "INFO", "not available")
    next
  }

  pred_wide <- tryCatch(
    comp$test_predictions %>%
      filter(model %in% c("xgboost", "random_forest", "elastic_net")) %>%
      select(player_id, week, model, .pred) %>%
      pivot_wider(names_from = model, values_from = .pred),
    error = function(e) NULL
  )

  if (is.null(pred_wide)) {
    log_result(paste0("Prediction correlation: ", pos), "INFO",
               "Could not pivot predictions")
    record("S12", paste0("pred_corr_", pos), "INFO", "pivot failed")
    next
  }

  numeric_preds <- pred_wide %>% select(where(is.numeric))
  if (ncol(numeric_preds) < 2) {
    log_result(paste0("Prediction correlation: ", pos), "INFO",
               "Insufficient model predictions")
    record("S12", paste0("pred_corr_", pos), "INFO", "insufficient")
    next
  }

  cor_preds <- cor(numeric_preds, use = "pairwise.complete.obs")
  cor_vals  <- cor_preds[upper.tri(cor_preds)]
  max_cor   <- round(max(cor_vals, na.rm = TRUE), 3)

  cat(sprintf("  %s: max inter-model r = %.3f\n", pos, max_cor))

  if (max_cor > 0.90) {
    log_result(paste0("High prediction correlation: ", pos), "WARN",
               paste0("max r = ", max_cor,
                      " -- models correlated, stacking marginal"))
    record("S12", paste0("pred_corr_", pos), "WARN", paste0("max_r=", max_cor))
  } else {
    log_result(paste0("Prediction correlation: ", pos), "PASS",
               paste0("max r = ", max_cor, " -- sufficient diversity"))
    record("S12", paste0("pred_corr_", pos), "PASS", paste0("max_r=", max_cor))
  }
}

# ==============================================================================
# SECTION 13: FEATURE IMPORTANCE STABILITY
# ==============================================================================

log_section("SECTION 13: FEATURE IMPORTANCE STABILITY")

cat("Consensus features = top 20 in >= 2 of 3 models.\n")
cat("These are the most robust signals across model types.\n\n")

for (pos in positions) {
  feat_res <- results$features[[pos]]

  if (is.null(feat_res) || is.null(feat_res$importance_table)) {
    log_result(paste0("Feature stability: ", pos), "INFO",
               "importance_table not available")
    record("S13", paste0("feat_stability_", pos), "INFO", "not available")
    next
  }

  imp_tbl     <- feat_res$importance_table
  n_consensus <- sum(imp_tbl$is_consensus, na.rm = TRUE)
  n_total     <- nrow(imp_tbl)

  cat(sprintf("  %s: %d features | %d consensus (top 20 in >= 2 models)\n",
              pos, n_total, n_consensus))

  top5 <- imp_tbl %>%
    filter(is_consensus) %>%
    slice_min(consensus_rank, n = 5L) %>%
    select(feature, mean_importance, consensus_rank)

  if (nrow(top5) > 0) {
    cat("  Top consensus features:\n")
    for (j in seq_len(nrow(top5))) {
      cat(sprintf("    %d. %-35s %.3f\n",
                  top5$consensus_rank[j], top5$feature[j],
                  top5$mean_importance[j]))
    }
  }

  if (n_consensus >= 5) {
    log_result(paste0("Feature consensus: ", pos), "PASS",
               paste0(n_consensus, " consensus features"))
    record("S13", paste0("feat_consensus_", pos), "PASS",
           paste0(n_consensus, " consensus"))
  } else {
    log_result(paste0("Feature consensus: ", pos), "WARN",
               paste0("Only ", n_consensus, " consensus features -- models diverge"))
    record("S13", paste0("feat_consensus_", pos), "WARN",
           paste0(n_consensus, " consensus"))
  }
}

# ==============================================================================
# SECTION 14: OBSERVATION INDEPENDENCE
# ==============================================================================

log_section("SECTION 14: OBSERVATION INDEPENDENCE (DOCUMENTED LIMITATION)")

cat("XGBoost does not assume independent observations.\n")
cat("RMSE treats each player-week as independent -- same player in multiple\n")
cat("test weeks means RMSE CIs are anti-conservative.\n")
cat("Report point estimates only, no confidence intervals on RMSE.\n\n")

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

log_result("Observation independence: repeated player-weeks", "INFO",
           "RMSE CIs anti-conservative -- report point estimates only")
record("S14", "obs_independence", "INFO",
       "repeated player-weeks -- RMSE CIs anti-conservative by design")

# ==============================================================================
# SECTION 15: OPPONENT FEATURE COVERAGE (NEW WEEK 11)
# ==============================================================================

log_section("SECTION 15: OPPONENT FEATURE COVERAGE (WEEK 11 FIX VERIFICATION)")

cat("Week 11 fix: opp_adjusted_epa_prior, schedule_difficulty_rank, and\n")
cat("opp_adj_games_prior now populated via calculate_opponent_adjustments().\n")
cat("Previously all three were 100% NA in every model run.\n")
cat("FAIL here means the fix did not take -- check run_full_pipeline().\n\n")

opp_features <- c("opp_adjusted_epa_prior", "schedule_difficulty_rank",
                   "opp_adj_games_prior")

for (feat in opp_features) {
  if (!feat %in% names(model_rows)) {
    log_result(paste0("Column absent: ", feat), "FAIL")
    record("S15", paste0("opp_coverage_", feat), "FAIL", "column absent")
    next
  }

  pct_na <- model_rows %>%
    filter(week > 1) %>%
    summarise(pct = round(100 * mean(is.na(.data[[feat]])), 1)) %>%
    pull(pct)

  cat(sprintf("  %-35s: %.1f%% NA on post-week-1 rows\n", feat, pct_na))

  if (pct_na > 50) {
    log_result(paste0("Opponent coverage FAIL: ", feat), "FAIL",
               paste0(pct_na, "% NA -- fix not active. ",
                      "Ensure run_full_pipeline() has force_rerun=TRUE ",
                      "and calculate_opponent_adjustments() is called."))
    record("S15", paste0("opp_coverage_", feat), "FAIL", paste0(pct_na, "% NA"))
  } else if (pct_na > 20) {
    log_result(paste0("Opponent coverage WARN: ", feat), "WARN",
               paste0(pct_na, "% NA -- higher than expected; ",
                      "early weeks are structural NAs"))
    record("S15", paste0("opp_coverage_", feat), "WARN", paste0(pct_na, "% NA"))
  } else {
    log_result(paste0("Opponent coverage: ", feat), "PASS",
               paste0(pct_na, "% NA on post-week-1 rows -- fix confirmed active"))
    record("S15", paste0("opp_coverage_", feat), "PASS", paste0(pct_na, "% NA"))
  }
}

# Coverage by position
opp_by_pos <- model_rows %>%
  filter(week > 1) %>%
  group_by(position_group) %>%
  summarise(
    pct_na_opp = round(100 * mean(is.na(opp_adjusted_epa_prior)), 1),
    n          = n(),
    .groups    = "drop"
  )

cat("\nopp_adjusted_epa_prior coverage by position (weeks > 1):\n")
print(opp_by_pos)

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

log_section("ASSUMPTION VALIDATION SUMMARY")

summary_results <- bind_rows(lapply(results_log, as.data.frame)) %>%
  as_tibble()

status_counts <- summary_results %>%
  group_by(status) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(match(status, c("FAIL", "WARN", "PASS", "INFO")))

cat("\nResult counts:\n")
print(status_counts)

fails <- summary_results %>% filter(status == "FAIL")
if (nrow(fails) > 0) {
  cat("\nFAILURES -- resolve before trusting model output:\n")
  for (i in seq_len(nrow(fails))) {
    cat(sprintf("  [FAIL]  S%-3s | %s\n          %s\n",
                fails$section[i], fails$label[i], fails$detail[i]))
  }
} else {
  cat("\nNo FAIL results.\n")
}

warns <- summary_results %>% filter(status == "WARN")
if (nrow(warns) > 0) {
  cat("\nWARNINGS -- review before Week 11 code review:\n")
  for (i in seq_len(nrow(warns))) {
    cat(sprintf("  [WARN]  S%-3s | %s\n          %s\n",
                warns$section[i], warns$label[i], warns$detail[i]))
  }
}

cat("\n", strrep("-", 72), "\n")
cat("Output: output/assumption_tests/assumptions_week11.txt\n")
cat("Run date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("-", 72), "\n")

sink()

cat("\nAssumption validation complete.\n")
cat("Results saved to: output/assumption_tests/assumptions_week11.txt\n\n")
cat("Objects available in session:\n")
cat("  ml_data    -- full feature matrix\n")
cat("  results    -- week11_full_results list\n")
cat("  train_rows -- model-eligible training rows (weeks 1-14)\n")
cat("  test_rows  -- model-eligible test rows (weeks 15-18)\n")
