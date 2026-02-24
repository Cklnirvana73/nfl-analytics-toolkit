################################################################################
# Week 4: Predictive Validity Study - 2025 Season Analysis
# Full statistical analysis testing which metrics predict future performance
#
# Research Questions:
#   RQ1: Which metrics predict fantasy points? (correlations)
#   RQ2: Does EPA add value beyond yards + TDs? (regression)
#   RQ3: How accurate are predictions? (MAE/RMSE)
#   RQ4: When do metrics stabilize? (early vs late season)
#
# Outputs:
#   - CSV files with all results (analysis/week4_results/)
#   - Console summary of key findings
################################################################################

# Setup
library(dplyr)
library(glue)
library(here)
library(purrr)  # FIX: Required for map_dfr() used in RQ3

# Source required functions
source(here("R", "01_data_loading.R"))
source(here("R", "02_player_stats.R"))
source(here("R", "03_team_stats.R"))
source(here("R", "05_consistency_metrics.R"))  # Contains calculate_fantasy_points()
source(here("R", "06_predictive_validation.R"))

# Create output directory
output_dir <- here("analysis", "week4_results")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message(glue("Created output directory: {output_dir}"))
}

cat("\n")
cat("================================================================================\n")
cat("  Week 4 Predictive Validity Study: 2025 NFL Season\n")
cat("================================================================================\n")
cat("\n")

################################################################################
# DATA LOADING AND PREPARATION
################################################################################

cat("STEP 1: Loading and preparing 2025 season data...\n")
cat("--------------------------------------------------------------------------------\n")

# Load 2025 play-by-play data
pbp_2025 <- load_and_validate_pbp(2025, validate = TRUE)

# Apply garbage time filter
# NFL Context: Exclude plays when win probability <20% or >80% in Q4
# These are non-representative game situations that inflate stats
pbp_clean <- pbp_2025 %>%
  filter(
    # Remove garbage time (Q4 with extreme win probabilities)
    !(qtr == 4 & (wp < 0.20 | wp > 0.80)),
    # Remove QB kneels and spikes (not real offensive plays)
    !play_type %in% c("qb_kneel", "qb_spike"),
    # Keep only offensive plays
    play_type %in% c("pass", "run"),
    # Must have EPA value
    !is.na(epa)
  )

cat(glue("Loaded {nrow(pbp_2025)} total plays\n"))
cat(glue("After filtering: {nrow(pbp_clean)} clean offensive plays\n"))
cat(glue("Removed: {nrow(pbp_2025) - nrow(pbp_clean)} plays ",
         "({round((nrow(pbp_2025) - nrow(pbp_clean)) / nrow(pbp_2025) * 100, 1)}%)\n"))

# Create train/test splits (weeks 1-8 vs 9-18)
# NFL Context: Week 8 is roughly mid-season
splits <- split_season_by_week(pbp_clean, split_week = 8)

cat("\n")

################################################################################
# CALCULATE PLAYER STATS FOR BOTH PERIODS
################################################################################

cat("STEP 2: Calculating player statistics for train and test periods...\n")
cat("--------------------------------------------------------------------------------\n")

# ---- Helper: Calculate summed fantasy points per player for a period ----
# calculate_fantasy_points() returns player-week rows with total_fantasy_points
# We need to aggregate to total points per player across the period
get_period_fantasy_totals <- function(pbp_period) {
  calculate_fantasy_points(
    pbp_period,
    # Use standard PPR settings (no tiered, for cleaner predictive study)
    use_tiered_ppr = FALSE,
    te_premium = FALSE,
    rush_att_bonus = 0,
    ppr = 1
  ) %>%
    group_by(player_id) %>%
    summarise(fantasy_ppr = sum(total_fantasy_points, na.rm = TRUE), .groups = "drop")
}

# Running backs
cat("Calculating RB stats...\n")
rb_train <- get_player_rushing_stats(
  splits$train, 
  season = 2025, 
  week_min = 1, 
  week_max = 8
) %>%
  filter(rushes >= 20) %>%  # Minimum 20 rushes for inclusion
  mutate(position = "RB")

rb_test <- get_player_rushing_stats(
  splits$test, 
  season = 2025, 
  week_min = 9, 
  week_max = 18
) %>%
  filter(rushes >= 20) %>%
  mutate(position = "RB")

# Add fantasy points to test period
rb_test <- rb_test %>%
  left_join(
    get_period_fantasy_totals(splits$test),
    by = "player_id"
  )

cat(glue("  RBs: {nrow(rb_train)} in train, {nrow(rb_test)} in test\n"))

# Quarterbacks
cat("Calculating QB stats...\n")
qb_train <- get_player_passing_stats(
  splits$train, 
  season = 2025, 
  week_min = 1, 
  week_max = 8
) %>%
  filter(attempts >= 100) %>%  # Minimum 100 attempts
  mutate(position = "QB")

qb_test <- get_player_passing_stats(
  splits$test, 
  season = 2025, 
  week_min = 9, 
  week_max = 18
) %>%
  filter(attempts >= 100) %>%
  mutate(position = "QB")

# Add fantasy points
qb_test <- qb_test %>%
  left_join(
    get_period_fantasy_totals(splits$test),
    by = "player_id"
  )

cat(glue("  QBs: {nrow(qb_train)} in train, {nrow(qb_test)} in test\n"))

# Wide receivers
cat("Calculating WR stats...\n")
wr_train <- get_player_receiving_stats(
  splits$train, 
  season = 2025, 
  week_min = 1, 
  week_max = 8
) %>%
  filter(targets >= 20) %>%  # Minimum 20 targets
  mutate(position = "WR")

wr_test <- get_player_receiving_stats(
  splits$test, 
  season = 2025, 
  week_min = 9, 
  week_max = 18
) %>%
  filter(targets >= 20) %>%
  mutate(position = "WR")

# Add fantasy points
wr_test <- wr_test %>%
  left_join(
    get_period_fantasy_totals(splits$test),
    by = "player_id"
  )

cat(glue("  WRs: {nrow(wr_train)} in train, {nrow(wr_test)} in test\n"))

cat("\n")

################################################################################
# RESEARCH QUESTION 1: Which metrics predict fantasy points?
################################################################################

cat("================================================================================\n")
cat("  RQ1: Which metrics predict fantasy points?\n")
cat("================================================================================\n")
cat("\n")

# Running backs: test rush metrics
cat("RQ1a: Running backs - rush metrics predicting fantasy PPR\n")
cat("--------------------------------------------------------------------------------\n")

rb_metrics <- c("rush_yards", "rush_tds", "yards_per_carry", 
                "rush_epa_per_play", "rush_success_rate")

rb_correlations <- calculate_predictive_correlations(
  train_stats = rb_train,
  test_stats = rb_test,
  metrics = rb_metrics,
  outcome = "fantasy_ppr"
)

print(rb_correlations)
cat("\n")

# Save results
write.csv(
  rb_correlations, 
  file.path(output_dir, "rq1_rb_correlations.csv"),
  row.names = FALSE
)

# Quarterbacks: test pass metrics
cat("RQ1b: Quarterbacks - pass metrics predicting fantasy PPR\n")
cat("--------------------------------------------------------------------------------\n")

qb_metrics <- c("pass_yards", "pass_tds", "interceptions", "completion_pct",
                "pass_epa_per_play", "cpoe")

qb_correlations <- calculate_predictive_correlations(
  train_stats = qb_train,
  test_stats = qb_test,
  metrics = qb_metrics,
  outcome = "fantasy_ppr"
)

print(qb_correlations)
cat("\n")

write.csv(
  qb_correlations,
  file.path(output_dir, "rq1_qb_correlations.csv"),
  row.names = FALSE
)

# Wide receivers: test receiving metrics
cat("RQ1c: Wide receivers - receiving metrics predicting fantasy PPR\n")
cat("--------------------------------------------------------------------------------\n")

wr_metrics <- c("rec_yards", "rec_tds", "receptions", "catch_rate",
                "rec_epa_per_target", "target_share")

wr_correlations <- calculate_predictive_correlations(
  train_stats = wr_train,
  test_stats = wr_test,
  metrics = wr_metrics,
  outcome = "fantasy_ppr"
)

print(wr_correlations)
cat("\n")

write.csv(
  wr_correlations,
  file.path(output_dir, "rq1_wr_correlations.csv"),
  row.names = FALSE
)

# Summary: Best predictor for each position
cat("RQ1 SUMMARY: Best predictors by position\n")
cat("--------------------------------------------------------------------------------\n")

rq1_summary <- bind_rows(
  rb_correlations %>% slice(1) %>% mutate(position = "RB"),
  qb_correlations %>% slice(1) %>% mutate(position = "QB"),
  wr_correlations %>% slice(1) %>% mutate(position = "WR")
) %>%
  select(position, metric, correlation, p_value, sample_size)

print(rq1_summary)
cat("\n")

write.csv(
  rq1_summary,
  file.path(output_dir, "rq1_summary.csv"),
  row.names = FALSE
)

################################################################################
# RESEARCH QUESTION 2: Does EPA add value beyond traditional stats?
################################################################################

cat("================================================================================\n")
cat("  RQ2: Does EPA add value beyond yards + TDs?\n")
cat("================================================================================\n")
cat("\n")

# Running backs: yards + TDs vs yards + TDs + EPA
cat("RQ2a: Running backs regression models\n")
cat("--------------------------------------------------------------------------------\n")

rb_model1 <- run_regression_analysis(
  train_stats = rb_train,
  test_stats = rb_test,
  predictors = c("rush_yards", "rush_tds"),
  outcome = "fantasy_ppr"
)

cat("Model 1: fantasy_ppr ~ rush_yards + rush_tds\n")
print(rb_model1$summary)
cat("\n")
print(rb_model1$coefficients)
cat("\n")

rb_model2 <- run_regression_analysis(
  train_stats = rb_train,
  test_stats = rb_test,
  predictors = c("rush_yards", "rush_tds", "rush_epa_per_play"),
  outcome = "fantasy_ppr"
)

cat("Model 2: fantasy_ppr ~ rush_yards + rush_tds + rush_epa_per_play\n")
print(rb_model2$summary)
cat("\n")
print(rb_model2$coefficients)
cat("\n")

rb_r2_improvement <- rb_model2$summary$r_squared - rb_model1$summary$r_squared

cat(glue("R² improvement from adding EPA: {round(rb_r2_improvement, 3)}\n"))
cat(glue("Interpretation: EPA adds {round(rb_r2_improvement * 100, 1)}% explained variance\n"))
cat("\n")

# Save RB regression results
write.csv(
  bind_rows(
    rb_model1$summary %>% mutate(model = "Model 1: yards + TDs"),
    rb_model2$summary %>% mutate(model = "Model 2: yards + TDs + EPA")
  ),
  file.path(output_dir, "rq2_rb_model_comparison.csv"),
  row.names = FALSE
)

# Quarterbacks
cat("RQ2b: Quarterbacks regression models\n")
cat("--------------------------------------------------------------------------------\n")

qb_model1 <- run_regression_analysis(
  train_stats = qb_train,
  test_stats = qb_test,
  predictors = c("pass_yards", "pass_tds", "interceptions"),
  outcome = "fantasy_ppr"
)

cat("Model 1: fantasy_ppr ~ pass_yards + pass_tds + interceptions\n")
print(qb_model1$summary)
cat("\n")

qb_model2 <- run_regression_analysis(
  train_stats = qb_train,
  test_stats = qb_test,
  predictors = c("pass_yards", "pass_tds", "interceptions", "pass_epa_per_play"),
  outcome = "fantasy_ppr"
)

cat("Model 2: fantasy_ppr ~ pass_yards + pass_tds + interceptions + pass_epa_per_play\n")
print(qb_model2$summary)
cat("\n")

qb_r2_improvement <- qb_model2$summary$r_squared - qb_model1$summary$r_squared

cat(glue("R² improvement from adding EPA: {round(qb_r2_improvement, 3)}\n"))
cat("\n")

write.csv(
  bind_rows(
    qb_model1$summary %>% mutate(model = "Model 1: yards + TDs + INTs"),
    qb_model2$summary %>% mutate(model = "Model 2: yards + TDs + INTs + EPA")
  ),
  file.path(output_dir, "rq2_qb_model_comparison.csv"),
  row.names = FALSE
)

# Wide receivers
cat("RQ2c: Wide receivers regression models\n")
cat("--------------------------------------------------------------------------------\n")

wr_model1 <- run_regression_analysis(
  train_stats = wr_train,
  test_stats = wr_test,
  predictors = c("rec_yards", "rec_tds", "receptions"),
  outcome = "fantasy_ppr"
)

cat("Model 1: fantasy_ppr ~ rec_yards + rec_tds + receptions\n")
print(wr_model1$summary)
cat("\n")

wr_model2 <- run_regression_analysis(
  train_stats = wr_train,
  test_stats = wr_test,
  predictors = c("rec_yards", "rec_tds", "receptions", "rec_epa_per_target"),
  outcome = "fantasy_ppr"
)

cat("Model 2: fantasy_ppr ~ rec_yards + rec_tds + receptions + rec_epa_per_target\n")
print(wr_model2$summary)
cat("\n")

wr_r2_improvement <- wr_model2$summary$r_squared - wr_model1$summary$r_squared

cat(glue("R² improvement from adding EPA: {round(wr_r2_improvement, 3)}\n"))
cat("\n")

write.csv(
  bind_rows(
    wr_model1$summary %>% mutate(model = "Model 1: yards + TDs + receptions"),
    wr_model2$summary %>% mutate(model = "Model 2: yards + TDs + receptions + EPA")
  ),
  file.path(output_dir, "rq2_wr_model_comparison.csv"),
  row.names = FALSE
)

# RQ2 Summary
cat("RQ2 SUMMARY: R² improvement from adding EPA\n")
cat("--------------------------------------------------------------------------------\n")

rq2_summary <- tibble(
  position = c("RB", "QB", "WR"),
  baseline_r2 = c(rb_model1$summary$r_squared,
                  qb_model1$summary$r_squared,
                  wr_model1$summary$r_squared),
  epa_model_r2 = c(rb_model2$summary$r_squared,
                   qb_model2$summary$r_squared,
                   wr_model2$summary$r_squared),
  r2_improvement = c(rb_r2_improvement, qb_r2_improvement, wr_r2_improvement),
  pct_improvement = r2_improvement * 100
)

print(rq2_summary)
cat("\n")

write.csv(
  rq2_summary,
  file.path(output_dir, "rq2_summary.csv"),
  row.names = FALSE
)

################################################################################
# RESEARCH QUESTION 3: How accurate are predictions?
################################################################################

cat("================================================================================\n")
cat("  RQ3: Prediction accuracy (MAE/RMSE)\n")
cat("================================================================================\n")
cat("\n")

# Running backs
cat("RQ3a: Running back prediction errors\n")
cat("--------------------------------------------------------------------------------\n")

rb_errors <- purrr::map_dfr(rb_metrics, function(metric) {
  calculate_prediction_error(
    train_stats = rb_train,
    test_stats = rb_test,
    metric = metric,
    outcome = "fantasy_ppr"
  )
})

print(rb_errors %>% arrange(mae))
cat("\n")

write.csv(
  rb_errors,
  file.path(output_dir, "rq3_rb_prediction_errors.csv"),
  row.names = FALSE
)

# Quarterbacks
cat("RQ3b: Quarterback prediction errors\n")
cat("--------------------------------------------------------------------------------\n")

qb_errors <- purrr::map_dfr(qb_metrics, function(metric) {
  calculate_prediction_error(
    train_stats = qb_train,
    test_stats = qb_test,
    metric = metric,
    outcome = "fantasy_ppr"
  )
})

print(qb_errors %>% arrange(mae))
cat("\n")

write.csv(
  qb_errors,
  file.path(output_dir, "rq3_qb_prediction_errors.csv"),
  row.names = FALSE
)

# Wide receivers
cat("RQ3c: Wide receiver prediction errors\n")
cat("--------------------------------------------------------------------------------\n")

wr_errors <- purrr::map_dfr(wr_metrics, function(metric) {
  calculate_prediction_error(
    train_stats = wr_train,
    test_stats = wr_test,
    metric = metric,
    outcome = "fantasy_ppr"
  )
})

print(wr_errors %>% arrange(mae))
cat("\n")

write.csv(
  wr_errors,
  file.path(output_dir, "rq3_wr_prediction_errors.csv"),
  row.names = FALSE
)

# RQ3 Summary: Best predictor by MAE
cat("RQ3 SUMMARY: Most accurate predictors (lowest MAE)\n")
cat("--------------------------------------------------------------------------------\n")

rq3_summary <- bind_rows(
  rb_errors %>% slice_min(mae, n = 1) %>% mutate(position = "RB"),
  qb_errors %>% slice_min(mae, n = 1) %>% mutate(position = "QB"),
  wr_errors %>% slice_min(mae, n = 1) %>% mutate(position = "WR")
) %>%
  select(position, metric, mae, rmse, correlation, sample_size)

print(rq3_summary)
cat("\n")

write.csv(
  rq3_summary,
  file.path(output_dir, "rq3_summary.csv"),
  row.names = FALSE
)

################################################################################
# RESEARCH QUESTION 4: When do metrics stabilize?
################################################################################

cat("================================================================================\n")
cat("  RQ4: Metric stability (test-retest reliability)\n")
cat("================================================================================\n")
cat("\n")

cat("NOTE: RQ4 requires additional implementation to test stability over time.\n")
cat("See calculate_predictive_correlations() for current stability assessment.\n")
cat("Train-test correlation (RQ1) already measures stability:\n")
cat("  - High correlation = early values predict late values (stable)\n")
cat("  - Low correlation = high variance across season (unstable)\n")
cat("\n")

# Use RQ1 correlations as proxy for stability
rq4_summary <- bind_rows(
  rb_correlations %>% mutate(position = "RB"),
  qb_correlations %>% mutate(position = "QB"),
  wr_correlations %>% mutate(position = "WR")
) %>%
  select(position, metric, correlation, sample_size) %>%
  arrange(position, desc(abs(correlation)))

write.csv(
  rq4_summary,
  file.path(output_dir, "rq4_metric_stability.csv"),
  row.names = FALSE
)

cat("Saved stability analysis (based on train-test correlations)\n")
cat("\n")

################################################################################
# FINAL SUMMARY
################################################################################

cat("================================================================================\n")
cat("  WEEK 4 STUDY COMPLETE - KEY FINDINGS\n")
cat("================================================================================\n")
cat("\n")

cat("RQ1: Best predictors of fantasy points\n")
cat("  - RBs: ", rq1_summary %>% filter(position == "RB") %>% pull(metric), 
    " (r = ", round(rq1_summary %>% filter(position == "RB") %>% pull(correlation), 3), ")\n", sep = "")
cat("  - QBs: ", rq1_summary %>% filter(position == "QB") %>% pull(metric),
    " (r = ", round(rq1_summary %>% filter(position == "QB") %>% pull(correlation), 3), ")\n", sep = "")
cat("  - WRs: ", rq1_summary %>% filter(position == "WR") %>% pull(metric),
    " (r = ", round(rq1_summary %>% filter(position == "WR") %>% pull(correlation), 3), ")\n", sep = "")
cat("\n")

cat("RQ2: EPA adds predictive value\n")
cat("  - RBs: +", round(rq2_summary %>% filter(position == "RB") %>% pull(pct_improvement), 1), 
    "% R² improvement\n", sep = "")
cat("  - QBs: +", round(rq2_summary %>% filter(position == "QB") %>% pull(pct_improvement), 1),
    "% R² improvement\n", sep = "")
cat("  - WRs: +", round(rq2_summary %>% filter(position == "WR") %>% pull(pct_improvement), 1),
    "% R² improvement\n", sep = "")
cat("\n")

cat("RQ3: Prediction accuracy\n")
cat("  - RBs: MAE = ", round(rq3_summary %>% filter(position == "RB") %>% pull(mae), 1),
    " fantasy points (", rq3_summary %>% filter(position == "RB") %>% pull(metric), ")\n", sep = "")
cat("  - QBs: MAE = ", round(rq3_summary %>% filter(position == "QB") %>% pull(mae), 1),
    " fantasy points (", rq3_summary %>% filter(position == "QB") %>% pull(metric), ")\n", sep = "")
cat("  - WRs: MAE = ", round(rq3_summary %>% filter(position == "WR") %>% pull(mae), 1),
    " fantasy points (", rq3_summary %>% filter(position == "WR") %>% pull(metric), ")\n", sep = "")
cat("\n")

cat("All results saved to:", output_dir, "\n")
cat("\n")

cat("Next steps:\n")
cat("  1. Create visualizations (examples/create_week4_visual.R)\n")
cat("  2. Write statistical report (docs/Week4_Statistical_Report.pdf)\n")
cat("  3. Review results and validate findings\n")
cat("\n")

cat("================================================================================\n")
cat("  Analysis complete!\n")
cat("================================================================================\n")
