# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 11
# Example Script: Injury Proximity Natural Experiment
# File: examples/example_season2_week11.R
#
# Purpose: Run the full injury proximity pipeline, inspect results at each
#          stage, and write all outputs to disk.
#
# What this script does:
#   1. Sources R/25_injury_proximity_experiment.R (which cascades to R/15
#      and R/17 via source guards)
#   2. Runs run_week11_pipeline() across the full 2010-2025 data range
#   3. Inspects each function's output in sequence:
#        - identify_returning_players()    -- returning player census
#        - classify_treatment_control_injury() -- group assignments
#        - check_balance_injury_groups()   -- covariate balance and SMDs
#        - validate_injury_assumptions()   -- five assumption checks
#        - design_power_analysis_injury()  -- power and MDE at 80/90%
#        - create_injury_experiment_specification() -- formal design doc
#        - calculate_injury_effect()       -- primary MAE and PPG effects
#        - analyze_injury_heterogeneous_effects() -- position/timing/era
#        - injury_robustness_check()       -- sensitivity analysis
#   4. Writes 8 CSV files to output/
#   5. Prints KEY INSIGHTS computed from live console output
#
# Runtime note:
#   First run loads 16 seasons of nflfastR PBP (2010-2025) and 16 seasons
#   of weekly fantasy scoring. Expect 25-45 minutes. Subsequent runs are
#   faster once the R/15 season cache is populated. The pipeline also writes
#   its own RDS cache files to data/season2_cache/ -- these are loaded on
#   re-runs automatically by load_normalized_season().
#
# Outputs (8 CSVs):
#   output/s2_week11_groups.csv                -- treatment/control assignments
#   output/s2_week11_group_summary.csv         -- group-level effect summary
#   output/s2_week11_player_effects.csv        -- player-level prediction errors
#   output/s2_week11_trajectory.csv            -- treatment week-by-week PPG
#   output/s2_week11_heterogeneous.csv         -- all heterogeneous effect strata
#   output/s2_week11_robustness_specs.csv      -- 9-spec robustness sweep
#   output/s2_week11_balance_smd.csv           -- covariate balance SMDs
#   output/s2_week11_assumption_checks.csv     -- five assumption check results
# ==============================================================================


# ==============================================================================
# LIBRARIES
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(here)
library(nflreadr)


# ==============================================================================
# SOURCE: R/25 (cascades to R/15 and R/17 via internal source guards)
# ==============================================================================

week25_path <- here::here("R", "25_injury_proximity_experiment.R")
if (!file.exists(week25_path)) {
  stop(glue(
    "R/25_injury_proximity_experiment.R not found at: {week25_path}\n",
    "Build the file before running this example."
  ), call. = FALSE)
}
source(week25_path)


# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Output directory for CSV files
OUTPUT_DIR <- here::here("output")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# Cache directory for PBP (inherits from R/15 default)
CACHE_DIR <- here::here("data", "season2_cache")

# Prefix for all output files this week
FILE_PREFIX <- "s2_week11_"


# ==============================================================================
# SECTION 1: RUN THE FULL PIPELINE
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 1: Full pipeline run\n")
cat(strrep("=", 60), "\n\n")

cat("Running run_week11_pipeline()...\n")
cat("Seasons loaded: ", min(SEASONS_W11), "-", max(SEASONS_W11), "\n")
cat("Analysis seasons: ", min(ANALYSIS_SEASONS_W11), "-",
    max(ANALYSIS_SEASONS_W11), "\n")
cat("Training window: Weeks ", min(TRAINING_WEEKS_W11), "-",
    max(TRAINING_WEEKS_W11), "\n")
cat("Outcome window:  Weeks ", min(OUTCOME_WEEKS_W11), "-",
    max(OUTCOME_WEEKS_W11), "\n\n")

# run_week11_pipeline() saves 6 RDS/txt files to data/season2_cache/ and
# returns all intermediate results as a named list. save_outputs = TRUE is
# the default -- set FALSE to skip the RDS writes during iteration.
results <- run_week11_pipeline(
  seasons              = SEASONS_W11,
  analysis_seasons     = ANALYSIS_SEASONS_W11,
  cache_dir            = CACHE_DIR,
  training_weeks       = TRAINING_WEEKS_W11,
  outcome_weeks        = OUTCOME_WEEKS_W11,
  treatment_return_max = TREATMENT_RETURN_MAX_W11,
  save_outputs         = TRUE,
  verbose              = TRUE
)

cat("\nPipeline complete. Inspecting outputs...\n\n")


# ==============================================================================
# SECTION 2: RETURNING PLAYER CENSUS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 2: Returning player census\n")
cat(strrep("=", 60), "\n\n")

# results$returning_players: all skill-position players who had at least one
# inferred absence in the training window AND returned before the window closed.
# This is the full pool before treatment/control classification.

n_returning <- nrow(results$returning_players)
n_seasons_w_returners <- dplyr::n_distinct(results$returning_players$season)

cat(glue(
  "Total returning player-seasons identified: ",
  "{format(n_returning, big.mark = ',')}\n",
  "Across {n_seasons_w_returners} unique seasons\n\n"
))

cat("Position breakdown of returning players:\n")
results$returning_players %>%
  dplyr::count(position, sort = TRUE) %>%
  dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\nReturn week distribution:\n")
results$returning_players %>%
  dplyr::count(return_week) %>%
  dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\nAbsence length distribution (training-window absent weeks):\n")
results$returning_players %>%
  dplyr::count(n_absent_weeks) %>%
  dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()


# ==============================================================================
# SECTION 3: TREATMENT AND CONTROL GROUP ASSIGNMENTS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 3: Group assignments\n")
cat(strrep("=", 60), "\n\n")

# results$groups: one row per player-season classified as treatment or control.
# group = "treatment" -> return_week <= TREATMENT_RETURN_MAX_W11 (4)
# group = "control"   -> no absent weeks, >= MIN_TRAINING_GAMES_W11 (3) games

n_trt <- sum(results$groups$group == "treatment", na.rm = TRUE)
n_ctl <- sum(results$groups$group == "control",   na.rm = TRUE)

cat(glue("Treatment: {n_trt} player-seasons\n"))
cat(glue("Control:   {n_ctl} player-seasons\n\n"))

cat("Treatment group: position and era breakdown\n")
results$groups %>%
  dplyr::filter(group == "treatment") %>%
  dplyr::count(position, era) %>%
  tidyr::pivot_wider(names_from = era, values_from = n, values_fill = 0L) %>%
  print()

cat("\nControl group: position and era breakdown\n")
results$groups %>%
  dplyr::filter(group == "control") %>%
  dplyr::count(position, era) %>%
  tidyr::pivot_wider(names_from = era, values_from = n, values_fill = 0L) %>%
  print()

cat("\nTreatment group: return week breakdown (should be <= 4 only)\n")
results$groups %>%
  dplyr::filter(group == "treatment") %>%
  dplyr::count(return_week) %>%
  print()


# ==============================================================================
# SECTION 4: COVARIATE BALANCE AND ASSUMPTION CHECKS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 4: Balance and assumption validation\n")
cat(strrep("=", 60), "\n\n")

# Standardized mean differences
cat("Standardized mean differences (abs(SMD) > 0.20 = flagged):\n")
print(results$balance$smd_table)

cat("\nPosition distribution by group:\n")
print(results$balance$position_dist)

cat("\nAssumption validation report:\n")
cat(results$assumptions$report, "\n\n")

cat("Individual assumption check results:\n")
print(results$assumptions$checks_passed)

cat("\nReturn timing distribution (Check B: no single week > 60%):\n")
print(results$assumptions$return_timing)

cat("\nParallel trends proxy (prior-season PPG by group):\n")
print(results$assumptions$parallel_trends)


# ==============================================================================
# SECTION 5: POWER ANALYSIS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 5: Power analysis\n")
cat(strrep("=", 60), "\n\n")

cat(results$power$summary, "\n\n")

cat("Power curve (d = 0.10 to 0.50):\n")
print(results$power$power_table)


# ==============================================================================
# SECTION 6: EXPERIMENT SPECIFICATION DOCUMENT
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 6: Experiment specification\n")
cat(strrep("=", 60), "\n\n")

# The spec was already printed by run_week11_pipeline(). Access it here if
# you need to inspect or re-print without re-running the pipeline.
cat("Spec written to: data/season2_cache/s2_week11_experiment_spec.txt\n")
cat("First 10 lines:\n")
cat(paste(head(strsplit(results$spec, "\n")[[1L]], 10L), collapse = "\n"), "\n\n")


# ==============================================================================
# SECTION 7: PRIMARY EFFECT RESULTS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 7: Primary effect -- MAE and outcome PPG\n")
cat(strrep("=", 60), "\n\n")

# Group-level summary (n, mean prior PPG, mean outcome PPG, mean MAE)
cat("Group summary (prediction accuracy comparison):\n")
print(results$effects$group_summary)

cat("\nMAE effect (treatment - control):\n")
cat(glue(
  "  Estimate : {round(results$effects$effect_mae$estimate, 3)} PPG\n",
  "  95% CI   : [{round(results$effects$effect_mae$ci_lower, 3)}, ",
  "{round(results$effects$effect_mae$ci_upper, 3)}]\n",
  "  Method   : {results$effects$effect_mae$method}\n",
  "  N (trt)  : {results$effects$effect_mae$n_a}\n",
  "  N (ctl)  : {results$effects$effect_mae$n_b}\n"
))

cat("\nOutcome PPG effect (treatment - control):\n")
cat(glue(
  "  Estimate : {round(results$effects$effect_ppg$estimate, 3)} PPG\n",
  "  95% CI   : [{round(results$effects$effect_ppg$ci_lower, 3)}, ",
  "{round(results$effects$effect_ppg$ci_upper, 3)}]\n"
))

cat(glue(
  "\nCohen's d (MAE comparison): {round(results$effects$cohens_d_mae, 3)}\n\n"
))

# Welch t-test p-values (secondary; bootstrap CI is the primary inference)
if (!is.null(results$effects$t_test_mae)) {
  cat(glue(
    "Welch t-test for MAE: p = ",
    "{round(results$effects$t_test_mae$p.value, 4)}\n"
  ))
}
if (!is.null(results$effects$t_test_ppg)) {
  cat(glue(
    "Welch t-test for PPG: p = ",
    "{round(results$effects$t_test_ppg$p.value, 4)}\n\n"
  ))
}

# Within-treatment performance trajectory in Weeks 9-18
cat("Within-treatment trajectory (Weeks 9-18 weekly mean PPG):\n")
cat("(Does performance trend up = stabilizing? Trend flat = still impaired?)\n")
print(results$effects$trajectory)

# Top and bottom 10 treatment players by prediction error
cat("\nTop 10 treatment players by abs prediction error (most surprising):\n")
results$effects$player_level %>%
  dplyr::filter(group == "treatment") %>%
  dplyr::arrange(dplyr::desc(abs_error)) %>%
  dplyr::select(player_name, season, position, return_week,
                prior_ppg, outcome_ppg, prediction_error, abs_error) %>%
  dplyr::slice_head(n = 10L) %>%
  as.data.frame() %>%
  print()

cat("\nTop 10 control players by abs prediction error (most surprising):\n")
results$effects$player_level %>%
  dplyr::filter(group == "control") %>%
  dplyr::arrange(dplyr::desc(abs_error)) %>%
  dplyr::select(player_name, season, position,
                prior_ppg, outcome_ppg, prediction_error, abs_error) %>%
  dplyr::slice_head(n = 10L) %>%
  as.data.frame() %>%
  print()


# ==============================================================================
# SECTION 8: HETEROGENEOUS EFFECTS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 8: Heterogeneous effects\n")
cat(strrep("=", 60), "\n\n")

cat("Effect by position:\n")
print(results$heterogeneous$by_position)

cat("\nEffect by return timing (early Week 1-2 vs mid Week 3-4):\n")
cat("Both strata compare the timing subgroup to the full control pool.\n")
print(results$heterogeneous$by_return_timing)

cat("\nEffect by era (Early = pre-2017 / Modern = 2017+):\n")
print(results$heterogeneous$by_era)

cat("\nNarrative:\n")
cat(results$heterogeneous$narrative, "\n\n")


# ==============================================================================
# SECTION 9: ROBUSTNESS CHECKS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 9: Robustness checks\n")
cat(strrep("=", 60), "\n\n")

cat("Sweep: treatment_return_max (3, 4, 5) x min_prior_games (1, 2, 3):\n")
print(results$robustness$results_table)

cat("\nOutcome metric comparison at base specification:\n")
if (nrow(results$robustness$outcome_metric_comparison) > 0L) {
  print(results$robustness$outcome_metric_comparison)
} else {
  cat("No outcome metric comparison available (base spec produced no data).\n")
}

cat("\nRobustness summary:\n")
cat(results$robustness$summary, "\n\n")


# ==============================================================================
# SECTION 10: WRITE CSV OUTPUTS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 10: Writing CSV outputs\n")
cat(strrep("=", 60), "\n\n")

# 1. Group assignments
groups_out <- results$groups
write.csv(
  groups_out,
  file      = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "groups.csv")),
  row.names = FALSE
)
cat(glue(
  "1. {FILE_PREFIX}groups.csv: ",
  "{format(nrow(groups_out), big.mark=',')} rows\n"
))

# 2. Group-level effect summary
group_summary_out <- results$effects$group_summary
write.csv(
  group_summary_out,
  file      = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "group_summary.csv")),
  row.names = FALSE
)
cat(glue(
  "2. {FILE_PREFIX}group_summary.csv: ",
  "{format(nrow(group_summary_out), big.mark=',')} rows\n"
))

# 3. Player-level prediction errors
player_effects_out <- results$effects$player_level %>%
  dplyr::arrange(group, dplyr::desc(abs_error))
write.csv(
  player_effects_out,
  file      = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "player_effects.csv")),
  row.names = FALSE
)
cat(glue(
  "3. {FILE_PREFIX}player_effects.csv: ",
  "{format(nrow(player_effects_out), big.mark=',')} rows\n"
))

# 4. Within-treatment trajectory
trajectory_out <- results$effects$trajectory
write.csv(
  trajectory_out,
  file      = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "trajectory.csv")),
  row.names = FALSE
)
cat(glue(
  "4. {FILE_PREFIX}trajectory.csv: ",
  "{format(nrow(trajectory_out), big.mark=',')} rows\n"
))

# 5. Heterogeneous effects (all three strata combined)
het_out <- dplyr::bind_rows(
  results$heterogeneous$by_position     %>% dplyr::mutate(stratum_type = "position"),
  results$heterogeneous$by_return_timing %>% dplyr::mutate(stratum_type = "return_timing"),
  results$heterogeneous$by_era           %>% dplyr::mutate(stratum_type = "era")
) %>%
  dplyr::select(stratum_type, stratum, dplyr::everything())

write.csv(
  het_out,
  file      = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "heterogeneous.csv")),
  row.names = FALSE
)
cat(glue(
  "5. {FILE_PREFIX}heterogeneous.csv: ",
  "{format(nrow(het_out), big.mark=',')} rows\n"
))

# 6. Robustness specs sweep
robustness_out <- results$robustness$results_table
write.csv(
  robustness_out,
  file      = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "robustness_specs.csv")),
  row.names = FALSE
)
cat(glue(
  "6. {FILE_PREFIX}robustness_specs.csv: ",
  "{format(nrow(robustness_out), big.mark=',')} rows\n"
))

# 7. Covariate balance SMDs
balance_out <- results$balance$smd_table
write.csv(
  balance_out,
  file      = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "balance_smd.csv")),
  row.names = FALSE
)
cat(glue(
  "7. {FILE_PREFIX}balance_smd.csv: ",
  "{format(nrow(balance_out), big.mark=',')} rows\n"
))

# 8. Assumption check results
assumption_out <- dplyr::tibble(
  check   = names(results$assumptions$checks_passed),
  passed  = as.logical(results$assumptions$checks_passed)
)
write.csv(
  assumption_out,
  file      = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "assumption_checks.csv")),
  row.names = FALSE
)
cat(glue(
  "8. {FILE_PREFIX}assumption_checks.csv: ",
  "{format(nrow(assumption_out), big.mark=',')} rows\n"
))

cat("\nAll CSVs written to:", OUTPUT_DIR, "\n")


# ==============================================================================
# KEY INSIGHTS (all values computed from live results)
# ==============================================================================

cat("\n")
sep <- strrep("=", 60)
cat(sep, "\n")
cat("KEY INSIGHTS -- Week 11 Injury Proximity Experiment\n")
cat(sep, "\n\n")

# Group sizes
n_trt_final <- sum(results$groups$group == "treatment", na.rm = TRUE)
n_ctl_final <- sum(results$groups$group == "control",   na.rm = TRUE)
n_returning_total <- nrow(results$returning_players)

# MAE effect
mae_est    <- round(results$effects$effect_mae$estimate, 2)
mae_ci_lo  <- round(results$effects$effect_mae$ci_lower, 2)
mae_ci_hi  <- round(results$effects$effect_mae$ci_upper, 2)
mae_method <- results$effects$effect_mae$method

# PPG effect
ppg_est <- round(results$effects$effect_ppg$estimate, 2)

# Cohen's d
d_val <- round(results$effects$cohens_d_mae, 3)

# Direction: determined from data
mae_direction <- if (is.na(mae_est)) {
  "indeterminate (NA estimate)"
} else if (mae_est > 0) {
  "higher -- returning players are harder to predict"
} else if (mae_est < 0) {
  "lower -- returning players are easier to predict"
} else {
  "no difference"
}

# CI interpretation: does the CI exclude zero?
ci_excludes_zero <- if (!is.na(mae_ci_lo) && !is.na(mae_ci_hi)) {
  if (mae_ci_lo > 0 || mae_ci_hi < 0) "yes (effect is significant at 5%)" else "no (CI crosses zero)"
} else "CI unavailable"

# Top position by heterogeneous effect
pos_tbl   <- results$heterogeneous$by_position
valid_pos  <- pos_tbl[!is.na(pos_tbl$mae_diff), ]
top_pos_by_effect <- if (nrow(valid_pos) > 0L) {
  valid_pos$stratum[which.max(abs(valid_pos$mae_diff))]
} else "insufficient data"

# Era comparison
era_tbl <- results$heterogeneous$by_era
era_modern_diff <- era_tbl$mae_diff[era_tbl$stratum == "Modern"]
era_early_diff  <- era_tbl$mae_diff[era_tbl$stratum == "Early"]
era_trend <- if (length(era_modern_diff) > 0L && length(era_early_diff) > 0L &&
                  !is.na(era_modern_diff) && !is.na(era_early_diff)) {
  if (era_modern_diff > era_early_diff) {
    "larger in Modern era"
  } else if (era_modern_diff < era_early_diff) {
    "larger in Early era"
  } else {
    "similar across eras"
  }
} else "insufficient data for era comparison"

# Assumption checks
n_checks_ok <- sum(results$assumptions$checks_passed, na.rm = TRUE)

# Power
pw_val   <- round(results$power$power_at_target, 3)
mde_80   <- round(results$power$detectable_at_80, 3)

# Robustness: are all valid specs pointing the same direction?
valid_rob <- results$robustness$results_table[
  !is.na(results$robustness$results_table$mae_diff), ]
rob_direction_consistent <- if (nrow(valid_rob) > 1L) {
  all(sign(valid_rob$mae_diff) == sign(valid_rob$mae_diff[[1L]]), na.rm = TRUE)
} else NA

rob_direction_label <- if (is.na(rob_direction_consistent)) {
  "insufficient specs to assess"
} else if (rob_direction_consistent) {
  "consistent direction across all specifications"
} else {
  "direction varies across specifications"
}

cat(glue(
  "DATA FOUNDATION\n",
  "  Returning player-seasons identified  : {format(n_returning_total, big.mark=',')}\n",
  "  Treatment (return Wks 1-{TREATMENT_RETURN_MAX_W11})          : {format(n_trt_final, big.mark=',')}\n",
  "  Control (healthy all 8 weeks)        : {format(n_ctl_final, big.mark=',')}\n",
  "  Assumption checks passed             : {n_checks_ok}/5\n\n",

  "PRIMARY EFFECT (MAE comparison: treatment - control)\n",
  "  Estimate     : {mae_est} PPG ({mae_method} CI)\n",
  "  95% CI       : [{mae_ci_lo}, {mae_ci_hi}]\n",
  "  Direction    : {mae_direction}\n",
  "  CI excludes 0: {ci_excludes_zero}\n",
  "  Cohen's d    : {d_val}\n\n",

  "OUTCOME PPG\n",
  "  Treatment - Control (Wks 9-18) : {ppg_est} PPG\n\n",

  "HETEROGENEOUS EFFECTS\n",
  "  Position with largest MAE effect : {top_pos_by_effect}\n",
  "  Era comparison                   : {era_trend}\n\n",

  "POWER\n",
  "  Power at d = 0.20 : {pw_val}\n",
  "  MDE at 80% power  : {mde_80}\n\n",

  "ROBUSTNESS (9-spec sweep: return_max x min_prior_games)\n",
  "  Valid specs        : {nrow(valid_rob)} / {nrow(results$robustness$results_table)}\n",
  "  Direction finding  : {rob_direction_label}\n"
))

cat("\n", sep, "\n")
cat("Week 11 example complete.\n")
cat(sep, "\n")
