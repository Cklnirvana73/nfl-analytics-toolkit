# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 12
# Example Script: Usage Ramp Experiment + Rookie Subgroup Analysis
# File: examples/example_season2_week12.R
#
# Purpose: Run the full usage ramp pipeline, inspect results at each stage,
#          and write all outputs to disk.
#
# What this script does:
#   1. Sources R/26_usage_ramp_experiment.R (which cascades to R/15 and R/17
#      via source guards)
#   2. Runs run_week12_pipeline() across the full 2010-2025 data range
#   3. Inspects each function's output in sequence:
#        - calculate_weekly_usage_share()       -- position-appropriate usage
#        - identify_usage_ramps()               -- sub-window ramp detection
#        - classify_treatment_control_ramp()    -- four-group classification
#        - check_balance_ramp_groups()          -- covariate balance
#        - validate_ramp_assumptions()          -- five assumption checks
#        - create_ramp_experiment_specification()-- formal design doc
#        - identify_rookies()                   -- years_exp == 0 flag
#        - stratify_by_rookie_status()          -- four-way rookie/ramp split
#        - calculate_usage_persistence()        -- mechanism check
#        - run_ramp_experiment()                -- primary PPG effect
#        - analyze_ramp_by_rookie_status()      -- rookie vs vet comparison
#        - rookie_ramp_heterogeneous_effects()  -- within-rookie variation
#        - analyze_ramp_heterogeneous_effects() -- position/era/size subgroups
#        - ramp_robustness_check()              -- 6-spec sensitivity grid
#        - create_ramp_experiment_report()      -- formatted results doc
#   4. Writes 8 CSV files to output/season2_week12/
#   5. Prints KEY INSIGHTS computed from live console output
#
# Runtime note:
#   First run loads 16 seasons of nflfastR PBP (2010-2025) and computes
#   weekly usage and fantasy scoring. Expect 25-45 minutes. Subsequent runs
#   are faster once the R/15 season cache is populated.
#   RDS cache files are written to data/season2_cache/ by the pipeline.
#
# Output files (written to output/season2_week12/):
#   s2_week12_groups.csv            -- classified player-seasons
#   s2_week12_ramp_flags.csv        -- ramp detection detail
#   s2_week12_balance.csv           -- balance check results
#   s2_week12_effects_summary.csv   -- primary PPG effect and group means
#   s2_week12_rookie_effects.csv    -- rookie vs vet stratum summary
#   s2_week12_heterogeneous.csv     -- position/era/size subgroup effects
#   s2_week12_robustness.csv        -- 6-spec robustness grid
#   s2_week12_persistence.csv       -- usage persistence by group
#
# ==============================================================================


# ==============================================================================
# SECTION 1: SETUP
# ==============================================================================

library(dplyr)
library(tidyr)
library(glue)
library(here)

# Source the main production file. Source guards inside R/26 handle R/15 and
# R/17 automatically -- no need to source them separately here.
source_path <- here::here("R", "26_usage_ramp_experiment.R")
if (!file.exists(source_path)) {
  stop(glue(
    "R/26_usage_ramp_experiment.R not found at: {source_path}\n",
    "Confirm the file exists at R/26_usage_ramp_experiment.R before running."
  ))
}
source(source_path)

# Output directory for this week's CSVs
output_dir <- here::here("output", "season2_week12")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message(glue("Created output directory: {output_dir}"))
}


# ==============================================================================
# SECTION 2: CONFIGURATION
# ==============================================================================

# All constants are defined in R/26. Set overrides here only if needed.
# To re-run a subset of seasons (faster for testing): set seasons here.
# Default: 2010-2025 full range.

EXAMPLE_SEASONS <- SEASONS_W12          # 2010L:2025L -- full range
EXAMPLE_CACHE   <- CACHE_DIR_W12        # data/season2_cache/

cat(strrep("=", 60), "\n")
cat("Week 12: Usage Ramp Experiment + Rookie Subgroup Analysis\n")
cat(strrep("=", 60), "\n")
cat(glue(
  "Seasons  : {min(EXAMPLE_SEASONS)}-{max(EXAMPLE_SEASONS)}\n",
  "Cache dir: {EXAMPLE_CACHE}\n",
  "Ramp threshold   : {RAMP_THRESHOLD_W12 * 100}% relative\n",
  "Games floor      : {MIN_ACTIVE_WEEKS_SUBWINDOW_W12} of 4 weeks per sub-window\n",
  "Training window  : Weeks {min(TRAINING_WEEKS_W12)}-{max(TRAINING_WEEKS_W12)}\n",
  "Outcome window   : Weeks {min(OUTCOME_WEEKS_W12)}-{max(OUTCOME_WEEKS_W12)}\n"
), "\n")


# ==============================================================================
# SECTION 3: RUN FULL PIPELINE
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 3: Running full pipeline\n")
cat(strrep("=", 60), "\n\n")

# run_week12_pipeline() handles all 9 steps internally:
#   1. Load rosters (position + years_exp)
#   2. Season-by-season PBP loop (usage + fantasy)
#   3. identify_usage_ramps()
#   4. classify_treatment_control_ramp()
#   5. identify_rookies() + stratify_by_rookie_status()
#   6. check_balance_ramp_groups() + validate_ramp_assumptions()
#   7. Experiment specification
#   8. Primary effect + rookie subgroup + heterogeneous effects
#   9. Robustness checks
#
# save_outputs = TRUE writes RDS files to data/season2_cache/ automatically.

pipeline <- run_week12_pipeline(
  seasons          = EXAMPLE_SEASONS,
  analysis_seasons = ANALYSIS_SEASONS_W12,
  cache_dir        = EXAMPLE_CACHE,
  training_weeks   = TRAINING_WEEKS_W12,
  outcome_weeks    = OUTCOME_WEEKS_W12,
  ramp_threshold   = RAMP_THRESHOLD_W12,
  save_outputs     = TRUE,
  verbose          = TRUE
)

cat("\nPipeline complete. Result slots:\n")
print(names(pipeline))
cat("\n")


# ==============================================================================
# SECTION 4: INSPECT WEEKLY USAGE SHARE
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 4: Weekly usage share inspection\n")
cat(strrep("=", 60), "\n\n")

cat("Weekly usage dimensions:\n")
cat(glue(
  "  Rows    : {format(nrow(pipeline$weekly_usage), big.mark=',')}\n",
  "  Columns : {ncol(pipeline$weekly_usage)}\n",
  "  Seasons : {paste(range(pipeline$weekly_usage$season), collapse='-')}\n",
  "  Positions: {paste(sort(unique(pipeline$weekly_usage$position)), collapse=', ')}\n"
), "\n")

cat("Column names:\n")
print(names(pipeline$weekly_usage))
cat("\n")

# Season-level usage coverage -- players per season in training window
coverage_by_season <- pipeline$weekly_usage %>%
  dplyr::filter(week %in% TRAINING_WEEKS_W12) %>%
  dplyr::group_by(season, position) %>%
  dplyr::summarise(
    n_players     = dplyr::n_distinct(player_id),
    mean_usage    = round(mean(usage_share, na.rm = TRUE), 4),
    active_rate   = round(mean(active, na.rm = TRUE), 3),
    .groups       = "drop"
  ) %>%
  dplyr::arrange(season, position)

cat("Training-window usage coverage by season and position:\n")
print(coverage_by_season, n = 30)
cat("\n")

# Top training-window usage shares -- single season spot-check (most recent)
most_recent_season <- max(pipeline$weekly_usage$season)

cat(glue("Top 15 WR usage shares in season {most_recent_season} training window:\n"))
pipeline$weekly_usage %>%
  dplyr::filter(
    season == most_recent_season,
    week   %in% TRAINING_WEEKS_W12,
    position == "WR",
    active
  ) %>%
  dplyr::group_by(player_id, player_name, team) %>%
  dplyr::summarise(
    mean_target_share = round(mean(usage_share, na.rm = TRUE), 3),
    weeks_active      = sum(active),
    .groups           = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(mean_target_share)) %>%
  dplyr::slice_head(n = 15) %>%
  print()
cat("\n")

cat(glue("Top 10 RB touch shares in season {most_recent_season} training window:\n"))
pipeline$weekly_usage %>%
  dplyr::filter(
    season   == most_recent_season,
    week     %in% TRAINING_WEEKS_W12,
    position == "RB",
    active
  ) %>%
  dplyr::group_by(player_id, player_name, team) %>%
  dplyr::summarise(
    mean_touch_share = round(mean(usage_share, na.rm = TRUE), 3),
    weeks_active     = sum(active),
    .groups          = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(mean_touch_share)) %>%
  dplyr::slice_head(n = 10) %>%
  print()
cat("\n")


# ==============================================================================
# SECTION 5: INSPECT RAMP FLAGS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 5: Ramp flag inspection\n")
cat(strrep("=", 60), "\n\n")

cat("Ramp flags dimensions:\n")
cat(glue(
  "  Rows    : {format(nrow(pipeline$ramp_flags), big.mark=',')}\n",
  "  Columns : {ncol(pipeline$ramp_flags)}\n"
), "\n")

cat("Ramp flag summary (games_floor_met == TRUE only):\n")
pipeline$ramp_flags %>%
  dplyr::filter(games_floor_met) %>%
  dplyr::summarise(
    n_player_seasons   = dplyr::n(),
    n_ramp             = sum(ramp_flag),
    pct_ramp           = round(mean(ramp_flag) * 100, 1),
    mean_early_avg     = round(mean(early_avg,      na.rm = TRUE), 4),
    mean_late_avg      = round(mean(late_avg,       na.rm = TRUE), 4),
    mean_relative_chg  = round(mean(relative_change, na.rm = TRUE), 3)
  ) %>%
  print()
cat("\n")

# Distribution of relative changes by position
cat("Relative change distribution by position (floor met):\n")
pipeline$ramp_flags %>%
  dplyr::filter(games_floor_met, !is.na(relative_change)) %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(
    n           = dplyr::n(),
    pct_ramp    = round(mean(ramp_flag) * 100, 1),
    mean_change = round(mean(relative_change), 3),
    median_chg  = round(stats::median(relative_change), 3),
    sd_change   = round(stats::sd(relative_change), 3),
    .groups     = "drop"
  ) %>%
  print()
cat("\n")

# Biggest ramps across all seasons -- sanity check
cat("Largest 10 single-season ramps (by relative_change):\n")
pipeline$ramp_flags %>%
  dplyr::filter(games_floor_met, !is.na(relative_change)) %>%
  dplyr::arrange(dplyr::desc(relative_change)) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::select(player_name, season, position, team,
                early_avg, late_avg, relative_change) %>%
  dplyr::mutate(
    early_avg       = round(early_avg, 3),
    late_avg        = round(late_avg, 3),
    relative_change = round(relative_change, 3)
  ) %>%
  print()
cat("\n")


# ==============================================================================
# SECTION 6: INSPECT GROUP CLASSIFICATION
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 6: Group classification inspection\n")
cat(strrep("=", 60), "\n\n")

cat("Group counts:\n")
pipeline$groups %>%
  dplyr::count(group) %>%
  dplyr::arrange(group) %>%
  print()
cat("\n")

cat("Group counts by position:\n")
pipeline$groups %>%
  dplyr::filter(group %in% c("treatment", "control", "declining")) %>%
  dplyr::count(group, position) %>%
  tidyr::pivot_wider(names_from = position, values_from = n, values_fill = 0L) %>%
  print()
cat("\n")

cat("Group counts by era:\n")
pipeline$groups %>%
  dplyr::filter(group %in% c("treatment", "control")) %>%
  dplyr::count(group, era) %>%
  print()
cat("\n")

cat("Rookie group counts (treatment + control only):\n")
pipeline$groups %>%
  dplyr::filter(rookie_group != "excluded") %>%
  dplyr::count(rookie_group) %>%
  dplyr::arrange(rookie_group) %>%
  print()
cat("\n")

# Mean early and late averages by group -- sanity check
cat("Mean usage by group (early vs late window):\n")
pipeline$groups %>%
  dplyr::filter(group %in% c("treatment", "control", "declining")) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(
    n               = dplyr::n(),
    mean_early_avg  = round(mean(early_avg,       na.rm = TRUE), 4),
    mean_late_avg   = round(mean(late_avg,         na.rm = TRUE), 4),
    mean_rel_change = round(mean(relative_change,  na.rm = TRUE), 3),
    .groups         = "drop"
  ) %>%
  print()
cat("\n")


# ==============================================================================
# SECTION 7: BALANCE CHECK
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 7: Balance check\n")
cat(strrep("=", 60), "\n\n")

cat("Balance summary:\n")
cat(pipeline$balance$summary, "\n\n")

cat("Early-window usage SMD (treatment vs control):\n")
print(pipeline$balance$usage_smd)
cat("\n")

cat("Position distribution by group:\n")
print(pipeline$balance$position_table)
cat("\n")

cat("Era distribution by group:\n")
print(pipeline$balance$era_table)
cat("\n")


# ==============================================================================
# SECTION 8: ASSUMPTION VALIDATION
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 8: Assumption validation\n")
cat(strrep("=", 60), "\n\n")

cat("Assumption check results:\n")
print(pipeline$assumptions$checks)
cat("\n")
cat(pipeline$assumptions$summary, "\n\n")

# Flag any failed checks
failed <- pipeline$assumptions$checks %>%
  dplyr::filter(!result)
if (nrow(failed) > 0L) {
  cat("ATTENTION -- failed assumption checks:\n")
  for (i in seq_len(nrow(failed))) {
    cat(glue("  [WARN] {failed$check[[i]]}: {failed$note[[i]]}\n"))
  }
} else {
  cat("All assumption checks passed.\n")
}
cat("\n")


# ==============================================================================
# SECTION 9: USAGE PERSISTENCE (MECHANISM CHECK)
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 9: Usage persistence (mechanism check)\n")
cat(strrep("=", 60), "\n\n")

cat("Does the usage ramp persist into Weeks 9-18?\n\n")
cat("Persistence summary by group:\n")
print(pipeline$effects$usage_persistence$persistence_summary)
cat("\n")

persist_effect <- pipeline$effects$usage_persistence$persistence_effect
cat(glue(
  "Usage persistence effect (treatment - control):\n",
  "  Estimate : {round(persist_effect$estimate, 4)} share points\n",
  "  95% CI   : [{round(persist_effect$ci_lower, 4)}, {round(persist_effect$ci_upper, 4)}]\n",
  "  Method   : {persist_effect$method}\n",
  "  N trt    : {persist_effect$n_a}\n",
  "  N ctl    : {persist_effect$n_b}\n",
  "  Cohen's d (usage): {round(pipeline$effects$usage_persistence$cohen_d_usage, 3)}\n"
), "\n")

cat(pipeline$effects$usage_persistence$summary, "\n\n")

interpretation_persist <- if (
  !is.na(persist_effect$estimate) && persist_effect$estimate > 0
) {
  "Treatment players maintained higher usage in the second half."
} else if (!is.na(persist_effect$estimate) && persist_effect$estimate < 0) {
  "Control players showed higher second-half usage -- ramp may not have persisted."
} else {
  "Usage persistence is indeterminate."
}
cat(glue("Interpretation: {interpretation_persist}\n\n"))


# ==============================================================================
# SECTION 10: PRIMARY EFFECT ANALYSIS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 10: Primary effect analysis\n")
cat(strrep("=", 60), "\n\n")

cat("Group-level PPR summary (Weeks 9-18):\n")
print(pipeline$effects$group_summary)
cat("\n")

ppg_effect <- pipeline$effects$effect_ppg
cat(glue(
  "Primary PPG effect (treatment - control):\n",
  "  Estimate : {round(ppg_effect$estimate, 2)} PPG\n",
  "  95% CI   : [{round(ppg_effect$ci_lower, 2)}, {round(ppg_effect$ci_upper, 2)}]\n",
  "  Method   : {ppg_effect$method}\n",
  "  N trt    : {ppg_effect$n_a}\n",
  "  N ctl    : {ppg_effect$n_b}\n",
  "  Cohen's d: {round(pipeline$effects$cohens_d_ppg, 3)}\n"
), "\n")

if (!is.null(pipeline$effects$t_test_ppg)) {
  cat(glue(
    "Welch t-test reference:\n",
    "  t  = {round(pipeline$effects$t_test_ppg$statistic, 3)}\n",
    "  df = {round(pipeline$effects$t_test_ppg$parameter, 1)}\n",
    "  p  = {round(pipeline$effects$t_test_ppg$p.value, 4)}\n"
  ), "\n")
}

cat(pipeline$effects$summary, "\n\n")


# ==============================================================================
# SECTION 11: ROOKIE SUBGROUP ANALYSIS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 11: Rookie vs veteran subgroup analysis\n")
cat(strrep("=", 60), "\n\n")

cat("Stratum-level summary (Weeks 9-18 PPG):\n")
print(pipeline$rookie_effects$stratum_summary)
cat("\n")

rookie_eff <- pipeline$rookie_effects$rookie_effect
vet_eff    <- pipeline$rookie_effects$vet_effect

cat(glue(
  "Rookie ramp vs flat effect:\n",
  "  Estimate : {round(rookie_eff$estimate, 2)} PPG\n",
  "  95% CI   : [{round(rookie_eff$ci_lower, 2)}, {round(rookie_eff$ci_upper, 2)}]\n",
  "  N ramp   : {rookie_eff$n_a}\n",
  "  N flat   : {rookie_eff$n_b}\n"
), "\n")

cat(glue(
  "Veteran ramp vs flat effect:\n",
  "  Estimate : {round(vet_eff$estimate, 2)} PPG\n",
  "  95% CI   : [{round(vet_eff$ci_lower, 2)}, {round(vet_eff$ci_upper, 2)}]\n",
  "  N ramp   : {vet_eff$n_a}\n",
  "  N flat   : {vet_eff$n_b}\n"
), "\n")

if (!is.null(pipeline$rookie_effects$interaction_test)) {
  it <- pipeline$rookie_effects$interaction_test
  cat(glue(
    "Interaction test (rookie ramp vs vet ramp PPG):\n",
    "  t  = {round(it$statistic, 3)}\n",
    "  p  = {round(it$p.value, 4)}\n"
  ), "\n")
}

cat(pipeline$rookie_effects$summary, "\n\n")

cat("Within-rookie variation by position:\n")
print(pipeline$rookie_het$by_position)
cat("\n")

cat("Within-rookie variation by era:\n")
print(pipeline$rookie_het$by_era)
cat("\n")

cat("Year 2 extension note:\n")
cat(pipeline$rookie_het$year2_note, "\n\n")


# ==============================================================================
# SECTION 12: HETEROGENEOUS EFFECTS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 12: Heterogeneous effects\n")
cat(strrep("=", 60), "\n\n")

cat("Effect by position:\n")
pipeline$heterogeneous$by_position %>%
  dplyr::mutate(
    estimate = round(estimate, 2),
    ci_lower = round(ci_lower, 2),
    ci_upper = round(ci_upper, 2)
  ) %>%
  print()
cat("\n")

cat("Effect by era:\n")
pipeline$heterogeneous$by_era %>%
  dplyr::mutate(
    estimate = round(estimate, 2),
    ci_lower = round(ci_lower, 2),
    ci_upper = round(ci_upper, 2)
  ) %>%
  print()
cat("\n")

cat("Effect by ramp size quintile (treatment vs control):\n")
pipeline$heterogeneous$by_ramp_size %>%
  dplyr::mutate(
    estimate = round(estimate, 2),
    ci_lower = round(ci_lower, 2),
    ci_upper = round(ci_upper, 2)
  ) %>%
  print()
cat("\n")

cat("BH-adjusted p-values (q < 0.10 flagged):\n")
pipeline$heterogeneous$p_adjusted %>%
  dplyr::filter(!is.na(p_raw)) %>%
  dplyr::mutate(
    p_raw = round(p_raw, 4),
    p_bh  = round(p_bh,  4),
    sig   = p_bh < 0.10
  ) %>%
  dplyr::arrange(p_bh) %>%
  print()
cat("\n")

cat(pipeline$heterogeneous$summary, "\n\n")


# ==============================================================================
# SECTION 13: ROBUSTNESS CHECKS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 13: Robustness checks\n")
cat(strrep("=", 60), "\n\n")

cat("6-spec robustness grid (method x threshold):\n")
pipeline$robustness$results_table %>%
  dplyr::mutate(
    effect_estimate = round(effect_estimate, 2),
    ci_lower        = round(ci_lower, 2),
    ci_upper        = round(ci_upper, 2),
    cohens_d        = round(cohens_d, 3)
  ) %>%
  dplyr::arrange(method, threshold) %>%
  print()
cat("\n")

cat(pipeline$robustness$summary, "\n\n")

# Count how many specs produce an estimate in the same direction as primary
primary_direction <- sign(pipeline$effects$effect_ppg$estimate)
n_same_direction  <- pipeline$robustness$results_table %>%
  dplyr::filter(!is.na(effect_estimate)) %>%
  dplyr::summarise(n = sum(sign(effect_estimate) == primary_direction)) %>%
  dplyr::pull(n)

cat(glue(
  "Directional consistency: {n_same_direction} of ",
  "{sum(!is.na(pipeline$robustness$results_table$effect_estimate))} ",
  "specs agree with primary direction.\n\n"
))


# ==============================================================================
# SECTION 14: WRITE CSV OUTPUTS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("SECTION 14: Writing CSV outputs\n")
cat(strrep("=", 60), "\n\n")

# 1. Group classifications
groups_csv <- file.path(output_dir, "s2_week12_groups.csv")
pipeline$groups %>%
  dplyr::select(
    player_id, player_name, season, team, position,
    early_avg, late_avg, n_early_active, n_late_active,
    relative_change, ramp_flag, games_floor_met,
    group, era, is_rookie, years_exp, rookie_group
  ) %>%
  readr::write_csv(groups_csv)
cat(glue("  Written: {groups_csv}  ({format(nrow(pipeline$groups), big.mark=',')} rows)\n"))

# 2. Ramp flags
ramp_csv <- file.path(output_dir, "s2_week12_ramp_flags.csv")
pipeline$ramp_flags %>%
  dplyr::mutate(
    early_avg       = round(early_avg,       4),
    late_avg        = round(late_avg,        4),
    relative_change = round(relative_change, 4)
  ) %>%
  readr::write_csv(ramp_csv)
cat(glue("  Written: {ramp_csv}  ({format(nrow(pipeline$ramp_flags), big.mark=',')} rows)\n"))

# 3. Balance check table
balance_csv <- file.path(output_dir, "s2_week12_balance.csv")
pipeline$balance$position_table %>%
  dplyr::mutate(pct = round(pct, 4)) %>%
  readr::write_csv(balance_csv)
cat(glue("  Written: {balance_csv}\n"))

# 4. Effects summary -- group means + primary effect
effects_summary <- pipeline$effects$group_summary %>%
  dplyr::mutate(
    mean_ppg   = round(mean_ppg,   2),
    sd_ppg     = round(sd_ppg,     2),
    median_ppg = round(median_ppg, 2)
  ) %>%
  dplyr::bind_rows(
    dplyr::tibble(
      group      = "EFFECT (trt - ctl)",
      n_players  = NA_integer_,
      mean_ppg   = round(pipeline$effects$effect_ppg$estimate, 2),
      sd_ppg     = NA_real_,
      median_ppg = NA_real_
    )
  )
effects_csv <- file.path(output_dir, "s2_week12_effects_summary.csv")
readr::write_csv(effects_summary, effects_csv)
cat(glue("  Written: {effects_csv}\n"))

# 5. Rookie effects stratum summary
rookie_csv <- file.path(output_dir, "s2_week12_rookie_effects.csv")
pipeline$rookie_effects$stratum_summary %>%
  dplyr::mutate(
    mean_ppg   = round(mean_ppg,   2),
    sd_ppg     = round(sd_ppg,     2),
    mean_usage = round(mean_usage, 4)
  ) %>%
  readr::write_csv(rookie_csv)
cat(glue("  Written: {rookie_csv}\n"))

# 6. Heterogeneous effects (all three cuts combined)
het_combined <- dplyr::bind_rows(
  pipeline$heterogeneous$by_position %>%
    dplyr::mutate(cut = "position"),
  pipeline$heterogeneous$by_era %>%
    dplyr::mutate(cut = "era"),
  pipeline$heterogeneous$by_ramp_size %>%
    dplyr::mutate(cut = "ramp_quintile")
) %>%
  dplyr::mutate(
    estimate = round(estimate, 2),
    ci_lower = round(ci_lower, 2),
    ci_upper = round(ci_upper, 2),
    p_value  = round(p_value,  4)
  ) %>%
  dplyr::select(cut, subgroup, n_trt, n_ctl, estimate, ci_lower, ci_upper, p_value)
het_csv <- file.path(output_dir, "s2_week12_heterogeneous.csv")
readr::write_csv(het_combined, het_csv)
cat(glue("  Written: {het_csv}  ({nrow(het_combined)} rows)\n"))

# 7. Robustness grid
robustness_csv <- file.path(output_dir, "s2_week12_robustness.csv")
pipeline$robustness$results_table %>%
  dplyr::mutate(
    effect_estimate = round(effect_estimate, 2),
    ci_lower        = round(ci_lower, 2),
    ci_upper        = round(ci_upper, 2),
    cohens_d        = round(cohens_d, 3)
  ) %>%
  readr::write_csv(robustness_csv)
cat(glue("  Written: {robustness_csv}  ({nrow(pipeline$robustness$results_table)} rows)\n"))

# 8. Usage persistence by group
persistence_csv <- file.path(output_dir, "s2_week12_persistence.csv")
pipeline$effects$usage_persistence$persistence_summary %>%
  dplyr::mutate(
    mean_usage = round(mean_usage, 4),
    sd_usage   = round(sd_usage,   4)
  ) %>%
  readr::write_csv(persistence_csv)
cat(glue("  Written: {persistence_csv}\n"))

cat("\nAll 8 CSV files written to:", output_dir, "\n\n")


# ==============================================================================
# SECTION 15: KEY INSIGHTS
# ==============================================================================
# All values computed from live pipeline output -- nothing hardcoded.

cat(strrep("=", 60), "\n")
cat("KEY INSIGHTS -- Week 12 Usage Ramp Experiment\n")
cat(strrep("=", 60), "\n\n")

n_trt  <- sum(pipeline$groups$group == "treatment", na.rm = TRUE)
n_ctl  <- sum(pipeline$groups$group == "control",   na.rm = TRUE)
n_dec  <- sum(pipeline$groups$group == "declining", na.rm = TRUE)
n_excl <- sum(pipeline$groups$group == "excluded",  na.rm = TRUE)
n_rook <- sum(pipeline$groups$is_rookie &
                pipeline$groups$group %in% c("treatment", "control"),
              na.rm = TRUE)

ppg_est <- round(pipeline$effects$effect_ppg$estimate, 2)
ppg_ci1 <- round(pipeline$effects$effect_ppg$ci_lower, 2)
ppg_ci2 <- round(pipeline$effects$effect_ppg$ci_upper, 2)
d_val   <- round(pipeline$effects$cohens_d_ppg, 3)

persist_est <- round(
  pipeline$effects$usage_persistence$persistence_effect$estimate, 4
)
persist_d <- round(
  pipeline$effects$usage_persistence$cohen_d_usage, 3
)

trt_ppg <- pipeline$effects$group_summary$mean_ppg[
  pipeline$effects$group_summary$group == "treatment"
]
ctl_ppg <- pipeline$effects$group_summary$mean_ppg[
  pipeline$effects$group_summary$group == "control"
]

trt_ppg_fmt <- if (length(trt_ppg) > 0L) round(trt_ppg, 2) else NA_real_
ctl_ppg_fmt <- if (length(ctl_ppg) > 0L) round(ctl_ppg, 2) else NA_real_

rook_est <- round(pipeline$rookie_effects$rookie_effect$estimate, 2)
vet_est  <- round(pipeline$rookie_effects$vet_effect$estimate,   2)

n_checks <- pipeline$assumptions$n_passed

direction <- if (!is.na(ppg_est) && ppg_est > 0) {
  "treatment scored higher"
} else if (!is.na(ppg_est) && ppg_est < 0) {
  "control scored higher (ramp did not predict better outcomes)"
} else {
  "indeterminate"
}

persist_direction <- if (!is.na(persist_est) && persist_est > 0) {
  "ramp persisted into second half"
} else if (!is.na(persist_est) && persist_est < 0) {
  "ramp did not persist"
} else "indeterminate"

cat(glue(
  "GROUP SIZES\n",
  "  Treatment : {format(n_trt,  big.mark=',')}\n",
  "  Control   : {format(n_ctl,  big.mark=',')}\n",
  "  Declining : {format(n_dec,  big.mark=',')}\n",
  "  Excluded  : {format(n_excl, big.mark=',')}\n",
  "  Rookies in T+C: {format(n_rook, big.mark=',')}\n\n",

  "ASSUMPTION CHECKS\n",
  "  {n_checks}/5 passed\n\n",

  "PRIMARY EFFECT (PPR fantasy points, Weeks 9-18)\n",
  "  Treatment mean PPG : {trt_ppg_fmt}\n",
  "  Control mean PPG   : {ctl_ppg_fmt}\n",
  "  Difference         : {ppg_est} PPG\n",
  "  95% CI             : [{ppg_ci1}, {ppg_ci2}]\n",
  "  Cohen's d          : {d_val}\n",
  "  Direction          : {direction}\n\n",

  "USAGE PERSISTENCE (mechanism check)\n",
  "  Difference in Wks 9-18 usage : {persist_est} share points\n",
  "  Cohen's d (usage)            : {persist_d}\n",
  "  Interpretation               : {persist_direction}\n\n",

  "ROOKIE vs VETERAN\n",
  "  Rookie ramp vs flat : {rook_est} PPG\n",
  "  Veteran ramp vs flat: {vet_est} PPG\n\n",

  "ROBUSTNESS\n",
  "  {pipeline$robustness$summary}\n\n",

  "HETEROGENEOUS EFFECTS\n",
  "  {pipeline$heterogeneous$summary}\n"
))

cat("\n")
cat(strrep("=", 60), "\n")
cat("Example script complete.\n")
cat(glue("CSV outputs: {output_dir}\n"))
cat(glue("RDS cache  : {EXAMPLE_CACHE}\n"))
cat(strrep("=", 60), "\n")
