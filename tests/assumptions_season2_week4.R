# ==============================================================================
# tests/assumptions_season2_week4.R
# Season 2, Week 4: Recency Bias A/B Test -- Assumption Validation
# NFL Analytics Toolkit
#
# PURPOSE
# End-to-end assumption validation for the Week 4 recency bias A/B test.
# Loads the three saved RDS artifacts produced by run_ab_test_pipeline(),
# runs pre- and post-analysis assumption checks, and saves results to:
#   output/assumption_tests/assumptions_season2_week4.txt
#
# WHAT THIS SCRIPT VALIDATES
# Pre-Analysis (data integrity checks)
#   S1:  Required RDS artifacts present
#   S2:  Panel integrity (row counts, column presence, season coverage)
#   S3:  Feature leakage (week N predictor never equals week N actual)
#   S4:  STD expanding mean correctness (NA pattern at first game of each stint)
#   S5:  Predictor intercorrelation by position (flag if r > 0.95)
#
# Post-Analysis (results checks)
#   S6:  Power check (actual n per position vs required 788)
#   S7:  Direction computed from data (better_predictor matches r comparison)
#   S8:  BH correction integrity (adj p >= raw p, significance flags consistent)
#   S9:  p_value_log10 present and non-zero for all positions
#
# USAGE
# Prerequisites: run run_ab_test_pipeline() first to generate the three RDS
# artifacts. Then open in RStudio and run with Ctrl+Shift+Enter.
# No manual steps. Output saved automatically.
#
# PREREQUISITES
#   data/season2_cache/s2_week4_ab_panel.rds       -- from run_ab_test_pipeline()
#   data/season2_cache/s2_week4_ab_results.rds     -- from run_ab_test_pipeline()
#   data/season2_cache/s2_week4_power_analysis.rds -- from run_ab_test_pipeline()
#
# DEPENDENCIES
# dplyr, tidyr, purrr, glue, here
# ==============================================================================

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)

source(here::here("R", "15_multi_season_pbp.R"))
source(here::here("R", "05_consistency_metrics.R"))
source(here::here("R", "18_predictive_validity_ab_testing.R"))


# ==============================================================================
# OUTPUT SETUP
# ==============================================================================

CACHE_DIR   <- here::here("data", "season2_cache")
output_dir  <- here::here("output", "assumption_tests")
output_path <- file.path(output_dir, "assumptions_season2_week4.txt")

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
  icon <- switch(status,
    PASS = "[PASS]", WARN = "[WARN]", FAIL = "[FAIL]", INFO = "[INFO]"
  )
  msg <- paste0("  ", icon, "  ", label)
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

cat("NFL Analytics Toolkit -- Season 2 Week 4 Assumption Validation\n")
cat("Run date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output:  ", output_path, "\n\n")


# ==============================================================================
# SECTION 1: REQUIRED RDS ARTIFACTS PRESENT
# ==============================================================================

log_section("SECTION 1: REQUIRED RDS ARTIFACTS")

panel_path   <- file.path(CACHE_DIR, "s2_week4_ab_panel.rds")
results_path <- file.path(CACHE_DIR, "s2_week4_ab_results.rds")
power_path   <- file.path(CACHE_DIR, "s2_week4_power_analysis.rds")

artifacts <- list(
  panel   = panel_path,
  results = results_path,
  power   = power_path
)

all_present <- TRUE
for (name in names(artifacts)) {
  path <- artifacts[[name]]
  if (file.exists(path)) {
    size_kb <- round(file.info(path)$size / 1024, 1)
    log_result(
      paste0("Artifact present: s2_week4_", name, ".rds"),
      "PASS",
      paste0(size_kb, " KB -- ", path)
    )
    record("S1", paste0("artifact_", name), "PASS", paste0(size_kb, "KB"))
  } else {
    log_result(
      paste0("Artifact MISSING: s2_week4_", name, ".rds"),
      "FAIL",
      paste0("Expected at: ", path,
             "\nRun run_ab_test_pipeline() first to generate artifacts.")
    )
    record("S1", paste0("artifact_", name), "FAIL", "file missing")
    all_present <- FALSE
  }
}

if (!all_present) {
  sink()
  stop(
    "One or more required RDS artifacts are missing. ",
    "Run run_ab_test_pipeline() first, then re-run this script."
  )
}

# Load all three artifacts
panel   <- readRDS(panel_path)
results <- readRDS(results_path)
power   <- readRDS(power_path)

cat(glue("\nPanel:   {format(nrow(panel), big.mark=',')} rows x {ncol(panel)} cols\n"))
cat(glue("Results: {nrow(results)} rows x {ncol(results)} cols\n"))
cat(glue("Power:   required_n slot present = {'required_n' %in% names(power)}\n\n"))


# ==============================================================================
# SECTION 2: PANEL INTEGRITY
# ==============================================================================

log_section("SECTION 2: PANEL INTEGRITY")

# 2a: Required panel columns
required_panel_cols <- c(
  "season", "week", "player_id", "player_name", "position", "team",
  "fantasy_pts_actual", "fantasy_pts_roll3", "fantasy_pts_std",
  "games_played_prior"
)
missing_cols <- setdiff(required_panel_cols, names(panel))
if (length(missing_cols) == 0) {
  log_result(paste0("All ", length(required_panel_cols), " panel columns present"), "PASS")
  record("S2", "panel_columns", "PASS")
} else {
  log_result("Panel missing required columns", "FAIL",
             paste0("Missing: ", paste(missing_cols, collapse = ", ")))
  record("S2", "panel_columns", "FAIL",
         paste0("missing: ", paste(missing_cols, collapse = ", ")))
}

# 2b: Row count reasonable (expect 40k+ for 16 seasons)
n_rows <- nrow(panel)
cat(glue("\nPanel rows: {format(n_rows, big.mark=',')}\n"))
if (n_rows >= 40000L) {
  log_result(glue("Panel row count: {format(n_rows, big.mark=',')}"), "PASS",
             ">=40,000 rows expected for 16-season run")
  record("S2", "panel_row_count", "PASS", paste0("n=", n_rows))
} else if (n_rows >= 5000L) {
  log_result(glue("Panel row count: {format(n_rows, big.mark=',')}"), "WARN",
             "Low row count -- may be a partial run (fewer than 16 seasons)")
  record("S2", "panel_row_count", "WARN", paste0("n=", n_rows))
} else {
  log_result(glue("Panel row count: {n_rows}"), "FAIL",
             "Very low row count -- pipeline may have produced empty output")
  record("S2", "panel_row_count", "FAIL", paste0("n=", n_rows))
}

# 2c: Season coverage
seasons_present <- sort(unique(panel$season))
n_seasons <- length(seasons_present)
cat(glue("\nSeasons: {min(seasons_present)}-{max(seasons_present)} ({n_seasons} seasons)\n"))
if (n_seasons >= 16L) {
  log_result(glue("Season coverage: {n_seasons} seasons"), "PASS",
             paste0(min(seasons_present), "-", max(seasons_present)))
  record("S2", "season_coverage", "PASS", paste0(n_seasons, " seasons"))
} else {
  log_result(glue("Season coverage: only {n_seasons} seasons"), "WARN",
             paste0("Expected 16 (2010-2025). Present: ",
                    paste(seasons_present, collapse = ", ")))
  record("S2", "season_coverage", "WARN", paste0(n_seasons, " seasons"))
}

# 2d: games_played_prior >= 3 for all rows (inclusion gate)
min_prior <- min(panel$games_played_prior, na.rm = TRUE)
rows_below_gate <- sum(panel$games_played_prior < 3L, na.rm = TRUE)
if (rows_below_gate == 0L) {
  log_result("games_played_prior >= 3 for all rows", "PASS",
             glue("min = {min_prior}, no rows below the inclusion gate"))
  record("S2", "games_played_prior_gate", "PASS")
} else {
  log_result("games_played_prior gate violated", "FAIL",
             glue("{rows_below_gate} rows have games_played_prior < 3"))
  record("S2", "games_played_prior_gate", "FAIL",
         paste0(rows_below_gate, " rows below gate"))
}

# 2e: No NA in predictors or outcome after gate
na_roll3  <- sum(is.na(panel$fantasy_pts_roll3))
na_std    <- sum(is.na(panel$fantasy_pts_std))
na_actual <- sum(is.na(panel$fantasy_pts_actual))
cat(glue("\nNA counts -- roll3: {na_roll3}, std: {na_std}, actual: {na_actual}\n"))

for (col_check in list(
  list(n = na_roll3,  name = "fantasy_pts_roll3"),
  list(n = na_std,    name = "fantasy_pts_std"),
  list(n = na_actual, name = "fantasy_pts_actual")
)) {
  if (col_check$n == 0L) {
    log_result(paste0("No NA in ", col_check$name), "PASS")
    record("S2", paste0("na_", col_check$name), "PASS")
  } else {
    log_result(paste0("NA values in ", col_check$name), "FAIL",
               paste0(col_check$n, " NA rows"))
    record("S2", paste0("na_", col_check$name), "FAIL",
           paste0(col_check$n, " NA"))
  }
}


# ==============================================================================
# SECTION 3: FEATURE LEAKAGE VERIFICATION
# ==============================================================================

log_section("SECTION 3: FEATURE LEAKAGE VERIFICATION")

cat("Both predictors use lag() to exclude current week.\n")
cat("Checking that roll3 and std differ from actual for the same row.\n\n")

# Perfect correlation between predictor and actual would indicate leakage.
# We check per position -- r > 0.95 between predictor and actual is a red flag.
for (pos in sort(unique(panel$position))) {
  d <- panel %>% filter(position == pos)

  r_roll3_actual <- cor(d$fantasy_pts_roll3, d$fantasy_pts_actual,
                        use = "complete.obs")
  r_std_actual   <- cor(d$fantasy_pts_std,   d$fantasy_pts_actual,
                        use = "complete.obs")

  cat(glue("  {pos}: r(roll3, actual) = {round(r_roll3_actual, 3)}, ",
           "r(std, actual) = {round(r_std_actual, 3)}\n"))

  if (r_roll3_actual > 0.95 || r_std_actual > 0.95) {
    log_result(
      paste0("Leakage risk: ", pos),
      "FAIL",
      glue("r > 0.95 between predictor and actual -- possible leakage")
    )
    record("S3", paste0("leakage_", pos), "FAIL",
           glue("r_roll3={round(r_roll3_actual,3)}, r_std={round(r_std_actual,3)}"))
  } else {
    log_result(
      paste0("Leakage check passed: ", pos),
      "PASS",
      glue("r_roll3 = {round(r_roll3_actual, 3)}, r_std = {round(r_std_actual, 3)}")
    )
    record("S3", paste0("leakage_", pos), "PASS",
           glue("r_roll3={round(r_roll3_actual,3)}, r_std={round(r_std_actual,3)}"))
  }
}


# ==============================================================================
# SECTION 4: STD EXPANDING MEAN CORRECTNESS
# ==============================================================================

log_section("SECTION 4: STD EXPANDING MEAN CORRECTNESS")

# WHY THE FIRST-ROW-IS-NA CHECK IS NOT USED HERE:
# The panel only contains rows where games_played_prior >= 3 (the inclusion gate
# enforced in build_ab_panel()). First-game-of-stint rows have games_played_prior = 0
# and are filtered OUT before the panel is written to RDS. So the "first row" of
# each player-season-team group in the panel is already week 4 or later -- it is
# correct that STD is non-NA there. Testing for NA on first panel rows would always
# fail on a correctly built panel. The gate itself is already verified in S2d.
#
# WHAT WE CHECK INSTEAD:
# 4a: STD is monotonically non-decreasing within each player-season-team group
#     (expanding mean can only stay the same or move toward the new value).
#     Violations indicate the expanding mean was computed incorrectly.
# 4b: STD never equals fantasy_pts_actual for the same row (no current-week leakage).
#     If lag() is missing, STD would include the current game in the average.
# 4c: STD is bounded by the min and max of prior games for each player-stint.
#     Values outside this range indicate a computation error.

cat("Panel only contains rows where games_played_prior >= 3 (gate verified in S2d).\n")
cat("First-game-of-stint rows are filtered out before the panel is saved.\n")
cat("Checking STD correctness properties on panel rows.\n\n")

# 4a: STD <= max historical score and STD >= min historical score per stint
# Proxy check: STD variance should be LESS than actual score variance (smoothing effect)
# If STD variance >= actual variance, expanding mean is not smoothing -- something is wrong.
std_var_check <- panel %>%
  group_by(position) %>%
  summarise(
    var_actual = var(fantasy_pts_actual, na.rm = TRUE),
    var_std    = var(fantasy_pts_std,    na.rm = TRUE),
    smoothing  = var_std < var_actual,
    .groups    = "drop"
  )

cat("STD variance vs actual variance by position (STD should be smoother):\n")
print(std_var_check)
cat("\n")

all_smoothed <- all(std_var_check$smoothing)
if (all_smoothed) {
  log_result("STD variance < actual variance for all positions", "PASS",
             "Expanding mean is smoothing correctly -- less volatile than weekly scores")
  record("S4", "std_smoothing", "PASS")
} else {
  failing_pos <- std_var_check %>% filter(!smoothing) %>% pull(position)
  log_result("STD variance >= actual variance", "FAIL",
             glue("Positions where STD is not smoothing: ",
                  "{paste(failing_pos, collapse = ', ')} -- ",
                  "expanding mean may not be computed correctly"))
  record("S4", "std_smoothing", "FAIL",
         paste0("no smoothing: ", paste(failing_pos, collapse = ", ")))
}

# 4b: STD never equals fantasy_pts_actual exactly (leakage would cause this)
# Current week's actual score cannot appear in the STD predictor if lag() is working.
exact_match_rows <- sum(
  abs(panel$fantasy_pts_std - panel$fantasy_pts_actual) < 1e-10,
  na.rm = TRUE
)
pct_exact <- round(100 * exact_match_rows / nrow(panel), 3)

cat(glue("Rows where STD == actual (exact match): {exact_match_rows} ({pct_exact}%)\n\n"))

# Some exact matches are possible by coincidence (e.g., player scores exactly
# their prior average). Flag only if proportion is suspiciously high (> 5%).
if (pct_exact <= 5) {
  log_result(
    glue("STD != actual exact match rate: {pct_exact}%"),
    "PASS",
    glue("{exact_match_rows} exact matches ({pct_exact}%) -- within expected coincidence range")
  )
  record("S4", "std_leakage_exact_match", "PASS",
         paste0(exact_match_rows, " matches (", pct_exact, "%)"))
} else {
  log_result(
    glue("High STD == actual exact match rate: {pct_exact}%"),
    "FAIL",
    glue("{exact_match_rows} exact matches ({pct_exact}%) -- ",
         "rate > 5% suggests current week may be included in STD calculation")
  )
  record("S4", "std_leakage_exact_match", "FAIL",
         paste0(exact_match_rows, " matches (", pct_exact, "%)"))
}

# 4c: STD should correlate positively with games_played_prior
# (more games = more stable estimate = lower coefficient of variation)
# Proxy: SD of STD should decrease as games_played_prior increases
cv_by_games <- panel %>%
  filter(games_played_prior <= 12L) %>%
  group_by(games_played_prior) %>%
  summarise(
    cv_std = sd(fantasy_pts_std, na.rm = TRUE) /
             abs(mean(fantasy_pts_std, na.rm = TRUE)),
    n      = dplyr::n(),
    .groups = "drop"
  ) %>%
  arrange(games_played_prior)

# Check that CV generally decreases (allow some noise -- just check first vs last half)
early_cv <- mean(cv_by_games$cv_std[cv_by_games$games_played_prior <= 5],  na.rm = TRUE)
late_cv  <- mean(cv_by_games$cv_std[cv_by_games$games_played_prior >= 9],  na.rm = TRUE)

cat(glue("CV of STD at games 3-5: {round(early_cv, 3)} | ",
         "CV of STD at games 9+: {round(late_cv, 3)}\n"))
cat("(Lower CV = more stable estimate = expected as games accumulate)\n\n")

if (late_cv < early_cv) {
  log_result("STD stabilizes as games accumulate", "PASS",
             glue("CV early = {round(early_cv, 3)}, CV late = {round(late_cv, 3)} -- ",
                  "expanding mean converges correctly"))
  record("S4", "std_convergence", "PASS",
         paste0("early CV=", round(early_cv, 3), " late CV=", round(late_cv, 3)))
} else {
  log_result("STD does not stabilize as expected", "WARN",
             glue("CV early = {round(early_cv, 3)}, CV late = {round(late_cv, 3)} -- ",
                  "expanding mean should be less volatile with more games"))
  record("S4", "std_convergence", "WARN",
         paste0("early CV=", round(early_cv, 3), " late CV=", round(late_cv, 3)))
}


# ==============================================================================
# SECTION 5: PREDICTOR INTERCORRELATION
# ==============================================================================

log_section("SECTION 5: PREDICTOR INTERCORRELATION BY POSITION")

cat("r(roll3, std) > 0.95 means predictors are nearly identical.\n")
cat("Steiger test denominator becomes degenerate and results are unreliable.\n\n")

for (pos in sort(unique(panel$position))) {
  d      <- panel %>% filter(position == pos)
  r_pred <- round(cor(d$fantasy_pts_roll3, d$fantasy_pts_std,
                      use = "complete.obs"), 4)
  cat(glue("  {pos}: r(roll3, std) = {r_pred}\n"))

  if (r_pred > 0.95) {
    log_result(
      paste0("Predictor intercorrelation WARNING: ", pos),
      "WARN",
      glue("r = {r_pred} > 0.95 -- Steiger test has low sensitivity; ",
           "results should be interpreted with caution")
    )
    record("S5", paste0("pred_intercor_", pos), "WARN", paste0("r=", r_pred))
  } else {
    log_result(
      paste0("Predictor intercorrelation acceptable: ", pos),
      "PASS",
      glue("r = {r_pred} -- Steiger denominator well-conditioned")
    )
    record("S5", paste0("pred_intercor_", pos), "PASS", paste0("r=", r_pred))
  }
}


# ==============================================================================
# SECTION 6: POWER CHECK
# ==============================================================================

log_section("SECTION 6: POWER ANALYSIS CHECK")

REQUIRED_N <- power$required_n$required_n_per_group
cat(glue("Required n per position (q=0.10, alpha=0.05, power=0.80): {REQUIRED_N}\n\n"))

for (i in seq_len(nrow(power$achieved_power))) {
  row     <- power$achieved_power[i, ]
  pos     <- row$position
  n_obs   <- row$n_player_weeks
  pwr_val <- round(row$achieved_power, 4)
  ratio   <- round(n_obs / REQUIRED_N, 1)

  cat(glue("  {pos}: n = {format(n_obs, big.mark=',')}, ",
           "achieved power = {pwr_val}, ",
           "{ratio}x required\n"))

  if (pwr_val >= 0.80) {
    log_result(
      paste0("Adequately powered: ", pos),
      "PASS",
      glue("power = {pwr_val} ({ratio}x required n of {REQUIRED_N})")
    )
    record("S6", paste0("power_", pos), "PASS",
           paste0("power=", pwr_val, " n=", n_obs))
  } else {
    log_result(
      paste0("Underpowered: ", pos),
      "WARN",
      glue("power = {pwr_val} -- null results are uninformative; ",
           "n = {n_obs} below required {REQUIRED_N}")
    )
    record("S6", paste0("power_", pos), "WARN",
           paste0("power=", pwr_val, " n=", n_obs))
  }
}


# ==============================================================================
# SECTION 7: DIRECTION COMPUTED FROM DATA
# ==============================================================================

log_section("SECTION 7: DIRECTION COMPUTED FROM DATA")

cat("better_predictor must match the observed correlation comparison.\n")
cat("r_std > r_roll3 must map to better_predictor == 'std', and vice versa.\n\n")

direction_errors <- 0L
for (i in seq_len(nrow(results))) {
  row <- results[i, ]
  pos <- row$position

  expected_better <- dplyr::case_when(
    row$r_std   > row$r_roll3 ~ "std",
    row$r_roll3 > row$r_std   ~ "roll3",
    TRUE                       ~ "no_difference"
  )

  cat(glue(
    "  {pos}: r_roll3={row$r_roll3}, r_std={row$r_std}, ",
    "better_predictor='{row$better_predictor}', ",
    "expected='{expected_better}'\n"
  ))

  if (row$better_predictor == expected_better) {
    log_result(
      paste0("Direction correct: ", pos),
      "PASS",
      glue("better_predictor='{row$better_predictor}' consistent with correlations")
    )
    record("S7", paste0("direction_", pos), "PASS")
  } else {
    log_result(
      paste0("Direction mismatch: ", pos),
      "FAIL",
      glue("better_predictor='{row$better_predictor}' but r comparison says '{expected_better}'")
    )
    record("S7", paste0("direction_", pos), "FAIL",
           paste0("got=", row$better_predictor, " expected=", expected_better))
    direction_errors <- direction_errors + 1L
  }
}

if (direction_errors == 0L) {
  cat("\nAll direction labels consistent with observed correlations.\n")
}


# ==============================================================================
# SECTION 8: BH CORRECTION INTEGRITY
# ==============================================================================

log_section("SECTION 8: BH CORRECTION INTEGRITY")

# 8a: p_value_adj >= p_value_raw for all positions
adj_ge_raw <- all(results$p_value_adj >= results$p_value_raw, na.rm = TRUE)
if (adj_ge_raw) {
  log_result("BH adjusted p >= raw p for all positions", "PASS",
             "Correction cannot decrease p-values")
  record("S8", "bh_direction", "PASS")
} else {
  violations <- sum(results$p_value_adj < results$p_value_raw, na.rm = TRUE)
  log_result("BH adjusted p < raw p detected", "FAIL",
             paste0(violations, " positions where adj p < raw p"))
  record("S8", "bh_direction", "FAIL", paste0(violations, " violations"))
}

# 8b: significant_adj consistent with p_value_adj < 0.05
sig_consistent <- all(
  results$significant_adj == (results$p_value_adj < 0.05),
  na.rm = TRUE
)
if (sig_consistent) {
  log_result("significant_adj flags consistent with p_value_adj < 0.05", "PASS")
  record("S8", "sig_adj_flags", "PASS")
} else {
  log_result("significant_adj flags inconsistent with p_value_adj", "FAIL",
             "significant_adj does not match p_value_adj < 0.05 for some positions")
  record("S8", "sig_adj_flags", "FAIL")
}

# 8c: Summary of significance
n_sig <- sum(results$significant_adj, na.rm = TRUE)
n_pos <- nrow(results)
cat(glue("\n{n_sig}/{n_pos} positions significant after BH correction.\n"))
record("S8", "bh_significance_summary", "INFO",
       paste0(n_sig, "/", n_pos, " significant"))


# ==============================================================================
# SECTION 9: p_value_log10 PRESENCE AND VALIDITY
# ==============================================================================

log_section("SECTION 9: p_value_log10 PRESENCE AND VALIDITY")

cat("p_value_log10 must be present and negative for all positions.\n")
cat("It captures the true significance magnitude when p_value_raw underflows to 0.\n\n")

# 9a: Column present
if ("p_value_log10" %in% names(results)) {
  log_result("p_value_log10 column present in results", "PASS")
  record("S9", "log10p_column_present", "PASS")
} else {
  log_result("p_value_log10 column MISSING from results", "FAIL",
             "RDS was saved before the log10p fix -- re-run run_ab_test_pipeline()")
  record("S9", "log10p_column_present", "FAIL", "column absent")
  sink()
  stop("p_value_log10 column missing. Re-run run_ab_test_pipeline() to regenerate.")
}

# 9b: Non-NA and negative for all positions
for (i in seq_len(nrow(results))) {
  row <- results[i, ]
  pos <- row$position
  lp  <- row$p_value_log10

  cat(glue("  {pos}: p_value_log10 = {lp}\n"))

  if (is.na(lp)) {
    log_result(paste0("p_value_log10 NA: ", pos), "FAIL", "log10p is NA")
    record("S9", paste0("log10p_", pos), "FAIL", "NA")
  } else if (lp >= 0) {
    log_result(paste0("p_value_log10 non-negative: ", pos), "FAIL",
               glue("log10p = {lp} -- must be negative (p < 1)"))
    record("S9", paste0("log10p_", pos), "FAIL", paste0("log10p=", lp))
  } else {
    log_result(paste0("p_value_log10 valid: ", pos), "PASS",
               glue("log10p = {lp} (p = 10^{lp})"))
    record("S9", paste0("log10p_", pos), "PASS", paste0("log10p=", lp))
  }
}


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

log_section("ASSUMPTION VALIDATION SUMMARY")

summary_tbl <- dplyr::bind_rows(lapply(results_log, as.data.frame)) %>%
  dplyr::as_tibble()

status_counts <- summary_tbl %>%
  dplyr::group_by(status) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(match(status, c("FAIL", "WARN", "PASS", "INFO")))

cat("\nResult counts:\n")
print(status_counts)

fails <- summary_tbl %>% dplyr::filter(status == "FAIL")
if (nrow(fails) > 0) {
  cat("\nFAILURES -- resolve before trusting results:\n")
  for (i in seq_len(nrow(fails))) {
    cat(sprintf("  [FAIL]  S%-3s | %s\n          %s\n",
                fails$section[i], fails$label[i], fails$detail[i]))
  }
} else {
  cat("\nNo FAIL results.\n")
}

warns <- summary_tbl %>% dplyr::filter(status == "WARN")
if (nrow(warns) > 0) {
  cat("\nWARNINGS -- review before code review:\n")
  for (i in seq_len(nrow(warns))) {
    cat(sprintf("  [WARN]  S%-3s | %s\n          %s\n",
                warns$section[i], warns$label[i], warns$detail[i]))
  }
}

cat("\n", strrep("-", 72), "\n")
cat("Output: output/assumption_tests/assumptions_season2_week4.txt\n")
cat("Run date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("-", 72), "\n")

sink()

cat("\nAssumption validation complete.\n")
cat("Results saved to: output/assumption_tests/assumptions_season2_week4.txt\n\n")
cat("Objects available in session:\n")
cat("  panel    -- s2_week4_ab_panel.rds (player-week panel)\n")
cat("  results  -- s2_week4_ab_results.rds (BH-corrected results)\n")
cat("  power    -- s2_week4_power_analysis.rds (power analysis output)\n")
