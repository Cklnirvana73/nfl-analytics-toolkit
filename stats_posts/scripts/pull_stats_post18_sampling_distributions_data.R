# ==============================================================================
# Statistics Series -- Post 18: Sampling Distributions
# Data pull runner
#
# Purpose: R/25's .bootstrap_mean_diff_ci() builds a 1,000-element vector of
#          resampled mean differences, passes it to two quantile() calls, and
#          discards it at function exit. This runner rebuilds the exact same
#          inputs, reruns the identical bootstrap logic, and RETAINS the
#          replicate vector so the sampling distribution can be described and
#          plotted.
#
# Reproduction guarantee: .bootstrap_mean_diff_ci() calls set.seed(42L)
#          immediately before replicate(). This runner does the same, in the
#          same position, with the same B. The retained vector is therefore the
#          same vector the delivered CI was computed from, not a fresh draw.
#
# Inputs (written by run_week11_pipeline(), no pipeline rerun needed):
#   data/season2_cache/s2_week11_weekly_fantasy.rds
#   data/season2_cache/s2_week11_groups.rds
#
# Outputs: console only. Nothing written to disk.
#
# Run from the project root so here::here() resolves.
# ==============================================================================

library(dplyr)
library(here)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Mirrors R/25 constants. Defined locally rather than sourced so this runner
# does not pull in R/15 and R/17. Verified against R/25 lines 116-176.
OUTCOME_WEEKS         <- 9L:18L
MIN_OUTCOME_GAMES     <- 3L
MIN_PRIOR_SEASON_GAMES <- 4L
BOOTSTRAP_B           <- 1000L
CONF_LEVEL            <- 0.95
BOOT_SEED             <- 42L

CACHE_DIR <- here::here("data", "season2_cache")

cat("\n")
cat("==============================================================\n")
cat("POST 18 RUNNER: SAMPLING DISTRIBUTIONS\n")
cat("==============================================================\n\n")

# ==============================================================================
# STEP 1: LOAD CACHED PIPELINE OBJECTS
# ==============================================================================

path_fantasy <- file.path(CACHE_DIR, "s2_week11_weekly_fantasy.rds")
path_groups  <- file.path(CACHE_DIR, "s2_week11_groups.rds")

for (p in c(path_fantasy, path_groups)) {
  if (!file.exists(p)) {
    stop(paste0(
      "Cache file not found: ", p, "\n",
      "Run run_week11_pipeline() first, or correct CACHE_DIR."
    ), call. = FALSE)
  }
}

weekly_fantasy <- readRDS(path_fantasy)
groups         <- readRDS(path_groups)

cat("STEP 1: CACHE LOADED\n")
cat("  weekly_fantasy rows :", format(nrow(weekly_fantasy), big.mark = ","), "\n")
cat("  groups rows         :", format(nrow(groups), big.mark = ","), "\n\n")

# ==============================================================================
# STEP 2: REBUILD player_level (verbatim from calculate_injury_effect)
# ==============================================================================

trt_ctrl_ids <- groups %>%
  dplyr::filter(group %in% c("treatment", "control"))

prior_ppg <- weekly_fantasy %>%
  dplyr::group_by(player_id, season) %>%
  dplyr::summarise(
    prior_ppg            = mean(total_fantasy_points, na.rm = TRUE),
    n_prior_season_games = dplyr::n(),
    .groups              = "drop"
  ) %>%
  dplyr::filter(n_prior_season_games >= MIN_PRIOR_SEASON_GAMES) %>%
  dplyr::mutate(season = season + 1L) %>%
  dplyr::select(player_id, season, prior_ppg, n_prior_season_games)

outcome_ppg <- weekly_fantasy %>%
  dplyr::filter(week %in% OUTCOME_WEEKS) %>%
  dplyr::group_by(player_id, season) %>%
  dplyr::summarise(
    outcome_ppg     = mean(total_fantasy_points, na.rm = TRUE),
    n_outcome_games = dplyr::n(),
    .groups         = "drop"
  ) %>%
  dplyr::filter(n_outcome_games >= MIN_OUTCOME_GAMES)

player_level <- trt_ctrl_ids %>%
  dplyr::select(player_id, player_name, season, group, position,
                era, return_week) %>%
  dplyr::inner_join(prior_ppg,   by = c("player_id", "season")) %>%
  dplyr::inner_join(outcome_ppg, by = c("player_id", "season")) %>%
  dplyr::mutate(
    prediction_error = outcome_ppg - prior_ppg,
    abs_error        = abs(prediction_error)
  )

trt_mae <- player_level %>% dplyr::filter(group == "treatment") %>% dplyr::pull(abs_error)
ctl_mae <- player_level %>% dplyr::filter(group == "control")   %>% dplyr::pull(abs_error)

trt_mae <- trt_mae[!is.na(trt_mae)]
ctl_mae <- ctl_mae[!is.na(ctl_mae)]

n_a <- length(trt_mae)
n_b <- length(ctl_mae)

cat("STEP 2: player_level REBUILT\n")
cat("  treatment player-seasons :", n_a, "\n")
cat("  control player-seasons   :", n_b, "\n\n")

# --- Branch check: did the bootstrap actually run in the delivered result? ----

if (min(n_a, n_b) < 10L) {
  cat("  *** WELCH FALLBACK BRANCH ***\n")
  cat("  min(n_a, n_b) is below 10, so .bootstrap_mean_diff_ci() returned a\n")
  cat("  Welch t-interval. NO BOOTSTRAP RAN in the delivered result.\n")
  cat("  The post premise needs revisiting. Stopping here.\n\n")
  stop("Welch fallback branch. Bootstrap did not run.", call. = FALSE)
}

cat("  Bootstrap branch confirmed (both group sizes >= 10).\n\n")

# ==============================================================================
# STEP 3: THE DATA DISTRIBUTION (what gets resampled)
# ==============================================================================

obs_diff <- mean(trt_mae) - mean(ctl_mae)

cat("STEP 3: THE DATA DISTRIBUTION\n")
cat("  Treatment absolute error, PPG\n")
cat("    mean   :", round(mean(trt_mae), 3), "\n")
cat("    median :", round(median(trt_mae), 3), "\n")
cat("    sd     :", round(sd(trt_mae), 3), "\n")
cat("    min    :", round(min(trt_mae), 3), "\n")
cat("    max    :", round(max(trt_mae), 3), "\n")
cat("  Control absolute error, PPG\n")
cat("    mean   :", round(mean(ctl_mae), 3), "\n")
cat("    median :", round(median(ctl_mae), 3), "\n")
cat("    sd     :", round(sd(ctl_mae), 3), "\n")
cat("    min    :", round(min(ctl_mae), 3), "\n")
cat("    max    :", round(max(ctl_mae), 3), "\n")
cat("  Observed difference in means (trt - ctl):", round(obs_diff, 4), "\n\n")

# Shape of the raw data. abs_error has a floor at zero and a right tail.
skew_fn <- function(x) {
  m <- mean(x); s <- sd(x); n <- length(x)
  sum((x - m)^3) / (n * s^3)
}

cat("  Skewness of raw absolute error\n")
cat("    treatment :", round(skew_fn(trt_mae), 3), "\n")
cat("    control   :", round(skew_fn(ctl_mae), 3), "\n\n")

# ==============================================================================
# STEP 4: THE SAMPLING DISTRIBUTION (retained)
# ==============================================================================
# Identical to .bootstrap_mean_diff_ci() internals, with boot_diffs retained.

set.seed(BOOT_SEED)
boot_diffs <- replicate(BOOTSTRAP_B, {
  mean(sample(trt_mae, n_a, replace = TRUE)) -
    mean(sample(ctl_mae, n_b, replace = TRUE))
})

alpha <- 1 - CONF_LEVEL

cat("STEP 4: THE SAMPLING DISTRIBUTION\n")
cat("  replicates retained :", length(boot_diffs), "\n")
cat("    mean   :", round(mean(boot_diffs), 4), "\n")
cat("    median :", round(median(boot_diffs), 4), "\n")
cat("    sd (bootstrap standard error) :", round(sd(boot_diffs), 4), "\n")
cat("    min    :", round(min(boot_diffs), 4), "\n")
cat("    max    :", round(max(boot_diffs), 4), "\n")
cat("    skewness :", round(skew_fn(boot_diffs), 3), "\n\n")

cat("  Quantiles of the sampling distribution\n")
qs <- quantile(boot_diffs, c(0.025, 0.10, 0.25, 0.50, 0.75, 0.90, 0.975))
for (i in seq_along(qs)) {
  cat("   ", names(qs)[i], ":", round(unname(qs[i]), 4), "\n")
}
cat("\n")

# Spread contrast: the data vs the statistic
pooled_data_sd <- sd(c(trt_mae, ctl_mae))
cat("  SPREAD CONTRAST (the core of the concept)\n")
cat("    sd of the pooled raw data      :", round(pooled_data_sd, 4), "\n")
cat("    sd of the resampled statistic  :", round(sd(boot_diffs), 4), "\n")
cat("    ratio (data sd / statistic sd) :",
    round(pooled_data_sd / sd(boot_diffs), 2), "\n\n")

# Normality of the sampling distribution vs the raw data
sw_boot <- shapiro.test(if (length(boot_diffs) > 5000)
  sample(boot_diffs, 5000) else boot_diffs)
sw_trt  <- shapiro.test(if (length(trt_mae) > 5000)
  sample(trt_mae, 5000) else trt_mae)

cat("  SHAPE CONTRAST\n")
cat("    Shapiro-Wilk on raw treatment abs_error : W =",
    round(sw_trt$statistic, 4), " p =", format.pval(sw_trt$p.value, digits = 3), "\n")
cat("    Shapiro-Wilk on the 1,000 replicates    : W =",
    round(sw_boot$statistic, 4), " p =", format.pval(sw_boot$p.value, digits = 3), "\n\n")

# ==============================================================================
# STEP 5: THE TWO NUMBERS THAT SURVIVED
# ==============================================================================

ci_lower_basic <- 2 * obs_diff - unname(quantile(boot_diffs, 1 - alpha / 2))
ci_upper_basic <- 2 * obs_diff - unname(quantile(boot_diffs, alpha / 2))

ci_lower_perc <- unname(quantile(boot_diffs, alpha / 2))
ci_upper_perc <- unname(quantile(boot_diffs, 1 - alpha / 2))

tt <- t.test(trt_mae, ctl_mae)

cat("STEP 5: WHAT SURVIVED THE FUNCTION\n")
cat("  Delivered CI, basic / reverse-percentile (R/25 method)\n")
cat("    [", round(ci_lower_basic, 4), ",", round(ci_upper_basic, 4), "]\n")
cat("    width :", round(ci_upper_basic - ci_lower_basic, 4), "\n")
cat("  Percentile CI, same replicates, different rule\n")
cat("    [", round(ci_lower_perc, 4), ",", round(ci_upper_perc, 4), "]\n")
cat("    width :", round(ci_upper_perc - ci_lower_perc, 4), "\n")
cat("  Welch t-test CI, formula not resampling\n")
cat("    [", round(tt$conf.int[1], 4), ",", round(tt$conf.int[2], 4), "]\n")
cat("    width :", round(tt$conf.int[2] - tt$conf.int[1], 4), "\n\n")

cat("  STANDARD ERROR, TWO ROUTES TO THE SAME QUANTITY\n")
se_formula <- sqrt(var(trt_mae) / n_a + var(ctl_mae) / n_b)
cat("    formula SE (Welch)   :", round(se_formula, 4), "\n")
cat("    bootstrap SE (sd of replicates) :", round(sd(boot_diffs), 4), "\n")
cat("    difference           :", round(abs(se_formula - sd(boot_diffs)), 5), "\n\n")

# ==============================================================================
# STEP 6: HOW MUCH GOT THROWN AWAY
# ==============================================================================

cat("STEP 6: DISCARD ACCOUNTING\n")
cat("  values generated inside the function :", BOOTSTRAP_B, "\n")
cat("  values returned by the function      : 2 (ci_lower, ci_upper)\n")
cat("  retention rate                       :",
    paste0(round(2 / BOOTSTRAP_B * 100, 2), "%"), "\n")
cat("  Note: estimate, n_a, n_b, and method are also returned, but they are\n")
cat("  computed outside the bootstrap and do not derive from the replicates.\n\n")

# ==============================================================================
# STEP 7: REPLICATE VECTOR SAMPLE (for visual script hardcoding)
# ==============================================================================

cat("STEP 7: HISTOGRAM BINS FOR THE VISUAL SCRIPT\n")
h <- hist(boot_diffs, breaks = 30, plot = FALSE)
cat("  bin_mids <- c(", paste(round(h$mids, 4), collapse = ", "), ")\n", sep = "")
cat("  bin_counts <- c(", paste(h$counts, collapse = ", "), ")\n", sep = "")
cat("\n")

cat("==============================================================\n")
cat("RUNNER COMPLETE\n")
cat("==============================================================\n\n")
