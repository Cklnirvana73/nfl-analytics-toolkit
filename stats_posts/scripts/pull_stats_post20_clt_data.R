# =============================================================================
# Statistics Series Post 20: The Central Limit Theorem
# Standalone data pull script
#
# PURPOSE
#   Build the 2025 third-down conversion population as a Bernoulli (0/1)
#   variable, then build empirical sampling distributions of the sample mean
#   at increasing sample sizes. Every number printed here is intended to be
#   hardcoded into the post prose and the visual script.
#
#   Nothing in this script is simulated from assumed parameters. The 0/1
#   population is real play outcomes; the sampling distributions are built by
#   resampling that real population.
#
# RUN CONTEXT
#   Self-contained. Sources no project file and calls no project function.
#   Requires only nflreadr (or nflfastR) plus base R.
#
# RECONCILIATION HOOK
#   Post 9 established 6,801 third-down pass and run plays in 2025 at a
#   40.2 percent conversion rate. This script prints its own population count
#   and rate and compares them to those figures. A mismatch is reported
#   loudly and is NOT silently accepted.
# =============================================================================

library(nflreadr)
library(dplyr)

set.seed(42L)

# ---- CONFIGURATION ----------------------------------------------------------

SEASON              <- 2025L
SAMPLE_SIZES        <- c(5L, 10L, 30L, 100L)
N_DRAWS             <- 2000L    # sampling-distribution replicates per n
POST9_N_PLAYS       <- 6801L    # reconciliation target from Post 9
POST9_RATE          <- 0.402    # reconciliation target from Post 9
RECONCILE_TOL_RATE  <- 0.002    # tolerance on the rate comparison

cat("\n")
cat("=============================================================\n")
cat("POST 20: CENTRAL LIMIT THEOREM -- DATA PULL\n")
cat("Season:", SEASON, "| Draws per sample size:", N_DRAWS, "\n")
cat("=============================================================\n\n")

# ---- STEP 1: LOAD PLAY BY PLAY ----------------------------------------------

cat("STEP 1: Loading play-by-play\n")

pbp <- nflreadr::load_pbp(seasons = SEASON)

stopifnot(
  "load_pbp returned zero rows" = nrow(pbp) > 0
)

cat("  Raw plays loaded:", format(nrow(pbp), big.mark = ","), "\n")

# Verify every column this script depends on actually exists before using it.
required_cols <- c("season_type", "down", "play_type", "first_down", "ydstogo")
missing_cols  <- setdiff(required_cols, names(pbp))

if (length(missing_cols) > 0) {
  stop(
    "Required columns missing from play-by-play:\n  ",
    paste(missing_cols, collapse = ", "),
    "\nInspect names(pbp) before proceeding."
  )
}

cat("  All required columns present\n\n")

# ---- STEP 2: BUILD THE BERNOULLI POPULATION ---------------------------------

cat("STEP 2: Building third-down conversion population\n")

third_downs <- pbp %>%
  filter(
    season_type == "REG",
    down == 3,
    play_type %in% c("pass", "run"),
    !is.na(first_down)
  ) %>%
  mutate(converted = as.integer(first_down == 1)) %>%
  select(converted, ydstogo)

n_pop <- nrow(third_downs)

stopifnot(
  "Third-down population is empty" = n_pop > 0,
  "converted contains values other than 0 and 1" =
    all(third_downs$converted %in% c(0L, 1L))
)

pop_values <- third_downs$converted

pop_mean <- mean(pop_values)
pop_sd   <- sd(pop_values)
n_zero   <- sum(pop_values == 0L)
n_one    <- sum(pop_values == 1L)

cat("  Population size (plays):", format(n_pop, big.mark = ","), "\n")
cat("  Distinct values present:", paste(sort(unique(pop_values)), collapse = ", "), "\n")
cat("  Count of 0 (no conversion):", format(n_zero, big.mark = ","), "\n")
cat("  Count of 1 (conversion):   ", format(n_one, big.mark = ","), "\n")
cat("  Population mean (conversion rate):", round(pop_mean, 4), "\n")
cat("  Population SD:", round(pop_sd, 4), "\n")

# Bernoulli SD identity check: sd should equal sqrt(p * (1 - p)) up to the
# n-1 denominator. Reported so the post can state the SD honestly.
bernoulli_sd_theoretical <- sqrt(pop_mean * (1 - pop_mean))
cat("  sqrt(p * (1-p)) for comparison:", round(bernoulli_sd_theoretical, 4), "\n\n")

# ---- STEP 3: RECONCILE AGAINST POST 9 ---------------------------------------

cat("STEP 3: Reconciliation against Post 9 figures\n")

rate_diff  <- abs(pop_mean - POST9_RATE)
count_diff <- n_pop - POST9_N_PLAYS

cat("  Post 9 play count:", format(POST9_N_PLAYS, big.mark = ","),
    "| This run:", format(n_pop, big.mark = ","),
    "| Difference:", count_diff, "\n")
cat("  Post 9 rate:", POST9_RATE,
    "| This run:", round(pop_mean, 4),
    "| Difference:", round(rate_diff, 4), "\n")

if (count_diff != 0L || rate_diff > RECONCILE_TOL_RATE) {
  cat("\n")
  cat("  *** RECONCILIATION MISMATCH ***\n")
  cat("  This run does not match the Post 9 population.\n")
  cat("  Do NOT draft prose until this is explained. Likely causes:\n")
  cat("    - Post 9 used a different conversion definition than first_down == 1\n")
  cat("      (e.g. series_success, or excluding penalty-only conversions)\n")
  cat("    - Post 9 included or excluded postseason differently\n")
  cat("    - nflverse data has been revised since Post 9 was written\n")
  cat("  Report this block verbatim rather than adjusting the script to match.\n\n")
} else {
  cat("  Reconciliation PASSED. Population matches Post 9.\n\n")
}

# ---- STEP 4: BUILD SAMPLING DISTRIBUTIONS -----------------------------------

cat("STEP 4: Building sampling distributions of the mean\n")
cat("  Method: draw n plays at random from the population without\n")
cat("  replacement, take the mean, repeat", N_DRAWS, "times.\n\n")

results <- list()

for (n in SAMPLE_SIZES) {

  if (n > n_pop) {
    cat("  SKIPPING n =", n, "-- larger than population\n")
    next
  }

  draws <- replicate(
    N_DRAWS,
    mean(sample(pop_values, size = n, replace = FALSE))
  )

  obs_se       <- sd(draws)
  predicted_se <- pop_sd / sqrt(n)

  # Guard: predicted_se cannot be zero here because pop_sd > 0 and n > 0,
  # but assert rather than assume.
  stopifnot("predicted SE is zero or non-finite" = is.finite(predicted_se) && predicted_se > 0)

  se_ratio <- obs_se / predicted_se

  # Normality of the sampling distribution. Shapiro-Wilk caps at 5000.
  shapiro_input <- if (length(draws) > 5000L) draws[1:5000L] else draws
  shapiro_p     <- shapiro.test(shapiro_input)$p.value

  # Skewness of the sampling distribution (moment coefficient).
  draw_skew <- mean((draws - mean(draws))^3) / (sd(draws)^3)

  # How many distinct values the sample mean can even take at this n.
  n_distinct_possible <- n + 1L

  results[[as.character(n)]] <- list(
    n                   = n,
    mean_of_means       = mean(draws),
    observed_se         = obs_se,
    predicted_se        = predicted_se,
    se_ratio            = se_ratio,
    shapiro_p           = shapiro_p,
    skewness            = draw_skew,
    min_draw            = min(draws),
    max_draw            = max(draws),
    n_distinct_possible = n_distinct_possible,
    n_distinct_observed = length(unique(draws))
  )

  cat("  n =", formatC(n, width = 3), "\n")
  cat("    Mean of the", N_DRAWS, "sample means:", round(mean(draws), 4), "\n")
  cat("    Observed SE (SD of sample means):  ", round(obs_se, 4), "\n")
  cat("    Predicted SE (pop SD / sqrt(n)):   ", round(predicted_se, 4), "\n")
  cat("    Observed / predicted ratio:        ", round(se_ratio, 4), "\n")
  cat("    Skewness of sampling distribution: ", round(draw_skew, 4), "\n")
  cat("    Shapiro-Wilk p:                    ", format.pval(shapiro_p, digits = 4), "\n")
  cat("    Range of sample means:             ",
      round(min(draws), 4), "to", round(max(draws), 4), "\n")
  cat("    Distinct means possible at this n: ", n_distinct_possible, "\n")
  cat("    Distinct means actually observed:  ", length(unique(draws)), "\n\n")
}

# ---- STEP 5: CONVERGENCE SPEED CONTEXT --------------------------------------

cat("STEP 5: Convergence speed context\n")
cat("  The population rate drives how fast the sampling distribution\n")
cat("  becomes symmetric. A rate near 0.50 is already symmetric; a rate\n")
cat("  near 0 or 1 is badly skewed and needs a much larger n.\n\n")

# Population skewness of the Bernoulli itself, for contrast with the
# sampling distributions above.
pop_skew <- (1 - 2 * pop_mean) / sqrt(pop_mean * (1 - pop_mean))

cat("  Population (single-play) skewness:", round(pop_skew, 4), "\n")
cat("  For contrast, skewness of a Bernoulli at other rates:\n")

for (p in c(0.05, 0.20, 0.402, 0.50)) {
  sk <- (1 - 2 * p) / sqrt(p * (1 - p))
  cat("    rate", formatC(p, format = "f", digits = 3),
      "-> skewness", formatC(round(sk, 4), format = "f", digits = 4), "\n")
}
cat("\n")

# Rule-of-thumb check often quoted for the normal approximation:
# n * p and n * (1 - p) both at least 10.
cat("  Normal-approximation rule of thumb (n*p and n*(1-p) both >= 10):\n")
for (n in SAMPLE_SIZES) {
  np  <- n * pop_mean
  nq  <- n * (1 - pop_mean)
  ok  <- if (np >= 10 && nq >= 10) "MEETS" else "does not meet"
  cat("    n =", formatC(n, width = 3),
      "| n*p =", formatC(round(np, 2), format = "f", digits = 2),
      "| n*(1-p) =", formatC(round(nq, 2), format = "f", digits = 2),
      "|", ok, "\n")
}
cat("\n")

# ---- STEP 6: SUMMARY TABLE FOR HARDCODING -----------------------------------

cat("=============================================================\n")
cat("SUMMARY TABLE (hardcode these into prose and visual script)\n")
cat("=============================================================\n\n")

summary_df <- do.call(rbind, lapply(results, function(r) {
  data.frame(
    n              = r$n,
    mean_of_means  = round(r$mean_of_means, 4),
    observed_se    = round(r$observed_se, 4),
    predicted_se   = round(r$predicted_se, 4),
    se_ratio       = round(r$se_ratio, 4),
    skewness       = round(r$skewness, 4),
    shapiro_p      = signif(r$shapiro_p, 4),
    stringsAsFactors = FALSE
  )
}))

print(summary_df, row.names = FALSE)

cat("\n")
cat("POPULATION CONSTANTS\n")
cat("  n_plays:      ", n_pop, "\n")
cat("  n_converted:  ", n_one, "\n")
cat("  n_failed:     ", n_zero, "\n")
cat("  pop_mean:     ", round(pop_mean, 4), "\n")
cat("  pop_sd:       ", round(pop_sd, 4), "\n")
cat("  pop_skewness: ", round(pop_skew, 4), "\n")
cat("  n_draws:      ", N_DRAWS, "\n")
cat("  seed:         ", 42L, "\n\n")

cat("=============================================================\n")
cat("PULL COMPLETE\n")
cat("Paste this entire console output back before any drafting.\n")
cat("=============================================================\n\n")
