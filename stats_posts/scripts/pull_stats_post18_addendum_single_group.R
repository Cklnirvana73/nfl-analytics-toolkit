# ==============================================================================
# Statistics Series -- Post 18 ADDENDUM
# Like-for-like sampling distribution (Option A)
#
# Why this exists: the first runner bootstrapped a DIFFERENCE between two group
# means. Comparing the spread of that difference against the spread of
# individual absolute errors is not like-for-like, and on a shared axis it puts
# a negative-valued bar next to two bars that cannot go below zero.
#
# This addendum bootstraps the mean of ONE group (returning players) and
# retains the replicate vector. Individual misses and the resampled mean are
# then the same quantity in the same unit, both positive, both centered near
# the same value. That is the comparison the post and Panel 1 should make.
#
# Run AFTER pull_stats_post18_sampling_distributions_data.R, in the same
# session, so trt_mae and ctl_mae are already in the environment. If starting
# fresh, rerun that script first.
# ==============================================================================

if (!exists("trt_mae") || !exists("ctl_mae")) {
  stop(paste0(
    "trt_mae / ctl_mae not found. Run ",
    "pull_stats_post18_sampling_distributions_data.R first in this session."
  ), call. = FALSE)
}

BOOTSTRAP_B <- 1000L
BOOT_SEED   <- 42L

n_a <- length(trt_mae)

skew_fn <- function(x) {
  m <- mean(x); s <- sd(x); n <- length(x)
  sum((x - m)^3) / (n * s^3)
}

cat("\n")
cat("==============================================================\n")
cat("ADDENDUM: SINGLE-GROUP SAMPLING DISTRIBUTION (RETURNING GROUP)\n")
cat("==============================================================\n\n")

# --- The data: individual absolute errors, returning group --------------------

cat("THE DATA (individual misses, returning group)\n")
cat("  n        :", n_a, "\n")
cat("  mean     :", round(mean(trt_mae), 4), "\n")
cat("  median   :", round(median(trt_mae), 4), "\n")
cat("  sd       :", round(sd(trt_mae), 4), "\n")
cat("  min      :", round(min(trt_mae), 4), "\n")
cat("  max      :", round(max(trt_mae), 4), "\n")
cat("  skewness :", round(skew_fn(trt_mae), 4), "\n\n")

# --- The statistic: resampled MEAN of that same group -------------------------

set.seed(BOOT_SEED)
boot_means <- replicate(BOOTSTRAP_B, mean(sample(trt_mae, n_a, replace = TRUE)))

cat("THE STATISTIC (1,000 resampled means of the same group)\n")
cat("  mean     :", round(mean(boot_means), 4), "\n")
cat("  median   :", round(median(boot_means), 4), "\n")
cat("  sd (bootstrap standard error) :", round(sd(boot_means), 4), "\n")
cat("  min      :", round(min(boot_means), 4), "\n")
cat("  max      :", round(max(boot_means), 4), "\n")
cat("  skewness :", round(skew_fn(boot_means), 4), "\n\n")

cat("LIKE-FOR-LIKE CONTRAST (same quantity, same unit, both positive)\n")
cat("  sd of individual misses :", round(sd(trt_mae), 4), "\n")
cat("  sd of the resampled mean:", round(sd(boot_means), 4), "\n")
cat("  ratio                   :",
    round(sd(trt_mae) / sd(boot_means), 2), "\n")
cat("  range of individual misses :",
    round(max(trt_mae) - min(trt_mae), 3), "\n")
cat("  range of resampled means   :",
    round(max(boot_means) - min(boot_means), 3), "\n\n")

cat("  Analytic check: sd / sqrt(n) =",
    round(sd(trt_mae) / sqrt(n_a), 4),
    " vs bootstrap SE", round(sd(boot_means), 4), "\n\n")

# --- Bins for the visual script ----------------------------------------------

cat("HISTOGRAM BINS FOR PANEL 1\n")
h <- hist(boot_means, breaks = 30, plot = FALSE)
cat("  bin_mids <- c(", paste(round(h$mids, 4), collapse = ", "), ")\n", sep = "")
cat("  bin_counts <- c(", paste(h$counts, collapse = ", "), ")\n", sep = "")
cat("\n")

cat("==============================================================\n")
cat("ADDENDUM COMPLETE\n")
cat("==============================================================\n\n")
