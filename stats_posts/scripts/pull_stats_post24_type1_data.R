# =============================================================================
# pull_stats_post24_type1_data.R
# Statistics Series, Post 24 (Concept 24: Type I error)
#
# PURPOSE
# Measure the false-positive rate of the estimator used in
# 18_predictive_validity_ab_testing.R when the null is true by construction.
#
# The Williams t computation below is transcribed from
# 18_predictive_validity_ab_testing.R lines 629-651. It is not an
# approximation of that test. It is the same arithmetic.
#
# PANEL-INDEPENDENT BY DESIGN. This script loads no cached panel and calls
# no project function. Nothing here is affected by the player-week count
# discrepancy carried in the handoff.
#
# Run top to bottom. Return the full console output.
# =============================================================================

library(stats)

# ----------------------------------------------------------------- CONFIG

SEED        <- 42L
N_REPS      <- 20000L      # replicates per condition
N_OBS       <- 1000L       # observations per replicate
R_PRED_OUT  <- 0.45        # true correlation of EACH predictor with outcome
R_PRED_PRED <- 0.75        # true correlation between the two predictors
ALPHA_SWEEP <- c(0.01, 0.05, 0.10)
ALPHA_MAIN  <- 0.05        # the default at lines 566 and 824

set.seed(SEED)

cat("=========================================================\n")
cat("POST 24 VERIFICATION PULL: TYPE I ERROR RATE\n")
cat("=========================================================\n\n")
cat("Seed              :", SEED, "\n")
cat("Replicates        :", format(N_REPS, big.mark = ","), "\n")
cat("Obs per replicate :", format(N_OBS, big.mark = ","), "\n")
cat("True r(pred, outcome), both predictors :", R_PRED_OUT, "\n")
cat("True r(pred1, pred2)                   :", R_PRED_PRED, "\n\n")
cat("The two predictors correlate with the outcome EQUALLY.\n")
cat("The true difference is exactly zero. Every rejection is false.\n\n")

# ------------------------------------------------------- POPULATION SETUP
# Column order: [x_r3, x_std, y] to match lines 605-607.

Sigma <- matrix(
  c(1,           R_PRED_PRED, R_PRED_OUT,
    R_PRED_PRED, 1,           R_PRED_OUT,
    R_PRED_OUT,  R_PRED_OUT,  1),
  nrow = 3, byrow = TRUE
)

stopifnot(isSymmetric(Sigma))
eig <- eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values
stopifnot(all(eig > 0))
cat("ASSERTION 1 PASS: population matrix is positive definite.\n")
cat("  eigenvalues:", paste(round(eig, 4), collapse = ", "), "\n")
cat("  true difference in predictor-outcome r:",
    R_PRED_OUT - R_PRED_OUT, "\n\n")

L <- chol(Sigma)

# ------------------------------------------------- ESTIMATOR (LINES 629-651)
# Returns the statistic and the two-sided p on the primary (Williams) branch,
# plus a flag if the degenerate fallback at 637-645 was taken.

williams_stat <- function(x_r3, x_std, y) {

  n <- length(y)

  r_roll3  <- cor(x_r3,  y,     use = "complete.obs")
  r_std    <- cor(x_std, y,     use = "complete.obs")
  r_r3_std <- cor(x_r3,  x_std, use = "complete.obs")

  # line 629-630
  det_R <- 1 - r_roll3^2 - r_std^2 - r_r3_std^2 +
           2 * r_roll3 * r_std * r_r3_std

  # line 632-634
  mean_r  <- (r_roll3 + r_std) / 2
  w_denom <- 2 * ((n - 1) / (n - 3)) * det_R +
             mean_r^2 * (1 - r_r3_std)^3

  if (is.na(w_denom) || w_denom <= 0) {
    # degenerate branch, lines 642-643
    z_r3  <- atanh(r_roll3)
    z_std <- atanh(r_std)
    stat  <- (z_r3 - z_std) / sqrt(2 / (n - 3))
    p_raw <- 2 * pnorm(-abs(stat))
    return(list(stat = stat, p = p_raw, degenerate = TRUE,
                diff = r_roll3 - r_std))
  }

  # primary branch, lines 647-649
  stat  <- (r_roll3 - r_std) *
           sqrt(((n - 1) * (1 + r_r3_std)) / w_denom)
  p_raw <- 2 * pt(-abs(stat), df = n - 3)

  list(stat = stat, p = p_raw, degenerate = FALSE,
       diff = r_roll3 - r_std)
}

# ------------------------------------------------------------- SIMULATION

cat("Running", format(N_REPS, big.mark = ","), "replicates...\n")

stat_vec <- numeric(N_REPS)
p_vec    <- numeric(N_REPS)
diff_vec <- numeric(N_REPS)
degen    <- logical(N_REPS)

for (i in seq_len(N_REPS)) {
  Z <- matrix(rnorm(N_OBS * 3), ncol = 3)
  X <- Z %*% L
  out <- williams_stat(X[, 1], X[, 2], X[, 3])
  stat_vec[i] <- out$stat
  p_vec[i]    <- out$p
  diff_vec[i] <- out$diff
  degen[i]    <- out$degenerate
}

cat("Done.\n\n")

cat("ASSERTION 2:", sum(degen), "replicate(s) took the degenerate fallback.\n")
cat("  Expected 0. All results below are the primary Williams t path.\n\n")

# --------------------------------------------- SECTION 1: RATE AT ALPHA 0.05

df_used   <- N_OBS - 3
crit_main <- qt(1 - ALPHA_MAIN / 2, df = df_used)   # line 651
rejects   <- abs(stat_vec) >= crit_main             # line 707 comparison
n_rej     <- sum(rejects)
rate      <- n_rej / N_REPS
se_rate   <- sqrt(ALPHA_MAIN * (1 - ALPHA_MAIN) / N_REPS)

cat("---------------------------------------------------------\n")
cat("SECTION 1: FALSE POSITIVE RATE AT THE DEFAULT ALPHA\n")
cat("---------------------------------------------------------\n")
cat("alpha (default, lines 566 and 824) :", ALPHA_MAIN, "\n")
cat("df (n - 3)                         :", df_used, "\n")
cat("critical value, qt(1 - alpha/2, df):", round(crit_main, 4), "\n\n")
cat("Rejections :", format(n_rej, big.mark = ","), "of",
    format(N_REPS, big.mark = ","), "\n")
cat("Rate       :", round(rate, 5), "\n")
cat("Expected   :", ALPHA_MAIN, "( +/- 1.96 SE =",
    round(ALPHA_MAIN - 1.96 * se_rate, 5), "to",
    round(ALPHA_MAIN + 1.96 * se_rate, 5), ")\n\n")

in_band <- abs(rate - ALPHA_MAIN) <= 1.96 * se_rate
cat("ASSERTION 3", if (in_band) "PASS" else "FAIL",
    ": observed rate within 1.96 SE of alpha.\n\n")

cat("Every one of those", format(n_rej, big.mark = ","),
    "rejections is a false positive.\n")
cat("The two predictors were built equally predictive.\n\n")

# ------------------------------------------- SECTION 2: THE TWO-TAILED SPLIT

up   <- sum(rejects & diff_vec > 0)
down <- sum(rejects & diff_vec < 0)

cat("---------------------------------------------------------\n")
cat("SECTION 2: WHERE THE alpha/2 IN THE CRITICAL VALUE GOES\n")
cat("---------------------------------------------------------\n")
cat("False positives calling roll3 better :", format(up, big.mark = ","),
    "( rate", round(up / N_REPS, 5), ")\n")
cat("False positives calling std better   :", format(down, big.mark = ","),
    "( rate", round(down / N_REPS, 5), ")\n")
cat("Each tail expected                   :", ALPHA_MAIN / 2, "\n\n")
cat("ASSERTION 4:", up, "+", down, "=", up + down,
    "should equal total rejections", n_rej, "\n")
stopifnot(up + down == n_rej)
cat("  PASS\n\n")

# ----------------------------------------------- SECTION 3: THE ALPHA SWEEP

cat("---------------------------------------------------------\n")
cat("SECTION 3: THE RATE TRACKS WHATEVER alpha IS SET TO\n")
cat("---------------------------------------------------------\n")
cat("Same 20,000 replicates. Only the threshold changes.\n\n")
cat(sprintf("%-8s %-14s %-12s %-12s\n",
            "alpha", "crit value", "rejections", "rate"))

for (a in ALPHA_SWEEP) {
  cv <- qt(1 - a / 2, df = df_used)
  nr <- sum(abs(stat_vec) >= cv)
  cat(sprintf("%-8s %-14s %-12s %-12s\n",
              a, round(cv, 4), format(nr, big.mark = ","),
              round(nr / N_REPS, 5)))
}
cat("\nThe false positive rate is not discovered. It is chosen.\n\n")

# ------------------------------------------- SECTION 4: p-VALUE UNIFORMITY

cat("---------------------------------------------------------\n")
cat("SECTION 4: p-VALUE DISTRIBUTION UNDER THE TRUE NULL\n")
cat("---------------------------------------------------------\n")
cat("Deciles of the", format(N_REPS, big.mark = ","), "p-values:\n")
print(round(quantile(p_vec, probs = seq(0, 1, 0.1)), 4))
cat("\nUnder a true null a correct test yields uniform p-values.\n")
cat("Fraction below 0.05 :", round(mean(p_vec < 0.05), 5), "\n")
cat("Fraction below 0.10 :", round(mean(p_vec < 0.10), 5), "\n")
cat("Fraction below 0.50 :", round(mean(p_vec < 0.50), 5), "\n\n")

ks <- suppressWarnings(ks.test(p_vec, "punif"))
cat("ASSERTION 5: KS test against uniform, D =",
    round(unname(ks$statistic), 5), ", p =", signif(ks$p.value, 4), "\n")
cat("  A large KS p-value supports uniformity. Reported, not asserted.\n\n")

# ------------------------------------------------------------------ SUMMARY

cat("=========================================================\n")
cat("SUMMARY FOR THE POST\n")
cat("=========================================================\n")
cat("Replicates                :", format(N_REPS, big.mark = ","), "\n")
cat("Obs per replicate         :", format(N_OBS, big.mark = ","), "\n")
cat("True difference           : 0 by construction\n")
cat("alpha                     :", ALPHA_MAIN, "\n")
cat("Critical value            :", round(crit_main, 4), "\n")
cat("False positives           :", format(n_rej, big.mark = ","), "\n")
cat("False positive rate       :", round(rate, 5), "\n")
cat("Tail split (roll3 / std)  :", up, "/", down, "\n")
cat("=========================================================\n")
cat("Session info:\n")
print(R.version.string)
