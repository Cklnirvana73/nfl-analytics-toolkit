# ==============================================================================
# pull_stats_post23_alpha_data.R
# Statistics Series, Post 23: Statistical Significance
#
# PURPOSE
# Verify every claim the post intends to make about the significance threshold
# in the predictive validity A/B study. Nothing here re-derives a result from
# a cached RDS: the panel and the comparison are re-run from source so the
# numbers in the post trace to this run.
#
# WHAT THIS ANSWERS
#   Q1  What number does alpha = 0.05 actually become, and how?
#   Q2  Does the t critical value differ from 1.96 at the observed df?
#   Q3  Do the two significance paths in the file agree on this data?
#   Q4  How far would alpha have to move before any decision changes?
#   Q5  What does the same alpha buy at a small n vs the observed n?
#
# RUN FROM PROJECT ROOT. Returns console output only. Writes nothing.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
  library(glue)
  library(here)
})

source(here("R", "18_predictive_validity_ab_testing.R"))

SEED        <- 42L
N_BOOTSTRAP <- 2000L
ALPHA       <- 0.05
TARGET_PWR  <- 0.80
MDE_Q       <- 0.10

cat("\n", strrep("=", 78), "\n", sep = "")
cat("POST 23 VERIFICATION PULL :: STATISTICAL SIGNIFICANCE\n")
cat(strrep("=", 78), "\n", sep = "")

# ------------------------------------------------------------------ Q1
# The translation from a chosen error rate to a boundary on the test scale.
# This is the single line the post is built on: qnorm(1 - alpha / 2).
cat("\n[Q1] ALPHA TO CRITICAL VALUE, ACROSS A RANGE OF ALPHAS\n")
cat(strrep("-", 78), "\n", sep = "")

alpha_grid <- c(0.20, 0.10, 0.05, 0.01, 0.005, 0.001)

q1 <- tibble(
  alpha      = alpha_grid,
  crit_z     = qnorm(1 - alpha_grid / 2),
  one_in_n   = round(1 / alpha_grid)
)

for (i in seq_len(nrow(q1))) {
  cat(glue(
    "  alpha = {format(q1$alpha[i], nsmall = 3)}  ->  ",
    "critical value = {format(round(q1$crit_z[i], 6), nsmall = 6)}  ",
    "(1 false positive in {q1$one_in_n[i]} true nulls)"
  ), "\n")
}

cat("\n  Full precision at alpha = 0.05: ", format(qnorm(0.975), digits = 15), "\n", sep = "")

# ------------------------------------------------------------------ Q2
# The file uses qt() on the Williams path and qnorm() only on the degenerate
# fallback. Confirm whether the distinction is visible at the observed df.
cat("\n[Q2] BUILDING PANEL, THEN COMPARING t AND z CRITICAL VALUES\n")
cat(strrep("-", 78), "\n", sep = "")

panel <- build_ab_panel(seasons = SEASONS_S2)

n_by_pos <- panel %>%
  count(position, name = "n_player_weeks") %>%
  arrange(desc(n_player_weeks))

q2 <- n_by_pos %>%
  mutate(
    df          = n_player_weeks - 3L,
    crit_t      = qt(1 - ALPHA / 2, df = df),
    crit_z      = qnorm(1 - ALPHA / 2),
    diff_abs    = crit_t - crit_z,
    same_at_2dp = round(crit_t, 2) == round(crit_z, 2)
  )

for (i in seq_len(nrow(q2))) {
  cat(glue(
    "  {q2$position[i]}: n = {format(q2$n_player_weeks[i], big.mark = ',')}, ",
    "df = {format(q2$df[i], big.mark = ',')}, ",
    "t crit = {round(q2$crit_t[i], 6)}, z crit = {round(q2$crit_z[i], 6)}, ",
    "diff = {format(q2$diff_abs[i], scientific = TRUE, digits = 3)}, ",
    "equal at 2dp = {q2$same_at_2dp[i]}"
  ), "\n")
}

# Small-n contrast: where the t/z distinction stops being cosmetic.
cat("\n  Same alpha, small samples (t critical value by df):\n")
for (nn in c(5L, 10L, 30L, 100L, 1000L)) {
  cat(glue(
    "    n = {format(nn, width = 5)}  df = {format(nn - 3L, width = 5)}  ",
    "t crit = {format(round(qt(1 - ALPHA / 2, df = nn - 3L), 4), nsmall = 4)}"
  ), "\n")
}

# ------------------------------------------------------------------ Q3
# Two independent significance paths exist in the file. Path A is the
# critical-value comparison that sets better_predictor. Path B is the
# FDR-adjusted p-value compared to a hardcoded 0.05. Confirm they agree here.
cat("\n[Q3] RE-RUNNING THE COMPARISON FROM SOURCE, THEN CHECKING BOTH PATHS\n")
cat(strrep("-", 78), "\n", sep = "")

results_raw <- compare_prediction_methods(
  panel       = panel,
  n_bootstrap = N_BOOTSTRAP,
  alpha       = ALPHA,
  seed        = SEED
)

results <- apply_fdr_correction(results_raw)

q3 <- results %>%
  mutate(
    df            = n_player_weeks - 3L,
    crit_t        = qt(1 - ALPHA / 2, df = df),
    # Path A: how the file decides better_predictor (line 707)
    path_a_signif = abs(z_stat) >= crit_t,
    # Path B: how the file reports significance (line 943)
    path_b_signif = significant_adj,
    paths_agree   = path_a_signif == path_b_signif
  ) %>%
  select(position, n_player_weeks, r_roll3, r_std, z_stat, crit_t,
         p_value_raw, p_value_adj, cohens_q, better_predictor,
         path_a_signif, path_b_signif, paths_agree)

print(as.data.frame(q3), row.names = FALSE)

cat(glue(
  "\n  Paths agree on all positions: {all(q3$paths_agree)}\n",
  "  Positions significant (path A, uncorrected crit value): {sum(q3$path_a_signif)}/{nrow(q3)}\n",
  "  Positions significant (path B, FDR-adjusted p):         {sum(q3$path_b_signif)}/{nrow(q3)}\n"
))

# ------------------------------------------------------------------ Q4
# How much would the threshold have to move to change any conclusion?
# Reported as the alpha at which each position's adjusted p-value would
# stop clearing the bar. The adjusted p-value IS that alpha by definition.
cat("\n[Q4] HOW FAR THE THRESHOLD WOULD HAVE TO MOVE TO FLIP A DECISION\n")
cat(strrep("-", 78), "\n", sep = "")

q4 <- q3 %>%
  mutate(
    flip_alpha   = p_value_adj,
    orders_below = log10(ALPHA) - log10(p_value_adj)
  ) %>%
  arrange(desc(p_value_adj))

for (i in seq_len(nrow(q4))) {
  cat(glue(
    "  {q4$position[i]}: adjusted p = {format(q4$flip_alpha[i], scientific = TRUE, digits = 4)}  ",
    "-> stays significant until alpha drops below that value, ",
    "{round(q4$orders_below[i], 1)} orders of magnitude under 0.05"
  ), "\n")
}

cat("\n  Decisions under each candidate threshold:\n")
for (a in alpha_grid) {
  n_sig <- sum(q4$p_value_adj < a)
  cat(glue(
    "    alpha = {format(a, nsmall = 3)}  ->  {n_sig}/{nrow(q4)} positions significant"
  ), "\n")
}

# ------------------------------------------------------------------ Q5
# Same threshold, different sample size. The power analysis is where the
# threshold's real cost shows up: it sets how much data is needed.
cat("\n[Q5] WHAT THE SAME THRESHOLD DEMANDS IN SAMPLE SIZE\n")
cat(strrep("-", 78), "\n", sep = "")

z_beta <- qnorm(TARGET_PWR)

q5 <- tibble(alpha = alpha_grid) %>%
  mutate(
    z_alpha    = qnorm(1 - alpha / 2),
    required_n = ceiling(((z_alpha + z_beta) / MDE_Q)^2 + 3)
  )

for (i in seq_len(nrow(q5))) {
  cat(glue(
    "  alpha = {format(q5$alpha[i], nsmall = 3)}  ->  z_alpha = {round(q5$z_alpha[i], 4)}, ",
    "required n = {format(q5$required_n[i], big.mark = ',')} player-weeks per position ",
    "(at power {TARGET_PWR}, MDE q = {MDE_Q})"
  ), "\n")
}

power_obs <- power_analysis_correlation_diff(
  panel = panel, mde_q = MDE_Q, alpha = ALPHA, target_power = TARGET_PWR
)

cat("\n  Observed n against the alpha = 0.05 requirement:\n")
req_n <- power_obs$required_n$required_n_per_group[1]
obs   <- power_obs$achieved_power %>%
  mutate(multiple_of_required = round(n_player_weeks / req_n, 1))

for (i in seq_len(nrow(obs))) {
  cat(glue(
    "    {obs$position[i]}: n = {format(obs$n_player_weeks[i], big.mark = ',')}, ",
    "required = {format(req_n, big.mark = ',')}, ",
    "{obs$multiple_of_required[i]}x the requirement, ",
    "achieved power = {obs$achieved_power[i]}"
  ), "\n")
}

# ------------------------------------------------- pre-delivery assertions
cat("\n", strrep("=", 78), "\n", sep = "")
cat("ASSERTIONS\n")
cat(strrep("-", 78), "\n", sep = "")

assert <- function(label, condition) {
  cat(glue("  [{if (isTRUE(condition)) 'PASS' else 'FAIL'}] {label}"), "\n")
  if (!isTRUE(condition)) warning(glue("ASSERTION FAILED: {label}"), call. = FALSE)
}

assert("qnorm(0.975) rounds to 1.96 at 2dp",
       round(qnorm(0.975), 2) == 1.96)
assert("t critical value equals z critical value at 2dp for every position",
       all(q2$same_at_2dp))
assert("t critical value is strictly greater than z at every observed df",
       all(q2$diff_abs > 0))
assert("both significance paths agree on every position",
       all(q3$paths_agree))
assert("three positions present",
       nrow(q3) == 3L)
assert("every adjusted p-value is below 0.05",
       all(q3$p_value_adj < 0.05))
assert("required n rises monotonically as alpha falls",
       all(diff(rev(q5$required_n)) > 0))
assert("observed n exceeds the required n for every position",
       all(obs$n_player_weeks > req_n))

cat(strrep("=", 78), "\n", sep = "")
cat("END OF PULL. Return this console output in full.\n")
cat(strrep("=", 78), "\n\n", sep = "")
