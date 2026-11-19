# pull_stats_post31_confidence_interval_data.R
# Statistics Series Post 31: confidence intervals (Concept 31, [NFL + WGU partial])
#
# Anchor: injury-proximity experiment, stored effects object
#   data/season2_cache/s2_week11_effects.rds  (rebuilt, control n = 1829)
#
# What this script does:
#   1. Reads the stored effects list and its player_level table.
#   2. Reproduces the stored basic bootstrap CI on the MAE gap exactly
#      (same seed, same B, same arithmetic as .bootstrap_mean_diff_ci()).
#   3. Rebuilds the Welch interval by hand: gap +/- t_crit * SE, and checks
#      it against t.test().
#   4. Prints the bootstrap spread next to the Welch SE (two routes, one scale).
#   5. Seed robustness: the basic interval under 20 other seeds.
#   6. Width at 90 / 95 / 99 percent confidence (Welch route).
#   7. Same two routes for the outcome PPG gap, for completeness.
#
# Nothing is written to disk. Every number the post uses comes from this run.

library(here)
library(dplyr)

B_BOOT     <- 1000L   # BOOTSTRAP_B_W11 in the source file
SEED_BOOT  <- 42L     # hardcoded inside .bootstrap_mean_diff_ci()
CONF_LEVEL <- 0.95

path_effects <- here::here("data", "season2_cache", "s2_week11_effects.rds")
if (!file.exists(path_effects)) stop("Missing: ", path_effects, call. = FALSE)

effects <- readRDS(path_effects)

# ------------------------------------------------------------ structure check

cat("=== STEP 0: STRUCTURE ===\n")
cat("effects names:", paste(names(effects), collapse = ", "), "\n")
need_top <- c("effect_mae", "effect_ppg", "t_test_mae", "t_test_ppg", "player_level")
miss_top <- setdiff(need_top, names(effects))
if (length(miss_top) > 0) stop("effects missing: ", paste(miss_top, collapse = ", "), call. = FALSE)

pl <- effects$player_level
cat("player_level class:", paste(class(pl), collapse = "/"), " rows:", nrow(pl), "\n")
cat("player_level names:", paste(names(pl), collapse = ", "), "\n")
need_pl <- c("group", "abs_error", "outcome_ppg")
miss_pl <- setdiff(need_pl, names(pl))
if (length(miss_pl) > 0) stop("player_level missing: ", paste(miss_pl, collapse = ", "), call. = FALSE)
cat("group values:\n"); print(table(pl$group, useNA = "ifany"))
cat("\n")

# ------------------------------------------------------------------- vectors

pull_grp <- function(col, grp) {
  x <- pl[[col]][pl$group == grp]
  x[!is.na(x)]
}
trt_mae <- pull_grp("abs_error",   "treatment")
ctl_mae <- pull_grp("abs_error",   "control")
trt_ppg <- pull_grp("outcome_ppg", "treatment")
ctl_ppg <- pull_grp("outcome_ppg", "control")

stopifnot(length(trt_mae) == effects$effect_mae$n_a,
          length(ctl_mae) == effects$effect_mae$n_b,
          identical(effects$effect_mae$method, "bootstrap"))

# ------------------------------------------------------ the two-route function

two_routes <- function(a, b, stored, label, seed = SEED_BOOT, B = B_BOOT,
                       conf = CONF_LEVEL) {
  n_a <- length(a); n_b <- length(b)
  gap <- mean(a) - mean(b)
  alpha <- 1 - conf

  cat(sprintf("=== %s ===\n", label))
  cat(sprintf("treatment n=%d mean=%.6f sd=%.6f\n", n_a, mean(a), sd(a)))
  cat(sprintf("control   n=%d mean=%.6f sd=%.6f\n", n_b, mean(b), sd(b)))
  cat(sprintf("gap (treatment - control) = %.6f\n\n", gap))

  # Route 1: basic bootstrap, verbatim arithmetic from the helper
  set.seed(seed)
  boot_diffs <- replicate(B, {
    mean(sample(a, n_a, replace = TRUE)) - mean(sample(b, n_b, replace = TRUE))
  })
  q_lo <- unname(quantile(boot_diffs, alpha / 2))
  q_hi <- unname(quantile(boot_diffs, 1 - alpha / 2))
  basic_lo <- 2 * gap - q_hi
  basic_hi <- 2 * gap - q_lo

  cat("-- Route 1: resampling (basic bootstrap) --\n")
  cat(sprintf("resampled gaps: B=%d  mean=%.6f  sd=%.6f  min=%.6f  max=%.6f\n",
              B, mean(boot_diffs), sd(boot_diffs), min(boot_diffs), max(boot_diffs)))
  cat(sprintf("2.5th pct of resampled gaps  = %.6f\n", q_lo))
  cat(sprintf("97.5th pct of resampled gaps = %.6f\n", q_hi))
  cat(sprintf("share of resampled gaps below 0 = %.4f  (count %d of %d)\n",
              mean(boot_diffs < 0), sum(boot_diffs < 0), B))
  cat(sprintf("percentile interval (middle 95%% as-is) = [%.6f, %.6f]\n", q_lo, q_hi))
  cat(sprintf("basic interval (reflected: 2*gap - pct) = [%.6f, %.6f]\n", basic_lo, basic_hi))
  cat(sprintf("basic width = %.6f\n", basic_hi - basic_lo))
  cat(sprintf("stored ci   = [%.6f, %.6f]\n", stored$ci_lower, stored$ci_upper))
  match_lo <- abs(basic_lo - stored$ci_lower) < 1e-9
  match_hi <- abs(basic_hi - stored$ci_upper) < 1e-9
  cat(sprintf("reproduces stored interval: lower %s, upper %s\n\n",
              match_lo, match_hi))

  # Route 2: Welch formula by hand, checked against t.test()
  se <- sqrt(var(a) / n_a + var(b) / n_b)
  df <- (var(a) / n_a + var(b) / n_b)^2 /
        ((var(a) / n_a)^2 / (n_a - 1) + (var(b) / n_b)^2 / (n_b - 1))
  t_crit <- qt(1 - alpha / 2, df)
  w_lo <- gap - t_crit * se
  w_hi <- gap + t_crit * se
  tt <- t.test(a, b, conf.level = conf)

  cat("-- Route 2: formula (Welch) --\n")
  cat(sprintf("SE = sqrt(%.6f/%d + %.6f/%d) = %.6f\n", var(a), n_a, var(b), n_b, se))
  cat(sprintf("df = %.4f   t_crit (%.1f%%) = %.6f\n", df, 100 * conf, t_crit))
  cat(sprintf("margin = t_crit * SE = %.6f\n", t_crit * se))
  cat(sprintf("interval = %.6f +/- %.6f = [%.6f, %.6f]\n", gap, t_crit * se, w_lo, w_hi))
  cat(sprintf("width = %.6f\n", w_hi - w_lo))
  cat(sprintf("t.test conf.int = [%.6f, %.6f]  (match: %s)\n",
              tt$conf.int[1], tt$conf.int[2],
              isTRUE(all.equal(c(w_lo, w_hi), as.numeric(tt$conf.int)))))
  cat(sprintf("t = %.6f   p = %.6g\n\n", tt$statistic, tt$p.value))

  cat("-- Routes side by side --\n")
  cat(sprintf("spread of resampled gaps (sd) = %.6f   Welch SE = %.6f   ratio = %.4f\n",
              sd(boot_diffs), se, sd(boot_diffs) / se))
  cat(sprintf("lower bound diff (basic - Welch) = %+.6f\n", basic_lo - w_lo))
  cat(sprintf("upper bound diff (basic - Welch) = %+.6f\n", basic_hi - w_hi))
  cat(sprintf("either interval contains 0? basic %s, Welch %s\n\n",
              basic_lo <= 0 & basic_hi >= 0, w_lo <= 0 & w_hi >= 0))

  invisible(list(gap = gap, se = se, df = df, boot_diffs = boot_diffs,
                 basic = c(basic_lo, basic_hi), welch = c(w_lo, w_hi)))
}

res_mae <- two_routes(trt_mae, ctl_mae, effects$effect_mae,
                      "MAE GAP (absolute projection error)")

# ------------------------------------------------------------ seed robustness

cat("=== SEED ROBUSTNESS: basic interval, MAE gap, seeds 1..20 ===\n")
seed_tab <- lapply(1:20, function(s) {
  set.seed(s)
  d <- replicate(B_BOOT, mean(sample(trt_mae, length(trt_mae), TRUE)) -
                           mean(sample(ctl_mae, length(ctl_mae), TRUE)))
  data.frame(seed = s,
             lower = 2 * res_mae$gap - unname(quantile(d, 0.975)),
             upper = 2 * res_mae$gap - unname(quantile(d, 0.025)))
})
seed_tab <- bind_rows(seed_tab)
print(seed_tab, digits = 6)
cat(sprintf("lower: min %.6f max %.6f range %.6f\n",
            min(seed_tab$lower), max(seed_tab$lower), diff(range(seed_tab$lower))))
cat(sprintf("upper: min %.6f max %.6f range %.6f\n\n",
            min(seed_tab$upper), max(seed_tab$upper), diff(range(seed_tab$upper))))

# ------------------------------------------------- width by confidence level

cat("=== WIDTH BY CONFIDENCE LEVEL (Welch route, MAE gap) ===\n")
for (cl in c(0.90, 0.95, 0.99)) {
  tc <- qt(1 - (1 - cl) / 2, res_mae$df)
  cat(sprintf("%2.0f%%: t_crit %.6f  interval [%.6f, %.6f]  width %.6f  contains 0: %s\n",
              100 * cl, tc, res_mae$gap - tc * res_mae$se, res_mae$gap + tc * res_mae$se,
              2 * tc * res_mae$se,
              (res_mae$gap - tc * res_mae$se) <= 0 & (res_mae$gap + tc * res_mae$se) >= 0))
}
cat("\n")

# ------------------------------------------------------------ PPG, same routes

stopifnot(identical(effects$effect_ppg$method, "bootstrap"))
res_ppg <- two_routes(trt_ppg, ctl_ppg, effects$effect_ppg,
                      "OUTCOME PPG GAP (for completeness)")

cat("=== DONE. Copy the full console block back; do not retype. ===\n")
