# =============================================================================
# pull_stats_post37_nonparametric_data.R
# Statistics Series, Post 37: Non-parametric alternatives
#
# Anchor: the paired Wilcoxon signed-rank tests in R/13 compare_models()
# (run_wilcoxon(), lines 773-794). Each pair is one held-out test player-week,
# model A absolute error vs model B absolute error.
#
# What this script does (read-only; writes nothing):
#   S1  Load the saved Week 11 results and inspect structure. Stops if the
#       per-row errors were not saved, so nothing is reconstructed by guess.
#   S2  Describe the test rows: seasons x weeks, rows per player.
#   S3  Reproduction gate: rerun wilcox.test() exactly as R/13 does and match
#       the stored p-values.
#   S4  Shape of the paired gaps: mean, median, skewness, zero gaps,
#       Shapiro-Wilk. Also skewness of each model's raw absolute errors
#       (checks the R/13 header claim that errors are right-skewed).
#   S5  Same pairs, two tests: paired t-test vs Wilcoxon signed-rank,
#       with the Hodges-Lehmann pseudo-median and the mean gap.
#   S6  Sensitivity: drop the 5 largest absolute gaps and rerun both tests.
#   S7  Independence fix: one value per player (mean gap), both tests again.
#   S8  Robustness: one random test week per player, seed 20261014,
#       200 reruns, both tests.
#
# Paste the full console output back verbatim.
# Assumed versions: R >= 4.3, dplyr >= 1.1, tidyr >= 1.3.
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

options(width = 120, pillar.width = 120, digits = 7)

TOOLKIT_ROOT <- here::here()
stopifnot(dir.exists(TOOLKIT_ROOT))
setwd(TOOLKIT_ROOT)
RESULTS_PATH <- file.path(TOOLKIT_ROOT, "output", "week11_full_results_2025.rds")
POSITIONS    <- c("passer", "rusher", "receiver")
MODEL_PAIRS  <- list(
  c("xgboost",       "random_forest"),
  c("xgboost",       "elastic_net"),
  c("random_forest", "elastic_net")
)
DROP_TOP_K   <- 5L
SEED_DRAW    <- 20261014L
N_RERUNS     <- 200L
GATE_TOL     <- 1e-10

# Moment skewness g1 = m3 / m2^(3/2), population moments. Stated so the post
# can name the formula. Returns NA when spread is zero or n < 3.
skew_g1 <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3L) return(NA_real_)
  m  <- mean(x)
  m2 <- mean((x - m)^2)
  if (m2 <= 0) return(NA_real_)
  mean((x - m)^3) / m2^1.5
}

# Both tests on one vector of paired gaps d = err_a - err_b.
# wilcox.test on d alone with mu = 0 is identical to the paired call on
# (err_a, err_b); S3 confirms that against the stored values.
two_tests <- function(d) {
  d  <- d[!is.na(d)]
  tt <- stats::t.test(d, mu = 0)
  wt <- suppressWarnings(stats::wilcox.test(d, mu = 0, exact = FALSE,
                                            correct = TRUE, conf.int = TRUE))
  tibble(
    n          = length(d),
    n_zero     = sum(d == 0),
    mean_gap   = mean(d),
    median_gap = stats::median(d),
    hl_est     = unname(wt$estimate),
    t_stat     = unname(tt$statistic),
    t_df       = unname(tt$parameter),
    t_p        = tt$p.value,
    t_ci_lo    = tt$conf.int[1],
    t_ci_hi    = tt$conf.int[2],
    w_V        = unname(wt$statistic),
    w_p        = wt$p.value,
    w_ci_lo    = wt$conf.int[1],
    w_ci_hi    = wt$conf.int[2]
  )
}

cat("\n================ S1: LOAD AND INSPECT ================\n")
stopifnot(file.exists(RESULTS_PATH))
cat("File:", RESULTS_PATH, " size:", file.size(RESULTS_PATH), "bytes\n")
results <- readRDS(RESULTS_PATH)
cat("Top-level names(results):\n"); print(names(results))

if (!"comparisons" %in% names(results)) {
  stop("No 'comparisons' element in results. Paste the names above back; ",
       "the extraction path needs to be confirmed before going further.")
}
comps <- results$comparisons
cat("names(results$comparisons):\n"); print(names(comps))
stopifnot(all(POSITIONS %in% names(comps)))

for (pos in POSITIONS) {
  cat("\n-- names(comparisons$", pos, "):\n", sep = "")
  print(names(comps[[pos]]))
}

has_errors <- vapply(POSITIONS,
                     function(p) "weekly_errors" %in% names(comps[[p]]),
                     logical(1))
cat("\nweekly_errors present per position:\n"); print(has_errors)
if (!all(has_errors)) {
  stop("weekly_errors is not saved in compare_models() output for every ",
       "position. Paste everything above back. Do not rebuild predictions ",
       "by hand; the next script will be written against the real structure.")
}

errs_long <- map_dfr(POSITIONS, function(p) {
  comps[[p]]$weekly_errors %>% mutate(position = p)
})
cat("\nweekly_errors columns:\n"); print(names(errs_long))
need_cols <- c("player_id", "season", "week", "actual", "model", "abs_error")
missing_cols <- setdiff(need_cols, names(errs_long))
if (length(missing_cols) > 0) {
  stop("weekly_errors is missing columns: ", paste(missing_cols, collapse = ", "))
}
stopifnot(is.numeric(errs_long$abs_error))
cat("Model labels found:\n"); print(sort(unique(errs_long$model)))

cat("\nStored wilcoxon_tests per position:\n")
stored <- map_dfr(POSITIONS, function(p) {
  comps[[p]]$wilcoxon_tests %>%
    mutate(position = p, comparison = as.character(comparison))
})
print(stored, n = Inf, width = Inf)

errs_wide <- errs_long %>%
  select(position, player_id, season, week, actual, model, abs_error) %>%
  pivot_wider(names_from = model, values_from = abs_error)
dup_check <- errs_wide %>% count(position, player_id, season, week) %>% filter(n > 1)
cat("\nDuplicate position-player-season-week rows after pivot:", nrow(dup_check), "\n")
stopifnot(nrow(dup_check) == 0)

cat("\n================ S2: TEST ROWS ================\n")
cat("Rows per position:\n")
print(errs_wide %>% count(position), n = Inf)
cat("\nSeason x week, all positions:\n")
print(errs_wide %>% count(season, week) %>%
        pivot_wider(names_from = week, values_from = n, values_fill = 0L),
      n = Inf, width = Inf)
cat("\nRows per player (distribution), by position:\n")
rows_per_player <- errs_wide %>% count(position, player_id, name = "rows")
print(rows_per_player %>% count(position, rows, name = "players") %>%
        arrange(position, rows), n = Inf)
print(rows_per_player %>%
        summarise(players = n(), max_rows = max(rows),
                  mean_rows = mean(rows), .by = position), n = Inf)
cat("\nActual target summary (ppr_points_next_week), by position:\n")
print(errs_wide %>% summarise(n = n(), mean = mean(actual), median = median(actual),
                              min = min(actual), max = max(actual),
                              n_zero = sum(actual == 0), .by = position), n = Inf)

cat("\n================ S3: REPRODUCTION GATE ================\n")
gate <- map_dfr(POSITIONS, function(p) {
  w <- errs_wide %>% filter(position == p)
  map_dfr(MODEL_PAIRS, function(pr) {
    a <- w[[pr[1]]]; b <- w[[pr[2]]]
    rep_p <- suppressWarnings(stats::wilcox.test(a, b, paired = TRUE,
                                                 exact = FALSE, correct = TRUE)$p.value)
    tibble(position = p, comparison = paste0(pr[1], "_vs_", pr[2]),
           n = length(a), rerun_p = rep_p)
  })
}) %>%
  left_join(stored %>% select(position, comparison, stored_p = p_value,
                              better_model),
            by = join_by(position, comparison),
            relationship = "one-to-one", na_matches = "never") %>%
  mutate(abs_diff = abs(rerun_p - stored_p),
         match    = !is.na(abs_diff) & abs_diff < GATE_TOL)
print(gate, n = Inf, width = Inf)
cat("Gate:", sum(gate$match), "of", nrow(gate), "match within", GATE_TOL, "\n")
if (!all(gate$match)) {
  stop("Reproduction gate failed. Paste everything above back.")
}

cat("\n================ S4: SHAPE OF THE GAPS ================\n")
gaps <- map_dfr(POSITIONS, function(p) {
  w <- errs_wide %>% filter(position == p)
  map_dfr(MODEL_PAIRS, function(pr) {
    tibble(position = p, comparison = paste0(pr[1], "_vs_", pr[2]),
           player_id = w$player_id, season = w$season, week = w$week,
           err_a = w[[pr[1]]], err_b = w[[pr[2]]],
           d = w[[pr[1]]] - w[[pr[2]]])
  })
})
shape <- gaps %>%
  summarise(n = n(), n_zero = sum(d == 0), mean_d = mean(d), median_d = median(d),
            sd_d = sd(d), min_d = min(d), max_d = max(d), skew_d = skew_g1(d),
            shapiro_p = if (n() >= 3 && n() <= 5000) stats::shapiro.test(d)$p.value
                        else NA_real_,
            .by = c(position, comparison))
print(shape, n = Inf, width = Inf)

cat("\nRaw absolute error shape per model (checks the right-skew claim):\n")
raw_shape <- errs_long %>%
  summarise(n = n(), mean = mean(abs_error), median = median(abs_error),
            skew = skew_g1(abs_error), max = max(abs_error),
            .by = c(position, model))
print(raw_shape, n = Inf, width = Inf)

cat("\nQuantiles of the paired gaps (5, 25, 50, 75, 95 pct):\n")
print(gaps %>%
        reframe(prob = c(0.05, 0.25, 0.5, 0.75, 0.95),
                q = stats::quantile(d, c(0.05, 0.25, 0.5, 0.75, 0.95)),
                .by = c(position, comparison)) %>%
        pivot_wider(names_from = prob, values_from = q, names_prefix = "q"),
      n = Inf, width = Inf)

cat("\n================ S5: SAME PAIRS, TWO TESTS ================\n")
both <- gaps %>%
  reframe(two_tests(d), .by = c(position, comparison)) %>%
  mutate(direction_agree = sign(mean_gap) == sign(hl_est))
print(both, n = Inf, width = Inf)

cat("\nShare of the total absolute gap from the 5 largest gaps:\n")
print(gaps %>%
        summarise(total_abs = sum(abs(d)),
                  top5_abs  = sum(sort(abs(d), decreasing = TRUE)[seq_len(min(5L, n()))]),
                  .by = c(position, comparison)) %>%
        mutate(top5_share = ifelse(total_abs > 0, top5_abs / total_abs, NA_real_)),
      n = Inf, width = Inf)

cat("\n================ S6: DROP THE", DROP_TOP_K, "LARGEST |GAPS| ================\n")
trimmed <- gaps %>%
  mutate(rank_abs = rank(-abs(d), ties.method = "first"),
         .by = c(position, comparison)) %>%
  filter(rank_abs > DROP_TOP_K) %>%
  reframe(two_tests(d), .by = c(position, comparison))
print(trimmed %>% select(position, comparison, n, mean_gap, hl_est, t_p, w_p),
      n = Inf, width = Inf)
cat("\nChange in p-value after dropping (trimmed minus full):\n")
print(both %>% select(position, comparison, t_p_full = t_p, w_p_full = w_p) %>%
        left_join(trimmed %>% select(position, comparison, t_p_trim = t_p, w_p_trim = w_p),
                  by = join_by(position, comparison), relationship = "one-to-one") %>%
        mutate(t_change = t_p_trim - t_p_full, w_change = w_p_trim - w_p_full),
      n = Inf, width = Inf)

cat("\n================ S7: ONE VALUE PER PLAYER (MEAN GAP) ================\n")
per_player <- gaps %>%
  summarise(d = mean(d), rows = n(), .by = c(position, comparison, player_id))
cat("Players per comparison:\n")
print(per_player %>% count(position, comparison), n = Inf)
pp_tests <- per_player %>%
  reframe(two_tests(d), .by = c(position, comparison)) %>%
  mutate(direction_agree = sign(mean_gap) == sign(hl_est))
print(pp_tests, n = Inf, width = Inf)
cat("\nPer-player gap skewness and Shapiro-Wilk:\n")
print(per_player %>%
        summarise(n = n(), skew = skew_g1(d),
                  shapiro_p = if (n() >= 3 && n() <= 5000) stats::shapiro.test(d)$p.value
                              else NA_real_,
                  .by = c(position, comparison)), n = Inf, width = Inf)

cat("\n================ S8: ONE RANDOM WEEK PER PLAYER ================\n")
set.seed(SEED_DRAW)
draw_once <- function(g) {
  g %>% slice_sample(n = 1, by = c(position, comparison, player_id))
}
first_draw <- draw_once(gaps)
cat("Headline draw, seed", SEED_DRAW, ":\n")
print(first_draw %>% reframe(two_tests(d), .by = c(position, comparison)) %>%
        select(position, comparison, n, mean_gap, hl_est, t_p, w_p),
      n = Inf, width = Inf)

rerun_tbl <- map_dfr(seq_len(N_RERUNS), function(r) {
  draw_once(gaps) %>%
    reframe(two_tests(d), .by = c(position, comparison)) %>%
    select(position, comparison, t_p, w_p) %>%
    mutate(rerun = r)
})
cat("\n", N_RERUNS, "reruns: count with p < 0.05 and median p, by test:\n")
print(rerun_tbl %>%
        summarise(t_sig = sum(t_p < 0.05), w_sig = sum(w_p < 0.05),
                  t_med_p = median(t_p), w_med_p = median(w_p),
                  disagree = sum((t_p < 0.05) != (w_p < 0.05)),
                  .by = c(position, comparison)), n = Inf, width = Inf)

cat("\n================ DONE ================\n")
cat("Paste everything from S1 down, verbatim.\n")
