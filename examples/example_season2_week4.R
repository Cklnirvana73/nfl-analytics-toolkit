# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 4
# Usage Examples
# File: examples/example_season2_week4.R
#
# Purpose: End-to-end demonstration of the recency bias A/B test pipeline.
#          Runs all functions on real 16-season data and prints findings
#          to the console. Run this script BEFORE the assumption tests and
#          visualization scripts -- it writes the RDS outputs they depend on.
#
# Prerequisites:
#   1. Season 2 cache populated via load_multi_season_pbp() from R/15
#   2. R packages installed: dplyr, tidyr, purrr, glue, here, boot, pwr
#
# Runtime: ~5-15 minutes depending on machine (16 seasons x fantasy scoring)
#          Subsequent runs are faster -- PBP is cached, only scoring recomputes.
#
# Outputs written to data/season2_cache/:
#   s2_week4_ab_panel.rds        -- full player-week panel
#   s2_week4_ab_results.rds      -- comparison results with FDR correction
#   s2_week4_power_analysis.rds  -- power analysis results
#
# Examples:
#   1. Full pipeline (recommended first run)
#   2. Inspect the panel structure
#   3. Inspect results by position
#   4. Quick single-season smoke test
#   5. Inspect power analysis output
#   6. Re-run comparison on a position subset
#   7. Explore the season-to-date vs roll3 predictor relationship
#
# Season 2 schema tag : s2_ab_v1
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(here)
library(boot)
library(pwr)

source(here("R", "15_multi_season_pbp.R"))
source(here("R", "05_consistency_metrics.R"))
source(here("R", "18_predictive_validity_ab_testing.R"))

# Output and cache paths
CACHE_DIR  <- here("data", "season2_cache")
OUTPUT_DIR <- here("output")


# ==============================================================================
# EXAMPLE 1: Full 16-Season Pipeline (Recommended First Run)
# ==============================================================================
# Runs the complete study end-to-end across all 16 seasons.
# Saves three RDS files that all downstream scripts depend on.
# Review the console output for KEY FINDINGS before proceeding.
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("  EXAMPLE 1: Full 16-Season A/B Test Pipeline\n")
cat("==============================================================================\n")
cat("  Research question: Does rolling 3-game average predict next-week\n")
cat("  fantasy points better than season-to-date average?\n")
cat("  Seasons: 2010-2025 | Positions: QB, RB, WR\n")
cat("==============================================================================\n\n")

output <- run_ab_test_pipeline(
  seasons     = 2010:2025,
  cache_dir   = CACHE_DIR,
  n_bootstrap = 2000L,
  mde_q       = 0.10,
  seed        = 42L,
  save_panel  = TRUE
)

cat("\nExample 1 complete. Three RDS files written to data/season2_cache/\n")
cat("Proceed to Example 2 to inspect the panel, or open the results directly:\n")
cat(glue("  readRDS(here('data', 'season2_cache', 's2_week4_ab_results.rds'))\n\n"))


# ==============================================================================
# EXAMPLE 2: Inspect the Player-Week Panel Structure
# ==============================================================================
# The panel is the core analytical dataset. Each row is one eligible
# player-week. Inspect it to confirm the filtering and predictor construction
# worked as expected before trusting the correlation results.
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("  EXAMPLE 2: Inspect the Player-Week Panel\n")
cat("==============================================================================\n\n")

panel <- output$panel

cat(glue("Panel dimensions: {format(nrow(panel), big.mark = ',')} rows x {ncol(panel)} columns\n\n"))

cat("Column names and types:\n")
glimpse(panel)

cat("\n--- Row counts by position ---\n")
panel %>%
  count(position, name = "player_weeks") %>%
  mutate(pct = round(player_weeks / sum(player_weeks) * 100, 1)) %>%
  arrange(desc(player_weeks)) %>%
  print()

cat("\n--- Season coverage ---\n")
panel %>%
  group_by(season) %>%
  summarise(
    player_weeks = n(),
    n_players    = n_distinct(player_id),
    .groups      = "drop"
  ) %>%
  arrange(season) %>%
  print(n = Inf)

cat("\n--- Predictor summary statistics by position ---\n")
panel %>%
  group_by(position) %>%
  summarise(
    n               = n(),
    mean_actual     = round(mean(fantasy_pts_actual,  na.rm = TRUE), 2),
    mean_roll3      = round(mean(fantasy_pts_roll3,   na.rm = TRUE), 2),
    mean_std        = round(mean(fantasy_pts_std,     na.rm = TRUE), 2),
    sd_actual       = round(sd(fantasy_pts_actual,    na.rm = TRUE), 2),
    sd_roll3        = round(sd(fantasy_pts_roll3,     na.rm = TRUE), 2),
    sd_std          = round(sd(fantasy_pts_std,       na.rm = TRUE), 2),
    .groups         = "drop"
  ) %>%
  print()

# KEY INSIGHT: computed from data
n_total    <- nrow(panel)
n_seasons  <- n_distinct(panel$season)
pos_counts <- panel %>% count(position)
wb_count   <- pos_counts %>% filter(position == "WR") %>% pull(n)
rb_count   <- pos_counts %>% filter(position == "RB") %>% pull(n)
qb_count   <- pos_counts %>% filter(position == "QB") %>% pull(n)

cat(glue("\n--- KEY INSIGHT ---\n"))
cat(glue(
  "Panel contains {format(n_total, big.mark = ',')} eligible player-weeks ",
  "across {n_seasons} seasons.\n"
))
cat(glue(
  "WR: {format(wb_count, big.mark = ',')} | ",
  "RB: {format(rb_count, big.mark = ',')} | ",
  "QB: {format(qb_count, big.mark = ',')}\n"
))
cat(glue(
  "Each row represents one player-week where both predictors were fully\n",
  "populated (>= 3 prior games in that season).\n\n"
))


# ==============================================================================
# EXAMPLE 3: Inspect A/B Results by Position
# ==============================================================================
# The results tibble is the primary study output. Each row is one position.
# Columns include both raw and FDR-adjusted p-values, Cohen's q, and 95% CIs.
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("  EXAMPLE 3: A/B Comparison Results by Position\n")
cat("==============================================================================\n\n")

results <- output$results

cat("Full results table:\n")
results %>%
  select(position, n_player_weeks, r_roll3, r_std, cohens_q,
         p_value_raw, p_value_adj, significant_adj, better_predictor) %>%
  print()

cat("\n--- Correlation confidence intervals ---\n")
results %>%
  select(position, r_roll3, r_roll3_ci_lo, r_roll3_ci_hi,
         r_std, r_std_ci_lo, r_std_ci_hi) %>%
  mutate(
    roll3_ci = glue("[{r_roll3_ci_lo}, {r_roll3_ci_hi}]"),
    std_ci   = glue("[{r_std_ci_lo},  {r_std_ci_hi}]")
  ) %>%
  select(position, r_roll3, roll3_ci, r_std, std_ci) %>%
  print()

# KEY INSIGHT: computed from data
n_sig <- sum(results$significant_adj)
sig_positions <- results %>%
  filter(significant_adj) %>%
  pull(position)

no_diff_positions <- results %>%
  filter(better_predictor == "no_difference") %>%
  pull(position)

best <- results %>%
  arrange(desc(cohens_q)) %>%
  slice(1)

cat(glue("\n--- KEY INSIGHT ---\n"))
cat(glue(
  "{n_sig}/3 positions show a statistically significant difference ",
  "between predictors (FDR-corrected alpha = 0.05).\n"
))

if (length(sig_positions) > 0) {
  cat(glue(
    "Significant positions: {paste(sig_positions, collapse = ', ')}\n"
  ))
}
if (length(no_diff_positions) > 0) {
  cat(glue(
    "No difference: {paste(no_diff_positions, collapse = ', ')} -- ",
    "recency and season average predict equally well for these positions.\n"
  ))
}
cat(glue(
  "Largest effect: {best$position} (Cohen's q = {best$cohens_q}, ",
  "better predictor: {best$better_predictor})\n"
))
cat(glue(
  "Cohen's q benchmarks: 0.10 = small, 0.30 = medium, 0.50 = large.\n\n"
))


# ==============================================================================
# EXAMPLE 4: Quick Smoke Test on 3 Recent Seasons
# ==============================================================================
# If Example 1 errors or takes too long, run this first to confirm the
# pipeline executes without issues before committing to the full 16-season run.
# Results will differ from the full run (smaller n, fewer seasons).
# Do NOT use these results for the study -- use the full run.
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("  EXAMPLE 4: Quick Smoke Test (3 Recent Seasons)\n")
cat("==============================================================================\n")
cat("  NOTE: For diagnostic use only. Do not use these results for the study.\n")
cat("  Use Example 1 (full 16-season run) for all reported findings.\n")
cat("==============================================================================\n\n")

output_test <- run_ab_test_pipeline(
  seasons     = 2023:2025,
  cache_dir   = CACHE_DIR,
  n_bootstrap = 500L,   # fewer replicates for speed
  mde_q       = 0.10,
  seed        = 42L,
  save_panel  = FALSE   # do not overwrite the full-run panel
)

n_test <- nrow(output_test$panel)
cat(glue(
  "\nSmoke test complete: {format(n_test, big.mark = ',')} ",
  "player-weeks from 3 seasons.\n"
))
cat("Direction of effects (may differ from full run due to small n):\n")
output_test$results %>%
  select(position, r_roll3, r_std, better_predictor) %>%
  print()
cat("\n")


# ==============================================================================
# EXAMPLE 5: Inspect Power Analysis Output
# ==============================================================================
# Review whether the study was adequately powered to detect a small recency
# effect. An underpowered position means a null result is uninformative.
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("  EXAMPLE 5: Power Analysis\n")
cat("==============================================================================\n\n")

power <- output$power

cat("Required sample size to detect Cohen's q = 0.10 at 80% power:\n")
power$required_n %>% print()

cat("\nAchieved power at observed n per position:\n")
power$achieved_power %>%
  mutate(
    adequately_powered = achieved_power >= 0.80,
    power_pct          = glue("{round(achieved_power * 100, 1)}%")
  ) %>%
  select(position, n_player_weeks, power_pct, adequately_powered) %>%
  print()

# KEY INSIGHT: computed from data
req_n      <- power$required_n$required_n_per_group
underpow   <- power$achieved_power %>% filter(achieved_power < 0.80)
fully_pow  <- power$achieved_power %>% filter(achieved_power >= 0.80)

cat(glue("\n--- KEY INSIGHT ---\n"))
cat(glue(
  "To detect a small recency effect (Cohen's q = 0.10) at 80% power, ",
  "{req_n} player-weeks per position are required.\n"
))
if (nrow(underpow) > 0) {
  cat(glue(
    "Underpowered: {paste(underpow$position, collapse = ', ')} -- ",
    "null results for these positions should not be interpreted as ",
    "evidence of no recency effect.\n"
  ))
}
if (nrow(fully_pow) > 0) {
  cat(glue(
    "Adequately powered: {paste(fully_pow$position, collapse = ', ')}\n"
  ))
}
cat("\n")


# ==============================================================================
# EXAMPLE 6: Re-Run Comparison on a Single Position
# ==============================================================================
# Use compare_prediction_methods() directly on a position subset.
# Useful for exploring sensitivity to inclusion criteria or checking
# whether results hold on a specific time window.
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("  EXAMPLE 6: Single-Position Comparison (WR Only)\n")
cat("==============================================================================\n\n")

panel_wr <- output$panel %>% filter(position == "WR")

cat(glue("WR panel: {format(nrow(panel_wr), big.mark = ',')} player-weeks\n\n"))

wr_result <- compare_prediction_methods(
  panel       = panel_wr,
  n_bootstrap = 1000L,
  seed        = 42L
)

cat("WR-only result:\n")
wr_result %>%
  select(position, n_player_weeks, r_roll3, r_std, cohens_q,
         p_value_raw, better_predictor) %>%
  print()

# Verify consistent with full-run result
full_wr_r_roll3 <- output$results %>%
  filter(position == "WR") %>%
  pull(r_roll3)

cat(glue(
  "\nConsistency check: full-run WR r_roll3 = {full_wr_r_roll3}, ",
  "single-position run r_roll3 = {round(wr_result$r_roll3, 4)}\n"
))
cat(glue(
  "Small differences expected due to bootstrap resampling (seed preserved).\n\n"
))


# ==============================================================================
# EXAMPLE 7: Explore the Predictor Relationship Directly
# ==============================================================================
# Before trusting the correlation comparison, inspect the raw relationship
# between each predictor and the outcome. This is the foundation for the
# assumption tests in tests/test_season2_week4_assumptions.R.
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("  EXAMPLE 7: Raw Predictor-Outcome Relationship\n")
cat("==============================================================================\n\n")

cat("Correlation matrix by position (roll3, std, actual):\n\n")

for (pos in c("QB", "RB", "WR")) {

  pos_data <- output$panel %>% filter(position == pos)

  cor_mat <- pos_data %>%
    select(fantasy_pts_roll3, fantasy_pts_std, fantasy_pts_actual) %>%
    cor(use = "complete.obs") %>%
    round(3)

  cat(glue("--- {pos} (n = {format(nrow(pos_data), big.mark = ',')}) ---\n"))
  print(cor_mat)
  cat("\n")
}

cat("--- Predictor-to-predictor correlation (r_roll3_std) ---\n")
cat("High r_roll3_std means the predictors are similar and the test\n")
cat("has low sensitivity to detect a difference between them.\n\n")

r_pred_by_pos <- output$panel %>%
  group_by(position) %>%
  summarise(
    r_roll3_vs_std = round(
      cor(fantasy_pts_roll3, fantasy_pts_std, use = "complete.obs"), 3
    ),
    .groups = "drop"
  )

r_pred_by_pos %>% print()

# KEY INSIGHT: computed from data
max_r_pred <- r_pred_by_pos %>%
  arrange(desc(r_roll3_vs_std)) %>%
  slice(1)

min_r_pred <- r_pred_by_pos %>%
  arrange(r_roll3_vs_std) %>%
  slice(1)

cat(glue("\n--- KEY INSIGHT ---\n"))
cat(glue(
  "Predictors are most similar for {max_r_pred$position} ",
  "(r = {max_r_pred$r_roll3_vs_std}) -- roll3 and STD carry nearly the\n",
  "same information for this position, limiting test sensitivity.\n"
))
cat(glue(
  "Predictors diverge most for {min_r_pred$position} ",
  "(r = {min_r_pred$r_roll3_vs_std}) -- the two weightings capture\n",
  "meaningfully different signals here.\n"
))
cat(glue(
  "This directly informs which positions are most interesting for the\n",
  "recency bias question.\n\n"
))

cat("==============================================================================\n")
cat("  All examples complete.\n")
cat("  Next step: run tests/test_season2_week4_assumptions.R\n")
cat("  That script loads the RDS outputs written here and runs formal\n")
cat("  assumption checks (autocorrelation, linearity, season stability).\n")
cat("==============================================================================\n\n")
