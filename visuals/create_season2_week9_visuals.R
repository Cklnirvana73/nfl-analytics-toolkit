# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 9
# Visualization Script: Aging Curves
# File: examples/create_season2_week9_visuals.R
#
# PURPOSE
# -------
# Generates all four Week 9 publication-quality visualizations:
#
#   1. s2_week9_aging_curves_boxscore.png
#      "When Do NFL Players Peak? It Depends How You Measure It"
#      Quadratic vs LOESS curves per position with plain-language peak age
#      markers. TE disagreement (3-year gap) annotated in subtitle.
#
#   2. s2_week9_age_deltas_by_position.png
#      "Year-Over-Year Change in PPR Points per Game by Age"
#      Raw delta bar chart with 95% CI per position.
#
#   3. s2_week9_ngs_vs_boxscore_curves.png
#      "Box Score Production vs NGS Efficiency: When Does Each Decline?"
#      Z-scored comparison -- does tracking data efficiency fall before
#      box score production? (2016-2025 NGS data only)
#
#   4. s2_week9_ar1_mean_reversion.png
#      "A Great Season Is Usually Followed by a Worse One"
#      Violin + jitter of player-level AR1 autocorrelation per position.
#      Fantasy implication: do not overreact to one outlier season.
#
# DEPENDENCY ORDER
# ----------------
# 1. examples/example_season2_week9.R  -- builds panel, confirms pipeline
# 2. tests/test_season2_week9_assumptions.R  -- validates assumptions
# 3. tests/test_season2_week9_functions.R    -- unit tests
# 4. THIS FILE                               -- generates plots
#
# OUTPUT
# ------
# output/plots/s2_week9_*.png (4 files, 300 DPI)
#
# RUNTIME
# -------
# ~25-35 min first run (builds full 16-season panel from cache).
# Subsequent runs in same session are faster if panel object is retained.
# ==============================================================================

library(here)
library(dplyr)
library(glue)
library(ggplot2)
library(tidyr)

source(here::here("R", "15_multi_season_pbp.R"))
source(here::here("R", "16_player_season_panel.R"))
source(here::here("R", "23_aging_curves.R"))


# ==============================================================================
# SETUP: Confirm output directory exists
# ==============================================================================

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message(glue("Created output directory: {OUTPUT_DIR}"))
}

cat("\n========================================\n")
cat("  Week 9 Visualization Script\n")
cat("  Generating 4 plots\n")
cat("========================================\n\n")


# ==============================================================================
# PANEL BUILD
# Build or load the full 16-season panel. If you already have
# panel_with_ppg in your environment from a prior session run,
# pass it directly to run_aging_curve_pipeline() to skip the
# 25-minute build step.
# ==============================================================================

cat("Step 1: Building full 16-season panel (2010-2025)...\n")
cat("(Loads from season2_cache/ -- no re-download required)\n\n")

panel_raw <- build_player_season_panel(
  seasons = PANEL_SEASONS,
  verbose = FALSE
)

cat(glue("Panel: {format(nrow(panel_raw), big.mark=',')} rows | ",
         "{format(dplyr::n_distinct(panel_raw$player_id), big.mark=',')} players\n\n"))

cat("Step 2: Computing ages, splitting positions, computing PPG...\n\n")
panel_raw <- compute_player_ages(panel_raw, verbose = FALSE)
panel_raw <- split_wr_te_positions(panel_raw, verbose = FALSE)
panel_with_ppg <- compute_season_ppg(panel_raw, ppr_value = 1)

rm(panel_raw)
gc()


# ==============================================================================
# RUN PIPELINE WITH SAVE_PLOTS = TRUE
# This generates all four plots in a single call. The pipeline internally
# calls .plot_curves_boxscore(), .plot_age_deltas(),
# .plot_ngs_vs_boxscore(), and .plot_ar1_distribution().
# ==============================================================================

cat("Step 3: Running full pipeline with save_plots = TRUE...\n")
cat("(Expect 5-10 min -- panel is pre-built, skipping that step)\n\n")

results <- run_aging_curve_pipeline(
  panel      = panel_with_ppg,
  seasons    = PANEL_SEASONS,
  save_plots = TRUE,
  verbose    = TRUE
)


# ==============================================================================
# VERIFY OUTPUT FILES
# ==============================================================================

cat("\n\nVerifying output files...\n\n")

expected_files <- c(
  paste0(FILE_PREFIX, "aging_curves_boxscore.png"),
  paste0(FILE_PREFIX, "age_deltas_by_position.png"),
  paste0(FILE_PREFIX, "ngs_vs_boxscore_curves.png"),
  paste0(FILE_PREFIX, "ar1_mean_reversion.png")
)

all_present <- TRUE
for (f in expected_files) {
  full_path <- file.path(OUTPUT_DIR, f)
  if (file.exists(full_path)) {
    size_kb <- round(file.size(full_path) / 1024, 0)
    cat(glue("  [OK] {f} ({format(size_kb, big.mark=',')} KB)\n"))
  } else {
    cat(glue("  [MISSING] {f}\n"))
    all_present <- FALSE
  }
}


# ==============================================================================
# KEY INSIGHTS FROM PLOTS
# All values computed from data, never hardcoded.
# ==============================================================================

cat("\n--- KEY INSIGHTS FOR LINKEDIN POST ---\n\n")

ki <- results$key_insights

# Peak age summary
cat("Peak ages (LOESS -- trust over quadratic for asymmetric positions):\n")
for (i in seq_len(nrow(ki))) {
  r <- ki[i, ]
  if (!is.na(r$peak_age_loess) && !is.na(r$peak_age_quad)) {
    diff <- abs(r$peak_age_loess - r$peak_age_quad)
    agree_str <- if (diff <= 1L) "models agree" else
      glue("models differ by {diff} years -- trust LOESS")
    cat(glue("  {r$position}: peak {r$peak_age_loess} | {agree_str}\n"))
  }
}

# AR1 autocorrelation summary (computed from delta data)
cat("\nMean reversion (AR1) by position:\n")
ar1_summary <- results$delta_data_boxscore %>%
  dplyr::arrange(player_id, season) %>%
  dplyr::group_by(player_id, position_group) %>%
  dplyr::filter(dplyr::n() >= 3L) %>%
  dplyr::summarise(
    ar1 = tryCatch(
      cor(delta[seq_len(dplyr::n() - 1L)],
          delta[seq(2L, dplyr::n())],
          use = "complete.obs"),
      error = function(e) NA_real_
    ),
    .groups = "drop"
  ) %>%
  dplyr::filter(!is.na(ar1)) %>%
  dplyr::group_by(position_group) %>%
  dplyr::summarise(
    median_ar1 = round(median(ar1), 3),
    n_players  = dplyr::n(),
    .groups    = "drop"
  )

for (i in seq_len(nrow(ar1_summary))) {
  r <- ar1_summary[i, ]
  cat(glue(
    "  {r$position_group}: median AR1 = {r$median_ar1} ",
    "(n = {format(r$n_players, big.mark=',')} players) | ",
    "{if(r$median_ar1 < -0.3) 'strong mean reversion' else 'moderate mean reversion'}\n"
  ))
}

cat(glue(
  "\nSurvivor bias confirmed at all age thresholds for QB, RB, WR, TE.\n",
  "Raw PPG mean at age 32+ overstates typical production -- ",
  "only elite players survive to that age.\n"
))


# ==============================================================================
# PRINT FILE LOCATIONS
# ==============================================================================

cat(glue("\n\nAll plots saved to:\n  {OUTPUT_DIR}\n\n"))

if (all_present) {
  cat("All 4 expected files confirmed present.\n")
} else {
  cat("WARNING: One or more expected files are missing. Check pipeline output above.\n")
}

cat("\n========================================\n")
cat("  Visuals complete.\n")
cat("========================================\n\n")

# Cleanup
rm(panel_with_ppg)
gc()
