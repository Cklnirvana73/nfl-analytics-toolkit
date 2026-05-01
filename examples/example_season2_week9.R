# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 9
# Usage Examples: Aging Curves
# File: examples/example_season2_week9.R
#
# Purpose: 7 self-contained examples demonstrating all Week 9 functions.
#          Each section can be run independently after sourcing R/23 and
#          having a populated season2_cache/ from R/15 and R/16.
#
# RUNTIME NOTES
# -------------
# Examples 1-5 use a 2018-2025 subset (8 seasons) for fast iteration.
# Example 6 demonstrates the full 2010-2025 pipeline (20-30 min first run).
# Example 7 inspects NGS data availability without a full pipeline run.
# Run examples 1-5 first to verify the setup before committing to example 6.
#
# Prerequisites:
#   - R/15_multi_season_pbp.R sourced (load_normalized_season available)
#   - R/16_player_season_panel.R sourced (build_player_season_panel available)
#   - R/23_aging_curves.R sourced
#   - Season cache populated for 2010-2025 via load_multi_season_pbp()
#   - nflreadr installed and internet access available (for DOB and NGS)
#
# Production-grade for portfolio display
# ==============================================================================

library(here)
library(dplyr)
library(glue)

# Source all required production files
source(here::here("R", "15_multi_season_pbp.R"))
source(here::here("R", "16_player_season_panel.R"))
source(here::here("R", "23_aging_curves.R"))


# ==============================================================================
# EXAMPLE 1: Build Panel Subset and Compute Player Ages
# ==============================================================================
# The September 1 age convention assigns each player a single integer age
# per season regardless of birth month. This example verifies that the
# age join works correctly on a recent 8-season subset and inspects the
# age distribution across positions.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 1: Build Panel Subset and Compute Player Ages\n")
cat("================================================================\n\n")

cat("Building player-season panel for 2018-2025 (8 seasons)...\n")
cat("(Uses season2_cache/ populated by R/15 and R/16. No re-download needed.)\n\n")

panel_subset <- build_player_season_panel(
  seasons  = 2018:2025,
  verbose  = TRUE
)

cat(glue("\nPanel dimensions: {format(nrow(panel_subset), big.mark = ',')} rows x ",
         "{ncol(panel_subset)} columns\n"))
cat(glue("Unique players  : {format(dplyr::n_distinct(panel_subset$player_id), big.mark = ',')}\n"))
cat(glue("Seasons covered : {min(panel_subset$season)}-{max(panel_subset$season)}\n"))
cat(glue("Positions       : {paste(sort(unique(panel_subset$position_group[!is.na(panel_subset$position_group)])), collapse = ', ')}\n\n"))

cat("Computing player ages (September 1 convention)...\n")
panel_with_ages <- compute_player_ages(panel_subset, verbose = TRUE)

cat("Splitting WR_TE into separate WR and TE position groups...\n")
panel_with_ages <- split_wr_te_positions(panel_with_ages, verbose = TRUE)

# Age distribution by position
cat("\n--- Age distribution by position (non-NA ages only) ---\n")
age_dist <- panel_with_ages %>%
  dplyr::filter(
    !is.na(age_at_season_start),
    position_group %in% c("QB", "RB", "WR", "TE")
  ) %>%
  dplyr::group_by(position_group) %>%
  dplyr::summarise(
    n_player_seasons = dplyr::n(),
    age_min  = min(age_at_season_start, na.rm = TRUE),
    age_mean = round(mean(age_at_season_start, na.rm = TRUE), 1),
    age_max  = max(age_at_season_start, na.rm = TRUE),
    pct_with_age = round(mean(!is.na(age_at_season_start)) * 100, 1),
    .groups = "drop"
  )

for (i in seq_len(nrow(age_dist))) {
  r <- age_dist[i, ]
  cat(glue(
    "  {r$position_group}: n={format(r$n_player_seasons, big.mark=',')} | ",
    "ages {r$age_min}-{r$age_max} | mean={r$age_mean} | ",
    "{r$pct_with_age}% with DOB\n"
  ))
}

cat("\n")
rm(panel_subset)
gc(verbose = FALSE)


# ==============================================================================
# EXAMPLE 2: Compute PPR Points Per Game and Inspect by Position
# ==============================================================================
# Fantasy production distribution varies dramatically by position.
# QBs score highest due to passing bonuses; RBs have the widest spread.
# This example shows the PPG distribution before any age analysis.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 2: Compute PPR Points Per Game -- Distribution by Position\n")
cat("================================================================\n\n")

panel_with_ppg <- compute_season_ppg(panel_with_ages, ppr_value = 1)

# Filter to qualifying players (not low volume) for a meaningful distribution
ppg_summary <- panel_with_ppg %>%
  dplyr::filter(
    !is.na(fp_per_game),
    !low_volume,
    position_group %in% c("QB", "RB", "WR", "TE")
  ) %>%
  dplyr::group_by(position_group) %>%
  dplyr::summarise(
    n_player_seasons = dplyr::n(),
    ppg_p10  = round(quantile(fp_per_game, 0.10, na.rm = TRUE), 1),
    ppg_p25  = round(quantile(fp_per_game, 0.25, na.rm = TRUE), 1),
    ppg_p50  = round(median(fp_per_game, na.rm = TRUE), 1),
    ppg_p75  = round(quantile(fp_per_game, 0.75, na.rm = TRUE), 1),
    ppg_p90  = round(quantile(fp_per_game, 0.90, na.rm = TRUE), 1),
    ppg_mean = round(mean(fp_per_game, na.rm = TRUE), 1),
    .groups  = "drop"
  )

cat("PPR PPG distribution by position (qualifying players, 2018-2025):\n\n")
cat(sprintf("  %-4s  %6s  %5s  %5s  %5s  %5s  %5s  %5s\n",
            "Pos", "n", "P10", "P25", "P50", "P75", "P90", "Mean"))
cat(sprintf("  %-4s  %6s  %5s  %5s  %5s  %5s  %5s  %5s\n",
            "---", "------", "-----", "-----", "-----", "-----", "-----", "-----"))

for (i in seq_len(nrow(ppg_summary))) {
  r <- ppg_summary[i, ]
  cat(sprintf("  %-4s  %6s  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f\n",
              r$position_group,
              format(r$n_player_seasons, big.mark = ","),
              r$ppg_p10, r$ppg_p25, r$ppg_p50,
              r$ppg_p75, r$ppg_p90, r$ppg_mean))
}

cat("\n")
rm(ppg_summary)
gc(verbose = FALSE)


# ==============================================================================
# EXAMPLE 3: Delta Method -- Year-Over-Year Changes
# ==============================================================================
# The delta method tracks each player's change from one season to the next.
# Only consecutive seasons are included -- gap years are excluded by design.
# This example shows how many transitions we get per position and
# demonstrates the survivor bias problem the delta method solves.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 3: Delta Method -- Year-Over-Year Changes by Position\n")
cat("================================================================\n\n")

delta_data <- compute_age_deltas(
  panel_with_ages = panel_with_ppg,
  metric_col      = "fp_per_game",
  positions       = c("QB", "RB", "WR", "TE"),
  age_min         = AGE_MIN,
  age_max         = AGE_MAX
)

cat(glue("Total consecutive-season transitions: ",
         "{format(nrow(delta_data), big.mark = ',')}\n\n"))

# Transitions per position
trans_by_pos <- delta_data %>%
  dplyr::group_by(position_group) %>%
  dplyr::summarise(
    n_transitions  = dplyr::n(),
    n_players      = dplyr::n_distinct(player_id),
    age_min        = min(age_at_season_start, na.rm = TRUE),
    age_max        = max(age_at_season_start, na.rm = TRUE),
    mean_delta     = round(mean(delta, na.rm = TRUE), 2),
    .groups        = "drop"
  )

cat("Transitions by position:\n")
cat(sprintf("  %-4s  %7s  %7s  %10s  %6s\n",
            "Pos", "Trans", "Players", "Age Range", "Avg Delta"))
cat(sprintf("  %-4s  %7s  %7s  %10s  %6s\n",
            "---", "-------", "-------", "----------", "---------"))

for (i in seq_len(nrow(trans_by_pos))) {
  r <- trans_by_pos[i, ]
  cat(sprintf("  %-4s  %7s  %7s  %5s-%4s  %+6.2f\n",
              r$position_group,
              format(r$n_transitions, big.mark = ","),
              format(r$n_players, big.mark = ","),
              r$age_min, r$age_max,
              r$mean_delta))
}

# Survivor bias illustration: compare raw mean by age vs delta mean
# at age 32+ (where survivor bias is most severe)
cat("\n--- Survivor bias check at age 32+ ---\n")
cat("Raw mean (only players still playing at 32+) vs delta mean (includes those who declined and left):\n\n")

raw_over_32 <- panel_with_ppg %>%
  dplyr::filter(
    !is.na(age_at_season_start),
    age_at_season_start >= 32L,
    !low_volume,
    position_group %in% c("QB", "RB", "WR", "TE")
  ) %>%
  dplyr::group_by(position_group) %>%
  dplyr::summarise(
    raw_mean_ppg = round(mean(fp_per_game, na.rm = TRUE), 2),
    n_raw        = dplyr::n(),
    .groups      = "drop"
  )

delta_over_32 <- delta_data %>%
  dplyr::filter(age_at_season_start >= 32L) %>%
  dplyr::group_by(position_group) %>%
  dplyr::summarise(
    delta_mean = round(mean(delta, na.rm = TRUE), 2),
    n_delta    = dplyr::n(),
    .groups    = "drop"
  )

bias_check <- dplyr::left_join(raw_over_32, delta_over_32, by = "position_group")

for (i in seq_len(nrow(bias_check))) {
  r <- bias_check[i, ]
  if (!is.na(r$raw_mean_ppg) && !is.na(r$delta_mean)) {
    cat(glue(
      "  {r$position_group}: raw mean PPG = {r$raw_mean_ppg} (n={r$n_raw}) | ",
      "avg YoY change = {r$delta_mean} (n={r$n_delta})\n"
    ))
  }
}

cat("\n  Positive raw mean + negative avg change = survivor bias confirmed.\n")
cat("  Only above-average players survive to age 32+, inflating the raw mean.\n")

cat("\n")
rm(trans_by_pos, raw_over_32, delta_over_32, bias_check)
gc(verbose = FALSE)


# ==============================================================================
# EXAMPLE 4: Fit Aging Curves for One Position
# ==============================================================================
# Demonstrates the full fit_aging_curves() output for RBs -- the position
# with the most dramatic and analytically interesting aging pattern.
# Shows peak age from both quadratic and LOESS fits.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 4: Fit Aging Curves -- Running Back Deep Dive\n")
cat("================================================================\n\n")

cat("Fitting quadratic + LOESS curves for RBs (2018-2025 subset)...\n")
cat("Note: 8-season subset produces noisier curves than the full 16-season run.\n\n")

rb_curves <- fit_aging_curves(
  delta_data   = delta_data,
  position     = "RB",
  baseline_age = AGE_BASELINE,
  min_obs      = MIN_AGE_OBS,
  loess_span   = LOESS_SPAN
)

if (!is.null(rb_curves)) {

  cat(glue("Peak age (quadratic) : {rb_curves$peak_age_quad}\n"))
  cat(glue("Peak age (LOESS)     : {rb_curves$peak_age_loess}\n"))
  cat(glue("Sparse age buckets   : {length(rb_curves$sparse_ages)} ",
           "(ages with < {MIN_AGE_OBS} transitions)\n"))
  if (length(rb_curves$sparse_ages) > 0L) {
    cat(glue("  Sparse ages: {paste(rb_curves$sparse_ages, collapse = ', ')}\n"))
  }

  cat("\nAge-by-age delta summary (RB, ages with data):\n")
  cat(sprintf("  %4s  %7s  %7s  %5s\n", "Age", "Mean Delta", "SE", "N"))
  cat(sprintf("  %4s  %7s  %7s  %5s\n", "---", "----------", "---", "-"))

  rb_age_summary <- rb_curves$age_summary %>%
    dplyr::arrange(age_at_season_start)

  for (i in seq_len(nrow(rb_age_summary))) {
    r <- rb_age_summary[i, ]
    sparse_flag <- if (r$sparse) " *" else ""
    cat(sprintf("  %4d  %+7.2f  %7s  %5d%s\n",
                r$age_at_season_start,
                r$mean_delta,
                if (is.na(r$se_delta)) "  NA" else sprintf("%5.2f", r$se_delta),
                r$n_obs,
                sparse_flag))
  }
  cat("  (* = below MIN_AGE_OBS threshold)\n")

  # KEY INSIGHT: computed from data, not hardcoded
  if (!is.na(rb_curves$peak_age_quad) && !is.na(rb_curves$peak_age_loess)) {
    model_agree <- abs(rb_curves$peak_age_quad - rb_curves$peak_age_loess) <= 1L
    cat(glue(
      "\n  KEY INSIGHT: RB peak age is {rb_curves$peak_age_loess} per LOESS and ",
      "{rb_curves$peak_age_quad} per quadratic. ",
      "Models {if(model_agree) 'agree' else 'disagree'} within 1 year.\n"
    ))
  }
} else {
  cat("  RB curve fitting returned NULL -- check delta data for this position.\n")
}

cat("\n")
rm(rb_curves, rb_age_summary)
gc(verbose = FALSE)


# ==============================================================================
# EXAMPLE 5: Assumption Validation on Subset Data
# ==============================================================================
# Before trusting any aging curve, validate the statistical assumptions
# underlying the delta method. This includes sample size per age bucket,
# consecutive transition rates, and delta distribution normality.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 5: Assumption Validation\n")
cat("================================================================\n\n")

validation <- validate_aging_assumptions(
  panel_with_ages = panel_with_ppg,
  delta_data      = delta_data,
  ngs_panel       = NULL,
  verbose         = TRUE
)

cat(glue("\nOverall valid: {validation$valid}\n"))
cat(glue("Checks run: {nrow(validation$report)}\n"))
cat(glue("Critical checks passed: {sum(validation$report$result[validation$report$critical])}/",
         "{sum(validation$report$critical)}\n"))
cat(glue("Informational flags: {sum(!validation$report$result[!validation$report$critical])}\n"))

# Age coverage per position
cat("\nAge coverage by position:\n")
for (i in seq_len(nrow(validation$age_coverage))) {
  r <- validation$age_coverage[i, ]
  cat(glue("  {r$position_group}: {format(r$n_player_seasons, big.mark=',')} player-seasons | ",
           "ages {r$age_min}-{r$age_max}\n"))
}

cat("\n")
rm(validation)
gc(verbose = FALSE)


# ==============================================================================
# EXAMPLE 6: Full Pipeline -- All Four Positions, No Plots
# ==============================================================================
# Runs the complete run_aging_curve_pipeline() on the 2018-2025 subset.
# save_plots = FALSE so no file I/O. This confirms end-to-end execution
# and computes KEY INSIGHTS from actual data before the full 16-season run.
#
# TIMING: ~25-35 minutes on first run (builds deltas across 16 seasons).
# Faster on subsequent runs if panel is pre-loaded from cache.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 6: Full Pipeline -- All Positions, 2010-2025 (Full 16-Season Run)\n")
cat("================================================================\n\n")

cat("Running full aging curve pipeline on 16 seasons (save_plots = FALSE)...\n")
cat("This is the definitive run. Expect 25-35 minutes on first execution.\n\n")

results <- run_aging_curve_pipeline(
  panel      = panel_with_ppg,
  seasons    = 2010:2025,
  save_plots = FALSE,
  verbose    = TRUE
)

# KEY INSIGHTS computed from results -- never hardcoded
cat("\n--- KEY INSIGHTS FROM PIPELINE ---\n\n")

ki <- results$key_insights
for (i in seq_len(nrow(ki))) {
  r <- ki[i, ]
  quad_str  <- if (is.na(r$peak_age_quad))  "NA" else as.character(r$peak_age_quad)
  loess_str <- if (is.na(r$peak_age_loess)) "NA" else as.character(r$peak_age_loess)
  cat(glue(
    "  {r$position}: peak age {loess_str} (LOESS) / {quad_str} (quad) | ",
    "{format(r$n_transitions, big.mark=',')} transitions | ",
    "{r$n_sparse_ages} sparse age buckets\n"
  ))
}

# Model agreement check across positions
cat("\nModel agreement (|LOESS peak - quad peak| <= 1):\n")
for (i in seq_len(nrow(ki))) {
  r <- ki[i, ]
  if (!is.na(r$peak_age_quad) && !is.na(r$peak_age_loess)) {
    diff   <- abs(r$peak_age_quad - r$peak_age_loess)
    status <- if (diff <= 1L) "AGREE" else glue("DIFFER by {diff} years")
    cat(glue("  {r$position}: {status}\n"))
  }
}

# Validation summary
cat(glue(
  "\nAssumption validation: {if(results$validation$valid) 'ALL CRITICAL CHECKS PASSED' ",
  "else 'CRITICAL CHECK FAILED -- review before interpreting curves'}\n"
))

cat("\n")
rm(delta_data)
gc(verbose = FALSE)


# ==============================================================================
# EXAMPLE 7: NGS Data Availability Check
# ==============================================================================
# Verifies that load_nextgen_stats() returns data for the NGS seasons
# and shows which positions have sufficient coverage for aging curve analysis.
# Does not run the full NGS pipeline -- just confirms data availability.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 7: NGS Data Availability Check (2022-2025)\n")
cat("================================================================\n\n")

cat("Loading NGS data for 2022-2025 (fast subset check)...\n\n")

ngs_check <- load_ngs_season_panel(
  seasons = 2022:2025,
  verbose = TRUE
)

cat(glue("\nNGS panel: {format(nrow(ngs_check), big.mark=',')} player-seasons\n"))
cat(glue("Unique players: {format(dplyr::n_distinct(ngs_check$player_id), big.mark=',')}\n"))
cat(glue("Seasons: {paste(sort(unique(ngs_check$season)), collapse=', ')}\n\n"))

# Coverage per NGS metric
ngs_metrics <- c("cpoe", "avg_separation", "avg_yac_above_expectation",
                 "rush_yards_over_expected_per_att", "ngs_efficiency")

cat("NGS metric availability:\n")
for (m in ngs_metrics) {
  if (m %in% names(ngs_check)) {
    n_non_na <- sum(!is.na(ngs_check[[m]]))
    pct      <- round(n_non_na / nrow(ngs_check) * 100, 1)
    cat(glue("  {m}: {format(n_non_na, big.mark=',')} non-NA ({pct}%)\n"))
  } else {
    cat(glue("  {m}: COLUMN NOT PRESENT\n"))
  }
}

# Position breakdown in NGS
cat("\nNGS player positions:\n")
pos_counts <- ngs_check %>%
  dplyr::count(player_position, sort = TRUE) %>%
  dplyr::filter(!is.na(player_position))

for (i in seq_len(min(nrow(pos_counts), 8L))) {
  r <- pos_counts[i, ]
  cat(glue("  {r$player_position}: {format(r$n, big.mark=',')}\n"))
}

# KEY INSIGHT: confirm CPOE available for QBs, separation for WR/TE
qb_cpoe_n <- ngs_check %>%
  dplyr::filter(player_position == "QB", !is.na(cpoe)) %>%
  nrow()
wr_sep_n <- ngs_check %>%
  dplyr::filter(player_position %in% c("WR", "TE"), !is.na(avg_separation)) %>%
  nrow()

cat(glue(
  "\n  KEY INSIGHT: {format(qb_cpoe_n, big.mark=',')} QB-seasons with CPOE | ",
  "{format(wr_sep_n, big.mark=',')} WR/TE-seasons with separation data\n"
))

cat("\n")
rm(ngs_check, pos_counts)
gc(verbose = FALSE)

# ==============================================================================
# CLEANUP
# ==============================================================================

rm(panel_with_ages, panel_with_ppg, results, ki)
gc(verbose = FALSE)

cat("================================================================\n")
cat("All examples complete.\n")
cat("Next step: run examples/assumptions_season2_week9.R\n")
cat("================================================================\n\n")
