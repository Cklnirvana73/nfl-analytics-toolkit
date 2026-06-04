# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 13
# Usage Examples: Final Prospect Scoring and Signal Tier CSV
# File: examples/example_season2_week13_scoring.R
#
# PURPOSE
#   Sources R/28 and runs the full scoring pipeline. Produces three output
#   files in data/season2_cache/:
#     s2_week13_final_prospect_scores.csv   -- 848-row prospect inspection CSV
#     s2_week13_tier_reference.csv          -- empirical tier boundary reference
#     s2_week13_metric_correlations.csv     -- per-position Pearson correlation matrix
#
# PREREQUISITES
#   R/27_translation_model_v3.R must have been run first and its RDS artifacts
#   present in data/season2_cache/. The Databricks marker CSV must also be
#   present at data/season2_cache/s2_week13_full_marker_scores.csv.
#
# EXECUTION ORDER (mandatory)
#   1. source(here::here("R", "27_translation_model_v3.R"))  -- if not already run
#   2. source(here::here("examples", "example_season2_week13_scoring.R"))
#
# SCHEMA TAG: s2_w13_v3
# ==============================================================================


# ==============================================================================
# LIBRARIES
# ==============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(glue)
library(here)


# ==============================================================================
# SOURCE GUARD: R/28
# ==============================================================================

r28_path <- here::here("R", "28_final_prospect_scores.R")

if (!file.exists(r28_path)) {
  stop(glue(
    "R/28 not found at: {r28_path}\n",
    "Place 28_final_prospect_scores.R in the R/ directory before running."
  ), call. = FALSE)
}

source(r28_path)


# ==============================================================================
# SECTION 1: RUN THE FULL SCORING PIPELINE
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("SECTION 1: Run full scoring pipeline\n")
cat(strrep("=", 70), "\n\n", sep = "")

# run_week13_scoring() is defined in R/28 and called at source time.
# It produces all three CSVs and prints a console summary.
# If you need to re-run without re-sourcing R/28, call it directly:

# results <- run_week13_scoring()

cat("Pipeline complete. Loading outputs for inspection ...\n\n")


# ==============================================================================
# SECTION 2: LOAD AND INSPECT OUTPUTS
# ==============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("SECTION 2: Load outputs\n")
cat(strrep("=", 70), "\n\n", sep = "")

PATH_SCORES   <- here::here("data", "season2_cache", "s2_week13_final_prospect_scores.csv")
PATH_TIER_REF <- here::here("data", "season2_cache", "s2_week13_tier_reference.csv")
PATH_CORR     <- here::here("data", "season2_cache", "s2_week13_metric_correlations.csv")

# Verify all three outputs were produced
missing_outputs <- c(
  scores   = PATH_SCORES,
  tier_ref = PATH_TIER_REF,
  corr     = PATH_CORR
)[!file.exists(c(PATH_SCORES, PATH_TIER_REF, PATH_CORR))]

if (length(missing_outputs) > 0L) {
  stop(glue(
    "Expected output files not found:\n",
    paste(names(missing_outputs), missing_outputs, sep = " -> ", collapse = "\n")
  ), call. = FALSE)
}

scores   <- readr::read_csv(PATH_SCORES,   show_col_types = FALSE)
tier_ref <- readr::read_csv(PATH_TIER_REF, show_col_types = FALSE)
corr     <- readr::read_csv(PATH_CORR,     show_col_types = FALSE)

cat(glue("Prospect scores:     {nrow(scores)} rows x {ncol(scores)} columns\n"))
cat(glue("Tier reference:      {nrow(tier_ref)} rows\n"))
cat(glue("Correlation matrix:  {nrow(corr)} rows\n\n"))

# Row count by position
cat("Row count by position:\n")
scores |>
  dplyr::count(position, name = "n_players") |>
  dplyr::arrange(position) |>
  print()

cat("\n")
rm(missing_outputs)
gc(verbose = FALSE)


# ==============================================================================
# SECTION 3: SCORE DISTRIBUTION BY POSITION
# ==============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("SECTION 3: Score distribution by position\n")
cat(strrep("=", 70), "\n\n", sep = "")

score_summary <- scores |>
  dplyr::group_by(position) |>
  dplyr::summarise(
    n              = dplyr::n(),
    score_mean     = round(mean(score_final,   na.rm = TRUE), 1),
    score_median   = round(median(score_final, na.rm = TRUE), 1),
    score_sd       = round(sd(score_final,     na.rm = TRUE), 1),
    score_min      = round(min(score_final,    na.rm = TRUE), 1),
    score_max      = round(max(score_final,    na.rm = TRUE), 1),
    enrich_pct_med = round(median(enriched_improvement_pct, na.rm = TRUE), 1),
    .groups        = "drop"
  )

print(score_summary)

# KEY INSIGHT: median enriched_improvement_pct shows how much draft capital
# shifted scores beyond pure college production signal
cat(glue(
  "\nKEY INSIGHT -- Median enriched_improvement_pct by position:\n",
  "  QB: {score_summary$enrich_pct_med[score_summary$position == 'QB']}%\n",
  "  RB: {score_summary$enrich_pct_med[score_summary$position == 'RB']}%\n",
  "  WR: {score_summary$enrich_pct_med[score_summary$position == 'WR']}%\n",
  "  TE: {score_summary$enrich_pct_med[score_summary$position == 'TE']}%\n"
))

cat("\n")
rm(score_summary)
gc(verbose = FALSE)


# ==============================================================================
# SECTION 4: TOP 10 PROSPECTS PER POSITION
# ==============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("SECTION 4: Top 10 prospects per position (score_final)\n")
cat(strrep("=", 70), "\n\n", sep = "")

for (pos in c("QB", "RB", "WR", "TE")) {
  cat(glue("--- {pos} ---\n"))

  top10 <- scores |>
    dplyr::filter(position == pos) |>
    dplyr::slice_max(score_final, n = 10L) |>
    dplyr::select(
      player_name, draft_year, score_final,
      enriched_improvement_pct, n_hit_markers, n_bust_markers
    ) |>
    dplyr::mutate(
      score_final              = round(score_final, 1),
      enriched_improvement_pct = round(enriched_improvement_pct, 1)
    )

  print(top10)
  cat("\n")
}

gc(verbose = FALSE)


# ==============================================================================
# SECTION 5: INSPECT TIER REFERENCE -- WHAT DID THE DATA PRODUCE?
# ==============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("SECTION 5: Tier reference -- empirical thresholds by position\n")
cat(strrep("=", 70), "\n\n", sep = "")

# Metrics covered per position
cat("Metrics with empirical tiers per position:\n")
tier_ref |>
  dplyr::distinct(position, metric_name) |>
  dplyr::count(position, name = "n_metrics") |>
  print()

cat("\n")

# Show Elite Signal (Tier 1) thresholds for WR as illustration
cat("WR -- Elite Signal (Tier 1) thresholds (highest hit rate band):\n")
tier_ref |>
  dplyr::filter(position == "WR", tier == 1L) |>
  dplyr::select(metric_name, break_lower, break_upper, hit_rate, n_in_bin, stable) |>
  dplyr::mutate(
    hit_rate    = paste0(round(hit_rate * 100, 1), "%"),
    break_lower = round(break_lower, 3),
    break_upper = round(break_upper, 3)
  ) |>
  dplyr::arrange(dplyr::desc(as.numeric(gsub("%", "", hit_rate)))) |>
  print()

cat("\n")

# KEY INSIGHT: flag unstable tiers (fewer than MIN_BIN_SIZE players in bin)
n_unstable <- sum(!tier_ref$stable, na.rm = TRUE)
cat(glue(
  "KEY INSIGHT -- Unstable tier bins (< 5 players): {n_unstable}\n",
  "  These hit rate estimates carry wide uncertainty intervals.\n",
  "  Cross-reference n_in_bin before trusting extreme hit rates.\n\n"
))

gc(verbose = FALSE)


# ==============================================================================
# SECTION 6: FULL PROSPECT ROW -- EVERY METRIC AND ITS TIER
# ==============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("SECTION 6: Full signal profile for a single prospect\n")
cat(strrep("=", 70), "\n\n", sep = "")

# Show the top WR prospect's full signal profile
top_wr_name <- scores |>
  dplyr::filter(position == "WR") |>
  dplyr::slice_max(score_final, n = 1L) |>
  dplyr::pull(player_name)

cat(glue("Top WR by score_final: {top_wr_name}\n\n"))

top_wr_row <- scores |>
  dplyr::filter(player_name == top_wr_name, position == "WR")

# Pivot tier label columns to long for readable display
tier_label_cols <- names(top_wr_row)[endsWith(names(top_wr_row), "_tier_label")]
metric_base_names <- sub("_tier_label$", "", tier_label_cols)

signal_profile <- purrr::map_dfr(metric_base_names, function(m) {
  raw_val   <- if (m %in% names(top_wr_row)) round(as.numeric(top_wr_row[[m]]), 3) else NA_real_
  tier_val  <- if (paste0(m, "_tier") %in% names(top_wr_row)) top_wr_row[[paste0(m, "_tier")]] else NA_integer_
  tier_lbl  <- if (paste0(m, "_tier_label") %in% names(top_wr_row)) top_wr_row[[paste0(m, "_tier_label")]] else "NO_DATA"
  tibble::tibble(metric = m, value = raw_val, tier = tier_val, signal = tier_lbl)
}) |>
  dplyr::arrange(tier)

print(signal_profile, n = Inf)

cat(glue(
  "\nscore_final:              {round(top_wr_row$score_final, 1)}\n",
  "enriched_improvement_pct: {round(top_wr_row$enriched_improvement_pct, 1)}%\n",
  "n_hit_markers:            {top_wr_row$n_hit_markers}\n",
  "n_bust_markers:           {top_wr_row$n_bust_markers}\n",
  "sos_imputed:              {top_wr_row$sos_imputed}\n"
))

cat("\n")
rm(top_wr_name, top_wr_row, tier_label_cols, metric_base_names, signal_profile)
gc(verbose = FALSE)


# ==============================================================================
# SECTION 7: HIGH-COLLINEARITY METRIC PAIRS (from correlation matrix)
# ==============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("SECTION 7: High-collinearity metric pairs (|r| >= 0.70)\n")
cat(strrep("=", 70), "\n\n", sep = "")

high_corr <- corr |>
  dplyr::filter(high_collinearity == TRUE) |>
  # Keep only one direction (x < y alphabetically) to avoid duplicates
  dplyr::filter(metric_x < metric_y) |>
  dplyr::arrange(position, dplyr::desc(abs(pearson_r))) |>
  dplyr::select(position, metric_x, metric_y, pearson_r)

cat(glue(
  "Correlated pairs per position (|r| >= 0.70):\n",
  "These pairs fire together and should not be treated as independent signals.\n\n"
))

for (pos in c("QB", "RB", "WR", "TE")) {
  pos_pairs <- dplyr::filter(high_corr, position == pos)
  cat(glue("  {pos}: {nrow(pos_pairs)} pairs\n"))
  if (nrow(pos_pairs) > 0L) {
    pos_pairs |>
      dplyr::mutate(pair = glue("{metric_x} + {metric_y} (r={pearson_r})")) |>
      dplyr::pull(pair) |>
      purrr::walk(~ cat("    -", .x, "\n"))
  }
}

cat("\n")

# KEY INSIGHT: how many total unique correlated pairs exist across all positions?
n_pairs <- nrow(high_corr)
cat(glue(
  "KEY INSIGHT -- Total high-collinearity pairs: {n_pairs}\n",
  "  Review these pairs before interpreting n_hit_markers counts.\n",
  "  A player with 6 hit markers where 4 are correlated pairs\n",
  "  has fewer independent confirmation signals than the count implies.\n\n"
))

rm(high_corr)
gc(verbose = FALSE)


# ==============================================================================
# SECTION 8: TRAINING HIT RATE CONFIRMATION
# ==============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("SECTION 8: Training player hit rate confirmation\n")
cat(strrep("=", 70), "\n\n", sep = "")

# Verify hit rates match expected thresholds (QB 12, RB 24, WR 36, TE 12)
# Training players are those with is_training_player == TRUE and is_hit not NA
hit_summary <- scores |>
  dplyr::filter(is_training_player == TRUE, !is.na(is_hit)) |>
  dplyr::group_by(position) |>
  dplyr::summarise(
    n_training    = dplyr::n(),
    n_hits        = sum(is_hit, na.rm = TRUE),
    hit_rate_pct  = round(mean(is_hit, na.rm = TRUE) * 100, 1),
    score_hit_med = round(median(score_final[is_hit  == TRUE],  na.rm = TRUE), 1),
    score_mis_med = round(median(score_final[is_hit  == FALSE], na.rm = TRUE), 1),
    .groups       = "drop"
  )

cat("Training player outcomes by position:\n")
print(hit_summary)

cat(glue(
  "\nKEY INSIGHT -- Model discrimination (hit vs miss median score_final):\n"
))
for (i in seq_len(nrow(hit_summary))) {
  row <- hit_summary[i, ]
  diff <- row$score_hit_med - row$score_mis_med
  cat(glue(
    "  {row$position}: hit median={row$score_hit_med}  miss median={row$score_mis_med}  ",
    "gap={round(diff, 1)} pts\n"
  ))
}

cat("\n")
rm(hit_summary)
gc(verbose = FALSE)


# ==============================================================================
# DONE
# ==============================================================================

cat(strrep("=", 70), "\n", sep = "")
cat("All sections complete.\n")
cat(glue("Output files confirmed in: {here::here('data', 'season2_cache')}\n"))
cat(strrep("=", 70), "\n\n", sep = "")
