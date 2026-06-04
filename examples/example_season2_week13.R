# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 13
# Example Script: College-to-NFL Translation Model v3
# File: examples/example_season2_week13.R
#
# Purpose: Run the full v3 translation pipeline, inspect results at each stage,
#          and verify all outputs before handing off to the Python/MLflow
#          notebook.
#
# What this script does:
#   1. Sources R/27_translation_model_v3.R (which sources R/24 via source guard,
#      which cascades to R/20, R/21, R/16)
#   2. Runs run_week13_pipeline() end-to-end
#   3. Inspects each new feature group in sequence:
#        - .load_combine_features()           -- athleticism join + NA rates
#        - .load_recruiting_features()        -- composite match rate + cache
#        - .compute_production_slopes()       -- slope distribution by position
#        - .compute_breakout_age()            -- breakout coverage + medians
#        - build_translation_features_v3()   -- feature counts per position
#   4. Compares v3 LOCO RMSE vs v1 R/24 baseline
#   5. Inspects the CSV export that feeds the Python notebook
#   6. Prints KEY INSIGHTS computed from live results
#
# Runtime note:
#   FIRST RUN: recruiting API pull adds 10-20 minutes (56 API calls).
#   All subsequent runs read from the recruiting cache (~2 seconds).
#   Full pipeline including panel loads: 30-60 minutes first run,
#   5-15 minutes on re-runs once R/21 and R/16 caches are populated.
#
#   To force a recruiting re-pull, delete the cache first:
#     file.remove(here::here(
#       "data", "season2_cfb_cache", "s2_week13_recruiting.rds"
#     ))
#
# Prerequisites:
#   data/season2_cache/          -- populated by prior weeks
#   data/season2_cfb_cache/      -- populated by R/20, R/21, R/22
#   data/season2_cfb_cache/s2_week8_cfb_sos_panel.rds
#   CFBD_API_KEY set in .Renviron (for recruiting pull; cache used after)
#
# Outputs written to data/season2_cache/:
#   s2_week13_crosswalk.rds
#   s2_week13_feature_matrix.rds
#   s2_week13_models.rds
#   s2_week13_performance.rds
#   s2_week13_predictions.rds
#   s2_week13_feature_matrix.csv   <-- Python/MLflow notebook input
#
# Output written to data/season2_cfb_cache/:
#   s2_week13_recruiting.rds       <-- recruiting cache (persists across runs)
#
# ==============================================================================


# ==============================================================================
# SETUP
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(here)

source(here::here("R", "27_translation_model_v3.R"))

# Output directory for this week
OUTPUT_DIR <- here::here("data", "season2_cache")


# ==============================================================================
# SECTION 1: FULL PIPELINE RUN
# ==============================================================================
# run_week13_pipeline() is the single entry point. It:
#   - Loads CFB panel (R/21), NFL panel (R/16), SOS panel (R/22)
#   - Builds ID crosswalk via link_cfb_to_nfl() (R/24)
#   - Loads combine features and recruiting features (new in v3)
#   - Computes production slopes and breakout ages (new in v3)
#   - Builds v3 feature matrix with all six new feature groups
#   - Trains LOCO Elastic Net as R-side baseline
#   - Exports CSV for the Python/MLflow notebook
#   - Saves all RDS outputs

cat("\n", strrep("=", 60), "\n")
cat("SECTION 1: Running full Week 13 v3 pipeline\n")
cat(strrep("=", 60), "\n\n")

results <- run_week13_pipeline(
  output_dir = OUTPUT_DIR,
  verbose    = TRUE
)

cat("\nPipeline complete.\n")
cat("CSV for Python notebook:", results$csv_path, "\n")


# ==============================================================================
# SECTION 2: CROSSWALK INSPECTION
# ==============================================================================
# The crosswalk is inherited from R/24 link_cfb_to_nfl(). Inspect match
# quality before examining new features -- unmatched players are excluded
# from all feature groups.

cat("\n", strrep("=", 60), "\n")
cat("SECTION 2: Crosswalk match quality\n")
cat(strrep("=", 60), "\n\n")

crosswalk <- results$crosswalk

cat("Crosswalk rows:", format(nrow(crosswalk), big.mark = ","), "\n\n")

match_summary <- crosswalk %>%
  dplyr::count(draft_position, match_method) %>%
  dplyr::arrange(draft_position, match_method)

cat("Match method by position:\n")
print(as.data.frame(match_summary))

unmatched_n <- sum(crosswalk$match_method == "unmatched", na.rm = TRUE)
cat(glue(
  "\nUnmatched players: {unmatched_n} ",
  "({round(100 * unmatched_n / nrow(crosswalk), 1)}% of crosswalk)\n"
))
cat("Note: unmatched players are excluded from all feature groups.\n")


# ==============================================================================
# SECTION 3: COMBINE FEATURES INSPECTION
# ==============================================================================
# Inspect NA rates for each athleticism metric by position and draft year.
# The diagnostic output from run_week13_pipeline() summarizes these, but
# this section gives a more granular view for verification.

cat("\n", strrep("=", 60), "\n")
cat("SECTION 3: Combine feature coverage\n")
cat(strrep("=", 60), "\n\n")

# Load combine for direct inspection
combine_raw <- nflreadr::load_combine()

combine_check <- combine_raw %>%
  dplyr::filter(
    season >= min(TRAINING_DRAFT_CLASSES),
    season <= CUTOFF_YEAR,
    pos %in% c("QB", "RB", "WR", "TE")
  ) %>%
  dplyr::group_by(pos) %>%
  dplyr::summarise(
    n            = dplyr::n(),
    pct_ht_wt    = round(100 * mean(!is.na(ht) & !is.na(wt)), 1),
    pct_forty    = round(100 * mean(!is.na(forty)), 1),
    pct_vertical = round(100 * mean(!is.na(vertical)), 1),
    pct_broad    = round(100 * mean(!is.na(broad_jump)), 1),
    .groups      = "drop"
  )

cat("Combine coverage across training classes",
    glue("({min(TRAINING_DRAFT_CLASSES)}-{CUTOFF_YEAR}):\n"))
print(as.data.frame(combine_check))

cat("\nNote: cone excluded (near-zero coverage in 2022-2023).\n")
cat("Note: vertical/broad_jump excluded for QB (no established signal).\n")


# ==============================================================================
# SECTION 4: RECRUITING FEATURES INSPECTION
# ==============================================================================
# Inspect recruiting match rate and rating distribution by position.
# Cache path is printed so you can verify the RDS was written correctly.

cat("\n", strrep("=", 60), "\n")
cat("SECTION 4: Recruiting composite features\n")
cat(strrep("=", 60), "\n\n")

recruiting_cache <- here::here(
  "data", "season2_cfb_cache", "s2_week13_recruiting.rds"
)

if (file.exists(recruiting_cache)) {
  cat("Recruiting cache found:", recruiting_cache, "\n\n")
  rec_features <- readRDS(recruiting_cache)
  cat("Recruiting cache rows:", format(nrow(rec_features), big.mark = ","), "\n\n")

  rec_summary <- rec_features %>%
    dplyr::summarise(
      n_total    = dplyr::n(),
      n_matched  = sum(!is.na(recruiting_rating)),
      match_pct  = round(100 * n_matched / n_total, 1),
      mean_stars = round(mean(recruiting_stars, na.rm = TRUE), 2),
      mean_rating = round(mean(recruiting_rating, na.rm = TRUE), 4),
      pct_5star  = round(
        100 * mean(recruiting_stars == 5L, na.rm = TRUE), 1
      ),
      pct_4star  = round(
        100 * mean(recruiting_stars == 4L, na.rm = TRUE), 1
      ),
      pct_3star  = round(
        100 * mean(recruiting_stars == 3L, na.rm = TRUE), 1
      )
    )

  cat("Recruiting match summary:\n")
  print(as.data.frame(rec_summary))

  cat("\nRating distribution (247Sports composite, 0-1 scale):\n")
  rating_dist <- quantile(
    rec_features$recruiting_rating,
    probs  = c(0.10, 0.25, 0.50, 0.75, 0.90),
    na.rm  = TRUE
  )
  print(round(rating_dist, 4))

} else {
  cat("WARNING: Recruiting cache not found at:", recruiting_cache, "\n")
  cat("Run run_week13_pipeline() to populate the cache.\n")
}


# ==============================================================================
# SECTION 5: PRODUCTION SLOPES INSPECTION
# ==============================================================================
# Inspect slope distributions to confirm the signal is non-trivial.
# A flat distribution centered on zero suggests no developmental signal;
# positive skew (especially for WR rec_epa_slope) suggests improving prospects
# translate better.

cat("\n", strrep("=", 60), "\n")
cat("SECTION 5: Production slope distributions\n")
cat(strrep("=", 60), "\n\n")

# Recompute slopes from cached panels for inspection
cfb_panel_inspect <- build_cfb_player_season_panel(
  seasons = CFB_DATA_FLOOR:(CUTOFF_YEAR - 1L),
  verbose = FALSE
)
crosswalk_inspect <- results$crosswalk

slopes_inspect <- .compute_production_slopes(
  cfb_panel_inspect, crosswalk_inspect, verbose = FALSE
)

cat("Production slope summary (OLS slope across college seasons):\n\n")

slope_summary <- slopes_inspect %>%
  dplyr::summarise(
    n_rec_slope   = sum(!is.na(rec_epa_slope)),
    mean_rec      = round(mean(rec_epa_slope,  na.rm = TRUE), 4),
    sd_rec        = round(sd(rec_epa_slope,    na.rm = TRUE), 4),
    n_rush_slope  = sum(!is.na(rush_epa_slope)),
    mean_rush     = round(mean(rush_epa_slope, na.rm = TRUE), 4),
    sd_rush       = round(sd(rush_epa_slope,   na.rm = TRUE), 4),
    n_pass_slope  = sum(!is.na(pass_epa_slope)),
    mean_pass     = round(mean(pass_epa_slope, na.rm = TRUE), 4),
    sd_pass       = round(sd(pass_epa_slope,   na.rm = TRUE), 4)
  )

print(as.data.frame(slope_summary))

cat(glue(
  "\nNote: {nrow(slopes_inspect) - max(slope_summary$n_rec_slope, ",
  "slope_summary$n_rush_slope, slope_summary$n_pass_slope)} ",
  "players have only one CFB season; slope is NA (imputed at median).\n"
))


# ==============================================================================
# SECTION 6: BREAKOUT AGE INSPECTION
# ==============================================================================
# Inspect breakout age distributions by position. This is the most novel
# feature in v3 -- confirm the medians are football-plausible before trusting
# the model output.

cat("\n", strrep("=", 60), "\n")
cat("SECTION 6: Breakout age distributions\n")
cat(strrep("=", 60), "\n\n")

breakout_inspect <- .compute_breakout_age(
  cfb_panel_inspect,
  crosswalk_inspect,
  cutoff_year = CUTOFF_YEAR,
  verbose     = FALSE
)

breakout_by_pos <- crosswalk_inspect %>%
  dplyr::filter(!is.na(cfb_player_name)) %>%
  dplyr::select(cfb_player_name, draft_position) %>%
  dplyr::distinct() %>%
  dplyr::left_join(breakout_inspect, by = "cfb_player_name") %>%
  dplyr::filter(draft_position %in% c("QB", "RB", "WR", "TE"))

cat("Breakout age by position (first season above position efficiency median):\n\n")

breakout_summary <- breakout_by_pos %>%
  dplyr::group_by(draft_position) %>%
  dplyr::summarise(
    n_total       = dplyr::n(),
    n_breakout    = sum(!is.na(breakout_age)),
    pct_breakout  = round(100 * n_breakout / n_total, 1),
    mean_age      = round(mean(breakout_age, na.rm = TRUE), 2),
    median_age    = round(median(breakout_age, na.rm = TRUE), 2),
    min_age       = round(min(breakout_age, na.rm = TRUE), 1),
    max_age       = round(max(breakout_age, na.rm = TRUE), 1),
    .groups       = "drop"
  )

print(as.data.frame(breakout_summary))

cat("\nNote: Players who never exceeded position median return NA.\n")
cat("Expected: WR/RB breakout ages 19-21, QB 20-22 (late developers common).\n")


# ==============================================================================
# SECTION 7: V3 FEATURE MATRIX INSPECTION
# ==============================================================================
# Confirm feature counts per position and verify all six new feature groups
# are present in the model matrix.

cat("\n", strrep("=", 60), "\n")
cat("SECTION 7: v3 feature matrix summary\n")
cat(strrep("=", 60), "\n\n")

fm <- results$feature_matrix_v3

cat("Feature counts per position:\n\n")
for (pos in c("QB", "RB", "WR", "TE")) {
  n_base     <- length(fm$base_feature_cols[[pos]])
  n_enriched <- length(fm$enriched_feature_cols[[pos]])
  n_train    <- nrow(fm$training[[pos]])
  n_pred     <- nrow(fm$prediction[[pos]])
  cat(glue(
    "  {pos}: {n_base} base features, {n_enriched} enriched | ",
    "{n_train} training, {n_pred} prediction rows\n"
  ))
}

cat("\nBase feature columns (WR example):\n")
cat(paste(fm$base_feature_cols[["WR"]], collapse = ", "), "\n")

# Verify all six v3 feature groups are present for WR
v3_check_cols <- list(
  "combine (ht/wt/forty)"        = c("ht", "wt", "forty"),
  "combine (vertical/broad_jump)" = c("vertical", "broad_jump"),
  "production slope"              = "rec_epa_slope",
  "age x competition"             = "sos_x_age",
  "recruiting composite"          = c("recruiting_rating", "recruiting_stars"),
  "receiving role proxy"          = "rec_role_share",
  "breakout age"                  = c("breakout_age", "seasons_since_breakout")
)

wr_cols <- fm$base_feature_cols[["WR"]]
cat("\nv3 feature group verification (WR):\n")
for (group_name in names(v3_check_cols)) {
  present <- all(v3_check_cols[[group_name]] %in% wr_cols)
  status  <- if (present) "PRESENT" else "MISSING"
  cat(glue("  {group_name}: {status}\n"))
}


# ==============================================================================
# SECTION 8: V3 vs V1 PERFORMANCE COMPARISON
# ==============================================================================
# The core question: do the six new features improve holdout RMSE?
# This section computes the delta and prints a position-by-position comparison.
# The Python/MLflow notebook will extend this with XGBoost comparisons.

cat("\n", strrep("=", 60), "\n")
cat("SECTION 8: v3 vs v1 LOCO performance comparison\n")
cat(strrep("=", 60), "\n\n")

perf_v3 <- results$performance

cat("v3 Elastic Net LOCO performance:\n\n")
perf_display <- perf_v3 %>%
  dplyr::select(draft_position, model_variant, n_players, rmse, mae, r_squared) %>%
  dplyr::arrange(draft_position, model_variant)
print(as.data.frame(perf_display))

# Load v1 performance for comparison if it exists
v1_perf_path <- here::here("data", "season2_cache", "s2_week10_performance.rds")
if (file.exists(v1_perf_path)) {
  perf_v1 <- readRDS(v1_perf_path)

  cat("\nv1 vs v3 RMSE delta (negative = v3 improvement):\n\n")

  delta_tbl <- perf_v3 %>%
    dplyr::select(draft_position, model_variant, rmse_v3 = rmse) %>%
    dplyr::left_join(
      perf_v1 %>%
        dplyr::select(draft_position, model_variant, rmse_v1 = rmse),
      by = c("draft_position", "model_variant")
    ) %>%
    dplyr::mutate(
      rmse_delta  = round(rmse_v3 - rmse_v1, 4),
      improved    = rmse_delta < 0
    ) %>%
    dplyr::arrange(draft_position, model_variant)

  print(as.data.frame(delta_tbl))

  n_improved <- sum(delta_tbl$improved, na.rm = TRUE)
  cat(glue(
    "\n{n_improved} / {nrow(delta_tbl)} position-variant combinations ",
    "show v3 RMSE improvement over v1.\n"
  ))
} else {
  cat("\nv1 performance file not found at:", v1_perf_path, "\n")
  cat("Run R/24 run_week10_pipeline() first to generate the v1 baseline.\n")
}


# ==============================================================================
# SECTION 9: TRANSLATION GAPS INSPECTION
# ==============================================================================
# Which features have non-zero coefficients in the LOCO model?
# This is the R-side version of the SHAP analysis that the Python notebook
# will extend. Key question: do any of the six new features show up as
# important predictors after regularization?

cat("\n", strrep("=", 60), "\n")
cat("SECTION 9: Translation gaps -- v3 feature importance (Elastic Net)\n")
cat(strrep("=", 60), "\n\n")

gaps <- results$translation_gaps

for (pos in c("QB", "RB", "WR", "TE")) {
  cat(glue("\n{pos} -- top features by coefficient magnitude (base model):\n"))
  pos_gaps <- gaps[[pos]]
  if (is.null(pos_gaps) || nrow(pos_gaps) == 0L) {
    cat("  No gap data available.\n")
    next
  }
  top_features <- pos_gaps %>%
    dplyr::filter(!is.na(base_coef), base_coef != 0) %>%
    dplyr::arrange(desc(abs(base_coef))) %>%
    dplyr::slice_head(n = 10L) %>%
    dplyr::select(feature, base_coef)
  print(as.data.frame(top_features))
}


# ==============================================================================
# SECTION 10: CSV EXPORT VERIFICATION
# ==============================================================================
# Verify the Python notebook input CSV is well-formed before moving to
# Databricks. Check row count, column count, NA rates, and column names.

cat("\n", strrep("=", 60), "\n")
cat("SECTION 10: Python notebook CSV verification\n")
cat(strrep("=", 60), "\n\n")

csv_path <- results$csv_path

if (file.exists(csv_path)) {
  csv_data <- utils::read.csv(csv_path, stringsAsFactors = FALSE)

  cat("CSV path:", csv_path, "\n")
  cat("Rows:", format(nrow(csv_data), big.mark = ","), "\n")
  cat("Columns:", ncol(csv_data), "\n\n")

  cat("Column names:\n")
  cat(paste(names(csv_data), collapse = ", "), "\n\n")

  # NA rate per column
  na_rates <- purrr::map_dbl(names(csv_data), function(col) {
    round(100 * mean(is.na(csv_data[[col]]) | csv_data[[col]] == ""), 1)
  }) %>%
    stats::setNames(names(csv_data))

  high_na <- na_rates[na_rates > 20]
  if (length(high_na) > 0L) {
    cat("Columns with >20% NA (will be imputed in Python notebook):\n")
    print(sort(high_na, decreasing = TRUE))
  } else {
    cat("No columns with >20% NA rate.\n")
  }

  cat("\nPosition distribution in CSV:\n")
  print(table(csv_data$position))

  cat("\nOutcome variable (ppr_per_game_y13) summary:\n")
  print(summary(csv_data$ppr_per_game_y13))

} else {
  cat("WARNING: CSV not found at:", csv_path, "\n")
  cat("Run run_week13_pipeline() to generate the CSV.\n")
}


# ==============================================================================
# KEY INSIGHTS
# ==============================================================================
# All values computed from live results above -- never hardcoded.

cat("\n", strrep("=", 70), "\n")
cat("KEY INSIGHTS -- computed from live run\n")
cat(strrep("=", 70), "\n\n")

# Training set size
total_training <- sum(purrr::map_int(
  c("QB", "RB", "WR", "TE"),
  function(pos) nrow(results$feature_matrix_v3$training[[pos]])
))
cat(glue(
  "Training players: {format(total_training, big.mark = ',')} ",
  "(draft classes {min(TRAINING_DRAFT_CLASSES)}-{CUTOFF_YEAR})\n"
))

# Feature count
total_features_wr <- length(results$feature_matrix_v3$base_feature_cols[["WR"]])
cat(glue("WR base feature count (v3): {total_features_wr}\n"))

# Recruiting match rate
if (file.exists(recruiting_cache)) {
  rec_data <- readRDS(recruiting_cache)
  rec_match_pct <- round(
    100 * mean(!is.na(rec_data$recruiting_rating)), 1
  )
  cat(glue("Recruiting match rate: {rec_match_pct}%\n"))
}

# Best performing position in v3 (lowest RMSE, base model)
best_pos_row <- perf_v3 %>%
  dplyr::filter(model_variant == "base") %>%
  dplyr::arrange(rmse) %>%
  dplyr::slice_head(n = 1L)

cat(glue(
  "Best v3 base RMSE: {round(best_pos_row$rmse, 3)} ",
  "({best_pos_row$draft_position}, {best_pos_row$n_players} players)\n"
))

# Worst performing position
worst_pos_row <- perf_v3 %>%
  dplyr::filter(model_variant == "base") %>%
  dplyr::arrange(desc(rmse)) %>%
  dplyr::slice_head(n = 1L)

cat(glue(
  "Highest v3 base RMSE: {round(worst_pos_row$rmse, 3)} ",
  "({worst_pos_row$draft_position}) -- ",
  "consistent with known landing-spot variance at this position\n"
))

# v3 vs v1 improvement summary (if v1 available)
if (file.exists(v1_perf_path)) {
  overall_improvement <- mean(delta_tbl$rmse_delta < 0, na.rm = TRUE)
  avg_delta <- round(mean(delta_tbl$rmse_delta, na.rm = TRUE), 4)
  cat(glue(
    "v3 improvement rate: {round(100 * overall_improvement, 0)}% of ",
    "position-variant combinations | avg RMSE delta: {avg_delta}\n"
  ))
}

cat(glue("\nCSV for Python notebook: {results$csv_path}\n"))
cat(glue(
  "Next step: Upload CSV to Databricks and run ",
  "notebooks/week13_translation_mlflow.ipynb\n"
))

cat("\n", strrep("=", 70), "\n")
cat("End of example_season2_week13.R\n")
cat(strrep("=", 70), "\n")
