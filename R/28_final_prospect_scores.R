# ==============================================================================
# 28_final_prospect_scores.R
# Season 2, Week 14 -- Final Prospect Scoring and Signal Tier CSV
# ==============================================================================
#
# PURPOSE
#   Scores all 848 CFB prospects (621 training + 227 prediction rows) using
#   the fitted Enriched Elastic Net models from R/27 and the v1 model from
#   R/24. Derives empirical 5-tier signal thresholds from training player
#   NFL outcomes. Outputs a flat inspection CSV.
#
# OUTCOME DEFINITION (PPR per game averaged over NFL Years 1-3):
#   QB: top 12 | RB: top 24 | WR: top 36 | TE: top 12
#
# TIER LOGIC:
#   For each position and metric, training players are partitioned into 5
#   quantile bins. Each bin's empirical hit rate is computed from actual NFL
#   outcomes. Bins re-ranked so Tier 1 = highest hit rate. No assumed thresholds.
#
# INPUTS:
#   data/season2_cache/s2_week13_models.rds
#   data/season2_cache/s2_week13_predictions.rds
#   data/season2_cache/s2_week13_feature_matrix_full.csv   (848 rows, R/27)
#   data/season2_cache/s2_week10_models.rds                (v1, auto-rebuilt if missing)
#
# OUTPUTS:
#   data/season2_cache/s2_week14_final_prospect_scores.csv
#   data/season2_cache/s2_week14_tier_reference.csv
#   data/season2_cache/s2_week14_metric_correlations.csv
#
# SOURCE DEPENDENCY:
#   All inputs produced by R/27 run_week13_pipeline() and R/24
#   run_week10_pipeline(). Auto-rebuilt if missing.
#
# SCHEMA TAG: s2_w14_v1
# ==============================================================================


# ==============================================================================
# LIBRARIES
# ==============================================================================

library(tidymodels)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(here)
library(glue)
library(nflreadr)

# Null-coalescing operator for model structure detection
`%||%` <- function(a, b) if (!is.null(a)) a else b


# ==============================================================================
# CONFIGURATION
# ==============================================================================

CUTOFF_YEAR    <- 2023L   # Matches R/27 run -- derive from predictions RDS at runtime
TRAINING_FLOOR <- 2015L
N_TIERS        <- 5L
MIN_BIN_SIZE   <- 5L

# Enriched model adds these two columns on top of base features (matches R/24)
ENRICHED_ONLY_COLS <- c("draft_round", "draft_pick")
MIN_NFL_GAMES        <- 4L      # Min qualifying games per NFL season (matches R/24)

# Hit thresholds per position (updated from user session 2026-05-18)
HIT_THRESHOLDS <- list(QB = 12L, RB = 24L, WR = 36L, TE = 12L)

POSITIONS <- c("QB", "RB", "WR", "TE")

# PPR scoring constants -- match R/24 exactly
PPR_PASS_YD   <-  0.04
PPR_PASS_TD   <-  4.00
PPR_INT       <- -2.00
PPR_RUSH_YD   <-  0.10
PPR_RUSH_TD   <-  6.00
PPR_RECEPTION <-  1.00
PPR_REC_YD    <-  0.10
PPR_REC_TD    <-  6.00

# Tier labels (Tier 1 = highest empirical hit rate)
TIER_LABELS <- c(
  "1" = "Elite Signal",
  "2" = "Strong Signal",
  "3" = "Moderate Signal",
  "4" = "Weak Signal",
  "5" = "No Signal"
)

# Columns that are NOT metrics (excluded from tier derivation and correlation)
NON_METRIC_COLS <- c(
  "player_name", "pfr_player_name", "cfb_player_name",
  "position", "draft_position", "draft_year", "draft_round", "draft_pick",
  "draft_age", "nfl_gsis_id", "gsis_id", "cfb_primary_team",
  "draft_class_type", "match_method", "match_confidence"
)

# Paths -- v3 inputs (from R/27)
PATH_MODELS    <- here::here("data", "season2_cache", "s2_week13_models.rds")
PATH_PREDS     <- here::here("data", "season2_cache", "s2_week13_predictions.rds")
PATH_FEATURES  <- here::here("data", "season2_cache", "s2_week13_feature_matrix_full.csv")

# Paths -- v1 model (from R/24, Week 10 -- better RMSE than v3)
# Predictions are generated live against all 848 rows so 2026 rookies get scored.
PATH_MODELS_V1 <- here::here("data", "season2_cache", "s2_week10_models.rds")

# Paths -- outputs
PATH_OUT_CSV   <- here::here("data", "season2_cache", "s2_week14_final_prospect_scores.csv")
PATH_TIER_REF  <- here::here("data", "season2_cache", "s2_week14_tier_reference.csv")
PATH_CORR_CSV  <- here::here("data", "season2_cache", "s2_week14_metric_correlations.csv")


# ==============================================================================
# NSE DECLARATIONS
# ==============================================================================

utils::globalVariables(c(
  "player_id", "player_name", "position", "season", "season_type",
  "week", "passing_yards", "passing_tds", "interceptions",
  "rushing_yards", "rushing_tds", "receptions", "receiving_yards",
  "receiving_tds", "games_played", "ppr_season", "ppr_per_game",
  "season_rank", "draft_year", "draft_round", "draft_pick", "draft_age",
  "nfl_gsis_id", "gsis_id", "is_hit", "ppr_per_game_y13",
  "qualifying_seasons", "pred_base", "pred_enriched",
  "score_base", "score_enriched", "score_final",
  "enriched_improvement_pct", "sos_imputed", "is_training_player",
  "hit_rate", "n_in_bin", "break_lower", "break_upper",
  "tier", "tier_label", "stable", "metric_name", ".bin",
  "n_hits", "ever_top_n", "score_v1"
))


# ==============================================================================
# HELPERS
# ==============================================================================

# ------------------------------------------------------------------------------
# .validate_inputs
#
# Checks all input files exist. Auto-rebuilds from R/27 or R/24 if missing.
# s2_week13_feature_matrix_full.csv is produced by R/27 run_week13_pipeline()
# combined with the prediction row export step. If missing, R/27 is sourced
# and run, then the user is prompted to run the export step manually.
# ------------------------------------------------------------------------------
.validate_inputs <- function() {

  r27_path <- here::here("R", "27_translation_model_v3.R")
  r24_path <- here::here("R", "24_translation_model.R")

  # --- v3 RDS files (R/27 can rebuild these) ---
  v3_rds_paths <- c(models = PATH_MODELS, preds = PATH_PREDS)
  v3_rds_missing <- v3_rds_paths[!file.exists(v3_rds_paths)]

  if (length(v3_rds_missing) > 0L) {
    message(glue::glue(
      "Missing v3 RDS files: {paste(names(v3_rds_missing), collapse = ', ')}. ",
      "Sourcing R/27 ..."
    ))
    if (!file.exists(r27_path)) {
      stop(glue::glue("R/27 not found at: {r27_path}"), call. = FALSE)
    }
    if (!exists("run_week13_pipeline", mode = "function")) source(r27_path)
    run_week13_pipeline()
    still_missing <- v3_rds_paths[!file.exists(v3_rds_paths)]
    if (length(still_missing) > 0L) {
      stop(glue::glue(
        "RDS files still not found after R/27 run:\n",
        paste(still_missing, collapse = "\n")
      ), call. = FALSE)
    }
    message("R/27 rebuild complete.")
  }

  # --- Feature matrix full CSV (621 training + 227 prediction rows) ---
  if (!file.exists(PATH_FEATURES)) {
    stop(glue::glue(
      "\nFeature matrix not found:\n  {PATH_FEATURES}\n\n",
      "Run the following in R after sourcing R/27 to create it:\n\n",
      "  library(dplyr)\n",
      "  training_rows <- read.csv(here::here('data/season2_cache/s2_week13_feature_matrix.csv'))\n",
      "  # pred_rows_extended must be in environment from run_week13_pipeline()\n",
      "  pred_export <- pred_rows_extended %>% mutate(position = draft_position)\n",
      "  for (col in setdiff(names(training_rows), names(pred_export))) pred_export[[col]] <- NA\n",
      "  pred_export <- pred_export[, names(training_rows)]\n",
      "  combined <- bind_rows(\n",
      "    training_rows %>% mutate(draft_class_type = 'training'),\n",
      "    pred_export   %>% mutate(draft_class_type = 'prediction')\n",
      "  )\n",
      "  write.csv(combined, here::here('data/season2_cache/s2_week13_feature_matrix_full.csv'), row.names = FALSE)\n"
    ), call. = FALSE)
  }

  # --- v1 model file (R/24) ---
  if (!file.exists(PATH_MODELS_V1)) {
    message("Missing v1 models. Sourcing R/24 ...")
    if (!file.exists(r24_path)) {
      stop(glue::glue("R/24 not found at: {r24_path}"), call. = FALSE)
    }
    if (!exists("run_week10_pipeline", mode = "function")) source(r24_path)
    run_week10_pipeline()
    if (!file.exists(PATH_MODELS_V1)) {
      stop(glue::glue("{PATH_MODELS_V1} still not found after R/24 run."), call. = FALSE)
    }
    message("R/24 rebuild complete.")
  }

  message("Input validation passed: all source files found.")

  # Guard: confirm the feature matrix contains every column the loaded models
  # expect. The full CSV (s2_week13_feature_matrix_full.csv) is produced by a
  # manual export step after run_week13_pipeline() and can become stale when R/27
  # adds new features. A stale CSV causes a cryptic "undefined columns selected"
  # error inside .score_all_prospects(); this check surfaces the problem with an
  # actionable message before any scoring begins.
  feature_df <- tryCatch(
    readr::read_csv(PATH_FEATURES, show_col_types = FALSE, n_max = 1L),
    error = function(e) NULL
  )
  models_check <- tryCatch(readRDS(PATH_MODELS), error = function(e) NULL)

  if (!is.null(feature_df) && !is.null(models_check)) {
    csv_cols <- names(feature_df)
    missing_by_pos <- purrr::map(POSITIONS, function(pos) {
      base_cols <- models_check$base_feature_cols[[pos]]
      enr_cols  <- c(base_cols, ENRICHED_ONLY_COLS)
      setdiff(enr_cols, csv_cols)
    })
    names(missing_by_pos) <- POSITIONS
    any_missing <- any(purrr::map_lgl(missing_by_pos, ~ length(.x) > 0L))

    if (any_missing) {
      missing_report <- purrr::imap_chr(missing_by_pos, function(cols, pos) {
        if (length(cols) == 0L) return(NULL)
        glue::glue("  {pos}: {paste(cols, collapse = ', ')}")
      })
      missing_report <- missing_report[!purrr::map_lgl(missing_report, is.null)]
      stop(glue::glue(
        "\nFeature matrix is STALE -- the following columns are expected by the ",
        "models but missing from:\n  {PATH_FEATURES}\n\n",
        "Missing columns by position:\n",
        "{paste(missing_report, collapse = '\n')}\n\n",
        "Fix: delete the stale CSV and regenerate it with the extended pipeline.\n",
        "See the R/28 header for the export procedure."
      ), call. = FALSE)
    }
    message(glue::glue(
      "Feature matrix column check: all model columns present in CSV."
    ))
  }
}


# ------------------------------------------------------------------------------
# .impute_with_medians
# Apply stored training medians to NA values. Matches R/24 .impute_features().
# Columns missing from df are added as 0 (safe fallback matching R/24 behavior).
# ------------------------------------------------------------------------------
.impute_with_medians <- function(df, medians) {
  for (col in names(medians)) {
    if (!col %in% names(df)) {
      df[[col]] <- 0
    } else {
      na_idx <- is.na(df[[col]])
      if (any(na_idx)) {
        fill_val <- if (is.na(medians[[col]])) 0 else medians[[col]]
        df[[col]][na_idx] <- fill_val
      }
    }
  }
  df
}


# ------------------------------------------------------------------------------
# .score_all_prospects
#
# Scores all 848 rows using cv.glmnet models from s2_week13_models.rds.
# Model structure (from train_translation_model() in R/24/R/27):
#   models$models_base[[pos]]      -- cv.glmnet base model
#   models$models_enriched[[pos]]  -- cv.glmnet enriched model
#   models$base_feature_cols[[pos]]-- character vector of base feature names
#   models$impute_medians[[pos]]   -- named numeric vector of imputation medians
#
# Enriched feature set = base_feature_cols + ENRICHED_ONLY_COLS (draft_round, draft_pick).
# Scaling uses training player LOCO prediction bounds from the predictions RDS.
# ------------------------------------------------------------------------------
.score_all_prospects <- function(marker_df, models, training_preds) {

  stopifnot(
    "pred_base"      %in% names(training_preds),
    "pred_enriched"  %in% names(training_preds),
    "draft_position" %in% names(training_preds)
  )

  result_list <- vector("list", length(POSITIONS))
  names(result_list) <- POSITIONS

  for (pos in POSITIONS) {

    pos_data <- dplyr::filter(marker_df, position == pos)
    if (nrow(pos_data) == 0L) next

    mdl_base     <- models$models_base[[pos]]
    mdl_enriched <- models$models_enriched[[pos]]

    if (is.null(mdl_base) || is.null(mdl_enriched)) {
      stop(glue::glue(
        "Models not found for position {pos}.\n",
        "Expected models$models_base${pos} and models$models_enriched${pos}.\n",
        "Inspect str(readRDS(PATH_MODELS)) to verify structure."
      ), call. = FALSE)
    }

    base_cols <- models$base_feature_cols[[pos]]
    enr_cols  <- c(base_cols, ENRICHED_ONLY_COLS)
    medians   <- models$impute_medians[[pos]]

    # Impute and build matrices
    base_imp <- .impute_with_medians(as.data.frame(pos_data[, base_cols, drop = FALSE]), medians[base_cols])
    enr_imp  <- .impute_with_medians(as.data.frame(pos_data[, enr_cols,  drop = FALSE]), medians[enr_cols])

    X_base <- as.matrix(base_imp)
    X_enr  <- as.matrix(enr_imp)

    raw_base     <- as.vector(predict(mdl_base,     newx = X_base, s = "lambda.min"))
    raw_enriched <- as.vector(predict(mdl_enriched, newx = X_enr,  s = "lambda.min"))

    # Training bounds from LOCO predictions RDS (position-specific)
    train_pos <- dplyr::filter(training_preds, draft_position == pos)
    if (nrow(train_pos) == 0L) {
      stop(glue::glue("No training predictions found for {pos} in predictions RDS."))
    }

    scale_0_100 <- function(x, mn, mx) {
      if (abs(mx - mn) < 1e-10) return(rep(50, length(x)))
      pmax(0, pmin(100, (x - mn) / (mx - mn) * 100))
    }

    score_base_clamped     <- scale_0_100(raw_base,     min(train_pos$pred_base,     na.rm = TRUE), max(train_pos$pred_base,     na.rm = TRUE))
    score_enriched_clamped <- scale_0_100(raw_enriched, min(train_pos$pred_enriched, na.rm = TRUE), max(train_pos$pred_enriched, na.rm = TRUE))

    n_oob <- sum(scale_0_100(raw_enriched, min(train_pos$pred_enriched, na.rm = TRUE), max(train_pos$pred_enriched, na.rm = TRUE)) != score_enriched_clamped)
    if (n_oob > 0L) message(glue::glue("  [{pos}] {n_oob} prospect(s) outside training score range -- clamped."))

    pos_data <- pos_data |>
      dplyr::mutate(
        score_base             = score_base_clamped,
        score_enriched         = score_enriched_clamped,
        score_final            = score_enriched_clamped,
        enriched_improvement_pct = dplyr::if_else(
          score_base > 0,
          (score_enriched - score_base) / score_base * 100,
          NA_real_
        )
      )

    result_list[[pos]] <- pos_data
  }

  dplyr::bind_rows(result_list)
}


# ------------------------------------------------------------------------------
# .derive_empirical_tiers
#
# For a single metric column within a position's training players, partitions
# values into N_TIERS quantile bins, computes the empirical hit rate per bin,
# and re-ranks bins so Tier 1 = highest hit rate.
#
# Returns a list:
#   $tier_ref : tibble with bin, tier, tier_label, break_lower, break_upper,
#               hit_rate, n_in_bin, stable, metric_name
#   $breaks   : numeric vector of bin boundaries (for applying to new players)
#
# Returns NULL if insufficient complete cases.
# ------------------------------------------------------------------------------
.derive_empirical_tiers <- function(training_df, metric, hit_col = "is_hit") {

  df <- dplyr::filter(
    training_df,
    !is.na(.data[[metric]]),
    !is.na(.data[[hit_col]])
  )

  n <- nrow(df)

  if (n < N_TIERS * MIN_BIN_SIZE) {
    message(glue::glue(
      "    [SKIP] {metric}: {n} complete cases < minimum {N_TIERS * MIN_BIN_SIZE}."
    ))
    return(NULL)
  }

  # Quantile-based break points
  probs  <- seq(0, 1, length.out = N_TIERS + 1L)
  breaks <- quantile(df[[metric]], probs = probs, na.rm = TRUE)

  # Deduplicate (handles highly discrete metrics with tied quantiles)
  breaks <- unique(breaks)
  if (length(breaks) < 3L) {
    message(glue::glue(
      "    [SKIP] {metric}: too few unique quantile breaks after deduplication."
    ))
    return(NULL)
  }

  # Extend outer edges to -Inf / +Inf so all values fall in a bin
  breaks[1]              <- -Inf
  breaks[length(breaks)] <-  Inf

  # Assign each training player to a bin
  df <- dplyr::mutate(
    df,
    .bin = as.integer(
      cut(.data[[metric]], breaks = breaks, include.lowest = TRUE, labels = FALSE)
    )
  )

  # Compute hit rate per bin
  bin_stats <- df |>
    dplyr::group_by(.bin) |>
    dplyr::summarise(
      n_in_bin = dplyr::n(),
      n_hits   = sum(.data[[hit_col]], na.rm = TRUE),
      hit_rate = mean(.data[[hit_col]], na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      break_lower = breaks[.bin],
      break_upper = breaks[.bin + 1L]
    )

  # Assign tier: Tier 1 = highest hit rate, ties broken by higher metric value
  bin_stats <- bin_stats |>
    dplyr::arrange(dplyr::desc(hit_rate), dplyr::desc(break_lower)) |>
    dplyr::mutate(
      tier       = dplyr::row_number(),
      tier_label = TIER_LABELS[as.character(tier)],
      stable     = n_in_bin >= MIN_BIN_SIZE,
      metric_name = metric
    )

  list(
    tier_ref = bin_stats,
    breaks   = breaks
  )
}


# ------------------------------------------------------------------------------
# .apply_tiers_to_column
#
# Vectorized tier assignment. Given a numeric vector of player values and the
# tier derivation output from .derive_empirical_tiers(), returns a list with:
#   $tier       : integer vector (1-5, or NA for NA input values)
#   $tier_label : character vector (label or "NO_DATA" for NA input values)
#
# Out-of-training-range values are clamped to the nearest bin.
# ------------------------------------------------------------------------------
.apply_tiers_to_column <- function(values, tier_result) {

  n <- length(values)

  if (is.null(tier_result)) {
    return(list(
      tier       = rep(NA_integer_, n),
      tier_label = rep("NO_DATA", n)
    ))
  }

  breaks   <- tier_result$breaks
  tier_ref <- tier_result$tier_ref

  # Build bin -> tier lookup from tier_ref
  bin_to_tier <- tibble::tibble(
    .bin       = tier_ref$.bin,
    tier       = tier_ref$tier,
    tier_label = tier_ref$tier_label
  )

  min_bin <- min(tier_ref$.bin, na.rm = TRUE)
  max_bin <- max(tier_ref$.bin, na.rm = TRUE)

  # Assign bins (NA values get NA bin)
  bins <- as.integer(cut(values, breaks = breaks, include.lowest = TRUE, labels = FALSE))

  # Clamp out-of-range to nearest valid bin
  bins <- dplyr::if_else(!is.na(bins), pmax(min_bin, pmin(max_bin, bins)), NA_integer_)

  # Join bin to tier label
  result_df <- tibble::tibble(.bin = bins) |>
    dplyr::left_join(bin_to_tier, by = ".bin")

  # Override NA input values with "NO_DATA"
  result_df$tier_label <- dplyr::if_else(
    is.na(values),
    "NO_DATA",
    dplyr::coalesce(result_df$tier_label, "NO_DATA")
  )
  result_df$tier <- dplyr::if_else(
    is.na(values),
    NA_integer_,
    result_df$tier
  )

  list(
    tier       = result_df$tier,
    tier_label = result_df$tier_label
  )
}


# ==============================================================================
# MAIN PIPELINE
# ==============================================================================

run_week14_scoring <- function() {

  message("\n=== R/28: Final Prospect Scores -- Week 14 ===\n")

  # --------------------------------------------------------------------------
  # STEP 1: Validate inputs
  # --------------------------------------------------------------------------
  .validate_inputs()

  # --------------------------------------------------------------------------
  # STEP 2: Load inputs
  # --------------------------------------------------------------------------
  message("Loading model artifacts ...")
  models         <- readRDS(PATH_MODELS)
  training_preds <- readRDS(PATH_PREDS)
  models_v1      <- readRDS(PATH_MODELS_V1)
  marker_df      <- readr::read_csv(PATH_FEATURES, show_col_types = FALSE)

  message(glue::glue("  Feature matrix loaded: {nrow(marker_df)} rows, {ncol(marker_df)} columns."))

  # Required column assertions
  required_cols <- c("position", "draft_year")
  missing_req   <- required_cols[!required_cols %in% names(marker_df)]
  if (length(missing_req) > 0L) {
    # Try draft_position if position column missing
    if ("draft_position" %in% names(marker_df) && "position" %in% missing_req) {
      marker_df <- dplyr::rename(marker_df, position = draft_position)
      missing_req <- missing_req[missing_req != "position"]
    }
    if (length(missing_req) > 0L) {
      stop(glue::glue(
        "Feature matrix missing required columns: {paste(missing_req, collapse = ', ')}"
      ))
    }
  }

  # Validate positions
  unexpected_positions <- setdiff(unique(marker_df$position), POSITIONS)
  if (length(unexpected_positions) > 0L) {
    warning(glue::glue(
      "Unexpected position values in marker CSV: {paste(unexpected_positions, collapse = ', ')}. ",
      "These rows will be excluded from scoring."
    ))
    marker_df <- dplyr::filter(marker_df, position %in% POSITIONS)
  }

  # --------------------------------------------------------------------------
  # STEP 3: Pull outcomes from predictions RDS -- already computed by R/27
  # No need to reload R/16 or nflreadr. The predictions RDS (loco_predictions)
  # has nfl_gsis_id, draft_position, ppr_per_game_y13, is_hit for all training
  # players. Derive CUTOFF_YEAR from the data, not from a hardcoded constant.
  # --------------------------------------------------------------------------
  cutoff_yr <- max(training_preds$draft_year, na.rm = TRUE)
  message(glue::glue(
    "  Training outcomes from predictions RDS: {nrow(training_preds)} players, ",
    "{sum(training_preds$is_hit, na.rm = TRUE)} hits. ",
    "Cutoff year: {cutoff_yr}"
  ))
  training_preds |>
    dplyr::group_by(draft_position) |>
    dplyr::summarise(n = dplyr::n(), hits = sum(is_hit, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(hit_rate_pct = round(hits / n * 100, 1)) |>
    print()

  # --------------------------------------------------------------------------
  # STEP 4: Mark training vs prediction players
  # is_hit, ppr_per_game_y13, and qualifying_seasons are already in the
  # feature matrix (R/27 exported them). No join needed.
  # --------------------------------------------------------------------------
  marker_df <- marker_df |>
    dplyr::mutate(
      is_training_player = draft_class_type == "training",
      sos_imputed        = draft_class_type == "prediction"
    )

  n_training      <- sum(marker_df$is_training_player, na.rm = TRUE)
  n_outcome_match <- sum(marker_df$is_training_player & !is.na(marker_df$is_hit), na.rm = TRUE)
  message(glue::glue(
    "  Training players: {n_training} | with outcomes: {n_outcome_match}"
  ))
  if (n_outcome_match < n_training * 0.80) {
    warning(glue::glue(
      "Fewer than 80% of training players have is_hit values ({n_outcome_match}/{n_training}). ",
      "Check feature matrix export from R/27."
    ))
  }

  # --------------------------------------------------------------------------
  # STEP 5: Score all 848 prospects
  # --------------------------------------------------------------------------
  message("\nScoring all prospects with enriched Elastic Net models ...")
  scored_df <- .score_all_prospects(marker_df, models, training_preds)

  # CONTRACTS edge case 2: no NA in score_final
  stopifnot(
    "score_final contains NA values after scoring. Check model predictions." =
      sum(is.na(scored_df$score_final)) == 0L
  )
  message(glue::glue(
    "  Scores generated: {nrow(scored_df)} rows, score_final range [",
    "{round(min(scored_df$score_final), 1)}, ",
    "{round(max(scored_df$score_final), 1)}]."
  ))

  # --------------------------------------------------------------------------
  # STEP 5b: Score all 848 rows with v1 (Week 10) model -- live prediction
  # v1 model structure from s2_week10_models.rds (train_translation_model()):
  #   models_v1$models_enriched[[pos]] -- cv.glmnet object
  #   models_v1$base_feature_cols[[pos]] -- character vector
  #   models_v1$impute_medians[[pos]]   -- named numeric vector
  # Scaled 0-100 using v3 training bounds for apples-to-apples comparison.
  # --------------------------------------------------------------------------
  message("\nScoring all prospects with v1 (Week 10) model ...")

  scored_df[["score_v1"]] <- NA_real_

  for (pos in POSITIONS) {
    pos_idx  <- which(scored_df$position == pos)
    if (length(pos_idx) == 0L) next

    pos_data <- scored_df[pos_idx, , drop = FALSE]

    mdl_v1     <- models_v1$models_enriched[[pos]]
    base_cols  <- models_v1$base_feature_cols[[pos]]
    enr_cols   <- c(base_cols, ENRICHED_ONLY_COLS)
    medians_v1 <- models_v1$impute_medians[[pos]]

    if (is.null(mdl_v1)) {
      message(glue::glue("  [{pos}] v1 enriched model not found in s2_week10_models.rds -- score_v1 left NA."))
      next
    }

    # Add any missing feature columns as NA (will be imputed with training medians)
    for (col in enr_cols) {
      if (!col %in% names(pos_data)) pos_data[[col]] <- NA_real_
    }

    enr_imp_v1 <- .impute_with_medians(
      as.data.frame(pos_data[, enr_cols, drop = FALSE]),
      medians_v1[enr_cols]
    )
    X_enr_v1 <- as.matrix(enr_imp_v1)

    raw_v1 <- tryCatch(
      as.vector(predict(mdl_v1, newx = X_enr_v1, s = "lambda.min")),
      error = function(e) {
        message(glue::glue("  [{pos}] v1 prediction error: {e$message}"))
        rep(NA_real_, nrow(pos_data))
      }
    )

    # Scale using v3 training bounds
    train_pos <- dplyr::filter(training_preds, draft_position == pos)
    mn <- min(train_pos$pred_enriched, na.rm = TRUE)
    mx <- max(train_pos$pred_enriched, na.rm = TRUE)

    if (abs(mx - mn) > 1e-10) {
      scored_df[["score_v1"]][pos_idx] <- pmax(0, pmin(100,
        (raw_v1 - mn) / (mx - mn) * 100
      ))
    }
  }

  n_v1_scored <- sum(!is.na(scored_df$score_v1))
  message(glue::glue("  score_v1 computed for {n_v1_scored} / {nrow(scored_df)} players."))

  rm(models_v1)
  gc(verbose = FALSE)

  # --------------------------------------------------------------------------
  # STEP 6: Derive empirical tier thresholds from training players
  # --------------------------------------------------------------------------
  message("\nDeriving empirical tier thresholds from training player outcomes ...")

  # Identify metric columns (numeric, not in the exclusion list)
  all_numeric_cols <- names(scored_df)[
    purrr::map_lgl(scored_df, is.numeric)
  ]
  score_output_cols <- c(
    "score_base", "score_enriched", "score_final", "score_v1",
    "enriched_improvement_pct", "ppr_per_game_y13", "qualifying_seasons",
    "draft_round", "draft_pick", "draft_age", "is_hit", "is_training_player",
    "sos_imputed"
  )
  metric_cols <- setdiff(all_numeric_cols, c(NON_METRIC_COLS, score_output_cols))

  message(glue::glue("  Metric columns identified: {length(metric_cols)}"))

  # Store tier derivation results: tier_results[[position]][[metric]]
  tier_results   <- list()
  tier_ref_rows  <- list()

  for (pos in POSITIONS) {
    tier_results[[pos]] <- list()
    training_pos <- dplyr::filter(
      scored_df,
      position == pos,
      is_training_player == TRUE,
      !is.na(is_hit)
    )

    message(glue::glue(
      "  [{pos}] {nrow(training_pos)} training players with outcomes."
    ))

    for (metric in metric_cols) {
      # Only derive tiers for metrics with meaningful variance in this position
      if (!metric %in% names(training_pos)) next
      n_nonmissing <- sum(!is.na(training_pos[[metric]]))
      if (n_nonmissing < N_TIERS * MIN_BIN_SIZE) next

      result <- .derive_empirical_tiers(training_pos, metric, hit_col = "is_hit")

      if (!is.null(result)) {
        tier_results[[pos]][[metric]] <- result
        tier_ref_rows <- append(
          tier_ref_rows,
          list(dplyr::mutate(result$tier_ref, position = pos))
        )
      }
    }

    n_metrics_with_tiers <- length(tier_results[[pos]])
    message(glue::glue(
      "  [{pos}] Tier thresholds derived for {n_metrics_with_tiers} metrics."
    ))
  }

  # Save tier reference CSV
  if (length(tier_ref_rows) > 0L) {
    tier_reference <- dplyr::bind_rows(tier_ref_rows) |>
      dplyr::select(
        position, metric_name, tier, tier_label,
        break_lower, break_upper, hit_rate, n_in_bin, stable
      ) |>
      dplyr::mutate(
        hit_rate   = round(hit_rate, 4),
        break_lower = round(break_lower, 4),
        break_upper = round(break_upper, 4)
      ) |>
      dplyr::arrange(position, metric_name, tier)

    readr::write_csv(tier_reference, PATH_TIER_REF)
    message(glue::glue(
      "\n  Tier reference saved: {nrow(tier_reference)} rows -> {PATH_TIER_REF}"
    ))
  }

  # --------------------------------------------------------------------------
  # STEP 7: Apply tier assignments to all 848 players
  # --------------------------------------------------------------------------
  message("\nApplying tier assignments to all prospects ...")

  # Build per-position wide output with tier columns
  position_outputs <- vector("list", length(POSITIONS))
  names(position_outputs) <- POSITIONS

  for (pos in POSITIONS) {
    pos_data    <- dplyr::filter(scored_df, position == pos)
    pos_tiers   <- tier_results[[pos]]
    metrics_pos <- names(pos_tiers)

    for (metric in metrics_pos) {
      tier_assigned <- .apply_tiers_to_column(pos_data[[metric]], pos_tiers[[metric]])
      pos_data[[paste0(metric, "_tier")]]       <- tier_assigned$tier
      pos_data[[paste0(metric, "_tier_label")]] <- tier_assigned$tier_label
    }

    position_outputs[[pos]] <- pos_data
  }

  full_output <- dplyr::bind_rows(position_outputs)
  message(glue::glue("  Tier columns added. Output: {ncol(full_output)} columns."))

  # --------------------------------------------------------------------------
  # STEP 8: Compute per-position metric correlation matrices
  # --------------------------------------------------------------------------
  message("\nComputing metric correlation matrices ...")
  corr_rows <- list()

  for (pos in POSITIONS) {
    pos_data    <- dplyr::filter(scored_df, position == pos)
    metrics_pos <- names(tier_results[[pos]])

    if (length(metrics_pos) < 2L) next

    corr_mat <- pos_data |>
      dplyr::select(dplyr::all_of(metrics_pos)) |>
      dplyr::select(where(~ sum(!is.na(.x)) > 10L)) |>
      cor(use = "pairwise.complete.obs", method = "pearson")

    corr_long <- as.data.frame(corr_mat) |>
      tibble::rownames_to_column("metric_x") |>
      tidyr::pivot_longer(
        cols      = -metric_x,
        names_to  = "metric_y",
        values_to = "pearson_r"
      ) |>
      dplyr::mutate(
        position  = pos,
        pearson_r = round(pearson_r, 4),
        high_collinearity = abs(pearson_r) >= 0.70 & metric_x != metric_y
      )

    corr_rows <- append(corr_rows, list(corr_long))
  }

  if (length(corr_rows) > 0L) {
    corr_full <- dplyr::bind_rows(corr_rows)
    readr::write_csv(corr_full, PATH_CORR_CSV)
    n_high_collinear <- sum(corr_full$high_collinearity, na.rm = TRUE) / 2L
    message(glue::glue(
      "  Correlation matrix saved: {PATH_CORR_CSV}",
      "\n  High-collinearity metric pairs (|r| >= 0.70): {n_high_collinear}"
    ))
  }

  # --------------------------------------------------------------------------
  # STEP 9: Assemble and save final CSV
  # --------------------------------------------------------------------------
  message("\nAssembling final output CSV ...")

  # Core output columns (always present)
  core_cols <- c(
    # Identity
    "cfb_player_name", "position", "draft_year", "draft_class_type",
    # Scores
    "score_v1", "score_final", "score_base", "score_enriched",
    "enriched_improvement_pct",
    # Outcome flags (training players only)
    "is_hit", "ppr_per_game_y13",
    # Flags
    "sos_imputed", "is_training_player"
  )

  # Optional identity columns (include if present)
  optional_identity <- c("cfb_primary_team", "nfl_gsis_id", "gsis_id",
                          "draft_round", "draft_pick", "qualifying_seasons")
  present_optional  <- optional_identity[optional_identity %in% names(full_output)]

  # Tier columns: raw value + _tier + _tier_label for each metric
  tier_col_metrics <- unique(unlist(purrr::map(POSITIONS, ~ names(tier_results[[.x]]))))
  tier_col_triples <- purrr::map(tier_col_metrics, ~ c(.x, paste0(.x, "_tier"), paste0(.x, "_tier_label"))) |>
    unlist()

  present_core    <- core_cols[core_cols %in% names(full_output)]
  present_triples <- tier_col_triples[tier_col_triples %in% names(full_output)]

  final_output <- full_output |>
    dplyr::select(
      dplyr::all_of(present_core),
      dplyr::all_of(present_optional),
      dplyr::all_of(present_triples)
    ) |>
    dplyr::arrange(position, dplyr::desc(score_final))

  readr::write_csv(final_output, PATH_OUT_CSV)

  # --------------------------------------------------------------------------
  # STEP 10: Console summary
  # --------------------------------------------------------------------------
  message(glue::glue("\n=== SCORING COMPLETE -- WEEK 14 (schema: s2_w14_v1) ===\n"))
  message(glue::glue("Output: {PATH_OUT_CSV}"))
  message(glue::glue("Rows: {nrow(final_output)} | Columns: {ncol(final_output)}\n"))

  message("--- Score Summary by Position ---")
  final_output |>
    dplyr::group_by(position) |>
    dplyr::summarise(
      n            = dplyr::n(),
      score_mean   = round(mean(score_final, na.rm = TRUE), 1),
      score_median = round(stats::median(score_final, na.rm = TRUE), 1),
      score_min    = round(min(score_final, na.rm = TRUE), 1),
      score_max    = round(max(score_final, na.rm = TRUE), 1),
      .groups      = "drop"
    ) |>
    print()

  message("\n--- Top 5 Prospects Per Position (score_final) ---")
  for (pos in POSITIONS) {
    message(glue::glue("\n  {pos}:"))
    final_output |>
      dplyr::filter(position == pos) |>
      dplyr::slice_max(score_final, n = 5L) |>
      dplyr::select(cfb_player_name, draft_year, score_final, score_v1) |>
      print()
  }

  message(glue::glue(
    "\nTier reference: {PATH_TIER_REF}",
    "\nMetric correlations: {PATH_CORR_CSV}",
    "\nSchema tag: s2_w14_v1\n"
  ))

  invisible(final_output)
}


# ==============================================================================
# EXECUTION
# ==============================================================================

run_week14_scoring()
