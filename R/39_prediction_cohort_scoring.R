# ==============================================================================
# NFL Analytics Toolkit -- Season 3
# Prediction-Cohort Live Scoring (real pred_base / pred_enriched for rookies)
# File: R/39_prediction_cohort_scoring.R
#
# PURPOSE
# -------
# The incoming rookie / prediction cohort has never played an NFL snap, so it
# can never appear in any leave-one-class-out (LOCO) fold. R/24's predictions
# file (s2_week10_predictions.rds) therefore contains TRAINING players only.
# When R/29 joins R/28's prospects against that file on nfl_gsis_id, every
# rookie misses and falls to the score_final * 0.15 fallback -- an uncalibrated
# anchor that lands on ~42% of the prospect pool, almost all of them rookies.
#
# This script closes that gap. It scores the prediction cohort LIVE against the
# fitted v1 models (the same models R/28 uses for its live score_v1), keeping
# the raw PPR/game predictions that R/28 computes then discards. It writes a
# single augmented predictions file that R/29 consumes in place of the old one,
# so rookies arrive with a real pred_base and pred_enriched instead of a
# rescaled percentile.
#
# WHY v1 MODELS (s2_week10), NOT v3 (s2_week13)
# ---------------------------------------------
# R/29's prior point estimate is pred_base, and the precision weight that pairs
# with it is the v1 base LOCO RMSE. Scoring the cohort with the SAME v1 models
# keeps rookie predictions on the exact scale as the training LOCO predictions
# and the sigma. Using v3 here would put rookies on a different scale than the
# sigma that weights them. R/28 uses v3 for its 0-100 tier scores; that is a
# separate consumer and does not change what R/29 needs.
#
# MIXED PREDICTION TYPES IN ONE FILE (by design)
# ----------------------------------------------
# The output file carries two prediction kinds, deliberately generated
# differently and labelled by a pred_type column:
#
#   pred_type = "loco"        TRAINING rows, copied unchanged from the existing
#                             R/24 LOCO file. These are honest out-of-sample
#                             backtest predictions and the basis of the sigma
#                             calibration and the comparable-player pool. They
#                             are NOT re-scored: re-scoring training players
#                             against the final model would make them in-sample
#                             (those players were in the fit), optimistically
#                             biased, and would corrupt the backtest.
#
#   pred_type = "final_model" PREDICTION-COHORT (rookie) rows, scored live
#                             against the final models fit on all training
#                             classes. No LOCO fold ever held a rookie out, so
#                             the final model is the only thing that can score
#                             them, and is the correct thing.
#
# This is the standard "cross-validate to backtest, refit on all data to
# deploy" pattern. The two types coexist legitimately on the same PPR/game
# scale; pred_type lets every downstream consumer tell them apart.
#
# KNOWN, DOCUMENTED LIMITATION
# ----------------------------
# When a downstream tool (R/40, the Shiny rookie tab) finds a rookie's nearest
# historical comparables, it matches a final_model prediction against loco
# predictions. The two come from slightly different generation processes on the
# same scale (the final model is the same model plus one more class of data).
# The resulting bias is small but real. It is named here rather than engineered
# around. Consumers that care can filter on pred_type.
#
# ROW COUNT EXPECTATION
# ---------------------
# The feature CSV has 621 training rows, but the LOCO file has ~494: roughly 127
# training-feature players never entered the model (no valid 3-NFL-season
# outcome, or filtered for insufficient games) and have no LOCO prediction.
# R/39 does NOT resurrect them -- they are not rookies, have no outcome, are
# useless as comparables, and receive history-based priors in R/29 regardless.
# Expected output: ~227 prediction + ~494 training = ~721 rows. The exact
# training count is whatever the LOCO file contains and is reported at runtime.
#
# INPUTS (read from season2_cache -- the Season 2 artifacts this builds on)
# -------------------------------------------------------------------------
#   data/season2_cache/s2_week13_feature_matrix_full.csv  (848 rows: 227
#       prediction + 621 training; carries all v1 model feature columns plus
#       draft_round, draft_pick, position, draft_year, nfl_gsis_id,
#       cfb_player_name, draft_class_type)
#   data/season2_cache/s2_week10_models.rds               (v1 fitted models:
#       models_base, models_enriched, base_feature_cols, impute_medians,
#       models_components_base, models_components_enriched, stat_components)
#   data/season2_cache/s2_week10_predictions.rds          (v1 training LOCO
#       predictions: nfl_gsis_id, draft_year, draft_position, ppr_per_game_y13,
#       pred_base, pred_enriched, is_hit, plus pred_base_* / pred_enriched_*
#       component columns)
#
# OUTPUT (written to season3_cache -- the Season 3 convention)
# ------------------------------------------------------------
#   data/season3_cache/s3_r39_translation_preds_augmented.rds   (R/29 consumes)
#   data/season3_cache/s3_r39_translation_preds_augmented.csv   (eyeball twin)
#
# OUTPUT SCHEMA (player_uid + R/24 loco_predictions schema + peak + pred_type
#                + Wave A2 stat-line components)
# --------------------------------------------------------
#   player_uid, nfl_gsis_id, draft_year, draft_position, ppr_per_game_y13,
#   pred_base, pred_enriched, is_hit, peak_ppr_per_game, peak_career_year,
#   pred_type,
#   pred_base_<component> x8, pred_enriched_<component> x8
#
# WAVE A2: STAT-LINE COMPONENTS
# -----------------------------
# The scalar pred_base / pred_enriched columns are PPR/game points produced by
# a model fit directly on points, under R/24's REFERENCE_SCORING (a 4-point
# pass TD PPR line). They are fixed-scale: they cannot answer what a player is
# worth under any other scoring, and rescaling them requires a conversion
# factor that must be estimated per format.
#
# The component columns carry the same prediction decomposed into the eight
# stat quantities scoring actually operates on (pass yd/td/int, rush yd/td,
# rec/rec yd/rec td). A consumer applies its own scoring to that stat line and
# gets a scoring-responsive point estimate with no conversion factor and no
# refit. This is what lets R/29 build a prior that responds to league scoring
# instead of blending fixed-PPR units with scoring-responsive history.
#
# The scalar columns are RETAINED, not replaced. Existing consumers (R/40, the
# Shiny rookie tab, comparable-player lookups) keep working unchanged, and the
# scalars remain the correct thing to use wherever reference-scoring points are
# what is wanted. Backtesting established the two carry equal accuracy; the
# components are preferred for their scoring behaviour, not their error.
#
# Component columns are NOT invented here. For training rows they are copied
# from the LOCO file exactly as the scalars are. For rookies they are predicted
# live from R/24's per-component models, by the same mechanics and the same
# imputation policy used for the scalars.
#
# OBSERVED component columns (y13_*, no pred_ prefix) are deliberately NOT
# carried. They exist only for training rows and are outcomes, not predictions;
# a consumer wanting truth should read the LOCO file directly.
#
# player_uid is the never-NA identity key. It equals nfl_gsis_id for any player
# who has one (all training players, and any rookie already assigned a gsis),
# and a deterministic synthetic "rk_<normname>_<draft_year>_<position>" for the
# rookie cohort, whose members have no NFL gsis yet. Downstream consumers must
# join on player_uid, never nfl_gsis_id: joining the rookie cohort on a column
# that is NA for every rookie either fans out NA-on-NA (dplyr default
# na_matches = "na") or drops the whole cohort under a !is.na() filter.
#
# draft_year travels with the prediction so R/29's lifecycle fix can derive
# years_exp = season - draft_year. ppr_per_game_y13 and is_hit are NA for
# rookies (no outcome yet).
#
# SOURCE DEPENDENCIES
# -------------------
# None sourced. R/39 is self-contained: it reads the model object's own stored
# base_feature_cols and impute_medians, so it never hardcodes a feature list
# and follows automatically if R/27/R/24 are rebuilt with a changed feature set.
# The two helpers below (.impute_with_medians, .predict_cohort_ppg) reproduce
# R/28's scoring mechanics exactly (R/28 .impute_with_medians and
# .score_all_prospects), keeping the raw PPR/game predictions instead of the
# 0-100 rescaling R/28 applies.
#
# SCHEMA TAG: s3_r39_v1
# Author    : Christian LeBlanc
# Created   : 2026-06
# ==============================================================================


# ==============================================================================
# LIBRARIES
# ==============================================================================

library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(glue)
library(rlang)   # for !!! splicing of component predictions into tibble()
library(here)

if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop(
    "Package 'glmnet' is required. Install with: install.packages('glmnet')",
    call. = FALSE
  )
}


# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Positions scored. Matches R/24 TRANSLATION_POSITIONS.
POSITIONS <- c("QB", "RB", "WR", "TE")

# Enriched feature set = base_feature_cols + these two. Matches R/24/R/28.
ENRICHED_ONLY_COLS <- c("draft_round", "draft_pick")

# Marker for the incoming class in the feature CSV's draft_class_type column.
PREDICTION_CLASS_MARKER <- "prediction"
TRAINING_CLASS_MARKER   <- "training"

# --- Input paths (Season 2 artifacts) ---
FEATURES_PATH_DEFAULT <- here::here(
  "data", "season2_cache", "s2_week13_feature_matrix_full.csv"
)
MODELS_PATH_DEFAULT <- here::here(
  "data", "season2_cache", "s2_week10_models.rds"
)
LOCO_PREDS_PATH_DEFAULT <- here::here(
  "data", "season2_cache", "s2_week10_predictions.rds"
)

# --- Output paths (Season 3 convention: data/season3_cache/, s3_r39_ prefix) ---
OUTPUT_DIR_DEFAULT <- here::here("data", "season3_cache")
OUTPUT_RDS_DEFAULT <- file.path(
  OUTPUT_DIR_DEFAULT, "s3_r39_translation_preds_augmented.rds"
)
OUTPUT_CSV_DEFAULT <- file.path(
  OUTPUT_DIR_DEFAULT, "s3_r39_translation_preds_augmented.csv"
)

# Scalar (pre-A2) portion of the output schema, in column order.
# player_uid is the never-NA identity key (see .build_player_uid): real
# nfl_gsis_id when present, a deterministic synthetic for rookies who have no
# NFL gsis yet. Downstream consumers join on player_uid, NOT nfl_gsis_id, so a
# rookie cohort with NA gsis can never collide NA-on-NA or be silently dropped.
OUTPUT_SCHEMA_SCALAR <- c(
  "player_uid",
  "nfl_gsis_id", "draft_year", "draft_position", "ppr_per_game_y13",
  "pred_base", "pred_enriched", "is_hit",
  "peak_ppr_per_game", "peak_career_year", "pred_type"
)

# Backward-compatible alias. Anything that referenced OUTPUT_SCHEMA before A2
# wanted the scalar column set, which is what this still is.
OUTPUT_SCHEMA <- OUTPUT_SCHEMA_SCALAR

# Prediction-column prefixes, one per model variant. Component prediction
# columns are "<prefix><component>", matching R/24's loco_predictions naming
# exactly so training rows carry through with no renaming.
VARIANT_PREFIXES <- c(base = "pred_base_", enriched = "pred_enriched_")

# ------------------------------------------------------------------------------
# .component_pred_cols / .build_output_schema
#
# The component list is NOT hardcoded. It is read from the model object's own
# stat_components slot at runtime, exactly as base_feature_cols and
# impute_medians already are, so a change to R/24's STAT_COMPONENTS is followed
# automatically instead of silently disagreeing with a copy kept here.
# ------------------------------------------------------------------------------
.component_pred_cols <- function(stat_components, variant) {
  paste0(VARIANT_PREFIXES[[variant]], stat_components)
}

.build_output_schema <- function(stat_components) {
  c(
    OUTPUT_SCHEMA_SCALAR,
    .component_pred_cols(stat_components, "base"),
    .component_pred_cols(stat_components, "enriched")
  )
}

# v2: adds the Wave A2 component prediction columns. Scalar columns unchanged,
# so a v1 consumer reading this file by name still finds everything it expects.
R39_SCHEMA_TAG <- "s3_r39_v2"


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

# ------------------------------------------------------------------------------
# .uid_normalize_name / .build_player_uid
#
# Identity-key construction. nfl_gsis_id is NA for the rookie cohort (a 2026
# prospect has not been assigned an NFL gsis yet), so it cannot be the join key
# for that cohort. Using it anyway has two failure modes: dplyr's default
# na_matches = "na" lets NA-keyed rows match each other in a Cartesian fan-out,
# and consumers that filter !is.na(nfl_gsis_id) silently drop the entire rookie
# cohort. player_uid removes both by guaranteeing a non-NA key on every row.
#
# CONTRACT: this normalizer is intentionally self-contained and must stay
# byte-identical anywhere the key is rebuilt (e.g. the name/pick crosswalk on
# the feature-matrix side). It deliberately does NOT call either project
# .normalize_player_name(): R/24's strips to [a-z] while R/29's keeps spaces, so
# reusing one would couple this key to a normalizer that disagrees with the
# other. The logic below mirrors R/24's (strict, space-free) on purpose, since
# R/39 lives in the R/24 translation world. If this is ever centralized into a
# shared util, both sides must source the same function.
# ------------------------------------------------------------------------------
.uid_normalize_name <- function(name) {
  name <- tolower(as.character(name))
  # Strip generational suffixes (matches R/24 .normalize_player_name).
  name <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b\\.?", "", name)
  # Strip to a-z only: removes spaces, apostrophes, hyphens, periods, digits.
  name <- gsub("[^a-z]", "", name)
  trimws(name)
}

# Deterministic, never-NA identity key.
#   - real nfl_gsis_id when present (training players, and any rookie who has
#     already been assigned one)
#   - else a synthetic "rk_<normname>_<draft_year>_<position>" so two distinct
#     keyless players never collapse together. position is part of the key, so a
#     genuinely dual-position prospect (e.g. Jam Miller QB/RB) splits cleanly
#     into two keys instead of one ambiguous row.
# Vectorized: all four arguments are columns of equal length.
.build_player_uid <- function(nfl_gsis_id, cfb_player_name, draft_year,
                              position) {
  syn <- paste0(
    "rk_", .uid_normalize_name(cfb_player_name),
    "_", draft_year, "_", position
  )
  ifelse(!is.na(nfl_gsis_id), as.character(nfl_gsis_id), syn)
}

# ------------------------------------------------------------------------------
# .impute_with_medians
#
# Apply stored training medians to NA values. Reproduces R/28's helper of the
# same name exactly, which in turn matches R/24's .impute_features() NA policy:
#   - a column the model expects but absent from df is added as all-zero
#   - an NA cell is filled with the stored median, or 0 if the median is itself
#     NA (a degenerate training column)
# Identical fill policy keeps rookie predictions on the same footing as the
# training predictions the model was fit on.
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
# .predict_cohort_ppg
#
# Score one position's rows against the fitted base and enriched cv.glmnet
# models, returning RAW predictions in PPR/game units (no 0-100 rescaling).
# Reproduces the predict mechanics of R/28 .score_all_prospects() up to but not
# including the scale_0_100() step.
#
# Feature names and imputation medians are read from the model object itself
# (models$base_feature_cols[[pos]], models$impute_medians[[pos]]); nothing is
# hardcoded, so a feature-set change in a rebuilt model is followed
# automatically.
#
# @param pos_data        data.frame. One position's rows from the feature matrix.
# @param models          list. The v1 model object (models_base, models_enriched,
#                        base_feature_cols, impute_medians, and the Wave A2
#                        slots models_components_base / _enriched).
# @param pos             character. "QB" / "RB" / "WR" / "TE".
# @param stat_components character. Component names, read from the model
#                        object's stat_components slot by the caller.
# @return named list: pred_base and pred_enriched (PPR/game), plus one element
#         per component per variant named pred_base_<cmp> / pred_enriched_<cmp>
#         in that component's own stat units. Every element has length
#         nrow(pos_data).
# @keywords internal
.predict_cohort_ppg <- function(pos_data, models, pos, stat_components) {

  mdl_base     <- models$models_base[[pos]]
  mdl_enriched <- models$models_enriched[[pos]]

  if (is.null(mdl_base) || is.null(mdl_enriched)) {
    stop(glue(
      "Models not found for position {pos}. Expected ",
      "models$models_base${pos} and models$models_enriched${pos}. ",
      "Inspect str(readRDS(models_path)) to verify structure."
    ), call. = FALSE)
  }

  base_cols <- models$base_feature_cols[[pos]]
  enr_cols  <- c(base_cols, ENRICHED_ONLY_COLS)
  medians   <- models$impute_medians[[pos]]

  if (is.null(base_cols) || length(base_cols) == 0L) {
    stop(glue("base_feature_cols missing/empty for {pos}."), call. = FALSE)
  }
  if (is.null(medians)) {
    stop(glue("impute_medians missing for {pos}."), call. = FALSE)
  }

  # Impute on the exact column sets the models were fit on, in order.
  base_imp <- .impute_with_medians(
    as.data.frame(pos_data[, base_cols, drop = FALSE]), medians[base_cols]
  )
  enr_imp <- .impute_with_medians(
    as.data.frame(pos_data[, enr_cols, drop = FALSE]), medians[enr_cols]
  )

  X_base <- as.matrix(base_imp)
  X_enr  <- as.matrix(enr_imp)

  raw_base     <- as.vector(
    stats::predict(mdl_base, newx = X_base, s = "lambda.min")
  )
  raw_enriched <- as.vector(
    stats::predict(mdl_enriched, newx = X_enr, s = "lambda.min")
  )

  # --- Wave A2: per-component stat-line predictions ---------------------------
  # One model per component per variant, fit by R/24 on the same feature sets
  # and scored on the same imputed matrices, so a component prediction is on
  # identical footing with the scalar prediction beside it.
  #
  # A component R/24 short-circuited (nonzero fraction below
  # MIN_COMPONENT_NONZERO_FRAC, e.g. TE pass TDs at 0.0000) has no fitted model.
  # .predict_one_component handles that by emitting the stored constant, which
  # is the correct prediction for a degenerate component and keeps the column
  # present rather than absent. See its comments for the storage forms handled.
  n_rows <- nrow(pos_data)

  comp_base <- lapply(stat_components, function(cmp) {
    .predict_one_component(
      models$models_components_base[[pos]][[cmp]],
      X_base, n_rows, pos, cmp, "base"
    )
  })
  names(comp_base) <- .component_pred_cols(stat_components, "base")

  comp_enriched <- lapply(stat_components, function(cmp) {
    .predict_one_component(
      models$models_components_enriched[[pos]][[cmp]],
      X_enr, n_rows, pos, cmp, "enriched"
    )
  })
  names(comp_enriched) <- .component_pred_cols(stat_components, "enriched")

  c(
    list(pred_base = raw_base, pred_enriched = raw_enriched),
    comp_base,
    comp_enriched
  )
}

# ------------------------------------------------------------------------------
# .predict_one_component
#
# Score a single stat component for one position, tolerating the two legitimate
# forms R/24 can leave in models_components_*:
#
#   - a fitted cv.glmnet object  -> predict at lambda.min, same as the scalars
#   - a bare numeric constant    -> the component was short-circuited (its
#     nonzero fraction fell below MIN_COMPONENT_NONZERO_FRAC), so R/24 stored
#     the constant it collapsed to instead of fitting. Recycling that constant
#     across every row IS the model's prediction.
#   - NULL                       -> treated as a zero constant, warned once, on
#     the reading that an absent model for a degenerate component means the
#     component contributes nothing.
#
# Anything else stops. A silent wrong-shaped prediction here would flow into
# scoring and be invisible, so an unrecognised storage form is a hard error
# with the object class named, not a guess.
# ------------------------------------------------------------------------------
.predict_one_component <- function(mdl, X, n_rows, pos, component, variant) {

  if (is.null(mdl)) {
    warning(glue(
      "No component model for {pos} / {component} ({variant}); predicting 0 ",
      "for all rows. Expected either a fitted model or a stored constant. ",
      "Verify R/24's short-circuit path if this is unexpected."
    ), call. = FALSE)
    return(rep(0, n_rows))
  }

  if (inherits(mdl, c("cv.glmnet", "glmnet"))) {
    return(as.vector(stats::predict(mdl, newx = X, s = "lambda.min")))
  }

  if (is.numeric(mdl) && length(mdl) == 1L) {
    return(rep(as.numeric(mdl), n_rows))
  }

  if (is.list(mdl) && !is.null(mdl$constant) &&
      is.numeric(mdl$constant) && length(mdl$constant) == 1L) {
    return(rep(as.numeric(mdl$constant), n_rows))
  }

  stop(glue(
    "Unrecognised component model form for {pos} / {component} ({variant}): ",
    "class {paste(class(mdl), collapse = '/')}, length {length(mdl)}. ",
    "R/39 handles a fitted cv.glmnet, a length-1 numeric constant, or a list ",
    "with a numeric $constant. Inspect ",
    "str(models$models_components_{variant}${pos}${component}) and extend ",
    ".predict_one_component() to match."
  ), call. = FALSE)
}

# ------------------------------------------------------------------------------
# .validate_inputs
#
# Fail early with an actionable message if any input is missing or stale.
# Mirrors R/28's stale-feature-matrix guard: confirms every column each
# position's model expects (base_feature_cols + ENRICHED_ONLY_COLS) is present
# in the feature CSV, and that the model object carries the four required slots.
# ------------------------------------------------------------------------------
.validate_inputs <- function(features_path, models_path, loco_preds_path,
                              verbose = TRUE) {

  missing_files <- c(
    features   = features_path,
    models     = models_path,
    loco_preds = loco_preds_path
  )
  missing_files <- missing_files[!file.exists(missing_files)]
  if (length(missing_files) > 0L) {
    stop(glue(
      "R/39 input(s) not found:\n",
      paste(glue("  {names(missing_files)}: {missing_files}"),
            collapse = "\n")
    ), call. = FALSE)
  }

  models <- readRDS(models_path)
  required_slots <- c("models_base", "models_enriched",
                      "base_feature_cols", "impute_medians",
                      "models_components_base", "models_components_enriched",
                      "stat_components")
  missing_slots <- setdiff(required_slots, names(models))
  if (length(missing_slots) > 0L) {
    stop(glue(
      "Models object is missing required slot(s): ",
      "{paste(missing_slots, collapse = ', ')}.\n",
      "Expected the train_translation_model() return structure. The component ",
      "slots arrived with the Wave A2 R/24 rewrite; a models file predating it ",
      "will fail here. Regenerate with run_week10_pipeline()."
    ), call. = FALSE)
  }

  # Every position must carry every component in both variants. A partially
  # populated component list would produce a ragged output schema, which the
  # select() at bind time would turn into a confusing missing-column error far
  # from the real cause. Catch it here and name the gaps.
  stat_components <- models$stat_components
  if (length(stat_components) == 0L) {
    stop("models$stat_components is empty; nothing to score.", call. = FALSE)
  }

  comp_gaps <- character(0)
  for (variant in c("base", "enriched")) {
    slot <- models[[paste0("models_components_", variant)]]
    for (pos in POSITIONS) {
      have <- names(slot[[pos]])
      gap  <- setdiff(stat_components, have)
      if (length(gap) > 0L) {
        comp_gaps <- c(comp_gaps, glue(
          "  {variant} / {pos}: {paste(gap, collapse = ', ')}"
        ))
      }
    }
  }
  if (length(comp_gaps) > 0L) {
    stop(glue(
      "Component models missing for some position/component pairs:\n",
      "{paste(comp_gaps, collapse = '\n')}\n\n",
      "Every component in models$stat_components must be present for every ",
      "position in both variants. Regenerate with run_week10_pipeline()."
    ), call. = FALSE)
  }

  # Header-only read to check columns without loading 848 rows.
  csv_head <- readr::read_csv(features_path, show_col_types = FALSE, n_max = 1L)
  csv_cols <- names(csv_head)

  required_meta <- c("position", "draft_class_type", "draft_year",
                     "nfl_gsis_id", "cfb_player_name")
  missing_meta <- setdiff(required_meta, csv_cols)
  if (length(missing_meta) > 0L) {
    stop(glue(
      "Feature CSV missing required metadata column(s): ",
      "{paste(missing_meta, collapse = ', ')}.\n  {features_path}"
    ), call. = FALSE)
  }

  missing_by_pos <- purrr::map(POSITIONS, function(pos) {
    base_cols <- models$base_feature_cols[[pos]]
    enr_cols  <- c(base_cols, ENRICHED_ONLY_COLS)
    setdiff(enr_cols, csv_cols)
  })
  names(missing_by_pos) <- POSITIONS
  any_missing <- any(purrr::map_lgl(missing_by_pos, ~ length(.x) > 0L))

  if (any_missing) {
    report <- purrr::imap_chr(missing_by_pos, function(cols, pos) {
      if (length(cols) == 0L) return(NA_character_)
      glue("  {pos}: {paste(cols, collapse = ', ')}")
    })
    report <- report[!is.na(report)]
    stop(glue(
      "Feature CSV is STALE -- columns expected by the models but missing:\n",
      "{paste(report, collapse = '\n')}\n\n",
      "Regenerate s2_week13_feature_matrix_full.csv (see R/28 header)."
    ), call. = FALSE)
  }

  if (verbose) message("  Input validation passed: files, slots, columns OK.")
  invisible(TRUE)
}


# ==============================================================================
# EXPORT: score_prediction_cohort
# ==============================================================================

#' Score the prediction (rookie) cohort live and write the augmented preds file
#'
#' Produces a single predictions file combining unchanged training LOCO
#' predictions with live final-model predictions for the rookie cohort, both on
#' the v1 PPR/game scale, labelled by pred_type. See the file header for the
#' design rationale and the mixed-prediction-type contract.
#'
#' @param features_path   Path to s2_week13_feature_matrix_full.csv.
#' @param models_path     Path to s2_week10_models.rds (v1 fitted models).
#' @param loco_preds_path Path to s2_week10_predictions.rds (training LOCO).
#' @param output_rds_path Output .rds path (R/29 consumes this).
#' @param output_csv_path Output .csv twin for inspection.
#' @param positions       Character vector of positions to score.
#' @param write_output    Logical. Write files to disk. Default TRUE.
#' @param verbose         Logical. Progress messages. Default TRUE.
#' @return (invisibly) the augmented predictions tibble.
#' @export
score_prediction_cohort <- function(
    features_path   = FEATURES_PATH_DEFAULT,
    models_path     = MODELS_PATH_DEFAULT,
    loco_preds_path = LOCO_PREDS_PATH_DEFAULT,
    output_rds_path = OUTPUT_RDS_DEFAULT,
    output_csv_path = OUTPUT_CSV_DEFAULT,
    positions       = POSITIONS,
    write_output    = TRUE,
    verbose         = TRUE) {

  t_start <- proc.time()

  if (verbose) {
    message(strrep("=", 70))
    message("R/39 score_prediction_cohort()")
    message(glue("Schema: {R39_SCHEMA_TAG}"))
    message(strrep("=", 70))
  }

  # --- Step 0: validate ---
  if (verbose) message("\n[1/6] Validating inputs...")
  .validate_inputs(features_path, models_path, loco_preds_path,
                   verbose = verbose)

  # --- Step 1: load ---
  if (verbose) message("\n[2/6] Loading models, features, and LOCO preds...")
  models   <- readRDS(models_path)
  features <- readr::read_csv(features_path, show_col_types = FALSE)
  loco     <- readRDS(loco_preds_path)

  # Component vocabulary comes from the model object, never from a local copy.
  stat_components <- models$stat_components
  output_schema   <- .build_output_schema(stat_components)
  component_cols  <- setdiff(output_schema, OUTPUT_SCHEMA_SCALAR)

  if (verbose) {
    message(glue("  Feature rows: {format(nrow(features), big.mark = ',')}"))
    message(glue("  LOCO rows:    {format(nrow(loco), big.mark = ',')}"))
    message(glue("  Stat components ({length(stat_components)}): ",
                 "{paste(stat_components, collapse = ', ')}"))
  }

  # --- Step 2: split prediction vs training in the feature matrix ---
  if (verbose) message("\n[3/6] Splitting prediction cohort from training...")
  pred_rows <- dplyr::filter(
    features, .data$draft_class_type == PREDICTION_CLASS_MARKER
  )
  if (nrow(pred_rows) == 0L) {
    stop(glue(
      "No rows with draft_class_type == '{PREDICTION_CLASS_MARKER}' in the ",
      "feature CSV. Nothing to score."
    ), call. = FALSE)
  }
  if (verbose) {
    message(glue("  Prediction-cohort rows: ",
                 "{format(nrow(pred_rows), big.mark = ',')}"))
    pos_counts <- pred_rows |>
      dplyr::count(.data$position) |>
      dplyr::arrange(dplyr::desc(.data$n))
    for (i in seq_len(nrow(pos_counts))) {
      message(glue("    {pos_counts$position[i]}: {pos_counts$n[i]}"))
    }
  }

  # --- Step 3: score the prediction cohort live (per position) ---
  if (verbose) message("\n[4/6] Scoring prediction cohort against v1 models...")

  scored_list <- vector("list", length(positions))
  names(scored_list) <- positions

  # Per-position diagnostics: distribution shape of raw predictions, plus any
  # rookie whose prediction lands outside the training LOCO range for his
  # position. That training range is the exact bound R/28 clamps to in its
  # 0-100 rescaling, so it is the principled definition of "out of range." R/39
  # keeps raw predictions, so this is the ONLY place such extrapolation is
  # visible; R/28 would silently clamp it. Flagged by player name for eyeballing.
  diag_list <- vector("list", length(positions))
  names(diag_list) <- positions
  oor_list  <- list()   # out-of-range rows, named, collected across positions

  for (pos in positions) {
    pos_data <- dplyr::filter(pred_rows, .data$position == pos)
    if (nrow(pos_data) == 0L) {
      if (verbose) message(glue("  [{pos}] no rookies -- skipped."))
      next
    }

    preds <- .predict_cohort_ppg(pos_data, models, pos, stat_components)

    # Training LOCO range for this position (clamp bound reference).
    loco_pos <- dplyr::filter(loco, .data$draft_position == pos)
    base_lo  <- suppressWarnings(min(loco_pos$pred_base,     na.rm = TRUE))
    base_hi  <- suppressWarnings(max(loco_pos$pred_base,     na.rm = TRUE))
    enr_lo   <- suppressWarnings(min(loco_pos$pred_enriched, na.rm = TRUE))
    enr_hi   <- suppressWarnings(max(loco_pos$pred_enriched, na.rm = TRUE))

    scored_list[[pos]] <- tibble::tibble(
      player_uid       = .build_player_uid(
        pos_data$nfl_gsis_id, pos_data$cfb_player_name,
        pos_data$draft_year, pos
      ),
      nfl_gsis_id      = pos_data$nfl_gsis_id,
      draft_year       = pos_data$draft_year,
      draft_position   = pos,
      ppr_per_game_y13 = NA_real_,   # rookies have no outcome yet
      pred_base        = preds$pred_base,
      pred_enriched    = preds$pred_enriched,
      # NA_integer_ (not bare NA) to match the training is_hit column type, so
      # the bind below never coerces the column to logical. The Shiny rookie
      # tab tests is_hit > 0; an integer column keeps that test unambiguous.
      is_hit           = NA_integer_,
      peak_ppr_per_game = NA_real_,    # rookies have no career peak yet
      peak_career_year  = NA_integer_,
      pred_type        = "final_model",
      # Wave A2 component predictions, spliced in under the same names R/24
      # uses in loco_predictions so the training bind needs no renaming.
      # component_cols is built once above from the model's own vocabulary.
      !!!preds[component_cols]
    )

    # Distribution shape for both variants (min/median/mean/max).
    diag_list[[pos]] <- tibble::tibble(
      position       = pos,
      n              = nrow(pos_data),
      base_min       = round(min(preds$pred_base,    na.rm = TRUE), 2),
      base_median    = round(stats::median(preds$pred_base,    na.rm = TRUE), 2),
      base_mean      = round(mean(preds$pred_base,    na.rm = TRUE), 2),
      base_max       = round(max(preds$pred_base,    na.rm = TRUE), 2),
      enr_min        = round(min(preds$pred_enriched, na.rm = TRUE), 2),
      enr_median     = round(stats::median(preds$pred_enriched, na.rm = TRUE), 2),
      enr_mean       = round(mean(preds$pred_enriched, na.rm = TRUE), 2),
      enr_max        = round(max(preds$pred_enriched, na.rm = TRUE), 2),
      train_base_lo  = round(base_lo, 2),
      train_base_hi  = round(base_hi, 2),
      train_enr_lo   = round(enr_lo,  2),
      train_enr_hi   = round(enr_hi,  2)
    )

    # Resolve a display name for flagging. cfb_player_name is present in the
    # feature CSV; fall back to gsis_id if absent.
    nm <- if ("cfb_player_name" %in% names(pos_data)) {
      pos_data$cfb_player_name
    } else {
      pos_data$nfl_gsis_id
    }

    oor_mask <- (preds$pred_base     < base_lo) | (preds$pred_base     > base_hi) |
                (preds$pred_enriched < enr_lo)  | (preds$pred_enriched > enr_hi)
    oor_mask[is.na(oor_mask)] <- FALSE

    if (any(oor_mask)) {
      oor_list[[pos]] <- tibble::tibble(
        position      = pos,
        player        = nm[oor_mask],
        pred_base     = round(preds$pred_base[oor_mask], 2),
        pred_enriched = round(preds$pred_enriched[oor_mask], 2),
        train_base_range = glue("[{round(base_lo,1)}, {round(base_hi,1)}]"),
        train_enr_range  = glue("[{round(enr_lo,1)}, {round(enr_hi,1)}]")
      )
    }

    if (verbose) {
      message(glue(
        "  [{pos}] scored {nrow(pos_data)} | ",
        "base [{round(min(preds$pred_base),1)}, {round(max(preds$pred_base),1)}] ",
        "median {round(stats::median(preds$pred_base),1)} | ",
        "enr [{round(min(preds$pred_enriched),1)}, ",
        "{round(max(preds$pred_enriched),1)}] ",
        "median {round(stats::median(preds$pred_enriched),1)}"
      ))
    }
  }

  rookie_preds <- dplyr::bind_rows(scored_list)
  diagnostics  <- dplyr::bind_rows(diag_list)
  out_of_range <- if (length(oor_list) > 0L) dplyr::bind_rows(oor_list) else NULL

  # Surface out-of-range rookies loudly and by name -- this is the "look here"
  # layer for catching the model extrapolating on extreme inputs.
  if (verbose) {
    if (is.null(out_of_range)) {
      message("\n  All rookie predictions fall within the training range ",
              "for their position (no extrapolation flags).")
    } else {
      message(glue(
        "\n  {nrow(out_of_range)} rookie prediction(s) OUTSIDE the training ",
        "range -- inspect these by name:"
      ))
      print(as.data.frame(out_of_range))
    }
  }

  # --- Step 4: bring training LOCO predictions through unchanged ---
  if (verbose) message("\n[5/6] Carrying training LOCO predictions through...")

  # Component predictions for training rows are COPIED, never recomputed. These
  # players were in the fit, so re-scoring them against the final model would
  # make them in-sample and break the same backtest guarantee the scalar
  # columns preserve. The LOCO file's component columns are the honest
  # out-of-sample ones and are carried through untouched.
  loco_cols_needed <- c("nfl_gsis_id", "draft_year", "draft_position",
                        "ppr_per_game_y13", "pred_base", "pred_enriched",
                        "is_hit", "peak_ppr_per_game", "peak_career_year",
                        .component_pred_cols(stat_components, "base"),
                        .component_pred_cols(stat_components, "enriched"))
  missing_loco <- setdiff(loco_cols_needed, names(loco))
  if (length(missing_loco) > 0L) {
    stop(glue(
      "LOCO predictions file missing expected column(s): ",
      "{paste(missing_loco, collapse = ', ')}.\n  {loco_preds_path}\n",
      "Component columns arrived with the Wave A2 R/24 rewrite; a predictions ",
      "file predating it will fail here. Regenerate with run_week10_pipeline()."
    ), call. = FALSE)
  }

  training_preds <- loco |>
    dplyr::select(dplyr::all_of(loco_cols_needed)) |>
    dplyr::mutate(pred_type = "loco")

  # player_uid: prefer the key R/24 built upstream, where cfb_player_name is
  # available and a synthetic can be constructed for a gsis-less player. Fall
  # back to nfl_gsis_id only for a LOCO file predating that change, which
  # cannot key those rows at all -- the fallback is compatibility, not a
  # correct path, and it warns below when it leaves an NA behind.
  if ("player_uid" %in% names(loco)) {
    training_preds$player_uid <- as.character(loco$player_uid)
    if (verbose) message("  player_uid: read from LOCO file (R/24 upstream).")
  } else {
    training_preds$player_uid <- as.character(loco$nfl_gsis_id)
    if (verbose) {
      message(
        "  player_uid: LOCO file carries none; derived from nfl_gsis_id. ",
        "Regenerate R/24 to get upstream keys for gsis-less training rows."
      )
    }
  }

  na_uid_train <- sum(is.na(training_preds$player_uid))
  if (na_uid_train > 0L) {
    warning(glue(
      "{na_uid_train} training LOCO row(s) have NA player_uid. If the LOCO ",
      "file carries a player_uid column, R/24 failed to build one (likely an ",
      "NA cfb_player_name on a gsis-less row). If it does not, regenerate ",
      "R/24: the nfl_gsis_id fallback cannot key a player who never reached ",
      "an NFL roster."
    ), call. = FALSE)
  }

  if (verbose) {
    message(glue("  Training LOCO rows carried: ",
                 "{format(nrow(training_preds), big.mark = ',')}"))
  }

  # --- Step 5: bind, order, sanity-check ---
  augmented <- dplyr::bind_rows(training_preds, rookie_preds) |>
    dplyr::select(dplyr::all_of(output_schema))

  # player_uid is the join key downstream and must be unique within the file. A
  # collision means two rows will fan out on join -- exactly the failure mode
  # player_uid exists to prevent -- so flag it by name. Unlike the old check
  # this does not skip NA: player_uid is never NA by construction, so any
  # duplicate here is a real identity collision (e.g. two distinct keyless
  # players normalizing to the same name+year+position).
  dup_uids <- unique(augmented$player_uid[duplicated(augmented$player_uid)])
  if (length(dup_uids) > 0L) {
    # Resolve display names from the prediction feature rows, keyed by the same
    # uid construction used above; fall back to the uid string itself.
    pred_uid_lookup <- pred_rows |>
      dplyr::mutate(
        .uid = .build_player_uid(
          .data$nfl_gsis_id, .data$cfb_player_name,
          .data$draft_year, .data$position
        )
      )
    dup_names <- vapply(dup_uids, function(uid) {
      nm <- pred_uid_lookup$cfb_player_name[pred_uid_lookup$.uid == uid]
      nm <- nm[!is.na(nm)]
      if (length(nm) > 0L) nm[1] else uid
    }, character(1))
    warning(glue(
      "{length(dup_uids)} player_uid(s) are duplicated in the augmented file. ",
      "Players: {paste(dup_names, collapse = ', ')}. ",
      "Downstream consumers join on player_uid; investigate before relying ",
      "on output."
    ), call. = FALSE)
  }

  if (verbose) {
    type_counts <- augmented |> dplyr::count(.data$pred_type)
    message("\n  Augmented file composition:")
    for (i in seq_len(nrow(type_counts))) {
      message(glue("    {type_counts$pred_type[i]}: ",
                   "{format(type_counts$n[i], big.mark = ',')}"))
    }
    message(glue("    total: {format(nrow(augmented), big.mark = ',')}"))
  }

  # --- Component sanity: negative predictions -----------------------------
  # Elastic net is unconstrained, so a component model can predict a negative
  # number of receptions or yards for a low-projection player. That is not a
  # bug in R/39 and it is NOT clamped here: clamping is a modelling decision
  # about R/24's component models and belongs with Christian, not buried in a
  # scoring script. It is reported instead, because a negative stat line will
  # flow into any scoring a consumer applies and would otherwise be invisible.
  #
  # Note the sign asymmetry: a negative interception prediction ADDS points
  # under any scoring with a negative pass_int, so this is not merely cosmetic.
  neg_counts <- vapply(component_cols, function(cl) {
    sum(augmented[[cl]] < 0, na.rm = TRUE)
  }, integer(1))
  neg_counts <- neg_counts[neg_counts > 0L]

  if (verbose) {
    if (length(neg_counts) == 0L) {
      message("\n  No negative component predictions.")
    } else {
      message(glue(
        "\n  NEGATIVE component predictions in {length(neg_counts)} column(s) ",
        "(not clamped, reported only):"
      ))
      neg_tbl <- tibble::tibble(
        column = names(neg_counts),
        n_negative = as.integer(neg_counts),
        min_value = round(vapply(names(neg_counts), function(cl) {
          min(augmented[[cl]], na.rm = TRUE)
        }, numeric(1)), 3)
      ) |>
        dplyr::arrange(dplyr::desc(.data$n_negative))
      print(as.data.frame(neg_tbl))
    }
  }

  # --- Step 6: write ---
  if (write_output) {
    if (verbose) message("\n[6/6] Writing outputs to season3_cache...")
    out_dir <- dirname(output_rds_path)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    saveRDS(augmented, output_rds_path)
    readr::write_csv(augmented, output_csv_path)

    if (verbose) {
      message(glue("  Wrote: {output_rds_path}"))
      message(glue("  Wrote: {output_csv_path}"))
    }
  } else if (verbose) {
    message("\n[6/6] write_output = FALSE -- nothing written.")
  }

  t_elapsed <- round((proc.time() - t_start)[["elapsed"]], 1)
  if (verbose) {
    message("\n  Per-position prediction distribution (raw PPR/game):")
    print(as.data.frame(diagnostics))
    message(strrep("=", 70))
    message(glue("R/39 complete in {t_elapsed}s | schema {R39_SCHEMA_TAG}"))
    message(strrep("=", 70))
  }

  # Return the augmented tibble as the primary value, with diagnostics attached
  # as attributes so a caller can inspect distribution shape and extrapolation
  # flags without re-deriving them. The Wave A1 validation step reads these.
  attr(augmented, "diagnostics")  <- diagnostics
  attr(augmented, "out_of_range") <- out_of_range
  invisible(augmented)
}


# ==============================================================================
# EXECUTION
# ==============================================================================

# Run only when executed as a script (Rscript R/39_prediction_cohort_scoring.R).
# sys.nframe() == 0L is FALSE under source(), so sourcing this file to get the
# functions no longer triggers a full scoring run (plus two file writes). Call
# score_prediction_cohort() explicitly after sourcing if needed.
if (sys.nframe() == 0L) {
  score_prediction_cohort()
}
