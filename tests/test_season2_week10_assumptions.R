# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 10
# Formal Assumption Validation: College-to-NFL Translation Model
# File: tests/test_season2_week10_assumptions.R
#
# Purpose: Formal assumption validation for R/24_translation_model.R.
#          Loads pipeline RDS artifacts and checks whether the statistical
#          assumptions underlying Elastic Net translation modeling hold for
#          this dataset. Results written to:
#            output/assumption_tests/assumptions_season2_week10.txt
#
# Sections:
#   A: ID Linkage Quality
#   B: Feature Distribution Integrity
#   C: Outcome Distribution Validity
#   D: Model Performance Diagnostics
#   E: Prediction Residual Structure
#
# Run:
#   source(here::here("tests", "test_season2_week10_assumptions.R"))
#
# Requires: Pipeline RDS outputs from run_week10_pipeline(). Run
#   examples/example_season2_week10.R first if these are not yet present.
# ==============================================================================

library(dplyr)
library(glue)
library(here)

source(here::here("R", "24_translation_model.R"))

# ==============================================================================
# CONFIGURATION
# ==============================================================================

CACHE_DIR  <- here::here("data", "season2_cache")
OUT_DIR    <- here::here("output", "assumption_tests")
OUT_FILE   <- file.path(OUT_DIR, "assumptions_season2_week10.txt")

# Collinearity threshold -- matches validate_translation_assumptions()
COLLIN_THRESHOLD <- 0.90

# Residual normality -- Shapiro-Wilk p-value threshold for flagging
SHAPIRO_P_FLAG <- 0.05

# Maximum acceptable SOS NA rate before flagging (informational)
SOS_NA_FLAG_PCT <- 30

# ==============================================================================
# LOAD PIPELINE ARTIFACTS
# ==============================================================================

cat("Loading pipeline artifacts...\n")

rds_paths <- list(
  crosswalk      = file.path(CACHE_DIR, "s2_week10_crosswalk.rds"),
  feature_matrix = file.path(CACHE_DIR, "s2_week10_feature_matrix.rds"),
  model_list     = file.path(CACHE_DIR, "s2_week10_models.rds"),
  performance    = file.path(CACHE_DIR, "s2_week10_performance.rds"),
  predictions    = file.path(CACHE_DIR, "s2_week10_predictions.rds")
)

missing_rds <- rds_paths[!purrr::map_lgl(rds_paths, file.exists)]
if (length(missing_rds) > 0L) {
  stop(glue(
    "Required pipeline artifacts not found:\n",
    paste(unlist(missing_rds), collapse = "\n"), "\n\n",
    "Run examples/example_season2_week10.R first to generate these files."
  ), call. = FALSE)
}

crosswalk      <- readRDS(rds_paths$crosswalk)
feature_matrix <- readRDS(rds_paths$feature_matrix)
model_list     <- readRDS(rds_paths$model_list)
performance    <- readRDS(rds_paths$performance)
loco_preds     <- readRDS(rds_paths$predictions)

cat(glue(
  "  Crosswalk     : {format(nrow(crosswalk), big.mark = ',')} rows\n",
  "  Feature matrix: {sum(purrr::map_int(feature_matrix$training, nrow))} ",
  "training | ",
  "{sum(purrr::map_int(feature_matrix$prediction, nrow))} prediction\n",
  "  LOCO preds    : {format(nrow(loco_preds), big.mark = ',')} rows\n\n"
))

# ==============================================================================
# LOGGING HELPERS
# ==============================================================================

results_log <- character(0L)

log <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  results_log <<- c(results_log, msg)
}

log_section <- function(title) {
  divider <- paste(rep("=", 60), collapse = "")
  log("\n", divider)
  log(title)
  log(divider)
}

status_line <- function(status, check_name, detail = "") {
  log(glue("  [{status}] {check_name}"))
  if (nchar(detail) > 0L) log(glue("        {detail}"))
}

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

log(paste(rep("=", 60), collapse = ""))
log("  Translation Model Assumption Validation")
log("  Season 2, Week 10")
log(glue(
  "  Training classes: {min(TRAINING_DRAFT_CLASSES)}-{CUTOFF_YEAR} ",
  "({length(TRAINING_DRAFT_CLASSES)} LOCO folds)"
))
log(glue("  Cutoff year: {CUTOFF_YEAR}"))
log(paste(rep("=", 60), collapse = ""))


# ==============================================================================
# SECTION A: ID Linkage Quality
# ==============================================================================

log_section("SECTION A: ID Linkage Quality")

# A1: Overall match rate
log("\nA1: Overall match rate")
n_total    <- nrow(crosswalk)
n_matched  <- sum(crosswalk$match_method != "unmatched")
match_rate <- round(100 * n_matched / n_total, 1)

if (match_rate >= 80) {
  status_line("PASS", "Overall match rate",
    glue("{match_rate}% matched ({format(n_matched, big.mark=',')} / ",
         "{format(n_total, big.mark=',')} draft entries)"))
} else if (match_rate >= 50) {
  status_line("FLAG", "Overall match rate",
    glue("{match_rate}% matched -- below 80% target. Review name normalization."))
} else {
  status_line("FAIL", "Overall match rate",
    glue("{match_rate}% matched -- critically low. Pipeline results unreliable."))
}

# A2: Match rate by method
log("\nA2: Match rate by method")
method_counts <- crosswalk %>%
  dplyr::count(match_method) %>%
  dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
  dplyr::arrange(dplyr::desc(n))

for (i in seq_len(nrow(method_counts))) {
  r <- method_counts[i, ]
  status_line("INFO", r$match_method,
    glue("{format(r$n, big.mark=',')} entries ({r$pct}%)"))
}

# A3: Match rate by position
log("\nA3: Match rate by position")
pos_match <- crosswalk %>%
  dplyr::group_by(draft_position) %>%
  dplyr::summarise(
    n_total   = dplyr::n(),
    n_matched = sum(match_method != "unmatched"),
    pct       = round(100 * n_matched / n_total, 1),
    .groups   = "drop"
  )

for (i in seq_len(nrow(pos_match))) {
  r <- pos_match[i, ]
  status <- if (r$pct >= 80) "PASS" else if (r$pct >= 50) "FLAG" else "FAIL"
  status_line(status, glue("{r$draft_position} match rate"),
    glue("{r$pct}% ({r$n_matched} / {r$n_total})"))
}

# A4: Fuzzy match confidence distribution
log("\nA4: Fuzzy match confidence (matched players only)")
fuzzy_rows <- crosswalk %>%
  dplyr::filter(match_method == "fuzzy_name")
n_fuzzy <- nrow(fuzzy_rows)

if (n_fuzzy == 0L) {
  status_line("INFO", "Fuzzy matches", "No fuzzy matches used")
} else {
  low_conf <- sum(fuzzy_rows$match_confidence < 0.85, na.rm = TRUE)
  status <- if (low_conf == 0L) "PASS" else "FLAG"
  status_line(status, "Fuzzy match confidence",
    glue("{n_fuzzy} fuzzy matches | {low_conf} with confidence < 0.85 ",
         "(candidates for manual review)"))
  if (low_conf > 0L) {
    log("        Low-confidence matches:")
    low_rows <- fuzzy_rows %>%
      dplyr::filter(match_confidence < 0.85) %>%
      dplyr::arrange(match_confidence)
    for (j in seq_len(nrow(low_rows))) {
      r <- low_rows[j, ]
      log(glue(
        "          {r$draft_year} {r$draft_position}: ",
        "{r$cfb_player_name} (conf={r$match_confidence})"
      ))
    }
  }
}

# A5: GSIS ID coverage for matched players
log("\nA5: GSIS ID coverage (matched players)")
matched_cw <- crosswalk %>% dplyr::filter(match_method != "unmatched")
n_no_gsis  <- sum(is.na(matched_cw$nfl_gsis_id))
gsis_rate  <- round(100 * (1 - n_no_gsis / nrow(matched_cw)), 1)

status <- if (gsis_rate >= 90) "PASS" else "FLAG"
status_line(status, "GSIS ID coverage",
  glue("{gsis_rate}% of matched players have GSIS ID ",
       "({n_no_gsis} missing -- may be historical or UDFA)"))


# ==============================================================================
# SECTION B: Feature Distribution Integrity
# ==============================================================================

log_section("SECTION B: Feature Distribution Integrity")

# B1: SOS NA rate by position (from feature matrix)
log("\nB1: SOS NA rate by position")
sos_na <- feature_matrix$sos_na_rate

for (i in seq_len(nrow(sos_na))) {
  r <- sos_na[i, ]
  status <- if (r$sos_na_pct <= SOS_NA_FLAG_PCT) "PASS" else "FLAG"
  status_line(status, glue("{r$draft_position} SOS NA rate"),
    glue("{r$sos_na_pct}% NA ({r$n_sos_na} / {r$n_total} players) -- ",
         "median imputed for missing values"))
}

# B2: Feature range plausibility per position
log("\nB2: Feature range plausibility (training data)")
implausible_found <- FALSE

for (pos in TRANSLATION_POSITIONS) {
  df        <- feature_matrix$training[[pos]]
  base_cols <- feature_matrix$base_feature_cols[[pos]]

  if (nrow(df) == 0L) {
    status_line("SKIP", glue("{pos} feature ranges"), "No training rows")
    next
  }

  # Position-specific plausibility bounds
  bounds <- list(
    completion_pct       = c(0.0,  1.0),
    catch_rate           = c(0.0,  1.0),
    success_rate         = c(0.0,  1.0),
    pass_epa_per_attempt = c(-2.0, 2.0),
    rush_epa_per_attempt = c(-2.0, 2.0),
    rec_epa_per_target   = c(-2.0, 2.0),
    games_played         = c(1.0, 16.0)
  )

  flags <- character(0L)
  for (col in intersect(names(bounds), base_cols)) {
    if (!col %in% names(df)) next
    vals <- df[[col]]
    lo   <- bounds[[col]][1L]
    hi   <- bounds[[col]][2L]
    n_out <- sum(vals < lo | vals > hi, na.rm = TRUE)
    if (n_out > 0L) {
      flags <- c(flags, glue("{col}: {n_out} values outside [{lo}, {hi}]"))
      implausible_found <- TRUE
    }
  }

  if (length(flags) == 0L) {
    status_line("PASS", glue("{pos} feature ranges"),
      glue("All bounded features within plausible range ",
           "(n={format(nrow(df), big.mark=',')})"))
  } else {
    for (f in flags) {
      status_line("FLAG", glue("{pos} feature range"), f)
    }
  }
}

if (!implausible_found) {
  log("        All positions: bounded features within expected ranges.")
}

# B3: Per-game volume stats -- check for extreme outliers (> 6 SD)
log("\nB3: Per-game volume outlier check (> 6 SD from mean)")

per_game_cols <- c("pass_att_pg", "pass_yd_pg", "rush_att_pg",
                   "rush_yd_pg", "rec_yd_pg", "rec_yd_per_team_pass_att")

for (pos in TRANSLATION_POSITIONS) {
  df   <- feature_matrix$training[[pos]]
  cols <- intersect(per_game_cols, names(df))

  if (nrow(df) < 5L || length(cols) == 0L) next

  outlier_flags <- character(0L)
  for (col in cols) {
    vals <- df[[col]]
    m    <- mean(vals, na.rm = TRUE)
    s    <- stats::sd(vals, na.rm = TRUE)
    if (is.na(s) || s == 0) next
    n_out <- sum(abs(vals - m) > 6 * s, na.rm = TRUE)
    if (n_out > 0L) {
      outlier_flags <- c(outlier_flags,
        glue("{col}: {n_out} player(s) > 6 SD"))
    }
  }

  if (length(outlier_flags) == 0L) {
    status_line("PASS", glue("{pos} volume outliers"), "No extreme outliers")
  } else {
    for (f in outlier_flags) {
      status_line("FLAG", glue("{pos}"), f)
    }
  }
}

# B4: Age centering -- training set means are within expected NFL draft range
log("\nB4: Draft age centering validity (expected 21-24 per position)")
age_centers <- feature_matrix$age_centers

for (pos in TRANSLATION_POSITIONS) {
  center <- age_centers[[pos]]
  if (is.null(center) || is.na(center)) {
    status_line("FLAG", glue("{pos} age center"), "Not computed")
    next
  }
  status <- if (center >= 21 && center <= 24) "PASS" else "FLAG"
  status_line(status, glue("{pos} age center"),
    glue("Mean draft age = {round(center, 2)} | ",
         if (status == "PASS") "Within expected range (21-24)"
         else "Outside expected range -- review draft data"))
}

# B5: Collinearity -- report pairs flagged above threshold
log(glue("\nB5: Feature collinearity (|r| > {COLLIN_THRESHOLD})"))
log("        (Elastic Net regularization handles collinearity -- informational)")

collin_result <- validate_translation_assumptions(
  crosswalk      = crosswalk,
  feature_matrix = feature_matrix,
  verbose        = FALSE
)

collin_flags <- collin_result$collinearity_flags

if (nrow(collin_flags) == 0L) {
  status_line("PASS", "Feature collinearity",
    glue("No pairs with |r| > {COLLIN_THRESHOLD}"))
} else {
  status_line("INFO", "Collinear feature pairs",
    glue("{nrow(collin_flags)} pair(s) flagged -- documented below"))
  for (i in seq_len(nrow(collin_flags))) {
    r <- collin_flags[i, ]
    log(glue(
      "          {r$position}: {r$feature_1} x {r$feature_2} ",
      "(r = {r$correlation})"
    ))
  }
}


# ==============================================================================
# SECTION C: Outcome Distribution Validity
# ==============================================================================

log_section("SECTION C: Outcome Distribution Validity")

# C1: PPR per game range check by position
log("\nC1: PPR per game range (training outcomes)")

for (pos in TRANSLATION_POSITIONS) {
  df <- feature_matrix$training[[pos]]
  if (nrow(df) == 0L || !"ppr_per_game_y13" %in% names(df)) {
    status_line("SKIP", glue("{pos} outcome range"), "No training rows")
    next
  }
  vals <- df$ppr_per_game_y13[!is.na(df$ppr_per_game_y13)]
  if (length(vals) == 0L) next

  ppr_min  <- round(min(vals), 2)
  ppr_max  <- round(max(vals), 2)
  ppr_mean <- round(mean(vals), 2)
  ppr_sd   <- round(stats::sd(vals), 2)

  range_ok <- ppr_min >= 0 && ppr_max <= 60
  status   <- if (range_ok) "PASS" else "FLAG"
  status_line(status, glue("{pos} PPR range"),
    glue("min={ppr_min} | max={ppr_max} | mean={ppr_mean} | sd={ppr_sd} | ",
         "n={length(vals)}"))
}

# C2: Hit rate by position -- should be ~top fraction matching threshold
log("\nC2: Hit rate by position")
hit_threshold_desc <- list(
  QB = "top-12 QB", RB = "top-36 RB", WR = "top-36 WR", TE = "top-12 TE"
)

for (pos in TRANSLATION_POSITIONS) {
  df <- feature_matrix$training[[pos]]
  if (nrow(df) == 0L || !"is_hit" %in% names(df)) {
    status_line("SKIP", glue("{pos} hit rate"), "No training rows")
    next
  }
  hits     <- sum(df$is_hit, na.rm = TRUE)
  n_total  <- sum(!is.na(df$is_hit))
  hit_rate <- round(100 * hits / n_total, 1)

  # Expected rate: if 12 hits per position per season, ~12/250 picks = ~5%
  # But our training set spans 9 draft classes -- hit rate > 5% is expected
  # since non-skill players are excluded
  status <- if (hit_rate > 0 && hit_rate <= 60) "PASS" else "FLAG"
  status_line(status, glue("{pos} hit rate"),
    glue("{hit_rate}% ({hits} / {n_total}) ever {hit_threshold_desc[[pos]]} ",
         "in Years 1-3"))
}

# C3: Outcome non-degeneracy -- not all zeros
log("\nC3: Outcome non-degeneracy (not all zeros)")

for (pos in TRANSLATION_POSITIONS) {
  df <- feature_matrix$training[[pos]]
  if (nrow(df) == 0L) next
  n_nonzero <- sum(df$ppr_per_game_y13 > 0, na.rm = TRUE)
  status    <- if (n_nonzero > 0L) "PASS" else "FAIL"
  status_line(status, glue("{pos} non-degenerate outcomes"),
    glue("{format(n_nonzero, big.mark=',')} players with PPR > 0"))
}

# C4: Qualifying seasons distribution
log("\nC4: Qualifying NFL seasons (players with >= 1 qualifying season)")

for (pos in TRANSLATION_POSITIONS) {
  df <- feature_matrix$training[[pos]]
  if (nrow(df) == 0L || !"qualifying_seasons" %in% names(df)) next

  n_zero_qual <- sum(df$qualifying_seasons == 0L, na.rm = TRUE)
  n_total     <- nrow(df)
  pct_zero    <- round(100 * n_zero_qual / n_total, 1)

  status <- if (pct_zero <= 30) "PASS" else "FLAG"
  status_line(status, glue("{pos} qualifying seasons"),
    glue("{n_zero_qual} / {n_total} ({pct_zero}%) players with 0 qualifying ",
         "seasons -- these contribute PPR = 0 to the outcome"))
}


# ==============================================================================
# SECTION D: Model Performance Diagnostics
# ==============================================================================

log_section("SECTION D: Model Performance Diagnostics")

# D1: LOCO RMSE by position and model variant
log("\nD1: LOCO RMSE (lower = better)")
log("        Reminder: enriched model adds draft capital to base model")

for (pos in TRANSLATION_POSITIONS) {
  base_row <- performance %>%
    dplyr::filter(draft_position == pos, model_variant == "base")
  enr_row  <- performance %>%
    dplyr::filter(draft_position == pos, model_variant == "enriched")

  if (nrow(base_row) == 0L || nrow(enr_row) == 0L) next

  delta       <- round(enr_row$accuracy_delta, 3)
  delta_label <- if (!is.na(delta) && delta < 0) {
    glue("enriched improves by {abs(delta)} PPR/game")
  } else if (!is.na(delta) && delta > 0) {
    glue("base outperforms enriched by {delta} -- college production alone stronger")
  } else {
    "no delta"
  }

  status_line("INFO", glue("{pos} RMSE"),
    glue("base={round(base_row$rmse, 3)} | enriched={round(enr_row$rmse, 3)} | ",
         delta_label))
}

# D2: R-squared by position and variant
log("\nD2: R-squared (LOCO out-of-sample)")
log("        Negative R-squared: model predicts worse than position mean")

for (pos in TRANSLATION_POSITIONS) {
  base_row <- performance %>%
    dplyr::filter(draft_position == pos, model_variant == "base")
  enr_row  <- performance %>%
    dplyr::filter(draft_position == pos, model_variant == "enriched")

  if (nrow(base_row) == 0L || nrow(enr_row) == 0L) next

  base_r2 <- round(base_row$r_squared, 3)
  enr_r2  <- round(enr_row$r_squared, 3)

  base_status <- if (base_r2 > 0.10) "PASS" else if (base_r2 > 0) "FLAG" else "FAIL"
  enr_status  <- if (enr_r2  > 0.10) "PASS" else if (enr_r2  > 0) "FLAG" else "FAIL"

  status_line(base_status, glue("{pos} base R-squared"),
    glue("{base_r2} | ",
         if (base_r2 < 0) "College production does not translate above mean (expected for TE)"
         else if (base_r2 < 0.10) "Weak -- draft capital likely required for meaningful signal"
         else "Meaningful signal in college production alone"))

  status_line(enr_status, glue("{pos} enriched R-squared"),
    glue("{enr_r2} | ",
         if (enr_r2 < 0) "Even with draft capital, prediction is below mean -- review features"
         else if (enr_r2 < 0.10) "Weak -- NFL opportunity dominates college production signal"
         else "Model predicts better than position mean"))
}

# D3: Hit classification accuracy
log("\nD3: Hit classification accuracy (predicted top-half = hit)")

for (pos in TRANSLATION_POSITIONS) {
  enr_row <- performance %>%
    dplyr::filter(draft_position == pos, model_variant == "enriched")
  if (nrow(enr_row) == 0L) next

  acc    <- round(enr_row$hit_accuracy, 3)
  status <- if (acc >= 0.60) "PASS" else if (acc >= 0.50) "FLAG" else "FAIL"
  status_line(status, glue("{pos} hit accuracy"),
    glue("{round(acc * 100, 1)}% correct (enriched model) | ",
         if (acc >= 0.60) "Above random baseline"
         else if (acc >= 0.50) "Near random -- model struggles with hit/bust classification"
         else "Below random -- review outcome variable definition"))
}

# D4: Draft capital contribution -- accuracy delta interpretation
log("\nD4: Draft capital contribution (accuracy_delta = enriched RMSE - base RMSE)")
log("        Negative delta = enriched model improves over base")
log("        Large negative delta = NFL opportunity drives outcomes more than production")

for (pos in TRANSLATION_POSITIONS) {
  enr_row <- performance %>%
    dplyr::filter(draft_position == pos, model_variant == "enriched")
  if (nrow(enr_row) == 0L || is.na(enr_row$accuracy_delta)) next

  delta  <- round(enr_row$accuracy_delta, 3)
  status <- if (delta < 0) "PASS" else "FLAG"
  status_line(status, glue("{pos} draft capital contribution"),
    glue("delta = {delta} | ",
         if (delta < -0.5) "Strong -- draft capital is a major predictor at this position"
         else if (delta < 0) "Moderate improvement from draft capital"
         else "Draft capital did not improve predictions -- production signal is primary"))
}


# ==============================================================================
# SECTION E: Prediction Residual Structure
# ==============================================================================

log_section("SECTION E: Prediction Residual Structure")

# E1: Residual normality (Shapiro-Wilk) -- Elastic Net does not assume normality
#     but severe non-normality may indicate outcome misspecification.
log("\nE1: Residual normality (Shapiro-Wilk on enriched model residuals)")
log("        Elastic Net does not require normality -- FLAG is informational only")

for (pos in TRANSLATION_POSITIONS) {
  pos_preds <- loco_preds %>%
    dplyr::filter(draft_position == pos, !is.na(pred_enriched),
                  !is.na(ppr_per_game_y13))

  if (nrow(pos_preds) < 5L) {
    status_line("SKIP", glue("{pos} residual normality"),
      glue("n={nrow(pos_preds)} -- too few for Shapiro-Wilk"))
    next
  }

  residuals <- pos_preds$ppr_per_game_y13 - pos_preds$pred_enriched

  sw <- tryCatch(
    stats::shapiro.test(residuals),
    error = function(e) NULL
  )

  if (is.null(sw)) {
    status_line("SKIP", glue("{pos} residual normality"), "Test failed to run")
  } else {
    status <- if (sw$p.value >= SHAPIRO_P_FLAG) "PASS" else "FLAG"
    status_line(status, glue("{pos} residual normality"),
      glue("W={round(sw$statistic, 4)}, p={format(sw$p.value, digits=3)} | ",
           if (sw$p.value < SHAPIRO_P_FLAG)
             "Non-normal. Elastic Net is robust to this. CIs may be approximate."
           else "Normal -- no concern"))
  }
}

# E2: Residual bias -- mean residual should be near zero (no systematic over/under)
log("\nE2: Residual bias check (mean residual near zero = unbiased)")

for (pos in TRANSLATION_POSITIONS) {
  pos_preds <- loco_preds %>%
    dplyr::filter(draft_position == pos, !is.na(pred_enriched),
                  !is.na(ppr_per_game_y13))

  if (nrow(pos_preds) < 3L) next

  residuals  <- pos_preds$ppr_per_game_y13 - pos_preds$pred_enriched
  mean_resid <- round(mean(residuals, na.rm = TRUE), 3)
  status     <- if (abs(mean_resid) < 0.5) "PASS" else "FLAG"

  status_line(status, glue("{pos} residual bias"),
    glue("Mean residual = {mean_resid} PPR/game | ",
         if (abs(mean_resid) < 0.5) "No systematic bias"
         else if (mean_resid > 0) "Model systematically under-predicts"
         else "Model systematically over-predicts"))
}

# E3: Residual heteroscedasticity -- do residuals grow with predicted value?
#     Test: Pearson correlation between |residual| and predicted value.
log("\nE3: Residual heteroscedasticity (|residual| vs predicted)")
log("        High correlation = variance grows with prediction = concern for interval accuracy")

for (pos in TRANSLATION_POSITIONS) {
  pos_preds <- loco_preds %>%
    dplyr::filter(draft_position == pos, !is.na(pred_enriched),
                  !is.na(ppr_per_game_y13))

  if (nrow(pos_preds) < 5L) next

  residuals     <- pos_preds$ppr_per_game_y13 - pos_preds$pred_enriched
  abs_residuals <- abs(residuals)
  pred_vals     <- pos_preds$pred_enriched

  r <- tryCatch(
    stats::cor(abs_residuals, pred_vals, use = "complete.obs"),
    error = function(e) NA_real_
  )

  if (is.na(r)) {
    status_line("SKIP", glue("{pos} heteroscedasticity"), "Correlation failed")
    next
  }

  status <- if (abs(r) < 0.40) "PASS" else "FLAG"
  detail_msg <- if (abs(r) < 0.40) {
    glue("|r| = {round(abs(r), 3)} | Low -- prediction intervals approximately uniform")
  } else {
    glue("|r| = {round(abs(r), 3)} | Moderate -- variance grows with predicted PPR. High scorers are harder to bound.")
  }
  status_line(status, glue("{pos} heteroscedasticity"), detail_msg)
}

# E4: Prediction range vs actual range -- compression check
log("\nE4: Prediction compression check")
log("        Elastic Net regularization compresses predictions toward mean by design")

for (pos in TRANSLATION_POSITIONS) {
  pos_preds <- loco_preds %>%
    dplyr::filter(draft_position == pos, !is.na(pred_enriched),
                  !is.na(ppr_per_game_y13))

  if (nrow(pos_preds) < 3L) next

  actual_range <- round(diff(range(pos_preds$ppr_per_game_y13, na.rm = TRUE)), 2)
  pred_range   <- round(diff(range(pos_preds$pred_enriched, na.rm = TRUE)), 2)
  compression  <- round(pred_range / actual_range, 3)

  status_line("INFO", glue("{pos} prediction compression"),
    glue("Actual range: {actual_range} PPR/game | Predicted range: {pred_range} | ",
         "Compression ratio: {compression} ",
         "(1.0 = no compression, <1.0 = compressed toward mean)"))
}

# E5: LOCO fold-level performance -- any single class driving results?
log("\nE5: LOCO fold stability (per-class RMSE)")
log("        One anomalous fold suggests a structural shift, not noise")

for (pos in TRANSLATION_POSITIONS) {
  pos_preds <- loco_preds %>%
    dplyr::filter(draft_position == pos, !is.na(pred_enriched),
                  !is.na(ppr_per_game_y13))

  if (nrow(pos_preds) < 3L) next

  fold_rmse <- pos_preds %>%
    dplyr::group_by(draft_year) %>%
    dplyr::summarise(
      n    = dplyr::n(),
      rmse = sqrt(mean((ppr_per_game_y13 - pred_enriched)^2, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(draft_year)

  overall_rmse <- sqrt(mean(
    (pos_preds$ppr_per_game_y13 - pos_preds$pred_enriched)^2, na.rm = TRUE
  ))

  max_fold_rmse <- max(fold_rmse$rmse, na.rm = TRUE)
  ratio         <- round(max_fold_rmse / overall_rmse, 2)
  worst_year    <- fold_rmse$draft_year[which.max(fold_rmse$rmse)]

  status <- if (ratio <= 2.0) "PASS" else "FLAG"
  status_line(status, glue("{pos} fold stability"),
    glue("Overall RMSE={round(overall_rmse, 3)} | ",
         "Worst fold: {worst_year} (RMSE={round(max_fold_rmse, 3)}, ",
         "{ratio}x overall) | ",
         if (ratio <= 2.0) "No anomalous fold"
         else "One draft class driving error -- may indicate structural shift"))

  log(glue("        Per-class RMSE: ",
    paste(
      purrr::map_chr(seq_len(nrow(fold_rmse)), function(j) {
        glue("{fold_rmse$draft_year[j]}:{round(fold_rmse$rmse[j], 2)}")
      }),
      collapse = " | "
    )
  ))
}


# ==============================================================================
# SUMMARY
# ==============================================================================

log_section("ASSUMPTION VALIDATION SUMMARY")

log(glue(
  "\nModel scope: {min(TRAINING_DRAFT_CLASSES)}-{CUTOFF_YEAR} ",
  "training classes | {length(TRAINING_DRAFT_CLASSES)} LOCO folds"
))
log(glue(
  "Total training players: ",
  "{format(sum(purrr::map_int(feature_matrix$training, nrow)), big.mark=',')}"
))
log(glue("ID match rate: {match_rate}%"))

log("\nKey findings:")
log(glue(
  "  - TE base R-squared = {round(performance$r_squared[performance$draft_position=='TE' & performance$model_variant=='base'], 3)} ",
  "-- college TE production does not translate above mean without draft capital"
))
log(glue(
  "  - WR base R-squared = {round(performance$r_squared[performance$draft_position=='WR' & performance$model_variant=='base'], 3)} ",
  "-- college WR production carries limited independent signal"
))
log(glue(
  "  - QB base R-squared = {round(performance$r_squared[performance$draft_position=='QB' & performance$model_variant=='base'], 3)} ",
  "-- strongest base-model signal of any position"
))
log(glue(
  "  - RB enriched R-squared = {round(performance$r_squared[performance$draft_position=='RB' & performance$model_variant=='enriched'], 3)} ",
  "-- most predictable position with full feature set"
))

log("\nKey informational flags (not failures):")
log("  - Elastic Net compression is by design -- prediction range < actual range")
log("  - SOS NA rate reflects R/22 coverage gaps -- median imputation applied")
log("  - Residual non-normality does not affect Elastic Net point estimates")
log("  - Collinear SOS features are handled by regularization, not removed")

log(glue("\nResults saved to: {OUT_FILE}"))
log(paste(rep("=", 60), collapse = ""))

# ==============================================================================
# WRITE TO FILE
# ==============================================================================

writeLines(results_log, OUT_FILE)
cat(glue(
  "\n\nAssumption validation complete. Results written to:\n{OUT_FILE}\n\n"
))
