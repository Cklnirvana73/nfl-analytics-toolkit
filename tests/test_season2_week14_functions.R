# ==============================================================================
# tests/test_season2_week14_functions.R
# Season 2, Week 14 -- Test Suite for R/28 Final Prospect Scoring
# ==============================================================================
#
# COVERAGE
#   Section 1  .impute_with_medians()    -- NA fill, missing col, non-NA guard
#   Section 2  .derive_empirical_tiers() -- tier count, hit-rate ordering,
#                                           label correctness, NULL on thin data
#   Section 3  .apply_tiers_to_column()  -- NA -> NO_DATA, valid tier range,
#                                           NULL tier_result path
#   Section 4  Output CSV contracts      -- row count, score bounds, schema,
#                                           training/prediction split correctness
#   Section 5  Tier reference contracts  -- hit-rate ordering, label validity,
#                                           stability flags
#
# DESIGN
#   - Sections 1-3 use synthetic fixtures. No live data downloads.
#   - Sections 4-5 skip automatically when production CSVs are absent.
#   - R/28 is NOT sourced directly (execution block would trigger full pipeline).
#     Helper functions are redefined inline from the confirmed R/28 source.
#
# RUN
#   testthat::test_file(here::here("tests", "test_season2_week14_functions.R"))
#
# SCHEMA TAG: s2_w14_v1
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)
library(here)


# ==============================================================================
# REPLICATED CONSTANTS (from R/28 -- keep in sync if R/28 changes)
# ==============================================================================

N_TIERS      <- 5L
MIN_BIN_SIZE <- 5L

TIER_LABELS <- c(
  "1" = "Elite Signal",
  "2" = "Strong Signal",
  "3" = "Moderate Signal",
  "4" = "Weak Signal",
  "5" = "No Signal"
)

VALID_TIER_LABELS <- c(
  "Elite Signal", "Strong Signal", "Moderate Signal",
  "Weak Signal", "No Signal", "NO_DATA"
)

POSITIONS <- c("QB", "RB", "WR", "TE")

# Production artifact paths
PATH_OUT_CSV  <- here::here("data", "season2_cache", "s2_week14_final_prospect_scores.csv")
PATH_TIER_REF <- here::here("data", "season2_cache", "s2_week14_tier_reference.csv")


# ==============================================================================
# REPLICATED HELPER FUNCTIONS (inline copies from R/28 for isolated unit tests)
# ==============================================================================

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

.derive_empirical_tiers <- function(training_df, metric, hit_col = "is_hit") {
  df <- dplyr::filter(
    training_df,
    !is.na(.data[[metric]]),
    !is.na(.data[[hit_col]])
  )
  n <- nrow(df)
  if (n < N_TIERS * MIN_BIN_SIZE) return(NULL)
  probs  <- seq(0, 1, length.out = N_TIERS + 1L)
  breaks <- quantile(df[[metric]], probs = probs, na.rm = TRUE)
  breaks <- unique(breaks)
  if (length(breaks) < 3L) return(NULL)
  breaks[1]              <- -Inf
  breaks[length(breaks)] <-  Inf
  df <- dplyr::mutate(
    df,
    .bin = as.integer(cut(.data[[metric]], breaks = breaks,
                          include.lowest = TRUE, labels = FALSE))
  )
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
  bin_stats <- bin_stats |>
    dplyr::arrange(dplyr::desc(hit_rate), dplyr::desc(break_lower)) |>
    dplyr::mutate(
      tier        = dplyr::row_number(),
      tier_label  = TIER_LABELS[as.character(tier)],
      stable      = n_in_bin >= MIN_BIN_SIZE,
      metric_name = metric
    )
  list(tier_ref = bin_stats, breaks = breaks)
}

.apply_tiers_to_column <- function(values, tier_result) {
  n <- length(values)
  if (is.null(tier_result)) {
    return(list(tier = rep(NA_integer_, n), tier_label = rep("NO_DATA", n)))
  }
  breaks   <- tier_result$breaks
  tier_ref <- tier_result$tier_ref
  bin_to_tier <- tibble::tibble(
    .bin       = tier_ref$.bin,
    tier       = tier_ref$tier,
    tier_label = tier_ref$tier_label
  )
  min_bin <- min(tier_ref$.bin, na.rm = TRUE)
  max_bin <- max(tier_ref$.bin, na.rm = TRUE)
  bins <- as.integer(cut(values, breaks = breaks, include.lowest = TRUE, labels = FALSE))
  bins <- dplyr::if_else(!is.na(bins), pmax(min_bin, pmin(max_bin, bins)), NA_integer_)
  result_df <- tibble::tibble(.bin = bins) |>
    dplyr::left_join(bin_to_tier, by = ".bin")
  result_df$tier_label <- dplyr::if_else(
    is.na(values), "NO_DATA",
    dplyr::coalesce(result_df$tier_label, "NO_DATA")
  )
  result_df$tier <- dplyr::if_else(is.na(values), NA_integer_, result_df$tier)
  list(tier = result_df$tier, tier_label = result_df$tier_label)
}


# ==============================================================================
# SYNTHETIC FIXTURE BUILDERS
# ==============================================================================

# Clean signal fixture: top decile are all hits, remainder are all misses.
# Produces predictable tier boundaries for assertion-level testing.
make_clean_signal_df <- function(n = 60L, seed = 99L) {
  set.seed(seed)
  n_hits <- 10L
  tibble::tibble(
    metric_a = c(seq(0, 0.79, length.out = n - n_hits),
                 seq(0.80, 1.00, length.out = n_hits)),
    is_hit   = c(rep(FALSE, n - n_hits), rep(TRUE, n_hits))
  )
}

# Noisy fixture: random metric, random hits, sufficient rows for tier derivation.
make_noisy_df <- function(n = 80L, seed = 7L) {
  set.seed(seed)
  tibble::tibble(
    metric_b = runif(n),
    is_hit   = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.25, 0.75))
  )
}

# Thin fixture: fewer rows than N_TIERS * MIN_BIN_SIZE to trigger NULL return.
make_thin_df <- function() {
  tibble::tibble(
    metric_c = seq(0, 1, length.out = 10L),
    is_hit   = rep(FALSE, 10L)
  )
}

# Impute fixture: single column, some NAs.
make_impute_df <- function() {
  data.frame(
    col_a = c(1.0, NA, 3.0, NA, 5.0),
    col_b = c(10.0, 20.0, NA, 40.0, 50.0)
  )
}


# ==============================================================================
# SECTION 1: .impute_with_medians
# ==============================================================================

test_that(".impute_with_medians fills NA values with supplied median", {
  df      <- make_impute_df()
  medians <- list(col_a = 3.0, col_b = 30.0)
  result  <- .impute_with_medians(df, medians)

  expect_equal(result$col_a[2], 3.0)
  expect_equal(result$col_b[3], 30.0)
  expect_false(anyNA(result$col_a))
  expect_false(anyNA(result$col_b))
})

test_that(".impute_with_medians does not alter non-NA values", {
  df      <- make_impute_df()
  medians <- list(col_a = 99.0, col_b = 99.0)
  result  <- .impute_with_medians(df, medians)

  expect_equal(result$col_a[1], 1.0)
  expect_equal(result$col_a[3], 3.0)
  expect_equal(result$col_a[5], 5.0)
  expect_equal(result$col_b[1], 10.0)
})

test_that(".impute_with_medians adds missing column as 0", {
  df      <- data.frame(col_a = c(1, 2, 3))
  medians <- list(col_a = 2.0, col_missing = 99.0)
  result  <- .impute_with_medians(df, medians)

  expect_true("col_missing" %in% names(result))
  expect_true(all(result$col_missing == 0))
})

test_that(".impute_with_medians uses 0 as fallback when median is NA", {
  df      <- data.frame(col_a = c(1, NA, 3))
  medians <- list(col_a = NA_real_)
  result  <- .impute_with_medians(df, medians)

  expect_equal(result$col_a[2], 0)
})

test_that(".impute_with_medians handles data frame with no NAs unchanged", {
  df      <- data.frame(col_a = c(1, 2, 3), col_b = c(4, 5, 6))
  medians <- list(col_a = 99, col_b = 99)
  result  <- .impute_with_medians(df, medians)

  expect_equal(result$col_a, c(1, 2, 3))
  expect_equal(result$col_b, c(4, 5, 6))
})


# ==============================================================================
# SECTION 2: .derive_empirical_tiers
# ==============================================================================

test_that(".derive_empirical_tiers returns NULL when data is too thin", {
  df     <- make_thin_df()
  result <- .derive_empirical_tiers(df, "metric_c", hit_col = "is_hit")

  expect_null(result)
})

test_that(".derive_empirical_tiers returns a list with tier_ref and breaks", {
  df     <- make_clean_signal_df()
  result <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")

  expect_false(is.null(result))
  expect_true(is.list(result))
  expect_true("tier_ref"  %in% names(result))
  expect_true("breaks"    %in% names(result))
})

test_that(".derive_empirical_tiers tier_ref has required columns", {
  df       <- make_clean_signal_df()
  result   <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")
  req_cols <- c("tier", "tier_label", "hit_rate", "n_in_bin",
                "break_lower", "break_upper", "stable", "metric_name")

  expect_true(all(req_cols %in% names(result$tier_ref)))
})

test_that(".derive_empirical_tiers Tier 1 has the highest hit rate", {
  df     <- make_clean_signal_df()
  result <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")
  ref    <- result$tier_ref

  tier1_hr <- ref$hit_rate[ref$tier == 1L]
  expect_true(all(tier1_hr >= ref$hit_rate))
})

test_that(".derive_empirical_tiers Tier 1 is labeled Elite Signal", {
  df     <- make_clean_signal_df()
  result <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")
  tier1  <- dplyr::filter(result$tier_ref, tier == 1L)

  expect_equal(unname(tier1$tier_label), "Elite Signal")
})

test_that(".derive_empirical_tiers all tier labels are valid values", {
  df     <- make_clean_signal_df()
  result <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")

  expect_true(all(result$tier_ref$tier_label %in% unname(TIER_LABELS)))
})

test_that(".derive_empirical_tiers metric_name column matches input metric", {
  df     <- make_noisy_df()
  result <- .derive_empirical_tiers(df, "metric_b", hit_col = "is_hit")

  expect_true(all(result$tier_ref$metric_name == "metric_b"))
})

test_that(".derive_empirical_tiers breaks vector has -Inf and +Inf at edges", {
  df     <- make_clean_signal_df()
  result <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")
  brks   <- result$breaks

  expect_equal(brks[1], -Inf)
  expect_equal(brks[length(brks)], Inf)
})

test_that(".derive_empirical_tiers hit rates are in [0, 1]", {
  df     <- make_noisy_df()
  result <- .derive_empirical_tiers(df, "metric_b", hit_col = "is_hit")

  expect_true(all(result$tier_ref$hit_rate >= 0))
  expect_true(all(result$tier_ref$hit_rate <= 1))
})

test_that(".derive_empirical_tiers n_in_bin sums to total non-NA rows", {
  df     <- make_clean_signal_df()
  n_nonmissing <- sum(!is.na(df$metric_a) & !is.na(df$is_hit))
  result <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")

  expect_equal(sum(result$tier_ref$n_in_bin), n_nonmissing)
})

test_that(".derive_empirical_tiers returns NULL for all-NA metric", {
  df <- tibble::tibble(
    metric_na = rep(NA_real_, 50L),
    is_hit    = rep(FALSE, 50L)
  )
  result <- .derive_empirical_tiers(df, "metric_na", hit_col = "is_hit")

  expect_null(result)
})


# ==============================================================================
# SECTION 3: .apply_tiers_to_column
# ==============================================================================

test_that(".apply_tiers_to_column assigns NO_DATA to NA inputs", {
  df     <- make_clean_signal_df()
  result <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")

  values  <- c(0.5, NA_real_, 0.9)
  applied <- .apply_tiers_to_column(values, result)

  expect_equal(unname(applied$tier_label[2]), "NO_DATA")
  expect_true(is.na(applied$tier[2]))
})

test_that(".apply_tiers_to_column assigns valid tiers to non-NA inputs", {
  df     <- make_clean_signal_df()
  result <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")

  values  <- seq(0, 1, length.out = 20)
  applied <- .apply_tiers_to_column(values, result)

  non_na_tiers <- applied$tier[!is.na(applied$tier)]
  expect_true(all(non_na_tiers %in% 1:5))
})

test_that(".apply_tiers_to_column all tier_labels are valid", {
  df     <- make_clean_signal_df()
  result <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")

  values  <- c(seq(0, 1, length.out = 50), NA_real_)
  applied <- .apply_tiers_to_column(values, result)

  expect_true(all(applied$tier_label %in% VALID_TIER_LABELS))
})

test_that(".apply_tiers_to_column with NULL tier_result returns NO_DATA for all", {
  values  <- c(1.0, 2.0, NA_real_, 4.0)
  applied <- .apply_tiers_to_column(values, NULL)

  expect_true(all(applied$tier_label == "NO_DATA"))
  expect_true(all(is.na(applied$tier)))
})

test_that(".apply_tiers_to_column output length matches input length", {
  df     <- make_clean_signal_df()
  result <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")

  values  <- runif(37)
  applied <- .apply_tiers_to_column(values, result)

  expect_equal(length(applied$tier),       37L)
  expect_equal(length(applied$tier_label), 37L)
})

test_that(".apply_tiers_to_column high values get Elite Signal on clean signal data", {
  df     <- make_clean_signal_df()
  result <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")

  # The top-decile values (0.9-1.0) should be Elite Signal (Tier 1)
  high_vals <- rep(0.98, 5)
  applied   <- .apply_tiers_to_column(high_vals, result)

  expect_true(all(applied$tier_label == "Elite Signal"))
})

test_that(".apply_tiers_to_column all-NA input returns all NO_DATA", {
  df     <- make_clean_signal_df()
  result <- .derive_empirical_tiers(df, "metric_a", hit_col = "is_hit")

  applied <- .apply_tiers_to_column(rep(NA_real_, 10), result)

  expect_true(all(applied$tier_label == "NO_DATA"))
  expect_true(all(is.na(applied$tier)))
})


# ==============================================================================
# SECTION 4: Output CSV contracts
# Skipped automatically when production CSV is absent (CI / fresh environment).
# ==============================================================================

test_that("output CSV exists and has correct dimensions", {
  skip_if_not(
    file.exists(PATH_OUT_CSV),
    "s2_week14_final_prospect_scores.csv not found -- run run_week14_scoring() first"
  )
  df <- readr::read_csv(PATH_OUT_CSV, show_col_types = FALSE)

  expect_equal(nrow(df), 848L)
  expect_true(ncol(df) >= 20L)
})

test_that("score_final is in [0, 100] with no NAs for all 848 rows", {
  skip_if_not(file.exists(PATH_OUT_CSV), "Output CSV absent")
  df <- readr::read_csv(PATH_OUT_CSV, show_col_types = FALSE)

  expect_false(anyNA(df$score_final))
  expect_true(all(df$score_final >= 0))
  expect_true(all(df$score_final <= 100))
})

test_that("score_v1 is in [0, 100] with no NAs for all 848 rows", {
  skip_if_not(file.exists(PATH_OUT_CSV), "Output CSV absent")
  df <- readr::read_csv(PATH_OUT_CSV, show_col_types = FALSE)

  expect_false(anyNA(df$score_v1))
  expect_true(all(df$score_v1 >= 0))
  expect_true(all(df$score_v1 <= 100))
})

test_that("training/prediction split matches expected counts", {
  skip_if_not(file.exists(PATH_OUT_CSV), "Output CSV absent")
  df <- readr::read_csv(PATH_OUT_CSV, show_col_types = FALSE)

  n_training   <- sum(df$draft_class_type == "training",   na.rm = TRUE)
  n_prediction <- sum(df$draft_class_type == "prediction", na.rm = TRUE)

  expect_equal(n_training,   621L)
  expect_equal(n_prediction, 227L)
})

test_that("is_hit is NA for all prediction rows", {
  skip_if_not(file.exists(PATH_OUT_CSV), "Output CSV absent")
  df      <- readr::read_csv(PATH_OUT_CSV, show_col_types = FALSE)
  pred_df <- dplyr::filter(df, draft_class_type == "prediction")

  expect_true(all(is.na(pred_df$is_hit)))
})

test_that("is_hit is non-NA for training rows that have qualifying seasons", {
  skip_if_not(file.exists(PATH_OUT_CSV), "Output CSV absent")
  df       <- readr::read_csv(PATH_OUT_CSV, show_col_types = FALSE)
  train_df <- dplyr::filter(df, draft_class_type == "training",
                             !is.na(qualifying_seasons), qualifying_seasons > 0L)

  expect_false(anyNA(train_df$is_hit))
})

test_that("all four positions are present", {
  skip_if_not(file.exists(PATH_OUT_CSV), "Output CSV absent")
  df <- readr::read_csv(PATH_OUT_CSV, show_col_types = FALSE)

  expect_true(all(POSITIONS %in% df$position))
})

test_that("cfb_player_name has no NA or blank values", {
  skip_if_not(file.exists(PATH_OUT_CSV), "Output CSV absent")
  df <- readr::read_csv(PATH_OUT_CSV, show_col_types = FALSE)

  expect_false(anyNA(df$cfb_player_name))
  expect_false(any(trimws(df$cfb_player_name) == ""))
})

test_that("tier label columns contain only valid label values", {
  skip_if_not(file.exists(PATH_OUT_CSV), "Output CSV absent")
  df         <- readr::read_csv(PATH_OUT_CSV, show_col_types = FALSE)
  label_cols <- names(df)[endsWith(names(df), "_tier_label")]

  expect_true(length(label_cols) > 0L)

  for (col in label_cols) {
    non_na_vals <- df[[col]][!is.na(df[[col]])]
    invalid     <- setdiff(unique(non_na_vals), VALID_TIER_LABELS)
    expect_equal(
      length(invalid), 0L,
      label = glue::glue("{col}: unexpected labels: {paste(invalid, collapse=', ')}")
    )
  }
})

test_that("schema tag columns are present (s2_w14_v1)", {
  skip_if_not(file.exists(PATH_OUT_CSV), "Output CSV absent")
  df <- readr::read_csv(PATH_OUT_CSV, show_col_types = FALSE)

  schema_cols <- c("cfb_player_name", "position", "draft_year", "draft_class_type",
                   "score_v1", "score_final", "score_base", "score_enriched",
                   "is_hit", "is_training_player")

  missing <- setdiff(schema_cols, names(df))
  expect_equal(length(missing), 0L,
               label = glue::glue("Missing schema columns: {paste(missing, collapse=', ')}"))
})

test_that("2026 draft class is present in prediction rows", {
  skip_if_not(file.exists(PATH_OUT_CSV), "Output CSV absent")
  df    <- readr::read_csv(PATH_OUT_CSV, show_col_types = FALSE)
  r2026 <- dplyr::filter(df, draft_year == 2026L)

  expect_true(nrow(r2026) > 0L)
  expect_true(all(r2026$draft_class_type == "prediction"))
})


# ==============================================================================
# SECTION 5: Tier reference contracts
# ==============================================================================

test_that("tier reference exists and has expected columns", {
  skip_if_not(
    file.exists(PATH_TIER_REF),
    "s2_week14_tier_reference.csv not found -- run run_week14_scoring() first"
  )
  ref      <- readr::read_csv(PATH_TIER_REF, show_col_types = FALSE)
  req_cols <- c("position", "metric_name", "tier", "tier_label",
                "break_lower", "break_upper", "hit_rate", "n_in_bin", "stable")

  expect_true(all(req_cols %in% names(ref)))
})

test_that("tier reference hit_rate values are in [0, 1]", {
  skip_if_not(file.exists(PATH_TIER_REF), "Tier reference absent")
  ref <- readr::read_csv(PATH_TIER_REF, show_col_types = FALSE)

  expect_true(all(ref$hit_rate >= 0, na.rm = TRUE))
  expect_true(all(ref$hit_rate <= 1, na.rm = TRUE))
})

test_that("tier reference Tier 1 hit_rate >= Tier 5 hit_rate for each metric/position", {
  skip_if_not(file.exists(PATH_TIER_REF), "Tier reference absent")
  ref <- readr::read_csv(PATH_TIER_REF, show_col_types = FALSE)

  # For each position+metric that has both tiers 1 and 5, T1 >= T5
  check_df <- ref |>
    dplyr::filter(tier %in% c(1L, 5L)) |>
    dplyr::select(position, metric_name, tier, hit_rate) |>
    tidyr::pivot_wider(names_from = tier, values_from = hit_rate,
                       names_prefix = "tier_") |>
    dplyr::filter(!is.na(tier_1), !is.na(tier_5))

  if (nrow(check_df) > 0L) {
    expect_true(
      all(check_df$tier_1 >= check_df$tier_5),
      label = "Tier 1 hit_rate must be >= Tier 5 hit_rate for every metric/position"
    )
  }
})

test_that("tier reference all tier_label values are valid", {
  skip_if_not(file.exists(PATH_TIER_REF), "Tier reference absent")
  ref <- readr::read_csv(PATH_TIER_REF, show_col_types = FALSE)

  valid_ref_labels <- unname(TIER_LABELS)
  invalid <- setdiff(unique(ref$tier_label), valid_ref_labels)

  expect_equal(length(invalid), 0L,
               label = glue::glue("Invalid tier labels: {paste(invalid, collapse=', ')}"))
})

test_that("tier reference stable tiers have n_in_bin >= MIN_BIN_SIZE", {
  skip_if_not(file.exists(PATH_TIER_REF), "Tier reference absent")
  ref <- readr::read_csv(PATH_TIER_REF, show_col_types = FALSE)

  stable_rows <- dplyr::filter(ref, stable == TRUE)
  expect_true(all(stable_rows$n_in_bin >= MIN_BIN_SIZE))
})

test_that("tier reference covers all four positions", {
  skip_if_not(file.exists(PATH_TIER_REF), "Tier reference absent")
  ref <- readr::read_csv(PATH_TIER_REF, show_col_types = FALSE)

  expect_true(all(POSITIONS %in% ref$position))
})


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat("\n")
cat("=== Week 14 Test Suite -- Season 2 (s2_w14_v1) ===\n")
cat(glue::glue(
  "Sections: impute_with_medians | derive_empirical_tiers | ",
  "apply_tiers_to_column | output CSV | tier reference\n"
))
cat(glue::glue(
  "Output CSV contract: {if (file.exists(PATH_OUT_CSV)) 'ACTIVE' else 'SKIPPED (file absent)'}\n"
))
cat(glue::glue(
  "Tier reference contract: {if (file.exists(PATH_TIER_REF)) 'ACTIVE' else 'SKIPPED (file absent)'}\n"
))
cat("Run with: testthat::test_file(here::here('tests', 'test_season2_week14_functions.R'))\n\n")