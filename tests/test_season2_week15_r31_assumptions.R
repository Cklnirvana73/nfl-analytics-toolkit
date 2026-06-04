# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# R/31 Player Volume Allocation -- Assumption Tests
# File: tests/test_season2_week15_r31_assumptions.R
#
# PURPOSE
# -------
# Standalone assumption-validation script for R/31. Not a testthat suite.
# Runs against real R/31 output (data/season2_cache/s2_week15_player_volume_
# allocation.rds), emits PASS / WARN / FAIL per check, and writes a log to
# output/assumption_tests/assumptions_season2_week15_r31.txt.
#
# CHECKS
# ------
#   1. TARGET_SHARE_PRIORS sum to 1.0 (config-constant integrity)
#   2. RUSH_SHARE_PRIORS sum to 1.0 (config-constant integrity)
#   3. Talent multipliers respect the [TALENT_MULT_FLOOR, TALENT_MULT_CEILING]
#      cap; z-score distribution by position surfaced as informational
#   4. Per-team target and rush share sums fall within the soft-constraint
#      band [CONSTRAINT_LOWER, CONSTRAINT_UPPER] after enforcement
#   5. No degenerate share values: no NA, no NaN, no Inf, no negatives in
#      the base, adjusted, or post-constraint share columns
#
# DESIGN NOTES
# ------------
#   - z-scores in R/31 output reflect player position vs the full R/28
#     prospect pool (848 players), not the R/31 active-roster subset. The
#     subset distribution is expected to skew. CHECK 3 surfaces this as
#     informational and only fails on cap violations.
#   - Each check is wrapped in tryCatch so an unexpected error in one
#     check does not block subsequent checks; the unexpected error logs
#     as FAIL with the captured error message.
#
# USAGE
#   source(here::here("tests", "test_season2_week15_r31_assumptions.R"))
#
# Author: Christian K. LeBlanc
# Version: 1.0
# ==============================================================================

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(here)
library(glue)

# Source R/31 to load constants: TARGET_SHARE_PRIORS, RUSH_SHARE_PRIORS,
# TALENT_MULT_FLOOR / TALENT_MULT_CEILING, CONSTRAINT_LOWER / CONSTRAINT_UPPER
source(here::here("R", "31_player_volume_allocation.R"))

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------

ALLOC_OUTPUT_RDS <- here::here(
  "data", "season2_cache", "s2_week15_player_volume_allocation.rds"
)

LOG_DIR_R31  <- here::here("output", "assumption_tests")
LOG_FILE_R31 <- file.path(LOG_DIR_R31, "assumptions_season2_week15_r31.txt")

# Tolerance bands for prior-sum integrity. These are config constants that
# should be exact, so PASS is tight.
PRIOR_SUM_TOL_PASS <- 0.001
PRIOR_SUM_TOL_WARN <- 0.01

dir.create(LOG_DIR_R31, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# LOGGING / STATE
# ------------------------------------------------------------------------------

.r31_state <- new.env(parent = emptyenv())
.r31_state$log_lines <- character(0)
.r31_state$results   <- list()

log_line_r31 <- function(...) {
  msg <- paste0(...)
  message(msg)
  .r31_state$log_lines <- c(.r31_state$log_lines, msg)
}

log_header_r31 <- function(title) {
  log_line_r31(strrep("=", 70))
  log_line_r31(title)
  log_line_r31(strrep("=", 70))
}

log_blank_r31 <- function() {
  log_line_r31("")
}

record_result_r31 <- function(check_name, status, details = NULL) {
  .r31_state$results[[check_name]] <- list(
    status  = status,
    details = details
  )
}

run_check_r31 <- function(check_name, check_fn) {
  tryCatch(
    check_fn(),
    error = function(e) {
      log_line_r31(glue(
        "  UNEXPECTED ERROR: {e$message}"
      ))
      log_line_r31("  Result: FAIL")
      log_blank_r31()
      record_result_r31(check_name, "FAIL",
                         list(error = e$message))
    }
  )
}

# ------------------------------------------------------------------------------
# CHECK 1: TARGET_SHARE_PRIORS sum to 1.0
# ------------------------------------------------------------------------------

check_target_priors_sum <- function() {
  log_header_r31("CHECK 1: TARGET_SHARE_PRIORS sum to 1.0")

  computed_sum <- sum(TARGET_SHARE_PRIORS, na.rm = TRUE)
  deviation    <- abs(computed_sum - 1.0)

  status <- if (deviation <= PRIOR_SUM_TOL_PASS) {
    "PASS"
  } else if (deviation <= PRIOR_SUM_TOL_WARN) {
    "WARN"
  } else {
    "FAIL"
  }

  log_line_r31(glue(
    "  Computed sum:    {format(round(computed_sum, 6), nsmall = 6)}"
  ))
  log_line_r31("  Expected:        1.000000")
  log_line_r31(glue(
    "  |deviation|:     {format(round(deviation, 6), nsmall = 6)}"
  ))
  log_line_r31(glue(
    "  PASS threshold:  {format(PRIOR_SUM_TOL_PASS, nsmall = 3)}"
  ))
  log_line_r31(glue("  Result:          {status}"))
  log_blank_r31()

  # Per-position breakdown for the log
  wr_sum <- sum(TARGET_SHARE_PRIORS[grepl("^WR", names(TARGET_SHARE_PRIORS))])
  te_sum <- sum(TARGET_SHARE_PRIORS[grepl("^TE", names(TARGET_SHARE_PRIORS))])
  rb_sum <- sum(TARGET_SHARE_PRIORS[grepl("^RB", names(TARGET_SHARE_PRIORS))])
  qb_sum <- sum(TARGET_SHARE_PRIORS[grepl("^QB", names(TARGET_SHARE_PRIORS))])

  log_line_r31("  Breakdown by position group:")
  log_line_r31(glue("    WR roles: {format(round(wr_sum, 4), nsmall = 4)}"))
  log_line_r31(glue("    TE roles: {format(round(te_sum, 4), nsmall = 4)}"))
  log_line_r31(glue("    RB roles: {format(round(rb_sum, 4), nsmall = 4)}"))
  log_line_r31(glue("    QB roles: {format(round(qb_sum, 4), nsmall = 4)}"))
  log_blank_r31()

  record_result_r31("target_priors_sum", status, list(
    computed  = computed_sum,
    deviation = deviation
  ))
}

# ------------------------------------------------------------------------------
# CHECK 2: RUSH_SHARE_PRIORS sum to 1.0
# ------------------------------------------------------------------------------

check_rush_priors_sum <- function() {
  log_header_r31("CHECK 2: RUSH_SHARE_PRIORS sum to 1.0")

  computed_sum <- sum(RUSH_SHARE_PRIORS, na.rm = TRUE)
  deviation    <- abs(computed_sum - 1.0)

  status <- if (deviation <= PRIOR_SUM_TOL_PASS) {
    "PASS"
  } else if (deviation <= PRIOR_SUM_TOL_WARN) {
    "WARN"
  } else {
    "FAIL"
  }

  log_line_r31(glue(
    "  Computed sum:    {format(round(computed_sum, 6), nsmall = 6)}"
  ))
  log_line_r31("  Expected:        1.000000")
  log_line_r31(glue(
    "  |deviation|:     {format(round(deviation, 6), nsmall = 6)}"
  ))
  log_line_r31(glue(
    "  PASS threshold:  {format(PRIOR_SUM_TOL_PASS, nsmall = 3)}"
  ))
  log_line_r31(glue("  Result:          {status}"))
  log_blank_r31()

  qb_sum <- sum(RUSH_SHARE_PRIORS[grepl("^QB", names(RUSH_SHARE_PRIORS))])
  rb_sum <- sum(RUSH_SHARE_PRIORS[grepl("^RB", names(RUSH_SHARE_PRIORS))])

  log_line_r31("  Breakdown by position group:")
  log_line_r31(glue("    QB scramble: {format(round(qb_sum, 4), nsmall = 4)}"))
  log_line_r31(glue("    RB carries:  {format(round(rb_sum, 4), nsmall = 4)}"))
  log_blank_r31()

  record_result_r31("rush_priors_sum", status, list(
    computed  = computed_sum,
    deviation = deviation
  ))
}

# ------------------------------------------------------------------------------
# CHECK 3: Talent multipliers respect cap; z-score distribution surfaced
# ------------------------------------------------------------------------------

check_talent_multipliers <- function() {
  log_header_r31(
    "CHECK 3: Talent multipliers respect cap [TALENT_MULT_FLOOR, TALENT_MULT_CEILING]"
  )

  if (!file.exists(ALLOC_OUTPUT_RDS)) {
    log_line_r31("  ALLOC output RDS not found at:")
    log_line_r31(glue("    {ALLOC_OUTPUT_RDS}"))
    log_line_r31("  Result: SKIP -- run R/31 first")
    log_blank_r31()
    record_result_r31("talent_multipliers", "SKIP")
    return(invisible())
  }

  alloc <- readRDS(ALLOC_OUTPUT_RDS)

  required_cols <- c("score_final", "talent_z", "talent_multiplier", "position")
  missing <- setdiff(required_cols, names(alloc))
  if (length(missing) > 0L) {
    log_line_r31(glue(
      "  Missing expected columns: {paste(missing, collapse = ', ')}"
    ))
    log_line_r31("  Result: FAIL")
    log_blank_r31()
    record_result_r31("talent_multipliers", "FAIL",
                       list(missing = missing))
    return(invisible())
  }

  # Restrict to players who actually had an R/28 score. Players with NA
  # score_final get talent_multiplier = 1.0 in R/31 by design, which would
  # bias the cap check downward and the z-score summary toward 0.
  with_scores <- alloc %>% dplyr::filter(!is.na(.data$score_final))

  n_with_scores   <- nrow(with_scores)
  n_below_floor   <- sum(
    with_scores$talent_multiplier < TALENT_MULT_FLOOR,
    na.rm = TRUE
  )
  n_above_ceiling <- sum(
    with_scores$talent_multiplier > TALENT_MULT_CEILING,
    na.rm = TRUE
  )

  log_line_r31(glue("  Players with R/28 score: {n_with_scores}"))
  log_line_r31(glue(
    "  Cap floor:               {format(TALENT_MULT_FLOOR, nsmall = 2)}"
  ))
  log_line_r31(glue(
    "  Cap ceiling:             {format(TALENT_MULT_CEILING, nsmall = 2)}"
  ))
  log_line_r31(glue("  Below floor:             {n_below_floor}"))
  log_line_r31(glue("  Above ceiling:           {n_above_ceiling}"))

  if (n_with_scores > 0L) {
    log_line_r31(glue(
      "  Min multiplier:          {format(round(min(with_scores$talent_multiplier, na.rm = TRUE), 4), nsmall = 4)}"
    ))
    log_line_r31(glue(
      "  Max multiplier:          {format(round(max(with_scores$talent_multiplier, na.rm = TRUE), 4), nsmall = 4)}"
    ))
  }

  status_cap <- if (n_below_floor == 0L && n_above_ceiling == 0L) {
    "PASS"
  } else {
    "FAIL"
  }

  log_line_r31(glue("  Cap respected:           {status_cap}"))
  log_blank_r31()

  # Informational: z-score distribution by position. NOT pass/fail because
  # z-scores were computed against the full R/28 prospect pool (848 players)
  # while R/31's output is the active-roster subset.
  log_line_r31("  Z-score distribution by position (informational):")

  if (n_with_scores > 0L) {
    z_stats <- with_scores %>%
      dplyr::group_by(.data$position) %>%
      dplyr::summarise(
        n      = dplyr::n(),
        z_mean = mean(.data$talent_z, na.rm = TRUE),
        z_sd   = stats::sd(.data$talent_z, na.rm = TRUE),
        z_min  = min(.data$talent_z, na.rm = TRUE),
        z_max  = max(.data$talent_z, na.rm = TRUE),
        .groups = "drop"
      )

    for (i in seq_len(nrow(z_stats))) {
      r <- z_stats[i, ]
      sd_str <- if (is.na(r$z_sd)) "NA" else {
        format(round(r$z_sd, 3), nsmall = 3)
      }
      log_line_r31(glue(
        "    {r$position}: n={r$n}, ",
        "mean={format(round(r$z_mean, 3), nsmall = 3)}, ",
        "sd={sd_str}, ",
        "range=[{format(round(r$z_min, 2), nsmall = 2)}, ",
        "{format(round(r$z_max, 2), nsmall = 2)}]"
      ))
    }
  } else {
    log_line_r31("    (no players with R/28 score in output -- nothing to report)")
  }

  log_line_r31(
    "  NOTE: z-scores were computed against the full R/28 prospect pool."
  )
  log_line_r31(
    "  The active-roster subset is expected to skew (design gap noted)."
  )
  log_blank_r31()

  record_result_r31("talent_multipliers", status_cap, list(
    n_with_scores = n_with_scores,
    n_below       = n_below_floor,
    n_above       = n_above_ceiling
  ))
}

# ------------------------------------------------------------------------------
# CHECK 4: Per-team share sums within soft-constraint band after enforcement
# ------------------------------------------------------------------------------

check_team_share_sums <- function() {
  log_header_r31(
    "CHECK 4: Per-team share sums within [CONSTRAINT_LOWER, CONSTRAINT_UPPER]"
  )

  if (!file.exists(ALLOC_OUTPUT_RDS)) {
    log_line_r31("  ALLOC output RDS not found.")
    log_line_r31("  Result: SKIP -- run R/31 first")
    log_blank_r31()
    record_result_r31("team_share_sums", "SKIP")
    return(invisible())
  }

  alloc <- readRDS(ALLOC_OUTPUT_RDS)

  required_cols <- c(
    "team",
    "target_share_adjusted", "rush_share_adjusted",
    "target_share", "rush_share"
  )
  missing <- setdiff(required_cols, names(alloc))
  if (length(missing) > 0L) {
    log_line_r31(glue(
      "  Missing expected columns: {paste(missing, collapse = ', ')}"
    ))
    log_line_r31("  Result: FAIL")
    log_blank_r31()
    record_result_r31("team_share_sums", "FAIL", list(missing = missing))
    return(invisible())
  }

  team_sums <- alloc %>%
    dplyr::group_by(.data$team) %>%
    dplyr::summarise(
      target_sum_adj  = sum(.data$target_share_adjusted, na.rm = TRUE),
      target_sum_post = sum(.data$target_share, na.rm = TRUE),
      rush_sum_adj    = sum(.data$rush_share_adjusted, na.rm = TRUE),
      rush_sum_post   = sum(.data$rush_share, na.rm = TRUE),
      .groups         = "drop"
    )

  log_line_r31(glue(
    "  Soft constraint band: [{CONSTRAINT_LOWER}, {CONSTRAINT_UPPER}]"
  ))
  log_line_r31(glue("  Active teams summed:  {nrow(team_sums)}"))
  log_blank_r31()

  # Informational: pre-constraint adjusted distribution
  log_line_r31("  Pre-constraint (adjusted) target_share sums:")
  log_line_r31(glue(
    "    min={format(round(min(team_sums$target_sum_adj), 4), nsmall = 4)}, ",
    "median={format(round(stats::median(team_sums$target_sum_adj), 4), nsmall = 4)}, ",
    "max={format(round(max(team_sums$target_sum_adj), 4), nsmall = 4)}"
  ))

  log_line_r31("  Pre-constraint (adjusted) rush_share sums:")
  log_line_r31(glue(
    "    min={format(round(min(team_sums$rush_sum_adj), 4), nsmall = 4)}, ",
    "median={format(round(stats::median(team_sums$rush_sum_adj), 4), nsmall = 4)}, ",
    "max={format(round(max(team_sums$rush_sum_adj), 4), nsmall = 4)}"
  ))
  log_blank_r31()

  # Pass/fail: post-constraint sums must fall within the band
  target_out_of_band <- team_sums %>%
    dplyr::filter(
      .data$target_sum_post < CONSTRAINT_LOWER |
        .data$target_sum_post > CONSTRAINT_UPPER
    )
  rush_out_of_band <- team_sums %>%
    dplyr::filter(
      .data$rush_sum_post < CONSTRAINT_LOWER |
        .data$rush_sum_post > CONSTRAINT_UPPER
    )

  n_target_out <- nrow(target_out_of_band)
  n_rush_out   <- nrow(rush_out_of_band)

  log_line_r31("  Post-constraint target_share sums:")
  log_line_r31(glue(
    "    min={format(round(min(team_sums$target_sum_post), 4), nsmall = 4)}, ",
    "max={format(round(max(team_sums$target_sum_post), 4), nsmall = 4)}"
  ))
  log_line_r31(glue("    teams outside band: {n_target_out}"))
  if (n_target_out > 0L) {
    for (i in seq_len(nrow(target_out_of_band))) {
      r <- target_out_of_band[i, ]
      log_line_r31(glue(
        "      {r$team}: {format(round(r$target_sum_post, 4), nsmall = 4)}"
      ))
    }
  }
  log_blank_r31()

  log_line_r31("  Post-constraint rush_share sums:")
  log_line_r31(glue(
    "    min={format(round(min(team_sums$rush_sum_post), 4), nsmall = 4)}, ",
    "max={format(round(max(team_sums$rush_sum_post), 4), nsmall = 4)}"
  ))
  log_line_r31(glue("    teams outside band: {n_rush_out}"))
  if (n_rush_out > 0L) {
    for (i in seq_len(nrow(rush_out_of_band))) {
      r <- rush_out_of_band[i, ]
      log_line_r31(glue(
        "      {r$team}: {format(round(r$rush_sum_post, 4), nsmall = 4)}"
      ))
    }
  }
  log_blank_r31()

  status <- if (n_target_out == 0L && n_rush_out == 0L) "PASS" else "FAIL"
  log_line_r31(glue("  Result: {status}"))
  log_blank_r31()

  record_result_r31("team_share_sums", status, list(
    target_out = n_target_out,
    rush_out   = n_rush_out
  ))
}

# ------------------------------------------------------------------------------
# CHECK 5: No degenerate share values
# ------------------------------------------------------------------------------

check_no_degenerate_shares <- function() {
  log_header_r31("CHECK 5: No degenerate share values")

  if (!file.exists(ALLOC_OUTPUT_RDS)) {
    log_line_r31("  ALLOC output RDS not found.")
    log_line_r31("  Result: SKIP -- run R/31 first")
    log_blank_r31()
    record_result_r31("no_degenerate", "SKIP")
    return(invisible())
  }

  alloc <- readRDS(ALLOC_OUTPUT_RDS)

  share_cols <- c(
    "target_share_base", "target_share_adjusted", "target_share",
    "rush_share_base", "rush_share_adjusted", "rush_share",
    "expected_targets_pg", "expected_carries_pg"
  )

  missing <- setdiff(share_cols, names(alloc))
  if (length(missing) > 0L) {
    log_line_r31(glue(
      "  Missing expected columns: {paste(missing, collapse = ', ')}"
    ))
    log_line_r31("  Result: FAIL")
    log_blank_r31()
    record_result_r31("no_degenerate", "FAIL", list(missing = missing))
    return(invisible())
  }

  log_line_r31(glue("  Rows checked: {nrow(alloc)}"))
  log_blank_r31()

  any_fail <- FALSE
  col_summaries <- list()

  for (col in share_cols) {
    vec <- alloc[[col]]
    n_na  <- sum(is.na(vec))
    n_inf <- sum(is.infinite(vec))
    n_nan <- sum(is.nan(vec))
    n_neg <- sum(vec < 0, na.rm = TRUE)

    col_status <- if (n_na == 0L && n_inf == 0L &&
                       n_nan == 0L && n_neg == 0L) {
      "PASS"
    } else {
      "FAIL"
    }
    if (col_status == "FAIL") any_fail <- TRUE

    log_line_r31(glue(
      "  {col}: NA={n_na}, Inf={n_inf}, NaN={n_nan}, neg={n_neg} -- {col_status}"
    ))

    col_summaries[[col]] <- list(
      n_na = n_na, n_inf = n_inf, n_nan = n_nan, n_neg = n_neg
    )
  }

  log_blank_r31()
  status <- if (any_fail) "FAIL" else "PASS"
  log_line_r31(glue("  Result: {status}"))
  log_blank_r31()

  record_result_r31("no_degenerate", status, col_summaries)
}

# ==============================================================================
# RUN ALL CHECKS
# ==============================================================================

log_header_r31("R/31 Player Volume Allocation -- Assumption Tests")
log_line_r31(glue(
  "Generated: {format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')}"
))
log_line_r31(glue("Source script: R/31_player_volume_allocation.R"))
log_line_r31(glue("ALLOC output:  {ALLOC_OUTPUT_RDS}"))
log_blank_r31()

run_check_r31("target_priors_sum",  check_target_priors_sum)
run_check_r31("rush_priors_sum",    check_rush_priors_sum)
run_check_r31("talent_multipliers", check_talent_multipliers)
run_check_r31("team_share_sums",    check_team_share_sums)
run_check_r31("no_degenerate",      check_no_degenerate_shares)

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------

log_header_r31("SUMMARY")

n_total <- length(.r31_state$results)
n_pass  <- sum(vapply(.r31_state$results,
                       function(r) r$status == "PASS",
                       logical(1)))
n_warn  <- sum(vapply(.r31_state$results,
                       function(r) r$status == "WARN",
                       logical(1)))
n_fail  <- sum(vapply(.r31_state$results,
                       function(r) r$status == "FAIL",
                       logical(1)))
n_skip  <- sum(vapply(.r31_state$results,
                       function(r) r$status == "SKIP",
                       logical(1)))

log_line_r31(glue("  Total checks:  {n_total}"))
log_line_r31(glue("  PASS:          {n_pass}"))
log_line_r31(glue("  WARN:          {n_warn}"))
log_line_r31(glue("  FAIL:          {n_fail}"))
log_line_r31(glue("  SKIP:          {n_skip}"))

# Per-check status
log_blank_r31()
log_line_r31("  By check:")
for (check_name in names(.r31_state$results)) {
  log_line_r31(glue(
    "    {check_name}: {.r31_state$results[[check_name]]$status}"
  ))
}

log_line_r31(strrep("=", 70))

# ------------------------------------------------------------------------------
# WRITE LOG TO FILE
# ------------------------------------------------------------------------------

writeLines(.r31_state$log_lines, LOG_FILE_R31)
message(glue("\nAssumption test log written to:\n  {LOG_FILE_R31}"))
