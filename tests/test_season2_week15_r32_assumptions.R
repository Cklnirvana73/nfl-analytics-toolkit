# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# R/32 Projection Reconciliation -- Assumption Tests
# File: tests/test_season2_week15_r32_assumptions.R
#
# PURPOSE
# -------
# Standalone assumption-validation script for R/32. Not a testthat suite.
# Emits PASS / WARN / FAIL per check and writes a log to
# output/assumption_tests/assumptions_season2_week15_r32.txt.
#
# CHECKS
# ------
#   1. POSITION_EFFICIENCY priors (catch_rate, yards_per_catch, td_per_target
#      for WR / TE / RB; yards_per_carry and td_per_carry for RB) validated
#      against 2023-2025 league averages derived from R/15 pbp cache.
#      Tolerances: 10% relative for volume metrics, 20% for TD rates.
#
#   2. SACK_ADJUSTMENT = 0.935 consistent with empirical sack rate from
#      the same 2023-2025 pbp window.
#      Tolerances: |empirical - 0.935| <= 0.015 pp = PASS,
#                                         <= 0.025 pp = WARN, else FAIL.
#
#   3. Blend mechanics produce corrections in the expected direction:
#      score_final_fallback players should show larger median absolute
#      delta from R/29 than blended players. Monotonic ordering across
#      all tiers in BLEND_WEIGHTS_BY_PRIOR_SOURCE is also checked.
#
# DESIGN NOTES
# ------------
#   - CHECKs 1 and 2 share one iterative pbp load pass (2023-2025).
#     Counts are accumulated season-by-season with rm / gc between loads.
#     Means are computed once at the end from totals to give play-weighted
#     league averages rather than season-weighted averages.
#   - Roster join uses nflreadr::load_rosters() keyed on gsis_id per season.
#     Most-recent week per player-season is used as the position anchor.
#   - receiver_player_id is the expected target column (nflfastR standard).
#     A guard checks for the alternate name receiver_id and falls back.
#   - CHECK 3 requires data/season2_cache/s2_week15_reconciled_projections.rds
#     from R/32. Run reconcile_projections() before this script.
#
# USAGE
#   source(here::here("tests", "test_season2_week15_r32_assumptions.R"))
#
# Author: Christian K. LeBlanc
# Version: 1.0
# ==============================================================================

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(here)
library(glue)
library(nflreadr)

# Sourcing R/32 brings in R/31, R/30, R/15. This makes load_normalized_season(),
# POSITION_EFFICIENCY, SACK_ADJUSTMENT, BLEND_WEIGHTS_BY_PRIOR_SOURCE,
# SEASON_RECON, and RECON_POSITIONS all available.
source(here::here("R", "32_projection_reconciliation.R"))

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------

# 2023-2025: matches R/30's HISTORICAL_SEASONS exactly
EFFICIENCY_SEASONS <- (SEASON_RECON - 3L):(SEASON_RECON - 1L)

CACHE_DIR_R32      <- here::here("data", "season2_cache")
RECON_OUTPUT_RDS   <- here::here(
  "data", "season2_cache", "s2_week15_reconciled_projections.rds"
)

LOG_DIR_R32  <- here::here("output", "assumption_tests")
LOG_FILE_R32 <- file.path(
  LOG_DIR_R32, "assumptions_season2_week15_r32.txt"
)

# Relative deviation tolerance bands
# Volume metrics (catch_rate, yards_per_catch, yards_per_carry):
#   PASS <= 10%, WARN <= 20%, FAIL > 20%
# TD rate metrics (lower count, higher variance):
#   PASS <= 20%, WARN <= 35%, FAIL > 35%
METRIC_TOLERANCES <- list(
  catch_rate      = list(pass = 0.10, warn = 0.20),
  yards_per_catch = list(pass = 0.10, warn = 0.20),
  td_per_target   = list(pass = 0.20, warn = 0.35),
  yards_per_carry = list(pass = 0.10, warn = 0.20),
  td_per_carry    = list(pass = 0.20, warn = 0.35)
)

# Sack adjustment: absolute pp deviation from 0.935
SACK_ADJ_TOL_PASS <- 0.015
SACK_ADJ_TOL_WARN <- 0.025

dir.create(LOG_DIR_R32, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# LOGGING / STATE
# ------------------------------------------------------------------------------

.r32_state <- new.env(parent = emptyenv())
.r32_state$log_lines <- character(0)
.r32_state$results   <- list()

log_line_r32 <- function(...) {
  msg <- paste0(...)
  message(msg)
  .r32_state$log_lines <- c(.r32_state$log_lines, msg)
}

log_header_r32 <- function(title) {
  log_line_r32(strrep("=", 70))
  log_line_r32(title)
  log_line_r32(strrep("=", 70))
}

log_blank_r32 <- function() log_line_r32("")

record_result_r32 <- function(check_name, status, details = NULL) {
  .r32_state$results[[check_name]] <- list(
    status  = status,
    details = details
  )
}

run_check_r32 <- function(check_name, check_fn) {
  tryCatch(
    check_fn(),
    error = function(e) {
      log_line_r32(glue("  UNEXPECTED ERROR: {e$message}"))
      log_line_r32("  Result: FAIL")
      log_blank_r32()
      record_result_r32(check_name, "FAIL", list(error = e$message))
    }
  )
}

# ------------------------------------------------------------------------------
# METRIC CHECK HELPER
# ------------------------------------------------------------------------------

# Computes relative deviation of empirical from prior, logs the comparison,
# and returns "PASS", "WARN", "FAIL", or "SKIP".
.check_metric_r32 <- function(label, prior, empirical, tol) {
  if (is.na(empirical) || is.nan(empirical) || is.infinite(empirical)) {
    log_line_r32(glue(
      "    {label}: prior={format(round(prior, 4), nsmall = 4)}, ",
      "empirical=NA -- SKIP"
    ))
    return("SKIP")
  }

  rel_dev <- abs(empirical - prior) / prior

  status <- if (rel_dev <= tol$pass) "PASS" else
    if (rel_dev <= tol$warn)         "WARN" else "FAIL"

  log_line_r32(glue(
    "    {label}: ",
    "prior={format(round(prior, 4), nsmall = 4)}, ",
    "empirical={format(round(empirical, 4), nsmall = 4)}, ",
    "rel_dev={format(round(rel_dev * 100, 1), nsmall = 1)}% -- {status}"
  ))

  status
}

# ==============================================================================
# PBP AGGREGATION (shared by CHECKs 1 and 2)
# ==============================================================================

#' Load and aggregate efficiency and sack data across seasons
#'
#' Iterates over seasons. For each: filters pbp to REG plays excluding
#' kneels/spikes/two-point attempts, joins receivers and rushers to season
#' rosters for position, accumulates target/completion/yard/TD counts for
#' WR/TE/RB, accumulates RB carry/yard/TD counts, and accumulates total
#' dropback and sack counts. rm/gc between seasons.
#'
#' @param seasons Integer vector.
#' @param cache_dir Character.
#' @return Named list: receiving (tibble by position), rushing (tibble),
#'   sacks (list with n_dropbacks/n_sacks). NULL if load fails for all seasons.
.load_pbp_efficiency_aggregates <- function(seasons, cache_dir) {

  message(glue(
    "  Loading {length(seasons)} season(s) of pbp for efficiency baseline: ",
    "{paste(seasons, collapse = ', ')}"
  ))

  # Load rosters for all seasons at once (small -- safe to hold in memory)
  rosters_raw <- tryCatch(
    nflreadr::load_rosters(seasons = seasons),
    error = function(e) {
      message(glue("  load_rosters() failed: {e$message}"))
      NULL
    }
  )

  if (is.null(rosters_raw) || nrow(rosters_raw) == 0L) {
    message("  No roster data -- cannot join receiver/rusher positions.")
    return(NULL)
  }

  rosters <- rosters_raw %>%
    dplyr::filter(!is.na(.data$gsis_id)) %>%
    dplyr::group_by(.data$gsis_id, .data$season) %>%
    dplyr::slice_max(.data$week, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(gsis_id, season, position)

  # Season-level count accumulators
  recv_acc <- tibble::tibble(
    position      = character(),
    n_targets     = integer(),
    n_completions = integer(),
    rec_yards     = numeric(),
    rec_tds       = integer()
  )
  rush_acc <- tibble::tibble(
    n_carries  = integer(),
    rush_yards = numeric(),
    rush_tds   = integer()
  )
  sack_acc <- list(n_dropbacks = 0L, n_sacks = 0L)

  for (s in seasons) {

    pbp <- tryCatch(
      load_normalized_season(s, cache_dir = cache_dir),
      error = function(e) {
        message(glue("  Season {s}: load failed -- {e$message}"))
        NULL
      }
    )
    if (is.null(pbp) || nrow(pbp) == 0L) next

    # Core filter: REG, no kneels/spikes/two-point attempts
    pbp_reg <- pbp %>%
      dplyr::filter(
        .data$season_type == "REG",
        dplyr::coalesce(.data$qb_kneel,          0L) == 0L,
        dplyr::coalesce(.data$qb_spike,           0L) == 0L,
        dplyr::coalesce(.data$two_point_attempt,  0L) == 0L
      )

    season_roster <- rosters %>%
      dplyr::filter(.data$season == s) %>%
      dplyr::select(gsis_id, position)

    # ---- Receiving plays ----

    recv_id_col <- if ("receiver_player_id" %in% names(pbp_reg)) {
      "receiver_player_id"
    } else if ("receiver_id" %in% names(pbp_reg)) {
      "receiver_id"
    } else {
      NA_character_
    }

    if (!is.na(recv_id_col)) {

      recv_plays <- pbp_reg %>%
        dplyr::filter(
          dplyr::coalesce(.data$pass, 0L) == 1L,
          !is.na(.data[[recv_id_col]])
        ) %>%
        dplyr::left_join(season_roster,
                          by = setNames("gsis_id", recv_id_col)) %>%
        dplyr::filter(.data$position %in% c("WR", "TE", "RB"))

      season_recv <- recv_plays %>%
        dplyr::group_by(.data$position) %>%
        dplyr::summarise(
          n_targets     = dplyr::n(),
          n_completions = sum(dplyr::coalesce(.data$complete_pass,   0L)),
          rec_yards     = sum(
            dplyr::coalesce(.data$yards_gained, 0) *
              dplyr::coalesce(.data$complete_pass, 0L)
          ),
          rec_tds       = sum(
            dplyr::coalesce(.data$pass_touchdown, 0L) *
              dplyr::coalesce(.data$complete_pass, 0L)
          ),
          .groups = "drop"
        )

      recv_acc <- dplyr::bind_rows(recv_acc, season_recv)
      rm(recv_plays)

    } else {
      message(glue(
        "  Season {s}: receiver target column not found -- ",
        "receiving aggregation skipped"
      ))
    }

    # ---- Rush plays (RB carries only) ----
    # Sack plays have rush == 1 but rusher_player_id is NA (attributed to
    # passer in nflfastR). Filtering !is.na(rusher_player_id) removes them.
    rush_plays <- pbp_reg %>%
      dplyr::filter(
        dplyr::coalesce(.data$rush, 0L) == 1L,
        !is.na(.data$rusher_player_id)
      ) %>%
      dplyr::left_join(season_roster,
                        by = c("rusher_player_id" = "gsis_id")) %>%
      dplyr::filter(.data$position == "RB")

    season_rush <- rush_plays %>%
      dplyr::summarise(
        n_carries  = dplyr::n(),
        rush_yards = sum(dplyr::coalesce(.data$yards_gained,    0)),
        rush_tds   = sum(dplyr::coalesce(.data$rush_touchdown,  0L))
      )

    rush_acc <- dplyr::bind_rows(rush_acc, season_rush)
    rm(rush_plays)

    # ---- Sack / dropback counts ----
    dropback_plays <- pbp_reg %>%
      dplyr::filter(dplyr::coalesce(.data$qb_dropback, 0L) == 1L)

    sack_acc$n_dropbacks <- sack_acc$n_dropbacks + nrow(dropback_plays)
    sack_acc$n_sacks     <- sack_acc$n_sacks +
      sum(dplyr::coalesce(dropback_plays$sack, 0L))

    rm(pbp, pbp_reg, dropback_plays)
    invisible(gc(verbose = FALSE))

    message(glue("  Season {s}: counts accumulated"))
  }

  if (nrow(recv_acc) == 0L && nrow(rush_acc) == 0L) {
    message("  All season loads failed -- no data accumulated.")
    return(NULL)
  }

  # Aggregate across seasons (sum of counts, not mean of rates)
  receiving_totals <- recv_acc %>%
    dplyr::group_by(.data$position) %>%
    dplyr::summarise(
      n_targets     = sum(.data$n_targets),
      n_completions = sum(.data$n_completions),
      rec_yards     = sum(.data$rec_yards),
      rec_tds       = sum(.data$rec_tds),
      .groups = "drop"
    )

  rush_totals <- rush_acc %>%
    dplyr::summarise(
      n_carries  = sum(.data$n_carries),
      rush_yards = sum(.data$rush_yards),
      rush_tds   = sum(.data$rush_tds)
    )

  message(glue(
    "  Aggregation complete: ",
    "{sum(receiving_totals$n_targets, na.rm = TRUE)} receiving targets, ",
    "{rush_totals$n_carries} RB carries, ",
    "{sack_acc$n_dropbacks} dropbacks"
  ))

  list(receiving = receiving_totals,
       rushing   = rush_totals,
       sacks     = sack_acc)
}

# ==============================================================================
# CHECK 1: POSITION_EFFICIENCY priors vs 2023-2025 pbp baseline
# ==============================================================================

check_efficiency_priors <- function(pbp_agg) {
  log_header_r32("CHECK 1: POSITION_EFFICIENCY priors vs 2023-2025 pbp baseline")

  if (is.null(pbp_agg)) {
    log_line_r32("  PBP aggregates unavailable (pbp load failed).")
    log_line_r32("  Result: SKIP")
    log_blank_r32()
    record_result_r32("efficiency_priors", "SKIP")
    return(invisible())
  }

  receiving <- pbp_agg$receiving
  rushing   <- pbp_agg$rushing

  log_line_r32(glue(
    "  Baseline window: {paste(EFFICIENCY_SEASONS, collapse = ', ')}"
  ))
  log_line_r32(glue(
    "  Tolerance bands: volume <=10%=PASS / <=20%=WARN; ",
    "TD rate <=20%=PASS / <=35%=WARN"
  ))
  log_blank_r32()

  any_fail <- FALSE
  any_warn <- FALSE

  for (pos in names(POSITION_EFFICIENCY)) {
    eff <- POSITION_EFFICIENCY[[pos]]

    recv_row <- receiving %>% dplyr::filter(.data$position == pos)

    if (nrow(recv_row) == 0L) {
      log_line_r32(glue("  {pos}: no pbp receiving data -- skipping"))
      log_blank_r32()
      next
    }

    empirical_catch_rate <- recv_row$n_completions / recv_row$n_targets
    empirical_ypc <- if (recv_row$n_completions > 0L) {
      recv_row$rec_yards / recv_row$n_completions
    } else NA_real_
    empirical_td_target  <- recv_row$rec_tds / recv_row$n_targets

    log_line_r32(glue(
      "  {pos} RECEIVING  (targets={format(recv_row$n_targets, big.mark = ',')}, ",
      "completions={format(recv_row$n_completions, big.mark = ',')}):"
    ))

    for (nm in c("catch_rate", "yards_per_catch", "td_per_target")) {
      prior_val <- eff[[nm]]
      emp_val   <- switch(nm,
        catch_rate      = empirical_catch_rate,
        yards_per_catch = empirical_ypc,
        td_per_target   = empirical_td_target
      )
      st <- .check_metric_r32(nm, prior_val, emp_val, METRIC_TOLERANCES[[nm]])
      if (st == "FAIL") any_fail <- TRUE
      if (st == "WARN") any_warn <- TRUE
    }

    # Rush metrics: RB only
    if (pos == "RB") {
      n_carries  <- rushing$n_carries
      empirical_ypc_carry <- if (n_carries > 0L) {
        rushing$rush_yards / n_carries
      } else NA_real_
      empirical_td_carry  <- if (n_carries > 0L) {
        rushing$rush_tds / n_carries
      } else NA_real_

      log_line_r32(glue(
        "  {pos} RUSHING   (carries={format(n_carries, big.mark = ',')}):"
      ))

      for (nm in c("yards_per_carry", "td_per_carry")) {
        prior_val <- eff[[nm]]
        emp_val   <- if (nm == "yards_per_carry") empirical_ypc_carry else
          empirical_td_carry
        st <- .check_metric_r32(nm, prior_val, emp_val, METRIC_TOLERANCES[[nm]])
        if (st == "FAIL") any_fail <- TRUE
        if (st == "WARN") any_warn <- TRUE
      }
    }

    log_blank_r32()
  }

  status <- if (any_fail) "FAIL" else if (any_warn) "WARN" else "PASS"
  log_line_r32(glue("  Overall result: {status}"))
  log_blank_r32()

  record_result_r32("efficiency_priors", status)
}

# ==============================================================================
# CHECK 2: SACK_ADJUSTMENT = 0.935 vs empirical sack rate
# ==============================================================================

check_sack_adjustment <- function(pbp_agg) {
  log_header_r32(
    "CHECK 2: SACK_ADJUSTMENT = 0.935 vs empirical 2023-2025 sack rate"
  )

  if (is.null(pbp_agg)) {
    log_line_r32("  PBP aggregates unavailable.")
    log_line_r32("  Result: SKIP")
    log_blank_r32()
    record_result_r32("sack_adjustment", "SKIP")
    return(invisible())
  }

  n_dropbacks <- pbp_agg$sacks$n_dropbacks
  n_sacks     <- pbp_agg$sacks$n_sacks

  if (n_dropbacks == 0L) {
    log_line_r32("  n_dropbacks = 0 -- cannot compute sack rate.")
    log_line_r32("  Result: SKIP")
    log_blank_r32()
    record_result_r32("sack_adjustment", "SKIP")
    return(invisible())
  }

  empirical_sack_rate <- n_sacks / n_dropbacks
  empirical_sack_adj  <- 1 - empirical_sack_rate
  deviation_pp        <- abs(empirical_sack_adj - SACK_ADJUSTMENT)

  status <- if (deviation_pp <= SACK_ADJ_TOL_PASS) "PASS" else
    if (deviation_pp <= SACK_ADJ_TOL_WARN)          "WARN" else "FAIL"

  log_line_r32(glue(
    "  Seasons: {paste(EFFICIENCY_SEASONS, collapse = ', ')}"
  ))
  log_line_r32(glue(
    "  Total dropbacks:               {format(n_dropbacks, big.mark = ',')}"
  ))
  log_line_r32(glue(
    "  Total sacks:                   {format(n_sacks, big.mark = ',')}"
  ))
  log_line_r32(glue(
    "  Empirical sack rate:           {format(round(empirical_sack_rate * 100, 2), nsmall = 2)}%"
  ))
  log_line_r32(glue(
    "  Empirical sack adj (1-rate):   {format(round(empirical_sack_adj, 4), nsmall = 4)}"
  ))
  log_line_r32(glue(
    "  Constant SACK_ADJUSTMENT:      {format(SACK_ADJUSTMENT, nsmall = 4)}"
  ))
  log_line_r32(glue(
    "  |deviation|:                   {format(round(deviation_pp * 100, 2), nsmall = 2)} pp"
  ))
  log_line_r32(glue(
    "  PASS threshold:                {SACK_ADJ_TOL_PASS * 100} pp"
  ))
  log_line_r32(glue(
    "  WARN threshold:                {SACK_ADJ_TOL_WARN * 100} pp"
  ))
  log_line_r32(glue("  Result:                        {status}"))
  log_blank_r32()

  record_result_r32("sack_adjustment", status, list(
    empirical_rate = empirical_sack_rate,
    empirical_adj  = empirical_sack_adj,
    deviation_pp   = deviation_pp
  ))
}

# ==============================================================================
# CHECK 3: Blend corrections in expected direction by prior_source tier
# ==============================================================================

check_blend_direction <- function() {
  log_header_r32(
    "CHECK 3: Blend corrections in expected direction by prior_source tier"
  )

  if (!file.exists(RECON_OUTPUT_RDS)) {
    log_line_r32("  Reconciled output RDS not found at:")
    log_line_r32(glue("    {RECON_OUTPUT_RDS}"))
    log_line_r32("  Result: SKIP -- run reconcile_projections() first")
    log_blank_r32()
    record_result_r32("blend_direction", "SKIP")
    return(invisible())
  }

  recon <- readRDS(RECON_OUTPUT_RDS)

  required <- c("prior_source", "r32_delta_from_r29",
                "blend_weight_r31", "position")
  missing <- setdiff(required, names(recon))
  if (length(missing) > 0L) {
    log_line_r32(glue(
      "  Missing columns: {paste(missing, collapse = ', ')}"
    ))
    log_line_r32("  Result: FAIL")
    log_blank_r32()
    record_result_r32("blend_direction", "FAIL", list(missing = missing))
    return(invisible())
  }

  # Restrict to RB/WR/TE: QBs have blend_weight_r31 = 0 by design
  rbwrte <- recon %>%
    dplyr::filter(
      .data$position %in% RECON_POSITIONS,
      !is.na(.data$r32_delta_from_r29),
      !is.na(.data$prior_source)
    )

  if (nrow(rbwrte) == 0L) {
    log_line_r32("  No RB/WR/TE rows with valid delta -- cannot evaluate.")
    log_line_r32("  Result: SKIP")
    log_blank_r32()
    record_result_r32("blend_direction", "SKIP")
    return(invisible())
  }

  # Median absolute delta and blend weight by prior_source
  tier_stats <- rbwrte %>%
    dplyr::group_by(.data$prior_source) %>%
    dplyr::summarise(
      n                = dplyr::n(),
      blend_weight     = mean(.data$blend_weight_r31, na.rm = TRUE),
      median_abs_delta = stats::median(abs(.data$r32_delta_from_r29),
                                        na.rm = TRUE),
      mean_abs_delta   = mean(abs(.data$r32_delta_from_r29), na.rm = TRUE),
      median_delta     = stats::median(.data$r32_delta_from_r29, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$blend_weight)

  log_line_r32(glue(
    "  RB/WR/TE rows evaluated: {nrow(rbwrte)}"
  ))
  log_blank_r32()
  log_line_r32(
    "  Blend-weight tier table (ascending blend_wt = trust R/31 less -> more):"
  )
  log_line_r32(glue(
    "  {formatC('prior_source', width = 28, flag = '-')} ",
    "{formatC('n', width = 5)} ",
    "{formatC('blend_wt', width = 9)} ",
    "{formatC('med|delta|', width = 11)} ",
    "{formatC('med_delta', width = 10)}"
  ))
  log_line_r32(strrep("-", 68))

  for (i in seq_len(nrow(tier_stats))) {
    r <- tier_stats[i, ]
    log_line_r32(glue(
      "  {formatC(r$prior_source, width = 28, flag = '-')} ",
      "{formatC(r$n, width = 5, format = 'd')} ",
      "{format(round(r$blend_weight, 2), nsmall = 2, width = 9)} ",
      "{format(round(r$median_abs_delta, 2), nsmall = 2, width = 11)} ",
      "{format(round(r$median_delta, 2), nsmall = 2, width = 10)}"
    ))
  }
  log_blank_r32()

  # Key assertion: the two anchor tiers maintain expected direction.
  # score_final_fallback (highest blend weight, 0.85) should produce the
  # largest absolute correction; blended (lowest, 0.30) the smallest.
  sfb     <- tier_stats %>% dplyr::filter(.data$prior_source == "score_final_fallback")
  blended <- tier_stats %>% dplyr::filter(.data$prior_source == "blended")

  if (nrow(sfb) == 0L || nrow(blended) == 0L) {
    log_line_r32(
      "  Cannot test anchor direction: score_final_fallback or blended tier absent."
    )
    log_line_r32("  Result: WARN (incomplete tier coverage)")
    log_blank_r32()
    record_result_r32("blend_direction", "WARN",
                       list(reason = "anchor tier absent from output"))
    return(invisible())
  }

  sfb_delta     <- sfb$median_abs_delta
  blended_delta <- blended$median_abs_delta
  direction_ok  <- sfb_delta > blended_delta

  log_line_r32(glue(
    "  Anchor check -- score_final_fallback med|delta|: ",
    "{format(round(sfb_delta, 2), nsmall = 2)}"
  ))
  log_line_r32(glue(
    "  Anchor check -- blended              med|delta|: ",
    "{format(round(blended_delta, 2), nsmall = 2)}"
  ))
  log_line_r32(glue(
    "  Direction correct (sfb > blended): {direction_ok}"
  ))
  log_blank_r32()

  # Secondary: monotonic ordering across all BLEND_WEIGHTS_BY_PRIOR_SOURCE tiers
  ordered_tiers  <- names(BLEND_WEIGHTS_BY_PRIOR_SOURCE)
  present_tiers  <- tier_stats %>%
    dplyr::filter(.data$prior_source %in% ordered_tiers) %>%
    dplyr::arrange(.data$blend_weight)

  n_violations <- 0L
  if (nrow(present_tiers) >= 2L) {
    deltas <- present_tiers$median_abs_delta
    for (j in 2:length(deltas)) {
      if (deltas[j] < deltas[j - 1L]) n_violations <- n_violations + 1L
    }
  }

  log_line_r32(glue(
    "  Monotonic ordering violations: {n_violations} ",
    "of {max(nrow(present_tiers) - 1L, 0L)} adjacent tier pair(s)"
  ))
  log_blank_r32()

  status <- if (!direction_ok) {
    "FAIL"
  } else if (n_violations > 0L) {
    "WARN"
  } else {
    "PASS"
  }

  log_line_r32(glue("  Result: {status}"))
  log_blank_r32()

  record_result_r32("blend_direction", status, list(
    direction_ok = direction_ok,
    sfb_delta    = sfb_delta,
    blended_delta = blended_delta,
    n_violations = n_violations
  ))
}

# ==============================================================================
# RUN ALL CHECKS
# ==============================================================================

log_header_r32("R/32 Projection Reconciliation -- Assumption Tests")
log_line_r32(glue(
  "Generated: {format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')}"
))
log_line_r32(glue("Source script: R/32_projection_reconciliation.R"))
log_line_r32(glue("Reconciled output: {RECON_OUTPUT_RDS}"))
log_line_r32(glue("Efficiency baseline: {paste(EFFICIENCY_SEASONS, collapse = ', ')}"))
log_blank_r32()

# Load pbp aggregates once -- shared by CHECK 1 and CHECK 2
log_line_r32("Loading pbp efficiency aggregates (shared by CHECKs 1 and 2)...")
log_blank_r32()
pbp_agg <- tryCatch(
  .load_pbp_efficiency_aggregates(EFFICIENCY_SEASONS, CACHE_DIR_R32),
  error = function(e) {
    message(glue("pbp aggregate load failed: {e$message}"))
    NULL
  }
)

run_check_r32("efficiency_priors", function() check_efficiency_priors(pbp_agg))
run_check_r32("sack_adjustment",   function() check_sack_adjustment(pbp_agg))
run_check_r32("blend_direction",   check_blend_direction)

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------

log_header_r32("SUMMARY")

n_total <- length(.r32_state$results)
n_pass  <- sum(vapply(.r32_state$results,
                       function(r) r$status == "PASS", logical(1)))
n_warn  <- sum(vapply(.r32_state$results,
                       function(r) r$status == "WARN", logical(1)))
n_fail  <- sum(vapply(.r32_state$results,
                       function(r) r$status == "FAIL", logical(1)))
n_skip  <- sum(vapply(.r32_state$results,
                       function(r) r$status == "SKIP", logical(1)))

log_line_r32(glue("  Total checks:  {n_total}"))
log_line_r32(glue("  PASS:          {n_pass}"))
log_line_r32(glue("  WARN:          {n_warn}"))
log_line_r32(glue("  FAIL:          {n_fail}"))
log_line_r32(glue("  SKIP:          {n_skip}"))

log_blank_r32()
log_line_r32("  By check:")
for (check_name in names(.r32_state$results)) {
  log_line_r32(glue(
    "    {check_name}: {.r32_state$results[[check_name]]$status}"
  ))
}

log_line_r32(strrep("=", 70))

# ------------------------------------------------------------------------------
# WRITE LOG TO FILE
# ------------------------------------------------------------------------------

writeLines(.r32_state$log_lines, LOG_FILE_R32)
message(glue("\nAssumption test log written to:\n  {LOG_FILE_R32}"))
