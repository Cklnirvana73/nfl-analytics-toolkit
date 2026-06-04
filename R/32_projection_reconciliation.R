# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Projection Reconciliation Engine
# File: R/32_projection_reconciliation.R
#
# PURPOSE
# -------
# Third foundation layer for team-aware projections (Option 3 architecture).
# Reconciles R/29's per-player Bayesian posteriors with R/31's team-constrained
# volume allocations. Produces corrected per-player PPG projections that respect
# team-level finite resources.
#
# WHY THIS EXISTS
# ---------------
# R/29 projects players in isolation. R/30 projects team volumes. R/31
# distributes those team volumes among players using depth chart, draft
# capital, and talent. R/32 closes the loop: it converts R/31's volume
# allocations into implied PPG using position-average efficiency, then blends
# that team-constrained signal with R/29's prior posterior at a weight that
# reflects how much we trust R/29's individual signal.
#
# For Tai Felton: R/29 said 10.2 PPG (no NFL data, no translation match,
# score_final_fallback prior). R/31 says ~3.5 targets per game (team
# constrained). R/32 converts those targets to ~6 PPG via WR efficiency
# priors and blends at 85% R/31 weight (because score_final_fallback means
# R/29 had nothing to anchor on). Final: ~7 PPG. Drops from WR31 to WR60ish.
#
# BLEND WEIGHT BY R/29 PRIOR SOURCE
# ----------------------------------
# Higher weight on R/31 when R/29 has less reliable input data:
#   blended                = 0.30  (full NFL history + R/24 translation)
#   veteran_history_only   = 0.45  (history, no translation match)
#   history_only_fallback  = 0.55  (history, no R/24 prediction)
#   translation_only       = 0.65  (rookie with CFB translation only)
#   score_final_fallback   = 0.85  (no NFL data, no translation - hardest constraint)
#
# POSITION HANDLING
# -----------------
#   QB: PASS-THROUGH. R/32 v1 does not reconcile QBs because they don't
#       compete with each other for the same pool. R/29's QB projections
#       are kept unchanged. Future v2 may incorporate team_pass_yds_pg from
#       R/30 to anchor QB1 to team passing offense.
#
#   RB: target_share + rush_share volumes from R/31, converted to PPG using
#       RB-specific efficiency (catch rate 0.78, ypc 4.3, td/carry 0.031).
#
#   WR: target_share from R/31, converted to PPG using WR efficiency
#       (catch rate 0.65, ypc 12.5, td/target 0.05).
#
#   TE: target_share from R/31, converted to PPG using TE efficiency
#       (catch rate 0.70, ypc 11.0, td/target 0.06).
#
# SACK ADJUSTMENT
# ---------------
# R/30's team_pass_pg includes sack plays (~6.5% inflation). When converting
# expected_targets_pg to "true thrown targets," multiply by 0.935.
#
# PROJECTION INTERVALS
# --------------------
# Width preserved from R/29 (R/29's projection_upper_80 minus
# projection_lower_80 carries through). Intervals re-centered on the new
# r32_posterior_mu. Boom and bust probabilities passed through unchanged
# from R/29; rigorous re-computation would require full distribution
# inference and is deferred to a future version.
#
# DESIGN DECISIONS (confirmed in scoping)
# ---------------------------------------
#   - Scope            : RB/WR/TE only. QBs pass through unchanged.
#   - Efficiency       : Position-average priors (hardcoded constants)
#   - Blend mechanism  : Linear weighted average per prior_source
#   - Sack adjustment  : 0.935 multiplier on team_pass_pg
#   - Interval width   : Preserved from R/29
#   - Probabilities    : Boom/bust passed through, not recomputed
#
# OUTPUTS
# -------
#   data/season2_cache/s2_week15_reconciled_projections.rds
#   data/season2_cache/s2_week15_reconciled_projections.csv
#
# One row per player from R/29 input. Schema preserves all R/29 columns
# (renaming posterior_mu to r29_posterior_mu) and adds:
#
#   r31_expected_targets_pg     dbl   From R/31 allocation
#   r31_expected_carries_pg     dbl   From R/31 allocation
#   r31_true_targets_pg         dbl   After sack adjustment
#   volume_implied_ppg_v2       dbl   PPG implied by R/31 volume + efficiency
#                                      (NA for QBs)
#   blend_weight_r31            dbl   Weight given to R/31 in the blend
#                                      (0 for QBs)
#   r32_posterior_mu            dbl   Final reconciled projection
#                                      (= r29_posterior_mu for QBs)
#   r32_delta_from_r29          dbl   r32_posterior_mu - r29_posterior_mu
#   r32_projection_lower_80     dbl   Shifted to new posterior
#   r32_projection_upper_80     dbl   Shifted to new posterior
#   r32_projection_lower_95     dbl   Shifted to new posterior
#   r32_projection_upper_95     dbl   Shifted to new posterior
#   schema_tag                  chr   "s2_w15_reconciled_v1"
#
# SOURCE DEPENDENCIES
# -------------------
#   R/30_team_volume_projections.R (for QB future work, not used in v1)
#   R/31_player_volume_allocation.R (allocate_player_volumes output)
#
# INPUT ARTIFACTS
# ---------------
#   data/season2_cache/s2_week15_player_projections.csv  (R/29 output)
#   data/season2_cache/s2_week15_player_volume_allocation.rds  (R/31 output)
#   data/season2_cache/s2_week15_team_volumes.rds (R/30 output, reserved
#                                                   for QB v2)
#
# RUN
# ---
#   source(here::here("R", "32_projection_reconciliation.R"))
#   reconciled <- reconcile_projections()
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
library(readr)
library(here)
library(glue)

source(here::here("R", "30_team_volume_projections.R"))
source(here::here("R", "31_player_volume_allocation.R"))

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

SEASON_RECON <- 2026L

# Position-specific PPR efficiency priors. Used to convert R/31's volume
# allocations into implied PPG. These are league-average rates -- a future
# version could compute per-player efficiency from R/15 historical pbp.
# All values are PPR scoring (1 point per reception).
POSITION_EFFICIENCY <- list(
  "WR" = list(
    catch_rate      = 0.65,
    yards_per_catch = 12.5,
    td_per_target   = 0.05,
    yards_per_carry = 0.0,
    td_per_carry    = 0.0
  ),
  "TE" = list(
    catch_rate      = 0.70,
    yards_per_catch = 11.0,
    td_per_target   = 0.06,
    yards_per_carry = 0.0,
    td_per_carry    = 0.0
  ),
  "RB" = list(
    catch_rate      = 0.78,
    yards_per_catch = 8.0,
    td_per_target   = 0.03,
    yards_per_carry = 4.3,
    td_per_carry    = 0.031
  )
)

# Sack adjustment. R/30 team_pass_pg counts sack plays as pass plays
# (~6.5% inflation). Multiply by this to convert to true thrown passes
# (which is what becomes target volume).
SACK_ADJUSTMENT <- 0.935

# Blend weights by R/29 prior_source. Higher = trust R/31 more.
# Players with no NFL history or translation match get the strongest
# team-constraint correction; players with rich history get the lightest.
BLEND_WEIGHTS_BY_PRIOR_SOURCE <- c(
  "blended"                = 0.30,
  "veteran_history_only"   = 0.45,
  "history_only_fallback"  = 0.55,
  "translation_only"       = 0.65,
  "score_final_fallback"   = 0.85
)

# Fallback weight for any unrecognized prior_source value
DEFAULT_R31_WEIGHT <- 0.50

# Positions reconciled in v1 (QB excluded by design)
RECON_POSITIONS <- c("RB", "WR", "TE")

# Paths
R29_PROJECTIONS_CSV_RECON <- here::here(
  "data", "season2_cache", "s2_week15_player_projections.csv"
)
R31_ALLOC_RDS_RECON <- here::here(
  "data", "season2_cache", "s2_week15_player_volume_allocation.rds"
)
R30_TEAM_VOL_RDS_RECON <- here::here(
  "data", "season2_cache", "s2_week15_team_volumes.rds"
)
OUTPUT_RDS_PATH_RECON <- here::here(
  "data", "season2_cache", "s2_week15_reconciled_projections.rds"
)
OUTPUT_CSV_PATH_RECON <- here::here(
  "data", "season2_cache", "s2_week15_reconciled_projections.csv"
)

SCHEMA_TAG_RECON <- "s2_w15_reconciled_v1"

# QB depth discount multipliers by depth_position from R/31.
# Backup QBs inherit starter-level R/29 projections because R/29 uses
# historical PPG from seasons when they started. These multipliers convert
# those full-starter projections to expected PPG accounting for playing time.
#   QB1 = 1.00  (no change -- they're the starter)
#   QB2 = 0.15  (~injury insurance value: ~2-3 spot starts per season)
#   QB3+ = 0.05 (emergency only)
# Applied AFTER the standard blend so QBs benefit from the unchanged
# r29_posterior_mu as a starting point.
QB_DEPTH_DISCOUNT_MULT <- c(
  "QB1" = 1.00,
  "QB2" = 0.15,
  "QB3" = 0.05
)
QB_DEPTH_DISCOUNT_DEFAULT_MULT <- 0.05  # depth_position outside QB1/QB2/QB3

# ------------------------------------------------------------------------------
# NSE DECLARATIONS
# ------------------------------------------------------------------------------

utils::globalVariables(c(
  "posterior_mu", "posterior_sigma", "prior_source",
  "projection_lower_80", "projection_upper_80",
  "projection_lower_95", "projection_upper_95",
  "boom_probability", "bust_probability",
  "r29_posterior_mu", "r29_posterior_sigma",
  "r29_projection_lower_80", "r29_projection_upper_80",
  "r29_projection_lower_95", "r29_projection_upper_95",
  "r31_expected_targets_pg", "r31_expected_carries_pg",
  "r31_true_targets_pg",
  "volume_implied_ppg_v2", "blend_weight_r31",
  "r32_posterior_mu", "r32_delta_from_r29",
  "r32_projection_lower_80", "r32_projection_upper_80",
  "r32_projection_lower_95", "r32_projection_upper_95",
  "ppr_per_target", "ppr_per_carry",
  "lower_80_delta", "upper_80_delta",
  "lower_95_delta", "upper_95_delta",
  "is_qb",
  "depth_position", "qb_discount_factor"
))

# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

# ------------------------------------------------------------------------------
# .load_r29_projections
# ------------------------------------------------------------------------------

#' Load R/29 in-season projections from CSV
#'
#' Required columns: nfl_gsis_id, player_name, position, team, posterior_mu,
#' posterior_sigma, prior_source, projection_lower_80, projection_upper_80,
#' projection_lower_95, projection_upper_95.
#'
#' All other R/29 columns are preserved for pass-through.
#'
#' @param path Character. Path to R/29 CSV.
#' @return Tibble with all R/29 columns.
#' @keywords internal
.load_r29_projections <- function(path = R29_PROJECTIONS_CSV_RECON) {

  if (!file.exists(path)) {
    stop(glue("R/29 projections not found at {path}. ",
              "Run R/29 projection engine first."))
  }

  proj <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) {
      stop(glue("R/29 read failed: {e$message}"))
    }
  )

  required <- c("nfl_gsis_id", "player_name", "position", "team",
                "posterior_mu", "prior_source",
                "projection_lower_80", "projection_upper_80")
  missing_cols <- setdiff(required, names(proj))
  if (length(missing_cols) > 0L) {
    stop(glue("R/29 CSV missing required columns: ",
              "{paste(missing_cols, collapse = ', ')}"))
  }

  message(glue("  Loaded {nrow(proj)} R/29 projections"))

  proj
}

# ------------------------------------------------------------------------------
# .load_r31_allocations
# ------------------------------------------------------------------------------

#' Load R/31 player volume allocations
#'
#' Required columns: nfl_gsis_id, team, position, depth_position,
#' expected_targets_pg, expected_carries_pg.
#'
#' @param path Character. Path to R/31 RDS.
#' @return Tibble with R/31 allocation columns.
#' @keywords internal
.load_r31_allocations <- function(path = R31_ALLOC_RDS_RECON) {

  if (!file.exists(path)) {
    stop(glue("R/31 allocations not found at {path}. ",
              "Run allocate_player_volumes() first."))
  }

  alloc <- readRDS(path)

  required <- c("nfl_gsis_id", "team", "position", "depth_position",
                "expected_targets_pg", "expected_carries_pg")
  missing_cols <- setdiff(required, names(alloc))
  if (length(missing_cols) > 0L) {
    stop(glue("R/31 RDS missing required columns: ",
              "{paste(missing_cols, collapse = ', ')}"))
  }

  message(glue("  Loaded {nrow(alloc)} R/31 allocations"))

  alloc
}

# ------------------------------------------------------------------------------
# .compute_volume_implied_ppg
# ------------------------------------------------------------------------------

#' Compute PPR PPG implied by R/31 volume + position efficiency
#'
#' For each RB/WR/TE, converts expected targets and carries (from R/31)
#' into implied PPR per game using POSITION_EFFICIENCY priors.
#'
#' Applies SACK_ADJUSTMENT to targets first.
#'
#' Formula:
#'   true_targets = expected_targets_pg * SACK_ADJUSTMENT
#'   ppr_per_target = catch_rate * (yards_per_catch * 0.1 + 1.0)
#'                  + td_per_target * 6
#'   ppr_per_carry  = yards_per_carry * 0.1 + td_per_carry * 6
#'   volume_implied = true_targets * ppr_per_target
#'                  + expected_carries * ppr_per_carry
#'
#' QBs return NA (handled separately as pass-through in v1).
#'
#' @param alloc Tibble from .load_r31_allocations().
#' @return Tibble: nfl_gsis_id, r31_true_targets_pg, volume_implied_ppg_v2.
#' @keywords internal
.compute_volume_implied_ppg <- function(alloc) {

  # Pre-compute per-position PPR coefficients
  pos_coefs <- purrr::imap_dfr(POSITION_EFFICIENCY, function(eff, pos) {
    tibble::tibble(
      position       = pos,
      ppr_per_target = eff$catch_rate *
                          (eff$yards_per_catch * 0.1 + 1.0) +
                          eff$td_per_target * 6,
      ppr_per_carry  = eff$yards_per_carry * 0.1 + eff$td_per_carry * 6
    )
  })

  alloc %>%
    dplyr::left_join(pos_coefs, by = "position") %>%
    dplyr::mutate(
      r31_true_targets_pg = .data$expected_targets_pg * SACK_ADJUSTMENT,
      volume_implied_ppg_v2 = dplyr::if_else(
        .data$position %in% RECON_POSITIONS,
        .data$r31_true_targets_pg *
          dplyr::coalesce(.data$ppr_per_target, 0) +
          .data$expected_carries_pg *
          dplyr::coalesce(.data$ppr_per_carry, 0),
        NA_real_
      )
    ) %>%
    dplyr::select(nfl_gsis_id, r31_true_targets_pg, volume_implied_ppg_v2)
}

# ------------------------------------------------------------------------------
# .determine_blend_weights
# ------------------------------------------------------------------------------

#' Map R/29 prior_source to R/31 blend weight
#'
#' Looks up BLEND_WEIGHTS_BY_PRIOR_SOURCE. Unrecognized sources get
#' DEFAULT_R31_WEIGHT. QBs get weight 0 (pass-through).
#'
#' @param r29 Tibble from .load_r29_projections().
#' @return Tibble: nfl_gsis_id, blend_weight_r31.
#' @keywords internal
.determine_blend_weights <- function(r29) {

  r29 %>%
    dplyr::mutate(
      blend_weight_r31 = dplyr::case_when(
        .data$position == "QB" ~ 0,
        TRUE ~ dplyr::coalesce(
          BLEND_WEIGHTS_BY_PRIOR_SOURCE[.data$prior_source],
          DEFAULT_R31_WEIGHT
        )
      )
    ) %>%
    dplyr::select(nfl_gsis_id, blend_weight_r31)
}

# ------------------------------------------------------------------------------
# .blend_projections
# ------------------------------------------------------------------------------

#' Apply the linear blend between R/29 posterior and R/31 volume-implied PPG
#'
#' r32_posterior_mu = blend_weight_r31 * volume_implied_ppg_v2
#'                  + (1 - blend_weight_r31) * r29_posterior_mu
#'
#' For QBs (blend_weight_r31 = 0) this reduces to r29_posterior_mu.
#' For RB/WR/TE without R/31 match (volume_implied NA), reduces to r29 too.
#'
#' Projection intervals are shifted by r32_delta_from_r29 (preserves width).
#'
#' @param merged Tibble with R/29 + R/31 + blend_weight + volume_implied joined.
#' @return Tibble with r32_posterior_mu and shifted intervals added.
#' @keywords internal
.blend_projections <- function(merged) {

  merged %>%
    dplyr::mutate(
      # When R/31 volume is NA (player not in allocation), fall back to R/29
      blend_weight_effective = dplyr::if_else(
        is.na(.data$volume_implied_ppg_v2),
        0, .data$blend_weight_r31
      ),

      r32_posterior_mu = .data$blend_weight_effective *
                            dplyr::coalesce(
                              .data$volume_implied_ppg_v2, 0
                            ) +
                          (1 - .data$blend_weight_effective) *
                            .data$r29_posterior_mu,

      r32_delta_from_r29 = .data$r32_posterior_mu -
                              .data$r29_posterior_mu,

      # Shift intervals by the same delta (preserve width)
      r32_projection_lower_80 = .data$r29_projection_lower_80 +
                                   .data$r32_delta_from_r29,
      r32_projection_upper_80 = .data$r29_projection_upper_80 +
                                   .data$r32_delta_from_r29,
      r32_projection_lower_95 = dplyr::coalesce(
        .data$r29_projection_lower_95, .data$r29_projection_lower_80
      ) + .data$r32_delta_from_r29,
      r32_projection_upper_95 = dplyr::coalesce(
        .data$r29_projection_upper_95, .data$r29_projection_upper_80
      ) + .data$r32_delta_from_r29,

      # Clamp negative projections to 0. All four interval bounds are floored
      # (not just the lowers): when R/29 hands over a negative mu, an unfloored
      # upper bound can fall below the floored lower bound and invert the
      # interval. Flooring is monotonic, so any already-valid interval stays
      # valid and a fully-negative interval collapses to [0, 0].
      r32_posterior_mu        = pmax(.data$r32_posterior_mu, 0),
      r32_projection_lower_80 = pmax(.data$r32_projection_lower_80, 0),
      r32_projection_upper_80 = pmax(.data$r32_projection_upper_80, 0),
      r32_projection_lower_95 = pmax(.data$r32_projection_lower_95, 0),
      r32_projection_upper_95 = pmax(.data$r32_projection_upper_95, 0)
    )
}

# ------------------------------------------------------------------------------
# .apply_qb_depth_discount
# ------------------------------------------------------------------------------

#' Apply depth-chart-based playing time discount to QB projections
#'
#' R/29 projects every QB as if they start all 17 games, using historical PPG
#' from seasons when they played. Backup QBs (QB2, QB3) inherit these inflated
#' projections. This function uses R/31's depth_position field to identify
#' backups and scale their r32_posterior_mu down to expected playing-time PPG.
#'
#' Multipliers (defined in QB_DEPTH_DISCOUNT_MULT):
#'   QB1: 1.00  -- starter, no change
#'   QB2: 0.15  -- injury-insurance value (~2-3 starts per season)
#'   QB3+: 0.05 -- emergency only
#'
#' QBs with no R/31 depth record (not on any tracked depth chart) are left
#' unchanged with a flag in qb_discount_factor = NA.
#'
#' Intervals are scaled proportionally. A backup QB's upside compresses with
#' their playing time fraction.
#'
#' @param reconciled Tibble. Output of .blend_projections().
#' @param r31 Tibble. Full R/31 allocations (must contain nfl_gsis_id,
#'   position, depth_position).
#' @return Tibble with r32_posterior_mu, intervals, and r32_delta_from_r29
#'   updated for QB backups. Adds qb_discount_factor column.
#' @keywords internal
.apply_qb_depth_discount <- function(reconciled, r31) {

  # depth_position is already present in `reconciled` from STEP 5's merge.
  # The r31 arg is kept in the signature for symmetry with other helpers but
  # is not used here.

  updated <- reconciled %>%
    dplyr::mutate(
      # Compute discount factor: only meaningful for QBs with R/31 depth record
      qb_discount_factor = dplyr::case_when(
        .data$position != "QB"      ~ NA_real_,
        is.na(.data$depth_position) ~ NA_real_,
        .data$depth_position == "QB1" ~
          QB_DEPTH_DISCOUNT_MULT[["QB1"]],
        .data$depth_position == "QB2" ~
          QB_DEPTH_DISCOUNT_MULT[["QB2"]],
        TRUE ~
          QB_DEPTH_DISCOUNT_DEFAULT_MULT
      ),

      # Apply discount -- only fires for QBs with a non-null non-QB1 factor
      r32_posterior_mu = dplyr::case_when(
        .data$position == "QB" &
          !is.na(.data$qb_discount_factor) &
          .data$qb_discount_factor < 1.0 ~
          .data$r29_posterior_mu * .data$qb_discount_factor,
        TRUE ~ .data$r32_posterior_mu
      ),

      # Re-derive delta from R/29 after QB adjustment
      r32_delta_from_r29 = .data$r32_posterior_mu - .data$r29_posterior_mu,

      # Scale intervals proportionally for discounted QBs
      r32_projection_upper_80 = dplyr::case_when(
        .data$position == "QB" &
          !is.na(.data$qb_discount_factor) &
          .data$qb_discount_factor < 1.0 ~
          .data$r29_projection_upper_80 * .data$qb_discount_factor,
        TRUE ~ .data$r32_projection_upper_80
      ),
      r32_projection_lower_80 = dplyr::case_when(
        .data$position == "QB" &
          !is.na(.data$qb_discount_factor) &
          .data$qb_discount_factor < 1.0 ~
          pmax(.data$r29_projection_lower_80 *
                 .data$qb_discount_factor, 0),
        TRUE ~ .data$r32_projection_lower_80
      ),
      r32_projection_upper_95 = dplyr::case_when(
        .data$position == "QB" &
          !is.na(.data$qb_discount_factor) &
          .data$qb_discount_factor < 1.0 ~
          dplyr::coalesce(.data$r29_projection_upper_95,
                           .data$r29_projection_upper_80) *
          .data$qb_discount_factor,
        TRUE ~ .data$r32_projection_upper_95
      ),
      r32_projection_lower_95 = dplyr::case_when(
        .data$position == "QB" &
          !is.na(.data$qb_discount_factor) &
          .data$qb_discount_factor < 1.0 ~
          pmax(dplyr::coalesce(.data$r29_projection_lower_95,
                                .data$r29_projection_lower_80) *
                 .data$qb_discount_factor, 0),
        TRUE ~ .data$r32_projection_lower_95
      )
    ) %>%
    # depth_position was a working join column -- remove from output
    dplyr::select(-depth_position)

  # Diagnostic
  n_qb1     <- sum(updated$qb_discount_factor == 1.00, na.rm = TRUE)
  n_backup  <- sum(!is.na(updated$qb_discount_factor) &
                     updated$qb_discount_factor < 1.0, na.rm = TRUE)
  n_no_rec  <- sum(updated$position == "QB" &
                     is.na(updated$qb_discount_factor), na.rm = TRUE)

  message(glue(
    "  QB depth discount: {n_qb1} QB1 (unchanged), ",
    "{n_backup} backup(s) discounted, ",
    "{n_no_rec} no R/31 depth record (kept R/29)"
  ))

  updated
}


# ==============================================================================
# PUBLIC ENTRY POINT
# ==============================================================================

# ------------------------------------------------------------------------------
# reconcile_projections
# ------------------------------------------------------------------------------

#' Reconcile R/29 player projections with R/31 team-constrained allocations
#'
#' Top-level orchestrator. Loads R/29 projections, R/31 allocations,
#' computes volume-implied PPG using position efficiency priors, determines
#' blend weights by R/29 prior_source, and produces corrected per-player
#' projections.
#'
#' QBs pass through unchanged in v1. Future v2 will use R/30 team passing
#' totals to anchor QB1 projections to team offense.
#'
#' @param r29_path Character. Path to R/29 projections CSV.
#' @param r31_path Character. Path to R/31 allocations RDS.
#' @param r30_path Character. Path to R/30 team volumes RDS (reserved for QB v2).
#' @param save_output Logical. Write RDS + CSV outputs.
#' @return Tibble with all R/29 columns (posterior_mu renamed to
#'   r29_posterior_mu) plus R/32 reconciled columns.
#'
#' @seealso allocate_player_volumes (R/31), project_team_volumes (R/30)
#' @export
reconcile_projections <- function(
    r29_path = R29_PROJECTIONS_CSV_RECON,
    r31_path = R31_ALLOC_RDS_RECON,
    r30_path = R30_TEAM_VOL_RDS_RECON,
    save_output = TRUE) {

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/32: Reconciling projections for season {SEASON_RECON}"))
  message(glue("Sack adjustment: {SACK_ADJUSTMENT}"))
  message(glue("Reconciled positions: {paste(RECON_POSITIONS, collapse = ', ')}"))
  message(glue("{strrep('=', 70)}"))

  # STEP 1: Load R/29 projections
  message("\nSTEP 1/6: Loading R/29 projections")
  r29 <- .load_r29_projections(path = r29_path)

  # Rename R/29 columns so output is unambiguous
  r29_renamed <- r29 %>%
    dplyr::rename(
      r29_posterior_mu        = posterior_mu,
      r29_posterior_sigma     = posterior_sigma,
      r29_projection_lower_80 = projection_lower_80,
      r29_projection_upper_80 = projection_upper_80,
      r29_projection_lower_95 = projection_lower_95,
      r29_projection_upper_95 = projection_upper_95
    )

  # STEP 2: Load R/31 allocations
  message("\nSTEP 2/6: Loading R/31 player allocations")
  r31 <- .load_r31_allocations(path = r31_path)

  # Deduplicate: a player can appear in multiple R/31 rows if listed at
  # multiple positions on the depth chart (e.g., Travis Hunter as both
  # WR and DB). For reconciliation we want one row per nfl_gsis_id --
  # the primary role, defined as the row with the highest combined
  # expected volume (targets + carries).
  n_before <- nrow(r31)
  r31 <- r31 %>%
    dplyr::group_by(.data$nfl_gsis_id) %>%
    dplyr::slice_max(
      dplyr::coalesce(.data$expected_targets_pg, 0) +
        dplyr::coalesce(.data$expected_carries_pg, 0),
      n = 1L, with_ties = FALSE
    ) %>%
    dplyr::ungroup()
  n_after <- nrow(r31)
  if (n_before != n_after) {
    message(glue("    Deduplicated {n_before - n_after} cross-position row(s) ",
                 "-- one row per player retained ({n_after} unique players)"))
  }

  # STEP 3: Compute volume-implied PPG from R/31
  message("\nSTEP 3/6: Computing volume-implied PPG (R/31 + position efficiency)")
  vol_implied <- .compute_volume_implied_ppg(alloc = r31)

  # STEP 4: Determine per-player blend weights
  message("\nSTEP 4/6: Determining blend weights by prior_source")
  blend_weights <- .determine_blend_weights(r29 = r29_renamed)

  # STEP 5: Merge all sources
  message("\nSTEP 5/6: Merging R/29 + R/31 + volume implications + weights")
  merged <- r29_renamed %>%
    dplyr::left_join(
      r31 %>% dplyr::select(nfl_gsis_id, expected_targets_pg,
                              expected_carries_pg, depth_position) %>%
        dplyr::rename(
          r31_expected_targets_pg = expected_targets_pg,
          r31_expected_carries_pg = expected_carries_pg
        ),
      by = "nfl_gsis_id"
    ) %>%
    dplyr::left_join(vol_implied, by = "nfl_gsis_id") %>%
    dplyr::left_join(blend_weights, by = "nfl_gsis_id")

  # STEP 6: Blend projections
  message("\nSTEP 6/6: Blending R/29 and R/31 into r32_posterior_mu")
  reconciled <- .blend_projections(merged)

  # STEP 6.5: Apply QB depth discount (backup QB inflation fix)
  message("\nSTEP 6.5: Applying QB depth discount for backup QBs")
  reconciled <- .apply_qb_depth_discount(reconciled, r31)

  # Tag schema and finalize
  output <- reconciled %>%
    dplyr::mutate(schema_tag = SCHEMA_TAG_RECON) %>%
    dplyr::arrange(dplyr::desc(.data$r32_posterior_mu))

  # Save outputs
  if (save_output) {
    dir.create(dirname(OUTPUT_RDS_PATH_RECON), recursive = TRUE,
                showWarnings = FALSE)
    saveRDS(output, OUTPUT_RDS_PATH_RECON)
    readr::write_csv(output, OUTPUT_CSV_PATH_RECON)
    message(glue("\n  Saved: {OUTPUT_RDS_PATH_RECON}"))
    message(glue("  Saved: {OUTPUT_CSV_PATH_RECON}"))
  }

  # KEY INSIGHTS (computed from data, never hardcoded)
  n_total <- nrow(output)
  n_recon <- sum(output$position %in% RECON_POSITIONS, na.rm = TRUE)
  n_qb_passthrough <- sum(output$position == "QB", na.rm = TRUE)
  n_with_r31 <- sum(!is.na(output$volume_implied_ppg_v2), na.rm = TRUE)

  # Largest movers (absolute delta from R/29)
  movers <- output %>%
    dplyr::filter(.data$position %in% RECON_POSITIONS,
                   !is.na(.data$r32_delta_from_r29)) %>%
    dplyr::arrange(dplyr::desc(abs(.data$r32_delta_from_r29)))

  if (nrow(movers) > 0L) {
    biggest_down <- movers %>%
      dplyr::filter(.data$r32_delta_from_r29 < 0) %>%
      dplyr::slice_head(n = 1L)
    biggest_up <- movers %>%
      dplyr::filter(.data$r32_delta_from_r29 > 0) %>%
      dplyr::slice_head(n = 1L)
  } else {
    biggest_down <- biggest_up <- NULL
  }

  message(glue("\n{strrep('=', 70)}"))
  message("KEY INSIGHTS")
  message(glue("{strrep('=', 70)}"))
  message(glue("  Total players reconciled:  {n_total}"))
  message(glue("  RB/WR/TE blended:          {n_recon}"))
  message(glue("  QBs passed through:        {n_qb_passthrough}"))
  message(glue("  Players matched to R/31:   {n_with_r31}"))

  n_qb_discounted <- sum(
    !is.na(output$qb_discount_factor) & output$qb_discount_factor < 1.0,
    na.rm = TRUE
  )
  n_qb1_unchanged <- sum(
    !is.na(output$qb_discount_factor) & output$qb_discount_factor == 1.0,
    na.rm = TRUE
  )
  message(glue("  QB starters (unchanged):   {n_qb1_unchanged}"))
  message(glue("  QB backups (discounted):   {n_qb_discounted}"))

  if (!is.null(biggest_down) && nrow(biggest_down) > 0L) {
    message(glue(
      "  Biggest downward mover:    {biggest_down$player_name} ",
      "({biggest_down$team} {biggest_down$position}) ",
      "{format(round(biggest_down$r29_posterior_mu, 1), nsmall = 1)} -> ",
      "{format(round(biggest_down$r32_posterior_mu, 1), nsmall = 1)} ",
      "(delta {format(round(biggest_down$r32_delta_from_r29, 1), nsmall = 1)})"
    ))
  }
  if (!is.null(biggest_up) && nrow(biggest_up) > 0L) {
    message(glue(
      "  Biggest upward mover:      {biggest_up$player_name} ",
      "({biggest_up$team} {biggest_up$position}) ",
      "{format(round(biggest_up$r29_posterior_mu, 1), nsmall = 1)} -> ",
      "{format(round(biggest_up$r32_posterior_mu, 1), nsmall = 1)} ",
      "(delta +{format(round(biggest_up$r32_delta_from_r29, 1), nsmall = 1)})"
    ))
  }

  # Felton check (if present)
  felton <- output %>%
    dplyr::filter(grepl("Felton", .data$player_name, ignore.case = TRUE))
  if (nrow(felton) > 0L) {
    f <- felton[1, ]
    message(glue(
      "  Tai Felton check:          ",
      "R/29 {format(round(f$r29_posterior_mu, 1), nsmall = 1)} -> ",
      "R/32 {format(round(f$r32_posterior_mu, 1), nsmall = 1)} ",
      "(blend weight {format(round(f$blend_weight_r31, 2), nsmall = 2)})"
    ))
  }

  message(glue("{strrep('=', 70)}\n"))

  output
}
