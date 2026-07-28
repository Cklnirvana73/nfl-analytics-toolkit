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
# BLEND WEIGHT BY R/29 PRIOR SOURCE (RB/WR/TE)
# ----------------------------------
# Higher weight on R/31 when R/29 has less reliable input data. Live values in
# BLEND_WEIGHTS_BY_PRIOR_SOURCE:
#   veteran_history_only         = 0.275 (R/45 in-sample sweep; re-derivation pending)
#   phased_out_history_only      = 0.325 (R/45 in-sample sweep; re-derivation pending)
#   translation_active           = 0.50  (provisional; phase-2 re-fit pending)
#   translation_capped_rookie    = 0.50  (provisional; phase-2 re-fit pending)
#   phaseout_no_history_fallback = 0.50  (provisional; no clean backtest yet)
#   calibrated_fallback          = 0.50  (provisional; no clean backtest yet)
# QB1s use the heavier QB_BLEND_WEIGHTS_BY_PRIOR_SOURCE table (same keys,
# judgment-set values 0.50-0.90); unknown sources fall back to the defaults.
#
# POSITION HANDLING
# -----------------
#   QB: PASS-THROUGH. R/32 v1 does not reconcile QBs because they don't
#       compete with each other for the same pool. R/29's QB projections
#       are kept unchanged. Future v2 may incorporate team_pass_yds_pg from
#       R/30 to anchor QB1 to team passing offense.
#
#   RB: target_share + rush_share volumes from R/31, converted to PPG using
#       RB-specific efficiency. As of v2.1 all five RB rates (catch rate,
#       receiving ypc, td/target, ypc, td/carry) are derived at load time from
#       SEASONS_FOR_TD_PRIOR via .derive_position_efficiency(); the standalone
#       EMPIRICAL_RB_TD_PER_CARRY constant is retained as a tripwire that must
#       match the derived RB td/carry to 4 dp.
#
#   WR: target_share from R/31, converted to PPG using WR efficiency
#       (catch rate, receiving ypc, td/target all derived at load time in v2.1).
#
#   TE: target_share from R/31, converted to PPG using TE efficiency
#       (catch rate, receiving ypc, td/target all derived at load time in v2.1).
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
#   - Efficiency       : Position-average priors, data-derived at load time
#                        (v2.1; see .derive_position_efficiency)
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
#                                      (RB/WR/TE); R/30 anchor for QB1; NA for
#                                      QB2/QB3
#   qb_anchor_pass_ppg          dbl   QB1 passing points from the R/30 anchor
#                                      (NA for non-QB1)
#   qb_anchor_rush_ppg          dbl   QB1 rushing points from the R/30 anchor
#                                      (NA for non-QB1)
#   blend_weight_r31_static     dbl   Preseason weight by prior_source
#                                      (before in-season decay; QB1 uses its own
#                                       heavier table; 0 for QB2/QB3)
#   blend_weight_r31            dbl   Weight given to the volume/anchor in the
#                                      blend (= static * compute_prior_weight
#                                       when as_of_week supplied)
#   r32_posterior_mu            dbl   Final reconciled projection (QB1 blended
#                                      toward the anchor; QB2/QB3 depth-scaled)
#   r32_delta_from_r29          dbl   r32_posterior_mu - r29_posterior_mu
#   r32_projection_lower_80     dbl   Shifted to new posterior
#   r32_projection_upper_80     dbl   Shifted to new posterior
#   r32_projection_lower_95     dbl   Shifted to new posterior
#   r32_projection_upper_95     dbl   Shifted to new posterior
#   schema_tag                  chr   "s2_w15_reconciled_v3"
#
# SOURCE DEPENDENCIES
# -------------------
#   R/30_team_volume_projections.R (team passing totals -> QB1 anchor)
#   R/31_player_volume_allocation.R (allocate_player_volumes output)
#   R/16 player-season panel (read for QB rusher identity in the anchor)
#   nflfastR pbp via nflreadr::load_pbp (QB rush efficiency)
#
# INPUT ARTIFACTS
# ---------------
#   data/season2_cache/s2_week15_player_projections.csv  (R/29 output)
#   data/season2_cache/s2_week15_player_volume_allocation.rds  (R/31 output)
#   data/season2_cache/s2_week15_team_volumes.rds (R/30 output, QB1 anchor)
#   data/season2_cache/s2_week15_player_season_panel_cache.rds (R/16, QB ids)
#
# RUN
# ---
#   source(here::here("R", "32_projection_reconciliation.R"))
#   reconciled <- reconcile_projections()                 # preseason
#   reconciled <- reconcile_projections(as_of_week = 9)   # in-season
#
# Author: Christian K. LeBlanc
# Version: 2.2
#
# CHANGELOG
# ---------
# 2.2  QB v2 anchor. QB1 projections are no longer a pure R/29 pass-through:
#      they are blended toward a team-passing-plus-rush anchor derived from R/30.
#      The anchor for a QB1 is
#          projected_pass_yds_pg * pass_yd + projected_pass_tds_pg * pass_td
#        + expected_carries_pg * (qb_ypc * rush_yd + qb_tdpc * rush_td)
#      Passing volume is R/30's team projection at full share (1.0): the resolved
#      starter takes essentially all team dropbacks; injury / mid-season rookie
#      takeover / wildcat are in-season events out of scope for this preseason
#      reconciliation. R/30's qb_id is used only as a consistency check against
#      R/31's QB1 (mismatches are logged, not acted on). No INT term: R/30 does
#      not project interceptions.
#      QB rush efficiency (qb_ypc, qb_tdpc) is derived at runtime by
#      .derive_qb_rush_efficiency() over QB_RUSH_EFF_SEASONS from nflfastR pbp,
#      restricted to rush_attempt == 1 regular-season rows (kneels and two-point
#      tries removed, QB rushers identified via the R/16 panel). This is the
#      identical carry universe R/31 counts for expected_carries_pg, so the
#      rushing term is self-consistent with the volume it multiplies regardless
#      of how scrambles are coded. It is a league-average QB rate; per-QB
#      designed-vs-scramble mix is a v3 refinement.
#      QB1 blend weights come from QB_BLEND_WEIGHTS_BY_PRIOR_SOURCE (their own
#      table, leaning harder on the anchor than the RB/WR/TE weights because
#      R/29's QB posteriors are compressed and mis-ordered). QB2/QB3 stay at
#      weight 0 and continue to flow through .apply_qb_depth_discount unchanged,
#      so there is no double-count. THE QB WEIGHT VALUES ARE JUDGMENT-SET pending
#      an out-of-sample backtest (see queued items), mirroring how the R/31 and
#      CFB QB reference thresholds were set then validated.
#      Scoring is now parameterised: reconcile_projections() takes a
#      scoring_settings list (default R32_DEFAULT_SCORING_SETTINGS). Under the
#      defaults the RB/WR/TE volume-implied math is byte-identical to v2.1; the
#      settings simply expose the previously hardcoded 0.1/1.0/6 constants and
#      add the passing constants the QB anchor needs.
#      Schema tag bumped s2_w15_reconciled_v2 -> s2_w15_reconciled_v3 (new QB
#      anchor debug columns, and QB1 mu now moves). Downstream consumers that
#      key on the v2 tag (R/33, R/35, R/38) must be checked.
# 2.1  POSITION_EFFICIENCY is now derived at load time by
#      .derive_position_efficiency() from nflreadr::load_player_stats() over
#      SEASONS_FOR_TD_PRIOR, replacing the hardcoded WR/TE/RB literals. All five
#      rates per position are pooled (opportunity-weighted, sum/sum) on the same
#      window and filter as EMPIRICAL_RB_TD_PER_CARRY, which is retained as a
#      4-dp tripwire against the unified derivation's RB td/carry. Falls back to
#      the v2.0 literals on any load error or zero denominator. Output columns
#      unchanged, so schema tag stays s2_w15_reconciled_v2; only the values of
#      volume_implied_ppg_v2 (and any blended RB/WR/TE mu) shift. Largest move is
#      TE td/target (0.060 -> ~0.052) from pooling low-target blocking TEs.
# 2.0  Two v2 fixes. (1) RB td_per_carry prior is now computed empirically at
#      load time (EMPIRICAL_RB_TD_PER_CARRY) from nflreadr::load_player_stats()
#      over the 5 most recent completed seasons, replacing a hardcoded literal.
#      (2) In-season blend-weight decay: new optional as_of_week argument on
#      reconcile_projections() multiplies every prior_source blend weight by
#      compute_prior_weight(as_of_week), so the R/31 team-constraint correction
#      fades as R/29's posterior absorbs observed volume over the season (worst
#      for rookies at 0.85, exactly where in-season data matters most). Week 1
#      pw ~= 1.0 preserves current behavior; week 18 pw = 0.05. New audit
#      column blend_weight_r31_static. Schema tag -> s2_w15_reconciled_v2.
# 1.0  Initial build.
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
library(nflreadr)

source(here::here("R", "30_team_volume_projections.R"))
source(here::here("R", "31_player_volume_allocation.R"))

# compute_prior_weight() (R/29) arrives transitively via the sources above,
# but R/32 now calls it directly for the in-season blend-weight decay, so the
# dependency is declared explicitly here. Guarded to be a no-op when already
# loaded; fails loudly rather than drifting if R/29 cannot load.
if (!exists("compute_prior_weight")) {
  source(here::here("R", "29_projection_engine.R"))
}

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

SEASON_RECON <- 2026L

# Season window for empirical rushing-TD-per-carry derivation.
# Defined as the 5 most recently completed seasons relative to SEASON_RECON.
# Window rolls forward automatically when SEASON_RECON is updated.
SEASONS_FOR_TD_PRIOR <- (SEASON_RECON - 5L):(SEASON_RECON - 1L)

# Empirical RB rushing TD per carry, computed at script load time from
# nflreadr::load_player_stats() across SEASONS_FOR_TD_PRIOR regular seasons.
# Replaces a hardcoded constant. Falls back to 0.031 with a warning if the
# data call fails so the script does not silently die.
EMPIRICAL_RB_TD_PER_CARRY <- local({

  message(glue(
    "  Computing empirical RB td_per_carry from seasons ",
    "{min(SEASONS_FOR_TD_PRIOR)}-{max(SEASONS_FOR_TD_PRIOR)}"
  ))

  stats <- tryCatch(
    nflreadr::load_player_stats(
      seasons   = SEASONS_FOR_TD_PRIOR,
      stat_type = "offense"
    ),
    error = function(e) {
      warning(glue(
        "nflreadr::load_player_stats() failed: {e$message}. ",
        "Using fallback td_per_carry = 0.031"
      ))
      NULL
    }
  )

  if (is.null(stats)) return(0.031)

  rb_totals <- stats %>%
    dplyr::filter(
      .data$season_type == "REG",
      .data$position    == "RB",
      !is.na(.data$carries),
      .data$carries     > 0L
    ) %>%
    dplyr::summarise(
      total_carries  = sum(.data$carries,     na.rm = TRUE),
      total_rush_tds = sum(.data$rushing_tds, na.rm = TRUE)
    )

  if (rb_totals$total_carries == 0L) {
    warning("No RB carries found in SEASONS_FOR_TD_PRIOR; using fallback 0.031")
    return(0.031)
  }

  rate <- rb_totals$total_rush_tds / rb_totals$total_carries

  message(glue(
    "  Empirical RB td_per_carry: {format(round(rate, 4), nsmall = 4)} ",
    "({format(rb_totals$total_rush_tds, big.mark = ',')} TDs / ",
    "{format(rb_totals$total_carries, big.mark = ',')} carries)"
  ))

  rate
})

# ------------------------------------------------------------------------------
# .derive_position_efficiency
# ------------------------------------------------------------------------------
#
# Derives the POSITION_EFFICIENCY rate table from
# nflreadr::load_player_stats() over `seasons` (default SEASONS_FOR_TD_PRIOR),
# replacing the hardcoded literals used through v2.0. One data pull; all five
# rates per position derived on the same window, filter, and pooled
# (opportunity-weighted) method as EMPIRICAL_RB_TD_PER_CARRY.
#
# Defined here in the constants region (rather than the helpers section below)
# because POSITION_EFFICIENCY is assigned at source() time and must see it.
#
# Pooled / opportunity-weighted (sum numerator / sum denominator), NOT a mean
# of per-player rates: each coefficient converts one unit of volume (a target,
# a carry) into expected points, so it is weighted by opportunities.
#   catch_rate      = sum(receptions)      / sum(targets)
#   yards_per_catch = sum(receiving_yards) / sum(receptions)
#   td_per_target   = sum(receiving_tds)   / sum(targets)
#   yards_per_carry = sum(rushing_yards)   / sum(carries)    [RB only]
#   td_per_carry    = sum(rushing_tds)     / sum(carries)    [RB only]
#
# Fail-safe: on any load error or zero denominator, returns the full literal
# fallback list (the v2.0 hardcoded values) with a warning, so the constant is
# never left undefined and the script does not silently die.
#
# @param seasons Integer vector of seasons to pool.
# @return Named list (WR, TE, RB), each a list of the five efficiency rates.
# @keywords internal
.derive_position_efficiency <- function(seasons = SEASONS_FOR_TD_PRIOR) {

  # Literal fallback = the v2.0 hardcoded values. Also the return on any failure.
  fallback <- list(
    "WR" = list(catch_rate = 0.65, yards_per_catch = 12.5, td_per_target = 0.05,
                yards_per_carry = 0.0, td_per_carry = 0.0),
    "TE" = list(catch_rate = 0.70, yards_per_catch = 11.0, td_per_target = 0.06,
                yards_per_carry = 0.0, td_per_carry = 0.0),
    "RB" = list(catch_rate = 0.78, yards_per_catch = 8.0,  td_per_target = 0.03,
                yards_per_carry = 4.3, td_per_carry = 0.031)
  )

  message(glue(
    "  Deriving POSITION_EFFICIENCY from seasons ",
    "{min(seasons)}-{max(seasons)}"
  ))

  stats <- tryCatch(
    nflreadr::load_player_stats(seasons = seasons, stat_type = "offense"),
    error = function(e) {
      warning(glue(
        "nflreadr::load_player_stats() failed: {e$message}. ",
        "Using hardcoded POSITION_EFFICIENCY fallback."
      ))
      NULL
    }
  )

  if (is.null(stats)) return(fallback)

  reg <- dplyr::filter(stats, .data$season_type == "REG")

  # Receiving rates per position (targeted rows only)
  rec <- reg %>%
    dplyr::filter(.data$position %in% c("WR", "TE", "RB"),
                  !is.na(.data$targets), .data$targets > 0) %>%
    dplyr::group_by(.data$position) %>%
    dplyr::summarise(
      tot_targets = sum(.data$targets,         na.rm = TRUE),
      tot_rec     = sum(.data$receptions,      na.rm = TRUE),
      tot_rec_yds = sum(.data$receiving_yards, na.rm = TRUE),
      tot_rec_tds = sum(.data$receiving_tds,   na.rm = TRUE),
      .groups = "drop"
    )

  # RB rushing rates (carry rows only)
  rush <- reg %>%
    dplyr::filter(.data$position == "RB",
                  !is.na(.data$carries), .data$carries > 0) %>%
    dplyr::summarise(
      tot_carries  = sum(.data$carries,       na.rm = TRUE),
      tot_rush_yds = sum(.data$rushing_yards, na.rm = TRUE),
      tot_rush_tds = sum(.data$rushing_tds,   na.rm = TRUE)
    )

  # Zero-denominator guard: any missing position or zero denom -> full fallback
  need_pos <- c("WR", "TE", "RB")
  have_all_pos <- all(need_pos %in% rec$position)
  denoms_ok <- have_all_pos &&
    all(rec$tot_targets > 0) && all(rec$tot_rec > 0) &&
    nrow(rush) == 1L && rush$tot_carries > 0

  if (!denoms_ok) {
    warning(glue(
      "POSITION_EFFICIENCY derivation hit a missing position or zero ",
      "denominator; using hardcoded fallback."
    ))
    return(fallback)
  }

  # Assemble per-position rate lists (receiving rates; rush stays 0 for WR/TE)
  rate_row <- function(pos) {
    r <- rec[rec$position == pos, ]
    list(
      catch_rate      = r$tot_rec / r$tot_targets,
      yards_per_catch = r$tot_rec_yds / r$tot_rec,
      td_per_target   = r$tot_rec_tds / r$tot_targets,
      yards_per_carry = 0.0,
      td_per_carry    = 0.0
    )
  }

  wr <- rate_row("WR")
  te <- rate_row("TE")
  rb <- rate_row("RB")
  rb$yards_per_carry <- rush$tot_rush_yds / rush$tot_carries
  rb$td_per_carry    <- rush$tot_rush_tds / rush$tot_carries

  out <- list("WR" = wr, "TE" = te, "RB" = rb)

  message(glue(
    "  Derived: ",
    "WR {format(round(wr$catch_rate, 3), nsmall = 3)}/",
    "{format(round(wr$yards_per_catch, 1), nsmall = 1)}/",
    "{format(round(wr$td_per_target, 4), nsmall = 4)}; ",
    "TE {format(round(te$catch_rate, 3), nsmall = 3)}/",
    "{format(round(te$yards_per_catch, 1), nsmall = 1)}/",
    "{format(round(te$td_per_target, 4), nsmall = 4)}; ",
    "RB {format(round(rb$catch_rate, 3), nsmall = 3)}/",
    "{format(round(rb$yards_per_catch, 1), nsmall = 1)}rec /",
    "{format(round(rb$yards_per_carry, 2), nsmall = 2)}ypc/",
    "{format(round(rb$td_per_carry, 4), nsmall = 4)}"
  ))

  out
}

# Position-specific PPR efficiency priors, derived at load time (v2.1) from
# nflreadr::load_player_stats() over SEASONS_FOR_TD_PRIOR. Replaces the v2.0
# hardcoded literals. Consumed only by .compute_volume_implied_ppg() to convert
# R/31 volume allocations into implied PPG. The scoring constants that turn
# these rates into points (0.1/yd, 1.0/reception, 6/TD) live in that consumer
# and match Sleeper receiving/rushing scoring.
POSITION_EFFICIENCY <- .derive_position_efficiency(SEASONS_FOR_TD_PRIOR)

# L-AB tripwire: the unified derivation computes RB td_per_carry independently;
# the standalone EMPIRICAL_RB_TD_PER_CARRY (retained above) must agree to 4 dp.
# A divergence signals the two load-time pulls saw different data, or one fell
# back to its literal -- surfaced as a loud warning, never a silent drift.
local({
  derived_tpc <- POSITION_EFFICIENCY[["RB"]][["td_per_carry"]]
  if (abs(derived_tpc - EMPIRICAL_RB_TD_PER_CARRY) > 1e-4) {
    warning(glue(
      "RB td_per_carry mismatch: unified derivation ",
      "{format(round(derived_tpc, 4), nsmall = 4)} vs standalone ",
      "EMPIRICAL_RB_TD_PER_CARRY ",
      "{format(round(EMPIRICAL_RB_TD_PER_CARRY, 4), nsmall = 4)}."
    ))
  } else {
    message(glue(
      "  L-AB tripwire OK: RB td_per_carry ",
      "{format(round(derived_tpc, 4), nsmall = 4)} matches standalone."
    ))
  }
})

# Sack adjustment. R/30 team_pass_pg counts sack plays as pass plays
# (~6.5% inflation). Multiply by this to convert to true thrown passes
# (which is what becomes target volume).
SACK_ADJUSTMENT <- 0.935

# Blend weights by R/29 prior_source. Higher = trust R/31 volume more.
#
# [2026-07-11] REKEYED and recalibrated from the R/45 out-of-sample blend-weight
# backtest (target years 2018-2025, preseason arms, player-clustered bootstrap).
# The prior key set (blended / history_only_fallback / translation_only /
# score_final_fallback) matched NO value R/29 actually emits, so every bucket
# except veteran_history_only silently fell through to DEFAULT_R31_WEIGHT (0.50).
# This restores the correct keys and sets the two leak-clean history buckets to
# their validated optima:
#   veteran_history_only     0.45 (effective) -> 0.275   95% CI [0.175, 0.375]
#   phased_out_history_only  0.50 (default)   -> 0.325   95% CI [0.200, 0.425]
# Both CIs exclude the prior weight; both buckets want LESS weight on the volume
# arm (a veteran's own history predicts him better than a volume projection). The
# RMSE-vs-w curve is flat near the optimum because the two arms are correlated,
# so nearby weights are near-equivalent and the accuracy gain is small (~0.05
# PPG); the value here is correctness, not magnitude.
#
# The translation/fallback buckets are NOT yet validated leak-free: their R/29
# prior still uses the production translation model, which sees future outcomes
# in a historical holdout, so their backtest weights are biased. They are held
# at the neutral 0.50 pending the phase-2 per-year translation re-fit. Leaky
# (lower-bound) evidence points below 0.50 (translation_active ~0.325,
# translation_capped_rookie ~0.375).
#
# IN-SEASON DECAY (v2): these are PRESEASON weights. When reconcile_projections()
# is called with as_of_week, every weight is multiplied by
# compute_prior_weight(as_of_week) -- the same decay curve as R/29, R/30 SOS,
# and R/31. Rationale: R/29's posterior already absorbs observed volume in-season
# via its own decay, so by late season the static R/31 weight would fight the
# real signal. At week 1, pw ~= 1.0 so behavior is unchanged; at week 18,
# pw = 0.05 so R/32 almost entirely defers to R/29's observed-driven posterior.
# The relative ordering across prior_sources is preserved at every week.
#
# [2026-07-27] CAVEAT: 0.275/0.325 were selected by an IN-SAMPLE sweep -- R/45
# stage 4 chose the argmin weight on the same pooled rows it evaluated (no
# holdout), so the "OOS-validated" label above overstates the evidence. Values
# are retained unchanged and are pending re-derivation under the forward-chained
# backtest harness (R/45 stage 4 now forward-chains; see that file).
BLEND_WEIGHTS_BY_PRIOR_SOURCE <- c(
  "veteran_history_only"         = 0.275,  # R/45 in-sample sweep; re-derivation pending
  "phased_out_history_only"      = 0.325,  # R/45 in-sample sweep; re-derivation pending
  "translation_active"           = 0.50,   # provisional; phase-2 re-fit pending
  "translation_capped_rookie"    = 0.50,   # provisional; phase-2 re-fit pending
  "phaseout_no_history_fallback" = 0.50,   # provisional; no clean backtest yet
  "calibrated_fallback"          = 0.50    # provisional; no clean backtest yet
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

SCHEMA_TAG_RECON <- "s2_w15_reconciled_v3"

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
# QB v2 ANCHOR CONSTANTS
# ------------------------------------------------------------------------------

# Season window for the QB rush-efficiency derivation. Matches R/31's
# QB_RUSH_RATE_SEASONS (the 3 most recently completed seasons) so the rate that
# builds expected_carries_pg and the efficiency that multiplies it are drawn
# from the same window.
QB_RUSH_EFF_SEASONS <- (SEASON_RECON - 3L):(SEASON_RECON - 1L)

# R/16 season panel, read by .derive_qb_rush_efficiency() only to identify which
# rusher_player_ids are QBs (position == "QB"). Same artifact R/31 uses; keeping
# an independent path constant avoids sourcing R/31 into this module.
PANEL_CACHE_PATH_RECON <- here::here(
  "data", "season2_cache", "s2_week15_player_season_panel_cache.rds"
)

# Scoring settings. Threaded through reconcile_projections() so a caller can
# reconcile under a non-default ruleset. The DEFAULTS REPRODUCE v2.1 EXACTLY:
# the RB/WR/TE receiving/rushing constants (rec 1.0, 0.1/yd, 6/TD) are the values
# that were hardcoded inside .compute_volume_implied_ppg through v2.1, and the
# passing constants (0.04/yd, 4/TD) match R/29's projection scoring. pass_int is
# carried for completeness but is unused by the QB anchor (R/30 projects no INTs).
# NAMESPACE GUARD  [2026-07-15]
# R/29 also defines DEFAULT_SCORING_SETTINGS, with the full 20-key R/17 schema.
# This one carries only the 8 keys R/32 needs. Same name, different objects, so
# whichever file was sourced last won. R/29 line 1781 does
# modifyList(DEFAULT_SCORING_SETTINGS, scoring_settings), meaning R/32's reduced
# list could silently become R/29's fallback base and drop fumbles, te_premium,
# sack_penalty and 9 more. Renamed so R/29 keeps the unprefixed name it owns.
# Callers passing a partial scoring list should route it through R/46's
# .complete_scoring() to fill the R/17 template first.
R32_DEFAULT_SCORING_SETTINGS <- list(
  pass_yd  = 0.04,
  pass_td  = 4,
  pass_int = -2,
  rush_yd  = 0.1,
  rush_td  = 6,
  ppr      = 1.0,   # R/17 schema key (was `rec`; see 2026-07-15 note)
  rec_yd   = 0.1,
  rec_td   = 6
)

# QB1 blend weights by R/29 prior_source. Higher = trust the R/30 anchor more.
#
# These are DELIBERATELY HEAVIER than the RB/WR/TE weights above. Two reasons:
# (1) R/29's QB posteriors are compressed and mis-ordered (the top starters sit
# inside a narrow band), so the R/30 team anchor carries most of the ordering
# signal; (2) the anchor is a strong, team-level construction, not a noisy
# single-player volume estimate. Even a QB with the richest R/29 input
# (translation_active) gets half its projection from the anchor; weak-prior QBs
# (phaseout_no_history_fallback, calibrated_fallback) are almost entirely
# anchor-driven, which is appropriate because that is exactly where R/29's own
# posterior is weakest.
#
# THESE VALUES ARE JUDGMENT-SET, NOT YET OOS-VALIDATED. They are the one part of
# this build resting on reasoning rather than an error backtest. Treat them as an
# initial calibration to be revisited by the queued QB blend-weight backtest,
# the same way the R/31 carryover weights and the CFB QB reference threshold were
# set first and validated after. In-season decay applies to these exactly as it
# does to the RB/WR/TE weights.
#
# [2026-07-27] REKEYED to the prior_source values R/29 actually emits. The old
# keys (blended / history_only_fallback / translation_only / score_final_fallback)
# matched nothing R/29 produces, so every QB1 except veteran_history_only fell
# through to QB_DEFAULT_WEIGHT (0.70). Intent-preserving mapping (old -> new):
#   blended               0.50 -> translation_active           (richest R/29 input)
#   veteran_history_only  0.65 -> veteran_history_only         (unchanged)
#   history_only_fallback 0.70 -> phased_out_history_only      (history, no translation)
#   translation_only      0.80 -> translation_capped_rookie    (rookie translation only)
#   score_final_fallback  0.90 -> phaseout_no_history_fallback (no usable history)
#   score_final_fallback  0.90 -> calibrated_fallback          (no data at all)
# Fallback-ish sources keep the higher anchor weight; translation/history-backed
# sources keep the lower. Only the KEYS changed here -- the values remain
# judgment-set (see above) pending the queued QB blend-weight backtest.
# .determine_blend_weights() warns loudly if a QB1 row carries a prior_source
# missing from this table before falling back to QB_DEFAULT_WEIGHT.
QB_BLEND_WEIGHTS_BY_PRIOR_SOURCE <- c(
  "translation_active"           = 0.50,
  "veteran_history_only"         = 0.65,
  "phased_out_history_only"      = 0.70,
  "translation_capped_rookie"    = 0.80,
  "phaseout_no_history_fallback" = 0.90,
  "calibrated_fallback"          = 0.90
)

# Fallback weight for a QB1 with an unrecognized prior_source value.
QB_DEFAULT_WEIGHT <- 0.70

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
  "volume_implied_ppg_v2", "blend_weight_r31_static", "blend_weight_r31",
  "r32_posterior_mu", "r32_delta_from_r29",
  "r32_projection_lower_80", "r32_projection_upper_80",
  "r32_projection_lower_95", "r32_projection_upper_95",
  "ppr_per_target", "ppr_per_carry",
  "lower_80_delta", "upper_80_delta",
  "lower_95_delta", "upper_95_delta",
  "is_qb",
  "depth_position", "qb_discount_factor",
  # QB v2 anchor
  "team", "position", "expected_targets_pg", "expected_carries_pg",
  "projected_pass_yds_pg", "projected_pass_tds_pg", "qb_id",
  "qb_anchor_pass_ppg", "qb_anchor_rush_ppg",
  "catch_rate", "yards_per_catch", "td_per_target",
  "yards_per_carry", "td_per_carry",
  # pbp / panel columns used by .derive_qb_rush_efficiency
  "player_id", "season", "season_type", "rush_attempt", "qb_kneel",
  "two_point_attempt", "rusher_player_id", "rushing_yards", "rush_touchdown"
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
# .load_r30_team_volumes
# ------------------------------------------------------------------------------

#' Load R/30 team volume projections (QB v2 anchor input)
#'
#' Required columns: team, qb_id, projected_pass_yds_pg, projected_pass_tds_pg.
#' One row per team. qb_id is R/30's resolved starter gsis, used downstream only
#' as a consistency check against R/31's QB1 (not as a join key or multiplier).
#'
#' @param path Character. Path to R/30 team volumes RDS.
#' @return Tibble with the R/30 team-volume columns.
#' @keywords internal
.load_r30_team_volumes <- function(path = R30_TEAM_VOL_RDS_RECON) {

  if (!file.exists(path)) {
    stop(glue("R/30 team volumes not found at {path}. ",
              "Run project_team_volumes() first."))
  }

  team_vol <- readRDS(path)

  required <- c("team", "projected_pass_yds_pg", "projected_pass_tds_pg")
  missing_cols <- setdiff(required, names(team_vol))
  if (length(missing_cols) > 0L) {
    stop(glue("R/30 RDS missing required columns: ",
              "{paste(missing_cols, collapse = ', ')}"))
  }

  # qb_id powers the QB1 starter consistency check but is NOT load-bearing for
  # the anchor. Older R/30 caches predate the column; note and proceed (the
  # check is skipped downstream). Regenerate R/30 to enable it.
  if (!"qb_id" %in% names(team_vol)) {
    message(glue("  Note: R/30 cache has no qb_id column (predates it); ",
                 "anchor runs, QB1 starter check skipped. Regenerate R/30 ",
                 "to enable the check."))
  }

  # One row per team is required for a clean team -> QB1 attribution.
  # Committee teams legitimately carry >1 row (one per candidate QB). Only a
  # duplicate among non-committee teams is an error.
  .noncomm <- if ("committee_group" %in% names(team_vol))
    team_vol[is.na(team_vol$committee_group), , drop = FALSE] else team_vol
  dup_teams <- .noncomm$team[duplicated(.noncomm$team)]
  if (length(dup_teams) > 0L) {
    stop(glue("R/30 team volumes has >1 row for non-committee team(s): ",
              "{paste(unique(dup_teams), collapse = ', ')}. ",
              "Expected exactly one row per non-committee team."))
  }

  message(glue("  Loaded {nrow(team_vol)} R/30 team volume rows"))

  team_vol
}

# ------------------------------------------------------------------------------
# .derive_qb_rush_efficiency
# ------------------------------------------------------------------------------

#' Derive league-average QB rushing efficiency for the QB v2 anchor
#'
#' Returns yards-per-carry and TD-per-carry for QB rushes, pooled
#' (opportunity-weighted, sum / sum) over `seasons`. The carry universe is
#' EXACTLY the one R/31 counts for expected_carries_pg: regular-season rows with
#' rush_attempt == 1, kneels and two-point tries removed. Both numerators
#' (rushing_yards, rush_touchdown) are summed over those same rush_attempt == 1
#' rows, so ypc and tdpc share the anchor's carry denominator no matter how
#' nflfastR codes scrambles. QB rushers are identified by joining
#' rusher_player_id to the R/16 panel's QB player_ids (the panel is the same
#' position source R/31 trusts), which avoids a roster-schema dependency.
#'
#' pbp is loaded one season at a time and filtered to the handful of QB-rush rows
#' immediately, so peak memory stays small even across several seasons.
#'
#' Fails loudly (stop()) if the panel or pbp cannot be read, or if
#' rush_touchdown is absent, rather than substituting a hardcoded efficiency:
#' a silent fallback here would quietly corrupt every QB1 anchor.
#'
#' @param seasons Integer vector of seasons to pool (default QB_RUSH_EFF_SEASONS).
#' @param panel_path Character. Path to the R/16 season panel RDS.
#' @return List: yards_per_carry, td_per_carry, n_carries, n_qbs, seasons.
#' @keywords internal
.derive_qb_rush_efficiency <- function(seasons    = QB_RUSH_EFF_SEASONS,
                                       panel_path = PANEL_CACHE_PATH_RECON) {

  message(glue(
    "  Deriving QB rush efficiency from pbp, seasons ",
    "{min(seasons)}-{max(seasons)} (rush_attempt == 1, REG, no kneels/2pt)"
  ))

  # --- QB identity from the R/16 panel -------------------------------------
  if (!file.exists(panel_path)) {
    stop(glue("QB rush efficiency: R/16 panel not found at {panel_path}. ",
              "Build the player-season panel first."))
  }
  panel <- readRDS(panel_path)
  panel_need <- c("player_id", "season", "position")
  if (length(setdiff(panel_need, names(panel))) > 0L) {
    stop(glue("QB rush efficiency: panel missing one of ",
              "{paste(panel_need, collapse = ', ')}."))
  }
  qb_ids <- panel %>%
    dplyr::filter(as.integer(.data$season) %in% seasons,
                  .data$position == "QB") %>%
    dplyr::pull("player_id") %>%
    unique()
  qb_ids <- qb_ids[!is.na(qb_ids)]
  if (length(qb_ids) == 0L) {
    stop("QB rush efficiency: no QB player_ids found in the panel window.")
  }

  # --- Pooled QB-rush totals from pbp, one season at a time -----------------
  tot_carries <- 0L
  tot_yards   <- 0
  tot_tds     <- 0

  for (yr in seasons) {
    pbp_yr <- tryCatch(
      nflreadr::load_pbp(seasons = yr),
      error = function(e) {
        stop(glue("QB rush efficiency: load_pbp({yr}) failed: {e$message}"))
      }
    )

    if (!"rush_touchdown" %in% names(pbp_yr)) {
      stop(glue("QB rush efficiency: rush_touchdown absent from pbp {yr}; ",
                "cannot compute the TD term."))
    }
    if ("season_type" %in% names(pbp_yr)) {
      pbp_yr <- dplyr::filter(pbp_yr, .data$season_type == "REG")
    }

    qb_rush <- pbp_yr %>%
      dplyr::filter(
        .data$rush_attempt == 1L,
        dplyr::coalesce(.data$qb_kneel, 0L)          != 1L,
        dplyr::coalesce(.data$two_point_attempt, 0L) != 1L,
        .data$rusher_player_id %in% qb_ids
      )

    tot_carries <- tot_carries + nrow(qb_rush)
    tot_yards   <- tot_yards + sum(qb_rush$rushing_yards,  na.rm = TRUE)
    tot_tds     <- tot_tds   + sum(qb_rush$rush_touchdown, na.rm = TRUE)

    rm(pbp_yr, qb_rush)
    gc(verbose = FALSE)
  }

  if (tot_carries == 0L) {
    stop(glue("QB rush efficiency: zero QB carries found across ",
              "{min(seasons)}-{max(seasons)}."))
  }

  ypc  <- tot_yards / tot_carries
  tdpc <- tot_tds   / tot_carries

  message(glue(
    "  QB rush efficiency: {format(round(ypc, 3), nsmall = 3)} yds/carry, ",
    "{format(round(tdpc, 4), nsmall = 4)} TD/carry ",
    "({format(tot_carries, big.mark = ',')} carries, ",
    "{length(qb_ids)} QBs pooled)"
  ))

  list(
    yards_per_carry = ypc,
    td_per_carry    = tdpc,
    n_carries       = tot_carries,
    n_qbs           = length(qb_ids),
    seasons         = seasons
  )
}

# ------------------------------------------------------------------------------
# .compute_volume_implied_ppg
# ------------------------------------------------------------------------------

#' Compute PPG implied by R/31 volume (RB/WR/TE) or the R/30 anchor (QB1)
#'
#' RB/WR/TE: converts expected targets and carries (from R/31) into implied PPG
#' using POSITION_EFFICIENCY priors and the `scoring` settings. Applies
#' SACK_ADJUSTMENT to targets first.
#'   true_targets   = expected_targets_pg * SACK_ADJUSTMENT
#'   ppr_per_target = catch_rate * (yards_per_catch * rec_yd + ppr)
#'                  + td_per_target * rec_td
#'   ppr_per_carry  = yards_per_carry * rush_yd + td_per_carry * rush_td
#'   volume_implied = true_targets * ppr_per_target
#'                  + expected_carries_pg * ppr_per_carry
#' Under R32_DEFAULT_SCORING_SETTINGS these coefficients equal the v2.1 hardcoded
#' math exactly, so RB/WR/TE output is unchanged.
#'
#' QB1 (position == "QB" & depth_position == "QB1"): the R/30 team-passing +
#' rush anchor.
#'   pass_ppg = projected_pass_yds_pg * pass_yd + projected_pass_tds_pg * pass_td
#'   rush_ppg = expected_carries_pg *
#'                (qb_rush_eff$yards_per_carry * rush_yd
#'                 + qb_rush_eff$td_per_carry * rush_td)
#'   volume_implied = pass_ppg + rush_ppg
#' Passing volume is R/30's team projection at full share (1.0). If a team's
#' passing projection is missing, the anchor is left NA so the QB1 falls back to
#' R/29 in the blend (rather than silently zeroing the passing term). R/30's
#' qb_id is checked against R/31's QB1 gsis and mismatches are logged only.
#'
#' QB2/QB3 (and any other position): NA, so the blend passes them through to R/29
#' and .apply_qb_depth_discount handles the backups.
#'
#' @param alloc Tibble from .load_r31_allocations() (deduped to one row/player).
#' @param team_vol Tibble from .load_r30_team_volumes().
#' @param qb_rush_eff List from .derive_qb_rush_efficiency().
#' @param scoring List of scoring settings (default R32_DEFAULT_SCORING_SETTINGS).
#' @return Tibble: nfl_gsis_id, r31_true_targets_pg, volume_implied_ppg_v2,
#'   qb_anchor_pass_ppg, qb_anchor_rush_ppg.
#' @keywords internal
.compute_volume_implied_ppg <- function(alloc,
                                        team_vol,
                                        qb_rush_eff,
                                        scoring = R32_DEFAULT_SCORING_SETTINGS) {

  # Per-position PPR coefficients (RB/WR/TE), now driven by `scoring`.
  # SCORING SCHEMA GUARD  [2026-07-15]
  # ---------------------------------------------------------------------------
  # R/32 used to read `scoring$rec` while R/17, R/29, R/42 and R/46 all key the
  # per-reception value as `ppr`. R/46's .complete_scoring() explicitly DELETES
  # `rec` to keep R/29 clean, so every per-league scoring object reached this
  # function with scoring$rec = NULL. In R, `x * NULL` returns numeric(0), which
  # collapsed pos_coefs to ZERO ROWS; the join below then gave every RB/WR/TE an
  # NA coefficient, the old coalesce(., 0) turned that into a real zero, and the
  # NA fallback never fired. Result: volume_implied = 0 for all 487 blended
  # players and r32 = (1 - w) * r29, distorting ranks by prior_source weight.
  # Fix: read `ppr` (the R/17 schema key) and fail loudly if any required field
  # is absent, rather than pricing the board at zero volume in silence.
  .req <- c("ppr", "rec_yd", "rec_td", "rush_yd", "rush_td")
  .bad <- .req[vapply(.req, function(k) {
    v <- scoring[[k]]
    is.null(v) || length(v) != 1L || !is.finite(suppressWarnings(as.numeric(v)))
  }, logical(1))]
  if (length(.bad) > 0L) {
    stop(glue(
      ".compute_volume_implied_ppg(): scoring is missing or non-numeric for: ",
      "{paste(.bad, collapse = ', ')}. Every efficiency coefficient depends on ",
      "these, so the run is halted rather than projecting zero volume. Note the ",
      "per-reception key is `ppr` (R/17 schema), not `rec`."
    ), call. = FALSE)
  }

  pos_coefs <- purrr::imap_dfr(POSITION_EFFICIENCY, function(eff, pos) {
    tibble::tibble(
      position       = pos,
      ppr_per_target = eff$catch_rate *
                          (eff$yards_per_catch * scoring$rec_yd + scoring$ppr) +
                          eff$td_per_target * scoring$rec_td,
      ppr_per_carry  = eff$yards_per_carry * scoring$rush_yd +
                          eff$td_per_carry * scoring$rush_td
    )
  })

  if (nrow(pos_coefs) != length(POSITION_EFFICIENCY) ||
      anyNA(pos_coefs$ppr_per_target) || anyNA(pos_coefs$ppr_per_carry)) {
    stop(glue(
      ".compute_volume_implied_ppg(): efficiency coefficient table is malformed ",
      "({nrow(pos_coefs)} row(s) for {length(POSITION_EFFICIENCY)} position(s), ",
      "NA present: {anyNA(pos_coefs$ppr_per_target) || anyNA(pos_coefs$ppr_per_carry)}). ",
      "This means a scoring term collapsed. Halting instead of blending zeros."
    ), call. = FALSE)
  }

  # Points per QB rush carry, from the derived league QB rush efficiency.
  qb_pts_per_carry <- qb_rush_eff$yards_per_carry * scoring$rush_yd +
                      qb_rush_eff$td_per_carry    * scoring$rush_td

  # Passing lines from R/30. Non-committee teams have one row per team; committee
  # teams have one row per candidate QB (keyed team+qb_id). Split so normal QB1s
  # join by team (unchanged) and committee QBs join by team+qb_id (Option A).
  has_qb_id <- "qb_id" %in% names(team_vol)
  if ("committee_group" %in% names(team_vol)) {
    tv_solo <- team_vol[is.na(team_vol$committee_group), , drop = FALSE]
    tv_comm <- team_vol[!is.na(team_vol$committee_group), , drop = FALSE]
  } else {
    tv_solo <- team_vol
    tv_comm <- team_vol[0, , drop = FALSE]
  }
  team_pass <- tv_solo %>%
    dplyr::select(dplyr::any_of(c("team", "projected_pass_yds_pg",
                                  "projected_pass_tds_pg", "qb_id")))
  team_pass_comm <- tv_comm %>%
    dplyr::select(dplyr::any_of(c("team", "qb_id",
                                  "projected_pass_yds_pg",
                                  "projected_pass_tds_pg"))) %>%
    dplyr::rename(cpy = projected_pass_yds_pg, cpt = projected_pass_tds_pg)

  has_comm_alloc <- "committee_group" %in% names(alloc)

  enriched <- alloc %>%
    dplyr::left_join(pos_coefs, by = "position") %>%
    dplyr::left_join(team_pass, by = "team") %>%
    dplyr::left_join(team_pass_comm, by = c("team", "nfl_gsis_id" = "qb_id")) %>%
    dplyr::mutate(
      is_qb1 = .data$position == "QB" &
        !is.na(.data$depth_position) & .data$depth_position == "QB1",
      is_committee_qb = .data$position == "QB" &
        (if (has_comm_alloc) !is.na(.data$committee_group) else FALSE),
      is_anchor_qb = .data$is_qb1 | .data$is_committee_qb,

      # committee QBs use their own (team, qb_id) passing; others the team line
      eff_pass_yds = dplyr::coalesce(.data$cpy, .data$projected_pass_yds_pg),
      eff_pass_tds = dplyr::coalesce(.data$cpt, .data$projected_pass_tds_pg),

      r31_true_targets_pg = .data$expected_targets_pg * SACK_ADJUSTMENT,

      qb_anchor_pass_ppg = dplyr::if_else(
        .data$is_anchor_qb,
        .data$eff_pass_yds * scoring$pass_yd +
          .data$eff_pass_tds * scoring$pass_td,
        NA_real_
      ),
      qb_anchor_rush_ppg = dplyr::if_else(
        .data$is_anchor_qb,
        dplyr::coalesce(.data$expected_carries_pg, 0) * qb_pts_per_carry,
        NA_real_
      ),

      # No coalesce(., 0) here [2026-07-15]: a missing coefficient must stay NA
      # so the NA fallback in .blend_r29_r31() reduces the player to r29. Zero
      # would silently mean "this player is projected for no volume points".
      # Coefficients are guarded non-NA above, so any NA here is a join miss.
      volume_implied_ppg_v2 = dplyr::case_when(
        .data$position %in% RECON_POSITIONS ~
          .data$r31_true_targets_pg * .data$ppr_per_target +
          .data$expected_carries_pg * .data$ppr_per_carry,
        .data$is_anchor_qb ~
          .data$qb_anchor_pass_ppg + .data$qb_anchor_rush_ppg,
        TRUE ~ NA_real_
      )
    )

  # qb_id consistency check: does R/30's resolved starter match R/31's QB1?
  # Logged only -- the passing term is attributed to R/31's QB1 regardless.
  if (has_qb_id) {
    mism <- enriched %>%
      dplyr::filter(.data$is_qb1, !is.na(.data$qb_id),
                    .data$qb_id != .data$nfl_gsis_id)
    if (nrow(mism) > 0L) {
      ex <- utils::head(mism, 5L)
      message(glue(
        "  QB1 starter mismatch (R/30 qb_id vs R/31 QB1): {nrow(mism)} team(s). ",
        "e.g. {paste(sprintf('%s [R31 %s / R30 %s]', ex$team, ex$nfl_gsis_id, ex$qb_id), collapse = '; ')}"
      ))
    } else {
      message("  QB1 starter check: R/30 and R/31 agree on every anchored team")
    }
  } else {
    message("  QB1 starter check skipped (no qb_id in R/30 cache)")
  }

  n_anchored <- sum(enriched$is_qb1 & !is.na(enriched$volume_implied_ppg_v2))
  n_qb1_no_anchor <- sum(enriched$is_qb1 & is.na(enriched$volume_implied_ppg_v2))
  message(glue(
    "  QB1 anchored: {n_anchored}",
    if (n_qb1_no_anchor > 0L)
      glue(" ({n_qb1_no_anchor} QB1 with no R/30 passing -> R/29 fallback)")
    else ""
  ))

  enriched %>%
    dplyr::select(nfl_gsis_id, r31_true_targets_pg, volume_implied_ppg_v2,
                  qb_anchor_pass_ppg, qb_anchor_rush_ppg)
}

# ------------------------------------------------------------------------------
# .determine_blend_weights
# ------------------------------------------------------------------------------

#' Map R/29 prior_source to R/31 blend weight, with optional in-season decay
#'
#' RB/WR/TE look up BLEND_WEIGHTS_BY_PRIOR_SOURCE (unrecognized -> DEFAULT_R31
#' _WEIGHT). QB1 (from R/31 depth_position) looks up the heavier
#' QB_BLEND_WEIGHTS_BY_PRIOR_SOURCE (unrecognized -> QB_DEFAULT_WEIGHT). QB2/QB3
#' and any QB with no R/31 depth record stay at 0, so they pass through to R/29
#' and are handled by .apply_qb_depth_discount (no double-count). A QB1 whose
#' anchor is NA is still auto-zeroed downstream by .blend_projections' NA guard.
#'
#' When as_of_week is supplied, every weight is multiplied by
#' compute_prior_weight(as_of_week) so both the RB/WR/TE correction and the QB1
#' anchor fade as R/29's posterior absorbs observed volume over the season. NULL
#' (preseason) leaves the static weights unchanged.
#'
#' @param r29 Tibble from .load_r29_projections().
#' @param r31 Tibble from .load_r31_allocations() (deduped), for depth_position.
#' @param as_of_week Integer 1-18 or NULL. NULL = preseason (no decay).
#' @return Tibble: nfl_gsis_id, blend_weight_r31_static, blend_weight_r31.
#' @seealso compute_prior_weight (R/29)
#' @keywords internal
.determine_blend_weights <- function(r29, r31, as_of_week = NULL) {

  # Decay factor: 1.0 in preseason, compute_prior_weight(week) in-season.
  decay <- if (is.null(as_of_week)) 1.0 else compute_prior_weight(as_of_week)

  if (!is.null(as_of_week)) {
    message(glue("    Blend-weight decay at week {as_of_week}: ",
                 "static weights scaled by ",
                 "{format(round(decay, 3), nsmall = 3)}"))
  }

  depth_lookup <- r31 %>%
    dplyr::select(dplyr::any_of(c("nfl_gsis_id", "depth_position",
                                  "committee_group")))
  if (!"committee_group" %in% names(depth_lookup))
    depth_lookup$committee_group <- NA_character_
  # Same guard for depth_position: an r31 frame without depth info (or an
  # empty/no-depth run) must not error -- QBs then fall through to weight 0.
  if (!"depth_position" %in% names(depth_lookup))
    depth_lookup$depth_position <- NA_character_

  joined <- r29 %>%
    dplyr::left_join(depth_lookup, by = "nfl_gsis_id")

  # Loud drift guard: if R/29 starts emitting a prior_source this table doesn't
  # know about, say so before silently falling back to QB_DEFAULT_WEIGHT.
  qb1_sources <- joined %>%
    dplyr::filter(
      .data$position == "QB",
      (!is.na(.data$depth_position) & .data$depth_position == "QB1") |
        !is.na(.data$committee_group)
    ) %>%
    dplyr::pull(.data$prior_source)
  unknown_qb_sources <- setdiff(
    unique(qb1_sources[!is.na(qb1_sources)]),
    names(QB_BLEND_WEIGHTS_BY_PRIOR_SOURCE)
  )
  if (length(unknown_qb_sources) > 0) {
    warning(glue(
      "QB blend weights: prior_source value(s) not in ",
      "QB_BLEND_WEIGHTS_BY_PRIOR_SOURCE -- ",
      "{paste(unknown_qb_sources, collapse = ', ')}. ",
      "Falling back to QB_DEFAULT_WEIGHT ({QB_DEFAULT_WEIGHT}) for those rows."
    ))
  }

  joined %>%
    dplyr::mutate(
      blend_weight_r31_static = dplyr::case_when(
        .data$position == "QB" &
          ((!is.na(.data$depth_position) & .data$depth_position == "QB1") |
             !is.na(.data$committee_group)) ~ dplyr::coalesce(
            QB_BLEND_WEIGHTS_BY_PRIOR_SOURCE[.data$prior_source],
            QB_DEFAULT_WEIGHT
          ),
        .data$position == "QB" ~ 0,
        TRUE ~ dplyr::coalesce(
          BLEND_WEIGHTS_BY_PRIOR_SOURCE[.data$prior_source],
          DEFAULT_R31_WEIGHT
        )
      ),
      blend_weight_r31 = .data$blend_weight_r31_static * decay
    ) %>%
    dplyr::select(nfl_gsis_id, blend_weight_r31_static, blend_weight_r31)
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
#' @param reconciled Tibble. Output of .blend_projections(). depth_position is
#'   already present from STEP 5's merge, so no R/31 frame is needed here.
#' @return Tibble with r32_posterior_mu, intervals, and r32_delta_from_r29
#'   updated for QB backups. Adds qb_discount_factor column.
#' @keywords internal
.apply_qb_depth_discount <- function(reconciled) {

  # Defensive guard (same idiom as reconcile_projections): a frame built
  # without committee data must not error -- NA means "not a committee QB".
  if (!"committee_group" %in% names(reconciled))
    reconciled$committee_group <- NA_character_

  updated <- reconciled %>%
    dplyr::mutate(
      # Compute discount factor: only meaningful for QBs with R/31 depth record
      qb_discount_factor = dplyr::case_when(
        .data$position != "QB"      ~ NA_real_,
        !is.na(.data$committee_group) ~ NA_real_,
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
#' Top-level orchestrator. Loads R/29 projections, R/31 allocations, and R/30
#' team volumes; derives QB rush efficiency; computes volume-implied PPG
#' (RB/WR/TE from position efficiency, QB1 from the R/30 team-passing + rush
#' anchor); determines blend weights by R/29 prior_source; and produces corrected
#' per-player projections.
#'
#' QB1 projections are blended toward the R/30 anchor (v2). QB2/QB3 pass through
#' to R/29 and are scaled by .apply_qb_depth_discount as before.
#'
#' @param r29_path Character. Path to R/29 projections CSV.
#' @param r31_path Character. Path to R/31 allocations RDS.
#' @param r30_path Character. Path to R/30 team volumes RDS (QB v2 anchor input).
#' @param as_of_week Integer 1-18 or NULL. When NULL (default), the blend weights
#'   are the static preseason values. When supplied, every blend weight is
#'   decayed by compute_prior_weight(as_of_week) so R/32 defers more to R/29's
#'   observed-driven posterior as the season progresses.
#' @param scoring_settings List of scoring constants (default
#'   R32_DEFAULT_SCORING_SETTINGS). The defaults reproduce v2.1 RB/WR/TE output
#'   exactly and supply the passing constants the QB anchor needs.
#' @param save_output Logical. Write RDS + CSV outputs.
#' @return Tibble with all R/29 columns (posterior_mu renamed to
#'   r29_posterior_mu) plus R/32 reconciled columns.
#'
#' @seealso allocate_player_volumes (R/31), project_team_volumes (R/30),
#'   compute_prior_weight (R/29)
#' @export
reconcile_projections <- function(
    r29_path = R29_PROJECTIONS_CSV_RECON,
    r31_path = R31_ALLOC_RDS_RECON,
    r30_path = R30_TEAM_VOL_RDS_RECON,
    as_of_week = NULL,
    scoring_settings = R32_DEFAULT_SCORING_SETTINGS,
    save_output = TRUE) {

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/32: Reconciling projections for season {SEASON_RECON}"))
  message(glue("Sack adjustment: {SACK_ADJUSTMENT}"))
  message(glue("Reconciled positions: {paste(RECON_POSITIONS, collapse = ', ')}"))
  if (is.null(as_of_week)) {
    message("Blend-weight mode: preseason (static weights by prior_source)")
  } else {
    message(glue("Blend-weight mode: in-season at week {as_of_week} ",
                 "(weights decay toward R/29 posterior)"))
  }
  message(glue("{strrep('=', 70)}"))

  # STEP 1: Load R/29 projections
  message("\nSTEP 1/7: Loading R/29 projections")
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
  message("\nSTEP 2/7: Loading R/31 player allocations")
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

  # STEP 3: Load R/30 team volumes and derive QB rush efficiency (QB v2 anchor)
  # Ensure the committee tag exists even if R/31 predates committee support.
  if (!"committee_group" %in% names(r31)) r31$committee_group <- NA_character_

  message("\nSTEP 3/7: Loading R/30 team volumes + deriving QB rush efficiency")
  team_vol    <- .load_r30_team_volumes(path = r30_path)
  qb_rush_eff <- .derive_qb_rush_efficiency()

  # STEP 4: Compute volume-implied PPG (RB/WR/TE efficiency; QB1 R/30 anchor)
  message("\nSTEP 4/7: Computing volume-implied PPG (efficiency + QB1 anchor)")
  vol_implied <- .compute_volume_implied_ppg(
    alloc       = r31,
    team_vol    = team_vol,
    qb_rush_eff = qb_rush_eff,
    scoring     = scoring_settings
  )

  # STEP 5: Determine per-player blend weights
  message("\nSTEP 5/7: Determining blend weights by prior_source")
  blend_weights <- .determine_blend_weights(r29 = r29_renamed,
                                            r31 = r31,
                                            as_of_week = as_of_week)

  # STEP 6: Merge all sources
  message("\nSTEP 6/7: Merging R/29 + R/31 + volume implications + weights")
  merged <- r29_renamed %>%
    dplyr::left_join(
      r31 %>% dplyr::select(nfl_gsis_id, expected_targets_pg,
                              expected_carries_pg, depth_position,
                              committee_group) %>%
        dplyr::rename(
          r31_expected_targets_pg = expected_targets_pg,
          r31_expected_carries_pg = expected_carries_pg
        ),
      by = "nfl_gsis_id"
    ) %>%
    dplyr::left_join(vol_implied, by = "nfl_gsis_id") %>%
    dplyr::left_join(blend_weights, by = "nfl_gsis_id")

  # STEP 7: Blend projections
  message("\nSTEP 7/7: Blending R/29 and R/31 into r32_posterior_mu")
  reconciled <- .blend_projections(merged)

  # STEP 7.5: Apply QB depth discount (backup QB inflation fix). QB1 factor is
  # 1.00, so the blended anchor value from STEP 7 is preserved untouched; only
  # QB2/QB3 are scaled down off their R/29 pass-through.
  message("\nSTEP 7.5: Applying QB depth discount for backup QBs")
  reconciled <- .apply_qb_depth_discount(reconciled)

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
  n_qb_total <- sum(output$position == "QB", na.rm = TRUE)
  n_qb1_anchored <- sum(output$position == "QB" &
                          !is.na(output$qb_anchor_pass_ppg) &
                          !is.na(output$volume_implied_ppg_v2), na.rm = TRUE)
  n_with_r31 <- sum(!is.na(output$volume_implied_ppg_v2), na.rm = TRUE)

  # Top QB1 by anchor value, with its passing / rushing split
  qb1_top <- output %>%
    dplyr::filter(.data$position == "QB",
                   !is.na(.data$qb_anchor_pass_ppg)) %>%
    dplyr::arrange(dplyr::desc(.data$volume_implied_ppg_v2)) %>%
    dplyr::slice_head(n = 1L)

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
  message(glue("  QBs total:                 {n_qb_total}"))
  message(glue("  QB1 anchored (R/30):       {n_qb1_anchored}"))
  message(glue("  Players matched to R/31:   {n_with_r31}"))
  message(glue(
    "  QB rush efficiency used:   ",
    "{format(round(qb_rush_eff$yards_per_carry, 2), nsmall = 2)} yds/carry, ",
    "{format(round(qb_rush_eff$td_per_carry, 4), nsmall = 4)} TD/carry"
  ))
  if (nrow(qb1_top) > 0L) {
    message(glue(
      "  Top QB1 anchor:            {qb1_top$player_name} ",
      "({qb1_top$team}) {format(round(qb1_top$volume_implied_ppg_v2, 1), nsmall = 1)} ",
      "= {format(round(qb1_top$qb_anchor_pass_ppg, 1), nsmall = 1)} pass ",
      "+ {format(round(qb1_top$qb_anchor_rush_ppg, 1), nsmall = 1)} rush; ",
      "R/29 {format(round(qb1_top$r29_posterior_mu, 1), nsmall = 1)} -> ",
      "R/32 {format(round(qb1_top$r32_posterior_mu, 1), nsmall = 1)}"
    ))
  }

  if (!is.null(as_of_week)) {
    eff_max <- max(output$blend_weight_r31, na.rm = TRUE)
    static_max <- max(output$blend_weight_r31_static, na.rm = TRUE)
    message(glue(
      "  Blend-weight decay:        week {as_of_week}, top weight ",
      "{format(round(static_max, 2), nsmall = 2)} -> ",
      "{format(round(eff_max, 2), nsmall = 2)} after decay"
    ))
  }

  n_qb_discounted <- sum(
    !is.na(output$qb_discount_factor) & output$qb_discount_factor < 1.0,
    na.rm = TRUE
  )
  n_qb1_unchanged <- sum(
    !is.na(output$qb_discount_factor) & output$qb_discount_factor == 1.0,
    na.rm = TRUE
  )
  message(glue("  QB1 (anchored, depth no-op): {n_qb1_unchanged}"))
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
