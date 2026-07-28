# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# In-Season Projection Engine
# File: R/29_projection_engine.R
#
# PURPOSE
# -------
# Phase 4 capstone. Produces per-player, per-week PPR fantasy projections
# with confidence intervals by precision-weighted blending of three signals:
#
#   1. PRIOR -- assembled from R/28 dynasty prospect scores (cfg_player_name +
#      nfl_gsis_id) and R/24 v1 translation model predictions (pred_base in
#      PPR/game units). For rookies and second-year players, the prior is
#      dominated by the translation score because NFL sample variance is wide.
#      For veterans with 3+ NFL seasons, the prior is dominated by historical
#      NFL performance. This transition is automatic via precision weighting,
#      not a hard-coded tier boundary.
#
#   2. OBSERVED -- season-to-date PPR/game from the current season's pbp via
#      calculate_fantasy_points_ext() (R/17).
#
#   3. WEEK-BASED DECAY -- prior weight declines linearly from 1.0 at Week 1
#      to PRIOR_WEIGHT_FLOOR (0.05) at Week 18, with crossover ~Week 9.
#      Carried forward from Season 1 R/14 exactly.
#
# Outputs include FantasyPros consensus delta (our projection vs market
# consensus) and ADP value scoring (our projected position rank vs ADP).
# DEF/ST scoring built as standalone export, ready for Week 16 optimizer.
#
# DESIGN DECISIONS (confirmed in scoping)
# ---------------------------------------
#   - Positions      : QB, RB, WR, TE (TE added vs Season 1 which excluded it)
#   - Prior source 1 : R/28 score_final, score_v1 (dynasty 0-100 scale)
#   - Prior source 2 : R/24 pred_base (PPR/game scale, primary point estimate)
#   - Join key       : nfl_gsis_id (present in both R/28 and R/24)
#   - Rookie/vet     : Precision weighting (no named tier constant)
#   - Volume/eff     : 70/30 weight from Week 4 finding (carried from R/14)
#   - ADP source     : FantasyPros consensus via ffpros::fp_rankings()
#   - Consensus proj : FantasyPros via ffpros::fp_projections() as benchmark
#   - Crosswalk      : Normalized name + team + position; no fuzzy match dep
#   - Cache TTL      : 24 hours for FantasyPros pulls (filesystem cache)
#   - DEF/ST         : Sleeper standard pts_allow tiers; standalone export
#
# SEASON 1 R/14 PRIOR WEIGHT DECAY (carried forward)
# ---------------------------------------------------
#   compute_prior_weight(week) returns:
#     Week  1 -> 1.000  (prior dominates completely)
#     Week  3 -> 0.776
#     Week  9 -> 0.498  (CROSSOVER -- observed begins to dominate)
#     Week 14 -> 0.265
#     Week 18 -> 0.050  (FLOOR -- small regularization pull preserved)
#
# Bye weeks carry the prior forward unchanged because no observed data exists
# for that week. Players with bye weeks see no update that week.
#
# WHY R/14 PARAMETERS CARRY FORWARD
# ----------------------------------
# The Season 1 design was validated against held-out 2024 season data with
# RMSE comparable to the test set standard deviation. The week-decay schedule
# is not arbitrary -- it was tuned on the predictive validity finding that
# 3+ weeks of observed data begins to be more predictive than prior alone
# (the MIN_WEEKS_OBSERVED = 3 threshold). Re-tuning here would require
# re-running the predictive validity study, which is out of scope for R/29.
#
# NAVIGATION
# ----------
#   Line  ~110 : Libraries & sources
#   Line  ~150 : Constants (prior weights, scoring, DEF/ST tiers)
#   Line  ~300 : NSE declarations
#   Line  ~330 : Internal helpers
#                  .normalize_player_name()
#                  .build_fantasypros_crosswalk()
#                  .check_fp_cache_freshness()
#                  .compute_position_baseline()
#                  .compute_player_ytd_stats()
#                  .compute_player_historical_stats()
#                  .blend_prior_components()
#                  .def_st_pts_allowed_points()
#   Line  ~750 : compute_prior_weight()
#   Line  ~830 : load_adp_fantasypros()
#   Line ~1020 : load_consensus_projections_fantasypros()
#   Line ~1200 : build_projection_priors()
#   Line ~1500 : update_projection_with_ytd()
#   Line ~1750 : calculate_projection_intervals()
#   Line ~1900 : calculate_def_st_points()
#   Line ~2120 : run_projection_engine()
#
# SOURCE DEPENDENCIES (load these BEFORE sourcing R/29)
# ------------------------------------------------------
#   R/15_multi_season_pbp.R    -- load_normalized_season()
#   R/17_extended_scoring.R    -- calculate_fantasy_points_ext()
#
# INPUT ARTIFACTS (required, with expected paths)
# ------------------------------------------------
#   data/season2_cache/s2_week14_final_prospect_scores.csv  (R/28 output)
#     Required columns: cfb_player_name, position, nfl_gsis_id,
#                       score_v1, score_final, score_base, score_enriched
#     848 rows confirmed.
#
#   data/season2_cache/s2_week10_predictions.rds            (R/24 v1 output)
#     Required columns: nfl_gsis_id, draft_year, draft_position,
#                       ppr_per_game_y13, pred_base, pred_enriched, is_hit
#     494 rows confirmed.
#
#   data/season2_cache/pbp_normalized_{season}.rds          (R/15 output)
#     Loaded via load_normalized_season(season).
#
# OUTPUT ARTIFACTS (written by run_projection_engine())
# -----------------------------------------------------
#   data/season2_cache/s2_week15_player_projections.csv
#   data/season2_cache/s2_week15_def_st_scores.csv
#   data/season2_cache/s2_week15_fp_adp_cache_{season}.rds       (24h TTL)
#   data/season2_cache/s2_week15_fp_projections_cache_{season}.rds (24h TTL)
#
# SCHEMA TAG: s2_w15_v1
# ==============================================================================

# ==============================================================================
# LIBRARIES
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(here)
library(glue)
library(nflreadr)
library(ffpros)

# Source dependencies -- R/15, R/16, R/17, R/19, R/23 are sourced automatically
# if not already loaded. Sourcing multiple times is safe; these files are
# idempotent.
if (!exists("load_normalized_season")) {
  source(here::here("R", "15_multi_season_pbp.R"))
}
if (!exists("calculate_fantasy_points_ext")) {
  source(here::here("R", "17_extended_scoring.R"))
}
if (!exists("build_player_season_panel")) {
  source(here::here("R", "16_player_season_panel.R"))
}
if (!exists("run_aging_curve_pipeline")) {
  source(here::here("R", "23_aging_curves.R"))
}
if (!exists("get_all_sleeper_players")) {
  source(here::here("R", "19_sleeper_api.R"))
}

# ==============================================================================
# CONSTANTS
# ==============================================================================

# ------------------------------------------------------------------------------
# Season scope and week boundaries (carried forward from R/14)
# ------------------------------------------------------------------------------

NFL_REGULAR_SEASON_MAX_WEEK <- 18L
NFL_PLAYOFF_START_WEEK      <- 19L

NFL_BYE_WEEK_MIN <- 5L
NFL_BYE_WEEK_MAX <- 14L

# Minimum weeks of observed data before observed precision is treated as
# meaningful in the precision-weighted blend.
MIN_WEEKS_OBSERVED <- 3L

# ------------------------------------------------------------------------------
# Prior weight schedule (carried forward from R/14 exactly)
# ------------------------------------------------------------------------------

# Prior weight declines linearly from PRIOR_WEIGHT_START in Week 1 to
# PRIOR_WEIGHT_FLOOR by Week 18. Observed weight = 1 - prior_weight.
# The PRIOR_WEIGHT_FLOOR ensures the prior never fully disappears -- even
# in Week 18 it provides a small regularization pull against single-game
# anomalies and statistical noise.
PRIOR_WEIGHT_START <- 1.0
PRIOR_WEIGHT_FLOOR <- 0.05

# Week 4 finding: volume/opportunity features (rushing yards, receptions,
# target share) significantly outpredict efficiency features (EPA per play,
# success rate). These weights apply inside the prior construction when
# blending feature contributions.
VOLUME_FEATURE_WEIGHT     <- 0.70
EFFICIENCY_FEATURE_WEIGHT <- 0.30

# QB-specific volume weight: weaker than RB/WR/TE because same-volume QBs
# vary enormously by quality -- a mediocre QB with 35 dropbacks/game does
# not produce like an elite QB with 35 dropbacks. The efficiency signal is
# more informative for QB than the Week 4 finding implies for skill positions.
QB_VOLUME_FEATURE_WEIGHT  <- 0.50
QB_EFFICIENCY_FEATURE_WEIGHT <- 0.50

# Confidence interval z-scores
CI_95_Z <- 1.96
CI_80_Z <- 1.282

# ------------------------------------------------------------------------------
# Positions supported by the projection engine
# ------------------------------------------------------------------------------

# Season 1 used c("QB", "RB", "WR") -- TE was projected under the WR umbrella.
# Season 2 adds TE as a fourth supported position since R/28 scored TEs
# explicitly and the R/14 TE-fix (splitting receiver group via nflreadr
# rosters) is a standing contract.
SUPPORTED_POSITIONS <- c("QB", "RB", "WR", "TE")

# ------------------------------------------------------------------------------
# Translation sigma (uncertainty of the rookie / early-career prior)
# ------------------------------------------------------------------------------
#
# The translation prior's sigma is the v1 (R/24) LOCO holdout RMSE of the model
# variant that actually produced the point estimate, PER POSITION. Because
# ROOKIE_TRANSLATION_VARIANT uses base for QB and enriched for RB/WR/TE, the
# sigma must be variant-matched: base RMSE for QB, enriched RMSE for RB/WR/TE.
# Weighting an enriched point estimate with base uncertainty (or vice versa)
# miscalibrates the precision blend.
#
# These values are READ LIVE from the v1 performance file at projection-build
# time, never hardcoded, so they re-sync on every retrain and cannot silently
# go stale. If the file is missing or malformed the build fails loudly rather
# than falling back to a value that could drift.

#' Read variant-matched v1 translation RMSE (sigma) per position
#'
#' For each position, returns the v1 (R/24) LOCO holdout RMSE for the variant
#' that position uses in the prior (see \code{ROOKIE_TRANSLATION_VARIANT}),
#' read from \code{s2_week10_performance.rds}. Fails loudly on a missing file,
#' missing columns, or a missing/invalid row; there is deliberately no
#' hardcoded fallback.
#'
#' @param perf_path Character. Path to the v1 performance RDS
#'   (\code{s2_week10_performance.rds}).
#' @return Named numeric vector (QB, RB, WR, TE) of variant-matched RMSE.
.load_translation_sigmas <- function(perf_path) {
  if (!file.exists(perf_path)) {
    stop(
      ".load_translation_sigmas(): v1 performance file not found at: ", perf_path,
      "\n  Run run_week10_pipeline() (R/24) to regenerate it before building priors.",
      call. = FALSE
    )
  }

  perf    <- readRDS(perf_path)
  req_col <- c("draft_position", "model_variant", "rmse")
  missing <- setdiff(req_col, names(perf))
  if (length(missing) > 0L) {
    stop(
      ".load_translation_sigmas(): performance file missing columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }

  positions <- names(ROOKIE_TRANSLATION_VARIANT)
  sigmas <- vapply(positions, function(pos) {
    variant <- ROOKIE_TRANSLATION_VARIANT[[pos]]
    row <- perf[perf$draft_position == pos & perf$model_variant == variant, ,
                drop = FALSE]
    if (nrow(row) != 1L || is.na(row$rmse[1L]) || row$rmse[1L] <= 0) {
      stop(sprintf(
        ".load_translation_sigmas(): no single valid RMSE for %s / %s in %s",
        pos, variant, perf_path), call. = FALSE)
    }
    row$rmse[1L]
  }, numeric(1L))

  names(sigmas) <- positions
  sigmas
}

# ------------------------------------------------------------------------------
# Translation lifecycle constants (Season 3 Wave A1)
# ------------------------------------------------------------------------------
#
# The college translation is a pre-NFL signal. It is most informative for
# rookies, decays as a player accrues real NFL production, and is statistically
# dead once enough NFL history exists. Two hard guardrails sit ON TOP OF the
# existing precision-weighted blend (which already hands off smoothly from
# translation to history as NFL games accrue):
#
#   ROOKIE_TRANSLATION_VARIANT -- which translation point estimate feeds the
#     prior, per position. Enriched (adds draft capital) beats base at RB/WR/TE
#     on v1 LOCO holdout (RB R2 0.19->0.40, WR base R2 negative->0.16,
#     TE 0.07->0.25); QB gains almost nothing from draft capital (5.29->5.16
#     RMSE) and draft slot is a noisy QB signal, so QB stays on base. The
#     base-vs-enriched GAP is itself the article signal and is exposed
#     elsewhere; this constant governs only the projection prior.
#
#   TRANSLATION_PHASEOUT_EXP -- years_exp >= this gets ZERO translation weight;
#     the prior becomes 100% NFL history. Set empirically: the translation's
#     partial correlation with same-season PPG (after conditioning on NFL
#     history) is insignificant at every position by year 4, and the only
#     "significant" later cells are tiny-sample negative-sign artifacts. The
#     signal makes its last real stand at year 3 (WR significant, RB
#     borderline). See diagnostics/s3_diag_translation_decay.R.
#
#   HISTORY_ONLY_FLOOR_SEASONS -- a player is not forced to history-only until
#     he has at least this many QUALIFYING NFL seasons. Protects the slow
#     developer (the fading-but-real 4th-year-breakout case): do not commit a
#     player's prior to thin early history before he has had time to show it.
#
#   ROOKIE_TRUST_CAP -- downward-only haircut on a rookie's (years_exp == 0)
#     translation point estimate. Rookies are the highest-variance projection
#     in the system; this trims, never raises. Applied before blending.
#
# Coherence: floor (3) keeps some translation alive THROUGH year 3; phase-out
# (4) zeroes it AT year 4. No gap, no overlap.
ROOKIE_TRANSLATION_VARIANT <- list(
  QB = "base", RB = "enriched", WR = "enriched", TE = "enriched"
)
TRANSLATION_PHASEOUT_EXP   <- 4L
HISTORY_ONLY_FLOOR_SEASONS <- 3L
ROOKIE_TRUST_CAP           <- 0.85

# ------------------------------------------------------------------------------
# Default Sleeper PPR scoring settings (passed to calculate_fantasy_points_ext)
# ------------------------------------------------------------------------------

# These match the Sleeper PPR default. Users override per-league via the
# scoring_settings argument to run_projection_engine().
DEFAULT_SCORING_SETTINGS <- list(
  pass_yd              = 0.04,
  pass_td              = 4,       # Sleeper default is 4 (use 6 for superflex)
  pass_int             = -2,
  pick6_penalty        = -4,
  rush_yd              = 0.1,
  rush_td              = 6,
  rec_yd               = 0.1,
  rec_td               = 6,
  ppr                  = 1,
  fumbles              = -2,
  use_tiered_ppr       = TRUE,
  te_premium           = TRUE,
  rush_att_bonus       = 0,       # not standard Sleeper
  first_down_points    = 0,
  # [2026-07-16] long_td_bonus + long_td_threshold -> long_td_tiers.
  # R/17 retired the scalar pair; it could not express per-stat-type or
  # multi-tier long TD bonuses and used > semantics. NULL = no bonus, which
  # is what this default always meant.
  long_td_tiers        = NULL,
  hundred_yard_bonus   = 0,
  superflex_pass_td    = 0,
  two_point_conversion = 2,
  sack_penalty         = 0
)

# ------------------------------------------------------------------------------
# Sleeper DEF/ST scoring tiers (carried from R/19 mapping_log unsupported set)
# ------------------------------------------------------------------------------

# Points allowed tiers per Sleeper standard
DEF_PTS_ALLOW_TIERS <- list(
  pts_allow_0    = 10,    # shutout
  pts_allow_1_6  = 7,
  pts_allow_7_13 = 4,
  pts_allow_14_20 = 1,
  pts_allow_21_27 = 0,
  pts_allow_28_34 = -1,
  pts_allow_35p   = -4
)

# Defensive event points (Sleeper standard)
DEF_EVENT_POINTS <- list(
  sack         = 1,
  def_int      = 2,
  fum_rec      = 2,
  def_td       = 6,
  safety       = 2,
  blk_kick     = 2
)

# ------------------------------------------------------------------------------
# FantasyPros cache TTL
# ------------------------------------------------------------------------------

# Cache FantasyPros pulls for 24 hours. ADP updates daily; consensus
# projections update more frequently but daily is sufficient for in-season
# projection use. To force refresh, pass force_refresh = TRUE to either
# load_adp_fantasypros() or load_consensus_projections_fantasypros().
FP_CACHE_TTL_HOURS <- 24

# ------------------------------------------------------------------------------
# Output paths
# ------------------------------------------------------------------------------

CACHE_DIR_DEFAULT <- here::here("data", "season2_cache")

PROSPECTS_PATH_DEFAULT <- here::here(
  "data", "season2_cache", "s2_week14_final_prospect_scores.csv"
)

TRANSLATION_PREDS_PATH_DEFAULT <- here::here(
  "data", "season3_cache", "s3_r39_translation_preds_augmented.rds"
)

OUTPUT_PROJECTIONS_PATH <- here::here(
  "data", "season2_cache", "s2_week15_player_projections.csv"
)

OUTPUT_DEF_ST_PATH <- here::here(
  "data", "season2_cache", "s2_week15_def_st_scores.csv"
)

# ------------------------------------------------------------------------------
# Aging curve cache paths (Build 1 integration)
# ------------------------------------------------------------------------------

# Cached outputs from R/16 and R/23 -- built on first run, reused after.
# The aging curve file stores the curves_boxscore list from R/23 pipeline,
# keyed by position with curve_data tibbles giving cumulative production
# delta by age. The panel file stores the full 16-season player panel from
# R/16 which is the input to the aging curve fitting.

AGING_PANEL_CACHE_PATH <- here::here(
  "data", "season2_cache", "s2_week15_player_season_panel_cache.rds"
)

AGING_CURVES_CACHE_PATH <- here::here(
  "data", "season2_cache", "s2_week15_aging_curves_cache.rds"
)

# Seasons used to build the aging panel. Matches R/23 default.
AGING_PANEL_SEASONS <- 2010:2025

# Position groups supported by aging curves (R/23 fits these four)
AGING_POSITIONS <- c("QB", "RB", "WR", "TE")

# ------------------------------------------------------------------------------
# Injury proximity cache paths and constants (Build 2 integration)
# ------------------------------------------------------------------------------

# Path to R/25 experiment groups output (confirmed present)
INJURY_GROUPS_PATH <- here::here(
  "data", "season2_cache", "s2_week11_groups.rds"
)

INJURY_EFFECTS_PATH <- here::here(
  "data", "season2_cache", "s2_week11_effects.rds"
)

# Fraction of the R/25 PPG effect to apply as a preseason mu adjustment.
#
# NEUTRALIZED 2026-07-16 (was 0.30). Machinery intact for revival; only the
# weight is zeroed. Same pattern as COACH_PATTERN_WEIGHT.
#
# Reason. The weight was never the problem. R/25's treatment group is the
# problem, and it is degenerate by construction:
#
#   MIN_PRIOR_GAMES_W11    = 2   -> must play weeks 1 and 2
#   TREATMENT_RETURN_MAX_W11 = 4 -> must be back by week 4
#   => the absence can ONLY be week 3. There is one path through.
#
# Verified 2026-07-16 against s2_week11_groups.rds, all 15 of the 2025
# treatment players: first_absent_week = 3, return_week = 4,
# n_absent_weeks = 1, n_prior_games = 2. Fifteen rows, one configuration.
# Across all 15 analysis seasons: 163 of 163 treatment players are identical
# on those four fields.
#
# So this constant did not adjust for injury. It adjusted for missing week 3.
#
# Worse, the filters silently drop the players it is meant to serve. A player
# is in the R/25 file only if he EITHER missed zero weeks 1-8, OR missed time
# and returned by week 4. Everyone else fails both filters and vanishes.
# Checked against real 2025 season-enders: Nabers (ACL wk 4), Kittle, Conner,
# Murray, McLaurin, Ekeler, Purdy, Aiyuk, Godwin, Watson, Mixon, Daniels,
# Pearsall are NOT IN THE FILE. Dobbins (season-ending foot) is classified
# CONTROL, i.e. healthy, and gets zero adjustment.
#
# R/25 also failed 2 of its own 5 assumption checks on the run that produced
# this estimate (return-timing concentration 100%, parallel-trends prior-PPG
# gap 4.11) and reported an SMD of -15.5 on n_prior_games. Its own spec text
# warns that inferred absence may reflect benching. The estimate was consumed
# anyway.
#
# To revive: fix R/25's group construction first (widen the training window so
# onset is not forced to week 3, and admit non-returning players), re-estimate,
# then set this above 0.00. Do not raise it against the current groups file.
#
# Prior value and its rationale, preserved:
#   Full effect = -4.54 PPG. Using 0.30 (30%) as a conservative preseason
#   estimate since the player may be fully recovered by the start of next
#   season. Increase toward 1.0 if projecting mid-season after a recent return.
INJURY_EFFECT_FRACTION <- 0.00

# Sigma inflation multipliers by weeks missed in prior season
# More missed time = wider projection interval
# NEUTRALIZED 2026-07-16. MILD 1.10 -> 1.00. Machinery intact, same pattern as
# COACH_PATTERN_WEIGHT and INJURY_EFFECT_FRACTION. Do not delete the code path.
#
# WHY. The mu neutralize (INJURY_EFFECT_FRACTION -> 0.00) removed half of an
# adjustment sourced from a degenerate R/25 treatment group. This is the other
# half, and it fires on the same population.
#
# R/25's filters MIN_PRIOR_GAMES_W11 = 2 and TREATMENT_RETURN_MAX_W11 = 4 force
# every treatment player-season into ONE configuration: played weeks 1-2,
# missed week 3, returned week 4. Verified on the production file: all 163
# treatment rows across 2011-2025 are identical on first_absent_week,
# return_week, n_absent_weeks and n_prior_games. It is not an injury measure,
# it is a "who sat out week 3" measure.
#
# Consequence for 2026: 15 players got sigma 1.10 for missing week 3 of 2025,
# while Nabers (ACL, week 4), Kittle, Conner, Murray, McLaurin, Ekeler, Purdy,
# Aiyuk, Godwin, Watson, Mixon, Daniels and Pearsall are ABSENT from R/25's
# file entirely and got 1.00. Dobbins, season-ending foot, is classified as
# CONTROL. The widening is pointed at the healthiest group in the file. A hedge
# aimed the wrong way is worse than no hedge.
#
# MODERATE and SEVERE are left at their original values deliberately. Because
# n_absent_weeks is always 1, both branches are unreachable. Zeroing unreachable
# code would imply an evidentiary claim that was never made. They stay as-is,
# documented as dead, so a repaired R/25 revives them intact.
#
# REVIVE WHEN: an availability layer exists that projects expected games, and
# the widening keys off a real absence-duration signal rather than off R/25's
# week-3 artifact.
INJURY_SIGMA_MULTIPLIER_MILD    <- 1.00  # was 1.10. Neutralized, see above.
INJURY_SIGMA_MULTIPLIER_MODERATE <- 1.20  # UNREACHABLE: n_absent_weeks always 1
INJURY_SIGMA_MULTIPLIER_SEVERE  <- 1.30  # UNREACHABLE: n_absent_weeks always 1

# ------------------------------------------------------------------------------
# Usage ramp constants (Build 3 integration)
# ------------------------------------------------------------------------------

RAMP_FLAGS_PATH <- here::here(
  "data", "season2_cache", "s2_week12_ramp_flags.rds"
)

RAMP_EFFECTS_PATH <- here::here(
  "data", "season2_cache", "s2_week12_effects.rds"
)

RAMP_ROOKIE_EFFECTS_PATH <- here::here(
  "data", "season2_cache", "s2_week12_rookie_effects.rds"
)

# NEUTRALIZED 2026-07-16. 0.30 -> 0.00. Machinery intact, same pattern as
# COACH_PATTERN_WEIGHT and INJURY_EFFECT_FRACTION. Do not delete the code path.
#
# WHY. ESTIMAND MISMATCH. R/26 measures a WITHIN-season ramp: a player's usage
# rises, and production within that same season lags it. R/29 applied the
# estimate ACROSS seasons, to a player's prior_mu for the following year. Those
# are different quantities and the second does not follow from the first.
#
# Tested. Once own-season production is controlled, the cross-season adjusted
# coefficient collapses to -0.064 with p = 0.769. R/29 was applying
# 0.30 * -1.5533 = -0.466 PPG to 86 players on the strength of a coefficient
# that does not survive its own controls.
#
# This is the same disposition as the coach prior, the veteran talent
# multiplier, the NGS efficiency wire-in, the L2 EB gate, and the R/25 injury
# adjustment. Tested, negative, neutralized. The difference is that this one
# had been moving production boards since May.
#
# PRIOR VALUE AND ITS RATIONALE, preserved for revival:
#   Full vet effect = -1.55 PPG (within-season). 0.30 was chosen as a
#   conservative preseason haircut. Rookie effect (+0.19 PPG) was already
#   non-significant and never applied.
#
# REVIVE WHEN: a CROSS-season ramp instrument exists, or R/26 is re-specified
# so its estimand matches the way R/29 consumes it. The within-season effect is
# not disproven; it is being asked a question it does not answer.
RAMP_EFFECT_FRACTION <- 0.00  # was 0.30. Neutralized, see above.

# Players with n_nfl_seasons >= this threshold treated as veterans for ramp
# adjustment. Rookies (0-1 seasons) get no downward adjustment.
RAMP_VETERAN_THRESHOLD <- 2L

# ------------------------------------------------------------------------------
# Schema tag
# ------------------------------------------------------------------------------

R29_SCHEMA_TAG <- "s3_r29_v1"

# ------------------------------------------------------------------------------
# Null-coalescing operator (defined early; used throughout)
# ------------------------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

# ==============================================================================
# NSE DECLARATIONS (silence R CMD check NOTEs for tidyverse non-standard eval)
# ==============================================================================

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "nfl_gsis_id", "cfb_player_name", "player_name", "player_id", "position",
    "team", "season", "week", "game_id", "pos", "fantasypros_id",
    "score_final", "score_v1", "score_base", "score_enriched",
    "pred_base", "pred_enriched", "draft_year", "draft_position",
    "ppr_per_game_y13", "is_hit",
    "total_fantasy_points", "pass_fantasy_points", "rush_fantasy_points",
    "rec_fantasy_points",
    "ecr", "sd", "best", "worst", "tier", "pos_rank", "player_owned_avg",
    "n_games", "n_games_total", "ppr_per_game", "ppr_var",
    "prior_mu", "prior_sigma", "ytd_mean", "ytd_sigma", "ytd_n",
    "posterior_mu", "posterior_sigma",
    # Season 3 Wave A1 lifecycle columns and temporaries
    "draft_year", "pred_type", "years_exp", "lifecycle_state",
    "pred_selected", "has_prediction", "mu_fallback", "mu_translation",
    "sigma_translation", "has_translation", "has_history",
    ".variant", ".phased_out", ".is_rookie", ".has_hist_raw",
    ".fb_intercept", ".fb_slope",
    ".mu_for_blend", "ppr_per_game_y13",
    "current_age", "effective_historical_age", "aging_delta_ppg",
    "prior_mu_pre_age", "birth_date",
    "injury_group_prior_season", "injury_n_absent_weeks",
    "injury_mu_adj", "injury_sigma_multiplier",
    "injury_group", "injury_weeks_missed", "injury_sigma_mult",
    "ramp_flag_prior_season", "ramp_relative_change", "ramp_mu_adj",
    "ramp_flag", "is_vet_ramper",
    "volume_implied_ppg", "volume_blend_adj", "n_panel_seasons",
    "panel_fantasy_pts", "panel_ppg", "opp_per_game",
    "volume_implied_ppg_season", "hist_mean_panel",
    "median_ppg_per_opp", "pos_use", "pos_season_efficiency",
    "season", "low_volume", "pass_tds", "rec_tds", "rush_tds",
    "interceptions_thrown", "qb_dropbacks", "pos_vol_weight",
    "projection_ros", "weeks_remaining",
    "consensus_proj", "consensus_delta",
    "adp_rank", "projected_rank", "value_score",
    "name_normalized", "match_key",
    "defteam", "posteam", "play_type", "interception", "sack",
    "fumble_lost", "touchdown", "td_team", "return_team", "two_point_attempt",
    "two_point_conv_result", "season_type", "week_filter",
    "pts_allowed", "def_st_points",
    ".", "n", "x"
  ))
}

# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

# ------------------------------------------------------------------------------
# SLEEPER ROSTER REFRESH (Phase 4 fix, Week 15)
# ------------------------------------------------------------------------------
#
# nflreadr::load_rosters() depends on PFR which lags 1-4 weeks behind actual
# NFL roster moves during the offseason. Sleeper updates its player database
# within hours. The helpers below merge Sleeper team assignments into the
# nflreadr roster for current/future seasons. Historical seasons are left
# alone (no need to refresh 2024 rosters with current data).
#
# Sleeper team codes mostly match nflreadr. Known differences are mapped via
# SLEEPER_TO_NFLREADR_TEAM_MAP. Unmapped codes pass through unchanged.

# Sleeper-to-nflreadr team code crosswalk. Only codes that DIFFER need entries.
# Add to this list if new mismatches are discovered.
SLEEPER_TO_NFLREADR_TEAM_MAP <- c(
  "LAR" = "LA",     # Los Angeles Rams
  "AZ"  = "ARI",    # Arizona Cardinals (Sleeper uses AZ, nflfastR uses ARI)
  "JAC" = "JAX",    # Jacksonville Jaguars (defensive)
  "WSH" = "WAS"     # Washington Commanders (defensive)
)


#' Normalize Sleeper team codes to nflreadr conventions
#'
#' Sleeper uses LAR for the Rams; nflfastR/nflreadr uses LA. Other
#' historical differences (JAC vs JAX, WSH vs WAS) are handled defensively
#' even though current Sleeper data uses the nflreadr conventions for those.
#'
#' @param team_vec Character vector of team codes.
#' @return Character vector with mapped codes substituted.
#' @keywords internal
.normalize_sleeper_team_codes <- function(team_vec) {
  out <- team_vec
  for (sleeper_code in names(SLEEPER_TO_NFLREADR_TEAM_MAP)) {
    out[!is.na(out) & out == sleeper_code] <-
      SLEEPER_TO_NFLREADR_TEAM_MAP[[sleeper_code]]
  }
  out
}


#' Merge Sleeper team assignments into an nflreadr roster
#'
#' Where Sleeper and nflreadr disagree on a player's team, prefer Sleeper
#' (Sleeper updates within hours, nflreadr lags 1-4 weeks). Players Sleeper
#' marks as free agents have their team set to NA, which causes them to be
#' filtered out by .build_active_roster_filter() downstream.
#'
#' @param nflreadr_roster Tibble from nflreadr::load_rosters().
#' @param sleeper_data Tibble from get_all_sleeper_players().
#' @return Same schema as nflreadr_roster, with team refreshed.
#' @keywords internal
.merge_sleeper_roster <- function(nflreadr_roster, sleeper_data) {

  if (is.null(sleeper_data) || nrow(sleeper_data) == 0L) {
    message("  Sleeper data empty -- using nflreadr roster as-is.")
    return(nflreadr_roster)
  }

  # Build Sleeper lookup keyed by gsis_id, with normalized team codes
  sleeper_lookup <- sleeper_data %>%
    dplyr::filter(!is.na(.data$nfl_gsis_id)) %>%
    dplyr::transmute(
      gsis_id       = .data$nfl_gsis_id,
      sleeper_team  = .normalize_sleeper_team_codes(.data$team),
      sleeper_is_fa = .data$is_free_agent
    ) %>%
    dplyr::distinct(gsis_id, .keep_all = TRUE)

  # Track pre-merge team for diagnostic counts
  orig_team <- nflreadr_roster$team

  refreshed <- nflreadr_roster %>%
    dplyr::left_join(sleeper_lookup, by = "gsis_id") %>%
    dplyr::mutate(
      team = dplyr::case_when(
        dplyr::coalesce(.data$sleeper_is_fa, FALSE) ~ NA_character_,
        !is.na(.data$sleeper_team)                  ~ .data$sleeper_team,
        TRUE                                         ~ .data$team
      ),
      # Normalize FINAL team value -- catches both Sleeper inputs (already
      # normalized) and nflreadr inputs that fell through case_when's default
      # branch when no Sleeper record matched. Without this, players whose
      # nflreadr team uses an alternate code (e.g., "AZ" for Arizona) and
      # have no Sleeper gsis_id match retain the un-normalized value.
      team = .normalize_sleeper_team_codes(.data$team)
    ) %>%
    dplyr::select(-sleeper_team, -sleeper_is_fa)

  # Diagnostic
  n_team_changed <- sum(
    !is.na(orig_team) & !is.na(refreshed$team) & orig_team != refreshed$team,
    na.rm = TRUE
  )
  n_set_fa <- sum(!is.na(orig_team) & is.na(refreshed$team), na.rm = TRUE)

  message(glue(
    "  Sleeper refresh: ",
    "{n_team_changed} team change(s), ",
    "{n_set_fa} marked FA"
  ))

  refreshed
}


#' Add Sleeper-only skill-position players to the merged roster
#'
#' nflreadr's per-season roster lags during the offseason -- recently
#' drafted rookies and free-agent signings often don't appear in
#' nflreadr's 2026 roster for weeks. Sleeper updates within hours, so it
#' has these players with team, position, and depth_chart_order set.
#' Without this integration, those players are dropped by
#' .build_active_roster_filter() and never appear in the final
#' projection set even though R/28 has dynasty scores for them.
#'
#' Two-pass identity recovery:
#'   1. Sleeper rows with a usable nfl_gsis_id that aren't already in
#'      the merged roster -- added directly.
#'   2. Sleeper rows with no nfl_gsis_id -- gsis_id is recovered by
#'      unique-match name + position against R/28 prospects, then added.
#'
#' Only skill positions are integrated because R/29 only projects QB,
#' RB, WR, TE. Only players with a non-FA team assignment are added so
#' off-roster prospects don't pollute the active filter.
#'
#' @param roster Tibble from .merge_sleeper_roster() (post-merge nflreadr
#'   roster).
#' @param sleeper_data Tibble from get_all_sleeper_players().
#' @return Roster with additional Sleeper-only rookie rows bound on. New
#'   rows carry gsis_id, full_name, team, position, status = "ACT".
#'   Other columns are NA and filled by downstream joins as needed.
#' @keywords internal
.integrate_sleeper_rookies <- function(roster, sleeper_data) {

  if (is.null(sleeper_data) || nrow(sleeper_data) == 0L) {
    return(roster)
  }

  # Load R/28 prospects for name+position gsis_id recovery
  prospects <- tryCatch(
    readr::read_csv(PROSPECTS_PATH_DEFAULT, show_col_types = FALSE),
    error = function(e) NULL
  )

  if (is.null(prospects) || nrow(prospects) == 0L) {
    message("  Rookie integration skipped: R/28 prospects file unavailable")
    return(roster)
  }

  # Restrict Sleeper to active skill-position roster members
  active_sleeper <- sleeper_data %>%
    dplyr::filter(
      !is.na(.data$team),
      !dplyr::coalesce(.data$is_free_agent, FALSE),
      .data$position %in% c("QB", "RB", "WR", "TE")
    ) %>%
    dplyr::mutate(team = .normalize_sleeper_team_codes(.data$team))

  if (nrow(active_sleeper) == 0L) {
    return(roster)
  }

  roster_gsis <- roster %>%
    dplyr::filter(!is.na(.data$gsis_id)) %>%
    dplyr::pull(.data$gsis_id)

  # Pass A: Sleeper rows with gsis_id, not in roster
  path_a <- active_sleeper %>%
    dplyr::filter(
      !is.na(.data$nfl_gsis_id),
      !.data$nfl_gsis_id %in% roster_gsis
    ) %>%
    dplyr::transmute(
      gsis_id   = .data$nfl_gsis_id,
      full_name = .data$player_name,
      team      = .data$team,
      position  = .data$position,
      status    = "ACT"
    ) %>%
    dplyr::distinct(gsis_id, .keep_all = TRUE)

  # Pass B: Sleeper rows without gsis_id -> name+position match vs R/28
  name_col_prospect <- if ("cfb_player_name" %in% names(prospects)) {
    "cfb_player_name"
  } else if ("player_name" %in% names(prospects)) {
    "player_name"
  } else {
    NA_character_
  }

  if (is.na(name_col_prospect) ||
      !"nfl_gsis_id" %in% names(prospects) ||
      !"position" %in% names(prospects)) {
    path_b <- tibble::tibble(
      gsis_id   = character(),
      full_name = character(),
      team      = character(),
      position  = character(),
      status    = character()
    )
  } else {
    prospects_lookup <- prospects %>%
      dplyr::mutate(
        name_norm = .normalize_player_name(.data[[name_col_prospect]])
      ) %>%
      dplyr::filter(
        !is.na(.data$nfl_gsis_id),
        nchar(.data$name_norm) > 0,
        .data$position %in% c("QB", "RB", "WR", "TE")
      ) %>%
      dplyr::group_by(.data$name_norm, .data$position) %>%
      dplyr::filter(dplyr::n() == 1L) %>%
      dplyr::ungroup() %>%
      dplyr::select(name_norm, position,
                     recovered_gsis = .data$nfl_gsis_id)

    already_added <- path_a$gsis_id

    path_b <- active_sleeper %>%
      dplyr::filter(is.na(.data$nfl_gsis_id)) %>%
      dplyr::mutate(name_norm = .normalize_player_name(.data$player_name)) %>%
      dplyr::filter(nchar(.data$name_norm) > 0) %>%
      dplyr::inner_join(prospects_lookup,
                         by = c("name_norm", "position")) %>%
      dplyr::filter(
        !.data$recovered_gsis %in% roster_gsis,
        !.data$recovered_gsis %in% already_added
      ) %>%
      dplyr::transmute(
        gsis_id   = .data$recovered_gsis,
        full_name = .data$player_name,
        team      = .data$team,
        position  = .data$position,
        status    = "ACT"
      ) %>%
      dplyr::distinct(gsis_id, .keep_all = TRUE)
  }

  new_rows <- dplyr::bind_rows(path_a, path_b) %>%
    dplyr::distinct(gsis_id, .keep_all = TRUE)

  if (nrow(new_rows) == 0L) {
    message("  Rookie integration: no Sleeper-only players to add")
    return(roster)
  }

  message(glue(
    "  Rookie integration: ",
    "+{nrow(path_a)} via gsis_id, ",
    "+{nrow(path_b)} via name+position recovery, ",
    "{nrow(new_rows)} total added to roster"
  ))

  dplyr::bind_rows(roster, new_rows)
}


#' Load an nflreadr roster with optional Sleeper-based refresh
#'
#' Wraps nflreadr::load_rosters(). For current and future seasons, applies
#' the Sleeper merge to pick up recent trades/signings/releases. For
#' historical seasons, returns nflreadr's roster unchanged.
#'
#' If get_all_sleeper_players() fails (network, endpoint error), this
#' function falls back to the raw nflreadr roster with a warning -- the
#' pipeline never breaks because Sleeper is unavailable.
#'
#' @param season Integer. Season to load.
#' @param refresh_sleeper Logical. Apply Sleeper merge for current/future
#'   seasons. Default TRUE.
#' @return Tibble. Same schema as nflreadr::load_rosters().
#' @keywords internal
.load_current_roster <- function(season, refresh_sleeper = TRUE) {

  season <- as.integer(season)
  roster <- nflreadr::load_rosters(seasons = season)

  if (nrow(roster) == 0L) {
    return(roster)
  }

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  is_current_or_future <- season >= current_year

  if (!isTRUE(refresh_sleeper) || !is_current_or_future) {
    return(roster)
  }

  message(glue("  Refreshing season {season} roster with Sleeper data..."))

  sleeper_data <- tryCatch(
    get_all_sleeper_players(),
    error = function(e) {
      warning(glue(
        "Sleeper refresh failed: {e$message}\n",
        "Continuing with nflreadr roster only (may be stale)."
      ), call. = FALSE)
      NULL
    }
  )

  if (is.null(sleeper_data)) {
    return(roster)
  }

  refreshed <- .merge_sleeper_roster(roster, sleeper_data)
  .integrate_sleeper_rookies(refreshed, sleeper_data)
}


# ------------------------------------------------------------------------------
# .build_active_roster_filter
# ------------------------------------------------------------------------------

#' Identify active 2026-roster players for projection inclusion
#'
#' Returns the set of nfl_gsis_ids for players currently on an active 2026
#' NFL roster with a team assignment. Used to filter the priors before
#' projection so the engine does not output retired players, unsigned free
#' agents, or players who exist in R/28 but never made an active roster.
#'
#' When a known free agent signs (e.g., Stefon Diggs), nflreadr updates
#' typically within 24 hours; the next run picks up the new signing
#' automatically. No whitelist or manual override is needed because the
#' roster is re-pulled every run.
#'
#' @param roster Tibble from nflreadr::load_rosters(seasons = current_season).
#' @return Tibble: nfl_gsis_id, player_name, team, status.
#' @keywords internal
.build_active_roster_filter <- function(roster) {

  if (nrow(roster) == 0) {
    return(tibble::tibble(
      nfl_gsis_id = character(),
      player_name = character(),
      team        = character(),
      status      = character()
    ))
  }

  # Filter to active players with a team assigned. nflreadr roster status
  # values: ACT (active), RES (reserve/IR), PUP, EXE, INA (inactive),
  # CUT, DEV (practice squad). For projection purposes we want active
  # roster members regardless of starter/backup status -- the engine
  # cannot tell which backups will start. Status filter excludes only
  # players who are not on a team at all (CUT, INA without team).
  roster %>%
    dplyr::filter(
      !is.na(.data$gsis_id),
      !is.na(.data$team),
      nchar(.data$team) > 0
    ) %>%
    dplyr::transmute(
      nfl_gsis_id = .data$gsis_id,
      player_name = .data$full_name,
      team        = .data$team,
      status      = .data$status
    ) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)
}

# ------------------------------------------------------------------------------
# .resolve_player_identity
# ------------------------------------------------------------------------------

#' Resolve player display names via a three-check chain
#'
#' Resolves a readable display name for each player using three fallback
#' sources in order:
#'   Check 1: nflreadr 2026 roster full_name
#'   Check 2: nflreadr draft picks player_name (matched by gsis_id)
#'   Check 3: R/28 cfb_player_name from prospects CSV
#'
#' Players who fail all three checks are returned with NA player_name; the
#' caller decides whether to drop them.
#'
#' @param gsis_ids Character vector of nfl_gsis_id values to resolve.
#' @param roster_filter Tibble from .build_active_roster_filter().
#' @param prospects Tibble loaded from R/28 prospects CSV. Must contain
#'   nfl_gsis_id and cfb_player_name columns.
#' @param draft_picks Tibble from nflreadr::load_draft_picks() spanning
#'   recent draft years.
#' @return Tibble: nfl_gsis_id, player_name, name_source.
#' @keywords internal
.resolve_player_identity <- function(gsis_ids, roster_filter, prospects,
                                      draft_picks) {

  if (length(gsis_ids) == 0) {
    return(tibble::tibble(
      nfl_gsis_id = character(),
      player_name = character(),
      name_source = character()
    ))
  }

  base <- tibble::tibble(nfl_gsis_id = gsis_ids)

  # Check 1: 2026 roster
  check1 <- base %>%
    dplyr::left_join(
      roster_filter %>%
        dplyr::select(nfl_gsis_id, name_1 = player_name),
      by = "nfl_gsis_id"
    )

  # Check 2: draft picks
  draft_lookup <- if (!is.null(draft_picks) && nrow(draft_picks) > 0 &&
                       "gsis_id" %in% names(draft_picks) &&
                       "pfr_player_name" %in% names(draft_picks)) {
    draft_picks %>%
      dplyr::filter(!is.na(.data$gsis_id)) %>%
      dplyr::transmute(
        nfl_gsis_id = .data$gsis_id,
        name_2      = .data$pfr_player_name
      ) %>%
      dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)
  } else {
    tibble::tibble(nfl_gsis_id = character(), name_2 = character())
  }

  check2 <- check1 %>%
    dplyr::left_join(draft_lookup, by = "nfl_gsis_id")

  # Check 3: R/28 cfb_player_name
  prospects_lookup <- prospects %>%
    dplyr::filter(!is.na(nfl_gsis_id)) %>%
    dplyr::transmute(
      nfl_gsis_id = nfl_gsis_id,
      name_3      = .data$cfb_player_name
    ) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)

  check3 <- check2 %>%
    dplyr::left_join(prospects_lookup, by = "nfl_gsis_id")

  # Coalesce in order: roster > draft picks > prospects
  resolved <- check3 %>%
    dplyr::mutate(
      player_name = dplyr::coalesce(name_1, name_2, name_3),
      name_source = dplyr::case_when(
        !is.na(name_1) ~ "roster_2026",
        !is.na(name_2) ~ "draft_picks",
        !is.na(name_3) ~ "cfb_prospects",
        TRUE           ~ "unresolved"
      )
    ) %>%
    dplyr::select(nfl_gsis_id, player_name, name_source) %>%
    # Deduplicate: one row per gsis_id -- keep the first match (highest
    # priority source was already selected via coalesce above)
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)

  resolved
}

# ------------------------------------------------------------------------------
# .normalize_player_name
# ------------------------------------------------------------------------------

#' Normalize a player name for crosswalk matching
#'
#' Strips common suffixes (Jr., Sr., II, III, IV, V), removes punctuation,
#' collapses whitespace, lowercases. Used to join FantasyPros names to
#' nflfastR player_id via name + team + position.
#'
#' @param x Character vector of player names.
#' @return Character vector of normalized names.
#' @keywords internal
.normalize_player_name <- function(x) {
  if (length(x) == 0) return(character(0))

  out <- as.character(x)
  out <- tolower(out)

  # Strip common name suffixes
  out <- gsub("\\b(jr\\.?|sr\\.?|ii|iii|iv|v)\\b", "", out, perl = TRUE)

  # Remove apostrophes, periods, hyphens, commas
  out <- gsub("[[:punct:]]", "", out)

  # Collapse multiple spaces and trim
  out <- gsub("\\s+", " ", out)
  out <- trimws(out)

  out
}

# ------------------------------------------------------------------------------
# .build_fantasypros_crosswalk
# ------------------------------------------------------------------------------

#' Build crosswalk from FantasyPros records to nfl_gsis_id
#'
#' Joins FantasyPros data (which has player_name + team + pos but no
#' nfl_gsis_id) to nflreadr rosters (which has both). Match keys:
#' normalized_name + team + position. Reports unmatched count for diagnostic.
#'
#' Why position is included in the match key: name + team can collide when
#' two players share a name on the same roster (e.g., a QB and a DB with
#' the same surname on the same team). Adding position reduces this risk.
#'
#' @param fp_data Tibble from fp_rankings() or fp_projections(). Must include
#'   player_name, team, pos columns.
#' @param roster_season Integer. Season for the nflreadr roster lookup.
#' @return Tibble: fp_data with nfl_gsis_id appended. Unmatched rows have
#'   NA nfl_gsis_id; caller logs unmatched count.
#' @keywords internal
.build_fantasypros_crosswalk <- function(fp_data, roster_season) {

  if (nrow(fp_data) == 0) {
    return(dplyr::mutate(fp_data, nfl_gsis_id = character(0)))
  }

  required_cols <- c("player_name", "team", "pos")
  missing_cols <- setdiff(required_cols, names(fp_data))
  if (length(missing_cols) > 0) {
    stop(
      ".build_fantasypros_crosswalk(): FantasyPros data missing required ",
      "columns: ", paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Load roster for the season (Sleeper-refreshed for current/future seasons)
  roster <- .load_current_roster(season = roster_season)

  if (nrow(roster) == 0) {
    warning(
      ".build_fantasypros_crosswalk(): no roster data for season ",
      roster_season, ". Returning fp_data with NA nfl_gsis_id.",
      call. = FALSE
    )
    return(dplyr::mutate(fp_data, nfl_gsis_id = NA_character_))
  }

  # Normalize names on both sides
  roster_norm <- roster %>%
    dplyr::transmute(
      nfl_gsis_id     = .data$gsis_id,
      name_normalized = .normalize_player_name(.data$full_name),
      team            = .data$team,
      pos             = .data$position
    ) %>%
    dplyr::filter(!is.na(nfl_gsis_id), nchar(name_normalized) > 0)

  fp_normalized <- fp_data %>%
    dplyr::mutate(
      name_normalized = .normalize_player_name(.data$player_name)
    )

  # Primary join: name + team + position
  joined <- fp_normalized %>%
    dplyr::left_join(
      roster_norm,
      by = c("name_normalized", "team", "pos"),
      relationship = "many-to-many"
    )

  # For ties (rare; happens when roster has duplicate entries for a player
  # mid-season trade), keep the first match per source row
  joined <- joined %>%
    dplyr::distinct(
      dplyr::pick(dplyr::all_of(names(fp_normalized))),
      .keep_all = TRUE
    )

  # Fallback: for unmatched rows, retry without team (handles cases where
  # FantasyPros lists a player with a stale team and roster has the new team)
  unmatched_mask <- is.na(joined$nfl_gsis_id)
  if (any(unmatched_mask)) {
    fallback_input <- fp_normalized[unmatched_mask, , drop = FALSE]
    fallback <- fallback_input %>%
      dplyr::left_join(
        roster_norm %>% dplyr::select(-team),
        by = c("name_normalized", "pos"),
        relationship = "many-to-many"
      ) %>%
      dplyr::distinct(
        dplyr::pick(dplyr::all_of(names(fp_normalized))),
        .keep_all = TRUE
      )

    joined$nfl_gsis_id[unmatched_mask] <- fallback$nfl_gsis_id
  }

  # Drop helper column
  joined <- dplyr::select(joined, -name_normalized)

  joined
}

# ------------------------------------------------------------------------------
# .check_fp_cache_freshness
# ------------------------------------------------------------------------------

#' Check whether a FantasyPros cache file is within TTL
#'
#' @param path Character. File path to check.
#' @param ttl_hours Numeric. Cache TTL in hours.
#' @return Logical. TRUE if cache exists and is within TTL.
#' @keywords internal
.check_fp_cache_freshness <- function(path, ttl_hours = FP_CACHE_TTL_HOURS) {
  if (!file.exists(path)) return(FALSE)

  mtime <- file.info(path)$mtime
  age_hours <- as.numeric(difftime(Sys.time(), mtime, units = "hours"))

  age_hours < ttl_hours
}

# ------------------------------------------------------------------------------
# .compute_position_baseline
# ------------------------------------------------------------------------------

#' Compute position-level baseline PPR/game from prior seasons
#'
#' Used as the prior shrinkage target and as the boom/bust reference baseline.
#' Returns a tibble with one row per position and the league-average PPR/game
#' for qualified players.
#'
#' @param season Integer. Current season (baseline computed from prior 3 seasons).
#' @param min_games Integer. Minimum games to qualify (default 8).
#' @param cache_dir Character. Path to pbp cache directory.
#' @param scoring_settings List. Scoring settings passed through to ext scorer.
#' @return Tibble with columns: position, baseline_ppr_per_game, baseline_sigma.
#' @keywords internal
.compute_position_baseline <- function(season,
                                        min_games = 8L,
                                        cache_dir = CACHE_DIR_DEFAULT,
                                        scoring_settings = DEFAULT_SCORING_SETTINGS) {

  prior_seasons <- (season - 3L):(season - 1L)

  baselines <- purrr::map_dfr(prior_seasons, function(s) {

    pbp <- tryCatch(
      load_normalized_season(s, cache_dir = cache_dir),
      error = function(e) NULL
    )

    if (is.null(pbp)) return(NULL)

    roster <- nflreadr::load_rosters(seasons = s)

    fp <- do.call(calculate_fantasy_points_ext, c(
      list(pbp_data = pbp, roster_data = roster),
      scoring_settings
    ))

    # Aggregate to player-season
    player_season <- fp %>%
      dplyr::group_by(player_id, position) %>%
      dplyr::summarise(
        n_games        = dplyr::n_distinct(game_id),
        total_pts      = sum(total_fantasy_points, na.rm = TRUE),
        ppr_per_game   = total_pts / pmax(n_games, 1),
        .groups = "drop"
      ) %>%
      dplyr::filter(
        n_games >= min_games,
        position %in% SUPPORTED_POSITIONS,
        is.finite(ppr_per_game)
      )

    player_season %>%
      dplyr::mutate(season = s)
  })

  if (nrow(baselines) == 0) {
    warning(
      ".compute_position_baseline(): no usable baseline data from seasons ",
      paste(prior_seasons, collapse = ", "),
      ". Returning position means at 8.0 PPR/game with sigma 6.0.",
      call. = FALSE
    )
    return(tibble::tibble(
      position = SUPPORTED_POSITIONS,
      baseline_ppr_per_game = 8.0,
      baseline_sigma = 6.0
    ))
  }

  baselines %>%
    dplyr::group_by(position) %>%
    dplyr::summarise(
      baseline_ppr_per_game = mean(ppr_per_game, na.rm = TRUE),
      baseline_sigma        = stats::sd(ppr_per_game, na.rm = TRUE),
      .groups = "drop"
    )
}

# ------------------------------------------------------------------------------
# .compute_player_ytd_stats
# ------------------------------------------------------------------------------

#' Compute season-to-date fantasy stats per player through a given week
#'
#' Aggregates calculate_fantasy_points_ext() output to player-season-level
#' stats: mean PPR/game, variance of weekly PPR, games played through
#' current_week. Bye weeks are excluded from the games count.
#'
#' @param pbp Play-by-play tibble for the current season.
#' @param current_week Integer. Through which week to aggregate (inclusive).
#' @param roster Roster tibble for the season.
#' @param scoring_settings List of scoring parameters.
#' @return Tibble: one row per player with player_id, position, team,
#'   ytd_n_games, ytd_mean (PPR/game), ytd_sigma (SD of weekly PPR).
#' @keywords internal
.compute_player_ytd_stats <- function(pbp,
                                       current_week,
                                       roster,
                                       scoring_settings = DEFAULT_SCORING_SETTINGS) {

  current_week <- as.integer(current_week)
  if (current_week < 1L || current_week > NFL_REGULAR_SEASON_MAX_WEEK) {
    stop(
      ".compute_player_ytd_stats(): current_week must be 1-",
      NFL_REGULAR_SEASON_MAX_WEEK, ". Got: ", current_week,
      call. = FALSE
    )
  }

  # Filter pbp to regular season through current week
  pbp_ytd <- pbp %>%
    dplyr::filter(
      .data$season_type == "REG",
      .data$week <= current_week
    )

  if (nrow(pbp_ytd) == 0) {
    return(tibble::tibble(
      player_id = character(),
      position = character(),
      team = character(),
      ytd_n_games = integer(),
      ytd_mean = numeric(),
      ytd_sigma = numeric()
    ))
  }

  fp <- do.call(calculate_fantasy_points_ext, c(
    list(pbp_data = pbp_ytd, roster_data = roster),
    scoring_settings
  ))

  if (nrow(fp) == 0) {
    return(tibble::tibble(
      player_id = character(),
      position = character(),
      team = character(),
      ytd_n_games = integer(),
      ytd_mean = numeric(),
      ytd_sigma = numeric()
    ))
  }

  # Per-game totals first
  per_game <- fp %>%
    dplyr::group_by(player_id, player_name, position, game_id) %>%
    dplyr::summarise(
      game_points = sum(total_fantasy_points, na.rm = TRUE),
      team = dplyr::last(team),
      .groups = "drop"
    )

  # Player-level aggregates
  per_game %>%
    dplyr::group_by(player_id, position) %>%
    dplyr::summarise(
      team        = dplyr::last(team),
      ytd_n_games = dplyr::n_distinct(game_id),
      ytd_mean    = mean(game_points, na.rm = TRUE),
      # Use n-1 SD; if only one game, sigma is NA (handled downstream)
      ytd_sigma   = stats::sd(game_points, na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    dplyr::filter(position %in% SUPPORTED_POSITIONS)
}

# ------------------------------------------------------------------------------
# .compute_player_historical_stats
# ------------------------------------------------------------------------------

#' Compute historical NFL stats per player (prior seasons)
#'
#' For each player, returns the mean and SD of PPR/game across prior NFL
#' seasons, along with the count of qualifying seasons (used for precision
#' weighting against the translation model prior).
#'
#' @param season Integer. Current season; history is the 3 seasons preceding.
#' @param cache_dir Character. Path to pbp cache directory.
#' @param min_games Integer. Per-season minimum games to count as qualifying.
#' @param scoring_settings List of scoring parameters.
#' @return Tibble: player_id, position, n_nfl_seasons, n_games_total,
#'   hist_mean, hist_sigma. n_games_total is the summed game count across the
#'   qualifying seasons; it feeds the history precision in
#'   .blend_prior_components (hist_sigma is a PER-GAME sd, so precision must
#'   scale with games, not seasons).
#' @keywords internal
.compute_player_historical_stats <- function(season,
                                              cache_dir = CACHE_DIR_DEFAULT,
                                              min_games = 8L,
                                              scoring_settings = DEFAULT_SCORING_SETTINGS) {

  prior_seasons <- (season - 3L):(season - 1L)

  history <- purrr::map_dfr(prior_seasons, function(s) {

    pbp <- tryCatch(
      load_normalized_season(s, cache_dir = cache_dir),
      error = function(e) NULL
    )

    if (is.null(pbp)) return(NULL)

    pbp <- dplyr::filter(pbp, .data$season_type == "REG")
    roster <- nflreadr::load_rosters(seasons = s)

    fp <- do.call(calculate_fantasy_points_ext, c(
      list(pbp_data = pbp, roster_data = roster),
      scoring_settings
    ))

    per_game <- fp %>%
      dplyr::group_by(player_id, position, game_id) %>%
      dplyr::summarise(
        game_points = sum(total_fantasy_points, na.rm = TRUE),
        .groups = "drop"
      )

    season_stats <- per_game %>%
      dplyr::group_by(player_id, position) %>%
      dplyr::summarise(
        n_games      = dplyr::n_distinct(game_id),
        season_mean  = mean(game_points, na.rm = TRUE),
        season_sigma = stats::sd(game_points, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(n_games >= min_games)

    # Free memory between seasons
    rm(pbp, fp, per_game)
    invisible(gc(verbose = FALSE))

    season_stats %>% dplyr::mutate(season = s)
  })

  if (nrow(history) == 0) {
    return(tibble::tibble(
      player_id = character(),
      position = character(),
      n_nfl_seasons = integer(),
      n_games_total = integer(),
      hist_mean = numeric(),
      hist_sigma = numeric()
    ))
  }

  # Aggregate across seasons: mean of means; sigma via pooled variance
  history %>%
    dplyr::group_by(player_id, position) %>%
    dplyr::summarise(
      n_nfl_seasons = dplyr::n(),
      n_games_total = as.integer(sum(n_games, na.rm = TRUE)),
      hist_mean     = mean(season_mean, na.rm = TRUE),
      # Pooled SD across qualifying seasons (simple mean of within-season SDs)
      hist_sigma    = mean(season_sigma, na.rm = TRUE),
      .groups       = "drop"
    ) %>%
    dplyr::filter(position %in% SUPPORTED_POSITIONS)
}

# ------------------------------------------------------------------------------
# .blend_prior_components
# ------------------------------------------------------------------------------

#' Blend translation prior and NFL history prior via precision weighting
#'
#' Returns precision-weighted blended prior. Players with no NFL history
#' get the translation prior unchanged. Players with NFL history get a
#' blend weighted by the inverse variance of each source.
#'
#' Math:
#'   precision_translation = 1 / sigma_translation^2
#'   precision_history     = n_games_total / sigma_history^2
#'   blended_precision     = precision_translation + precision_history
#'   blended_mu = (precision_translation * mu_translation +
#'                 precision_history * mu_history) / blended_precision
#'   blended_sigma = sqrt(1 / blended_precision)
#'
#' [2026-07-27] BUG FIX: precision_history previously used n_nfl_seasons /
#' sigma_history^2. sigma_history is a PER-GAME sd, so the precision of the
#' history mean (an average of n_games_total games) is n_games_total /
#' sigma_game^2 -- the season-count version understated history precision by
#' roughly the games-per-season factor (~17x), over-weighting the translation
#' arm for every veteran. The function now takes n_games_total (summed
#' qualifying-season games from .compute_player_historical_stats); n_nfl_seasons
#' is retained only as the has-history gate.
#'
#' For rookies (n_nfl_seasons = 0), precision_history = 0 and the prior
#' reduces to the translation prior. For veterans with many games,
#' precision_history dominates and the prior converges to historical NFL
#' performance. Transition is automatic; no tier boundary.
#'
#' @param mu_translation Numeric. Prior mean from R/24 pred_base (PPR/game).
#' @param sigma_translation Numeric. Translation RMSE for the position.
#' @param mu_history Numeric. Mean PPR/game from NFL history (NA if no history).
#' @param sigma_history Numeric. Per-game SD of PPR/game from NFL history.
#' @param n_nfl_seasons Integer. Count of qualifying NFL seasons (gate only).
#' @param n_games_total Integer. Total games across the qualifying seasons;
#'   scales the history precision.
#' @return Named list: list(prior_mu, prior_sigma).
#' @keywords internal
.blend_prior_components <- function(mu_translation,
                                     sigma_translation,
                                     mu_history,
                                     sigma_history,
                                     n_nfl_seasons,
                                     n_games_total) {

  # Guard against zero or NA sigmas
  sigma_translation <- ifelse(is.na(sigma_translation) | sigma_translation <= 0,
                              5.0, sigma_translation)
  precision_translation <- 1 / (sigma_translation^2)

  has_history <- !is.na(mu_history) &
                 !is.na(sigma_history) &
                 sigma_history > 0 &
                 !is.na(n_nfl_seasons) &
                 n_nfl_seasons >= 1L &
                 !is.na(n_games_total) &
                 n_games_total >= 1L

  if (!has_history) {
    return(list(
      prior_mu    = mu_translation,
      prior_sigma = sigma_translation
    ))
  }

  # sigma_history is per-game, so the mean of n_games_total games has
  # precision n_games_total / sigma_game^2 (see bug-fix note above).
  precision_history <- n_games_total / (sigma_history^2)
  blended_precision <- precision_translation + precision_history

  blended_mu <- (precision_translation * mu_translation +
                 precision_history * mu_history) / blended_precision

  blended_sigma <- sqrt(1 / blended_precision)

  list(prior_mu = blended_mu, prior_sigma = blended_sigma)
}

# ------------------------------------------------------------------------------
# .load_injury_groups
# ------------------------------------------------------------------------------

#' Load R/25 injury proximity experiment groups
#'
#' Loads the pre-computed treatment/control group assignments from the R/25
#' injury proximity experiment. Filters to the most recent completed season
#' (current_season - 1) and treatment group players.
#'
#' The R/25 treatment definition: players who returned from injury in Weeks
#' 1-4 of the season. Effect size: -4.54 PPG vs healthy controls (bootstrap
#' CI: -5.55 to -3.53).
#'
#' @param current_season Integer. Season being projected (groups filtered to
#'   current_season - 1).
#' @return Tibble: player_id, position, group, n_absent_weeks, prior_season.
#' @keywords internal
.load_injury_groups <- function(current_season) {

  if (!file.exists(INJURY_GROUPS_PATH)) {
    warning(
      ".load_injury_groups(): R/25 groups file not found at: ",
      INJURY_GROUPS_PATH,
      ". Skipping injury proximity adjustments.",
      call. = FALSE
    )
    return(tibble::tibble(
      player_id = character(), position = character(),
      group = character(), n_absent_weeks = integer(),
      prior_season = integer()
    ))
  }

  prior_season <- as.integer(current_season) - 1L

  groups <- readRDS(INJURY_GROUPS_PATH) %>%
    dplyr::filter(
      season == prior_season,
      !is.na(player_id)
    ) %>%
    dplyr::transmute(
      player_id       = player_id,
      position        = position,
      group           = group,
      n_absent_weeks  = dplyr::coalesce(as.integer(n_absent_weeks), 0L),
      prior_season    = as.integer(season)
    ) %>%
    dplyr::distinct(player_id, .keep_all = TRUE)

  message(glue("  Injury groups (season {prior_season}): ",
               "{nrow(groups)} players -- ",
               "{sum(groups$group == 'treatment', na.rm = TRUE)} treatment, ",
               "{sum(groups$group == 'control', na.rm = TRUE)} control"))

  groups
}

# ------------------------------------------------------------------------------
# .compute_injury_adjustments
# ------------------------------------------------------------------------------

#' Compute per-player injury proximity adjustments to prior_mu and prior_sigma
#'
#' For players who were in the R/25 treatment group in the prior season
#' (returned from injury in Weeks 1-4), applies:
#'   - Downward prior_mu adjustment: INJURY_EFFECT_FRACTION * effect_ppg
#'   - Sigma inflation based on n_absent_weeks
#'
#' NEUTRALIZED 2026-07-16. INJURY_EFFECT_FRACTION = 0.00, so injury_mu_adj is
#' 0 for every player and this function is a no-op on mu. It still runs, still
#' emits its columns, and still sets injury_sigma_multiplier. See the comment
#' at INJURY_EFFECT_FRACTION for the evidence.
#'
#' Short version: R/25's treatment group can only ever contain players who
#' missed exactly week 3 (MIN_PRIOR_GAMES_W11 = 2 and TREATMENT_RETURN_MAX_W11
#' = 4 admit one configuration). All 163 treatment player-seasons across
#' 2011-2025 are identical on first_absent_week, return_week, n_absent_weeks,
#' and n_prior_games. Real season-ending injuries fail both R/25 filters and
#' are absent from the groups file entirely.
#'
#' Effect size (R/25 bootstrap estimate, reproduced 2026-07-16): -4.53733 PPG,
#' CI [-5.5497, -3.5298], n_treatment = 115, n_control = 1830. The estimate
#' reproduces exactly. It is the group it was estimated on that does not
#' support the use R/29 was making of it.
#'
#' @param priors Tibble from build_projection_priors() before injury adjustment.
#' @param injury_groups Tibble from .load_injury_groups().
#' @param effect_ppg Numeric. The R/25 estimated PPG effect for treatment.
#'   Default: extracted from INJURY_EFFECTS_PATH.
#' @return Tibble: nfl_gsis_id, injury_group_prior_season,
#'   injury_n_absent_weeks, injury_mu_adj, injury_sigma_multiplier.
#' @keywords internal
.compute_injury_adjustments <- function(priors, injury_groups,
                                          effect_ppg = -4.54) {

  if (nrow(injury_groups) == 0L) {
    return(tibble::tibble(
      nfl_gsis_id              = character(),
      injury_group_prior_season = character(),
      injury_n_absent_weeks    = integer(),
      injury_mu_adj            = numeric(),
      injury_sigma_multiplier  = numeric()
    ))
  }

  # Load the confirmed effect estimate from disk if available
  if (file.exists(INJURY_EFFECTS_PATH)) {
    effects <- readRDS(INJURY_EFFECTS_PATH)
    if (!is.null(effects$effect_ppg$estimate)) {
      effect_ppg <- effects$effect_ppg$estimate
    }
  }

  # Join priors to injury groups on player_id = nfl_gsis_id
  adj <- priors %>%
    dplyr::select(nfl_gsis_id) %>%
    dplyr::left_join(
      injury_groups %>%
        dplyr::select(player_id, group, n_absent_weeks),
      by = c("nfl_gsis_id" = "player_id")
    ) %>%
    dplyr::mutate(
      injury_group_prior_season = dplyr::coalesce(group, "none"),
      injury_n_absent_weeks     = dplyr::coalesce(n_absent_weeks, 0L),

      # Mu adjustment: apply only to treatment group players
      # Effect estimate is negative (-4.54); fraction damps it for preseason
      injury_mu_adj = dplyr::case_when(
        injury_group_prior_season == "treatment" ~
          INJURY_EFFECT_FRACTION * effect_ppg,
        TRUE ~ 0
      ),

      # Sigma multiplier: inflates uncertainty for any player with missed time
      injury_sigma_multiplier = dplyr::case_when(
        injury_n_absent_weeks >= 5L ~ INJURY_SIGMA_MULTIPLIER_SEVERE,
        injury_n_absent_weeks >= 3L ~ INJURY_SIGMA_MULTIPLIER_MODERATE,
        injury_n_absent_weeks >= 1L ~ INJURY_SIGMA_MULTIPLIER_MILD,
        TRUE ~ 1.0
      )
    ) %>%
    dplyr::select(
      nfl_gsis_id, injury_group_prior_season, injury_n_absent_weeks,
      injury_mu_adj, injury_sigma_multiplier
    )

  adj
}

# ------------------------------------------------------------------------------
# .load_ramp_flags
# ------------------------------------------------------------------------------

#' Load R/26 usage ramp flags for the prior season
#'
#' Loads the pre-computed ramp flag classifications from the R/26 usage ramp
#' experiment. Filters to the most recent completed season (current_season - 1)
#' and returns treatment (ramping) and control (flat/declining) players with
#' their relative change in usage share.
#'
#' R/26 key finding: veteran ramplers score -1.55 PPG less in Weeks 9-18 vs
#' stable/declining veterans (bootstrap CI: -2.13 to -0.93). Rookie ramplers
#' show a non-significant +0.19 PPG difference.
#'
#' @param current_season Integer. Season being projected.
#' @return Tibble: player_id, position, ramp_flag, relative_change,
#'   prior_season.
#' @keywords internal
.load_ramp_flags <- function(current_season) {

  if (!file.exists(RAMP_FLAGS_PATH)) {
    warning(
      ".load_ramp_flags(): R/26 ramp flags file not found at: ",
      RAMP_FLAGS_PATH,
      ". Skipping usage ramp adjustments.",
      call. = FALSE
    )
    return(tibble::tibble(
      player_id      = character(),
      position       = character(),
      ramp_flag      = logical(),
      relative_change = numeric(),
      prior_season   = integer()
    ))
  }

  prior_season <- as.integer(current_season) - 1L

  flags <- readRDS(RAMP_FLAGS_PATH) %>%
    dplyr::filter(season == prior_season, !is.na(player_id)) %>%
    dplyr::transmute(
      player_id       = player_id,
      position        = position,
      ramp_flag       = as.logical(ramp_flag),
      relative_change = as.numeric(relative_change),
      prior_season    = as.integer(season)
    ) %>%
    dplyr::distinct(player_id, .keep_all = TRUE)

  n_ramp    <- sum(flags$ramp_flag, na.rm = TRUE)
  n_control <- sum(!flags$ramp_flag, na.rm = TRUE)

  message(glue("  Ramp flags (season {prior_season}): ",
               "{nrow(flags)} players -- ",
               "{n_ramp} ramping, {n_control} flat/declining"))

  flags
}

# ------------------------------------------------------------------------------
# .compute_ramp_adjustments
# ------------------------------------------------------------------------------

#' Compute per-player usage ramp adjustments to prior_mu
#'
#' Applies the R/26 veteran ramp effect to ramping veterans in the prior
#' season. Key findings from R/26:
#'   Veteran ramp effect: -1.55 PPG (CI: -2.13 to -0.93) -- significant
#'   Rookie ramp effect:  +0.19 PPG (CI: -1.40 to  1.74) -- not significant
#'
#' Only veteran ramplers receive a downward mu adjustment. Rookies and
#' players not in the ramp_flags data receive no adjustment.
#'
#' @param priors Tibble from build_projection_priors() before ramp adjustment.
#' @param ramp_flags Tibble from .load_ramp_flags().
#' @param vet_effect_ppg Numeric. Vet ramp PPG effect (default loaded from
#'   RAMP_ROOKIE_EFFECTS_PATH).
#' @return Tibble: nfl_gsis_id, ramp_flag_prior_season, ramp_relative_change,
#'   ramp_mu_adj.
#' @keywords internal
.compute_ramp_adjustments <- function(priors, ramp_flags,
                                        vet_effect_ppg = -1.553) {

  if (nrow(ramp_flags) == 0L) {
    return(tibble::tibble(
      nfl_gsis_id          = character(),
      ramp_flag_prior_season = logical(),
      ramp_relative_change = numeric(),
      ramp_mu_adj          = numeric()
    ))
  }

  # Load confirmed vet effect estimate
  if (file.exists(RAMP_ROOKIE_EFFECTS_PATH)) {
    rookie_eff <- readRDS(RAMP_ROOKIE_EFFECTS_PATH)
    if (!is.null(rookie_eff$vet_effect$estimate)) {
      vet_effect_ppg <- rookie_eff$vet_effect$estimate
    }
  }

  adj <- priors %>%
    dplyr::select(nfl_gsis_id, n_nfl_seasons) %>%
    dplyr::left_join(
      ramp_flags %>% dplyr::select(player_id, ramp_flag, relative_change),
      by = c("nfl_gsis_id" = "player_id")
    ) %>%
    dplyr::mutate(
      ramp_flag_prior_season = dplyr::coalesce(ramp_flag, FALSE),
      ramp_relative_change   = dplyr::coalesce(relative_change, 0),

      # Apply vet ramp adjustment only:
      # - Player must be flagged as ramping (ramp_flag == TRUE)
      # - Player must be a veteran (n_nfl_seasons >= RAMP_VETERAN_THRESHOLD)
      # - Rookie ramp effect is non-significant; no adjustment applied
      is_vet_ramper = ramp_flag_prior_season &
                      n_nfl_seasons >= RAMP_VETERAN_THRESHOLD,

      ramp_mu_adj = dplyr::case_when(
        is_vet_ramper ~ RAMP_EFFECT_FRACTION * vet_effect_ppg,
        TRUE          ~ 0
      )
    ) %>%
    dplyr::select(
      nfl_gsis_id, ramp_flag_prior_season, ramp_relative_change, ramp_mu_adj
    )

  adj
}

# ------------------------------------------------------------------------------
# .compute_volume_efficiency_blend
# ------------------------------------------------------------------------------

#' Compute volume/efficiency 70/30 blend adjustment per player
#'
#' Implements the Week 4 predictive validity finding: volume/opportunity
#' features predict second-half fantasy production more reliably than
#' efficiency features. Adjusts each player's prior_mu toward the
#' volume-implied prediction by weighting volume at VOLUME_FEATURE_WEIGHT
#' (0.70) and the historical mean at EFFICIENCY_FEATURE_WEIGHT (0.30).
#'
#' Volume is defined position-specifically:
#'   QB:    qb_dropbacks per game (primary volume signal)
#'   RB:    (rush_attempts + targets) per game
#'   WR/TE: targets per game
#'
#' Volume-implied PPG = player_opps_per_game x
#'                      position_season_median_ppg_per_opp
#'
#' The blend adjustment = 0.70 x (volume_implied - hist_mean) pulls the
#' prior 70% of the way toward the volume-based prediction. Positive
#' adjustment for high-volume/below-average-efficiency players; negative
#' for low-volume/above-average-efficiency players.
#'
#' @param priors Tibble from build_projection_priors() after Builds 1-3.
#' @param panel_data Tibble. Cached R/16 player-season panel.
#' @param season Integer. Current projected season.
#' @param min_games Integer. Minimum games to qualify per season. Default 8.
#' @return Tibble: nfl_gsis_id, volume_implied_ppg, volume_blend_adj,
#'   n_panel_seasons.
#' @keywords internal
.compute_volume_efficiency_blend <- function(priors, panel_data,
                                               season,
                                               min_games = 8L,
                                               scoring_settings = DEFAULT_SCORING_SETTINGS) {

  prior_seasons <- (season - 3L):(season - 1L)

  # League-aware scoring for the volume prior. Merge the caller's ruleset over
  # DEFAULT_SCORING_SETTINGS so any keys the league omits fall back to defaults.
  sc <- utils::modifyList(DEFAULT_SCORING_SETTINGS, scoring_settings %||% list())

  # Filter panel to qualifying player-seasons
  panel_use <- panel_data %>%
    dplyr::filter(
      season %in% prior_seasons,
      games_played >= min_games,
      !is.na(player_id),
      position_group %in% SUPPORTED_POSITIONS | position %in% SUPPORTED_POSITIONS
    ) %>%
    dplyr::mutate(
      # Resolve position from both columns -- position_group is set by R/16
      pos_use = dplyr::case_when(
        position %in% SUPPORTED_POSITIONS ~ position,
        position_group %in% SUPPORTED_POSITIONS ~ position_group,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(pos_use))

  if (nrow(panel_use) == 0L) {
    return(tibble::tibble(
      nfl_gsis_id       = character(),
      volume_implied_ppg = numeric(),
      volume_blend_adj  = numeric(),
      n_panel_seasons   = integer()
    ))
  }

  # sacks_taken is the only newly-referenced term (for a QB sack penalty). It
  # comes from R/16's passing builder; guard so an older panel cache without it
  # cannot crash the run. Missing means no sacks recorded, so 0 is correct.
  if (!"sacks_taken" %in% names(panel_use)) panel_use$sacks_taken <- 0

  # Compute PPG per player-season from panel stats using the LEAGUE'S scoring.
  #
  # WHY (decision, 2026-07): this volume prior used to hardcode PPR, so two
  # leagues with different scoring got the same volume-implied prior. That
  # contradicts the per-league ranking goal. Now it scores from `sc` (the
  # league ruleset). This is the "Option A" fix: the R/16 panel holds SEASON
  # AGGREGATES, so only scoring terms expressible from season totals are honored
  # here -- per-yard, per-TD, INT, PPR, the per-attempt rush bonus, and a QB
  # sack penalty. Play-level bonuses (tiered PPR, long-TD, 100/300/400-yard
  # bonuses, first-down points, 2PT) CANNOT be applied at the aggregate level
  # and are intentionally omitted from THIS prior signal only. They are still
  # fully applied to the final projection and everything downstream, which score
  # play-by-play via calculate_fantasy_points_ext(). Backlog: rebuild the panel
  # at play level to make even this prior bonus-aware.
  panel_use <- panel_use %>%
    dplyr::mutate(
      # Coalesce each stat term to 0 before summing. The R/16 panel stores NA
      # (not 0) for categories a player never accrued -- passing_yards for a TE,
      # rushing_yards in a season with no carries, etc. Without coalesce, NA
      # propagates through the sum, panel_fantasy_pts becomes NA, panel_ppg
      # becomes NA, and the is.finite(panel_ppg) filter below silently drops the
      # player from the volume signal. NA here means zero activity, so 0 is the
      # correct substitution. (Diagnosed: 93% of window player-seasons were
      # being dropped, TEs hit hardest at 209/213.)
      panel_fantasy_pts =
        dplyr::coalesce(passing_yards, 0)        * sc$pass_yd +
        dplyr::coalesce(pass_tds, 0)             * sc$pass_td +
        dplyr::coalesce(interceptions_thrown, 0) * sc$pass_int +
        dplyr::coalesce(sacks_taken, 0)          * sc$sack_penalty +
        dplyr::coalesce(rushing_yards, 0)        * sc$rush_yd +
        dplyr::coalesce(rush_tds, 0)             * sc$rush_td +
        dplyr::coalesce(rush_attempts, 0)        * sc$rush_att_bonus +
        dplyr::coalesce(receiving_yards, 0)      * sc$rec_yd +
        dplyr::coalesce(rec_tds, 0)              * sc$rec_td +
        dplyr::coalesce(receptions, 0)           * sc$ppr,
      panel_ppg = panel_fantasy_pts / pmax(games_played, 1),

      # Position-specific opportunity rate (touches per game)
      opp_per_game = dplyr::case_when(
        pos_use == "QB" ~ (dplyr::coalesce(qb_dropbacks, 0L) +
                           dplyr::coalesce(rush_attempts, 0L)) /
                          pmax(games_played, 1),
        pos_use == "RB" ~ (dplyr::coalesce(rush_attempts, 0L) +
                           dplyr::coalesce(targets, 0L)) /
                          pmax(games_played, 1),
        pos_use %in% c("WR", "TE") ~ dplyr::coalesce(targets, 0L) /
                                      pmax(games_played, 1),
        TRUE ~ 0
      )
    ) %>%
    dplyr::filter(opp_per_game > 0, is.finite(panel_ppg))

  # Position-season median PPG per opportunity unit
  # (used as the "average efficiency" benchmark)
  pos_season_efficiency <- panel_use %>%
    dplyr::filter(!low_volume) %>%
    dplyr::group_by(season, pos_use) %>%
    dplyr::summarise(
      median_ppg_per_opp = stats::median(
        panel_ppg / opp_per_game, na.rm = TRUE
      ),
      .groups = "drop"
    )

  # Join efficiency benchmark and compute volume-implied PPG
  panel_vol <- panel_use %>%
    dplyr::left_join(
      pos_season_efficiency,
      by = c("season", "pos_use")
    ) %>%
    dplyr::mutate(
      volume_implied_ppg_season = opp_per_game * dplyr::coalesce(
        median_ppg_per_opp, 1
      )
    )

  # Average volume-implied PPG across qualifying seasons per player
  player_vol <- panel_vol %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(
      volume_implied_ppg = mean(volume_implied_ppg_season, na.rm = TRUE),
      hist_mean_panel    = mean(panel_ppg, na.rm = TRUE),
      n_panel_seasons    = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_panel_seasons >= 1L, is.finite(volume_implied_ppg))

  # Join to priors and compute blend adjustment.
  # Apply only to players with real NFL history -- the volume-implied PPG comes
  # from the R/16 panel, which only exists for players who have played. Gate on
  # has_history (robust to lifecycle-label changes) rather than enumerating
  # prior_source states; the !is.na(volume_implied_ppg) filter below is the
  # second gate (player must actually be in the panel). Rookies and pure-
  # fallback players have no panel rows and are excluded by both gates.
  adj <- priors %>%
    dplyr::select(nfl_gsis_id, prior_source, has_history, prior_mu_pre_age,
                  n_nfl_seasons, position) %>%
    dplyr::filter(has_history) %>%
    dplyr::left_join(
      player_vol %>% dplyr::select(player_id, volume_implied_ppg,
                                    n_panel_seasons),
      by = c("nfl_gsis_id" = "player_id")
    ) %>%
    dplyr::filter(!is.na(volume_implied_ppg)) %>%
    dplyr::mutate(
      # Position-specific volume weight:
      #   QB:    50/50 -- efficiency signal more informative per position
      #   RB/WR/TE: 70/30 -- volume is the primary predictor (Week 4 finding)
      pos_vol_weight = dplyr::if_else(
        position == "QB",
        QB_VOLUME_FEATURE_WEIGHT,
        VOLUME_FEATURE_WEIGHT
      ),
      volume_blend_adj = pos_vol_weight *
                          (volume_implied_ppg - prior_mu_pre_age),
      volume_blend_adj = pmax(pmin(volume_blend_adj, 4.0), -4.0)
    ) %>%
    dplyr::select(
      nfl_gsis_id, volume_implied_ppg, volume_blend_adj, n_panel_seasons
    )

  adj
}

# ------------------------------------------------------------------------------
# .load_or_build_aging_curves
# ------------------------------------------------------------------------------

#' Load cached aging curves, or build them from scratch via R/16 + R/23
#'
#' On first run, builds the 16-season player-season panel via
#' build_player_season_panel() (R/16), then runs run_aging_curve_pipeline()
#' (R/23) to fit position-specific aging curves. Caches both outputs to
#' disk for fast loading on subsequent runs.
#'
#' First run runtime: 15-25 minutes (panel build is the slow step).
#' Subsequent runs: ~10 seconds (load from cache).
#'
#' @param force_rebuild Logical. Force a fresh rebuild even if cache exists.
#'   Default FALSE.
#' @param cache_dir Character. Cache directory path.
#' @return Named list with elements:
#'   panel  -- the R/16 player-season panel
#'   curves -- the curves_boxscore element from R/23 pipeline:
#'             named list per position with $curve_data containing
#'             age_at_season_start, mean_delta, fitted_loess, fitted_quad
#' @keywords internal
.load_or_build_aging_curves <- function(force_rebuild = FALSE,
                                          cache_dir = CACHE_DIR_DEFAULT) {

  panel_cached  <- file.exists(AGING_PANEL_CACHE_PATH) && !force_rebuild
  curves_cached <- file.exists(AGING_CURVES_CACHE_PATH) && !force_rebuild

  if (panel_cached && curves_cached) {
    message("  Loading aging curves from cache...")
    panel  <- readRDS(AGING_PANEL_CACHE_PATH)
    curves <- readRDS(AGING_CURVES_CACHE_PATH)

    # Normalize stale cache layout. Older builds wrapped the position-keyed
    # curves as list(curves = <position-keyed>, built_for_season = <int>).
    # .lookup_aging_delta() expects the position-keyed list directly, where
    # names() are QB/RB/WR/TE. Unwrap so a wrapped cache works without a
    # rebuild; this is a no-op on a correctly-keyed cache. Without it,
    # position %in% names(curves) is always FALSE and every aging delta
    # collapses to 0 (aging applied to 0 players).
    if (is.list(curves) && "curves" %in% names(curves) &&
        !any(SUPPORTED_POSITIONS %in% names(curves))) {
      curves <- curves$curves
    }

    message(glue("    Panel: {nrow(panel)} rows; ",
                 "Curves: {length(curves)} positions"))
    return(list(panel = panel, curves = curves))
  }

  message(glue(""))
  message(glue("  FIRST-RUN AGING CURVE BUILD"))
  message(glue("  ============================"))
  message(glue("  Building R/16 player-season panel (16 seasons)..."))
  message(glue("  This step takes 10-15 minutes. Subsequent runs load from cache."))
  message(glue(""))

  panel <- build_player_season_panel(
    seasons   = AGING_PANEL_SEASONS,
    cache_dir = cache_dir,
    verbose   = TRUE
  )

  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  saveRDS(panel, AGING_PANEL_CACHE_PATH)
  message(glue("  Panel cached: {basename(AGING_PANEL_CACHE_PATH)}"))

  message(glue(""))
  message(glue("  Fitting R/23 aging curves..."))

  aging_results <- run_aging_curve_pipeline(
    panel      = panel,
    save_plots = FALSE,
    verbose    = TRUE
  )

  curves <- aging_results$curves_boxscore

  saveRDS(curves, AGING_CURVES_CACHE_PATH)
  message(glue("  Curves cached: {basename(AGING_CURVES_CACHE_PATH)}"))

  list(panel = panel, curves = curves)
}

# ------------------------------------------------------------------------------
# .compute_aging_adjustments
# ------------------------------------------------------------------------------

#' Compute per-player aging-based adjustment to prior_mu
#'
#' For each player with a known birth_date and recent NFL history, computes
#' the expected production delta from their effective historical age to
#' their 2026 age using the position-specific aging curve from R/23.
#'
#' The adjustment is the difference between the curve's cumulative value at
#' the player's 2026 age and at their effective historical age. Positive
#' values mean the player is expected to improve due to aging; negative
#' values mean they are expected to decline.
#'
#' For players past the curve's age range (e.g., a 40-year-old whose age
#' exceeds the maximum age observed in training), the curve value is
#' linearly extrapolated using the slope at the last two observed ages.
#'
#' @param priors Tibble with nfl_gsis_id, position, n_nfl_seasons. Output
#'   from build_projection_priors() before aging adjustment.
#' @param roster Tibble from nflreadr::load_rosters() for current season.
#'   Must include gsis_id and birth_date columns.
#' @param curves List of per-position aging curves from R/23
#'   (curves_boxscore from run_aging_curve_pipeline output). Each element
#'   has $curve_data with age_at_season_start and fitted_loess columns.
#' @param current_season Integer. Season being projected (e.g., 2026L).
#' @return Tibble: nfl_gsis_id, current_age, effective_historical_age,
#'   aging_delta_ppg.
#' @keywords internal
.compute_aging_adjustments <- function(priors, roster, curves,
                                         current_season) {

  if (nrow(priors) == 0L) {
    return(tibble::tibble(
      nfl_gsis_id              = character(),
      current_age              = numeric(),
      effective_historical_age = numeric(),
      aging_delta_ppg          = numeric()
    ))
  }

  current_season <- as.integer(current_season)

  # Build age lookup from roster birth_date
  # NFL convention: age computed as of September 1 of season year
  age_anchor <- as.Date(paste0(current_season, "-09-01"))

  ages <- roster %>%
    dplyr::filter(!is.na(.data$gsis_id), !is.na(.data$birth_date)) %>%
    dplyr::transmute(
      nfl_gsis_id = .data$gsis_id,
      birth_date  = as.Date(.data$birth_date),
      current_age = as.numeric(age_anchor - birth_date) / 365.25
    ) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)

  # Join ages to priors
  base <- priors %>%
    dplyr::select(nfl_gsis_id, position, n_nfl_seasons) %>%
    dplyr::left_join(ages, by = "nfl_gsis_id")

  # For players with NFL history, effective_historical_age = midpoint of
  # their qualifying seasons. We computed history from prior 3 seasons,
  # so midpoint = current_age - (current_season - midpoint_season).
  # Midpoint season for a 3-season window ending in (current_season - 1) =
  # current_season - 2.
  base <- base %>%
    dplyr::mutate(
      effective_historical_age = dplyr::case_when(
        is.na(current_age)              ~ NA_real_,
        n_nfl_seasons == 0L             ~ NA_real_,
        TRUE                            ~ current_age - 2.0
      )
    )

  # Deduplication: one row per gsis_id -- duplicate roster entries (mid-season
  # trades) can produce multiple age rows for the same player
  base <- base %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)

  # Look up aging delta per player from the position curve
  base <- base %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      aging_delta_ppg = .lookup_aging_delta(
        position                  = position,
        current_age               = current_age,
        effective_historical_age  = effective_historical_age,
        curves                    = curves
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(nfl_gsis_id, current_age, effective_historical_age,
                  aging_delta_ppg)

  base
}

# ------------------------------------------------------------------------------
# .lookup_aging_delta
# ------------------------------------------------------------------------------

#' Look up aging delta from a position-specific curve
#'
#' Returns the expected PPG change from effective_historical_age to
#' current_age based on the fitted aging curve. Returns 0 if either age
#' is missing or if no curve exists for the position.
#'
#' @param position Character. QB, RB, WR, or TE.
#' @param current_age Numeric. Player's age at the start of the projected season.
#' @param effective_historical_age Numeric. Midpoint age of player's
#'   historical seasons used to compute prior_mu.
#' @param curves List of per-position curves from R/23.
#' @return Numeric. Aging delta in PPG units.
#' @keywords internal
.lookup_aging_delta <- function(position, current_age,
                                 effective_historical_age, curves) {

  if (is.na(current_age) || is.na(effective_historical_age)) return(0)
  if (is.null(curves) || !position %in% names(curves)) return(0)

  curve_obj <- curves[[position]]
  if (is.null(curve_obj) || is.null(curve_obj$curve_data)) return(0)

  curve_df <- curve_obj$curve_data
  if (nrow(curve_df) == 0L) return(0)
  if (!"fitted_loess" %in% names(curve_df)) return(0)

  # Use fitted_loess as the primary curve; fall back to fitted_quad if
  # loess values are all NA
  curve_col <- if (any(!is.na(curve_df$fitted_loess))) "fitted_loess"
               else "fitted_quad"

  ages_in_curve <- curve_df$age_at_season_start
  values_in_curve <- curve_df[[curve_col]]

  # Drop NAs
  keep <- !is.na(values_in_curve)
  if (sum(keep) < 2L) return(0)
  ages_in_curve <- ages_in_curve[keep]
  values_in_curve <- values_in_curve[keep]

  # Interpolate curve value at a given age. Use linear interp for ages
  # within the curve range; extrapolate using endpoint slope for ages
  # outside the curve range.
  interp_curve <- function(age) {
    if (age >= min(ages_in_curve) && age <= max(ages_in_curve)) {
      stats::approx(x = ages_in_curve, y = values_in_curve,
                    xout = age, rule = 2)$y
    } else if (age < min(ages_in_curve)) {
      # Extrapolate below curve range using first two points
      slope <- (values_in_curve[2] - values_in_curve[1]) /
               (ages_in_curve[2] - ages_in_curve[1])
      values_in_curve[1] + slope * (age - ages_in_curve[1])
    } else {
      # Extrapolate above curve range using last two points
      n <- length(ages_in_curve)
      slope <- (values_in_curve[n] - values_in_curve[n - 1]) /
               (ages_in_curve[n] - ages_in_curve[n - 1])
      values_in_curve[n] + slope * (age - ages_in_curve[n])
    }
  }

  delta <- interp_curve(current_age) - interp_curve(effective_historical_age)

  # Sanity guard: cap any single-year aging adjustment at +/- 5 PPG.
  # Realistic year-over-year aging deltas are typically 0-2 PPG; anything
  # larger likely reflects extrapolation noise.
  max(min(delta, 5), -5)
}

# ------------------------------------------------------------------------------
# .def_st_pts_allowed_points
# ------------------------------------------------------------------------------

#' Map points allowed to Sleeper DEF/ST tier points
#'
#' @param pts_allowed Numeric vector of points allowed.
#' @return Numeric vector of tier points.
#' @keywords internal
.def_st_pts_allowed_points <- function(pts_allowed) {
  dplyr::case_when(
    is.na(pts_allowed)     ~ NA_real_,
    pts_allowed == 0       ~ DEF_PTS_ALLOW_TIERS$pts_allow_0,
    pts_allowed <= 6       ~ DEF_PTS_ALLOW_TIERS$pts_allow_1_6,
    pts_allowed <= 13      ~ DEF_PTS_ALLOW_TIERS$pts_allow_7_13,
    pts_allowed <= 20      ~ DEF_PTS_ALLOW_TIERS$pts_allow_14_20,
    pts_allowed <= 27      ~ DEF_PTS_ALLOW_TIERS$pts_allow_21_27,
    pts_allowed <= 34      ~ DEF_PTS_ALLOW_TIERS$pts_allow_28_34,
    TRUE                   ~ DEF_PTS_ALLOW_TIERS$pts_allow_35p
  )
}

# ==============================================================================
# EXPORT 1: compute_prior_weight
# ==============================================================================

#' Compute prior weight for a given NFL week
#'
#' Returns the weight to place on the PRIOR projection for a given week.
#' Weight declines linearly from PRIOR_WEIGHT_START (1.0) in Week 1 to
#' PRIOR_WEIGHT_FLOOR (0.05) at Week 18. Observed weight = 1 - prior_weight.
#'
#' Carried forward from Season 1 R/14 exactly. The week schedule was tuned
#' against the Week 4 predictive validity finding (3+ weeks of observed
#' data starts to outpredict prior). Crossover occurs near Week 9.
#'
#' NFL context: Early season (weeks 1-3) the prior dominates because sample
#' sizes are too small to trust observed rates. By Week 9+ observed data
#' becomes the primary signal. The floor at 0.05 ensures the prior never
#' fully disappears -- even in Week 18 it provides a small regularization
#' pull against single-game anomalies.
#'
#' @param week Integer week number (1-18). Playoff weeks (19+) are outside
#'   projection scope and produce an error.
#' @return Numeric weight in [PRIOR_WEIGHT_FLOOR, PRIOR_WEIGHT_START].
#'
#' @examples
#' compute_prior_weight(1)   # 1.00 (full prior)
#' compute_prior_weight(9)   # ~0.50 (crossover)
#' compute_prior_weight(18)  # 0.05 (floor)
#'
#' @seealso update_projection_with_ytd, run_projection_engine
#' @export
compute_prior_weight <- function(week) {

  if (!is.numeric(week) || length(week) != 1L ||
      week < 1 || week > NFL_REGULAR_SEASON_MAX_WEEK) {
    stop(
      "compute_prior_weight(): week must be a single integer 1-",
      NFL_REGULAR_SEASON_MAX_WEEK,
      ". Got: ", paste(week, collapse = ", "),
      ". Playoff weeks (19+) are outside projection scope.",
      call. = FALSE
    )
  }

  slope <- (PRIOR_WEIGHT_START - PRIOR_WEIGHT_FLOOR) /
           (NFL_REGULAR_SEASON_MAX_WEEK - 1)

  weight <- PRIOR_WEIGHT_START - slope * (week - 1)

  max(weight, PRIOR_WEIGHT_FLOOR)
}

# ==============================================================================
# EXPORT 2: load_adp_fantasypros
# ==============================================================================

#' Load FantasyPros consensus ADP and crosswalk to nfl_gsis_id
#'
#' Pulls FantasyPros consensus cheatsheet rankings via ffpros::fp_rankings()
#' and joins to nflreadr roster data to provide nfl_gsis_id. Cached to
#' filesystem with 24h TTL by default.
#'
#' @param season Integer. Season for the roster crosswalk lookup.
#' @param scoring Character. Scoring format: "PPR" (default), "HALF", "STD".
#'   Passed to fp_rankings() page selection.
#' @param use_cache Logical. Use filesystem cache if fresh. Default TRUE.
#' @param force_refresh Logical. Force re-pull even if cache is fresh.
#'   Default FALSE.
#' @param cache_dir Character. Cache directory path.
#' @return Tibble with columns: nfl_gsis_id, player_name, pos, team,
#'   adp_rank (= rank), adp_ecr (= ecr), adp_sd (= sd), adp_best (= best),
#'   adp_worst (= worst), adp_tier (= tier), fantasypros_id.
#'
#' @details
#' Crosswalk to nfl_gsis_id uses normalized name + team + position. Players
#' without a crosswalk match (typically very late ADP rookies or recently
#' added free agents) have NA nfl_gsis_id. The unmatched count is printed
#' as a diagnostic message; if unmatched exceeds 5% the function warns.
#'
#' ADP is updated daily on FantasyPros. The 24h cache TTL avoids repeated
#' scraping while keeping data fresh enough for in-season projection use.
#'
#' @examples
#' \dontrun{
#' adp <- load_adp_fantasypros(season = 2025L)
#' adp <- load_adp_fantasypros(season = 2025L, force_refresh = TRUE)
#' }
#'
#' @seealso load_consensus_projections_fantasypros, build_projection_priors
#' @export
load_adp_fantasypros <- function(season,
                                  scoring = "PPR",
                                  use_cache = TRUE,
                                  force_refresh = FALSE,
                                  cache_dir = CACHE_DIR_DEFAULT) {

  season <- as.integer(season)
  scoring <- toupper(scoring)

  if (!scoring %in% c("PPR", "HALF", "STD")) {
    stop(
      "load_adp_fantasypros(): scoring must be 'PPR', 'HALF', or 'STD'. ",
      "Got: ", scoring,
      call. = FALSE
    )
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_path <- file.path(
    cache_dir,
    glue("s2_week15_fp_adp_cache_{season}_{scoring}.rds")
  )

  if (use_cache && !force_refresh &&
      .check_fp_cache_freshness(cache_path)) {
    message(glue("Loading FantasyPros ADP from cache: {basename(cache_path)}"))
    return(readRDS(cache_path))
  }

  message(glue("Pulling FantasyPros ADP via ffpros::fp_rankings() ",
               "(scoring = {scoring})..."))

  fp_page <- switch(
    scoring,
    "PPR"  = "consensus-cheatsheets",
    "HALF" = "consensus-cheatsheets",  # ffpros returns multi-scoring at once
    "STD"  = "consensus-cheatsheets"
  )

  adp_raw <- tryCatch(
    ffpros::fp_rankings(page = fp_page),
    error = function(e) {
      stop(
        "load_adp_fantasypros(): fp_rankings() failed. ",
        "Error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (nrow(adp_raw) == 0) {
    stop(
      "load_adp_fantasypros(): fp_rankings() returned 0 rows. ",
      "FantasyPros may be down or the page name has changed.",
      call. = FALSE
    )
  }

  # Crosswalk to nfl_gsis_id
  adp_xw <- .build_fantasypros_crosswalk(adp_raw, roster_season = season)

  n_matched <- sum(!is.na(adp_xw$nfl_gsis_id))
  n_unmatched <- sum(is.na(adp_xw$nfl_gsis_id))
  pct_unmatched <- n_unmatched / nrow(adp_xw)

  message(glue("  Matched {n_matched}/{nrow(adp_xw)} ",
               "({round(100 * (1 - pct_unmatched), 1)}%) to nfl_gsis_id"))

  if (pct_unmatched > 0.05) {
    warning(
      "load_adp_fantasypros(): ", n_unmatched, " of ", nrow(adp_xw),
      " ADP entries (", round(100 * pct_unmatched, 1),
      "%) did not crosswalk to nfl_gsis_id. ",
      "Late-ADP rookies and recent FA signings are expected to be unmatched.",
      call. = FALSE
    )
  }

  # Tidy output schema
  adp_out <- adp_xw %>%
    dplyr::transmute(
      nfl_gsis_id    = nfl_gsis_id,
      fantasypros_id = fantasypros_id,
      player_name    = player_name,
      pos            = pos,
      team           = team,
      adp_rank       = rank,
      adp_ecr        = ecr,
      adp_sd         = sd,
      adp_best       = best,
      adp_worst      = worst,
      adp_tier       = tier
    )

  saveRDS(adp_out, cache_path)
  message(glue("  Cached to: {basename(cache_path)}"))

  adp_out
}

# ==============================================================================
# EXPORT 3: load_consensus_projections_fantasypros
# ==============================================================================

#' Load FantasyPros consensus projections and crosswalk to nfl_gsis_id
#'
#' Pulls FantasyPros consensus projections via ffpros::fp_projections() for
#' QB, RB, WR, and TE. Crosswalk to nfl_gsis_id via normalized name + team
#' + position. Cached with 24h TTL.
#'
#' These consensus projections serve as an external market benchmark for the
#' R/29 projection engine output. The consensus_delta column in the final
#' projections table (our projection minus FantasyPros consensus) is one of
#' the primary outputs of the engine.
#'
#' @param season Integer. Season for the roster crosswalk lookup.
#' @param week Integer. Week number for the projection. If NULL, returns
#'   season-long consensus projections.
#' @param scoring Character. "ppr" (default), "half-ppr", "standard".
#' @param use_cache Logical. Use filesystem cache if fresh. Default TRUE.
#' @param force_refresh Logical. Force re-pull. Default FALSE.
#' @param cache_dir Character. Cache directory path.
#' @return Tibble: nfl_gsis_id, player_name, pos, team, consensus_proj
#'   (projected PPR/game), and raw stat projections (pass_yds, rush_yds,
#'   rec_yds, tds, etc. when available).
#'
#' @details
#' FantasyPros provides position-specific projection pages (qb, rb, wr, te).
#' This function pulls all four and combines them. Consensus projections
#' update multiple times per week; the 24h cache TTL is a balance between
#' freshness and not hammering FantasyPros.
#'
#' @examples
#' \dontrun{
#' proj <- load_consensus_projections_fantasypros(season = 2025L)
#' proj_w5 <- load_consensus_projections_fantasypros(season = 2025L, week = 5L)
#' }
#'
#' @seealso load_adp_fantasypros, run_projection_engine
#' @export
load_consensus_projections_fantasypros <- function(season,
                                                     week = NULL,
                                                     scoring = "ppr",
                                                     use_cache = TRUE,
                                                     force_refresh = FALSE,
                                                     cache_dir = CACHE_DIR_DEFAULT) {

  season <- as.integer(season)
  scoring <- tolower(scoring)

  if (!scoring %in% c("ppr", "half-ppr", "standard")) {
    stop(
      "load_consensus_projections_fantasypros(): scoring must be 'ppr', ",
      "'half-ppr', or 'standard'. Got: ", scoring,
      call. = FALSE
    )
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  week_tag <- if (is.null(week)) "season" else paste0("w", week)
  cache_path <- file.path(
    cache_dir,
    glue("s2_week15_fp_projections_cache_{season}_{week_tag}_{scoring}.rds")
  )

  if (use_cache && !force_refresh &&
      .check_fp_cache_freshness(cache_path)) {
    message(glue("Loading FantasyPros projections from cache: ",
                 "{basename(cache_path)}"))
    return(readRDS(cache_path))
  }

  message("Pulling FantasyPros consensus projections via ",
          "ffpros::fp_projections()...")

  positions <- c("qb", "rb", "wr", "te")

  proj_raw <- purrr::map_dfr(positions, function(p) {

    # ffpros builds URL as fantasypros.com/nfl/projections/{page}.php
    # Correct page name is the position abbreviation only: "qb", "rb", etc.
    res <- tryCatch(
      ffpros::fp_projections(page = p, sport = "nfl"),
      error = function(e) {
        message(glue("  Warning: fp_projections() failed for {p}: ",
                     "{conditionMessage(e)}"))
        NULL
      }
    )

    if (is.null(res) || nrow(res) == 0) return(NULL)

    # Normalize position column
    res$pos <- toupper(p)
    res
  })

  if (nrow(proj_raw) == 0) {
    stop(
      "load_consensus_projections_fantasypros(): all position projection ",
      "pulls returned 0 rows. FantasyPros may be down.",
      call. = FALSE
    )
  }

  # Ensure required columns exist
  if (!"player_name" %in% names(proj_raw)) {
    stop(
      "load_consensus_projections_fantasypros(): ffpros response missing ",
      "player_name column. Schema may have changed. Names returned: ",
      paste(names(proj_raw), collapse = ", "),
      call. = FALSE
    )
  }

  if (!"team" %in% names(proj_raw)) {
    proj_raw$team <- NA_character_
  }

  # Crosswalk to gsis_id
  proj_xw <- .build_fantasypros_crosswalk(
    proj_raw %>% dplyr::select(player_name, team, pos, dplyr::everything()),
    roster_season = season
  )

  # Identify projected PPR/game column. fp_projections returns a column
  # often named "fpts" (fantasy points total) or "fpts_ppr" depending on
  # scoring. We compute per-game later in the engine; here we capture
  # whatever projected total is present.
  pts_col <- intersect(c("misc_fpts", "fpts", "fpts_ppr", "FPTS",
                          "fantasy_points", "misc_FPTS"),
                        names(proj_xw))[1]

  if (is.na(pts_col)) {
    warning(
      "load_consensus_projections_fantasypros(): no recognized projected ",
      "fantasy points column. Setting consensus_proj to NA. ",
      "Columns returned: ", paste(names(proj_xw), collapse = ", "),
      call. = FALSE
    )
    proj_xw$consensus_proj_total <- NA_real_
  } else {
    proj_xw$consensus_proj_total <- as.numeric(proj_xw[[pts_col]])
  }

  proj_out <- proj_xw %>%
    dplyr::transmute(
      nfl_gsis_id          = nfl_gsis_id,
      player_name          = player_name,
      pos                  = pos,
      team                 = team,
      consensus_proj_total = consensus_proj_total,
      # FantasyPros misc_fpts column is already per-game PPR projection
      # (verified empirically -- e.g. Jalen Hurts = 23.1 matches his
      # actual PPG, not a season total). No division required.
      consensus_proj       = consensus_proj_total
    )

  n_matched <- sum(!is.na(proj_out$nfl_gsis_id))
  message(glue("  Matched {n_matched}/{nrow(proj_out)} to nfl_gsis_id"))

  saveRDS(proj_out, cache_path)
  message(glue("  Cached to: {basename(cache_path)}"))

  proj_out
}

# ------------------------------------------------------------------------------
# .fit_score_final_fallback (Season 3 Wave A1)
# ------------------------------------------------------------------------------

#' Fit per-position score_final -> PPR/game calibration for the last-resort prior
#'
#' Replaces the legacy uncalibrated `score_final * 0.15` anchor. That constant
#' assumed (a) a single global scale across all positions and (b) that the
#' minimum training prediction maps to ~0 PPR/game; both are false. score_final
#' is a per-position min-max rescaling of the v3 enriched prediction, so the
#' relationship between score_final and actual PPR/game differs by position and
#' has a nonzero intercept.
#'
#' This fits `ppr_per_game_y13 ~ score_final` per position on training rows that
#' have BOTH columns, and returns per-position (intercept, slope). The fallback
#' is the LAST resort -- it fires only for a player with no usable translation
#' point estimate after R/39 (which scores the rookie cohort live), so it should
#' be rare. When it does fire, an empirical position-specific line is far better
#' than a hand-picked global multiplier.
#'
#' Guard: a position with fewer than MIN_FALLBACK_FIT_N complete rows cannot
#' support a fit; that position falls back to the legacy 0.15 anchor with a
#' warning rather than fitting a line on too few points.
#'
#' @param fallback_df Tibble with at least position, score_final,
#'   ppr_per_game_y13 (training rows only -- prediction-cohort rows have NA
#'   outcome and are excluded by the is.na filter).
#' @param verbose Logical. Print fitted coefficients. Default TRUE.
#' @return Named list per position: list(intercept = <dbl>, slope = <dbl>,
#'   method = "fit" | "legacy_0.15", n = <int>).
#' @keywords internal
.fit_score_final_fallback <- function(fallback_df, verbose = TRUE) {

  MIN_FALLBACK_FIT_N <- 15L
  LEGACY_MULTIPLIER  <- 0.15

  out <- list()
  for (pos in SUPPORTED_POSITIONS) {
    d <- fallback_df %>%
      dplyr::filter(
        .data$position == pos,
        !is.na(.data$score_final),
        !is.na(.data$ppr_per_game_y13)
      )
    n <- nrow(d)

    if (n < MIN_FALLBACK_FIT_N || stats::sd(d$score_final) == 0) {
      warning(glue(
        ".fit_score_final_fallback(): {pos} has {n} complete rows ",
        "(< {MIN_FALLBACK_FIT_N}) -- using legacy {LEGACY_MULTIPLIER} anchor ",
        "for this position's fallback."
      ), call. = FALSE)
      out[[pos]] <- list(intercept = 0, slope = LEGACY_MULTIPLIER,
                         method = "legacy_0.15", n = n)
      next
    }

    fit <- stats::lm(ppr_per_game_y13 ~ score_final, data = d)
    out[[pos]] <- list(
      intercept = unname(stats::coef(fit)[1]),
      slope     = unname(stats::coef(fit)[2]),
      method    = "fit",
      n         = n
    )
  }

  if (verbose) {
    message("  score_final -> PPR/game fallback calibration (per position):")
    for (pos in SUPPORTED_POSITIONS) {
      o <- out[[pos]]
      message(glue(
        "    {pos}: ppg = {round(o$intercept, 3)} + ",
        "{round(o$slope, 4)} * score_final  ",
        "[{o$method}, n = {o$n}]"
      ))
    }
  }

  out
}

# ==============================================================================
# EXPORT 4: build_projection_priors
# ==============================================================================

#' Build precision-weighted projection priors from R/28 and R/24 outputs
#'
#' For each player in the R/28 prospects dataset, constructs a prior
#' distribution (mean + sigma in PPR/game units) by precision-weighted
#' blending of the R/24 translation model prediction and NFL historical
#' performance. Rookies get the translation prior unchanged. Veterans
#' converge to historical NFL performance.
#'
#' For veterans NOT in R/28 (pre-2015 draftees: Rodgers, Cousins, Stafford,
#' etc.), constructs a history-only prior with no translation contribution.
#' This is required for completeness; without it the engine silently drops
#' pre-2015 vets, missing a meaningful fraction of fantasy-relevant players.
#'
#' @param prospects_path Character. Path to R/28 final prospect scores CSV.
#' @param translation_path Character. Path to R/24 v1 predictions RDS.
#' @param season Integer. Current season (for historical lookup).
#' @param cache_dir Character. Path to pbp cache directory.
#' @param scoring_settings List. Scoring settings.
#' @param include_pre2015_vets Logical. Whether to include history-only
#'   priors for veterans not in R/28 (pre-2015 draftees). Default TRUE.
#' @return Tibble: nfl_gsis_id, position, prior_mu, prior_sigma,
#'   n_nfl_seasons, score_final, score_v1, has_translation, has_history,
#'   prior_source, years_exp. prior_source is the lifecycle state, one of:
#'   "translation_active", "translation_capped_rookie",
#'   "phased_out_history_only", "calibrated_fallback",
#'   "phaseout_no_history_fallback", "veteran_history_only".
#'
#' @details
#' The precision-weighted blend uses these formulas:
#'   precision_translation = 1 / sigma_translation^2
#'   precision_history     = n_games_total / sigma_history^2
#'   prior_mu              = weighted average by precision
#'   prior_sigma           = 1 / sqrt(total precision)
#'
#' Sigma_translation is read live from the v1 (R/24) performance file by
#' .load_translation_sigmas(), variant-matched per position (base for QB,
#' enriched for RB/WR/TE) to the model that produced the point estimate.
#' Sigma_history comes from the within-player SD of
#' PPR/game across prior NFL seasons.
#'
#' The translation lifecycle (Season 3 Wave A1) governs how the college signal
#' is used as a function of NFL experience (years_exp = season - draft_year):
#'   - years_exp == 0 (rookie): translation point estimate is haircut by
#'     ROOKIE_TRUST_CAP (downward only), then blended.
#'   - years_exp 1..3: translation blended against history by precision; the
#'     HISTORY_ONLY_FLOOR_SEASONS guard keeps some translation alive through a
#'     slow developer's early seasons.
#'   - years_exp >= TRANSLATION_PHASEOUT_EXP (4): translation gets ZERO weight;
#'     prior is pure NFL history. Safety net: a phased-out player with no
#'     history (rare data glitch) falls to the calibrated score_final line
#'     rather than producing an empty prior.
#'   - no usable translation point estimate: calibrated per-position
#'     score_final -> PPR/game line (replaces the legacy 0.15 anchor).
#'
#' For pre-2015 veterans (not in R/28), prior_mu = hist_mean and
#' prior_sigma = hist_sigma; prior_source is "veteran_history_only".
#'
#' @examples
#' \dontrun{
#' priors <- build_projection_priors(season = 2025L)
#' }
#'
#' @seealso update_projection_with_ytd, run_projection_engine
#' @export
build_projection_priors <- function(season,
                                     prospects_path = PROSPECTS_PATH_DEFAULT,
                                     translation_path = TRANSLATION_PREDS_PATH_DEFAULT,
                                     translation_perf_path =
                                       file.path(CACHE_DIR_DEFAULT,
                                                 "s2_week10_performance.rds"),
                                     cache_dir = CACHE_DIR_DEFAULT,
                                     scoring_settings = DEFAULT_SCORING_SETTINGS,
                                     include_pre2015_vets = TRUE) {

  season <- as.integer(season)

  if (!file.exists(prospects_path)) {
    stop(
      "build_projection_priors(): R/28 prospects file not found at: ",
      prospects_path,
      call. = FALSE
    )
  }
  if (!file.exists(translation_path)) {
    stop(
      "build_projection_priors(): R/24 predictions file not found at: ",
      translation_path,
      call. = FALSE
    )
  }

  # Variant-matched v1 translation sigma, read live from the v1 performance
  # file in season2_cache (where R/24 writes it; NOT the season3 augmented
  # predictions dir). Fails loud if absent so a stale or missing baseline can
  # never silently miscalibrate the prior.
  translation_sigmas <- .load_translation_sigmas(translation_perf_path)

  message("STEP 1/4: Loading R/28 prospects and R/24 predictions...")

  prospects <- readr::read_csv(prospects_path, show_col_types = FALSE) %>%
    dplyr::filter(!is.na(nfl_gsis_id), position %in% SUPPORTED_POSITIONS)

  predictions <- readRDS(translation_path) %>%
    dplyr::filter(!is.na(nfl_gsis_id))

  message(glue("  Prospects: {nrow(prospects)} rows ",
               "({length(unique(prospects$nfl_gsis_id))} unique gsis_ids)"))
  message(glue("  Predictions: {nrow(predictions)} rows ",
               "({length(unique(predictions$nfl_gsis_id))} unique gsis_ids)"))

  # Join R/28 prospects + R/39 augmented translation predictions on nfl_gsis_id.
  # The R/39 file carries pred_base and pred_enriched (rookies scored live
  # against the v1 models; training rows keep their LOCO predictions), plus
  # draft_year (for years_exp) and pred_type ("final_model" rookie vs "loco"
  # training -- informational; the lifecycle logic keys on years_exp, not type).
  base <- prospects %>%
    dplyr::select(nfl_gsis_id, position, score_v1, score_final,
                  score_base, score_enriched) %>%
    dplyr::left_join(
      predictions %>% dplyr::select(nfl_gsis_id, pred_base, pred_enriched,
                                    draft_year, pred_type),
      by = "nfl_gsis_id"
    )

  message(glue("  Joined: {nrow(base)} prospect rows; ",
               "{sum(!is.na(base$pred_base))} have pred_base, ",
               "{sum(!is.na(base$pred_enriched))} have pred_enriched"))

  # Fit the per-position score_final -> PPR/game fallback calibration once, from
  # the training rows in the joined set (those with a known ppr_per_game_y13).
  # The R/39 augmented file does not carry ppr_per_game_y13 into `base` (only
  # pred_* and meta were selected), so pull outcome from the predictions object
  # directly for the fit.
  fallback_fit_df <- prospects %>%
    dplyr::select(nfl_gsis_id, position, score_final) %>%
    dplyr::inner_join(
      predictions %>%
        dplyr::filter(pred_type == "loco") %>%
        dplyr::select(nfl_gsis_id, ppr_per_game_y13),
      by = "nfl_gsis_id"
    )
  fallback_calib <- .fit_score_final_fallback(fallback_fit_df)

  message("STEP 2/4: Computing NFL historical performance...")

  history <- .compute_player_historical_stats(
    season = season,
    cache_dir = cache_dir,
    scoring_settings = scoring_settings
  )

  message(glue("  Historical stats computed for {nrow(history)} players"))

  message("STEP 3/4: Blending priors with precision weighting (R/28 players)...")

  r28_priors <- base %>%
    dplyr::left_join(
      history %>% dplyr::select(player_id, hist_mean, hist_sigma,
                                 n_nfl_seasons, n_games_total),
      by = c("nfl_gsis_id" = "player_id")
    )

  r28_priors <- r28_priors %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # --- years_exp: NFL seasons of experience (0 = rookie year). NA when
      # draft_year is missing; an NA years_exp is treated as neither rookie nor
      # phased-out (it flows through the normal translation/blend path).
      years_exp = if (!is.na(draft_year)) season - as.integer(draft_year)
                  else NA_integer_,

      # --- Per-position point-estimate variant (QB base, RB/WR/TE enriched).
      # Select the configured prediction; if the configured one is NA but the
      # other exists, fall to the available one rather than dropping to the
      # score_final fallback unnecessarily.
      .variant = ROOKIE_TRANSLATION_VARIANT[[position]] %||% "base",
      pred_selected = {
        primary  <- if (.variant == "enriched") pred_enriched else pred_base
        fallback <- if (.variant == "enriched") pred_base else pred_enriched
        if (!is.na(primary)) as.numeric(primary)
        else if (!is.na(fallback)) as.numeric(fallback)
        else NA_real_
      },
      has_prediction = !is.na(pred_selected),

      # --- Lifecycle state. Order matters: phase-out is checked first, then
      # rookie cap, then normal translation, then the calibrated fallback.
      #   phased_out  : years_exp >= TRANSLATION_PHASEOUT_EXP -> zero translation
      #                 weight; prior is 100% NFL history (Option A: route via
      #                 has_translation = FALSE through the existing history
      #                 path). Safety net: if no computable history exists for a
      #                 phased-out player (a long-career vet whose recent 3-season
      #                 window is all sub-8-game seasons, or a data glitch), use
      #                 the calibrated score_final line -- NEVER the translation.
      #                 The phase-out's premise is that a 4+ year player's college
      #                 signal is dead; falling back to it for the MOST
      #                 experienced players would be the worst case. So there is
      #                 no "phaseout uses translation" state.
      #   rookie      : years_exp == 0 -> mu_translation = ROOKIE_TRUST_CAP *
      #                 pred_selected (downward-only haircut).
      #   translation : years_exp 1..3 (or NA) with a prediction -> use
      #                 pred_selected; blended against history by precision.
      #   fallback    : no usable prediction -> calibrated per-position line.
      .phased_out = !is.na(years_exp) & years_exp >= TRANSLATION_PHASEOUT_EXP,
      .is_rookie  = !is.na(years_exp) & years_exp == 0L,
      .has_hist_raw = !is.na(hist_mean) & !is.na(n_nfl_seasons) &
                      n_nfl_seasons >= 1L,

      lifecycle_state = dplyr::case_when(
        .phased_out &  .has_hist_raw  ~ "phased_out_history_only",
        # Phased out, no computable history: calibrated fallback, NOT translation
        # (Option 1). A 4+ year player's college signal is dead by construction.
        .phased_out & !.has_hist_raw  ~ "phaseout_no_history_fallback",
        .is_rookie  &  has_prediction ~ "translation_capped_rookie",
        has_prediction                ~ "translation_active",
        TRUE                          ~ "calibrated_fallback"
      ),

      # --- Calibrated fallback value (used by fallback states). Pull the two
      # scalars directly rather than storing the 4-element list in a column
      # cell (which mutate rejects as size 4). Guard a position whose fit was
      # unavailable.
      .fb_intercept = {
        f <- fallback_calib[[position]]
        if (!is.null(f)) f$intercept else 0
      },
      .fb_slope = {
        f <- fallback_calib[[position]]
        if (!is.null(f)) f$slope else 0.15
      },
      mu_fallback = .fb_intercept + .fb_slope * as.numeric(score_final),

      # --- mu_translation by state.
      mu_translation = dplyr::case_when(
        lifecycle_state == "translation_capped_rookie"
          ~ ROOKIE_TRUST_CAP * pred_selected,
        lifecycle_state == "translation_active"
          ~ pred_selected,
        lifecycle_state %in% c("calibrated_fallback",
                               "phaseout_no_history_fallback")
          ~ mu_fallback,
        # phased_out_history_only: translation is zeroed; mu_translation is
        # unused because the post-blend overwrite forces pure history. Set NA.
        TRUE ~ NA_real_
      ),

      sigma_translation = translation_sigmas[[position]],

      # has_translation drives .blend_prior_components(): FALSE -> history-only.
      # The two fallback states carry mu_fallback as their mu_translation with no
      # usable history, so the blend returns the fallback line unchanged.
      has_translation = lifecycle_state %in% c(
        "translation_capped_rookie", "translation_active",
        "calibrated_fallback", "phaseout_no_history_fallback"
      ),

      # For phased_out_history_only, pass NA translation so the blend returns
      # pure history (precision_translation drops out). The post-blend overwrite
      # below is belt-and-suspenders on top of this. All other states pass their
      # real mu_translation; .blend_prior_components() correctly returns the
      # translation alone when history is absent (n_nfl_seasons 0/NA) and blends
      # when history is present.
      .mu_for_blend = if (lifecycle_state == "phased_out_history_only")
                        NA_real_ else mu_translation,

      blend = list(.blend_prior_components(
        mu_translation     = .mu_for_blend,
        sigma_translation  = sigma_translation,
        mu_history         = hist_mean,
        sigma_history      = hist_sigma,
        n_nfl_seasons      = n_nfl_seasons,
        n_games_total      = n_games_total
      ))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # For phased_out_history_only, force the prior to pure history: the blend
      # above still mixed in any mu_translation we passed, so overwrite with the
      # history mean/sigma directly for that state. This is the explicit
      # zero-translation guardrail (Option A).
      prior_mu = dplyr::if_else(
        lifecycle_state == "phased_out_history_only",
        hist_mean,
        purrr::map_dbl(blend, "prior_mu")
      ),
      prior_sigma = dplyr::if_else(
        lifecycle_state == "phased_out_history_only",
        hist_sigma,
        purrr::map_dbl(blend, "prior_sigma")
      ),
      n_nfl_seasons   = dplyr::coalesce(n_nfl_seasons, 0L),
      has_history     = n_nfl_seasons >= 1L,
      prior_source    = lifecycle_state
    ) %>%
    dplyr::select(
      nfl_gsis_id, position, prior_mu, prior_sigma, n_nfl_seasons,
      score_final, score_v1, has_translation, has_history, prior_source,
      years_exp
    )

  # Lifecycle state summary -- shows how many players took each path and the
  # mean rookie haircut, so a run immediately reveals whether gates fired on
  # sensible populations.
  message("  Lifecycle state breakdown:")
  lc_summary <- r28_priors %>%
    dplyr::count(prior_source, name = "n") %>%
    dplyr::arrange(dplyr::desc(n))
  for (i in seq_len(nrow(lc_summary))) {
    message(glue("    {lc_summary$prior_source[i]}: {lc_summary$n[i]}"))
  }

  # ---- STEP 4: Add history-only priors for non-R/28 veterans ----

  if (include_pre2015_vets) {
    message("STEP 4/4: Adding history-only priors for non-R/28 veterans...")

    # Veterans with history but NOT in R/28 (pre-2015 draftees)
    vet_priors <- history %>%
      dplyr::filter(!player_id %in% r28_priors$nfl_gsis_id) %>%
      dplyr::mutate(
        nfl_gsis_id   = player_id,
        prior_mu      = hist_mean,
        # For veterans, prior sigma = within-player historical SD;
        # if only one qualifying season, use position-specific TRANSLATION RMSE
        # as a conservative variance estimate
        prior_sigma   = dplyr::if_else(
          is.na(hist_sigma) | hist_sigma <= 0,
          purrr::map_dbl(position, ~ translation_sigmas[[.x]]),
          hist_sigma
        ),
        score_final     = NA_real_,
        score_v1        = NA_real_,
        has_translation = FALSE,
        has_history     = TRUE,
        prior_source    = "veteran_history_only",
        # Pre-2015 vets are by definition well past phase-out; years_exp is not
        # computed for them (no draft_year in the history table) and is unused
        # downstream for history-only players. NA keeps the column aligned for
        # the bind_rows with r28_priors.
        years_exp       = NA_integer_
      ) %>%
      dplyr::select(
        nfl_gsis_id, position, prior_mu, prior_sigma, n_nfl_seasons,
        score_final, score_v1, has_translation, has_history, prior_source,
        years_exp
      )

    message(glue("  Added {nrow(vet_priors)} non-R/28 veterans ",
                 "(pre-2015 draftees with NFL history)"))

    priors <- dplyr::bind_rows(r28_priors, vet_priors) %>%
      # Deduplication: a player can appear in both R/28 and the veteran
      # history extension (e.g., multi-position players like Taysom Hill).
      # Keep the richest entry when both exist. Priority: any state carrying a
      # live translation or blend first, then history-based states, with the
      # bare pre-2015 veteran row last.
      dplyr::arrange(dplyr::case_when(
        prior_source == "translation_active"              ~ 1L,
        prior_source == "translation_capped_rookie"       ~ 2L,
        prior_source == "phased_out_history_only"         ~ 3L,
        prior_source == "calibrated_fallback"             ~ 4L,
        prior_source == "phaseout_no_history_fallback"    ~ 5L,
        prior_source == "veteran_history_only"            ~ 6L,
        TRUE                                              ~ 7L
      )) %>%
      dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)
  } else {
    priors <- r28_priors
  }

  message(glue("  Built priors for {nrow(priors)} total players"))
  message("  Prior source breakdown:")
  source_summary <- priors %>%
    dplyr::count(prior_source, name = "n_players") %>%
    dplyr::arrange(dplyr::desc(n_players))
  for (i in seq_len(nrow(source_summary))) {
    message(glue("    {source_summary$prior_source[i]}: ",
                 "{source_summary$n_players[i]}"))
  }

  # ---- STEP 5: Apply aging curve adjustments (Build 1 integration) ----

  message("STEP 5/5: Applying aging curve adjustments (R/23 integration)...")

  aging_artifacts <- .load_or_build_aging_curves(
    cache_dir = cache_dir
  )
  curves <- aging_artifacts$curves

  current_roster <- .load_current_roster(season = season)

  aging_adj <- .compute_aging_adjustments(
    priors         = priors,
    roster         = current_roster,
    curves         = curves,
    current_season = season
  )

  priors <- priors %>%
    dplyr::left_join(aging_adj, by = "nfl_gsis_id",
                     relationship = "many-to-one") %>%
    dplyr::mutate(
      # Preserve original prior_mu before aging adjustment for diagnostic
      prior_mu_pre_age = prior_mu,
      # Apply aging delta (0 for players without ages)
      aging_delta_ppg  = dplyr::coalesce(aging_delta_ppg, 0),
      prior_mu         = prior_mu + aging_delta_ppg
    )

  n_aged <- sum(priors$aging_delta_ppg != 0, na.rm = TRUE)
  mean_abs_delta <- mean(abs(priors$aging_delta_ppg[priors$aging_delta_ppg != 0]),
                          na.rm = TRUE)
  max_neg_delta <- min(priors$aging_delta_ppg, na.rm = TRUE)
  max_pos_delta <- max(priors$aging_delta_ppg, na.rm = TRUE)

  message(glue("  Aging adjustments applied to {n_aged} players"))
  message(glue("    Mean absolute delta: {round(mean_abs_delta, 2)} PPG"))
  message(glue("    Largest decline: {round(max_neg_delta, 2)} PPG"))
  message(glue("    Largest improvement: {round(max_pos_delta, 2)} PPG"))

  # ---- STEP 6: Apply injury proximity adjustments (Build 2 integration) ----

  message("STEP 6/6: Applying injury proximity adjustments (R/25 integration)...")

  injury_groups <- .load_injury_groups(current_season = season)

  injury_adj <- .compute_injury_adjustments(
    priors        = priors,
    injury_groups = injury_groups
  )

  priors <- priors %>%
    dplyr::left_join(injury_adj, by = "nfl_gsis_id",
                     relationship = "many-to-one") %>%
    dplyr::mutate(
      # Apply injury mu adjustment
      prior_mu    = prior_mu + injury_mu_adj,
      # Apply injury sigma inflation
      prior_sigma = prior_sigma * injury_sigma_multiplier
    )

  n_treatment <- sum(priors$injury_group_prior_season == "treatment",
                     na.rm = TRUE)
  n_sigma_inflated <- sum(priors$injury_sigma_multiplier > 1.0,
                           na.rm = TRUE)
  mean_mu_adj <- mean(
    priors$injury_mu_adj[priors$injury_group_prior_season == "treatment"],
    na.rm = TRUE
  )

  message(glue("  Treatment group (returning from injury): ",
               "{n_treatment} players"))
  message(glue("  Sigma inflated (any missed time): {n_sigma_inflated} players"))
  if (n_treatment > 0) {
    message(glue("  Mean prior_mu adjustment: {round(mean_mu_adj, 2)} PPG"))
  }

  # ---- STEP 7: Apply usage ramp adjustments (Build 3 integration) ----

  message("STEP 7/7: Applying usage ramp adjustments (R/26 integration)...")

  ramp_flags  <- .load_ramp_flags(current_season = season)

  ramp_adj <- .compute_ramp_adjustments(
    priors     = priors,
    ramp_flags = ramp_flags
  )

  priors <- priors %>%
    dplyr::left_join(ramp_adj, by = "nfl_gsis_id",
                     relationship = "many-to-one") %>%
    dplyr::mutate(
      prior_mu = prior_mu + ramp_mu_adj
    )

  n_vet_ramp  <- sum(priors$ramp_mu_adj != 0, na.rm = TRUE)
  n_ramp_total <- sum(priors$ramp_flag_prior_season, na.rm = TRUE)

  message(glue("  Ramping players in 2025: {n_ramp_total}"))
  message(glue("  Veteran ramplers adjusted: {n_vet_ramp}"))
  if (n_vet_ramp > 0) {
    mean_ramp_adj <- mean(
      priors$ramp_mu_adj[priors$ramp_mu_adj != 0],
      na.rm = TRUE
    )
    message(glue("  Mean ramp mu adjustment: {round(mean_ramp_adj, 2)} PPG"))
  }

  # ---- STEP 8: Apply volume/efficiency 70/30 blend (Build 4 integration) ----

  message("STEP 8/8: Applying volume/efficiency 70/30 blend (R/16 + Week 4)...")

  # Load cached panel (written by Build 1; always present after first run)
  if (!file.exists(AGING_PANEL_CACHE_PATH)) {
    message("  Panel cache not found -- skipping volume/efficiency blend.")
    message("  Re-run once to trigger the panel build.")
    priors$volume_implied_ppg <- NA_real_
    priors$volume_blend_adj   <- 0
    priors$n_panel_seasons    <- 0L
  } else {
    panel_data <- readRDS(AGING_PANEL_CACHE_PATH)

    vol_adj <- .compute_volume_efficiency_blend(
      priors           = priors,
      panel_data       = panel_data,
      season           = season,
      scoring_settings = scoring_settings
    )

    # Free panel memory after use
    rm(panel_data)
    invisible(gc(verbose = FALSE))

    priors <- priors %>%
      dplyr::left_join(
        vol_adj %>% dplyr::select(nfl_gsis_id, volume_implied_ppg,
                                   volume_blend_adj, n_panel_seasons),
        by = "nfl_gsis_id"
      ) %>%
      dplyr::mutate(
        volume_blend_adj = dplyr::coalesce(volume_blend_adj, 0),
        prior_mu         = prior_mu + volume_blend_adj
      )

    n_adjusted <- sum(priors$volume_blend_adj != 0, na.rm = TRUE)
    mean_abs_vol_adj <- mean(
      abs(priors$volume_blend_adj[priors$volume_blend_adj != 0]),
      na.rm = TRUE
    )
    max_pos_vol <- max(priors$volume_blend_adj, na.rm = TRUE)
    max_neg_vol <- min(priors$volume_blend_adj, na.rm = TRUE)

    message(glue("  Volume blend applied to {n_adjusted} players",
                 " ({VOLUME_FEATURE_WEIGHT * 100}% volume / ",
                 "{EFFICIENCY_FEATURE_WEIGHT * 100}% historical mean)"))
    message(glue("    Mean absolute adjustment: {round(mean_abs_vol_adj, 2)} PPG"))
    message(glue("    Largest boost: {round(max_pos_vol, 2)} PPG"))
    message(glue("    Largest discount: {round(max_neg_vol, 2)} PPG"))
  }

  priors
}

# Null-coalescing operator
# Null-coalescing operator -- defined earlier in the file (near constants)

# ==============================================================================
# EXPORT 5: update_projection_with_ytd
# ==============================================================================

#' Update projection prior with season-to-date observed data
#'
#' Applies week-based linear decay (compute_prior_weight) to blend the prior
#' with season-to-date observed PPR/game. Carries forward the Season 1 R/14
#' update rule exactly.
#'
#' @param priors Tibble from build_projection_priors().
#' @param ytd_stats Tibble from .compute_player_ytd_stats() with columns
#'   player_id, position, ytd_n_games, ytd_mean, ytd_sigma, team.
#' @param week Integer. Current NFL week (1-18).
#' @return Tibble with columns: nfl_gsis_id, position, team, prior_mu,
#'   prior_sigma, ytd_n, ytd_mean, ytd_sigma, posterior_mu, posterior_sigma.
#'
#' @details
#' Update rule:
#'   prior_weight = compute_prior_weight(week)
#'   observed_weight = 1 - prior_weight
#'   posterior_mu = prior_weight * prior_mu + observed_weight * ytd_mean
#'   posterior_sigma = sqrt(prior_weight^2 * prior_sigma^2 +
#'                          observed_weight^2 * (ytd_sigma^2 / max(ytd_n, 1)))
#'
#' Players with no observed data (rookies pre-Week-1, IR) keep the prior
#' unchanged. Players with fewer than MIN_WEEKS_OBSERVED games of data get
#' the prior weighted slightly higher (the observed variance estimate is
#' too noisy to be reliable).
#'
#' Bye weeks: a bye is detected as ytd_n < (week - 1). For those players,
#' the projection effectively carries the prior forward unchanged for that
#' specific week, but the running ytd_mean and ytd_sigma are still used in
#' future weeks once they return.
#'
#' @seealso compute_prior_weight, build_projection_priors
#' @export
update_projection_with_ytd <- function(priors, ytd_stats, week) {

  week <- as.integer(week)
  prior_weight <- compute_prior_weight(week)
  observed_weight <- 1 - prior_weight

  message(glue("update_projection_with_ytd(): week={week}, ",
               "prior_weight={round(prior_weight, 3)}, ",
               "observed_weight={round(observed_weight, 3)}"))

  # Join priors to YTD stats
  updated <- priors %>%
    dplyr::left_join(
      ytd_stats %>% dplyr::select(player_id, team, ytd_n_games, ytd_mean,
                                   ytd_sigma),
      by = c("nfl_gsis_id" = "player_id")
    ) %>%
    dplyr::rename(ytd_n = ytd_n_games) %>%
    dplyr::mutate(
      ytd_n = dplyr::coalesce(ytd_n, 0L),
      has_observed = ytd_n > 0L
    )

  # For players with no observed data, posterior = prior unchanged
  updated <- updated %>%
    dplyr::mutate(
      # Effective ytd_sigma: if only 1 game, no SD available -- use prior_sigma
      # as a conservative variance estimate
      effective_ytd_sigma = dplyr::case_when(
        is.na(ytd_sigma)               ~ prior_sigma,
        ytd_n < MIN_WEEKS_OBSERVED     ~ pmax(ytd_sigma, prior_sigma * 0.7),
        TRUE                           ~ ytd_sigma
      ),
      # Posterior mean -- if no observed, use prior_mu directly
      posterior_mu = dplyr::if_else(
        has_observed,
        prior_weight * prior_mu + observed_weight * ytd_mean,
        prior_mu
      ),
      # Posterior sigma -- combine via weighted variance formula. Divide
      # observed variance by sample size to get variance of the mean estimator.
      posterior_sigma = dplyr::if_else(
        has_observed,
        sqrt(
          (prior_weight^2) * (prior_sigma^2) +
          (observed_weight^2) * (effective_ytd_sigma^2 / pmax(ytd_n, 1))
        ),
        prior_sigma
      )
    ) %>%
    dplyr::select(
      nfl_gsis_id, position, team, prior_mu, prior_sigma,
      ytd_n, ytd_mean, ytd_sigma, posterior_mu, posterior_sigma,
      score_final, score_v1, has_translation, has_history, n_nfl_seasons,
      prior_source,
      current_age, aging_delta_ppg, prior_mu_pre_age,
      injury_group_prior_season, injury_n_absent_weeks,
      injury_mu_adj, injury_sigma_multiplier,
      ramp_flag_prior_season, ramp_relative_change, ramp_mu_adj,
      volume_implied_ppg, volume_blend_adj, n_panel_seasons
    )

  updated
}

# ==============================================================================
# EXPORT 6: calculate_projection_intervals
# ==============================================================================

#' Calculate 80% and 95% projection intervals plus boom/bust probabilities
#'
#' Uses posterior_mu and posterior_sigma to derive symmetric Gaussian
#' projection intervals and flags boom/bust probabilities relative to the
#' position-specific baseline PPR/game.
#'
#' @param updated_proj Tibble from update_projection_with_ytd().
#' @param baselines Tibble with position, baseline_ppr_per_game, baseline_sigma.
#'   Pass from .compute_position_baseline() output.
#' @param boom_threshold Numeric. Boom defined as projection > baseline + this
#'   many SDs of position baseline. Default 1.0 (top ~16% per position).
#' @param bust_threshold Numeric. Bust defined as projection < baseline - this
#'   many SDs. Default 1.0.
#' @return Input tibble with new columns: projection_lower_80,
#'   projection_upper_80, projection_lower_95, projection_upper_95,
#'   boom_probability, bust_probability.
#'
#' @details
#' Boom probability is computed as P(X > baseline + boom_threshold *
#' baseline_sigma) under the posterior Normal(posterior_mu, posterior_sigma).
#' Bust probability is P(X < baseline - bust_threshold * baseline_sigma).
#'
#' Intervals are symmetric Normal CIs:
#'   projection_lower_80 = posterior_mu - CI_80_Z * posterior_sigma
#'   projection_upper_80 = posterior_mu + CI_80_Z * posterior_sigma
#'   projection_lower_95 = posterior_mu - CI_95_Z * posterior_sigma
#'   projection_upper_95 = posterior_mu + CI_95_Z * posterior_sigma
#'
#' @seealso update_projection_with_ytd, run_projection_engine
#' @export
calculate_projection_intervals <- function(updated_proj,
                                            baselines,
                                            boom_threshold = 1.0,
                                            bust_threshold = 1.0) {

  if (nrow(updated_proj) == 0) return(updated_proj)

  required <- c("posterior_mu", "posterior_sigma", "position")
  missing <- setdiff(required, names(updated_proj))
  if (length(missing) > 0) {
    stop(
      "calculate_projection_intervals(): missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  out <- updated_proj %>%
    dplyr::left_join(baselines, by = "position") %>%
    dplyr::mutate(
      projection_lower_80 = posterior_mu - CI_80_Z * posterior_sigma,
      projection_upper_80 = posterior_mu + CI_80_Z * posterior_sigma,
      projection_lower_95 = posterior_mu - CI_95_Z * posterior_sigma,
      projection_upper_95 = posterior_mu + CI_95_Z * posterior_sigma,
      boom_threshold_pts  = baseline_ppr_per_game + boom_threshold * baseline_sigma,
      bust_threshold_pts  = baseline_ppr_per_game - bust_threshold * baseline_sigma,
      boom_probability    = 1 - stats::pnorm(
        boom_threshold_pts, mean = posterior_mu, sd = posterior_sigma
      ),
      bust_probability    = stats::pnorm(
        bust_threshold_pts, mean = posterior_mu, sd = posterior_sigma
      )
    )

  out
}

# ==============================================================================
# EXPORT 7: calculate_def_st_points
# ==============================================================================

#' Calculate team-level DEF/ST fantasy points from pbp
#'
#' Aggregates defensive and special teams events per team per game and
#' applies Sleeper standard scoring tiers. This function fills the DEF/ST
#' gap left by R/17 (which covers only QB/RB/WR/TE/FLEX) and R/19 (which
#' captured these fields in mapping_log as status="unsupported").
#'
#' Required for complete lineup optimizer in Week 16. Built here as a
#' standalone export rather than woven into the player projection pipeline
#' because DEF/ST scoring operates at the team level, not player level.
#'
#' @param pbp Play-by-play tibble.
#' @param scoring_settings List with optional DEF/ST overrides. Defaults
#'   match Sleeper standard.
#' @return Tibble: season, week, team, game_id, opponent_pts_allowed,
#'   sacks, def_ints, fum_recs, def_tds, safeties, blocked_kicks,
#'   pts_allow_points, def_event_points, def_st_points.
#'
#' @details
#' Team identification:
#'   - Each play has defteam (defending) and posteam (possessing).
#'   - Defensive events (sacks, INTs, def TDs) accrue to the defteam.
#'   - Points allowed accrue to the team that was on defense for the
#'     scoring play.
#'
#' Two-point conversion attempts against the defense count as 2 points
#' allowed (handled via td_team comparison).
#'
#' @examples
#' \dontrun{
#' pbp <- load_normalized_season(2025L)
#' def_pts <- calculate_def_st_points(pbp)
#' }
#'
#' @seealso run_projection_engine, calculate_fantasy_points_ext
#' @export
calculate_def_st_points <- function(pbp,
                                     scoring_settings = DEFAULT_SCORING_SETTINGS) {

  if (nrow(pbp) == 0) {
    return(tibble::tibble(
      season = integer(), week = integer(), team = character(),
      game_id = character(), opponent_pts_allowed = numeric(),
      sacks = integer(), def_ints = integer(), fum_recs = integer(),
      def_tds = integer(), safeties = integer(), blocked_kicks = integer(),
      pts_allow_points = numeric(), def_event_points = numeric(),
      def_st_points = numeric()
    ))
  }

  # Filter to regular season
  pbp_use <- dplyr::filter(pbp, .data$season_type == "REG")

  # Identify required columns; some may be missing in older seasons
  required <- c("game_id", "season", "week", "defteam", "posteam")
  missing <- setdiff(required, names(pbp_use))
  if (length(missing) > 0) {
    stop(
      "calculate_def_st_points(): pbp missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # Defensive event tallies per team per game
  def_events <- pbp_use %>%
    dplyr::filter(!is.na(defteam)) %>%
    dplyr::group_by(season, week, game_id, defteam) %>%
    dplyr::summarise(
      sacks         = sum(.data$sack == 1L, na.rm = TRUE),
      def_ints      = sum(.data$interception == 1L, na.rm = TRUE),
      fum_recs      = sum(.data$fumble_lost == 1L &
                           dplyr::coalesce(.data$posteam, "") != "" &
                           dplyr::coalesce(.data$posteam, "") != defteam,
                           na.rm = TRUE),
      def_tds       = sum(.data$touchdown == 1L &
                           dplyr::coalesce(.data$td_team, "") == defteam,
                           na.rm = TRUE),
      safeties      = sum(grepl("safety", tolower(
                            dplyr::coalesce(.data$desc, ""))),
                           na.rm = TRUE),
      blocked_kicks = sum(grepl("blocked", tolower(
                            dplyr::coalesce(.data$desc, ""))),
                           na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(team = defteam)

  # Points allowed per team per game: sum of opponent scoring plays where
  # the team was on defense. We use the score_differential or a simpler
  # approach: total opponent score minus any defensive/special teams TDs
  # by the team. For Sleeper standard, points allowed = opponent offensive
  # + special teams points (not defensive TDs by the opponent).
  pts_allowed <- pbp_use %>%
    dplyr::filter(!is.na(defteam), !is.na(posteam)) %>%
    dplyr::group_by(season, week, game_id, posteam, defteam) %>%
    dplyr::summarise(
      offensive_pts = sum(
        dplyr::case_when(
          .data$touchdown == 1L &
            dplyr::coalesce(.data$td_team, "") == posteam ~ 6,
          .data$field_goal_result == "made" ~ 3,
          TRUE ~ 0
        ),
        na.rm = TRUE
      ),
      xp_pts = sum(
        dplyr::case_when(
          dplyr::coalesce(.data$extra_point_result, "") == "good" ~ 1,
          TRUE ~ 0
        ),
        na.rm = TRUE
      ),
      two_pt_pts = sum(
        dplyr::case_when(
          .data$two_point_attempt == 1L &
            dplyr::coalesce(.data$two_point_conv_result, "") == "success" ~ 2,
          TRUE ~ 0
        ),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pts_scored_by_offense = offensive_pts + xp_pts + two_pt_pts
    ) %>%
    dplyr::group_by(season, week, game_id, defteam) %>%
    dplyr::summarise(
      opponent_pts_allowed = sum(pts_scored_by_offense, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(team = defteam)

  # Combine
  combined <- def_events %>%
    dplyr::left_join(
      pts_allowed,
      by = c("season", "week", "game_id", "team")
    ) %>%
    dplyr::mutate(
      opponent_pts_allowed = dplyr::coalesce(opponent_pts_allowed, 0)
    )

  # Apply scoring
  pts_allow_pts_value <- DEF_EVENT_POINTS  # alias for readability

  combined <- combined %>%
    dplyr::mutate(
      pts_allow_points = .def_st_pts_allowed_points(opponent_pts_allowed),
      def_event_points =
        sacks         * pts_allow_pts_value$sack +
        def_ints      * pts_allow_pts_value$def_int +
        fum_recs      * pts_allow_pts_value$fum_rec +
        def_tds       * pts_allow_pts_value$def_td +
        safeties      * pts_allow_pts_value$safety +
        blocked_kicks * pts_allow_pts_value$blk_kick,
      def_st_points = pts_allow_points + def_event_points
    )

  combined
}

# ==============================================================================
# EXPORT 8: run_projection_engine
# ==============================================================================

#' Run the full in-season projection engine for a given season and week
#'
#' Orchestration function. Produces the complete s2_week15_player_projections
#' table and the s2_week15_def_st_scores table.
#'
#' PRESEASON MODE: If no pbp cache exists for the requested season, the
#' engine runs in preseason mode -- it builds priors, pulls FantasyPros
#' ADP and consensus projections, and writes the projection CSV with every
#' posterior equal to its prior. The DEF/ST table is empty in preseason
#' mode (no games to score). This is the correct preseason behavior:
#' the engine starts with what it knows before any games are played.
#'
#' ACTIVE ROSTER FILTER: Projections are filtered to only players who
#' appear in the current nflreadr 2026 roster WITH an assigned team.
#' Retired players, unsigned free agents not in the roster file, and
#' players with R/28 entries but no NFL roster spot are dropped. The
#' roster is re-pulled fresh on every run -- no caching. When a known
#' free agent signs (e.g., Stefon Diggs), the next run picks up the
#' signing automatically once nflreadr updates (typically within 24h).
#'
#' NAME RESOLUTION: Player display names resolved via three-check chain:
#'   1. nflreadr 2026 roster full_name
#'   2. nflreadr draft picks player_name (for recent draft picks not yet
#'      in the roster file)
#'   3. R/28 cfb_player_name (for prospects with non-standard gsis_ids)
#' Players who fail all three checks are dropped from output.
#'
#' Pipeline:
#'   1. Load R/28 prospects and R/24 predictions
#'   2. Compute NFL historical stats (prior 3 seasons)
#'   3. Build projection priors (precision-weighted blend)
#'   4. Load current season pbp via load_normalized_season()
#'   5. Compute season-to-date stats per player
#'   6. Update projections with YTD (week-based decay)
#'   7. Compute baselines and projection intervals + boom/bust
#'   8. Load FantasyPros ADP and consensus projections
#'   9. Compute consensus_delta and ADP value scores
#'  10. Compute DEF/ST scores from pbp
#'  11. Write outputs to data/season2_cache/
#'
#' @param season Integer. Current season.
#' @param week Integer. Current NFL week (1-18).
#' @param scoring_settings List. Pass-through to calculate_fantasy_points_ext().
#'   Defaults to DEFAULT_SCORING_SETTINGS (Sleeper PPR).
#' @param prospects_path Path to R/28 output.
#' @param translation_path Path to R/24 output.
#' @param cache_dir Cache directory.
#' @param write_outputs Logical. Whether to write CSVs to disk. Default TRUE.
#' @param force_fp_refresh Logical. Force FantasyPros cache refresh.
#'   Default FALSE.
#' @return Named list with elements: projections (tibble), def_st (tibble),
#'   baselines (tibble), prior_weight_used (numeric), n_players_projected
#'   (integer), fp_match_rate (numeric).
#'
#' @examples
#' \dontrun{
#' results <- run_projection_engine(season = 2025L, week = 6L)
#' head(results$projections)
#' head(results$def_st)
#' }
#'
#' @seealso build_projection_priors, update_projection_with_ytd,
#'   calculate_def_st_points
#' @export
run_projection_engine <- function(season,
                                   week,
                                   scoring_settings = DEFAULT_SCORING_SETTINGS,
                                   prospects_path = PROSPECTS_PATH_DEFAULT,
                                   translation_path = TRANSLATION_PREDS_PATH_DEFAULT,
                                   cache_dir = CACHE_DIR_DEFAULT,
                                   write_outputs = TRUE,
                                   force_fp_refresh = FALSE) {

  season <- as.integer(season)
  week <- as.integer(week)

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/29 IN-SEASON PROJECTION ENGINE -- Season {season} Week {week}"))
  message(glue("{strrep('=', 70)}\n"))

  # ---- STEP 1-3: Build priors ----
  message("[1/11] Building projection priors...")
  priors <- build_projection_priors(
    season = season,
    prospects_path = prospects_path,
    translation_path = translation_path,
    cache_dir = cache_dir,
    scoring_settings = scoring_settings
  )

  # ---- STEP 4: Load current season pbp ----
  message(glue("\n[4/11] Loading {season} pbp..."))

  pbp_cache_path <- file.path(cache_dir, glue("pbp_normalized_{season}.rds"))
  pbp_exists <- file.exists(pbp_cache_path)

  if (pbp_exists) {
    pbp <- load_normalized_season(season, cache_dir = cache_dir)
  } else {
    message(glue("  No pbp cache for season {season}. ",
                 "Running in PRESEASON MODE -- no YTD update will be applied."))
    message("  All posteriors will equal their priors at full prior weight.")
    pbp <- NULL
  }

  roster <- .load_current_roster(season = season)

  # ---- STEP 5: Compute YTD stats ----
  message(glue("\n[5/11] Computing season-to-date stats through Week {week}..."))

  if (pbp_exists) {
    ytd <- .compute_player_ytd_stats(
      pbp = pbp,
      current_week = week,
      roster = roster,
      scoring_settings = scoring_settings
    )
    message(glue("  YTD stats computed for {nrow(ytd)} qualified players"))
  } else {
    # Empty YTD tibble for preseason mode -- update step will leave priors
    # unchanged because has_observed will be FALSE for every player.
    ytd <- tibble::tibble(
      player_id = character(),
      position = character(),
      team = character(),
      ytd_n_games = integer(),
      ytd_mean = numeric(),
      ytd_sigma = numeric()
    )
    message("  Preseason mode: YTD stats are empty.")
  }

  # ---- STEP 6: Update with YTD ----
  message("\n[6/11] Updating projections with YTD data...")
  updated <- update_projection_with_ytd(priors, ytd, week)

  # ---- STEP 7: Baselines + intervals ----
  message("\n[7/11] Computing position baselines and projection intervals...")
  baselines <- .compute_position_baseline(
    season = season,
    cache_dir = cache_dir,
    scoring_settings = scoring_settings
  )
  with_intervals <- calculate_projection_intervals(updated, baselines)

  # ---- STEP 8: FantasyPros pulls ----
  message("\n[8/11] Loading FantasyPros ADP...")
  adp <- tryCatch(
    load_adp_fantasypros(
      season = season,
      cache_dir = cache_dir,
      force_refresh = force_fp_refresh
    ),
    error = function(e) {
      message(glue("  WARNING: FantasyPros ADP pull failed: ",
                   "{conditionMessage(e)}"))
      message("  Continuing with NA ADP values.")
      NULL
    }
  )

  message("\n[8b/11] Loading FantasyPros consensus projections...")
  consensus <- tryCatch(
    load_consensus_projections_fantasypros(
      season = season,
      cache_dir = cache_dir,
      force_refresh = force_fp_refresh
    ),
    error = function(e) {
      message(glue("  WARNING: FantasyPros consensus pull failed: ",
                   "{conditionMessage(e)}"))
      message("  Continuing with NA consensus values.")
      NULL
    }
  )

  # ---- STEP 9: Join FP data, compute deltas ----
  message("\n[9/11] Joining FantasyPros data and computing value scores...")

  if (!is.null(adp)) {
    with_intervals <- with_intervals %>%
      dplyr::left_join(
        adp %>% dplyr::select(nfl_gsis_id, adp_rank, adp_ecr, adp_sd,
                              adp_tier),
        by = "nfl_gsis_id"
      )
    fp_match_rate_adp <- mean(!is.na(with_intervals$adp_rank))
  } else {
    with_intervals$adp_rank <- NA_real_
    with_intervals$adp_ecr <- NA_real_
    with_intervals$adp_sd <- NA_real_
    with_intervals$adp_tier <- NA_integer_
    fp_match_rate_adp <- 0
  }

  if (!is.null(consensus)) {
    with_intervals <- with_intervals %>%
      dplyr::left_join(
        consensus %>% dplyr::select(nfl_gsis_id, consensus_proj),
        by = "nfl_gsis_id"
      ) %>%
      dplyr::mutate(
        consensus_delta = posterior_mu - consensus_proj
      )
    fp_match_rate_consensus <- mean(!is.na(with_intervals$consensus_proj))
  } else {
    with_intervals$consensus_proj <- NA_real_
    with_intervals$consensus_delta <- NA_real_
    fp_match_rate_consensus <- 0
  }

  # Projected position rank from posterior_mu (descending)
  with_value <- with_intervals %>%
    dplyr::group_by(position) %>%
    dplyr::mutate(
      projected_rank = dplyr::dense_rank(dplyr::desc(posterior_mu)),
      value_score    = adp_rank - projected_rank
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(position, projected_rank)

  # ---- STEP 10: DEF/ST ----
  message("\n[10/11] Computing DEF/ST scores...")
  if (pbp_exists) {
    def_st <- calculate_def_st_points(pbp, scoring_settings = scoring_settings)
    message(glue("  DEF/ST scores: {nrow(def_st)} team-game rows"))
  } else {
    def_st <- tibble::tibble(
      season = integer(), week = integer(), team = character(),
      game_id = character(), opponent_pts_allowed = numeric(),
      sacks = integer(), def_ints = integer(), fum_recs = integer(),
      def_tds = integer(), safeties = integer(), blocked_kicks = integer(),
      pts_allow_points = numeric(), def_event_points = numeric(),
      def_st_points = numeric()
    )
    message("  Preseason mode: DEF/ST table is empty until games are played.")
  }

  # ---- STEP 11: Write outputs ----
  message("\n[11/11] Writing outputs...")

  # Build active roster filter and resolve names via three-check chain.
  # The roster is always fresh on each run; no caching. When a known free
  # agent signs, the next run picks up the change automatically.
  active_filter <- .build_active_roster_filter(roster)

  message(glue("  Active 2026 roster: {nrow(active_filter)} players ",
               "with team assigned"))

  # Load draft picks for the recent draft years to support name resolution
  # for non-standard gsis_ids. Wrap in tryCatch -- if nflreadr fails or the
  # draft data is unavailable, we fall through to other checks.
  draft_picks <- tryCatch(
    nflreadr::load_draft_picks(seasons = (season - 5L):season),
    error = function(e) {
      message(glue("  Warning: draft picks lookup failed: ",
                   "{conditionMessage(e)}"))
      NULL
    }
  )

  # Load prospects again for name resolution (lightweight CSV read)
  prospects_for_names <- tryCatch(
    readr::read_csv(prospects_path, show_col_types = FALSE),
    error = function(e) {
      tibble::tibble(nfl_gsis_id = character(),
                     cfb_player_name = character())
    }
  )

  # Resolve names for all projected players
  name_resolution <- .resolve_player_identity(
    gsis_ids     = with_value$nfl_gsis_id,
    roster_filter = active_filter,
    prospects    = prospects_for_names,
    draft_picks  = draft_picks
  )

  # Apply active roster filter: keep only players in active_filter OR who
  # have a name resolved via the chain. Players whose name is "unresolved"
  # and who are not in the active filter get dropped entirely.
  active_gsis_ids <- active_filter$nfl_gsis_id

  with_names <- with_value %>%
    dplyr::left_join(name_resolution, by = "nfl_gsis_id",
                     relationship = "many-to-one") %>%
    dplyr::left_join(
      active_filter %>% dplyr::select(nfl_gsis_id, roster_team = team),
      by = "nfl_gsis_id"
    ) %>%
    dplyr::mutate(
      is_active = nfl_gsis_id %in% active_gsis_ids,
      team      = dplyr::coalesce(roster_team, team)
    ) %>%
    dplyr::select(-roster_team)

  # Drop players who are not on the active roster AND have no resolved
  # name from any check. These are unverifiable players who should not
  # appear in projection output.
  n_before_filter <- nrow(with_names)

  with_names <- with_names %>%
    dplyr::filter(
      is_active | name_source != "unresolved"
    )

  # Further filter: drop players not on the active roster (this is the
  # main filter -- it removes retired players, unsigned free agents not
  # in the roster file, and historical-only entries)
  with_names <- with_names %>% dplyr::filter(is_active)

  n_after_filter <- nrow(with_names)
  n_dropped <- n_before_filter - n_after_filter

  message(glue("  Active roster filter: kept {n_after_filter}, ",
               "dropped {n_dropped} ",
               "(retired, unsigned, or unverifiable)"))

  name_source_summary <- with_names %>%
    dplyr::count(name_source, name = "n")
  message("  Name source breakdown:")
  for (i in seq_len(nrow(name_source_summary))) {
    message(glue("    {name_source_summary$name_source[i]}: ",
                 "{name_source_summary$n[i]}"))
  }

  # Re-rank within position after filtering
  with_names <- with_names %>%
    dplyr::group_by(position) %>%
    dplyr::mutate(
      projected_rank = dplyr::dense_rank(dplyr::desc(posterior_mu)),
      value_score    = adp_rank - projected_rank
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(position, projected_rank)

  # Final projections schema -- consolidated and ordered
  projections_out <- with_names %>%
    dplyr::transmute(
      season              = season,
      week                = week,
      nfl_gsis_id         = nfl_gsis_id,
      player_name         = player_name,
      name_source         = name_source,
      position            = position,
      team                = team,
      n_nfl_seasons       = n_nfl_seasons,
      current_age         = round(current_age, 1),
      aging_delta_ppg     = round(aging_delta_ppg, 2),
      prior_mu_pre_age    = round(prior_mu_pre_age, 3),
      injury_group        = injury_group_prior_season,
      injury_weeks_missed = injury_n_absent_weeks,
      injury_mu_adj       = round(injury_mu_adj, 2),
      injury_sigma_mult   = round(injury_sigma_multiplier, 2),
      ramp_flag           = ramp_flag_prior_season,
      ramp_relative_change = round(ramp_relative_change, 3),
      ramp_mu_adj         = round(ramp_mu_adj, 2),
      volume_implied_ppg  = round(volume_implied_ppg, 3),
      volume_blend_adj    = round(volume_blend_adj, 2),
      prior_mu            = round(prior_mu, 3),
      prior_sigma         = round(prior_sigma, 3),
      ytd_n               = ytd_n,
      ytd_mean            = round(ytd_mean, 3),
      ytd_sigma           = round(ytd_sigma, 3),
      posterior_mu        = round(posterior_mu, 3),
      posterior_sigma     = round(posterior_sigma, 3),
      projection_lower_80 = round(projection_lower_80, 3),
      projection_upper_80 = round(projection_upper_80, 3),
      projection_lower_95 = round(projection_lower_95, 3),
      projection_upper_95 = round(projection_upper_95, 3),
      boom_probability    = round(boom_probability, 4),
      bust_probability    = round(bust_probability, 4),
      adp_rank            = adp_rank,
      adp_ecr             = adp_ecr,
      adp_tier            = adp_tier,
      projected_rank      = projected_rank,
      value_score         = value_score,
      consensus_proj      = round(consensus_proj, 3),
      consensus_delta     = round(consensus_delta, 3),
      score_final         = round(score_final, 1),
      score_v1            = round(score_v1, 1),
      prior_source        = prior_source,
      schema_tag          = R29_SCHEMA_TAG
    )

  if (write_outputs) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

    readr::write_csv(projections_out, OUTPUT_PROJECTIONS_PATH)
    message(glue("  Wrote: {OUTPUT_PROJECTIONS_PATH}"))

    readr::write_csv(def_st, OUTPUT_DEF_ST_PATH)
    message(glue("  Wrote: {OUTPUT_DEF_ST_PATH}"))
  }

  message(glue("\n{strrep('=', 70)}"))
  message("PIPELINE COMPLETE")
  message(glue("  Players projected: {nrow(projections_out)}"))
  message(glue("  ADP match rate: {round(100 * fp_match_rate_adp, 1)}%"))
  message(glue("  Consensus match rate: ",
               "{round(100 * fp_match_rate_consensus, 1)}%"))
  message(glue("  Prior weight (week {week}): ",
               "{round(compute_prior_weight(week), 3)}"))
  message(glue("{strrep('=', 70)}\n"))

  invisible(list(
    projections          = projections_out,
    def_st               = def_st,
    baselines            = baselines,
    prior_weight_used    = compute_prior_weight(week),
    n_players_projected  = nrow(projections_out),
    fp_match_rate_adp    = fp_match_rate_adp,
    fp_match_rate_consensus = fp_match_rate_consensus
  ))
}

# ==============================================================================
# END OF FILE
# ==============================================================================
