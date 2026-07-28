# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Player Volume Allocation Engine
# File: R/31_player_volume_allocation.R
#
# PURPOSE
# -------
# Second foundation layer for team-aware projections (Option 3 architecture).
# Distributes R/30 team-level volumes (pass attempts, rush attempts) among
# individual players using:
#   1. Depth chart position from nflreadr::load_depth_charts()
#   2. Rookie draft capital from nflreadr::load_draft_picks()
#   3. Talent multiplier from R/28 dynasty score_final
#   4. Soft constraint enforcement -- per-team target/rush shares rescaled
#      to sum to ~1.0 only when total drifts outside [0.95, 1.05]
#
# WHY THIS EXISTS
# ---------------
# R/29 projects players in isolation. Tai Felton gets ~10 PPG whether or not
# Jefferson and Addison exist on his team. R/31 fixes this by allocating a
# finite pool: the MIN WR target pool is ~60% of MIN's team passes (rest go
# to TE/RB), and the entrenched WR1/WR2 already claim ~43% of that pool.
# Whatever's left is what's actually available for Felton to compete for.
#
# THREE-STAGE ALLOCATION
# ----------------------
#   1. BASE PRIOR     : Depth chart position drives the starting share
#                       (WR1 = 25%, WR2 = 17%, etc. -- see TARGET_SHARE_PRIORS)
#
#   2. ROOKIE ADJUST  : Rookies get a draft-capital multiplier on their
#                       base prior. 1st round = 1.15x, 7th round = 0.55x.
#                       Reflects that high-capital rookies outperform their
#                       initial depth chart position.
#
#   3. TALENT ADJUST  : Non-rookies get a z-score-based multiplier from
#                       their R/28 score_final relative to their position.
#                       Capped at [0.70, 1.30] to prevent extreme rescaling.
#
# After all three stages, per-team shares are summed per role group. If the
# sum falls outside [0.95, 1.05] the team's shares get rescaled. Within
# tolerance, the modeled differences are preserved (a team summing to 1.04
# means the model thinks that team has slightly more talent than baseline,
# which is worth keeping as signal).
#
# DESIGN DECISIONS (confirmed in scoping)
# ---------------------------------------
#   - Depth chart      : nflreadr most recent week, accepted as-is
#   - Constraint type  : Soft. Rescale only outside [0.95, 1.05]
#   - Rookie priors    : Draft capital multiplier on depth-position prior
#   - Talent signal    : R/28 score_final, z-scored by position
#   - Injury logic     : NOT in R/31 (separate weekly function in R/32+)
#   - In-season blend  : v2. Preseason shares taper toward observed
#                        current-season shares via compute_prior_weight()
#                        when as_of_week + current_season_volume supplied.
#                        3-week observation floor mirrors R/29.
#   - Positions        : QB, RB, WR, TE only (DEF/ST handled separately)
#   - Sack adjustment  : R/30 team_pass_pg includes sacks; downstream may
#                        apply a 0.93 multiplier to convert to thrown passes
#
# DEPTH POSITION CLASSIFICATION
# -----------------------------
# nflreadr's depth_position column uses strings like "WR", "RB", "TE", "QB"
# with a depth_team rank. We collapse to:
#   QB1, QB2 (and QB3+ as needed)
#   RB1, RB2, RB3 (others lumped as RB4)
#   WR1, WR2, WR3, WR4, WR5 (others lumped as WR5)
#   TE1, TE2 (others lumped as TE3)
#
# TARGET SHARE PRIORS (historical league averages)
# ------------------------------------------------
# Sum across WR + TE + RB roles approximates 1.0 (typical NFL distribution):
#   WR1 = 0.250, WR2 = 0.170, WR3 = 0.110, WR4 = 0.040, WR5 = 0.015
#   TE1 = 0.150, TE2 = 0.050, TE3 = 0.010
#   RB1 = 0.140, RB2 = 0.050, RB3 = 0.010
#   Total = 1.000
#
# RUSH SHARE PRIORS (historical league averages)
# ----------------------------------------------
#   RB1 = 0.650, RB2 = 0.250, RB3 = 0.080
#   QB1 = 0.015 (v3_4: inert legacy for rushing; QB carries now come from a
#         pooled per-QB rush rate, not this share. See .compute_qb_rush_rate.)
#   WR/TE end-arounds: minimal, not modeled
#   Total = 0.995 (small remainder for incidental carries)
#
# OUTPUTS
# -------
#   data/season2_cache/s2_week15_player_volume_allocation.rds
#   data/season2_cache/s2_week15_player_volume_allocation.csv
#
# One row per active 2026 offensive player. Schema:
#   nfl_gsis_id                chr   gsis ID (join key)
#   player_name                chr   display name
#   team                       chr   2026 team
#   position                   chr   QB/RB/WR/TE
#   depth_position             chr   QB1/RB1/WR3/TE2 etc.
#   depth_rank                 int   numeric rank within position
#   is_rookie                  lgl   2026 first NFL season
#   draft_round                int   1-7 for drafted, NA for UDFA/veteran
#   rookie_capital_mult        dbl   1.15 for R1 rookie down to 0.45 UDFA
#                                    1.0 for non-rookies
#   score_final                dbl   R/28 dynasty score (NA if not present)
#   talent_z                   dbl   within-position z-score. For prospects this
#                                    derives from R/28 score_final; for veterans
#                                    (years_exp >= VETERAN_MIN_EXP) it derives
#                                    from NFL efficiency over the trailing window
#   talent_multiplier          dbl   1 + 0.10 * talent_z, capped [0.70, 1.30]
#   talent_source              chr   "veteran_nfl" (NFL-efficiency z),
#                                    "prospect_college" (R/28 score z), or
#                                    "neutral" (no signal; talent_z = 0)
#   window_volume              dbl   veteran trailing-window volume (QB
#                                    dropbacks, RB rush attempts, WR targets);
#                                    NA for non-veterans
#   n_window_seasons           int   veteran seasons observed in the window;
#                                    NA for non-veterans
#   target_share_base          dbl   depth-position prior
#   target_share_adjusted      dbl   after rookie + talent multipliers (and
#                                    in-season observed blend when as_of_week
#                                    is supplied)
#   target_share               dbl   after soft constraint per team
#   rush_share_base            dbl   depth-position prior
#   rush_share_adjusted        dbl   after rookie + talent multipliers (and
#                                    in-season observed blend when as_of_week
#                                    is supplied)
#   rush_share                 dbl   after soft constraint per team
#   observed_target_share      dbl   caller-supplied observed share (in-season
#                                    mode only; NA preseason / unmatched)
#   observed_rush_share        dbl   caller-supplied observed share (in-season
#                                    mode only; NA preseason / unmatched)
#   share_blend_weight         dbl   compute_prior_weight(as_of_week) used in
#                                    the blend (NA preseason / blend skipped)
#   projected_team_pass_pg     dbl   from R/30
#   projected_team_rush_pg     dbl   from R/30
#   expected_targets_pg        dbl   target_share * team_pass_pg
#   expected_carries_pg        dbl   rush_share * team_rush_pg
#   schema_tag                 chr   "s2_w15_player_alloc_v3_2"
#
# SOURCE DEPENDENCIES
# -------------------
#   R/30_team_volume_projections.R -- project_team_volumes() output
#
# INPUT ARTIFACTS
# ---------------
#   data/season2_cache/s2_week15_team_volumes.rds (or .csv)
#     From R/30. Required.
#
#   data/season2_cache/s2_week14_final_prospect_scores.csv
#     From R/28. Required for prospect talent multiplier. Players missing from
#     this file get talent_multiplier = 1.0 (neutral) unless a veteran signal
#     applies.
#
#   data/season2_cache/s2_week15_player_season_panel_cache.rds
#     From R/16 (built and cached by R/29). Optional. Supplies NFL efficiency
#     for the veteran talent signal. If absent, all players keep the prospect
#     talent_z.
#
# RUN
# ---
#   source(here::here("R", "31_player_volume_allocation.R"))
#   alloc <- allocate_player_volumes()                    # preseason
#   alloc <- allocate_player_volumes(                     # in-season
#     as_of_week = 6,
#     current_season_volume = obs                         # caller-supplied
#   )
#
# Author: Christian K. LeBlanc
# Version: 3.5
#
# CHANGELOG
# ---------
# 3.5  Rookie-QB1 rush fallback: no-history QB1s (e.g., rookies entering as
#      starters) now get a college-tier rate instead of the v3.4 provisional
#      median. Pool the QB's college rush/game (game-weighted, R/21 CFB panel,
#      name-normalized via .qb_name_key to collapse split spellings), rank him
#      against the college QB starter distribution, and map his tier
#      (mobile/balanced/pocket) to the NFL QB1 pooled rate at the matching
#      quantile (Q75/median/Q25). The college rate assigns a tier only; it is
#      not translated directly (scales are uncalibrated). No college match falls
#      back to the NFL QB1 median. New qb_rush_rate_source values: college_mobile
#      / college_balanced / college_pocket / prior_median_fallback. Schema tag ->
#      s2_w15_player_alloc_v3_5.
# 3.4  QB rushing volume no longer routes through team-carry share. QB
#      expected_carries_pg now derives from a pooled, game-weighted per-QB rush
#      rate (sum rush_attempts / sum games_played over QB_RUSH_RATE_SEASONS,
#      2023-2025), a player trait that TRANSFERS across team moves. RBs on a
#      team split the residual team rush after the QB1 rate is removed
#      (subtract-first, rate space at STEP 7), so the RB soft constraint runs
#      untouched. The old share/carryover path is defective for QBs (a share of
#      team carries self-deflates mobile QBs and drops movers to the 0.015
#      floor); its QB rush-share and carryover audit fields are blanked for QB
#      rows since they no longer feed output. No-history QB1s take a provisional
#      NFL QB1 median rate (qb_rush_rate_source = "prior_median_provisional")
#      pending the college-tier fallback. New QB columns: qb_rush_pooled_pg,
#      qb_rush_seasons_pooled, qb_rush_rate_source. RB/WR/TE, all target logic,
#      and the soft constraint are unchanged. Schema tag ->
#      s2_w15_player_alloc_v3_4.
# 3.3  Preseason share carryover: for returning, non-mover, above-floor players,
#      the primary share is blended toward last season's realized share at
#      fitted per-position weights before the in-season blend. Schema tag ->
#      s2_w15_player_alloc_v3_3.
# 3.2  Veteran volume floors recalibrated from the observed 2023-2025 qualifier
#      distribution: RB 100 -> 200 rush attempts, WR 50 -> 150 targets (QB
#      unchanged at 200 dropbacks). With shrinkage in place the remaining
#      ranking distortion came entirely from marginal-volume backups clearing
#      the old floors (e.g., a passing-down RB3 scored on a thin, efficient
#      receiving sample). Raising the floors moves those players to the prospect
#      or neutral path. Downstream effect is small either way, since the capped
#      talent multiplier sits on top of small depth-position base shares for low
#      tiers; this change is about ranking credibility. Schema tag ->
#      s2_w15_player_alloc_v3_2. No structural change to the output columns.
# 3.1  Veteran efficiency rates now empirical-Bayes shrunk toward the position
#      mean before standardizing, so low-volume players regress to neutral
#      instead of producing extreme z-scores (a hard volume floor alone left
#      boundary backups topping the talent ranking). Each component is a
#      trust-weighted standardized value, z_i = B_i * (theta_i - mu0) / sd0 with
#      B_i = n_i / (n_i + K). K is estimated per position-component from the
#      cross-section (moment regression of squared deviations on 1/volume) and
#      floored at the median window volume, since the moment estimate is not
#      robust to the outliers it must tame. RB rushing and receiving components
#      are shrunk by their own sample sizes; the WR slope is shrunk like the
#      level. Two diagnostic columns added: window_volume, n_window_seasons.
#      Schema tag -> s2_w15_player_alloc_v3_1.
# 3.0  Veteran talent split added. Established veterans
#      (years_exp >= VETERAN_MIN_EXP, sourced from nflreadr::load_rosters)
#      replace the stale R/28 college score with an NFL-efficiency talent_z
#      computed from the R/16 player-season panel over the trailing
#      VETERAN_WINDOW_SEASONS. Per-position metric grounded in the R/28
#      residual analysis: QB = CPOE z + EPA/dropback z (reuses the R/30
#      QB-quality construct); RB = rush success rate z + receiving EPA/target
#      z; WR = receiving EPA/target level z + trajectory (slope) z; TE has no
#      clean efficiency signal so veteran TEs keep the prospect talent_z.
#      Composite is re-standardized to unit z within position so it shares the
#      prospect z scale. Veterans below the per-position volume floor fall back
#      to the prospect talent_z (hard switch at the threshold, no blend band).
#      New audit column talent_source. Schema tag -> s2_w15_player_alloc_v3.
#      Prospect and preseason behavior otherwise unchanged.
# 2.0  In-season share blend added. New optional as_of_week and
#      current_season_volume arguments: each player's preseason adjusted
#      share is blended toward their observed current-season share using
#      R/29's compute_prior_weight() decay curve (same curve as R/29, R/30
#      SOS, and R/34 -- no divergence). Blend runs BEFORE the soft constraint
#      so per-team sums stay coherent. Observation floor mirrors R/29's
#      MIN_WEEKS_OBSERVED = 3: below week 3, preseason shares are retained.
#      Caller supplies observed shares (same contract as R/30's
#      current_season_sos); players without an observed record retain
#      preseason shares at full weight. Three new audit columns:
#      observed_target_share, observed_rush_share, share_blend_weight.
#      Schema tag -> s2_w15_player_alloc_v2. Preseason behavior unchanged.
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
source(here::here("R", "19_sleeper_api.R"))   # for get_all_sleeper_players()

# compute_prior_weight() (R/29) normally arrives via R/30's guarded source
# chain above. This guard protects against future refactors of R/30: the
# in-season share blend depends on the canonical decay curve and must fail
# loudly if it is unavailable, never fall back to a local copy.
if (!exists("compute_prior_weight")) {
  source(here::here("R", "29_projection_engine.R"))
}

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

SEASON_ALLOC <- 2026L

# Supported positions for allocation
ALLOC_POSITIONS <- c("QB", "RB", "WR", "TE")

# Soft constraint bounds. If per-team summed share falls outside this range,
# rescale to 1.0. Inside this range, preserve modeled differences as signal.
CONSTRAINT_LOWER <- 0.95
CONSTRAINT_UPPER <- 1.05

# [2026-07-13] Rescale-up floor. The rescale factor is 1 / team_sum; for a sparse
# team pool (many players missing from the depth data) team_sum can be far below
# 1, and an uncapped 1/team_sum inflates the listed players' shares to impossible
# levels (a traded/thin-depth RB1 was pushed to a ~0.70 target share). Flooring
# the denominator caps the up-rescale at 1/RESCALE_SUM_FLOOR: a near-complete
# pool still normalizes toward 1, but a materially incomplete pool rescales only
# partway and leaves the missing volume unallocated, which is correct because that
# volume belongs to players not in the pool, not to the ones who are. Down-rescale
# (team_sum > 1.05) is unaffected since the sum already exceeds the floor.
RESCALE_SUM_FLOOR <- 0.80  # max up-rescale = 1/0.80 = 1.25x; lower = more inflation

# ------------------------------------------------------------------------------
# IN-SEASON SHARE BLEND (v2)
# ------------------------------------------------------------------------------
# When allocate_player_volumes() is called with as_of_week and a
# current_season_volume table, each player's preseason adjusted share is
# blended toward their OBSERVED current-season share using R/29's
# compute_prior_weight(as_of_week) -- the same validated decay curve used
# across the engine (week 1 = full preseason weight, ~week 9 crossover,
# 0.05 floor). The blend happens BEFORE the soft constraint, so the
# constraint runs once on the blended shares and per-team sums stay coherent.
#
# Minimum-weeks guard: observed shares from one or two games are dominated
# by single-game noise (one big game can spike a WR3's share). Below this
# threshold, preseason shares are retained regardless. Mirrors R/29's
# MIN_WEEKS_OBSERVED = 3 convention so R/31's in-season behavior is
# consistent with the projection engine's.
MIN_WEEKS_OBSERVED_ALLOC <- 3L

# Talent multiplier sensitivity: 1 + 0.10 * z-score, capped
TALENT_MULT_SENSITIVITY <- 0.10
TALENT_MULT_FLOOR <- 0.70
TALENT_MULT_CEILING <- 1.30

# ------------------------------------------------------------------------------
# VETERAN TALENT SIGNAL (talent-z veteran split)
# ------------------------------------------------------------------------------
# For established veterans, R/28's college-based score_final is stale. Veterans
# instead receive an NFL-efficiency talent_z from the R/16 player-season panel
# over a trailing window, replacing the prospect score (hard switch at the
# threshold). Players below the threshold, veteran TEs, and veterans with thin
# recent NFL data keep the R/28 prospect talent_z.
#
# Per-position metric is grounded in the R/28 residual analysis (which features
# separate model hits from misses):
#   QB : CPOE z + EPA/dropback z, equal weight (reuses the R/30 QB-quality
#        construct; EPA/dropback already absorbs interceptions, so INT rate is
#        not added as a separate term)
#   RB : rush success rate z + receiving EPA/target z, equal weight
#   WR : receiving EPA/target level z + trajectory (slope across window) z;
#        trajectory was the dominant pass-catcher separator in the analysis
#   TE : no clean efficiency separator found -> veteran TEs keep prospect z
# Each position composite is re-standardized to unit z within position so it is
# on the same scale as the prospect talent_z and TALENT_MULT_SENSITIVITY
# applies consistently to both signals.

# Years of NFL experience at/above which a player is treated as a veteran.
# Sourced from nflreadr::load_rosters(season)$years_exp.
VETERAN_MIN_EXP <- 3L

# Trailing completed NFL seasons used to measure veteran efficiency.
VETERAN_WINDOW_SEASONS <- 3L

# Minimum trailing-window volume for a stable veteran read. Below these floors
# the veteran is treated as thin-data and falls back to the prospect talent_z.
# The QB floor mirrors R/30's 200-dropback QB-quality rule. The RB and WR floors
# were recalibrated from the observed 2023-2025 qualifier distribution (RB rush
# median ~471, WR target median ~190): set well below the median to keep
# committee-or-better backs and rotational-or-better receivers, but high enough
# to exclude pure backups whose efficiency on a thin sample distorted the
# ranking (e.g., a ~40-carry-per-year RB3 or ~40-target-per-year WR5). Tunable.
VETERAN_MIN_QB_DROPBACKS <- 200L
VETERAN_MIN_RB_ATTEMPTS  <- 200L
VETERAN_MIN_WR_TARGETS   <- 150L

# WR composite weighting: share of the WR veteran z coming from the EPA/target
# trajectory (slope) vs the level. Equal weight by default; tunable.
VETERAN_WR_SLOPE_WEIGHT <- 0.5

# Empirical-Bayes shrinkage of veteran efficiency rates toward the position
# mean, weighted by sample size, so low-volume players regress to neutral
# rather than producing extreme z-scores. The shrinkage constant K (in volume
# units) is estimated per position-component from the cross-section
# (K = within-variance / between-variance via a moment regression of squared
# deviations on 1/volume). When a position-component has fewer than
# VETERAN_EB_MIN_PLAYERS usable players, or the moment estimate is degenerate
# (non-positive variance), K falls back to the median window volume of that
# component (a median-volume player is then shrunk halfway).
VETERAN_EB_MIN_PLAYERS <- 8L

# Target share priors by depth position (sum across all positions ~= 1.0)
TARGET_SHARE_PRIORS <- c(
  "QB1" = 0.000, "QB2" = 0.000, "QB3" = 0.000,
  "RB1" = 0.140, "RB2" = 0.050, "RB3" = 0.010, "RB4" = 0.005,
  "WR1" = 0.250, "WR2" = 0.170, "WR3" = 0.110, "WR4" = 0.040,
  "WR5" = 0.015,
  "TE1" = 0.150, "TE2" = 0.050, "TE3" = 0.010
)

# Rush share priors by depth position (sum ~= 1.0)
RUSH_SHARE_PRIORS <- c(
  "QB1" = 0.015, "QB2" = 0.000, "QB3" = 0.000,
  "RB1" = 0.650, "RB2" = 0.250, "RB3" = 0.080, "RB4" = 0.005,
  "WR1" = 0.000, "WR2" = 0.000, "WR3" = 0.000, "WR4" = 0.000,
  "WR5" = 0.000,
  "TE1" = 0.000, "TE2" = 0.000, "TE3" = 0.000
)

# Rookie draft capital multipliers (applied to base prior for rookies only)
ROOKIE_CAPITAL_MULTIPLIERS <- c(
  "1"  = 1.15,
  "2"  = 1.05,
  "3"  = 0.95,
  "4"  = 0.85,
  "5"  = 0.75,
  "6"  = 0.65,
  "7"  = 0.55,
  "U"  = 0.45    # undrafted free agent
)

# Position-specific max depth rank to keep (deeper players get folded into max)
MAX_DEPTH_RANK <- c("QB" = 3L, "RB" = 4L, "WR" = 5L, "TE" = 3L)

# ------------------------------------------------------------------------------
# PRESEASON SHARE CARRYOVER (v3_3)
# ------------------------------------------------------------------------------
# A returning player's realized prior-season usage share is a far stronger
# preseason signal than the flat depth-position prior. Each eligible player's
# primary share (target share for WR/TE, rush share for RB/QB) is blended:
#
#   adjusted = w_pos * prior_season_share + (1 - w_pos) * positional_base
#
# w_pos was fitted out-of-sample (2011-2025, returning non-mover players at or
# above the volume floors below, anchor = week-1 depth-chart slot). Every
# position's blend beat the positional-prior-only baseline with the bootstrap
# 95% CI on the MAE improvement clearing 0. Weights are the per-position
# interior MAE minima on the corrected sample (BLT/CLV/HST/SL codes mapped):
#   QB 0.85  RB 0.80  WR 0.95  TE 0.70
# QB is secondary: the R/16 panel's rush_attempts excludes scrambles, so QB
# rush share is undercounted; the weight is retained but treated as provisional.
#
# ELIGIBILITY (all must hold; anyone failing keeps the positional prior path):
#   - not a rookie
#   - has a prior-season (CARRYOVER_PRIOR_SEASON) usage row
#   - non-mover: 2026 team == prior-season team (a share earned against a
#     different offense's target competition does not transfer, so movers fall
#     back to the positional prior, matching how the weight was fitted)
#   - prior-season primary volume >= the position floor below (a thin prior
#     season is too noisy to carry at full weight; below-floor returners fall
#     back to the positional prior)
#
# The blend uses the positional BASE (pre rookie/talent multiplier), so for an
# eligible player the validated carryover SUPERSEDES the talent nudge rather
# than stacking on it. Eligible players are non-rookies whose veteran talent
# multiplier is 1.0 (neutralized) in nearly all cases; the only players this
# changes are year 2-3 returners still carrying a nonzero R/28 prospect
# talent_z, for whom a realized NFL usage season is the stronger signal.
#
# Deferred (their own future items, not in this weight set): RB receiving-share
# carryover; a mover-specific weight; and empirical-Bayes volume-continuous
# shrinkage to replace the hard floor and flat weight. Toggle with the
# preseason_carryover argument to allocate_player_volumes().
PRESEASON_CARRYOVER_WEIGHTS <- c(QB = 0.85, RB = 0.80, WR = 0.95, TE = 0.70)

# Prior-season primary volume floors for carryover eligibility, matching the
# floors the weights were fitted under (QB dropbacks, RB carries, WR/TE targets).
CARRYOVER_FLOOR <- c(QB = 100L, RB = 50L, WR = 50L, TE = 50L)

# Prior season whose realized shares are carried into the preseason baseline.
CARRYOVER_PRIOR_SEASON <- SEASON_ALLOC - 1L

# Paths
TEAM_VOLUMES_RDS  <- here::here("data", "season2_cache",
                                 "s2_week15_team_volumes.rds")
DYNASTY_SCORES_CSV <- here::here("data", "season2_cache",
                                  "s2_week14_final_prospect_scores.csv")
OUTPUT_RDS_PATH_ALLOC <- here::here("data", "season2_cache",
                                     "s2_week15_player_volume_allocation.rds")
OUTPUT_CSV_PATH_ALLOC <- here::here("data", "season2_cache",
                                     "s2_week15_player_volume_allocation.csv")

# R/16 player-season panel cache (built and cached by R/29). Same path R/29
# uses. Optional input for the veteran talent signal.
PANEL_CACHE_PATH_ALLOC <- here::here("data", "season2_cache",
                                      "s2_week15_player_season_panel_cache.rds")

SCHEMA_TAG_ALLOC <- "s2_w15_player_alloc_v3_5"

# QB rushing volume window (v3_4). QB expected carries derive from a pooled,
# game-weighted per-QB rush rate over these seasons (a player trait that
# transfers across team moves), NOT a share of team rush volume. Pooled rate =
# sum(rush_attempts) / sum(games_played). The panel's rush_attempts is verified
# to include scrambles (97-100% of true pbp QB rush volume) with kneels already
# excluded, so the rate is the QB's real per-game rushing workload.
QB_RUSH_RATE_SEASONS <- (SEASON_ALLOC - 3L):(SEASON_ALLOC - 1L)

# Committee QB config (uncertain rooms), season-guarded. Mirrors R/30's loader
# so R/31 does not depend on R/30 being sourced.
QB_COMMITTEES_PATH_ALLOC <- here::here("data", "ref", "qb_committees_2026.csv")

.load_qb_committees_alloc <- function(path = QB_COMMITTEES_PATH_ALLOC,
                                      season = SEASON_ALLOC) {
  empty <- tibble::tibble(team = character(), qb_id = character(),
                          committee_group = character())
  if (!file.exists(path)) return(empty)
  raw <- tryCatch(readr::read_csv(path, show_col_types = FALSE),
                  error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0L) return(empty)
  if (length(setdiff(c("season", "team", "qb_id", "committee_group"),
                     names(raw))) > 0L) return(empty)
  raw %>%
    dplyr::mutate(.season = suppressWarnings(as.integer(.data$season))) %>%
    dplyr::filter(.data$.season == as.integer(season)) %>%
    dplyr::transmute(team = as.character(.data$team),
                     qb_id = as.character(.data$qb_id),
                     committee_group = as.character(.data$committee_group)) %>%
    dplyr::filter(!is.na(.data$qb_id)) %>%
    dplyr::distinct(.data$team, .data$qb_id, .keep_all = TRUE)
}

# College QB starter threshold (v3_5) for the rookie-QB1 college-tier fallback
# reference distribution. College QBs with fewer games are excluded from the
# tier-cutoff quantiles so the reference reflects starters, not deep backups.
CFB_QB_REF_MIN_GAMES <- 6L

# ------------------------------------------------------------------------------
# NSE DECLARATIONS
# ------------------------------------------------------------------------------

utils::globalVariables(c(
  "nfl_gsis_id", "player_name", "team", "position", "depth_position",
  "depth_rank", "is_rookie", "draft_round", "rookie_capital_mult",
  "score_final", "talent_z", "talent_multiplier",
  "target_share_base", "target_share_adjusted", "target_share",
  "rush_share_base", "rush_share_adjusted", "rush_share",
  "projected_team_pass_pg", "projected_team_rush_pg",
  "expected_targets_pg", "expected_carries_pg",
  "gsis_id", "full_name", "first_name", "last_name",
  "club_code", "depth_chart_position", "depth_team",
  "pos_grp", "pos_abb", "pos_name", "pos_rank", "pos_slot",
  "pos_grp_id", "pos_id", "dt", "espn_id",
  "rookie_year", "round", "pos", "side", "season", "week",
  "depth_pos_clean", "depth_rank_raw", "key",
  "cfb_player_name", "position_score", "position_mean", "position_sd",
  "team_target_sum", "team_rush_sum", "team_target_rescale",
  "team_rush_rescale", "schema_tag",
  "observed_target_share", "observed_rush_share", "share_blend_weight",
  "talent_source", "talent_z_prospect", "veteran_talent_z", "years_exp",
  "player_id", "qb_dropbacks", "pass_epa_per_dropback", "mean_cpoe",
  "rush_attempts", "rush_success_rate", "rush_epa_per_attempt",
  "targets", "rec_epa_per_target", "window_volume", "n_window_seasons",
  "n_window_targets", "vet_composite", "comp_a", "comp_b", "cpoe_w", "epa_w",
  "rsr_w", "reptt_w", "level_w", "slope_w", "cpoe_s", "epa_s", "rsr_s",
  "reptt_s", "level_s", "slope_s", "inv_n",
  "carryover_applied", "carryover_prior_share", "carryover_weight",
  "qb_rush_pooled_pg", "qb_rush_seasons_pooled", "qb_rush_rate_source",
  "games_played", "qb_carries_tmp", "rush_residual_tmp",
  "team_qb1_carries", "team_nonqb_rush_share",
  "name_key", "rookie_rate", "rookie_source", "cfb_rush_pg", "position_group",
  "prior_team", "prior_position", "prior_target_share", "prior_rush_share",
  "prior_vol_targets", "prior_vol_rush", "prior_vol_dropbacks",
  "team_targets", "team_carries", "posteam", "receiver_player_id",
  "pass_attempt", "two_point_attempt", "rush_attempt", "qb_kneel",
  "w_pos", "floor_pos", "prior_primary_share", "prior_primary_vol",
  "primary_base", "eligible", "blend_primary"
))

# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

# ------------------------------------------------------------------------------
# SLEEPER DEPTH CHART OVERRIDES (Phase 4 fix, Week 15)
# ------------------------------------------------------------------------------
#
# nflreadr::load_depth_charts() depends on PFR which lags 1-4 weeks behind
# actual NFL roster moves during the offseason. Sleeper updates its player
# database within hours. The helpers below merge Sleeper team assignments
# and depth_chart_order into the nflreadr depth chart to capture trades and
# signings that nflreadr hasn't picked up yet.
#
# Mirrors the team code crosswalk and normalizer used in R/29. Future
# refactor: lift both into a shared utility.

# Sleeper-to-nflreadr team code crosswalk. Only entries that DIFFER.
SLEEPER_TO_NFLREADR_TEAM_MAP_R31 <- c(
  "LAR" = "LA",     # Los Angeles Rams
  "AZ"  = "ARI",    # Arizona Cardinals (Sleeper uses AZ, nflfastR uses ARI)
  "JAC" = "JAX",    # Jacksonville Jaguars (defensive)
  "WSH" = "WAS"     # Washington Commanders (defensive)
)


#' Normalize Sleeper team codes to nflreadr conventions
#' @keywords internal
.normalize_sleeper_team_codes_r31 <- function(team_vec) {
  out <- team_vec
  for (sleeper_code in names(SLEEPER_TO_NFLREADR_TEAM_MAP_R31)) {
    out[!is.na(out) & out == sleeper_code] <-
      SLEEPER_TO_NFLREADR_TEAM_MAP_R31[[sleeper_code]]
  }
  out
}


#' Normalize player name for fallback matching
#'
#' Strips apostrophes, periods, suffixes (Jr/Sr/II/III/IV), non-letters,
#' and collapses whitespace. Used to match Sleeper records that lack
#' gsis_id back to nflreadr depth chart entries when both refer to the
#' same player despite typographic differences.
#'
#' Examples:
#'   "Ja'Marr Chase"     -> "jamarr chase"
#'   "D.J. Chark Jr."    -> "dj chark"
#'   "Marvin Harrison Jr." -> "marvin harrison"
#'
#' @keywords internal
.normalize_name_for_match_r31 <- function(name_vec) {
  out <- tolower(as.character(name_vec))
  out <- gsub("[.']", "", out)
  out <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", out, ignore.case = TRUE)
  out <- gsub("[^a-z ]", "", out)
  out <- gsub("\\s+", " ", out)
  trimws(out)
}


#' Merge Sleeper depth chart data into nflreadr depth chart
#'
#' Two-pass match against Sleeper:
#'   1. By gsis_id -- direct match for Sleeper records that have it
#'   2. By normalized name + position -- recovers nflreadr's gsis_id for
#'      Sleeper records where Sleeper's gsis_id field is NA
#'
#' For matched players, Sleeper's team and depth_chart_order replace
#' nflreadr's values. nflreadr-only players (not in Sleeper at all) keep
#' their nflreadr assignment. After merge, depth ranks are re-numbered
#' within team + position so they remain contiguous integers starting at 1.
#'
#' The name-fallback pass is critical for veterans Sleeper has but whose
#' gsis_id mapping is missing in Sleeper's data (e.g., Jaylen Waddle's
#' DEN reassignment in 2026).
#'
#' @param nflreadr_dc Tibble from .load_2026_depth_charts() pre-merge.
#'   Must have nfl_gsis_id, player_name, team, position, depth_rank_raw.
#' @return Same schema. Player rows replaced with Sleeper data where
#'   available (via gsis_id or name match), re-ranked within team + position.
#' @keywords internal
.merge_sleeper_depth_overrides <- function(nflreadr_dc) {

  message("    Applying Sleeper depth chart overrides...")

  sleeper <- tryCatch(
    get_all_sleeper_players(),
    error = function(e) {
      message(glue(
        "    Sleeper data unavailable: {e$message}. ",
        "Continuing with nflreadr depth chart only (may be stale)."
      ))
      NULL
    }
  )

  if (is.null(sleeper) || nrow(sleeper) == 0L) {
    return(nflreadr_dc)
  }

  # Common filters: skill position + Sleeper team that passes ACTIVE_TEAMS_2026
  sleeper_filtered <- sleeper %>%
    dplyr::filter(
      !is.na(.data$team),
      .data$position %in% ALLOC_POSITIONS
    ) %>%
    dplyr::mutate(team = .normalize_sleeper_team_codes_r31(.data$team)) %>%
    dplyr::filter(.data$team %in% ACTIVE_TEAMS_2026)

  # ----- Pass 1: direct gsis_id match -----
  sleeper_with_gsis <- sleeper_filtered %>%
    dplyr::filter(!is.na(.data$nfl_gsis_id)) %>%
    dplyr::transmute(
      nfl_gsis_id       = .data$nfl_gsis_id,
      player_name       = .data$player_name,
      team              = .data$team,
      position          = .data$position,
      depth_rank_raw    = as.integer(dplyr::coalesce(
        .data$depth_chart_order, 999L
      )),
      depth_was_missing = is.na(.data$depth_chart_order)
    ) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)

  # ----- Pass 2: name fallback match for Sleeper records missing gsis_id -----
  # Build nflreadr lookup keyed by normalized name + position, restricted to
  # unique matches (a name+position appearing only once in nflreadr's pool)
  # to avoid false-positive joins to similarly-named players.
  nflreadr_lookup <- nflreadr_dc %>%
    dplyr::mutate(
      name_norm = .normalize_name_for_match_r31(.data$player_name)
    ) %>%
    dplyr::group_by(.data$name_norm, .data$position) %>%
    dplyr::filter(dplyr::n() == 1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(name_norm, position, recovered_gsis_id = nfl_gsis_id)

  sleeper_no_gsis <- sleeper_filtered %>%
    dplyr::filter(is.na(.data$nfl_gsis_id)) %>%
    dplyr::mutate(
      name_norm = .normalize_name_for_match_r31(.data$player_name)
    )

  sleeper_recovered <- sleeper_no_gsis %>%
    dplyr::inner_join(nflreadr_lookup,
                       by = c("name_norm", "position")) %>%
    # Don't recover a gsis_id already covered by Pass 1
    dplyr::anti_join(sleeper_with_gsis, by = c("recovered_gsis_id" = "nfl_gsis_id")) %>%
    dplyr::transmute(
      nfl_gsis_id       = .data$recovered_gsis_id,
      player_name       = .data$player_name,
      team              = .data$team,
      position          = .data$position,
      depth_rank_raw    = as.integer(dplyr::coalesce(
        .data$depth_chart_order, 999L
      )),
      depth_was_missing = is.na(.data$depth_chart_order)
    ) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)

  # Combine both passes
  sleeper_rows <- dplyr::bind_rows(sleeper_with_gsis, sleeper_recovered)

  # nflreadr rows for players NOT covered by either Sleeper pass
  # depth_was_missing = FALSE for all nflreadr rows (their depth rank comes
  # from actual depth chart data, not Sleeper's often-incomplete pre-season
  # depth_chart_order field).
  sleeper_gsis <- sleeper_rows$nfl_gsis_id
  nflreadr_only <- nflreadr_dc %>%
    dplyr::filter(!.data$nfl_gsis_id %in% sleeper_gsis) %>%
    dplyr::mutate(depth_was_missing = FALSE)

  # Combine and re-rank within team + position
  unified <- dplyr::bind_rows(nflreadr_only, sleeper_rows) %>%
    dplyr::group_by(.data$team, .data$position) %>%
    dplyr::arrange(.data$depth_rank_raw, .data$player_name,
                    .by_group = TRUE) %>%
    dplyr::mutate(depth_rank_raw = dplyr::row_number()) %>%
    dplyr::ungroup()

  # Diagnostic counts
  team_lookup <- nflreadr_dc %>%
    dplyr::select(nfl_gsis_id, team_old = team)
  n_team_moved <- sleeper_rows %>%
    dplyr::inner_join(team_lookup, by = "nfl_gsis_id") %>%
    dplyr::filter(.data$team != .data$team_old) %>%
    nrow()
  n_recovered <- nrow(sleeper_recovered)
  n_added <- nrow(sleeper_rows) -
    nrow(dplyr::inner_join(
      sleeper_rows %>% dplyr::select(nfl_gsis_id),
      nflreadr_dc %>% dplyr::select(nfl_gsis_id),
      by = "nfl_gsis_id"
    ))

  message(glue(
    "    Depth chart Sleeper merge: ",
    "{n_team_moved} team change(s), ",
    "{n_recovered} recovered via name fallback, ",
    "{n_added} new rookie(s)/signing(s) added, ",
    "{nrow(unified)} total rows after re-rank"
  ))

  unified
}


# ------------------------------------------------------------------------------
# .load_2026_depth_charts
# ------------------------------------------------------------------------------

#' Load most recent available depth charts for the target season
#'
#' Tries the target season first (e.g., 2026 pre-season depth charts). If
#' empty, falls back to the final week of the prior season. Returns a tibble
#' with one row per player-team-position with the latest depth rank.
#'
#' @param season Integer. Target season (default SEASON_ALLOC).
#' @return Tibble: gsis_id, full_name, team, position, depth_position,
#'   depth_rank.
#' @keywords internal
.load_2026_depth_charts <- function(season = SEASON_ALLOC) {

  message(glue("  Loading depth charts for season {season}"))

  dc <- tryCatch(
    nflreadr::load_depth_charts(seasons = season),
    error = function(e) {
      message(glue("    Season {season} depth charts unavailable: ",
                   "{e$message}"))
      NULL
    }
  )

  if (is.null(dc) || nrow(dc) == 0L) {
    message(glue("    Falling back to season {season - 1L} final week"))
    dc <- tryCatch(
      nflreadr::load_depth_charts(seasons = season - 1L),
      error = function(e) NULL
    )

    if (is.null(dc) || nrow(dc) == 0L) {
      stop(".load_2026_depth_charts(): no depth chart data available")
    }
  }

  # Identify the latest week available.
  # Pre-season depth charts may not have a week column at all -- in that
  # case the full table is already the most recent snapshot.
  has_week_col <- "week" %in% names(dc) &&
                    any(!is.na(dc[["week"]]))

  if (has_week_col) {
    latest_week <- max(dc[["week"]], na.rm = TRUE)
    message(glue("    Filtering to week {latest_week}"))
    dc_latest <- dc %>%
      dplyr::filter(.data$week == latest_week) %>%
      dplyr::filter(!is.na(.data$gsis_id), nchar(.data$gsis_id) > 0)
  } else {
    message("    No week column -- using full depth chart snapshot")
    dc_latest <- dc %>%
      dplyr::filter(!is.na(.data$gsis_id), nchar(.data$gsis_id) > 0)
  }

  # Column name handling: depth_position vs position vs depth_chart_position
  # varies across nflreadr versions
  # pos_abb = player position abbreviation (WR/QB/RB/TE)
  # pos_grp = formation name (Base 4-3 D, 3WR 1TE) -- NOT a position column
  pos_col <- dplyr::case_when(
    "pos_abb"              %in% names(dc_latest) ~ "pos_abb",
    "depth_position"       %in% names(dc_latest) ~ "depth_position",
    "position"             %in% names(dc_latest) ~ "position",
    "depth_chart_position" %in% names(dc_latest) ~ "depth_chart_position",
    "pos_name"             %in% names(dc_latest) ~ "pos_name",
    TRUE                                          ~ NA_character_
  )[1]

  if (is.na(pos_col)) {
    stop(glue(".load_2026_depth_charts(): no position column found. ",
              "Available columns: {paste(names(dc_latest), collapse = ', ')}"))
  }

  team_col <- dplyr::case_when(
    "team"      %in% names(dc_latest) ~ "team",
    "club_code" %in% names(dc_latest) ~ "club_code",
    TRUE                               ~ NA_character_
  )[1]

  if (is.na(team_col)) {
    stop(".load_2026_depth_charts(): no team column found in depth charts")
  }

  # Rank column: pos_rank in newer nflreadr, depth_team in older
  rank_col <- dplyr::case_when(
    "pos_rank"           %in% names(dc_latest) ~ "pos_rank",
    "depth_team"         %in% names(dc_latest) ~ "depth_team",
    "depth_position_rank" %in% names(dc_latest) ~ "depth_position_rank",
    TRUE                                        ~ NA_character_
  )[1]

  # Name column: full_name preferred
  name_col <- dplyr::case_when(
    "full_name" %in% names(dc_latest) ~ "full_name",
    "player_name" %in% names(dc_latest) ~ "player_name",
    "first_name" %in% names(dc_latest) ~ "first_name",
    TRUE ~ NA_character_
  )[1]

  if (is.na(name_col)) {
    dc_latest$player_name_synth <- dc_latest$gsis_id
    name_col <- "player_name_synth"
  }

  result <- dc_latest %>%
    dplyr::transmute(
      nfl_gsis_id    = .data$gsis_id,
      player_name    = .data[[name_col]],
      # Apply both normalizers: .normalize_team_codes handles legacy NFL
      # codes (STL/OAK/SD); .normalize_sleeper_team_codes_r31 then maps
      # alternate-spelling codes (AZ, LAR, JAC, WSH) to canonical form.
      # Both passes needed so nflreadr's "AZ" rows aren't silently dropped
      # by the ACTIVE_TEAMS_2026 filter below.
      team           = .normalize_sleeper_team_codes_r31(
                          .normalize_team_codes(.data[[team_col]])
                       ),
      position       = .data[[pos_col]],
      depth_rank_raw = if (!is.na(rank_col)) as.integer(.data[[rank_col]]) else 1L
    ) %>%
    dplyr::filter(
      .data$position %in% ALLOC_POSITIONS,
      .data$team %in% ACTIVE_TEAMS_2026,
      !is.na(.data$nfl_gsis_id)
    ) %>%
    # One row per player: this data has one record per player-formation
    # combination (167K+ rows total). Keep the row with the lowest depth rank
    # per player-team-position, which represents their primary role.
    dplyr::group_by(.data$nfl_gsis_id, .data$team, .data$position) %>%
    dplyr::slice_min(.data$depth_rank_raw, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup()

  week_label <- if (exists("latest_week", inherits = FALSE)) {
    paste0("week ", latest_week)
  } else {
    "no week column"
  }
  message(glue("    Depth chart loaded: {nrow(result)} player-positions ",
               "({week_label})"))

  # Apply Sleeper overrides to catch offseason moves nflreadr/PFR has missed
  result <- .merge_sleeper_depth_overrides(result)

  result
}

# ------------------------------------------------------------------------------
# .classify_depth_position
# ------------------------------------------------------------------------------

#' Collapse raw depth_team rank into bucketed depth position string
#'
#' Caps the rank per position per MAX_DEPTH_RANK (e.g., 5 WRs per team kept
#' as WR1-WR5; deeper WRs all become WR5). Produces a normalized string
#' like "WR3" or "TE1" suitable for joining to TARGET_SHARE_PRIORS.
#'
#' @param positions Character vector of position codes (QB, RB, WR, TE).
#' @param ranks Integer vector of raw depth ranks.
#' @return Character vector of bucketed depth_position strings.
#' @keywords internal
.classify_depth_position <- function(positions, ranks) {
  max_ranks <- MAX_DEPTH_RANK[positions]
  capped <- pmin(ranks, max_ranks)
  capped <- pmax(capped, 1L)  # any 0 or negative becomes 1
  paste0(positions, capped)
}

# ------------------------------------------------------------------------------
# .load_rookie_draft_capital
# ------------------------------------------------------------------------------

#' Load 2026 rookie draft picks and assign capital multipliers
#'
#' Pulls nflreadr::load_draft_picks(season) and joins on gsis_id. Returns
#' the round for each rookie, plus the multiplier from
#' ROOKIE_CAPITAL_MULTIPLIERS. UDFAs (not in draft pool) get round NA
#' and the "U" multiplier (0.45).
#'
#' @param season Integer. Rookie class season (default SEASON_ALLOC).
#' @return Tibble: nfl_gsis_id, draft_round, rookie_capital_mult.
#' @keywords internal
.load_rookie_draft_capital <- function(season = SEASON_ALLOC) {

  message(glue("  Loading {season} draft picks"))

  picks <- tryCatch(
    nflreadr::load_draft_picks(seasons = season),
    error = function(e) {
      message(glue("    Draft picks unavailable: {e$message}"))
      NULL
    }
  )

  if (is.null(picks) || nrow(picks) == 0L) {
    message(glue("    No {season} draft data -- all rookies treated as UDFA"))
    return(tibble::tibble(
      nfl_gsis_id          = character(),
      draft_round          = integer(),
      rookie_capital_mult  = numeric()
    ))
  }

  # gsis_id column varies in nflreadr drafts table
  id_col <- dplyr::case_when(
    "gsis_id" %in% names(picks)    ~ "gsis_id",
    "player_id" %in% names(picks)  ~ "player_id",
    TRUE                            ~ NA_character_
  )[1]

  if (is.na(id_col)) {
    message("    Draft picks table has no gsis_id column -- skipping")
    return(tibble::tibble(
      nfl_gsis_id          = character(),
      draft_round          = integer(),
      rookie_capital_mult  = numeric()
    ))
  }

  result <- picks %>%
    dplyr::filter(!is.na(.data[[id_col]]), nchar(.data[[id_col]]) > 0) %>%
    dplyr::transmute(
      nfl_gsis_id  = .data[[id_col]],
      draft_round  = as.integer(.data$round)
    ) %>%
    dplyr::filter(!is.na(.data$draft_round)) %>%
    dplyr::mutate(
      round_key = as.character(.data$draft_round),
      rookie_capital_mult = ROOKIE_CAPITAL_MULTIPLIERS[.data$round_key],
      rookie_capital_mult = dplyr::coalesce(.data$rookie_capital_mult, 0.55)
    ) %>%
    dplyr::select(nfl_gsis_id, draft_round, rookie_capital_mult)

  message(glue("    Loaded {nrow(result)} drafted rookies"))

  result
}

# ------------------------------------------------------------------------------
# .load_dynasty_scores
# ------------------------------------------------------------------------------

#' Load R/28 dynasty score_final per player
#'
#' Used for talent multiplier computation. Players missing from R/28 output
#' get talent_multiplier = 1.0 (neutral) downstream.
#'
#' @param path Character. Path to R/28 CSV output.
#' @return Tibble: nfl_gsis_id, position, score_final.
#' @keywords internal
.load_dynasty_scores <- function(path = DYNASTY_SCORES_CSV) {

  if (!file.exists(path)) {
    message(glue("  R/28 dynasty scores not found at: {path}"))
    message("  All talent multipliers will default to 1.0 (neutral)")
    return(tibble::tibble(
      nfl_gsis_id = character(),
      position    = character(),
      score_final = numeric()
    ))
  }

  scores <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) {
      message(glue("  R/28 read failed: {e$message}"))
      NULL
    }
  )

  if (is.null(scores) || nrow(scores) == 0L) {
    return(tibble::tibble(
      nfl_gsis_id = character(),
      position    = character(),
      score_final = numeric()
    ))
  }

  required <- c("nfl_gsis_id", "position", "score_final")
  missing_cols <- setdiff(required, names(scores))
  if (length(missing_cols) > 0L) {
    message(glue("  R/28 CSV missing columns: ",
                 "{paste(missing_cols, collapse = ', ')}"))
    return(tibble::tibble(
      nfl_gsis_id = character(),
      position    = character(),
      score_final = numeric()
    ))
  }

  result <- scores %>%
    dplyr::select(nfl_gsis_id, position, score_final) %>%
    dplyr::filter(
      !is.na(.data$nfl_gsis_id), nchar(.data$nfl_gsis_id) > 0,
      !is.na(.data$score_final)
    )

  message(glue("  Loaded {nrow(result)} R/28 dynasty scores"))

  result
}

# ------------------------------------------------------------------------------
# Small numeric helpers for the veteran talent signal
# ------------------------------------------------------------------------------

#' Weighted mean with NA/zero-weight guards
#'
#' @param x Numeric vector of values.
#' @param w Numeric vector of weights (same length as x).
#' @return Weighted mean over entries with non-NA value and positive weight,
#'   or NA_real_ if no such entry exists.
#' @keywords internal
.wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

#' Z-score a vector; sd <= 0 or NA guarded to 1.0
#'
#' @param x Numeric vector.
#' @return (x - mean) / sd, computed over non-NA values.
#' @keywords internal
.zscore <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s <= 0) s <- 1.0
  (x - m) / s
}

#' OLS slope of y on x with degenerate-input guards
#'
#' Used for the WR EPA/target trajectory over the trailing window. Returns 0
#' when fewer than two distinct x values are available (a single season gives
#' no trajectory), so the slope component is neutral rather than NA.
#'
#' @param x Numeric predictor (season).
#' @param y Numeric response (per-season efficiency).
#' @return Slope coefficient, or 0.0 when undefined.
#' @keywords internal
.slope <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 2L || length(unique(x[ok])) < 2L) return(0.0)
  xv <- x[ok]
  yv <- y[ok]
  vx <- sum((xv - mean(xv))^2)
  if (vx <= 0) return(0.0)
  sum((xv - mean(xv)) * (yv - mean(yv))) / vx
}

#' Sample-size-shrunk standardized component via empirical Bayes
#'
#' Returns a standardized, shrinkage-attenuated component for each player:
#'   z_i = B_i * (theta_i - mu0) / sd0,   B_i = n_i / (n_i + K)
#' where mu0 and sd0 are the volume-weighted mean and SD of the rate, and K is
#' the empirical-Bayes shrinkage constant. Low-volume players have small B_i and
#' are pulled toward 0; high-volume players keep their standardized signal. This
#' attenuates the z directly (rather than shrinking the value and re-z-scoring,
#' which recompresses the spread and can re-inflate a moderate-volume outlier).
#'
#' K is estimated by moments: under theta_i ~ Normal(mu_i, c / n_i) with
#' mu_i ~ Normal(mu0, tau2), E[(theta_i - mu0)^2] = tau2 + c * (1 / n_i).
#' Regressing observed squared deviations on 1/n_i gives intercept tau2 and
#' slope c, and K = c / tau2. The moment estimate is not robust (a low-volume
#' outlier inflates the estimated between-variance and depresses its own
#' shrinkage), so K is floored at the median volume among qualifying players:
#' K = max(moment estimate, median volume). This keeps EB when it is stronger
#' and falls back to the robust median anchor when it is degenerate or too
#' weak. With realistic qualifying-veteran volumes the moment estimate is
#' typically the binding (stronger) term.
#'
#' Players with a non-finite theta (no usable sample for this component) return
#' 0 (neutral).
#'
#' @param theta Numeric vector of per-player rate estimates.
#' @param n Numeric vector of per-player sample sizes (same length as theta).
#' @param label Character tag for the diagnostic message.
#' @return Numeric vector of shrunk standardized components, same length as
#'   theta.
#' @keywords internal
.eb_shrink <- function(theta, n, label = "") {
  out <- rep(0.0, length(theta))
  ok  <- is.finite(theta) & is.finite(n) & n > 0
  if (!any(ok)) return(out)

  w   <- n[ok]
  mu0 <- sum(theta[ok] * w) / sum(w)
  sd0 <- sqrt(sum(w * (theta[ok] - mu0)^2) / sum(w))
  if (!is.finite(sd0) || sd0 <= 0) sd0 <- 1.0

  K      <- NA_real_
  method <- "EB"
  if (sum(ok) < VETERAN_EB_MIN_PLAYERS) {
    method <- "fallback (insufficient n)"
  } else {
    inv_n <- 1 / n[ok]
    d     <- (theta[ok] - mu0)^2
    if (stats::var(inv_n) > 0) {
      fit <- tryCatch(stats::lm(d ~ inv_n), error = function(e) NULL)
      if (!is.null(fit)) {
        co   <- stats::coef(fit)
        tau2 <- unname(co[[1]])
        cwin <- unname(co[[2]])
        if (is.finite(tau2) && is.finite(cwin) && tau2 > 0 && cwin > 0) {
          K <- cwin / tau2
        }
      }
    }
    if (!is.finite(K) || K <= 0) method <- "fallback (degenerate EB)"
  }

  # Robust anchor (option-2 fallback): median volume among qualifying players.
  # K is never allowed to shrink WEAKER than this, because the moment estimate
  # of K is not robust (a low-volume outlier inflates the estimated between-
  # variance and depresses its own shrinkage). Using max() keeps EB when it is
  # stronger and floors it at the robust anchor otherwise.
  anchor <- stats::median(n[ok], na.rm = TRUE)
  if (!is.finite(K) || K <= 0) {
    K <- anchor
  } else if (K < anchor) {
    K      <- anchor
    method <- paste0(method, " -> floored at median anchor")
  }

  B <- n / (n + K)
  z <- B * (theta - mu0) / sd0
  z[!is.finite(theta)] <- 0.0

  message(glue("    EB shrink [{label}]: method={method}, ",
               "K={format(round(K, 1), nsmall = 1)}, ",
               "mu0={round(mu0, 4)}, n_players={sum(ok)}"))

  z
}

# ------------------------------------------------------------------------------
# .load_player_season_panel
# ------------------------------------------------------------------------------

#' Load the R/16 player-season panel from the R/29-built cache
#'
#' Optional input for the veteran talent signal. If the cache is missing,
#' unreadable, or lacks required columns, returns an empty tibble and the
#' veteran signal is skipped (all players keep the prospect talent_z).
#'
#' @param cache_path Character. Path to the cached R/16 panel RDS.
#' @return Tibble with nfl_gsis_id, season, position, and the efficiency
#'   columns needed for veteran scoring.
#' @keywords internal
.load_player_season_panel <- function(cache_path = PANEL_CACHE_PATH_ALLOC) {

  empty <- tibble::tibble(
    nfl_gsis_id           = character(),
    season                = integer(),
    position              = character(),
    qb_dropbacks          = numeric(),
    pass_epa_per_dropback = numeric(),
    mean_cpoe             = numeric(),
    rush_attempts         = numeric(),
    rush_success_rate     = numeric(),
    rush_epa_per_attempt  = numeric(),
    targets               = numeric(),
    rec_epa_per_target    = numeric()
  )

  if (!file.exists(cache_path)) {
    message(glue("  R/16 panel cache not found at: {cache_path}"))
    message("  Veteran talent signal skipped; all players keep prospect talent_z")
    return(empty)
  }

  panel <- tryCatch(
    readRDS(cache_path),
    error = function(e) {
      message(glue("  R/16 panel read failed: {e$message}"))
      NULL
    }
  )

  if (is.null(panel) || nrow(panel) == 0L) {
    return(empty)
  }

  needed <- c("player_id", "season", "position", "qb_dropbacks",
              "pass_epa_per_dropback", "mean_cpoe", "rush_attempts",
              "rush_success_rate", "rush_epa_per_attempt", "targets",
              "rec_epa_per_target")
  missing_cols <- setdiff(needed, names(panel))
  if (length(missing_cols) > 0L) {
    message(glue("  R/16 panel missing columns: ",
                 "{paste(missing_cols, collapse = ', ')}; ",
                 "veteran signal skipped"))
    return(empty)
  }

  panel %>%
    dplyr::transmute(
      nfl_gsis_id           = .data$player_id,
      season                = as.integer(.data$season),
      position              = .data$position,
      qb_dropbacks          = .data$qb_dropbacks,
      pass_epa_per_dropback = .data$pass_epa_per_dropback,
      mean_cpoe             = .data$mean_cpoe,
      rush_attempts         = .data$rush_attempts,
      rush_success_rate     = .data$rush_success_rate,
      rush_epa_per_attempt  = .data$rush_epa_per_attempt,
      targets               = .data$targets,
      rec_epa_per_target    = .data$rec_epa_per_target
    )
}

# ------------------------------------------------------------------------------
# .load_roster_experience
# ------------------------------------------------------------------------------

#' Load years-of-experience per player for the target season
#'
#' Sources years_exp from nflreadr::load_rosters(season). If unavailable or the
#' expected columns are absent (older nflreadr), returns an empty tibble and no
#' player is flagged as a veteran (everyone keeps the prospect talent_z).
#'
#' @param season Integer. Target season (default SEASON_ALLOC).
#' @return Tibble: nfl_gsis_id, years_exp.
#' @keywords internal
.load_roster_experience <- function(season = SEASON_ALLOC) {

  empty <- tibble::tibble(nfl_gsis_id = character(), years_exp = integer())

  rosters <- tryCatch(
    nflreadr::load_rosters(seasons = season),
    error = function(e) {
      message(glue("  load_rosters({season}) failed: {e$message}"))
      NULL
    }
  )

  if (is.null(rosters) || nrow(rosters) == 0L) {
    message("  Roster experience unavailable; no veterans flagged")
    return(empty)
  }

  if (!all(c("gsis_id", "years_exp") %in% names(rosters))) {
    message("  Roster data missing gsis_id/years_exp; no veterans flagged")
    return(empty)
  }

  rosters %>%
    dplyr::filter(!is.na(.data$gsis_id), nchar(.data$gsis_id) > 0) %>%
    dplyr::transmute(
      nfl_gsis_id = .data$gsis_id,
      years_exp   = suppressWarnings(as.integer(.data$years_exp))
    ) %>%
    dplyr::filter(!is.na(.data$years_exp)) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)
}

# ------------------------------------------------------------------------------
# .compute_veteran_talent_z
# ------------------------------------------------------------------------------

#' Veteran talent_z, NEUTRALIZED (retired on out-of-sample evidence)
#'
#' An out-of-sample MAE backtest over outcome seasons 2013 to 2025 showed that
#' every veteran talent signal buildable from the R/16 panel increased
#' projection error versus no adjustment, at every position. The efficiency
#' composite was worst (QB +0.48 MAE vs neutral). The opportunity-scoring gap
#' only appeared to help against a one-year prior-PPG baseline (regression to
#' the mean) and hurt against the stabler 3-season window-mean baseline.
#' Decision: apply no veteran talent nudge. This function returns a neutral
#' veteran_talent_z of 0 for every veteran clearing the per-position
#' window-volume floor, yielding talent_multiplier = 1.0 downstream. Veterans
#' are valued by depth slot and team volume. Prospects are untouched and keep
#' their R/28 college talent_z via .compute_talent_multipliers. Gating and the
#' window_volume / n_window_seasons diagnostics are preserved; TE is now
#' included in the gate so veteran TEs are neutralized, not left on a stale
#' college score.
#'
#' @param panel Tibble from .load_player_season_panel().
#' @param roster_exp Tibble from .load_roster_experience().
#' @param season Integer. Target season (default SEASON_ALLOC).
#' @return Tibble: nfl_gsis_id, position, veteran_talent_z (all 0),
#'   window_volume, n_window_seasons. Empty if no veteran clears its floor.
#' @keywords internal
.compute_veteran_talent_z <- function(panel, roster_exp,
                                       season = SEASON_ALLOC) {

  empty <- tibble::tibble(
    nfl_gsis_id      = character(),
    position         = character(),
    veteran_talent_z = numeric(),
    window_volume    = numeric(),
    n_window_seasons = integer()
  )

  if (nrow(panel) == 0L || nrow(roster_exp) == 0L) return(empty)

  window_seasons <- (season - VETERAN_WINDOW_SEASONS):(season - 1L)

  veteran_ids <- roster_exp %>%
    dplyr::filter(.data$years_exp >= VETERAN_MIN_EXP) %>%
    dplyr::select(nfl_gsis_id)

  # Per-position window-volume floor on primary usage. TE reuses the WR target
  # floor. Gating is retained so the veteran population and the diagnostic
  # columns match the prior version; only the returned z changed (now 0).
  floors <- c(QB = VETERAN_MIN_QB_DROPBACKS,
              RB = VETERAN_MIN_RB_ATTEMPTS,
              WR = VETERAN_MIN_WR_TARGETS,
              TE = VETERAN_MIN_WR_TARGETS)

  vets <- panel %>%
    dplyr::filter(.data$season %in% window_seasons,
                  .data$position %in% c("QB", "RB", "WR", "TE")) %>%
    dplyr::inner_join(veteran_ids, by = "nfl_gsis_id") %>%
    dplyr::group_by(.data$nfl_gsis_id, .data$position) %>%
    dplyr::summarise(
      w_dropbacks      = sum(.data$qb_dropbacks,  na.rm = TRUE),
      w_rush_att       = sum(.data$rush_attempts, na.rm = TRUE),
      w_targets        = sum(.data$targets,       na.rm = TRUE),
      n_window_seasons = dplyr::n_distinct(.data$season),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      window_volume = dplyr::case_when(
        .data$position == "QB" ~ .data$w_dropbacks,
        .data$position == "RB" ~ .data$w_rush_att,
        TRUE                   ~ .data$w_targets
      )
    ) %>%
    dplyr::filter(.data$window_volume >= floors[.data$position])

  if (nrow(vets) == 0L) return(empty)

  # NEUTRAL: no player-level talent nudge for veterans (see header note).
  vets %>%
    dplyr::mutate(veteran_talent_z = 0) %>%
    dplyr::select(nfl_gsis_id, position, veteran_talent_z,
                  window_volume, n_window_seasons)
}

# ------------------------------------------------------------------------------
# .compute_talent_multipliers
# ------------------------------------------------------------------------------

#' Compute talent multiplier per player from R/28 score_final
#'
#' Compute talent multiplier per player, with veteran override
#'
#' Prospects receive talent_z from R/28 score_final, z-scored within position
#' (formula unchanged). Veterans with a usable NFL-efficiency signal
#' (veteran_z) replace that with their veteran talent_z (hard switch). The
#' multiplier is 1 + TALENT_MULT_SENSITIVITY * talent_z, capped at
#' [TALENT_MULT_FLOOR, TALENT_MULT_CEILING].
#'
#' Resolution per player: veteran NFL z if present, else prospect college z if
#' present, else neutral (talent_z = 0). talent_source records which applied.
#'
#' @param scores Tibble from .load_dynasty_scores().
#' @param veteran_z Tibble from .compute_veteran_talent_z(), or NULL.
#' @return Tibble: nfl_gsis_id, score_final, talent_z, talent_multiplier,
#'   talent_source.
#' @keywords internal
.compute_talent_multipliers <- function(scores, veteran_z = NULL) {

  if (is.null(veteran_z)) {
    veteran_z <- tibble::tibble(
      nfl_gsis_id      = character(),
      position         = character(),
      veteran_talent_z = numeric()
    )
  }

  empty_out <- tibble::tibble(
    nfl_gsis_id       = character(),
    score_final       = numeric(),
    talent_z          = numeric(),
    talent_multiplier = numeric(),
    talent_source     = character(),
    window_volume     = numeric(),
    n_window_seasons  = integer()
  )

  if (nrow(scores) == 0L && nrow(veteran_z) == 0L) {
    return(empty_out)
  }

  # ---- Prospect talent_z from R/28 score_final (unchanged formula) ----
  if (nrow(scores) > 0L) {
    pos_stats <- scores %>%
      dplyr::group_by(.data$position) %>%
      dplyr::summarise(
        position_mean = mean(.data$score_final, na.rm = TRUE),
        position_sd   = stats::sd(.data$score_final, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        position_sd = dplyr::if_else(
          is.na(.data$position_sd) | .data$position_sd <= 0,
          1.0, .data$position_sd
        )
      )

    prospect <- scores %>%
      dplyr::left_join(pos_stats, by = "position") %>%
      dplyr::transmute(
        nfl_gsis_id       = .data$nfl_gsis_id,
        score_final       = .data$score_final,
        talent_z_prospect = (.data$score_final - .data$position_mean) /
                            .data$position_sd
      )
  } else {
    prospect <- tibble::tibble(
      nfl_gsis_id       = character(),
      score_final       = numeric(),
      talent_z_prospect = numeric()
    )
  }

  # ---- Combine: veteran NFL z overrides prospect z (hard switch) ----
  vet <- veteran_z %>%
    dplyr::select(dplyr::any_of(c("nfl_gsis_id", "veteran_talent_z",
                                  "window_volume", "n_window_seasons")))

  # Ensure diagnostic columns exist even when veteran_z came in without them
  if (!"window_volume" %in% names(vet))    vet$window_volume <- numeric(0)
  if (!"n_window_seasons" %in% names(vet)) vet$n_window_seasons <- integer(0)

  dplyr::full_join(prospect, vet, by = "nfl_gsis_id") %>%
    dplyr::mutate(
      talent_source = dplyr::case_when(
        !is.na(.data$veteran_talent_z)  ~ "veteran_nfl",
        !is.na(.data$talent_z_prospect) ~ "prospect_college",
        TRUE                            ~ "neutral"
      ),
      talent_z = dplyr::case_when(
        !is.na(.data$veteran_talent_z)  ~ .data$veteran_talent_z,
        !is.na(.data$talent_z_prospect) ~ .data$talent_z_prospect,
        TRUE                            ~ 0.0
      ),
      talent_multiplier = 1 + TALENT_MULT_SENSITIVITY * .data$talent_z,
      talent_multiplier = pmin(
        pmax(.data$talent_multiplier, TALENT_MULT_FLOOR),
        TALENT_MULT_CEILING
      )
    ) %>%
    dplyr::select(nfl_gsis_id, score_final, talent_z, talent_multiplier,
                  talent_source, window_volume, n_window_seasons)
}

# ------------------------------------------------------------------------------
# .build_player_allocation_table
# ------------------------------------------------------------------------------

#' Assemble per-player allocation inputs by joining all sources
#'
#' Joins depth chart, rookie capital, and talent multipliers into a single
#' tibble. Players without R/28 scores keep talent_multiplier = 1.0.
#' Non-rookies keep rookie_capital_mult = 1.0 (no draft capital adjustment).
#'
#' @param depth_chart Tibble from .load_2026_depth_charts().
#' @param rookies Tibble from .load_rookie_draft_capital().
#' @param talent Tibble from .compute_talent_multipliers().
#' @return Tibble with one row per player containing all allocation inputs.
#' @keywords internal
.build_player_allocation_table <- function(depth_chart, rookies, talent) {

  rookie_ids <- rookies$nfl_gsis_id

  base <- depth_chart %>%
    dplyr::mutate(
      depth_rank     = pmax(.data$depth_rank_raw, 1L),
      depth_position = .classify_depth_position(.data$position,
                                                  .data$depth_rank),
      is_rookie      = .data$nfl_gsis_id %in% rookie_ids
    )

  with_rookie <- base %>%
    dplyr::left_join(
      rookies %>% dplyr::select(nfl_gsis_id, draft_round,
                                  rookie_capital_mult),
      by = "nfl_gsis_id"
    ) %>%
    dplyr::mutate(
      # Non-rookies get 1.0 (no capital adjustment); rookies without draft
      # entry get the UDFA multiplier
      rookie_capital_mult = dplyr::case_when(
        !.data$is_rookie ~ 1.0,
        is.na(.data$rookie_capital_mult) ~ ROOKIE_CAPITAL_MULTIPLIERS[["U"]],
        TRUE ~ .data$rookie_capital_mult
      ),
      draft_round = dplyr::if_else(
        .data$is_rookie, .data$draft_round, NA_integer_
      )
    )

  with_talent <- with_rookie %>%
    dplyr::left_join(
      talent %>% dplyr::select(nfl_gsis_id, score_final, talent_z,
                                 talent_multiplier, talent_source,
                                 window_volume, n_window_seasons),
      by = "nfl_gsis_id"
    ) %>%
    dplyr::mutate(
      talent_multiplier = dplyr::coalesce(.data$talent_multiplier, 1.0),
      talent_z          = dplyr::coalesce(.data$talent_z, 0.0),
      talent_source     = dplyr::coalesce(.data$talent_source, "neutral")
    )

  with_talent
}

# ------------------------------------------------------------------------------
# .compute_initial_shares
# ------------------------------------------------------------------------------

#' Compute pre-constraint target and rush shares for each player
#'
#' Applies the three-stage formula:
#'   base    = lookup TARGET_SHARE_PRIORS[depth_position]
#'   adjusted = base * rookie_capital_mult * talent_multiplier
#'
#' Same for rush shares using RUSH_SHARE_PRIORS.
#'
#' @param alloc_table Tibble from .build_player_allocation_table().
#' @return Tibble with target_share_base, target_share_adjusted,
#'   rush_share_base, rush_share_adjusted columns added.
#' @keywords internal
.compute_initial_shares <- function(alloc_table) {

  alloc_table %>%
    dplyr::mutate(
      target_share_base = dplyr::coalesce(
        TARGET_SHARE_PRIORS[.data$depth_position], 0
      ),
      rush_share_base = dplyr::coalesce(
        RUSH_SHARE_PRIORS[.data$depth_position], 0
      ),
      target_share_adjusted = .data$target_share_base *
                                .data$rookie_capital_mult *
                                .data$talent_multiplier,
      rush_share_adjusted   = .data$rush_share_base *
                                .data$rookie_capital_mult *
                                .data$talent_multiplier
    )
}

# ------------------------------------------------------------------------------
# .compute_prior_season_shares
# ------------------------------------------------------------------------------

#' Build each player's realized prior-season usage share (carryover source)
#'
#' Reads the prior-season (CARRYOVER_PRIOR_SEASON) normalized pbp once to build
#' clean team target and carry denominators using the exact definitions the
#' carryover weights were validated against: a target is a receiver-charged
#' pass attempt (receiver_player_id present, pass_attempt == 1, not a two-point
#' try); a carry is a designed rush attempt (rush_attempt == 1, not a kneel or
#' two-point try). Player numerators (targets, rush_attempts, qb_dropbacks) come
#' from the R/16 panel's prior-season row. Team codes on both sides are put
#' through the same normalizers .load_2026_depth_charts() uses so the join is
#' consistent.
#'
#' Degrades gracefully: if the prior-season pbp or panel is unavailable, or the
#' expected columns are absent, returns an empty tibble and carryover applies to
#' no one (every player keeps the positional prior path).
#'
#' @param panel_path Character. Path to the cached R/16 panel RDS.
#' @param prior_season Integer. Season whose shares are carried over.
#' @return Tibble keyed by nfl_gsis_id: prior_team, prior_position,
#'   prior_target_share, prior_rush_share, prior_vol_targets, prior_vol_rush,
#'   prior_vol_dropbacks. Empty tibble on any failure.
#' @keywords internal
.compute_prior_season_shares <- function(panel_path   = PANEL_CACHE_PATH_ALLOC,
                                         prior_season = CARRYOVER_PRIOR_SEASON) {

  empty <- tibble::tibble(
    nfl_gsis_id         = character(),
    prior_team          = character(),
    prior_position      = character(),
    prior_target_share  = numeric(),
    prior_rush_share    = numeric(),
    prior_vol_targets   = numeric(),
    prior_vol_rush      = numeric(),
    prior_vol_dropbacks = numeric()
  )

  # Prior-season pbp for clean team denominators (load_normalized_season is
  # reachable via R/15, sourced transitively through R/30). Read from the same
  # cache dir the panel lives in so the path is explicit, not R/15's default.
  cache_dir <- dirname(panel_path)
  pbp <- tryCatch(
    load_normalized_season(prior_season, cache_dir = cache_dir),
    error = function(e) {
      message(glue("    Carryover: prior-season {prior_season} pbp ",
                   "unavailable ({e$message}) -- carryover skipped"))
      NULL
    }
  )
  if (is.null(pbp) || nrow(pbp) == 0L) return(empty)

  needed_pbp <- c("season_type", "posteam", "receiver_player_id",
                  "pass_attempt", "two_point_attempt", "rush_attempt",
                  "qb_kneel")
  if (length(setdiff(needed_pbp, names(pbp))) > 0L) {
    message("    Carryover: prior-season pbp missing required columns -- ",
            "carryover skipped")
    return(empty)
  }

  reg <- pbp %>% dplyr::filter(.data$season_type == "REG")
  team_targets <- reg %>%
    dplyr::filter(!is.na(.data$receiver_player_id),
                  .data$pass_attempt == 1L,
                  dplyr::coalesce(.data$two_point_attempt, 0L) != 1L) %>%
    dplyr::group_by(.data$posteam) %>%
    dplyr::summarise(team_targets = dplyr::n(), .groups = "drop")
  team_carries <- reg %>%
    dplyr::filter(.data$rush_attempt == 1L,
                  dplyr::coalesce(.data$qb_kneel, 0L) != 1L,
                  dplyr::coalesce(.data$two_point_attempt, 0L) != 1L) %>%
    dplyr::group_by(.data$posteam) %>%
    dplyr::summarise(team_carries = dplyr::n(), .groups = "drop")

  team_totals <- dplyr::full_join(team_targets, team_carries, by = "posteam") %>%
    dplyr::mutate(
      team = .normalize_sleeper_team_codes_r31(
               .normalize_team_codes(.data$posteam)),
      team_targets = dplyr::coalesce(.data$team_targets, 0),
      team_carries = dplyr::coalesce(.data$team_carries, 0)
    ) %>%
    dplyr::group_by(.data$team) %>%
    dplyr::summarise(team_targets = sum(.data$team_targets),
                     team_carries = sum(.data$team_carries),
                     .groups = "drop")

  rm(pbp, reg); invisible(gc(verbose = FALSE))

  # Player numerators from the raw R/16 panel. Read the RDS directly rather than
  # via .load_player_season_panel(), which transmutes away the team column that
  # the share denominator join needs.
  panel_raw <- tryCatch(readRDS(panel_path), error = function(e) NULL)
  if (is.null(panel_raw) || nrow(panel_raw) == 0L) {
    message("    Carryover: panel unavailable -- carryover skipped")
    return(empty)
  }
  needed_panel <- c("player_id", "season", "team", "position",
                    "targets", "rush_attempts", "qb_dropbacks")
  if (length(setdiff(needed_panel, names(panel_raw))) > 0L) {
    message("    Carryover: panel missing required columns -- carryover skipped")
    return(empty)
  }

  n_unmatched <- 0L
  result <- panel_raw %>%
    dplyr::filter(as.integer(.data$season) == prior_season,
                  .data$position %in% ALLOC_POSITIONS) %>%
    dplyr::transmute(
      nfl_gsis_id         = as.character(.data$player_id),
      prior_team          = .normalize_sleeper_team_codes_r31(
                              .normalize_team_codes(as.character(.data$team))),
      prior_position      = .data$position,
      prior_vol_targets   = .data$targets,
      prior_vol_rush      = .data$rush_attempts,
      prior_vol_dropbacks = .data$qb_dropbacks
    ) %>%
    dplyr::left_join(team_totals, by = c("prior_team" = "team")) %>%
    dplyr::mutate(
      prior_target_share = dplyr::if_else(
        !is.na(.data$team_targets) & .data$team_targets > 0,
        .data$prior_vol_targets / .data$team_targets, NA_real_),
      prior_rush_share = dplyr::if_else(
        !is.na(.data$team_carries) & .data$team_carries > 0,
        .data$prior_vol_rush / .data$team_carries, NA_real_)
    )

  # Surface (do not silently drop) any player whose prior team failed to match a
  # team total, so an unmapped code is visible the way BLT/CLV/HST/SL were.
  n_unmatched <- sum(is.na(result$team_targets))
  if (n_unmatched > 0L) {
    bad <- result %>% dplyr::filter(is.na(.data$team_targets)) %>%
      dplyr::distinct(.data$prior_team) %>% dplyr::pull(.data$prior_team)
    message(glue("    Carryover: {n_unmatched} prior-season rows unmatched to a ",
                 "team total (codes: {paste(bad, collapse = ', ')})"))
  }

  result %>%
    dplyr::select(nfl_gsis_id, prior_team, prior_position,
                  prior_target_share, prior_rush_share,
                  prior_vol_targets, prior_vol_rush, prior_vol_dropbacks) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)
}

# ------------------------------------------------------------------------------
# .apply_preseason_carryover
# ------------------------------------------------------------------------------

#' Blend eligible returning players' primary share toward their prior-season
#' realized share (preseason analogue of .blend_observed_shares)
#'
#' For each eligible player, overwrites the PRIMARY adjusted share
#' (target_share_adjusted for WR/TE, rush_share_adjusted for RB/QB) with
#'   w_pos * prior_season_share + (1 - w_pos) * positional_base
#' using the position weight in PRESEASON_CARRYOVER_WEIGHTS and the positional
#' BASE, so the validated carryover supersedes the rookie/talent multiplier.
#' Ineligible players (rookies, movers, below-floor, no prior row) are left on
#' the positional prior path untouched. Runs AFTER .compute_initial_shares and
#' BEFORE .blend_observed_shares, so in-season observed data tapers from the
#' carryover baseline rather than from the flat prior.
#'
#' Adds audit columns (present in both on/off modes for schema stability):
#'   carryover_applied     lgl
#'   carryover_prior_share dbl  (prior primary share used; NA if not applied)
#'   carryover_weight      dbl  (w_pos used; NA if not applied)
#'
#' @param alloc_table Tibble from .compute_initial_shares(): needs nfl_gsis_id,
#'   position, team, is_rookie, target_share_base, rush_share_base,
#'   target_share_adjusted, rush_share_adjusted.
#' @param prior_shares Tibble from .compute_prior_season_shares(), or NULL.
#' @param enabled Logical. FALSE returns alloc_table with neutral audit columns
#'   and no share change.
#' @return alloc_table with primary shares blended for eligible players plus the
#'   three audit columns.
#' @keywords internal
.apply_preseason_carryover <- function(alloc_table, prior_shares = NULL,
                                       enabled = TRUE) {

  neutral <- alloc_table %>%
    dplyr::mutate(
      carryover_applied     = FALSE,
      carryover_prior_share = NA_real_,
      carryover_weight      = NA_real_
    )

  if (!enabled) return(neutral)
  if (is.null(prior_shares) || nrow(prior_shares) == 0L) {
    message("    Preseason carryover: no prior-season shares -- ",
            "positional priors retained for all players")
    return(neutral)
  }

  joined <- neutral %>%
    dplyr::left_join(
      prior_shares %>%
        dplyr::select(nfl_gsis_id, prior_team, prior_target_share,
                      prior_rush_share, prior_vol_targets, prior_vol_rush,
                      prior_vol_dropbacks),
      by = "nfl_gsis_id"
    ) %>%
    dplyr::mutate(
      w_pos     = PRESEASON_CARRYOVER_WEIGHTS[.data$position],
      floor_pos = CARRYOVER_FLOOR[.data$position],
      prior_primary_share = dplyr::if_else(
        .data$position %in% c("WR", "TE"),
        .data$prior_target_share, .data$prior_rush_share),
      prior_primary_vol = dplyr::case_when(
        .data$position == "QB" ~ .data$prior_vol_dropbacks,
        .data$position == "RB" ~ .data$prior_vol_rush,
        .data$position %in% c("WR", "TE") ~ .data$prior_vol_targets,
        TRUE ~ NA_real_),
      primary_base = dplyr::if_else(
        .data$position %in% c("WR", "TE"),
        .data$target_share_base, .data$rush_share_base),
      eligible = !.data$is_rookie &
                 !is.na(.data$prior_primary_share) &
                 !is.na(.data$prior_team) &
                 .data$team == .data$prior_team &
                 !is.na(.data$prior_primary_vol) &
                 .data$prior_primary_vol >= .data$floor_pos
    )

  blended <- joined %>%
    dplyr::mutate(
      carryover_applied     = dplyr::coalesce(.data$eligible, FALSE),
      carryover_prior_share = dplyr::if_else(.data$carryover_applied,
                                             .data$prior_primary_share, NA_real_),
      carryover_weight      = dplyr::if_else(.data$carryover_applied,
                                             .data$w_pos, NA_real_),
      blend_primary = dplyr::if_else(
        .data$carryover_applied,
        .data$w_pos * .data$prior_primary_share +
          (1 - .data$w_pos) * .data$primary_base,
        NA_real_),
      target_share_adjusted = dplyr::if_else(
        .data$carryover_applied & .data$position %in% c("WR", "TE"),
        .data$blend_primary, .data$target_share_adjusted),
      rush_share_adjusted = dplyr::if_else(
        .data$carryover_applied & .data$position %in% c("RB", "QB"),
        .data$blend_primary, .data$rush_share_adjusted)
    ) %>%
    dplyr::select(-w_pos, -floor_pos, -prior_primary_share, -prior_primary_vol,
                  -primary_base, -eligible, -blend_primary,
                  -prior_team, -prior_target_share, -prior_rush_share,
                  -prior_vol_targets, -prior_vol_rush, -prior_vol_dropbacks)

  n_applied <- sum(blended$carryover_applied, na.rm = TRUE)
  message(glue("    Preseason carryover: applied to {n_applied} of ",
               "{nrow(blended)} players (returning, non-mover, above floor)"))

  blended
}

# ------------------------------------------------------------------------------
# .blend_observed_shares
# ------------------------------------------------------------------------------

#' Blend preseason adjusted shares toward observed current-season shares
#'
#' In-season hook (v2). Blends each player's preseason adjusted target and
#' rush share toward their observed current-season share:
#'
#'   blended = pw * preseason_adjusted + (1 - pw) * observed
#'   where pw = compute_prior_weight(as_of_week)
#'
#' Runs BEFORE the soft constraint so the constraint operates once on the
#' blended shares. Players with no observed record (injured all year, not in
#' the supplied table) retain their preseason share at full weight -- their
#' absence is not evidence of a zero share.
#'
#' Guards (all retain preseason shares unchanged, with a message):
#'   - as_of_week < MIN_WEEKS_OBSERVED_ALLOC (single-game noise dominates)
#'   - current_season_volume NULL or empty
#'   - current_season_volume missing required columns
#'
#' @param alloc_table Tibble. Output of .compute_initial_shares(): must have
#'   nfl_gsis_id, target_share_adjusted, rush_share_adjusted.
#' @param current_season_volume Tibble or NULL. Caller-supplied observed
#'   shares: nfl_gsis_id (chr), observed_target_share (dbl, player targets /
#'   team targets to date), observed_rush_share (dbl, player carries / team
#'   carries to date). NA in either share column means no observation for
#'   that play type; the preseason share is retained for that component.
#' @param as_of_week Integer 1-18 or NULL. NULL = preseason mode, no blend.
#' @return alloc_table with target_share_adjusted / rush_share_adjusted
#'   blended where observed data exists, plus audit columns
#'   observed_target_share, observed_rush_share, share_blend_weight.
#' @seealso compute_prior_weight (R/29), .enforce_soft_constraints
#' @keywords internal
.blend_observed_shares <- function(alloc_table,
                                   current_season_volume = NULL,
                                   as_of_week = NULL) {

  # Audit columns exist in both modes so the output schema is stable.
  neutral <- alloc_table %>%
    dplyr::mutate(
      observed_target_share = NA_real_,
      observed_rush_share   = NA_real_,
      share_blend_weight    = NA_real_
    )

  if (is.null(as_of_week)) return(neutral)

  if (is.null(current_season_volume) || nrow(current_season_volume) == 0L) {
    message("    In-season blend: no current_season_volume supplied -- ",
            "preseason shares retained")
    return(neutral)
  }

  req_cols <- c("nfl_gsis_id", "observed_target_share", "observed_rush_share")
  missing_cols <- setdiff(req_cols, names(current_season_volume))
  if (length(missing_cols) > 0L) {
    message(glue("    In-season blend: current_season_volume missing ",
                 "column(s) {paste(missing_cols, collapse = ', ')} -- ",
                 "preseason shares retained"))
    return(neutral)
  }

  if (as_of_week < MIN_WEEKS_OBSERVED_ALLOC) {
    message(glue("    In-season blend: week {as_of_week} is below the ",
                 "{MIN_WEEKS_OBSERVED_ALLOC}-week observation floor ",
                 "(mirrors R/29) -- preseason shares retained"))
    return(neutral)
  }

  pw <- compute_prior_weight(as_of_week)
  message(glue("    In-season blend at week {as_of_week}: preseason weight ",
               "{format(round(pw, 3), nsmall = 3)}, observed weight ",
               "{format(round(1 - pw, 3), nsmall = 3)}"))

  blended <- alloc_table %>%
    dplyr::left_join(
      current_season_volume %>%
        dplyr::select(nfl_gsis_id, observed_target_share,
                      observed_rush_share) %>%
        dplyr::distinct(nfl_gsis_id, .keep_all = TRUE),
      by = "nfl_gsis_id"
    ) %>%
    dplyr::mutate(
      share_blend_weight = pw,
      target_share_adjusted = dplyr::if_else(
        is.na(.data$observed_target_share),
        .data$target_share_adjusted,
        pw * .data$target_share_adjusted +
          (1 - pw) * .data$observed_target_share
      ),
      rush_share_adjusted = dplyr::if_else(
        is.na(.data$observed_rush_share),
        .data$rush_share_adjusted,
        pw * .data$rush_share_adjusted +
          (1 - pw) * .data$observed_rush_share
      )
    )

  n_matched <- sum(!is.na(blended$observed_target_share) |
                     !is.na(blended$observed_rush_share))
  message(glue("    In-season blend: {n_matched} of {nrow(blended)} ",
               "players matched to observed shares"))

  blended
}

# ------------------------------------------------------------------------------
# .enforce_soft_constraints
# ------------------------------------------------------------------------------

#' Rescale per-team shares only when outside [CONSTRAINT_LOWER, CONSTRAINT_UPPER]
#'
#' For each team, sums adjusted target shares and adjusted rush shares.
#' If sum falls outside the constraint bounds, rescales all players on
#' that team proportionally so the sum equals 1.0. If within bounds,
#' leaves the shares untouched (preserves modeled signal).
#'
#' @param alloc_table Tibble with target_share_adjusted and
#'   rush_share_adjusted columns.
#' @return Same tibble with target_share and rush_share columns added
#'   (post-constraint values).
#' @keywords internal
.enforce_soft_constraints <- function(alloc_table) {

  team_sums <- alloc_table %>%
    dplyr::group_by(.data$team) %>%
    dplyr::summarise(
      team_target_sum = sum(.data$target_share_adjusted, na.rm = TRUE),
      team_rush_sum   = sum(.data$rush_share_adjusted, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      team_target_rescale = dplyr::case_when(
        .data$team_target_sum < CONSTRAINT_LOWER |
          .data$team_target_sum > CONSTRAINT_UPPER ~
          1.0 / pmax(.data$team_target_sum, RESCALE_SUM_FLOOR),
        TRUE ~ 1.0
      ),
      team_rush_rescale = dplyr::case_when(
        .data$team_rush_sum < CONSTRAINT_LOWER |
          .data$team_rush_sum > CONSTRAINT_UPPER ~
          1.0 / pmax(.data$team_rush_sum, RESCALE_SUM_FLOOR),
        TRUE ~ 1.0
      )
    )

  alloc_table %>%
    dplyr::left_join(team_sums, by = "team") %>%
    dplyr::mutate(
      target_share = .data$target_share_adjusted *
                       dplyr::coalesce(.data$team_target_rescale, 1.0),
      rush_share   = .data$rush_share_adjusted *
                       dplyr::coalesce(.data$team_rush_rescale, 1.0)
    )
}

# ------------------------------------------------------------------------------
# .qb_name_key
# ------------------------------------------------------------------------------

#' Normalize a player name to a first-initial + last-name key for college joins
#'
#' College play-by-play names appear both fully ("Fernando Mendoza") and
#' abbreviated ("F.Mendoza"), and the NFL side may carry suffixes. This collapses
#' all forms to "<first_initial>_<last>" so a rookie's split college rows pool to
#' one player. Vectorized. Periods are treated as separators (handles F.Mendoza),
#' generational suffixes dropped, non-letters removed.
#'
#' @param name Character vector of player names.
#' @return Character vector of normalized keys (NA for empty/unparseable).
#' @keywords internal
.qb_name_key <- function(name) {
  vapply(name, function(x) {
    if (is.na(x)) return(NA_character_)
    n <- tolower(x)
    n <- gsub("\\.", " ", n)                       # period -> space (F.Mendoza)
    n <- gsub("[^a-z ]", " ", n)                   # other punctuation -> space
    n <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", " ", n) # drop generational suffixes
    n <- trimws(gsub("\\s+", " ", n))
    parts <- strsplit(n, " ", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0L) return(NA_character_)
    paste0(substr(parts[1], 1, 1), "_", parts[length(parts)])
  }, character(1), USE.NAMES = FALSE)
}

# ------------------------------------------------------------------------------
# .compute_rookie_qb_rush_rate
# ------------------------------------------------------------------------------

#' College-tier rush-rate fallback for no-history (rookie) QB1s (v3_5)
#'
#' A rookie QB1 has no NFL rush history, so his expected carries come from a
#' three-tier step function: pool his college rush/game (game-weighted, from the
#' R/21 CFB panel, name-normalized to collapse split spellings), rank him against
#' the college QB starter distribution (games_played >= CFB_QB_REF_MIN_GAMES),
#' and map his tier to the NFL QB1 pooled rate at the matching quantile:
#'   college >= college-Q75 -> mobile   -> NFL QB1 Q75 rate
#'   college >= college-Q50 -> balanced -> NFL QB1 median rate
#'   college <  college-Q50 -> pocket   -> NFL QB1 Q25 rate
#' The college rate assigns a TIER ONLY; it is not translated to an NFL rate
#' (the two scales are not calibrated). Rookies with no college match are omitted
#' (the caller then uses the NFL QB1 median fallback). R/21 is sourced lazily so
#' the CFB build only runs when a rookie QB1 actually exists.
#'
#' @param rookie_names Character vector of no-history QB1 player names.
#' @param nfl_qb1_quantiles Named numeric from quantile(., c(.25,.5,.75)) of the
#'   NFL QB1 pooled rush rates.
#' @param seasons Integer vector of college seasons to pool.
#' @param min_games College QB starter threshold for the reference distribution.
#' @return Tibble: name_key, rookie_rate, rookie_source. Empty on any failure.
#' @keywords internal
.compute_rookie_qb_rush_rate <- function(rookie_names,
                                         nfl_qb1_quantiles,
                                         seasons   = QB_RUSH_RATE_SEASONS,
                                         min_games = CFB_QB_REF_MIN_GAMES) {
  empty <- tibble::tibble(name_key = character(),
                          rookie_rate = numeric(),
                          rookie_source = character())
  if (length(rookie_names) == 0L) return(empty)

  cfb <- tryCatch({
    if (!exists("build_cfb_player_season_panel", mode = "function")) {
      source(here::here("R", "21_cfb_player_season_panel.R"))
    }
    build_cfb_player_season_panel(seasons = seasons)
  }, error = function(e) {
    message(glue("    Rookie QB rush: CFB panel unavailable ({e$message}) -- ",
                 "median fallback"))
    NULL
  })
  if (is.null(cfb) || nrow(cfb) == 0L) return(empty)
  needed <- c("player_name", "position_group", "rush_attempts", "games_played")
  if (length(setdiff(needed, names(cfb))) > 0L) {
    message("    Rookie QB rush: CFB panel missing columns -- median fallback")
    return(empty)
  }

  cfb_qb <- cfb %>%
    dplyr::filter(grepl("QB", toupper(.data$position_group)),
                  !is.na(.data$games_played), .data$games_played > 0) %>%
    dplyr::mutate(name_key = .qb_name_key(.data$player_name),
                  cfb_rush_pg = .data$rush_attempts / .data$games_played)

  # College QB starter reference distribution -> tier cutoffs.
  ref <- cfb_qb %>% dplyr::filter(.data$games_played >= min_games)
  if (nrow(ref) < 20L) {
    message("    Rookie QB rush: thin college reference -- median fallback")
    return(empty)
  }
  col_q   <- stats::quantile(ref$cfb_rush_pg, c(0.50, 0.75), na.rm = TRUE)
  col_med <- col_q[[1]]
  col_p75 <- col_q[[2]]

  q25 <- nfl_qb1_quantiles[["25%"]]
  q50 <- nfl_qb1_quantiles[["50%"]]
  q75 <- nfl_qb1_quantiles[["75%"]]

  rk_keys <- unique(.qb_name_key(rookie_names))

  cfb_qb %>%
    dplyr::filter(.data$name_key %in% rk_keys) %>%
    dplyr::group_by(.data$name_key) %>%
    dplyr::summarise(
      cfb_rush_pg = sum(.data$rush_attempts, na.rm = TRUE) /
                    sum(.data$games_played, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      rookie_source = dplyr::case_when(
        .data$cfb_rush_pg >= col_p75 ~ "college_mobile",
        .data$cfb_rush_pg >= col_med ~ "college_balanced",
        TRUE                         ~ "college_pocket"
      ),
      rookie_rate = dplyr::case_when(
        .data$rookie_source == "college_mobile"   ~ q75,
        .data$rookie_source == "college_balanced" ~ q50,
        TRUE                                      ~ q25
      )
    ) %>%
    dplyr::select(name_key, rookie_rate, rookie_source)
}

# ------------------------------------------------------------------------------
# .compute_qb_rush_rate
# ------------------------------------------------------------------------------

#' Pooled game-weighted per-QB rush rate (v3_4)
#'
#' QB rushing is a player trait, not a share of the team's carry pool, so it is
#' modeled as the QB's own carries per game and transfers across team moves.
#' Reads the R/16 panel directly (its rush_attempts is verified to include
#' scrambles at 97-100% of true pbp QB rush volume, kneels already excluded) and
#' pools OPPORTUNITY-WEIGHTED over `seasons`: sum(rush_attempts)/sum(games),
#' NOT a mean of per-season rates, so injury-shortened seasons contribute in
#' proportion to real exposure. Same weighting principle as the R/32 efficiency
#' table.
#'
#' On any failure returns an empty tibble; the caller then falls the affected
#' QB1s back to a provisional NFL QB1 median rate.
#'
#' @param panel_path Path to the R/16 player-season panel RDS.
#' @param seasons Integer vector of seasons to pool (default QB_RUSH_RATE_SEASONS).
#' @return Tibble: nfl_gsis_id, qb_rush_pooled_pg, qb_rush_seasons_pooled.
#' @keywords internal
.compute_qb_rush_rate <- function(panel_path = PANEL_CACHE_PATH_ALLOC,
                                  seasons    = QB_RUSH_RATE_SEASONS) {
  empty <- tibble::tibble(
    nfl_gsis_id            = character(),
    qb_rush_pooled_pg      = numeric(),
    qb_rush_seasons_pooled = integer()
  )

  panel <- tryCatch(readRDS(panel_path), error = function(e) NULL)
  if (is.null(panel) || nrow(panel) == 0L) {
    message("    QB rush rate: panel unavailable -- QB carries use median fallback")
    return(empty)
  }
  needed <- c("player_id", "season", "position", "rush_attempts", "games_played")
  if (length(setdiff(needed, names(panel))) > 0L) {
    message("    QB rush rate: panel missing required columns -- median fallback")
    return(empty)
  }

  rate <- panel %>%
    dplyr::filter(as.integer(.data$season) %in% seasons,
                  .data$position == "QB",
                  !is.na(.data$games_played), .data$games_played > 0) %>%
    dplyr::group_by(nfl_gsis_id = as.character(.data$player_id)) %>%
    dplyr::summarise(
      qb_rush_seasons_pooled = dplyr::n(),
      tot_att = sum(.data$rush_attempts, na.rm = TRUE),
      tot_g   = sum(.data$games_played,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$tot_g > 0) %>%
    dplyr::mutate(qb_rush_pooled_pg = .data$tot_att / .data$tot_g) %>%
    dplyr::select(nfl_gsis_id, qb_rush_pooled_pg, qb_rush_seasons_pooled)

  message(glue(
    "    QB rush rate: pooled {nrow(rate)} QBs over ",
    "{min(seasons)}-{max(seasons)} (game-weighted, scrambles included)"
  ))
  rate
}

# ------------------------------------------------------------------------------
# .apply_team_volume
# ------------------------------------------------------------------------------

#' Multiply per-player shares by R/30 team volumes to get expected PPG inputs
#'
#' Targets: expected_targets_pg = target_share * pass_pg (unchanged).
#'
#' Rushing (v3_4): QBs no longer route through team-carry share. Each QB1 is
#' assigned its pooled per-QB rush rate (or a provisional NFL QB1 median for
#' no-history QB1s); QB2/QB3 get 0. The QB1 rate is SUBTRACTED from the team
#' rush pool first, and the RBs (and any non-QB rusher) split the residual by
#' their existing rush_share renormalized among themselves. This keeps the RB
#' soft constraint untouched and makes the QB carries a true player trait rather
#' than a self-deflating share. The now-vestigial QB rush-share and carryover
#' audit fields are blanked to NA for QB rows.
#'
#' @param alloc_table Tibble with target_share and rush_share columns.
#' @param team_volumes Tibble from R/30 project_team_volumes().
#' @param qb_rush_rates Tibble from .compute_qb_rush_rate() (or NULL).
#' @param rookie_qb_rates Tibble from .compute_rookie_qb_rush_rate() keyed by
#'   name_key (or NULL): the college-tier fallback for no-history QB1s.
#' @return Same tibble with expected_targets_pg, expected_carries_pg, and the
#'   qb_rush_pooled_pg / qb_rush_seasons_pooled / qb_rush_rate_source columns.
#' @keywords internal
.apply_team_volume <- function(alloc_table, team_volumes,
                               qb_rush_rates = NULL, rookie_qb_rates = NULL) {

  if (is.null(qb_rush_rates)) {
    qb_rush_rates <- tibble::tibble(
      nfl_gsis_id            = character(),
      qb_rush_pooled_pg      = numeric(),
      qb_rush_seasons_pooled = integer()
    )
  }
  if (is.null(rookie_qb_rates)) {
    rookie_qb_rates <- tibble::tibble(
      name_key      = character(),
      rookie_rate   = numeric(),
      rookie_source = character()
    )
  }

  joined <- alloc_table %>%
    dplyr::left_join(
      team_volumes %>%
        dplyr::select(team, projected_pass_pg, projected_rush_pg) %>%
        dplyr::rename(
          projected_team_pass_pg = projected_pass_pg,
          projected_team_rush_pg = projected_rush_pg
        ),
      by = "team"
    ) %>%
    dplyr::left_join(qb_rush_rates, by = "nfl_gsis_id") %>%
    dplyr::mutate(name_key = dplyr::if_else(
      .data$position == "QB", .qb_name_key(.data$player_name), NA_character_)) %>%
    dplyr::left_join(rookie_qb_rates, by = "name_key")

  # Last-resort fallback for a no-history QB1 with no college match either:
  # median pooled rate among the QB1s that DO have NFL history.
  qb1_hist <- joined %>%
    dplyr::filter(.data$position == "QB",
                  .data$depth_position == "QB1",
                  !is.na(.data$qb_rush_pooled_pg)) %>%
    dplyr::pull(.data$qb_rush_pooled_pg)
  fallback_rate <- if (length(qb1_hist) > 0L) stats::median(qb1_hist) else 0

  joined <- joined %>%
    dplyr::mutate(
      # Priority: NFL history pool > college-tier rookie fallback > median.
      qb_rush_rate_source = dplyr::case_when(
        .data$position != "QB"          ~ NA_character_,
        .data$depth_position != "QB1"   ~ "backup_zero",
        !is.na(.data$qb_rush_pooled_pg) ~ "history_pool",
        !is.na(.data$rookie_source)     ~ .data$rookie_source,
        TRUE                            ~ "prior_median_fallback"
      ),
      qb_carries_tmp = dplyr::case_when(
        .data$position == "QB" & .data$depth_position == "QB1" ~
          dplyr::coalesce(.data$qb_rush_pooled_pg, .data$rookie_rate,
                          fallback_rate),
        .data$position == "QB"                                 ~ 0,
        TRUE                                                   ~ NA_real_
      )
    )

  # Per-team: QB1 carries to subtract, and non-QB rush-share sum to renormalize
  # (computed on the pre-blank shares).
  team_adj <- joined %>%
    dplyr::group_by(.data$team) %>%
    dplyr::summarise(
      team_qb1_carries = sum(
        .data$qb_carries_tmp[.data$position == "QB" &
                               .data$depth_position == "QB1"],
        na.rm = TRUE),
      team_nonqb_rush_share = sum(
        .data$rush_share[.data$position != "QB"], na.rm = TRUE),
      .groups = "drop"
    )

  joined %>%
    dplyr::left_join(team_adj, by = "team") %>%
    dplyr::mutate(
      rush_residual_tmp = pmax(
        dplyr::coalesce(.data$projected_team_rush_pg, 0) -
          dplyr::coalesce(.data$team_qb1_carries, 0), 0),
      expected_targets_pg = .data$target_share *
                              dplyr::coalesce(.data$projected_team_pass_pg, 0),
      expected_carries_pg = dplyr::case_when(
        .data$position == "QB" & .data$depth_position == "QB1" ~
          .data$qb_carries_tmp,
        .data$position == "QB" ~ 0,
        .data$team_nonqb_rush_share > 0 ~
          (.data$rush_share / .data$team_nonqb_rush_share) *
            .data$rush_residual_tmp,
        TRUE ~ 0
      ),
      qb_rush_pooled_pg = dplyr::if_else(
        .data$position == "QB", .data$qb_rush_pooled_pg, NA_real_),
      qb_rush_seasons_pooled = dplyr::if_else(
        .data$position == "QB", .data$qb_rush_seasons_pooled, NA_integer_),
      # Blank vestigial QB rush-share + carryover audit fields (they no longer
      # feed QB output; keeping them would misleadingly imply a share basis).
      rush_share_base = dplyr::if_else(
        .data$position == "QB", NA_real_, .data$rush_share_base),
      rush_share_adjusted = dplyr::if_else(
        .data$position == "QB", NA_real_, .data$rush_share_adjusted),
      rush_share = dplyr::if_else(
        .data$position == "QB", NA_real_, .data$rush_share),
      carryover_applied = dplyr::if_else(
        .data$position == "QB", FALSE, .data$carryover_applied),
      carryover_prior_share = dplyr::if_else(
        .data$position == "QB", NA_real_, .data$carryover_prior_share),
      carryover_weight = dplyr::if_else(
        .data$position == "QB", NA_real_, .data$carryover_weight)
    ) %>%
    dplyr::select(-qb_carries_tmp, -rush_residual_tmp,
                  -team_qb1_carries, -team_nonqb_rush_share,
                  -name_key, -rookie_rate, -rookie_source)
}

# ==============================================================================
# PUBLIC ENTRY POINT
# ==============================================================================

# ------------------------------------------------------------------------------
# .correct_rookie_depth_ranks
# ------------------------------------------------------------------------------

#' Replace 999-defaulted depth ranks for 2026 draftees using draft capital
#'
#' Sleeper's pre-season depth_chart_order for recently drafted rookies is
#' frequently NULL -- the depth chart hasn't been set yet. When the Sleeper
#' merge coalesces NULL to 999, those players sort to the bottom of their
#' position group after re-ranking and receive near-zero target/carry share.
#' For high-dynasty-score rookies (Carnell Tate, Jordyn Tyson, Kenyon Sadiq,
#' Jeremiyah Love, etc.) this produces wildly incorrect allocations.
#'
#' This function detects players where depth_was_missing is TRUE (Sleeper
#' had no depth_chart_order), cross-references the 2026 draft pick data, and
#' replaces their depth_rank_raw with a round-based default:
#'   Round 1 -> 1  (day-1 starter / immediate contributor)
#'   Round 2 -> 2
#'   Round 3 -> 3
#'   Round 4+ -> 4
#'
#' After patching, all team + position groups are re-ranked so adjacent
#' incumbents shift accordingly.
#'
#' @param depth_chart Tibble from .load_2026_depth_charts(), must include
#'   the depth_was_missing column added by .merge_sleeper_depth_overrides().
#' @param rookies Tibble from .load_rookie_draft_capital().
#' @return depth_chart with corrected depth_rank_raw and depth_was_missing
#'   column removed.
#' @keywords internal
.correct_rookie_depth_ranks <- function(depth_chart, rookies) {

  eligible <- depth_chart %>%
    dplyr::filter(dplyr::coalesce(.data$depth_was_missing, FALSE)) %>%
    dplyr::inner_join(
      rookies %>%
        dplyr::filter(!is.na(.data$draft_round)) %>%
        dplyr::select(nfl_gsis_id, draft_round),
      by = "nfl_gsis_id"
    )

  if (nrow(eligible) == 0L) {
    message("  Rookie depth rank correction: no eligible players (all have Sleeper depth data)")
    return(depth_chart %>% dplyr::select(-depth_was_missing))
  }

  rank_overrides <- eligible %>%
    dplyr::mutate(
      new_rank = dplyr::case_when(
        .data$draft_round == 1L ~ 1L,
        .data$draft_round == 2L ~ 2L,
        .data$draft_round == 3L ~ 3L,
        .data$draft_round >= 4L ~ 4L,
        TRUE                    ~ 4L
      )
    ) %>%
    dplyr::select(nfl_gsis_id, new_rank)

  patched <- depth_chart %>%
    dplyr::left_join(rank_overrides, by = "nfl_gsis_id") %>%
    dplyr::mutate(
      depth_rank_raw = dplyr::if_else(
        !is.na(.data$new_rank),
        .data$new_rank,
        .data$depth_rank_raw
      )
    ) %>%
    dplyr::select(-new_rank, -depth_was_missing) %>%
    dplyr::group_by(.data$team, .data$position) %>%
    dplyr::arrange(.data$depth_rank_raw, .data$player_name, .by_group = TRUE) %>%
    dplyr::mutate(depth_rank_raw = dplyr::row_number()) %>%
    dplyr::ungroup()

  message(glue(
    "  Rookie depth rank correction: ",
    "{nrow(eligible)} players corrected using draft capital ",
    "(R1->rank 1, R2->rank 2, R3->rank 3, R4+->rank 4)"
  ))

  patched
}



#' Allocate 2026 team volumes to individual players
#'
#' Top-level orchestrator. Pulls depth charts, draft picks, R/28 dynasty
#' scores, and R/30 team volumes. Computes depth-position-based priors,
#' adjusts for rookie draft capital and player talent, enforces soft
#' per-team constraints, and multiplies shares by team volumes to produce
#' per-player expected targets and carries per game.
#'
#' Output is the foundation for R/32 reconciliation, which will use these
#' expected volumes to correct the player projections in R/29 priors.
#'
#' @param team_volumes_path Character. Path to R/30 output RDS.
#' @param dynasty_scores_path Character. Path to R/28 output CSV.
#' @param season Integer. Target season (default SEASON_ALLOC).
#' @param as_of_week Integer 1-18 or NULL. When NULL (default), allocation is
#'   fully preseason. When supplied (and >= MIN_WEEKS_OBSERVED_ALLOC), each
#'   player's preseason share is blended toward their observed current-season
#'   share via compute_prior_weight(as_of_week). Requires
#'   current_season_volume to have an effect.
#' @param current_season_volume Tibble or NULL. Caller-supplied observed
#'   shares with columns nfl_gsis_id, observed_target_share,
#'   observed_rush_share (player share of team targets / carries to date).
#'   Only consulted when as_of_week is non-NULL. Same caller-supplies-data
#'   contract as R/30's current_season_sos.
#' @param save_output Logical. Write RDS + CSV outputs.
#' @return Tibble with one row per active 2026 offensive player.
#'
#' @seealso project_team_volumes (R/30), compute_prior_weight (R/29)
#' @export
allocate_player_volumes <- function(team_volumes_path  = TEAM_VOLUMES_RDS,
                                      dynasty_scores_path = DYNASTY_SCORES_CSV,
                                      season             = SEASON_ALLOC,
                                      as_of_week         = NULL,
                                      current_season_volume = NULL,
                                      preseason_carryover = TRUE,
                                      save_output        = TRUE) {

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/31: Allocating player volumes for season {season}"))
  message(glue("Soft constraint bounds: [{CONSTRAINT_LOWER}, ",
               "{CONSTRAINT_UPPER}]"))
  if (is.null(as_of_week)) {
    message(glue("Allocation mode: preseason (depth chart + capital + talent",
                 "{if (preseason_carryover) ' + prior-season share carryover' else ''})"))
  } else {
    message(glue("Allocation mode: in-season at week {as_of_week} ",
                 "(preseason shares taper toward observed)"))
  }
  message(glue("{strrep('=', 70)}"))

  # STEP 1: Load R/30 team volumes
  message("\nSTEP 1/7: Loading R/30 team volumes")
  if (!file.exists(team_volumes_path)) {
    stop(glue("R/30 output not found at {team_volumes_path}. ",
              "Run project_team_volumes() first."))
  }
  team_volumes <- readRDS(team_volumes_path)
  message(glue("  Loaded {nrow(team_volumes)} team volume projections"))

  # Committee teams carry >1 R/30 row (one per candidate QB). R/31 uses only
  # team-level pass/rush volume here, near-identical across candidates, so
  # collapse to one representative row per team (highest QB quality). Prevents
  # fanning every player on a committee team. Non-committee teams unaffected.
  if ("committee_group" %in% names(team_volumes)) {
    .qcol <- if ("qb_quality_score" %in% names(team_volumes))
      team_volumes$qb_quality_score else rep(0, nrow(team_volumes))
    team_volumes <- team_volumes[order(team_volumes$team, -.qcol), , drop = FALSE]
    team_volumes <- team_volumes[!duplicated(team_volumes$team), , drop = FALSE]
    message(glue("  Committee collapse: {nrow(team_volumes)} team rows after ",
                 "one-per-team"))
  }

  # STEP 2: Load 2026 depth charts
  message("\nSTEP 2/7: Loading depth charts")
  depth_chart <- .load_2026_depth_charts(season = season)

  # STEP 3: Load rookie draft capital
  message("\nSTEP 3/7: Loading rookie draft capital")
  rookies <- .load_rookie_draft_capital(season = season)

  # STEP 3.5: Correct depth ranks for 2026 draftees missing Sleeper depth data
  # Must run AFTER both depth chart and rookie capital are loaded. Players whose
  # Sleeper depth_chart_order was NULL (flagged as depth_was_missing) get their
  # depth_rank_raw replaced with a draft-round-based default before allocation
  # shares are computed.
  message("\nSTEP 3.5/7: Correcting rookie depth ranks via draft capital")
  depth_chart <- .correct_rookie_depth_ranks(depth_chart, rookies)

  # STEP 4: Load talent inputs and compute talent multipliers.
  # Prospects keep the R/28 college score; veterans (years_exp >=
  # VETERAN_MIN_EXP) receive an NFL-efficiency talent_z from the R/16 panel.
  message("\nSTEP 4/7: Loading talent inputs and computing talent multipliers")
  scores     <- .load_dynasty_scores(path = dynasty_scores_path)
  panel      <- .load_player_season_panel()
  roster_exp <- .load_roster_experience(season = season)
  veteran_z  <- .compute_veteran_talent_z(panel, roster_exp, season = season)
  talent     <- .compute_talent_multipliers(scores, veteran_z = veteran_z)

  # STEP 5: Build the per-player allocation table
  message("\nSTEP 5/7: Assembling per-player allocation inputs")
  alloc_table <- .build_player_allocation_table(
    depth_chart = depth_chart,
    rookies     = rookies,
    talent      = talent
  )

  # STEP 6: Compute initial shares, blend with observed (in-season), constrain
  message("\nSTEP 6/7: Computing shares and enforcing soft constraints")
  alloc_with_shares <- .compute_initial_shares(alloc_table)

  # STEP 6.25: Preseason share carryover (v3_3). For returning, non-mover,
  # above-floor players, blend the primary share toward last season's realized
  # share at the fitted per-position weight. Runs BEFORE the in-season blend so
  # observed data tapers from the carryover baseline. No-op when disabled or
  # when prior-season shares are unavailable.
  prior_shares <- if (preseason_carryover) .compute_prior_season_shares() else NULL
  alloc_with_shares <- .apply_preseason_carryover(
    alloc_table  = alloc_with_shares,
    prior_shares = prior_shares,
    enabled      = preseason_carryover
  )

  # STEP 6.5: In-season blend (no-op in preseason mode). Runs BEFORE the
  # constraint so the constraint operates once on the blended shares.
  alloc_blended <- .blend_observed_shares(
    alloc_table           = alloc_with_shares,
    current_season_volume = current_season_volume,
    as_of_week            = as_of_week
  )

  alloc_constrained <- .enforce_soft_constraints(alloc_blended)

  # STEP 7: Apply team volume to get expected volumes per player
  message("\nSTEP 7/7: Applying team volumes to get expected per-player volumes")
  # v3_4: pooled per-QB rush trait rate (player trait, transfers across moves).
  qb_rush_rates <- .compute_qb_rush_rate()

  # v3_5: college-tier fallback for no-history (rookie) QB1s. The CFB panel is
  # built (via lazy-sourced R/21) only when a QB1 actually lacks NFL rush history.
  qb1_now       <- dplyr::filter(alloc_constrained,
                                 .data$position == "QB",
                                 .data$depth_position == "QB1")
  qb1_hist_rate <- dplyr::semi_join(qb_rush_rates, qb1_now, by = "nfl_gsis_id")
  no_hist_qb1   <- dplyr::anti_join(qb1_now, qb_rush_rates, by = "nfl_gsis_id")
  rookie_qb_rates <- NULL
  if (nrow(no_hist_qb1) > 0L && nrow(qb1_hist_rate) > 0L) {
    nfl_qb1_q <- stats::quantile(qb1_hist_rate$qb_rush_pooled_pg,
                                 c(0.25, 0.50, 0.75), na.rm = TRUE)
    rookie_qb_rates <- .compute_rookie_qb_rush_rate(
      rookie_names      = no_hist_qb1$player_name,
      nfl_qb1_quantiles = nfl_qb1_q
    )
    n_tiered <- if (!is.null(rookie_qb_rates)) nrow(rookie_qb_rates) else 0L
    message(glue(
      "    Rookie QB1 fallback (F2): {nrow(no_hist_qb1)} no-history QB1(s), ",
      "{n_tiered} tiered via college, rest use NFL QB1 median"
    ))
  }

  alloc_final <- .apply_team_volume(
    alloc_table     = alloc_constrained,
    team_volumes    = team_volumes,
    qb_rush_rates   = qb_rush_rates,
    rookie_qb_rates = rookie_qb_rates
  )

  # Final output schema
  output <- alloc_final %>%
    dplyr::mutate(schema_tag = SCHEMA_TAG_ALLOC) %>%
    dplyr::select(
      nfl_gsis_id, player_name, team, position, depth_position, depth_rank,
      is_rookie, draft_round, rookie_capital_mult,
      score_final, talent_z, talent_multiplier, talent_source,
      window_volume, n_window_seasons,
      target_share_base, target_share_adjusted, target_share,
      rush_share_base, rush_share_adjusted, rush_share,
      observed_target_share, observed_rush_share, share_blend_weight,
      carryover_applied, carryover_prior_share, carryover_weight,
      qb_rush_pooled_pg, qb_rush_seasons_pooled, qb_rush_rate_source,
      projected_team_pass_pg, projected_team_rush_pg,
      expected_targets_pg, expected_carries_pg,
      schema_tag
    ) %>%
    dplyr::arrange(team, position, depth_rank)

  # Committee QB rooms (own-rate): tag each listed candidate and give the
  # non-QB1 candidate his own pooled rush rate, so R/32 anchors both as full
  # starters. RB allocation above is unchanged (it used the depth-QB1 subtract).
  output <- output %>%
    dplyr::left_join(.load_qb_committees_alloc(season = season),
                     by = c("team", "nfl_gsis_id" = "qb_id")) %>%
    dplyr::mutate(
      expected_carries_pg = dplyr::if_else(
        !is.na(.data$committee_group) & .data$position == "QB",
        dplyr::coalesce(.data$qb_rush_pooled_pg, .data$expected_carries_pg),
        .data$expected_carries_pg
      )
    )

  # Save outputs
  if (save_output) {
    dir.create(dirname(OUTPUT_RDS_PATH_ALLOC), recursive = TRUE,
               showWarnings = FALSE)
    saveRDS(output, OUTPUT_RDS_PATH_ALLOC)
    readr::write_csv(output, OUTPUT_CSV_PATH_ALLOC)
    message(glue("\n  Saved: {OUTPUT_RDS_PATH_ALLOC}"))
    message(glue("  Saved: {OUTPUT_CSV_PATH_ALLOC}"))
  }

  # KEY INSIGHTS (computed from data, never hardcoded)
  n_players <- nrow(output)
  n_rookies <- sum(output$is_rookie, na.rm = TRUE)
  n_with_scores <- sum(!is.na(output$score_final), na.rm = TRUE)
  n_teams <- dplyr::n_distinct(output$team)

  # Talent-source split (computed from data, never hardcoded)
  n_veteran_nfl <- sum(output$talent_source == "veteran_nfl", na.rm = TRUE)
  n_prospect    <- sum(output$talent_source == "prospect_college",
                       na.rm = TRUE)
  n_neutral     <- sum(output$talent_source == "neutral", na.rm = TRUE)

  # Soft constraint diagnostics: how many teams were rescaled?
  team_check <- output %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      target_sum_raw = sum(target_share_adjusted, na.rm = TRUE),
      rush_sum_raw   = sum(rush_share_adjusted, na.rm = TRUE),
      .groups        = "drop"
    )
  n_target_rescaled <- sum(
    team_check$target_sum_raw < CONSTRAINT_LOWER |
      team_check$target_sum_raw > CONSTRAINT_UPPER,
    na.rm = TRUE
  )
  n_rush_rescaled <- sum(
    team_check$rush_sum_raw < CONSTRAINT_LOWER |
      team_check$rush_sum_raw > CONSTRAINT_UPPER,
    na.rm = TRUE
  )

  # Top expected targets and carries
  top_target_player <- output$player_name[which.max(output$expected_targets_pg)]
  top_target_team   <- output$team[which.max(output$expected_targets_pg)]
  top_target_pg     <- max(output$expected_targets_pg, na.rm = TRUE)
  top_carry_player  <- output$player_name[which.max(output$expected_carries_pg)]
  top_carry_team    <- output$team[which.max(output$expected_carries_pg)]
  top_carry_pg      <- max(output$expected_carries_pg, na.rm = TRUE)

  message(glue("\n{strrep('=', 70)}"))
  message("KEY INSIGHTS")
  message(glue("{strrep('=', 70)}"))
  message(glue("  Players allocated:        {n_players}"))
  message(glue("  Active teams covered:     {n_teams}"))
  message(glue("  Rookies in pool:          {n_rookies}"))
  message(glue("  Players with R/28 score:  {n_with_scores}"))
  message(glue("  Talent source - veteran:  {n_veteran_nfl} (NFL efficiency)"))
  message(glue("  Talent source - prospect: {n_prospect} (R/28 college)"))
  message(glue("  Talent source - neutral:  {n_neutral} (no signal)"))
  message(glue("  Teams rescaled (targets): {n_target_rescaled} of {n_teams}"))
  message(glue("  Teams rescaled (rushes):  {n_rush_rescaled} of {n_teams}"))
  n_carryover <- sum(output$carryover_applied, na.rm = TRUE)
  message(glue("  Preseason carryover:      {n_carryover} players ",
               "(returning, non-mover, above floor)"))
  if (!is.null(as_of_week)) {
    n_blended <- sum(!is.na(output$observed_target_share) |
                       !is.na(output$observed_rush_share), na.rm = TRUE)
    blend_w <- unique(stats::na.omit(output$share_blend_weight))
    blend_w_label <- if (length(blend_w) == 1L) {
      format(round(blend_w, 3), nsmall = 3)
    } else {
      "n/a (blend not applied)"
    }
    message(glue("  In-season blend:          week {as_of_week}, ",
                 "preseason weight {blend_w_label}, ",
                 "{n_blended} players blended"))
  }
  message(glue("  Highest expected targets: {top_target_player} ",
               "({top_target_team}) at ",
               "{format(round(top_target_pg, 1), nsmall = 1)} per game"))
  message(glue("  Highest expected carries: {top_carry_player} ",
               "({top_carry_team}) at ",
               "{format(round(top_carry_pg, 1), nsmall = 1)} per game"))
  message(glue("{strrep('=', 70)}\n"))

  output
}
