# ==============================================================================
# NFL Analytics Toolkit -- Season 3
# Start/Sit Uncertainty Quantification
# File: R/36_start_sit_uq.R
#
# PURPOSE
# -------
# A thin uncertainty layer on top of R/35. R/35 returns the best legal starting
# lineup and a binary confidence_flag (intervals overlap or they do not). R/36
# answers the questions that flag cannot:
#
#   1. How likely is each start to actually be correct?  (p_start_correct)
#   2. When a call goes wrong, how much does it typically cost? (avg_miss)
#   3. How much expected value is at risk across the whole lineup?
#      (week_risk_score)
#
# It does NOT re-solve the lineup. It enriches the lineup R/35 already produced.
# R/35 is consumed, never modified.
#
# THE MODEL (all analytical, no simulation)
# -----------------------------------------
#   - Each player's weekly score is treated as an independent Normal. The mean
#     is the matchup-adjusted projection (adj_proj).
#   - Sigma is r32_sigma_week: the predictive SD for ONE future week. R/32
#     builds it as sqrt(r32_posterior_sigma^2 + r32_omega^2) -- tau, the
#     uncertainty in the player's mean, plus omega, the week-to-week
#     dispersion, added in quadrature.
#
#     THIS IS THE WHOLE BALLGAME FOR THIS FILE. A start/sit call is a question
#     about one week, so the denominator must be the one-week SD. Using the
#     season-long posterior sigma instead is not conservative, it is wrong by a
#     material factor. On the live managed-league reconciled: median tau
#     (r32_posterior_sigma) 3.33, median omega (r32_omega) 4.75, median
#     r32_sigma_week 5.84. Omega carries roughly two thirds of weekly variance
#     there, and substituting tau alone shrinks the denominator by about 1.75x,
#     which is enough to turn coin flips into confident starts.
#
#     The split is board-dependent and the difference is worth knowing. R/32's
#     .build_omega() docstring cites tau near 1.1 against omega near 5.7, where
#     omega is roughly 98 percent of the variance and tau alone runs at about a
#     fifth of the correct SD. That describes the DK BEST-BALL board, not this
#     one: best-ball projections carry far less uncertainty in the mean. Do not
#     carry that 98 percent figure over to managed-league output.
#
#     Preference order (recorded per row in sigma_source, so every number is
#     auditable):
#         1. r32_sigma_week                          -> "r32_sigma_week"
#         2. sqrt(r32_posterior_sigma^2 + r32_omega^2) recomputed, if the
#            column is absent but its parts are  -> "recomputed_quadrature"
#         3. board-wide position median           -> "position_median"
#         4. nothing usable                      -> "missing" (sigma NA)
#     2 through 4 exist for older reconciled files; on a current one, every row
#     resolves at step 1. NOTE: r32_sigma_week is NOT scaled by
#     qb_discount_factor. The discount acts on the mean upstream; the weekly SD
#     is built from tau and omega directly, so scaling it here would double-count.
#   - Sigma is NOT scaled by matchup_factor, and this is verified rather than
#     assumed: R/32 contains no reference to matchup, opponent, DvP, or
#     schedule anywhere in the file, so r32_sigma_week is matchup-independent
#     by construction. The matchup moves the mean (adj_proj) only. Note also
#     that as of the 2026-09-09 neutralization in R/35, DVP_CAP_OFFENSE is
#     0.00, so matchup_factor is exactly 1.0 for every offensive player.
#   - For starter S and best alternative A, both Normal and independent:
#         p_start_correct = pnorm( (mu_s - mu_a) / sqrt(sigma_s^2 + sigma_a^2) )
#         expected_regret = E[max(A - S, 0)]            (probability-weighted)
#         avg_miss        = expected_regret / (1 - p_start_correct)  (conditional)
#     expected_regret folds in both the odds and the magnitude, so it is the
#     quantity summed into week_risk_score. avg_miss is the human-readable
#     "if this goes wrong, you lose about X points."
#
# THREE DOCUMENTED ASSUMPTIONS (none is a bug; all are real limitations)
# -----------------------------------------------------------------------
# INDEPENDENCE. Player scores are treated as independent. Same-game correlation
# (a QB and his WR move together) is real and is NOT modeled here. R/50 contains
# correlation machinery (draw_correlated_z, draw_copula_z) that could close this;
# not wired up.
#
# SYMMETRY. Both marginals are Normal, which is what makes the closed forms
# above valid. Weekly fantasy scoring is right-skewed: the R/50 shape table
# (s3_weekly_shape_table.rds, built on 2023-2024) reports within-player
# standardized skew of 1.39 at RB and WR, 1.35 at TE, 0.451 at QB. Because the
# upside tail is fatter than a Normal, this file's p_start_correct UNDERSTATES
# the chance a bench player outscores the starter, and expected_regret
# UNDERSTATES the loss when that happens. Both errors run the same direction:
# the tool reads as more confident than it should. The `shape` argument is a
# placeholder for wiring in R/50's predictive_draw(); it is accepted and
# validated but not yet used.
#
# AVAILABILITY. Inherited wholesale from R/32 and worth restating here, because
# this is the file that produces a number acted on every week. omega_raw is
# computed only over games a player actually PLAYED: missed weeks are absent
# rather than scored as zero. That overstates weekly reliability for
# injury-prone players, so sigma_s is too small and p_start_correct too
# confident for exactly the players whose weeks are most in doubt. R/48 is the
# intended input. Until it lands, this bias is UNMEASURED and must not be
# described as handled.
#
# DEF / ST (the streaming case)
# -----------------------------
# R/34 produces a DEF projected PPG but no interval, and R/35's
# suggest_waiver_adds() excludes DEF from waiver candidates entirely, so nothing
# upstream answers "drop my DEF for one on waivers?". R/36 fills that gap:
#   - A league-wide WITHIN-TEAM weekly DEF scoring sigma is computed empirically
#     from calculate_def_st_points() (R/29) over def_seasons. Residuals are
#     demeaned by team-season before pooling, because the projection already
#     carries the team's mean level: the spread that matters for a weekly call
#     is how much a given defense varies around its own average, not how much
#     defenses differ from each other. Pooling raw team-weeks (the v1 behaviour)
#     mixed both and overstated the spread, making every DEF comparison read as
#     more of a tossup than it is.
#   - That sigma puts every DEF on the same Normal footing as offense, so the
#     same probability and regret formulas apply.
#   - For the DEF slot, the alternative set includes AVAILABLE (waiver) defenses,
#     not just benched ones: P(waiver DEF outscores my DEF) and the expected
#     miss, directly.
#   - When the DEF slot is OPEN (no DEF in the optimal lineup, the streamer's
#     usual state), there is no started DEF to compare against, so R/36 instead
#     returns a ranked streaming table (lineup$def_streaming): the top available
#     defenses by matchup-adjusted projection, each with P(it beats the
#     next-ranked option) and a clarity label (clear / slight edge / tossup).
#     Clarity reads off that probability, not off regret: DEF variance is large
#     enough that the absolute point swing between near-equal defenses is big
#     even when the choice barely matters.
#   - DEF scoring settings are a parameter (def_scoring_settings), passed
#     straight to calculate_def_st_points(). The default is R/29's
#     DEFAULT_SCORING_SETTINGS (Sleeper standard). A league with custom DEF
#     scoring should pass its own; the sigma cache keys on the settings, so
#     changing them invalidates the cache automatically.
#   - The sigma is cached to data/season3_cache/s3_r36_def_sigma.rds. The v1
#     behaviour reloaded a full pbp season on every call.
#
# BYE WEEKS (v2 guard)
# --------------------
# R/35's .apply_matchup() joins opponent by team. A team on bye has no row in
# the week's schedule, so opponent comes back NA, matchup_factor defaults to 1
# and adj_proj equals base_proj -- a player who is not playing carries a full
# projection. R/36 refuses to score that: when `week` is supplied, any starter
# with a missing opponent is a hard stop (override with allow_bye_starters),
# and any alternative with a missing opponent is dropped from the pool. The
# real fix belongs in R/35 and is a separate delivery; this is the guard so a
# bye cannot silently inflate a start/sit call in the meantime.
#
# OUTPUTS (only when save_output = TRUE)
# --------------------------------------
#   data/season3_cache/s3_r36_{league}_w{NN}_start_sit_uq.rds / .csv
#   data/season3_cache/s3_r36_{league}_w{NN}_def_streaming.rds / .csv
# The week is in the filename so weekly runs accumulate instead of overwriting
# each other. That archive is the input to the calibration backtest: there are
# no historical weekly projection snapshots on disk, so a genuine
# predicted-vs-realized calibration set can only be built going forward.
#
# v3 adds the league segment. Running more than one league in the same week
# under the v2 naming silently overwrote the previous league's snapshot, so the
# archive would have held one league per week rather than all of them. `league`
# is the sanitized `league_id` argument; when league_id is NULL it is the
# literal "nolg", which preserves v2 behavior for single-league runs but is NOT
# safe to use across leagues.
#
# SOURCE DEPENDENCIES (sourced below if not already loaded)
# ---------------------------------------------------------
#   R/35_lineup_optimizer.R  -- optimize_lineup(), assemble_player_pool(),
#                               compute_dvp_factors(), compute_def_matchup_factors(),
#                               .apply_matchup(), .load_week_matchups(),
#                               SLOT_ELIGIBILITY, LINEUP_OFFENSE_POSITIONS
#                               (R/35 in turn sources R/33/R/32/R/29/R/19/R/15)
#   R/29_projection_engine.R -- calculate_def_st_points(), DEFAULT_SCORING_SETTINGS
#   R/15_multi_season_pbp.R  -- load_normalized_season()
#
# Consumed as data (passed in by the caller, not sourced):
#   lineup      -- the list returned by optimize_lineup() (R/35)
#   reconciled  -- R/32 reconciled projections (recovers starter sigmas)
#   vorp        -- R/33 VORP rankings (only for the waiver-DEF pool)
#   def_proj    -- R/34 DEF projections   (only for the waiver-DEF pool)
#
# RUN
# ---
#   source(here::here("R", "36_start_sit_uq.R"))
#   # offense-only enrichment (no waiver DEF comparison):
#   uq <- analyze_start_sit(lineup, reconciled)
#
#   # full enrichment with the rostered-vs-waiver DEF comparison, week 6:
#   uq <- analyze_start_sit(lineup, reconciled,
#                           source = src, vorp = vorp, def_proj = def_proj,
#                           week = 6L, save_output = TRUE)
#   uq$starters          # enriched lineup with the UQ columns
#   uq$week_risk_score   # one number: expected points at risk this week
#
# SCHEMA TAG: s3_r36_uq_v5
# Author: Christian K. LeBlanc
# Version: 2.0
# ==============================================================================

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(here)
library(glue)

# Null-coalescing infix (defined in R/19; redefine defensively so this file is
# self-contained if the R/19 chain has not been sourced yet).
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Source dependencies only if their entry points are not already in the session.
# Sourcing R/35 pulls in the whole projection/optimizer chain it depends on.
if (!exists("optimize_lineup") || !exists("assemble_player_pool")) {
  source(here::here("R", "35_lineup_optimizer.R"))
}
if (!exists("calculate_def_st_points")) {
  source(here::here("R", "29_projection_engine.R"))
}
if (!exists("load_normalized_season")) {
  source(here::here("R", "15_multi_season_pbp.R"))
}

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

SEASON <- 2026L
SCHEMA_TAG_UQ <- "s3_r36_uq_v5"

# Stakes label thresholds, in points of expected regret (tunable). A slot below
# STAKES_LOW_MAX is "low" stakes, at or above STAKES_HIGH_MIN is "high".
STAKES_LOW_MAX  <- 1.0
STAKES_HIGH_MIN <- 3.0

# Plain-language call thresholds, on p_start_correct (tunable). After the
# exclusive greedy assignment a slot CAN come back below 0.50, which means the
# alternative assigned to it projects better than the starter R/35 chose.
CALL_START_MIN <- 0.60
CALL_LEAN_MIN  <- 0.50

# DEF streaming pick-clarity thresholds, on P(the top defense beats the next
# option). DEF weekly variance is large, so adjacent defenses are usually a
# tossup; these label how separated the top pick actually is (tunable).
PICK_CLEAR_MIN  <- 0.60
PICK_SLIGHT_MIN <- 0.53

# Fallback weekly DEF scoring sigma used only if the empirical history pull
# returns too little to estimate one.
DEF_SIGMA_FALLBACK <- 4.0

# Minimum team-season residuals required before a within-team DEF sigma is
# trusted; below this, fall back.
DEF_SIGMA_MIN_N <- 50L

# Below this wrong-call probability, the conditional avg_miss is undefined
# (a near-lock has no meaningful "when wrong" loss); report NA.
P_WRONG_FLOOR <- 1e-6

# A sigma at or below this is treated as absent rather than as certainty.
SIGMA_ZERO_TOL <- 1e-8

# The pbp cache R/15 reads from. Deliberately distinct from the R/36 OUTPUT
# directory below: v1 redefined the global CACHE_DIR_DEFAULT after sourcing
# R/15 and R/29, which silently repointed their default too.
R36_PBP_CACHE_DIR <- here::here("data", "season2_cache")

# Where R/36's own artifacts go.
R36_OUTPUT_DIR <- here::here("data", "season3_cache")

DEF_SIGMA_CACHE_PATH <- file.path(R36_OUTPUT_DIR, "s3_r36_def_sigma.rds")

# Filename segment used when analyze_start_sit() is called without a league_id.
# Kept explicit so a snapshot written without league provenance is obvious on
# disk rather than looking like a real league.
LEAGUE_TAG_NONE <- "nolg"

# nflreadr::load_schedules() reports gametime in US/Eastern. Hardcoded because
# the field has no timezone attached; it is not a user setting.
SCHEDULE_TIMEZONE <- "America/New_York"

# Sanitize a league_id into a filename-safe segment. Sleeper league_ids are
# long digit strings, but this runs on whatever is passed so a hand-typed label
# cannot produce an invalid path.
.league_tag <- function(league_id) {
  if (is.null(league_id) || length(league_id) == 0L) return(LEAGUE_TAG_NONE)
  x <- as.character(league_id)[[1L]]
  if (is.na(x)) return(LEAGUE_TAG_NONE)
  x <- gsub("[^A-Za-z0-9_-]", "_", trimws(x))
  if (!nzchar(x)) LEAGUE_TAG_NONE else x
}

# ------------------------------------------------------------------------------
# NSE DECLARATIONS
# ------------------------------------------------------------------------------

utils::globalVariables(c(
  ".data", "nfl_gsis_id", "player_name", "team", "position", "opponent",
  "base_proj", "matchup_factor", "adj_proj", "adjusted_vorp", "confidence_flag",
  "schema_tag", "r32_sigma_week", "r32_posterior_sigma", "r32_omega",
  "lower_80", "upper_80", "is_available", "def_st_points", "slot",
  "mu_a", "sigma_a", "mu_s", "sigma_s", "alt_player", "alt_position",
  "alt_player_uc", "alt_adj_proj_uc", "p_start_correct_uc",
  "expected_regret_uc", "call_uc", "alt_player_re", "alt_adj_proj_re",
  "p_start_correct_re", "expected_regret_re", "call_re", "re_path",
  "alt_source", "alt_adj_proj", "p_start_correct", "avg_miss",
  "expected_regret", "stakes", "uq_schema_tag", "sigma_source", "call",
  "def_team", "p_vs_next", "pick_clarity", "rank", "sigma", "alt_row",
  "season", "week", "resid_z", "team_season_mean"
))

# ==============================================================================
# SECTION 1: DISTRIBUTION HELPERS (scalar; use base if/else, not dplyr::if_else)
# ==============================================================================

# ------------------------------------------------------------------------------
# .weekly_sigma_from_parts
# ------------------------------------------------------------------------------

#' Recompute the one-week predictive SD from its two parts
#'
#' Mirrors R/32's construction: Var(week) = tau^2 + omega^2, where tau is the
#' uncertainty in the player's mean (r32_posterior_sigma) and omega is
#' week-to-week dispersion (r32_omega). Used only when r32_sigma_week itself is
#' absent, i.e. an older reconciled file.
#'
#' @param tau,omega Numeric vectors. NA in omega propagates to NA, since
#'   without omega there is no weekly component and tau alone would understate
#'   the SD roughly fivefold.
#' @return Numeric vector of weekly sigmas.
#' @keywords internal
.weekly_sigma_from_parts <- function(tau, omega) {
  ifelse(is.na(omega), NA_real_, sqrt(dplyr::coalesce(tau, 0)^2 + omega^2))
}

# ------------------------------------------------------------------------------
# .valid_sigma
# ------------------------------------------------------------------------------

#' Is a sigma usable (finite, positive)?
#'
#' A missing or zero sigma must NOT be read as certainty. v1 let NA fall through
#' to a deterministic branch that returned p = 1 or p = 0, so an unmatched join
#' produced a fabricated near-lock.
#'
#' @keywords internal
.valid_sigma <- function(x) {
  !is.na(x) & is.finite(x) & x > SIGMA_ZERO_TOL
}

# ------------------------------------------------------------------------------
# .prob_start_correct
# ------------------------------------------------------------------------------

#' P(starter outscores alternative) for two independent Normals
#'
#' Returns NA when either mean is missing or either sigma is unusable. NA is
#' deliberate: an unscored slot is honest, a fabricated 1.0 is not.
#'
#' @param mu_s,sd_s Starter mean and sigma (scalars).
#' @param mu_a,sd_a Alternative mean and sigma (scalars).
#' @return Probability in [0,1], or NA.
#' @keywords internal
.prob_start_correct <- function(mu_s, sd_s, mu_a, sd_a) {
  if (is.na(mu_s) || is.na(mu_a)) return(NA_real_)
  if (!.valid_sigma(sd_s) || !.valid_sigma(sd_a)) return(NA_real_)
  sd_diff <- sqrt(sd_s^2 + sd_a^2)
  stats::pnorm((mu_s - mu_a) / sd_diff)
}

# ------------------------------------------------------------------------------
# .expected_regret
# ------------------------------------------------------------------------------

#' Unconditional expected regret E[max(A - S, 0)] for two independent Normals
#'
#' The probability-weighted points left on the bench: it is large only when the
#' alternative is both plausibly better AND the gap could be material. Uses the
#' closed-form mean of the positive part of a Normal:
#'   E[Y+] = m * pnorm(m / s) + s * dnorm(m / s),  m = mu_a - mu_s, s = sd_diff.
#'
#' @param mu_s,sd_s Starter mean and sigma (scalars).
#' @param mu_a,sd_a Alternative mean and sigma (scalars).
#' @return Expected regret in points, or NA.
#' @keywords internal
.expected_regret <- function(mu_s, sd_s, mu_a, sd_a) {
  if (is.na(mu_s) || is.na(mu_a)) return(NA_real_)
  if (!.valid_sigma(sd_s) || !.valid_sigma(sd_a)) return(NA_real_)
  sd_diff <- sqrt(sd_s^2 + sd_a^2)
  m <- mu_a - mu_s
  m * stats::pnorm(m / sd_diff) + sd_diff * stats::dnorm(m / sd_diff)
}

# ------------------------------------------------------------------------------
# .stakes_label
# ------------------------------------------------------------------------------

#' Bucket expected regret into a stakes label
#' @keywords internal
.stakes_label <- function(expected_regret) {
  if (is.na(expected_regret)) return(NA_character_)
  if (expected_regret < STAKES_LOW_MAX) return("low")
  if (expected_regret >= STAKES_HIGH_MIN) return("high")
  "medium"
}

# ------------------------------------------------------------------------------
# .call_label
# ------------------------------------------------------------------------------

#' Plain-language verdict for a slot, from p_start_correct
#'
#' "swap flagged" is reachable: alternatives are assigned exclusively, so a slot
#' can be left with an alternative that outprojects its starter.
#' @keywords internal
.call_label <- function(p_start_correct) {
  if (is.na(p_start_correct)) return(NA_character_)
  if (p_start_correct >= CALL_START_MIN) return("start")
  if (p_start_correct >= CALL_LEAN_MIN)  return("lean start")
  "swap flagged"
}

# ------------------------------------------------------------------------------
# .pick_clarity
# ------------------------------------------------------------------------------

#' Label how clearly the top streaming defense beats the next option
#'
#' Based on P(top beats next), not on regret: DEF variance makes the absolute
#' point swing large even between interchangeable defenses, so probability of
#' separation is the honest signal for a streaming pick.
#' @keywords internal
.pick_clarity <- function(p_vs_next) {
  if (is.na(p_vs_next)) return(NA_character_)
  if (p_vs_next >= PICK_CLEAR_MIN)  return("clear")
  if (p_vs_next >= PICK_SLIGHT_MIN) return("slight edge")
  "tossup"
}

# ------------------------------------------------------------------------------
# .eligible_positions_for_slot
# ------------------------------------------------------------------------------

#' Positions that may legally fill a given lineup slot (reverse of SLOT_ELIGIBILITY)
#' @keywords internal
.eligible_positions_for_slot <- function(slot) {
  names(SLOT_ELIGIBILITY)[
    vapply(SLOT_ELIGIBILITY, function(v) slot %in% v, logical(1))
  ]
}

# ==============================================================================
# SECTION 2: SIGMA ATTACHMENT
# ==============================================================================

# ------------------------------------------------------------------------------
# .build_recon_sigma
# ------------------------------------------------------------------------------

#' One-row-per-player sigma lookup from R/32 reconciled
#'
#' Rows with a missing nfl_gsis_id are dropped outright. The live reconciled
#' file carries several such rows, and dplyr's left_join defaults to
#' na_matches = "na", so an NA id on the left side would match EVERY NA id on
#' the right and multiply that player's row. Dropping them here plus
#' na_matches = "never" at the join site closes both halves of that.
#'
#' @param reconciled R/32 reconciled projections.
#' @return Tibble: nfl_gsis_id, r32_sigma_week, r32_posterior_sigma, r32_omega.
#' @keywords internal
.build_recon_sigma <- function(reconciled) {
  keep <- c("nfl_gsis_id", "r32_sigma_week", "r32_posterior_sigma",
            "r32_omega")
  out <- reconciled %>%
    dplyr::select(dplyr::any_of(c(keep, "position")))

  # Columns that are genuinely optional get materialized as NA so downstream
  # code can reference them unconditionally.
  for (nm in setdiff(keep, names(out))) {
    out[[nm]] <- NA_real_
  }

  out <- out %>%
    dplyr::filter(!is.na(.data$nfl_gsis_id)) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)

  n_dup <- sum(duplicated(out$nfl_gsis_id))
  if (n_dup > 0L) {
    stop(glue(".build_recon_sigma(): {n_dup} duplicate nfl_gsis_id remain ",
              "after distinct(); reconciled is malformed."), call. = FALSE)
  }

  # Board-wide position medians for the fallback, attached here rather than
  # computed inside .attach_sigma(). They MUST come from the whole board: a
  # lineup holds one QB and one TE, so a median taken over the frame being
  # scored has no other same-position row to draw on and the fallback can
  # never fire for a single-slot position.
  board_sigma <- out$r32_sigma_week
  recomp <- .weekly_sigma_from_parts(out$r32_posterior_sigma, out$r32_omega)
  board_sigma[!.valid_sigma(board_sigma)] <- recomp[!.valid_sigma(board_sigma)]

  pos <- if ("position" %in% names(out)) out$position else
    rep(NA_character_, nrow(out))
  pos_median <- tapply(board_sigma, pos, function(v) {
    v <- v[.valid_sigma(v)]
    if (length(v) == 0L) NA_real_ else stats::median(v)
  })

  out <- dplyr::select(out, dplyr::all_of(keep))
  attr(out, "pos_median") <- pos_median
  out
}

# ------------------------------------------------------------------------------
# .attach_sigma
# ------------------------------------------------------------------------------

#' Attach a sigma and its provenance to a player table
#'
#' The sigma is the ONE-WEEK predictive SD, not the season-long posterior SD.
#' See the header: omega carries roughly 98 percent of weekly variance, so
#' substituting r32_posterior_sigma here understates the denominator by a large
#' factor and makes every call read as more confident than it is.
#'
#' Preference order (Chain B item 5c):
#'   1. r32_sigma_week                                 -> "r32_sigma_week"
#'   2. sqrt(r32_posterior_sigma^2 + r32_omega^2)      -> "recomputed_quadrature"
#'   3. board-wide position median (from reconciled)   -> "position_median"
#'   4. nothing usable                                 -> "missing" (sigma NA)
#'
#' No qb_discount_factor scaling: the discount acts on the mean upstream in
#' R/32, and r32_sigma_week is built from tau and omega directly, so applying
#' it here would double-count.
#'
#' @param df Table with nfl_gsis_id and position.
#' @param recon_sigma Output of .build_recon_sigma().
#' @param sigma_col,source_col Names for the two output columns.
#' @return df with the two columns added; row count is asserted unchanged.
#' @keywords internal
.attach_sigma <- function(df, recon_sigma,
                          sigma_col = "sigma_s", source_col = "sigma_source") {

  n_before <- nrow(df)

  joined <- df %>%
    dplyr::left_join(recon_sigma, by = "nfl_gsis_id",
                     na_matches = "never", relationship = "many-to-one")

  if (nrow(joined) != n_before) {
    stop(glue(".attach_sigma(): join changed the row count ",
              "({n_before} -> {nrow(joined)}). Reconciled is not unique on ",
              "nfl_gsis_id."), call. = FALSE)
  }

  week_sigma <- joined$r32_sigma_week
  recomputed <- .weekly_sigma_from_parts(joined$r32_posterior_sigma,
                                         joined$r32_omega)

  sig <- rep(NA_real_, n_before)
  src <- rep("missing", n_before)

  use_week <- .valid_sigma(week_sigma)
  sig[use_week] <- week_sigma[use_week]
  src[use_week] <- "r32_sigma_week"

  use_recomp <- is.na(sig) & .valid_sigma(recomputed)
  sig[use_recomp] <- recomputed[use_recomp]
  src[use_recomp] <- "recomputed_quadrature"

  # Position median fallback, taken from the BOARD (attached by
  # .build_recon_sigma), not from the frame being scored. A lineup holds one QB
  # and one TE, so a within-frame median cannot fire for a single-slot
  # position -- the only row of that position is the one that needs filling.
  if (any(is.na(sig))) {
    med <- attr(recon_sigma, "pos_median")
    if (!is.null(med)) {
      need <- which(is.na(sig))
      fill <- as.numeric(med[joined$position[need]])
      ok <- .valid_sigma(fill)
      sig[need[ok]] <- fill[ok]
      src[need[ok]] <- "position_median"
    }
  }

  df[[sigma_col]]  <- sig
  df[[source_col]] <- src
  df
}

# ==============================================================================
# SECTION 3: EMPIRICAL DEF SIGMA
# ==============================================================================

# ------------------------------------------------------------------------------
# .compute_def_sigma
# ------------------------------------------------------------------------------

#' Within-team league-wide weekly DEF scoring sigma from history
#'
#' Loads each season's pbp, scores team defenses per game with
#' calculate_def_st_points() (R/29), demeans by team-season, pools the residuals
#' and returns their standard deviation.
#'
#' Demeaning is the point. The projection a defense carries already encodes how
#' good that defense is; the quantity a weekly start/sit call needs is how much
#' a defense swings around its OWN mean. Pooling raw team-weeks (v1) added the
#' between-team spread on top of that and overstated sigma, which pushed every
#' DEF comparison toward "tossup".
#'
#' Result is cached to DEF_SIGMA_CACHE_PATH, keyed on seasons and the scoring
#' settings. calculate_def_st_points() filters to season_type == "REG" itself
#' (R/29), so postseason games are already excluded.
#'
#' @param seasons Integer vector of prior seasons (default SEASON - 1).
#' @param scoring_settings DEF scoring passed to calculate_def_st_points().
#' @param cache_dir Character. R/15 pbp cache directory.
#' @param use_cache Logical. Read/write the cached sigma.
#' @return A single numeric sigma (falls back to DEF_SIGMA_FALLBACK if history
#'   is unavailable or too thin).
#' @seealso calculate_def_st_points, load_normalized_season
#' @keywords internal
.compute_def_sigma <- function(seasons = SEASON - 1L,
                               scoring_settings = DEFAULT_SCORING_SETTINGS,
                               cache_dir = R36_PBP_CACHE_DIR,
                               use_cache = TRUE) {

  key <- list(seasons = sort(as.integer(seasons)),
              scoring = scoring_settings)

  if (use_cache && file.exists(DEF_SIGMA_CACHE_PATH)) {
    cached <- tryCatch(readRDS(DEF_SIGMA_CACHE_PATH), error = function(e) NULL)
    if (!is.null(cached) && identical(cached$key, key) &&
        .valid_sigma(cached$sigma)) {
      message(glue("  DEF sigma from cache: {round(cached$sigma, 3)} ",
                   "(n = {cached$n})"))
      return(cached$sigma)
    }
  }

  resid <- purrr::map_dfr(seasons, function(s) {
    pbp <- tryCatch(load_normalized_season(s, cache_dir = cache_dir),
                    error = function(e) NULL)
    if (is.null(pbp) || nrow(pbp) == 0L) return(tibble::tibble())
    dg <- tryCatch(calculate_def_st_points(pbp,
                                            scoring_settings = scoring_settings),
                   error = function(e) NULL)
    need <- c("season", "team", "def_st_points")
    if (is.null(dg) || nrow(dg) == 0L ||
        length(setdiff(need, names(dg))) > 0L) {
      return(tibble::tibble())
    }
    dg %>% dplyr::select(dplyr::all_of(need))
  })

  if (nrow(resid) == 0L) {
    message(glue("  .compute_def_sigma(): no DEF history; using fallback ",
                 "sigma {DEF_SIGMA_FALLBACK}."))
    return(DEF_SIGMA_FALLBACK)
  }

  # Demean within team-season, then pool.
  resid <- resid %>%
    dplyr::filter(is.finite(.data$def_st_points)) %>%
    dplyr::group_by(.data$season, .data$team) %>%
    dplyr::mutate(team_season_mean = mean(.data$def_st_points)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(resid_z = .data$def_st_points - .data$team_season_mean)

  vals <- resid$resid_z[is.finite(resid$resid_z)]

  if (length(vals) < DEF_SIGMA_MIN_N) {
    message(glue("  .compute_def_sigma(): only {length(vals)} residuals ",
                 "(need {DEF_SIGMA_MIN_N}); using fallback sigma ",
                 "{DEF_SIGMA_FALLBACK}."))
    return(DEF_SIGMA_FALLBACK)
  }

  # Degrees of freedom: one mean was estimated per team-season.
  n_groups <- nrow(dplyr::distinct(resid, .data$season, .data$team))
  dof <- length(vals) - n_groups
  if (dof < 2L) {
    message(glue("  .compute_def_sigma(): insufficient degrees of freedom; ",
                 "using fallback sigma {DEF_SIGMA_FALLBACK}."))
    return(DEF_SIGMA_FALLBACK)
  }
  sigma <- sqrt(sum(vals^2) / dof)

  if (!.valid_sigma(sigma)) return(DEF_SIGMA_FALLBACK)

  message(glue("  DEF sigma (within-team): {round(sigma, 3)} from ",
               "{length(vals)} team-weeks across {n_groups} team-seasons"))

  if (use_cache) {
    dir.create(dirname(DEF_SIGMA_CACHE_PATH), recursive = TRUE,
               showWarnings = FALSE)
    saveRDS(list(key = key, sigma = sigma, n = length(vals),
                 built_at = Sys.time()),
            DEF_SIGMA_CACHE_PATH)
  }

  sigma
}

# ------------------------------------------------------------------------------
# .assemble_waiver_def
# ------------------------------------------------------------------------------

#' Available (waiver) team defenses, matchup-adjusted to the lineup's scale
#'
#' Re-assembles the player pool to recover every DEF row, keeps the ones no team
#' rosters, and applies the same matchup adjustment the started DEF received so
#' the comparison is like-for-like.
#'
#' @param source league_source from build_league_source() (R/35).
#' @param reconciled R/32 reconciled projections.
#' @param vorp R/33 VORP rankings.
#' @param def_proj R/34 DEF projections.
#' @param week Integer or NULL. NULL = neutral matchup.
#' @param dvp,def_factors Optional precomputed matchup factors.
#' @param season Integer NFL season.
#' @param cache_dir Character. R/15 pbp cache directory.
#' @return Tibble of available DEF rows with adj_proj and matchup_factor; empty
#'   tibble if none.
#' @keywords internal
.assemble_waiver_def <- function(source, reconciled, vorp, def_proj,
                                 week, dvp, def_factors, season, cache_dir) {
  pool <- assemble_player_pool(source, reconciled, vorp, def_proj)
  def_av <- pool %>%
    dplyr::filter(.data$position == "DEF", .data$is_available %in% TRUE)
  if (nrow(def_av) == 0L) return(def_av)

  if (!is.null(week)) {
    if (is.null(dvp)) {
      dvp <- compute_dvp_factors(seasons = season - 1L, cache_dir = cache_dir)
    }
    if (is.null(def_factors)) {
      def_factors <- compute_def_matchup_factors(seasons = season - 1L,
                                                 cache_dir = cache_dir)
    }
    week_matchups <- .load_week_matchups(season, week)
  } else {
    week_matchups <- NULL
  }

  out <- .apply_matchup(def_av, week_matchups, dvp, def_factors)

  # Bye guard: with a week supplied, a missing opponent means the team is not
  # playing. Such a row still carries adj_proj == base_proj out of
  # .apply_matchup(), so it must be dropped before it can be recommended.
  if (!is.null(week)) {
    n_before <- nrow(out)
    out <- dplyr::filter(out, !is.na(.data$opponent))
    n_dropped <- n_before - nrow(out)
    if (n_dropped > 0L) {
      message(glue("  Dropped {n_dropped} waiver DEF on bye in week {week}."))
    }
  }
  out
}

# ------------------------------------------------------------------------------
# .recommend_def_stream
# ------------------------------------------------------------------------------

#' Rank available defenses to stream into an open DEF slot
#'
#' For a roster with no DEF in the optimal lineup (a streamer's open slot), this
#' ranks the available defenses by matchup-adjusted projection and, for each,
#' reports the probability of taking it over the next-ranked option. The top row
#' is the recommended stream; the p_vs_next on that row says how clear-cut the
#' choice is over the second-best defense.
#'
#' @param source,reconciled,vorp,def_proj Inputs for the available-DEF pool.
#' @param def_sigma Within-team empirical weekly DEF sigma (.compute_def_sigma).
#' @param week,dvp,def_factors,season,cache_dir Matchup inputs (see optimize_lineup).
#' @param max_suggestions Integer. Top-N defenses to return.
#' @return Tibble: rank, def_team, opponent, adj_proj, p_vs_next, pick_clarity,
#'   uq_schema_tag; or NULL if none available.
#' @seealso .assemble_waiver_def, .compute_def_sigma
#' @keywords internal
.recommend_def_stream <- function(source, reconciled, vorp, def_proj,
                                  def_sigma, week, dvp, def_factors,
                                  season, cache_dir, max_suggestions) {

  wd <- .assemble_waiver_def(source, reconciled, vorp, def_proj,
                             week = week, dvp = dvp, def_factors = def_factors,
                             season = season, cache_dir = cache_dir)
  if (is.null(wd) || nrow(wd) == 0L) return(NULL)

  ranked <- wd %>%
    dplyr::transmute(
      def_team = .data$team,
      opponent = .data$opponent,
      adj_proj = .data$adj_proj,
      # Matchup shifts the mean (already in adj_proj); sigma stays unscaled.
      sigma    = def_sigma
    ) %>%
    dplyr::arrange(dplyr::desc(.data$adj_proj)) %>%
    dplyr::slice_head(n = max_suggestions) %>%
    dplyr::mutate(rank = dplyr::row_number())

  # P(this defense outscores the next-ranked one). The absolute point swing is
  # dominated by DEF variance and is not decision-relevant for near-ties, so it
  # is intentionally not reported; the clarity label reads off this probability.
  n <- nrow(ranked)
  p_vs_next <- rep(NA_real_, n)
  if (n >= 2L) {
    for (i in seq_len(n - 1L)) {
      p_vs_next[i] <- .prob_start_correct(ranked$adj_proj[i], ranked$sigma[i],
                                          ranked$adj_proj[i + 1L],
                                          ranked$sigma[i + 1L])
    }
  }
  clarity <- vapply(p_vs_next, .pick_clarity, character(1))

  ranked %>%
    dplyr::transmute(
      rank,
      def_team,
      opponent,
      adj_proj = round(.data$adj_proj, 2),
      p_vs_next = round(p_vs_next, 3),
      pick_clarity = clarity,
      uq_schema_tag = SCHEMA_TAG_UQ
    )
}

# ==============================================================================
# SECTION 4: MAIN ENTRY POINT
# ==============================================================================

# ------------------------------------------------------------------------------
# analyze_start_sit
# ------------------------------------------------------------------------------

#' Enrich an optimized lineup with start/sit uncertainty quantification
#'
#' Takes the list optimize_lineup() returned and, for every started slot,
#' computes the probability the start is correct, the conditional average miss,
#' the probability-weighted expected regret, a stakes label and a plain-language
#' call, plus a single week-level risk score. Offense sigma comes from R/32 (see
#' .attach_sigma for the preference order); DEF uses a within-team empirical
#' sigma and compares against available (waiver) defenses.
#'
#' The offense comparison needs only lineup and reconciled. The
#' rostered-vs-waiver DEF comparison additionally needs source, vorp, and
#' def_proj. When include_waiver_def is TRUE but those are not all supplied, the
#' DEF alternatives fall back to the bench with a one-line message.
#'
#' @param lineup List from optimize_lineup(): starters, bench, unavailable,
#'   all_close.
#' @param reconciled R/32 reconciled projections (for starter and bench sigmas).
#' @param source league_source, or NULL. Needed for the waiver-DEF comparison.
#' @param vorp R/33 VORP rankings, or NULL. Needed for the waiver-DEF pool.
#' @param def_proj R/34 DEF projections, or NULL. Needed for the waiver-DEF pool.
#' @param week Integer or NULL. The week the lineup was solved for. Supplying it
#'   also enables the bye-week guard.
#' @param league_id Character or NULL. Provenance for the saved snapshot only;
#'   it has no effect on any number produced. When save_output = TRUE this
#'   becomes a filename segment so runs for different leagues in the same week
#'   do not overwrite each other. Required in practice for any multi-league run.
#' @param dvp,def_factors Optional precomputed matchup factors for the waiver DEF.
#' @param def_seasons Integer vector. History window for the empirical DEF sigma.
#' @param def_scoring_settings DEF scoring passed to calculate_def_st_points().
#'   Default is R/29's DEFAULT_SCORING_SETTINGS (Sleeper standard). Pass the
#'   league's own settings for a custom-scoring league.
#' @param include_waiver_def Logical. Compare the started DEF against available
#'   defenses, not just benched ones; and, when the DEF slot is open, produce a
#'   ranked streaming recommendation.
#' @param def_max_suggestions Integer. Top-N defenses in the streaming table.
#' @param allow_bye_starters Logical. FALSE (default) turns a starter with a
#'   missing opponent into a hard stop when week is supplied. Set TRUE only to
#'   inspect a known-bad lineup; the numbers for that slot will be wrong.
#' @param shape Reserved. The R/50 shape table, for the skew-aware core that is
#'   not yet wired up. Validated if supplied, otherwise ignored. See the SYMMETRY
#'   note in the file header.
#' @param season Integer or NULL (default = source season, else SEASON).
#' @param cache_dir Character. R/15 pbp cache directory.
#' @param save_output Logical. Write the enriched starters table to RDS + CSV,
#'   with the week in the filename.
#' @return The input lineup list with starters enriched (adds alt_player,
#'   alt_position, alt_source, alt_adj_proj, p_start_correct, avg_miss,
#'   expected_regret, stakes, call, sigma_s, sigma_source, uq_schema_tag), a
#'   week_risk_score element, and a def_streaming element.
#' @seealso optimize_lineup, assemble_player_pool, calculate_def_st_points
#' @export
#' Teams whose game for a given week has already kicked off
#'
#' A bench player whose game has started cannot be moved into a lineup. Scoring
#' him as an "alternative" produces a recommendation that cannot be acted on,
#' and worse, exclusivity then consumes him so a genuinely startable player is
#' never considered for that slot.
#'
#' Source is nflreadr::load_schedules(), already used by R/35's
#' .load_week_matchups(). gameday is a date and gametime an "HH:MM" clock in US
#' Eastern; they are combined and compared against `as_of`.
#'
#' Rows with a missing gameday or gametime are treated as NOT kicked off. That
#' is the deliberate direction to fail: schedules can be incomplete for a
#' future week, and wrongly excluding a startable player is worse than wrongly
#' including one, since the second case is visible on screen and the first is
#' silent.
#'
#' @param season Integer season.
#' @param week Integer week.
#' @param as_of POSIXct. Defaults to now.
#'
#' @return Character vector of team abbreviations whose game has started.
#'   character(0) if the schedule cannot be read.
#' @keywords internal
.kicked_off_teams <- function(season, week, as_of = Sys.time()) {

  if (is.null(season) || is.null(week)) return(character(0))

  sched <- tryCatch(nflreadr::load_schedules(seasons = as.integer(season)),
                    error = function(e) NULL)
  if (is.null(sched) || nrow(sched) == 0L) {
    message("  .kicked_off_teams(): schedule unavailable; no kickoff filter ",
            "applied.")
    return(character(0))
  }

  wk <- dplyr::filter(sched, .data$week == as.integer(!!week))
  if (nrow(wk) == 0L) return(character(0))

  if (!all(c("gameday", "gametime") %in% names(wk))) {
    message("  .kicked_off_teams(): schedule has no gameday/gametime; no ",
            "kickoff filter applied.")
    return(character(0))
  }

  kickoff <- suppressWarnings(as.POSIXct(
    paste(as.character(wk$gameday), as.character(wk$gametime)),
    format = "%Y-%m-%d %H:%M", tz = SCHEDULE_TIMEZONE
  ))

  started <- !is.na(kickoff) & kickoff <= as_of
  if (!any(started)) return(character(0))

  out <- unique(c(wk$home_team[started], wk$away_team[started]))
  out[!is.na(out)]
}


analyze_start_sit <- function(lineup, reconciled,
                              source = NULL, vorp = NULL, def_proj = NULL,
                              week = NULL, league_id = NULL,
                              dvp = NULL, def_factors = NULL,
                              def_seasons = SEASON - 1L,
                              def_scoring_settings = DEFAULT_SCORING_SETTINGS,
                              include_waiver_def = TRUE,
                              def_max_suggestions = 5L,
                              allow_bye_starters = FALSE,
                              shape = NULL,
                              season = NULL,
                              cache_dir = R36_PBP_CACHE_DIR,
                              save_output = FALSE) {

  # ---- validate inputs ----
  if (!is.list(lineup) || is.null(lineup$starters) || is.null(lineup$bench)) {
    stop("lineup must be the list returned by optimize_lineup() ",
         "(with $starters and $bench).", call. = FALSE)
  }
  if (!"nfl_gsis_id" %in% names(reconciled)) {
    stop("reconciled missing column: nfl_gsis_id", call. = FALSE)
  }
  have_week   <- "r32_sigma_week" %in% names(reconciled)
  have_parts  <- all(c("r32_posterior_sigma", "r32_omega") %in% names(reconciled))
  if (!have_week && !have_parts) {
    stop("reconciled carries neither r32_sigma_week nor the pair ",
         "(r32_posterior_sigma, r32_omega) needed to recompute it. R/36 needs ",
         "a ONE-WEEK predictive SD; the season-long posterior sigma is not a ",
         "substitute. Regenerate R/32 with .build_omega().", call. = FALSE)
  }
  if (!have_week && have_parts) {
    message("  Note: r32_sigma_week absent; recomputing it from ",
            "r32_posterior_sigma and r32_omega in quadrature.")
  }
  if (!is.null(shape)) {
    if (!is.list(shape) || is.null(shape$meta)) {
      stop("shape must be the R/50 shape table (a list with a $meta element).",
           call. = FALSE)
    }
    message("  Note: `shape` is accepted but not yet used; the core is still ",
            "Normal. See the SYMMETRY note in the R/36 header.")
  }

  starters <- lineup$starters
  bench    <- lineup$bench
  if (nrow(starters) == 0L) {
    message("analyze_start_sit(): lineup has no starters; nothing to enrich.")
    lineup$week_risk_score <- 0
    return(lineup)
  }
  season <- season %||% (if (!is.null(source)) source$season else NULL) %||% SEASON

  # ---- bye-week guard ----
  # R/35's .apply_matchup() leaves a bye-week player with opponent NA,
  # matchup_factor 1 and adj_proj == base_proj, i.e. a full projection for
  # someone who is not playing. Refuse to score that.
  if (!is.null(week)) {
    bye_start <- starters %>% dplyr::filter(is.na(.data$opponent))
    if (nrow(bye_start) > 0L) {
      who <- paste(bye_start$player_name, collapse = ", ")
      if (!allow_bye_starters) {
        stop(glue("analyze_start_sit(): {nrow(bye_start)} starter(s) have no ",
                  "week {week} opponent and are on bye: {who}. R/35 projected ",
                  "them at full strength. Re-solve the lineup, or set ",
                  "allow_bye_starters = TRUE to score it anyway (the numbers ",
                  "for those slots will be wrong)."), call. = FALSE)
      }
      message(glue("  WARNING: scoring {nrow(bye_start)} bye-week starter(s) ",
                   "at full projection: {who}"))
    }

    n_bench_before <- nrow(bench)
    bench <- dplyr::filter(bench, !is.na(.data$opponent))
    n_bye_bench <- n_bench_before - nrow(bench)
    if (n_bye_bench > 0L) {
      message(glue("  Dropped {n_bye_bench} bench player(s) on bye from the ",
                   "alternative pool."))
    }
  }

  # ---- empirical DEF sigma (computed when a DEF is started; the streaming
  #      path below computes it on demand when the DEF slot is open instead) ----
  has_def_starter <- "DEF" %in% starters$position
  def_sigma <- if (has_def_starter) {
    .compute_def_sigma(seasons = def_seasons,
                       scoring_settings = def_scoring_settings,
                       cache_dir = cache_dir)
  } else {
    NA_real_
  }

  # ---- recover starter means and sigmas ----
  recon_sigma <- .build_recon_sigma(reconciled)

  starters2 <- starters %>%
    dplyr::mutate(mu_s = .data$adj_proj) %>%
    .attach_sigma(recon_sigma, sigma_col = "sigma_s",
                  source_col = "sigma_source")

  if (nrow(starters2) != nrow(starters)) {
    stop(glue("analyze_start_sit(): starter row count changed during sigma ",
              "attachment ({nrow(starters)} -> {nrow(starters2)})."),
         call. = FALSE)
  }

  # DEF starters carry a synthetic nfl_gsis_id ("DEF_<team>") that is not in
  # reconciled, so they resolve to the pooled DEF sigma instead.
  is_def <- starters2$position == "DEF"
  if (any(is_def)) {
    starters2$sigma_s[is_def] <- def_sigma
    starters2$sigma_source[is_def] <- "pooled_def"
  }

  # ---- kickoff filter ----
  # Drop bench players whose game has already started: they are not startable,
  # so they are not alternatives. Applied BEFORE the pool is built so the
  # exclusivity pass never consumes a locked player on behalf of a slot that
  # could have used a startable one.
  #
  # Starters are NOT filtered. A starter whose game has begun is already
  # locked in; the honest treatment is to keep scoring him and let the
  # comparison show what the decision looked like, not to erase the slot.
  n_locked <- 0L
  if (!is.null(week)) {
    locked_teams <- .kicked_off_teams(season %||% SEASON, week)
    if (length(locked_teams) > 0L) {
      n_before <- nrow(bench)
      bench <- dplyr::filter(bench, !(.data$team %in% locked_teams))
      n_locked <- n_before - nrow(bench)
      if (n_locked > 0L) {
        message(glue("  Kickoff filter: dropped {n_locked} bench player(s) ",
                     "whose game has started ",
                     "({paste(locked_teams, collapse = ', ')})."))
      }
    }
  }

  # ---- build the alternative pool ----
  # Offense alternatives: benched offensive players. Sigma comes from the same
  # preference order the starters used, not from the bench interval columns, so
  # both sides of every comparison are on the same footing.
  off_alts <- bench %>%
    dplyr::filter(.data$position %in% LINEUP_OFFENSE_POSITIONS) %>%
    .attach_sigma(recon_sigma, sigma_col = "sigma_a",
                  source_col = "alt_sigma_source") %>%
    dplyr::transmute(
      position,
      alt_player = .data$player_name,
      alt_source = "bench",
      mu_a       = .data$adj_proj,
      sigma_a    = .data$sigma_a
    )

  # DEF alternatives: benched DEFs, plus available waiver DEFs when requested
  # and the inputs to assemble them are present.
  def_alt_raw <- bench %>%
    dplyr::filter(.data$position == "DEF") %>%
    dplyr::transmute(position,
                     alt_player = .data$player_name,
                     alt_source = "bench",
                     mu_a = .data$adj_proj)

  if (has_def_starter && include_waiver_def) {
    have_waiver_inputs <- !is.null(source) && !is.null(vorp) &&
      !is.null(def_proj)
    if (have_waiver_inputs) {
      wd <- .assemble_waiver_def(source, reconciled, vorp, def_proj,
                                 week = week, dvp = dvp,
                                 def_factors = def_factors,
                                 season = season, cache_dir = cache_dir)
      if (nrow(wd) > 0L) {
        def_alt_raw <- dplyr::bind_rows(
          def_alt_raw,
          wd %>% dplyr::transmute(position,
                                  alt_player = .data$player_name,
                                  alt_source = "waiver",
                                  mu_a = .data$adj_proj)
        )
      }
    } else {
      message("  include_waiver_def = TRUE but source/vorp/def_proj not all ",
              "supplied; DEF alternatives limited to the bench.")
    }
  }

  def_alts <- def_alt_raw %>%
    dplyr::mutate(sigma_a = def_sigma) %>%
    dplyr::select(position, alt_player, alt_source, mu_a, sigma_a)

  alternatives <- dplyr::bind_rows(off_alts, def_alts)

  # ---- per-slot metrics ----
  # Alternatives are assigned EXCLUSIVELY: a benched player can be the best
  # alternative for at most one slot. Slots are processed greedily in
  # descending first-pass regret (computed vs the full pool), and each slot's
  # chosen alternative is removed from the pool before the next slot is
  # evaluated. Without this, one strong bench player could stand in as the
  # replacement for several starters at once and the summed regrets would
  # count him repeatedly.
  if (nrow(alternatives) > 0L) {
    alternatives <- alternatives %>%
      dplyr::mutate(alt_row = dplyr::row_number())
  } else {
    alternatives <- alternatives %>% dplyr::mutate(alt_row = integer(0))
  }

  empty_metrics <- tibble::tibble(
    alt_row = NA_integer_,
    alt_player = NA_character_, alt_position = NA_character_,
    alt_source = NA_character_, alt_adj_proj = NA_real_,
    p_start_correct = NA_real_, avg_miss = NA_real_,
    expected_regret = NA_real_, stakes = NA_character_,
    call = NA_character_
  )

  # The alternative chosen is the one that maximizes EXPECTED REGRET against
  # this starter, not the one with the highest mean. Expected regret is what
  # week_risk_score sums, and a higher-variance alternative with a slightly
  # lower mean can carry more of it than the max-mean candidate.
  metrics_for <- function(row, cand) {
    if (nrow(cand) == 0L) return(empty_metrics)

    regrets <- vapply(seq_len(nrow(cand)), function(j) {
      r <- .expected_regret(row$mu_s, row$sigma_s,
                            cand$mu_a[j], cand$sigma_a[j])
      if (is.na(r)) -Inf else r
    }, numeric(1))

    if (all(!is.finite(regrets))) {
      # No candidate could be scored (missing sigma on both sides, say). Fall
      # back to the max-mean candidate so the row still identifies WHO the
      # alternative was, with NA metrics.
      j <- which.max(cand$mu_a)
      if (length(j) == 0L) return(empty_metrics)
      return(tibble::tibble(
        alt_row = cand$alt_row[j], alt_player = cand$alt_player[j],
        alt_position = cand$position[j], alt_source = cand$alt_source[j],
        alt_adj_proj = round(cand$mu_a[j], 2),
        p_start_correct = NA_real_, avg_miss = NA_real_,
        expected_regret = NA_real_, stakes = NA_character_,
        call = NA_character_
      ))
    }

    j <- which.max(regrets)
    e_reg <- regrets[j]
    p_corr <- .prob_start_correct(row$mu_s, row$sigma_s,
                                  cand$mu_a[j], cand$sigma_a[j])
    p_wrong <- 1 - p_corr
    a_miss <- if (is.na(p_wrong) || p_wrong < P_WRONG_FLOOR) {
      NA_real_
    } else {
      e_reg / p_wrong
    }

    tibble::tibble(
      alt_row         = cand$alt_row[j],
      alt_player      = cand$alt_player[j],
      alt_position    = cand$position[j],
      alt_source      = cand$alt_source[j],
      alt_adj_proj    = round(cand$mu_a[j], 2),
      p_start_correct = round(p_corr, 3),
      avg_miss        = round(a_miss, 2),
      expected_regret = round(e_reg, 2),
      stakes          = .stakes_label(e_reg),
      call            = .call_label(p_corr)
    )
  }

  # First pass: regret vs the full pool, used to order the greedy pass AND
  # retained as the unconstrained pairwise view.
  #
  # TWO PROBABILITIES, TWO QUESTIONS. They are not interchangeable:
  #
  #   p_start_correct / expected_regret  -- EXCLUSIVE. Each bench player backs
  #     at most one slot. This is the portfolio view, and it is what
  #     week_risk_score sums. Without exclusivity one strong bench player would
  #     be counted as the replacement for several starters at once and the
  #     total would be inflated.
  #
  #   p_start_correct_uc / alt_player_uc -- UNCONSTRAINED. Each slot scored
  #     against the full eligible pool, ignoring what other slots claimed. This
  #     answers "should I start X over Y" for one slot in isolation.
  #
  # NEVER sum the _uc columns. They double count by construction; that is the
  # whole reason the exclusive pass exists.
  #
  # The case that motivated this: a lineup with one bench TE, a FLEX slot
  # holding a TE, and a TE slot. The FLEX slot claimed the only bench TE, so
  # the TE slot scored NA under exclusivity even though the pairwise comparison
  # was the closest call on the board.
  first_pass <- lapply(seq_len(nrow(starters2)), function(i) {
    row  <- starters2[i, ]
    cand <- alternatives %>%
      dplyr::filter(.data$position %in% .eligible_positions_for_slot(row$slot))
    metrics_for(row, cand)
  })

  first_pass_regret <- vapply(first_pass, function(m) {
    if (is.na(m$expected_regret)) -Inf else m$expected_regret
  }, numeric(1))

  # ---- REASSIGNMENT PASS ----
  # The bench is not the whole move set. A starter who is eligible for another
  # started slot can be moved into it, and a bench player then fills the slot
  # he vacated. Starting Loveland at TE and someone else at FLEX is a lineup
  # you can actually set, and the pairwise bench comparison never sees it.
  #
  # THE ALGEBRA, which is why this is cheap:
  #
  #   current     = mu_P (slot S) + mu_Q (slot T)
  #   reassigned  = mu_Q (slot S) + mu_X (slot T)
  #   difference  = mu_P - mu_X
  #
  # Q is on BOTH sides. Not two similar players -- the same player, the same
  # random variable, so he cancels exactly in the difference. No correlation
  # assumption is needed for that cancellation and no joint distribution has
  # to be built.
  #
  # What survives is: the starter in slot S competes against the bench players
  # eligible for slot T, the slot that gets vacated. Pitts is NOT compared to
  # Loveland. He is compared to whoever would fill FLEX once Loveland slides
  # over.
  #
  # SCOPE LIMIT, deliberate. This enumerates single reassignments only: one
  # starter moves, one bench player fills behind him. Chains of two or more
  # moves are not considered, and the vacated slot is filled independently of
  # what other slots claimed. The general form is a lineup-level re-solve,
  # which needs a joint predictive distribution and is scheduled with the
  # shape-aware core, not here.
  #
  # These columns are UNCONSTRAINED in the same sense as the _uc columns: they
  # ignore what other slots took, so they MUST NOT be summed.
  reassignment <- dplyr::bind_rows(lapply(seq_len(nrow(starters2)), function(i) {
    row      <- starters2[i, ]
    elig_S   <- .eligible_positions_for_slot(row$slot)

    # Other started players who could legally occupy THIS slot.
    movers <- which(seq_len(nrow(starters2)) != i &
                      starters2$position %in% elig_S)

    empty_re <- tibble::tibble(
      alt_player_re   = NA_character_, alt_adj_proj_re = NA_real_,
      p_start_correct_re = NA_real_, expected_regret_re = NA_real_,
      call_re = NA_character_, re_path = NA_character_
    )

    if (length(movers) == 0L) return(empty_re)

    best <- NULL
    for (j in movers) {
      mover   <- starters2[j, ]
      # Bench players eligible for the slot the mover would vacate.
      fillers <- alternatives %>%
        dplyr::filter(.data$position %in%
                        .eligible_positions_for_slot(mover$slot))
      m <- metrics_for(row, fillers)
      if (is.na(m$expected_regret)) next
      if (is.null(best) || m$expected_regret > best$m$expected_regret) {
        best <- list(m = m, mover = mover)
      }
    }

    if (is.null(best)) return(empty_re)

    tibble::tibble(
      alt_player_re      = best$m$alt_player,
      alt_adj_proj_re    = best$m$alt_adj_proj,
      p_start_correct_re = best$m$p_start_correct,
      expected_regret_re = best$m$expected_regret,
      call_re            = best$m$call,
      re_path            = glue("{best$mover$player_name} -> {row$slot}; ",
                                "{best$m$alt_player} -> {best$mover$slot}")
    )
  }))

  unconstrained <- dplyr::bind_rows(first_pass) %>%
    dplyr::transmute(
      alt_player_uc      = .data$alt_player,
      alt_adj_proj_uc    = .data$alt_adj_proj,
      p_start_correct_uc = .data$p_start_correct,
      expected_regret_uc = .data$expected_regret,
      call_uc            = .data$call
    )

  # Greedy pass: highest-regret slot claims its alternative first.
  metric_rows_list <- vector("list", nrow(starters2))
  remaining <- alternatives
  for (i in order(first_pass_regret, decreasing = TRUE)) {
    row  <- starters2[i, ]
    cand <- remaining %>%
      dplyr::filter(.data$position %in% .eligible_positions_for_slot(row$slot))
    m <- metrics_for(row, cand)
    if (!is.na(m$alt_row)) {
      remaining <- remaining %>% dplyr::filter(.data$alt_row != m$alt_row)
    }
    metric_rows_list[[i]] <- dplyr::select(m, -alt_row)
  }
  metric_rows <- dplyr::bind_rows(metric_rows_list)

  if (nrow(metric_rows) != nrow(starters)) {
    stop(glue("analyze_start_sit(): metric row count ({nrow(metric_rows)}) ",
              "does not match starters ({nrow(starters)}); refusing to ",
              "bind_cols and misalign players."), call. = FALSE)
  }

  if (nrow(reassignment) != nrow(starters)) {
    stop(glue("analyze_start_sit(): reassignment row count ",
              "({nrow(reassignment)}) does not match starters ",
              "({nrow(starters)}); refusing to bind_cols and misalign ",
              "players."), call. = FALSE)
  }

  if (nrow(unconstrained) != nrow(starters)) {
    stop(glue("analyze_start_sit(): unconstrained row count ",
              "({nrow(unconstrained)}) does not match starters ",
              "({nrow(starters)}); refusing to bind_cols and misalign ",
              "players."), call. = FALSE)
  }

  enriched <- dplyr::bind_cols(
    starters,
    starters2 %>% dplyr::select(sigma_s, sigma_source),
    metric_rows,
    unconstrained,
    reassignment
  ) %>%
    dplyr::mutate(sigma_s = round(.data$sigma_s, 3),
                  uq_schema_tag = SCHEMA_TAG_UQ)

  # week_risk_score is an UPPER-BOUND HEURISTIC: per-slot expected regrets are
  # computed independently (each slot vs its own exclusively-assigned best
  # alternative) and summed, ignoring cross-slot correlation, so the true
  # joint risk is at most this sum.
  # Summed from the EXCLUSIVE column only. Summing expected_regret_uc here
  # would double count any bench player who is the best alternative for more
  # than one slot.
  week_risk_score <- sum(enriched$expected_regret, na.rm = TRUE)

  # ---- DEF streaming recommendation (open DEF slot) ----
  def_streaming <- NULL
  if (!has_def_starter && include_waiver_def) {
    have_waiver_inputs <- !is.null(source) && !is.null(vorp) &&
      !is.null(def_proj)
    if (have_waiver_inputs) {
      if (is.na(def_sigma)) {
        def_sigma <- .compute_def_sigma(seasons = def_seasons,
                                        scoring_settings = def_scoring_settings,
                                        cache_dir = cache_dir)
      }
      def_streaming <- .recommend_def_stream(
        source, reconciled, vorp, def_proj, def_sigma,
        week = week, dvp = dvp, def_factors = def_factors,
        season = season, cache_dir = cache_dir,
        max_suggestions = def_max_suggestions)
    } else {
      message("  DEF slot is open but source/vorp/def_proj not all supplied; ",
              "skipping the DEF streaming recommendation.")
    }
  }

  # ---- optional save ----
  # League and week are both in the filename so weekly runs across multiple
  # leagues accumulate rather than overwrite. That archive is the only route to
  # a real calibration set.
  if (save_output) {
    wk_tag <- if (is.null(week)) "pre" else sprintf("%02d", as.integer(week))
    lg_tag <- .league_tag(league_id)

    if (identical(lg_tag, LEAGUE_TAG_NONE)) {
      message("  save_output = TRUE with no league_id: writing under the ",
              "'", LEAGUE_TAG_NONE, "' tag. Runs for different leagues in the ",
              "same week WILL overwrite each other. Pass league_id to keep ",
              "them separate.")
    }

    uq_rds  <- file.path(R36_OUTPUT_DIR,
                         glue("s3_r36_{lg_tag}_w{wk_tag}_start_sit_uq.rds"))
    uq_csv  <- file.path(R36_OUTPUT_DIR,
                         glue("s3_r36_{lg_tag}_w{wk_tag}_start_sit_uq.csv"))
    def_rds <- file.path(R36_OUTPUT_DIR,
                         glue("s3_r36_{lg_tag}_w{wk_tag}_def_streaming.rds"))
    def_csv <- file.path(R36_OUTPUT_DIR,
                         glue("s3_r36_{lg_tag}_w{wk_tag}_def_streaming.csv"))

    dir.create(R36_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    saveRDS(enriched, uq_rds)
    readr::write_csv(enriched, uq_csv)
    message(glue("  Saved: {uq_rds}"))
    message(glue("  Saved: {uq_csv}"))
    if (!is.null(def_streaming) && nrow(def_streaming) > 0L) {
      saveRDS(def_streaming, def_rds)
      readr::write_csv(def_streaming, def_csv)
      message(glue("  Saved: {def_rds}"))
      message(glue("  Saved: {def_csv}"))
    }
  }

  # ---- KEY INSIGHTS (computed from the enriched output, never hardcoded) ----
  n_high   <- sum(enriched$stakes == "high", na.rm = TRUE)
  n_scored <- sum(!is.na(enriched$p_start_correct))
  n_swap   <- sum(enriched$call == "swap flagged", na.rm = TRUE)
  wk_label <- if (is.null(week)) "preseason" else glue("week {week}")

  src_tbl <- sort(table(enriched$sigma_source), decreasing = TRUE)
  src_txt <- paste(names(src_tbl), as.integer(src_tbl), sep = "=",
                   collapse = ", ")

  riskiest <- enriched %>%
    dplyr::filter(!is.na(.data$p_start_correct)) %>%
    dplyr::arrange(.data$p_start_correct) %>%
    dplyr::slice(1)

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/36: Start/sit uncertainty ({wk_label})"))
  message(glue("{strrep('=', 70)}"))
  message(glue("  Slots scored:        {n_scored} of {nrow(enriched)}"))
  message(glue("  High-stakes slots:   {n_high}"))
  message(glue("  Swap flagged:        {n_swap}"))
  message(glue("  Sigma sources:       {src_txt}"))
  message(glue("  Week risk score:     ",
               "{format(round(week_risk_score, 1), nsmall = 1)} pts at risk"))
  if (nrow(riskiest) == 1L) {
    message(glue("  Closest call:        {riskiest$player_name} ",
                 "({riskiest$slot}) at p={riskiest$p_start_correct} ",
                 "vs {riskiest$alt_player %||% 'NA'}"))
  }
  if (!is.null(def_streaming) && nrow(def_streaming) > 0L) {
    top <- def_streaming[1, ]
    opp_txt <- if (is.na(top$opponent)) "neutral" else glue("vs {top$opponent}")
    clarity_txt <- if (is.na(top$pick_clarity)) "only option" else
      top$pick_clarity
    message(glue("  DEF stream pick:     {top$def_team} {opp_txt} ",
                 "({format(round(top$adj_proj, 1), nsmall = 1)} pts, ",
                 "{clarity_txt})"))
  }
  message(glue("{strrep('=', 70)}\n"))

  lineup$starters        <- enriched
  lineup$week_risk_score <- week_risk_score
  lineup$def_streaming   <- def_streaming
  lineup
}
