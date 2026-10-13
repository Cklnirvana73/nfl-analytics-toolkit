# ==============================================================================
# 49_uncertainty_calibration_gate.R
# Season 3 Wave A2 -- R4: out-of-sample gate on the uncertainty layer
# NFL Analytics Toolkit
#
# NAVIGATION GUIDE
# Line  120 : .season_game_points()      -- per player-game points under a scoring
# Line  190 : .build_history_estimates() -- hist_mean / hist_sigma / n from priors
# Line  250 : .assemble_gate_frame()     -- join predictions to realized outcomes
# Line  330 : gate_tau()                 -- is tau a calibrated SE of the mean
# Line  400 : gate_omega()               -- does omega predict realized weekly SD
# Line  470 : gate_shrinkage_k()         -- pick k by OOS error, not by formula
# Line  540 : gate_weekly_pit()          -- PIT / coverage / skew on weekly outcomes
# Line  640 : gate_availability_bias()   -- size of the played-games-only bias
# Line  720 : run_uncertainty_gate()     -- runs all five, returns a list
#
# DESCRIPTION
# This file GATES the uncertainty layer built in Wave A2 R2/R3. It is a
# read-only diagnostic: it loads cached normalized pbp, rebuilds the SAME
# estimators R/29 uses from a history window, and scores them against a
# held-out target season. It writes no cache, edits no pipeline file, and is
# never sourced by the production path.
#
# WHY THIS EXISTS
# R2 established that posterior_sigma is tau, a standard error of a player's
# SEASON MEAN. R3 added omega, the within-season week-to-week dispersion, and
# sigma_week = sqrt(tau^2 + omega^2) as the predictive SD for one future week.
# None of those three has ever been tested against out-of-sample error. R6 is
# scheduled to delete five hand-set weights in R/33 on the strength of this
# layer, and replacing five undefended numbers with one undefended distribution
# would be no improvement at all. So the layer gets gated first.
#
# WHAT EACH GATE ANSWERS
#   tau        z = (realized season mean - predicted mean) / tau
#              A calibrated tau gives sd(z) = 1. Above 1 means tau is too
#              small and every interval on the board is too tight.
#   omega      ratio = realized within-season SD / predicted omega
#              A calibrated omega gives a ratio of 1 in the median.
#   k          The EB shrinkage constant. Method of moments produced 3.25 on
#              the 2026 board. This picks k by out-of-sample error instead and
#              reports whether the two agree.
#   PIT        u = F(realized weekly points | mu, sigma_week). If the
#              predictive distribution is right, u is Uniform(0,1). Departures
#              say WHICH way it is wrong: sd(z) sizes the miss, and PIT skew
#              is the direct evidence for or against R5's right-skewed form.
#   available  omega is computed only over games PLAYED. Missed weeks are
#              absent, not zero. This measures how much that overstates weekly
#              reliability, which determines whether R/48 stays a long-term
#              item or becomes a prerequisite for the best-ball board.
#
# SCOPE LIMIT, STATED RATHER THAN HIDDEN
# This gates the HISTORY-BASED estimators, which is where ground truth exists.
# It does NOT gate the translation-prior path (R/24 -> R/28 -> R/29), because
# reproducing a prior-season translation prior needs the full engine run for
# that season and the college caches to match. Translation-path calibration is
# a separate gate and must not be claimed on the back of this one. Coverage is
# reported explicitly in every table so the gap is visible.
#
# The role model in R/32 .build_omega() uses PROJECTED volume from R/31. This
# file cannot reproduce an R/31 allocation for a past season, so it gates the
# shrinkage mechanics and the omega estimator, not the role regression. What it
# CAN do is report whether omega error varies systematically with projection
# size, which is evidence about whether a role term is earning its place.
#
# DEPENDENCIES
# library(dplyr), library(tidyr), library(purrr), library(glue), library(here)
# library(nflreadr)
# Upstream: R/15_multi_season_pbp.R (load_normalized_season)
#           R/17_extended_scoring.R (calculate_fantasy_points_ext,
#                                    DK_BEST_BALL_SCORING)
# ==============================================================================

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(nflreadr)

source(here::here("R", "15_multi_season_pbp.R"))
source(here::here("R", "17_extended_scoring.R"))


# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

GATE_CACHE_DIR <- here::here("data", "season2_cache")

# Mirrors R/29's min_games = 8L. Below this a per-player SD is too noisy to
# either train on or be scored against.
GATE_MIN_GAMES_HISTORY <- 8L

# A realized within-season SD needs enough games to be a meaningful target.
# Deliberately lower than the history threshold: we want to SCORE players on
# thin target seasons too, because excluding them would select on availability
# and quietly reproduce the very bias gate_availability_bias() measures.
GATE_MIN_GAMES_TARGET <- 4L

# Regular-season games per team, 2021 onward. Used only as the availability
# denominator. Players who changed teams mid-season or were signed late will
# look artificially unavailable; that caveat is printed with the result.
GATE_TEAM_GAMES <- 17L

GATE_SUPPORTED_POSITIONS <- c("QB", "RB", "WR", "TE")

# Normal quantiles for coverage checks.
GATE_Z80 <- 1.281552
GATE_Z95 <- 1.959964

utils::globalVariables(c(
  "player_id", "position", "game_id", "game_points", "season",
  "total_fantasy_points", "n_games", "season_mean", "season_sigma",
  "week", "team",
  "hist_mean", "hist_sigma", "n_games_total", "n_seasons",
  "pred_mu", "tau", "tau_sampling", "change_sigma", "phi2", "pred_lag",
  "hist_centroid", "target_season", "lag", "d2", "samp", "n_pairs",
  "change_var", "n_pairs_total", "s_a", "s_b", "m_a", "m_b", "sd_a", "sd_b",
  "n_a", "n_b",
  "var_season_mean", "mean_sampling_var",
  "omega_raw", "omega_shrunk", "sigma_week",
  "real_mean", "real_sigma", "real_n", "z_mean", "omega_ratio",
  "u_pit", "z_week", "k", "rmse", "mu_bin", "grp", "n", "sd_z",
  "cover80", "cover95", "skew", "played", "missed", "sd_played",
  "sd_with_zeros", "inflation"
))


# ==============================================================================
# .season_game_points()
#
# Per player-game fantasy points for one season under a given scoring.
#
# Mirrors the aggregation inside R/29 .compute_nfl_history() exactly:
# calculate_fantasy_points_ext() then sum over player_id + position + game_id.
# Any divergence here would gate a different estimator than the one in
# production, which would make the whole file worthless, so this must stay in
# step with R/29 if that aggregation ever changes.
#
# @param s Integer season.
# @param scoring_settings List. R/17 schema.
# @param cache_dir Character.
# @return Tibble: player_id, position, game_id, season, game_points. NULL if
#   the season has no cache.
# ==============================================================================

.season_game_points <- function(s, scoring_settings, cache_dir = GATE_CACHE_DIR) {

  pbp <- tryCatch(
    load_normalized_season(s, cache_dir = cache_dir),
    error = function(e) NULL
  )
  if (is.null(pbp)) {
    message(glue("  season {s}: no normalized cache, skipping"))
    return(NULL)
  }

  roster <- nflreadr::load_rosters(seasons = s)

  fp <- do.call(calculate_fantasy_points_ext, c(
    list(pbp_data = pbp, roster_data = roster),
    scoring_settings
  ))

  # week and team are carried through for the correlation work in R/50. Adding
  # them to the grouping does not change the grain: game_id already fixes a
  # player-game, and both are constant within one. [2026-07-30]
  out <- fp %>%
    dplyr::group_by(player_id, position, game_id, week, team) %>%
    dplyr::summarise(game_points = sum(total_fantasy_points, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::filter(position %in% GATE_SUPPORTED_POSITIONS) %>%
    dplyr::mutate(season = as.integer(s))

  rm(pbp, fp)
  invisible(gc(verbose = FALSE))

  out
}


# ==============================================================================
# .build_history_estimates()
#
# Rebuilds R/29's history estimator over a window of prior seasons.
#
# hist_sigma is the MEAN OF WITHIN-SEASON SDs, matching R/29 line ~1743. That
# is a per-game dispersion (omega). tau is derived from it the way
# .blend_prior_components() does, as a standard error over the pooled game
# count. Keeping both derivations identical to production is the whole point:
# this gates what ships, not a cleaner reimplementation of it.
#
# @param game_pts Tibble from .season_game_points(), possibly several seasons.
# @param min_games Integer. Per-season qualifying threshold.
# @return Tibble: player_id, position, n_seasons, n_games_total, hist_mean,
#   hist_sigma, tau, omega_raw.
# ==============================================================================

.build_history_estimates <- function(game_pts, target_season,
                                      min_games = GATE_MIN_GAMES_HISTORY,
                                      change_by_pos = NULL) {

  if (is.null(change_by_pos)) {
    change_by_pos <- .estimate_change_sigma(game_pts, min_games = min_games)
  }

  per_season <- game_pts %>%
    dplyr::group_by(player_id, position, season) %>%
    dplyr::summarise(
      n_games      = dplyr::n_distinct(game_id),
      season_mean  = mean(game_points, na.rm = TRUE),
      season_sigma = stats::sd(game_points, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_games >= min_games)

  per_season %>%
    dplyr::group_by(player_id, position) %>%
    dplyr::summarise(
      n_seasons     = dplyr::n(),
      n_games_total = as.integer(sum(n_games, na.rm = TRUE)),
      hist_mean     = mean(season_mean, na.rm = TRUE),
      hist_sigma    = mean(season_sigma, na.rm = TRUE),
      hist_centroid = mean(season, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      !is.na(hist_mean), !is.na(hist_sigma), hist_sigma > 0,
      n_games_total >= 1L
    ) %>%
    dplyr::mutate(
      pred_mu   = hist_mean,
      omega_raw = hist_sigma,
      # Sampling-only tau, matching .blend_prior_components() BEFORE the
      # 2026-07-30 change-variance fix. Retained so the gate can report the
      # before and after side by side.
      tau_sampling = hist_sigma / sqrt(n_games_total)
    ) %>%
    dplyr::left_join(change_by_pos, by = "position") %>%
    dplyr::mutate(
      phi2         = dplyr::coalesce(phi2, 0),
      pred_lag     = pmax(target_season - hist_centroid, 0),
      change_sigma = sqrt(phi2 * pred_lag),
      # Post-fix tau. A player's true per-game mean moves between seasons, and
      # that movement does not shrink with more games, so it caps how certain a
      # long career can make us.
      tau          = sqrt(tau_sampling^2 + change_sigma^2)
    )
}


# ==============================================================================
# .estimate_change_sigma()
#
# Season-to-season change variance per position, mirroring the estimator added
# to R/29 .compute_nfl_history() on 2026-07-30.
#
#   Var(season_mean within a player) = change_sigma^2 + mean(season_sigma^2 / n)
#
# The second term is what pure sampling would produce, so the remainder is real
# movement in ability. Floored at zero: a negative variance estimate is invalid,
# not a smaller correction.
#
# @param game_pts Tibble from .season_game_points() across the history window.
# @param min_games Integer.
# @return Tibble: position, n_change, change_sigma.
# ==============================================================================

.estimate_change_sigma <- function(game_pts, min_games = GATE_MIN_GAMES_HISTORY) {

  per_season <- game_pts %>%
    dplyr::group_by(player_id, position, season) %>%
    dplyr::summarise(
      n_games      = dplyr::n_distinct(game_id),
      season_mean  = mean(game_points, na.rm = TRUE),
      season_sigma = stats::sd(game_points, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_games >= min_games)

  h_a <- per_season %>%
    dplyr::select(player_id, position, s_a = season, n_a = n_games,
                   m_a = season_mean, sd_a = season_sigma)
  h_b <- per_season %>%
    dplyr::select(player_id, position, s_b = season, n_b = n_games,
                   m_b = season_mean, sd_b = season_sigma)

  pairs <- dplyr::inner_join(h_a, h_b, by = c("player_id", "position"),
                              relationship = "many-to-many") %>%
    dplyr::filter(s_b > s_a) %>%
    dplyr::mutate(
      lag  = as.integer(s_b - s_a),
      d2   = (m_b - m_a)^2,
      samp = sd_a^2 / pmax(n_a, 1L) + sd_b^2 / pmax(n_b, 1L)
    ) %>%
    dplyr::filter(is.finite(d2), is.finite(samp))

  lag_tbl <- pairs %>%
    dplyr::group_by(position, lag) %>%
    dplyr::summarise(
      n_pairs    = dplyr::n(),
      change_var = pmax(mean(d2, na.rm = TRUE) - mean(samp, na.rm = TRUE), 0),
      .groups = "drop"
    )

  lag_tbl %>%
    dplyr::group_by(position) %>%
    dplyr::summarise(
      n_pairs_total = sum(n_pairs),
      phi2 = pmax(sum(n_pairs * lag * change_var) /
                    pmax(sum(n_pairs * lag^2), 1e-9), 0),
      .groups = "drop"
    )
}


# ==============================================================================
# .assemble_gate_frame()
#
# Joins predictions built from the history window to realized target-season
# outcomes. One row per player who appears in both.
#
# @param hist_est Tibble from .build_history_estimates().
# @param target_pts Tibble from .season_game_points() for the target season.
# @param k Numeric. EB shrinkage constant applied to omega.
# @param min_games_target Integer.
# @return Tibble with prediction and realized columns plus derived diagnostics.
# ==============================================================================

.assemble_gate_frame <- function(hist_est, target_pts, k,
                                  min_games_target = GATE_MIN_GAMES_TARGET) {

  realized <- target_pts %>%
    dplyr::group_by(player_id, position) %>%
    dplyr::summarise(
      real_n     = dplyr::n_distinct(game_id),
      real_mean  = mean(game_points, na.rm = TRUE),
      real_sigma = stats::sd(game_points, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(real_n >= min_games_target, !is.na(real_sigma))

  hist_est %>%
    dplyr::inner_join(realized, by = c("player_id", "position")) %>%
    dplyr::mutate(
      # Shrink toward the position mean of omega. This is deliberately the
      # SIMPLE target (fallback option B), not R/32's role regression, because
      # an R/31 allocation cannot be reproduced for a past season. What is
      # gated here is the shrinkage mechanics and the estimator, not the role
      # model. Stated again at the call site so it cannot be misread.
      omega_shrunk = NA_real_,
      z_mean       = (real_mean - pred_mu) / tau,
      omega_ratio  = real_sigma / omega_raw
    )
}

.apply_position_shrinkage <- function(df, k) {
  pos_mean <- df %>%
    dplyr::group_by(position) %>%
    dplyr::summarise(pos_omega = mean(omega_raw, na.rm = TRUE), .groups = "drop")

  df %>%
    dplyr::left_join(pos_mean, by = "position") %>%
    dplyr::mutate(
      w            = n_games_total / (n_games_total + k),
      omega_shrunk = w * omega_raw + (1 - w) * pos_omega,
      sigma_week   = sqrt(tau^2 + omega_shrunk^2)
    )
}


# ==============================================================================
# gate_tau()
#
# Is tau a calibrated standard error of the season mean?
#
# z = (realized season mean - predicted mean) / tau, and a calibrated tau gives
# sd(z) = 1. Expect a value well above 1: tau captures SAMPLING error in the
# historical mean only, and says nothing about a player's true ability actually
# CHANGING between seasons (age, role, scheme, injury). Both sources are real,
# so a large sd(z) here is not automatically a bug in tau. It is a measurement
# of how much of the season-to-season move tau was never designed to see, and
# that number is what a projection interval on the board should reflect.
#
# @param gf Tibble from .assemble_gate_frame() + .apply_position_shrinkage().
# @return Tibble of sd(z) and coverage by group, plus an overall row.
# ==============================================================================

gate_tau <- function(gf) {

  .summ <- function(d, label) {
    tibble::tibble(
      grp       = label,
      n         = nrow(d),
      chg_sigma = round(stats::median(d$change_sigma, na.rm = TRUE), 3),
      med_lag   = round(stats::median(d$pred_lag, na.rm = TRUE), 2),
      # Sampling-only tau: what shipped before 2026-07-30.
      sd_z_old  = round(stats::sd((d$real_mean - d$pred_mu) / d$tau_sampling,
                                   na.rm = TRUE), 3),
      # With the change-variance component. Target is 1.000.
      sd_z_new  = round(stats::sd(d$z_mean, na.rm = TRUE), 3),
      mean_z    = round(mean(d$z_mean, na.rm = TRUE), 3),
      cover80   = round(100 * mean(abs(d$z_mean) <= GATE_Z80, na.rm = TRUE), 1),
      cover95   = round(100 * mean(abs(d$z_mean) <= GATE_Z95, na.rm = TRUE), 1)
    )
  }

  by_pos <- purrr::map_dfr(sort(unique(gf$position)), function(p) {
    .summ(dplyr::filter(gf, position == p), p)
  })

  dplyr::bind_rows(.summ(gf, "ALL"), by_pos)
}


# ==============================================================================
# gate_omega()
#
# Does omega predict realized within-season weekly dispersion?
#
# ratio = realized within-season SD / predicted omega. A calibrated omega gives
# a median ratio of 1. Reported as a median rather than a mean because the
# ratio is bounded below by zero and right-skewed, so a mean would be dragged
# by a handful of players whose predicted omega was near zero.
#
# Also reports the ratio by projection decile. If the ratio drifts with
# projection size, then a role or volume term is carrying real signal and
# R/32's role regression is earning its place. If it is flat, the role model is
# doing nothing and option B (shrink to a position mean) is sufficient.
#
# @param gf Tibble.
# @return List of two tibbles: by_position, by_mu_decile.
# ==============================================================================

gate_omega <- function(gf) {

  by_position <- gf %>%
    dplyr::group_by(position) %>%
    dplyr::summarise(
      n            = dplyr::n(),
      med_ratio    = round(stats::median(omega_ratio, na.rm = TRUE), 3),
      q25          = round(stats::quantile(omega_ratio, 0.25, na.rm = TRUE), 3),
      q75          = round(stats::quantile(omega_ratio, 0.75, na.rm = TRUE), 3),
      med_pred     = round(stats::median(omega_shrunk, na.rm = TRUE), 2),
      med_real     = round(stats::median(real_sigma, na.rm = TRUE), 2),
      .groups = "drop"
    )

  by_mu_decile <- gf %>%
    dplyr::mutate(mu_bin = dplyr::ntile(pred_mu, 10)) %>%
    dplyr::group_by(mu_bin) %>%
    dplyr::summarise(
      n         = dplyr::n(),
      med_mu    = round(stats::median(pred_mu, na.rm = TRUE), 2),
      med_ratio = round(stats::median(omega_ratio, na.rm = TRUE), 3),
      med_pred  = round(stats::median(omega_shrunk, na.rm = TRUE), 2),
      med_real  = round(stats::median(real_sigma, na.rm = TRUE), 2),
      .groups = "drop"
    )

  list(by_position = by_position, by_mu_decile = by_mu_decile)
}


# ==============================================================================
# gate_shrinkage_k()
#
# Picks the EB shrinkage constant by out-of-sample error rather than by formula.
#
# R/32 derives k by method of moments and produced 3.25 on the 2026 board. A
# small k means omega_raw is trusted heavily even at 8 games, which is a strong
# claim about between-player differences in weekly volatility. This sweeps k and
# reports the value minimising RMSE against realized within-season SD.
#
# If the OOS optimum is far from the moment estimate, the moment estimate is
# the one to distrust: its variance formula assumes an SD's sampling behaviour
# that is shaky at eight games.
#
# @param gf Tibble BEFORE shrinkage is applied.
# @param k_grid Numeric vector of candidate k.
# @return Tibble: k, rmse, mae, and a flag on the chosen value.
# ==============================================================================

gate_shrinkage_k <- function(gf, k_grid = c(0, 1, 2, 3.25, 5, 8, 12, 16,
                                             24, 32, 48, 64, 100)) {

  purrr::map_dfr(k_grid, function(kk) {
    d <- .apply_position_shrinkage(gf, kk)
    err <- d$omega_shrunk - d$real_sigma
    tibble::tibble(
      k    = kk,
      n    = sum(!is.na(err)),
      rmse = round(sqrt(mean(err^2, na.rm = TRUE)), 4),
      mae  = round(mean(abs(err), na.rm = TRUE), 4)
    )
  }) %>%
    dplyr::mutate(best = dplyr::if_else(rmse == min(rmse, na.rm = TRUE),
                                         "<-- OOS min", ""))
}


# ==============================================================================
# gate_weekly_pit()
#
# The central gate. Probability Integral Transform on WEEKLY outcomes.
#
# For every player-week in the target season:
#   u = pnorm(actual_week_points, mean = pred_mu, sd = sigma_week)
# If the predictive distribution is correct, u is Uniform(0, 1) and
# z = (actual - pred_mu) / sigma_week is standard normal.
#
# Three readings, each pointing at a different fix:
#   sd(z) far from 1        -> sigma_week is the wrong SIZE
#   coverage below nominal  -> intervals too narrow, same conclusion
#   PIT skew                -> sigma_week is the wrong SHAPE, and a symmetric
#                              normal cannot be patched by rescaling. This is
#                              the direct evidence for or against R5.
#
# A right-skewed weekly distribution shows up as a u histogram with too much
# mass low and a thin tail high, because a symmetric normal under-predicts big
# weeks and over-predicts the frequency of moderately-bad ones. Deciles are
# printed so the shape is visible rather than summarised away.
#
# @param gf Tibble with pred_mu and sigma_week.
# @param target_pts Tibble of player-game points for the target season.
# @return List: summary tibble, decile tibble, by-position tibble.
# ==============================================================================

gate_weekly_pit <- function(gf, target_pts) {

  wk <- target_pts %>%
    dplyr::inner_join(
      gf %>% dplyr::select(player_id, position, pred_mu, sigma_week),
      by = c("player_id", "position")
    ) %>%
    dplyr::filter(!is.na(sigma_week), sigma_week > 0) %>%
    dplyr::mutate(
      z_week = (game_points - pred_mu) / sigma_week,
      u_pit  = stats::pnorm(z_week)
    )

  .skew <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 3) return(NA_real_)
    m <- mean(x); s <- stats::sd(x)
    if (!is.finite(s) || s == 0) return(NA_real_)
    mean((x - m)^3) / s^3
  }

  summary_tbl <- tibble::tibble(
    n_player_weeks = nrow(wk),
    n_players      = dplyr::n_distinct(wk$player_id),
    sd_z           = round(stats::sd(wk$z_week, na.rm = TRUE), 3),
    mean_z         = round(mean(wk$z_week, na.rm = TRUE), 3),
    skew_z         = round(.skew(wk$z_week), 3),
    cover80        = round(100 * mean(abs(wk$z_week) <= GATE_Z80, na.rm = TRUE), 1),
    cover95        = round(100 * mean(abs(wk$z_week) <= GATE_Z95, na.rm = TRUE), 1)
  )

  # A perfectly calibrated model puts 10 percent of mass in each decile.
  decile_tbl <- wk %>%
    dplyr::mutate(bin = cut(u_pit, breaks = seq(0, 1, 0.1),
                             include.lowest = TRUE, labels = FALSE)) %>%
    dplyr::count(bin, name = "n") %>%
    dplyr::mutate(pct      = round(100 * n / sum(n), 2),
                   expected = 10.00,
                   dev      = round(pct - 10.00, 2))

  by_pos <- wk %>%
    dplyr::group_by(position) %>%
    dplyr::summarise(
      n_weeks = dplyr::n(),
      sd_z    = round(stats::sd(z_week, na.rm = TRUE), 3),
      skew_z  = round(.skew(z_week), 3),
      cover80 = round(100 * mean(abs(z_week) <= GATE_Z80, na.rm = TRUE), 1),
      .groups = "drop"
    )

  list(summary = summary_tbl, deciles = decile_tbl, by_position = by_pos)
}


# ==============================================================================
# gate_availability_bias()
#
# How much does measuring omega over PLAYED games only overstate reliability?
#
# omega_raw is built from games a player appeared in. Missed weeks are absent
# rather than zero. In best ball a missed week is a real cost: that roster slot
# produces nothing from that player. So the dispersion that matters for
# best-ball value is the one INCLUDING zeros for missed weeks.
#
# This compares, in the target season:
#   sd_played      SD over games played (what omega currently estimates)
#   sd_with_zeros  SD over all GATE_TEAM_GAMES weeks, missed weeks as zero
#   inflation      sd_with_zeros / sd_played
#
# CAVEAT, and it is not small: players signed mid-season, traded, or on a team
# with a different game count will look artificially unavailable, because the
# denominator is a flat GATE_TEAM_GAMES. This OVERSTATES the bias. Treat the
# result as an upper bound on the effect, and as a decision input for whether
# R/48 stops being a long-term queue item, not as a correction factor to apply.
#
# @param gf Tibble.
# @param target_pts Tibble.
# @return List: summary tibble and a by-missed-games tibble.
# ==============================================================================

gate_availability_bias <- function(gf, target_pts) {

  per_player <- target_pts %>%
    dplyr::semi_join(gf, by = c("player_id", "position")) %>%
    dplyr::group_by(player_id, position) %>%
    dplyr::summarise(
      played    = dplyr::n_distinct(game_id),
      sd_played = stats::sd(game_points, na.rm = TRUE),
      pts       = list(game_points),
      .groups = "drop"
    ) %>%
    dplyr::filter(played >= GATE_MIN_GAMES_TARGET, !is.na(sd_played),
                   sd_played > 0) %>%
    dplyr::mutate(
      missed = pmax(GATE_TEAM_GAMES - played, 0L),
      sd_with_zeros = purrr::map2_dbl(pts, missed, function(v, m) {
        stats::sd(c(v, rep(0, m)), na.rm = TRUE)
      }),
      inflation = sd_with_zeros / sd_played
    )

  summary_tbl <- tibble::tibble(
    n              = nrow(per_player),
    med_played     = stats::median(per_player$played),
    med_missed     = stats::median(per_player$missed),
    med_sd_played  = round(stats::median(per_player$sd_played), 3),
    med_sd_zeros   = round(stats::median(per_player$sd_with_zeros), 3),
    med_inflation  = round(stats::median(per_player$inflation), 3),
    pct_over_1_25  = round(100 * mean(per_player$inflation > 1.25), 1)
  )

  by_missed <- per_player %>%
    dplyr::mutate(missed_bin = dplyr::case_when(
      missed == 0L          ~ "0",
      missed <= 2L          ~ "1-2",
      missed <= 5L          ~ "3-5",
      TRUE                  ~ "6+"
    )) %>%
    dplyr::group_by(missed_bin) %>%
    dplyr::summarise(
      n             = dplyr::n(),
      med_inflation = round(stats::median(inflation), 3),
      med_sd_played = round(stats::median(sd_played), 2),
      med_sd_zeros  = round(stats::median(sd_with_zeros), 2),
      .groups = "drop"
    )

  list(summary = summary_tbl, by_missed = by_missed)
}


# ==============================================================================
# run_uncertainty_gate()
#
# Runs every gate and prints a report.
#
# @param target_season Integer. Held-out season scored against.
# @param history_seasons Integer vector. Defaults to the three seasons before
#   target_season, matching R/29's window.
# @param scoring_settings List. Defaults to DK best ball, since this layer is
#   being gated for the best-ball board. Pass another scoring to gate that one.
# @param k Numeric. Shrinkage constant used for the PIT and omega gates.
#   Defaults to R/32's moment estimate so the report reflects what ships.
# @param cache_dir Character.
# @param verbose Logical.
# @return Invisible list of every gate result plus the assembled frame.
# ==============================================================================

run_uncertainty_gate <- function(target_season   = 2025L,
                                  history_seasons = NULL,
                                  scoring_settings = DK_BEST_BALL_SCORING,
                                  k               = 3.25,
                                  cache_dir       = GATE_CACHE_DIR,
                                  verbose         = TRUE) {

  target_season <- as.integer(target_season)
  if (is.null(history_seasons)) {
    history_seasons <- (target_season - 3L):(target_season - 1L)
  }
  history_seasons <- as.integer(history_seasons)

  if (any(history_seasons >= target_season)) {
    stop("run_uncertainty_gate(): history_seasons must all precede ",
         "target_season. Overlap would leak the outcome into the ",
         "prediction and every gate below would report a fantasy.",
         call. = FALSE)
  }

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/49 UNCERTAINTY CALIBRATION GATE"))
  message(glue("Target season : {target_season}"))
  message(glue("History window: {paste(history_seasons, collapse = ', ')}"))
  message(glue("Shrinkage k   : {k}"))
  message(glue("{strrep('=', 70)}"))

  message("\nSTEP 1/5: Loading history seasons")
  hist_pts <- purrr::map_dfr(history_seasons, function(s) {
    .season_game_points(s, scoring_settings, cache_dir = cache_dir)
  })
  if (nrow(hist_pts) == 0) {
    stop("run_uncertainty_gate(): no history seasons could be loaded from ",
         cache_dir, ". Nothing can be gated.", call. = FALSE)
  }
  loaded_hist <- sort(unique(hist_pts$season))
  if (!identical(as.integer(loaded_hist), sort(history_seasons))) {
    message(glue("  WARNING: requested {length(history_seasons)} history ",
                 "season(s), loaded {length(loaded_hist)} ",
                 "({paste(loaded_hist, collapse = ', ')}). ",
                 "Estimates rest on a shorter window than production uses."))
  }

  message("\nSTEP 2/5: Loading target season")
  target_pts <- .season_game_points(target_season, scoring_settings,
                                     cache_dir = cache_dir)
  if (is.null(target_pts) || nrow(target_pts) == 0) {
    stop("run_uncertainty_gate(): target season ", target_season,
         " has no normalized cache. Cannot score anything.", call. = FALSE)
  }

  message("\nSTEP 3/5: Building history estimates and joining outcomes")
  hist_est <- .build_history_estimates(hist_pts, target_season = target_season)
  gf_raw   <- .assemble_gate_frame(hist_est, target_pts, k = k)

  if (nrow(gf_raw) == 0) {
    stop("run_uncertainty_gate(): no players appear in both the history ",
         "window and the target season. Check the caches.", call. = FALSE)
  }

  gf <- .apply_position_shrinkage(gf_raw, k)

  message(glue("  players with history estimates : {nrow(hist_est)}"))
  message(glue("  scored against target season   : {nrow(gf)}"))
  message(glue("  COVERAGE NOTE: history-based paths only. The translation ",
               "prior path is NOT gated here."))

  message("\nSTEP 4/5: Running gates")
  res_tau   <- gate_tau(gf)
  res_omega <- gate_omega(gf)
  res_k     <- gate_shrinkage_k(gf_raw)
  res_pit   <- gate_weekly_pit(gf, target_pts)
  res_avail <- gate_availability_bias(gf, target_pts)

  message("\nSTEP 5/5: Report")

  if (verbose) {
    cat("\n", strrep("-", 70), "\n", sep = "")
    cat("GATE 1: tau as a standard error of the season mean\n")
    cat("sd_z_old = sampling-only tau. sd_z_new = with change variance.\n")
    cat("Calibrated tau gives sd_z_new = 1.000.\n")
    cat(strrep("-", 70), "\n", sep = "")
    print(as.data.frame(res_tau))

    cat("\n", strrep("-", 70), "\n", sep = "")
    cat("GATE 2: omega against realized weekly dispersion\n")
    cat("Calibrated omega gives med_ratio = 1.\n")
    cat(strrep("-", 70), "\n", sep = "")
    print(as.data.frame(res_omega$by_position))
    cat("\nBy projection decile. A DRIFTING ratio means a role/volume term\n")
    cat("carries real signal. A FLAT ratio means the role model is idle and\n")
    cat("shrinking to a position mean would do the same job.\n")
    print(as.data.frame(res_omega$by_mu_decile))

    cat("\n", strrep("-", 70), "\n", sep = "")
    cat("GATE 3: shrinkage k by out-of-sample error\n")
    cat("R/32's method-of-moments estimate was 3.25.\n")
    cat(strrep("-", 70), "\n", sep = "")
    print(as.data.frame(res_k))

    cat("\n", strrep("-", 70), "\n", sep = "")
    cat("GATE 4: weekly PIT. THE CENTRAL GATE.\n")
    cat("sd_z far from 1 = wrong SIZE. skew_z far from 0 = wrong SHAPE,\n")
    cat("which rescaling cannot fix and R5 exists to address.\n")
    cat(strrep("-", 70), "\n", sep = "")
    print(as.data.frame(res_pit$summary))
    cat("\nPIT deciles. Perfect calibration = 10.00 percent in every bin.\n")
    print(as.data.frame(res_pit$deciles))
    cat("\nBy position:\n")
    print(as.data.frame(res_pit$by_position))

    cat("\n", strrep("-", 70), "\n", sep = "")
    cat("GATE 5: availability bias (played-games-only omega)\n")
    cat("UPPER BOUND, not a correction factor. Flat 17-game denominator\n")
    cat("counts mid-season signings and trades as missed time.\n")
    cat(strrep("-", 70), "\n", sep = "")
    print(as.data.frame(res_avail$summary))
    print(as.data.frame(res_avail$by_missed))
    cat("\n", strrep("-", 70), "\n", sep = "")
  }

  invisible(list(
    frame        = gf,
    tau          = res_tau,
    omega        = res_omega,
    shrinkage_k  = res_k,
    pit          = res_pit,
    availability = res_avail,
    meta         = list(target_season = target_season,
                        history_seasons = loaded_hist,
                        k = k,
                        n_scored = nrow(gf))
  ))
}
