# ==============================================================================
# 43_efficiency_gate.R
# ==============================================================================
#
# PURPOSE (Wave A2 Item 6.5)
# --------------------------
# Diagnostic that decides, per position, whether an NGS efficiency metric earns
# a place in the projection over the incumbent R/16 signal. It does NOT modify
# any model. It scores candidate metrics on out-of-sample error and returns a
# table; wiring winners into R/29->R/32 is a separate, later step.
#
# METHOD
#   TARGET   next-season fantasy PPG under a SUPPLIED scoring ruleset
#            (resolver offense params, DK preset, or any calculate_fantasy_
#            points_ext() list). No scoring is hardcoded here.
#   FEATURES trailing-window, volume-weighted, ending the season before target:
#              base      = trailing PPG (games-weighted)
#              incumbent = R/16 efficiency (per-position, own-volume weighted)
#              ngs       = R/41 volume-weighted NGS panel for the target season
#   MODELS   per position, on IDENTICAL rows (complete cases across all three):
#              (a) target ~ base
#              (b) target ~ base + incumbent      [skipped for TE: no incumbent]
#              (c) target ~ base + ngs
#   OOS      forward-chaining: train target years < ty, predict ty. Pool
#            residuals across folds. Report RMSE, MAE, n, fold count.
#
# HONEST LIMITS
#   NGS coverage is minimum-attempt gated, so the common-support rows per
#   position-year are thin (~40 QB / 50 RB / 120 WR+TE per year before the
#   join). Fold counts and n are returned so the noise is visible.
#
# DEPENDENCIES (guarded-sourced)
#   R/16_player_season_panel.R : build_player_season_panel()
#   R/17_extended_scoring.R    : calculate_fantasy_points_ext()
#   R/41_ngs_veteran_panel.R   : build_ngs_veteran_panel()
#   nflreadr : load_pbp()
#
# VERSION
#   1.0  Initial build. Diagnostic only, no model wiring.
#   1.1  Added robustness_rb_ngs(): per-fold + bootstrap CI on the RB result.
#   1.2  Added the R/32 Level 2 pre-gate (Stage A of the two-stage C gate).
#        Answers ONE question: does per-player empirical-Bayes efficiency beat
#        position-average efficiency at converting volume to PPG, out of sample?
#        It is the kill-switch for R/32 Level 2. See the L2 section below for the
#        full method and the dated design rationale. Diagnostic only: it wires
#        nothing into R/29 -> R/32. If Stage A clears, Stage B measures the same
#        swap at the blended r32_posterior_mu. If Stage A does not clear, there
#        is no Stage B and no Level 2.
#   1.3  Added Stage B-lite: measures the a2 efficiency swap at the blended
#        r32 number using real R/32 weights, a leakage-safe trailing-PPG anchor
#        in place of a historical R/29 rebuild, and a weight sweep so blend
#        dilution is visible. Still diagnostic; wires nothing in.
# ==============================================================================

library(dplyr)
library(tidyr)
library(tibble)
library(glue)
library(here)
library(nflreadr)

source(here::here("R", "16_player_season_panel.R"))
source(here::here("R", "17_extended_scoring.R"))
source(here::here("R", "41_ngs_veteran_panel.R"))

for (fn in c("build_player_season_panel", "calculate_fantasy_points_ext",
             "build_ngs_veteran_panel")) {
  if (!exists(fn)) {
    stop(glue("43_efficiency_gate.R requires {fn}(), not found after sourcing."))
  }
}


# ------------------------------------------------------------------------------
# PER-POSITION METRIC MAP (confirmed against live R/16 and R/41 column names)
# ------------------------------------------------------------------------------
# Each incumbent entry: metric column in the R/16 panel + its own volume column.
# Each ngs entry: column in the R/41 panel (already volume-weighted upstream).

GATE_INCUMBENT <- list(
  QB = list(c(col = "mean_cpoe",            w = "qb_dropbacks"),
            c(col = "pass_epa_per_dropback", w = "qb_dropbacks")),
  RB = list(c(col = "rush_success_rate",    w = "rush_attempts"),
            c(col = "rec_epa_per_target",   w = "targets")),
  WR = list(c(col = "rec_epa_per_target",   w = "targets")),
  TE = list()  # no clean incumbent signal: TE is judged base vs base+NGS
)

GATE_NGS <- list(
  QB = "ngs_cpoe_vw",
  RB = c("ngs_ryoe_att_vw", "ngs_rush_eff_vw"),
  WR = c("ngs_separation_vw", "ngs_yac_oe_vw"),
  TE = c("ngs_separation_vw", "ngs_yac_oe_vw")
)


# ------------------------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------------------------

# Trailing-window volume-weighted mean of one metric, per player, ending the
# season before `target_year`.
.trailing_vw <- function(df, value_col, weight_col, target_year, window) {
  win <- (target_year - window):(target_year - 1L)
  d <- df[df$season %in% win, c("player_id", "season", value_col, weight_col)]
  names(d) <- c("player_id", "season", "v", "w")
  d <- d[!is.na(d$v) & !is.na(d$w) & d$w > 0, , drop = FALSE]
  if (nrow(d) == 0L) {
    return(tibble(player_id = character(), vw = numeric()))
  }
  d %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(vw = sum(v * w) / sum(w), .groups = "drop")
}

# Latest known R/16 position for each player within the trailing window.
.trailing_position <- function(panel, target_year, window) {
  win <- (target_year - window):(target_year - 1L)
  panel[panel$season %in% win, c("player_id", "season", "position")] %>%
    dplyr::filter(!is.na(position)) %>%
    dplyr::group_by(player_id) %>%
    dplyr::slice_max(season, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(player_id, position)
}


# ------------------------------------------------------------------------------
# build_scored_ppg  (the heavy, cacheable step)
# ------------------------------------------------------------------------------

#' Score multi-season pbp under one ruleset and return player-season PPG.
#'
#' @param seasons Integer vector of seasons to score.
#' @param scoring_params Named list of calculate_fantasy_points_ext() params
#'   (e.g. resolver offense params, or a DK preset).
#' @param min_games Integer. Minimum games for a player-season to be kept as a
#'   TARGET. Feature-window seasons are not filtered here. Default 4L.
#' @param cache_path Character or NULL. If given, saved/loaded as .rds.
#' @param verbose Logical.
#'
#' @return tibble(player_id, season, games, ppg).
build_scored_ppg <- function(seasons,
                             scoring_params,
                             min_games  = 4L,
                             cache_path = NULL,
                             verbose    = TRUE) {

  if (!is.null(cache_path) && file.exists(cache_path)) {
    if (verbose) message(glue("build_scored_ppg(): loading cache {cache_path}"))
    return(readRDS(cache_path))
  }

  ppg_list <- lapply(seasons, function(s) {
    if (verbose) message(glue("  scoring {s}..."))
    pbp <- nflreadr::load_pbp(s)
    fp  <- do.call(
      calculate_fantasy_points_ext,
      c(list(pbp_data = pbp, season = s), scoring_params)
    )
    fp %>%
      dplyr::group_by(player_id, season) %>%
      dplyr::summarise(
        games = dplyr::n_distinct(game_id),
        ppg   = sum(total_fantasy_points, na.rm = TRUE) / dplyr::n_distinct(game_id),
        .groups = "drop"
      )
  })

  ppg <- dplyr::bind_rows(ppg_list)

  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(ppg, cache_path)
    if (verbose) message(glue("build_scored_ppg(): cached -> {cache_path}"))
  }

  ppg
}


# ------------------------------------------------------------------------------
# assemble_gate_frame  (feature/target rows, one per player-target-year)
# ------------------------------------------------------------------------------

#' Build the modeling frame the gate fits on.
#'
#' @param ppg Output of build_scored_ppg() (target ruleset).
#' @param panel Output of build_player_season_panel() (incumbent features).
#' @param target_years Integer vector of TARGET seasons to include. Each needs
#'   a full `window`-season feature history within NGS coverage (2016+).
#' @param window Integer trailing window. Default 3L (mirror
#'   VETERAN_WINDOW_SEASONS).
#' @param min_games Integer target-season games floor. Default 4L.
#' @param verbose Logical.
#'
#' @return tibble with player_id, position, target_year, base_ppg, target_ppg,
#'   incumbent columns (inc_*), and NGS columns.
assemble_gate_frame <- function(ppg, panel, target_years,
                                window = 3L, min_games = 4L, verbose = TRUE) {

  frames <- lapply(target_years, function(ty) {
    if (verbose) message(glue("  assembling target {ty}..."))

    base <- .trailing_vw(ppg, "ppg", "games", ty, window) %>%
      dplyr::rename(base_ppg = vw)

    pos <- .trailing_position(panel, ty, window)

    # incumbent metrics for every position, own-volume weighted
    inc_specs <- unique(unlist(lapply(GATE_INCUMBENT, function(l) {
      vapply(l, function(x) x[["col"]], character(1))
    })))
    inc_tbl <- base["player_id"]
    for (l in GATE_INCUMBENT) {
      for (spec in l) {
        nm <- paste0("inc_", spec[["col"]])
        if (nm %in% names(inc_tbl)) next
        v <- .trailing_vw(panel, spec[["col"]], spec[["w"]], ty, window) %>%
          dplyr::rename(!!nm := vw)
        inc_tbl <- dplyr::full_join(inc_tbl, v, by = "player_id")
      }
    }

    ngs <- build_ngs_veteran_panel(target_season = ty, window_seasons = window,
                                   verbose = FALSE) %>%
      dplyr::select(player_id, dplyr::any_of(unique(unlist(GATE_NGS))))

    target <- ppg %>%
      dplyr::filter(season == ty, games >= min_games) %>%
      dplyr::transmute(player_id, target_ppg = ppg)

    pos %>%
      dplyr::inner_join(base,    by = "player_id") %>%
      dplyr::left_join(inc_tbl,  by = "player_id") %>%
      dplyr::left_join(ngs,      by = "player_id") %>%
      dplyr::inner_join(target,  by = "player_id") %>%
      dplyr::mutate(target_year = ty)
  })

  dplyr::bind_rows(frames)
}


# ------------------------------------------------------------------------------
# run_efficiency_gate  (forward-chained OOS, per position)
# ------------------------------------------------------------------------------

#' Fit base / base+incumbent / base+NGS per position, forward-chained OOS.
#'
#' @param frame Output of assemble_gate_frame().
#' @param min_train Integer. Minimum training rows to fit a fold. Default 15L.
#' @param verbose Logical.
#'
#' @return tibble(position, model, rmse, mae, n, folds) where model is one of
#'   base / incumbent / ngs. Rows within a position are scored on the same
#'   common-support players so the comparison is fair.
run_efficiency_gate <- function(frame, min_train = 15L, verbose = TRUE) {

  out <- list()

  for (p in c("QB", "RB", "WR", "TE")) {
    fp <- frame[frame$position == p, , drop = FALSE]
    if (nrow(fp) == 0L) next

    inc_cols <- paste0("inc_", vapply(GATE_INCUMBENT[[p]],
                                      function(x) x[["col"]], character(1)))
    inc_cols <- inc_cols[inc_cols %in% names(fp)]
    ngs_cols <- GATE_NGS[[p]][GATE_NGS[[p]] %in% names(fp)]

    # common support: rows with base, target, all incumbent, all NGS present
    need <- c("base_ppg", "target_ppg", inc_cols, ngs_cols)
    cc <- stats::complete.cases(fp[, need, drop = FALSE])
    fp <- fp[cc, , drop = FALSE]
    if (nrow(fp) < (min_train + 1L)) {
      if (verbose) message(glue("  {p}: too few common-support rows ({nrow(fp)}), skipped."))
      next
    }

    models <- list(base = "base_ppg")
    if (length(inc_cols) > 0L) models$incumbent <- c("base_ppg", inc_cols)
    models$ngs <- c("base_ppg", ngs_cols)

    years <- sort(unique(fp$target_year))

    for (mname in names(models)) {
      preds <- numeric(0); actuals <- numeric(0); folds <- 0L
      for (ty in years) {
        train <- fp[fp$target_year <  ty, , drop = FALSE]
        test  <- fp[fp$target_year == ty, , drop = FALSE]
        if (nrow(train) < min_train || nrow(test) == 0L) next
        fit <- tryCatch(
          stats::lm(stats::reformulate(models[[mname]], response = "target_ppg"),
                    data = train),
          error = function(e) NULL
        )
        if (is.null(fit)) next
        pr <- tryCatch(stats::predict(fit, newdata = test), error = function(e) NULL)
        if (is.null(pr)) next
        preds   <- c(preds, pr)
        actuals <- c(actuals, test$target_ppg)
        folds   <- folds + 1L
      }
      if (length(preds) == 0L) next
      resid <- actuals - preds
      out[[length(out) + 1L]] <- tibble(
        position = p,
        model    = mname,
        rmse     = sqrt(mean(resid^2)),
        mae      = mean(abs(resid)),
        n        = length(preds),
        folds    = folds
      )
    }
  }

  dplyr::bind_rows(out)
}


# ------------------------------------------------------------------------------
# robustness_rb_ngs  (stress-test the RB NGS-vs-base result)
# ------------------------------------------------------------------------------
# Same standard that retired the coach prior: per-fold direction plus a
# player-clustered bootstrap CI on the OOS improvement. Resamples PLAYERS (a
# back recurs across years, so resampling rows would fake precision). Reuses the
# exact RB common-support rows the gate scored, so the point estimate matches
# run_efficiency_gate()'s RB row. Bootstraps evaluation uncertainty on FIXED
# out-of-sample predictions (residuals resampled, models not refit inside).

# RB feature columns, mirroring GATE_INCUMBENT$RB and GATE_NGS$RB above.
RB_INC_COLS <- c("inc_rush_success_rate", "inc_rec_epa_per_target")
RB_NGS_COLS <- c("ngs_ryoe_att_vw", "ngs_rush_eff_vw")

#' Robustness check on the RB NGS-vs-base OOS improvement.
#'
#' @param frame Output of assemble_gate_frame().
#' @param B Integer. Bootstrap resamples. Default 2000L.
#' @param min_train Integer. Minimum training rows per fold. Default 15L.
#' @param seed Integer. RNG seed. Default 1L.
#'
#' @return list(per_fold, boot, predictions).
robustness_rb_ngs <- function(frame, B = 2000L, min_train = 15L, seed = 1L) {

  rb <- frame[frame$position == "RB", , drop = FALSE]

  need <- c("base_ppg", "target_ppg", RB_INC_COLS, RB_NGS_COLS)
  miss <- setdiff(need, names(rb))
  if (length(miss) > 0L) {
    stop(glue("frame is missing RB columns: {paste(miss, collapse = ', ')}"))
  }

  rb <- rb[stats::complete.cases(rb[, need, drop = FALSE]), , drop = FALSE]
  if (nrow(rb) < (min_train + 1L)) {
    stop(glue("Too few RB common-support rows: {nrow(rb)}."))
  }

  years <- sort(unique(rb$target_year))

  rows <- list()
  for (ty in years) {
    train <- rb[rb$target_year <  ty, , drop = FALSE]
    test  <- rb[rb$target_year == ty, , drop = FALSE]
    if (nrow(train) < min_train || nrow(test) == 0L) next

    fit_base <- stats::lm(target_ppg ~ base_ppg, data = train)
    fit_ngs  <- stats::lm(
      stats::reformulate(c("base_ppg", RB_NGS_COLS), response = "target_ppg"),
      data = train
    )

    rows[[length(rows) + 1L]] <- tibble(
      player_id   = test$player_id,
      target_year = ty,
      actual      = test$target_ppg,
      pred_base   = as.numeric(stats::predict(fit_base, newdata = test)),
      pred_ngs    = as.numeric(stats::predict(fit_ngs,  newdata = test))
    )
  }
  pred <- dplyr::bind_rows(rows)
  pred$err_base <- pred$actual - pred$pred_base
  pred$err_ngs  <- pred$actual - pred$pred_ngs

  .rmse <- function(e) sqrt(mean(e^2))
  .mae  <- function(e) mean(abs(e))

  per_fold <- pred %>%
    dplyr::group_by(target_year) %>%
    dplyr::summarise(
      n         = dplyr::n(),
      rmse_base = .rmse(err_base),
      rmse_ngs  = .rmse(err_ngs),
      mae_base  = .mae(err_base),
      mae_ngs   = .mae(err_ngs),
      .groups   = "drop"
    ) %>%
    dplyr::mutate(
      rmse_gain = rmse_base - rmse_ngs,   # positive = NGS better
      mae_gain  = mae_base  - mae_ngs,
      ngs_wins  = rmse_gain > 0
    )

  set.seed(seed)
  idx_by_player <- split(seq_len(nrow(pred)), pred$player_id)
  players <- names(idx_by_player)

  d_rmse <- numeric(B); d_mae <- numeric(B)
  for (b in seq_len(B)) {
    samp <- sample(players, length(players), replace = TRUE)
    idx  <- unlist(idx_by_player[samp], use.names = FALSE)
    eb   <- pred$err_base[idx]; en <- pred$err_ngs[idx]
    d_rmse[b] <- .rmse(eb) - .rmse(en)
    d_mae[b]  <- .mae(eb)  - .mae(en)
  }

  boot <- tibble(
    metric       = c("rmse", "mae"),
    point_gain   = c(.rmse(pred$err_base) - .rmse(pred$err_ngs),
                     .mae(pred$err_base)  - .mae(pred$err_ngs)),
    ci_lo        = c(stats::quantile(d_rmse, 0.025), stats::quantile(d_mae, 0.025)),
    ci_hi        = c(stats::quantile(d_rmse, 0.975), stats::quantile(d_mae, 0.975)),
    p_ngs_better = c(mean(d_rmse > 0), mean(d_mae > 0))
  )

  list(per_fold = per_fold, boot = boot, predictions = pred)
}


# ==============================================================================
# R/32 LEVEL 2 PRE-GATE  (Stage A of the two-stage "C" gate)
# ==============================================================================
#
# WHAT THIS ANSWERS
# -----------------
# R/32 converts R/31 volume to PPG with POSITION_EFFICIENCY: one catch_rate,
# one yards_per_catch, one td_per_target per position (and, for RB, one
# yards_per_carry and one td_per_carry). Every WR shares the same coefficients.
# Level 2 would replace that with a per-player, empirical-Bayes-shrunk
# efficiency. Stage A gates whether that swap earns its place, out of sample,
# BEFORE any of it is built into the model.
#
# ISOLATION (why realized volume, why core-only scoring)  [2026-07-10]
# --------------------------------------------------------------------
# The efficiency change reaches the shipped projection only through
# volume_implied_ppg_v2, which R/32 then blends against R/29 at 0.30 to 0.85.
# Stage A deliberately strips the blend and the volume-projection error away so
# the ONLY thing under test is the efficiency coefficient:
#   * Volume is the player's REALIZED per-game targets/carries in the target
#     year. Handing the arms perfect volume removes R/31 projection noise and
#     gives per-player efficiency its best possible chance. If it cannot win
#     here, it is dead everywhere.
#   * Scoring is CORE terms only (rec, rec_yd, rec_td, rush_yd, rush_td), the
#     same core constants R/32's .compute_volume_implied_ppg uses. DK best-ball
#     core equals these; DK's play-level yardage bonuses are omitted IDENTICALLY
#     across every arm and the target, so the base-vs-EB delta is unbiased by
#     their absence (same Option A stance taken in R/29 this session).
#   * The target is the player's realized CORE PPG, built from the same volume
#     components, position-aligned: receiving core for all three positions plus
#     rushing core for RB (WR/TE carry no rushing coefficient in R/32, so their
#     rushing is excluded from BOTH prediction and target -- a constant that
#     cancels in the delta). Given realized volume, realized CORE PPG equals
#     realized_volume * realized_efficiency exactly, so the sole error source is
#     trailing-vs-realized efficiency. That is precisely what Level 2 changes.
#
# THE ARMS
# --------
#   base : POSITION_EFFICIENCY reproduced on the trailing window (opportunity-
#          pooled sum/sum, the identical method R/32 uses). Same for every player
#          in a position.
#   a1   : per-player EB on ALL five rates.
#   a2   : per-player EB on the EFFICIENCY rates (catch_rate, yards_per_catch,
#          yards_per_carry); the noisy TD rates held at the position mean. This
#          exists so a badly-behaved TD rate cannot sink an otherwise-useful
#          efficiency signal, and so a false KILL cannot come from TD noise alone.
#
# EMPIRICAL BAYES  (one estimator, all rates, estimated OOS per fold)
# ------------------------------------------------------------------
# Per position, per rate, per target year, using ONLY the trailing window
# (seasons < target year, no leakage): shrink each player's own rate toward the
# position pooled mean by an amount set by how reliably his sample separates
# from that mean.
#   raw_i = total_i / denom_i     (e.g. receptions / targets)
#   mu    = sum(total) / sum(denom)   (opportunity-pooled, = R/32's method)
#   eb_i  = (total_i + k*mu) / (denom_i + k)
# k (the prior's equivalent sample size, in denominator units) is estimated by a
# two-moment method of moments that needs no sub-season data and no distributional
# assumption beyond a mean/variance structure. With a_i = raw_i - mu and weights
# denom_i, and modeling Var(raw_i) = tau2 + sigma2/denom_i:
#   W1 = sum(denom*a^2)/sum(denom) = tau2 + sigma2*(n/S)     [denom-weighted]
#   U  = mean(a^2)                 = tau2 + sigma2*mean(1/denom)   [unweighted]
#   sigma2 = (U - W1) / (mean(1/denom) - n/S)   ; tau2 = W1 - sigma2*(n/S)
#   k = sigma2 / tau2
# The two weightings give two equations in (sigma2, tau2). Any degenerate case
# (denominators near-constant, no identifiable sampling variance, or no
# between-player variance) returns k = Inf, i.e. full shrink to the position
# mean. That bias is toward the null: it makes Stage A HARDER to pass, which is
# the correct bias for a kill-switch. A win under conservative shrinkage is real.
#
# POPULATIONS
# -----------
# No-history players shrink exactly to mu, so they are identical to base and
# contribute nothing to the delta. Two populations are reported:
#   full   : every target-year player with >= min_games. Realistic; the delta is
#            diluted by all the no-history rows, which previews the Stage B blend
#            dilution.
#   movers : players with enough trailing-window volume to actually differ from
#            the position mean. This is where an efficiency effect CAN show up,
#            so the KILL decision reads the movers population. If EB cannot beat
#            base among movers, Level 2 is dead.
#
# OOS + UNCERTAINTY
# -----------------
# Forward-chained folds: for each target year, the efficiency table is built from
# its trailing window only and applied to that year's realized volume; residuals
# pool across years. robustness_l2_efficiency() adds a player-clustered bootstrap
# CI on rmse_base - rmse_arm (positive = EB better), the same standard that
# retired the coach prior and gated the RB NGS result.
#
# SCOPE / LIMITS
# --------------
#   * RB/WR/TE only (QB uses the R/30 anchor path in R/32, not this conversion).
#   * DK-core scoped, consistent with all Slot 2-3 validation.
#   * Diagnostic only. Nothing here modifies R/29 or R/32.
# ==============================================================================

L2_RECON_POSITIONS   <- c("RB", "WR", "TE")
L2_DEFAULT_WINDOW    <- 3L    # mirror VETERAN_WINDOW_SEASONS / R/32 efficiency window
L2_MOVER_MIN_TARGETS <- 100L  # trailing-window targets to count as a receiving "mover"
L2_MOVER_MIN_CARRIES <- 100L  # trailing-window carries to count as a rushing "mover"

# Core scoring. Mirrors R/32 DEFAULT_SCORING_SETTINGS core terms exactly. DK
# best-ball core equals these; play-level bonuses omitted identically across arms
# and target (Option A, 2026-07-10).
L2_CORE_SCORING <- list(rec = 1.0, rec_yd = 0.1, rec_td = 6,
                        rush_yd = 0.1, rush_td = 6)


# ------------------------------------------------------------------------------
# build_l2_components  (player-season volume + core components, one source)
# ------------------------------------------------------------------------------

# Last non-NA position for a player-season. load_player_stats leaves position NA
# on some weeks; an NA here would poison base-R position subsetting downstream
# (NA logical index -> phantom NA rows -> NA sums).
.l2_last_pos <- function(p) {
  p <- p[!is.na(p)]
  if (length(p)) p[[length(p)]] else NA_character_
}

#' Load player-season offensive components for the L2 gate.
#'
#' Single source: nflreadr::load_player_stats(stat_type = "offense"), the SAME
#' pull R/32's .derive_position_efficiency uses, so the base arm reproduces
#' POSITION_EFFICIENCY by construction. Aggregates REG rows to player-season and
#' counts games. Handles both weekly-grain and season-grain returns.
#'
#' @param seasons Integer vector of seasons to load.
#' @param cache_path Character or NULL. If given, saved/loaded as .rds.
#' @param verbose Logical.
#'
#' @return tibble(player_id, season, position, games, targets, receptions,
#'   receiving_yards, receiving_tds, carries, rushing_yards, rushing_tds).
build_l2_components <- function(seasons, cache_path = NULL, verbose = TRUE) {

  if (!is.null(cache_path) && file.exists(cache_path)) {
    if (verbose) message(glue("build_l2_components(): loading cache {cache_path}"))
    return(readRDS(cache_path))
  }

  raw <- nflreadr::load_player_stats(seasons = seasons, stat_type = "offense")

  # Resolve the player id column across nflreadr schema variants.
  id_candidates <- c("player_id", "gsis_id", "player_gsis_id")
  id_col <- id_candidates[id_candidates %in% names(raw)][1]
  if (is.na(id_col)) {
    stop(glue("build_l2_components(): no player id column found. ",
              "Looked for: {paste(id_candidates, collapse = ', ')}. ",
              "Present: {paste(head(names(raw), 30), collapse = ', ')}"))
  }

  need_cols <- c("position", "season", "season_type",
                 "targets", "receptions", "receiving_yards", "receiving_tds",
                 "carries", "rushing_yards", "rushing_tds")
  miss <- setdiff(need_cols, names(raw))
  if (length(miss) > 0L) {
    stop(glue("build_l2_components(): missing columns from load_player_stats: ",
              "{paste(miss, collapse = ', ')}"))
  }

  d <- raw
  d$player_id <- d[[id_col]]
  d <- d[d$season_type == "REG", , drop = FALSE]

  # Games: prefer distinct weeks (weekly grain); else a games column (season grain).
  has_week  <- "week" %in% names(d)
  games_col <- intersect(c("games", "games_played"), names(d))
  if (!has_week && length(games_col) == 0L) {
    stop("build_l2_components(): cannot determine games (no 'week' and no ",
         "'games'/'games_played' column). Report the load_player_stats schema.")
  }

  num0 <- function(x) dplyr::coalesce(as.numeric(x), 0)

  agg <- d %>%
    dplyr::group_by(.data$player_id, .data$season) %>%
    dplyr::summarise(
      position        = .l2_last_pos(.data$position),
      games           = if (has_week) dplyr::n_distinct(.data$week)
                        else sum(num0(.data[[games_col[1]]])),
      targets         = sum(num0(.data$targets)),
      receptions      = sum(num0(.data$receptions)),
      receiving_yards = sum(num0(.data$receiving_yards)),
      receiving_tds   = sum(num0(.data$receiving_tds)),
      carries         = sum(num0(.data$carries)),
      rushing_yards   = sum(num0(.data$rushing_yards)),
      rushing_tds     = sum(num0(.data$rushing_tds)),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$games > 0, !is.na(.data$position))

  if (verbose) {
    message(glue("  build_l2_components(): {nrow(agg)} player-seasons across ",
                 "{min(seasons)}-{max(seasons)} (id col: {id_col}, ",
                 "grain: {if (has_week) 'weekly' else 'seasonal'})"))
  }

  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(agg, cache_path)
    if (verbose) message(glue("  cached -> {cache_path}"))
  }

  agg
}


# ------------------------------------------------------------------------------
# EB helpers
# ------------------------------------------------------------------------------

# Rates applicable per position (RB gets rushing rates; WR/TE receiving only).
.l2_rates_for_position <- function(pos) {
  if (pos == "RB") {
    c("catch_rate", "yards_per_catch", "td_per_target",
      "yards_per_carry", "td_per_carry")
  } else {
    c("catch_rate", "yards_per_catch", "td_per_target")
  }
}

# Two-moment method-of-moments prior strength k for one position-rate.
# `total`, `denom` are per-player trailing-window aggregates. Returns Inf on any
# degenerate case (full shrink to mu), which biases the gate toward the null.
.l2_estimate_k <- function(total, denom, mu) {
  keep <- is.finite(total) & is.finite(denom) & denom > 0
  total <- total[keep]; denom <- denom[keep]
  n <- length(denom)
  if (n < 5L || is.na(mu)) return(Inf)
  S <- sum(denom)
  if (is.na(S) || S <= 0) return(Inf)
  raw <- total / denom
  a   <- raw - mu
  W1  <- sum(denom * a^2) / S       # denom-weighted mean squared deviation
  U   <- mean(a^2)                  # unweighted mean squared deviation
  gap <- mean(1 / denom) - (n / S)  # >= 0 by AM-HM; 0 iff denominators equal
  if (!is.finite(gap) || gap <= 1e-12) return(Inf)
  sigma2 <- (U - W1) / gap
  if (!is.finite(sigma2) || sigma2 <= 0) return(Inf)
  tau2 <- W1 - sigma2 * (n / S)
  if (!is.finite(tau2) || tau2 <= 0) return(Inf)
  k <- sigma2 / tau2
  if (!is.finite(k) || k < 0) return(Inf)
  k
}

# Row-wise EB posterior mean where mu and k are per-row columns (k varies by
# position). Any row with a non-finite k (full-shrink case) returns mu.
.l2_eb_col <- function(total, denom, mu, k) {
  total <- dplyr::coalesce(total, 0)
  denom <- dplyr::coalesce(denom, 0)
  out   <- (total + k * mu) / (denom + k)
  dplyr::if_else(is.finite(k), out, mu)
}

# Per position, per rate: opportunity-pooled mu and MoM k from window rows.
# Aggregates to one (total, denom) per player first (a player is one theta_i).
.l2_fit_priors <- function(wdf) {
  # window totals per player, per rate component
  wtot <- wdf %>%
    dplyr::group_by(.data$player_id) %>%
    dplyr::summarise(
      position = .l2_last_pos(.data$position),
      t_targets    = sum(.data$targets),
      t_recept     = sum(.data$receptions),
      t_rec_yards  = sum(.data$receiving_yards),
      t_rec_tds    = sum(.data$receiving_tds),
      t_carries    = sum(.data$carries),
      t_rush_yards = sum(.data$rushing_yards),
      t_rush_tds   = sum(.data$rushing_tds),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(.data$position))

  rate_pair <- list(
    catch_rate      = c("t_recept",    "t_targets"),
    yards_per_catch = c("t_rec_yards", "t_recept"),
    td_per_target   = c("t_rec_tds",   "t_targets"),
    yards_per_carry = c("t_rush_yards","t_carries"),
    td_per_carry    = c("t_rush_tds",  "t_carries")
  )

  out <- list()
  for (pos in L2_RECON_POSITIONS) {
    sub <- wtot[wtot$position == pos, , drop = FALSE]
    if (nrow(sub) == 0L) next
    for (rn in .l2_rates_for_position(pos)) {
      tot_col <- rate_pair[[rn]][1]; den_col <- rate_pair[[rn]][2]
      tot <- sub[[tot_col]]; den <- sub[[den_col]]
      S   <- sum(den)
      mu  <- if (!is.na(S) && S > 0) sum(tot) / S else NA_real_
      k   <- if (is.na(mu)) Inf else .l2_estimate_k(tot, den, mu)
      out[[length(out) + 1L]] <- tibble(position = pos, rate = rn, mu = mu, k = k)
    }
  }
  dplyr::bind_rows(out)
}


# ------------------------------------------------------------------------------
# assemble_l2_gate_frame  (per player-target-year predictions for all three arms)
# ------------------------------------------------------------------------------

#' Build the L2 gate frame: base / a1 / a2 predicted PPG plus the realized core
#' PPG target, one row per player-target-year, forward-chained (priors from the
#' trailing window only).
#'
#' @param components Output of build_l2_components().
#' @param target_years Integer vector of TARGET seasons. Each needs a full
#'   `window`-season trailing history in `components`.
#' @param window Integer trailing window. Default L2_DEFAULT_WINDOW.
#' @param min_games Integer target-season games floor. Default 4L.
#' @param scoring Named list of core scoring constants. Default L2_CORE_SCORING.
#' @param mover_min_targets,mover_min_carries Integer thresholds for the movers
#'   population (trailing-window volume). Defaults L2_MOVER_MIN_*.
#' @param verbose Logical.
#'
#' @return tibble(player_id, position, target_year, games, targets_pg,
#'   carries_pg, target_ppg, pred_base, pred_a1, pred_a2, w_targets, w_carries,
#'   is_mover).
assemble_l2_gate_frame <- function(components, target_years,
                                   window            = L2_DEFAULT_WINDOW,
                                   min_games         = 4L,
                                   scoring           = L2_CORE_SCORING,
                                   mover_min_targets = L2_MOVER_MIN_TARGETS,
                                   mover_min_carries = L2_MOVER_MIN_CARRIES,
                                   verbose           = TRUE) {

  frames <- lapply(target_years, function(ty) {
    if (verbose) message(glue("  L2 assembling target {ty}..."))
    win <- (ty - window):(ty - 1L)
    wdf <- components[components$season %in% win, , drop = FALSE]
    tdf <- components[components$season == ty, , drop = FALSE]
    if (nrow(wdf) == 0L || nrow(tdf) == 0L) return(NULL)

    priors <- .l2_fit_priors(wdf)
    if (nrow(priors) == 0L) return(NULL)

    # wide priors: mu_<rate>, k_<rate> per position
    priors_wide <- priors %>%
      tidyr::pivot_wider(
        names_from  = rate,
        values_from = c(mu, k),
        names_glue  = "{.value}_{rate}"
      )

    # ensure every expected column exists (a fold could lack a rate for a
    # position); assemble references all five rate columns unconditionally.
    all_rates <- c("catch_rate", "yards_per_catch", "td_per_target",
                   "yards_per_carry", "td_per_carry")
    for (rn in all_rates) {
      for (pfx in c("mu_", "k_")) {
        cn <- paste0(pfx, rn)
        if (!cn %in% names(priors_wide)) priors_wide[[cn]] <- NA_real_
      }
    }

    # per-player trailing-window totals (coalesced 0 for no-history players)
    wtot <- wdf %>%
      dplyr::group_by(.data$player_id) %>%
      dplyr::summarise(
        w_targets   = sum(.data$targets),
        w_recept    = sum(.data$receptions),
        w_rec_yards = sum(.data$receiving_yards),
        w_rec_tds   = sum(.data$receiving_tds),
        w_carries   = sum(.data$carries),
        w_rush_yards= sum(.data$rushing_yards),
        w_rush_tds  = sum(.data$rushing_tds),
        .groups = "drop"
      )

    # target-year realized, RB/WR/TE, min_games gate
    tgt <- tdf %>%
      dplyr::filter(.data$games >= min_games,
                    .data$position %in% L2_RECON_POSITIONS) %>%
      dplyr::transmute(
        player_id, position, games,
        tg_targets = targets, tg_recept = receptions,
        tg_rec_yards = receiving_yards, tg_rec_tds = receiving_tds,
        tg_carries = carries, tg_rush_yards = rushing_yards,
        tg_rush_tds = rushing_tds
      )
    if (nrow(tgt) == 0L) return(NULL)

    row <- tgt %>%
      dplyr::left_join(wtot,        by = "player_id") %>%
      dplyr::left_join(priors_wide, by = "position")

    # coalesce window totals (no-history -> 0 -> EB returns mu)
    zc <- c("w_targets","w_recept","w_rec_yards","w_rec_tds",
            "w_carries","w_rush_yards","w_rush_tds")
    for (cc in zc) row[[cc]] <- dplyr::coalesce(row[[cc]], 0)

    is_rb <- row$position == "RB"

    # ---- per-rate arm values (base = mu; a1 = EB all; a2 = EB eff, TD at mu) ----
    # receiving rates (all three positions)
    cr_base  <- row$mu_catch_rate
    ypc_base <- row$mu_yards_per_catch
    tdt_base <- row$mu_td_per_target

    cr_eb  <- .l2_eb_col(row$w_recept,   row$w_targets, row$mu_catch_rate,      row$k_catch_rate)
    ypc_eb <- .l2_eb_col(row$w_rec_yards,row$w_recept,  row$mu_yards_per_catch, row$k_yards_per_catch)
    tdt_eb <- .l2_eb_col(row$w_rec_tds,  row$w_targets, row$mu_td_per_target,   row$k_td_per_target)

    # rushing rates (RB only; 0 for WR/TE)
    ypcarry_base <- dplyr::if_else(is_rb, dplyr::coalesce(row$mu_yards_per_carry, 0), 0)
    tdc_base     <- dplyr::if_else(is_rb, dplyr::coalesce(row$mu_td_per_carry, 0), 0)
    ypcarry_eb   <- dplyr::if_else(is_rb,
                     .l2_eb_col(row$w_rush_yards, row$w_carries, row$mu_yards_per_carry, row$k_yards_per_carry), 0)
    tdc_eb       <- dplyr::if_else(is_rb,
                     .l2_eb_col(row$w_rush_tds,   row$w_carries, row$mu_td_per_carry,    row$k_td_per_carry), 0)

    # arm coefficient bundles
    ppr_t <- function(cr, ypc, tdt) cr * (ypc * scoring$rec_yd + scoring$rec) + tdt * scoring$rec_td
    ppr_c <- function(ypc, tdc)     ypc * scoring$rush_yd + tdc * scoring$rush_td

    pt_base <- ppr_t(cr_base, ypc_base, tdt_base)
    pc_base <- ppr_c(ypcarry_base, tdc_base)

    pt_a1 <- ppr_t(cr_eb, ypc_eb, tdt_eb)
    pc_a1 <- ppr_c(ypcarry_eb, tdc_eb)

    # a2: efficiency rates EB, TD rates at position mean
    pt_a2 <- ppr_t(cr_eb, ypc_eb, tdt_base)
    pc_a2 <- ppr_c(ypcarry_eb, tdc_base)

    targets_pg <- row$tg_targets / row$games
    carries_pg <- row$tg_carries / row$games

    # realized CORE PPG target, position-aligned (RB adds rushing core)
    rec_core  <- row$tg_recept * scoring$rec +
                 row$tg_rec_yards * scoring$rec_yd +
                 row$tg_rec_tds * scoring$rec_td
    rush_core <- row$tg_rush_yards * scoring$rush_yd + row$tg_rush_tds * scoring$rush_td
    core_pts  <- rec_core + dplyr::if_else(is_rb, rush_core, 0)

    is_mover <- dplyr::if_else(
      is_rb,
      (row$w_targets >= mover_min_targets) | (row$w_carries >= mover_min_carries),
      row$w_targets >= mover_min_targets
    )

    tibble(
      player_id   = row$player_id,
      position    = row$position,
      target_year = ty,
      games       = row$games,
      targets_pg  = targets_pg,
      carries_pg  = carries_pg,
      target_ppg  = core_pts / row$games,
      pred_base   = targets_pg * pt_base + carries_pg * pc_base,
      pred_a1     = targets_pg * pt_a1   + carries_pg * pc_a1,
      pred_a2     = targets_pg * pt_a2   + carries_pg * pc_a2,
      w_targets   = row$w_targets,
      w_carries   = row$w_carries,
      is_mover    = is_mover
    )
  })

  out <- dplyr::bind_rows(frames)
  # drop rows where base could not be built (position had no window support)
  out[is.finite(out$pred_base) & is.finite(out$target_ppg), , drop = FALSE]
}


# ------------------------------------------------------------------------------
# run_l2_efficiency_gate  (point RMSE/MAE per position, arm, population)
# ------------------------------------------------------------------------------

#' Score base / a1 / a2 against realized core PPG, per position and population.
#'
#' @param frame Output of assemble_l2_gate_frame().
#' @param verbose Logical.
#'
#' @return tibble(position, population, model, rmse, mae, n, folds,
#'   rmse_gain_vs_base, mae_gain_vs_base). Positive gain = arm beats base.
run_l2_efficiency_gate <- function(frame, verbose = TRUE) {

  .rmse <- function(e) sqrt(mean(e^2))
  .mae  <- function(e) mean(abs(e))
  out <- list()

  for (pos in L2_RECON_POSITIONS) {
    for (popn in c("full", "movers")) {
      fp <- frame[frame$position == pos, , drop = FALSE]
      if (popn == "movers") fp <- fp[fp$is_mover, , drop = FALSE]
      fp <- fp[stats::complete.cases(
        fp[, c("target_ppg","pred_base","pred_a1","pred_a2")]), , drop = FALSE]
      if (nrow(fp) == 0L) next

      folds <- dplyr::n_distinct(fp$target_year)
      base_rmse <- .rmse(fp$target_ppg - fp$pred_base)
      base_mae  <- .mae(fp$target_ppg - fp$pred_base)

      for (m in c("base", "a1", "a2")) {
        pc  <- switch(m, base = "pred_base", a1 = "pred_a1", a2 = "pred_a2")
        res <- fp$target_ppg - fp[[pc]]
        out[[length(out) + 1L]] <- tibble(
          position   = pos,
          population = popn,
          model      = m,
          rmse       = .rmse(res),
          mae        = .mae(res),
          n          = nrow(fp),
          folds      = folds,
          rmse_gain_vs_base = base_rmse - .rmse(res),
          mae_gain_vs_base  = base_mae  - .mae(res)
        )
      }
    }
  }

  res <- dplyr::bind_rows(out)
  if (verbose && nrow(res) > 0L) {
    message("  L2 gate (positive gain = per-player EB beats position-average):")
    for (i in seq_len(nrow(res))) {
      if (res$model[i] == "base") next
      message(glue(
        "    {res$position[i]}/{res$population[i]}/{res$model[i]}: ",
        "rmse {format(round(res$rmse[i], 3), nsmall = 3)} ",
        "(gain {format(round(res$rmse_gain_vs_base[i], 3), nsmall = 3)}), ",
        "n {res$n[i]}, folds {res$folds[i]}"
      ))
    }
  }
  res
}


# ------------------------------------------------------------------------------
# robustness_l2_efficiency  (player-clustered bootstrap CI on the EB gain)
# ------------------------------------------------------------------------------

#' Bootstrap CI on rmse_base - rmse_arm (and mae), per position, player-clustered.
#'
#' Resamples PLAYERS (a player recurs across target years; resampling rows would
#' fake precision). Predictions are FIXED; only evaluation uncertainty is
#' bootstrapped. Mirrors robustness_rb_ngs().
#'
#' @param frame Output of assemble_l2_gate_frame().
#' @param arm Character, "a1" or "a2".
#' @param population Character, "movers" or "full".
#' @param B Integer bootstrap resamples. Default 2000L.
#' @param seed Integer RNG seed. Default 1L.
#'
#' @return list(per_fold, boot). boot has position, metric, point_gain, ci_lo,
#'   ci_hi, p_arm_better.
robustness_l2_efficiency <- function(frame, arm = c("a1", "a2"),
                                     population = c("movers", "full"),
                                     B = 2000L, seed = 1L) {
  arm        <- match.arg(arm)
  population <- match.arg(population)
  pred_col   <- if (arm == "a1") "pred_a1" else "pred_a2"

  .rmse <- function(e) sqrt(mean(e^2))
  .mae  <- function(e) mean(abs(e))

  per_fold_all <- list()
  boot_all     <- list()

  for (pos in L2_RECON_POSITIONS) {
    fp <- frame[frame$position == pos, , drop = FALSE]
    if (population == "movers") fp <- fp[fp$is_mover, , drop = FALSE]
    fp <- fp[stats::complete.cases(
      fp[, c("target_ppg", "pred_base", pred_col)]), , drop = FALSE]
    if (nrow(fp) < 10L) next

    eb_base <- fp$target_ppg - fp$pred_base
    eb_arm  <- fp$target_ppg - fp[[pred_col]]

    per_fold_all[[length(per_fold_all) + 1L]] <- tibble(
      position = pos, target_year = fp$target_year,
      err_base = eb_base, err_arm = eb_arm
    ) %>%
      dplyr::group_by(position, target_year) %>%
      dplyr::summarise(
        n = dplyr::n(),
        rmse_base = .rmse(err_base), rmse_arm = .rmse(err_arm),
        mae_base  = .mae(err_base),  mae_arm  = .mae(err_arm),
        .groups = "drop"
      ) %>%
      dplyr::mutate(rmse_gain = rmse_base - rmse_arm,
                    mae_gain  = mae_base  - mae_arm,
                    arm_wins  = rmse_gain > 0)

    set.seed(seed)
    idx_by_player <- split(seq_len(nrow(fp)), fp$player_id)
    players <- names(idx_by_player)
    d_rmse <- numeric(B); d_mae <- numeric(B)
    for (b in seq_len(B)) {
      samp <- sample(players, length(players), replace = TRUE)
      idx  <- unlist(idx_by_player[samp], use.names = FALSE)
      eb   <- eb_base[idx]; en <- eb_arm[idx]
      d_rmse[b] <- .rmse(eb) - .rmse(en)
      d_mae[b]  <- .mae(eb)  - .mae(en)
    }

    boot_all[[length(boot_all) + 1L]] <- tibble(
      position     = pos,
      arm          = arm,
      population   = population,
      metric       = c("rmse", "mae"),
      point_gain   = c(.rmse(eb_base) - .rmse(eb_arm),
                       .mae(eb_base)  - .mae(eb_arm)),
      ci_lo        = c(stats::quantile(d_rmse, 0.025), stats::quantile(d_mae, 0.025)),
      ci_hi        = c(stats::quantile(d_rmse, 0.975), stats::quantile(d_mae, 0.975)),
      p_arm_better = c(mean(d_rmse > 0), mean(d_mae > 0))
    )
  }

  list(per_fold = dplyr::bind_rows(per_fold_all),
       boot     = dplyr::bind_rows(boot_all))
}


# ==============================================================================
# R/32 LEVEL 2 PRE-GATE  (Stage B-lite: the swap at the blended r32 number)
# ==============================================================================
#
# WHAT THIS ANSWERS
# -----------------
# Stage A proved per-player EB efficiency (a2 form: EB on catch_rate,
# yards_per_catch, yards_per_carry; TD rates at the position mean) converts
# realized volume to PPG better than position-average, in isolation. Stage B
# asks the only question left before a wire-in: does that survive R/32's blend,
#   r32 = w * volume_implied + (1 - w) * r29_posterior,
# where movers are veterans and therefore sit at the LOW end of w (0.30 to 0.45
# under the live RB/WR/TE weights), so the isolated gain gets diluted, and where
# the r29 term already embeds the player's own efficiency (the double-count
# concern).
#
# WHY "LITE" (and why it is still honest)  [2026-07-10]
# ----------------------------------------------------
# The exact measurement needs historical R/29 holdout posteriors for 2018-2025.
# They do not exist on disk and R/29 cannot produce them without a per-year
# upstream regen (translation, aging, panel, priors) under leakage control. That
# is a multi-day build that collides with the draft timeline. Stage B-lite buys
# the decision-relevant signal far more cheaply:
#   * The blend arithmetic and weights are REAL: w comes from R/32's live
#     BLEND_WEIGHTS_BY_PRIOR_SOURCE (sourced if available, else a dated copy).
#   * The r29 term is a leakage-safe PROXY: the player's trailing-window
#     opportunity-weighted core PPG, built only from seasons before the target
#     year. For veteran movers (the population L2 acts on) this is exactly what
#     R/29's history prior mostly is, so the proxy is faithful where it matters
#     and the double-count geometry is preserved: making volume_implied use the
#     player's own efficiency correlates it with the anchor, and if that hurts
#     the blend, this measures it.
#   * A WEIGHT SWEEP reports the a2-vs-base gain at every real weight value, so
#     dilution is shown as a curve rather than hidden behind one proxy weight.
# Limits: the proxy diverges from real R/29 for translation and rookie priors,
# but those players are not movers. Everything is position-aligned core PPG
# (WR/TE receiving only, RB receiving plus rushing), consistent across the
# anchor, both arms, and the target, so all comparisons are like-for-like.
#
# READING IT
# ----------
# For each position and population, run_l2_stageb_gate() reports rmse for base
# and a2 at each swept weight and at a per-player proxy weight, with the a2 gain.
# robustness_l2_stageb() puts a player-clustered bootstrap CI on the blended gain
# at a chosen weight. WIRE-IN test: a2 beats base at the blended number across
# the veteran weight range (0.30 to 0.45) with a CI clear of zero among movers.
# If the gain only appears at high w (which veterans do not get), or straddles
# zero across 0.30 to 0.45, L2 dilutes to neutral and the wire-in is optional.
# ==============================================================================

# Dated copy of live R/32 v2.2 BLEND_WEIGHTS_BY_PRIOR_SOURCE (RB/WR/TE). Used
# ONLY when R/32 is not sourced; prefer the live constant.
L2_BLEND_WEIGHTS_DEFAULT <- c(
  "blended"               = 0.30,
  "veteran_history_only"  = 0.45,
  "history_only_fallback" = 0.55,
  "translation_only"      = 0.65,
  "score_final_fallback"  = 0.85
)
L2_BLEND_DEFAULT_W <- 0.50   # unrecognized prior_source (R/32 DEFAULT_R31_WEIGHT)

# Resolve blend weights: caller override > live R/32 constant > dated copy.
.l2_resolve_blend_weights <- function(blend_weights = NULL) {
  if (!is.null(blend_weights)) return(blend_weights)
  if (exists("BLEND_WEIGHTS_BY_PRIOR_SOURCE", inherits = TRUE)) {
    return(get("BLEND_WEIGHTS_BY_PRIOR_SOURCE", inherits = TRUE))
  }
  message("  Stage B: R/32 not sourced; using dated L2_BLEND_WEIGHTS_DEFAULT.")
  L2_BLEND_WEIGHTS_DEFAULT
}

# Position-aligned core points for each component row (RB adds rushing core).
.l2_core_pts_row <- function(df, scoring) {
  rec  <- df$receptions * scoring$rec +
          df$receiving_yards * scoring$rec_yd +
          df$receiving_tds * scoring$rec_td
  rush <- df$rushing_yards * scoring$rush_yd + df$rushing_tds * scoring$rush_td
  rec + dplyr::if_else(df$position == "RB", rush, 0)
}


# ------------------------------------------------------------------------------
# assemble_l2_stageb  (attach r29 proxy anchor + proxy blend weight)
# ------------------------------------------------------------------------------

#' Augment a Stage A frame with the leakage-safe R/29 anchor proxy and a
#' per-player proxy blend weight.
#'
#' @param frame Output of assemble_l2_gate_frame() (carries pred_base, pred_a2,
#'   target_ppg, position, target_year, player_id, is_mover).
#' @param components Output of build_l2_components().
#' @param window Integer trailing window for the anchor. Default L2_DEFAULT_WINDOW.
#' @param scoring Core scoring list. Default L2_CORE_SCORING (must match the
#'   frame's scoring).
#' @param blend_weights Named vector by prior_source, or NULL to resolve from
#'   live R/32 / dated copy.
#' @param verbose Logical.
#'
#' @return frame rows that have a trailing anchor, plus r29_proxy,
#'   career_prior_seasons, prior_source_proxy, w_proxy.
assemble_l2_stageb <- function(frame, components,
                               window        = L2_DEFAULT_WINDOW,
                               scoring       = L2_CORE_SCORING,
                               blend_weights = NULL,
                               verbose       = TRUE) {

  bw <- .l2_resolve_blend_weights(blend_weights)

  comp <- components
  comp$core_pts <- .l2_core_pts_row(comp, scoring)

  tys <- sort(unique(frame$target_year))
  proxy <- dplyr::bind_rows(lapply(tys, function(ty) {
    win <- (ty - window):(ty - 1L)
    w   <- comp[comp$season %in% win, , drop = FALSE]
    anchor <- w %>%
      dplyr::group_by(.data$player_id) %>%
      dplyr::summarise(
        trail_pts   = sum(.data$core_pts),
        trail_games = sum(.data$games),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        r29_proxy   = dplyr::if_else(trail_games > 0, trail_pts / trail_games,
                                     NA_real_),
        target_year = ty
      )
    car <- comp[comp$season < ty & comp$games > 0, , drop = FALSE] %>%
      dplyr::group_by(.data$player_id) %>%
      dplyr::summarise(career_prior_seasons = dplyr::n_distinct(.data$season),
                       .groups = "drop")
    anchor %>%
      dplyr::left_join(car, by = "player_id") %>%
      dplyr::select(player_id, target_year, r29_proxy, career_prior_seasons)
  }))

  sb <- frame %>%
    dplyr::left_join(proxy, by = c("player_id", "target_year")) %>%
    dplyr::filter(!is.na(.data$r29_proxy))    # Stage B conditions on an anchor

  sb$career_prior_seasons <- dplyr::coalesce(sb$career_prior_seasons, 0L)
  sb$prior_source_proxy <- dplyr::case_when(
    sb$career_prior_seasons == 0L ~ "score_final_fallback",
    sb$career_prior_seasons >= 4L ~ "veteran_history_only",
    TRUE                          ~ "blended"
  )
  w_proxy <- unname(bw[sb$prior_source_proxy])
  w_proxy[is.na(w_proxy)] <- L2_BLEND_DEFAULT_W
  sb$w_proxy <- w_proxy

  if (verbose) {
    message(glue("  Stage B frame: {nrow(sb)} rows with a trailing anchor; ",
                 "movers {sum(sb$is_mover)}."))
  }
  sb
}


# ------------------------------------------------------------------------------
# run_l2_stageb_gate  (base vs a2 at the blended number; weight sweep + proxy)
# ------------------------------------------------------------------------------

#' Score base vs a2 at r32 = w*volume_implied + (1-w)*r29_proxy, per position and
#' population, across a weight sweep and at the per-player proxy weight.
#'
#' @param sb Output of assemble_l2_stageb().
#' @param w_grid Numeric weights to sweep, or NULL to use the distinct live
#'   RB/WR/TE weight values.
#' @param blend_weights Passed to weight resolution for the default grid.
#' @param verbose Logical.
#'
#' @return tibble(position, population, w_scheme, w_value, rmse_base, rmse_a2,
#'   rmse_gain, mae_base, mae_a2, mae_gain, n, folds). w_scheme is the formatted
#'   weight or "proxy". Positive gain = a2 beats base at the blended number.
run_l2_stageb_gate <- function(sb, w_grid = NULL, blend_weights = NULL,
                               verbose = TRUE) {

  .rmse <- function(e) sqrt(mean(e^2))
  .mae  <- function(e) mean(abs(e))

  if (is.null(w_grid)) {
    bw <- .l2_resolve_blend_weights(blend_weights)
    w_grid <- sort(unique(unname(bw)))
  }

  score_at <- function(fp, wvec) {
    r32_base <- wvec * fp$pred_base + (1 - wvec) * fp$r29_proxy
    r32_a2   <- wvec * fp$pred_a2   + (1 - wvec) * fp$r29_proxy
    eb <- fp$target_ppg - r32_base
    ea <- fp$target_ppg - r32_a2
    list(rmse_base = .rmse(eb), rmse_a2 = .rmse(ea),
         mae_base  = .mae(eb),  mae_a2  = .mae(ea))
  }

  out <- list()
  for (pos in L2_RECON_POSITIONS) {
    for (popn in c("full", "movers")) {
      fp <- sb[sb$position == pos, , drop = FALSE]
      if (popn == "movers") fp <- fp[fp$is_mover, , drop = FALSE]
      fp <- fp[stats::complete.cases(
        fp[, c("target_ppg","pred_base","pred_a2","r29_proxy")]), , drop = FALSE]
      if (nrow(fp) == 0L) next
      folds <- dplyr::n_distinct(fp$target_year)

      schemes <- c(as.list(w_grid), list("proxy"))
      for (s in schemes) {
        if (identical(s, "proxy")) {
          wvec <- fp$w_proxy; label <- "proxy"; wval <- NA_real_
        } else {
          wvec <- as.numeric(s); label <- format(round(wvec, 2), nsmall = 2)
          wval <- as.numeric(s)
        }
        sc <- score_at(fp, wvec)
        out[[length(out) + 1L]] <- tibble(
          position = pos, population = popn,
          w_scheme = label, w_value = wval,
          rmse_base = sc$rmse_base, rmse_a2 = sc$rmse_a2,
          rmse_gain = sc$rmse_base - sc$rmse_a2,
          mae_base = sc$mae_base, mae_a2 = sc$mae_a2,
          mae_gain = sc$mae_base - sc$mae_a2,
          n = nrow(fp), folds = folds
        )
      }
    }
  }

  res <- dplyr::bind_rows(out)
  if (verbose && nrow(res) > 0L) {
    message("  Stage B (movers, a2 gain at the blended number):")
    mv <- res[res$population == "movers", , drop = FALSE]
    for (i in seq_len(nrow(mv))) {
      message(glue(
        "    {mv$position[i]}/w={mv$w_scheme[i]}: ",
        "rmse_base {format(round(mv$rmse_base[i], 3), nsmall = 3)} -> ",
        "a2 {format(round(mv$rmse_a2[i], 3), nsmall = 3)} ",
        "(gain {format(round(mv$rmse_gain[i], 3), nsmall = 3)})"
      ))
    }
  }
  res
}


# ------------------------------------------------------------------------------
# robustness_l2_stageb  (player-clustered bootstrap CI on the blended gain)
# ------------------------------------------------------------------------------

#' Bootstrap CI on rmse_base - rmse_a2 at the blended number, per position.
#'
#' @param sb Output of assemble_l2_stageb().
#' @param w Numeric weight, or "proxy" for the per-player proxy weight.
#' @param population "movers" or "full".
#' @param B Integer resamples. Default 2000L.
#' @param seed Integer. Default 1L.
#'
#' @return list(per_fold, boot), player-clustered, mirroring the Stage A checks.
robustness_l2_stageb <- function(sb, w = 0.45, population = c("movers", "full"),
                                 B = 2000L, seed = 1L) {
  population <- match.arg(population)
  .rmse <- function(e) sqrt(mean(e^2))
  .mae  <- function(e) mean(abs(e))

  per_fold_all <- list(); boot_all <- list()
  for (pos in L2_RECON_POSITIONS) {
    fp <- sb[sb$position == pos, , drop = FALSE]
    if (population == "movers") fp <- fp[fp$is_mover, , drop = FALSE]
    fp <- fp[stats::complete.cases(
      fp[, c("target_ppg","pred_base","pred_a2","r29_proxy")]), , drop = FALSE]
    if (nrow(fp) < 10L) next

    wvec <- if (identical(w, "proxy")) fp$w_proxy else as.numeric(w)
    r32_base <- wvec * fp$pred_base + (1 - wvec) * fp$r29_proxy
    r32_a2   <- wvec * fp$pred_a2   + (1 - wvec) * fp$r29_proxy
    eb <- fp$target_ppg - r32_base
    ea <- fp$target_ppg - r32_a2

    per_fold_all[[length(per_fold_all) + 1L]] <- tibble(
      position = pos, target_year = fp$target_year, err_base = eb, err_a2 = ea
    ) %>%
      dplyr::group_by(position, target_year) %>%
      dplyr::summarise(
        n = dplyr::n(),
        rmse_base = .rmse(err_base), rmse_a2 = .rmse(err_a2),
        mae_base  = .mae(err_base),  mae_a2  = .mae(err_a2),
        .groups = "drop"
      ) %>%
      dplyr::mutate(rmse_gain = rmse_base - rmse_a2,
                    mae_gain  = mae_base  - mae_a2,
                    a2_wins   = rmse_gain > 0)

    set.seed(seed)
    idx_by_player <- split(seq_len(nrow(fp)), fp$player_id)
    players <- names(idx_by_player)
    d_rmse <- numeric(B); d_mae <- numeric(B)
    for (b in seq_len(B)) {
      samp <- sample(players, length(players), replace = TRUE)
      idx  <- unlist(idx_by_player[samp], use.names = FALSE)
      d_rmse[b] <- .rmse(eb[idx]) - .rmse(ea[idx])
      d_mae[b]  <- .mae(eb[idx])  - .mae(ea[idx])
    }

    boot_all[[length(boot_all) + 1L]] <- tibble(
      position     = pos,
      w            = if (identical(w, "proxy")) "proxy" else format(round(as.numeric(w), 2), nsmall = 2),
      population   = population,
      metric       = c("rmse", "mae"),
      point_gain   = c(.rmse(eb) - .rmse(ea), .mae(eb) - .mae(ea)),
      ci_lo        = c(stats::quantile(d_rmse, 0.025), stats::quantile(d_mae, 0.025)),
      ci_hi        = c(stats::quantile(d_rmse, 0.975), stats::quantile(d_mae, 0.975)),
      p_a2_better  = c(mean(d_rmse > 0), mean(d_mae > 0))
    )
  }

  list(per_fold = dplyr::bind_rows(per_fold_all),
       boot     = dplyr::bind_rows(boot_all))
}
