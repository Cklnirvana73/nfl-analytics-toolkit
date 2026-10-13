# ==============================================================================
# NFL Analytics Toolkit -- Season 3 -- DIAGNOSTIC (throwaway, run-once)
# Translation Signal Decay by NFL Experience
# File: diagnostics/s3_diag_translation_decay.R
#
# PURPOSE
# -------
# Set the R/29 lifecycle "phase-out" upper bound empirically instead of by
# guess. The phase-out is the years_exp level above which the college
# translation (pred_base) gets ZERO weight in a player's prior, because by then
# his actual NFL history tells the whole story and the college signal is dead.
#
# This script measures, per position and per experience level, how much the
# translation still ADDS once NFL-history-to-date is already known. Where that
# incremental value collapses into noise is the phase-out cutoff.
#
# It writes nothing the pipeline depends on. Run once, read the curve, set the
# constant in R/29, move on. If you want to revisit it each offseason as new
# draft classes mature, promote it to a real R-number then.
#
# THE QUESTION, MADE CONCRETE
# ---------------------------
# For a player at years_exp = k, two things predict his actual PPG this season:
#   (1) his NFL history to date (career mean PPG through the PRIOR season)
#   (2) his college translation, pred_base
# If (1) already explains the outcome, (2) is dead weight. We measure (2)'s
# incremental contribution after (1), at each k, and watch it decay.
#
# TWO GUARDS AGAINST SILENT BUGS (called out, not buried)
# -------------------------------------------------------
# 1. NO OUTCOME LEAK. "History to date" is built STRICTLY from seasons BEFORE
#    the season being predicted. Including the current season in its own
#    predictor would make history explain everything and make the translation
#    look falsely dead. The career mean excludes the current row by construction.
# 2. UNDEFINED HISTORY IS REPORTED, NOT DROPPED SILENTLY. A player at years_exp
#    k with zero prior qualifying seasons has no history control. The count of
#    such players is printed at each level -- it is itself informative about how
#    thin early careers are.
#
# KNOWN LIMITATION
# ----------------
# Training outcomes run NFL years 1-3 for draft classes 2015-2023, so player-
# seasons thin out fast above years_exp 3-4. Estimates in the cutoff region are
# noisy. n is printed on EVERY row so noisy points read as noisy. The phase-out
# floor is already fixed at 3 qualifying seasons (R/29), so the cutoff must land
# ABOVE 3; the table starts at years_exp 3 and climbs.
#
# INPUTS (confirmed present; id formats match, 494/494 overlap)
# -------------------------------------------------------------
#   data/season2_cache/s2_week15_player_season_panel_cache.rds  (R/16 panel,
#       9488 player-seasons: player_id, season, position, games_played, raw
#       scoring stats)
#   data/season2_cache/s2_week10_predictions.rds                (R/24 v1 LOCO:
#       nfl_gsis_id, draft_year, draft_position, pred_base, ...)
#
# OUTPUT
# ------
# Console table per position, ordered by years_exp, n on every row. Optional
# PNG if WRITE_PLOT = TRUE (off by default; this is a read-once diagnostic).
#
# SCHEMA TAG: s3_diag_v1
# ==============================================================================


# ==============================================================================
# LIBRARIES
# ==============================================================================

library(dplyr)
library(tidyr)
library(glue)
library(here)


# ==============================================================================
# CONFIGURATION
# ==============================================================================

# PPR scoring constants -- match R/29 DEFAULT_SCORING_SETTINGS / R/24 exactly.
PPR_PASS_YD   <-  0.04
PPR_PASS_TD   <-  4.00
PPR_INT       <- -2.00
PPR_RUSH_YD   <-  0.10
PPR_RUSH_TD   <-  6.00
PPR_RECEPTION <-  1.00
PPR_REC_YD    <-  0.10
PPR_REC_TD    <-  6.00

# A season counts as "real" history / a valid outcome only with enough games.
# Matches the spirit of R/29 .compute_player_historical_stats min_games = 8.
MIN_GAMES_QUALIFYING <- 8L

# Experience levels to report. Floor is fixed at 3 (R/29), so the cutoff lives
# at 3 or above; report from 3 up. Rows above MAX_EXP are too thin to read.
MIN_EXP_REPORT <- 3L
MAX_EXP_REPORT <- 8L

# Minimum players at an (position, years_exp) cell to attempt the comparison.
# Below this the cell is reported as n only, no error/correlation estimate.
MIN_CELL_N <- 8L

POSITIONS <- c("QB", "RB", "WR", "TE")

WRITE_PLOT <- FALSE   # read-once diagnostic; flip on only if you want a PNG

PANEL_PATH <- here::here(
  "data", "season2_cache", "s2_week15_player_season_panel_cache.rds"
)
LOCO_PATH  <- here::here(
  "data", "season2_cache", "s2_week10_predictions.rds"
)
PLOT_PATH  <- here::here("output", "plots", "s3_diag_translation_decay.png")


# ==============================================================================
# HELPERS
# ==============================================================================

# ------------------------------------------------------------------------------
# .ppg_from_panel
# Compute PPR points per game for each panel player-season. NA stat categories
# mean "no activity" (a TE has NA passing_yards), so coalesce to 0 before
# summing -- otherwise NA propagates and the season silently drops. Matches the
# R/29 volume-blend coalesce rationale.
# ------------------------------------------------------------------------------
.ppg_from_panel <- function(panel) {
  panel %>%
    dplyr::mutate(
      ppr_points =
        dplyr::coalesce(passing_yards, 0)        * PPR_PASS_YD +
        dplyr::coalesce(pass_tds, 0)             * PPR_PASS_TD +
        dplyr::coalesce(interceptions_thrown, 0) * PPR_INT +
        dplyr::coalesce(rushing_yards, 0)        * PPR_RUSH_YD +
        dplyr::coalesce(rush_tds, 0)             * PPR_RUSH_TD +
        dplyr::coalesce(receiving_yards, 0)      * PPR_REC_YD +
        dplyr::coalesce(rec_tds, 0)              * PPR_REC_TD +
        dplyr::coalesce(receptions, 0)           * PPR_RECEPTION,
      ppg = dplyr::if_else(games_played > 0,
                           ppr_points / games_played, NA_real_)
    )
}

# ------------------------------------------------------------------------------
# .career_mean_to_date
# For each player-season, the mean PPG over that player's PRIOR qualifying
# seasons only (strictly season < current). No current-season leak. Returns NA
# when the player has no prior qualifying season.
# ------------------------------------------------------------------------------
.career_mean_to_date <- function(df) {
  df %>%
    dplyr::arrange(player_id, season) %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(
      # cumulative mean of PPG over strictly-prior qualifying seasons
      .qual          = !is.na(ppg) & games_played >= MIN_GAMES_QUALIFYING,
      .cum_pts_prior = dplyr::lag(cumsum(dplyr::if_else(.qual, ppg, 0)), default = 0),
      .cum_n_prior   = dplyr::lag(cumsum(dplyr::if_else(.qual, 1L, 0L)), default = 0L),
      history_to_date = dplyr::if_else(.cum_n_prior > 0L,
                                       .cum_pts_prior / .cum_n_prior,
                                       NA_real_),
      n_prior_seasons = .cum_n_prior
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.qual, -.cum_pts_prior, -.cum_n_prior)
}

# ------------------------------------------------------------------------------
# .cell_stats
# For one (position, years_exp) cell, fit two nested models and report the
# translation's incremental value. Both models predict current-season ppg.
#   m_hist : ppg ~ history_to_date
#   m_full : ppg ~ history_to_date + pred_base
# Reports in-sample RMSE for each (this is an explanatory decay read, not a
# predictive model), the RMSE reduction, the partial correlation of pred_base
# with ppg after conditioning on history, and its p-value.
# ------------------------------------------------------------------------------
.cell_stats <- function(cell) {

  # Require both predictors present and non-degenerate.
  d <- dplyr::filter(cell, !is.na(ppg), !is.na(history_to_date),
                     !is.na(pred_base))
  n <- nrow(d)

  out <- tibble::tibble(
    n_with_history = n,
    rmse_hist      = NA_real_,
    rmse_full      = NA_real_,
    rmse_reduction = NA_real_,
    partial_r      = NA_real_,
    partial_p      = NA_real_
  )

  if (n < MIN_CELL_N) return(out)
  if (stats::sd(d$pred_base) == 0 || stats::sd(d$history_to_date) == 0) {
    return(out)
  }

  m_hist <- stats::lm(ppg ~ history_to_date, data = d)
  m_full <- stats::lm(ppg ~ history_to_date + pred_base, data = d)

  rmse <- function(m) sqrt(mean(stats::residuals(m)^2))

  # Partial correlation of pred_base with ppg, controlling for history:
  # correlate the residuals of ppg~history with residuals of pred_base~history.
  r_ppg  <- stats::residuals(stats::lm(ppg ~ history_to_date, data = d))
  r_pred <- stats::residuals(stats::lm(pred_base ~ history_to_date, data = d))
  pc     <- suppressWarnings(stats::cor.test(r_ppg, r_pred))

  out$rmse_hist      <- rmse(m_hist)
  out$rmse_full      <- rmse(m_full)
  out$rmse_reduction <- rmse(m_hist) - rmse(m_full)
  out$partial_r      <- unname(pc$estimate)
  out$partial_p      <- pc$p.value
  out
}


# ==============================================================================
# MAIN
# ==============================================================================

run_translation_decay_diagnostic <- function(verbose = TRUE) {

  if (verbose) {
    message(strrep("=", 70))
    message("s3_diag_translation_decay -- phase-out cutoff diagnostic")
    message(strrep("=", 70))
  }

  stopifnot(file.exists(PANEL_PATH), file.exists(LOCO_PATH))

  panel <- readRDS(PANEL_PATH)
  loco  <- readRDS(LOCO_PATH)

  # Draft year + translation, one row per player (LOCO is already one per id).
  player_meta <- loco %>%
    dplyr::select(nfl_gsis_id, draft_year, draft_position, pred_base) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE)

  # PPG per panel season, then attach draft_year/pred_base, derive experience.
  scored <- .ppg_from_panel(panel) %>%
    dplyr::inner_join(player_meta, by = c("player_id" = "nfl_gsis_id")) %>%
    dplyr::mutate(years_exp = season - draft_year) %>%
    dplyr::filter(years_exp >= 0L)

  # Use the draft_position from the model (stable position label), not the
  # panel's per-season position which can wobble.
  scored <- dplyr::filter(scored, draft_position %in% POSITIONS)

  # Career-mean-to-date control (strictly prior qualifying seasons).
  scored <- .career_mean_to_date(scored)

  # The outcome row must itself be a qualifying season (enough games to be a
  # real outcome, not a 2-game cameo).
  scored_outcome <- dplyr::filter(scored, games_played >= MIN_GAMES_QUALIFYING)

  if (verbose) {
    message(glue(
      "\nPlayer-seasons after join: {nrow(scored)} | ",
      "qualifying-outcome rows: {nrow(scored_outcome)}"
    ))
    message(glue(
      "Experience coverage (qualifying-outcome rows) by position:"
    ))
    cov <- scored_outcome %>%
      dplyr::filter(years_exp >= MIN_EXP_REPORT, years_exp <= MAX_EXP_REPORT) %>%
      dplyr::count(draft_position, years_exp) %>%
      tidyr::pivot_wider(names_from = years_exp, values_from = n,
                         values_fill = 0) %>%
      dplyr::arrange(match(draft_position, POSITIONS))
    print(as.data.frame(cov))
  }

  # Per (position, years_exp) cell stats.
  results <- list()
  for (pos in POSITIONS) {
    for (k in MIN_EXP_REPORT:MAX_EXP_REPORT) {
      cell <- dplyr::filter(scored_outcome,
                            draft_position == pos, years_exp == k)
      n_total_cell   <- nrow(cell)
      n_no_history   <- sum(is.na(cell$history_to_date))
      cs <- .cell_stats(cell)
      results[[length(results) + 1L]] <- dplyr::bind_cols(
        tibble::tibble(
          position        = pos,
          years_exp       = k,
          n_cell          = n_total_cell,
          n_no_history    = n_no_history
        ),
        cs
      )
    }
  }
  decay <- dplyr::bind_rows(results)

  # ---- Print per-position decay tables ----
  if (verbose) {
    for (pos in POSITIONS) {
      message(glue("\n--- {pos}: translation incremental value by experience ---"))
      tbl <- decay %>%
        dplyr::filter(position == pos) %>%
        dplyr::transmute(
          years_exp,
          n_cell,
          n_with_history = n_with_history,
          n_no_history,
          rmse_hist      = round(rmse_hist, 3),
          rmse_full      = round(rmse_full, 3),
          rmse_reduction = round(rmse_reduction, 3),
          partial_r      = round(partial_r, 3),
          partial_p      = round(partial_p, 4)
        )
      print(as.data.frame(tbl))
    }
    message(glue(
      "\nREADING THIS TABLE:\n",
      "  rmse_reduction > 0 and partial_p < 0.05  -> translation still adds ",
      "real signal at this experience level.\n",
      "  rmse_reduction near 0 or partial_p high   -> signal is dead; this is ",
      "a phase-out candidate.\n",
      "  Watch n_cell / n_with_history -- small cells are noisy, weight them ",
      "less.\n",
      "  Phase-out cutoff = the lowest years_exp at/after which the signal is ",
      "consistently dead. Must be > 3 (the fixed history floor)."
    ))
  }

  if (WRITE_PLOT && requireNamespace("ggplot2", quietly = TRUE)) {
    plot_df <- dplyr::filter(decay, !is.na(rmse_reduction))
    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = years_exp, y = rmse_reduction)
    ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          color = "grey50") +
      ggplot2::geom_line(ggplot2::aes(color = position), linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(color = position, size = n_with_history)) +
      ggplot2::labs(
        title = "Translation incremental value decays with NFL experience",
        subtitle = "RMSE reduction from adding pred_base after NFL history; >0 = still useful",
        x = "Years of NFL experience", y = "RMSE reduction (PPR/game)",
        color = "Position", size = "n (with history)"
      ) +
      ggplot2::theme_minimal(base_size = 13)
    if (!dir.exists(dirname(PLOT_PATH))) {
      dir.create(dirname(PLOT_PATH), recursive = TRUE)
    }
    ggplot2::ggsave(PLOT_PATH, p, width = 9, height = 6, dpi = 300)
    if (verbose) message(glue("\nPlot written: {PLOT_PATH}"))
  }

  invisible(decay)
}


# ==============================================================================
# EXECUTION
# ==============================================================================

decay_results <- run_translation_decay_diagnostic()
