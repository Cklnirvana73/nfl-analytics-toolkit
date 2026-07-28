# ==============================================================================
# 44_volume_weight_sweep.R
# ==============================================================================
#
# PURPOSE (Slot 3)
# ----------------
# Find the out-of-sample-best value of R/29's volume weight per position,
# instead of resting on the round 70/30 (RB/WR/TE) and QB 50/50 that the Week 4
# study justified only in DIRECTION (volume > efficiency), never as exact
# numbers.
#
# EXACT, NOT PROXY. It reconstructs the real R/29 blend from R/29's OWN helpers,
# so the swept quantity is the production quantity:
#   prior_mu_pre_age = .blend_prior_components(mu_translation, sigma_translation,
#                                              hist_mean, hist_sigma,
#                                              n_nfl_seasons, n_games_total)
#   pred(w)          = prior + clamp(w * (volume_implied_ppg - prior), -4, +4)
# The clamp and the blend rule are copied from R/29 line-for-line.
#
# INPUTS (all real artifacts already on disk)
#   hist_mean / hist_sigma / n_nfl_seasons : .compute_player_historical_stats()
#   sigma_translation                      : .load_translation_sigmas() (v1 perf)
#   mu_translation                         : R/24 holdout preds
#       (pred_base for QB, pred_enriched for RB/WR/TE -- the variant R/29 uses)
#   volume_implied_ppg                     : R/16 panel, mirrored from R/29's
#                                            .compute_volume_efficiency_blend()
#   target                                 : actual next-season PPG (same scorer,
#                                            same pbp path as the history helper)
#
# POPULATION
#   Player-seasons WITH NFL history (n_nfl_seasons >= 1), because the volume
#   blend only applies to has_history players in R/29. Rookies-at-entry are out
#   of scope here (their blend is translation-only, no volume term).
#
# METHOD
#   Forward-chained by season (R/43-style): for each target year ty, the best
#   w on the grid is chosen from the pooled years < ty and evaluated on ty
#   only. Per-fold chosen weights and per-fold + pooled OOS RMSE are
#   reported. The pooled same-rows sweep (choose AND evaluate on all years at
#   once) is retained for reference but labeled rmse_insample -- it is
#   selection-biased and must not be read as held-out. A player-clustered
#   bootstrap CI on the in-sample argmin is also reported so we adopt a
#   number only if it is clear of the current one, same bar as the
#   coach-prior retirement.
#
# RUNTIME
#   Rebuilds league-scored history per target year (re-scores overlapping
#   seasons). Heavy. assemble_weight_frame() caches so it is paid once.
#
# DEPENDENCIES (guarded)
#   R/29_projection_engine.R : .compute_player_historical_stats,
#                              .load_translation_sigmas, .blend_prior_components,
#                              DEFAULT_SCORING_SETTINGS, SUPPORTED_POSITIONS,
#                              CACHE_DIR_DEFAULT, load_normalized_season,
#                              calculate_fantasy_points_ext
#   R/16_player_season_panel.R : panel (via cache)
#
# VERSION
#   1.0  Initial build. Exact-blend OOS weight sweep.
# ==============================================================================

library(dplyr)
library(tibble)
library(purrr)
library(glue)
library(here)

source(here::here("R", "29_projection_engine.R"))

for (fn in c(".compute_player_historical_stats", ".load_translation_sigmas",
             ".blend_prior_components", "load_normalized_season",
             "calculate_fantasy_points_ext")) {
  if (!exists(fn)) {
    stop(glue("44_volume_weight_sweep.R requires {fn}() from R/29, not found."))
  }
}

# Paths to the real artifacts (defaults match the season2 cache layout).
PERF_PATH_DEFAULT   <- here::here("data", "season2_cache", "s2_week10_performance.rds")
PREDS_PATH_DEFAULT  <- here::here("data", "season2_cache", "s2_week10_predictions.rds")
PANEL_CACHE_DEFAULT <- if (exists("AGING_PANEL_CACHE_PATH")) AGING_PANEL_CACHE_PATH else
  here::here("data", "season2_cache", "s2_week15_player_season_panel_cache.rds")

# Current production settings, for the confirm-or-move comparison.
CURRENT_W_QB    <- 0.50
CURRENT_W_OTHER <- 0.70

# R/29's blend cap, copied so the reconstruction is exact.
.blend_clamp <- function(x) pmax(pmin(x, 4.0), -4.0)


# ------------------------------------------------------------------------------
# .volume_implied_asof  (mirrors R/29 .compute_volume_efficiency_blend exactly)
# ------------------------------------------------------------------------------
.volume_implied_asof <- function(panel_data, season, scoring_settings,
                                 min_games = 8L) {
  sc <- utils::modifyList(DEFAULT_SCORING_SETTINGS, scoring_settings %||% list())
  prior_seasons <- (season - 3L):(season - 1L)

  pu <- panel_data %>%
    dplyr::filter(
      season %in% prior_seasons,
      games_played >= min_games,
      !is.na(player_id),
      position_group %in% SUPPORTED_POSITIONS | position %in% SUPPORTED_POSITIONS
    ) %>%
    dplyr::mutate(pos_use = dplyr::case_when(
      position %in% SUPPORTED_POSITIONS ~ position,
      position_group %in% SUPPORTED_POSITIONS ~ position_group,
      TRUE ~ NA_character_
    )) %>%
    dplyr::filter(!is.na(pos_use))

  if (nrow(pu) == 0L) {
    return(tibble(player_id = character(), volume_implied_ppg = numeric()))
  }
  if (!"sacks_taken" %in% names(pu)) pu$sacks_taken <- 0

  pu <- pu %>%
    dplyr::mutate(
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
      opp_per_game = dplyr::case_when(
        pos_use == "QB" ~ (dplyr::coalesce(qb_dropbacks, 0L) +
                           dplyr::coalesce(rush_attempts, 0L)) / pmax(games_played, 1),
        pos_use == "RB" ~ (dplyr::coalesce(rush_attempts, 0L) +
                           dplyr::coalesce(targets, 0L)) / pmax(games_played, 1),
        pos_use %in% c("WR", "TE") ~ dplyr::coalesce(targets, 0L) / pmax(games_played, 1),
        TRUE ~ 0
      )
    ) %>%
    dplyr::filter(opp_per_game > 0, is.finite(panel_ppg))

  pos_eff <- pu %>%
    dplyr::filter(!low_volume) %>%
    dplyr::group_by(season, pos_use) %>%
    dplyr::summarise(median_ppg_per_opp = stats::median(panel_ppg / opp_per_game, na.rm = TRUE),
                     .groups = "drop")

  pu %>%
    dplyr::left_join(pos_eff, by = c("season", "pos_use")) %>%
    dplyr::mutate(vip_season = opp_per_game * dplyr::coalesce(median_ppg_per_opp, 1)) %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(volume_implied_ppg = mean(vip_season, na.rm = TRUE),
                     n_panel_seasons = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n_panel_seasons >= 1L, is.finite(volume_implied_ppg))
}


# ------------------------------------------------------------------------------
# .season_ppg  (target: actual PPG in one season, same scorer/path as history)
# ------------------------------------------------------------------------------
.season_ppg <- function(season, scoring_settings, min_games = 8L,
                        cache_dir = CACHE_DIR_DEFAULT) {
  pbp <- load_normalized_season(season, cache_dir = cache_dir)
  fp  <- do.call(calculate_fantasy_points_ext,
                 c(list(pbp_data = pbp, season = season), scoring_settings))
  fp %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(n_games = dplyr::n_distinct(game_id),
                     target_ppg = mean(total_fantasy_points, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::filter(n_games >= min_games)
}


# ------------------------------------------------------------------------------
# assemble_weight_frame
# ------------------------------------------------------------------------------

#' Build one row per player-target-year with the exact prior, the volume-implied
#' PPG, and the actual next-season PPG.
#'
#' @param target_years Integer vector of seasons to predict.
#' @param scoring_settings Ruleset (defaults to DEFAULT_SCORING_SETTINGS).
#' @param min_games Integer qualifying games. Default 8L (matches R/29 history).
#' @param perf_path,preds_path,panel_cache_path Artifact paths.
#' @param cache_path Character or NULL. If given, saved/loaded as .rds.
#' @param verbose Logical.
#'
#' @return tibble(player_id, position, target_year, prior_mu, volume_implied_ppg,
#'   target_ppg).
assemble_weight_frame <- function(target_years,
                                  scoring_settings = DEFAULT_SCORING_SETTINGS,
                                  min_games        = 8L,
                                  perf_path        = PERF_PATH_DEFAULT,
                                  preds_path       = PREDS_PATH_DEFAULT,
                                  panel_cache_path = PANEL_CACHE_DEFAULT,
                                  cache_path       = NULL,
                                  verbose          = TRUE) {

  if (!is.null(cache_path) && file.exists(cache_path)) {
    if (verbose) message(glue("assemble_weight_frame(): loading cache {cache_path}"))
    return(readRDS(cache_path))
  }

  sigmas <- .load_translation_sigmas(perf_path)          # named QB/RB/WR/TE
  preds  <- readRDS(preds_path) %>%
    dplyr::transmute(player_id = nfl_gsis_id, pred_base, pred_enriched)
  panel  <- readRDS(panel_cache_path)

  frames <- lapply(target_years, function(ty) {
    if (verbose) message(glue("  target {ty}..."))

    hist <- .compute_player_historical_stats(
      season = ty, min_games = min_games, scoring_settings = scoring_settings
    )                                                    # player_id, position, n_nfl_seasons, n_games_total, hist_mean, hist_sigma
    if (nrow(hist) == 0L) return(NULL)

    vip <- .volume_implied_asof(panel, ty, scoring_settings, min_games)
    tgt <- .season_ppg(ty, scoring_settings, min_games)

    hist %>%
      dplyr::inner_join(vip, by = "player_id") %>%
      dplyr::inner_join(tgt %>% dplyr::select(player_id, target_ppg), by = "player_id") %>%
      dplyr::left_join(preds, by = "player_id") %>%
      dplyr::mutate(
        # variant R/29 uses for the translation prior: base for QB, enriched else
        mu_translation    = dplyr::if_else(position == "QB", pred_base, pred_enriched),
        sigma_translation  = unname(sigmas[position]),
        has_translation    = !is.na(mu_translation) & !is.na(sigma_translation),
        # exact prior: blend when a translation exists, else history-only
        prior_mu = purrr::pmap_dbl(
          list(mu_translation, sigma_translation, hist_mean, hist_sigma,
               n_nfl_seasons, n_games_total, has_translation),
          function(mt, st, mh, sh, n, ng, ht) {
            if (!isTRUE(ht)) return(mh)
            .blend_prior_components(mt, st, mh, sh, n, ng)$prior_mu
          }
        ),
        target_year = ty
      ) %>%
      dplyr::select(player_id, position, target_year, prior_mu,
                    volume_implied_ppg, target_ppg)
  })

  frame <- dplyr::bind_rows(frames)

  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(frame, cache_path)
    if (verbose) message(glue("assemble_weight_frame(): cached -> {cache_path}"))
  }

  frame
}


# ------------------------------------------------------------------------------
# run_volume_weight_sweep
# ------------------------------------------------------------------------------

#' Sweep the volume weight per position, forward-chained by season, with a
#' bootstrap CI on the in-sample argmin.
#'
#' The headline numbers are FORWARD-CHAINED (R/43-style): for each target year
#' ty, the weight is chosen on the pooled years < ty and evaluated on ty only;
#' fold results are pooled for the OOS RMSE. The pooled same-rows sweep
#' (choose and evaluate on the same rows) is retained for reference but
#' explicitly named *_insample -- it is selection-biased.
#'
#' @param frame Output of assemble_weight_frame().
#' @param grid Numeric weight grid. Default seq(0, 1, 0.05).
#' @param B Integer bootstrap resamples of the argmin. Default 2000L.
#' @param seed Integer. Default 1L.
#' @param min_train Integer. Minimum pooled training rows for a fold. Default 20L.
#'
#' @return list(summary, folds, curve):
#'   summary  per position: n, rmse_oos_pooled + oos_folds (forward-chained),
#'            best_w_insample, rmse_insample (same-rows reference),
#'            current_w, rmse_current (fixed weight, no selection), and the
#'            player-clustered CI on the IN-SAMPLE argmin.
#'   folds    per (position, target_year): w_chosen on years < ty, n_train,
#'            n_test, rmse_oos at ty.
#'   curve    the full same-rows RMSE(w) per position (reference only).
run_volume_weight_sweep <- function(frame, grid = seq(0, 1, 0.05),
                                    B = 2000L, seed = 1L, min_train = 20L) {

  set.seed(seed)
  .pred_at <- function(prior, vip, w) prior + .blend_clamp(w * (vip - prior))
  .rmse    <- function(a, p) sqrt(mean((a - p)^2))

  summ <- list(); curves <- list(); folds <- list()

  for (p in c("QB", "RB", "WR", "TE")) {
    fp <- frame[frame$position == p, , drop = FALSE]
    fp <- fp[stats::complete.cases(fp[, c("prior_mu", "volume_implied_ppg", "target_ppg")]), ]
    if (nrow(fp) < 20L) next

    cur_w  <- if (p == "QB") CURRENT_W_QB else CURRENT_W_OTHER
    # Fixed weight, no selection: evaluating on all rows is unbiased.
    rmse_cur <- .rmse(fp$target_ppg, .pred_at(fp$prior_mu, fp$volume_implied_ppg, cur_w))

    # ---- forward-chained OOS sweep (headline numbers) ----
    years <- sort(unique(fp$target_year))
    oos_pred <- numeric(0); oos_act <- numeric(0)
    for (ty in years) {
      train <- fp[fp$target_year <  ty, , drop = FALSE]
      test  <- fp[fp$target_year == ty, , drop = FALSE]
      if (nrow(train) < min_train || nrow(test) == 0L) next

      rmse_train <- vapply(grid, function(w)
        .rmse(train$target_ppg,
              .pred_at(train$prior_mu, train$volume_implied_ppg, w)),
        numeric(1))
      w_fold <- grid[which.min(rmse_train)]
      pr <- .pred_at(test$prior_mu, test$volume_implied_ppg, w_fold)

      folds[[length(folds) + 1L]] <- tibble(
        position    = p,
        target_year = ty,
        n_train     = nrow(train),
        n_test      = nrow(test),
        w_chosen    = w_fold,
        rmse_oos    = .rmse(test$target_ppg, pr)
      )
      oos_pred <- c(oos_pred, pr)
      oos_act  <- c(oos_act, test$target_ppg)
    }
    n_folds <- sum(vapply(folds, function(f) f$position[1] == p, logical(1)))
    rmse_oos_pooled <- if (length(oos_pred) > 0L) {
      .rmse(oos_act, oos_pred)
    } else {
      NA_real_
    }

    # ---- same-rows sweep (IN-SAMPLE reference; selection-biased) ----
    rmse_by_w <- vapply(grid, function(w)
      .rmse(fp$target_ppg, .pred_at(fp$prior_mu, fp$volume_implied_ppg, w)),
      numeric(1))
    best_w_insample <- grid[which.min(rmse_by_w)]

    # player-clustered bootstrap of the IN-SAMPLE argmin (stability check on
    # the same-rows optimum, not an OOS statement)
    idx_by_player <- split(seq_len(nrow(fp)), fp$player_id)
    players <- names(idx_by_player)
    best_boot <- vapply(seq_len(B), function(b) {
      idx <- unlist(idx_by_player[sample(players, length(players), replace = TRUE)],
                    use.names = FALSE)
      a <- fp$target_ppg[idx]; pr <- fp$prior_mu[idx]; v <- fp$volume_implied_ppg[idx]
      grid[which.min(vapply(grid, function(w) .rmse(a, .pred_at(pr, v, w)), numeric(1)))]
    }, numeric(1))

    summ[[length(summ) + 1L]] <- tibble(
      position        = p,
      n               = nrow(fp),
      oos_folds       = n_folds,
      rmse_oos_pooled = rmse_oos_pooled,
      best_w_insample = best_w_insample,
      rmse_insample   = min(rmse_by_w),
      current_w       = cur_w,
      rmse_current    = rmse_cur,
      w_ci_lo         = unname(stats::quantile(best_boot, 0.025)),
      w_ci_hi         = unname(stats::quantile(best_boot, 0.975))
    )
    curves[[p]] <- tibble(position = p, w = grid, rmse_insample = rmse_by_w)
  }

  list(summary = dplyr::bind_rows(summ),
       folds   = dplyr::bind_rows(folds),
       curve   = dplyr::bind_rows(curves))
}
