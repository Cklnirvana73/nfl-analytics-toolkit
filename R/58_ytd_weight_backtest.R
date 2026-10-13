# ==============================================================================
# 58_ytd_weight_backtest.R
#
# Out-of-sample gate for the R/29 in-season update rule.  [2026-09-18]
# [2026-10-01] Outputs keyed per scoring (see Output below).
#
# WHAT IS BEING TESTED
#   R/29 blends a preseason prior with the season-to-date mean:
#     posterior_mu = (1 - w) * prior_mu + w * ytd_mean
#   Production sets w from the CALENDAR week: compute_prior_weight() declines
#   linearly from 1.0 at week 1 to 0.05 at week 18, crossover ~week 9. That
#   schedule was carried forward from R/14 and has never been scored. It also
#   ignores how many games a player has actually played.
#
#   The candidate is the empirical-Bayes form R/32 already uses for omega:
#     w = ytd_n / (ytd_n + k)
#   with k fitted per position. k is the number of observed games at which the
#   prior and the observed mean carry equal weight.
#
# DESIGN
#   Target seasons 2023-2025. For each, the prior is built from the three
#   preceding seasons with R/49's estimators (.build_history_estimates), the
#   same construction R/50 uses for its residual pool. That prior is a PROXY
#   for R/29's: it is the history arm only, with no translation, aging, injury
#   or volume terms. What is gated is the weighting rule, not the prior.
#
#   Unit of analysis: player-game in the target season with at least one
#   earlier game that season. For each, the season-to-date mean over the
#   EARLIER games only predicts THIS game's points. No target-season survival
#   filter is applied, so a player benched after two games still contributes
#   his two games (R/49's gate frame requires four target games; that filter
#   is deliberately not reused here because it would remove exactly the rows
#   an in-season update most needs to get right).
#
#   Fit: k per position by minimum RMSE on the train seasons.
#   Gate (v2, leave-one-season-out): each target season is held out once with
#   k fitted on the others; paired errors from all holdouts are pooled and the
#   decision per position is a cluster bootstrap (by player) of the RMSE
#   difference. A position passes when the 90 percent interval on the
#   improvement excludes zero in favor of the games-based rule.
#
#   Output: s3_r58_ytd_k.rds under the default (flat PPR) scoring; under any
#   other scoring s3_r58_ytd_k_<scoring key>.rds (all three outputs are keyed
#   the same way, via R/29 scoring_gate_path()). [2026-10-01] The k object
#   records scoring_key and scoring_settings. R/29 reads the table for its own
#   scoring via load_ytd_k_table(scoring_settings = ...). Positions
#   that fail the gate are omitted from k_by_position, and R/29 keeps the
#   linear schedule for them. If no position passes, gate_passed is FALSE and
#   R/29 behaves exactly as before.
#
# LIMITS (stated so they are not forgotten)
#   - The prior proxy has no rookies (no history) and is weaker than R/29's
#     blended prior, so k may be slightly LOW for production: a stronger prior
#     deserves more weight, i.e. a larger k. Direction of the bias is known.
#   - Played games only. A DNP is not a zero.
#   - Three target seasons, one holdout. This is a first pass, not a final k.
#
# USAGE
#   source(here::here("R", "58_ytd_weight_backtest.R"))
#   res <- run_ytd_weight_backtest()
#   res_fd <- run_ytd_weight_backtest(scoring_settings = FANDUEL_SCORING)
# ==============================================================================

library(dplyr)
library(glue)
library(purrr)
library(readr)
source(here::here("R", "29_projection_engine.R"))
source(here::here("R", "49_uncertainty_calibration_gate.R"))

R58_OUTPUT_DIR   <- here::here("data", "season3_cache")
R58_K_PATH       <- file.path(R58_OUTPUT_DIR, "s3_r58_ytd_k.rds")
R58_RESULTS_PATH <- file.path(R58_OUTPUT_DIR, "s3_r58_ytd_backtest.rds")
R58_SUMMARY_CSV  <- file.path(R58_OUTPUT_DIR, "s3_r58_ytd_backtest_summary.csv")
R58_SCHEMA_TAG   <- "s3_r58_ytd_gate_v2_loso"

R58_TARGET_SEASONS <- 2023:2025
R58_K_GRID <- c(1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 25, 30, 40, 50, 75, 100)
R58_POSITIONS <- c("QB", "RB", "WR", "TE")
R58_BOOT_REPS <- 500L
R58_BOOT_LEVEL <- 0.90
R58_SEED <- 20260918L

# ------------------------------------------------------------------------------
# .r58_player_weeks: one target season's player-game rows with prior + YTD
# ------------------------------------------------------------------------------
.r58_player_weeks <- function(target_season, scoring_settings) {

  target_season   <- as.integer(target_season)
  history_seasons <- (target_season - 3L):(target_season - 1L)

  hist_pts <- purrr::map_dfr(history_seasons, function(s) {
    .season_game_points(s, scoring_settings, cache_dir = GATE_CACHE_DIR)
  })
  target_pts <- .season_game_points(target_season, scoring_settings,
                                     cache_dir = GATE_CACHE_DIR)
  if (nrow(hist_pts) == 0 || is.null(target_pts) || nrow(target_pts) == 0) {
    message(glue("  target {target_season}: missing history or target cache"))
    return(NULL)
  }

  hist_est <- .build_history_estimates(hist_pts, target_season = target_season)

  target_pts %>%
    dplyr::filter(position %in% R58_POSITIONS, !is.na(game_points)) %>%
    dplyr::inner_join(
      hist_est %>% dplyr::select(player_id, position, prior_mu = pred_mu),
      by = c("player_id", "position")
    ) %>%
    dplyr::arrange(player_id, week, game_id) %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(
      ytd_n    = dplyr::row_number() - 1L,
      ytd_sum  = cumsum(game_points) - game_points,
      ytd_mean = dplyr::if_else(ytd_n > 0L, ytd_sum / ytd_n, NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(ytd_n >= 1L, week >= 2L, week <= NFL_REGULAR_SEASON_MAX_WEEK) %>%
    dplyr::mutate(target = target_season) %>%
    dplyr::select(target, player_id, position, week, game_id, game_points,
                  prior_mu, ytd_n, ytd_mean)
}

# ------------------------------------------------------------------------------
# .r58_predict: add prediction columns for every rule on the grid
# ------------------------------------------------------------------------------
.r58_predict <- function(pw, k_grid = R58_K_GRID) {

  w_lin <- vapply(pw$week, function(w) 1 - compute_prior_weight(w), numeric(1))

  out <- pw %>%
    dplyr::mutate(
      pred_prior  = prior_mu,
      pred_ytd    = ytd_mean,
      pred_linear = (1 - w_lin) * prior_mu + w_lin * ytd_mean
    )
  for (k in k_grid) {
    w <- out$ytd_n / (out$ytd_n + k)
    out[[paste0("pred_eb_", k)]] <- (1 - w) * out$prior_mu + w * out$ytd_mean
  }
  out
}

# ------------------------------------------------------------------------------
# .r58_score: long table of RMSE / MAE by season x position x rule
# ------------------------------------------------------------------------------
.r58_score <- function(pred_df) {
  pred_cols <- grep("^pred_", names(pred_df), value = TRUE)
  purrr::map_dfr(pred_cols, function(pc) {
    pred_df %>%
      dplyr::group_by(target, position) %>%
      dplyr::summarise(
        rule = pc,
        n    = dplyr::n(),
        rmse = sqrt(mean((game_points - .data[[pc]])^2)),
        mae  = mean(abs(game_points - .data[[pc]])),
        .groups = "drop"
      )
  }) %>%
    dplyr::mutate(
      k = suppressWarnings(as.numeric(sub("^pred_eb_", "", rule))),
      k = dplyr::if_else(grepl("^pred_eb_", rule), k, NA_real_)
    )
}

# ------------------------------------------------------------------------------
# .r58_boot_improvement: cluster bootstrap (by player) of RMSE(linear) - RMSE(eb)
# ------------------------------------------------------------------------------
.r58_boot_improvement <- function(df, eb_col, reps = R58_BOOT_REPS,
                                  level = R58_BOOT_LEVEL, seed = R58_SEED) {
  set.seed(seed)
  players <- unique(df$player_id)
  by_player <- split(seq_len(nrow(df)), df$player_id)
  stat <- function(idx) {
    d <- df[idx, ]
    sqrt(mean((d$game_points - d$pred_linear)^2)) -
      sqrt(mean((d$game_points - d[[eb_col]])^2))
  }
  point <- stat(seq_len(nrow(df)))
  draws <- vapply(seq_len(reps), function(i) {
    samp <- sample(players, length(players), replace = TRUE)
    stat(unlist(by_player[samp], use.names = FALSE))
  }, numeric(1))
  a <- (1 - level) / 2
  c(improvement = point,
    lo = unname(stats::quantile(draws, a)),
    hi = unname(stats::quantile(draws, 1 - a)))
}

# ------------------------------------------------------------------------------
# .r58_gate_one_holdout: fit k on the other seasons, score this one
# ------------------------------------------------------------------------------
.r58_gate_one_holdout <- function(pred, scores, holdout, train_seasons) {
  train_eb <- scores %>%
    dplyr::filter(target %in% train_seasons, !is.na(k)) %>%
    dplyr::group_by(position, k) %>%
    dplyr::summarise(rmse = sqrt(sum(rmse^2 * n) / sum(n)), .groups = "drop")
  k_fit <- train_eb %>%
    dplyr::group_by(position) %>%
    dplyr::slice_min(rmse, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(position, k_train = k)

  hold <- pred %>% dplyr::filter(target == holdout)
  rows <- purrr::map_dfr(R58_POSITIONS, function(pos) {
    kk <- k_fit$k_train[k_fit$position == pos]
    if (length(kk) == 0) return(NULL)
    hold %>% dplyr::filter(position == pos) %>%
      dplyr::mutate(k_train = kk,
                    pred_eb_sel = .data[[paste0("pred_eb_", kk)]])
  })
  rows %>% dplyr::select(target, player_id, position, week, game_points, ytd_n,
                         k_train, pred_prior, pred_ytd, pred_linear, pred_eb_sel)
}

.r58_summarise_gate <- function(rows, label) {
  purrr::map_dfr(R58_POSITIONS, function(pos) {
    d <- rows %>% dplyr::filter(position == pos)
    if (nrow(d) < 50) return(NULL)
    b <- .r58_boot_improvement(d, "pred_eb_sel")
    tibble::tibble(
      scope       = label,
      position    = pos,
      n           = nrow(d),
      n_players   = dplyr::n_distinct(d$player_id),
      k_train     = paste(sort(unique(d$k_train)), collapse = "/"),
      rmse_prior  = sqrt(mean((d$game_points - d$pred_prior)^2)),
      rmse_linear = sqrt(mean((d$game_points - d$pred_linear)^2)),
      rmse_eb     = sqrt(mean((d$game_points - d$pred_eb_sel)^2)),
      rmse_ytd    = sqrt(mean((d$game_points - d$pred_ytd)^2)),
      improvement = b[["improvement"]],
      lo          = b[["lo"]],
      hi          = b[["hi"]],
      passed      = is.finite(b[["lo"]]) && b[["lo"]] > 0
    )
  })
}

# ------------------------------------------------------------------------------
# run_ytd_weight_backtest
#
# [2026-09-18 v2] Leave-one-season-out. Every target season is the holdout
# once, with k fitted on the other seasons; the paired errors from all
# holdouts are pooled and the gate is the cluster bootstrap on the pool.
# Per-holdout rows are reported beside it so a single-season fluke is
# visible. The first (single-holdout, 2025) run: RB/WR/TE improvement 0.03 to
# 0.06 with lower bounds 0.005 to 0.013 below zero, QB -0.065; gate failed
# 4/4. This version exists to answer whether that was power or signal.
# ------------------------------------------------------------------------------
run_ytd_weight_backtest <- function(target_seasons = R58_TARGET_SEASONS,
                                    k_grid = R58_K_GRID,
                                    scoring_settings = DEFAULT_SCORING_SETTINGS,
                                    write_outputs = TRUE) {

  target_seasons <- sort(unique(as.integer(target_seasons)))
  if (length(target_seasons) < 2) stop("R/58: need at least two target seasons.", call. = FALSE)

  # [2026-10-01] Outputs are keyed on scoring. The default (flat PPR) keeps the
  # original paths; any other scoring gets its own files, so a FanDuel refit
  # can never overwrite the production flat-PPR table.
  scoring_key  <- .r29_scoring_key(scoring_settings)
  is_default   <- .r29_is_default_scoring(scoring_settings)
  k_path       <- scoring_gate_path(R58_K_PATH, scoring_settings)
  results_path <- scoring_gate_path(R58_RESULTS_PATH, scoring_settings)
  summary_csv  <- scoring_gate_path(R58_SUMMARY_CSV, scoring_settings)

  message(glue("\n{strrep('=', 70)}"))
  message("R/58: YTD weight backtest -- linear calendar schedule vs games-based EB")
  message(glue("Leave-one-season-out over: {paste(target_seasons, collapse = ', ')}"))
  message(glue("Scoring key {scoring_key}{if (is_default) ' (default flat PPR)' else ''}; ",
               "k table -> {basename(k_path)}"))
  message(glue("{strrep('=', 70)}\n"))

  message("STEP 1/5: Building player-week frames")
  pw <- purrr::map_dfr(target_seasons, function(ts) {
    message(glue("  target {ts}..."))
    .r58_player_weeks(ts, scoring_settings)
  })
  if (nrow(pw) == 0) stop("R/58: no player-weeks built.", call. = FALSE)
  message(glue("  {nrow(pw)} player-games, {dplyr::n_distinct(pw$player_id)} players"))
  print(pw %>% dplyr::count(target, position) %>%
          tidyr::pivot_wider(names_from = position, values_from = n))

  message("\nSTEP 2/5: Scoring every rule")
  pred   <- .r58_predict(pw, k_grid)
  scores <- .r58_score(pred)

  message("\nSTEP 3/5: Leave-one-season-out folds")
  folds <- purrr::map_dfr(target_seasons, function(h) {
    tr <- setdiff(target_seasons, h)
    message(glue("  holdout {h}, train {paste(tr, collapse = '+')}"))
    .r58_gate_one_holdout(pred, scores, holdout = h, train_seasons = tr)
  })
  per_fold <- purrr::map_dfr(target_seasons, function(h) {
    .r58_summarise_gate(folds %>% dplyr::filter(target == h), label = as.character(h))
  })
  message("\n  Per-holdout:")
  print(per_fold %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))),
        n = 50)

  message("\nSTEP 4/5: Pooled gate across all holdouts")
  pooled <- .r58_summarise_gate(folds, label = "pooled_loso")
  print(pooled %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))))

  # Where the two rules diverge, pooled by week
  by_week <- folds %>%
    dplyr::group_by(position, week) %>%
    dplyr::summarise(n = dplyr::n(),
                     rmse_linear = sqrt(mean((game_points - pred_linear)^2)),
                     rmse_eb     = sqrt(mean((game_points - pred_eb_sel)^2)),
                     eb_wins     = rmse_eb < rmse_linear,
                     w_linear    = 1 - compute_prior_weight(week[1]),
                     w_eb_median = stats::median(ytd_n / (ytd_n + k_train)),
                     .groups = "drop")
  message("\n  Pooled RMSE by week, weeks 2 to 9:")
  print(by_week %>% dplyr::filter(week <= 9) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))), n = 40)
  message("\n  Weeks won by games-based rule, out of weeks 2 to 18:")
  print(by_week %>% dplyr::group_by(position) %>%
          dplyr::summarise(eb_wins = sum(eb_wins), weeks = dplyr::n(), .groups = "drop"))

  # Production k: refit on ALL seasons, for positions that passed the pooled gate
  all_eb <- scores %>%
    dplyr::filter(!is.na(k)) %>%
    dplyr::group_by(position, k) %>%
    dplyr::summarise(rmse = sqrt(sum(rmse^2 * n) / sum(n)), .groups = "drop")
  k_all <- all_eb %>%
    dplyr::group_by(position) %>%
    dplyr::slice_min(rmse, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(position, k_all = k)
  pooled <- pooled %>% dplyr::left_join(k_all, by = "position")

  message("\nSTEP 5/5: Writing gate table")
  passed <- pooled %>% dplyr::filter(passed)
  k_by_position <- stats::setNames(passed$k_all, passed$position)
  gate_passed <- length(k_by_position) > 0

  k_obj <- list(
    schema_tag      = "s3_r58_ytd_gate_v2_loso",
    built_at        = Sys.time(),
    scoring_key     = scoring_key,
    scoring_settings = scoring_settings,
    gate_passed     = gate_passed,
    k_by_position   = k_by_position,
    k_all           = stats::setNames(k_all$k_all, k_all$position),
    target_seasons  = target_seasons,
    method          = "leave-one-season-out, pooled cluster bootstrap by player",
    gate            = pooled,
    per_fold        = per_fold,
    boot_reps       = R58_BOOT_REPS,
    boot_level      = R58_BOOT_LEVEL,
    prior_proxy     = "R/49 .build_history_estimates, three prior seasons, history arm only"
  )

  if (write_outputs) {
    dir.create(R58_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    saveRDS(k_obj, k_path)
    saveRDS(list(scores = scores, folds = folds, per_fold = per_fold,
                 pooled = pooled, by_week = by_week, k_all = k_all, n_rows = nrow(pw),
                 scoring_key = scoring_key),
            results_path)
    readr::write_csv(dplyr::bind_rows(per_fold, pooled %>% dplyr::select(-k_all)),
                     summary_csv)
    message(glue("  Wrote {k_path}"))
    message(glue("  Wrote {results_path}"))
    message(glue("  Wrote {summary_csv}"))
  }

  message(glue("\n{strrep('=', 70)}"))
  message(glue("GATE (pooled LOSO): {if (gate_passed) 'PASSED' else 'FAILED'} -- ",
               "positions on games-based weight: ",
               "{if (gate_passed) paste(names(k_by_position), k_by_position, sep = '=', collapse = ', ') else 'none (R/29 stays linear)'}"))
  message(glue("{strrep('=', 70)}\n"))

  invisible(k_obj)
}
