# ==============================================================================
# 64_rb_blend_gate.R
#
# Out-of-sample gate for the R/32 RB blend and its in-season behavior.
# [2026-09-30]
#
# WHAT IS BEING TESTED
#   In season, R/32 projects an RB as
#     r32 = (1 - w) * r29_posterior + w * volume_implied
#   Production sets w = BLEND_WEIGHTS_BY_PRIOR_SOURCE[prior_source] *
#   compute_prior_weight(week). For RBs the weights (0.275 veteran, 0.325
#   phased-out) came from an IN-SAMPLE R/45 sweep that R/32 itself flags as
#   "re-derivation pending", and the calendar decay has never been scored.
#   R/31 has an in-season hook (.blend_observed_shares) that moves preseason
#   shares toward observed shares. No caller has ever passed it data.
#
#   This file fits three things on held-out seasons and gates them together:
#     w0        pooled RB blend weight, grid 0 to 1 by 0.05
#     schedule  how w0 decays in season:
#                 "calendar"  w0 * compute_prior_weight(week)   (production)
#                 "none"      w0 at every week
#                 "games"     w0 * k_w / (ytd_n + k_w)
#     k_s       observed-share update of the volume side:
#                 none, or lambda = ytd_n / (ytd_n + k_s)
#   Diagnostic only (never wired): a separate w0 per history-depth bucket.
#
# DESIGN
#   Frame: R/58's player-game frame, RB only, target seasons 2023-2025. Each row
#   is one RB game; everything on the right-hand side uses only EARLIER games
#   of that season. Scoring is R/29 DEFAULT_SCORING_SETTINGS (flat PPR).
#
#   R/29 side: R/58's history-arm prior updated with the PRODUCTION games-based
#   weight n / (n + k_RB), k_RB read from s3_r58_ytd_k.rds, capped at
#   1 - PRIOR_WEIGHT_FLOOR as in R/29.
#
#   Volume side: the real R/30 and R/31 run per historical season, the same
#   route as R/45's build_vi_arm(): leak-safe depth loader (earliest week, no
#   Sleeper overlay), season globals reset and restored on exit, R/45's
#   bounded position efficiency (seasons before the target only), R/32's sack
#   factor. Two differences from build_vi_arm(), both deliberate:
#     1. R/30 writes to a gate-scoped prefix, s3_r64_hist_<season>_, and R/31
#        reads that file. build_vi_arm() writes with the DEFAULT prefix, which
#        overwrites the production TEAM_VOLUMES_RDS that live R/31 reads. This
#        file hashes TEAM_VOLUMES_RDS before and after and stops if it changed.
#     2. Shares and team volume are kept, not just vi_mu, so the observed-share
#        arm can rebuild volume from blended shares on R/31's own basis:
#          targets  = target_share * projected_team_pass_pg
#          carries  = rush share among non-QB carries * team non-QB residual
#
#   Observed shares come from the normalized pbp with R/31's carryover
#   definitions: targets exclude two-point tries; carries exclude kneels and
#   two-point tries; the rush denominator is non-QB carries, matching how
#   .apply_team_volume() allocates RB carries. Shares are over the games the
#   player played, before the current game.
#
#   Gate: leave-one-season-out. In each fold the (w0, schedule, k_w, k_s) arm
#   with the lowest train RMSE is chosen, then scored on the holdout. Holdout
#   errors are pooled and compared to production by a player-cluster
#   bootstrap. Pass when the lower bound of the 90 percent interval on
#   RMSE(production) - RMSE(selected) is above zero.
#
# OUTPUTS (data/season3_cache)
#   s3_r64_rb_blend_gate.rds          decision, all-season refit arm, provenance
#   s3_r64_rb_blend_gate_results.rds  arm table, folds, diagnostics
#   s3_r64_rb_blend_gate_summary.csv  per-fold and pooled summary
#   s3_r64_vi_<season>.rds            per-season volume side (cache)
#   data/season2_cache/s3_r64_hist_<season>_team_volumes.rds/.csv
#                                     R/30 historical output (gate-scoped)
#
# NOTHING IS WIRED BY THIS FILE. If the gate passes, wiring R/32 and R/31 is a
# separate change.
#
# LIMITS
#   - The R/29 side is R/58's history-arm proxy, so rookies are absent from
#     the frame. The rookie and translation weights (0.50 provisional) are NOT
#     tested here and stay as they are.
#   - prior_source is not reproducible on this frame. The production baseline
#     uses the veteran_history_only weight read live from R/32; the
#     phased_out weight is reported as a sensitivity row. The per-bucket
#     diagnostic buckets by history depth (1, 2, 3 seasons), a PROXY for
#     prior_source, not prior_source itself.
#   - R/31 talent input is neutral for rookies (2026-only R/28 scores), which
#     affects teammates' shares on rookie-heavy depth charts.
#   - Volume uses the player's PRESEASON team. A midseason trade keeps the old
#     team's volume.
#   - Calendar decay is evaluated at the game week, the same convention as
#     R/58's linear rule.
#
# USAGE
#   source(here::here("R", "64_rb_blend_gate.R"))
#   res <- run_rb_blend_gate()
# ==============================================================================

library(dplyr)
library(tidyr)
library(glue)
library(purrr)
library(readr)
library(here)
library(nflreadr)

source(here::here("R", "58_ytd_weight_backtest.R"))        # R/29, R/49
source(here::here("R", "32_projection_reconciliation.R"))  # R/30, R/31, weights
source(here::here("R", "45_blend_weight_backtest.R"))      # leak-safe loader, efficiency

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------
R64_OUTPUT_DIR   <- here::here("data", "season3_cache")
R64_GATE_PATH    <- file.path(R64_OUTPUT_DIR, "s3_r64_rb_blend_gate.rds")
R64_RESULTS_PATH <- file.path(R64_OUTPUT_DIR, "s3_r64_rb_blend_gate_results.rds")
R64_SUMMARY_CSV  <- file.path(R64_OUTPUT_DIR, "s3_r64_rb_blend_gate_summary.csv")
R64_SCHEMA_TAG   <- "s3_r64_rb_blend_gate_v1"

R64_TARGET_SEASONS <- 2023:2025
R64_W0_GRID <- seq(0, 1, by = 0.05)
R64_KW_GRID <- c(1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20)
R64_KS_GRID <- 1:8
R64_MIN_BUCKET_ROWS <- 200L

R64_BOOT_REPS  <- R58_BOOT_REPS
R64_BOOT_LEVEL <- R58_BOOT_LEVEL
R64_SEED       <- R58_SEED

.r64_hist_prefix <- function(ty) paste0("s3_r64_hist_", as.integer(ty), "_")
.r64_vi_path     <- function(ty) file.path(R64_OUTPUT_DIR,
                                           glue("s3_r64_vi_{as.integer(ty)}.rds"))

# ------------------------------------------------------------------------------
# .r64_game_usage: per player-game targets / carries and team denominators
# ------------------------------------------------------------------------------
.r64_game_usage <- function(ty) {

  pbp <- load_normalized_season(ty, cache_dir = GATE_CACHE_DIR)
  need <- c("season_type", "game_id", "week", "posteam", "receiver_player_id",
            "rusher_player_id", "pass_attempt", "rush_attempt",
            "two_point_attempt", "qb_kneel")
  miss <- setdiff(need, names(pbp))
  if (length(miss) > 0L) {
    stop(glue("R/64: normalized pbp {ty} missing column(s): ",
              "{paste(miss, collapse = ', ')}"), call. = FALSE)
  }
  reg <- pbp %>% dplyr::filter(.data$season_type == "REG")
  rm(pbp)

  roster <- nflreadr::load_rosters(seasons = ty)
  qb_ids <- unique(roster$gsis_id[roster$position %in% "QB"])

  tgt <- reg %>%
    dplyr::filter(!is.na(.data$receiver_player_id),
                  .data$pass_attempt == 1L,
                  dplyr::coalesce(.data$two_point_attempt, 0L) != 1L) %>%
    dplyr::transmute(game_id, team = posteam, player_id = receiver_player_id)

  car <- reg %>%
    dplyr::filter(!is.na(.data$rusher_player_id),
                  .data$rush_attempt == 1L,
                  dplyr::coalesce(.data$qb_kneel, 0L) != 1L,
                  dplyr::coalesce(.data$two_point_attempt, 0L) != 1L) %>%
    dplyr::transmute(game_id, team = posteam, player_id = rusher_player_id)
  rm(reg); invisible(gc(verbose = FALSE))

  team_tot <- dplyr::full_join(
    tgt %>% dplyr::count(game_id, team, name = "team_tgt"),
    car %>% dplyr::filter(!.data$player_id %in% qb_ids) %>%
      dplyr::count(game_id, team, name = "team_car_nonqb"),
    by = c("game_id", "team")
  ) %>%
    dplyr::mutate(team_tgt       = dplyr::coalesce(.data$team_tgt, 0L),
                  team_car_nonqb = dplyr::coalesce(.data$team_car_nonqb, 0L))

  player <- dplyr::full_join(
    tgt %>% dplyr::count(game_id, player_id, name = "tgt"),
    car %>% dplyr::count(game_id, player_id, name = "car"),
    by = c("game_id", "player_id")
  ) %>%
    dplyr::mutate(tgt = dplyr::coalesce(.data$tgt, 0L),
                  car = dplyr::coalesce(.data$car, 0L))

  list(team = team_tot, player = player)
}

# ------------------------------------------------------------------------------
# .r64_frame: R/58 player-game frame for RBs, plus history depth, team, and
# season-to-date observed shares from EARLIER games only
# ------------------------------------------------------------------------------
.r64_frame <- function(ty, scoring_settings) {

  ty <- as.integer(ty)
  history_seasons <- (ty - 3L):(ty - 1L)

  hist_pts <- purrr::map_dfr(history_seasons, function(s) {
    .season_game_points(s, scoring_settings, cache_dir = GATE_CACHE_DIR)
  })
  target_pts <- .season_game_points(ty, scoring_settings,
                                    cache_dir = GATE_CACHE_DIR)
  if (nrow(hist_pts) == 0 || is.null(target_pts) || nrow(target_pts) == 0) {
    stop(glue("R/64: missing history or target cache for {ty}"), call. = FALSE)
  }
  hist_est <- .build_history_estimates(hist_pts, target_season = ty)

  usage <- .r64_game_usage(ty)

  games <- target_pts %>%
    dplyr::filter(.data$position == "RB", !is.na(.data$game_points)) %>%
    dplyr::inner_join(
      hist_est %>% dplyr::select(player_id, position, prior_mu = pred_mu,
                                 hist_depth = n_seasons),
      by = c("player_id", "position")
    ) %>%
    dplyr::left_join(usage$player, by = c("game_id", "player_id")) %>%
    dplyr::left_join(usage$team,   by = c("game_id", "team")) %>%
    dplyr::mutate(tgt = dplyr::coalesce(.data$tgt, 0L),
                  car = dplyr::coalesce(.data$car, 0L))

  n_team_miss <- sum(is.na(games$team_tgt))
  if (n_team_miss > 0.02 * nrow(games)) {
    stop(glue("R/64: {n_team_miss} of {nrow(games)} RB games in {ty} failed ",
              "the game_id + team join to pbp team totals."), call. = FALSE)
  }

  # Cumulative sums minus the current game: every share uses earlier games only.
  cum <- games %>%
    dplyr::arrange(player_id, week, game_id) %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(
      ytd_n    = dplyr::row_number() - 1L,
      ytd_sum  = cumsum(game_points) - game_points,
      ytd_mean = dplyr::if_else(ytd_n > 0L, ytd_sum / ytd_n, NA_real_),
      prev_tgt      = cumsum(tgt) - tgt,
      prev_team_tgt = cumsum(dplyr::coalesce(team_tgt, 0L)) -
                        dplyr::coalesce(team_tgt, 0L),
      prev_car      = cumsum(car) - car,
      prev_team_car = cumsum(dplyr::coalesce(team_car_nonqb, 0L)) -
                        dplyr::coalesce(team_car_nonqb, 0L)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      obs_tgt_share = dplyr::if_else(prev_team_tgt > 0,
                                     prev_tgt / prev_team_tgt, NA_real_),
      obs_car_share = dplyr::if_else(prev_team_car > 0,
                                     prev_car / prev_team_car, NA_real_)
    )

  # LEAK CHECK: recompute a sample of observed shares directly from the raw
  # game rows with week strictly below the row's week. Any mismatch stops.
  set.seed(R64_SEED)
  chk <- cum %>% dplyr::filter(ytd_n >= 1L)
  chk <- chk[sample(seq_len(nrow(chk)), min(300L, nrow(chk))), ]
  fails <- 0L
  for (i in seq_len(nrow(chk))) {
    r <- chk[i, ]
    e <- games[games$player_id == r$player_id & games$week < r$week, ]
    den_t <- sum(dplyr::coalesce(e$team_tgt, 0L))
    den_c <- sum(dplyr::coalesce(e$team_car_nonqb, 0L))
    exp_t <- if (den_t > 0) sum(e$tgt) / den_t else NA_real_
    exp_c <- if (den_c > 0) sum(e$car) / den_c else NA_real_
    ok_t <- (is.na(exp_t) && is.na(r$obs_tgt_share)) ||
      (!is.na(exp_t) && !is.na(r$obs_tgt_share) && abs(exp_t - r$obs_tgt_share) < 1e-9)
    ok_c <- (is.na(exp_c) && is.na(r$obs_car_share)) ||
      (!is.na(exp_c) && !is.na(r$obs_car_share) && abs(exp_c - r$obs_car_share) < 1e-9)
    ok_n <- nrow(e) == r$ytd_n
    if (!(ok_t && ok_c && ok_n)) fails <- fails + 1L
  }
  message(glue("  leak check {ty}: {nrow(chk)} rows recomputed from earlier ",
               "games, {fails} failure(s)"))
  if (fails > 0L) stop(glue("R/64: observed-share leak check failed for {ty}."),
                       call. = FALSE)

  cum %>%
    dplyr::filter(ytd_n >= 1L, week >= 2L,
                  week <= NFL_REGULAR_SEASON_MAX_WEEK) %>%
    dplyr::mutate(target = ty) %>%
    dplyr::select(target, player_id, position, team, week, game_id,
                  game_points, prior_mu, hist_depth, ytd_n, ytd_mean,
                  obs_tgt_share, obs_car_share, tgt, car)
}

# ------------------------------------------------------------------------------
# .r64_vi_season: real R/30 + R/31 for one historical season, gate-scoped
# ------------------------------------------------------------------------------
.r64_vi_season <- function(ty, scoring_settings, refresh = FALSE) {

  ty <- as.integer(ty)
  vi_path <- .r64_vi_path(ty)
  if (!refresh && file.exists(vi_path)) {
    message(glue("  vi {ty}: cached ({basename(vi_path)})"))
    return(readRDS(vi_path))
  }

  hist_seasons <- .r30_historical_seasons(ty)
  if (max(hist_seasons) >= ty) {
    stop(glue("R/64: R/30 historical window for {ty} reaches {max(hist_seasons)}."),
         call. = FALSE)
  }

  prefix  <- .r64_hist_prefix(ty)
  tv_path <- .r30_output_rds_path(CACHE_DIR_DEFAULT, prefix)
  if (normalizePath(tv_path, mustWork = FALSE) ==
      normalizePath(TEAM_VOLUMES_RDS, mustWork = FALSE)) {
    stop("R/64: gate-scoped R/30 path resolves to the production file.",
         call. = FALSE)
  }

  # Save and restore every global build_vi_arm() touches.
  g_names <- c(".load_2026_depth_charts", "SEASON", "HISTORICAL_SEASONS",
               "SEASON_ALLOC", "CARRYOVER_PRIOR_SEASON")
  saved <- lapply(g_names, function(nm) {
    if (exists(nm, envir = .GlobalEnv)) get(nm, envir = .GlobalEnv) else NULL
  })
  names(saved) <- g_names
  on.exit({
    for (nm in g_names) {
      if (!is.null(saved[[nm]])) assign(nm, saved[[nm]], envir = .GlobalEnv)
    }
  }, add = TRUE)

  # R/31 now calls .load_2026_depth_charts(season =, use_sleeper_overrides =).
  # R/45's leak-safe loader predates that argument, so it is wrapped here with
  # the argument accepted and ignored (the leak-safe loader never overlays
  # Sleeper). R/45's own build_vi_arm() hits the same unused-argument error.
  leaksafe_loader <- function(season = SEASON_ALLOC, use_sleeper_overrides = FALSE) {
    .harness_leaksafe_depth_loader(season = season)
  }
  assign(".load_2026_depth_charts", leaksafe_loader, envir = .GlobalEnv)

  # QB committees are disabled for historical seasons. The committee loaders in
  # R/30 and R/31 filter with `.season == as.integer(season)` inside a data
  # mask, where `season` resolves to the CSV's own column, not the function
  # argument, so the filter is always TRUE and the 2026 LV/ATL committees were
  # applied to 2023. Pointing both at a path that does not exist returns the
  # loaders' empty table.
  no_committees <- file.path(tempdir(), "r64_no_qb_committees.csv")
  if (file.exists(no_committees)) unlink(no_committees)
  assign("SEASON", ty, envir = .GlobalEnv)
  assign("HISTORICAL_SEASONS", hist_seasons, envir = .GlobalEnv)
  assign("SEASON_ALLOC", ty, envir = .GlobalEnv)
  assign("CARRYOVER_PRIOR_SEASON", ty - 1L, envir = .GlobalEnv)

  message(glue("  vi {ty}: R/30 -> {basename(tv_path)}"))
  project_team_volumes(season = ty, out_prefix = prefix,
                       qb_committees_path = no_committees,
                       as_of_week = NULL, save_output = TRUE)
  if (!file.exists(tv_path)) {
    stop(glue("R/64: R/30 did not write {tv_path}"), call. = FALSE)
  }

  message(glue("  vi {ty}: R/31 (leak-safe depth, no Sleeper overlay)"))
  alloc <- allocate_player_volumes(team_volumes_path = tv_path,
                                   season = ty,
                                   use_sleeper_overrides = FALSE,
                                   qb_committees_path = no_committees,
                                   as_of_week = NULL,
                                   current_season_volume = NULL,
                                   save_output = FALSE)

  eff_seasons <- (ty - VI_EFF_WINDOW):(ty - 1L)
  eff <- .harness_position_efficiency(eff_seasons, scoring_settings) %>%
    dplyr::filter(.data$position == "RB")
  if (nrow(eff) != 1L) stop("R/64: RB efficiency row missing.", call. = FALSE)

  # Team non-QB carry residual: R/31 allocates exactly this to non-QB players.
  resid <- alloc %>%
    dplyr::filter(.data$position != "QB") %>%
    dplyr::group_by(.data$team) %>%
    dplyr::summarise(team_car_resid = sum(dplyr::coalesce(expected_carries_pg, 0)),
                     .groups = "drop")

  out <- alloc %>%
    dplyr::filter(.data$position == "RB") %>%
    dplyr::left_join(resid, by = "team") %>%
    dplyr::transmute(
      player_id      = .data$nfl_gsis_id,
      target         = ty,
      vi_team        = .data$team,
      s_tgt_ps       = dplyr::coalesce(.data$target_share, 0),
      team_pass_pg   = dplyr::coalesce(.data$projected_team_pass_pg, 0),
      et_ps          = dplyr::coalesce(.data$expected_targets_pg, 0),
      ec_ps          = dplyr::coalesce(.data$expected_carries_pg, 0),
      team_car_resid = dplyr::coalesce(.data$team_car_resid, 0),
      s_car_ps       = dplyr::if_else(.data$team_car_resid > 0,
                                      .data$ec_ps / .data$team_car_resid, 0),
      ppr_per_target = eff$ppr_per_target,
      ppr_per_carry  = eff$ppr_per_carry
    ) %>%
    dplyr::filter(.data$et_ps <= VI_MAX_TARGETS_PG,
                  .data$ec_ps <= VI_MAX_CARRIES_PG) %>%
    dplyr::mutate(
      vi_ps = .data$et_ps * VI_SACK_FACTOR * .data$ppr_per_target +
              .data$ec_ps * .data$ppr_per_carry
    ) %>%
    dplyr::distinct(player_id, .keep_all = TRUE)

  saveRDS(out, vi_path)
  message(glue("  vi {ty}: {nrow(out)} RBs, saved {basename(vi_path)}"))
  out
}

# ------------------------------------------------------------------------------
# .r64_components: per-row pieces every arm is built from
# ------------------------------------------------------------------------------
.r64_components <- function(df, k_rb) {
  cap <- 1 - PRIOR_WEIGHT_FLOOR
  w_r29 <- pmin(df$ytd_n / (df$ytd_n + k_rb), cap)
  df %>%
    dplyr::mutate(
      r29      = prior_mu + w_r29 * (ytd_mean - prior_mu),
      g_cal    = vapply(week, compute_prior_weight, numeric(1)),
      resid0   = game_points - r29
    )
}

# Volume side under share-update k_s (0 = no update, preseason volume).
.r64_vi <- function(df, k_s) {
  if (k_s == 0) return(df$vi_ps)
  lam <- df$ytd_n / (df$ytd_n + k_s)
  s_t <- dplyr::if_else(is.na(df$obs_tgt_share), df$s_tgt_ps,
                        (1 - lam) * df$s_tgt_ps + lam * df$obs_tgt_share)
  s_c <- dplyr::if_else(is.na(df$obs_car_share), df$s_car_ps,
                        (1 - lam) * df$s_car_ps + lam * df$obs_car_share)
  (s_t * df$team_pass_pg) * VI_SACK_FACTOR * df$ppr_per_target +
    (s_c * df$team_car_resid) * df$ppr_per_carry
}

# Schedule multiplier g (w = w0 * g).
.r64_g <- function(df, schedule, k_w) {
  switch(schedule,
         calendar = df$g_cal,
         none     = rep(1, nrow(df)),
         games    = k_w / (df$ytd_n + k_w),
         stop("unknown schedule"))
}

.r64_schedules <- function() {
  dplyr::bind_rows(
    tibble::tibble(schedule = "calendar", k_w = NA_real_),
    tibble::tibble(schedule = "none",     k_w = NA_real_),
    tibble::tibble(schedule = "games",    k_w = R64_KW_GRID)
  )
}

# ------------------------------------------------------------------------------
# .r64_arm_table: SSE per season for every (k_s, schedule, k_w, w0)
# SSE(w0) = A - 2 w0 B + w0^2 C, with d = vi - r29 and u = g * d
# ------------------------------------------------------------------------------
.r64_arm_table <- function(df, bucket_col = NULL) {
  sched <- .r64_schedules()
  grp <- if (is.null(bucket_col)) df$target else
    paste(df$target, df[[bucket_col]], sep = "|")
  purrr::map_dfr(c(0, R64_KS_GRID), function(ks) {
    d <- .r64_vi(df, ks) - df$r29
    purrr::map_dfr(seq_len(nrow(sched)), function(j) {
      u <- .r64_g(df, sched$schedule[j], sched$k_w[j]) * d
      agg <- tibble::tibble(grp = grp, r = df$resid0, u = u) %>%
        dplyr::group_by(grp) %>%
        dplyr::summarise(n = dplyr::n(), A = sum(r^2), B = sum(r * u),
                         C = sum(u^2), .groups = "drop")
      tidyr::crossing(agg, w0 = R64_W0_GRID) %>%
        dplyr::mutate(sse = A - 2 * w0 * B + w0^2 * C,
                      k_s = ks, schedule = sched$schedule[j],
                      k_w = sched$k_w[j]) %>%
        dplyr::select(grp, k_s, schedule, k_w, w0, n, sse)
    })
  }) %>%
    tidyr::separate(grp, into = c("target", "bucket"), sep = "\\|",
                    fill = "right", remove = TRUE) %>%
    dplyr::mutate(target = as.integer(target))
}

.r64_pred <- function(df, k_s, schedule, k_w, w0) {
  df$r29 + w0 * .r64_g(df, schedule, k_w) * (.r64_vi(df, k_s) - df$r29)
}

# ------------------------------------------------------------------------------
# .r64_boot: player-cluster bootstrap of RMSE(base) - RMSE(cand)
# ------------------------------------------------------------------------------
.r64_boot <- function(y, base, cand, player, reps = R64_BOOT_REPS,
                      level = R64_BOOT_LEVEL, seed = R64_SEED) {
  set.seed(seed)
  by_p <- split(seq_along(y), player)
  pl <- names(by_p)
  stat <- function(i) sqrt(mean((y[i] - base[i])^2)) - sqrt(mean((y[i] - cand[i])^2))
  point <- stat(seq_along(y))
  draws <- vapply(seq_len(reps), function(r) {
    stat(unlist(by_p[sample(pl, length(pl), replace = TRUE)], use.names = FALSE))
  }, numeric(1))
  a <- (1 - level) / 2
  c(improvement = point,
    lo = unname(stats::quantile(draws, a)),
    hi = unname(stats::quantile(draws, 1 - a)))
}

.r64_pick <- function(arms, train_seasons) {
  arms %>%
    dplyr::filter(target %in% train_seasons) %>%
    dplyr::group_by(k_s, schedule, k_w, w0) %>%
    dplyr::summarise(rmse = sqrt(sum(sse) / sum(n)), .groups = "drop") %>%
    dplyr::slice_min(rmse, n = 1, with_ties = FALSE)
}

# ------------------------------------------------------------------------------
# run_rb_blend_gate
# ------------------------------------------------------------------------------
run_rb_blend_gate <- function(target_seasons = R64_TARGET_SEASONS,
                              scoring_settings = DEFAULT_SCORING_SETTINGS,
                              refresh_volume = FALSE,
                              write_outputs = TRUE) {

  target_seasons <- sort(unique(as.integer(target_seasons)))
  if (length(target_seasons) < 2) stop("R/64: need two or more seasons.", call. = FALSE)

  message(glue("\n{strrep('=', 70)}"))
  message("R/64: RB blend gate -- weight, in-season schedule, observed-share update")
  message(glue("Leave-one-season-out over: {paste(target_seasons, collapse = ', ')}"))
  message(glue("{strrep('=', 70)}\n"))

  # ---- production parameters, read live, never copied ------------------------
  k_tab <- load_ytd_k_table()
  if (is.null(k_tab) || !"RB" %in% names(k_tab)) {
    stop("R/64: s3_r58_ytd_k.rds has no passing RB k. Run R/58 first.", call. = FALSE)
  }
  k_rb <- unname(k_tab[["RB"]])
  w_prod_vet <- unname(BLEND_WEIGHTS_BY_PRIOR_SOURCE[["veteran_history_only"]])
  w_prod_po  <- unname(BLEND_WEIGHTS_BY_PRIOR_SOURCE[["phased_out_history_only"]])
  message(glue("  Production: R/29 RB k = {k_rb}; R/32 RB w0 veteran {w_prod_vet}, ",
               "phased-out {w_prod_po}; decay compute_prior_weight(week)"))

  # ---- guard the production R/30 cache ---------------------------------------
  prod_hash_before <- if (file.exists(TEAM_VOLUMES_RDS))
    unname(tools::md5sum(TEAM_VOLUMES_RDS)) else NA_character_

  message("\nSTEP 1/6: Player-game frames (RB)")
  frame <- purrr::map_dfr(target_seasons, function(ty) {
    message(glue("  target {ty}..."))
    .r64_frame(ty, scoring_settings)
  })
  message(glue("  {nrow(frame)} RB player-games, ",
               "{dplyr::n_distinct(frame$player_id)} players"))

  r58_res_path <- R58_RESULTS_PATH
  if (file.exists(r58_res_path)) {
    r58 <- readRDS(r58_res_path)
    n58 <- sum(r58$folds$position == "RB" & r58$folds$target %in% target_seasons)
    message(glue("  R/58 RB rows for the same seasons: {n58} ",
                 "({if (n58 == nrow(frame)) 'match' else 'MISMATCH'})"))
  }

  message("\nSTEP 2/6: Volume side (real R/30 + R/31 per season, gate-scoped)")
  vi <- purrr::map_dfr(target_seasons, function(ty) {
    .r64_vi_season(ty, scoring_settings, refresh = refresh_volume)
  })

  prod_hash_after <- if (file.exists(TEAM_VOLUMES_RDS))
    unname(tools::md5sum(TEAM_VOLUMES_RDS)) else NA_character_
  if (!identical(prod_hash_before, prod_hash_after)) {
    stop("R/64: production TEAM_VOLUMES_RDS changed during the run. ",
         "Rerun production R/30 before using live R/31.", call. = FALSE)
  }
  message("  Production R/30 cache unchanged (md5 verified)")

  df <- frame %>%
    dplyr::inner_join(vi, by = c("player_id", "target"))
  cov <- df %>% dplyr::count(target, name = "with_vi") %>%
    dplyr::left_join(frame %>% dplyr::count(target, name = "frame"), by = "target") %>%
    dplyr::mutate(coverage = round(with_vi / frame, 3))
  message("  Coverage (rows with a preseason R/31 allocation):")
  print(cov)
  if (nrow(df) == 0) stop("R/64: no rows with a volume side.", call. = FALSE)

  message("\nSTEP 3/6: Components (leak check ran per season in STEP 1)")
  df <- .r64_components(df, k_rb)
  df <- df %>%
    dplyr::mutate(pred_prod    = r29 + w_prod_vet * g_cal * (vi_ps - r29),
                  pred_prod_po = r29 + w_prod_po  * g_cal * (vi_ps - r29))

  message("\nSTEP 4/6: Arm table and leave-one-season-out selection")
  arms <- .r64_arm_table(df)
  message(glue("  {dplyr::n_distinct(paste(arms$k_s, arms$schedule, arms$k_w, arms$w0))} arms"))

  folds <- purrr::map(target_seasons, function(h) {
    tr <- setdiff(target_seasons, h)
    pick <- .r64_pick(arms, tr)
    hold <- df %>% dplyr::filter(target == h)
    hold$pred_sel <- .r64_pred(hold, pick$k_s, pick$schedule, pick$k_w, pick$w0)
    message(glue("  holdout {h}: w0 {pick$w0}, schedule {pick$schedule}",
                 "{if (pick$schedule == 'games') paste0(' k_w ', pick$k_w) else ''}, ",
                 "k_s {pick$k_s}"))
    list(pick = pick %>% dplyr::mutate(holdout = h), rows = hold)
  })
  fold_picks <- purrr::map_dfr(folds, "pick")
  hold_rows  <- purrr::map_dfr(folds, "rows")

  summ <- function(d, label) {
    b <- .r64_boot(d$game_points, d$pred_prod, d$pred_sel, d$player_id)
    tibble::tibble(
      scope       = label,
      n           = nrow(d),
      n_players   = dplyr::n_distinct(d$player_id),
      rmse_r29    = sqrt(mean((d$game_points - d$r29)^2)),
      rmse_prod   = sqrt(mean((d$game_points - d$pred_prod)^2)),
      rmse_prod_po = sqrt(mean((d$game_points - d$pred_prod_po)^2)),
      rmse_sel    = sqrt(mean((d$game_points - d$pred_sel)^2)),
      improvement = b[["improvement"]],
      lo          = b[["lo"]],
      hi          = b[["hi"]],
      passed      = is.finite(b[["lo"]]) && b[["lo"]] > 0
    )
  }
  per_fold <- purrr::map_dfr(target_seasons, function(h) {
    summ(hold_rows %>% dplyr::filter(target == h), as.character(h))
  })
  pooled <- summ(hold_rows, "pooled_loso")

  message("\n  Per-holdout:")
  print(per_fold %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))))
  message("\n  Pooled:")
  print(pooled %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))))

  by_week <- hold_rows %>%
    dplyr::group_by(week) %>%
    dplyr::summarise(n = dplyr::n(),
                     rmse_prod = sqrt(mean((game_points - pred_prod)^2)),
                     rmse_sel  = sqrt(mean((game_points - pred_sel)^2)),
                     sel_wins  = rmse_sel < rmse_prod, .groups = "drop")
  message(glue("\n  Weeks won by the selected arm: {sum(by_week$sel_wins)} of {nrow(by_week)}"))

  # ---- STEP 5: diagnostic C, per history-depth bucket (never wired) ---------
  message("\nSTEP 5/6: Diagnostic -- separate w0 per history-depth bucket")
  df$bucket <- as.character(df$hist_depth)
  arms_b <- .r64_arm_table(df, bucket_col = "bucket")
  diag_rows <- purrr::map_dfr(target_seasons, function(h) {
    tr <- setdiff(target_seasons, h)
    pk <- fold_picks %>% dplyr::filter(holdout == h)
    per_b <- arms_b %>%
      dplyr::filter(target %in% tr, k_s == pk$k_s, schedule == pk$schedule,
                    (is.na(k_w) & is.na(pk$k_w)) | k_w %in% pk$k_w) %>%
      dplyr::group_by(bucket, w0) %>%
      dplyr::summarise(n = sum(n), rmse = sqrt(sum(sse) / sum(n)), .groups = "drop") %>%
      dplyr::group_by(bucket) %>%
      dplyr::slice_min(rmse, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
    hold <- hold_rows %>% dplyr::filter(target == h) %>%
      dplyr::mutate(bucket = as.character(hist_depth)) %>%
      dplyr::left_join(per_b %>% dplyr::select(bucket, w0_b = w0, n_train = n),
                       by = "bucket") %>%
      dplyr::mutate(w0_b = dplyr::if_else(is.na(n_train) | n_train < R64_MIN_BUCKET_ROWS,
                                          pk$w0, w0_b))
    hold$pred_bucket <- hold$r29 + hold$w0_b *
      .r64_g(hold, pk$schedule, pk$k_w) * (.r64_vi(hold, pk$k_s) - hold$r29)
    hold
  })
  diag_b <- .r64_boot(diag_rows$game_points, diag_rows$pred_sel,
                      diag_rows$pred_bucket, diag_rows$player_id)
  diag_c <- list(
    note = paste("history depth (seasons with games in the prior window) is a",
                 "proxy for prior_source; reported only, never wired"),
    rows_by_bucket = diag_rows %>% dplyr::count(bucket),
    rmse_pooled_w0 = sqrt(mean((diag_rows$game_points - diag_rows$pred_sel)^2)),
    rmse_bucket_w0 = sqrt(mean((diag_rows$game_points - diag_rows$pred_bucket)^2)),
    improvement_bucket_over_pooled = diag_b
  )
  message(glue("  Bucketed vs pooled w0: improvement ",
               "{round(diag_b[['improvement']], 3)} ",
               "[{round(diag_b[['lo']], 3)}, {round(diag_b[['hi']], 3)}] ",
               "(diagnostic only)"))

  # ---- STEP 6: all-season refit and write ------------------------------------
  message("\nSTEP 6/6: All-season refit and outputs")
  refit <- .r64_pick(arms, target_seasons)
  gate_passed <- isTRUE(pooled$passed)
  message(glue("  All-season arm: w0 {refit$w0}, schedule {refit$schedule}",
               "{if (refit$schedule == 'games') paste0(' k_w ', refit$k_w) else ''}, ",
               "k_s {refit$k_s} (train RMSE {round(refit$rmse, 3)})"))

  gate_obj <- list(
    schema_tag     = R64_SCHEMA_TAG,
    built_at       = Sys.time(),
    gate_passed    = gate_passed,
    position       = "RB",
    arm_all_seasons = refit,
    fold_picks     = fold_picks,
    gate           = pooled,
    per_fold       = per_fold,
    production     = list(k_rb = k_rb, w0_veteran = w_prod_vet,
                          w0_phased_out = w_prod_po,
                          decay = "compute_prior_weight(week)"),
    target_seasons = target_seasons,
    provenance     = list(
      frame        = "R/58 player-game frame, RB, flat PPR (DEFAULT_SCORING_SETTINGS)",
      volume       = "real R/30 + R/31 per season, leak-safe depth, gate-scoped prefix",
      efficiency   = glue("R/45 .harness_position_efficiency, {VI_EFF_WINDOW} prior seasons"),
      sack_factor  = VI_SACK_FACTOR,
      shares       = "pbp, R/31 carryover definitions, non-QB carry denominator",
      boot         = list(reps = R64_BOOT_REPS, level = R64_BOOT_LEVEL, seed = R64_SEED)
    ),
    coverage       = cov,
    limits         = c(
      "rookies absent from frame; rookie/translation weights untested",
      "prior_source not reproducible; baseline uses veteran weight",
      "R/31 rookie talent neutral historically",
      "volume uses preseason team"
    )
  )

  if (write_outputs) {
    dir.create(R64_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    saveRDS(gate_obj, R64_GATE_PATH)
    saveRDS(list(arms = arms, hold_rows = hold_rows, by_week = by_week,
                 diag_c = diag_c, coverage = cov, n_rows = nrow(df)),
            R64_RESULTS_PATH)
    readr::write_csv(dplyr::bind_rows(per_fold, pooled), R64_SUMMARY_CSV)
    message(glue("  Wrote {R64_GATE_PATH}"))
    message(glue("  Wrote {R64_RESULTS_PATH}"))
    message(glue("  Wrote {R64_SUMMARY_CSV}"))
  }

  message(glue("\n{strrep('=', 70)}"))
  message(glue("GATE (pooled LOSO, RB): {if (gate_passed) 'PASSED' else 'FAILED'} -- ",
               "improvement {round(pooled$improvement, 3)} ",
               "[{round(pooled$lo, 3)}, {round(pooled$hi, 3)}]"))
  message("Nothing is wired by this file.")
  message(glue("{strrep('=', 70)}\n"))

  invisible(gate_obj)
}
