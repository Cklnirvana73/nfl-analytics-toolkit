# ==============================================================================
# 68_offense_vegas_gate.R
#
# Out-of-sample gate: does the team's Vegas implied total improve the
# per-game projection for QB / RB / WR / TE?  [2026-10-22]
#
# WHY
#   Offense has no week-specific term in the chain. R/35's opponent matchup
#   factor was neutralized on gate evidence (DVP_CAP_OFFENSE = 0.00), and the
#   only market input is R/34's DEF shift (R/66 passed). The team's own implied
#   total is the market's week-specific view of how much that offense scores.
#
# WHAT IS BEING TESTED (per position, both gated)
#   Baseline  The production update rule as R/63 gates it: history-arm prior
#             (R/58 / R/49 proxy) blended with the season-to-date mean over
#             earlier games; games-based weight ytd_n / (ytd_n + k) at
#             RB/WR/TE with k refit on each fold's training seasons, linear
#             calendar schedule at QB. k, center and b are fit on training
#             rows only. Caveat (same as R/58 / R/63): a training row's prior
#             is built from the three seasons before it, so when 2023 is held
#             out, 2024 and 2025 training priors include 2023 games. No
#             held-out row's own outcome is ever used.
#   C1 additive      pred = base + b * (team_implied - center)
#   C2 proportional  pred = base * (1 + b * (team_implied - center) / center)
#                    (a bench WR and a WR1 should not move by the same points)
#   team_implied: home = (total_line + spread_line) / 2,
#                 away = (total_line - spread_line) / 2, spread_line > 0 means
#                 the home team is favored (sign checked against realized
#                 margins before any scoring; the run stops if it reads wrong).
#   center = mean team_implied over the training rows; b by no-intercept OLS
#   on the baseline's training-fold residuals. A missing line contributes 0;
#   no row is ever dropped.
#
# GATE (same standard as R/58, R/63, R/66)
#   Leave-one-season-out over 2023-2025, pooled paired errors, cluster
#   bootstrap (R58_BOOT_REPS, R58_BOOT_LEVEL, R58_SEED). Clusters are
#   team-seasons, not players: the feature varies at the team-game level, so
#   team clustering is the conservative choice. A candidate passes at a
#   position when the lower bound on RMSE(base) - RMSE(cand) is above zero.
#   WIRING RULE: if both pass at a position, the one with the larger pooled
#   improvement is wired; if neither passes, the position stays off (b = 0).
#   Two candidates per position is a multiple-comparison cost; it is reported,
#   not corrected, and the per-holdout table shows whether a pass holds in
#   each season.
#
# OUTPUTS (data/season3_cache/, schema s3_r68_offense_vegas_v1)
#   s3_r68_offense_vegas.rds          gate object read by R/59 (decisions with
#                                     mode / b / center per position)
#   s3_r68_offense_vegas_results.rds  paired predictions, per-fold fits
#   s3_r68_offense_vegas_summary.csv  per-fold and pooled rows
#   Default names are flat PPR. Any other scoring gets _<scoring key> before
#   the extension (R/29 scoring_gate_path), and the object records
#   scoring_key, which R/59 checks.
#
# LIVE HELPER
#   team_implied_totals(season, week): the same construction for one week
#   (console checks). R/59 builds the same quantity itself in
#   .r59_week_teams from the schedule it already loads.
#
# LIMITS (stated so they are not forgotten)
#   - Lines are nflverse schedule lines (closing). Production pulls them when
#     the chain runs (often Thursday), so live lines are noisier than tested.
#   - The baseline is the R/58 proxy prior, not R/32's reconciled mean. A pass
#     says the market adds information to a per-game season mean; R/59 applies
#     it to R/32's mean on that basis.
#   - Production WR on flat PPR uses the R/63 xFP blend; the baseline here does
#     not, so any WR overlap between xFP and the market is not measured.
#
# USAGE
#   source(here::here("R", "68_offense_vegas_gate.R"))
#   g    <- run_offense_vegas_gate()                                  # flat PPR
#   g_fd <- run_offense_vegas_gate(scoring_settings = FANDUEL_SCORING)
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(glue)
library(readr)
library(nflreadr)
library(here)
if (!exists("R58_SEED")) source(here::here("R", "58_ytd_weight_backtest.R"))   # sources R/29 and R/49

R68_OUTPUT_DIR   <- here::here("data", "season3_cache")
R68_GATE_PATH    <- file.path(R68_OUTPUT_DIR, "s3_r68_offense_vegas.rds")
R68_RESULTS_PATH <- file.path(R68_OUTPUT_DIR, "s3_r68_offense_vegas_results.rds")
R68_SUMMARY_CSV  <- file.path(R68_OUTPUT_DIR, "s3_r68_offense_vegas_summary.csv")
R68_SCHEMA_TAG   <- "s3_r68_offense_vegas_v1"
R68_TARGET_SEASONS <- 2023:2025
R68_MAX_NO_LINE_SHARE <- 0.05   # more rows than this without a line = join bug, stop
R68_LEAK_ROWS    <- 60L

R68_TESTS <- list(
  list(test = "c1_additive",     cand = "pred_add"),
  list(test = "c2_proportional", cand = "pred_pro")
)
R68_REVIVE <- paste("Revive when a re-run of R/68 with an added season passes the",
                    "pooled LOSO gate at this position.")

.r68_rmse <- function(y, p) sqrt(mean((y - p)^2))
.r68_abbr <- function(x) nflreadr::clean_team_abbrs(as.character(x), keep_non_matches = TRUE)

.r68_ols1 <- function(r, x) {
  if (sum(x^2) <= 0) return(0)
  unname(sum(x * r) / sum(x^2))
}

# ------------------------------------------------------------------------------
# Lines: one row per (game_id, team) with team_implied
# ------------------------------------------------------------------------------
.r68_lines_from_schedule <- function(sch) {
  dplyr::bind_rows(
    sch %>% dplyr::transmute(game_id, season, week, team = .r68_abbr(home_team),
                             team_implied = (total_line + spread_line) / 2),
    sch %>% dplyr::transmute(game_id, season, week, team = .r68_abbr(away_team),
                             team_implied = (total_line - spread_line) / 2)
  )
}

.r68_lines <- function(seasons) {
  sch <- nflreadr::load_schedules(seasons)
  need <- c("game_id", "season", "week", "game_type", "home_team", "away_team",
            "spread_line", "total_line", "home_score", "away_score")
  miss <- setdiff(need, names(sch))
  if (length(miss) > 0) stop("R/68: load_schedules missing: ", paste(miss, collapse = ", "),
                             call. = FALSE)
  sch <- sch %>% dplyr::filter(game_type == "REG")
  rr <- stats::cor(sch$home_score - sch$away_score, sch$spread_line, use = "complete.obs")
  message(glue("  spread_line vs realized home margin: r = {round(rr, 3)}"))
  if (!is.finite(rr) || rr <= 0) {
    stop("R/68: spread_line does not read as home-favored-positive. Not scoring.", call. = FALSE)
  }
  message(glue("  Lines: {nrow(sch)} REG games, total NA {sum(is.na(sch$total_line))}, ",
               "spread NA {sum(is.na(sch$spread_line))}"))
  .r68_lines_from_schedule(sch) %>% dplyr::select(game_id, team, team_implied)
}

#' Team implied totals for one week (live; same construction as the gate)
#' @return Tibble team, team_implied (NA where no line is posted).
#' @export
team_implied_totals <- function(season, week) {
  sch <- tryCatch(nflreadr::load_schedules(season), error = function(e) NULL)
  if (is.null(sch)) return(tibble::tibble(team = character(), team_implied = numeric()))
  sch <- sch %>% dplyr::filter(game_type == "REG", .data$week == !!as.integer(week))
  .r68_lines_from_schedule(sch) %>% dplyr::select(team, team_implied)
}

# ------------------------------------------------------------------------------
# Frame for one target season (R/58 construction, plus team and line)
# ------------------------------------------------------------------------------
.r68_frame <- function(ts, scoring_settings, lines) {
  hist_pts <- purrr::map_dfr((ts - 3L):(ts - 1L), function(s) {
    .season_game_points(s, scoring_settings, cache_dir = GATE_CACHE_DIR)
  })
  tgt <- .season_game_points(ts, scoring_settings, cache_dir = GATE_CACHE_DIR)
  if (nrow(hist_pts) == 0 || is.null(tgt) || nrow(tgt) == 0) {
    stop("R/68: missing history or target game points for ", ts, call. = FALSE)
  }
  if (!"team" %in% names(tgt)) stop("R/68: game points carry no team column.", call. = FALSE)
  hist_est <- .build_history_estimates(hist_pts, target_season = ts)

  full <- tgt %>%
    dplyr::mutate(week = as.integer(week)) %>%
    dplyr::filter(position %in% R58_POSITIONS, !is.na(game_points)) %>%
    dplyr::inner_join(hist_est %>% dplyr::select(player_id, position, prior_mu = pred_mu),
                      by = c("player_id", "position")) %>%
    dplyr::arrange(player_id, week, game_id) %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(ytd_n    = dplyr::row_number() - 1L,
                  ytd_sum  = cumsum(game_points) - game_points,
                  ytd_mean = dplyr::if_else(ytd_n > 0L, ytd_sum / ytd_n, NA_real_)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(target = ts, team = .r68_abbr(team))

  frame <- full %>%
    dplyr::filter(ytd_n >= 1L, week >= 2L, week <= NFL_REGULAR_SEASON_MAX_WEEK) %>%
    dplyr::left_join(lines, by = c("game_id", "team"), relationship = "many-to-one") %>%
    dplyr::mutate(w_lin = vapply(week, function(w) 1 - compute_prior_weight(w), numeric(1)),
                  cluster = paste(target, team))

  no_line <- mean(is.na(frame$team_implied))
  message(glue("    target {ts}: {nrow(frame)} rows; no line {sum(is.na(frame$team_implied))} ",
               "({round(100 * no_line, 2)}%)"))
  if (no_line > R68_MAX_NO_LINE_SHARE) {
    print(frame %>% dplyr::filter(is.na(team_implied)) %>% dplyr::count(team, sort = TRUE) %>%
            dplyr::slice_head(n = 10))
    stop(glue("R/68: {round(100 * no_line, 1)}% of {ts} rows have no line; that is a ",
              "game_id/team join problem, not missing markets. Not scoring."), call. = FALSE)
  }
  list(frame = frame, full = full %>% dplyr::select(target, player_id, week, game_points))
}

.r68_leak_check <- function(frame, fulls) {
  set.seed(R58_SEED)
  pick <- sample(seq_len(nrow(frame)), min(R68_LEAK_ROWS, nrow(frame)))
  bad <- character(0)
  for (i in pick) {
    r <- frame[i, ]
    f <- fulls[[as.character(r$target)]] %>% dplyr::filter(player_id == r$player_id, week < r$week)
    if (!(nrow(f) == r$ytd_n && isTRUE(all.equal(mean(f$game_points), r$ytd_mean)))) {
      bad <- c(bad, glue("player {r$player_id} target {r$target} week {r$week}"))
    }
  }
  list(n_rows_checked = length(pick), failures = bad)
}

# ------------------------------------------------------------------------------
# Fit / predict (one position, one training set)
# ------------------------------------------------------------------------------
.r68_base <- function(df, pos, k) {
  w <- if (pos == "QB" || is.na(k)) df$w_lin else df$ytd_n / (df$ytd_n + k)
  (1 - w) * df$prior_mu + w * df$ytd_mean
}

.r68_x <- function(df, base, center) {
  ok <- !is.na(df$team_implied)
  list(add = dplyr::if_else(ok, df$team_implied - center, 0),
       pro = dplyr::if_else(ok, base * (df$team_implied - center) / center, 0))
}

.r68_fit <- function(tr, pos) {
  y <- tr$game_points
  k0 <- NA_real_
  if (pos != "QB") {
    rm0 <- vapply(R58_K_GRID, function(k) .r68_rmse(y, .r68_base(tr, pos, k)), numeric(1))
    k0 <- R58_K_GRID[which.min(rm0)]
  }
  base   <- .r68_base(tr, pos, k0)
  center <- mean(tr$team_implied, na.rm = TRUE)
  x      <- .r68_x(tr, base, center)
  list(position = pos, n_train = nrow(tr), k0 = k0, center = center,
       b_add = .r68_ols1(y - base, x$add),
       b_pro = .r68_ols1(y - base, x$pro))
}

.r68_predict <- function(df, pos, fit) {
  base <- .r68_base(df, pos, fit$k0)
  x    <- .r68_x(df, base, fit$center)
  df %>% dplyr::mutate(pred_base = base,
                       pred_add  = base + fit$b_add * x$add,
                       pred_pro  = base + fit$b_pro * x$pro,
                       k0 = fit$k0, center = fit$center, b_add = fit$b_add, b_pro = fit$b_pro)
}

.r68_fit_row <- function(fit, scope) {
  tibble::tibble(scope = scope, position = fit$position, n_train = fit$n_train,
                 k0 = fit$k0, center = fit$center, b_add = fit$b_add, b_pro = fit$b_pro)
}

.r68_boot <- function(df, cand_col, reps = R58_BOOT_REPS, level = R58_BOOT_LEVEL, seed = R58_SEED) {
  set.seed(seed)
  y <- df$game_points; b <- df$pred_base; cc <- df[[cand_col]]
  by_cl <- split(seq_len(nrow(df)), df$cluster)
  cls   <- names(by_cl)
  stat  <- function(idx) sqrt(mean((y[idx] - b[idx])^2)) - sqrt(mean((y[idx] - cc[idx])^2))
  point <- stat(seq_len(nrow(df)))
  draws <- vapply(seq_len(reps), function(i) {
    stat(unlist(by_cl[sample(cls, length(cls), replace = TRUE)], use.names = FALSE))
  }, numeric(1))
  a <- (1 - level) / 2
  c(improvement = point, lo = unname(stats::quantile(draws, a)),
    hi = unname(stats::quantile(draws, 1 - a)))
}

.r68_spearman_by_week <- function(df, col) {
  r <- df %>% dplyr::group_by(target, week) %>%
    dplyr::filter(dplyr::n() >= 10) %>%
    dplyr::summarise(r = suppressWarnings(stats::cor(game_points, .data[[col]], method = "spearman")),
                     .groups = "drop")
  mean(r$r, na.rm = TRUE)
}

.r68_summarise <- function(rows, scope) {
  purrr::map_dfr(R68_TESTS, function(t) {
    purrr::map_dfr(R58_POSITIONS, function(pos) {
      d <- rows %>% dplyr::filter(position == pos)
      if (nrow(d) < 50) return(NULL)
      b <- .r68_boot(d, t$cand)
      tibble::tibble(
        scope = scope, test = t$test, position = pos,
        n = nrow(d), n_clusters = dplyr::n_distinct(d$cluster),
        rmse_base = .r68_rmse(d$game_points, d$pred_base),
        rmse_cand = .r68_rmse(d$game_points, d[[t$cand]]),
        improvement = b[["improvement"]], lo = b[["lo"]], hi = b[["hi"]],
        spearman_base = .r68_spearman_by_week(d, "pred_base"),
        spearman_cand = .r68_spearman_by_week(d, t$cand),
        passed = is.finite(b[["lo"]]) && b[["lo"]] > 0
      )
    })
  })
}

# ------------------------------------------------------------------------------
# run_offense_vegas_gate
# ------------------------------------------------------------------------------
run_offense_vegas_gate <- function(target_seasons = R68_TARGET_SEASONS,
                                   scoring_settings = DEFAULT_SCORING_SETTINGS,
                                   write_outputs = TRUE) {
  target_seasons <- sort(unique(as.integer(target_seasons)))
  if (length(target_seasons) < 2) stop("R/68: need at least two target seasons.", call. = FALSE)

  scoring_key  <- .r29_scoring_key(scoring_settings)
  is_default   <- .r29_is_default_scoring(scoring_settings)
  gate_path    <- scoring_gate_path(R68_GATE_PATH, scoring_settings)
  results_path <- scoring_gate_path(R68_RESULTS_PATH, scoring_settings)
  summary_csv  <- scoring_gate_path(R68_SUMMARY_CSV, scoring_settings)

  message(glue("\n{strrep('=', 70)}"))
  message("R/68: offense Vegas gate (team implied total; additive and proportional)")
  message(glue("Leave-one-season-out over: {paste(target_seasons, collapse = ', ')}"))
  message(glue("Scoring key {scoring_key}{if (is_default) ' (default flat PPR)' else ''}; ",
               "gate -> {basename(gate_path)}"))
  message(glue("{strrep('=', 70)}\n"))

  message("STEP 1/5: lines")
  lines <- .r68_lines(target_seasons)

  message("\nSTEP 2/5: frames (R/58 construction + team + line)")
  frs <- lapply(target_seasons, .r68_frame, scoring_settings = scoring_settings, lines = lines)
  names(frs) <- as.character(target_seasons)
  frame <- dplyr::bind_rows(lapply(frs, `[[`, "frame"))
  fulls <- lapply(frs, `[[`, "full")
  message("\n  Rows by target x position (compare with the R/58 run):")
  print(frame %>% dplyr::count(target, position) %>%
          tidyr::pivot_wider(names_from = position, values_from = n))
  message("\n  team_implied by target (rows with a line):")
  print(frame %>% dplyr::filter(!is.na(team_implied)) %>% dplyr::group_by(target) %>%
          dplyr::summarise(mean = round(mean(team_implied), 2), sd = round(stats::sd(team_implied), 2),
                           min = min(team_implied), max = max(team_implied), .groups = "drop"))

  message("\nSTEP 3/5: leakage assertion (YTD over earlier games only)")
  leak <- .r68_leak_check(frame, fulls)
  message(glue("  Checked {leak$n_rows_checked} rows: {length(leak$failures)} failure(s)"))
  if (length(leak$failures) > 0) {
    print(utils::head(leak$failures, 20))
    stop("R/68: leakage assertion failed. No fitting was run.", call. = FALSE)
  }

  message("\nSTEP 4/5: leave-one-season-out folds")
  fold_fits <- list()
  folds <- purrr::map_dfr(target_seasons, function(h) {
    tr_seasons <- setdiff(target_seasons, h)
    message(glue("  holdout {h}, train {paste(tr_seasons, collapse = '+')}"))
    purrr::map_dfr(R58_POSITIONS, function(pos) {
      tr  <- frame %>% dplyr::filter(target %in% tr_seasons, position == pos)
      ho  <- frame %>% dplyr::filter(target == h, position == pos)
      fit <- .r68_fit(tr, pos)
      fold_fits[[paste(h, pos)]] <<- .r68_fit_row(fit, as.character(h))
      .r68_predict(ho, pos, fit)
    })
  })
  fold_params <- dplyr::bind_rows(fold_fits)
  message("\n  Per-fold fitted parameters:")
  print(fold_params %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))), n = 20)

  per_fold <- purrr::map_dfr(target_seasons, function(h) {
    .r68_summarise(folds %>% dplyr::filter(target == h), as.character(h))
  })
  pooled <- .r68_summarise(folds, "pooled_loso")
  message("\n  Pooled LOSO results:")
  print(pooled %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))), n = 20, width = Inf)
  message("\n  Per-holdout results:")
  print(per_fold %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))), n = 40, width = Inf)

  message("\nSTEP 5/5: decisions, all-season parameters, outputs")
  all_fits <- stats::setNames(lapply(R58_POSITIONS, function(pos) {
    .r68_fit(frame %>% dplyr::filter(position == pos), pos)
  }), R58_POSITIONS)
  all_params <- dplyr::bind_rows(lapply(all_fits, .r68_fit_row, scope = "all_seasons"))

  decisions <- purrr::map_dfr(R58_POSITIONS, function(pos) {
    p <- pooled %>% dplyr::filter(position == pos, passed) %>% dplyr::arrange(dplyr::desc(improvement))
    f <- all_fits[[pos]]
    if (nrow(p) == 0) {
      return(tibble::tibble(position = pos, mode = "off", b = 0, center = f$center,
                            test = NA_character_, improvement = NA_real_, lo = NA_real_, hi = NA_real_,
                            neutral_value = "b = 0", revive_condition = R68_REVIVE))
    }
    w <- p[1, ]
    tibble::tibble(position = pos,
                   mode = if (w$test == "c1_additive") "additive" else "proportional",
                   b = if (w$test == "c1_additive") f$b_add else f$b_pro,
                   center = f$center, test = w$test, improvement = w$improvement,
                   lo = w$lo, hi = w$hi, neutral_value = "b = 0", revive_condition = NA_character_)
  })
  message("\n  Decisions (what R/59 will apply):")
  print(decisions %>% dplyr::select(position, mode, b, center, test, improvement, lo, hi) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))), n = 10)

  gate_obj <- list(
    schema_tag = R68_SCHEMA_TAG, built_at = Sys.time(),
    scoring_key = scoring_key, is_default_scoring = is_default,
    target_seasons = target_seasons,
    method = glue("leave-one-season-out, pooled team-season cluster bootstrap, ",
                  "{round(100 * R58_BOOT_LEVEL)}% lower bound > 0 vs production-shape baseline"),
    decisions = decisions, params_all_seasons = all_params, pooled = pooled, per_fold = per_fold,
    leak_check = leak,
    provenance = list(nflreadr_version = as.character(utils::packageVersion("nflreadr")),
                      scoring_settings = scoring_settings,
                      boot_reps = R58_BOOT_REPS, boot_level = R58_BOOT_LEVEL, seed = R58_SEED),
    limits = c("Lines are closing schedule lines; production pulls earlier lines.",
               "Baseline is the R/58 proxy prior, not R/32's reconciled mean.",
               "WR baseline omits R/63's xFP blend.")
  )
  if (write_outputs) {
    dir.create(R68_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    saveRDS(gate_obj, gate_path)
    saveRDS(list(folds = folds, fold_params = fold_params, scoring_key = scoring_key), results_path)
    readr::write_csv(dplyr::bind_rows(per_fold, pooled), summary_csv)
    message(glue("  Wrote {gate_path}"))
    message(glue("  Wrote {results_path}"))
    message(glue("  Wrote {summary_csv}"))
  }
  on <- decisions %>% dplyr::filter(mode != "off")
  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/68 GATE: {nrow(on)} position(s) pass",
               "{if (nrow(on) > 0) paste0(': ', paste(on$position, on$mode, sep = '=', collapse = ', ')) else ''}"))
  message("R/59 applies exactly these on its next run; positions marked off get no shift.")
  message(glue("{strrep('=', 70)}\n"))
  invisible(gate_obj)
}
