# ==============================================================================
# 66_def_gate.R
#
# Out-of-sample gate for the DEF/ST projection.  [2026-10-01]
#
# WHAT IS BEING TESTED
#   Baseline: R/34 project_def_st() exactly as production computes it before
#   week W: the three-prior-season team DEF PPG, regressed REGRESSION_STRENGTH
#   toward the league mean, blended with season-to-date DEF PPG through week
#   W-1 at prior weight compute_prior_weight(W - 1) (R/34's as_of_week is the
#   last COMPLETED week; week 1 and teams with no game played stay on the
#   prior). Nothing in the baseline is fit here.
#
#   C1  R/35 opponent-offense matchup factor, exactly as production builds it:
#       compute_def_matchup_factors(seasons = target - 1, cap = 0.25),
#       applied multiplicatively (pred_base * factor of the opponent offense).
#       Was live in production at 0.25 and untested; FAILED 2026-10-01 and
#       neutralized in R/35 (DVP_CAP_DEF = 0.00). The tested cap is pinned in
#       R66_C1_CAP_TESTED so a re-run still tests 0.25.
#
#   C2  Vegas implied total of the OPPONENT (load_schedules spread_line and
#       total_line; spread_line > 0 means the home team is favored):
#         home implied = (total + spread) / 2, away implied = (total - spread) / 2
#       Shifter b * (opp_implied - center), center = training-fold mean,
#       b by no-intercept OLS on the baseline's training-fold residuals.
#       A missing line contributes 0; no row is ever dropped.
#
#   DIAGNOSTIC (reported, NOT gated): C2 refit on top of C1.
#
# DESIGN (forks locked 2026-10-01)
#   Unit    : team-game, regular season, target seasons 2023-2025.
#   Gate    : leave-one-season-out, pooled paired errors, team-cluster
#             bootstrap (500 reps, 90 percent, seed as R/58). A candidate
#             passes when the lower bound on RMSE(base) - RMSE(cand) is > 0.
#   Ranking : within season-week Spearman reported for every arm, not gated.
#   Fidelity: the reconstructed prior is asserted equal to R/34's own
#             .compute_def_prior() for the first target before any scoring;
#             the run stops on a mismatch. C1 is R/35's function, not a copy.
#   Leakage : sampled rows are recomputed from games before week W only, and
#             the prior weight is re-derived; the run stops on any mismatch.
#
# SCORING
#   Default is R/34's DEF_SCORING_DEFAULT (R/29's Sleeper-standard constants).
#   FanDuel D/ST scoring matches it on every tier and event (fanduel.com/rules,
#   checked 2026-10-01), so one run covers FanDuel. R/29 counts every play
#   whose description contains "blocked" as a blocked kick, slightly broader
#   than FanDuel's blocked punt; logged, not modelled. C1 always uses R/35's
#   standard-scored factor, because that is what production applies.
#
# LIMITS (stated so they are not forgotten)
#   - load_schedules carries CLOSING lines. Lineups lock near kickoff, so this
#     is close to what is available, but slightly optimistic for C2.
#   - 32 team clusters; intervals are wider than the player gates.
#   - C2 is linear; points-allowed tiers are not. Distributional form (implied
#     total -> expected tier points) is the revive path if C2 fails.
#
# OUTPUTS (data/season3_cache/, schema s3_r66_def_gate_v1)
#   s3_r66_def_gate.rds          decisions, all-season parameters, provenance
#   s3_r66_def_gate_results.rds  paired predictions, per-fold fits, by-week
#   s3_r66_def_gate_summary.csv  per-fold and pooled rows
#   Nothing is wired into R/34 or R/35 by this file.
#
# USAGE
#   source(here::here("R", "66_def_gate.R"))
#   res66 <- run_def_gate()
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(glue)
library(readr)
library(nflreadr)
library(here)

if (!exists("project_def_st")) {
  source(here::here("R", "34_def_st_projection.R"))   # sources R/29, R/15, R/19
}
if (!exists("compute_def_matchup_factors")) {
  source(here::here("R", "35_lineup_optimizer.R"))
}

R66_OUTPUT_DIR   <- here::here("data", "season3_cache")
R66_GATE_PATH    <- file.path(R66_OUTPUT_DIR, "s3_r66_def_gate.rds")
R66_RESULTS_PATH <- file.path(R66_OUTPUT_DIR, "s3_r66_def_gate_results.rds")
R66_SUMMARY_CSV  <- file.path(R66_OUTPUT_DIR, "s3_r66_def_gate_summary.csv")
R66_SCHEMA_TAG   <- "s3_r66_def_gate_v1"

R66_TARGET_SEASONS <- 2023:2025
R66_BOOT_REPS  <- 500L
R66_BOOT_LEVEL <- 0.90
R66_SEED       <- 20260918L          # same seed as R/58 and R/63
R66_LEAK_ROWS  <- 60L
R66_FIDELITY_TOL <- 1e-9
# The cap C1 was tested at (production value on 2026-10-01). Pinned here
# because R/35 neutralized DVP_CAP_DEF to 0.00 on the strength of this gate; a
# re-run reading DVP_CAP_DEF would silently test a factor of exactly 1.0.
R66_C1_CAP_TESTED <- 0.25

R66_TESTS <- list(
  list(test = "c1_matchup_factor", base = "pred_base", cand = "pred_c1",  gated = TRUE),
  list(test = "c2_vegas_implied",  base = "pred_base", cand = "pred_c2",  gated = TRUE),
  list(test = "diag_c2_over_c1",   base = "pred_c1",   cand = "pred_c12", gated = FALSE)
)

R66_NEUTRAL <- c(
  c1_matchup_factor = "DVP_CAP_DEF = 0.00 in R/35 (every def_matchup_factor exactly 1.0); keep compute_def_matchup_factors() and .apply_matchup()",
  c2_vegas_implied  = "b = 0 (not wired)"
)
R66_REVIVE <- c(
  c1_matchup_factor = paste("Revive when a re-run on an added season passes the pooled",
                            "LOSO gate, or a factor built from in-season conceded",
                            "points (weeks before W) passes it."),
  c2_vegas_implied  = paste("Revive when the distributional form (opponent implied",
                            "total -> expected points-allowed tier points) passes the gate.")
)

# ------------------------------------------------------------------------------
# Small helpers
# ------------------------------------------------------------------------------
.r66_rmse <- function(y, p) sqrt(mean((y - p)^2))

.r66_prior_weight <- function(w) {
  # R/34: as_of_week = W - 1; preseason (as_of_week < 1) is pure prior.
  vapply(w, function(wk) if (wk - 1L < 1L) 1.0 else compute_prior_weight(wk - 1L),
         numeric(1))
}

.r66_boot <- function(df, base_col, cand_col, reps = R66_BOOT_REPS,
                      level = R66_BOOT_LEVEL, seed = R66_SEED) {
  set.seed(seed)
  y <- df$def_st_points; b <- df[[base_col]]; cc <- df[[cand_col]]
  by_team <- split(seq_len(nrow(df)), df$team)
  teams   <- names(by_team)
  stat <- function(idx) sqrt(mean((y[idx] - b[idx])^2)) - sqrt(mean((y[idx] - cc[idx])^2))
  point <- stat(seq_len(nrow(df)))
  draws <- vapply(seq_len(reps), function(i) {
    stat(unlist(by_team[sample(teams, length(teams), replace = TRUE)], use.names = FALSE))
  }, numeric(1))
  a <- (1 - level) / 2
  c(improvement = point,
    lo = unname(stats::quantile(draws, a)),
    hi = unname(stats::quantile(draws, 1 - a)))
}

.r66_spearman_by_week <- function(df, col) {
  df %>%
    dplyr::group_by(target, week) %>%
    dplyr::filter(dplyr::n() >= 4) %>%
    dplyr::summarise(rho = suppressWarnings(stats::cor(.data[[col]], def_st_points,
                                                       method = "spearman")),
                     .groups = "drop") %>%
    dplyr::summarise(mean_rho = mean(rho, na.rm = TRUE)) %>%
    dplyr::pull(mean_rho)
}

# ------------------------------------------------------------------------------
# STEP 1: per team-game DEF points under the gate scoring (mirrors R/34)
# ------------------------------------------------------------------------------
.r66_season_games <- function(s, scoring, cache_dir) {
  pbp   <- load_normalized_season(s, cache_dir = cache_dir)
  games <- calculate_def_st_points(pbp)
  yds   <- .compute_yds_allowed(pbp)
  if (!is.null(yds)) {
    games <- dplyr::left_join(games, yds, by = c("season", "week", "game_id", "team"))
  }
  opp <- .game_opponent_map(pbp)
  out <- .score_def_games(games, scoring) %>%
    dplyr::left_join(opp, by = c("game_id", "team"), relationship = "many-to-one") %>%
    dplyr::transmute(season = as.integer(season), week = as.integer(week),
                     game_id, team, opponent, def_st_points)
  rm(pbp); invisible(gc(verbose = FALSE))
  message(glue("    {s}: {nrow(out)} team-games, {dplyr::n_distinct(out$team)} teams, ",
               "opponent NA {sum(is.na(out$opponent))}"))
  out
}

# R/34 .compute_def_prior() rebuilt from the per-game table (same mean of
# season PPG, same regression to the league mean of those means).
.r66_prior <- function(games, seasons, strength) {
  per_season <- games %>%
    dplyr::filter(season %in% seasons) %>%
    dplyr::group_by(season, team) %>%
    dplyr::summarise(def_ppg = mean(def_st_points, na.rm = TRUE), .groups = "drop")
  blended <- per_season %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(prior_raw_ppg = mean(def_ppg, na.rm = TRUE), .groups = "drop")
  lm_ <- mean(blended$prior_raw_ppg, na.rm = TRUE)
  blended %>% dplyr::mutate(def_prior_ppg = .regress_to_mean(prior_raw_ppg, lm_, strength))
}

# ------------------------------------------------------------------------------
# STEP 3: Vegas implied total of each team's OPPONENT
# ------------------------------------------------------------------------------
.r66_lines <- function(seasons) {
  sch <- nflreadr::load_schedules(seasons)
  need <- c("game_id", "season", "game_type", "home_team", "away_team",
            "spread_line", "total_line")
  miss <- setdiff(need, names(sch))
  if (length(miss) > 0) stop("R/66: load_schedules missing: ",
                             paste(miss, collapse = ", "), call. = FALSE)
  sch <- sch %>% dplyr::filter(game_type == "REG")
  # Convention check: spread_line > 0 must mean the home team is favored, so it
  # must correlate positively with the realized home margin. A wrong sign would
  # swap every implied total, so stop rather than score it.
  if (all(c("home_score", "away_score") %in% names(sch))) {
    rr <- stats::cor(sch$home_score - sch$away_score, sch$spread_line,
                     use = "complete.obs")
    message(glue("  spread_line vs realized home margin: r = {round(rr, 3)}"))
    if (!is.finite(rr) || rr <= 0) {
      stop("R/66: spread_line does not read as home-favored-positive. Not scoring C2.",
           call. = FALSE)
    }
  }
  message(glue("  Lines: {nrow(sch)} REG games, total NA {sum(is.na(sch$total_line))}, ",
               "spread NA {sum(is.na(sch$spread_line))}"))
  dplyr::bind_rows(
    sch %>% dplyr::transmute(game_id, team = home_team,
                             opp_implied = (total_line - spread_line) / 2),
    sch %>% dplyr::transmute(game_id, team = away_team,
                             opp_implied = (total_line + spread_line) / 2)
  )
}

# ------------------------------------------------------------------------------
# STEP 4: frame for one target season
# ------------------------------------------------------------------------------
.r66_frame <- function(ts, games, lines, factors, strength) {
  prior <- .r66_prior(games, (ts - 3L):(ts - 1L), strength)
  tgt <- games %>% dplyr::filter(season == ts, !is.na(def_st_points))
  n0  <- nrow(tgt)

  out <- tgt %>%
    dplyr::arrange(team, week, game_id) %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(ytd_n    = dplyr::row_number() - 1L,
                  ytd_sum  = cumsum(def_st_points) - def_st_points,
                  ytd_mean = dplyr::if_else(ytd_n > 0L, ytd_sum / ytd_n, NA_real_)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(prior %>% dplyr::select(team, def_prior_ppg),
                     by = "team", relationship = "many-to-one") %>%
    dplyr::left_join(factors %>% dplyr::select(offense_team, def_matchup_factor),
                     by = c("opponent" = "offense_team"), relationship = "many-to-one") %>%
    dplyr::left_join(lines, by = c("game_id", "team"), relationship = "many-to-one") %>%
    dplyr::mutate(
      target       = ts,
      prior_weight = .r66_prior_weight(week),
      has_observed = ytd_n > 0L,
      pred_base    = .blend_prior_observed(def_prior_ppg, ytd_mean, prior_weight, has_observed),
      factor       = dplyr::coalesce(def_matchup_factor, 1),
      pred_c1      = pred_base * factor
    )
  stopifnot(nrow(out) == n0)

  message(glue("    target {ts}: {nrow(out)} rows; prior NA {sum(is.na(out$def_prior_ppg))}, ",
               "factor NA {sum(is.na(out$def_matchup_factor))}, ",
               "line NA {sum(is.na(out$opp_implied))}"))
  if (any(is.na(out$def_prior_ppg))) {
    stop("R/66: ", sum(is.na(out$def_prior_ppg)), " row(s) with no prior in ", ts,
         " (team code with no history). Not imputing.", call. = FALSE)
  }
  out
}

# ------------------------------------------------------------------------------
# STEP 5: leak check and fidelity
# ------------------------------------------------------------------------------
.r66_leak_check <- function(frame, games) {
  set.seed(R66_SEED)
  pick <- unique(c(sample(which(frame$week <= 3L), min(15L, sum(frame$week <= 3L))),
                   sample(seq_len(nrow(frame)), min(R66_LEAK_ROWS, nrow(frame)))))
  bad <- character(0)
  for (i in pick) {
    r <- frame[i, ]
    f <- games %>% dplyr::filter(season == r$target, team == r$team, week < r$week,
                                 !is.na(def_st_points))
    pw <- if (r$week - 1L < 1L) 1 else compute_prior_weight(r$week - 1L)
    ok <- nrow(f) == r$ytd_n &&
      (r$ytd_n == 0L || isTRUE(all.equal(mean(f$def_st_points), r$ytd_mean))) &&
      isTRUE(all.equal(pw, r$prior_weight))
    if (!ok) bad <- c(bad, glue("{r$team} {r$target} week {r$week}"))
  }
  list(n_rows_checked = length(pick), failures = bad)
}

# ------------------------------------------------------------------------------
# Fit and predict
# ------------------------------------------------------------------------------
.r66_fit <- function(tr) {
  ok <- !is.na(tr$opp_implied)
  center <- mean(tr$opp_implied[ok])
  x <- dplyr::if_else(ok, tr$opp_implied - center, 0)
  r0 <- tr$def_st_points - tr$pred_base
  r1 <- tr$def_st_points - tr$pred_c1
  list(center = center,
       b2  = if (sum(x^2) > 0) sum(x * r0) / sum(x^2) else 0,
       b12 = if (sum(x^2) > 0) sum(x * r1) / sum(x^2) else 0,
       n_train = nrow(tr), n_lines = sum(ok))
}

.r66_predict <- function(df, fit) {
  x <- dplyr::if_else(is.na(df$opp_implied), 0, df$opp_implied - fit$center)
  df %>% dplyr::mutate(x_implied = x,
                       pred_c2  = pred_base + fit$b2 * x,
                       pred_c12 = pred_c1 + fit$b12 * x)
}

.r66_summarise <- function(rows, scope) {
  purrr::map_dfr(R66_TESTS, function(t) {
    b <- .r66_boot(rows, t$base, t$cand)
    tibble::tibble(
      scope = scope, test = t$test, gated = t$gated, n = nrow(rows),
      rmse_base = .r66_rmse(rows$def_st_points, rows[[t$base]]),
      rmse_cand = .r66_rmse(rows$def_st_points, rows[[t$cand]]),
      improvement = b[["improvement"]], lo = b[["lo"]], hi = b[["hi"]],
      passed = t$gated && is.finite(b[["lo"]]) && b[["lo"]] > 0
    )
  })
}

# ------------------------------------------------------------------------------
# run_def_gate
# ------------------------------------------------------------------------------
run_def_gate <- function(target_seasons = R66_TARGET_SEASONS,
                         scoring = DEF_SCORING_DEFAULT,
                         regression_strength = REGRESSION_STRENGTH,
                         cache_dir = CACHE_DIR_DEFAULT,
                         write_outputs = TRUE) {

  target_seasons <- sort(unique(as.integer(target_seasons)))
  if (length(target_seasons) < 2) stop("R/66: need at least two target seasons.", call. = FALSE)
  game_seasons <- (min(target_seasons) - 3L):max(target_seasons)

  message(glue("\n{strrep('=', 70)}"))
  message("R/66: DEF/ST gate -- R/34 baseline vs R/35 matchup factor vs Vegas implied total")
  message(glue("Leave-one-season-out over: {paste(target_seasons, collapse = ', ')}"))
  message(glue("Production: R/34 regression {regression_strength}, prior weight ",
               "compute_prior_weight(W - 1); C1 cap tested {R66_C1_CAP_TESTED} ",
               "(R/35 DVP_CAP_DEF now {DVP_CAP_DEF})"))
  message(glue("{strrep('=', 70)}\n"))

  message("STEP 1/7: per team-game DEF points (R/29 counts, R/34 scoring)")
  games <- purrr::map_dfr(game_seasons, .r66_season_games,
                          scoring = scoring, cache_dir = cache_dir)

  message("\nSTEP 2/7: fidelity -- reconstructed prior vs R/34 .compute_def_prior()")
  ts0 <- min(target_seasons)
  ref <- .compute_def_prior((ts0 - 3L):(ts0 - 1L), scoring, cache_dir,
                            regression_strength = regression_strength)
  mine <- .r66_prior(games, (ts0 - 3L):(ts0 - 1L), regression_strength)
  cmp <- dplyr::full_join(ref %>% dplyr::select(team, ref = def_prior_ppg),
                          mine %>% dplyr::select(team, mine = def_prior_ppg), by = "team")
  max_diff <- max(abs(cmp$ref - cmp$mine))
  message(glue("  {ts0} prior: {nrow(cmp)} teams, max |R/34 - R/66| = {signif(max_diff, 3)}"))
  if (!is.finite(max_diff) || max_diff > R66_FIDELITY_TOL) {
    print(cmp %>% dplyr::filter(!(abs(ref - mine) <= R66_FIDELITY_TOL)))
    stop("R/66: reconstructed prior does not match R/34. Nothing was scored.", call. = FALSE)
  }

  message("\nSTEP 3/7: R/35 matchup factors (prior season) and Vegas lines")
  factors <- stats::setNames(lapply(target_seasons, function(ts) {
    compute_def_matchup_factors(seasons = ts - 1L, cache_dir = cache_dir,
                                cap = R66_C1_CAP_TESTED)
  }), target_seasons)
  lines <- .r66_lines(target_seasons)

  message("\nSTEP 4/7: frames")
  frame <- dplyr::bind_rows(lapply(target_seasons, function(ts) {
    .r66_frame(ts, games, lines, factors[[as.character(ts)]], regression_strength)
  }))
  print(frame %>% dplyr::count(target, name = "team_games"))

  message("\nSTEP 5/7: leakage assertion")
  leak <- .r66_leak_check(frame, games)
  message(glue("  Checked {leak$n_rows_checked} rows: {length(leak$failures)} failure(s)"))
  if (length(leak$failures) > 0) {
    print(utils::head(leak$failures, 20))
    stop("R/66: leakage assertion failed. No fitting was run.", call. = FALSE)
  }

  message("\nSTEP 6/7: leave-one-season-out folds")
  fold_fits <- list()
  folds <- purrr::map_dfr(target_seasons, function(h) {
    tr  <- frame %>% dplyr::filter(target != h)
    fit <- .r66_fit(tr)
    fold_fits[[as.character(h)]] <<- tibble::tibble(
      holdout = h, center = fit$center, b2 = fit$b2, b12 = fit$b12,
      n_train = fit$n_train, n_lines = fit$n_lines)
    .r66_predict(frame %>% dplyr::filter(target == h), fit)
  })
  fold_params <- dplyr::bind_rows(fold_fits)
  message("  Per-fold C2 fits:")
  print(fold_params %>% dplyr::mutate(dplyr::across(c(center, b2, b12), ~ round(.x, 4))))

  per_fold <- purrr::map_dfr(target_seasons, function(h) {
    .r66_summarise(folds %>% dplyr::filter(target == h), as.character(h))
  })
  pooled <- .r66_summarise(folds, "pooled_loso")
  message("\n  Pooled LOSO results:")
  print(pooled %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))), width = Inf)
  message("\n  Per-holdout results:")
  print(per_fold %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))),
        n = 30, width = Inf)

  spearman <- tibble::tibble(
    arm = c("base", "c1", "c2", "c12"),
    mean_within_week_rho = vapply(c("pred_base", "pred_c1", "pred_c2", "pred_c12"),
                                  function(cl) .r66_spearman_by_week(folds, cl), numeric(1)))
  message("\n  Within season-week Spearman (diagnostic, not gated):")
  print(spearman %>% dplyr::mutate(mean_within_week_rho = round(mean_within_week_rho, 4)))

  by_week <- folds %>%
    dplyr::group_by(week) %>%
    dplyr::summarise(n = dplyr::n(),
                     rmse_base = .r66_rmse(def_st_points, pred_base),
                     rmse_c1   = .r66_rmse(def_st_points, pred_c1),
                     rmse_c2   = .r66_rmse(def_st_points, pred_c2),
                     .groups = "drop")

  message("\nSTEP 7/7: decisions, all-season parameters, outputs")
  all_fit <- .r66_fit(frame)
  decisions <- pooled %>%
    dplyr::filter(gated) %>%
    dplyr::mutate(
      wiring_eligible  = passed,
      neutral_value    = unname(R66_NEUTRAL[test]),
      revive_condition = unname(R66_REVIVE[test]),
      evidence = glue::glue("pooled LOSO improvement {round(improvement, 3)} ",
                            "[{round(lo, 3)}, {round(hi, 3)}], n = {n}")
    )
  message("\n  Decisions:")
  print(decisions %>% dplyr::select(test, improvement, lo, hi, passed, wiring_eligible) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))))

  gate_obj <- list(
    schema_tag     = R66_SCHEMA_TAG,
    built_at       = Sys.time(),
    target_seasons = target_seasons,
    method         = "leave-one-season-out, pooled team-cluster bootstrap, 90% lower bound > 0 vs R/34 production rule",
    decisions      = decisions,
    params_all_seasons = tibble::tibble(center = all_fit$center, b2 = all_fit$b2,
                                        b12 = all_fit$b12, n = all_fit$n_train),
    pooled = pooled, per_fold = per_fold, spearman = spearman, leak_check = leak,
    provenance = list(
      def_scoring          = scoring,
      regression_strength  = regression_strength,
      c1_cap_tested        = R66_C1_CAP_TESTED,
      prior_weight_rule    = "compute_prior_weight(W - 1); W = 1 pure prior",
      prior_fidelity_max_abs_diff = max_diff,
      nflreadr_version     = as.character(utils::packageVersion("nflreadr")),
      boot_reps = R66_BOOT_REPS, boot_level = R66_BOOT_LEVEL, seed = R66_SEED
    ),
    limits = c(
      "Closing lines from load_schedules; slightly optimistic for C2.",
      "32 team clusters.",
      "C2 is linear; points-allowed tiers are not.",
      "R/29 counts any 'blocked' play description as a blocked kick."
    )
  )

  if (write_outputs) {
    dir.create(R66_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    saveRDS(gate_obj, R66_GATE_PATH)
    saveRDS(list(folds = folds, fold_params = fold_params, by_week = by_week,
                 factors = factors, n_rows = nrow(frame)), R66_RESULTS_PATH)
    readr::write_csv(dplyr::bind_rows(per_fold, pooled), R66_SUMMARY_CSV)
    message(glue("  Wrote {R66_GATE_PATH}"))
    message(glue("  Wrote {R66_RESULTS_PATH}"))
    message(glue("  Wrote {R66_SUMMARY_CSV}"))
  }

  passed_txt <- decisions %>% dplyr::filter(wiring_eligible)
  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/66 GATE: {nrow(passed_txt)} candidate(s) passed",
               "{if (nrow(passed_txt) > 0) paste0(': ', paste(passed_txt$test, collapse = ', ')) else ''}"))
  message("Nothing is wired into R/34 or R/35 by this run.")
  message(glue("{strrep('=', 70)}\n"))

  invisible(gate_obj)
}
