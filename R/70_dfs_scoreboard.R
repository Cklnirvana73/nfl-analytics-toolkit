# ==============================================================================
# 70_dfs_scoreboard.R
#
# Forward scoreboard for the DFS optimizer.  [2026-10-22]
#
# WHY
#   Rules, the Vegas shift and the simulation are only worth keeping if they
#   beat the plain mean-optimal lineup on real slates. Week 7 was the first
#   live snapshot. Weeks played before that, or missed, are rebuilt with
#   snapshot_dfs_week_retro() from the projections saved that week (RETRO).
#
# TWO STEPS PER WEEK
#   1. snapshot_dfs_week(week, ...)  BEFORE kickoff, after the final solve.
#      Solves the candidate set with your rules, simulates it (R/69), solves
#      the site-rules-only lineup too, and saves everything the score needs to
#      s3_fd_dfs_snapshot_wNN.rds. Nothing is re-solved later with hindsight
#      projections.
#   2. score_dfs_week(week)  AFTER the week (Tuesday, once the pbp cache is
#      rebuilt). Actual FanDuel points per player-game from the same scorer
#      R/29 and the gates use (.season_game_points under FANDUEL_SCORING), DEF
#      from R/29 calculate_def_st_points. Appends one row per variant to
#      s3_fd_dfs_scoreboard.csv (a re-score of the same week replaces its rows).
#
# VARIANTS SCORED EVERY WEEK
#   mean_site     best projected mean under site rules only
#   mean_rules    best projected mean under the rules you snapshotted
#   sim_p80 / sim_p90 / sim_mean   top of the candidate set by that sim objective
# For each: projected, realized, error, whether the realized total fell inside
# the sim's p10-p90 (target 80 percent of weeks) and above p95 (target 5
# percent), and efficiency = realized / hindsight optimum (the best legal
# lineup from the same eligible pool and salaries had you known the actual
# points).
#
# summary_dfs_scoreboard(): per variant means; paired bootstrap of each
# variant minus mean_site only once 4 or more weeks are scored. No claim about
# which variant is better before that.
#
# LIMITS
#   - A player with no gsis id (id-less rookies) cannot be scored from pbp;
#     he is counted as 0 and flagged (n_unscored).
#   - Hindsight optimum uses the snapshot's eligible pool: a questionable player
#     you left out is out of the hindsight lineup too.
#
# RETRO  [2026-11-05]
#   snapshot_dfs_week_retro(week, salary_path) rebuilds a snapshot for a week
#   that was never snapshotted, from that week's salary CSV and the
#   s3_fd_r59_weekly_bands_wNN / s3_fd_def_st_wNN files its chain run wrote.
#   Projections and availability are what the model held before kickoff; a
#   leak guard stops if the bands were built after the slate's first game day.
#   Re-solved with the current solver under site rules only, because the rules
#   played that week were not recorded, so mean_rules equals mean_site. Writes
#   only the snapshot (pool and lineup CSVs on disk are left alone) and never
#   overwrites an existing snapshot. Scoreboard rows carry retro = TRUE and the
#   bands schema tag, so rebuilt weeks can be read apart from live ones.
#
# BLEND  [2026-11-05]
#   From R/65 v3 the solver's proj is the FanDuel blend when the R/71 gate
#   passes. Snapshots keep proj_ours, file_proj and blend_w per player so R/71
#   can refit on every scored week; scoreboard rows carry blend_w (NA = ours
#   only), so weeks played on ours and on the blend can be read apart.
#
# USAGE
#   source(here::here("R", "70_dfs_scoreboard.R"))
#   snapshot_dfs_week(7, rules = dfs_rules(qb_stack = 1), excludes = "Deshaun Watson")
#   # Tuesday:
#   score_dfs_week(7); summary_dfs_scoreboard()
#   # A week never snapshotted, rebuilt from its saved files:
#   snapshot_dfs_week_retro(5, here::here("data", "dfs_salaries", "week5.csv"))
#   score_dfs_week(5)
# ==============================================================================

library(dplyr)
library(tibble)
library(glue)
library(readr)
library(here)
if (!exists("run_dfs_optimizer")) source(here::here("R", "65_daily_optimizer.R"))
if (!exists("simulate_lineups"))  source(here::here("R", "69_dfs_simulation.R"))
# .season_game_points() and GATE_CACHE_DIR live in R/49 (R/69 -> R/50 -> R/49
# loads them); explicit in case R/69 was sourced in a session without R/50.
if (!exists(".season_game_points") || !exists("GATE_CACHE_DIR")) {
  source(here::here("R", "49_uncertainty_calibration_gate.R"))
}

R70_SCHEMA_TAG <- "s3_r70_dfs_scoreboard_v3"   # v3: proj_ours, file_proj, blend_w in snapshot; blend_w column [2026-11-05]
R70_BOOT_REPS  <- 2000L
R70_MIN_WEEKS  <- 4L

.r70_snapshot_path <- function(cfg, week) .r65_path(cfg, "dfs_snapshot", week, "rds")
.r70_board_path    <- function(cfg) file.path(R65_OUTPUT_DIR, paste0(cfg$prefix, "dfs_scoreboard.csv"))

# ------------------------------------------------------------------------------
# 1. Snapshot (before kickoff)
# ------------------------------------------------------------------------------

# Everything after the solve, shared by the live and retro snapshots.
.r70_build_snapshot <- function(res, cfg, week, season, site, rules, locks, excludes,
                                include_q, n_draws, extra = list()) {
  pool <- res$pool
  li <- .r65_resolve_names(locks, pool, "lock")
  ei <- .r65_resolve_names(excludes, pool, "exclude")
  site_best <- solve_dfs_lineup(pool, cfg, li, ei, dfs_rules(cfg))
  if (site_best$status != 0) stop("R/70: no site-rules lineup with these locks/excludes.", call. = FALSE)

  lus <- res$lineups
  inputs <- load_sim_inputs()
  sim <- NULL
  if (!is.null(inputs) && !is.null(lus) && nrow(lus) > 0) {
    sim <- simulate_lineups(lus, inputs, n_draws = n_draws)
    message("\n  Simulated candidates (top 10 by p90):")
    print(rank_lineups(sim, "p90") %>% dplyr::slice_head(n = 10))
  } else {
    message("  Snapshot without simulation (no sim cache or no candidate set).")
  }
  # The site-rules lineup is simulated separately (same seed, different
  # roster, so not common random numbers with the candidate set); only its
  # p10 / p90 / p95 bands are used, for the coverage check.
  site_sim <- if (!is.null(inputs)) {
    simulate_lineups(tibble::tibble(lineup_id = "SITE", total_proj = round(site_best$total_proj, 2),
                                    lineup = list(site_best$lineup)), inputs, n_draws = n_draws)
  } else NULL

  snap <- list(
    schema_tag = R70_SCHEMA_TAG, built_at = Sys.time(), site = site, season = as.integer(season),
    week = as.integer(week), rules = rules, rules_text = .r65_rules_text(rules, cfg),
    locks = locks, excludes = excludes, include_q = include_q,
    pool = pool %>% dplyr::select(player_uid, nfl_gsis_id, site_name, position, team, opponent,
                                  salary, proj, sigma_wk, eligible, excluded_reason, value,
                                  dplyr::any_of(c("proj_ours", "file_proj", "blend_w"))),
    blend_w = if ("blend_w" %in% names(pool)) unique(stats::na.omit(pool$blend_w)) else numeric(0),
    lock_idx = li, exclude_idx = ei,
    mean_rules = list(lineup = res$lineup, total_proj = res$total_proj),
    mean_site  = list(lineup = site_best$lineup, total_proj = site_best$total_proj),
    lineups = lus, sim = sim, site_sim = site_sim,
    sim_label = if (!is.null(sim)) attr(sim, "label") else NA_character_,
    retro = FALSE
  )
  utils::modifyList(snap, extra)
}

#' Save the week's candidate lineups, sim and pool for later scoring
#' @param rules dfs_rules() you are playing with.
#' @param n_lineups,min_diff Candidate set for the sim.
#' @param n_draws Simulation draws.
#' @param salary_path Character or NULL (newest CSV in data/dfs_salaries/).
#' @export
snapshot_dfs_week <- function(week, season = 2026L, site = "fd", rules = NULL,
                              locks = character(0), excludes = character(0),
                              include_q = FALSE, n_lineups = 20L, min_diff = 1L,
                              n_draws = 10000L, salary_path = NULL) {
  cfg <- .r65_site(site)
  if (is.null(rules)) rules <- dfs_rules(cfg)
  res <- run_dfs_optimizer(week = week, season = season, site = site, salary_path = salary_path,
                           locks = locks, excludes = excludes, include_q = include_q, rules = rules,
                           n_lineups = n_lineups, min_diff = min_diff, write_outputs = TRUE)
  snap <- .r70_build_snapshot(res, cfg, week, season, site, rules, locks, excludes,
                              include_q, n_draws)
  p <- .r70_snapshot_path(cfg, week)
  saveRDS(snap, p)
  message(glue("\n  Wrote {p}"))
  invisible(snap)
}

# ------------------------------------------------------------------------------
# 1b. Retro snapshot (a week that was never snapshotted)  [2026-11-05]
# ------------------------------------------------------------------------------

.r70_require_cols <- function(df, need, what) {
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) {
    stop(glue("R/70 retro: {what} lacks {paste(miss, collapse = ', ')}. ",
              "This week cannot be rebuilt from its saved files."), call. = FALSE)
  }
  invisible(TRUE)
}

#' Rebuild a snapshot for a past week from the files saved that week
#'
#' Uses that week's salary CSV and the R/59 bands and R/34 DEF files written by
#' that week's run_dfs_chain(), so projections and availability are what the
#' model held before kickoff. Re-solves with the current solver under site
#' rules only (the rules played were not recorded), simulates with R/69, and
#' writes nothing except the snapshot. Refuses to overwrite an existing
#' snapshot, so a live pre-kickoff snapshot is never replaced.
#' @param salary_path Character. That week's salary CSV.
#' @export
snapshot_dfs_week_retro <- function(week, salary_path, season = 2026L, site = "fd",
                                    n_lineups = 20L, min_diff = 1L, n_draws = 10000L) {
  week <- as.integer(week); season <- as.integer(season)
  cfg <- .r65_site(site)
  p <- .r70_snapshot_path(cfg, week)
  if (file.exists(p)) {
    stop(glue("R/70 retro: {basename(p)} already exists; score it with score_dfs_week({week})."),
         call. = FALSE)
  }
  if (!file.exists(salary_path)) stop(glue("R/70 retro: {salary_path} not found."), call. = FALSE)

  bands_rds <- file.path(R65_OUTPUT_DIR, sprintf("%sr59_weekly_bands_w%02d.rds", cfg$prefix, week))
  def_rds   <- .r65_path(cfg, "def_st", week, "rds")
  for (f in c(bands_rds, def_rds)) {
    if (!file.exists(f)) stop(glue("R/70 retro: {basename(f)} not found."), call. = FALSE)
  }
  bands <- readRDS(bands_rds); def <- readRDS(def_rds)
  .r70_require_cols(bands, c("player_uid", "nfl_gsis_id", "player_name", "position", "team",
                             "season", "week", "mu", "sigma_wk", "p95", "availability",
                             "playable", "opponent", "on_bye", "built_at"), basename(bands_rds))
  .r70_require_cols(def, c("team", "def_proj_ppg", "vegas_shift", "lines_pulled_at"),
                    basename(def_rds))
  if (!identical(unique(as.integer(bands$week)), week)) {
    stop(glue("R/70 retro: {basename(bands_rds)} is not a week {week} file."), call. = FALSE)
  }

  # Leak guard: the saved projections must predate the slate's first game day.
  sal <- read_fd_salaries(salary_path)
  sch <- .r65_load_schedules(season) %>%
    dplyr::filter(.data$game_type == "REG", .data$week == !!week) %>%
    dplyr::mutate(game = paste(.r65_clean_abbr(away_team), "@", .r65_clean_abbr(home_team)))
  slate_days <- as.Date(sch$gameday[sch$game %in% unique(sal$game)])
  if (length(slate_days) == 0) {
    stop(glue("R/70 retro: no week {week} games match {basename(salary_path)}."), call. = FALSE)
  }
  first_day <- min(slate_days)
  bands_day <- as.Date(max(bands$built_at), tz = "America/Chicago")
  if (bands_day > first_day) {
    stop(glue("R/70 retro: bands built {bands_day}, slate starts {first_day}. ",
              "Not pre-kickoff projections; not rebuilding."), call. = FALSE)
  }
  bands_schema <- if ("schema_tag" %in% names(bands)) {
    paste(unique(bands$schema_tag), collapse = ",")
  } else NA_character_
  message(glue("\nR/70 retro: week {week} | salary {basename(salary_path)} | bands built ",
               "{format(max(bands$built_at), '%Y-%m-%d %H:%M')} (schema {bands_schema}) | ",
               "slate starts {first_day}"))

  rules <- dfs_rules(cfg)
  res <- run_dfs_optimizer(week = week, season = season, site = site, salary_path = salary_path,
                           rules = rules, n_lineups = n_lineups, min_diff = min_diff,
                           write_outputs = FALSE)
  snap <- .r70_build_snapshot(res, cfg, week, season, site, rules, character(0), character(0),
                              FALSE, n_draws,
                              extra = list(retro = TRUE, bands_schema = bands_schema,
                                           bands_built_at = max(bands$built_at),
                                           salary_file = basename(salary_path)))
  saveRDS(snap, p)
  message(glue("\n  Wrote {p}"))
  invisible(snap)
}

# ------------------------------------------------------------------------------
# 2. Score (after the week)
# ------------------------------------------------------------------------------

.r70_actuals <- function(season, week, scoring) {
  gp <- .season_game_points(season, scoring, cache_dir = GATE_CACHE_DIR)
  if (is.null(gp)) stop("R/70: no ", season, " pbp cache. Rebuild it first.", call. = FALSE)
  gp <- gp %>% dplyr::filter(week == !!as.integer(week))
  if (nrow(gp) == 0) stop(glue("R/70: no week {week} games in the {season} pbp cache yet. ",
                               "Rebuild it after the week is played."), call. = FALSE)
  pl <- gp %>% dplyr::group_by(player_id) %>%
    dplyr::summarise(actual = sum(game_points), .groups = "drop")
  if (!exists("calculate_def_st_points")) source(here::here("R", "29_projection_engine.R"))
  pbp <- load_normalized_season(season)
  df <- calculate_def_st_points(pbp) %>% dplyr::filter(week == !!as.integer(week)) %>%
    dplyr::transmute(team = .r65_clean_abbr(team), actual = def_st_points)
  list(players = pl, def = df, games = dplyr::n_distinct(gp$game_id))
}

.r70_attach_actual <- function(lu, act) {
  lu %>%
    dplyr::left_join(act$players, by = c("nfl_gsis_id" = "player_id"), na_matches = "never") %>%
    dplyr::rename(actual_pl = actual) %>%
    dplyr::left_join(act$def %>% dplyr::rename(actual_def = actual) %>%
                       dplyr::mutate(player_uid = paste0("DEF_", team)) %>% dplyr::select(-team),
                     by = "player_uid") %>%
    dplyr::mutate(unscored = position != "DEF" & is.na(nfl_gsis_id),
                  actual = dplyr::case_when(position == "DEF" ~ dplyr::coalesce(actual_def, 0),
                                            TRUE ~ dplyr::coalesce(actual_pl, 0))) %>%
    dplyr::select(-actual_pl, -actual_def)
}

.r70_row <- function(variant, lu, proj, act, sim_row, hindsight, snap) {
  a <- .r70_attach_actual(lu, act)
  realized <- sum(a$actual)
  tibble::tibble(
    site = snap$site, season = snap$season, week = snap$week, variant = variant,
    rules = if (variant == "mean_site") "site rules only" else snap$rules_text,
    retro = isTRUE(snap$retro),
    bands_schema = if (is.null(snap$bands_schema)) NA_character_ else snap$bands_schema,
    blend_w = if (length(snap$blend_w) == 1L) snap$blend_w else NA_real_,
    projected = round(proj, 2), realized = round(realized, 2), error = round(realized - proj, 2),
    sim_p10 = if (is.null(sim_row)) NA_real_ else sim_row$p10,
    sim_p90 = if (is.null(sim_row)) NA_real_ else sim_row$p90,
    sim_p95 = if (is.null(sim_row)) NA_real_ else sim_row$p95,
    in_p10_p90 = if (is.null(sim_row)) NA else realized >= sim_row$p10 & realized <= sim_row$p90,
    above_p95  = if (is.null(sim_row)) NA else realized > sim_row$p95,
    hindsight = round(hindsight, 2), efficiency = round(realized / hindsight, 4),
    n_unscored = sum(a$unscored),
    players = paste(a$site_name, collapse = "; "),
    schema_tag = R70_SCHEMA_TAG, scored_at = Sys.time()
  )
}

#' Score a snapshotted week against actual points and append to the scoreboard
#' @export
score_dfs_week <- function(week, site = "fd") {
  cfg  <- .r65_site(site)
  p    <- .r70_snapshot_path(cfg, week)
  if (!file.exists(p)) stop(glue("R/70: no snapshot for week {week} ({basename(p)}). ",
                                 "Run snapshot_dfs_week() before kickoff."), call. = FALSE)
  snap <- readRDS(p)
  scoring <- get(cfg$scoring_name)
  message(glue("\n{strrep('=', 70)}\nR/70: scoring {cfg$name} week {week} (snapshot {format(snap$built_at, '%a %m-%d %H:%M')})\n{strrep('=', 70)}"))
  act <- .r70_actuals(snap$season, week, scoring)
  message(glue("  actuals: {nrow(act$players)} players across {act$games} games; {nrow(act$def)} DEF"))

  # Hindsight optimum: same eligible pool, salaries, locks/excludes, site rules
  hp <- .r70_attach_actual(snap$pool, act) %>% dplyr::mutate(proj = actual)
  hs <- solve_dfs_lineup(hp, cfg, snap$lock_idx, snap$exclude_idx, dfs_rules(cfg))
  hindsight <- if (hs$status == 0) hs$total_proj else NA_real_
  message(glue("  hindsight optimum (same pool and salaries): {round(hindsight, 2)}"))

  rows <- list(
    .r70_row("mean_site", snap$mean_site$lineup, snap$mean_site$total_proj, act,
             if (is.null(snap$site_sim)) NULL else snap$site_sim[1, ], hindsight, snap),
    .r70_row("mean_rules", snap$mean_rules$lineup, snap$mean_rules$total_proj, act,
             if (is.null(snap$sim)) NULL else snap$sim[1, ], hindsight, snap)
  )
  if (!is.null(snap$sim)) {
    for (obj in c("mean", "p80", "p90")) {
      r <- rank_lineups(snap$sim, obj)[1, ]
      lu <- snap$lineups$lineup[[match(r$lineup_id, snap$lineups$lineup_id)]]
      rows[[length(rows) + 1]] <- .r70_row(paste0("sim_", obj), lu, r$total_proj, act, r, hindsight, snap)
    }
  }
  out <- dplyr::bind_rows(rows)
  print(out %>% dplyr::select(variant, projected, realized, error, sim_p10, sim_p90,
                              in_p10_p90, above_p95, hindsight, efficiency, n_unscored), width = Inf)

  bp <- .r70_board_path(cfg)
  old <- if (file.exists(bp)) {
    readr::read_csv(bp, col_types = readr::cols(.default = "c"))
  } else NULL
  new <- out %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  if (!is.null(old)) {
    old <- old %>% dplyr::filter(!(season == as.character(snap$season) & week == as.character(snap$week)))
  }
  board <- dplyr::bind_rows(old, new) %>%
    dplyr::arrange(as.integer(season), as.integer(week))
  readr::write_csv(board, bp)
  message(glue("  Wrote {bp} ({nrow(board)} rows, {dplyr::n_distinct(paste(board$season, board$week))} week(s))"))
  invisible(out)
}

#' Per-variant summary; paired comparison vs mean_site once enough weeks exist
#' @export
summary_dfs_scoreboard <- function(site = "fd", min_weeks = R70_MIN_WEEKS, seed = R69_SEED) {
  cfg <- .r65_site(site)
  bp <- .r70_board_path(cfg)
  if (!file.exists(bp)) stop("R/70: no scoreboard yet. Score a week first.", call. = FALSE)
  b <- readr::read_csv(bp, show_col_types = FALSE) %>%
    dplyr::mutate(in_p10_p90 = as.logical(in_p10_p90), above_p95 = as.logical(above_p95))
  if (!"retro" %in% names(b)) b$retro <- FALSE
  b <- b %>% dplyr::mutate(retro = dplyr::coalesce(as.logical(retro), FALSE))
  s <- b %>% dplyr::group_by(variant) %>%
    dplyr::summarise(weeks = dplyr::n(), retro_weeks = sum(retro), mean_projected = round(mean(projected), 2),
                     mean_realized = round(mean(realized), 2), mean_error = round(mean(error), 2),
                     mean_efficiency = round(mean(efficiency, na.rm = TRUE), 4),
                     cover_p10_p90 = round(mean(in_p10_p90, na.rm = TRUE), 3),
                     above_p95 = round(mean(above_p95, na.rm = TRUE), 3), .groups = "drop")
  print(s, width = Inf)
  nweeks <- dplyr::n_distinct(paste(b$season, b$week))
  if (nweeks < min_weeks) {
    message(glue("  {nweeks} week(s) scored; paired comparison starts at {min_weeks}. No verdict yet."))
    return(invisible(list(summary = s, paired = NULL)))
  }
  base <- b %>% dplyr::filter(variant == "mean_site") %>% dplyr::select(season, week, base = realized)
  set.seed(seed)
  paired <- b %>% dplyr::filter(variant != "mean_site") %>%
    dplyr::inner_join(base, by = c("season", "week")) %>%
    dplyr::group_by(variant) %>%
    dplyr::group_modify(function(d, k) {
      diff <- d$realized - d$base
      draws <- replicate(R70_BOOT_REPS, mean(sample(diff, length(diff), replace = TRUE)))
      tibble::tibble(weeks = length(diff), mean_diff_vs_mean_site = round(mean(diff), 2),
                     lo90 = round(stats::quantile(draws, 0.05, names = FALSE), 2),
                     hi90 = round(stats::quantile(draws, 0.95, names = FALSE), 2))
    }) %>% dplyr::ungroup()
  message("  Realized points vs the plain mean-optimal lineup (paired by week, 90% bootstrap):")
  print(paired, width = Inf)
  invisible(list(summary = s, paired = paired))
}
