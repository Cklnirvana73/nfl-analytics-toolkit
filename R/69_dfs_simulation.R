# ==============================================================================
# 69_dfs_simulation.R
#
# Simulate-and-rank for DFS lineups.  [2026-10-22]
#
# WHY
#   The optimizer maximizes the projected mean. On the week 7 FanDuel slate the
#   top five lineups were within 0.59 points of each other and every swap cost
#   0.25 to 1.16, so the mean cannot choose between them. What separates them
#   is how they are spread: a QB with his own receivers moves together (R/50:
#   QB-WR, QB-TE, QB-RB correlations replicated out of sample), and a
#   tournament pays for the right tail, not the mean.
#
# WHAT IT DOES
#   For a set of candidate lineups (solve_dfs_lineups() output), draws N joint
#   weeks for every player in any candidate:
#     players  mu = the pool's proj (R/59 mean, Vegas-shifted where R/68
#              passed), sigma = R/59 sigma_wk, standardized shape from the R/50
#              shape table, same-team dependence from the R/50 Gaussian copula
#              (QB cells kept, non-QB cells neutralized to 0 by R/50's policy;
#              Gaussian inputs calibrated so the copula returns the measured
#              correlations).
#     DEF      independent Normal(proj, sigma_def). sigma_def is the pooled
#              within-team weekly SD of DEF points (same estimator as R/36).
#   Common random numbers: each player has ONE draw vector shared by every
#   lineup he appears in, so differences between lineups are not Monte Carlo
#   noise.
#   Returns per lineup: mean, SD, p10/p50/p80/p90/p95 and P(total >= t) for
#   any thresholds passed. rank_lineups() orders by the objective chosen.
#
# ONE-TIME SETUP: build_sim_cache()
#   Builds and saves data/season3_cache/s3_r69_sim_cache.rds: the calibrated
#   copula matrix (R/50 functions, R/50's validated settings: residuals on
#   2023-2024, DK best-ball scoring, k = 12, the same fit the policy and the
#   shape table come from) and sigma_def (2025 DEF points, default scoring;
#   FanDuel D/ST scoring matches it). Rerun only when R/50 is rebuilt.
#
# LIMITS (stated so they are not forgotten)
#   - Not a calibrated distribution yet. The shape table and copula were built
#     on 2023-2024 under DK best-ball scoring; R/70 scores coverage weekly.
#     Until it has, read the sim as a RANKING of near-optimal lineups.
#   - DEF is independent of everything: a DEF facing your QB is not
#     anti-correlated here.
#   - Players on different teams are independent (game-level correlation,
#     a QB and the opposing receivers, is not modelled).
#   - sigma_wk is the one-week SD of the season mean (R/32); availability
#     risk is not in it (questionable players are excluded by default).
#
# USAGE
#   source(here::here("R", "69_dfs_simulation.R"))
#   build_sim_cache()                       # once
#   sim <- simulate_lineups(lus, n_draws = 10000)
#   rank_lineups(sim, objective = "p90")
# ==============================================================================

library(dplyr)
library(tibble)
library(glue)
library(here)
if (!exists("prepare_copula") || !exists("pair_correlation_matrix")) {
  source(here::here("R", "50_weekly_predictive_shape.R"))   # sources R/49 -> R/15, R/17
}

R69_OUTPUT_DIR  <- here::here("data", "season3_cache")
R69_CACHE_PATH  <- file.path(R69_OUTPUT_DIR, "s3_r69_sim_cache.rds")
R69_SCHEMA_TAG  <- "s3_r69_sim_v1"
R69_FIT_SEASONS <- c(2023L, 2024L)     # R/50's validated fit window
R69_DEF_SEASONS <- 2025L
R69_DEF_SIGMA_FALLBACK <- 4.0          # R/36's fallback, used only if DEF history fails
R69_SEED        <- 20261022L
R69_LABEL <- paste("Ranking of near-optimal lineups. Shape and copula built on 2023-2024;",
                   "not a calibrated distribution until R/70 has scored it.")

# ------------------------------------------------------------------------------
# Cache
# ------------------------------------------------------------------------------

.r69_def_sigma <- function(seasons = R69_DEF_SEASONS) {
  if (!exists("calculate_def_st_points")) source(here::here("R", "29_projection_engine.R"))
  d <- purrr::map_dfr(seasons, function(s) {
    pbp <- tryCatch(load_normalized_season(s), error = function(e) NULL)
    if (is.null(pbp)) return(tibble::tibble())
    calculate_def_st_points(pbp) %>% dplyr::select(season, team, def_st_points)
  })
  if (nrow(d) == 0) {
    message(glue("  sigma_def: no DEF history; fallback {R69_DEF_SIGMA_FALLBACK}"))
    return(list(sigma = R69_DEF_SIGMA_FALLBACK, n = 0L))
  }
  d <- d %>% dplyr::filter(is.finite(def_st_points)) %>%
    dplyr::group_by(season, team) %>%
    dplyr::mutate(r = def_st_points - mean(def_st_points)) %>% dplyr::ungroup()
  dof <- nrow(d) - dplyr::n_distinct(paste(d$season, d$team))
  sigma <- sqrt(sum(d$r^2) / dof)
  message(glue("  sigma_def (within team-season): {round(sigma, 3)} from {nrow(d)} team-games"))
  list(sigma = sigma, n = nrow(d))
}

#' Build the simulation cache (calibrated copula + DEF sigma). Run once.
#' @export
build_sim_cache <- function(fit_seasons = R69_FIT_SEASONS, k = 12) {
  if (!exists("DK_BEST_BALL_SCORING")) stop("R/69: DK_BEST_BALL_SCORING not defined (R/17).", call. = FALSE)
  if (!file.exists(SHAPE_CACHE_PATH)) stop("R/69: R/50 shape table not found at ", SHAPE_CACHE_PATH, call. = FALSE)
  st <- readRDS(SHAPE_CACHE_PATH)
  message(glue("\n{strrep('=', 70)}\nR/69: building simulation cache\n{strrep('=', 70)}"))
  message(glue("  shape table built {format(st$meta$built_at, '%Y-%m-%d')} on ",
               "{paste(st$meta$target_seasons, collapse = ',')}"))

  message("\nSTEP 1/4: weekly residuals (R/50, DK best-ball scoring)")
  resid <- build_weekly_residuals(fit_seasons, DK_BEST_BALL_SCORING, k = k)
  message(glue("  {nrow(resid)} player-weeks"))

  message("\nSTEP 2/4: pair correlations + R/50 cell policy")
  pc_tbl <- apply_correlation_policy(estimate_pair_correlations(resid))
  print(pc_tbl %>% dplyr::select(pos_a, pos_b, n_pairs, obs_cor_measured, obs_cor, neutralized))
  pc_target <- pair_correlation_matrix(pc_tbl)

  message("\nSTEP 3/4: calibrate Gaussian inputs (can take a few minutes)")
  set.seed(R69_SEED)
  pc_cal <- calibrate_copula_targets(pc_target, st)
  rec <- gate_copula_recovery(pc_cal, st)
  message("  Copula recovery check (target vs realized):")
  print(rec)

  message("\nSTEP 4/4: DEF sigma")
  ds <- .r69_def_sigma()

  obj <- list(schema_tag = R69_SCHEMA_TAG, built_at = Sys.time(),
              pc_target = pc_target, pc_calibrated = pc_cal, pair_cor = pc_tbl,
              recovery = rec, sigma_def = ds$sigma, sigma_def_n = ds$n,
              shape_built = st$meta$built_at,
              meta = list(fit_seasons = fit_seasons, k = k, scoring = "DK_BEST_BALL_SCORING",
                          def_seasons = R69_DEF_SEASONS))
  dir.create(R69_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  saveRDS(obj, R69_CACHE_PATH)
  message(glue("\n  Wrote {R69_CACHE_PATH}"))
  invisible(obj)
}

#' Load the simulation inputs (shape table + cache). NULL with a message if absent.
#' @export
load_sim_inputs <- function() {
  if (!file.exists(R69_CACHE_PATH)) {
    message("R/69: no simulation cache. Run build_sim_cache() once.")
    return(NULL)
  }
  cache <- readRDS(R69_CACHE_PATH)
  if (!identical(cache$schema_tag, R69_SCHEMA_TAG)) {
    message("R/69: simulation cache has the wrong schema. Rerun build_sim_cache().")
    return(NULL)
  }
  st <- readRDS(SHAPE_CACHE_PATH)
  if (!identical(st$meta$built_at, cache$shape_built)) {
    message("R/69: the R/50 shape table changed since the cache was built. Rerun build_sim_cache().")
    return(NULL)
  }
  list(shape_table = st, pc = cache$pc_calibrated, sigma_def = cache$sigma_def, cache = cache)
}

# ------------------------------------------------------------------------------
# Draws
# ------------------------------------------------------------------------------

# Vectorized form of R/50 shape_quantile() for ONE position (same rule: the
# stored quantile curve, linear interpolation, rule = 2; Normal when the entry
# is missing or marked normal). R/50's version loops element by element.
.r69_qz <- function(position, u, st) {
  ent <- st[[as.character(position)]]
  if (is.null(ent) || is.null(ent$quantiles) || identical(ent$method, "normal")) return(stats::qnorm(u))
  stats::approx(x = ent$probs, y = ent$quantiles, xout = u, rule = 2)$y
}

# One team's players: n_draws x length(positions) matrix of standardized z.
.r69_team_z <- function(positions, pc, st, n_draws) {
  prep <- prepare_copula(positions, pc)
  g <- matrix(stats::rnorm(n_draws * length(positions)), nrow = n_draws) %*% prep$L
  u <- stats::pnorm(g)
  z <- matrix(NA_real_, nrow = n_draws, ncol = length(positions))
  for (j in seq_along(positions)) z[, j] <- .r69_qz(positions[j], u[, j], st)
  z
}

#' Simulate candidate lineups with common random numbers
#'
#' @param lineups Tibble from solve_dfs_lineups() (lineup_id, total_proj, lineup).
#' @param inputs List from load_sim_inputs().
#' @param n_draws Integer.
#' @param thresholds Numeric vector; P(total >= t) is reported for each.
#' @param seed Integer.
#' @return Tibble one row per lineup: lineup_id, total_proj, mean_sim, sd_sim,
#'   p10, p50, p80, p90, p95, p_ge_<t>... ; attribute "draws" holds the
#'   n_draws x lineups matrix of simulated totals.
#' @export
simulate_lineups <- function(lineups, inputs = load_sim_inputs(), n_draws = 10000L,
                             thresholds = numeric(0), seed = R69_SEED) {
  if (is.null(inputs)) stop("R/69: simulation inputs unavailable (see message above).", call. = FALSE)
  if (nrow(lineups) == 0) stop("R/69: no lineups to simulate.", call. = FALSE)
  set.seed(seed)
  st <- inputs$shape_table; pc <- inputs$pc

  roster <- dplyr::bind_rows(lapply(lineups$lineup, function(lu)
    lu %>% dplyr::select(player_uid, position, team, proj, sigma_wk))) %>%
    dplyr::distinct(player_uid, .keep_all = TRUE)
  bad <- roster %>% dplyr::filter(position != "DEF", !(is.finite(sigma_wk) & sigma_wk > 0))
  if (nrow(bad) > 0) stop("R/69: players with no sigma_wk: ", paste(bad$player_uid, collapse = ", "), call. = FALSE)

  if (anyNA(roster$team)) stop("R/69: lineup player(s) with no team: ",
                               paste(roster$player_uid[is.na(roster$team)], collapse = ", "), call. = FALSE)
  pts <- matrix(NA_real_, nrow = n_draws, ncol = nrow(roster), dimnames = list(NULL, roster$player_uid))
  skill <- roster %>% dplyr::mutate(i = dplyr::row_number()) %>% dplyr::filter(position != "DEF")
  for (tm in sort(unique(skill$team))) {
    s <- skill %>% dplyr::filter(team == tm)
    z <- .r69_team_z(s$position, pc, st, n_draws)
    pts[, s$i] <- sweep(sweep(z, 2, s$sigma_wk, `*`), 2, s$proj, `+`)
  }
  dfi <- which(roster$position == "DEF")
  for (i in dfi) pts[, i] <- stats::rnorm(n_draws, roster$proj[i], inputs$sigma_def)

  tot <- vapply(lineups$lineup, function(lu) rowSums(pts[, lu$player_uid, drop = FALSE]),
                numeric(n_draws))
  if (is.null(dim(tot))) tot <- matrix(tot, ncol = 1)
  colnames(tot) <- lineups$lineup_id

  q <- function(p) apply(tot, 2, stats::quantile, probs = p, names = FALSE)
  out <- tibble::tibble(lineup_id = lineups$lineup_id, total_proj = lineups$total_proj,
                        mean_sim = colMeans(tot), sd_sim = apply(tot, 2, stats::sd),
                        p10 = q(0.10), p50 = q(0.50), p80 = q(0.80), p90 = q(0.90), p95 = q(0.95))
  for (t in thresholds) out[[paste0("p_ge_", t)]] <- colMeans(tot >= t)
  out <- out %>% dplyr::mutate(dplyr::across(-lineup_id, ~ round(.x, 3)))
  attr(out, "draws") <- tot
  attr(out, "label") <- R69_LABEL
  out
}

#' Order simulated lineups by an objective
#' @param objective "mean", "p50", "p80", "p90", "p95", or "p_ge" (needs threshold).
#' @export
rank_lineups <- function(sim, objective = c("mean", "p50", "p80", "p90", "p95", "p_ge"),
                         threshold = NULL) {
  objective <- match.arg(objective)
  col <- switch(objective, mean = "mean_sim", p_ge = paste0("p_ge_", threshold), objective)
  if (!col %in% names(sim)) stop("R/69: column ", col, " not in the simulation (pass that threshold).",
                                 call. = FALSE)
  sim %>% dplyr::arrange(dplyr::desc(.data[[col]])) %>%
    dplyr::mutate(sim_rank = dplyr::row_number(), objective = col, .before = 1)
}
