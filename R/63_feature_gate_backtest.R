# ==============================================================================
# 63_feature_gate_backtest.R
#
# Out-of-sample feature gate for the R/29 in-season YTD update.  [2026-09-30]
#
# WHAT IS BEING TESTED
#   Three candidate features, each gated against the CURRENT production update
#   rule (games-based EB weight at RB/WR/TE, linear calendar schedule at QB,
#   per s3_r58_ytd_k.rds):
#
#   C1  Expected fantasy points (nflreadr::load_ff_opportunity, weekly).
#       Scale: per-position linear map game_points ~ total_fantasy_points,
#       fitted on the training seasons, applied to total_fantasy_points_exp.
#       Update: observed mean = a * actual + (1 - a) * xFP over earlier games.
#       a in 0..1 by 0.1. RB/WR/TE fit (k, a) jointly on the R/58 k grid;
#       QB fits a only and keeps the linear schedule. a = 1 IS the baseline.
#
#   C2  Goal-line role share. Inside-10 and 11-to-20 shares of team carries
#       plus targets, in games the player played, earlier weeks only. Each
#       share is shrunk toward the position mean with beta-binomial strength
#       from the PRIOR season (method of moments, not tuned on the gate).
#       Shifter b1 * f_i10 + b2 * f_z20 per position, fitted by OLS on the
#       baseline's training-fold residuals.
#
#   C3  Blitz x protection, QB only. Opponent defense blitz rate (FTN
#       n_pass_rushers >= 5, per charted dropback) times the offense's
#       sack-plus-QB-hit rate on blitzed dropbacks. Both season-centered
#       (team rate minus league rate, weeks before W) and shrunk toward the
#       team's prior-season centered rate (method of moments strength).
#       One coefficient c on the product, fitted on training-fold residuals.
#
# DESIGN (forks locked 2026-09-30)
#   Frame   : mirrors R/58 .r58_player_weeks exactly (history-arm prior proxy
#             from R/49, YTD over earlier games only, weeks 2-18). Row counts
#             are printed so they can be compared with the R/58 run.
#   Baseline: production rule shape; k refit on each fold's training seasons.
#   Gate    : leave-one-season-out over 2023-2025, pooled paired errors,
#             player-cluster bootstrap (R/58 reps, level, seed). A gated
#             candidate passes at a position when the 90 percent lower bound
#             on RMSE improvement over the baseline is above zero.
#   Rows    : every candidate is scored on exactly the baseline's rows. A
#             feature that cannot be computed yet contributes 0; no row is
#             ever dropped for a candidate.
#   Leakage : every feature at week W uses only weeks before W. A sampled
#             recomputation (including week-2 rows) asserts this before any
#             fitting; the run stops if a single sampled row disagrees.
#
# DIAGNOSTICS (reported, NOT gated)
#   diag_c2_increment_over_c1 : C2 shifter refit on top of C1. WIRING RULE for
#     C2: it may be wired only if it passes its gate AND, at positions where C1
#     passed, this diagnostic's interval does not sit wholly below zero.
#   diag_c3_rb_checkdown      : the C3 shifter applied to RB rows.
#
# OUTPUTS (data/season3_cache/, schema s3_r63_feature_gate_v1)
#   s3_r63_feature_gate.rds          decisions, all-season parameters,
#                                    neutralization evidence, provenance
#   s3_r63_feature_gate_results.rds  paired predictions, per-fold fits,
#                                    by-week table, diagnostics
#   s3_r63_feature_gate_summary.csv  per-fold and pooled rows
#   [2026-10-01] Those are the DEFAULT (flat PPR) names. Under any other
#   scoring each name gets _<scoring key> before the extension (R/29
#   scoring_gate_path()), and the gate object records scoring_key, which R/29
#   load_r63_xfp_params() checks. The schema tag is unchanged: the object's
#   fields are a superset of v1.
#
# NOT IN SCOPE
#   Wiring into R/29 or R/32 (separate go). teammate_out join. Anything on the
#   rejected list (defense-allowed EPA by position, offense blitz gap, team
#   EPA/play, PA/screen/PROE rates).
#
# LIMITS (stated so they are not forgotten)
#   - Prior is R/58's history-arm proxy (no rookies, weaker than R/29's), so
#     fitted k is biased low in a known direction.
#   - QB is the smallest sample (about 1,200 pooled player-games).
#   - C2 share denominators are team zone plays in games the player played.
#   - ff_opportunity models are documented as trained on 2006-2020; the
#     version actually used and its coverage are recorded in provenance.
#
# USAGE
#   source(here::here("R", "63_feature_gate_backtest.R"))
#   res <- run_feature_gate_backtest()
#   res_fd <- run_feature_gate_backtest(scoring_settings = FANDUEL_SCORING)
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(glue)
library(readr)
library(nflreadr)
library(here)
source(here::here("R", "58_ytd_weight_backtest.R"))   # sources R/29 and R/49

R63_OUTPUT_DIR   <- here::here("data", "season3_cache")
R63_GATE_PATH    <- file.path(R63_OUTPUT_DIR, "s3_r63_feature_gate.rds")
R63_RESULTS_PATH <- file.path(R63_OUTPUT_DIR, "s3_r63_feature_gate_results.rds")
R63_SUMMARY_CSV  <- file.path(R63_OUTPUT_DIR, "s3_r63_feature_gate_summary.csv")
R63_SCHEMA_TAG   <- "s3_r63_feature_gate_v1"

R63_TARGET_SEASONS  <- 2023:2025
R63_A_GRID          <- seq(0, 1, by = 0.1)
R63_MAX_WEEK        <- NFL_REGULAR_SEASON_MAX_WEEK
R63_EP_PINNED       <- "v1.0.0"
R63_EP_DOC_TRAINING <- "2006-2020 (ffopportunity README, checked 2026-09-30)"
R63_EP_MIN_COVERAGE <- 0.99
R63_MIN_TEAM_OPPS_MOM <- 10L          # player-season zone denominator for MoM
R63_PHI_BOUNDS      <- c(1, 500)      # beta-binomial concentration, C2
R63_M_BOUNDS        <- c(1, 2000)     # team-rate shrinkage strength, C3
R63_LEAK_ROWS       <- 50L
R63_LEAK_WEEK2_ROWS <- 10L

R63_TESTS <- list(
  list(test = "c1_xfp",                    base = "pred_base", cand = "pred_c1",
       positions = R58_POSITIONS, gated = TRUE),
  list(test = "c2_role_share",             base = "pred_base", cand = "pred_c2",
       positions = R58_POSITIONS, gated = TRUE),
  list(test = "c3_blitz_protection",       base = "pred_base", cand = "pred_c3",
       positions = "QB",          gated = TRUE),
  list(test = "diag_c2_increment_over_c1", base = "pred_c1",   cand = "pred_c2_on_c1",
       positions = R58_POSITIONS, gated = FALSE),
  list(test = "diag_c3_rb_checkdown",      base = "pred_base", cand = "pred_c3",
       positions = "RB",          gated = FALSE)
)

R63_REVIVE <- c(
  c1_xfp = paste("Revive when a re-run of R/63 on an added season passes the",
                 "pooled LOSO gate at this position, or when a stronger prior",
                 "than the R/49 proxy is available for the backtest."),
  c2_role_share = paste("Revive when a re-run passes the gate at this position",
                        "and the increment over C1 is not wholly below zero."),
  c3_blitz_protection = paste("Revive when a re-run with a true pressure field",
                              "(not sack plus hit) passes the QB gate.")
)

# ------------------------------------------------------------------------------
# Small helpers
# ------------------------------------------------------------------------------
.r63_rmse  <- function(y, p) sqrt(mean((y - p)^2))
.r63_clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

.r63_opponent <- function(game_id, team) {
  sp   <- strsplit(as.character(game_id), "_", fixed = TRUE)
  away <- vapply(sp, function(p) if (length(p) >= 4L) p[3] else NA_character_, character(1))
  home <- vapply(sp, function(p) if (length(p) >= 4L) p[4] else NA_character_, character(1))
  dplyr::case_when(!is.na(team) & team == home ~ away,
                   !is.na(team) & team == away ~ home,
                   TRUE ~ NA_character_)
}

.r63_ols0 <- function(r, X) {
  X   <- as.matrix(X)
  out <- stats::setNames(rep(0, ncol(X)), colnames(X))
  keep <- colSums(X^2) > 0
  if (any(keep)) {
    b <- stats::lm.fit(X[, keep, drop = FALSE], r)$coefficients
    b[is.na(b)] <- 0
    out[keep] <- b
  }
  out
}

# ------------------------------------------------------------------------------
# STEP 1: ff_opportunity weekly, pinned version preferred
# ------------------------------------------------------------------------------
.r63_load_ep <- function(seasons) {
  prep <- function(x) {
    need <- c("season", "week", "game_id", "player_id", "position", "posteam",
              "total_fantasy_points", "total_fantasy_points_exp")
    miss <- setdiff(need, names(x))
    if (length(miss) > 0) stop("R/63: ff_opportunity missing: ",
                               paste(miss, collapse = ", "), call. = FALSE)
    x %>%
      dplyr::mutate(season = as.integer(season), week = as.integer(week)) %>%
      dplyr::filter(position %in% R58_POSITIONS, week <= R63_MAX_WEEK) %>%
      dplyr::select(dplyr::all_of(need)) %>%
      dplyr::distinct(season, player_id, week, .keep_all = TRUE)
  }
  latest <- prep(nflreadr::load_ff_opportunity(seasons = seasons, stat_type = "weekly",
                                               model_version = "latest"))
  pinned <- tryCatch(
    prep(nflreadr::load_ff_opportunity(seasons = seasons, stat_type = "weekly",
                                       model_version = R63_EP_PINNED)),
    error = function(e) {
      message(glue("  {R63_EP_PINNED} unavailable: {conditionMessage(e)}"))
      NULL
    })

  cov_tbl <- latest %>% dplyr::count(season, name = "n_latest")
  coverage <- 0
  max_diff <- NA_real_
  if (!is.null(pinned)) {
    cov_tbl <- cov_tbl %>%
      dplyr::left_join(pinned %>% dplyr::count(season, name = "n_pinned"),
                       by = "season", relationship = "one-to-one") %>%
      dplyr::mutate(n_pinned = dplyr::coalesce(n_pinned, 0L),
                    share = n_pinned / n_latest)
    coverage <- min(cov_tbl$share)
    same <- latest %>%
      dplyr::inner_join(pinned %>% dplyr::select(season, player_id, week,
                                                 xp = total_fantasy_points_exp),
                        by = c("season", "player_id", "week"),
                        relationship = "one-to-one")
    if (nrow(same) > 0)
      max_diff <- max(abs(same$total_fantasy_points_exp - same$xp), na.rm = TRUE)
  }
  use_pinned <- !is.null(pinned) && coverage >= R63_EP_MIN_COVERAGE
  message("  ff_opportunity coverage by season:")
  print(cov_tbl)
  message(glue("  Using model_version = ",
               "{if (use_pinned) R63_EP_PINNED else 'latest'}; pinned coverage ",
               "{round(coverage, 4)}; max |pinned - latest| xFP = ",
               "{if (is.na(max_diff)) 'NA' else round(max_diff, 4)}"))
  list(ep = if (use_pinned) pinned else latest,
       model_version = if (use_pinned) R63_EP_PINNED else "latest",
       coverage = coverage, max_abs_diff_vs_latest = max_diff)
}

# ------------------------------------------------------------------------------
# STEP 2: per-season pbp + FTN tables (zone opportunities, blitz, protection)
# ------------------------------------------------------------------------------
.r63_season_tables <- function(s) {
  pbp  <- nflreadr::load_pbp(s)
  need <- c("season_type", "week", "game_id", "play_id", "posteam", "defteam",
            "play_type", "yardline_100", "rusher_player_id", "receiver_player_id",
            "qb_dropback", "sack", "qb_hit", "two_point_attempt", "qb_kneel")
  miss <- setdiff(need, names(pbp))
  if (length(miss) > 0) stop("R/63: pbp ", s, " missing: ",
                             paste(miss, collapse = ", "), call. = FALSE)
  pbp <- pbp %>%
    dplyr::select(dplyr::all_of(need)) %>%
    dplyr::filter(season_type == "REG", !is.na(posteam), week <= R63_MAX_WEEK) %>%
    dplyr::mutate(week   = as.integer(week),
                  two_pt = dplyr::coalesce(as.integer(two_point_attempt), 0L),
                  kneel  = dplyr::coalesce(as.integer(qb_kneel), 0L))

  # C2: zone opportunities = non-kneel rushes + targeted passes, inside the 20
  zone <- pbp %>%
    dplyr::filter(two_pt == 0L, !is.na(yardline_100), yardline_100 <= 20) %>%
    dplyr::mutate(
      zone = dplyr::if_else(yardline_100 <= 10, "i10", "z20"),
      opp_player = dplyr::case_when(
        play_type == "run"  & kneel == 0L ~ rusher_player_id,
        play_type == "pass"               ~ receiver_player_id,
        TRUE                              ~ NA_character_)
    ) %>%
    dplyr::filter(!is.na(opp_player))

  widen <- function(df, key, prefix) {
    out <- df %>%
      tidyr::pivot_wider(names_from = zone, values_from = n, values_fill = 0L,
                         names_prefix = prefix)
    for (z in c("i10", "z20")) {
      cn <- paste0(prefix, z)
      if (!cn %in% names(out)) out[[cn]] <- 0L
    }
    out
  }
  team_zone <- zone %>% dplyr::count(posteam, week, zone) %>% widen("posteam", "team_")
  ply_zone  <- zone %>% dplyr::count(player_id = opp_player, week, zone) %>%
    widen("player_id", "ply_")

  # C3: charted dropbacks with a rusher count
  ftn <- nflreadr::load_ftn_charting(s) %>%
    dplyr::transmute(key = paste(nflverse_game_id, as.integer(nflverse_play_id)),
                     n_pass_rushers, date_pulled)
  ftn_pulled <- as.character(max(ftn$date_pulled, na.rm = TRUE))

  db <- pbp %>%
    dplyr::filter(dplyr::coalesce(qb_dropback, 0) == 1, two_pt == 0L, !is.na(defteam)) %>%
    dplyr::mutate(key = paste(game_id, as.integer(play_id))) %>%
    dplyr::left_join(ftn %>% dplyr::select(key, n_pass_rushers), by = "key",
                     relationship = "many-to-one") %>%
    dplyr::filter(dplyr::coalesce(n_pass_rushers, 0L) > 0) %>%
    dplyr::mutate(blitz    = n_pass_rushers >= 5,
                  sack_hit = dplyr::coalesce(sack, 0) == 1 | dplyr::coalesce(qb_hit, 0) == 1)

  def_wk <- db %>%
    dplyr::group_by(team = defteam, week) %>%
    dplyr::summarise(n = dplyr::n(), x = sum(blitz), .groups = "drop")
  off_wk <- db %>%
    dplyr::filter(blitz) %>%
    dplyr::group_by(team = posteam, week) %>%
    dplyr::summarise(n = dplyr::n(), x = sum(sack_hit), .groups = "drop")

  message(glue("    {s}: {nrow(zone)} zone opportunities, {nrow(db)} charted dropbacks, ",
               "{sum(db$blitz)} blitzed; FTN pulled {ftn_pulled}"))
  out <- list(team_zone = team_zone, ply_zone = ply_zone, def_wk = def_wk,
              off_wk = off_wk, ftn_pulled = ftn_pulled)
  rm(pbp, zone, ftn, db); invisible(gc(verbose = FALSE))
  out
}

# ------------------------------------------------------------------------------
# Priors from the PRIOR season (never the target season)
# ------------------------------------------------------------------------------
.r63_share_prior <- function(ep_prev, tab_prev) {
  ps <- ep_prev %>%
    dplyr::select(player_id, position, week, posteam) %>%
    dplyr::left_join(tab_prev$team_zone, by = c("posteam", "week"),
                     relationship = "many-to-one") %>%
    dplyr::left_join(tab_prev$ply_zone, by = c("player_id", "week"),
                     relationship = "many-to-one") %>%
    dplyr::mutate(dplyr::across(c(team_i10, team_z20, ply_i10, ply_z20),
                                ~ dplyr::coalesce(as.numeric(.x), 0))) %>%
    dplyr::group_by(player_id, position) %>%
    dplyr::summarise(dplyr::across(c(team_i10, team_z20, ply_i10, ply_z20), sum),
                     .groups = "drop")

  purrr::map_dfr(R58_POSITIONS, function(pos) {
    purrr::map_dfr(c("i10", "z20"), function(z) {
      d <- ps %>% dplyr::filter(position == pos)
      n <- d[[paste0("team_", z)]]; x <- d[[paste0("ply_", z)]]
      keep <- n >= R63_MIN_TEAM_OPPS_MOM
      n <- n[keep]; x <- x[keep]
      mu <- if (sum(n) > 0) sum(x) / sum(n) else 0
      vb <- if (length(n) > 1) stats::var(x / n) - mean(mu * (1 - mu) / n) else NA_real_
      phi <- if (is.na(vb) || vb <= 0 || mu <= 0 || mu >= 1) R63_PHI_BOUNDS[2]
             else .r63_clamp(mu * (1 - mu) / vb - 1, R63_PHI_BOUNDS[1], R63_PHI_BOUNDS[2])
      tibble::tibble(position = pos, zone = z, mu = mu, phi = phi, n_players = length(n))
    })
  })
}

.r63_team_prior <- function(wk_prev) {
  t <- wk_prev %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(n = sum(n), x = sum(x), .groups = "drop") %>%
    dplyr::filter(n > 0)
  L  <- sum(t$x) / sum(t$n)
  vb <- if (nrow(t) > 1) stats::var(t$x / t$n) - mean(L * (1 - L) / t$n) else NA_real_
  m  <- if (is.na(vb) || vb <= 0) R63_M_BOUNDS[2]
        else .r63_clamp(L * (1 - L) / vb - 1, R63_M_BOUNDS[1], R63_M_BOUNDS[2])
  list(dev = t %>% dplyr::transmute(team, dev_prev = x / n - L), league_prev = L, m = m)
}

# Team rate before week W, centered on the league rate before week W, shrunk
# toward (league before W + team's prior-season centered deviation).
.r63_team_cum <- function(wk, prior) {
  tidyr::expand_grid(team = sort(unique(c(wk$team, prior$dev$team))),
                     week = seq_len(R63_MAX_WEEK)) %>%
    dplyr::left_join(wk, by = c("team", "week"), relationship = "one-to-one") %>%
    dplyr::mutate(n = dplyr::coalesce(as.numeric(n), 0), x = dplyr::coalesce(as.numeric(x), 0)) %>%
    dplyr::arrange(team, week) %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(n_prev = cumsum(n) - n, x_prev = cumsum(x) - x) %>%
    dplyr::group_by(week) %>%
    dplyr::mutate(league = dplyr::if_else(sum(n_prev) > 0, sum(x_prev) / sum(n_prev),
                                          prior$league_prev)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(prior$dev, by = "team", relationship = "many-to-one") %>%
    dplyr::mutate(dev_prev   = dplyr::coalesce(dev_prev, 0),
                  prior_rate = .r63_clamp(league + dev_prev, 0.001, 0.999),
                  shrunk     = (x_prev + prior$m * prior_rate) / (n_prev + prior$m),
                  centered   = shrunk - league) %>%
    dplyr::select(team, week, n_prev, x_prev, league, centered)
}

# ------------------------------------------------------------------------------
# STEP 4: frame for one target season (mirrors .r58_player_weeks, plus features)
# ------------------------------------------------------------------------------
.r63_build_frame <- function(ts, gp, ep, tab, share_prior, def_cum, off_cum) {
  hist <- dplyr::bind_rows(gp[as.character((ts - 3L):(ts - 1L))])
  tgt  <- gp[[as.character(ts)]]
  if (is.null(tgt) || nrow(hist) == 0) stop("R/63: missing game points for ", ts, call. = FALSE)
  hist_est <- .build_history_estimates(hist, target_season = ts)

  ep_s <- ep %>%
    dplyr::filter(season == ts) %>%
    dplyr::select(player_id, week, ep_team = posteam,
                  fp_ep = total_fantasy_points, xfp_raw = total_fantasy_points_exp)
  sp_wide <- share_prior %>%
    dplyr::select(position, zone, mu, phi) %>%
    tidyr::pivot_wider(names_from = zone, values_from = c(mu, phi))

  base <- tgt %>%
    dplyr::mutate(week = as.integer(week)) %>%
    dplyr::filter(position %in% R58_POSITIONS, !is.na(game_points)) %>%
    dplyr::inner_join(hist_est %>% dplyr::select(player_id, position, prior_mu = pred_mu),
                      by = c("player_id", "position"))
  n0 <- nrow(base)

  full <- base %>%
    dplyr::left_join(ep_s, by = c("player_id", "week"), relationship = "many-to-one") %>%
    dplyr::left_join(tab$team_zone, by = c("ep_team" = "posteam", "week"),
                     relationship = "many-to-one") %>%
    dplyr::left_join(tab$ply_zone, by = c("player_id", "week"), relationship = "many-to-one") %>%
    dplyr::mutate(dplyr::across(c(team_i10, team_z20, ply_i10, ply_z20),
                                ~ dplyr::coalesce(as.numeric(.x), 0)),
                  xfp_ok = !is.na(xfp_raw))
  stopifnot(nrow(full) == n0)

  full <- full %>%
    dplyr::arrange(player_id, week, game_id) %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(
      ytd_n    = dplyr::row_number() - 1L,
      ytd_sum  = cumsum(game_points) - game_points,
      ytd_mean = dplyr::if_else(ytd_n > 0L, ytd_sum / ytd_n, NA_real_),
      ytd_xfp_n   = cumsum(xfp_ok) - xfp_ok,
      ytd_xfp_sum = cumsum(dplyr::coalesce(xfp_raw, 0)) - dplyr::coalesce(xfp_raw, 0),
      ytd_xfp_raw_mean = dplyr::if_else(ytd_xfp_n > 0L, ytd_xfp_sum / ytd_xfp_n, NA_real_),
      cum_team_i10 = cumsum(team_i10) - team_i10,
      cum_team_z20 = cumsum(team_z20) - team_z20,
      cum_ply_i10  = cumsum(ply_i10) - ply_i10,
      cum_ply_z20  = cumsum(ply_z20) - ply_z20
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(target = ts)

  frame <- full %>%
    dplyr::filter(ytd_n >= 1L, week >= 2L, week <= R63_MAX_WEEK) %>%
    dplyr::left_join(sp_wide, by = "position", relationship = "many-to-one") %>%
    dplyr::mutate(
      f_i10 = (cum_ply_i10 + phi_i10 * mu_i10) / (cum_team_i10 + phi_i10) - mu_i10,
      f_z20 = (cum_ply_z20 + phi_z20 * mu_z20) / (cum_team_z20 + phi_z20) - mu_z20,
      opp   = .r63_opponent(game_id, ep_team)
    ) %>%
    dplyr::left_join(def_cum %>% dplyr::select(team, week, def_blitz_c = centered),
                     by = c("opp" = "team", "week"), relationship = "many-to-one") %>%
    dplyr::left_join(off_cum %>% dplyr::select(team, week, off_prot_c = centered),
                     by = c("ep_team" = "team", "week"), relationship = "many-to-one") %>%
    dplyr::mutate(
      x3    = dplyr::coalesce(def_blitz_c * off_prot_c, 0),
      f_i10 = dplyr::coalesce(f_i10, 0),
      f_z20 = dplyr::coalesce(f_z20, 0),
      w_lin = vapply(week, function(w) 1 - compute_prior_weight(w), numeric(1))
    )

  message(glue("    target {ts}: {nrow(frame)} rows; team NA {sum(is.na(frame$ep_team))}, ",
               "xFP history NA {sum(frame$ytd_xfp_n == 0L)}, ",
               "opponent NA {sum(is.na(frame$opp))}, ",
               "C3 inputs NA {sum(is.na(frame$def_blitz_c) | is.na(frame$off_prot_c))}"))

  list(frame = frame,
       full  = full %>% dplyr::select(target, player_id, week, game_points, xfp_ok,
                                      team_i10, team_z20, ply_i10, ply_z20))
}

# ------------------------------------------------------------------------------
# STEP 5: leakage assertion (recompute sampled rows from week < W only)
# ------------------------------------------------------------------------------
.r63_leak_check <- function(frame, fulls, cums, tabs) {
  set.seed(R58_SEED)
  wk2  <- which(frame$week == 2L)
  pick <- unique(c(sample(wk2, min(R63_LEAK_WEEK2_ROWS, length(wk2))),
                   sample(seq_len(nrow(frame)), min(R63_LEAK_ROWS, nrow(frame)))))
  bad <- character(0)
  for (i in pick) {
    r <- frame[i, ]
    f <- fulls[[as.character(r$target)]] %>%
      dplyr::filter(player_id == r$player_id, week < r$week)
    ok <- nrow(f) == r$ytd_n &&
      isTRUE(all.equal(mean(f$game_points), r$ytd_mean)) &&
      sum(f$xfp_ok) == r$ytd_xfp_n &&
      sum(f$team_i10) == r$cum_team_i10 && sum(f$team_z20) == r$cum_team_z20 &&
      sum(f$ply_i10)  == r$cum_ply_i10  && sum(f$ply_z20)  == r$cum_ply_z20
    if (!ok) bad <- c(bad, glue("player {r$player_id} target {r$target} week {r$week}"))
  }
  for (ts in names(cums)) {
    for (side in c("def", "off")) {
      cum <- cums[[ts]][[side]]
      wk  <- tabs[[ts]][[paste0(side, "_wk")]]
      samp <- cum[sample(seq_len(nrow(cum)), min(30L, nrow(cum))), ]
      for (j in seq_len(nrow(samp))) {
        ref <- wk %>% dplyr::filter(team == samp$team[j], week < samp$week[j])
        if (sum(ref$n) != samp$n_prev[j] || sum(ref$x) != samp$x_prev[j])
          bad <- c(bad, glue("{side} {samp$team[j]} {ts} week {samp$week[j]}"))
      }
    }
  }
  list(n_rows_checked = length(pick), n_week2_checked = sum(frame$week[pick] == 2L),
       failures = bad)
}

# ------------------------------------------------------------------------------
# Fit and predict (one position, one training set)
# ------------------------------------------------------------------------------
.r63_w_obs <- function(df, pos, k) {
  if (pos == "QB" || is.na(k)) df$w_lin else df$ytd_n / (df$ytd_n + k)
}
.r63_xfp_mean <- function(df, map) {
  dplyr::if_else(df$ytd_xfp_n > 0L,
                 map[["a0"]] + map[["b1"]] * df$ytd_xfp_raw_mean,
                 df$ytd_mean)
}
.r63_pred_c1 <- function(df, pos, k, a, map) {
  w   <- .r63_w_obs(df, pos, k)
  obs <- a * df$ytd_mean + (1 - a) * .r63_xfp_mean(df, map)
  (1 - w) * df$prior_mu + w * obs
}

.r63_fit <- function(tr, pos) {
  y <- tr$game_points
  k0 <- NA_real_
  if (pos != "QB") {
    rm0 <- vapply(R58_K_GRID, function(k) {
      w <- tr$ytd_n / (tr$ytd_n + k)
      .r63_rmse(y, (1 - w) * tr$prior_mu + w * tr$ytd_mean)
    }, numeric(1))
    k0 <- R58_K_GRID[which.min(rm0)]
  }
  w0   <- .r63_w_obs(tr, pos, k0)
  base <- (1 - w0) * tr$prior_mu + w0 * tr$ytd_mean

  okm <- !is.na(tr$fp_ep)
  cf  <- stats::coef(stats::lm(game_points ~ fp_ep, data = tr[okm, , drop = FALSE]))
  map <- c(a0 = unname(cf[1]), b1 = unname(cf[2]))

  # a descending so an exact tie resolves toward a = 1 (the baseline)
  grid <- expand.grid(a = rev(R63_A_GRID),
                      k = if (pos == "QB") NA_real_ else R58_K_GRID)
  rm1 <- vapply(seq_len(nrow(grid)), function(g) {
    .r63_rmse(y, .r63_pred_c1(tr, pos, grid$k[g], grid$a[g], map))
  }, numeric(1))
  k1 <- grid$k[which.min(rm1)]
  a1 <- grid$a[which.min(rm1)]
  p1 <- .r63_pred_c1(tr, pos, k1, a1, map)

  X2 <- tr[, c("f_i10", "f_z20")]
  list(position = pos, n_train = nrow(tr), k0 = k0, map = map, k1 = k1, a1 = a1,
       b2   = .r63_ols0(y - base, X2),
       b2c1 = .r63_ols0(y - p1, X2),
       c3   = unname(.r63_ols0(y - base, tr[, "x3", drop = FALSE])[1]))
}

.r63_predict <- function(df, pos, fit) {
  w0   <- .r63_w_obs(df, pos, fit$k0)
  base <- (1 - w0) * df$prior_mu + w0 * df$ytd_mean
  p1   <- .r63_pred_c1(df, pos, fit$k1, fit$a1, fit$map)
  df %>%
    dplyr::mutate(
      pred_prior    = prior_mu,
      pred_base     = base,
      pred_c1       = p1,
      pred_c2       = base + fit$b2[["f_i10"]] * f_i10 + fit$b2[["f_z20"]] * f_z20,
      pred_c2_on_c1 = p1 + fit$b2c1[["f_i10"]] * f_i10 + fit$b2c1[["f_z20"]] * f_z20,
      pred_c3       = base + fit$c3 * x3,
      k0 = fit$k0, k1 = fit$k1, a1 = fit$a1
    )
}

.r63_fit_row <- function(fit, scope) {
  tibble::tibble(scope = scope, position = fit$position, n_train = fit$n_train,
                 k0 = fit$k0, map_a0 = fit$map[["a0"]], map_b1 = fit$map[["b1"]],
                 k1 = fit$k1, a1 = fit$a1,
                 b2_i10 = fit$b2[["f_i10"]], b2_z20 = fit$b2[["f_z20"]],
                 b2c1_i10 = fit$b2c1[["f_i10"]], b2c1_z20 = fit$b2c1[["f_z20"]],
                 c3 = fit$c3)
}

# ------------------------------------------------------------------------------
# Gate: player-cluster bootstrap of RMSE(base) - RMSE(candidate)
# ------------------------------------------------------------------------------
.r63_boot <- function(df, base_col, cand_col, reps = R58_BOOT_REPS,
                      level = R58_BOOT_LEVEL, seed = R58_SEED) {
  set.seed(seed)
  y <- df$game_points; b <- df[[base_col]]; cc <- df[[cand_col]]
  by_player <- split(seq_len(nrow(df)), df$player_id)
  players   <- names(by_player)
  stat <- function(idx) sqrt(mean((y[idx] - b[idx])^2)) - sqrt(mean((y[idx] - cc[idx])^2))
  point <- stat(seq_len(nrow(df)))
  draws <- vapply(seq_len(reps), function(i) {
    stat(unlist(by_player[sample(players, length(players), replace = TRUE)],
                use.names = FALSE))
  }, numeric(1))
  a <- (1 - level) / 2
  c(improvement = point,
    lo = unname(stats::quantile(draws, a)),
    hi = unname(stats::quantile(draws, 1 - a)))
}

.r63_summarise <- function(rows, scope) {
  purrr::map_dfr(R63_TESTS, function(t) {
    purrr::map_dfr(t$positions, function(pos) {
      d <- rows %>% dplyr::filter(position == pos)
      if (nrow(d) < 50) return(NULL)
      b <- .r63_boot(d, t$base, t$cand)
      tibble::tibble(
        scope = scope, test = t$test, gated = t$gated, position = pos,
        n = nrow(d), n_players = dplyr::n_distinct(d$player_id),
        rmse_base = .r63_rmse(d$game_points, d[[t$base]]),
        rmse_cand = .r63_rmse(d$game_points, d[[t$cand]]),
        improvement = b[["improvement"]], lo = b[["lo"]], hi = b[["hi"]],
        passed = t$gated && is.finite(b[["lo"]]) && b[["lo"]] > 0
      )
    })
  })
}

# ------------------------------------------------------------------------------
# run_feature_gate_backtest
# ------------------------------------------------------------------------------
run_feature_gate_backtest <- function(target_seasons = R63_TARGET_SEASONS,
                                      scoring_settings = DEFAULT_SCORING_SETTINGS,
                                      write_outputs = TRUE) {

  target_seasons <- sort(unique(as.integer(target_seasons)))
  if (length(target_seasons) < 2) stop("R/63: need at least two target seasons.", call. = FALSE)
  feat_seasons <- (min(target_seasons) - 1L):max(target_seasons)
  gp_seasons   <- (min(target_seasons) - 3L):max(target_seasons)

  # [2026-10-01] Outputs keyed on scoring (default flat PPR keeps the original
  # names), so a per-site refit never overwrites the production gate.
  scoring_key  <- .r29_scoring_key(scoring_settings)
  is_default   <- .r29_is_default_scoring(scoring_settings)
  gate_path    <- scoring_gate_path(R63_GATE_PATH, scoring_settings)
  results_path <- scoring_gate_path(R63_RESULTS_PATH, scoring_settings)
  summary_csv  <- scoring_gate_path(R63_SUMMARY_CSV, scoring_settings)

  message(glue("\n{strrep('=', 70)}"))
  message("R/63: feature gate for the R/29 YTD update (C1 xFP, C2 role share, C3 blitz x protection)")
  message(glue("Leave-one-season-out over: {paste(target_seasons, collapse = ', ')}"))
  message(glue("Scoring key {scoring_key}{if (is_default) ' (default flat PPR)' else ''}; ",
               "gate -> {basename(gate_path)}"))
  message(glue("{strrep('=', 70)}\n"))

  message("STEP 1/7: ff_opportunity weekly")
  ep_obj <- .r63_load_ep(feat_seasons)
  ep     <- ep_obj$ep

  message("\nSTEP 2/7: pbp + FTN tables")
  tabs <- stats::setNames(lapply(feat_seasons, .r63_season_tables), feat_seasons)

  message("\nSTEP 3/7: game points (league scoring)")
  gp <- stats::setNames(lapply(gp_seasons, function(s) {
    .season_game_points(s, scoring_settings, cache_dir = GATE_CACHE_DIR)
  }), gp_seasons)

  message("\nSTEP 4/7: priors from the prior season, then frames")
  share_priors <- list(); cums <- list(); frames <- list(); fulls <- list()
  for (ts in target_seasons) {
    k <- as.character(ts); kp <- as.character(ts - 1L)
    share_priors[[k]] <- .r63_share_prior(ep %>% dplyr::filter(season == ts - 1L), tabs[[kp]]) %>%
      dplyr::mutate(target = ts)
    cums[[k]] <- list(
      def = .r63_team_cum(tabs[[k]]$def_wk, .r63_team_prior(tabs[[kp]]$def_wk)),
      off = .r63_team_cum(tabs[[k]]$off_wk, .r63_team_prior(tabs[[kp]]$off_wk))
    )
    fr <- .r63_build_frame(ts, gp, ep, tabs[[k]], share_priors[[k]],
                           cums[[k]]$def, cums[[k]]$off)
    frames[[k]] <- fr$frame; fulls[[k]] <- fr$full
  }
  frame <- dplyr::bind_rows(frames)
  message("\n  Rows by target x position (compare with the R/58 run):")
  print(frame %>% dplyr::count(target, position) %>%
          tidyr::pivot_wider(names_from = position, values_from = n))
  message("\n  C2 share priors (from the prior season):")
  print(dplyr::bind_rows(share_priors) %>%
          dplyr::mutate(dplyr::across(c(mu, phi), ~ round(.x, 3))), n = 40)

  message("\nSTEP 5/7: leakage assertion")
  leak <- .r63_leak_check(frame, fulls, cums, tabs)
  message(glue("  Checked {leak$n_rows_checked} player rows ({leak$n_week2_checked} at week 2) ",
               "plus team-week rates: {length(leak$failures)} failure(s)"))
  if (length(leak$failures) > 0) {
    print(utils::head(leak$failures, 20))
    stop("R/63: leakage assertion failed. No fitting was run.", call. = FALSE)
  }

  message("\nSTEP 6/7: leave-one-season-out folds")
  fold_fits <- list()
  folds <- purrr::map_dfr(target_seasons, function(h) {
    tr_seasons <- setdiff(target_seasons, h)
    message(glue("  holdout {h}, train {paste(tr_seasons, collapse = '+')}"))
    purrr::map_dfr(R58_POSITIONS, function(pos) {
      tr  <- frame %>% dplyr::filter(target %in% tr_seasons, position == pos)
      ho  <- frame %>% dplyr::filter(target == h, position == pos)
      fit <- .r63_fit(tr, pos)
      fold_fits[[paste(h, pos)]] <<- .r63_fit_row(fit, as.character(h))
      .r63_predict(ho, pos, fit)
    })
  })
  fold_params <- dplyr::bind_rows(fold_fits)
  message("\n  Per-fold fitted parameters:")
  print(fold_params %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))),
        n = 20, width = Inf)

  per_fold <- purrr::map_dfr(target_seasons, function(h) {
    .r63_summarise(folds %>% dplyr::filter(target == h), as.character(h))
  })
  pooled <- .r63_summarise(folds, "pooled_loso")
  message("\n  Pooled LOSO results:")
  print(pooled %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))),
        n = 20, width = Inf)
  message("\n  Per-holdout results:")
  print(per_fold %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))),
        n = 60, width = Inf)

  by_week <- folds %>%
    dplyr::group_by(position, week) %>%
    dplyr::summarise(n = dplyr::n(),
                     rmse_base = .r63_rmse(game_points, pred_base),
                     rmse_c1   = .r63_rmse(game_points, pred_c1),
                     rmse_c2   = .r63_rmse(game_points, pred_c2),
                     rmse_c3   = .r63_rmse(game_points, pred_c3),
                     .groups = "drop")

  message("\nSTEP 7/7: decisions, all-season parameters, outputs")
  all_fits <- stats::setNames(lapply(R58_POSITIONS, function(pos) {
    .r63_fit(frame %>% dplyr::filter(position == pos), pos)
  }), R58_POSITIONS)
  all_params <- dplyr::bind_rows(lapply(all_fits, .r63_fit_row, scope = "all_seasons"))

  c1_pass <- pooled %>% dplyr::filter(test == "c1_xfp") %>% dplyr::select(position, c1_passed = passed)
  incr    <- pooled %>% dplyr::filter(test == "diag_c2_increment_over_c1") %>%
    dplyr::select(position, incr_hi = hi)

  decisions <- pooled %>%
    dplyr::filter(gated) %>%
    dplyr::left_join(c1_pass, by = "position", relationship = "many-to-one") %>%
    dplyr::left_join(incr, by = "position", relationship = "many-to-one") %>%
    dplyr::mutate(
      wiring_eligible = dplyr::case_when(
        test == "c2_role_share" ~ passed & (!dplyr::coalesce(c1_passed, FALSE) |
                                              dplyr::coalesce(incr_hi, -Inf) >= 0),
        TRUE ~ passed),
      weight_active = wiring_eligible,
      neutral_value = dplyr::case_when(
        test == "c1_xfp"              ~ "a = 1 (actual points only; baseline k)",
        test == "c2_role_share"       ~ "b1 = b2 = 0",
        test == "c3_blitz_protection" ~ "c = 0"),
      revive_condition = unname(R63_REVIVE[test]),
      evidence = glue::glue("pooled LOSO improvement {round(improvement, 3)} ",
                            "[{round(lo, 3)}, {round(hi, 3)}], n = {n}")
    ) %>%
    dplyr::select(-c1_passed, -incr_hi)

  message("\n  Decisions:")
  print(decisions %>% dplyr::select(test, position, improvement, lo, hi, passed,
                                     wiring_eligible) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))), n = 20)

  gate_obj <- list(
    schema_tag     = R63_SCHEMA_TAG,
    built_at       = Sys.time(),
    scoring_key    = scoring_key,
    target_seasons = target_seasons,
    method         = "leave-one-season-out, pooled player-cluster bootstrap, 90% lower bound > 0 vs production rule",
    decisions      = decisions,
    params_all_seasons = all_params,
    fits_all_seasons   = all_fits,
    pooled         = pooled,
    per_fold       = per_fold,
    leak_check     = leak,
    provenance = list(
      nflreadr_version        = as.character(utils::packageVersion("nflreadr")),
      ep_model_version        = ep_obj$model_version,
      ep_pinned_coverage      = ep_obj$coverage,
      ep_max_abs_diff_latest  = ep_obj$max_abs_diff_vs_latest,
      ep_documented_training  = R63_EP_DOC_TRAINING,
      ftn_date_pulled         = vapply(tabs, `[[`, character(1), "ftn_pulled"),
      scoring_settings        = scoring_settings,
      # [2026-10-01] The production k table for THIS scoring (NULL when none
      # is gated). Provenance only: the baseline k is refit per fold above.
      baseline_k_table        = load_ytd_k_table(scoring_settings = scoring_settings),
      boot_reps = R58_BOOT_REPS, boot_level = R58_BOOT_LEVEL, seed = R58_SEED
    ),
    limits = c(
      "Prior is the R/58 history-arm proxy (no rookies); fitted k biased low.",
      "QB is the smallest sample.",
      "C2 share denominators are team zone plays in games the player played.",
      "C3 protection is sack plus QB hit on blitzed dropbacks; pbp has no pressure field."
    )
  )

  if (write_outputs) {
    dir.create(R63_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    saveRDS(gate_obj, gate_path)
    saveRDS(list(folds = folds, fold_params = fold_params, by_week = by_week,
                 share_priors = dplyr::bind_rows(share_priors),
                 team_cums = cums, n_rows = nrow(frame), scoring_key = scoring_key),
            results_path)
    readr::write_csv(dplyr::bind_rows(per_fold, pooled), summary_csv)
    message(glue("  Wrote {gate_path}"))
    message(glue("  Wrote {results_path}"))
    message(glue("  Wrote {summary_csv}"))
  }

  passed_txt <- decisions %>% dplyr::filter(wiring_eligible)
  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/63 GATE: {nrow(passed_txt)} candidate-position pair(s) eligible for wiring",
               "{if (nrow(passed_txt) > 0) paste0(': ', paste(passed_txt$test, passed_txt$position, sep = '@', collapse = ', ')) else ''}"))
  message("Nothing is wired into R/29 by this run. Wiring is a separate go.")
  message(glue("{strrep('=', 70)}\n"))

  invisible(gate_obj)
}
