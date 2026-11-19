# ==============================================================================
# 71_dfs_blend_gate.R
#
# Out-of-sample gate for the FanDuel projection blend used by R/65 v3.
# [2026-11-05]
#
# WHY
#   The R/70 diagnostic on weeks 5-8 found the eligible pool unbiased (every
#   position's 90% CI on actual minus projection covered 0) while the players
#   the solver rostered ran 3.85 points per slot under projection. That is
#   winner's curse: the solver selects on projection per salary dollar, which
#   selects the players where our number disagrees most with FanDuel's price,
#   and those disagreements are where our error is largest. Blending toward
#   FanDuel's own projection (the salary file's file_proj) shrinks exactly
#   those disagreements.
#
# THE BLEND
#   proj = w * ours + (1 - w) * file_proj, one pooled w for all positions,
#   w on a 0.05 grid. A player with no FanDuel number keeps ours.
#
# GATES (pre-registered 2026-11-05; both required for use = TRUE)
#   Gate 1  Leave-one-week-out: for each scored week, w is fit on the other
#           weeks (player RMSE over all eligible players). Held-out squared
#           error must improve on ours with the 90% bootstrap lower bound
#           (players resampled within week) above 0.
#   Gate 2  Each week is re-solved under site rules twice, on ours and on the
#           held-out blend. The blended lineup's mean per-slot error (actual
#           minus the projection the solver saw) must sit closer to 0 than
#           ours, averaged over the held-out weeks.
#   Lineup totals are reported, not gated.
#   Fewer than R71_MIN_WEEKS scored weeks: use = FALSE, R/65 keeps ours.
#
# WHAT R/65 READS
#   s3_fd_dfs_blend_gate.rds: w is the weight fit on ALL scored weeks (the
#   held-out weights are the evidence; the full-sample weight is what runs).
#   R/65 checks schema, site, scoring and use before applying it.
#
# DATA
#   Every R/70 snapshot on disk whose week has actuals in the pbp cache.
#   Snapshots from before R/65 v3 carry only ours (proj) and no file_proj, so
#   file_proj is joined from that week's salary CSV (snapshot salary_file, or
#   weekN.csv); later snapshots carry proj_ours and file_proj themselves.
#   A week with a FanDuel number for fewer than R71_MIN_FD_SHARE of eligible
#   players stops the run.
#
# LIMITS
#   - Bootstrap resamples players within week; teammates and game scripts are
#     correlated, so the true interval is wider than reported.
#   - One pooled w. Per-position weights are queued until about 8 weeks.
#   - sigma_wk is not re-fit for the blend; R/69's sim keeps our sigma.
#
# RUN (Tuesday, after the pbp rebuild and score_dfs_week)
#   source(here::here("R", "71_dfs_blend_gate.R"))
#   run_dfs_blend_gate()
# ==============================================================================

library(dplyr)
library(tibble)
library(glue)
library(readr)
library(here)
if (!exists("score_dfs_week")) source(here::here("R", "70_dfs_scoreboard.R"))   # R/65, R/69, R/49
if (!exists("R65_BLEND_PATH") || !exists(".r65_apply_blend")) {
  stop("R/71 needs R/65 v3 (R65_BLEND_PATH, .r65_apply_blend). Save the 2026-11-05 R/65 first.",
       call. = FALSE)
}

R71_W_GRID       <- seq(0, 1, by = 0.05)
R71_MIN_WEEKS    <- 4L
R71_MIN_FD_SHARE <- 0.90
R71_BOOT_REPS    <- 2000L

.r71_rmse  <- function(a, p) sqrt(mean((a - p)^2))
.r71_blend <- function(d, w) dplyr::if_else(d$has_fd, w * d$proj_ours + (1 - w) * d$file_proj, d$proj_ours)
.r71_best_w <- function(d) {
  r <- vapply(R71_W_GRID, function(w) .r71_rmse(d$actual, .r71_blend(d, w)), numeric(1))
  list(w = R71_W_GRID[which.min(r)], curve = r)
}

# One scored week: snapshot pool with proj_ours and file_proj, plus actuals.
.r71_load_week <- function(w, cfg, scoring) {
  p <- .r70_snapshot_path(cfg, w)
  if (!file.exists(p)) return(NULL)
  snap <- readRDS(p)
  act <- tryCatch(.r70_actuals(snap$season, w, scoring), error = function(e) NULL)
  if (is.null(act)) {
    message(glue("  week {w}: snapshot found, no actuals in the pbp cache yet; skipped"))
    return(NULL)
  }
  pool <- snap$pool
  if (!"proj_ours" %in% names(pool)) pool$proj_ours <- pool$proj   # pre-v3 snapshot: proj is ours
  if (!"file_proj" %in% names(pool)) {
    sal_file <- if (!is.null(snap$salary_file)) snap$salary_file else sprintf("week%d.csv", w)
    sp <- file.path(R65_SALARY_DIR, sal_file)
    if (!file.exists(sp)) stop(glue("R/71: week {w} needs {sal_file} for file_proj; not found."), call. = FALSE)
    sal <- read_fd_salaries(sp) %>% dplyr::select(site_name, team, file_proj)
    if (anyDuplicated(sal[c("site_name", "team")]) > 0) {
      stop(glue("R/71: duplicate site_name + team in {sal_file}."), call. = FALSE)
    }
    pool <- pool %>% dplyr::left_join(sal, by = c("site_name", "team"), relationship = "many-to-one")
    src <- sal_file
  } else src <- "snapshot"
  el <- pool %>%
    dplyr::filter(eligible) %>%
    .r70_attach_actual(act) %>%
    dplyr::mutate(week = as.integer(w), retro = isTRUE(snap$retro), has_fd = is.finite(file_proj))
  share <- mean(el$has_fd)
  message(glue("  week {w}: {nrow(el)} eligible, FanDuel number for {sum(el$has_fd)} ",
               "({round(100 * share, 1)}%), file_proj from {src}"))
  if (share < R71_MIN_FD_SHARE) {
    stop(glue("R/71: week {w} has a FanDuel number for only {round(100 * share, 1)}% of eligible players ",
              "(minimum {100 * R71_MIN_FD_SHARE}%). Not fitting."), call. = FALSE)
  }
  list(week = as.integer(w), snap = snap, pool = pool, act = act, el = el)
}

# Re-solve a week under site rules with new projections for the eligible rows.
.r71_solve <- function(x, cfg, new_proj) {
  full <- x$pool %>%
    dplyr::left_join(new_proj, by = "player_uid", relationship = "one-to-one") %>%
    dplyr::mutate(proj = dplyr::coalesce(new_proj, proj)) %>%
    dplyr::select(-new_proj)
  s <- solve_dfs_lineup(full, cfg, integer(0), integer(0), dfs_rules(cfg))
  if (s$status != 0) stop(glue("R/71: no legal site-rules lineup in week {x$week}."), call. = FALSE)
  s$lineup
}

#' Fit and gate the FanDuel blend weight; write the table R/65 reads
#' @param weeks Integer weeks, or NULL for every snapshot on disk.
#' @param write Logical. Write s3_fd_dfs_blend_gate.rds.
#' @export
run_dfs_blend_gate <- function(weeks = NULL, site = "fd", write = TRUE, seed = R69_SEED) {
  cfg <- .r65_site(site)
  scoring <- get(cfg$scoring_name)
  message(glue("\n{strrep('=', 70)}\nR/71: {cfg$name} projection blend gate\n{strrep('=', 70)}"))
  if (is.null(weeks)) {
    f <- list.files(R65_OUTPUT_DIR, pattern = sprintf("^%sdfs_snapshot_w[0-9]{2}\\.rds$", cfg$prefix))
    weeks <- sort(as.integer(sub(".*_w([0-9]{2})\\.rds$", "\\1", f)))
  }
  wk <- Filter(Negate(is.null), lapply(weeks, .r71_load_week, cfg = cfg, scoring = scoring))
  used <- vapply(wk, function(x) x$week, integer(1))

  gate <- list(schema_tag = R65_BLEND_SCHEMA, site = site, scoring_name = cfg$scoring_name,
               built_at = Sys.time(), weeks = used, n_player_weeks = NA_integer_,
               w = NA_real_, use = FALSE, reason = NA_character_)

  if (length(used) < R71_MIN_WEEKS) {
    gate$reason <- glue("{length(used)} scored week(s); minimum {R71_MIN_WEEKS}")
    message(glue("  NOT GATED: {gate$reason}. R/65 keeps our projection."))
    if (write) { saveRDS(gate, R65_BLEND_PATH); message(glue("  Wrote {R65_BLEND_PATH}")) }
    return(invisible(gate))
  }

  pw <- dplyr::bind_rows(lapply(wk, `[[`, "el"))
  full <- .r71_best_w(pw)

  # Gate 1: leave-one-week-out
  te <- dplyr::bind_rows(lapply(used, function(h) {
    fit <- .r71_best_w(pw %>% dplyr::filter(week != h))
    x <- pw %>% dplyr::filter(week == h)
    x$w_hat <- fit$w
    x$proj_blend <- .r71_blend(x, fit$w)
    x
  }))
  dsq <- (te$actual - te$proj_ours)^2 - (te$actual - te$proj_blend)^2
  idx_by_week <- split(seq_len(nrow(te)), te$week)
  set.seed(seed)
  boot <- replicate(R71_BOOT_REPS, {
    i <- unlist(lapply(idx_by_week, function(ix) sample(ix, length(ix), replace = TRUE)))
    mean(dsq[i])
  })
  g1 <- tibble::tibble(
    mse_ours = mean((te$actual - te$proj_ours)^2),
    mse_blend = mean((te$actual - te$proj_blend)^2),
    improvement = mean(dsq),
    lo90 = stats::quantile(boot, 0.05, names = FALSE),
    hi90 = stats::quantile(boot, 0.95, names = FALSE)
  )

  # Gate 2: rostered-player error on re-solved lineups
  lu <- dplyr::bind_rows(lapply(wk, function(x) {
    p <- te %>% dplyr::filter(week == x$week)
    lo <- .r71_solve(x, cfg, p %>% dplyr::transmute(player_uid, new_proj = proj_ours))
    lb <- .r71_solve(x, cfg, p %>% dplyr::transmute(player_uid, new_proj = proj_blend))
    ao <- .r70_attach_actual(lo, x$act)
    ab <- .r70_attach_actual(lb, x$act)
    tibble::tibble(week = x$week, retro = isTRUE(x$snap$retro), w_hat = unique(p$w_hat),
                   ours_proj = sum(ao$proj), ours_real = sum(ao$actual),
                   ours_err_slot = mean(ao$actual - ao$proj),
                   blend_proj = sum(ab$proj), blend_real = sum(ab$actual),
                   blend_err_slot = mean(ab$actual - ab$proj),
                   swaps = nrow(lb) - length(intersect(lb$player_uid, lo$player_uid)))
  }))
  g2 <- lu %>% dplyr::summarise(ours_err_slot = mean(ours_err_slot), blend_err_slot = mean(blend_err_slot),
                                ours_real = mean(ours_real), blend_real = mean(blend_real))

  pass1 <- g1$lo90 > 0
  pass2 <- abs(g2$blend_err_slot) < abs(g2$ours_err_slot)

  gate$n_player_weeks <- nrow(pw)
  gate$w <- full$w
  gate$use <- pass1 && pass2
  gate$reason <- if (gate$use) "both gates passed" else
    paste(c(if (!pass1) "Gate 1 failed", if (!pass2) "Gate 2 failed"), collapse = "; ")
  gate$gate1 <- g1
  gate$gate2 <- g2
  gate$lowo_weights <- te %>% dplyr::distinct(week, w_hat)
  gate$lineups <- lu
  gate$curve <- tibble::tibble(w = R71_W_GRID, rmse = full$curve)

  r2 <- function(d) d %>% dplyr::mutate(dplyr::across(where(is.double), ~ round(.x, 3)))
  message(glue("\n  Scored weeks: {paste(used, collapse = ', ')} ({nrow(pw)} eligible player-weeks)"))
  message("\n  Held-out weight per week:")
  print(gate$lowo_weights)
  message("\n  GATE 1: held-out MSE improvement (ours minus blend), bootstrap within week:")
  print(r2(g1))
  message("\n  GATE 2: re-solved lineups under site rules (per-slot error: actual minus the projection used):")
  print(r2(lu), width = Inf)
  print(r2(g2), width = Inf)
  message(glue("\n  Full-sample weight (what R/65 runs): w = {full$w} ours, {1 - full$w} FanDuel"))
  message(glue("  VERDICT: Gate 1 {if (pass1) 'PASS' else 'FAIL'} | Gate 2 {if (pass2) 'PASS' else 'FAIL'} | ",
               "blend {if (gate$use) 'ON' else 'OFF'} in R/65"))
  if (write) {
    saveRDS(gate, R65_BLEND_PATH)
    message(glue("  Wrote {R65_BLEND_PATH}"))
  }
  message(glue("{strrep('=', 70)}\n"))
  invisible(gate)
}
