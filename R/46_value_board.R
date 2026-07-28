# ==============================================================================
# R/46  VALUE BOARD -- VORP vs market ADP (FantasyPros redraft + DK best ball)
# ==============================================================================
#
# PURPOSE
# -------
# Presentation layer on top of the R/33 VORP board. For every player it lines up
# where WE rank them (VORP) against where the MARKET drafts them, from two ADP
# sources:
#   fp_adp  -- FantasyPros redraft PPR consensus, already pulled by R/29
#   dk_adp  -- DraftKings best-ball ADP, from the DK pre-draft rankings CSV export
# and surfaces the value gap (ADP minus our rank; positive = the market lets him
# fall past where we'd take him = a target) plus the FantasyPros consensus delta
# (our projected PPG minus their consensus projection).
#
# This is a VIEW. It changes no projection and writes no cache. It reads the
# fresh R/33 rankings, joins the FantasyPros fields straight from the R/29 CSV
# (authoritative, in case R/32/R/33 dropped them), and crosswalks the DK CSV to
# the board by normalized name + position.
#
# CAVEAT ON QB: vorp overall_rank and ADP are both overall draft position, but
# in a 1-QB league VORP pushes QBs far down while ADP drafts elite QBs early, so
# QB value gaps read very negative by construction. Read RB/WR/TE for clean value
# signal, or filter with position.
# ==============================================================================

library(dplyr)
library(readr)
library(glue)
library(here)

# Mirror of R/31 .normalize_name_for_match_r31 so the DK crosswalk matches the
# board exactly the way the rest of the pipeline matches names.
.vb_normalize_name <- function(name_vec) {
  out <- tolower(as.character(name_vec))
  out <- gsub("[.']", "", out)
  out <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", out, ignore.case = TRUE)
  out <- gsub("[^a-z ]", "", out)
  out <- gsub("\\s+", " ", out)
  trimws(out)
}


# ------------------------------------------------------------------------------
# load_dk_adp
# ------------------------------------------------------------------------------

#' Load the DraftKings best-ball ADP export.
#'
#' The DK pre-draft rankings CSV has columns ID, Name, Position, ADP, Team (plus
#' an instructions column that is ignored). ADP is a fractional overall pick.
#'
#' @param dk_csv_path Path to the DK export.
#' @param verbose Logical.
#' @return tibble(name_norm, position, team_dk, dk_adp).
load_dk_adp <- function(dk_csv_path, verbose = TRUE) {
  raw <- readr::read_csv(dk_csv_path, show_col_types = FALSE,
                         name_repair = "unique")
  need <- c("Name", "Position", "ADP")
  miss <- setdiff(need, names(raw))
  if (length(miss) > 0L) {
    stop(glue("load_dk_adp(): missing columns {paste(miss, collapse = ', ')}. ",
              "Present: {paste(head(names(raw), 12), collapse = ', ')}"))
  }

  dk <- raw %>%
    dplyr::transmute(
      name_norm = .vb_normalize_name(.data$Name),
      position  = toupper(trimws(as.character(.data$Position))),
      team_dk   = if ("Team" %in% names(raw)) toupper(trimws(as.character(.data$Team)))
                  else NA_character_,
      dk_adp    = suppressWarnings(as.numeric(.data$ADP))
    ) %>%
    dplyr::filter(!is.na(.data$dk_adp), nchar(.data$name_norm) > 0,
                  .data$position %in% c("QB", "RB", "WR", "TE")) %>%
    dplyr::arrange(.data$dk_adp) %>%
    # one row per name+position (keep the earliest ADP if duplicated)
    dplyr::distinct(.data$name_norm, .data$position, .keep_all = TRUE)

  if (verbose) message(glue("  load_dk_adp(): {nrow(dk)} DK best-ball ADP entries"))
  dk
}


# ------------------------------------------------------------------------------
# build_value_board
# ------------------------------------------------------------------------------

#' Join the VORP board to FantasyPros and DK ADP and compute value gaps.
#'
#' @param rankings Output of compute_vorp_rankings() (R/33). Needs nfl_gsis_id,
#'   player_name, position, team, r32_posterior_mu, adjusted_vorp, overall_rank.
#' @param r29_csv_path R/29 projections CSV (source of the FantasyPros fields).
#' @param dk_csv_path DK best-ball ADP export.
#' @param verbose Logical.
#'
#' @return tibble sorted by our VORP rank with: player_name, position, team,
#'   r32_posterior_mu, adjusted_vorp, vorp_rank, fp_adp, dk_adp, value_vs_fp,
#'   value_vs_dk, consensus_delta. Value = ADP - our rank (positive = value).
build_value_board <- function(rankings,
                              r29_csv_path = here::here(
                                "data", "season2_cache",
                                "s2_week15_player_projections.csv"),
                              dk_csv_path  = here::here(
                                "data", "season3_cache",
                                "DkPreDraftRankings20260713.csv"),
                              season = 2026L,
                              verbose = TRUE) {

  # consensus_delta stays from the R/29 CSV (keyed by gsis).
  fp_consensus <- readr::read_csv(r29_csv_path, show_col_types = FALSE) %>%
    dplyr::select(dplyr::any_of(c("nfl_gsis_id", "consensus_delta"))) %>%
    dplyr::distinct(.data$nfl_gsis_id, .keep_all = TRUE)

  # FP ADP by NAME, not gsis: R/29's ADP join keys on gsis, so rookies with a
  # synthetic gsis come back NA. load_adp_fantasypros() is name-keyed and cached,
  # so crosswalking it by name+position (same as DK) recovers the rookies.
  fp_adp_tbl <- tryCatch(
    load_adp_fantasypros(season = season, scoring = "PPR"),
    error = function(e) { message(glue("  FP ADP load failed: {e$message}")); NULL })
  if (!is.null(fp_adp_tbl)) {
    fp_adp_tbl <- fp_adp_tbl %>%
      dplyr::transmute(name_norm = .vb_normalize_name(.data$player_name),
                       position  = toupper(.data$pos),
                       fp_adp    = .data$adp_rank) %>%
      dplyr::filter(!is.na(.data$fp_adp), nchar(.data$name_norm) > 0) %>%
      dplyr::arrange(.data$fp_adp) %>%
      dplyr::distinct(.data$name_norm, .data$position, .keep_all = TRUE)
  } else {
    fp_adp_tbl <- tibble::tibble(name_norm = character(), position = character(),
                                 fp_adp = numeric())
  }

  dk <- load_dk_adp(dk_csv_path, verbose = verbose)

  board <- rankings %>%
    dplyr::mutate(name_norm = .vb_normalize_name(.data$player_name)) %>%
    dplyr::left_join(fp_consensus, by = "nfl_gsis_id") %>%
    dplyr::left_join(fp_adp_tbl, by = c("name_norm", "position")) %>%
    dplyr::left_join(dplyr::select(dk, name_norm, position, dk_adp),
                     by = c("name_norm", "position"))

  if (verbose) {
    fp_hit <- mean(!is.na(board$fp_adp))
    dk_hit <- mean(!is.na(board$dk_adp))
    message(glue("  value board: {nrow(board)} players | ",
                 "FP ADP matched {round(100*fp_hit)}% | ",
                 "DK ADP matched {round(100*dk_hit)}%"))
  }

  board %>%
    dplyr::transmute(
      player_name, position, team,
      r32_posterior_mu = round(.data$r32_posterior_mu, 1),
      adjusted_vorp    = round(.data$adjusted_vorp, 2),
      vorp_rank        = .data$overall_rank,
      fp_adp           = .data$fp_adp,
      dk_adp           = round(.data$dk_adp, 1),
      value_vs_fp      = .data$fp_adp - .data$overall_rank,
      value_vs_dk      = round(.data$dk_adp - .data$overall_rank, 1),
      consensus_delta  = round(.data$consensus_delta, 1)
    ) %>%
    dplyr::arrange(.data$vorp_rank)
}


# ------------------------------------------------------------------------------
# value_targets / value_reaches  (convenience views)
# ------------------------------------------------------------------------------

#' Biggest values (market drafts them well after our rank), RB/WR/TE by default.
#' @param board Output of build_value_board().
#' @param source c("dk","fp"). @param n rows. @param positions to include.
value_targets <- function(board, source = c("dk", "fp"), n = 25L,
                          positions = c("RB", "WR", "TE")) {
  source <- match.arg(source)
  col <- if (source == "dk") "value_vs_dk" else "value_vs_fp"
  board %>%
    dplyr::filter(.data$position %in% positions, !is.na(.data[[col]])) %>%
    dplyr::arrange(dplyr::desc(.data[[col]])) %>%
    utils::head(n)
}

#' Biggest reaches (market drafts them well ahead of our rank).
value_reaches <- function(board, source = c("dk", "fp"), n = 25L,
                          positions = c("RB", "WR", "TE")) {
  source <- match.arg(source)
  col <- if (source == "dk") "value_vs_dk" else "value_vs_fp"
  board %>%
    dplyr::filter(.data$position %in% positions, !is.na(.data[[col]])) %>%
    dplyr::arrange(.data[[col]]) %>%
    utils::head(n)
}


# ------------------------------------------------------------------------------
# dk_best_ball_config  (verified DK Best Ball preset for R/33)
# ------------------------------------------------------------------------------

# DK Best Ball scoring (verified 2026-07-13 vs DK DFS rules): full PPR, 0.04/pass
# yd, 4/pass TD, -1 INT, -1 fumble, 0.1 rush+rec yd, 6 rush+rec TD, +3 at 100
# rush yds, +3 at 100 rec yds, +3 at 300 pass yds. R/17 schema.
# DK_BEST_BALL_SCORING is defined ONCE, in R/17. [2026-07-16]
# Was dual-defined here and at R/45:61; source order silently picked a winner.
# Do not redefine. Derive variants with modifyList(DK_BEST_BALL_SCORING, ...).
if (!exists("DK_BEST_BALL_SCORING")) source(here::here("R", "17_extended_scoring.R"))

#' DraftKings Best Ball league config, verified 2026-07-13 against DK rules.
#' Weekly lineup: 1 QB, 2 RB, 3 WR, 1 TE, 1 FLEX (RB/WR/TE); 20-man roster (the
#' bench depth does not change starter replacement levels). Scoring is full PPR
#' with the 100/300-yard bonuses; those bonuses shape the board through the
#' best_ball CEILING weighting in R/33 (boom_weight 4.0, ceiling_factor 0.3),
#' which is best ball's proxy for upside, not literal per-projection bonus points
#' (R/29 cannot price play-level bonuses at the aggregate prior level).
#'
#' num_teams defaults to 12 (DK Millionaire pod size). Requires R/33 sourced.
#'
#' @return a league_config object for compute_vorp_rankings().
dk_best_ball_config <- function(num_teams = 12L, league_name = "DK Best Ball") {
  if (!exists("build_league_config")) {
    stop("dk_best_ball_config(): source R/33 first (build_league_config not found).")
  }
  build_league_config(
    league_name = league_name,
    num_teams   = as.integer(num_teams),
    starters    = list(QB = 1L, RB = 2L, WR = 3L, TE = 1L),
    flex        = 1L,
    superflex   = 0L,
    format      = "best_ball"
  )
}


# ------------------------------------------------------------------------------
# run_value_boards  (guided, TRUE per-league scoring)
# ------------------------------------------------------------------------------

# Stable key for a scoring config so leagues with identical scoring share one
# engine run.
# Serialize one scoring value deterministically.
#
# [2026-07-16] Rewritten for nested params. The old version was
# paste(x, collapse = ","), which flattens a named list to its values and drops
# the names. long_td_tiers = list(rec = c("40"=2,"50"=4)) and
# list(rush = c("40"=2,"50"=4)) would both serialize to "2,4" and collide, so
# R/47's dedupe would run ONE engine for two different rulesets and write the
# same board to both. tiered_rec_tiers survived that only because it is a bare
# unnamed vector.
.scoring_val <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")
  if (is.list(x)) {
    x <- x[order(names(x))]
    return(paste0("{", paste(names(x), vapply(x, .scoring_val, character(1)),
                             sep = ":", collapse = "|"), "}"))
  }
  if (!is.null(names(x)) && any(nzchar(names(x)))) {
    o <- order(names(x))
    return(paste(names(x)[o], x[o], sep = "+", collapse = ","))
  }
  paste(x, collapse = ",")
}

.scoring_key <- function(sc) {
  flat <- sc[order(names(sc))]
  paste(names(flat), vapply(flat, .scoring_val, character(1)),
        sep = "=", collapse = ";")
}

# R/42 resolves only the scoring fields Sleeper provides; R/29 references its full
# R/17-schema set, and a missing field reads as NULL and collapses a scoring term
# to length 0. Merge league scoring onto this complete R/17 template (neutral
# gap-fills), so league values override and every other field has a real default.
# NOTE: this is R/17's schema (per-reception key is `ppr`). It deliberately does
# NOT read a global DEFAULT_SCORING_SETTINGS, because R/29 and R/32 each define
# one with different keys (R/32 uses `rec`), and whichever was sourced last would
# otherwise poison the scoring passed to calculate_fantasy_points_ext.
# NOTE [2026-07-16] on pick6_penalty = 0.
# This template's job is to fill keys the league did NOT specify, so every
# default here must be NEUTRAL, not R/17's opinionated default. te_premium
# (R/17: TRUE), rush_att_bonus (R/17: 0.25) and use_tiered_ppr (R/17: TRUE)
# were all neutralized. pick6_penalty was not: it sat at -4, mirroring R/17.
#
# Until pass_int_td was wired into R/19 (STEP 6c) no league ever emitted
# pick6_penalty, so all 7 boards silently carried a -4 pick-six penalty that
# only 1 league (Super flex, pass_int_td = -4) actually scores. That league
# was correct by accident; the other six were not. Surfaced by the
# 10 Guys / 4th and Drunk scoring_key divergence: 10 Guys sends
# pass_int_td = 0 explicitly, 4th and Drunk omits the field, so identical
# leagues stopped hashing identically.
#
# Absence of a Sleeper field means the league does not score it. Default 0.
.SCORING_TEMPLATE <- list(
  pass_yd = 0.04, pass_td = 4, pass_int = -2, pick6_penalty = 0,
  rush_yd = 0.1, rush_td = 6, rec_yd = 0.1, rec_td = 6, ppr = 1, fumbles = -2,
  use_tiered_ppr = FALSE, te_premium = FALSE, rush_att_bonus = 0,
  first_down_points = 0, long_td_tiers = NULL,
  hundred_yard_bonus = 0, superflex_pass_td = 0, two_point_conversion = 2,
  sack_penalty = 0, tiered_rec_tiers = NULL,
  bonus_pass_yd_300 = 0, bonus_pass_yd_400 = 0,
  bonus_rec_yd_200 = 0, bonus_rush_yd_200 = 0, pass_2pt = 2
)

.complete_scoring <- function(sc) {
  merged <- utils::modifyList(.SCORING_TEMPLATE, sc)
  merged[["rec"]] <- NULL   # R/17 uses `ppr`; drop any stray Sleeper-style `rec`
  merged
}

#' Guided value-board runner with TRUE per-league scoring.
#'
#' Prompts for a Sleeper username, lets you pick leagues, resolves each league's
#' real scoring via R/42, and, deduped by scoring, RE-RUNS R/29 and R/32 under
#' that scoring (to temp files, production caches untouched) so each league's
#' board is projected on its own rules, not a shared PPR baseline. Then VORP's
#' each league on its scored board and writes one value board CSV per league.
#'
#' Requires the fresh 2026 R/30 and R/31 caches on disk (volume is scoring-
#' independent and reused across scorings). Run interactively.
#'
#' @param dk_csv_path DK best-ball ADP export for the dk_adp column.
#' @param out_dir Output directory for per-league CSVs.
#' @param season Season to project. Default 2026.
#' @return (invisibly) a named list of value boards, one per league.
run_value_boards <- function(
    dk_csv_path = here::here("data", "season3_cache",
                             "DkPreDraftRankings20260713.csv"),
    out_dir     = here::here("data", "season3_cache"),
    season      = 2026L) {

  need <- c("resolve_league_scoring", "get_user_leagues", "build_config_from_sleeper",
            "compute_vorp_rankings", "run_projection_engine", "reconcile_projections")
  miss <- need[!vapply(need, exists, logical(1))]
  if (length(miss) > 0L) {
    stop(glue("run_value_boards(): missing {paste(miss, collapse = ', ')}. ",
              "Source R/29, R/32, R/33, R/42 first."))
  }
  if (!interactive()) message("run_value_boards(): prompts you; run interactively.")

  # ---- gather leagues ----
  leagues <- list()   # each: list(name, config, scoring)

  username <- trimws(readline("Sleeper username (blank to skip Sleeper): "))
  if (nzchar(username)) {
    lg <- tryCatch(get_user_leagues(username, season = season),
                   error = function(e) { message(glue("  Sleeper error: {e$message}")); NULL })
    if (!is.null(lg) && nrow(lg) > 0L) {
      picks <- utils::select.list(lg$name, multiple = TRUE,
                                  title = "Select league(s) to build")
      for (nm in picks) {
        lid <- lg$league_id[match(nm, lg$name)]
        cfg <- tryCatch(build_config_from_sleeper(lid),
                        error = function(e) { message(glue("  Skip {nm}: {e$message}")); NULL })
        sc  <- tryCatch(resolve_league_scoring(lid, verbose = FALSE)$offense,
                        error = function(e) { message(glue("  Scoring skip {nm}: {e$message}")); NULL })
        if (!is.null(cfg) && !is.null(sc)) {
          leagues[[cfg$league_name]] <- list(name = cfg$league_name, config = cfg,
                                             scoring = .complete_scoring(sc))
        }
      }
    } else message("  No Sleeper leagues found.")
  }

  if (tolower(trimws(readline("Include DK Best Ball board? [Y/n]: "))) %in% c("", "y", "yes")) {
    leagues[["DK Best Ball"]] <- list(name = "DK Best Ball",
                                      config = dk_best_ball_config(),
                                      scoring = .complete_scoring(DK_BEST_BALL_SCORING))
  }

  if (length(leagues) == 0L) { message("Nothing selected."); return(invisible(list())) }

  # ---- confirm ----
  message("\nLeagues to build (each projected on its own scoring):")
  for (L in leagues) {
    st <- L$config$starters
    message(glue("  - {L$name}: format={L$config$format}, teams={L$config$num_teams}, ",
                 "lineup QB{st$QB}/RB{st$RB}/WR{st$WR}/TE{st$TE}+{L$config$flex}FLEX | ",
                 "ppr={L$scoring$ppr %||% NA}, pass_td={L$scoring$pass_td %||% NA}, ",
                 "te_prem={L$scoring$te_premium %||% NA}"))
  }
  if (!(tolower(trimws(readline("Proceed? [Y/n]: "))) %in% c("", "y", "yes"))) {
    message("Aborted."); return(invisible(list()))
  }

  # ---- dedupe by scoring, re-run engine per distinct scoring ----
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  keys <- vapply(leagues, function(L) .scoring_key(L$scoring), character(1))
  boards <- list()

  old_out <- if (exists("OUTPUT_PROJECTIONS_PATH", envir = .GlobalEnv)) get("OUTPUT_PROJECTIONS_PATH", envir = .GlobalEnv) else NULL
  old_def <- if (exists("OUTPUT_DEF_ST_PATH", envir = .GlobalEnv)) get("OUTPUT_DEF_ST_PATH", envir = .GlobalEnv) else NULL
  on.exit({
    if (!is.null(old_out)) assign("OUTPUT_PROJECTIONS_PATH", old_out, envir = .GlobalEnv)
    if (!is.null(old_def)) assign("OUTPUT_DEF_ST_PATH", old_def, envir = .GlobalEnv)
  }, add = TRUE)

  for (k in unique(keys)) {
    group <- leagues[keys == k]
    sc    <- group[[1]]$scoring
    grp_names <- paste(vapply(group, function(g) g$name, character(1)), collapse = ", ")
    message(glue("\n=== scoring group: {grp_names} ==="))

    tmp_csv <- tempfile(fileext = ".csv")
    assign("OUTPUT_PROJECTIONS_PATH", tmp_csv, envir = .GlobalEnv)
    if (!is.null(old_def)) assign("OUTPUT_DEF_ST_PATH", tempfile(fileext = ".csv"), envir = .GlobalEnv)

    message("  running R/29 under this scoring...")
    run_projection_engine(season = season, week = 1L, scoring_settings = sc)
    message("  reconciling (R/32) under this scoring...")
    # R/29 uses `ppr`; R/32's volume-implied conversion uses `rec`. Give R/32 a
    # `rec` mirror of `ppr` so it scores receptions (without it, vi receiving = 0
    # and every pass-catcher's blended projection collapses).
    sc_r32 <- utils::modifyList(sc, list(rec = sc$ppr))
    board_sc <- reconcile_projections(r29_path = tmp_csv, scoring_settings = sc_r32,
                                      save_output = FALSE)

    for (L in group) {
      rankings <- compute_vorp_rankings(board_sc, L$config)
      vb <- build_value_board(rankings, r29_csv_path = tmp_csv,
                              dk_csv_path = dk_csv_path, season = season,
                              verbose = TRUE)
      safe  <- gsub("[^A-Za-z0-9]+", "_", L$name)
      fpath <- file.path(out_dir, glue("value_board_{safe}.csv"))
      readr::write_csv(vb, fpath)
      message(glue("  wrote {basename(fpath)}  ({nrow(vb)} players)"))
      boards[[L$name]] <- vb
    }
    unlink(tmp_csv)
  }

  message(glue("\nDone. {length(boards)} league board(s), ",
               "{length(unique(keys))} engine run(s), in {out_dir}."))
  invisible(boards)
}
