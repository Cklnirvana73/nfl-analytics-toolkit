# ==============================================================================
# 65_daily_optimizer.R
#
# Daily (DFS) lineup optimizer, v3: FanDuel classic.  [2026-11-05]
# (v1 2026-10-05; v2 adds user rules, alternatives, swap cost. No projection
#  changes: every v2 addition is a constraint the user sets or a re-solve.)
#
# V2 (forks locked 2026-10-22)
#   F1 C  Exposure caps, each on its own switch: max players per team (count)
#         and max share of the lineup's projected points from one team
#         (DEF counts toward its team, as for the site's 4-player limit).
#         Both are linear: share uses sum_i p_i * (1[team_i = t] - s) x_i <= 0.
#   F2 C  Top-N alternatives (no-good cuts: each new lineup shares at most
#         9 - min_diff players with every earlier one; the DEF row counts) and
#         a swap cost per lineup player (best total minus the best total with
#         him excluded, plus who comes in).
#   F3 B  Stacking. QB stack k: an eligible QB may be used only with >= k WR/TE
#         from his team. This is the one stacking rule with evidence in our own
#         data: R/50 found QB-WR / QB-TE / QB-RB correlations replicate out of
#         sample and every non-QB pair cell did not (neutralized to 0). Bring-
#         back (>= 1 opponent player with the QB) and no-DEF-conflict (no
#         player facing your DEF) are contest rules the user chooses; no gate.
#   Default rules reproduce v1 exactly (verified 2026-10-22: same 9, 121.33).
#   SOLVER: GLPK via Rglpk replaces lpSolve (2026-10-22). The verification run
#   showed lpSolve's week 7 lineup was not optimal (a lineup with Gibbs
#   excluded scored 4.36 more). Every solve in run_dfs_optimizer and the app
#   is now followed by an optimality check: no swap cost may be negative.
#   QB roles: teams whose QBs are all on bye are labelled 'bye' (was the
#   default 'no_snaps', which read as a data gap; checked 2026-10-22: BUF/JAX/
#   LAC/WAS all 6 games cached, no dropback missing a QB id).
#   Visibility (Watson class, 2026-10-22): prior_mu, ytd_mean, ytd_n and
#   ytd_gap = ytd_mean - prior_mu joined from the site's R/29 file by
#   player_uid. Display only; nothing in the objective changes.
#
# TWO ENTRY POINTS
#   run_dfs_chain(week)      Runs the projection chain under the SITE's scoring
#                            with a site prefix on every output, so nothing
#                            touches the flat-PPR production files:
#                              R/29 run_projection_engine   (FANDUEL_SCORING)
#                              R/32 reconcile_projections   (FANDUEL_SCORING)
#                              R/34 project_def_st          (+ R/66 Vegas shift)
#                              R/59 run_weekly_bands        (availability flags)
#   run_dfs_optimizer(week)  Reads the site's salary CSV, matches it to the
#                            chain output, and solves one lineup under the cap
#                            with locks and excludes.
#
# DESIGN (forks locked 2026-09-30 / 2026-10-01)
#   1 C  Salaries from a drop-folder CSV (data/dfs_salaries/), newest file wins
#        unless a path is passed.
#   2 A  One lineup per site/slate; locks and excludes. Differ-by-k is later.
#   3 A  Chain run per site scoring (above).
#   4 B  The salary file defines the slate. Classic contests only.
#   6 A  Name + team matching through R/53's normalizer, a manual overrides CSV
#        that wins, unmatched rows flagged no_projection (excluded, listed),
#        and a hard stop if two salary rows resolve to one player.
#   7 A  Objective = projected mean: R/59 mu (players), R/34 def_proj_ppg (DEF).
#   8 C  DEF from R/34 v2 (R/66-gated Vegas shift on; R/35 factor neutralized).
#   9 B  Questionable players excluded by default; include_q = TRUE includes
#        them. Out / IR / suspended / bye are never eligible.
#  10 B  Per-site R/58 and R/63 gate tables (R/29 loads them by scoring key).
#  11 A  File format verified on a real FanDuel export (2026-10-05):
#          header  player,team,gameInfo,salary,value,... (20 columns)
#          players 20 fields; D/ST rows 14 fields (stat columns shifted);
#          kicker rows 17 fields with salary N/A (no K slot: dropped);
#          some skill players also carry N/A salary (not on the slate: dropped)
#          team = full name ("Cincinnati Bengals"); gameInfo = "JAC @ CIN";
#          salary = "$6000"; no position column (position comes from R/59).
#  12 B  The standalone Shiny app (shiny_dfs/) is a separate delivery and calls
#        these functions; nothing here depends on it.
#
# FANDUEL CLASSIC RULES (fanduel.com/rules, checked 2026-10-05; cap confirmed by
# Christian 2026-10-01)
#   QB, RB x2, WR x3, TE, FLEX (RB/WR/TE), DEF; salary cap $60,000;
#   at most 4 players from one team; players from at least 3 teams.
#   ASSUMPTION: the D/ST counts toward its team for the 4-player limit.
#
# QB ROLES (2026-10-09): backup QBs are excluded as qb_backup by a starter
#   rule built from 2026 pbp (see .r65_qb_roles). Lockable. Projections unchanged.
#
# V3 (fork A locked 2026-11-05): FanDuel projection blend
#   proj = w * ours + (1 - w) * file_proj, applied inside .r65_match() so the
#   console optimizer and shiny_dfs get the same number. w comes from the R/71
#   gate table (s3_fd_dfs_blend_gate.rds), fit on every scored R/70 week.
#   Why: the R/70 diagnostic (weeks 5-8) found rostered players ran 3.85
#   points per slot under projection while the pool was unbiased (winner's
#   curse: the solver selects where ours disagrees most with the price).
#   The blend passed both R/71 gates (held-out MSE, rostered-player error).
#   Rows with no FanDuel number, and any run where the gate file is missing,
#   unreadable, for another site or scoring, or did not pass, keep ours and
#   say so in the console. Our number stays visible as proj_ours; blend_w
#   records the weight used (NA = ours only). sigma_wk is unchanged.
#
# LIMITS (stated so they are not forgotten)
#   - R/59 mu is a per-game season mean (R/29 YTD rule), not week-specific: no
#     opponent adjustment for offense (R/35 offense factor neutralized on gate
#     evidence). Only DEF carries a week-specific term (Vegas).
#   - The salary file has no position, so a player FanDuel lists at a
#     different position than our roster source is slotted by OUR position.
#   - R/50 shape table predates flat-PPR (built 2026-08-02); affects bands in
#     R/59, not the mean this optimizer uses.
#   - The file's own projection (file_proj) enters the objective only through
#     the V3 blend, and only while the R/71 gate passes.
#
# NOTHING IN THIS FILE WRITES TO PRODUCTION (flat-PPR) FILES. Every output is
# prefixed with the site prefix (fd: "s3_fd_").
#
# USAGE
#   source(here::here("R", "65_daily_optimizer.R"))
#   run_dfs_chain(week = 5)                         # once per week, after the
#                                                   # pbp cache covers week - 1
#   lu <- run_dfs_optimizer(week = 5)               # newest CSV in data/dfs_salaries
#   lu <- run_dfs_optimizer(week = 5, locks = c("Ja'Marr Chase"),
#                           excludes = c("Joe Flacco"), include_q = TRUE)
#   lu <- run_dfs_optimizer(week = 7,
#                           rules = dfs_rules(max_per_team = 3, max_team_share = 0.35,
#                                             qb_stack = 1),
#                           n_lineups = 10, min_diff = 2)
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(readr)
library(glue)
library(here)
library(nflreadr)
library(Rglpk)     # [2026-10-22] solver (lpSolve returned suboptimal lineups; see solve_dfs_lineup)

if (!exists(".pidr_normalize_name")) {
  source(here::here("R", "53_player_identity.R"))
}
if (!exists("load_normalized_season")) {
  source(here::here("R", "15_multi_season_pbp.R"))   # 2026 pbp for QB roles
}

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------
R65_SCHEMA_TAG   <- "s3_r65_dfs_v3"   # v3: FanDuel projection blend, proj_ours, blend_w [2026-11-05]
R65_SALARY_DIR   <- here::here("data", "dfs_salaries")
R65_OVERRIDE_PATH <- file.path(R65_SALARY_DIR, "name_overrides.csv")
R65_OUTPUT_DIR   <- here::here("data", "season3_cache")
R65_CHAIN_CACHE  <- here::here("data", "season2_cache")   # R/29 and R/32 write here
R65_BLEND_PATH   <- file.path(R65_OUTPUT_DIR, "s3_fd_dfs_blend_gate.rds")   # written by R/71
R65_BLEND_SCHEMA <- "s3_r71_dfs_blend_v1"

R65_SITES <- list(
  fd = list(
    name         = "FanDuel",
    scoring_name = "FANDUEL_SCORING",     # R/17
    def_scoring  = NULL,                  # FanDuel D/ST == R/34 default (checked 2026-10-01)
    prefix       = "s3_fd_",
    cap          = 60000L,
    slots        = c(QB = 1L, RB = 2L, WR = 3L, TE = 1L, FLEX = 1L, DEF = 1L),
    flex         = c("RB", "WR", "TE"),
    max_per_team = 4L,
    min_teams    = 3L,
    header_first = c("player", "team", "gameInfo", "salary")
  )
)

# Abbreviations seen in salary-file gameInfo that differ from nflverse. Applied
# before nflreadr::clean_team_abbrs(); any code still unknown after both is a
# hard stop, never a guess.
R65_ABBR_ALIASES <- c(JAC = "JAX", WSH = "WAS", LAR = "LA")

# Wrappers so the network calls are in one place (and testable).
.r65_load_teams     <- function() nflreadr::load_teams(current = TRUE)
.r65_load_schedules <- function(season) nflreadr::load_schedules(seasons = season)

.r65_clean_abbr <- function(x) {
  x <- toupper(trimws(as.character(x)))
  hit <- x %in% names(R65_ABBR_ALIASES)
  x[hit] <- unname(R65_ABBR_ALIASES[x[hit]])
  nflreadr::clean_team_abbrs(x)
}

.r65_site <- function(site) {
  cfg <- R65_SITES[[site]]
  if (is.null(cfg)) stop("R/65: unknown site '", site, "'. Known: ",
                         paste(names(R65_SITES), collapse = ", "), call. = FALSE)
  cfg
}

.r65_path <- function(cfg, stem, week, ext) {
  file.path(R65_OUTPUT_DIR, sprintf("%s%s_w%02d.%s", cfg$prefix, stem, as.integer(week), ext))
}

# ==============================================================================
# PART 1: THE CHAIN
# ==============================================================================

.r65_source_chain <- function() {
  # R/34 and R/59 are re-sourced every time: both are light, and a session that
  # sourced an older copy earlier would otherwise keep the stale definition
  # (first R/65 run, 2026-10-06: R/59 saved on disk, old run_weekly_bands still
  # in memory). R/32 is heavy and unchanged by R/65, so it stays conditional.
  source(here::here("R", "34_def_st_projection.R"))   # sources R/29 only if absent
  if (!exists("reconcile_projections")) source(here::here("R", "32_projection_reconciliation.R"))
  source(here::here("R", "59_weekly_projection_bands.R"))
  for (fn in c("run_projection_engine", "reconcile_projections", "project_def_st",
               "run_weekly_bands", ".r32_output_rds_path")) {
    if (!exists(fn)) stop("R/65: ", fn, "() not found after sourcing the chain.", call. = FALSE)
  }
  if (!"out_prefix" %in% names(formals(run_weekly_bands))) {
    stop("R/65: R/59 run_weekly_bands() has no out_prefix argument even after ",
         "re-sourcing R/59 from disk. Save the 2026-10-05 R/59 replacement; ",
         "otherwise this run would overwrite the flat-PPR bands.", call. = FALSE)
  }
  if (!all(c("vegas_gate_path", "vegas_scoring_key") %in% names(formals(run_weekly_bands)))) {
    stop("R/65: R/59 run_weekly_bands() has no vegas_gate_path argument. Save the ",
         "2026-10-22 R/59 replacement first.", call. = FALSE)
  }
  if (!exists("scoring_gate_path") || !exists(".r29_scoring_key")) {
    stop("R/65: R/29's scoring_gate_path() not loaded after sourcing the chain.", call. = FALSE)
  }
  if (!"week" %in% names(formals(project_def_st))) {
    stop("R/65: R/34 project_def_st() has no week argument (pre-v2.0). Save the ",
         "2026-10-01 R/34 replacement first.", call. = FALSE)
  }
}

.r65_assert_fresh <- function(path, t0, what) {
  if (!file.exists(path) || file.mtime(path) < t0) {
    stop(glue("R/65: {what} was not written to {path} by this run. If R/46 ",
              "reassigned OUTPUT_PROJECTIONS_PATH in this session, restart R ",
              "and rerun. Nothing downstream was run."), call. = FALSE)
  }
  message(glue("  OK: {basename(path)} written {format(file.mtime(path), '%H:%M:%S')}"))
}

#' Run the projection chain for one site and week
#'
#' @param week Integer. The week being projected (the slate week).
#' @param season Integer.
#' @param site Character. "fd".
#' @return Invisible list of output paths.
#' @export
run_dfs_chain <- function(week, season = 2026L, site = "fd") {
  week   <- as.integer(week)
  season <- as.integer(season)
  cfg    <- .r65_site(site)
  .r65_source_chain()
  if (!exists(cfg$scoring_name)) stop("R/65: ", cfg$scoring_name, " not defined (R/17).", call. = FALSE)
  scoring <- get(cfg$scoring_name)

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/65 CHAIN: {cfg$name} -- season {season} week {week}, prefix '{cfg$prefix}'"))
  message(glue("{strrep('=', 70)}"))
  t0 <- Sys.time()

  message("\n[chain 1/4] R/29 under site scoring")
  r29_csv <- file.path(R65_CHAIN_CACHE, paste0(cfg$prefix, "player_projections.csv"))
  run_projection_engine(season = season, week = week, scoring_settings = scoring,
                        cache_dir = R65_CHAIN_CACHE, out_prefix = cfg$prefix,
                        write_outputs = TRUE)
  .r65_assert_fresh(r29_csv, t0, "R/29 projections")

  message("\n[chain 2/4] R/32 reconciliation under site scoring")
  reconcile_projections(r29_path = r29_csv, as_of_week = week,
                        scoring_settings = scoring, season = season,
                        cache_dir = R65_CHAIN_CACHE, out_prefix = cfg$prefix,
                        save_output = TRUE)
  recon_rds <- .r32_output_rds_path(R65_CHAIN_CACHE, cfg$prefix)
  .r65_assert_fresh(recon_rds, t0, "R/32 reconciled projections")

  message("\n[chain 3/4] R/34 DEF with the R/66 Vegas shift")
  def <- project_def_st(season = season, as_of_week = week - 1L, week = week,
                        scoring = cfg$def_scoring, save_output = FALSE)
  dir.create(R65_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  def_rds <- .r65_path(cfg, "def_st", week, "rds")
  saveRDS(def, def_rds)
  readr::write_csv(def, .r65_path(cfg, "def_st", week, "csv"))
  message(glue("  Wrote {def_rds}"))

  message("\n[chain 4/4] R/59 bands and availability")
  # [2026-10-22] R/68 offense Vegas shift for THIS site's scoring (R/59 turns
  # it off, with a console line, if the keyed gate file is absent or mismatched).
  run_weekly_bands(week = week, season = season, recon_path = recon_rds,
                   out_prefix = cfg$prefix, write_outputs = TRUE,
                   vegas_gate_path = scoring_gate_path(
                     file.path(R65_OUTPUT_DIR, "s3_r68_offense_vegas.rds"), scoring),
                   vegas_scoring_key = .r29_scoring_key(scoring))
  bands_rds <- file.path(R65_OUTPUT_DIR,
                         sprintf("%sr59_weekly_bands_w%02d.rds", cfg$prefix, week))
  .r65_assert_fresh(bands_rds, t0, "R/59 bands")

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/65 CHAIN COMPLETE ({round(as.numeric(difftime(Sys.time(), t0, units = 'mins')), 1)} min). ",
               "Next: run_dfs_optimizer(week = {week})"))
  message(glue("{strrep('=', 70)}\n"))
  invisible(list(r29 = r29_csv, recon = recon_rds, def = def_rds, bands = bands_rds))
}

# ==============================================================================
# PART 2: SALARY FILE
# ==============================================================================

#' Newest salary CSV in the drop folder (overrides file excluded)
#' @export
newest_salary_file <- function(dir = R65_SALARY_DIR) {
  if (!dir.exists(dir)) stop("R/65: salary folder not found: ", dir,
                             ". Create it and drop the site's CSV in.", call. = FALSE)
  f <- list.files(dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  f <- f[basename(f) != basename(R65_OVERRIDE_PATH)]
  if (length(f) == 0) stop("R/65: no salary CSV in ", dir, call. = FALSE)
  f[which.max(file.mtime(f))]
}

#' Parse a FanDuel salary CSV (format verified 2026-10-05)
#'
#' @param path Character.
#' @return Tibble: site_name, row_type ("player"/"dst"), team_full, team,
#'   opponent, home, game, salary, file_proj. Rows with no numeric salary are
#'   dropped (counted in the console).
#' @export
read_fd_salaries <- function(path) {
  cfg <- R65_SITES$fd
  hdr <- names(utils::read.csv(path, nrows = 0, check.names = FALSE))
  if (!identical(hdr[seq_along(cfg$header_first)], cfg$header_first)) {
    stop(glue("R/65: unexpected header in {basename(path)}. Expected it to start ",
              "with {paste(cfg$header_first, collapse = ',')}; got ",
              "{paste(utils::head(hdr, 4), collapse = ',')}. Not guessing columns."),
         call. = FALSE)
  }
  nf <- utils::count.fields(path, sep = ",", quote = "\"", blank.lines.skip = TRUE)[-1]
  if (max(nf) > length(hdr)) {
    stop("R/65: a row has more fields than the header (", max(nf), " > ", length(hdr),
         "). Not guessing columns.", call. = FALSE)
  }
  raw <- utils::read.csv(path, header = TRUE, colClasses = "character", fill = TRUE,
                         col.names = hdr, check.names = FALSE, strip.white = TRUE,
                         na.strings = character(0))
  if (nrow(raw) != length(nf)) {
    stop("R/65: row count mismatch after parsing (", nrow(raw), " vs ", length(nf), ").",
         call. = FALSE)
  }

  is_dst <- grepl("D/ST$", raw$player)
  full_n <- length(hdr)
  raw <- raw %>%
    dplyr::mutate(
      .nf       = nf,
      row_type  = dplyr::case_when(is_dst ~ "dst",
                                   .nf == full_n ~ "player",
                                   TRUE ~ "other_shape"),
      salary_num = suppressWarnings(as.integer(gsub("[$,]", "", .data$salary))),
      # The file's own projection. Enters the objective only through the V3
      # blend (.r65_apply_blend, R/71 gate). Player rows: column "fantasy".
      # D/ST rows are shifted: field 12.
      file_proj = suppressWarnings(as.numeric(dplyr::if_else(
        row_type == "dst", .data[[hdr[12]]],
        if ("fantasy" %in% hdr) .data[["fantasy"]] else NA_character_)))
    )

  n_all   <- nrow(raw)
  no_sal  <- is.na(raw$salary_num)
  message(glue("  {basename(path)}: {n_all} rows -- player {sum(raw$row_type == 'player')}, ",
               "D/ST {sum(raw$row_type == 'dst')}, other shape {sum(raw$row_type == 'other_shape')}"))
  message(glue("  Dropped for no salary (N/A): {sum(no_sal)} ",
               "({sum(no_sal & raw$row_type == 'other_shape')} kicker-shaped, ",
               "{sum(no_sal & raw$row_type == 'player')} skill players not on the slate)"))
  odd <- raw %>% dplyr::filter(!no_sal, row_type == "other_shape")
  if (nrow(odd) > 0) {
    print(odd %>% dplyr::select(player, team, salary))
    stop("R/65: ", nrow(odd), " row(s) with a salary but an unrecognized shape. ",
         "Not guessing which slot they fill.", call. = FALSE)
  }
  sal <- raw %>% dplyr::filter(!no_sal)

  # Team: full name -> nflverse abbreviation (stop on any miss)
  teams <- .r65_load_teams() %>% dplyr::select(team_abbr, team_name)
  sal <- sal %>% dplyr::left_join(teams, by = c("team" = "team_name"),
                                  relationship = "many-to-one")
  miss <- unique(sal$team[is.na(sal$team_abbr)])
  if (length(miss) > 0) stop("R/65: team name(s) not in nflreadr::load_teams(): ",
                             paste(miss, collapse = ", "), call. = FALSE)

  # Game: "AWAY @ HOME" -> opponent, home flag (stop if the team is not in its game)
  parts <- strsplit(sal$gameInfo, "\\s*@\\s*")
  bad_g <- vapply(parts, length, integer(1)) != 2L
  if (any(bad_g)) stop("R/65: unparseable gameInfo: ",
                       paste(unique(sal$gameInfo[bad_g]), collapse = ", "), call. = FALSE)
  # (local names differ from the output columns: tibble() would otherwise read
  # the just-built `home` column instead of these vectors)
  away_ab <- .r65_clean_abbr(vapply(parts, `[`, character(1), 1))
  home_ab <- .r65_clean_abbr(vapply(parts, `[`, character(1), 2))
  team_ab <- .r65_clean_abbr(sal$team_abbr)
  in_game <- team_ab == away_ab | team_ab == home_ab
  if (!all(in_game)) {
    x <- unique(data.frame(team = sal$team[!in_game], game = sal$gameInfo[!in_game]))
    print(x)
    stop("R/65: team abbreviation does not appear in its gameInfo for the rows ",
         "above. Add the code to R65_ABBR_ALIASES only after checking it.", call. = FALSE)
  }

  out <- tibble::tibble(
    site_name = sal$player,
    row_type  = sal$row_type,
    team_full = sal$team,
    team      = team_ab,
    opponent  = dplyr::if_else(team_ab == home_ab, away_ab, home_ab),
    home      = team_ab == home_ab,
    game      = paste(away_ab, "@", home_ab),
    salary    = sal$salary_num,
    file_proj = sal$file_proj
  )
  message(glue("  Slate: {dplyr::n_distinct(out$game)} games, {dplyr::n_distinct(out$team)} teams, ",
               "{sum(out$row_type == 'player')} players + {sum(out$row_type == 'dst')} D/ST with salaries"))
  out
}

# Every slate game must be a scheduled REG game in `week`. If not, say which
# week the file does match, and stop.
.r65_check_week <- function(sal, season, week) {
  sch <- .r65_load_schedules(season) %>%
    dplyr::filter(.data$game_type == "REG") %>%
    dplyr::mutate(away = .r65_clean_abbr(away_team), home = .r65_clean_abbr(home_team),
                  game = paste(away, "@", home))
  games <- unique(sal$game)
  wk <- sch %>% dplyr::filter(.data$week == !!week)
  missing <- setdiff(games, wk$game)
  if (length(missing) > 0) {
    fits <- sch %>% dplyr::filter(game %in% games) %>% dplyr::count(week) %>%
      dplyr::filter(n == length(games)) %>% dplyr::pull(week)
    stop(glue("R/65: {length(missing)} slate game(s) are not week {week} games ",
              "({paste(utils::head(missing, 4), collapse = '; ')}). ",
              "{if (length(fits) == 1) paste0('This file matches week ', fits, '.') else 'No single week matches every game.'}"),
         call. = FALSE)
  }
  message(glue("  Slate check: all {length(games)} games are {season} week {week} games"))
  invisible(TRUE)
}

# ==============================================================================
# PART 3: MATCHING
# ==============================================================================

.r65_read_overrides <- function(site, path = R65_OVERRIDE_PATH) {
  empty <- tibble::tibble(site_name = character(), team = character(), nfl_gsis_id = character())
  if (!file.exists(path)) {
    message(glue("  No overrides file at {basename(path)} (optional; columns site, site_name, team, nfl_gsis_id)"))
    return(empty)
  }
  ov <- readr::read_csv(path, col_types = readr::cols(.default = "c"))
  need <- c("site", "site_name", "team", "nfl_gsis_id")
  if (!all(need %in% names(ov))) {
    stop("R/65: overrides file needs columns ", paste(need, collapse = ", "), call. = FALSE)
  }
  ov <- ov %>%
    dplyr::filter(.data$site == !!site) %>%
    dplyr::transmute(site_name = trimws(site_name), team = .r65_clean_abbr(team),
                     nfl_gsis_id = trimws(nfl_gsis_id))
  dup <- ov %>% dplyr::count(site_name, team) %>% dplyr::filter(n > 1)
  if (nrow(dup) > 0) stop("R/65: duplicate override rows for: ",
                          paste(dup$site_name, collapse = ", "), call. = FALSE)
  message(glue("  Overrides: {nrow(ov)} row(s) for site '{site}'"))
  ov
}

# ------------------------------------------------------------------------------
# .r65_qb_roles  [2026-10-09]
#
# WHY. R/59 mu is a player's per-game value WHEN HE PLAYS. Nothing upstream
# says who starts this week, so on the week 5 FanDuel slate every backup QB
# with a starter's history (Dobbs 17.0, Trubisky 16.2, Lock 16.0, Richardson
# 14.9, Mac Jones 14.6; all 0 games in 2026) became the best value on the
# board. RB/WR/TE did not show the problem (1 eligible player projected >= 5
# with a site projection < 1), so this is QB-only.
#
# RULE (fork A, 2026-10-09). From 2026 REG pbp before the slate week:
#   1. starter = the QB with the most dropbacks (passes, sacks, scrambles) in
#      the team's most recent game                          basis "last_game"
#   2. if that QB is not available (availability not active/questionable),
#      starter = the available QB on the team with the most dropbacks this
#      season                                      basis "fallback_season"
#   3. if the last-game starter has no row in our projections at all, no
#      backup is promoted: every team QB is "unknown"  basis "starter_not_in_pool"
#   4. if no available QB has a 2026 dropback, every team QB is "unknown"
#                                                           basis "no_snaps"
#   Every other QB on a team with a starter is "backup", excluded from the
#   solve as qb_backup. A lock overrides it (late benchings are what locks
#   are for). Projections are not changed.
#
# @param bands R/59 bands for the slate week (all positions; QBs used).
# @return Tibble player_uid, qb_role, qb_role_basis for every QB in bands.
# ------------------------------------------------------------------------------
.r65_qb_roles <- function(bands) {
  season <- unique(as.integer(bands$season))
  week   <- unique(as.integer(bands$week))
  if (length(season) != 1L || length(week) != 1L) {
    stop("R/65: bands carry more than one season or week; cannot set QB roles.", call. = FALSE)
  }
  qbs <- bands %>%
    dplyr::filter(position == "QB") %>%
    dplyr::mutate(team = .r65_clean_abbr(team),
                  available = availability %in% c("active", "questionable")) %>%
    dplyr::filter(!is.na(team)) %>%
    dplyr::select(player_uid, nfl_gsis_id, player_name, team, availability, available)

  pbp <- tryCatch(load_normalized_season(season), error = function(e) NULL)
  need <- c("season_type", "week", "game_id", "posteam", "qb_dropback",
            "passer_player_id", "rusher_player_id")
  if (is.null(pbp) || !all(need %in% names(pbp))) {
    message("  QB roles: 2026 pbp unavailable or missing columns; every QB left 'unknown'.")
    return(qbs %>% dplyr::transmute(player_uid, qb_role = "unknown", qb_role_basis = "no_pbp"))
  }
  db <- pbp %>%
    dplyr::filter(season_type == "REG", week < !!week,
                  dplyr::coalesce(as.numeric(qb_dropback), 0) == 1, !is.na(posteam)) %>%
    dplyr::mutate(qb_id = dplyr::coalesce(passer_player_id, rusher_player_id),
                  team = .r65_clean_abbr(posteam)) %>%
    dplyr::filter(!is.na(qb_id)) %>%
    dplyr::count(team, week, game_id, qb_id, name = "dropbacks")
  rm(pbp)

  last_start <- db %>%
    dplyr::group_by(team) %>%
    dplyr::filter(week == max(week)) %>%
    dplyr::slice_max(dropbacks, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(team, last_id = qb_id, last_week = week)
  season_db <- db %>% dplyr::count(team, qb_id, wt = dropbacks, name = "season_dropbacks")

  roles <- purrr::map_dfr(split(qbs, qbs$team), function(q) {
    t  <- q$team[1]
    # [2026-10-22] Whole team on bye: not on the slate, so no role is needed.
    # Labelled explicitly; before v2 it fell through to 'no_snaps'.
    if (all(q$availability %in% "bye")) {
      return(q %>% dplyr::transmute(player_uid, qb_role = "bye", qb_role_basis = "bye"))
    }
    ls <- last_start %>% dplyr::filter(team == t)
    starter <- NA_character_; basis <- "no_snaps"
    if (nrow(ls) == 1L) {
      hit <- q %>% dplyr::filter(nfl_gsis_id %in% ls$last_id)
      if (nrow(hit) == 0L) {
        basis <- "starter_not_in_pool"
      } else if (hit$available[1]) {
        starter <- hit$player_uid[1]; basis <- "last_game"
      } else {
        fb <- q %>% dplyr::filter(available) %>%
          dplyr::left_join(season_db %>% dplyr::filter(team == t),
                           by = c("nfl_gsis_id" = "qb_id", "team")) %>%
          dplyr::filter(dplyr::coalesce(season_dropbacks, 0L) > 0) %>%
          dplyr::arrange(dplyr::desc(season_dropbacks))
        if (nrow(fb) > 0L) { starter <- fb$player_uid[1]; basis <- "fallback_season" }
      }
    }
    q %>% dplyr::transmute(
      player_uid,
      qb_role = dplyr::case_when(is.na(starter) ~ "unknown",
                                 player_uid == starter ~ "starter",
                                 TRUE ~ "backup"),
      qb_role_basis = basis)
  })

  st <- roles %>% dplyr::filter(qb_role == "starter") %>%
    dplyr::left_join(qbs %>% dplyr::select(player_uid, player_name, team), by = "player_uid")
  unk <- roles %>% dplyr::filter(qb_role == "unknown") %>%
    dplyr::left_join(qbs %>% dplyr::select(player_uid, team), by = "player_uid") %>%
    dplyr::distinct(team, qb_role_basis)
  n_bye_teams <- roles %>% dplyr::filter(qb_role == "bye") %>%
    dplyr::left_join(qbs %>% dplyr::select(player_uid, team), by = "player_uid") %>%
    dplyr::distinct(team) %>% nrow()
  message(glue("  QB roles (pbp weeks < {week}): {nrow(st)} starters, ",
               "{sum(roles$qb_role == 'backup')} backups excluded, ",
               "{nrow(unk)} team(s) unknown, {n_bye_teams} team(s) on bye"))
  fb <- st %>% dplyr::filter(qb_role_basis == "fallback_season")
  if (nrow(fb) > 0) message(glue("    starter by injury fallback: ",
                                 "{paste0(fb$player_name, ' (', fb$team, ')', collapse = ', ')}"))
  if (nrow(unk) > 0) message(glue("    unknown (no QB excluded): ",
                                  "{paste0(unk$team, ' [', unk$qb_role_basis, ']', collapse = ', ')}"))
  roles
}

# ------------------------------------------------------------------------------
# .r65_read_r29_context  [2026-10-22]
#
# Display-only context from the site's R/29 file (written by run_dfs_chain):
# the prior, the 2026 per-game mean and its game count. Lets a stale-prior
# projection (Deshaun Watson, week 7: prior 25.5, 2026 mean 15.2 over 6 games,
# linear YTD weight 0.335 -> R/29 22.0) be seen on the board. R/58 gated the
# YTD weight rule and it FAILED at QB twice, so this does not change the
# projection; a stale-prior QB gate is queued separately.
#
# @return Tibble player_uid, prior_mu, ytd_mean, ytd_n (empty, with a message,
#   if the file or a column is missing; duplicated player_uid rows dropped).
# ------------------------------------------------------------------------------
.r65_read_r29_context <- function(cfg) {
  empty <- tibble::tibble(player_uid = character(), prior_mu = numeric(),
                          ytd_mean = numeric(), ytd_n = numeric())
  p <- file.path(R65_CHAIN_CACHE, paste0(cfg$prefix, "player_projections.csv"))
  if (!file.exists(p)) {
    message(glue("  R/29 context: {basename(p)} not found; prior/YTD columns left NA"))
    return(empty)
  }
  r <- readr::read_csv(p, col_types = readr::cols(.default = "c"), progress = FALSE)
  need <- c("player_uid", "prior_mu", "ytd_mean", "ytd_n")
  if (!all(need %in% names(r))) {
    message(glue("  R/29 context: {basename(p)} lacks ",
                 "{paste(setdiff(need, names(r)), collapse = ', ')}; prior/YTD columns left NA"))
    return(empty)
  }
  r <- r %>%
    dplyr::filter(!is.na(player_uid)) %>%
    dplyr::transmute(player_uid,
                     prior_mu = suppressWarnings(as.numeric(prior_mu)),
                     ytd_mean = suppressWarnings(as.numeric(ytd_mean)),
                     ytd_n    = suppressWarnings(as.numeric(ytd_n)))
  dup <- unique(r$player_uid[duplicated(r$player_uid)])
  if (length(dup) > 0) {
    message(glue("  R/29 context: {length(dup)} duplicated player_uid(s) left out of the join"))
    r <- r %>% dplyr::filter(!player_uid %in% dup)
  }
  r
}

# ------------------------------------------------------------------------------
# [2026-11-05] V3 FanDuel projection blend (gate table from R/71)
# ------------------------------------------------------------------------------

#' Read the R/71 blend gate; NULL (ours only) unless it is valid and passed
#' @keywords internal
.r65_read_blend <- function(path = R65_BLEND_PATH, site = "fd") {
  if (!file.exists(path)) {
    message("  FD blend OFF: no gate file (run R/71 run_dfs_blend_gate()); using our projection")
    return(NULL)
  }
  g <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(g) || !identical(g$schema_tag, R65_BLEND_SCHEMA) || !identical(g$site, site) ||
      !identical(g$scoring_name, R65_SITES[[site]]$scoring_name)) {
    message("  FD blend OFF: gate file unreadable, wrong schema, or wrong site/scoring; using our projection")
    return(NULL)
  }
  if (!isTRUE(g$use)) {
    message(glue("  FD blend OFF: R/71 gate did not pass ({g$reason}; built ",
                 "{format(g$built_at, '%Y-%m-%d')}, weeks {paste(g$weeks, collapse = ',')}); ",
                 "using our projection"))
    return(NULL)
  }
  if (!is.numeric(g$w) || length(g$w) != 1L || !is.finite(g$w) || g$w < 0 || g$w > 1) {
    message("  FD blend OFF: gate passed but its weight is not a number in [0, 1]; using our projection")
    return(NULL)
  }
  message(glue("  FD blend ON: {g$w} ours + {1 - g$w} FanDuel (R/71 gate built ",
               "{format(g$built_at, '%Y-%m-%d')}, weeks {paste(g$weeks, collapse = ',')})"))
  g
}

#' Apply the blend to matched rows that have both projections
#' @return pool with proj blended, plus proj_ours (always) and blend_w (NA = ours).
#' @keywords internal
.r65_apply_blend <- function(pool, g) {
  pool <- pool %>% dplyr::mutate(proj_ours = proj, blend_w = NA_real_)
  if (is.null(g)) return(pool)
  matched <- pool$match_status %in% c("matched", "override", "dst")
  ok <- matched & is.finite(pool$proj) & is.finite(pool$file_proj)
  pool$proj[ok] <- g$w * pool$proj_ours[ok] + (1 - g$w) * pool$file_proj[ok]
  pool$blend_w[ok] <- g$w
  message(glue("  FD blend applied to {sum(ok)} of {sum(matched)} matched rows; ",
               "{sum(matched & !ok)} keep our projection (no FanDuel number or no projection)"))
  pool
}

#' Match salary rows to the chain output
#'
#' @param r29_ctx Tibble from .r65_read_r29_context(), or NULL (columns NA).
#' @return Tibble, one row per salary row, with match_status in
#'   matched / override / no_projection / ambiguous / dst / dst_no_projection,
#'   plus nfl_gsis_id, player_name, position, proj, availability, and the
#'   display-only prior_mu, ytd_mean, ytd_n, ytd_gap; proj is the V3 blend when
#'   the R/71 gate passes, with proj_ours and blend_w alongside.
#' @param blend Gate from .r65_read_blend(), or NULL for our projection only.
#' @keywords internal
.r65_match <- function(sal, bands, def, overrides, r29_ctx = NULL, blend = .r65_read_blend()) {
  b <- bands %>%
    dplyr::mutate(team = .r65_clean_abbr(team),
                  key  = paste0(.pidr_normalize_name(player_name), "|", team))
  dup_keys <- b$key[duplicated(b$key)]
  b_unique <- b %>% dplyr::filter(!key %in% dup_keys)
  bcols <- c("player_uid", "nfl_gsis_id", "player_name", "position", "mu", "sigma_wk", "p95",
             "availability", "playable", "opponent", "on_bye")

  pl <- sal %>%
    dplyr::filter(row_type == "player") %>%
    dplyr::mutate(key = paste0(.pidr_normalize_name(site_name), "|", team)) %>%
    dplyr::left_join(overrides %>% dplyr::rename(ov_gsis = nfl_gsis_id),
                     by = c("site_name", "team"), relationship = "many-to-one")

  by_ov  <- pl %>% dplyr::filter(!is.na(ov_gsis)) %>%
    dplyr::left_join(b %>% dplyr::select(dplyr::all_of(bcols)) %>%
                       dplyr::rename(our_opponent = opponent),
                     by = c("ov_gsis" = "nfl_gsis_id"), relationship = "many-to-one") %>%
    dplyr::mutate(nfl_gsis_id = ov_gsis,
                  match_status = dplyr::if_else(is.na(player_uid), "override_not_found", "override"))
  by_key <- pl %>% dplyr::filter(is.na(ov_gsis)) %>%
    dplyr::left_join(b_unique %>% dplyr::select(key, dplyr::all_of(bcols)) %>%
                       dplyr::rename(our_opponent = opponent),
                     by = "key", relationship = "many-to-one") %>%
    dplyr::mutate(match_status = dplyr::case_when(
      !is.na(player_uid) ~ "matched",
      key %in% dup_keys ~ "ambiguous",
      TRUE ~ "no_projection"))

  players <- dplyr::bind_rows(by_ov, by_key) %>%
    dplyr::mutate(proj = mu) %>%
    dplyr::select(-key, -ov_gsis)

  # Two salary rows on one player would let the solver roster him twice.
  # player_uid (never NA in R/59) is the identity; nfl_gsis_id is NA for the
  # id-less rookie cohort.
  ids <- players$player_uid[players$match_status %in% c("matched", "override") &
                              !is.na(players$player_uid)]
  dup_ids <- unique(ids[duplicated(ids)])
  if (length(dup_ids) > 0) {
    print(players %>% dplyr::filter(player_uid %in% dup_ids) %>%
            dplyr::select(site_name, team, salary, player_uid, player_name, match_status))
    stop("R/65: ", length(dup_ids), " player id(s) matched by more than one salary row ",
         "(above). Fix with the overrides file; not solving.", call. = FALSE)
  }

  d <- def %>% dplyr::mutate(team = .r65_clean_abbr(team)) %>%
    dplyr::select(team, def_proj_ppg, vegas_shift)
  dst <- sal %>%
    dplyr::filter(row_type == "dst") %>%
    dplyr::left_join(d, by = "team", relationship = "many-to-one") %>%
    dplyr::mutate(position = "DEF", player_uid = paste0("DEF_", team), nfl_gsis_id = NA_character_,
                  player_name = site_name, proj = def_proj_ppg,
                  availability = "active", playable = TRUE,
                  our_opponent = opponent,
                  match_status = dplyr::if_else(is.na(proj), "dst_no_projection", "dst"))

  # [2026-10-09] QB starter/backup role (see .r65_qb_roles).
  # [2026-10-22] R/29 context, display only.
  if (is.null(r29_ctx)) {
    r29_ctx <- tibble::tibble(player_uid = character(), prior_mu = numeric(),
                              ytd_mean = numeric(), ytd_n = numeric())
  }
  dplyr::bind_rows(players, dst) %>%
    dplyr::left_join(.r65_qb_roles(bands), by = "player_uid", relationship = "many-to-one") %>%
    dplyr::left_join(r29_ctx, by = "player_uid", relationship = "many-to-one") %>%
    dplyr::mutate(ytd_gap = round(ytd_mean - prior_mu, 2)) %>%
    .r65_apply_blend(blend)
}

# Eligibility: matched, a slot position, a projection, available.
.r65_eligibility <- function(pool, include_q) {
  ok_status <- if (include_q) c("active", "questionable") else "active"
  pool %>%
    dplyr::mutate(
      excluded_reason = dplyr::case_when(
        !match_status %in% c("matched", "override", "dst") ~ match_status,
        !position %in% c("QB", "RB", "WR", "TE", "DEF") ~ paste0("position:", position),
        is.na(proj) ~ "no_projection",
        on_bye %in% TRUE ~ "bye",
        availability == "questionable" & !include_q ~ "questionable",
        !availability %in% ok_status ~ paste0("availability:", availability),
        position == "QB" & qb_role %in% "backup" ~ "qb_backup",
        TRUE ~ NA_character_),
      eligible = is.na(excluded_reason),
      value = round(proj / (salary / 1000), 3)
    )
}

# ==============================================================================
# PART 4: SOLVER
# ==============================================================================

# Resolve user-supplied names or gsis ids to pool rows. Errors on miss/ambiguity.
.r65_resolve_names <- function(x, pool, what) {
  if (length(x) == 0) return(integer(0))
  norm <- .pidr_normalize_name(pool$site_name)
  vapply(x, function(v) {
    hit <- which(pool$nfl_gsis_id %in% v | pool$player_uid %in% v |
                   norm == .pidr_normalize_name(v))
    if (length(hit) == 0) stop(glue("R/65: {what} '{v}' is not on this slate."), call. = FALSE)
    if (length(hit) > 1) stop(glue("R/65: {what} '{v}' is ambiguous ",
                                   "({paste(pool$site_name[hit], pool$team[hit], collapse = '; ')}). ",
                                   "Pass the player_uid or nfl_gsis_id instead."), call. = FALSE)
    hit
  }, integer(1))
}

# ------------------------------------------------------------------------------
# USER RULES  [2026-10-22]
# ------------------------------------------------------------------------------

#' Lineup rules the user sets (all constraints, no projection changes)
#'
#' @param cfg Site config (default FanDuel).
#' @param max_per_team Integer or NULL (site limit). 1..cfg$max_per_team.
#' @param max_team_share Numeric in (0, 1] or NULL (off). Max share of the
#'   lineup's projected points from one team; DEF counts toward its team.
#' @param qb_stack Integer 0, 1 or 2. WR/TE from the QB's team required with him.
#' @param bring_back Logical. At least one non-DEF opponent player with the QB.
#' @param no_def_conflict Logical. No non-DEF player facing your DEF.
#' @return Validated list.
#' @export
dfs_rules <- function(cfg = .r65_site("fd"), max_per_team = NULL, max_team_share = NULL,
                      qb_stack = 0L, bring_back = FALSE, no_def_conflict = FALSE) {
  if (is.null(max_per_team)) max_per_team <- cfg$max_per_team
  max_per_team <- as.integer(max_per_team)
  qb_stack <- as.integer(qb_stack)
  if (length(max_per_team) != 1L || is.na(max_per_team) ||
      max_per_team < 1L || max_per_team > cfg$max_per_team) {
    stop(glue("R/65: max_per_team must be 1 to {cfg$max_per_team} (the site limit)."), call. = FALSE)
  }
  if (!is.null(max_team_share)) {
    if (length(max_team_share) != 1L || is.na(max_team_share) ||
        max_team_share <= 0 || max_team_share > 1) {
      stop("R/65: max_team_share must be in (0, 1], or NULL for off.", call. = FALSE)
    }
    if (max_team_share >= 1) max_team_share <- NULL     # 100 percent is off
  }
  if (length(qb_stack) != 1L || is.na(qb_stack) || !qb_stack %in% 0:2) {
    stop("R/65: qb_stack must be 0, 1 or 2.", call. = FALSE)
  }
  if (qb_stack > 0L && qb_stack + 1L > max_per_team) {
    stop(glue("R/65: a QB stack of {qb_stack} needs {qb_stack + 1} players from one team, ",
              "but max_per_team is {max_per_team}."), call. = FALSE)
  }
  list(max_per_team = max_per_team, max_team_share = max_team_share,
       qb_stack = qb_stack, bring_back = isTRUE(bring_back),
       no_def_conflict = isTRUE(no_def_conflict))
}

# Plain-language list of the rules tighter than the site's own.
.r65_rules_text <- function(rules, cfg) {
  x <- character(0)
  if (rules$max_per_team < cfg$max_per_team) x <- c(x, glue("max {rules$max_per_team} per team"))
  if (!is.null(rules$max_team_share)) {
    x <- c(x, glue("max {round(100 * rules$max_team_share)}% of projected points from one team"))
  }
  if (rules$qb_stack > 0L) x <- c(x, glue("QB stack {rules$qb_stack} (WR/TE)"))
  if (rules$bring_back) x <- c(x, "bring-back")
  if (rules$no_def_conflict) x <- c(x, "no players facing your DEF")
  if (length(x) == 0) "site rules only" else paste(x, collapse = "; ")
}

.r65_candidates <- function(pool, lock_idx = integer(0), exclude_idx = integer(0)) {
  cand <- pool$eligible
  cand[lock_idx] <- TRUE                 # a lock overrides the Q / qb_backup default (hard-out checked earlier)
  cand[exclude_idx] <- FALSE
  cand
}

#' QBs the stacking rules make unusable on this pool (review focus: with
#' questionable players excluded a QB's catchers can all be gone, and the
#' solver would drop him silently).
#' @return Tibble site_name, team, salary, proj, reason.
#' @export
rule_blocked_qbs <- function(pool, rules, lock_idx = integer(0), exclude_idx = integer(0)) {
  none <- tibble::tibble(site_name = character(), team = character(), salary = integer(),
                         proj = numeric(), reason = character())
  if (rules$qb_stack == 0L && !rules$bring_back) return(none)
  P <- pool[.r65_candidates(pool, lock_idx, exclude_idx), ]
  qbs <- P %>% dplyr::filter(position == "QB")
  if (nrow(qbs) == 0) return(none)
  catchers <- P %>% dplyr::filter(position %in% c("WR", "TE")) %>% dplyr::count(team, name = "n_catchers")
  opp_pl   <- P %>% dplyr::filter(position != "DEF") %>% dplyr::count(team, name = "n_opp")
  qbs %>%
    dplyr::left_join(catchers, by = "team") %>%
    dplyr::left_join(opp_pl, by = c("opponent" = "team")) %>%
    dplyr::mutate(
      n_catchers = dplyr::coalesce(n_catchers, 0L),
      n_opp = dplyr::coalesce(n_opp, 0L),
      reason = dplyr::case_when(
        rules$qb_stack > 0L & n_catchers < rules$qb_stack ~
          paste0("only ", n_catchers, " eligible WR/TE for a ", rules$qb_stack, "-player stack"),
        rules$bring_back & n_opp == 0L ~ "no eligible opponent player for a bring-back",
        TRUE ~ NA_character_)) %>%
    dplyr::filter(!is.na(reason)) %>%
    dplyr::select(site_name, team, salary, proj, reason)
}

# Feasibility of the lock set before the solver runs. Returns problem strings.
.r65_lock_problems <- function(pool, lock_idx, cfg, rules = dfs_rules(cfg)) {
  if (length(lock_idx) == 0) return(character(0))
  L <- pool[lock_idx, ]
  p <- character(0)
  bad <- L %>% dplyr::filter(!match_status %in% c("matched", "override", "dst") | is.na(proj))
  if (nrow(bad) > 0) p <- c(p, glue("locked with no projection: {paste(bad$site_name, collapse = ', ')}"))
  # Questionable players and QB backups may be locked; everything else may not.
  hard_out <- L %>% dplyr::filter(!is.na(excluded_reason) &
                                    !excluded_reason %in% c("questionable", "qb_backup"))
  if (nrow(hard_out) > 0) p <- c(p, glue("locked but not available: ",
                                         "{paste(hard_out$site_name, ' (', hard_out$excluded_reason, ')', sep = '', collapse = ', ')}"))
  n <- table(factor(L$position, levels = c("QB", "RB", "WR", "TE", "DEF")))
  s <- cfg$slots
  maxes <- c(QB = s[["QB"]], RB = s[["RB"]] + s[["FLEX"]], WR = s[["WR"]] + s[["FLEX"]],
             TE = s[["TE"]] + s[["FLEX"]], DEF = s[["DEF"]])
  over <- names(maxes)[n[names(maxes)] > maxes]
  if (length(over) > 0) p <- c(p, glue("too many locked at {paste(over, collapse = ', ')}"))
  flex_n <- sum(n[cfg$flex]); flex_max <- sum(s[cfg$flex]) + s[["FLEX"]]
  if (flex_n > flex_max) p <- c(p, glue("{flex_n} locked RB/WR/TE; only {flex_max} slots"))
  if (sum(L$salary) > cfg$cap) p <- c(p, glue("locked salary ${sum(L$salary)} exceeds the ${cfg$cap} cap"))
  tm <- L %>% dplyr::count(team) %>% dplyr::filter(n > rules$max_per_team)
  if (nrow(tm) > 0) p <- c(p, glue("more than {rules$max_per_team} locked from {paste(tm$team, collapse = ', ')}"))
  # [2026-10-22] a locked QB the stacking rules cannot satisfy
  blk <- rule_blocked_qbs(pool, rules, lock_idx) %>%
    dplyr::filter(site_name %in% L$site_name[L$position == "QB"])
  if (nrow(blk) > 0) p <- c(p, glue("locked QB {blk$site_name} ({blk$team}): {blk$reason}"))
  p
}

#' Solve one lineup
#'
#' @param pool Tibble from .r65_eligibility(): eligible rows plus any locked
#'   questionable rows are candidates.
#' @param cfg Site config.
#' @param lock_idx,exclude_idx Integer row indices into pool.
#' @param rules List from dfs_rules(). Defaults reproduce v1.
#' @param cuts List of player_uid vectors (earlier lineups); the solution must
#'   share at most (slots - min_diff) players with each.
#' @param min_diff Integer, used with cuts.
#' @return list(status, lineup, total_proj, total_salary, team_share, rules)
#' @export
solve_dfs_lineup <- function(pool, cfg, lock_idx = integer(0), exclude_idx = integer(0),
                             rules = dfs_rules(cfg), cuts = list(), min_diff = 1L) {
  cand <- .r65_candidates(pool, lock_idx, exclude_idx)
  P <- pool[cand, ]
  is_lock <- which(cand) %in% lock_idx
  n <- nrow(P)
  teams <- sort(unique(P$team)); nt <- length(teams)
  s <- cfg$slots
  total <- sum(s)
  pos <- P$position

  row <- function(xcoef, ycoef = rep(0, nt)) c(xcoef, ycoef)
  A <- list(); dir <- character(0); rhs <- numeric(0)
  add <- function(a, d, r) { A[[length(A) + 1]] <<- a; dir <<- c(dir, d); rhs <<- c(rhs, r) }
  ind <- function(cond) as.numeric(cond)

  add(row(rep(1, n)), "=", total)
  add(row(ind(pos == "QB")),  "=",  s[["QB"]])
  add(row(ind(pos == "DEF")), "=",  s[["DEF"]])
  for (pp in cfg$flex) {
    add(row(ind(pos == pp)), ">=", s[[pp]])
    add(row(ind(pos == pp)), "<=", s[[pp]] + s[["FLEX"]])
  }
  add(row(ind(pos %in% cfg$flex)), "=", sum(s[cfg$flex]) + s[["FLEX"]])
  add(row(P$salary), "<=", cfg$cap)
  for (j in seq_len(nt)) {
    add(row(ind(P$team == teams[j])), "<=", rules$max_per_team)       # v1: cfg$max_per_team
    yj <- rep(0, nt); yj[j] <- 1
    add(row(-ind(P$team == teams[j]), yj), "<=", 0)   # y_t <= players from t
    # [2026-10-22] F1: share of projected points from team t <= s.
    # Projections floored at 0 for this row only (a negative DEF projection
    # would otherwise make "share" meaningless); the objective is unchanged.
    if (!is.null(rules$max_team_share)) {
      add(row(pmax(P$proj, 0) * (ind(P$team == teams[j]) - rules$max_team_share)), "<=", 0)
    }
  }
  add(row(rep(0, n), rep(1, nt)), ">=", cfg$min_teams)
  for (i in which(is_lock)) { a <- rep(0, n); a[i] <- 1; add(row(a), "=", 1) }

  # [2026-10-22] F3: stacking rules
  for (q in which(pos == "QB")) {
    if (rules$qb_stack > 0L) {
      a <- ind(pos %in% c("WR", "TE") & P$team == P$team[q]); a[q] <- -rules$qb_stack
      add(row(a), ">=", 0)
    }
    if (rules$bring_back) {
      a <- ind(P$team == P$opponent[q] & pos != "DEF"); a[q] <- -1
      add(row(a), ">=", 0)
    }
  }
  if (rules$no_def_conflict) {
    M <- total - s[["DEF"]]
    for (d in which(pos == "DEF")) {
      a <- ind(P$team == P$opponent[d] & pos != "DEF"); a[d] <- M
      add(row(a), "<=", M)
    }
  }
  # [2026-10-22] F2: no-good cuts against earlier lineups (DEF row included)
  for (cut in cuts) {
    add(row(ind(P$player_uid %in% cut)), "<=", total - min_diff)
  }

  # [2026-10-22] Solver: GLPK (Rglpk). lpSolve::lp() returned suboptimal
  # lineups on the week 7 slate: its 121.33 answer was beaten by 4.36 points
  # with Jahmyr Gibbs EXCLUDED (a smaller feasible set cannot have a higher
  # optimum), five of nine swap costs were negative, and adding a max-3-per-
  # team rule also raised the total. Same matrix, same objective; only the
  # solver changed. lpSolve is kept, not called, for comparison runs:
  #   lpSolve::lp("max", c(P$proj, rep(0, nt)), do.call(rbind, A), dir, rhs, all.bin = TRUE)
  # Every console and app solve is also checked by .r65_assert_optimal().
  glpk_dir <- dplyr::if_else(dir == "=", "==", dir)
  sol <- Rglpk::Rglpk_solve_LP(obj = c(P$proj, rep(0, nt)),
                               mat = do.call(rbind, A), dir = glpk_dir, rhs = rhs,
                               types = rep("B", n + nt), max = TRUE)
  if (sol$status != 0) {
    return(list(status = sol$status, lineup = NULL, total_proj = NA, total_salary = NA,
                team_share = NULL, rules = rules))
  }
  chosen <- round(sol$solution[seq_len(n)]) == 1
  pick <- P[chosen, ] %>% dplyr::mutate(locked = is_lock[chosen])

  # Slot labels: the FLEX is the surplus RB/WR/TE; within that position the
  # lowest projection goes to FLEX (display only; FanDuel scores slots equally).
  cnt <- table(factor(pick$position, levels = cfg$flex))
  surplus <- names(cnt)[cnt > s[cfg$flex]]
  pick <- pick %>%
    dplyr::arrange(match(position, c("QB", "RB", "WR", "TE", "DEF")), dplyr::desc(proj)) %>%
    dplyr::group_by(position) %>%
    dplyr::mutate(slot = dplyr::if_else(position %in% surplus & dplyr::row_number() == dplyr::n(),
                                        "FLEX", position)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(match(slot, c("QB", "RB", "WR", "TE", "FLEX", "DEF")), dplyr::desc(proj))

  tp <- sum(pick$proj)
  sh <- pick %>% dplyr::group_by(team) %>%
    dplyr::summarise(pts = sum(pmax(proj, 0)), .groups = "drop") %>%   # same floor as the share row
    dplyr::arrange(dplyr::desc(pts))
  share_den <- sum(sh$pts)
  list(status = 0L, lineup = pick, total_proj = tp, total_salary = sum(pick$salary),
       team_share = stats::setNames(if (share_den > 0) round(sh$pts / share_den, 3) else rep(NA_real_, nrow(sh)),
                                    sh$team),
       rules = rules)
}

#' Top-N lineups, each differing from every earlier one by >= min_diff players
#'
#' Lineup 1 is solve_dfs_lineup() with the same inputs. Stops early (fewer
#' than n rows) when no further legal lineup exists.
#' @return Tibble: rank, lineup_id, total_proj, total_salary, delta_vs_best,
#'   top_team, top_team_share, n_teams, lineup (list column of lineup tibbles).
#' @export
solve_dfs_lineups <- function(pool, cfg, lock_idx = integer(0), exclude_idx = integer(0),
                              rules = dfs_rules(cfg), n = 5L, min_diff = 1L) {
  n <- as.integer(n); min_diff <- as.integer(min_diff)
  if (is.na(n) || n < 1L) stop("R/65: n must be >= 1.", call. = FALSE)
  if (is.na(min_diff) || min_diff < 1L || min_diff > sum(cfg$slots)) {
    stop(glue("R/65: min_diff must be 1 to {sum(cfg$slots)}."), call. = FALSE)
  }
  out <- list(); cuts <- list()
  for (k in seq_len(n)) {
    r <- solve_dfs_lineup(pool, cfg, lock_idx, exclude_idx, rules, cuts, min_diff)
    if (r$status != 0) break
    out[[k]] <- r
    cuts[[k]] <- r$lineup$player_uid
  }
  if (length(out) == 0) {
    return(tibble::tibble(rank = integer(), lineup_id = character(), total_proj = numeric(),
                          total_salary = numeric(), delta_vs_best = numeric(),
                          top_team = character(), top_team_share = numeric(),
                          n_teams = integer(), lineup = list()))
  }
  tp <- purrr::map_dbl(out, "total_proj")
  tibble::tibble(
    rank           = seq_along(out),
    lineup_id      = sprintf("L%02d", seq_along(out)),
    total_proj     = round(tp, 2),
    total_salary   = purrr::map_dbl(out, "total_salary"),
    delta_vs_best  = round(tp - tp[1], 2),
    top_team       = purrr::map_chr(out, ~ names(.x$team_share)[1]),
    top_team_share = purrr::map_dbl(out, ~ unname(.x$team_share[1])),
    n_teams        = purrr::map_int(out, ~ dplyr::n_distinct(.x$lineup$team)),
    lineup         = purrr::map(out, "lineup")
  )
}

#' Swap cost for every player in a solved lineup
#'
#' cost = best total minus the best total with that player excluded (same
#' locks, excludes and rules); replacement = the players who come in (the
#' re-solve may reshuffle more than one slot). Locked players are not tested.
#' @param best List returned by solve_dfs_lineup().
#' @return Tibble player_uid, swap_cost, replacement.
#' @export
swap_costs <- function(pool, cfg, lock_idx = integer(0), exclude_idx = integer(0),
                       rules = dfs_rules(cfg), best) {
  bl <- best$lineup
  purrr::map_dfr(seq_len(nrow(bl)), function(i) {
    uid <- bl$player_uid[i]
    if (isTRUE(bl$locked[i])) {
      return(tibble::tibble(player_uid = uid, swap_cost = NA_real_, replacement = "(locked)"))
    }
    idx <- which(pool$player_uid %in% uid)
    r <- solve_dfs_lineup(pool, cfg, lock_idx, union(exclude_idx, idx), rules)
    if (r$status != 0) {
      return(tibble::tibble(player_uid = uid, swap_cost = NA_real_,
                            replacement = "(no legal lineup without him)"))
    }
    added <- setdiff(r$lineup$player_uid, bl$player_uid)
    tibble::tibble(player_uid = uid,
                   swap_cost = round(best$total_proj - r$total_proj, 2),
                   replacement = paste(r$lineup$site_name[match(added, r$lineup$player_uid)],
                                       collapse = ", "))
  })
}

#' Optimality check from the swap costs  [2026-10-22]
#'
#' Excluding a player can only shrink the feasible set, so no swap cost may be
#' negative. A negative one means the solver returned a lineup that is not the
#' best (the lpSolve failure, week 7). Returns problem strings; empty = OK.
#' @export
optimality_problems <- function(sw, best, tol = 0.01) {
  bad <- sw %>% dplyr::filter(!is.na(swap_cost), swap_cost < -tol) %>%
    dplyr::left_join(best$lineup %>% dplyr::select(player_uid, site_name), by = "player_uid")
  if (nrow(bad) == 0) return(character(0))
  glue("solver returned a lineup that is not optimal: without {bad$site_name} the best total ",
       "is {round(best$total_proj - bad$swap_cost, 2)} vs {round(best$total_proj, 2)}")
}

.r65_assert_optimal <- function(sw, best) {
  p <- optimality_problems(sw, best)
  if (length(p) > 0) {
    stop("R/65: ", paste(p, collapse = "\n  "), "\nDo not use this lineup.", call. = FALSE)
  }
  message("  Optimality check: no single-player swap beats this lineup")
  invisible(TRUE)
}

# ==============================================================================
# PART 5: ENTRY POINT
# ==============================================================================

#' Build the pool and solve DFS lineups
#'
#' @param week Integer. Slate week (must match the salary file's games).
#' @param season Integer.
#' @param site Character. "fd".
#' @param salary_path Character or NULL (newest CSV in data/dfs_salaries/).
#' @param locks,excludes Character. Salary-file names (as written in the CSV),
#'   player_uid or nfl_gsis_id values.
#' @param include_q Logical. Include questionable players. Default FALSE.
#' @param rules List from dfs_rules(), or NULL for site rules only (= v1).
#' @param n_lineups Integer. 1 = best lineup only; > 1 also solves the next
#'   best lineups (solve_dfs_lineups) and writes them.
#' @param min_diff Integer. Players each extra lineup must differ by.
#' @param write_outputs Logical. Write the pool and lineup CSVs.
#' @return Invisible list: lineup, pool, status, totals, team_share,
#'   swap (tibble), lineups (tibble or NULL), rules.
#' @export
run_dfs_optimizer <- function(week, season = 2026L, site = "fd", salary_path = NULL,
                              locks = character(0), excludes = character(0),
                              include_q = FALSE, rules = NULL, n_lineups = 1L,
                              min_diff = 1L, write_outputs = TRUE) {
  week <- as.integer(week); season <- as.integer(season)
  cfg <- .r65_site(site)
  if (site != "fd") stop("R/65 parses FanDuel files only.", call. = FALSE)
  if (is.null(rules)) rules <- dfs_rules(cfg)

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/65 OPTIMIZER: {cfg$name} -- season {season} week {week}, cap ${cfg$cap}"))
  message(glue("Rules: {.r65_rules_text(rules, cfg)}"))
  message(glue("{strrep('=', 70)}"))

  message("\nSTEP 1/4: salary file")
  if (is.null(salary_path)) salary_path <- newest_salary_file()
  message(glue("  Using {salary_path} (modified {format(file.mtime(salary_path), '%Y-%m-%d %H:%M')})"))
  sal <- read_fd_salaries(salary_path)
  .r65_check_week(sal, season, week)

  message("\nSTEP 2/4: chain output")
  bands_rds <- file.path(R65_OUTPUT_DIR, sprintf("%sr59_weekly_bands_w%02d.rds", cfg$prefix, week))
  def_rds   <- .r65_path(cfg, "def_st", week, "rds")
  for (f in c(bands_rds, def_rds)) {
    if (!file.exists(f)) stop(glue("R/65: {basename(f)} not found. Run run_dfs_chain(week = {week}) first."),
                              call. = FALSE)
  }
  bands <- readRDS(bands_rds); def <- readRDS(def_rds)
  message(glue("  bands: {nrow(bands)} rows, built {format(max(bands$built_at), '%Y-%m-%d %H:%M')}; ",
               "DEF: {nrow(def)} teams, lines pulled {format(max(def$lines_pulled_at, na.rm = TRUE), '%Y-%m-%d %H:%M')}"))
  r29_ctx <- .r65_read_r29_context(cfg)
  message(glue("  R/29 context: {nrow(r29_ctx)} rows (prior_mu, ytd_mean, ytd_n; display only)"))

  message("\nSTEP 3/4: matching")
  ov   <- .r65_read_overrides(site)
  pool <- .r65_match(sal, bands, def, ov, r29_ctx) %>% .r65_eligibility(include_q = include_q)
  print(pool %>% dplyr::count(row_type, match_status))
  um <- pool %>% dplyr::filter(match_status %in% c("no_projection", "ambiguous", "override_not_found",
                                                   "dst_no_projection")) %>%
    dplyr::arrange(dplyr::desc(salary))
  if (nrow(um) > 0) {
    message(glue("  Unmatched, by salary (first 15 of {nrow(um)}); add to {basename(R65_OVERRIDE_PATH)} as ",
                 "site,site_name,team,nfl_gsis_id if any should be in play:"))
    print(um %>% dplyr::select(site_name, team, salary, match_status, file_proj) %>% dplyr::slice_head(n = 15))
  }
  opp_mis <- pool %>% dplyr::filter(match_status %in% c("matched", "override"),
                                    !is.na(our_opponent), our_opponent != opponent)
  if (nrow(opp_mis) > 0) {
    message(glue("  {nrow(opp_mis)} matched player(s) whose team in our data disagrees with the salary file:"))
    print(opp_mis %>% dplyr::select(site_name, team, opponent, player_name, our_opponent))
  }
  message("  Excluded from the solve (matched rows):")
  print(pool %>% dplyr::filter(match_status %in% c("matched", "override", "dst"), !eligible) %>%
          dplyr::count(excluded_reason, sort = TRUE))
  q_top <- pool %>% dplyr::filter(excluded_reason %in% "questionable") %>% dplyr::arrange(dplyr::desc(proj))
  if (nrow(q_top) > 0) {
    message(glue("  Questionable and excluded (include_q = TRUE to allow), top {min(10, nrow(q_top))} by projection:"))
    print(q_top %>% dplyr::select(site_name, team, position, salary, proj, value) %>% dplyr::slice_head(n = 10))
  }
  message(glue("  Eligible: {sum(pool$eligible)} ",
               "({paste(names(table(pool$position[pool$eligible])), table(pool$position[pool$eligible]), sep = '=', collapse = ', ')})"))
  # [2026-10-22] Eligible QBs, prior vs 2026 (display only; Watson class)
  qb_ctx <- pool %>% dplyr::filter(eligible, position == "QB", !is.na(ytd_gap)) %>%
    dplyr::arrange(ytd_gap)
  if (nrow(qb_ctx) > 0) {
    message("  Eligible QBs, 2026 per-game mean minus prior (most negative first, top 8):")
    print(qb_ctx %>% dplyr::transmute(site_name, team, salary, proj = round(proj, 2),
                                      prior_mu = round(prior_mu, 2), ytd_mean = round(ytd_mean, 2),
                                      ytd_n, ytd_gap) %>% dplyr::slice_head(n = 8))
  }

  message("\nSTEP 4/4: solve")
  lock_idx <- .r65_resolve_names(locks, pool, "lock")
  excl_idx <- .r65_resolve_names(excludes, pool, "exclude")
  both <- intersect(lock_idx, excl_idx)
  if (length(both) > 0) stop("R/65: locked and excluded: ", paste(pool$site_name[both], collapse = ", "),
                             call. = FALSE)
  probs <- .r65_lock_problems(pool, lock_idx, cfg, rules)
  if (length(probs) > 0) {
    stop("R/65: the locks cannot form a legal lineup:\n  - ", paste(probs, collapse = "\n  - "),
         call. = FALSE)
  }
  blk <- rule_blocked_qbs(pool, rules, lock_idx, excl_idx)
  if (nrow(blk) > 0) {
    message(glue("  {nrow(blk)} eligible QB(s) cannot be used under the stacking rules:"))
    print(blk %>% dplyr::arrange(dplyr::desc(proj)), n = Inf)
  }
  res <- solve_dfs_lineup(pool, cfg, lock_idx, excl_idx, rules)
  if (res$status != 0) {
    stop(glue("R/65: no legal lineup exists (solver status {res$status}). Active rules: ",
              "{.r65_rules_text(rules, cfg)}. Loosen a rule or a lock, or include questionable players."),
         call. = FALSE)
  }
  sw <- swap_costs(pool, cfg, lock_idx, excl_idx, rules, res)
  .r65_assert_optimal(sw, res)
  lu <- res$lineup %>% dplyr::left_join(sw, by = "player_uid")
  message(glue("\n  {cfg$name} lineup, week {week}:"))
  print(lu %>% dplyr::transmute(slot, name = site_name, team, opp = opponent, salary,
                                proj = round(proj, 2), ours = round(proj_ours, 2), fd = file_proj,
                                value, swap_cost, replacement, locked, availability),
        n = Inf, width = Inf)
  message(glue("  Projected {round(res$total_proj, 2)} | salary ${res$total_salary} of ${cfg$cap} ",
               "(${cfg$cap - res$total_salary} left) | teams {dplyr::n_distinct(lu$team)}"))
  message(glue("  Share of projected points by team: ",
               "{paste0(names(res$team_share), ' ', round(100 * res$team_share), '%', collapse = ', ')}"))

  lus <- NULL
  if (n_lineups > 1L) {
    lus <- solve_dfs_lineups(pool, cfg, lock_idx, excl_idx, rules, n = n_lineups, min_diff = min_diff)
    message(glue("\n  {nrow(lus)} of {n_lineups} lineups found (each differs from every earlier one ",
                 "by >= {min_diff} player(s)):"))
    print(lus %>% dplyr::select(-lineup), n = Inf)
  }

  out_pool <- pool %>%
    dplyr::mutate(site = site, season = season, week = week, salary_file = basename(salary_path),
                  in_lineup = !is.na(player_uid) & player_uid %in% lu$player_uid,
                  schema_tag = R65_SCHEMA_TAG, built_at = Sys.time())
  if (write_outputs) {
    dir.create(R65_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(out_pool, .r65_path(cfg, "dfs_pool", week, "csv"))
    readr::write_csv(lu %>% dplyr::mutate(site = site, week = week, rules = .r65_rules_text(rules, cfg),
                                          schema_tag = R65_SCHEMA_TAG),
                     .r65_path(cfg, "dfs_lineup", week, "csv"))
    message(glue("  Wrote {.r65_path(cfg, 'dfs_pool', week, 'csv')}"))
    message(glue("  Wrote {.r65_path(cfg, 'dfs_lineup', week, 'csv')}"))
    if (!is.null(lus) && nrow(lus) > 0) {
      long <- lus %>% dplyr::select(lineup_id, rank, total_proj, lineup) %>%
        tidyr::unnest(lineup, names_sep = "_") %>%
        dplyr::mutate(site = site, week = week, min_diff = min_diff,
                      rules = .r65_rules_text(rules, cfg), schema_tag = R65_SCHEMA_TAG)
      readr::write_csv(long, .r65_path(cfg, "dfs_lineups", week, "csv"))
      message(glue("  Wrote {.r65_path(cfg, 'dfs_lineups', week, 'csv')}"))
    }
  }
  message(glue("{strrep('=', 70)}\n"))
  invisible(list(lineup = lu, pool = out_pool, status = res$status,
                 total_proj = res$total_proj, total_salary = res$total_salary,
                 team_share = res$team_share, swap = sw, lineups = lus, rules = rules))
}
