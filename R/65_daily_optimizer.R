# ==============================================================================
# 65_daily_optimizer.R
#
# Daily (DFS) lineup optimizer, v1: FanDuel classic.  [2026-10-05]
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
# LIMITS (stated so they are not forgotten)
#   - R/59 mu is a per-game season mean (R/29 YTD rule), not week-specific: no
#     opponent adjustment for offense (R/35 offense factor neutralized on gate
#     evidence). Only DEF carries a week-specific term (Vegas).
#   - The salary file has no position, so a player FanDuel lists at a
#     different position than our roster source is slotted by OUR position.
#   - R/50 shape table predates flat-PPR (built 2026-08-02); affects bands in
#     R/59, not the mean this optimizer uses.
#   - The file's own projection column is carried as file_proj for reference
#     only. It never enters the objective.
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
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(readr)
library(glue)
library(here)
library(nflreadr)
library(lpSolve)

if (!exists(".pidr_normalize_name")) {
  source(here::here("R", "53_player_identity.R"))
}
if (!exists("load_normalized_season")) {
  source(here::here("R", "15_multi_season_pbp.R"))   # 2026 pbp for QB roles
}

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------
R65_SCHEMA_TAG   <- "s3_r65_dfs_v1"
R65_SALARY_DIR   <- here::here("data", "dfs_salaries")
R65_OVERRIDE_PATH <- file.path(R65_SALARY_DIR, "name_overrides.csv")
R65_OUTPUT_DIR   <- here::here("data", "season3_cache")
R65_CHAIN_CACHE  <- here::here("data", "season2_cache")   # R/29 and R/32 write here

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
  run_weekly_bands(week = week, season = season, recon_path = recon_rds,
                   out_prefix = cfg$prefix, write_outputs = TRUE)
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
      # Reference only (never in the objective): the file's own projection.
      # Player rows: column "fantasy". D/ST rows are shifted: field 12.
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
  message(glue("  QB roles (pbp weeks < {week}): {nrow(st)} starters, ",
               "{sum(roles$qb_role == 'backup')} backups excluded, ",
               "{nrow(unk)} team(s) unknown"))
  fb <- st %>% dplyr::filter(qb_role_basis == "fallback_season")
  if (nrow(fb) > 0) message(glue("    starter by injury fallback: ",
                                 "{paste0(fb$player_name, ' (', fb$team, ')', collapse = ', ')}"))
  if (nrow(unk) > 0) message(glue("    unknown (no QB excluded): ",
                                  "{paste0(unk$team, ' [', unk$qb_role_basis, ']', collapse = ', ')}"))
  roles
}

#' Match salary rows to the chain output
#'
#' @return Tibble, one row per salary row, with match_status in
#'   matched / override / no_projection / ambiguous / dst / dst_no_projection,
#'   plus nfl_gsis_id, player_name, position, proj, availability.
#' @keywords internal
.r65_match <- function(sal, bands, def, overrides) {
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
  dplyr::bind_rows(players, dst) %>%
    dplyr::left_join(.r65_qb_roles(bands), by = "player_uid", relationship = "many-to-one")
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

# Feasibility of the lock set before the solver runs. Returns problem strings.
.r65_lock_problems <- function(pool, lock_idx, cfg) {
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
  tm <- L %>% dplyr::count(team) %>% dplyr::filter(n > cfg$max_per_team)
  if (nrow(tm) > 0) p <- c(p, glue("more than {cfg$max_per_team} locked from {paste(tm$team, collapse = ', ')}"))
  p
}

#' Solve one lineup
#'
#' @param pool Tibble from .r65_eligibility(): eligible rows plus any locked
#'   questionable rows are candidates.
#' @param cfg Site config.
#' @param lock_idx,exclude_idx Integer row indices into pool.
#' @return list(status, lineup, total_proj, total_salary)
#' @export
solve_dfs_lineup <- function(pool, cfg, lock_idx = integer(0), exclude_idx = integer(0)) {
  cand <- pool$eligible
  cand[lock_idx] <- TRUE                 # a lock overrides the Q default (not hard-out: checked earlier)
  cand[exclude_idx] <- FALSE
  P <- pool[cand, ]
  is_lock <- seq_len(nrow(pool))[cand] %in% lock_idx
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
    add(row(ind(P$team == teams[j])), "<=", cfg$max_per_team)
    yj <- rep(0, nt); yj[j] <- 1
    add(row(-ind(P$team == teams[j]), yj), "<=", 0)   # y_t <= players from t
  }
  add(row(rep(0, n), rep(1, nt)), ">=", cfg$min_teams)
  for (i in which(is_lock)) { a <- rep(0, n); a[i] <- 1; add(row(a), "=", 1) }

  sol <- lpSolve::lp(direction = "max",
                     objective.in = c(P$proj, rep(0, nt)),
                     const.mat = do.call(rbind, A), const.dir = dir, const.rhs = rhs,
                     all.bin = TRUE)
  if (sol$status != 0) {
    return(list(status = sol$status, lineup = NULL, total_proj = NA, total_salary = NA))
  }
  pick <- P[round(sol$solution[seq_len(n)]) == 1, ] %>% dplyr::mutate(locked = is_lock[round(sol$solution[seq_len(n)]) == 1])

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

  list(status = 0L, lineup = pick, total_proj = sum(pick$proj), total_salary = sum(pick$salary))
}

# ==============================================================================
# PART 5: ENTRY POINT
# ==============================================================================

#' Build the pool and solve one DFS lineup
#'
#' @param week Integer. Slate week (must match the salary file's games).
#' @param season Integer.
#' @param site Character. "fd".
#' @param salary_path Character or NULL (newest CSV in data/dfs_salaries/).
#' @param locks,excludes Character. Salary-file names (as written in the CSV),
#'   player_uid or nfl_gsis_id values.
#' @param include_q Logical. Include questionable players. Default FALSE.
#' @param write_outputs Logical. Write the pool and lineup CSVs.
#' @return Invisible list: lineup, pool, status, totals.
#' @export
run_dfs_optimizer <- function(week, season = 2026L, site = "fd", salary_path = NULL,
                              locks = character(0), excludes = character(0),
                              include_q = FALSE, write_outputs = TRUE) {
  week <- as.integer(week); season <- as.integer(season)
  cfg <- .r65_site(site)
  if (site != "fd") stop("R/65 v1 parses FanDuel files only.", call. = FALSE)

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/65 OPTIMIZER: {cfg$name} -- season {season} week {week}, cap ${cfg$cap}"))
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

  message("\nSTEP 3/4: matching")
  ov   <- .r65_read_overrides(site)
  pool <- .r65_match(sal, bands, def, ov) %>% .r65_eligibility(include_q = include_q)
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

  message("\nSTEP 4/4: solve")
  lock_idx <- .r65_resolve_names(locks, pool, "lock")
  excl_idx <- .r65_resolve_names(excludes, pool, "exclude")
  both <- intersect(lock_idx, excl_idx)
  if (length(both) > 0) stop("R/65: locked and excluded: ", paste(pool$site_name[both], collapse = ", "),
                             call. = FALSE)
  probs <- .r65_lock_problems(pool, lock_idx, cfg)
  if (length(probs) > 0) {
    stop("R/65: the locks cannot form a legal lineup:\n  - ", paste(probs, collapse = "\n  - "),
         call. = FALSE)
  }
  res <- solve_dfs_lineup(pool, cfg, lock_idx, excl_idx)
  if (res$status != 0) {
    stop(glue("R/65: no legal lineup exists with these locks/excludes (lpSolve status {res$status}). ",
              "Loosen a lock or include questionable players."), call. = FALSE)
  }
  lu <- res$lineup
  message(glue("\n  {cfg$name} lineup, week {week}:"))
  print(lu %>% dplyr::transmute(slot, name = site_name, team, opp = opponent, salary,
                                proj = round(proj, 2), value, locked, availability),
        n = Inf)
  message(glue("  Projected {round(res$total_proj, 2)} | salary ${res$total_salary} of ${cfg$cap} ",
               "(${cfg$cap - res$total_salary} left) | teams {dplyr::n_distinct(lu$team)}"))

  out_pool <- pool %>%
    dplyr::mutate(site = site, season = season, week = week, salary_file = basename(salary_path),
                  in_lineup = !is.na(player_uid) & player_uid %in% lu$player_uid,
                  schema_tag = R65_SCHEMA_TAG, built_at = Sys.time())
  if (write_outputs) {
    dir.create(R65_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(out_pool, .r65_path(cfg, "dfs_pool", week, "csv"))
    readr::write_csv(lu %>% dplyr::mutate(site = site, week = week, schema_tag = R65_SCHEMA_TAG),
                     .r65_path(cfg, "dfs_lineup", week, "csv"))
    message(glue("  Wrote {.r65_path(cfg, 'dfs_pool', week, 'csv')}"))
    message(glue("  Wrote {.r65_path(cfg, 'dfs_lineup', week, 'csv')}"))
  }
  message(glue("{strrep('=', 70)}\n"))
  invisible(list(lineup = lu, pool = out_pool, status = res$status,
                 total_proj = res$total_proj, total_salary = res$total_salary))
}
