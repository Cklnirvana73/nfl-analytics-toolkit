# ==============================================================================
# 56_weekly_usage_report.R
# Season 3 -- weekly, league-agnostic usage report for finding under-the-radar
# players. NFL Analytics Toolkit
#
# NAVIGATION GUIDE
# Line  137 : constants, screening thresholds
# Line  205 : .r56_load_pbp()          -- fresh 2026 pbp, REG, through week
# Line  226 : .r56_screen_ftn()        -- FTN placeholder screen (v2.1)
# Line  268 : .r56_load_ftn()          -- FTN charting, joined to pbp (v2)
# Line  323 : .r56_load_snaps()        -- PFR snap counts, crosswalked to gsis
# Line  390 : .r56_load_ffo()          -- ffopportunity expected points
# Line  404 : .r56_load_adp()          -- preseason FP ADP from the R/29 cache
# Line  427 : .r56_sleeper_trending()  -- Sleeper trending adds, via R/19
# Line  506 : .r56_player_week_ext()   -- per player-week efficiency counts (v2)
# Line  626 : .r56_ext_rates()         -- rates from summed counts (v2)
# Line  706 : .r56_player_week()       -- one row per player-week
# Line  813 : .r56_aggregate()         -- this week / season to date / delta
# Line  911 : build_team_table()       -- 32-row team breakdown
# Line  996 : build_player_weeks()     -- one row per player per game week
# Line 1034 : build_team_weeks()       -- team table, one week at a time
# Line 1081 : build_radar_screen()     -- usage rank vs market rank
# Line 1154 : run_weekly_usage()       -- entry point
#
# PURPOSE
# Run once a week after the games finish. No league id, no roster, no lineup.
# Outputs:
#   1. Player usage table: snap pct, targets and target share, air yards
#      share, rush share, red zone share, carries inside the 10, expected vs
#      actual fantasy points. Each at three grains: this week, season to date,
#      week-over-week delta.
#      v2 adds efficiency columns per player at the wk_ and ytd_ grains:
#        receivers: aDOT, targets by depth, end-zone and red-zone targets,
#                   EPA per target, catchable / contested / drop / created
#                   reception rates (FTN), targets and EPA vs the blitz
#        rushers:   conversion rate (1st down or TD per carry), stuff rate,
#                   explosive rate, EPA per carry, light vs stacked box
#                   carries (FTN), goal-line and short-yardage carries
#        passers:   EPA per dropback, CPOE, aDOT, deep rate, sack and
#                   scramble rate, play-action rate and EPA, blitz rate faced
#                   and EPA vs the blitz, interception-worthy / throwaway /
#                   out-of-pocket rates and QB-fault sack share (FTN)
#      QB rows now exist for every game with a dropback (v1 only had a QB
#      row when he rushed or was targeted).
#   2. Team breakdown: plays per game, pass rate, neutral-script pass rate,
#      red zone trips, EPA per play, and how concentrated the targets and
#      carries are. (Team-level tendencies and situational splits on both
#      sides of the ball live in R/62, not here.)
#   3. Under-the-radar screen: players whose opportunity rank within position
#      is well ahead of where the preseason market drafted them, with Sleeper
#      trending adds alongside so you can see whether the market has already
#      caught up.
#   4. Player-week log (v1.1): one row per player per game week, weeks 1..N,
#      each week's own shares and (v2) rates. Feeds R/57's By Week tab.
#   5. Team-week log (v1.1): the team breakdown computed one week at a time.
#
# FILES WRITTEN (data/season3_cache/, NN = zero-padded run week)
#   s3_r56_players_wNN.csv       wk_ / ytd_ / d_ grains (v2: wider)
#   s3_r56_teams_wNN.csv         season to date (unchanged)
#   s3_r56_radar_wNN.csv         screen (unchanged)
#   s3_r56_player_weeks_wNN.csv  long, one row per player-week (v2: wider)
#   s3_r56_team_weeks_wNN.csv    long, one row per team-week (unchanged)
#
# DATA SOURCES (verified 2026-09-16 on week 1; FTN verified 2026-09-29 on
# weeks 1-3)
#   nflreadr::load_pbp(2026)            2,756 plays, 16 REG games (week 1)
#   nflreadr::load_ftn_charting(2026)   8,065 rows weeks 1-3; joins 100% of
#                                       scrimmage plays on game_id =
#                                       nflverse_game_id, play_id =
#                                       nflverse_play_id. Blitz = 5+ rushers.
#   nflreadr::load_snap_counts(2026)    1,492 rows, keyed on pfr_player_id.
#                                       offense_pct is the snap share column.
#   nflreadr::load_ff_opportunity(2026) 338 rows, keyed on gsis player_id.
#                                       total_fantasy_points_exp / _diff used.
#   nflreadr::load_participation(2026)  404 Not Found. ROUTE PARTICIPATION IS
#                                       NOT AVAILABLE. The routes column is not
#                                       in this file rather than proxied from
#                                       snaps. Revisit if the release appears.
#   R/29 FP ADP cache (optional)        s2_week15_fp_adp_cache_{season}_PPR.rds
#   Sleeper trending adds (optional)    GET /players/nfl/trending/add, via the
#                                       R/19 player universe for id matching.
#
# SCREENING THRESHOLDS
# The constants below are screening parameters, not model weights. They decide
# what gets printed, not what any projection is worth. Change freely.
#
# SCHEMA TAG: s3_r56_usage_v2
# Author: Christian K. LeBlanc
# Version: 1.0  Initial build.
#          1.1  [2026-09-22] Adds the player-week and team-week logs. No
#               existing column or file changed, so the schema tag stays v1.
#               run_weekly_usage() also checks that the player-week log sums
#               back to the ytd_ counts before writing.
#          2.0  [2026-09-29] FTN charting loader (shared with R/62). Player
#               efficiency block for receivers, rushers and passers at wk_ and
#               ytd_ grains and in the player-week log. QB rows for every
#               dropback game. No v1 column renamed or redefined; files are
#               wider, so the schema tag moves to v2.
#          2.1  [2026-10-13] FTN placeholder screen inside .r56_load_ftn(),
#               so R/56 and R/62 both receive screened FTN. See FTN
#               PLACEHOLDER ROWS below. No column added or renamed, so the
#               schema tag stays v2; FTN values for uncharted games are now NA
#               instead of 0.
#
# FTN PLACEHOLDER ROWS (found 2026-10-13 on the week 5 pull)
# FTN publishes a row for every play of a game before it has charted it.
# Those rows carry n_defense_box = 0, n_pass_rushers = 0 and FALSE in every
# is_* flag rather than NA, so v2.0 counted them as charted: on the week 5
# pull 14 of 15 games were placeholders, and every FTN-based player column
# (targets and dropbacks vs the blitz, play-action rate, box carries,
# catchable / contested / drop rates, interception-worthy rate, QB-fault
# sacks) read 0 for those games. .r56_screen_ftn() marks a game charted only
# when at least R56_FTN_CHARTED_MIN_SHARE of its rows show a box count or
# pass-rusher count of 1+, and sets every FTN column to NA for the rest, which
# drops them from every *_ftn denominator. require_ftn = TRUE now stops when
# the run week is in the FTN file but none of its games are charted.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(glue); library(here)
  library(nflreadr)
})

`%||%` <- function(x, y) if (!is.null(x)) x else y

# R/08 supplies the share denominators. Guarded source, same pattern as R/35.
if (!exists("calculate_usage_metrics")) {
  source(here::here("R", "08_usage_features.R"))
}
# R/19 is optional. Without it the Sleeper trending column is NA.
if (!exists("get_all_sleeper_players") &&
    file.exists(here::here("R", "19_sleeper_api.R"))) {
  try(source(here::here("R", "19_sleeper_api.R")), silent = TRUE)
}

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

R56_SEASON      <- 2026L
R56_SCHEMA_TAG  <- "s3_r56_usage_v2"
R56_OUT_DIR     <- here::here("data", "season3_cache")
R56_ADP_DIR     <- here::here("data", "season2_cache")
R56_POSITIONS   <- c("QB", "RB", "WR", "TE")

# Neutral game script: win probability between these bounds, quarters 1-3.
R56_NEUTRAL_WP    <- c(0.20, 0.80)
R56_NEUTRAL_QTRS  <- 1:3

# Screen: a player is "under the radar" when his season-to-date opportunity
# rank within position is inside R56_RADAR_TOP and the market rank trails it
# by at least R56_RADAR_GAP. Undrafted players get market rank = (deepest
# drafted rank at the position) + 1, so they always trail.
R56_RADAR_TOP   <- 36L
R56_RADAR_GAP   <- 12L

# Role change: week-over-week snap pct rise at or above this (fraction).
R56_ROLE_CHANGE_DELTA <- 0.15

# Minimum season-to-date opportunities before a player enters the screen.
R56_MIN_OPPS <- 5L

# R/08's incomplete-data guard drops any team-game whose team_targets falls
# below min_team_plays (default 20). A real low-volume game clears that bar
# legitimately: in 2026 week 1, ATL threw 26 times, landed under 20 targets,
# and every ATL receiver was dropped from the usage frame, which is why ATL's
# target shares came back empty while its snap counts did not. This report
# already verifies game completeness itself (16 games in pbp), so the guard is
# redundant here and is set to 1. Do not raise it without re-checking the
# low-volume teams. [2026-09-16]
R56_MIN_TEAM_PLAYS <- 1

# Sleeper trending window and depth.
R56_TRENDING_HOURS <- 168L
R56_TRENDING_LIMIT <- 100L

# FTN charting: blitz = five or more pass rushers (FTN codes exactly this:
# n_blitzers >= 1 and n_pass_rushers >= 5 are the same rows). Explosive
# thresholds are yards based.
R56_BLITZ_RUSHERS      <- 5L
R56_EXPLOSIVE_RUSH_YDS <- 10L

# FTN columns carried through the pbp join (v2). Shared with R/62.
R56_FTN_COLS <- c("n_pass_rushers", "n_blitzers", "n_defense_box",
                  "n_offense_backfield", "is_play_action", "is_screen_pass",
                  "is_rpo", "is_motion", "is_no_huddle", "is_qb_out_of_pocket",
                  "is_interception_worthy", "is_throw_away", "is_catchable_ball",
                  "is_contested_ball", "is_created_reception", "is_drop",
                  "is_qb_sneak", "is_qb_fault_sack")

# FTN placeholder screen (v2.1, see header). A game counts as charted when at
# least this share of its FTN rows has a box count or pass-rusher count of 1+.
# Real charting sits near 100% and placeholders at 0%; a share between the two
# warn bounds fits neither and is reported.
R56_FTN_CHARTED_MIN_SHARE <- 0.5
R56_FTN_WARN_BOUNDS       <- c(0.05, 0.95)

# ------------------------------------------------------------------------------
# LOADERS
# ------------------------------------------------------------------------------

#' Fresh pbp for the season, regular season, weeks 1..week.
#' Reads nflreadr directly rather than the R/15 cache so a stale cache built
#' before kickoff cannot silently return zero weeks.
.r56_load_pbp <- function(season, week) {
  pbp <- nflreadr::load_pbp(season) %>%
    dplyr::filter(.data$season_type == "REG", .data$week <= !!week)
  n_games <- dplyr::n_distinct(pbp$game_id)
  message(glue("  pbp: {format(nrow(pbp), big.mark = ',')} plays, ",
               "{n_games} REG games through week {week}"))
  wk_games <- pbp %>% dplyr::filter(.data$week == !!week) %>%
    dplyr::distinct(game_id) %>% nrow()
  if (wk_games < 16L) {
    warning(glue("Week {week} has {wk_games} games in pbp (expected 16 in a ",
                 "full non-bye week). The week may be incomplete."),
            call. = FALSE)
  }
  pbp
}

#' Mark each FTN game charted or not and set every FTN column to NA for the
#' games that are not (v2.1). See FTN PLACEHOLDER ROWS in the header.
#' @param ftn Raw FTN frame with nflverse_game_id and week.
#' @return list(ftn = screened frame, games = one row per game with week,
#'   n_rows, share_marked, charted).
.r56_screen_ftn <- function(ftn, min_share = R56_FTN_CHARTED_MIN_SHARE) {
  as_int <- function(col) {
    if (col %in% names(ftn)) suppressWarnings(as.integer(ftn[[col]]))
    else rep(NA_integer_, nrow(ftn))
  }
  box  <- as_int("n_defense_box")
  rush <- as_int("n_pass_rushers")
  marked <- (!is.na(box) & box >= 1L) | (!is.na(rush) & rush >= 1L)

  games <- tibble::tibble(game_id = ftn$nflverse_game_id,
                          week = as.integer(ftn$week), marked = marked) %>%
    dplyr::group_by(game_id, week) %>%
    dplyr::summarise(n_rows = dplyr::n(), share_marked = mean(marked),
                     .groups = "drop") %>%
    dplyr::mutate(charted = share_marked >= min_share)

  odd <- games %>%
    dplyr::filter(share_marked > R56_FTN_WARN_BOUNDS[1],
                  share_marked < R56_FTN_WARN_BOUNDS[2])
  if (nrow(odd) > 0L) {
    warning(glue("FTN screen: {nrow(odd)} game(s) are partly marked and fit ",
                 "neither the charted nor the placeholder pattern: ",
                 "{paste0(odd$game_id, ' (', round(100 * odd$share_marked), '%)', collapse = ', ')}. ",
                 "Games at or above {round(100 * min_share)}% are kept as charted."),
            call. = FALSE)
  }

  drop_ids <- games$game_id[!games$charted]
  if (length(drop_ids) > 0L) {
    idx <- ftn$nflverse_game_id %in% drop_ids
    for (cc in intersect(R56_FTN_COLS, names(ftn))) ftn[[cc]][idx] <- NA
  }
  list(ftn = ftn, games = games)
}

#' FTN charting, weeks 1..week, keyed for the pbp join (v2). Returns NULL when
#' the feed is unavailable. When `require_week` is TRUE (default) and the run
#' week is not in the file yet, stops: FTN posts during the week and a report
#' built with every FTN column NA for the newest week would look complete
#' while being empty where it matters. v2.1: placeholder rows for games FTN
#' has not charted are screened out (.r56_screen_ftn), and a run week whose
#' games are all placeholders counts as not in the file.
.r56_load_ftn <- function(season, week, require_week = TRUE) {
  ftn <- tryCatch(nflreadr::load_ftn_charting(season), error = function(e) NULL)
  if (is.null(ftn) || nrow(ftn) == 0L) {
    msg <- glue("FTN charting for {season} is unavailable; FTN columns will be NA")
    if (isTRUE(require_week)) stop("R/56: ", msg, ". Rerun with require_ftn = FALSE ",
                                   "to proceed without it.", call. = FALSE)
    message("  ", msg)
    return(NULL)
  }
  ftn <- ftn %>% dplyr::filter(.data$week <= !!week)
  have <- sort(unique(ftn$week))
  if (!(week %in% have)) {
    msg <- glue("FTN charting has weeks {paste(have, collapse = ',')} but not ",
                "week {week} yet")
    if (isTRUE(require_week)) stop("R/56: ", msg, ". Wait for the FTN release or ",
                                   "rerun with require_ftn = FALSE.", call. = FALSE)
    message("  ", msg, "; FTN columns are NA for that week")
  }

  # v2.1 placeholder screen
  scr <- .r56_screen_ftn(ftn)
  ftn <- scr$ftn
  by_wk <- scr$games %>%
    dplyr::group_by(week) %>%
    dplyr::summarise(n_games = dplyr::n(), n_charted = sum(charted),
                     missing = paste(game_id[!charted], collapse = ", "),
                     .groups = "drop")
  for (i in seq_len(nrow(by_wk))) {
    r <- by_wk[i, ]
    if (r$n_charted < r$n_games) {
      message(glue("  FTN week {r$week}: {r$n_charted} of {r$n_games} games charted ",
                   "(placeholders set to NA): {r$missing}"))
    }
  }
  run_wk <- by_wk[which(by_wk$week == week), , drop = FALSE]
  if (nrow(run_wk) > 0L && run_wk$n_charted == 0L) {
    msg <- glue("FTN lists week {week} but has charted none of its ",
                "{run_wk$n_games} games yet")
    if (isTRUE(require_week)) stop("R/56: ", msg, ". Wait for the FTN release or ",
                                   "rerun with require_ftn = FALSE.", call. = FALSE)
    message("  ", msg, "; FTN columns are NA for that week")
  }

  keep <- intersect(c("nflverse_game_id", "nflverse_play_id", R56_FTN_COLS), names(ftn))
  out <- ftn %>%
    dplyr::select(dplyr::all_of(keep)) %>%
    dplyr::distinct(nflverse_game_id, nflverse_play_id, .keep_all = TRUE)
  message(glue("  FTN charting: {format(nrow(out), big.mark = ',')} plays, ",
               "weeks {paste(have, collapse = ',')}; ",
               "{sum(scr$games$charted)} of {nrow(scr$games)} games charted"))
  out
}

#' PFR snap counts crosswalked to gsis via the season roster (pfr_id) with the
#' all-time players table as the fallback. Reports the match rate.
.r56_load_snaps <- function(season, week) {
  snaps <- tryCatch(nflreadr::load_snap_counts(season), error = function(e) NULL)
  if (is.null(snaps) || nrow(snaps) == 0L) {
    message("  snap counts: unavailable, snap columns will be NA")
    return(NULL)
  }
  # [2026-09-30] PFR labels some running backs "HB" (every CIN back in weeks
  # 1-3 of 2026: Brown, Perine, Brooks). The position filter below kept only
  # "RB", so those rows were dropped before the gsis join and the players'
  # snap columns came out NA. HB is normalized to RB first. FB is NOT mapped:
  # fullbacks were excluded before this fix and remain excluded.
  n_hb <- sum(snaps$position %in% "HB", na.rm = TRUE)
  hb_teams <- sort(unique(snaps$team[snaps$position %in% "HB"]))
  snaps <- snaps %>%
    dplyr::mutate(position = dplyr::if_else(.data$position %in% "HB", "RB", .data$position))
  if (n_hb > 0L) {
    message(glue("  snap counts: {n_hb} 'HB' row(s) normalized to RB ",
                 "(teams: {paste(hb_teams, collapse = ', ')})"))
  }
  snaps <- snaps %>%
    dplyr::filter(.data$game_type == "REG", .data$week <= !!week,
                  .data$position %in% R56_POSITIONS)

  # offense_pct arrives as a fraction in nflreadr. Guard in case a release
  # ever ships it as a percent.
  if (max(snaps$offense_pct, na.rm = TRUE) > 1.5) {
    snaps$offense_pct <- snaps$offense_pct / 100
  }

  roster <- tryCatch(nflreadr::load_rosters(season), error = function(e) NULL)
  xw <- NULL
  if (!is.null(roster) && all(c("pfr_id", "gsis_id") %in% names(roster))) {
    xw <- roster %>%
      dplyr::filter(!is.na(.data$pfr_id), !is.na(.data$gsis_id)) %>%
      dplyr::distinct(pfr_id, .keep_all = TRUE) %>%
      dplyr::select(pfr_player_id = "pfr_id", player_id = "gsis_id")
  }
  players <- tryCatch(nflreadr::load_players(), error = function(e) NULL)
  if (!is.null(players) && all(c("pfr_id", "gsis_id") %in% names(players))) {
    xw2 <- players %>%
      dplyr::filter(!is.na(.data$pfr_id), !is.na(.data$gsis_id)) %>%
      dplyr::distinct(pfr_id, .keep_all = TRUE) %>%
      dplyr::select(pfr_player_id = "pfr_id", player_id = "gsis_id")
    xw <- dplyr::bind_rows(xw, xw2) %>% dplyr::distinct(pfr_player_id, .keep_all = TRUE)
  }
  if (is.null(xw)) {
    message("  snap counts: no pfr -> gsis crosswalk available, snaps dropped")
    return(NULL)
  }

  out <- snaps %>%
    dplyr::left_join(xw, by = "pfr_player_id") %>%
    dplyr::transmute(player_id, week, snap_team = .data$team,
                     offense_snaps = as.integer(.data$offense_snaps),
                     offense_pct   = as.numeric(.data$offense_pct),
                     # implied team offensive snaps, for season-to-date pct
                     team_snaps = dplyr::if_else(.data$offense_pct > 0,
                                                 .data$offense_snaps / .data$offense_pct,
                                                 NA_real_))
  n_match <- sum(!is.na(out$player_id))
  message(glue("  snap counts: {nrow(out)} player-weeks, ",
               "{n_match} matched to gsis ({round(100 * n_match / nrow(out), 1)}%)"))
  out %>% dplyr::filter(!is.na(player_id))
}

#' ffopportunity expected vs actual fantasy points, per player-week.
#' Scoring is whatever ffopportunity computes; it is not the league scoring.
.r56_load_ffo <- function(season, week) {
  ffo <- tryCatch(nflreadr::load_ff_opportunity(season), error = function(e) NULL)
  if (is.null(ffo) || nrow(ffo) == 0L) {
    message("  ff_opportunity: unavailable, expected-points columns will be NA")
    return(NULL)
  }
  ffo %>%
    dplyr::filter(.data$week <= !!week, .data$position %in% R56_POSITIONS) %>%
    dplyr::transmute(player_id, week,
                     fp_act = as.numeric(.data$total_fantasy_points),
                     fp_exp = as.numeric(.data$total_fantasy_points_exp))
}

#' Preseason FantasyPros ADP from the R/29 cache. Optional.
.r56_load_adp <- function(season, adp_dir = R56_ADP_DIR) {
  path <- file.path(adp_dir, glue("s2_week15_fp_adp_cache_{season}_PPR.rds"))
  if (!file.exists(path)) {
    message(glue("  ADP: cache not found ({basename(path)}); market rank will be NA"))
    return(NULL)
  }
  adp <- readRDS(path)
  need <- c("nfl_gsis_id", "adp_rank")
  if (!all(need %in% names(adp))) {
    message("  ADP: cache lacks nfl_gsis_id / adp_rank; market rank will be NA")
    return(NULL)
  }
  adp <- adp %>%
    dplyr::filter(!is.na(.data$nfl_gsis_id), !is.na(.data$adp_rank)) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE) %>%
    dplyr::transmute(player_id = .data$nfl_gsis_id, adp_rank = as.numeric(.data$adp_rank))
  message(glue("  ADP: {nrow(adp)} drafted players from cache ",
               "(preseason snapshot, goes stale as the season moves)"))
  adp
}

#' Sleeper trending adds over the last R56_TRENDING_HOURS, matched to gsis.
#' Needs R/19 for the player universe. Returns NULL when anything is missing.
.r56_sleeper_trending <- function() {
  if (!exists("get_all_sleeper_players")) {
    message("  Sleeper trending: R/19 not loaded, column will be NA")
    return(NULL)
  }
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    message("  Sleeper trending: httr/jsonlite missing, column will be NA")
    return(NULL)
  }
  url <- glue("https://api.sleeper.app/v1/players/nfl/trending/add",
              "?lookback_hours={R56_TRENDING_HOURS}&limit={R56_TRENDING_LIMIT}")
  resp <- tryCatch(httr::GET(url, httr::timeout(20)), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200L) {
    message("  Sleeper trending: request failed, column will be NA")
    return(NULL)
  }
  trend <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  if (!is.data.frame(trend) || nrow(trend) == 0L) return(NULL)
  trend <- tibble::tibble(sleeper_player_id = as.character(trend$player_id),
                          trending_adds = as.integer(trend$count))

  univ <- tryCatch(get_all_sleeper_players(), error = function(e) NULL)
  if (is.null(univ)) return(NULL)
  univ <- univ %>%
    dplyr::transmute(sleeper_player_id = as.character(.data$sleeper_player_id),
                     player_id = .data$nfl_gsis_id,
                     name_key = .r56_norm_name(.data$player_name),
                     position = .data$position)
  out <- trend %>% dplyr::left_join(univ, by = "sleeper_player_id")
  message(glue("  Sleeper trending: {nrow(out)} players, ",
               "{sum(!is.na(out$player_id))} with a direct gsis id"))
  out
}

#' max() that returns NA instead of -Inf (with a warning) on an all-NA group.
#' A player whose team had no red zone plays that week hits exactly this.
.r56_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA_real_ else max(x)
}

.r56_norm_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", x)
  gsub("[^a-z0-9]", "", x)
}

# ------------------------------------------------------------------------------
# PLAYER-WEEK EFFICIENCY BLOCK  [2026-09-29, v2]
# ------------------------------------------------------------------------------

#' Every extension column is a COUNT or a SUM per player-week, so the season
#' grain is sum over weeks and the rate is (summed numerator / summed
#' denominator), same convention as the shares. Rates are built afterwards by
#' .r56_ext_rates() from these names.
R56_EXT_COLS <- c(
  # receiving (thrown targets: complete, incomplete or intercepted)
  "tgts", "tgt_behind", "tgt_short", "tgt_int", "tgt_deep", "rec_air_sum",
  "rec_epa_sum", "rec_fd", "ez_targets", "rz_targets", "receptions",
  "rec_yards", "rec_yac", "rec_tds", "tgts_ftn", "rec_catchable",
  "rec_contested", "rec_drops", "rec_created", "tgt_vs_blitz", "rec_epa_blitz_sum",
  # rushing (designed runs; scrambles excluded)
  "carries", "rush_yards", "rush_epa_sum", "rush_conv", "rush_stuffs",
  "rush_explosive", "rush_fd", "rush_tds", "carries_box_known",
  "carries_light_box", "carries_stacked_box", "gl_carries", "sy_carries", "sy_conv",
  # passing (dropbacks: pass plays incl. sacks, plus scrambles)
  "dropbacks", "pass_epa_sum", "thrown", "completions", "pass_yards", "pass_tds",
  "ints", "pass_fd", "cpoe_sum", "cpoe_n", "pass_air_sum", "deep_att", "sacks",
  "scrambles", "dropbacks_ftn", "pa_dropbacks", "pa_epa_sum", "db_vs_blitz",
  "blitz_epa_sum", "thrown_ftn", "int_worthy", "throwaways", "out_of_pocket",
  "sacks_ftn", "qb_fault_sacks"
)

#' Per player-week counts and sums for receivers, rushers and passers, from
#' pbp with FTN joined. One row per player_id x week x team; a player with
#' targets and carries in the same game is one row with both blocks filled.
#' Returns the R56_EXT_COLS plus player_id, week, team.
.r56_player_week_ext <- function(pbp, ftn) {

  clean <- pbp %>%
    dplyr::filter(!is.na(.data$posteam), .data$play_type %in% c("pass", "run"),
                  .data$qb_kneel == 0, .data$qb_spike == 0)

  if (!is.null(ftn)) {
    clean <- clean %>%
      dplyr::left_join(ftn, by = c("game_id" = "nflverse_game_id",
                                   "play_id" = "nflverse_play_id"))
  } else {
    for (cc in R56_FTN_COLS) clean[[cc]] <- NA
  }

  num0 <- function(x) dplyr::coalesce(as.numeric(x), 0)
  flag <- function(x) as.numeric(as.logical(x))

  clean <- clean %>%
    dplyr::mutate(
      thrown   = num0(complete_pass) == 1 | num0(incomplete_pass) == 1 | num0(interception) == 1,
      is_rush  = play_type == "run" & num0(qb_scramble) == 0,
      dropback = num0(qb_dropback) == 1,
      n_pass_rushers = suppressWarnings(as.integer(n_pass_rushers)),
      n_defense_box  = suppressWarnings(as.integer(n_defense_box)),
      is_blitz = as.numeric(n_pass_rushers >= R56_BLITZ_RUSHERS),
      dplyr::across(dplyr::all_of(c("is_play_action", "is_qb_out_of_pocket",
                                    "is_throw_away", "is_interception_worthy",
                                    "is_catchable_ball", "is_contested_ball",
                                    "is_drop", "is_created_reception",
                                    "is_qb_fault_sack")), flag),
      fd = num0(first_down), td = num0(touchdown), yds = num0(yards_gained)
    )

  rec <- clean %>%
    dplyr::filter(thrown, !is.na(receiver_player_id)) %>%
    dplyr::group_by(player_id = receiver_player_id, week, team = posteam) %>%
    dplyr::summarise(
      tgts        = dplyr::n(),
      tgt_behind  = sum(!is.na(air_yards) & air_yards < 0),
      tgt_short   = sum(!is.na(air_yards) & air_yards >= 0 & air_yards < 10),
      tgt_int     = sum(!is.na(air_yards) & air_yards >= 10 & air_yards < 20),
      tgt_deep    = sum(!is.na(air_yards) & air_yards >= 20),
      rec_air_sum = sum(air_yards, na.rm = TRUE),
      rec_epa_sum = sum(epa, na.rm = TRUE),
      rec_fd      = sum(fd),
      ez_targets  = sum(!is.na(air_yards) & !is.na(yardline_100) & air_yards >= yardline_100),
      rz_targets  = sum(!is.na(yardline_100) & yardline_100 <= 20),
      receptions  = sum(num0(complete_pass)),
      rec_yards   = sum(yds[num0(complete_pass) == 1]),
      rec_yac     = sum(yards_after_catch, na.rm = TRUE),
      rec_tds     = sum(td * (num0(complete_pass) == 1)),
      tgts_ftn    = sum(!is.na(is_catchable_ball)),
      rec_catchable = sum(is_catchable_ball, na.rm = TRUE),
      rec_contested = sum(is_contested_ball, na.rm = TRUE),
      rec_drops     = sum(is_drop, na.rm = TRUE),
      rec_created   = sum(is_created_reception, na.rm = TRUE),
      tgt_vs_blitz  = sum(is_blitz %in% 1),
      rec_epa_blitz_sum = sum(epa[is_blitz %in% 1], na.rm = TRUE),
      .groups = "drop")

  rush <- clean %>%
    dplyr::filter(is_rush, !is.na(rusher_player_id)) %>%
    dplyr::group_by(player_id = rusher_player_id, week, team = posteam) %>%
    dplyr::summarise(
      carries      = dplyr::n(),
      rush_yards   = sum(yds),
      rush_epa_sum = sum(epa, na.rm = TRUE),
      rush_conv    = sum(fd == 1 | td == 1),
      rush_stuffs  = sum(yds <= 0),
      rush_explosive = sum(yds >= R56_EXPLOSIVE_RUSH_YDS),
      rush_fd      = sum(fd),
      rush_tds     = sum(td),
      carries_box_known   = sum(!is.na(n_defense_box)),
      carries_light_box   = sum(n_defense_box <= 6, na.rm = TRUE),
      carries_stacked_box = sum(n_defense_box >= 8, na.rm = TRUE),
      gl_carries   = sum(!is.na(yardline_100) & yardline_100 <= 5),
      sy_carries   = sum(down %in% c(3, 4) & !is.na(ydstogo) & ydstogo <= 2),
      sy_conv      = sum(down %in% c(3, 4) & !is.na(ydstogo) & ydstogo <= 2 & (fd == 1 | td == 1)),
      .groups = "drop")

  pass <- clean %>%
    dplyr::filter(dropback, !is.na(passer_player_id)) %>%
    dplyr::group_by(player_id = passer_player_id, week, team = posteam) %>%
    dplyr::summarise(
      dropbacks    = dplyr::n(),
      pass_epa_sum = sum(epa, na.rm = TRUE),
      thrown       = sum(thrown),
      completions  = sum(num0(complete_pass)),
      pass_yards   = sum(yds[num0(complete_pass) == 1]),
      pass_tds     = sum(num0(pass_touchdown)),
      ints         = sum(num0(interception)),
      pass_fd      = sum(fd * (play_type == "pass")),
      cpoe_sum     = sum(cpoe, na.rm = TRUE),
      cpoe_n       = sum(!is.na(cpoe)),
      pass_air_sum = sum(air_yards[thrown], na.rm = TRUE),
      deep_att     = sum(thrown & !is.na(air_yards) & air_yards >= 20),
      sacks        = sum(num0(sack)),
      scrambles    = sum(num0(qb_scramble)),
      dropbacks_ftn = sum(!is.na(n_pass_rushers)),
      pa_dropbacks = sum(is_play_action, na.rm = TRUE),
      pa_epa_sum   = sum(epa[is_play_action %in% 1], na.rm = TRUE),
      db_vs_blitz  = sum(is_blitz %in% 1),
      blitz_epa_sum = sum(epa[is_blitz %in% 1], na.rm = TRUE),
      thrown_ftn   = sum(thrown & !is.na(is_catchable_ball)),
      int_worthy   = sum(is_interception_worthy, na.rm = TRUE),
      throwaways   = sum(is_throw_away, na.rm = TRUE),
      out_of_pocket = sum(is_qb_out_of_pocket, na.rm = TRUE),
      sacks_ftn    = sum(num0(sack) == 1 & !is.na(is_qb_fault_sack)),
      qb_fault_sacks = sum(is_qb_fault_sack, na.rm = TRUE),
      .groups = "drop")

  rec %>%
    dplyr::full_join(rush, by = c("player_id", "week", "team")) %>%
    dplyr::full_join(pass, by = c("player_id", "week", "team")) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(R56_EXT_COLS), ~ dplyr::coalesce(.x, 0)))
}

#' Rates from a frame that carries the R56_EXT_COLS (one week, or summed
#' over weeks). Adds the rate columns; keeps the counts. NA when the
#' denominator is zero, never Inf or NaN.
.r56_ext_rates <- function(d) {
  r <- function(num, den) dplyr::if_else(den > 0, num / den, NA_real_)
  d %>%
    dplyr::mutate(
      # receiving
      rec_adot           = r(rec_air_sum, tgts),
      rec_epa_tgt        = r(rec_epa_sum, tgts),
      rec_ypt            = r(rec_yards, tgts),
      rec_catch_rate     = r(receptions, tgts),
      rec_fd_rate        = r(rec_fd, tgts),
      rec_deep_share     = r(tgt_deep, tgts),
      rec_yac_per_rec    = r(rec_yac, receptions),
      rec_catchable_rate = r(rec_catchable, tgts_ftn),
      rec_contested_rate = r(rec_contested, tgts_ftn),
      rec_drop_rate      = r(rec_drops, tgts_ftn),
      rec_created_rate   = r(rec_created, tgts_ftn),
      rec_epa_vs_blitz   = r(rec_epa_blitz_sum, tgt_vs_blitz),
      # rushing
      rush_ypc            = r(rush_yards, carries),
      rush_epa_carry      = r(rush_epa_sum, carries),
      rush_conv_rate      = r(rush_conv, carries),
      rush_stuff_rate     = r(rush_stuffs, carries),
      rush_explosive_rate = r(rush_explosive, carries),
      rush_fd_rate        = r(rush_fd, carries),
      rush_light_box_rate   = r(carries_light_box, carries_box_known),
      rush_stacked_box_rate = r(carries_stacked_box, carries_box_known),
      rush_sy_conv_rate   = r(sy_conv, sy_carries),
      # passing
      pass_epa_db        = r(pass_epa_sum, dropbacks),
      pass_sack_rate     = r(sacks, dropbacks),
      pass_scramble_rate = r(scrambles, dropbacks),
      pass_fd_rate       = r(pass_fd, dropbacks),
      pass_comp_pct      = r(completions, thrown),
      pass_cpoe          = r(cpoe_sum, cpoe_n),
      pass_adot          = r(pass_air_sum, thrown),
      pass_deep_rate     = r(deep_att, thrown),
      pass_ypa           = r(pass_yards, thrown),
      pass_pa_rate       = r(pa_dropbacks, dropbacks_ftn),
      pass_pa_epa_db     = r(pa_epa_sum, pa_dropbacks),
      pass_blitz_rate_faced = r(db_vs_blitz, dropbacks_ftn),
      pass_blitz_epa_db  = r(blitz_epa_sum, db_vs_blitz),
      pass_int_worthy_rate = r(int_worthy, thrown_ftn),
      pass_throwaway_rate  = r(throwaways, dropbacks_ftn),
      pass_oop_rate        = r(out_of_pocket, dropbacks_ftn),
      pass_qb_fault_sack_share = r(qb_fault_sacks, sacks_ftn)
    )
}

R56_EXT_RATE_COLS <- c(
  "rec_adot", "rec_epa_tgt", "rec_ypt", "rec_catch_rate", "rec_fd_rate",
  "rec_deep_share", "rec_yac_per_rec", "rec_catchable_rate", "rec_contested_rate",
  "rec_drop_rate", "rec_created_rate", "rec_epa_vs_blitz",
  "rush_ypc", "rush_epa_carry", "rush_conv_rate", "rush_stuff_rate",
  "rush_explosive_rate", "rush_fd_rate", "rush_light_box_rate",
  "rush_stacked_box_rate", "rush_sy_conv_rate",
  "pass_epa_db", "pass_sack_rate", "pass_scramble_rate", "pass_fd_rate",
  "pass_comp_pct", "pass_cpoe", "pass_adot", "pass_deep_rate", "pass_ypa",
  "pass_pa_rate", "pass_pa_epa_db", "pass_blitz_rate_faced", "pass_blitz_epa_db",
  "pass_int_worthy_rate", "pass_throwaway_rate", "pass_oop_rate",
  "pass_qb_fault_sack_share"
)

# Extension columns that travel into the players file at both grains: the
# rates above plus the counts a reader wants next to them.
R56_EXT_OUT_COLS <- c(
  "tgts", "tgt_deep", "ez_targets", "rz_targets", "receptions", "rec_yards",
  "rec_tds", "tgt_vs_blitz",
  "carries", "rush_yards", "rush_tds", "gl_carries", "sy_carries",
  "dropbacks", "thrown", "pass_yards", "pass_tds", "ints", "sacks", "db_vs_blitz",
  R56_EXT_RATE_COLS
)

# ------------------------------------------------------------------------------
# PLAYER-WEEK ASSEMBLY
# ------------------------------------------------------------------------------

#' One row per player-week. Collapses R/08's receiver/rusher rows, adds
#' carries inside the 10 from pbp, snaps, expected points, and (v2) the
#' efficiency block. Passers with a dropback but no target or carry enter
#' through the efficiency block.
.r56_player_week <- function(pbp, snaps, ffo, ext, season, week) {

  usage <- calculate_usage_metrics(pbp, season = season,
                                   week_min = 1, week_max = week,
                                   min_team_plays = R56_MIN_TEAM_PLAYS)

  # R/08 emits a receiver row and a rusher row for the same player-game. The
  # team denominators are identical on both, so max() recovers them.
  pw <- usage %>%
    dplyr::group_by(player_id, week, team) %>%
    dplyr::summarise(
      player_name        = dplyr::first(player_name),
      targets            = sum(targets, na.rm = TRUE),
      rushes             = sum(rushes, na.rm = TRUE),
      redzone_opps       = sum(redzone_opportunities, na.rm = TRUE),
      team_targets       = .r56_max(team_targets),
      team_rushes        = .r56_max(team_rushes),
      team_redzone_plays = .r56_max(team_redzone_plays),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("team_"),
                                ~ dplyr::if_else(is.finite(.x), .x, NA_real_)))

  # POSITIVE AIR YARDS, from pbp directly.
  # R/08 computes team_air_yards_pos internally but its final select() drops
  # it, exposing only the raw air_yards / team_air_yards, which include the
  # negative air yards on screens. A share built on those can go negative or
  # blow past 1, so both numerator and denominator are rebuilt here with
  # pmax(air_yards, 0), matching R/08's own internal definition. [2026-09-16]
  ay_player <- pbp %>%
    dplyr::filter(.data$play_type == "pass", .data$qb_kneel == 0,
                  .data$qb_spike == 0, !is.na(.data$receiver_player_id)) %>%
    dplyr::group_by(player_id = .data$receiver_player_id, week) %>%
    dplyr::summarise(air_yards_pos = sum(pmax(.data$air_yards, 0), na.rm = TRUE),
                     .groups = "drop")

  ay_team <- pbp %>%
    dplyr::filter(.data$play_type == "pass", .data$qb_kneel == 0,
                  .data$qb_spike == 0, !is.na(.data$posteam)) %>%
    dplyr::group_by(team = .data$posteam, week) %>%
    dplyr::summarise(team_air_yards_pos = sum(pmax(.data$air_yards, 0), na.rm = TRUE),
                     .groups = "drop")

  # Carries inside the 10 straight from pbp (R/08 stops at the 20).
  inside10 <- pbp %>%
    dplyr::filter(.data$play_type == "run", .data$qb_scramble == 0,
                  .data$qb_kneel == 0, !is.na(.data$rusher_player_id),
                  !is.na(.data$yardline_100), .data$yardline_100 <= 10) %>%
    dplyr::count(player_id = .data$rusher_player_id, week, name = "carries_inside10")

  # Position AND full name from the roster. pbp carries the abbreviated form
  # ("M.Washington"), which reads poorly and, more importantly, never matches
  # the Sleeper trending universe by name -- that is why trending_adds came
  # back NA for everyone the direct gsis join missed. [2026-09-16]
  roster <- tryCatch(nflreadr::load_rosters(season), error = function(e) NULL)
  name_col <- if (!is.null(roster)) {
    intersect(c("full_name", "player_name", "display_name"), names(roster))[1]
  } else NA_character_
  pos_lk <- if (!is.null(roster)) {
    roster %>% dplyr::filter(!is.na(gsis_id)) %>%
      dplyr::distinct(gsis_id, .keep_all = TRUE) %>%
      dplyr::transmute(player_id = .data$gsis_id, position,
                       full_name = if (!is.na(name_col)) .data[[name_col]]
                                   else NA_character_)
  } else tibble::tibble(player_id = character(), position = character(),
                        full_name = character())

  # v2: efficiency block. full_join so passers with only dropbacks get a row.
  pw <- pw %>%
    dplyr::full_join(ext, by = c("player_id", "week", "team")) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(R56_EXT_COLS), ~ dplyr::coalesce(.x, 0)))

  pw <- pw %>%
    dplyr::left_join(inside10, by = c("player_id", "week")) %>%
    dplyr::left_join(ay_player, by = c("player_id", "week")) %>%
    dplyr::left_join(ay_team, by = c("team", "week")) %>%
    dplyr::left_join(pos_lk, by = "player_id")

  if (!is.null(snaps)) {
    pw <- pw %>% dplyr::full_join(snaps, by = c("player_id", "week"))
    pw$team <- dplyr::coalesce(pw$team, pw$snap_team)
    pw$snap_team <- NULL
  } else {
    pw$offense_snaps <- NA_integer_; pw$offense_pct <- NA_real_; pw$team_snaps <- NA_real_
  }
  if (!is.null(ffo)) {
    pw <- pw %>% dplyr::left_join(ffo, by = c("player_id", "week"))
  } else {
    pw$fp_act <- NA_real_; pw$fp_exp <- NA_real_
  }

  pw %>%
    dplyr::mutate(
      player_name      = dplyr::coalesce(full_name, player_name),
      carries_inside10 = dplyr::coalesce(carries_inside10, 0L),
      dplyr::across(c(targets, rushes, air_yards_pos, redzone_opps),
                    ~ dplyr::coalesce(.x, 0)),
      dplyr::across(dplyr::all_of(R56_EXT_COLS), ~ dplyr::coalesce(.x, 0)),
      opportunities = targets + rushes,
      team_opps     = team_targets + team_rushes
    ) %>%
    dplyr::select(-full_name) %>%
    dplyr::filter(!is.na(position), position %in% R56_POSITIONS)
}

#' Three grains from the player-week frame. Season-to-date shares are ratios
#' of summed numerators to summed denominators, not means of weekly shares.
.r56_aggregate <- function(pw, week) {

  share <- function(num, den) dplyr::if_else(den > 0, num / den, NA_real_)

  this_wk <- pw %>%
    dplyr::filter(.data$week == !!week) %>%
    dplyr::transmute(
      player_id, player_name, position, team,
      wk_snap_pct   = offense_pct,
      wk_targets    = targets,
      wk_tgt_share  = share(targets, team_targets),
      wk_ay_share   = share(air_yards_pos, team_air_yards_pos),
      wk_rushes     = rushes,
      wk_rush_share = share(rushes, team_rushes),
      wk_rz_share   = share(redzone_opps, team_redzone_plays),
      wk_inside10   = carries_inside10,
      wk_fp_act     = fp_act,
      wk_fp_exp     = fp_exp
    )

  # v2: this week's efficiency block
  wk_ext <- pw %>%
    dplyr::filter(.data$week == !!week) %>%
    dplyr::select(player_id, dplyr::all_of(R56_EXT_COLS)) %>%
    .r56_ext_rates() %>%
    dplyr::select(player_id, dplyr::all_of(R56_EXT_OUT_COLS)) %>%
    dplyr::rename_with(~ paste0("wk_", .x), -player_id)

  ytd <- pw %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(
      player_name_ytd = dplyr::last(player_name),
      position_ytd    = dplyr::last(position),
      team_ytd        = dplyr::last(team),
      games           = dplyr::n_distinct(week),
      ytd_snap_pct    = share(sum(offense_snaps, na.rm = TRUE),
                              sum(team_snaps, na.rm = TRUE)),
      ytd_targets     = sum(targets),
      ytd_tgt_share   = share(sum(targets), sum(team_targets, na.rm = TRUE)),
      ytd_ay_share    = share(sum(air_yards_pos), sum(team_air_yards_pos, na.rm = TRUE)),
      ytd_rushes      = sum(rushes),
      ytd_rush_share  = share(sum(rushes), sum(team_rushes, na.rm = TRUE)),
      ytd_rz_share    = share(sum(redzone_opps), sum(team_redzone_plays, na.rm = TRUE)),
      ytd_inside10    = sum(carries_inside10),
      ytd_opps        = sum(opportunities),
      ytd_opp_share   = share(sum(opportunities), sum(team_opps, na.rm = TRUE)),
      ytd_fp_act      = sum(fp_act, na.rm = TRUE),
      ytd_fp_exp      = sum(fp_exp, na.rm = TRUE),
      .groups = "drop"
    )

  # v2: season efficiency block = summed counts, then rates
  ytd_ext <- pw %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(R56_EXT_COLS), ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop") %>%
    .r56_ext_rates() %>%
    dplyr::select(player_id, dplyr::all_of(R56_EXT_OUT_COLS)) %>%
    dplyr::rename_with(~ paste0("ytd_", .x), -player_id)

  prev <- if (week > 1L) {
    pw %>%
      dplyr::filter(.data$week == !!week - 1L) %>%
      dplyr::transmute(player_id,
                       prev_snap_pct  = offense_pct,
                       prev_tgt_share = share(targets, team_targets),
                       prev_rush_share = share(rushes, team_rushes))
  } else {
    tibble::tibble(player_id = character(), prev_snap_pct = numeric(),
                   prev_tgt_share = numeric(), prev_rush_share = numeric())
  }

  ytd %>%
    dplyr::full_join(this_wk, by = "player_id") %>%
    dplyr::left_join(prev, by = "player_id") %>%
    dplyr::left_join(ytd_ext, by = "player_id") %>%
    dplyr::left_join(wk_ext, by = "player_id") %>%
    dplyr::mutate(
      player_name = dplyr::coalesce(player_name, player_name_ytd),
      position    = dplyr::coalesce(position, position_ytd),
      team        = dplyr::coalesce(team, team_ytd),
      d_snap_pct   = wk_snap_pct  - prev_snap_pct,
      d_tgt_share  = wk_tgt_share - prev_tgt_share,
      d_rush_share = wk_rush_share - prev_rush_share,
      ytd_fp_diff  = ytd_fp_exp - ytd_fp_act,
      week = week, season = R56_SEASON, schema_tag = R56_SCHEMA_TAG
    ) %>%
    dplyr::select(-player_name_ytd, -position_ytd, -team_ytd,
                  -prev_snap_pct, -prev_tgt_share, -prev_rush_share) %>%
    dplyr::arrange(position, dplyr::desc(ytd_snap_pct))
}

# ------------------------------------------------------------------------------
# TEAM TABLE
# ------------------------------------------------------------------------------

#' 32-row team breakdown through the week. Concentration columns are the
#' combined share of the top three target and carry earners.
build_team_table <- function(pbp, pw, week) {

  clean <- pbp %>%
    dplyr::filter(!is.na(posteam), play_type %in% c("pass", "run"),
                  qb_kneel == 0, qb_spike == 0)

  games <- clean %>% dplyr::distinct(posteam, game_id) %>%
    dplyr::count(posteam, name = "games")

  # Points from the final score columns in pbp.
  pts <- pbp %>%
    dplyr::distinct(game_id, home_team, away_team, home_score, away_score) %>%
    tidyr::pivot_longer(c(home_team, away_team), values_to = "posteam") %>%
    dplyr::mutate(points = dplyr::if_else(name == "home_team", home_score, away_score)) %>%
    dplyr::group_by(posteam) %>%
    dplyr::summarise(points_pg = mean(points, na.rm = TRUE), .groups = "drop")

  neutral <- clean %>%
    dplyr::filter(!is.na(wp), wp >= R56_NEUTRAL_WP[1], wp <= R56_NEUTRAL_WP[2],
                  qtr %in% R56_NEUTRAL_QTRS) %>%
    dplyr::group_by(posteam) %>%
    dplyr::summarise(neutral_pass_rate = mean(play_type == "pass"),
                     neutral_plays = dplyr::n(), .groups = "drop")

  rz_trips <- clean %>%
    dplyr::filter(!is.na(yardline_100), yardline_100 <= 20) %>%
    dplyr::distinct(posteam, game_id, drive) %>%
    dplyr::count(posteam, name = "rz_trips")

  base <- clean %>%
    dplyr::group_by(posteam) %>%
    dplyr::summarise(
      plays        = dplyr::n(),
      pass_rate    = mean(play_type == "pass"),
      epa_play     = mean(epa, na.rm = TRUE),
      pass_epa     = mean(epa[play_type == "pass"], na.rm = TRUE),
      rush_epa     = mean(epa[play_type == "run"], na.rm = TRUE),
      .groups = "drop"
    )

  conc <- pw %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      top3_tgt_share  = sum(sort(tapply(targets, player_id, sum), decreasing = TRUE)[1:3],
                            na.rm = TRUE) / max(sum(targets), 1),
      top_target      = names(sort(tapply(targets, player_id, sum), decreasing = TRUE))[1],
      top3_rush_share = sum(sort(tapply(rushes, player_id, sum), decreasing = TRUE)[1:3],
                            na.rm = TRUE) / max(sum(rushes), 1),
      top_rusher      = names(sort(tapply(rushes, player_id, sum), decreasing = TRUE))[1],
      .groups = "drop"
    )
  name_lk <- pw %>% dplyr::distinct(player_id, player_name)
  conc <- conc %>%
    dplyr::left_join(name_lk, by = c("top_target" = "player_id")) %>%
    dplyr::rename(top_target_name = player_name) %>%
    dplyr::left_join(name_lk, by = c("top_rusher" = "player_id")) %>%
    dplyr::rename(top_rusher_name = player_name) %>%
    dplyr::select(-top_target, -top_rusher)

  base %>%
    dplyr::left_join(games, by = "posteam") %>%
    dplyr::left_join(pts, by = "posteam") %>%
    dplyr::left_join(neutral, by = "posteam") %>%
    dplyr::left_join(rz_trips, by = "posteam") %>%
    dplyr::left_join(conc, by = c("posteam" = "team")) %>%
    dplyr::mutate(plays_pg = plays / games,
                  rz_trips_pg = dplyr::coalesce(rz_trips, 0L) / games,
                  week = week, season = R56_SEASON, schema_tag = R56_SCHEMA_TAG) %>%
    dplyr::select(team = posteam, games, points_pg, plays_pg, pass_rate,
                  neutral_pass_rate, neutral_plays, rz_trips_pg,
                  epa_play, pass_epa, rush_epa,
                  top3_tgt_share, top_target_name, top3_rush_share, top_rusher_name,
                  week, season, schema_tag) %>%
    dplyr::arrange(dplyr::desc(epa_play))
}

# ------------------------------------------------------------------------------
# WEEK LOGS  [2026-09-22, v1.1; v2 adds the efficiency block]
# ------------------------------------------------------------------------------

#' One row per player per game week, weeks 1..N, each week's own shares and
#' rates. Same definitions as the wk_ columns in .r56_aggregate(), applied to
#' every week in pw instead of only the run week. Season totals are NOT built
#' here: they live in the players file's ytd_ columns (summed numerator over
#' summed denominator), and R/57 reads them from there.
build_player_weeks <- function(pw, week) {

  share <- function(num, den) dplyr::if_else(den > 0, num / den, NA_real_)

  dup <- pw %>% dplyr::count(player_id, week) %>% dplyr::filter(n > 1L)
  if (nrow(dup) > 0L) {
    warning(glue("R/56 player-week log: {nrow(dup)} player-week(s) carry more ",
                 "than one row (team split inside a week). R/57 shows the ",
                 "first row for those cells."), call. = FALSE)
  }

  pw %>%
    .r56_ext_rates() %>%
    dplyr::transmute(
      player_id, player_name, position, team,
      game_week     = .data$week,
      offense_snaps,
      snap_pct      = offense_pct,
      targets,
      tgt_share     = share(targets, team_targets),
      ay_share      = share(air_yards_pos, team_air_yards_pos),
      rushes,
      rush_share    = share(rushes, team_rushes),
      rz_share      = share(redzone_opps, team_redzone_plays),
      inside10      = carries_inside10,
      opps          = opportunities,
      opp_share     = share(opportunities, team_opps),
      fp_act, fp_exp,
      dplyr::across(dplyr::all_of(R56_EXT_OUT_COLS)),
      # !! forces the function argument; bare `week` would hit pw's column.
      through_week  = !!week, season = R56_SEASON, schema_tag = R56_SCHEMA_TAG
    ) %>%
    dplyr::arrange(player_id, game_week)
}

#' The team table computed one game week at a time. Reuses build_team_table()
#' on a single week's pbp and player-week rows, so every column keeps its
#' season-table definition with games = 1. A team on bye has no row that week.
build_team_weeks <- function(pbp, pw, week) {
  wk_tables <- lapply(seq_len(week), function(w) {
    pbp_w <- pbp %>% dplyr::filter(.data$week == !!w)
    pw_w  <- pw  %>% dplyr::filter(.data$week == !!w, !is.na(team))
    if (nrow(pbp_w) == 0L) return(NULL)
    build_team_table(pbp_w, pw_w, w) %>%
      dplyr::rename(game_week = week)
  })
  dplyr::bind_rows(wk_tables) %>%
    dplyr::mutate(through_week = week) %>%
    dplyr::relocate(game_week, .after = team) %>%
    dplyr::arrange(team, game_week)
}

#' Consistency gate: summed weekly counts must equal the ytd_ counts in the
#' players file. Guards against the two files drifting apart. v2 adds the
#' same check on thrown targets and carries from the efficiency block.
.r56_check_week_log <- function(player_weeks, players) {
  chk <- player_weeks %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(sum_tgt = sum(targets), sum_rush = sum(rushes),
                     sum_tgts = sum(tgts), sum_carries = sum(carries),
                     sum_db = sum(dropbacks), .groups = "drop") %>%
    dplyr::inner_join(players %>% dplyr::select(player_id, ytd_targets, ytd_rushes,
                                                ytd_tgts, ytd_carries, ytd_dropbacks),
                      by = "player_id")
  bad <- chk %>% dplyr::filter(abs(sum_tgt - ytd_targets) > 1e-9 |
                               abs(sum_rush - ytd_rushes) > 1e-9 |
                               abs(sum_tgts - ytd_tgts) > 1e-9 |
                               abs(sum_carries - ytd_carries) > 1e-9 |
                               abs(sum_db - ytd_dropbacks) > 1e-9)
  if (nrow(bad) > 0L) {
    stop(glue("R/56 week-log gate FAILED: {nrow(bad)} player(s) whose weekly ",
              "targets, rushes, thrown targets, carries or dropbacks do not sum ",
              "to ytd. Nothing written."), call. = FALSE)
  }
  message(glue("  week-log gate: PASS ({nrow(chk)} players, weekly counts sum to ytd)"))
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# UNDER-THE-RADAR SCREEN
# ------------------------------------------------------------------------------

#' Usage rank (season-to-date opportunity share within position) against the
#' preseason ADP position rank. Gap is market rank minus usage rank; positive
#' means the market drafted him later than his usage says.
build_radar_screen <- function(players, adp, trending, week) {

  base <- players %>%
    dplyr::filter(ytd_opps >= R56_MIN_OPPS) %>%
    dplyr::group_by(position) %>%
    dplyr::mutate(
      opp_rank  = dplyr::min_rank(dplyr::desc(ytd_opp_share)),
      snap_rank = dplyr::min_rank(dplyr::desc(ytd_snap_pct))
    ) %>%
    dplyr::ungroup()

  if (!is.null(adp)) {
    base <- base %>% dplyr::left_join(adp, by = "player_id")
    base <- base %>%
      dplyr::group_by(position) %>%
      dplyr::mutate(
        adp_pos_rank = dplyr::min_rank(adp_rank),
        adp_pos_rank = dplyr::if_else(is.na(adp_rank),
                                      max(adp_pos_rank, na.rm = TRUE) + 1L,
                                      adp_pos_rank),
        undrafted = is.na(adp_rank)
      ) %>%
      dplyr::ungroup()
  } else {
    base$adp_rank <- NA_real_; base$adp_pos_rank <- NA_integer_; base$undrafted <- NA
  }

  if (!is.null(trending)) {
    direct <- trending %>% dplyr::filter(!is.na(player_id)) %>%
      dplyr::select(player_id, trending_adds)
    by_name <- trending %>% dplyr::filter(is.na(player_id)) %>%
      dplyr::select(name_key, position, trending_adds_nm = trending_adds)
    base <- base %>%
      dplyr::left_join(direct, by = "player_id") %>%
      dplyr::mutate(name_key = .r56_norm_name(player_name)) %>%
      dplyr::left_join(by_name, by = c("name_key", "position")) %>%
      dplyr::mutate(trending_adds = dplyr::coalesce(trending_adds, trending_adds_nm)) %>%
      dplyr::select(-trending_adds_nm, -name_key)
  } else {
    base$trending_adds <- NA_integer_
  }

  base %>%
    dplyr::mutate(
      market_gap   = adp_pos_rank - opp_rank,
      under_radar  = opp_rank <= R56_RADAR_TOP &
                     !is.na(market_gap) & market_gap >= R56_RADAR_GAP,
      role_change  = !is.na(d_snap_pct) & d_snap_pct >= R56_ROLE_CHANGE_DELTA,
      market_caught_up = !is.na(trending_adds)
    ) %>%
    dplyr::select(player_id, player_name, position, team, games,
                  ytd_opp_share, opp_rank, ytd_snap_pct, snap_rank,
                  wk_snap_pct, d_snap_pct,
                  ytd_tgt_share, ytd_rush_share, ytd_rz_share, ytd_inside10,
                  ytd_fp_exp, ytd_fp_act, ytd_fp_diff,
                  adp_rank, adp_pos_rank, undrafted, market_gap,
                  trending_adds, market_caught_up,
                  under_radar, role_change, week) %>%
    dplyr::arrange(dplyr::desc(under_radar), dplyr::desc(market_gap), opp_rank)
}

# ------------------------------------------------------------------------------
# ENTRY POINT
# ------------------------------------------------------------------------------

#' Run the weekly usage report.
#' @param week Integer 1-18. The week that just finished.
#' @param season Integer.
#' @param write Logical. Write the five CSVs to R56_OUT_DIR.
#' @param n_print Rows per printed section.
#' @param require_ftn Logical. Stop if FTN charting for `week` is not
#'   published yet (default). FALSE proceeds with NA in the FTN-based columns.
#' @return Invisibly, list(players, teams, radar, player_weeks, team_weeks).
run_weekly_usage <- function(week, season = R56_SEASON, write = TRUE,
                             n_print = 15L, require_ftn = TRUE) {

  week <- as.integer(week)
  stopifnot(week >= 1L, week <= 18L)
  message(glue("\n{strrep('=', 70)}\nR/56 WEEKLY USAGE -- Season {season}, through week {week}\n",
               "{strrep('=', 70)}\n[1/6] Loading sources..."))

  pbp      <- .r56_load_pbp(season, week)
  ftn      <- .r56_load_ftn(season, week, require_week = require_ftn)
  snaps    <- .r56_load_snaps(season, week)
  ffo      <- .r56_load_ffo(season, week)
  adp      <- .r56_load_adp(season)
  trending <- .r56_sleeper_trending()
  message("  routes: not available for this season (participation 404)")

  message("[2/6] Player-week assembly...")
  ext     <- .r56_player_week_ext(pbp, ftn)
  pw      <- .r56_player_week(pbp, snaps, ffo, ext, season, week)
  message(glue("  {nrow(pw)} player-weeks, {dplyr::n_distinct(pw$player_id)} players, ",
               "{sum(pw$position == 'QB' & pw$dropbacks > 0)} QB dropback games"))
  message("[3/6] Aggregating grains...")
  players <- .r56_aggregate(pw, week)
  message("[4/6] Team table...")
  teams   <- build_team_table(pbp, pw %>% dplyr::filter(!is.na(team)), week)
  message("[5/6] Under-the-radar screen...")
  radar   <- build_radar_screen(players, adp, trending, week)
  message("[6/6] Week logs...")
  player_weeks <- build_player_weeks(pw, week)
  team_weeks   <- build_team_weeks(pbp, pw, week)
  .r56_check_week_log(player_weeks, players)

  if (isTRUE(write)) {
    dir.create(R56_OUT_DIR, recursive = TRUE, showWarnings = FALSE)
    tag <- sprintf("w%02d", week)
    readr::write_csv(players, file.path(R56_OUT_DIR, glue("s3_r56_players_{tag}.csv")))
    readr::write_csv(teams,   file.path(R56_OUT_DIR, glue("s3_r56_teams_{tag}.csv")))
    readr::write_csv(radar,   file.path(R56_OUT_DIR, glue("s3_r56_radar_{tag}.csv")))
    readr::write_csv(player_weeks,
                     file.path(R56_OUT_DIR, glue("s3_r56_player_weeks_{tag}.csv")))
    readr::write_csv(team_weeks,
                     file.path(R56_OUT_DIR, glue("s3_r56_team_weeks_{tag}.csv")))
    message(glue("  Wrote 5 files to {R56_OUT_DIR} ({tag}); players file has ",
                 "{ncol(players)} columns"))
  }

  # ---- console report ----
  pr <- function(df) print(as.data.frame(df), row.names = FALSE, digits = 3)

  cat(glue("\n\n--- UNDER THE RADAR (opp rank top {R56_RADAR_TOP}, market gap >= ",
           "{R56_RADAR_GAP}, min {R56_MIN_OPPS} opps) ---\n\n"))
  radar %>% dplyr::filter(under_radar) %>%
    dplyr::select(player_name, position, team, ytd_opp_share, opp_rank,
                  ytd_snap_pct, adp_pos_rank, market_gap, trending_adds,
                  ytd_fp_exp, ytd_fp_act) %>%
    head(n_print) %>% pr()

  if (week > 1L) {
    cat("\n--- ROLE CHANGES (snap pct up >= ",
        R56_ROLE_CHANGE_DELTA, " week over week) ---\n\n")
    radar %>% dplyr::filter(role_change) %>%
      dplyr::select(player_name, position, team, wk_snap_pct, d_snap_pct,
                    ytd_tgt_share, ytd_rush_share) %>%
      dplyr::arrange(dplyr::desc(d_snap_pct)) %>% head(n_print) %>% pr()
  }

  cat("\n--- EXPECTED POINTS NOT YET CASHED (ytd exp minus actual, top) ---\n\n")
  radar %>% dplyr::filter(!is.na(ytd_fp_diff)) %>%
    dplyr::arrange(dplyr::desc(ytd_fp_diff)) %>%
    dplyr::select(player_name, position, team, ytd_fp_exp, ytd_fp_act,
                  ytd_fp_diff, ytd_opp_share) %>%
    head(n_print) %>% pr()

  for (p in c("RB", "WR", "TE")) {
    cat(glue("\n--- {p}: top snap pct, season to date ---\n\n"))
    players %>% dplyr::filter(position == p) %>%
      dplyr::select(player_name, team, games, ytd_snap_pct, ytd_tgt_share,
                    ytd_rush_share, ytd_rz_share, wk_snap_pct, d_snap_pct) %>%
      head(n_print) %>% pr()
  }

  cat("\n--- QB: EPA per dropback, season to date (min 30 dropbacks) ---\n\n")
  players %>% dplyr::filter(position == "QB", ytd_dropbacks >= 30) %>%
    dplyr::select(player_name, team, ytd_dropbacks, ytd_pass_epa_db, ytd_pass_cpoe,
                  ytd_pass_adot, ytd_pass_sack_rate, ytd_pass_blitz_rate_faced,
                  ytd_pass_blitz_epa_db, ytd_pass_int_worthy_rate) %>%
    dplyr::arrange(dplyr::desc(ytd_pass_epa_db)) %>% head(n_print) %>% pr()

  cat("\n--- RB: rush conversion and box, season to date (min 20 carries) ---\n\n")
  players %>% dplyr::filter(position == "RB", ytd_carries >= 20) %>%
    dplyr::select(player_name, team, ytd_carries, ytd_rush_ypc, ytd_rush_epa_carry,
                  ytd_rush_conv_rate, ytd_rush_stuff_rate, ytd_rush_light_box_rate,
                  ytd_gl_carries) %>%
    dplyr::arrange(dplyr::desc(ytd_rush_conv_rate)) %>% head(n_print) %>% pr()

  cat("\n--- WR/TE: target quality, season to date (min 15 targets) ---\n\n")
  players %>% dplyr::filter(position %in% c("WR", "TE"), ytd_tgts >= 15) %>%
    dplyr::select(player_name, position, team, ytd_tgts, ytd_rec_adot,
                  ytd_rec_epa_tgt, ytd_rec_deep_share, ytd_ez_targets,
                  ytd_rec_catchable_rate, ytd_rec_drop_rate) %>%
    dplyr::arrange(dplyr::desc(ytd_rec_epa_tgt)) %>% head(n_print) %>% pr()

  cat("\n--- TEAMS (by EPA/play) ---\n\n")
  teams %>%
    dplyr::select(team, points_pg, plays_pg, pass_rate, neutral_pass_rate,
                  rz_trips_pg, epa_play, top3_tgt_share, top_target_name,
                  top3_rush_share, top_rusher_name) %>% pr()

  invisible(list(players = players, teams = teams, radar = radar,
                 player_weeks = player_weeks, team_weeks = team_weeks))
}

# RUN
#   source(here::here("R", "56_weekly_usage_report.R"))
#   out <- run_weekly_usage(week = 3)
