# ==============================================================================
# NFL Analytics Toolkit -- Season 3
# R/57: Weekly Usage Explorer (local Shiny)
# File: R/57_usage_explorer_app.R
#
# PURPOSE
# -------
# An interactive filter surface over R/56's weekly usage output and (v2)
# R/62's team splits and matchup output. R/56 and R/62 print fixed head()
# tables; this app lets you cut every frame they write any way you want.
#
# It does NOT recompute anything. R/56 and R/62 own every number here. The app
# reads the CSVs they already wrote, or calls run_weekly_usage() once and holds
# the result in memory for the session.
#
# RUN (fresh R session, working directory = project root)
# -------------------------------------------------------
#   source(here::here("R", "57_usage_explorer_app.R"))
#   run_usage_explorer()            # reads the newest R/56 (+ R/62) CSVs on disk
#   run_usage_explorer(week = 3)    # a specific week
#   run_usage_explorer(week = 3, recompute = TRUE)   # re-runs R/56 first
#
# TABS (v2)
# ---------
#   Radar          R/56 screen with team context joined (unchanged)
#   Players        R/56 players file; the View picker switches between Usage
#                  (v1 columns), Receiving, Rushing and Passing column sets
#   By Week        one column per game week for a chosen stat; v2 adds the
#                  efficiency rates to the stat picker
#   Teams          R/56 team table (unchanged)
#   Defense        R/62 team file, defense rows. Every "epa_*" here is EPA
#                  ALLOWED; lower is better. Views group the columns.
#   Off Tendencies R/62 team file, offense rows. Same views.
#   Situations     R/62 grid: down x distance x field zone, raw + shrunk +
#                  league cell value + 2025 baseline, either side
#   Splits         R/62 long cuts: blitz, box, play action, screen, depth,
#                  target position, pass location, run gap, score state,
#                  neutral, half, third-down distance, blitz x situation
#   Matchups       R/62 next-week pairings: offense value vs defense value
#                  vs league on the same cut, with naive_expect
#   Notes, Dictionary
#
# Every R/62 tab is empty with a one-line message when no s3_r62_* file exists
# for the loaded week. Run run_weekly_matchup(week) to fill them.
#
# WHAT THE TEAM CONTEXT COLUMN IS FOR (read this)
# -----------------------------------------------
# The radar screen ranks OPPORTUNITY SHARE. Share is a fraction of a team's
# pie, and it says nothing about the size of the pie. In 2026 week 1 the two
# highest-gap players were both Miami receivers on an offense at -0.236 EPA per
# play that scored 13 points. A big share of very little.
#
# So the Radar tab joins each player's team EPA per play, plays per game and
# pass rate onto his row, and the default sort is market gap. Sorting by
# team_epa_play is how you separate "the market missed him" from "the market
# correctly priced his offense". The app does not make that call for you and
# no column in it should be read as a verdict.
#
# ONE-GAME CAVEAT
# ---------------
# Ranks built on one game reshuffle hard. The app prints the games count in
# the header and every table carries `games` or `n`. Below about week 4 treat
# the ordering as a conversation starter, not a ranking. The Situations tab
# exists precisely because 27 cells against three games is thin: read the
# shrunk column there, not the raw one, until n is comfortable.
#
# Not deployable. Same reason as R/55: here() anchors to the PROJECT root and
# the data lives under data/season3_cache/ there, not inside a bundle. There is
# nothing private in these frames, but the paths are local.
#
# DEPENDENCIES
#   R/56_weekly_usage_report.R  (sourced below; needed for recompute and for
#                                the R56_ constants used as filter defaults)
#   R/62_weekly_matchup_report.R (sourced if present; supplies the metric
#                                registry used to build the dictionary)
#   shiny, DT, dplyr, tidyr, glue, readr, here
#
# SCHEMA TAG: s3_r57_app_v2
# Author: Christian K. LeBlanc
# Version: 1.0
#          1.1  [2026-09-22] By Week tab; Teams tab can show one game week.
#          2.0  [2026-09-29] R/62 tabs (Defense, Off Tendencies, Situations,
#               Splits, Matchups); Players view picker for the R/56 v2
#               efficiency block; opponent filter; dictionary covers every
#               column in both scripts' files.
# ==============================================================================

library(shiny)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(glue); library(readr); library(here)
})

if (!requireNamespace("DT", quietly = TRUE)) {
  stop("R/57 needs the DT package for sortable tables. install.packages('DT')",
       call. = FALSE)
}

if (!exists("run_weekly_usage")) {
  source(here::here("R", "56_weekly_usage_report.R"))
}
if (!exists("run_weekly_matchup") &&
    file.exists(here::here("R", "62_weekly_matchup_report.R"))) {
  source(here::here("R", "62_weekly_matchup_report.R"))
}

R57_SCHEMA_TAG <- "s3_r57_app_v2"
R57_CACHE_DIR  <- R56_OUT_DIR

# ------------------------------------------------------------------------------
# DATA ACCESS
# ------------------------------------------------------------------------------

#' Weeks that have an R/56 player file on disk, newest first.
.r57_available_weeks <- function(dir = R57_CACHE_DIR) {
  f <- list.files(dir, pattern = "^s3_r56_players_w[0-9]{2}\\.csv$")
  if (length(f) == 0L) return(integer(0))
  sort(as.integer(sub("^s3_r56_players_w([0-9]{2})\\.csv$", "\\1", f)),
       decreasing = TRUE)
}

#' Load one week's frames. Reads the CSVs unless recompute is TRUE, in which
#' case R/56 is run and its return value used directly. R/62 files are always
#' read from disk and are optional.
.r57_load_week <- function(week, recompute = FALSE, dir = R57_CACHE_DIR) {

  tag <- sprintf("w%02d", week)
  read_opt <- function(prefix, stub, quiet = FALSE) {
    f <- file.path(dir, glue("{prefix}_{stub}_{tag}.csv"))
    if (!file.exists(f)) {
      if (!quiet) message(glue("R/57: {basename(f)} not found."))
      return(NULL)
    }
    readr::read_csv(f, show_col_types = FALSE, progress = FALSE)
  }

  if (isTRUE(recompute)) {
    message(glue("R/57: recomputing week {week} via run_weekly_usage()..."))
    out <- run_weekly_usage(week = week, write = TRUE, n_print = 0L)
  } else {
    paths <- file.path(dir, glue("s3_r56_{c('players','teams','radar')}_{tag}.csv"))
    missing <- paths[!file.exists(paths)]
    if (length(missing) > 0L) {
      stop(glue("R/57: no R/56 output for week {week}. Missing:\n  ",
                paste(basename(missing), collapse = "\n  "),
                "\nRun run_weekly_usage(week = {week}) first, or launch with ",
                "recompute = TRUE."), call. = FALSE)
    }
    out <- list(
      players = readr::read_csv(paths[1], show_col_types = FALSE, progress = FALSE),
      teams   = readr::read_csv(paths[2], show_col_types = FALSE, progress = FALSE),
      radar   = readr::read_csv(paths[3], show_col_types = FALSE, progress = FALSE),
      player_weeks = read_opt("s3_r56", "player_weeks"),
      team_weeks   = read_opt("s3_r56", "team_weeks")
    )
  }
  # R/62 (optional). Missing files leave the tabs empty with a message.
  out$r62_team       <- read_opt("s3_r62", "team", quiet = TRUE)
  out$r62_situation  <- read_opt("s3_r62", "situation", quiet = TRUE)
  out$r62_splits     <- read_opt("s3_r62", "splits", quiet = TRUE)
  out$r62_matchup    <- read_opt("s3_r62", "matchup", quiet = TRUE)
  out$r62_team_weeks <- read_opt("s3_r62", "team_weeks", quiet = TRUE)
  if (is.null(out$r62_team)) {
    message(glue("R/57: no R/62 output for week {week}; Defense, Off Tendencies, ",
                 "Situations, Splits and Matchups tabs are empty. Run ",
                 "run_weekly_matchup(week = {week})."))
  }
  out
}

# ------------------------------------------------------------------------------
# BY WEEK  [2026-09-22, v1.1; v2 adds the efficiency rates]
# ------------------------------------------------------------------------------

#' Stat picker map: label -> weekly column in player_weeks, season column in
#' players, and whether it displays as a percentage.
R57_BYWEEK_STATS <- tibble::tribble(
  ~label,                        ~wk_col,       ~ytd_col,           ~is_pct,
  "Snap %",                      "snap_pct",    "ytd_snap_pct",     TRUE,
  "Target share",                "tgt_share",   "ytd_tgt_share",    TRUE,
  "Air yards share",             "ay_share",    "ytd_ay_share",     TRUE,
  "Rush share",                  "rush_share",  "ytd_rush_share",   TRUE,
  "Red zone share",              "rz_share",    "ytd_rz_share",     TRUE,
  "Opportunity share",           "opp_share",   "ytd_opp_share",    TRUE,
  "Targets",                     "targets",     "ytd_targets",      FALSE,
  "Rushes",                      "rushes",      "ytd_rushes",       FALSE,
  "Opportunities",               "opps",        "ytd_opps",         FALSE,
  "Carries inside 10",           "inside10",    "ytd_inside10",     FALSE,
  "Fantasy pts (actual)",        "fp_act",      "ytd_fp_act",       FALSE,
  "Fantasy pts (expected)",      "fp_exp",      "ytd_fp_exp",       FALSE,
  # v2 receiving
  "aDOT (receiver)",             "rec_adot",    "ytd_rec_adot",     FALSE,
  "EPA per target",              "rec_epa_tgt", "ytd_rec_epa_tgt",  FALSE,
  "Deep target share",           "rec_deep_share", "ytd_rec_deep_share", TRUE,
  "End zone targets",            "ez_targets",  "ytd_ez_targets",   FALSE,
  "Red zone targets",            "rz_targets",  "ytd_rz_targets",   FALSE,
  "Catchable target rate",       "rec_catchable_rate", "ytd_rec_catchable_rate", TRUE,
  "Drop rate",                   "rec_drop_rate", "ytd_rec_drop_rate", TRUE,
  "Targets vs blitz",            "tgt_vs_blitz", "ytd_tgt_vs_blitz", FALSE,
  # v2 rushing
  "Carries",                     "carries",     "ytd_carries",      FALSE,
  "Yards per carry",             "rush_ypc",    "ytd_rush_ypc",     FALSE,
  "EPA per carry",               "rush_epa_carry", "ytd_rush_epa_carry", FALSE,
  "Rush conversion rate",        "rush_conv_rate", "ytd_rush_conv_rate", TRUE,
  "Stuff rate",                  "rush_stuff_rate", "ytd_rush_stuff_rate", TRUE,
  "Light box rate",              "rush_light_box_rate", "ytd_rush_light_box_rate", TRUE,
  "Goal line carries (<= 5)",    "gl_carries",  "ytd_gl_carries",   FALSE,
  # v2 passing
  "Dropbacks",                   "dropbacks",   "ytd_dropbacks",    FALSE,
  "EPA per dropback",            "pass_epa_db", "ytd_pass_epa_db",  FALSE,
  "CPOE",                        "pass_cpoe",   "ytd_pass_cpoe",    FALSE,
  "aDOT (passer)",               "pass_adot",   "ytd_pass_adot",    FALSE,
  "Sack rate",                   "pass_sack_rate", "ytd_pass_sack_rate", TRUE,
  "Blitz rate faced",            "pass_blitz_rate_faced", "ytd_pass_blitz_rate_faced", TRUE,
  "EPA per dropback vs blitz",   "pass_blitz_epa_db", "ytd_pass_blitz_epa_db", FALSE,
  "Play action rate",            "pass_pa_rate", "ytd_pass_pa_rate", TRUE,
  "Interception-worthy rate",    "pass_int_worthy_rate", "ytd_pass_int_worthy_rate", TRUE
)

#' Reshape to one row per player, W1..WN columns, then Season from ytd_.
#' Display reshape only. No arithmetic on the values.
.r57_by_week <- function(player_weeks, players, stat_label) {
  st <- R57_BYWEEK_STATS[R57_BYWEEK_STATS$label == stat_label, ]
  if (is.null(player_weeks) || nrow(player_weeks) == 0L || nrow(st) != 1L ||
      !st$wk_col %in% names(player_weeks) || !st$ytd_col %in% names(players)) {
    return(list(df = NULL, week_cols = character(), is_pct = FALSE, n_dup = 0L))
  }
  long <- player_weeks %>%
    dplyr::select(player_id, game_week, value = dplyr::all_of(st$wk_col))
  n_dup <- long %>% dplyr::count(player_id, game_week) %>%
    dplyr::filter(n > 1L) %>% nrow()
  weeks <- sort(unique(long$game_week))
  wide <- long %>%
    dplyr::mutate(game_week = factor(paste0("W", game_week),
                                     levels = paste0("W", weeks))) %>%
    tidyr::pivot_wider(names_from = game_week, values_from = value,
                       values_fn = dplyr::first)
  df <- players %>%
    dplyr::select(player_id, player_name, position, team, games,
                  dplyr::any_of(c("ytd_snap_pct", "ytd_opp_share")),
                  Season = dplyr::all_of(st$ytd_col)) %>%
    dplyr::left_join(wide, by = "player_id") %>%
    dplyr::relocate(Season, .after = dplyr::last_col())
  list(df = df, week_cols = paste0("W", weeks), is_pct = st$is_pct, n_dup = n_dup)
}

#' Attach team context to the radar frame. See the header note: share without
#' the size of the pie is half the picture.
.r57_join_team_context <- function(radar, teams) {
  radar %>%
    dplyr::left_join(
      teams %>% dplyr::select(team,
                              team_epa_play = "epa_play",
                              team_plays_pg = "plays_pg",
                              team_pass_rate = "pass_rate",
                              team_points_pg = "points_pg",
                              team_rz_trips_pg = "rz_trips_pg"),
      by = "team"
    )
}

# ------------------------------------------------------------------------------
# COLUMN SETS
# ------------------------------------------------------------------------------

# Players tab views (R/56 v2). Each is a column set at both grains.
R57_PLAYER_VIEWS <- list(
  "Usage" = c("ytd_snap_pct", "wk_snap_pct", "d_snap_pct",
              "ytd_tgt_share", "wk_tgt_share", "d_tgt_share",
              "ytd_rush_share", "wk_rush_share", "d_rush_share",
              "ytd_ay_share", "ytd_rz_share", "ytd_inside10",
              "ytd_targets", "ytd_rushes", "ytd_opps", "ytd_opp_share",
              "ytd_fp_exp", "ytd_fp_act", "ytd_fp_diff", "wk_fp_exp", "wk_fp_act"),
  "Receiving" = c("ytd_tgts", "wk_tgts", "ytd_receptions", "ytd_rec_yards", "ytd_rec_tds",
                  "ytd_rec_adot", "wk_rec_adot", "ytd_rec_epa_tgt", "wk_rec_epa_tgt",
                  "ytd_rec_ypt", "ytd_rec_catch_rate", "ytd_rec_fd_rate",
                  "ytd_rec_deep_share", "ytd_tgt_deep", "ytd_ez_targets", "ytd_rz_targets",
                  "ytd_rec_yac_per_rec", "ytd_rec_catchable_rate", "ytd_rec_contested_rate",
                  "ytd_rec_drop_rate", "ytd_rec_created_rate",
                  "ytd_tgt_vs_blitz", "ytd_rec_epa_vs_blitz"),
  "Rushing" = c("ytd_carries", "wk_carries", "ytd_rush_yards", "ytd_rush_tds",
                "ytd_rush_ypc", "wk_rush_ypc", "ytd_rush_epa_carry", "wk_rush_epa_carry",
                "ytd_rush_conv_rate", "wk_rush_conv_rate", "ytd_rush_stuff_rate",
                "ytd_rush_explosive_rate", "ytd_rush_fd_rate",
                "ytd_rush_light_box_rate", "ytd_rush_stacked_box_rate",
                "ytd_gl_carries", "ytd_sy_carries", "ytd_rush_sy_conv_rate"),
  "Passing" = c("ytd_dropbacks", "wk_dropbacks", "ytd_pass_epa_db", "wk_pass_epa_db",
                "ytd_pass_cpoe", "wk_pass_cpoe", "ytd_pass_comp_pct", "ytd_pass_adot",
                "ytd_pass_ypa", "ytd_pass_deep_rate", "ytd_pass_yards", "ytd_pass_tds",
                "ytd_ints", "ytd_pass_fd_rate", "ytd_sacks", "ytd_pass_sack_rate",
                "ytd_pass_scramble_rate", "ytd_pass_pa_rate", "ytd_pass_pa_epa_db",
                "ytd_db_vs_blitz", "ytd_pass_blitz_rate_faced", "ytd_pass_blitz_epa_db",
                "ytd_pass_int_worthy_rate", "ytd_pass_throwaway_rate",
                "ytd_pass_oop_rate", "ytd_pass_qb_fault_sack_share")
)

# R/62 team file views (same sets serve Defense and Off Tendencies).
R57_TEAM62_VIEWS <- list(
  "Overview" = c("games", "plays_pg", "points_pg", "epa_play", "rank_epa_play",
                 "success_rate", "explosive_rate", "first_down_rate",
                 "epa_dropback", "rank_epa_dropback", "epa_rush", "rank_epa_rush",
                 "pass_rate", "pass_oe", "neutral_pass_rate", "neutral_epa_play",
                 "base_epa_play"),
  "Pass rush and box" = c("n_dropback_ftn", "blitz_rate", "rank_blitz_rate",
                          "avg_pass_rushers", "rush3_or_fewer_rate", "rush6_plus_rate",
                          "pressure_rate", "rank_pressure_rate", "sack_rate",
                          "qb_fault_sack_share", "scramble_rate", "out_of_pocket_rate",
                          "avg_def_box", "light_box_rate", "stacked_box_rate",
                          "neutral_blitz_rate", "base_blitz_rate", "base_pressure_rate"),
  "Coverage" = c("n_thrown", "comp_pct", "cpoe", "adot", "epa_target", "yac_per_comp",
                 "deep_target_rate", "explosive_pass_rate", "int_rate", "int_worthy_rate",
                 "catchable_rate", "contested_rate", "drop_rate", "created_rec_rate",
                 "throwaway_rate", "pa_rate", "screen_rate", "rpo_rate"),
  "Run" = c("n_is_rush", "epa_rush", "rank_epa_rush", "success_rush", "ypc",
            "stuff_rate", "rush_conv_rate", "explosive_rush_rate",
            "avg_def_box", "light_box_rate", "stacked_box_rate", "motion_rate",
            "base_epa_rush", "base_success_rush"),
  "Drives and pace" = c("drives", "points_per_drive", "scoring_drive_rate",
                        "turnover_drive_rate", "three_and_out_rate", "plays_per_drive",
                        "sec_per_play", "no_huddle_rate", "dropbacks_pg",
                        "penalties_pg", "penalty_yards_pg"),
  "All" = character()
)

# Splits tab default column set (the full row is available with the toggle).
R57_SPLIT_COLS <- c("n_is_play", "n_dropback", "n_dropback_ftn", "n_is_rush", "n_thrown",
                    "epa_play", "league_epa_play", "success_rate", "league_success_rate",
                    "epa_dropback", "league_epa_dropback", "success_dropback",
                    "sack_rate", "pressure_rate", "blitz_rate", "league_blitz_rate",
                    "epa_rush", "league_epa_rush", "success_rush", "stuff_rate",
                    "epa_target", "league_epa_target", "comp_pct", "adot",
                    "explosive_rate", "first_down_rate")

R57_SHARE_COLS <- c("ytd_opp_share", "ytd_snap_pct", "wk_snap_pct", "d_snap_pct",
                    "ytd_tgt_share", "ytd_rush_share", "ytd_rz_share",
                    "ytd_ay_share", "wk_tgt_share", "wk_rush_share",
                    "wk_rz_share", "wk_ay_share", "d_tgt_share", "d_rush_share",
                    "pass_rate", "neutral_pass_rate", "top3_tgt_share",
                    "top3_rush_share")

# Every rate column that reads better as a percentage: anything ending in
# _rate, _share or _pct (plus the named share columns), except counts (n_) and
# ranks (rank_), which are integers whatever their suffix.
.r57_pct_cols <- function(df) {
  nm <- names(df)
  hit <- (grepl("(_rate|_share|_pct|success_dropback|success_rush)$", nm) | nm %in% R57_SHARE_COLS) &
    !grepl("^(n_|rank_)", nm)
  nm[hit & vapply(df, is.numeric, logical(1))]
}

# ------------------------------------------------------------------------------
# DISPLAY HELPERS
# ------------------------------------------------------------------------------

#' A DT table with sane defaults: sortable, paged, numbers rounded, share
#' columns shown as percentages so 0.2105 reads as 21.1%.
.r57_dt <- function(df, pct_cols = character(), digits = 3, page = 25L,
                    message = "No rows match these filters.") {
  if (is.null(df) || nrow(df) == 0L) {
    return(DT::datatable(data.frame(Message = message),
                         rownames = FALSE, options = list(dom = "t")))
  }
  pct_cols <- intersect(pct_cols, names(df))
  num_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], pct_cols)
  dt <- DT::datatable(
    df, rownames = FALSE, filter = "top",
    extensions = "Buttons",
    options = list(pageLength = page, scrollX = TRUE,
                   dom = "Bfrtip", buttons = c("copy", "csv"),
                   order = list())
  )
  if (length(pct_cols) > 0L) dt <- DT::formatPercentage(dt, pct_cols, 1)
  if (length(num_cols) > 0L) dt <- DT::formatRound(dt, num_cols, digits)
  dt
}

.r57_no62 <- "No R/62 file for this week. Run run_weekly_matchup(week = N) and reload."

# ------------------------------------------------------------------------------
# DATA DICTIONARY  [2026-09-18; v2 2026-09-29]
# Every column R/56 writes (schema s3_r56_usage_v2) and every column R/62
# writes (schema s3_r62_matchup_v1), plus the team-context columns this app
# joins onto the radar frame. Rendered in the Dictionary tab. Single source of
# truth for column meanings; edit here.
# ------------------------------------------------------------------------------
R57_DICTIONARY_BASE <- tibble::tribble(
  ~table, ~column, ~type, ~source, ~meaning,

  # ---- shared ------------------------------------------------------------------
  "all", "season", "int", "R/56 / R/62", "NFL season.",
  "all", "week", "int", "argument", "The week that just finished. Season-to-date columns run weeks 1 through this week.",
  "all", "schema_tag", "chr", "R/56 / R/62", "s3_r56_usage_v2 or s3_r62_matchup_v1.",
  "all", "(grains)", "", "R/56", "Three grains in the players file: wk_ = this week only; ytd_ = season to date, computed as summed numerator over summed denominator (not a mean of weekly rates); d_ = this week minus last week.",
  "all", "(what is missing)", "", "R/56 / R/62", "Route participation, man/zone coverage, NGS box counts and personnel groupings are NOT in these files: nflreadr load_participation(2026) returns 404. Not proxied. True pressure rate needs tracking data; pressure_rate is (sack + qb_hit) per dropback. Played games only; a DNP is not a zero.",
  "all", "(FTN)", "", "FTN charting via nflreadr", "Per-play charting joined 100% to scrimmage plays. Blitz = n_pass_rushers >= 5 (FTN's own coding: n_blitzers >= 1 is the same set). Columns that depend on FTN carry an _ftn denominator (n_dropback_ftn, tgts_ftn, thrown_ftn). FTN posts during the week; R/56 and R/62 stop by default if the run week is not in the feed yet.",

  # ---- players (v1) ----------------------------------------------------------------
  "players", "player_id", "chr", "pbp / roster", "nflreadr gsis id. Join key.",
  "players", "player_name", "chr", "roster", "Full name from the season roster (pbp's abbreviated form is not used).",
  "players", "position", "chr", "roster", "QB, RB, WR or TE from the season roster. Other positions dropped.",
  "players", "team", "chr", "pbp / snaps", "Team on the most recent game; snap-count team fills a gap.",
  "players", "games", "int", "R/56", "Distinct weeks the player appears in the player-week frame. v2: a QB with only dropbacks now has a row, so QB games count every start.",
  "players", "wk_snap_pct", "dbl", "nflreadr snap counts (offense_pct)", "Share of team offensive snaps this week, as a fraction. Keyed on pfr_player_id and crosswalked to gsis.",
  "players", "wk_targets", "int", "R/08", "Targets this week (R/08 definition).",
  "players", "wk_tgt_share", "dbl", "R/08", "Targets / team targets this week.",
  "players", "wk_ay_share", "dbl", "pbp", "Share of team POSITIVE air yards this week (pmax(air_yards, 0) in both numerator and denominator, so screens cannot push the share negative).",
  "players", "wk_rushes", "int", "R/08", "Rush attempts this week (designed runs; QB scrambles excluded).",
  "players", "wk_rush_share", "dbl", "R/08", "Rushes / team rushes this week.",
  "players", "wk_rz_share", "dbl", "R/08", "(Red-zone targets + red-zone rushes) / team red-zone plays this week. Red zone = yardline_100 <= 20.",
  "players", "wk_inside10", "int", "pbp", "Carries from the 10-yard line in, this week. Built from pbp directly (R/08 stops at the 20).",
  "players", "wk_fp_act", "dbl", "nflreadr ff_opportunity", "Actual fantasy points this week under ffopportunity's default scoring (not your league scoring).",
  "players", "wk_fp_exp", "dbl", "nflreadr ff_opportunity", "Expected fantasy points this week from ffopportunity's opportunity model.",
  "players", "ytd_snap_pct", "dbl", "snap counts", "Season snap share: summed offensive snaps / summed team snaps.",
  "players", "ytd_targets", "int", "R/08", "Season targets.",
  "players", "ytd_tgt_share", "dbl", "R/08", "Season targets / season team targets.",
  "players", "ytd_ay_share", "dbl", "pbp", "Season share of team positive air yards.",
  "players", "ytd_rushes", "int", "R/08", "Season rush attempts.",
  "players", "ytd_rush_share", "dbl", "R/08", "Season rushes / season team rushes.",
  "players", "ytd_rz_share", "dbl", "R/08", "Season red-zone opportunities / season team red-zone plays.",
  "players", "ytd_inside10", "int", "pbp", "Season carries inside the 10.",
  "players", "ytd_opps", "int", "R/56", "Season opportunities = targets + rushes.",
  "players", "ytd_opp_share", "dbl", "R/56", "Season opportunities / season team opportunities (team targets + team rushes). The rank basis for the radar screen.",
  "players", "ytd_fp_act", "dbl", "ff_opportunity", "Season actual fantasy points (ffopportunity scoring).",
  "players", "ytd_fp_exp", "dbl", "ff_opportunity", "Season expected fantasy points.",
  "players", "ytd_fp_diff", "dbl", "R/56", "ytd_fp_exp - ytd_fp_act. Positive = usage has not cashed yet; negative = scoring above what the opportunity implies.",
  "players", "d_snap_pct", "dbl", "R/56", "wk_snap_pct minus last week's. NA in week 1 or if absent last week.",
  "players", "d_tgt_share", "dbl", "R/56", "wk_tgt_share minus last week's.",
  "players", "d_rush_share", "dbl", "R/56", "wk_rush_share minus last week's.",

  # ---- players (v2 receiving; each exists as wk_ and ytd_) --------------------------
  "players", "tgts", "int", "pbp", "Thrown targets: complete, incomplete or intercepted passes with this receiver. The denominator for every rec_ rate. Can differ slightly from R/08 targets.",
  "players", "tgt_deep", "int", "pbp", "Targets with air_yards >= 20.",
  "players", "ez_targets", "int", "pbp", "Targets thrown to or past the goal line (air_yards >= yardline_100).",
  "players", "rz_targets", "int", "pbp", "Targets from the 20-yard line in.",
  "players", "receptions / rec_yards / rec_tds", "int", "pbp", "Catches, receiving yards on catches, receiving touchdowns.",
  "players", "tgt_vs_blitz", "int", "pbp + FTN", "Targets on dropbacks where the defense rushed 5+.",
  "players", "rec_adot", "dbl", "pbp", "Average depth of target: summed air_yards / tgts.",
  "players", "rec_epa_tgt", "dbl", "pbp", "EPA per target (all EPA on plays where he was the target, including incompletions and picks).",
  "players", "rec_ypt", "dbl", "pbp", "Receiving yards per target.",
  "players", "rec_catch_rate", "dbl", "pbp", "Receptions / tgts.",
  "players", "rec_fd_rate", "dbl", "pbp", "Share of targets that produced a first down.",
  "players", "rec_deep_share", "dbl", "pbp", "tgt_deep / tgts.",
  "players", "rec_yac_per_rec", "dbl", "pbp", "Yards after catch per reception.",
  "players", "rec_catchable_rate", "dbl", "FTN", "Share of FTN-charted targets judged catchable. Denominator tgts_ftn.",
  "players", "rec_contested_rate", "dbl", "FTN", "Share of charted targets judged contested.",
  "players", "rec_drop_rate", "dbl", "FTN", "Drops / charted targets.",
  "players", "rec_created_rate", "dbl", "FTN", "Share of charted targets that were a 'created reception' (receiver made the play).",
  "players", "rec_epa_vs_blitz", "dbl", "pbp + FTN", "EPA per target when the defense blitzed. Small denominators; read with tgt_vs_blitz.",

  # ---- players (v2 rushing) ----------------------------------------------------------
  "players", "carries", "int", "pbp", "Designed rushes (play_type run, qb_scramble 0). Denominator for every rush_ rate. Should equal R/08 rushes.",
  "players", "rush_yards / rush_tds", "int", "pbp", "Rushing yards and touchdowns.",
  "players", "gl_carries", "int", "pbp", "Carries from the 5-yard line in.",
  "players", "sy_carries", "int", "pbp", "Carries on 3rd or 4th down with 2 or fewer to go.",
  "players", "rush_ypc", "dbl", "pbp", "Yards per carry.",
  "players", "rush_epa_carry", "dbl", "pbp", "EPA per carry.",
  "players", "rush_conv_rate", "dbl", "pbp", "Share of carries that gained a first down or scored. The 'rush conversion rate' from the backlog.",
  "players", "rush_stuff_rate", "dbl", "pbp", "Share of carries at or behind the line (yards_gained <= 0).",
  "players", "rush_explosive_rate", "dbl", "pbp", "Share of carries of 10+ yards.",
  "players", "rush_fd_rate", "dbl", "pbp", "Share of carries that gained a first down.",
  "players", "rush_light_box_rate", "dbl", "FTN", "Share of box-charted carries against 6 or fewer in the box.",
  "players", "rush_stacked_box_rate", "dbl", "FTN", "Share of box-charted carries against 8+ in the box.",
  "players", "rush_sy_conv_rate", "dbl", "pbp", "Short-yardage conversions / sy_carries.",

  # ---- players (v2 passing) ----------------------------------------------------------
  "players", "dropbacks", "int", "pbp", "Dropbacks (pass plays incl. sacks, plus scrambles). Denominator for pass_epa_db, sack and scramble rates.",
  "players", "thrown", "int", "pbp", "Thrown balls: completions + incompletions + interceptions. Denominator for comp_pct, adot, ypa, deep_rate.",
  "players", "pass_yards / pass_tds / ints / sacks", "int", "pbp", "Passing yards, passing TDs, interceptions, sacks taken.",
  "players", "db_vs_blitz", "int", "pbp + FTN", "Dropbacks against 5+ rushers.",
  "players", "pass_epa_db", "dbl", "pbp", "EPA per dropback.",
  "players", "pass_cpoe", "dbl", "pbp", "Completion percentage over expected, averaged over throws with a cpoe value.",
  "players", "pass_comp_pct", "dbl", "pbp", "Completions / thrown.",
  "players", "pass_adot", "dbl", "pbp", "Average depth of target as the passer.",
  "players", "pass_ypa", "dbl", "pbp", "Passing yards per throw.",
  "players", "pass_deep_rate", "dbl", "pbp", "Share of throws with air_yards >= 20.",
  "players", "pass_fd_rate", "dbl", "pbp", "First downs by pass / dropbacks.",
  "players", "pass_sack_rate", "dbl", "pbp", "Sacks / dropbacks.",
  "players", "pass_scramble_rate", "dbl", "pbp", "Scrambles / dropbacks.",
  "players", "pass_pa_rate", "dbl", "FTN", "Play-action dropbacks / charted dropbacks.",
  "players", "pass_pa_epa_db", "dbl", "pbp + FTN", "EPA per play-action dropback.",
  "players", "pass_blitz_rate_faced", "dbl", "FTN", "db_vs_blitz / charted dropbacks.",
  "players", "pass_blitz_epa_db", "dbl", "pbp + FTN", "EPA per dropback when blitzed.",
  "players", "pass_int_worthy_rate", "dbl", "FTN", "Interception-worthy throws / charted throws.",
  "players", "pass_throwaway_rate", "dbl", "FTN", "Throwaways / charted dropbacks.",
  "players", "pass_oop_rate", "dbl", "FTN", "Share of charted dropbacks where the QB left the pocket.",
  "players", "pass_qb_fault_sack_share", "dbl", "FTN", "Share of charted sacks FTN attributes to the QB rather than protection.",

  # ---- teams (R/56) ----------------------------------------------------------------
  "teams", "team", "chr", "pbp posteam", "Offense.",
  "teams", "games", "int", "pbp", "Games played through the week.",
  "teams", "points_pg", "dbl", "pbp final scores", "Points scored per game.",
  "teams", "plays_pg", "dbl", "pbp", "Pass + run plays per game (kneels and spikes excluded).",
  "teams", "pass_rate", "dbl", "pbp", "Share of pass + run plays that were passes, all situations.",
  "teams", "neutral_pass_rate", "dbl", "pbp", "Pass rate with win probability between 0.20 and 0.80 in quarters 1-3. Strips game script.",
  "teams", "neutral_plays", "int", "pbp", "Plays that met the neutral-script definition. Small numbers make neutral_pass_rate noisy.",
  "teams", "rz_trips_pg", "dbl", "pbp", "Distinct drives with a snap from the 20-yard line in, per game.",
  "teams", "epa_play", "dbl", "pbp", "Mean EPA per pass + run play. Offensive efficiency.",
  "teams", "pass_epa", "dbl", "pbp", "Mean EPA per pass play.",
  "teams", "rush_epa", "dbl", "pbp", "Mean EPA per run play.",
  "teams", "top3_tgt_share", "dbl", "R/56", "Combined season target share of the three most-targeted players. High = concentrated passing game.",
  "teams", "top_target_name", "chr", "R/56", "Most-targeted player, season to date.",
  "teams", "top3_rush_share", "dbl", "R/56", "Combined season rush share of the three most-used rushers.",
  "teams", "top_rusher_name", "chr", "R/56", "Most-used rusher, season to date.",

  # ---- radar -----------------------------------------------------------------------
  "radar", "(who is in it)", "", "R/56", "Players with ytd_opps >= 5 (R56_MIN_OPPS). Everything below is season to date unless prefixed wk_ or d_.",
  "radar", "ytd_opp_share", "dbl", "players", "Season opportunity share. See players table.",
  "radar", "opp_rank", "int", "R/56", "Rank of ytd_opp_share within position, 1 = most. Ties share the lower rank.",
  "radar", "ytd_snap_pct", "dbl", "players", "Season snap share.",
  "radar", "snap_rank", "int", "R/56", "Rank of ytd_snap_pct within position.",
  "radar", "wk_snap_pct / d_snap_pct", "dbl", "players", "This week's snap share and the change from last week.",
  "radar", "ytd_tgt_share / ytd_rush_share / ytd_rz_share / ytd_inside10", "", "players", "Season shares and inside-10 carries. See players table.",
  "radar", "ytd_fp_exp / ytd_fp_act / ytd_fp_diff", "dbl", "players", "Season expected, actual, and expected minus actual.",
  "radar", "adp_rank", "dbl", "R/29 FantasyPros ADP cache (PPR)", "Preseason overall ADP rank. NA if undrafted or not in the cache.",
  "radar", "adp_pos_rank", "int", "R/56", "Preseason ADP rank within position. Undrafted players get (deepest drafted rank at the position) + 1, so they always trail.",
  "radar", "undrafted", "lgl", "R/56", "TRUE when no ADP was found.",
  "radar", "market_gap", "int", "R/56", "adp_pos_rank - opp_rank. Positive = the market drafted him later than his usage rank says. The sort key.",
  "radar", "trending_adds", "int", "Sleeper trending adds (168h, top 100)", "Add count on Sleeper's trending list. NA = not on the list (or Sleeper unavailable).",
  "radar", "market_caught_up", "lgl", "R/56", "TRUE when trending_adds is non-NA: the waiver market has already noticed.",
  "radar", "under_radar", "lgl", "R/56", "opp_rank <= 36 and market_gap >= 12 (R56_RADAR_TOP, R56_RADAR_GAP). Screening thresholds, not model weights.",
  "radar", "role_change", "lgl", "R/56", "d_snap_pct >= 0.15 (R56_ROLE_CHANGE_DELTA). Week-over-week snap jump.",
  "radar", "team_epa_play", "dbl", "teams (joined by R/57)", "The team's EPA per play. Separates a big share of a bad offense from a missed player.",
  "radar", "team_plays_pg", "dbl", "teams (joined by R/57)", "Team plays per game: size of the pie.",
  "radar", "team_pass_rate", "dbl", "teams (joined by R/57)", "Team pass rate.",
  "radar", "team_points_pg", "dbl", "teams (joined by R/57)", "Team points per game.",
  "radar", "team_rz_trips_pg", "dbl", "teams (joined by R/57)", "Team red-zone trips per game.",

  # ---- player_weeks / team_weeks ---------------------------------------------------
  "player_weeks", "(what it is)", "", "R/56", "One row per player per game week, weeks 1 through the run week. Each week's own shares and (v2) rates, same definitions as the players file's wk_ columns without the prefix. Feeds the By Week tab.",
  "player_weeks", "game_week", "int", "pbp", "The week the game was played.",
  "player_weeks", "through_week", "int", "R/56 argument", "The run week this file was written for.",
  "player_weeks", "offense_snaps", "int", "snap counts", "Offensive snaps that week (the numerator behind snap_pct).",
  "team_weeks", "(what it is)", "", "R/56", "The teams table computed one game week at a time. Same columns and definitions as teams, plus game_week and through_week; games = 1. A bye week has no row.",

  # ---- R/62 shared -----------------------------------------------------------------
  "r62_all", "side", "chr", "R/62", "off = the team's offense (plays where team = posteam); def = the team's defense (team = defteam). Every metric is computed the same way on both; on a def row it is the value ALLOWED.",
  "r62_all", "team", "chr", "R/62", "The team the row describes, on whichever side.",
  "r62_all", "n_is_play", "int", "R/62", "Scrimmage plays in the row (pass + run, kneels and spikes out, EPA present). Same filter as R/56's team table.",
  "r62_all", "n_dropback", "int", "R/62", "Dropbacks: pass plays incl. sacks, plus scrambles (qb_dropback = 1).",
  "r62_all", "n_dropback_ftn", "int", "R/62", "Dropbacks with an FTN rusher count. Denominator for blitz_rate and the other FTN dropback rates.",
  "r62_all", "n_is_rush", "int", "R/62", "Designed rushes (scrambles excluded).",
  "r62_all", "n_thrown", "int", "R/62", "Thrown balls: completions + incompletions + interceptions.",
  "r62_all", "n_thrown_ftn / n_completed / n_sack_ftn / n_box_known / n_play_ftn", "int", "R/62", "Other denominators: charted throws; completions; charted sacks; plays with an FTN box count; plays with FTN motion charting.",
  "r62_all", "(league_ prefix)", "", "R/62", "The same metric over every play in the same cut/level or situation cell, all 32 teams. The value shrinkage pulls toward.",
  "r62_all", "(base_ prefix)", "", "R/62", "The same metric for the same team, side and cell over the full 2025 season. A real prior to read three games against.",
  "r62_all", "(shrunk_ prefix)", "", "R/62", "(n * raw + k * league) / (n + k). k is fitted per metric per side on 2025 by method of moments; higher k = more shrinkage. Situation file only.",

  # ---- R/62 team extras ---------------------------------------------------------------
  "r62_team", "games / plays_pg / dropbacks_pg", "dbl", "R/62", "Games, scrimmage plays per game, dropbacks per game (faced, on a def row).",
  "r62_team", "points_pg", "dbl", "pbp final scores", "Points scored per game (off) or allowed per game (def).",
  "r62_team", "neutral_*", "dbl", "R/62", "The metric restricted to neutral script: win probability 0.20-0.80 in quarters 1-3 (R/56's definition). neutral_plays is the count.",
  "r62_team", "drives", "int", "pbp fixed_drive", "Offensive drives (off) or drives faced (def).",
  "r62_team", "points_per_drive", "dbl", "pbp", "7 per touchdown drive + 3 per field goal drive, over drives. Ignores missed XPs and two-point tries.",
  "r62_team", "scoring_drive_rate", "dbl", "pbp", "Share of drives ending in a TD or FG.",
  "r62_team", "turnover_drive_rate", "dbl", "pbp", "Share of drives ending in a turnover (fumble or interception; downs excluded).",
  "r62_team", "three_and_out_rate", "dbl", "pbp", "Share of drives that ended in a punt after 3 or fewer scrimmage plays.",
  "r62_team", "plays_per_drive", "dbl", "pbp", "Scrimmage plays per drive.",
  "r62_team", "sec_per_play", "dbl", "pbp", "Game-clock seconds between consecutive plays of the same drive (0 to 120 s window). Offense pace; on a def row, pace faced.",
  "r62_team", "penalties_pg / penalty_yards_pg", "dbl", "pbp", "Penalties committed by this side per game, and their yards.",
  "r62_team", "rank_*", "int", "R/62", "Rank within side, 1 = best for the efficiency metrics (highest EPA for off, lowest allowed for def) and 1 = most for tendencies (blitz_rate, pressure_rate, pass_rate).",

  # ---- R/62 situation ----------------------------------------------------------------
  "r62_situation", "down_bucket", "chr", "R/62", "1st, 2nd, 3rd-4th.",
  "r62_situation", "dist_bucket", "chr", "R/62", "Yards to go: 1-3, 4-6, 7+.",
  "r62_situation", "zone", "chr", "R/62", "Field position of the OFFENSE: own_half (yardline_100 > 50), opp_21_50, red_zone (<= 20). 'On the defense's side of the 50' = opp_21_50 or red_zone.",

  # ---- R/62 splits -------------------------------------------------------------------
  "r62_splits", "cut", "chr", "R/62", "Which split: blitz, box, play_action, screen, depth, tgt_pos, pass_location, run_gap, score_state, neutral, half, third_down_dist, blitz_x_down_dist, blitz_x_situation.",
  "r62_splits", "level", "chr", "R/62", "The level inside the cut. blitz: blitz / no_blitz. box: light_6minus / seven / stacked_8plus. play_action: play_action / standard. screen: screen / non_screen. depth: behind_los / short_0_9 / intermediate_10_19 / deep_20plus. tgt_pos: WR / TE / RB / OTHER. pass_location: left / middle / right. run_gap: end / tackle / guard / middle. score_state: trail_8plus / one_score / lead_8plus. half: first_half / second_half / overtime. third_down_dist: 1-3 / 4-6 / 7+ on 3rd and 4th down. blitz_x_down_dist: down|dist|blitz. blitz_x_situation: down|dist|zone|blitz.",

  # ---- R/62 matchup -------------------------------------------------------------------
  "r62_matchup", "(what it is)", "", "R/62", "Next week's games. One row per offense-defense pairing x cut x level. off_value is the offense's number in that cut; def_value the defense's number ALLOWED in the same cut; league_value the league in that cut.",
  "r62_matchup", "game_id / game_week", "", "schedule", "nflverse game id of next week's game (YYYY_WK_AWAY_HOME) and its week.",
  "r62_matchup", "off_team / def_team / off_home", "", "schedule", "The pairing and whether the offense is the home team.",
  "r62_matchup", "off_value / def_value / league_value", "dbl", "R/62", "The metric named in `metric`: for the offense in that cut and level, for the defense (allowed) in the same cut and level, and for the league there.",
  "r62_matchup", "metric", "chr", "R/62", "Which metric off_value and def_value are: epa_play for overall / situation (shrunk there) / box / score_state / neutral / half; epa_dropback for blitz, play_action, screen, blitz_x_*; epa_target for depth, tgt_pos, pass_location; epa_rush for run_gap; first_down_rate for third_down_dist.",
  "r62_matchup", "naive_expect", "dbl", "R/62", "off_value + def_value - league_value. An additive reading aid with no interaction term. Not a model, nothing consumes it.",
  "r62_matchup", "gap_vs_league_off / gap_vs_league_def", "dbl", "R/62", "off_value - league_value and def_value - league_value.",
  "r62_matchup", "off_n / def_n", "int", "R/62", "Plays behind each value.",
  "r62_matchup", "off_tendency / def_tendency", "dbl", "R/62", "Overall and blitz cuts: the defense's blitz rate. Situation cut: the offense's pass rate and the defense's blitz rate in that cell. Depth / tgt_pos / pass_location / run_gap: the play count in that level (how often each side is in it).",
  "r62_matchup", "off_raw / def_raw", "dbl", "R/62", "Situation rows only: the unshrunk epa_play next to the shrunk values.",

  # ---- By Week tab -------------------------------------------------------------------
  "by_week", "W1, W2, ...", "", "player_weeks", "The chosen stat in that game week. Blank = no row that week (bye, inactive, or no usage), not a zero.",
  "by_week", "Season", "", "players ytd_", "Read from the players file's ytd_ column for the chosen stat. For rates this is summed numerator over summed denominator, so it will not equal the average of the week columns."
)

# One meaning per R/62 registry metric. Built into dictionary rows below so
# every metric in R62_METRICS is documented (a metric without a meaning here
# is flagged in the Dictionary tab rather than silently missing).
R57_R62_MEANINGS <- c(
  epa_play = "Mean EPA per scrimmage play. Def rows: EPA allowed, lower is better.",
  success_rate = "Share of plays with EPA > 0.",
  explosive_rate = "Share of plays that were a 15+ yard pass or a 10+ yard rush.",
  first_down_rate = "Share of plays that gained a first down (or scored).",
  pass_rate = "Share of scrimmage plays that were passes (sacks count as passes).",
  pass_oe = "Mean pass-over-expected (nflfastR pass_oe, percentage points): pass rate above what the situation predicts.",
  avg_def_box = "Mean FTN defenders in the box on plays with a box count.",
  light_box_rate = "Share of box-charted plays with 6 or fewer in the box.",
  stacked_box_rate = "Share of box-charted plays with 8+ in the box.",
  epa_dropback = "Mean EPA per dropback.",
  success_dropback = "Share of dropbacks with EPA > 0.",
  explosive_pass_rate = "Share of dropbacks gaining 15+ yards.",
  sack_rate = "Sacks per dropback.",
  pressure_rate = "(Sacks + QB hits) per dropback. A pbp proxy; not a tracking-data pressure rate.",
  int_rate = "Interceptions per dropback.",
  scramble_rate = "Scrambles per dropback.",
  blitz_rate = "Share of charted dropbacks with 5+ pass rushers.",
  avg_pass_rushers = "Mean pass rushers on charted dropbacks.",
  rush3_or_fewer_rate = "Share of charted dropbacks with 3 or fewer rushers (drop-eight looks).",
  rush6_plus_rate = "Share of charted dropbacks with 6+ rushers.",
  pa_rate = "Play-action share of charted dropbacks.",
  screen_rate = "Screen share of charted dropbacks.",
  rpo_rate = "RPO share of charted dropbacks.",
  out_of_pocket_rate = "Share of charted dropbacks where the QB left the pocket.",
  throwaway_rate = "Throwaways per charted dropback.",
  qb_fault_sack_share = "Share of charted sacks FTN attributes to the QB.",
  comp_pct = "Completions / thrown balls.",
  cpoe = "Mean completion percentage over expected on thrown balls.",
  adot = "Mean air yards per thrown ball.",
  epa_target = "Mean EPA per thrown ball.",
  yac_per_comp = "Yards after catch per completion.",
  deep_target_rate = "Share of throws with air_yards >= 20.",
  int_worthy_rate = "Interception-worthy throws per charted throw.",
  catchable_rate = "Catchable throws per charted throw.",
  contested_rate = "Contested throws per charted throw.",
  drop_rate = "Drops per charted throw.",
  created_rec_rate = "'Created receptions' per charted throw.",
  epa_rush = "Mean EPA per designed rush.",
  success_rush = "Share of rushes with EPA > 0.",
  ypc = "Yards per carry.",
  stuff_rate = "Share of rushes gaining 0 or fewer yards.",
  rush_conv_rate = "Share of rushes gaining a first down or scoring.",
  explosive_rush_rate = "Share of rushes of 10+ yards.",
  motion_rate = "Share of FTN-charted plays with pre-snap motion.",
  no_huddle_rate = "Share of plays run without a huddle."
)

.r57_build_dictionary <- function() {
  if (!exists("R62_METRICS")) return(R57_DICTIONARY_BASE)
  rows <- lapply(seq_len(nrow(R62_METRICS)), function(i) {
    m <- R62_METRICS$metric[i]
    meaning <- if (m %in% names(R57_R62_MEANINGS)) unname(R57_R62_MEANINGS[m])
               else "(no meaning recorded; add it to R57_R62_MEANINGS)"
    tibble::tibble(table = "r62_metric", column = m, type = "dbl",
                   source = glue::glue("R/62: mean({R62_METRICS$value_col[i]}) over ",
                                       "n_{R62_METRICS$den_col[i]}") %>% as.character(),
                   meaning = meaning)
  })
  dplyr::bind_rows(R57_DICTIONARY_BASE, dplyr::bind_rows(rows))
}

# ------------------------------------------------------------------------------
# APP
# ------------------------------------------------------------------------------

#' Launch the usage explorer.
#' @param week Integer or NULL. NULL picks the newest week on disk.
#' @param recompute Logical. Re-run R/56 at startup instead of reading CSVs.
#' @param dir Character. Where R/56 and R/62 write.
run_usage_explorer <- function(week = NULL, recompute = FALSE,
                               dir = R57_CACHE_DIR) {

  weeks_on_disk <- .r57_available_weeks(dir)
  if (is.null(week)) {
    if (length(weeks_on_disk) == 0L && !isTRUE(recompute)) {
      stop("R/57: no R/56 output found in ", dir,
           ". Run run_weekly_usage(week = N) first.", call. = FALSE)
    }
    week <- weeks_on_disk[1]
  }
  week <- as.integer(week)

  dat0  <- .r57_load_week(week, recompute = recompute, dir = dir)
  week_choices <- sort(unique(c(weeks_on_disk, week)))

  ui     <- .r57_ui(week, week_choices, dat0)
  server <- .r57_server(dat0, week, dir)
  shiny::runApp(shinyApp(ui, server), launch.browser = TRUE)
}

#' Cut and metric pickers, from R/62 when it is sourced, else the same lists.
.r57_cut_choices <- function() {
  if (exists("R62_CUTS")) R62_CUTS else
    c("blitz", "box", "play_action", "screen", "depth", "tgt_pos", "pass_location",
      "run_gap", "score_state", "neutral", "half", "third_down_dist",
      "blitz_x_down_dist", "blitz_x_situation")
}
.r57_sit_metrics <- function() {
  if (exists("R62_SHRINK_METRICS")) R62_SHRINK_METRICS else
    c("epa_play", "success_rate", "pass_rate", "epa_dropback", "success_dropback",
      "sack_rate", "blitz_rate", "epa_rush", "success_rush")
}

#' The page. Separate from the server so both can be tested without a browser.
.r57_ui <- function(week, week_choices, dat0) {
  teams_all   <- sort(unique(stats::na.omit(dat0$players$team)))
  cut_choices <- .r57_cut_choices()
  sit_metrics <- .r57_sit_metrics()

  fluidPage(
    titlePanel(glue("Weekly Usage Explorer -- {R56_SEASON}")),

    fluidRow(
      column(2, selectInput("week", "Week", choices = week_choices,
                            selected = week, width = "100%")),
      column(3, selectizeInput("pos", "Position", choices = R56_POSITIONS,
                               selected = R56_POSITIONS, multiple = TRUE,
                               width = "100%")),
      column(4, selectizeInput("team", "Team (blank = all)", choices = teams_all,
                               selected = NULL, multiple = TRUE, width = "100%")),
      column(3, br(), actionButton("reload", "Reload week", class = "btn-primary"),
                      actionButton("rerun", "Re-run R/56"))
    ),

    fluidRow(
      column(3, sliderInput("min_snap", "Min snap pct (season)", min = 0, max = 1,
                            value = 0, step = 0.05, width = "100%")),
      column(3, sliderInput("min_opp", "Min opportunity share", min = 0, max = 0.6,
                            value = 0, step = 0.01, width = "100%")),
      column(2, numericInput("min_games", "Min games", value = 1, min = 1, step = 1)),
      column(2, numericInput("min_gap", "Min market gap", value = 0, step = 1)),
      column(2, br(), checkboxInput("undrafted_only", "Undrafted only", FALSE),
                      checkboxInput("screen_only", "Radar flag only", FALSE))
    ),

    div(style = "color:#555;", textOutput("status")),
    div(style = "color:#a33;", textOutput("small_sample")),
    hr(),

    tabsetPanel(
      tabPanel("Radar", br(),
               helpText("Sorted by market gap. Sort by team_epa_play to separate ",
                        "a missed player from a big share of a bad offense. ",
                        "Positive fp_diff means the usage has not cashed yet."),
               DT::dataTableOutput("radar")),
      tabPanel("Players", br(),
               fluidRow(
                 column(3, selectInput("pl_view", "View", choices = names(R57_PLAYER_VIEWS),
                                       selected = "Usage", width = "100%")),
                 column(9, helpText("Every qualifying player-week aggregate from R/56. ",
                                    "Usage = the v1 columns. Receiving, Rushing and ",
                                    "Passing are the v2 efficiency block; rates are ",
                                    "summed numerator over summed denominator at ytd_. ",
                                    "Filters above apply."))
               ),
               DT::dataTableOutput("players")),
      tabPanel("By Week", br(),
               fluidRow(
                 column(4, selectInput("bw_stat", "Stat",
                                       choices = R57_BYWEEK_STATS$label,
                                       selected = "Snap %", width = "100%")),
                 column(8, helpText("One column per game week, then Season. Season ",
                                    "is read from R/56's season-to-date column, not ",
                                    "averaged from the weeks. Blank = no row that week, ",
                                    "not a zero. Filters above apply."))
               ),
               div(style = "color:#a33;", textOutput("bw_note")),
               DT::dataTableOutput("by_week")),
      tabPanel("Teams", br(),
               fluidRow(
                 column(4, selectInput("team_view", "View",
                                       choices = c("Season to date"),
                                       selected = "Season to date", width = "100%")),
                 column(8, helpText("The size of each pie. Concentration columns are the ",
                                    "combined share of the top three earners. A single ",
                                    "week is one game, so treat it as noise-heavy."))
               ),
               DT::dataTableOutput("teams")),

      # ---- R/62 tabs ---------------------------------------------------------------
      tabPanel("Defense", br(),
               fluidRow(
                 column(3, selectInput("def_view", "View", choices = names(R57_TEAM62_VIEWS),
                                       selected = "Overview", width = "100%")),
                 column(3, selectInput("def_week", "Week", choices = c("Season to date"),
                                       selected = "Season to date", width = "100%")),
                 column(6, helpText("R/62 team file, defense rows. Every epa_ column is EPA ",
                                    "ALLOWED and lower is better; rank_ columns already ",
                                    "point the right way (1 = best). base_ = 2025 full ",
                                    "season for the same team. Team filter above applies."))
               ),
               DT::dataTableOutput("defense")),
      tabPanel("Off Tendencies", br(),
               fluidRow(
                 column(3, selectInput("off_view", "View", choices = names(R57_TEAM62_VIEWS),
                                       selected = "Overview", width = "100%")),
                 column(3, selectInput("off_week", "Week", choices = c("Season to date"),
                                       selected = "Season to date", width = "100%")),
                 column(6, helpText("R/62 team file, offense rows: how each offense plays ",
                                    "(pass rate, play action, screens, motion, pace) and how ",
                                    "it fares against pressure and box counts. The 'Pass rush ",
                                    "and box' view on an offense row = what it FACES."))
               ),
               DT::dataTableOutput("offense")),
      tabPanel("Situations", br(),
               fluidRow(
                 column(2, radioButtons("sit_side", "Side", choices = c("def", "off"),
                                        selected = "def", inline = TRUE)),
                 column(2, selectizeInput("sit_down", "Down", choices = c("1st", "2nd", "3rd-4th"),
                                          selected = NULL, multiple = TRUE, width = "100%")),
                 column(2, selectizeInput("sit_dist", "Distance", choices = c("1-3", "4-6", "7+"),
                                          selected = NULL, multiple = TRUE, width = "100%")),
                 column(2, selectizeInput("sit_zone", "Zone",
                                          choices = c("own_half", "opp_21_50", "red_zone"),
                                          selected = NULL, multiple = TRUE, width = "100%")),
                 column(2, selectInput("sit_metric", "Sort metric", choices = sit_metrics,
                                       selected = "epa_play", width = "100%")),
                 column(2, numericInput("sit_min_n", "Min plays (n)", value = 0, min = 0, step = 1))
               ),
               helpText("down x distance x field zone. zone is the OFFENSE's field position ",
                        "(opp_21_50 and red_zone = on the defense's side of the 50). Each ",
                        "metric has raw, shrunk_, league_ and base_ (2025) versions; read ",
                        "shrunk_ when n is small. Blank multi-selects = all levels."),
               DT::dataTableOutput("situations")),
      tabPanel("Splits", br(),
               fluidRow(
                 column(2, radioButtons("sp_side", "Side", choices = c("def", "off"),
                                        selected = "def", inline = TRUE)),
                 column(3, selectInput("sp_cut", "Cut", choices = cut_choices,
                                       selected = "blitz", width = "100%")),
                 column(3, selectizeInput("sp_level", "Level (blank = all)", choices = NULL,
                                          selected = NULL, multiple = TRUE, width = "100%")),
                 column(2, numericInput("sp_min_n", "Min plays (n)", value = 0, min = 0, step = 1)),
                 column(2, br(), checkboxInput("sp_all_cols", "All columns", FALSE))
               ),
               helpText("One cut at a time. For blitz_x_situation the level reads ",
                        "down|distance|zone|blitz, so '3rd-4th|7+|opp_21_50|blitz' on a def ",
                        "row is that defense blitzing on third-and-long on its own side of ",
                        "the 50, and the same level on an off row is how that offense did ",
                        "against it. league_ columns are the league in the same level."),
               DT::dataTableOutput("splits")),
      tabPanel("Matchups", br(),
               fluidRow(
                 column(3, selectInput("mu_game", "Game", choices = c("(all)"),
                                       selected = "(all)", width = "100%")),
                 column(3, selectInput("mu_cut", "Cut", choices = c("overall", "situation", cut_choices),
                                       selected = "overall", width = "100%")),
                 column(3, selectizeInput("mu_opp", "Opponent (defense) filter", choices = NULL,
                                          selected = NULL, multiple = TRUE, width = "100%")),
                 column(3, numericInput("mu_min_n", "Min n (both sides)", value = 0, min = 0, step = 1))
               ),
               helpText("Next week. Each row: the offense's value in a cut against the ",
                        "defense's value allowed in the same cut, with the league value. ",
                        "naive_expect = off + def - league, an additive reading aid. ",
                        "def_tendency on blitz and situation rows is the defense's blitz ",
                        "rate there; off_tendency on situation rows is the offense's pass ",
                        "rate there. Team filter above = the offense."),
               DT::dataTableOutput("matchups")),

      tabPanel("Notes", br(), verbatimTextOutput("notes")),
      tabPanel("Dictionary", br(),
               helpText("Every column in the R/56 and R/62 files plus the team-context ",
                        "columns this app joins on. Filter by table or search a column."),
               DT::dataTableOutput("dictionary"))
    )
  )
}

#' The server, as a function of the initial data. Returns the function(input,
#' output, session) shiny expects, so it can also be driven by
#' shiny::testServer() without a browser.
.r57_server <- function(dat0, week, dir = R57_CACHE_DIR) {
  dictionary <- .r57_build_dictionary()

  function(input, output, session) {

    dat    <- reactiveVal(dat0)

    team_view_choices <- function(d) {
      wks <- if (!is.null(d$team_weeks)) sort(unique(d$team_weeks$game_week)) else integer(0)
      c("Season to date", if (length(wks)) paste("Week", wks, "only"))
    }
    r62_week_choices <- function(d) {
      wks <- if (!is.null(d$r62_team_weeks)) sort(unique(d$r62_team_weeks$game_week)) else integer(0)
      c("Season to date", if (length(wks)) paste("Week", wks, "only"))
    }
    game_choices <- function(d) {
      if (is.null(d$r62_matchup) || nrow(d$r62_matchup) == 0L) return("(all)")
      c("(all)", sort(unique(d$r62_matchup$game_id)))
    }
    refresh_pickers <- function(d) {
      updateSelectInput(session, "team_view", choices = team_view_choices(d),
                        selected = "Season to date")
      updateSelectInput(session, "def_week", choices = r62_week_choices(d),
                        selected = "Season to date")
      updateSelectInput(session, "off_week", choices = r62_week_choices(d),
                        selected = "Season to date")
      updateSelectInput(session, "mu_game", choices = game_choices(d), selected = "(all)")
      opp <- if (!is.null(d$r62_matchup) && nrow(d$r62_matchup) > 0L)
        sort(unique(d$r62_matchup$def_team)) else character()
      updateSelectizeInput(session, "mu_opp", choices = opp, selected = NULL)
    }
    refresh_pickers(dat0)

    status <- reactiveVal(glue("Week {week} loaded. {nrow(dat0$players)} players, ",
                               "{nrow(dat0$radar)} in the screen pool",
                               if (is.null(dat0$r62_team)) "; no R/62 files for this week." else
                                 glue("; R/62 team {nrow(dat0$r62_team)} rows, splits ",
                                      "{nrow(dat0$r62_splits %||% tibble::tibble())} rows, ",
                                      "matchup {nrow(dat0$r62_matchup %||% tibble::tibble())} rows.")))

    load_it <- function(recompute) {
      wk <- as.integer(input$week)
      status(glue("Loading week {wk}..."))
      res <- tryCatch(.r57_load_week(wk, recompute = recompute, dir = dir),
                      error = function(e) e)
      if (inherits(res, "error")) {
        status(glue("ERROR: {conditionMessage(res)}"))
        return(invisible(NULL))
      }
      dat(res)
      refresh_pickers(res)
      updateSelectizeInput(session, "team",
                           choices = sort(unique(stats::na.omit(res$players$team))),
                           selected = input$team)
      status(glue("Week {wk} loaded. {nrow(res$players)} players, ",
                  "{nrow(res$radar)} in the screen pool",
                  if (is.null(res$r62_team)) "; no R/62 files for this week." else
                    glue("; R/62 team {nrow(res$r62_team)} rows.")))
    }

    observeEvent(input$reload, load_it(FALSE))
    observeEvent(input$rerun,  load_it(TRUE))

    output$status <- renderText(status())

    output$small_sample <- renderText({
      g <- suppressWarnings(max(dat()$players$games, na.rm = TRUE))
      if (is.finite(g) && g < 4) {
        glue("Small sample: {g} game(s) of data. Ranks will reshuffle. ",
             "Week-over-week delta columns are empty until week 2. On the ",
             "Situations tab read the shrunk_ columns.")
      } else ""
    })

    # Shared filter applied to any frame carrying the relevant columns.
    apply_filters <- function(df) {
      if (!is.null(input$pos) && "position" %in% names(df)) {
        df <- df %>% dplyr::filter(.data$position %in% input$pos)
      }
      if (length(input$team) > 0L && "team" %in% names(df)) {
        df <- df %>% dplyr::filter(.data$team %in% input$team)
      }
      if ("games" %in% names(df) && "position" %in% names(df) && !is.na(input$min_games)) {
        df <- df %>% dplyr::filter(dplyr::coalesce(.data$games, 0L) >= input$min_games)
      }
      if ("ytd_snap_pct" %in% names(df) && input$min_snap > 0) {
        df <- df %>% dplyr::filter(dplyr::coalesce(.data$ytd_snap_pct, 0) >= input$min_snap)
      }
      if ("ytd_opp_share" %in% names(df) && input$min_opp > 0) {
        df <- df %>% dplyr::filter(dplyr::coalesce(.data$ytd_opp_share, 0) >= input$min_opp)
      }
      df
    }

    # ---- Radar -------------------------------------------------------------------
    radar_f <- reactive({
      d <- .r57_join_team_context(dat()$radar, dat()$teams) %>% apply_filters()
      if (isTRUE(input$undrafted_only) && "undrafted" %in% names(d)) {
        d <- d %>% dplyr::filter(.data$undrafted %in% TRUE)
      }
      if (isTRUE(input$screen_only) && "under_radar" %in% names(d)) {
        d <- d %>% dplyr::filter(.data$under_radar %in% TRUE)
      }
      if (!is.na(input$min_gap) && "market_gap" %in% names(d)) {
        d <- d %>% dplyr::filter(dplyr::coalesce(.data$market_gap, -999) >= input$min_gap)
      }
      d %>% dplyr::arrange(dplyr::desc(.data$market_gap))
    })

    output$radar <- DT::renderDataTable({
      radar_f() %>%
        dplyr::select(dplyr::any_of(c(
          "player_name", "position", "team", "games",
          "ytd_opp_share", "opp_rank", "ytd_snap_pct", "snap_rank",
          "ytd_tgt_share", "ytd_rush_share", "ytd_rz_share", "ytd_inside10",
          "adp_pos_rank", "undrafted", "market_gap", "trending_adds",
          "ytd_fp_exp", "ytd_fp_act", "ytd_fp_diff",
          "team_epa_play", "team_plays_pg", "team_pass_rate",
          "team_points_pg", "team_rz_trips_pg",
          "under_radar", "role_change", "d_snap_pct"))) %>%
        .r57_dt(pct_cols = R57_SHARE_COLS)
    })

    # ---- Players -----------------------------------------------------------------
    output$players <- DT::renderDataTable({
      cols <- R57_PLAYER_VIEWS[[input$pl_view %||% "Usage"]]
      d <- apply_filters(dat()$players) %>%
        dplyr::select(dplyr::any_of(c("player_name", "position", "team", "games", cols)))
      sort_col <- intersect(c("ytd_snap_pct", "ytd_tgts", "ytd_carries", "ytd_dropbacks"),
                            names(d))
      if (input$pl_view == "Receiving") sort_col <- "ytd_tgts"
      if (input$pl_view == "Rushing")   sort_col <- "ytd_carries"
      if (input$pl_view == "Passing")   sort_col <- "ytd_dropbacks"
      if (input$pl_view == "Usage")     sort_col <- "ytd_snap_pct"
      if (sort_col[1] %in% names(d)) d <- d %>% dplyr::arrange(dplyr::desc(.data[[sort_col[1]]]))
      .r57_dt(d, pct_cols = .r57_pct_cols(d))
    })

    # ---- Teams (R/56) --------------------------------------------------------------
    output$teams <- DT::renderDataTable({
      d <- dat()$teams
      tv <- input$team_view %||% "Season to date"
      if (tv != "Season to date" && !is.null(dat()$team_weeks)) {
        wk_pick <- as.integer(gsub("[^0-9]", "", tv))
        d <- dat()$team_weeks %>% dplyr::filter(.data$game_week == wk_pick)
      }
      if (length(input$team) > 0L) d <- d %>% dplyr::filter(.data$team %in% input$team)
      d %>%
        dplyr::select(dplyr::any_of(c(
          "team", "games", "points_pg", "plays_pg", "pass_rate",
          "neutral_pass_rate", "neutral_plays", "rz_trips_pg",
          "epa_play", "pass_epa", "rush_epa",
          "top3_tgt_share", "top_target_name",
          "top3_rush_share", "top_rusher_name"))) %>%
        dplyr::arrange(dplyr::desc(.data$epa_play)) %>%
        .r57_dt(pct_cols = R57_SHARE_COLS, page = 32L)
    })

    # ---- By Week -----------------------------------------------------------------
    by_week_f <- reactive({
      bw <- .r57_by_week(dat()$player_weeks, dat()$players, input$bw_stat)
      if (!is.null(bw$df)) {
        bw$df <- apply_filters(bw$df) %>%
          dplyr::arrange(dplyr::desc(.data$Season))
      }
      bw
    })

    output$bw_note <- renderText({
      bw <- by_week_f()
      if (is.null(dat()$player_weeks)) {
        "No player-week file for this week. Rerun R/56 (v1.1+) for it."
      } else if (is.null(bw$df)) {
        "This stat is not in the loaded files (written by R/56 v2 and later). Rerun R/56 for this week."
      } else if (bw$n_dup > 0L) {
        glue("{bw$n_dup} player-week cell(s) had more than one row; the first is shown.")
      } else ""
    })

    output$by_week <- DT::renderDataTable({
      bw <- by_week_f()
      if (is.null(bw$df)) return(.r57_dt(NULL))
      show <- bw$df %>%
        dplyr::select(player_name, position, team, games,
                      dplyr::all_of(bw$week_cols), Season)
      val_cols <- c(bw$week_cols, "Season")
      if (isTRUE(bw$is_pct)) {
        .r57_dt(show, pct_cols = val_cols)
      } else {
        .r57_dt(show, digits = 2)
      }
    })

    # ---- R/62: team file (Defense / Off Tendencies) ---------------------------------
    team62 <- function(side_pick, view, week_pick) {
      d <- dat()$r62_team
      if (is.null(d)) return(NULL)
      if (!is.null(week_pick) && week_pick != "Season to date" && !is.null(dat()$r62_team_weeks)) {
        wk_pick <- as.integer(gsub("[^0-9]", "", week_pick))
        d <- dat()$r62_team_weeks %>% dplyr::filter(.data$game_week == wk_pick)
      }
      d <- d %>% dplyr::filter(.data$side == side_pick)
      if (length(input$team) > 0L) d <- d %>% dplyr::filter(.data$team %in% input$team)
      cols <- R57_TEAM62_VIEWS[[view %||% "Overview"]]
      if (length(cols) == 0L) {
        d <- d %>% dplyr::select(-dplyr::any_of(c("season", "schema_tag", "through_week")))
      } else {
        d <- d %>% dplyr::select(dplyr::any_of(c("team", "game_week", cols)))
      }
      if ("epa_play" %in% names(d)) {
        d <- if (side_pick == "def") d %>% dplyr::arrange(.data$epa_play)
             else d %>% dplyr::arrange(dplyr::desc(.data$epa_play))
      }
      d
    }
    output$defense <- DT::renderDataTable({
      d <- team62("def", input$def_view, input$def_week)
      if (is.null(d)) return(.r57_dt(NULL, message = .r57_no62))
      .r57_dt(d, pct_cols = .r57_pct_cols(d), page = 32L)
    })
    output$offense <- DT::renderDataTable({
      d <- team62("off", input$off_view, input$off_week)
      if (is.null(d)) return(.r57_dt(NULL, message = .r57_no62))
      .r57_dt(d, pct_cols = .r57_pct_cols(d), page = 32L)
    })

    # ---- R/62: Situations --------------------------------------------------------------
    output$situations <- DT::renderDataTable({
      d <- dat()$r62_situation
      if (is.null(d)) return(.r57_dt(NULL, message = .r57_no62))
      d <- d %>% dplyr::filter(.data$side == input$sit_side)
      if (length(input$team) > 0L)     d <- d %>% dplyr::filter(.data$team %in% input$team)
      if (length(input$sit_down) > 0L) d <- d %>% dplyr::filter(.data$down_bucket %in% input$sit_down)
      if (length(input$sit_dist) > 0L) d <- d %>% dplyr::filter(.data$dist_bucket %in% input$sit_dist)
      if (length(input$sit_zone) > 0L) d <- d %>% dplyr::filter(.data$zone %in% input$sit_zone)
      if (!is.na(input$sit_min_n) && input$sit_min_n > 0) {
        d <- d %>% dplyr::filter(.data$n_is_play >= input$sit_min_n)
      }
      m <- input$sit_metric %||% "epa_play"
      show_cols <- c("team", "down_bucket", "dist_bucket", "zone",
                     "n_is_play", "n_dropback", "n_dropback_ftn", "n_is_rush",
                     m, paste0("shrunk_", m), paste0("league_", m), paste0("base_", m),
                     "epa_play", "shrunk_epa_play", "success_rate", "pass_rate",
                     "epa_dropback", "shrunk_epa_dropback", "sack_rate", "pressure_rate",
                     "blitz_rate", "shrunk_blitz_rate", "league_blitz_rate", "base_blitz_rate",
                     "epa_rush", "shrunk_epa_rush", "explosive_rate", "first_down_rate")
      d <- d %>% dplyr::select(dplyr::any_of(unique(show_cols)))
      sm <- paste0("shrunk_", m)
      if (sm %in% names(d)) {
        d <- if (input$sit_side == "def") d %>% dplyr::arrange(.data[[sm]])
             else d %>% dplyr::arrange(dplyr::desc(.data[[sm]]))
      }
      .r57_dt(d, pct_cols = .r57_pct_cols(d), page = 27L)
    })

    # ---- R/62: Splits -----------------------------------------------------------------
    observe({
      d <- dat()$r62_splits
      lv <- if (!is.null(d)) sort(unique(d$level[d$cut == input$sp_cut])) else character()
      updateSelectizeInput(session, "sp_level", choices = lv, selected = NULL)
    })
    output$splits <- DT::renderDataTable({
      d <- dat()$r62_splits
      if (is.null(d)) return(.r57_dt(NULL, message = .r57_no62))
      d <- d %>% dplyr::filter(.data$side == input$sp_side, .data$cut == input$sp_cut)
      if (length(input$team) > 0L)     d <- d %>% dplyr::filter(.data$team %in% input$team)
      if (length(input$sp_level) > 0L) d <- d %>% dplyr::filter(.data$level %in% input$sp_level)
      if (!is.na(input$sp_min_n) && input$sp_min_n > 0) {
        d <- d %>% dplyr::filter(.data$n_is_play >= input$sp_min_n)
      }
      if (!isTRUE(input$sp_all_cols)) {
        d <- d %>% dplyr::select(dplyr::any_of(c("team", "level", R57_SPLIT_COLS)))
      } else {
        d <- d %>% dplyr::select(-dplyr::any_of(c("season", "schema_tag", "side", "cut", "week")))
      }
      d <- d %>% dplyr::arrange(.data$level, .data$team)
      .r57_dt(d, pct_cols = .r57_pct_cols(d), page = 32L)
    })

    # ---- R/62: Matchups ------------------------------------------------------------------
    output$matchups <- DT::renderDataTable({
      d <- dat()$r62_matchup
      if (is.null(d) || nrow(d) == 0L) {
        return(.r57_dt(NULL, message = paste(.r57_no62, "(Week 18 has no next-week file.)")))
      }
      d <- d %>% dplyr::filter(.data$cut == input$mu_cut)
      if (!is.null(input$mu_game) && input$mu_game != "(all)") {
        d <- d %>% dplyr::filter(.data$game_id == input$mu_game)
      }
      if (length(input$team) > 0L)   d <- d %>% dplyr::filter(.data$off_team %in% input$team)
      if (length(input$mu_opp) > 0L) d <- d %>% dplyr::filter(.data$def_team %in% input$mu_opp)
      if (!is.na(input$mu_min_n) && input$mu_min_n > 0) {
        d <- d %>% dplyr::filter(dplyr::coalesce(.data$off_n, 0L) >= input$mu_min_n,
                                 dplyr::coalesce(.data$def_n, 0L) >= input$mu_min_n)
      }
      d <- d %>%
        dplyr::select(dplyr::any_of(c("game_id", "off_team", "def_team", "off_home", "level",
                                      "metric", "off_value", "def_value", "league_value",
                                      "naive_expect", "gap_vs_league_off", "gap_vs_league_def",
                                      "off_n", "def_n", "off_tendency", "def_tendency",
                                      "off_raw", "def_raw"))) %>%
        dplyr::arrange(dplyr::desc(.data$naive_expect))
      .r57_dt(d, pct_cols = character(), page = 32L)
    })

    # ---- Dictionary / Notes ------------------------------------------------------------
    output$dictionary <- DT::renderDataTable({
      DT::datatable(
        dictionary, rownames = FALSE, filter = "top",
        options = list(pageLength = 100, dom = "ft", scrollX = TRUE,
                       columnDefs = list(list(width = "45%", targets = 4)))
      )
    })

    output$notes <- renderText({
      r62_tag <- if (!is.null(dat()$r62_team) && "schema_tag" %in% names(dat()$r62_team))
        dat()$r62_team$schema_tag[1] else "(no R/62 file loaded)"
      paste(
        glue("R/57 {R57_SCHEMA_TAG} over R/56 {R56_SCHEMA_TAG} and R/62 {r62_tag}"),
        "",
        "WHAT IS NOT HERE",
        "  Route participation, man/zone, NGS box counts, personnel groupings:",
        "  nflreadr::load_participation(2026) returns 404. Not proxied.",
        "  True pressure rate needs tracking data; pressure_rate here is",
        "  (sack + qb_hit) per dropback. PFR advanced stats lag the current",
        "  week and are not in the weekly run.",
        "",
        "SIDES",
        "  side = def rows are the value ALLOWED. Nothing is sign-flipped. A",
        "  defense with epa_dropback -0.10 is good; an offense with -0.10 is bad.",
        "",
        "SHRINKAGE (Situations tab)",
        "  shrunk_<metric> = (n * raw + k * league_cell) / (n + k), k fitted per",
        "  metric per side on 2025. Small n reads as the league cell value.",
        "",
        "SCREEN THRESHOLDS (R/56 constants, they decide what gets FLAGGED only)",
        glue("  opportunity rank inside top {R56_RADAR_TOP}"),
        glue("  market gap at least {R56_RADAR_GAP}"),
        glue("  minimum {R56_MIN_OPPS} season opportunities"),
        glue("  role change at snap pct up {R56_ROLE_CHANGE_DELTA} week over week"),
        "  The sliders above filter the view; they do not change under_radar.",
        "",
        "SCORING",
        "  ytd_fp_exp and ytd_fp_act come from ffopportunity's own scoring, not",
        "  any league's. Read fp_diff as a direction, not a points total.",
        "",
        "MARKET",
        "  adp_pos_rank is a PRESEASON FantasyPros snapshot and goes stale as",
        "  the season moves. trending_adds is a live Sleeper add count across",
        "  all leagues; a high gap with a high trending count means the market",
        "  has already caught up.",
        sep = "\n"
      )
    })
  }
}
