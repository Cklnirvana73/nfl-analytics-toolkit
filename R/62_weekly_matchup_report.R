# ==============================================================================
# 62_weekly_matchup_report.R
# Season 3 -- weekly team splits on BOTH sides of the ball (offense and
# defense), situational grid, pressure / coverage / run cuts, and a matchup
# table for the coming week. NFL Analytics Toolkit
#
# NAVIGATION GUIDE
# Line  121 : constants, buckets, metric registry
# Line  241 : .r62_load_schedule()        -- next week's games
# Line  278 : .r62_screen_ftn()           -- blank FTN placeholder rows for
#                                            games FTN has not charted yet
# Line  318 : .r62_ftn_coverage()         -- one row per game: charted or not
# Line  357 : .r62_plays()                -- one clean scrimmage play frame
#                                            with FTN charting joined
# Line  496 : .r62_long()                 -- every play twice: off row, def row
# Line  515 : .r62_summarise()            -- the generic metric block
# Line  608 : build_team_table62()        -- team x side, season to date
# Line  688 : build_situation_grid()      -- team x side x down x dist x zone
# Line  749 : build_splits()              -- long cut/level table
# Line  811 : .r62_baseline()             -- 2025 fit: shrinkage k + baselines
# Line  846 : .r62_apply_shrinkage()
# Line  884 : build_matchups()            -- next week's games, both sides
# Line  967 : build_team_weeks62()        -- team table one game week at a time
# Line 1033 : run_weekly_matchup()        -- entry point
#
# PURPOSE
# Run once a week after the games finish, after run_weekly_usage(). Every
# number here is computed once on posteam (side = "off") and once on defteam
# (side = "def") through the same function, so the offense and defense frames
# share column names and definitions and the matchup table joins on identical
# keys. "epa_play" on a def row is EPA per play ALLOWED; lower is better for
# the defense. No column is flipped in sign.
#
# FILES WRITTEN (data/season3_cache/, NN = zero-padded run week)
#   s3_r62_team_wNN.csv        one row per team x side, season to date, wide
#   s3_r62_situation_wNN.csv   team x side x down_bucket x dist_bucket x zone,
#                              raw + shrunk + league cell value + 2025 baseline
#   s3_r62_splits_wNN.csv      long: team x side x cut x level, every cut in
#                              R62_CUTS (blitz, box, play action, screen,
#                              target depth, target position, pass location,
#                              run gap, score state, neutral, half, third down
#                              distance, blitz x situation)
#   s3_r62_matchup_wNN.csv     next week's games, one row per offense-defense
#                              pairing x cut x level x metric
#   s3_r62_team_weeks_wNN.csv  the team file computed one game week at a time
#   s3_r62_ftn_coverage_wNN.csv one row per game played: whether FTN has
#                              charted it (see FTN PLACEHOLDER ROWS below)
#   s3_r62_baseline_2025.rds   cache: shrinkage k table + 2025 baselines.
#                              Built once, reused every week.
#
# DATA SOURCES (verified 2026-09-29 on weeks 1-3)
#   nflreadr::load_pbp(2026)             via R/56's .r56_load_pbp()
#   nflreadr::load_ftn_charting(2026)    8,065 rows, weeks 1-3. All 5,829
#                                        scrimmage plays joined on
#                                        game_id = nflverse_game_id and
#                                        play_id = nflverse_play_id.
#                                        n_blitzers >= 1 and n_pass_rushers >= 5
#                                        are the same 1,144 rows, so
#                                        is_blitz = n_pass_rushers >= 5.
#   nflreadr::load_schedules(2026)       next week's games for the matchup table
#   nflreadr::load_rosters(2026)         receiver position group
#   nflreadr::load_participation(2026)   404. Man/zone and NGS box counts are
#                                        NOT here. FTN's n_defense_box is the
#                                        box count used.
#   PFR advanced stats                   NOT used: lags the current week.
#
# FTN PLACEHOLDER ROWS (found 2026-10-13 on the week 5 pull)
# FTN publishes a row for every play of a game before it has charted the game.
# Until charting lands, those rows carry n_defense_box = 0, n_pass_rushers = 0
# and FALSE in every is_* flag, not NA. v1.0 tested !is.na() to decide a play
# was charted, so the placeholders counted as real charting: on the week 5
# pull 14 of 15 games were placeholders and the week came out at a 2.3% blitz
# rate, 1.6% play action and an average box of 0.43 defenders.
# .r62_screen_ftn() now marks a game charted only when at least
# R62_FTN_CHARTED_MIN_SHARE of its FTN rows show a box count or pass-rusher
# count of 1 or more. A real charted play always has defenders in the box, so
# charted games sit near 100% and placeholder games at 0%. Every FTN value
# column is set to NA for games that fail, which drops them from every
# *_ftn denominator exactly as an absent game was already dropped. Games with
# a share between 5% and 95% are flagged with a warning, because neither rule
# explains them. Coverage is written to s3_r62_ftn_coverage_wNN.csv.
# R/56 loads FTN through the same .r56_load_ftn() and needs the same screen;
# this file screens its own copy so it is protected either way.
#
# SHRINKAGE (situation grid only)
# A 27-cell grid against ~200 plays per team leaves single-digit n in most
# cells. Each metric in R62_SHRINK_METRICS gets a shrunk column:
#   shrunk = (n * raw + k * league_cell) / (n + k)
# k is fitted per metric per side on the 2025 season by method of moments on
# team x down x dist cells (deviations from the league cell value):
#   k = within-play variance / between-team variance of true cell rates.
# Raw and shrunk are both written; read raw when n is large.
#
# WHAT IS NOT HERE
#   True pressure rate (tracking data). pressure_rate = (sack + qb_hit) per
#   dropback, a proxy. Man/zone coverage. Pass rush win rate. Yards before
#   contact. Defensive player rows (queued, needs its own snap loader).
#
# SCHEMA TAG: s3_r62_matchup_v1
# Author: Christian K. LeBlanc
# Version: 1.0  [2026-09-29] Initial build.
#          1.1  [2026-10-13] FTN placeholder screen (.r62_screen_ftn), FTN
#               coverage file, run stops when require_ftn = TRUE and no game
#               in the run week is charted. Output columns unchanged, so the
#               schema tag is unchanged; FTN values for uncharted games are
#               now NA instead of 0.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(glue); library(here)
  library(nflreadr)
})

# R/56 supplies the pbp loader, the FTN loader, the cache dir and the neutral
# script definition. Guarded source, same pattern as R/57.
if (!exists("run_weekly_usage")) {
  source(here::here("R", "56_weekly_usage_report.R"))
}

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

R62_SEASON          <- R56_SEASON
R62_SCHEMA_TAG      <- "s3_r62_matchup_v1"
R62_OUT_DIR         <- R56_OUT_DIR
R62_BASELINE_SEASON <- 2025L
R62_BASELINE_FILE   <- file.path(R62_OUT_DIR,
                                 glue("s3_r62_baseline_{R62_BASELINE_SEASON}.rds"))

# Explosive plays are yards based (the common convention), not EPA based.
R62_EXPLOSIVE_PASS_YDS <- 15L
R62_EXPLOSIVE_RUSH_YDS <- 10L

# Blitz = five or more pass rushers (FTN codes exactly this; see header).
R62_BLITZ_RUSHERS <- 5L

# FTN placeholder screen (see header). A game counts as charted when at least
# this share of its FTN rows has a box count or pass-rusher count of 1+.
# Shares between the two warn bounds are reported as ambiguous.
R62_FTN_CHARTED_MIN_SHARE <- 0.5
R62_FTN_WARN_BOUNDS       <- c(0.05, 0.95)

# Every FTN value column this file reads. Blanked to NA for uncharted games.
R62_FTN_VALUE_COLS <- c("n_pass_rushers", "n_blitzers", "n_defense_box",
                        "n_offense_backfield", "is_play_action", "is_screen_pass",
                        "is_rpo", "is_motion", "is_qb_out_of_pocket",
                        "is_throw_away", "is_interception_worthy",
                        "is_catchable_ball", "is_contested_ball", "is_drop",
                        "is_created_reception", "is_qb_fault_sack", "is_qb_sneak")

# Shrinkage: cap on k so a metric with no measurable between-team spread in
# 2025 collapses to the league value instead of producing Inf.
R62_K_MAX <- 500

# Bucket levels, in display order.
R62_DOWN_LEVELS  <- c("1st", "2nd", "3rd-4th")
R62_DIST_LEVELS  <- c("1-3", "4-6", "7+")
R62_ZONE_LEVELS  <- c("own_half", "opp_21_50", "red_zone")
R62_BOX_LEVELS   <- c("light_6minus", "seven", "stacked_8plus")
R62_DEPTH_LEVELS <- c("behind_los", "short_0_9", "intermediate_10_19", "deep_20plus")
R62_SCORE_LEVELS <- c("trail_8plus", "one_score", "lead_8plus")
R62_TGT_POS      <- c("WR", "TE", "RB", "OTHER")

# Metrics shrunk in the situation grid and carried as 2025 baselines.
R62_SHRINK_METRICS <- c("epa_play", "success_rate", "pass_rate",
                        "epa_dropback", "success_dropback", "sack_rate",
                        "blitz_rate", "epa_rush", "success_rush")

# Cut names for the splits file (order is the app's picker order).
R62_CUTS <- c("blitz", "box", "play_action", "screen", "depth", "tgt_pos",
              "pass_location", "run_gap", "score_state", "neutral", "half",
              "third_down_dist", "blitz_x_down_dist", "blitz_x_situation")

# ------------------------------------------------------------------------------
# METRIC REGISTRY
# Every rate is mean(value_col) over the plays where den_col is TRUE. Adding a
# metric is one row here plus a dictionary row in R/57. n columns are named
# n_<den_col>.
# ------------------------------------------------------------------------------
R62_METRICS <- tibble::tribble(
  ~metric,               ~value_col,        ~den_col,
  # all scrimmage plays
  "epa_play",            "epa",             "is_play",
  "success_rate",        "success",         "is_play",
  "explosive_rate",      "explosive",       "is_play",
  "first_down_rate",     "first_down",      "is_play",
  "pass_rate",           "is_pass",         "is_play",
  "pass_oe",             "pass_oe",         "is_play",
  "avg_def_box",         "n_defense_box",   "box_known",
  "light_box_rate",      "light_box",       "box_known",
  "stacked_box_rate",    "stacked_box",     "box_known",
  # dropbacks (pass plays incl. sacks, plus scrambles)
  "epa_dropback",        "epa",             "dropback",
  "success_dropback",    "success",         "dropback",
  "explosive_pass_rate", "explosive",       "dropback",
  "sack_rate",           "sack",            "dropback",
  "pressure_rate",       "pressure",        "dropback",
  "int_rate",            "interception",    "dropback",
  "scramble_rate",       "qb_scramble",     "dropback",
  "blitz_rate",          "is_blitz",        "dropback_ftn",
  "avg_pass_rushers",    "n_pass_rushers",  "dropback_ftn",
  "rush3_or_fewer_rate", "rush3_or_fewer",  "dropback_ftn",
  "rush6_plus_rate",     "rush6_plus",      "dropback_ftn",
  "pa_rate",             "is_play_action",  "dropback_ftn",
  "screen_rate",         "is_screen_pass",  "dropback_ftn",
  "rpo_rate",            "is_rpo",          "dropback_ftn",
  "out_of_pocket_rate",  "is_qb_out_of_pocket", "dropback_ftn",
  "throwaway_rate",      "is_throw_away",   "dropback_ftn",
  "qb_fault_sack_share", "is_qb_fault_sack","sack_ftn",
  # thrown balls (complete + incomplete + interception)
  "comp_pct",            "complete_pass",   "thrown",
  "cpoe",                "cpoe",            "thrown",
  "adot",                "air_yards",       "thrown",
  "epa_target",          "epa",             "thrown",
  "yac_per_comp",        "yards_after_catch","completed",
  "deep_target_rate",    "deep_target",     "thrown",
  "int_worthy_rate",     "is_interception_worthy", "thrown_ftn",
  "catchable_rate",      "is_catchable_ball","thrown_ftn",
  "contested_rate",      "is_contested_ball","thrown_ftn",
  "drop_rate",           "is_drop",         "thrown_ftn",
  "created_rec_rate",    "is_created_reception", "thrown_ftn",
  # rushes (designed runs; scrambles excluded)
  "epa_rush",            "epa",             "is_rush",
  "success_rush",        "success",         "is_rush",
  "ypc",                 "yards_gained",    "is_rush",
  "stuff_rate",          "stuff",           "is_rush",
  "rush_conv_rate",      "rush_conv",       "is_rush",
  "explosive_rush_rate", "explosive",       "is_rush",
  "motion_rate",         "is_motion",       "play_ftn",
  "no_huddle_rate",      "no_huddle",       "is_play"
)

R62_DEN_COLS <- unique(R62_METRICS$den_col)

# ------------------------------------------------------------------------------
# LOADERS
# ------------------------------------------------------------------------------

#' Next week's games from the schedule. Returns NULL past week 18.
.r62_load_schedule <- function(season, week) {
  if (week >= 18L) return(NULL)
  sch <- tryCatch(nflreadr::load_schedules(season), error = function(e) NULL)
  if (is.null(sch)) {
    message("  schedule: unavailable, matchup file will be empty")
    return(NULL)
  }
  out <- sch %>%
    dplyr::filter(.data$game_type == "REG", .data$week == !!week + 1L) %>%
    dplyr::transmute(game_id, game_week = .data$week, away_team, home_team)
  message(glue("  schedule: {nrow(out)} games in week {week + 1L}"))
  out
}

#' Receiver position group lookup from the season roster.
.r62_position_lookup <- function(season) {
  roster <- tryCatch(nflreadr::load_rosters(season), error = function(e) NULL)
  if (is.null(roster) || !all(c("gsis_id", "position") %in% names(roster))) {
    return(tibble::tibble(receiver_player_id = character(), tgt_pos = character()))
  }
  roster %>%
    dplyr::filter(!is.na(.data$gsis_id)) %>%
    dplyr::distinct(gsis_id, .keep_all = TRUE) %>%
    dplyr::transmute(receiver_player_id = .data$gsis_id,
                     tgt_pos = dplyr::if_else(.data$position %in% c("WR", "TE", "RB"),
                                              .data$position, "OTHER"))
}

# ------------------------------------------------------------------------------
# FTN PLACEHOLDER SCREEN
# ------------------------------------------------------------------------------

#' Mark each FTN game charted or not and blank every FTN value column for the
#' games that are not. See the FTN PLACEHOLDER ROWS note in the header.
#' @param ftn FTN frame from .r56_load_ftn(), or NULL.
#' @return list(ftn = screened frame, games = one row per FTN game with
#'   n_rows, share_marked, charted).
.r62_screen_ftn <- function(ftn, min_share = R62_FTN_CHARTED_MIN_SHARE) {
  empty <- tibble::tibble(game_id = character(), n_rows = integer(),
                          share_marked = numeric(), charted = logical())
  if (is.null(ftn) || nrow(ftn) == 0L) return(list(ftn = ftn, games = empty))

  as_int <- function(col) {
    if (col %in% names(ftn)) suppressWarnings(as.integer(ftn[[col]]))
    else rep(NA_integer_, nrow(ftn))
  }
  box  <- as_int("n_defense_box")
  rush <- as_int("n_pass_rushers")
  marked <- (!is.na(box) & box >= 1L) | (!is.na(rush) & rush >= 1L)

  games <- tibble::tibble(game_id = ftn$nflverse_game_id, marked = marked) %>%
    dplyr::group_by(game_id) %>%
    dplyr::summarise(n_rows = dplyr::n(), share_marked = mean(marked),
                     .groups = "drop") %>%
    dplyr::mutate(charted = share_marked >= min_share)

  odd <- games %>%
    dplyr::filter(share_marked > R62_FTN_WARN_BOUNDS[1],
                  share_marked < R62_FTN_WARN_BOUNDS[2])
  if (nrow(odd) > 0L) {
    warning(glue("R/62 FTN screen: {nrow(odd)} game(s) are partly marked and fit ",
                 "neither the charted nor the placeholder pattern: ",
                 "{paste0(odd$game_id, ' (', round(100 * odd$share_marked), '%)', collapse = ', ')}. ",
                 "Games at or above {round(100 * min_share)}% are kept as charted."),
            call. = FALSE)
  }

  drop_ids <- games$game_id[!games$charted]
  if (length(drop_ids) > 0L) {
    idx <- ftn$nflverse_game_id %in% drop_ids
    for (cc in intersect(R62_FTN_VALUE_COLS, names(ftn))) ftn[[cc]][idx] <- NA
  }
  list(ftn = ftn, games = games)
}

#' One row per game played: is it charted? Games with no FTN rows at all are
#' uncharted. Prints a line for every week with an uncharted game.
.r62_ftn_coverage <- function(clean_pbp, ftn_games) {
  played <- clean_pbp %>%
    dplyr::distinct(game_id, game_week = as.integer(.data$week), home_team, away_team)
  cov <- played %>%
    dplyr::left_join(ftn_games, by = "game_id") %>%
    dplyr::mutate(charted = dplyr::coalesce(.data$charted, FALSE),
                  n_rows = dplyr::coalesce(.data$n_rows, 0L),
                  share_marked = dplyr::coalesce(.data$share_marked, 0)) %>%
    dplyr::arrange(game_week, game_id)
  by_wk <- cov %>%
    dplyr::group_by(game_week) %>%
    dplyr::summarise(n_games = dplyr::n(), n_charted = sum(charted),
                     missing = paste(game_id[!charted], collapse = ", "),
                     .groups = "drop")
  for (i in seq_len(nrow(by_wk))) {
    r <- by_wk[i, ]
    if (r$n_charted < r$n_games) {
      message(glue("  FTN week {r$game_week}: {r$n_charted} of {r$n_games} games ",
                   "charted. Not charted: {r$missing}"))
    }
  }
  if (all(by_wk$n_charted == by_wk$n_games)) {
    message(glue("  FTN: every game charted ({sum(by_wk$n_games)} games)"))
  }
  cov
}

# ------------------------------------------------------------------------------
# PLAY FRAME
# ------------------------------------------------------------------------------

#' One clean scrimmage play frame with FTN joined and every bucket and flag
#' this file uses. Same play filter as R/56's team table: pass and run plays,
#' kneels and spikes out, no garbage-time cut (the neutral flag is separate).
#' FTN is screened for placeholder rows first (.r62_screen_ftn). The per-game
#' coverage table rides along as attr(result, "ftn_coverage").
#' @param pbp Play-by-play from .r56_load_pbp().
#' @param ftn FTN frame from .r56_load_ftn(), or NULL.
#' @param pos_lk Receiver position lookup.
.r62_plays <- function(pbp, ftn, pos_lk) {

  clean <- pbp %>%
    dplyr::filter(!is.na(.data$posteam), !is.na(.data$defteam),
                  .data$play_type %in% c("pass", "run"),
                  .data$qb_kneel == 0, .data$qb_spike == 0,
                  !is.na(.data$epa))

  scr <- .r62_screen_ftn(ftn)
  ftn <- scr$ftn
  coverage <- .r62_ftn_coverage(clean, scr$games)

  if (!is.null(ftn)) {
    n_before <- nrow(clean)
    clean <- clean %>%
      dplyr::left_join(ftn, by = c("game_id" = "nflverse_game_id",
                                   "play_id" = "nflverse_play_id"))
    n_joined <- sum(!is.na(clean$n_pass_rushers) | !is.na(clean$n_defense_box))
    message(glue("  FTN join: {n_joined} of {n_before} scrimmage plays carry ",
                 "charting ({round(100 * n_joined / n_before, 1)}%)"))
  } else {
    for (cc in R56_FTN_COLS) clean[[cc]] <- NA
  }

  clean <- clean %>% dplyr::left_join(pos_lk, by = "receiver_player_id")

  num0 <- function(x) dplyr::coalesce(as.numeric(x), 0)
  lgl  <- function(x) as.logical(x)

  out <- clean %>%
    dplyr::mutate(
      # ---- play type flags ----
      is_play   = TRUE,
      is_pass   = .data$play_type == "pass",
      dropback  = num0(.data$qb_dropback) == 1,
      is_rush   = .data$play_type == "run" & num0(.data$qb_scramble) == 0,
      thrown    = num0(.data$complete_pass) == 1 | num0(.data$incomplete_pass) == 1 |
                  num0(.data$interception) == 1,
      completed = num0(.data$complete_pass) == 1,
      sack      = num0(.data$sack),
      qb_hit    = num0(.data$qb_hit),
      interception = num0(.data$interception),
      qb_scramble  = num0(.data$qb_scramble),
      complete_pass = num0(.data$complete_pass),
      first_down = num0(.data$first_down),
      touchdown  = num0(.data$touchdown),
      no_huddle  = num0(.data$no_huddle),
      # ---- outcomes ----
      success   = as.numeric(.data$epa > 0),
      explosive = as.numeric(
        (is_pass & num0(.data$yards_gained) >= R62_EXPLOSIVE_PASS_YDS) |
        (is_rush & num0(.data$yards_gained) >= R62_EXPLOSIVE_RUSH_YDS)),
      pressure  = as.numeric(sack == 1 | qb_hit == 1),
      stuff     = as.numeric(is_rush & num0(.data$yards_gained) <= 0),
      rush_conv = as.numeric(is_rush & (first_down == 1 | touchdown == 1)),
      deep_target = as.numeric(thrown & !is.na(.data$air_yards) & .data$air_yards >= 20),
      # ---- FTN ----
      n_pass_rushers = suppressWarnings(as.integer(.data$n_pass_rushers)),
      n_blitzers     = suppressWarnings(as.integer(.data$n_blitzers)),
      n_defense_box  = suppressWarnings(as.integer(.data$n_defense_box)),
      n_offense_backfield = suppressWarnings(as.integer(.data$n_offense_backfield)),
      is_blitz       = as.numeric(n_pass_rushers >= R62_BLITZ_RUSHERS),
      rush3_or_fewer = as.numeric(n_pass_rushers <= 3),
      rush6_plus     = as.numeric(n_pass_rushers >= 6),
      dplyr::across(dplyr::all_of(c("is_play_action", "is_screen_pass", "is_rpo",
                                    "is_motion", "is_qb_out_of_pocket",
                                    "is_throw_away", "is_interception_worthy",
                                    "is_catchable_ball", "is_contested_ball",
                                    "is_drop", "is_created_reception",
                                    "is_qb_fault_sack", "is_qb_sneak")),
                    ~ as.numeric(lgl(.x))),
      box_known    = is_play & !is.na(n_defense_box),
      light_box    = as.numeric(n_defense_box <= 6),
      stacked_box  = as.numeric(n_defense_box >= 8),
      dropback_ftn = dropback & !is.na(n_pass_rushers),
      sack_ftn     = dropback_ftn & sack == 1,
      thrown_ftn   = thrown & !is.na(is_catchable_ball),
      play_ftn     = is_play & !is.na(is_motion),
      # ---- buckets ----
      down_bucket = dplyr::case_when(.data$down == 1 ~ "1st",
                                     .data$down == 2 ~ "2nd",
                                     .data$down %in% c(3, 4) ~ "3rd-4th",
                                     TRUE ~ NA_character_),
      dist_bucket = dplyr::case_when(.data$ydstogo <= 3 ~ "1-3",
                                     .data$ydstogo <= 6 ~ "4-6",
                                     .data$ydstogo >= 7 ~ "7+",
                                     TRUE ~ NA_character_),
      zone = dplyr::case_when(.data$yardline_100 > 50 ~ "own_half",
                              .data$yardline_100 > 20 ~ "opp_21_50",
                              .data$yardline_100 <= 20 ~ "red_zone",
                              TRUE ~ NA_character_),
      score_state = dplyr::case_when(.data$score_differential <= -8 ~ "trail_8plus",
                                     .data$score_differential >= 8  ~ "lead_8plus",
                                     !is.na(.data$score_differential) ~ "one_score",
                                     TRUE ~ NA_character_),
      box_bucket = dplyr::case_when(n_defense_box <= 6 ~ "light_6minus",
                                    n_defense_box == 7 ~ "seven",
                                    n_defense_box >= 8 ~ "stacked_8plus",
                                    TRUE ~ NA_character_),
      depth_bucket = dplyr::case_when(!thrown | is.na(.data$air_yards) ~ NA_character_,
                                      .data$air_yards < 0   ~ "behind_los",
                                      .data$air_yards < 10  ~ "short_0_9",
                                      .data$air_yards < 20  ~ "intermediate_10_19",
                                      TRUE                  ~ "deep_20plus"),
      tgt_pos   = dplyr::if_else(thrown, dplyr::coalesce(.data$tgt_pos, "OTHER"),
                                 NA_character_),
      pass_loc  = dplyr::if_else(thrown, .data$pass_location, NA_character_),
      run_gap_lbl = dplyr::if_else(is_rush,
                                   dplyr::coalesce(.data$run_gap, .data$run_location),
                                   NA_character_),
      half = dplyr::case_when(.data$qtr %in% c(1, 2) ~ "first_half",
                              .data$qtr %in% c(3, 4) ~ "second_half",
                              TRUE ~ "overtime"),
      neutral = !is.na(.data$wp) & .data$wp >= R56_NEUTRAL_WP[1] &
                .data$wp <= R56_NEUTRAL_WP[2] & .data$qtr %in% R56_NEUTRAL_QTRS,
      blitz_lbl = dplyr::case_when(is_blitz == 1 ~ "blitz",
                                   is_blitz == 0 ~ "no_blitz",
                                   TRUE ~ NA_character_),
      pa_lbl = dplyr::case_when(is_play_action == 1 ~ "play_action",
                                is_play_action == 0 ~ "standard",
                                TRUE ~ NA_character_),
      screen_lbl = dplyr::case_when(is_screen_pass == 1 ~ "screen",
                                    is_screen_pass == 0 ~ "non_screen",
                                    TRUE ~ NA_character_),
      third_dist = dplyr::if_else(.data$down %in% c(3, 4), dist_bucket, NA_character_),
      neutral_lbl = dplyr::if_else(neutral, "neutral", NA_character_),
      blitz_x_dd  = dplyr::if_else(dropback_ftn & !is.na(down_bucket) & !is.na(dist_bucket),
                                   paste(down_bucket, dist_bucket, blitz_lbl, sep = "|"),
                                   NA_character_),
      blitz_x_sit = dplyr::if_else(dropback_ftn & !is.na(down_bucket) &
                                     !is.na(dist_bucket) & !is.na(zone),
                                   paste(down_bucket, dist_bucket, zone, blitz_lbl, sep = "|"),
                                   NA_character_)
    )
  attr(out, "ftn_coverage") <- coverage
  out
}

#' Every play twice: once keyed on the offense, once on the defense.
.r62_long <- function(plays) {
  dplyr::bind_rows(
    plays %>% dplyr::mutate(team = .data$posteam, side = "off"),
    plays %>% dplyr::mutate(team = .data$defteam, side = "def")
  )
}

# ------------------------------------------------------------------------------
# GENERIC METRIC BLOCK
# ------------------------------------------------------------------------------

.r62_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA_real_ else mean(x)
}

#' The metric block for any grouping. n_<den> columns first, then one column
#' per registry row. Rates are mean(value) over rows where the denominator
#' flag is TRUE; an empty denominator gives NA, never NaN.
.r62_summarise <- function(df, by, metrics = R62_METRICS) {
  g <- df %>% dplyr::group_by(dplyr::across(dplyr::all_of(by)))

  out <- g %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(R62_DEN_COLS), ~ sum(.x %in% TRUE),
                    .names = "n_{.col}"),
      .groups = "drop")

  for (i in seq_len(nrow(metrics))) {
    m <- metrics$metric[i]; v <- metrics$value_col[i]; d <- metrics$den_col[i]
    tmp <- g %>%
      dplyr::summarise(!!m := .r62_mean(.data[[v]][.data[[d]] %in% TRUE]),
                       .groups = "drop")
    out <- dplyr::left_join(out, tmp, by = by)
  }
  out
}

#' n column that goes with a metric.
.r62_n_col <- function(metric, metrics = R62_METRICS) {
  paste0("n_", metrics$den_col[metrics$metric == metric])
}

# ------------------------------------------------------------------------------
# TEAM TABLE (wide, season to date, both sides)
# ------------------------------------------------------------------------------

#' Drive-level rates per team x side from the raw pbp (needs fixed_drive).
#' points_per_drive counts 7 for a touchdown and 3 for a field goal, an
#' approximation that ignores missed extra points and two-point tries.
.r62_drives <- function(pbp) {
  if (!all(c("fixed_drive", "fixed_drive_result") %in% names(pbp))) {
    return(NULL)
  }
  d <- pbp %>%
    dplyr::filter(!is.na(.data$posteam), !is.na(.data$fixed_drive)) %>%
    dplyr::group_by(game_id, posteam, defteam, fixed_drive) %>%
    dplyr::summarise(
      result = dplyr::last(stats::na.omit(.data$fixed_drive_result)),
      scrim  = sum(.data$play_type %in% c("pass", "run") &
                     .data$qb_kneel == 0 & .data$qb_spike == 0),
      .groups = "drop") %>%
    dplyr::mutate(
      pts   = dplyr::case_when(result %in% "Touchdown" ~ 7, result %in% "Field goal" ~ 3,
                               TRUE ~ 0),
      score = result %in% c("Touchdown", "Field goal"),
      turnover = result %in% "Turnover",
      three_out = result %in% "Punt" & scrim <= 3
    )
  long <- dplyr::bind_rows(d %>% dplyr::mutate(team = posteam, side = "off"),
                           d %>% dplyr::mutate(team = defteam, side = "def"))
  long %>%
    dplyr::group_by(side, team) %>%
    dplyr::summarise(drives = dplyr::n(),
                     points_per_drive   = mean(pts),
                     scoring_drive_rate = mean(score),
                     turnover_drive_rate = mean(turnover),
                     three_and_out_rate = mean(three_out),
                     plays_per_drive    = mean(scrim),
                     .groups = "drop")
}

#' Points for (off) and against (def) per game from the final scores.
.r62_points <- function(pbp) {
  pts <- pbp %>%
    dplyr::distinct(game_id, home_team, away_team, home_score, away_score)
  dplyr::bind_rows(
    pts %>% dplyr::transmute(game_id, team = home_team, pf = home_score, pa = away_score),
    pts %>% dplyr::transmute(game_id, team = away_team, pf = away_score, pa = home_score)
  ) %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(games = dplyr::n_distinct(game_id),
                     points_for_pg = mean(pf, na.rm = TRUE),
                     points_against_pg = mean(pa, na.rm = TRUE),
                     .groups = "drop")
}

#' Penalties per game per side (penalty_team is the team that committed it).
.r62_penalties <- function(pbp) {
  if (!all(c("penalty", "penalty_team", "penalty_yards") %in% names(pbp))) return(NULL)
  pbp %>%
    dplyr::filter(.data$penalty %in% 1, !is.na(.data$penalty_team)) %>%
    dplyr::mutate(side = dplyr::if_else(.data$penalty_team == .data$posteam, "off", "def"),
                  team = .data$penalty_team) %>%
    dplyr::group_by(side, team) %>%
    dplyr::summarise(penalties = dplyr::n(),
                     penalty_yards = sum(.data$penalty_yards, na.rm = TRUE),
                     .groups = "drop")
}

#' Team x side, season to date. Wide. Includes neutral-script versions of the
#' headline rates, drive rates, points, penalties, and league ranks.
build_team_table62 <- function(plays, pbp, week) {

  long <- .r62_long(plays)
  base <- .r62_summarise(long, c("side", "team"))

  neutral <- .r62_summarise(long %>% dplyr::filter(.data$neutral), c("side", "team")) %>%
    dplyr::select(side, team, neutral_plays = n_is_play,
                  neutral_epa_play = epa_play, neutral_success_rate = success_rate,
                  neutral_pass_rate = pass_rate, neutral_pass_oe = pass_oe,
                  neutral_epa_dropback = epa_dropback, neutral_epa_rush = epa_rush,
                  neutral_blitz_rate = blitz_rate)

  drives <- .r62_drives(pbp)
  pts    <- .r62_points(pbp)
  pens   <- .r62_penalties(pbp)

  # pace: seconds of game clock between consecutive plays of the same drive,
  # from the play frame. An offense property; on the def row it is pace FACED.
  pace <- plays %>%
    dplyr::arrange(game_id, play_id) %>%
    dplyr::group_by(game_id, posteam, drive) %>%
    dplyr::mutate(sec_next = dplyr::lag(game_seconds_remaining) - game_seconds_remaining) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(sec_next), sec_next > 0, sec_next < 120) %>%
    dplyr::group_by(team = posteam) %>%
    dplyr::summarise(sec_per_play = mean(sec_next), .groups = "drop")

  out <- base %>%
    dplyr::left_join(neutral, by = c("side", "team")) %>%
    dplyr::left_join(pts, by = "team") %>%
    dplyr::mutate(points_pg = dplyr::if_else(side == "off", points_for_pg, points_against_pg)) %>%
    dplyr::select(-points_for_pg, -points_against_pg)
  if (!is.null(drives)) out <- out %>% dplyr::left_join(drives, by = c("side", "team"))
  if (!is.null(pens)) {
    out <- out %>% dplyr::left_join(pens, by = c("side", "team")) %>%
      dplyr::mutate(penalties_pg = dplyr::coalesce(penalties, 0L) / games,
                    penalty_yards_pg = dplyr::coalesce(penalty_yards, 0) / games) %>%
      dplyr::select(-penalties, -penalty_yards)
  }
  # pace is an offense property; on the def row it is the pace FACED
  out <- out %>%
    dplyr::left_join(pace, by = "team") %>%
    dplyr::mutate(plays_pg = n_is_play / games,
                  dropbacks_pg = n_dropback / games)

  # Ranks within side. Offense: higher EPA = rank 1. Defense: lower = rank 1.
  # Tendency ranks (blitz, pass rate) are "most" = rank 1 on both sides.
  out <- out %>%
    dplyr::group_by(side) %>%
    dplyr::mutate(
      rank_epa_play     = dplyr::if_else(side == "off",
                                         dplyr::min_rank(dplyr::desc(epa_play)),
                                         dplyr::min_rank(epa_play)),
      rank_epa_dropback = dplyr::if_else(side == "off",
                                         dplyr::min_rank(dplyr::desc(epa_dropback)),
                                         dplyr::min_rank(epa_dropback)),
      rank_epa_rush     = dplyr::if_else(side == "off",
                                         dplyr::min_rank(dplyr::desc(epa_rush)),
                                         dplyr::min_rank(epa_rush)),
      rank_success_rate = dplyr::if_else(side == "off",
                                         dplyr::min_rank(dplyr::desc(success_rate)),
                                         dplyr::min_rank(success_rate)),
      rank_blitz_rate    = dplyr::min_rank(dplyr::desc(blitz_rate)),
      rank_pressure_rate = dplyr::min_rank(dplyr::desc(pressure_rate)),
      rank_pass_rate     = dplyr::min_rank(dplyr::desc(pass_rate))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(week = week, season = R62_SEASON, schema_tag = R62_SCHEMA_TAG) %>%
    dplyr::relocate(side, team, games, plays_pg, dropbacks_pg, points_pg) %>%
    dplyr::arrange(side, rank_epa_play)
  out
}

# ------------------------------------------------------------------------------
# SITUATION GRID
# ------------------------------------------------------------------------------

#' team x side x down_bucket x dist_bucket x zone. Raw metrics for every
#' registry row; league cell value and shrunk value for R62_SHRINK_METRICS.
#' Baselines from 2025 are joined by run_weekly_matchup().
build_situation_grid <- function(plays, week) {
  keys <- c("down_bucket", "dist_bucket", "zone")
  long <- .r62_long(plays) %>%
    dplyr::filter(!is.na(down_bucket), !is.na(dist_bucket), !is.na(zone))

  grid <- .r62_summarise(long, c("side", "team", keys))

  league <- .r62_summarise(plays %>%
                             dplyr::filter(!is.na(down_bucket), !is.na(dist_bucket),
                                           !is.na(zone)),
                           keys) %>%
    dplyr::select(dplyr::all_of(keys), dplyr::all_of(R62_SHRINK_METRICS)) %>%
    dplyr::rename_with(~ paste0("league_", .x), dplyr::all_of(R62_SHRINK_METRICS))

  grid %>%
    dplyr::left_join(league, by = keys) %>%
    dplyr::mutate(down_bucket = factor(down_bucket, R62_DOWN_LEVELS),
                  dist_bucket = factor(dist_bucket, R62_DIST_LEVELS),
                  zone = factor(zone, R62_ZONE_LEVELS)) %>%
    dplyr::arrange(side, team, down_bucket, dist_bucket, zone) %>%
    dplyr::mutate(dplyr::across(c(down_bucket, dist_bucket, zone), as.character),
                  week = week, season = R62_SEASON, schema_tag = R62_SCHEMA_TAG)
}

# ------------------------------------------------------------------------------
# SPLITS (long)
# ------------------------------------------------------------------------------

#' Which play-frame column carries each cut's level, and which plays enter it.
R62_CUT_SPEC <- tibble::tribble(
  ~cut,                ~level_col,     ~den,
  "blitz",             "blitz_lbl",    "dropback_ftn",
  "box",               "box_bucket",   "box_known",
  "play_action",       "pa_lbl",       "dropback_ftn",
  "screen",            "screen_lbl",   "dropback_ftn",
  "depth",             "depth_bucket", "thrown",
  "tgt_pos",           "tgt_pos",      "thrown",
  "pass_location",     "pass_loc",     "thrown",
  "run_gap",           "run_gap_lbl",  "is_rush",
  "score_state",       "score_state",  "is_play",
  "neutral",           "neutral_lbl",  "is_play",
  "half",              "half",         "is_play",
  "third_down_dist",   "third_dist",   "is_play",
  "blitz_x_down_dist", "blitz_x_dd",   "dropback_ftn",
  "blitz_x_situation", "blitz_x_sit",  "dropback_ftn"
)

#' One cut for one frame (long or single). Returns cut, level, keys, metrics.
.r62_one_cut <- function(df, cut, by_extra = character()) {
  spec <- R62_CUT_SPEC[R62_CUT_SPEC$cut == cut, ]
  sub <- df %>%
    dplyr::filter(.data[[spec$den]] %in% TRUE, !is.na(.data[[spec$level_col]])) %>%
    dplyr::mutate(level = .data[[spec$level_col]])
  if (nrow(sub) == 0L) return(NULL)
  .r62_summarise(sub, c(by_extra, "level")) %>%
    dplyr::mutate(cut = cut) %>%
    dplyr::relocate(cut, .before = level)
}

#' Every cut in R62_CUTS for team x side, with the league value of the same
#' cut/level joined as league_<metric> for the headline metrics.
build_splits <- function(plays, week) {
  long <- .r62_long(plays)
  team_side <- dplyr::bind_rows(lapply(R62_CUTS, function(ct)
    .r62_one_cut(long, ct, by_extra = c("side", "team"))))
  league <- dplyr::bind_rows(lapply(R62_CUTS, function(ct) .r62_one_cut(plays, ct))) %>%
    dplyr::select(cut, level, dplyr::all_of(R62_SHRINK_METRICS), epa_target, n_is_play) %>%
    dplyr::rename_with(~ paste0("league_", .x),
                       c(dplyr::all_of(R62_SHRINK_METRICS), "epa_target", "n_is_play"))
  team_side %>%
    dplyr::left_join(league, by = c("cut", "level")) %>%
    dplyr::mutate(cut = factor(cut, R62_CUTS)) %>%
    dplyr::arrange(side, team, cut, level) %>%
    dplyr::mutate(cut = as.character(cut),
                  week = week, season = R62_SEASON, schema_tag = R62_SCHEMA_TAG) %>%
    dplyr::relocate(side, team, cut, level)
}

# ------------------------------------------------------------------------------
# 2025 BASELINE: SHRINKAGE k AND BASELINE VALUES
# ------------------------------------------------------------------------------

#' Method-of-moments k per metric per side, fitted on team x down x dist cells
#' (zone pooled) as deviations from the league cell value.
#'   k = pooled within-play variance / between-team variance of true rates
#' where between_true = var(cell deviations) - mean(within / n_cell), floored
#' at a small positive number so k is finite, then capped at R62_K_MAX.
.r62_fit_k <- function(plays_base) {
  keys <- c("down_bucket", "dist_bucket")
  long <- .r62_long(plays_base) %>% dplyr::filter(!is.na(down_bucket), !is.na(dist_bucket))
  cells  <- .r62_summarise(long, c("side", "team", keys))
  league <- .r62_summarise(plays_base %>% dplyr::filter(!is.na(down_bucket), !is.na(dist_bucket)),
                           keys)
  out <- list()
  for (m in R62_SHRINK_METRICS) {
    v <- R62_METRICS$value_col[R62_METRICS$metric == m]
    d <- R62_METRICS$den_col[R62_METRICS$metric == m]
    ncol_ <- paste0("n_", d)
    vals <- plays_base[[v]][plays_base[[d]] %in% TRUE]
    within <- stats::var(vals, na.rm = TRUE)
    for (s in c("off", "def")) {
      cc <- cells %>% dplyr::filter(side == s) %>%
        dplyr::select(dplyr::all_of(keys), team, x = dplyr::all_of(m), n = dplyr::all_of(ncol_)) %>%
        dplyr::left_join(league %>% dplyr::select(dplyr::all_of(keys), lg = dplyr::all_of(m)),
                         by = keys) %>%
        dplyr::filter(!is.na(x), n >= 5)
      dev <- cc$x - cc$lg
      between_obs <- stats::var(dev)
      noise <- mean(within / cc$n)
      between_true <- max(between_obs - noise, within / (R62_K_MAX * 4))
      k <- min(within / between_true, R62_K_MAX)
      out[[length(out) + 1L]] <- tibble::tibble(side = s, metric = m,
                                                k = k, within_var = within,
                                                between_var = between_true,
                                                n_cells = nrow(cc))
    }
  }
  dplyr::bind_rows(out)
}

#' Build or read the 2025 baseline cache: k table, situation baselines,
#' team baselines. Loads 2025 pbp + FTN only when the cache is absent or
#' force = TRUE.
.r62_baseline <- function(force = FALSE, season = R62_BASELINE_SEASON) {
  if (!force && file.exists(R62_BASELINE_FILE)) {
    b <- readRDS(R62_BASELINE_FILE)
    message(glue("  baseline: read {basename(R62_BASELINE_FILE)} ",
                 "({nrow(b$k)} k rows, built {b$built})"))
    return(b)
  }
  message(glue("  baseline: building from {season} (one-time; cached afterwards)..."))
  pbp_b <- .r56_load_pbp(season, 18L)
  ftn_b <- .r56_load_ftn(season, 18L, require_week = FALSE)
  pos_b <- .r62_position_lookup(season)
  plays_b <- .r62_plays(pbp_b, ftn_b, pos_b)

  k <- .r62_fit_k(plays_b)
  sit <- build_situation_grid(plays_b, 18L) %>%
    dplyr::select(side, team, down_bucket, dist_bucket, zone,
                  dplyr::all_of(R62_SHRINK_METRICS), n_is_play, n_dropback, n_is_rush) %>%
    dplyr::rename_with(~ paste0("base_", .x),
                       c(dplyr::all_of(R62_SHRINK_METRICS), "n_is_play", "n_dropback", "n_is_rush"))
  team <- .r62_summarise(.r62_long(plays_b), c("side", "team")) %>%
    dplyr::select(side, team, dplyr::all_of(R62_SHRINK_METRICS), pressure_rate, n_is_play) %>%
    dplyr::rename_with(~ paste0("base_", .x),
                       c(dplyr::all_of(R62_SHRINK_METRICS), "pressure_rate", "n_is_play"))
  b <- list(season = season, built = format(Sys.time(), "%Y-%m-%d %H:%M"),
            k = k, situation = sit, team = team)
  dir.create(dirname(R62_BASELINE_FILE), recursive = TRUE, showWarnings = FALSE)
  saveRDS(b, R62_BASELINE_FILE)
  message(glue("  baseline: wrote {basename(R62_BASELINE_FILE)}"))
  message("  baseline k (higher = more shrinkage):")
  print(as.data.frame(k %>% dplyr::select(side, metric, k, n_cells) %>%
                        dplyr::mutate(k = round(k, 1))), row.names = FALSE)
  b
}

#' Add shrunk_<metric> columns to the situation grid.
.r62_apply_shrinkage <- function(grid, k_tbl) {
  for (m in R62_SHRINK_METRICS) {
    ncol_ <- .r62_n_col(m)
    kk <- k_tbl %>% dplyr::select(side, metric, k) %>% dplyr::filter(metric == m) %>%
      dplyr::select(-metric)
    grid <- grid %>%
      dplyr::left_join(kk, by = "side") %>%
      dplyr::mutate(
        !!paste0("shrunk_", m) := {
          n <- .data[[ncol_]]; raw <- .data[[m]]; lg <- .data[[paste0("league_", m)]]
          dplyr::if_else(is.na(raw) | n == 0, lg, (n * raw + k * lg) / (n + k))
        }) %>%
      dplyr::select(-k)
  }
  grid
}

# ------------------------------------------------------------------------------
# MATCHUPS
# ------------------------------------------------------------------------------

#' Metric compared for each cut. The matchup row carries offense value,
#' defense value (allowed), league value, both n's, and
#'   naive_expect = off + def - league
#' an additive expectation with no interaction term. It is a reading aid, not
#' a model, and nothing downstream consumes it.
R62_MATCHUP_METRIC <- c(
  overall = "epa_play", situation = "epa_play",
  blitz = "epa_dropback", box = "epa_play", play_action = "epa_dropback",
  screen = "epa_dropback", depth = "epa_target", tgt_pos = "epa_target",
  pass_location = "epa_target", run_gap = "epa_rush", score_state = "epa_play",
  neutral = "epa_play", half = "epa_play", third_down_dist = "first_down_rate",
  blitz_x_down_dist = "epa_dropback", blitz_x_situation = "epa_dropback"
)

#' Next week's games. One row per offense-defense pairing x cut x level.
#' Situation rows also carry the defense's blitz rate in that cell and the
#' offense's pass rate in that cell (the tendency alongside the result).
build_matchups <- function(team, situation, splits, schedule, week) {
  if (is.null(schedule) || nrow(schedule) == 0L) {
    return(tibble::tibble())
  }
  pairs <- dplyr::bind_rows(
    schedule %>% dplyr::transmute(game_id, game_week, off_team = home_team,
                                  def_team = away_team, off_home = TRUE),
    schedule %>% dplyr::transmute(game_id, game_week, off_team = away_team,
                                  def_team = home_team, off_home = FALSE)
  )

  # overall
  ov <- pairs %>%
    dplyr::left_join(team %>% dplyr::filter(side == "off") %>%
                       dplyr::select(off_team = team, off_value = epa_play,
                                     off_n = n_is_play),
                     by = "off_team") %>%
    dplyr::left_join(team %>% dplyr::filter(side == "def") %>%
                       dplyr::select(def_team = team, def_value = epa_play,
                                     def_n = n_is_play, def_tendency = blitz_rate),
                     by = "def_team") %>%
    dplyr::mutate(cut = "overall", level = "all", metric = "epa_play",
                  league_value = mean(team$epa_play[team$side == "off"], na.rm = TRUE),
                  off_tendency = NA_real_)

  # situation grid
  sit_off <- situation %>% dplyr::filter(side == "off") %>%
    dplyr::transmute(off_team = team, level = paste(down_bucket, dist_bucket, zone, sep = "|"),
                     off_value = shrunk_epa_play, off_raw = epa_play, off_n = n_is_play,
                     off_tendency = pass_rate, league_value = league_epa_play)
  sit_def <- situation %>% dplyr::filter(side == "def") %>%
    dplyr::transmute(def_team = team, level = paste(down_bucket, dist_bucket, zone, sep = "|"),
                     def_value = shrunk_epa_play, def_raw = epa_play, def_n = n_is_play,
                     def_tendency = blitz_rate)
  st <- pairs %>%
    dplyr::inner_join(sit_off, by = "off_team", relationship = "many-to-many") %>%
    dplyr::inner_join(sit_def, by = c("def_team", "level")) %>%
    dplyr::mutate(cut = "situation", metric = "epa_play_shrunk")

  # splits
  sp_rows <- lapply(R62_CUTS, function(ct) {
    m <- R62_MATCHUP_METRIC[[ct]]
    lg <- paste0("league_", m)
    if (!m %in% names(splits)) return(NULL)
    lg_col <- if (lg %in% names(splits)) lg else NA_character_
    o <- splits %>% dplyr::filter(side == "off", cut == ct) %>%
      dplyr::transmute(off_team = team, level,
                       off_value = .data[[m]], off_n = .data[[.r62_n_col(m)]],
                       league_value = if (!is.na(lg_col)) .data[[lg_col]] else NA_real_,
                       off_tendency = if (ct %in% c("depth", "tgt_pos", "pass_location", "run_gap"))
                         .data$n_is_play else NA_real_)
    d <- splits %>% dplyr::filter(side == "def", cut == ct) %>%
      dplyr::transmute(def_team = team, level,
                       def_value = .data[[m]], def_n = .data[[.r62_n_col(m)]],
                       def_tendency = if (ct %in% c("blitz", "blitz_x_down_dist",
                                                    "blitz_x_situation")) .data$blitz_rate
                                      else .data$n_is_play)
    pairs %>%
      dplyr::inner_join(o, by = "off_team", relationship = "many-to-many") %>%
      dplyr::inner_join(d, by = c("def_team", "level")) %>%
      dplyr::mutate(cut = ct, metric = m)
  })
  sp <- dplyr::bind_rows(sp_rows)

  dplyr::bind_rows(ov, st, sp) %>%
    dplyr::mutate(naive_expect = off_value + def_value - league_value,
                  gap_vs_league_off = off_value - league_value,
                  gap_vs_league_def = def_value - league_value,
                  week = week, season = R62_SEASON, schema_tag = R62_SCHEMA_TAG) %>%
    dplyr::select(game_id, game_week, off_team, def_team, off_home, cut, level, metric,
                  off_value, def_value, league_value, naive_expect,
                  gap_vs_league_off, gap_vs_league_def, off_n, def_n,
                  off_tendency, def_tendency,
                  dplyr::any_of(c("off_raw", "def_raw")),
                  week, season, schema_tag) %>%
    dplyr::arrange(game_id, off_team, cut, level)
}

# ------------------------------------------------------------------------------
# WEEK LOG
# ------------------------------------------------------------------------------

#' The team table one game week at a time (games = 1 on every row).
build_team_weeks62 <- function(plays, pbp, week) {
  out <- lapply(seq_len(week), function(w) {
    pl <- plays %>% dplyr::filter(.data$week == !!w)
    if (nrow(pl) == 0L) return(NULL)
    build_team_table62(pl, pbp %>% dplyr::filter(.data$week == !!w), w) %>%
      dplyr::rename(game_week = week)
  })
  dplyr::bind_rows(out) %>%
    dplyr::mutate(through_week = week) %>%
    dplyr::relocate(game_week, .after = team) %>%
    dplyr::arrange(side, team, game_week)
}

# ------------------------------------------------------------------------------
# GATES
# ------------------------------------------------------------------------------

#' Offense and defense rows must cover the same plays, and the offense
#' epa_play must reconcile to R/56's teams file for the same week when one
#' exists on disk (same play filter, so they must agree to float precision).
.r62_gate <- function(team, week) {
  n_off <- sum(team$n_is_play[team$side == "off"])
  n_def <- sum(team$n_is_play[team$side == "def"])
  if (n_off != n_def) {
    stop(glue("R/62 gate FAILED: offense rows cover {n_off} plays, defense rows ",
              "{n_def}. Nothing written."), call. = FALSE)
  }
  f56 <- file.path(R56_OUT_DIR, sprintf("s3_r56_teams_w%02d.csv", week))
  if (file.exists(f56)) {
    t56 <- readr::read_csv(f56, show_col_types = FALSE, progress = FALSE)
    chk <- team %>% dplyr::filter(side == "off") %>%
      dplyr::select(team, epa_play) %>%
      dplyr::inner_join(t56 %>% dplyr::select(team, epa56 = epa_play), by = "team") %>%
      dplyr::mutate(gap = abs(epa_play - epa56))
    bad <- sum(chk$gap > 1e-6, na.rm = TRUE)
    if (bad > 0L) {
      warning(glue("R/62 gate: {bad} team(s) where offense epa_play differs from ",
                   "s3_r56_teams_w{sprintf('%02d', week)} by more than 1e-6 ",
                   "(max {round(max(chk$gap), 4)}). Same filter is expected; ",
                   "check that both ran on the same pbp pull."), call. = FALSE)
    } else {
      message(glue("  gate: PASS (off/def cover {n_off} plays each; offense ",
                   "epa_play matches R/56 for {nrow(chk)} teams)"))
    }
  } else {
    message(glue("  gate: PASS (off/def cover {n_off} plays each; no R/56 teams ",
                 "file for week {week} to reconcile against)"))
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# ENTRY POINT
# ------------------------------------------------------------------------------

#' Run the weekly matchup report.
#' @param week Integer 1-18. The week that just finished.
#' @param season Integer.
#' @param write Logical. Write the CSVs to R62_OUT_DIR.
#' @param force_baseline Logical. Rebuild the 2025 baseline cache.
#' @param require_ftn Logical. Stop if FTN charting for `week` is not yet
#'   published (default), counting placeholder-only games as unpublished.
#'   FALSE proceeds with NA in every FTN column for uncharted games.
#' @param n_print Rows per printed section.
#' @return Invisibly, list(team, situation, splits, matchup, team_weeks, k,
#'   ftn_coverage).
run_weekly_matchup <- function(week, season = R62_SEASON, write = TRUE,
                               force_baseline = FALSE, require_ftn = TRUE,
                               n_print = 12L) {

  week <- as.integer(week)
  stopifnot(week >= 1L, week <= 18L)
  message(glue("\n{strrep('=', 70)}\nR/62 WEEKLY MATCHUP -- Season {season}, through week {week}\n",
               "{strrep('=', 70)}\n[1/7] Loading sources..."))

  pbp <- .r56_load_pbp(season, week)
  ftn <- .r56_load_ftn(season, week, require_week = require_ftn)
  pos <- .r62_position_lookup(season)
  sch <- .r62_load_schedule(season, week)

  message("[2/7] Play frame...")
  plays <- .r62_plays(pbp, ftn, pos)
  message(glue("  {format(nrow(plays), big.mark = ',')} scrimmage plays, ",
               "{sum(plays$dropback)} dropbacks, {sum(plays$is_rush)} rushes"))

  # FTN coverage for the run week. Placeholder-only weeks are treated as not
  # published: stop when require_ftn = TRUE, carry NA otherwise.
  ftn_coverage <- attr(plays, "ftn_coverage")
  cov_wk <- ftn_coverage[which(ftn_coverage$game_week == week), , drop = FALSE]
  n_charted_wk <- sum(cov_wk$charted)
  if (isTRUE(require_ftn) && nrow(cov_wk) > 0L && n_charted_wk == 0L) {
    stop(glue("R/62: FTN has not charted any week {week} game yet (all ",
              "{nrow(cov_wk)} are placeholder or missing). Rerun later, or pass ",
              "require_ftn = FALSE to write NA in every FTN column for the week."),
         call. = FALSE)
  }
  ftn_coverage <- ftn_coverage %>%
    dplyr::mutate(week = week, season = R62_SEASON, schema_tag = R62_SCHEMA_TAG)

  message("[3/7] Baseline (2025 shrinkage k + baselines)...")
  base <- .r62_baseline(force = force_baseline)

  message("[4/7] Team table, both sides...")
  team <- build_team_table62(plays, pbp, week) %>%
    dplyr::left_join(base$team, by = c("side", "team"))
  .r62_gate(team, week)

  message("[5/7] Situation grid + shrinkage...")
  situation <- build_situation_grid(plays, week) %>%
    .r62_apply_shrinkage(base$k) %>%
    dplyr::left_join(base$situation,
                     by = c("side", "team", "down_bucket", "dist_bucket", "zone"))

  message("[6/7] Splits and matchups...")
  splits  <- build_splits(plays, week)
  matchup <- build_matchups(team, situation, splits, sch, week)

  message("[7/7] Team week log...")
  team_weeks <- build_team_weeks62(plays, pbp, week)

  if (isTRUE(write)) {
    dir.create(R62_OUT_DIR, recursive = TRUE, showWarnings = FALSE)
    tag <- sprintf("w%02d", week)
    readr::write_csv(team,       file.path(R62_OUT_DIR, glue("s3_r62_team_{tag}.csv")))
    readr::write_csv(situation,  file.path(R62_OUT_DIR, glue("s3_r62_situation_{tag}.csv")))
    readr::write_csv(splits,     file.path(R62_OUT_DIR, glue("s3_r62_splits_{tag}.csv")))
    readr::write_csv(matchup,    file.path(R62_OUT_DIR, glue("s3_r62_matchup_{tag}.csv")))
    readr::write_csv(team_weeks, file.path(R62_OUT_DIR, glue("s3_r62_team_weeks_{tag}.csv")))
    readr::write_csv(ftn_coverage, file.path(R62_OUT_DIR, glue("s3_r62_ftn_coverage_{tag}.csv")))
    message(glue("  Wrote 6 files to {R62_OUT_DIR} ({tag}): team {nrow(team)}, ",
                 "situation {nrow(situation)}, splits {nrow(splits)}, ",
                 "matchup {nrow(matchup)}, team_weeks {nrow(team_weeks)}, ",
                 "ftn_coverage {nrow(ftn_coverage)} rows"))
  }
  message(glue("  FTN week {week}: {n_charted_wk} of {nrow(cov_wk)} games charted. ",
               "Every FTN rate this run covers charted games only."))

  # ---- console report ----
  pr <- function(df) print(as.data.frame(df), row.names = FALSE, digits = 3)

  cat("\n\n--- DEFENSE: EPA per play allowed (lower = better) ---\n\n")
  team %>% dplyr::filter(side == "def") %>%
    dplyr::select(team, games, epa_play, success_rate, epa_dropback, epa_rush,
                  blitz_rate, pressure_rate, sack_rate, points_pg) %>%
    dplyr::arrange(epa_play) %>% head(n_print) %>% pr()

  cat("\n--- DEFENSE: blitz rate leaders ---\n\n")
  team %>% dplyr::filter(side == "def") %>%
    dplyr::select(team, n_dropback_ftn, blitz_rate, avg_pass_rushers, pressure_rate,
                  sack_rate, epa_dropback) %>%
    dplyr::arrange(dplyr::desc(blitz_rate)) %>% head(n_print) %>% pr()

  cat("\n--- OFFENSE vs blitz (EPA per dropback when blitzed, top and bottom) ---\n\n")
  vb <- splits %>% dplyr::filter(side == "off", cut == "blitz") %>%
    dplyr::select(team, level, n_dropback_ftn, epa_dropback, success_dropback,
                  sack_rate) %>%
    tidyr::pivot_wider(names_from = level,
                       values_from = c(n_dropback_ftn, epa_dropback, success_dropback,
                                       sack_rate)) %>%
    dplyr::mutate(blitz_delta = epa_dropback_blitz - epa_dropback_no_blitz) %>%
    dplyr::arrange(dplyr::desc(blitz_delta))
  pr(head(vb, n_print))

  if (nrow(matchup) > 0L) {
    cat("\n--- NEXT WEEK: overall matchup (naive_expect = off + def - league) ---\n\n")
    matchup %>% dplyr::filter(cut == "overall") %>%
      dplyr::select(game_id, off_team, def_team, off_value, def_value, naive_expect,
                    def_blitz_rate = def_tendency) %>%
      dplyr::arrange(dplyr::desc(naive_expect)) %>% pr()
  }

  invisible(list(team = team, situation = situation, splits = splits,
                 matchup = matchup, team_weeks = team_weeks, k = base$k,
                 ftn_coverage = ftn_coverage))
}

# RUN
#   source(here::here("R", "62_weekly_matchup_report.R"))
#   out <- run_weekly_matchup(week = 3)
