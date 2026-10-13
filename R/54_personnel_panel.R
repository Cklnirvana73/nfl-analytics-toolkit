# ==============================================================================
# 54_personnel_panel.R
# ==============================================================================
#
# PURPOSE
# -------
# Read-only descriptive layer joining nflverse participation data (offensive
# personnel groups and per-play player IDs) to play-by-play scoring. Answers
# three questions that the projection pipeline does not touch:
#
#   A1  Team-grain personnel usage rates and points per snap by group.
#   B5  Per-player snap and fantasy-point exposure by personnel group.
#   C3  Whether team play volume converts into individual fantasy points once
#       opportunity share is held fixed, and whether play volume is
#       predictable one season ahead.
#
# This file edits NOTHING in the projection chain. It reads participation, pbp,
# and nothing else. No file in R/29 through R/33 is sourced or modified.
#
# WINDOW: 2016-2025 (H1)
# ----------------------
# Settled empirically, not assumed. Participation switches source at 2023
# (NGS 2016-2022, FTN 2023+). The NGS era shows an 8.5 pct blank rate on
# offense_players and a 24 pct NA rate on offense_personnel, BUT conditional on
# play_type in pass/run both collapse to zero:
#
#   season  scrim_rows  blank_scrim  napers_scrim
#   2016    33790       1e-04        0
#   2017    33503       0            0
#   2018    33292       1e-04        0
#   2019    33513       0            0
#   2020    34238       0            0
#   2021    35766       0            0
#   2022    35430       0            0
#   2023-25 -           0            0
#
# Every blank and every NA-personnel row is non-scrimmage. The scrimmage filter
# already removes them. Ten seasons are usable.
#
# FILTERS (settled, D1)
# ---------------------
#   play_type in c("pass", "run")
#   qb_kneel == 0, qb_spike == 0
#   exactly 11 offensive player IDs on the play
#
# The 11-ID filter is not cosmetic. Three seasons carry a systematic recording
# issue, measured on the completed ten-season build:
#
#   season  pct of scrimmage plays lost to the 11-ID filter
#   2016    0.0641
#   2018    0.0589
#   2019    0.0233
#   all others   0.0000 to 0.0003
#
# 2016 is systematically missing one offensive player, 2018 systematically
# carries one extra. 2019 sits between the clean seasons and those two and was
# NOT predicted by the pre-build coverage check, which reported it at 0.9801
# and was read as noise. It is not noise. These three seasons lose materially
# more plays than the other seven. That differential exclusion is DISCLOSED,
# not corrected: .personnel_exclusion_report() emits per-season counts so any
# cross-season comparison can state it.
#
# JOIN COVERAGE IS ALSO ERA-ASYMMETRIC
# ------------------------------------
# Roughly 2,150 participation rows per season fail to match pbp at all through
# 2022, falling to 8 in 2023 and 0 in 2024-2025. About 21,000 NGS-era rows sit
# outside the join entirely. They are excluded by construction, but the NGS and
# FTN eras are therefore built on slightly different join populations.
#
# GARBAGE TIME IS NOT FILTERED AT BUILD TIME
# ------------------------------------------
# R/09's get_game_script_splits() drops garbage time (qtr == 4 and wp outside
# 0.10-0.90) because it contaminates efficiency reads. For a personnel study
# garbage time is subject matter, not noise: trailing teams in blowouts run
# empty and four-wide sets, which is exactly the extreme personnel usage a
# drafter faces when a player's team is bad. Excluding it at build time would
# silently remove the most informative tail. It is exposed as an ARGUMENT on
# every accessor instead (drop_garbage_time), so any result can be run both
# ways and the difference reported.
#
# For the same reason the panel does NOT require non-NA epa. R/09 requires it
# because it computes efficiency. Fantasy points exist on plays where epa is
# NA, and dropping those rows would remove data for a reason unrelated to the
# question being asked.
#
# GAME SCRIPT BANDING
# -------------------
# .band_game_script() is a deliberate lift of R/09 lines 207-213, same
# thresholds (leading wp > 0.60, trailing wp < 0.40). It is duplicated here
# rather than sourced so that this file has no dependency on the pipeline, but
# the thresholds are arguments defaulting to R/09's values. If R/09's defaults
# ever change, this file's defaults must be changed to match by hand. That is a
# stated maintenance cost, accepted in exchange for zero coupling.
#
# TWO-STAGE CACHE (J3)
# --------------------
# Stage 1: exploded play-player table, one row per offensive player per
#          scrimmage play. ~3.7M rows over ten seasons.
# Stage 2: aggregates derived from stage 1 by the accessors.
#
# The cache path is KEYED ON THE SEASON WINDOW. This is deliberate. R/29's
# .r29_panel_cache_path() reads from a fixed filename and hands the whole panel
# downstream regardless of the window requested, which is the open windowing
# defect in that file. This file does not repeat that pattern.
#
# SCORING
# -------
# These are SCRIMMAGE-PLAY PPR totals, not season PPR totals, and must never
# be described as season totals in prose. Points are credited only on plays
# that survive the scrimmage filter and on which the player appears in
# participation. Two-point conversions are excluded by construction. The gap
# is roughly one to three points per player-season and is uniform across
# personnel groups, so no comparative finding is threatened, but the absolute
# numbers will read low against any public leaderboard.
#
# Flat PPR, computed at play grain from pbp. Deliberately NOT DK_BEST_BALL_
# SCORING from R/17: this is a descriptive study of how points distribute
# across personnel groups, and a format with yardage bonuses would make the
# result format-specific. The scoring is exposed via ppr_reception so a 0.5 or
# 0 PPR variant can be run without editing the file.
#
# OUTPUT SURFACE (K2)
# -------------------
#   build_personnel_panel()          stage-1 builder
#   load_or_build_personnel_panel()  cached reader
#   personnel_team_rates()           A1
#   personnel_player_exposure()      B5
#   volume_to_points()               C3 half one (N3 log spec)
#   volume_persistence()             C3 half two (O2 demeaned lag-1)
#
# VERSION
# -------
#   v1.0  Initial build. Spec closed 2026-08-24. All forks settled before code:
#         A both grains, B5 exposure unit, C3 with N3 log spec, D1 scrimmage
#         only, E1 points attach to the play scored on, F1 floor + F3 disclosed
#         cell counts, G1 position-specific share, H1 ten seasons, I1 new file,
#         J3 two-stage window-keyed cache, K2 five-function surface, L2 raw wp
#         stored with banding on read, M1 total plays with M2 robustness,
#         N3 log-log with null at 1.0, O2 demeaned persistence.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(glue)
library(here)
library(nflreadr)


# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

# Participation data availability floor. Hard data fact, not a tunable.
PERSONNEL_MIN_SEASON <- 2016L

# Offensive players on a legal scrimmage snap.
PERSONNEL_N_OFFENSE <- 11L

# R/09 game-script thresholds, mirrored. See header note on the maintenance
# cost of this duplication.
PERSONNEL_LEADING_WP  <- 0.60
PERSONNEL_TRAILING_WP <- 0.40

# Minimum snaps in a personnel group before a player-group rate is reported
# (F1). Set from the observed distribution, not picked in advance: see
# .personnel_cell_report() which prints the snap-count quantiles the floor
# should be read off. The default below is a STARTING value and should be
# revisited against that report before any figure reaches prose.
PERSONNEL_MIN_CELL_SNAPS <- 25L

# Columns this file requires from pbp. Checked fail-loud rather than assumed.
.PERSONNEL_PBP_COLS <- c(
  "game_id", "play_id", "season", "week", "posteam", "play_type",
  "qb_kneel", "qb_spike", "wp", "qtr", "score_differential",
  "yards_gained", "complete_pass", "touchdown", "td_player_id",
  "passer_player_id", "rusher_player_id", "receiver_player_id",
  "passing_yards", "rushing_yards", "receiving_yards",
  "pass_touchdown", "rush_touchdown", "interception",
  "fumble_lost", "fumbled_1_player_id", "two_point_conv_result"
)


# ------------------------------------------------------------------------------
# INTERNAL HELPERS
# ------------------------------------------------------------------------------

# Cache path keyed on the season window. Two different windows never collide.
.personnel_cache_path <- function(seasons, cache_dir = NULL) {
  if (is.null(cache_dir)) cache_dir <- here::here("data", "season3_cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  tag <- glue("{min(seasons)}_{max(seasons)}")
  file.path(cache_dir, glue("s3_personnel_panel_{tag}.rds"))
}


# Normalise a personnel string to a compact group label.
#
# offense_personnel arrives as "1 RB, 1 TE, 3 WR" or, on non-scrimmage rows,
# as defensive junk ("1 FS, 2 LB, 1 OLB, 2 RB, 3 TE, 2 WR"). The scrimmage
# filter removes the junk before this is called. The conventional label is
# RB count followed by TE count: 1 RB + 1 TE = "11", 1 RB + 2 TE = "12".
# QB is assumed to be 1 and is not encoded, matching standard usage.
.personnel_group_label <- function(personnel_chr) {
  # perl = TRUE is required, not cosmetic. R's default regex engine is ERE,
  # which does not support the lazy quantifier .*? and silently treats it as
  # greedy. On 2024 strings greedy backtracking happens to land on the same
  # digit and the labels come out correct, but that is coincidence, not
  # construction. PCRE makes the pattern mean what it reads as.
  rb <- suppressWarnings(as.integer(sub(
    ".*?([0-9]+)\\s*RB.*", "\\1", personnel_chr, perl = TRUE)))
  te <- suppressWarnings(as.integer(sub(
    ".*?([0-9]+)\\s*TE.*", "\\1", personnel_chr, perl = TRUE)))

  # No RB or no TE token means zero of that back, not a parse failure.
  rb[!grepl("RB", personnel_chr, fixed = TRUE)] <- 0L
  te[!grepl("TE", personnel_chr, fixed = TRUE)] <- 0L

  out <- ifelse(is.na(rb) | is.na(te), NA_character_,
                paste0(rb, te))
  out
}


# R/09 lines 207-213, lifted. Garbage time returned as a FLAG, never filtered
# here; the caller decides.
.band_game_script <- function(wp, qtr,
                              leading_threshold  = PERSONNEL_LEADING_WP,
                              trailing_threshold = PERSONNEL_TRAILING_WP) {
  band <- dplyr::case_when(
    is.na(wp)               ~ NA_character_,
    wp > leading_threshold  ~ "leading",
    wp < trailing_threshold ~ "trailing",
    TRUE                    ~ "neutral"
  )
  garbage <- !is.na(wp) & !is.na(qtr) & qtr == 4 & (wp < 0.10 | wp > 0.90)
  list(band = band, garbage_time = garbage)
}


# Flat PPR at play grain (E1: points attach to the play they were scored on).
#
# Returns a long table of player_id, points, and the stat role that produced
# them. A single play can credit up to three players (passer, rusher or
# receiver, and a separate fumbler), so this is intentionally long.
.play_fantasy_points <- function(pbp, ppr_reception = 1.0) {

  # --- Passing ---
  passing <- pbp %>%
    dplyr::filter(!is.na(passer_player_id)) %>%
    dplyr::transmute(
      game_id, play_id,
      player_id = passer_player_id,
      stat_role = "passer",
      points =
        dplyr::coalesce(passing_yards, 0) * 0.04 +
        dplyr::coalesce(pass_touchdown, 0) * 4 +
        dplyr::coalesce(interception, 0) * -2
    )

  # --- Rushing ---
  rushing <- pbp %>%
    dplyr::filter(!is.na(rusher_player_id)) %>%
    dplyr::transmute(
      game_id, play_id,
      player_id = rusher_player_id,
      stat_role = "rusher",
      points =
        dplyr::coalesce(rushing_yards, 0) * 0.1 +
        dplyr::coalesce(rush_touchdown, 0) * 6
    )

  # --- Receiving ---
  # Reception credit is gated on complete_pass, not on non-NA receiving_yards:
  # a zero-yard catch is a reception and a completion to a receiver with NA
  # yards is not.
  receiving <- pbp %>%
    dplyr::filter(!is.na(receiver_player_id)) %>%
    dplyr::transmute(
      game_id, play_id,
      player_id = receiver_player_id,
      stat_role = "receiver",
      points =
        dplyr::coalesce(receiving_yards, 0) * 0.1 +
        dplyr::coalesce(complete_pass, 0) * ppr_reception +
        dplyr::if_else(
          dplyr::coalesce(pass_touchdown, 0) == 1 &
            !is.na(td_player_id) & td_player_id == receiver_player_id,
          6, 0
        )
    )

  # --- Lost fumbles ---
  fumbles <- pbp %>%
    dplyr::filter(
      dplyr::coalesce(fumble_lost, 0) == 1,
      !is.na(fumbled_1_player_id)
    ) %>%
    dplyr::transmute(
      game_id, play_id,
      player_id = fumbled_1_player_id,
      stat_role = "fumble",
      points    = -2
    )

  dplyr::bind_rows(passing, rushing, receiving, fumbles) %>%
    dplyr::filter(!is.na(player_id), player_id != "")
}


# Per-season report of what the scrimmage and 11-ID filters removed.
# Printed at build time so the differential exclusion in 2016, 2018 and 2019 is
# never silent.
.personnel_exclusion_report <- function(rep_tbl, verbose = TRUE) {
  if (!verbose) return(invisible(rep_tbl))
  message("\n--- personnel panel: per-season exclusions ---")
  message(paste(utils::capture.output(print(as.data.frame(rep_tbl),
                                            row.names = FALSE)),
                collapse = "\n"))
  message("--- 2016, 2018 and 2019 lose materially more plays to the ---")
  message("--- 11-ID filter than any other season. Disclose in any    ---")
  message("--- cross-season comparison.                               ---\n")
  invisible(rep_tbl)
}


# ------------------------------------------------------------------------------
# STAGE 1: BUILDER
# ------------------------------------------------------------------------------

#' Build the exploded personnel play-player panel.
#'
#' One row per offensive player per qualifying scrimmage play. This is the
#' expensive call: ten seasons is roughly 340,000 plays exploded to about
#' 3.7M rows. Everything downstream derives from it.
#'
#' @param seasons integer vector of seasons. Must be within
#'   PERSONNEL_MIN_SEASON and the latest completed season.
#' @param ppr_reception points per reception. 1.0 flat PPR by default.
#' @param save_output write the result to the window-keyed cache.
#' @param cache_dir override the cache directory.
#' @param verbose emit progress and the exclusion report.
#'
#' @return a tibble, one row per player per play, with attributes
#'   schema_tag, seasons, and exclusion_report.
build_personnel_panel <- function(seasons       = 2016:2025,
                                  ppr_reception = 1.0,
                                  save_output   = TRUE,
                                  cache_dir     = NULL,
                                  verbose       = TRUE) {

  # --- Input validation ---
  if (!is.numeric(seasons) || length(seasons) == 0) {
    stop("seasons must be a non-empty numeric vector.")
  }
  seasons <- sort(unique(as.integer(seasons)))
  if (min(seasons) < PERSONNEL_MIN_SEASON) {
    stop(glue(
      "Participation data begins in {PERSONNEL_MIN_SEASON}. ",
      "Requested {min(seasons)}."
    ))
  }
  if (!is.numeric(ppr_reception) || ppr_reception < 0) {
    stop("ppr_reception must be a non-negative number.")
  }

  season_panels <- vector("list", length(seasons))
  excl_rows     <- vector("list", length(seasons))

  for (i in seq_along(seasons)) {
    s <- seasons[i]
    if (verbose) message(glue("build_personnel_panel(): season {s} ..."))

    part <- as.data.frame(nflreadr::load_participation(seasons = s))
    pbp  <- as.data.frame(nflreadr::load_pbp(seasons = s))

    # --- Fail loud on missing pbp columns rather than silently coalescing ---
    missing_cols <- setdiff(.PERSONNEL_PBP_COLS, names(pbp))
    if (length(missing_cols) > 0) {
      stop(glue(
        "season {s}: pbp is missing required columns: ",
        "{paste(missing_cols, collapse = ', ')}. nflfastR schema may have ",
        "changed. Do not proceed on a partial join."
      ))
    }

    # --- Join participation to pbp on the verified key ---
    # nflverse_game_id matches pbp game_id exactly: verified 45,919 of 45,919
    # rows matched with zero unmatched in 2024.
    # offense_positions is NOT required. It is absent in the NGS era (verified:
    # 2016 load fails the required-column check on it) and is stored for
    # inspection only. No accessor reads it. personnel_group is derived from
    # offense_personnel, which IS populated on every scrimmage play in all ten
    # seasons. When the column is missing the panel carries NA for it rather
    # than losing the season.
    keep_part <- c("nflverse_game_id", "play_id", "possession_team",
                   "offense_personnel", "offense_players")
    missing_part <- setdiff(keep_part, names(part))
    if (length(missing_part) > 0) {
      stop(glue(
        "season {s}: participation is missing: ",
        "{paste(missing_part, collapse = ', ')}."
      ))
    }
    if ("offense_positions" %in% names(part)) {
      keep_part <- c(keep_part, "offense_positions")
    } else {
      part$offense_positions <- NA_character_
      keep_part <- c(keep_part, "offense_positions")
      if (verbose) {
        message(glue(
          "  season {s}: offense_positions absent, filled NA (not used ",
          "by any accessor)."
        ))
      }
    }

    j <- merge(
      part[, keep_part],
      pbp[, .PERSONNEL_PBP_COLS],
      by.x = c("nflverse_game_id", "play_id"),
      by.y = c("game_id", "play_id"),
      all.x = TRUE
    )
    names(j)[names(j) == "nflverse_game_id"] <- "game_id"

    n_part      <- nrow(j)
    n_unmatched <- sum(is.na(j$play_type))

    # --- D1 scrimmage filter ---
    scrim <- !is.na(j$play_type) &
      j$play_type %in% c("pass", "run") &
      dplyr::coalesce(j$qb_kneel, 0) == 0 &
      dplyr::coalesce(j$qb_spike, 0) == 0
    j <- j[scrim, , drop = FALSE]
    n_scrim <- nrow(j)

    # --- 11-ID filter ---
    op    <- j$offense_players
    blank <- is.na(op) | trimws(op) == ""
    n_ids <- rep(NA_integer_, length(op))
    n_ids[!blank] <- lengths(strsplit(op[!blank], ";", fixed = TRUE))

    keep11 <- !blank & !is.na(n_ids) & n_ids == PERSONNEL_N_OFFENSE
    n_drop_ids <- sum(!keep11)
    j <- j[keep11, , drop = FALSE]

    excl_rows[[i]] <- data.frame(
      season         = s,
      part_rows      = n_part,
      unmatched_pbp  = n_unmatched,
      scrimmage      = n_scrim,
      dropped_not_11 = n_drop_ids,
      kept           = nrow(j),
      pct_lost_11    = round(n_drop_ids / max(n_scrim, 1), 4),
      stringsAsFactors = FALSE
    )

    if (nrow(j) == 0) {
      warning(glue("season {s}: zero plays survived filtering."))
      next
    }

    # --- Personnel label and game-script banding (L2: stored raw too) ---
    j$personnel_group <- .personnel_group_label(j$offense_personnel)
    bands <- .band_game_script(j$wp, j$qtr)
    j$game_script  <- bands$band
    j$garbage_time <- bands$garbage_time

    play_meta <- tibble::as_tibble(j) %>%
      dplyr::select(
        game_id, play_id, season, week,
        posteam, play_type, personnel_group,
        offense_personnel, offense_positions,
        wp, qtr, score_differential,
        game_script, garbage_time
      )

    # --- Explode to player grain ---
    exploded <- tibble::as_tibble(j) %>%
      dplyr::select(game_id, play_id, offense_players) %>%
      dplyr::mutate(
        player_id = strsplit(offense_players, ";", fixed = TRUE)
      ) %>%
      dplyr::select(-offense_players) %>%
      tidyr::unnest(player_id) %>%
      dplyr::mutate(player_id = trimws(player_id)) %>%
      dplyr::filter(player_id != "")

    # --- Attach points (E1) ---
    pts <- .play_fantasy_points(
      tibble::as_tibble(j), ppr_reception = ppr_reception
    ) %>%
      dplyr::group_by(game_id, play_id, player_id) %>%
      dplyr::summarise(
        points    = sum(points, na.rm = TRUE),
        stat_role = paste(sort(unique(stat_role)), collapse = "+"),
        .groups   = "drop"
      )

    season_panels[[i]] <- exploded %>%
      dplyr::left_join(play_meta, by = c("game_id", "play_id")) %>%
      dplyr::left_join(pts, by = c("game_id", "play_id", "player_id")) %>%
      dplyr::mutate(
        points  = dplyr::coalesce(points, 0),
        touched = !is.na(stat_role)
      )

    if (verbose) {
      message(glue(
        "  season {s}: {nrow(j)} plays -> {nrow(season_panels[[i]])} ",
        "player-play rows"
      ))
    }
  }

  panel <- dplyr::bind_rows(season_panels)
  excl  <- dplyr::bind_rows(excl_rows)

  if (nrow(panel) == 0) {
    stop("build_personnel_panel(): no rows produced. Check the season window.")
  }

  attr(panel, "schema_tag")       <- "s3_personnel_panel_v1"
  attr(panel, "seasons")          <- seasons
  attr(panel, "ppr_reception")    <- ppr_reception
  attr(panel, "exclusion_report") <- excl

  .personnel_exclusion_report(excl, verbose = verbose)

  if (verbose) {
    message(glue(
      "build_personnel_panel(): {nrow(panel)} player-play rows | ",
      "{length(unique(panel$player_id))} players | ",
      "{length(seasons)} seasons"
    ))
  }

  if (isTRUE(save_output)) {
    out_path <- .personnel_cache_path(seasons, cache_dir)
    saveRDS(panel, out_path)
    if (verbose) message(glue("build_personnel_panel(): cached -> {out_path}"))
  }

  panel
}


#' Cached reader for the personnel panel.
#'
#' Reads the window-keyed cache if it exists and its stored season attribute
#' matches the request exactly. Rebuilds otherwise. The equality check on the
#' seasons attribute is the point: a cache built for a different window is a
#' cache miss, not a usable file.
load_or_build_personnel_panel <- function(seasons   = 2016:2025,
                                          force     = FALSE,
                                          cache_dir = NULL,
                                          verbose   = TRUE,
                                          ...) {
  seasons  <- sort(unique(as.integer(seasons)))
  out_path <- .personnel_cache_path(seasons, cache_dir)

  if (!isTRUE(force) && file.exists(out_path)) {
    panel <- readRDS(out_path)
    cached_seasons <- attr(panel, "seasons")
    if (!is.null(cached_seasons) && identical(
      sort(as.integer(cached_seasons)), seasons)) {
      if (verbose) {
        message(glue(
          "load_or_build_personnel_panel(): cache hit -> {out_path} ",
          "({nrow(panel)} rows)"
        ))
      }
      return(panel)
    }
    if (verbose) {
      message(
        "load_or_build_personnel_panel(): cache window mismatch, rebuilding."
      )
    }
  }

  build_personnel_panel(
    seasons = seasons, save_output = TRUE,
    cache_dir = cache_dir, verbose = verbose, ...
  )
}


# ------------------------------------------------------------------------------
# STAGE 2: ACCESSORS
# ------------------------------------------------------------------------------

# Shared scope filter for every accessor. Garbage time is a parameter here and
# nowhere else.
.personnel_scope <- function(panel, drop_garbage_time = FALSE,
                             game_script = NULL) {
  if (isTRUE(drop_garbage_time)) {
    panel <- dplyr::filter(panel, !garbage_time)
  }
  if (!is.null(game_script)) {
    panel <- dplyr::filter(panel, .data$game_script %in% !!game_script)
  }
  panel
}


#' A1: team-grain personnel usage and points per snap by group.
#'
#' Snap counts here are play counts, deduplicated to the play level, so a
#' single play contributes one snap to its team and not eleven.
personnel_team_rates <- function(panel,
                                 drop_garbage_time = FALSE,
                                 game_script       = NULL) {

  scoped <- .personnel_scope(panel, drop_garbage_time, game_script)

  play_level <- scoped %>%
    dplyr::group_by(season, posteam, game_id, play_id, personnel_group) %>%
    dplyr::summarise(play_points = sum(points, na.rm = TRUE),
                     .groups = "drop")

  team_totals <- play_level %>%
    dplyr::group_by(season, posteam) %>%
    dplyr::summarise(team_plays = dplyr::n(), .groups = "drop")

  play_level %>%
    dplyr::group_by(season, posteam, personnel_group) %>%
    dplyr::summarise(
      snaps            = dplyr::n(),
      points           = sum(play_points, na.rm = TRUE),
      points_per_snap  = points / dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::left_join(team_totals, by = c("season", "posteam")) %>%
    dplyr::mutate(usage_rate = snaps / team_plays) %>%
    dplyr::arrange(season, posteam, dplyr::desc(snaps))
}


#' B5: per-player snap and point exposure by personnel group.
#'
#' F1 applies the snap floor. F3 is satisfied by returning the raw snap count
#' on every row so any table can disclose its cell sizes.
personnel_player_exposure <- function(panel,
                                      min_cell_snaps    = PERSONNEL_MIN_CELL_SNAPS,
                                      drop_garbage_time = FALSE,
                                      game_script       = NULL) {

  scoped <- .personnel_scope(panel, drop_garbage_time, game_script)

  by_group <- scoped %>%
    dplyr::group_by(season, player_id, posteam, personnel_group) %>%
    dplyr::summarise(
      snaps   = dplyr::n(),
      touches = sum(touched, na.rm = TRUE),
      points  = sum(points, na.rm = TRUE),
      .groups = "drop"
    )

  player_totals <- by_group %>%
    dplyr::group_by(season, player_id) %>%
    dplyr::summarise(
      total_snaps  = sum(snaps),
      total_points = sum(points),
      .groups = "drop"
    )

  by_group %>%
    dplyr::left_join(player_totals, by = c("season", "player_id")) %>%
    dplyr::mutate(
      snap_share_in_group = snaps / total_snaps,
      points_per_snap     = points / snaps,
      below_floor         = snaps < min_cell_snaps
    ) %>%
    dplyr::arrange(season, player_id, dplyr::desc(snaps))
}


#' Build the opportunity-share table that volume_to_points() requires.
#'
#' G1: share is position-specific. Target share for WR and TE, carries plus
#' targets share for RB, pass attempts share for QB.
#'
#' DERIVED FROM THE PANEL, NOT FROM PBP. This is the point of the function.
#' .play_fantasy_points() already recorded stat_role at build time, so a row
#' whose role includes "receiver" was targeted and one whose role includes
#' "rusher" carried. Reading share off the panel guarantees that share and the
#' team-volume term in volume_to_points() sit on the same filtered population.
#' Computing share from raw pbp instead would put the numerator and denominator
#' on different populations, and under the N3 log specification that mismatch
#' would push the coefficient away from 1.0 for a purely mechanical reason,
#' which is the exact artifact the specification exists to avoid.
#'
#' Denominator is team targets and team carries, not team pass attempts. Sacks
#' and throwaways are attempts with no target and would inflate the denominator
#' without giving any receiver an opportunity.
#'
#' Positions come from nflreadr::load_players() on gsis_id. Players with no
#' position match are returned with position NA and are dropped by
#' volume_to_points() when it filters to the requested positions.
#'
#' @param panel the stage-1 panel from build_personnel_panel().
#' @param drop_garbage_time must match the value passed to volume_to_points().
#'   Mismatched scoping silently changes the denominator.
#' @param verbose emit a coverage summary.
#'
#' @return tibble with season, player_id, position, targets, carries,
#'   opp_share, and the team denominators that produced it.
build_opportunity_shares <- function(panel,
                                     drop_garbage_time = FALSE,
                                     verbose           = TRUE) {

  if (!"stat_role" %in% names(panel)) {
    stop(
      "panel is missing stat_role. build_opportunity_shares() requires a ",
      "panel from build_personnel_panel() v1, not an aggregate."
    )
  }

  scoped <- .personnel_scope(panel, drop_garbage_time)

  # A play credits at most one target and one carry, so role membership is
  # counted per player-play rather than summed across roles.
  opp <- scoped %>%
    dplyr::filter(!is.na(stat_role)) %>%
    dplyr::mutate(
      is_target = grepl("receiver", stat_role, fixed = TRUE),
      is_carry  = grepl("rusher",   stat_role, fixed = TRUE),
      is_att    = grepl("passer",   stat_role, fixed = TRUE)
    ) %>%
    dplyr::group_by(season, player_id, posteam) %>%
    dplyr::summarise(
      targets = sum(is_target),
      carries = sum(is_carry),
      attempts = sum(is_att),
      .groups = "drop"
    ) %>%
    dplyr::filter(targets + carries + attempts > 0)

  # Team denominators on the identical population.
  team_opp <- opp %>%
    dplyr::group_by(season, posteam) %>%
    dplyr::summarise(
      team_targets  = sum(targets),
      team_carries  = sum(carries),
      team_attempts = sum(attempts),
      .groups = "drop"
    )

  players <- nflreadr::load_players()
  pos_lookup <- data.frame(
    player_id = players$gsis_id,
    position  = players$position,
    display_name = players$display_name,
    stringsAsFactors = FALSE
  )
  pos_lookup <- pos_lookup[!is.na(pos_lookup$player_id), ]
  pos_lookup <- pos_lookup[!duplicated(pos_lookup$player_id), ]

  out <- opp %>%
    dplyr::left_join(team_opp, by = c("season", "posteam")) %>%
    dplyr::left_join(pos_lookup, by = "player_id") %>%
    dplyr::mutate(
      opp_share = dplyr::case_when(
        position %in% c("WR", "TE") & team_targets > 0 ~
          targets / team_targets,
        position == "RB" & (team_targets + team_carries) > 0 ~
          (targets + carries) / (team_targets + team_carries),
        position == "QB" & team_attempts > 0 ~
          attempts / team_attempts,
        TRUE ~ NA_real_
      )
    )

  if (verbose) {
    cov <- out %>%
      dplyr::group_by(position) %>%
      dplyr::summarise(
        n          = dplyr::n(),
        with_share = sum(!is.na(opp_share)),
        .groups    = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(n))
    message(glue(
      "build_opportunity_shares(): {nrow(out)} player-seasons | ",
      "{sum(is.na(out$position))} with no position match"
    ))
    message(paste(utils::capture.output(
      print(as.data.frame(utils::head(cov, 10)), row.names = FALSE)),
      collapse = "\n"))
  }

  out
}


#' C3 half one: does team play volume convert into player points?
#'
#' N3 log-log specification. THE NULL HERE IS 1.0, NOT ZERO.
#'
#' Player points are approximately share x team plays x efficiency. In logs
#' that is additive, so under pure arithmetic the coefficient on
#' log(team_plays_per_game) is exactly 1.0: a 10 pct larger offense produces
#' 10 pct more points at fixed share, and nothing has been learned. The finding
#' is the DEPARTURE from 1.0.
#'
#'   coefficient < 1.0   high-volume offenses dilute. Extra plays are spread
#'                       across more mouths than share alone captures.
#'   coefficient = 1.0   pure proportionality. Volume is arithmetic.
#'   coefficient > 1.0   high-volume offenses compound. Extra plays are worth
#'                       more than their count.
#'
#' G1: share is position-specific and positions are NOT pooled. The caller
#' supplies a share table because target and carry shares come from the pbp
#' stat lines, not from participation, and this file does not own that
#' computation.
#'
#' @param panel the stage-1 panel.
#' @param share_tbl a tibble with season, player_id, position, opp_share.
#'   opp_share is target share for WR and TE, and carries-plus-targets share
#'   for RB. Built by the caller.
#' @param positions which positions to model, run separately.
#' @param min_games games played floor.
#' @param neutral_only M2 robustness: restrict volume to neutral-script plays.
volume_to_points <- function(panel,
                             share_tbl,
                             positions         = c("WR", "RB"),
                             min_games         = 8L,
                             drop_garbage_time = FALSE,
                             neutral_only      = FALSE) {

  req <- c("season", "player_id", "position", "opp_share")
  missing_share <- setdiff(req, names(share_tbl))
  if (length(missing_share) > 0) {
    stop(glue(
      "share_tbl is missing: {paste(missing_share, collapse = ', ')}"
    ))
  }

  scoped <- .personnel_scope(panel, drop_garbage_time)

  # M1 primary: all scrimmage plays. M2: neutral band only.
  vol_source <- if (isTRUE(neutral_only)) {
    dplyr::filter(scoped, game_script == "neutral")
  } else {
    scoped
  }

  team_volume <- vol_source %>%
    dplyr::distinct(season, posteam, game_id, play_id) %>%
    dplyr::group_by(season, posteam) %>%
    dplyr::summarise(
      team_plays = dplyr::n(),
      team_games = dplyr::n_distinct(game_id),
      .groups    = "drop"
    ) %>%
    dplyr::mutate(team_plays_pg = team_plays / team_games)

  player_season <- scoped %>%
    dplyr::group_by(season, player_id, posteam) %>%
    dplyr::summarise(
      games  = dplyr::n_distinct(game_id),
      points = sum(points, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(games >= min_games, points > 0)

  model_df <- player_season %>%
    dplyr::left_join(team_volume, by = c("season", "posteam")) %>%
    dplyr::left_join(share_tbl, by = c("season", "player_id")) %>%
    dplyr::filter(
      !is.na(opp_share), opp_share > 0,
      !is.na(team_plays_pg), team_plays_pg > 0
    ) %>%
    dplyr::mutate(
      ppg          = points / games,
      log_ppg      = log(ppg),
      log_plays_pg = log(team_plays_pg),
      log_share    = log(opp_share)
    ) %>%
    dplyr::filter(is.finite(log_ppg))

  fits <- lapply(positions, function(pos) {
    d <- dplyr::filter(model_df, position == pos)
    if (nrow(d) < 30) {
      warning(glue("{pos}: only {nrow(d)} rows, model not fit."))
      return(NULL)
    }
    fit <- stats::lm(log_ppg ~ log_plays_pg + log_share + factor(season),
                     data = d)
    ci  <- stats::confint(fit, "log_plays_pg")
    list(
      position     = pos,
      n            = nrow(d),
      coef_plays   = unname(stats::coef(fit)["log_plays_pg"]),
      ci_low       = ci[1],
      ci_high      = ci[2],
      excludes_one = !(ci[1] <= 1 & ci[2] >= 1),
      fit          = fit
    )
  })
  names(fits) <- positions

  summary_tbl <- do.call(rbind, lapply(fits[!vapply(fits, is.null, TRUE)],
    function(f) data.frame(
      position     = f$position,
      n            = f$n,
      coef_plays   = round(f$coef_plays, 4),
      ci_low       = round(f$ci_low, 4),
      ci_high      = round(f$ci_high, 4),
      excludes_one = f$excludes_one,
      stringsAsFactors = FALSE
    )))

  list(data = model_df, fits = fits, summary = summary_tbl)
}


#' C3 half two: is team play volume predictable one season ahead?
#'
#' O2. The season mean is removed before correlating. League-wide pace drifted
#' over 2016-2025, and a raw lag-1 correlation would credit that drift to team
#' persistence. Demeaning by season isolates the team-level component, which is
#' the only part a drafter could act on in advance.
volume_persistence <- function(panel, drop_garbage_time = FALSE) {

  scoped <- .personnel_scope(panel, drop_garbage_time)

  team_season <- scoped %>%
    dplyr::distinct(season, posteam, game_id, play_id) %>%
    dplyr::group_by(season, posteam) %>%
    dplyr::summarise(
      team_plays = dplyr::n(),
      team_games = dplyr::n_distinct(game_id),
      .groups    = "drop"
    ) %>%
    dplyr::mutate(plays_pg = team_plays / team_games) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(plays_pg_dm = plays_pg - mean(plays_pg, na.rm = TRUE)) %>%
    dplyr::ungroup()

  lagged <- team_season %>%
    dplyr::arrange(posteam, season) %>%
    dplyr::group_by(posteam) %>%
    dplyr::mutate(
      prior_plays_pg    = dplyr::lag(plays_pg),
      prior_plays_pg_dm = dplyr::lag(plays_pg_dm),
      prior_season      = dplyr::lag(season)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(prior_plays_pg), prior_season == season - 1)

  r_raw <- stats::cor(lagged$plays_pg, lagged$prior_plays_pg,
                      use = "complete.obs")
  r_dm  <- stats::cor(lagged$plays_pg_dm, lagged$prior_plays_pg_dm,
                      use = "complete.obs")

  by_season <- lagged %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(
      n = dplyr::n(),
      r_demeaned = stats::cor(plays_pg_dm, prior_plays_pg_dm,
                              use = "complete.obs"),
      .groups = "drop"
    )

  list(
    team_season   = team_season,
    pairs         = lagged,
    r_raw         = r_raw,
    r_demeaned    = r_dm,
    r_by_season   = by_season,
    n_pairs       = nrow(lagged)
  )
}


#' Diagnostic: snap-count distribution for setting the F1 floor.
#'
#' PERSONNEL_MIN_CELL_SNAPS is a starting value. Run this and read the floor
#' off the observed distribution before any figure from
#' personnel_player_exposure() reaches prose.
.personnel_cell_report <- function(panel, drop_garbage_time = FALSE) {
  scoped <- .personnel_scope(panel, drop_garbage_time)
  cells <- scoped %>%
    dplyr::group_by(season, player_id, personnel_group) %>%
    dplyr::summarise(snaps = dplyr::n(), .groups = "drop")

  list(
    n_cells   = nrow(cells),
    quantiles = stats::quantile(cells$snaps,
                                probs = c(0.05, 0.10, 0.25, 0.50,
                                          0.75, 0.90, 0.95)),
    by_group  = cells %>%
      dplyr::group_by(personnel_group) %>%
      dplyr::summarise(
        n_cells      = dplyr::n(),
        median_snaps = stats::median(snaps),
        .groups      = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(n_cells))
  )
}


# ==============================================================================
# END 54_personnel_panel.R
# ==============================================================================
