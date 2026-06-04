# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 16
# Lineup + Waiver Optimizer
# File: R/35_lineup_optimizer.R
#
# PURPOSE
# -------
# The decision layer that sits on top of the entire projection stack. Takes the
# reconciled offensive projections (R/32), cross-position VORP (R/33), and the
# DEF/ST projection (R/34), applies an opponent-vs-position matchup adjustment,
# and answers the two questions a manager actually asks each week:
#
#   1. optimize_lineup()      -- given my roster, what is my best legal starting
#                                lineup this week? (full-lineup integer program)
#   2. suggest_waiver_adds()  -- among players nobody rosters, which most improve
#                                my team, and who would I drop?
#
# DESIGN DECISIONS (locked with Christian, Week 16)
# -------------------------------------------------
#   - Solver           : integer programming via lpSolve. The lineup is a true
#                        full-lineup optimization (every slot decided jointly),
#                        not a position-by-position sort. FLEX and SUPER_FLEX
#                        make the slot choices interdependent, which a greedy
#                        sort gets wrong often enough to lose a close week.
#   - Objective        : maximize matchup-adjusted projected points. Projection
#                        intervals are used only to FLAG confidence, never inside
#                        the objective.
#   - Matchup layer    : opponent-vs-position is a first-class input, same depth
#                        as the offensive projection itself. R/34 is the
#                        context-neutral DEF base; the opponent-offense matchup
#                        is applied here, alongside defense-vs-position (DvP) for
#                        offensive players. DvP is computed scoring-neutral and
#                        NORMALIZED, so league scoring quirks cancel out and the
#                        factor is a pure positional-matchup signal.
#   - Confidence flag  : offense uses the R/32 80% interval overlap (the Felton
#                        vs Chase pattern -- non-overlapping = confident,
#                        overlapping = close). DEF has no R/34 interval, so the
#                        DEF slot uses a fixed PPG-gap threshold against the
#                        next-best available defense instead.
#   - Format-aware     : redraft vs dynasty. The WEEKLY lineup is format-agnostic
#                        (start your best healthy player regardless of format).
#                        Format changes ROSTER decisions: redraft value is
#                        rest-of-season points, availability-gated, so an
#                        out-for-season player collapses to droppable; dynasty
#                        value blends rest-of-season with a forward score (talent
#                        + age trajectory + prospect pedigree), so an injured
#                        elite young player stays protected. Dynasty is
#                        auto-detected from the Sleeper TAXI slot, overridable.
#   - roll3 form       : optional recency nudge toward the rolling 3-game average
#                        (R/18). OFF by default (roll3_weight = 0); when > 0 it
#                        blends with, never replaces, the R/32 projection.
#   - Availability     : inferred from Sleeper. status == "Injured Reserve" and
#                        the roster IR/reserve flag are the out-for-season proxy;
#                        injury_status ("Out") is out-for-this-week. Neither feed
#                        has a true season-ending field, so this is an inference,
#                        documented as such. An optional manual override CSV gets
#                        the final word.
#   - Connector        : platform-agnostic. Sleeper now via R/19/R/33; ESPN and
#                        Yahoo are explicit stubs.
#
# OUTPUTS (only when save_output = TRUE)
# --------------------------------------
#   data/season2_cache/s2_week16_optimal_lineup.rds / .csv
#
# SOURCE DEPENDENCIES (sourced below if not already loaded)
# ---------------------------------------------------------
#   R/05_consistency_metrics.R -- calculate_fantasy_points()  (DvP scoring)
#   R/15_multi_season_pbp.R    -- load_normalized_season()
#   R/19_sleeper_api.R         -- connect_sleeper_league(), get_sleeper_rosters(),
#                                 get_all_sleeper_players(), match_sleeper_players()
#   R/29_projection_engine.R   -- calculate_def_st_points(), DEF scoring constants
#   R/33_vorp_rankings.R       -- build_config_from_sleeper(), compute_vorp_rankings()
#                                 (also sources R/19 and R/32)
#
# Consumed as data (passed in by the caller, not sourced):
#   R/32 reconciled projections (s2_week15_reconciled_projections.rds)
#   R/34 DEF projections        (s2_week16_def_st_projections.rds)
#   R/23 aging curves           (optional, for the dynasty trajectory)
#   R/28 prospect scores        (optional, for the dynasty pedigree bump)
#
# RUN
# ---
#   source(here::here("R", "35_lineup_optimizer.R"))
#   src       <- build_league_source(league_id = "123456789")
#   reconciled <- readRDS(here::here("data","season2_cache",
#                                    "s2_week15_reconciled_projections.rds"))
#   def_proj   <- readRDS(here::here("data","season2_cache",
#                                    "s2_week16_def_st_projections.rds"))
#   vorp       <- compute_vorp_rankings(reconciled, src$config)
#
#   # preseason / no opponent -> matchup factors default to neutral
#   lineup <- optimize_lineup(src, reconciled, vorp, def_proj)
#
#   # in-season week 6 with matchup adjustment
#   dvp     <- compute_dvp_factors(seasons = 2025L)
#   defmtch <- compute_def_matchup_factors(seasons = 2025L)
#   lineup  <- optimize_lineup(src, reconciled, vorp, def_proj, week = 6L,
#                              dvp = dvp, def_factors = defmtch)
#
#   adds <- suggest_waiver_adds(src, reconciled, vorp, def_proj)
#
# SCHEMA TAG: s2_w16_lineup_v1
# Author: Christian K. LeBlanc
# Version: 1.0
# ==============================================================================

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(here)
library(glue)
library(lpSolve)
library(nflreadr)

# Null-coalescing infix (defined in R/19; redefine defensively so this file is
# self-contained if R/19 has not been sourced yet).
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Source dependencies only if their entry points are not already in the session.
if (!exists("calculate_fantasy_points")) {
  source(here::here("R", "05_consistency_metrics.R"))
}
if (!exists("load_normalized_season")) {
  source(here::here("R", "15_multi_season_pbp.R"))
}
if (!exists("calculate_def_st_points")) {
  source(here::here("R", "29_projection_engine.R"))
}
# R/33 sources R/19 (Sleeper) and R/32 (reconciliation) as a side effect.
if (!exists("build_config_from_sleeper")) {
  source(here::here("R", "33_vorp_rankings.R"))
}
if (!exists("get_all_sleeper_players")) {
  source(here::here("R", "19_sleeper_api.R"))
}

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

SEASON <- 2026L
SCHEMA_TAG_LINEUP <- "s2_w16_lineup_v1"

# Offensive positions we project (DEF handled as a pseudo-player).
LINEUP_OFFENSE_POSITIONS <- c("QB", "RB", "WR", "TE")

# Slot eligibility: which lineup slots each position may legally fill.
SLOT_ELIGIBILITY <- list(
  QB  = c("QB", "SUPER_FLEX"),
  RB  = c("RB", "FLEX", "SUPER_FLEX"),
  WR  = c("WR", "FLEX", "SUPER_FLEX"),
  TE  = c("TE", "FLEX", "SUPER_FLEX"),
  DEF = c("DEF")
)

# DvP / DEF-matchup factor cap. Factors are clamped to [1 - cap, 1 + cap] so a
# matchup tilts a projection without ever swamping it.
DVP_CAP <- 0.25

# Fixed PPG gap used for the DEF slot confidence flag (offense uses intervals).
DEF_GAP_THRESHOLD <- 1.5

# Dynasty forward-score weight: how much the forward-looking score counts versus
# rest-of-season value when ranking dynasty roster/drop decisions.
DYNASTY_FUTURE_WEIGHT_DEFAULT <- 0.5

# Forward-adjusted VORP (cross-position dynasty stash value). Additive on top of
# R/33 VORP so below-replacement free agents are still lifted by upside:
#   forward_vorp = adjusted_vorp + (aging_mult - 1) * AGE_VORP_SCALE
#                                + (score_final / 100) * PROSPECT_VORP_SCALE
# AGE_VORP_SCALE converts the aging multiplier's deviation from 1 into VORP
# points; PROSPECT_VORP_SCALE is the max pedigree bonus (score_final = 100).
AGE_VORP_SCALE      <- 12
PROSPECT_VORP_SCALE <- 6

# Internal positional peak-age fallback (used only when no R/23 curve supplied).
# Slope and bounds shape a simple trajectory multiplier around the peak.
PEAK_AGE_FALLBACK   <- c(QB = 29L, RB = 25L, WR = 27L, TE = 27L)
AGING_SLOPE         <- 0.05
AGING_MULT_BOUNDS   <- c(0.60, 1.40)

# Sleeper status / injury values mapped to availability.
OUT_FOR_SEASON_STATUSES <- c("Injured Reserve", "IR",
                             "Physically Unable to Perform", "PUP",
                             "Non Football Injury", "NFI")
OUT_THIS_WEEK_INJURY    <- c("Out", "IR", "Doubtful")

# Standard Sleeper DEF scoring fallback (used only if R/29's constants are not
# in scope). DvP/DEF factors are normalized, so exact values cancel; these only
# need to be internally consistent.
DEF_PTS_ALLOW_TIERS_FALLBACK <- c(
  pts_allow_0 = 10, pts_allow_1_6 = 7, pts_allow_7_13 = 4, pts_allow_14_20 = 1,
  pts_allow_21_27 = 0, pts_allow_28_34 = -1, pts_allow_35p = -4
)
DEF_EVENT_POINTS_FALLBACK <- c(
  sack = 1, def_int = 2, fum_rec = 2, def_td = 6, safety = 2, blk_kick = 2
)

CACHE_DIR_DEFAULT <- here::here("data", "season2_cache")
OUTPUT_RDS_PATH_LINEUP <- here::here("data", "season2_cache",
                                     "s2_week16_optimal_lineup.rds")
OUTPUT_CSV_PATH_LINEUP <- here::here("data", "season2_cache",
                                     "s2_week16_optimal_lineup.csv")

# ------------------------------------------------------------------------------
# NSE DECLARATIONS
# ------------------------------------------------------------------------------

utils::globalVariables(c(
  ".data", "home_team", "away_team", "season_type", "total_fantasy_points",
  "opponent_def", "fp_allowed", "dvp_allowed_ppg", "dvp_factor",
  "def_st_points", "opponent_pts_allowed", "sacks", "def_ints", "fum_recs",
  "def_tds", "safeties", "blocked_kicks", "conceded_ppg", "def_matchup_factor",
  "offense_team", "def_team", "base_proj", "adj_proj", "matchup_factor",
  "r32_posterior_mu", "r32_projection_lower_80", "r32_projection_upper_80",
  "boom_probability", "bust_probability", "adjusted_vorp", "nfl_gsis_id",
  "def_proj_ppg", "is_reserve", "status", "injury_status", "is_free_agent",
  "availability_status", "on_user_roster", "is_available", "ros_value",
  "forward_score", "value", "fantasy_pts_roll3", "score_final", "age",
  "lower_80", "upper_80", "slot", "confidence_flag", "player_id",
  "sleeper_player_id", "roster_id", "match_method", "match_confidence",
  "gsis_id", "full_name", "sp_name", "sp_pos", "name_key", "pool_key",
  "dynasty_value", "value_gain", "forward_score", "forward_vorp"
))

# ==============================================================================
# SECTION 1: CONNECTOR (platform-agnostic league source)
# ==============================================================================

# ------------------------------------------------------------------------------
# .resolve_roster_gsis
# ------------------------------------------------------------------------------

#' Attach gsis_id and Sleeper status to a raw roster table
#'
#' Recovers nfl_gsis_id by NAME via match_sleeper_players() against the season's
#' nflfastR roster, NOT off Sleeper's raw gsis field. Sleeper only tags roughly
#' a third of its universe with a gsis (older players), so a plain join on that
#' field silently drops every newer player. The 3-stage matcher (gsis, exact
#' name + position, fuzzy name) recovers them. Status, injury_status, and age
#' still come from the Sleeper universe. DEF entries are team codes, not numeric
#' ids, so they are excluded from name matching and handled separately by team.
#'
#' @param rosters Tibble from get_sleeper_rosters().
#' @param players Tibble from get_all_sleeper_players().
#' @param match_season Integer. Season whose nflfastR roster anchors the name
#'   match (use the league's own season; gsis is stable across seasons).
#' @return rosters with nfl_gsis_id, match_method, match_confidence, status,
#'   injury_status, age added.
#' @keywords internal
.resolve_roster_gsis <- function(rosters, players, match_season) {

  # Status / injury / age come straight from the Sleeper universe.
  status_lk <- players %>%
    dplyr::select(sleeper_player_id, status, injury_status, age)

  # gsis recovered by name. Restrict to numeric Sleeper ids (DEF ids are team
  # abbreviations like "NE" and never name-match).
  numeric_ids <- rosters %>%
    dplyr::filter(grepl("^[0-9]+$", .data$player_id)) %>%
    dplyr::pull(.data$player_id) %>%
    unique()

  # Span the league season and the prior one, so both returning veterans and
  # players absent from a thin early-offseason 2026 snapshot are covered. Fall
  # back to the single season if the wider pull fails.
  roster_seasons <- sort(unique(c(match_season - 1L, match_season)))
  nfl_roster <- tryCatch(
    nflreadr::load_rosters(seasons = roster_seasons),
    error = function(e) nflreadr::load_rosters(seasons = match_season)
  )

  # min_match_rate = 0 so an unmatched IDP / practice-squad id cannot abort the
  # run; unmatched players simply carry NA gsis and drop downstream with a count.
  mapping <- match_sleeper_players(numeric_ids, nfl_roster,
                                   min_match_rate = 0) %>%
    dplyr::select(sleeper_player_id,
                  nfl_gsis_id = "gsis_id",
                  match_method, match_confidence)

  rosters %>%
    dplyr::left_join(mapping, by = c("player_id" = "sleeper_player_id")) %>%
    dplyr::left_join(status_lk, by = c("player_id" = "sleeper_player_id"))
}

# ------------------------------------------------------------------------------
# build_league_source
# ------------------------------------------------------------------------------

#' Build a platform-agnostic league source for the optimizer
#'
#' Bundles everything the optimizer needs: the league_config (slot counts,
#' format, weights from R/33), every team's rostered players resolved to
#' gsis_id, the user's own roster, the full Sleeper player universe (for
#' availability and the waiver pool), and the redraft/dynasty format.
#'
#' Dynasty is auto-detected from a TAXI roster slot (taxi squads exist only in
#' dynasty/keeper leagues). Override with the format argument.
#'
#' @param league_id Character. Sleeper league id (required for platform sleeper).
#' @param user_roster_id Integer or NULL. The roster_id that is "yours". If
#'   NULL, the user's roster must be supplied later; lineup calls will error
#'   without it. Use owner-based resolution upstream if preferred.
#' @param season Integer. NFL season (default SEASON).
#' @param platform Character. "sleeper" (others stubbed).
#' @param format Character or NULL. "redraft" / "dynasty"; NULL auto-detects.
#' @param sleeper_players Tibble or NULL. Pre-fetched get_all_sleeper_players()
#'   to avoid a repeat ~10MB pull; NULL fetches it.
#' @return A league_source list.
#' @export
build_league_source <- function(league_id = NULL,
                                 user_roster_id = NULL,
                                 season = SEASON,
                                 platform = "sleeper",
                                 format = NULL,
                                 sleeper_players = NULL) {

  if (!identical(platform, "sleeper")) {
    stop(glue("platform '{platform}' is not yet implemented. ",
              "Only 'sleeper' is supported in v1."), call. = FALSE)
  }
  if (is.null(league_id)) {
    stop("league_id is required for the sleeper platform.", call. = FALSE)
  }
  league_id <- trimws(as.character(league_id))

  meta    <- connect_sleeper_league(league_id)
  if (is.null(meta)) {
    stop(glue("Could not connect to Sleeper league {league_id}."), call. = FALSE)
  }
  config  <- build_config_from_sleeper(league_id)
  rosters <- get_sleeper_rosters(league_id)
  players <- sleeper_players %||% get_all_sleeper_players()

  match_season <- as.integer(meta$season %||% season)
  rosters_resolved <- .resolve_roster_gsis(rosters, players,
                                           match_season = match_season)

  # DEF slots come from the raw roster_positions (R/33 config drops DEF/K).
  roster_positions <- meta$roster_positions %||% character(0)
  def_slots <- sum(roster_positions == "DEF", na.rm = TRUE)

  # Format detection. Sleeper's settings$type is authoritative:
  #   0 = redraft, 1 = keeper, 2 = dynasty. Keeper and dynasty both carry
  #   forward value, so both use the dynasty valuation path. TAXI presence is
  #   only a fallback for the rare case where type is missing, because plenty of
  #   dynasty leagues run without a taxi squad.
  league_type <- suppressWarnings(as.integer(meta$settings$type %||% NA))
  fmt <- format
  if (is.null(fmt)) {
    fmt <- if (!is.na(league_type)) {
      if (league_type == 0L) "redraft" else "dynasty"
    } else if ("TAXI" %in% roster_positions) {
      "dynasty"
    } else {
      "redraft"
    }
  }
  if (!fmt %in% c("redraft", "dynasty")) {
    stop(glue("format must be 'redraft' or 'dynasty', got '{fmt}'."),
         call. = FALSE)
  }

  all_rostered_gsis <- rosters_resolved %>%
    dplyr::filter(!is.na(nfl_gsis_id)) %>%
    dplyr::pull(nfl_gsis_id) %>%
    unique()

  user_roster_gsis <- if (is.null(user_roster_id)) {
    character(0)
  } else {
    rosters_resolved %>%
      dplyr::filter(roster_id == as.integer(user_roster_id),
                    !is.na(nfl_gsis_id)) %>%
      dplyr::pull(nfl_gsis_id) %>%
      unique()
  }

  structure(
    list(
      platform          = platform,
      league_id         = league_id,
      season            = as.integer(season),
      config            = config,
      format            = fmt,
      league_type       = league_type,
      def_slots         = as.integer(def_slots),
      roster_positions  = roster_positions,
      rosters_resolved  = rosters_resolved,
      players           = players,
      all_rostered_gsis = all_rostered_gsis,
      user_roster_id    = user_roster_id,
      user_roster_gsis  = user_roster_gsis,
      source            = glue("sleeper_{league_id}")
    ),
    class = c("league_source", "list")
  )
}

# ==============================================================================
# SECTION 2: AVAILABILITY (Sleeper-inferred, manual override on top)
# ==============================================================================

# ------------------------------------------------------------------------------
# load_availability
# ------------------------------------------------------------------------------

#' Infer player availability from Sleeper, with a manual override
#'
#' Combines two Sleeper signals into a transparent availability status. NOTE:
#' neither Sleeper field is a true "out for the season" flag; IR status is the
#' closest proxy and is treated as out-for-season for valuation. The optional
#' override CSV is the authoritative final word.
#'
#'   status == "Injured Reserve" / PUP / NFI, OR roster is_reserve == TRUE
#'        -> "out_for_season"   (collapses redraft value)
#'   injury_status %in% c("Out","Doubtful","IR")
#'        -> "out_week"         (cannot start this week; redraft value intact)
#'   otherwise
#'        -> "active"
#'
#' @param source A league_source from build_league_source().
#' @param availability_path Character or NULL. CSV with columns gsis_id, status
#'   (one of active/out_week/out_for_season), and optional return_week. Overrides
#'   the inferred status for any matching gsis_id.
#' @return Tibble: nfl_gsis_id, availability_status, return_week, avail_source.
#' @export
load_availability <- function(source, availability_path = NULL) {

  if (!inherits(source, "league_source")) {
    stop("source must be a league_source (use build_league_source()).",
         call. = FALSE)
  }

  reserve_gsis <- source$rosters_resolved %>%
    dplyr::filter(.data$is_reserve %in% TRUE,
                  !is.na(nfl_gsis_id)) %>%
    dplyr::pull(nfl_gsis_id) %>%
    unique()

  inferred <- source$players %>%
    dplyr::filter(!is.na(nfl_gsis_id)) %>%
    dplyr::distinct(nfl_gsis_id, .keep_all = TRUE) %>%
    dplyr::transmute(
      nfl_gsis_id,
      out_season = (.data$status %in% OUT_FOR_SEASON_STATUSES) |
                     (nfl_gsis_id %in% reserve_gsis),
      out_week   = .data$injury_status %in% OUT_THIS_WEEK_INJURY,
      availability_status = dplyr::case_when(
        out_season ~ "out_for_season",
        out_week   ~ "out_week",
        TRUE       ~ "active"
      ),
      return_week  = NA_integer_,
      avail_source = "sleeper_inferred"
    ) %>%
    dplyr::select(nfl_gsis_id, availability_status, return_week, avail_source)

  # Manual override (authoritative final word).
  if (!is.null(availability_path) && file.exists(availability_path)) {
    ov <- readr::read_csv(availability_path, show_col_types = FALSE)
    if (!"gsis_id" %in% names(ov) || !"status" %in% names(ov)) {
      warning("Override CSV must have columns gsis_id and status; ignoring.",
              call. = FALSE)
    } else {
      ov <- ov %>%
        dplyr::transmute(
          nfl_gsis_id        = as.character(gsis_id),
          availability_status = as.character(status),
          return_week        = if ("return_week" %in% names(ov)) {
            suppressWarnings(as.integer(.data$return_week))
          } else {
            NA_integer_
          },
          avail_source       = "manual_override"
        )
      inferred <- inferred %>%
        dplyr::filter(!nfl_gsis_id %in% ov$nfl_gsis_id) %>%
        dplyr::bind_rows(ov)
    }
  }

  inferred
}

# ==============================================================================
# SECTION 3: MATCHUP ENGINE (defense-vs-position + opponent-offense)
# ==============================================================================

# ------------------------------------------------------------------------------
# .reg_season_pbp
# ------------------------------------------------------------------------------

#' Filter pbp to regular season (week cap by era; season_type if present)
#' @keywords internal
.reg_season_pbp <- function(pbp) {
  s <- if ("season" %in% names(pbp)) pbp$season[1] else NA_integer_
  wk_cap <- if (!is.na(s) && s >= 2021L) 18L else if (!is.na(s) && s == 2020L) 17L else 16L
  out <- pbp
  if ("season_type" %in% names(out)) {
    out <- dplyr::filter(out, season_type == "REG")
  }
  dplyr::filter(out, .data$week <= wk_cap)
}

# ------------------------------------------------------------------------------
# .game_opponent_map
# ------------------------------------------------------------------------------

#' Long team -> opponent map for every game in a pbp tibble
#'
#' @param pbp Normalized pbp (must contain game_id, home_team, away_team).
#' @return Tibble: game_id, team, opponent (two rows per game).
#' @keywords internal
.game_opponent_map <- function(pbp) {
  need <- c("game_id", "home_team", "away_team")
  miss <- setdiff(need, names(pbp))
  if (length(miss) > 0L) {
    stop(glue("pbp missing columns for opponent map: ",
              "{paste(miss, collapse = ', ')}"), call. = FALSE)
  }
  games <- dplyr::distinct(pbp, game_id, home_team, away_team)
  dplyr::bind_rows(
    dplyr::transmute(games, game_id, team = home_team, opponent = away_team),
    dplyr::transmute(games, game_id, team = away_team, opponent = home_team)
  )
}

# ------------------------------------------------------------------------------
# compute_dvp_factors
# ------------------------------------------------------------------------------

#' Defense-vs-position matchup factors
#'
#' For each defense and position, computes fantasy points allowed per game over
#' the supplied seasons, normalized to the league average for that position. A
#' factor above 1 means the defense gives up more than average to that position
#' (a favorable matchup for the offensive player facing it). Scoring is
#' standard-PPR and the result is normalized, so league scoring quirks cancel.
#'
#' @param seasons Integer vector. Prior seasons to pool (default SEASON - 1).
#' @param roster_data Tibble or NULL. nflreadr roster for accurate positions;
#'   NULL loads nflreadr::load_rosters() per season.
#' @param cache_dir Character. R/15 cache directory.
#' @param cap Numeric. Factor clamp half-width.
#' @return Tibble: def_team, position, dvp_allowed_ppg, dvp_factor.
#' @export
compute_dvp_factors <- function(seasons = SEASON - 1L,
                                roster_data = NULL,
                                cache_dir = CACHE_DIR_DEFAULT,
                                cap = DVP_CAP) {

  per_game <- purrr::map_dfr(seasons, function(s) {
    pbp <- .reg_season_pbp(load_normalized_season(s, cache_dir = cache_dir))
    rost <- roster_data %||% tryCatch(
      nflreadr::load_rosters(seasons = s),
      error = function(e) NULL
    )

    fp <- calculate_fantasy_points(
      pbp_data       = pbp,
      roster_data    = rost,
      use_tiered_ppr = FALSE,
      te_premium     = FALSE,
      rush_att_bonus = 0,
      ppr            = 1
    )
    if (is.null(fp) || nrow(fp) == 0L) return(tibble::tibble())

    opp <- .game_opponent_map(pbp)

    fp %>%
      dplyr::filter(.data$position %in% LINEUP_OFFENSE_POSITIONS) %>%
      dplyr::inner_join(opp, by = c("game_id", "team")) %>%
      dplyr::group_by(.data$opponent, .data$position, .data$game_id) %>%
      dplyr::summarise(fp_allowed = sum(total_fantasy_points, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::rename(def_team = "opponent")
  })

  if (nrow(per_game) == 0L) {
    stop("compute_dvp_factors(): no fantasy data produced. Check cache.",
         call. = FALSE)
  }

  by_def_pos <- per_game %>%
    dplyr::group_by(def_team, .data$position) %>%
    dplyr::summarise(dvp_allowed_ppg = mean(fp_allowed, na.rm = TRUE),
                     .groups = "drop")

  league_pos <- by_def_pos %>%
    dplyr::group_by(.data$position) %>%
    dplyr::summarise(league_ppg = mean(dvp_allowed_ppg, na.rm = TRUE),
                     .groups = "drop")

  by_def_pos %>%
    dplyr::left_join(league_pos, by = "position") %>%
    dplyr::mutate(
      dvp_factor = dplyr::if_else(
        .data$league_ppg > 0,
        pmin(pmax(.data$dvp_allowed_ppg / .data$league_ppg, 1 - cap), 1 + cap),
        1
      )
    ) %>%
    dplyr::select(def_team, position, dvp_allowed_ppg, dvp_factor)
}

# ------------------------------------------------------------------------------
# .score_def_games_standard
# ------------------------------------------------------------------------------

#' Score raw DEF/ST per-game counts under standard scoring
#'
#' Uses R/29's exported DEF scoring constants when present, else the standard
#' fallback. Scoring is normalized downstream, so exact values only need to be
#' internally consistent.
#' @keywords internal
.score_def_games_standard <- function(counts) {
  tiers <- if (exists("DEF_PTS_ALLOW_TIERS")) {
    unlist(get("DEF_PTS_ALLOW_TIERS"))
  } else {
    DEF_PTS_ALLOW_TIERS_FALLBACK
  }
  ev <- if (exists("DEF_EVENT_POINTS")) {
    unlist(get("DEF_EVENT_POINTS"))
  } else {
    DEF_EVENT_POINTS_FALLBACK
  }

  pa <- counts$opponent_pts_allowed
  pa_pts <- dplyr::case_when(
    is.na(pa)    ~ NA_real_,
    pa == 0      ~ tiers[["pts_allow_0"]],
    pa <= 6      ~ tiers[["pts_allow_1_6"]],
    pa <= 13     ~ tiers[["pts_allow_7_13"]],
    pa <= 20     ~ tiers[["pts_allow_14_20"]],
    pa <= 27     ~ tiers[["pts_allow_21_27"]],
    pa <= 34     ~ tiers[["pts_allow_28_34"]],
    TRUE         ~ tiers[["pts_allow_35p"]]
  )

  counts %>%
    dplyr::mutate(
      def_st_points = pa_pts +
        .data$sacks         * ev[["sack"]] +
        .data$def_ints      * ev[["def_int"]] +
        .data$fum_recs      * ev[["fum_rec"]] +
        .data$def_tds       * ev[["def_td"]] +
        .data$safeties      * ev[["safety"]] +
        .data$blocked_kicks * ev[["blk_kick"]]
    )
}

# ------------------------------------------------------------------------------
# compute_def_matchup_factors
# ------------------------------------------------------------------------------

#' Opponent-offense matchup factors for the DEF/ST slot
#'
#' For each offense, computes the average DEF/ST fantasy points it CONCEDES to
#' opposing defenses per game, normalized to the league average. A defense
#' facing a generous (turnover-prone, low-scoring) offense gets a factor above
#' 1. This is the DEF analogue of defense-vs-position.
#'
#' @param seasons Integer vector. Prior seasons to pool (default SEASON - 1).
#' @param cache_dir Character. R/15 cache directory.
#' @param cap Numeric. Factor clamp half-width.
#' @return Tibble: offense_team, conceded_ppg, def_matchup_factor.
#' @export
compute_def_matchup_factors <- function(seasons = SEASON - 1L,
                                        cache_dir = CACHE_DIR_DEFAULT,
                                        cap = DVP_CAP) {

  per_game <- purrr::map_dfr(seasons, function(s) {
    pbp    <- .reg_season_pbp(load_normalized_season(s, cache_dir = cache_dir))
    counts <- calculate_def_st_points(pbp)
    if (is.null(counts) || nrow(counts) == 0L) return(tibble::tibble())

    scored <- .score_def_games_standard(counts)
    opp    <- .game_opponent_map(pbp)

    # The offense that conceded = the opponent of the defensive team.
    scored %>%
      dplyr::inner_join(opp, by = c("game_id", "team")) %>%
      dplyr::transmute(game_id,
                       offense_team = .data$opponent,
                       def_st_points)
  })

  if (nrow(per_game) == 0L) {
    stop("compute_def_matchup_factors(): no DEF data produced. Check cache.",
         call. = FALSE)
  }

  by_off <- per_game %>%
    dplyr::group_by(offense_team) %>%
    dplyr::summarise(conceded_ppg = mean(def_st_points, na.rm = TRUE),
                     .groups = "drop")

  league_ppg <- mean(by_off$conceded_ppg, na.rm = TRUE)

  by_off %>%
    dplyr::mutate(
      def_matchup_factor = if (league_ppg > 0) {
        pmin(pmax(.data$conceded_ppg / league_ppg, 1 - cap), 1 + cap)
      } else {
        1
      }
    )
}

# ------------------------------------------------------------------------------
# .apply_matchup
# ------------------------------------------------------------------------------

#' Apply matchup factors to a player pool for a given week
#'
#' Offensive players: base_proj * dvp_factor(opponent defense, position).
#' DEF rows: base_proj * def_matchup_factor(opponent offense).
#' Preseason / no schedule (week_matchups NULL) -> factor 1 (neutral).
#'
#' @param pool Player pool with nfl_gsis_id, team, position, base_proj.
#' @param week_matchups Tibble team/opponent for the target week, or NULL.
#' @param dvp compute_dvp_factors() output, or NULL.
#' @param def_factors compute_def_matchup_factors() output, or NULL.
#' @return pool with opponent, matchup_factor, adj_proj added.
#' @keywords internal
.apply_matchup <- function(pool, week_matchups, dvp, def_factors) {

  if (is.null(week_matchups)) {
    return(dplyr::mutate(pool,
                         opponent       = NA_character_,
                         matchup_factor = 1,
                         adj_proj       = .data$base_proj))
  }

  pool <- pool %>%
    dplyr::left_join(dplyr::select(week_matchups, team, opponent),
                     by = "team")

  off <- pool %>%
    dplyr::filter(.data$position %in% LINEUP_OFFENSE_POSITIONS)
  if (!is.null(dvp)) {
    off <- off %>%
      dplyr::left_join(dvp,
                       by = c("opponent" = "def_team", "position")) %>%
      dplyr::mutate(matchup_factor = dplyr::coalesce(.data$dvp_factor, 1)) %>%
      dplyr::select(-dvp_allowed_ppg, -dvp_factor)
  } else {
    off <- dplyr::mutate(off, matchup_factor = 1)
  }

  def <- pool %>% dplyr::filter(.data$position == "DEF")
  if (!is.null(def_factors)) {
    def <- def %>%
      dplyr::left_join(dplyr::select(def_factors, offense_team,
                                     def_matchup_factor),
                       by = c("opponent" = "offense_team")) %>%
      dplyr::mutate(matchup_factor = dplyr::coalesce(.data$def_matchup_factor, 1)) %>%
      dplyr::select(-def_matchup_factor)
  } else {
    def <- dplyr::mutate(def, matchup_factor = 1)
  }

  dplyr::bind_rows(off, def) %>%
    dplyr::mutate(adj_proj = .data$base_proj * .data$matchup_factor)
}

# ------------------------------------------------------------------------------
# .load_week_matchups
# ------------------------------------------------------------------------------

#' team -> opponent map for one week from the NFL schedule
#' @keywords internal
.load_week_matchups <- function(season, week) {
  if (is.null(week)) return(NULL)
  sched <- tryCatch(nflreadr::load_schedules(seasons = season),
                    error = function(e) NULL)
  if (is.null(sched) || nrow(sched) == 0L) return(NULL)
  wk <- dplyr::filter(sched, .data$week == as.integer(!!week))
  if (nrow(wk) == 0L) return(NULL)
  dplyr::bind_rows(
    dplyr::transmute(wk, team = .data$home_team, opponent = .data$away_team),
    dplyr::transmute(wk, team = .data$away_team, opponent = .data$home_team)
  )
}

# ==============================================================================
# SECTION 4: SCORING ASSEMBLY + FORMAT-AWARE VALUATION
# ==============================================================================

# ------------------------------------------------------------------------------
# .norm_name
# ------------------------------------------------------------------------------

#' Normalize a player name for cross-source matching (lowercase, drop suffixes
#' and punctuation). Vectorized; NA becomes "".
#' @keywords internal
.norm_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", x)
  gsub("[^a-z0-9]", "", x)
}

# ------------------------------------------------------------------------------
# assemble_player_pool
# ------------------------------------------------------------------------------

#' Assemble one player pool from offense, DEF, and VORP
#'
#' Stacks R/32 offensive projections and R/34 DEF projections (DEF as a
#' pseudo-player with id "DEF_<team>"), joins R/33 VORP for offense, optionally
#' nudges base_proj toward the R/18 roll3 form, and tags roster/availability
#' membership against the league source.
#'
#' @param source league_source.
#' @param reconciled R/32 reconciled projections.
#' @param vorp R/33 VORP rankings (single-league).
#' @param def_proj R/34 DEF projections.
#' @param roll3 Tibble or NULL. nfl_gsis_id + fantasy_pts_roll3 (R/18).
#' @param roll3_weight Numeric in [0,1]. 0 = projection only (default).
#' @return Pool tibble with nfl_gsis_id, player_name, team, position, base_proj,
#'   lower_80, upper_80, boom_probability, bust_probability, adjusted_vorp,
#'   on_user_roster, is_available.
#' @export
assemble_player_pool <- function(source, reconciled, vorp, def_proj,
                                 roll3 = NULL, roll3_weight = 0) {

  off <- reconciled %>%
    dplyr::transmute(
      nfl_gsis_id,
      player_name      = .data$player_name,
      team             = .data$team,
      position         = .data$position,
      base_proj        = .data$r32_posterior_mu,
      lower_80         = .data$r32_projection_lower_80,
      upper_80         = .data$r32_projection_upper_80,
      boom_probability = .data$boom_probability,
      bust_probability = .data$bust_probability
    ) %>%
    dplyr::filter(.data$position %in% LINEUP_OFFENSE_POSITIONS)

  # Optional roll3 recency nudge (blend, never replace).
  if (!is.null(roll3) && roll3_weight > 0) {
    r3 <- roll3 %>% dplyr::select(nfl_gsis_id, fantasy_pts_roll3)
    off <- off %>%
      dplyr::left_join(r3, by = "nfl_gsis_id") %>%
      dplyr::mutate(
        base_proj = dplyr::if_else(
          !is.na(fantasy_pts_roll3),
          (1 - roll3_weight) * base_proj + roll3_weight * fantasy_pts_roll3,
          base_proj
        )
      ) %>%
      dplyr::select(-fantasy_pts_roll3)
  }

  vorp_lk <- vorp %>% dplyr::select(nfl_gsis_id, adjusted_vorp)
  off <- dplyr::left_join(off, vorp_lk, by = "nfl_gsis_id")

  def <- def_proj %>%
    dplyr::transmute(
      nfl_gsis_id      = paste0("DEF_", .data$team),
      player_name      = paste(.data$team, "DEF"),
      team             = .data$team,
      position         = "DEF",
      base_proj        = .data$def_proj_ppg,
      lower_80         = NA_real_,
      upper_80         = NA_real_,
      boom_probability = NA_real_,
      bust_probability = NA_real_,
      adjusted_vorp    = NA_real_
    )

  pool <- dplyr::bind_rows(off, def)

  # Roster membership resolves by gsis AND by name+position against the pool.
  # The name path is essential: just-drafted rookies often have no gsis in a
  # thin early-offseason nflreadr roster, so a gsis-only check would wrongly
  # flag a rostered rookie as a free agent.
  roster_named <- source$rosters_resolved %>%
    dplyr::left_join(
      dplyr::select(source$players, sleeper_player_id,
                    sp_name = "player_name", sp_pos = "position"),
      by = c("player_id" = "sleeper_player_id")
    ) %>%
    dplyr::mutate(name_key = paste(.norm_name(.data$sp_name),
                                   toupper(.data$sp_pos)))

  all_rostered_gsis <- source$all_rostered_gsis
  all_rostered_key  <- roster_named$name_key

  user_rid  <- if (is.null(source$user_roster_id)) NA_integer_ else
    as.integer(source$user_roster_id)
  user_key  <- roster_named %>%
    dplyr::filter(.data$roster_id == user_rid) %>%
    dplyr::pull(name_key)
  user_gsis <- source$user_roster_gsis

  pool %>%
    dplyr::mutate(
      pool_key       = paste(.norm_name(.data$player_name),
                             toupper(.data$position)),
      on_user_roster = .data$nfl_gsis_id %in% user_gsis |
                         .data$pool_key %in% user_key,
      is_available   = !(.data$nfl_gsis_id %in% all_rostered_gsis |
                           .data$pool_key %in% all_rostered_key)
    ) %>%
    dplyr::select(-pool_key)
}

# ------------------------------------------------------------------------------
# .aging_trajectory_multiplier
# ------------------------------------------------------------------------------

#' Age-based trajectory multiplier for the dynasty forward score
#'
#' Above 1 below the positional peak (rising asset), below 1 past it (declining).
#' Consumes only the verified peak_age_quad field from an R/23 curve list when
#' supplied; otherwise uses the internal peak-age fallback table.
#'
#' @param age Numeric vector of ages (NA allowed -> multiplier 1).
#' @param position Character vector, same length as age.
#' @param aging_curves Named list keyed by position (R/23 fit output), or NULL.
#' @return Numeric vector of multipliers, clamped to AGING_MULT_BOUNDS.
#' @keywords internal
.aging_trajectory_multiplier <- function(age, position, aging_curves = NULL) {
  peak_for <- function(pos) {
    if (!is.null(aging_curves) && !is.na(pos) && !is.null(aging_curves[[pos]]) &&
        !is.null(aging_curves[[pos]]$peak_age_quad)) {
      return(as.numeric(aging_curves[[pos]]$peak_age_quad))
    }
    if (!is.na(pos) && pos %in% names(PEAK_AGE_FALLBACK)) {
      return(as.numeric(PEAK_AGE_FALLBACK[[pos]]))
    }
    27
  }

  peak <- vapply(position, peak_for, numeric(1))
  raw  <- 1 + AGING_SLOPE * (peak - age)
  raw[is.na(age)] <- 1
  pmin(pmax(raw, AGING_MULT_BOUNDS[1]), AGING_MULT_BOUNDS[2])
}

# ------------------------------------------------------------------------------
# .pct_rank_within
# ------------------------------------------------------------------------------

#' Position-wise percentile rank in [0,1] (NA-safe; ties averaged)
#' @keywords internal
.pct_rank_within <- function(x, group) {
  out <- rep(NA_real_, length(x))
  for (g in unique(group)) {
    idx <- which(group == g & !is.na(x))
    if (length(idx) == 0L) next
    if (length(idx) == 1L) {
      out[idx] <- 1
    } else {
      out[idx] <- (rank(x[idx], ties.method = "average") - 1) /
        (length(idx) - 1)
    }
  }
  out
}

# ------------------------------------------------------------------------------
# compute_player_values
# ------------------------------------------------------------------------------

#' Format-aware player value (redraft vs dynasty)
#'
#' Redraft value = rest-of-season projected points, gated by availability (an
#' out-for-season player collapses toward 0). Dynasty value blends, on a common
#' within-position percentile scale, the rest-of-season value with a forward
#' score = talent percentile * age-trajectory multiplier + prospect pedigree.
#' The format-selected value drives roster and waiver drop decisions only; the
#' weekly lineup objective always uses points, not this value.
#'
#' @param pool assemble_player_pool() output.
#' @param format "redraft" or "dynasty".
#' @param availability load_availability() output, or NULL (all active).
#' @param aging_curves R/23 curve list or NULL.
#' @param prospect_scores R/28 prospect tibble (nfl_gsis_id, score_final) or NULL.
#' @param player_ages Tibble (nfl_gsis_id, age) or NULL; the Sleeper source is
#'   used automatically when this is NULL and a source's players are available.
#' @param dynasty_future_weight Numeric in [0,1].
#' @return pool with availability_status, ros_value, forward_score, value added.
#' @export
compute_player_values <- function(pool,
                                   format = c("redraft", "dynasty"),
                                   availability = NULL,
                                   aging_curves = NULL,
                                   prospect_scores = NULL,
                                   player_ages = NULL,
                                   dynasty_future_weight =
                                     DYNASTY_FUTURE_WEIGHT_DEFAULT) {

  format <- match.arg(format)

  # Availability gate.
  if (is.null(availability)) {
    pool$availability_status <- "active"
  } else {
    av <- availability %>%
      dplyr::select(nfl_gsis_id, availability_status)
    pool <- pool %>% dplyr::left_join(av, by = "nfl_gsis_id")
    pool$availability_status <- dplyr::coalesce(pool$availability_status,
                                                "active")
  }

  ros_factor <- dplyr::if_else(pool$availability_status == "out_for_season",
                               0, 1)
  pool$ros_value <- pool$base_proj * ros_factor

  if (format == "redraft") {
    pool$forward_score <- NA_real_
    pool$forward_vorp  <- dplyr::coalesce(pool$adjusted_vorp, 0)
    pool$value <- pool$ros_value
    return(pool)
  }

  # Dynasty forward score (offense only; DEF carries ros_value as value).
  ages <- player_ages
  if (is.null(ages)) ages <- tibble::tibble(nfl_gsis_id = character(),
                                            age = numeric())
  pool <- dplyr::left_join(pool, dplyr::select(ages, nfl_gsis_id, age),
                           by = "nfl_gsis_id")

  prosp <- if (is.null(prospect_scores)) {
    tibble::tibble(nfl_gsis_id = character(), score_final = numeric())
  } else {
    prospect_scores %>% dplyr::select(nfl_gsis_id, score_final)
  }
  pool <- dplyr::left_join(pool, prosp, by = "nfl_gsis_id")

  off_mask <- pool$position %in% LINEUP_OFFENSE_POSITIONS

  talent_pct <- .pct_rank_within(pool$base_proj, pool$position)
  traj       <- .aging_trajectory_multiplier(pool$age, pool$position,
                                             aging_curves)
  prospect_pct <- .pct_rank_within(pool$score_final, pool$position)
  prospect_pct[is.na(prospect_pct)] <- 0

  fwd_raw <- talent_pct * traj + prospect_pct
  pool$forward_score <- .pct_rank_within(fwd_raw, pool$position)

  # Cross-position forward value: VORP nudged up for youth and pedigree, down for
  # age. Additive, so a below-replacement VORP is still lifted by upside.
  pool$forward_vorp <- dplyr::coalesce(pool$adjusted_vorp, 0) +
    (traj - 1) * AGE_VORP_SCALE +
    PROSPECT_VORP_SCALE * (dplyr::coalesce(pool$score_final, 0) / 100)

  ros_pct <- .pct_rank_within(pool$ros_value, pool$position)

  dyn <- (1 - dynasty_future_weight) * ros_pct +
    dynasty_future_weight * pool$forward_score

  # DEF (no forward score) keeps its ros-based percentile.
  pool$value <- dplyr::if_else(off_mask & !is.na(dyn), dyn, ros_pct)
  pool
}

# ==============================================================================
# SECTION 5: OPTIMIZER (integer program via lpSolve)
# ==============================================================================

# ------------------------------------------------------------------------------
# .build_slot_caps
# ------------------------------------------------------------------------------

#' Per-roster slot capacities from a league_source
#' @keywords internal
.build_slot_caps <- function(source) {
  st <- source$config$starters
  caps <- c(
    QB         = as.numeric(st$QB %||% 0),
    RB         = as.numeric(st$RB %||% 0),
    WR         = as.numeric(st$WR %||% 0),
    TE         = as.numeric(st$TE %||% 0),
    FLEX       = as.numeric(source$config$flex %||% 0),
    SUPER_FLEX = as.numeric(source$config$superflex %||% 0),
    DEF        = as.numeric(source$def_slots %||% 0)
  )
  caps[caps > 0]
}

# ------------------------------------------------------------------------------
# .solve_lineup_ip
# ------------------------------------------------------------------------------

#' Solve the lineup assignment as a binary integer program
#'
#' Maximizes sum of opt_value over chosen (player, slot) assignments, subject to
#' each player used at most once and each slot filled to at most its capacity.
#'
#' @param players Tibble with nfl_gsis_id, position, opt_value (finite).
#' @param slot_caps Named numeric vector of slot capacities (> 0).
#' @return Tibble: nfl_gsis_id, slot (the chosen assignment); empty if none.
#' @keywords internal
.solve_lineup_ip <- function(players, slot_caps) {

  players <- dplyr::filter(players, !is.na(.data$opt_value),
                           is.finite(.data$opt_value))
  slots <- names(slot_caps)
  if (nrow(players) == 0L || length(slots) == 0L) {
    return(tibble::tibble(nfl_gsis_id = character(), slot = character()))
  }

  # Enumerate eligible (player-row, slot) columns.
  cols <- list()
  for (i in seq_len(nrow(players))) {
    elig <- intersect(SLOT_ELIGIBILITY[[players$position[i]]] %||% character(0),
                      slots)
    for (s in elig) cols[[length(cols) + 1L]] <- c(i = i, s = match(s, slots))
  }
  if (length(cols) == 0L) {
    return(tibble::tibble(nfl_gsis_id = character(), slot = character()))
  }

  col_i <- vapply(cols, function(cc) as.integer(cc[["i"]]), integer(1))
  col_s <- vapply(cols, function(cc) as.integer(cc[["s"]]), integer(1))
  obj   <- players$opt_value[col_i]

  player_rows <- sort(unique(col_i))
  con <- list(); dir <- character(0); rhs <- numeric(0)

  # Each player assigned at most once.
  for (pi in player_rows) {
    con[[length(con) + 1L]] <- as.numeric(col_i == pi)
    dir <- c(dir, "<="); rhs <- c(rhs, 1)
  }
  # Each slot at most its capacity.
  for (si in seq_along(slots)) {
    con[[length(con) + 1L]] <- as.numeric(col_s == si)
    dir <- c(dir, "<="); rhs <- c(rhs, slot_caps[[slots[si]]])
  }

  mat <- do.call(rbind, con)
  sol <- lpSolve::lp(direction = "max", objective.in = obj,
                     const.mat = mat, const.dir = dir, const.rhs = rhs,
                     all.bin = TRUE)

  if (sol$status != 0L) {
    warning("lpSolve returned a non-optimal status; lineup may be empty.",
            call. = FALSE)
    return(tibble::tibble(nfl_gsis_id = character(), slot = character()))
  }

  chosen <- which(sol$solution > 0.5)
  if (length(chosen) == 0L) {
    return(tibble::tibble(nfl_gsis_id = character(), slot = character()))
  }
  tibble::tibble(
    nfl_gsis_id = players$nfl_gsis_id[col_i[chosen]],
    slot        = slots[col_s[chosen]]
  )
}

# ------------------------------------------------------------------------------
# .flag_confidence
# ------------------------------------------------------------------------------

#' Confidence flag per started slot
#'
#' Offense: compares the starter's 80% interval to the best benched alternative
#' eligible for the same slot. Non-overlapping (starter lower >= alt upper) is
#' "confident"; overlapping is "close"; no alternative is "no_alt". DEF uses a
#' fixed PPG gap against the next-best available defense.
#'
#' @param starters Tibble of started players with slot, position, lower_80,
#'   upper_80, adj_proj.
#' @param alternatives Tibble of non-started eligible players (bench + available
#'   for DEF) with position, lower_80, upper_80, adj_proj.
#' @param def_gap_threshold Numeric PPG gap for the DEF flag.
#' @return starters with confidence_flag added.
#' @keywords internal
.flag_confidence <- function(starters, alternatives,
                             def_gap_threshold = DEF_GAP_THRESHOLD) {

  flag_one <- function(row) {
    s <- row$slot
    if (s == "DEF") {
      alt_def <- alternatives %>% dplyr::filter(.data$position == "DEF")
      if (nrow(alt_def) == 0L) return("no_alt")
      best_alt <- max(alt_def$adj_proj, na.rm = TRUE)
      gap <- row$adj_proj - best_alt
      return(if (gap >= def_gap_threshold) "confident" else "close")
    }

    # Offensive slot: alternatives eligible for this slot.
    elig_pos <- names(SLOT_ELIGIBILITY)[
      vapply(SLOT_ELIGIBILITY, function(v) s %in% v, logical(1))
    ]
    alt <- alternatives %>%
      dplyr::filter(.data$position %in% elig_pos)
    if (nrow(alt) == 0L) return("no_alt")
    best_idx <- which.max(alt$adj_proj)
    alt_upper <- alt$upper_80[best_idx]
    if (is.na(row$lower_80) || is.na(alt_upper)) return("close")
    if (row$lower_80 >= alt_upper) "confident" else "close"
  }

  flags <- vapply(seq_len(nrow(starters)),
                  function(i) flag_one(starters[i, ]), character(1))
  dplyr::mutate(starters, confidence_flag = flags)
}

# ------------------------------------------------------------------------------
# optimize_lineup
# ------------------------------------------------------------------------------

#' Optimal weekly starting lineup for the user's roster
#'
#' Builds the player pool, gates out unavailable players, applies the matchup
#' adjustment (if a week and factors are supplied), solves the full-lineup
#' integer program on matchup-adjusted points, and flags decision confidence.
#'
#' @param source league_source (must carry a user roster).
#' @param reconciled R/32 reconciled projections.
#' @param vorp R/33 VORP rankings.
#' @param def_proj R/34 DEF projections.
#' @param week Integer or NULL. NULL = preseason / neutral matchup.
#' @param availability load_availability() output, or NULL.
#' @param dvp compute_dvp_factors() output, or NULL (computed if week given).
#' @param def_factors compute_def_matchup_factors() output, or NULL.
#' @param roll3 R/18 roll3 tibble, or NULL.
#' @param roll3_weight Numeric in [0,1].
#' @param season Integer (default = source season).
#' @param cache_dir Character. R/15 cache dir (for on-the-fly matchup factors).
#' @param save_output Logical. Write RDS + CSV of the starting lineup.
#' @return List: starters, bench, unavailable, all_close (the close calls).
#' @export
optimize_lineup <- function(source, reconciled, vorp, def_proj,
                            week = NULL, availability = NULL,
                            dvp = NULL, def_factors = NULL,
                            roll3 = NULL, roll3_weight = 0,
                            season = NULL, cache_dir = CACHE_DIR_DEFAULT,
                            save_output = FALSE) {

  if (!inherits(source, "league_source")) {
    stop("source must be a league_source.", call. = FALSE)
  }
  if (length(source$user_roster_gsis) == 0L) {
    stop("source has no user roster. Pass user_roster_id to ",
         "build_league_source().", call. = FALSE)
  }
  season <- season %||% source$season

  pool <- assemble_player_pool(source, reconciled, vorp, def_proj,
                               roll3 = roll3, roll3_weight = roll3_weight)

  # Restrict to the user's roster: offense by gsis, DEF by team membership.
  user_def_teams <- source$rosters_resolved %>%
    dplyr::filter(roster_id == as.integer(source$user_roster_id)) %>%
    dplyr::pull(player_id)
  # DEF pseudo-ids are "DEF_<team>"; we keep any DEF whose team's gsis-less
  # Sleeper slot is on the user roster is non-trivial, so we keep all DEF on the
  # roster by matching the projected team to the user's rostered DEF where the
  # Sleeper player_id position is DEF. Simpler and robust: keep DEF rows whose
  # team appears among the user's rostered Sleeper DEF entries.
  user_def_team_codes <- source$players %>%
    dplyr::filter(.data$position == "DEF",
                  .data$sleeper_player_id %in% user_def_teams) %>%
    dplyr::pull(.data$team)

  roster_pool <- pool %>%
    dplyr::filter(.data$on_user_roster |
                    (.data$position == "DEF" &
                       .data$team %in% user_def_team_codes))

  # Availability gate: both out_for_season and out_week cannot start this week.
  vals <- compute_player_values(roster_pool, format = "redraft",
                                availability = availability)
  startable <- vals %>%
    dplyr::filter(!.data$availability_status %in%
                    c("out_for_season", "out_week"))
  unavailable <- vals %>%
    dplyr::filter(.data$availability_status %in%
                    c("out_for_season", "out_week"))

  # Matchup adjustment.
  if (!is.null(week)) {
    if (is.null(dvp)) dvp <- compute_dvp_factors(seasons = season - 1L,
                                                 cache_dir = cache_dir)
    if (is.null(def_factors)) {
      def_factors <- compute_def_matchup_factors(seasons = season - 1L,
                                                 cache_dir = cache_dir)
    }
    week_matchups <- .load_week_matchups(season, week)
  } else {
    week_matchups <- NULL
  }
  startable <- .apply_matchup(startable, week_matchups, dvp, def_factors)

  # Solve.
  slot_caps <- .build_slot_caps(source)
  ip_in <- startable %>%
    dplyr::transmute(nfl_gsis_id, position, opt_value = .data$adj_proj)
  assign <- .solve_lineup_ip(ip_in, slot_caps)

  starters <- assign %>%
    dplyr::left_join(startable, by = "nfl_gsis_id") %>%
    dplyr::arrange(match(.data$slot, names(slot_caps)),
                   dplyr::desc(.data$adj_proj))

  bench <- startable %>%
    dplyr::filter(!.data$nfl_gsis_id %in% starters$nfl_gsis_id)

  # Confidence flags use bench + DEF alternatives.
  starters <- .flag_confidence(starters, bench)

  out_tbl <- starters %>%
    dplyr::transmute(
      slot, nfl_gsis_id, player_name, team, position,
      opponent       = .data$opponent,
      base_proj      = round(.data$base_proj, 2),
      matchup_factor = round(.data$matchup_factor, 3),
      adj_proj       = round(.data$adj_proj, 2),
      adjusted_vorp  = round(.data$adjusted_vorp, 1),
      confidence_flag,
      schema_tag     = SCHEMA_TAG_LINEUP
    )

  if (save_output) {
    dir.create(dirname(OUTPUT_RDS_PATH_LINEUP), recursive = TRUE,
               showWarnings = FALSE)
    saveRDS(out_tbl, OUTPUT_RDS_PATH_LINEUP)
    readr::write_csv(out_tbl, OUTPUT_CSV_PATH_LINEUP)
    message(glue("  Saved: {OUTPUT_RDS_PATH_LINEUP}"))
    message(glue("  Saved: {OUTPUT_CSV_PATH_LINEUP}"))
  }

  # KEY INSIGHTS (computed from output).
  total_pts  <- sum(out_tbl$adj_proj, na.rm = TRUE)
  n_close    <- sum(out_tbl$confidence_flag == "close", na.rm = TRUE)
  wk_label   <- if (is.null(week)) "preseason" else glue("week {week}")

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/35: Optimal lineup ({wk_label}, {source$format})"))
  message(glue("{strrep('=', 70)}"))
  message(glue("  Slots filled:        {nrow(out_tbl)}"))
  message(glue("  Projected total:     {format(round(total_pts, 1), nsmall = 1)} pts"))
  message(glue("  Close-call slots:    {n_close}"))
  message(glue("  Unavailable held:    {nrow(unavailable)}"))
  message(glue("{strrep('=', 70)}\n"))

  list(starters = out_tbl, bench = bench, unavailable = unavailable,
       all_close = out_tbl %>% dplyr::filter(.data$confidence_flag == "close"))
}

# ==============================================================================
# SECTION 6: WAIVER OPTIMIZER
# ==============================================================================

# ------------------------------------------------------------------------------
# suggest_waiver_adds
# ------------------------------------------------------------------------------

#' Rank waiver / free-agent adds by lineup improvement, format-aware
#'
#' Available players (no team's roster holds them) are ranked by how many
#' projected points adding them would add to the optimal starting lineup. The
#' drop candidate is the lowest format-value player on the user roster: in
#' redraft that is the rest-of-season floor (an out-for-season player drops out);
#' in dynasty it is the lowest blended value, which protects an injured elite.
#'
#' @param source league_source (with a user roster).
#' @param reconciled R/32 reconciled projections.
#' @param vorp R/33 VORP rankings.
#' @param def_proj R/34 DEF projections.
#' @param format "redraft"/"dynasty"/NULL (NULL uses source$format).
#' @param mode "auto"/"lineup"/"stash". auto picks stash for dynasty, lineup for
#'   redraft. lineup ranks by this-week lineup-point gain; stash skips the lineup
#'   solve and ranks available players by forward-looking dynasty value.
#' @param week Integer or NULL.
#' @param availability load_availability() output, or NULL.
#' @param dvp / def_factors Optional precomputed matchup factors.
#' @param aging_curves / prospect_scores / player_ages Dynasty inputs.
#' @param dynasty_future_weight Numeric in [0,1].
#' @param max_suggestions Integer. Top-N adds to return.
#' @param cache_dir Character. R/15 cache dir.
#' @return lineup mode: add_player, add_team, add_position, proj_gain,
#'   drop_player, drop_value, format. stash mode: add_player, add_team,
#'   add_position, forward_vorp, adjusted_vorp, forward_score, value_gain,
#'   drop_player, drop_value, format.
#' @export
suggest_waiver_adds <- function(source, reconciled, vorp, def_proj,
                                format = NULL,
                                mode = c("auto", "lineup", "stash"),
                                week = NULL,
                                availability = NULL,
                                dvp = NULL, def_factors = NULL,
                                aging_curves = NULL, prospect_scores = NULL,
                                player_ages = NULL,
                                dynasty_future_weight =
                                  DYNASTY_FUTURE_WEIGHT_DEFAULT,
                                max_suggestions = 10L,
                                cache_dir = CACHE_DIR_DEFAULT) {

  if (!inherits(source, "league_source")) {
    stop("source must be a league_source.", call. = FALSE)
  }
  if (length(source$user_roster_gsis) == 0L) {
    stop("source has no user roster.", call. = FALSE)
  }
  format <- format %||% source$format
  mode   <- match.arg(mode)
  if (mode == "auto") mode <- if (format == "dynasty") "stash" else "lineup"
  season <- source$season

  # Default player ages from the Sleeper source (used by the dynasty path).
  if (is.null(player_ages)) {
    player_ages <- source$players %>%
      dplyr::filter(!is.na(nfl_gsis_id)) %>%
      dplyr::distinct(nfl_gsis_id, .keep_all = TRUE) %>%
      dplyr::select(nfl_gsis_id, age)
  }

  pool <- assemble_player_pool(source, reconciled, vorp, def_proj)
  pool <- compute_player_values(pool, format = format,
                                availability = availability,
                                aging_curves = aging_curves,
                                prospect_scores = prospect_scores,
                                player_ages = player_ages,
                                dynasty_future_weight = dynasty_future_weight)

  user_gsis <- pool %>%
    dplyr::filter(.data$on_user_roster) %>%
    dplyr::pull(.data$nfl_gsis_id)

  # ---- STASH MODE: rank available free agents by dynasty forward value ----
  # No lineup solve. Surfaces future-oriented adds (young upside) that a
  # this-week lineup-gain check structurally cannot show. Ranked by the format
  # value (in dynasty, a blend of rest-of-season and forward score), with the
  # lowest-value roster player as the standing drop.
  if (mode == "stash") {
    roster_vals <- pool %>% dplyr::filter(.data$nfl_gsis_id %in% user_gsis)
    drop_row <- roster_vals %>%
      dplyr::arrange(.data$forward_vorp) %>% dplyr::slice(1)
    drop_val <- if (nrow(drop_row) == 0L) NA_real_ else drop_row$forward_vorp
    drop_nm  <- if (nrow(drop_row) == 0L) NA_character_ else drop_row$player_name

    fa <- pool %>%
      dplyr::filter(.data$is_available,
                    .data$position %in% LINEUP_OFFENSE_POSITIONS)
    if (nrow(fa) == 0L) {
      message("No available offensive free agents to evaluate.")
      return(tibble::tibble(
        add_player = character(), add_team = character(),
        add_position = character(), forward_vorp = numeric(),
        adjusted_vorp = numeric(), forward_score = numeric(),
        value_gain = numeric(), drop_player = character(),
        drop_value = numeric(), format = character()))
    }
    return(
      fa %>%
        dplyr::transmute(
          add_player    = .data$player_name,
          add_team      = .data$team,
          add_position  = .data$position,
          forward_vorp  = round(.data$forward_vorp, 1),
          adjusted_vorp = round(.data$adjusted_vorp, 1),
          forward_score = round(.data$forward_score, 3),
          value_gain    = round(.data$forward_vorp - drop_val, 1),
          drop_player   = drop_nm,
          drop_value    = round(drop_val, 1),
          format        = format
        ) %>%
        dplyr::arrange(dplyr::desc(.data$forward_vorp)) %>%
        dplyr::slice_head(n = max_suggestions)
    )
  }

  # Matchup factors (shared across all candidate solves).
  if (!is.null(week)) {
    if (is.null(dvp)) dvp <- compute_dvp_factors(seasons = season - 1L,
                                                 cache_dir = cache_dir)
    if (is.null(def_factors)) {
      def_factors <- compute_def_matchup_factors(seasons = season - 1L,
                                                 cache_dir = cache_dir)
    }
    week_matchups <- .load_week_matchups(season, week)
  } else {
    week_matchups <- NULL
  }

  slot_caps <- .build_slot_caps(source)

  # Helper: optimal startable lineup points for a roster set (offense gsis +
  # DEF rows whose team is in the set).
  lineup_points <- function(roster_gsis, extra = NULL) {
    set_pool <- pool %>%
      dplyr::filter(.data$nfl_gsis_id %in% roster_gsis)
    if (!is.null(extra)) {
      set_pool <- dplyr::bind_rows(set_pool,
                                   dplyr::filter(pool,
                                                 .data$nfl_gsis_id %in% extra))
    }
    set_pool <- set_pool %>%
      dplyr::filter(!.data$availability_status %in%
                      c("out_for_season", "out_week"))
    set_pool <- .apply_matchup(set_pool, week_matchups, dvp, def_factors)
    ip_in <- set_pool %>%
      dplyr::transmute(nfl_gsis_id, position, opt_value = .data$adj_proj)
    assign <- .solve_lineup_ip(ip_in, slot_caps)
    if (nrow(assign) == 0L) return(0)
    set_pool %>%
      dplyr::filter(.data$nfl_gsis_id %in% assign$nfl_gsis_id) %>%
      dplyr::summarise(p = sum(.data$adj_proj, na.rm = TRUE)) %>%
      dplyr::pull(.data$p)
  }

  baseline   <- lineup_points(user_gsis)

  # Candidate available players (offense). Prefilter to those who could plausibly
  # crack the lineup: projection above the current worst starter at an eligible
  # slot. This prunes hundreds of free agents to a handful before full solves.
  starters_now <- pool %>%
    dplyr::filter(.data$nfl_gsis_id %in% user_gsis,
                  !.data$availability_status %in%
                    c("out_for_season", "out_week"))
  worst_start <- if (nrow(starters_now) == 0L) -Inf else
    min(starters_now$base_proj, na.rm = TRUE)

  candidates <- pool %>%
    dplyr::filter(.data$is_available,
                  .data$position %in% LINEUP_OFFENSE_POSITIONS,
                  .data$availability_status == "active",
                  .data$base_proj > worst_start)

  if (nrow(candidates) == 0L) {
    message("No available players project to improve the current lineup.")
    return(tibble::tibble(add_player = character(), add_team = character(),
                          add_position = character(), proj_gain = numeric(),
                          drop_player = character(), drop_value = numeric(),
                          format = character()))
  }

  # Drop candidate: lowest format-value roster player NOT in the current optimal
  # lineup (bench), falling back to lowest overall if the bench is empty.
  base_set <- pool %>% dplyr::filter(.data$nfl_gsis_id %in% user_gsis)
  base_startable <- base_set %>%
    dplyr::filter(!.data$availability_status %in%
                    c("out_for_season", "out_week"))
  base_adj <- .apply_matchup(base_startable, week_matchups, dvp, def_factors)
  base_assign <- .solve_lineup_ip(
    base_adj %>% dplyr::transmute(nfl_gsis_id, position,
                                  opt_value = .data$adj_proj),
    slot_caps
  )
  drop_pool <- base_set %>%
    dplyr::filter(!.data$nfl_gsis_id %in% base_assign$nfl_gsis_id)
  if (nrow(drop_pool) == 0L) drop_pool <- base_set
  drop_row <- drop_pool %>% dplyr::arrange(.data$value) %>% dplyr::slice(1)

  gains <- purrr::map_dfr(seq_len(nrow(candidates)), function(i) {
    cg <- candidates$nfl_gsis_id[i]
    new_pts <- lineup_points(setdiff(user_gsis, drop_row$nfl_gsis_id),
                             extra = cg)
    tibble::tibble(
      add_player   = candidates$player_name[i],
      add_team     = candidates$team[i],
      add_position = candidates$position[i],
      proj_gain    = round(new_pts - baseline, 2),
      drop_player  = drop_row$player_name %||% NA_character_,
      drop_value   = round(drop_row$value %||% NA_real_, 3),
      format       = format
    )
  })

  gains %>%
    dplyr::filter(.data$proj_gain > 0) %>%
    dplyr::arrange(dplyr::desc(.data$proj_gain)) %>%
    dplyr::slice_head(n = max_suggestions)
}

# ------------------------------------------------------------------------------
# print method for league_source
# ------------------------------------------------------------------------------

#' @export
print.league_source <- function(x, ...) {
  type_label <- switch(as.character(x$league_type %||% NA),
                       "0" = "redraft", "1" = "keeper", "2" = "dynasty",
                       "unknown")
  cat(glue("League Source: {x$config$league_name}\n"))
  cat(glue("  Platform:   {x$platform}\n"))
  cat(glue("  Format:     {x$format} (Sleeper type: {type_label})\n"))
  cat(glue("  Teams:      {x$config$num_teams}\n"))
  cat(glue("  DEF slots:  {x$def_slots}\n"))
  cat(glue("  Rostered:   {length(x$all_rostered_gsis)} players (gsis-matched)\n"))
  cat(glue("  Your roster:{length(x$user_roster_gsis)} players\n"))
  invisible(x)
}
