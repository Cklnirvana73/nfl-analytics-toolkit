# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 16
# DEF/ST Forward Projection
# File: R/34_def_st_projection.R
#
# PURPOSE
# -------
# Projects each team's 2026 DEF/ST fantasy PPG, in-season aware. This is the
# defense input the lineup optimizer (R/35) needs to slot a team defense, the
# DEF analogue of the offensive player projections that flow through R/29-R/32.
#
# This file does NOT re-implement DEF/ST scoring. R/29 already exports
# calculate_def_st_points(), which tallies per-game team defensive events
# (sacks, INTs, fumble recoveries, def/ST TDs, safeties, blocked kicks) and
# points allowed from pbp. R/34 consumes those raw per-game tallies, re-scores
# them under the league's actual scoring, aggregates to per-team PPG, and
# projects forward.
#
# DESIGN DECISIONS
# ----------------
#   - Reuse              : R/29 calculate_def_st_points() for raw per-game
#                          counts; R/29 compute_prior_weight() for the in-season
#                          decay. R/29 is not modified.
#   - League scoring     : pulled live from connect_sleeper_league() (R/19) and
#                          applied here. R/29's calculate_def_st_points() takes
#                          a scoring_settings argument but does NOT use it for
#                          DEF (it hardcodes the Sleeper-standard constants), so
#                          R/34 re-scores the raw counts itself. Standard
#                          scoring is the fallback when no league is connected.
#   - Prior              : equal-weight blend of the prior HISTORICAL_WINDOW
#                          seasons of team DEF PPG, regressed toward the league
#                          mean. Team defense is turnover-driven and turnover
#                          luck regresses hard year to year, so a meaningful
#                          pull toward the mean is the correct prior.
#   - In-season blend    : projected = prior_weight * prior
#                                     + (1 - prior_weight) * season_to_date,
#                          with prior_weight = compute_prior_weight(as_of_week),
#                          matching R/29 exactly. Teams with zero games played
#                          (preseason, or as_of_week = NULL) stay at pure prior,
#                          mirroring R/29's has_observed gate.
#   - Game state         : NO garbage-time filter. Fantasy DEF scoring counts
#                          every event regardless of win probability, unlike
#                          efficiency metrics. All regular-season plays count.
#
# OUTPUTS
# -------
#   data/season2_cache/s2_week16_def_st_projections.rds
#   data/season2_cache/s2_week16_def_st_projections.csv
#
# SOURCE DEPENDENCIES (sourced below if not already loaded)
# ---------------------------------------------------------
#   R/15_multi_season_pbp.R   -- load_normalized_season()
#   R/29_projection_engine.R  -- calculate_def_st_points(), compute_prior_weight(),
#                                DEF_PTS_ALLOW_TIERS, DEF_EVENT_POINTS
#   R/19_sleeper_api.R        -- connect_sleeper_league()
#
# RUN
# ---
#   source(here::here("R", "34_def_st_projection.R"))
#   def_proj <- project_def_st()                         # preseason, std scoring
#   def_proj <- project_def_st(as_of_week = 8)           # in-season, week 8
#   def_proj <- project_def_st(as_of_week = 8,
#                              league_id = "123456789")  # league scoring
#
# SCHEMA TAG: s2_w16_def_st_v1
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
library(here)
library(glue)

# Source dependencies only if not already in the session. Sourcing is
# idempotent (function definitions are redefined) and these guards avoid the
# heavy R/29 dependency chain when it is already loaded.
if (!exists("calculate_def_st_points") || !exists("compute_prior_weight")) {
  source(here::here("R", "29_projection_engine.R"))
}
if (!exists("load_normalized_season")) {
  source(here::here("R", "15_multi_season_pbp.R"))
}
if (!exists("connect_sleeper_league")) {
  source(here::here("R", "19_sleeper_api.R"))
}

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

SEASON <- 2026L
HISTORICAL_WINDOW <- 3L
PRIOR_SEASONS <- (SEASON - HISTORICAL_WINDOW):(SEASON - 1L)

# Shrinkage of the historical prior toward the league mean. Team defense is
# noisy season to season, so a substantial pull toward average is appropriate.
# 0 = no regression, 1 = full league mean.
#
# Calibrated empirically in tests/assumptions_season2_week16.R: pooled
# year-over-year correlation of team DEF PPG is only ~0.32 (weak stickiness),
# and a leave-one-season-out backtest over 2010-2025 finds RMSE roughly flat
# across 0.50-0.65. 0.50 sits at the front of that flat optimum, capturing
# nearly all the accuracy gain while compressing the preseason spread the
# least. Re-run the assumptions script if the prior window or scoring changes.
REGRESSION_STRENGTH <- 0.50

# Sleeper yards-allowed tier keys. Sleeper standard scoring does not award
# yardage-tier points (all zero), but leagues can configure them, so the keys
# are carried in the scoring config and parsed from league settings the same
# way as the pts_allow tiers. Boundaries follow Sleeper's key names:
# <100, 100-199, 200-299, 300-349, 350-399, 400-449, 450-499, 500-549, 550+.
DEF_YDS_ALLOW_TIERS_DEFAULT <- c(
  yds_allow_0_100   = 0,
  yds_allow_100_199 = 0,
  yds_allow_200_299 = 0,
  yds_allow_300_349 = 0,
  yds_allow_350_399 = 0,
  yds_allow_400_449 = 0,
  yds_allow_450_499 = 0,
  yds_allow_500_549 = 0,
  yds_allow_550p    = 0
)

# Default DEF/ST scoring (Sleeper standard), sourced from R/29's constants so
# there is a single source of truth for the default values. Shape: a list with
# named pts_allow and yds_allow tier vectors and a named events vector.
DEF_SCORING_DEFAULT <- list(
  pts_allow = unlist(DEF_PTS_ALLOW_TIERS),
  yds_allow = DEF_YDS_ALLOW_TIERS_DEFAULT,
  events    = unlist(DEF_EVENT_POINTS)
)

# Sleeper scoring_settings key candidates per event category. Different league
# configs use different key spellings; the first present key wins.
SLEEPER_DEF_EVENT_KEYS <- list(
  sack     = c("def_sack", "sack"),
  def_int  = c("def_int", "int"),
  fum_rec  = c("def_st_fum_rec", "fum_rec"),
  def_td   = c("def_td", "def_st_td"),
  safety   = c("safe", "safety"),
  blk_kick = c("blk_kick", "blocked_kick")
)

CACHE_DIR_DEFAULT <- here::here("data", "season2_cache")
OUTPUT_RDS_PATH <- here::here("data", "season2_cache",
                              "s2_week16_def_st_projections.rds")
OUTPUT_CSV_PATH <- here::here("data", "season2_cache",
                              "s2_week16_def_st_projections.csv")

SCHEMA_TAG <- "s2_w16_def_st_v1"

# ==============================================================================
# PURE HELPERS (no I/O -- unit tested)
# ==============================================================================

# ------------------------------------------------------------------------------
# .parse_def_scoring
# ------------------------------------------------------------------------------

#' Build a DEF scoring config from raw Sleeper scoring_settings
#'
#' Reads the points-allowed tiers, yards-allowed tiers, and event point values
#' from a league's raw Sleeper scoring_settings, falling back to the supplied
#' default for any key not present. Event categories tolerate multiple Sleeper
#' key spellings via SLEEPER_DEF_EVENT_KEYS (first present wins).
#'
#' @param scoring_settings Named list. Raw Sleeper scoring_settings, or NULL.
#' @param default List. Fallback config, shape list(pts_allow, yds_allow,
#'   events).
#' @return List with named numeric vectors pts_allow, yds_allow, and events.
#' @keywords internal
.parse_def_scoring <- function(scoring_settings, default = DEF_SCORING_DEFAULT) {
  # Tolerate a default built before yds_allow existed (e.g. a test fixture).
  if (is.null(default$yds_allow)) {
    default$yds_allow <- DEF_YDS_ALLOW_TIERS_DEFAULT
  }
  if (is.null(scoring_settings) || length(scoring_settings) == 0L) {
    return(default)
  }

  pa <- default$pts_allow
  for (k in names(pa)) {
    if (!is.null(scoring_settings[[k]])) {
      pa[[k]] <- as.numeric(scoring_settings[[k]])
    }
  }

  ya <- default$yds_allow
  for (k in names(ya)) {
    if (!is.null(scoring_settings[[k]])) {
      ya[[k]] <- as.numeric(scoring_settings[[k]])
    }
  }

  ev <- default$events
  for (cat in names(SLEEPER_DEF_EVENT_KEYS)) {
    cand <- SLEEPER_DEF_EVENT_KEYS[[cat]]
    hit  <- cand[cand %in% names(scoring_settings)]
    if (length(hit) > 0L) {
      ev[[cat]] <- as.numeric(scoring_settings[[hit[1]]])
    }
  }

  list(pts_allow = pa, yds_allow = ya, events = ev)
}

# ------------------------------------------------------------------------------
# .def_pts_allowed_to_points
# ------------------------------------------------------------------------------

#' Map points allowed to tier points under a supplied tier vector
#'
#' Vectorized. League-parameterized version of R/29's internal
#' .def_st_pts_allowed_points (which is hardcoded to standard tiers).
#'
#' @param pts_allowed Numeric vector of points allowed.
#' @param tiers Named numeric vector with the seven pts_allow_* keys.
#' @return Numeric vector of tier points (NA where pts_allowed is NA).
#' @keywords internal
.def_pts_allowed_to_points <- function(pts_allowed, tiers) {
  dplyr::case_when(
    is.na(pts_allowed)  ~ NA_real_,
    pts_allowed == 0    ~ tiers[["pts_allow_0"]],
    pts_allowed <= 6    ~ tiers[["pts_allow_1_6"]],
    pts_allowed <= 13   ~ tiers[["pts_allow_7_13"]],
    pts_allowed <= 20   ~ tiers[["pts_allow_14_20"]],
    pts_allowed <= 27   ~ tiers[["pts_allow_21_27"]],
    pts_allowed <= 34   ~ tiers[["pts_allow_28_34"]],
    TRUE                ~ tiers[["pts_allow_35p"]]
  )
}

# ------------------------------------------------------------------------------
# .def_yds_allowed_to_points
# ------------------------------------------------------------------------------

#' Map total yards allowed to tier points under a supplied tier vector
#'
#' Vectorized, mirroring .def_pts_allowed_to_points for the yds_allow_* family.
#' Tier boundaries follow Sleeper's key names (yds_allow_0_100 = under 100,
#' ..., yds_allow_550p = 550 or more).
#'
#' @param yds_allowed Numeric vector of total yards allowed.
#' @param tiers Named numeric vector with the nine yds_allow_* keys.
#' @return Numeric vector of tier points (NA where yds_allowed is NA).
#' @keywords internal
.def_yds_allowed_to_points <- function(yds_allowed, tiers) {
  dplyr::case_when(
    is.na(yds_allowed)  ~ NA_real_,
    yds_allowed < 100   ~ tiers[["yds_allow_0_100"]],
    yds_allowed < 200   ~ tiers[["yds_allow_100_199"]],
    yds_allowed < 300   ~ tiers[["yds_allow_200_299"]],
    yds_allowed < 350   ~ tiers[["yds_allow_300_349"]],
    yds_allowed < 400   ~ tiers[["yds_allow_350_399"]],
    yds_allowed < 450   ~ tiers[["yds_allow_400_449"]],
    yds_allowed < 500   ~ tiers[["yds_allow_450_499"]],
    yds_allowed < 550   ~ tiers[["yds_allow_500_549"]],
    TRUE                ~ tiers[["yds_allow_550p"]]
  )
}

# ------------------------------------------------------------------------------
# .score_def_games
# ------------------------------------------------------------------------------

#' Re-score per-game DEF/ST event counts under a supplied scoring config
#'
#' Takes the raw per-game tallies returned by calculate_def_st_points() and
#' applies the league's scoring config. Replaces the standard-scoring
#' def_st_points column with a league-specific one.
#'
#' @param counts Tibble from calculate_def_st_points() with columns
#'   opponent_pts_allowed, sacks, def_ints, fum_recs, def_tds, safeties,
#'   blocked_kicks (plus identifiers), optionally augmented with
#'   opponent_yds_allowed (see .compute_yds_allowed).
#' @param scoring List config, shape list(pts_allow, yds_allow, events).
#' @return The input tibble with pts_allow_points, yds_allow_points,
#'   def_event_points, and def_st_points recomputed under the supplied
#'   scoring.
#' @keywords internal
.score_def_games <- function(counts, scoring) {
  ev <- scoring$events
  ya <- scoring$yds_allow %||% DEF_YDS_ALLOW_TIERS_DEFAULT
  has_yds_tiers <- any(ya != 0)
  has_yds_data  <- "opponent_yds_allowed" %in% names(counts)

  if (has_yds_tiers && !has_yds_data) {
    warning(
      "DEF scoring has non-zero yds_allow_* tiers but the per-game counts ",
      "carry no opponent_yds_allowed column -- yardage-tier points are NOT ",
      "applied.",
      call. = FALSE
    )
  }

  counts %>%
    dplyr::mutate(
      pts_allow_points = .def_pts_allowed_to_points(
        .data$opponent_pts_allowed, scoring$pts_allow
      ),
      yds_allow_points = if (has_yds_tiers && has_yds_data) {
        dplyr::coalesce(
          .def_yds_allowed_to_points(.data$opponent_yds_allowed, ya), 0
        )
      } else {
        0
      },
      def_event_points =
        .data$sacks         * ev[["sack"]] +
        .data$def_ints      * ev[["def_int"]] +
        .data$fum_recs      * ev[["fum_rec"]] +
        .data$def_tds       * ev[["def_td"]] +
        .data$safeties      * ev[["safety"]] +
        .data$blocked_kicks * ev[["blk_kick"]],
      def_st_points = .data$pts_allow_points + .data$yds_allow_points +
        .data$def_event_points
    )
}

# ------------------------------------------------------------------------------
# .regress_to_mean
# ------------------------------------------------------------------------------

#' Shrink values toward a target by a given strength
#'
#' @param values Numeric vector.
#' @param target Numeric scalar (e.g., the league mean).
#' @param strength Numeric in [0, 1]. 0 = unchanged, 1 = fully the target.
#' @return Numeric vector shrunk toward target.
#' @keywords internal
.regress_to_mean <- function(values, target, strength) {
  (1 - strength) * values + strength * target
}

# ------------------------------------------------------------------------------
# .blend_prior_observed
# ------------------------------------------------------------------------------

#' Blend a prior with season-to-date observed by the prior weight
#'
#' Mirrors R/29's update_projection_with_ytd mean logic: teams with observed
#' data blend prior_weight * prior + (1 - prior_weight) * observed; teams with
#' no observed data (has_observed FALSE) keep the prior unchanged.
#'
#' @param prior Numeric vector of prior PPG.
#' @param observed Numeric vector of season-to-date PPG (may be NA where none).
#' @param prior_weight Numeric scalar from compute_prior_weight().
#' @param has_observed Logical vector, TRUE where the team has played games.
#' @return Numeric vector of blended projections.
#' @keywords internal
.blend_prior_observed <- function(prior, observed, prior_weight, has_observed) {
  dplyr::if_else(
    has_observed,
    prior_weight * prior + (1 - prior_weight) * dplyr::coalesce(observed, 0),
    prior
  )
}

# ==============================================================================
# I/O AND AGGREGATION
# ==============================================================================

# ------------------------------------------------------------------------------
# .load_league_def_scoring
# ------------------------------------------------------------------------------

#' Resolve the DEF scoring config for a league (or the standard default)
#'
#' @param league_id Character or NULL. Sleeper league id. NULL uses standard.
#' @return List: scoring (config) and source (character label).
#' @keywords internal
.load_league_def_scoring <- function(league_id = NULL) {
  if (is.null(league_id)) {
    return(list(scoring = DEF_SCORING_DEFAULT, source = "sleeper_standard_default"))
  }

  league <- tryCatch(
    connect_sleeper_league(league_id),
    error = function(e) {
      message(glue("  Sleeper league pull failed ({e$message}) -- ",
                   "falling back to standard DEF scoring"))
      NULL
    }
  )

  if (is.null(league) || length(league$scoring_settings) == 0L) {
    return(list(scoring = DEF_SCORING_DEFAULT, source = "sleeper_standard_default"))
  }

  list(
    scoring = .parse_def_scoring(league$scoring_settings),
    source  = glue("sleeper_league_{league_id}")
  )
}

# ------------------------------------------------------------------------------
# .compute_yds_allowed
# ------------------------------------------------------------------------------

#' Per-team total yards allowed per game, computed from pbp
#'
#' calculate_def_st_points() (R/29) does not tally yards allowed, so R/34
#' derives it here from the same pbp used for points allowed: total yards
#' gained by the opposing offense on rush/pass plays (sacks count as negative
#' passing yards, matching official team-total-yards convention). Returns NULL
#' when the pbp lacks the needed columns, in which case .score_def_games()
#' warns if the league actually scores yardage tiers.
#'
#' @param pbp Play-by-play tibble (one season or season-to-date).
#' @return Tibble: season, week, game_id, team, opponent_yds_allowed; or NULL.
#' @keywords internal
.compute_yds_allowed <- function(pbp) {
  needed <- c("season_type", "season", "week", "game_id", "defteam",
              "posteam", "yards_gained")
  if (!all(needed %in% names(pbp))) return(NULL)

  pbp_use <- pbp %>%
    dplyr::filter(.data$season_type == "REG",
                   !is.na(.data$defteam), !is.na(.data$posteam))

  if (all(c("rush", "pass") %in% names(pbp_use))) {
    pbp_use <- dplyr::filter(
      pbp_use,
      dplyr::coalesce(.data$rush, 0L) == 1L |
        dplyr::coalesce(.data$pass, 0L) == 1L
    )
  } else if ("play_type" %in% names(pbp_use)) {
    pbp_use <- dplyr::filter(pbp_use,
                              .data$play_type %in% c("run", "pass"))
  }

  pbp_use %>%
    dplyr::group_by(.data$season, .data$week, .data$game_id, .data$defteam) %>%
    dplyr::summarise(
      opponent_yds_allowed = sum(.data$yards_gained, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(team = defteam)
}

# ------------------------------------------------------------------------------
# .compute_team_def_ppg
# ------------------------------------------------------------------------------

#' Per-team DEF/ST PPG for a single pbp tibble under a scoring config
#'
#' @param pbp Play-by-play tibble (one season or season-to-date).
#' @param scoring List scoring config.
#' @return Tibble: team, def_ppg, n_games.
#' @keywords internal
.compute_team_def_ppg <- function(pbp, scoring) {
  games <- calculate_def_st_points(pbp)
  if (nrow(games) == 0L) {
    return(tibble::tibble(team = character(), def_ppg = numeric(),
                          n_games = integer()))
  }

  # Attach yards allowed so leagues with yds_allow_* tiers score correctly.
  yds <- .compute_yds_allowed(pbp)
  if (!is.null(yds)) {
    games <- dplyr::left_join(games, yds,
                              by = c("season", "week", "game_id", "team"))
  }

  .score_def_games(games, scoring) %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      def_ppg = mean(.data$def_st_points, na.rm = TRUE),
      n_games = dplyr::n(),
      .groups = "drop"
    )
}

# ------------------------------------------------------------------------------
# .compute_def_prior
# ------------------------------------------------------------------------------

#' Historical DEF PPG prior per team, blended across seasons and regressed
#'
#' @param seasons Integer vector of prior seasons.
#' @param scoring List scoring config.
#' @param cache_dir Character. R/15 cache directory.
#' @param regression_strength Numeric shrinkage toward the league mean.
#' @return Tibble: team, prior_raw_ppg, seasons_used, def_prior_ppg.
#' @keywords internal
.compute_def_prior <- function(seasons, scoring, cache_dir,
                               regression_strength = REGRESSION_STRENGTH) {

  per_season <- purrr::map_dfr(seasons, function(s) {
    pbp <- load_normalized_season(s, cache_dir = cache_dir)
    .compute_team_def_ppg(pbp, scoring) %>% dplyr::mutate(season = s)
  })

  blended <- per_season %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      prior_raw_ppg = mean(.data$def_ppg, na.rm = TRUE),
      seasons_used  = dplyr::n(),
      .groups       = "drop"
    )

  league_mean <- mean(blended$prior_raw_ppg, na.rm = TRUE)

  blended %>%
    dplyr::mutate(
      def_prior_ppg = .regress_to_mean(.data$prior_raw_ppg, league_mean,
                                       regression_strength)
    )
}

# ------------------------------------------------------------------------------
# .compute_def_observed
# ------------------------------------------------------------------------------

#' Season-to-date DEF PPG per team through a given week (NULL if preseason)
#'
#' @param season Integer. Target season.
#' @param as_of_week Integer or NULL. Most recent completed week.
#' @param scoring List scoring config.
#' @param cache_dir Character. R/15 cache directory.
#' @return Tibble: team, def_observed_ppg, n_observed_games; or NULL if there
#'   is no season-to-date data (preseason or no cached current-season pbp).
#' @keywords internal
.compute_def_observed <- function(season, as_of_week, scoring, cache_dir) {
  if (is.null(as_of_week) || as_of_week < 1L) return(NULL)

  pbp <- tryCatch(
    load_normalized_season(season, cache_dir = cache_dir),
    error = function(e) {
      message(glue("  Current-season pbp unavailable ({e$message}) -- ",
                   "projection will use prior only"))
      NULL
    }
  )
  if (is.null(pbp) || nrow(pbp) == 0L) return(NULL)

  pbp <- dplyr::filter(pbp, .data$week <= as_of_week)
  if (nrow(pbp) == 0L) return(NULL)

  .compute_team_def_ppg(pbp, scoring) %>%
    dplyr::rename(def_observed_ppg = def_ppg, n_observed_games = n_games)
}

# ==============================================================================
# PUBLIC ENTRY POINT
# ==============================================================================

# ------------------------------------------------------------------------------
# project_def_st
# ------------------------------------------------------------------------------

#' Project 2026 DEF/ST fantasy PPG per team, in-season aware
#'
#' Blends a regressed multi-season historical prior with season-to-date DEF PPG
#' using R/29's compute_prior_weight() decay. Preseason (as_of_week = NULL) or
#' teams with no games played stay at the pure prior.
#'
#' @param season Integer. Target season (default SEASON).
#' @param as_of_week Integer or NULL. Most recent completed week; NULL = preseason.
#' @param league_id Character or NULL. Sleeper league id for scoring; NULL =
#'   Sleeper standard scoring.
#' @param prior_seasons Integer vector. Seasons for the historical prior.
#' @param regression_strength Numeric. Shrinkage of the prior toward the mean.
#' @param scoring List or NULL. Explicit scoring config; overrides league_id
#'   when supplied (mainly for testing).
#' @param cache_dir Character. R/15 cache directory.
#' @param save_output Logical. Write RDS and CSV when TRUE.
#' @return Tibble, one row per team, sorted by def_proj_ppg descending.
#' @seealso calculate_def_st_points, compute_prior_weight (R/29);
#'   connect_sleeper_league (R/19)
#' @export
project_def_st <- function(season = SEASON,
                           as_of_week = NULL,
                           league_id = NULL,
                           prior_seasons = PRIOR_SEASONS,
                           regression_strength = REGRESSION_STRENGTH,
                           scoring = NULL,
                           cache_dir = CACHE_DIR_DEFAULT,
                           save_output = TRUE) {

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/34: Projecting DEF/ST PPG for season {season}"))
  wk_label <- if (is.null(as_of_week)) "preseason" else glue("through week {as_of_week}")
  message(glue("As of: {wk_label}"))
  message(glue("{strrep('=', 70)}"))

  # Resolve scoring (explicit config wins; else league pull; else standard)
  if (is.null(scoring)) {
    resolved <- .load_league_def_scoring(league_id)
    scoring        <- resolved$scoring
    scoring_source <- resolved$source
  } else {
    scoring_source <- "explicit"
  }
  message(glue("  Scoring source: {scoring_source}"))

  # Prior
  message(glue("\nSTEP 1/3: Building historical prior ",
               "({paste(prior_seasons, collapse = ', ')})"))
  prior <- .compute_def_prior(prior_seasons, scoring, cache_dir,
                              regression_strength = regression_strength)
  message(glue("  Prior built for {nrow(prior)} teams"))

  # Observed season-to-date
  message("\nSTEP 2/3: Loading season-to-date observed")
  observed <- .compute_def_observed(season, as_of_week, scoring, cache_dir)

  prior_weight <- if (is.null(as_of_week) || as_of_week < 1L) {
    1.0
  } else {
    compute_prior_weight(as_of_week)
  }
  message(glue("  Prior weight: {format(round(prior_weight, 3), nsmall = 3)} ",
               "(observed weight {format(round(1 - prior_weight, 3), nsmall = 3)})"))

  # Blend
  message("\nSTEP 3/3: Blending prior and observed")
  out <- prior
  if (!is.null(observed)) {
    out <- dplyr::left_join(out, observed, by = "team")
  } else {
    out <- dplyr::mutate(out, def_observed_ppg = NA_real_,
                         n_observed_games = 0L)
  }

  out <- out %>%
    dplyr::mutate(
      n_observed_games = dplyr::coalesce(.data$n_observed_games, 0L),
      has_observed     = .data$n_observed_games > 0L,
      prior_weight     = prior_weight,
      def_proj_ppg     = .blend_prior_observed(
        .data$def_prior_ppg, .data$def_observed_ppg,
        prior_weight, .data$has_observed
      ),
      season         = season,
      as_of_week     = if (is.null(as_of_week)) NA_integer_ else as.integer(as_of_week),
      scoring_source = as.character(scoring_source),
      schema_tag     = SCHEMA_TAG
    ) %>%
    dplyr::select(
      season, as_of_week, team, def_prior_ppg, def_observed_ppg,
      n_observed_games, prior_weight, def_proj_ppg, scoring_source, schema_tag
    ) %>%
    dplyr::arrange(dplyr::desc(def_proj_ppg))

  if (save_output) {
    dir.create(dirname(OUTPUT_RDS_PATH), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, OUTPUT_RDS_PATH)
    readr::write_csv(out, OUTPUT_CSV_PATH)
    message(glue("\n  Saved: {OUTPUT_RDS_PATH}"))
    message(glue("  Saved: {OUTPUT_CSV_PATH}"))
  }

  # KEY INSIGHTS (computed from output, never hardcoded)
  n_teams     <- nrow(out)
  n_observed  <- sum(out$n_observed_games > 0L, na.rm = TRUE)
  league_avg  <- mean(out$def_proj_ppg, na.rm = TRUE)
  top_team    <- out$team[which.max(out$def_proj_ppg)]
  top_ppg     <- max(out$def_proj_ppg, na.rm = TRUE)
  bot_team    <- out$team[which.min(out$def_proj_ppg)]
  bot_ppg     <- min(out$def_proj_ppg, na.rm = TRUE)

  message(glue("\n{strrep('=', 70)}"))
  message("KEY INSIGHTS")
  message(glue("{strrep('=', 70)}"))
  message(glue("  Teams projected:          {n_teams}"))
  message(glue("  Teams with observed data: {n_observed}"))
  message(glue("  League avg DEF PPG:       {format(round(league_avg, 2), nsmall = 2)}"))
  message(glue("  Highest projected:        {top_team} at ",
               "{format(round(top_ppg, 2), nsmall = 2)} PPG"))
  message(glue("  Lowest projected:         {bot_team} at ",
               "{format(round(bot_ppg, 2), nsmall = 2)} PPG"))
  message(glue("{strrep('=', 70)}\n"))

  out
}
