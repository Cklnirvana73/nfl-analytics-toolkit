# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 9
# Aging Curves: Production Trajectory by Position and Age
# File: R/23_aging_curves.R
#
# PURPOSE
# -------
# Builds position-specific aging curves using the delta method to eliminate
# survivor bias. Older age buckets in raw averages only contain players who
# lasted long enough to be above average -- making aging look better than it
# is. The delta method corrects this by tracking year-over-year change per
# individual player and averaging the changes, not the levels.
#
# Two parallel analyses are run and compared:
#   (1) Box score production curves  -- 2010-2025, 16 seasons
#   (2) NGS efficiency metric curves -- 2016-2025, 10 seasons
#
# The comparison tests whether tracking data efficiency declines BEFORE
# box score production falls off -- an early-warning signal for dynasty
# and keeper leagues.
#
# DESIGN DECISIONS
# ----------------
# Age definition  : September 1 of season year (integer; NFL convention)
# Survivor bias   : Eliminated via delta method (YoY changes per player)
# Curve fitting   : Quadratic polynomial AND LOESS -- both fit and compared
# Positions       : QB, RB, WR, TE -- all four separately
# Sample size     : Permissive -- include all players; low_volume retained
# Min obs per age : MIN_AGE_OBS = 5L (flag, not drop)
#   Min career seasons: MIN_CAREER_SEASONS = 4L (mirrors rookie contract length;
#                       excludes players who washed out before establishing
#                       themselves as viable NFL contributors)
# NGS data source : nflreadr::load_nextgen_stats() (2016+; free; no ToS risk)
# NGS metrics     : CPOE (QB), separation (WR/TE), RYOE per att (RB)
#
# NAVIGATION
# ----------
#   Line  ~75  : Libraries and constants
#   Line  ~120 : compute_player_ages()
#   Line  ~225 : compute_season_ppg()
#   Line  ~330 : compute_age_deltas()
#   Line  ~460 : load_ngs_season_panel()
#   Line  ~590 : fit_aging_curves()
#   Line  ~720 : validate_aging_assumptions()
#   Line  ~840 : run_aging_curve_pipeline()
#   Line  ~985 : .plot_curves_boxscore()
#   Line ~1050 : .plot_age_deltas()
#   Line ~1120 : .plot_ngs_vs_boxscore()
#
# DEPENDENCIES
# ------------
#   R/15_multi_season_pbp.R   -- load_normalized_season()
#   R/16_player_season_panel.R -- build_player_season_panel()
#   nflreadr: load_rosters(), load_nextgen_stats()
#   dplyr, purrr, tidyr, ggplot2, glue, here
#
# DATA SOURCES
# ------------
#   R/16 player-season panel : box score stats, 2010-2025
#   nflreadr::load_rosters() : birth_date for age computation
#   nflreadr::load_nextgen_stats() : NGS efficiency metrics, 2016-2025
# ==============================================================================


# ==============================================================================
# LIBRARIES
# ==============================================================================

library(nflreadr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(glue)
library(here)


# ==============================================================================
# CONSTANTS AND CONFIGURATION
# ==============================================================================

# Season ranges
PANEL_SEASONS     <- 2010:2025   # Full box score panel range
NGS_SEASONS       <- 2016:2025   # NGS coverage starts 2016

# Age configuration (September 1 convention)
AGE_BASELINE      <- 23L         # Anchor age for curve reconstruction
AGE_MIN           <- 21L         # Minimum age to include in curves
AGE_MAX           <- 38L         # Maximum age to include in curves
MIN_AGE_OBS       <- 5L          # Minimum player transitions per age bucket
MIN_CAREER_SEASONS <- 4L         # Minimum seasons in panel to include a player's deltas
                                  # Mirrors the NFL rookie contract length -- players who
                                  # wash out before 4 seasons represent roster turnover,
                                  # not the aging trajectory of viable NFL contributors

# Positions in scope
CURVE_POSITIONS   <- c("QB", "RB", "WR", "TE")

# LOESS smoothing parameter (0.75 = moderate smoothing; appropriate for ~15 age pts)
LOESS_SPAN        <- 0.75

# Output directory and file prefix
OUTPUT_DIR        <- here::here("output", "plots")
FILE_PREFIX       <- "s2_week9_"

# Cache directory (must be populated by R/15 before running)
CACHE_DIR         <- here::here("data", "season2_cache")


# ==============================================================================
# FUNCTION: compute_player_ages
# ==============================================================================

#' Compute Player Age at Season Start (September 1 Convention)
#'
#' @description
#' Joins birth dates from nflreadr rosters and computes each player's integer
#' age as of September 1 of their season year. The September 1 convention is
#' standard in NFL aging curve literature: it assigns a single consistent age
#' per player per season regardless of birth month, enabling clean year-to-year
#' comparisons and reproducible joins.
#'
#' @param panel Tibble. Output of \code{build_player_season_panel()}. Must
#'   contain \code{player_id} and \code{season} columns.
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return The input panel with one additional column:
#'   \describe{
#'     \item{age_at_season_start}{Integer. Player age as of September 1 of the
#'       season year. NA when birth_date is unavailable in nflreadr rosters.}
#'   }
#'
#' @details
#' Birth dates come from nflreadr::load_rosters(). Some historical players --
#' especially specialists and players from 2010-2013 -- have no roster match
#' and will receive NA. These rows are excluded from delta calculations.
#'
#' When a player appears in multiple roster seasons with conflicting birth dates
#' (a data quality issue), the first non-NA value is used.
#'
#' @seealso \code{\link{compute_age_deltas}}, \code{\link{run_aging_curve_pipeline}}
#'
#' @examples
#' \dontrun{
#' panel <- build_player_season_panel(seasons = 2020:2024)
#' panel_with_ages <- compute_player_ages(panel)
#' summary(panel_with_ages$age_at_season_start)
#' }
#'
#' @export
compute_player_ages <- function(panel, verbose = TRUE) {

  if (!is.data.frame(panel) || nrow(panel) == 0L) {
    stop("panel must be a non-empty data frame from build_player_season_panel().")
  }
  if (!all(c("player_id", "season") %in% names(panel))) {
    stop("panel must contain player_id and season columns.")
  }

  if (verbose) message("compute_player_ages(): Loading rosters for birth dates...")

  seasons_needed <- sort(unique(panel$season))

  rosters_raw <- tryCatch(
    nflreadr::load_rosters(seasons = seasons_needed),
    error = function(e) {
      stop(glue(
        "Failed to load nflreadr rosters: {conditionMessage(e)}\n",
        "Ensure nflreadr is installed and internet access is available."
      ))
    }
  )

  # Verify required columns exist
  if (!"gsis_id" %in% names(rosters_raw)) {
    stop("nflreadr::load_rosters() did not return a gsis_id column. Check nflreadr version.")
  }
  if (!"birth_date" %in% names(rosters_raw)) {
    stop("nflreadr::load_rosters() did not return a birth_date column. Check nflreadr version.")
  }

  # One birth_date per player (gsis_id). Take first non-NA when multiple
  # seasons of roster data disagree (data quality edge case).
  dob_lookup <- rosters_raw %>%
    dplyr::filter(!is.na(gsis_id), !is.na(birth_date)) %>%
    dplyr::arrange(gsis_id, season) %>%
    dplyr::group_by(gsis_id) %>%
    dplyr::summarise(birth_date = dplyr::first(birth_date), .groups = "drop") %>%
    dplyr::rename(player_id = gsis_id)

  rm(rosters_raw)
  gc()

  # Compute age as of September 1 of season year
  panel_with_ages <- panel %>%
    dplyr::left_join(dob_lookup, by = "player_id") %>%
    dplyr::mutate(
      sep1_date          = as.Date(paste0(season, "-09-01")),
      birth_date_parsed  = as.Date(birth_date),
      age_at_season_start = as.integer(
        floor(as.numeric(sep1_date - birth_date_parsed) / 365.25)
      )
    ) %>%
    dplyr::select(-sep1_date, -birth_date_parsed, -birth_date)

  n_with_age    <- sum(!is.na(panel_with_ages$age_at_season_start))
  n_without_age <- sum(is.na(panel_with_ages$age_at_season_start))
  pct_covered   <- round(n_with_age / nrow(panel_with_ages) * 100, 1)

  if (verbose) {
    message(glue(
      "compute_player_ages(): Complete.\n",
      "  {format(n_with_age, big.mark = ',')} player-seasons with age ({pct_covered}%)\n",
      "  {format(n_without_age, big.mark = ',')} player-seasons missing birth_date (will be NA)"
    ))
  }

  panel_with_ages
}



# ==============================================================================
# FUNCTION: split_wr_te_positions
# ==============================================================================

#' Split WR_TE Position Group into Separate WR and TE Groups
#'
#' @description
#' R/16 classifies wide receivers and tight ends into a combined \code{WR_TE}
#' position group for panel construction. Aging curves require them to be
#' separate because WR and TE production peaks at different ages and declines
#' at different rates. This function loads nflreadr rosters to retrieve
#' individual position values and recodes \code{position_group} accordingly.
#'
#' This function is called automatically inside
#' \code{run_aging_curve_pipeline()} at Step 3b. It can also be called
#' independently on any panel before passing it to \code{compute_age_deltas()}.
#'
#' @details
#' nflreadr caches \code{load_rosters()} internally. When called a second time
#' in the same R session (after \code{compute_player_ages()} already called it),
#' the roster data is served from the package cache -- no additional network
#' request is made.
#'
#' Rows where \code{position_group == "WR_TE"} but the individual position
#' is neither WR nor TE (e.g., FB, H-back) retain the \code{WR_TE} label.
#' These are not dropped -- they simply do not match \code{CURVE_POSITIONS}
#' and are excluded from delta calculations naturally.
#'
#' @param panel Tibble. Panel from \code{build_player_season_panel()}.
#'   Must contain \code{player_id}, \code{season}, and \code{position_group}.
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return Input panel with \code{position_group} recoded: rows that were
#'   \code{WR_TE} are now \code{WR} or \code{TE} where the individual
#'   roster position confirms the distinction.
#'
#' @seealso \code{\link{run_aging_curve_pipeline}},
#'   \code{\link{compute_age_deltas}}
#'
#' @examples
#' \dontrun{
#' panel <- build_player_season_panel(seasons = 2020:2025)
#' panel <- compute_player_ages(panel)
#' panel <- split_wr_te_positions(panel)
#' table(panel$position_group)
#' }
#'
#' @export
split_wr_te_positions <- function(panel, verbose = TRUE) {

  if (!is.data.frame(panel) || nrow(panel) == 0L) {
    stop("panel must be a non-empty data frame.")
  }
  required <- c("player_id", "season", "position_group")
  missing_cols <- setdiff(required, names(panel))
  if (length(missing_cols) > 0L) {
    stop(glue("panel is missing columns: {paste(missing_cols, collapse = ', ')}"))
  }

  n_wt_before <- sum(panel$position_group == "WR_TE", na.rm = TRUE)

  if (n_wt_before == 0L) {
    if (verbose) message("split_wr_te_positions(): No WR_TE rows found -- panel already split.")
    return(panel)
  }

  seasons_needed <- sort(unique(panel$season))

  if (verbose) {
    message(glue(
      "split_wr_te_positions(): Splitting {format(n_wt_before, big.mark=',')} ",
      "WR_TE rows into WR and TE...
",
      "  (nflreadr caches load_rosters() -- second call in session is free)"
    ))
  }

  rosters_raw <- tryCatch(
    nflreadr::load_rosters(seasons = seasons_needed),
    error = function(e) {
      warning(glue(
        "split_wr_te_positions(): Could not load rosters: {conditionMessage(e)}\n",
        "WR_TE rows retained unsplit."
      ))
      return(NULL)
    }
  )

  if (is.null(rosters_raw)) return(panel)

  if (!"gsis_id" %in% names(rosters_raw) || !"position" %in% names(rosters_raw)) {
    warning("split_wr_te_positions(): Rosters missing gsis_id or position column. WR_TE retained.")
    return(panel)
  }

  # One individual position per player per season.
  # Use the position column (WR/TE) not position_group (WR_TE).
  pos_lookup <- rosters_raw %>%
    dplyr::filter(!is.na(gsis_id), position %in% c("WR", "TE")) %>%
    dplyr::distinct(player_id = gsis_id, season, individual_position = position)

  rm(rosters_raw)
  gc()

  panel_split <- panel %>%
    dplyr::left_join(pos_lookup, by = c("player_id", "season")) %>%
    dplyr::mutate(
      position_group = dplyr::case_when(
        position_group == "WR_TE" & individual_position == "WR" ~ "WR",
        position_group == "WR_TE" & individual_position == "TE" ~ "TE",
        TRUE ~ position_group
      )
    ) %>%
    dplyr::select(-individual_position)

  n_wr  <- sum(panel_split$position_group == "WR",    na.rm = TRUE)
  n_te  <- sum(panel_split$position_group == "TE",    na.rm = TRUE)
  n_wt  <- sum(panel_split$position_group == "WR_TE", na.rm = TRUE)

  if (verbose) {
    message(glue(
      "split_wr_te_positions(): Complete.
",
      "  WR: {format(n_wr, big.mark = ',')} | ",
      "TE: {format(n_te, big.mark = ',')} | ",
      "WR_TE remaining: {format(n_wt, big.mark = ',')} (FB/H-back/unmatched)"
    ))
  }

  panel_split
}

# ==============================================================================
# FUNCTION: compute_season_ppg
# ==============================================================================

#' Compute PPR Fantasy Points Per Game from Panel Stats
#'
#' @description
#' DEPRECATED [2026-07-15]. Use \code{run_aging_curve_pipeline(scoring_settings
#' = ...)}, which routes play-by-play through R/17 via R/16's
#' \code{attach_scored_ppg()}.
#'
#' Retained ONLY so that the aging curves underlying the 2026-07-15 value boards
#' can be reproduced exactly for A/B comparison against the scored path. Delete
#' once that diff is complete.
#'
#' Why it is deprecated: the eight terms below are hardcoded. They happen to
#' match DK Best Ball on every term the season panel can support, but the
#' function is structurally scoring-blind. It cannot represent fumbles,
#' pick-sixes, first downs, or two-point conversions (those columns are absent
#' from the R/16 panel entirely), nor per-game threshold bonuses or tiered PPR
#' (irreducibly game- or play-level), nor TE premium, rush attempt bonus, sack
#' penalty, or superflex (implementable here, but never wired up).
#'
#' Calculates season-level PPR fantasy points per game directly from the
#' player-season panel columns. This keeps R/23 self-contained without
#' requiring a re-run of the play-level R/17 scoring engine.
#'
#' Standard scoring applied:
#' \itemize{
#'   \item Passing yards: 0.04 per yard (1 pt per 25 yds)
#'   \item Passing TDs: 4 points
#'   \item Interceptions: -1 point
#'   \item Rushing yards: 0.1 per yard (1 pt per 10 yds)
#'   \item Rushing TDs: 6 points
#'   \item Receiving yards: 0.1 per yard
#'   \item Receiving TDs: 6 points
#'   \item Receptions (PPR): 1 point per reception
#' }
#'
#' @param panel Tibble. Output of \code{build_player_season_panel()}.
#' @param ppr_value Numeric. Points per reception. Default 1 (full PPR).
#'   Use 0.5 for half-PPR, 0 for standard.
#'
#' @return Input panel with two additional columns:
#'   \describe{
#'     \item{season_fp_ppr}{Numeric. Total PPR fantasy points for the season.}
#'     \item{fp_per_game}{Numeric. PPR fantasy points per game played.
#'       NA when games_played is 0 or NA.}
#'   }
#'
#' @seealso \code{\link{compute_age_deltas}}
#'
#' @export
compute_season_ppg <- function(panel, ppr_value = 1) {

  if (!is.data.frame(panel) || nrow(panel) == 0L) {
    stop("panel must be a non-empty data frame.")
  }

  required <- c(
    "passing_yards", "pass_tds", "interceptions_thrown",
    "rushing_yards", "rush_tds",
    "receiving_yards", "rec_tds", "receptions",
    "games_played"
  )
  missing_cols <- setdiff(required, names(panel))
  if (length(missing_cols) > 0L) {
    stop(glue(
      "panel is missing columns: {paste(missing_cols, collapse = ', ')}.\n",
      "Ensure this came from build_player_season_panel()."
    ))
  }

  if (!is.numeric(ppr_value) || length(ppr_value) != 1L) {
    stop("ppr_value must be a single numeric value (e.g., 1, 0.5, or 0).")
  }

  panel %>%
    dplyr::mutate(
      season_fp_ppr =
        dplyr::coalesce(passing_yards,       0) * 0.04  +
        dplyr::coalesce(pass_tds,            0) * 4     +
        # -2 per INT: aligned with the rest of the toolkit (R/24 PPR_INT,
        # R/28), which scores interceptions at -2, not -1.
        dplyr::coalesce(interceptions_thrown,0) * (-2)  +
        dplyr::coalesce(rushing_yards,       0) * 0.1   +
        dplyr::coalesce(rush_tds,            0) * 6     +
        dplyr::coalesce(receiving_yards,     0) * 0.1   +
        dplyr::coalesce(rec_tds,             0) * 6     +
        dplyr::coalesce(receptions,          0) * ppr_value,

      fp_per_game = dplyr::if_else(
        dplyr::coalesce(games_played, 0L) > 0L,
        season_fp_ppr / games_played,
        NA_real_
      )
    )
}


# ==============================================================================
# FUNCTION: compute_age_deltas
# ==============================================================================

#' Compute Year-Over-Year Production Changes Per Player (Delta Method)
#'
#' @description
#' Implements the delta method for aging curve construction. For each player,
#' computes the year-over-year change in a production metric at each age
#' transition. Only consecutive seasons (no gap years) are included.
#'
#' The delta method eliminates survivor bias: a player who declines and gets
#' cut at age 29 contributes his decline to the age-28-to-29 transition even
#' though he never appears in the age-29 level average. Raw averages by age
#' would exclude this player from age 29 entirely, making late-career aging
#' look better than it is.
#'
#' @param panel_with_ages Tibble. Panel output with \code{age_at_season_start}
#'   column added by \code{compute_player_ages()}.
#' @param metric_col Character. Name of the column to compute deltas on.
#'   Typically "fp_per_game" for production curves.
#' @param positions Character vector. Positions to include. Default all four.
#' @param age_min Integer. Minimum age to retain. Default \code{AGE_MIN}.
#' @param age_max Integer. Maximum age to retain. Default \code{AGE_MAX}.
#' @param min_career_seasons Integer. Minimum number of seasons a player must
#'   appear in the panel (within their position group) for their transitions to
#'   be included. Defaults to \code{MIN_CAREER_SEASONS} (4). Set to 1L to
#'   disable. Mirrors the NFL rookie contract -- players who wash out before
#'   4 seasons represent roster turnover, not aging trajectory. CAVEAT: this
#'   is also a survivor-bias filter. Players who decline out of the league in
#'   years 2-3 are dropped, yet their decline is genuine aging signal;
#'   filtering them makes late-career aging look milder than it is. Re-running
#'   with min_career_seasons = 1L is the recommended sensitivity analysis for
#'   quantifying that bias.
#' @param filter_low_volume Logical. If TRUE (default) and a \code{low_volume}
#'   column is present, exclude low-volume player-seasons before computing
#'   deltas. Low-volume rows are players with too few plays to produce
#'   meaningful production estimates.
#'
#' @return A tibble with one row per valid consecutive-season player transition:
#'   \describe{
#'     \item{player_id}{Character. Player identifier.}
#'     \item{player_name}{Character. Player name.}
#'     \item{position_group}{Character. Position (QB/RB/WR/TE).}
#'     \item{season}{Integer. Current season (the "to" year of the transition).}
#'     \item{age_at_season_start}{Integer. Age in the current season.}
#'     \item{metric_value}{Numeric. Current season metric value.}
#'     \item{prev_metric_value}{Numeric. Prior season metric value.}
#'     \item{delta}{Numeric. Change: current - prior.}
#'   }
#'
#' @details
#' A gap year (player misses a season) breaks the consecutive requirement.
#' The player's return-year delta is dropped because the age gap is two years,
#' not one. This is correct behavior: injury-year absences are not aging signals.
#'
#' @seealso \code{\link{fit_aging_curves}}, \code{\link{compute_player_ages}}
#'
#' @export
compute_age_deltas <- function(
    panel_with_ages,
    metric_col          = "fp_per_game",
    positions           = CURVE_POSITIONS,
    age_min             = AGE_MIN,
    age_max             = AGE_MAX,
    min_career_seasons  = MIN_CAREER_SEASONS,
    filter_low_volume   = TRUE
) {

  if (!is.data.frame(panel_with_ages) || nrow(panel_with_ages) == 0L) {
    stop("panel_with_ages must be a non-empty data frame.")
  }
  required <- c("player_id", "player_name", "position_group",
                "season", "age_at_season_start", metric_col)
  missing_cols <- setdiff(required, names(panel_with_ages))
  if (length(missing_cols) > 0L) {
    stop(glue("Missing columns: {paste(missing_cols, collapse = ', ')}"))
  }

  # Apply low_volume filter if requested and column is present.
  # Low-volume players (few plays, typically garbage time or IR stints)
  # suppress production at a given age -- their deltas are noise, not aging signal.
  working_panel <- panel_with_ages
  if (filter_low_volume && "low_volume" %in% names(working_panel)) {
    working_panel <- working_panel %>%
      dplyr::filter(low_volume == FALSE)
  }

  # Career-length filter: only include players with >= min_career_seasons
  # seasons in the panel. Players who wash out before completing a rookie
  # contract represent roster turnover, not aging trajectory.
  # This is computed on the FULL panel (before age/position filtering) so
  # that a player who played at age 20-21 outside the age window still
  # counts toward their career season total.
  if (!is.na(min_career_seasons) && min_career_seasons > 1L) {
    qualifying_players <- panel_with_ages %>%
      dplyr::filter(position_group %in% positions) %>%
      dplyr::group_by(player_id) %>%
      dplyr::summarise(career_seasons = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(career_seasons >= min_career_seasons) %>%
      dplyr::pull(player_id)

    working_panel <- working_panel %>%
      dplyr::filter(player_id %in% qualifying_players)
  }

  working_panel %>%
    dplyr::filter(
      position_group %in% positions,
      !is.na(age_at_season_start),
      !is.na(.data[[metric_col]]),
      age_at_season_start >= age_min,
      age_at_season_start <= age_max
    ) %>%
    dplyr::select(
      player_id, player_name, position_group,
      season, age_at_season_start,
      metric_value = dplyr::all_of(metric_col)
    ) %>%
    dplyr::group_by(player_id) %>%
    dplyr::arrange(season, .by_group = TRUE) %>%
    dplyr::mutate(
      prev_metric_value = dplyr::lag(metric_value),
      prev_season       = dplyr::lag(season),
      consecutive       = (!is.na(prev_season) & season == prev_season + 1L),
      delta             = metric_value - prev_metric_value
    ) %>%
    dplyr::filter(consecutive, !is.na(delta)) %>%
    dplyr::select(
      player_id, player_name, position_group,
      season, age_at_season_start,
      metric_value, prev_metric_value, delta
    ) %>%
    dplyr::ungroup()
}


# ==============================================================================
# FUNCTION: load_ngs_season_panel
# ==============================================================================

#' Load and Aggregate NGS Metrics to Player-Season Level
#'
#' @description
#' Loads NFL Next Gen Stats via nflreadr::load_nextgen_stats() and returns
#' season-level summaries per player for the three stat types: passing,
#' receiving, and rushing.
#'
#' Uses week == 0 rows which are pre-aggregated season summaries provided
#' by NGS directly. No manual re-aggregation from weekly data required.
#'
#' Primary aging-relevant metrics extracted:
#' \itemize{
#'   \item Passing: completion_percentage_above_expectation (CPOE)
#'   \item Receiving: avg_separation, avg_yac_above_expectation
#'   \item Rushing: rush_yards_over_expected_per_att, efficiency
#' }
#'
#' @param seasons Integer vector. Seasons to load. Must be 2016 or later.
#'   Default \code{NGS_SEASONS}.
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return A tibble with one row per player per season, columns:
#'   \describe{
#'     \item{player_id}{Character. GSIS ID (matches panel player_id).}
#'     \item{season}{Integer.}
#'     \item{player_display_name}{Character.}
#'     \item{player_position}{Character. Position per NGS.}
#'     \item{cpoe}{Numeric. Completion pct above expectation (passers only).}
#'     \item{avg_separation}{Numeric. Avg separation at catch/incompletion (receivers).}
#'     \item{avg_yac_above_expectation}{Numeric. YAC above expected (receivers).}
#'     \item{rush_yards_over_expected_per_att}{Numeric. RYOE per attempt (rushers).}
#'     \item{ngs_efficiency}{Numeric. Rushing efficiency metric (lower = more NS).}
#'   }
#'
#' @details
#' NGS applies a minimum attempt filter -- players with few attempts do not
#' appear. This is expected and appropriate for efficiency metrics.
#'
#' @seealso \code{\link{run_aging_curve_pipeline}}
#'
#' @export
load_ngs_season_panel <- function(seasons = NGS_SEASONS, verbose = TRUE) {

  invalid_seasons <- seasons[seasons < 2016L]
  if (length(invalid_seasons) > 0L) {
    stop(glue(
      "NGS data is only available from 2016 onward.\n",
      "Invalid seasons requested: {paste(invalid_seasons, collapse = ', ')}"
    ))
  }

  if (verbose) message(glue(
    "load_ngs_season_panel(): Loading NGS for {min(seasons)}-{max(seasons)}..."
  ))

  # --- Passing NGS ---
  if (verbose) message("  Loading passing NGS...")
  ngs_pass <- tryCatch(
    nflreadr::load_nextgen_stats(seasons = seasons, stat_type = "passing"),
    error = function(e) {
      warning(glue("NGS passing load failed: {conditionMessage(e)}"))
      return(NULL)
    }
  )

  # --- Receiving NGS ---
  if (verbose) message("  Loading receiving NGS...")
  ngs_rec <- tryCatch(
    nflreadr::load_nextgen_stats(seasons = seasons, stat_type = "receiving"),
    error = function(e) {
      warning(glue("NGS receiving load failed: {conditionMessage(e)}"))
      return(NULL)
    }
  )

  # --- Rushing NGS ---
  if (verbose) message("  Loading rushing NGS...")
  ngs_rush <- tryCatch(
    nflreadr::load_nextgen_stats(seasons = seasons, stat_type = "rushing"),
    error = function(e) {
      warning(glue("NGS rushing load failed: {conditionMessage(e)}"))
      return(NULL)
    }
  )

  # Use week == 0 rows which are pre-computed season summaries from NGS
  # season_type filtering to REG to exclude postseason summaries
  extract_season_rows <- function(ngs_df) {
    if (is.null(ngs_df) || nrow(ngs_df) == 0L) return(NULL)
    ngs_df %>%
      dplyr::filter(
        week == 0,
        season_type == "REG"
      )
  }

  ngs_pass_season  <- extract_season_rows(ngs_pass)
  ngs_rec_season   <- extract_season_rows(ngs_rec)
  ngs_rush_season  <- extract_season_rows(ngs_rush)

  rm(ngs_pass, ngs_rec, ngs_rush)
  gc()

  # --- Extract relevant columns per type ---
  pass_panel <- if (!is.null(ngs_pass_season) && nrow(ngs_pass_season) > 0L) {
    ngs_pass_season %>%
      dplyr::select(
        player_id          = player_gsis_id,
        season,
        player_display_name,
        player_position,
        cpoe               = completion_percentage_above_expectation,
        ngs_attempts       = attempts
      ) %>%
      dplyr::filter(!is.na(player_id))
  } else {
    warning("NGS passing season panel is empty. CPOE will be unavailable.")
    NULL
  }

  rec_panel <- if (!is.null(ngs_rec_season) && nrow(ngs_rec_season) > 0L) {
    ngs_rec_season %>%
      dplyr::select(
        player_id                  = player_gsis_id,
        season,
        player_display_name,
        player_position,
        avg_separation,
        avg_yac_above_expectation,
        ngs_targets                = targets
      ) %>%
      dplyr::filter(!is.na(player_id))
  } else {
    warning("NGS receiving season panel is empty. Separation/YAC will be unavailable.")
    NULL
  }

  rush_panel <- if (!is.null(ngs_rush_season) && nrow(ngs_rush_season) > 0L) {
    ngs_rush_season %>%
      dplyr::select(
        player_id                        = player_gsis_id,
        season,
        player_display_name,
        player_position,
        rush_yards_over_expected_per_att,
        ngs_efficiency                   = efficiency,
        ngs_rush_attempts                = rush_attempts
      ) %>%
      dplyr::filter(!is.na(player_id))
  } else {
    warning("NGS rushing season panel is empty. RYOE will be unavailable.")
    NULL
  }

  rm(ngs_pass_season, ngs_rec_season, ngs_rush_season)
  gc()

  # Combine all three into one panel. Use full_join so players with data
  # in only one or two types are retained.
  panels_to_join <- purrr::discard(
    list(pass_panel, rec_panel, rush_panel),
    is.null
  )

  if (length(panels_to_join) == 0L) {
    stop("All three NGS load attempts failed. Cannot build NGS season panel.")
  }

  ngs_combined <- purrr::reduce(
    panels_to_join,
    function(x, y) {
      dplyr::full_join(
        x, y,
        by = c("player_id", "season")
      ) %>%
        dplyr::mutate(
          player_display_name = dplyr::coalesce(player_display_name.x, player_display_name.y),
          player_position     = dplyr::coalesce(player_position.x, player_position.y)
        ) %>%
        dplyr::select(-dplyr::starts_with("player_display_name."),
                      -dplyr::starts_with("player_position."))
    }
  )

  rm(panels_to_join, pass_panel, rec_panel, rush_panel)
  gc()

  if (verbose) {
    n_players <- dplyr::n_distinct(ngs_combined$player_id)
    n_seasons <- dplyr::n_distinct(ngs_combined$season)
    message(glue(
      "load_ngs_season_panel(): Complete.\n",
      "  {format(nrow(ngs_combined), big.mark = ',')} player-seasons across\n",
      "  {n_players} unique players | {n_seasons} seasons"
    ))
  }

  ngs_combined
}


# ==============================================================================
# FUNCTION: fit_aging_curves
# ==============================================================================

#' Fit Quadratic and LOESS Aging Curves from Delta Method Data
#'
#' @description
#' Takes pre-computed year-over-year deltas and returns fitted aging curves
#' (quadratic and LOESS) for a single position. The curve represents the
#' expected cumulative production trajectory relative to the baseline age,
#' constructed from the average delta at each age transition.
#'
#' @param delta_data Tibble. Output of \code{compute_age_deltas()}.
#' @param position Character scalar. One of "QB", "RB", "WR", "TE".
#' @param baseline_age Integer. Age to anchor at zero on the relative scale.
#'   Default \code{AGE_BASELINE} (23).
#' @param min_obs Integer. Minimum player transitions required per age bucket.
#'   Age buckets below this threshold are flagged but not dropped. Default
#'   \code{MIN_AGE_OBS}.
#' @param loess_span Numeric. LOESS smoothing parameter. Default \code{LOESS_SPAN}.
#'
#' @return A named list:
#'   \describe{
#'     \item{position}{Character. The position this curve was fit for.}
#'     \item{age_summary}{Tibble. Mean delta, SE, n per age bucket.}
#'     \item{curve_data}{Tibble. Reconstructed cumulative curve with age,
#'       quadratic fitted, LOESS fitted, and raw cumulative values.}
#'     \item{quad_model}{lm object. Quadratic fit on cumulative values.}
#'     \item{peak_age_quad}{Integer. Age with highest fitted value (quadratic).}
#'     \item{peak_age_loess}{Integer. Age with highest fitted value (LOESS).}
#'     \item{sparse_ages}{Integer vector. Ages below min_obs threshold.}
#'     \item{baseline_age_used}{Integer. Age the cumulative curve was actually
#'       anchored at. Equals baseline_age unless that age had no transitions,
#'       in which case the nearest available age was used (and a message
#'       emitted).}
#'   }
#'
#' @seealso \code{\link{compute_age_deltas}}, \code{\link{run_aging_curve_pipeline}}
#'
#' @export
fit_aging_curves <- function(
    delta_data,
    position,
    baseline_age = AGE_BASELINE,
    min_obs      = MIN_AGE_OBS,
    loess_span   = LOESS_SPAN
) {

  if (!position %in% CURVE_POSITIONS) {
    stop(glue("position must be one of: {paste(CURVE_POSITIONS, collapse = ', ')}"))
  }

  pos_deltas <- delta_data %>%
    dplyr::filter(position_group == position)

  if (nrow(pos_deltas) == 0L) {
    warning(glue("No delta data found for position: {position}. Returning NULL."))
    return(NULL)
  }

  # Average delta at each age transition
  age_summary <- pos_deltas %>%
    dplyr::group_by(age_at_season_start) %>%
    dplyr::summarise(
      mean_delta = mean(delta, na.rm = TRUE),
      sd_delta   = sd(delta, na.rm = TRUE),
      n_obs      = dplyr::n(),
      se_delta   = dplyr::if_else(n_obs > 1L, sd_delta / sqrt(n_obs), NA_real_),
      ci_lower   = dplyr::if_else(
        !is.na(se_delta), mean_delta - 1.96 * se_delta, NA_real_
      ),
      ci_upper   = dplyr::if_else(
        !is.na(se_delta), mean_delta + 1.96 * se_delta, NA_real_
      ),
      .groups    = "drop"
    ) %>%
    dplyr::arrange(age_at_season_start) %>%
    dplyr::mutate(sparse = n_obs < min_obs)

  sparse_ages <- age_summary %>%
    dplyr::filter(sparse) %>%
    dplyr::pull(age_at_season_start)

  # Reconstruct cumulative curve anchored at baseline_age = 0
  # Ages below baseline: cumulate backwards (reverse prefix sum)
  # Ages above baseline: cumulate forward from 0

  all_ages   <- age_summary$age_at_season_start
  age_range  <- seq(min(all_ages), max(all_ages))

  # Interpolate mean_delta for every integer age in range
  # (some ages may have no transitions -- fill with NA for safety)
  age_grid <- tibble::tibble(age_at_season_start = age_range) %>%
    dplyr::left_join(
      age_summary %>%
        dplyr::select(age_at_season_start, mean_delta, n_obs, sparse),
      by = "age_at_season_start"
    )

  # Build cumulative curve relative to baseline_age
  # For ages >= baseline: cumsum of deltas starting from 0 at baseline
  # For ages <  baseline: negative cumsum going backwards

  baseline_idx <- which(age_grid$age_at_season_start == baseline_age)
  if (length(baseline_idx) == 0L) {
    # Find nearest age to baseline if exact match not in data
    baseline_idx <- which.min(abs(age_grid$age_at_season_start - baseline_age))
    if (length(baseline_idx) == 0L) baseline_idx <- 1L
  }

  # Record the anchor actually used. When baseline_age has no transitions in
  # the data, the nearest-age fallback above re-anchors the curve; surface
  # that instead of doing it silently.
  baseline_age_used <- age_grid$age_at_season_start[baseline_idx]
  if (!isTRUE(baseline_age_used == baseline_age)) {
    message(glue(
      "Position {position}: requested baseline_age {baseline_age} has no ",
      "age transitions; curve re-anchored at nearest available age ",
      "{baseline_age_used}."
    ))
  }

  n_ages          <- nrow(age_grid)
  cumulative_vals <- rep(NA_real_, n_ages)
  cumulative_vals[baseline_idx] <- 0

  # Forward pass: ages above baseline
  if (baseline_idx < n_ages) {
    for (i in seq(baseline_idx + 1L, n_ages)) {
      prev_val  <- cumulative_vals[i - 1L]
      this_delta <- age_grid$mean_delta[i]
      if (!is.na(prev_val) && !is.na(this_delta)) {
        cumulative_vals[i] <- prev_val + this_delta
      }
    }
  }

  # Backward pass: ages below baseline (subtract deltas going backward)
  if (baseline_idx > 1L) {
    for (i in seq(baseline_idx - 1L, 1L)) {
      next_val   <- cumulative_vals[i + 1L]
      next_delta <- age_grid$mean_delta[i + 1L]
      if (!is.na(next_val) && !is.na(next_delta)) {
        cumulative_vals[i] <- next_val - next_delta
      }
    }
  }

  age_grid$cumulative_value <- cumulative_vals

  # Filter to ages with non-NA cumulative values for curve fitting
  fit_data <- age_grid %>%
    dplyr::filter(!is.na(cumulative_value))

  if (nrow(fit_data) < 4L) {
    warning(glue(
      "Position {position}: fewer than 4 age points with data. ",
      "Curves will not be reliable."
    ))
  }

  # Weight both fits by the number of observed transitions at each age so
  # thin tails (few players) do not pull the curve as hard as well-populated
  # ages. n_obs can be NA for an age with no transitions (e.g., the baseline
  # anchor row); floor those at 1 so weights stay valid.
  fit_data <- fit_data %>%
    dplyr::mutate(fit_weight = dplyr::coalesce(as.numeric(n_obs), 1))

  # --- Quadratic fit ---
  quad_model <- tryCatch(
    lm(cumulative_value ~ poly(age_at_season_start, 2, raw = FALSE),
       data = fit_data, weights = fit_weight),
    error = function(e) {
      warning(glue("Quadratic fit failed for {position}: {conditionMessage(e)}"))
      NULL
    }
  )

  # --- LOESS fit ---
  loess_model <- tryCatch(
    loess(
      cumulative_value ~ age_at_season_start,
      data    = fit_data,
      span    = loess_span,
      degree  = 2L,
      weights = fit_weight
    ),
    error = function(e) {
      warning(glue("LOESS fit failed for {position}: {conditionMessage(e)}"))
      NULL
    }
  )

  # Predicted values on full age grid.
  # IMPORTANT: pass only the predictor column to predict() -- not the full
  # age_grid tibble. age_grid contains NA values in mean_delta and
  # cumulative_value for ages with no observed transitions. predict.loess
  # (via predLoess) treats all columns in newdata as inputs and throws
  # "NA/NaN/Inf in foreign function call (arg 5)" when it encounters them.
  # Passing a single-column tibble with only age_at_season_start avoids this.
  newdata_ages <- tibble::tibble(
    age_at_season_start = age_grid$age_at_season_start
  )

  # Predictions wrapped in tryCatch at the predict() stage, not just at fit().
  # A LOESS model fit on degenerate data (< 3 unique x values) can construct
  # without error but produce a broken internal state. predict.loess then
  # throws "NA/NaN/Inf in foreign function call (arg 5)" at prediction time.
  # tryCatch here catches that case and fills NA for all ages gracefully.
  n_pred <- nrow(newdata_ages)

  fitted_quad_vals <- if (!is.null(quad_model)) {
    tryCatch(
      predict(quad_model, newdata = newdata_ages),
      error = function(e) {
        warning(glue("Quadratic prediction failed for {position}: {conditionMessage(e)}"))
        rep(NA_real_, n_pred)
      }
    )
  } else {
    rep(NA_real_, n_pred)
  }

  fitted_loess_vals <- if (!is.null(loess_model)) {
    tryCatch(
      suppressWarnings(predict(loess_model, newdata = newdata_ages)),
      error = function(e) {
        warning(glue("LOESS prediction failed for {position}: {conditionMessage(e)}"))
        rep(NA_real_, n_pred)
      }
    )
  } else {
    rep(NA_real_, n_pred)
  }

  age_grid <- age_grid %>%
    dplyr::mutate(
      fitted_quad  = fitted_quad_vals,
      fitted_loess = fitted_loess_vals
    )

  # Peak age estimates from fitted values
  peak_age_quad <- if (!all(is.na(age_grid$fitted_quad))) {
    age_grid$age_at_season_start[which.max(age_grid$fitted_quad)]
  } else { NA_integer_ }

  peak_age_loess <- if (!all(is.na(age_grid$fitted_loess))) {
    age_grid$age_at_season_start[which.max(age_grid$fitted_loess)]
  } else { NA_integer_ }

  list(
    position          = position,
    age_summary       = age_summary,
    curve_data        = age_grid,
    quad_model        = quad_model,
    peak_age_quad     = peak_age_quad,
    peak_age_loess    = peak_age_loess,
    sparse_ages       = sparse_ages,
    baseline_age_used = baseline_age_used
  )
}


# ==============================================================================
# FUNCTION: validate_aging_assumptions
# ==============================================================================

#' Validate Aging Curve Analytical Assumptions
#'
#' @description
#' Runs a battery of pre-modeling assumption checks on the player-season
#' panel and delta data before fitting curves. Returns a structured report
#' with pass/flag status per check.
#'
#' @param panel_with_ages Tibble. Panel with age_at_season_start, fp_per_game.
#' @param delta_data Tibble. Output of \code{compute_age_deltas()}.
#' @param ngs_panel Tibble or NULL. NGS panel from \code{load_ngs_season_panel()}.
#'   If NULL, NGS checks are skipped.
#' @param verbose Logical. Print report to console. Default TRUE.
#'
#' @return A named list:
#'   \describe{
#'     \item{valid}{Logical. TRUE if all critical checks passed.}
#'     \item{report}{Tibble. One row per check.}
#'     \item{age_coverage}{Tibble. Age range and observation count by position.}
#'     \item{obs_per_age}{Tibble. N transitions per age per position.}
#'   }
#'
#' @export
validate_aging_assumptions <- function(
    panel_with_ages,
    delta_data,
    ngs_panel = NULL,
    verbose   = TRUE
) {

  checks <- list()

  # --- Check 1: Age coverage (critical) ---
  age_coverage <- panel_with_ages %>%
    dplyr::filter(
      !is.na(age_at_season_start),
      position_group %in% CURVE_POSITIONS
    ) %>%
    dplyr::group_by(position_group) %>%
    dplyr::summarise(
      n_player_seasons  = dplyr::n(),
      age_min           = min(age_at_season_start, na.rm = TRUE),
      age_max           = max(age_at_season_start, na.rm = TRUE),
      pct_with_age      = mean(!is.na(age_at_season_start)) * 100,
      .groups           = "drop"
    )

  checks[["age_coverage"]] <- tibble::tibble(
    check    = "Age coverage per position",
    critical = TRUE,
    result   = all(age_coverage$n_player_seasons >= 50L),
    note     = paste(
      age_coverage$position_group,
      "n =", format(age_coverage$n_player_seasons, big.mark = ","),
      collapse = "; "
    )
  )

  # --- Check 2: Delta observations per age bucket (critical) ---
  obs_per_age <- delta_data %>%
    dplyr::group_by(position_group, age_at_season_start) %>%
    dplyr::summarise(n_transitions = dplyr::n(), .groups = "drop")

  sparse_count <- obs_per_age %>%
    dplyr::filter(n_transitions < MIN_AGE_OBS) %>%
    nrow()

  checks[["sparse_ages"]] <- tibble::tibble(
    check    = glue("Age buckets below MIN_AGE_OBS = {MIN_AGE_OBS}"),
    critical = FALSE,
    result   = sparse_count == 0L,
    note     = glue("{sparse_count} age-position buckets below threshold (will be flagged in plots)")
  )

  # --- Check 3: Consecutive season rate (informational) ---
  total_transitions   <- nrow(delta_data)
  all_potential_pairs <- panel_with_ages %>%
    dplyr::filter(
      !is.na(age_at_season_start),
      position_group %in% CURVE_POSITIONS,
      !is.na(fp_per_game)
    ) %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(n_seasons = dplyr::n(), .groups = "drop") %>%
    dplyr::summarise(total_pairs = sum(pmax(n_seasons - 1L, 0L))) %>%
    dplyr::pull(total_pairs)

  consecutive_pct <- if (all_potential_pairs > 0L)
    round(total_transitions / all_potential_pairs * 100, 1)
  else 0

  checks[["consecutive_rate"]] <- tibble::tibble(
    check    = "Consecutive season transition rate",
    critical = FALSE,
    result   = consecutive_pct >= 50,
    note     = glue(
      "{format(total_transitions, big.mark = ',')} transitions used of ",
      "{format(all_potential_pairs, big.mark = ',')} potential ({consecutive_pct}%). ",
      "Gap-year absences excluded by design."
    )
  )

  # --- Check 4: NGS season coverage (informational, only if ngs_panel provided) ---
  if (!is.null(ngs_panel) && nrow(ngs_panel) > 0L) {
    ngs_seasons_present <- sort(unique(ngs_panel$season))
    expected_ngs        <- NGS_SEASONS

    checks[["ngs_coverage"]] <- tibble::tibble(
      check    = "NGS season coverage",
      critical = FALSE,
      result   = all(expected_ngs %in% ngs_seasons_present),
      note     = glue(
        "NGS seasons present: {min(ngs_seasons_present)}-{max(ngs_seasons_present)} ",
        "({length(ngs_seasons_present)} seasons)"
      )
    )
  }

  # --- Check 5: Delta distribution (informational) ---
  # Normality of deltas at peak ages per position (Shapiro-Wilk)
  # Only run if n >= 8 (Shapiro-Wilk requirement)
  normality_results <- purrr::map_dfr(CURVE_POSITIONS, function(pos) {
    test_deltas <- delta_data %>%
      dplyr::filter(
        position_group == pos,
        age_at_season_start >= 24L,
        age_at_season_start <= 30L
      ) %>%
      dplyr::pull(delta)

    if (length(test_deltas) < 8L) {
      return(tibble::tibble(
        position = pos, shapiro_p = NA_real_, normal = NA
      ))
    }

    sw <- tryCatch(
      shapiro.test(test_deltas[seq_len(min(length(test_deltas), 5000L))]),
      error = function(e) list(p.value = NA_real_)
    )

    tibble::tibble(
      position  = pos,
      shapiro_p = round(sw$p.value, 4),
      normal    = is.na(sw$p.value) || sw$p.value >= 0.05
    )
  })

  non_normal <- normality_results %>%
    dplyr::filter(!is.na(normal), !normal) %>%
    dplyr::pull(position)

  checks[["delta_normality"]] <- tibble::tibble(
    check    = "Delta distribution normality (ages 24-30)",
    critical = FALSE,
    result   = length(non_normal) == 0L,
    note     = if (length(non_normal) > 0L)
      glue("Non-normal delta distributions: {paste(non_normal, collapse = ', ')}. ",
           "LOESS is robust to non-normality; quadratic CIs may be slightly liberal.")
    else
      "All tested positions have approximately normal delta distributions."
  )

  # Compile report
  report <- dplyr::bind_rows(checks)

  all_critical_passed <- all(report$result[report$critical])

  if (verbose) {
    message("\n--- Aging Curve Assumption Validation ---")
    for (i in seq_len(nrow(report))) {
      status <- if (report$result[i]) "PASS" else if (report$critical[i]) "FAIL" else "FLAG"
      message(glue("  [{status}] {report$check[i]}: {report$note[i]}"))
    }
    message(glue("\nOverall valid: {all_critical_passed}"))
    message("-----------------------------------------\n")
  }

  list(
    valid       = all_critical_passed,
    report      = report,
    age_coverage= age_coverage,
    obs_per_age = obs_per_age
  )
}


# ==============================================================================
# INTERNAL: Visualization Functions
# ==============================================================================

# .plot_curves_boxscore()
# 4-panel (QB/RB/WR/TE): quadratic vs LOESS aging curves with peak age markers.
# Peak age vertical lines are labeled in plain language so non-technical
# readers immediately see where production peaks and -- for TE -- why the
# two models disagree by 3 years.
.plot_curves_boxscore <- function(curve_results, output_dir) {

  # Assemble curve data for all four positions
  all_curves <- purrr::map_dfr(curve_results, function(cr) {
    if (is.null(cr)) return(NULL)
    cr$curve_data %>%
      dplyr::mutate(position = cr$position) %>%
      dplyr::filter(
        age_at_season_start >= AGE_MIN,
        age_at_season_start <= AGE_MAX
      )
  })

  if (nrow(all_curves) == 0L) {
    warning(".plot_curves_boxscore(): No curve data to plot.")
    return(invisible(NULL))
  }

  # Build peak age data frame for vertical line annotations.
  # Two rows per position: one for quadratic peak, one for LOESS peak.
  # Used with geom_vline + geom_label so each facet gets its own markers.
  peak_vlines <- purrr::map_dfr(curve_results, function(cr) {
    if (is.null(cr)) return(NULL)
    rows <- list()
    if (!is.na(cr$peak_age_quad)) {
      rows[[1]] <- tibble::tibble(
        position = cr$position,
        peak_age = cr$peak_age_quad,
        model    = "Quadratic"
      )
    }
    if (!is.na(cr$peak_age_loess)) {
      rows[[2]] <- tibble::tibble(
        position = cr$position,
        peak_age = cr$peak_age_loess,
        model    = "LOESS"
      )
    }
    if (length(rows) > 0) dplyr::bind_rows(rows) else NULL
  }) %>%
    dplyr::mutate(
      position = factor(position, levels = CURVE_POSITIONS),
      # Plain-language label: "Peak at 26" rather than model jargon
      label = paste0("Peak
age ", peak_age)
    )

  # Reshape to long format for dual-line plot
  curves_long <- all_curves %>%
    tidyr::pivot_longer(
      cols      = c(fitted_quad, fitted_loess),
      names_to  = "model",
      values_to = "fitted_value"
    ) %>%
    dplyr::mutate(
      model = dplyr::case_when(
        model == "fitted_quad"  ~ "Quadratic",
        model == "fitted_loess" ~ "LOESS",
        TRUE                    ~ model
      ),
      position = factor(position, levels = CURVE_POSITIONS)
    ) %>%
    dplyr::filter(!is.na(fitted_value))

  # Raw cumulative values as scatter background
  raw_scatter <- all_curves %>%
    dplyr::filter(!is.na(cumulative_value)) %>%
    dplyr::mutate(position = factor(position, levels = CURVE_POSITIONS))

  # y-position for peak age labels: top of each facet's data range
  label_y_data <- all_curves %>%
    dplyr::filter(!is.na(cumulative_value)) %>%
    dplyr::mutate(position = factor(position, levels = CURVE_POSITIONS)) %>%
    dplyr::group_by(position) %>%
    dplyr::summarise(y_top = max(cumulative_value, na.rm = TRUE), .groups = "drop")

  peak_vlines <- peak_vlines %>%
    dplyr::left_join(label_y_data, by = "position")

  p <- ggplot2::ggplot(
    curves_long,
    ggplot2::aes(x = age_at_season_start, y = fitted_value,
                 color = model, linetype = model)
  ) +
    ggplot2::geom_point(
      data  = raw_scatter,
      ggplot2::aes(x = age_at_season_start, y = cumulative_value),
      color = "gray60", size = 1.2, alpha = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
    ggplot2::geom_line(linewidth = 1.1, na.rm = TRUE) +
    # Peak age vertical lines -- one per model per facet
    ggplot2::geom_vline(
      data        = peak_vlines,
      ggplot2::aes(xintercept = peak_age, color = model, linetype = model),
      linewidth   = 0.7,
      alpha       = 0.75,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    # Plain-language peak age labels at top of each facet
    ggplot2::geom_label(
      data        = peak_vlines,
      ggplot2::aes(
        x     = peak_age,
        y     = y_top,
        label = label,
        color = model
      ),
      size        = 2.6,
      fontface    = "bold",
      linewidth   = 0.2,
      label.padding = ggplot2::unit(0.15, "lines"),
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    ggplot2::facet_wrap(~ position, scales = "free_y", nrow = 2L) +
    ggplot2::scale_color_manual(
      values = c("Quadratic" = "#1565c0", "LOESS" = "#c62828"),
      name   = "Curve"
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Quadratic" = "solid", "LOESS" = "dashed"),
      name   = "Curve"
    ) +
    ggplot2::labs(
      title    = "When Do NFL Players Peak? It Depends How You Measure It",
      subtitle = paste0(
        "Vertical lines show each model's peak age estimate | ",
        "When the lines are far apart, the math assumption matters\n",
        "For TEs, the two models disagree by 3 years -- TEs rise slowly, ",
        "which breaks the quadratic model's symmetry assumption"
      ),
      x        = "Age (as of September 1 of season year)",
      y        = "PPR Fantasy Points per Game (relative to age 23)",
      caption  = paste0(
        "Only players with 4+ seasons included (mirrors rookie contract length) | ",
        "Low-volume seasons excluded\n",
        "Seasons: ", min(PANEL_SEASONS), "-", max(PANEL_SEASONS),
        " | Delta method eliminates survivor bias | nflfastR | NFL Analytics Toolkit"
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle   = ggplot2::element_text(size = 9, color = "gray35", lineheight = 1.3),
      strip.text      = ggplot2::element_text(face = "bold", size = 11),
      legend.position = "bottom",
      panel.grid.minor= ggplot2::element_blank(),
      plot.caption    = ggplot2::element_text(size = 7.5, color = "gray50")
    )

  out_path <- file.path(output_dir, paste0(FILE_PREFIX, "aging_curves_boxscore.png"))
  ggplot2::ggsave(out_path, plot = p, width = 11, height = 8, dpi = 300)
  message(glue("  Saved: {out_path}"))
  invisible(p)
}


# .plot_age_deltas()
# 4-panel bar chart: raw average year-over-year change by age per position
.plot_age_deltas <- function(curve_results, output_dir) {

  all_summaries <- purrr::map_dfr(curve_results, function(cr) {
    if (is.null(cr)) return(NULL)
    cr$age_summary %>%
      dplyr::mutate(position = cr$position) %>%
      dplyr::filter(
        age_at_season_start >= AGE_MIN,
        age_at_season_start <= AGE_MAX,
        n_obs >= 3L
      )
  }) %>%
    dplyr::mutate(
      direction = dplyr::if_else(mean_delta >= 0, "Positive", "Negative"),
      position  = factor(position, levels = CURVE_POSITIONS)
    )

  if (nrow(all_summaries) == 0L) {
    warning(".plot_age_deltas(): No delta summary data to plot.")
    return(invisible(NULL))
  }

  p <- ggplot2::ggplot(
    all_summaries,
    ggplot2::aes(x = age_at_season_start, y = mean_delta, fill = direction)
  ) +
    ggplot2::geom_col(width = 0.7, alpha = 0.85) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.3, color = "gray30", na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    ggplot2::facet_wrap(~ position, scales = "free_y", nrow = 2L) +
    ggplot2::scale_fill_manual(
      values = c("Positive" = "#2e7d32", "Negative" = "#c62828"),
      guide  = "none"
    ) +
    ggplot2::labs(
      title    = "Year-Over-Year Change in PPR Points per Game by Age",
      subtitle = paste0(
        "Delta method raw output | Error bars: 95% CI | ",
        "Ages below n = ", MIN_AGE_OBS, " transitions excluded"
      ),
      x        = "Age (as of September 1)",
      y        = "Average change in PPR PPG",
      caption  = paste0(
        "Seasons: ", min(PANEL_SEASONS), "-", max(PANEL_SEASONS),
        " | Only consecutive-season transitions used\n",
        "Data: nflfastR | NFL Analytics Toolkit"
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40"),
      strip.text    = ggplot2::element_text(face = "bold", size = 11),
      panel.grid.minor = ggplot2::element_blank()
    )

  out_path <- file.path(output_dir, paste0(FILE_PREFIX, "age_deltas_by_position.png"))
  ggplot2::ggsave(out_path, plot = p, width = 10, height = 8, dpi = 300)
  message(glue("  Saved: {out_path}"))
  invisible(p)
}


# .plot_ngs_vs_boxscore()
# 4-panel: box score curve vs NGS efficiency curve (both z-scored for comparison)
.plot_ngs_vs_boxscore <- function(
    boxscore_curves,
    ngs_curves,
    output_dir
) {

  # Primary NGS metric per position (for annotation labels)
  ngs_metric_labels <- c(
    QB  = "CPOE",
    RB  = "RYOE per Att",
    WR  = "Avg Separation",
    TE  = "Avg Separation"
  )

  # Standardize curves to z-score so both are on the same scale
  .zscore_curve <- function(curve_data, value_col) {
    vals <- curve_data[[value_col]]
    mu   <- mean(vals, na.rm = TRUE)
    sigma<- sd(vals, na.rm = TRUE)
    if (is.na(sigma) || sigma == 0) return(rep(NA_real_, length(vals)))
    (vals - mu) / sigma
  }

  combined <- purrr::map_dfr(CURVE_POSITIONS, function(pos) {

    bs_result  <- boxscore_curves[[pos]]
    ngs_result <- ngs_curves[[pos]]

    if (is.null(bs_result) || is.null(ngs_result)) return(NULL)

    bs_data <- bs_result$curve_data %>%
      dplyr::filter(
        !is.na(fitted_loess),
        age_at_season_start >= AGE_MIN,
        age_at_season_start <= AGE_MAX
      )

    ngs_data <- ngs_result$curve_data %>%
      dplyr::filter(
        !is.na(fitted_loess),
        age_at_season_start >= AGE_MIN,
        age_at_season_start <= AGE_MAX
      )

    if (nrow(bs_data) == 0L || nrow(ngs_data) == 0L) return(NULL)

    bs_data$z_loess  <- .zscore_curve(bs_data,  "fitted_loess")
    ngs_data$z_loess <- .zscore_curve(ngs_data, "fitted_loess")

    dplyr::bind_rows(
      bs_data  %>% dplyr::transmute(
        position = pos,
        age_at_season_start,
        z_loess,
        source = "Box Score (PPR PPG)"
      ),
      ngs_data %>% dplyr::transmute(
        position = pos,
        age_at_season_start,
        z_loess,
        source = paste0("NGS: ", ngs_metric_labels[[pos]])
      )
    )
  }) %>%
    dplyr::mutate(position = factor(position, levels = CURVE_POSITIONS))

  if (nrow(combined) == 0L) {
    warning(".plot_ngs_vs_boxscore(): No comparable data across both sources.")
    return(invisible(NULL))
  }

  p <- ggplot2::ggplot(
    combined %>% dplyr::filter(!is.na(z_loess)),
    ggplot2::aes(
      x = age_at_season_start, y = z_loess,
      color = source, linetype = source
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
    ggplot2::geom_line(linewidth = 1.1, na.rm = TRUE) +
    ggplot2::facet_wrap(~ position, scales = "free_y", nrow = 2L) +
    ggplot2::scale_color_manual(
      values = c(
        "Box Score (PPR PPG)"    = "#1565c0",
        "NGS: CPOE"              = "#e65100",
        "NGS: RYOE per Att"      = "#e65100",
        "NGS: Avg Separation"    = "#e65100"
      ),
      name = "Source"
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        "Box Score (PPR PPG)"    = "solid",
        "NGS: CPOE"              = "dashed",
        "NGS: RYOE per Att"      = "dashed",
        "NGS: Avg Separation"    = "dashed"
      ),
      name = "Source"
    ) +
    ggplot2::labs(
      title    = "Box Score Production vs NGS Efficiency: When Does Each Decline?",
      subtitle = "Both curves standardized (z-score) for comparison | LOESS fit | NGS seasons 2016-2025",
      x        = "Age (as of September 1)",
      y        = "Standardized production (z-score)",
      caption  = paste0(
        "Box score: 2010-2025 | NGS: 2016-2025 | LOESS span = 0.75\n",
        "NGS metrics: QB = CPOE, RB = RYOE/att, WR/TE = Avg Separation\n",
        "Data: nflfastR + NFL Next Gen Stats via nflreadr | NFL Analytics Toolkit"
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle   = ggplot2::element_text(size = 10, color = "gray40"),
      strip.text      = ggplot2::element_text(face = "bold", size = 11),
      legend.position = "bottom",
      legend.text     = ggplot2::element_text(size = 9),
      panel.grid.minor= ggplot2::element_blank()
    )

  out_path <- file.path(output_dir, paste0(FILE_PREFIX, "ngs_vs_boxscore_curves.png"))
  ggplot2::ggsave(out_path, plot = p, width = 10, height = 8, dpi = 300)
  message(glue("  Saved: {out_path}"))
  invisible(p)
}



# .plot_ar1_distribution()
# 4-panel violin + jitter: player-level AR1 autocorrelation per position.
# Tells the mean reversion story in plain language: a great year tends to
# be followed by a worse one. Negative AR1 = mean reversion.
# This is an important context for fantasy managers: do NOT overreact to
# one great or terrible season.
.plot_ar1_distribution <- function(delta_data, output_dir) {

  # Compute AR1 (lag-1 autocorrelation) per player.
  # Only include players with >= 3 consecutive transitions.
  ar1_data <- delta_data %>%
    dplyr::filter(position_group %in% CURVE_POSITIONS) %>%
    dplyr::arrange(player_id, season) %>%
    dplyr::group_by(player_id, position_group) %>%
    dplyr::filter(dplyr::n() >= 3L) %>%
    dplyr::summarise(
      ar1 = tryCatch(
        cor(delta[seq_len(dplyr::n() - 1L)],
            delta[seq(2L, dplyr::n())],
            use = "complete.obs"),
        error = function(e) NA_real_
      ),
      n_transitions = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(ar1)) %>%
    dplyr::mutate(
      position_group = factor(position_group, levels = CURVE_POSITIONS)
    )

  if (nrow(ar1_data) == 0L) {
    warning(".plot_ar1_distribution(): No AR1 data computed. Need players with 3+ transitions.")
    return(invisible(NULL))
  }

  # Median AR1 per position for annotation
  median_ar1 <- ar1_data %>%
    dplyr::group_by(position_group) %>%
    dplyr::summarise(
      median_ar1 = round(median(ar1, na.rm = TRUE), 2),
      n_players  = dplyr::n(),
      .groups    = "drop"
    ) %>%
    dplyr::mutate(
      label = paste0("Median: ", median_ar1,
                     "\nn = ", format(n_players, big.mark = ","), " players")
    )

  p <- ggplot2::ggplot(ar1_data,
    ggplot2::aes(x = position_group, y = ar1, fill = position_group)
  ) +
    # Reference lines first so they sit behind the data
    ggplot2::geom_hline(yintercept =  0,    linetype = "solid",  color = "gray30", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = -0.5,  linetype = "dashed", color = "#c62828", linewidth = 0.6, alpha = 0.7) +
    # Reference line labels use a data frame with factor x so ggplot2
    # treats them as discrete coordinates, matching the x-axis scale.
    ggplot2::geom_text(
      data = tibble::tibble(
        position_group = factor("QB", levels = CURVE_POSITIONS),
        y_val  = c(0.06, -0.44),
        label  = c("No pattern", "Strong mean reversion"),
        colour = c("gray30", "#c62828")
      ),
      ggplot2::aes(x = position_group, y = y_val, label = label, colour = colour),
      hjust = -0.05, size = 3, inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::geom_violin(
      alpha = 0.35, trim = TRUE, scale = "width",
      color = NA
    ) +
    ggplot2::geom_jitter(
      width = 0.15, alpha = 0.4, size = 1.3, color = "gray30"
    ) +
    ggplot2::stat_summary(
      fun = median, geom = "crossbar",
      width = 0.45, linewidth = 0.9,
      color = "black", fatten = 2
    ) +
    # Median annotation inside each violin
    ggplot2::geom_text(
      data = median_ar1,
      ggplot2::aes(x = position_group, y = -0.9, label = label),
      size = 3, color = "gray20", lineheight = 1.2,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c("QB" = "#1565c0", "RB" = "#2e7d32",
                 "WR" = "#e65100", "TE" = "#6a1b9a"),
      guide  = "none"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-1.05, 1.05),
      breaks = seq(-1, 1, by = 0.25),
      labels = function(x) paste0(x)
    ) +
    ggplot2::labs(
      title    = "A Great Season Is Usually Followed by a Worse One",
      subtitle = paste0(
        "Mean reversion is the dominant pattern across all positions\n",
        "Each dot = one player. The crossbar = median. Values below 0 = next year ",
        "tends to be worse after a good year (and better after a bad one)"
      ),
      x        = "Position",
      y        = "Lag-1 Autocorrelation of Season-to-Season Change",
      caption  = paste0(
        "Only players with 3+ consecutive seasons included | ",
        "Seasons: ", min(PANEL_SEASONS), "-", max(PANEL_SEASONS), "\n",
        "Fantasy implication: do not overreact to one outlier season -- ",
        "regression to the mean is real | nflfastR | NFL Analytics Toolkit"
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle   = ggplot2::element_text(size = 9.5, color = "gray35", lineheight = 1.3),
      axis.title.x    = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(size = 12, face = "bold"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      plot.caption    = ggplot2::element_text(size = 7.5, color = "gray50")
    )

  out_path <- file.path(output_dir, paste0(FILE_PREFIX, "ar1_mean_reversion.png"))
  ggplot2::ggsave(out_path, plot = p, width = 9, height = 7, dpi = 300)
  message(glue("  Saved: {out_path}"))
  invisible(p)
}

# ==============================================================================
# FUNCTION: run_aging_curve_pipeline
# ==============================================================================

#' Run Complete Aging Curve Pipeline (End-to-End)
#'
#' @description
#' Orchestrates the full aging curve analysis:
#' \enumerate{
#'   \item Load or accept player-season panel from R/16
#'   \item Compute player ages (September 1 convention)
#'   \item Compute PPR points per game
#'   \item Compute year-over-year deltas (box score, delta method)
#'   \item Load NGS season panel (2016-2025)
#'   \item Compute NGS deltas for primary efficiency metrics
#'   \item Validate analytical assumptions
#'   \item Fit quadratic + LOESS curves for all four positions (both data sources)
#'   \item Generate three publication-quality visualizations
#'   \item Print KEY INSIGHTS from data (computed, never hardcoded)
#' }
#'
#' @param panel Tibble or NULL. Pre-loaded player-season panel. If NULL, the
#'   function calls \code{build_player_season_panel()} automatically.
#' @param seasons Integer vector. Seasons to include. Default \code{PANEL_SEASONS}.
#' @param cache_dir Character. Cache directory for R/16. Default \code{CACHE_DIR}.
#' @param save_plots Logical. Save PNG files to output/plots/. Default TRUE.
#' @param min_career_seasons Integer. Passed through to
#'   \code{compute_age_deltas()}. Default \code{MIN_CAREER_SEASONS} (4).
#'   NOTE: this default is a survivor-bias filter -- players who decline out
#'   of the league in years 2-3 never accumulate 4 seasons, yet their decline
#'   IS aging signal. Set to 1L for a sensitivity analysis of how much the
#'   career-length filter flatters late-career aging, without editing the
#'   toolkit constants.
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return A named list:
#'   \describe{
#'     \item{panel_with_ages}{Tibble. Panel enriched with age and PPG columns.}
#'     \item{delta_data_boxscore}{Tibble. PPG delta transitions.}
#'     \item{delta_data_ngs}{Named list. NGS metric delta transitions per position.}
#'     \item{ngs_panel}{Tibble. NGS season panel.}
#'     \item{curves_boxscore}{Named list. fit_aging_curves() results per position.}
#'     \item{curves_ngs}{Named list. fit_aging_curves() results per position.}
#'     \item{validation}{List. validate_aging_assumptions() output.}
#'     \item{key_insights}{Tibble. Computed peak ages and observation counts.}
#'   }
#'
#' @seealso
#'   \code{\link{compute_player_ages}}, \code{\link{compute_age_deltas}},
#'   \code{\link{fit_aging_curves}}, \code{\link{validate_aging_assumptions}}
#'
#' @examples
#' \dontrun{
#' # Full pipeline from scratch (first run: ~20 min; subsequent: faster via cache)
#' results <- run_aging_curve_pipeline()
#'
#' # Re-run with pre-loaded panel
#' panel <- build_player_season_panel()
#' results <- run_aging_curve_pipeline(panel = panel, save_plots = FALSE)
#'
#' # Inspect peak ages
#' results$key_insights
#'
#' # Check assumption validation
#' results$validation$report
#' }
#'
#' @export
run_aging_curve_pipeline <- function(
    panel     = NULL,
    seasons   = PANEL_SEASONS,
    cache_dir = CACHE_DIR,
    scoring_settings = NULL,
    save_plots= TRUE,
    min_career_seasons = MIN_CAREER_SEASONS,
    verbose   = TRUE
) {

  if (verbose) message("\n========================================")
  if (verbose) message("  NFL Aging Curve Pipeline -- Season 2 Week 9")
  if (verbose) message("========================================\n")

  # ------------------------------------------------------------------
  # Step 1: Load or validate player-season panel
  # ------------------------------------------------------------------
  if (verbose) message("Step 1: Player-season panel...")

  if (is.null(panel)) {
    # Source R/16 if build_player_season_panel not yet available
    if (!exists("build_player_season_panel", mode = "function")) {
      r16_path <- here::here("R", "16_player_season_panel.R")
      if (!file.exists(r16_path)) {
        stop(glue(
          "R/16_player_season_panel.R not found at: {r16_path}\n",
          "This file is required for build_player_season_panel()."
        ))
      }
      if (verbose) message("  Sourcing R/16_player_season_panel.R...")
      source(r16_path)
    }
    panel <- build_player_season_panel(
      seasons   = seasons,
      cache_dir = cache_dir,
      verbose   = verbose
    )
  } else {
    required_panel_cols <- c(
      "player_id", "season", "position_group",
      "games_played", "passing_yards", "rushing_yards", "receiving_yards"
    )
    missing_panel <- setdiff(required_panel_cols, names(panel))
    if (length(missing_panel) > 0L) {
      stop(glue(
        "Provided panel is missing columns: {paste(missing_panel, collapse = ', ')}.\n",
        "Ensure it was built by build_player_season_panel()."
      ))
    }
    if (verbose) message(glue(
      "  Using provided panel: {format(nrow(panel), big.mark = ',')} rows."
    ))
  }

  # ------------------------------------------------------------------
  # Step 2: Compute player ages
  # ------------------------------------------------------------------
  if (verbose) message("\nStep 2: Computing player ages (September 1 convention)...")

  panel <- compute_player_ages(panel, verbose = verbose)

  # ------------------------------------------------------------------
  # Step 3: Compute fantasy points per game
  # ------------------------------------------------------------------
  # Two paths [2026-07-15]:
  #
  #   scoring_settings = NULL  -> LEGACY. compute_season_ppg()'s eight
  #     hardcoded terms. Retained so the curves underlying the 2026-07-15
  #     value boards remain exactly reproducible for A/B comparison.
  #     DEPRECATED. Delete once the scored path is validated.
  #
  #   scoring_settings = list(...) -> SCORED. Routes play-by-play through
  #     R/17's engine via R/16's build_player_game_panel(), which supports
  #     every term R/17 implements: fumbles, per-game threshold bonuses,
  #     tiered PPR, TE premium, rush attempt bonus, sack penalty, superflex,
  #     and first-down points. None of these are recoverable from the
  #     season panel.
  # ------------------------------------------------------------------
  if (is.null(scoring_settings)) {

    if (verbose) {
      message("\nStep 3: Computing PPR points per game (LEGACY hardcode)...")
      message(
        "  NOTE: scoring_settings = NULL uses compute_season_ppg()'s hardcoded\n",
        "  scale. This is deprecated and cannot represent fumbles, threshold\n",
        "  bonuses, tiered PPR, TE premium, or first downs. Pass scoring_settings\n",
        "  to score through R/17."
      )
    }

    panel <- compute_season_ppg(panel, ppr_value = 1)

  } else {

    if (verbose) message("\nStep 3: Scoring play-by-play through R/17 (SCORED path)...")

    if (!exists("attach_scored_ppg", mode = "function")) {
      r16_path <- here::here("R", "16_player_season_panel.R")
      if (!file.exists(r16_path)) {
        stop(glue(
          "R/16_player_season_panel.R not found at: {r16_path}\n",
          "This file is required for attach_scored_ppg()."
        ))
      }
      if (verbose) message("  Sourcing R/16_player_season_panel.R...")
      source(r16_path)
    }

    panel <- attach_scored_ppg(
      panel            = panel,
      seasons          = seasons,
      scoring_settings = scoring_settings,
      cache_dir        = cache_dir,
      verbose          = verbose
    )
  }

  if (verbose) {
    n_with_ppg <- sum(!is.na(panel$fp_per_game))
    message(glue(
      "  {format(n_with_ppg, big.mark = ',')} player-seasons with fp_per_game."
    ))
  }

  # ------------------------------------------------------------------
  # Step 3b: Split WR_TE into separate WR and TE position groups
  # ------------------------------------------------------------------
  # R/16 collapses WR and TE into WR_TE for panel construction.
  # Aging curves require them separate -- WR and TE peak at different ages
  # and decline at different rates. split_wr_te_positions() loads rosters
  # a second time; nflreadr's internal cache makes this call free in session.
  # ------------------------------------------------------------------
  if (verbose) message("\nStep 3b: Splitting WR_TE into WR and TE position groups...")

  panel <- split_wr_te_positions(panel, verbose = verbose)

  # ------------------------------------------------------------------
  # Step 4: Box score deltas
  # ------------------------------------------------------------------
  if (verbose) message("\nStep 4: Computing year-over-year PPG deltas (box score)...")

  delta_boxscore <- compute_age_deltas(
    panel_with_ages    = panel,
    metric_col         = "fp_per_game",
    positions          = CURVE_POSITIONS,
    age_min            = AGE_MIN,
    age_max            = AGE_MAX,
    min_career_seasons = min_career_seasons,
    filter_low_volume  = TRUE
  )

  if (verbose) {
    message(glue(
      "  {format(nrow(delta_boxscore), big.mark = ',')} consecutive-season transitions."
    ))
    delta_by_pos <- delta_boxscore %>%
      dplyr::count(position_group) %>%
      dplyr::arrange(position_group)
    for (i in seq_len(nrow(delta_by_pos))) {
      message(glue(
        "  {delta_by_pos$position_group[i]}: ",
        "{format(delta_by_pos$n[i], big.mark = ',')} transitions"
      ))
    }
  }

  # ------------------------------------------------------------------
  # Step 5: Load NGS panel
  # ------------------------------------------------------------------
  if (verbose) message("\nStep 5: Loading NGS season panel...")

  ngs_panel <- load_ngs_season_panel(seasons = NGS_SEASONS, verbose = verbose)

  # ------------------------------------------------------------------
  # Step 6: Compute NGS deltas per position
  # ------------------------------------------------------------------
  if (verbose) message("\nStep 6: Computing NGS metric deltas per position...")

  # Map each position to its primary NGS metric column
  ngs_metric_map <- list(
    QB  = "cpoe",
    RB  = "rush_yards_over_expected_per_att",
    WR  = "avg_separation",
    TE  = "avg_separation"
  )

  # Build NGS panel enriched with ages, joined by player_id
  # Only seasons 2016+ can have NGS data
  ngs_positions_for_panel <- c("QB", "RB", "WR", "TE")
  # Map NGS position labels to panel position_group
  panel_ngs_subset <- panel %>%
    dplyr::filter(
      season %in% NGS_SEASONS,
      position_group %in% ngs_positions_for_panel,
      !is.na(age_at_season_start)
    ) %>%
    dplyr::select(
      player_id, season, player_name, position_group,
      age_at_season_start, games_played
    )

  panel_ngs_joined <- panel_ngs_subset %>%
    dplyr::left_join(
      ngs_panel %>% dplyr::select(
        player_id, season,
        cpoe, avg_separation, avg_yac_above_expectation,
        rush_yards_over_expected_per_att, ngs_efficiency
      ),
      by = c("player_id", "season")
    )

  rm(panel_ngs_subset)
  gc()

  # Compute deltas for each position using its primary NGS metric
  delta_ngs <- purrr::imap(ngs_metric_map, function(metric_col, pos) {

    if (!metric_col %in% names(panel_ngs_joined)) {
      warning(glue("NGS metric '{metric_col}' not found for {pos}. Skipping."))
      return(NULL)
    }

    pos_data <- panel_ngs_joined %>%
      dplyr::filter(
        position_group == pos,
        !is.na(.data[[metric_col]])
      )

    if (nrow(pos_data) == 0L) {
      warning(glue("No NGS data for position: {pos}"))
      return(NULL)
    }

    compute_age_deltas(
      panel_with_ages = pos_data %>%
        dplyr::rename(fp_per_game_ngs = dplyr::all_of(metric_col)) %>%
        dplyr::mutate(!!metric_col := fp_per_game_ngs),
      metric_col = metric_col,
      positions  = pos,
      age_min    = AGE_MIN,
      age_max    = AGE_MAX
    )
  })

  # ------------------------------------------------------------------
  # Step 7: Assumption validation
  # ------------------------------------------------------------------
  if (verbose) message("\nStep 7: Validating assumptions...")

  validation <- validate_aging_assumptions(
    panel_with_ages = panel,
    delta_data      = delta_boxscore,
    ngs_panel       = ngs_panel,
    verbose         = verbose
  )

  # ------------------------------------------------------------------
  # Step 8: Fit aging curves
  # ------------------------------------------------------------------
  if (verbose) message("\nStep 8: Fitting aging curves...")

  # Box score curves (2010-2025)
  if (verbose) message("  Fitting box score curves...")
  curves_boxscore <- purrr::set_names(
    purrr::map(CURVE_POSITIONS, function(pos) {
      if (verbose) message(glue("    {pos}..."))
      fit_aging_curves(
        delta_data   = delta_boxscore,
        position     = pos,
        baseline_age = AGE_BASELINE,
        min_obs      = MIN_AGE_OBS,
        loess_span   = LOESS_SPAN
      )
    }),
    CURVE_POSITIONS
  )

  # NGS efficiency curves (2016-2025)
  if (verbose) message("  Fitting NGS efficiency curves...")
  curves_ngs <- purrr::imap(ngs_metric_map, function(metric_col, pos) {

    pos_ngs_deltas <- delta_ngs[[pos]]
    if (is.null(pos_ngs_deltas) || nrow(pos_ngs_deltas) == 0L) {
      return(NULL)
    }

    if (verbose) message(glue("    {pos} ({metric_col})..."))

    # Rename delta data metric col to match what fit_aging_curves expects
    renamed_deltas <- pos_ngs_deltas %>%
      dplyr::mutate(position_group = pos)

    fit_aging_curves(
      delta_data   = renamed_deltas,
      position     = pos,
      baseline_age = AGE_BASELINE,
      min_obs      = MIN_AGE_OBS,
      loess_span   = LOESS_SPAN
    )
  })

  # ------------------------------------------------------------------
  # Step 9: KEY INSIGHTS (computed from data, not hardcoded)
  # ------------------------------------------------------------------
  if (verbose) message("\nStep 9: Key insights...")

  key_insights <- purrr::map_dfr(CURVE_POSITIONS, function(pos) {
    cr <- curves_boxscore[[pos]]
    if (is.null(cr)) {
      return(tibble::tibble(
        position = pos,
        peak_age_quad  = NA_integer_,
        peak_age_loess = NA_integer_,
        n_transitions  = 0L,
        n_sparse_ages  = 0L
      ))
    }
    tibble::tibble(
      position       = pos,
      peak_age_quad  = cr$peak_age_quad,
      peak_age_loess = cr$peak_age_loess,
      n_transitions  = nrow(delta_boxscore %>% dplyr::filter(position_group == pos)),
      n_sparse_ages  = length(cr$sparse_ages)
    )
  })

  if (verbose) {
    message("\n--- KEY INSIGHTS (box score curves, delta method) ---")
    for (i in seq_len(nrow(key_insights))) {
      message(glue(
        "  {key_insights$position[i]}: ",
        "Peak age (quadratic) = {key_insights$peak_age_quad[i]} | ",
        "Peak age (LOESS) = {key_insights$peak_age_loess[i]} | ",
        "n transitions = {format(key_insights$n_transitions[i], big.mark = ',')}"
      ))
    }
    message("-----------------------------------------------------")
  }

  # ------------------------------------------------------------------
  # Step 10: Visualizations
  # ------------------------------------------------------------------
  if (save_plots) {
    if (verbose) message("\nStep 10: Generating visualizations...")

    if (!dir.exists(OUTPUT_DIR)) {
      dir.create(OUTPUT_DIR, recursive = TRUE)
      if (verbose) message(glue("  Created output directory: {OUTPUT_DIR}"))
    }

    .plot_curves_boxscore(curves_boxscore, OUTPUT_DIR)
    .plot_age_deltas(curves_boxscore, OUTPUT_DIR)
    .plot_ngs_vs_boxscore(curves_boxscore, curves_ngs, OUTPUT_DIR)
    .plot_ar1_distribution(delta_boxscore, OUTPUT_DIR)

    if (verbose) {
      message(glue("\n  All plots saved to: {OUTPUT_DIR}"))
      message(glue("  Files: {FILE_PREFIX}aging_curves_boxscore.png"))
      message(glue("         {FILE_PREFIX}age_deltas_by_position.png"))
      message(glue("         {FILE_PREFIX}ngs_vs_boxscore_curves.png"))
      message(glue("         {FILE_PREFIX}ar1_mean_reversion.png"))
    }
  }

  if (verbose) {
    message("\n========================================")
    message("  Pipeline complete.")
    message("========================================\n")
  }

  list(
    panel_with_ages       = panel,
    delta_data_boxscore   = delta_boxscore,
    delta_data_ngs        = delta_ngs,
    ngs_panel             = ngs_panel,
    curves_boxscore       = curves_boxscore,
    curves_ngs            = curves_ngs,
    validation            = validation,
    key_insights          = key_insights
  )
}


# ==============================================================================
# EXAMPLE USAGE
# ==============================================================================
#
# Run the full pipeline from scratch:
#   results <- run_aging_curve_pipeline()
#
# Run with a pre-loaded panel (skip the ~20-minute build step):
#   panel <- build_player_season_panel(seasons = 2010:2025)
#   results <- run_aging_curve_pipeline(panel = panel)
#
# Inspect peak ages:
#   results$key_insights
#
# View box score aging curve for RBs:
#   results$curves_boxscore$RB$curve_data
#
# View validation report:
#   results$validation$report
#
# Rerun with plots off for faster iteration:
#   results <- run_aging_curve_pipeline(panel = panel, save_plots = FALSE)
#
# Check sparse age buckets (ages with fewer than MIN_AGE_OBS transitions):
#   results$curves_boxscore$QB$sparse_ages
#   results$curves_boxscore$RB$sparse_ages


# ==============================================================================
# SECTION: OUT-OF-SAMPLE CURVE GATE  [2026-07-15]
# ==============================================================================
#
# THE QUESTION
# ------------
# run_aging_curve_pipeline() can fit its curves two ways:
#
#   scoring_settings = NULL   LEGACY. compute_season_ppg()'s hardcoded PPR
#                             scale. Scoring-blind: identical curves for every
#                             league.
#   scoring_settings = list() SCORED. Play-by-play through R/17, so the curves
#                             measure the aging of THAT league's points.
#
# The scored curves are correct by construction: if you project a league's
# points, a curve fit on that league's points is the target rather than a proxy.
# But correctness is not the same as accuracy. Threshold bonuses are chunky and
# high-variance, and a curve fit on a noisier target can generalize worse. The
# scoring-blind hardcode may be accidentally smoothing.
#
# That is an empirical question and this is what settles it, per league.
#
# THE DESIGN
# ----------
# Temporal expanding window. For each test season t, fit both curves on
# transitions completing STRICTLY BEFORE t, then predict season t. Nothing the
# curve saw at fit time comes from t or later.
#
# Three arms, all starting from the SAME league-scored baseline, so the only
# thing that varies is which curve's delta is added:
#
#   null     pred = ppg[t-1]
#   legacy   pred = ppg[t-1] + legacy_curve(age)      <- PPR-scale delta
#   scored   pred = ppg[t-1] + scored_curve(age)      <- league-scale delta
#   actual   ppg[t] under the league's scoring
#
# The legacy arm deliberately adds a PPR-scale delta to a league-scale baseline.
# That is not a flaw in the test, it is what R/29 does in production today, so
# the test replicates the real usage rather than an idealized one.
#
# THE NULL ARM IS THE POINT. If both curves lose to "assume no change", the
# aging adjustment itself is the finding and which curve wins is moot. The same
# control retired the veteran talent multiplier and the coach prior.
#
# UNCERTAINTY
# -----------
# Player-clustered bootstrap. A player contributes up to one row per test
# season; treating those as independent manufactures significance. Resampling
# players rather than rows is what collapsed the apparent NGS RB edge.
#
# KNOWN LIMITATION: SURVIVORSHIP
# ------------------------------
# compute_age_deltas() applies MIN_CAREER_SEASONS using career length measured
# across the WHOLE panel. Computed once and split by season, the 2016 fold
# therefore contains only players who turned out to have long careers, which is
# information from after 2016.
#
# This inflates every arm's absolute accuracy. It does NOT bias the comparison:
# all three arms are scored on the identical row set, so the RANKING is sound
# while the absolute RMSE is optimistic. Read the deltas between arms, not the
# levels. Fixing it means recomputing career length per fold and is a separate
# item.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# Internal: year-over-year delta implied by a fitted curve, at given ages
# ------------------------------------------------------------------------------
# CRITICAL [2026-07-15]. fit_aging_curves() does NOT fit its LOESS on mean_delta.
# It fits on cumulative_value (R/23:947), a reconstructed production LEVEL
# anchored at 0 on AGE_BASELINE. So fitted_loess is a level, not a delta, and
# adding it to a per-game average is meaningless. The first version of this gate
# did exactly that and reported that both aging curves were significantly worse
# than no adjustment at every position. That result was entirely an artifact.
#
# cumulative_value is built as cumulative[i] = cumulative[i-1] + mean_delta[i]
# (R/23:903), so by construction:
#
#     C(a) - C(a-1) = mean_delta(a)
#
# The year-over-year delta for a player arriving at age a is therefore the
# DIFFERENCE of the fitted curve between a-1 and a. Transitions are consecutive
# seasons by construction in compute_age_deltas(), so age always advances by
# exactly 1 and this is well defined.
#
# fit_aging_curves() returns quad_model but not the loess model object, so the
# level lookup is a table read rather than predict(). Ages are integers, so it
# is exact wherever the training fold had coverage. Ages it did not cover fall
# back to the NEAREST covered age; dropping them instead would change the test
# sample between arms. Carry count is returned so it is reported, not hidden.
# ------------------------------------------------------------------------------
.lookup_curve_delta <- function(curve, ages, metric = c("loess", "quad")) {

  metric <- match.arg(metric)
  col    <- if (metric == "loess") "fitted_loess" else "fitted_quad"

  na_out <- list(delta = rep(NA_real_, length(ages)), n_carried = NA_integer_)

  if (is.null(curve) || is.null(curve$curve_data) ||
      !col %in% names(curve$curve_data)) {
    return(na_out)
  }

  grid <- curve$curve_data %>%
    dplyr::filter(!is.na(.data[[col]])) %>%
    dplyr::arrange(age_at_season_start) %>%
    dplyr::select(age_at_season_start, .fit = dplyr::all_of(col))

  if (nrow(grid) < 2L) return(na_out)

  # Level at an arbitrary age, nearest-age fallback outside coverage.
  .level_at <- function(a) {
    exact <- grid$.fit[match(a, grid$age_at_season_start)]
    need  <- is.na(exact) & !is.na(a)
    if (any(need)) {
      nearest <- vapply(
        a[need],
        function(x) which.min(abs(grid$age_at_season_start - x)),
        integer(1)
      )
      exact[need] <- grid$.fit[nearest]
    }
    list(v = exact, n_need = sum(need))
  }

  lv_now  <- .level_at(ages)
  lv_prev <- .level_at(ages - 1L)

  list(
    delta     = lv_now$v - lv_prev$v,
    n_carried = lv_now$n_need + lv_prev$n_need
  )
}


# ------------------------------------------------------------------------------
# Internal: panel with BOTH metrics attached, ages computed, positions split
# ------------------------------------------------------------------------------
.prepare_oos_panel <- function(panel,
                               scoring_settings,
                               seasons,
                               cache_dir,
                               game_cache_dir,
                               verbose) {

  if (verbose) message("\n  [1/4] Computing player ages...")
  panel <- compute_player_ages(panel, verbose = FALSE)

  if (verbose) message("  [2/4] Legacy metric (hardcoded PPR scale)...")
  legacy_panel <- compute_season_ppg(panel, ppr_value = 1)

  if (verbose) message("  [3/4] Scored metric (league scoring via R/17)...")
  if (!exists("attach_scored_ppg", mode = "function")) {
    r16_path <- here::here("R", "16_player_season_panel.R")
    if (!file.exists(r16_path)) {
      stop(glue("R/16_player_season_panel.R not found at: {r16_path}"))
    }
    source(r16_path)
  }
  scored_panel <- attach_scored_ppg(
    panel            = panel,
    seasons          = seasons,
    scoring_settings = scoring_settings,
    cache_dir        = cache_dir,
    game_cache_dir   = game_cache_dir,
    verbose          = verbose
  )

  out <- legacy_panel %>%
    dplyr::mutate(season = as.integer(season)) %>%
    dplyr::select(player_id, season, fp_legacy = fp_per_game) %>%
    dplyr::inner_join(
      scored_panel %>%
        dplyr::mutate(season = as.integer(season)) %>%
        dplyr::select(-dplyr::any_of("fp_legacy")),
      by = c("player_id", "season")
    ) %>%
    dplyr::rename(fp_scored = fp_per_game)

  # Both metrics divide by the same games_played, so a row present in one and
  # absent from the other means something upstream diverged. The inner_join
  # above would hide that by silently dropping it.
  if (nrow(out) != nrow(legacy_panel)) {
    stop(glue(
      ".prepare_oos_panel(): legacy panel has {nrow(legacy_panel)} rows but ",
      "only {nrow(out)} survived the join to the scored panel. The two metrics ",
      "should cover identical player-seasons. Investigate before proceeding."
    ), call. = FALSE)
  }

  if (verbose) message("  [4/4] Splitting WR_TE into WR and TE...")
  out <- split_wr_te_positions(out, verbose = FALSE)

  out
}


# ------------------------------------------------------------------------------
# Internal: one test season. Fit on season < t, predict t.
# ------------------------------------------------------------------------------
.aging_oos_fold <- function(deltas_legacy,
                            deltas_scored,
                            test_season,
                            positions,
                            curve_metric,
                            min_train_transitions,
                            verbose) {

  train_legacy <- deltas_legacy %>% dplyr::filter(season < test_season)
  train_scored <- deltas_scored %>% dplyr::filter(season < test_season)

  # Test rows must be IDENTICAL across arms, so join the two delta frames and
  # keep only transitions both represent. prev_scored is the shared baseline;
  # metric_scored is the target. The legacy columns are not used to predict,
  # only to confirm the row exists in both.
  test <- deltas_scored %>%
    dplyr::filter(season == test_season) %>%
    dplyr::select(player_id, player_name, position_group, season,
                  age_at_season_start,
                  actual = metric_value, base = prev_metric_value) %>%
    dplyr::semi_join(
      deltas_legacy %>% dplyr::filter(season == test_season),
      by = c("player_id", "season")
    )

  if (nrow(test) == 0L) return(NULL)

  res <- purrr::map_dfr(positions, function(p) {

    tr_l <- train_legacy %>% dplyr::filter(position_group == p)
    tr_s <- train_scored %>% dplyr::filter(position_group == p)
    te   <- test        %>% dplyr::filter(position_group == p)

    if (nrow(te) == 0L) return(NULL)

    if (nrow(tr_l) < min_train_transitions || nrow(tr_s) < min_train_transitions) {
      if (verbose) message(glue(
        "      {p}: only {min(nrow(tr_l), nrow(tr_s))} training transitions ",
        "(< {min_train_transitions}). Fold skipped for this position."
      ))
      return(NULL)
    }

    curve_l <- suppressWarnings(fit_aging_curves(tr_l, position = p))
    curve_s <- suppressWarnings(fit_aging_curves(tr_s, position = p))

    if (is.null(curve_l) || is.null(curve_s)) return(NULL)

    look_l <- .lookup_curve_delta(curve_l, te$age_at_season_start, curve_metric)
    look_s <- .lookup_curve_delta(curve_s, te$age_at_season_start, curve_metric)

    te %>%
      dplyr::mutate(
        pred_null   = base,
        pred_legacy = base + look_l$delta,
        pred_scored = base + look_s$delta,
        n_train_legacy = nrow(tr_l),
        n_train_scored = nrow(tr_s),
        n_carried_legacy = look_l$n_carried,
        n_carried_scored = look_s$n_carried
      )
  })

  if (is.null(res) || nrow(res) == 0L) return(NULL)

  # An arm with an NA prediction cannot be scored, and dropping the row from one
  # arm only would break the like-for-like comparison. Drop from ALL arms or
  # none.
  res %>%
    dplyr::filter(!is.na(actual), !is.na(pred_null),
                  !is.na(pred_legacy), !is.na(pred_scored))
}


# ------------------------------------------------------------------------------
# Internal: RMSE per position per arm on a given row set
# ------------------------------------------------------------------------------
.arm_rmse <- function(df) {
  df %>%
    dplyr::group_by(position_group) %>%
    dplyr::summarise(
      n           = dplyr::n(),
      rmse_null   = sqrt(mean((actual - pred_null)^2)),
      rmse_legacy = sqrt(mean((actual - pred_legacy)^2)),
      rmse_scored = sqrt(mean((actual - pred_scored)^2)),
      .groups = "drop"
    )
}


#' Out-of-Sample Gate: Does Scoring-Aware Aging Beat the Scoring-Blind Hardcode?
#'
#' @description
#' Settles, for a GIVEN league scoring, whether curves fit on that league's
#' points predict next season better than curves fit on
#' \code{compute_season_ppg()}'s hardcoded PPR scale, and whether either beats
#' applying no aging adjustment at all.
#'
#' Temporal expanding window: for each test season, both curves are fit only on
#' transitions completing strictly earlier, then used to predict that season.
#' All three arms predict the same target from the same baseline on the same
#' rows; only the added delta differs.
#'
#' @param panel Tibble. Output of \code{build_player_season_panel()}. Ages are
#'   computed internally; do not pre-compute them.
#' @param scoring_settings Named list in R/17's schema. REQUIRED. This is the
#'   league being tested. There is no default: a default would invite the same
#'   silent-wrong-scoring failure this gate exists to detect.
#' @param test_seasons Integer vector. Seasons to predict. Default 2016:2025.
#' @param positions Character vector. Default \code{CURVE_POSITIONS}.
#' @param curve_metric "loess" (default) or "quad". Which fitted curve to read.
#' @param min_train_transitions Integer. Minimum training transitions for a
#'   position-fold to be fit at all. Default 100.
#' @param n_boot Integer. Player-clustered bootstrap resamples. Default 1000.
#'   Set 0 to skip.
#' @param seed Integer. RNG seed for the bootstrap. Default 1234.
#' @param cache_dir,game_cache_dir Passed to \code{attach_scored_ppg()}.
#' @param verbose Logical.
#'
#' @return A named list:
#'   \describe{
#'     \item{predictions}{Tibble. One row per scored test transition, all arms.}
#'     \item{rmse}{Tibble. RMSE per position per arm, pooled across folds.}
#'     \item{by_season}{Tibble. RMSE per position per arm per test season.}
#'     \item{boot}{Tibble or NULL. Bootstrap CIs on the RMSE differences.}
#'     \item{carried}{Tibble. How often a curve delta was carried from a
#'       neighbouring age because the training fold lacked that age.}
#'     \item{scoring_settings}{The list that was tested.}
#'   }
#'
#' @seealso \code{\link{run_aging_curve_pipeline}}, \code{\link{fit_aging_curves}}
#'
#' @export
run_aging_curve_oos_gate <- function(
    panel,
    scoring_settings,
    test_seasons          = 2016:2025,
    positions             = CURVE_POSITIONS,
    curve_metric          = c("loess", "quad"),
    min_train_transitions = 100L,
    n_boot                = 1000L,
    seed                  = 1234L,
    cache_dir             = CACHE_DIR,
    game_cache_dir        = here::here("data", "season3_cache"),
    verbose               = TRUE
) {

  curve_metric <- match.arg(curve_metric)

  if (missing(scoring_settings) || is.null(scoring_settings)) {
    stop(
      "scoring_settings is required. This gate compares a league's own scoring ",
      "against the scoring-blind PPR hardcode; there is no meaningful default.",
      call. = FALSE
    )
  }
  if (!is.data.frame(panel) || nrow(panel) == 0L) {
    stop("panel must be a non-empty data frame from build_player_season_panel().")
  }

  panel_seasons <- sort(unique(as.integer(panel$season)))
  bad <- setdiff(test_seasons, panel_seasons)
  if (length(bad) > 0L) {
    stop(glue(
      "test_seasons not present in panel: {paste(bad, collapse = ', ')}"
    ), call. = FALSE)
  }
  if (min(test_seasons) <= min(panel_seasons) + 1L) {
    stop(glue(
      "test_seasons starts at {min(test_seasons)} but the panel starts at ",
      "{min(panel_seasons)}. The earliest fold would have no training data."
    ), call. = FALSE)
  }

  if (verbose) {
    message("\n========================================")
    message("  Aging Curve OOS Gate")
    message("========================================")
    message(glue("Panel        : {min(panel_seasons)}-{max(panel_seasons)}"))
    message(glue("Test seasons : {min(test_seasons)}-{max(test_seasons)}"))
    message(glue("Curve metric : {curve_metric}"))
    message(glue("Bootstrap    : {n_boot} player-clustered resamples"))
  }

  prepped <- .prepare_oos_panel(
    panel            = panel,
    scoring_settings = scoring_settings,
    seasons          = panel_seasons,
    cache_dir        = cache_dir,
    game_cache_dir   = game_cache_dir,
    verbose          = verbose
  )

  if (verbose) message("\n  Computing deltas for both metrics...")
  deltas_legacy <- compute_age_deltas(prepped, metric_col = "fp_legacy",
                                      positions = positions)
  deltas_scored <- compute_age_deltas(prepped, metric_col = "fp_scored",
                                      positions = positions)

  if (verbose) {
    message(glue(
      "    legacy: {format(nrow(deltas_legacy), big.mark = ',')} transitions | ",
      "scored: {format(nrow(deltas_scored), big.mark = ',')} transitions"
    ))
  }

  if (verbose) message("\n  Running folds...")
  folds <- purrr::map_dfr(sort(test_seasons), function(t) {
    if (verbose) message(glue("    Test season {t} (train: < {t})..."))
    .aging_oos_fold(
      deltas_legacy         = deltas_legacy,
      deltas_scored         = deltas_scored,
      test_season           = t,
      positions             = positions,
      curve_metric          = curve_metric,
      min_train_transitions = min_train_transitions,
      verbose               = verbose
    )
  })

  if (is.null(folds) || nrow(folds) == 0L) {
    stop("No test rows survived. Check test_seasons and min_train_transitions.",
         call. = FALSE)
  }

  rmse_pooled <- .arm_rmse(folds)

  by_season <- folds %>%
    dplyr::group_by(season, position_group) %>%
    dplyr::summarise(
      n           = dplyr::n(),
      rmse_null   = sqrt(mean((actual - pred_null)^2)),
      rmse_legacy = sqrt(mean((actual - pred_legacy)^2)),
      rmse_scored = sqrt(mean((actual - pred_scored)^2)),
      .groups = "drop"
    )

  carried <- folds %>%
    dplyr::group_by(season, position_group) %>%
    dplyr::summarise(
      n_rows           = dplyr::n(),
      n_carried_legacy = dplyr::first(n_carried_legacy),
      n_carried_scored = dplyr::first(n_carried_scored),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_carried_legacy > 0 | n_carried_scored > 0)

  # --------------------------------------------------------------------------
  # Player-clustered bootstrap on the RMSE DIFFERENCES
  # --------------------------------------------------------------------------
  # Resample PLAYERS with replacement and take all of a sampled player's rows.
  # A player appears in up to one row per test season and those rows are
  # correlated; resampling rows would treat them as independent and shrink the
  # interval until noise looks like signal.
  # --------------------------------------------------------------------------
  boot <- NULL
  if (n_boot > 0L) {
    if (verbose) message(glue("\n  Bootstrapping ({n_boot} resamples, clustered on player)..."))
    set.seed(seed)

    boot <- purrr::map_dfr(positions, function(p) {
      dp <- folds %>% dplyr::filter(position_group == p)
      if (nrow(dp) == 0L) return(NULL)

      players <- unique(dp$player_id)
      idx     <- split(seq_len(nrow(dp)), dp$player_id)

      .rmse <- function(a, pr) sqrt(mean((a - pr)^2))

      # Order is fixed explicitly rather than read off rownames(): vapply only
      # carries names through when FUN.VALUE is named, and a NULL rowname here
      # would silently produce a zero-row tibble.
      cmp_names <- c("scored_minus_legacy", "scored_minus_null",
                     "legacy_minus_null")

      draws <- vapply(seq_len(n_boot), function(b) {
        samp <- sample(players, length(players), replace = TRUE)
        rows <- unlist(idx[samp], use.names = FALSE)
        d    <- dp[rows, ]
        r_s  <- .rmse(d$actual, d$pred_scored)
        r_l  <- .rmse(d$actual, d$pred_legacy)
        r_n  <- .rmse(d$actual, d$pred_null)
        c(r_s - r_l, r_s - r_n, r_l - r_n)
      }, FUN.VALUE = numeric(3))

      r_s0 <- .rmse(dp$actual, dp$pred_scored)
      r_l0 <- .rmse(dp$actual, dp$pred_legacy)
      r_n0 <- .rmse(dp$actual, dp$pred_null)

      tibble::tibble(
        position   = p,
        comparison = cmp_names,
        point      = c(r_s0 - r_l0, r_s0 - r_n0, r_l0 - r_n0),
        ci_lo      = apply(draws, 1, stats::quantile, probs = 0.025, na.rm = TRUE),
        ci_hi      = apply(draws, 1, stats::quantile, probs = 0.975, na.rm = TRUE)
      ) %>%
        dplyr::mutate(crosses_zero = (ci_lo <= 0 & ci_hi >= 0))
    })
  }

  if (verbose) {
    message("\n--- POOLED RMSE (lower is better) ---")
    print(as.data.frame(rmse_pooled), row.names = FALSE)

    if (!is.null(boot)) {
      message("\n--- RMSE DIFFERENCES (negative = first arm better) ---")
      message("    CI crossing zero = no distinguishable difference.")
      print(as.data.frame(boot), row.names = FALSE)
    }

    if (nrow(carried) > 0L) {
      message(glue(
        "\n  NOTE: curve deltas carried from a neighbouring age in ",
        "{nrow(carried)} position-fold(s). See $carried."
      ))
    }

    message("\n  REMINDER: MIN_CAREER_SEASONS is applied across the whole panel,")
    message("  so absolute RMSE is optimistic (survivorship). All arms share the")
    message("  identical rows, so the RANKING holds. Read differences, not levels.")
    message("\n========================================\n")
  }

  list(
    predictions      = folds,
    rmse             = rmse_pooled,
    by_season        = by_season,
    boot             = boot,
    carried          = carried,
    scoring_settings = scoring_settings
  )
}
