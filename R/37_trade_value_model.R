# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 17
# Trade Value Model
# File: R/37_trade_value_model.R
#
# PURPOSE
# -------
# Produces a single, cross-position-comparable trade value per player per
# league. It is additive on top of the existing projection stack, not a
# rebuild. Four inputs combine:
#
#   1. Current projection value  -- R/33 adjusted_vorp (the base for veterans)
#   2. Aging trajectory          -- R/23 curves, applied forward over a horizon
#   3. Prospect upside           -- R/28 score_final (the base for rookies)
#   4. League scoring + format   -- R/19 (scoring already baked into VORP;
#                                   dynasty vs redraft derived here)
#
# Two populations are handled separately:
#   - Veterans  : already in R/33 VORP rankings (NFL projection history).
#                 trade_value = adjusted_vorp * aging_multiplier
#   - Rookies   : R/28 prediction-cohort players with no NFL projection, so
#                 they are NOT in VORP rankings. They are added as new rows;
#                 score_final is rescaled to VORP units and discounted for
#                 unproven risk.
#
# REDRAFT VS DYNASTY: HARD BRANCH (not a blend)
# ---------------------------------------------
# A league with Sleeper taxi_slots > 0 is dynasty; otherwise redraft. The
# branch sets the aging horizon (1 season redraft, 3 seasons dynasty) and the
# rookie discount (steeper for redraft, since an unproven rookie has fewer
# games to return value in a one-season window).
#
# THE AGING MULTIPLIER (the one piece of real math here)
# ------------------------------------------------------
# R/23 curves are CUMULATIVE fantasy-points-per-game deltas anchored at age
# 23 = 0. They are additive, can be negative, and cross zero, so a ratio of
# curve values is unstable. Instead the expected fppg change over the horizon
# is divided by the player's own absolute projection (r32_posterior_mu) to get
# the fraction of production retained or gained:
#
#   curve_delta_s    = curve(age + s) - curve(age)        for s in 1..horizon
#   mean_delta       = mean(curve_delta_s)                 (average over horizon)
#   aging_multiplier = 1 + mean_delta / r32_posterior_mu   (clipped)
#
# A 22-year-old still climbing gets a multiplier above 1.0 (dynasty rewards
# this). A 31-year-old RB on the decline gets a multiplier well below 1.0.
# Position matters automatically: R/23 fits a separate curve per position, so
# the steeper RB decline enters through the curve lookup with no separate
# positional tier. CROSS-POSITION comparability comes from VORP itself, which
# is already replacement-normalized per league roster configuration.
#
# WHAT IS DELIBERATELY NOT HERE (documented, not missing)
# -------------------------------------------------------
#   - Positional scarcity tier multipliers: VORP already normalizes by
#     replacement level per league, so a separate tier would double-count.
#   - NGS signal: belongs in the R/32 reconciliation layer, where it would
#     propagate forward into VORP and into this file automatically. Wiring it
#     in here would correct at the wrong layer. Deferred to R/32, not removed.
#   - Same-game / stack correlation in evaluate_trade: trades are summed as
#     independent values. A documented v2 limitation.
#
# OUTPUTS (only when save_output = TRUE)
# --------------------------------------
#   data/season2_cache/s2_week17_trade_values.rds / .csv
#
# SOURCE DEPENDENCIES (sourced below if their entry points are not loaded)
# ------------------------------------------------------------------------
#   R/19_sleeper_api.R       -- get_user_leagues(), connect_sleeper_league()
#   R/23_aging_curves.R      -- run_aging_curve_pipeline(), fit_aging_curves()
#   R/29_projection_engine.R -- aging-curve cache builder (fallback refit path)
#
# Consumed as data (read from cache, not sourced):
#   s2_week15_vorp_rankings.rds          (R/33) -- veteran base
#   s2_week14_final_prospect_scores.csv  (R/28) -- rookie base
#   s2_week15_reconciled_projections.rds (R/32) -- full 80% interval width
#   s2_week15_aging_curves_cache.rds     (R/29) -- aging curves (fallback refit)
#
# RUN
# ---
#   source(here::here("R", "37_trade_value_model.R"))
#
#   # Build the per-league trade value table (interactive league menu):
#   tv <- build_trade_value_table(username = "GABlancbeard")
#
#   # Evaluate a specific trade within one league:
#   res <- evaluate_trade(
#     give_ids    = c("00-0036322"),            # give away
#     receive_ids = c("00-0033873", "00-0034796"),  # receive
#     trade_values = tv,
#     league_name  = "Dynasty Main"
#   )
#   res$verdict        # "WIN" / "LOSS" / "ROUGHLY EVEN"
#   res$net_delta      # receive_value - give_value
#
# SCHEMA TAG: s2_w17_tradevalue_v1
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

# Null-coalescing infix (defined in R/19; redefine defensively so this file is
# self-contained if the R/19 chain has not been sourced yet).
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Source dependencies only if their entry points are not already in the session.
if (!exists("get_user_leagues") || !exists("connect_sleeper_league")) {
  source(here::here("R", "19_sleeper_api.R"))
}
if (!exists("run_aging_curve_pipeline") || !exists("fit_aging_curves")) {
  source(here::here("R", "23_aging_curves.R"))
}

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

# Decision / projection season. Matches R/33 SEASON_VORP and R/36 SEASON.
SEASON <- 2026L

# Last completed season the aging curves are fit through. This is the staleness
# anchor for the curve cache, NOT the projection season. R/29 fits curves on
# 2010:2025, so the panel's last season is 2025.
AGING_PANEL_LAST_SEASON <- 2025L

SCHEMA_TAG_TV <- "s2_w17_tradevalue_v1"

# Aging horizons (in seasons).
N_SEASONS_REDRAFT <- 1L
N_SEASONS_DYNASTY <- 3L

# Aging multiplier clips. The floor stops any asset from being valued below a
# fraction of its current VORP. Ceilings cap upside (boom_probability already
# carries ceiling in adjusted_vorp, so the aging multiplier is not the place
# for large upward swings).
AGING_MULTIPLIER_FLOOR           <- 0.40
AGING_MULTIPLIER_REDRAFT_CEILING <- 1.05
AGING_MULTIPLIER_DYNASTY_CEILING <- 1.35

# Rookie discounts on scaled prospect VORP. Redraft penalizes unproven players
# harder (fewer games to return value this season); dynasty rewards ceiling.
ROOKIE_DISCOUNT_REDRAFT <- 0.60
ROOKIE_DISCOUNT_DYNASTY <- 0.85

# Each additional point of 80% interval width shaves this fraction off a
# rookie's value (uncertainty discount). Floored so it never zeroes out.
INTERVAL_WIDTH_SCALE        <- 0.10
UNCERTAINTY_DISCOUNT_FLOOR  <- 0.50

# Verdict band for evaluate_trade, in trade-value points (tunable on first run).
TRADE_VERDICT_BAND <- 0.5

# Five-tier labels assigned by within-league trade_value quantile.
TV_TIER_BREAKS <- c(0, 0.20, 0.40, 0.65, 0.85, 1.0)
TV_TIER_LABELS <- c("Depth", "Handcuff", "Flex", "Starter", "Elite")

FILE_PREFIX <- "s2_week17_"
CACHE_DIR_DEFAULT <- here::here("data", "season2_cache")

VORP_CACHE_PATH        <- here::here("data", "season2_cache",
                                     "s2_week15_vorp_rankings.rds")
RECONCILED_CACHE_PATH  <- here::here("data", "season2_cache",
                                     "s2_week15_reconciled_projections.rds")
PROSPECT_CSV_PATH      <- here::here("data", "season2_cache",
                                     "s2_week14_final_prospect_scores.csv")
AGING_CURVES_CACHE_PATH <- here::here("data", "season2_cache",
                                      "s2_week15_aging_curves_cache.rds")

OUTPUT_RDS_PATH_TV <- here::here("data", "season2_cache",
                                 "s2_week17_trade_values.rds")
OUTPUT_CSV_PATH_TV <- here::here("data", "season2_cache",
                                 "s2_week17_trade_values.csv")

# Positions with fitted aging curves (mirrors R/23 CURVE_POSITIONS).
CURVE_POSITIONS_TV <- c("QB", "RB", "WR", "TE")

# ------------------------------------------------------------------------------
# NSE DECLARATIONS
# ------------------------------------------------------------------------------

utils::globalVariables(c(
  ".data", "nfl_gsis_id", "gsis_id", "player_name", "team", "position",
  "league_name", "league_format", "league_teams", "league_id", "is_dynasty",
  "r32_posterior_mu", "r32_projection_upper_80", "r32_projection_lower_80",
  "boom_probability", "bust_probability", "adjusted_vorp",
  "age_at_season_start", "birth_date", "sep1_date", "birth_date_parsed",
  "aging_multiplier", "rookie_flag", "prospect_score_final", "trade_value",
  "tv_tier", "overall_tv_rank", "position_tv_rank", "interval_width",
  "score_final", "cfb_player_name", "draft_class_type", "draft_year",
  "fitted_loess", "fitted_quad", "schema_tag", "tv_schema_tag",
  "scoring_type", "name", "total_rosters", "is_superflex", "status"
))

# ==============================================================================
# SECTION 1: AGING CURVE LOADING (with fallback refit)
# ==============================================================================

# ------------------------------------------------------------------------------
# .load_aging_curves_with_fallback
# ------------------------------------------------------------------------------

#' Load aging curves from cache, refitting from R/23 if stale or absent
#'
#' "Stale" for aging curves means one of: the cache file is absent, or the
#' cache was built for a different last-completed season than the current
#' AGING_PANEL_LAST_SEASON. Time elapsed is NOT a trigger: the curves are a
#' historical model fit, not live data. The cache is wrapped as a named list
#' with a built_for_season slot; R/29 writes a bare object, so the first R/37
#' run on a fresh clone refits and overwrites with the richer wrapped format.
#'
#' @param season Integer. Last completed season the curves should be fit
#'   through. Default AGING_PANEL_LAST_SEASON.
#' @param force_refit Logical. Always refit regardless of cache state.
#' @param verbose Logical. Print progress.
#' @return The curves_boxscore named list (keyed by position) from R/23.
#' @keywords internal
.load_aging_curves_with_fallback <- function(season = AGING_PANEL_LAST_SEASON,
                                             force_refit = FALSE,
                                             verbose = TRUE) {

  refit <- function() {
    if (verbose) {
      message(glue("  Refitting aging curves through season {season} (R/23)..."))
    }
    # The panel builder lives in R/16 and depends on R/15 and R/17. R/23 does
    # not source them, so the refit fallback must (mirrors R/29's loading).
    if (!exists("load_normalized_season")) {
      source(here::here("R", "15_multi_season_pbp.R"))
    }
    if (!exists("calculate_fantasy_points_ext")) {
      source(here::here("R", "17_extended_scoring.R"))
    }
    if (!exists("build_player_season_panel")) {
      source(here::here("R", "16_player_season_panel.R"))
    }
    panel  <- build_player_season_panel(
      seasons   = 2010:season,
      cache_dir = CACHE_DIR_DEFAULT,
      verbose   = verbose
    )
    result <- run_aging_curve_pipeline(panel = panel, save_plots = FALSE,
                                       verbose = verbose)
    curves <- result$curves_boxscore
    wrapped <- list(curves = curves, built_for_season = season)
    if (!dir.exists(dirname(AGING_CURVES_CACHE_PATH))) {
      dir.create(dirname(AGING_CURVES_CACHE_PATH), recursive = TRUE)
    }
    saveRDS(wrapped, AGING_CURVES_CACHE_PATH)
    if (verbose) message(glue("  Aging curves cached (wrapped format)."))
    curves
  }

  if (isTRUE(force_refit)) return(refit())

  if (!file.exists(AGING_CURVES_CACHE_PATH)) {
    if (verbose) message("  Aging curve cache absent; refitting.")
    return(refit())
  }

  cached <- readRDS(AGING_CURVES_CACHE_PATH)

  # Wrapped format from a prior R/37 run.
  if (is.list(cached) && !is.null(cached$built_for_season) &&
      !is.null(cached$curves)) {
    if (identical(as.integer(cached$built_for_season), as.integer(season))) {
      if (verbose) {
        message(glue("  Aging curves loaded from cache ",
                     "(built for season {season})."))
      }
      return(cached$curves)
    }
    if (verbose) {
      message(glue("  Aging curve cache built for season ",
                   "{cached$built_for_season}, need {season}; refitting."))
    }
    return(refit())
  }

  # Bare format written by R/29 (no metadata). Treat as usable for the current
  # season once, then rewrite wrapped so future runs can check freshness.
  if (verbose) {
    message("  Aging curve cache in bare R/29 format; adopting and rewrapping.")
  }
  wrapped <- list(curves = cached, built_for_season = season)
  saveRDS(wrapped, AGING_CURVES_CACHE_PATH)
  cached
}

# ==============================================================================
# SECTION 2: DYNASTY DETECTION
# ==============================================================================

# ------------------------------------------------------------------------------
# .detect_is_dynasty
# ------------------------------------------------------------------------------

#' Detect whether a Sleeper league is dynasty (vs redraft)
#'
#' Sleeper exposes no explicit dynasty flag. The reliable signal is a taxi
#' squad: dynasty leagues set taxi_slots > 0. connect_sleeper_league() (R/19)
#' returns settings as a named list; taxi_slots lives there. If the field is
#' absent or the API call fails, the league is treated as redraft with a
#' warning, so the function never blocks an offline run.
#'
#' @param league_id Character. Sleeper league ID.
#' @return Single logical. TRUE if dynasty.
#' @keywords internal
.detect_is_dynasty <- function(league_id) {
  meta <- tryCatch(connect_sleeper_league(league_id),
                   error = function(e) NULL)
  if (is.null(meta)) {
    warning(glue("Could not reach Sleeper for league {league_id}; ",
                 "treating as redraft."), call. = FALSE)
    return(FALSE)
  }
  taxi <- meta$settings$taxi_slots %||% NA
  taxi <- suppressWarnings(as.integer(taxi))
  if (is.na(taxi)) {
    warning(glue("League {league_id} has no taxi_slots field; ",
                 "treating as redraft."), call. = FALSE)
    return(FALSE)
  }
  taxi > 0L
}

# ==============================================================================
# SECTION 3: AGING MULTIPLIER (scalar; base if/else, not dplyr::if_else)
# ==============================================================================

# ------------------------------------------------------------------------------
# .curve_value_at_age
# ------------------------------------------------------------------------------

#' Read a fitted cumulative curve value at a given age
#'
#' Prefers the LOESS fit; falls back to the quadratic fit when LOESS is NA at
#' that age. Ages outside the fitted range clamp to the nearest age that has a
#' non-NA fitted value, so an old or very young player never returns NA purely
#' because the exact age bucket was sparse.
#'
#' @param curve_data Tibble. The curve_data element of a fit_aging_curves()
#'   result: columns age_at_season_start, fitted_loess, fitted_quad.
#' @param age Integer. Age to read at.
#' @return Numeric scalar (cumulative fppg delta relative to age 23), or NA.
#' @keywords internal
.curve_value_at_age <- function(curve_data, age) {
  if (is.null(curve_data) || is.na(age)) return(NA_real_)
  row <- curve_data[curve_data$age_at_season_start == age, , drop = FALSE]
  if (nrow(row) == 1L) {
    if (!is.na(row$fitted_loess)) return(row$fitted_loess)
    if (!is.na(row$fitted_quad))  return(row$fitted_quad)
  }
  valid <- curve_data[!is.na(curve_data$fitted_loess), , drop = FALSE]
  if (nrow(valid) == 0L) {
    valid <- curve_data[!is.na(curve_data$fitted_quad), , drop = FALSE]
    if (nrow(valid) == 0L) return(NA_real_)
    nearest <- valid$age_at_season_start[
      which.min(abs(valid$age_at_season_start - age))]
    return(valid$fitted_quad[valid$age_at_season_start == nearest])
  }
  nearest <- valid$age_at_season_start[
    which.min(abs(valid$age_at_season_start - age))]
  valid$fitted_loess[valid$age_at_season_start == nearest]
}

# ------------------------------------------------------------------------------
# .project_aging_multiplier
# ------------------------------------------------------------------------------

#' Fraction of current production an asset retains over a horizon
#'
#' Averages the expected fppg change across each season in the horizon, then
#' expresses it as a multiplier on the player's own absolute projection. See
#' the file header for the full derivation. Returns 1.0 (age-neutral) when age,
#' projection, or curve data are unavailable, rather than dropping the player.
#'
#' @param player_age Integer. Age as of Sept 1 of SEASON.
#' @param position Character. QB/RB/WR/TE.
#' @param curves Named list. curves_boxscore from R/23.
#' @param n_seasons Integer. Horizon (1 redraft, 3 dynasty).
#' @param posterior_mu Numeric. r32_posterior_mu, the absolute projection.
#' @param is_dynasty Logical. Selects the ceiling clip.
#' @return Numeric scalar multiplier, clipped to [floor, ceiling].
#' @keywords internal
.project_aging_multiplier <- function(player_age, position, curves,
                                      n_seasons, posterior_mu, is_dynasty) {
  if (is.na(player_age) || is.na(posterior_mu) || posterior_mu <= 0) {
    return(1.0)
  }
  if (!position %in% names(curves)) return(1.0)
  cd <- curves[[position]]$curve_data
  v_now <- .curve_value_at_age(cd, player_age)
  if (is.na(v_now)) return(1.0)

  deltas <- vapply(seq_len(n_seasons), function(s) {
    v_s <- .curve_value_at_age(cd, player_age + s)
    if (is.na(v_s)) NA_real_ else v_s - v_now
  }, numeric(1))

  if (all(is.na(deltas))) return(1.0)
  mean_delta <- mean(deltas, na.rm = TRUE)
  mult <- 1 + mean_delta / posterior_mu

  ceiling_clip <- if (isTRUE(is_dynasty)) {
    AGING_MULTIPLIER_DYNASTY_CEILING
  } else {
    AGING_MULTIPLIER_REDRAFT_CEILING
  }
  max(AGING_MULTIPLIER_FLOOR, min(ceiling_clip, mult))
}

# ==============================================================================
# SECTION 4: PLAYER AGES
# ==============================================================================

# ------------------------------------------------------------------------------
# .attach_player_ages
# ------------------------------------------------------------------------------

#' Attach age_at_season_start to a player table by GSIS id
#'
#' Replicates R/23's Sept 1 age convention exactly: age is floor of the year
#' fraction between Sept 1 of SEASON and the player's birth_date. Players with
#' no birth_date in the roster get NA age (handled downstream as age-neutral).
#'
#' @param df Tibble with an nfl_gsis_id column.
#' @param season Integer. Season year for the Sept 1 anchor. Default SEASON.
#' @return df with an age_at_season_start integer column added.
#' @keywords internal
.attach_player_ages <- function(df, season = SEASON) {
  # Free agents and recently unsigned veterans are not on active rosters for
  # the current season, so nflreadr::load_rosters(season) returns no row for
  # them. Birth dates do not change year to year, so prior-season rosters are
  # valid fallback sources. Cascade through the current season and the two
  # preceding seasons, accumulating any gsis_id not found in earlier passes.
  dob_lookup <- tibble::tibble(nfl_gsis_id = character(),
                               birth_date  = character())
  for (try_season in c(season, season - 1L, season - 2L)) {
    roster_try <- tryCatch(
      nflreadr::load_rosters(seasons = try_season),
      error = function(e) NULL
    )
    if (is.null(roster_try) ||
        !all(c("gsis_id", "birth_date") %in% names(roster_try))) {
      next
    }
    new_rows <- roster_try %>%
      dplyr::filter(!is.na(.data$gsis_id), !is.na(.data$birth_date),
                    !(.data$gsis_id %in% dob_lookup$nfl_gsis_id)) %>%
      dplyr::distinct(.data$gsis_id, .keep_all = TRUE) %>%
      dplyr::select(nfl_gsis_id = gsis_id, birth_date) %>%
      dplyr::mutate(birth_date = as.character(.data$birth_date))
    dob_lookup <- dplyr::bind_rows(dob_lookup, new_rows)
  }
  if (nrow(dob_lookup) == 0L) {
    warning("Could not load roster birth dates from any season; ages set to NA.",
            call. = FALSE)
    return(dplyr::mutate(df, age_at_season_start = NA_integer_))
  }
  sep1 <- as.Date(paste0(season, "-09-01"))
  df %>%
    dplyr::left_join(dob_lookup, by = "nfl_gsis_id") %>%
    dplyr::mutate(
      age_at_season_start = as.integer(
        floor(as.numeric(sep1 - as.Date(.data$birth_date)) / 365.25)
      )
    ) %>%
    dplyr::select(-birth_date)
}

# ==============================================================================
# SECTION 5: PROSPECT SCALING (rookies)
# ==============================================================================

# ------------------------------------------------------------------------------
# .scale_prospect_to_vorp_units
# ------------------------------------------------------------------------------

#' Map a rookie's prospect score to VORP units by within-position rank transfer
#'
#' Rookies have no VORP (no NFL projection). To place them on the same scale as
#' veterans, the Nth-best rookie at a position (by score_final) is mapped to the
#' adjusted_vorp of the Nth-best veteran at that position in the same league.
#' This is a rank transfer, not a regression, so it assumes no functional form
#' between score_final and VORP. The rookie discount applied later (not here)
#' is what reflects that an unproven rookie is worth less than the established
#' veteran at the same rank. When a position has more rookies than ranked
#' veterans, the surplus rookies floor at the minimum veteran VORP.
#'
#' @param rookies Tibble. Rows for one position: nfl_gsis_id, score_final.
#' @param position Character.
#' @param vorp_one_league Tibble. The VORP rankings for ONE league.
#' @return Numeric vector, same length / order as rookies, in VORP units.
#' @keywords internal
.scale_prospect_to_vorp_units <- function(rookies, position, vorp_one_league) {
  pos_arg <- position
  vet_vorp <- vorp_one_league %>%
    dplyr::filter(.data$position == pos_arg, !is.na(.data$adjusted_vorp)) %>%
    dplyr::arrange(dplyr::desc(.data$adjusted_vorp)) %>%
    dplyr::pull(.data$adjusted_vorp)

  if (length(vet_vorp) == 0L) {
    return(rep(NA_real_, nrow(rookies)))
  }

  rk_order <- order(rookies$score_final, decreasing = TRUE)
  scaled <- rep(NA_real_, nrow(rookies))
  vet_floor <- min(vet_vorp)
  for (i in seq_along(rk_order)) {
    idx <- rk_order[i]
    scaled[idx] <- if (i <= length(vet_vorp)) vet_vorp[i] else vet_floor
  }
  scaled
}

# ==============================================================================
# SECTION 6: CORE COMPUTATION
# ==============================================================================

# ------------------------------------------------------------------------------
# compute_player_trade_values
# ------------------------------------------------------------------------------

#' Compute trade values for one league's veterans and rookies
#'
#' Veterans (already in vorp_one_league) get adjusted_vorp scaled by the aging
#' multiplier. Rookies (R/28 prediction cohort, not in vorp_one_league) are
#' added as new rows with score_final rescaled to VORP units, then discounted
#' for unproven risk and projection-interval uncertainty.
#'
#' @param vorp_one_league Tibble. R/33 VORP rankings filtered to ONE league.
#'   Must contain nfl_gsis_id, player_name, team, position, adjusted_vorp,
#'   r32_posterior_mu, age_at_season_start.
#' @param rookies Tibble. R/28 prediction-cohort players: nfl_gsis_id,
#'   player_name, position, score_final, interval_width.
#' @param curves Named list. curves_boxscore from R/23.
#' @param league_is_dynasty Logical.
#' @return Tibble: all veterans and rookies for the league with trade_value and
#'   the supporting columns added. One row per player.
#' @export
compute_player_trade_values <- function(vorp_one_league, rookies, curves,
                                        league_is_dynasty) {

  n_seasons <- if (isTRUE(league_is_dynasty)) {
    N_SEASONS_DYNASTY
  } else {
    N_SEASONS_REDRAFT
  }
  rookie_discount <- if (isTRUE(league_is_dynasty)) {
    ROOKIE_DISCOUNT_DYNASTY
  } else {
    ROOKIE_DISCOUNT_REDRAFT
  }

  # ---- Veteran path ----------------------------------------------------------
  # Compute the multiplier as an explicit vector against the column vectors,
  # rather than referencing the .data pronoun inside a nested vapply (which is
  # fragile inside mutate). Then attach the derived columns.
  vets <- vorp_one_league
  vets$aging_multiplier <- vapply(
    seq_len(nrow(vets)),
    function(i) .project_aging_multiplier(
      player_age   = vets$age_at_season_start[i],
      position     = vets$position[i],
      curves       = curves,
      n_seasons    = n_seasons,
      posterior_mu = vets$r32_posterior_mu[i],
      is_dynasty   = league_is_dynasty
    ),
    numeric(1)
  )
  vets$rookie_flag          <- FALSE
  vets$prospect_score_final <- NA_real_
  # pmax(0, ...) floors below-replacement players at zero trade value before
  # applying the aging multiplier. Without this, a youth multiplier > 1 on a
  # negative adjusted_vorp makes the player look worse in dynasty than redraft,
  # which is the opposite of the intended behavior.
  vets$trade_value          <- pmax(0, vets$adjusted_vorp) * vets$aging_multiplier

  # ---- Rookie path -----------------------------------------------------------
  # Exclude rookies that already appear as veterans (a rookie who logged enough
  # NFL time to earn a VORP row is handled on the veteran path).
  rookies <- rookies %>%
    dplyr::filter(!(.data$nfl_gsis_id %in% vorp_one_league$nfl_gsis_id),
                  !is.na(.data$score_final))

  if (nrow(rookies) > 0L) {
    # Median interval width per position, from veterans, to fill rookies that
    # have no R/32 row of their own.
    pos_median_width <- vorp_one_league %>%
      dplyr::mutate(interval_width = .data$r32_projection_upper_80 -
                      .data$r32_posterior_mu) %>%
      dplyr::group_by(.data$position) %>%
      dplyr::summarise(med_width = stats::median(.data$interval_width,
                                                 na.rm = TRUE),
                       .groups = "drop")

    rookie_rows <- purrr::map_dfr(CURVE_POSITIONS_TV, function(pos) {
      pos_rookies <- dplyr::filter(rookies, .data$position == pos)
      if (nrow(pos_rookies) == 0L) return(NULL)

      scaled <- .scale_prospect_to_vorp_units(pos_rookies, pos, vorp_one_league)
      med_w  <- pos_median_width$med_width[pos_median_width$position == pos]
      med_w  <- if (length(med_w) == 0L || is.na(med_w)) 0 else med_w

      iw  <- dplyr::coalesce(pos_rookies$interval_width, med_w)
      unc <- pmax(UNCERTAINTY_DISCOUNT_FLOOR, 1 - iw * INTERVAL_WIDTH_SCALE)
      tv  <- scaled * rookie_discount * unc

      tibble::tibble(
        nfl_gsis_id             = pos_rookies$nfl_gsis_id,
        player_name             = pos_rookies$player_name,
        team                    = "ROOKIE",
        position                = pos,
        league_name             = vets$league_name[1],
        league_format           = vets$league_format[1],
        r32_posterior_mu        = NA_real_,
        r32_projection_upper_80 = NA_real_,
        adjusted_vorp           = scaled,
        age_at_season_start     = NA_integer_,
        aging_multiplier        = NA_real_,
        rookie_flag             = TRUE,
        prospect_score_final    = pos_rookies$score_final,
        trade_value             = tv
      )
    })

    if (is.null(rookie_rows) || nrow(rookie_rows) == 0L) rookie_rows <- NULL
  } else {
    rookie_rows <- NULL
  }

  # ---- Combine ---------------------------------------------------------------
  vet_keep <- vets %>%
    dplyr::select(
      nfl_gsis_id, player_name, team, position,
      league_name, league_format,
      r32_posterior_mu, r32_projection_upper_80,
      adjusted_vorp, age_at_season_start,
      aging_multiplier, rookie_flag, prospect_score_final, trade_value
    )

  combined <- if (is.null(rookie_rows)) vet_keep else {
    dplyr::bind_rows(vet_keep, rookie_rows)
  }

  combined %>% dplyr::filter(!is.na(.data$trade_value))
}

# ------------------------------------------------------------------------------
# .assign_tiers_and_ranks
# ------------------------------------------------------------------------------

#' Assign within-league trade-value tiers and ranks
#'
#' Tiers come from quantile cuts of trade_value within the league. Overall rank
#' is a dense rank across the league; position rank resets per position.
#'
#' @param league_tv Tibble. One league's combined trade values.
#' @return league_tv with tv_tier, overall_tv_rank, position_tv_rank added.
#' @keywords internal
.assign_tiers_and_ranks <- function(league_tv) {
  # Players floored to zero trade value (below replacement) get "Depth" directly
  # without entering the quantile calculation. Including them would collapse the
  # lower breakpoints through the unique() guard, leaving fewer bins than labels
  # and mislabeling the top tier. Tiering the positive-TV population separately
  # restores the full five-tier distribution within the tradeable player set.
  league_tv$tv_tier <- "Depth"
  pos_mask <- league_tv$trade_value > 0

  if (sum(pos_mask) >= 5L) {
    pos_vals <- league_tv$trade_value[pos_mask]
    brks <- stats::quantile(pos_vals, probs = TV_TIER_BREAKS, na.rm = TRUE)
    brks <- unique(brks)
    if (length(brks) < 3L) {
      league_tv$tv_tier[pos_mask] <- TV_TIER_LABELS[length(TV_TIER_LABELS)]
    } else {
      labs <- TV_TIER_LABELS[seq_len(length(brks) - 1L)]
      league_tv$tv_tier[pos_mask] <- as.character(
        cut(pos_vals, breaks = brks, labels = labs, include.lowest = TRUE)
      )
    }
  }

  league_tv %>%
    dplyr::arrange(dplyr::desc(.data$trade_value)) %>%
    dplyr::mutate(overall_tv_rank = dplyr::row_number()) %>%
    dplyr::group_by(.data$position) %>%
    dplyr::mutate(position_tv_rank = dplyr::row_number()) %>%
    dplyr::ungroup()
}

# ==============================================================================
# SECTION 7: LEAGUE SELECTION MENU
# ==============================================================================

# ------------------------------------------------------------------------------
# .build_league_menu
# ------------------------------------------------------------------------------

#' Enumerate a user's leagues, mark dynasty status, and prompt for selection
#'
#' The user never needs a league_id. This pulls the user's leagues from R/19,
#' tags each with is_dynasty, restricts to leagues that actually have VORP
#' rankings on disk (since trade value is built on those), prints a numbered
#' menu, and reads the selection. In a non-interactive session it returns all
#' matching leagues without prompting.
#'
#' @param username Character. Sleeper username.
#' @param season Integer. Season for the league lookup.
#' @param vorp_league_names Character vector. League names present in the VORP
#'   rankings cache.
#' @param verbose Logical.
#' @return Tibble of selected leagues: league_id, name, is_dynasty.
#' @keywords internal
.build_league_menu <- function(username, season, vorp_league_names,
                               verbose = TRUE) {
  leagues <- get_user_leagues(username, season = season)
  if (is.null(leagues) || nrow(leagues) == 0L) {
    stop(glue("No Sleeper leagues found for '{username}' in {season}."),
         call. = FALSE)
  }

  # Keep only leagues that have VORP rankings to build trade value on.
  leagues <- leagues %>%
    dplyr::filter(.data$name %in% vorp_league_names)
  if (nrow(leagues) == 0L) {
    stop(glue("None of {username}'s {season} leagues match a league in the ",
              "VORP rankings cache. Build R/33 for these leagues first."),
         call. = FALSE)
  }

  # Tag dynasty status (one connect call per league).
  leagues$is_dynasty <- vapply(leagues$league_id, .detect_is_dynasty,
                               logical(1))

  fmt_line <- function(i) {
    lg <- leagues[i, ]
    glue("  [{i}]  {lg$name} | {toupper(lg$scoring_type)} | ",
         "{if (lg$is_dynasty) 'Dynasty' else 'Redraft'} | ",
         "{lg$total_rosters} teams")
  }

  if (!interactive()) {
    if (verbose) {
      message(glue("Non-interactive session: using all {nrow(leagues)} ",
                   "matching league(s)."))
    }
    return(leagues %>%
             dplyr::select(league_id, name, is_dynasty))
  }

  cat(glue("\nAvailable leagues for {username} ({season} season):\n\n"))
  for (i in seq_len(nrow(leagues))) cat(fmt_line(i), "\n")
  cat("\nEnter league numbers separated by commas, or \"all\": ")
  sel <- trimws(readline())

  if (tolower(sel) == "all" || nchar(sel) == 0L) {
    chosen <- seq_len(nrow(leagues))
  } else {
    chosen <- suppressWarnings(
      as.integer(trimws(strsplit(sel, ",")[[1]]))
    )
    chosen <- chosen[!is.na(chosen) & chosen >= 1L & chosen <= nrow(leagues)]
    if (length(chosen) == 0L) {
      stop("No valid league numbers entered.", call. = FALSE)
    }
  }

  leagues[chosen, ] %>%
    dplyr::select(league_id, name, is_dynasty)
}

# ==============================================================================
# SECTION 8: ORCHESTRATION
# ==============================================================================

# ------------------------------------------------------------------------------
# build_trade_value_table
# ------------------------------------------------------------------------------

#' Build the per-league trade value table
#'
#' Loads the VORP rankings (R/33), prospect scores (R/28), reconciled intervals
#' (R/32), and aging curves (R/23 via cache), prompts the user to pick which of
#' their leagues to value, computes veteran and rookie trade values per league
#' with the right dynasty branch, tiers and ranks within each league, and
#' optionally saves the result.
#'
#' @param username Character. Sleeper username (used to enumerate leagues and
#'   detect dynasty status). The user never supplies a league_id.
#' @param season Integer. Season for league lookup. Default SEASON.
#' @param force_refit Logical. Force an aging-curve refit. Default FALSE.
#' @param save_output Logical. Write RDS and CSV. Default TRUE.
#' @param cache_dir Character. Cache directory. Default CACHE_DIR_DEFAULT.
#' @param verbose Logical. Default TRUE.
#' @return Tibble: one row per player per selected league, sorted by
#'   overall_tv_rank within each league.
#' @seealso compute_player_trade_values, evaluate_trade
#' @export
build_trade_value_table <- function(username,
                                    season      = SEASON,
                                    force_refit = FALSE,
                                    save_output = TRUE,
                                    cache_dir   = CACHE_DIR_DEFAULT,
                                    verbose     = TRUE) {

  if (verbose) message(glue("Building trade values (schema {SCHEMA_TAG_TV})..."))

  # ---- Load inputs -----------------------------------------------------------
  if (!file.exists(VORP_CACHE_PATH)) {
    stop(glue("VORP rankings cache not found: {VORP_CACHE_PATH}. ",
              "Run R/33 first."), call. = FALSE)
  }
  vorp <- readRDS(VORP_CACHE_PATH)

  if (!file.exists(PROSPECT_CSV_PATH)) {
    stop(glue("Prospect scores not found: {PROSPECT_CSV_PATH}. ",
              "Run R/28 first."), call. = FALSE)
  }
  prospects_raw <- readr::read_csv(PROSPECT_CSV_PATH, show_col_types = FALSE)

  # Reconciled intervals (R/32): supplies lower_80, which R/33 does not carry.
  reconciled <- if (file.exists(RECONCILED_CACHE_PATH)) {
    readRDS(RECONCILED_CACHE_PATH)
  } else {
    if (verbose) {
      message("  R/32 reconciled cache absent; rookie interval widths will ",
              "fall back to position medians from VORP upper bounds.")
    }
    NULL
  }

  curves <- .load_aging_curves_with_fallback(
    season = AGING_PANEL_LAST_SEASON, force_refit = force_refit,
    verbose = verbose
  )

  # ---- Prepare rookie table (prediction cohort only) -------------------------
  if (!"nfl_gsis_id" %in% names(prospects_raw)) {
    prospects_raw$nfl_gsis_id <- NA_character_
  }
  rookies <- prospects_raw %>%
    dplyr::filter(.data$draft_class_type == "prediction",
                  !is.na(.data$score_final)) %>%
    dplyr::transmute(
      nfl_gsis_id = .data$nfl_gsis_id,
      player_name = .data$cfb_player_name,
      position    = .data$position,
      score_final = .data$score_final
    )

  # Attach a per-rookie interval width if R/32 has a row for them; otherwise
  # left NA here and filled with a position median inside the per-league loop.
  if (!is.null(reconciled) &&
      all(c("nfl_gsis_id", "r32_projection_upper_80",
            "r32_projection_lower_80") %in% names(reconciled))) {
    rk_width <- reconciled %>%
      dplyr::filter(!is.na(.data$nfl_gsis_id)) %>%
      dplyr::transmute(
        nfl_gsis_id,
        interval_width = .data$r32_projection_upper_80 -
          .data$r32_projection_lower_80
      )
    rookies <- rookies %>%
      dplyr::left_join(rk_width, by = "nfl_gsis_id")
  } else {
    rookies$interval_width <- NA_real_
  }

  # ---- Attach veteran ages once ----------------------------------------------
  vorp <- .attach_player_ages(vorp, season = season)

  # ---- Select leagues --------------------------------------------------------
  vorp_league_names <- unique(vorp$league_name)
  selected <- .build_league_menu(username, season, vorp_league_names,
                                 verbose = verbose)

  # ---- Compute per league ----------------------------------------------------
  out <- purrr::map_dfr(seq_len(nrow(selected)), function(i) {
    lg_name    <- selected$name[i]
    lg_dynasty <- selected$is_dynasty[i]
    vorp_one   <- dplyr::filter(vorp, .data$league_name == lg_name)
    if (nrow(vorp_one) == 0L) return(NULL)

    league_tv <- compute_player_trade_values(
      vorp_one_league   = vorp_one,
      rookies           = rookies,
      curves            = curves,
      league_is_dynasty = lg_dynasty
    )
    league_tv <- .assign_tiers_and_ranks(league_tv)
    league_tv$is_dynasty <- lg_dynasty
    league_tv
  })

  out <- out %>%
    dplyr::mutate(tv_schema_tag = SCHEMA_TAG_TV) %>%
    dplyr::select(
      league_name, league_format, is_dynasty,
      nfl_gsis_id, player_name, team, position,
      age_at_season_start, adjusted_vorp, aging_multiplier,
      rookie_flag, prospect_score_final,
      trade_value, tv_tier, overall_tv_rank, position_tv_rank,
      tv_schema_tag
    )

  # ---- Save ------------------------------------------------------------------
  if (isTRUE(save_output)) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    saveRDS(out, OUTPUT_RDS_PATH_TV)
    readr::write_csv(out, OUTPUT_CSV_PATH_TV)
    if (verbose) {
      message(glue("  Saved: {OUTPUT_RDS_PATH_TV}"))
      message(glue("  Saved: {OUTPUT_CSV_PATH_TV}"))
    }
  }

  .print_tv_summary(out, selected)
  out
}

# ------------------------------------------------------------------------------
# .print_tv_summary
# ------------------------------------------------------------------------------

#' Console summary for build_trade_value_table
#'
#' @param out Tibble. The full trade value table.
#' @param selected Tibble. Selected leagues with is_dynasty.
#' @keywords internal
.print_tv_summary <- function(out, selected) {
  message(glue("\n{strrep('=', 70)}"))
  message(glue("TRADE VALUE SUMMARY (schema {SCHEMA_TAG_TV})"))
  message(glue("{strrep('=', 70)}"))
  for (i in seq_len(nrow(selected))) {
    lg_name <- selected$name[i]
    lg_out  <- dplyr::filter(out, .data$league_name == lg_name)
    if (nrow(lg_out) == 0L) next
    n_rookies <- sum(lg_out$rookie_flag, na.rm = TRUE)
    fmt <- if (isTRUE(selected$is_dynasty[i])) "Dynasty" else "Redraft"
    message(glue("\n  {lg_name} ({fmt}) | {nrow(lg_out)} players | ",
                 "{n_rookies} rookies"))
    top5 <- lg_out %>% dplyr::arrange(.data$overall_tv_rank) %>%
      dplyr::slice_head(n = 5L)
    for (j in seq_len(nrow(top5))) {
      r <- top5[j, ]
      tag <- if (isTRUE(r$rookie_flag)) " (R)" else ""
      message(glue("    {r$overall_tv_rank}. {r$player_name}{tag} ",
                   "({r$position}) TV ",
                   "{format(round(r$trade_value, 1), nsmall = 1)} ",
                   "[{r$tv_tier}]"))
    }
  }
  message(glue("\n{strrep('=', 70)}\n"))
}

# ==============================================================================
# SECTION 9: TRADE EVALUATION (Shiny interface; pure function)
# ==============================================================================

# ------------------------------------------------------------------------------
# evaluate_trade
# ------------------------------------------------------------------------------

#' Evaluate a proposed trade for one league
#'
#' Sums trade_value on each side and reports the net delta and a verdict. Pure:
#' no I/O, no side effects. Unknown player IDs are warned about and excluded
#' from the sum rather than erroring, so a Shiny caller never crashes on a
#' stale id. Same-game correlation between the players is not modeled (v2).
#'
#' @param give_ids Character vector. nfl_gsis_id of players traded away.
#' @param receive_ids Character vector. nfl_gsis_id of players received.
#' @param trade_values Tibble. Output of build_trade_value_table().
#' @param league_name Character or NULL. If NULL, uses the first league present.
#' @return Named list: give_value, receive_value, net_delta, give_detail,
#'   receive_detail, verdict.
#' @seealso build_trade_value_table
#' @export
evaluate_trade <- function(give_ids, receive_ids, trade_values,
                           league_name = NULL) {

  target_league <- if (is.null(league_name)) {
    trade_values$league_name[1]
  } else {
    league_name
  }
  tv <- dplyr::filter(trade_values, .data$league_name == target_league)
  if (nrow(tv) == 0L) {
    stop(glue("No trade values found for league '{target_league}'."),
         call. = FALSE)
  }

  side_detail <- function(ids) {
    found   <- tv %>% dplyr::filter(.data$nfl_gsis_id %in% ids)
    missing <- setdiff(ids, found$nfl_gsis_id)
    if (length(missing) > 0L) {
      warning(glue("Not in '{target_league}' (excluded): ",
                   "{paste(missing, collapse = ', ')}"), call. = FALSE)
    }
    found %>%
      dplyr::select(player_name, position, trade_value, tv_tier)
  }

  give_detail    <- side_detail(give_ids)
  receive_detail <- side_detail(receive_ids)

  give_value    <- sum(give_detail$trade_value, na.rm = TRUE)
  receive_value <- sum(receive_detail$trade_value, na.rm = TRUE)
  net_delta     <- receive_value - give_value

  verdict <- if (net_delta > TRADE_VERDICT_BAND) {
    "WIN"
  } else if (net_delta < -TRADE_VERDICT_BAND) {
    "LOSS"
  } else {
    "ROUGHLY EVEN"
  }

  list(
    give_value     = give_value,
    receive_value  = receive_value,
    net_delta      = net_delta,
    give_detail    = give_detail,
    receive_detail = receive_detail,
    verdict        = verdict
  )
}
