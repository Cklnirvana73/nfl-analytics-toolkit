# ==============================================================================
# R/45  PRIOR-SOURCE BLEND-WEIGHT OUT-OF-SAMPLE BACKTEST HARNESS
# ==============================================================================
#
# PURPOSE
# -------
# R/32 blends the R/31 volume-implied projection against the R/29 posterior:
#   r32 = w * volume_implied + (1 - w) * r29_posterior
# where w = blend_weight_r31 is judgment-set per prior_source (RB/WR/TE 0.30 to
# 0.85). Those weights are the last unvalidated first-order lever under VORP.
# This harness tunes w per prior_source out of sample, per holdout year, with
# strict per-player leakage control (A2: translation re-fit per year, aging held
# at production).
#
# SCORING IS AN INPUT, NEVER HARDCODED
# ------------------------------------
# Every quantity is computed under a scoring config passed in (R/17's schema, the
# same one R/42 resolves Sleeper leagues into). The first run uses DK Best Ball;
# other leagues drop in their own config later. The tuning target is the realized
# actual under that config, bonuses included. It is reported two ways, full
# (bonuses on) and core (yardage bonuses off), so the effect of the bonuses on
# the tuned weights is measured, not assumed.
#
# PROJECTIONS STAY BONUS-FREE (engine limit, acknowledged)
# --------------------------------------------------------
# R/29 cannot price play-level bonuses at the aggregate prior level (its own
# note, lines 1822-1828). So the two projection arms carry only linear scoring.
# Bonus-aware projection is a separate downstream layer (expected-bonus), built
# after this harness; it sits after the blend, like R/33's ceiling, so it does
# not disturb the weights tuned here.
#
# STAGES (each self-contained and independently runnable)
# -------------------------------------------------------
#   1  build_actual_ppg()      realized full + core PPG per player-season   <-- THIS FILE
#   2  r29 arm                 build_projection_priors per holdout year (preseason,
#                              per-year translation preds, panel < ty)
#   3  vi arm                  R/30 -> R/31 per holdout year, R/32 volume-implied
#   4  sweep + bootstrap       forward-chained w per prior_source vs current,
#                              both targets (same-rows argmin kept as a
#                              clearly-labeled in-sample reference)
#
# Stages 2-4 are added after Stage 1 verifies. DK Best Ball scoring verified
# 2026-07-11 against DraftKings DFS rules.
# ==============================================================================

library(dplyr)
library(tibble)
library(glue)
library(here)
library(nflreadr)

source(here::here("R", "17_extended_scoring.R"))


# ------------------------------------------------------------------------------
# SCORING CONFIGS (R/17 schema). First run: DK Best Ball.
# ------------------------------------------------------------------------------
# DK_BEST_BALL_SCORING is defined ONCE, in R/17, and arrives via the
# source() at the top of this file. [2026-07-16]
#
# It used to be defined here AND in R/46:203 as two separate lists. The values
# were identical so no board was ever wrong, but R has no notion of a constant
# defined twice: whichever file sourced last silently overwrote the other. That
# became live when R/17's signature changed, since this copy still held the
# retired long_td_bonus / long_td_threshold. Same failure mode as
# DEFAULT_SCORING_SETTINGS in R/29 and R/32.
#
# Do not redefine it here. If you need a variant, derive it:
#   my_scoring <- utils::modifyList(DK_BEST_BALL_SCORING, list(pass_td = 6))
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# build_actual_ppg  (Stage 1)
# ------------------------------------------------------------------------------

#' Realized per-game fantasy points per player-season under a scoring config.
#'
#' Runs R/17 at the play/game level so the yardage bonuses are applied per game
#' before aggregation, which is the only correct way to price them. Returns both
#' the full (bonuses on) and core (yardage bonuses removed) per-game averages, by
#' subtracting R/17's isolated bonus components from the total.
#'
#' @param target_years Integer vector of seasons.
#' @param scoring Named list in R/17's schema. Default DK_BEST_BALL_SCORING.
#' @param use_roster Logical. Pass rosters to R/17 so position is roster-anchored
#'   (stable) rather than per-play inferred. Default TRUE.
#' @param min_games Integer floor on games played. Default 1L.
#' @param verbose Logical.
#'
#' @return tibble(player_id, player_name, position, season, games,
#'   ppg_full, ppg_core). ppg_full includes the config's yardage bonuses;
#'   ppg_core removes the 100/200-yard and 300/400-pass bonuses only.
build_actual_ppg <- function(target_years,
                             scoring    = DK_BEST_BALL_SCORING,
                             use_roster = TRUE,
                             min_games  = 1L,
                             verbose    = TRUE) {

  # --------------------------------------------------------------------------
  # CONSOLIDATION [2026-07-15]
  # --------------------------------------------------------------------------
  # This function used to call nflreadr::load_pbp() and calculate_fantasy_
  # points_ext() itself. R/16's build_player_game_panel() now does exactly that
  # (load pbp -> score through R/17 -> player-game rows), with per-season,
  # per-scoring caching and staleness detection against the source PBP. Two
  # copies of the same scoring path would drift, so the load-and-score step is
  # delegated.
  #
  # The AGGREGATION below is deliberately unchanged. In particular `games` is
  # still n_distinct(game_id) off R/17's own output, NOT R/16's season panel
  # games_played. Those differ (a player can appear in the panel for a game in
  # which he recorded no scoring-relevant play), and ppg_full feeds the blend
  # weights, so this denominator is load-bearing and stays as it was.
  #
  # ONE BEHAVIOR CHANGE: the PBP source moves from nflreadr::load_pbp() to
  # R/15's normalized per-season cache. R/15 keeps every raw nflfastR column and
  # only adds missing optionals as NA, so these are expected to be equivalent,
  # but that is an expectation and not a proof. Verify with the equivalence
  # check before trusting any weight produced after this change.
  # --------------------------------------------------------------------------

  if (!exists("build_player_game_panel", mode = "function")) {
    r16_path <- here::here("R", "16_player_season_panel.R")
    if (!file.exists(r16_path)) {
      stop(glue("R/16_player_season_panel.R not found at: {r16_path}"))
    }
    if (verbose) message("  Sourcing R/16_player_season_panel.R...")
    source(r16_path)
  }

  out <- lapply(target_years, function(ty) {
    if (verbose) message(glue("  actual: {ty} pbp + scoring..."))

    pg <- build_player_game_panel(
      seasons          = ty,
      scoring_settings = scoring,
      use_roster       = use_roster,
      verbose          = verbose
    )

    if (!"total_fantasy_points" %in% names(pg)) {
      stop("build_actual_ppg(): R/17 did not return total_fantasy_points. ",
           "Columns: ", paste(head(names(pg), 30), collapse = ", "))
    }

    # yardage-bonus components R/17 isolates (0 when the config zeroes them)
    hb <- if ("hundred_yard_fantasy_points" %in% names(pg))
            dplyr::coalesce(pg$hundred_yard_fantasy_points, 0) else 0
    pm <- if ("pass_milestone_fantasy_points" %in% names(pg))
            dplyr::coalesce(pg$pass_milestone_fantasy_points, 0) else 0

    pg %>%
      dplyr::mutate(
        .bonus_pts = hb + pm,
        .core_pts  = .data$total_fantasy_points - .bonus_pts
      ) %>%
      dplyr::group_by(.data$player_id) %>%
      dplyr::summarise(
        player_name = dplyr::first(.data$player_name),
        position    = dplyr::first(.data$position),
        games       = dplyr::n_distinct(.data$game_id),
        total_full  = sum(.data$total_fantasy_points, na.rm = TRUE),
        total_core  = sum(.data$.core_pts, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        season   = ty,
        ppg_full = total_full / games,
        ppg_core = total_core / games
      ) %>%
      dplyr::filter(games >= min_games) %>%
      dplyr::select(player_id, player_name, position, season, games,
                    ppg_full, ppg_core)
  })

  res <- dplyr::bind_rows(out)
  if (verbose) {
    message(glue("  build_actual_ppg(): {nrow(res)} player-seasons, ",
                 "{min(target_years)}-{max(target_years)}."))
  }
  res
}


# ==============================================================================
# STAGE 2: r29 arm  (preseason posterior per holdout year, leak-safe history)
# ==============================================================================
# build_projection_priors(season = ty) builds each veteran's history prior from
# seasons (ty-3):(ty-1) only (R/29 line 1343), so the history-based prior_source
# buckets are leakage-clean with no panel surgery. It also computes that history
# at game level through R/17, so it honors the scoring config passed in.
#
# The two arms are compared on a common BONUS-FREE basis (bonuses cannot be
# priced in an aggregate projection anyway), so the arm scoring is dk_core_of()
# of the target config. The yardage bonuses live only in the Stage 1 target,
# where the full-vs-core sensitivity measures whether they move the weights.
#
# translation_path / prospects_path default to R/29's production files. The
# history-only buckets (veteran_history_only, history_only_fallback) are clean
# under those defaults. The translation/prospect buckets are PROVISIONAL until
# phase two swaps in per-year translation preds and as-of-class R/28 scores.
# ==============================================================================

if (!exists("build_projection_priors")) {
  source(here::here("R", "29_projection_engine.R"))
}

# Buckets whose prior is pure NFL history < ty, hence leak-clean immediately.
R29_CLEAN_HISTORY_SOURCES <- c("veteran_history_only", "history_only_fallback",
                               "phased_out_history_only")

#' Bonus-free counterpart of a scoring config. Zeros the play-level yardage
#' milestone bonuses so the projection arms share one basis with each other.
dk_core_of <- function(scoring) {
  out <- utils::modifyList(scoring, list(
    hundred_yard_bonus = 0, bonus_pass_yd_300 = 0, bonus_pass_yd_400 = 0,
    bonus_rec_yd_200 = 0, bonus_rush_yd_200 = 0
  ))
  # long_td_tiers replaced long_td_bonus in R/17 [2026-07-16] and is still a
  # play-level bonus, so it belongs in the core-stripping set.
  #
  # It CANNOT go in the modifyList() above. modifyList() treats a NULL value as
  # "delete this key", so list(long_td_tiers = NULL) drops the parameter rather
  # than zeroing it. Assigning list(NULL) to a single-bracket index sets it
  # present-and-NULL, which is what R/17 reads as "no long-TD bonus" and what
  # .scoring_key() serializes deterministically.
  out["long_td_tiers"] <- list(NULL)
  out
}

#' r29 arm: preseason posterior (= prior_mu, no YTD) and prior_source per player,
#' per holdout year.
#'
#' @param target_years Integer vector.
#' @param scoring Named list (R/17 / R/29 schema), already bonus-free. Default
#'   dk_core_of(DK_BEST_BALL_SCORING).
#' @param translation_path,prospects_path,cache_dir Optional overrides; NULL uses
#'   R/29 production defaults.
#' @param verbose Logical.
#'
#' @return tibble(player_id, season, position, r29_mu, prior_source, r29_clean).
build_r29_arm <- function(target_years,
                          scoring          = dk_core_of(DK_BEST_BALL_SCORING),
                          translation_path = NULL,
                          prospects_path   = NULL,
                          cache_dir        = NULL,
                          verbose          = TRUE) {

  out <- lapply(target_years, function(ty) {
    if (verbose) message(glue("  r29 arm: build_projection_priors(season = {ty})..."))
    args <- list(season = ty, scoring_settings = scoring)
    if (!is.null(translation_path)) args$translation_path <- translation_path
    if (!is.null(prospects_path))   args$prospects_path   <- prospects_path
    if (!is.null(cache_dir))        args$cache_dir         <- cache_dir

    pri <- do.call(build_projection_priors, args)

    id_col <- intersect(c("nfl_gsis_id", "player_id", "gsis_id"), names(pri))[1]
    if (is.na(id_col)) {
      stop("build_r29_arm(): no id column in build_projection_priors output. ",
           "Columns: ", paste(head(names(pri), 30), collapse = ", "))
    }

    tibble::tibble(
      player_id    = pri[[id_col]],
      season       = ty,
      position     = pri$position,
      r29_mu       = pri$prior_mu,
      prior_source = pri$prior_source
    ) %>%
      dplyr::mutate(r29_clean = .data$prior_source %in% R29_CLEAN_HISTORY_SOURCES)
  })

  res <- dplyr::bind_rows(out)
  if (verbose) {
    message(glue("  build_r29_arm(): {nrow(res)} player-seasons; ",
                 "{sum(res$r29_clean)} in clean history buckets."))
  }
  res
}


# ==============================================================================
# STAGE 3: vi arm  (R/30 team volumes -> R/31 allocation -> R/32 volume-implied)
# ==============================================================================
# Two leakage controls, both confirmed necessary against the live files:
#   * R/30 is driven by the source-time globals SEASON and HISTORICAL_SEASONS
#     (it takes no season arg), so both are reset per holdout year.
#   * R/31's depth loader takes the LATEST week (end-of-season roles) and then
#     overlays the CURRENT Sleeper roster. Both leak into a historical preseason
#     projection. The harness installs a leak-safe replacement that takes the
#     EARLIEST week and skips the Sleeper overlay. R resolves callees at call
#     time in the global env, so this overrides R/31 without editing its file.
#     NOTE: it is a copy of R/31's .load_2026_depth_charts with only those two
#     changes; if that function changes in R/31, refresh this copy. The original
#     is saved and restored on exit so production behavior is untouched after.
#
# Efficiency is derived leak-safe per year (R/32's opportunity-pooled method,
# bounded to seasons before ty) and the volume-implied conversion mirrors R/32:
#   vi = true_targets_pg * ppr_per_target + expected_carries_pg * ppr_per_carry
# with true_targets_pg = expected_targets_pg * SACK_FACTOR (R/32's 0.935).
# ==============================================================================

if (!exists("allocate_player_volumes")) {
  source(here::here("R", "31_player_volume_allocation.R"))  # sources R/30 too
}

VI_SACK_FACTOR <- 0.935   # R/32: expected_targets_pg -> true thrown targets
VI_EFF_WINDOW  <- 5L      # trailing seasons for the position efficiency table
VI_MAX_TARGETS_PG <- 14   # sanity ceiling; real max ~12/gm. Above = broken alloc.
VI_MAX_CARRIES_PG <- 24   # sanity ceiling; real max ~22/gm.

# Leak-safe depth loader: verbatim copy of R/31 .load_2026_depth_charts with
# (1) earliest week instead of latest, (2) no Sleeper overlay.
.harness_leaksafe_depth_loader <- function(season = SEASON_ALLOC) {
  message(glue("  [leak-safe depth] season {season}"))
  dc <- tryCatch(nflreadr::load_depth_charts(seasons = season),
                 error = function(e) NULL)
  if (is.null(dc) || nrow(dc) == 0L) {
    dc <- tryCatch(nflreadr::load_depth_charts(seasons = season - 1L),
                   error = function(e) NULL)
    if (is.null(dc) || nrow(dc) == 0L) {
      stop(".harness_leaksafe_depth_loader(): no depth chart data available")
    }
  }

  has_week_col <- "week" %in% names(dc) && any(!is.na(dc[["week"]]))
  if (has_week_col) {
    earliest_week <- min(dc[["week"]], na.rm = TRUE)   # <-- earliest, not latest
    message(glue("    Filtering to week {earliest_week} (preseason proxy)"))
    dc_latest <- dc %>%
      dplyr::filter(.data$week == earliest_week) %>%
      dplyr::filter(!is.na(.data$gsis_id), nchar(.data$gsis_id) > 0)
  } else {
    dc_latest <- dc %>%
      dplyr::filter(!is.na(.data$gsis_id), nchar(.data$gsis_id) > 0)
  }

  pos_col <- dplyr::case_when(
    "pos_abb"              %in% names(dc_latest) ~ "pos_abb",
    "depth_position"       %in% names(dc_latest) ~ "depth_position",
    "position"             %in% names(dc_latest) ~ "position",
    "depth_chart_position" %in% names(dc_latest) ~ "depth_chart_position",
    "pos_name"             %in% names(dc_latest) ~ "pos_name",
    TRUE                                          ~ NA_character_
  )[1]
  if (is.na(pos_col)) {
    stop(glue(".harness_leaksafe_depth_loader(): no position column. ",
              "Columns: {paste(names(dc_latest), collapse = ', ')}"))
  }
  team_col <- dplyr::case_when(
    "team"      %in% names(dc_latest) ~ "team",
    "club_code" %in% names(dc_latest) ~ "club_code",
    TRUE                               ~ NA_character_
  )[1]
  if (is.na(team_col)) stop(".harness_leaksafe_depth_loader(): no team column")
  rank_col <- dplyr::case_when(
    "pos_rank"            %in% names(dc_latest) ~ "pos_rank",
    "depth_team"          %in% names(dc_latest) ~ "depth_team",
    "depth_position_rank" %in% names(dc_latest) ~ "depth_position_rank",
    TRUE                                        ~ NA_character_
  )[1]
  name_col <- dplyr::case_when(
    "full_name"   %in% names(dc_latest) ~ "full_name",
    "player_name" %in% names(dc_latest) ~ "player_name",
    "first_name"  %in% names(dc_latest) ~ "first_name",
    TRUE ~ NA_character_
  )[1]
  if (is.na(name_col)) {
    dc_latest$player_name_synth <- dc_latest$gsis_id
    name_col <- "player_name_synth"
  }

  dc_latest %>%
    dplyr::transmute(
      nfl_gsis_id    = .data$gsis_id,
      player_name    = .data[[name_col]],
      team           = .normalize_sleeper_team_codes_r31(
                          .normalize_team_codes(.data[[team_col]])),
      position       = .data[[pos_col]],
      depth_rank_raw = if (!is.na(rank_col)) as.integer(.data[[rank_col]]) else 1L,
      # all rows are nflreadr-sourced (no Sleeper overlay), so none were filled
      # from a missing depth; matches R/31's own FALSE default for nflreadr rows.
      depth_was_missing = FALSE
    ) %>%
    dplyr::filter(.data$position %in% ALLOC_POSITIONS,
                  .data$team %in% ACTIVE_TEAMS_2026,
                  !is.na(.data$nfl_gsis_id)) %>%
    dplyr::group_by(.data$nfl_gsis_id, .data$team, .data$position) %>%
    dplyr::slice_min(.data$depth_rank_raw, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup()
  # NOTE: no .merge_sleeper_depth_overrides() -- that overlays the CURRENT roster.
}

# Leak-safe position efficiency (R/32 opportunity-pooled method, bounded < ty).
.harness_position_efficiency <- function(seasons, scoring) {
  d <- nflreadr::load_player_stats(seasons = seasons, stat_type = "offense")
  d <- d[d$season_type == "REG", , drop = FALSE]
  s0 <- function(x) sum(dplyr::coalesce(as.numeric(x), 0))
  d %>%
    dplyr::filter(.data$position %in% c("RB", "WR", "TE")) %>%
    dplyr::group_by(.data$position) %>%
    dplyr::summarise(
      tgt = s0(.data$targets), rec = s0(.data$receptions),
      recyd = s0(.data$receiving_yards), rectd = s0(.data$receiving_tds),
      car = s0(.data$carries), rushyd = s0(.data$rushing_yards),
      rushtd = s0(.data$rushing_tds), .groups = "drop"
    ) %>%
    dplyr::mutate(
      catch_rate = dplyr::if_else(tgt > 0, rec / tgt, 0),
      ypc        = dplyr::if_else(rec > 0, recyd / rec, 0),
      tdt        = dplyr::if_else(tgt > 0, rectd / tgt, 0),
      ypcarry    = dplyr::if_else(position == "RB" & car > 0, rushyd / car, 0),
      tdc        = dplyr::if_else(position == "RB" & car > 0, rushtd / car, 0),
      ppr_per_target = catch_rate * (ypc * scoring$rec_yd + scoring$ppr) +
                       tdt * scoring$rec_td,
      ppr_per_carry  = ypcarry * scoring$rush_yd + tdc * scoring$rush_td
    ) %>%
    dplyr::select(position, ppr_per_target, ppr_per_carry)
}

#' vi arm: R/31 expected volume converted to volume-implied PPG, per holdout year.
#'
#' @param target_years Integer vector.
#' @param scoring Bonus-free config. Default dk_core_of(DK_BEST_BALL_SCORING).
#' @param sack_factor,eff_window See VI_* constants.
#' @param verbose Logical.
#' @return tibble(player_id, season, position, vi_mu).
build_vi_arm <- function(target_years,
                         scoring     = dk_core_of(DK_BEST_BALL_SCORING),
                         sack_factor = VI_SACK_FACTOR,
                         eff_window  = VI_EFF_WINDOW,
                         verbose     = TRUE) {

  # install leak-safe depth loader; save globals to restore on exit
  had_depth <- exists(".load_2026_depth_charts", envir = .GlobalEnv)
  old_depth <- if (had_depth) get(".load_2026_depth_charts", envir = .GlobalEnv) else NULL
  old_season <- if (exists("SEASON", envir = .GlobalEnv)) get("SEASON", envir = .GlobalEnv) else NULL
  old_hist   <- if (exists("HISTORICAL_SEASONS", envir = .GlobalEnv)) get("HISTORICAL_SEASONS", envir = .GlobalEnv) else NULL
  old_alloc  <- if (exists("SEASON_ALLOC", envir = .GlobalEnv)) get("SEASON_ALLOC", envir = .GlobalEnv) else NULL
  old_carry  <- if (exists("CARRYOVER_PRIOR_SEASON", envir = .GlobalEnv)) get("CARRYOVER_PRIOR_SEASON", envir = .GlobalEnv) else NULL
  on.exit({
    if (had_depth) assign(".load_2026_depth_charts", old_depth, envir = .GlobalEnv)
    if (!is.null(old_season)) assign("SEASON", old_season, envir = .GlobalEnv)
    if (!is.null(old_hist))   assign("HISTORICAL_SEASONS", old_hist, envir = .GlobalEnv)
    if (!is.null(old_alloc))  assign("SEASON_ALLOC", old_alloc, envir = .GlobalEnv)
    if (!is.null(old_carry))  assign("CARRYOVER_PRIOR_SEASON", old_carry, envir = .GlobalEnv)
  }, add = TRUE)
  assign(".load_2026_depth_charts", .harness_leaksafe_depth_loader, envir = .GlobalEnv)

  out <- lapply(target_years, function(ty) {
    if (verbose) message(glue("  vi arm: R/30 + R/31 for season {ty}..."))
    assign("SEASON", ty, envir = .GlobalEnv)
    assign("HISTORICAL_SEASONS", (ty - 3L):(ty - 1L), envir = .GlobalEnv)
    # R/31 carryover reads CARRYOVER_PRIOR_SEASON (= SEASON_ALLOC - 1) as a
    # global default; reset both so carryover uses ty-1, not 2025.
    assign("SEASON_ALLOC", ty, envir = .GlobalEnv)
    assign("CARRYOVER_PRIOR_SEASON", ty - 1L, envir = .GlobalEnv)

    project_team_volumes(as_of_week = NULL, save_output = TRUE)
    alloc <- allocate_player_volumes(season = ty, as_of_week = NULL,
                                     save_output = FALSE)

    eff <- .harness_position_efficiency((ty - eff_window):(ty - 1L), scoring)

    df <- alloc %>%
      dplyr::filter(.data$position %in% c("RB", "WR", "TE")) %>%
      dplyr::transmute(
        player_id = .data$nfl_gsis_id, season = ty, position = .data$position,
        et = .data$expected_targets_pg, ec = .data$expected_carries_pg
      ) %>%
      dplyr::left_join(eff, by = "position") %>%
      dplyr::mutate(
        true_t = dplyr::coalesce(et, 0) * sack_factor,
        vi_mu  = true_t * ppr_per_target + dplyr::coalesce(ec, 0) * ppr_per_carry
      )

    bad <- df %>%
      dplyr::filter(dplyr::coalesce(et, 0) > VI_MAX_TARGETS_PG |
                    dplyr::coalesce(ec, 0) > VI_MAX_CARRIES_PG)
    if (nrow(bad) > 0L && verbose) {
      message(glue("    dropped {nrow(bad)} impossible-volume allocation(s) ",
                   "(et > {VI_MAX_TARGETS_PG} or ec > {VI_MAX_CARRIES_PG})"))
    }

    df %>%
      dplyr::filter(dplyr::coalesce(et, 0) <= VI_MAX_TARGETS_PG,
                    dplyr::coalesce(ec, 0) <= VI_MAX_CARRIES_PG) %>%
      dplyr::select(player_id, season, position, vi_mu)
  })

  res <- dplyr::bind_rows(out)
  if (verbose) message(glue("  build_vi_arm(): {nrow(res)} player-seasons."))
  res
}


# ==============================================================================
# STAGE 4: sweep + bootstrap  (optimal w per prior_source vs current-effective)
# ==============================================================================
# Joins the three arms and evaluates the blend weight per R/29 prior_source
# bucket against both the full (DK bonuses on) and core (bonuses off) realized
# target.
#
# [2026-07-27] STAGE 4 IS NOW FORWARD-CHAINED: for each target season ty in a
# bucket, w is chosen on the bucket's seasons < ty only and evaluated on ty;
# per-season test residuals are pooled for the OOS RMSE. The previous behavior
# (argmin and evaluation on the same pooled rows) is retained but explicitly
# labeled *_insample -- it is selection-biased and produced the 0.275/0.325
# values documented as pending re-derivation in R/32. The bootstrap now
# resamples players within the OOS residuals instead of re-running the
# in-sample argmin.
#
# Compares to the weight each bucket ACTUALLY receives in production
# (R/32 BLEND_WEIGHTS_BY_PRIOR_SOURCE, keyed on the real R/29 prior_source
# values; unknown keys coalesce to 0.50). r29_clean marks the two leak-clean
# history buckets; the translation buckets are PROVISIONAL until phase-2
# re-fits the translation model per year.
# ==============================================================================

# Dated copy of live R/32 weights [2026-07-27, post-rekey]; prefer the live
# constant when R/32 is sourced.
BLEND_WEIGHTS_DEFAULT_R45 <- c(
  "veteran_history_only"         = 0.275,
  "phased_out_history_only"      = 0.325,
  "translation_active"           = 0.50,
  "translation_capped_rookie"    = 0.50,
  "phaseout_no_history_fallback" = 0.50,
  "calibrated_fallback"          = 0.50
)
BLEND_DEFAULT_W_R45 <- 0.50

# Current EFFECTIVE weight, replicating R/32's coalesce-to-default keying.
.resolve_current_weight <- function(prior_source) {
  bw <- if (exists("BLEND_WEIGHTS_BY_PRIOR_SOURCE", inherits = TRUE)) {
    get("BLEND_WEIGHTS_BY_PRIOR_SOURCE", inherits = TRUE)
  } else BLEND_WEIGHTS_DEFAULT_R45
  w <- unname(bw[prior_source])
  w[is.na(w)] <- BLEND_DEFAULT_W_R45
  w
}

#' Join the three arms into one blend frame.
#' @return tibble(player_id, season, position, prior_source, r29_clean,
#'   r29_mu, vi_mu, actual_full, actual_core).
assemble_blend_frame <- function(actual, r29, vi) {
  r29 %>%
    dplyr::inner_join(dplyr::select(vi, player_id, season, vi_mu),
                      by = c("player_id", "season")) %>%
    dplyr::inner_join(dplyr::select(actual, player_id, season,
                                    actual_full = ppg_full, actual_core = ppg_core),
                      by = c("player_id", "season")) %>%
    dplyr::filter(is.finite(.data$r29_mu), is.finite(.data$vi_mu),
                  is.finite(.data$actual_full), is.finite(.data$actual_core)) %>%
    dplyr::select(player_id, season, position, prior_source, r29_clean,
                  r29_mu, vi_mu, actual_full, actual_core)
}

.blend_rmse <- function(actual_v, vi_v, r29_v, w) {
  e <- actual_v - (w * vi_v + (1 - w) * r29_v)
  sqrt(mean(e^2))
}

#' Sweep w per prior_source: forward-chained OOS headline numbers, plus the
#' same-rows in-sample sweep kept as a clearly-labeled reference.
#'
#' Forward chaining (stage-4 fix, 2026-07-27): for each target season ty in a
#' bucket, w is chosen on the bucket's seasons < ty and evaluated on ty only;
#' the per-season test residuals are pooled for rmse_oos_*. The old behavior
#' (argmin and evaluation on the same pooled rows) is retained in the
#' opt_w_insample_* / rmse_insample_* columns and must NOT be read as
#' held-out. rmse_cur_* uses the fixed production weight (no selection, so
#' evaluating on all rows is unbiased).
#'
#' @param frame Output of assemble_blend_frame().
#' @param w_grid Numeric grid over [0,1]. Default seq(0, 1, 0.025).
#' @param min_n Minimum bucket size to report. Default 30.
#' @param min_train Minimum pooled training rows per forward fold. Default 30.
#' @param verbose Logical.
#' @return list(summary, folds):
#'   summary  per bucket: n, r29_clean, current_w; per target (full/core):
#'            rmse_cur_*, pooled forward-chained rmse_oos_* with n_oos_*, and
#'            the in-sample reference opt_w_insample_* / rmse_insample_*.
#'   folds    per (bucket, target, season): w_chosen (on seasons < ty),
#'            n_train, n_test, rmse_oos at ty.
run_blend_backtest <- function(frame, w_grid = seq(0, 1, by = 0.025),
                               min_n = 30L, min_train = 30L, verbose = TRUE) {

  buckets <- frame %>% dplyr::count(prior_source) %>%
    dplyr::filter(n >= min_n) %>% dplyr::pull(prior_source)

  fold_rows <- list()

  rows <- lapply(buckets, function(ps) {
    fb   <- frame[frame$prior_source == ps, , drop = FALSE]
    curw <- .resolve_current_weight(ps)

    sweep_target <- function(actual_col) {
      # In-sample reference: argmin and evaluation on the same pooled rows.
      rmse <- vapply(w_grid,
                     function(w) .blend_rmse(fb[[actual_col]], fb$vi_mu, fb$r29_mu, w),
                     numeric(1))

      # Forward-chained OOS: choose w on seasons < ty, evaluate on ty only.
      seasons <- sort(unique(fb$season))
      oos_res <- numeric(0)
      for (ty in seasons) {
        tr <- fb[fb$season <  ty, , drop = FALSE]
        te <- fb[fb$season == ty, , drop = FALSE]
        if (nrow(tr) < min_train || nrow(te) == 0L) next
        rmse_tr <- vapply(w_grid,
                          function(w) .blend_rmse(tr[[actual_col]], tr$vi_mu,
                                                  tr$r29_mu, w),
                          numeric(1))
        w_fold <- w_grid[which.min(rmse_tr)]
        res <- te[[actual_col]] -
          (w_fold * te$vi_mu + (1 - w_fold) * te$r29_mu)
        fold_rows[[length(fold_rows) + 1L]] <<- tibble(
          prior_source = ps,
          target       = sub("^actual_", "", actual_col),
          season       = ty,
          n_train      = nrow(tr),
          n_test       = nrow(te),
          w_chosen     = w_fold,
          rmse_oos     = sqrt(mean(res^2))
        )
        oos_res <- c(oos_res, res)
      }

      list(opt_w_insample = w_grid[which.min(rmse)],
           rmse_insample  = min(rmse),
           rmse_cur = .blend_rmse(fb[[actual_col]], fb$vi_mu, fb$r29_mu, curw),
           rmse_oos = if (length(oos_res) > 0L) sqrt(mean(oos_res^2))
                      else NA_real_,
           n_oos    = length(oos_res))
    }
    sf <- sweep_target("actual_full")
    sc <- sweep_target("actual_core")

    tibble(
      prior_source = ps, n = nrow(fb),
      r29_clean = isTRUE(fb$r29_clean[1]), current_w = curw,
      rmse_cur_full = sf$rmse_cur, rmse_oos_full = sf$rmse_oos,
      n_oos_full = sf$n_oos,
      opt_w_insample_full = sf$opt_w_insample,
      rmse_insample_full  = sf$rmse_insample,
      rmse_cur_core = sc$rmse_cur, rmse_oos_core = sc$rmse_oos,
      n_oos_core = sc$n_oos,
      opt_w_insample_core = sc$opt_w_insample,
      rmse_insample_core  = sc$rmse_insample
    )
  })

  res <- dplyr::bind_rows(rows) %>%
    dplyr::mutate(
      rmse_gain_oos_full = rmse_cur_full - rmse_oos_full,
      rmse_gain_oos_core = rmse_cur_core - rmse_oos_core
    ) %>%
    dplyr::arrange(dplyr::desc(r29_clean), dplyr::desc(n))

  if (verbose && nrow(res) > 0L) {
    message("  Blend backtest (forward-chained OOS vs current-effective, ",
            "full target):")
    for (i in seq_len(nrow(res))) {
      message(glue(
        "    {res$prior_source[i]} (n {res$n[i]}, clean {res$r29_clean[i]}): ",
        "current {format(res$current_w[i], nsmall = 2)} ",
        "rmse {format(round(res$rmse_cur_full[i], 2), nsmall = 2)} vs ",
        "chained OOS rmse ",
        "{format(round(res$rmse_oos_full[i], 2), nsmall = 2)} ",
        "(n_oos {res$n_oos_full[i]}; in-sample argmin ",
        "{format(res$opt_w_insample_full[i], nsmall = 3)}, reference only)"
      ))
    }
  }
  list(summary = res, folds = dplyr::bind_rows(fold_rows))
}

#' Player-clustered bootstrap CI on the forward-chained OOS RMSE for one
#' bucket and target.
#'
#' Stage-4 fix (2026-07-27): the previous version re-ran the IN-SAMPLE argmin
#' on each resample, which only measured the stability of a selection-biased
#' optimum. This version first computes the forward-chained OOS residuals
#' (w chosen on seasons < ty, applied to the held-out ty) and, on the same
#' held-out rows, the residuals at the current production weight; the
#' bootstrap then resamples PLAYERS within those fixed residual sets -- no
#' argmin is re-run inside the bootstrap. gain = rmse_cur - rmse_oos, so a
#' gain CI excluding 0 means the chained weights beat the current one OOS.
#'
#' @param frame Output of assemble_blend_frame().
#' @param prior_source Bucket to bootstrap.
#' @param target c("full","core").
#' @param w_grid,B,seed Sweep grid, resamples, seed.
#' @param min_train Minimum pooled training rows per forward fold. Default 30.
#' @return tibble(prior_source, target, rmse_oos, ci_lo, ci_hi, rmse_cur,
#'   gain_ci_lo, gain_ci_hi, current_w, n_oos).
robustness_blend_w <- function(frame, prior_source, target = c("full", "core"),
                               w_grid = seq(0, 1, by = 0.025),
                               B = 2000L, seed = 1L, min_train = 30L) {
  target <- match.arg(target)
  acol   <- if (target == "full") "actual_full" else "actual_core"
  fb   <- frame[frame$prior_source == prior_source, , drop = FALSE]
  curw <- .resolve_current_weight(prior_source)

  na_row <- tibble(prior_source = prior_source, target = target,
                   rmse_oos = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                   rmse_cur = NA_real_, gain_ci_lo = NA_real_,
                   gain_ci_hi = NA_real_, current_w = curw, n_oos = 0L)
  if (nrow(fb) < 10L) return(na_row)

  # Forward-chained OOS residuals (chained w) and current-w residuals on the
  # same held-out rows.
  seasons <- sort(unique(fb$season))
  res_oos <- numeric(0); res_cur <- numeric(0); pid <- character(0)
  for (ty in seasons) {
    tr <- fb[fb$season <  ty, , drop = FALSE]
    te <- fb[fb$season == ty, , drop = FALSE]
    if (nrow(tr) < min_train || nrow(te) == 0L) next
    rmse_tr <- vapply(w_grid, function(w)
      .blend_rmse(tr[[acol]], tr$vi_mu, tr$r29_mu, w), numeric(1))
    w_fold <- w_grid[which.min(rmse_tr)]
    res_oos <- c(res_oos,
                 te[[acol]] - (w_fold * te$vi_mu + (1 - w_fold) * te$r29_mu))
    res_cur <- c(res_cur,
                 te[[acol]] - (curw * te$vi_mu + (1 - curw) * te$r29_mu))
    pid     <- c(pid, te$player_id)
  }
  if (length(res_oos) == 0L) return(na_row)

  point_oos <- sqrt(mean(res_oos^2))
  point_cur <- sqrt(mean(res_cur^2))

  set.seed(seed)
  idx_by_player <- split(seq_along(res_oos), pid)
  players <- names(idx_by_player)
  boot_oos <- numeric(B); boot_gain <- numeric(B)
  for (b in seq_len(B)) {
    idx <- unlist(idx_by_player[sample(players, length(players), replace = TRUE)],
                  use.names = FALSE)
    boot_oos[b]  <- sqrt(mean(res_oos[idx]^2))
    boot_gain[b] <- sqrt(mean(res_cur[idx]^2)) - sqrt(mean(res_oos[idx]^2))
  }

  tibble(prior_source = prior_source, target = target,
         rmse_oos = point_oos,
         ci_lo = unname(stats::quantile(boot_oos, 0.025)),
         ci_hi = unname(stats::quantile(boot_oos, 0.975)),
         rmse_cur = point_cur,
         gain_ci_lo = unname(stats::quantile(boot_gain, 0.025)),
         gain_ci_hi = unname(stats::quantile(boot_gain, 0.975)),
         current_w = curw, n_oos = length(res_oos))
}
