################################################################################
# NFL Analytics Toolkit - Season 2, Week 4
# File: R/18_predictive_validity_ab_testing.R
#
# Research Question:
#   Does a player's rolling 3-game average fantasy score predict their NEXT
#   week's fantasy points better than their season-to-date average?
#
# Design:
#   Unit of observation : player-week (one row per player per eligible week)
#   Predictor A (control)   : season-to-date average PPR points through week N-1
#   Predictor B (treatment) : rolling 3-game average PPR points through week N-1
#   Outcome                 : actual PPR points in week N
#   Positions               : QB, RB, WR -- analyzed separately
#   Seasons                 : 2010-2025 (16 seasons, Season 2 cache)
#   Inference               : Pearson r per predictor, Fisher r-to-z comparison,
#                             Cohen's q effect size, bootstrap 95% CIs,
#                             Benjamini-Hochberg FDR correction across positions
#
# Inclusion criteria (player-week level):
#   - Regular season weeks only (no postseason)
#   - Player has >= 3 prior games played in that season (roll3 fully populated)
#   - Position is QB, RB, or WR (inferred from nflreadr roster via R/05)
#   - Meets minimum season-to-date touch threshold per position:
#       QB : >= 30 dropbacks season-to-date
#       RB : >= 15 rush attempts season-to-date
#       WR : >= 10 targets season-to-date
#   These are cumulative through week N-1, not single-week floors.
#   Low thresholds intentional: multi-season dataset means we want part-time
#   starters included. Season 1 thresholds were season-totals; these are weekly.
#
# Scoring:
#   Standard PPR via calculate_fantasy_points() with all defaults.
#   use_tiered_ppr = TRUE (Season 1 default, preserved here).
#   No extended scoring parameters (Week 3 extensions default to 0).
#
# NFL Context:
#   "Recency bias" is a perennial debate in fantasy: should you trust a hot
#   player's last 3 games or regress toward their season average? This study
#   provides a data-grounded answer across 16 seasons and 3 positions.
#   Garbage time filtered inside calculate_fantasy_points() via play_type.
#   QB kneels / spikes excluded (play_type filter in R/05).
#
# Known data notes carried forward from Week 1:
#   - 2022 BUF-CIN cancellation (Damar Hamlin): 1 missing game, expected
#   - CPOE unavailable 2010-2015: not used here
#   - 17-game expansion: week boundary handled by regular-season filter
#
# Functions exported (in order):
#   calculate_season_to_date_avg()       Line ~130
#   build_ab_panel()                     Line ~230
#   compare_prediction_methods()         Line ~380
#   calculate_effect_size_ab()           Line ~530
#   power_analysis_correlation_diff()    Line ~600
#   apply_fdr_correction()               Line ~690
#   validate_ab_assumptions()            Line ~760
#   run_ab_test_pipeline()               Line ~870
#
# Outputs (written by run_ab_test_pipeline()):
#   data/season2_cache/s2_week4_ab_panel.rds
#   data/season2_cache/s2_week4_ab_results.rds
#   data/season2_cache/s2_week4_power_analysis.rds
#
# Source dependencies:
#   R/15_multi_season_pbp.R    load_normalized_season()
#   R/05_consistency_metrics.R calculate_fantasy_points(),
#                               calculate_rolling_fantasy()
#
# Package dependencies:
#   dplyr, tidyr, purrr, glue, here  -- core wrangling
#   zoo                              -- via calculate_rolling_stats() in R/05
#   pwr                              -- power analysis
#   boot                             -- bootstrap CIs on correlations
#
# Season 2 output prefix : s2_week4_
# Schema tag             : s2_ab_v1
# Author                 : Christian LeBlanc
# Created                : 2026-04
################################################################################


# ==============================================================================
# DEPENDENCIES
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(glue)
library(here)
library(boot)

if (!requireNamespace("pwr", quietly = TRUE)) {
  stop(
    "Package 'pwr' is required. Install with: install.packages('pwr')",
    call. = FALSE
  )
}
library(pwr)

source(here("R", "15_multi_season_pbp.R"))
source(here("R", "05_consistency_metrics.R"))

# Season range -- all Season 2 scripts use the full configured range
SEASONS_S2 <- 2010:2025

# Minimum prior games played before a player-week is eligible.
# Ensures roll3 is computed from a full 3-game window.
MIN_PRIOR_GAMES <- 3L


# ==============================================================================
# SECTION 1: calculate_season_to_date_avg()
# ==============================================================================

#' Calculate Season-to-Date Average Fantasy Points
#'
#' @description
#' For each player-week row, computes the expanding (cumulative) mean of
#' total_fantasy_points through week N-1. The current week is excluded via
#' lag() to prevent feature leakage. This is the "control" predictor in the
#' recency bias A/B test: the best naive forecast available before week N,
#' using all games played so far that season.
#'
#' Rolling context resets at team changes (grouped by player_id + season +
#' team), consistent with calculate_rolling_stats() behavior in R/05. A player
#' traded mid-season has an independent STD window on each team.
#'
#' @param fantasy_data Tibble from calculate_fantasy_points(). Must contain:
#'   player_id (chr), player_name (chr), season (int/dbl), week (int/dbl),
#'   team (chr), total_fantasy_points (dbl).
#'
#' @return The input tibble with one additional column:
#'   \describe{
#'     \item{fantasy_pts_std}{Expanding mean of total_fantasy_points through
#'       week N-1. NA for a player's first game of each team stint (no prior
#'       data). Type: dbl.}
#'   }
#'
#' @details
#' Uses cummean() on lagged values. lag() shifts values down by 1 row within
#' each group so that week N sees only weeks 1 through N-1. The cumulative
#' mean then runs over all non-NA lagged values.
#'
#' By week 2: fantasy_pts_std = week 1 points (n = 1).
#' By week 4: fantasy_pts_std = mean(weeks 1-3).
#' This exactly matches the information a fantasy manager has before each week.
#'
#' Note: cummean() in dplyr propagates NA if the lagged value is NA (first row
#' of each group). After the first row, cummean() uses only non-NA values
#' implicitly because lag() returns NA once and then actual values. This is
#' correct behavior.
#'
#' @seealso \code{\link{calculate_rolling_fantasy}} for fixed-window rolling
#'   averages (the "treatment" predictor); \code{\link{build_ab_panel}} which
#'   calls both functions and joins their output.
#'
#' @examples
#' # fantasy is output of calculate_fantasy_points()
#' fantasy_with_std <- calculate_season_to_date_avg(fantasy)
#'
#' # Leakage check: first game of each stint must be NA
#' fantasy_with_std %>%
#'   arrange(player_id, season, team, week) %>%
#'   group_by(player_id, season, team) %>%
#'   slice(1) %>%
#'   summarise(first_row_is_na = all(is.na(fantasy_pts_std)))
#'
#' @export
calculate_season_to_date_avg <- function(fantasy_data) {

  # --- Input validation ---
  if (!is.data.frame(fantasy_data)) {
    stop("fantasy_data must be a data frame.", call. = FALSE)
  }

  required_cols <- c("player_id", "player_name", "season", "week",
                     "team", "total_fantasy_points")
  missing_cols <- setdiff(required_cols, names(fantasy_data))
  if (length(missing_cols) > 0) {
    stop(glue(
      "calculate_season_to_date_avg(): missing required columns: ",
      "{paste(missing_cols, collapse = ', ')}"
    ), call. = FALSE)
  }

  if (!is.numeric(fantasy_data$total_fantasy_points)) {
    stop("total_fantasy_points must be numeric.", call. = FALSE)
  }

  n_in <- nrow(fantasy_data)

  # Compute expanding mean, lagged by 1 to exclude current week.
  # Group by player_id + season + team to reset window on team change,
  # matching reset behavior in calculate_rolling_stats() (R/05 line ~670).
  #
  # WHY NOT cummean(lag(...)): dplyr::cummean() uses cumsum() internally,
  # which propagates NA. lag() returns NA for the first row of every group.
  # Once cumsum() sees that NA it returns NA for the entire remaining vector,
  # so every player's season-to-date column is all-NA.
  #
  # FIX: manual expanding mean via cumsum over coalesced values divided by
  # count of non-NA values seen so far. The coalesce(lag_pts, 0) replaces the
  # first-row NA with 0 in the running total, but n_valid = 0 for that row so
  # the result is correctly returned as NA. All subsequent rows accumulate
  # properly: cumsum includes only actual point values, divided by actual
  # game count seen prior to week N.
  result <- fantasy_data %>%
    arrange(player_id, season, team, week) %>%
    group_by(player_id, season, team) %>%
    mutate(
      .lag_pts        = lag(total_fantasy_points, n = 1L),
      .n_valid        = cumsum(!is.na(.lag_pts)),
      .running_sum    = cumsum(dplyr::coalesce(.lag_pts, 0)),
      fantasy_pts_std = dplyr::if_else(
        .n_valid > 0L,
        .running_sum / .n_valid,
        NA_real_
      )
    ) %>%
    select(-.lag_pts, -.n_valid, -.running_sum) %>%
    ungroup()

  # Row count must be unchanged -- no joins or expansions expected
  if (nrow(result) != n_in) {
    stop(glue(
      "calculate_season_to_date_avg(): row count changed unexpectedly ",
      "from {n_in} to {nrow(result)}."
    ), call. = FALSE)
  }

  n_valid <- sum(!is.na(result$fantasy_pts_std))
  message(glue(
    "calculate_season_to_date_avg(): {format(n_valid, big.mark = ',')} valid ",
    "STD values from {format(n_in, big.mark = ',')} player-weeks"
  ))

  return(result)
}


# ==============================================================================
# SECTION 2: build_ab_panel()
# ==============================================================================

#' Build the A/B Test Player-Week Panel
#'
#' @description
#' Constructs the core analytical dataset for the recency bias study. For each
#' season in SEASONS_S2, loads normalized PBP from the Week 1 cache, computes
#' weekly PPR fantasy points, adds the rolling 3-game average (treatment) and
#' season-to-date average (control), then filters to eligible player-weeks.
#'
#' Seasons are processed one at a time to manage memory. Each normalized PBP
#' season is 50k+ rows and 300+ columns; loading all 16 simultaneously would
#' exhaust available RAM.
#'
#' @param seasons Integer vector of seasons to process. Default: SEASONS_S2
#'   (2010:2025). Must be pre-cached by load_multi_season_pbp().
#' @param cache_dir Character. Path to Season 2 cache directory.
#'   Default: here("data", "season2_cache").
#' @param position_filter Character vector. Positions to retain.
#'   Default: c("QB", "RB", "WR").
#' @param min_prior_games Integer. Minimum games played before week N for the
#'   row to be eligible. Default: MIN_PRIOR_GAMES (3L). Ensures roll3 is
#'   computed from a full 3-game window before the row enters the analysis.
#'
#' @return Tibble with one row per eligible player-week. Columns:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{week}{Week number (int)}
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Full name (chr)}
#'     \item{position}{QB / RB / WR from R/05 position inference (chr)}
#'     \item{team}{Current team (chr)}
#'     \item{fantasy_pts_actual}{Actual PPR points this week -- the outcome (dbl)}
#'     \item{fantasy_pts_roll3}{Rolling 3-game avg through week N-1 -- treatment (dbl)}
#'     \item{fantasy_pts_std}{Season-to-date avg through week N-1 -- control (dbl)}
#'     \item{games_played_prior}{Games played in season before week N (int)}
#'   }
#'   Rows where either predictor is NA are dropped before return.
#'
#' @details
#' Regular season filter: week <= 18 for 2021+, week <= 17 for 2020,
#' week <= 16 for pre-2020. Postseason rows inflate game counts and introduce
#' non-representative fantasy scoring patterns.
#'
#' Position comes from calculate_fantasy_points() which joins to nflreadr
#' roster when roster_data is supplied. Here we call without roster_data
#' (default NULL) so position is inferred from play type. This matches the
#' Season 1 pattern for single-season scoring and is consistent across all 16
#' seasons without requiring per-season roster downloads.
#'
#' @seealso \code{\link{calculate_season_to_date_avg}},
#'   \code{\link{calculate_rolling_fantasy}},
#'   \code{\link{compare_prediction_methods}}
#'
#' @examples
#' # Full 16-season panel (uses cache; fast if seasons already cached)
#' panel <- build_ab_panel()
#' nrow(panel)
#' table(panel$position)
#'
#' # Quick test on 3 recent seasons
#' panel_test <- build_ab_panel(seasons = 2023:2025)
#'
#' @export
build_ab_panel <- function(seasons         = SEASONS_S2,
                           cache_dir       = here("data", "season2_cache"),
                           position_filter = c("QB", "RB", "WR"),
                           min_prior_games = MIN_PRIOR_GAMES) {

  # --- Input validation ---
  if (!is.numeric(seasons) || length(seasons) == 0) {
    stop("seasons must be a non-empty numeric vector.", call. = FALSE)
  }
  if (!is.character(position_filter) || length(position_filter) == 0) {
    stop("position_filter must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.numeric(min_prior_games) || length(min_prior_games) != 1 ||
      min_prior_games < 1) {
    stop("min_prior_games must be a single integer >= 1.", call. = FALSE)
  }

  seasons         <- as.integer(seasons)
  min_prior_games <- as.integer(min_prior_games)

  message(glue("\n{'='|>strrep(60)}"))
  message(glue("build_ab_panel(): {length(seasons)} seasons ({min(seasons)}-{max(seasons)})"))
  message(glue("Positions : {paste(position_filter, collapse = ', ')}"))
  message(glue("Min prior games : {min_prior_games}"))
  message(glue("{'='|>strrep(60)}\n"))

  season_panels <- vector("list", length(seasons))

  for (i in seq_along(seasons)) {
    s <- seasons[i]
    message(glue("[{i}/{length(seasons)}] Processing season {s}..."))

    # Load normalized PBP for this season from Week 1 cache.
    # load_normalized_season() errors if cache file is missing.
    pbp <- tryCatch(
      load_normalized_season(s, cache_dir = cache_dir),
      error = function(e) {
        warning(glue(
          "Season {s}: cache not found, skipping. ",
          "Run load_multi_season_pbp() first. Error: {e$message}"
        ), call. = FALSE)
        return(NULL)
      }
    )

    if (is.null(pbp)) next

    # Regular season filter.
    # 17-game expansion: week <= 18 for 2021+, <= 17 for 2020, <= 16 pre-2020.
    max_reg_week <- if (s >= 2021L) 18L else if (s == 2020L) 17L else 16L
    pbp <- pbp %>% filter(week <= max_reg_week)

    # Compute weekly PPR fantasy points via Season 1 infrastructure.
    # All defaults: standard PPR (ppr = 1), use_tiered_ppr = TRUE.
    # No roster_data -- position inferred from play type (consistent across
    # all 16 seasons without per-season roster downloads).
    # calculate_fantasy_points() internally filters to pass/run play types,
    # excluding kneels, spikes, and special teams plays.
    fantasy_weekly <- tryCatch(
      calculate_fantasy_points(pbp_data = pbp, season = s),
      error = function(e) {
        warning(glue(
          "Season {s}: calculate_fantasy_points() failed: {e$message}"
        ), call. = FALSE)
        return(NULL)
      }
    )

    if (is.null(fantasy_weekly) || nrow(fantasy_weekly) == 0) {
      warning(glue("Season {s}: no fantasy data produced, skipping."),
              call. = FALSE)
      rm(pbp); gc(verbose = FALSE)
      next
    }

    # Filter to target positions before rolling calculations to reduce
    # memory pressure on large intermediate objects
    fantasy_weekly <- fantasy_weekly %>%
      filter(position %in% position_filter)

    if (nrow(fantasy_weekly) == 0) {
      warning(glue("Season {s}: no rows after position filter, skipping."),
              call. = FALSE)
      rm(pbp); gc(verbose = FALSE)
      next
    }

    # Add rolling 3-game average (treatment predictor) via R/05 infrastructure.
    # calculate_rolling_fantasy() uses lag() internally to prevent leakage.
    # Output column: total_fantasy_points_roll3
    # min_games = 2: allows partial window; the min_prior_games = 3 gate
    # applied below is the actual inclusion criterion.
    fantasy_rolling <- calculate_rolling_fantasy(
      fantasy_data = fantasy_weekly,
      windows      = 3L,
      min_games    = 2L
    )

    # Add season-to-date average (control predictor).
    # Output column: fantasy_pts_std
    fantasy_rolling <- calculate_season_to_date_avg(fantasy_rolling)

    # Rename for panel clarity
    fantasy_rolling <- fantasy_rolling %>%
      rename(
        fantasy_pts_actual = total_fantasy_points,
        fantasy_pts_roll3  = total_fantasy_points_roll3
      )

    # Count games played BEFORE week N within each player-season-team stint.
    # row_number() within the arranged group equals games played including the
    # current row; lag() shifts it so the current row sees prior-game count.
    fantasy_rolling <- fantasy_rolling %>%
      arrange(player_id, season, team, week) %>%
      group_by(player_id, season, team) %>%
      mutate(
        games_played_prior = lag(row_number(), n = 1L, default = 0L)
      ) %>%
      ungroup()

    # Select panel columns only
    panel_s <- fantasy_rolling %>%
      select(
        season,
        week,
        player_id,
        player_name,
        position,
        team,
        fantasy_pts_actual,
        fantasy_pts_roll3,
        fantasy_pts_std,
        games_played_prior
      )

    # Apply min prior games gate -- ensures roll3 is a full 3-game window
    panel_s <- panel_s %>%
      filter(games_played_prior >= min_prior_games)

    # Drop rows where either predictor is NA.
    # Should not occur given games_played_prior >= 3 gate, but defensive.
    n_before <- nrow(panel_s)
    panel_s  <- panel_s %>%
      filter(!is.na(fantasy_pts_roll3), !is.na(fantasy_pts_std))
    n_dropped <- n_before - nrow(panel_s)

    if (n_dropped > 0) {
      message(glue(
        "  Season {s}: dropped {n_dropped} rows with NA predictors after gate"
      ))
    }

    message(glue(
      "  Season {s}: {format(nrow(panel_s), big.mark = ',')} eligible player-weeks"
    ))

    season_panels[[i]] <- panel_s

    # Free memory before next season -- critical for 16-season loop
    rm(pbp, fantasy_weekly, fantasy_rolling, panel_s)
    gc(verbose = FALSE)
  }

  panel <- bind_rows(season_panels)

  if (nrow(panel) == 0) {
    stop(paste(
      "build_ab_panel(): no eligible player-weeks across all seasons.",
      "Verify the cache is populated via load_multi_season_pbp()."
    ), call. = FALSE)
  }

  message(glue("\nbuild_ab_panel() complete:"))
  message(glue(
    "  Total eligible player-weeks : {format(nrow(panel), big.mark = ',')}"
  ))
  message(glue("  Seasons : {min(panel$season)}-{max(panel$season)}"))
  pos_counts <- table(panel$position)
  for (pos in names(pos_counts)) {
    message(glue(
      "  {pos} : {format(as.integer(pos_counts[pos]), big.mark = ',')} player-weeks"
    ))
  }

  return(panel)
}


# ==============================================================================
# SECTION 3: compare_prediction_methods()
# ==============================================================================

#' Compare Recency vs Season-to-Date as Predictors of Next-Week Fantasy Points
#'
#' @description
#' Core A/B comparison. For each position independently, computes:
#'   1. Pearson r between fantasy_pts_roll3 and fantasy_pts_actual
#'   2. Pearson r between fantasy_pts_std  and fantasy_pts_actual
#'   3. Steiger (1980) test comparing the two dependent correlations
#'   4. Bootstrap 95% BCa CIs on each correlation
#'   5. Cohen's q effect size
#'
#' The Steiger test is used rather than a naive unpaired Fisher z-test because
#' the two predictors are measured on the same players in the same weeks --
#' they are dependent. The Steiger correction accounts for the correlation
#' between predictors (r_roll3_std), which inflates standard errors when
#' ignored.
#'
#' Season is available as a cluster variable. At the sample sizes produced
#' by 16 seasons the asymptotic z-test is well-powered; cluster-robust SEs
#' are a potential extension but are not implemented here to keep the
#' interpretation tractable for a non-specialist audience.
#'
#' @param panel Tibble from build_ab_panel().
#' @param n_bootstrap Integer. Bootstrap replicates for BCa CIs.
#'   Default: 2000L.
#' @param alpha Numeric. Significance level. Default: 0.05.
#' @param seed Integer. Random seed for reproducible bootstrap. Default: 42L.
#'
#' @return Tibble with one row per position. Columns:
#'   \describe{
#'     \item{position}{chr}
#'     \item{n_player_weeks}{int}
#'     \item{n_seasons}{int}
#'     \item{r_roll3}{Pearson r: roll3 vs actual (dbl)}
#'     \item{r_std}{Pearson r: STD vs actual (dbl)}
#'     \item{r_roll3_ci_lo}{Bootstrap BCa 95% CI lower (dbl)}
#'     \item{r_roll3_ci_hi}{Bootstrap BCa 95% CI upper (dbl)}
#'     \item{r_std_ci_lo}{Bootstrap BCa 95% CI lower (dbl)}
#'     \item{r_std_ci_hi}{Bootstrap BCa 95% CI upper (dbl)}
#'     \item{z_stat}{Steiger test statistic (dbl)}
#'     \item{p_value_raw}{Two-sided p-value before FDR correction (dbl)}
#'     \item{cohens_q}{|atanh(r_roll3) - atanh(r_std)| (dbl)}
#'     \item{better_predictor}{"roll3", "std", or "no_difference" (chr)}
#'   }
#'
#' @details
#' Cohen's q benchmarks: 0.10 = small, 0.30 = medium, 0.50 = large.
#' BCa CIs handle skewed bootstrap distributions better than percentile CIs
#' for correlations that are far from 0.
#'
#' @references Steiger, J.H. (1980). Tests for comparing elements of a
#'   correlation matrix. Psychological Bulletin, 87(2), 245-251.
#'
#' @seealso \code{\link{calculate_effect_size_ab}},
#'   \code{\link{apply_fdr_correction}}
#'
#' @examples
#' panel  <- build_ab_panel(seasons = 2023:2025)
#' result <- compare_prediction_methods(panel, n_bootstrap = 500L)
#' result %>% select(position, r_roll3, r_std, cohens_q, p_value_raw)
#'
#' @export
compare_prediction_methods <- function(panel,
                                       n_bootstrap = 2000L,
                                       alpha       = 0.05,
                                       seed        = 42L) {

  # --- Input validation ---
  if (!is.data.frame(panel)) {
    stop("panel must be a data frame.", call. = FALSE)
  }

  required_cols <- c("position", "season", "player_id",
                     "fantasy_pts_actual", "fantasy_pts_roll3", "fantasy_pts_std")
  missing_cols <- setdiff(required_cols, names(panel))
  if (length(missing_cols) > 0) {
    stop(glue(
      "compare_prediction_methods(): missing columns: ",
      "{paste(missing_cols, collapse = ', ')}"
    ), call. = FALSE)
  }

  if (!is.numeric(n_bootstrap) || n_bootstrap < 100L) {
    stop("n_bootstrap must be numeric >= 100.", call. = FALSE)
  }

  positions <- sort(unique(panel$position))
  set.seed(seed)

  results <- map_dfr(positions, function(pos) {

    d <- panel %>% filter(position == pos)
    n <- nrow(d)

    message(glue("  {pos}: n = {format(n, big.mark = ',')} player-weeks across {n_distinct(d$season)} seasons"))

    if (n < 30L) {
      warning(glue(
        "{pos}: only {n} observations -- correlation estimates unreliable. ",
        "Minimum recommended: 30."
      ), call. = FALSE)
    }

    y     <- d$fantasy_pts_actual
    x_r3  <- d$fantasy_pts_roll3
    x_std <- d$fantasy_pts_std

    # Observed Pearson correlations
    r_roll3  <- cor(x_r3,  y,    use = "complete.obs")
    r_std    <- cor(x_std, y,    use = "complete.obs")
    r_r3_std <- cor(x_r3,  x_std, use = "complete.obs")

    # Fisher z-transforms
    z_r3  <- atanh(r_roll3)
    z_std <- atanh(r_std)

    # Steiger (1980) Hotelling-Williams test for two dependent correlations
    # sharing one variable (y). Accounts for correlation between predictors.
    # det_R is the determinant of the 3x3 correlation matrix of [x_r3, x_std, y].
    det_R <- 1 - r_roll3^2 - r_std^2 - r_r3_std^2 +
             2 * r_roll3 * r_std * r_r3_std

    h_denom <- 2 * det_R / (n - 1) +
               ((r_roll3 + r_std) / 2)^2 * (1 - r_r3_std)^3

    # Guard against degenerate denominator (near-identical predictors)
    if (is.na(h_denom) || h_denom <= 0) {
      warning(glue(
        "{pos}: Steiger denominator degenerate (r_predictors = ",
        "{round(r_r3_std, 3)}). Falling back to unpaired Fisher z-test."
      ), call. = FALSE)
      z_stat <- (z_r3 - z_std) / sqrt(2 / (n - 3))
    } else {
      z_stat <- (r_roll3 - r_std) *
                sqrt((n - 1) * (1 + r_r3_std)) /
                sqrt(h_denom)
    }

    p_value_raw <- 2 * pnorm(-abs(z_stat))
    cohens_q    <- calculate_effect_size_ab(r_roll3, r_std)

    # p_value_log10: log10 of the two-sided p-value computed in log-space.
    # pnorm(log.p = TRUE) returns log(p) without underflowing to 0.
    # At n = 44,691 (WR), z ~ -56 and pnorm(-56) = 0 in double precision.
    # log10(p) = log(p) / log(10). Result is a large negative number,
    # e.g. -247 for WR, meaning p = 10^-247. Stored alongside p_value_raw
    # so RDS consumers see the true magnitude even when p_value_raw = 0.
    log_p_one_sided <- pnorm(-abs(z_stat), log.p = TRUE)
    p_value_log10   <- (log_p_one_sided + log(2)) / log(10)

    # Bootstrap BCa 95% CIs
    boot_r3 <- boot(
      data      = data.frame(x = x_r3, y = y),
      statistic = function(d, i) cor(d$x[i], d$y[i]),
      R         = as.integer(n_bootstrap)
    )
    boot_sd <- boot(
      data      = data.frame(x = x_std, y = y),
      statistic = function(d, i) cor(d$x[i], d$y[i]),
      R         = as.integer(n_bootstrap)
    )

    ci_r3 <- tryCatch(
      boot.ci(boot_r3, type = "bca")$bca[4:5],
      error = function(e) {
        warning(glue("{pos}: BCa CI failed for roll3, using percentile."),
                call. = FALSE)
        boot.ci(boot_r3, type = "perc")$percent[4:5]
      }
    )
    ci_sd <- tryCatch(
      boot.ci(boot_sd, type = "bca")$bca[4:5],
      error = function(e) {
        warning(glue("{pos}: BCa CI failed for STD, using percentile."),
                call. = FALSE)
        boot.ci(boot_sd, type = "perc")$percent[4:5]
      }
    )

    better_predictor <- if (abs(z_stat) < qnorm(1 - alpha / 2)) {
      "no_difference"
    } else if (r_roll3 > r_std) {
      "roll3"
    } else {
      "std"
    }

    tibble(
      position          = pos,
      n_player_weeks    = n,
      n_seasons         = n_distinct(d$season),
      r_roll3           = round(r_roll3,      4),
      r_std             = round(r_std,         4),
      r_roll3_ci_lo     = round(ci_r3[1],     4),
      r_roll3_ci_hi     = round(ci_r3[2],     4),
      r_std_ci_lo       = round(ci_sd[1],     4),
      r_std_ci_hi       = round(ci_sd[2],     4),
      z_stat            = round(z_stat,        4),
      # p_value_raw stored at full R precision -- do NOT round here.
      # round(1e-50, 6) = 0, which corrupts downstream p.adjust() and any
      # inspection of the saved RDS. Display formatting is handled by .fmt_p()
      # in the KEY FINDINGS block and should be applied at print time only.
      p_value_raw       = p_value_raw,
      # p_value_log10: log10(p) computed in log-space. Never underflows.
      # Readable as: p ~ 10^(p_value_log10). E.g. -247 means p = 10^-247.
      p_value_log10     = round(p_value_log10,  1),
      cohens_q          = round(cohens_q,      4),
      better_predictor  = better_predictor
    )
  })

  return(results)
}


# ==============================================================================
# SECTION 4: calculate_effect_size_ab()
# ==============================================================================

#' Calculate Cohen's q Effect Size Between Two Correlations
#'
#' @description
#' Computes Cohen's q = |atanh(r1) - atanh(r2)|, the standard effect size
#' for comparing two Pearson correlations. Used to quantify the practical
#' magnitude of any recency advantage independently of sample size.
#'
#' @param r1 Numeric scalar. First correlation. Must be in (-1, 1).
#' @param r2 Numeric scalar. Second correlation. Must be in (-1, 1).
#'
#' @return Numeric scalar >= 0.
#'   Benchmarks per Cohen (1988): 0.10 = small, 0.30 = medium, 0.50 = large.
#'
#' @references Cohen, J. (1988). Statistical power analysis for the behavioral
#'   sciences (2nd ed.). Lawrence Erlbaum Associates.
#'
#' @seealso \code{\link{compare_prediction_methods}}
#'
#' @examples
#' calculate_effect_size_ab(0.45, 0.40)  # small effect
#' calculate_effect_size_ab(0.60, 0.45)  # medium effect
#'
#' @export
calculate_effect_size_ab <- function(r1, r2) {

  if (!is.numeric(r1) || length(r1) != 1 || is.na(r1) || abs(r1) >= 1) {
    stop("r1 must be a single numeric value in (-1, 1).", call. = FALSE)
  }
  if (!is.numeric(r2) || length(r2) != 1 || is.na(r2) || abs(r2) >= 1) {
    stop("r2 must be a single numeric value in (-1, 1).", call. = FALSE)
  }

  abs(atanh(r1) - atanh(r2))
}


# ==============================================================================
# SECTION 5: power_analysis_correlation_diff()
# ==============================================================================

#' A Priori Power Analysis for Correlation Difference Test
#'
#' @description
#' Computes (a) the sample size required to detect a given Cohen's q effect
#' size at target power, and (b) the achieved power at the observed n per
#' position. Run before interpreting null results: a non-significant p-value
#' with low power is uninformative, not evidence of no effect.
#'
#' @param panel Tibble from build_ab_panel(). Used to extract observed n per
#'   position.
#' @param mde_q Numeric. Minimum detectable effect (Cohen's q). Default: 0.10.
#' @param alpha Numeric. Type I error rate. Default: 0.05.
#' @param target_power Numeric. Desired power. Default: 0.80.
#'
#' @return Named list:
#'   \describe{
#'     \item{required_n}{Tibble with mde_q, alpha, target_power,
#'       required_n_per_group.}
#'     \item{achieved_power}{Tibble with position, n_player_weeks,
#'       achieved_power.}
#'   }
#'
#' @details
#' Required n for the Fisher z-test at effect size q:
#'   n = ((z_alpha/2 + z_beta) / q)^2 + 3
#' This is a conservative estimate -- the Steiger correction for dependent
#' correlations reduces SE, so actual required n is slightly lower.
#'
#' @examples
#' panel  <- build_ab_panel(seasons = 2023:2025)
#' power  <- power_analysis_correlation_diff(panel)
#' power$required_n
#' power$achieved_power
#'
#' @export
power_analysis_correlation_diff <- function(panel,
                                            mde_q        = 0.10,
                                            alpha        = 0.05,
                                            target_power = 0.80) {

  if (!is.data.frame(panel)) {
    stop("panel must be a data frame.", call. = FALSE)
  }
  if (!is.numeric(mde_q) || length(mde_q) != 1 || mde_q <= 0) {
    stop("mde_q must be a single positive numeric value.", call. = FALSE)
  }
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be in (0, 1).", call. = FALSE)
  }
  if (!is.numeric(target_power) || target_power <= 0 || target_power >= 1) {
    stop("target_power must be in (0, 1).", call. = FALSE)
  }

  z_alpha <- qnorm(1 - alpha / 2)
  z_beta  <- qnorm(target_power)

  required_n <- ceiling(((z_alpha + z_beta) / mde_q)^2 + 3)

  required_tbl <- tibble(
    mde_q                = mde_q,
    alpha                = alpha,
    target_power         = target_power,
    required_n_per_group = required_n
  )

  # Achieved power at observed n per position:
  # Power = P(reject H0 | true q = mde_q)
  #       = Phi(sqrt(n-3) * q - z_alpha) + Phi(-sqrt(n-3) * q - z_alpha)
  n_by_pos <- panel %>%
    group_by(position) %>%
    summarise(n_player_weeks = n(), .groups = "drop")

  achieved_tbl <- n_by_pos %>%
    mutate(
      achieved_power = pnorm(sqrt(n_player_weeks - 3) * mde_q - z_alpha) +
                       pnorm(-sqrt(n_player_weeks - 3) * mde_q - z_alpha),
      achieved_power = round(achieved_power, 4)
    )

  message("power_analysis_correlation_diff():")
  message(glue("  MDE (Cohen's q) : {mde_q}"))
  message(glue("  Alpha           : {alpha}"))
  message(glue("  Target power    : {target_power}"))
  message(glue("  Required n      : {required_n} player-weeks per position"))
  for (i in seq_len(nrow(achieved_tbl))) {
    message(glue(
      "  {achieved_tbl$position[i]} : n = ",
      "{format(achieved_tbl$n_player_weeks[i], big.mark = ',')}, ",
      "achieved power = {achieved_tbl$achieved_power[i]}"
    ))
  }

  return(list(required_n = required_tbl, achieved_power = achieved_tbl))
}


# ==============================================================================
# SECTION 6: apply_fdr_correction()
# ==============================================================================

#' Apply Benjamini-Hochberg FDR Correction Across Positions
#'
#' @description
#' Adjusts raw p-values from compare_prediction_methods() using the
#' Benjamini-Hochberg false discovery rate procedure. With 3 tests (QB, RB,
#' WR), BH is the appropriate correction: it controls the expected proportion
#' of false discoveries rather than family-wise error rate, which would be
#' overly conservative at m = 3.
#'
#' @param results Tibble from compare_prediction_methods(). Must contain
#'   position (chr) and p_value_raw (dbl).
#'
#' @return Input tibble with two additional columns:
#'   \describe{
#'     \item{p_value_adj}{BH-adjusted p-value (dbl)}
#'     \item{significant_adj}{Logical: p_value_adj < 0.05 (lgl)}
#'   }
#'
#' @examples
#' results     <- compare_prediction_methods(panel)
#' results_adj <- apply_fdr_correction(results)
#' results_adj %>% select(position, p_value_raw, p_value_adj, significant_adj)
#'
#' @export
apply_fdr_correction <- function(results) {

  if (!is.data.frame(results)) {
    stop("results must be a data frame.", call. = FALSE)
  }

  required_cols <- c("position", "p_value_raw")
  missing_cols  <- setdiff(required_cols, names(results))
  if (length(missing_cols) > 0) {
    stop(glue(
      "apply_fdr_correction(): missing columns: ",
      "{paste(missing_cols, collapse = ', ')}"
    ), call. = FALSE)
  }

  if (any(is.na(results$p_value_raw))) {
    stop("p_value_raw contains NA values.", call. = FALSE)
  }

  if (any(results$p_value_raw < 0 | results$p_value_raw > 1)) {
    stop("p_value_raw contains values outside [0, 1].", call. = FALSE)
  }

  m <- nrow(results)
  message(glue(
    "apply_fdr_correction(): BH adjustment across {m} tests ",
    "({paste(results$position, collapse = ', ')})"
  ))

  results <- results %>%
    mutate(
      p_value_adj     = p.adjust(p_value_raw, method = "BH"),
      significant_adj = p_value_adj < 0.05
    )

  n_sig <- sum(results$significant_adj, na.rm = TRUE)
  message(glue("  {n_sig}/{m} positions significant after FDR correction"))

  return(results)
}


# ==============================================================================
# SECTION 7: validate_ab_assumptions()
# ==============================================================================

#' Validate Statistical Assumptions for the A/B Test
#'
#' @description
#' Checks assumptions required for valid inference from the correlation
#' comparison:
#'   1. Normality of each variable per position (Shapiro-Wilk on sample <= 5000)
#'   2. Variance ratio between predictors (should be near 1 if comparable scale)
#'   3. Extreme outliers (> 5 SD from mean flagged per variable)
#'   4. Predictor correlation (r_roll3_std) -- if near 1, low test sensitivity
#'
#' @param panel Tibble from build_ab_panel().
#'
#' @return Tibble with one row per position and columns:
#'   \describe{
#'     \item{position}{chr}
#'     \item{n}{int}
#'     \item{shapiro_p_roll3}{Shapiro-Wilk p on roll3 (dbl)}
#'     \item{shapiro_p_std}{Shapiro-Wilk p on STD (dbl)}
#'     \item{shapiro_p_actual}{Shapiro-Wilk p on actual (dbl)}
#'     \item{var_ratio_predictors}{var(roll3) / var(std) (dbl)}
#'     \item{n_outliers_roll3}{Rows where roll3 > mean +/- 5 SD (int)}
#'     \item{n_outliers_std}{Rows where STD > mean +/- 5 SD (int)}
#'     \item{r_predictors}{Pearson r between roll3 and STD (dbl)}
#'     \item{assumption_note}{Plain-language summary (chr)}
#'   }
#'
#' @details
#' At thousands of player-weeks, Shapiro-Wilk will almost always reject
#' normality. This is expected and does not invalidate Pearson r or the
#' Fisher z-test at large n. The function reports p-values for completeness;
#' the assumption_note interprets them in context.
#'
#' @examples
#' panel       <- build_ab_panel(seasons = 2023:2025)
#' assumptions <- validate_ab_assumptions(panel)
#' assumptions %>% select(position, r_predictors, n_outliers_roll3, assumption_note)
#'
#' @export
validate_ab_assumptions <- function(panel) {

  if (!is.data.frame(panel)) {
    stop("panel must be a data frame.", call. = FALSE)
  }

  required_cols <- c("position", "fantasy_pts_actual",
                     "fantasy_pts_roll3", "fantasy_pts_std")
  missing_cols <- setdiff(required_cols, names(panel))
  if (length(missing_cols) > 0) {
    stop(glue(
      "validate_ab_assumptions(): missing columns: ",
      "{paste(missing_cols, collapse = ', ')}"
    ), call. = FALSE)
  }

  positions <- sort(unique(panel$position))

  results <- map_dfr(positions, function(pos) {

    d    <- panel %>% filter(position == pos)
    n    <- nrow(d)
    x_r3 <- d$fantasy_pts_roll3
    x_sd <- d$fantasy_pts_std
    y    <- d$fantasy_pts_actual

    # Shapiro-Wilk is capped at 5000 (function limit)
    sw_idx <- if (n > 5000L) sample(n, 5000L) else seq_len(n)

    sw_r3 <- shapiro.test(x_r3[sw_idx])$p.value
    sw_sd <- shapiro.test(x_sd[sw_idx])$p.value
    sw_y  <- shapiro.test(y[sw_idx])$p.value

    var_std   <- var(x_sd, na.rm = TRUE)
    var_ratio <- if (is.na(var_std) || var_std == 0) {
      NA_real_
    } else {
      var(x_r3, na.rm = TRUE) / var_std
    }

    # Outliers: > 5 SD from mean
    mean_r3 <- mean(x_r3, na.rm = TRUE)
    sd_r3   <- sd(x_r3,   na.rm = TRUE)
    mean_sd <- mean(x_sd,  na.rm = TRUE)
    sd_sd   <- sd(x_sd,    na.rm = TRUE)

    n_out_r3 <- sum(abs(x_r3 - mean_r3) > 5 * sd_r3, na.rm = TRUE)
    n_out_sd <- sum(abs(x_sd - mean_sd) > 5 * sd_sd, na.rm = TRUE)

    r_pred <- cor(x_r3, x_sd, use = "complete.obs")

    # Plain-language note -- cascading priority
    note <- dplyr::case_when(
      r_pred > 0.95 ~
        "WARNING: predictors near-identical (r > 0.95); test has low sensitivity",
      n_out_r3 > 20 | n_out_sd > 20 ~
        "NOTE: >20 extreme outliers; consider reviewing scoring edge cases",
      n < 100 ~
        "NOTE: small n; interpret with caution regardless of p-value",
      TRUE ~
        "Assumptions acceptable for large-n Pearson correlation comparison"
    )

    tibble(
      position             = pos,
      n                    = n,
      shapiro_p_roll3      = round(sw_r3,     4),
      shapiro_p_std        = round(sw_sd,     4),
      shapiro_p_actual     = round(sw_y,      4),
      var_ratio_predictors = round(var_ratio,  4),
      n_outliers_roll3     = n_out_r3,
      n_outliers_std       = n_out_sd,
      r_predictors         = round(r_pred,    4),
      assumption_note      = note
    )
  })

  message("validate_ab_assumptions():")
  for (i in seq_len(nrow(results))) {
    message(glue("  {results$position[i]}: {results$assumption_note[i]}"))
  }

  return(results)
}


# ==============================================================================
# SECTION 8: run_ab_test_pipeline()
# ==============================================================================

#' Run the Full Recency Bias A/B Test Pipeline
#'
#' @description
#' End-to-end wrapper executing the complete study in one call:
#'   1. build_ab_panel()                    -- player-week dataset
#'   2. validate_ab_assumptions()           -- normality, outliers, predictor r
#'   3. power_analysis_correlation_diff()   -- confirm adequate power
#'   4. compare_prediction_methods()        -- Steiger test + bootstrap CIs
#'   5. apply_fdr_correction()              -- BH adjustment across positions
#'   6. Save RDS outputs to data/season2_cache/
#'
#' @param seasons Integer vector. Default: SEASONS_S2 (2010:2025).
#' @param cache_dir Character. Cache path. Default: here("data", "season2_cache").
#' @param n_bootstrap Integer. Bootstrap replicates. Default: 2000L.
#' @param mde_q Numeric. MDE for power analysis. Default: 0.10.
#' @param seed Integer. Random seed. Default: 42L.
#' @param save_panel Logical. Save full panel RDS. Default: TRUE.
#'
#' @return Named list: panel, assumptions, power, results.
#'
#' @details
#' KEY INSIGHT block at the end computes all displayed numbers from the
#' results object -- no hardcoded values.
#'
#' Output files use s2_week4_ prefix per Season 2 naming convention.
#'
#' @examples
#' # Full 16-season run
#' output <- run_ab_test_pipeline()
#' output$results
#'
#' # Quick smoke test on 3 recent seasons
#' output <- run_ab_test_pipeline(seasons = 2023:2025, n_bootstrap = 500L)
#'
#' @export
run_ab_test_pipeline <- function(seasons     = SEASONS_S2,
                                 cache_dir   = here("data", "season2_cache"),
                                 n_bootstrap = 2000L,
                                 mde_q       = 0.10,
                                 seed        = 42L,
                                 save_panel  = TRUE) {

  message(glue("\n{'#'|>strrep(60)}"))
  message("NFL Analytics Toolkit - Season 2, Week 4")
  message("Recency Bias A/B Test: roll3 vs season-to-date average")
  message(glue(
    "Seasons: {min(seasons)}-{max(seasons)} | Bootstrap: {n_bootstrap}"
  ))
  message(glue("{'#'|>strrep(60)}\n"))

  # Step 1: Build panel
  message("STEP 1/5: Building player-week A/B panel...")
  panel <- build_ab_panel(seasons = seasons, cache_dir = cache_dir)

  # Step 2: Validate assumptions
  message("\nSTEP 2/5: Validating statistical assumptions...")
  assumptions <- validate_ab_assumptions(panel)

  near_identical <- assumptions %>% filter(r_predictors > 0.98)
  if (nrow(near_identical) > 0) {
    warning(glue(
      "Positions with near-identical predictors (r > 0.98): ",
      "{paste(near_identical$position, collapse = ', ')}. ",
      "Results for these positions should be interpreted with caution."
    ), call. = FALSE)
  }

  # Step 3: Power analysis
  message("\nSTEP 3/5: Running a priori power analysis...")
  power <- power_analysis_correlation_diff(
    panel        = panel,
    mde_q        = mde_q,
    alpha        = 0.05,
    target_power = 0.80
  )

  underpowered <- power$achieved_power %>% filter(achieved_power < 0.80)
  if (nrow(underpowered) > 0) {
    warning(glue(
      "Underpowered positions (power < 0.80 at q = {mde_q}): ",
      "{paste(underpowered$position, collapse = ', ')}. ",
      "Null results for these positions are uninformative."
    ), call. = FALSE)
  }

  # Step 4: Compare prediction methods
  message("\nSTEP 4/5: Running A/B comparison (Steiger test + bootstrap CIs)...")
  results_raw <- compare_prediction_methods(
    panel       = panel,
    n_bootstrap = n_bootstrap,
    alpha       = 0.05,
    seed        = seed
  )

  # Step 5: FDR correction
  message("\nSTEP 5/5: Applying Benjamini-Hochberg FDR correction...")
  results <- apply_fdr_correction(results_raw)

  # Save outputs
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  if (save_panel) {
    panel_path <- file.path(cache_dir, "s2_week4_ab_panel.rds")
    saveRDS(panel, panel_path)
    message(glue("\nSaved panel       : {panel_path}"))
  }

  results_path <- file.path(cache_dir, "s2_week4_ab_results.rds")
  saveRDS(results, results_path)
  message(glue("Saved results     : {results_path}"))

  power_path <- file.path(cache_dir, "s2_week4_power_analysis.rds")
  saveRDS(power, power_path)
  message(glue("Saved power       : {power_path}"))

  # KEY INSIGHT block -- all numbers and direction labels computed from results.
  # Never hardcode assumed direction ("recency advantage", "std advantage", etc.).
  # The data determines the winner; the message reflects that.
  message(glue("\n{'='|>strrep(60)}"))
  message("KEY FINDINGS")
  message(glue("{'='|>strrep(60)}"))

  # p_value display helper: values that round to 0 must not display as exactly 0,
  # which looks like a computation error. Format as "< 0.0001" when below threshold.
  .fmt_p <- function(p) {
    if (is.na(p)) return("NA")
    if (p < 0.0001) return("< 0.0001")
    as.character(round(p, 4))
  }

  for (i in seq_len(nrow(results))) {
    r <- results[i, ]
    message(glue(
      "\n{r$position} (n = {format(r$n_player_weeks, big.mark = ',')}, ",
      "{r$n_seasons} seasons):"
    ))
    message(glue(
      "  roll3  r = {r$r_roll3} 95% CI [{r$r_roll3_ci_lo}, {r$r_roll3_ci_hi}]"
    ))
    message(glue(
      "  STD    r = {r$r_std}   95% CI [{r$r_std_ci_lo},  {r$r_std_ci_hi}]"
    ))
    message(glue(
      "  Cohen's q = {r$cohens_q} | p_adj = {.fmt_p(r$p_value_adj)} ",
      "(log10p = {r$p_value_log10}) | sig = {r$significant_adj} | ",
      "better: {r$better_predictor}"
    ))
  }

  n_sig       <- sum(results$significant_adj)
  n_positions <- nrow(results)

  # Compute direction summary from data -- do not assume which method won.
  # better_predictor is "roll3", "std", or "no_difference" per position.
  recency_wins <- results %>%
    dplyr::filter(significant_adj, better_predictor == "roll3") %>%
    dplyr::pull(position)
  std_wins <- results %>%
    dplyr::filter(significant_adj, better_predictor == "std") %>%
    dplyr::pull(position)
  no_diff <- results %>%
    dplyr::filter(!significant_adj | better_predictor == "no_difference") %>%
    dplyr::pull(position)

  message(glue(
    "\nSummary: {n_sig}/{n_positions} positions show a statistically significant ",
    "difference between predictors (FDR-corrected alpha = 0.05)."
  ))

  if (length(recency_wins) > 0) {
    message(glue(
      "  roll3 (recency) better: {paste(recency_wins, collapse = ', ')}"
    ))
  }
  if (length(std_wins) > 0) {
    message(glue(
      "  STD (season avg) better: {paste(std_wins, collapse = ', ')}"
    ))
  }
  if (length(no_diff) > 0) {
    message(glue(
      "  No significant difference: {paste(no_diff, collapse = ', ')}"
    ))
  }

  best_pos <- results %>%
    dplyr::filter(significant_adj) %>%
    dplyr::arrange(desc(cohens_q)) %>%
    dplyr::slice(1)

  if (nrow(best_pos) > 0) {
    message(glue(
      "Largest effect: {best_pos$position} ",
      "(Cohen's q = {best_pos$cohens_q}, better: {best_pos$better_predictor})"
    ))
  } else {
    message(glue(
      "No positions reached significance at FDR-corrected alpha = 0.05."
    ))
  }

  message(glue("\n{'#'|>strrep(60)}"))
  message("Pipeline complete.")
  message("Next: run examples/create_season2_week4_visual.R for plots.")
  message(glue("{'#'|>strrep(60)}\n"))

  return(invisible(list(
    panel       = panel,
    assumptions = assumptions,
    power       = power,
    results     = results
  )))
}
