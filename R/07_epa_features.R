# ==============================================================================
# WEEK 5: EPA TREND FEATURES & SITUATIONAL SPLITS
# ==============================================================================
#
# Production-grade EPA trend and situational analysis for NFL player evaluation.
# Built for personal analytics use and portfolio display.
#
# This file contains four main functions:
# 1. calculate_epa_trends()      - Rolling EPA slopes per player
# 2. get_situational_splits()    - EPA by down, distance, quarter, game script
# 3. calculate_stability_metrics() - Week-to-week metric autocorrelation
# 4. get_pressure_performance()  - High-leverage situation EPA
#
# Navigation:
# - Lines   30-330:  calculate_epa_trends()
# - Lines  332-620:  get_situational_splits()
# - Lines  622-850:  calculate_stability_metrics()
# - Lines  852-1080: get_pressure_performance()
#
# Dependencies: dplyr, glue, zoo
# Builds on: R/01_data_loading.R (Week 1), R/02_player_stats.R (Week 2)
#
# NFL Context:
# - EPA = Expected Points Added (EP_after - EP_before)
# - Success rate = proportion of plays with EPA > 0
# - Garbage time: WP <10% or >90% in Q4 (excluded from efficiency metrics)
# - QB kneels/spikes excluded from all efficiency calculations
# - Neutral game script: WP 20-80% (for unbiased efficiency)
#
# ==============================================================================


# ==============================================================================
# SECTION 1: EPA TREND FEATURES
# ==============================================================================

#' Calculate EPA Trend Features Per Player
#'
#' @description
#' Computes rolling EPA slopes (linear trend) over configurable windows for each
#' player. Captures whether a player's efficiency is improving or declining, not
#' just their current level. Uses lagged data to prevent feature leakage.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric season(s) to filter.
#' @param week_min Optional. Minimum week (inclusive).
#' @param week_max Optional. Maximum week (inclusive).
#' @param min_plays_per_game Minimum plays in a game-week to count as a valid
#'   observation. Players with fewer plays are treated as inactive that week.
#'   Default: 5
#' @param trend_windows Numeric vector of window sizes for trend calculation.
#'   Default: c(3, 4, 6). Minimum 3 required for meaningful slope.
#' @param filter_garbage_time Logical. If TRUE (default), excludes plays where
#'   WP <10\% or >90\% in Q4. Recommended for efficiency metrics.
#'
#' @return Tibble with one row per player per game-week containing:
#'   player_id, player_name, season, week, game_id, posteam,
#'   weekly_epa (total EPA that week), plays (play count),
#'   epa_per_play (mean EPA), success_rate,
#'   and for each window: epa_trend_{window} (slope), epa_level_{window} (mean).
#'   Trend columns use lagged data (current game excluded).
#'
#' @details
#' **Trend Calculation:**
#'
#' For each window W, the trend is the OLS slope of EPA per play across the
#' previous W games. A positive slope means the player is improving; negative
#' means declining. The slope is in units of EPA/play per game-week.
#'
#' **Feature Leakage Prevention:**
#'
#' All trend and level columns use only PRIOR games. The current game's EPA
#' is available in weekly_epa / epa_per_play but is NOT included in any
#' trend or rolling column. This is critical for downstream ML models.
#'
#' **NFL Context:**
#' - EPA = EP_after - EP_before (context-adjusted play value)
#' - Garbage time plays (WP <10% or >90% in Q4) excluded by default
#' - QB kneels and spikes are excluded (play_type filter)
#' - Minimum 5 plays/game to count as a valid week (avoids noise from
#'   players with 1-2 garbage time touches)
#' - League average EPA/play: ~0.0 rush, ~+0.08 pass
#'
#' **Minimum window of 3 required:** Linear regression with 1-2 points is
#' degenerate or undefined. Trends require >= 3 data points.
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' epa_trends <- calculate_epa_trends(pbp, season = 2025)
#'
#' # Find players with strongest positive trend (improving)
#' epa_trends %>%
#'   filter(!is.na(epa_trend_4)) %>%
#'   arrange(desc(epa_trend_4)) %>%
#'   head(10)
#' }
#'
#' @seealso
#' \code{\link{get_situational_splits}} for EPA by down/distance/game script
#' \code{\link{calculate_stability_metrics}} for metric autocorrelation
#' \code{\link{get_pressure_performance}} for high-leverage EPA
#'
#' @export
calculate_epa_trends <- function(pbp_data,
                                 season = NULL,
                                 week_min = NULL,
                                 week_max = NULL,
                                 min_plays_per_game = 5,
                                 trend_windows = c(3, 4, 6),
                                 filter_garbage_time = TRUE) {

  library(dplyr)
  library(glue)
  library(zoo)

  # ==========================================================================
  # INPUT VALIDATION
  # ==========================================================================

  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }

  required_cols <- c("game_id", "season", "week", "play_type", "posteam",
                     "epa", "success", "wp", "qtr",
                     "passer_player_id", "rusher_player_id",
                     "receiver_player_id")
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  if (!is.numeric(min_plays_per_game) || min_plays_per_game < 1) {
    stop("min_plays_per_game must be a positive integer")
  }

  if (!is.numeric(trend_windows) || any(trend_windows < 3)) {
    stop("trend_windows must be numeric with all values >= 3 (need 3+ points for slope)")
  }

  # ==========================================================================
  # FILTER DATA
  # ==========================================================================

  if (!is.null(season)) {
    pbp_data <- pbp_data %>% filter(season %in% !!season)
  }
  if (!is.null(week_min)) {
    pbp_data <- pbp_data %>% filter(week >= !!week_min)
  }
  if (!is.null(week_max)) {
    pbp_data <- pbp_data %>% filter(week <= !!week_max)
  }

  # Filter to offensive plays, exclude kneels/spikes
  pbp_clean <- pbp_data %>%
    filter(
      !is.na(epa),
      !is.na(posteam),
      play_type %in% c("pass", "run")
    )

  # Garbage time filter: WP <10% or >90% in Q4
  if (filter_garbage_time) {
    rows_before <- nrow(pbp_clean)
    pbp_clean <- pbp_clean %>%
      filter(!(qtr == 4 & !is.na(wp) & (wp < 0.10 | wp > 0.90)))
    rows_removed <- rows_before - nrow(pbp_clean)
    message(glue("Filtered {format(rows_removed, big.mark = ',')} garbage time plays"))
  }

  if (nrow(pbp_clean) == 0) {
    warning("No plays remaining after filters")
    return(tibble())
  }

  # ==========================================================================
  # ASSIGN PLAYS TO PLAYERS
  # ==========================================================================

  # Each play is attributed to its primary skill player:
  #   pass plays -> passer, run plays -> rusher
  # Receivers are handled through the passer's EPA (team-level receiving
  # captured through passing efficiency)
  player_plays <- pbp_clean %>%
    mutate(
      player_id = case_when(
        play_type == "pass" & !is.na(passer_player_id) ~ passer_player_id,
        play_type == "run"  & !is.na(rusher_player_id) ~ rusher_player_id,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(player_id))

  # ==========================================================================
  # AGGREGATE TO PLAYER-WEEK LEVEL
  # ==========================================================================

  weekly_epa <- player_plays %>%
    group_by(player_id, season, week, game_id, posteam) %>%
    summarise(
      # Capture player name from most common appearance
      player_name = {
        pass_name <- first(na.omit(passer_player_id))
        rush_name <- first(na.omit(rusher_player_id))
        # Use the description-based name approach
        NA_character_
      },
      plays = n(),
      weekly_epa = sum(epa, na.rm = TRUE),
      epa_per_play = mean(epa, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      pass_plays = sum(play_type == "pass", na.rm = TRUE),
      run_plays = sum(play_type == "run", na.rm = TRUE),
      .groups = "drop"
    )

  # Get player names from the play data more reliably
  player_names <- player_plays %>%
    mutate(
      name = case_when(
        play_type == "pass" ~ as.character(passer_player_name),
        play_type == "run"  ~ as.character(rusher_player_name),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(name)) %>%
    group_by(player_id) %>%
    summarise(player_name = first(name), .groups = "drop")

  # Join names and drop the placeholder
  weekly_epa <- weekly_epa %>%
    select(-player_name) %>%
    left_join(player_names, by = "player_id")

  # Filter to games where player had meaningful involvement
  weekly_epa <- weekly_epa %>%
    filter(plays >= min_plays_per_game) %>%
    arrange(player_id, season, week)

  message(glue("Aggregated {nrow(weekly_epa)} player-weeks ({n_distinct(weekly_epa$player_id)} players)"))

  # ==========================================================================
  # CALCULATE EPA TRENDS (LAGGED - NO LEAKAGE)
  # ==========================================================================

  # Helper: OLS slope of y over x = 1:n
  calc_slope <- function(vals) {
    n <- length(vals)
    non_na <- sum(!is.na(vals))
    if (non_na < 3) return(NA_real_)  # Need 3+ points for meaningful slope
    x <- seq_len(n)
    valid <- !is.na(vals)
    if (sum(valid) < 3) return(NA_real_)
    fit <- lm(vals[valid] ~ x[valid])
    coef(fit)[2]
  }

  for (w in trend_windows) {

    trend_col <- glue("epa_trend_{w}")
    level_col <- glue("epa_level_{w}")

    weekly_epa <- weekly_epa %>%
      group_by(player_id) %>%
      mutate(
        .row_num = row_number(),

        # Lagged EPA per play (exclude current game)
        .lagged_epa = lag(epa_per_play, n = 1),

        # Rolling slope over previous w games
        !!trend_col := {
          zoo::rollapply(
            .lagged_epa,
            width = w,
            FUN = calc_slope,
            align = "right",
            fill = NA,
            partial = FALSE
          )
        },

        # Rolling mean level over previous w games
        !!level_col := {
          zoo::rollapply(
            .lagged_epa,
            width = w,
            FUN = function(x) {
              if (sum(!is.na(x)) >= 2) mean(x, na.rm = TRUE) else NA_real_
            },
            align = "right",
            fill = NA,
            partial = FALSE
          )
        }
      ) %>%
      # First w rows can't have a valid trend (insufficient prior games)
      mutate(
        !!trend_col := ifelse(.row_num <= w, NA_real_, !!sym(trend_col)),
        !!level_col := ifelse(.row_num <= w, NA_real_, !!sym(level_col))
      ) %>%
      select(-.row_num, -.lagged_epa) %>%
      ungroup()

    valid_trends <- sum(!is.na(weekly_epa[[trend_col]]))
    message(glue("  {w}-game EPA trend: {valid_trends} valid player-weeks"))
  }

  message(glue("Calculated EPA trends for {n_distinct(weekly_epa$player_id)} players"))

  return(weekly_epa)
}


# ==============================================================================
# SECTION 2: SITUATIONAL SPLITS
# ==============================================================================

#' Get Situational EPA Splits
#'
#' @description
#' Breaks down player EPA by down, distance bucket, quarter, and game script.
#' Reveals how players perform in different contexts -- critical for identifying
#' players whose aggregate stats mask situational strengths or weaknesses.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric season(s) to filter.
#' @param week_min Optional. Minimum week (inclusive).
#' @param week_max Optional. Maximum week (inclusive).
#' @param min_plays Minimum plays in a split to return a result. Splits with
#'   fewer plays return NA for efficiency metrics. Default: 10
#' @param filter_garbage_time Logical. Exclude garbage time plays. Default: TRUE
#'
#' @return Tibble with one row per player per split_type per split_value:
#'   player_id, player_name, posteam, split_type (chr: "down", "distance",
#'   "quarter", "game_script"), split_value (chr: the category value),
#'   plays, total_epa, epa_per_play, success_rate.
#'
#' @details
#' **Split Definitions:**
#'
#' - **Down:** 1st, 2nd, 3rd, 4th (separate analysis per down)
#' - **Distance:** short (1-3 yards), medium (4-7), long (8+)
#' - **Quarter:** Q1, Q2, Q3, Q4, OT
#' - **Game Script:** leading (WP > 0.60), trailing (WP < 0.40),
#'   neutral (WP 0.40-0.60)
#'
#' **NFL Context:**
#' - Down context matters enormously: a 4-yard gain on 3rd-and-3 (success,
#'   positive EPA) differs from 4 yards on 3rd-and-10 (failure, negative EPA)
#' - Distance buckets: short = likely run/play-action, long = obvious passing
#' - Game script splits reveal script-dependent players (RBs who only produce
#'   when team is ahead; WRs who pad stats in garbage time)
#' - Minimum 10 plays per split avoids noise from tiny samples
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' splits <- get_situational_splits(pbp, season = 2025)
#'
#' # Compare a RB's performance by game script
#' splits %>%
#'   filter(player_name == "Saquon Barkley", split_type == "game_script")
#' }
#'
#' @seealso
#' \code{\link{calculate_epa_trends}} for EPA trend features
#' \code{\link{get_pressure_performance}} for high-leverage situations
#'
#' @export
get_situational_splits <- function(pbp_data,
                                   season = NULL,
                                   week_min = NULL,
                                   week_max = NULL,
                                   min_plays = 10,
                                   filter_garbage_time = TRUE) {

  library(dplyr)
  library(glue)

  # ==========================================================================
  # INPUT VALIDATION
  # ==========================================================================

  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }

  required_cols <- c("game_id", "season", "week", "play_type", "posteam",
                     "epa", "success", "wp", "qtr", "down", "ydstogo",
                     "passer_player_id", "passer_player_name",
                     "rusher_player_id", "rusher_player_name")
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  if (!is.numeric(min_plays) || min_plays < 1) {
    stop("min_plays must be a positive integer")
  }

  # ==========================================================================
  # FILTER DATA
  # ==========================================================================

  if (!is.null(season)) {
    pbp_data <- pbp_data %>% filter(season %in% !!season)
  }
  if (!is.null(week_min)) {
    pbp_data <- pbp_data %>% filter(week >= !!week_min)
  }
  if (!is.null(week_max)) {
    pbp_data <- pbp_data %>% filter(week <= !!week_max)
  }

  pbp_clean <- pbp_data %>%
    filter(
      !is.na(epa),
      !is.na(posteam),
      play_type %in% c("pass", "run")
    )

  if (filter_garbage_time) {
    pbp_clean <- pbp_clean %>%
      filter(!(qtr == 4 & !is.na(wp) & (wp < 0.10 | wp > 0.90)))
  }

  if (nrow(pbp_clean) == 0) {
    warning("No plays remaining after filters")
    return(tibble())
  }

  # ==========================================================================
  # ASSIGN PLAYER IDS
  # ==========================================================================

  pbp_clean <- pbp_clean %>%
    mutate(
      player_id = case_when(
        play_type == "pass" & !is.na(passer_player_id) ~ passer_player_id,
        play_type == "run"  & !is.na(rusher_player_id) ~ rusher_player_id,
        TRUE ~ NA_character_
      ),
      player_name = case_when(
        play_type == "pass" & !is.na(passer_player_name) ~ passer_player_name,
        play_type == "run"  & !is.na(rusher_player_name) ~ rusher_player_name,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(player_id))

  # ==========================================================================
  # CREATE SPLIT VARIABLES
  # ==========================================================================

  pbp_splits <- pbp_clean %>%
    mutate(
      # Down split (1st, 2nd, 3rd, 4th)
      down_split = ifelse(!is.na(down), paste0("down_", down), NA_character_),

      # Distance buckets: short (1-3), medium (4-7), long (8+)
      distance_split = case_when(
        is.na(ydstogo) ~ NA_character_,
        ydstogo <= 3   ~ "short_1_3",
        ydstogo <= 7   ~ "medium_4_7",
        TRUE           ~ "long_8plus"
      ),

      # Quarter split
      quarter_split = case_when(
        is.na(qtr) ~ NA_character_,
        qtr <= 4   ~ paste0("Q", qtr),
        TRUE       ~ "OT"
      ),

      # Game script: leading (WP > 0.60), trailing (WP < 0.40), neutral
      game_script_split = case_when(
        is.na(wp)  ~ NA_character_,
        wp > 0.60  ~ "leading",
        wp < 0.40  ~ "trailing",
        TRUE       ~ "neutral"
      )
    )

  # ==========================================================================
  # COMPUTE SPLITS
  # ==========================================================================

  # Helper to aggregate one split type
  aggregate_split <- function(data, split_col, split_name) {
    data %>%
      filter(!is.na(!!sym(split_col))) %>%
      group_by(player_id, player_name, posteam, split_value = !!sym(split_col)) %>%
      summarise(
        plays = n(),
        total_epa = sum(epa, na.rm = TRUE),
        epa_per_play = ifelse(plays >= min_plays, mean(epa, na.rm = TRUE), NA_real_),
        success_rate = ifelse(plays >= min_plays, mean(success, na.rm = TRUE), NA_real_),
        .groups = "drop"
      ) %>%
      mutate(split_type = split_name) %>%
      # Use most recent team per player
      group_by(player_id) %>%
      mutate(posteam = last(posteam)) %>%
      ungroup()
  }

  splits_down     <- aggregate_split(pbp_splits, "down_split", "down")
  splits_distance <- aggregate_split(pbp_splits, "distance_split", "distance")
  splits_quarter  <- aggregate_split(pbp_splits, "quarter_split", "quarter")
  splits_script   <- aggregate_split(pbp_splits, "game_script_split", "game_script")

  all_splits <- bind_rows(splits_down, splits_distance, splits_quarter, splits_script) %>%
    select(player_id, player_name, posteam, split_type, split_value,
           plays, total_epa, epa_per_play, success_rate) %>%
    arrange(player_id, split_type, split_value)

  n_players <- n_distinct(all_splits$player_id)
  n_splits <- nrow(all_splits)
  message(glue("Generated {n_splits} split rows for {n_players} players"))
  message(glue("Split types: down ({nrow(splits_down)}), distance ({nrow(splits_distance)}), quarter ({nrow(splits_quarter)}), game_script ({nrow(splits_script)})"))

  return(all_splits)
}


# ==============================================================================
# SECTION 3: STABILITY METRICS
# ==============================================================================

#' Calculate Week-to-Week Metric Stability
#'
#' @description
#' Measures how "sticky" each metric is from week to week using autocorrelation.
#' High-stability metrics reflect talent; low-stability metrics reflect noise.
#' This is critical for knowing which features to trust in ML models.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric season(s) to filter.
#' @param week_min Optional. Minimum week (inclusive).
#' @param week_max Optional. Maximum week (inclusive).
#' @param min_plays_per_game Minimum plays in a game-week to include. Default: 5
#' @param min_games Minimum total games for a player to be included. Default: 8
#'   (half a season, as recommended by Week 4 findings)
#' @param filter_garbage_time Logical. Exclude garbage time. Default: TRUE
#'
#' @return Tibble with one row per metric containing:
#'   metric (chr), autocorrelation (dbl: week-to-week Pearson r),
#'   split_half_r (dbl: odd/even week correlation),
#'   n_players (int: sample size), stability_tier (chr: "high", "moderate",
#'   "low", "unstable").
#'
#' @details
#' **Stability Tiers (based on Week 4 findings):**
#' - High (r >= 0.50): Volume metrics (attempts, targets, yards)
#' - Moderate (r 0.30-0.49): Some rate metrics (completion pct, success rate)
#' - Low (r 0.15-0.29): Touchdowns, some efficiency metrics
#' - Unstable (r < 0.15): EPA per play, highly context-dependent metrics
#'
#' **NFL Context:**
#' Volume metrics are stable because they reflect coaching decisions about
#' player roles -- decisions that change slowly. Efficiency metrics are
#' unstable because they reflect performance against specific opponents,
#' which varies game to game. This matches Week 4 predictive validity
#' findings: volume predicts volume, but EPA adds no incremental validity.
#'
#' **Methodology:**
#' - Autocorrelation: cor(metric_week_n, metric_week_n+1) across all players
#' - Split-half: cor(mean_odd_weeks, mean_even_weeks) across players
#' - Both use Pearson r with pairwise complete observations
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' stability <- calculate_stability_metrics(pbp, season = 2025)
#'
#' # View metrics ranked by stability
#' stability %>% arrange(desc(autocorrelation))
#' }
#'
#' @seealso
#' \code{\link{calculate_epa_trends}} for EPA trend features
#' \code{\link{get_situational_splits}} for situational EPA
#'
#' @export
calculate_stability_metrics <- function(pbp_data,
                                        season = NULL,
                                        week_min = NULL,
                                        week_max = NULL,
                                        min_plays_per_game = 5,
                                        min_games = 8,
                                        filter_garbage_time = TRUE) {

  library(dplyr)
  library(glue)

  # ==========================================================================
  # INPUT VALIDATION
  # ==========================================================================

  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }

  required_cols <- c("game_id", "season", "week", "play_type", "posteam",
                     "epa", "success", "wp", "qtr",
                     "rushing_yards", "passing_yards", "receiving_yards",
                     "complete_pass", "air_yards",
                     "passer_player_id", "passer_player_name",
                     "rusher_player_id", "rusher_player_name",
                     "receiver_player_id", "receiver_player_name")
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  # ==========================================================================
  # FILTER DATA
  # ==========================================================================

  if (!is.null(season)) {
    pbp_data <- pbp_data %>% filter(season %in% !!season)
  }
  if (!is.null(week_min)) {
    pbp_data <- pbp_data %>% filter(week >= !!week_min)
  }
  if (!is.null(week_max)) {
    pbp_data <- pbp_data %>% filter(week <= !!week_max)
  }

  pbp_clean <- pbp_data %>%
    filter(
      !is.na(epa),
      !is.na(posteam),
      play_type %in% c("pass", "run")
    )

  if (filter_garbage_time) {
    pbp_clean <- pbp_clean %>%
      filter(!(qtr == 4 & !is.na(wp) & (wp < 0.10 | wp > 0.90)))
  }

  if (nrow(pbp_clean) == 0) {
    warning("No plays remaining after filters")
    return(tibble())
  }

  message(glue("Analyzing stability across {n_distinct(pbp_clean$game_id)} games..."))

  # ==========================================================================
  # BUILD PLAYER-WEEK METRIC MATRIX
  # ==========================================================================

  # QB weekly metrics
  qb_weekly <- pbp_clean %>%
    filter(!is.na(passer_player_id), play_type == "pass") %>%
    group_by(player_id = passer_player_id, player_name = passer_player_name,
             season, week) %>%
    summarise(
      plays = n(),
      epa_per_play = mean(epa, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      passing_yards = sum(passing_yards, na.rm = TRUE),
      completion_pct = ifelse(n() > 0, mean(complete_pass, na.rm = TRUE), NA_real_),
      air_yards_per_att = mean(air_yards, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(plays >= min_plays_per_game)

  # RB weekly metrics
  rb_weekly <- pbp_clean %>%
    filter(!is.na(rusher_player_id), play_type == "run") %>%
    group_by(player_id = rusher_player_id, player_name = rusher_player_name,
             season, week) %>%
    summarise(
      plays = n(),
      epa_per_play = mean(epa, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      rushing_yards = sum(rushing_yards, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(plays >= min_plays_per_game)

  # WR/TE weekly metrics (receiver-level)
  wr_weekly <- pbp_clean %>%
    filter(!is.na(receiver_player_id), play_type == "pass") %>%
    group_by(player_id = receiver_player_id, player_name = receiver_player_name,
             season, week) %>%
    summarise(
      plays = n(),  # targets
      epa_per_target = mean(epa, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      receiving_yards = sum(receiving_yards, na.rm = TRUE),
      catch_rate = ifelse(n() > 0, mean(complete_pass, na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) %>%
    filter(plays >= min_plays_per_game)

  # ==========================================================================
  # CALCULATE AUTOCORRELATION AND SPLIT-HALF
  # ==========================================================================

  calc_stability <- function(weekly_data, metric_col, metric_name) {
    # Filter to players with enough games
    qualified <- weekly_data %>%
      group_by(player_id) %>%
      filter(n() >= min_games) %>%
      ungroup()

    if (nrow(qualified) == 0 || n_distinct(qualified$player_id) < 5) {
      return(tibble(
        metric = metric_name,
        autocorrelation = NA_real_,
        split_half_r = NA_real_,
        n_players = 0L,
        stability_tier = "insufficient_data"
      ))
    }

    # Week-to-week autocorrelation
    paired <- qualified %>%
      arrange(player_id, season, week) %>%
      group_by(player_id) %>%
      mutate(next_val = lead(!!sym(metric_col))) %>%
      ungroup() %>%
      filter(!is.na(!!sym(metric_col)), !is.na(next_val))

    auto_r <- if (nrow(paired) >= 10) {
      suppressWarnings(cor(paired[[metric_col]], paired$next_val,
                           use = "pairwise.complete.obs"))
    } else {
      NA_real_
    }

    # Split-half: odd weeks vs even weeks
    split_data <- qualified %>%
      mutate(half = ifelse(week %% 2 == 1, "odd", "even")) %>%
      group_by(player_id, half) %>%
      summarise(mean_val = mean(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = half, values_from = mean_val) %>%
      filter(!is.na(odd), !is.na(even))

    split_r <- if (nrow(split_data) >= 5) {
      suppressWarnings(cor(split_data$odd, split_data$even,
                           use = "pairwise.complete.obs"))
    } else {
      NA_real_
    }

    # Classify tier
    tier <- case_when(
      is.na(auto_r) ~ "insufficient_data",
      auto_r >= 0.50 ~ "high",
      auto_r >= 0.30 ~ "moderate",
      auto_r >= 0.15 ~ "low",
      TRUE ~ "unstable"
    )

    tibble(
      metric = metric_name,
      autocorrelation = round(auto_r, 3),
      split_half_r = round(split_r, 3),
      n_players = n_distinct(qualified$player_id),
      stability_tier = tier
    )
  }

  # Run stability analysis for each metric
  results <- bind_rows(
    # QB metrics
    calc_stability(qb_weekly, "epa_per_play", "qb_epa_per_play"),
    calc_stability(qb_weekly, "success_rate", "qb_success_rate"),
    calc_stability(qb_weekly, "passing_yards", "qb_passing_yards"),
    calc_stability(qb_weekly, "completion_pct", "qb_completion_pct"),
    calc_stability(qb_weekly, "air_yards_per_att", "qb_air_yards_per_att"),

    # RB metrics
    calc_stability(rb_weekly, "epa_per_play", "rb_epa_per_play"),
    calc_stability(rb_weekly, "success_rate", "rb_success_rate"),
    calc_stability(rb_weekly, "rushing_yards", "rb_rushing_yards"),

    # WR/TE metrics
    calc_stability(wr_weekly, "epa_per_target", "wr_epa_per_target"),
    calc_stability(wr_weekly, "success_rate", "wr_success_rate"),
    calc_stability(wr_weekly, "receiving_yards", "wr_receiving_yards"),
    calc_stability(wr_weekly, "catch_rate", "wr_catch_rate")
  ) %>%
    arrange(desc(autocorrelation))

  message(glue("Calculated stability for {nrow(results)} metrics"))

  return(results)
}


# ==============================================================================
# SECTION 4: PRESSURE PERFORMANCE (HIGH-LEVERAGE)
# ==============================================================================

#' Get High-Leverage Performance Metrics
#'
#' @description
#' Calculates player EPA in high-leverage situations: close games in Q4,
#' 3rd down conversions, and red zone plays. Identifies clutch performers
#' versus garbage-time producers.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param season Optional. Numeric season(s) to filter.
#' @param week_min Optional. Minimum week (inclusive).
#' @param week_max Optional. Maximum week (inclusive).
#' @param min_plays Minimum plays in a leverage category to return metrics.
#'   Default: 10. Small samples inherent in leverage splits -- always check
#'   the plays column.
#'
#' @return Tibble with one row per player per leverage_type per leverage_value:
#'   player_id, player_name, posteam, leverage_type (chr: "quarter_score",
#'   "third_down", "red_zone"), leverage_value (chr: category),
#'   plays, total_epa, epa_per_play, success_rate.
#'
#' @details
#' **Leverage Categories:**
#'
#' - **quarter_score:** Q4 close games (score differential <= 8 points),
#'   Q4 blowout (> 8 points), and non-Q4 for baseline comparison
#' - **third_down:** 3rd down plays vs all other downs (3rd down is where
#'   drive success is determined)
#' - **red_zone:** Inside the 20-yard line vs outside (where TDs happen)
#'
#' **NFL Context:**
#' - Close Q4 games have highest leverage (WPA swings are largest)
#' - 3rd down conversion is the most consequential regular-down situation
#' - Red zone efficiency separates TD scorers from FG settlers
#' - Small sample warning: a player may have only 15-20 plays in each
#'   leverage bucket over a full season. Show play counts alongside rates.
#'
#' **Sample Size Caution:**
#' Leverage splits produce small samples by definition. A player with
#' 15 red zone plays has wide confidence intervals. These features are
#' EXPERIMENTAL for ML models and should be validated before weighting.
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' pressure <- get_pressure_performance(pbp, season = 2025)
#'
#' # Find best QBs in close Q4 games
#' pressure %>%
#'   filter(leverage_type == "quarter_score",
#'          leverage_value == "q4_close",
#'          plays >= 20) %>%
#'   arrange(desc(epa_per_play))
#' }
#'
#' @seealso
#' \code{\link{get_situational_splits}} for down/distance/game script splits
#' \code{\link{calculate_epa_trends}} for EPA trend features
#'
#' @export
get_pressure_performance <- function(pbp_data,
                                     season = NULL,
                                     week_min = NULL,
                                     week_max = NULL,
                                     min_plays = 10) {

  library(dplyr)
  library(glue)

  # ==========================================================================
  # INPUT VALIDATION
  # ==========================================================================

  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }

  required_cols <- c("game_id", "season", "week", "play_type", "posteam",
                     "epa", "success", "qtr", "down", "score_differential",
                     "yardline_100",
                     "passer_player_id", "passer_player_name",
                     "rusher_player_id", "rusher_player_name")
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  if (!is.numeric(min_plays) || min_plays < 1) {
    stop("min_plays must be a positive integer")
  }

  # ==========================================================================
  # FILTER DATA (no garbage time filter here -- leverage includes Q4)
  # ==========================================================================

  if (!is.null(season)) {
    pbp_data <- pbp_data %>% filter(season %in% !!season)
  }
  if (!is.null(week_min)) {
    pbp_data <- pbp_data %>% filter(week >= !!week_min)
  }
  if (!is.null(week_max)) {
    pbp_data <- pbp_data %>% filter(week <= !!week_max)
  }

  pbp_clean <- pbp_data %>%
    filter(
      !is.na(epa),
      !is.na(posteam),
      play_type %in% c("pass", "run")
    )

  if (nrow(pbp_clean) == 0) {
    warning("No plays remaining after filters")
    return(tibble())
  }

  # ==========================================================================
  # ASSIGN PLAYER IDS
  # ==========================================================================

  pbp_clean <- pbp_clean %>%
    mutate(
      player_id = case_when(
        play_type == "pass" & !is.na(passer_player_id) ~ passer_player_id,
        play_type == "run"  & !is.na(rusher_player_id) ~ rusher_player_id,
        TRUE ~ NA_character_
      ),
      player_name = case_when(
        play_type == "pass" & !is.na(passer_player_name) ~ passer_player_name,
        play_type == "run"  & !is.na(rusher_player_name) ~ rusher_player_name,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(player_id))

  # ==========================================================================
  # CREATE LEVERAGE VARIABLES
  # ==========================================================================

  pbp_leverage <- pbp_clean %>%
    mutate(
      # Q4 close vs blowout vs non-Q4
      quarter_score = case_when(
        qtr == 4 & !is.na(score_differential) & abs(score_differential) <= 8 ~ "q4_close",
        qtr == 4 & !is.na(score_differential) & abs(score_differential) > 8  ~ "q4_blowout",
        qtr < 4  ~ "non_q4",
        TRUE     ~ NA_character_
      ),

      # 3rd down vs other
      third_down = case_when(
        !is.na(down) & down == 3 ~ "third_down",
        !is.na(down) & down != 3 ~ "other_downs",
        TRUE ~ NA_character_
      ),

      # Red zone (inside 20) vs outside
      # yardline_100 = yards from opponent end zone; red zone = yardline_100 <= 20
      red_zone = case_when(
        !is.na(yardline_100) & yardline_100 <= 20 ~ "red_zone",
        !is.na(yardline_100) & yardline_100 > 20  ~ "outside_red_zone",
        TRUE ~ NA_character_
      )
    )

  # ==========================================================================
  # COMPUTE LEVERAGE SPLITS
  # ==========================================================================

  aggregate_leverage <- function(data, split_col, split_name) {
    data %>%
      filter(!is.na(!!sym(split_col))) %>%
      group_by(player_id, player_name, posteam, leverage_value = !!sym(split_col)) %>%
      summarise(
        plays = n(),
        total_epa = sum(epa, na.rm = TRUE),
        epa_per_play = ifelse(plays >= min_plays, mean(epa, na.rm = TRUE), NA_real_),
        success_rate = ifelse(plays >= min_plays, mean(success, na.rm = TRUE), NA_real_),
        .groups = "drop"
      ) %>%
      mutate(leverage_type = split_name) %>%
      group_by(player_id) %>%
      mutate(posteam = last(posteam)) %>%
      ungroup()
  }

  lev_qtr     <- aggregate_leverage(pbp_leverage, "quarter_score", "quarter_score")
  lev_third   <- aggregate_leverage(pbp_leverage, "third_down", "third_down")
  lev_redzone <- aggregate_leverage(pbp_leverage, "red_zone", "red_zone")

  all_leverage <- bind_rows(lev_qtr, lev_third, lev_redzone) %>%
    select(player_id, player_name, posteam, leverage_type, leverage_value,
           plays, total_epa, epa_per_play, success_rate) %>%
    arrange(player_id, leverage_type, leverage_value)

  n_players <- n_distinct(all_leverage$player_id)
  message(glue("Generated {nrow(all_leverage)} leverage rows for {n_players} players"))
  message(glue("Leverage types: quarter_score ({nrow(lev_qtr)}), third_down ({nrow(lev_third)}), red_zone ({nrow(lev_redzone)})"))

  return(all_leverage)
}
