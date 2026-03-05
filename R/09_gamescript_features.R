# ==============================================================================
# WEEK 7: GAME SCRIPT & LEVERAGE FEATURES
# ==============================================================================
#
# Production-grade game script and leverage metrics for NFL player evaluation.
# Built for personal analytics use and portfolio display.
#
# Game script is the single biggest driver of play-calling bias. A team leading
# by 14 in the 4th quarter will run the ball to kill clock even with an
# inefficient RB. A team trailing will abandon the run entirely. Without
# accounting for this, raw usage and efficiency metrics are contaminated by
# game state rather than talent.
#
# This file contains four main functions:
# 1. get_game_script_splits()         - Lines  65-360
#    Per-player EPA and usage in leading/neutral/trailing game scripts.
# 2. calculate_leverage_features()    - Lines 362-590
#    Win probability added (WPA), high-leverage play identification, clutch rate.
# 3. get_comeback_profile()           - Lines 592-790
#    Performance when trailing by 8+ points. Separates scheme garbage from
#    true production under duress.
# 4. calculate_script_adjusted_epa()  - Lines 792-1000
#    EPA adjusted for game script using neutral-game baseline. Removes the
#    leading/trailing bias from raw per-play EPA.
#
# Dependencies:
#   Existing: dplyr, glue, zoo, tidyr, here
#   New this week: (none -- all dependencies already present)
#
# Builds on:
#   Week 1: load_and_validate_pbp() -> pbp tibble
#   Week 2: get_player_rushing_stats(), get_player_receiving_stats()
#   Week 5: calculate_epa_trends(), get_situational_splits() -- WP thresholds
#   Week 6: calculate_usage_metrics() -- role context
#
# NFL Context:
#   - Game script = win probability-based classification of game state
#   - Leading (WP > 0.60): offense runs more, passes shorter, protects lead
#   - Neutral (WP 0.40-0.60): most representative of true offensive intent
#   - Trailing (WP < 0.40): offense abandons run, WRs get garbage-time targets
#   - Leverage = how much a single play could swing win probability
#   - High-leverage: |WPA| >= 0.05 (5 pct point swing possible)
#   - Garbage time: WP < 0.10 or > 0.90 in Q4 (excluded from all metrics)
#   - "Clutch rate" = positive WPA rate on high-leverage plays (small sample warning)
#   - Script-adjusted EPA is the cleanest apples-to-apples efficiency metric
#
# nflfastR column reference (verified):
#   epa               -- Expected Points Added per play
#   wp                -- Win probability for possessing team (0 to 1)
#   wpa               -- Win probability added on this play
#   play_type         -- "pass", "run", "qb_kneel", "qb_spike", etc.
#   posteam           -- Possessing (offensive) team abbreviation
#   defteam           -- Defending team abbreviation
#   qtr               -- Quarter (1-5, 5 = OT)
#   score_differential     -- Possessing team score minus defending team score
#   passer_player_id  -- QB GSIS ID on pass plays
#   rusher_player_id  -- Ball carrier GSIS ID on run plays
#   receiver_player_id-- Target GSIS ID on pass plays
#   passer_player_name  -- QB name
#   rusher_player_name  -- Ball carrier name
#   receiver_player_name-- Target name
#   qb_kneel          -- 1 if QB kneel play
#   qb_spike          -- 1 if spike play
#   qb_scramble       -- 1 if QB scramble
#   season, week, game_id
#
# ==============================================================================

library(dplyr)
library(glue)
library(zoo)
library(tidyr)
library(here)


# ==============================================================================
# SECTION 1: GAME SCRIPT SPLITS
# ==============================================================================

#' Get Per-Player EPA and Usage by Game Script
#'
#' @description
#' Computes per-player efficiency and usage metrics separated by game script:
#' leading (WP > 0.60), neutral (WP 0.40-0.60), and trailing (WP < 0.40).
#'
#' Neutral-script stats are the most predictive of true talent. Leading-script
#' inflates RB production (clock-killing runs) and depresses WR usage.
#' Trailing-script inflates WR target counts (garbage-time volume) and
#' collapses RB usage.
#'
#' Garbage time is excluded: WP < 0.10 or WP > 0.90 in Q4.
#'
#' @param pbp_data Play-by-play tibble from load_and_validate_pbp().
#'   Must contain: play_type, posteam, season, week, game_id, epa, wp, qtr,
#'   passer_player_id, rusher_player_id, receiver_player_id,
#'   passer_player_name, rusher_player_name, receiver_player_name,
#'   qb_kneel, qb_spike, qb_scramble
#' @param season Optional numeric scalar or vector. Filter to specific season(s).
#'   If NULL, all seasons in data are used.
#' @param week_min Optional numeric. Minimum week (inclusive). Default: 1.
#' @param week_max Optional numeric. Maximum week (inclusive). Default: 22.
#' @param leading_threshold Win probability above which team is "leading".
#'   Default: 0.60.
#' @param trailing_threshold Win probability below which team is "trailing".
#'   Default: 0.40.
#' @param min_plays Minimum total clean plays per player-season. Default: 10.
#'
#' @return Tibble with one row per player-position_group-season. Columns:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Player name (chr)}
#'     \item{position_group}{"passer", "rusher", or "receiver" (chr)}
#'     \item{team}{Most frequent team this season (chr)}
#'     \item{total_plays}{Total clean plays included (int)}
#'     \item{neutral_plays}{Plays in neutral script (int)}
#'     \item{leading_plays}{Plays when team is leading (int)}
#'     \item{trailing_plays}{Plays when team is trailing (int)}
#'     \item{neutral_epa_per_play}{Mean EPA in neutral script (dbl)}
#'     \item{leading_epa_per_play}{Mean EPA when leading (dbl)}
#'     \item{trailing_epa_per_play}{Mean EPA when trailing (dbl)}
#'     \item{neutral_success_rate}{Success rate (EPA > 0) in neutral script (dbl)}
#'     \item{leading_success_rate}{Success rate when leading (dbl)}
#'     \item{trailing_success_rate}{Success rate when trailing (dbl)}
#'     \item{neutral_share}{Proportion of plays in neutral script (dbl)}
#'     \item{leading_share}{Proportion of plays when leading (dbl)}
#'     \item{trailing_share}{Proportion of plays when trailing (dbl)}
#'     \item{script_epa_delta}{neutral_epa_per_play minus trailing_epa_per_play.
#'       Large positive: production neutral-dependent (not garbage-time inflated).
#'       Large negative: production trailing-dependent (potential garbage time). (dbl)}
#'   }
#'
#' @details
#' A player may appear in multiple position groups (e.g., an RB who also
#' receives targets will appear as both "rusher" and "receiver").
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' script_splits <- get_game_script_splits(pbp, season = 2025)
#' names(script_splits)
#'
#' # RBs most dependent on leading script
#' script_splits %>%
#'   filter(position_group == "rusher", total_plays >= 50) %>%
#'   arrange(desc(leading_share)) %>%
#'   select(player_name, team, leading_share, neutral_epa_per_play,
#'          leading_epa_per_play, script_epa_delta) %>%
#'   head(10)
#' }
#'
#' @seealso [calculate_leverage_features()] for WPA-based leverage metrics,
#'   [calculate_script_adjusted_epa()] for bias-corrected efficiency,
#'   [get_situational_splits()] (Week 5) for down/distance context
#'
#' @export
get_game_script_splits <- function(pbp_data,
                                   season             = NULL,
                                   week_min           = 1,
                                   week_max           = 22,
                                   leading_threshold  = 0.60,
                                   trailing_threshold = 0.40,
                                   min_plays          = 10) {

  # --- Input validation ---
  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame or tibble.")
  }
  required_cols <- c(
    "play_type", "posteam", "season", "week", "game_id", "epa", "wp", "qtr",
    "passer_player_id", "rusher_player_id", "receiver_player_id",
    "passer_player_name", "rusher_player_name", "receiver_player_name",
    "qb_kneel", "qb_spike", "qb_scramble"
  )
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }
  if (!is.null(season) && !is.numeric(season)) stop("season must be numeric or NULL.")
  if (!is.numeric(week_min) || !is.numeric(week_max)) stop("week_min and week_max must be numeric.")
  if (week_min > week_max) stop(glue("week_min ({week_min}) cannot exceed week_max ({week_max})."))
  if (!is.numeric(leading_threshold) || leading_threshold <= 0.5 || leading_threshold >= 1) {
    stop("leading_threshold must be between 0.5 and 1.0 (exclusive).")
  }
  if (!is.numeric(trailing_threshold) || trailing_threshold <= 0 || trailing_threshold >= 0.5) {
    stop("trailing_threshold must be between 0.0 and 0.5 (exclusive).")
  }
  if (!is.numeric(min_plays) || min_plays < 1) stop("min_plays must be a positive integer.")
  if (nrow(pbp_data) == 0) {
    message("pbp_data is empty. Returning empty game script tibble.")
    return(tibble())
  }

  # --- Scope filter ---
  if (!is.null(season)) pbp_data <- pbp_data %>% filter(season %in% !!season)
  pbp_data <- pbp_data %>% filter(week >= week_min, week <= week_max)

  # --- Clean and classify by game script ---
  plays_raw <- nrow(pbp_data)
  pbp_clean <- pbp_data %>%
    filter(
      !is.na(posteam), !is.na(epa), !is.na(wp),
      play_type %in% c("pass", "run"),
      qb_kneel == 0, qb_spike == 0,
      !(qtr == 4 & (wp < 0.10 | wp > 0.90))   # garbage time
    ) %>%
    mutate(
      game_script = case_when(
        wp > leading_threshold  ~ "leading",
        wp < trailing_threshold ~ "trailing",
        TRUE                    ~ "neutral"
      )
    )

  message(glue(
    "Game script splits: {nrow(pbp_clean)} clean plays ",
    "({plays_raw - nrow(pbp_clean)} removed: kneels/spikes/garbage time/missing WP)"
  ))

  # --- Aggregate helper: one position group ---
  agg_script <- function(df, id_col, name_col, pos_label) {
    df %>%
      filter(!is.na({{ id_col }}), !is.na({{ name_col }})) %>%
      group_by(season, player_id = {{ id_col }}, player_name = {{ name_col }},
               team = posteam, game_script) %>%
      summarise(
        n_plays   = n(),
        sum_epa   = sum(epa, na.rm = TRUE),
        n_success = sum(epa > 0, na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      mutate(position_group = pos_label)
  }

  passers   <- agg_script(pbp_clean %>% filter(play_type == "pass"),
                          passer_player_id,   passer_player_name,   "passer")
  receivers <- agg_script(pbp_clean %>% filter(play_type == "pass"),
                          receiver_player_id, receiver_player_name, "receiver")
  rushers   <- agg_script(pbp_clean %>% filter(play_type == "run", qb_scramble == 0),
                          rusher_player_id,   rusher_player_name,   "rusher")

  long_stats <- bind_rows(passers, receivers, rushers)

  # Most frequent team per player-season
  player_team <- long_stats %>%
    group_by(season, player_id, team) %>%
    summarise(team_plays = sum(n_plays), .groups = "drop") %>%
    group_by(season, player_id) %>%
    slice_max(team_plays, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(season, player_id, team)

  # Total plays per player-position (denominator for shares)
  player_totals <- long_stats %>%
    group_by(season, player_id, player_name, position_group) %>%
    summarise(total_plays = sum(n_plays), .groups = "drop") %>%
    filter(total_plays >= min_plays)

  # Per-script aggregation
  script_agg <- long_stats %>%
    group_by(season, player_id, position_group, game_script) %>%
    summarise(
      n_plays   = sum(n_plays),
      sum_epa   = sum(sum_epa),
      n_success = sum(n_success),
      .groups   = "drop"
    ) %>%
    mutate(
      epa_per_play = ifelse(n_plays > 0, sum_epa   / n_plays, NA_real_),
      success_rate = ifelse(n_plays > 0, n_success / n_plays, NA_real_)
    )

  # Pivot to wide using names_glue to produce deterministic column names directly.
  # names_glue = "{.value}_{game_script}" produces e.g. n_plays_neutral, epa_per_play_leading.
  # values_fill guarantees all three script columns always exist even when a player
  # has zero plays in a given script category -- this prevents "object not found"
  # errors in the downstream mutate when test data or sparse real data lacks one script.
  script_wide <- script_agg %>%
    select(season, player_id, position_group, game_script,
           n_plays, epa_per_play, success_rate) %>%
    pivot_wider(
      id_cols     = c(season, player_id, position_group),
      names_from  = game_script,
      values_from = c(n_plays, epa_per_play, success_rate),
      names_glue  = "{.value}_{game_script}",
      values_fill = list(n_plays = 0L, epa_per_play = NA_real_, success_rate = NA_real_)
    )

  # Guarantee all nine expected columns exist regardless of which scripts appear in data
  for (col in c("n_plays_neutral",     "n_plays_leading",     "n_plays_trailing",
                "epa_per_play_neutral","epa_per_play_leading","epa_per_play_trailing",
                "success_rate_neutral","success_rate_leading","success_rate_trailing")) {
    if (!col %in% names(script_wide)) {
      script_wide[[col]] <- if (startsWith(col, "n_plays")) 0L else NA_real_
    }
  }

  script_final <- player_totals %>%
    left_join(script_wide, by = c("season", "player_id", "position_group")) %>%
    left_join(player_team, by = c("season", "player_id")) %>%
    mutate(
      neutral_plays  = coalesce(n_plays_neutral,  0L),
      leading_plays  = coalesce(n_plays_leading,  0L),
      trailing_plays = coalesce(n_plays_trailing, 0L),
      neutral_epa_per_play  = epa_per_play_neutral,
      leading_epa_per_play  = epa_per_play_leading,
      trailing_epa_per_play = epa_per_play_trailing,
      neutral_success_rate  = success_rate_neutral,
      leading_success_rate  = success_rate_leading,
      trailing_success_rate = success_rate_trailing,
      neutral_share  = ifelse(total_plays > 0, neutral_plays  / total_plays, NA_real_),
      leading_share  = ifelse(total_plays > 0, leading_plays  / total_plays, NA_real_),
      trailing_share = ifelse(total_plays > 0, trailing_plays / total_plays, NA_real_),
      script_epa_delta = ifelse(
        !is.na(neutral_epa_per_play) & !is.na(trailing_epa_per_play),
        neutral_epa_per_play - trailing_epa_per_play,
        NA_real_
      )
    ) %>%
    select(
      season, player_id, player_name, position_group, team, total_plays,
      neutral_plays, leading_plays, trailing_plays,
      neutral_epa_per_play, leading_epa_per_play, trailing_epa_per_play,
      neutral_success_rate, leading_success_rate, trailing_success_rate,
      neutral_share, leading_share, trailing_share,
      script_epa_delta
    ) %>%
    arrange(desc(total_plays))

  message(glue(
    "Game script splits complete: {n_distinct(script_final$player_id)} players ",
    "(leading: >{leading_threshold} WP, neutral: {trailing_threshold}-{leading_threshold}, ",
    "trailing: <{trailing_threshold})"
  ))
  return(script_final)
}


# ==============================================================================
# SECTION 2: LEVERAGE FEATURES
# ==============================================================================

#' Calculate Win Probability Added and Leverage Metrics
#'
#' @description
#' Computes per-player leverage metrics using Win Probability Added (WPA).
#' WPA measures how much each play changed the team's win probability.
#' High-leverage plays are those where the absolute WPA was large -- these
#' separate clutch performers from padding stats in low-stakes situations.
#'
#' Clutch rate = proportion of high-leverage plays with positive WPA.
#' This is experimental with small-sample caveats; most players see only
#' 5-15 truly high-leverage plays per season.
#'
#' @param pbp_data Play-by-play tibble from load_and_validate_pbp().
#'   Must contain all columns from get_game_script_splits() plus: wpa
#' @param season Optional numeric scalar or vector. Default: NULL.
#' @param week_min Numeric. Default: 1.
#' @param week_max Numeric. Default: 22.
#' @param leverage_threshold Minimum absolute WPA for "high leverage".
#'   Default: 0.05 (5 pct point swing).
#' @param min_plays Minimum total plays per player-season. Default: 10.
#'
#' @return Tibble with one row per player-position_group-season. Columns:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Player name (chr)}
#'     \item{position_group}{"passer", "rusher", or "receiver" (chr)}
#'     \item{team}{Most frequent team (chr)}
#'     \item{total_plays}{Total clean plays (int)}
#'     \item{total_wpa}{Sum of WPA across all plays (dbl)}
#'     \item{mean_wpa_per_play}{Average WPA per play (dbl)}
#'     \item{high_leverage_plays}{Plays meeting leverage threshold (int)}
#'     \item{high_leverage_rate}{high_leverage_plays / total_plays (dbl)}
#'     \item{high_leverage_wpa}{Sum of WPA on high-leverage plays (dbl)}
#'     \item{clutch_rate}{Proportion of high-leverage plays with positive WPA (dbl).
#'       EXPERIMENTAL: check clutch_note before interpreting.}
#'     \item{high_leverage_epa_per_play}{Mean EPA on high-leverage plays (dbl)}
#'     \item{clutch_note}{"small_sample" if high_leverage_plays < 10,
#'       "sufficient_sample" otherwise (chr)}
#'   }
#'
#' @details
#' WPA differs from EPA: EPA measures drive scoring probability; WPA measures
#' game-winning probability. Same play can have high EPA but low WPA (1st quarter)
#' or low EPA but high WPA (4th quarter, 1-score game).
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' leverage <- calculate_leverage_features(pbp, season = 2025)
#'
#' leverage %>%
#'   filter(position_group == "passer", total_plays >= 200) %>%
#'   arrange(desc(total_wpa)) %>%
#'   select(player_name, team, total_wpa, clutch_rate, high_leverage_plays, clutch_note)
#' }
#'
#' @seealso [get_game_script_splits()] for script-based splits,
#'   [get_comeback_profile()] for trailing-game performance
#'
#' @export
calculate_leverage_features <- function(pbp_data,
                                        season             = NULL,
                                        week_min           = 1,
                                        week_max           = 22,
                                        leverage_threshold = 0.05,
                                        min_plays          = 10) {

  if (!is.data.frame(pbp_data)) stop("pbp_data must be a data frame or tibble.")

  required_cols <- c(
    "play_type", "posteam", "season", "week", "game_id",
    "epa", "wp", "wpa", "qtr",
    "passer_player_id", "rusher_player_id", "receiver_player_id",
    "passer_player_name", "rusher_player_name", "receiver_player_name",
    "qb_kneel", "qb_spike", "qb_scramble"
  )
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }
  if (!is.null(season) && !is.numeric(season)) stop("season must be numeric or NULL.")
  if (!is.numeric(week_min) || !is.numeric(week_max)) stop("week_min and week_max must be numeric.")
  if (week_min > week_max) stop(glue("week_min ({week_min}) cannot exceed week_max ({week_max})."))
  if (!is.numeric(leverage_threshold) || leverage_threshold <= 0 || leverage_threshold >= 1) {
    stop("leverage_threshold must be between 0 and 1.")
  }
  if (!is.numeric(min_plays) || min_plays < 1) stop("min_plays must be a positive integer.")
  if (nrow(pbp_data) == 0) {
    message("pbp_data is empty. Returning empty leverage tibble.")
    return(tibble())
  }

  if (!is.null(season)) pbp_data <- pbp_data %>% filter(season %in% !!season)
  pbp_data <- pbp_data %>% filter(week >= week_min, week <= week_max)

  pbp_clean <- pbp_data %>%
    filter(
      !is.na(posteam), !is.na(epa), !is.na(wp), !is.na(wpa),
      play_type %in% c("pass", "run"),
      qb_kneel == 0, qb_spike == 0,
      !(qtr == 4 & (wp < 0.10 | wp > 0.90))
    ) %>%
    mutate(is_high_leverage = abs(wpa) >= leverage_threshold)

  message(glue(
    "Leverage features: {nrow(pbp_clean)} clean plays, ",
    "{sum(pbp_clean$is_high_leverage, na.rm = TRUE)} high-leverage (|WPA| >= {leverage_threshold})"
  ))

  agg_leverage <- function(df, id_col, name_col, pos_label) {
    df %>%
      filter(!is.na({{ id_col }}), !is.na({{ name_col }})) %>%
      group_by(season, player_id = {{ id_col }}, player_name = {{ name_col }},
               team = posteam) %>%
      summarise(
        total_plays          = n(),
        total_wpa            = sum(wpa, na.rm = TRUE),
        high_leverage_plays  = sum(is_high_leverage, na.rm = TRUE),
        high_leverage_wpa    = sum(wpa  * is_high_leverage, na.rm = TRUE),
        high_leverage_epa    = sum(epa  * is_high_leverage, na.rm = TRUE),
        high_leverage_pos_wpa = sum(is_high_leverage & wpa > 0, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(position_group = pos_label)
  }

  passers   <- agg_leverage(pbp_clean %>% filter(play_type == "pass"),
                            passer_player_id, passer_player_name, "passer")
  receivers <- agg_leverage(pbp_clean %>% filter(play_type == "pass"),
                            receiver_player_id, receiver_player_name, "receiver")
  rushers   <- agg_leverage(pbp_clean %>% filter(play_type == "run", qb_scramble == 0),
                            rusher_player_id, rusher_player_name, "rusher")

  result <- bind_rows(passers, receivers, rushers) %>%
    group_by(season, player_id, player_name, position_group) %>%
    summarise(
      team                 = first(team),
      total_plays          = sum(total_plays),
      total_wpa            = sum(total_wpa),
      high_leverage_plays  = sum(high_leverage_plays),
      high_leverage_wpa    = sum(high_leverage_wpa),
      high_leverage_epa    = sum(high_leverage_epa),
      high_leverage_pos_wpa = sum(high_leverage_pos_wpa),
      .groups = "drop"
    ) %>%
    filter(total_plays >= min_plays) %>%
    mutate(
      mean_wpa_per_play          = ifelse(total_plays > 0, total_wpa / total_plays, NA_real_),
      high_leverage_rate         = ifelse(total_plays > 0, high_leverage_plays / total_plays, NA_real_),
      high_leverage_epa_per_play = ifelse(high_leverage_plays > 0,
                                          high_leverage_epa / high_leverage_plays, NA_real_),
      clutch_rate                = ifelse(high_leverage_plays > 0,
                                          high_leverage_pos_wpa / high_leverage_plays, NA_real_),
      clutch_note                = ifelse(high_leverage_plays < 10, "small_sample", "sufficient_sample")
    ) %>%
    select(
      season, player_id, player_name, position_group, team,
      total_plays, total_wpa, mean_wpa_per_play,
      high_leverage_plays, high_leverage_rate, high_leverage_wpa,
      clutch_rate, high_leverage_epa_per_play, clutch_note
    ) %>%
    arrange(desc(total_wpa))

  n_sufficient <- sum(result$clutch_note == "sufficient_sample", na.rm = TRUE)
  message(glue(
    "Leverage features complete: {n_distinct(result$player_id)} players, ",
    "{n_sufficient} with sufficient sample for clutch_rate (>= 10 high-leverage plays)"
  ))
  return(result)
}


# ==============================================================================
# SECTION 3: COMEBACK PROFILE
# ==============================================================================

#' Get Player Performance When Trailing (Comeback Profile)
#'
#' @description
#' Measures player efficiency when the team is trailing by a meaningful
#' deficit (default: 8+ points). Separates players who produce in structured
#' offenses from those who thrive only in garbage-time scripts.
#'
#' For fantasy: trailing-script WR production often reflects garbage-time
#' volume that will not repeat. For scouting: QBs who maintain efficiency
#' while trailing demonstrate true arm talent over scheme.
#'
#' Only competitive deficits are counted (WP 0.10-0.40). True garbage time
#' (WP < 0.10) is excluded even inside deficit situations.
#'
#' @param pbp_data Play-by-play tibble from load_and_validate_pbp().
#'   Must contain all columns from get_game_script_splits() plus:
#'   score_differential
#' @param season Optional numeric scalar or vector. Default: NULL.
#' @param week_min Numeric. Default: 1.
#' @param week_max Numeric. Default: 22.
#' @param deficit_threshold Minimum score deficit (points) for comeback.
#'   Default: 8. Use 14 for "significant deficit" analysis.
#' @param min_plays Minimum comeback plays to include player. Default: 5.
#'   Lower than other functions because trailing-game volume is fewer by design.
#'
#' @return Tibble with one row per player-position_group-season. Columns:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Player name (chr)}
#'     \item{position_group}{"passer", "rusher", or "receiver" (chr)}
#'     \item{team}{Most frequent team (chr)}
#'     \item{comeback_plays}{Plays trailing by >= threshold, non-garbage-time (int)}
#'     \item{comeback_epa_per_play}{Mean EPA in comeback situations (dbl)}
#'     \item{comeback_success_rate}{EPA > 0 rate in comeback situations (dbl)}
#'     \item{overall_epa_per_play}{Mean EPA across all clean plays for context (dbl)}
#'     \item{comeback_vs_overall_epa}{comeback minus overall EPA per play.
#'       Positive = player performs above their average when trailing. (dbl)}
#'     \item{sample_note}{"small_sample" if comeback_plays < 10 (chr)}
#'   }
#'
#' @details
#' score_differential in nflfastR is from the perspective of the possessing
#' team. Negative = possessing team trailing. A deficit of 8 means
#' score_differential <= -8.
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' comeback <- get_comeback_profile(pbp, season = 2025)
#'
#' comeback %>%
#'   filter(position_group == "passer", comeback_plays >= 30) %>%
#'   arrange(desc(comeback_vs_overall_epa)) %>%
#'   select(player_name, team, comeback_plays, comeback_epa_per_play,
#'          overall_epa_per_play, comeback_vs_overall_epa, sample_note)
#' }
#'
#' @seealso [get_game_script_splits()] for full leading/neutral/trailing splits,
#'   [calculate_leverage_features()] for WPA-based clutch metrics
#'
#' @export
get_comeback_profile <- function(pbp_data,
                                 season            = NULL,
                                 week_min          = 1,
                                 week_max          = 22,
                                 deficit_threshold = 8,
                                 min_plays         = 5) {

  if (!is.data.frame(pbp_data)) stop("pbp_data must be a data frame or tibble.")
  required_cols <- c(
    "play_type", "posteam", "season", "week", "game_id",
    "epa", "wp", "score_differential", "qtr",
    "passer_player_id", "rusher_player_id", "receiver_player_id",
    "passer_player_name", "rusher_player_name", "receiver_player_name",
    "qb_kneel", "qb_spike", "qb_scramble"
  )
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }
  if (!is.null(season) && !is.numeric(season)) stop("season must be numeric or NULL.")
  if (!is.numeric(week_min) || !is.numeric(week_max)) stop("week_min and week_max must be numeric.")
  if (week_min > week_max) stop(glue("week_min ({week_min}) cannot exceed week_max ({week_max})."))
  if (!is.numeric(deficit_threshold) || deficit_threshold < 1) stop("deficit_threshold must be >= 1.")
  if (!is.numeric(min_plays) || min_plays < 1) stop("min_plays must be a positive integer.")
  if (nrow(pbp_data) == 0) {
    message("pbp_data is empty. Returning empty comeback tibble.")
    return(tibble())
  }

  if (!is.null(season)) pbp_data <- pbp_data %>% filter(season %in% !!season)
  pbp_data <- pbp_data %>% filter(week >= week_min, week <= week_max)

  # Base: all plays (no garbage time by definition -- exclude WP < 0.10 in Q4)
  pbp_base <- pbp_data %>%
    filter(
      !is.na(posteam), !is.na(epa), !is.na(wp),
      play_type %in% c("pass", "run"),
      qb_kneel == 0, qb_spike == 0,
      !(qtr == 4 & wp < 0.10)
    )

  # Comeback: trailing by deficit_threshold, but still competitive (wp >= 0.10)
  pbp_comeback <- pbp_base %>%
    filter(
      !is.na(score_differential),
      score_differential <= -deficit_threshold,
      wp >= 0.10
    )

  message(glue(
    "Comeback profile: {nrow(pbp_comeback)} plays trailing by >= {deficit_threshold} pts ",
    "(non-garbage-time) out of {nrow(pbp_base)} total clean plays"
  ))

  agg_comeback <- function(df_base, df_cmb, id_col, name_col, pos_label) {
    overall <- df_base %>%
      filter(!is.na({{ id_col }}), !is.na({{ name_col }})) %>%
      group_by(season, player_id = {{ id_col }}, player_name = {{ name_col }},
               team = posteam) %>%
      summarise(
        overall_plays   = n(),
        overall_sum_epa = sum(epa, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(position_group = pos_label)

    cmb <- df_cmb %>%
      filter(!is.na({{ id_col }})) %>%
      group_by(season, player_id = {{ id_col }}) %>%
      summarise(
        comeback_plays   = n(),
        comeback_sum_epa = sum(epa, na.rm = TRUE),
        comeback_success = sum(epa > 0, na.rm = TRUE),
        .groups = "drop"
      )

    overall %>%
      left_join(cmb, by = c("season", "player_id")) %>%
      mutate(
        comeback_plays   = coalesce(comeback_plays,   0L),
        comeback_sum_epa = coalesce(comeback_sum_epa, 0.0),
        comeback_success = coalesce(comeback_success, 0L)
      )
  }

  pass_base <- pbp_base     %>% filter(play_type == "pass")
  pass_cmb  <- pbp_comeback %>% filter(play_type == "pass")
  run_base  <- pbp_base     %>% filter(play_type == "run", qb_scramble == 0)
  run_cmb   <- pbp_comeback %>% filter(play_type == "run", qb_scramble == 0)

  passers   <- agg_comeback(pass_base, pass_cmb, passer_player_id,   passer_player_name,   "passer")
  receivers <- agg_comeback(pass_base, pass_cmb, receiver_player_id, receiver_player_name, "receiver")
  rushers   <- agg_comeback(run_base,  run_cmb,  rusher_player_id,   rusher_player_name,   "rusher")

  result <- bind_rows(passers, receivers, rushers) %>%
    group_by(season, player_id, player_name, position_group) %>%
    summarise(
      team             = first(team),
      overall_plays    = sum(overall_plays),
      overall_sum_epa  = sum(overall_sum_epa),
      comeback_plays   = sum(comeback_plays),
      comeback_sum_epa = sum(comeback_sum_epa),
      comeback_success = sum(comeback_success),
      .groups = "drop"
    ) %>%
    filter(comeback_plays >= min_plays) %>%
    mutate(
      comeback_epa_per_play   = ifelse(comeback_plays > 0,
                                       comeback_sum_epa / comeback_plays, NA_real_),
      comeback_success_rate   = ifelse(comeback_plays > 0,
                                       comeback_success / comeback_plays, NA_real_),
      overall_epa_per_play    = ifelse(overall_plays  > 0,
                                       overall_sum_epa  / overall_plays,  NA_real_),
      comeback_vs_overall_epa = ifelse(
        !is.na(comeback_epa_per_play) & !is.na(overall_epa_per_play),
        comeback_epa_per_play - overall_epa_per_play, NA_real_
      ),
      sample_note = ifelse(comeback_plays < 10, "small_sample", "sufficient_sample")
    ) %>%
    select(
      season, player_id, player_name, position_group, team,
      comeback_plays, comeback_epa_per_play, comeback_success_rate,
      overall_epa_per_play, comeback_vs_overall_epa, sample_note
    ) %>%
    arrange(desc(comeback_plays))

  n_sufficient <- sum(result$sample_note == "sufficient_sample", na.rm = TRUE)
  message(glue(
    "Comeback profile complete: {nrow(result)} players, ",
    "{n_sufficient} with sufficient sample (>= 10 comeback plays)"
  ))
  return(result)
}


# ==============================================================================
# SECTION 4: SCRIPT-ADJUSTED EPA
# ==============================================================================

#' Calculate Script-Adjusted EPA per Play
#'
#' @description
#' Adjusts raw EPA per play for game script bias using league-average EPA
#' baselines per script category. Raw EPA is contaminated by game state:
#' trailing offenses call obvious passes (often negative EPA), leading offenses
#' call clock-killing runs (often negative EPA). Neutral-script EPA is the
#' cleanest measure of true offensive efficiency.
#'
#' Adjustment method (additive):
#'   script_adjusted_epa = raw_epa_per_play
#'     + leading_share  * (league_neutral_epa - league_leading_epa)
#'     + trailing_share * (league_neutral_epa - league_trailing_epa)
#'
#' This is an experimental metric. Always compare to raw_epa_per_play and
#' document adjustment size when including in reports.
#'
#' @param pbp_data Play-by-play tibble from load_and_validate_pbp().
#'   Must contain all columns from get_game_script_splits().
#' @param season Optional numeric scalar or vector. Default: NULL.
#' @param week_min Numeric. Default: 1.
#' @param week_max Numeric. Default: 22.
#' @param leading_threshold Win probability for "leading". Default: 0.60.
#' @param trailing_threshold Win probability for "trailing". Default: 0.40.
#' @param min_plays Minimum plays per player-season. Default: 20.
#'
#' @return Tibble with one row per player-position_group-season. Columns:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Player name (chr)}
#'     \item{position_group}{"passer", "rusher", or "receiver" (chr)}
#'     \item{team}{Most frequent team (chr)}
#'     \item{total_plays}{Total plays included (int)}
#'     \item{raw_epa_per_play}{Unadjusted EPA per play (dbl)}
#'     \item{neutral_share}{Proportion of plays in neutral script (dbl)}
#'     \item{leading_share}{Proportion of plays when leading (dbl)}
#'     \item{trailing_share}{Proportion of plays when trailing (dbl)}
#'     \item{script_adjusted_epa}{EPA corrected for game script bias (dbl).
#'       EXPERIMENTAL: validate before using in production models.}
#'     \item{adjustment_applied}{script_adjusted_epa minus raw_epa_per_play.
#'       Positive = player penalized for favorable script exposure. (dbl)}
#'   }
#'
#' @details
#' League baselines are computed from the same pbp_data passed in. For
#' multi-season analyses, baselines pool all seasons together.
#'
#' Large adjustments (> 0.10 EPA/play) indicate extreme game script exposure
#' and should be flagged. The function emits a message if any player exceeds
#' this threshold.
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' adjusted <- calculate_script_adjusted_epa(pbp, season = 2025)
#'
#' # Players most penalized by script adjustment (played in favorable scripts)
#' adjusted %>%
#'   filter(position_group == "rusher", total_plays >= 50) %>%
#'   arrange(desc(adjustment_applied)) %>%
#'   select(player_name, team, raw_epa_per_play, script_adjusted_epa,
#'          adjustment_applied, leading_share)
#' }
#'
#' @seealso [get_game_script_splits()] for raw script splits,
#'   [get_comeback_profile()] for trailing-game detail
#'
#' @export
calculate_script_adjusted_epa <- function(pbp_data,
                                           season             = NULL,
                                           week_min           = 1,
                                           week_max           = 22,
                                           leading_threshold  = 0.60,
                                           trailing_threshold = 0.40,
                                           min_plays          = 20) {

  if (!is.data.frame(pbp_data)) stop("pbp_data must be a data frame or tibble.")
  required_cols <- c(
    "play_type", "posteam", "season", "week", "game_id", "epa", "wp", "qtr",
    "passer_player_id", "rusher_player_id", "receiver_player_id",
    "passer_player_name", "rusher_player_name", "receiver_player_name",
    "qb_kneel", "qb_spike", "qb_scramble"
  )
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }
  if (!is.null(season) && !is.numeric(season)) stop("season must be numeric or NULL.")
  if (!is.numeric(week_min) || !is.numeric(week_max)) stop("week_min and week_max must be numeric.")
  if (week_min > week_max) stop(glue("week_min ({week_min}) cannot exceed week_max ({week_max})."))
  if (!is.numeric(leading_threshold) || leading_threshold <= 0.5 || leading_threshold >= 1) {
    stop("leading_threshold must be between 0.5 and 1.0 (exclusive).")
  }
  if (!is.numeric(trailing_threshold) || trailing_threshold <= 0 || trailing_threshold >= 0.5) {
    stop("trailing_threshold must be between 0.0 and 0.5 (exclusive).")
  }
  if (!is.numeric(min_plays) || min_plays < 1) stop("min_plays must be a positive integer.")
  if (nrow(pbp_data) == 0) {
    message("pbp_data is empty. Returning empty script-adjusted EPA tibble.")
    return(tibble())
  }

  if (!is.null(season)) pbp_data <- pbp_data %>% filter(season %in% !!season)
  pbp_data <- pbp_data %>% filter(week >= week_min, week <= week_max)

  pbp_clean <- pbp_data %>%
    filter(
      !is.na(posteam), !is.na(epa), !is.na(wp),
      play_type %in% c("pass", "run"),
      qb_kneel == 0, qb_spike == 0,
      !(qtr == 4 & (wp < 0.10 | wp > 0.90))
    ) %>%
    mutate(
      game_script = case_when(
        wp > leading_threshold  ~ "leading",
        wp < trailing_threshold ~ "trailing",
        TRUE                    ~ "neutral"
      )
    )

  # League baselines per script
  baselines <- pbp_clean %>%
    group_by(game_script) %>%
    summarise(league_epa = mean(epa, na.rm = TRUE), .groups = "drop")

  get_baseline <- function(script_val) {
    val <- baselines %>% filter(game_script == script_val) %>% pull(league_epa)
    # If a script category is absent from the data (e.g. no neutral plays in a
    # small controlled dataset), fall back to the mean of available baselines.
    # Returning NA_real_ here propagates NA through every downstream calculation,
    # making adjustment_applied all-NA -- analytically wrong. A player with zero
    # plays in a script should receive zero adjustment for that script, not NA.
    if (length(val) == 0) mean(baselines$league_epa, na.rm = TRUE) else val
  }
  b_neutral  <- get_baseline("neutral")
  b_leading  <- get_baseline("leading")
  b_trailing <- get_baseline("trailing")

  message(glue(
    "League EPA baselines -- Neutral: {round(b_neutral, 4)}, ",
    "Leading: {round(b_leading, 4)}, Trailing: {round(b_trailing, 4)}"
  ))

  agg_adj <- function(df, id_col, name_col, pos_label) {
    df %>%
      filter(!is.na({{ id_col }}), !is.na({{ name_col }})) %>%
      group_by(season, player_id = {{ id_col }}, player_name = {{ name_col }},
               team = posteam, game_script) %>%
      summarise(n_plays = n(), sum_epa = sum(epa, na.rm = TRUE), .groups = "drop") %>%
      mutate(position_group = pos_label)
  }

  passers   <- agg_adj(pbp_clean %>% filter(play_type == "pass"),
                       passer_player_id, passer_player_name, "passer")
  receivers <- agg_adj(pbp_clean %>% filter(play_type == "pass"),
                       receiver_player_id, receiver_player_name, "receiver")
  rushers   <- agg_adj(pbp_clean %>% filter(play_type == "run", qb_scramble == 0),
                       rusher_player_id, rusher_player_name, "rusher")

  long_stats <- bind_rows(passers, receivers, rushers)

  player_team <- long_stats %>%
    group_by(season, player_id, team) %>%
    summarise(team_plays = sum(n_plays), .groups = "drop") %>%
    group_by(season, player_id) %>%
    slice_max(team_plays, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(season, player_id, team)

  # Totals -- include player_name via first() so it survives to the final select.
  # Without this, player_name is dropped after the group_by aggregation and the
  # downstream select("player_name", ...) fails with "column doesn't exist".
  totals <- long_stats %>%
    group_by(season, player_id, position_group) %>%
    summarise(
      player_name = first(player_name),
      total_plays = sum(n_plays),
      total_epa   = sum(sum_epa),
      .groups     = "drop"
    ) %>%
    filter(total_plays >= min_plays)

  # Script-level plays
  script_plays <- long_stats %>%
    group_by(season, player_id, position_group, game_script) %>%
    summarise(n_plays = sum(n_plays), .groups = "drop") %>%
    pivot_wider(
      id_cols     = c(season, player_id, position_group),
      names_from  = game_script,
      values_from = n_plays,
      values_fill = list(n_plays = 0L)
    )

  # Ensure all three script columns exist
  for (col in c("neutral", "leading", "trailing")) {
    if (!col %in% names(script_plays)) script_plays[[col]] <- 0L
  }

  result <- totals %>%
    left_join(script_plays, by = c("season", "player_id", "position_group")) %>%
    left_join(player_team,  by = c("season", "player_id")) %>%
    mutate(
      neutral_plays  = coalesce(neutral,  0L),
      leading_plays  = coalesce(leading,  0L),
      trailing_plays = coalesce(trailing, 0L),
      raw_epa_per_play = ifelse(total_plays > 0, total_epa / total_plays, NA_real_),
      neutral_share  = ifelse(total_plays > 0, neutral_plays  / total_plays, NA_real_),
      leading_share  = ifelse(total_plays > 0, leading_plays  / total_plays, NA_real_),
      trailing_share = ifelse(total_plays > 0, trailing_plays / total_plays, NA_real_),
      leading_adj    = coalesce(leading_share,  0.0) * (b_neutral - b_leading),
      trailing_adj   = coalesce(trailing_share, 0.0) * (b_neutral - b_trailing),
      script_adjusted_epa = ifelse(!is.na(raw_epa_per_play),
                                   raw_epa_per_play + leading_adj + trailing_adj, NA_real_),
      adjustment_applied  = ifelse(!is.na(script_adjusted_epa) & !is.na(raw_epa_per_play),
                                   script_adjusted_epa - raw_epa_per_play, NA_real_)
    ) %>%
    select(
      season, player_id, player_name, position_group, team,
      total_plays, raw_epa_per_play,
      neutral_share, leading_share, trailing_share,
      script_adjusted_epa, adjustment_applied
    ) %>%
    arrange(desc(total_plays))

  large_adj <- sum(abs(result$adjustment_applied) > 0.10, na.rm = TRUE)
  if (large_adj > 0) {
    message(glue(
      "Note: {large_adj} players have |adjustment_applied| > 0.10 EPA/play. ",
      "Interpret cautiously -- extreme game script exposure."
    ))
  }
  message(glue(
    "Script-adjusted EPA complete: {nrow(result)} players, ",
    "mean adjustment = {round(mean(result$adjustment_applied, na.rm = TRUE), 4)}"
  ))
  return(result)
}
