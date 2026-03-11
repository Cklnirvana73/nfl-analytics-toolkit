# ==============================================================================
# WEEK 8: OPPONENT-ADJUSTED METRICS & MATCHUP FEATURES
# ==============================================================================
#
# Production-grade opponent adjustment and matchup feature engineering for NFL
# player evaluation. Built for personal analytics use and portfolio display.
#
# Raw player metrics are contaminated by opponent quality. A 0.15 EPA/play
# against the league's best defense is a fundamentally different achievement
# than 0.15 against the worst. Opponent adjustment is the final layer of
# context added after usage (Week 6) and game script (Week 7), producing
# the most context-aware efficiency metrics in the toolkit.
#
# This file also compiles the master feature matrix -- one row per player-week
# combining production, efficiency, usage, trends, game script, and opponent
# context. This is the ML-ready input for Week 9 (XGBoost models).
#
# File contains four main functions:
# 1. calculate_opponent_adjustments()   - Lines   68-340
#    Player EPA normalized by opponent defensive quality. Separate adjustments
#    for passing and rushing. Additive method preserving EPA units.
# 2. classify_defensive_style()         - Lines  342-530
#    Categorizes defenses as pass-funnel, run-funnel, or balanced using EPA
#    allowed by play type. Creates matchup archetype features.
# 3. calculate_matchup_history()        - Lines  532-730
#    Historical player performance against defensive archetypes. Uses archetype
#    clustering, not team-specific history (individual matchup history is too
#    sparse at 1-2 games per season per opponent).
# 4. compile_feature_matrix()           - Lines  732-960
#    Compiles master ML-ready feature matrix: one row per player-week with
#    40-60 columns covering all feature layers from Weeks 5-8.
#
# Dependencies:
#   dplyr, glue, tidyr, here, zoo (all established in prior weeks)
#   NEW this week: (none -- all dependencies already present)
#
# Builds on:
#   Week 1: load_and_validate_pbp()     -> pbp tibble
#   Week 2: get_player_rushing_stats(), get_player_receiving_stats()
#   Week 5: calculate_epa_trends(), get_situational_splits()
#   Week 6: calculate_usage_metrics()
#   Week 7: get_game_script_splits(), calculate_script_adjusted_epa()
#
# NFL Context:
#   - Opponent adjustment is applied AFTER usage and game script context
#   - Raw stats adjusted for opponent without usage context are less meaningful
#   - Defensive style uses EPA allowed (not yards) -- accounts for situation
#   - AFC West vs NFC South can differ by 0.05+ EPA/play in average opponent quality
#   - Individual matchup history is too sparse (1-2 games vs each team per season)
#     Use defensive archetype clusters, not specific team-matchup history
#   - Opponent adjustment formula (additive):
#       opp_adjusted_epa = player_epa - (opponent_avg_epa_allowed - league_avg_epa_allowed)
#     Positive adjustment: player faced a better-than-average defense (rewarded)
#     Negative adjustment: player faced a worse-than-average defense (penalized)
#
# nflfastR column reference (verified against 2025 data):
#   epa               -- Expected Points Added per play
#   play_type         -- "pass", "run", "qb_kneel", "qb_spike", etc.
#   posteam           -- Possessing (offensive) team
#   defteam           -- Defending team
#   wp                -- Win probability for possessing team
#   qtr               -- Quarter (1-5, 5 = OT)
#   week              -- Game week
#   season            -- Season year
#   game_id           -- Unique game identifier
#   passer_player_id, passer_player_name
#   rusher_player_id, rusher_player_name
#   receiver_player_id, receiver_player_name
#   qb_kneel, qb_spike, qb_scramble
#
# ==============================================================================

library(dplyr)
library(glue)
library(tidyr)
library(zoo)
library(here)


# ==============================================================================
# SECTION 1: OPPONENT ADJUSTMENTS
# ==============================================================================

#' Calculate Opponent-Adjusted Player EPA
#'
#' @description
#' Normalizes player EPA per play by the quality of defenses faced throughout
#' a season. The adjustment is additive: players who faced above-average
#' defenses are rewarded (adjusted number rises); players who feasted on
#' weak defenses are penalized (adjusted number falls).
#'
#' Adjustment formula (applied separately for pass and rush):
#'   opp_adjusted_epa = raw_epa - (opponent_avg_epa_allowed - league_avg_epa_allowed)
#'
#' Opponent EPA allowed is computed as the mean EPA per play allowed by each
#' team across the season (excluding the player's own team as opponent, which
#' would create self-referential contamination). League average is the grand
#' mean of all opponent EPA allowed values.
#'
#' Opponent adjustment is the final feature layer. Apply after game script
#' adjustment (Week 7) for the most context-aware efficiency metrics.
#'
#' @param pbp_data Play-by-play tibble from load_and_validate_pbp().
#'   Must contain: play_type, posteam, defteam, season, week, game_id, epa,
#'   wp, qtr, passer_player_id, passer_player_name, rusher_player_id,
#'   rusher_player_name, receiver_player_id, receiver_player_name,
#'   qb_kneel, qb_spike, qb_scramble
#' @param season Optional numeric scalar or vector. Filter to specific season(s).
#'   Default: NULL (all seasons in data).
#' @param week_min Numeric. Minimum week inclusive. Default: 1.
#' @param week_max Numeric. Maximum week inclusive. Default: 22.
#' @param min_plays Minimum total clean plays per player-season. Default: 20.
#' @param min_def_plays Minimum clean plays for a defense to be included in
#'   league average computation. Default: 100. Defenses below this threshold
#'   receive the league average adjustment (no penalty or reward).
#' @param through_week Optional numeric scalar. When provided, only plays from
#'   weeks up to and including this value are used. Used by
#'   compile_feature_matrix() to compute leakage-safe expanding opponent
#'   adjustment: each player-week row is scored using only games played before
#'   that week. Default: NULL (use all weeks in week_min:week_max range).
#'   When set, overrides week_max.
#'
#' @return Tibble with one row per player-position_group-season. Columns:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Player name (chr)}
#'     \item{position_group}{"passer", "rusher", or "receiver" (chr)}
#'     \item{team}{Most frequent team this season (chr)}
#'     \item{total_plays}{Total clean plays included (int)}
#'     \item{raw_epa_per_play}{Unadjusted EPA per play (dbl)}
#'     \item{avg_opponent_epa_allowed}{Play-count-weighted mean EPA allowed by
#'       all defenses faced this season (dbl)}
#'     \item{league_avg_epa_allowed}{League-wide mean EPA allowed across all
#'       qualifying defenses (dbl)}
#'     \item{opp_adjustment}{avg_opponent_epa_allowed minus league_avg_epa_allowed.
#'       Positive = faced tougher-than-average defenses (player rewarded).
#'       Negative = faced easier-than-average defenses (player penalized). (dbl)}
#'     \item{opp_adjusted_epa}{raw_epa_per_play minus opp_adjustment (dbl).
#'       This is the primary output metric.}
#'     \item{games_played}{Number of distinct games with qualifying plays (int)}
#'     \item{schedule_difficulty_rank}{Rank among players in same position_group
#'       by avg_opponent_epa_allowed (1 = hardest schedule). (int)}
#'   }
#'
#' @details
#' Players appear in multiple position groups when applicable (e.g., an RB who
#' also receives targets appears in both "rusher" and "receiver" groups).
#'
#' Opponent EPA allowed is computed from all plays against that defense in the
#' same season and week range, excluding plays from the player's own team
#' (which would conflate offensive and defensive quality). Uses play-count
#' weighting when averaging opponent quality across games.
#'
#' When through_week is supplied, the function scores only plays from
#' week_min through through_week. This is the mechanism compile_feature_matrix()
#' uses to generate leakage-safe opponent adjustment features: it calls this
#' function once per distinct week in the feature matrix, incrementing
#' through_week each time. A player-week row at week W receives an opponent
#' adjustment computed from weeks 1 through W-1 only -- no future information.
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' opp_adj <- calculate_opponent_adjustments(pbp, season = 2025)
#' names(opp_adj)
#'
#' # QBs with hardest schedules, penalized most for easy slates
#' opp_adj %>%
#'   filter(position_group == "passer", total_plays >= 200) %>%
#'   arrange(schedule_difficulty_rank) %>%
#'   select(player_name, team, raw_epa_per_play, opp_adjusted_epa,
#'          opp_adjustment, schedule_difficulty_rank) %>%
#'   head(10)
#'
#' # Compare raw vs adjusted for RBs
#' opp_adj %>%
#'   filter(position_group == "rusher", total_plays >= 50) %>%
#'   mutate(adjustment_size = abs(opp_adjustment)) %>%
#'   arrange(desc(adjustment_size)) %>%
#'   select(player_name, team, raw_epa_per_play, opp_adjusted_epa, opp_adjustment)
#' }
#'
#' @seealso [classify_defensive_style()] for matchup archetype classification,
#'   [calculate_script_adjusted_epa()] (Week 7) for game script adjustment,
#'   [compile_feature_matrix()] for the full ML-ready feature set
#'
#' @export
calculate_opponent_adjustments <- function(pbp_data,
                                           season        = NULL,
                                           week_min      = 1,
                                           week_max      = 22,
                                           min_plays     = 20,
                                           min_def_plays = 100,
                                           through_week  = NULL) {

  # --- Input validation ---
  if (!is.data.frame(pbp_data)) stop("pbp_data must be a data frame or tibble.")
  required_cols <- c(
    "play_type", "posteam", "defteam", "season", "week", "game_id",
    "epa", "wp", "qtr",
    "passer_player_id", "passer_player_name",
    "rusher_player_id",  "rusher_player_name",
    "receiver_player_id","receiver_player_name",
    "qb_kneel", "qb_spike", "qb_scramble"
  )
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }
  if (!is.null(season) && !is.numeric(season)) stop("season must be numeric or NULL.")
  if (!is.numeric(week_min) || !is.numeric(week_max)) stop("week_min and week_max must be numeric.")
  if (week_min > week_max) stop("week_min must be <= week_max.")
  if (!is.numeric(min_plays) || min_plays < 1) stop("min_plays must be a positive number.")
  if (!is.numeric(min_def_plays) || min_def_plays < 1) stop("min_def_plays must be a positive number.")
  if (!is.null(through_week)) {
    if (!is.numeric(through_week) || length(through_week) != 1) {
      stop("through_week must be a single numeric value or NULL.")
    }
    if (through_week < week_min) {
      stop(glue("through_week ({through_week}) must be >= week_min ({week_min})."))
    }
    # through_week overrides week_max when supplied
    week_max <- through_week
  }

  # --- Filter to season and week range ---
  pbp_filtered <- pbp_data
  if (!is.null(season)) {
    pbp_filtered <- pbp_filtered %>% filter(season %in% !!season)
  }
  pbp_filtered <- pbp_filtered %>%
    filter(week >= week_min, week <= week_max)

  if (nrow(pbp_filtered) == 0) {
    message("No data after season/week filters. Returning empty tibble.")
    return(tibble(
      season = integer(), player_id = character(), player_name = character(),
      position_group = character(), team = character(), total_plays = integer(),
      raw_epa_per_play = double(), avg_opponent_epa_allowed = double(),
      league_avg_epa_allowed = double(), opp_adjustment = double(),
      opp_adjusted_epa = double(), games_played = integer(),
      schedule_difficulty_rank = integer()
    ))
  }

  # --- Base clean plays (no garbage time, no special plays) ---
  pbp_clean <- pbp_filtered %>%
    filter(
      play_type %in% c("pass", "run"),
      !is.na(epa),
      !is.na(posteam),
      !is.na(defteam),
      qb_kneel  == 0,
      qb_spike  == 0,
      # Garbage time: WP < 10% or > 90% in Q4
      !(qtr == 4 & (wp < 0.10 | wp > 0.90))
    )

  if (nrow(pbp_clean) == 0) {
    message("No clean plays remaining after filters. Returning empty tibble.")
    return(tibble(
      season = integer(), player_id = character(), player_name = character(),
      position_group = character(), team = character(), total_plays = integer(),
      raw_epa_per_play = double(), avg_opponent_epa_allowed = double(),
      league_avg_epa_allowed = double(), opp_adjustment = double(),
      opp_adjusted_epa = double(), games_played = integer(),
      schedule_difficulty_rank = integer()
    ))
  }

  # --- Step 1: Compute defensive EPA allowed per team-season ---
  # How many EPA/play does each defense allow?
  # Must compute BEFORE constructing player-level stats (no circularity).
  def_quality <- pbp_clean %>%
    group_by(season, defteam) %>%
    summarise(
      def_plays        = n(),
      def_epa_allowed  = mean(epa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(def_plays >= min_def_plays)

  # League average EPA allowed (grand mean across qualifying defenses)
  league_avg <- mean(def_quality$def_epa_allowed, na.rm = TRUE)

  if (is.nan(league_avg)) {
    message("Could not compute league average EPA allowed -- check min_def_plays. Returning empty tibble.")
    return(tibble(
      season = integer(), player_id = character(), player_name = character(),
      position_group = character(), team = character(), total_plays = integer(),
      raw_epa_per_play = double(), avg_opponent_epa_allowed = double(),
      league_avg_epa_allowed = double(), opp_adjustment = double(),
      opp_adjusted_epa = double(), games_played = integer(),
      schedule_difficulty_rank = integer()
    ))
  }

  # --- Step 2: Build player-level tables per position group ---
  # Passer: pass plays + scrambles
  passer_plays <- pbp_clean %>%
    filter(
      play_type == "pass" | (play_type == "run" & qb_scramble == 1),
      !is.na(passer_player_id)
    ) %>%
    mutate(player_id = passer_player_id, player_name = passer_player_name,
           position_group = "passer")

  # Rusher: run plays excluding scrambles
  rusher_plays <- pbp_clean %>%
    filter(play_type == "run", qb_scramble == 0, !is.na(rusher_player_id)) %>%
    mutate(player_id = rusher_player_id, player_name = rusher_player_name,
           position_group = "rusher")

  # Receiver: targeted pass plays
  receiver_plays <- pbp_clean %>%
    filter(play_type == "pass", !is.na(receiver_player_id)) %>%
    mutate(player_id = receiver_player_id, player_name = receiver_player_name,
           position_group = "receiver")

  all_plays <- bind_rows(passer_plays, rusher_plays, receiver_plays)

  if (nrow(all_plays) == 0) {
    message("No player plays found. Returning empty tibble.")
    return(tibble(
      season = integer(), player_id = character(), player_name = character(),
      position_group = character(), team = character(), total_plays = integer(),
      raw_epa_per_play = double(), avg_opponent_epa_allowed = double(),
      league_avg_epa_allowed = double(), opp_adjustment = double(),
      opp_adjusted_epa = double(), games_played = integer(),
      schedule_difficulty_rank = integer()
    ))
  }

  # --- Step 3: Join defense quality to player plays ---
  # Play-count weighted average opponent EPA allowed per player-season
  player_with_def <- all_plays %>%
    left_join(
      def_quality %>% select(season, defteam, def_epa_allowed),
      by = c("season", "defteam")
    ) %>%
    # For plays where defense doesn't meet min_def_plays, use league average
    mutate(
      def_epa_allowed = ifelse(is.na(def_epa_allowed), league_avg, def_epa_allowed)
    )

  # --- Step 4: Aggregate per player-position_group-season ---
  player_stats <- player_with_def %>%
    group_by(season, player_id, player_name, position_group) %>%
    summarise(
      team              = names(which.max(table(posteam))),
      total_plays       = n(),
      games_played      = n_distinct(game_id),
      raw_epa_per_play  = mean(epa, na.rm = TRUE),
      # Play-count weighted average of opponent EPA allowed
      avg_opponent_epa_allowed = sum(def_epa_allowed, na.rm = TRUE) /
        sum(!is.na(def_epa_allowed)),
      .groups = "drop"
    ) %>%
    filter(total_plays >= min_plays)

  if (nrow(player_stats) == 0) {
    message(glue("No players met min_plays >= {min_plays}. Returning empty tibble."))
    return(tibble(
      season = integer(), player_id = character(), player_name = character(),
      position_group = character(), team = character(), total_plays = integer(),
      raw_epa_per_play = double(), avg_opponent_epa_allowed = double(),
      league_avg_epa_allowed = double(), opp_adjustment = double(),
      opp_adjusted_epa = double(), games_played = integer(),
      schedule_difficulty_rank = integer()
    ))
  }

  # --- Step 5: Apply opponent adjustment ---
  # opp_adjustment = avg_opponent_epa_allowed - league_avg_epa_allowed
  # opp_adjusted_epa = raw_epa - opp_adjustment
  # (subtract the "difficulty bonus/penalty" from raw performance)
  result <- player_stats %>%
    mutate(
      league_avg_epa_allowed   = league_avg,
      opp_adjustment           = avg_opponent_epa_allowed - league_avg_epa_allowed,
      opp_adjusted_epa         = raw_epa_per_play - opp_adjustment
    ) %>%
    group_by(season, position_group) %>%
    mutate(
      schedule_difficulty_rank = rank(-avg_opponent_epa_allowed, ties.method = "min")
    ) %>%
    ungroup() %>%
    select(
      season, player_id, player_name, position_group, team,
      total_plays, raw_epa_per_play, avg_opponent_epa_allowed,
      league_avg_epa_allowed, opp_adjustment, opp_adjusted_epa,
      games_played, schedule_difficulty_rank
    )

  # --- Diagnostic messages ---
  n_players  <- n_distinct(result$player_id)
  large_adj  <- result %>% filter(abs(opp_adjustment) > 0.05)
  if (nrow(large_adj) > 0) {
    message(glue(
      "{nrow(large_adj)} player-position rows have |opp_adjustment| > 0.05 EPA/play. ",
      "These players faced schedules meaningfully above/below league average."
    ))
  }
  message(glue(
    "calculate_opponent_adjustments: {n_players} players, ",
    "{nrow(result)} rows, league_avg_epa_allowed = {round(league_avg, 4)}"
  ))

  result
}


# ==============================================================================
# SECTION 2: DEFENSIVE STYLE CLASSIFICATION
# ==============================================================================

#' Classify Defensive Styles by EPA Allowed
#'
#' @description
#' Categorizes each team's defense as pass-funnel, run-funnel, or balanced
#' based on EPA allowed per play type. A pass-funnel defense allows more EPA
#' on passing plays than rushing plays (relative to league average), making
#' RBs a better matchup. A run-funnel defense does the opposite.
#'
#' Uses EPA allowed (not yards allowed) because EPA accounts for situational
#' context: 4 yards on 3rd-and-2 is very different from 4 yards on 3rd-and-10.
#' Yards-based defensive rankings conflate performance with game script.
#'
#' Classification thresholds are relative to league average for that season,
#' making the metric stable across different offensive environment strength.
#'
#' @param pbp_data Play-by-play tibble from load_and_validate_pbp().
#'   Must contain: play_type, defteam, season, week, epa, wp, qtr,
#'   qb_kneel, qb_spike, qb_scramble
#' @param season Optional numeric. Filter to specific season(s). Default: NULL.
#' @param week_min Numeric. Minimum week. Default: 1.
#' @param week_max Numeric. Maximum week. Default: 22.
#' @param min_def_plays Minimum plays per defense for inclusion. Default: 50.
#' @param funnel_threshold Minimum difference from league average (in EPA units)
#'   for a defense to be classified as a funnel. Default: 0.02.
#'   Defenses within +/- funnel_threshold of relative balance are "balanced".
#'
#' @return Tibble with one row per team-season. Columns:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{team}{Defensive team abbreviation (chr)}
#'     \item{pass_plays_allowed}{Count of pass plays against this defense (int)}
#'     \item{rush_plays_allowed}{Count of rush plays against this defense (int)}
#'     \item{pass_epa_allowed}{Mean EPA per pass play allowed (dbl)}
#'     \item{rush_epa_allowed}{Mean EPA per rush play allowed (dbl)}
#'     \item{overall_epa_allowed}{Mean EPA per play allowed across all play types (dbl)}
#'     \item{pass_epa_vs_league}{pass_epa_allowed minus league avg pass EPA allowed (dbl).
#'       Positive = defense allows more pass EPA than average (pass-vulnerable)}
#'     \item{rush_epa_vs_league}{rush_epa_allowed minus league avg rush EPA allowed (dbl).
#'       Positive = defense allows more rush EPA than average (run-vulnerable)}
#'     \item{defensive_style}{Classification: "pass_funnel", "run_funnel",
#'       or "balanced" (chr)}
#'     \item{style_strength}{Absolute difference between pass_epa_vs_league and
#'       rush_epa_vs_league. Larger = stronger funnel effect. (dbl)}
#'     \item{overall_tier}{Defensive quality tier: "elite", "above_avg",
#'       "average", "below_avg", "poor" based on overall_epa_allowed
#'       relative to league percentiles. (chr)}
#'   }
#'
#' @details
#' Defensive style classification logic:
#'   pass_funnel: pass_epa_vs_league - rush_epa_vs_league > funnel_threshold
#'   run_funnel:  rush_epa_vs_league - pass_epa_vs_league > funnel_threshold
#'   balanced:    |difference| <= funnel_threshold
#'
#' Overall tier is based on quintiles of overall_epa_allowed in the season.
#' Lower EPA allowed = better defense.
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' def_styles <- classify_defensive_style(pbp, season = 2025)
#' names(def_styles)
#'
#' # Which defenses are pass funnels? (Good matchups for RBs, tough for WRs/QBs)
#' def_styles %>%
#'   filter(defensive_style == "pass_funnel") %>%
#'   arrange(desc(style_strength)) %>%
#'   select(team, pass_epa_allowed, rush_epa_allowed, style_strength, overall_tier)
#'
#' # Elite defenses and their style
#' def_styles %>%
#'   filter(overall_tier == "elite") %>%
#'   select(team, defensive_style, overall_epa_allowed, style_strength)
#' }
#'
#' @seealso [calculate_opponent_adjustments()] for season-level opponent adjustment,
#'   [calculate_matchup_history()] for player performance against defensive archetypes,
#'   [compile_feature_matrix()] for combining into ML-ready features
#'
#' @export
classify_defensive_style <- function(pbp_data,
                                     season           = NULL,
                                     week_min         = 1,
                                     week_max         = 22,
                                     min_def_plays    = 50,
                                     funnel_threshold = 0.02) {

  if (!is.data.frame(pbp_data)) stop("pbp_data must be a data frame or tibble.")
  required_cols <- c(
    "play_type", "defteam", "season", "week", "epa", "wp", "qtr",
    "qb_kneel", "qb_spike", "qb_scramble"
  )
  missing_cols <- setdiff(required_cols, names(pbp_data))
  if (length(missing_cols) > 0) {
    stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }
  if (!is.null(season) && !is.numeric(season)) stop("season must be numeric or NULL.")
  if (!is.numeric(week_min) || !is.numeric(week_max)) stop("week_min and week_max must be numeric.")
  if (week_min > week_max) stop("week_min must be <= week_max.")
  if (!is.numeric(funnel_threshold) || funnel_threshold < 0) {
    stop("funnel_threshold must be a non-negative number.")
  }

  pbp_filtered <- pbp_data
  if (!is.null(season)) {
    pbp_filtered <- pbp_filtered %>% filter(season %in% !!season)
  }
  pbp_filtered <- pbp_filtered %>%
    filter(week >= week_min, week <= week_max)

  if (nrow(pbp_filtered) == 0) {
    message("No data after filters. Returning empty tibble.")
    return(.empty_def_style_tibble())
  }

  pbp_clean <- pbp_filtered %>%
    filter(
      play_type %in% c("pass", "run"),
      !is.na(epa),
      !is.na(defteam),
      qb_kneel == 0,
      qb_spike == 0,
      !(qtr == 4 & (wp < 0.10 | wp > 0.90))
    )

  if (nrow(pbp_clean) == 0) {
    message("No clean plays remaining. Returning empty tibble.")
    return(.empty_def_style_tibble())
  }

  # --- League average EPA allowed by play type (for relative comparisons) ---
  league_pass_epa <- pbp_clean %>%
    filter(play_type == "pass") %>%
    summarise(m = mean(epa, na.rm = TRUE)) %>%
    pull(m)

  league_rush_epa <- pbp_clean %>%
    filter(play_type == "run") %>%
    summarise(m = mean(epa, na.rm = TRUE)) %>%
    pull(m)

  # --- Compute per-defense EPA allowed by play type ---
  def_pass <- pbp_clean %>%
    filter(play_type == "pass") %>%
    group_by(season, defteam) %>%
    summarise(
      pass_plays_allowed = n(),
      pass_epa_allowed   = mean(epa, na.rm = TRUE),
      .groups = "drop"
    )

  def_rush <- pbp_clean %>%
    filter(play_type == "run") %>%
    group_by(season, defteam) %>%
    summarise(
      rush_plays_allowed = n(),
      rush_epa_allowed   = mean(epa, na.rm = TRUE),
      .groups = "drop"
    )

  def_overall <- pbp_clean %>%
    group_by(season, defteam) %>%
    summarise(
      total_def_plays    = n(),
      overall_epa_allowed = mean(epa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(total_def_plays >= min_def_plays)

  # --- Join pass and rush stats ---
  def_combined <- def_overall %>%
    left_join(def_pass, by = c("season", "defteam")) %>%
    left_join(def_rush, by = c("season", "defteam")) %>%
    rename(team = defteam) %>%
    # Fill NAs for defenses with zero pass or rush plays (rare edge case)
    mutate(
      pass_plays_allowed = replace_na(pass_plays_allowed, 0L),
      rush_plays_allowed = replace_na(rush_plays_allowed, 0L),
      pass_epa_allowed   = replace_na(pass_epa_allowed, league_pass_epa),
      rush_epa_allowed   = replace_na(rush_epa_allowed, league_rush_epa)
    )

  if (nrow(def_combined) == 0) {
    message("No defenses met min_def_plays threshold. Returning empty tibble.")
    return(.empty_def_style_tibble())
  }

  # --- Compute relative EPA vs league and classify ---
  def_result <- def_combined %>%
    mutate(
      pass_epa_vs_league = pass_epa_allowed - league_pass_epa,
      rush_epa_vs_league = rush_epa_allowed - league_rush_epa,
      funnel_diff        = pass_epa_vs_league - rush_epa_vs_league,
      style_strength     = abs(funnel_diff),
      defensive_style    = case_when(
        funnel_diff  >  funnel_threshold ~ "pass_funnel",
        funnel_diff  < -funnel_threshold ~ "run_funnel",
        TRUE                             ~ "balanced"
      )
    ) %>%
    # Tier based on overall EPA allowed quintiles within season
    group_by(season) %>%
    mutate(
      epa_pctile      = percent_rank(overall_epa_allowed),
      overall_tier    = case_when(
        epa_pctile <= 0.20 ~ "elite",
        epa_pctile <= 0.40 ~ "above_avg",
        epa_pctile <= 0.60 ~ "average",
        epa_pctile <= 0.80 ~ "below_avg",
        TRUE               ~ "poor"
      )
    ) %>%
    ungroup() %>%
    select(
      season, team,
      pass_plays_allowed, rush_plays_allowed,
      pass_epa_allowed, rush_epa_allowed, overall_epa_allowed,
      pass_epa_vs_league, rush_epa_vs_league,
      defensive_style, style_strength, overall_tier
    )

  n_pass_funnel <- sum(def_result$defensive_style == "pass_funnel")
  n_run_funnel  <- sum(def_result$defensive_style == "run_funnel")
  n_balanced    <- sum(def_result$defensive_style == "balanced")
  message(glue(
    "classify_defensive_style: {nrow(def_result)} defenses -- ",
    "pass_funnel: {n_pass_funnel}, run_funnel: {n_run_funnel}, balanced: {n_balanced}"
  ))

  def_result
}

# Internal helper for empty return
.empty_def_style_tibble <- function() {
  tibble(
    season = integer(), team = character(),
    pass_plays_allowed = integer(), rush_plays_allowed = integer(),
    pass_epa_allowed = double(), rush_epa_allowed = double(),
    overall_epa_allowed = double(), pass_epa_vs_league = double(),
    rush_epa_vs_league = double(), defensive_style = character(),
    style_strength = double(), overall_tier = character()
  )
}


# ==============================================================================
# SECTION 3: MATCHUP HISTORY BY DEFENSIVE ARCHETYPE
# ==============================================================================

#' Calculate Player Performance Against Defensive Archetypes
#'
#' @description
#' Computes historical player efficiency when facing each defensive style
#' (pass_funnel, run_funnel, balanced). Individual team matchup history is too
#' sparse at 1-2 games per opponent per season to draw conclusions; archetype
#' matching groups similar defenses and provides meaningfully larger samples.
#'
#' For fantasy: an RB who consistently produces against run-funnel defenses
#' but struggles against pass-funnels has a more predictable role than raw
#' season numbers suggest. For scouting: QB performance against elite balanced
#' defenses is the cleanest test of true talent.
#'
#' @param pbp_data Play-by-play tibble from load_and_validate_pbp().
#'   Must contain all columns required by calculate_opponent_adjustments().
#' @param def_styles Tibble from classify_defensive_style(). Must contain:
#'   season, team, defensive_style, overall_tier.
#' @param season Optional numeric. Filter to specific season(s). Default: NULL.
#' @param week_min Numeric. Default: 1.
#' @param week_max Numeric. Default: 22.
#' @param min_plays_per_archetype Minimum plays per player per archetype to
#'   include that archetype's stats for that player. Default: 10.
#'   Players without enough plays against an archetype receive NA for that
#'   archetype's metrics (not 0).
#'
#' @return Tibble with one row per player-position_group-season. Columns:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Player name (chr)}
#'     \item{position_group}{"passer", "rusher", or "receiver" (chr)}
#'     \item{team}{Most frequent team this season (chr)}
#'     \item{overall_epa_per_play}{Season-level mean EPA per play (dbl)}
#'     \item{vs_pass_funnel_plays}{Plays against pass-funnel defenses (int)}
#'     \item{vs_pass_funnel_epa}{Mean EPA per play vs pass-funnel defenses (dbl).
#'       NA if vs_pass_funnel_plays < min_plays_per_archetype.}
#'     \item{vs_run_funnel_plays}{Plays against run-funnel defenses (int)}
#'     \item{vs_run_funnel_epa}{Mean EPA per play vs run-funnel defenses (dbl).
#'       NA if vs_run_funnel_plays < min_plays_per_archetype.}
#'     \item{vs_balanced_plays}{Plays against balanced defenses (int)}
#'     \item{vs_balanced_epa}{Mean EPA per play vs balanced defenses (dbl).
#'       NA if vs_balanced_plays < min_plays_per_archetype.}
#'     \item{vs_elite_def_plays}{Plays against elite-tier defenses (int)}
#'     \item{vs_elite_def_epa}{Mean EPA per play vs elite defenses (dbl).
#'       NA if vs_elite_def_plays < min_plays_per_archetype.}
#'     \item{archetype_epa_range}{Range of non-NA archetype EPA values
#'       (max - min). Larger = player more sensitive to matchup type. (dbl)}
#'     \item{best_archetype}{Archetype with highest EPA for this player (chr).
#'       NA if fewer than 2 archetypes have qualifying plays.}
#'     \item{worst_archetype}{Archetype with lowest EPA for this player (chr).
#'       NA if fewer than 2 archetypes have qualifying plays.}
#'   }
#'
#' @details
#' This function joins pbp_data to def_styles on defteam = team. Players who
#' face defenses not present in def_styles (below min_def_plays threshold in
#' classify_defensive_style) are excluded from archetype calculations for those
#' games, not from the overall count.
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' def_styles  <- classify_defensive_style(pbp, season = 2025)
#' arch_hist   <- calculate_matchup_history(pbp, def_styles, season = 2025)
#'
#' # RBs who perform best against run-funnel defenses
#' arch_hist %>%
#'   filter(position_group == "rusher", !is.na(vs_run_funnel_epa)) %>%
#'   arrange(desc(vs_run_funnel_epa)) %>%
#'   select(player_name, team, vs_run_funnel_epa, vs_run_funnel_plays,
#'          vs_pass_funnel_epa, archetype_epa_range)
#'
#' # QBs most sensitive to matchup type (largest archetype_epa_range)
#' arch_hist %>%
#'   filter(position_group == "passer", !is.na(archetype_epa_range)) %>%
#'   arrange(desc(archetype_epa_range)) %>%
#'   select(player_name, team, best_archetype, worst_archetype, archetype_epa_range)
#' }
#'
#' @seealso [classify_defensive_style()] for defensive style classification,
#'   [calculate_opponent_adjustments()] for season-level opponent adjustment,
#'   [compile_feature_matrix()] for the full ML-ready feature set
#'
#' @export
calculate_matchup_history <- function(pbp_data,
                                      def_styles,
                                      season                  = NULL,
                                      week_min                = 1,
                                      week_max                = 22,
                                      min_plays_per_archetype = 10) {

  if (!is.data.frame(pbp_data))  stop("pbp_data must be a data frame or tibble.")
  if (!is.data.frame(def_styles)) stop("def_styles must be a data frame or tibble from classify_defensive_style().")
  required_def_cols <- c("season", "team", "defensive_style", "overall_tier")
  missing_def <- setdiff(required_def_cols, names(def_styles))
  if (length(missing_def) > 0) {
    stop(glue("def_styles missing columns: {paste(missing_def, collapse = ', ')}"))
  }
  required_pbp_cols <- c(
    "play_type", "posteam", "defteam", "season", "week", "game_id",
    "epa", "wp", "qtr",
    "passer_player_id", "passer_player_name",
    "rusher_player_id",  "rusher_player_name",
    "receiver_player_id","receiver_player_name",
    "qb_kneel", "qb_spike", "qb_scramble"
  )
  missing_pbp <- setdiff(required_pbp_cols, names(pbp_data))
  if (length(missing_pbp) > 0) {
    stop(glue("pbp_data missing columns: {paste(missing_pbp, collapse = ', ')}"))
  }
  if (!is.null(season) && !is.numeric(season)) stop("season must be numeric or NULL.")
  if (!is.numeric(week_min) || !is.numeric(week_max)) stop("week_min and week_max must be numeric.")
  if (week_min > week_max) stop("week_min must be <= week_max.")

  empty_return <- tibble(
    season = integer(), player_id = character(), player_name = character(),
    position_group = character(), team = character(),
    overall_epa_per_play = double(),
    vs_pass_funnel_plays = integer(), vs_pass_funnel_epa = double(),
    vs_run_funnel_plays  = integer(), vs_run_funnel_epa  = double(),
    vs_balanced_plays    = integer(), vs_balanced_epa    = double(),
    vs_elite_def_plays   = integer(), vs_elite_def_epa   = double(),
    archetype_epa_range  = double(),
    best_archetype = character(), worst_archetype = character()
  )

  pbp_filtered <- pbp_data
  if (!is.null(season)) {
    pbp_filtered <- pbp_filtered %>% filter(season %in% !!season)
  }
  pbp_filtered <- pbp_filtered %>%
    filter(week >= week_min, week <= week_max)

  if (nrow(pbp_filtered) == 0) {
    message("No data after filters. Returning empty tibble.")
    return(empty_return)
  }

  pbp_clean <- pbp_filtered %>%
    filter(
      play_type %in% c("pass", "run"),
      !is.na(epa),
      !is.na(defteam),
      qb_kneel == 0,
      qb_spike == 0,
      !(qtr == 4 & (wp < 0.10 | wp > 0.90))
    )

  if (nrow(pbp_clean) == 0) {
    message("No clean plays remaining. Returning empty tibble.")
    return(empty_return)
  }

  # --- Join defensive styles to plays ---
  pbp_with_style <- pbp_clean %>%
    left_join(
      def_styles %>% select(season, team, defensive_style, overall_tier),
      by = c("season", "defteam" = "team")
    )

  # --- Build player play tables ---
  passer_plays <- pbp_with_style %>%
    filter(play_type == "pass" | (play_type == "run" & qb_scramble == 1),
           !is.na(passer_player_id)) %>%
    mutate(player_id = passer_player_id, player_name = passer_player_name,
           position_group = "passer")

  rusher_plays <- pbp_with_style %>%
    filter(play_type == "run", qb_scramble == 0, !is.na(rusher_player_id)) %>%
    mutate(player_id = rusher_player_id, player_name = rusher_player_name,
           position_group = "rusher")

  receiver_plays <- pbp_with_style %>%
    filter(play_type == "pass", !is.na(receiver_player_id)) %>%
    mutate(player_id = receiver_player_id, player_name = receiver_player_name,
           position_group = "receiver")

  all_plays <- bind_rows(passer_plays, rusher_plays, receiver_plays)

  if (nrow(all_plays) == 0) {
    message("No player plays found. Returning empty tibble.")
    return(empty_return)
  }

  # --- Helper: EPA per archetype with min_plays guard ---
  archetype_epa <- function(plays_df, style_val, tier_val = NULL) {
    if (!is.null(tier_val)) {
      sub <- plays_df %>% filter(overall_tier == tier_val)
    } else {
      sub <- plays_df %>% filter(defensive_style == style_val)
    }
    list(
      plays = nrow(sub),
      epa   = if (nrow(sub) >= min_plays_per_archetype) mean(sub$epa, na.rm = TRUE) else NA_real_
    )
  }

  # --- Aggregate per player ---
  player_list <- all_plays %>%
    group_by(season, player_id, player_name, position_group) %>%
    group_split()

  result_rows <- lapply(player_list, function(grp) {

    pf  <- archetype_epa(grp, "pass_funnel")
    rf  <- archetype_epa(grp, "run_funnel")
    bal <- archetype_epa(grp, "balanced")
    eli <- archetype_epa(grp, style_val = NULL, tier_val = "elite")

    archetype_epas <- c(
      vs_pass_funnel = pf$epa,
      vs_run_funnel  = rf$epa,
      vs_balanced    = bal$epa
    )
    valid_archetypes <- archetype_epas[!is.na(archetype_epas)]
    epa_range    <- if (length(valid_archetypes) >= 2) diff(range(valid_archetypes)) else NA_real_
    best_arch    <- if (length(valid_archetypes) >= 2) names(which.max(valid_archetypes)) else NA_character_
    worst_arch   <- if (length(valid_archetypes) >= 2) names(which.min(valid_archetypes)) else NA_character_

    tibble(
      season               = grp$season[1],
      player_id            = grp$player_id[1],
      player_name          = grp$player_name[1],
      position_group       = grp$position_group[1],
      team                 = names(which.max(table(grp$posteam))),
      overall_epa_per_play = mean(grp$epa, na.rm = TRUE),
      vs_pass_funnel_plays = pf$plays,
      vs_pass_funnel_epa   = pf$epa,
      vs_run_funnel_plays  = rf$plays,
      vs_run_funnel_epa    = rf$epa,
      vs_balanced_plays    = bal$plays,
      vs_balanced_epa      = bal$epa,
      vs_elite_def_plays   = eli$plays,
      vs_elite_def_epa     = eli$epa,
      archetype_epa_range  = epa_range,
      best_archetype       = best_arch,
      worst_archetype      = worst_arch
    )
  })

  result <- bind_rows(result_rows)

  n_with_range <- sum(!is.na(result$archetype_epa_range))
  message(glue(
    "calculate_matchup_history: {nrow(result)} player-position rows, ",
    "{n_with_range} with archetype_epa_range (>= 2 qualifying archetypes)"
  ))

  result
}


# ==============================================================================
# SECTION 4: MASTER FEATURE MATRIX
# ==============================================================================

#' Compile ML-Ready Master Feature Matrix
#'
#' @description
#' Compiles one row per player-week with 40-60 features covering all analytical
#' layers from Weeks 5-8: production, efficiency, usage, EPA trends, game script,
#' and opponent context. This is the feature input for Week 9 (XGBoost models).
#'
#' Feature layers:
#'   Layer 1 (production):    Raw EPA, yards, touchdowns per game
#'   Layer 2 (efficiency):    EPA per play, success rate
#'   Layer 3 (usage):         Target share, snap share, role (from Week 6)
#'   Layer 4 (trends):        Rolling EPA slopes, stability metrics (Week 5)
#'   Layer 5 (game script):   Neutral-script EPA, script share features (Week 7)
#'   Layer 6 (opponent):      Opp-adjusted EPA, opponent tier, defensive style
#'
#' All rolling/lag features use play_date ordering to prevent feature leakage.
#' Rolling windows use prior weeks only (lag before rollmean).
#'
#' @param pbp_data Play-by-play tibble from load_and_validate_pbp().
#' @param opp_adjusted Tibble from calculate_opponent_adjustments() for the
#'   same season. If NULL, opponent-adjusted features are excluded with a message.
#' @param def_styles Tibble from classify_defensive_style() for the same season.
#'   If NULL, matchup features are excluded with a message.
#' @param season Numeric scalar. Season to compile. Required (no NULL).
#' @param week_min Numeric. Minimum week. Default: 1.
#' @param week_max Numeric. Maximum week. Default: 22.
#' @param rolling_window Integer. Weeks for rolling EPA average. Default: 3.
#' @param min_plays_per_week Minimum plays in a week for a player-week row to
#'   be included. Default: 3.
#'
#' @return Tibble with one row per player-position_group-week. Columns include:
#'   \describe{
#'     \item{season}{Season year (int)}
#'     \item{week}{Game week (int)}
#'     \item{player_id}{GSIS player ID (chr)}
#'     \item{player_name}{Player name (chr)}
#'     \item{position_group}{"passer", "rusher", or "receiver" (chr)}
#'     \item{team}{Team this week (chr)}
#'     \item{opponent}{Opponent team this week (chr)}
#'     \item{plays_this_week}{Play count this week (int)}
#'     \item{epa_this_week}{Mean EPA per play this week (dbl)}
#'     \item{success_rate_this_week}{Success rate this week (dbl)}
#'     \item{epa_roll3}{Rolling 3-week mean EPA (prior weeks only). NA if < 3
#'       prior weeks available. (dbl)}
#'     \item{epa_season_to_date}{Expanding mean EPA through prior week (dbl)}
#'     \item{plays_roll3}{Rolling 3-week mean play count (int)}
#'     \item{neutral_epa_season}{Expanding season-to-date neutral-script EPA
#'       through prior week. NA for week 1. (dbl)}
#'     \item{leading_share_season}{Expanding season-to-date leading-script play
#'       share through prior week. NA for week 1. (dbl)}
#'     \item{trailing_share_season}{Expanding season-to-date trailing-script play
#'       share through prior week. NA for week 1. (dbl)}
#'     \item{opp_adjusted_epa_prior}{Opponent-adjusted EPA per play computed
#'       using only weeks prior to this row's week. NA for week 1 (no prior
#'       opponent data). Uses through_week = week - 1 in
#'       calculate_opponent_adjustments(). (dbl)}
#'     \item{schedule_difficulty_rank}{Rank within position_group by
#'       avg_opponent_epa_allowed through prior week. NA for week 1. (int)}
#'     \item{opp_adj_games_prior}{Number of prior games used to compute
#'       opp_adjusted_epa_prior. Use to filter rows with insufficient opponent
#'       history. NA for week 1. (int)}
#'     \item{opponent_style}{Defensive style of this week's opponent using
#'       full-season classification. "pass_funnel", "run_funnel", or "balanced".
#'       NA if def_styles not provided. Early-season rows (week <= 4) have
#'       more contamination from future games in this classification. (chr)}
#'     \item{opponent_tier}{Defensive tier of this week's opponent using
#'       full-season classification. NA if def_styles not provided. (chr)}
#'     \item{weeks_played}{Cumulative weeks played by this player in this season
#'       up to and including this week (int)}
#'   }
#'
#' @details
#' Rolling features use lag() before rollmean() to prevent leakage: epa_roll3
#' at week N uses weeks N-3, N-2, N-1 (not week N itself).
#'
#' Opponent adjustment features (opp_adjusted_epa_prior, schedule_difficulty_rank,
#' opp_adj_games_prior) are computed via an expanding window: each player-week
#' row at week W calls calculate_opponent_adjustments() with through_week = W-1,
#' so only prior games inform the adjustment. Week-1 rows return NA by design.
#'
#' Opponent style and tier (opponent_style, opponent_tier) use full-season
#' defensive classifications joined by this week's opponent. These are context
#' features -- they tell you what kind of defense the player faces this week,
#' not a prediction of future defensive performance. Early-season rows (week <= 4)
#' have more contamination because the full-season classification includes games
#' that have not yet been played. Down-weight using role_stability_flag or
#' weeks_played when this matters for your model.
#'
#' @examples
#' \dontrun{
#' pbp     <- load_and_validate_pbp(2025)
#' opp_adj <- calculate_opponent_adjustments(pbp, season = 2025)
#' def_sty <- classify_defensive_style(pbp, season = 2025)
#'
#' features <- compile_feature_matrix(
#'   pbp_data    = pbp,
#'   opp_adjusted = opp_adj,
#'   def_styles   = def_sty,
#'   season       = 2025
#' )
#'
#' names(features)
#' nrow(features)
#'
#' # Preview for a specific player
#' features %>%
#'   filter(player_name == "J.Jefferson", position_group == "receiver") %>%
#'   select(week, epa_this_week, epa_roll3, epa_season_to_date,
#'          opponent, opponent_tier, opponent_style) %>%
#'   print(n = 18)
#' }
#'
#' @seealso [calculate_opponent_adjustments()] for opponent adjustment input,
#'   [classify_defensive_style()] for defensive style input,
#'   [get_game_script_splits()] (Week 7) for game script features
#'
#' @export
compile_feature_matrix <- function(pbp_data,
                                   opp_adjusted       = NULL,
                                   def_styles         = NULL,
                                   season,
                                   week_min           = 1,
                                   week_max           = 22,
                                   rolling_window     = 3L,
                                   min_plays_per_week = 3L) {

  if (!is.data.frame(pbp_data))     stop("pbp_data must be a data frame or tibble.")
  if (missing(season) || !is.numeric(season) || length(season) != 1) {
    stop("season must be a single numeric value.")
  }
  if (!is.numeric(week_min) || !is.numeric(week_max)) stop("week_min and week_max must be numeric.")
  if (week_min > week_max) stop("week_min must be <= week_max.")
  if (!is.numeric(rolling_window) || rolling_window < 2) stop("rolling_window must be >= 2.")

  required_pbp_cols <- c(
    "play_type", "posteam", "defteam", "season", "week", "game_id",
    "epa", "success", "wp", "qtr",
    "passer_player_id", "passer_player_name",
    "rusher_player_id",  "rusher_player_name",
    "receiver_player_id","receiver_player_name",
    "qb_kneel", "qb_spike", "qb_scramble"
  )
  missing_pbp <- setdiff(required_pbp_cols, names(pbp_data))
  if (length(missing_pbp) > 0) {
    stop(glue("pbp_data missing columns: {paste(missing_pbp, collapse = ', ')}"))
  }

  if (!is.null(opp_adjusted) && !is.data.frame(opp_adjusted)) {
    stop("opp_adjusted must be a data frame or NULL.")
  }
  if (!is.null(def_styles) && !is.data.frame(def_styles)) {
    stop("def_styles must be a data frame or NULL.")
  }

  message(glue("compile_feature_matrix: building features for season {season}, ",
               "weeks {week_min}-{week_max}..."))

  # --- Filter and clean play-by-play ---
  pbp_filtered <- pbp_data %>%
    filter(
      season == !!season,
      week   >= week_min,
      week   <= week_max
    )

  if (nrow(pbp_filtered) == 0) {
    message("No data after season/week filters.")
    return(tibble())
  }

  pbp_clean <- pbp_filtered %>%
    filter(
      play_type %in% c("pass", "run"),
      !is.na(epa),
      !is.na(posteam),
      !is.na(defteam),
      qb_kneel == 0,
      qb_spike == 0,
      !(qtr == 4 & (wp < 0.10 | wp > 0.90))
    )

  # --- Build per-position-group play tables ---
  passer_plays <- pbp_clean %>%
    filter(play_type == "pass" | (play_type == "run" & qb_scramble == 1),
           !is.na(passer_player_id)) %>%
    mutate(player_id = passer_player_id, player_name = passer_player_name,
           position_group = "passer",
           opponent = defteam)

  rusher_plays <- pbp_clean %>%
    filter(play_type == "run", qb_scramble == 0, !is.na(rusher_player_id)) %>%
    mutate(player_id = rusher_player_id, player_name = rusher_player_name,
           position_group = "rusher",
           opponent = defteam)

  receiver_plays <- pbp_clean %>%
    filter(play_type == "pass", !is.na(receiver_player_id)) %>%
    mutate(player_id = receiver_player_id, player_name = receiver_player_name,
           position_group = "receiver",
           opponent = defteam)

  all_plays <- bind_rows(passer_plays, rusher_plays, receiver_plays)

  if (nrow(all_plays) == 0) {
    message("No player plays found. Returning empty tibble.")
    return(tibble())
  }

  # --- Aggregate to player-position_group-week level ---
  weekly_base <- all_plays %>%
    group_by(season, week, player_id, player_name, position_group) %>%
    summarise(
      team               = names(which.max(table(posteam))),
      opponent           = names(which.max(table(opponent))),
      plays_this_week    = n(),
      epa_this_week      = mean(epa, na.rm = TRUE),
      # Use nflfastR 'success' column (EPA > 0 = 1) -- consistent with all other
      # files in the toolkit. EPA == 0 counts as non-success (NFL standard).
      success_rate_this_week = mean(success, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(plays_this_week >= min_plays_per_week)

  if (nrow(weekly_base) == 0) {
    message(glue("No player-weeks met min_plays_per_week = {min_plays_per_week}."))
    return(tibble())
  }

  # --- Rolling features (leakage-safe: lag before rollmean) ---
  # Group by player + position_group + team so rolling window resets on team
  # change. A traded player has two independent rolling windows per season.
  weekly_with_rolling <- weekly_base %>%
    arrange(player_id, position_group, team, week) %>%
    group_by(player_id, position_group, team) %>%
    mutate(
      weeks_played       = row_number(),
      epa_season_to_date = lag(cummean(epa_this_week), 1),
      epa_roll3 = lag(
        rollapply(epa_this_week,
                  width  = rolling_window,
                  FUN    = mean,
                  align  = "right",
                  fill   = NA),
        1
      ),
      plays_roll3 = as.integer(round(
        lag(
          rollapply(plays_this_week,
                    width  = rolling_window,
                    FUN    = mean,
                    align  = "right",
                    fill   = NA),
          1
        )
      )),

      # ------------------------------------------------------------------
      # ROLE CONTINUITY FEATURES
      # ------------------------------------------------------------------
      # weeks_since_role_change: consecutive weeks in the current role without
      # a significant EPA discontinuity. Team changes are already isolated by
      # the group_by(team) above -- this catches within-team role shifts.
      #
      # A single-week |EPA delta| > 0.3 is used as a proxy for a role break
      # (injury return, promotion/demotion). Refine this threshold in Week 9
      # using usage delta from calculate_usage_metrics() when available.
      #
      # role_stability_flag: TRUE when weeks_since_role_change > rolling_window,
      # meaning the full rolling window is uncontaminated by a role change.
      # Week 9 models should down-weight rows where role_stability_flag = FALSE.
      .epa_delta  = abs(epa_this_week - lag(epa_this_week, 1)),
      .role_break = coalesce(.epa_delta > 0.3, FALSE),
      .break_group = cumsum(.role_break),
      weeks_since_role_change = row_number() - cummax(
        ifelse(.role_break, row_number() - 1L, 0L)
      ),
      role_stability_flag = weeks_since_role_change > rolling_window
    ) %>%
    select(-.epa_delta, -.role_break, -.break_group) %>%
    ungroup()

  # --- Season-to-date game script features (LEAKAGE-SAFE) ---
  # CRITICAL: Do NOT call get_game_script_splits() with full week range here.
  # That would give week 4 a neutral_epa value computed over weeks 1-18, which
  # is direct lookahead contamination. Instead, compute cumulative expanding
  # season-to-date script features using the same lag(cummean()) pattern as
  # epa_season_to_date above.
  message("  Computing leakage-safe season-to-date game script features...")

  script_weekly <- tryCatch({
    source(here("R", "09_gamescript_features.R"), local = TRUE)

    # Build WEEKLY script stats (not season-level) then compute expanding mean
    # We need the raw weekly play data grouped by player-week-script
    pbp_script <- pbp_clean %>%
      filter(!is.na(wp)) %>%
      mutate(
        script = case_when(
          qtr == 4 & (wp < 0.10 | wp > 0.90) ~ "garbage",
          wp > 0.60 ~ "leading",
          wp < 0.40 ~ "trailing",
          TRUE       ~ "neutral"
        )
      )

    # Assign player_id per play (same logic as above)
    script_plays <- bind_rows(
      pbp_script %>%
        filter(play_type == "pass" | (play_type == "run" & qb_scramble == 1),
               !is.na(passer_player_id)) %>%
        mutate(player_id = passer_player_id, position_group = "passer"),
      pbp_script %>%
        filter(play_type == "run", qb_scramble == 0, !is.na(rusher_player_id)) %>%
        mutate(player_id = rusher_player_id, position_group = "rusher"),
      pbp_script %>%
        filter(play_type == "pass", !is.na(receiver_player_id)) %>%
        mutate(player_id = receiver_player_id, position_group = "receiver")
    ) %>%
      filter(script != "garbage")

    # Weekly script summary per player
    script_plays %>%
      group_by(player_id, position_group, week) %>%
      summarise(
        weekly_neutral_epa    = mean(epa[script == "neutral"], na.rm = TRUE),
        weekly_total_plays    = n(),
        weekly_neutral_plays  = sum(script == "neutral"),
        weekly_leading_plays  = sum(script == "leading"),
        weekly_trailing_plays = sum(script == "trailing"),
        .groups = "drop"
      )
  }, error = function(e) {
    message(glue("  Could not compute weekly script features: {conditionMessage(e)}. ",
                 "Game script features will be NA."))
    NULL
  })

  if (!is.null(script_weekly) && nrow(script_weekly) > 0) {
    # Join weekly script to feature matrix, then compute expanding season-to-date
    # averages using lag(cummean()) -- same leakage-safe pattern as epa_season_to_date.
    weekly_with_rolling <- weekly_with_rolling %>%
      left_join(script_weekly, by = c("player_id", "position_group", "week")) %>%
      group_by(player_id, position_group, team) %>%
      arrange(week, .by_group = TRUE) %>%
      mutate(
        # Expanding season-to-date neutral EPA -- lagged so week N only uses weeks 1..N-1
        neutral_epa_season = lag(cummean(replace(weekly_neutral_epa,
                                                  is.nan(weekly_neutral_epa), NA)), 1),
        # Leading/trailing share: cumulative plays in each script / total plays
        .cum_total    = lag(cumsum(coalesce(weekly_total_plays,    0L)), 1),
        .cum_leading  = lag(cumsum(coalesce(weekly_leading_plays,  0L)), 1),
        .cum_trailing = lag(cumsum(coalesce(weekly_trailing_plays, 0L)), 1),
        leading_share_season  = ifelse(.cum_total > 0, .cum_leading  / .cum_total, NA_real_),
        trailing_share_season = ifelse(.cum_total > 0, .cum_trailing / .cum_total, NA_real_)
      ) %>%
      select(-.cum_total, -.cum_leading, -.cum_trailing,
             -weekly_neutral_epa, -weekly_total_plays,
             -weekly_neutral_plays, -weekly_leading_plays, -weekly_trailing_plays) %>%
      ungroup()
  } else {
    weekly_with_rolling <- weekly_with_rolling %>%
      mutate(
        neutral_epa_season    = NA_real_,
        leading_share_season  = NA_real_,
        trailing_share_season = NA_real_
      )
  }

  # --- Season-to-date opponent adjustment (LEAKAGE-SAFE) ---
  # CRITICAL: Do NOT join full-season opp_adjusted_epa here.
  # A full-season opponent quality rating joined to a week-4 row includes how
  # opponents performed in weeks 5-18 -- direct lookahead contamination.
  #
  # FIX: Call calculate_opponent_adjustments() with through_week = w - 1 for
  # each distinct week w in the feature matrix. This produces an expanding
  # opponent adjustment where each player-week row sees only opponent quality
  # information from prior weeks. Week 1 rows correctly return NA (no prior
  # data), matching the same NA-for-first-week convention used by epa_roll3
  # and epa_season_to_date.
  #
  # Cost: O(W) calls to calculate_opponent_adjustments() where W = distinct
  # weeks in the matrix. Each call processes a growing slice of pbp_data.
  # For a full season (W ~ 18) this is acceptable. A progress message is
  # emitted per week so the user can track progress.
  if (!is.null(opp_adjusted)) {
    message("  Computing leakage-safe expanding opponent adjustment per week...")

    distinct_weeks <- sort(unique(weekly_with_rolling$week))

    opp_adj_weekly_list <- lapply(distinct_weeks, function(w) {
      # Use weeks 1 through w-1 only (prior weeks, not current week)
      prior_week_max <- w - 1L

      if (prior_week_max < week_min) {
        # Week 1 (or first observed week): no prior data -- return NA rows
        # for every player present in week w of the feature matrix
        players_this_week <- weekly_with_rolling %>%
          filter(week == w) %>%
          select(player_id, position_group, team) %>%
          distinct()
        return(
          players_this_week %>%
            mutate(
              opp_adjusted_epa_prior   = NA_real_,
              schedule_difficulty_rank = NA_integer_,
              opp_adj_games_prior      = 0L,
              week                     = w
            )
        )
      }

      message(glue("    Week {w}: scoring opponent adjustment through week {prior_week_max}..."))

      adj <- tryCatch(
        calculate_opponent_adjustments(
          pbp_data      = pbp_data,
          season        = season,
          week_min      = week_min,
          through_week  = prior_week_max,
          min_plays     = 1L,       # Low threshold: expanding window has fewer plays
                                    # early in season. Week 9 models can apply their
                                    # own min_plays filter on opp_adj_games_prior.
          min_def_plays = 30L       # Relaxed for early-season slices; full-season
                                    # default is 100 but weeks 1-3 have fewer plays.
        ),
        error = function(e) {
          message(glue("    Week {w}: calculate_opponent_adjustments failed: ",
                       "{conditionMessage(e)}. Returning NA for this week."))
          NULL
        }
      )

      if (is.null(adj) || nrow(adj) == 0) {
        players_this_week <- weekly_with_rolling %>%
          filter(week == w) %>%
          select(player_id, position_group, team) %>%
          distinct()
        return(
          players_this_week %>%
            mutate(
              opp_adjusted_epa_prior   = NA_real_,
              schedule_difficulty_rank = NA_integer_,
              opp_adj_games_prior      = 0L,
              week                     = w
            )
        )
      }

      # Pull this week's player-team mapping from the feature matrix.
      # Using weekly_with_rolling as the source of truth for which team
      # each player was on in week w -- not adj$team, which reflects the
      # most frequent team over prior weeks and may differ for traded players.
      players_week_w <- weekly_with_rolling %>%
        filter(week == w) %>%
        select(player_id, position_group, team) %>%
        distinct()

      adj %>%
        select(player_id, position_group,
               opp_adjusted_epa_prior   = opp_adjusted_epa,
               schedule_difficulty_rank,
               opp_adj_games_prior      = games_played) %>%
        left_join(players_week_w, by = c("player_id", "position_group")) %>%
        mutate(week = w)
    })

    opp_adj_weekly <- bind_rows(opp_adj_weekly_list)

    weekly_with_rolling <- weekly_with_rolling %>%
      left_join(opp_adj_weekly,
                by = c("player_id", "position_group", "team", "week"))

    n_covered <- sum(!is.na(weekly_with_rolling$opp_adjusted_epa_prior))
    message(glue(
      "  Leakage-safe opponent adjustment complete: ",
      "{n_covered} of {nrow(weekly_with_rolling)} player-week rows have ",
      "non-NA opp_adjusted_epa_prior. Week-1 rows are NA by design."
    ))

  } else {
    message("  opp_adjusted not provided -- skipping expanding opponent adjustment.")
    weekly_with_rolling <- weekly_with_rolling %>%
      mutate(
        opp_adjusted_epa_prior   = NA_real_,
        schedule_difficulty_rank = NA_integer_,
        opp_adj_games_prior      = NA_integer_
      )
  }

  # --- This-week opponent style and tier (LEAKAGE-SAFE) ---
  # Defensive style joined by OPPONENT this week -- this is forward-looking in
  # the sense that the full-season style classification uses future games.
  # For early-season rows (week 1-3), this is contaminated.
  # Flag is added to the output so Week 9 models can down-weight early weeks.
  if (!is.null(def_styles)) {
    def_join <- def_styles %>%
      filter(season == !!season) %>%
      select(team, defensive_style, overall_tier)

    weekly_with_rolling <- weekly_with_rolling %>%
      left_join(
        def_join %>% rename(opponent = team,
                            opponent_style = defensive_style,
                            opponent_tier  = overall_tier),
        by = "opponent"
      )

    message(glue("  NOTE: opponent_style and opponent_tier are full-season classifications. ",
                 "Early-season rows (week <= 4) are most contaminated. ",
                 "role_stability_flag will be FALSE for these rows."))
  } else {
    message("  def_styles not provided -- opponent matchup features will be NA.")
    weekly_with_rolling <- weekly_with_rolling %>%
      mutate(
        opponent_style = NA_character_,
        opponent_tier  = NA_character_
      )
  }

  # --- Final column ordering ---
  result <- weekly_with_rolling %>%
    select(
      season, week, player_id, player_name, position_group, team, opponent,
      plays_this_week, epa_this_week, success_rate_this_week,
      epa_roll3, epa_season_to_date, plays_roll3,
      neutral_epa_season, leading_share_season, trailing_share_season,
      opp_adjusted_epa_prior, schedule_difficulty_rank, opp_adj_games_prior,
      opponent_style, opponent_tier,
      weeks_played, weeks_since_role_change, role_stability_flag
    ) %>%
    arrange(player_id, position_group, week)

  n_players <- n_distinct(result$player_id)
  n_weeks   <- n_distinct(result$week)
  n_cols    <- ncol(result)
  message(glue(
    "compile_feature_matrix complete: {nrow(result)} player-week rows, ",
    "{n_players} players, {n_weeks} weeks, {n_cols} features."
  ))

  result
}
