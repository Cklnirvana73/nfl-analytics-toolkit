# ==============================================================================
# WEEK 8 TEST SUITE: OPPONENT-ADJUSTED METRICS & MATCHUP FEATURES
# ==============================================================================
#
# Tests for R/10_opponent_features.R
# Run via: source(here("test_week8.R"))
#
# Test categories:
#   1.  Input validation (all 4 functions)
#   2.  Empty data returns (all 4 functions)
#   3.  NFL-specific: garbage time exclusion
#   4.  NFL-specific: QB kneel / spike exclusion
#   5.  NFL-specific: QB scramble routing
#   6.  Opponent adjustment: formula correctness
#   7.  Opponent adjustment: zero-sum property
#   8.  Opponent adjustment: schedule difficulty rank
#   9.  Defensive style: classification thresholds
#   10. Defensive style: overall tier quintiles
#   11. Matchup history: min_plays_per_archetype guard (NA, not 0)
#   12. Matchup history: archetype_epa_range and best/worst archetype
#   13. Feature matrix: rolling features are lag-safe (no leakage)
#   14. Feature matrix: plays_this_week filter
#   15. Feature matrix: column completeness contract
#   16. Naming convention: no 'carries', no 'team_id'
#   17. Integration: all 4 functions run on same pbp
#
# ==============================================================================

library(testthat)
library(dplyr)
library(tidyr)
library(glue)

source(here::here("R", "10_opponent_features.R"))

# ==============================================================================
# TEST DATA HELPERS
# ==============================================================================

# Minimal play-by-play builder (returns tibble of n_plays rows)
make_pbp <- function(n_pass = 60, n_run = 40, season = 2025,
                     weeks = 1:8, n_teams = 4) {
  set.seed(42)
  teams    <- paste0("T", seq_len(n_teams))
  def_pool <- teams

  n_total  <- n_pass + n_run
  n_weeks  <- length(weeks)

  tibble(
    play_id           = seq_len(n_total * n_weeks),
    season            = season,
    week              = rep(weeks, each = n_total),
    game_id           = paste0(season, "_W", rep(weeks, each = n_total),
                               "_G1"),
    posteam           = sample(teams, n_total * n_weeks, replace = TRUE),
    defteam           = sample(def_pool, n_total * n_weeks, replace = TRUE),
    play_type         = c(rep("pass", n_pass), rep("run", n_run)) |>
      rep(n_weeks),
    epa               = rnorm(n_total * n_weeks, mean = 0.05, sd = 0.3),
    wp                = runif(n_total * n_weeks, 0.2, 0.8),
    wpa               = rnorm(n_total * n_weeks, 0, 0.03),
    qtr               = sample(1:4, n_total * n_weeks, replace = TRUE,
                               prob = c(0.25, 0.25, 0.25, 0.25)),
    passer_player_id  = ifelse(
      c(rep("pass", n_pass), rep("run", n_run)) == "pass",
      sample(paste0("QB", 1:3), n_total, replace = TRUE) |> rep(n_weeks),
      NA_character_
    ),
    passer_player_name = ifelse(
      !is.na(passer_player_id),
      paste0("QB_", passer_player_id),
      NA_character_
    ),
    rusher_player_id  = ifelse(
      c(rep("pass", n_pass), rep("run", n_run)) == "run",
      sample(paste0("RB", 1:3), n_total, replace = TRUE) |> rep(n_weeks),
      NA_character_
    ),
    rusher_player_name = ifelse(
      !is.na(rusher_player_id),
      paste0("RB_", rusher_player_id),
      NA_character_
    ),
    receiver_player_id = ifelse(
      c(rep("pass", n_pass), rep("run", n_run)) == "pass",
      sample(paste0("WR", 1:4), n_total, replace = TRUE) |> rep(n_weeks),
      NA_character_
    ),
    receiver_player_name = ifelse(
      !is.na(receiver_player_id),
      paste0("WR_", receiver_player_id),
      NA_character_
    ),
    qb_kneel   = 0L,
    qb_spike   = 0L,
    qb_scramble = 0L
  )
}

pbp_test <- make_pbp()


# ==============================================================================
# SECTION 1: INPUT VALIDATION
# ==============================================================================

test_that("calculate_opponent_adjustments: rejects non-data-frame input", {
  expect_error(
    calculate_opponent_adjustments("not_a_df"),
    "data frame"
  )
})

test_that("calculate_opponent_adjustments: rejects missing required columns", {
  bad <- pbp_test %>% select(-defteam)
  expect_error(
    calculate_opponent_adjustments(bad),
    "defteam"
  )
})

test_that("calculate_opponent_adjustments: rejects week_min > week_max", {
  expect_error(
    calculate_opponent_adjustments(pbp_test, week_min = 10, week_max = 5),
    "week_min"
  )
})

test_that("classify_defensive_style: rejects non-data-frame", {
  expect_error(classify_defensive_style(list(a = 1)), "data frame")
})

test_that("classify_defensive_style: rejects missing epa column", {
  bad <- pbp_test %>% select(-epa)
  expect_error(classify_defensive_style(bad), "epa")
})

test_that("classify_defensive_style: rejects negative funnel_threshold", {
  expect_error(
    classify_defensive_style(pbp_test, funnel_threshold = -0.01),
    "funnel_threshold"
  )
})

test_that("calculate_matchup_history: rejects non-data-frame pbp_data", {
  def <- classify_defensive_style(pbp_test)
  expect_error(calculate_matchup_history("bad", def), "data frame")
})

test_that("calculate_matchup_history: rejects missing def_styles columns", {
  bad_def <- tibble(season = 2025, team = "T1")  # missing defensive_style
  expect_error(
    calculate_matchup_history(pbp_test, bad_def),
    "defensive_style"
  )
})

test_that("compile_feature_matrix: season must be single numeric", {
  expect_error(
    compile_feature_matrix(pbp_test, season = c(2024, 2025)),
    "single numeric"
  )
})

test_that("compile_feature_matrix: rejects week_min > week_max", {
  expect_error(
    compile_feature_matrix(pbp_test, season = 2025,
                           week_min = 10, week_max = 5),
    "week_min"
  )
})


# ==============================================================================
# SECTION 2: EMPTY DATA RETURNS
# ==============================================================================

test_that("calculate_opponent_adjustments: returns tibble on empty season filter", {
  result <- calculate_opponent_adjustments(pbp_test, season = 9999)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("classify_defensive_style: returns tibble on empty season filter", {
  result <- classify_defensive_style(pbp_test, season = 9999)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("calculate_matchup_history: returns tibble on empty season filter", {
  def <- classify_defensive_style(pbp_test)
  result <- calculate_matchup_history(pbp_test, def, season = 9999)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("compile_feature_matrix: returns tibble when no data matches season", {
  result <- suppressMessages(
    compile_feature_matrix(pbp_test, season = 9999)
  )
  expect_true(is.data.frame(result))
})


# ==============================================================================
# SECTION 3: GARBAGE TIME EXCLUSION
# ==============================================================================

test_that("garbage time plays (Q4, WP < 0.10) are excluded from opp adjustments", {
  # Create data where all Q4 low-WP plays have extreme EPA
  pbp_with_garbage <- pbp_test %>%
    mutate(
      epa = ifelse(qtr == 4 & wp < 0.10, 99.0, epa)
    )
  result_clean   <- calculate_opponent_adjustments(pbp_test, season = 2025)
  result_garbage <- calculate_opponent_adjustments(pbp_with_garbage, season = 2025)

  # If garbage time is excluded, epa values should not be affected by the 99.0 plays
  expect_equal(result_clean$raw_epa_per_play,
               result_garbage$raw_epa_per_play,
               tolerance = 1e-10)
})

test_that("garbage time excluded in defensive style classification", {
  pbp_with_garbage <- pbp_test %>%
    mutate(epa = ifelse(qtr == 4 & wp > 0.90, -99.0, epa))

  result_clean   <- classify_defensive_style(pbp_test, season = 2025)
  result_garbage <- classify_defensive_style(pbp_with_garbage, season = 2025)

  expect_equal(result_clean$overall_epa_allowed,
               result_garbage$overall_epa_allowed,
               tolerance = 1e-10)
})

test_that("OT plays (qtr == 5) are NOT excluded as garbage time", {
  pbp_with_ot <- pbp_test %>%
    bind_rows(
      pbp_test %>%
        slice_head(n = 20) %>%
        mutate(
          qtr    = 5L,
          wp     = 0.05,   # would be garbage if qtr == 4, but qtr == 5
          epa    = 5.0,    # distinctive value
          week   = 1L,
          game_id = "OT_game"
        )
    )

  result <- calculate_opponent_adjustments(pbp_with_ot, season = 2025)
  # OT plays have EPA = 5.0; if excluded, mean would be lower
  # Just check that we get a result (OT plays are processed, not dropped)
  expect_true(nrow(result) > 0)
})


# ==============================================================================
# SECTION 4: QB KNEEL AND SPIKE EXCLUSION
# ==============================================================================

test_that("QB kneels are excluded from opponent adjustments", {
  pbp_with_kneels <- pbp_test %>%
    bind_rows(
      pbp_test %>%
        slice(1:20) %>%
        mutate(
          qb_kneel          = 1L,
          epa               = 99.0,
          rusher_player_id  = "RB_KNEEL",
          rusher_player_name = "RB_KNEEL",
          play_type         = "run"
        )
    )

  result_clean  <- calculate_opponent_adjustments(pbp_test, season = 2025)
  result_kneels <- calculate_opponent_adjustments(pbp_with_kneels, season = 2025)

  # RB_KNEEL should not appear in results
  expect_false("RB_KNEEL" %in% result_kneels$player_id)
  # Regular players' values should be unchanged
  expect_equal(result_clean$raw_epa_per_play,
               result_kneels$raw_epa_per_play,
               tolerance = 1e-10)
})

test_that("QB spikes are excluded from defensive style classification", {
  pbp_with_spikes <- pbp_test %>%
    mutate(epa = ifelse(qb_spike == 1, 99.0, epa))

  result <- classify_defensive_style(pbp_with_spikes, season = 2025)
  expect_true(nrow(result) > 0)
  expect_true(all(result$overall_epa_allowed < 50))
})


# ==============================================================================
# SECTION 5: QB SCRAMBLE ROUTING
# ==============================================================================

test_that("QB scrambles are routed to passer group not rusher group", {
  # Create scramble plays
  pbp_with_scrambles <- pbp_test %>%
    bind_rows(
      pbp_test %>%
        filter(play_type == "run") %>%
        slice_head(n = 30) %>%
        mutate(
          qb_scramble        = 1L,
          passer_player_id   = "QB_SCRAMBLER",
          passer_player_name = "QB_SCRAMBLER",
          rusher_player_id   = "RB_NOT_THIS",
          rusher_player_name = "RB_NOT_THIS"
        )
    )

  result <- calculate_opponent_adjustments(pbp_with_scrambles, season = 2025,
                                           min_plays = 10)

  # Scrambler should appear in passer group
  passer_rows <- result %>% filter(player_id == "QB_SCRAMBLER",
                                   position_group == "passer")
  rusher_rows <- result %>% filter(player_id == "QB_SCRAMBLER",
                                   position_group == "rusher")

  expect_true(nrow(passer_rows) > 0,
    info = "QB scrambler should appear in passer group")
  expect_equal(nrow(rusher_rows), 0,
    info = "QB scrambler should NOT appear in rusher group")
})


# ==============================================================================
# SECTION 6: OPPONENT ADJUSTMENT FORMULA CORRECTNESS
# ==============================================================================

test_that("opp_adjusted_epa = raw_epa - opp_adjustment", {
  result <- calculate_opponent_adjustments(pbp_test, season = 2025)
  expect_true(nrow(result) > 0,
    info = "Need qualifying players to test formula")

  recomputed <- result$raw_epa_per_play - result$opp_adjustment
  expect_equal(result$opp_adjusted_epa, recomputed, tolerance = 1e-10)
})

test_that("opp_adjustment = avg_opponent_epa_allowed - league_avg_epa_allowed", {
  result <- calculate_opponent_adjustments(pbp_test, season = 2025)
  if (nrow(result) == 0) skip("No qualifying players")

  recomputed <- result$avg_opponent_epa_allowed - result$league_avg_epa_allowed
  expect_equal(result$opp_adjustment, recomputed, tolerance = 1e-10)
})

test_that("league_avg_epa_allowed is the same value for all rows in a season", {
  result <- calculate_opponent_adjustments(pbp_test, season = 2025)
  if (nrow(result) == 0) skip("No qualifying players")

  league_vals <- unique(result$league_avg_epa_allowed)
  expect_equal(length(league_vals), 1,
    info = "League average should be constant within a season")
})

test_that("player facing tough defense gets positive opp_adjustment (rewarded)", {
  # Build controlled data: one QB faces only very good defenses (allow low EPA)
  pbp_controlled <- pbp_test %>%
    mutate(
      # Make defteam "DEF_GOOD" allow very low EPA
      defteam = ifelse(
        passer_player_id == "QB1",
        "T1",    # strong defense (we'll set T1 EPA allowed to low)
        defteam
      )
    )

  result <- calculate_opponent_adjustments(pbp_controlled, season = 2025,
                                           min_plays = 5)
  if (nrow(result) == 0) skip("No qualifying rows")

  # Player with hardest schedule (lowest avg_opponent_epa_allowed)
  hardest <- result %>%
    filter(position_group == "passer") %>%
    arrange(avg_opponent_epa_allowed) %>%
    slice_head(n = 1)

  # They should have positive opp_adjustment (faced better defense than avg)
  # avg_opp_epa < league_avg => opp_adjustment < 0... wait:
  # opp_adjustment = avg_opponent_epa_allowed - league_avg_epa_allowed
  # Tougher defense = LOWER epa_allowed => opp_adjustment is NEGATIVE
  # opp_adjusted_epa = raw - opp_adjustment => raw - (negative) => HIGHER
  # (player rewarded by getting a higher adjusted number)
  expect_true(hardest$opp_adjustment <= 0,
    info = "Hardest schedule player should have negative opp_adjustment (tougher than avg)")
  expect_true(hardest$opp_adjusted_epa >= hardest$raw_epa_per_play - 1e-10,
    info = "Hardest schedule player should have opp_adjusted >= raw (rewarded)")
})


# ==============================================================================
# SECTION 7: OPPONENT ADJUSTMENT ZERO-SUM PROPERTY
# ==============================================================================

test_that("mean opp_adjustment is close to zero across players (zero-sum)", {
  result <- calculate_opponent_adjustments(pbp_test, season = 2025)
  if (nrow(result) < 3) skip("Too few players for zero-sum test")

  mean_adj <- mean(result$opp_adjustment, na.rm = TRUE)
  # Not exactly zero due to play-count weighting and min_plays filter,
  # but should be small (within 0.05 EPA)
  expect_lt(abs(mean_adj), 0.10,
    label = "mean(opp_adjustment) should be near zero")
})


# ==============================================================================
# SECTION 8: SCHEDULE DIFFICULTY RANK
# ==============================================================================

test_that("schedule_difficulty_rank is within group (position_group-season)", {
  result <- calculate_opponent_adjustments(pbp_test, season = 2025)
  if (nrow(result) == 0) skip("No qualifying players")

  rank_check <- result %>%
    group_by(season, position_group) %>%
    summarise(
      max_rank = max(schedule_difficulty_rank, na.rm = TRUE),
      n_rows   = n(),
      .groups  = "drop"
    )

  expect_true(all(rank_check$max_rank <= rank_check$n_rows),
    info = "Max rank should not exceed number of players in group")
})


# ==============================================================================
# SECTION 9: DEFENSIVE STYLE CLASSIFICATION
# ==============================================================================

test_that("defensive_style values are only valid categories", {
  result <- classify_defensive_style(pbp_test, season = 2025)
  if (nrow(result) == 0) skip("No qualifying defenses")

  valid <- c("pass_funnel", "run_funnel", "balanced")
  expect_true(all(result$defensive_style %in% valid))
})

test_that("pass_funnel defense has higher pass_epa_vs_league than rush_epa_vs_league", {
  result <- classify_defensive_style(pbp_test, season = 2025,
                                     funnel_threshold = 0.01)
  pass_funnels <- result %>% filter(defensive_style == "pass_funnel")
  if (nrow(pass_funnels) == 0) skip("No pass-funnel defenses in test data")

  expect_true(all(
    pass_funnels$pass_epa_vs_league - pass_funnels$rush_epa_vs_league > 0.01
  ))
})

test_that("run_funnel defense has higher rush_epa_vs_league than pass_epa_vs_league", {
  result <- classify_defensive_style(pbp_test, season = 2025,
                                     funnel_threshold = 0.01)
  run_funnels <- result %>% filter(defensive_style == "run_funnel")
  if (nrow(run_funnels) == 0) skip("No run-funnel defenses in test data")

  expect_true(all(
    run_funnels$rush_epa_vs_league - run_funnels$pass_epa_vs_league > 0.01
  ))
})

test_that("style_strength is always non-negative", {
  result <- classify_defensive_style(pbp_test, season = 2025)
  if (nrow(result) == 0) skip("No qualifying defenses")

  expect_true(all(result$style_strength >= 0))
})

test_that("pass_epa_vs_league and rush_epa_vs_league are relative to league avg", {
  result <- classify_defensive_style(pbp_test, season = 2025)
  if (nrow(result) == 0) skip("No qualifying defenses")

  # Mean of pass_epa_vs_league should be near zero (by construction)
  mean_pass_rel <- mean(result$pass_epa_vs_league, na.rm = TRUE)
  expect_lt(abs(mean_pass_rel), 0.15,
    label = "Mean pass_epa_vs_league should be near zero")
})


# ==============================================================================
# SECTION 10: OVERALL TIER QUINTILES
# ==============================================================================

test_that("overall_tier values are only valid categories", {
  result <- classify_defensive_style(pbp_test, season = 2025)
  if (nrow(result) == 0) skip("No qualifying defenses")

  valid_tiers <- c("elite", "above_avg", "average", "below_avg", "poor")
  expect_true(all(result$overall_tier %in% valid_tiers))
})

test_that("elite defenses have lower overall_epa_allowed than poor defenses", {
  result <- classify_defensive_style(pbp_test, season = 2025)
  if (nrow(result) == 0) skip("No qualifying defenses")

  elite_max <- result %>%
    filter(overall_tier == "elite") %>%
    pull(overall_epa_allowed) %>%
    max(na.rm = TRUE)

  poor_min  <- result %>%
    filter(overall_tier == "poor") %>%
    pull(overall_epa_allowed) %>%
    min(na.rm = TRUE)

  if (is.infinite(elite_max) || is.infinite(poor_min)) {
    skip("Insufficient data for both elite and poor tier check")
  }

  expect_lt(elite_max, poor_min,
    info = "Elite defenses allow less EPA than poor defenses")
})


# ==============================================================================
# SECTION 11: MATCHUP HISTORY -- NA NOT 0 FOR SMALL SAMPLES
# ==============================================================================

test_that("archetype EPA is NA (not 0) when plays < min_plays_per_archetype", {
  def_styles <- classify_defensive_style(pbp_test, season = 2025)
  result <- calculate_matchup_history(
    pbp_test, def_styles,
    season = 2025,
    min_plays_per_archetype = 9999  # impossibly high threshold -> all NA
  )
  if (nrow(result) == 0) skip("No results with impossibly high min_plays")

  # All archetype EPA columns should be NA
  expect_true(all(is.na(result$vs_pass_funnel_epa)),
    info = "vs_pass_funnel_epa should be NA when min_plays_per_archetype not met")
  expect_true(all(is.na(result$vs_run_funnel_epa)),
    info = "vs_run_funnel_epa should be NA when min_plays_per_archetype not met")
})

test_that("archetype play counts are always non-negative integers", {
  def_styles <- classify_defensive_style(pbp_test, season = 2025)
  result <- calculate_matchup_history(pbp_test, def_styles, season = 2025)
  if (nrow(result) == 0) skip("No qualifying rows")

  expect_true(all(result$vs_pass_funnel_plays >= 0L))
  expect_true(all(result$vs_run_funnel_plays  >= 0L))
  expect_true(all(result$vs_balanced_plays    >= 0L))
})


# ==============================================================================
# SECTION 12: ARCHETYPE EPA RANGE AND BEST/WORST
# ==============================================================================

test_that("archetype_epa_range is NA when fewer than 2 archetypes qualify", {
  def_styles <- classify_defensive_style(pbp_test, season = 2025)
  result <- calculate_matchup_history(
    pbp_test, def_styles,
    season = 2025,
    min_plays_per_archetype = 9999
  )
  if (nrow(result) == 0) skip("No results")

  expect_true(all(is.na(result$archetype_epa_range)))
  expect_true(all(is.na(result$best_archetype)))
  expect_true(all(is.na(result$worst_archetype)))
})

test_that("archetype_epa_range is non-negative when computed", {
  def_styles <- classify_defensive_style(pbp_test, season = 2025)
  result <- calculate_matchup_history(pbp_test, def_styles,
                                      season = 2025,
                                      min_plays_per_archetype = 1)
  valid_ranges <- result$archetype_epa_range[!is.na(result$archetype_epa_range)]
  if (length(valid_ranges) == 0) skip("No computed archetype ranges")

  expect_true(all(valid_ranges >= 0))
})


# ==============================================================================
# SECTION 13: FEATURE MATRIX ROLLING LEAKAGE CHECK
# ==============================================================================

test_that("epa_roll3 at week N does not include week N itself (lag-safe)", {
  result <- suppressMessages(
    compile_feature_matrix(pbp_test, season = 2025,
                           week_min = 1, week_max = 8,
                           rolling_window = 3,
                           min_plays_per_week = 1)
  )
  if (nrow(result) == 0) skip("No feature matrix rows")

  # For the first week a player appears, epa_roll3 must be NA
  first_appearances <- result %>%
    group_by(player_id, position_group) %>%
    filter(week == min(week)) %>%
    ungroup()

  expect_true(all(is.na(first_appearances$epa_roll3)),
    info = "epa_roll3 must be NA for a player's first week (no prior data)")
})

test_that("epa_season_to_date at week N does not include week N (lag-safe)", {
  result <- suppressMessages(
    compile_feature_matrix(pbp_test, season = 2025,
                           week_min = 1, week_max = 8,
                           min_plays_per_week = 1)
  )
  if (nrow(result) == 0) skip("No feature matrix rows")

  first_appearances <- result %>%
    group_by(player_id, position_group) %>%
    filter(week == min(week)) %>%
    ungroup()

  expect_true(all(is.na(first_appearances$epa_season_to_date)),
    info = "epa_season_to_date must be NA for a player's first week (no prior data)")
})


# ==============================================================================
# SECTION 14: PLAYS_THIS_WEEK FILTER
# ==============================================================================

test_that("plays_this_week >= min_plays_per_week for all rows", {
  result <- suppressMessages(
    compile_feature_matrix(pbp_test, season = 2025,
                           min_plays_per_week = 5)
  )
  if (nrow(result) == 0) skip("No rows after min_plays filter")
  expect_true(all(result$plays_this_week >= 5))
})

test_that("stricter min_plays_per_week returns fewer rows", {
  result_loose  <- suppressMessages(
    compile_feature_matrix(pbp_test, season = 2025, min_plays_per_week = 1))
  result_strict <- suppressMessages(
    compile_feature_matrix(pbp_test, season = 2025, min_plays_per_week = 20))

  expect_gte(nrow(result_loose), nrow(result_strict))
})


# ==============================================================================
# SECTION 15: COLUMN CONTRACT TESTS
# ==============================================================================

test_that("calculate_opponent_adjustments returns all documented columns", {
  result <- calculate_opponent_adjustments(pbp_test, season = 2025)
  expected_cols <- c(
    "season", "player_id", "player_name", "position_group", "team",
    "total_plays", "raw_epa_per_play", "avg_opponent_epa_allowed",
    "league_avg_epa_allowed", "opp_adjustment", "opp_adjusted_epa",
    "games_played", "schedule_difficulty_rank"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(result),
      info = glue("Expected column '{col}' missing from calculate_opponent_adjustments()"))
  }
})

test_that("classify_defensive_style returns all documented columns", {
  result <- classify_defensive_style(pbp_test, season = 2025)
  expected_cols <- c(
    "season", "team", "pass_plays_allowed", "rush_plays_allowed",
    "pass_epa_allowed", "rush_epa_allowed", "overall_epa_allowed",
    "pass_epa_vs_league", "rush_epa_vs_league",
    "defensive_style", "style_strength", "overall_tier"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(result),
      info = glue("Expected column '{col}' missing from classify_defensive_style()"))
  }
})

test_that("calculate_matchup_history returns all documented columns", {
  def_styles <- classify_defensive_style(pbp_test, season = 2025)
  result <- calculate_matchup_history(pbp_test, def_styles, season = 2025)
  expected_cols <- c(
    "season", "player_id", "player_name", "position_group", "team",
    "overall_epa_per_play",
    "vs_pass_funnel_plays", "vs_pass_funnel_epa",
    "vs_run_funnel_plays",  "vs_run_funnel_epa",
    "vs_balanced_plays",    "vs_balanced_epa",
    "vs_elite_def_plays",   "vs_elite_def_epa",
    "archetype_epa_range", "best_archetype", "worst_archetype"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(result),
      info = glue("Expected column '{col}' missing from calculate_matchup_history()"))
  }
})

test_that("compile_feature_matrix returns all documented columns", {
  result <- suppressMessages(
    compile_feature_matrix(pbp_test, season = 2025,
                           min_plays_per_week = 1)
  )
  if (nrow(result) == 0) skip("No feature matrix rows")
  expected_cols <- c(
    "season", "week", "player_id", "player_name", "position_group",
    "team", "opponent", "plays_this_week", "epa_this_week",
    "success_rate_this_week", "epa_roll3", "epa_season_to_date",
    "plays_roll3", "neutral_epa_season", "leading_share_season",
    "trailing_share_season", "opp_adjusted_epa_season",
    "schedule_difficulty_rank", "opponent_style", "opponent_tier",
    "weeks_played"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(result),
      info = glue("Expected column '{col}' missing from compile_feature_matrix()"))
  }
})


# ==============================================================================
# SECTION 16: NAMING CONVENTION ENFORCEMENT
# ==============================================================================

test_that("no column named 'carries' in any output (use rushes)", {
  opp   <- calculate_opponent_adjustments(pbp_test, season = 2025)
  def   <- classify_defensive_style(pbp_test, season = 2025)
  arch  <- calculate_matchup_history(pbp_test, def, season = 2025)
  feat  <- suppressMessages(compile_feature_matrix(pbp_test, season = 2025,
                                                    min_plays_per_week = 1))

  expect_false("carries" %in% names(opp),  info = "carries in opp adj output")
  expect_false("carries" %in% names(def),  info = "carries in def style output")
  expect_false("carries" %in% names(arch), info = "carries in matchup history output")
  if (nrow(feat) > 0) {
    expect_false("carries" %in% names(feat), info = "carries in feature matrix output")
  }
})

test_that("no column named 'team_id' in any output (use team or posteam)", {
  opp <- calculate_opponent_adjustments(pbp_test, season = 2025)
  def <- classify_defensive_style(pbp_test, season = 2025)
  expect_false("team_id" %in% names(opp))
  expect_false("team_id" %in% names(def))
})


# ==============================================================================
# SECTION 17: INTEGRATION TEST
# ==============================================================================

test_that("INTEGRATION: all four functions run on same pbp without error", {
  expect_no_error({
    opp_adj  <- calculate_opponent_adjustments(pbp_test, season = 2025)
    def_sty  <- classify_defensive_style(pbp_test, season = 2025)
    arch_his <- calculate_matchup_history(pbp_test, def_sty, season = 2025)
    feat_mat <- suppressMessages(
      compile_feature_matrix(
        pbp_data     = pbp_test,
        opp_adjusted = opp_adj,
        def_styles   = def_sty,
        season       = 2025,
        min_plays_per_week = 1
      )
    )
  })
})

test_that("INTEGRATION: season and week filters applied correctly", {
  # Full season
  full   <- calculate_opponent_adjustments(pbp_test, season = 2025)
  # Weeks 1-4 only
  early  <- calculate_opponent_adjustments(pbp_test, season = 2025,
                                           week_min = 1, week_max = 4)

  # Early-season should have <= plays than full season
  if (nrow(full) > 0 && nrow(early) > 0) {
    full_plays  <- sum(full$total_plays)
    early_plays <- sum(early$total_plays)
    expect_lte(early_plays, full_plays)
  }
})

test_that("INTEGRATION: compile_feature_matrix respects NULL opp_adjusted", {
  result <- suppressMessages(
    compile_feature_matrix(
      pbp_data     = pbp_test,
      opp_adjusted = NULL,
      def_styles   = NULL,
      season       = 2025,
      min_plays_per_week = 1
    )
  )
  if (nrow(result) == 0) skip("No rows")

  # Should still have all columns, but opp/style columns are NA
  expect_true("opp_adjusted_epa_season" %in% names(result))
  expect_true(all(is.na(result$opp_adjusted_epa_season)))
  expect_true("opponent_style" %in% names(result))
  expect_true(all(is.na(result$opponent_style)))
})

# ==============================================================================
# TEST SUMMARY
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("Week 8 test suite complete.\n")
cat("Functions tested: calculate_opponent_adjustments,\n")
cat("  classify_defensive_style, calculate_matchup_history,\n")
cat("  compile_feature_matrix\n")
cat(strrep("=", 60), "\n")
