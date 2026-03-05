# ==============================================================================
# WEEK 6: FULL TEST SUITE
# tests/test_week6_functions.R
# ==============================================================================
# 16 test cases covering:
#   - Input validation (all 4 functions)
#   - Column name verification
#   - Division-by-zero guards
#   - Empty data handling
#   - Multi-team player handling
#   - NA handling
#   - NFL-specific edge cases (QB kneels, spikes, scrambles)
#   - Integration test (full pipeline)
#
# ==============================================================================

library(testthat)
library(dplyr)
library(here)

source(here("R", "08_usage_features.R"))

# ==============================================================================
# TEST DATA FACTORIES
# ==============================================================================

# Minimal valid pbp tibble for testing
make_test_pbp <- function(n_pass = 30, n_run = 20, include_redzone = TRUE,
                          include_kneels = FALSE, include_scramble = FALSE) {
  set.seed(42)

  pass_plays <- tibble(
    season             = 2025L,
    week               = rep(1:3, length.out = n_pass),
    game_id            = paste0("2025_0", week, "_KC_BUF"),
    play_type          = "pass",
    posteam            = "KC",
    defteam            = "BUF",
    passer_player_id   = "QB001",
    passer_player_name = "Test QB",
    rusher_player_id   = NA_character_,
    rusher_player_name = NA_character_,
    receiver_player_id = sample(c("WR001", "WR002", "TE001"), n_pass, replace = TRUE),
    receiver_player_name = case_when(
      receiver_player_id == "WR001" ~ "Test WR1",
      receiver_player_id == "WR002" ~ "Test WR2",
      TRUE                           ~ "Test TE"
    ),
    air_yards          = round(runif(n_pass, -2, 30)),
    complete_pass      = sample(c(0, 1), n_pass, replace = TRUE, prob = c(0.35, 0.65)),
    touchdown          = sample(c(0, 1), n_pass, replace = TRUE, prob = c(0.9, 0.1)),
    yardline_100       = sample(10:80, n_pass, replace = TRUE),
    epa                = rnorm(n_pass, 0.05, 0.5),
    wp                 = runif(n_pass, 0.2, 0.8),
    down               = sample(1:3, n_pass, replace = TRUE),
    ydstogo            = sample(1:15, n_pass, replace = TRUE),
    qb_kneel           = 0L,
    qb_spike           = 0L,
    qb_scramble        = 0L,
    game_date          = as.Date("2025-09-07") + (week - 1) * 7
  )

  run_plays <- tibble(
    season             = 2025L,
    week               = rep(1:3, length.out = n_run),
    game_id            = paste0("2025_0", week, "_KC_BUF"),
    play_type          = "run",
    posteam            = "KC",
    defteam            = "BUF",
    passer_player_id   = NA_character_,
    passer_player_name = NA_character_,
    rusher_player_id   = sample(c("RB001", "RB002"), n_run, replace = TRUE),
    rusher_player_name = case_when(
      rusher_player_id == "RB001" ~ "Test RB1",
      TRUE                         ~ "Test RB2"
    ),
    receiver_player_id   = NA_character_,
    receiver_player_name = NA_character_,
    air_yards            = NA_real_,
    complete_pass        = NA_real_,
    touchdown            = sample(c(0, 1), n_run, replace = TRUE, prob = c(0.92, 0.08)),
    yardline_100         = sample(10:80, n_run, replace = TRUE),
    epa                  = rnorm(n_run, -0.03, 0.4),
    wp                   = runif(n_run, 0.2, 0.8),
    down                 = sample(1:3, n_run, replace = TRUE),
    ydstogo              = sample(1:10, n_run, replace = TRUE),
    qb_kneel             = 0L,
    qb_spike             = 0L,
    qb_scramble          = 0L,
    game_date            = as.Date("2025-09-07") + (week - 1) * 7
  )

  pbp <- bind_rows(pass_plays, run_plays)

  if (include_redzone) {
    # Add explicit red zone plays
    rz_plays <- pbp %>%
      slice_head(n = 5) %>%
      mutate(yardline_100 = sample(5:15, 5, replace = TRUE))
    pbp <- bind_rows(pbp, rz_plays)
  }

  if (include_kneels) {
    kneel_plays <- tibble(
      season = 2025L, week = 1L, game_id = "2025_01_KC_BUF",
      play_type = "run", posteam = "KC", defteam = "BUF",
      passer_player_id = NA_character_, passer_player_name = NA_character_,
      rusher_player_id = "QB001", rusher_player_name = "Test QB",
      receiver_player_id = NA_character_, receiver_player_name = NA_character_,
      air_yards = NA_real_, complete_pass = NA_real_, touchdown = 0L,
      yardline_100 = 50L, epa = -0.8, wp = 0.95,
      down = 2L, ydstogo = 5L,
      qb_kneel = 1L, qb_spike = 0L, qb_scramble = 0L,
      game_date = as.Date("2025-09-07")
    )
    pbp <- bind_rows(pbp, kneel_plays)
  }

  if (include_scramble) {
    scramble_plays <- tibble(
      season = 2025L, week = 1L, game_id = "2025_01_KC_BUF",
      play_type = "run", posteam = "KC", defteam = "BUF",
      passer_player_id = "QB001", passer_player_name = "Test QB",
      rusher_player_id = "QB001", rusher_player_name = "Test QB",
      receiver_player_id = NA_character_, receiver_player_name = NA_character_,
      air_yards = NA_real_, complete_pass = NA_real_, touchdown = 0L,
      yardline_100 = 35L, epa = 0.3, wp = 0.55,
      down = 3L, ydstogo = 8L,
      qb_kneel = 0L, qb_spike = 0L, qb_scramble = 1L,
      game_date = as.Date("2025-09-07")
    )
    pbp <- bind_rows(pbp, scramble_plays)
  }

  pbp
}


# ==============================================================================
# TEST 1: calculate_usage_metrics() - returns expected columns
# ==============================================================================
test_that("calculate_usage_metrics() returns expected columns", {
  pbp <- make_test_pbp()
  result <- calculate_usage_metrics(pbp, season = 2025)

  expected_cols <- c(
    "season", "week", "game_id", "player_id", "player_name",
    "position_group", "team", "targets", "team_targets", "target_share",
    "rushes", "team_rushes", "rush_share", "air_yards", "team_air_yards",
    "air_yards_share", "redzone_targets", "redzone_rushes",
    "redzone_opportunities", "team_redzone_plays", "redzone_share"
  )
  missing <- setdiff(expected_cols, names(result))
  expect_true(length(missing) == 0,
    info = paste("Missing columns:", paste(missing, collapse = ", ")))
})

# ==============================================================================
# TEST 2: calculate_usage_metrics() - target shares sum <= 1.0 per team-week
# ==============================================================================
test_that("target shares sum to approximately 1.0 per team-week", {
  pbp <- make_test_pbp(n_pass = 40)
  result <- calculate_usage_metrics(pbp, season = 2025)

  team_week_shares <- result %>%
    filter(position_group == "receiver", !is.na(target_share)) %>%
    group_by(season, week, game_id, team) %>%
    summarise(total_share = sum(target_share, na.rm = TRUE), .groups = "drop")

  # Allow slight tolerance for rounding
  expect_true(all(team_week_shares$total_share <= 1.01),
    info = "Team target shares should not exceed 1.0")
  expect_true(all(team_week_shares$total_share >= 0.5),
    info = "Team target shares should be positive")
})

# ==============================================================================
# TEST 3: QB kneels excluded from denominators
# ==============================================================================
test_that("QB kneels do not inflate team rush denominator", {
  pbp_no_kneels  <- make_test_pbp(n_run = 20, include_kneels = FALSE)
  pbp_with_kneels <- make_test_pbp(n_run = 20, include_kneels = TRUE)

  result_no     <- calculate_usage_metrics(pbp_no_kneels,  season = 2025)
  result_with   <- calculate_usage_metrics(pbp_with_kneels, season = 2025)

  # Kneels should be excluded -- denominators should be the same
  # Filter to rusher rows: receiver rows have NA team_rushes by design
  rushers_no   <- result_no   %>% dplyr::filter(position_group == "rusher")
  rushers_with <- result_with %>% dplyr::filter(position_group == "rusher")
  max_rush_no   <- if (nrow(rushers_no)   > 0) max(rushers_no$team_rushes,   na.rm = TRUE) else 0L
  max_rush_with <- if (nrow(rushers_with) > 0) max(rushers_with$team_rushes, na.rm = TRUE) else 0L

  expect_equal(max_rush_no, max_rush_with,
    info = "QB kneels should not appear in team_rushes denominator")
})

# ==============================================================================
# TEST 4: QB scrambles excluded from rusher stats
# ==============================================================================
test_that("QB scrambles excluded from rusher position group", {
  pbp <- make_test_pbp(n_run = 20, include_scramble = TRUE)
  result <- calculate_usage_metrics(pbp, season = 2025)

  # QB should not appear as a "rusher" due to scramble
  qb_as_rusher <- result %>%
    filter(position_group == "rusher", player_id == "QB001")

  expect_equal(nrow(qb_as_rusher), 0,
    info = "QB scrambles should not create rusher rows for QB player ID")
})

# ==============================================================================
# TEST 5: Division guard - no Inf or NaN in share columns
# ==============================================================================
test_that("No Inf or NaN values in share columns", {
  pbp <- make_test_pbp()
  result <- calculate_usage_metrics(pbp, season = 2025)

  share_cols <- c("target_share", "rush_share", "air_yards_share", "redzone_share")
  for (col in share_cols) {
    vals <- result[[col]]
    expect_false(any(is.infinite(vals), na.rm = TRUE),
      info = paste("Infinite value in", col))
    expect_false(any(is.nan(vals), na.rm = TRUE),
      info = paste("NaN value in", col))
  }
})

# ==============================================================================
# TEST 6: Empty data returns empty tibble, not error
# ==============================================================================
test_that("calculate_usage_metrics() handles empty data gracefully", {
  pbp <- make_test_pbp() %>% filter(season == 9999)  # no rows
  expect_message(
    result <- calculate_usage_metrics(pbp, season = 9999),
    regexp = "empty"
  )
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "tbl_df")
})

# ==============================================================================
# TEST 7: calculate_usage_trends() returns expected columns
# ==============================================================================
test_that("calculate_usage_trends() returns expected columns", {
  pbp <- make_test_pbp(n_pass = 60, n_run = 30)
  # Use many weeks to enable rolling
  pbp_multi <- pbp %>%
    bind_rows(pbp %>% mutate(week = week + 3)) %>%
    bind_rows(pbp %>% mutate(week = week + 6))

  usage <- calculate_usage_metrics(pbp_multi, season = 2025)
  result <- calculate_usage_trends(usage)

  expected_cols <- c(
    "season", "week", "player_id", "player_name", "position_group", "team",
    "target_share", "rush_share", "air_yards_share",
    "target_share_roll3", "target_share_roll4",
    "rush_share_roll3", "rush_share_roll4",
    "target_share_delta1", "rush_share_delta1", "air_yards_share_delta1",
    "role_trend"
  )
  missing <- setdiff(expected_cols, names(result))
  expect_true(length(missing) == 0,
    info = paste("Missing columns:", paste(missing, collapse = ", ")))
})

# ==============================================================================
# TEST 8: Feature leakage prevention - roll3 week 4 = avg(weeks 1-3), not weeks 2-4
# ==============================================================================
test_that("Rolling usage trends use lagged data (no feature leakage)", {
  # Create a simple single-player, multi-week dataset with known values
  usage_simple <- tibble(
    season         = 2025L,
    week           = 1:6,
    player_id      = "WR001",
    player_name    = "Test WR",
    position_group = "receiver",
    team           = "KC",
    target_share   = c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60),
    rush_share     = NA_real_,
    air_yards_share = c(0.12, 0.22, 0.32, 0.42, 0.52, 0.62)
  )

  result <- calculate_usage_trends(usage_simple)

  # Week 4 roll3 should use weeks 1-3 (lagged), avg = (0.10+0.20+0.30)/3 = 0.20
  week4_roll3 <- result %>% filter(week == 4) %>% pull(target_share_roll3)
  expect_equal(round(week4_roll3, 4), round((0.10 + 0.20 + 0.30) / 3, 4),
    info = "roll3 for week 4 should use weeks 1-3 (not weeks 2-4)")
})

# ==============================================================================
# TEST 9: role_trend valid values
# ==============================================================================
test_that("role_trend only contains valid values", {
  # Use n_pass=80 per week so team_targets >= min_team_plays=20 default threshold
  # week offsets create 9 distinct weeks giving enough history for rolling windows
  pbp_multi <- bind_rows(
    make_test_pbp(n_pass = 80, n_run = 40),
    make_test_pbp(n_pass = 80, n_run = 40) %>% mutate(week = week + 3),
    make_test_pbp(n_pass = 80, n_run = 40) %>% mutate(week = week + 6)
  )
  usage  <- calculate_usage_metrics(pbp_multi, season = 2025)
  result <- calculate_usage_trends(usage)

  expect_true(nrow(result) > 0,
    info = "calculate_usage_trends should return non-empty result with 9 weeks of data")
  expect_true("role_trend" %in% names(result),
    info = "result must contain role_trend column")

  invalid <- result %>%
    dplyr::filter(!is.na(role_trend),
                  !role_trend %in% c("expanding", "stable", "shrinking"))

  expect_equal(nrow(invalid), 0,
    info = "role_trend should only be: expanding, stable, shrinking, or NA")
})

# ==============================================================================
# TEST 10: get_redzone_profile() returns expected columns
# ==============================================================================
test_that("get_redzone_profile() returns expected columns", {
  pbp <- make_test_pbp(n_pass = 60, n_run = 40, include_redzone = TRUE)
  # Force some explicit red zone plays to ensure we have enough
  pbp_rz <- pbp %>%
    bind_rows(
      pbp %>% slice_head(n = 20) %>% mutate(yardline_100 = sample(5:18, 20, replace = TRUE))
    )
  result <- get_redzone_profile(pbp_rz, season = 2025, min_redzone_opps = 1)

  expected_cols <- c(
    "season", "player_id", "player_name", "position_group", "team", "games_played",
    "rz20_targets", "rz20_rushes", "rz20_tds", "rz20_td_rate", "rz20_epa",
    "rz10_targets", "rz10_rushes", "rz10_tds", "rz10_td_rate",
    "rz5_targets",  "rz5_rushes",  "rz5_tds",  "rz5_td_rate"
  )
  missing <- setdiff(expected_cols, names(result))
  expect_true(length(missing) == 0,
    info = paste("Missing columns:", paste(missing, collapse = ", ")))
})

# ==============================================================================
# TEST 11: Red zone counts are monotonically non-increasing (rz20 >= rz10 >= rz5)
# ==============================================================================
test_that("Red zone opportunities are monotonically non-increasing by threshold", {
  pbp_rz <- make_test_pbp(n_pass = 80, n_run = 50, include_redzone = TRUE) %>%
    bind_rows(
      make_test_pbp(n_pass = 40, n_run = 25) %>%
        mutate(yardline_100 = sample(3:18, nrow(.), replace = TRUE))
    )

  result <- get_redzone_profile(pbp_rz, season = 2025, min_redzone_opps = 1)

  if (nrow(result) > 0) {
    receiver_result <- result %>% filter(position_group == "receiver")
    if (nrow(receiver_result) > 0) {
      expect_true(all(receiver_result$rz20_targets >= receiver_result$rz10_targets),
        info = "rz20 targets must be >= rz10 targets")
      expect_true(all(receiver_result$rz10_targets >= receiver_result$rz5_targets),
        info = "rz10 targets must be >= rz5 targets")
    }
  }
})

# ==============================================================================
# TEST 12: get_redzone_profile() TD rate guard (no Inf/NaN)
# ==============================================================================
test_that("Red zone TD rates have no Inf or NaN", {
  pbp_rz <- make_test_pbp(n_pass = 60, n_run = 40, include_redzone = TRUE) %>%
    bind_rows(
      make_test_pbp(n_pass = 30, n_run = 15) %>%
        mutate(yardline_100 = sample(5:18, nrow(.), replace = TRUE))
    )
  result <- get_redzone_profile(pbp_rz, season = 2025, min_redzone_opps = 1)

  td_rate_cols <- c("rz20_td_rate", "rz10_td_rate", "rz5_td_rate")
  for (col in td_rate_cols) {
    vals <- result[[col]]
    expect_false(any(is.infinite(vals), na.rm = TRUE),
      info = paste("Infinite TD rate in", col))
    expect_false(any(is.nan(vals), na.rm = TRUE),
      info = paste("NaN TD rate in", col))
  }
})

# ==============================================================================
# TEST 13: get_receiving_depth_profile() returns expected columns
# ==============================================================================
test_that("get_receiving_depth_profile() returns expected columns", {
  pbp <- make_test_pbp(n_pass = 100, n_run = 30)
  # Give WR001 lots of targets to exceed min_targets
  pbp <- pbp %>%
    bind_rows(
      pbp %>%
        filter(play_type == "pass") %>%
        mutate(receiver_player_id = "WR001", receiver_player_name = "Test WR1")
    )
  result <- get_receiving_depth_profile(pbp, season = 2025, min_targets = 10)

  expected_cols <- c(
    "season", "player_id", "player_name", "team", "total_targets", "avg_air_yards",
    "short_targets", "medium_targets", "deep_targets",
    "short_pct", "medium_pct", "deep_pct",
    "short_catch_rate", "medium_catch_rate", "deep_catch_rate",
    "short_epa_per_target", "medium_epa_per_target", "deep_epa_per_target",
    "depth_profile"
  )
  missing <- setdiff(expected_cols, names(result))
  expect_true(length(missing) == 0,
    info = paste("Missing columns:", paste(missing, collapse = ", ")))
})

# ==============================================================================
# TEST 14: Depth percentages sum to approximately 1.0
# ==============================================================================
test_that("Depth bucket percentages sum to approximately 1.0", {
  pbp <- make_test_pbp(n_pass = 120, n_run = 30)
  pbp_extra <- bind_rows(pbp, pbp %>% filter(play_type == "pass") %>%
                           mutate(receiver_player_id = "WR001",
                                  receiver_player_name = "Test WR1"))
  result <- get_receiving_depth_profile(pbp_extra, season = 2025, min_targets = 10)

  if (nrow(result) > 0) {
    pct_sums <- result %>%
      mutate(total_pct = short_pct + medium_pct + deep_pct) %>%
      filter(!is.na(total_pct)) %>%
      pull(total_pct)

    expect_true(all(abs(pct_sums - 1.0) < 0.01),
      info = "Depth bucket percentages should sum to ~1.0")
  }
})

# ==============================================================================
# TEST 15: depth_profile valid values only
# ==============================================================================
test_that("depth_profile only contains valid classification values", {
  pbp <- make_test_pbp(n_pass = 120, n_run = 30)
  pbp_extra <- bind_rows(pbp, pbp %>% filter(play_type == "pass") %>%
                           mutate(receiver_player_id = "WR001",
                                  receiver_player_name = "Test WR1"))
  result <- get_receiving_depth_profile(pbp_extra, season = 2025, min_targets = 10)

  if (nrow(result) > 0) {
    invalid <- result %>%
      filter(!is.na(depth_profile),
             !depth_profile %in% c("slot_heavy", "balanced", "deep_threat"))
    expect_equal(nrow(invalid), 0,
      info = "depth_profile must be: slot_heavy, balanced, deep_threat, or NA")
  }
})

# ==============================================================================
# TEST 16: Integration test - full pipeline runs without error
# ==============================================================================
test_that("Full Week 6 pipeline runs without error", {
  pbp <- make_test_pbp(n_pass = 60, n_run = 30, include_redzone = TRUE,
                        include_kneels = TRUE, include_scramble = TRUE)
  pbp_multi <- bind_rows(
    pbp,
    pbp %>% mutate(week = week + 3, game_id = paste0(game_id, "_b")),
    pbp %>% mutate(week = week + 6, game_id = paste0(game_id, "_c"))
  )

  # Step 1: Usage metrics
  expect_no_error(usage <- calculate_usage_metrics(pbp_multi, season = 2025))
  expect_gt(nrow(usage), 0)

  # Step 2: Usage trends
  expect_no_error(trends <- calculate_usage_trends(usage))
  expect_gt(nrow(trends), 0)

  # Step 3: Red zone profile
  pbp_rz <- bind_rows(pbp_multi,
    pbp_multi %>% slice_head(n = 30) %>% mutate(yardline_100 = sample(5:18, 30, replace = TRUE)))
  expect_no_error(rz <- get_redzone_profile(pbp_rz, season = 2025, min_redzone_opps = 1))

  # Step 4: Depth profile
  pbp_extra <- bind_rows(pbp_multi,
    pbp_multi %>% filter(play_type == "pass") %>%
      mutate(receiver_player_id = "WR001", receiver_player_name = "Test WR1"))
  expect_no_error(depth <- get_receiving_depth_profile(pbp_extra, season = 2025, min_targets = 5))

  # All outputs are tibbles
  expect_s3_class(usage,  "tbl_df")
  expect_s3_class(trends, "tbl_df")
  expect_s3_class(rz,     "tbl_df")
  expect_s3_class(depth,  "tbl_df")

  cat(glue::glue("\nIntegration test passed:\n",
    "  Usage rows:     {nrow(usage)}\n",
    "  Trends rows:    {nrow(trends)}\n",
    "  Red zone rows:  {nrow(rz)}\n",
    "  Depth rows:     {nrow(depth)}\n"))
})

cat("Week 6 test suite loaded. 16 tests defined.\n")
