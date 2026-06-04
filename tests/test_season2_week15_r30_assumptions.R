# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# R/30 Team Volume Projections -- Assumption Tests
# File: tests/test_season2_week15_r30_assumptions.R
#
# PURPOSE
# -------
# Standalone assumption-validation script for R/30. Not a testthat suite.
# Emits PASS / WARN / FAIL per check and writes a log to
# output/assumption_tests/assumptions_season2_week15_r30.txt.
#
# CHECKS
# ------
#   1. MIN_GAMES_TEAM_SEASON = 14 met by each active team in each season of
#      the 2023-2025 historical window. Validates that no team-season is at
#      risk of exclusion from R/30's 3-year average.
#      Source: nflreadr::load_schedules() -- no pbp load required.
#
#   2. MIN_DROPBACKS_QB = 200 met by the top QB on each team in the prior
#      season (2025). Teams below this threshold get a neutral QB quality
#      score (0) in R/30. Acceptable floor: >= 29 of 32 teams qualify.
#      Source: 2025 pbp via load_normalized_season().
#
#   3. QB quality z-scores non-degenerate. Validates that CPOE and EPA/
#      dropback have non-trivial variance among qualifying 2025 starters so
#      R/30's standardization produces real signal rather than all-zero scores.
#      Source: same 2025 pbp load as CHECK 2.
#
#   4. No structural break in 3-year historical window. Verifies game-count
#      consistency across 2023-2025 (all 17-game seasons, all active teams
#      present). Surfaces R/30 team volume output league distribution as
#      informational for manual inspection.
#      Source: nflreadr::load_schedules() + R/30 team_volumes RDS.
#
# DESIGN NOTES
# ------------
#   - CHECKs 2 and 3 share one 2025 pbp load. rm/gc after the shared section.
#   - CHECKs 1 and 4 use nflreadr::load_schedules() -- much lighter than pbp.
#   - R/30 team_volumes RDS required for CHECK 4 informational section.
#     If not present, CHECK 4 reports what it can from schedules only.
#   - Mirrors R/30's .compute_qb_quality_index() logic for CHECKs 2 and 3
#     so the assumption validates the actual intermediate computation path.
#
# USAGE
#   source(here::here("tests", "test_season2_week15_r30_assumptions.R"))
#
# Author: Christian K. LeBlanc
# Version: 1.0
# ==============================================================================

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(here)
library(glue)
library(nflreadr)

# Sourcing R/30 makes available: MIN_GAMES_TEAM_SEASON, MIN_DROPBACKS_QB,
# HISTORICAL_SEASONS, ACTIVE_TEAMS_2026, SEASON, CACHE_DIR_DEFAULT,
# OUTPUT_RDS_PATH, .normalize_team_codes(), load_normalized_season()
source(here::here("R", "30_team_volume_projections.R"))

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------

PRIOR_SEASON     <- SEASON - 1L              # 2025: prior season for QB quality
TEAM_VOL_RDS     <- OUTPUT_RDS_PATH          # s2_week15_team_volumes.rds from R/30

LOG_DIR_R30  <- here::here("output", "assumption_tests")
LOG_FILE_R30 <- file.path(LOG_DIR_R30, "assumptions_season2_week15_r30.txt")

# Game count thresholds (CHECK 1 and CHECK 4)
GAMES_PASS_THRESHOLD <- 17L   # All 2023-2025 are 17-game seasons
GAMES_WARN_THRESHOLD <- 14L   # R/30 exclusion floor

# QB dropback coverage thresholds (CHECK 2)
QB_COVERAGE_PASS <- 29L   # >= 29 of 32 teams with qualifying starter
QB_COVERAGE_WARN <- 25L   # 25-28: some teams defaulted to neutral

# QB z-score degeneracy thresholds (CHECK 3)
CPOE_SD_MIN  <- 0.005   # Any meaningful CPOE spread in the QB pool
EPA_SD_MIN   <- 0.001   # Any meaningful EPA spread

# Structural break relative deviation (CHECK 4)
BREAK_WARN_REL <- 0.05  # 5% relative from 3-year mean = WARN

dir.create(LOG_DIR_R30, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# LOGGING / STATE
# ------------------------------------------------------------------------------

.r30_state <- new.env(parent = emptyenv())
.r30_state$log_lines <- character(0)
.r30_state$results   <- list()

log_line_r30 <- function(...) {
  msg <- paste0(...)
  message(msg)
  .r30_state$log_lines <- c(.r30_state$log_lines, msg)
}

log_header_r30 <- function(title) {
  log_line_r30(strrep("=", 70))
  log_line_r30(title)
  log_line_r30(strrep("=", 70))
}

log_blank_r30 <- function() log_line_r30("")

record_result_r30 <- function(check_name, status, details = NULL) {
  .r30_state$results[[check_name]] <- list(status = status, details = details)
}

run_check_r30 <- function(check_name, check_fn) {
  tryCatch(
    check_fn(),
    error = function(e) {
      log_line_r30(glue("  UNEXPECTED ERROR: {e$message}"))
      log_line_r30("  Result: FAIL")
      log_blank_r30()
      record_result_r30(check_name, "FAIL", list(error = e$message))
    }
  )
}

# ==============================================================================
# CHECK 1: MIN_GAMES_TEAM_SEASON = 14 met per team per season
# ==============================================================================

check_games_per_season <- function() {
  log_header_r30(
    "CHECK 1: MIN_GAMES_TEAM_SEASON = 14 met per active team per season"
  )
  log_line_r30(glue("  Historical window: {paste(HISTORICAL_SEASONS, collapse = ', ')}"))
  log_line_r30(glue("  Threshold:         {MIN_GAMES_TEAM_SEASON} games"))
  log_blank_r30()

  schedules_raw <- tryCatch(
    nflreadr::load_schedules(seasons = HISTORICAL_SEASONS),
    error = function(e) {
      log_line_r30(glue("  load_schedules() failed: {e$message}"))
      NULL
    }
  )

  if (is.null(schedules_raw) || nrow(schedules_raw) == 0L) {
    log_line_r30("  No schedule data returned.")
    log_line_r30("  Result: SKIP")
    log_blank_r30()
    record_result_r30("games_per_season", "SKIP")
    return(invisible())
  }

  # Keep regular season only; normalize team codes
  reg <- schedules_raw %>%
    dplyr::filter(.data$game_type == "REG")

  # Count appearances as home or away -- each = one game played
  home <- reg %>%
    dplyr::transmute(
      season = .data$season,
      team   = .normalize_team_codes(as.character(.data$home_team))
    )
  away <- reg %>%
    dplyr::transmute(
      season = .data$season,
      team   = .normalize_team_codes(as.character(.data$away_team))
    )

  team_games <- dplyr::bind_rows(home, away) %>%
    dplyr::filter(.data$team %in% ACTIVE_TEAMS_2026) %>%
    dplyr::group_by(.data$season, .data$team) %>%
    dplyr::summarise(n_games = dplyr::n(), .groups = "drop")

  # Summary per season
  for (s in sort(unique(team_games$season))) {
    sg <- team_games %>% dplyr::filter(.data$season == s)
    n_teams       <- nrow(sg)
    n_below_14    <- sum(sg$n_games < MIN_GAMES_TEAM_SEASON)
    n_below_17    <- sum(sg$n_games < GAMES_PASS_THRESHOLD)
    n_at_17       <- sum(sg$n_games == GAMES_PASS_THRESHOLD)
    log_line_r30(glue(
      "  {s}: {n_teams} teams, {n_at_17} at 17 games, ",
      "{n_below_17} below 17, {n_below_14} below {MIN_GAMES_TEAM_SEASON}"
    ))
  }
  log_blank_r30()

  # Identify any team-seasons below threshold
  at_risk <- team_games %>%
    dplyr::filter(.data$n_games < MIN_GAMES_TEAM_SEASON)
  below_17 <- team_games %>%
    dplyr::filter(.data$n_games < GAMES_PASS_THRESHOLD,
                   .data$n_games >= MIN_GAMES_TEAM_SEASON)

  if (nrow(at_risk) > 0L) {
    log_line_r30("  Teams BELOW exclusion threshold (< 14 games -- excluded from R/30):")
    for (i in seq_len(nrow(at_risk))) {
      r <- at_risk[i, ]
      log_line_r30(glue("    {r$season} {r$team}: {r$n_games} games"))
    }
    log_blank_r30()
  }

  if (nrow(below_17) > 0L) {
    log_line_r30("  Teams with partial season (14-16 games -- included but partial):")
    for (i in seq_len(nrow(below_17))) {
      r <- below_17[i, ]
      log_line_r30(glue("    {r$season} {r$team}: {r$n_games} games"))
    }
    log_blank_r30()
  }

  status <- if (nrow(at_risk) > 0L) "FAIL" else
    if (nrow(below_17) > 0L)        "WARN" else "PASS"

  log_line_r30(glue("  Result: {status}"))
  log_blank_r30()

  record_result_r30("games_per_season", status, list(
    n_at_risk  = nrow(at_risk),
    n_below_17 = nrow(below_17)
  ))
}

# ==============================================================================
# SHARED PBP LOAD (CHECKs 2 and 3)
# ==============================================================================

.load_2025_qb_aggregates <- function() {

  message(glue("  Loading {PRIOR_SEASON} pbp for QB quality checks..."))

  pbp <- tryCatch(
    load_normalized_season(PRIOR_SEASON, cache_dir = CACHE_DIR_DEFAULT),
    error = function(e) {
      message(glue("  {PRIOR_SEASON} pbp load failed: {e$message}"))
      NULL
    }
  )

  if (is.null(pbp) || nrow(pbp) == 0L) return(NULL)

  pbp_qb <- pbp %>%
    dplyr::filter(
      .data$season_type == "REG",
      !is.na(.data$posteam),
      !is.na(.data$passer_player_id),
      dplyr::coalesce(.data$qb_dropback,         0L) == 1L,
      dplyr::coalesce(.data$qb_kneel,            0L) == 0L,
      dplyr::coalesce(.data$qb_spike,            0L) == 0L,
      dplyr::coalesce(.data$two_point_attempt,   0L) == 0L
    ) %>%
    dplyr::mutate(
      posteam = .normalize_team_codes(.data$posteam)
    )

  rm(pbp)
  invisible(gc(verbose = FALSE))

  if (nrow(pbp_qb) == 0L) return(NULL)

  # QB-team aggregation
  qb_agg <- pbp_qb %>%
    dplyr::group_by(team = .data$posteam,
                     qb_id = .data$passer_player_id) %>%
    dplyr::summarise(
      n_dropbacks  = dplyr::n(),
      cpoe_mean    = mean(.data$cpoe, na.rm = TRUE),
      epa_per_db   = mean(.data$epa,  na.rm = TRUE),
      .groups      = "drop"
    )

  rm(pbp_qb)
  invisible(gc(verbose = FALSE))

  # Top QB per team: highest dropback count
  top_qb <- qb_agg %>%
    dplyr::group_by(.data$team) %>%
    dplyr::slice_max(.data$n_dropbacks, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$team %in% ACTIVE_TEAMS_2026)

  list(all_qb = qb_agg, top_qb = top_qb)
}

# ==============================================================================
# CHECK 2: MIN_DROPBACKS_QB = 200 -- qualifying QB coverage
# ==============================================================================

check_qb_dropback_coverage <- function(qb_data) {
  log_header_r30(
    "CHECK 2: MIN_DROPBACKS_QB = 200 -- qualifying QB coverage in 2025"
  )

  if (is.null(qb_data)) {
    log_line_r30("  2025 pbp unavailable.")
    log_line_r30("  Result: SKIP")
    log_blank_r30()
    record_result_r30("qb_dropback_coverage", "SKIP")
    return(invisible())
  }

  top_qb <- qb_data$top_qb

  qualifying  <- top_qb %>% dplyr::filter(.data$n_dropbacks >= MIN_DROPBACKS_QB)
  not_qualify <- top_qb %>% dplyr::filter(.data$n_dropbacks <  MIN_DROPBACKS_QB)
  missing     <- setdiff(ACTIVE_TEAMS_2026, top_qb$team)

  n_qualify   <- nrow(qualifying)
  n_not       <- nrow(not_qualify)
  n_missing   <- length(missing)
  n_neutral   <- n_not + n_missing

  log_line_r30(glue("  Prior season: {PRIOR_SEASON}"))
  log_line_r30(glue("  Dropback threshold:  {MIN_DROPBACKS_QB}"))
  log_line_r30(glue("  Active teams:        {length(ACTIVE_TEAMS_2026)}"))
  log_line_r30(glue("  Qualifying (>={MIN_DROPBACKS_QB} dropbacks): {n_qualify}"))
  log_line_r30(glue("  Below threshold:     {n_not}"))
  log_line_r30(glue("  No pbp record:       {n_missing}"))
  log_line_r30(glue("  Teams defaulted to neutral quality: {n_neutral}"))
  log_blank_r30()

  if (n_not > 0L) {
    log_line_r30("  Teams below threshold (top QB dropbacks):")
    for (i in seq_len(nrow(not_qualify))) {
      r <- not_qualify[i, ]
      log_line_r30(glue("    {r$team}: {r$n_dropbacks} dropbacks"))
    }
    log_blank_r30()
  }

  if (n_missing > 0L) {
    log_line_r30(glue(
      "  Teams with no pbp record: {paste(missing, collapse = ', ')}"
    ))
    log_blank_r30()
  }

  # Dropback distribution across all top QBs
  log_line_r30("  Dropback distribution (qualifying starters only):")
  log_line_r30(glue(
    "    min={min(qualifying$n_dropbacks)}, ",
    "median={round(stats::median(qualifying$n_dropbacks))}, ",
    "max={max(qualifying$n_dropbacks)}"
  ))
  log_blank_r30()

  status <- if (n_qualify >= QB_COVERAGE_PASS) "PASS" else
    if (n_qualify >= QB_COVERAGE_WARN)          "WARN" else "FAIL"

  log_line_r30(glue("  PASS threshold: >= {QB_COVERAGE_PASS} teams qualifying"))
  log_line_r30(glue("  WARN threshold: >= {QB_COVERAGE_WARN} teams qualifying"))
  log_line_r30(glue("  Result: {status}"))
  log_blank_r30()

  record_result_r30("qb_dropback_coverage", status, list(
    n_qualify = n_qualify,
    n_neutral = n_neutral
  ))
}

# ==============================================================================
# CHECK 3: QB quality z-scores non-degenerate
# ==============================================================================

check_qb_zscore_degeneracy <- function(qb_data) {
  log_header_r30("CHECK 3: QB quality z-scores non-degenerate")

  if (is.null(qb_data)) {
    log_line_r30("  2025 pbp unavailable.")
    log_line_r30("  Result: SKIP")
    log_blank_r30()
    record_result_r30("qb_zscore_degeneracy", "SKIP")
    return(invisible())
  }

  qualifying <- qb_data$top_qb %>%
    dplyr::filter(.data$n_dropbacks >= MIN_DROPBACKS_QB)

  n_qualifying <- nrow(qualifying)

  log_line_r30(glue("  Qualifying starters: {n_qualifying}"))
  log_blank_r30()

  if (n_qualifying < 10L) {
    log_line_r30("  Fewer than 10 qualifying starters -- z-scores not meaningful.")
    log_line_r30("  Result: FAIL")
    log_blank_r30()
    record_result_r30("qb_zscore_degeneracy", "FAIL",
                       list(n_qualifying = n_qualifying))
    return(invisible())
  }

  cpoe_mean_val <- mean(qualifying$cpoe_mean, na.rm = TRUE)
  cpoe_sd_val   <- stats::sd(qualifying$cpoe_mean, na.rm = TRUE)
  epa_mean_val  <- mean(qualifying$epa_per_db, na.rm = TRUE)
  epa_sd_val    <- stats::sd(qualifying$epa_per_db, na.rm = TRUE)

  cpoe_sd_ok <- !is.na(cpoe_sd_val) && cpoe_sd_val > CPOE_SD_MIN
  epa_sd_ok  <- !is.na(epa_sd_val)  && epa_sd_val  > EPA_SD_MIN

  log_line_r30("  Pre-standardization CPOE distribution:")
  log_line_r30(glue(
    "    mean={format(round(cpoe_mean_val, 4), nsmall = 4)}, ",
    "sd={format(round(cpoe_sd_val, 4), nsmall = 4)} -- ",
    "{if (cpoe_sd_ok) 'non-degenerate' else 'DEGENERATE (would trigger fallback)'}"
  ))

  log_line_r30("  Pre-standardization EPA per dropback distribution:")
  log_line_r30(glue(
    "    mean={format(round(epa_mean_val, 4), nsmall = 4)}, ",
    "sd={format(round(epa_sd_val, 4), nsmall = 4)} -- ",
    "{if (epa_sd_ok) 'non-degenerate' else 'DEGENERATE (would trigger fallback)'}"
  ))
  log_blank_r30()

  # Compute z-scores the same way R/30 does
  cpoe_sd_use <- if (cpoe_sd_ok) cpoe_sd_val else 1.0
  epa_sd_use  <- if (epa_sd_ok)  epa_sd_val  else 1.0

  qualifying_z <- qualifying %>%
    dplyr::mutate(
      cpoe_z           = (.data$cpoe_mean  - cpoe_mean_val) / cpoe_sd_use,
      epa_z            = (.data$epa_per_db - epa_mean_val)  / epa_sd_use,
      qb_quality_score = (.data$cpoe_z + .data$epa_z) / 2
    )

  # qb_quality_score distribution (informational)
  qs_mean <- mean(qualifying_z$qb_quality_score, na.rm = TRUE)
  qs_sd   <- stats::sd(qualifying_z$qb_quality_score, na.rm = TRUE)
  qs_min  <- min(qualifying_z$qb_quality_score, na.rm = TRUE)
  qs_max  <- max(qualifying_z$qb_quality_score, na.rm = TRUE)

  log_line_r30("  qb_quality_score distribution (informational):")
  log_line_r30(glue(
    "    mean={format(round(qs_mean, 3), nsmall = 3)}, ",
    "sd={format(round(qs_sd, 3), nsmall = 3)}, ",
    "range=[{format(round(qs_min, 2), nsmall = 2)}, ",
    "{format(round(qs_max, 2), nsmall = 2)}]"
  ))
  log_line_r30(
    "  NOTE: qb_quality_score is the average of two unit z-scores."
  )
  log_line_r30(
    "  SD is expected between 0.5 and 1.0 depending on CPOE/EPA correlation."
  )
  log_blank_r30()

  status <- if (!cpoe_sd_ok || !epa_sd_ok) "FAIL" else "PASS"
  log_line_r30(glue("  Result: {status}"))
  log_blank_r30()

  record_result_r30("qb_zscore_degeneracy", status, list(
    n_qualifying = n_qualifying,
    cpoe_sd      = cpoe_sd_val,
    epa_sd       = epa_sd_val,
    qs_sd        = qs_sd
  ))
}

# ==============================================================================
# CHECK 4: No structural break -- season structure + R/30 volume distribution
# ==============================================================================

check_structural_break <- function() {
  log_header_r30(
    "CHECK 4: No structural break in 2023-2025 historical window"
  )
  log_blank_r30()

  # Part A: game-count consistency across seasons (load_schedules)
  log_line_r30("  Part A: Season structure (game count consistency)")

  schedules_raw <- tryCatch(
    nflreadr::load_schedules(seasons = HISTORICAL_SEASONS),
    error = function(e) {
      log_line_r30(glue("  load_schedules() failed: {e$message}"))
      NULL
    }
  )

  if (is.null(schedules_raw)) {
    log_line_r30("  Cannot validate season structure.")
  } else {
    reg <- schedules_raw %>% dplyr::filter(.data$game_type == "REG")

    season_counts <- reg %>%
      dplyr::group_by(.data$season) %>%
      dplyr::summarise(
        n_games        = dplyr::n(),
        n_distinct_teams = dplyr::n_distinct(c(.data$home_team, .data$away_team)),
        .groups = "drop"
      )

    for (i in seq_len(nrow(season_counts))) {
      r <- season_counts[i, ]
      expected_games <- 32L * 17L / 2L  # 272 games in a 17-game season
      games_ok <- abs(r$n_games - expected_games) <= 2L  # allow 2-game slack
      log_line_r30(glue(
        "    {r$season}: {r$n_games} games, {r$n_distinct_teams} distinct teams ",
        "-- {if (games_ok) 'consistent' else 'INCONSISTENT'}"
      ))
    }

    # Check per-team game counts for missing or short seasons
    home <- reg %>%
      dplyr::transmute(
        season = .data$season,
        team   = .normalize_team_codes(as.character(.data$home_team))
      )
    away <- reg %>%
      dplyr::transmute(
        season = .data$season,
        team   = .normalize_team_codes(as.character(.data$away_team))
      )
    team_games <- dplyr::bind_rows(home, away) %>%
      dplyr::filter(.data$team %in% ACTIVE_TEAMS_2026) %>%
      dplyr::group_by(.data$season, .data$team) %>%
      dplyr::summarise(n_games = dplyr::n(), .groups = "drop")

    # Any active team missing from any season entirely?
    expected_combos <- tidyr::crossing(
      season = HISTORICAL_SEASONS,
      team   = ACTIVE_TEAMS_2026
    )
    missing_combos <- dplyr::anti_join(expected_combos, team_games,
                                        by = c("season", "team"))
    n_missing <- nrow(missing_combos)

    # Any team below MIN_GAMES_TEAM_SEASON?
    n_below_floor <- sum(team_games$n_games < MIN_GAMES_TEAM_SEASON)

    log_blank_r30()
    log_line_r30(glue("    Active team-season combinations missing: {n_missing}"))
    log_line_r30(glue(
      "    Team-seasons below floor ({MIN_GAMES_TEAM_SEASON} games):  {n_below_floor}"
    ))
  }

  log_blank_r30()

  # Part B: R/30 pass/rush volume distribution (informational)
  log_line_r30("  Part B: R/30 historical volume distribution (informational)")

  if (!file.exists(TEAM_VOL_RDS)) {
    log_line_r30(glue("  Team volumes RDS not found at: {TEAM_VOL_RDS}"))
    log_line_r30("  (Run project_team_volumes() to generate)")
  } else {
    vols <- readRDS(TEAM_VOL_RDS)

    if (all(c("historical_pass_pg", "historical_rush_pg") %in% names(vols))) {
      pass_mean   <- mean(vols$historical_pass_pg, na.rm = TRUE)
      pass_sd     <- stats::sd(vols$historical_pass_pg, na.rm = TRUE)
      pass_min    <- min(vols$historical_pass_pg, na.rm = TRUE)
      pass_max    <- max(vols$historical_pass_pg, na.rm = TRUE)
      rush_mean   <- mean(vols$historical_rush_pg, na.rm = TRUE)
      rush_sd     <- stats::sd(vols$historical_rush_pg, na.rm = TRUE)
      rush_min    <- min(vols$historical_rush_pg, na.rm = TRUE)
      rush_max    <- max(vols$historical_rush_pg, na.rm = TRUE)

      log_line_r30(glue(
        "    historical_pass_pg: ",
        "mean={format(round(pass_mean, 1), nsmall = 1)}, ",
        "sd={format(round(pass_sd, 1), nsmall = 1)}, ",
        "range=[{format(round(pass_min, 1), nsmall = 1)}, ",
        "{format(round(pass_max, 1), nsmall = 1)}]"
      ))
      log_line_r30(glue(
        "    historical_rush_pg: ",
        "mean={format(round(rush_mean, 1), nsmall = 1)}, ",
        "sd={format(round(rush_sd, 1), nsmall = 1)}, ",
        "range=[{format(round(rush_min, 1), nsmall = 1)}, ",
        "{format(round(rush_max, 1), nsmall = 1)}]"
      ))
      log_blank_r30()

      # Check for any team whose historical_pass_pg deviates > 5% from league mean
      outlier_pass <- vols %>%
        dplyr::mutate(
          rel_dev = abs(.data$historical_pass_pg - pass_mean) / pass_mean
        ) %>%
        dplyr::filter(.data$rel_dev > BREAK_WARN_REL) %>%
        dplyr::arrange(dplyr::desc(.data$rel_dev))

      if (nrow(outlier_pass) > 0L) {
        log_line_r30(glue(
          "    Teams with historical_pass_pg >5% from league mean ({format(round(pass_mean, 1), nsmall = 1)}):"
        ))
        for (i in seq_len(nrow(outlier_pass))) {
          r <- outlier_pass[i, ]
          log_line_r30(glue(
            "      {r$team}: {format(round(r$historical_pass_pg, 1), nsmall = 1)} ",
            "({format(round(r$rel_dev * 100, 1), nsmall = 1)}% deviation)"
          ))
        }
      } else {
        log_line_r30(
          "    All teams within 5% of league mean pass volume. No outliers."
        )
      }
    }
  }

  log_blank_r30()

  # Overall status based on structural checks
  if (is.null(schedules_raw)) {
    status <- "SKIP"
  } else if (n_missing > 0L || n_below_floor > 0L) {
    status <- "FAIL"
  } else {
    status <- "PASS"
  }

  log_line_r30(glue("  Result: {status}"))
  log_blank_r30()

  record_result_r30("structural_break", status, list(
    n_missing    = if (exists("n_missing"))    n_missing    else NA,
    n_below_floor = if (exists("n_below_floor")) n_below_floor else NA
  ))
}

# ==============================================================================
# RUN ALL CHECKS
# ==============================================================================

log_header_r30("R/30 Team Volume Projections -- Assumption Tests")
log_line_r30(glue(
  "Generated: {format(Sys.time(), '%Y-%m-%d %H:%M:%S %Z')}"
))
log_line_r30(glue("Source script: R/30_team_volume_projections.R"))
log_line_r30(glue("Historical window: {paste(HISTORICAL_SEASONS, collapse = ', ')}"))
log_line_r30(glue("Prior season (QB quality): {PRIOR_SEASON}"))
log_line_r30(glue("Team volumes output: {TEAM_VOL_RDS}"))
log_blank_r30()

# Load 2025 pbp once -- shared by CHECKs 2 and 3
log_line_r30("Loading 2025 pbp for QB quality checks (shared by CHECKs 2 and 3)...")
log_blank_r30()
qb_data <- tryCatch(
  .load_2025_qb_aggregates(),
  error = function(e) {
    message(glue("QB aggregate load failed: {e$message}"))
    NULL
  }
)

run_check_r30("games_per_season",      check_games_per_season)
run_check_r30("qb_dropback_coverage",  function() check_qb_dropback_coverage(qb_data))
run_check_r30("qb_zscore_degeneracy",  function() check_qb_zscore_degeneracy(qb_data))
run_check_r30("structural_break",      check_structural_break)

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------

log_header_r30("SUMMARY")

n_total <- length(.r30_state$results)
n_pass  <- sum(vapply(.r30_state$results,
                       function(r) r$status == "PASS", logical(1)))
n_warn  <- sum(vapply(.r30_state$results,
                       function(r) r$status == "WARN", logical(1)))
n_fail  <- sum(vapply(.r30_state$results,
                       function(r) r$status == "FAIL", logical(1)))
n_skip  <- sum(vapply(.r30_state$results,
                       function(r) r$status == "SKIP", logical(1)))

log_line_r30(glue("  Total checks:  {n_total}"))
log_line_r30(glue("  PASS:          {n_pass}"))
log_line_r30(glue("  WARN:          {n_warn}"))
log_line_r30(glue("  FAIL:          {n_fail}"))
log_line_r30(glue("  SKIP:          {n_skip}"))

log_blank_r30()
log_line_r30("  By check:")
for (check_name in names(.r30_state$results)) {
  log_line_r30(glue(
    "    {check_name}: {.r30_state$results[[check_name]]$status}"
  ))
}

log_line_r30(strrep("=", 70))

# ------------------------------------------------------------------------------
# WRITE LOG TO FILE
# ------------------------------------------------------------------------------

writeLines(.r30_state$log_lines, LOG_FILE_R30)
message(glue("\nAssumption test log written to:\n  {LOG_FILE_R30}"))
