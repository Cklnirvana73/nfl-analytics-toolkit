# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Test Suite: R/29 Projection Engine
# File: tests/test_season2_week15_r29_functions.R
#
# COVERAGE
# --------
#   .normalize_sleeper_team_codes  -- crosswalk correctness, NA passthrough
#   .merge_sleeper_roster          -- empty Sleeper, FA flag, Sleeper team wins,
#                                     nflreadr fallback, AZ->ARI regression fix
#   .integrate_sleeper_rookies     -- Pass A (gsis_id), Pass B (name+pos),
#                                     FA filter, position filter, empty input
#   .build_active_roster_filter    -- empty input, row drops, column renames
#
# DEPENDENCIES
# ------------
#   R/19_sleeper_api.R  (defines get_all_sleeper_players -- sourced before R/29)
#   R/29_projection_engine.R
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
  library(glue)
})

suppressPackageStartupMessages({
  source(here::here("R", "19_sleeper_api.R"))
  source(here::here("R", "29_projection_engine.R"))
})


# ==============================================================================
# FIXTURE BUILDERS
# ==============================================================================

make_nflreadr_roster <- function(gsis_ids = c("00-001", "00-002", "00-003"),
                                  teams    = "KC",
                                  names    = NULL,
                                  positions = NULL,
                                  status   = NULL) {
  n <- length(gsis_ids)
  if (length(teams) == 1L) teams <- rep(teams, n)
  if (is.null(names))     names <- paste("Player", seq_len(n))
  if (is.null(positions)) positions <- rep("WR", n)
  if (is.null(status))    status <- rep("ACT", n)
  tibble::tibble(
    gsis_id   = gsis_ids,
    full_name = names,
    team      = teams,
    position  = positions,
    status    = status
  )
}

make_sleeper_data <- function(gsis_ids    = c("00-001", "00-002"),
                               teams       = "KC",
                               positions   = "WR",
                               is_fa       = FALSE,
                               names       = NULL,
                               sleeper_ids = NULL,
                               depth_order = NULL,
                               status      = NULL) {
  n <- length(gsis_ids)
  if (length(teams) == 1L)     teams <- rep(teams, n)
  if (length(positions) == 1L) positions <- rep(positions, n)
  if (length(is_fa) == 1L)     is_fa <- rep(is_fa, n)
  if (is.null(names))          names <- paste("Sleeper Player", seq_len(n))
  if (is.null(sleeper_ids))    sleeper_ids <- paste0("sl_", seq_len(n))
  if (is.null(depth_order))    depth_order <- rep(NA_integer_, n)
  if (is.null(status))         status <- rep("Active", n)
  tibble::tibble(
    sleeper_player_id    = sleeper_ids,
    nfl_gsis_id          = gsis_ids,
    player_name          = names,
    position             = positions,
    team                 = teams,
    status               = status,
    injury_status        = rep(NA_character_, n),
    depth_chart_position = positions,
    depth_chart_order    = depth_order,
    years_exp            = rep(2L, n),
    age                  = rep(25L, n),
    is_free_agent        = is_fa
  )
}

make_prospects_fixture <- function(gsis_ids  = c("00-901", "00-902"),
                                    names     = c("Rookie One", "Rookie Two"),
                                    positions = c("WR", "RB")) {
  n <- length(gsis_ids)
  tibble::tibble(
    cfb_player_name = names,
    nfl_gsis_id     = gsis_ids,
    position        = positions,
    score_final     = rep(85.0, n),
    score_v1        = rep(80.0, n)
  )
}


# ==============================================================================
# .normalize_sleeper_team_codes
# ==============================================================================

test_that("[R/29] .normalize_sleeper_team_codes maps all four crosswalk entries", {
  input  <- c("LAR", "AZ", "JAC", "WSH")
  output <- .normalize_sleeper_team_codes(input)
  expect_equal(output, c("LA", "ARI", "JAX", "WAS"))
})

test_that("[R/29] .normalize_sleeper_team_codes leaves unknown codes unchanged", {
  input  <- c("KC", "BUF", "ARI", "LA")
  output <- .normalize_sleeper_team_codes(input)
  expect_equal(output, input)
})

test_that("[R/29] .normalize_sleeper_team_codes handles NA passthrough", {
  input  <- c("AZ", NA, "LAR", NA_character_)
  output <- .normalize_sleeper_team_codes(input)
  expect_equal(output, c("ARI", NA, "LA", NA_character_))
})


# ==============================================================================
# .merge_sleeper_roster
# ==============================================================================

test_that("[R/29] .merge_sleeper_roster returns roster unchanged when Sleeper is empty", {
  roster    <- make_nflreadr_roster()
  out_null  <- .merge_sleeper_roster(roster, NULL)
  out_empty <- .merge_sleeper_roster(roster, make_sleeper_data()[0, ])
  expect_equal(out_null, roster)
  expect_equal(out_empty, roster)
})

test_that("[R/29] .merge_sleeper_roster sets team to NA when Sleeper flags free agent", {
  roster <- make_nflreadr_roster(
    gsis_ids = c("00-001", "00-002"),
    teams    = c("KC", "BUF")
  )
  sleeper <- make_sleeper_data(
    gsis_ids = c("00-001", "00-002"),
    teams    = c("KC", "BUF"),
    is_fa    = c(FALSE, TRUE)
  )
  out <- .merge_sleeper_roster(roster, sleeper)
  expect_equal(out$team, c("KC", NA_character_))
})

test_that("[R/29] .merge_sleeper_roster prefers Sleeper team over nflreadr team", {
  roster  <- make_nflreadr_roster(gsis_ids = "00-001", teams = "BUF")
  sleeper <- make_sleeper_data(gsis_ids = "00-001", teams = "KC")
  out     <- .merge_sleeper_roster(roster, sleeper)
  expect_equal(out$team, "KC")
})

test_that("[R/29] .merge_sleeper_roster falls back to nflreadr team when no Sleeper match", {
  roster  <- make_nflreadr_roster(gsis_ids = "00-999", teams = "DAL")
  sleeper <- make_sleeper_data(gsis_ids = "00-001", teams = "KC")
  out     <- .merge_sleeper_roster(roster, sleeper)
  expect_equal(out$team, "DAL")
})

test_that("[R/29] .merge_sleeper_roster normalizes nflreadr AZ to ARI when no Sleeper match (regression: Week 15 bug fix)", {
  # Bug: nflreadr roster row with team = "AZ" and no Sleeper match fell through
  # case_when's default branch and kept "AZ", causing ACTIVE_TEAMS_2026 to drop
  # the player. Fix: a final .normalize_sleeper_team_codes() pass after case_when.
  roster  <- make_nflreadr_roster(gsis_ids = "00-arz", teams = "AZ")
  sleeper <- make_sleeper_data(gsis_ids = "00-001", teams = "KC")
  out     <- .merge_sleeper_roster(roster, sleeper)
  expect_equal(out$team, "ARI",
    info = "Final normalization pass missing -- AZ should map to ARI even with no Sleeper match")
})


# ==============================================================================
# .integrate_sleeper_rookies
# ==============================================================================

test_that("[R/29] .integrate_sleeper_rookies returns roster unchanged when Sleeper is empty", {
  roster    <- make_nflreadr_roster()
  out_null  <- .integrate_sleeper_rookies(roster, NULL)
  out_empty <- .integrate_sleeper_rookies(roster, make_sleeper_data()[0, ])
  expect_equal(out_null, roster)
  expect_equal(out_empty, roster)
})

test_that("[R/29] .integrate_sleeper_rookies Pass A adds Sleeper gsis_ids not already in roster", {
  roster <- make_nflreadr_roster(gsis_ids = "00-001", teams = "KC")
  sleeper <- make_sleeper_data(
    gsis_ids  = c("00-001", "00-rook"),
    teams     = c("KC", "DAL"),
    positions = c("WR", "RB"),
    is_fa     = c(FALSE, FALSE)
  )
  # Non-matching prospects so function passes the file-available check but
  # Pass B finds no name matches -- isolates Pass A behavior.
  prospects_noop <- make_prospects_fixture(
    gsis_ids  = "00-noop",
    names     = "Nonmatching Name",
    positions = "QB"
  )
  testthat::local_mocked_bindings(
    read_csv = function(...) prospects_noop,
    .package = "readr"
  )
  out   <- .integrate_sleeper_rookies(roster, sleeper)
  added <- out %>% dplyr::filter(gsis_id == "00-rook")
  expect_equal(nrow(out), 2L)
  expect_true("00-rook" %in% out$gsis_id)
  expect_equal(added$team, "DAL")
  expect_equal(added$position, "RB")
  expect_equal(added$status, "ACT")
})

test_that("[R/29] .integrate_sleeper_rookies Pass B recovers gsis_id via name+position match against R/28 prospects", {
  roster <- make_nflreadr_roster(gsis_ids = "00-001", teams = "KC")
  sleeper <- make_sleeper_data(
    gsis_ids  = NA_character_,
    teams     = "DAL",
    positions = "WR",
    is_fa     = FALSE,
    names     = "Rookie One"
  )
  prospects <- make_prospects_fixture(
    gsis_ids  = "00-901",
    names     = "Rookie One",
    positions = "WR"
  )
  testthat::local_mocked_bindings(
    read_csv = function(...) prospects,
    .package = "readr"
  )
  out   <- .integrate_sleeper_rookies(roster, sleeper)
  added <- out %>% dplyr::filter(gsis_id == "00-901")
  expect_true("00-901" %in% out$gsis_id,
    info = "Pass B should recover gsis_id via name+position match")
  expect_equal(added$team, "DAL")
  expect_equal(added$full_name, "Rookie One")
})

test_that("[R/29] .integrate_sleeper_rookies excludes free agents", {
  roster <- make_nflreadr_roster(gsis_ids = "00-001", teams = "KC")
  sleeper <- make_sleeper_data(
    gsis_ids  = "00-rook",
    teams     = "DAL",
    positions = "WR",
    is_fa     = TRUE
  )
  prospects_noop <- make_prospects_fixture(
    gsis_ids  = "00-noop",
    names     = "Nonmatching Name",
    positions = "QB"
  )
  testthat::local_mocked_bindings(
    read_csv = function(...) prospects_noop,
    .package = "readr"
  )
  out <- .integrate_sleeper_rookies(roster, sleeper)
  expect_false("00-rook" %in% out$gsis_id)
})

test_that("[R/29] .integrate_sleeper_rookies excludes non-skill positions (K, DEF, OL)", {
  roster <- make_nflreadr_roster(gsis_ids = "00-001", teams = "KC")
  sleeper <- make_sleeper_data(
    gsis_ids  = c("00-k", "00-def", "00-ol", "00-rb"),
    teams     = "DAL",
    positions = c("K", "DEF", "OL", "RB"),
    is_fa     = FALSE
  )
  prospects_noop <- make_prospects_fixture(
    gsis_ids  = "00-noop",
    names     = "Nonmatching Name",
    positions = "QB"
  )
  testthat::local_mocked_bindings(
    read_csv = function(...) prospects_noop,
    .package = "readr"
  )
  out <- .integrate_sleeper_rookies(roster, sleeper)
  expect_true("00-rb" %in% out$gsis_id)
  expect_false(any(c("00-k", "00-def", "00-ol") %in% out$gsis_id))
})


# ==============================================================================
# .build_active_roster_filter
# ==============================================================================

test_that("[R/29] .build_active_roster_filter returns correct empty schema on empty input", {
  out <- .build_active_roster_filter(tibble::tibble())
  expect_equal(nrow(out), 0L)
  expect_true(all(c("nfl_gsis_id", "player_name", "team", "status") %in% names(out)))
})

test_that("[R/29] .build_active_roster_filter drops rows with NA gsis_id, NA team, or empty-string team", {
  roster <- tibble::tibble(
    gsis_id   = c("00-001", "00-002", NA_character_, "00-004", "00-005"),
    full_name = c("A", "B", "C", "D", "E"),
    team      = c("KC", NA_character_, "BUF", "", "DAL"),
    position  = rep("WR", 5),
    status    = rep("ACT", 5)
  )
  out <- .build_active_roster_filter(roster)
  expect_equal(nrow(out), 2L)
  expect_setequal(out$nfl_gsis_id, c("00-001", "00-005"))
})

test_that("[R/29] .build_active_roster_filter renames gsis_id -> nfl_gsis_id and full_name -> player_name", {
  roster <- make_nflreadr_roster(gsis_ids = "00-001", teams = "KC")
  out    <- .build_active_roster_filter(roster)
  expect_true("nfl_gsis_id" %in% names(out))
  expect_true("player_name" %in% names(out))
  expect_false("gsis_id" %in% names(out))
  expect_false("full_name" %in% names(out))
})
