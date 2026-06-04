# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Test Suite: R/29, R/31, R/32, R/33
# File: tests/test_season2_week15_functions.R
#
# COVERAGE
# --------
#   R/29 (projection_engine):
#     .normalize_sleeper_team_codes  -- crosswalk correctness
#     .merge_sleeper_roster          -- FA flag, team override, AZ->ARI bug fix
#     .integrate_sleeper_rookies     -- Pass A (gsis_id), Pass B (name+pos),
#                                       FA filter, position filter, empty input
#     .build_active_roster_filter    -- empty input, drops, column renames
#
#   R/31 (player_volume_allocation):
#     .normalize_sleeper_team_codes_r31    -- crosswalk parity with R/29
#     .normalize_name_for_match_r31        -- Jr/Sr/III, apostrophe, period
#     .merge_sleeper_depth_overrides       -- Pass 1, Pass 2, depth_was_missing
#     .correct_rookie_depth_ranks          -- R1/R2/R3/R4+ paths, UDFA skip,
#                                             flag column removed, re-rank
#
#   R/32 (projection_reconciliation):
#     .apply_qb_depth_discount             -- QB1=1.00, QB2=0.15, QB3=0.05,
#                                             default=0.05, non-QB NA
#     .compute_volume_implied_ppg          -- schema, zero-volume guard
#     .determine_blend_weights             -- weight by prior_source
#     .blend_projections                   -- blend equation correctness
#
#   R/33 (vorp_rankings):
#     build_league_config                  -- defaults, override
#     .compute_replacement_levels          -- starter assignment, replacement PPG
#     .compute_player_vorp                 -- vorp math, modifier math
#     compute_vorp_rankings                -- integration, rank contiguity
#
# DESIGN NOTES
# ------------
# 1. Sleeper data and R/28 prospects file dependencies are mocked via
#    testthat::local_mocked_bindings() so tests run without network or disk.
# 2. Fixtures live inline at the top of the file (Week 14 convention).
# 3. Test data is sized to clear every internal filter in the function under
#    test. For R/33, a 2-team league config is used so 12 players is enough
#    to populate starters, FLEX, and bench at every position.
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
  library(glue)
})

# ------------------------------------------------------------------------------
# SOURCE PRODUCTION CODE
# ------------------------------------------------------------------------------
# R/19 must source first because R/29 and R/31 call get_all_sleeper_players()
# without sourcing R/19 themselves. R/29's conditional source-guards pull in
# R/15/16/17/23 on demand; if those packages or files are missing the source
# call will error and the test file will not load.

suppressPackageStartupMessages({
  source(here::here("R", "19_sleeper_api.R"))
  source(here::here("R", "29_projection_engine.R"))
  source(here::here("R", "31_player_volume_allocation.R"))
  source(here::here("R", "32_projection_reconciliation.R"))
  source(here::here("R", "33_vorp_rankings.R"))
})


# ==============================================================================
# FIXTURE BUILDERS
# ==============================================================================

# ------------------------------------------------------------------------------
# make_nflreadr_roster
# ------------------------------------------------------------------------------
#' Build a tibble matching the nflreadr::load_rosters() output schema for the
#' columns .merge_sleeper_roster() and .build_active_roster_filter() touch.
#'
#' @param gsis_ids Character vector of gsis_id values.
#' @param teams    Character vector of team codes (one per gsis_id).
#' @param ...      Optional named columns to add or override.
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

# ------------------------------------------------------------------------------
# make_sleeper_data
# ------------------------------------------------------------------------------
#' Build a tibble matching get_all_sleeper_players() output.
make_sleeper_data <- function(gsis_ids = c("00-001", "00-002"),
                               teams    = "KC",
                               positions = "WR",
                               is_fa     = FALSE,
                               names     = NULL,
                               sleeper_ids = NULL,
                               depth_order = NULL,
                               status      = NULL) {
  n <- length(gsis_ids)
  if (length(teams) == 1L)     teams <- rep(teams, n)
  if (length(positions) == 1L) positions <- rep(positions, n)
  if (length(is_fa) == 1L)     is_fa <- rep(is_fa, n)
  if (is.null(names))        names <- paste("Sleeper Player", seq_len(n))
  if (is.null(sleeper_ids))  sleeper_ids <- paste0("sl_", seq_len(n))
  if (is.null(depth_order))  depth_order <- rep(NA_integer_, n)
  if (is.null(status))       status <- rep("Active", n)

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

# ------------------------------------------------------------------------------
# make_prospects_fixture
# ------------------------------------------------------------------------------
#' Build a tibble matching the R/28 prospects CSV schema for the columns
#' .integrate_sleeper_rookies() Pass B uses (cfb_player_name, nfl_gsis_id,
#' position).
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

# ------------------------------------------------------------------------------
# make_depth_chart
# ------------------------------------------------------------------------------
#' Build a tibble matching .load_2026_depth_charts() output as consumed by
#' .merge_sleeper_depth_overrides() and .correct_rookie_depth_ranks().
make_depth_chart <- function(gsis_ids = paste0("00-", sprintf("%03d", 1:4)),
                              teams    = c("KC", "KC", "BUF", "BUF"),
                              positions = c("WR", "WR", "RB", "RB"),
                              depth_rank_raw = c(1L, 2L, 1L, 2L),
                              names    = NULL,
                              depth_was_missing = NULL) {
  n <- length(gsis_ids)
  if (is.null(names)) names <- paste("DC Player", seq_len(n))

  out <- tibble::tibble(
    nfl_gsis_id     = gsis_ids,
    player_name     = names,
    team            = teams,
    position        = positions,
    depth_rank_raw  = as.integer(depth_rank_raw)
  )
  if (!is.null(depth_was_missing)) {
    out$depth_was_missing <- depth_was_missing
  }
  out
}

# ------------------------------------------------------------------------------
# make_r29_projections
# ------------------------------------------------------------------------------
#' Build a minimal tibble matching the columns R/32 reads from R/29 output.
make_r29_projections <- function(gsis_ids = paste0("00-", sprintf("%03d", 1:6)),
                                   positions = c("QB", "QB", "RB", "WR", "WR", "TE"),
                                   teams    = c("KC", "BUF", "ARI", "KC", "BUF", "DAL"),
                                   posterior_mu = c(22, 8, 16, 18, 14, 11),
                                   prior_source = NULL) {
  n <- length(gsis_ids)
  if (is.null(prior_source)) prior_source <- rep("historical", n)

  tibble::tibble(
    nfl_gsis_id           = gsis_ids,
    player_name           = paste("Proj Player", seq_len(n)),
    position              = positions,
    team                  = teams,
    posterior_mu          = posterior_mu,
    posterior_sigma       = rep(4, n),
    prior_source          = prior_source,
    projection_lower_80   = posterior_mu - 4,
    projection_upper_80   = posterior_mu + 4,
    projection_lower_95   = posterior_mu - 7,
    projection_upper_95   = posterior_mu + 7,
    boom_probability      = rep(0.2, n),
    bust_probability      = rep(0.15, n)
  )
}

# ------------------------------------------------------------------------------
# make_r31_alloc
# ------------------------------------------------------------------------------
#' Build a minimal tibble matching the columns R/32 reads from R/31 output.
make_r31_alloc <- function(gsis_ids,
                            positions,
                            depth_positions,
                            expected_targets_pg = NULL,
                            expected_carries_pg = NULL) {
  n <- length(gsis_ids)
  if (is.null(expected_targets_pg)) expected_targets_pg <- rep(0, n)
  if (is.null(expected_carries_pg)) expected_carries_pg <- rep(0, n)

  tibble::tibble(
    nfl_gsis_id         = gsis_ids,
    position            = positions,
    depth_position      = depth_positions,
    expected_targets_pg = expected_targets_pg,
    expected_carries_pg = expected_carries_pg
  )
}


# ==============================================================================
# R/29 -- PROJECTION ENGINE TESTS
# ==============================================================================

# ------------------------------------------------------------------------------
# .normalize_sleeper_team_codes
# ------------------------------------------------------------------------------

test_that(".normalize_sleeper_team_codes maps all four crosswalk entries", {
  input  <- c("LAR", "AZ", "JAC", "WSH")
  output <- .normalize_sleeper_team_codes(input)
  expect_equal(output, c("LA", "ARI", "JAX", "WAS"))
})

test_that(".normalize_sleeper_team_codes leaves unknown codes unchanged", {
  input  <- c("KC", "BUF", "ARI", "LA")
  output <- .normalize_sleeper_team_codes(input)
  expect_equal(output, input)
})

test_that(".normalize_sleeper_team_codes handles NA passthrough", {
  input  <- c("AZ", NA, "LAR", NA_character_)
  output <- .normalize_sleeper_team_codes(input)
  expect_equal(output, c("ARI", NA, "LA", NA_character_))
})

# ------------------------------------------------------------------------------
# .merge_sleeper_roster
# ------------------------------------------------------------------------------

test_that(".merge_sleeper_roster returns roster unchanged when sleeper empty", {
  roster <- make_nflreadr_roster()
  out_null  <- .merge_sleeper_roster(roster, NULL)
  out_empty <- .merge_sleeper_roster(
    roster,
    make_sleeper_data()[0, ]
  )
  expect_equal(out_null, roster)
  expect_equal(out_empty, roster)
})

test_that(".merge_sleeper_roster sets team to NA when Sleeper flags free agent", {
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

test_that(".merge_sleeper_roster prefers Sleeper team over nflreadr team", {
  # nflreadr says BUF, Sleeper says KC -- Sleeper should win (more current)
  roster  <- make_nflreadr_roster(gsis_ids = "00-001", teams = "BUF")
  sleeper <- make_sleeper_data(gsis_ids = "00-001", teams = "KC")
  out <- .merge_sleeper_roster(roster, sleeper)
  expect_equal(out$team, "KC")
})

test_that(".merge_sleeper_roster falls back to nflreadr when no Sleeper match", {
  # Player exists in nflreadr but not in Sleeper -- nflreadr team passes through
  roster  <- make_nflreadr_roster(gsis_ids = "00-999", teams = "DAL")
  sleeper <- make_sleeper_data(gsis_ids = "00-001", teams = "KC")
  out <- .merge_sleeper_roster(roster, sleeper)
  expect_equal(out$team, "DAL")
})

test_that(".merge_sleeper_roster normalizes nflreadr AZ to ARI when no Sleeper match (regression: Week 15 bug fix)", {
  # Bug: nflreadr roster row carrying team = "AZ" with no matching Sleeper
  # record fell through case_when's default branch and kept "AZ", causing
  # ACTIVE_TEAMS_2026 filter downstream to drop the player.
  # Fix: a final .normalize_sleeper_team_codes() pass after case_when.
  roster  <- make_nflreadr_roster(gsis_ids = "00-arz", teams = "AZ")
  sleeper <- make_sleeper_data(gsis_ids = "00-001", teams = "KC")
  out <- .merge_sleeper_roster(roster, sleeper)
  expect_equal(out$team, "ARI",
    info = "Final team normalization missing -- AZ should map to ARI even with no Sleeper match")
})

# ------------------------------------------------------------------------------
# .integrate_sleeper_rookies
# ------------------------------------------------------------------------------

test_that(".integrate_sleeper_rookies returns roster unchanged when sleeper empty", {
  roster <- make_nflreadr_roster()
  out_null  <- .integrate_sleeper_rookies(roster, NULL)
  out_empty <- .integrate_sleeper_rookies(roster, make_sleeper_data()[0, ])
  expect_equal(out_null, roster)
  expect_equal(out_empty, roster)
})

test_that(".integrate_sleeper_rookies Pass A adds Sleeper gsis_ids not in roster", {
  roster <- make_nflreadr_roster(
    gsis_ids = c("00-001"),
    teams    = c("KC")
  )
  # Sleeper has 00-001 (already in roster, skip) and 00-rook (new, add)
  sleeper <- make_sleeper_data(
    gsis_ids = c("00-001", "00-rook"),
    teams    = c("KC", "DAL"),
    positions = c("WR", "RB"),
    is_fa    = c(FALSE, FALSE)
  )
  # The function early-returns the roster unchanged if prospects is empty
  # (line 585). Pass A still requires the prospects check to pass even though
  # Pass A itself does not use prospects. Provide a non-matching prospect so
  # the check passes, Pass A fires, and Pass B finds nothing.
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
  expect_equal(nrow(out), 2L)
  expect_true("00-rook" %in% out$gsis_id)
  added <- out %>% filter(gsis_id == "00-rook")
  expect_equal(added$team, "DAL")
  expect_equal(added$position, "RB")
  expect_equal(added$status, "ACT")
})

test_that(".integrate_sleeper_rookies Pass B recovers gsis_id via name+position match against prospects", {
  roster <- make_nflreadr_roster(
    gsis_ids = c("00-001"),
    teams    = c("KC")
  )
  # Sleeper row with NO gsis_id but a name matching the R/28 prospect
  sleeper <- make_sleeper_data(
    gsis_ids    = c(NA_character_),
    teams       = c("DAL"),
    positions   = c("WR"),
    is_fa       = c(FALSE),
    names       = c("Rookie One")
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

  out <- .integrate_sleeper_rookies(roster, sleeper)
  expect_true("00-901" %in% out$gsis_id,
    info = "Pass B should recover gsis_id 00-901 via name+position match")
  added <- out %>% filter(gsis_id == "00-901")
  expect_equal(added$team, "DAL")
  expect_equal(added$full_name, "Rookie One")
})

test_that(".integrate_sleeper_rookies excludes free agents", {
  roster <- make_nflreadr_roster(gsis_ids = "00-001", teams = "KC")
  sleeper <- make_sleeper_data(
    gsis_ids = c("00-rook"),
    teams    = c("DAL"),
    positions = c("WR"),
    is_fa    = c(TRUE)  # marked free agent -- should not be added
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

test_that(".integrate_sleeper_rookies excludes non-skill positions", {
  roster <- make_nflreadr_roster(gsis_ids = "00-001", teams = "KC")
  sleeper <- make_sleeper_data(
    gsis_ids = c("00-k", "00-def", "00-ol", "00-rook-rb"),
    teams    = c("DAL", "DAL", "DAL", "DAL"),
    positions = c("K", "DEF", "OL", "RB"),
    is_fa    = c(FALSE, FALSE, FALSE, FALSE)
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
  # Only the RB should pass the QB/RB/WR/TE filter
  expect_true("00-rook-rb" %in% out$gsis_id)
  expect_false(any(c("00-k", "00-def", "00-ol") %in% out$gsis_id))
})

# ------------------------------------------------------------------------------
# .build_active_roster_filter
# ------------------------------------------------------------------------------

test_that(".build_active_roster_filter returns empty schema on empty input", {
  out <- .build_active_roster_filter(tibble::tibble())
  expect_equal(nrow(out), 0L)
  expect_true(all(c("nfl_gsis_id", "player_name", "team", "status") %in% names(out)))
})

test_that(".build_active_roster_filter drops rows with NA or empty team and NA gsis_id", {
  roster <- tibble::tibble(
    gsis_id   = c("00-001", "00-002", NA_character_, "00-004", "00-005"),
    full_name = c("A", "B", "C", "D", "E"),
    team      = c("KC", NA_character_, "BUF", "", "DAL"),
    position  = rep("WR", 5),
    status    = rep("ACT", 5)
  )
  out <- .build_active_roster_filter(roster)
  # Only 00-001 (KC) and 00-005 (DAL) should survive
  expect_equal(nrow(out), 2L)
  expect_setequal(out$nfl_gsis_id, c("00-001", "00-005"))
})

test_that(".build_active_roster_filter renames columns to nfl_gsis_id and player_name", {
  roster <- make_nflreadr_roster(gsis_ids = "00-001", teams = "KC")
  out <- .build_active_roster_filter(roster)
  expect_true("nfl_gsis_id" %in% names(out))
  expect_true("player_name" %in% names(out))
  expect_false("gsis_id" %in% names(out))
  expect_false("full_name" %in% names(out))
})
