# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Test Suite: R/31 Player Volume Allocation
# File: tests/test_season2_week15_r31_functions.R
#
# COVERAGE
# --------
#   .normalize_sleeper_team_codes_r31  -- crosswalk parity with R/29
#   .normalize_name_for_match_r31      -- Jr/Sr/III suffix, apostrophe, period,
#                                         whitespace collapse
#   .merge_sleeper_depth_overrides     -- Pass 1 (gsis_id), depth_was_missing
#                                         flag TRUE/FALSE
#   .correct_rookie_depth_ranks        -- R1/R2/R3/R4/R5+ paths, FALSE flag
#                                         not touched, UDFA (no draft_round)
#                                         not corrected, column removed, re-rank
#
# DEPENDENCIES
# ------------
#   R/19_sleeper_api.R  (defines get_all_sleeper_players -- sourced before R/31)
#   R/30_team_volume_projections.R  (sourced automatically by R/31 at load time)
#   R/31_player_volume_allocation.R
#
# MOCKING
# -------
#   .merge_sleeper_depth_overrides calls get_all_sleeper_players() internally.
#   Tests mock this binding in the global environment (no .package argument) so
#   no network call is made. If testthat cannot find the binding to replace,
#   pass .env = globalenv() as an additional argument.
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
  library(glue)
})

suppressPackageStartupMessages({
  source(here::here("R", "19_sleeper_api.R"))
  source(here::here("R", "31_player_volume_allocation.R"))
})


# ==============================================================================
# FIXTURE BUILDERS
# ==============================================================================

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

make_depth_chart <- function(gsis_ids          = paste0("00-", sprintf("%03d", 1:4)),
                              teams             = c("KC", "KC", "BUF", "BUF"),
                              positions         = c("WR", "WR", "RB", "RB"),
                              depth_rank_raw    = c(1L, 2L, 1L, 2L),
                              names             = NULL,
                              depth_was_missing = NULL) {
  n <- length(gsis_ids)
  if (is.null(names)) names <- paste("DC Player", seq_len(n))
  out <- tibble::tibble(
    nfl_gsis_id    = gsis_ids,
    player_name    = names,
    team           = teams,
    position       = positions,
    depth_rank_raw = as.integer(depth_rank_raw)
  )
  if (!is.null(depth_was_missing)) out$depth_was_missing <- depth_was_missing
  out
}


# ==============================================================================
# .normalize_sleeper_team_codes_r31
# ==============================================================================

test_that("[R/31] .normalize_sleeper_team_codes_r31 maps all four crosswalk entries and passes unknowns and NA unchanged", {
  # Mirrors the R/29 crosswalk exactly -- confirms parity between the two copies.
  input  <- c("LAR", "AZ", "JAC", "WSH", "KC", "BUF", NA_character_)
  output <- .normalize_sleeper_team_codes_r31(input)
  expect_equal(output, c("LA", "ARI", "JAX", "WAS", "KC", "BUF", NA_character_))
})


# ==============================================================================
# .normalize_name_for_match_r31
# ==============================================================================

test_that("[R/31] .normalize_name_for_match_r31 strips Jr, Sr, II, III, IV suffixes", {
  inputs  <- c("Marvin Harrison Jr.", "D.J. Chark Sr.", "Robert Griffin III",
                "Calvin Ridley IV")
  outputs <- .normalize_name_for_match_r31(inputs)
  expect_equal(outputs, c("marvin harrison", "dj chark", "robert griffin",
                            "calvin ridley"))
})

test_that("[R/31] .normalize_name_for_match_r31 strips apostrophes, periods, and non-letter characters", {
  inputs  <- c("Ja'Marr Chase", "D.K. Metcalf", "Odell Beckham Jr. Jr.")
  outputs <- .normalize_name_for_match_r31(inputs)
  expect_equal(outputs, c("jamarr chase", "dk metcalf", "odell beckham"))
  expect_true(grepl("jamarr", outputs[1]),
    info = "Apostrophe should be removed: Ja'Marr -> jamarr")
})

test_that("[R/31] .normalize_name_for_match_r31 collapses multiple spaces and trims leading/trailing whitespace", {
  inputs  <- c("  John   Smith  ", "Patrick  Mahomes")
  outputs <- .normalize_name_for_match_r31(inputs)
  expect_equal(outputs, c("john smith", "patrick mahomes"))
  expect_false(any(grepl("  ", outputs)),
    info = "Double spaces should be collapsed to single")
})


# ==============================================================================
# .merge_sleeper_depth_overrides
# get_all_sleeper_players() is mocked -- no network call made.
# ==============================================================================

test_that("[R/31] .merge_sleeper_depth_overrides Pass 1 replaces nflreadr team with Sleeper team via gsis_id", {
  dc <- make_depth_chart(
    gsis_ids       = c("00-001", "00-002"),
    teams          = c("BUF", "KC"),
    positions      = c("WR", "WR"),
    depth_rank_raw = c(1L, 1L)
  )
  # 00-001 moved from BUF to KC per Sleeper
  sleeper_mock <- make_sleeper_data(
    gsis_ids    = "00-001",
    teams       = "KC",
    positions   = "WR",
    is_fa       = FALSE,
    depth_order = 1L
  )
  orig_fn <- get_all_sleeper_players
  assign("get_all_sleeper_players", function(...) sleeper_mock, envir = globalenv())
  on.exit(assign("get_all_sleeper_players", orig_fn, envir = globalenv()), add = TRUE)

  out      <- .merge_sleeper_depth_overrides(dc)
  moved    <- out %>% dplyr::filter(nfl_gsis_id == "00-001")
  expect_equal(nrow(moved), 1L)
  expect_equal(moved$team, "KC",
    info = "Sleeper team should override nflreadr team for gsis_id match")
})

test_that("[R/31] .merge_sleeper_depth_overrides sets depth_was_missing TRUE when Sleeper depth_chart_order is NA", {
  dc <- make_depth_chart(
    gsis_ids       = "00-001",
    teams          = "KC",
    positions      = "WR",
    depth_rank_raw = 1L
  )
  sleeper_mock <- make_sleeper_data(
    gsis_ids    = "00-001",
    teams       = "KC",
    positions   = "WR",
    is_fa       = FALSE,
    depth_order = NA_integer_
  )
  orig_fn <- get_all_sleeper_players
  assign("get_all_sleeper_players", function(...) sleeper_mock, envir = globalenv())
  on.exit(assign("get_all_sleeper_players", orig_fn, envir = globalenv()), add = TRUE)

  out <- .merge_sleeper_depth_overrides(dc)
  row <- out %>% dplyr::filter(nfl_gsis_id == "00-001")
  expect_equal(row$depth_was_missing, TRUE,
    info = "depth_was_missing should be TRUE when Sleeper depth_chart_order is NA")
})

test_that("[R/31] .merge_sleeper_depth_overrides sets depth_was_missing FALSE for nflreadr-only rows", {
  dc <- make_depth_chart(
    gsis_ids       = c("00-001", "00-vet"),
    teams          = c("KC", "KC"),
    positions      = c("WR", "WR"),
    depth_rank_raw = c(1L, 2L)
  )
  # Sleeper only covers 00-001; 00-vet is nflreadr-only
  sleeper_mock <- make_sleeper_data(
    gsis_ids    = "00-001",
    teams       = "KC",
    positions   = "WR",
    is_fa       = FALSE,
    depth_order = 1L
  )
  orig_fn <- get_all_sleeper_players
  assign("get_all_sleeper_players", function(...) sleeper_mock, envir = globalenv())
  on.exit(assign("get_all_sleeper_players", orig_fn, envir = globalenv()), add = TRUE)

  out     <- .merge_sleeper_depth_overrides(dc)
  vet_row <- out %>% dplyr::filter(nfl_gsis_id == "00-vet")
  expect_equal(nrow(vet_row), 1L)
  expect_equal(vet_row$depth_was_missing, FALSE,
    info = "nflreadr-only rows should always get depth_was_missing = FALSE")
})


# ==============================================================================
# .correct_rookie_depth_ranks
# Pure function -- no mocking needed.
# Each rank test uses enough players in the same team+position group to put
# the rookie at the expected ordinal position after re-rank.
# ==============================================================================

test_that("[R/31] .correct_rookie_depth_ranks R1 draft corrects depth rank to 1", {
  dc <- make_depth_chart(
    gsis_ids          = c("00-r1", "00-vet"),
    teams             = c("KC", "KC"),
    positions         = c("WR", "WR"),
    depth_rank_raw    = c(999L, 2L),
    depth_was_missing = c(TRUE, FALSE)
  )
  rookies <- tibble::tibble(nfl_gsis_id = "00-r1", draft_round = 1L)
  out <- .correct_rookie_depth_ranks(dc, rookies)
  expect_equal(
    out %>% dplyr::filter(nfl_gsis_id == "00-r1") %>% dplyr::pull(depth_rank_raw),
    1L
  )
})

test_that("[R/31] .correct_rookie_depth_ranks R2 draft corrects depth rank to 2", {
  dc <- make_depth_chart(
    gsis_ids          = c("00-vet", "00-r2"),
    teams             = c("KC", "KC"),
    positions         = c("RB", "RB"),
    depth_rank_raw    = c(1L, 999L),
    depth_was_missing = c(FALSE, TRUE)
  )
  rookies <- tibble::tibble(nfl_gsis_id = "00-r2", draft_round = 2L)
  out <- .correct_rookie_depth_ranks(dc, rookies)
  expect_equal(
    out %>% dplyr::filter(nfl_gsis_id == "00-r2") %>% dplyr::pull(depth_rank_raw),
    2L
  )
})

test_that("[R/31] .correct_rookie_depth_ranks R3 draft corrects depth rank to 3", {
  dc <- make_depth_chart(
    gsis_ids          = c("00-v1", "00-v2", "00-r3"),
    teams             = c("BUF", "BUF", "BUF"),
    positions         = c("RB", "RB", "RB"),
    depth_rank_raw    = c(1L, 2L, 999L),
    depth_was_missing = c(FALSE, FALSE, TRUE)
  )
  rookies <- tibble::tibble(nfl_gsis_id = "00-r3", draft_round = 3L)
  out <- .correct_rookie_depth_ranks(dc, rookies)
  expect_equal(
    out %>% dplyr::filter(nfl_gsis_id == "00-r3") %>% dplyr::pull(depth_rank_raw),
    3L
  )
})

test_that("[R/31] .correct_rookie_depth_ranks R4 draft corrects depth rank to 4", {
  dc <- make_depth_chart(
    gsis_ids          = c("00-v1", "00-v2", "00-v3", "00-r4"),
    teams             = c("BUF", "BUF", "BUF", "BUF"),
    positions         = c("TE", "TE", "TE", "TE"),
    depth_rank_raw    = c(1L, 2L, 3L, 999L),
    depth_was_missing = c(FALSE, FALSE, FALSE, TRUE)
  )
  rookies <- tibble::tibble(nfl_gsis_id = "00-r4", draft_round = 4L)
  out <- .correct_rookie_depth_ranks(dc, rookies)
  expect_equal(
    out %>% dplyr::filter(nfl_gsis_id == "00-r4") %>% dplyr::pull(depth_rank_raw),
    4L
  )
})

test_that("[R/31] .correct_rookie_depth_ranks R5+ draft also corrects depth rank to 4 (floor at 4)", {
  dc <- make_depth_chart(
    gsis_ids          = c("00-v1", "00-v2", "00-v3", "00-r7"),
    teams             = c("KC", "KC", "KC", "KC"),
    positions         = c("WR", "WR", "WR", "WR"),
    depth_rank_raw    = c(1L, 2L, 3L, 999L),
    depth_was_missing = c(FALSE, FALSE, FALSE, TRUE)
  )
  rookies <- tibble::tibble(nfl_gsis_id = "00-r7", draft_round = 7L)
  out <- .correct_rookie_depth_ranks(dc, rookies)
  expect_equal(
    out %>% dplyr::filter(nfl_gsis_id == "00-r7") %>% dplyr::pull(depth_rank_raw),
    4L
  )
})

test_that("[R/31] .correct_rookie_depth_ranks depth_was_missing FALSE players are not corrected regardless of draft round", {
  dc <- make_depth_chart(
    gsis_ids          = "00-vet",
    teams             = "KC",
    positions         = "QB",
    depth_rank_raw    = 1L,
    depth_was_missing = FALSE
  )
  rookies <- tibble::tibble(nfl_gsis_id = "00-vet", draft_round = 1L)
  out <- .correct_rookie_depth_ranks(dc, rookies)
  expect_equal(
    out %>% dplyr::filter(nfl_gsis_id == "00-vet") %>% dplyr::pull(depth_rank_raw),
    1L,
    info = "depth_was_missing=FALSE means no correction regardless of draft round"
  )
})

test_that("[R/31] .correct_rookie_depth_ranks UDFA rookies with no draft_round entry are not corrected", {
  # UDFA: depth_was_missing=TRUE but not in rookies table (no draft entry).
  # eligible join returns 0 rows -> early return fires -> rank 999 preserved.
  dc <- make_depth_chart(
    gsis_ids          = c("00-udfa", "00-vet"),
    teams             = c("DAL", "DAL"),
    positions         = c("WR", "WR"),
    depth_rank_raw    = c(999L, 1L),
    depth_was_missing = c(TRUE, FALSE)
  )
  rookies <- tibble::tibble(nfl_gsis_id = character(), draft_round = integer())
  out <- .correct_rookie_depth_ranks(dc, rookies)
  expect_equal(
    out %>% dplyr::filter(nfl_gsis_id == "00-udfa") %>% dplyr::pull(depth_rank_raw),
    999L,
    info = "UDFA with no draft entry should not be corrected (early return path, no re-rank)"
  )
})

test_that("[R/31] .correct_rookie_depth_ranks removes depth_was_missing column from output", {
  dc <- make_depth_chart(
    gsis_ids          = "00-r1",
    teams             = "KC",
    positions         = "WR",
    depth_rank_raw    = 999L,
    depth_was_missing = TRUE
  )
  rookies <- tibble::tibble(nfl_gsis_id = "00-r1", draft_round = 1L)
  out <- .correct_rookie_depth_ranks(dc, rookies)
  expect_false("depth_was_missing" %in% names(out),
    info = "depth_was_missing is a working column and must be stripped before output")
})

test_that("[R/31] .correct_rookie_depth_ranks re-ranks to contiguous integers within team+position after correction", {
  dc <- make_depth_chart(
    gsis_ids          = c("00-v1", "00-r2", "00-v3"),
    teams             = c("KC", "KC", "KC"),
    positions         = c("WR", "WR", "WR"),
    depth_rank_raw    = c(1L, 999L, 3L),
    depth_was_missing = c(FALSE, TRUE, FALSE)
  )
  rookies <- tibble::tibble(nfl_gsis_id = "00-r2", draft_round = 2L)
  out <- .correct_rookie_depth_ranks(dc, rookies)
  ranks <- out %>%
    dplyr::filter(team == "KC", position == "WR") %>%
    dplyr::pull(depth_rank_raw) %>%
    sort()
  expect_equal(ranks, c(1L, 2L, 3L),
    info = "Ranks within team+position must be contiguous integers after re-rank")
})
