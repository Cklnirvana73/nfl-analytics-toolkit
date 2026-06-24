# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 17
# Test Suite: Trade Value Model Functions
# File: tests/test_season2_week17_functions.R
#
# Tests:
#   Section A: .curve_value_at_age()            --  6 tests
#   Section B: .project_aging_multiplier()      --  8 tests
#   Section C: .scale_prospect_to_vorp_units()  --  5 tests
#   Section D: .assign_tiers_and_ranks()        --  7 tests
#   Section E: compute_player_trade_values()    --  8 tests
#   Section F: evaluate_trade()                 -- 10 tests
#   Section G: schema and type validation       --  4 tests
#
# Total: 48 tests
#
# No network calls. All tests use synthetic in-memory fixtures.
# Does NOT call build_trade_value_table(); that function requires Sleeper and
# nflreadr live connections. Its component functions are tested directly.
#
# Run:
#   testthat::test_file(here::here("tests", "test_season2_week17_functions.R"))
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)
library(here)

source(here::here("R", "37_trade_value_model.R"))


# ==============================================================================
# SHARED FIXTURES
# ==============================================================================
# All fixture builder functions are parameterised and deterministic.
# Defaults produce the smallest dataset that covers the tested behavior.
# ==============================================================================

# make_curve_data(): one position's curve_data tibble. Peak at age 25,
# anchored to 0 at age 23 (mirrors R/23 convention), declining afterward.
make_curve_data <- function() {
  tibble::tibble(
    age_at_season_start = 22:35,
    fitted_loess = c(-1, 0, 1, 2, 1, 0, -1, -2, -3, -4, -5, -6, -7, -8),
    fitted_quad  = c(-1, 0, 1, 2, 1, 0, -1, -2, -3, -4, -5, -6, -7, -8)
  )
}

# make_curves_fixture(): full curves_boxscore list, same curve for all positions.
make_curves_fixture <- function() {
  cd <- make_curve_data()
  list(
    QB = list(curve_data = cd, peak_age_loess = 25),
    RB = list(curve_data = cd, peak_age_loess = 25),
    WR = list(curve_data = cd, peak_age_loess = 25),
    TE = list(curve_data = cd, peak_age_loess = 25)
  )
}

# make_vorp_fixture(): minimal VORP-rankings tibble for one league.
# Contains all columns that compute_player_trade_values() selects from.
make_vorp_fixture <- function() {
  tibble::tibble(
    nfl_gsis_id             = paste0("V-", 1:6),
    player_name             = paste0("Veteran ", 1:6),
    team                    = rep("KC", 6),
    position                = c("QB", "WR", "WR", "RB", "RB", "TE"),
    league_name             = "Test League",
    league_format           = "PPR",
    r32_posterior_mu        = c(20.0, 12.0,  8.0, 15.0, 10.0,  6.0),
    r32_projection_upper_80 = c(24.0, 15.0, 11.0, 18.0, 13.0,  8.0),
    adjusted_vorp           = c( 5.0,  4.0,  2.0,  6.0,  3.0,  1.0),
    # Age 23 = peak-climb zone; age 30 = decline zone for testing multiplier
    age_at_season_start     = c(26L, 23L, 24L, 30L, 25L, 27L)
  )
}

# make_rookies_fixture(): prediction-cohort rookies with no NFL history.
make_rookies_fixture <- function(n = 3L) {
  tibble::tibble(
    nfl_gsis_id    = paste0("R-", seq_len(n)),
    player_name    = paste0("Rookie ", LETTERS[seq_len(n)]),
    position       = rep("WR", n),
    score_final    = c(90, 60, 30)[seq_len(n)],
    interval_width = rep(NA_real_, n)
  )
}

# make_trade_values_fixture(): pre-built output of build_trade_value_table().
# Trade values are hand-chosen to give deterministic WIN / LOSS / EVEN verdicts.
#   V-1 = 5.0, V-2 = 4.0, V-3 = 3.5, V-4 = 2.0, V-5 = 2.1
#   WIN:          give V-1 (5.0), receive V-2 + V-5 (6.1), delta +1.1
#   LOSS:         give V-2 + V-5 (6.1), receive V-1 (5.0), delta -1.1
#   ROUGHLY EVEN: give V-4 (2.0), receive V-5 (2.1), delta +0.1
make_trade_values_fixture <- function() {
  tibble::tibble(
    league_name          = "Test League",
    league_format        = "PPR",
    is_dynasty           = FALSE,
    nfl_gsis_id          = c("V-1", "V-2", "V-3", "V-4", "V-5"),
    player_name          = c("QB Star", "WR One", "WR Two", "RB One", "RB Two"),
    team                 = rep("KC", 5),
    position             = c("QB", "WR", "WR", "RB", "RB"),
    age_at_season_start  = c(26L, 28L, 24L, 30L, 25L),
    adjusted_vorp        = c(5.0, 4.0, 3.5, 2.0, 2.1),
    aging_multiplier     = c(1.0, 1.0, 1.0, 1.0, 1.0),
    rookie_flag          = FALSE,
    prospect_score_final = NA_real_,
    trade_value          = c(5.0, 4.0, 3.5, 2.0, 2.1),
    tv_tier              = c("Elite", "Elite", "Starter", "Flex", "Flex"),
    overall_tv_rank      = c(1L, 2L, 3L, 5L, 4L),
    position_tv_rank     = c(1L, 1L, 2L, 2L, 1L),
    tv_schema_tag        = "s2_w17_tradevalue_v1"
  )
}

# make_league_tv(): raw trade-value tibble for .assign_tiers_and_ranks() tests.
make_league_tv_fixture <- function() {
  tibble::tibble(
    nfl_gsis_id = paste0("P-", 1:10),
    player_name = paste0("Player ", 1:10),
    position    = c("QB", "WR", "WR", "RB", "RB", "TE", "WR", "RB", "QB", "WR"),
    trade_value = c(10, 8, 6, 5, 4, 3, 2, 1, 0, 0),
    rookie_flag = c(FALSE, FALSE, FALSE, FALSE, FALSE,
                    FALSE, FALSE, FALSE, FALSE, TRUE)
  )
}


# ==============================================================================
# SECTION A: .curve_value_at_age()
# ==============================================================================

test_that("A1: returns correct value when age is in curve_data", {
  cd <- make_curve_data()
  expect_equal(.curve_value_at_age(cd, 23L), 0.0)
  expect_equal(.curve_value_at_age(cd, 25L), 2.0)
})

test_that("A2: returns NA when curve_data is NULL", {
  expect_true(is.na(.curve_value_at_age(NULL, 25L)))
})

test_that("A3: returns NA when age is NA", {
  cd <- make_curve_data()
  expect_true(is.na(.curve_value_at_age(cd, NA)))
})

test_that("A4: clamps to max age when age is above the fitted range", {
  cd <- make_curve_data()
  # max age in fixture is 35 with value -8; age 45 should clamp to -8
  expect_equal(.curve_value_at_age(cd, 45L), -8.0)
})

test_that("A5: clamps to min age when age is below the fitted range", {
  cd <- make_curve_data()
  # min age in fixture is 22 with value -1; age 18 should clamp to -1
  expect_equal(.curve_value_at_age(cd, 18L), -1.0)
})

test_that("A6: falls back to fitted_quad when fitted_loess is NA at requested age", {
  cd <- make_curve_data()
  cd$fitted_loess[cd$age_at_season_start == 24L] <- NA_real_
  cd$fitted_quad [cd$age_at_season_start == 24L] <- 1.5
  expect_equal(.curve_value_at_age(cd, 24L), 1.5)
})


# ==============================================================================
# SECTION B: .project_aging_multiplier()
# ==============================================================================

test_that("B1: returns 1.0 when player_age is NA", {
  curves <- make_curves_fixture()
  result <- .project_aging_multiplier(NA, "WR", curves, 1L, 10.0, FALSE)
  expect_equal(result, 1.0)
})

test_that("B2: returns 1.0 when posterior_mu is 0", {
  curves <- make_curves_fixture()
  result <- .project_aging_multiplier(25L, "WR", curves, 1L, 0.0, FALSE)
  expect_equal(result, 1.0)
})

test_that("B3: returns 1.0 when posterior_mu is NA", {
  curves <- make_curves_fixture()
  result <- .project_aging_multiplier(25L, "WR", curves, 1L, NA_real_, FALSE)
  expect_equal(result, 1.0)
})

test_that("B4: returns 1.0 when position is not in curves (e.g. K)", {
  curves <- make_curves_fixture()
  result <- .project_aging_multiplier(25L, "K", curves, 1L, 10.0, FALSE)
  expect_equal(result, 1.0)
})

test_that("B5: returns multiplier > 1 for young player on climbing dynasty arc", {
  curves <- make_curves_fixture()
  # Age 23, dynasty (3 seasons). Curve: 23->0, 24->1, 25->2, 26->1.
  # Deltas: 1, 2, 1. Mean = 1.333. Mult = 1 + 1.333/10 = 1.133.
  result <- .project_aging_multiplier(23L, "WR", curves, 3L, 10.0, TRUE)
  expect_gt(result, 1.0)
  expect_lte(result, AGING_MULTIPLIER_DYNASTY_CEILING)
})

test_that("B6: returns multiplier < 1 for old player on declining arc", {
  curves <- make_curves_fixture()
  # Age 31, r32_posterior_mu = 10. Curve: 31->-4, 32->-5, 33->-6.
  # Dynasty deltas: -1, -2, -3. Mean = -2. Mult = 1 + (-2)/10 = 0.8.
  result <- .project_aging_multiplier(31L, "RB", curves, 3L, 10.0, TRUE)
  expect_lt(result, 1.0)
  expect_gte(result, AGING_MULTIPLIER_FLOOR)
})

test_that("B7: clips to AGING_MULTIPLIER_FLOOR for extreme decline", {
  curves <- make_curves_fixture()
  # Age 33, tiny posterior_mu drives mult far below floor.
  # Curve: 33->-6, 34->-7, 35->-8. Deltas: -1, -2, -2 (35 clamped twice).
  # Mean = -1.667. Mult = 1 + (-1.667)/0.5 = -2.33 -> clamped to 0.40.
  result <- .project_aging_multiplier(33L, "RB", curves, 3L, 0.5, TRUE)
  expect_equal(result, AGING_MULTIPLIER_FLOOR)
})

test_that("B8: clips to respective ceiling for extreme youth upside", {
  curves <- make_curves_fixture()
  # Same age 23 youth curve but tiny posterior_mu makes mult very large.
  # Redraft: 1 season, delta = 1. Mult = 1 + 1/0.5 = 3.0 -> capped at 1.05.
  redraft_result <- .project_aging_multiplier(23L, "WR", curves, 1L, 0.5, FALSE)
  expect_equal(redraft_result, AGING_MULTIPLIER_REDRAFT_CEILING)
  # Dynasty: mean_delta = 1.333. Mult = 1 + 1.333/0.5 = 3.67 -> capped at 1.35.
  dynasty_result <- .project_aging_multiplier(23L, "WR", curves, 3L, 0.5, TRUE)
  expect_equal(dynasty_result, AGING_MULTIPLIER_DYNASTY_CEILING)
})


# ==============================================================================
# SECTION C: .scale_prospect_to_vorp_units()
# ==============================================================================

test_that("C1: highest-scored rookie maps to highest veteran VORP at that position", {
  rookies <- make_rookies_fixture(3L)
  vorp    <- make_vorp_fixture()
  result  <- .scale_prospect_to_vorp_units(rookies, "WR", vorp)
  # WR veterans sorted by adjusted_vorp: V-2 (4.0), V-3 (2.0).
  # Rookie rank 1 (score=90) -> vet_vorp[1] = 4.0.
  expect_equal(result[1], 4.0)
})

test_that("C2: second-ranked rookie maps to second veteran VORP", {
  rookies <- make_rookies_fixture(3L)
  vorp    <- make_vorp_fixture()
  result  <- .scale_prospect_to_vorp_units(rookies, "WR", vorp)
  # Rookie rank 2 (score=60) -> vet_vorp[2] = 2.0.
  expect_equal(result[2], 2.0)
})

test_that("C3: surplus rookies beyond veteran count floor at minimum veteran VORP", {
  rookies <- make_rookies_fixture(3L)
  vorp    <- make_vorp_fixture()
  result  <- .scale_prospect_to_vorp_units(rookies, "WR", vorp)
  # Only 2 WR vets; rookie rank 3 (score=30) -> min(4.0, 2.0) = 2.0.
  expect_equal(result[3], min(c(4.0, 2.0)))
})

test_that("C4: returns NA vector when no veterans at that position", {
  rookies <- make_rookies_fixture(2L)
  vorp    <- make_vorp_fixture()
  result  <- .scale_prospect_to_vorp_units(rookies, "K", vorp)
  expect_equal(length(result), nrow(rookies))
  expect_true(all(is.na(result)))
})

test_that("C5: return length equals nrow(rookies)", {
  rookies <- make_rookies_fixture(3L)
  vorp    <- make_vorp_fixture()
  result  <- .scale_prospect_to_vorp_units(rookies, "WR", vorp)
  expect_equal(length(result), nrow(rookies))
})


# ==============================================================================
# SECTION D: .assign_tiers_and_ranks()
# ==============================================================================

test_that("D1: players with trade_value == 0 receive Depth tier", {
  lt <- make_league_tv_fixture()
  result <- .assign_tiers_and_ranks(lt)
  zero_tiers <- result$tv_tier[result$trade_value == 0]
  expect_true(all(zero_tiers == "Depth"))
})

test_that("D2: highest trade_value player receives Elite tier", {
  lt <- make_league_tv_fixture()
  result <- .assign_tiers_and_ranks(lt)
  top_tier <- result$tv_tier[result$trade_value == max(result$trade_value)]
  expect_equal(top_tier, "Elite")
})

test_that("D3: overall_tv_rank is 1 for the highest trade_value player", {
  lt <- make_league_tv_fixture()
  result <- .assign_tiers_and_ranks(lt)
  best_rank <- result$overall_tv_rank[result$trade_value == max(result$trade_value)]
  expect_equal(best_rank, 1L)
})

test_that("D4: position_tv_rank resets to 1 for each position", {
  lt <- make_league_tv_fixture()
  result <- .assign_tiers_and_ranks(lt)
  min_pos_ranks <- result %>%
    dplyr::group_by(position) %>%
    dplyr::summarise(min_rank = min(position_tv_rank), .groups = "drop")
  expect_true(all(min_pos_ranks$min_rank == 1L))
})

test_that("D5: tier labels are drawn from TV_TIER_LABELS", {
  lt <- make_league_tv_fixture()
  result <- .assign_tiers_and_ranks(lt)
  expect_true(all(result$tv_tier %in% TV_TIER_LABELS))
})

test_that("D6: output contains overall_tv_rank and position_tv_rank columns", {
  lt <- make_league_tv_fixture()
  result <- .assign_tiers_and_ranks(lt)
  expect_true("overall_tv_rank"  %in% names(result))
  expect_true("position_tv_rank" %in% names(result))
})

test_that("D7: handles all-zero trade_values without error", {
  lt <- make_league_tv_fixture()
  lt$trade_value <- 0
  expect_no_error(.assign_tiers_and_ranks(lt))
  result <- .assign_tiers_and_ranks(lt)
  expect_true(all(result$tv_tier == "Depth"))
})


# ==============================================================================
# SECTION E: compute_player_trade_values()
# ==============================================================================

test_that("E1: output row count equals veterans plus non-overlapping rookies", {
  vorp    <- make_vorp_fixture()
  rookies <- make_rookies_fixture(3L)
  curves  <- make_curves_fixture()
  result  <- compute_player_trade_values(vorp, rookies, curves, FALSE)
  # All 3 rookies are WRs not already in vorp (different gsis IDs).
  expect_equal(nrow(result), nrow(vorp) + nrow(rookies))
})

test_that("E2: no negative trade_value in output (pmax floor)", {
  vorp    <- make_vorp_fixture()
  rookies <- make_rookies_fixture(1L)
  curves  <- make_curves_fixture()
  # Inject a deeply negative adjusted_vorp to trigger the pmax guard.
  vorp$adjusted_vorp[1] <- -20.0
  result <- compute_player_trade_values(vorp, rookies, curves, FALSE)
  expect_true(all(result$trade_value >= 0, na.rm = TRUE))
})

test_that("E3: veterans have rookie_flag = FALSE", {
  vorp    <- make_vorp_fixture()
  rookies <- make_rookies_fixture(2L)
  curves  <- make_curves_fixture()
  result  <- compute_player_trade_values(vorp, rookies, curves, FALSE)
  vet_flags <- result$rookie_flag[result$nfl_gsis_id %in% vorp$nfl_gsis_id]
  expect_true(all(!vet_flags))
})

test_that("E4: rookie rows have rookie_flag = TRUE", {
  vorp    <- make_vorp_fixture()
  rookies <- make_rookies_fixture(2L)
  curves  <- make_curves_fixture()
  result  <- compute_player_trade_values(vorp, rookies, curves, FALSE)
  rk_flags <- result$rookie_flag[result$nfl_gsis_id %in% rookies$nfl_gsis_id]
  expect_true(all(rk_flags))
})

test_that("E5: veterans get non-NA aging_multiplier; rookies get NA", {
  vorp    <- make_vorp_fixture()
  rookies <- make_rookies_fixture(2L)
  curves  <- make_curves_fixture()
  result  <- compute_player_trade_values(vorp, rookies, curves, FALSE)
  vet_mult <- result$aging_multiplier[!result$rookie_flag]
  rk_mult  <- result$aging_multiplier[result$rookie_flag]
  expect_true(all(!is.na(vet_mult)))
  expect_true(all(is.na(rk_mult)))
})

test_that("E6: dynasty run produces different multipliers than redraft for young player", {
  vorp    <- make_vorp_fixture()
  rookies <- make_rookies_fixture(1L)
  curves  <- make_curves_fixture()
  # V-2 is WR age 23 with positive VORP; dynasty and redraft use different horizons.
  tv_rd <- compute_player_trade_values(vorp, rookies, curves, FALSE)
  tv_dy <- compute_player_trade_values(vorp, rookies, curves, TRUE)
  rd_mult <- tv_rd$aging_multiplier[tv_rd$nfl_gsis_id == "V-2"]
  dy_mult <- tv_dy$aging_multiplier[tv_dy$nfl_gsis_id == "V-2"]
  expect_false(isTRUE(all.equal(rd_mult, dy_mult)))
})

test_that("E7: required output columns are all present", {
  vorp    <- make_vorp_fixture()
  rookies <- make_rookies_fixture(1L)
  curves  <- make_curves_fixture()
  result  <- compute_player_trade_values(vorp, rookies, curves, FALSE)
  required <- c(
    "nfl_gsis_id", "player_name", "team", "position",
    "league_name", "league_format",
    "r32_posterior_mu", "r32_projection_upper_80",
    "adjusted_vorp", "age_at_season_start",
    "aging_multiplier", "rookie_flag", "prospect_score_final", "trade_value"
  )
  expect_true(all(required %in% names(result)))
})

test_that("E8: rookies already in vorp gsis_ids are excluded from rookie rows", {
  vorp <- make_vorp_fixture()
  # Inject a rookie whose gsis_id overlaps with a veteran.
  rookies <- tibble::tibble(
    nfl_gsis_id    = c("V-2", "R-99"),     # V-2 is already a vet
    player_name    = c("Dup Vet", "New Rk"),
    position       = c("WR", "WR"),
    score_final    = c(80, 70),
    interval_width = c(NA_real_, NA_real_)
  )
  curves <- make_curves_fixture()
  result <- compute_player_trade_values(vorp, rookies, curves, FALSE)
  # Only R-99 should appear as a rookie; V-2 stays on veteran path.
  rk_ids <- result$nfl_gsis_id[result$rookie_flag]
  expect_false("V-2" %in% rk_ids)
  expect_true("R-99" %in% rk_ids)
})


# ==============================================================================
# SECTION F: evaluate_trade()
# ==============================================================================

test_that("F1: returns WIN when receive_value exceeds give_value by more than band", {
  tv <- make_trade_values_fixture()
  # Give V-1 (5.0); receive V-2 + V-5 (4.0 + 2.1 = 6.1); delta = 1.1 > 0.5.
  res <- evaluate_trade(
    give_ids     = "V-1",
    receive_ids  = c("V-2", "V-5"),
    trade_values = tv,
    league_name  = "Test League"
  )
  expect_equal(res$verdict, "WIN")
})

test_that("F2: returns LOSS when give_value exceeds receive_value by more than band", {
  tv <- make_trade_values_fixture()
  # Give V-2 + V-5 (6.1); receive V-1 (5.0); delta = -1.1 < -0.5.
  res <- evaluate_trade(
    give_ids     = c("V-2", "V-5"),
    receive_ids  = "V-1",
    trade_values = tv,
    league_name  = "Test League"
  )
  expect_equal(res$verdict, "LOSS")
})

test_that("F3: returns ROUGHLY EVEN when values are within TRADE_VERDICT_BAND", {
  tv <- make_trade_values_fixture()
  # Give V-4 (2.0); receive V-5 (2.1); delta = 0.1 < 0.5.
  res <- evaluate_trade(
    give_ids     = "V-4",
    receive_ids  = "V-5",
    trade_values = tv,
    league_name  = "Test League"
  )
  expect_equal(res$verdict, "ROUGHLY EVEN")
})

test_that("F4: unknown player ID in give_ids produces a warning, not an error", {
  tv <- make_trade_values_fixture()
  expect_warning(
    evaluate_trade("UNKNOWN-999", "V-2", tv, "Test League"),
    regexp = "UNKNOWN-999"
  )
})

test_that("F5: unknown player ID in receive_ids produces a warning, not an error", {
  tv <- make_trade_values_fixture()
  expect_warning(
    evaluate_trade("V-1", c("V-2", "UNKNOWN-999"), tv, "Test League"),
    regexp = "UNKNOWN-999"
  )
})

test_that("F6: give_value is correct sum of given players' trade values", {
  tv <- make_trade_values_fixture()
  res <- evaluate_trade("V-1", "V-2", tv, "Test League")
  expect_equal(res$give_value, 5.0)
})

test_that("F7: receive_value is correct sum of received players' trade values", {
  tv <- make_trade_values_fixture()
  res <- evaluate_trade("V-1", c("V-2", "V-5"), tv, "Test League")
  expect_equal(res$receive_value, 4.0 + 2.1)
})

test_that("F8: net_delta is receive_value minus give_value", {
  tv <- make_trade_values_fixture()
  res <- evaluate_trade("V-1", c("V-2", "V-5"), tv, "Test League")
  expect_equal(res$net_delta, res$receive_value - res$give_value)
})

test_that("F9: result list contains all required elements", {
  tv  <- make_trade_values_fixture()
  res <- evaluate_trade("V-1", "V-2", tv, "Test League")
  expected_names <- c("give_value", "receive_value", "net_delta",
                      "give_detail", "receive_detail", "verdict")
  expect_true(all(expected_names %in% names(res)))
})

test_that("F10: NULL league_name uses the first league in trade_values", {
  tv <- make_trade_values_fixture()
  # Works without specifying league_name -- should default to "Test League".
  res <- evaluate_trade("V-1", "V-2", tv, league_name = NULL)
  expect_equal(res$verdict, "LOSS")  # give 5.0, receive 4.0 -> LOSS
})


# ==============================================================================
# SECTION G: schema and type validation
# ==============================================================================

test_that("G1: compute_player_trade_values output columns are all correct types", {
  vorp    <- make_vorp_fixture()
  rookies <- make_rookies_fixture(1L)
  curves  <- make_curves_fixture()
  result  <- compute_player_trade_values(vorp, rookies, curves, FALSE)
  expect_type(result$trade_value,     "double")
  expect_type(result$aging_multiplier, "double")
  expect_type(result$rookie_flag,     "logical")
  expect_type(result$nfl_gsis_id,     "character")
})

test_that("G2: evaluate_trade detail tibbles contain required columns", {
  tv  <- make_trade_values_fixture()
  res <- evaluate_trade("V-1", "V-2", tv, "Test League")
  expect_true(all(c("player_name", "position", "trade_value", "tv_tier") %in%
                    names(res$give_detail)))
  expect_true(all(c("player_name", "position", "trade_value", "tv_tier") %in%
                    names(res$receive_detail)))
})

test_that("G3: trade_value is non-negative for all positive adjusted_vorp players", {
  vorp    <- make_vorp_fixture()
  rookies <- make_rookies_fixture(0L)
  curves  <- make_curves_fixture()
  result  <- compute_player_trade_values(vorp, rookies, curves, FALSE)
  pos_vets <- result[!result$rookie_flag & result$adjusted_vorp > 0, ]
  expect_true(all(pos_vets$trade_value >= 0))
})

test_that("G4: .assign_tiers_and_ranks preserves input row count", {
  lt     <- make_league_tv_fixture()
  result <- .assign_tiers_and_ranks(lt)
  expect_equal(nrow(result), nrow(lt))
})


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n================================================================\n")
cat("TEST SUITE: test_season2_week17_functions.R\n")
cat("================================================================\n")
cat("Functions under test:\n")
cat("  .curve_value_at_age()            Section A  6 tests\n")
cat("  .project_aging_multiplier()      Section B  8 tests\n")
cat("  .scale_prospect_to_vorp_units()  Section C  5 tests\n")
cat("  .assign_tiers_and_ranks()        Section D  7 tests\n")
cat("  compute_player_trade_values()    Section E  8 tests\n")
cat("  evaluate_trade()                 Section F 10 tests\n")
cat("  schema and type validation       Section G  4 tests\n")
cat("  -----------------------------------------------\n")
cat("  Total                                       48 tests\n")
cat("================================================================\n")
cat("No network calls. All fixtures are synthetic in-memory data.\n")
cat("================================================================\n\n")
