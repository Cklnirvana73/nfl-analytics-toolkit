# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 17
# Usage Examples
# File: examples/example_season2_week17.R
#
# Purpose: Self-contained examples demonstrating the Week 17 trade value model.
#          Examples 1 through 6 run offline against the cached projection stack
#          and set the dynasty flag manually, so no live Sleeper call is needed.
#          Example 7 shows the full interactive build_trade_value_table() call,
#          which does reach Sleeper to enumerate leagues and detect dynasty.
#
# Prerequisites (all produced by earlier weeks, present in season2_cache):
#   - data/season2_cache/s2_week15_vorp_rankings.rds          (R/33)
#   - data/season2_cache/s2_week14_final_prospect_scores.csv  (R/28)
#   - data/season2_cache/s2_week15_reconciled_projections.rds (R/32, optional)
#   - data/season2_cache/s2_week15_aging_curves_cache.rds     (R/29; if absent,
#     SETUP refits from R/23, which takes several minutes the first time)
#
# Run order: source this file top to bottom, or run examples independently
# after the SETUP block.
#
# Production-grade for portfolio display. Built for personal analytics use.
# ==============================================================================

library(here)
library(dplyr)
library(glue)

# Source the production file (auto-sources R/19 and R/23 dependencies).
source(here::here("R", "37_trade_value_model.R"))


# ==============================================================================
# SETUP: Load the cached inputs once, shared by the examples below
# ==============================================================================
# Mirrors the loading the orchestrator does internally, exposed here so each
# example can operate on real data without re-reading the cache every time.
# ==============================================================================

cat("\n================================================================\n")
cat("SETUP: Loading cached projection stack\n")
cat("================================================================\n\n")

vorp_all <- readRDS(VORP_CACHE_PATH)
cat("VORP rankings rows:", format(nrow(vorp_all), big.mark = ","), "\n")
cat("Leagues in cache:", paste(unique(vorp_all$league_name), collapse = ", "),
    "\n")

# Attach ages once (Sept 1 of SEASON convention, same as R/23).
vorp_all <- .attach_player_ages(vorp_all, season = SEASON)
cat("Veterans with a known age:",
    sum(!is.na(vorp_all$age_at_season_start)), "of", nrow(vorp_all), "\n")

# Aging curves (cache or fallback refit).
curves <- .load_aging_curves_with_fallback(season = AGING_PANEL_LAST_SEASON,
                                            verbose = TRUE)
cat("Aging curve positions:", paste(names(curves), collapse = ", "), "\n")

# Prospect (rookie) prediction cohort.
prospects_raw <- readr::read_csv(PROSPECT_CSV_PATH, show_col_types = FALSE)
if (!"nfl_gsis_id" %in% names(prospects_raw)) {
  prospects_raw$nfl_gsis_id <- NA_character_
}
rookies_all <- prospects_raw %>%
  dplyr::filter(draft_class_type == "prediction", !is.na(score_final)) %>%
  dplyr::transmute(nfl_gsis_id, player_name = cfb_player_name,
                   position, score_final)
rookies_all$interval_width <- NA_real_
cat("Rookie prediction-cohort players:", nrow(rookies_all), "\n")

# Pick the first league in the cache as the working example league.
example_league <- unique(vorp_all$league_name)[1]
vorp_one <- dplyr::filter(vorp_all, league_name == example_league)
cat("Working league for examples:", example_league,
    "(", nrow(vorp_one), "players )\n\n")


# ==============================================================================
# EXAMPLE 1: Aging curve cache freshness
# ==============================================================================
# The aging curves are a historical model fit, so they do not expire on a
# clock. They are stale only if the cache is absent or was built for a
# different last-completed season than AGING_PANEL_LAST_SEASON.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 1: Aging curve cache freshness\n")
cat("================================================================\n\n")

cat("Staleness anchor (last completed season):", AGING_PANEL_LAST_SEASON, "\n")
cat("Decision / projection season (SEASON):", SEASON, "\n")
cat("Curve positions loaded:", paste(names(curves), collapse = ", "), "\n")

qb_curve <- curves[["QB"]]$curve_data
cat("\nQB curve age range:",
    min(qb_curve$age_at_season_start), "to",
    max(qb_curve$age_at_season_start), "\n")
cat("QB peak age (LOESS):", curves[["QB"]]$peak_age_loess, "\n")

# KEY INSIGHT: the curve value is a cumulative fppg delta anchored at age 23.
# Read it directly at a couple of ages to confirm the shape.
for (a in c(23, 27, 32)) {
  v <- .curve_value_at_age(qb_curve, a)
  cat(glue("  QB cumulative fppg delta at age {a}: ",
           "{format(round(v, 2), nsmall = 2)}"), "\n")
}


# ==============================================================================
# EXAMPLE 2: The aging multiplier, young vs old, redraft vs dynasty
# ==============================================================================
# The multiplier is 1 + (avg expected fppg change over the horizon) divided by
# the player's own projection. A young player on the climb sits above 1.0; an
# older player on the decline sits below it. Dynasty uses a 3-season horizon,
# redraft a 1-season horizon.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 2: Aging multiplier across age and format\n")
cat("================================================================\n\n")

demo_mu <- 15  # a mid-tier absolute projection in fppg, for illustration
for (pos in c("RB", "WR")) {
  cat(glue("Position: {pos} (projection held at {demo_mu} fppg)"), "\n")
  for (age in c(23L, 27L, 31L)) {
    m_redraft <- .project_aging_multiplier(age, pos, curves,
                                           N_SEASONS_REDRAFT, demo_mu,
                                           is_dynasty = FALSE)
    m_dynasty <- .project_aging_multiplier(age, pos, curves,
                                           N_SEASONS_DYNASTY, demo_mu,
                                           is_dynasty = TRUE)
    cat(glue("  age {age}: redraft x{format(round(m_redraft, 3), nsmall = 3)} ",
             "| dynasty x{format(round(m_dynasty, 3), nsmall = 3)}"), "\n")
  }
  cat("\n")
}

# KEY INSIGHT: the redraft and dynasty multipliers diverge most at the age
# extremes, where the multi-season trajectory carries the older player further
# down and the younger player further up than a single season does.


# ==============================================================================
# EXAMPLE 3: Trade values for one league (redraft branch)
# ==============================================================================
# compute_player_trade_values() bypasses the interactive menu. Here it runs on
# the working league with the dynasty flag set manually to FALSE (redraft).
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 3: Trade values for one league (redraft)\n")
cat("================================================================\n\n")

tv_redraft <- compute_player_trade_values(
  vorp_one_league   = vorp_one,
  rookies           = rookies_all,
  curves            = curves,
  league_is_dynasty = FALSE
)
tv_redraft <- .assign_tiers_and_ranks(tv_redraft)

cat("Players valued:", nrow(tv_redraft),
    "| rookies:", sum(tv_redraft$rookie_flag), "\n\n")
cat("Top 10 by trade value (redraft):\n")
tv_redraft %>%
  dplyr::arrange(overall_tv_rank) %>%
  dplyr::slice_head(n = 10L) %>%
  dplyr::transmute(
    rank = overall_tv_rank, player_name, position,
    vorp = round(adjusted_vorp, 1),
    mult = round(aging_multiplier, 3),
    trade_value = round(trade_value, 1), tier = tv_tier
  ) %>%
  print(n = 10)


# ==============================================================================
# EXAMPLE 4: Same league, dynasty branch, and the value swing
# ==============================================================================
# Running the same league as dynasty changes the horizon to 3 seasons and
# rewards youth. Comparing the two shows which players the format favors.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 4: Redraft vs dynasty value swing\n")
cat("================================================================\n\n")

tv_dynasty <- compute_player_trade_values(
  vorp_one_league   = vorp_one,
  rookies           = rookies_all,
  curves            = curves,
  league_is_dynasty = TRUE
)
tv_dynasty <- .assign_tiers_and_ranks(tv_dynasty)

swing <- tv_redraft %>%
  dplyr::select(nfl_gsis_id, player_name, position,
                age_at_season_start, tv_redraft = trade_value) %>%
  dplyr::inner_join(
    tv_dynasty %>% dplyr::select(nfl_gsis_id, tv_dynasty = trade_value),
    by = "nfl_gsis_id"
  ) %>%
  dplyr::mutate(delta = tv_dynasty - tv_redraft)

cat("Biggest dynasty gainers (youth rewarded):\n")
swing %>%
  dplyr::arrange(dplyr::desc(delta)) %>%
  dplyr::slice_head(n = 5L) %>%
  dplyr::transmute(player_name, position, age = age_at_season_start,
                   tv_redraft = round(tv_redraft, 1),
                   tv_dynasty = round(tv_dynasty, 1),
                   delta = round(delta, 1)) %>%
  print(n = 5)

cat("\nBiggest dynasty fallers (age discounted):\n")
swing %>%
  dplyr::arrange(delta) %>%
  dplyr::slice_head(n = 5L) %>%
  dplyr::transmute(player_name, position, age = age_at_season_start,
                   tv_redraft = round(tv_redraft, 1),
                   tv_dynasty = round(tv_dynasty, 1),
                   delta = round(delta, 1)) %>%
  print(n = 5)

# KEY INSIGHT: the gainers and fallers are sorted by age, which is the model
# working as intended. The dynasty branch is not re-ranking on a whim; it is
# applying the multi-season aging trajectory.


# ==============================================================================
# EXAMPLE 5: Where the rookies land
# ==============================================================================
# Rookies are not in the VORP rankings, so they enter as new rows with values
# rescaled from their prospect score and discounted for unproven risk. They
# should generally sit below established veterans at the same position rank.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 5: Rookie placement (dynasty)\n")
cat("================================================================\n\n")

rookies_placed <- tv_dynasty %>%
  dplyr::filter(rookie_flag) %>%
  dplyr::arrange(overall_tv_rank)

cat("Rookies entering the dynasty table:", nrow(rookies_placed), "\n\n")
if (nrow(rookies_placed) > 0L) {
  cat("Top rookies by trade value:\n")
  rookies_placed %>%
    dplyr::slice_head(n = 8L) %>%
    dplyr::transmute(
      overall_rank = overall_tv_rank, player_name, position,
      prospect_score = round(prospect_score_final, 1),
      trade_value = round(trade_value, 1), tier = tv_tier
    ) %>%
    print(n = 8)
} else {
  cat("No prediction-cohort rookies matched this league's positions.\n")
}


# ==============================================================================
# EXAMPLE 6: Evaluate a concrete trade
# ==============================================================================
# evaluate_trade() sums each side and returns a verdict. Pure function: no
# I/O. Here we trade the league's overall #1 for the #3 plus #15, to show a
# realistic two-for-one.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 6: Evaluate a two-for-one trade\n")
cat("================================================================\n\n")

ranked <- tv_dynasty %>% dplyr::arrange(overall_tv_rank)
give_player    <- ranked$nfl_gsis_id[1]
receive_a      <- ranked$nfl_gsis_id[3]
receive_b      <- ranked$nfl_gsis_id[15]

# evaluate_trade keys off league_name in the table, so tag it on.
tv_dynasty$league_name <- example_league

res <- evaluate_trade(
  give_ids     = give_player,
  receive_ids  = c(receive_a, receive_b),
  trade_values = tv_dynasty,
  league_name  = example_league
)

cat("Giving away:\n"); print(res$give_detail)
cat("\nReceiving:\n"); print(res$receive_detail)
cat(glue("\nGive value:    {format(round(res$give_value, 1), nsmall = 1)}"), "\n")
cat(glue("Receive value: {format(round(res$receive_value, 1), nsmall = 1)}"), "\n")
cat(glue("Net delta:     {format(round(res$net_delta, 1), nsmall = 1)}"), "\n")
cat(glue("Verdict:       {res$verdict}"), "\n")

rm(tv_redraft, tv_dynasty, swing, ranked, res)
gc(verbose = FALSE)


# ==============================================================================
# EXAMPLE 7: The full interactive build (reaches Sleeper)
# ==============================================================================
# This is the normal entry point. It enumerates the user's leagues, prints a
# numbered menu, detects dynasty per league via taxi_slots, and builds the
# table for the leagues chosen. Commented out so sourcing this file does not
# block on the menu; uncomment and supply a real Sleeper username to run it.
# ==============================================================================

cat("\n================================================================\n")
cat("EXAMPLE 7: Full interactive build (commented; needs a username)\n")
cat("================================================================\n\n")

# tv <- build_trade_value_table(username = "GABlancbeard")
# head(tv, 20)
#
# # Then evaluate a trade in one of the built leagues:
# res <- evaluate_trade(
#   give_ids     = c("00-0036322"),
#   receive_ids  = c("00-0033873", "00-0034796"),
#   trade_values = tv,
#   league_name  = "Dynasty Main"
# )
# res$verdict

cat("See the commented block above to run the full interactive build.\n\n")
