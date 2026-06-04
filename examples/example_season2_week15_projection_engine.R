# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Example: In-Season Projection Engine -- 2026 Preseason Baseline
# File: examples/example_season2_week15_projection_engine.R
#
# PURPOSE
# -------
# Runs the full R/29 projection engine for the 2026 NFL season at Week 1
# (preseason baseline -- no regular season games played yet).
#
# Because no 2026 in-season pbp exists yet, every player's posterior equals
# their prior. This is the correct behavior: the engine starts with what it
# knows before any games are played and will update weekly once the season
# begins. The output is the 2026 preseason projection baseline you will use
# to evaluate roster composition, ADP value, and FantasyPros consensus gaps.
#
# WHAT THIS SCRIPT SHOWS
# ----------------------
#   Section 1: Source dependencies and engine load
#   Section 2: Run the full pipeline (builds priors, pulls FP ADP + consensus,
#              DEF/ST scoring, writes CSVs)
#   Section 3: Prior source breakdown (how many players on each prior type)
#   Section 4: Top projections by position
#   Section 5: ADP value leaders (who is underpriced vs FantasyPros ADP)
#   Section 6: Consensus delta leaders (where our engine disagrees with FP)
#   Section 7: Rookie/early-career projection preview (translation-only priors)
#   Section 8: DEF/ST preseason scoring context
#
# PRESEASON BASELINE CONTEXT
# --------------------------
# At Week 1 compute_prior_weight(1) = 1.0 -- the prior has full weight and
# no YTD observed data exists. Projection intervals will be wider than at
# mid-season; uncertainty is highest before the season starts. This is
# intentional and correct. The intervals narrow weekly as observed data
# accumulates and prior weight decays toward 0.05 by Week 18.
#
# RUN SEQUENCE (source these first, then this script):
#   source(here::here("R", "15_multi_season_pbp.R"))
#   source(here::here("R", "17_extended_scoring.R"))
#   source(here::here("R", "29_projection_engine.R"))
#   source(here::here("examples", "example_season2_week15_projection_engine.R"))
#
# ==============================================================================

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(glue)

# Source dependencies in order -- R/29 requires R/15 and R/17 to be loaded first
source(here::here("R", "15_multi_season_pbp.R"))
source(here::here("R", "17_extended_scoring.R"))
source(here::here("R", "29_projection_engine.R"))

# ==============================================================================
# CONFIGURATION -- edit these before running
# ==============================================================================

if (!exists("PROJECTION_SEASON")) PROJECTION_SEASON <- 2026L
if (!exists("PROJECTION_WEEK"))   PROJECTION_WEEK   <- 1L

# Set TRUE to force a fresh FantasyPros pull regardless of cache age.
# Use this when consensus data looks stale or after changing the season.
# To override without editing this file, run in the console first:
#   FORCE_FP_REFRESH <- TRUE
#   source(here::here("examples", "example_season2_week15_projection_engine.R"))
if (!exists("FORCE_FP_REFRESH")) FORCE_FP_REFRESH <- FALSE

# ==============================================================================
# SECTION 1: VERIFY DEPENDENCIES
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 1: Verifying dependencies\n")
cat(strrep("=", 70), "\n\n")

required_functions <- c(
  "load_normalized_season",
  "calculate_fantasy_points_ext",
  "compute_prior_weight",
  "load_adp_fantasypros",
  "load_consensus_projections_fantasypros",
  "build_projection_priors",
  "update_projection_with_ytd",
  "calculate_projection_intervals",
  "calculate_def_st_points",
  "run_projection_engine"
)

missing_fns <- required_functions[!sapply(required_functions, exists)]

if (length(missing_fns) > 0) {
  stop(
    "Missing required functions: ", paste(missing_fns, collapse = ", "),
    "\nSource R/15, R/17, and R/29 before running this example.",
    call. = FALSE
  )
}

cat("All 10 required functions present.\n")

required_inputs <- c(
  here::here("data", "season2_cache", "s2_week14_final_prospect_scores.csv"),
  here::here("data", "season2_cache", "s2_week10_predictions.rds")
)

for (path in required_inputs) {
  status <- if (file.exists(path)) "FOUND" else "MISSING"
  cat(glue("  [{status}] {basename(path)}\n"))
}

missing_inputs <- required_inputs[!sapply(required_inputs, file.exists)]
if (length(missing_inputs) > 0) {
  stop(
    "Missing required input files:\n",
    paste("  ", missing_inputs, collapse = "\n"),
    call. = FALSE
  )
}

cat("\nPrior weight at Week 1 (preseason): ",
    compute_prior_weight(1), "(full prior -- no YTD yet)\n")
cat("Prior weight at Week 9 (crossover): ",
    round(compute_prior_weight(9), 3), "\n")
cat("Prior weight at Week 18 (floor):    ",
    compute_prior_weight(18), "\n\n")

# ==============================================================================
# SECTION 2: RUN THE PROJECTION ENGINE
# ==============================================================================

cat(strrep("=", 70), "\n")
cat(glue("SECTION 2: Running the {PROJECTION_SEASON} projection engine ",
         "(Week {PROJECTION_WEEK})\n"))
cat(strrep("=", 70), "\n\n")

# NOTE: No 2026 pbp cache exists yet (regular season has not started).
# The engine will load historical data (2023-2025) for prior construction
# and position baselines. YTD stats will be empty, so every posterior
# equals its prior. This is the correct preseason behavior.

cat(glue("Season: {PROJECTION_SEASON}\n"))
cat(glue("Week:   {PROJECTION_WEEK} (preseason baseline -- full prior weight)\n"))
cat(glue("Force FP refresh: {FORCE_FP_REFRESH}\n\n"))

results <- run_projection_engine(
  season           = PROJECTION_SEASON,
  week             = PROJECTION_WEEK,
  scoring_settings = DEFAULT_SCORING_SETTINGS,
  write_outputs    = TRUE,
  force_fp_refresh = FORCE_FP_REFRESH
)

proj  <- results$projections
def   <- results$def_st
bases <- results$baselines

cat(glue("\nEngine complete.\n"))
cat(glue("  Players projected:      {results$n_players_projected}\n"))
cat(glue("  ADP match rate:         {round(100 * results$fp_match_rate_adp, 1)}%\n"))
cat(glue("  Consensus match rate:   {round(100 * results$fp_match_rate_consensus, 1)}%\n"))
cat(glue("  Prior weight used:      {results$prior_weight_used} (Week {PROJECTION_WEEK})\n"))

# ==============================================================================
# SECTION 3: PRIOR SOURCE BREAKDOWN
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 3: Prior source breakdown\n")
cat(strrep("=", 70), "\n\n")

cat("How each player's prior was constructed:\n\n")

source_breakdown <- proj %>%
  dplyr::count(prior_source, name = "n_players") %>%
  dplyr::arrange(dplyr::desc(n_players)) %>%
  dplyr::mutate(
    description = dplyr::case_when(
      prior_source == "blended"               ~
        "R/28 + R/24 translation + NFL history (precision-weighted)",
      prior_source == "translation_only"      ~
        "R/28 + R/24 translation only (rookies / early-career)",
      prior_source == "history_only_fallback" ~
        "R/28 score_final + NFL history (no R/24 pred_base match)",
      prior_source == "score_final_fallback"  ~
        "R/28 score_final only (no R/24, no NFL history)",
      prior_source == "veteran_history_only"  ~
        "NFL history only (pre-2015 draftees, not in R/28)",
      TRUE ~ prior_source
    )
  )

for (i in seq_len(nrow(source_breakdown))) {
  cat(glue("  {source_breakdown$prior_source[i]} ({source_breakdown$n_players[i]} players)\n"))
  cat(glue("    {source_breakdown$description[i]}\n\n"))
}

cat("Prior source by position:\n\n")
print(
  proj %>%
    dplyr::count(position, prior_source) %>%
    tidyr::pivot_wider(names_from = prior_source, values_from = n,
                       values_fill = 0L) %>%
    dplyr::arrange(position)
)

cat("\nName resolution source breakdown:\n\n")
print(
  proj %>%
    dplyr::count(name_source, name = "n_players") %>%
    dplyr::arrange(dplyr::desc(n_players))
)
cat("\n")

# ==============================================================================
# SECTION 4: TOP PROJECTIONS BY POSITION
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SECTION 4: Top projections by position (2026 preseason baseline)\n")
cat(strrep("=", 70), "\n\n")

cat("NOTE: posterior_mu = prior_mu at Week 1 (no YTD data yet).\n")
cat("Intervals reflect prior uncertainty; they narrow as the season progresses.\n\n")

for (pos in c("QB", "RB", "WR", "TE")) {

  top_pos <- proj %>%
    dplyr::filter(position == pos) %>%
    dplyr::arrange(projected_rank) %>%
    dplyr::slice_head(n = 12L) %>%
    dplyr::transmute(
      rank          = projected_rank,
      player        = player_name,
      projection    = round(posterior_mu, 2),
      lower_80      = round(projection_lower_80, 2),
      upper_80      = round(projection_upper_80, 2),
      boom_pct      = round(boom_probability * 100, 1),
      bust_pct      = round(bust_probability * 100, 1),
      n_nfl_seasons = n_nfl_seasons,
      prior_source  = prior_source
    )

  pos_baseline <- bases %>%
    dplyr::filter(position == pos) %>%
    dplyr::pull(baseline_ppr_per_game) %>%
    round(2)

  cat(glue("--- {pos} (top 12 | position baseline: {pos_baseline} PPR/game) ---\n\n"))
  print(top_pos, n = 12L)
  cat("\n")
}

# ==============================================================================
# SECTION 5: ADP VALUE LEADERS
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("SECTION 5: ADP value leaders -- who is underpriced vs FantasyPros ADP\n")
cat(strrep("=", 70), "\n\n")

cat("value_score = adp_rank - projected_rank\n")
cat("Positive value_score = player projected higher than ADP suggests.\n\n")

adp_available <- !all(is.na(proj$adp_rank))

if (!adp_available) {
  cat("NOTE: ADP data not available (FantasyPros pull may have failed).\n")
  cat("Set force_fp_refresh = TRUE in run_projection_engine() to retry.\n\n")
} else {
  # Top overall value plays
  top_value <- proj %>%
    dplyr::filter(!is.na(value_score), !is.na(adp_rank)) %>%
    dplyr::arrange(dplyr::desc(value_score)) %>%
    dplyr::slice_head(n = 20L) %>%
    dplyr::transmute(
      position       = position,
      player         = player_name,
      adp_rank       = adp_rank,
      projected_rank = projected_rank,
      value_score    = value_score,
      projection     = round(posterior_mu, 2),
      prior_source   = prior_source
    )

  cat("Top 20 ADP value plays (all positions):\n\n")
  print(top_value, n = 20L)
  cat("\n")

  # Top value by position
  cat("Top 5 ADP value plays per position:\n\n")
  for (pos in c("QB", "RB", "WR", "TE")) {
    top_pos_value <- proj %>%
      dplyr::filter(position == pos, !is.na(value_score), !is.na(adp_rank)) %>%
      dplyr::arrange(dplyr::desc(value_score)) %>%
      dplyr::slice_head(n = 5L) %>%
      dplyr::transmute(
        player        = player_name,
        adp_rank      = adp_rank,
        proj_rank     = projected_rank,
        value_score   = value_score,
        projection    = round(posterior_mu, 2)
      )
    cat(glue("  {pos}:\n"))
    print(top_pos_value, n = 5L)
    cat("\n")
  }

  # Overpriced alert: engine projects lower than ADP suggests
  cat("Top 10 potential overpriced plays (negative value_score):\n\n")
  overpriced <- proj %>%
    dplyr::filter(!is.na(value_score), !is.na(adp_rank),
                  value_score < 0) %>%
    dplyr::arrange(value_score) %>%
    dplyr::slice_head(n = 10L) %>%
    dplyr::transmute(
      position       = position,
      player         = player_name,
      adp_rank       = adp_rank,
      projected_rank = projected_rank,
      value_score    = value_score,
      projection     = round(posterior_mu, 2)
    )
  print(overpriced, n = 10L)
  cat("\n")
}

# ==============================================================================
# SECTION 6: CONSENSUS DELTA -- WHERE OUR ENGINE DISAGREES WITH FANTASYPROS
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("SECTION 6: Consensus delta -- our engine vs FantasyPros consensus\n")
cat(strrep("=", 70), "\n\n")

cat("consensus_delta = posterior_mu - consensus_proj (PPR/game)\n")
cat("Positive = we project higher than FP consensus.\n")
cat("Negative = we project lower than FP consensus.\n\n")

consensus_available <- !all(is.na(proj$consensus_proj))

if (!consensus_available) {
  cat("NOTE: Consensus projection data not available.\n")
  cat("Set force_fp_refresh = TRUE in run_projection_engine() to retry.\n\n")
} else {
  # Summary by position
  cat("Consensus delta summary by position:\n\n")
  consensus_summary <- proj %>%
    dplyr::filter(!is.na(consensus_delta)) %>%
    dplyr::group_by(position) %>%
    dplyr::summarise(
      n_players     = dplyr::n(),
      mean_delta    = round(mean(consensus_delta, na.rm = TRUE), 2),
      sd_delta      = round(sd(consensus_delta, na.rm = TRUE), 2),
      n_bullish     = sum(consensus_delta > 0, na.rm = TRUE),
      n_bearish     = sum(consensus_delta < 0, na.rm = TRUE),
      max_bullish   = round(max(consensus_delta, na.rm = TRUE), 2),
      max_bearish   = round(min(consensus_delta, na.rm = TRUE), 2),
      .groups = "drop"
    )
  print(consensus_summary)
  cat("\n")

  # Biggest bulls (we like more than FP)
  cat("Top 15 bullish departures (engine > FP consensus):\n\n")
  bulls <- proj %>%
    dplyr::filter(!is.na(consensus_delta)) %>%
    dplyr::arrange(dplyr::desc(consensus_delta)) %>%
    dplyr::slice_head(n = 15L) %>%
    dplyr::transmute(
      position       = position,
      player         = player_name,
      our_proj       = round(posterior_mu, 2),
      fp_consensus   = round(consensus_proj, 2),
      delta          = round(consensus_delta, 2),
      prior_source   = prior_source
    )
  print(bulls, n = 15L)
  cat("\n")

  # Biggest bears (FP likes more than we do)
  cat("Top 15 bearish departures (engine < FP consensus):\n\n")
  bears <- proj %>%
    dplyr::filter(!is.na(consensus_delta)) %>%
    dplyr::arrange(consensus_delta) %>%
    dplyr::slice_head(n = 15L) %>%
    dplyr::transmute(
      position       = position,
      player         = player_name,
      our_proj       = round(posterior_mu, 2),
      fp_consensus   = round(consensus_proj, 2),
      delta          = round(consensus_delta, 2),
      prior_source   = prior_source
    )
  print(bears, n = 15L)
  cat("\n")
}

# ==============================================================================
# SECTION 7: ROOKIE AND EARLY-CAREER PROJECTION PREVIEW
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("SECTION 7: Rookie and early-career projection preview\n")
cat(strrep("=", 70), "\n\n")

cat("These players have priors anchored entirely or primarily to the\n")
cat("R/28 dynasty score + R/24 college translation model.\n")
cat("Their projections will shift most aggressively once 2026 games are played.\n\n")

translation_players <- proj %>%
  dplyr::filter(prior_source %in% c("translation_only",
                                     "score_final_fallback")) %>%
  dplyr::arrange(position, projected_rank) %>%
  dplyr::group_by(position) %>%
  dplyr::slice_head(n = 8L) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    position      = position,
    player        = player_name,
    proj_rank     = projected_rank,
    projection    = round(posterior_mu, 2),
    interval_80   = glue("[{round(projection_lower_80, 1)}, ",
                          "{round(projection_upper_80, 1)}]"),
    score_final   = round(score_final, 1),
    prior_source  = prior_source
  )

print(translation_players, n = 40L)

cat("\nNote: wider intervals on these players reflect translation model\n")
cat("uncertainty (R/27 holdout RMSE: QB=6.09, RB=4.13, WR=3.90, TE=2.58).\n")
cat("Uncertainty resolves quickly once early-season game data arrives.\n\n")

# ==============================================================================
# SECTION 8: DEF/ST PRESEASON SCORING CONTEXT
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("SECTION 8: DEF/ST preseason scoring context\n")
cat(strrep("=", 70), "\n\n")

if (nrow(def) == 0) {
  cat("No DEF/ST data available -- 2026 pbp not yet loaded.\n")
  cat("DEF/ST scoring will populate once the season begins.\n\n")
  cat("DEF/ST scoring tiers (Sleeper standard) for reference:\n")
  cat("  Shutout (0 pts allowed):     10 pts\n")
  cat("  1-6 pts allowed:              7 pts\n")
  cat("  7-13 pts allowed:             4 pts\n")
  cat("  14-20 pts allowed:            1 pt\n")
  cat("  21-27 pts allowed:            0 pts\n")
  cat("  28-34 pts allowed:           -1 pt\n")
  cat("  35+ pts allowed:             -4 pts\n")
  cat("  Per sack:                    +1 pt\n")
  cat("  Per INT / fumble recovery:   +2 pts\n")
  cat("  Defensive TD:                +6 pts\n")
  cat("  Safety:                      +2 pts\n")
  cat("  Blocked kick:                +2 pts\n\n")
} else {
  # Historical DEF/ST context from prior seasons loaded during prior build
  cat("Historical DEF/ST scoring context (prior seasons loaded):\n\n")

  def_summary <- def %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      games          = dplyr::n(),
      avg_pts        = round(mean(def_st_points, na.rm = TRUE), 2),
      avg_allowed    = round(mean(opponent_pts_allowed, na.rm = TRUE), 1),
      avg_sacks      = round(mean(sacks, na.rm = TRUE), 1),
      avg_ints       = round(mean(def_ints, na.rm = TRUE), 1),
      avg_def_tds    = round(mean(def_tds, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(avg_pts))

  cat("Top 10 DEF/ST units by average fantasy points:\n\n")
  print(def_summary %>% dplyr::slice_head(n = 10L), n = 10L)
  cat("\n")
  cat("Bottom 10 DEF/ST units by average fantasy points:\n\n")
  print(def_summary %>% dplyr::slice_tail(n = 10L), n = 10L)
  cat("\n")
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat(glue("Season projected:        {PROJECTION_SEASON}\n"))
cat(glue("Week:                    {PROJECTION_WEEK} (preseason baseline)\n"))
cat(glue("Players with priors:     {nrow(proj)}\n"))
cat(glue("Positions covered:       {paste(sort(unique(proj$position)), collapse = ', ')}\n"))
cat(glue("Prior weight:            {results$prior_weight_used} (full -- no YTD)\n"))
cat(glue("ADP match rate:          {round(100 * results$fp_match_rate_adp, 1)}%\n"))
cat(glue("Consensus match rate:    {round(100 * results$fp_match_rate_consensus, 1)}%\n"))
cat(glue("Output CSV:              s2_week15_player_projections.csv\n"))
cat(glue("DEF/ST CSV:              s2_week15_def_st_scores.csv\n\n"))

cat("Output files written to data/season2_cache/\n")
cat("Run again at Week 2+ with updated season and week to see YTD updates.\n\n")

cat("Next: source('tests/test_season2_week15_functions.R') to validate engine.\n\n")
