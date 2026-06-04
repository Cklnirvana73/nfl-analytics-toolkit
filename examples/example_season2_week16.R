# ==============================================================================
# SEASON 2 WEEK 16: DEF/ST FORWARD PROJECTION -- USAGE EXAMPLES
# File: examples/example_season2_week16.R
# ==============================================================================
#
# PURPOSE
# -------
# Six self-contained examples demonstrating project_def_st() and the supporting
# functions in R/34_def_st_projection.R. Each example is runnable on its own
# after the SETUP block. All examples set save_output = FALSE so running them
# never overwrites the real s2_week16_def_st_projections output.
#
# Examples 1-2 use the real 2023-2025 cache (the preseason prior). Example 3
# demonstrates the in-season blend by treating a COMPLETED season (2025) as if
# it were the live season, so the mechanic is visible and runnable today even
# though the 2026 season has not started. Example 5 demonstrates league scoring
# without any network call by passing a sample scoring config directly.
#
# EXAMPLES
#   1. Preseason projection, standard Sleeper scoring
#   2. How regression-to-mean shapes the prior (raw vs regressed)
#   3. In-season blend, using 2025 as a stand-in live season
#   4. The prior-weight decay schedule (the in-season switch)
#   5. Custom league scoring changes the projection
#   6. The scoring layer: per-game DEF/ST points for one team
#
# PREREQUISITES
# -------------
# Normalized pbp in data/season2_cache/ for the seasons referenced:
#   pbp_normalized_2023.rds, pbp_normalized_2024.rds, pbp_normalized_2025.rds
# R/34 sources R/15, R/19, and R/29 automatically if not already loaded.
#
# NFL SEASON STRUCTURE
# --------------------
# Regular season weeks 1-18. compute_prior_weight() (from R/29) is defined only
# for weeks 1-18; preseason is represented by as_of_week = NULL (pure prior).
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP (run once before any example)
# ------------------------------------------------------------------------------

library(dplyr)
library(here)
library(glue)

source(here::here("R", "34_def_st_projection.R"))

# ==============================================================================
# EXAMPLE 1: Preseason projection under standard Sleeper scoring
# ==============================================================================
# Purpose: the default call. No week supplied means preseason, so every team
# sits at its pure regressed historical prior (prior_weight = 1.0, no observed
# data folded in). This is the defense input the optimizer uses before Week 1.
#
# Key insight: def_proj_ppg equals def_prior_ppg here because there is no
# season-to-date data yet. def_observed_ppg is NA and n_observed_games is 0.

ex1 <- project_def_st(save_output = FALSE)

cat("\nTop 5 projected defenses (preseason):\n")
print(head(ex1[, c("team", "def_proj_ppg", "def_observed_ppg",
                    "n_observed_games", "prior_weight")], 5))

cat("\nBottom 5 projected defenses (preseason):\n")
print(tail(ex1[, c("team", "def_proj_ppg")], 5))


# ==============================================================================
# EXAMPLE 2: How regression-to-mean shapes the prior
# ==============================================================================
# Purpose: inspect the prior directly to see the shrinkage. .compute_def_prior()
# returns both prior_raw_ppg (the unshrunk multi-season blend) and def_prior_ppg
# (after regressing REGRESSION_STRENGTH of the way to the league mean).
#
# Key insight: extreme defenses are pulled toward the league mean. Team defense
# is turnover-driven and turnover luck regresses hard year to year, so the raw
# multi-season average overstates how repeatable a great or terrible season is.

scoring_std <- DEF_SCORING_DEFAULT
prior_tbl <- .compute_def_prior(PRIOR_SEASONS, scoring_std, CACHE_DIR_DEFAULT)

league_mean <- mean(prior_tbl$prior_raw_ppg, na.rm = TRUE)
shrinkage_view <- prior_tbl %>%
  dplyr::mutate(pull_toward_mean = def_prior_ppg - prior_raw_ppg) %>%
  dplyr::arrange(dplyr::desc(prior_raw_ppg)) %>%
  dplyr::select(team, prior_raw_ppg, def_prior_ppg, pull_toward_mean)

cat(glue("\nLeague mean raw DEF PPG: {format(round(league_mean, 2), nsmall = 2)}\n"))
cat("\nMost extreme priors get pulled hardest toward the mean:\n")
print(head(shrinkage_view, 3))
print(tail(shrinkage_view, 3))


# ==============================================================================
# EXAMPLE 3: In-season blend, using 2025 as a stand-in live season
# ==============================================================================
# Purpose: demonstrate the in-season update mechanic against real data. We treat
# 2025 as if it were the live season and ask for the projection through week 8,
# using 2023-2024 as the prior. This is exactly what the real 2026 call will do
# once games are played: project_def_st(as_of_week = current_week).
#
# Key insight: with observed data present, def_proj_ppg moves off the pure prior
# toward season-to-date. prior_weight at week 8 is roughly 0.6, so the prior
# still anchors the estimate but observed has started to pull it.

ex3 <- project_def_st(
  season        = 2025L,
  as_of_week    = 8L,
  prior_seasons = 2023:2024,
  save_output   = FALSE
)

cat("\nWeek-8 in-season blend (2025 stand-in), teams that moved most:\n")
moved <- ex3 %>%
  dplyr::mutate(move = def_proj_ppg - def_prior_ppg) %>%
  dplyr::arrange(dplyr::desc(abs(move))) %>%
  dplyr::select(team, def_prior_ppg, def_observed_ppg, def_proj_ppg,
                n_observed_games, prior_weight, move)
print(head(moved, 6))


# ==============================================================================
# EXAMPLE 4: The prior-weight decay schedule (the in-season switch)
# ==============================================================================
# Purpose: show the curve that governs how fast observed data takes over. This
# is R/29's compute_prior_weight(), reused unchanged so DEF and offense decay on
# the same schedule.
#
# Key insight: the prior dominates early, crosses over to observed around the
# middle of the season, and a small prior floor is preserved at the end so a
# single noisy stretch never fully owns the projection.

decay <- tibble::tibble(week = c(1L, 3L, 6L, 9L, 12L, 15L, 18L)) %>%
  dplyr::mutate(
    prior_weight    = vapply(week, compute_prior_weight, numeric(1)),
    observed_weight = 1 - prior_weight
  )

cat("\nPrior / observed weight by week:\n")
print(decay)


# ==============================================================================
# EXAMPLE 5: Custom league scoring changes the projection
# ==============================================================================
# Purpose: show that league scoring actually flows through. We build a sample
# scoring_settings list (a sack-heavy, big-play league) and parse it, then
# project under both standard and custom scoring and compare. No network call
# is needed because we pass the parsed config directly via the scoring argument.
#
# Key insight: a league that rewards sacks and takeaways more heavily reranks
# the defenses, which is exactly why pulling real league scoring matters rather
# than assuming standard. In live use, pass league_id instead and R/34 pulls the
# real settings from connect_sleeper_league().

sample_league_settings <- list(
  def_sack    = 2,    # standard is 1
  def_int     = 3,    # standard is 2
  fum_rec     = 3,    # standard is 2
  pts_allow_0 = 12    # standard is 10
)
custom_scoring <- .parse_def_scoring(sample_league_settings)

ex5_std <- project_def_st(scoring = DEF_SCORING_DEFAULT, save_output = FALSE) %>%
  dplyr::select(team, std_ppg = def_proj_ppg)
ex5_cust <- project_def_st(scoring = custom_scoring, save_output = FALSE) %>%
  dplyr::select(team, custom_ppg = def_proj_ppg)

compare <- ex5_std %>%
  dplyr::inner_join(ex5_cust, by = "team") %>%
  dplyr::mutate(diff = custom_ppg - std_ppg) %>%
  dplyr::arrange(dplyr::desc(diff))

cat("\nStandard vs sack-heavy league, defenses that gain the most:\n")
print(head(compare, 5))


# ==============================================================================
# EXAMPLE 6: The scoring layer -- per-game DEF/ST points for one team
# ==============================================================================
# Purpose: drop down a level to the per-game scoring that the projection is
# built on. calculate_def_st_points() (from R/29) tallies the raw events per
# game; .score_def_games() applies the league config. This is what gets averaged
# into a team's PPG.
#
# Key insight: the projection is just a regressed, decayed average of these
# per-game scores. Seeing the game log makes the PPG number concrete.

pbp_2025 <- load_normalized_season(2025L, cache_dir = CACHE_DIR_DEFAULT)
games_2025 <- calculate_def_st_points(pbp_2025)
scored_2025 <- .score_def_games(games_2025, DEF_SCORING_DEFAULT)

example_team <- scored_2025$team[1]
team_log <- scored_2025 %>%
  dplyr::filter(team == example_team) %>%
  dplyr::arrange(week) %>%
  dplyr::select(season, week, team, opponent_pts_allowed, sacks, def_ints,
                fum_recs, def_tds, def_st_points)

cat(glue("\n2025 per-game DEF/ST scoring for {example_team}:\n"))
print(team_log)
cat(glue("\nSeason DEF/ST PPG for {example_team}: ",
         "{format(round(mean(team_log$def_st_points), 2), nsmall = 2)}\n"))
