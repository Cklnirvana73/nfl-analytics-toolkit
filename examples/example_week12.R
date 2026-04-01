# ==============================================================================
# example_week12.R
# NFL Analytics Toolkit - Week 12: Projection System Usage Examples
# ==============================================================================
#
# 7 self-contained examples demonstrating the season-long projection system.
# Each example builds on the prior conceptually but can be run independently
# given the required artifacts.
#
# PREREQUISITES
# -------------
# All Week 9-11 model artifacts must exist:
#   output/models/ensemble_passer.rds
#   output/models/ensemble_rusher.rds
#   output/models/ensemble_receiver.rds
#   output/models/boom_bust_passer.rds
#   output/models/boom_bust_rusher.rds
#   output/models/boom_bust_receiver.rds
#   output/ml_data_2025.rds            (ML-ready feature matrix from ensemble pipeline)
#   output/player_stats_2024.rds   (auto-generated from nflreadr if missing)
#
# NFL SEASON STRUCTURE (2021 onward -- 17-game regular season)
# ------------------------------------------------------------
# Regular season: weeks 1-18  (fantasy relevance boundary)
# Wild Card     : week 19     (outside projection scope)
# Super Bowl    : week 22     (outside projection scope -- not a data error)
#
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(glue)

source(here::here("R", "14_projection_system.R"))

# ==============================================================================
# EXAMPLE 1: Preseason Prior -- What Does the System Know Before Week 1?
# ==============================================================================
# Purpose: Understand the starting point. The prior is anchored entirely to
# prior-season data with regression-to-mean applied. Wide uncertainty intervals
# reflect genuine uncertainty -- no current-season games have been played.
#
# Key insight: Volume metrics (rushing yards, receptions, target share) carry
# 70% of the prior weight per the Week 4 predictive validity finding. EPA
# efficiency metrics carry 30% and are diagnostic, not primary.
#
# Column verification:
#   generate_preseason_projections() returns:
#   player_id, player_name, position, prior_season, projected_ppr_per_game,
#   projected_ppr_lower_95, projected_ppr_upper_95, projected_ppr_sd,
#   prior_games_played, regression_alpha, prior_volume_rate,
#   prior_efficiency_rate

cat("==========================================================\n")
cat("EXAMPLE 1: Preseason Prior\n")
cat("==========================================================\n\n")

prior_stats_path <- here::here("output", "player_stats_2024.rds")

if (file.exists(prior_stats_path)) {
  prior_stats <- readRDS(prior_stats_path)
  cat("Loaded prior season stats from cache.\n\n")
} else {
  cat("player_stats_2024.rds not found -- building from nflreadr...\n")
  weekly_2024 <- nflreadr::load_player_stats(seasons = 2024) %>%
    dplyr::filter(
      position %in% c("QB", "RB", "WR", "TE"),
      season_type == "REG"
    )

  prior_stats <- weekly_2024 %>%
    dplyr::group_by(player_id, player_display_name, position) %>%
    dplyr::summarise(
      games_played          = dplyr::n_distinct(week),
      total_ppr             = sum(fantasy_points_ppr, na.rm = TRUE),
      avg_ppr_per_game      = total_ppr / games_played,
      avg_pass_epa_per_game = sum(passing_epa, na.rm = TRUE) / games_played,
      avg_rush_epa_per_game = sum(rushing_epa, na.rm = TRUE) / games_played,
      .groups = "drop"
    ) %>%
    dplyr::rename(player_name = player_display_name) %>%
    dplyr::select(player_id, player_name, position, games_played,
                  avg_ppr_per_game, avg_pass_epa_per_game, avg_rush_epa_per_game)

  saveRDS(prior_stats, prior_stats_path)
  cat(glue("Saved {nrow(prior_stats)} players to {prior_stats_path}\n\n"))
}

preseason_proj <- generate_preseason_projections(
  prior_season_stats = prior_stats,
  prior_season       = 2024,
  min_prior_games    = 4L
)

cat("Columns returned:\n")
print(names(preseason_proj))
cat("\n")

cat("Top 10 WR preseason projections (with uncertainty):\n")
preseason_proj %>%
  dplyr::filter(position == "WR") %>%
  dplyr::select(
    player_name, projected_ppr_per_game,
    projected_ppr_lower_95, projected_ppr_upper_95,
    prior_games_played, regression_alpha
  ) %>%
  dplyr::slice_head(n = 10) %>%
  print()

cat("\n")
cat("KEY INSIGHT: regression_alpha shows how much each player was pulled\n")
cat("toward the positional mean. Players with fewer prior games have higher\n")
cat("alpha (stronger regression) and wider confidence intervals.\n\n")

# Demonstrate regression alpha distribution
cat("Regression alpha summary (how much prior-season data regularises each player):\n")
preseason_proj %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(
    mean_alpha    = round(mean(regression_alpha), 3),
    median_alpha  = round(median(regression_alpha), 3),
    n_players     = dplyr::n(),
    .groups       = "drop"
  ) %>%
  print()

cat("\n")

# ==============================================================================
# EXAMPLE 2: Single-Week Update -- Watching the Prior Yield to Evidence
# ==============================================================================
# Purpose: Show how the projection changes after one week of observed data.
# In week 1 the prior weight = 1.0 (prior dominates, no observed data).
# In week 5 the prior weight ~ 0.78 (observed starts contributing).
# In week 14 the prior weight ~ 0.18 (observed data dominates).
#
# NFL context: Early season (weeks 1-3) projections are the least reliable.
# By week 9+ the system has enough observed data for the ensemble predictions
# to dominate the posterior.

cat("==========================================================\n")
cat("EXAMPLE 2: Single-Week Update\n")
cat("==========================================================\n\n")

ensemble_models <- load_ensemble_models()
feature_matrix  <- readRDS(FEATURE_MATRIX_PATH)

feat_wk5 <- feature_matrix %>% dplyr::filter(week == 5)

cat("Prior weight at different weeks (how much preseason data still matters):\n")
for (wk in c(1, 3, 5, 9, 14, 18)) {
  pw <- compute_prior_weight(wk)
  cat(glue("  Week {wk}: prior_weight = {round(pw, 3)}, ",
           "observed_weight = {round(1 - pw, 3)}\n\n"))
}

cat("\n")

# Demonstrate a week 5 update with a bye week player
# NOTE: Bye weeks only occur in weeks 5-14 in the NFL regular season
# ml_data includes absence-reconstructed rows: players on bye or injured
# have is_absence_week = TRUE with performance features as NA.
if ("is_absence_week" %in% names(feat_wk5)) {
  bye_wk5_players <- feat_wk5 %>%
    dplyr::filter(is_absence_week == TRUE) %>%
    dplyr::pull(player_id) %>%
    head(3)
} else {
  # Fallback for raw feature matrix without absence rows
  players_in_wk5 <- feat_wk5 %>% dplyr::pull(player_id) %>% unique()
  bye_wk5_players <- preseason_proj %>%
    dplyr::filter(!player_id %in% players_in_wk5) %>%
    dplyr::pull(player_id) %>%
    head(3)
}

cat(glue("Simulating week 5 update with {length(bye_wk5_players)} bye-week players...\n\n"))

wk5_proj <- update_weekly_projections(
  prior_projections = preseason_proj,
  current_week      = 5L,
  ensemble_models   = ensemble_models,
  feature_matrix    = feat_wk5,
  bye_week_players  = bye_wk5_players
)

cat("Columns returned:\n")
print(names(wk5_proj))
cat("\n")

cat("Sample: Top 5 QB projections after week 5 update:\n")
wk5_proj %>%
  dplyr::filter(position == "QB", !is_bye_week) %>%
  dplyr::select(
    player_name, projected_ppr_per_game,
    ensemble_prediction, prior_weight, observed_weight
  ) %>%
  dplyr::slice_head(n = 5) %>%
  print()

cat("\n")
cat("KEY INSIGHT: ensemble_prediction is the raw Week 11 model output.\n")
cat("projected_ppr_per_game is the blended posterior. At week 5 the\n")
cat("prior still contributes ~78% -- big ensemble swings are dampened.\n\n")

# ==============================================================================
# EXAMPLE 3: Rest-of-Season Projections -- ROS Rankings at Midseason
# ==============================================================================
# Purpose: ROS rankings drive trade decisions more than single-week projections.
# A player coming off a bye with 9 games remaining is more valuable than one
# with a bye upcoming and 8 effective games remaining.
#
# NFL context: Schedule difficulty matters late in the season when divisional
# matchups concentrate. A player facing four consecutive tough defenses has
# a materially worse ROS outlook than raw rate suggests.
#
# Remaining games = 18 - current_week (hard capped at week 18, regular season end)

cat("==========================================================\n")
cat("EXAMPLE 3: Rest-of-Season Projections at Midseason\n")
cat("==========================================================\n\n")

# Simulated bye week schedule for demonstration
# In practice this comes from nflreadr::load_schedules() or team-level data
example_bye_schedule <- c(
  "00-0030506" = 11L,  # Illustrative player ID
  "00-0033873" = 9L,
  "00-0035676" = 7L
)

# Run update through week 9 (midseason)
proj_wk9 <- preseason_proj
for (wk in 1:9) {
  feat_wk <- feature_matrix %>% dplyr::filter(week == wk)
  if (nrow(feat_wk) == 0) next
  bye_this_wk <- names(example_bye_schedule[example_bye_schedule == wk])
  proj_wk9 <- update_weekly_projections(
    prior_projections = proj_wk9,
    current_week      = wk,
    ensemble_models   = ensemble_models,
    feature_matrix    = feat_wk,
    bye_week_players  = bye_this_wk
  )
}

ros_wk9 <- generate_ros_projections(
  current_projections = proj_wk9,
  current_week        = 9L,
  bye_week_schedule   = example_bye_schedule,
  opponent_difficulty = derive_opponent_difficulty(feature_matrix, target_week = 9L)
)

cat("Columns returned:\n")
print(names(ros_wk9))
cat("\n")

cat("Top 10 RB ROS projections at week 9 (9 games remaining):\n")
ros_wk9 %>%
  dplyr::filter(position == "RB") %>%
  dplyr::select(
    player_name, remaining_games, projected_ros_ppr,
    projected_ros_lower_80, projected_ros_upper_80,
    had_remaining_bye, schedule_adjustment
  ) %>%
  dplyr::slice_head(n = 10) %>%
  print()

cat("\n")
cat("KEY INSIGHT: had_remaining_bye = TRUE means the player loses one game\n")
cat("from remaining_games (bye is already baked in). schedule_adjustment\n")
cat("reflects Week 8 opponent quality -- capped at 0.80-1.20 multiplier.\n\n")

cat("ROS confidence interval width by position (how certain are the projections?):\n")
ros_wk9 %>%
  dplyr::mutate(ci_width_80 = projected_ros_upper_80 - projected_ros_lower_80) %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(
    median_ros_pts     = round(median(projected_ros_ppr), 1),
    median_ci_width_80 = round(median(ci_width_80),       1),
    .groups            = "drop"
  ) %>%
  print()

cat("\n")

# ==============================================================================
# EXAMPLE 4: Full Projection Report -- Decision-Ready Output
# ==============================================================================
# Purpose: The report integrates three tracks:
#   - Weekly projection (regression: how many points?)
#   - Boom/bust probability (classification: outcome tier?)
#   - Matchup flag (opponent quality: how hard is this week?)
#
# NFL context: A player projected for 14 points with 40% boom probability
# and a favorable matchup is more startable than one projected 15 points
# with 45% bust probability against a top-5 defense.

cat("==========================================================\n")
cat("EXAMPLE 4: Full Projection Report\n")
cat("==========================================================\n\n")

boom_bust_models <- load_boom_bust_models()
feat_wk9 <- feature_matrix %>% dplyr::filter(week == 9)
opp_diff_wk9 <- derive_opponent_difficulty(feature_matrix, target_week = 9L)

report_wk9 <- create_projection_report(
  weekly_projections  = proj_wk9,
  ros_projections     = ros_wk9,
  boom_bust_models    = boom_bust_models,
  feature_matrix      = feat_wk9,
  roster_season       = 2025L,
  export_csv          = FALSE,
  opponent_difficulty = opp_diff_wk9
)

cat("Columns returned:\n")
print(names(report_wk9))
cat("\n")

cat("WR report -- week 9 (top 15, non-bye, sorted by projection):\n")
report_wk9 %>%
  dplyr::filter(position == "WR", !is_bye_week) %>%
  dplyr::select(
    player_name, player_position, projected_ppr_per_game,
    boom_probability, bust_probability,
    boom_bust_recommendation, matchup_flag
  ) %>%
  dplyr::slice_head(n = 15) %>%
  print()

cat("\n")

cat("TE-only filter -- uses player_position (not available via position alone):\n")
cat("NOTE: TEs are modelled as 'receiver' (same model as WRs). Projections\n")
cat("are less reliable for TEs -- the model learned a WR+TE mixed signal.\n")
report_wk9 %>%
  dplyr::filter(player_position == "TE", !is_bye_week) %>%
  dplyr::select(
    player_name, player_position, projected_ppr_per_game,
    boom_probability, boom_bust_recommendation
  ) %>%
  dplyr::slice_head(n = 10) %>%
  print()

cat("\n")

cat("Boom/bust recommendation breakdown by position:\n")
report_wk9 %>%
  dplyr::filter(!is_bye_week) %>%
  dplyr::count(position, boom_bust_recommendation) %>%
  tidyr::pivot_wider(
    names_from  = boom_bust_recommendation,
    values_from = n,
    values_fill = 0L
  ) %>%
  print()

cat("\n")
cat("KEY INSIGHT: boom_bust_recommendation = 'avoid' flags players where\n")
cat("bust_probability > 0.30 AND matchup_flag = 'tough'. These are\n")
cat("high-risk starts even if the weekly projection looks acceptable.\n\n")

# ==============================================================================
# EXAMPLE 5: Backtest -- Does the System Beat Naive Baselines?
# ==============================================================================
# Purpose: Validate that Bayesian updating adds value over simple heuristics.
# Two baselines compete: last week's actual score and season-to-date average.
# Both are strong in NFL fantasy -- the ensemble must earn its complexity.
#
# The Week 4 predictive validity study finding informs what to expect:
# volume metrics are strong predictors. A system that only uses volume
# (season-to-date rushing yards, receptions) will be competitive.
# The ensemble should outperform by incorporating:
#   - Game script context (Week 7)
#   - Opponent adjustment (Week 8)
#   - Role stability flags (Audit checklist Issue 6)
#
# If it does not beat both baselines in late season (weeks 9-18),
# backtest_projections() documents this explicitly in console output.

cat("==========================================================\n")
cat("EXAMPLE 5: Backtest Against Naive Baselines\n")
cat("==========================================================\n\n")

backtest_results <- backtest_projections(
  feature_matrix    = feature_matrix,
  preseason_stats   = prior_stats,
  ensemble_models   = ensemble_models,
  boom_bust_models  = boom_bust_models,
  backtest_season   = 2024,
  bye_week_schedule = example_bye_schedule
)

cat("Weekly RMSE comparison -- first 6 weeks (prior dominates):\n")
backtest_results$weekly_errors %>%
  dplyr::filter(week <= 6) %>%
  dplyr::select(position, week, rmse_ensemble, rmse_last_week, rmse_season_avg) %>%
  print()

cat("\n")

cat("Weekly RMSE comparison -- last 6 weeks (observed data dominates):\n")
backtest_results$weekly_errors %>%
  dplyr::filter(week >= 13) %>%
  dplyr::select(position, week, rmse_ensemble, rmse_last_week, rmse_season_avg) %>%
  print()

cat("\n")

cat("Overall summary:\n")
summary <- backtest_results$summary_stats
cat(glue(
  "  Overall RMSE (ensemble):       {round(summary$overall_rmse, 3)}\n",
  "  Overall RMSE (last week):      {round(summary$baseline_rmse_lw, 3)}\n",
  "  Overall RMSE (season avg):     {round(summary$baseline_rmse_sa, 3)}\n",
  "  Late-season beats last week:   {summary$beats_last_week}\n",
  "  Late-season beats season avg:  {summary$beats_season_avg}\n\n"
))

cat("Players with lowest season RMSE (most predictable):\n")
backtest_results$player_errors %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::select(player_name, position, season_rmse, n_weeks) %>%
  print()

cat("\n")
cat("KEY INSIGHT: If beats_both_baselines = FALSE, the finding is documented\n")
cat("rather than hidden. Likely causes: incomplete feature leakage audit\n")
cat("(Week 10 deferred) or role_stability_flag not applied to training data.\n\n")

# ==============================================================================
# EXAMPLE 6: Playoff Week Guard -- Scope Enforcement in Practice
# ==============================================================================
# Purpose: Demonstrate that the system's scope enforcement works correctly
# and explain the NFL week numbering context.
#
# NFL calendar (17-game regular season, 2021 onward):
#   Weeks  1-18: Regular season (fantasy relevance)
#   Week  19   : Wild Card (playoff)
#   Week  20   : Divisional
#   Week  21   : Conference Championship
#   Week  22   : Super Bowl
#
# In nflfastR data, week 22 = Super Bowl. This is correctly coded, not
# an anomaly. The Week 9 artifact produced predicted_week = 22 because
# the model saw postseason rows. The fix is filtering week <= 18 at
# data load, which backtest_projections() does automatically.

cat("==========================================================\n")
cat("EXAMPLE 6: Playoff Week Scope Enforcement\n")
cat("==========================================================\n\n")

cat("Demonstrating hard stop for playoff week requests:\n\n")

# This should produce a clear, informative error
tryCatch(
  {
    generate_ros_projections(
      current_projections = proj_wk9,
      current_week        = 19L   # Wild Card week -- outside scope
    )
  },
  error = function(e) {
    cat("Expected error caught:\n")
    cat(conditionMessage(e))
    cat("\n\n")
  }
)

cat("Demonstrating that feature matrix is correctly filtered:\n")
n_before <- nrow(feature_matrix)

# Show how backtest_projections() handles postseason rows automatically
fm_with_postseason <- feature_matrix %>%
  dplyr::bind_rows(
    feature_matrix %>%
      dplyr::slice_head(n = 50) %>%
      dplyr::mutate(week = 22L)  # Simulate Super Bowl rows
  )

cat(glue("Feature matrix rows before filter: {nrow(fm_with_postseason)}\n"))

fm_filtered <- fm_with_postseason %>%
  dplyr::filter(week <= NFL_REGULAR_SEASON_MAX_WEEK)

cat(glue("Feature matrix rows after filter:  {nrow(fm_filtered)}\n"))
cat(glue(
  "Rows removed: {nrow(fm_with_postseason) - nrow(fm_filtered)} ",
  "(week 22 = Super Bowl, outside scope)\n\n"
))

cat("KEY INSIGHT: week 22 in nflfastR is the Super Bowl -- correctly coded.\n")
cat("The Week 9 'predicted_week = 22' join failure was not a data error;\n")
cat("it was a missing scope filter. One filter at data load resolves it.\n\n")

# ==============================================================================
# EXAMPLE 7: Run Full Pipeline -- Single Call From Prior Stats to Report
# ==============================================================================
# Purpose: Show the end-to-end workflow a user would actually run on game day.
# One function call regenerates the full projection system from saved artifacts.
#
# Practical workflow:
#   1. Monday: Collect game results, update feature_matrix
#   2. Tuesday: run_full_pipeline(current_week = N) -- takes 1-5 minutes
#   3. Review report, make start/sit decisions
#   4. Thursday/Sunday: lineup locked
#
# The pipeline covers all weeks 1 through current_week. By week 14 the
# system has 14 rounds of Bayesian updating and the ensemble predictions
# carry ~82% of the posterior weight.

cat("==========================================================\n")
cat("EXAMPLE 7: run_full_pipeline() -- Full System, Single Call\n")
cat("==========================================================\n\n")

pipeline_output <- run_full_pipeline(
  current_week      = 14L,
  preseason_stats   = prior_stats,
  feature_matrix    = feature_matrix,
  bye_week_schedule = example_bye_schedule,
  export_report     = FALSE,   # Set TRUE in production to write CSV
  save_intermediates = FALSE,
  opponent_difficulty = derive_opponent_difficulty(feature_matrix, target_week = 14L)
)

cat("Pipeline output elements:\n")
print(names(pipeline_output))
cat("\n")

cat("Prior weight at week 14 (observed data dominance by this point):\n")
cat(glue(
  "  prior_weight    = {round(compute_prior_weight(14), 3)}\n",
  "  observed_weight = {round(1 - compute_prior_weight(14), 3)}\n\n"
))

cat("Top 10 WR weekly projections -- week 14:\n")
pipeline_output$report %>%
  dplyr::filter(position == "WR", !is_bye_week) %>%
  dplyr::select(
    player_name,
    player_position,
    projected_ppr_per_game,
    projected_ppr_lower_80,
    projected_ppr_upper_80,
    boom_probability,
    boom_bust_recommendation,
    matchup_flag,
    projected_ros_ppr,
    remaining_games
  ) %>%
  dplyr::slice_head(n = 10) %>%
  print()

cat("\n")

cat("Top 5 TE projections -- week 14 (player_position = TE within receiver model):\n")
pipeline_output$report %>%
  dplyr::filter(player_position == "TE", !is_bye_week) %>%
  dplyr::select(
    player_name, player_position,
    projected_ppr_per_game, boom_probability,
    boom_bust_recommendation
  ) %>%
  dplyr::slice_head(n = 5) %>%
  print()

cat("\n")

cat("Top 10 RB ROS projections -- weeks 15-18 remaining:\n")
pipeline_output$ros_projections %>%
  dplyr::filter(position == "RB") %>%
  dplyr::select(
    player_name, remaining_games, projected_ros_ppr,
    projected_ros_lower_80, projected_ros_upper_80,
    had_remaining_bye
  ) %>%
  dplyr::slice_head(n = 10) %>%
  print()

cat("\n")

cat("BEST PRACTICES SUMMARY\n")
cat("======================\n")
cat("1. Always run feature_matrix through week <= 18 filter at data load.\n")
cat("   Do not patch individual functions -- filter once at the top.\n\n")
cat("2. Bye week schedule must be provided for weeks 5-14.\n")
cat("   NFL bye windows: weeks 5-14 only. No byes in weeks 1-4 or 15-18.\n\n")
cat("3. The prior carries meaningful weight through week 6 (~40%+).\n")
cat("   Early-season projections have wide CIs -- this is correct, not a bug.\n\n")
cat("4. Volume metrics outpredict EPA efficiency at the player-aggregate level\n")
cat("   (Week 4 finding). Do not over-weight efficiency signals in the prior.\n\n")
cat("5. backtest_projections() prints findings honestly. If the ensemble\n")
cat("   does not beat both baselines, investigate role_stability_flag\n")
cat("   application and feature leakage audit (Week 10 deferred issues).\n\n")
cat("6. report column 'boom_bust_recommendation = avoid' = high bust risk\n")
cat("   combined with tough matchup. A projected 12-point RB with avoid flag\n")
cat("   is a worse start than an 11-point RB flagged average + favorable.\n\n")
cat("7. player_position (QB/RB/WR/TE) is the real NFL position from the roster.\n")
cat("   position (QB/RB/WR) is the model routing label. Use player_position to\n")
cat("   filter TEs -- they are modelled as 'receiver' alongside WRs.\n")
cat("   TE projections are less reliable (mixed WR+TE training signal).\n\n")
