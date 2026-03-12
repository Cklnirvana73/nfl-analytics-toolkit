# ==============================================================================
# WEEK 9 USAGE EXAMPLES: XGBOOST FANTASY PREDICTION
# ==============================================================================
#
# Five self-contained examples demonstrating the Week 9 pipeline.
# Each example builds on the previous, from data prep through evaluation.
#
# Run from project root: source("examples/example_week9.R")
#
# ==============================================================================

library(dplyr)
library(tidyr)
library(glue)
library(here)

source(here::here("R", "11_xgboost_fantasy.R"))

cat("=======================================================\n")
cat("WEEK 9 USAGE EXAMPLES: XGBoost Fantasy Prediction\n")
cat("=======================================================\n\n")


# ==============================================================================
# EXAMPLE 1: prepare_model_features() -- Understanding what it returns
# NFL Context: The feature matrix is the bridge between feature engineering
#   (Weeks 5-8) and ML training (Week 9). The key additions here are the
#   absence features. A player's availability_rate captures injury history
#   as a predictive signal, not just a filter.
# ==============================================================================

cat("EXAMPLE 1: Feature preparation and absence reconstruction\n")
cat(rep("-", 55), "\n")

# Simulate feature matrix (Week 8 output) and play-by-play
set.seed(2025)

simulate_feature_matrix <- function(n_players = 6, n_weeks = 12, season = 2025) {
  players <- paste0("P", seq_len(n_players))
  expand.grid(player_id = players, week = seq_len(n_weeks),
              stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(
      season                  = season,
      player_name             = paste0("Player_", player_id),
      position_group          = "rusher",
      team                    = "BUF",
      opponent                = "MIA",
      plays_this_week         = sample(8:25, n(), replace = TRUE),
      epa_this_week           = rnorm(n(), 0.04, 0.28),
      success_rate_this_week  = runif(n(), 0.33, 0.58),
      epa_roll3               = ifelse(week <= 3, NA_real_, rnorm(n(), 0.04, 0.18)),
      epa_season_to_date      = ifelse(week == 1, NA_real_, rnorm(n(), 0.04, 0.12)),
      plays_roll3             = ifelse(week <= 3, NA_real_, runif(n(), 12, 28)),
      neutral_epa_season      = rnorm(n(), 0.03, 0.18),
      leading_share_season    = runif(n(), 0.2, 0.48),
      trailing_share_season   = runif(n(), 0.2, 0.48),
      opp_adjusted_epa_prior  = ifelse(week == 1, NA_real_, rnorm(n(), 0.02, 0.18)),
      schedule_difficulty_rank = sample(1:32, n(), replace = TRUE),
      opp_adj_games_prior     = pmax(0L, as.integer(week - 1L)),
      opponent_style          = sample(c("pass_funnel","run_funnel","balanced"), n(), replace = TRUE),
      opponent_tier           = sample(c("elite","above_avg","average","below_avg","poor"), n(), replace = TRUE),
      weeks_played            = pmin(week, 12L),
      weeks_since_role_change = sample(0:4, n(), replace = TRUE),
      role_stability_flag     = TRUE
    )
}

simulate_pbp <- function(n_plays = 600, n_players = 6, n_weeks = 12, season = 2025) {
  players <- paste0("P", seq_len(n_players))
  tibble(
    season               = season,
    week                 = sample(seq_len(n_weeks), n_plays, replace = TRUE),
    posteam              = "BUF",
    play_type            = sample(c("pass", "run"), n_plays, replace = TRUE, prob = c(0.54, 0.46)),
    qb_kneel = 0L, qb_spike = 0L, qb_scramble = 0L,
    passer_player_id     = ifelse(runif(n_plays) > 0.55, sample(players, n_plays, replace = TRUE), NA_character_),
    rusher_player_id     = ifelse(runif(n_plays) > 0.46, sample(players, n_plays, replace = TRUE), NA_character_),
    receiver_player_id   = ifelse(runif(n_plays) > 0.55, sample(players, n_plays, replace = TRUE), NA_character_),
    pass_attempt         = sample(0:1, n_plays, replace = TRUE),
    complete_pass        = sample(0:1, n_plays, replace = TRUE, prob = c(0.38, 0.62)),
    passing_yards        = ifelse(runif(n_plays) > 0.48, sample(0:38, n_plays, replace = TRUE), NA_real_),
    rushing_yards        = ifelse(runif(n_plays) > 0.48, sample(0:18, n_plays, replace = TRUE), NA_real_),
    receiving_yards      = ifelse(runif(n_plays) > 0.52, sample(0:28, n_plays, replace = TRUE), NA_real_),
    touchdown = 0L, pass_touchdown = sample(0:1, n_plays, replace = TRUE, prob = c(0.94, 0.06)),
    rush_touchdown = sample(0:1, n_plays, replace = TRUE, prob = c(0.95, 0.05)),
    return_touchdown = 0L,
    interception = sample(0:1, n_plays, replace = TRUE, prob = c(0.97, 0.03)),
    fumble_lost = 0L, two_point_conv_result = NA_character_,
    epa = rnorm(n_plays, 0, 0.5)
  )
}

fm  <- simulate_feature_matrix()
pbp <- simulate_pbp()

ml_data <- prepare_model_features(fm, pbp, seasons = 2025, positions = "rusher")

cat(sprintf("\nInput feature matrix rows:  %d\n", nrow(fm)))
cat(sprintf("Output ML matrix rows:      %d (includes reconstructed absence weeks)\n", nrow(ml_data)))
cat(sprintf("Absence weeks reconstructed: %d\n", sum(ml_data$is_absence_week)))
cat(sprintf("Rows with training targets: %d\n", sum(ml_data$has_target)))

# Key insight: absence weeks add availability features
cat("\nAvailability features for Player_P1:\n")
ml_data %>%
  filter(player_name == "Player_P1") %>%
  select(week, is_absence_week, weeks_since_last_played,
         missed_weeks_this_season, availability_rate, ppr_points_this_week) %>%
  print(n = 12)

cat("\nKey insight: availability_rate is the injury-proneness signal.\n")
cat("A player at 0.75 availability rate has missed 25% of team games --\n")
cat("a real floor risk that the model can learn from.\n\n")


# ==============================================================================
# EXAMPLE 2: Structural NAs vs Absence NAs -- verifying correct behavior
# NFL Context: Week 1 epa_roll3 = NA is expected (no prior games).
#   This is NOT imputed. XGBoost handles it natively.
#   An absent week row is also NA on epa_roll3 -- but for a different reason.
#   The model distinguishes via is_absence_week.
# ==============================================================================

cat("EXAMPLE 2: Verifying NA handling -- structural vs absence\n")
cat(rep("-", 55), "\n")

# Week 1 rows: structural NAs (no history yet)
week1_rows <- ml_data %>%
  filter(week == 1, !is_absence_week) %>%
  select(player_name, week, is_absence_week,
         epa_roll3, opp_adjusted_epa_prior, epa_season_to_date)

cat("\nWeek 1 structural NAs (not imputed -- XGBoost handles natively):\n")
print(week1_rows)

# Absence week rows: NAs on performance features, NOT on availability features
absence_rows <- ml_data %>%
  filter(is_absence_week == TRUE) %>%
  select(player_name, week, is_absence_week,
         availability_rate, missed_weeks_this_season,
         ppr_points_this_week, epa_this_week)

if (nrow(absence_rows) > 0) {
  cat(sprintf("\nAbsence week rows (%d total):\n", nrow(absence_rows)))
  cat("  Performance features (epa_this_week, ppr_points_this_week): NA by design\n")
  cat("  Availability features (availability_rate, missed_weeks): computed and filled\n")
  print(head(absence_rows, 5))
} else {
  cat("\nNo absence weeks in this mock dataset.\n")
}

cat("\nKey principle: epa_roll3 is NA in week 1 (structural) and in absence weeks.\n")
cat("The model treats them differently via is_absence_week flag.\n\n")


# ==============================================================================
# EXAMPLE 3: train_fantasy_model() -- minimal training run
# NFL Context: Position-specific models because QB features (CPOE, pressure EPA)
#   are fundamentally different from WR features (target share, air yards depth).
#   A single combined model would learn noisy cross-position patterns.
# ==============================================================================

cat("EXAMPLE 3: Training position-specific XGBoost models\n")
cat(rep("-", 55), "\n")

if (requireNamespace("xgboost", quietly = TRUE) &&
    requireNamespace("tidymodels", quietly = TRUE)) {

  cat("\nTraining rusher model (tune_grid_size = 5 for demo speed)...\n")

  models <- train_fantasy_model(
    ml_data,
    positions       = "rusher",
    min_train_weeks = 4L,
    tune_grid_size  = 5L,   # use 30 in production
    seed            = 2025L
  )

  if (!is.null(models$rusher)) {
    cat(sprintf("\nModel trained successfully.\n"))
    cat(sprintf("  Training rows:   %d\n", nrow(models$rusher$train_data)))
    cat(sprintf("  Test rows:       %d\n", nrow(models$rusher$test_data)))
    cat(sprintf("  Features:        %d\n", models$rusher$n_features))

    cat("\nBest hyperparameters (selected by CV RMSE):\n")
    models$rusher$best_params %>%
      select(-any_of(".config")) %>%
      mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
      print()

    cat("\nKey design decision: temporal expanding-window CV.\n")
    cat("Train on weeks 1-N, validate on week N+1. Never random splits.\n")
  }

} else {
  cat("xgboost / tidymodels not installed. Skipping training examples.\n")
  models <- NULL
}
cat("\n")


# ==============================================================================
# EXAMPLE 4: predict_fantasy_points() -- generating next-week predictions
# NFL Context: Prediction intervals matter as much as point estimates.
#   A player projected at 15 pts with pi_lower = 8, pi_upper = 22 is a
#   volatile pick. A player at 14 pts with pi_lower = 11, pi_upper = 17 is
#   a safer starter. Fantasy managers need both.
#   Also: availability_rate is in the output as a floor-risk reminder.
# ==============================================================================

cat("EXAMPLE 4: Generating next-week predictions with intervals\n")
cat(rep("-", 55), "\n")

if (!is.null(models) && !is.null(models$rusher)) {

  # Use the most recent week in the data as "current week"
  current_week_num <- max(ml_data$week)
  current_data <- ml_data %>%
    filter(week == current_week_num,
           position_group == "rusher",
           !is_absence_week)

  cat(sprintf("\nGenerating predictions for week %d -> week %d...\n",
              current_week_num, current_week_num + 1))

  predictions <- predict_fantasy_points(
    model_result = models$rusher,
    new_data     = current_data,
    position     = "rusher"
  )

  cat("\nTop 5 projected rushers (next week):\n")
  predictions %>%
    select(player_name, predicted_ppr, pi_lower, pi_upper,
           availability_rate, weeks_since_last_played) %>%
    head(5) %>%
    mutate(
      predicted_ppr = round(predicted_ppr, 1),
      pi_lower      = round(pi_lower, 1),
      pi_upper      = round(pi_upper, 1),
      availability_rate = round(availability_rate, 3)
    ) %>%
    print()

  cat("\nKey insight: Availability rate provides floor-risk context.\n")
  cat("Two players projected at 14 pts -- one at 1.0 availability, one at 0.72.\n")
  cat("The second player has ~28% chance of not suiting up. Different starts.\n")

} else {
  cat("Skipping (no trained model available).\n")
}
cat("\n")


# ==============================================================================
# EXAMPLE 5: evaluate_model() -- model vs baseline comparison
# NFL Context: "Last week's score" is a notoriously strong baseline for fantasy.
#   Fantasy scores have high week-to-week autocorrelation partly because
#   role/usage is stable within seasons. Our model adds value by incorporating
#   opponent quality (Week 8), game script tendency (Week 7), and availability
#   risk. If XGBoost cannot beat the persistence baseline, the feature
#   engineering from Weeks 5-8 has not added predictive value.
# ==============================================================================

cat("EXAMPLE 5: Evaluating model vs naive baselines\n")
cat(rep("-", 55), "\n")

if (!is.null(models) && !is.null(models$rusher)) {

  eval_result <- evaluate_model(
    model_result = models$rusher,
    ml_data      = ml_data,
    position     = "rusher"
  )

  if (!is.null(eval_result)) {
    cat("\nFull comparison table:\n")
    eval_result$comparison_table %>%
      mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
      print()

    xgb_rmse <- eval_result$comparison_table %>%
      filter(model == "XGBoost") %>% pull(rmse)
    lw_rmse  <- eval_result$comparison_table %>%
      filter(grepl("Last Week", model)) %>% pull(rmse)

    if (!is.na(xgb_rmse) && !is.na(lw_rmse)) {
      improvement <- round(lw_rmse - xgb_rmse, 2)
      if (improvement > 0) {
        cat(sprintf("\nXGBoost beats last-week baseline by %.2f RMSE points. Model adds value.\n",
                    improvement))
      } else {
        cat(sprintf("\nXGBoost does not beat last-week baseline (diff: %.2f pts).\n", improvement))
        cat("Review feature importance -- baseline may be capturing most signal already.\n")
      }
    }

    if (nrow(eval_result$importance_stability) > 0) {
      unstable <- eval_result$importance_stability %>%
        filter(!is.na(cv_importance), cv_importance > 0.5)
      if (nrow(unstable) > 0) {
        cat(sprintf("\nUnstable features (%d): %s\n",
                    nrow(unstable),
                    paste(head(unstable$variable, 5), collapse = ", ")))
        cat("These features should not be interpreted individually.\n")
      } else {
        cat("\nAll feature importances are stable across CV folds (CV <= 0.5).\n")
      }
    }
  }

} else {
  cat("Skipping (evaluate_model returned NULL).\n")
  cat("With mock data (n_weeks = 12), the single-season split puts the test set\n")
  cat("at weeks 15-18 -- beyond the simulated range. No test rows exist, so\n")
  cat("evaluate_model() returns NULL by design. Run with real 2025 data to\n")
  cat("see evaluation output.\n")
}

# ==============================================================================
# BEST PRACTICES SUMMARY
# ==============================================================================

cat("\n=======================================================\n")
cat("WEEK 9 BEST PRACTICES SUMMARY\n")
cat("=======================================================\n")
cat("1. NEVER impute structural NAs (epa_roll3, opp_adjusted_epa_prior).\n")
cat("   XGBoost learns optimal default direction natively.\n\n")
cat("2. Absent weeks are reconstructed and kept for absence features.\n")
cat("   They are NOT used as training targets (ppr_points_next_week = NA).\n\n")
cat("3. availability_rate is a floor-risk signal, not an efficiency signal.\n")
cat("   Players with identical per-game stats can have very different risk.\n\n")
cat("4. Temporal CV only. expanding window. Never random splits on time data.\n\n")
cat("5. Always compare to baselines. Last-week persistence is surprisingly\n")
cat("   strong. The model must beat it or the complexity is not justified.\n\n")
cat("6. Check SHAP correlation pairs before interpreting individual features.\n")
cat("   Usage metrics (targets, air_yards, routes) are often correlated.\n\n")
cat("7. Clip predicted_ppr at 0. Model can output negative predictions.\n")
cat("   A player cannot score negative PPR points.\n")
cat("=======================================================\n")
