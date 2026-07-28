################################################################################
# Week 4: Predictive Validation Functions
# Statistical functions for testing metric predictive power
#
# Functions:
#   1. split_season_by_week() - Lines 20-85
#   2. calculate_predictive_correlations() - Lines 87-210
#   3. run_regression_analysis() - Lines 212-320
#   4. calculate_prediction_error() - Lines 322-425
#   5. test_metric_stability() - NOT IMPLEMENTED (stops with an error)
################################################################################

library(dplyr)
library(glue)
library(purrr)  # Required for map_dfr() in calculate_predictive_correlations()


#' Split Season Data into Train and Test Sets
#'
#' @description
#' Splits play-by-play data by week to create proper time-series train/test splits.
#' Ensures no data leakage by strictly separating early-season (train) from 
#' late-season (test) data.
#'
#' @param pbp_data Play-by-play data from load_and_validate_pbp()
#' @param split_week Integer. Week number to split on (default: 8)
#'                   Train = weeks 1 to split_week, Test = weeks split_week+1 to end
#'
#' @return List with two elements:
#'   \describe{
#'     \item{train}{Tibble containing weeks 1 through split_week}
#'     \item{test}{Tibble containing weeks split_week+1 through end of season}
#'   }
#'
#' @details
#' NFL Context: Week 8 is roughly mid-season (9 weeks in, 9 weeks remaining).
#' Common split point in NFL analytics for within-season prediction studies.
#' 
#' Key validations:
#' - Regular season only: playoff weeks are excluded via season_type == "REG"
#'   (when the season_type column is present)
#' - No plays appear in both train and test
#' - Both periods have data
#' - Bye weeks handled automatically (players missing games retained)
#'
#' @examples
#' \dontrun{
#' pbp <- load_and_validate_pbp(2025)
#' splits <- split_season_by_week(pbp, split_week = 8)
#' 
#' # Verify no overlap
#' train_plays <- splits$train %>% distinct(game_id, play_id)
#' test_plays <- splits$test %>% distinct(game_id, play_id)
#' nrow(inner_join(train_plays, test_plays)) # Should be 0
#' }
#'
#' @seealso 
#' \code{\link{calculate_predictive_correlations}} for using train/test splits
#'
#' @export
split_season_by_week <- function(pbp_data, split_week = 8) {
  
  # Input validation
  if (!is.data.frame(pbp_data)) {
    stop("pbp_data must be a data frame")
  }
  
  if (!is.numeric(split_week) || length(split_week) != 1 || split_week < 1) {
    stop("split_week must be a single positive integer")
  }
  
  if (!"week" %in% names(pbp_data)) {
    stop("pbp_data must contain a 'week' column")
  }

  # CRITICAL FIX: Restrict to regular season so playoff weeks (19-22) do not
  # leak into the test split (guarded for column existence)
  if ("season_type" %in% names(pbp_data)) {
    pbp_data <- pbp_data %>% filter(season_type == "REG")

    if (nrow(pbp_data) == 0) {
      stop("No regular-season plays (season_type == 'REG') found in pbp_data")
    }
  }

  # Check split_week is valid for this data
  max_week <- max(pbp_data$week, na.rm = TRUE)
  if (split_week >= max_week) {
    stop(glue("split_week ({split_week}) must be less than max week in data ({max_week})"))
  }
  
  # Create train and test splits
  train_data <- pbp_data %>%
    filter(week <= split_week)
  
  test_data <- pbp_data %>%
    filter(week > split_week)
  
  # Validation checks
  if (nrow(train_data) == 0) {
    stop(glue("Train split has 0 rows. Check that weeks <= {split_week} exist in data."))
  }
  
  if (nrow(test_data) == 0) {
    stop(glue("Test split has 0 rows. Check that weeks > {split_week} exist in data."))
  }
  
  # Return as named list
  result <- list(
    train = train_data,
    test = test_data
  )
  
  message(glue("Split created: Train = weeks 1-{split_week} ({nrow(train_data)} plays), ",
               "Test = weeks {split_week + 1}-{max_week} ({nrow(test_data)} plays)"))
  
  return(result)
}


#' Calculate Predictive Correlations Between Metrics and Outcomes
#'
#' @description
#' Computes Pearson correlations between train-period metrics and test-period outcomes
#' to quantify which metrics are most predictive of future performance.
#'
#' @param train_stats Tibble. Player statistics from training period (e.g., weeks 1-8)
#' @param test_stats Tibble. Player statistics from test period (e.g., weeks 9-18)
#' @param metrics Character vector. Metric column names to test as predictors
#' @param outcome Character. Outcome column name to predict
#'
#' @return Tibble with one row per metric containing:
#'   \describe{
#'     \item{metric}{Name of predictor metric}
#'     \item{correlation}{Pearson correlation coefficient}
#'     \item{p_value}{Statistical significance (two-tailed)}
#'     \item{sample_size}{Number of players included in analysis}
#'     \item{conf_low}{Lower bound of 95% confidence interval}
#'     \item{conf_high}{Upper bound of 95% confidence interval}
#'   }
#'
#' @details
#' NFL Context: Tests whether early-season metrics (EPA, yards, TDs) predict 
#' late-season outcomes (fantasy points, future EPA). Only includes players with
#' data in BOTH periods to ensure fair comparison.
#' 
#' Statistical notes:
#' - Uses Pearson correlation (assumes linear relationship)
#' - Two-tailed significance test
#' - 95% confidence intervals via Fisher's Z transformation
#' - Missing values excluded pairwise
#' 
#' Interpretation:
#' - r close to 1: Strong positive prediction
#' - r close to 0: No predictive value
#' - r close to -1: Strong negative prediction (rare for NFL metrics)
#' - p < 0.05: Statistically significant at alpha = 0.05
#'
#' @examples
#' \dontrun{
#' # Split season
#' splits <- split_season_by_week(pbp, split_week = 8)
#' 
#' # Get player stats for each period
#' train_stats <- get_player_rushing_stats(splits$train)
#' test_stats <- get_player_rushing_stats(splits$test)
#' 
#' # Test which metrics predict future EPA
#' correlations <- calculate_predictive_correlations(
#'   train_stats = train_stats,
#'   test_stats = test_stats,
#'   metrics = c("rush_yards", "rush_epa_per_play", "yards_per_carry"),
#'   outcome = "rush_epa_per_play"
#' )
#' 
#' # View results sorted by correlation
#' correlations %>% arrange(desc(abs(correlation)))
#' }
#'
#' @seealso 
#' \code{\link{run_regression_analysis}} for multivariate prediction testing
#' \code{\link{calculate_prediction_error}} for prediction accuracy metrics
#'
#' @export
calculate_predictive_correlations <- function(train_stats, test_stats, metrics, outcome) {
  
  # Input validation
  if (!is.data.frame(train_stats)) {
    stop("train_stats must be a data frame")
  }
  
  if (!is.data.frame(test_stats)) {
    stop("test_stats must be a data frame")
  }
  
  if (!is.character(metrics) || length(metrics) == 0) {
    stop("metrics must be a non-empty character vector")
  }
  
  if (!is.character(outcome) || length(outcome) != 1) {
    stop("outcome must be a single character string")
  }
  
  # Verify player_id exists in both datasets
  if (!"player_id" %in% names(train_stats)) {
    stop("train_stats must contain 'player_id' column")
  }
  
  if (!"player_id" %in% names(test_stats)) {
    stop("test_stats must contain 'player_id' column")
  }
  
  # Verify outcome column exists in test_stats
  if (!outcome %in% names(test_stats)) {
    stop(glue("outcome column '{outcome}' not found in test_stats. ",
              "Available columns: {paste(names(test_stats), collapse=', ')}"))
  }
  
  # Verify all metric columns exist in train_stats
  missing_metrics <- setdiff(metrics, names(train_stats))
  if (length(missing_metrics) > 0) {
    stop(glue("Metrics not found in train_stats: {paste(missing_metrics, collapse=', ')}. ",
              "Available columns: {paste(names(train_stats), collapse=', ')}"))
  }
  
  # Join train and test stats (inner join = only players in both periods)
  # CRITICAL FIX: include season in the join keys when present on both sides
  # so multi-season inputs do not fan out, and enforce a one-to-one
  # relationship so any remaining duplication errors instead of silently
  # inflating rows
  join_keys <- if ("season" %in% names(train_stats) && "season" %in% names(test_stats)) {
    c("player_id", "season")
  } else {
    "player_id"
  }

  combined <- train_stats %>%
    select(all_of(join_keys), all_of(metrics)) %>%
    inner_join(
      test_stats %>% select(all_of(join_keys), outcome_value = !!sym(outcome)),
      by = join_keys,
      relationship = "one-to-one"
    )
  
  if (nrow(combined) == 0) {
    stop("No players found in both train_stats and test_stats. Check player_id overlap.")
  }
  
  message(glue("Analyzing {nrow(combined)} players with data in both periods"))
  
  # Calculate correlation for each metric
  results <- map_dfr(metrics, function(metric) {
    
    # Extract predictor and outcome, remove NAs
    predictor <- combined[[metric]]
    outcome_vals <- combined$outcome_value
    
    # Remove rows where either is NA
    valid_idx <- !is.na(predictor) & !is.na(outcome_vals)
    pred_clean <- predictor[valid_idx]
    outcome_clean <- outcome_vals[valid_idx]
    
    n <- length(pred_clean)
    
    if (n < 3) {
      # Need at least 3 observations for correlation
      return(tibble(
        metric = metric,
        correlation = NA_real_,
        p_value = NA_real_,
        sample_size = n,
        conf_low = NA_real_,
        conf_high = NA_real_
      ))
    }
    
    # Calculate Pearson correlation
    cor_test <- cor.test(pred_clean, outcome_clean, method = "pearson")
    
    tibble(
      metric = metric,
      correlation = cor_test$estimate[[1]],
      p_value = cor_test$p.value,
      sample_size = n,
      conf_low = cor_test$conf.int[1],
      conf_high = cor_test$conf.int[2]
    )
  })
  
  # Sort by absolute correlation (strongest predictors first)
  results <- results %>%
    arrange(desc(abs(correlation)))
  
  return(results)
}


#' Run Multiple Regression Analysis for Prediction
#'
#' @description
#' Tests multiple predictors simultaneously using linear regression to determine
#' which metrics add unique predictive value beyond others.
#'
#' @param train_stats Tibble. Player statistics from training period
#' @param test_stats Tibble. Player statistics from test period
#' @param predictors Character vector. Column names to use as predictors
#' @param outcome Character. Outcome column name to predict
#'
#' @return List containing:
#'   \describe{
#'     \item{model}{lm object (the fitted model)}
#'     \item{summary}{Model summary tibble with R², adj R², F-statistic, p-value}
#'     \item{coefficients}{Tibble with coefficient estimates, SEs, t-values, p-values}
#'     \item{sample_size}{Number of observations used}
#'   }
#'
#' @details
#' NFL Context: Tests whether advanced metrics (EPA) add predictive value beyond
#' traditional stats (yards, TDs). For example: Does EPA predict fantasy points
#' better than yards + TDs alone?
#' 
#' Model formula: outcome ~ predictor1 + predictor2 + ... + predictorN
#' 
#' Interpretation:
#' - R²: Proportion of variance explained (0 to 1, higher is better)
#' - Coefficients: Change in outcome per 1-unit change in predictor
#' - p-values: Statistical significance of each predictor
#' 
#' Assumptions (should check with residual plots):
#' - Linear relationship between predictors and outcome
#' - Independence of observations (one row per player)
#' - Homoscedasticity (constant variance of residuals)
#' - Normality of residuals (for inference)
#'
#' @examples
#' \dontrun{
#' # Test if EPA adds value beyond yards and TDs
#' model1 <- run_regression_analysis(
#'   train_stats, test_stats,
#'   predictors = c("rush_yards", "rush_tds"),
#'   outcome = "fantasy_ppr"
#' )
#' 
#' model2 <- run_regression_analysis(
#'   train_stats, test_stats,
#'   predictors = c("rush_yards", "rush_tds", "rush_epa_per_play"),
#'   outcome = "fantasy_ppr"
#' )
#' 
#' # Compare R²
#' cat("Model 1 R²:", model1$summary$r_squared, "\n")
#' cat("Model 2 R²:", model2$summary$r_squared, "\n")
#' cat("EPA adds:", model2$summary$r_squared - model1$summary$r_squared, "R²\n")
#' 
#' # Check assumptions
#' plot(model2$model)  # Residual diagnostic plots
#' }
#'
#' @seealso 
#' \code{\link{calculate_predictive_correlations}} for univariate prediction testing
#'
#' @export
run_regression_analysis <- function(train_stats, test_stats, predictors, outcome) {
  
  # Input validation
  if (!is.data.frame(train_stats) || !is.data.frame(test_stats)) {
    stop("train_stats and test_stats must be data frames")
  }
  
  if (!is.character(predictors) || length(predictors) == 0) {
    stop("predictors must be a non-empty character vector")
  }
  
  if (!is.character(outcome) || length(outcome) != 1) {
    stop("outcome must be a single character string")
  }
  
  # Verify player_id column
  if (!"player_id" %in% names(train_stats) || !"player_id" %in% names(test_stats)) {
    stop("Both train_stats and test_stats must contain 'player_id' column")
  }
  
  # Verify columns exist
  missing_pred <- setdiff(predictors, names(train_stats))
  if (length(missing_pred) > 0) {
    stop(glue("Predictors not found in train_stats: {paste(missing_pred, collapse=', ')}"))
  }
  
  if (!outcome %in% names(test_stats)) {
    stop(glue("Outcome '{outcome}' not found in test_stats"))
  }
  
  # Join train predictors with test outcome
  # CRITICAL FIX: join by season too when present on both sides, and enforce
  # one-to-one so multi-season fan-out errors instead of duplicating players
  join_keys <- if ("season" %in% names(train_stats) && "season" %in% names(test_stats)) {
    c("player_id", "season")
  } else {
    "player_id"
  }

  model_data <- train_stats %>%
    select(all_of(join_keys), all_of(predictors)) %>%
    inner_join(
      test_stats %>% select(all_of(join_keys), outcome_value = !!sym(outcome)),
      by = join_keys,
      relationship = "one-to-one"
    ) %>%
    # Remove any rows with NA in predictors or outcome
    filter(if_all(all_of(c(predictors, "outcome_value")), ~ !is.na(.)))
  
  if (nrow(model_data) < length(predictors) + 2) {
    stop(glue("Insufficient data for regression. Need at least {length(predictors) + 2} observations, ",
              "have {nrow(model_data)}"))
  }
  
  # Build formula
  formula_str <- paste("outcome_value ~", paste(predictors, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Fit linear model
  model <- lm(formula_obj, data = model_data)
  
  # Extract summary statistics
  model_summary_obj <- summary(model)
  
  summary_stats <- tibble(
    r_squared = model_summary_obj$r.squared,
    adj_r_squared = model_summary_obj$adj.r.squared,
    f_statistic = model_summary_obj$fstatistic[1],
    f_p_value = pf(
      model_summary_obj$fstatistic[1],
      model_summary_obj$fstatistic[2],
      model_summary_obj$fstatistic[3],
      lower.tail = FALSE
    ),
    residual_se = model_summary_obj$sigma,
    sample_size = nrow(model_data)
  )
  
  # Extract coefficients
  coef_table <- as.data.frame(model_summary_obj$coefficients)
  coef_table$term <- rownames(coef_table)
  rownames(coef_table) <- NULL
  
  coefficients <- coef_table %>%
    as_tibble() %>%
    select(
      term,
      estimate = Estimate,
      std_error = `Std. Error`,
      t_value = `t value`,
      p_value = `Pr(>|t|)`
    )
  
  # Return results
  result <- list(
    model = model,
    summary = summary_stats,
    coefficients = coefficients,
    sample_size = nrow(model_data)
  )
  
  message(glue("Regression complete: R² = {round(summary_stats$r_squared, 3)}, ",
               "n = {nrow(model_data)}"))
  
  return(result)
}


#' Calculate Prediction Error Metrics
#'
#' @description
#' Computes Mean Absolute Error (MAE) and Root Mean Squared Error (RMSE) to quantify
#' prediction accuracy in original units.
#'
#' @param train_stats Tibble. Player statistics from training period
#' @param test_stats Tibble. Player statistics from test period
#' @param metric Character. Column name in train_stats to use as predictor
#' @param outcome Character. Column name in test_stats to predict
#' @param per_game Logical. If TRUE (default), divide metric and outcome by
#'   games_played (when present in both datasets) so train/test totals over
#'   different game counts are comparable. Set FALSE for metrics that are
#'   already rates (e.g., epa_per_play, yards_per_carry).
#'
#' @return Tibble with one row containing:
#'   \describe{
#'     \item{metric}{Name of predictor used}
#'     \item{outcome}{Name of outcome predicted}
#'     \item{mae}{Mean Absolute Error (average prediction error)}
#'     \item{rmse}{Root Mean Squared Error (penalizes large errors more)}
#'     \item{sample_size}{Number of players included}
#'     \item{correlation}{Pearson r between predicted and actual}
#'     \item{per_game}{Whether values were normalized to per-game}
#'     \item{baseline_mae}{MAE of predicting every player at the sample
#'       (positional) mean of the actual outcome}
#'     \item{mae_skill}{1 - mae / baseline_mae; positive = beats the baseline}
#'   }
#'
#' @details
#' NFL Context: Quantifies prediction accuracy in interpretable units (e.g., fantasy points).
#' "This metric predicts within ±X fantasy points per game on average."
#'
#' Prediction method: Use train metric value as prediction for test outcome
#' (assumes stability: early-season value = late-season value). With
#' per_game = TRUE, both sides are normalized by games_played first so that
#' differing game counts between periods do not inflate the error.
#'
#' Baseline comparison: baseline_mae is the error from predicting the sample
#' mean for everyone. Compare mae to baseline_mae (or read mae_skill) to see
#' whether the metric carries real predictive signal.
#'
#' MAE vs RMSE:
#' - MAE: Average absolute error (more robust to outliers)
#' - RMSE: Square root of mean squared error (penalizes large errors)
#' - Both in original units (e.g., fantasy points)
#' - Lower is better for both
#' 
#' Interpretation:
#' - MAE = 5: Average error of 5 fantasy points per game
#' - RMSE = 7: Typical error of 7 points (larger due to squaring)
#' - RMSE > MAE indicates some large prediction errors
#'
#' @examples
#' \dontrun{
#' # How accurately does early-season EPA predict late-season fantasy points?
#' error_metrics <- calculate_prediction_error(
#'   train_stats, test_stats,
#'   metric = "rush_epa_per_play",
#'   outcome = "fantasy_ppr"
#' )
#' 
#' cat("Prediction accuracy:\n")
#' cat("  MAE:", error_metrics$mae, "fantasy points\n")
#' cat("  RMSE:", error_metrics$rmse, "fantasy points\n")
#' }
#'
#' @seealso 
#' \code{\link{calculate_predictive_correlations}} for correlation-based prediction assessment
#'
#' @export
calculate_prediction_error <- function(train_stats, test_stats, metric, outcome,
                                       per_game = TRUE) {
  
  # Input validation
  if (!is.data.frame(train_stats) || !is.data.frame(test_stats)) {
    stop("train_stats and test_stats must be data frames")
  }
  
  if (!is.character(metric) || length(metric) != 1) {
    stop("metric must be a single character string")
  }
  
  if (!is.character(outcome) || length(outcome) != 1) {
    stop("outcome must be a single character string")
  }
  
  # Verify columns
  if (!"player_id" %in% names(train_stats) || !"player_id" %in% names(test_stats)) {
    stop("Both datasets must contain 'player_id' column")
  }
  
  if (!metric %in% names(train_stats)) {
    stop(glue("Metric '{metric}' not found in train_stats"))
  }
  
  if (!outcome %in% names(test_stats)) {
    stop(glue("Outcome '{outcome}' not found in test_stats"))
  }
  
  # Join train metric with test outcome
  # CRITICAL FIX: join by season too when present on both sides, and enforce
  # one-to-one so multi-season fan-out errors instead of duplicating players
  join_keys <- if ("season" %in% names(train_stats) && "season" %in% names(test_stats)) {
    c("player_id", "season")
  } else {
    "player_id"
  }

  # CRITICAL FIX: train and test periods cover different numbers of games, so
  # comparing raw totals conflates volume with accuracy. Normalize both sides
  # to per-game values when games_played is available in both datasets.
  has_games <- per_game &&
    "games_played" %in% names(train_stats) &&
    "games_played" %in% names(test_stats)

  if (per_game && !has_games) {
    warning("per_game = TRUE but 'games_played' not found in both datasets - using raw values")
  }

  if (has_games) {
    combined <- train_stats %>%
      select(all_of(join_keys), predicted = !!sym(metric), train_games = games_played) %>%
      inner_join(
        test_stats %>%
          select(all_of(join_keys), actual = !!sym(outcome), test_games = games_played),
        by = join_keys,
        relationship = "one-to-one"
      ) %>%
      filter(!is.na(predicted), !is.na(actual), train_games > 0, test_games > 0) %>%
      mutate(
        predicted = predicted / train_games,
        actual = actual / test_games
      )
  } else {
    combined <- train_stats %>%
      select(all_of(join_keys), predicted = !!sym(metric)) %>%
      inner_join(
        test_stats %>% select(all_of(join_keys), actual = !!sym(outcome)),
        by = join_keys,
        relationship = "one-to-one"
      ) %>%
      filter(!is.na(predicted), !is.na(actual))
  }

  if (nrow(combined) == 0) {
    stop("No valid observations after joining and removing NAs")
  }

  # Calculate errors
  errors <- combined %>%
    mutate(
      error = actual - predicted,
      abs_error = abs(error),
      squared_error = error^2
    )

  # Calculate metrics
  mae <- mean(errors$abs_error)
  rmse <- sqrt(mean(errors$squared_error))
  correlation <- cor(errors$predicted, errors$actual, method = "pearson")
  n <- nrow(errors)

  # Baseline: MAE of predicting every player at the sample (positional) mean
  # of the actual outcome. mae_skill > 0 means the metric beats that baseline.
  baseline_mae <- mean(abs(errors$actual - mean(errors$actual)))

  result <- tibble(
    metric = metric,
    outcome = outcome,
    mae = mae,
    rmse = rmse,
    sample_size = n,
    correlation = correlation,
    per_game = has_games,
    baseline_mae = baseline_mae,
    mae_skill = 1 - mae / baseline_mae
  )

  message(glue("Prediction error{if (has_games) ' (per game)' else ''}: ",
               "MAE = {round(mae, 2)}, RMSE = {round(rmse, 2)}, ",
               "baseline MAE = {round(baseline_mae, 2)}, n = {n}"))
  
  return(result)
}


#' Test Metric Stability Across Season Halves (NOT IMPLEMENTED)
#'
#' @description
#' Deprecated / not implemented. This function was a stub: it required a
#' 'player_id' column that does not exist in raw play-by-play data and always
#' returned correlation = NA. It now stops immediately rather than returning
#' misleading output.
#'
#' For a working alternative, split the season with
#' \code{\link{split_season_by_week}}, aggregate each half with a
#' get_player_*_stats() function, and pass the results to
#' \code{\link{calculate_predictive_correlations}}.
#'
#' @param pbp_data Unused.
#' @param metric Unused.
#' @param min_games Unused.
#'
#' @return Does not return; always throws an error.
#'
#' @examples
#' \dontrun{
#' # test_metric_stability() is not implemented. Working alternative:
#' splits <- split_season_by_week(pbp, split_week = 8)
#' train_stats <- get_player_rushing_stats(splits$train)
#' test_stats <- get_player_rushing_stats(splits$test)
#'
#' calculate_predictive_correlations(
#'   train_stats = train_stats,
#'   test_stats = test_stats,
#'   metrics = "rush_epa_per_play",
#'   outcome = "rush_epa_per_play"
#' )
#' }
#'
#' @seealso
#' \code{\link{calculate_predictive_correlations}} for the working alternative
#'
#' @keywords internal
#' @export
test_metric_stability <- function(pbp_data, metric, min_games = 4) {
  stop("test_metric_stability() is not implemented — see calculate_predictive_correlations() for a working alternative.")
}