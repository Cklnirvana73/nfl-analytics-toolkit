# ==============================================================================
# WEEK 9 VISUALIZATIONS: XGBOOST FANTASY PREDICTION
# Uses ACTUAL 2025 data from saved RDS files
# ==============================================================================
#
# Generates publication-quality plots from the real 2025 XGBoost model runs.
# All data sourced from output/week9_models_2025.rds and
# output/week9_predictions_2025.rds -- no mock data anywhere in this script.
#
# Plots produced:
#   1. Prediction vs Actual scatter (per position, real test set) -- interactive HTML
#   2. Model vs Baseline RMSE comparison bar chart -- static PNG
#   3. Availability rate vs PPR volatility (real 2025 players) -- interactive HTML
#   4. Next-week projections leaderboard (real predictions) -- static PNG
#
# Usage:
#   Run week9_working_on.R first to produce output/week9_models_2025.rds
#   and output/week9_predictions_2025.rds.
#   Then: source("visuals/create_week9_visual.R")
#
# ==============================================================================

library(tidymodels)
library(xgboost)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(scales)
library(glue)
library(here)

# Output directory
dir.create(here::here("output", "plots"), recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# LOAD REAL 2025 DATA
# ==============================================================================

cat("Loading 2025 model and prediction data...\n")

models_path <- here::here("output", "week9_models_2025.rds")
preds_path  <- here::here("output", "week9_predictions_2025.rds")

if (!file.exists(models_path)) {
  stop(paste0(
    "week9_models_2025.rds not found at: ", models_path, "\n",
    "Run week9_working_on.R (Steps 1-10) to generate it first."
  ))
}
if (!file.exists(preds_path)) {
  stop(paste0(
    "week9_predictions_2025.rds not found at: ", preds_path, "\n",
    "Run week9_working_on.R (Step 10) to generate it first."
  ))
}

models    <- readRDS(models_path)
all_preds <- readRDS(preds_path)

fitted_positions <- names(Filter(Negate(is.null), models))
cat(glue("  Models loaded: {paste(fitted_positions, collapse = ', ')}\n"))
cat(glue("  Predictions loaded: {nrow(all_preds)} player projections across ",
         "{n_distinct(all_preds$position_group)} positions\n\n"))

if (length(fitted_positions) == 0) {
  stop("All models are NULL in week9_models_2025.rds. Cannot generate visuals.")
}


# ==============================================================================
# HELPER: Generate test-set predictions from a saved model
#
# Calls predict() on model_result$test_data once per position.
# Returns a tibble with actual_ppr, predicted_ppr, and key identifiers.
# Returns NULL if model or test data is missing.
# ==============================================================================

build_test_eval <- function(model_result, pos) {

  if (is.null(model_result)) {
    message(glue("  {pos}: model is NULL, skipping."))
    return(NULL)
  }

  test_data <- model_result$test_data

  if (is.null(test_data) || nrow(test_data) == 0) {
    message(glue("  {pos}: test_data is empty. ",
                 "Model may have been trained on a single week -- ",
                 "no held-out weeks remain."))
    return(NULL)
  }

  # Required columns guard
  required <- c("ppr_points_next_week", "ppr_points_this_week",
                "player_id", "player_name", "week", "availability_rate")
  missing_cols <- setdiff(required, names(test_data))
  if (length(missing_cols) > 0) {
    message(glue("  {pos}: test_data missing columns: ",
                 "{paste(missing_cols, collapse = ', ')}. Skipping."))
    return(NULL)
  }

  preds <- predict(model_result$fitted_workflow, new_data = test_data) %>%
    rename(predicted_ppr = .pred) %>%
    mutate(predicted_ppr = pmax(predicted_ppr, 0))

  test_data %>%
    select(
      player_id, player_name, week,
      actual_ppr    = ppr_points_next_week,
      last_week_ppr = ppr_points_this_week,
      availability_rate
    ) %>%
    bind_cols(preds) %>%
    filter(!is.na(actual_ppr)) %>%
    mutate(position_group = pos)
}


# ==============================================================================
# BUILD TEST-SET EVALUATIONS (predict called once per position)
# ==============================================================================

cat("Generating test-set predictions from real 2025 models...\n")

positions   <- c("passer", "rusher", "receiver")
eval_list   <- lapply(positions, function(pos) build_test_eval(models[[pos]], pos))
names(eval_list) <- positions

eval_combined <- bind_rows(Filter(Negate(is.null), eval_list))

if (nrow(eval_combined) == 0) {
  stop(paste0(
    "No test evaluation data could be generated from week9_models_2025.rds.\n",
    "Possible cause: the 2025 season only covered weeks 1-14 in the training run,\n",
    "leaving no held-out test weeks. Check week9_working_on.R Step 8 output."
  ))
}

cat(glue("  Test eval rows: {nrow(eval_combined)} player-weeks | ",
         "Positions: {paste(sort(unique(eval_combined$position_group)), collapse = ', ')}\n\n"))


# ==============================================================================
# BUILD RMSE COMPARISON TABLE (reuses eval_combined -- no second predict() call)
# ==============================================================================

compute_comparison <- function(eval_df, model_result, pos) {

  if (is.null(eval_df) || nrow(eval_df) == 0) return(NULL)

  # Season-avg baseline: per-player average from the training period
  season_avg <- model_result$train_data %>%
    group_by(player_id) %>%
    summarise(season_avg_ppr = mean(ppr_points_this_week, na.rm = TRUE),
              .groups = "drop")

  eval_sa <- eval_df %>%
    left_join(season_avg, by = "player_id") %>%
    filter(!is.na(season_avg_ppr))

  lw_valid <- eval_df %>% filter(!is.na(last_week_ppr))

  # RMSE -- each subset checked for non-zero rows before aggregating
  xgb_rmse <- sqrt(mean((eval_df$actual_ppr - eval_df$predicted_ppr)^2, na.rm = TRUE))
  xgb_mae  <- mean(abs(eval_df$actual_ppr - eval_df$predicted_ppr), na.rm = TRUE)

  lw_rmse <- if (nrow(lw_valid) > 0)
    sqrt(mean((lw_valid$actual_ppr - lw_valid$last_week_ppr)^2, na.rm = TRUE))
    else NA_real_
  lw_mae  <- if (nrow(lw_valid) > 0)
    mean(abs(lw_valid$actual_ppr - lw_valid$last_week_ppr), na.rm = TRUE)
    else NA_real_

  sa_rmse <- if (nrow(eval_sa) > 0)
    sqrt(mean((eval_sa$actual_ppr - eval_sa$season_avg_ppr)^2, na.rm = TRUE))
    else NA_real_
  sa_mae  <- if (nrow(eval_sa) > 0)
    mean(abs(eval_sa$actual_ppr - eval_sa$season_avg_ppr), na.rm = TRUE)
    else NA_real_

  tibble(
    position = pos,
    model    = c("XGBoost", "Last Week", "Season Avg"),
    rmse     = c(xgb_rmse, lw_rmse, sa_rmse),
    mae      = c(xgb_mae,  lw_mae,  sa_mae)
  )
}

comparison_list <- lapply(positions, function(pos) {
  if (is.null(eval_list[[pos]])) return(NULL)
  compute_comparison(eval_list[[pos]], models[[pos]], pos)
})
real_comparison <- bind_rows(Filter(Negate(is.null), comparison_list))

cat("RMSE comparison table (real 2025 data):\n")
print(real_comparison %>% mutate(across(where(is.numeric), ~ round(.x, 2))))
cat("\n")


# ==============================================================================
# PLOT 1: Predicted vs Actual PPR (real 2025 test set)
# Interactive HTML -- real data typically has 60-150+ labeled player-weeks
# ==============================================================================

cat("Building Plot 1: Predicted vs Actual scatter (real 2025 data)...\n")

RMSE_BAND <- 5.0

# Per-position RMSE for facet labels
position_rmse <- eval_combined %>%
  group_by(position_group) %>%
  summarise(
    rmse  = sqrt(mean((actual_ppr - predicted_ppr)^2, na.rm = TRUE)),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  mutate(facet_label = glue("{position_group}\nRMSE {round(rmse, 1)} pts  (n={n_obs})"))

eval_plot1 <- eval_combined %>%
  left_join(position_rmse %>% select(position_group, facet_label), by = "position_group")

p1 <- ggplot(eval_plot1,
             aes(x = predicted_ppr, y = actual_ppr,
                 color = position_group,
                 text = paste0(
                   "Player: ", player_name, "\n",
                   "Position: ", position_group, "\n",
                   "Week: ", week, "\n",
                   "Predicted: ", round(predicted_ppr, 1), " pts\n",
                   "Actual: ", round(actual_ppr, 1), " pts\n",
                   "Error: ", round(actual_ppr - predicted_ppr, 1), " pts"
                 ))) +
  geom_abline(slope = 1, intercept = 0,
              color = "gray40", linetype = "dashed", linewidth = 0.8) +
  geom_abline(slope = 1, intercept =  RMSE_BAND,
              color = "gray70", linetype = "dotted", linewidth = 0.5) +
  geom_abline(slope = 1, intercept = -RMSE_BAND,
              color = "gray70", linetype = "dotted", linewidth = 0.5) +
  geom_point(size = 2.5, alpha = 0.70) +
  facet_wrap(~ facet_label, nrow = 1, scales = "free") +
  scale_color_manual(
    values = c("passer" = "#003f5c", "rusher" = "#bc5090", "receiver" = "#ffa600"),
    guide  = "none"
  ) +
  scale_x_continuous(labels = number_format(suffix = " pts")) +
  scale_y_continuous(labels = number_format(suffix = " pts")) +
  labs(
    title    = "XGBoost Predicted vs Actual PPR: 2025 Season Test Set",
    subtitle = glue("Dashed = perfect prediction | Dotted bands = +/-{RMSE_BAND} pts | ",
                    "Temporal holdout: weeks 15-18"),
    x        = "Predicted PPR Points",
    y        = "Actual PPR Points",
    caption  = "Data: nflfastR 2025 | Analysis: NFL Analytics Toolkit Week 9"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  here::here("output", "plots", "week9_predicted_vs_actual.png"),
  plot = p1, width = 12, height = 5, dpi = 300, bg = "white"
)
cat("  Saved: output/plots/week9_predicted_vs_actual.png\n")

if (requireNamespace("plotly",      quietly = TRUE) &&
    requireNamespace("htmlwidgets", quietly = TRUE)) {

  p1_interactive <- plotly::ggplotly(p1, tooltip = "text") %>%
    plotly::layout(
      title = list(text = paste0(
        "XGBoost Predicted vs Actual PPR: 2025 Season Test Set<br>",
        "<sub>Hover for player details | Dashed = perfect prediction</sub>"
      ))
    )

  htmlwidgets::saveWidget(
    p1_interactive,
    here::here("output", "plots", "week9_predicted_vs_actual.html"),
    selfcontained = TRUE,
    title         = "Week 9 XGBoost: Predicted vs Actual PPR (2025)"
  )
  cat("  Saved: output/plots/week9_predicted_vs_actual.html (interactive)\n")
} else {
  cat("  plotly/htmlwidgets not installed -- skipping interactive HTML.\n")
}


# ==============================================================================
# PLOT 2: Model vs Baseline RMSE (real 2025 test set)
# Static PNG: 3 positions x 3 models = 9 bars, well under 50-point threshold
# ==============================================================================

cat("\nBuilding Plot 2: Model vs Baseline RMSE (real 2025 data)...\n")

if (nrow(real_comparison) == 0 || all(is.na(real_comparison$rmse))) {
  cat("  WARNING: No RMSE comparison data available -- skipping Plot 2.\n")
} else {

  p2 <- real_comparison %>%
    filter(!is.na(rmse)) %>%
    mutate(
      model    = factor(model, levels = c("XGBoost", "Last Week", "Season Avg")),
      position = factor(position,
                        levels = c("passer", "rusher", "receiver"),
                        labels = c("QB (Passer)", "RB (Rusher)", "WR/TE (Receiver)"))
    ) %>%
    ggplot(aes(x = position, y = rmse, fill = model)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.9) +
    geom_text(aes(label = round(rmse, 1)),
              position = position_dodge(width = 0.75),
              vjust = -0.4, size = 3.2, fontface = "bold") +
    scale_fill_manual(
      values = c("XGBoost"    = "#003f5c",
                 "Last Week"  = "#bc5090",
                 "Season Avg" = "#ffa600"),
      name = "Model"
    ) +
    scale_y_continuous(
      labels = number_format(suffix = " pts"),
      expand = expansion(mult = c(0, 0.14))
    ) +
    labs(
      title    = "XGBoost vs Naive Baselines: Test Set RMSE by Position (2025 Season)",
      subtitle = "Lower RMSE = better prediction | XGBoost must beat 'Last Week' baseline to justify complexity",
      x        = "Position Group",
      y        = "RMSE (PPR Points)",
      caption  = "Data: nflfastR 2025 | Analysis: NFL Analytics Toolkit Week 9"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(size = 10, color = "gray40"),
      legend.position    = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank()
    )

  ggsave(
    here::here("output", "plots", "week9_model_vs_baseline_rmse.png"),
    plot = p2, width = 9, height = 6, dpi = 300, bg = "white"
  )
  cat("  Saved: output/plots/week9_model_vs_baseline_rmse.png\n")
}


# ==============================================================================
# PLOT 3: Availability Rate vs PPR Volatility (real 2025 players)
# Interactive HTML -- real data has 50+ labeled player-season observations
# ==============================================================================

cat("\nBuilding Plot 3: Availability rate vs PPR volatility (real 2025 data)...\n")

# Build per-player summary from all positions' training data
all_train_rows <- bind_rows(lapply(positions, function(pos) {
  if (is.null(models[[pos]])) return(NULL)
  td <- models[[pos]]$train_data
  if (!"availability_rate"    %in% names(td)) return(NULL)
  if (!"ppr_points_this_week" %in% names(td)) return(NULL)
  td %>%
    select(player_id, player_name, availability_rate, ppr_points_this_week) %>%
    filter(!is.na(ppr_points_this_week)) %>%
    mutate(position_group = pos)
}))

if (nrow(all_train_rows) == 0) {
  cat("  WARNING: No training data with availability_rate available -- skipping Plot 3.\n")
} else {

  player_avail_summary <- all_train_rows %>%
    group_by(player_id, player_name, position_group) %>%
    summarise(
      mean_ppr          = mean(ppr_points_this_week, na.rm = TRUE),
      sd_ppr            = sd(ppr_points_this_week,   na.rm = TRUE),
      availability_rate = last(availability_rate),
      n_games           = n(),
      .groups           = "drop"
    ) %>%
    filter(n_games >= 4, !is.na(sd_ppr), !is.na(availability_rate))

  cat(glue("  Availability summary: {nrow(player_avail_summary)} players ",
           "(>= 4 games)\n"))

  if (nrow(player_avail_summary) == 0) {
    cat("  WARNING: No players with >= 4 games -- skipping Plot 3.\n")
  } else {

    sd_max <- max(player_avail_summary$sd_ppr, na.rm = TRUE)

    p3 <- ggplot(player_avail_summary,
                 aes(x = availability_rate, y = sd_ppr,
                     color = position_group,
                     size  = mean_ppr,
                     text  = paste0(
                       "Player: ", player_name, "\n",
                       "Position: ", position_group, "\n",
                       "Availability: ", percent(availability_rate, accuracy = 0.1), "\n",
                       "Mean PPR/game: ", round(mean_ppr, 1), " pts\n",
                       "PPR Std Dev: ", round(sd_ppr, 1), " pts\n",
                       "Games in training set: ", n_games
                     ))) +
      geom_vline(xintercept = 0.80, color = "firebrick", linetype = "dashed",
                 linewidth = 0.8, alpha = 0.7) +
      annotate("text", x = 0.79, y = sd_max * 0.92,
               label = "80% threshold\n(injury risk)", hjust = 1, size = 3,
               color = "firebrick") +
      geom_point(alpha = 0.75) +
      geom_smooth(method = "lm", se = TRUE, color = "gray30",
                  linewidth = 0.7, alpha = 0.15, inherit.aes = FALSE,
                  aes(x = availability_rate, y = sd_ppr)) +
      scale_color_manual(
        values = c("passer" = "#003f5c", "rusher" = "#bc5090", "receiver" = "#ffa600"),
        name   = "Position"
      ) +
      scale_size_continuous(name = "Mean PPR/game", range = c(2, 8)) +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      scale_y_continuous(labels = number_format(suffix = " pts")) +
      labs(
        title    = "Availability Rate vs PPR Volatility: 2025 Season",
        subtitle = "Players below 80% availability introduce floor risk independent of per-game talent",
        x        = "Availability Rate (games played / team games)",
        y        = "PPR Standard Deviation (game-to-game volatility)",
        caption  = "Data: nflfastR 2025 | Analysis: NFL Analytics Toolkit Week 9"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(size = 10, color = "gray40"),
        panel.grid.minor = element_blank()
      )

    ggsave(
      here::here("output", "plots", "week9_availability_vs_volatility.png"),
      plot = p3, width = 10, height = 6, dpi = 300, bg = "white"
    )
    cat("  Saved: output/plots/week9_availability_vs_volatility.png\n")

    if (requireNamespace("plotly",      quietly = TRUE) &&
        requireNamespace("htmlwidgets", quietly = TRUE)) {

      p3_interactive <- plotly::ggplotly(p3, tooltip = "text") %>%
        plotly::layout(
          title = list(text = paste0(
            "Availability Rate vs PPR Volatility: 2025 Season<br>",
            "<sub>Players below 80% carry floor risk from injury history</sub>"
          ))
        )

      htmlwidgets::saveWidget(
        p3_interactive,
        here::here("output", "plots", "week9_availability_vs_volatility.html"),
        selfcontained = TRUE,
        title         = "Week 9: Availability Rate vs PPR Volatility (2025)"
      )
      cat("  Saved: output/plots/week9_availability_vs_volatility.html (interactive)\n")
    }
  }
}


# ==============================================================================
# PLOT 4: Next-Week Projections Leaderboard (real 2025 predictions)
# NEW -- shows the actual model output: who to start next week
# Static PNG: top N per position shown as dot + interval chart
# ==============================================================================

cat("\nBuilding Plot 4: Next-week projections leaderboard (real 2025 predictions)...\n")

# Required columns guard
required_pred_cols <- c("player_name", "position_group", "team", "opponent",
                        "predicted_ppr", "pi_lower", "pi_upper", "predicted_week")
missing_pred_cols  <- setdiff(required_pred_cols, names(all_preds))

if (length(missing_pred_cols) > 0) {
  cat(glue("  WARNING: all_preds missing columns: ",
           "{paste(missing_pred_cols, collapse = ', ')}. Skipping Plot 4.\n"))
} else {

  N_TOP          <- 12
  next_week_num  <- max(all_preds$predicted_week, na.rm = TRUE)

  leaderboard <- all_preds %>%
    filter(!is.na(predicted_ppr)) %>%
    group_by(position_group) %>%
    slice_max(order_by = predicted_ppr, n = N_TOP, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      pos_label    = factor(
        position_group,
        levels = c("passer", "rusher", "receiver"),
        labels = c("QB", "RB", "WR/TE")
      ),
      # Label shows matchup context below the player name
      player_label = paste0(player_name, "\n", team, " vs ", opponent),
      # Order within each facet: highest projected PPR at top
      player_label = reorder(player_label, predicted_ppr)
    )

  if (nrow(leaderboard) == 0) {
    cat("  WARNING: No projection data -- skipping Plot 4.\n")
  } else {

    p4 <- ggplot(leaderboard,
                 aes(x = predicted_ppr, y = player_label,
                     color = pos_label)) +
      # Prediction interval line
      geom_segment(aes(x = pi_lower, xend = pi_upper,
                       y = player_label, yend = player_label),
                   linewidth = 1.2, alpha = 0.35) +
      # Point estimate
      geom_point(size = 3.5, alpha = 0.9) +
      facet_wrap(~ pos_label, scales = "free_y", nrow = 1) +
      scale_color_manual(
        values = c("QB"    = "#003f5c",
                   "RB"    = "#bc5090",
                   "WR/TE" = "#ffa600"),
        guide  = "none"
      ) +
      scale_x_continuous(labels = number_format(suffix = " pts")) +
      labs(
        title    = glue("Week {next_week_num} Fantasy Projections: Top {N_TOP} Per Position"),
        subtitle = paste0(
          "Point = XGBoost predicted PPR | ",
          "Line = 95% prediction interval | ",
          "Wider line = higher floor-to-ceiling range"
        ),
        x       = "Projected PPR Points",
        y       = NULL,
        caption = "Data: nflfastR 2025 | Analysis: NFL Analytics Toolkit Week 9 XGBoost"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(size = 9, color = "gray40"),
        strip.text       = element_text(face = "bold", size = 11),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_text(size = 7.5)
      )

    ggsave(
      here::here("output", "plots", "week9_projections_leaderboard.png"),
      plot = p4, width = 14, height = 8, dpi = 300, bg = "white"
    )
    cat(glue("  Saved: output/plots/week9_projections_leaderboard.png\n"))
  }
}


# ==============================================================================
# CONSOLE SUMMARY STATS
# ==============================================================================

cat("\n============================================================\n")
cat("WEEK 9 VISUALIZATION SUMMARY (REAL 2025 DATA)\n")
cat("============================================================\n")

cat("\nTest evaluation (actual 2025 held-out weeks):\n")
eval_combined %>%
  group_by(position_group) %>%
  summarise(
    n           = n(),
    rmse        = round(sqrt(mean((actual_ppr - predicted_ppr)^2, na.rm = TRUE)), 2),
    mean_actual = round(mean(actual_ppr, na.rm = TRUE), 1),
    .groups     = "drop"
  ) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

if (nrow(real_comparison) > 0) {
  cat("\nModel vs Baseline RMSE summary:\n")
  real_comparison %>%
    filter(!is.na(rmse)) %>%
    arrange(position, rmse) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
    as.data.frame() %>%
    print(row.names = FALSE)
}

cat("\nNext-week projections:\n")
cat(sprintf("  Total players projected: %d\n", nrow(all_preds)))
all_preds %>%
  group_by(position_group) %>%
  summarise(
    n         = n(),
    mean_proj = round(mean(predicted_ppr, na.rm = TRUE), 1),
    .groups   = "drop"
  ) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\nFiles saved to output/plots/\n")
cat("  week9_predicted_vs_actual.png + .html\n")
cat("  week9_model_vs_baseline_rmse.png\n")
cat("  week9_availability_vs_volatility.png + .html\n")
cat("  week9_projections_leaderboard.png  [NEW -- real predictions]\n")
cat("============================================================\n")
