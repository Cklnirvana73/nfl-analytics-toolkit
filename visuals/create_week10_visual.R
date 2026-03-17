# ==============================================================================
# create_week10_visual.R
# Week 10: Boom / Bust Classification -- Visualizations
# NFL Analytics Toolkit
# examples/create_week10_visual.R
#
# PLOTS PRODUCED
#   Plot 1 (Static PNG):  Boom/bust probability distributions by actual outcome
#                         tier -- do model probabilities separate cleanly?
#   Plot 2 (Static PNG):  Confusion matrix heatmaps, one panel per position
#   Plot 3 (Static PNG):  Boom recall and precision by position (bar chart)
#   Plot 4 (Interactive HTML + Static PNG):  p_boom vs actual PPR, colored by
#                         outcome tier, all three positions, 50+ points triggers
#                         interactive version
#
# PREREQUISITES
# Run after train_classification_model() for all three positions.
# Requires boom_bust_results and ml_data_tiered in session, OR will rebuild
# from saved RDS artifacts.
#
# OUTPUT
#   output/plots/week10_probability_distributions.png
#   output/plots/week10_confusion_matrices.png
#   output/plots/week10_boom_recall_precision.png
#   output/plots/week10_pboom_vs_ppr.png
#   output/plots/week10_pboom_vs_ppr_interactive.html
#
# DEPENDENCIES
# library(ggplot2), library(dplyr), library(tidyr), library(purrr),
# library(here), library(glue), library(scales)
# Optional (interactive): library(plotly), library(htmlwidgets)
# ==============================================================================

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(glue)
library(scales)

# ------------------------------------------------------------------------------
# Setup: source production code and load / build required objects
# ------------------------------------------------------------------------------

source(here::here("R", "01_data_loading.R"))
source(here::here("R", "02_player_stats.R"))
source(here::here("R", "03_team_stats.R"))
source(here::here("R", "04_game_analysis.R"))
source(here::here("R", "05_consistency_metrics.R"))
source(here::here("R", "06_predictive_validation.R"))
source(here::here("R", "07_epa_features.R"))
source(here::here("R", "08_usage_features.R"))
source(here::here("R", "09_gamescript_features.R"))
source(here::here("R", "10_opponent_features.R"))
source(here::here("R", "11_xgboost_fantasy.R"))
source(here::here("R", "12_boom_bust_model.R"))

if (!exists("ml_data_tiered")) {
  message("Building ml_data_tiered from artifacts...")
  features <- readRDS(here::here("output", "feature_matrix_2025.rds"))
  pbp      <- readRDS(here::here("data", "cache", "pbp_2025_2025.rds"))
  ml_data_raw    <- prepare_model_features(features, pbp, seasons = 2025)
  rm(pbp); gc()
  ml_data_tiered <- define_outcome_tiers(ml_data_raw, train_weeks = 1:14,
                                          bust_pct = 0.25, boom_pct = 0.75)
  rm(ml_data_raw); gc()
}

if (!exists("boom_bust_results")) {
  message("Training classification models...")
  boom_bust_results <- list(
    passer   = train_classification_model(ml_data_tiered, "passer"),
    rusher   = train_classification_model(ml_data_tiered, "rusher"),
    receiver = train_classification_model(ml_data_tiered, "receiver")
  )
  gc()
}

# ------------------------------------------------------------------------------
# Output directory
# ------------------------------------------------------------------------------

PLOT_DIR <- here::here("output", "plots")
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

# ------------------------------------------------------------------------------
# Shared theme and palette
# ------------------------------------------------------------------------------

TIER_COLORS <- c(
  boom    = "#2166ac",   # blue
  average = "#969696",   # gray
  bust    = "#d6604d"    # red-orange
)

POSITION_LABELS <- c(
  passer   = "Passer",
  rusher   = "Rusher",
  receiver = "Receiver"
)

theme_toolkit <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(size = 10, color = "gray40"),
      plot.caption    = element_text(size = 8,  color = "gray50"),
      strip.text      = element_text(face = "bold", size = 11),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

# ==============================================================================
# PLOT 1: Probability distributions by actual outcome tier
# Question: Do boom/bust probabilities actually separate by outcome?
# ==============================================================================

message("\n[Plot 1] Building probability distributions...")

test_preds_all <- map_dfr(names(boom_bust_results), function(pos) {
  boom_bust_results[[pos]]$test_predictions %>%
    mutate(position_group = pos)
}) %>%
  filter(!is.na(outcome_tier)) %>%
  mutate(
    outcome_tier   = factor(outcome_tier, levels = c("boom", "average", "bust")),
    position_group = factor(position_group,
                            levels = names(POSITION_LABELS),
                            labels = POSITION_LABELS)
  )

# p_boom distribution faceted by position, colored by actual tier
p1 <- ggplot(test_preds_all,
             aes(x = p_boom, fill = outcome_tier, color = outcome_tier)) +
  geom_density(alpha = 0.35, linewidth = 0.7) +
  geom_vline(xintercept = 0.333, linetype = "dashed",
             color = "gray40", linewidth = 0.5) +
  scale_fill_manual(values  = TIER_COLORS,
                    labels  = c(boom = "Boom", average = "Average", bust = "Bust")) +
  scale_color_manual(values = TIER_COLORS,
                     labels = c(boom = "Boom", average = "Average", bust = "Bust")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  facet_wrap(~ position_group, ncol = 3) +
  labs(
    title    = "Boom Probability Distributions by Actual Outcome Tier",
    subtitle = "Test weeks 15-18, 2025 season | Dashed line = 33% (uniform baseline)",
    x        = "Predicted Boom Probability",
    y        = "Density",
    fill     = "Actual Outcome",
    color    = "Actual Outcome",
    caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nBoom = top 25% PPR scorer for position, Bust = bottom 25%"
  ) +
  theme_toolkit()

ggsave(
  filename = file.path(PLOT_DIR, "week10_probability_distributions.png"),
  plot     = p1,
  width    = 12, height = 5, dpi = 300, bg = "white"
)
message("  Saved: week10_probability_distributions.png")

# Console summary: mean p_boom by tier per position
dist_summary <- test_preds_all %>%
  group_by(position_group, outcome_tier) %>%
  summarise(
    n         = n(),
    mean_pboom = round(mean(p_boom, na.rm = TRUE), 3),
    .groups   = "drop"
  )
cat("\nMean p_boom by position and actual outcome tier:\n")
print(dist_summary)

# ==============================================================================
# PLOT 2: Confusion matrix heatmaps, one panel per position
# ==============================================================================
# PLOT 2: Confusion matrix heatmaps, one panel per position
# ==============================================================================

message("\n[Plot 2] Building confusion matrix heatmaps...")

cm_data <- map_dfr(names(boom_bust_results), function(pos) {
  preds <- boom_bust_results[[pos]]$test_predictions %>%
    filter(!is.na(outcome_tier), !is.na(.pred_class)) %>%
    mutate(
      actual    = factor(outcome_tier, levels = c("boom", "average", "bust")),
      predicted = factor(.pred_class,  levels = c("boom", "average", "bust"))
    )

  preds %>%
    count(actual, predicted) %>%
    group_by(actual) %>%
    mutate(
      pct            = n / sum(n),
      position_group = factor(pos,
                              levels = names(POSITION_LABELS),
                              labels = POSITION_LABELS)
    ) %>%
    ungroup()
})

p2 <- ggplot(cm_data, aes(x = predicted, y = actual, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(n, "\n(", percent(pct, accuracy = 1), ")")),
            size = 3.2, lineheight = 1.2) +
  scale_fill_gradient2(
    low      = "white",
    mid      = "#d1e5f0",
    high     = "#2166ac",
    midpoint = 0.33,
    labels   = percent_format(accuracy = 1),
    name     = "Row %\n(% of actual)"
  ) +
  scale_x_discrete(labels = c(boom = "Boom", average = "Average", bust = "Bust")) +
  scale_y_discrete(labels = c(boom = "Boom", average = "Average", bust = "Bust")) +
  facet_wrap(~ position_group, ncol = 3) +
  labs(
    title    = "Boom/Bust Classification: Confusion Matrices by Position",
    subtitle = "Test weeks 15-18, 2025 season | Row % = % of actual tier classified as each prediction",
    x        = "Predicted Tier",
    y        = "Actual Tier",
    caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nDiagonal = correct classifications"
  ) +
  theme_toolkit() +
  theme(legend.position = "right")

ggsave(
  filename = file.path(PLOT_DIR, "week10_confusion_matrices.png"),
  plot     = p2,
  width    = 13, height = 5, dpi = 300, bg = "white"
)
message("  Saved: week10_confusion_matrices.png")

diag_acc <- cm_data %>%
  group_by(position_group) %>%
  summarise(
    accuracy    = round(sum(n[actual == predicted]) / sum(n), 3),
    boom_recall = round(
      sum(n[actual == "boom" & predicted == "boom"]) /
        sum(n[actual == "boom"]), 3
    ),
    .groups = "drop"
  )
cat("\nOverall accuracy and boom recall by position:\n")
print(diag_acc)

# ==============================================================================
# PLOT 3: Boom recall and precision by position
# ==============================================================================

message("\n[Plot 3] Building boom recall/precision bar chart...")

perf_data <- map_dfr(names(boom_bust_results), function(pos) {
  eval_result <- evaluate_classifier(boom_bust_results[[pos]], verbose = FALSE)

  # boom_recall and boom_precision are scalars in the return list,
  # not inside class_metrics (which uses macro_weighted estimator, no .level column)
  boom_recall    <- eval_result$boom_recall
  boom_precision <- eval_result$boom_precision
  boom_f1 <- if (!is.na(boom_recall) && !is.na(boom_precision) &&
                   (boom_recall + boom_precision) > 0) {
    round(2 * boom_recall * boom_precision / (boom_recall + boom_precision), 4)
  } else {
    NA_real_
  }

  tibble(
    position_group = factor(pos,
                            levels = names(POSITION_LABELS),
                            labels = POSITION_LABELS),
    metric         = c("Boom Recall", "Boom Precision", "Boom F1"),
    value          = c(boom_recall, boom_precision, boom_f1)
  )
}) %>%
  filter(!is.na(value)) %>%
  mutate(metric = factor(metric, levels = c("Boom Recall", "Boom Precision", "Boom F1")))

p3 <- ggplot(perf_data, aes(x = position_group, y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  geom_text(
    aes(label = percent(value, accuracy = 1)),
    position = position_dodge(width = 0.75),
    vjust    = -0.4,
    size     = 3.2
  ) +
  annotate("text",
           x = 0.55, y = 0.02,
           label = "Naive baseline (0% boom recall)",
           hjust = 0, size = 3, color = "gray40") +
  scale_fill_manual(values = c(
    "Boom Recall"    = "#2166ac",
    "Boom Precision" = "#4dac26",
    "Boom F1"        = "#b8860b"
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1.05)) +
  labs(
    title    = "Boom Classification Performance by Position",
    subtitle = "Test weeks 15-18, 2025 season | Boom = top 25% PPR scorer for position",
    x        = NULL,
    y        = "Metric Value",
    fill     = NULL,
    caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nNaive baseline predicts 'average' for all players (0% boom recall)"
  ) +
  theme_toolkit()

ggsave(
  filename = file.path(PLOT_DIR, "week10_boom_recall_precision.png"),
  plot     = p3,
  width    = 9, height = 6, dpi = 300, bg = "white"
)
message("  Saved: week10_boom_recall_precision.png")

cat("\nBoom recall/precision/F1 by position:\n")
print(perf_data %>% mutate(value = round(value, 3)))

# ==============================================================================
# PLOT 4: p_boom vs actual PPR -- static PNG + interactive HTML
# ==============================================================================

message("\n[Plot 4] Building p_boom vs actual PPR scatter...")

scatter_data <- map_dfr(names(boom_bust_results), function(pos) {
  preds <- boom_bust_results[[pos]]$test_predictions %>%
    filter(!is.na(outcome_tier))

  actuals <- ml_data_tiered %>%
    filter(position_group == pos, week >= 15) %>%
    select(player_id, week, ppr_points_this_week)

  preds %>%
    inner_join(actuals, by = c("player_id", "week"),
               relationship = "many-to-many") %>%
    distinct(player_id, week, .keep_all = TRUE) %>%
    mutate(
      position_group = factor(pos,
                              levels = names(POSITION_LABELS),
                              labels = POSITION_LABELS),
      outcome_tier   = factor(outcome_tier, levels = c("boom", "average", "bust"))
    )
})

n_labeled <- nrow(scatter_data)
DENSITY_THRESHOLD <- 50L

cat(paste0("\n  Scatter data: ", n_labeled,
           " player-weeks across all positions\n"))

# Static PNG
p4 <- ggplot(scatter_data,
             aes(x = p_boom, y = ppr_points_this_week, color = outcome_tier)) +
  geom_point(alpha = 0.55, size = 1.8) +
  geom_vline(xintercept = 0.333, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  scale_color_manual(values = TIER_COLORS,
                     labels = c(boom = "Boom", average = "Average", bust = "Bust")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  facet_wrap(~ position_group, ncol = 3, scales = "free_y") +
  labs(
    title    = "Boom Probability vs Actual PPR Score",
    subtitle = "Test weeks 15-18, 2025 season | Points colored by actual outcome tier",
    x        = "Predicted Boom Probability",
    y        = "Actual PPR Points",
    color    = "Actual Outcome",
    caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nDashed line = 33% uniform baseline | Each point = one player-week"
  ) +
  theme_toolkit()

ggsave(
  filename = file.path(PLOT_DIR, "week10_pboom_vs_ppr.png"),
  plot     = p4,
  width    = 13, height = 5, dpi = 300, bg = "white"
)
message("  Saved: week10_pboom_vs_ppr.png (static)")

# Interactive HTML
if (n_labeled >= DENSITY_THRESHOLD) {
  if (!requireNamespace("plotly",      quietly = TRUE) ||
      !requireNamespace("htmlwidgets", quietly = TRUE)) {
    warning("plotly or htmlwidgets not installed -- skipping interactive HTML.")
  } else {
    library(plotly)
    library(htmlwidgets)

    positions <- levels(scatter_data$position_group)
    tiers     <- c("boom", "average", "bust")
    tier_labels <- c(boom = "Boom", average = "Average", bust = "Bust")

    p4_interactive <- plot_ly()

    for (pos in positions) {
      for (tier in tiers) {
        sub_data <- scatter_data %>%
          filter(as.character(position_group) == pos,
                 as.character(outcome_tier)   == tier)
        if (nrow(sub_data) == 0) next

        p4_interactive <- add_trace(
          p4_interactive,
          data        = sub_data,
          x           = ~p_boom,
          y           = ~ppr_points_this_week,
          type        = "scatter",
          mode        = "markers",
          name        = paste0(pos, " - ", tier_labels[[tier]]),
          legendgroup = paste0(pos, " - ", tier_labels[[tier]]),
          marker      = list(
            color   = TIER_COLORS[[tier]],
            opacity = 0.65,
            size    = 7
          ),
          text      = ~paste0(
            player_name, "<br>",
            "Week: ", week, "<br>",
            "Actual PPR: ", round(ppr_points_this_week, 1), "<br>",
            "p_boom: ",    scales::percent(p_boom,    accuracy = 1), "<br>",
            "p_average: ", scales::percent(p_average, accuracy = 1), "<br>",
            "p_bust: ",    scales::percent(p_bust,    accuracy = 1), "<br>",
            "Actual tier: ", as.character(outcome_tier)
          ),
          hoverinfo = "text"
        )
      }
    }

    p4_interactive <- layout(
      p4_interactive,
      title = list(
        text = paste0(
          "Boom Probability vs Actual PPR Score (Interactive)",
          "<br><sup>Test weeks 15-18, 2025 | Click legend entries to toggle groups</sup>"
        ),
        font = list(size = 14)
      ),
      xaxis = list(
        title      = "Predicted Boom Probability",
        tickformat = ".0%",
        range      = c(0, 1)
      ),
      yaxis  = list(title = "Actual PPR Points"),
      legend = list(
        title       = list(text = "Position - Actual Tier"),
        orientation = "v"
      ),
      shapes = list(list(
        type = "line",
        x0 = 0.333, x1 = 0.333,
        y0 = 0,     y1 = 1,
        xref = "x", yref = "paper",
        line = list(dash = "dash", color = "gray", width = 1)
      )),
      annotations = list(list(
        text      = "Data: nflfastR | Analysis: NFL Analytics Toolkit",
        xref      = "paper", yref = "paper",
        x = 0, y = -0.08,
        showarrow = FALSE,
        font      = list(size = 10, color = "gray")
      ))
    )

    html_path <- file.path(PLOT_DIR, "week10_pboom_vs_ppr_interactive.html")
    saveWidget(
      widget        = p4_interactive,
      file          = html_path,
      selfcontained = TRUE,
      title         = "Week 10: Boom Probability vs Actual PPR"
    )
    message(paste0("  Saved: week10_pboom_vs_ppr_interactive.html (",
                   n_labeled, " points)"))
  }
} else {
  message(paste0("  Skipping interactive HTML: ", n_labeled,
                 " points < threshold (", DENSITY_THRESHOLD, ")"))
}

# ==============================================================================
# Final summary
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("Week 10 visualizations complete\n")
cat(strrep("=", 60), "\n\n")
cat("Files saved to:", PLOT_DIR, "\n\n")
cat("Static PNGs (300 dpi):\n")
cat("  week10_probability_distributions.png\n")
cat("  week10_confusion_matrices.png\n")
cat("  week10_boom_recall_precision.png\n")
cat("  week10_pboom_vs_ppr.png\n")
if (n_labeled >= DENSITY_THRESHOLD) {
  cat("Interactive HTML:\n")
  cat("  week10_pboom_vs_ppr_interactive.html\n")
}
cat("\nKey findings to check:\n")
cat("  1. Plot 1: Blue (boom) density should peak right of 0.33, red (bust) left\n")
cat("  2. Plot 2: Diagonal cells should be darkest -- correct classifications\n")
cat("  3. Plot 3: All positions show boom recall > 0% (beats naive baseline)\n")
cat("  4. Plot 4: High p_boom points should cluster in upper right per panel\n")
