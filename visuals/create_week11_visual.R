# =============================================================================
# NFL Analytics Toolkit - Week 11 Visualization Script
# =============================================================================
# File:    examples/create_week11_visual.R
# Purpose: Publication-quality plots from week11_full_results_2025.rds
#
# Built using:
#   - Data Science Code Reviewer visualization-patterns.md (v4.6)
#   - NFL Analytics Expert skill
#
# Design decisions (per visualization-patterns.md):
#   Plot 1 -- Model comparison (ranking): dot plot, correct chart type
#   Plot 2 -- Blend weights (composition): horizontal bar, geom_col()
#   Plot 3 -- Feature importance static top-15 (ranking): lollipop
#             45 bars total (15 per position) -- UNDER 50 threshold,
#             no interactive required for this one
#   Plot 4 -- Feature importance interactive ALL features (23 per position
#             = 69 bars) -- OVER 50 threshold, HTML required
#             Uses ggplotly(tooltip = "text") per reference to avoid
#             native plotly trace-splitting / customdata size mismatches
#
# Factor ordering rule applied: arrange() + factor() before every plot call.
#   stats::reorder() inside a plotly formula renders as literal call string.
#
# Silent facet exclusion rule: all three positions always shown; any
#   exclusion documented in subtitle/caption.
#
# Colorblind-safe palette (Okabe-Ito) throughout.
#
# Outputs (all to output/plots/):
#   week11_model_comparison.png
#   week11_ensemble_weights.png
#   week11_feature_importance.png
#   week11_feature_importance_interactive.html   (selfcontained)
#
# PREREQUISITES:
#   output/week11_full_results_2025.rds  -- from run_full_pipeline()
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(glue)
library(here)

# =============================================================================
# Setup and artifact loading
# Per visualization-patterns.md "Loading From Saved Pipeline Artifacts":
# File-existence guard, load from RDS, never re-run pipeline inline.
# =============================================================================

SEASON       <- 2025L
OUT_DIR      <- here::here("output", "plots")
RESULTS_PATH <- here::here("output", glue("week11_full_results_{SEASON}.rds"))

if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR, recursive = TRUE)
  cat(glue("Created output directory: {OUT_DIR}\n"))
}

if (!file.exists(RESULTS_PATH)) {
  stop(
    "Required artifact not found: ", RESULTS_PATH, "\n",
    "Run run_full_pipeline() from R/13_ensemble_pipeline.R first.",
    call. = FALSE
  )
}

cat(glue("Loading Week 11 results from: {RESULTS_PATH}\n"))
results <- readRDS(RESULTS_PATH)

required_slots <- c("comparisons", "ensembles", "features", "summary")
missing_slots  <- setdiff(required_slots, names(results))
if (length(missing_slots) > 0L) {
  stop(glue("Results artifact missing slots: {paste(missing_slots, collapse = ', ')}"),
       call. = FALSE)
}

positions       <- names(results$comparisons)
POSITION_LABELS <- c(passer = "Quarterback", rusher = "Running Back",
                     receiver = "Receiver")

# Okabe-Ito colorblind-safe palette (visualization-patterns.md Section 3)
POS_COLORS <- c(passer = "#0072B2", rusher = "#D55E00", receiver = "#009E73")

FAMILY_COLORS <- c(
  "XGBoost"       = "#0072B2",
  "Random Forest" = "#D55E00",
  "Elastic Net"   = "#009E73",
  "Other"         = "#999999"
)

# Shared theme (visualization-patterns.md Section 7)
theme_toolkit <- function(base_size = 12L) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.15), hjust = 0),
      plot.subtitle    = element_text(color = "gray40", size = rel(0.88), hjust = 0),
      plot.caption     = element_text(color = "gray55", size = rel(0.72), hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title       = element_text(color = "gray30", size = rel(0.9)),
      axis.text        = element_text(color = "gray40"),
      strip.text       = element_text(face = "bold", size = rel(1.0)),
      legend.position  = "bottom",
      legend.title     = element_text(size = rel(0.85)),
      plot.margin      = margin(10, 15, 10, 10)
    )
}

ATTRIBUTION <- "Data: nflfastR | Analysis: NFL Analytics Toolkit | Week 11"

# =============================================================================
# PLOT 1: Model Comparison -- RMSE dot plot
# Chart type: dot plot (ranking comparison, multiple groups per category)
# Reference: "Ranking -> dot plot, lollipop chart"
# Faceted by position, scales = "free_x" (RMSE ranges differ per position)
# Titles state the FINDING (visualization-patterns.md Section 5)
# =============================================================================

cat("\nBuilding Plot 1: Model comparison...\n")

comp_metrics <- dplyr::bind_rows(
  lapply(names(results$comparisons), function(pos) {
    results$comparisons[[pos]]$metrics_table %>%
      dplyr::mutate(position = pos)
  })
)

ens_metrics <- dplyr::bind_rows(
  lapply(names(results$ensembles), function(pos) {
    results$ensembles[[pos]]$test_metrics
  })
)

all_metrics <- dplyr::bind_rows(comp_metrics, ens_metrics) %>%
  dplyr::mutate(
    position_label = factor(
      POSITION_LABELS[position],
      levels = c("Quarterback", "Running Back", "Receiver")
    ),
    is_baseline = grepl("baseline", model),
    is_ensemble = model == "ensemble",
    model_label = dplyr::case_when(
      model == "xgboost"             ~ "XGBoost",
      model == "random_forest"        ~ "Random Forest",
      model == "elastic_net"          ~ "Elastic Net",
      model == "ensemble"             ~ "Ensemble",
      model == "season_avg_baseline"  ~ "Season Avg (baseline)",
      model == "persistence_baseline" ~ "Persistence (baseline)",
      TRUE                            ~ model
    )
  )

# Factor ordering: worst RMSE at top of y-axis (highest value = last factor level
# = bottom of chart; lowest RMSE = first level = top position within facet)
# We want best model at top so it is the last level drawn.
# ggplot y-axis: factor level 1 = bottom, last level = top.
# Model order (level 1 = bottom = worst):
model_order <- c(
  "Persistence (baseline)", "Season Avg (baseline)",
  "XGBoost", "Random Forest", "Elastic Net", "Ensemble"
)

all_metrics <- all_metrics %>%
  dplyr::filter(model_label %in% model_order) %>%
  dplyr::mutate(
    model_label  = factor(model_label, levels = model_order),
    point_shape  = dplyr::case_when(
      is_ensemble ~ "diamond filled",
      is_baseline ~ "circle open",
      TRUE        ~ "circle"
    ),
    point_size  = ifelse(is_ensemble, 4.5, 3.5),
    point_alpha = ifelse(is_baseline, 0.55, 1.0)
  )

# Best model per position for dashed reference line
best_per_pos <- all_metrics %>%
  dplyr::filter(!is_baseline) %>%
  dplyr::group_by(position_label) %>%
  dplyr::slice_min(rmse, n = 1L) %>%
  dplyr::ungroup()

p1 <- ggplot2::ggplot(
  all_metrics,
  ggplot2::aes(x = rmse, y = model_label, color = position_label)
) +
  ggplot2::geom_vline(
    data      = best_per_pos,
    ggplot2::aes(xintercept = rmse, color = position_label),
    linetype  = "dashed", linewidth = 0.55, alpha = 0.45
  ) +
  ggplot2::geom_point(
    ggplot2::aes(shape = point_shape, size = point_size, alpha = point_alpha)
  ) +
  ggplot2::scale_shape_identity() +
  ggplot2::scale_size_identity() +
  ggplot2::scale_alpha_identity() +
  ggplot2::scale_color_manual(
    values = POS_COLORS,
    labels = c("Quarterback", "Running Back", "Receiver"),
    name   = "Position"
  ) +
  ggplot2::facet_wrap(~ position_label, ncol = 1L, scales = "free_x") +
  ggplot2::labs(
    title    = "All models beat persistence -- EN leads for RBs and Receivers",
    subtitle = glue("RMSE on held-out test weeks 15-18 | 2025 season | ",
                    "Lower = better | Dashed line = best model per position"),
    x        = "RMSE (PPR Fantasy Points)",
    y        = NULL,
    caption  = ATTRIBUTION
  ) +
  theme_toolkit()

cat("Model comparison RMSE by position:\n")
all_metrics %>%
  dplyr::select(position_label, model_label, rmse, mae) %>%
  dplyr::arrange(position_label, rmse) %>%
  print(n = 30L)

ggplot2::ggsave(
  filename = file.path(OUT_DIR, "week11_model_comparison.png"),
  plot     = p1,
  width    = 9L, height = 11L, dpi = 300L,
  bg       = "white"
)
cat("Saved: week11_model_comparison.png\n")

# =============================================================================
# PLOT 2: Ensemble Blend Weights
# Chart type: horizontal bar -- geom_col() for pre-computed values
# Reference: "Composition -> horizontal bar"; "geom_col() is shorthand for
#   geom_bar(stat='identity')" listed as common false positive NOT to flag
# Factor ordering rule: arrange() + factor() BEFORE plot call
# Silent facet exclusion rule: all 3 positions shown
# Model family via startsWith() -- exact prefix match guaranteed by
#   add_candidates(name = "xgb/rf/en")
# =============================================================================

cat("\nBuilding Plot 2: Ensemble blend weights...\n")

weights_raw <- dplyr::bind_rows(
  lapply(names(results$ensembles), function(pos) {
    results$ensembles[[pos]]$member_weights %>%
      dplyr::mutate(position = pos)
  })
)

cat("  Raw member names (first 12 rows):\n")
print(head(weights_raw %>% dplyr::select(position, member, weight), 12L))

# startsWith() over grepl: exact prefix match, version-stable
# Handles both old artifact naming (suite_list.xgb_res) and new (xgb_1_1)
weights_data <- weights_raw %>%
  dplyr::mutate(
    position_label = factor(
      POSITION_LABELS[position],
      levels = c("Quarterback", "Running Back", "Receiver")
    ),
    model_family = dplyr::case_when(
      startsWith(member, "xgb") |
        grepl("xgboost|boost_tree", member, ignore.case = TRUE)  ~ "XGBoost",
      startsWith(member, "rf") |
        grepl("rand_forest|ranger", member, ignore.case = TRUE)  ~ "Random Forest",
      startsWith(member, "en") |
        grepl("linear_reg|glmnet|elastic", member, ignore.case = TRUE) ~ "Elastic Net",
      TRUE ~ "Other"
    )
  )

weights_agg <- weights_data %>%
  dplyr::group_by(position, position_label, model_family) %>%
  dplyr::summarise(
    total_weight = round(sum(weight, na.rm = TRUE), 3L),
    n_members    = dplyr::n(),
    .groups      = "drop"
  ) %>%
  # Factor ordering: set levels so bars display in meaningful order
  dplyr::mutate(
    model_family = factor(model_family,
                          levels = c("Other", "Elastic Net",
                                     "Random Forest", "XGBoost"))
  )

cat("Ensemble blend weights by position:\n")
print(weights_agg %>% dplyr::select(position_label, model_family, total_weight))

p2 <- ggplot2::ggplot(
  weights_agg,
  ggplot2::aes(x = total_weight, y = model_family, fill = model_family)
) +
  ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = ifelse(total_weight > 0.005, round(total_weight, 2), "0"),
      hjust = ifelse(total_weight > 0.15, 1.25, -0.15)
    ),
    size = 3.3, color = "gray20"
  ) +
  ggplot2::scale_fill_manual(values = FAMILY_COLORS) +
  ggplot2::scale_x_continuous(
    limits = c(0, NA),
    expand = ggplot2::expansion(mult = c(0, 0.20))
  ) +
  ggplot2::facet_wrap(~ position_label, ncol = 1L, scales = "free_y") +
  ggplot2::labs(
    title    = "Ridge meta-learner selects one model family per position",
    subtitle = "EN dominates QBs; tree models split weight for RBs and Receivers",
    x        = "Blend Weight",
    y        = NULL,
    caption  = paste0(
      ATTRIBUTION, "\n",
      "Weight = 0: model excluded by ridge regularization | ",
      "'Other': member names not matching xgb/rf/en prefix"
    )
  ) +
  theme_toolkit()

ggplot2::ggsave(
  filename = file.path(OUT_DIR, "week11_ensemble_weights.png"),
  plot     = p2,
  width    = 8L, height = 9L, dpi = 300L,
  bg       = "white"
)
cat("Saved: week11_ensemble_weights.png\n")

# =============================================================================
# PLOT 3: Consensus Feature Importance -- static, top 15 per position
# Chart type: lollipop (ranking, many ordered categories)
# 15 per position = 45 bars total -- UNDER 50, no interactive needed
#
# Faceted Heatmap Ordering Rule (visualization-patterns.md):
#   1. arrange() by importance ASCENDING within each group
#      (lowest = level 1 = bottom; highest = last level = TOP of chart)
#   2. Set factor levels from that order BEFORE ggplot call
#   3. Never use unique() on a mixed-group vector
#   4. With scales = "free_y" each facet uses its own levels correctly
#
# Silent facet exclusion: all 3 positions shown, no filter applied.
# NFL analytics: plays_this_week dominance flagged in caption.
# =============================================================================

cat("\nBuilding Plot 3: Feature importance (static, top 15)...\n")

feat_raw <- dplyr::bind_rows(
  lapply(names(results$features), function(pos) {
    results$features[[pos]]$importance_table %>%
      dplyr::mutate(position = pos)
  })
)

feat_data <- feat_raw %>%
  dplyr::mutate(
    position_label = factor(
      POSITION_LABELS[position],
      levels = c("Quarterback", "Running Back", "Receiver")
    ),
    feature_short = if (requireNamespace("stringr", quietly = TRUE)) {
      stringr::str_trunc(feature, 28L, ellipsis = "..")
    } else {
      substr(feature, 1L, 28L)
    }
  ) %>%
  dplyr::group_by(position_label) %>%
  dplyr::slice_min(consensus_rank, n = 15L) %>%
  dplyr::ungroup()

# Faceted Heatmap Ordering Rule:
# Sort ascending within position (worst first -> best last -> top of chart)
# Use a composite key (position + feature) so levels are unique across facets
feat_order <- feat_data %>%
  dplyr::arrange(position_label, mean_importance) %>%
  dplyr::mutate(feat_key = paste0(as.character(position_label), "__", feature_short)) %>%
  dplyr::pull(feat_key) %>%
  unique()

feat_data <- feat_data %>%
  dplyr::mutate(
    feat_key = paste0(as.character(position_label), "__", feature_short),
    feat_key = factor(feat_key, levels = feat_order)
  )

p3 <- ggplot2::ggplot(
  feat_data,
  ggplot2::aes(x = mean_importance, y = feat_key, color = is_consensus)
) +
  # Lollipop: segment first (lower layer), point on top
  ggplot2::geom_segment(
    ggplot2::aes(x = 0, xend = mean_importance, yend = feat_key),
    linewidth = 0.55, alpha = 0.70
  ) +
  ggplot2::geom_point(size = 3.0) +
  ggplot2::scale_color_manual(
    values = c("TRUE" = "#0072B2", "FALSE" = "#999999"),
    labels = c("TRUE"  = "Consensus (top 20 in >= 2 models)",
               "FALSE" = "Model-specific"),
    name   = NULL
  ) +
  # Strip the position prefix from y-axis labels for clean display
  ggplot2::scale_y_discrete(
    labels = function(x) sub("^[^_]*__", "", x)
  ) +
  ggplot2::facet_wrap(~ position_label, ncol = 1L, scales = "free_y") +
  ggplot2::labs(
    title    = "Volume and efficiency dominate -- but team matters for QBs",
    subtitle = paste0(
      "Mean normalized importance (0-1) across XGBoost (gain), ",
      "Random Forest (permutation), Elastic Net (coefficient) | Top 15 per position"
    ),
    x        = "Mean Normalized Importance (0-1 scale)",
    y        = NULL,
    caption  = paste0(
      ATTRIBUTION, "\n",
      "Blue = consensus pick (top 20 in >= 2 models) | ",
      "plays_this_week dominance for RBs/Receivers reflects opportunity volume -- ",
      "treat as opportunity signal, not pure skill."
    )
  ) +
  theme_toolkit() +
  ggplot2::theme(legend.position = "top")

cat("Top 5 consensus features per position:\n")
feat_data %>%
  dplyr::filter(is_consensus) %>%
  dplyr::group_by(position_label) %>%
  dplyr::slice_min(consensus_rank, n = 5L) %>%
  dplyr::select(position_label, feature, mean_importance, consensus_rank) %>%
  print(n = 20L)

ggplot2::ggsave(
  filename = file.path(OUT_DIR, "week11_feature_importance.png"),
  plot     = p3,
  width    = 10L, height = 13L, dpi = 300L,
  bg       = "white"
)
cat("Saved: week11_feature_importance.png\n")

# =============================================================================
# PLOT 4: Feature Importance -- INTERACTIVE HTML (all 23 features per position)
# 23 features x 3 positions = 69 bars -- OVER 50 threshold, HTML required
#
# Per visualization-patterns.md "Interactive Visualization" section:
# CORRECT APPROACH: ggplotly(p, tooltip = "text") with text built as
# a paste0() in aes(). This is explicitly documented in the reference.
#
# Why NOT native plotly: color = ~variable splits data into multiple traces
# internally. customdata must match EACH TRACE's row count, not the full
# dataset. ggplotly() avoids this entirely -- it converts a ggplot where
# all data transformations are done correctly in the grammar of graphics.
#
# selfcontained = TRUE: single portable HTML file, no external dependencies.
# Static PNG (Plot 3) is the shareable artifact.
# HTML is for exploration only -- cannot be embedded in LinkedIn or PDFs.
# =============================================================================

cat("\nBuilding Plot 4: Interactive feature importance (all features)...\n")

if (requireNamespace("plotly", quietly = TRUE) &&
    requireNamespace("htmlwidgets", quietly = TRUE)) {

  feat_full <- feat_raw %>%
    dplyr::mutate(
      position_label = factor(
        POSITION_LABELS[position],
        levels = c("Quarterback", "Running Back", "Receiver")
      ),
      consensus_label = ifelse(is_consensus, "Consensus", "Model-specific"),
      feature_short   = if (requireNamespace("stringr", quietly = TRUE)) {
        stringr::str_trunc(feature, 30L, ellipsis = "..")
      } else {
        substr(feature, 1L, 30L)
      }
    )

  # Faceted Heatmap Ordering Rule applied identically to Plot 3
  feat_full_order <- feat_full %>%
    dplyr::arrange(position_label, mean_importance) %>%
    dplyr::mutate(feat_key = paste0(as.character(position_label), "__", feature_short)) %>%
    dplyr::pull(feat_key) %>%
    unique()

  feat_full <- feat_full %>%
    dplyr::mutate(
      feat_key = paste0(as.character(position_label), "__", feature_short),
      feat_key = factor(feat_key, levels = feat_full_order),
      # Hover text built as aes() -- ggplotly(tooltip = "text") picks this up.
      # NA values in importance columns handled with coalesce to display "N/A"
      hover_text = paste0(
        "<b>", feature_short, "</b> (", position_label, ")<br>",
        "Mean importance: ", round(mean_importance, 3L), "<br>",
        "XGBoost gain: ",
          ifelse(is.na(importance_xgb), "N/A", round(importance_xgb, 3L)), "<br>",
        "RF permutation: ",
          ifelse(is.na(importance_rf), "N/A", round(importance_rf, 3L)), "<br>",
        "Elastic Net coef: ",
          ifelse(is.na(importance_en), "N/A", round(importance_en, 3L)), "<br>",
        "Consensus rank: ", consensus_rank, " | ",
        ifelse(is_consensus, "CONSENSUS", "model-specific")
      )
    )

  # Build ggplot with text aesthetic
  p4_base <- ggplot2::ggplot(
    feat_full,
    ggplot2::aes(
      x    = mean_importance,
      y    = feat_key,
      fill = consensus_label,
      text = hover_text          # ggplotly uses this via tooltip = "text"
    )
  ) +
    ggplot2::geom_col(orientation = "y", width = 0.70) +
    ggplot2::scale_fill_manual(
      values = c("Consensus" = "#0072B2", "Model-specific" = "#999999"),
      name   = NULL
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) sub("^[^_]*__", "", x)
    ) +
    ggplot2::facet_wrap(~ position_label, ncol = 1L, scales = "free_y") +
    ggplot2::labs(
      x = "Mean Normalized Importance (0-1)", y = NULL
    ) +
    theme_toolkit(base_size = 10L) +
    ggplot2::theme(legend.position = "bottom")

  # Convert to interactive -- ggplotly handles all trace management correctly
  p4_interactive <- plotly::ggplotly(p4_base, tooltip = "text") %>%
    plotly::layout(
      title = list(
        text = paste0(
          "<b>Feature Importance Across All Models - 2025</b><br>",
          "<sub>Hover for per-model breakdown | Blue = consensus | ",
          "Toggle positions via legend</sub>"
        ),
        x    = 0.02
      ),
      legend = list(orientation = "h", x = 0, y = -0.03),
      margin = list(l = 160L, r = 20L, b = 60L, t = 80L)
    ) %>%
    plotly::config(
      modeBarButtonsToRemove = c("lasso2d", "select2d"),
      displaylogo            = FALSE
    )

  html_path <- file.path(OUT_DIR, "week11_feature_importance_interactive.html")
  htmlwidgets::saveWidget(
    widget        = p4_interactive,
    file          = html_path,
    selfcontained = TRUE    # single file, no external dependencies
  )
  cat(glue(
    "Saved interactive HTML ({nrow(feat_full)} bars, density threshold applied): ",
    "week11_feature_importance_interactive.html\n"
  ))

} else {
  cat("plotly or htmlwidgets not installed. Skipping interactive plot.\n")
  cat("Install with: install.packages(c('plotly', 'htmlwidgets'))\n")
}

# =============================================================================
# Summary console output
# =============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("Week 11 visualizations complete\n")
cat(strrep("=", 60), "\n\n")
cat("Saved to:", OUT_DIR, "\n\n")
cat("  week11_model_comparison.png                -- Plot 1 (static)\n")
cat("  week11_ensemble_weights.png                -- Plot 2 (static)\n")
cat("  week11_feature_importance.png              -- Plot 3 (static, top 15)\n")
cat("  week11_feature_importance_interactive.html -- Plot 4 (interactive, all)\n\n")
cat("NOTE: HTML is for local exploration only.\n")
cat("      Static PNGs are the shareable artifacts (LinkedIn, README, PDF).\n")
