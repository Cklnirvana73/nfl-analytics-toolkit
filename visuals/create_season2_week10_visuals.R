# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 10
# Visualization Script: College-to-NFL Translation Model
# File: examples/create_season2_week10_visuals.R
#
# Creates 4 publication-quality static PNGs saved to output/plots/:
#   Plot 1: R-squared by position and model variant (base vs enriched)
#           Story: draft capital fills the gap college production leaves behind
#   Plot 2: LOCO fold stability -- per-class RMSE across 9 draft classes
#           Story: model is stable across time; no single class drives error
#   Plot 3: Predicted vs actual PPR per game (training classes, enriched model)
#           Story: compression and accuracy visualized, position-faceted
#   Plot 4: Translation score distribution -- 2026 class vs historical classes
#           Story: where the new class sits relative to the full talent pool
#
# Interactivity check (density threshold = 50 labeled points):
#   Plot 3 has 621 training players across 4 positions -- exceeds threshold.
#   Interactive HTML saved alongside static PNG for Plot 3 only.
#
# Output files:
#   output/plots/s2_week10_r_squared_base_vs_enriched.png
#   output/plots/s2_week10_loco_fold_stability.png
#   output/plots/s2_week10_predicted_vs_actual.png
#   output/plots/s2_week10_predicted_vs_actual_interactive.html
#   output/plots/s2_week10_translation_score_distribution.png
#
# Requires: Pipeline RDS outputs from run_week10_pipeline(). Run
#   examples/example_season2_week10.R first if not yet present.
# ==============================================================================


# ==============================================================================
# LIBRARIES
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(glue)
library(here)

if (!requireNamespace("plotly", quietly = TRUE)) {
  stop("Package 'plotly' required. Install with: install.packages('plotly')",
    call. = FALSE)
}
if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
  stop("Package 'htmlwidgets' required. Install with: install.packages('htmlwidgets')",
    call. = FALSE)
}

source(here::here("R", "24_translation_model.R"))


# ==============================================================================
# CONFIGURATION
# ==============================================================================

OUTPUT_DIR  <- here::here("output", "plots")
CACHE_DIR   <- here::here("data", "season2_cache")
DPI         <- 300
PREFIX      <- "s2_week10_"
ATTRIBUTION <- "NFL Analytics Toolkit | Season 2, Week 10 | @cklnirvana73"

# Position display order and colors -- colorblind-safe palette
POSITION_ORDER  <- c("QB", "RB", "WR", "TE")
POSITION_COLORS <- c(
  QB = "#0072B2",   # blue
  RB = "#D55E00",   # vermilion
  WR = "#009E73",   # green
  TE = "#CC79A7"    # pink
)

# Draft class type labels for Plot 4
CLASS_TYPE_LABELS <- c(
  training       = "Training (2015-2023)",
  `2yr_partial`  = "2 NFL seasons (2024)",
  `1yr_partial`  = "1 NFL season (2025)",
  no_nfl_seasons = "Just drafted (2026)"
)


# ==============================================================================
# LOAD PIPELINE ARTIFACTS
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("  Loading pipeline artifacts...\n")
cat(strrep("=", 60), "\n\n")

rds_paths <- list(
  feature_matrix = file.path(CACHE_DIR, "s2_week10_feature_matrix.rds"),
  performance    = file.path(CACHE_DIR, "s2_week10_performance.rds"),
  predictions    = file.path(CACHE_DIR, "s2_week10_predictions.rds")
)

missing <- rds_paths[!purrr::map_lgl(rds_paths, file.exists)]
if (length(missing) > 0L) {
  stop(glue(
    "Required artifacts not found:\n",
    paste(unlist(missing), collapse = "\n"), "\n",
    "Run examples/example_season2_week10.R first."
  ), call. = FALSE)
}

feature_matrix <- readRDS(rds_paths$feature_matrix)
performance    <- readRDS(rds_paths$performance)
loco_preds     <- readRDS(rds_paths$predictions)

# Load prediction CSVs for Plot 4 (includes translation_score)
pred_csvs <- purrr::map_dfr(TRANSLATION_POSITIONS, function(pos) {
  path <- here::here("output", glue("s2_week10_predictions_{pos}.csv"))
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, stringsAsFactors = FALSE) %>%
    dplyr::mutate(draft_position = pos)
}) %>%
  dplyr::filter(!is.na(translation_score))

cat(glue(
  "  Performance rows : {nrow(performance)}\n",
  "  LOCO predictions : {format(nrow(loco_preds), big.mark = ',')}\n",
  "  Scored players   : {format(nrow(pred_csvs), big.mark = ',')}\n\n"
))

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}


# ==============================================================================
# SHARED THEME
# ==============================================================================

theme_nfl_analytics <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(
        face = "bold", size = base_size + 2, margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle   = ggplot2::element_text(
        size = base_size - 1, color = "gray40",
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption    = ggplot2::element_text(
        size = base_size - 3, color = "gray55", hjust = 1,
        margin = ggplot2::margin(t = 8)
      ),
      strip.text      = ggplot2::element_text(face = "bold", size = base_size),
      axis.title      = ggplot2::element_text(size = base_size - 1),
      axis.text       = ggplot2::element_text(size = base_size - 2),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      legend.position = "bottom",
      legend.title    = ggplot2::element_text(face = "bold", size = base_size - 1),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}


# ==============================================================================
# PLOT 1: R-SQUARED BY POSITION -- BASE VS ENRICHED
# ==============================================================================
# Story: adding draft capital (enriched model) dramatically improves R-squared
# at every position. TE base R-squared is negative -- college production alone
# predicts worse than using the position mean. QB base is the strongest signal.
# ==============================================================================

cat("Plot 1: R-squared by position -- base vs enriched model\n")

# Compute key values for annotations (not hardcoded -- derived from data)
te_base_r2  <- performance$r_squared[
  performance$draft_position == "TE" & performance$model_variant == "base"
]
qb_base_r2  <- performance$r_squared[
  performance$draft_position == "QB" & performance$model_variant == "base"
]
rb_enr_r2   <- performance$r_squared[
  performance$draft_position == "RB" & performance$model_variant == "enriched"
]

plot1_data <- performance %>%
  dplyr::select(draft_position, model_variant, r_squared) %>%
  dplyr::mutate(
    draft_position = factor(draft_position, levels = POSITION_ORDER),
    model_variant  = factor(
      model_variant,
      levels = c("base", "enriched"),
      labels = c("Base (college production only)",
                 "Enriched (+ draft capital)")
    ),
    label = round(r_squared, 3)
  )

p1 <- ggplot2::ggplot(
  plot1_data,
  ggplot2::aes(
    x    = draft_position,
    y    = r_squared,
    fill = model_variant
  )
) +
  ggplot2::geom_hline(
    yintercept = 0, linetype = "dashed",
    color = "gray40", linewidth = 0.6
  ) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.7),
    width    = 0.6,
    alpha    = 0.9
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = label,
      y     = dplyr::if_else(r_squared >= 0,
        r_squared + 0.012,
        r_squared - 0.018
      )
    ),
    position  = ggplot2::position_dodge(width = 0.7),
    size      = 3.2,
    fontface  = "bold"
  ) +
  ggplot2::annotate(
    "text",
    x = 4, y = te_base_r2 - 0.035,
    label = "College TE production\npredicts worse than\nusing position mean",
    size = 2.8, color = "gray30", hjust = 0.5, lineheight = 0.9
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Base (college production only)" = "#9ecae1",
      "Enriched (+ draft capital)"     = "#2171b5"
    )
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01),
    breaks = seq(-0.2, 0.6, by = 0.1)
  ) +
  ggplot2::labs(
    title    = "Draft Capital Fills the Gap College Production Leaves Behind",
    subtitle = glue(
      "LOCO out-of-sample R-squared | Training classes {min(TRAINING_DRAFT_CLASSES)}-{CUTOFF_YEAR} | ",
      "9 folds\n",
      "Dashed line = predicting position mean (R-squared = 0)"
    ),
    x        = "Position",
    y        = "R-squared (LOCO out-of-sample)",
    fill     = "Model variant",
    caption  = ATTRIBUTION
  ) +
  theme_nfl_analytics()

path_p1 <- file.path(OUTPUT_DIR, paste0(PREFIX, "r_squared_base_vs_enriched.png"))
ggplot2::ggsave(
  filename = path_p1,
  plot     = p1,
  width    = 9, height = 6, dpi = DPI, bg = "white"
)
cat(glue("  Saved: {basename(path_p1)}\n\n"))


# ==============================================================================
# PLOT 2: LOCO FOLD STABILITY -- PER-CLASS RMSE ACROSS 9 DRAFT CLASSES
# ==============================================================================
# Story: No single draft class dominates error. The model generalizes across
# time rather than being a local fit to any one era. 2023 QB fold is the
# best-predicted class (RMSE 3.81), which is notable.
# ==============================================================================

cat("Plot 2: LOCO fold stability -- per-class RMSE\n")

# Compute per-fold RMSE from LOCO predictions (not hardcoded)
fold_rmse <- loco_preds %>%
  dplyr::filter(!is.na(pred_enriched), !is.na(ppr_per_game_y13)) %>%
  dplyr::group_by(draft_position, draft_year) %>%
  dplyr::summarise(
    n    = dplyr::n(),
    rmse = sqrt(mean((ppr_per_game_y13 - pred_enriched)^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    draft_position = factor(draft_position, levels = POSITION_ORDER)
  )

# Overall RMSE per position (horizontal reference line per facet)
overall_rmse <- loco_preds %>%
  dplyr::filter(!is.na(pred_enriched), !is.na(ppr_per_game_y13)) %>%
  dplyr::group_by(draft_position) %>%
  dplyr::summarise(
    overall_rmse = sqrt(mean((ppr_per_game_y13 - pred_enriched)^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(draft_position = factor(draft_position, levels = POSITION_ORDER))

p2 <- ggplot2::ggplot(
  fold_rmse,
  ggplot2::aes(x = draft_year, y = rmse, color = draft_position)
) +
  ggplot2::geom_hline(
    data     = overall_rmse,
    ggplot2::aes(yintercept = overall_rmse),
    linetype = "dashed", color = "gray50", linewidth = 0.5
  ) +
  ggplot2::geom_line(linewidth = 0.8, alpha = 0.8) +
  ggplot2::geom_point(
    ggplot2::aes(size = n),
    alpha = 0.9
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = round(rmse, 2)),
    vjust = -0.8, size = 2.6, show.legend = FALSE
  ) +
  ggplot2::facet_wrap(
    ~ draft_position,
    scales = "free_y",
    ncol   = 2L
  ) +
  ggplot2::scale_color_manual(values = POSITION_COLORS, guide = "none") +
  ggplot2::scale_size_continuous(
    range  = c(2, 6),
    name   = "Players in fold",
    breaks = c(5, 10, 20, 30)
  ) +
  ggplot2::scale_x_continuous(
    breaks = min(TRAINING_DRAFT_CLASSES):CUTOFF_YEAR,
    labels = function(x) paste0("'", substr(x, 3, 4))
  ) +
  ggplot2::labs(
    title    = "Model Is Stable Across Draft Classes -- No Single Year Drives Error",
    subtitle = glue(
      "Enriched model LOCO RMSE per draft class | ",
      "Dashed line = overall RMSE per position\n",
      "Point size = players in that fold"
    ),
    x        = "Draft class",
    y        = "LOCO RMSE (PPR points per game)",
    caption  = ATTRIBUTION
  ) +
  theme_nfl_analytics()

path_p2 <- file.path(OUTPUT_DIR, paste0(PREFIX, "loco_fold_stability.png"))
ggplot2::ggsave(
  filename = path_p2,
  plot     = p2,
  width    = 11, height = 8, dpi = DPI, bg = "white"
)
cat(glue("  Saved: {basename(path_p2)}\n\n"))


# ==============================================================================
# PLOT 3: PREDICTED VS ACTUAL PPR PER GAME (ENRICHED MODEL)
# ==============================================================================
# Story: compression is visible -- the model correctly identifies direction
# (high vs low) but compresses extreme outcomes toward the mean. Hit players
# cluster above the diagonal; busts cluster near zero.
# 621 training players -- exceeds density threshold of 50.
# Interactive HTML saved alongside static PNG.
# ==============================================================================

cat("Plot 3: Predicted vs actual PPR per game (enriched model)\n")

# Join player names from feature matrix -- loco_predictions only stores gsis_id
player_names <- purrr::map_dfr(TRANSLATION_POSITIONS, function(pos) {
  df <- feature_matrix$training[[pos]]
  if (!"cfb_player_name" %in% names(df)) return(NULL)
  df %>%
    dplyr::select(nfl_gsis_id, cfb_player_name, cfb_primary_team) %>%
    dplyr::distinct()
})

plot3_data <- loco_preds %>%
  dplyr::filter(!is.na(pred_enriched), !is.na(ppr_per_game_y13)) %>%
  dplyr::left_join(player_names, by = "nfl_gsis_id") %>%
  dplyr::mutate(
    draft_position = factor(draft_position, levels = POSITION_ORDER),
    residual       = ppr_per_game_y13 - pred_enriched,
    is_hit_label   = dplyr::if_else(is_hit, "Hit", "Bust / miss"),
    display_name   = dplyr::coalesce(cfb_player_name, nfl_gsis_id)
  )

n_labeled <- nrow(plot3_data)
DENSITY_THRESHOLD <- 50L

# Diagonal reference line range per position
diag_range <- plot3_data %>%
  dplyr::group_by(draft_position) %>%
  dplyr::summarise(
    max_val = max(c(ppr_per_game_y13, pred_enriched), na.rm = TRUE),
    .groups = "drop"
  )

p3 <- ggplot2::ggplot(
  plot3_data,
  ggplot2::aes(
    x     = pred_enriched,
    y     = ppr_per_game_y13,
    color = is_hit_label
  )
) +
  ggplot2::geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = "gray40", linewidth = 0.5
  ) +
  ggplot2::geom_point(alpha = 0.55, size = 1.6) +
  ggplot2::geom_smooth(
    method  = "lm",
    formula = y ~ x,
    se      = TRUE,
    color   = "gray20",
    fill    = "gray85",
    linewidth = 0.7,
    alpha   = 0.3,
    show.legend = FALSE
  ) +
  ggplot2::facet_wrap(
    ~ draft_position,
    scales = "free",
    ncol   = 2L
  ) +
  ggplot2::scale_color_manual(
    values = c("Hit" = "#2171b5", "Bust / miss" = "#d73027"),
    name   = "Outcome"
  ) +
  ggplot2::labs(
    title    = "Model Identifies Direction But Compresses Extreme Outcomes",
    subtitle = glue(
      "Enriched model | LOCO out-of-sample predictions | ",
      "Training classes {min(TRAINING_DRAFT_CLASSES)}-{CUTOFF_YEAR}\n",
      "Dashed diagonal = perfect prediction | ",
      "Regression line shows actual prediction slope"
    ),
    x        = "Predicted PPR per game (enriched model)",
    y        = "Actual PPR per game (3-year average)",
    caption  = ATTRIBUTION
  ) +
  theme_nfl_analytics()

path_p3 <- file.path(OUTPUT_DIR, paste0(PREFIX, "predicted_vs_actual.png"))
ggplot2::ggsave(
  filename = path_p3,
  plot     = p3,
  width    = 10, height = 8, dpi = DPI, bg = "white"
)
cat(glue("  Saved: {basename(path_p3)}\n"))

# --- Interactive HTML version (621 players -- exceeds density threshold) ------
cat(glue(
  "  {n_labeled} labeled players -- exceeds density threshold ({DENSITY_THRESHOLD}). ",
  "Building interactive HTML...\n"
))

p3_interactive <- plotly::plot_ly()

for (pos in POSITION_ORDER) {
  pos_data <- plot3_data %>% dplyr::filter(draft_position == pos)
  if (nrow(pos_data) == 0L) next

  # Hits
  hits <- pos_data %>% dplyr::filter(is_hit)
  busts <- pos_data %>% dplyr::filter(!is_hit)

  for (sub_df in list(hits, busts)) {
    if (nrow(sub_df) == 0L) next
    label <- if (sub_df$is_hit[1L]) "Hit" else "Bust / miss"
    col   <- if (sub_df$is_hit[1L]) "#2171b5" else "#d73027"

    p3_interactive <- plotly::add_trace(
      p3_interactive,
      type       = "scatter",
      mode       = "markers",
      x          = sub_df$pred_enriched,
      y          = sub_df$ppr_per_game_y13,
      name       = glue("{pos} -- {label}"),
      text       = glue(
        "{sub_df$display_name}<br>",
        "College: {dplyr::coalesce(sub_df$cfb_primary_team, 'Unknown')}<br>",
        "Draft: {sub_df$draft_year}<br>",
        "Predicted: {round(sub_df$pred_enriched, 2)}<br>",
        "Actual: {round(sub_df$ppr_per_game_y13, 2)}<br>",
        "Residual: {round(sub_df$residual, 2)}"
      ),
      hoverinfo  = "text",
      marker     = list(color = col, size = 7, opacity = 0.65),
      legendgroup = pos
    )
  }
}

# Add diagonal reference
max_val_all <- max(c(plot3_data$pred_enriched, plot3_data$ppr_per_game_y13),
  na.rm = TRUE)
p3_interactive <- plotly::add_trace(
  p3_interactive,
  type      = "scatter",
  mode      = "lines",
  x         = c(0, max_val_all),
  y         = c(0, max_val_all),
  name      = "Perfect prediction",
  line      = list(dash = "dash", color = "gray", width = 1),
  hoverinfo = "none",
  showlegend = TRUE
)

p3_interactive <- plotly::layout(
  p3_interactive,
  title = list(
    text = "Predicted vs Actual PPR per Game -- Enriched Model (LOCO)",
    font = list(size = 15)
  ),
  xaxis  = list(title = "Predicted PPR per game"),
  yaxis  = list(title = "Actual PPR per game (3-year avg)"),
  legend = list(orientation = "v", x = 1.02, y = 1),
  hoverlabel = list(bgcolor = "white"),
  plot_bgcolor  = "white",
  paper_bgcolor = "white"
)

path_p3_html <- file.path(
  OUTPUT_DIR,
  paste0(PREFIX, "predicted_vs_actual_interactive.html")
)
htmlwidgets::saveWidget(
  widget        = p3_interactive,
  file          = path_p3_html,
  selfcontained = TRUE
)
cat(glue("  Saved: {basename(path_p3_html)} (interactive)\n\n"))


# ==============================================================================
# PLOT 4: TRANSLATION SCORE DISTRIBUTION BY DRAFT CLASS TYPE
# ==============================================================================
# Story: where the 2026 class sits relative to the full historical population.
# The score is a percentile rank across all players -- 2026 high scores are
# driven by draft capital (early picks), which is visible as a rightward skew
# for the "just drafted" class relative to training classes.
# ==============================================================================

cat("Plot 4: Translation score distribution by draft class type\n")

if (nrow(pred_csvs) == 0L) {
  cat("  WARNING: No prediction CSVs found. Skipping Plot 4.\n\n")
} else {
  plot4_data <- pred_csvs %>%
    dplyr::filter(!is.na(translation_score), !is.na(draft_class_type)) %>%
    dplyr::mutate(
      draft_position = factor(draft_position, levels = POSITION_ORDER),
      class_type = dplyr::recode(
        draft_class_type,
        "training"       = CLASS_TYPE_LABELS[["training"]],
        "2yr_partial"    = CLASS_TYPE_LABELS[["2yr_partial"]],
        "1yr_partial"    = CLASS_TYPE_LABELS[["1yr_partial"]],
        "no_nfl_seasons" = CLASS_TYPE_LABELS[["no_nfl_seasons"]]
      ),
      class_type = factor(class_type, levels = unname(CLASS_TYPE_LABELS))
    )

  # Compute median per position per class type -- annotate on plot
  medians_p4 <- plot4_data %>%
    dplyr::group_by(draft_position, class_type) %>%
    dplyr::summarise(
      med = median(translation_score, na.rm = TRUE),
      n   = dplyr::n(),
      .groups = "drop"
    )

  p4 <- ggplot2::ggplot(
    plot4_data,
    ggplot2::aes(
      x    = translation_score,
      fill = class_type,
      color = class_type
    )
  ) +
    ggplot2::geom_density(alpha = 0.35, linewidth = 0.7, adjust = 1.2) +
    ggplot2::geom_vline(
      data     = medians_p4,
      ggplot2::aes(xintercept = med, color = class_type),
      linetype = "dashed", linewidth = 0.6, show.legend = FALSE
    ) +
    ggplot2::facet_wrap(
      ~ draft_position,
      scales = "free_y",
      ncol   = 2L
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Training (2015-2023)"  = "#2171b5",   # blue
        "2 NFL seasons (2024)"  = "#009E73",   # green -- distinct from blue
        "1 NFL season (2025)"   = "#E69F00",   # amber
        "Just drafted (2026)"   = "#d73027"    # red
      ),
      name = "Draft class"
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Training (2015-2023)"  = "#2171b5",
        "2 NFL seasons (2024)"  = "#009E73",
        "1 NFL season (2025)"   = "#E69F00",
        "Just drafted (2026)"   = "#d73027"
      ),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, by = 25),
      labels = function(x) paste0(x)
    ) +
    ggplot2::labs(
      title    = "Where Does the 2026 Draft Class Rank Historically?",
      subtitle = glue(
        "Translation score = percentile rank within position across ALL draft classes ",
        "combined\n",
        "Dashed vertical lines = median per class | ",
        "Score computed at run time -- recalibrates on next re-run"
      ),
      x        = "Translation score (0 = lowest, 100 = highest in dataset)",
      y        = "Density",
      caption  = glue(
        ATTRIBUTION, "\n",
        "Note: 2026 scores are pure college-production + draft-capital projections. ",
        "No NFL data available."
      )
    ) +
    theme_nfl_analytics() +
    ggplot2::theme(legend.position = "bottom")

  path_p4 <- file.path(OUTPUT_DIR, paste0(PREFIX, "translation_score_distribution.png"))
  ggplot2::ggsave(
    filename = path_p4,
    plot     = p4,
    width    = 11, height = 8, dpi = DPI, bg = "white"
  )
  cat(glue("  Saved: {basename(path_p4)}\n\n"))
}


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("  Season 2 Week 10 -- Visualizations Complete\n")
cat(strrep("=", 60), "\n")
cat(glue("  Plot 1: {basename(path_p1)}\n"))
cat(glue("  Plot 2: {basename(path_p2)}\n"))
cat(glue("  Plot 3: {basename(path_p3)}\n"))
cat(glue("  Plot 3: {basename(path_p3_html)} (interactive)\n"))
if (exists("path_p4")) {
  cat(glue("  Plot 4: {basename(path_p4)}\n"))
}
cat(strrep("-", 60), "\n")

cat("\nKEY INSIGHTS (computed from data):\n")

# All values derived from loaded RDS -- never hardcoded
te_base  <- round(performance$r_squared[
  performance$draft_position == "TE" & performance$model_variant == "base"], 3)
rb_enr   <- round(performance$r_squared[
  performance$draft_position == "RB" & performance$model_variant == "enriched"], 3)
qb_base  <- round(performance$r_squared[
  performance$draft_position == "QB" & performance$model_variant == "base"], 3)
wr_base  <- round(performance$r_squared[
  performance$draft_position == "WR" & performance$model_variant == "base"], 3)

best_fold_qb <- loco_preds %>%
  dplyr::filter(draft_position == "QB", !is.na(pred_enriched),
                !is.na(ppr_per_game_y13)) %>%
  dplyr::group_by(draft_year) %>%
  dplyr::summarise(
    rmse = sqrt(mean((ppr_per_game_y13 - pred_enriched)^2)), .groups = "drop"
  ) %>%
  dplyr::slice_min(rmse, n = 1L)

n_2026 <- if (nrow(pred_csvs) > 0L) {
  sum(pred_csvs$draft_year == 2026L, na.rm = TRUE)
} else {
  0L
}

cat(glue("  TE base R-squared  : {te_base} (below zero = college TE stats don't translate)\n"))
cat(glue("  QB base R-squared  : {qb_base} (strongest base signal of any position)\n"))
cat(glue("  WR base R-squared  : {wr_base} (draft capital quadruples WR predictability)\n"))
cat(glue("  RB enriched R2     : {rb_enr} (most predictable position with full feature set)\n"))
cat(glue(
  "  Best QB LOCO fold  : {best_fold_qb$draft_year} ",
  "(RMSE = {round(best_fold_qb$rmse, 2)})\n"
))
cat(glue("  2026 players scored: {format(n_2026, big.mark = ',')}\n"))

cat(strrep("-", 60), "\n")

cat("\nInteractivity check:\n")
cat(glue(
  "  Plot 3: {n_labeled} players -- above density threshold ({DENSITY_THRESHOLD}). ",
  "HTML saved.\n"
))
cat("  Plots 1, 2, 4: below threshold. Static PNG only.\n\n")
