# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Visualization Script: R/29 Projection Engine
# File: examples/create_s2_week15_r29_visuals.R
#
# PURPOSE
# -------
# Four publication-quality visualizations derived from R/29 output:
#
#   V1: Prior weight decay schedule (Week 1-18 computed from constants)
#   V2: ADP value scatter -- model projected rank vs FantasyPros ADP rank
#   V3: Confidence interval width by NFL experience tier and position
#   V4: Prior adjustment ladder -- pre-adjustment vs post-adjustment prior
#
# DEPENDENCIES
# ------------
#   data/season2_cache/s2_week15_player_projections.csv  (R/29 output)
#
# OUTPUTS (all to output/plots/)
# --------------------------------
#   s2_week15_r29_prior_weight_decay.png
#   s2_week15_r29_adp_value_scatter.png
#   s2_week15_r29_adp_value_scatter.html   (interactive, plotly -- if >= 50 labeled)
#   s2_week15_r29_ci_width_by_experience.png
#   s2_week15_r29_adjustment_ladder.png
#
# SCHEMA TAG: s2_w15_v1
# ==============================================================================

# ==============================================================================
# LIBRARIES
# ==============================================================================

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggrepel)
library(scales)
library(glue)
library(here)
library(readr)

if (!requireNamespace("plotly", quietly = TRUE)) {
  stop("Package 'plotly' is required. Install with: install.packages('plotly')")
}
if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
  stop("Package 'htmlwidgets' is required. Install with: install.packages('htmlwidgets')")
}
library(plotly)
library(htmlwidgets)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

SEASON      <- 2026L
WEEK        <- 15L
OUTPUT_DIR  <- here::here("output", "plots")
DATA_DIR    <- here::here("data", "season2_cache")
PLOT_DPI    <- 300L
FILE_PREFIX <- "s2_week15_r29"

# Prior weight schedule -- must match R/29 constants exactly
PRIOR_WEIGHT_START          <- 1.0
PRIOR_WEIGHT_FLOOR          <- 0.05
NFL_REGULAR_SEASON_MAX_WEEK <- 18L

# V4: number of top players to display in adjustment ladder
TOP_N_LADDER <- 30L

# Density threshold: add interactive HTML when labeled points >= this value
DENSITY_THRESHOLD <- 50L

# Consistent position ordering and colors across all four plots
POSITION_ORDER  <- c("QB", "RB", "WR", "TE")
POSITION_COLORS <- c(QB = "#E63946", RB = "#457B9D", WR = "#2A9D8F", TE = "#E9C46A")

# ==============================================================================
# SETUP
# ==============================================================================

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat(glue("Created output directory: {OUTPUT_DIR}\n\n"))
}

cat(paste0(strrep("=", 70), "\n"))
cat(glue("R/29 PROJECTION ENGINE -- VISUALIZATION SCRIPT\n"))
cat(glue("Season {SEASON} | Week {WEEK}\n"))
cat(paste0(strrep("=", 70), "\n\n"))

# ==============================================================================
# DATA LOAD
# ==============================================================================

projections_path <- file.path(DATA_DIR, "s2_week15_player_projections.csv")

if (!file.exists(projections_path)) {
  stop(glue(
    "Required input not found: {projections_path}\n",
    "Run R/29_projection_engine.R first to generate this file."
  ))
}

proj <- readr::read_csv(projections_path, show_col_types = FALSE)

cat(glue("Loaded: s2_week15_player_projections.csv ",
         "({format(nrow(proj), big.mark = ',')} rows)\n\n"))

# Validate required columns
required_cols <- c(
  "player_name", "position", "team",
  "n_nfl_seasons",
  "prior_mu_pre_age", "aging_delta_ppg", "prior_mu",
  "posterior_mu", "posterior_sigma",
  "projection_lower_80", "projection_upper_80",
  "projection_lower_95", "projection_upper_95",
  "boom_probability", "bust_probability",
  "adp_rank", "adp_ecr", "projected_rank", "value_score",
  "consensus_proj", "consensus_delta",
  "ytd_n", "ytd_mean",
  "score_final"
)

missing_cols <- setdiff(required_cols, names(proj))
if (length(missing_cols) > 0) {
  stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
}

cat(glue("Column validation: all {length(required_cols)} required columns present\n\n"))

# Derived columns used across multiple plots
proj <- proj %>%
  dplyr::mutate(
    position    = factor(position, levels = POSITION_ORDER),
    ci_width_80 = projection_upper_80 - projection_lower_80,
    ci_width_95 = projection_upper_95 - projection_lower_95,
    net_adj     = prior_mu - prior_mu_pre_age
  )

# ==============================================================================
# HELPER: compute_prior_weight
# Mirrors R/29 exactly -- linear decay from PRIOR_WEIGHT_START at week 1
# to PRIOR_WEIGHT_FLOOR at week NFL_REGULAR_SEASON_MAX_WEEK.
# ==============================================================================

compute_prior_weight <- function(week) {
  slope <- (PRIOR_WEIGHT_START - PRIOR_WEIGHT_FLOOR) /
    (NFL_REGULAR_SEASON_MAX_WEEK - 1L)
  raw <- PRIOR_WEIGHT_START - slope * (week - 1L)
  pmax(PRIOR_WEIGHT_FLOOR, raw)
}

# ==============================================================================
# VISUALIZATION 1: Prior Weight Decay Schedule
# ==============================================================================
# Computed entirely from constants -- no player data required.
# Shows prior-vs-observed crossover with current week annotated.

cat("Building V1: Prior weight decay schedule...\n")

decay_df <- tibble::tibble(
  week = 1L:NFL_REGULAR_SEASON_MAX_WEEK,
  prior_weight = compute_prior_weight(1L:NFL_REGULAR_SEASON_MAX_WEEK),
  obs_weight   = 1 - compute_prior_weight(1L:NFL_REGULAR_SEASON_MAX_WEEK)
) %>%
  tidyr::pivot_longer(
    cols      = c(prior_weight, obs_weight),
    names_to  = "signal",
    values_to = "weight"
  ) %>%
  dplyr::mutate(
    signal = dplyr::recode(
      signal,
      prior_weight = "Prior (dynasty + translation model)",
      obs_weight   = "Observed (season-to-date PPR/game)"
    )
  )

# Crossover: week where prior weight is closest to 0.5
crossover_week <- tibble::tibble(
  week         = 1L:NFL_REGULAR_SEASON_MAX_WEEK,
  prior_weight = compute_prior_weight(1L:NFL_REGULAR_SEASON_MAX_WEEK)
) %>%
  dplyr::filter(abs(prior_weight - 0.5) == min(abs(prior_weight - 0.5))) %>%
  dplyr::pull(week) %>%
  dplyr::first()

current_prior <- round(compute_prior_weight(WEEK), 3)

p1 <- ggplot2::ggplot(
  decay_df,
  ggplot2::aes(x = week, y = weight, color = signal, linetype = signal)
) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::geom_vline(
    xintercept = crossover_week,
    linetype   = "dashed",
    color      = "gray55",
    linewidth  = 0.7
  ) +
  ggplot2::annotate(
    "text",
    x     = crossover_week + 0.35,
    y     = 0.57,
    label = glue("Crossover\nWeek {crossover_week}"),
    hjust = 0,
    size  = 3.4,
    color = "gray40"
  ) +
  ggplot2::annotate(
    "point",
    x     = WEEK,
    y     = current_prior,
    size  = 4.5,
    color = "#E63946",
    shape = 16
  ) +
  ggplot2::annotate(
    "text",
    x     = WEEK + 0.35,
    y     = current_prior + 0.04,
    label = glue("Now (Wk {WEEK})\n{current_prior} prior weight"),
    hjust = 0,
    size  = 3.2,
    color = "#E63946"
  ) +
  ggplot2::scale_x_continuous(
    name   = "NFL Week",
    breaks = c(1, 3, 6, 9, 12, 15, 18)
  ) +
  ggplot2::scale_y_continuous(
    name   = "Weight in Posterior Projection",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Prior (dynasty + translation model)" = "#457B9D",
      "Observed (season-to-date PPR/game)"  = "#2A9D8F"
    )
  ) +
  ggplot2::scale_linetype_manual(
    values = c(
      "Prior (dynasty + translation model)" = "solid",
      "Observed (season-to-date PPR/game)"  = "solid"
    )
  ) +
  ggplot2::labs(
    title    = "Prior Weight Decays Linearly -- Observed Data Takes Over After Week 9",
    subtitle = glue(
      "Week {WEEK} prior weight: {current_prior} | ",
      "Floor = {PRIOR_WEIGHT_FLOOR} (small regularization pull retained through Week 18)"
    ),
    color    = NULL,
    linetype = NULL,
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | R/29 Projection Engine"
    )
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 14),
    plot.subtitle    = ggplot2::element_text(size = 10, color = "#555555"),
    plot.caption     = ggplot2::element_text(size = 8, color = "#888888"),
    legend.position  = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )

v1_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_prior_weight_decay.png"))
ggplot2::ggsave(v1_path, p1, width = 10, height = 6, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_prior_weight_decay.png\n\n"))

# ==============================================================================
# VISUALIZATION 2: ADP Value Scatter
# ==============================================================================
# Model projected rank vs FantasyPros ADP rank.
# Reference line y = x: above = model more pessimistic, below = model more
# optimistic (undervalued by ADP).
# value_score = adp_rank - projected_rank: positive = model ranks higher.

cat("Building V2: ADP value scatter...\n")

adp_data <- proj %>%
  dplyr::filter(!is.na(adp_rank), !is.na(projected_rank)) %>%
  dplyr::mutate(
    # Label top 5 buy signals and top 5 sell signals per position
    buy_rank  = dplyr::dense_rank(dplyr::desc(value_score)),
    sell_rank = dplyr::dense_rank(value_score),
    label_player = buy_rank <= 5 | sell_rank <= 5
  )

n_labeled_v2 <- sum(adp_data$label_player, na.rm = TRUE)
cat(glue(
  "  ADP-matched players: {nrow(adp_data)} | ",
  "Labeled: {n_labeled_v2}\n"
))

# Axis range: same scale on both axes so y=x diagonal is meaningful
rank_max <- max(c(adp_data$adp_rank, adp_data$projected_rank), na.rm = TRUE)

p2 <- ggplot2::ggplot(
  adp_data,
  ggplot2::aes(x = adp_rank, y = projected_rank, color = position)
) +
  ggplot2::geom_abline(
    slope     = 1,
    intercept = 0,
    linetype  = "dashed",
    color     = "gray55",
    linewidth = 0.8
  ) +
  ggplot2::annotate(
    "text",
    x = rank_max * 0.82, y = rank_max * 0.72,
    label    = "Market agrees\nwith model",
    size     = 3.0,
    color    = "gray50",
    fontface = "italic"
  ) +
  ggplot2::annotate(
    "text",
    x = rank_max * 0.75, y = rank_max * 0.28,
    label    = "Model ranks higher\nthan ADP (buy signal)",
    size     = 3.0,
    color    = "#2A9D8F",
    fontface = "italic"
  ) +
  ggplot2::annotate(
    "text",
    x = rank_max * 0.28, y = rank_max * 0.75,
    label    = "ADP ranks higher\nthan model (sell signal)",
    size     = 3.0,
    color    = "#E63946",
    fontface = "italic"
  ) +
  ggplot2::geom_point(alpha = 0.65, size = 2.0) +
  ggrepel::geom_text_repel(
    data         = dplyr::filter(adp_data, label_player),
    mapping      = ggplot2::aes(label = glue("{player_name} ({team})")),
    size         = 2.6,
    max.overlaps = 25,
    segment.color = "gray65",
    segment.size  = 0.3,
    min.segment.length = 0.2
  ) +
  ggplot2::scale_x_continuous(
    name   = "ADP Rank (FantasyPros Consensus -- higher = worse market rank)",
    limits = c(1, rank_max),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  ggplot2::scale_y_continuous(
    name   = "Model Projected Rank (higher = worse model rank)",
    limits = c(1, rank_max),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  ggplot2::scale_color_manual(
    values = POSITION_COLORS,
    name   = "Position"
  ) +
  ggplot2::labs(
    title    = "Where Does the Model Disagree with the Fantasy Market?",
    subtitle = paste0(
      "Points below diagonal = model ranks player higher than ADP (positive value_score)\n",
      "Top 5 buy and sell signals per position labeled"
    ),
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | R/29 Projection Engine\n",
      "value_score = adp_rank - projected_rank | ADP source: FantasyPros consensus"
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle    = ggplot2::element_text(size = 9, color = "#555555"),
    plot.caption     = ggplot2::element_text(size = 8, color = "#888888"),
    legend.position  = "right",
    panel.grid.minor = ggplot2::element_blank()
  )

v2_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_adp_value_scatter.png"))
ggplot2::ggsave(v2_path, p2, width = 11, height = 10, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_adp_value_scatter.png\n"))

# Interactive HTML -- triggered when labeled points >= DENSITY_THRESHOLD
if (n_labeled_v2 >= DENSITY_THRESHOLD) {
  cat(glue(
    "  {n_labeled_v2} labeled points >= threshold ({DENSITY_THRESHOLD}) ",
    "-- building interactive HTML\n"
  ))

  p2_interactive <- plotly::plot_ly(
    data      = adp_data,
    x         = ~adp_rank,
    y         = ~projected_rank,
    color     = ~position,
    colors    = POSITION_COLORS,
    type      = "scatter",
    mode      = "markers",
    text      = ~glue(
      "{player_name} ({team})\n",
      "Position: {position}\n",
      "Projected rank: {projected_rank} | ADP rank: {adp_rank}\n",
      "value_score: {value_score}\n",
      "Posterior: {round(posterior_mu, 1)} PPR/gm\n",
      "Dynasty score: {score_final}"
    ),
    hoverinfo = "text",
    marker    = list(size = 8, opacity = 0.7)
  ) %>%
    plotly::add_trace(
      x        = c(1, rank_max),
      y        = c(1, rank_max),
      type     = "scatter",
      mode     = "lines",
      name     = "y = x (market agrees)",
      line     = list(color = "gray", dash = "dash", width = 1),
      showlegend = TRUE,
      hoverinfo  = "skip",
      inherit    = FALSE
    ) %>%
    plotly::layout(
      title  = list(text = "Model vs Market: Projected Rank vs ADP Rank"),
      xaxis  = list(
        title      = "ADP Rank (FantasyPros)",
        autorange  = FALSE,
        range      = c(1, rank_max)
      ),
      yaxis  = list(
        title      = "Model Projected Rank",
        autorange  = FALSE,
        range      = c(1, rank_max)
      ),
      legend = list(title = list(text = "Position"))
    )

  v2_html_path <- file.path(
    OUTPUT_DIR,
    glue("{FILE_PREFIX}_adp_value_scatter.html")
  )
  htmlwidgets::saveWidget(
    widget        = p2_interactive,
    file          = v2_html_path,
    selfcontained = TRUE
  )
  cat(glue(
    "  Saved: {FILE_PREFIX}_adp_value_scatter.html (interactive)\n\n"
  ))
} else {
  cat(glue(
    "  {n_labeled_v2} labeled points < threshold ({DENSITY_THRESHOLD}) ",
    "-- skipping interactive HTML\n\n"
  ))
}

# ==============================================================================
# VISUALIZATION 3: Confidence Interval Width by NFL Experience
# ==============================================================================
# Demonstrates precision weighting in action: wider intervals for rookies,
# narrower for veterans. Faceted by position to show positional differences.

cat("Building V3: CI width by NFL experience...\n")

ci_data <- proj %>%
  dplyr::filter(!is.na(ci_width_80), !is.na(n_nfl_seasons)) %>%
  dplyr::mutate(
    experience_group = dplyr::case_when(
      n_nfl_seasons == 0L ~ "Rookie\n(0 seasons)",
      n_nfl_seasons == 1L ~ "2nd Year\n(1 season)",
      n_nfl_seasons == 2L ~ "3rd Year\n(2 seasons)",
      n_nfl_seasons <= 5L ~ "Mid-Career\n(3-5 seasons)",
      TRUE                ~ "Veteran\n(6+ seasons)"
    ),
    experience_group = factor(
      experience_group,
      levels = c(
        "Rookie\n(0 seasons)",
        "2nd Year\n(1 season)",
        "3rd Year\n(2 seasons)",
        "Mid-Career\n(3-5 seasons)",
        "Veteran\n(6+ seasons)"
      )
    )
  )

# Counts per group for subtitle
group_counts <- ci_data %>%
  dplyr::count(experience_group) %>%
  dplyr::summarise(
    summary = paste(
      glue("{experience_group}: {n}"),
      collapse = " | "
    )
  ) %>%
  dplyr::pull(summary)

p3 <- ggplot2::ggplot(
  ci_data,
  ggplot2::aes(x = experience_group, y = ci_width_80, fill = position)
) +
  ggplot2::geom_boxplot(
    outlier.shape = 16,
    outlier.size  = 1.5,
    outlier.alpha = 0.45,
    alpha         = 0.75,
    linewidth     = 0.5
  ) +
  ggplot2::scale_fill_manual(values = POSITION_COLORS) +
  ggplot2::facet_wrap(~ position, nrow = 1) +
  ggplot2::labs(
    title    = "Uncertainty Narrows as Players Accumulate NFL Experience",
    subtitle = glue(
      "80% CI width (PPR pts/game) | Wider band = less certain projection\n",
      "{group_counts}"
    ),
    x        = "NFL Experience Tier",
    y        = "80% Confidence Interval Width (PPR pts/game)",
    fill     = "Position",
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | R/29 Projection Engine\n",
      "CI width = projection_upper_80 - projection_lower_80 | Precision weighting in action"
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle    = ggplot2::element_text(size = 9, color = "#555555"),
    plot.caption     = ggplot2::element_text(size = 8, color = "#888888"),
    legend.position  = "none",
    strip.text       = ggplot2::element_text(face = "bold", size = 11),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x      = ggplot2::element_text(size = 8)
  )

v3_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_ci_width_by_experience.png"))
ggplot2::ggsave(v3_path, p3, width = 14, height = 6, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_ci_width_by_experience.png\n\n"))

# ==============================================================================
# VISUALIZATION 4: Prior Adjustment Ladder
# ==============================================================================
# Arrow chart: prior_mu_pre_age (before adjustments) to prior_mu (after aging,
# injury, ramp adjustments) for the top TOP_N_LADDER players by posterior_mu.
# Shows the net effect of the full adjustment stack per player.

cat("Building V4: Prior adjustment ladder...\n")

ladder_data <- proj %>%
  dplyr::filter(!is.na(prior_mu_pre_age), !is.na(prior_mu)) %>%
  dplyr::slice_max(order_by = posterior_mu, n = TOP_N_LADDER) %>%
  dplyr::mutate(
    adj_direction = dplyr::if_else(net_adj >= 0, "Upward", "Downward"),
    player_label  = glue("{player_name} ({position}, {team})")
  ) %>%
  dplyr::arrange(posterior_mu) %>%
  dplyr::mutate(
    player_label = factor(player_label, levels = unique(player_label))
  )

n_up   <- sum(ladder_data$adj_direction == "Upward")
n_down <- sum(ladder_data$adj_direction == "Downward")

p4 <- ggplot2::ggplot(ladder_data) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x     = prior_mu_pre_age,
      xend  = prior_mu,
      y     = player_label,
      yend  = player_label,
      color = adj_direction
    ),
    linewidth = 1.1,
    arrow = ggplot2::arrow(
      length = ggplot2::unit(0.10, "inches"),
      type   = "closed"
    )
  ) +
  ggplot2::geom_point(
    ggplot2::aes(x = prior_mu_pre_age, y = player_label),
    color = "gray45",
    size  = 2.2,
    shape = 16
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype   = "dashed",
    color      = "gray70",
    linewidth  = 0.5
  ) +
  ggplot2::scale_color_manual(
    name   = "Net adjustment direction",
    values = c(Upward = "#2A9D8F", Downward = "#E63946")
  ) +
  ggplot2::scale_x_continuous(
    name   = "PPR Points per Game",
    breaks = scales::pretty_breaks(n = 6)
  ) +
  ggplot2::labs(
    title    = glue(
      "Prior Adjustment Stack: Pre-Adjustment vs Post-Adjustment ",
      "(Top {TOP_N_LADDER} by Projection)"
    ),
    subtitle = glue(
      "Gray dot = prior_mu_pre_age (before aging + injury + ramp adjustments)\n",
      "Arrow endpoint = prior_mu (after full adjustment stack) | ",
      "Upward: {n_up} | Downward: {n_down}"
    ),
    y        = NULL,
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | R/29 Projection Engine\n",
      "net_adj = prior_mu - prior_mu_pre_age | Sorted by posterior_mu descending (bottom = highest)"
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 12),
    plot.subtitle    = ggplot2::element_text(size = 9, color = "#555555"),
    plot.caption     = ggplot2::element_text(size = 8, color = "#888888"),
    legend.position  = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_text(size = 8),
    plot.margin      = ggplot2::margin(t = 8, r = 15, b = 8, l = 8)
  )

v4_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_adjustment_ladder.png"))
ggplot2::ggsave(v4_path, p4, width = 11, height = 12, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_adjustment_ladder.png\n\n"))

# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

html_note <- if (n_labeled_v2 >= DENSITY_THRESHOLD) " + .html (interactive)" else ""

cat(paste0(strrep("=", 70), "\n"))
cat("R/29 VISUALIZATION SUMMARY\n")
cat(paste0(strrep("=", 70), "\n"))
cat(glue("  Season: {SEASON} | Week: {WEEK}\n"))
cat(glue("  Players in projection output:  {format(nrow(proj), big.mark = ',')}\n"))
cat(glue("  Players with ADP match:        {nrow(adp_data)}\n"))
cat(glue("  Prior weight at Week {WEEK}:     {current_prior}\n"))
cat(glue("  Crossover week:                {crossover_week}\n"))
cat("\n  Plots generated: 4\n")
cat(glue("  1. {FILE_PREFIX}_prior_weight_decay.png\n"))
cat(glue("  2. {FILE_PREFIX}_adp_value_scatter.png{html_note}\n"))
cat(glue("  3. {FILE_PREFIX}_ci_width_by_experience.png\n"))
cat(glue("  4. {FILE_PREFIX}_adjustment_ladder.png\n"))
cat(glue("  Output: output/plots/\n"))
cat(glue("  Resolution: {PLOT_DPI} dpi\n"))
cat(paste0(strrep("=", 70), "\n"))

# ==============================================================================
# END OF FILE
# ==============================================================================
