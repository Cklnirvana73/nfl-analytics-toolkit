# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 15
# Visualization Script: R/32 Projection Reconciliation
# File: examples/create_s2_week15_r32_visuals.R
#
# PURPOSE
# -------
# Three publication-quality visualizations derived from R/32 output:
#
#   V1: R/29 vs R/32 posterior scatter -- where did team constraints move players?
#       (static PNG + interactive HTML if labeled points >= 50)
#
#   V2: Biggest movers -- top 15 up and top 15 down by r32_delta_from_r29
#       (two-panel horizontal lollipop)
#
#   V3: Correction magnitude by prior source tier
#       (box plot of r32_delta_from_r29 by blend_weight bucket)
#
# DEPENDENCIES
# ------------
#   data/season2_cache/s2_week15_reconciled_projections.csv  (R/32 output)
#
# OUTPUTS (all to output/plots/)
# --------------------------------
#   s2_week15_r32_r29_vs_r32_scatter.png
#   s2_week15_r32_r29_vs_r32_scatter.html  (interactive, if >= 50 labeled pts)
#   s2_week15_r32_biggest_movers.png
#   s2_week15_r32_correction_by_prior_source.png
#
# SCHEMA TAG: s2_w15_reconciled_v1
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
  stop("Package 'plotly' required. Install with: install.packages('plotly')")
}
if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
  stop("Package 'htmlwidgets' required. Install with: install.packages('htmlwidgets')")
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
FILE_PREFIX <- "s2_week15_r32"

# Biggest movers: N per direction
TOP_N_MOVERS <- 15L

# Density threshold for interactive HTML
DENSITY_THRESHOLD <- 50L

# Label players in V1 when |delta| exceeds this or r32_posterior_mu exceeds this
LABEL_DELTA_THRESHOLD  <- 3.0
LABEL_MU_THRESHOLD     <- 15.0

# Consistent position ordering and colors
POSITION_ORDER  <- c("QB", "RB", "WR", "TE")
POSITION_COLORS <- c(QB = "#E63946", RB = "#457B9D", WR = "#2A9D8F", TE = "#E9C46A")

# Prior source factor ordering: most trusted to least trusted (by blend weight)
PRIOR_SOURCE_ORDER <- c(
  "blended",
  "veteran_history_only",
  "history_only_fallback",
  "translation_only",
  "score_final_fallback"
)

PRIOR_SOURCE_LABELS <- c(
  "blended"               = "blended\n(30% R/31)",
  "veteran_history_only"  = "veteran_history_only\n(45% R/31)",
  "history_only_fallback" = "history_only_fallback\n(55% R/31)",
  "translation_only"      = "translation_only\n(65% R/31)",
  "score_final_fallback"  = "score_final_fallback\n(85% R/31)"
)

# ==============================================================================
# SETUP
# ==============================================================================

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat(glue("Created output directory: {OUTPUT_DIR}\n\n"))
}

cat(paste0(strrep("=", 70), "\n"))
cat(glue("R/32 PROJECTION RECONCILIATION -- VISUALIZATION SCRIPT\n"))
cat(glue("Season {SEASON} | Week {WEEK}\n"))
cat(paste0(strrep("=", 70), "\n\n"))

# ==============================================================================
# DATA LOAD
# ==============================================================================

recon_path <- file.path(DATA_DIR, "s2_week15_reconciled_projections.csv")

if (!file.exists(recon_path)) {
  stop(glue(
    "Required input not found: {recon_path}\n",
    "Run R/32_projection_reconciliation.R first to generate this file."
  ))
}

recon <- readr::read_csv(recon_path, show_col_types = FALSE)

cat(glue(
  "Loaded: s2_week15_reconciled_projections.csv ",
  "({format(nrow(recon), big.mark = ',')} rows)\n\n"
))

# Validate required columns
required_cols <- c(
  "player_name", "team", "position",
  "r29_posterior_mu", "r32_posterior_mu", "r32_delta_from_r29",
  "blend_weight_r31", "volume_implied_ppg_v2",
  "r31_expected_targets_pg", "r31_expected_carries_pg",
  "prior_source", "score_final", "n_nfl_seasons"
)

missing_cols <- setdiff(required_cols, names(recon))
if (length(missing_cols) > 0) {
  stop(glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
}

cat(glue("Column validation: all {length(required_cols)} required columns present\n"))

# Subset to non-QB for reconciliation plots (QBs pass through unchanged in v1)
recon_skill <- recon %>%
  dplyr::filter(position %in% c("RB", "WR", "TE")) %>%
  dplyr::mutate(
    position = factor(position, levels = c("RB", "WR", "TE"))
  )

recon_all <- recon %>%
  dplyr::mutate(
    position = factor(position, levels = POSITION_ORDER)
  )

n_total   <- nrow(recon)
n_skill   <- nrow(recon_skill)
n_qb      <- sum(recon$position == "QB", na.rm = TRUE)

cat(glue(
  "Total players: {n_total} | ",
  "RB/WR/TE reconciled: {n_skill} | ",
  "QB pass-through: {n_qb}\n\n"
))

# ==============================================================================
# VISUALIZATION 1: R/29 vs R/32 Posterior Scatter
# ==============================================================================
# The signature reconciliation chart. Every RB/WR/TE as one point.
# y = x diagonal = no change. Below = team constraint pulled them down.
# Above = team constraint boosted them. Color by position.

cat("Building V1: R/29 vs R/32 posterior scatter...\n")

v1_data <- recon_skill %>%
  dplyr::filter(
    !is.na(r29_posterior_mu),
    !is.na(r32_posterior_mu)
  ) %>%
  dplyr::mutate(
    label_player = (
      abs(r32_delta_from_r29) >= LABEL_DELTA_THRESHOLD |
        r32_posterior_mu >= LABEL_MU_THRESHOLD
    )
  )

n_labeled_v1 <- sum(v1_data$label_player, na.rm = TRUE)
cat(glue(
  "  Scatter: {nrow(v1_data)} players | ",
  "{n_labeled_v1} labeled (|delta| >= {LABEL_DELTA_THRESHOLD} ",
  "or posterior >= {LABEL_MU_THRESHOLD})\n"
))

# Axis limits: shared scale so y=x diagonal is visually correct
mu_max <- max(c(v1_data$r29_posterior_mu, v1_data$r32_posterior_mu),
              na.rm = TRUE) * 1.05
mu_min <- min(c(v1_data$r29_posterior_mu, v1_data$r32_posterior_mu),
              na.rm = TRUE)
mu_min <- max(0, mu_min - 0.5)

p1 <- ggplot2::ggplot(
  v1_data,
  ggplot2::aes(x = r29_posterior_mu, y = r32_posterior_mu, color = position)
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
    x = mu_max * 0.75, y = mu_max * 0.82,
    label    = "No change\n(R/32 = R/29)",
    size     = 3.0, color = "gray55", fontface = "italic"
  ) +
  ggplot2::annotate(
    "text",
    x = mu_max * 0.72, y = mu_max * 0.55,
    label    = "Team constraint\npulled DOWN",
    size     = 3.0, color = "#E63946", fontface = "italic"
  ) +
  ggplot2::annotate(
    "text",
    x = mu_max * 0.35, y = mu_max * 0.72,
    label    = "Team constraint\nboosted UP",
    size     = 3.0, color = "#2A9D8F", fontface = "italic"
  ) +
  ggplot2::geom_point(alpha = 0.65, size = 2.0) +
  ggrepel::geom_text_repel(
    data         = dplyr::filter(v1_data, label_player),
    mapping      = ggplot2::aes(label = glue("{player_name} ({team})")),
    size         = 2.5,
    max.overlaps = 30,
    segment.color = "gray65",
    segment.size  = 0.3,
    min.segment.length = 0.2,
    box.padding  = 0.3
  ) +
  ggplot2::scale_color_manual(
    values = POSITION_COLORS[c("RB", "WR", "TE")],
    name   = "Position"
  ) +
  ggplot2::scale_x_continuous(
    name   = "R/29 Posterior (isolated Bayesian prior, PPR pts/game)",
    limits = c(mu_min, mu_max),
    breaks = scales::pretty_breaks(n = 7)
  ) +
  ggplot2::scale_y_continuous(
    name   = "R/32 Posterior (team-constrained reconciled, PPR pts/game)",
    limits = c(mu_min, mu_max),
    breaks = scales::pretty_breaks(n = 7)
  ) +
  ggplot2::labs(
    title    = glue(
      "R/29 vs R/32: Where Did the Team Constraint Move Each Player?"
    ),
    subtitle = glue(
      "RB/WR/TE only ({n_skill} players) | ",
      "QB projections pass through unchanged in R/32 v1\n",
      "Labeled: |delta| >= {LABEL_DELTA_THRESHOLD} PPG ",
      "or r32 projection >= {LABEL_MU_THRESHOLD} PPG"
    ),
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | ",
      "R/32 Projection Reconciliation\n",
      "Blend: (1 - blend_weight_r31) * r29_posterior_mu + ",
      "blend_weight_r31 * volume_implied_ppg_v2"
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

v1_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_r29_vs_r32_scatter.png"))
ggplot2::ggsave(v1_path, p1, width = 11, height = 10, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_r29_vs_r32_scatter.png\n"))

# Interactive HTML: always build for this plot -- 500+ points, hover adds value
# regardless of label count on static version
cat(glue(
  "  Building interactive HTML (500+ point scatter, hover adds value)\n"
))

p1_interactive <- plotly::plot_ly(
  data      = v1_data,
  x         = ~r29_posterior_mu,
  y         = ~r32_posterior_mu,
  color     = ~position,
  colors    = POSITION_COLORS[c("RB", "WR", "TE")],
  type      = "scatter",
  mode      = "markers",
  text      = ~glue(
    "{player_name} ({team})\n",
    "Position: {position}\n",
    "R/29 posterior: {round(r29_posterior_mu, 2)} PPG\n",
    "R/32 posterior: {round(r32_posterior_mu, 2)} PPG\n",
    "Delta: {round(r32_delta_from_r29, 2)} PPG\n",
    "Blend weight (R/31): {round(blend_weight_r31, 2)}\n",
    "Prior source: {prior_source}"
  ),
  hoverinfo = "text",
  marker    = list(size = 7, opacity = 0.7)
) %>%
  plotly::add_trace(
    x        = c(mu_min, mu_max),
    y        = c(mu_min, mu_max),
    type     = "scatter",
    mode     = "lines",
    name     = "No change (y = x)",
    line     = list(color = "gray", dash = "dash", width = 1),
    showlegend = TRUE,
    hoverinfo  = "skip",
    inherit    = FALSE
  ) %>%
  plotly::layout(
    title  = list(text = "R/29 vs R/32: Team Constraint Effect"),
    xaxis  = list(
      title = "R/29 Posterior (PPR pts/game)",
      range = c(mu_min, mu_max)
    ),
    yaxis  = list(
      title = "R/32 Posterior (PPR pts/game)",
      range = c(mu_min, mu_max)
    ),
    legend = list(title = list(text = "Position"))
  )

v1_html_path <- file.path(
  OUTPUT_DIR,
  glue("{FILE_PREFIX}_r29_vs_r32_scatter.html")
)
htmlwidgets::saveWidget(
  widget        = p1_interactive,
  file          = v1_html_path,
  selfcontained = TRUE
)
cat(glue("  Saved: {FILE_PREFIX}_r29_vs_r32_scatter.html (interactive)\n\n"))

# ==============================================================================
# VISUALIZATION 2: Biggest Movers -- r32_delta_from_r29
# ==============================================================================
# Two-panel lollipop. Left panel: top TOP_N_MOVERS downward movers.
# Right panel: top TOP_N_MOVERS upward movers. Shows who the team
# constraint corrected most dramatically in each direction.

cat("Building V2: Biggest movers...\n")

movers_data <- recon_skill %>%
  dplyr::filter(!is.na(r32_delta_from_r29)) %>%
  dplyr::mutate(
    player_label = glue("{player_name} ({position}, {team})")
  )

top_down <- movers_data %>%
  dplyr::slice_min(order_by = r32_delta_from_r29, n = TOP_N_MOVERS) %>%
  dplyr::arrange(r32_delta_from_r29) %>%
  dplyr::mutate(
    player_label = factor(player_label, levels = unique(player_label)),
    direction    = "Pulled Down by Team Constraint"
  )

top_up <- movers_data %>%
  dplyr::slice_max(order_by = r32_delta_from_r29, n = TOP_N_MOVERS) %>%
  dplyr::arrange(r32_delta_from_r29) %>%
  dplyr::mutate(
    player_label = factor(player_label, levels = unique(player_label)),
    direction    = "Boosted by Team Constraint"
  )

cat(glue(
  "  Biggest down: {top_down$player_label[1]} ",
  "({round(min(top_down$r32_delta_from_r29), 2)} PPG)\n"
))
cat(glue(
  "  Biggest up:   {top_up$player_label[nrow(top_up)]} ",
  "(+{round(max(top_up$r32_delta_from_r29), 2)} PPG)\n"
))

v2_data <- dplyr::bind_rows(top_down, top_up) %>%
  dplyr::mutate(
    direction = factor(
      direction,
      levels = c("Pulled Down by Team Constraint", "Boosted by Team Constraint")
    )
  )

p2 <- ggplot2::ggplot(
  v2_data,
  ggplot2::aes(x = r32_delta_from_r29, y = player_label, color = position)
) +
  ggplot2::geom_vline(
    xintercept = 0,
    color      = "gray55",
    linewidth  = 0.7,
    linetype   = "solid"
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = 0, xend = r32_delta_from_r29,
                 y = player_label, yend = player_label),
    linewidth = 0.9,
    alpha     = 0.75
  ) +
  ggplot2::geom_point(size = 3.0) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = glue("{round(r32_delta_from_r29, 1)}"),
      hjust = if_else(r32_delta_from_r29 >= 0, -0.4, 1.4)
    ),
    size  = 2.6,
    color = "gray30"
  ) +
  ggplot2::scale_color_manual(
    values = POSITION_COLORS[c("RB", "WR", "TE")],
    name   = "Position"
  ) +
  ggplot2::scale_x_continuous(
    name   = "r32_delta_from_r29 (PPR pts/game)",
    breaks = scales::pretty_breaks(n = 6)
  ) +
  ggplot2::facet_wrap(
    ~ direction,
    scales = "free",
    nrow   = 1L
  ) +
  ggplot2::labs(
    title    = glue(
      "Biggest Movers: Who Did the Team Constraint Correct Most? ",
      "(Top {TOP_N_MOVERS} Each Direction)"
    ),
    subtitle = paste0(
      "r32_delta_from_r29 = r32_posterior_mu - r29_posterior_mu\n",
      "Negative = R/32 lower than R/29 | Positive = R/32 higher than R/29"
    ),
    y        = NULL,
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | ",
      "R/32 Projection Reconciliation\n",
      "QB excluded (pass-through in v1) | ",
      "Delta driven by blend weight and gap between R/29 and volume-implied PPG"
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title         = ggplot2::element_text(face = "bold", size = 12),
    plot.subtitle      = ggplot2::element_text(size = 9, color = "#555555"),
    plot.caption       = ggplot2::element_text(size = 8, color = "#888888"),
    legend.position    = "bottom",
    strip.text         = ggplot2::element_text(face = "bold", size = 10),
    panel.grid.minor   = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    axis.text.y        = ggplot2::element_text(size = 8)
  )

v2_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_biggest_movers.png"))
ggplot2::ggsave(v2_path, p2, width = 14, height = 10, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_biggest_movers.png\n\n"))

# ==============================================================================
# VISUALIZATION 3: Correction Magnitude by Prior Source Tier
# ==============================================================================
# Box plot of r32_delta_from_r29 grouped by prior_source, ordered from most
# trusted (blended = 30% R/31 weight) to least (score_final_fallback = 85%).
# Tests whether higher blend weight -> bigger corrections, as expected.

cat("Building V3: Correction magnitude by prior source tier...\n")

v3_data <- recon_skill %>%
  dplyr::filter(
    !is.na(r32_delta_from_r29),
    !is.na(prior_source),
    prior_source %in% PRIOR_SOURCE_ORDER
  ) %>%
  dplyr::mutate(
    prior_source_f = factor(
      prior_source,
      levels = PRIOR_SOURCE_ORDER,
      labels = PRIOR_SOURCE_LABELS[PRIOR_SOURCE_ORDER]
    )
  )

# Count per tier for subtitle
tier_counts <- v3_data %>%
  dplyr::count(prior_source_f) %>%
  dplyr::summarise(
    txt = paste(glue("{prior_source_f}: {n}"), collapse = " | ")
  ) %>%
  dplyr::pull(txt)

# Median delta per tier for annotation
tier_medians <- v3_data %>%
  dplyr::group_by(prior_source_f) %>%
  dplyr::summarise(
    median_delta = median(r32_delta_from_r29, na.rm = TRUE),
    n            = dplyr::n(),
    .groups      = "drop"
  )

cat(glue("  Prior source tiers present: {n_distinct(v3_data$prior_source_f)}\n"))

p3 <- ggplot2::ggplot(
  v3_data,
  ggplot2::aes(x = prior_source_f, y = r32_delta_from_r29)
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color      = "gray55",
    linewidth  = 0.8,
    linetype   = "dashed"
  ) +
  ggplot2::geom_boxplot(
    fill          = "#457B9D",
    color         = "gray35",
    alpha         = 0.65,
    outlier.shape = 16,
    outlier.size  = 1.5,
    outlier.alpha = 0.45,
    linewidth     = 0.5,
    width         = 0.55
  ) +
  ggplot2::geom_text(
    data    = tier_medians,
    mapping = ggplot2::aes(
      x     = prior_source_f,
      y     = median_delta,
      label = glue("n={n}\nmed={round(median_delta, 2)}")
    ),
    inherit.aes = FALSE,
    hjust  = -0.65,
    size   = 2.8,
    color  = "gray30",
    lineheight = 0.9
  ) +
  ggplot2::scale_x_discrete(name = NULL) +
  ggplot2::scale_y_continuous(
    name   = "r32_delta_from_r29 (PPR pts/game)",
    breaks = scales::pretty_breaks(n = 7)
  ) +
  ggplot2::labs(
    title    = glue(
      "Stronger Team Constraint = Bigger Correction: ",
      "Delta by Prior Source Trust Tier"
    ),
    subtitle = glue(
      "Left = most trusted R/29 prior (30% R/31 weight) -> ",
      "Right = least trusted (85% R/31 weight)\n",
      "{tier_counts}"
    ),
    caption  = glue(
      "NFL Analytics Toolkit | Season {SEASON} Week {WEEK} | ",
      "R/32 Projection Reconciliation\n",
      "QB excluded | Blend weight = how much R/31 volume-implied PPG ",
      "overrides R/29 posterior"
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 13),
    plot.subtitle    = ggplot2::element_text(size = 9, color = "#555555"),
    plot.caption     = ggplot2::element_text(size = 8, color = "#888888"),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x      = ggplot2::element_text(size = 9, lineheight = 0.9),
    plot.margin      = ggplot2::margin(t = 8, r = 50, b = 8, l = 8)
  )

v3_path <- file.path(OUTPUT_DIR, glue("{FILE_PREFIX}_correction_by_prior_source.png"))
ggplot2::ggsave(v3_path, p3, width = 12, height = 7, dpi = PLOT_DPI)
cat(glue("  Saved: {FILE_PREFIX}_correction_by_prior_source.png\n\n"))

# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

biggest_down_row <- recon_skill %>%
  dplyr::filter(!is.na(r32_delta_from_r29)) %>%
  dplyr::slice_min(r32_delta_from_r29, n = 1L, with_ties = FALSE)

biggest_up_row <- recon_skill %>%
  dplyr::filter(!is.na(r32_delta_from_r29)) %>%
  dplyr::slice_max(r32_delta_from_r29, n = 1L, with_ties = FALSE)

cat(paste0(strrep("=", 70), "\n"))
cat("R/32 VISUALIZATION SUMMARY\n")
cat(paste0(strrep("=", 70), "\n"))
cat(glue("  Season: {SEASON} | Week: {WEEK}\n"))
cat(glue("  Total players in output:   {n_total}\n"))
cat(glue("  RB/WR/TE reconciled:       {n_skill}\n"))
cat(glue("  QB pass-through:           {n_qb}\n"))
cat(glue(
  "  Biggest downward mover:    ",
  "{biggest_down_row$player_name} ({biggest_down_row$team}) ",
  "{round(biggest_down_row$r32_delta_from_r29, 2)} PPG\n"
))
cat(glue(
  "  Biggest upward mover:      ",
  "{biggest_up_row$player_name} ({biggest_up_row$team}) ",
  "+{round(biggest_up_row$r32_delta_from_r29, 2)} PPG\n"
))
cat("\n  Plots generated:\n")
cat(glue("  1. {FILE_PREFIX}_r29_vs_r32_scatter.png + .html (interactive)\n"))
cat(glue("  2. {FILE_PREFIX}_biggest_movers.png\n"))
cat(glue("  3. {FILE_PREFIX}_correction_by_prior_source.png\n"))
cat(glue("  Output: output/plots/\n"))
cat(glue("  Resolution: {PLOT_DPI} dpi\n"))
cat(paste0(strrep("=", 70), "\n"))

# ==============================================================================
# END OF FILE
# ==============================================================================
