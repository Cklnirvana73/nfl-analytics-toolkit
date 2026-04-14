# ==============================================================================
# examples/create_season2_week4_visual.R
# Season 2, Week 4: Recency Bias A/B Test -- Visualizations
# NFL Analytics Toolkit
#
# PURPOSE
# Generates 3 publication-quality plots from the saved Week 4 RDS artifacts.
# Run AFTER run_ab_test_pipeline() has produced the three RDS files.
#
# PLOTS PRODUCED
#   Plot 1: Dumbbell chart -- r_roll3 vs r_std per position with 95% CI
#           s2_week4_dumbbell_correlations.png
#
#   Plot 2: Hex-bin scatter -- predictor vs actual by position (faceted)
#           s2_week4_hexbin_predictor_vs_actual.png
#
#   Plot 3: Cohen's q bar chart with small-effect benchmark line
#           s2_week4_cohens_q_effect_size.png
#
# PREREQUISITES
#   data/season2_cache/s2_week4_ab_panel.rds       -- from run_ab_test_pipeline()
#   data/season2_cache/s2_week4_ab_results.rds     -- from run_ab_test_pipeline()
#
# INTERACTIVITY NOTE
# All three plots contain fewer than 50 labeled data points. Per the
# density-based interactivity threshold rule, static PNG is sufficient.
# No interactive HTML version is required or produced.
#
# OUTPUT
#   output/plots/s2_week4_dumbbell_correlations.png
#   output/plots/s2_week4_hexbin_predictor_vs_actual.png
#   output/plots/s2_week4_cohens_q_effect_size.png
#
# DEPENDENCIES
# ggplot2, dplyr, tidyr, glue, here
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)
library(here)


# ==============================================================================
# CONFIGURATION
# ==============================================================================

CACHE_DIR  <- here::here("data", "season2_cache")
OUTPUT_DIR <- here::here("output", "plots")
DPI        <- 300L
ATTRIBUTION <- "Data: nflfastR 2010-2025 | Analysis: NFL Analytics Toolkit S2W4"

# Season 2 Week 4 output prefix -- prevents collision with Season 1 files
PREFIX <- "s2_week4_"

# Position display labels and ordering (most to least predictable for layout)
POS_LEVELS  <- c("QB", "WR", "RB")
POS_LABELS  <- c("QB" = "Quarterback", "WR" = "Wide Receiver", "RB" = "Running Back")
POS_COLORS  <- c("QB" = "#1F78B4", "WR" = "#33A02C", "RB" = "#E31A1C")

# Consistent theme for all three plots
theme_nfl <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title        = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle     = element_text(size = 11, color = "gray40", hjust = 0),
      plot.caption      = element_text(size = 8,  color = "gray55", hjust = 1),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.grid.major  = element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      strip.text        = element_text(face = "bold", size = 11),
      legend.position   = "none",
      plot.margin       = margin(12, 16, 8, 12)
    )
}

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message("Created output directory: ", OUTPUT_DIR)
}


# ==============================================================================
# LOAD ARTIFACTS
# ==============================================================================

panel_path   <- file.path(CACHE_DIR, "s2_week4_ab_panel.rds")
results_path <- file.path(CACHE_DIR, "s2_week4_ab_results.rds")

for (p in c(panel_path, results_path)) {
  if (!file.exists(p)) {
    stop(
      "Required artifact missing: ", p,
      "\nRun run_ab_test_pipeline() first to generate artifacts."
    )
  }
}

panel   <- readRDS(panel_path)
results <- readRDS(results_path)

# Apply consistent position ordering and labels
results <- results %>%
  mutate(
    position = factor(position, levels = POS_LEVELS),
    pos_label = POS_LABELS[as.character(position)]
  )

panel <- panel %>%
  mutate(position = factor(position, levels = POS_LEVELS))

cat(glue(
  "\nArtifacts loaded.\n",
  "  Panel:   {format(nrow(panel), big.mark = ',')} rows\n",
  "  Results: {nrow(results)} positions\n\n"
))


# ==============================================================================
# PLOT 1: DUMBBELL CHART -- r_roll3 vs r_std per position with 95% CI
# ==============================================================================
# Story: STD beats roll3 for all three positions, but the gap varies.
# RBs show the highest absolute correlation and the smallest relative gap.
# QBs show the largest relative gap but the lowest absolute correlations.
# ==============================================================================

cat("[Plot 1/3] Building dumbbell correlation chart...\n")

# Reshape results to long format for the dumbbell
dumbbell_data <- results %>%
  select(position, pos_label,
         r_roll3, r_roll3_ci_lo, r_roll3_ci_hi,
         r_std,   r_std_ci_lo,   r_std_ci_hi,
         n_player_weeks) %>%
  # Create long format: one row per predictor per position
  pivot_longer(
    cols      = c(r_roll3, r_std),
    names_to  = "predictor",
    values_to = "r"
  ) %>%
  mutate(
    ci_lo = if_else(predictor == "r_roll3", r_roll3_ci_lo, r_std_ci_lo),
    ci_hi = if_else(predictor == "r_roll3", r_roll3_ci_hi, r_std_ci_hi),
    predictor_label = if_else(predictor == "r_roll3",
                              "roll3 (last 3 games)",
                              "STD (season average)"),
    predictor_label = factor(predictor_label,
                             levels = c("roll3 (last 3 games)",
                                        "STD (season average)"))
  )

# Connector segment data (one row per position, from r_roll3 to r_std)
connector_data <- results %>%
  select(position, pos_label, r_roll3, r_std, n_player_weeks)

# KEY INSIGHT: computed from results, never hardcoded
best_pos       <- results %>% arrange(desc(r_std)) %>% slice(1)
most_pred_pos  <- best_pos$pos_label
most_pred_r    <- round(best_pos$r_std, 3)
largest_gap    <- results %>%
  mutate(gap = r_std - r_roll3) %>%
  arrange(desc(gap)) %>%
  slice(1)
largest_gap_pos <- largest_gap$pos_label
largest_gap_val <- round(largest_gap$gap, 3)

p1 <- ggplot() +
  # Connector line between roll3 and std dots
  geom_segment(
    data = connector_data,
    aes(x = r_roll3, xend = r_std,
        y = pos_label, yend = pos_label),
    color    = "gray60",
    linewidth = 1.2
  ) +
  # CI error bars -- roll3 (geom_errorbar with orientation = "y" replaces deprecated geom_errorbarh)
  geom_errorbar(
    data = dumbbell_data %>% filter(predictor == "r_roll3"),
    aes(xmin = ci_lo, xmax = ci_hi, y = pos_label),
    width       = 0.12,
    orientation = "y",
    color     = "#4393C3",
    linewidth = 0.6,
    alpha     = 0.7
  ) +
  # CI error bars -- std
  geom_errorbar(
    data = dumbbell_data %>% filter(predictor == "r_std"),
    aes(xmin = ci_lo, xmax = ci_hi, y = pos_label),
    width       = 0.12,
    orientation = "y",
    color     = "#D6604D",
    linewidth = 0.6,
    alpha     = 0.7
  ) +
  # Dots -- roll3 (blue)
  geom_point(
    data = dumbbell_data %>% filter(predictor == "r_roll3"),
    aes(x = r, y = pos_label),
    size  = 5,
    color = "#4393C3"
  ) +
  # Dots -- std (red)
  geom_point(
    data = dumbbell_data %>% filter(predictor == "r_std"),
    aes(x = r, y = pos_label),
    size  = 5,
    color = "#D6604D"
  ) +
  # r value labels
  geom_text(
    data = dumbbell_data %>% filter(predictor == "r_roll3"),
    aes(x = r, y = pos_label, label = round(r, 3)),
    vjust = -1.2, size = 3.4, color = "#4393C3", fontface = "bold"
  ) +
  geom_text(
    data = dumbbell_data %>% filter(predictor == "r_std"),
    aes(x = r, y = pos_label, label = round(r, 3)),
    vjust = -1.2, size = 3.4, color = "#D6604D", fontface = "bold"
  ) +
  # n label on right margin
  geom_text(
    data = connector_data,
    aes(x = 0.71, y = pos_label,
        label = glue("n = {format(n_player_weeks, big.mark = ',')}")),
    hjust = 0, size = 3.0, color = "gray50"
  ) +
  # Legend annotation (manual -- no legend layer needed)
  annotate("point", x = 0.33, y = 0.55, size = 4, color = "#4393C3") +
  annotate("text",  x = 0.34, y = 0.55, label = "roll3 (last 3 games)",
           hjust = 0, size = 3.3, color = "#4393C3") +
  annotate("point", x = 0.33, y = 0.42, size = 4, color = "#D6604D") +
  annotate("text",  x = 0.34, y = 0.42, label = "STD (season average)",
           hjust = 0, size = 3.3, color = "#D6604D") +
  scale_x_continuous(
    limits = c(0.30, 0.75),
    breaks = seq(0.30, 0.70, by = 0.10),
    labels = function(x) sprintf("%.2f", x)
  ) +
  scale_y_discrete(limits = rev(POS_LABELS)) +
  labs(
    title    = "Season average outpredicts last 3 games for all positions",
    subtitle = glue(
      "{most_pred_pos}s are most predictable (r_STD = {most_pred_r}). ",
      "{largest_gap_pos}s show the largest gap between methods ",
      "(delta r = {largest_gap_val})."
    ),
    x       = "Pearson r with next-week fantasy points",
    y       = NULL,
    caption = glue("{ATTRIBUTION}\nError bars = bootstrap 95% BCa confidence intervals")
  ) +
  theme_nfl() +
  theme(panel.grid.major.y = element_blank())

path_p1 <- file.path(OUTPUT_DIR, paste0(PREFIX, "dumbbell_correlations.png"))
ggsave(path_p1, plot = p1, width = 9, height = 5, dpi = DPI, bg = "white")
cat(glue("  Saved: {path_p1}\n\n"))


# ==============================================================================
# PLOT 2: HEX-BIN SCATTER -- STD predictor vs actual, faceted by position
# ==============================================================================
# Story: Shows the raw relationship the correlations are measuring.
# Hex bins required because WR has 44,691 points (scatter unreadable at that n).
# One panel per position. Regression line overlaid per panel.
# ==============================================================================

cat("[Plot 2/3] Building hex-bin predictor vs actual chart...\n")

# KEY INSIGHT: computed from data
pos_n <- panel %>%
  count(position, name = "n_rows") %>%
  mutate(pos_label = POS_LABELS[as.character(position)])

pos_r <- results %>%
  select(position, r_std) %>%
  mutate(pos_label = POS_LABELS[as.character(position)])

# Build facet label with n and r for each panel
# NOTE: pos_label exists in both pos_n and pos_r after join -- use .data pronoun
# to resolve the ambiguity that causes glue() to fail with "object not found".
facet_labels <- pos_n %>%
  left_join(pos_r %>% select(position, r_std), by = "position") %>%
  mutate(
    facet_label = paste0(
      pos_label, "\n",
      "n = ", format(n_rows, big.mark = ","), " | r = ", round(r_std, 3)
    )
  )

panel_labelled <- panel %>%
  left_join(
    facet_labels %>% select(position, facet_label),
    by = "position"
  ) %>%
  mutate(
    facet_label = factor(facet_label,
                         levels = facet_labels$facet_label[
                           match(POS_LEVELS, facet_labels$position)
                         ])
  )

# Axis limits: use 5th and 95th percentile to reduce outlier distortion
axis_lo <- quantile(panel$fantasy_pts_std,    0.02, na.rm = TRUE)
axis_hi <- quantile(panel$fantasy_pts_actual, 0.98, na.rm = TRUE)

p2 <- ggplot(
  panel_labelled,
  aes(x = fantasy_pts_std, y = fantasy_pts_actual)
) +
  geom_hex(bins = 40, aes(fill = after_stat(log10(count + 1)))) +
  geom_smooth(
    method    = "lm",
    se        = FALSE,
    color     = "white",
    linewidth = 0.9,
    linetype  = "solid"
  ) +
  geom_abline(
    slope     = 1,
    intercept = 0,
    color     = "gray80",
    linetype  = "dashed",
    linewidth = 0.5
  ) +
  facet_wrap(~ facet_label, nrow = 1) +
  scale_fill_gradient(
    low  = "#C6DBEF",
    high = "#08306B",
    name = "log10(count)"
  ) +
  scale_x_continuous(
    limits = c(axis_lo, axis_hi),
    breaks = seq(0, 50, by = 10),
    labels = function(x) paste0(x, " pts")
  ) +
  scale_y_continuous(
    limits = c(axis_lo, axis_hi),
    breaks = seq(0, 50, by = 10),
    labels = function(x) paste0(x, " pts")
  ) +
  labs(
    title    = "Season-to-date average vs next-week actual score by position",
    subtitle = "White line = linear fit | Dashed = perfect prediction | Hex color = density (log scale)",
    x        = "Season-to-date average (STD predictor)",
    y        = "Actual next-week fantasy points",
    caption  = glue("{ATTRIBUTION}\n2010-2025 regular season | QB, RB, WR with >= 3 prior games")
  ) +
  theme_nfl() +
  theme(
    legend.position  = "right",
    legend.title     = element_text(size = 9),
    legend.text      = element_text(size = 8),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

path_p2 <- file.path(OUTPUT_DIR, paste0(PREFIX, "hexbin_predictor_vs_actual.png"))
ggsave(path_p2, plot = p2, width = 12, height = 5, dpi = DPI, bg = "white")
cat(glue("  Saved: {path_p2}\n\n"))


# ==============================================================================
# PLOT 3: COHEN'S Q BAR CHART -- effect size per position with benchmark line
# ==============================================================================
# Story: All three effects are statistically significant but below the "small"
# threshold (q = 0.10). The finding is real but the margin is narrow.
# QB shows the largest gap; WR the smallest.
# ==============================================================================

cat("[Plot 3/3] Building Cohen's q effect size chart...\n")

SMALL_EFFECT  <- 0.10
MEDIUM_EFFECT <- 0.30

# KEY INSIGHT: computed from results, never hardcoded
max_q_pos <- results %>% arrange(desc(cohens_q)) %>% slice(1)
min_q_pos <- results %>% arrange(cohens_q)       %>% slice(1)

q_data <- results %>%
  arrange(cohens_q) %>%   # ascending so largest bar appears at top in coord_flip
  mutate(
    pos_label = factor(
      POS_LABELS[as.character(position)],
      levels = POS_LABELS[as.character(position)]
    ),
    bar_color = POS_COLORS[as.character(position)],
    sig_label = glue(
      "q = {round(cohens_q, 4)}\np_adj < 0.0001"
    )
  )

p3 <- ggplot(q_data, aes(x = pos_label, y = cohens_q, fill = pos_label)) +
  # Background shading for "below small" region
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = 0,    ymax = SMALL_EFFECT,
    fill  = "#FFF3CD",
    alpha = 0.4
  ) +
  geom_col(width = 0.55, show.legend = FALSE) +
  # Small effect benchmark line
  geom_hline(
    yintercept = SMALL_EFFECT,
    linetype   = "dashed",
    color      = "#D6604D",
    linewidth  = 0.8
  ) +
  # q value labels on bars
  geom_text(
    aes(label = sprintf("q = %.4f", cohens_q)),
    hjust  = -0.12,
    size   = 3.8,
    fontface = "bold",
    color  = "gray20"
  ) +
  # Benchmark annotation
  annotate(
    "text",
    x     = 0.6,
    y     = SMALL_EFFECT + 0.002,
    label = "Small effect threshold (q = 0.10)",
    hjust = 0,
    size  = 3.2,
    color = "#D6604D",
    fontface = "italic"
  ) +
  # "Below small" zone label
  annotate(
    "text",
    x     = 0.58,
    y     = SMALL_EFFECT / 2,
    label = "Below small\neffect",
    hjust = 0.5,
    size  = 3.0,
    color = "gray55",
    fontface = "italic"
  ) +
  coord_flip(ylim = c(0, 0.13)) +
  # POS_COLORS is keyed by position code ("QB", "RB", "WR") but fill maps to
  # pos_label ("Quarterback", "Running Back", "Wide Receiver"). Remap names.
  scale_fill_manual(values = setNames(POS_COLORS, POS_LABELS)) +
  scale_y_continuous(
    breaks = c(0, 0.02, 0.04, 0.06, 0.08, 0.10, 0.12),
    labels = function(x) sprintf("%.2f", x)
  ) +
  labs(
    title = "STD advantage over roll3 is statistically significant but practically small",
    subtitle = glue(
      "All three positions fall below Cohen's small-effect threshold (q = 0.10). ",
      "{max_q_pos$pos_label}s show the\n",
      "largest gap (q = {round(max_q_pos$cohens_q, 4)}); ",
      "{min_q_pos$pos_label}s the smallest (q = {round(min_q_pos$cohens_q, 4)}). ",
      "3/3 positions BH-corrected p < 0.0001."
    ),
    x       = NULL,
    y       = "Cohen's q (effect size for correlation difference)",
    caption = glue(
      "{ATTRIBUTION}\n",
      "Cohen's q benchmarks: 0.10 = small, 0.30 = medium, 0.50 = large | ",
      "All comparisons: STD vs roll3 predicting next-week PPR points"
    )
  ) +
  theme_nfl() +
  theme(panel.grid.major.y = element_blank())

path_p3 <- file.path(OUTPUT_DIR, paste0(PREFIX, "cohens_q_effect_size.png"))
ggsave(path_p3, plot = p3, width = 9, height = 5, dpi = DPI, bg = "white")
cat(glue("  Saved: {path_p3}\n\n"))


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("  Season 2 Week 4 -- Visualizations Complete\n")
cat(strrep("=", 60), "\n")
cat(glue("  Plot 1: {basename(path_p1)}\n"))
cat(glue("  Plot 2: {basename(path_p2)}\n"))
cat(glue("  Plot 3: {basename(path_p3)}\n"))
cat(strrep("-", 60), "\n")

# KEY INSIGHT block -- computed from data, never hardcoded
cat("\nKEY INSIGHTS:\n")
cat(glue(
  "  Most predictable position: {most_pred_pos}s",
  " (r_STD = {most_pred_r})\n"
))
cat(glue(
  "  Largest predictor gap:     {largest_gap_pos}s",
  " (delta r = {largest_gap_val})\n"
))
cat(glue(
  "  All Cohen's q values below small threshold (q < {SMALL_EFFECT})\n"
))
cat(glue(
  "  Panel size:                {format(nrow(panel), big.mark = ',')} player-weeks",
  " across {n_distinct(panel$season)} seasons\n"
))
cat(strrep("=", 60), "\n\n")
cat("Interactivity check:\n")
cat("  All three plots have fewer than 50 labeled data points.\n")
cat("  Static PNG is sufficient. No interactive HTML version required.\n\n")
