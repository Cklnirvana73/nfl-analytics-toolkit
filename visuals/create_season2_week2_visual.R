# =============================================================================
# examples/create_season2_week2_visual.R
# =============================================================================
# Season 2, Week 2: Player-Season Panel -- Visualization Script
# NFL Analytics Toolkit | Production-grade for portfolio display
# =============================================================================
#
# PLOTS IN THIS FILE
# ------------------
#   Plot 1: QB Efficiency Scatter (2025)
#           EPA per dropback vs pass success rate for qualified starters.
#           Quadrant lines at league averages. Labels on all points.
#           Question answered: "Who were the most efficient QBs in 2025?"
#
#   Plot 2: Dual-Threat RB Lollipop (2025)
#           Rush EPA per attempt vs receiving EPA per target, side by side.
#           Sorted by combined contribution. Shows which RBs produce in both
#           phases -- the panel's core structural advantage.
#           Question answered: "Which RBs contributed in both phases in 2025?"
#
#   Plot 3: Total EPA Distribution by Position Group (2023-2025)
#           Violin + box hybrid showing the spread of season-level total EPA
#           across the three most recent seasons, by position group.
#           Question answered: "How does EPA production vary within and across
#           position groups over three seasons?"
#
# DENSITY THRESHOLD CHECK
# -----------------------
# All three plots have fewer than 50 labeled data points per the filters
# applied below. Static PNG only -- no interactive HTML required.
# If filters are relaxed and labeled points exceed 50, add an interactive
# HTML version per the density threshold rule in visualization-patterns.md.
#
# PREREQUISITES
# -------------
# Season 2 cache must be populated for 2023, 2024, and 2025.
# Run if needed:
#   source(here::here("R", "15_multi_season_pbp.R"))
#   load_multi_season_pbp(seasons = 2023:2025)
#
# Then source Week 2 functions:
#   source(here::here("R", "16_player_season_panel.R"))
#
# OUTPUT
# ------
# All plots saved to output/plots/ at 300 dpi.
# =============================================================================

library(nflfastR)
library(nflreadr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(here)
library(glue)
library(scales)

source(here::here("R", "16_player_season_panel.R"))

# Output directory
OUTPUT_DIR <- here::here("output", "plots")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# Shared theme -- defined once, applied to all plots
theme_analytics <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = rel(1.2), hjust = 0),
      plot.subtitle   = element_text(color = "gray40", size = rel(0.9), hjust = 0),
      plot.caption    = element_text(color = "gray60", size = rel(0.75), hjust = 1),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title      = element_text(color = "gray30", size = rel(0.9)),
      axis.text       = element_text(color = "gray40"),
      legend.position = "top",
      legend.title    = element_text(size = rel(0.85)),
      plot.margin     = margin(10, 15, 10, 10)
    )
}

CAPTION_STD <- "Data: nflfastR + nflreadr | Analysis: NFL Analytics Toolkit Season 2"


# =============================================================================
# DATA LOAD
# =============================================================================
# Load 2025 panel for plots 1 and 2. Load 2023-2025 panel for plot 3.
# Build separately to keep memory lean -- the 3-season panel is the superset.

message("\n--- Loading player-season panels ---")

panel_2025 <- build_player_season_panel(
  seasons   = 2025L,
  min_plays = 10L,
  verbose   = TRUE
)

panel_3yr <- build_player_season_panel(
  seasons   = 2023:2025,
  min_plays = 10L,
  verbose   = FALSE
)

message(glue(
  "2025 panel: {nrow(panel_2025)} player-seasons | ",
  "3-year panel: {nrow(panel_3yr)} player-seasons"
))


# =============================================================================
# PLOT 1: QB Efficiency Scatter -- 2025
# =============================================================================
# Scatter: two continuous variables, correct per chart-type selection framework.
# X-axis: pass success rate (proportion of pass attempts with EPA > 0)
# Y-axis: EPA per dropback (primary QB efficiency metric)
# Reference lines at approximate league averages.
# All qualified QBs labeled -- check point count before proceeding.
#
# NFL context:
#   League avg pass EPA per dropback: approximately +0.04 to +0.08
#   League avg pass success rate: approximately 0.45 (45%)
#   QBs above both lines = above average in both efficiency and consistency.
#   QBs below both lines = below average in both dimensions.
#   QBs in the top-left quadrant (high EPA, low success rate) = boom/bust.
#   QBs in the bottom-right (low EPA, high success rate) = consistent but limited.
#
# League average reference values. These are stable approximations.
# Exact values shift year to year by a few hundredths -- close enough for context.
LEAGUE_AVG_PASS_EPA_DB  <- 0.06
LEAGUE_AVG_PASS_SUCCESS <- 0.45

MIN_DROPBACKS_QB <- 200L  # Roughly a half-season starter minimum

message("\n--- Plot 1: QB Efficiency Scatter (2025) ---")

plot1_data <- panel_2025 %>%
  filter(
    position_group == "QB",
    !is.na(pass_epa_per_dropback),
    !is.na(pass_success_rate),
    qb_dropbacks >= MIN_DROPBACKS_QB
  ) %>%
  mutate(
    # Quadrant classification for color coding
    quadrant = case_when(
      pass_epa_per_dropback >= LEAGUE_AVG_PASS_EPA_DB &
        pass_success_rate >= LEAGUE_AVG_PASS_SUCCESS ~ "Above avg both",
      pass_epa_per_dropback >= LEAGUE_AVG_PASS_EPA_DB &
        pass_success_rate <  LEAGUE_AVG_PASS_SUCCESS ~ "High EPA, low success",
      pass_epa_per_dropback <  LEAGUE_AVG_PASS_EPA_DB &
        pass_success_rate >= LEAGUE_AVG_PASS_SUCCESS ~ "Low EPA, high success",
      TRUE                                            ~ "Below avg both"
    ),
    # Display label: name + dropback count
    label = glue("{player_name}\n({qb_dropbacks} db)")
  )

n_labeled_plot1 <- nrow(plot1_data)
message(glue("  Qualified QBs (>= {MIN_DROPBACKS_QB} dropbacks): {n_labeled_plot1}"))

# Density threshold check: static only if < 50 labeled points
if (n_labeled_plot1 >= 50L) {
  warning(glue(
    "Plot 1 has {n_labeled_plot1} labeled points (>= 50). ",
    "Add an interactive HTML version per the density threshold rule."
  ))
}

# Console summary
cat(glue("\nPlot 1 summary (2025 QBs, >= {MIN_DROPBACKS_QB} dropbacks):\n"))
cat(glue("  n = {n_labeled_plot1} qualified QBs\n"))
cat(glue(
  "  EPA/db range: [{round(min(plot1_data$pass_epa_per_dropback), 3)}, ",
  "{round(max(plot1_data$pass_epa_per_dropback), 3)}]\n"
))
cat(glue(
  "  Success rate range: [{round(min(plot1_data$pass_success_rate), 3)}, ",
  "{round(max(plot1_data$pass_success_rate), 3)}]\n"
))
cat(glue(
  "  Above avg both dimensions: ",
  "{sum(plot1_data$quadrant == 'Above avg both')}\n"
))

quadrant_colors <- c(
  "Above avg both"       = "#0072B2",
  "High EPA, low success" = "#56B4E9",
  "Low EPA, high success" = "#E69F00",
  "Below avg both"       = "#CC79A7"
)

p1 <- ggplot(plot1_data,
             aes(x = pass_success_rate, y = pass_epa_per_dropback,
                 color = quadrant)) +
  # Reference lines drawn first (behind points)
  geom_hline(yintercept = LEAGUE_AVG_PASS_EPA_DB,
             linetype = "dashed", color = "gray60", linewidth = 0.5) +
  geom_vline(xintercept = LEAGUE_AVG_PASS_SUCCESS,
             linetype = "dashed", color = "gray60", linewidth = 0.5) +
  # Quadrant labels (drawn before points so they sit behind)
  annotate("text", x = max(plot1_data$pass_success_rate) * 0.99,
           y = max(plot1_data$pass_epa_per_dropback) * 0.97,
           label = "Elite", color = "gray50", fontface = "italic",
           size = 3, hjust = 1) +
  annotate("text", x = min(plot1_data$pass_success_rate) * 1.01,
           y = min(plot1_data$pass_epa_per_dropback) * 0.97,
           label = "Struggling", color = "gray50", fontface = "italic",
           size = 3, hjust = 0) +
  # Points
  geom_point(size = 3, alpha = 0.85) +
  # Non-overlapping labels
  ggrepel::geom_text_repel(
    aes(label = player_name),
    size          = 2.8,
    max.overlaps  = 20L,
    segment.color = "gray70",
    segment.size  = 0.3,
    show.legend   = FALSE
  ) +
  scale_color_manual(values = quadrant_colors, name = NULL) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Pass Success Rate (EPA > 0 on pass attempts)") +
  scale_y_continuous(name = "EPA per Dropback") +
  labs(
    title    = "QB Efficiency: EPA per Dropback vs Pass Success Rate",
    subtitle = glue(
      "2025 Regular Season | >= {MIN_DROPBACKS_QB} dropbacks | ",
      "Dashed lines = league avg ({LEAGUE_AVG_PASS_EPA_DB} EPA/db, ",
      "{scales::percent(LEAGUE_AVG_PASS_SUCCESS, accuracy = 1)} success rate)"
    ),
    caption  = CAPTION_STD
  ) +
  theme_analytics()

ggsave(
  filename = file.path(OUTPUT_DIR, "week2_plot1_qb_efficiency_2025.png"),
  plot     = p1,
  width    = 10, height = 8, dpi = 300, bg = "white"
)
message("  Saved: week2_plot1_qb_efficiency_2025.png")


# =============================================================================
# PLOT 2: Dual-Threat RB Lollipop -- 2025
# =============================================================================
# Lollipop chart: ranking visualization per chart-type selection framework.
# Shows rush EPA per attempt and receiving EPA per target side by side,
# for RBs who clear minimum thresholds in both roles.
#
# Design: one row per RB, two segments extending left (rush, orange) and
# right (receiving, blue) from a center spine at zero. Sorted by combined EPA.
# This is only possible because both stats live on the same panel row.
#
# NFL context:
#   Rush EPA per attempt: league avg ~ 0.0 (neutral). Above 0 = above average.
#   Rec EPA per target: league avg ~ 0.0. Pass-catching RBs above avg here
#     are disproportionately valuable in PPR fantasy and modern offenses.
#   Combined contribution = rush_epa_per_attempt + rec_epa_per_target.
#   Not a formal metric -- a ranking heuristic for this visualization only.

MIN_RUSH_ATT_RB <- 50L
MIN_TARGETS_RB  <- 20L
TOP_N_RB        <- 20L

message("\n--- Plot 2: Dual-Threat RB Lollipop (2025) ---")

plot2_data <- panel_2025 %>%
  filter(
    position_group == "RB",
    !low_volume,
    rush_attempts >= MIN_RUSH_ATT_RB,
    targets       >= MIN_TARGETS_RB,
    !is.na(rush_epa_per_attempt),
    !is.na(rec_epa_per_target)
  ) %>%
  mutate(
    combined_epa = rush_epa_per_attempt + rec_epa_per_target
  ) %>%
  arrange(desc(combined_epa)) %>%
  slice_head(n = TOP_N_RB) %>%
  # Factor ordered worst-to-best so ggplot draws best at top of y-axis
  mutate(
    player_label = factor(player_name,
                          levels = rev(player_name))
  )

n_labeled_plot2 <- nrow(plot2_data)
message(glue("  Qualified RBs (>= {MIN_RUSH_ATT_RB} rush, >= {MIN_TARGETS_RB} targets): {n_labeled_plot2}"))

if (n_labeled_plot2 >= 50L) {
  warning(glue(
    "Plot 2 has {n_labeled_plot2} labeled points (>= 50). ",
    "Add an interactive HTML version per the density threshold rule."
  ))
}

cat(glue("\nPlot 2 summary (2025 dual-threat RBs):\n"))
cat(glue("  n = {n_labeled_plot2} qualified RBs\n"))
cat(glue(
  "  Rush EPA/att range: [{round(min(plot2_data$rush_epa_per_attempt), 3)}, ",
  "{round(max(plot2_data$rush_epa_per_attempt), 3)}]\n"
))
cat(glue(
  "  Rec EPA/target range: [{round(min(plot2_data$rec_epa_per_target), 3)}, ",
  "{round(max(plot2_data$rec_epa_per_target), 3)}]\n"
))

# Reshape to long format for a clean two-sided lollipop
plot2_long <- plot2_data %>%
  select(player_label, rush_epa_per_attempt, rec_epa_per_target) %>%
  tidyr::pivot_longer(
    cols      = c(rush_epa_per_attempt, rec_epa_per_target),
    names_to  = "metric",
    values_to = "epa_value"
  ) %>%
  mutate(
    metric_label = dplyr::recode(metric,
      "rush_epa_per_attempt" = "Rush EPA / attempt",
      "rec_epa_per_target"   = "Receiving EPA / target"
    )
  )

p2 <- ggplot(plot2_long,
             aes(x = epa_value, y = player_label, color = metric_label)) +
  # Zero spine
  geom_vline(xintercept = 0, color = "gray40", linewidth = 0.6) +
  # Segments from 0 to value
  geom_segment(aes(x = 0, xend = epa_value,
                   y = player_label, yend = player_label),
               linewidth = 1.2, alpha = 0.7) +
  # Point caps
  geom_point(size = 3.5) +
  scale_color_manual(
    values = c(
      "Rush EPA / attempt"    = "#D55E00",
      "Receiving EPA / target" = "#0072B2"
    ),
    name = NULL
  ) +
  scale_x_continuous(name = "EPA per opportunity (0 = league average)") +
  labs(
    title    = "Dual-Threat RBs: Rushing and Receiving Efficiency",
    subtitle = glue(
      "2025 Regular Season | >= {MIN_RUSH_ATT_RB} rush attempts AND ",
      ">= {MIN_TARGETS_RB} targets | Top {TOP_N_RB} by combined EPA"
    ),
    y       = NULL,
    caption = glue(
      CAPTION_STD,
      "\nNote: combined EPA = rush EPA/att + rec EPA/target (ranking heuristic only)"
    )
  ) +
  theme_analytics() +
  theme(
    legend.position  = "top",
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_blank()
  )

ggsave(
  filename = file.path(OUTPUT_DIR, "week2_plot2_dual_threat_rb_2025.png"),
  plot     = p2,
  width    = 10, height = 9, dpi = 300, bg = "white"
)
message("  Saved: week2_plot2_dual_threat_rb_2025.png")


# =============================================================================
# PLOT 3: Total EPA Distribution by Position Group (2023-2025)
# =============================================================================
# Violin + box hybrid: distribution of a continuous variable across categories.
# Correct chart type per framework -- shows shape, spread, and median together.
# Filtered to above-threshold skill positions only (QB, RB, WR_TE).
# "Other" position group excluded -- OL, DL, K, P have near-zero offensive EPA
# by construction and would compress the y-axis without adding insight.
# Silent exclusion rule: exclusion documented explicitly in subtitle and caption.
#
# NFL context:
#   Total EPA = pass_epa + rush_epa + rec_epa (coalescing NAs to 0).
#   QBs have the widest spread -- a great QB accumulates hundreds of EPA
#   while a poor QB is deeply negative. WR/TE and RB spreads are narrower
#   because opportunity volume is more constrained.
#   The panel makes this cross-season, cross-position view trivial to produce.

MIN_PLAYS_PLOT3 <- 50L  # Tighter than panel default for cleaner distribution

message("\n--- Plot 3: EPA Distribution by Position Group (2023-2025) ---")

plot3_data <- panel_3yr %>%
  filter(
    position_group %in% c("QB", "RB", "WR_TE"),
    total_plays >= MIN_PLAYS_PLOT3
  ) %>%
  mutate(
    position_label = dplyr::recode(position_group,
      "QB"    = "Quarterback",
      "RB"    = "Running Back",
      "WR_TE" = "WR / TE"
    ),
    season_label = as.character(season)
  )

n_plot3 <- nrow(plot3_data)
message(glue("  Player-seasons above {MIN_PLAYS_PLOT3} plays: {n_plot3}"))

cat(glue("\nPlot 3 summary (2023-2025, >= {MIN_PLAYS_PLOT3} total plays):\n"))
pos_summary <- plot3_data %>%
  group_by(position_label, season) %>%
  summarise(
    n          = n(),
    median_epa = round(median(total_epa, na.rm = TRUE), 1),
    .groups    = "drop"
  )
print(pos_summary)

# Compute per-group medians for annotation -- must include season_label
# so geom_text maps correctly into each facet panel.
# Without season_label, ggplot repeats the same collapsed value across all facets.
group_medians <- plot3_data %>%
  group_by(season_label, position_label) %>%
  summarise(median_epa = median(total_epa, na.rm = TRUE), .groups = "drop")

p3 <- ggplot(plot3_data,
             aes(x = position_label, y = total_epa, fill = position_label)) +
  # Reference line at zero (zero-EPA season = net neutral)
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  # Violin shows full distribution shape
  geom_violin(alpha = 0.3, color = NA, trim = TRUE) +
  # Box inside violin shows median and IQR clearly
  geom_boxplot(width = 0.15, outlier.shape = NA,
               color = "gray30", fill = "white", linewidth = 0.5) +
  # Jittered points show individual player-seasons (small alpha, not labeled)
  geom_jitter(width = 0.08, height = 0, alpha = 0.12, size = 0.8,
              color = "gray40") +
  # Median annotation per position group
  geom_text(
    data   = group_medians,
    aes(x = position_label, y = median_epa,
        label = glue("Median: {round(median_epa, 1)}")),
    color  = "gray20",
    size   = 3,
    vjust  = -0.8,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Quarterback" = "#0072B2",
      "Running Back" = "#D55E00",
      "WR / TE"     = "#009E73"
    ),
    guide = "none"
  ) +
  scale_y_continuous(name = "Season Total EPA") +
  scale_x_discrete(name = NULL) +
  facet_wrap(~ season_label, nrow = 1) +
  labs(
    title    = "Season Total EPA Distribution by Position Group",
    subtitle = glue(
      "2023-2025 Regular Season | >= {MIN_PLAYS_PLOT3} total offensive plays | ",
      "Other position group excluded (specialists, OL, defense)"
    ),
    caption  = glue(
      CAPTION_STD,
      "\nTotal EPA = pass EPA + rush EPA + receiving EPA | ",
      "Points show individual player-seasons | Box shows median and IQR"
    )
  ) +
  theme_analytics() +
  theme(
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold", size = rel(1.0))
  )

ggsave(
  filename = file.path(OUTPUT_DIR, "week2_plot3_epa_distribution_2023_2025.png"),
  plot     = p3,
  width    = 12, height = 7, dpi = 300, bg = "white"
)
message("  Saved: week2_plot3_epa_distribution_2023_2025.png")


# =============================================================================
# COMPLETION SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("VISUALIZATION COMPLETE -- Season 2, Week 2\n")
cat("=============================================================================\n\n")

cat("Files saved to output/plots/:\n")
cat("  week2_plot1_qb_efficiency_2025.png\n")
cat("  week2_plot2_dual_threat_rb_2025.png\n")
cat("  week2_plot3_epa_distribution_2023_2025.png\n\n")

cat("Density threshold check:\n")
cat(glue("  Plot 1: {n_labeled_plot1} labeled QBs   -- {if (n_labeled_plot1 < 50) 'static only (< 50)' else 'INTERACTIVE REQUIRED'}\n"))
cat(glue("  Plot 2: {n_labeled_plot2} labeled RBs   -- {if (n_labeled_plot2 < 50) 'static only (< 50)' else 'INTERACTIVE REQUIRED'}\n"))
cat("  Plot 3: no individual labels     -- static only\n\n")

cat("Key findings:\n")
cat("  Plot 1: Quadrant view surfaces boom/bust vs consistent vs elite QBs.\n")
cat("  Plot 2: Lollipop structure only possible because panel stores\n")
cat("          rushing and receiving on the same row per player.\n")
cat("  Plot 3: QB EPA spread is dramatically wider than RB or WR/TE --\n")
cat("          confirms QB is the highest-variance position for total contribution.\n")
cat("  Silent exclusion: 'other' position group excluded from Plot 3.\n")
cat("          Documented in subtitle and caption per visualization standards.\n")
