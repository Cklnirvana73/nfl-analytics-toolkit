# ==============================================================================
# WEEK 5: VISUALIZATION SCRIPT (REDESIGNED)
# ==============================================================================
# Generates 3 publication-quality plots for EPA trend analysis
#
# Design principles applied:
# - Every plot must answer ONE clear question readable in 3 seconds
# - No spaghetti charts (max 1-2 lines per panel)
# - Labels on the data, not in legends where possible
# - Color used for meaning, not decoration
# - Small multiples over overplotting
#
# Plot 1: EPA Trend Small Multiples (3 improving, 3 declining)
# Plot 2: Metric Stability Dot Plot (full names, tier shading)
# Plot 3: Down x Distance EPA Heatmap (the interesting cross-tab)
#
# All plots saved to output/plots/ at 300 dpi
# ==============================================================================

library(here)
library(dplyr)
library(ggplot2)
library(tidyr)

# Source Week 5 functions
source(here("R", "01_data_loading.R"))
source(here("R", "07_epa_features.R"))

# Load data
message("Loading 2025 play-by-play data...")
pbp <- load_and_validate_pbp(2025)

# Create output directory
output_dir <- here("output", "plots")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


# ==============================================================================
# PLOT 1: EPA TREND SMALL MULTIPLES
# ==============================================================================
# Question answered: "Who is trending up and who is trending down?"
# One panel per player, 3 improving + 3 declining. Smoothed trend line
# overlaid on weekly points. Green = improving, red = declining.
# ==============================================================================

message("Generating Plot 1: EPA Trend Small Multiples...")

epa_trends <- calculate_epa_trends(pbp, season = 2025, trend_windows = c(4))

# Identify top 3 improving and top 3 declining players
latest_trends <- epa_trends %>%
  filter(!is.na(epa_trend_4)) %>%
  group_by(player_id, player_name) %>%
  filter(week == max(week)) %>%
  ungroup()

top_improving <- latest_trends %>% arrange(desc(epa_trend_4)) %>% head(3)
top_declining <- latest_trends %>% arrange(epa_trend_4) %>% head(3)

highlight_ids <- c(top_improving$player_id, top_declining$player_id)

plot_data <- epa_trends %>%
  filter(player_id %in% highlight_ids) %>%
  mutate(
    trend_direction = ifelse(
      player_id %in% top_improving$player_id, "Improving", "Declining"
    ),
    facet_label = paste0(player_name, "\n", trend_direction)
  )

# Order facets: improving first, then declining
player_order <- bind_rows(
  top_improving %>% mutate(trend_direction = "Improving"),
  top_declining %>% mutate(trend_direction = "Declining")
) %>%
  mutate(facet_label = paste0(player_name, "\n", trend_direction))

plot_data$facet_label <- factor(
  plot_data$facet_label,
  levels = player_order$facet_label
)

p1 <- ggplot(plot_data, aes(x = week, y = epa_per_play)) +
  geom_point(aes(color = trend_direction), size = 2, alpha = 0.7) +
  geom_smooth(
    aes(color = trend_direction),
    method = "loess", se = FALSE, linewidth = 1.2, span = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  facet_wrap(~ facet_label, nrow = 2, scales = "free_x") +
  scale_color_manual(
    values = c("Improving" = "#2E8B57", "Declining" = "#CD3333"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(0, 21, by = 4)) +
  labs(
    title = "Who Is Trending Up (and Down) in EPA per Play?",
    subtitle = "2025 season | Weekly EPA/play with smoothed trend | Garbage time excluded | Dashed line = league avg (0.0)",
    x = "Week",
    y = "EPA per Play",
    caption = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nTrend based on 4-game rolling OLS slope of EPA/play"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 9),
    plot.caption = element_text(color = "grey50", size = 8),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

ggsave(file.path(output_dir, "week5_epa_trends.png"), p1,
       width = 11, height = 7, dpi = 300)
message("  Saved: week5_epa_trends.png")


# ==============================================================================
# PLOT 2: METRIC STABILITY DOT PLOT
# ==============================================================================
# Question answered: "Which metrics are talent-driven vs random noise?"
# Lollipop chart with full metric names. Background bands for tiers.
# One row per position-metric combo. Readable at a glance.
# ==============================================================================

message("Generating Plot 2: Metric Stability Dot Plot...")

stability <- calculate_stability_metrics(pbp, season = 2025, min_games = 6)

stability_plot <- stability %>%
  filter(!is.na(autocorrelation)) %>%
  mutate(
    position = case_when(
      grepl("^qb_", metric) ~ "QB",
      grepl("^rb_", metric) ~ "RB",
      grepl("^wr_", metric) ~ "WR",
      TRUE ~ "Other"
    ),
    metric_clean = gsub("^(qb|rb|wr)_", "", metric),
    metric_clean = gsub("_", " ", metric_clean),
    metric_clean = tools::toTitleCase(metric_clean),
    display_name = paste0(position, ": ", metric_clean)
  ) %>%
  mutate(display_name = reorder(display_name, autocorrelation))

p2 <- ggplot(stability_plot, aes(x = autocorrelation, y = display_name)) +
  # Tier background bands
  annotate("rect", xmin = -Inf, xmax = 0.15, ymin = -Inf, ymax = Inf,
           fill = "#FFCCCC", alpha = 0.3) +
  annotate("rect", xmin = 0.15, xmax = 0.30, ymin = -Inf, ymax = Inf,
           fill = "#FFFFCC", alpha = 0.3) +
  annotate("rect", xmin = 0.30, xmax = 0.50, ymin = -Inf, ymax = Inf,
           fill = "#CCFFCC", alpha = 0.3) +
  annotate("rect", xmin = 0.50, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#99FF99", alpha = 0.3) +
  # Tier labels at top
  annotate("text", x = 0.075, y = Inf, label = "Unstable\n(noise)",
           vjust = 1.3, size = 2.5, color = "#8B0000", fontface = "italic") +
  annotate("text", x = 0.225, y = Inf, label = "Low",
           vjust = 1.3, size = 2.5, color = "#8B8B00", fontface = "italic") +
  annotate("text", x = 0.40, y = Inf, label = "Moderate",
           vjust = 1.3, size = 2.5, color = "#006400", fontface = "italic") +
  annotate("text", x = 0.52, y = Inf, label = "High\n(talent)",
           vjust = 1.3, size = 2.5, color = "#006400", fontface = "italic") +
  # Lollipop stems
  geom_segment(aes(x = 0, xend = autocorrelation,
                   y = display_name, yend = display_name,
                   color = position),
               linewidth = 0.6, alpha = 0.5) +
  # Dots
  geom_point(aes(color = position), size = 4) +
  # Value labels
  geom_text(aes(label = sprintf("%.2f", autocorrelation)),
            hjust = -0.3, size = 3) +
  scale_color_manual(values = c("QB" = "#2C5F8A", "RB" = "#8A2C2C",
                                 "WR" = "#2C8A5F", "Other" = "grey60")) +
  scale_x_continuous(
    limits = c(0, max(stability_plot$autocorrelation, na.rm = TRUE) + 0.08),
    breaks = seq(0, 0.5, by = 0.1)
  ) +
  labs(
    title = "Which NFL Metrics Are Talent vs Noise?",
    subtitle = "Week-to-week autocorrelation (r) | Higher = more stable/predictive | 2025 season",
    x = "Autocorrelation (r)",
    y = NULL,
    color = "Position",
    caption = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nMin 6 games played | Garbage time excluded"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 9),
    plot.caption = element_text(color = "grey50", size = 8),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9),
    plot.margin = margin(t = 15, r = 15, b = 10, l = 10)
  )

ggsave(file.path(output_dir, "week5_stability_analysis.png"), p2,
       width = 10, height = 7, dpi = 300)
message("  Saved: week5_stability_analysis.png")


# ==============================================================================
# PLOT 3: DOWN x DISTANCE EPA HEATMAP
# ==============================================================================
# Question answered: "How does EPA change across down-and-distance combos?"
# Down on Y axis, distance bucket on X axis. Fill = EPA per play.
# Cell labels show EPA value + play count. The 2D cross is the insight.
# ==============================================================================

message("Generating Plot 3: Down x Distance EPA Heatmap...")

# Build from play-level data (need down + distance on same play)
pbp_clean <- pbp %>%
  filter(
    !is.na(epa),
    !is.na(posteam),
    play_type %in% c("pass", "run"),
    !is.na(down),
    !is.na(ydstogo),
    !(qtr == 4 & !is.na(wp) & (wp < 0.10 | wp > 0.90))
  ) %>%
  mutate(
    down_label = paste0(
      down,
      ifelse(down == 1, "st",
             ifelse(down == 2, "nd",
                    ifelse(down == 3, "rd", "th")))
    ),
    distance_bucket = case_when(
      ydstogo <= 3  ~ "Short\n(1-3 yds)",
      ydstogo <= 7  ~ "Medium\n(4-7 yds)",
      ydstogo <= 12 ~ "Long\n(8-12 yds)",
      TRUE          ~ "Very Long\n(13+ yds)"
    )
  )

pbp_clean$down_label <- factor(pbp_clean$down_label,
                                levels = c("1st", "2nd", "3rd", "4th"))
pbp_clean$distance_bucket <- factor(pbp_clean$distance_bucket,
                                     levels = c("Short\n(1-3 yds)", "Medium\n(4-7 yds)",
                                                "Long\n(8-12 yds)", "Very Long\n(13+ yds)"))

# Aggregate: down x distance
heatmap_data <- pbp_clean %>%
  group_by(down_label, distance_bucket) %>%
  summarise(
    avg_epa = mean(epa, na.rm = TRUE),
    success_rate = mean(epa > 0, na.rm = TRUE),
    plays = n(),
    .groups = "drop"
  ) %>%
  mutate(
    avg_epa_display = ifelse(plays >= 50, avg_epa, NA_real_),
    cell_label = ifelse(
      plays >= 50,
      paste0(sprintf("%+.2f", avg_epa), "\n", format(plays, big.mark = ","), " plays"),
      paste0("n=", plays, "\n(too few)")
    ),
    # For text color: white on dark fills, dark on light fills
    text_color = ifelse(
      !is.na(avg_epa_display) & abs(avg_epa_display) > 0.12, "white", "grey20"
    )
  )

p3 <- ggplot(heatmap_data, aes(x = distance_bucket, y = down_label, fill = avg_epa_display)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = cell_label, color = text_color),
            size = 3.5, fontface = "bold", lineheight = 0.9) +
  scale_color_identity() +
  scale_fill_gradient2(
    low = "#B22222", mid = "#F5F5DC", high = "#228B22",
    midpoint = 0, na.value = "grey85",
    name = "EPA/Play",
    labels = function(x) sprintf("%+.2f", x)
  ) +
  scale_y_discrete(limits = rev(levels(heatmap_data$down_label))) +
  labs(
    title = "EPA per Play by Down and Distance",
    subtitle = "2025 season | Green = efficient, Red = inefficient | Garbage time excluded",
    x = "Yards to Go",
    y = "Down",
    caption = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nCells with <50 plays grayed out"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 9),
    plot.caption = element_text(color = "grey50", size = 8),
    panel.grid = element_blank(),
    axis.text = element_text(size = 11),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  )

ggsave(file.path(output_dir, "week5_situational_epa.png"), p3,
       width = 10, height = 6, dpi = 300)
message("  Saved: week5_situational_epa.png")


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("  Week 5 Visualization Summary (REDESIGNED)\n")
cat("================================================================\n")
cat("  Plots generated: 3\n")
cat("  1. week5_epa_trends.png        - Small multiples: 3 improving + 3 declining\n")
cat("  2. week5_stability_analysis.png - Lollipop dot plot with tier shading\n")
cat("  3. week5_situational_epa.png   - Down x Distance heatmap grid\n")
cat("  Output: output/plots/\n")
cat("  Resolution: 300 dpi\n")
cat("\n")
cat("  Design fixes from v1:\n")
cat("  - Plot 1: Spaghetti (10 lines) -> Small multiples (6 panels)\n")
cat("  - Plot 2: Confusing stacked bars -> Lollipop with full names + tier bands\n")
cat("  - Plot 3: 1D bars with 4th-down spike -> 2D heatmap (down x distance)\n")
cat("================================================================\n")
