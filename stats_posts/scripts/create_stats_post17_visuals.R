# ============================================================
# Statistics Series - Post 17: Sampling Methods
# Visual script: random sampling range vs. stratified precision
# ============================================================
#
# Two panels:
#   Panel 1: min-max range of position counts across 1,000 random
#            draws (n = 40), with the population-share expectation
#            and the stratified sample result marked for contrast.
#   Panel 2: average deviation from the expected position count per
#            draw, random vs. stratified, by position.
#
# All figures below are hardcoded from the verified console output
# of pull_stats_post17_sampling_methods_demo_data.R (population
# n = 608, sample n = 40, 1,000 random draws, seed 2017 / seed 99
# for the example draw). Do not regenerate by re-running the
# simulation with a different seed; these are the locked, verified
# numbers the post's prose is built on.
#
# Color convention note: navy is reserved series-wide for the
# correct/actual value and red for a naive or wrong assumption.
# Neither sampling method here is wrong, so red is not used. Random
# sampling uses amber/gold (a legitimate but wider-variance method),
# stratified uses navy (the tighter, by-design method). This follows
# the same departure flagged for Post 16 Panel 2, pending Christian's
# decision on a series-wide fix.

# ---- CONFIGURATION ----
library(ggplot2)
library(dplyr)
library(scales)

OUTPUT_DIR <- "stats_posts/output/plots"
FILE_PREFIX <- "stats_post17_"
DPI <- 300

COLOR_STRATIFIED <- "#1B3A6B"  # navy - by-design, tight match
COLOR_RANDOM     <- "#D4A017"  # amber/gold - legitimate but wide
COLOR_EXPECTED   <- "gray40"

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ---- Data (verified console output, do not regenerate) ----
panel1_data <- tibble::tibble(
  position       = c("QB", "TE", "RB", "WR"),
  proportion     = c(0.133, 0.224, 0.248, 0.395),
  random_min     = c(0, 2, 3, 7),
  random_max     = c(14, 17, 18, 24),
  expected_n     = c(5.33, 8.95, 9.94, 15.8),
  stratified_n   = c(5, 9, 10, 16)
) %>%
  mutate(position = factor(position, levels = c("QB", "TE", "RB", "WR")))

panel2_data <- tibble::tibble(
  position = rep(c("QB", "TE", "RB", "WR"), times = 2),
  method   = rep(c("Random", "Stratified"), each = 4),
  mean_abs_dev = c(1.67, 2.09, 2.14, 2.38,   # random, in QB/TE/RB/WR order
                   0.33, 0.05, 0.06, 0.21)   # stratified, same order
) %>%
  mutate(
    position = factor(position, levels = c("QB", "TE", "RB", "WR")),
    method = factor(method, levels = c("Random", "Stratified"))
  )

# ---- Panel 1: range of random draws vs. stratified precision ----
p1 <- ggplot(panel1_data, aes(y = position)) +
  geom_segment(
    aes(x = random_min, xend = random_max, yend = position),
    color = COLOR_RANDOM, linewidth = 3, alpha = 0.55
  ) +
  geom_point(
    aes(x = expected_n), color = COLOR_EXPECTED, size = 3, shape = 16
  ) +
  geom_point(
    aes(x = stratified_n), color = COLOR_STRATIFIED, size = 4.5, shape = 18
  ) +
  geom_text(
    aes(x = random_min, label = random_min),
    hjust = 1.4, size = 3.2, color = COLOR_RANDOM
  ) +
  geom_text(
    aes(x = random_max, label = random_max),
    hjust = -0.5, size = 3.2, color = COLOR_RANDOM
  ) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  coord_cartesian(xlim = c(-2, 27)) +
  labs(
    title = "A random draw could hand you zero quarterbacks, or fourteen",
    subtitle = paste0(
      "Gold bars: min-to-max range across 1,000 random draws of 40 players.  ",
      "Gray dot: population-share expectation.  Navy diamond: stratified result."
    ),
    x = "Players in a 40-player sample",
    y = NULL,
    caption = "2025 season, n = 608 population (QB/RB/WR/TE, >=1 regular season game)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9.5, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(size = 8, color = "gray50")
  )

# ---- Panel 2: mean absolute deviation, random vs. stratified ----
p2 <- ggplot(panel2_data, aes(x = position, y = mean_abs_dev, fill = method)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = sprintf("%.2f", mean_abs_dev)),
    position = position_dodge(width = 0.7),
    vjust = -0.5, size = 3.2
  ) +
  scale_fill_manual(values = c("Random" = COLOR_RANDOM, "Stratified" = COLOR_STRATIFIED)) +
  scale_y_continuous(limits = c(0, 2.8), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Stratified sampling misses its target by a fraction of random's miss",
    subtitle = "Average deviation from the expected position count per draw, by method",
    x = NULL,
    y = "Mean absolute deviation (players)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9.5, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.justification = "left"
  )

# ---- Combine with patchwork, graceful degradation to separate PNGs ----
has_patchwork <- requireNamespace("patchwork", quietly = TRUE)

if (has_patchwork) {
  library(patchwork)
  combined <- p1 / p2 +
    patchwork::plot_layout(heights = c(1, 1.1)) +
    patchwork::plot_annotation(
      title = "Sampling Methods: Random vs. Stratified",
      theme = theme(plot.title = element_text(face = "bold", size = 15))
    )

  ggsave(
    filename = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "combined.png")),
    plot = combined, width = 9, height = 9.5, dpi = DPI, bg = "white"
  )
  cat("Saved combined two-panel figure:", file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "combined.png")), "\n")
} else {
  message("patchwork not installed, saving panels separately")
  ggsave(
    filename = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "panel1_range.png")),
    plot = p1, width = 9, height = 4.5, dpi = DPI, bg = "white"
  )
  ggsave(
    filename = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "panel2_deviation.png")),
    plot = p2, width = 9, height = 5, dpi = DPI, bg = "white"
  )
  cat("Saved panel 1:", file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "panel1_range.png")), "\n")
  cat("Saved panel 2:", file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "panel2_deviation.png")), "\n")
}

# ---- Console summary ----
cat("\n=== VISUAL SCRIPT SUMMARY ===\n")
cat("Panel 1: random draw range (min/max) vs. expected count vs. stratified result, by position\n")
cat("Panel 2: mean absolute deviation from expected count, random vs. stratified, by position\n")
cat("Output directory:", OUTPUT_DIR, "\n")
cat("DPI:", DPI, "\n")
cat("File prefix:", FILE_PREFIX, "\n")
cat("\nWatch items to check on render:\n")
cat("1. Panel 1: QB row min-label ('0') sits close to the y-axis at x = -2 xlim;\n")
cat("   confirm it doesn't clip against the panel edge.\n")
cat("2. Panel 1: navy diamond and gray dot nearly overlap at every position\n")
cat("   (by design, this is the point) -- confirm both remain visually distinct\n")
cat("   at render size rather than merging into one mark.\n")
cat("3. Panel 2: dodge width and text label vjust -- confirm the '2.38' label\n")
cat("   for WR/Random doesn't clip the top plot margin given the 2.8 y-limit.\n")
