# ==============================================================================
# create_stats_post14_visuals.R
# ==============================================================================
#
# PURPOSE
#   Statistics Series Post 14 (The Normal Distribution and Z-Scores).
#   Two stacked panels built entirely from verified console output captured by
#   pull_stats_post14_normal_distribution_demo_data.R. Nothing is re-pulled at
#   render time.
#
# PANELS
#   Panel 1  Deviation from what a normal predicts, per position, for the
#            within-1-SD and within-2-SD bands. Zero is the normal's own
#            prediction. RB lands on it. QB sits 6.5 points above.
#
#   Panel 2  Shapiro-Wilk W per position against a perfect normal (W = 1.0),
#            with p-value and verdict. All four sit close to 1. Three reject.
#
# DESIGN NOTES (v2. v1 failed the 5-second rule and was rebuilt.)
#   - Panel 1 plots DEVIATION, not raw percent. Raw percent on a zero-based
#     axis renders a 6.46-point finding as four visually identical bars. The
#     deviation encoding puts the finding on a -1 to +7 scale where it is the
#     tallest thing in the panel.
#   - Both bands share one axis. facet + scales = "free_y" gave two different
#     scales to the same unit. The visualization reference names this exact
#     pitfall: free scales hide important differences.
#   - Panels stack vertically. Side by side at 5.5in each, the insight titles
#     collided and clipped into one another.
#   - Panel 2 is flipped horizontal so the p-value and verdict fit on one line
#     instead of a three-line axis label overrunning the caption.
#
# COLOR CONVENTION (series standard)
#   navy  = observed / actual
#   red   = model / prediction / assumption
#
# OUTPUT
#   stats_posts/output/plots/stats_post14_normal_distribution.png at 300 DPI
#   Degrades to two separate PNGs if patchwork is unavailable.
#
# ==============================================================================

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

OUTPUT_DIR   <- file.path("stats_posts", "output", "plots")
FILE_PREFIX  <- "stats_post14_"
PLOT_DPI     <- 300
PLOT_WIDTH   <- 10
PLOT_HEIGHT  <- 9.5

COL_OBSERVED <- "#1B2A4A"   # navy: observed, 1 SD band
COL_OBS_ALT  <- "#7C8DA8"   # lighter navy: observed, 2 SD band
COL_MODEL    <- "#C0392B"   # red: what the normal predicts
COL_GRID     <- "grey88"
COL_SUBTEXT  <- "grey35"

# Analytic normal coverage. Not sampled, not rounded from data.
NORMAL_WITHIN_1SD <- 68.27
NORMAL_WITHIN_2SD <- 95.45
PERFECT_W         <- 1.0
ALPHA             <- 0.05

POSITION_ORDER <- c("QB", "TE", "WR", "RB")  # worst to best fit, left to right

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ------------------------------------------------------------------------------
# VERIFIED DATA (embedded from console output, not re-pulled)
# ------------------------------------------------------------------------------

# From: --- DELTA DISTRIBUTION SHAPE (ages 24-30) ---
delta_shape <- tibble::tribble(
  ~position, ~n,    ~within_1sd, ~within_2sd,
  "QB",      372L,  74.73,       94.62,
  "RB",      954L,  68.24,       95.81,
  "TE",      607L,  72.49,       95.55,
  "WR",      1147L, 69.22,       94.77
)

# From: --- SHAPIRO-WILK PER POSITION (the discarded values) ---
normality <- tibble::tribble(
  ~position, ~n,     ~shapiro_W, ~shapiro_p, ~normal,
  "QB",      372L,   0.97633,    0.000009,   FALSE,
  "RB",      954L,   0.99833,    0.493333,   TRUE,
  "WR",      1147L,  0.99621,    0.006535,   FALSE,
  "TE",      607L,   0.99017,    0.000442,   FALSE
)

# ------------------------------------------------------------------------------
# PANEL 1: deviation from the normal's prediction
# ------------------------------------------------------------------------------

p1_data <- delta_shape %>%
  dplyr::mutate(
    dev_1sd = within_1sd - NORMAL_WITHIN_1SD,
    dev_2sd = within_2sd - NORMAL_WITHIN_2SD
  ) %>%
  dplyr::select(position, n, dev_1sd, dev_2sd) %>%
  tidyr::pivot_longer(
    cols      = c(dev_1sd, dev_2sd),
    names_to  = "band",
    values_to = "deviation"
  ) %>%
  dplyr::mutate(
    band = factor(
      band,
      levels = c("dev_1sd", "dev_2sd"),
      labels = c("Within 1 SD", "Within 2 SD")
    ),
    position = factor(position, levels = POSITION_ORDER),
    lbl = sprintf("%+.2f", deviation),
    # Negative bars need the label below them, positive bars above.
    # vjust is a valid geom_text aesthetic, so it can vary row by row.
    lbl_vjust = ifelse(deviation >= 0, -0.45, 1.35)
  )

DODGE <- position_dodge(width = 0.72)

p1 <- ggplot(p1_data, aes(x = position, y = deviation, fill = band)) +
  geom_col(width = 0.68, position = DODGE) +
  geom_text(
    aes(label = lbl, vjust = lbl_vjust),
    position = DODGE, size = 3.2, fontface = "bold", color = COL_SUBTEXT
  ) +
  # Zero IS the normal's prediction. Drawn after the bars so it stays visible.
  geom_hline(yintercept = 0, color = COL_MODEL, linetype = "dashed",
             linewidth = 0.8) +
  scale_fill_manual(
    values = c("Within 1 SD" = COL_OBSERVED, "Within 2 SD" = COL_OBS_ALT),
    name   = NULL
  ) +
  scale_x_discrete(
    labels = function(x) {
      n_map <- setNames(delta_shape$n, delta_shape$position)
      sprintf("%s\nn = %s", x, trimws(format(n_map[x], big.mark = ",")))
    }
  ) +
  coord_cartesian(ylim = c(-2.2, 7.6), clip = "off") +
  labs(
    title = "RB lands on the empirical rule. QB sits 6.5 points above it.",
    subtitle = paste0(
      "Distance between the observed share of year-over-year fantasy points ",
      "per game changes and what a normal\ndistribution predicts (68.27 pct ",
      "within 1 SD, 95.45 pct within 2 SD). The red line is the normal, so ",
      "zero means\nthe data matched it exactly. Ages 24 to 30, 2010 to 2025 ",
      "seasons."
    ),
    x = NULL,
    y = "Observed minus predicted (percentage points)"
  )

# ------------------------------------------------------------------------------
# PANEL 2: Shapiro-Wilk W against a perfect normal
# ------------------------------------------------------------------------------

p2_data <- normality %>%
  dplyr::mutate(
    # rev() so QB reads at the top after the flip, matching Panel 1's order
    position = factor(position, levels = rev(POSITION_ORDER)),
    verdict  = ifelse(normal, "passed", "rejected"),
    # p = 0.000009 and p = 0.493333 need different precision
    p_label  = ifelse(
      shapiro_p < 0.001,
      sprintf("p = %.6f", shapiro_p),
      sprintf("p = %.3f", shapiro_p)
    ),
    w_only    = sprintf("W = %.5f", shapiro_W),
    p_verdict = sprintf("%s, %s", p_label, verdict)
  )

# Flipped horizontal. The vertical version needed a three-line axis label per
# bar, which overran the caption at any sane figure height.
p2 <- ggplot(p2_data, aes(x = position, y = shapiro_W)) +
  geom_col(fill = COL_OBSERVED, width = 0.6) +
  # Fixed y just above the zoom floor. Every bar clears it, so all four labels
  # start at the same place and none escape their bar.
  # W sits INSIDE the bar, right-aligned to the bar end, so it can never
  # run off into white space. Every bar is long enough to hold it.
  geom_text(
    aes(y = shapiro_W, label = w_only),
    hjust = 1.12, size = 3.2, color = "white", fontface = "bold"
  ) +
  # p and verdict sit AFTER the bar end in dark text on white background.
  geom_text(
    aes(y = shapiro_W, label = p_verdict),
    hjust = -0.12, size = 3.2, color = COL_SUBTEXT
  ) +
  geom_hline(yintercept = PERFECT_W, color = COL_MODEL,
             linetype = "dashed", linewidth = 0.8) +
  scale_x_discrete(
    labels = function(x) {
      n_map <- setNames(normality$n, normality$position)
      sprintf("%s\nn = %s", x, trimws(format(n_map[x], big.mark = ",")))
    }
  ) +
  # Zoom, not filter. All four W sit between 0.976 and 0.999, so a zero-based
  # axis renders four identical full-height bars. scale_y_continuous(limits=)
  # would drop the rows instead of zooming and take the labels with them.
  coord_flip(ylim = c(0.970, 1.0105)) +
  labs(
    title = "All four shapes sit close to normal. Three of them still reject it.",
    subtitle = paste0(
      "Shapiro-Wilk on the same transitions. W measures how close the shape is ",
      "to a normal, and the red line at\nW = 1.0 is a perfect one. The p-value ",
      "asks a different question: whether a normal can be ruled out.\nWith ",
      "1,147 WR transitions, a W of 0.996 is enough to reject. Alpha 0.05."
    ),
    x = NULL,
    y = "Shapiro-Wilk W"
  )

# ------------------------------------------------------------------------------
# THEME
# ------------------------------------------------------------------------------

theme_stats_series <- theme_minimal(base_size = 11) +
  theme(
    plot.title           = element_text(face = "bold", size = 13.5,
                                        margin = margin(b = 4)),
    plot.subtitle        = element_text(size = 9, color = COL_SUBTEXT,
                                        lineheight = 1.25,
                                        margin = margin(b = 12)),
    axis.title           = element_text(size = 9, color = COL_SUBTEXT),
    axis.text            = element_text(size = 9.5),
    legend.position      = "top",
    legend.justification = "left",
    legend.text          = element_text(size = 9),
    panel.grid.minor     = element_blank(),
    plot.margin          = margin(14, 18, 12, 14)
  )

p1 <- p1 + theme_stats_series +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = COL_GRID))

p2 <- p2 + theme_stats_series +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = COL_GRID),
        legend.position    = "none")

FIG_CAPTION <- paste0(
  "Data: nflfastR via my aging curve panel, 2010 to 2025 regular seasons. ",
  "3,080 year-over-year transitions across QB, RB, WR, TE at ages 24 to 30.\n",
  "Normal reference values are analytic (68.27 and 95.45), not sampled."
)

# ------------------------------------------------------------------------------
# COMBINE AND SAVE
# ------------------------------------------------------------------------------

has_patchwork <- requireNamespace("patchwork", quietly = TRUE)

if (has_patchwork) {

  # Stacked, not side by side. Each panel needs the full width for its title.
  combined <- patchwork::wrap_plots(p1, p2, ncol = 1) +
    patchwork::plot_annotation(
      caption = FIG_CAPTION,
      theme = theme(
        plot.caption = element_text(size = 8, color = COL_SUBTEXT, hjust = 0,
                                    lineheight = 1.2, margin = margin(t = 10))
      )
    )

  out_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "normal_distribution.png"))
  ggsave(out_path, plot = combined, width = PLOT_WIDTH, height = PLOT_HEIGHT,
         dpi = PLOT_DPI)
  saved_files <- out_path

} else {

  message("patchwork not available. Saving two separate PNGs instead.")

  p1_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "empirical_rule.png"))
  p2_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "shapiro_w.png"))

  ggsave(p1_path, plot = p1 + labs(caption = FIG_CAPTION),
         width = PLOT_WIDTH, height = 5.2, dpi = PLOT_DPI)
  ggsave(p2_path, plot = p2 + labs(caption = FIG_CAPTION),
         width = PLOT_WIDTH, height = 4.6, dpi = PLOT_DPI)

  saved_files <- c(p1_path, p2_path)
}

# ------------------------------------------------------------------------------
# CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("==============================================================\n")
cat("  POST 14 VISUALS\n")
cat("==============================================================\n\n")

cat("Files written:\n")
for (f in saved_files) cat(sprintf("  %s\n", f))
cat(sprintf("\nDPI: %d | size: %g x %g in | patchwork: %s\n",
            PLOT_DPI, PLOT_WIDTH, PLOT_HEIGHT,
            if (has_patchwork) "yes, stacked" else "no, two PNGs"))

cat("\n--- PANEL 1: deviation from the normal ---\n")
print(as.data.frame(
  delta_shape %>%
    dplyr::mutate(
      dev_1sd = round(within_1sd - NORMAL_WITHIN_1SD, 2),
      dev_2sd = round(within_2sd - NORMAL_WITHIN_2SD, 2)
    ) %>%
    dplyr::select(position, n, within_1sd, dev_1sd, within_2sd, dev_2sd) %>%
    dplyr::arrange(abs(dev_1sd))
), row.names = FALSE)
cat(sprintf("\nNormal predicts %.2f pct within 1 SD, %.2f pct within 2 SD\n",
            NORMAL_WITHIN_1SD, NORMAL_WITHIN_2SD))

cat("\n--- PANEL 2: Shapiro-Wilk ---\n")
print(as.data.frame(
  normality %>%
    dplyr::mutate(verdict = ifelse(normal, "passed", "rejected")) %>%
    dplyr::select(position, n, shapiro_W, shapiro_p, verdict)
), row.names = FALSE)

cat(sprintf("\nTransitions in the 24 to 30 band: %s\n",
            format(sum(delta_shape$n), big.mark = ",")))
cat(sprintf("Rejecting normality: %s\n",
            paste(normality$position[!normality$normal], collapse = ", ")))
cat(sprintf("W range: %.5f to %.5f (a perfect normal is 1.0)\n",
            min(normality$shapiro_W), max(normality$shapiro_W)))

cat("\n==============================================================\n\n")
