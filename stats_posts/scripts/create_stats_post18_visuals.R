# ==============================================================================
# Statistics Series -- Post 18: Sampling Distributions
# Visual script (v2, like-for-like)
#
# v1 compared individual absolute errors against a DIFFERENCE of two group
# means. Those are different quantities, and on a shared axis the difference
# sat below zero next to two bars that cannot go below zero. This version
# compares individual misses against the resampled MEAN of the same group.
# Both are PPR points per game, both positive, both centered near 3.14.
#
# Panel 1: the data range with the sampling distribution nested inside it.
#          The nesting is the finding. Shared axis is mandatory.
# Panel 2: the sampling distribution at its own scale, so its shape is legible.
#
# All numbers HARDCODED from verified console output of
# pull_stats_post18_addendum_single_group.R. Nothing is re-simulated.
#
# Output: 300 DPI PNG to stats_posts/output/plots/, prefix stats_post18_
# ==============================================================================

library(ggplot2)

OUT_DIR <- file.path("stats_posts", "output", "plots")
PREFIX  <- "stats_post18_"
DPI     <- 300

# navy = the statistic, gold = the raw data. Neither is a wrong value, so gold
# substitutes for red here. Series-wide convention decision still pending
# (open since Post 16).
COL_NAVY <- "#1B3A6B"
COL_GOLD <- "#C89B3C"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# ==============================================================================
# VERIFIED VALUES
# ==============================================================================

N_PLAYERS  <- 115L

DATA_MIN   <- 0.0189;  DATA_MAX <- 14.0201
DATA_MEAN  <- 3.1404;  DATA_SD  <- 2.7211
DATA_SKEW  <- 1.4983

BOOT_MIN   <- 2.4124;  BOOT_MAX <- 3.9578
BOOT_MEAN  <- 3.1314;  BOOT_SD  <- 0.2543
BOOT_SKEW  <- 0.1216

SE_FORMULA <- 0.2537
SD_RATIO   <- 10.7

bin_mids <- c(2.425, 2.475, 2.525, 2.575, 2.625, 2.675, 2.725, 2.775, 2.825,
              2.875, 2.925, 2.975, 3.025, 3.075, 3.125, 3.175, 3.225, 3.275,
              3.325, 3.375, 3.425, 3.475, 3.525, 3.575, 3.625, 3.675, 3.725,
              3.775, 3.825, 3.875, 3.925, 3.975)

bin_counts <- c(1, 0, 8, 5, 13, 21, 17, 34, 34, 51, 66, 54, 73, 83, 63, 67,
                95, 78, 50, 42, 42, 31, 19, 19, 8, 8, 6, 4, 4, 2, 1, 1)

stopifnot(length(bin_mids) == length(bin_counts), sum(bin_counts) == 1000L)

theme_series <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle    = element_text(size = 11, color = "grey30", hjust = 0),
    plot.caption     = element_text(size = 9, color = "grey45", hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position  = "none",
    plot.margin      = margin(14, 18, 12, 14)
  )

# ==============================================================================
# PANEL 1: THE STATISTIC NESTED INSIDE THE DATA (SHARED AXIS)
# ==============================================================================

p1_data <- data.frame(
  label = factor(
    c("Individual misses\n115 players",
      "The statistic\n1,000 resampled averages"),
    levels = c("The statistic\n1,000 resampled averages",
               "Individual misses\n115 players")
  ),
  lo = c(DATA_MIN, BOOT_MIN),
  hi = c(DATA_MAX, BOOT_MAX),
  ctr = c(DATA_MEAN, BOOT_MEAN),
  is_stat = c(FALSE, TRUE)
)

p1 <- ggplot(p1_data, aes(y = label)) +
  geom_segment(aes(x = lo, xend = hi, yend = label, color = is_stat),
               linewidth = 9, lineend = "round") +
  geom_point(aes(x = ctr), color = "white", size = 2.8) +
  scale_color_manual(values = c("FALSE" = COL_GOLD, "TRUE" = COL_NAVY)) +
  scale_x_continuous(breaks = seq(0, 14, by = 2),
                     limits = c(-0.6, 15.4)) +
  annotate("text", x = DATA_MAX + 0.3, y = 2,
           label = paste0("spans ", round(DATA_MAX - DATA_MIN, 1), " points"),
           hjust = 0, size = 3.5, color = "grey25") +
  annotate("text", x = BOOT_MAX + 0.3, y = 1,
           label = paste0("spans ", round(BOOT_MAX - BOOT_MIN, 2), " points"),
           hjust = 0, size = 3.5, color = COL_NAVY, fontface = "bold") +
  labs(
    title = paste0("An average of 115 values moves ", SD_RATIO,
                   " times less than the values themselves"),
    subtitle = paste0(
      "Same quantity, same unit. White dot marks the centre. Standard ",
      "deviation ", DATA_SD, " for the individual\nmisses against ", BOOT_SD,
      " for the 1,000 resampled averages."
    ),
    x = "Miss between prior-season average and actual second-half scoring (PPR points per game)",
    y = NULL,
    caption = paste0(
      "Injury proximity experiment, 2011-2025. 115 returning player-seasons. ",
      "Bootstrap: 1,000 resamples, seed 42."
    )
  ) +
  theme_series +
  theme(axis.text.y = element_text(size = 10, lineheight = 0.95),
        axis.title.x = element_text(size = 10))

# ==============================================================================
# PANEL 2: THE SAMPLING DISTRIBUTION AT ITS OWN SCALE
# ==============================================================================
# Zoomed deliberately. Panel 1 already established how narrow this is; this
# panel exists to show its SHAPE, which is invisible at Panel 1's scale.

p2 <- ggplot(data.frame(mid = bin_mids, count = bin_counts),
             aes(x = mid, y = count)) +
  geom_col(width = 0.047, fill = COL_NAVY, alpha = 0.88) +
  geom_vline(xintercept = DATA_MEAN, color = COL_GOLD,
             linewidth = 1.0, linetype = "dashed") +
  annotate("text", x = DATA_MEAN + 0.04, y = max(bin_counts) * 0.97,
           label = paste0("observed average ", round(DATA_MEAN, 2)),
           hjust = 0, size = 3.4, color = COL_GOLD, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  scale_x_continuous(breaks = seq(2.4, 4.0, by = 0.2)) +
  labs(
    title = "Skewed data, symmetric average",
    subtitle = paste0(
      "The individual misses carry a skewness of ", DATA_SKEW,
      ". These 1,000 averages of those same misses\ncome back at ",
      BOOT_SKEW, ". The function keeps only the 2.5th and 97.5th percentiles ",
      "of this shape."
    ),
    x = "Resampled average miss (PPR points per game)",
    y = "Resamples",
    caption = paste0(
      "Bootstrap standard error ", BOOT_SD, " against the formula ",
      "standard deviation \u00F7 \u221An = ", SE_FORMULA, "."
    )
  ) +
  theme_series

# ==============================================================================
# EXPORT
# ==============================================================================

f1 <- file.path(OUT_DIR, paste0(PREFIX, "spread_contrast.png"))
f2 <- file.path(OUT_DIR, paste0(PREFIX, "sampling_distribution.png"))

ggsave(f1, p1, width = 9, height = 4.4, dpi = DPI, bg = "white")
ggsave(f2, p2, width = 9, height = 5.2, dpi = DPI, bg = "white")

combined_ok <- FALSE
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  ggsave(file.path(OUT_DIR, paste0(PREFIX, "combined.png")),
         p1 / p2, width = 9, height = 9.6, dpi = DPI, bg = "white")
  combined_ok <- TRUE
}

cat("\n==============================================================\n")
cat("POST 18 VISUALS v2 WRITTEN\n")
cat("==============================================================\n")
cat("  ", f1, "\n")
cat("  ", f2, "\n")
if (combined_ok) {
  cat("  ", file.path(OUT_DIR, paste0(PREFIX, "combined.png")), "\n")
} else {
  cat("   patchwork not installed. Separate PNGs only.\n")
}
cat("\nWATCH ITEMS ON RENDER:\n")
cat("  1. Panel 1: the navy bar sits inside the gold bar's span, near 2.4 to\n")
cat("     4.0. Confirm the two bars read as nested, not as two separate rows\n")
cat("     that happen to overlap.\n")
cat("  2. Panel 1: 'spans 1.55 points' sits right of the navy bar at x = 4.3.\n")
cat("     Confirm it does not collide with the gold bar on the row above.\n")
cat("  3. Panel 1: x limit is 15.4 to fit the 'spans 14 points' label.\n")
cat("     Confirm that label is not clipped at the right edge.\n")
cat("  4. Panel 2: the gold dashed line at 3.14 sits near the tallest bars.\n")
cat("     Confirm its label does not obscure the peak at 3.225.\n")
cat("  5. Panel 2 caption uses the division and square-root symbols. Confirm\n")
cat("     both render and do not appear as boxes.\n")
cat("==============================================================\n\n")
