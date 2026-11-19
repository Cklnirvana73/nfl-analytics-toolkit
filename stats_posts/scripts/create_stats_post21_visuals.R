################################################################################
# Statistics Series - Post 21: The Hypothesis Testing Framework
# File: create_stats_post21_visuals.R
#
# Purpose:
#   Two LinkedIn graphics for Post 21. All values are HARDCODED from the
#   verified console output of the Post 21 source pull (re-run of
#   compare_prediction_methods() on s2_week4_ab_panel.rds, seed 42,
#   n_bootstrap 2000). Nothing here re-simulates or re-derives. This is
#   deliberate: re-running inference in a visual script allows the plotted
#   numbers to drift from the published prose.
#
#   Panel 1: Which predictor won, per position, with bootstrap CIs.
#   Panel 2: The two verdicts on their two separate scales.
#
# CRITICAL DESIGN NOTE (Panel 2):
#   The test statistic and the effect size are different types of quantity and
#   are NEVER placed on a shared axis. They are two separate plots combined by
#   patchwork, each with its own x axis, its own units, and its own reference
#   line. Do not "simplify" this into one faceted plot with a shared scale.
#
# Source of every number: verified console output, 2026-08-13.
#   Panel counts : QB 1,242 | RB 8,123 | WR 44,691 | total 54,056
#   Seasons      : 2010-2025 (16)
#   Threshold    : 1.96 (alpha 0.05, two sided; crit t = 1.9619 / 1.9603 / 1.96)
#   Effect floor : Cohen's q = 0.10 (MDE set a priori)
#
# Output: stats_posts/output/plots/stats_post21_*.png at 300 DPI
# Author: Christian LeBlanc
################################################################################

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(scales)
library(here)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

OUTPUT_DIR  <- here("stats_posts", "output", "plots")
FILE_PREFIX <- "stats_post21_"
DPI         <- 300

# Fixed literal titles. Length asserted below so a clipped title fails the
# build rather than shipping truncated.
TITLE_P1 <- "The season average predicted next week better at all three positions"
TITLE_P2 <- "The test rejected the null. The effect stayed under the floor."

COL_STD    <- "#1B3A5C"  # season to date (the winner)
COL_ROLL3  <- "#8C9BAA"  # rolling 3 game
COL_THRESH <- "#C8860D"  # thresholds set in advance
COL_TEXT   <- "gray30"

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

theme_post <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = rel(1.15), hjust = 0),
      plot.subtitle   = element_text(color = COL_TEXT, size = rel(0.88), hjust = 0),
      plot.caption    = element_text(color = "gray55", size = rel(0.7), hjust = 1),
      panel.grid.minor = element_blank(),
      axis.title      = element_text(color = COL_TEXT, size = rel(0.85)),
      axis.text       = element_text(color = COL_TEXT),
      legend.position = "top",
      legend.title    = element_blank(),
      plot.margin     = margin(12, 16, 12, 12)
    )
}

# ==============================================================================
# VERIFIED DATA (hardcoded from console output -- do not recompute)
# ==============================================================================

results <- tibble::tibble(
  position   = c("QB", "RB", "WR"),
  n          = c(1242L, 8123L, 44691L),
  r_roll3    = c(0.3647, 0.6227, 0.5080),
  r_std      = c(0.4311, 0.6520, 0.5425),
  r3_lo      = c(0.3012, 0.5960, 0.4900),
  r3_hi      = c(0.4367, 0.6487, 0.5291),
  std_lo     = c(0.3627, 0.6258, 0.5260),
  std_hi     = c(0.4993, 0.6763, 0.5639),
  t_abs      = c(4.9442, 8.9856, 19.1698),
  cohens_q   = c(0.0789, 0.0493, 0.0477)
)

THRESHOLD_T <- 1.96
FLOOR_Q     <- 0.10
N_TOTAL     <- 54056L

# ==============================================================================
# PRE-BUILD ASSERTIONS (build fails loudly rather than shipping a wrong panel)
# ==============================================================================

stopifnot(
  "Row count is not 3 positions" =
    nrow(results) == 3L,
  "Position n values do not sum to the published 54,056" =
    sum(results$n) == N_TOTAL,
  "Season-to-date must win at every position (r_std > r_roll3)" =
    all(results$r_std > results$r_roll3),
  "Every test statistic must exceed the 1.96 threshold" =
    all(results$t_abs > THRESHOLD_T),
  "Every effect size must fall under the 0.10 floor" =
    all(results$cohens_q < FLOOR_Q),
  "QB must carry the smallest n and the largest effect" =
    results$n[results$position == "QB"] == min(results$n) &&
    results$cohens_q[results$position == "QB"] == max(results$cohens_q),
  "Bootstrap CIs must bracket their point estimates" =
    all(results$r3_lo  < results$r_roll3 & results$r_roll3 < results$r3_hi) &&
    all(results$std_lo < results$r_std   & results$r_std   < results$std_hi),
  "No NA values in plotted data" =
    all(!is.na(unlist(results[, sapply(results, is.numeric)]))),
  "Panel 1 title exceeds safe render width" =
    nchar(TITLE_P1) <= 78,
  "Panel 2 title exceeds safe render width" =
    nchar(TITLE_P2) <= 78
)

message("All pre-build assertions passed.")

# ==============================================================================
# PANEL 1: Which predictor won, per position
#
# Encoding rationale: the finding is a DIRECTIONAL gap, so a dumbbell is the
# right encoding -- the segment length IS the finding and the arrowless
# left-to-right read shows which side won. Bootstrap CIs are drawn as thin
# rules behind the points so the reader can see the intervals separate.
# Positions are ordered by sample size so the label text stays honest.
# ==============================================================================

p1_long <- results %>%
  mutate(pos_label = paste0(position, "  (n = ", comma(n), ")")) %>%
  mutate(pos_label = factor(pos_label, levels = rev(
    paste0(position, "  (n = ", comma(n), ")")
  )))

p1_points <- p1_long %>%
  select(pos_label, r_roll3, r_std) %>%
  pivot_longer(c(r_roll3, r_std), names_to = "predictor", values_to = "r") %>%
  mutate(predictor = factor(
    predictor,
    levels = c("r_roll3", "r_std"),
    labels = c("Rolling 3 game average", "Season to date average")
  ))

p1_ci <- p1_long %>%
  select(pos_label, r3_lo, r3_hi, std_lo, std_hi) %>%
  pivot_longer(
    everything() & !pos_label,
    names_to = c("predictor", "bound"),
    names_pattern = "(r3|std)_(lo|hi)",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = bound, values_from = value) %>%
  mutate(predictor = factor(
    predictor,
    levels = c("r3", "std"),
    labels = c("Rolling 3 game average", "Season to date average")
  ))

p1 <- ggplot() +
  geom_segment(
    data = p1_long,
    aes(x = r_roll3, xend = r_std, y = pos_label, yend = pos_label),
    color = "gray75", linewidth = 1.6
  ) +
  geom_linerange(
    data = p1_ci,
    aes(y = pos_label, xmin = lo, xmax = hi, color = predictor),
    linewidth = 0.6, alpha = 0.55,
    position = position_nudge(y = 0.16)
  ) +
  geom_point(
    data = p1_points,
    aes(x = r, y = pos_label, color = predictor),
    size = 5
  ) +
  scale_color_manual(values = c(
    "Rolling 3 game average"  = COL_ROLL3,
    "Season to date average"  = COL_STD
  )) +
  scale_x_continuous(
    limits = c(0.28, 0.70),
    breaks = seq(0.30, 0.70, by = 0.05)
  ) +
  labs(
    title    = TITLE_P1,
    subtitle = paste0(
      "Correlation with next week's PPR points | 54,056 player weeks, ",
      "2010-2025 | thin rules are bootstrap 95% CIs"
    ),
    x = "Correlation with next week's points (higher = better predictor)",
    y = NULL,
    caption = "Data: nflfastR via NFL Analytics Toolkit"
  ) +
  theme_post()

# ==============================================================================
# PANEL 2: Two verdicts, two scales
#
# CRITICAL: test statistic and effect size are DIFFERENT TYPES. They get two
# separate plots with two separate x axes, combined by patchwork. They are
# never placed on a shared axis even though both are unitless decimals.
# ==============================================================================

p2_order <- results %>%
  mutate(pos_label = factor(position, levels = c("WR", "RB", "QB")))

p2a <- ggplot(p2_order, aes(x = t_abs, y = pos_label)) +
  geom_vline(
    xintercept = THRESHOLD_T, color = COL_THRESH,
    linewidth = 1.1, linetype = "dashed"
  ) +
  annotate(
    "text", x = THRESHOLD_T, y = 3.62,
    label = "Threshold set\nin advance: 1.96",
    hjust = -0.08, vjust = 1, size = 3.3, color = COL_THRESH, lineheight = 0.95
  ) +
  geom_segment(
    aes(x = THRESHOLD_T, xend = t_abs, y = pos_label, yend = pos_label),
    color = COL_STD, linewidth = 1.2
  ) +
  geom_point(size = 5, color = COL_STD) +
  geom_text(
    aes(label = sprintf("%.2f", t_abs)),
    vjust = -1.3, size = 3.5, color = COL_STD, fontface = "bold"
  ) +
  scale_x_continuous(limits = c(0, 23), breaks = seq(0, 20, by = 5)) +
  # Headroom for the threshold annotation must come from the DISCRETE scale.
  # coord_cartesian(ylim = ...) takes continuous limits and errors here.
  scale_y_discrete(expand = expansion(add = c(0.6, 1.0))) +
  coord_cartesian(clip = "off") +
  labs(
    subtitle = "Is there a difference?  Yes, at every position.",
    x = "Test statistic, distance from zero",
    y = NULL
  ) +
  theme_post()

p2b <- ggplot(p2_order, aes(x = cohens_q, y = pos_label)) +
  geom_vline(
    xintercept = FLOOR_Q, color = COL_THRESH,
    linewidth = 1.1, linetype = "dashed"
  ) +
  annotate(
    "text", x = FLOOR_Q, y = 3.62,
    label = "Smallest effect worth\ndetecting, set in advance: 0.10",
    hjust = 1.06, vjust = 1, size = 3.3, color = COL_THRESH, lineheight = 0.95
  ) +
  geom_segment(
    aes(x = 0, xend = cohens_q, y = pos_label, yend = pos_label),
    color = COL_ROLL3, linewidth = 1.2
  ) +
  geom_point(size = 5, color = COL_ROLL3) +
  geom_text(
    aes(label = sprintf("%.4f", cohens_q)),
    vjust = -1.3, size = 3.5, color = "gray35", fontface = "bold"
  ) +
  scale_x_continuous(limits = c(0, 0.125), breaks = seq(0, 0.12, by = 0.03)) +
  # Same discrete-scale expansion as p2a; keeps both panels vertically aligned.
  scale_y_discrete(expand = expansion(add = c(0.6, 1.0))) +
  coord_cartesian(clip = "off") +
  labs(
    subtitle = "Is it big enough to matter?  Not at any position.",
    x = "Effect size for comparing correlations (separate scale)",
    y = NULL
  ) +
  theme_post()

p2 <- (p2a | p2b) +
  plot_annotation(
    title    = TITLE_P2,
    subtitle = paste0(
      "Two questions, two pre-set bars, two different scales | ",
      "QB n = 1,242 carries the smallest sample and the largest effect"
    ),
    caption  = "Data: nflfastR via NFL Analytics Toolkit",
    theme    = theme_post()
  )

# ==============================================================================
# EXPORT
# ==============================================================================

path_p1 <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "predictor_comparison.png"))
path_p2 <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "two_verdicts.png"))

ggsave(path_p1, plot = p1, width = 10, height = 5.6, dpi = DPI, bg = "white")
ggsave(path_p2, plot = p2, width = 11, height = 5.4, dpi = DPI, bg = "white")

# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat("\n", strrep("=", 62), "\n", sep = "")
cat("Post 21 visuals written\n")
cat(strrep("=", 62), "\n", sep = "")
cat("  Panel 1 :", path_p1, "\n")
cat("  Panel 2 :", path_p2, "\n")
cat("  DPI     :", DPI, "\n\n")
cat("Reconciliation against published prose:\n")
for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  cat(sprintf(
    "  %-2s  n = %6s | r_std %.4f > r_roll3 %.4f | |t| %6.2f > 1.96 | q %.4f < 0.10\n",
    r$position, comma(r$n), r$r_std, r$r_roll3, r$t_abs, r$cohens_q
  ))
}
cat("\n  Total player weeks:", comma(sum(results$n)), "(prose states 54,056)\n\n")

cat("RENDER WATCH ITEMS (check the PNGs before posting):\n")
cat("  1. Panel 1: do the two CI rules visibly separate at every position?\n")
cat("     If they overlap heavily at QB, the nudge may need widening.\n")
cat("  2. Panel 1: is the x range 0.28-0.70 wide enough that the QB CI\n")
cat("     lower bound (0.3012) is not touching the axis edge?\n")
cat("  3. Panel 1: legend at top, two entries, neither clipped.\n")
cat("  4. Panel 2: both threshold annotations fully inside the panel.\n")
cat("     Left one extends right, right one extends left; check neither\n")
cat("     collides with the WR point or the panel border.\n")
cat("  5. Panel 2: the 19.17 label at the far right is not clipped.\n")
cat("  6. Panel 2: the two x axis titles read clearly as DIFFERENT units.\n")
cat("     If a reader could mistake them for one scale, the panel fails.\n")
cat("  7. Both: no title clipped at either width.\n")
cat("  8. Both: 5-second test. Panel 1 should read as navy-beats-gray at\n")
cat("     every row. Panel 2 should read as bars-past-the-line on the left\n")
cat("     and bars-short-of-the-line on the right.\n\n")
