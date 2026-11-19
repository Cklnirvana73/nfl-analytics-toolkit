################################################################################
# Statistics Series -- Post 22: p-values
# Two-panel visual: (1) the correlation gap by position, (2) the p-value
# magnitude (zero-count) by position with row count labeled, showing the
# p-value ordering tracks row count, not gap size.
#
# All numbers below are hardcoded from this session's verified console output
# (pull_stats_post22_pvalue_data.R). No re-simulation, no re-derivation from
# raw data. If the source pull is ever re-run, reconcile this block against
# the new output before touching anything downstream.
#
# Verified source values (Post 22 session):
#   r_roll3: QB 0.3647 | RB 0.6227 | WR 0.5080
#   r_std:   QB 0.4311 | RB 0.6520 | WR 0.5425
#   n_player_weeks: QB 1,242 | RB 8,123 | WR 44,691
#   p_value_raw: QB 8.695586e-07 | RB 3.157261e-19 | WR 1.407168e-81
#   zero count (digits after decimal before first significant digit):
#     QB 6 | RB 18 | WR 80
#
# Output: stats_posts/output/plots/stats_post22_pvalues.png (300 DPI)
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(glue)
library(here)

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

OUTPUT_DIR <- here::here("stats_posts", "output", "plots")
OUTPUT_FILE <- file.path(OUTPUT_DIR, "stats_post22_pvalues.png")

COLOR_STD   <- "#0072B2"  # season-to-date average (Okabe-Ito blue)
COLOR_ROLL3 <- "#E69F00"  # rolling 3-game average (Okabe-Ito gold)
# Gold substituted for the roll3 series rather than red, consistent with
# Posts 16/17/20/21: neither predictor is "wrong" here, one simply tracked
# the outcome less closely. Series-wide red-vs-gold convention remains
# unresolved and is applied here per established recent-post practice.

# ------------------------------------------------------------------------------
# HARDCODED DATA (verified console output, Post 22 pull script)
# ------------------------------------------------------------------------------

gap_data <- tibble(
  position = c("Quarterback", "Running Back", "Receiver"),
  pos_short = factor(c("QB", "RB", "WR"), levels = c("QB", "RB", "WR")),
  r_roll3  = c(0.3647, 0.6227, 0.5080),
  r_std    = c(0.4311, 0.6520, 0.5425),
  n        = c(1242L, 8123L, 44691L)
) %>%
  mutate(gap = round(r_std - r_roll3, 4))

pval_data <- tibble(
  pos_short  = factor(c("QB", "RB", "WR"), levels = c("QB", "RB", "WR")),
  n          = c(1242L, 8123L, 44691L),
  zero_count = c(6L, 18L, 80L),
  p_raw      = c(8.695586e-07, 3.157261e-19, 1.407168e-81)
)

# ------------------------------------------------------------------------------
# PRE-BUILD ASSERTIONS
# ------------------------------------------------------------------------------

stopifnot(
  "gap_data must have exactly 3 rows (QB, RB, WR)" =
    nrow(gap_data) == 3,
  "pval_data must have exactly 3 rows (QB, RB, WR)" =
    nrow(pval_data) == 3,
  "All correlations must be in [0, 1]" =
    all(gap_data$r_roll3 >= 0 & gap_data$r_roll3 <= 1) &&
    all(gap_data$r_std   >= 0 & gap_data$r_std   <= 1),
  "gap column must equal r_std - r_roll3 exactly (to 4 dp)" =
    all(gap_data$gap == round(gap_data$r_std - gap_data$r_roll3, 4)),
  "QB must hold the widest gap of the three" =
    gap_data$gap[gap_data$pos_short == "QB"] == max(gap_data$gap),
  "RB must hold the narrowest gap of the three" =
    gap_data$gap[gap_data$pos_short == "RB"] == min(gap_data$gap),
  "QB must hold the smallest zero-count (least extreme p-value)" =
    pval_data$zero_count[pval_data$pos_short == "QB"] == min(pval_data$zero_count),
  "WR must hold the largest zero-count (most extreme p-value)" =
    pval_data$zero_count[pval_data$pos_short == "WR"] == max(pval_data$zero_count),
  "zero_count ordering must match n ordering exactly (the post's claim)" =
    identical(
      order(pval_data$n),
      order(pval_data$zero_count)
    ),
  "n values must match between gap_data and pval_data" =
    all(gap_data$n == pval_data$n[match(gap_data$pos_short, pval_data$pos_short)]),
  "No NA cells in either data frame" =
    sum(is.na(gap_data)) == 0 && sum(is.na(pval_data)) == 0
)

cat("Pre-build assertions passed: 10/10\n")

# ------------------------------------------------------------------------------
# PANEL 1: The correlation gap by position (dumbbell)
# ------------------------------------------------------------------------------

panel1_data <- gap_data %>%
  tidyr::pivot_longer(
    cols = c(r_roll3, r_std),
    names_to = "method",
    values_to = "r"
  ) %>%
  mutate(
    method_label = if_else(method == "r_std", "Season average", "Last 3 games"),
    method_label = factor(method_label, levels = c("Last 3 games", "Season average"))
  )

# Order positions by gap size, widest at top (ggplot draws bottom-to-top,
# so largest gap needs to be the LAST factor level to render at the top).
gap_order <- gap_data %>% arrange(gap) %>% pull(pos_short) %>% as.character()
panel1_data <- panel1_data %>%
  mutate(pos_short = factor(pos_short, levels = gap_order))
gap_data_ord <- gap_data %>%
  mutate(pos_short = factor(pos_short, levels = gap_order))

p1 <- ggplot() +
  geom_segment(
    data = gap_data_ord,
    aes(x = r_roll3, xend = r_std, y = pos_short, yend = pos_short),
    color = "gray70", linewidth = 1
  ) +
  geom_point(
    data = panel1_data,
    aes(x = r, y = pos_short, color = method_label),
    size = 4
  ) +
  geom_text(
    data = gap_data_ord,
    aes(x = pmax(r_roll3, r_std) + 0.025, y = pos_short,
        label = glue("gap {sprintf('%.4f', gap)}")),
    hjust = 0, size = 3.4, color = "gray30"
  ) +
  scale_color_manual(values = c("Last 3 games" = COLOR_ROLL3,
                                "Season average" = COLOR_STD)) +
  scale_x_continuous(limits = c(0.30, 0.78), breaks = seq(0.3, 0.7, 0.1)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 0.6))) +
  labs(
    title = "The season average predicted better at every position",
    subtitle = "Correlation with next week's score, 2010\u20132025 \u00b7 gap = season avg minus last-3 avg",
    x = "Correlation (0 to 1)",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = rel(1.05)),
    plot.subtitle = element_text(color = "gray40", size = rel(0.82))
  )

# ------------------------------------------------------------------------------
# PANEL 2: The p-value magnitude (zero count) by position, n labeled
# ------------------------------------------------------------------------------

# Keep QB / RB / WR in the SAME left-to-right order as Panel 1's row order
# reversed to a natural x-axis reading order (QB, RB, WR), so the reader can
# see the gap ranking (Panel 1) does not predict the zero-count ranking here.
pval_data_ord <- pval_data %>%
  mutate(pos_short = factor(pos_short, levels = c("QB", "RB", "WR")))

p2 <- ggplot(pval_data_ord, aes(x = pos_short, y = zero_count)) +
  geom_col(fill = COLOR_STD, width = 0.55) +
  geom_text(
    aes(label = glue("{zero_count} zeros")),
    vjust = -0.6, size = 3.6, fontface = "bold", color = "gray20"
  ) +
  geom_text(
    aes(y = -6, label = glue("n = {format(n, big.mark = ',')}")),
    vjust = 1, size = 3.1, color = "gray45"
  ) +
  scale_y_continuous(
    limits = c(-12, 90),
    breaks = c(0, 20, 40, 60, 80)
  ) +
  labs(
    title = "But the p-values ranked by row count, not by that gap",
    subtitle = "Zeros after the decimal point before the first digit, by position",
    x = NULL,
    y = "Zero count (higher = more extreme p-value)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = rel(1.05)),
    plot.subtitle = element_text(color = "gray40", size = rel(0.82)),
    axis.text.x = element_text(face = "bold", size = rel(1.05))
  )

# ------------------------------------------------------------------------------
# COMBINE AND EXPORT
# ------------------------------------------------------------------------------

combined <- (p1 / p2) +
  plot_annotation(
    caption = "NFL Analytics Toolkit \u00b7 54,056 player-weeks, 2010\u20132025"
  ) &
  theme(plot.caption = element_text(color = "gray50", size = rel(0.75)))

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

ggsave(
  filename = OUTPUT_FILE,
  plot     = combined,
  width    = 8,
  height   = 8.5,
  dpi      = 300
)

# ------------------------------------------------------------------------------
# CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat(strrep("=", 60), "\n")
cat("POST 22 VISUAL BUILD COMPLETE\n")
cat(strrep("=", 60), "\n")
cat(glue("Saved: {OUTPUT_FILE}"), "\n")
cat("Panel 1: correlation gap by position (dumbbell), ordered by gap size\n")
cat("Panel 2: p-value zero-count by position, n labeled beneath each bar\n\n")
cat("Reconciliation:\n")
print(gap_data %>% select(pos_short, r_roll3, r_std, gap, n))
print(pval_data %>% select(pos_short, n, zero_count))
cat("\nKey check: zero-count order (QB<RB<WR) matches n order (QB<RB<WR).\n")
cat("Gap order is QB>WR>RB -- does NOT match. That mismatch is the post.\n")
