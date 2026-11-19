# =============================================================================
# Statistics Series Post 20: The Central Limit Theorem
# Visual script v2
#
# WHY v2
#   v1 Panel 2 plotted predicted and observed standard error as paired bars.
#   The finding was that the two values MATCH, and paired bars are the worst
#   possible encoding for sameness: the reader sees eight descending bars and
#   four near-identical pairs with no extractable insight. That is a 5-second
#   rule failure. v1 Panel 1 also omitted the starting population, so the
#   reader never saw the two-spike distribution the whole post opens on.
#
# TWO PANELS, MATCHED TO THE POST'S NARRATIVE
#   Panel 1: The progression. Row 1 is the raw population (two spikes at 0
#            and 1). Rows 2 to 5 are the sampling distributions at n = 5, 10,
#            30, 100. Shared x axis. An amber bracket on each row marks the
#            spread the formula predicted. The reader watches the distribution
#            collapse toward 0.402, sees the amber bracket track it at every
#            step, and sees the shape stay lumpy to the end. Both halves of
#            the post in one image.
#
#   Panel 2: The width half alone, as a decay curve. Amber line is the formula
#            evaluated continuously across sample size. Navy points are what
#            resampling actually produced. Points sitting on a line reads as
#            "the formula traces the data" instantly, which paired bars did
#            not.
#
# DATA PROVENANCE
#   All summary values HARDCODED from verified console output of
#   pull_stats_post20_clt_data.R (seed 42, 2000 draws, 2025 regular season).
#   Panel 1 histograms are redrawn from a population reconstructed out of the
#   verified counts, then asserted against the hardcoded values before any
#   plot is built.
#
# COLORS
#   Navy  = observed (what resampling produced)
#   Amber = predicted (what the formula said to expect)
#   Neither is wrong, so no error color appears in this script.
# =============================================================================

library(ggplot2)
library(dplyr)

set.seed(42L)

# ---- CONFIGURATION ----------------------------------------------------------

OUTPUT_DIR  <- file.path("stats_posts", "output", "plots")
FILE_PREFIX <- "stats_post20_"
DPI         <- 300L

COL_OBSERVED  <- "#1F3864"   # navy
COL_PREDICTED <- "#C8892B"   # amber
COL_NEUTRAL   <- "gray45"

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# ---- VERIFIED CONSTANTS FROM CONSOLE OUTPUT ---------------------------------

POP_N         <- 6801L
POP_CONVERTED <- 2733L
POP_FAILED    <- 4068L
POP_MEAN      <- 0.4019
POP_SD        <- 0.4903
N_DRAWS       <- 2000L

verified <- data.frame(
  n                   = c(5L, 10L, 30L, 100L),
  mean_of_means       = c(0.3992, 0.3959, 0.4032, 0.4022),
  observed_se         = c(0.2188, 0.1518, 0.0888, 0.0479),
  predicted_se        = c(0.2193, 0.1550, 0.0895, 0.0490),
  skewness            = c(0.1752, 0.1173, 0.0734, 0.0290),
  n_distinct_observed = c(6L, 10L, 18L, 31L),
  stringsAsFactors    = FALSE
)

# ---- PRE-BUILD ASSERTIONS ---------------------------------------------------

stopifnot(
  "Population counts do not sum to population size" =
    POP_CONVERTED + POP_FAILED == POP_N,

  "POP_MEAN does not reconcile to the conversion count" =
    abs(POP_CONVERTED / POP_N - POP_MEAN) < 0.001,

  "POP_SD does not match sqrt(p*(1-p)) for a Bernoulli" =
    abs(sqrt(POP_MEAN * (1 - POP_MEAN)) - POP_SD) < 0.001,

  "predicted_se does not equal POP_SD / sqrt(n) at every n" =
    all(abs(verified$predicted_se - POP_SD / sqrt(verified$n)) < 0.001),

  "Post claims n=5 formula accurate within half a percent" =
    abs(verified$observed_se[1] / verified$predicted_se[1] - 1) < 0.005,

  "Post claims skewness falls monotonically as n grows" =
    all(diff(verified$skewness) < 0),

  "Post claims skewness runs 0.175 down to 0.029" =
    abs(verified$skewness[1] - 0.175) < 0.001 &&
    abs(verified$skewness[4] - 0.029) < 0.001,

  "Post claims 31 distinct values at n=100 out of 101 possible" =
    verified$n_distinct_observed[4] == 31L,

  "Post claims exactly six possible values at n=5" =
    verified$n_distinct_observed[1] == 6L,

  "Observed SE must fall as n rises" =
    all(diff(verified$observed_se) < 0)
)

cat("All pre-build assertions passed.\n\n")

# ---- REBUILD SAMPLING DISTRIBUTIONS -----------------------------------------

pop_values <- c(rep(1L, POP_CONVERTED), rep(0L, POP_FAILED))

stopifnot(
  "Reconstructed population is the wrong size" = length(pop_values) == POP_N,
  "Reconstructed population mean does not match verified mean" =
    abs(mean(pop_values) - POP_MEAN) < 0.001
)

draw_frames <- lapply(verified$n, function(n) {
  draws <- replicate(N_DRAWS, mean(sample(pop_values, size = n, replace = FALSE)))
  data.frame(n = n, value = draws, stringsAsFactors = FALSE)
})

sampling_data <- do.call(rbind, draw_frames)

redraw_check <- sampling_data %>%
  group_by(n) %>%
  summarise(obs_se = sd(value), .groups = "drop") %>%
  arrange(n)

stopifnot(
  "Redraw does not reproduce the verified observed SE values" =
    all(abs(redraw_check$obs_se - verified$observed_se) < 0.005)
)

cat("Redraw reproduces verified console values.\n\n")

# ---- ASSEMBLE PANEL 1 DATA: POPULATION ROW PLUS SAMPLING ROWS ---------------
# Row 1 is the raw population: every individual play, which is either 0 or 1.
# This is the distribution the post opens on and v1 never showed.

population_row <- data.frame(
  n     = 1L,
  value = as.numeric(pop_values),
  stringsAsFactors = FALSE
)

panel1_data <- rbind(population_row, sampling_data)

row_labels <- c(
  "1"   = "Single plays\nevery value is 0 or 1",
  "5"   = "Averages of 5 plays\n6 distinct averages",
  "10"  = "Averages of 10 plays\n10 distinct averages",
  "30"  = "Averages of 30 plays\n18 distinct averages",
  "100" = "Averages of 100 plays\n31 distinct averages"
)

# Assert the label counts match the verified distinct-value counts so the
# labels cannot drift from the data they describe.
stopifnot(
  "Row label distinct counts do not match verified values" =
    all(vapply(seq_len(nrow(verified)), function(i) {
      grepl(paste0("\n", verified$n_distinct_observed[i], " distinct"),
            row_labels[as.character(verified$n[i])], fixed = TRUE)
    }, logical(1)))
)

panel1_data$row_label <- factor(
  row_labels[as.character(panel1_data$n)],
  levels = row_labels[c("1", "5", "10", "30", "100")]
)

# Amber brackets: one standard error either side of the true rate, using the
# PREDICTED standard error. Population row gets the play-level SD instead,
# since a single play has no sample size to divide by.
bracket_data <- data.frame(
  n        = c(1L, verified$n),
  half_width = c(POP_SD, verified$predicted_se),
  stringsAsFactors = FALSE
)
bracket_data$xmin <- POP_MEAN - bracket_data$half_width
bracket_data$xmax <- POP_MEAN + bracket_data$half_width
bracket_data$row_label <- factor(
  row_labels[as.character(bracket_data$n)],
  levels = row_labels[c("1", "5", "10", "30", "100")]
)

# ---- PANEL 1 ----------------------------------------------------------------

title_p1 <- "Averaging collapses the spread on schedule while the shape stays lumpy"
stopifnot("Panel 1 title too long for render" = nchar(title_p1) <= 95)

p1 <- ggplot(panel1_data, aes(x = value)) +
  geom_histogram(bins = 60, fill = COL_OBSERVED, color = NA) +
  geom_errorbarh(
    data = bracket_data,
    aes(xmin = xmin, xmax = xmax, y = 0),
    inherit.aes = FALSE,
    height = 0, linewidth = 1.6, color = COL_PREDICTED
  ) +
  geom_vline(xintercept = POP_MEAN, linetype = "dashed",
             color = COL_NEUTRAL, linewidth = 0.5) +
  facet_wrap(~ row_label, ncol = 1, scales = "free_y", strip.position = "right") +
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(breaks = c(0, 0.2, 0.402, 0.6, 0.8, 1),
                     labels = c("0", "0.2", "0.402", "0.6", "0.8", "1")) +
  labs(
    title    = title_p1,
    subtitle = paste0(
      "Navy is what resampling produced. Amber marks the spread the formula ",
      "predicted, one standard error\neither side of the true rate. ",
      "2,000 samples at each size, from 6,801 third downs in the 2025 regular season."
    ),
    x = "Third-down conversion rate",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = rel(1.15), hjust = 0),
    plot.subtitle      = element_text(color = "gray40", size = rel(0.8), hjust = 0),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_blank(),
    strip.text.y.right = element_text(angle = 0, hjust = 0, size = rel(0.8),
                                      face = "bold", color = "gray25"),
    axis.title         = element_text(color = "gray30"),
    plot.margin        = margin(12, 16, 12, 12)
  )

# NOTE: x scale is FIXED across rows, which is the comparison axis and the
# entire point. y uses free scales only because row 1 counts 6,801 plays while
# the other rows count 2,000 samples: different quantities, and the reference
# rule against free scales applies to the axis being compared, not to raw
# counts on an axis whose values are never read. Y labels are removed for
# that reason.

ggsave(
  filename = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "clt_progression.png")),
  plot     = p1,
  width    = 9, height = 10, dpi = DPI
)

# ---- PANEL 2: WIDTH AS A DECAY CURVE ----------------------------------------
# Replaces v1's paired bars. The formula is drawn as a continuous curve and
# the observed values as points on top of it. Points landing on a line is an
# instantly readable encoding for "the prediction tracks reality."

curve_data <- data.frame(n = seq(2, 110, by = 0.5))
curve_data$predicted <- POP_SD / sqrt(curve_data$n)

points_data <- data.frame(
  n        = verified$n,
  observed = verified$observed_se,
  label    = sprintf("%.4f", verified$observed_se),
  stringsAsFactors = FALSE
)

title_p2 <- "The observed spread landed on the predicted curve at every sample size"
stopifnot("Panel 2 title too long for render" = nchar(title_p2) <= 95)

p2 <- ggplot() +
  geom_line(data = curve_data, aes(x = n, y = predicted),
            color = COL_PREDICTED, linewidth = 1.4) +
  geom_point(data = points_data, aes(x = n, y = observed),
             color = COL_OBSERVED, size = 4.2) +
  geom_text(data = points_data, aes(x = n, y = observed, label = label),
            vjust = -1.4, size = 3.4, color = COL_OBSERVED) +
  annotate("text", x = 78, y = 0.185,
           label = "Amber curve: play-level standard\ndeviation divided by the square\nroot of the sample size",
           hjust = 0, size = 3.4, color = COL_PREDICTED, lineheight = 1.15) +
  coord_cartesian(xlim = c(0, 112), ylim = c(0, 0.30)) +
  scale_x_continuous(breaks = c(5, 10, 30, 50, 100)) +
  labs(
    title    = title_p2,
    subtitle = "Navy points are the spread of 2,000 sample averages at each size",
    x = "Plays per sample",
    y = "Spread of the sample averages"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = rel(1.1), hjust = 0),
    plot.subtitle      = element_text(color = "gray40", size = rel(0.85), hjust = 0),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title         = element_text(color = "gray30"),
    plot.margin        = margin(12, 16, 12, 12)
  )

ggsave(
  filename = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "se_decay_curve.png")),
  plot     = p2,
  width    = 9, height = 5.5, dpi = DPI
)

# ---- CONSOLE SUMMARY --------------------------------------------------------

cat("=============================================================\n")
cat("POST 20 VISUALS v2 WRITTEN\n")
cat("=============================================================\n")
cat("  ", file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "clt_progression.png")), "\n")
cat("  ", file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "se_decay_curve.png")), "\n\n")

cat("SUPERSEDED by this run (delete if present):\n")
cat("   stats_post20_sampling_distributions.png\n")
cat("   stats_post20_se_predicted_vs_observed.png\n\n")

cat("RENDER WATCH ITEMS\n")
cat("  1. Panel 1 row 1 must show TWO spikes only, at 0 and at 1, nothing between.\n")
cat("  2. Panel 1 rows must narrow visibly from top to bottom. If they do not,\n")
cat("     the x axis is not fixed and the panel is wrong.\n")
cat("  3. Panel 1 amber bracket should hug the navy mass on every row.\n")
cat("  4. Panel 1 bottom row should still look granular, not a smooth curve.\n")
cat("  5. Panel 1 right-side strip labels must not be cut off at 9in width.\n")
cat("  6. Panel 2 navy points must sit ON the amber curve, not beside it.\n")
cat("  7. Panel 2 annotation must not collide with the curve or the points.\n")
cat("  8. Both titles render on one line without truncation.\n\n")
