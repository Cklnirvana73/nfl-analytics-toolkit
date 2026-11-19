# =============================================================================
# create_stats_post24_visuals.R
# Statistics Series, Post 24: Type I Error
#
# ONE PANEL. The finding: the false positive rate is not a property of the
# data, it is the threshold position. Observed rate is plotted against the
# rate that was set, with an identity reference line. Points landing on the
# line is the finding, visible in under 5 seconds.
#
# ENCODING NOTE: this is a "these two things match" finding. Paired bars are
# the wrong encoding for sameness (Post 20 v1 failure). Points on a reference
# line read as "tracks" immediately.
#
# ALL NUMBERS HARDCODED from the verified console output of
# pull_stats_post24_type1_data.R. Nothing is re-simulated. The script does
# not load the panel and does not call any project function.
#
# NOTE ON OUTPUT SIZE: project spec says 1200x800; the code-review standard
# says 300 DPI. 8 x 5.333 at 300 DPI gives 2400x1600, which downscales to
# 1200x800 cleanly. Flagged rather than silently chosen.
# =============================================================================

library(ggplot2)
library(dplyr)
library(scales)

# ------------------------------------------------------------- CONFIGURATION

OUTPUT_DIR  <- file.path("stats_posts", "output", "plots")
FILE_PREFIX <- "stats_post24_"

COL_ACTUAL <- "#0072B2"   # navy, observed values
COL_REF    <- "#E69F00"   # gold, the set rate (a reference, not a wrong value)
COL_TEXT   <- "gray30"

PLOT_W   <- 8
PLOT_H   <- 5.333
PLOT_DPI <- 300

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# --------------------------------------------- VERIFIED DATA (HARDCODED)
# Source: pull_stats_post24_type1_data.R console output.
# N_REPS = 20,000; N_OBS = 1,000; df = 997; true difference = 0.

N_REPS <- 20000L

sweep <- tibble::tibble(
  alpha_set   = c(0.01,   0.05,   0.10),
  crit_value  = c(2.5808, 1.9623, 1.6464),
  rejections  = c(188L,   1002L,  2026L),
  rate_obs    = c(0.0094, 0.0501, 0.1013)
)

# ------------------------------------------------------- PRE-BUILD ASSERTIONS

cat("Pre-build assertions:\n")

stopifnot(nrow(sweep) == 3L)
cat("  1 PASS: three threshold conditions present.\n")

# Rates must reconcile to counts over N_REPS at 4dp.
recomputed <- round(sweep$rejections / N_REPS, 4)
stopifnot(all(abs(recomputed - sweep$rate_obs) < 1e-9))
cat("  2 PASS: every observed rate reconciles to its count over",
    format(N_REPS, big.mark = ","), "runs.\n")

# The central claim: observed rate tracks the set rate within Monte Carlo error.
se <- sqrt(sweep$alpha_set * (1 - sweep$alpha_set) / N_REPS)
within <- abs(sweep$rate_obs - sweep$alpha_set) <= 1.96 * se
stopifnot(all(within))
cat("  3 PASS: all three observed rates within 1.96 SE of the rate set.\n")

# Critical values must be strictly decreasing as alpha loosens.
stopifnot(all(diff(sweep$crit_value) < 0))
cat("  4 PASS: critical value falls as alpha rises.\n")

# Rejections must be strictly increasing as alpha loosens.
stopifnot(all(diff(sweep$rejections) > 0))
cat("  5 PASS: rejection count rises as alpha rises.\n")

# The alpha 0.05 row must carry the figure quoted in the post.
main <- sweep %>% filter(alpha_set == 0.05)
stopifnot(nrow(main) == 1L, main$rejections == 1002L, main$rate_obs == 0.0501)
cat("  6 PASS: the 1,002 / 0.0501 figures quoted in the post are present.\n")

# Every plotted rate must be a false positive rate: true difference is zero.
stopifnot(all(sweep$rate_obs > 0), all(sweep$rate_obs < 1))
cat("  7 PASS: all plotted rates in (0, 1).\n")

# Axis bound must contain every point plus the identity line endpoint.
AX_MAX <- 0.11
stopifnot(max(sweep$alpha_set) <= AX_MAX, max(sweep$rate_obs) <= AX_MAX)
cat("  8 PASS: axis bound", AX_MAX, "contains all plotted values.\n")

# Guard the label column before it is built.
stopifnot(all(!is.na(sweep$rejections)))
cat("  9 PASS: no missing rejection counts.\n\n")

# --------------------------------------------------------------- LABEL BUILD

# Annotation must not sit on the identity line it describes. The line runs
# y = x, so a label at (ax, ay) is struck through when ay is near ax. Require
# clear vertical separation at the annotation's x position.
ANNO_X <- 0.004
ANNO_Y <- 0.094
stopifnot(
  "Gold annotation overlaps the identity line it labels" =
    abs(ANNO_Y - ANNO_X) > 0.03
)
cat("  11 PASS: annotation clears the identity line by",
    round(abs(ANNO_Y - ANNO_X), 3), "on the y scale.\n\n")

sweep <- sweep %>%
  mutate(
    point_label = paste0(
      format(rejections, big.mark = ","), " of ",
      format(N_REPS, big.mark = ",")
    )
  )

# Minimum pre-delivery visualization assertion, run against the object that
# is actually drawn rather than against the source figures above.
stopifnot(
  "Chart has no non-NA values on the plotted y aesthetic" =
    sum(!is.na(sweep$rate_obs)) > 0,
  "Chart has no non-NA values on the plotted x aesthetic" =
    sum(!is.na(sweep$alpha_set)) > 0,
  "Chart has fewer than 2 data points" =
    nrow(sweep) >= 2,
  "Point labels failed to build" =
    all(nzchar(sweep$point_label)) && !any(is.na(sweep$point_label))
)
cat("  10 PASS: plot data non-empty, non-NA on both aesthetics, labels built.\n\n")

# ---------------------------------------------------------------- THE PANEL

p <- ggplot(sweep, aes(x = alpha_set, y = rate_obs)) +

  # Reference line UNDER the data.
  geom_abline(
    slope = 1, intercept = 0,
    color = COL_REF, linewidth = 1.1, linetype = "solid"
  ) +

  geom_point(color = COL_ACTUAL, size = 5) +

  # Labels above geometry to avoid collision with the line.
  geom_text(
    aes(label = point_label),
    vjust = -1.6, hjust = 0.5,
    color = COL_TEXT, size = 3.6
  ) +

  annotate(
    "text",
    x = 0.004, y = 0.094,
    label = "Gold line: the rate that was set",
    color = COL_REF, size = 3.8, fontface = "bold", hjust = 0
  ) +

  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = c(0.01, 0.05, 0.10),
    name   = "False positive rate set in advance"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = c(0.01, 0.05, 0.10),
    name   = "False positive rate actually observed"
  ) +
  coord_cartesian(xlim = c(0, AX_MAX), ylim = c(0, AX_MAX)) +

  labs(
    title = "The false positive rate is not discovered. It is set.",
    subtitle = paste0(
      "20,000 runs of the same comparison on built data where the two ",
      "predictors are equally\npredictive, so every reported difference is ",
      "false. Only the threshold changes between points."
    ),
    caption = "Built data, not collected. Each run: 1,000 observations, 997 degrees of freedom."
  ) +

  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = rel(1.25), hjust = 0),
    plot.subtitle   = element_text(color = "gray40", size = rel(0.82), hjust = 0),
    plot.caption    = element_text(color = "gray60", size = rel(0.68), hjust = 1),
    panel.grid.minor = element_blank(),
    axis.title      = element_text(color = COL_TEXT),
    axis.text       = element_text(color = "gray40"),
    plot.margin     = margin(14, 18, 12, 12)
  )

# ------------------------------------------------------------------- EXPORT

png_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "type1_rate_tracks_alpha.png"))

ggsave(png_path, plot = p, width = PLOT_W, height = PLOT_H, dpi = PLOT_DPI)
cat("Saved PNG:", png_path, "\n")

# SVG guarded: an unguarded ggsave to .svg triggers an interactive install
# prompt when svglite is absent, which swallows the rest of the script.
if (requireNamespace("svglite", quietly = TRUE)) {
  svg_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "type1_rate_tracks_alpha.svg"))
  ggsave(svg_path, plot = p, width = PLOT_W, height = PLOT_H)
  cat("Saved SVG:", svg_path, "\n")
} else {
  cat("svglite not installed. SVG skipped, PNG written.\n")
}

# ------------------------------------------------------------ CONSOLE SUMMARY

cat("\n---------------------------------------------------------\n")
cat("POST 24 VISUAL SUMMARY\n")
cat("---------------------------------------------------------\n")
cat("Panels                : 1\n")
cat("Encoding              : points on identity line (sameness finding)\n")
cat("Replicates per point  :", format(N_REPS, big.mark = ","), "\n")
cat("Conditions plotted    :", paste(sweep$alpha_set, collapse = ", "), "\n")
cat("Observed rates        :", paste(sweep$rate_obs, collapse = ", "), "\n")
cat("Assertions passed     : 11 of 11\n")
cat("Output size           :", PLOT_W, "x", PLOT_H, "in at", PLOT_DPI, "dpi\n")
cat("---------------------------------------------------------\n")
print(p)
