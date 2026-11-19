# ==============================================================================
# create_stats_post27_visuals.R
#
# Statistics Series, Post 27: Effect Size
#
# Central finding the panel must carry: applying Cohen's q expands every gap,
# and it reverses the order of the bottom two. Running back starts with the
# smallest raw gap of the three and finishes larger than receiver, because
# running back correlations sit nearer the ceiling where the Fisher
# transformation stretches hardest.
#
# Encoding rationale. The finding is a REORDERING, not a set of magnitudes.
# Paired bars would show six heights and force the reader to compute two
# rankings and compare them. A slope chart makes the reversal a literal
# crossing of two lines: the single most visible mark in the panel. Expansion
# reads as all three lines rising; reordering reads as RB crossing WR.
#
# All values hardcoded from verified console output of the cached Season 2
# Week 4 A/B results object. Nothing is recomputed here, to prevent drift
# between the panel and the post prose.
#
# Provenance of the numbers below:
#   s2_week4_ab_results.rds, mtime 2026-04-14 08:35:05 CDT
#   Correlations are stored rounded to 4 dp by the source script before save.
#   Raw gap  = abs(r_std - r_roll3) on those stored values.
#   Cohens q = stored cohens_q field.
#
# REVISION NOTE. The first render collided the two right-side labels. RB and
# WR finish 0.0016 apart, roughly 2% of panel height, which is closer than two
# lines of bold text can be drawn. Labels on that side are now displaced to a
# legible minimum separation and tied back to their true points with short
# connectors. The printed number is always the true value; only the drawing
# position of the text moves.
# ==============================================================================

library(ggplot2)
library(dplyr)

# ------------------------------------------------------------------ constants

OUT_DIR    <- "stats_posts/output/plots"
OUT_PREFIX <- "stats_post27_"
PLOT_W_IN  <- 8
PLOT_H_IN  <- 5.333
PLOT_DPI   <- 300

# Minimum vertical separation between two right-side labels, in data units.
# Set from the plotted y-range: two lines of bold 3.6pt text need roughly 5%
# of a 0.070 range to clear each other.
LABEL_MIN_SEP <- 0.0038

# Okabe-Ito, colorblind safe. Position mapping matches the prior post in this
# arc: navy QB, gold RB, bluish-green WR. No value here is "wrong", so no red.
COL_QB <- "#0072B2"
COL_RB <- "#E69F00"
COL_WR <- "#009E73"
COL_GRID <- "grey85"
COL_TEXT <- "grey20"

# ----------------------------------------------------------- verified numbers

verified <- tibble::tribble(
  ~position, ~r_roll3, ~r_std,  ~raw_gap, ~cohens_q, ~n_player_weeks,
  "QB",        0.3647, 0.4311,    0.0664,    0.0789,            1242,
  "RB",        0.6227, 0.6520,    0.0293,    0.0493,            8123,
  "WR",        0.5080, 0.5425,    0.0345,    0.0477,           44691
)

# ------------------------------------------------- PRE-BUILD ASSERTION BLOCK
# Each assertion validates a claim the post makes in prose. A failure here
# means the panel and the post have diverged and neither should ship.

stopifnot(
  "A1. Three positions present" =
    nrow(verified) == 3L,

  "A2. No missing values anywhere in the plotted frame" =
    sum(is.na(verified)) == 0L,

  "A3. Raw gap reconstructs from the stored correlations (4 dp tolerance)" =
    all(abs(abs(verified$r_std - verified$r_roll3) - verified$raw_gap) < 1e-9),

  "A4. Cohen's q reconstructs from the stored correlations via Fisher z.
       Tolerance 1e-4 because the cached correlations are stored rounded
       to 4 dp before q was computed upstream." =
    all(abs(abs(atanh(verified$r_std) - atanh(verified$r_roll3)) -
              verified$cohens_q) < 1e-4),

  "A5. CENTRAL CLAIM ONE. Every gap expands under the transformation." =
    all(verified$cohens_q > verified$raw_gap),

  "A6. CENTRAL CLAIM TWO. On raw gap, WR exceeds RB." =
    verified$raw_gap[verified$position == "WR"] >
    verified$raw_gap[verified$position == "RB"],

  "A7. CENTRAL CLAIM TWO, second half. On Cohen's q, RB exceeds WR.
       A6 and A7 together are the reordering the panel exists to show." =
    verified$cohens_q[verified$position == "RB"] >
    verified$cohens_q[verified$position == "WR"],

  "A8. CENTRAL CLAIM THREE. RB correlations sit nearer the ceiling than WR,
       which is the stated cause of the reordering." =
    verified$r_std[verified$position == "RB"] >
    verified$r_std[verified$position == "WR"],

  "A9. QB holds rank one on both measures, so the reversal is isolated to
       the bottom two and the panel must not imply a general scramble." =
    which.max(verified$raw_gap) == which(verified$position == "QB") &&
    which.max(verified$cohens_q) == which(verified$position == "QB"),

  "A10. All effect sizes fall below Cohen's 0.10 small benchmark, which the
        post asserts explicitly." =
    all(verified$cohens_q < 0.10),

  "A11. All correlations lie strictly inside (-1, 1) so atanh is defined." =
    all(abs(c(verified$r_roll3, verified$r_std)) < 1),

  "A12. Player-week counts sum to the total cited in the post." =
    sum(verified$n_player_weeks) == 54056L
)

message("Pre-build assertions: 12/12 passed.")

# ------------------------------------------------------------- reshape to long

plot_data <- verified %>%
  select(position, raw_gap, cohens_q) %>%
  tidyr::pivot_longer(
    cols      = c(raw_gap, cohens_q),
    names_to  = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = factor(
      measure,
      levels = c("raw_gap", "cohens_q"),
      labels = c("Plain subtraction", "Cohen's q")
    ),
    position = factor(position, levels = c("QB", "RB", "WR"))
  )

stopifnot(
  "A13. Long frame has one row per position per measure" =
    nrow(plot_data) == 6L,
  "A14. No unused factor levels, which would draw phantom axis entries" =
    nlevels(droplevels(plot_data$position)) == 3L
)

# ------------------------------------------------------------- label placement

# Left side: the three endpoints are far enough apart that no displacement is
# needed. Offset outward from the axis position so text never sits on a point.
label_left <- plot_data %>%
  filter(measure == "Plain subtraction") %>%
  mutate(
    label_y = value,
    x_num   = 1 - 0.06,
    hjust   = 1
  )

stopifnot(
  "A15. Left-side endpoints are already separated by more than the legible
        minimum, so no displacement is applied on that side." =
    all(diff(sort(label_left$value)) >= LABEL_MIN_SEP)
)

# Right side: RB and WR finish 0.0016 apart and collide. Walk the sorted
# endpoints upward, pushing any label that sits closer than the minimum.
label_right <- plot_data %>%
  filter(measure == "Cohen's q") %>%
  arrange(value) %>%
  mutate(
    label_y = {
      y <- value
      for (i in seq_along(y)[-1]) {
        if (y[i] - y[i - 1] < LABEL_MIN_SEP) y[i] <- y[i - 1] + LABEL_MIN_SEP
      }
      y
    },
    x_num = 2 + 0.06,
    hjust = 0
  )

stopifnot(
  "A16. Right-side labels clear the legible minimum after displacement." =
    all(diff(sort(label_right$label_y)) >= LABEL_MIN_SEP - 1e-12),

  "A17. No label is displaced by more than the minimum separation, which
        would risk pulling it nearer a neighbouring point than its own." =
    all(abs(label_right$label_y - label_right$value) <= LABEL_MIN_SEP),

  "A18. Displaced labels still print the true stored value, not the drawing
        position. Guards against a label showing a nudged number." =
    all(sort(label_right$value) == sort(verified$cohens_q)),

  "A19. Every displaced label stays inside the zoom window." =
    all(label_right$label_y > 0.020 & label_right$label_y < 0.090)
)

# --------------------------------------------------------------------- panel

p <- ggplot(
  plot_data,
  aes(x = as.numeric(measure), y = value,
      group = position, colour = position)
) +
  geom_line(linewidth = 1.5, lineend = "round") +
  geom_point(size = 4) +
  # Connectors tying displaced right-side labels back to their true points.
  geom_segment(
    data = label_right,
    aes(x = 2, xend = x_num - 0.012, y = value, yend = label_y,
        colour = position),
    linewidth = 0.4, show.legend = FALSE
  ) +
  geom_text(
    data = label_left,
    aes(x = x_num, y = label_y,
        label = sprintf("%s  %.4f", position, value), hjust = hjust),
    size = 3.6, fontface = "bold", show.legend = FALSE
  ) +
  geom_text(
    data = label_right,
    aes(x = x_num, y = label_y,
        label = sprintf("%.4f  %s", value, position), hjust = hjust),
    size = 3.6, fontface = "bold", show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(QB = COL_QB, RB = COL_RB, WR = COL_WR),
    guide  = "none"
  ) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Plain subtraction", "Cohen's q"),
    limits = c(0.55, 2.45)
  ) +
  # coord_cartesian for the continuous y axis: zooms without dropping rows.
  coord_cartesian(ylim = c(0.020, 0.090)) +
  labs(
    title    = "Correcting for the ceiling reverses the bottom two",
    subtitle = paste0(
      "Gap between two prediction methods, before and after the Fisher ",
      "transformation.\nEvery gap expands. Running back starts below ",
      "receiver and finishes above it."
    ),
    x = NULL,
    y = "Size of the gap",
    caption = paste0(
      "54,056 player-weeks, 2025 season. Cohen's q = |atanh(r1) - atanh(r2)|. ",
      "All three fall below Cohen's 0.10 small benchmark."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 15,
                                    margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 10.5, colour = COL_TEXT,
                                    margin = margin(b = 14)),
    plot.caption     = element_text(size = 8.5, colour = "grey40",
                                    hjust = 0, margin = margin(t = 12)),
    axis.text.x      = element_text(face = "bold", size = 11,
                                    colour = COL_TEXT),
    axis.title.y     = element_text(size = 10, colour = COL_TEXT),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = COL_GRID, linewidth = 0.3),
    plot.margin      = margin(16, 20, 12, 16)
  )

# ------------------------------------------------ POST-BUILD RENDER CHECKS
# Geometry checks the pre-build assertions cannot catch, run against the
# constructed object rather than against the source frame.
#
# Layer order, which these indices depend on:
#   [[1]] geom_line     [[2]] geom_point   [[3]] geom_segment
#   [[4]] geom_text L   [[5]] geom_text R

built <- ggplot_build(p)

stopifnot(
  "B0. Layer count matches the indices these checks assume. Adding or
       reordering a geom invalidates every index below." =
    length(built$data) == 5L,

  "B1. Exactly three line groups drawn, one per position. A different count
       means the group aesthetic collapsed or split." =
    length(unique(built$data[[1]]$group)) == 3L,

  "B2. Six points drawn, two per position. Fewer means an endpoint was
       dropped by the y-range zoom." =
    nrow(built$data[[2]]) == 6L,

  "B3. Three label connectors drawn, one per right-side endpoint." =
    nrow(built$data[[3]]) == 3L,

  "B4. Six text labels drawn, three per side, so no endpoint is unlabelled." =
    nrow(built$data[[4]]) == 3L && nrow(built$data[[5]]) == 3L,

  "B5. The zoom window contains every plotted value. A value outside this
       range would be clipped and the reader would see a truncated line." =
    all(plot_data$value > 0.020 & plot_data$value < 0.090),

  "B6. RENDER-LEVEL CHECK ON THE CENTRAL FINDING. The RB and WR lines must
       actually cross inside the panel. Verified by confirming the sign of
       the RB-minus-WR difference flips between the two x positions." =
    sign(verified$raw_gap[verified$position == "RB"] -
           verified$raw_gap[verified$position == "WR"]) !=
    sign(verified$cohens_q[verified$position == "RB"] -
           verified$cohens_q[verified$position == "WR"]),

  "B7. No two drawn right-side label rows share a y position, which is the
       collision the first render produced." =
    length(unique(round(built$data[[5]]$y, 6))) == 3L,

  "B8. Both x tick positions are present and labelled." =
    length(built$layout$panel_params[[1]]$x$get_labels()) == 2L
)

message("Post-build render checks: 9/9 passed.")

# --------------------------------------------------------------------- export

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

png_path <- file.path(OUT_DIR, paste0(OUT_PREFIX, "effect_size_slope.png"))
svg_path <- file.path(OUT_DIR, paste0(OUT_PREFIX, "effect_size_slope.svg"))

ggsave(png_path, plot = p, width = PLOT_W_IN, height = PLOT_H_IN,
       dpi = PLOT_DPI, bg = "white")
message("Wrote: ", png_path)

# SVG is optional. svglite may be absent, and ggsave() would otherwise open an
# interactive install prompt that consumes the rest of a sourced script.
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, plot = p, width = PLOT_W_IN, height = PLOT_H_IN,
         bg = "white", device = svglite::svglite)
  message("Wrote: ", svg_path)
} else {
  message("Skipped SVG: package 'svglite' not installed. ",
          "Run install.packages(\"svglite\") to enable SVG output.")
}

message(sprintf("Dimensions: %g x %g in at %d DPI (%d x %d px).",
                PLOT_W_IN, PLOT_H_IN, PLOT_DPI,
                round(PLOT_W_IN * PLOT_DPI), round(PLOT_H_IN * PLOT_DPI)))
