# ==============================================================================
# create_stats_post26_visuals.R
# Statistics Series Post 26: Statistical Power
#
# Single panel. Achieved power as a function of the target gap size, for the
# three positions in the A/B predictive validity study, at their observed
# sample sizes.
#
# The finding the panel must land in 5 seconds:
#   All three positions clear the 0.80 standard at the 0.10 default, and the
#   points where their curves cross 0.80 are far apart. "Adequately powered"
#   is one label covering three very different detection floors.
#
# Data provenance:
#   Sample sizes and all verified power values come from the console output of
#   the Post 26 pull (s2_week4_power_analysis.rds). The curve itself is
#   computed from the source formula at R/18 lines 861-862 so the line is
#   smooth; the assertion block then requires the computed curve to reproduce
#   every verified console figure exactly. Any drift fails the script.
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tibble)
library(glue)

# ---- CONFIGURATION -----------------------------------------------------------

OUTPUT_DIR  <- file.path("stats_posts", "output", "plots")
FILE_PREFIX <- "stats_post26_"
PLOT_WIDTH  <- 8
PLOT_HEIGHT <- 5.333
PLOT_DPI    <- 300

# Okabe-Ito, colorblind safe. No red: none of the three positions is the
# "wrong" one, so the red-vs-gold convention resolves to gold here.
COL_QB   <- "#0072B2"   # navy
COL_RB   <- "#E69F00"   # gold
COL_WR   <- "#009E73"   # green
COL_REF  <- "gray45"
COL_NOTE <- "gray30"

ALPHA        <- 0.05
STANDARD     <- 0.80
DEFAULT_Q    <- 0.10

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ---- VERIFIED VALUES (hardcoded from console output) -------------------------

positions <- tibble(
  position       = c("QB", "RB", "WR"),
  n_player_weeks = c(1242L, 8123L, 44691L),
  power_at_dflt  = c(0.9406, 1.0000, 1.0000),   # achieved power at q = 0.10
  floor_at_80    = c(0.07959, 0.03109, 0.01325) # smallest q catchable at 0.80
)

# QB sweep, verified: the four points the post quotes
qb_verified <- tibble(
  mde_q = c(0.030, 0.050, 0.075, 0.100),
  power = c(0.1843, 0.4208, 0.7517, 0.9406)
)

# ---- SOURCE FORMULA ----------------------------------------------------------
# Reproduces R/18 lines 861-862 exactly, including the second (opposite
# direction) term. That term is below 1e-7 everywhere in this panel but is
# retained so the computation matches source rather than approximating it.

z_alpha <- qnorm(1 - ALPHA / 2)

power_at <- function(n, q) {
  pnorm(sqrt(n - 3) * q - z_alpha) + pnorm(-sqrt(n - 3) * q - z_alpha)
}

# ---- BUILD CURVE DATA --------------------------------------------------------

q_grid <- seq(0.001, 0.20, length.out = 400)

curve_data <- positions %>%
  select(position, n_player_weeks) %>%
  tidyr::crossing(mde_q = q_grid) %>%
  mutate(power = power_at(n_player_weeks, mde_q)) %>%
  mutate(position = factor(position, levels = c("QB", "RB", "WR")))

floor_points <- positions %>%
  mutate(
    power_at_floor = power_at(n_player_weeks, floor_at_80),
    position       = factor(position, levels = c("QB", "RB", "WR"))
  )

default_points <- positions %>%
  mutate(
    power_computed = power_at(n_player_weeks, DEFAULT_Q),
    position       = factor(position, levels = c("QB", "RB", "WR"))
  )

# ---- PRE-BUILD ASSERTION BLOCK -----------------------------------------------
# Every assertion below validates a claim the post makes. A failure here means
# the panel and the prose have drifted apart.

stopifnot(
  "Chart has fewer than 2 data points" =
    nrow(curve_data) >= 2,
  "Curve contains NA power values" =
    sum(is.na(curve_data$power)) == 0,
  "Expected exactly 3 positions" =
    dplyr::n_distinct(curve_data$position) == 3L,
  "Power values fall outside [0, 1]" =
    all(curve_data$power >= 0 & curve_data$power <= 1),

  # Computed curve must reproduce the verified achieved-power console values
  "Computed power at q = 0.10 does not match verified console output" =
    all(abs(round(power_at(positions$n_player_weeks, DEFAULT_Q), 4) -
            positions$power_at_dflt) < 1e-8),

  # Computed curve must reproduce every QB sweep value the post quotes
  "Computed QB sweep does not match verified console output" =
    all(abs(round(power_at(1242L, qb_verified$mde_q), 4) -
            qb_verified$power) < 1e-8),

  # Each detection floor must actually return the 0.80 standard at its own n
  "A detection floor does not return 0.80 power at its own sample size" =
    all(abs(floor_points$power_at_floor - STANDARD) < 1e-3),

  # The post's core claim: all three floors sit below the default, which is
  # why all three passed
  "Not all detection floors sit below the 0.10 default" =
    all(positions$floor_at_80 < DEFAULT_Q),

  # The post's ratio claim, checked rather than asserted in prose alone
  "QB floor is not approximately 6x the WR floor" =
    abs(positions$floor_at_80[1] / positions$floor_at_80[3] - 6) < 0.10,

  # Ordering claim: larger sample means smaller detectable gap
  "Detection floors are not ordered inversely to sample size" =
    all(diff(positions$floor_at_80) < 0) &&
    all(diff(positions$n_player_weeks) > 0),

  # The QB collapse the post leads on
  "QB power at 0.03 is not far below the 0.80 standard" =
    power_at(1242L, 0.030) < 0.20
)

message("Pre-build assertions passed (11 checks).")

# ---- PLOT --------------------------------------------------------------------

pos_colors <- c("QB" = COL_QB, "RB" = COL_RB, "WR" = COL_WR)

floor_labels <- floor_points %>%
  mutate(
    label = glue("{position} floor: {format(round(floor_at_80, 4), nsmall = 4)}")
  )

p <- ggplot(curve_data, aes(x = mde_q, y = power, color = position)) +

  # Reference layers first so data draws on top
  geom_hline(yintercept = STANDARD, linetype = "dashed",
             color = COL_REF, linewidth = 0.5) +
  geom_vline(xintercept = DEFAULT_Q, linetype = "dotted",
             color = COL_REF, linewidth = 0.5) +

  geom_line(linewidth = 1.15) +

  # Where each curve crosses the 0.80 standard
  geom_point(data = floor_points,
             aes(x = floor_at_80, y = power_at_floor, color = position),
             size = 3.2, show.legend = FALSE) +

  # Where each position sat at the default the study actually used
  geom_point(data = default_points,
             aes(x = DEFAULT_Q, y = power_computed, color = position),
             size = 2.6, shape = 21, fill = "white", stroke = 1.1,
             show.legend = FALSE) +

  # Annotations above geometry
  annotate("text", x = 0.2, y = STANDARD, label = "0.80 standard",
           hjust = 1, vjust = -0.7, size = 3.3, color = COL_NOTE) +
  annotate("text", x = DEFAULT_Q, y = 0.02,
           label = "0.10, the default the study used",
           hjust = -0.04, vjust = 0, size = 3.3, color = COL_NOTE) +

  annotate("text", x = floor_points$floor_at_80[1], y = STANDARD,
           label = floor_labels$label[1],
           hjust = -0.10, vjust = 2.0, size = 3.2, color = COL_QB) +
  annotate("text", x = floor_points$floor_at_80[2], y = STANDARD,
           label = floor_labels$label[2],
           hjust = -0.10, vjust = 3.6, size = 3.2, color = COL_RB) +
  annotate("text", x = floor_points$floor_at_80[3], y = STANDARD,
           label = floor_labels$label[3],
           hjust = -0.10, vjust = 5.2, size = 3.2, color = COL_WR) +

  scale_color_manual(
    values = pos_colors,
    labels = c(
      "QB" = glue("QB ({format(positions$n_player_weeks[1], big.mark = ',')} rows)"),
      "RB" = glue("RB ({format(positions$n_player_weeks[2], big.mark = ',')} rows)"),
      "WR" = glue("WR ({format(positions$n_player_weeks[3], big.mark = ',')} rows)")
    ),
    name = NULL
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(breaks = seq(0, 0.20, by = 0.05)) +
  coord_cartesian(xlim = c(0, 0.20), ylim = c(0, 1.02), expand = FALSE) +

  labs(
    title    = "Three positions passed the same power check at three very different floors",
    subtitle = paste0(
      "Chance of catching a gap between the two prediction methods, ",
      "at each position's observed sample size.\n",
      "Filled dots mark the smallest gap each position can catch at the 0.80 standard."
    ),
    x = "Size of the gap the check is asked to catch",
    y = "Chance of catching it",
    caption = "Source: A/B predictive validity study, three seasons of player-weeks"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = rel(1.12), hjust = 0),
    plot.subtitle    = element_text(color = "gray40", size = rel(0.82), hjust = 0),
    plot.caption     = element_text(color = "gray60", size = rel(0.68), hjust = 1),
    panel.grid.minor = element_blank(),
    axis.title       = element_text(color = "gray30"),
    axis.text        = element_text(color = "gray40"),
    legend.position  = "top",
    legend.text      = element_text(size = rel(0.85)),
    plot.margin      = margin(12, 16, 10, 12)
  )

# ---- POST-BUILD RENDER CHECK -------------------------------------------------
# Inspects rendered geometry rather than input data. Catches cases where the
# data is correct but the panel draws something else.

built <- ggplot_build(p)

line_layer <- built$data[[3]]   # layer 3 = geom_line (after hline, vline)
stopifnot(
  "Rendered line layer does not contain 3 groups" =
    dplyr::n_distinct(line_layer$group) == 3L,
  "Rendered line layer has y values outside [0, 1]" =
    all(line_layer$y >= 0 & line_layer$y <= 1, na.rm = TRUE),
  "Rendered floor points are not all at the 0.80 standard" =
    all(abs(built$data[[4]]$y - STANDARD) < 1e-3)
)

message("Post-build render checks passed (3 checks).")

# ---- SAVE --------------------------------------------------------------------

png_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "power_floors.png"))
svg_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "power_floors.svg"))

ggsave(png_path, plot = p, width = PLOT_WIDTH, height = PLOT_HEIGHT,
       dpi = PLOT_DPI, bg = "white")

if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, plot = p, width = PLOT_WIDTH, height = PLOT_HEIGHT,
         device = svglite::svglite, bg = "white")
  svg_note <- svg_path
} else {
  svg_note <- "SKIPPED (svglite not installed)"
}

# ---- CONSOLE SUMMARY ---------------------------------------------------------

cat("\n")
cat("==============================================================\n")
cat("STATS POST 26 VISUAL: statistical power\n")
cat("==============================================================\n")
cat(glue("PNG : {png_path}"), "\n")
cat(glue("SVG : {svg_note}"), "\n")
cat(glue("Size: {PLOT_WIDTH} x {PLOT_HEIGHT} in at {PLOT_DPI} DPI"), "\n\n")

cat("Detection floors at the 0.80 standard:\n")
for (i in seq_len(nrow(positions))) {
  cat(glue(
    "  {positions$position[i]} : n = ",
    "{format(positions$n_player_weeks[i], big.mark = ',')}, ",
    "power at 0.10 = {format(positions$power_at_dflt[i], nsmall = 4)}, ",
    "floor = {format(positions$floor_at_80[i], nsmall = 5)}"
  ), "\n")
}
cat("\n")
cat(glue(
  "Floor spread: QB {format(positions$floor_at_80[1], nsmall = 5)} is ",
  "{round(positions$floor_at_80[1] / positions$floor_at_80[3], 2)}x the WR floor."
), "\n")
cat("All 14 assertions passed (11 pre-build, 3 post-build).\n")
cat("==============================================================\n")
