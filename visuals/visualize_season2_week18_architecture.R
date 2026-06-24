# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 18
# Visualization: Shiny app data-flow architecture diagram
# File: examples/visualize_season2_week18_architecture.R
#
# PURPOSE
# -------
# Produces the postable architecture visual for the Week 18 deliverable. An
# infrastructure week has no data finding to chart, so this is a system
# diagram instead: it shows how Phase 4 cached outputs flow through the data
# layer into the four Shiny tabs, and makes the core rule visible (the app
# reads cached outputs and never re-runs the projection pipeline).
#
# Built with ggplot2 to stay on the project's visualization stack. Saves a
# 300 DPI PNG to output/plots/ using the Okabe-Ito colorblind-safe palette.
#
# RUN
# ---
#   source(here::here("examples", "visualize_season2_week18_architecture.R"))
#
# OUTPUT
# ------
#   output/plots/s2_week18_architecture.png   (300 DPI)
# ==============================================================================

library(ggplot2)
library(tibble)
library(here)

SEASON <- 2026L

# ------------------------------------------------------------------------------
# OKABE-ITO PALETTE (colorblind-safe)
# ------------------------------------------------------------------------------

OI_BLUE   <- "#0072B2"  # cached and loaded sources
OI_GRAY   <- "#999999"  # not yet generated sources
OI_ORANGE <- "#E69F00"  # data layer
OI_GREEN  <- "#009E73"  # shiny tabs
ARROW_COL <- "#444444"

# ------------------------------------------------------------------------------
# BOXES
# ------------------------------------------------------------------------------
# Three layers stacked top to bottom in a 0-100 x 0-106 coordinate space.

boxes <- tibble::tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~fill,     ~text_col,  ~label,
  # --- Layer 1: Phase 4 cached outputs (y 82-94) ---
  2,     16,    82,    94,    OI_BLUE,   "#FFFFFF",  "VORP\nR/33",
  18,    32,    82,    94,    OI_BLUE,   "#FFFFFF",  "Start/Sit\nR/36",
  34,    48,    82,    94,    OI_BLUE,   "#FFFFFF",  "DEF\nR/36",
  50,    64,    82,    94,    OI_GRAY,   "#FFFFFF",  "Lineup\nR/35",
  66,    80,    82,    94,    OI_BLUE,   "#FFFFFF",  "Prospects\nR/28",
  82,    96,    82,    94,    OI_GRAY,   "#FFFFFF",  "Trade\nR/37",
  # --- Layer 3: Shiny tabs (y 8-22) ---
  6,     25,    8,     22,    OI_GREEN,  "#FFFFFF",  "Projections",
  29,    48,    8,     22,    OI_GREEN,  "#FFFFFF",  "Waiver /\nStart-Sit",
  52,    71,    8,     22,    OI_GREEN,  "#FFFFFF",  "Trade\nEvaluator",
  75,    94,    8,     22,    OI_GREEN,  "#FFFFFF",  "Rookie\nTracker"
)
boxes$xmid <- (boxes$xmin + boxes$xmax) / 2
boxes$ymid <- (boxes$ymin + boxes$ymax) / 2

# Data layer is one wide box, drawn separately so its two-line label sits right.
data_layer <- tibble::tibble(
  xmin = 20, xmax = 80, ymin = 48, ymax = 62, fill = OI_ORANGE
)

# ------------------------------------------------------------------------------
# ARROWS
# ------------------------------------------------------------------------------
# Sources converge into the data layer; data layer diverges into the tabs.

source_centers <- c(9, 25, 41, 57, 73, 89)
land_in        <- c(28, 36, 44, 56, 64, 72)   # landing x on data-layer top edge
arrows_in <- tibble::tibble(
  x    = source_centers,
  xend = land_in,
  y    = 81.5,
  yend = 62.3
)

tab_centers <- c(15.5, 38.5, 61.5, 84.5)
start_out   <- c(35, 45, 55, 65)              # start x on data-layer bottom edge
arrows_out <- tibble::tibble(
  x    = start_out,
  xend = tab_centers,
  y    = 47.7,
  yend = 22.3
)

# ------------------------------------------------------------------------------
# LEFT-SIDE LAYER LABELS
# ------------------------------------------------------------------------------

layer_labels <- tibble::tibble(
  x = -8,
  y = c(88, 55, 15),
  label = c("PHASE 4\nOUTPUTS", "DATA\nLAYER", "SHINY\nAPP")
)

# ------------------------------------------------------------------------------
# PLOT
# ------------------------------------------------------------------------------

p <- ggplot() +
  # arrows first so boxes sit on top of any overlap
  geom_segment(
    data = arrows_in,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = grid::arrow(length = grid::unit(0.18, "cm"), type = "closed"),
    color = ARROW_COL, linewidth = 0.4
  ) +
  geom_segment(
    data = arrows_out,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = grid::arrow(length = grid::unit(0.18, "cm"), type = "closed"),
    color = ARROW_COL, linewidth = 0.4
  ) +
  # data layer box
  geom_rect(
    data = data_layer,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = OI_ORANGE, color = "#FFFFFF", linewidth = 0.6
  ) +
  annotate("text", x = 50, y = 57, label = "load_shiny_data()",
           fontface = "bold", size = 4.2, color = "#000000") +
  annotate("text", x = 50, y = 52.5,
           label = "6 slots  |  missing-file safe  |  loaded once at startup",
           size = 2.8, color = "#000000") +
  # source and tab boxes
  geom_rect(
    data = boxes,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
    color = "#FFFFFF", linewidth = 0.6
  ) +
  geom_text(
    data = boxes,
    aes(x = xmid, y = ymid, label = label, color = text_col),
    fontface = "bold", size = 3.1, lineheight = 0.95
  ) +
  # left-side layer labels
  geom_text(
    data = layer_labels,
    aes(x = x, y = y, label = label),
    fontface = "bold", size = 2.9, color = OI_GRAY,
    hjust = 0.5, lineheight = 0.95
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_fixed(ratio = 1, xlim = c(-14, 100), ylim = c(2, 100)) +
  labs(
    title    = "NFL Analytics Toolkit: Shiny App Data Flow",
    subtitle = paste0("Season ", SEASON,
                      "  |  Phase 5  |  Architecture and Data Layer"),
    caption  = paste(
      "Blue: cached and loaded.  Gray: not yet generated.",
      "The app reads cached outputs and never re-runs the projection pipeline."
    )
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0,
                                 margin = margin(b = 2)),
    plot.subtitle = element_text(size = 11, color = "#333333", hjust = 0,
                                 margin = margin(b = 8)),
    plot.caption  = element_text(size = 9, color = "#555555", hjust = 0,
                                 margin = margin(t = 10)),
    plot.margin   = margin(14, 14, 14, 14)
  )

# ------------------------------------------------------------------------------
# SAVE
# ------------------------------------------------------------------------------

out_dir <- here::here("output", "plots")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}
out_path <- file.path(out_dir, "s2_week18_architecture.png")

ggsave(out_path, plot = p, width = 9.5, height = 8.0, dpi = 300, bg = "#FFFFFF")

message(paste0("Saved: ", out_path))
