# ==============================================================================
# create_stats_post45_visual.R
# Statistics Series Post 45: Ordinary Least Squares
#
# Concept test: the post defines ordinary least squares as squaring each
# receiver's miss from the line, adding the squares, and keeping the line with
# the smallest total.
# Visual purpose (NFL-38): the text cannot show every receiver's miss at once
# as a vertical gap to the line, or the squared total as a valley whose bottom
# is the fitted line, with a 5-point slide climbing the same 3,325 either way.
# Salience: panel B's valley and its gold bottom point are the heaviest
# elements; panel A's gaps are light gray except the five largest misses,
# which are thicker and purple (width carries the meaning, not color alone).
# Color: navy = actual receivers. Gold = the least squares line and its total
# (a fitted summary, not a wrong value). Reddish purple = the five largest
# misses (highlight, not wrong). Gray = other misses and reference.
# No red: no value on the chart is wrong.
#
# Data: per-receiver rows from stats_post44v3_wr.csv, written by
# stats_post44_pull_v3.R (2026-11-09). Values the post quotes are hardcoded
# from the stats_post45_pull.R output (2026-11-12) and checked against a refit
# from that CSV before anything is drawn.
#
# Run from the nfl-analytics-toolkit project root:
#   source("stats_posts/scripts/create_stats_post45_visual.R")
# Outputs: stats_posts/output/plots/stats_post45_least_squares.png and .svg (1200x800)
# Requires: dplyr, ggplot2, here, glue, patchwork; svglite for the SVG.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(glue)
})
if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("Package 'patchwork' is required: install.packages('patchwork')", call. = FALSE)
}
suppressPackageStartupMessages(library(patchwork))

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------
OUT_DIR    <- here::here("stats_posts", "output", "plots")
DATA_DIR   <- here::here("stats_posts", "output", "data")
FILE_STEM  <- "stats_post45_least_squares"
WIDTH_PX   <- 1200
HEIGHT_PX  <- 800
DPI        <- 300

COL_DOT    <- "#0072B2"   # Okabe-Ito navy: actual receivers
COL_LINE   <- "#E69F00"   # Okabe-Ito gold: least squares line and its total
COL_LINE_T <- "#9A6A00"   # darker gold for text on white (same hue)
COL_BIG    <- "#CC79A7"   # Okabe-Ito reddish purple: five largest misses
COL_BIG_T  <- "#9E4F80"   # darker purple for text on white (same hue)
COL_GAP    <- "gray75"    # every other miss
COL_CURVE  <- "gray35"    # valley curve and the two slide points
COL_TEXT   <- "gray20"

# Verified values: stats_post45_pull.R output, 2026-11-12
N_WR        <- 133L
SLOPE_V     <- 1.86668463052563
INT_V       <- -9.38893260159217
SSE_V       <- 58362.1666789621
SHIFT       <- 5
SHIFT_COST  <- 3325
TOP5_NAMES  <- c("P.Nacua", "J.Jeudy", "J.Smith-Njigba", "J.Jefferson", "K.Boutte")
TOL_COEF    <- 1e-8
X_MIN       <- 20L
X_MAX       <- 185L

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

gate <- function(ok, msg) {
  if (!isTRUE(ok)) stop("GATE FAILED: ", msg, call. = FALSE)
  cat("  [PASS] ", msg, "\n", sep = "")
}

# ------------------------------------------------------------------------------
# REPRODUCTION GATE: refit from the CSV and match every value the post quotes
# ------------------------------------------------------------------------------
cat("\nReproduction gate\n")
f_wr <- file.path(DATA_DIR, "stats_post44v3_wr.csv")
gate(file.exists(f_wr), "stats_post44v3_wr.csv exists (run stats_post44_pull_v3.R first)")
wr <- utils::read.csv(f_wr, stringsAsFactors = FALSE)
gate(all(c("player_name", "targets", "total_points") %in% names(wr)),
     "CSV has player_name, targets, total_points")
gate(nrow(wr) == N_WR && !anyNA(wr$targets) && !anyNA(wr$total_points),
     glue("{N_WR} receivers, no missing values"))
gate(min(wr$targets) == X_MIN && max(wr$targets) == X_MAX,
     glue("targets run {X_MIN} to {X_MAX}"))

fit       <- stats::lm(total_points ~ targets, data = wr)
SLOPE     <- unname(coef(fit)[2])
INTERCEPT <- unname(coef(fit)[1])
cat(sprintf("  slope %.10f | intercept %.10f\n", SLOPE, INTERCEPT))
gate(abs(SLOPE - SLOPE_V) < TOL_COEF, "slope matches the Post 45 pull")
gate(abs(INTERCEPT - INT_V) < TOL_COEF, "intercept matches the Post 45 pull")
gate(sprintf("%.2f", SLOPE) == "1.87", "slope reads 1.87 (post wording)")

wr <- wr %>%
  mutate(fitted = INTERCEPT + SLOPE * targets,
         resid  = total_points - fitted,
         sq     = resid^2)
SSE <- sum(wr$sq)
cat(sprintf("  squared total %.4f\n", SSE))
gate(abs(SSE - SSE_V) < 1e-6 * SSE_V, "squared total matches the Post 45 pull")
gate(format(round(SSE), big.mark = ",") == "58,362", "squared total reads 58,362 (post wording)")

cost_up   <- sum((wr$resid - SHIFT)^2) - SSE
cost_down <- sum((wr$resid + SHIFT)^2) - SSE
cat(sprintf("  slide up 5: +%.6f | slide down 5: +%.6f\n", cost_up, cost_down))
gate(abs(cost_up - SHIFT_COST) < 1e-6 && abs(cost_down - SHIFT_COST) < 1e-6,
     "a 5-point slide either way adds 3,325 (post wording)")

top5 <- wr %>% arrange(desc(sq)) %>% slice_head(n = 5)
nac_share  <- 100 * top5$sq[1] / SSE
top5_share <- 100 * sum(top5$sq) / SSE
cat(sprintf("  top five: %s | Nacua miss %.4f, share %.4f%% | top-five share %.4f%%\n",
            paste(top5$player_name, collapse = ", "), top5$resid[1], nac_share, top5_share))
gate(identical(top5$player_name, TOP5_NAMES), "five largest misses are the expected receivers")
gate(abs(top5$resid[1] - 74.5) < 0.05, "Nacua 74.5 above the line (post wording)")
gate(sprintf("%.1f", nac_share) == "9.5", "Nacua share reads 9.5% (post wording)")
gate(sprintf("%.1f", top5_share) == "33.5", "top-five share reads 33.5% (post wording)")

wr <- wr %>% mutate(big = player_name %in% TOP5_NAMES)
gate(sum(wr$big) == 5L, "exactly five highlighted receivers")

# Valley: squared total recomputed directly at every slide, not from a formula
valley <- tibble(shift = seq(-10, 10, by = 0.1)) %>%
  mutate(total = vapply(shift, function(d) sum((wr$resid - d)^2), numeric(1)))
gate(max(abs(valley$total - (SSE + N_WR * valley$shift^2))) < 1e-6 * SSE,
     "valley equals the squared total plus 133 x slide squared")
gate(abs(valley$shift[which.min(valley$total)]) < 1e-9, "valley bottom sits at a slide of 0")
marks <- tibble(shift = c(-SHIFT, 0, SHIFT)) %>%
  mutate(total = vapply(shift, function(d) sum((wr$resid - d)^2), numeric(1)),
         kind  = c("slide", "fit", "slide"))

# ------------------------------------------------------------------------------
# THEME
# ------------------------------------------------------------------------------
theme_post <- function() {
  theme_minimal(base_size = 6) +
    theme(
      plot.title       = element_text(face = "bold", size = 7, color = COL_TEXT, hjust = 0),
      plot.subtitle    = element_text(size = 5.6, color = "gray35", hjust = 0, lineheight = 1.05),
      axis.title       = element_text(size = 6, color = "gray30"),
      axis.text        = element_text(size = 5.6, color = "gray40"),
      panel.grid.minor = element_blank(),
      plot.margin      = margin(4, 6, 4, 4)
    )
}

# ------------------------------------------------------------------------------
# PANEL A: the misses being squared
# ------------------------------------------------------------------------------
pA <- ggplot(wr) +
  geom_segment(data = filter(wr, !big),
               aes(x = targets, xend = targets, y = fitted, yend = total_points),
               color = COL_GAP, linewidth = 0.25) +
  annotate("segment", x = X_MIN, xend = X_MAX,
           y = INTERCEPT + SLOPE * X_MIN, yend = INTERCEPT + SLOPE * X_MAX,
           color = COL_LINE, linewidth = 0.8) +
  geom_segment(data = filter(wr, big),
               aes(x = targets, xend = targets, y = fitted, yend = total_points),
               color = COL_BIG, linewidth = 0.8) +
  geom_point(aes(x = targets, y = total_points), color = COL_DOT, size = 0.6, alpha = 0.85) +
  annotate("text", x = 156, y = 392, hjust = 1, vjust = 0.5,
           label = "Nacua: 9.5% of the total",
           color = COL_BIG_T, size = 2.0, fontface = "bold") +
  annotate("segment", x = 157.5, y = 391, xend = 165, yend = 380,
           color = COL_BIG_T, linewidth = 0.35) +
  scale_x_continuous(breaks = c(50, 100, 150)) +
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400)) +
  coord_cartesian(xlim = c(15, 190), ylim = c(0, 410), expand = FALSE) +
  labs(title    = "A. The misses being squared",
       subtitle = "Gray: each receiver's miss from the line\nPurple: the five largest, 33.5% of the squared total",
       x = "Targets", y = "Season fantasy points") +
  theme_post()

# ------------------------------------------------------------------------------
# PANEL B: the squared total as the line slides
# ------------------------------------------------------------------------------
pB <- ggplot(valley, aes(x = shift, y = total)) +
  geom_line(color = COL_CURVE, linewidth = 0.6) +
  geom_point(data = filter(marks, kind == "slide"), color = COL_CURVE, size = 1.2) +
  geom_point(data = filter(marks, kind == "fit"), color = COL_LINE, size = 2.2) +
  annotate("text", x = 0, y = 71000, label = "Bottom: fitted line, 58,362",
           color = COL_LINE_T, size = 2.0, fontface = "bold", vjust = 0.5) +
  annotate("text", x = 4.6, y = 62300, hjust = 1, label = "+3,325",
           color = COL_CURVE, size = 2.0) +
  annotate("text", x = -4.6, y = 62300, hjust = 0, label = "+3,325",
           color = COL_CURVE, size = 2.0) +
  scale_x_continuous(breaks = c(-10, -5, 0, 5, 10),
                     labels = c("-10", "-5", "0", "+5", "+10")) +
  scale_y_continuous(breaks = c(60000, 65000, 70000),
                     labels = c("60k", "65k", "70k")) +
  coord_cartesian(xlim = c(-10.5, 10.5), ylim = c(57500, 73000), expand = FALSE) +
  labs(title    = "B. The squared total",
       subtitle = "Slide the line up or down:\nthe total climbs either way",
       x = "Slide of the line (points)",
       y = "Squared total, ∑(actual - line)²") +
  theme_post()

p <- (pA | pB) +
  plot_layout(widths = c(1.45, 1)) +
  plot_annotation(
    title    = "Least squares keeps the line with the smallest squared total",
    subtitle = "2025 wide receivers with 20+ targets (n = 133), full PPR fantasy points",
    caption  = "A miss is a receiver's actual points minus the line's points at his targets. Toolkit 2025 regular-season data.",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 8.5, color = COL_TEXT, hjust = 0),
      plot.subtitle = element_text(size = 6, color = "gray35", hjust = 0),
      plot.caption  = element_text(size = 5, color = "gray50", hjust = 0),
      plot.margin   = margin(6, 6, 4, 4)
    )
  )

# ------------------------------------------------------------------------------
# PRE-SAVE ASSERTIONS ON THE BUILT LAYERS
# ------------------------------------------------------------------------------
cat("\nBuilt-layer checks\n")
lA_gap  <- layer_data(pA, 1)
lA_line <- layer_data(pA, 2)
lA_big  <- layer_data(pA, 3)
lA_pts  <- layer_data(pA, 4)
gate(nrow(lA_gap) == N_WR - 5L, "panel A: 128 gray miss segments")
gate(nrow(lA_big) == 5L, "panel A: 5 purple miss segments")
gate(nrow(lA_pts) == N_WR, "panel A: 133 receiver points")
gate(all(abs(lA_pts$y - wr$total_points) < 1e-9), "panel A: points sit at actual points")
gate(abs(lA_line$x - X_MIN) < 1e-9 && abs(lA_line$xend - X_MAX) < 1e-9,
     "panel A: fitted line spans only 20 to 185 targets")
gate(all(lA_big$colour == COL_BIG) && all(lA_big$linewidth > lA_gap$linewidth[1]),
     "panel A: largest misses are purple and thicker than the rest")
lA_lead <- layer_data(pA, 6)
nac_row <- wr[wr$player_name == "P.Nacua", ]
gate(nrow(lA_lead) == 1L && abs(lA_lead$xend - nac_row$targets) < 2 &&
       abs(lA_lead$yend - nac_row$total_points) < 6,
     "panel A: Nacua leader line ends at Nacua's dot")

lB_curve <- layer_data(pB, 1)
lB_fit   <- layer_data(pB, 3)
gate(nrow(lB_curve) == nrow(valley), "panel B: valley drawn at every slide")
gate(nrow(lB_fit) == 1L && abs(lB_fit$x) < 1e-9 && abs(lB_fit$y - SSE) < 1e-6,
     "panel B: gold point at slide 0, height 58,362")
gate(lB_fit$colour == COL_LINE && lB_fit$size > layer_data(pB, 2)$size[1],
     "panel B: fitted-line point is gold and the largest point")
gate(all(abs(marks$total[marks$kind == "slide"] - (SSE + SHIFT_COST)) < 1e-6),
     "panel B: both slide points sit 3,325 above the bottom")

# ------------------------------------------------------------------------------
# SAVE
# ------------------------------------------------------------------------------
png_path <- file.path(OUT_DIR, paste0(FILE_STEM, ".png"))
svg_path <- file.path(OUT_DIR, paste0(FILE_STEM, ".svg"))
ggsave(png_path, plot = p, width = WIDTH_PX / DPI, height = HEIGHT_PX / DPI,
       units = "in", dpi = DPI, bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, plot = p, width = WIDTH_PX / DPI, height = HEIGHT_PX / DPI,
         units = "in", bg = "white")
} else {
  message("svglite not installed; SVG skipped. install.packages('svglite')")
}
gate(file.exists(png_path), "PNG written")

# ------------------------------------------------------------------------------
# CONSOLE SUMMARY
# ------------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("Stats Post 45 visual\n")
cat(glue("  n {nrow(wr)} | slope {sprintf('%.4f', SLOPE)} | squared total {format(round(SSE), big.mark = ',')} | ",
         "5-point slide +{format(round(cost_up), big.mark = ',')}"), "\n")
cat(glue("  Nacua share {sprintf('%.1f', nac_share)}% | top-five share {sprintf('%.1f', top5_share)}%"), "\n")
cat("  PNG: ", png_path, "\n", sep = "")
cat("  SVG: ", if (file.exists(svg_path)) svg_path else "not written", "\n", sep = "")
cat(strrep("=", 70), "\n", sep = "")
