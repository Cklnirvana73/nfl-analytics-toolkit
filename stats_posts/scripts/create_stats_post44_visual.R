# ==============================================================================
# create_stats_post44_visual.R
# Statistics Series Post 44: Simple Linear Regression
#
# One panel: 133 wide receivers (2025 regular season, 20+ targets), targets on
# x and season fantasy points (full PPR) on y, with the fitted line drawn over
# the 20 to 185 targets it was fit on and a dotted gray extension back to the
# intercept at zero targets, inside a shaded band where no receiver exists.
#
# Concept test: the chart shows the post's core claim, the line itself: one
# straight line through the receivers, summarized by a slope and an intercept,
# read only across the range it was fit on.
# Visual purpose (NFL-38): the text cannot show the cloud of 133 receivers
# following one straight line, the slope as the line's steady climb, or the
# -9.39 intercept sitting in empty space left of 20 targets.
# Salience: the gold fitted line and its slope label are the heaviest elements;
# dots are lighter; Nacua and Chase are the only labeled receivers.
# Color: navy = actual receivers. Gold = fitted line over the data (an average
# pattern, not a wrong value). Gray = dotted extension and empty band
# (reference only). No red: no value on the chart is wrong.
#
# Data: per-receiver rows from stats_post44v3_wr.csv, written by
# stats_post44_pull_v3.R (2026-11-09). The script refits the line and stops
# unless every value the post quotes reproduces from that file.
#
# Run from the nfl-analytics-toolkit project root:
#   source("stats_posts/scripts/create_stats_post44_visual.R")
# Outputs: stats_posts/output/plots/stats_post44_targets_line.png and .svg (1200x800)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(glue)
})

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------
OUT_DIR    <- here::here("stats_posts", "output", "plots")
DATA_DIR   <- here::here("stats_posts", "output", "data")
FILE_STEM  <- "stats_post44_targets_line"
WIDTH_PX   <- 1200
HEIGHT_PX  <- 800
DPI        <- 300

COL_DOT    <- "#0072B2"   # Okabe-Ito navy: actual receivers
COL_LINE   <- "#E69F00"   # Okabe-Ito gold: fitted line over the data
COL_LINE_T <- "#9A6A00"   # darker gold for text on white (same hue, legible)
COL_REF    <- "gray55"    # extension and reference
COL_TEXT   <- "gray20"

# Verified values from the stats_post44_pull_v3.R console output (2026-11-09)
N_WR        <- 133L
SLOPE_TXT   <- "1.87"
INT_TXT     <- "-9.39"
X_MIN       <- 20L
X_MAX       <- 185L
NACUA       <- list(name = "P.Nacua", targets = 166L, points = 375, line = 300)
CHASE       <- list(name = "J.Chase", targets = 185L, points = 314, line = 336)
LINE_AT_150 <- 271
LOW_GROUP   <- list(n = 27L, x_lo = 20L, x_hi = 35L, x_mean = 26, y_mean = 43, line = 39)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

gate <- function(ok, msg) {
  if (!isTRUE(ok)) stop("GATE FAILED: ", msg, call. = FALSE)
  cat("  [PASS] ", msg, "\n", sep = "")
}

# ------------------------------------------------------------------------------
# REPRODUCTION GATE: refit from the pull's CSV and match every quoted value
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
line_at   <- function(x) INTERCEPT + SLOPE * x

gate(sprintf("%.2f", SLOPE) == SLOPE_TXT, "slope 1.87 (post wording)")
gate(sprintf("%.2f", INTERCEPT) == INT_TXT, "intercept -9.39 (post wording)")
gate(round(line_at(150)) == LINE_AT_150, "line about 271 at 150 targets (post wording)")

pick <- function(p) wr[wr$player_name == p$name, ]
nac <- pick(NACUA); cha <- pick(CHASE)
cat(sprintf("  Nacua: %d targets, %.2f points, line %.2f | Chase: %d targets, %.2f points, line %.2f\n",
            nac$targets, nac$total_points, line_at(nac$targets),
            cha$targets, cha$total_points, line_at(cha$targets)))
# "about" in the post means within 1 point of the stated whole number
gate(nrow(nac) == 1L && nac$targets == NACUA$targets && abs(nac$total_points - NACUA$points) < 1 &&
       abs(line_at(nac$targets) - NACUA$line) < 1,
     "Nacua 166 targets, 375 points, line about 300 (post wording)")
gate(abs(nac$total_points - line_at(nac$targets) - 75) < 1,
     "Nacua about 75 above the line (post wording)")
gate(nrow(cha) == 1L && cha$targets == CHASE$targets && abs(cha$total_points - CHASE$points) < 1 &&
       abs(line_at(cha$targets) - CHASE$line) < 1,
     "Chase 185 targets, 314 points, line about 336 (post wording)")
gate(cha$targets == max(wr$targets), "Chase has the most targets in the data")

bins <- wr %>%
  mutate(bin = dplyr::ntile(targets, 5)) %>%
  group_by(bin) %>%
  summarise(n = n(), x_lo = min(targets), x_hi = max(targets),
            x_mean = mean(targets), y_mean = mean(total_points), .groups = "drop") %>%
  mutate(gap = abs(y_mean - line_at(x_mean)))
low <- bins[bins$bin == 1, ]
gate(low$n == LOW_GROUP$n && low$x_lo == LOW_GROUP$x_lo && low$x_hi == LOW_GROUP$x_hi &&
       round(low$x_mean) == LOW_GROUP$x_mean && round(low$y_mean) == LOW_GROUP$y_mean &&
       round(line_at(low$x_mean)) == LOW_GROUP$line,
     "lowest group: 27 receivers, 20 to 35 targets, 26 targets, 43 points, line 39")
gate(round(low$gap) == 4 && all(bins$gap[bins$bin != 1] < low$gap),
     "lowest group 4 points off; the other four groups sit closer")
gate(round(bins$y_mean[bins$bin == 5]) == 230, "top group averages 230 points")

# ------------------------------------------------------------------------------
# PLOT DATA
# ------------------------------------------------------------------------------
fit_seg  <- tibble(x = X_MIN, xend = X_MAX, y = line_at(X_MIN), yend = line_at(X_MAX))
ext_seg  <- tibble(x = 0, xend = X_MIN, y = INTERCEPT, yend = line_at(X_MIN))
callouts <- tibble(
  name    = c("Puka Nacua", "Ja'Marr Chase"),
  targets = c(nac$targets, cha$targets),
  points  = c(nac$total_points, cha$total_points),
  line    = c(line_at(nac$targets), line_at(cha$targets))
)
others <- wr %>% filter(!player_name %in% c(NACUA$name, CHASE$name))
stopifnot(nrow(others) == N_WR - 2L)

X_LIM <- c(0, 200)
Y_LIM <- c(-40, 410)
TXT   <- 2.0   # annotation size, mm

# ------------------------------------------------------------------------------
# BUILD
# ------------------------------------------------------------------------------
p <- ggplot() +
  # Layer 1: empty band, fewer than 20 targets
  annotate("rect", xmin = 0, xmax = X_MIN, ymin = -Inf, ymax = Inf, fill = "gray95") +
  # Layer 2: zero reference
  geom_hline(yintercept = 0, color = COL_REF, linewidth = 0.25, linetype = "dotted") +
  # Layer 3: gaps for the two named receivers
  geom_segment(data = callouts, aes(x = targets, xend = targets, y = line, yend = points),
               color = COL_REF, linewidth = 0.35) +
  # Layer 4: the other 131 receivers
  geom_point(data = others, aes(x = targets, y = total_points),
             color = COL_DOT, alpha = 0.45, size = 1.0) +
  # Layer 5: dotted extension back to zero targets (outside the data)
  geom_segment(data = ext_seg, aes(x = x, xend = xend, y = y, yend = yend),
               color = COL_REF, linewidth = 0.6, linetype = "22") +
  # Layer 6: fitted line over the data (eye-first element)
  geom_segment(data = fit_seg, aes(x = x, xend = xend, y = y, yend = yend),
               color = COL_LINE, linewidth = 1.3) +
  # Layer 7: named receivers at full strength
  geom_point(data = callouts, aes(x = targets, y = points), color = COL_DOT, size = 1.7) +
  # Layer 8: intercept marker
  geom_point(aes(x = 0, y = INTERCEPT), shape = 21, fill = "white",
             color = COL_REF, size = 1.6, stroke = 0.5) +
  # Slope label on the line
  annotate("segment", x = 62, xend = 88, y = 262, yend = line_at(88) + 8,
           color = COL_LINE_T, linewidth = 0.3) +
  annotate("text", x = 62, y = 266, hjust = 0.5, vjust = 0,
           label = "Fitted line: 1.87 points\nper extra target",
           size = TXT, color = COL_LINE_T, fontface = "bold", lineheight = 0.9) +
  # Named receivers
  annotate("text", x = nac$targets - 4, y = nac$total_points + 8, hjust = 1, vjust = 0,
           label = "Nacua: 375 vs line 300", size = TXT, color = COL_DOT) +
  annotate("segment", x = cha$targets, xend = cha$targets, y = cha$total_points - 6, yend = 222,
           color = COL_REF, linewidth = 0.25) +
  annotate("text", x = 199, y = 218, hjust = 1, vjust = 1,
           label = "Chase: 314 vs line 336", size = TXT, color = COL_DOT) +
  # Empty band and intercept
  annotate("text", x = 10, y = 400, vjust = 1, label = "No\nreceiver\nunder 20\ntargets",
           size = 1.7, color = "gray40", lineheight = 0.9) +
  annotate("text", x = 3, y = INTERCEPT - 14, hjust = 0, vjust = 1,
           label = "Intercept -9.39, outside the data", size = 1.8, color = "gray35") +
  scale_x_continuous(breaks = c(0, 20, 50, 100, 150, 185), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 400, by = 100)) +
  coord_cartesian(xlim = X_LIM, ylim = Y_LIM, clip = "off") +
  labs(
    title    = "One straight line through 133 receivers",
    subtitle = paste0("2025 wide receivers with 20+ targets. Each extra target came with about\n",
                      "1.87 more fantasy points, on average, across the 20 to 185 targets in the data."),
    x        = "Targets (2025 regular season)",
    y        = "Season fantasy points (full PPR)",
    caption  = "Full PPR: one point per catch plus yards and touchdowns. Toolkit 2025 regular-season data."
  ) +
  theme_minimal(base_size = 7) +
  theme(
    plot.title            = element_text(face = "bold", size = 9, hjust = 0, color = COL_TEXT),
    plot.title.position   = "plot",
    plot.subtitle         = element_text(size = 6.2, hjust = 0, color = "gray35"),
    plot.caption          = element_text(size = 5.0, hjust = 1, color = "gray50"),
    plot.caption.position = "plot",
    axis.title            = element_text(size = 6.2, color = "gray35"),
    axis.text             = element_text(size = 6, color = "gray40"),
    panel.grid.minor      = element_blank(),
    panel.grid.major      = element_line(color = "gray92", linewidth = 0.25),
    plot.margin           = margin(8, 12, 6, 8),
    plot.background       = element_rect(fill = "white", color = NA)
  )

# ------------------------------------------------------------------------------
# POST-BUILD ASSERTIONS
# ------------------------------------------------------------------------------
cat("\nPost-build assertions\n")
gate(inherits(p, "ggplot"), "plot object built")
# Layer order: 1 band, 2 hline, 3 gap segments, 4 dots, 5 extension, 6 fitted
# line, 7 named dots, 8 intercept, then text. Indexed by number for ggplot2 3.x/4.0.
dots_b <- ggplot2::layer_data(p, i = 4L)
ext_b  <- ggplot2::layer_data(p, i = 5L)
fit_b  <- ggplot2::layer_data(p, i = 6L)
nam_b  <- ggplot2::layer_data(p, i = 7L)
gate(nrow(dots_b) + nrow(nam_b) == N_WR, "all 133 receivers drawn")
gate(fit_b$x == X_MIN && fit_b$xend == X_MAX, "fitted line spans only 20 to 185 targets")
gate(ext_b$x == 0 && ext_b$xend == X_MIN && abs(ext_b$y - INTERCEPT) < 1e-9,
     "dotted extension runs from the intercept at 0 to 20 targets")
gate(isTRUE(all.equal(ext_b$yend, fit_b$y)), "extension meets the fitted line at 20 targets")
gate(fit_b$linewidth > ext_b$linewidth, "fitted line is the heaviest line (eye-first)")
gate(all(wr$total_points > Y_LIM[1] & wr$total_points < Y_LIM[2]) &&
       INTERCEPT > Y_LIM[1], "every receiver and the intercept sit inside the view")

# ------------------------------------------------------------------------------
# SAVE
# ------------------------------------------------------------------------------
png_path <- file.path(OUT_DIR, paste0(FILE_STEM, ".png"))
svg_path <- file.path(OUT_DIR, paste0(FILE_STEM, ".svg"))
ggsave(png_path, plot = p, width = WIDTH_PX, height = HEIGHT_PX,
       units = "px", dpi = DPI, bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, plot = p, width = WIDTH_PX, height = HEIGHT_PX,
         units = "px", dpi = DPI, bg = "white")
} else {
  warning("svglite not installed; SVG not written. install.packages('svglite')")
}

# ------------------------------------------------------------------------------
# CONSOLE SUMMARY
# ------------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("Stats Post 44 visual\n")
cat(glue("  n {nrow(wr)} | slope {sprintf('%.4f', SLOPE)} | intercept {sprintf('%.4f', INTERCEPT)} | ",
         "line at 150: {sprintf('%.1f', line_at(150))}"), "\n")
cat("  PNG: ", png_path, "\n", sep = "")
cat("  SVG: ", if (file.exists(svg_path)) svg_path else "not written", "\n", sep = "")
cat(strrep("=", 70), "\n", sep = "")
