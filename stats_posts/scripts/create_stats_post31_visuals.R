# create_stats_post31_visuals.R
# Statistics Series Post 31: Confidence Intervals
#
# One panel. Four horizontal ranges for the same gap (how many fewer fantasy
# points per game the previous-season guess missed the returning group by):
#   95% formula route    (navy, emphasized: the post's headline range)
#   95% redraw route     (gold: neither route is wrong)
#   90% and 99% formula  (thin navy: confidence costs width)
# A dashed line at 0 marks "no difference". The 99% range stops just short.
#
# Every number below is HARDCODED from the verified console output of
# pull_stats_post31_confidence_interval_data.R (2026-09-21). Nothing is
# re-simulated, so the visual cannot drift from the prose.
#
# NOT render-tested in session (no R in the container). Run locally and
# apply the 5-second check to the PNG before calling it done.

suppressPackageStartupMessages({
  library(grid)
  library(ggplot2)
  library(here)
})

# ==============================================================================
# CONFIGURATION
# ==============================================================================

POST_N      <- 31L
OUTPUT_DIR  <- here::here("stats_posts", "output", "plots")
FILE_PREFIX <- sprintf("stats_post%d_", POST_N)
PLOT_WIDTH  <- 8        # inches
PLOT_HEIGHT <- 5.333    # inches (3:2, matches Post 30)
PLOT_DPI    <- 300

POST_PATH <- here::here("stats_posts", "output", "posts", "Stats_Post31_Confidence_Intervals.md")

COL_ACTUAL  <- "#0072B2"   # navy: actual values
COL_NEITHER <- "#E69F00"   # gold: second route, neither is wrong
COL_ZERO    <- "gray45"

# Verified console values (sign flipped: positive = fewer points missed)
GAP        <- 0.707948
SE_WELCH   <- 0.264619
T_CRIT_95  <- 1.977723
MARGIN_95  <- 0.523343
N_TRT      <- 115L
N_CTL      <- 1829L
MEAN_TRT   <- 3.140398
MEAN_CTL   <- 3.848346

W90 <- c(lo = 0.269676, hi = 1.146220)
W95 <- c(lo = 0.184605, hi = 1.231291)
W99 <- c(lo = 0.016551, hi = 1.399345)
P95 <- c(lo = 0.172402, hi = 1.214901)   # middle 95% of 1,000 redrawn gaps

PRIOR_TRT <- 8.29
PRIOR_CTL <- 12.1

PLOT_TITLE    <- "Returning players were missed by 0.18 to 1.23\nfewer fantasy points per game"
PLOT_SUBTITLE <- paste0(
  "Confidence ranges for one gap, 2011 to 2025 (115 vs 1,829 player-seasons).\n",
  "More confidence, wider range: the 99% range stops just short of zero."
)
PLOT_CAPTION <- paste0(
  "Formula: gap \u00b1 1.978 \u00d7 standard error (gap plus or minus 1.978 times the standard error).\n",
  "Redraw: middle 95% of 1,000 gaps from players redrawn at random, repeats allowed.\n",
  "Data: nflfastR via NFL Analytics Toolkit injury-proximity experiment."
)

# ==============================================================================
# PRE-BUILD ASSERTIONS (numbers and the post's prose claims)
# ==============================================================================

r2 <- function(x) formatC(round(x, 2), format = "f", digits = 2)

stopifnot(
  "A01 gap equals difference of group means" =
    abs((MEAN_CTL - MEAN_TRT) - GAP) < 1e-6,
  "A02 margin equals t_crit times SE" =
    abs(T_CRIT_95 * SE_WELCH - MARGIN_95) < 1e-5,
  "A03 95% bounds equal gap -/+ margin" =
    abs((GAP - MARGIN_95) - W95[["lo"]]) < 1e-5 &&
    abs((GAP + MARGIN_95) - W95[["hi"]]) < 1e-5,
  "A04 post equation: 1.978 x 0.2646 rounds to 0.523" =
    round(1.978 * 0.2646, 3) == 0.523,
  "A05 post equation: 0.708 -/+ 0.523 gives 0.185 and 1.231" =
    round(0.708 - 0.523, 3) == 0.185 && round(0.708 + 0.523, 3) == 1.231,
  "A06 multiplier close to 2" = abs(T_CRIT_95 - 2) < 0.05,
  "A07 widths increase with confidence" =
    diff(W90) < diff(W95) && diff(W95) < diff(W99),
  "A08 no range contains zero" =
    W90[["lo"]] > 0 && W95[["lo"]] > 0 && W99[["lo"]] > 0 && P95[["lo"]] > 0,
  "A09 99% lower bound is 'just short of zero' (rounds to 0.02)" =
    r2(W99[["lo"]]) == "0.02",
  "A10 routes within 0.02 at both ends" =
    abs(W95[["lo"]] - P95[["lo"]]) < 0.02 && abs(W95[["hi"]] - P95[["hi"]]) < 0.02,
  "A11 prose rounding: 95% range reads 0.18 to 1.23" =
    r2(W95[["lo"]]) == "0.18" && r2(W95[["hi"]]) == "1.23",
  "A12 prose rounding: redraw range reads 0.17 to 1.21" =
    r2(P95[["lo"]]) == "0.17" && r2(P95[["hi"]]) == "1.21",
  "A13 prose rounding: 90% reads 0.27 to 1.15, 99% reads 0.02 to 1.40" =
    r2(W90[["lo"]]) == "0.27" && r2(W90[["hi"]]) == "1.15" &&
    r2(W99[["hi"]]) == "1.40",
  "A14 prose rounding: hook means and gap" =
    r2(MEAN_TRT) == "3.14" && r2(MEAN_CTL) == "3.85" && r2(GAP) == "0.71",
  "A15 prior scoring direction (returning group scored less)" =
    PRIOR_TRT < PRIOR_CTL
)

# A16: every number the visual prints also appears in the delivered post text
if (file.exists(POST_PATH)) {
  post_txt <- paste(readLines(POST_PATH, warn = FALSE, encoding = "UTF-8"),
                    collapse = "\n")
  must_appear <- c("0.18", "1.23", "0.17", "1.21", "0.27", "1.15",
                   "0.02", "1.40", "1.978", "115", "1,829", "2011", "2025")
  missing_nums <- must_appear[!vapply(must_appear, grepl, logical(1),
                                      x = post_txt, fixed = TRUE)]
  stopifnot("A16 visual numbers appear in post" = length(missing_nums) == 0)
  message("A16 checked against ", POST_PATH)
} else {
  message("A16 SKIPPED: post file not at ", POST_PATH,
          " (place the delivered .md there to enable)")
}
message("Pre-build assertions passed: 15/15 (+A16 as reported above)")

# ==============================================================================
# DATA
# ==============================================================================

row_levels <- c("99% formula", "95% redraw", "95% formula", "90% formula")
# ggplot draws discrete y bottom-to-top: first level at the bottom.
# Reading order top to bottom: 90% formula, 95% formula, 95% redraw, 99% formula.

plot_data <- data.frame(
  row   = factor(c("90% formula", "95% formula", "95% redraw", "99% formula"),
                 levels = row_levels),
  lo    = c(W90[["lo"]], W95[["lo"]], P95[["lo"]], W99[["lo"]]),
  hi    = c(W90[["hi"]], W95[["hi"]], P95[["hi"]], W99[["hi"]]),
  col   = c(COL_ACTUAL,  COL_ACTUAL,  COL_NEITHER, COL_ACTUAL),
  lwd   = c(2.2,         5.0,         3.2,         2.2),
  lab_sz = c(3.4,        4.6,         3.8,         3.4),
  face  = c("plain",     "bold",      "plain",     "plain"),
  stringsAsFactors = FALSE
)
plot_data$lo_lab <- r2(plot_data$lo)
plot_data$hi_lab <- r2(plot_data$hi)

stopifnot(
  "A17 four rows, no NA" = nrow(plot_data) == 4 && !anyNA(plot_data[, c("lo", "hi")]),
  "A18 plotted bounds are the verified values" =
    identical(plot_data$lo, c(W90[["lo"]], W95[["lo"]], P95[["lo"]], W99[["lo"]]))
)

# ==============================================================================
# PLOT
# ==============================================================================

p <- ggplot(plot_data, aes(y = row)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = COL_ZERO,
             linewidth = 0.6) +
  annotate("text", x = 0.02, y = 4.62, label = "no difference",
           hjust = 0, size = 3.3, color = COL_ZERO) +
  geom_segment(aes(x = lo, xend = hi, yend = row),
               color = plot_data$col, linewidth = plot_data$lwd,
               lineend = "round") +
  geom_point(aes(x = GAP), shape = 21, size = 3.2, stroke = 1,
             fill = "white", color = "gray20") +
  # Bound labels ABOVE the segments to avoid collisions with geometry
  geom_text(aes(x = lo, label = lo_lab), vjust = -1.1,
            size = plot_data$lab_sz, fontface = plot_data$face,
            color = plot_data$col) +
  geom_text(aes(x = hi, label = hi_lab), vjust = -1.1,
            size = plot_data$lab_sz, fontface = plot_data$face,
            color = plot_data$col) +
  annotate("text", x = GAP, y = 4.62, label = "gap 0.71",
           size = 3.3, color = "gray20") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25)) +
  scale_y_discrete(expand = expansion(add = c(0.55, 0.9))) +
  coord_cartesian(xlim = c(-0.05, 1.55), clip = "off") +
  labs(
    title    = PLOT_TITLE,
    subtitle = PLOT_SUBTITLE,
    caption  = PLOT_CAPTION,
    x        = "Fewer fantasy points per game missed (returning group vs comparison group)",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 15.5, hjust = 0),
    plot.subtitle      = element_text(color = "gray35", size = 11, hjust = 0,
                                      margin = margin(b = 8)),
    plot.caption       = element_text(color = "gray45", size = 8.5, hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    axis.text.y        = element_text(size = 11.5, color = "gray20"),
    axis.title.x       = element_text(size = 10.5, color = "gray30",
                                      margin = margin(t = 6)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position    = "none",
    plot.margin        = margin(14, 20, 10, 14)
  )

# ==============================================================================
# POST-BUILD RENDER CHECKS
# ==============================================================================

b <- ggplot_build(p)
usable_in <- PLOT_WIDTH - (20 + 14) / 72

text_fits <- function(txt, fontsize, fontface = "plain") {
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  w <- max(vapply(lines, function(s) {
    grid::convertWidth(grid::grobWidth(grid::textGrob(
      s, gp = grid::gpar(fontsize = fontsize, fontface = fontface))),
      "in", valueOnly = TRUE)
  }, numeric(1)))
  w <= usable_in
}

stopifnot(
  "R01 seven layers built" = length(b$data) == 7,
  "R02 four segments drawn" = nrow(b$data[[3]]) == 4,
  "R03 segment x-ends are the verified bounds" =
    isTRUE(all.equal(sort(b$data[[3]]$x), sort(plot_data$lo))) &&
    isTRUE(all.equal(sort(b$data[[3]]$xend), sort(plot_data$hi))),
  "R04 95% formula row drawn thickest (5-second focus)" =
    plot_data$lwd[plot_data$row == "95% formula"] == max(plot_data$lwd),
  "R05 y order top to bottom: 90, 95 formula, 95 redraw, 99" =
    identical(levels(plot_data$row), row_levels),
  "R06 zero line present at x = 0" = b$data[[1]]$xintercept[1] == 0,
  "R07 x view includes zero and the widest bound" =
    -0.05 < 0 && 1.55 > W99[["hi"]],
  "R08 title fits device"    = text_fits(PLOT_TITLE, 15.5, "bold"),
  "R09 subtitle fits device" = text_fits(PLOT_SUBTITLE, 11),
  "R10 caption fits device"  = text_fits(PLOT_CAPTION, 8.5),
  "R11 x-axis title fits device" =
    text_fits("Fewer fantasy points per game missed (returning group vs comparison group)", 10.5)
)
message("Post-build render checks passed: 11/11")

# ==============================================================================
# EXPORT
# ==============================================================================

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
png_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "confidence_ranges.png"))
svg_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "confidence_ranges.svg"))

ggsave(png_path, p, width = PLOT_WIDTH, height = PLOT_HEIGHT,
       dpi = PLOT_DPI, bg = "white")

if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, p, width = PLOT_WIDTH, height = PLOT_HEIGHT,
         device = svglite::svglite, bg = "white")
  svg_msg <- svg_path
} else {
  svg_msg <- "SKIPPED (svglite not installed)"
}

# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat("\n=== Stats Post 31 visual ===\n")
cat("PNG:", png_path, "\n")
cat("SVG:", svg_msg, "\n")
cat(sprintf("95%% formula range: %s to %s (gap %s +/- %s)\n",
            r2(W95[["lo"]]), r2(W95[["hi"]]), r2(GAP), formatC(MARGIN_95, format = "f", digits = 3)))
cat(sprintf("95%% redraw range:  %s to %s\n", r2(P95[["lo"]]), r2(P95[["hi"]])))
cat(sprintf("90%% / 99%% ranges:  %s to %s / %s to %s\n",
            r2(W90[["lo"]]), r2(W90[["hi"]]), r2(W99[["lo"]]), r2(W99[["hi"]])))
cat("5-SECOND CHECK (manual): is the thick navy 95% bar and its 0.18 / 1.23\n",
    "labels the first thing the eye lands on? If not, rebuild.\n")
