# =============================================================================
# create_stats_post37_visuals.R
# Statistics Series, Post 37: Non-Parametric Alternatives
#
# PURPOSE (the one thing the chart shows that the post text cannot):
#   The post says a long tail of big elastic net wins pulled the average up.
#   The chart shows that tail: the 611 gaps reach further to the right
#   (elastic net closer) than to the left, set against the two tests on one
#   ruler, one past the 1.96 bar and one short of it.
#
# Every number below is hardcoded from verified console output
# (pull_stats_post37_nonparametric_data.R, sections S2, S4, S5, run
# 2026-10-14) or from arithmetic on it. Nothing is re-simulated and no data
# file is read, so the chart cannot drift from the post.
#
# Comparison shown: rushers, XGBoost vs elastic net, 611 player-weeks,
# weeks 15 to 18 of 2023, 2024 and 2025. Gap = XGBoost miss minus elastic
# net miss (fantasy points). Positive gap = elastic net closer.
#
# Output: stats_posts/output/plots/stats_post37_two_tests_gap_tail.png
#         (8 x 5.333 in at 150 DPI = 1200 x 800 px, Post 36 convention)
#         plus .svg when svglite is installed.
#
# Assumed versions: R >= 4.3, ggplot2 >= 4.0 (layer_data, @ access),
# patchwork >= 1.2. Pure ASCII file.
# =============================================================================

library(ggplot2)
library(patchwork)
library(grid)

# ------------------------------ CONFIGURATION --------------------------------
TOOLKIT_ROOT <- here::here()
stopifnot(dir.exists(TOOLKIT_ROOT))
OUTPUT_DIR   <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "plots")
FILE_STEM    <- "stats_post37_two_tests_gap_tail"
POST_PATH    <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "posts",
                          "Stats_Post37_Non_Parametric_Alternatives.md")
WIDTH_IN     <- 8
HEIGHT_IN    <- 16 / 3          # 5.333 in
DPI          <- 150

COL_NAVY <- "#0072B2"           # actual values
COL_GOLD <- "#E69F00"           # second compared value, neither is wrong
COL_GREY <- "grey45"
COL_TAIL <- "grey70"

# --------------------------- VERIFIED VALUES ---------------------------------
N_PAIRS     <- 611L             # S2 rusher rows
MEAN_GAP    <- 0.202            # S5 mean_gap 0.202
SE_MEAN     <- 0.0777           # 1.92 / sqrt(611), S4 sd_d 1.92
T_STAT      <- 2.60             # S5 t_stat 2.60
V_STAT      <- 101378           # S5 w_V 101378
RANK_TOTAL  <- 186966           # 611 * 612 / 2
RANK_HALF   <- 93483            # RANK_TOTAL / 2
SE_V        <- 4365             # sqrt(611 * 612 * 1223 / 24) = 4365.2
Z_RANK      <- 1.81             # (101378 - 93483) / 4365.2 = 1.809
BAR         <- 1.96             # qnorm(0.975); t bar at df 610 is 1.964
MEDIAN_GAP  <- 0.070            # S4 median_d 0.0704
GAP_MIN     <- -6.71            # S4 min_d
GAP_MAX     <- 8.59             # S4 max_d
Q05 <- -2.54; Q25 <- -1.02; Q75 <- 1.21; Q95 <- 3.52   # S4 quantiles

# ------------------------- PRE-BUILD ASSERTIONS ------------------------------
# A1-A14 replicated in Python before delivery. A15 checks the post text.
near <- function(a, b, tol) abs(a - b) <= tol
stopifnot(
  "A1 t = mean / SE"              = near(MEAN_GAP / SE_MEAN, T_STAT, 0.005),
  "A2 SE of mean from sd"         = near(1.92 / sqrt(N_PAIRS), SE_MEAN, 0.0005),
  "A3 rank total"                 = RANK_TOTAL == N_PAIRS * (N_PAIRS + 1) / 2,
  "A4 rank half"                  = RANK_HALF * 2 == RANK_TOTAL,
  "A5 V minus half"               = V_STAT - RANK_HALF == 7895,
  "A6 SE of rank sum"             = near(sqrt(N_PAIRS * (N_PAIRS + 1) *
                                       (2 * N_PAIRS + 1) / 24), SE_V, 0.5),
  "A7 z for rank test"            = near((V_STAT - RANK_HALF) / SE_V, Z_RANK, 0.005),
  "A8 bar is the 97.5th pct"      = near(qnorm(0.975), BAR, 0.001),
  "A9 t-test past the bar"        = T_STAT > BAR,
  "A10 rank test short of bar"    = Z_RANK < BAR,
  "A11 quantiles ordered"         = all(diff(c(GAP_MIN, Q05, Q25, MEDIAN_GAP,
                                                Q75, Q95, GAP_MAX)) > 0),
  "A12 mean right of median"      = MEAN_GAP > MEDIAN_GAP,
  "A13 right reach beats left"    = (GAP_MAX - MEDIAN_GAP) > (MEDIAN_GAP - GAP_MIN),
  "A14 right 95 beats left 5"     = (Q95 - MEDIAN_GAP) > (MEDIAN_GAP - Q05)
)

# A15: every figure the chart shares with the post appears in the post text.
if (file.exists(POST_PATH)) {
  post_txt <- paste(readLines(POST_PATH, warn = FALSE), collapse = " ")
  shared   <- c("611", "0.202", "0.0777", "2.60", "186,966", "93,483",
                "101,378", "7,895", "1.81", "4,365", "1.96", "8.59", "0.070")
  missing_in_post <- shared[!vapply(shared, function(s)
    grepl(s, post_txt, fixed = TRUE), logical(1))]
  if (length(missing_in_post) > 0) {
    stop("A15 failed. Not found in post: ", paste(missing_in_post, collapse = ", "))
  }
  cat("A15 passed: all", length(shared), "shared figures found in the post.\n")
} else {
  warning("A15 skipped: post file not found at ", POST_PATH,
          ". Copy the .md there to run the post-text check.")
}
cat("Pre-build assertions A1 to A14 passed.\n")

# ------------------------------ PANEL A --------------------------------------
# Both tests on one ruler: distance from no difference, in standard errors.
tests <- data.frame(
  test  = factor(c("Signed-rank test (ranks)", "Paired t-test (average)"),
                 levels = c("Signed-rank test (ranks)", "Paired t-test (average)")),
  se    = c(Z_RANK, T_STAT),
  col   = c(COL_GOLD, COL_NAVY),
  label = c("1.81: short of the bar", "2.60: past the bar"),
  hj    = c(1, 0)    # short label ends at its dot, past label starts at its dot,
                     # so neither crosses the dashed bar line
)

p_a <- ggplot(tests, aes(y = test)) +
  geom_vline(xintercept = 0, colour = COL_GREY, linewidth = 0.4) +
  geom_vline(xintercept = BAR, colour = COL_GREY, linewidth = 0.7,
             linetype = "dashed") +
  geom_segment(aes(x = 0, xend = se, yend = test), colour = tests$col,
               linewidth = 2.2) +
  geom_point(aes(x = se), colour = tests$col, size = 4.5) +
  geom_text(aes(x = se, label = label, hjust = hj), colour = tests$col,
            nudge_y = 0.32, size = 3.6, fontface = "bold") +
  annotate("text", x = BAR, y = 2.62, label = "Bar: 1.96",
           colour = COL_GREY, size = 3.3, hjust = 0.5) +
  scale_x_continuous(breaks = 0:3) +
  scale_y_discrete(expand = expansion(add = c(0.45, 0.75))) +
  coord_cartesian(xlim = c(0, 3.5), clip = "off") +
  labs(x = "Distance from no difference, in standard errors", y = NULL,
       title = "Same 611 games, two tests") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", size = 11),
        axis.text.y = element_text(size = 10, colour = "grey20"),
        plot.margin = margin(4, 12, 6, 4))

# ------------------------------ PANEL B --------------------------------------
# Spread of the 611 gaps: whisker to min and max, band 5th to 95th pct,
# box 25th to 75th pct, white tick at the median.
spread <- data.frame(y = "611 gaps")
ends <- data.frame(
  x     = c(GAP_MIN, GAP_MAX, Q05, Q95),
  y     = "611 gaps",
  label = c("-6.71", "8.59", "5th pct -2.54", "95th pct 3.52"),
  vjust = c(-1.6, -1.6, 2.4, 2.4)
)

p_b <- ggplot(spread, aes(y = y)) +
  geom_vline(xintercept = 0, colour = COL_GREY, linewidth = 0.4,
             linetype = "dashed") +
  geom_segment(aes(x = GAP_MIN, xend = GAP_MAX, yend = y),
               colour = COL_TAIL, linewidth = 0.9) +
  geom_segment(aes(x = Q05, xend = Q95, yend = y),
               colour = COL_NAVY, linewidth = 3.5, alpha = 0.55) +
  geom_segment(aes(x = Q25, xend = Q75, yend = y),
               colour = COL_NAVY, linewidth = 9) +
  geom_point(aes(x = MEDIAN_GAP), colour = "white", size = 2.6, shape = 15) +
  geom_point(aes(x = GAP_MIN), colour = COL_TAIL, size = 2.4) +
  geom_point(aes(x = GAP_MAX), colour = COL_TAIL, size = 2.4) +
  geom_text(data = ends, aes(x = x, y = y, label = label, vjust = vjust),
            colour = "grey25", size = 3.2) +
  annotate("text", x = -4.4, y = 1.62, label = "XGBoost closer",
           colour = "grey25", size = 3.3, fontface = "italic") +
  annotate("text", x = 5.2, y = 1.62, label = "Elastic net closer",
           colour = "grey25", size = 3.3, fontface = "italic") +
  scale_x_continuous(breaks = seq(-6, 8, by = 2)) +
  scale_y_discrete(expand = expansion(add = c(0.9, 0.9))) +
  coord_cartesian(xlim = c(-7.4, 9.3), clip = "off") +
  labs(x = "Gap: XGBoost miss minus elastic net miss (fantasy points)",
       y = NULL,
       title = "The right tail reaches further. Average 0.202, median 0.070") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", size = 11),
        axis.text.y = element_text(size = 10, colour = "grey20"),
        plot.margin = margin(4, 12, 4, 4))

# ------------------------------ ASSEMBLE -------------------------------------
p <- (p_a / p_b) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title    = "Past the bar by average, short of it by rank",
    subtitle = "Rushers, weeks 15 to 18 of 2023, 2024 and 2025. Positive gap = elastic net closer.",
    caption  = "Data: my NFL toolkit forecasts. Box = middle half of gaps, band = 5th to 95th pct.",
    theme    = theme(plot.title = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(size = 10, colour = "grey30"),
                     plot.caption = element_text(size = 8, colour = "grey45"))
  )

# --------------------------- RENDER CHECKS -----------------------------------
# R1-R14 read the built layers with layer_data() (ggplot2 4.0 safe).
ld_a_seg <- layer_data(p_a, 3)
ld_a_pt  <- layer_data(p_a, 4)
ld_a_bar <- layer_data(p_a, 2)
ld_b_box <- layer_data(p_b, 4)
ld_b_bnd <- layer_data(p_b, 3)
ld_b_wsk <- layer_data(p_b, 2)
ld_b_med <- layer_data(p_b, 5)
stopifnot(
  "R1 two test points"          = nrow(ld_a_pt) == 2,
  "R2 points at 1.81 and 2.60"  = isTRUE(all.equal(sort(ld_a_pt$x), c(Z_RANK, T_STAT))),
  "R3 segments start at 0"      = all(ld_a_seg$x == 0),
  "R4 bar line at 1.96"         = isTRUE(all.equal(ld_a_bar$xintercept, BAR)),
  "R5 exactly one point past"   = sum(ld_a_pt$x > BAR) == 1,
  "R6 navy point is the past one" = toupper(ld_a_pt$colour[ld_a_pt$x > BAR]) == COL_NAVY,
  "R7 box spans 25th to 75th"   = isTRUE(all.equal(c(ld_b_box$x, ld_b_box$xend), c(Q25, Q75))),
  "R8 band spans 5th to 95th"   = isTRUE(all.equal(c(ld_b_bnd$x, ld_b_bnd$xend), c(Q05, Q95))),
  "R9 whisker spans min to max" = isTRUE(all.equal(c(ld_b_wsk$x, ld_b_wsk$xend), c(GAP_MIN, GAP_MAX))),
  "R10 median tick at 0.070"    = isTRUE(all.equal(ld_b_med$x, MEDIAN_GAP)),
  "R11 whisker inside view"     = GAP_MIN > -7.4 && GAP_MAX < 9.3,
  "R12 points inside view"      = all(ld_a_pt$x < 3.5),
  "R13 patchwork object"        = inherits(p, "patchwork"),
  "R14 labels anchored at dots" = isTRUE(all.equal(sort(layer_data(p_a, 5)$x), c(Z_RANK, T_STAT)))
)
cat("Render checks R1 to R14 passed.\n")

# ------------------------------ SAVE -----------------------------------------
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
png_path <- file.path(OUTPUT_DIR, paste0(FILE_STEM, ".png"))
ggsave(png_path, plot = p, width = WIDTH_IN, height = HEIGHT_IN, dpi = DPI,
       bg = "white")
stopifnot("PNG written" = file.exists(png_path))

svg_path <- file.path(OUTPUT_DIR, paste0(FILE_STEM, ".svg"))
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, plot = p, width = WIDTH_IN, height = HEIGHT_IN, bg = "white")
} else {
  svg_path <- NA_character_
  message("svglite not installed; SVG skipped. PNG is the shareable file.")
}

# --------------------------- CONSOLE SUMMARY ---------------------------------
cat("\n================ Post 37 visual ================\n")
cat("PNG:", png_path, "\n")
cat("SVG:", ifelse(is.na(svg_path), "skipped", svg_path), "\n")
cat("Size:", WIDTH_IN, "x", round(HEIGHT_IN, 3), "in at", DPI, "DPI =",
    WIDTH_IN * DPI, "x", round(HEIGHT_IN * DPI), "px\n")
cat("Panel A: t-test", T_STAT, "SE (past), signed-rank", Z_RANK,
    "SE (short), bar", BAR, "\n")
cat("Panel B: min", GAP_MIN, "| 5th", Q05, "| 25th", Q25, "| median",
    MEDIAN_GAP, "| 75th", Q75, "| 95th", Q95, "| max", GAP_MAX, "\n")
cat("Right reach from median:", GAP_MAX - MEDIAN_GAP, "| left reach:",
    MEDIAN_GAP - GAP_MIN, "\n")
cat("5-second check: open the PNG and confirm the past-vs-short split and the",
    "longer right tail are the first two things you see.\n")
