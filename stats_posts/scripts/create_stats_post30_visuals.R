# create_stats_post30_visuals.R
# Statistics Series Post 30: The Paired t-Test
#
# One panel. The same 0.42-point gap (three-game average error minus
# season-to-date error, per quarterback) drawn twice: once with the standard
# error from treating the two sets of misses as separate groups, once with the
# paired standard error. The width difference is the post's central finding.
#
# Every number is hardcoded from verified console output
# (floor >= 8 eligible weeks block, rebuilt A/B panel, 2026-09-17).
# Nothing is re-simulated or recomputed from data.
#
# Bars show the gap plus or minus ONE standard error. They are not confidence
# intervals, and nothing in the chart labels them as intervals.

# ==== CONFIGURATION ==========================================================
library(ggplot2)
library(dplyr)
library(grid)
library(here)

OUTPUT_DIR  <- here("stats_posts", "output", "plots")
FILE_PREFIX <- "stats_post30_"
PLOT_NAME   <- "paired_vs_separate_standard_error"
WIDTH_IN    <- 8
HEIGHT_IN   <- 8 * 2 / 3   # 1200x800 aspect
DPI         <- 300
BASE_SIZE   <- 13
TITLE_SIZE  <- 16
SUB_SIZE    <- 11
MARGIN_PT   <- c(t = 14, r = 20, b = 12, l = 14)

COL_PAIRED   <- "#0072B2"  # navy: the reading the post teaches
COL_SEPARATE <- "#E69F00"  # gold: not wrong, less precise

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ==== VERIFIED VALUES (console output, floor >= 8 block) =====================
N_QB          <- 133L
ERR_STD       <- 7.985064
ERR_ROLL3     <- 8.400999
GAP           <- 0.415935
VAR_STD       <- 2.213179
VAR_ROLL3     <- 2.588883
VAR_DIFF      <- 0.491382
SE_SEPARATE   <- 0.190015
SE_PAIRED     <- 0.060783
QB_ROLL3_WORSE <- 104L
QB_STD_WORSE   <- 29L
QB_TIES        <- 0L

# ==== PRE-BUILD ASSERTIONS ===================================================
tol <- 1e-4
checks <- c(
  # Internal consistency of the hardcoded values
  A01_gap_reconciles        = abs((ERR_ROLL3 - ERR_STD) - GAP) < 1e-5,
  A02_separate_se_from_vars = abs(sqrt((VAR_STD + VAR_ROLL3) / N_QB) - SE_SEPARATE) < tol,
  A03_paired_se_from_var    = abs(sqrt(VAR_DIFF / N_QB) - SE_PAIRED) < tol,
  A04_win_counts_sum        = QB_ROLL3_WORSE + QB_STD_WORSE + QB_TIES == N_QB,
  A05_positive_values       = all(c(GAP, SE_SEPARATE, SE_PAIRED, VAR_DIFF) > 0),
  # Prose claims in Stats_Post30_The_Paired_t_Test.md, checked in code
  P01_miss_7985             = sprintf("%.3f", ERR_STD) == "7.985",
  P02_miss_8401             = sprintf("%.3f", ERR_ROLL3) == "8.401",
  P03_gap_0416_about_042    = sprintf("%.3f", GAP) == "0.416" && sprintf("%.2f", GAP) == "0.42",
  P04_separate_se_019       = sprintf("%.2f", SE_SEPARATE) == "0.19",
  P05_paired_se_006         = sprintf("%.2f", SE_PAIRED) == "0.06",
  P06_variances_221_259     = sprintf("%.2f", VAR_STD) == "2.21" && sprintf("%.2f", VAR_ROLL3) == "2.59",
  P07_sum_480               = sprintf("%.2f", VAR_STD + VAR_ROLL3) == "4.80",
  P08_diff_var_049          = sprintf("%.2f", VAR_DIFF) == "0.49",
  P09_about_90_percent      = round(100 * (1 - VAR_DIFF / (VAR_STD + VAR_ROLL3))) == 90,
  P10_104_of_133            = QB_ROLL3_WORSE == 104L && N_QB == 133L,
  P11_title_three_times     = round(SE_SEPARATE / SE_PAIRED) == 3
)
if (!all(checks)) {
  stop("Pre-build assertion(s) failed: ", paste(names(checks)[!checks], collapse = ", "))
}
cat("Pre-build assertions:", sum(checks), "/", length(checks), "passed\n")

# ==== PLOT DATA ==============================================================
plot_data <- tibble(
  reading = factor(c("Treated as separate groups", "Paired"),
                   levels = c("Paired", "Treated as separate groups")),  # separate on top
  gap     = GAP,
  se      = c(SE_SEPARATE, SE_PAIRED),
  label   = c(
    sprintf("Separate groups: %.2f \u00b1 %.2f\nvariances add: %.2f + %.2f = %.2f",
            GAP, SE_SEPARATE, VAR_STD, VAR_ROLL3, VAR_STD + VAR_ROLL3),
    sprintf("Paired: %.2f \u00b1 %.2f\nvariance of the %d differences: %.2f",
            GAP, SE_PAIRED, N_QB, VAR_DIFF)
  ),
  colour  = c(COL_SEPARATE, COL_PAIRED)
) %>%
  mutate(lo = gap - se, hi = gap + se)

stopifnot(nrow(plot_data) == 2, !anyNA(plot_data))

# ==== TEXT ===================================================================
TITLE    <- "Pairing measured the same gap about three times more tightly"
SUBTITLE <- sprintf("Extra points missed by the three-game average, per quarterback (%d QBs, 2010 to 2025)", N_QB)
CAPTION  <- "133 quarterbacks with at least 8 eligible weeks | Data: nflfastR via my NFL analytics toolkit"

# ==== BUILD ==================================================================
p <- ggplot(plot_data, aes(y = reading)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray60") +
  geom_linerange(aes(xmin = lo, xmax = hi, colour = colour), linewidth = 9,
                 alpha = 0.35, show.legend = FALSE) +
  geom_point(aes(x = gap, colour = colour), size = 4.5, show.legend = FALSE) +
  # Labels sit above each bar so they never collide with the geometry
  geom_text(aes(x = 0.02, label = label, colour = colour),
            hjust = 0, vjust = 0, nudge_y = 0.22, size = 3.9, lineheight = 1.05,
            show.legend = FALSE) +
  scale_colour_identity() +
  scale_x_continuous(breaks = seq(0, 0.7, by = 0.1)) +
  scale_y_discrete(expand = expansion(add = c(0.45, 0.85))) +
  coord_cartesian(xlim = c(0, 0.72)) +
  labs(
    title    = TITLE,
    subtitle = SUBTITLE,
    x        = "Gap in fantasy points (bar spans one standard error either side)",
    y        = NULL,
    caption  = CAPTION
  ) +
  theme_minimal(base_size = BASE_SIZE) +
  theme(
    plot.title         = element_text(face = "bold", size = TITLE_SIZE, hjust = 0),
    plot.subtitle      = element_text(colour = "gray35", size = SUB_SIZE, hjust = 0),
    plot.caption       = element_text(colour = "gray50", size = 9, hjust = 1),
    plot.title.position = "plot",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_blank(),
    axis.title.x       = element_text(colour = "gray30", size = 11),
    legend.position    = "none",
    plot.margin        = margin(MARGIN_PT["t"], MARGIN_PT["r"], MARGIN_PT["b"], MARGIN_PT["l"])
  )

# ==== POST-BUILD RENDER CHECKS ===============================================
b <- ggplot_build(p)
text_fits <- function(txt, size_pt, face = "plain") {
  w_in <- convertWidth(grobWidth(textGrob(txt, gp = gpar(fontsize = size_pt, fontface = face))),
                       "inches", valueOnly = TRUE)
  avail <- WIDTH_IN - (MARGIN_PT["l"] + MARGIN_PT["r"]) / 72
  w_in <= avail
}
pdf(NULL, width = WIDTH_IN, height = HEIGHT_IN)  # measurement device
render <- c(
  R01_two_bars          = nrow(b$data[[2]]) == 2,
  R02_two_points        = nrow(b$data[[3]]) == 2,
  R03_bar_ends_match    = isTRUE(all.equal(sort(b$data[[2]]$xmax), sort(plot_data$hi))),
  R04_points_at_gap     = all(abs(b$data[[3]]$x - GAP) < 1e-9),
  R05_bars_inside_view  = all(plot_data$lo >= 0 & plot_data$hi <= 0.72),
  R06_separate_wider    = diff(range(plot_data$hi - plot_data$lo)) > 0,
  R07_title_fits        = text_fits(TITLE, TITLE_SIZE, "bold"),
  R08_subtitle_fits     = text_fits(SUBTITLE, SUB_SIZE),
  R09_caption_fits      = text_fits(CAPTION, 9),
  R10_labels_fit        = all(vapply(unlist(strsplit(plot_data$label, "\n")),
                                     function(s) {
                                       w <- convertWidth(grobWidth(textGrob(s, gp = gpar(fontsize = 3.9 * .pt))),
                                                         "inches", valueOnly = TRUE)
                                       w <= WIDTH_IN * 0.72  # label starts near x = 0.02 of panel
                                     }, logical(1)))
)
dev.off()
if (!all(render)) {
  stop("Render check(s) failed: ", paste(names(render)[!render], collapse = ", "))
}
cat("Post-build render checks:", sum(render), "/", length(render), "passed\n")
cat("NOTE: R01 to R04 introspect ggplot_build() layer order; R07 to R10 measure text",
    "on a null PDF device. Both are unverified until rendered locally.\n")

# ==== EXPORT =================================================================
png_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, PLOT_NAME, ".png"))
ggsave(png_path, plot = p, width = WIDTH_IN, height = HEIGHT_IN, dpi = DPI, bg = "white")

svg_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, PLOT_NAME, ".svg"))
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, plot = p, width = WIDTH_IN, height = HEIGHT_IN, bg = "white")
} else {
  message("svglite not installed: SVG skipped. install.packages('svglite') to enable.")
  svg_path <- NA_character_
}

stopifnot(file.exists(png_path), file.size(png_path) > 0)

# ==== CONSOLE SUMMARY ========================================================
cat("\n==== Post 30 visual summary ====\n")
cat("PNG:", png_path, "\n")
cat("SVG:", ifelse(is.na(svg_path), "skipped", svg_path), "\n")
cat(sprintf("Gap %.3f | separate SE %.3f | paired SE %.3f | ratio %.2f\n",
            GAP, SE_SEPARATE, SE_PAIRED, SE_SEPARATE / SE_PAIRED))
cat("5-second check (do this by eye): the gold bar should read about three times",
    "the width of the navy bar, at the same center.\n")
