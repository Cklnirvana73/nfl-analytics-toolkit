# create_stats_post33_visuals.R
# Statistics Series, Post 33: Two-Way ANOVA.
#
# One panel: a fantasy-point number line.
#   Grey band  : usual range of sampling error for the RB/WR difference, within 2 standard
#                errors of zero (0 +/- 2 x 0.676422 = +/- 1.35 points).
#   Navy point : the observed RB/WR reversal, 0.845725 points (1.25 SE).
#   Dashed line: zero, no difference between the two gaps.
# 5-second read: the reversal sits well inside the band.
#
# Every number is HARDCODED from verified console output:
#   pull_stats_post33_two_way_anova_data.R  (section 5: F, MSW, cell n)
#   pull_stats_post33_gap_se_addon.R        (sections 3 and 4: gaps, SEs, difference)
# Nothing is re-simulated or recomputed from data.
#
# NOT render-tested in session (no R in the container). Run locally and apply
# the 5-second check to the PNG before calling it done.

library(here)
library(ggplot2)
library(grid)

# ---- CONFIGURATION -----------------------------------------------------------
SEASON     <- 2025L
POST_NUM   <- 33L
OUTPUT_DIR <- here::here("stats_posts", "output", "plots")
POST_PATH  <- here::here("stats_posts", "output", "posts", "Stats_Post33_Two_Way_ANOVA.md")
FILE_STEM  <- "stats_post33_rb_wr_reversal_vs_sampling_error"
PLOT_W_IN  <- 8
PLOT_H_IN  <- 5.333          # 3:2, same sizing as Post 32 (1200x800 vs 300 DPI ruling still open)
PLOT_DPI   <- 300
BASE_SIZE  <- 13

NAVY <- "#0072B2"            # Okabe-Ito blue: the actual observed difference
GREY <- "#999999"            # Okabe-Ito grey: sampling-error range (neither value is wrong)
INK  <- "gray20"

# Verified console values
MSW        <- 4.773519       # main pull, section 5
F_INT      <- 0.838613       # main pull, section 5
N_RB       <- c(early = 31L, veteran = 39L)
N_WR       <- c(early = 49L, veteran = 57L)
GAP_RB     <- 0.362641       # add-on, section 3
GAP_WR     <- -0.483084
VAR_GAP_RB <- 0.276382
VAR_GAP_WR <- 0.181165
DIFF       <- 0.845725       # add-on, section 4: RB gap minus WR gap
SE_DIFF    <- 0.676422
RATIO      <- 1.250292

# Derived for the plot
BAND_HALF  <- 2 * SE_DIFF    # usual range of sampling error, as stated in the post
X_LIM      <- c(-1.8, 1.8)

TITLE    <- "The RB/WR reversal is within sampling error"
SUBTITLE <- paste0("Difference between the RB and WR experience gaps, ", SEASON,
                   " season, in fantasy points")
CAPTION  <- paste0("202 QB, RB and WR players with 8+ forecast games. Standard error of the difference: ",
                   sprintf("%.2f", SE_DIFF), " points. ANOVA interaction F = ",
                   sprintf("%.2f", F_INT), ".")

# ---- PRE-BUILD ASSERTIONS ----------------------------------------------------
tol <- 1e-5
stopifnot(
  "A01 difference != RB gap minus WR gap"          = abs((GAP_RB - GAP_WR) - DIFF) < tol,
  "A02 SE != sqrt(var RB gap + var WR gap)"        = abs(sqrt(VAR_GAP_RB + VAR_GAP_WR) - SE_DIFF) < tol,
  "A03 var RB gap != MSW x (1/31 + 1/39)"          = abs(MSW * sum(1 / N_RB) - VAR_GAP_RB) < tol,
  "A04 var WR gap != MSW x (1/49 + 1/57)"          = abs(MSW * sum(1 / N_WR) - VAR_GAP_WR) < tol,
  "A05 ratio != difference / SE"                   = abs(DIFF / SE_DIFF - RATIO) < tol,
  "A06 post shows 0.85 (difference rounded)"       = sprintf("%.2f", DIFF) == "0.85",
  "A07 post shows 0.68 (SE rounded)"               = sprintf("%.2f", SE_DIFF) == "0.68",
  "A08 post shows 1.25 (ratio rounded)"            = sprintf("%.2f", RATIO) == "1.25",
  "A09 displayed 0.85 / 0.68 rounds to 1.25"       = sprintf("%.2f", 0.85 / 0.68) == "1.25",
  "A10 post shows +0.36 and -0.48"                 = sprintf("%.2f", GAP_RB) == "0.36" &&
                                                     sprintf("%.2f", GAP_WR) == "-0.48",
  "A11 post shows F = 0.84"                        = sprintf("%.2f", F_INT) == "0.84",
  "A12 reversal inside the sampling-error range"         = abs(DIFF) < BAND_HALF,
  "A13 ratio under 2 standard errors"              = RATIO < 2,
  "A14 x-limits contain band and point"            = X_LIM[1] < -BAND_HALF && X_LIM[2] > BAND_HALF &&
                                                     X_LIM[2] > DIFF
)

# A15: the post file on disk states the same numbers the plot shows (skips if absent)
if (file.exists(POST_PATH)) {
  post <- paste(readLines(POST_PATH, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  must <- c("0.85 points apart", "0.68 points", "1.25", "came to 0.84", "+0.36", "-0.48",
            "two standard errors")
  missing_txt <- must[!vapply(must, grepl, logical(1), x = post, fixed = TRUE)]
  if (length(missing_txt)) stop("A15 post text missing: ", paste(missing_txt, collapse = " | "))
  cat("A15 post text check: PASS\n")
} else {
  cat("A15 post text check: SKIPPED (post not found at ", POST_PATH, ")\n", sep = "")
}
cat("Pre-build assertions A01 to A14: PASS\n")

# ---- BUILD -------------------------------------------------------------------
Y_BAND <- c(0.30, 0.70)
Y_PT   <- 0.50

p <- ggplot() +
  # Layer 1: usual range of sampling error
  annotate("rect", xmin = -BAND_HALF, xmax = BAND_HALF,
           ymin = Y_BAND[1], ymax = Y_BAND[2], fill = GREY, alpha = 0.35) +
  # Layer 2: zero, no difference
  geom_vline(xintercept = 0, linetype = "dashed", colour = "gray45", linewidth = 0.6) +
  # Layer 3: the observed reversal
  geom_point(aes(x = DIFF, y = Y_PT), colour = NAVY, size = 6) +
  # Layers 4 to 6: labels above the geometry
  annotate("text", x = -BAND_HALF, y = Y_BAND[2] + 0.07, hjust = 0, vjust = 0,
           label = paste0("Usual range of sampling error: within 2 standard errors (\u00b1",
                          sprintf("%.2f", BAND_HALF), " points)"),
           colour = "gray35", size = 3.8) +
  annotate("text", x = DIFF, y = Y_PT + 0.08, hjust = 0.5, vjust = 0,
           label = paste0(sprintf("%.2f", DIFF), " points\n(",
                          sprintf("%.2f", RATIO), " standard errors)"),
           colour = NAVY, fontface = "bold", size = 4.4, lineheight = 0.95) +
  annotate("text", x = 0, y = Y_BAND[1] - 0.07, hjust = 0.5, vjust = 1,
           label = "No difference", colour = "gray45", size = 3.6) +
  scale_x_continuous(breaks = seq(-1.5, 1.5, by = 0.5),
                     labels = function(x) sprintf("%+.1f", x)) +
  coord_cartesian(xlim = X_LIM, ylim = c(0, 1), expand = FALSE) +
  labs(title = TITLE, subtitle = SUBTITLE, caption = CAPTION,
       x = "RB experience gap minus WR experience gap (fantasy points)", y = NULL) +
  theme_minimal(base_size = BASE_SIZE) +
  theme(
    plot.title         = element_text(face = "bold", colour = INK, size = rel(1.2)),
    plot.subtitle      = element_text(colour = "gray35", size = rel(0.9)),
    plot.caption       = element_text(colour = "gray45", size = rel(0.7), hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    axis.text.y        = element_blank(),
    axis.title.x       = element_text(colour = "gray30", margin = margin(t = 8)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(14, 18, 10, 14)
  )

# ---- RENDER CHECKS -----------------------------------------------------------
b <- ggplot2::ggplot_build(p)
stopifnot(
  "R01 expected 6 layers"                   = length(b$data) == 6L,
  "R02 point drawn at the verified difference" = abs(b$data[[3]]$x - DIFF) < tol,
  "R03 band spans -2 SE to +2 SE"           = abs(b$data[[1]]$xmin + BAND_HALF) < tol &&
                                              abs(b$data[[1]]$xmax - BAND_HALF) < tol,
  "R04 zero line at 0"                      = b$data[[2]]$xintercept == 0,
  "R05 point is the largest mark (size 6)"  = b$data[[3]]$size == 6
)

# Text-fit checks: title, subtitle, caption must fit inside the plot width
fits <- function(txt, size_pt, face = "plain") {
  w <- grid::convertWidth(grid::grobWidth(grid::textGrob(
         txt, gp = grid::gpar(fontsize = size_pt, fontface = face))), "in", valueOnly = TRUE)
  w <= (PLOT_W_IN - 0.45)
}
stopifnot(
  "R06 title overflows plot width"    = fits(TITLE, BASE_SIZE * 1.2, "bold"),
  "R07 subtitle overflows plot width" = fits(SUBTITLE, BASE_SIZE * 0.9),
  "R08 caption overflows plot width"  = fits(CAPTION, BASE_SIZE * 0.7)
)
cat("Render checks R01 to R08: PASS\n")

# ---- SAVE --------------------------------------------------------------------
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
png_path <- file.path(OUTPUT_DIR, paste0(FILE_STEM, ".png"))
ggsave(png_path, plot = p, width = PLOT_W_IN, height = PLOT_H_IN, dpi = PLOT_DPI, bg = "white")
stopifnot("R09 PNG not written" = file.exists(png_path) && file.size(png_path) > 0)

if (requireNamespace("svglite", quietly = TRUE)) {
  svg_path <- file.path(OUTPUT_DIR, paste0(FILE_STEM, ".svg"))
  ggsave(svg_path, plot = p, width = PLOT_W_IN, height = PLOT_H_IN, bg = "white")
  cat("Saved SVG:", svg_path, "\n")
} else {
  cat("svglite not installed; SVG skipped.\n")
}

# ---- CONSOLE SUMMARY ---------------------------------------------------------
cat("\n==== Post", POST_NUM, "visual ====\n")
cat("Saved PNG:", png_path, "\n")
cat(sprintf("Reversal %.2f points = %.2f SE; sampling-error range +/- %.2f points; inside: %s\n",
            DIFF, RATIO, BAND_HALF, abs(DIFF) < BAND_HALF))
cat("Now apply the 5-second check by eye: the navy point inside the grey band must be the first thing you see.\n")
