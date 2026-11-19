# create_stats_post35_visuals.R
# Statistics Series, Post 35 (Chi-Square Goodness of Fit). One visual.
#
# What the chart shows that the post's text cannot:
#   The post asks the reader to PICTURE how chi-square totals spread when the
#   ranges are correct. This chart draws it. One axis of chi-square totals:
#     Gold curve : totals that correct ranges produce (chi-square, 4 degrees of
#                  freedom), theoretical, from the formula. The tail past the
#                  bar (9.49) is shaded: 1 in 20.
#     Navy strip : my 200 reruns. Thin line lowest to highest, thick bar the
#                  middle 80% (10th to 90th percentile), dot the median.
#     Navy diamond: my first test, 6.12.
#     Grey dashed : the bar, 9.49.
#   5-second read: the gold mass sits left of the bar; the navy strip sits
#   mostly right of it.
#
# Every number is HARDCODED from verified console output of
# pull_stats_post35_chi_square_goodness_of_fit_data.R (v3 run, 2026-10-06):
#   section 6 : observed 45/37/42/27/46, expected 39.4, chi-square 6.121827,
#               df 4, critical 9.487729
#   section 7d: 200 draws, share p < 0.05 = 0.705000, statistic quantiles
#               (0, 0.1, 0.5, 0.9, 1) = 1.959391 / 6.467005 / 12.670051 /
#               21.010152 / 37.593909
# Nothing is re-simulated. dchisq()/qchisq()/pchisq() are formulas, used for the
# theoretical curve and to re-derive the bar as an assertion.
#
# Size: 8 x 5.333 in at 150 DPI = 1200 x 800 px (project instructions). The
# 150 vs 300 DPI ruling is still open.
#
# NOT render-tested in session (no R in the container). Run locally and apply
# the 5-second check to the PNG before calling it done. Text widths were
# measured in Python (Liberation Sans, Arial metrics) before delivery.

library(here)
library(ggplot2)

# ---- CONFIGURATION -----------------------------------------------------------
SEASON     <- 2025L
POST_NUM   <- 35L
OUTPUT_DIR <- here::here("stats_posts", "output", "plots")
POST_PATH  <- here::here("stats_posts", "output", "posts", "Stats_Post35_Chi_Square_Goodness_of_Fit.md")
FILE_STEM  <- "stats_post35_correct_ranges_vs_my_reruns"
PLOT_W_IN  <- 8
PLOT_H_IN  <- 5.333
PLOT_DPI   <- 150            # 8 x 5.333 in at 150 DPI = 1200 x 800 px
BASE_SIZE  <- 13
LABEL_SIZE <- 3.6            # mm, about 10.2 pt

NAVY <- "#0072B2"            # Okabe-Ito blue: actual values (my tests)
GOLD <- "#E69F00"            # Okabe-Ito orange/gold: theoretical reference (neither value wrong)
GREY <- "#999999"            # Okabe-Ito grey: the bar
INK  <- "gray20"

# Verified console values
OBS      <- c(45L, 37L, 42L, 27L, 46L)   # section 6
N_TESTED <- 197L                          # section 6
EXPECTED <- 39.4                          # section 6
FIRST    <- 6.121827                      # section 6, chi-square on the headline draw
DF       <- 4L                            # section 6
BAR      <- 9.487729                      # section 6, critical value at alpha 0.05
ALPHA    <- 0.05
N_DRAWS  <- 200L                          # section 7d
SHARE_OVER <- 0.705                       # section 7d, share of draws with p < 0.05
Q_RERUN  <- c(q0 = 1.959391, q10 = 6.467005, q50 = 12.670051,
              q90 = 21.010152, q100 = 37.593909)   # section 7d

# Layout (data units)
X_MAX    <- 40
Y_TOP    <- 0.315
Y_RERUN  <- 0.255            # navy strip row
Y_FIRST  <- 0.212            # first-test diamond row
Y_LAB_RR <- 0.270            # rerun label, above the strip
Y_LAB_FT <- 0.224            # first-test label, above the diamond
Y_LAB_BAR <- 0.293           # bar label, top
X_GOLD   <- 13               # gold label anchor (left edge)
Y_GOLD1  <- 0.085
Y_GOLD2  <- 0.068

TITLE    <- paste0("My 200 reruns mostly landed above the bar\n",
                   "that correct ranges cross only 1 time in 20")
SUBTITLE <- paste0("Chi-square totals (\u03c7\u00b2) from five slices, 4 degrees of freedom, ",
                   SEASON, " NFL data")
CAPTION  <- paste0("Gold: totals that correct ranges produce, drawn from the chi-square formula ",
                   "(theoretical, not my data).\n",
                   "Navy: my 200 reruns, one random game per player. Thin line: lowest to highest. ",
                   "Thick bar: middle 80%. Dot: median.")
X_TITLE  <- "Chi-square total (\u03c7\u00b2)"
LAB_BAR  <- sprintf("The bar: %.2f", BAR)
LAB_RR   <- "My 200 reruns"
LAB_FT   <- sprintf("My first test: %.2f", FIRST)
LAB_G1   <- "Gold: totals from correct ranges."
LAB_G2   <- "Only 1 in 20 land past the bar."

# ---- PRE-BUILD ASSERTIONS ------------------------------------------------------
stat_from_counts <- sum((OBS - EXPECTED)^2 / EXPECTED)
stopifnot(
  "A1 bar is the 5% chi-square critical value at 4 df" =
    abs(qchisq(1 - ALPHA, DF) - BAR) < 1e-6,
  "A2 a correct range lands above the bar 1 time in 20" =
    abs(pchisq(BAR, DF, lower.tail = FALSE) - ALPHA) < 1e-7,
  "A3 headline total re-derives from the five counts" =
    abs(stat_from_counts - FIRST) < 1e-6,
  "A4 counts total 197 and expected is 197 / 5" =
    sum(OBS) == N_TESTED && abs(N_TESTED / 5 - EXPECTED) < 1e-12,
  "A5 df is slices minus one" = DF == length(OBS) - 1L,
  "A6 rerun quantiles strictly increase" = all(diff(Q_RERUN) > 0),
  "A7 first test stayed under the bar (post claim)" = FIRST < BAR,
  "A8 median rerun is above the bar" = Q_RERUN[["q50"]] > BAR,
  "A9 share over the bar sits between 50% and 90%, matching q10 < bar < q50" =
    Q_RERUN[["q10"]] < BAR && BAR < Q_RERUN[["q50"]] && SHARE_OVER > 0.5 && SHARE_OVER < 0.9,
  "A10 141 of 200 reruns over the bar (post claim)" = round(SHARE_OVER * N_DRAWS) == 141,
  "A11 a correct range would put about 10 of 200 over (post claim)" = N_DRAWS * ALPHA == 10,
  "A12 slice 4 piece is 3.90 (post claim)" =
    abs(round((OBS[4] - EXPECTED)^2 / EXPECTED, 2) - 3.90) < 1e-9,
  "A13 highest rerun fits inside the x axis" = Q_RERUN[["q100"]] < X_MAX,
  "A14 curve peak sits below the first-test row" = dchisq(2, DF) < Y_FIRST - 0.02
)
cat("Pre-build assertions A1 to A14 passed.\n")

# A15: post text check (skipped if the post file is not in stats_posts/output/posts/)
if (file.exists(POST_PATH)) {
  post <- paste(readLines(POST_PATH, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  needles <- c("6.12", "9.49", "141 did", "1 in every 20", "about 10 of the 200",
               "4 degrees of freedom", "45, 37, 42, 27 and 46")
  found <- vapply(needles, function(s) grepl(s, post, fixed = TRUE), logical(1))
  if (!all(found)) stop("A15 post text missing: ", paste(needles[!found], collapse = " | "))
  cat("A15 post text check passed.\n")
} else {
  cat("A15 skipped: post not found at", POST_PATH, "\n")
}

# ---- DATA FOR LAYERS -----------------------------------------------------------
curve_df <- data.frame(x = seq(0, X_MAX, length.out = 801))
curve_df$y <- dchisq(curve_df$x, DF)
tail_df  <- curve_df[curve_df$x >= BAR, ]
stopifnot(nrow(curve_df) > 0, nrow(tail_df) > 0)

# ---- PLOT ----------------------------------------------------------------------
p <- ggplot() +
  # 1-2: theoretical curve for correct ranges, tail past the bar shaded darker
  geom_ribbon(data = curve_df, aes(x = x, ymin = 0, ymax = y), fill = GOLD, alpha = 0.22) +
  geom_ribbon(data = tail_df,  aes(x = x, ymin = 0, ymax = y), fill = GOLD, alpha = 0.60) +
  # 3: curve outline
  geom_line(data = curve_df, aes(x = x, y = y), colour = GOLD, linewidth = 1) +
  # 4: the bar
  geom_vline(xintercept = BAR, linetype = "dashed", colour = GREY, linewidth = 0.7) +
  # 5-7: my 200 reruns (full range, middle 80%, median)
  annotate("segment", x = Q_RERUN[["q0"]], xend = Q_RERUN[["q100"]],
           y = Y_RERUN, yend = Y_RERUN, colour = NAVY, linewidth = 0.8) +
  annotate("segment", x = Q_RERUN[["q10"]], xend = Q_RERUN[["q90"]],
           y = Y_RERUN, yend = Y_RERUN, colour = NAVY, linewidth = 4.5) +
  annotate("point", x = Q_RERUN[["q50"]], y = Y_RERUN, shape = 21, size = 4,
           fill = NAVY, colour = "white", stroke = 1.2) +
  # 8: my first test
  annotate("point", x = FIRST, y = Y_FIRST, shape = 18, size = 4.5, colour = NAVY) +
  # 9-13: labels, all above their geometry
  annotate("text", x = BAR - 0.3, y = Y_LAB_BAR, label = LAB_BAR, hjust = 1, vjust = 0,
           size = LABEL_SIZE, colour = INK) +
  annotate("text", x = Q_RERUN[["q50"]], y = Y_LAB_RR, label = LAB_RR, hjust = 0.5, vjust = 0,
           size = LABEL_SIZE, colour = NAVY, fontface = "bold") +
  annotate("text", x = FIRST, y = Y_LAB_FT, label = LAB_FT, hjust = 0.5, vjust = 0,
           size = LABEL_SIZE, colour = NAVY) +
  annotate("text", x = X_GOLD, y = Y_GOLD1, label = LAB_G1, hjust = 0, vjust = 0,
           size = LABEL_SIZE, colour = INK) +
  annotate("text", x = X_GOLD, y = Y_GOLD2, label = LAB_G2, hjust = 0, vjust = 0,
           size = LABEL_SIZE, colour = INK) +
  scale_x_continuous(breaks = seq(0, X_MAX, by = 5)) +
  scale_y_continuous(breaks = NULL) +
  coord_cartesian(xlim = c(0, X_MAX), ylim = c(0, Y_TOP), expand = FALSE, clip = "off") +
  labs(title = TITLE, subtitle = SUBTITLE, caption = CAPTION, x = X_TITLE, y = NULL) +
  theme_minimal(base_size = BASE_SIZE) +
  theme(
    plot.title          = element_text(face = "bold", size = 15, colour = INK, hjust = 0),
    plot.subtitle       = element_text(size = 11.5, colour = "gray35", hjust = 0,
                                       margin = margin(b = 10)),
    plot.caption        = element_text(size = 10, colour = "gray40", hjust = 0,
                                       margin = margin(t = 10)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.minor    = element_blank(),
    panel.grid.major.y  = element_blank(),
    axis.title.x        = element_text(colour = "gray30", margin = margin(t = 6)),
    axis.text.x         = element_text(colour = "gray40"),
    plot.margin         = margin(12, 12, 12, 12),
    plot.background     = element_rect(fill = "white", colour = NA)
  )

# ---- RENDER CHECKS (layer_data only; no $ access, for ggplot2 4.0) -------------
ld <- lapply(seq_len(13), function(i) layer_data(p, i))
stopifnot(
  "R1 thirteen layers built" = length(ld) == 13,
  "R2 full curve ribbon has rows" = nrow(ld[[1]]) == nrow(curve_df),
  "R3 tail ribbon starts at the bar" = min(ld[[2]][["x"]]) >= BAR,
  "R4 tail area is 1 in 20 of the curve (trapezoid check)" =
    abs(sum(diff(tail_df$x) * (head(tail_df$y, -1) + tail(tail_df$y, -1)) / 2) +
          pchisq(X_MAX, DF, lower.tail = FALSE) - ALPHA) < 0.002,
  "R5 bar line at 9.49" = abs(ld[[4]][["xintercept"]] - BAR) < 1e-9,
  "R6 rerun full-range segment matches quantiles" =
    abs(ld[[5]][["x"]] - Q_RERUN[["q0"]]) < 1e-9 && abs(ld[[5]][["xend"]] - Q_RERUN[["q100"]]) < 1e-9,
  "R7 rerun middle-80% segment matches quantiles" =
    abs(ld[[6]][["x"]] - Q_RERUN[["q10"]]) < 1e-9 && abs(ld[[6]][["xend"]] - Q_RERUN[["q90"]]) < 1e-9,
  "R8 median dot at 12.67" = abs(ld[[7]][["x"]] - Q_RERUN[["q50"]]) < 1e-9,
  "R9 first-test diamond at 6.12 and left of the bar" =
    abs(ld[[8]][["x"]] - FIRST) < 1e-9 && ld[[8]][["x"]] < BAR,
  "R10 labels carry the intended text" =
    identical(ld[[9]][["label"]], LAB_BAR) && identical(ld[[10]][["label"]], LAB_RR) &&
    identical(ld[[11]][["label"]], LAB_FT),
  "R11 every label sits above its geometry" =
    ld[[10]][["y"]] > Y_RERUN && ld[[11]][["y"]] > Y_FIRST && ld[[9]][["y"]] > Y_LAB_RR,
  "R12 gold labels sit above the curve where they start" =
    Y_GOLD2 > dchisq(X_GOLD, DF) + 0.03,
  "R13 rows do not collide: first-test label below the strip" =
    Y_LAB_FT + 0.012 < Y_RERUN - 0.006
)
cat("Render checks R1 to R13 passed.\n")

# ---- SAVE ----------------------------------------------------------------------
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
png_path <- file.path(OUTPUT_DIR, paste0(FILE_STEM, ".png"))
svg_path <- file.path(OUTPUT_DIR, paste0(FILE_STEM, ".svg"))
ggsave(png_path, plot = p, width = PLOT_W_IN, height = PLOT_H_IN, dpi = PLOT_DPI, bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, plot = p, width = PLOT_W_IN, height = PLOT_H_IN, bg = "white")
} else {
  cat("SVG skipped: svglite is not installed (known open item).\n")
}

# ---- CONSOLE SUMMARY -------------------------------------------------------------
cat("\nPost", POST_NUM, "visual written:\n  ", png_path, "\n")
if (file.exists(svg_path)) cat("  ", svg_path, "\n")
cat(sprintf("  Bar %.2f | first test %.2f | reruns median %.2f, middle 80%% %.2f to %.2f\n",
            BAR, FIRST, Q_RERUN[["q50"]], Q_RERUN[["q10"]], Q_RERUN[["q90"]]))
cat("  5-second check: gold mass left of the dashed bar, navy strip mostly right of it.\n")
