# create_stats_post34_visuals.R
# Statistics Series, Post 34: Post-Hoc Tests.
#
# What this shows that the post's text cannot: all three position pairs on one
# fantasy-point axis, each gap set against its own two bars.
#   Navy dot   : gap in average forecast miss for the pair.
#   Grey tick  : single-pair bar (no adjustment).
#   Gold tick  : Tukey's bar (about a fifth higher).
# The picture makes three things visible at once: the bars differ by pair (the
# two quarterback rows sit higher because only 26 players are quarterbacks),
# both quarterback gaps sit far past both bars, and the running back vs receiver
# gap falls short of both.
# 5-second read: two dots far right of their ticks, one dot left of both.
#
# Every number is HARDCODED from verified console output of
# pull_stats_post34_post_hoc_tests_data.R (run 2026-10-01), section 4.
# Nothing is re-simulated. qtukey() and qt() are called only to re-derive the
# critical values as an assertion, and the bars are re-derived from MSW and n.
#
# Size: 8 x 5.333 in at 150 DPI = 1200 x 800 px, the size the project
# instructions specify. (Posts 32 and 33 used 300 DPI; that ruling is still open.)
#
# NOT render-tested in session (no R in the container). Run locally and apply
# the 5-second check to the PNG before calling it done.

library(here)
library(ggplot2)
library(grid)

# ---- CONFIGURATION -----------------------------------------------------------
SEASON     <- 2025L
POST_NUM   <- 34L
OUTPUT_DIR <- here::here("stats_posts", "output", "plots")
POST_PATH  <- here::here("stats_posts", "output", "posts", "Stats_Post34_Post_Hoc_Tests.md")
FILE_STEM  <- "stats_post34_pair_gaps_vs_bars"
PLOT_W_IN  <- 8
PLOT_H_IN  <- 5.333
PLOT_DPI   <- 150            # 8 x 5.333 in at 150 DPI = 1200 x 800 px
BASE_SIZE  <- 13

NAVY <- "#0072B2"            # Okabe-Ito blue: actual values (the gaps)
GOLD <- "#E69F00"            # Okabe-Ito orange/gold: Tukey's bar (neither value is wrong)
GREY <- "#999999"            # Okabe-Ito grey: single-pair bar
INK  <- "gray20"

# Verified console values, section 4
MSW      <- 4.744818
DF_W     <- 199L
K_GROUPS <- 3L
ALPHA    <- 0.05
Q_CRIT   <- 3.339502
T_CRIT   <- 1.971957
N_POS    <- c(QB = 26L, RB = 70L, WR = 106L)

pairs <- data.frame(
  pair       = c("Quarterbacks vs running backs", "Quarterbacks vs receivers",
                 "Running backs vs receivers"),
  a          = c("QB", "QB", "RB"),
  b          = c("RB", "WR", "WR"),
  gap        = c(2.6278894, 3.1156326, 0.4877432),   # absolute diff, section 4
  bar_single = c(0.9865226, 0.9400576, 0.6615482),   # min_diff_unadj, section 4
  bar_tukey  = c(1.1813441, 1.1257031, 0.7921928),   # min_diff_tukey, section 4
  y          = c(3, 2, 1),
  stringsAsFactors = FALSE
)
pairs$label <- sprintf("%.1f apart, Tukey bar %.1f", pairs$gap, pairs$bar_tukey)

X_MAX  <- 3.6
TICK_H <- 0.15               # half-height of a bar tick
LAB_DY <- 0.27               # labels sit this far above their row
LAB_X  <- 0.03               # every row label starts at the left edge, above its row

TITLE    <- paste0("Both quarterback gaps clear Tukey's bar by more than double\n",
                   "Running backs vs receivers clears neither bar")
SUBTITLE <- paste0("Gap in average forecast miss for each position pair, ", SEASON,
                   " data, fantasy points")
CAPTION  <- "202 players with 8+ forecasts. Only 26 are quarterbacks, so their pairs get higher bars."

# ---- PRE-BUILD ASSERTIONS ----------------------------------------------------
tol <- 1e-5
se_u <- sqrt(MSW * (1 / N_POS[pairs$a] + 1 / N_POS[pairs$b]))
stopifnot(
  "A01 Tukey q re-derives from qtukey()"            = abs(qtukey(1 - ALPHA, K_GROUPS, DF_W) - Q_CRIT) < tol,
  "A02 unadjusted t re-derives from qt()"           = abs(qt(1 - ALPHA / 2, DF_W) - T_CRIT) < tol,
  "A03 single-pair bars == t x SE"                  = all(abs(T_CRIT * se_u - pairs$bar_single) < tol),
  "A04 Tukey bars == (q / sqrt 2) x SE"             = all(abs(Q_CRIT / sqrt(2) * se_u - pairs$bar_tukey) < tol),
  "A05 gaps == differences of position means"       = all(abs(pairs$gap - abs(c(5.645086 - 8.272975,
                                                        5.157343 - 8.272975, 5.157343 - 5.645086))) < tol),
  "A06 Tukey bar about a fifth higher, every pair"  = all(abs(pairs$bar_tukey / pairs$bar_single - 1.197483) < tol),
  "A07 both QB gaps more than double Tukey's bar"   = all(pairs$gap[1:2] > 2 * pairs$bar_tukey[1:2]),
  "A08 RB vs WR gap short of both bars"             = pairs$gap[3] < pairs$bar_single[3] && pairs$gap[3] < pairs$bar_tukey[3],
  "A09 QB pairs get the higher bars"                = min(pairs$bar_tukey[1:2]) > pairs$bar_tukey[3],
  "A10 post rounding: gaps 2.6, 3.1, 0.5"           = identical(sprintf("%.1f", pairs$gap), c("2.6", "3.1", "0.5")),
  "A11 post rounding: Tukey bars 1.2, 1.1, 0.8"     = identical(sprintf("%.1f", pairs$bar_tukey), c("1.2", "1.1", "0.8")),
  "A12 post rounding: RB vs WR single bar 0.7"      = sprintf("%.1f", pairs$bar_single[3]) == "0.7",
  "A13 group sizes sum to 202"                      = sum(N_POS) == 202L && DF_W == 202L - K_GROUPS,
  "A14 x-axis contains every mark"                  = X_MAX > max(pairs$gap) + 0.1
)

# A15: the post file on disk states the same numbers the plot shows (skips if absent)
if (file.exists(POST_PATH)) {
  post <- paste(readLines(POST_PATH, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  must <- c("misses 2.6 points apart, against a Tukey bar of 1.2", "3.1 apart, bar 1.1",
            "0.5 apart, bar 0.8", "pre-Tukey bar of 0.7", "pairs with the 26 quarterbacks get higher bars",
            "Both clear by more than double")
  missing_txt <- must[!vapply(must, grepl, logical(1), x = post, fixed = TRUE)]
  if (length(missing_txt)) stop("A15 post text missing: ", paste(missing_txt, collapse = " | "))
  cat("A15 post text check: PASS\n")
} else {
  cat("A15 post text check: SKIPPED (post not found at ", POST_PATH, ")\n", sep = "")
}
cat("Pre-build assertions A01 to A14: PASS\n")

# ---- BUILD -------------------------------------------------------------------
KEY_Y <- 3.82
p <- ggplot(pairs) +
  # Layer 1: faint guide from zero to the gap
  geom_segment(aes(x = 0, xend = gap, y = y, yend = y), colour = "gray85", linewidth = 0.8) +
  # Layer 2: single-pair bar ticks
  geom_segment(aes(x = bar_single, xend = bar_single, y = y - TICK_H, yend = y + TICK_H),
               colour = GREY, linewidth = 1.6) +
  # Layer 3: Tukey bar ticks
  geom_segment(aes(x = bar_tukey, xend = bar_tukey, y = y - TICK_H, yend = y + TICK_H),
               colour = GOLD, linewidth = 1.6) +
  # Layer 4: the gaps (largest mark)
  geom_point(aes(x = gap, y = y), colour = NAVY, size = 5) +
  # Layer 5: value labels above each row, left-aligned so none runs off the panel
  geom_text(aes(x = LAB_X, y = y + LAB_DY, label = label), hjust = 0, vjust = 0,
            colour = NAVY, fontface = "bold", size = 3.9) +
  # Layers 6 to 10: key above the top row
  annotate("segment", x = 0.05, xend = 0.05, y = KEY_Y - 0.09, yend = KEY_Y + 0.09,
           colour = GREY, linewidth = 1.6) +
  annotate("text", x = 0.12, y = KEY_Y, label = "Single-pair bar", hjust = 0,
           colour = "gray30", size = 3.6) +
  annotate("segment", x = 1.15, xend = 1.15, y = KEY_Y - 0.09, yend = KEY_Y + 0.09,
           colour = GOLD, linewidth = 1.6) +
  annotate("text", x = 1.22, y = KEY_Y, label = "Tukey's bar", hjust = 0,
           colour = "gray30", size = 3.6) +
  annotate("point", x = 2.05, y = KEY_Y, colour = NAVY, size = 3.5) +
  annotate("text", x = 2.13, y = KEY_Y, label = "Gap in average miss", hjust = 0,
           colour = "gray30", size = 3.6) +
  scale_x_continuous(breaks = seq(0, 3.5, by = 0.5), labels = function(x) sprintf("%.1f", x)) +
  scale_y_continuous(breaks = pairs$y, labels = pairs$pair) +
  coord_cartesian(xlim = c(0, X_MAX), ylim = c(0.6, 4.05), expand = FALSE) +
  labs(title = TITLE, subtitle = SUBTITLE, caption = CAPTION,
       x = "Gap in average forecast miss (fantasy points)", y = NULL) +
  theme_minimal(base_size = BASE_SIZE) +
  theme(
    plot.title            = element_text(face = "bold", colour = INK, size = rel(1.1),
                                         lineheight = 1.05),
    plot.subtitle         = element_text(colour = "gray35", size = rel(0.85)),
    plot.caption          = element_text(colour = "gray45", size = rel(0.7), hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    axis.text.y           = element_text(colour = INK, face = "bold", size = rel(0.85)),
    axis.title.x          = element_text(colour = "gray30", margin = margin(t = 8)),
    panel.grid.major.y    = element_blank(),
    panel.grid.minor      = element_blank(),
    plot.margin           = margin(14, 18, 10, 14)
  )

# ---- RENDER CHECKS -----------------------------------------------------------
# layer_data() instead of ggplot_build()$data ($ access is deprecated in ggplot2 4.0).
n_layers <- if (inherits(p, "S7_object")) length(p@layers) else length(p[["layers"]])
l_single <- layer_data(p, 2)
l_tukey  <- layer_data(p, 3)
l_dots   <- layer_data(p, 4)
l_labels <- layer_data(p, 5)
stopifnot(
  "R01 expected 11 layers"                     = n_layers == 11L,
  "R02 three gap dots at the verified gaps"    = nrow(l_dots) == 3L && all(abs(sort(l_dots$x) - sort(pairs$gap)) < tol),
  "R03 single-pair ticks at the verified bars" = all(abs(sort(l_single$x) - sort(pairs$bar_single)) < tol),
  "R04 Tukey ticks at the verified bars"       = all(abs(sort(l_tukey$x) - sort(pairs$bar_tukey)) < tol),
  "R05 labels sit above their rows"            = all(abs(sort(l_labels$y) - sort(pairs$y + LAB_DY)) < tol),
  "R06 labels clear the tick tops"             = LAB_DY > TICK_H,
  "R07 key sits above the top row's label"     = KEY_Y - 0.09 > max(pairs$y) + LAB_DY + 0.2,
  "R08 dots are the largest marks"             = all(l_dots$size == 5)
)

# Text-fit checks: every line of title, subtitle and caption must fit the plot width.
line_fits <- function(txt, size_pt, face = "plain") {
  w <- vapply(strsplit(txt, "\n", fixed = TRUE)[[1]], function(s)
         grid::convertWidth(grid::grobWidth(grid::textGrob(
           s, gp = grid::gpar(fontsize = size_pt, fontface = face))), "in", valueOnly = TRUE),
       numeric(1))
  all(w <= (PLOT_W_IN - 0.45))
}
stopifnot(
  "R09 title line overflows plot width"    = line_fits(TITLE, BASE_SIZE * 1.1, "bold"),
  "R10 subtitle overflows plot width"      = line_fits(SUBTITLE, BASE_SIZE * 0.85),
  "R11 caption overflows plot width"       = line_fits(CAPTION, BASE_SIZE * 0.7)
)

# The widest row label must stay inside the panel. Panel width is approximated
# conservatively as plot width minus 3.4 in (margins plus the bold y-axis labels,
# measured at up to 2.7 in with a wide sans font).
panel_w_in <- PLOT_W_IN - 3.4
lab_w_in <- vapply(pairs$label, function(s) grid::convertWidth(grid::grobWidth(grid::textGrob(
  s, gp = grid::gpar(fontsize = 3.9 * .pt, fontface = "bold"))), "in", valueOnly = TRUE), numeric(1))
lab_end <- LAB_X + lab_w_in / panel_w_in * X_MAX
stopifnot("R12 a row label runs past the panel edge" = all(lab_end <= X_MAX))
cat("Render checks R01 to R12: PASS\n")

# ---- SAVE --------------------------------------------------------------------
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
png_path <- file.path(OUTPUT_DIR, paste0(FILE_STEM, ".png"))
ggsave(png_path, plot = p, width = PLOT_W_IN, height = PLOT_H_IN, dpi = PLOT_DPI, bg = "white")
stopifnot("R13 PNG not written" = file.exists(png_path) && file.size(png_path) > 0)

if (requireNamespace("svglite", quietly = TRUE)) {
  svg_path <- file.path(OUTPUT_DIR, paste0(FILE_STEM, ".svg"))
  ggsave(svg_path, plot = p, width = PLOT_W_IN, height = PLOT_H_IN, bg = "white")
  cat("Saved SVG:", svg_path, "\n")
} else {
  cat("svglite not installed; SVG skipped.\n")
}

# ---- CONSOLE SUMMARY ---------------------------------------------------------
cat("\n==== Post", POST_NUM, "visual ====\n")
cat("Saved PNG:", png_path, "(", PLOT_W_IN * PLOT_DPI, "x", round(PLOT_H_IN * PLOT_DPI), "px )\n")
for (i in seq_len(nrow(pairs))) {
  cat(sprintf("%-30s gap %.2f | single bar %.2f | Tukey bar %.2f | clears Tukey: %s\n",
              pairs$pair[i], pairs$gap[i], pairs$bar_single[i], pairs$bar_tukey[i],
              pairs$gap[i] > pairs$bar_tukey[i]))
}
cat("Now apply the 5-second check by eye: two navy dots far right of their ticks,",
    "one navy dot left of both, must be the first thing you see.\n")
