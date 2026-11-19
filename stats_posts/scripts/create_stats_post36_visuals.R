# create_stats_post36_visuals.R
# Statistics Series, Post 36 (Concept 36, chi-square test of independence). Visual.
#
# One panel. For each position, the actual count of tested games that landed
# below the season-to-date forecast (navy dot) next to its no-link count, the
# count expected if position made no difference (gold ring). The point the text
# cannot show at a glance: all three dots sit almost on top of their rings.
#
# Every number is hardcoded from the verified console output of
# pull_stats_post36_chi_square_independence_data.R (run 2026-10-12, section 7),
# never re-simulated.
#
# NOT render-tested in session: the build container has no R. Text widths were
# measured in Python against Liberation Sans at this size. Confirm the PNG
# renders with all assertions passing, then apply the 5-second check by eye.
#
# Pure ASCII file.

suppressPackageStartupMessages({
  library(ggplot2)
  library(here)
})

# ---- CONFIGURATION -----------------------------------------------------------
OUT_DIR   <- here::here("stats_posts", "output", "plots")
PREFIX    <- "stats_post36_"
FILE_STEM <- "position_vs_nolink_counts"
WIDTH_IN  <- 8
HEIGHT_IN <- 5.333
DPI       <- 150                 # 1200 x 800; DPI ruling still open (150 since Post 34 v2)
X_MAX     <- 70
NAVY      <- "#0072B2"           # actual values
GOLD      <- "#E69F00"           # no-link reference (not a wrong value)
GREY_SEG  <- "grey70"
POST_PATH <- here::here("Stats_Post36_Chi_Square_Test_of_Independence.md")

# Verified console values (section 7, Post 35 headline draw, seed 20261006)
pos_label <- c("Quarterbacks (26)", "Running backs (70)", "Receivers (101)")
n_players <- c(26L, 70L, 101L)
below     <- c(12L, 38L, 56L)
above     <- c(14L, 32L, 45L)
NOLINK_BELOW_VERIFIED <- c(13.98985, 37.66497, 54.34518)
STAT_VERIFIED <- 0.728242
CRIT_VERIFIED <- 5.991465

# ---- PRE-BUILD ASSERTIONS ----------------------------------------------------
n_tot   <- sum(n_players)
b_tot   <- sum(below)
a_tot   <- sum(above)
nolink_below <- n_players * b_tot / n_tot
nolink_above <- n_players * a_tot / n_tot
obs  <- c(below, above)
expd <- c(nolink_below, nolink_above)
stat <- sum((obs - expd)^2 / expd)
dfree <- (length(n_players) - 1L) * (2L - 1L)
crit  <- qchisq(0.95, dfree)
share_below <- 100 * below / n_players

stopifnot(
  "A1 players total 197"                 = n_tot == 197L,
  "A2 games below total 106"             = b_tot == 106L,
  "A3 games above total 91"              = a_tot == 91L,
  "A4 below + above equals players"      = all(below + above == n_players),
  "A5 no-link below matches console"     = max(abs(nolink_below - NOLINK_BELOW_VERIFIED)) < 1e-4,
  "A6 no-link above sums to 91"          = abs(sum(nolink_above) - 91) < 1e-9,
  "A7 chi-square matches console"        = abs(stat - STAT_VERIFIED) < 1e-5,
  "A8 degrees of freedom is 2"           = dfree == 2L,
  "A9 bar matches console"               = abs(crit - CRIT_VERIFIED) < 1e-5,
  "A10 total is under the bar"           = stat < crit,
  "A11 QB gap is -1.99"                  = round(below[1] - nolink_below[1], 2) == -1.99,
  "A12 shares 46.2 / 54.3 / 55.4"        = all(round(share_below, 1) == c(46.2, 54.3, 55.4)),
  "A13 one QB game is 3.85 points"       = round(100 / n_players[1], 2) == 3.85,
  "A14 every gap is under 2 games"       = all(abs(below - nolink_below) < 2)
)

# A15: post-text check (skips if the post file is not next to the project root)
if (file.exists(POST_PATH)) {
  post_txt <- paste(readLines(POST_PATH, warn = FALSE), collapse = " ")
  needed <- c("0.73", "5.99", "13.99", "37.66", "54.35", "14, 32 and 45", "106", "197")
  missing_in_post <- needed[!vapply(needed, function(s) grepl(s, post_txt, fixed = TRUE), logical(1))]
  stopifnot("A15 post text carries every charted number" = length(missing_in_post) == 0)
  cat("A15 post-text check: passed\n")
} else {
  cat("A15 post-text check: skipped (post file not found at", POST_PATH, ")\n")
}

# ---- PLOT DATA ---------------------------------------------------------------
lvl <- rev(pos_label)            # Quarterbacks drawn at the top
plot_df <- data.frame(
  position = factor(pos_label, levels = lvl),
  actual   = below,
  nolink   = nolink_below,
  stringsAsFactors = FALSE
)
plot_df$row_y <- as.numeric(plot_df$position)
plot_df$label <- sprintf("%d actual vs %.2f no-link", plot_df$actual, plot_df$nolink)
plot_df$lab_x <- (plot_df$actual + plot_df$nolink) / 2
plot_df$lab_y <- plot_df$row_y + 0.32   # labels sit above the geometry
stopifnot("Plot data has 3 rows, no NA" = nrow(plot_df) == 3L && !anyNA(plot_df))

# ---- BUILD -------------------------------------------------------------------
p <- ggplot(plot_df, aes(y = position)) +
  geom_segment(aes(x = actual, xend = nolink, yend = position),
               colour = GREY_SEG, linewidth = 1.2) +
  geom_point(aes(x = nolink), shape = 21, size = 7, stroke = 1.6,
             colour = GOLD, fill = NA) +
  geom_point(aes(x = actual), shape = 16, size = 4.2, colour = NAVY) +
  geom_text(aes(x = lab_x, y = lab_y, label = label),
            size = 3.6, colour = "grey20") +
  scale_x_continuous(breaks = seq(0, 70, by = 10), expand = expansion(mult = 0)) +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.75))) +
  coord_cartesian(xlim = c(0, X_MAX)) +
  labs(
    title    = "Actual counts sat almost on top of the no-link counts",
    subtitle = paste0(
      "Games below the season-to-date forecast, one random 2025 game per player (197 players)\n",
      "Chi-square total 0.73, far under the bar of 5.99 at 2 degrees of freedom"),
    x = "Games below the forecast",
    y = NULL,
    caption = paste0(
      "Navy dot: actual games below the forecast. Gold ring: no-link count (the expected count) ",
      "if position made no difference.\nSource: 2025 NFL data, my season-to-date forecast.")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle      = element_text(size = 11, colour = "grey35", hjust = 0),
    plot.caption       = element_text(size = 8.5, colour = "grey45", hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    axis.text.y        = element_text(size = 11, colour = "grey20"),
    axis.text.x        = element_text(size = 10, colour = "grey35"),
    axis.title.x       = element_text(size = 10, colour = "grey30"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.background    = element_rect(fill = "white", colour = NA),
    plot.margin        = margin(12, 18, 10, 12)
  )

# ---- RENDER CHECKS (layer_data, ggplot2 4.0 safe) ----------------------------
stopifnot("R1 plot object" = inherits(p, "ggplot"))
seg  <- layer_data(p, 1)
ring <- layer_data(p, 2)
dot  <- layer_data(p, 3)
txt  <- layer_data(p, 4)
ord  <- order(plot_df$row_y)
stopifnot(
  "R2 segment layer has 3 rows"          = nrow(seg) == 3L,
  "R3 ring layer has 3 rows"             = nrow(ring) == 3L,
  "R4 dot layer has 3 rows"              = nrow(dot) == 3L,
  "R5 text layer has 3 rows"             = nrow(txt) == 3L,
  "R6 dots sit at the actual counts"     = setequal(round(dot$x, 6), round(below, 6)),
  "R7 rings sit at the no-link counts"   = max(abs(sort(ring$x) - sort(nolink_below))) < 1e-9,
  "R8 dots are navy"                     = all(toupper(dot$colour) == NAVY),
  "R9 rings are gold"                    = all(toupper(ring$colour) == GOLD),
  "R10 labels sit above their rows"      = all(abs(sort(txt$y) - sort(dot$y) - 0.32) < 1e-9),
  "R11 every point inside the x range"   = all(c(dot$x, ring$x) > 0 & c(dot$x, ring$x) < X_MAX),
  "R12 labels carry the charted numbers" = all(grepl("actual vs", txt$label)),
  "R13 quarterbacks drawn at the top"    = dot$y[dot$x == 12] == max(dot$y)
)

# ---- SAVE --------------------------------------------------------------------
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
png_path <- file.path(OUT_DIR, paste0(PREFIX, FILE_STEM, ".png"))
ggsave(png_path, plot = p, width = WIDTH_IN, height = HEIGHT_IN, dpi = DPI, bg = "white")
svg_path <- file.path(OUT_DIR, paste0(PREFIX, FILE_STEM, ".svg"))
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, plot = p, width = WIDTH_IN, height = HEIGHT_IN, bg = "white")
} else {
  svg_path <- "(skipped: svglite not installed)"
}

# ---- CONSOLE SUMMARY ---------------------------------------------------------
cat("\n==================== Post 36 visual ====================\n")
cat("Pre-build assertions A1 to A14 passed; A15 reported above.\n")
cat("Render checks R1 to R13 passed.\n")
cat(sprintf("Chi-square %.6f, df %d, bar %.6f\n", stat, dfree, crit))
cat("Gaps (actual - no-link):",
    paste(sprintf("%+.2f", below - nolink_below), collapse = " / "), "\n")
cat("PNG:", png_path, "\n")
cat("SVG:", svg_path, "\n")
cat("Next: open the PNG and apply the 5-second check.\n")
