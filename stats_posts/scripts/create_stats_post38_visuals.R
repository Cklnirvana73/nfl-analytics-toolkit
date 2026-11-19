# ==============================================================================
# Statistics Series: Post 38 visual (v3, four corners)
# Covariance (Concept 38, [GAP] demo built this session)
#
# Pictures the post's definition paragraph: for each week, take how far each
# player landed above (+) or below (-) his own average and multiply the two.
# Both above or both below gives a positive product; one of each, negative.
#
# One scatter of all 419 teammate pair-weeks (31 QB1 + WR1 pairs, 2025
# regular season). Across: the QB's distance from his own average. Up: the
# WR's. A dashed cross at 0 splits the four corners.
#   navy #0072B2 = same-side weeks (positive products), the finding
#   gold #E69F00 = opposite-side weeks (negative products), not wrong, the
#                  other side of the ledger
#
# Data: stats_posts/output/data/stats_post38_pairweek_deviations.csv, written by
# pull_stats_post38_corner_data.R (run 2026-10-19, reproduction gate passed).
# The script stops unless the CSV reproduces the verified console values.
# Nothing is re-simulated. No equation appears, so the notation rule does not apply.
#
# HOW TO RUN: source("stats_posts/scripts/create_stats_post38_visuals.R")
# Writes stats_posts/output/plots/stats_post38_covariance_four_corners.png (1200 x 800)
# and .svg if svglite is installed.
# ==============================================================================

# ---- CONFIGURATION ------------------------------------------------------------
TOOLKIT_ROOT <- here::here()
if (!dir.exists(TOOLKIT_ROOT)) stop("TOOLKIT_ROOT not found: ", TOOLKIT_ROOT)

library(ggplot2)

DATA_CSV   <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "data", "stats_post38_pairweek_deviations.csv")
OUTPUT_DIR <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "plots")
FILE_STEM  <- "stats_post38_covariance_four_corners"
POST_PATH  <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "posts", "Stats_Post38_Covariance.md")
WIDTH_IN   <- 8
DPI        <- 150
HEIGHT_IN  <- 800 / DPI          # 1200 x 800 px

NAVY      <- "#0072B2"
GOLD      <- "#E69F00"
GOLD_TEXT <- "#9A6A00"           # darker gold for readable label text
TEXT_DARK <- "#333333"

# Verified console values (corner pull, sections 3 and 4)
N_PAIRS       <- 31L
N_WEEKS       <- 419L
COV_COMBINED  <- 21.6992
N_BOTH_ABOVE  <- 117L
N_BOTH_BELOW  <- 141L
N_QB_UP_WR_DN <- 92L
N_QB_DN_WR_UP <- 69L
POS_SUM       <- 12755.1148
NEG_SUM       <- -4335.8425

X_LIM <- c(-20, 27)
Y_LIM <- c(-27, 38)            # widened so corner labels sit clear of every dot

# ---- LOAD THE SAVED ARTIFACT -------------------------------------------------
if (!file.exists(DATA_CSV)) {
  stop("Required artifact not found. Run pull_stats_post38_corner_data.R first.\n",
       "Missing: ", DATA_CSV)
}
d <- read.csv(DATA_CSV, stringsAsFactors = FALSE)
need <- c("team", "week", "qb_dev", "wr_dev", "product", "corner")
miss <- setdiff(need, names(d))
if (length(miss) > 0) stop("CSV missing columns: ", paste(miss, collapse = ", "))

# ---- PRE-BUILD ASSERTIONS (A-series) -----------------------------------------
same_side <- d$corner %in% c("both_above", "both_below")
stopifnot(
  "A1 419 pair-weeks" = nrow(d) == N_WEEKS,
  "A2 31 pairs" = length(unique(d$team)) == N_PAIRS,
  "A3 no missing distances" = !anyNA(d$qb_dev) && !anyNA(d$wr_dev),
  "A4 CSV reproduces the combined covariance 21.70" =
    abs(sum(d$product) / (N_WEEKS - N_PAIRS) - COV_COMBINED) < 1e-4,
  "A5 both-above count matches the console" = sum(d$corner == "both_above") == N_BOTH_ABOVE,
  "A6 both-below count matches the console" = sum(d$corner == "both_below") == N_BOTH_BELOW,
  "A7 QB-above WR-below count matches" = sum(d$corner == "qb_above_wr_below") == N_QB_UP_WR_DN,
  "A8 QB-below WR-above count matches" = sum(d$corner == "qb_below_wr_above") == N_QB_DN_WR_UP,
  "A9 corners cover every week" =
    N_BOTH_ABOVE + N_BOTH_BELOW + N_QB_UP_WR_DN + N_QB_DN_WR_UP == N_WEEKS,
  "A10 positive and negative products match the console" =
    abs(sum(d$product[d$product > 0]) - POS_SUM) < 1e-3 &&
    abs(sum(d$product[d$product < 0]) - NEG_SUM) < 1e-3,
  "A11 same-side products outweigh opposite-side ones, as the post says" =
    POS_SUM > abs(NEG_SUM),
  "A12 the corner label matches the product sign" =
    all(d$product[same_side] > 0) && all(d$product[!same_side] < 0),
  "A13b no dot lies under a top or bottom corner label" =
    max(d$wr_dev) < Y_LIM[2] - 6.9 && min(d$wr_dev) > Y_LIM[1] + 6.9,
  "A13 every dot falls inside the view" =
    all(d$qb_dev > X_LIM[1] & d$qb_dev < X_LIM[2]) &&
    all(d$wr_dev > Y_LIM[1] & d$wr_dev < Y_LIM[2])
)

# A14: numbers the chart shares with the post appear in the locked post
# (skipped with a warning if the post is not at POST_PATH). The corner counts
# are chart-only labels from the corner pull and are not in the post.
POST_NUMBERS <- c("31", "2025")
if (file.exists(POST_PATH)) {
  post_text <- paste(readLines(POST_PATH, warn = FALSE, encoding = "UTF-8"),
                     collapse = "\n")
  missing_nums <- POST_NUMBERS[!vapply(POST_NUMBERS, grepl, logical(1),
                                       x = post_text, fixed = TRUE)]
  if (length(missing_nums) > 0) {
    stop("A14 numbers in chart but not in post: ", paste(missing_nums, collapse = ", "))
  }
  cat("A14 passed.\n")
} else {
  warning("A14 skipped: post not found at ", POST_PATH)
}

# ---- PLOT DATA ---------------------------------------------------------------
d$side <- ifelse(same_side, "same", "opposite")
# draw opposite-side dots first so the navy finding sits on top
d <- d[order(d$side == "same"), ]
d$dot_col <- ifelse(d$side == "same", NAVY, GOLD)

corner_labs <- data.frame(
  x     = c(X_LIM[2] - 0.6, X_LIM[1] + 0.6, X_LIM[2] - 0.6, X_LIM[1] + 0.6),
  y     = c(Y_LIM[2] - 0.8, Y_LIM[1] + 0.8, Y_LIM[1] + 0.8, Y_LIM[2] - 0.8),
  hjust = c(1, 0, 1, 0),
  vjust = c(1, 0, 0, 1),
  lab   = c(sprintf("Both above average\n%d weeks", N_BOTH_ABOVE),
            sprintf("Both below average\n%d weeks", N_BOTH_BELOW),
            sprintf("QB above, WR below\n%d weeks", N_QB_UP_WR_DN),
            sprintf("QB below, WR above\n%d weeks", N_QB_DN_WR_UP)),
  col   = c(NAVY, NAVY, GOLD_TEXT, GOLD_TEXT),
  stringsAsFactors = FALSE
)

stopifnot("A15 four corner labels" = nrow(corner_labs) == 4)

# ---- BUILD -------------------------------------------------------------------
p <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray55", linewidth = 0.5) +
  geom_point(data = d, aes(x = qb_dev, y = wr_dev),
             color = d$dot_col, alpha = 0.75, size = 2) +
  geom_label(data = corner_labs,
             aes(x = x, y = y, label = lab),
             hjust = corner_labs$hjust, vjust = corner_labs$vjust,
             color = corner_labs$col, fill = "white", fontface = "bold",
             size = 3.6, lineheight = 0.95,
             label.padding = unit(0.15, "lines")) +
  scale_x_continuous(breaks = seq(-15, 25, by = 5)) +
  scale_y_continuous(breaks = seq(-20, 30, by = 10)) +
  coord_cartesian(xlim = X_LIM, ylim = Y_LIM, expand = FALSE) +
  labs(
    title = "Same-side weeks outnumbered opposite-side weeks, 258 to 161",
    subtitle = paste0("Each dot is one 2025 week for a QB and his own top WR: how far each landed\n",
                      "above (+) or below (-) his own average. 31 pairs, 419 weeks."),
    x = "QB: fantasy points above (+) or below (-) his own average",
    y = "WR: fantasy points above (+)\nor below (-) his own average",
    caption = paste0("Navy: both above or both below (positive product). ",
                     "Gold: one of each (negative product).\n",
                     "Data: nflverse weekly player stats via nflreadr, 2025 regular season.")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0, color = TEXT_DARK),
    plot.subtitle = element_text(size = 10, hjust = 0, color = "gray35",
                                 lineheight = 1.05, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, hjust = 0, color = "gray45", lineheight = 1.05),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.text = element_text(size = 9.5, color = "gray35"),
    axis.title.x = element_text(size = 10, color = "gray30", margin = margin(t = 8)),
    axis.title.y = element_text(size = 10, color = "gray30", margin = margin(r = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray93"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(14, 20, 10, 12)
  )

# ---- POST-RENDER ASSERTIONS (R-series, ggplot2 4.0 safe) ---------------------
ld_pts  <- layer_data(p, 3)
ld_labs <- layer_data(p, 4)
stopifnot(
  "R1 419 dots drawn" = nrow(ld_pts) == N_WEEKS,
  "R2 navy dots equal the same-side weeks" =
    sum(toupper(ld_pts$colour) == NAVY) == N_BOTH_ABOVE + N_BOTH_BELOW,
  "R3 gold dots equal the opposite-side weeks" =
    sum(toupper(ld_pts$colour) == GOLD) == N_QB_UP_WR_DN + N_QB_DN_WR_UP,
  "R4 navy dots are drawn last (on top)" =
    all(toupper(tail(ld_pts$colour, N_BOTH_ABOVE + N_BOTH_BELOW)) == NAVY),
  "R5 four corner labels drawn" = nrow(ld_labs) == 4,
  "R6 labels anchored inside the view" =
    all(ld_labs$x > X_LIM[1] & ld_labs$x < X_LIM[2]) &&
    all(ld_labs$y > Y_LIM[1] & ld_labs$y < Y_LIM[2]),
  "R7 plot inherits from ggplot" = inherits(p, "ggplot")
)

# ---- SAVE --------------------------------------------------------------------
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
png_path <- file.path(OUTPUT_DIR, paste0(FILE_STEM, ".png"))
ggsave(png_path, plot = p, width = WIDTH_IN, height = HEIGHT_IN, dpi = DPI, bg = "white")

svg_path <- NA_character_
if (requireNamespace("svglite", quietly = TRUE)) {
  svg_path <- file.path(OUTPUT_DIR, paste0(FILE_STEM, ".svg"))
  ggsave(svg_path, plot = p, width = WIDTH_IN, height = HEIGHT_IN, bg = "white")
} else {
  message("svglite not installed: SVG skipped. The PNG is the shareable file.")
}

# ---- CONSOLE SUMMARY ---------------------------------------------------------
cat("\n================ Post 38 visual ================\n")
cat("PNG:", png_path, "\n")
cat("SVG:", ifelse(is.na(svg_path), "skipped", svg_path), "\n")
cat("Size:", WIDTH_IN, "x", round(HEIGHT_IN, 3), "in at", DPI, "DPI =",
    WIDTH_IN * DPI, "x", round(HEIGHT_IN * DPI), "px\n")
cat("Same-side weeks:", N_BOTH_ABOVE + N_BOTH_BELOW, "| opposite-side weeks:",
    N_QB_UP_WR_DN + N_QB_DN_WR_UP, "\n")
cat("Positive products:", sprintf("%.2f", POS_SUM), "| negative products:",
    sprintf("%.2f", NEG_SUM), "| combined covariance:", sprintf("%.2f", COV_COMBINED), "\n")
cat("5-second check: open the PNG and confirm the navy corners (top right,",
    "bottom left) are visibly fuller than the gold ones.\n")
