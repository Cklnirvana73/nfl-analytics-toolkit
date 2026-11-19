################################################################################
# create_stats_post47_visual.R
# Statistics Series Post 47, Concept 46: Regression assumptions (LINE)
#
# Pictures the post sentence: "For the top fifth that was too narrow: 5 of 27
# outside. For the bottom three fifths it was too wide: 1 of 79 outside, where
# about 4 would be expected."
# Purpose (what the text cannot show): the receivers fan out as targets rise
# while the standard 95% band keeps one width.
#
# Reads stats_posts/output/data/stats_post47_receivers.csv (written by stats_post47_pull.R).
# Does not re-run the pull. Every value the post quotes is gated against the
# verified console output before anything is drawn.
# Text fit checked in Python (Liberation Sans, metric-compatible with Arial, at
# 300 DPI): title 880 / ~1130 px, subtitle 1064 / ~1130 px, group labels at most
# 192 px inside regions of about 200 px (fourth fifth) to 530 px (top fifth).
################################################################################

TOOLKIT_ROOT <- here::here()
stopifnot(dir.exists(TOOLKIT_ROOT))
setwd(TOOLKIT_ROOT)
suppressPackageStartupMessages(library(ggplot2))

# ---- CONFIGURATION -----------------------------------------------------------

F_REC     <- file.path("stats_posts", "output", "data", "stats_post47_receivers.csv")
OUT_DIR   <- file.path("stats_posts", "output", "plots")
PREFIX    <- "stats_post47_"
WIDTH_IN  <- 4
HEIGHT_IN <- 8 / 3          # 1200 x 800 px at 300 DPI
DPI       <- 300

COL_ACTUAL  <- "#0072B2"    # navy: actual values and the fitted line
COL_WRONG   <- "#D55E00"    # Okabe-Ito vermillion: the one-width standard band
COL_NEITHER <- "#E69F00"    # gold: receivers outside the band

# Verified console output, stats_post47_pull.R, 2026-11-19
N_WR       <- 133L
SLOPE      <- 1.866685
INTERCEPT  <- -9.388933
S_RESID    <- 21.10717
FIFTH_N    <- c(26L, 27L, 26L, 27L, 27L)
FIFTH_OUT  <- c(0L, 1L, 0L, 1L, 5L)
FIFTH_SD_TXT <- c("14.0", "15.0", "16.4", "21.2", "33.0")
PI_MIN_TXT <- "41.9"
PI_MAX_TXT <- "43.4"

# Post groups: bottom three fifths, fourth fifth, top fifth
SEP_X  <- c(67.5, 101)      # gaps: fifth 3 ends at 66, fifth 4 runs 69 to 100, fifth 5 starts at 102
X_LIM  <- c(15, 190)

TITLE    <- "One band width for every receiver: too narrow at the top"
SUBTITLE <- "2025 WRs, 20+ targets. Standard 95% band (about 42 points each way) assumes equal spread."
CAPTION  <- "Gold triangles: outside the band. Points: 2025 full-PPR season totals. Data: nflfastR."

gate <- function(ok, msg) {
  if (!isTRUE(ok)) stop("GATE FAILED: ", msg, call. = FALSE)
  cat("  [PASS] ", msg, "\n", sep = "")
}

# ---- 1. LOAD AND REPRODUCE ---------------------------------------------------

cat("\n================ 1. LOAD AND REPRODUCE ================\n")
gate(file.exists(F_REC), "stats_post47_receivers.csv exists (run stats_post47_pull.R first)")
rec <- utils::read.csv(F_REC, stringsAsFactors = FALSE)
cat("  Columns:", paste(names(rec), collapse = ", "), "\n")
need <- c("player_name", "targets", "total_points", "fifth", "fitted", "resid",
          "pi_lo", "pi_hi", "outside_pi")
gate(all(need %in% names(rec)), "CSV has every column the chart uses")
gate(nrow(rec) == N_WR, sprintf("%d receivers", N_WR))
gate(!anyNA(rec[, need]), "no missing values in chart columns")
gate(is.logical(rec$outside_pi), "outside_pi read as logical")

m <- stats::lm(total_points ~ targets, data = rec)
b <- unname(coef(m)[2]); a <- unname(coef(m)[1]); s <- stats::sigma(m)
cat(sprintf("  slope %.7f | intercept %.7f | s %.6f\n", b, a, s))
gate(abs(b - SLOPE) < 5e-7, "slope matches console (1.866685)")
gate(abs(a - INTERCEPT) < 5e-7, "intercept matches console (-9.388933)")
gate(abs(s - S_RESID) < 5e-6, "residual SD matches console (21.10717)")

# ---- 2. POST CLAIMS ----------------------------------------------------------

cat("\n================ 2. POST CLAIMS ================\n")
f_n <- as.vector(table(factor(rec$fifth, levels = 1:5)))
gate(identical(f_n, FIFTH_N), "fifth sizes 26, 27, 26, 27, 27")
rng <- function(f) range(rec$targets[rec$fifth == f])
cat(sprintf("  fifth 3 max %d | fifth 4 %d to %d | fifth 5 min %d\n",
            rng(3)[2], rng(4)[1], rng(4)[2], rng(5)[1]))
gate(rng(3)[2] < SEP_X[1] && rng(4)[1] > SEP_X[1], "first separator sits between fifths 3 and 4")
gate(rng(4)[2] < SEP_X[2] && rng(5)[1] > SEP_X[2], "second separator sits between fifths 4 and 5")

half_csv <- rec$pi_hi - rec$fitted
gate(max(abs((rec$fitted - rec$pi_lo) - half_csv)) < 1e-8, "band is symmetric about the line (same-run identity)")
gate(identical(rec$outside_pi, abs(rec$resid) > half_csv), "outside flag matches |miss| > band half-width")
f_out <- as.vector(tapply(rec$outside_pi, factor(rec$fifth, levels = 1:5), sum))
gate(identical(as.integer(f_out), FIFTH_OUT), "outside by fifth 0, 1, 0, 1, 5")
gate(sprintf("%.1f", min(half_csv)) == PI_MIN_TXT && sprintf("%.1f", max(half_csv)) == PI_MAX_TXT,
     "band half-width runs 41.9 to 43.4 ('about 42 points')")
f_sd <- as.vector(tapply(rec$resid, factor(rec$fifth, levels = 1:5), stats::sd))
cat("  SD by fifth:", sprintf("%.2f", f_sd), "\n")
gate(identical(sprintf("%.1f", f_sd), FIFTH_SD_TXT), "SD by fifth reads 14.0, 15.0, 16.4, 21.2, 33.0")

grp <- ifelse(rec$fifth <= 3, "bottom", ifelse(rec$fifth == 4, "fourth", "top"))
g_n   <- as.vector(table(factor(grp, levels = c("bottom", "fourth", "top"))))
g_out <- as.vector(tapply(rec$outside_pi, factor(grp, levels = c("bottom", "fourth", "top")), sum))
g_exp <- round(0.05 * g_n)
cat(sprintf("  bottom %d of %d (exp %d) | fourth %d of %d | top %d of %d (exp %d) | total %d\n",
            g_out[1], g_n[1], g_exp[1], g_out[2], g_n[2], g_out[3], g_n[3], g_exp[3], sum(g_out)))
gate(g_n[1] == 79L && g_out[1] == 1L && g_exp[1] == 4L, "bottom three fifths: 1 of 79, about 4 expected")
gate(g_n[2] == 27L && g_out[2] == 1L, "fourth fifth: 1 of 27")
gate(g_n[3] == 27L && g_out[3] == 5L && g_exp[3] == 1L, "top fifth: 5 of 27, about 1 expected")
gate(sum(g_out) == 7L && round(0.05 * N_WR) == 7L, "7 of 133 outside, about the 7 that 95 in 100 implies")
gate(g_out[3] / g_n[3] > 3 * g_out[1] / g_n[1], "5-second claim: top-fifth outside rate is over 3x the bottom group's")

# Band on a grid, from the same formula the pull used; must reproduce the CSV
x_bar <- mean(rec$targets); sxx <- sum((rec$targets - x_bar)^2)
tq    <- stats::qt(0.975, N_WR - 2L)
half_at <- function(x) tq * s * sqrt(1 + 1 / N_WR + (x - x_bar)^2 / sxx)
gate(max(abs(half_at(rec$targets) - half_csv)) < 1e-8, "grid band formula reproduces the CSV band (same-run identity)")
grid <- data.frame(x = seq(20, 185, by = 0.5))
grid$fit <- a + b * grid$x
grid$lo  <- grid$fit - half_at(grid$x)
grid$hi  <- grid$fit + half_at(grid$x)

# ---- 3. BUILD ----------------------------------------------------------------

cat("\n================ 3. BUILD ================\n")
y_top <- max(c(rec$total_points, grid$hi))
y_bot <- min(c(rec$total_points, grid$lo))
lab <- data.frame(
  x  = c(mean(c(20, SEP_X[1])), mean(SEP_X), mean(c(SEP_X[2], 185))),
  l1 = c(sprintf("%d of %d outside", g_out[1], g_n[1]),
         sprintf("%d of %d", g_out[2], g_n[2]),
         sprintf("%d of %d outside", g_out[3], g_n[3])),
  l2 = c(sprintf("about %d expected", g_exp[1]),
         sprintf("about %d", g_exp[2]),
         sprintf("about %d expected", g_exp[3]))
)
Y_LAB1 <- y_top + 44
Y_LAB2 <- y_top + 20
Y_LIM  <- c(y_bot - 10, Y_LAB1 + 18)
inside  <- rec[!rec$outside_pi, ]
outside <- rec[rec$outside_pi, ]

p <- ggplot() +
  geom_vline(xintercept = SEP_X, linetype = "dotted", colour = "grey55", linewidth = 0.3) +
  geom_ribbon(data = grid, aes(x = x, ymin = lo, ymax = hi), fill = COL_WRONG, alpha = 0.12) +
  geom_line(data = grid, aes(x = x, y = lo), colour = COL_WRONG, linetype = "dashed", linewidth = 0.35) +
  geom_line(data = grid, aes(x = x, y = hi), colour = COL_WRONG, linetype = "dashed", linewidth = 0.35) +
  geom_line(data = grid, aes(x = x, y = fit), colour = COL_ACTUAL, linewidth = 0.5) +
  geom_point(data = inside, aes(x = targets, y = total_points),
             colour = COL_ACTUAL, alpha = 0.55, size = 0.8, shape = 16) +
  geom_point(data = outside, aes(x = targets, y = total_points),
             fill = COL_NEITHER, colour = "black", shape = 24, size = 1.6, stroke = 0.3) +
  annotate("text", x = lab$x, y = Y_LAB1, label = lab$l1, fontface = "bold",
           size = 6 / .pt, colour = "grey15") +
  annotate("text", x = lab$x, y = Y_LAB2, label = lab$l2,
           size = 6 / .pt, colour = "grey30") +
  coord_cartesian(xlim = X_LIM, ylim = Y_LIM, expand = FALSE) +
  labs(title = TITLE, subtitle = SUBTITLE, caption = CAPTION,
       x = "Targets", y = "Fantasy points (season total)") +
  theme_minimal(base_size = 6.5) +
  theme(
    plot.title         = element_text(face = "bold", size = 8, hjust = 0),
    plot.subtitle      = element_text(size = 6, colour = "grey30", hjust = 0),
    plot.caption       = element_text(size = 5, colour = "grey45", hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    panel.grid.minor   = element_blank(),
    axis.title         = element_text(colour = "grey25"),
    plot.margin        = margin(6, 8, 4, 4)
  )

# Post-build checks on what will actually render
ld_in  <- layer_data(p, 6)
ld_out <- layer_data(p, 7)
ld_rib <- layer_data(p, 2)
gate(nrow(ld_in) + nrow(ld_out) == N_WR, "all 133 receivers drawn")
gate(nrow(ld_out) == 7L, "7 receivers drawn as outside")
gate(nrow(ld_rib) == nrow(grid), "band ribbon drawn across the full grid")
gate(all(rec$targets >= X_LIM[1] & rec$targets <= X_LIM[2]), "no receiver clipped on the x axis")
gate(all(rec$total_points >= Y_LIM[1] & rec$total_points <= Y_LIM[2]), "no receiver clipped on the y axis")

# ---- 4. SAVE -----------------------------------------------------------------

cat("\n================ 4. SAVE ================\n")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
f_png <- file.path(OUT_DIR, paste0(PREFIX, "one_band_width.png"))
f_svg <- file.path(OUT_DIR, paste0(PREFIX, "one_band_width.svg"))
ggsave(f_png, plot = p, width = WIDTH_IN, height = HEIGHT_IN, dpi = DPI, bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(f_svg, plot = p, width = WIDTH_IN, height = HEIGHT_IN, device = svglite::svglite, bg = "white")
} else {
  cat("  svglite not installed; SVG skipped. install.packages('svglite') to add it.\n")
}

# ---- CONSOLE SUMMARY ---------------------------------------------------------

cat("\n================ SUMMARY ================\n")
cat("  Saved:", f_png, "\n")
if (file.exists(f_svg)) cat("  Saved:", f_svg, "\n")
cat(sprintf("  %d receivers | line %.2f per target | band about 42 points each way\n", N_WR, b))
cat(sprintf("  Outside the band: bottom three fifths %d of %d, fourth fifth %d of %d, top fifth %d of %d\n",
            g_out[1], g_n[1], g_out[2], g_n[2], g_out[3], g_n[3]))
cat("  Typical miss by fifth:", paste(FIFTH_SD_TXT, collapse = ", "), "points\n")
cat("\nALL GATES PASSED\n")
