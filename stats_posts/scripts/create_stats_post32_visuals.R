# create_stats_post32_visuals.R
# Statistics Series Post 32: One-Way ANOVA
#
# One panel. Two horizontal 100% bars of the total squared spread in players'
# average forecast misses (2025, QB/RB/WR, 202 players):
#   Top bar    : the real data. Between positions (navy) 17.7%, within (grey) 82.3%.
#                F = 21.43 labeled at the bar's end.
#   Bottom bar : ILLUSTRATIVE arithmetic from the post: every player's number
#                copied a second time. Same split, F about 43.
# What the text cannot show at a glance: the between slice is small and does
# not move, while F doubles.
#
# Every number is HARDCODED from the verified console output of
# pull_stats_post32_one_way_anova_data.R (2026-09-24, mae_std | floor 8).
# The doubled F is derived from those hardcoded values and asserted.
# Nothing is re-simulated.
#
# NOT render-tested in session (no R in the container). Run locally and
# apply the 5-second check to the PNG before calling it done.

library(here)
library(ggplot2)
library(scales)
library(grid)

# ---- CONFIGURATION -----------------------------------------------------------
SEASON      <- 2025L
POST_NUM    <- 32L
OUTPUT_DIR  <- here::here("stats_posts", "output", "plots")
POST_PATH   <- here::here("stats_posts", "output", "posts", "Stats_Post32_One_Way_ANOVA.md")
FILE_STEM   <- "stats_post32_anova_between_within_split"
PLOT_W_IN   <- 8
PLOT_H_IN   <- 5.333          # 3:2, same sizing as Post 31 (1200x800 vs 300 DPI ruling still open)
PLOT_DPI    <- 300
BASE_SIZE   <- 13

NAVY  <- "#0072B2"            # Okabe-Ito blue: the actual between-positions slice
GREY  <- "#999999"            # Okabe-Ito grey: within positions (neither value is wrong)
INK   <- "gray20"

# Verified console values (mae_std | floor 8 (MAIN))
N_PLAYERS <- 202L
K_GROUPS  <- 3L
GRP_N     <- c(QB = 26L, RB = 70L, WR = 106L)
GRP_MEAN  <- c(QB = 8.272975, RB = 5.645086, WR = 5.157343)
GRAND     <- 5.727385
SS_B      <- 203.399412
SS_W      <- 944.218806
SS_T      <- 1147.618219
DF_B      <- 2L
DF_W      <- 199L
MS_B      <- 101.699706
MS_W      <- 4.744818
F_STAT    <- 21.433847
P_VAL     <- 3.71436e-09

# Derived (illustrative copy: every player's number counted twice)
SHARE     <- SS_B / SS_T
DF_W_DBL  <- 2L * N_PLAYERS - K_GROUPS          # 401
F_DBL     <- (2 * SS_B / DF_B) / (2 * SS_W / DF_W_DBL)
SHARE_DBL <- (2 * SS_B) / (2 * SS_T)

# ---- PRE-BUILD ASSERTIONS ----------------------------------------------------
tol <- 1e-4
stopifnot(
  "A01 SS between + within != total"        = abs(SS_B + SS_W - SS_T) < tol,
  "A02 MS between != SS_B / 2"              = abs(SS_B / DF_B - MS_B) < tol,
  "A03 MS within != SS_W / 199"             = abs(SS_W / DF_W - MS_W) < tol,
  "A04 F != MS_B / MS_W"                    = abs(MS_B / MS_W - F_STAT) < 1e-5,
  "A05 df within != players - groups"       = DF_W == N_PLAYERS - K_GROUPS,
  "A06 df between != groups - 1"            = DF_B == K_GROUPS - 1L,
  "A07 group sizes do not sum to 202"       = sum(GRP_N) == N_PLAYERS,
  "A08 grand mean != weighted group means"  = abs(sum(GRP_N * GRP_MEAN) / N_PLAYERS - GRAND) < tol,
  "A09 SS between != sum n*(mean-grand)^2"  = abs(sum(GRP_N * (GRP_MEAN - GRAND)^2) - SS_B) < 1e-3,
  # Post claims at shown precision
  "A10 post: 101.70 / 4.745 must show 21.43" = round(101.70 / 4.745, 2) == 21.43,
  "A11 post: MS values round to 101.70, 4.745" = round(MS_B, 2) == 101.70 && round(MS_W, 3) == 4.745,
  "A12 post: share 17.7, within 82.3"       = round(100 * SHARE, 1) == 17.7 && round(100 * (1 - SHARE), 1) == 82.3,
  "A13 post: '21 times the size'"           = floor(F_STAT) == 21,
  "A14 post: under 4 times in a billion"    = P_VAL < 4e-9,
  "A15 post: copied F is 'about 43'"        = round(F_DBL) == 43,
  "A16 post: copying leaves share unchanged" = abs(SHARE_DBL - SHARE) < 1e-12,
  "A17 post: hook means 8.27 / 5.65 / 5.16" = all(round(GRP_MEAN, 2) == c(8.27, 5.65, 5.16)),
  "A18 post: overall average 5.73"          = round(GRAND, 2) == 5.73
)

# A19: numbers in the visual appear in the delivered post (skips if absent)
if (file.exists(POST_PATH)) {
  post_txt <- paste(readLines(POST_PATH, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  must_have <- c("F = 101.70 \u00f7 4.745 = 21.43", "17.7 percent", "about 43", "202",
                 "8.27", "5.65", "5.16", "5.73", "1,147.6", "203.4", "944.2", "199")
  missing <- must_have[!vapply(must_have, grepl, logical(1), x = post_txt, fixed = TRUE)]
  if (length(missing) > 0) stop("A19 post text missing: ", paste(missing, collapse = " | "))
  cat("A19 PASS: all", length(must_have), "visual numbers found in the post.\n")
} else {
  cat("A19 SKIPPED: post not found at", POST_PATH, "\n")
}
cat("Pre-build assertions A01 to A18 PASS.\n")

# ---- PLOT DATA ---------------------------------------------------------------
Y_ACT <- 2; Y_ILL <- 1; HALF <- 0.22
bars <- data.frame(
  row   = c("actual", "actual", "illus", "illus"),
  piece = c("between", "within", "between", "within"),
  xmin  = c(0, SHARE, 0, SHARE_DBL),
  xmax  = c(SHARE, 1, SHARE_DBL, 1),
  y     = c(Y_ACT, Y_ACT, Y_ILL, Y_ILL),
  stringsAsFactors = FALSE
)
bars$ymin  <- bars$y - HALF
bars$ymax  <- bars$y + HALF
bars$fill  <- ifelse(bars$piece == "between", NAVY, GREY)
bars$alpha <- ifelse(bars$row == "actual", 1, 0.45)
bars$lty   <- ifelse(bars$row == "actual", "solid", "dashed")

pct <- function(x) paste0(format(round(100 * x, 1), nsmall = 1), "%")
LAB_F_ACT <- paste0("F = ", format(round(F_STAT, 2), nsmall = 2))
LAB_F_ILL <- paste0("F about ", round(F_DBL))

TITLE    <- "Position is 17.7% of the spread, and F is still 21.43"
SUBTITLE <- paste0(
  "Average forecast miss per player, ", SEASON, " NFL season, QB, RB and WR (", N_PLAYERS, " players).\n",
  "Copying every player a second time leaves the split unchanged and roughly doubles F."
)
CAPTION  <- paste0(
  "F = 101.70 \u00f7 4.745 = 21.43     plain text: F = 101.70 / 4.745 = 21.43\n",
  "Bottom bar is illustrative arithmetic, not new data. Data: nflfastR via my NFL Analytics Toolkit, toolkit scoring rules."
)

SZ_SHARE <- 8      # mm, the finding: biggest text in the panel
SZ_DESC  <- 4.2
SZ_F_ACT <- 7
SZ_SMALL <- 4.6

# ---- BUILD -------------------------------------------------------------------
p <- ggplot(bars) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = fill, alpha = alpha, linetype = lty),
            colour = "white", linewidth = 0.6) +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_linetype_identity() +
  # Top bar labels, above the geometry
  annotate("text", x = 0, y = Y_ACT + HALF + 0.30, label = pct(SHARE),
           hjust = 0, size = SZ_SHARE, fontface = "bold", colour = NAVY) +
  annotate("text", x = 0, y = Y_ACT + HALF + 0.10, label = "between positions",
           hjust = 0, size = SZ_DESC, colour = NAVY) +
  annotate("text", x = SHARE + (1 - SHARE) / 2, y = Y_ACT + HALF + 0.30, label = pct(1 - SHARE),
           hjust = 0.5, size = SZ_SHARE, fontface = "bold", colour = "gray40") +
  annotate("text", x = SHARE + (1 - SHARE) / 2, y = Y_ACT + HALF + 0.10,
           label = "within positions", hjust = 0.5, size = SZ_DESC, colour = "gray40") +
  # Bottom bar labels, above the geometry
  annotate("text", x = 0, y = Y_ILL + HALF + 0.10, label = pct(SHARE_DBL),
           hjust = 0, size = SZ_SMALL, colour = NAVY) +
  annotate("text", x = SHARE_DBL + (1 - SHARE_DBL) / 2, y = Y_ILL + HALF + 0.10,
           label = pct(1 - SHARE_DBL), hjust = 0.5, size = SZ_SMALL, colour = "gray40") +
  # F at each bar's end
  annotate("text", x = 1.03, y = Y_ACT, label = LAB_F_ACT, hjust = 0,
           size = SZ_F_ACT, fontface = "bold", colour = NAVY) +
  annotate("text", x = 1.03, y = Y_ILL, label = LAB_F_ILL, hjust = 0,
           size = SZ_SMALL + 0.6, colour = "gray30") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = c(Y_ILL, Y_ACT),
                     labels = c("Illustrative:\nevery player\ncounted twice",
                                paste0(SEASON, " data\n", N_PLAYERS, " players"))) +
  coord_cartesian(xlim = c(0, 1.30), ylim = c(0.62, 2.85), expand = FALSE, clip = "off") +
  labs(title = TITLE, subtitle = SUBTITLE, caption = CAPTION,
       x = "Share of the total squared spread in players' average misses", y = NULL) +
  theme_minimal(base_size = BASE_SIZE) +
  theme(
    plot.title         = element_text(face = "bold", size = rel(1.25), colour = INK, hjust = 0),
    plot.subtitle      = element_text(size = rel(0.85), colour = "gray35", hjust = 0,
                                      margin = margin(b = 8)),
    plot.caption       = element_text(size = rel(0.68), colour = "gray45", hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    axis.text.y        = element_text(colour = INK, size = rel(0.85), hjust = 1),
    axis.text.x        = element_text(colour = "gray40"),
    axis.title.x       = element_text(colour = "gray30", size = rel(0.85), margin = margin(t = 6)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(12, 16, 10, 12)
  )

# ---- RENDER CHECKS -----------------------------------------------------------
build_warn <- character(0)
b <- withCallingHandlers(ggplot_build(p),
                         warning = function(w) { build_warn <<- c(build_warn, conditionMessage(w)); invokeRestart("muffleWarning") })
rect_df <- b$data[[1]]
txt_labels <- unlist(lapply(b$data[-1], function(d) as.character(d$label)))

stopifnot(
  "R01 build produced warnings"                 = length(build_warn) == 0,
  "R02 expected 4 rect segments"                = nrow(rect_df) == 4,
  "R03 segments do not end at 100%"             = isTRUE(all.equal(max(rect_df$xmax), 1)),
  "R04 actual between slice is not navy"        = rect_df$fill[rect_df$xmin == 0 & rect_df$ymin > 1.5] == NAVY,
  "R05 bars overlap vertically"                 = (Y_ACT - HALF) > (Y_ILL + HALF + 0.15),
  "R06 F label outside x range"                 = 1.03 < 1.30,
  "R07 F label text wrong"                      = LAB_F_ACT %in% txt_labels && LAB_F_ILL %in% txt_labels,
  "R08 share labels wrong"                      = all(c("17.7%", "82.3%") %in% txt_labels),
  "R09 finding is not the largest text"         = SZ_SHARE * .pt > BASE_SIZE * 1.25 && SZ_F_ACT * .pt > BASE_SIZE * 1.25,
  "R10 top labels exceed panel"                 = (Y_ACT + HALF + 0.30) < 2.85
)

# R11 to R13: text fit (grobWidth), longest line of each block vs plot width
fit_in <- function(txt, pts, face = "plain") {
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  max(vapply(lines, function(l) convertWidth(grobWidth(textGrob(l, gp = gpar(fontsize = pts, fontface = face))),
                                             "inches", valueOnly = TRUE), numeric(1)))
}
AVAIL_IN <- PLOT_W_IN - (12 + 16) / 72.27 - 0.05
w_title <- fit_in(TITLE,    BASE_SIZE * 1.25, "bold")
w_sub   <- fit_in(SUBTITLE, BASE_SIZE * 0.85)
w_cap   <- fit_in(CAPTION,  BASE_SIZE * 0.68)
cat(sprintf("Text widths (in): title %.2f, subtitle %.2f, caption %.2f, available %.2f\n",
            w_title, w_sub, w_cap, AVAIL_IN))
stopifnot(
  "R11 title overflows"    = w_title <= AVAIL_IN,
  "R12 subtitle overflows" = w_sub   <= AVAIL_IN,
  "R13 caption overflows"  = w_cap   <= AVAIL_IN
)
cat("Render checks R01 to R13 PASS.\n")

# ---- EXPORT ------------------------------------------------------------------
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
png_path <- file.path(OUTPUT_DIR, paste0(FILE_STEM, ".png"))
ggsave(png_path, plot = p, width = PLOT_W_IN, height = PLOT_H_IN, dpi = PLOT_DPI, bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  svg_path <- file.path(OUTPUT_DIR, paste0(FILE_STEM, ".svg"))
  ggsave(svg_path, plot = p, width = PLOT_W_IN, height = PLOT_H_IN, bg = "white")
} else {
  svg_path <- NA_character_
  message("svglite not installed: SVG skipped. install.packages('svglite') to produce it.")
}

# ---- CONSOLE SUMMARY ---------------------------------------------------------
cat("\n==================== POST", POST_NUM, "VISUAL SUMMARY ====================\n")
cat("PNG:", png_path, sprintf("(%d x %d px)\n", round(PLOT_W_IN * PLOT_DPI), round(PLOT_H_IN * PLOT_DPI)))
cat("SVG:", ifelse(is.na(svg_path), "skipped", svg_path), "\n")
cat(sprintf("Between share %.1f%% | within %.1f%% | F %.2f | copied F %.2f (share %.1f%%)\n",
            100 * SHARE, 100 * (1 - SHARE), F_STAT, F_DBL, 100 * SHARE_DBL))
cat("5-second check (do by eye on the PNG): the navy 17.7% slice and 'F = 21.43' must be\n",
    "the first things seen; both bars' splits must read as identical.\n")
