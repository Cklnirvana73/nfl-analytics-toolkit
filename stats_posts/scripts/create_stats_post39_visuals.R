# ==============================================================================
# Statistics Series: Post 39 visual (v2, r line on standard-deviation units)
# Pearson correlation (Concept 39, tag [NFL], anchor R/18 saved panel)
#
# Pictures the post's defining sentence: "r measures how tightly the dots line
# up along a straight line ... In practice, every gap gets counted in standard
# deviations instead of points."
#
# Two panels, quarterbacks and wide receivers. Every player-game is one dot,
# with both gaps counted in standard deviations, so both clouds share the same
# units and the same spread. Each panel carries:
#   navy #0072B2 solid line: the best-fit line, whose slope in these units
#                            equals Pearson r (asserted in code below)
#   grey dashed line:        r of 1, every dot on the line (reference)
# Receivers' line is steeper (0.465 vs 0.377): the stronger link.
# Dots are neutral grey so the lines carry the finding.
#
# Every label uses the post's numbers. The script reads the saved R/18 panel
# and STOPS before drawing unless the panel reproduces every number the post
# prints (reproduction gate). Nothing is re-simulated.
#
# Data: data/season2_cache/s2_week4_ab_panel.rds (R/18; reproduced by
# pull_stats_post39_pearson_data.R, run 2026-10-22, gates G1 to G4 passed).
#
# HOW TO RUN: source("stats_posts/scripts/create_stats_post39_visuals.R") from the project root.
# Writes stats_posts/output/plots/stats_post39_pearson_r_line.png (1200 x 800)
# and .svg if svglite is installed.
# ==============================================================================

# ---- CONFIGURATION -----------------------------------------------------------
TOOLKIT_ROOT <- here::here()
if (!dir.exists(TOOLKIT_ROOT)) stop("TOOLKIT_ROOT not found: ", TOOLKIT_ROOT)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

PANEL_PATH <- file.path(TOOLKIT_ROOT, "data", "season2_cache", "s2_week4_ab_panel.rds")
OUT_DIR    <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "plots")
FILE_STEM  <- "stats_post39_pearson_r_line"

WIDTH_IN  <- 8
HEIGHT_IN <- 16 / 3          # 8 x 5.333 in at 150 DPI = 1200 x 800 px
DPI       <- 150

COL_LINE <- "#0072B2"        # navy: the actual r line
COL_REF  <- "grey40"         # dashed r of 1 reference
COL_DOT  <- "grey55"
XLIM     <- c(-3, 4)         # view window, standard deviations (crop share reported)
YLIM     <- c(-2.5, 4.5)

POST <- list(
  n_qb = 6566L, n_wr = 23021L,
  cov_qb = 26.25, cov_wr = 18.51,
  r_qb = 0.377,  r_wr = 0.465,
  sdy_qb = 10.693, sdy_wr = 7.924,
  sdx_qb = 6.511,  sdx_wr = 5.025
)

if (!file.exists(PANEL_PATH)) {
  stop("Required artifact not found. Run R/18 first.\nMissing: ", PANEL_PATH)
}

# ---- LOAD AND STRUCTURE CHECKS -----------------------------------------------
panel <- readRDS(PANEL_PATH)
need  <- c("position", "fantasy_pts_std", "fantasy_pts_actual")
miss  <- setdiff(need, names(panel))
if (length(miss) > 0) stop("Panel missing: ", paste(miss, collapse = ", "))

d <- panel %>%
  filter(position %in% c("QB", "WR")) %>%
  transmute(position, x = fantasy_pts_std, y = fantasy_pts_actual)
stopifnot(
  "QB + WR rows must be 29,587" = nrow(d) == POST$n_qb + POST$n_wr,
  "No NA allowed in plotted columns" = !anyNA(d$x) && !anyNA(d$y)
)

# ---- PRE-BUILD ASSERTIONS: REPRODUCTION GATE ---------------------------------
st <- d %>%
  group_by(position) %>%
  summarise(n = n(), sx = sd(x), sy = sd(y), cv = cov(x, y), r = cor(x, y),
            .groups = "drop")
g    <- function(pos, col) st[[col]][st$position == pos]
same <- function(value, digits, target) abs(round(value, digits) - target) < 1e-9

gate <- c(
  n_qb   = g("QB", "n") == POST$n_qb,
  n_wr   = g("WR", "n") == POST$n_wr,
  cov_qb = same(g("QB", "cv"), 2, POST$cov_qb),
  cov_wr = same(g("WR", "cv"), 2, POST$cov_wr),
  r_qb   = same(g("QB", "r"), 3, POST$r_qb),
  r_wr   = same(g("WR", "r"), 3, POST$r_wr),
  sdy_qb = same(g("QB", "sy"), 3, POST$sdy_qb),
  sdy_wr = same(g("WR", "sy"), 3, POST$sdy_wr),
  sdx_qb = same(g("QB", "sx"), 3, POST$sdx_qb),
  sdx_wr = same(g("WR", "sx"), 3, POST$sdx_wr)
)
print(as.data.frame(st), digits = 6)
if (!all(gate)) {
  stop("Reproduction gate FAILED: ", paste(names(gate)[!gate], collapse = ", "),
       ". Nothing drawn.")
}
cat("Reproduction gate PASS: all", length(gate), "post values reproduced.\n")

stopifnot(
  "Covariance must rank QB above WR" = g("QB", "cv") > g("WR", "cv"),
  "r must rank WR above QB"          = g("WR", "r")  > g("QB", "r")
)

# ---- PLOT DATA: GAPS IN STANDARD DEVIATIONS ----------------------------------
z <- d %>%
  group_by(position) %>%
  mutate(zx = (x - mean(x)) / sd(x), zy = (y - mean(y)) / sd(y)) %>%
  ungroup()

# Chart-only claim, checked in code: in these units the best-fit line passes
# through 0 and its slope equals r.
fit <- z %>%
  group_by(position) %>%
  summarise(
    slope     = unname(coef(lm(zy ~ zx))[2]),
    intercept = unname(coef(lm(zy ~ zx))[1]),
    r         = cor(x, y),
    sd_zx     = sd(zx),
    sd_zy     = sd(zy),
    .groups   = "drop"
  )
print(as.data.frame(fit), digits = 6)
stopifnot(
  "Slope equals r"            = all(abs(fit$slope - fit$r) < 1e-10),
  "Line passes through 0"     = all(abs(fit$intercept) < 1e-10),
  "Both spreads equal 1"      = all(abs(fit$sd_zx - 1) < 1e-10 & abs(fit$sd_zy - 1) < 1e-10)
)

POS_LABELS <- c(QB = "Quarterbacks", WR = "Wide receivers")
z   <- z   %>% mutate(pos_lab = factor(POS_LABELS[position], levels = POS_LABELS))
fit <- fit %>% mutate(pos_lab = factor(POS_LABELS[position], levels = POS_LABELS),
                      label   = sprintf("Pearson r %.3f", r))

stopifnot(
  any(fit$label == "Pearson r 0.377"),
  any(fit$label == "Pearson r 0.465")
)

# Share of games inside the view window (reported in the caption, never hidden).
in_view <- with(z, zx >= XLIM[1] & zx <= XLIM[2] & zy >= YLIM[1] & zy <= YLIM[2])
pct_in  <- 100 * mean(in_view)
cat(sprintf("Games inside the view window: %.1f%%\n", pct_in))
stopifnot("View window must keep at least 97% of games" = pct_in >= 97)

# Label positions: upper-left of each panel, the sparse corner of a rising cloud.
lab_r   <- fit %>% mutate(lx = XLIM[1] + 0.2, ly = YLIM[2] - 0.25)
lab_ref <- data.frame(lx = 3.9, ly = 4.1, txt = "r of 1")   # just above the dashed line's top end
ink_corner <- z %>%
  filter(zx <= XLIM[1] + 3.0, zy >= YLIM[2] - 0.9) %>%
  count(position)
cat("Dots under the r-label corner:\n"); print(as.data.frame(ink_corner))

# ---- BUILD -------------------------------------------------------------------
# Alpha differs by position only to equalize ink: receivers have 3.5 times as
# many games, so equal alpha would make their cloud look denser for that reason alone.
p <- ggplot(z, aes(zx, zy)) +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_point(aes(alpha = position), colour = COL_DOT, size = 0.4, stroke = 0,
             show.legend = FALSE) +
  geom_abline(slope = 1, intercept = 0, colour = COL_REF, linetype = "dashed",
              linewidth = 0.6) +
  geom_abline(data = fit, aes(slope = slope, intercept = intercept),
              colour = COL_LINE, linewidth = 1.4) +
  geom_text(data = lab_r, aes(lx, ly, label = label), hjust = 0, vjust = 1,
            colour = COL_LINE, size = 5.2, fontface = "bold") +
  geom_text(data = lab_ref, aes(lx, ly, label = txt), inherit.aes = FALSE,
            hjust = 1, vjust = 0, colour = COL_REF, size = 3.3) +
  scale_alpha_manual(values = c(QB = 0.20, WR = 0.06)) +
  facet_wrap(~ pos_lab, ncol = 2) +
  coord_fixed(ratio = 1, xlim = XLIM, ylim = YLIM, expand = FALSE) +
  labs(
    title    = "Once gap size is divided out, the stronger link belongs to receivers",
    subtitle = paste0("Each dot is one player-game, 2010 to 2025, with both gaps counted in ",
                      "standard deviations instead of points.\n",
                      "In these units the best-fit line's slope equals Pearson r. ",
                      "The dashed line is an r of 1."),
    x        = "Season-to-date average: gap in standard deviations",
    y        = "Score that game: gap in standard deviations",
    caption  = sprintf(paste0("Data: my NFL analytics toolkit. 6,566 quarterback and 23,021 ",
                              "wide receiver games, toolkit fantasy scoring. ",
                              "View shows %.1f%% of games."), pct_in)
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle    = element_text(colour = "grey35", size = 9.5, hjust = 0),
    plot.caption     = element_text(colour = "grey50", size = 7.5, hjust = 0),
    strip.text       = element_text(face = "bold", size = 10, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1.5, "lines"),
    plot.margin      = margin(10, 15, 8, 10),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

stopifnot("Point layer row count" = nrow(layer_data(p, 3)) == nrow(z))

# ---- SAVE --------------------------------------------------------------------
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
png_path <- file.path(OUT_DIR, paste0(FILE_STEM, ".png"))
ggsave(png_path, plot = p, width = WIDTH_IN, height = HEIGHT_IN, dpi = DPI, bg = "white")
stopifnot(file.exists(png_path), file.size(png_path) > 0)

svg_path <- NA_character_
if (requireNamespace("svglite", quietly = TRUE)) {
  svg_path <- file.path(OUT_DIR, paste0(FILE_STEM, ".svg"))
  ggsave(svg_path, plot = p, width = WIDTH_IN, height = HEIGHT_IN, bg = "white")
} else {
  message("svglite not installed; SVG skipped. The PNG is the shareable file.")
}

# ---- CONSOLE SUMMARY ---------------------------------------------------------
cat("\n================ Post 39 visual ================\n")
cat("PNG:", png_path, "\n")
cat("SVG:", ifelse(is.na(svg_path), "skipped", svg_path), "\n")
cat(sprintf("r line slopes: QB %.3f, WR %.3f | view keeps %.1f%% of games\n",
            fit$slope[fit$position == "QB"], fit$slope[fit$position == "WR"], pct_in))
