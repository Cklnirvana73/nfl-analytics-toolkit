################################################################################
# create_stats_post46_visual.R  (v4)
# Statistics Series Post 46: Multiple Linear Regression
#
# Purpose (NFL-38): show the offense slice inside the best and worst defenses'
# edges, the slice that "defense measured alone" counts as defense. That split
# is what moves defense from 5.5 (alone) to 4.4 (offense held fixed).
#
# Pre-checks (Post 43 rule), run before this code was written:
#   core claim  : measured alone, defense was credited with its offense's share
#   purpose     : picture that share; the post's text cannot show the split
#   eye-draw    : title + 2-line subtitle carry the 5.5 / 4.4 link at readable
#                 size; direct labels, no legend; only the two outer thirds
#   color       : navy = defense's own part (kept when held fixed); gold =
#                 offense's part (not wrong; credited to defense when alone)
# visualization-patterns.md: insight title, direct labels over legends,
# labels outside geometry, 5-second rule. Text fit checked in Python (NFL-39):
# every string fits 1200 x 800 at 300 DPI; label geometry gated below.
#
# Bars: the two-input equation's split of each third's average point
# differential vs league average:
#   b x (mean_off_t - mean_off) + c x (mean_def_t - mean_def)
# The third's mean miss is printed and gated small, not drawn.
# Data: stats_posts/output/data/stats_post46v2_team_seasons.csv (stats_post46_pull_v2.R).
# Fit gated against that pull's console values. Run from the toolkit root.
################################################################################

suppressPackageStartupMessages(library(ggplot2))

# ---- CONFIGURATION ------------------------------------------------------------
OUTPUT_DIR  <- file.path("stats_posts", "output", "plots")
FILE_PREFIX <- "stats_post46_"
F_TS        <- file.path("stats_posts", "output", "data", "stats_post46v2_team_seasons.csv")
PLOT_WIDTH  <- 4          # 4 in x 300 DPI = 1200 px
PLOT_HEIGHT <- 8 / 3      # 2.667 in x 300 DPI = 800 px
PLOT_DPI    <- 300
NAVY <- "#0072B2"; GOLD <- "#E69F00"
X_LIM <- c(-6.5, 6.5)

# Verified console values (stats_post46_pull_v2.R, Section 1)
OFF_ALONE_10 <- 4.931531; DEF_ALONE_10 <- 5.481235     # per 0.10, magnitudes
OFF_HELD_10  <- 4.364545; DEF_HELD_10  <- 4.396254
OFF_COEF  <-  43.64545
DEF_COEF  <- -43.96254
R_OFF_DEF <- -0.1790548
N_ROWS    <- 160L
TOL       <- 1e-4

# Label geometry limits from the Python text-fit check (6.5 pt bold, 60.3 px/pt)
NAVY_LABEL_MIN <- 2.95    # points of bar width needed for "Defense +x.x" inside
GOLD_LABEL_HALF <- 1.40   # half-width of "Offense +x.x" in points

gate <- function(ok, msg) {
  if (!isTRUE(ok)) stop("GATE FAILED: ", msg, call. = FALSE)
  cat("  [PASS] ", msg, "\n", sep = "")
}

# ---- PRE-BUILD ASSERTIONS -------------------------------------------------------
cat("\nPre-build assertions\n")
gate(file.exists(F_TS), "stats_post46v2_team_seasons.csv exists (run stats_post46_pull_v2.R first)")
ts <- utils::read.csv(F_TS, stringsAsFactors = FALSE)
gate(all(c("point_diff", "off_epa", "def_epa") %in% names(ts)), "CSV has point_diff, off_epa, def_epa")
gate(nrow(ts) == N_ROWS && !anyNA(ts[, c("point_diff", "off_epa", "def_epa")]),
     "160 team-seasons, no missing values")

m_off  <- lm(point_diff ~ off_epa, data = ts)
m_def  <- lm(point_diff ~ def_epa, data = ts)
m_both <- lm(point_diff ~ off_epa + def_epa, data = ts)
b_fit <- unname(coef(m_both)["off_epa"]); c_fit <- unname(coef(m_both)["def_epa"])
r_od  <- cor(ts$off_epa, ts$def_epa)
cat(sprintf("  refit: off coef %.5f | def coef %.5f | r(off, def) %.7f\n", b_fit, c_fit, r_od))
gate(abs(b_fit - OFF_COEF) < TOL, "offense coefficient matches console (43.64545)")
gate(abs(c_fit - DEF_COEF) < TOL, "defense coefficient matches console (-43.96254)")
gate(abs(r_od - R_OFF_DEF) < 1e-6, "r(off, def) matches console (-0.1790548)")
gate(abs(0.1 * coef(m_def)[2] + DEF_ALONE_10) < TOL && abs(0.1 * coef(m_off)[2] - OFF_ALONE_10) < TOL,
     "one-input slopes match console (5.48, 4.93 per 0.10)")

# Subtitle numbers come from the post; confirm they round as written
gate(sprintf("%.1f", DEF_ALONE_10) == "5.5" && sprintf("%.1f", DEF_HELD_10) == "4.4",
     "subtitle reads 5.5 (alone) and 4.4 (held fixed), as in the post")

# Thirds by defense: lowest EPA allowed = best defense
cuts <- quantile(ts$def_epa, c(1/3, 2/3))
ts$tier <- ifelse(ts$def_epa <= cuts[1], "best", ifelse(ts$def_epa <= cuts[2], "middle", "worst"))
ts$miss <- resid(m_both)
cat("  rows per third:\n"); print(table(ts$tier))
gate(all(table(ts$tier) >= 50), "each third has at least 50 team-seasons")

split_one <- function(t) {
  d <- ts[ts$tier == t, ]
  data.frame(tier = t, n = nrow(d),
             from_defense = c_fit * (mean(d$def_epa) - mean(ts$def_epa)),
             from_offense = b_fit * (mean(d$off_epa) - mean(ts$off_epa)),
             mean_miss    = mean(d$miss),
             actual       = mean(d$point_diff) - mean(ts$point_diff))
}
split_tbl <- rbind(split_one("worst"), split_one("middle"), split_one("best"))
cat("\n  Split of each third's average point differential vs league average:\n")
print(split_tbl, row.names = FALSE, digits = 4)

gate(max(abs(split_tbl$from_defense + split_tbl$from_offense + split_tbl$mean_miss -
             split_tbl$actual)) < 1e-8, "defense part + offense part + mean miss = actual, every third")
best  <- split_tbl[split_tbl$tier == "best", ]
worst <- split_tbl[split_tbl$tier == "worst", ]
gate(best$from_offense > 0 && worst$from_offense < 0,
     "offense slice is positive for the best-defense third and negative for the worst")
gate(best$from_defense > 0 && worst$from_defense < 0, "defense part points the expected way")
gate(max(abs(c(best$mean_miss, worst$mean_miss))) < 0.5,
     "mean miss under 0.5 in both outer thirds (bars stand in for the actual averages)")

# Label geometry (from the Python text-fit check)
gate(min(abs(c(best$from_defense, worst$from_defense))) >= NAVY_LABEL_MIN,
     "navy bars wide enough for their inside labels")
mids <- c(best$from_defense + best$from_offense / 2, worst$from_defense + worst$from_offense / 2)
gate(all(abs(mids) + GOLD_LABEL_HALF <= X_LIM[2]), "gold labels above the slices stay inside the axis window")
gate(all(abs(c(best$from_defense + best$from_offense, worst$from_defense + worst$from_offense)) < X_LIM[2]),
     "bars stay inside the axis window")

# ---- DATA FOR THE PLOT -------------------------------------------------------
tiers <- c("Worst third of defenses", "Best third of defenses")    # bottom to top
outer <- split_tbl[split_tbl$tier %in% c("worst", "best"), ]
outer$label_tier <- ifelse(outer$tier == "best", tiers[2], tiers[1])
bars <- rbind(
  data.frame(tier = outer$label_tier, part = "defense", value = outer$from_defense),
  data.frame(tier = outer$label_tier, part = "offense", value = outer$from_offense)
)
bars$tier <- factor(bars$tier, levels = tiers)
bars$part <- factor(bars$part, levels = c("offense", "defense"))   # defense stacks next to zero

navy_lab <- data.frame(tier = factor(outer$label_tier, levels = tiers),
                       x = outer$from_defense / 2,
                       label = sprintf("Defense %+.1f", outer$from_defense))
gold_lab <- data.frame(tier = factor(outer$label_tier, levels = tiers),
                       x = outer$from_defense + outer$from_offense / 2,
                       label = sprintf("Offense %+.1f", outer$from_offense))
stopifnot("Plot data has no rows" = nrow(bars) == 4L, !anyNA(bars$value))

# ---- PLOT ----------------------------------------------------------------------
p <- ggplot(bars, aes(x = value, y = tier)) +
  geom_vline(xintercept = 0, color = "gray60", linewidth = 0.3) +
  geom_col(aes(fill = part), width = 0.45, position = position_stack(), show.legend = FALSE) +
  geom_text(data = navy_lab, aes(x = x, y = tier, label = label), inherit.aes = FALSE,
            color = "white", size = 6.5 / .pt, fontface = "bold") +
  geom_text(data = gold_lab, aes(x = x, y = tier, label = label), inherit.aes = FALSE,
            color = GOLD, size = 6.5 / .pt, fontface = "bold",
            position = position_nudge(y = 0.36)) +
  scale_fill_manual(values = c(defense = NAVY, offense = GOLD)) +
  scale_y_discrete(expand = expansion(add = c(0.45, 0.6))) +
  scale_x_continuous(breaks = seq(-6, 6, by = 2)) +
  coord_cartesian(xlim = X_LIM) +
  labs(
    title    = "Measured alone, defense also got credit for its offense",
    subtitle = paste0(
      sprintf("Gold was counted as defense when defense was measured alone: %.1f per 0.10 EPA.\n", DEF_ALONE_10),
      sprintf("Held fixed, defense keeps only navy: %.1f. Thirds of 160 team-seasons, 2021 to 2025.", DEF_HELD_10)),
    x = "Points per game vs league average, the equation's split of each third",
    y = NULL
  ) +
  theme_minimal(base_size = 7) +
  theme(
    plot.title.position = "plot",
    plot.title    = element_text(face = "bold", size = 10, hjust = 0),
    plot.subtitle = element_text(color = "gray25", size = 6.5, hjust = 0, lineheight = 1.15),
    axis.text.y   = element_text(size = 7, face = "bold", color = "gray20"),
    axis.text.x   = element_text(size = 6, color = "gray40"),
    axis.title.x  = element_text(size = 6.5, color = "gray30"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(8.6, 8.6, 8.6, 8.6)
  )

# ---- SAVE -------------------------------------------------------------------------
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
png_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "multiple_regression.png"))
svg_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "multiple_regression.svg"))
ggsave(png_path, p, width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")

svg_written <- FALSE
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, p, width = PLOT_WIDTH, height = PLOT_HEIGHT, bg = "white",
         device = svglite::svglite)
  svg_written <- TRUE
} else {
  message("svglite not installed. PNG written, SVG skipped. install.packages(\"svglite\") to enable.")
}

# ---- CONSOLE SUMMARY ---------------------------------------------------------------
cat("\n=== VISUAL SCRIPT SUMMARY ===\n")
for (i in seq_len(nrow(outer))) {
  cat(sprintf("%-24s defense %+5.2f | offense %+5.2f | mean miss %+5.2f | actual %+5.2f (n = %d)\n",
              outer$label_tier[i], outer$from_defense[i], outer$from_offense[i],
              outer$mean_miss[i], outer$actual[i], outer$n[i]))
}
cat(sprintf("Subtitle link: defense %.1f alone -> %.1f held fixed (per 0.10 EPA per play)\n",
            DEF_ALONE_10, DEF_HELD_10))
cat("PNG:", png_path, "(1200 x 800 at", PLOT_DPI, "DPI)\n")
cat("SVG:", if (svg_written) svg_path else "skipped, svglite not installed", "\n")
