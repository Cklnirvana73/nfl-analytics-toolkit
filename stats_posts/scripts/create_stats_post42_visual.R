# ==============================================================================
# create_stats_post42_visual.R
# Statistics Series Post 42: Correlation and Causation
#
# One panel. Average fantasy points per game for the two groups in the prior
# season and in weeks 9 to 18, with the weeks 9 to 18 gap split into the part
# that already existed the season before (3.82) and the remainder (0.7).
#
# Visual purpose (NFL-38): the two groups were already 3.82 points per game
# apart the season before week 3, so most of the later 4.53 gap is visibly a
# gap that predates week 3.
#
# Numbers are HARDCODED from the verified console output of stats_post42_pull.R
# (2026-11-02, cache dated 2026-09-17). They are never re-simulated. A
# reproduction gate checks them against the CSVs that pull script wrote.
#
# Run from the nfl-analytics-toolkit project root:
#   source("stats_posts/scripts/create_stats_post42_visual.R")
# Outputs: stats_posts/output/plots/stats_post42_gap_already_there.png and .svg (1200x800)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(glue)
})

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------
OUT_DIR    <- here::here("stats_posts", "output", "plots")
DATA_DIR   <- here::here("stats_posts", "output", "data")
FILE_STEM  <- "stats_post42_gap_already_there"
WIDTH_PX   <- 1200
HEIGHT_PX  <- 800
DPI        <- 300
TOL        <- 1e-4

COL_COMPARISON <- "#0072B2"   # Okabe-Ito navy: actual values
COL_FIRST      <- "#E69F00"   # Okabe-Ito gold: actual values, neither wrong
COL_REMAINDER  <- "#999999"   # Okabe-Ito gray: the mixed remainder
COL_TEXT       <- "gray20"

# Verified values (stats_post42_pull.R console output, sections 2A to 2C)
N_FIRST       <- 115L
N_COMPARISON  <- 1829L
PRIOR_FIRST   <- 8.29296    # mean prior-season PPG, first group
PRIOR_COMP    <- 12.10980   # mean prior-season PPG, comparison group
OUT_FIRST     <- 7.65471    # mean weeks 9-18 PPG, first group
OUT_COMP      <- 12.18729   # mean weeks 9-18 PPG, comparison group
GAP_OUTCOME   <- -4.5326    # stored effect_ppg estimate
GAP_PRIOR     <- -3.8168
CI_LO         <- -5.5130    # stored basic bootstrap CI on the outcome gap
CI_HI         <- -3.4838

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# REPRODUCTION GATE: hardcoded values must match the pull script's CSVs
# ------------------------------------------------------------------------------
gate <- function(ok, msg) {
  if (!isTRUE(ok)) stop("GATE FAILED: ", msg, call. = FALSE)
  cat("  [PASS] ", msg, "\n", sep = "")
}

cat("\nReproduction gate\n")
f_groups  <- file.path(DATA_DIR, "stats_post42_r25_groups.csv")
f_summary <- file.path(DATA_DIR, "stats_post42_summary.csv")
gate(file.exists(f_groups) && file.exists(f_summary),
     "pull-script CSVs exist (run stats_post42_pull.R first)")

grp <- utils::read.csv(f_groups, stringsAsFactors = FALSE)
smy <- utils::read.csv(f_summary, stringsAsFactors = FALSE)
gate(all(c("group", "n", "mean_prior_ppg", "mean_outcome_ppg") %in% names(grp)),
     "groups CSV has the expected columns")
gate(all(c("study", "quantity", "estimate", "ci_lower", "ci_upper") %in% names(smy)),
     "summary CSV has the expected columns")

g_first <- grp[grp$group == "treatment", ]
g_comp  <- grp[grp$group == "control", ]
gate(nrow(g_first) == 1L && nrow(g_comp) == 1L, "one row per group")
gate(g_first$n == N_FIRST && g_comp$n == N_COMPARISON,
     glue("group sizes {N_FIRST} and {N_COMPARISON}"))
gate(abs(g_first$mean_prior_ppg - PRIOR_FIRST) < TOL &&
       abs(g_comp$mean_prior_ppg - PRIOR_COMP) < TOL,
     "prior-season means match")
gate(abs(g_first$mean_outcome_ppg - OUT_FIRST) < TOL &&
       abs(g_comp$mean_outcome_ppg - OUT_COMP) < TOL,
     "weeks 9-18 means match")

ci_row <- smy[smy$study == "R25" & smy$quantity == "outcome_gap", ]
gate(nrow(ci_row) == 1L &&
       abs(ci_row$estimate - GAP_OUTCOME) < TOL &&
       abs(ci_row$ci_lower - CI_LO) < TOL &&
       abs(ci_row$ci_upper - CI_HI) < TOL,
     "outcome gap and its interval match")

# ------------------------------------------------------------------------------
# PRE-BUILD ASSERTIONS: the post's central claims, checked in code
# ------------------------------------------------------------------------------
cat("\nPost claim assertions\n")
gap_out   <- OUT_FIRST - OUT_COMP
gap_prior <- PRIOR_FIRST - PRIOR_COMP
share     <- gap_prior / gap_out
remainder <- abs(gap_out) - abs(gap_prior)

gate(abs(gap_out - GAP_OUTCOME) < TOL, "outcome gap = 4.53 (post: '4.53 fewer')")
gate(abs(gap_prior - GAP_PRIOR) < TOL, "prior gap = 3.82 (post: '3.82 fewer')")
gate(round(100 * share) == 84, "share = 84% (post: '84% of the gap')")
gate(round(remainder, 1) == 0.7, "remainder = 0.7 (post: 'the remaining 0.7 points')")
gate(CI_HI < 0, "interval excludes zero (post: 'Zero ... is not in that range')")
gate(round(-CI_HI, 2) == 3.48 && round(-CI_LO, 2) == 5.51,
     "interval reads 3.48 to 5.51 (post wording)")

# ------------------------------------------------------------------------------
# PLOT DATA
# ------------------------------------------------------------------------------
X_PRIOR <- 1
X_OUT   <- 2

pts <- dplyr::tibble(
  group = factor(c("comparison", "first", "comparison", "first"),
                 levels = c("comparison", "first")),
  x     = c(X_PRIOR, X_PRIOR, X_OUT, X_OUT),
  ppg   = c(PRIOR_COMP, PRIOR_FIRST, OUT_COMP, OUT_FIRST)
)
stopifnot("Plot data has NA values" = !anyNA(pts$ppg), nrow(pts) == 4L)

# Bracket geometry for the weeks 9-18 gap, split at comparison minus 3.82
X_BRACKET   <- X_OUT + 0.14
split_point <- OUT_COMP - abs(gap_prior)   # bottom of the 'already there' part

already_lbl <- glue("{format(round(abs(gap_prior), 2), nsmall = 2)} already there\nthe season before")
rem_lbl     <- glue("{round(remainder, 1)}: week 3 mixed with\nordinary change")
prior_lbl   <- glue("{format(round(abs(gap_prior), 2), nsmall = 2)} apart")

# Labels sit above each trajectory at its midpoint, clear of the y-axis text
# and the brackets (widths checked in Python at 300 DPI before delivery).
X_MID <- (X_PRIOR + X_OUT) / 2
group_lbl <- dplyr::tibble(
  x     = c(X_MID, X_MID),
  ppg   = c((PRIOR_COMP + OUT_COMP) / 2, (PRIOR_FIRST + OUT_FIRST) / 2),
  label = c(glue("Involved every week, weeks 1 to 8 ({format(N_COMPARISON, big.mark = ',')})"),
            glue("Unused in week 3 ({N_FIRST})")),
  col   = c(COL_COMPARISON, COL_FIRST)
)

# ------------------------------------------------------------------------------
# BUILD
# ------------------------------------------------------------------------------
p <- ggplot(pts, aes(x = x, y = ppg, color = group, group = group)) +
  # Prior-season gap bracket (left of the prior-season points)
  annotate("segment", x = X_PRIOR - 0.14, xend = X_PRIOR - 0.14,
           y = PRIOR_FIRST, yend = PRIOR_COMP,
           color = COL_FIRST, linewidth = 1.6) +
  annotate("text", x = X_PRIOR - 0.18, y = (PRIOR_FIRST + PRIOR_COMP) / 2,
           label = prior_lbl, hjust = 1, size = 2.6, color = COL_TEXT,
           fontface = "bold") +
  # Weeks 9-18 gap bracket: the part already there, then the remainder
  annotate("segment", x = X_BRACKET, xend = X_BRACKET,
           y = split_point, yend = OUT_COMP,
           color = COL_FIRST, linewidth = 1.6) +
  annotate("segment", x = X_BRACKET, xend = X_BRACKET,
           y = OUT_FIRST, yend = split_point,
           color = COL_REMAINDER, linewidth = 1.6) +
  annotate("text", x = X_BRACKET + 0.04, y = (split_point + OUT_COMP) / 2,
           label = already_lbl, hjust = 0, size = 2.6, color = COL_TEXT,
           fontface = "bold", lineheight = 0.9) +
  annotate("text", x = X_BRACKET + 0.04, y = OUT_FIRST - 0.35,
           label = rem_lbl, hjust = 0, vjust = 1, size = 2.2,
           color = "gray35", lineheight = 0.9) +
  # Group trajectories
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  # Group labels placed ABOVE each trajectory
  annotate("text", x = group_lbl$x, y = group_lbl$ppg + 0.45,
           label = group_lbl$label, color = group_lbl$col,
           hjust = 0.5, vjust = 0, size = 2.3, fontface = "bold") +
  scale_color_manual(values = c(comparison = COL_COMPARISON, first = COL_FIRST),
                     guide = "none") +
  scale_x_continuous(breaks = c(X_PRIOR, X_OUT),
                     labels = c("Prior season", "Weeks 9 to 18")) +
  scale_y_continuous(breaks = seq(6, 14, by = 2)) +
  coord_cartesian(xlim = c(0.40, 2.85), ylim = c(6, 14), clip = "off") +
  labs(
    title    = "84% of the later gap was already there the season before",
    subtitle = glue("Average fantasy points per game, 2011 to 2025 player-seasons.\n",
                    "Weeks 9 to 18 gap: {format(round(abs(gap_out), 2), nsmall = 2)} ",
                    "(95% CI {format(round(-CI_HI, 2), nsmall = 2)} to ",
                    "{format(round(-CI_LO, 2), nsmall = 2)})"),
    x        = NULL,
    y        = "Fantasy points per game",
    caption  = "Toolkit fantasy scoring. Both groups: 4+ prior-season games, 3+ games in weeks 9 to 18."
  ) +
  theme_minimal(base_size = 7) +
  theme(
    plot.title         = element_text(face = "bold", size = 9, hjust = 0, color = COL_TEXT),
    plot.subtitle      = element_text(size = 6.2, hjust = 0, color = "gray35"),
    plot.caption       = element_text(size = 5.2, hjust = 1, color = "gray50"),
    axis.text.x        = element_text(size = 7, face = "bold", color = COL_TEXT),
    axis.text.y        = element_text(size = 6, color = "gray40"),
    axis.title.y       = element_text(size = 6.5, color = "gray30"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(8, 10, 6, 8),
    plot.background    = element_rect(fill = "white", color = NA)
  )

# ------------------------------------------------------------------------------
# POST-BUILD ASSERTIONS
# ------------------------------------------------------------------------------
cat("\nPost-build assertions\n")
gate(inherits(p, "ggplot"), "plot object built")
# Layer order: 1-6 annotations, 7 geom_line, 8 geom_point, 9 group labels.
# Indexed by number, not p$layers, so it runs on ggplot2 3.x and 4.0 alike.
built <- ggplot2::layer_data(p, i = 8L)
gate(nrow(built) == 4L && !anyNA(built$y), "point layer has 4 non-NA points")
gate(all(built$y >= 6 & built$y <= 14), "every point sits inside the 6 to 14 view")
gate(abs((OUT_COMP - split_point) - abs(gap_prior)) < 1e-9,
     "gold bracket length equals the 3.82 prior gap")
gate(abs((split_point - OUT_FIRST) - remainder) < 1e-9,
     "gray bracket length equals the 0.7 remainder")

# ------------------------------------------------------------------------------
# SAVE
# ------------------------------------------------------------------------------
png_path <- file.path(OUT_DIR, paste0(FILE_STEM, ".png"))
svg_path <- file.path(OUT_DIR, paste0(FILE_STEM, ".svg"))

ggsave(png_path, plot = p, width = WIDTH_PX, height = HEIGHT_PX,
       units = "px", dpi = DPI, bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, plot = p, width = WIDTH_PX, height = HEIGHT_PX,
         units = "px", dpi = DPI, bg = "white")
} else {
  warning("svglite not installed; SVG not written. install.packages('svglite')")
}

# ------------------------------------------------------------------------------
# CONSOLE SUMMARY
# ------------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("Stats Post 42 visual\n")
cat(glue("  Prior-season gap: {round(abs(gap_prior), 2)} | Weeks 9-18 gap: ",
         "{round(abs(gap_out), 2)} | Share already there: {round(100 * share)}% | ",
         "Remainder: {round(remainder, 1)}"), "\n")
cat("  PNG: ", png_path, "\n", sep = "")
cat("  SVG: ", if (file.exists(svg_path)) svg_path else "not written", "\n", sep = "")
cat(strrep("=", 70), "\n", sep = "")
