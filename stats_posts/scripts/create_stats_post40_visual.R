# ==============================================================================
# Statistics Series: Post 40 visual
# Spearman's rank correlation: one squared rank move dominates the sum
#
# Purpose sentence: every quarterback's squared rank move as a bar, so the
# reader sees Jackson's 361 dwarf the other 23, the squaring mechanism the
# text can only describe.
#
# Reads the saved pull artifact behind a reproduction gate. Nothing is
# re-simulated. Every number on the chart is checked against verified console
# output from pull_stats_post40_rank_correlation_data.R (2026-10-26) before
# anything is drawn.
#
# Output: stats_posts/output/plots/stats_post40_spearman_squared_moves.png and .svg,
# 1200 x 800 px (12 x 8 in at 100 dpi).
# HOW TO RUN: source("stats_posts/scripts/create_stats_post40_visual.R") from the project root.
# Assumed versions: R 4.3+, dplyr 1.1+, ggplot2 3.5+ (4.0 safe).
# ==============================================================================

# ---- CONFIGURATION -----------------------------------------------------------
TOOLKIT_ROOT <- here::here()
if (!dir.exists(TOOLKIT_ROOT)) stop("TOOLKIT_ROOT not found: ", TOOLKIT_ROOT)
DATA_PATH <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "data", "stats_post40_half_pairs.csv")
PLOT_DIR  <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "plots")
if (!file.exists(DATA_PATH)) stop("Run pull_stats_post40_rank_correlation_data.R first: ", DATA_PATH)
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)
OUT_STEM <- file.path(PLOT_DIR, "stats_post40_spearman_squared_moves")

SEASON      <- 2025L
TOL         <- 1e-6
COL_ACTUAL  <- "#0072B2"   # Okabe-Ito navy: actual values
COL_FOCUS   <- "#E69F00"   # Okabe-Ito gold: the highlighted player (not a wrong value)
COL_TEXT    <- "gray25"

# Verified console values (2026-10-26 pull and device check)
EXPECT <- list(
  n_qb        = 24L,
  sum_d2      = 1484,
  jackson_d2  = 361,
  jackson_r1  = 1,
  jackson_r2  = 20,
  spearman    = 0.354783,
  next_d2     = 225            # second-largest squared move (Trevor Lawrence)
)

library(dplyr)
library(ggplot2)

# ---- LOAD AND GATE -----------------------------------------------------------
pairs <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
need  <- c("player_name", "position", "rank_h1", "rank_h2", "ppg_h1", "ppg_h2")
miss  <- setdiff(need, names(pairs))
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse = ", "))

qb <- pairs %>%
  filter(position == "QB") %>%
  mutate(d = rank_h2 - rank_h1, d2 = d^2)

n <- nrow(qb)
jk <- qb %>% filter(player_name == "Lamar Jackson")
rho_formula <- 1 - 6 * sum(qb$d2) / (n * (n^2 - 1))
rho_cor     <- cor(qb$ppg_h1, qb$ppg_h2, method = "spearman")

stopifnot(
  "QB count differs from verified 24"           = n == EXPECT$n_qb,
  "No ties expected in QB ranks"                = all(qb$rank_h1 == round(qb$rank_h1)) &&
                                                  all(qb$rank_h2 == round(qb$rank_h2)),
  "Sum of squared moves differs from 1,484"     = sum(qb$d2) == EXPECT$sum_d2,
  "Exactly one Lamar Jackson row"               = nrow(jk) == 1,
  "Jackson ranks differ from 1 and 20"          = jk$rank_h1 == EXPECT$jackson_r1 &&
                                                  jk$rank_h2 == EXPECT$jackson_r2,
  "Jackson d^2 differs from 361"                = jk$d2 == EXPECT$jackson_d2,
  "Jackson is not the largest squared move"     = jk$d2 == max(qb$d2),
  "Second-largest squared move differs from 225"= sort(qb$d2, decreasing = TRUE)[2] == EXPECT$next_d2,
  "Formula rho does not reproduce 0.354783"     = abs(rho_formula - EXPECT$spearman) < TOL,
  "cor(spearman) does not match the formula"    = abs(rho_cor - rho_formula) < 1e-10
)
share <- jk$d2 / sum(qb$d2)
stopifnot("Jackson share is not 24.3%" = round(100 * share, 1) == 24.3)
cat("Reproduction gate PASS: sum d^2 =", sum(qb$d2), "| Jackson =", jk$d2,
    "| share =", round(100 * share, 1), "% | rho =", round(rho_formula, 3), "\n")

# ---- PLOT DATA ---------------------------------------------------------------
plot_df <- qb %>%
  arrange(d2, desc(player_name)) %>%                    # smallest first -> drawn at bottom
  mutate(player_name = factor(player_name, levels = player_name),
         is_jackson  = player_name == "Lamar Jackson")

lab_jk <- "Jackson, 1st to 20th: 19 \u00d7 19 = 361, 24.3 percent of all 1,484"

p <- ggplot(plot_df, aes(x = d2, y = player_name, fill = is_jackson)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = d2), hjust = -0.25, size = 3.4, color = COL_TEXT) +
  annotate("text", x = 361, y = n + 1.15, label = lab_jk,
           hjust = 1, vjust = 0, size = 4.6, fontface = "bold", color = COL_FOCUS) +
  scale_fill_manual(values = c(`TRUE` = COL_FOCUS, `FALSE` = COL_ACTUAL), guide = "none") +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.9))) +
  coord_cartesian(xlim = c(0, 400), clip = "off") +
  labs(
    title    = "One quarterback's slide supplied 24.3 percent of the squared rank moves",
    subtitle = paste0("24 quarterbacks, ", SEASON, " regular season, ranked by full-PPR points per game, ",
                      "weeks 1 to 9 vs weeks 10 to 18.\n",
                      "Each bar is d\u00b2 (d squared): one player's change in rank between the halves, times itself."),
    x        = "Squared rank move, d\u00b2 (d squared)",
    y        = NULL,
    caption  = paste0(
      "\u03c1 (rho) = 1 - 6\u2211d\u00b2 / (n(n\u00b2 - 1))   Plain text: rho = 1 - 6 * sum(d^2) / (n * (n^2 - 1))   ",
      "Here: 1 - 6 * 1,484 / (24 * 575) = 0.355\n",
      "Quarterbacks with at least five games in each half. No tied ranks. Data: nflreadr weekly player stats.")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle      = element_text(color = "gray35", size = 11.5, hjust = 0),
    plot.caption       = element_text(color = "gray45", size = 9.5, hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 10.5, color = COL_TEXT),
    plot.margin        = margin(16, 30, 12, 16)
  )

# ---- POST-BUILD ASSERTIONS ---------------------------------------------------
bars <- layer_data(p, 1)
stopifnot(
  "Bar count is not 24"                    = nrow(bars) == EXPECT$n_qb,
  "Tallest bar is not 361"                 = max(bars$xmax) == EXPECT$jackson_d2,
  "Bars do not sum to 1,484"               = sum(bars$xmax) == EXPECT$sum_d2,
  "Exactly one gold bar"                   = sum(bars$fill == COL_FOCUS) == 1,
  "Gold bar is not the 361 bar"            = bars$xmax[bars$fill == COL_FOCUS] == EXPECT$jackson_d2
)

# ---- SAVE --------------------------------------------------------------------
png_dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
ggsave(paste0(OUT_STEM, ".png"), p, width = 12, height = 8, units = "in", dpi = 100,
       device = png_dev, bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(paste0(OUT_STEM, ".svg"), p, width = 12, height = 8, units = "in", bg = "white")
} else {
  message("svglite not installed; SVG skipped. install.packages('svglite') to enable.")
}

# ---- CONSOLE SUMMARY ---------------------------------------------------------
cat("\n==== POST 40 VISUAL ====\n")
cat("Saved:", paste0(OUT_STEM, ".png"), "(1200 x 800)\n")
cat("QBs:", n, "| sum d^2:", sum(qb$d2), "| Jackson d^2:", jk$d2,
    "(", round(100 * share, 1), "% ) | next largest:", sort(qb$d2, decreasing = TRUE)[2], "\n")
cat("Spearman rho (formula):", round(rho_formula, 6), "\n")
