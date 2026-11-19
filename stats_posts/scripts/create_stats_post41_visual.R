# ==============================================================================
# Statistics Series: Post 41 visual
# Kendall's rank correlation: pairs that held their order minus pairs that flipped
#
# Purpose sentence: for each position, one bar split into the share of all
# player pairs that kept their order and the share that flipped, with tau
# printed beside it, so the reader sees tau = held share minus flipped share
# and sees quarterbacks hold up least.
#
# Reads the saved pull artifacts behind a reproduction gate. Nothing is
# re-simulated. Every number on the chart is checked against verified console
# output from pull_stats_post40_rank_correlation_data.R (2026-10-26) before
# anything is drawn. The pair counts are also recomputed from the player-level
# file, so the chart does not rest on the summary file alone.
#
# Output: stats_posts/output/plots/stats_post41_kendall_pairs_held.png and .svg,
# 1200 x 800 px (12 x 8 in at 100 dpi).
# HOW TO RUN: source("stats_posts/scripts/create_stats_post41_visual.R") from the project root.
# Assumed versions: R 4.3+, dplyr 1.1+, tidyr 1.3+, ggplot2 3.5+ (4.0 safe).
# ==============================================================================

# ---- CONFIGURATION -----------------------------------------------------------
TOOLKIT_ROOT <- here::here()
if (!dir.exists(TOOLKIT_ROOT)) stop("TOOLKIT_ROOT not found: ", TOOLKIT_ROOT)
DATA_DIR  <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "data")
SUM_PATH  <- file.path(DATA_DIR, "stats_post40_position_summary.csv")
PAIR_PATH <- file.path(DATA_DIR, "stats_post40_half_pairs.csv")
SENS_PATH <- file.path(DATA_DIR, "stats_post40_sensitivity.csv")
for (f in c(SUM_PATH, PAIR_PATH, SENS_PATH)) {
  if (!file.exists(f)) stop("Run pull_stats_post40_rank_correlation_data.R first: ", f)
}
PLOT_DIR <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "plots")
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)
OUT_STEM <- file.path(PLOT_DIR, "stats_post41_kendall_pairs_held")

SEASON    <- 2025L
TOL       <- 1e-4
COL_HELD  <- "#0072B2"   # Okabe-Ito navy: actual held share
COL_FLIP  <- "#E69F00"   # Okabe-Ito gold: flipped share (not a wrong value)
COL_TIE   <- "gray70"    # tied pairs (receivers 13, tight ends 1)
COL_TEXT  <- "gray25"

# Verified console values (2026-10-26 pull)
EXPECT <- tibble::tribble(
  ~position, ~n,  ~n_pairs, ~held, ~flipped, ~tied, ~tau,
  "RB",      75L, 2775,     2288,  487,      0,     0.649009,
  "TE",      61L, 1830,     1442,  387,      1,     0.576660,
  "WR",     114L, 6441,     5040,  1388,     13,    0.567698,
  "QB",      24L, 276,      171,   105,      0,     0.239130
)
POS_LABEL <- c(RB = "Running backs", TE = "Tight ends", WR = "Receivers", QB = "Quarterbacks")

library(dplyr)
library(tidyr)
library(ggplot2)

# ---- LOAD --------------------------------------------------------------------
summ  <- readr::read_csv(SUM_PATH,  show_col_types = FALSE)
pairs <- readr::read_csv(PAIR_PATH, show_col_types = FALSE)
sens  <- readr::read_csv(SENS_PATH, show_col_types = FALSE)
need  <- c("position", "n", "n_pairs", "concordant", "discordant",
           "tied_x", "tied_y", "tied_both", "kendall", "spearman")
miss  <- setdiff(need, names(summ))
if (length(miss) > 0) stop("Missing summary columns: ", paste(miss, collapse = ", "))

# ---- GATE 1: independent pair count from the player-level file ----------------
count_pairs <- function(r1, r2) {
  ij <- utils::combn(length(r1), 2)
  s  <- sign(r1[ij[1, ]] - r1[ij[2, ]]) * sign(r2[ij[1, ]] - r2[ij[2, ]])
  c(held = sum(s > 0), flipped = sum(s < 0), tied = sum(s == 0))
}
recount <- pairs %>%
  group_by(position) %>%
  summarise(n = n(),
            held    = count_pairs(rank_h1, rank_h2)[["held"]],
            flipped = count_pairs(rank_h1, rank_h2)[["flipped"]],
            tied    = count_pairs(rank_h1, rank_h2)[["tied"]],
            # Ranks, not ppg: the CSV prints ppg to 15 digits, which creates
            # ties that the full-precision values did not have (TE, WR).
            tau_b   = cor(rank_h1, rank_h2, method = "kendall"),
            .groups = "drop")

chk <- EXPECT %>%
  inner_join(summ,    by = "position", suffix = c("", ".sum")) %>%
  inner_join(recount, by = "position", suffix = c("", ".re"))

stopifnot(
  "Four positions expected"                    = nrow(chk) == 4,
  "Player counts differ"                       = all(chk$n == chk$n.sum & chk$n == chk$n.re),
  "Pair totals are not n(n-1)/2"               = all(chk$n_pairs == chk$n * (chk$n - 1) / 2),
  "Summary pair totals differ"                 = all(chk$n_pairs == chk$n_pairs.sum),
  "Held counts differ (summary or recount)"    = all(chk$held == chk$concordant & chk$held == chk$held.re),
  "Flipped counts differ (summary or recount)" = all(chk$flipped == chk$discordant & chk$flipped == chk$flipped.re),
  "Tied counts differ from recount"            = all(chk$tied == chk$tied.re),
  "Tied counts disagree with summary ties"     = all(chk$tied == chk$tied_x + chk$tied_y - chk$tied_both),
  "Held + flipped + tied is not all pairs"     = all(chk$held + chk$flipped + chk$tied == chk$n_pairs),
  "Summary tau differs from verified"          = all(abs(chk$kendall - chk$tau) < 1e-5),
  "cor(kendall) recount differs"               = all(abs(chk$tau_b - chk$tau) < 1e-5)
)

# ---- GATE 2: the post's central claims ---------------------------------------
rb <- filter(chk, position == "RB")
qb <- filter(chk, position == "QB")
stopifnot(
  "RB held share is not 0.8245"           = round(rb$held / rb$n_pairs, 4) == 0.8245,
  "RB flipped share is not 0.1755"        = round(rb$flipped / rb$n_pairs, 4) == 0.1755,
  "RB held minus flipped is not 0.649"    = round((rb$held - rb$flipped) / rb$n_pairs, 3) == 0.649,
  "RB Spearman is not 0.840"              = round(rb$spearman, 3) == 0.840,
  "QB held share is not 61.96 percent"    = round(100 * qb$held / qb$n_pairs, 2) == 61.96,
  "RB held share is not 82.45 percent"    = round(100 * rb$held / rb$n_pairs, 2) == 82.45,
  "Tau order is not RB > TE > WR > QB"    = identical(chk$position[order(-chk$tau)], c("RB", "TE", "WR", "QB"))
)
# Ties move tau-b away from (C - D) / N by under 0.001 at WR and TE
stopifnot("Tie adjustment larger than 0.001" =
            all(abs(chk$tau - (chk$held - chk$flipped) / chk$n_pairs) < 0.001))
# Position order on tau holds at four, five and six games
sens_order <- sens %>% group_by(min_games) %>% arrange(desc(kendall), .by_group = TRUE) %>%
  summarise(ord = paste(position, collapse = ">"), .groups = "drop")
stopifnot("Sensitivity runs missing" = nrow(sens_order) == 3,
          "Tau order changes across 4/5/6 games" = all(sens_order$ord == "RB>TE>WR>QB"))
cat("Reproduction gate PASS: counts, shares, tau, and position order all match.\n")

# ---- PLOT DATA ---------------------------------------------------------------
pos_levels <- rev(c("RB", "TE", "WR", "QB"))      # RB drawn at top
bar_df <- chk %>%
  transmute(position, n, n_pairs, tau,
            Held = held / n_pairs, Flipped = flipped / n_pairs, Tied = tied / n_pairs) %>%
  pivot_longer(c(Held, Flipped, Tied), names_to = "part", values_to = "share") %>%
  mutate(part     = factor(part, levels = c("Tied", "Flipped", "Held")),  # Held leftmost
         position = factor(position, levels = pos_levels))

lab_df <- chk %>%
  mutate(position  = factor(position, levels = pos_levels),
         ylab      = paste0(POS_LABEL[as.character(position)], "\n",
                            n, " players, ", format(n_pairs, big.mark = ","), " pairs"),
         held_lab  = sprintf("%.2f%% held", 100 * held / n_pairs),
         flip_lab  = sprintf("%.2f%% flipped", 100 * flipped / n_pairs),
         flip_x    = held / n_pairs + (flipped / n_pairs) / 2,
         tau_lab   = sprintf("\u03c4 = %.3f", tau))
y_labels <- setNames(lab_df$ylab, as.character(lab_df$position))

rb_eq <- "Running backs: 0.8245 held - 0.1755 flipped = 0.649"

p <- ggplot(bar_df, aes(x = share, y = position, fill = part)) +
  geom_col(width = 0.62, position = position_stack(reverse = FALSE)) +
  geom_text(data = lab_df, aes(x = 0.02, y = position, label = held_lab),
            inherit.aes = FALSE, hjust = 0, size = 4.8, fontface = "bold", color = "white") +
  geom_text(data = lab_df, aes(x = flip_x, y = position, label = flip_lab),
            inherit.aes = FALSE, hjust = 0.5, size = 3.6, color = "gray15") +
  geom_text(data = lab_df, aes(x = 1.025, y = position, label = tau_lab),
            inherit.aes = FALSE, hjust = 0, size = 5.2, fontface = "bold", color = COL_TEXT) +
  annotate("text", x = 0, y = 4.45, label = rb_eq, hjust = 0, vjust = 0,
           size = 4.4, color = COL_HELD, fontface = "bold") +
  scale_fill_manual(values = c(Held = COL_HELD, Flipped = COL_FLIP, Tied = COL_TIE),
                    breaks = c("Held", "Flipped", "Tied"),
                    labels = c("Kept their order", "Flipped", "Tied"), name = NULL) +
  scale_y_discrete(labels = y_labels, expand = expansion(add = c(0.5, 0.95))) +
  scale_x_continuous(breaks = seq(0, 1, 0.25),
                     labels = function(x) paste0(100 * x, "%")) +
  coord_cartesian(xlim = c(0, 1.16), clip = "off") +
  labs(
    title    = "Of every two players, how often did the one ahead stay ahead?",
    subtitle = paste0(SEASON, " regular season, full-PPR points per game, weeks 1 to 9 vs weeks 10 to 18. ",
                      "Players with at least five games in each half.\n",
                      "Each bar is every pair of players at that position, split by whether the pair kept its order."),
    x        = "Share of all pairs",
    y        = NULL,
    caption  = paste0(
      "\u03c4 (tau) = (C - D) / N   Plain text: tau = (C - D) / N, where C = pairs that kept their order, ",
      "D = pairs that flipped, N = all pairs.\n",
      "Receivers had 13 tied pairs and tight ends 1 (gray, too thin to see); their \u03c4 uses the standard tie ",
      "adjustment (tau-b), which changes it by less than 0.001.\n",
      "Position order on \u03c4 is the same with a four- or six-game minimum. Data: nflreadr weekly player stats.")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title            = element_text(face = "bold", size = 19, hjust = 0),
    plot.subtitle         = element_text(color = "gray35", size = 11.5, hjust = 0),
    plot.caption          = element_text(color = "gray45", size = 9.5, hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = "top",
    legend.justification  = "left",
    legend.text           = element_text(size = 11),
    panel.grid.major.y    = element_blank(),
    panel.grid.minor      = element_blank(),
    axis.text.y           = element_text(size = 11.5, color = COL_TEXT, hjust = 0, lineheight = 1.1),
    plot.margin           = margin(16, 20, 12, 16)
  )

# ---- POST-BUILD ASSERTIONS ---------------------------------------------------
bars <- layer_data(p, 1)
stopifnot(
  "Expected 12 segments (4 positions x 3 parts)" = nrow(bars) == 12,
  "Each position's bar does not reach 100%"      = all(abs(tapply(bars$xmax, bars$y, max) - 1) < 1e-9),
  "Held segments do not start at 0"              = all(bars$xmin[bars$fill == COL_HELD] == 0)
)
held_rb <- bars$xmax[bars$fill == COL_HELD & bars$y == which(pos_levels == "RB")]
stopifnot("RB held segment is not 0.8245" = round(held_rb, 4) == 0.8245)

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
cat("\n==== POST 41 VISUAL ====\n")
cat("Saved:", paste0(OUT_STEM, ".png"), "(1200 x 800)\n")
print(chk %>% transmute(position, n, n_pairs, held, flipped, tied,
                        held_pct = round(100 * held / n_pairs, 2), tau = round(tau, 3)))
