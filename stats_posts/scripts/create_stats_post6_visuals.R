# ==============================================================================
# Statistics Series | Post 6: Percentiles and Quantiles
# Visuals for LinkedIn post
#
# Outputs (300 DPI):
#   stats_posts/output/plots/stats_post6_equal_count.png
#   stats_posts/output/plots/stats_post6_cut_collision.png
#
# Requires: nflreadr, ggplot2, dplyr, tidyr
# Run from project root. Internet access required for nflreadr on first pull.
#
# SCOPE NOTE
#   These plots illustrate the quantile MECHANICS on 2025 player scoring so the
#   visual is self contained and reproducible. The prospect scoring tier system
#   applies the same quantile cut to its prospect metrics. Same mechanics,
#   different table.
# ==============================================================================

# CONFIGURATION ----------------------------------------------------------------
POST_NUM   <- 6L
CONCEPT    <- "Percentiles and Quantiles"
SEASON     <- 2025L
MIN_GAMES  <- 8L
SKILL_POS  <- c("QB", "RB", "WR", "TE")
N_BINS     <- 5L
OUTPUT_DIR <- "stats_posts/output/plots"
DPI        <- 300L

# SCORING (series standard: flat 1.0 PPR, pass TD = 6) -------------------------
PPR_PASS_YD   <-  0.04
PPR_PASS_TD   <-  6.00
PPR_INT       <- -2.00
PPR_RUSH_YD   <-  0.10
PPR_RUSH_TD   <-  6.00
PPR_RECEPTION <-  1.00
PPR_REC_YD    <-  0.10
PPR_REC_TD    <-  6.00

# COLORS -----------------------------------------------------------------------
BIN_FILLS   <- c("#D6E4F0", "#9DC3E6", "#5B9BD5", "#2E75B6", "#1F4E79")
FILL_HIST   <- "#9DC3E6"
CUT_LINE    <- "#C0392B"   # red, dashed (the quantile cut points)
COL_TEXT    <- "#1C1C1A"
COL_SUBTEXT <- "#5F5E5A"
COL_BG      <- "#FAFAF8"
COL_PANEL   <- "#FFFFFF"
COL_GRID    <- "#E8E6DF"
COL_STRIP   <- "#ECEAE3"

# LIBRARIES --------------------------------------------------------------------
library(nflreadr)
library(ggplot2)
library(dplyr)
library(tidyr)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# SHARED THEME -----------------------------------------------------------------
theme_stats_series <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = COL_BG,    color = NA),
      panel.background = element_rect(fill = COL_PANEL, color = NA),
      panel.grid.major = element_line(color = COL_GRID, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = COL_STRIP, color = NA),
      strip.text       = element_text(face = "bold", size = 11, color = COL_TEXT),
      plot.title       = element_text(face = "bold", size = 14, color = COL_TEXT,
                                      margin = margin(b = 6)),
      plot.subtitle    = element_text(size = 10, color = COL_SUBTEXT,
                                      margin = margin(b = 10)),
      plot.caption     = element_text(size = 9, color = COL_SUBTEXT,
                                      margin = margin(t = 8)),
      axis.title       = element_text(size = 10, color = COL_SUBTEXT),
      axis.text        = element_text(size = 9,  color = COL_SUBTEXT),
      legend.title     = element_text(size = 9,  color = COL_TEXT),
      legend.text      = element_text(size = 9,  color = COL_SUBTEXT),
      plot.margin      = margin(16, 20, 12, 16)
    )
}

# DATA: one season value per player --------------------------------------------
message("Loading 2025 weekly player stats from nflreadr ...")
weekly_raw <- nflreadr::load_player_stats(seasons = SEASON)

# Resolve interceptions column across nflreadr schema versions, then alias to a
# stable name so the scoring code below stays unchanged. Newer nflverse stats
# use passing_interceptions; older releases use interceptions.
int_col <- if ("passing_interceptions" %in% names(weekly_raw)) {
  "passing_interceptions"
} else if ("interceptions" %in% names(weekly_raw)) {
  "interceptions"
} else {
  NA_character_
}
if (is.na(int_col)) {
  stop("nflreadr output has neither 'passing_interceptions' nor 'interceptions'.",
       call. = FALSE)
}
weekly_raw$interceptions <- weekly_raw[[int_col]]

req_cols <- c("season_type", "week", "position", "player_id",
              "passing_yards", "passing_tds", "interceptions",
              "rushing_yards", "rushing_tds",
              "receptions", "receiving_yards", "receiving_tds")
missing_cols <- setdiff(req_cols, names(weekly_raw))
if (length(missing_cols) > 0L) {
  stop(paste0("nflreadr output missing required columns: ",
              paste(missing_cols, collapse = ", ")), call. = FALSE)
}

# Flexible player name column across nflreadr versions
name_col <- if ("player_display_name" %in% names(weekly_raw)) {
  "player_display_name"
} else {
  "player_name"
}

weekly <- weekly_raw %>%
  dplyr::filter(
    .data$season_type == "REG",
    .data$week <= 18,
    .data$position %in% SKILL_POS
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(passing_yards, passing_tds, interceptions,
        rushing_yards, rushing_tds,
        receptions, receiving_yards, receiving_tds),
      ~ tidyr::replace_na(.x, 0)
    ),
    ppr_week =
      PPR_PASS_YD   * passing_yards   +
      PPR_PASS_TD   * passing_tds     +
      PPR_INT       * interceptions   +
      PPR_RUSH_YD   * rushing_yards   +
      PPR_RUSH_TD   * rushing_tds     +
      PPR_RECEPTION * receptions      +
      PPR_REC_YD    * receiving_yards +
      PPR_REC_TD    * receiving_tds,
    pname = .data[[name_col]]
  )

# Group by player_id only (a traded player keeps one row per week)
players <- weekly %>%
  dplyr::group_by(player_id) %>%
  dplyr::arrange(week, .by_group = TRUE) %>%
  dplyr::summarise(
    pname        = dplyr::last(pname),
    position     = dplyr::last(position),
    games        = dplyr::n(),
    ppr_per_game = sum(ppr_week) / dplyr::n(),
    rush_tds     = sum(rushing_tds),
    .groups      = "drop"
  ) %>%
  dplyr::filter(games >= MIN_GAMES)

rm(weekly_raw, weekly); invisible(gc(verbose = FALSE))

n_players <- nrow(players)
message(sprintf("Qualifying players (>= %d games): %d", MIN_GAMES, n_players))
stopifnot(n_players >= N_BINS * 5L)

# Quantile cut points ----------------------------------------------------------
probs     <- seq(0, 1, length.out = N_BINS + 1L)
breaks_pg <- as.numeric(stats::quantile(players$ppr_per_game, probs = probs, na.rm = TRUE))

# ==============================================================================
# PLOT 1: equal count, unequal width
# ==============================================================================
players <- players %>%
  dplyr::mutate(
    bin = factor(
      cut(ppr_per_game, breaks = breaks_pg, include.lowest = TRUE, labels = FALSE),
      levels = seq_len(N_BINS)
    )
  )

bin_summary <- players %>%
  dplyr::group_by(bin) %>%
  dplyr::summarise(
    n  = dplyr::n(),
    lo = min(ppr_per_game),
    hi = max(ppr_per_game),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    width = hi - lo,
    mid   = (lo + hi) / 2,
    lbl   = paste0("n=", n, "\nspan ", round(width, 1))
  )

p1 <- ggplot(players, aes(x = ppr_per_game)) +
  geom_histogram(aes(fill = bin), binwidth = 1, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = breaks_pg[2:N_BINS], linetype = "dashed",
             color = CUT_LINE, linewidth = 0.5) +
  geom_text(data = bin_summary, aes(x = mid, label = lbl),
            y = Inf, vjust = 1.2, size = 3, color = COL_TEXT, lineheight = 0.9) +
  scale_fill_manual(
    values = BIN_FILLS, name = "Quintile bin",
    labels = c("1 (lowest)", "2", "3", "4", "5 (highest)")
  ) +
  labs(
    title    = "Five quintile bins: equal count, unequal width",
    subtitle = sprintf(
      "Each bin holds about one fifth of the %d qualifying players. The value span of each bin is whatever the data dictates.",
      n_players
    ),
    x = "Fantasy points per game (2025, PPR)",
    y = "Players",
    caption = "Cut points are the 20th, 40th, 60th, 80th percentiles. Illustration on 2025 player scoring."
  ) +
  theme_stats_series()

ggsave(file.path(OUTPUT_DIR, "stats_post6_equal_count.png"),
       p1, width = 10, height = 6.5, dpi = DPI)

# ==============================================================================
# PLOT 2: the cut collision (continuous vs discrete)
# ==============================================================================
breaks_cont <- as.numeric(stats::quantile(players$ppr_per_game, probs = probs, na.rm = TRUE))
breaks_disc <- as.numeric(stats::quantile(players$rush_tds,     probs = probs, na.rm = TRUE))

distinct_cont <- length(unique(round(breaks_cont, 6)))
distinct_disc <- length(unique(round(breaks_disc, 6)))
bins_cont <- distinct_cont - 1L
bins_disc <- distinct_disc - 1L

M_CONT <- "Points per game (continuous)"
M_DISC <- "Rushing TDs (discrete, many ties)"
lvl    <- c(M_CONT, M_DISC)

plot2_df <- dplyr::bind_rows(
  data.frame(metric = M_CONT, value = players$ppr_per_game, stringsAsFactors = FALSE),
  data.frame(metric = M_DISC, value = players$rush_tds,     stringsAsFactors = FALSE)
)
plot2_df$metric <- factor(plot2_df$metric, levels = lvl)

lines_df <- dplyr::bind_rows(
  data.frame(metric = M_CONT, brk = breaks_cont, stringsAsFactors = FALSE),
  data.frame(metric = M_DISC, brk = breaks_disc, stringsAsFactors = FALSE)
)
lines_df$metric <- factor(lines_df$metric, levels = lvl)

label_df <- data.frame(
  metric = factor(lvl, levels = lvl),
  lbl = c(
    sprintf("%d distinct cut points, %d bins", distinct_cont, bins_cont),
    sprintf("%d distinct cut points, %d bins", distinct_disc, bins_disc)
  ),
  stringsAsFactors = FALSE
)

p2 <- ggplot(plot2_df, aes(x = value)) +
  geom_histogram(bins = 25, fill = FILL_HIST, color = "white", linewidth = 0.2) +
  geom_vline(data = lines_df, aes(xintercept = brk), linetype = "dashed",
             color = CUT_LINE, linewidth = 0.5) +
  geom_text(data = label_df, aes(label = lbl), x = Inf, y = Inf,
            hjust = 1.05, vjust = 1.4, size = 3.2, color = COL_TEXT) +
  facet_wrap(~ metric, ncol = 1, scales = "free_x") +
  labs(
    title    = "A quantile is a request the data has to honor",
    subtitle = "Same five cut points asked of both metrics. The continuous metric yields five clean bins; the discrete metric's cut points collide.",
    x = "Metric value",
    y = "Players",
    caption = "When cut points repeat, distinct bins cannot form. The build deduplicates collided edges and drops a metric when too few survive."
  ) +
  theme_stats_series()

ggsave(file.path(OUTPUT_DIR, "stats_post6_cut_collision.png"),
       p2, width = 9, height = 8, dpi = DPI)

# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================
message("\n=== Post 6 visuals summary ===")
message(sprintf("Season: %d | qualifying players: %d (>= %d games)",
                SEASON, n_players, MIN_GAMES))

message("Points-per-game quintile breaks:")
print(round(breaks_pg, 2))
message(sprintf("  distinct breaks: %d  ->  %d bins", distinct_cont, bins_cont))

message("Rushing-TD quintile breaks:")
print(round(breaks_disc, 2))
message(sprintf("  distinct breaks: %d  ->  %d bins (collision on a discrete metric)",
                distinct_disc, bins_disc))

message("Per-bin counts and spans (points per game):")
print(as.data.frame(bin_summary[, c("bin", "n", "lo", "hi", "width")]))

message(sprintf("Saved: %s", file.path(OUTPUT_DIR, "stats_post6_equal_count.png")))
message(sprintf("Saved: %s", file.path(OUTPUT_DIR, "stats_post6_cut_collision.png")))
