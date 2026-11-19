# =============================================================================
# create_stats_post16_visuals.R
#
# Statistics Series, Post 16: Populations vs. Samples (Concept 16, [GAP])
#
# PURPOSE
#   Two panels supporting the post's argument that a minimum-appearance filter
#   defines a population rather than cleaning a sample.
#
#   Panel 1: mean and standard deviation of PPR per game across four nested
#            pools. The mean climbs, the standard deviation stays flat. Noise
#            removal would tighten the spread. It does not tighten.
#
#   Panel 2: mean PPR per game before and after the 8-game filter, by position.
#            One rule, four different-sized shifts.
#
#   Numbers are recomputed from source here rather than hardcoded, so this
#   script and the post cannot silently drift apart.
#
# COLOR NOTE
#   Navy = the filtered pool, red = the full pool. Unlike prior posts in this
#   series, red does NOT mean wrong here. Neither pool is an error. The colors
#   separate two populations, and the subtitle says so explicitly.
#
# HOW TO RUN
#   Rscript create_stats_post16_visuals.R
# =============================================================================

# ---------------------------------------------------------------------------
# CONFIGURATION
# ---------------------------------------------------------------------------
SEASON          <- 2025L
POSITIONS       <- c("QB", "RB", "WR", "TE")
GAME_THRESHOLDS <- c(1L, 4L, 8L, 12L)
HEADLINE_MIN    <- 8L

OUTPUT_DIR   <- "stats_posts/output/plots"
FILE_PREFIX  <- "stats_post16_"
PLOT_DPI     <- 300
PLOT_WIDTH   <- 10
PLOT_HEIGHT  <- 11

COL_FILTERED <- "#1B2A4A"   # navy, the 8+ game pool
COL_FULL     <- "#B22222"   # red, the full pool
COL_NEUTRAL  <- "gray45"

suppressPackageStartupMessages({
  library(nflreadr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ---------------------------------------------------------------------------
# 1. REBUILD THE PLAYER-SEASON TABLE (same logic as the pull script)
# ---------------------------------------------------------------------------
weekly_raw <- tryCatch(
  nflreadr::load_player_stats(seasons = SEASON, summary_level = "week"),
  error = function(e) {
    message("Modern signature failed, falling back. Message: ", conditionMessage(e))
    nflreadr::load_player_stats(seasons = SEASON)
  }
)

needed_cols <- c("player_id", "position", "week", "fantasy_points_ppr")
missing_cols <- setdiff(needed_cols, names(weekly_raw))
if (length(missing_cols) > 0) {
  stop("Missing expected columns: ", paste(missing_cols, collapse = ", "))
}

weekly <- weekly_raw
if ("season_type" %in% names(weekly)) {
  weekly <- dplyr::filter(weekly, season_type == "REG")
} else {
  weekly <- dplyr::filter(weekly, week <= 18L)
}

weekly <- weekly %>%
  dplyr::filter(position %in% POSITIONS, !is.na(fantasy_points_ppr))

if (nrow(weekly) == 0) stop("No rows survived filtering. Check season and columns.")

player_season <- weekly %>%
  dplyr::group_by(player_id) %>%
  dplyr::summarise(
    position  = dplyr::last(position),
    games     = dplyr::n(),
    total_ppr = sum(fantasy_points_ppr, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  dplyr::filter(games > 0) %>%
  dplyr::mutate(ppr_per_game = total_ppr / games)

# ---------------------------------------------------------------------------
# 2. PANEL 1 DATA: mean and SD across the four nested pools
# ---------------------------------------------------------------------------
pool_stats <- lapply(GAME_THRESHOLDS, function(g) {
  pool <- dplyr::filter(player_season, games >= g)
  if (nrow(pool) < 2) return(NULL)          # sd() needs at least 2 values
  data.frame(
    min_games = g,
    players   = nrow(pool),
    Mean      = mean(pool$ppr_per_game),
    SD        = sd(pool$ppr_per_game)
  )
})
pool_stats <- do.call(rbind, pool_stats)
if (is.null(pool_stats) || nrow(pool_stats) == 0) stop("No pools qualified for Panel 1.")

pool_long <- pool_stats %>%
  tidyr::pivot_longer(
    cols      = c(Mean, SD),
    names_to  = "measure",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    measure = factor(
      measure,
      levels = c("Mean", "SD"),
      labels = c("Average PPR per game", "Standard deviation (spread)")
    )
  )

# Direct labels at the right-hand end of each line, so no legend is needed.
label_pts <- pool_long %>%
  dplyr::filter(min_games == max(GAME_THRESHOLDS))

mean_lo <- pool_stats$Mean[pool_stats$min_games == min(GAME_THRESHOLDS)]
mean_hi <- pool_stats$Mean[pool_stats$min_games == max(GAME_THRESHOLDS)]
sd_rng  <- range(pool_stats$SD)

p1 <- ggplot(pool_long, aes(x = min_games, y = value,
                            color = measure, group = measure)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3.2) +
  geom_text(
    data = label_pts,
    aes(label = sprintf("%.2f", value)),
    hjust = -0.35, vjust = 0.4, size = 4, fontface = "bold",
    show.legend = FALSE
  ) +
  geom_text(
    data = label_pts,
    aes(label = as.character(measure)),
    hjust = -0.18, vjust = 2.1, size = 3.6,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c(
    "Average PPR per game"        = COL_FILTERED,
    "Standard deviation (spread)" = COL_FULL
  )) +
  scale_x_continuous(breaks = GAME_THRESHOLDS) +
  # coord_cartesian, not scale limits: expands the panel without dropping data
  coord_cartesian(
    xlim = c(min(GAME_THRESHOLDS), max(GAME_THRESHOLDS) + 5),
    ylim = c(0, max(pool_long$value) * 1.15)
  ) +
  labs(
    title    = sprintf("The average climbed %.2f points. The spread moved %.2f.",
                       mean_hi - mean_lo, diff(sd_rng)),
    subtitle = paste0(
      "Raising the minimum-games filter shifts the center of the pool but not its spread.\n",
      "Removing noise from one population would tighten the spread. It did not tighten."
    ),
    x        = "Minimum games required to be in the pool",
    y        = "PPR per game",
    caption  = sprintf("%s NFL regular season | QB, RB, WR, TE | source: nflreadr",
                       SEASON)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position   = "none",
    plot.title        = element_text(face = "bold", size = 16),
    plot.subtitle     = element_text(color = COL_NEUTRAL, size = 11,
                                     margin = margin(b = 12)),
    plot.caption      = element_text(color = COL_NEUTRAL, size = 9),
    panel.grid.minor  = element_blank()
  )

# ---------------------------------------------------------------------------
# 3. PANEL 2 DATA: per-position shift under the headline filter
# ---------------------------------------------------------------------------
full_pos <- player_season %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(n_full = dplyr::n(),
                   mean_full = mean(ppr_per_game), .groups = "drop")

filt_pos <- player_season %>%
  dplyr::filter(games >= HEADLINE_MIN) %>%
  dplyr::group_by(position) %>%
  dplyr::summarise(n_filt = dplyr::n(),
                   mean_filt = mean(ppr_per_game), .groups = "drop")

pos_shift <- full_pos %>%
  dplyr::inner_join(filt_pos, by = "position") %>%
  dplyr::mutate(
    shift       = mean_filt - mean_full,
    pct_dropped = 100 * (n_full - n_filt) / n_full,
    # ggplot draws the y axis bottom to top, so sort ascending to put the
    # largest shift at the top of the panel.
    position    = factor(position, levels = position[order(shift)])
  )

# Every position present in the data must appear as a row. Fail loudly if not.
if (nrow(pos_shift) != length(unique(player_season$position))) {
  warning("A position present in the data is missing from Panel 2. Check the join.")
}

p2 <- ggplot(pos_shift, aes(y = position)) +
  geom_segment(aes(x = mean_full, xend = mean_filt, yend = position),
               color = COL_NEUTRAL, linewidth = 1.1) +
  geom_point(aes(x = mean_full), color = COL_FULL, size = 4.5) +
  geom_point(aes(x = mean_filt), color = COL_FILTERED, size = 4.5) +
  geom_text(aes(x = mean_filt, label = sprintf("+%.2f", shift)),
            hjust = -0.45, size = 4, fontface = "bold", color = COL_FILTERED) +
  geom_text(aes(x = mean_full,
                label = sprintf("%.0f%% dropped", pct_dropped)),
            hjust = 1.15, size = 3.4, color = COL_NEUTRAL) +
  coord_cartesian(xlim = c(0, max(pos_shift$mean_filt) * 1.25)) +
  labs(
    title    = sprintf("One filter, four different groups: QB moved %.2f, WR moved %.2f.",
                       pos_shift$shift[pos_shift$position == "QB"],
                       pos_shift$shift[pos_shift$position == "WR"]),
    subtitle = paste0(
      "Red = every player with a recorded game. Navy = players with ",
      HEADLINE_MIN, " or more games.\n",
      "Neither pool is wrong. They are different populations."
    ),
    x        = "Average PPR per game",
    y        = NULL,
    caption  = sprintf("%s NFL regular season | source: nflreadr", SEASON)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(color = COL_NEUTRAL, size = 11,
                                    margin = margin(b = 12)),
    plot.caption     = element_text(color = COL_NEUTRAL, size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y      = element_text(face = "bold", size = 13)
  )

# ---------------------------------------------------------------------------
# 4. COMBINE AND SAVE (patchwork with graceful degradation)
# ---------------------------------------------------------------------------
combined_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "populations_vs_samples.png"))
p1_path       <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "panel1_mean_vs_spread.png"))
p2_path       <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "panel2_position_shift.png"))

if (requireNamespace("patchwork", quietly = TRUE)) {
  combined <- patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(1, 1))
  ggsave(combined_path, combined,
         width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
  cat("Saved combined figure:", combined_path, "\n")
} else {
  message("patchwork not installed. Saving panels separately.")
  ggsave(p1_path, p1, width = PLOT_WIDTH, height = PLOT_HEIGHT / 2, dpi = PLOT_DPI)
  ggsave(p2_path, p2, width = PLOT_WIDTH, height = PLOT_HEIGHT / 2, dpi = PLOT_DPI)
  cat("Saved panel 1:", p1_path, "\n")
  cat("Saved panel 2:", p2_path, "\n")
}

# ---------------------------------------------------------------------------
# CONSOLE SUMMARY
# ---------------------------------------------------------------------------
cat("\n=========================================================\n")
cat("Post 16 visuals\n")
cat("=========================================================\n")
cat("Season:", SEASON, "| players in full pool:", nrow(player_season), "\n")
cat(sprintf("Mean across pools: %.2f -> %.2f (change %.2f)\n",
            mean_lo, mean_hi, mean_hi - mean_lo))
cat(sprintf("SD across pools:   %.2f to %.2f (range %.2f)\n",
            sd_rng[1], sd_rng[2], diff(sd_rng)))
cat("Per-position shift under the", HEADLINE_MIN, "game filter:\n")
print(as.data.frame(
  pos_shift %>%
    dplyr::mutate(dplyr::across(c(mean_full, mean_filt, shift, pct_dropped),
                                ~ round(.x, 2))) %>%
    dplyr::select(position, n_full, n_filt, mean_full, mean_filt, shift, pct_dropped)
), row.names = FALSE)
cat("DPI:", PLOT_DPI, "| output:", OUTPUT_DIR, "\n")
cat("=========================================================\n\n")
