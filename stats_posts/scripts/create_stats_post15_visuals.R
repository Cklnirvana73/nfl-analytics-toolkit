# ==============================================================================
# Statistics Series: Post 15 visuals
# Log-Normal and Heavy-Tailed Distributions (Concept 15)
#
# Two stacked panels, built from the same 2025 weekly PPR the pull script used.
#
#   Panel 1: the real right-skewed weekly PPR distribution (navy) against the
#            symmetric normal the R/36 recovery assumes (red). The red curve
#            falls short of the real ceiling and spills probability below zero,
#            onto scores that cannot happen. Both misses annotated.
#
#   Panel 2: the ceiling miss by position. Quarterback is near zero (the
#            assumption is free); receivers, running backs, and tight ends
#            carry a real gap. This is the "free here, expensive there" beat.
#
# Color convention (locked series standard): navy = actual/observed,
# red = the symmetric-normal assumption. Color is always paired with a label
# or annotation so the chart does not depend on color alone.
#
# Honesty note carried into the caption: weekly PPR floors near zero, so it is
# right-skewed and heavy-tailed but not a clean log-normal.
#
# HOW TO RUN
#   source("stats_posts/scripts/create_stats_post15_visuals.R")
# Requires internet (nflreadr). Writes PNGs to stats_posts/output/plots/ at 300 DPI.
# ==============================================================================

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------
SEASON     <- 2025L
MIN_GAMES  <- 8L
POSITIONS  <- c("QB", "RB", "WR", "TE")
Z80        <- stats::qnorm(0.9)               # ~1.2816, the R/36 constant

NAVY       <- "#12385C"                       # actual / observed
RED        <- "#C1272D"                       # the assumption

OUT_DIR    <- file.path("stats_posts", "output", "plots")
FILE_MAIN  <- file.path(OUT_DIR, "stats_post15_normal_assumption_cost.png")
FILE_P1    <- file.path(OUT_DIR, "stats_post15_distribution_miss.png")
FILE_P2    <- file.path(OUT_DIR, "stats_post15_position_gap.png")

PLOT_W     <- 8
PLOT_H     <- 9.5
PLOT_DPI   <- 300
BINWIDTH   <- 2

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------
library(nflreadr)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------------------
# THEME (consistent, publication-ready)
# ------------------------------------------------------------------------------
theme_analytics <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = rel(1.15), hjust = 0),
      plot.subtitle   = element_text(color = "gray40", size = rel(0.85), hjust = 0),
      plot.caption    = element_text(color = "gray55", size = rel(0.7), hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title      = element_text(color = "gray30"),
      axis.text       = element_text(color = "gray40"),
      legend.position = "none",
      plot.margin     = margin(12, 16, 12, 12)
    )
}

# ------------------------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------------------------
recover_sigma <- function(lower_80, upper_80) (upper_80 - lower_80) / (2 * Z80)

# Pooled/group scalars needed for the plots.
group_stats <- function(x) {
  x <- x[is.finite(x)]
  q <- stats::quantile(x, probs = c(0.10, 0.50, 0.90), names = FALSE, type = 7)
  mu    <- mean(x)
  sig   <- recover_sigma(q[1], q[3])
  list(
    n           = length(x),
    mean        = mu,
    p10         = q[1],
    p90         = q[3],
    sigma_sym   = sig,
    sym_ceiling = mu + Z80 * sig,
    sym_floor   = mu - Z80 * sig,
    ceiling_gap = q[3] - (mu + Z80 * sig)
  )
}

# ------------------------------------------------------------------------------
# LOAD + FILTER (self-contained; nflreadr signature tolerant)
# ------------------------------------------------------------------------------
load_weekly <- function(season) {
  out <- tryCatch(nflreadr::load_player_stats(seasons = season, stat_type = "offense"),
                  error = function(e) NULL)
  if (is.null(out)) {
    out <- tryCatch(nflreadr::load_player_stats(seasons = season),
                    error = function(e) NULL)
  }
  if (is.null(out)) stop("Could not load weekly player stats. Check nflreadr.")
  out
}

raw <- load_weekly(SEASON)
nm  <- names(raw)

id_col   <- if ("player_id" %in% nm) "player_id" else "gsis_id"
pos_col  <- if ("position" %in% nm) "position" else "position_group"
ppr_col  <- "fantasy_points_ppr"
if (!ppr_col %in% nm) stop("fantasy_points_ppr not found. Columns:\n",
                           paste(nm, collapse = ", "))

work <- data.frame(
  player_id   = raw[[id_col]],
  position    = raw[[pos_col]],
  week        = raw[["week"]],
  ppr         = raw[[ppr_col]],
  season_type = if ("season_type" %in% nm) raw[["season_type"]] else NA_character_,
  stringsAsFactors = FALSE
)

work <- work[is.finite(work$ppr), ]
if (any(!is.na(work$season_type))) {
  work <- work[work$season_type == "REG", ]
} else {
  work <- work[work$week <= 18L, ]
}
work <- work[work$position %in% POSITIONS, ]

games    <- aggregate(week ~ player_id, data = work,
                      FUN = function(w) length(unique(w)))
names(games)[2] <- "games"
keep_ids <- games$player_id[games$games >= MIN_GAMES]
weekly   <- work[work$player_id %in% keep_ids, ]

stopifnot(
  "No weekly rows after filtering" = nrow(weekly) >= 2,
  "All positions dropped" = length(unique(weekly$position)) >= 2
)

# ------------------------------------------------------------------------------
# DERIVE (same method as the pull script)
# ------------------------------------------------------------------------------
pooled <- group_stats(weekly$ppr)

pos_rows <- lapply(POSITIONS, function(p) {
  g <- group_stats(weekly$ppr[weekly$position == p])
  data.frame(position = p, ceiling_gap = g$ceiling_gap)
})
pos_df <- do.call(rbind, pos_rows)
# Order smallest cost to largest so QB reads first.
pos_df <- pos_df[order(pos_df$ceiling_gap), ]
pos_df$position <- factor(pos_df$position, levels = pos_df$position)
pos_df$label_y  <- pos_df$ceiling_gap + ifelse(pos_df$ceiling_gap >= 0, 0.10, -0.10)

# ------------------------------------------------------------------------------
# PANEL 1: real distribution vs the symmetric normal the recovery assumes
# ------------------------------------------------------------------------------
xmin <- -8; xmax <- 40
dens_max <- max(stats::density(weekly$ppr)$y)

neg_grid <- data.frame(x = seq(xmin, 0, length.out = 120))
neg_grid$y <- stats::dnorm(neg_grid$x, mean = pooled$mean, sd = pooled$sigma_sym)

p1 <- ggplot(weekly, aes(x = ppr)) +
  # draw order: histogram, then impossible-region shade, then the assumed curve
  geom_histogram(aes(y = after_stat(density)), binwidth = BINWIDTH, boundary = 0,
                 fill = NAVY, color = "white", alpha = 0.9) +
  geom_area(data = neg_grid, aes(x = x, y = y),
            fill = RED, alpha = 0.30, inherit.aes = FALSE) +
  stat_function(fun = stats::dnorm,
                args = list(mean = pooled$mean, sd = pooled$sigma_sym),
                color = RED, linewidth = 1.1) +
  geom_vline(xintercept = pooled$p90, color = NAVY,
             linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = pooled$sym_ceiling, color = RED,
             linetype = "dashed", linewidth = 0.7) +
  annotate("text", x = pooled$p90, y = dens_max * 0.98,
           label = sprintf("real ceiling %.1f", pooled$p90),
           color = NAVY, hjust = -0.05, vjust = 1, size = 3.3, fontface = "bold") +
  annotate("text", x = pooled$sym_ceiling, y = dens_max * 0.86,
           label = sprintf("assumed %.1f", pooled$sym_ceiling),
           color = RED, hjust = 1.05, vjust = 1, size = 3.3, fontface = "bold") +
  annotate("text", x = -4, y = dens_max * 0.40,
           label = "the assumed curve puts\nprobability below 0,\non weeks that cannot happen",
           color = RED, hjust = 0.5, vjust = 1, size = 3.0, lineheight = 0.95) +
  coord_cartesian(xlim = c(xmin, xmax)) +
  labs(
    title    = "A symmetric normal misses on both ends",
    subtitle = sprintf("2025 weekly PPR, %s skill-position player-weeks. Navy = actual, red = the symmetric normal the recovery assumes",
                       format(pooled$n, big.mark = ",")),
    x = "Weekly PPR points", y = "Density"
  ) +
  theme_analytics()

# ------------------------------------------------------------------------------
# PANEL 2: the ceiling miss by position
# ------------------------------------------------------------------------------
p2 <- ggplot(pos_df, aes(x = position, y = ceiling_gap)) +
  geom_hline(yintercept = 0, color = "gray55", linewidth = 0.6) +
  geom_col(fill = NAVY, width = 0.62) +
  geom_text(aes(y = label_y, label = sprintf("%+.1f", ceiling_gap)),
            color = NAVY, fontface = "bold", size = 3.6, vjust = 0.5) +
  labs(
    title    = "The miss lands on pass catchers, not quarterbacks",
    subtitle = "Points the symmetric ceiling falls short of the true 90th-percentile week, 2025",
    x = NULL, y = "Ceiling understated (pts)"
  ) +
  theme_analytics() +
  theme(panel.grid.major.y = element_line(color = "gray92"))

# ------------------------------------------------------------------------------
# COMBINE + SAVE (patchwork if available, else separate PNGs)
# ------------------------------------------------------------------------------
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

cap <- "Data: nflreadr weekly player stats, 2025 regular season, 8+ games. Weekly PPR floors near 0: right-skewed and heavy-tailed, not a clean log-normal."

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  combined <- (p1 / p2) +
    patchwork::plot_layout(heights = c(1.15, 1)) +
    patchwork::plot_annotation(
      title   = "What the normal assumption costs weekly fantasy scoring",
      caption = cap,
      theme   = theme(
        plot.title   = element_text(face = "bold", size = 15),
        plot.caption = element_text(color = "gray55", size = 8, hjust = 0)
      )
    )
  print(combined)
  ggsave(FILE_MAIN, plot = combined, width = PLOT_W, height = PLOT_H, dpi = PLOT_DPI)
  saved <- FILE_MAIN
} else {
  message("patchwork not available; saving two separate PNGs.")
  p1c <- p1 + labs(caption = cap)
  print(p1c); ggsave(FILE_P1, plot = p1c, width = PLOT_W, height = PLOT_H / 2 + 0.5,
                     dpi = PLOT_DPI)
  print(p2);  ggsave(FILE_P2, plot = p2,  width = PLOT_W, height = PLOT_H / 2 - 0.5,
                     dpi = PLOT_DPI)
  saved <- paste(FILE_P1, FILE_P2, sep = ", ")
}

# ------------------------------------------------------------------------------
# CONSOLE SUMMARY
# ------------------------------------------------------------------------------
line <- function() cat(strrep("=", 74), "\n")
cat("\n"); line()
cat("POST 15 VISUALS\n"); line()
cat(sprintf("Player-weeks plotted: %s\n", format(pooled$n, big.mark = ",")))
cat(sprintf("Pooled mean %.2f | recovered sigma_sym %.2f\n",
            pooled$mean, pooled$sigma_sym))
cat(sprintf("Real ceiling P90 %.2f vs assumed %.2f  (miss %.2f)\n",
            pooled$p90, pooled$sym_ceiling, pooled$ceiling_gap))
cat(sprintf("Assumed floor %.2f vs real floor %.2f\n",
            pooled$sym_floor, pooled$p10))
cat("Ceiling miss by position (smallest to largest):\n")
for (i in seq_len(nrow(pos_df))) {
  cat(sprintf("  %-3s %+.2f\n",
              as.character(pos_df$position[i]), pos_df$ceiling_gap[i]))
}
cat(sprintf("Saved: %s\n", saved))
line(); cat("\n")
