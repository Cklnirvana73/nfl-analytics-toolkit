# ==============================================================================
# Statistics Series | Post 4: Skewness
# Visuals for LinkedIn post
#
# Outputs (300 DPI):
#   stats_posts/output/plots/stats_post4_positions.png
#   stats_posts/output/plots/stats_post4_shapes.png
#
# Requires: nflfastR, ggplot2, dplyr, tidyr, tibble
# Run from project root. Internet access required for nflfastR on first pull.
# ==============================================================================

# CONFIGURATION ----------------------------------------------------------------
POST_NUM   <- 4L
CONCEPT    <- "Skewness"
SEASON     <- 2025L
MIN_GAMES  <- 8L
SKILL_POS  <- c("QB", "RB", "WR", "TE")
OUTPUT_DIR <- "stats_posts/output/plots"
DPI        <- 300L
WIDTH_IN   <- 10
HEIGHT_IN  <- 8

# Colors -----------------------------------------------------------------------
FILL_DENSITY <- "#DCE8F5"
LINE_MEAN    <- "#C0392B"    # red   -- solid, mean
LINE_MEDIAN  <- "#1A5276"    # navy  -- dashed, median
COL_TEXT     <- "#1C1C1A"
COL_SUBTEXT  <- "#5F5E5A"
COL_BG       <- "#FAFAF8"
COL_PANEL    <- "#FFFFFF"
COL_GRID     <- "#E8E6DF"
COL_STRIP    <- "#ECEAE3"

# LIBRARIES --------------------------------------------------------------------
library(nflfastR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# SHARED THEME -----------------------------------------------------------------
theme_stats_series <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = COL_BG,    color = NA),
      panel.background = element_rect(fill = COL_PANEL, color = NA),
      panel.grid.major = element_line(color = COL_GRID,  linewidth = 0.4),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = COL_STRIP,  color = NA),
      strip.text       = element_text(face = "bold", size = 11, color = COL_TEXT),
      plot.title       = element_text(face = "bold", size = 14, color = COL_TEXT,
                                      margin = margin(b = 6)),
      plot.subtitle    = element_text(size = 10, color = COL_SUBTEXT,
                                      margin = margin(b = 10)),
      plot.caption     = element_text(size = 9,  color = COL_SUBTEXT,
                                      margin = margin(t = 8)),
      axis.title       = element_text(size = 10, color = COL_SUBTEXT),
      axis.text        = element_text(size = 9,  color = COL_SUBTEXT),
      plot.margin      = margin(16, 20, 12, 16)
    )
}

# SKEWNESS FUNCTION (Fisher's moment coefficient) ------------------------------
compute_skewness <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- sd(x)
  (n / ((n - 1) * (n - 2))) * sum(((x - m) / s)^3)
}

# ==============================================================================
# DATA: 2025 NFL Player Stats
# ==============================================================================
cat("Loading 2025 player stats...\n")
raw <- nflfastR::load_player_stats(seasons = SEASON)

player_ppr <- raw %>%
  filter(season_type == "REG", position %in% SKILL_POS) %>%
  group_by(player_id, player_name, position) %>%
  summarise(
    games        = n(),
    ppr_per_game = mean(fantasy_points_ppr, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  filter(games >= MIN_GAMES) %>%
  mutate(position = factor(position, levels = c("QB", "RB", "WR", "TE")))

# Position-level stats for vlines and annotation
pos_stats <- player_ppr %>%
  group_by(position) %>%
  summarise(
    n        = n(),
    mean_val = mean(ppr_per_game),
    med_val  = median(ppr_per_game),
    gap      = round(mean(ppr_per_game) - median(ppr_per_game), 2),
    skewness = round(compute_skewness(ppr_per_game), 3),
    x95      = quantile(ppr_per_game, 0.95),
    .groups  = "drop"
  ) %>%
  mutate(position = factor(position, levels = c("QB", "RB", "WR", "TE")))

cat("Position stats:\n")
print(pos_stats)

# Annotation labels: one row per position, placed near top-right of each panel
annot_pos <- pos_stats %>%
  mutate(
    label = paste0(
      "n = ", n, "  |  skewness = ", sprintf("%.3f", skewness),
      "\nmean = ",    sprintf("%.2f", mean_val),
      "   median = ", sprintf("%.2f", med_val),
      "\ngap = ",     sprintf("%+.2f", gap)
    )
  )

# ==============================================================================
# VISUAL 1: PPR Per Game Density by Position
# ==============================================================================
p_positions <- ggplot(player_ppr, aes(x = ppr_per_game)) +
  geom_density(
    fill = FILL_DENSITY, color = "#89A9C8",
    alpha = 0.85, linewidth = 0.6
  ) +
  geom_vline(
    data = pos_stats, aes(xintercept = mean_val),
    color = LINE_MEAN, linewidth = 0.9, linetype = "solid"
  ) +
  geom_vline(
    data = pos_stats, aes(xintercept = med_val),
    color = LINE_MEDIAN, linewidth = 0.9, linetype = "dashed"
  ) +
  geom_text(
    data  = annot_pos,
    aes(x = x95, y = Inf, label = label),
    hjust = 1, vjust = 1.4,
    size  = 3.0, color = COL_SUBTEXT
  ) +
  facet_wrap(~ position, scales = "free", ncol = 2) +
  labs(
    title    = "PPR Points Per Game by Position | 2025 NFL Regular Season",
    subtitle = "Red solid = mean  |  Blue dashed = median  |  8+ game minimum",
    x        = "PPR Points Per Game",
    y        = "Density",
    caption  = "Source: nflfastR 2025"
  ) +
  theme_stats_series()

out_pos <- file.path(OUTPUT_DIR,
                     paste0("stats_post", POST_NUM, "_positions.png"))
ggsave(out_pos, p_positions,
       width = WIDTH_IN, height = HEIGHT_IN, dpi = DPI, bg = COL_BG)
cat(sprintf("Saved: %s\n", out_pos))

# ==============================================================================
# DATA: Synthetic Distributions
# ==============================================================================
set.seed(42)
shapes_list <- list(
  "Normal"       = rnorm(1000, mean = 10, sd = 3),
  "Right-Skewed" = rgamma(1000, shape = 2, scale = 3),
  "Left-Skewed"  = 20 - rgamma(1000, shape = 2, scale = 3),
  "Bimodal"      = c(rnorm(500, 5, 1.5), rnorm(500, 15, 1.5))
)

shapes_long <- bind_rows(
  lapply(names(shapes_list), function(nm) {
    tibble(shape = nm, value = shapes_list[[nm]])
  })
) %>%
  mutate(shape = factor(shape,
                        levels = c("Normal", "Right-Skewed",
                                   "Left-Skewed", "Bimodal")))

shape_stats <- shapes_long %>%
  group_by(shape) %>%
  summarise(
    n        = n(),
    mean_val = mean(value),
    med_val  = median(value),
    gap      = round(mean(value) - median(value), 3),
    skewness = round(compute_skewness(value), 3),
    x95      = quantile(value, 0.95),
    .groups  = "drop"
  )

cat("\nSynthetic shape stats:\n")
print(shape_stats)

annot_shapes <- shape_stats %>%
  mutate(
    label = paste0(
      "skewness = ", sprintf("%.3f", skewness),
      "\nmean = ",    sprintf("%.3f", mean_val),
      "   median = ", sprintf("%.3f", med_val),
      "\ngap = ",     sprintf("%+.3f", gap)
    )
  )

# ==============================================================================
# VISUAL 2: Synthetic Shape Reference
# ==============================================================================
p_shapes <- ggplot(shapes_long, aes(x = value)) +
  geom_density(
    fill = FILL_DENSITY, color = "#89A9C8",
    alpha = 0.85, linewidth = 0.6
  ) +
  geom_vline(
    data = shape_stats, aes(xintercept = mean_val),
    color = LINE_MEAN, linewidth = 0.9, linetype = "solid"
  ) +
  geom_vline(
    data = shape_stats, aes(xintercept = med_val),
    color = LINE_MEDIAN, linewidth = 0.9, linetype = "dashed"
  ) +
  geom_text(
    data  = annot_shapes,
    aes(x = x95, y = Inf, label = label),
    hjust = 1, vjust = 1.4,
    size  = 3.0, color = COL_SUBTEXT
  ) +
  facet_wrap(~ shape, scales = "free", ncol = 2) +
  labs(
    title    = "Skewness: Four Distribution Shapes",
    subtitle = "Red solid = mean  |  Blue dashed = median  |  n = 1,000 per shape",
    x        = "Value",
    y        = "Density",
    caption  = "Synthetic distributions | set.seed(42)"
  ) +
  theme_stats_series()

out_shapes <- file.path(OUTPUT_DIR,
                        paste0("stats_post", POST_NUM, "_shapes.png"))
ggsave(out_shapes, p_shapes,
       width = WIDTH_IN, height = HEIGHT_IN, dpi = DPI, bg = COL_BG)
cat(sprintf("Saved: %s\n", out_shapes))

# CONSOLE SUMMARY --------------------------------------------------------------
cat("\n=== POST 4 VISUAL BUILD COMPLETE ===\n")
cat(sprintf("Post:    %d | %s\n", POST_NUM, CONCEPT))
cat(sprintf("Season:  %d | Min games: %d\n", SEASON, MIN_GAMES))
cat(sprintf("Players: %d across %s\n",
            nrow(player_ppr),
            paste(levels(player_ppr$position), collapse = "/")))
cat(sprintf("Output:  %s\n", OUTPUT_DIR))
cat(sprintf("  %s\n", out_pos))
cat(sprintf("  %s\n", out_shapes))
cat(sprintf("DPI: %d | %g x %g in\n", DPI, WIDTH_IN, HEIGHT_IN))
