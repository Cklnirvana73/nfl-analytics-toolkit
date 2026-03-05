# ==============================================================================
# WEEK 7 VISUALIZATIONS: Game Script & Leverage Features
# create_week7_visual.R
# ==============================================================================
#
# Generates 3 publication-quality plots:
#   Plot 1: Script Share Matrix -- position group breakdown of leading/neutral/
#           trailing script exposure for top skill players. Reveals which
#           players' production is most and least context-dependent.
#
#   Plot 2: Raw vs Script-Adjusted EPA -- scatter plot comparing unadjusted
#           EPA/play to script-adjusted EPA/play, with adjustment size encoded
#           as point color. Players furthest from the diagonal are most
#           affected by game script.
#
#   Plot 3: WPA Leaders with Clutch Rate -- horizontal lollipop showing top
#           players by total WPA with clutch_rate encoded as color and a
#           sample size warning for small-sample clutch rates.
#
# Output: output/plots/week7_*.png (300 dpi)
# Data:   nflfastR | Analysis: NFL Analytics Toolkit
#
# Usage:
#   source(here::here("examples", "create_week7_visual.R"))
# ==============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(here)
library(glue)

source(here::here("R", "09_gamescript_features.R"))

# ==============================================================================
# DATA GENERATION (synthetic -- replace with real pbp in production)
# ==============================================================================

set.seed(2024)
n_plays <- 3000L

make_visual_pbp <- function(n) {
  players <- tibble(
    passer_id   = c("QB01","QB02","QB03","QB04","QB05"),
    passer_name = c("Lamar Jackson","Jalen Hurts","Josh Allen","Joe Burrow","Brock Purdy"),
    rusher_id   = c("RB01","RB02","RB03","RB04","RB05"),
    rusher_name = c("Christian McCaffrey","Derrick Henry","Breece Hall","Josh Jacobs","Saquon Barkley"),
    recv_id     = c("WR01","WR02","WR03","WR04","WR05"),
    recv_name   = c("Tyreek Hill","Justin Jefferson","CeeDee Lamb","Davante Adams","Amon-Ra St. Brown")
  )

  # Simulate with varied game script profiles per team
  # Some teams lead a lot (high leading_share), some trail a lot
  team_scripts <- tibble(
    posteam   = c("BAL","PHI","BUF","CIN","SF","LAC","PIT","NYJ","NE","TEN"),
    mean_wp   = c(0.65, 0.62, 0.58, 0.50, 0.63, 0.48, 0.45, 0.43, 0.40, 0.38)
  )

  plays <- tibble(
    season = 2024L,
    week   = sample(1:17, n, replace = TRUE)
  ) %>%
    mutate(
      game_id   = paste0("2024_", sprintf("%02d", week), "_",
                         sample(team_scripts$posteam, n, replace = TRUE),
                         "_OPP"),
      posteam   = sub(".*_(.+)_OPP", "\\1", game_id),
      defteam   = "OPP",
      qtr       = sample(1:4, n, replace = TRUE, prob = c(0.25,0.25,0.25,0.25))
    ) %>%
    left_join(team_scripts, by = "posteam") %>%
    mutate(
      wp              = pmin(pmax(rnorm(n, mean_wp, 0.15), 0.05), 0.95),
      play_type       = sample(c("pass","run"), n, replace = TRUE, prob = c(0.6,0.4)),
      epa             = rnorm(n, 0.04, 0.45),
      wpa             = rnorm(n, 0.0, 0.025) + (wp - 0.5) * 0.02,
      score_differential = as.integer(round((wp - 0.5) * 30)),
      qb_kneel        = 0L, qb_spike = 0L, qb_scramble = 0L,
      # Assign players based on play type
      passer_player_id   = ifelse(play_type == "pass",
                                  players$passer_id[match(posteam,
                                    c("BAL","PHI","BUF","CIN","SF","LAC","PIT","NYJ","NE","TEN"))],
                                  NA_character_),
      passer_player_name = ifelse(!is.na(passer_player_id),
                                  players$passer_name[match(passer_player_id, players$passer_id)],
                                  NA_character_),
      rusher_player_id   = ifelse(play_type == "run",
                                  players$rusher_id[match(posteam,
                                    c("BAL","PHI","BUF","CIN","SF","LAC","PIT","NYJ","NE","TEN"))],
                                  NA_character_),
      rusher_player_name = ifelse(!is.na(rusher_player_id),
                                  players$rusher_name[match(rusher_player_id, players$rusher_id)],
                                  NA_character_),
      receiver_player_id   = ifelse(play_type == "pass",
                                    players$recv_id[match(posteam,
                                      c("BAL","PHI","BUF","CIN","SF","LAC","PIT","NYJ","NE","TEN"))],
                                    NA_character_),
      receiver_player_name = ifelse(!is.na(receiver_player_id),
                                    players$recv_name[match(receiver_player_id, players$recv_id)],
                                    NA_character_)
    )
  plays
}

pbp_visual <- make_visual_pbp(n_plays)

# Run all four functions
cat("Computing game script splits...\n")
gs  <- get_game_script_splits(pbp_visual, min_plays = 30)

cat("Computing script-adjusted EPA...\n")
adj <- calculate_script_adjusted_epa(pbp_visual, min_plays = 30)

cat("Computing leverage features...\n")
lev <- calculate_leverage_features(pbp_visual, min_plays = 30)

# Console summary
cat("\n--- Console Summary ---\n")
cat(glue("Game script splits: {nrow(gs)} player-position rows\n"))
cat(glue("Script-adjusted EPA: {nrow(adj)} player-position rows\n"))
cat(glue("Leverage features: {nrow(lev)} player-position rows\n\n"))

# League averages for reference lines
avg_epa       <- mean(adj$raw_epa_per_play, na.rm = TRUE)
avg_adj_epa   <- mean(adj$script_adjusted_epa, na.rm = TRUE)
avg_wpa       <- mean(lev$total_wpa, na.rm = TRUE)

cat(glue("League avg raw EPA/play:      {round(avg_epa, 4)}\n"))
cat(glue("League avg adjusted EPA/play: {round(avg_adj_epa, 4)}\n"))
cat(glue("League avg total WPA:         {round(avg_wpa, 4)}\n\n"))

# Script share breakdown by position
gs %>%
  group_by(position_group) %>%
  summarise(
    mean_neutral_share  = mean(neutral_share,  na.rm = TRUE),
    mean_leading_share  = mean(leading_share,  na.rm = TRUE),
    mean_trailing_share = mean(trailing_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Output directory
dir.create(here::here("output", "plots"), recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# PLOT 1: SCRIPT SHARE MATRIX (stacked bar)
# ==============================================================================
# Shows leading/neutral/trailing share per player, faceted by position group.
# Reveals which players' usage is most context-dependent.

gs_long <- gs %>%
  filter(total_plays >= 50) %>%
  select(player_name, position_group, team,
         leading_share, neutral_share, trailing_share) %>%
  pivot_longer(
    cols      = c(leading_share, neutral_share, trailing_share),
    names_to  = "script",
    values_to = "share"
  ) %>%
  mutate(
    script = factor(
      gsub("_share", "", script),
      levels = c("trailing", "neutral", "leading")
    )
  )

# Order players by leading_share within each position group
player_order <- gs %>%
  filter(total_plays >= 50) %>%
  arrange(position_group, desc(leading_share)) %>%
  pull(player_name)

gs_long <- gs_long %>%
  mutate(player_name = factor(player_name, levels = unique(player_order)))

script_colors <- c(
  "leading"  = "#2166AC",
  "neutral"  = "#878787",
  "trailing" = "#D6604D"
)

p1 <- ggplot(gs_long, aes(x = player_name, y = share, fill = script)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~ position_group, scales = "free_x", nrow = 1) +
  scale_fill_manual(
    values = script_colors,
    labels = c("Leading (WP > 60%)", "Neutral (40-60%)", "Trailing (WP < 40%)"),
    name   = "Game Script"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.01)) +
  geom_hline(yintercept = 0.60, linetype = "dashed", color = "white", linewidth = 0.4) +
  labs(
    title    = "Game Script Share by Player and Position",
    subtitle = "Blue = leading opportunities | Red = trailing (garbage time excluded)",
    x        = NULL,
    y        = "Share of Total Plays",
    caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nGarbage time excluded (WP < 10% or > 90% in Q4)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x     = element_text(angle = 35, hjust = 1, size = 8),
    strip.text      = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 9, color = "grey40"),
    plot.caption    = element_text(size = 7, color = "grey50"),
    panel.grid.minor = element_blank()
  )

ggsave(
  here::here("output", "plots", "week7_script_share_matrix.png"),
  plot   = p1,
  width  = 12,
  height = 6,
  dpi    = 300
)
cat("Saved: week7_script_share_matrix.png\n")


# ==============================================================================
# PLOT 2: RAW vs SCRIPT-ADJUSTED EPA
# ==============================================================================
# Scatter plot: x = raw EPA/play, y = script-adjusted EPA/play.
# Color = adjustment_applied (positive = adjusted up, negative = adjusted down).
# Diagonal = no adjustment (equal raw and adjusted).
# Players below diagonal played in favorable scripts; above = unfavorable.

adj_plot <- adj %>%
  filter(total_plays >= 40, !is.na(raw_epa_per_play), !is.na(script_adjusted_epa))

epa_range <- range(c(adj_plot$raw_epa_per_play, adj_plot$script_adjusted_epa), na.rm = TRUE)
epa_range <- c(epa_range[1] - 0.02, epa_range[2] + 0.02)

adj_limit <- max(abs(adj_plot$adjustment_applied), na.rm = TRUE)

p2 <- ggplot(adj_plot, aes(x = raw_epa_per_play, y = script_adjusted_epa,
                            color = adjustment_applied, label = player_name)) +
  # Diagonal: no adjustment line
  geom_abline(slope = 1, intercept = 0, color = "grey60", linetype = "dashed", linewidth = 0.8) +
  geom_point(size = 3, alpha = 0.85) +
  ggrepel::geom_text_repel(size = 2.8, max.overlaps = 10, color = "grey20") +
  # League avg reference lines
  geom_vline(xintercept = avg_epa,
             linetype = "dotted", color = "grey40", linewidth = 0.7) +
  geom_hline(yintercept = avg_adj_epa,
             linetype = "dotted", color = "grey40", linewidth = 0.7) +
  scale_color_gradient2(
    low      = "#D6604D",
    mid      = "grey80",
    high     = "#2166AC",
    midpoint = 0,
    limits   = c(-adj_limit, adj_limit),
    name     = "Adjustment\n(adj - raw)",
    labels   = scales::number_format(accuracy = 0.01)
  ) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  facet_wrap(~ position_group, scales = "free", nrow = 1) +
  labs(
    title    = "Raw vs Script-Adjusted EPA per Play",
    subtitle = "Diagonal = no adjustment | Blue = penalized (favorable scripts) | Red = rewarded (unfavorable)",
    x        = "Raw EPA per Play",
    y        = "Script-Adjusted EPA per Play",
    caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nExperimental metric: additive game script correction using league neutral baseline"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold", size = 10),
    legend.position  = "right",
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 9, color = "grey40"),
    plot.caption     = element_text(size = 7, color = "grey50"),
    panel.grid.minor = element_blank()
  )

# ggrepel might not be available; fall back gracefully
p2_safe <- tryCatch({
  p2
}, error = function(e) {
  # Remove geom_text_repel if ggrepel not installed
  ggplot(adj_plot, aes(x = raw_epa_per_play, y = script_adjusted_epa,
                        color = adjustment_applied)) +
    geom_abline(slope = 1, intercept = 0, color = "grey60", linetype = "dashed", linewidth = 0.8) +
    geom_point(size = 3, alpha = 0.85) +
    geom_text(aes(label = player_name), size = 2.5, nudge_y = 0.005, check_overlap = TRUE) +
    scale_color_gradient2(low = "#D6604D", mid = "grey80", high = "#2166AC",
                          midpoint = 0, name = "Adjustment") +
    facet_wrap(~ position_group, scales = "free", nrow = 1) +
    labs(title = "Raw vs Script-Adjusted EPA per Play",
         x = "Raw EPA per Play", y = "Script-Adjusted EPA per Play",
         caption = "Data: nflfastR | Analysis: NFL Analytics Toolkit") +
    theme_minimal(base_size = 11)
})

ggsave(
  here::here("output", "plots", "week7_raw_vs_adjusted_epa.png"),
  plot   = p2_safe,
  width  = 13,
  height = 6,
  dpi    = 300
)
cat("Saved: week7_raw_vs_adjusted_epa.png\n")


# ==============================================================================
# PLOT 3: WPA LEADERS WITH CLUTCH RATE
# ==============================================================================
# Lollipop chart: top 15 players by total WPA (passers only -- QBs dominate WPA).
# Point color = clutch_rate (high = more positive WPA on high-leverage plays).
# Dashed point border = small_sample clutch rate.

lev_qbs <- lev %>%
  filter(position_group == "passer", total_plays >= 50) %>%
  arrange(desc(total_wpa)) %>%
  head(15) %>%
  mutate(
    player_label = glue("{player_name}\n({high_leverage_plays} HLev plays)"),
    player_label = factor(player_label, levels = rev(player_label)),
    has_clutch_sample = clutch_note == "sufficient_sample"
  )

league_avg_wpa_qb <- mean(lev_qbs$total_wpa, na.rm = TRUE)

p3 <- ggplot(lev_qbs, aes(y = player_label, x = total_wpa)) +
  # Segment from 0 to point
  geom_segment(aes(x = 0, xend = total_wpa, yend = player_label),
               color = "grey70", linewidth = 0.8) +
  # Points colored by clutch rate; shape distinguishes sample quality
  geom_point(aes(color = clutch_rate, shape = has_clutch_sample), size = 5) +
  scale_shape_manual(
    values = c("TRUE" = 19, "FALSE" = 21),
    labels = c("TRUE" = "Sufficient sample (>=10)", "FALSE" = "Small sample (<10)"),
    name   = "High-Leverage\nSample Size"
  ) +
  scale_color_gradient(
    low    = "#D6604D",
    high   = "#2166AC",
    na.value = "grey60",
    name   = "Clutch Rate\n(HLev +WPA%)",
    labels = scales::percent_format(accuracy = 1)
  ) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = league_avg_wpa_qb, color = "grey40",
             linetype = "dashed", linewidth = 0.7) +
  annotate("text", x = league_avg_wpa_qb + 0.002, y = 0.5,
           label = "Avg (top 15)", color = "grey40", size = 2.8, hjust = 0) +
  labs(
    title    = "QB Win Probability Added Leaders",
    subtitle = "Point color = clutch rate (positive WPA on high-leverage plays) | Open circles = small sample",
    x        = "Total WPA (season)",
    y        = NULL,
    caption  = "Data: nflfastR | Analysis: NFL Analytics Toolkit\nHigh-leverage defined as |WPA| >= 0.05 | Garbage time excluded"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y      = element_text(size = 9),
    legend.position  = "right",
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 9, color = "grey40"),
    plot.caption     = element_text(size = 7, color = "grey50"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(
  here::here("output", "plots", "week7_wpa_leaders_clutch.png"),
  plot   = p3,
  width  = 11,
  height = 7,
  dpi    = 300
)
cat("Saved: week7_wpa_leaders_clutch.png\n")

cat("\n=== Week 7 Visualizations Complete ===\n")
cat("Plots saved to: output/plots/\n")
cat("  week7_script_share_matrix.png\n")
cat("  week7_raw_vs_adjusted_epa.png\n")
cat("  week7_wpa_leaders_clutch.png\n")
