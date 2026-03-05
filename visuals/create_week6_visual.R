# ==============================================================================
# WEEK 6: VISUALIZATIONS
# examples/create_week6_visual.R
# ==============================================================================
# Creates 4 publication-quality plots:
#   1. Target share lollipop chart (top receivers by avg target share)
#   2. Air yards share vs target share quadrant scatter (opportunity quality)
#   3. Receiving depth profile heatmap (slot vs balanced vs deep)
#   4. Red zone opportunity concentration (rz20 vs rz10 scatter)
#
# Output: output/plots/week6_*.png (300 dpi)
#
# Run from project root:
#   source("examples/create_week6_visual.R")
#
# NFL Context:
#   - Target share is the single strongest WR fantasy predictor (Week 4 finding)
#   - Air yards share captures opportunity QUALITY beyond raw target count
#   - Depth profile distinguishes slot/YAC receivers from vertical threats
#   - Red zone concentration identifies TD-dependent vs yardage producers
#
# Data notes:
#   - Uses 2025 season throughout (most recent season)
#   - Qualification thresholds set for mid-season applicability:
#       games >= 4 (not 6), total_targets >= 20 (not 30)
#     These are still analytically defensible -- 4 games is minimum for
#     any meaningful rate stat; 20 targets is the WR minimum from Week 4
#     predictive validity study.
#   - Row-count checks halt execution with a diagnostic message before
#     any plot is attempted -- never silently blank.
# ==============================================================================

library(dplyr)
library(ggplot2)
library(glue)
library(here)
library(forcats)
library(tidyr)
library(scales)

source(here("R", "01_data_loading.R"))
source(here("R", "08_usage_features.R"))

# ------------------------------------------------------------------------------
# Diagnostic helper
# Stops immediately with an actionable message if a data prep step
# produces zero rows, so the failure is visible rather than a blank plot.
# ------------------------------------------------------------------------------
check_rows <- function(df, step_label, min_rows = 1L) {
  n <- nrow(df)
  if (n < min_rows) {
    stop(glue(
      "\n[create_week6_visual] DATA FAILURE at step: '{step_label}'\n",
      "  Rows returned: {n} (minimum needed: {min_rows})\n",
      "  Likely causes:\n",
      "    1. 2025 pbp not cached -- run load_and_validate_pbp(seasons = 2025) ",
      "and verify nrow() > 0 before re-running this script\n",
      "    2. calculate_usage_metrics() returned empty -- check 08_usage_features.R ",
      "filters against available weeks in your 2025 cache\n",
      "    3. Qualification threshold too high for current week count in season\n",
      "  Fix: confirm pbp cache with nrow(load_and_validate_pbp(seasons = 2025))"
    ))
  }
  message(glue("  [{step_label}] OK: {n} rows"))
  invisible(df)
}

# ------------------------------------------------------------------------------
# Load 2025 data
# ------------------------------------------------------------------------------
message("Loading play-by-play data (2025 season)...")
pbp <- load_and_validate_pbp(seasons = 2025)

n_pbp   <- nrow(pbp)
n_weeks <- length(unique(pbp$week[!is.na(pbp$week)]))
message(glue("Loaded {n_pbp} plays | {n_weeks} weeks present in 2025 cache"))

if (n_pbp == 0) {
  stop(glue(
    "\n[create_week6_visual] 2025 play-by-play returned 0 rows.\n",
    "  This means nflfastR has not yet cached 2025 data in your environment.\n",
    "  Resolution: run nflfastR::load_pbp(2025) once in your R console to\n",
    "  populate the cache, then re-run this script."
  ))
}

# Compute usage metrics for full 2025 season
message("Computing usage metrics...")
usage <- calculate_usage_metrics(pbp, season = 2025)
rz    <- get_redzone_profile(pbp, season = 2025, min_redzone_opps = 5)
depth <- get_receiving_depth_profile(pbp, season = 2025, min_targets = 20)

message(glue("  usage: {nrow(usage)} player-week rows"))
message(glue("  rz:    {nrow(rz)} players"))
message(glue("  depth: {nrow(depth)} players"))

dir.create(here("output", "plots"), recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# PLOT 1: TOP RECEIVER TARGET SHARES (Season cumulative)
# Lollipop chart -- target share per player, colored by air yards share
# NFL Context: Target share is the most predictive single-game metric for
# WR fantasy production. Air yards share layers on whether targets are
# high-value (downfield) or low-value (check-downs).
# Thresholds: games >= 4 (minimum for any rate stat stability),
#             total_targets >= 20 (WR minimum from Week 4 study)
# ==============================================================================
message("\nBuilding Plot 1: Target share lollipop chart...")

target_shares_season <- usage %>%
  filter(position_group == "receiver", !is.na(target_share)) %>%
  group_by(player_id, player_name, team) %>%
  summarise(
    games               = n_distinct(game_id),
    avg_target_share    = mean(target_share,     na.rm = TRUE),
    avg_air_yards_share = mean(air_yards_share,  na.rm = TRUE),
    total_targets       = sum(targets,           na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(games >= 4, total_targets >= 20) %>%
  arrange(desc(avg_target_share)) %>%
  slice_head(n = 25) %>%
  mutate(
    player_label = paste0(player_name, " (", team, ")"),
    player_label = fct_reorder(player_label, avg_target_share)
  )

check_rows(target_shares_season, "Plot 1 -- qualified receivers", min_rows = 5L)

league_avg_target_share <- mean(target_shares_season$avg_target_share, na.rm = TRUE)
x_max <- max(target_shares_season$avg_target_share, na.rm = TRUE)

p1 <- ggplot(target_shares_season,
             aes(x = avg_target_share, y = player_label)) +
  geom_vline(
    xintercept = league_avg_target_share,
    color = "#CC0000", linetype = "dashed", linewidth = 0.8
  ) +
  # y must reference a valid factor level when y is discrete
  # Use the lowest-ranked player label as the anchor position
  annotate(
    "text",
    x     = league_avg_target_share + 0.003,
    y     = levels(target_shares_season$player_label)[1],
    label = glue("Top-25 avg: {round(league_avg_target_share * 100, 1)}%"),
    hjust = 0, vjust = -0.5, color = "#CC0000", size = 3
  ) +
  geom_segment(
    aes(x = 0, xend = avg_target_share,
        y = player_label, yend = player_label),
    color = "gray75", linewidth = 0.6
  ) +
  geom_point(aes(color = avg_air_yards_share), size = 3.5) +
  scale_color_gradient(
    low    = "#D4E6F1", high = "#154360",
    name   = "Air Yards\nShare",
    labels = percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = c(0, 0),
    limits = c(0, x_max * 1.15)
  ) +
  labs(
    title    = glue("Top 25 Receivers: Average Target Share, 2025"),
    subtitle = glue("Min 4 games, 20 targets | {n_weeks} weeks in dataset | Darker = higher air yards share"),
    x        = "Average Target Share (% of Team Targets)",
    y        = NULL,
    caption  = paste0(
      "Data: nflfastR | Analysis: NFL Analytics Toolkit\n",
      "Target share = receiver targets / team pass attempts per game"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 10, color = "gray40"),
    plot.caption       = element_text(size = 8,  color = "gray50", hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "right",
    axis.text.y        = element_text(size = 9)
  )

ggsave(here("output", "plots", "week6_target_share.png"),
       plot = p1, width = 10, height = 9, dpi = 300)
message("  Saved: output/plots/week6_target_share.png")
print(p1)


# ==============================================================================
# PLOT 2: AIR YARDS SHARE vs TARGET SHARE QUADRANT SCATTER
# NFL Context: Four analytically distinct receiver archetypes:
#   High target share + high air yards share  = true WR1 (volume + quality)
#   High target share + low air yards share   = PPR volume receiver (check-down)
#   Low target share  + high air yards share  = deep threat / boom-bust
#   Low target share  + low air yards share   = low-value depth option
# Fantasy implication: WR1 quadrant has highest floor AND ceiling.
# ==============================================================================
message("\nBuilding Plot 2: Air yards share vs target share quadrant scatter...")

opportunity_quality <- usage %>%
  filter(position_group == "receiver",
         !is.na(target_share), !is.na(air_yards_share)) %>%
  group_by(player_id, player_name, team) %>%
  summarise(
    games               = n_distinct(game_id),
    avg_target_share    = mean(target_share,    na.rm = TRUE),
    avg_air_yards_share = mean(air_yards_share, na.rm = TRUE),
    total_targets       = sum(targets,          na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(games >= 4, total_targets >= 20) %>%
  mutate(player_label = paste0(player_name, " (", team, ")"))

check_rows(opportunity_quality, "Plot 2 -- opportunity quality", min_rows = 5L)

# Median lines for quadrant boundaries
med_ts  <- median(opportunity_quality$avg_target_share,    na.rm = TRUE)
med_ays <- median(opportunity_quality$avg_air_yards_share, na.rm = TRUE)

# Top players to label (top 12 by combined rank)
top_opp <- opportunity_quality %>%
  mutate(combined_rank = rank(-avg_target_share) + rank(-avg_air_yards_share)) %>%
  arrange(combined_rank) %>%
  slice_head(n = 12)

p2 <- ggplot(opportunity_quality,
             aes(x = avg_target_share, y = avg_air_yards_share)) +
  geom_hline(yintercept = med_ays, color = "gray60",
             linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = med_ts,  color = "gray60",
             linetype = "dashed", linewidth = 0.7) +
  annotate("text",
           x = max(opportunity_quality$avg_target_share) * 0.97,
           y = max(opportunity_quality$avg_air_yards_share) * 0.97,
           label = "WR1\n(Volume + Quality)",
           hjust = 1, color = "gray35", size = 2.8, fontface = "italic") +
  annotate("text",
           x = max(opportunity_quality$avg_target_share) * 0.97,
           y = min(opportunity_quality$avg_air_yards_share, na.rm = TRUE) * 1.08,
           label = "PPR Volume\n(Check-down)",
           hjust = 1, color = "gray35", size = 2.8, fontface = "italic") +
  annotate("text",
           x = min(opportunity_quality$avg_target_share) * 1.08,
           y = max(opportunity_quality$avg_air_yards_share) * 0.97,
           label = "Deep Threat\n(Boom-Bust)",
           hjust = 0, color = "gray35", size = 2.8, fontface = "italic") +
  geom_point(aes(size = total_targets), color = "#2980B9",
             alpha = 0.55, shape = 16) +
  geom_point(data = top_opp, aes(size = total_targets),
             color = "#154360", alpha = 0.90, shape = 16) +
  ggrepel::geom_text_repel(
    data          = top_opp,
    aes(label     = player_label),
    size          = 2.7,
    max.overlaps  = 15,
    segment.color = "gray60",
    box.padding   = 0.3
  ) +
  scale_size_continuous(range = c(2, 7), name = "Total\nTargets") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = glue("Receiver Opportunity Quality, 2025"),
    subtitle = glue(
      "Min 4 games, 20 targets | Dashed lines = median | ",
      "{nrow(opportunity_quality)} qualified receivers"
    ),
    x       = "Average Target Share (% of Team Targets)",
    y       = "Average Air Yards Share (% of Team Air Yards)",
    caption = paste0(
      "Data: nflfastR | Analysis: NFL Analytics Toolkit\n",
      "Air yards share on all targets including incompletions"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    plot.caption     = element_text(size = 8,  color = "gray50", hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )

if (!requireNamespace("ggrepel", quietly = TRUE)) {
  p2 <- p2 +
    geom_text(data = top_opp, aes(label = player_label),
              size = 2.6, hjust = -0.1)
  message("  ggrepel not installed -- using geom_text (labels may overlap)")
}

ggsave(here("output", "plots", "week6_opportunity_quality.png"),
       plot = p2, width = 10, height = 8, dpi = 300)
message("  Saved: output/plots/week6_opportunity_quality.png")
print(p2)


# ==============================================================================
# PLOT 3: RECEIVING DEPTH PROFILE HEATMAP
# NFL Context: Distinguishes slot-heavy YAC receivers from vertical threats.
# This feeds directly into Week 7 game script features: deep threats suffer
# more when their team trails and defenses drop into two-deep zone coverage.
# Short  = <10 air yards  (screens, hitches, slants)
# Medium = 10-19 air yards (in-routes, crossers, curls)
# Deep   = 20+ air yards  (posts, go routes, corners)
# ==============================================================================
message("\nBuilding Plot 3: Receiving depth profile heatmap...")

depth_top <- depth %>%
  arrange(desc(total_targets)) %>%
  slice_head(n = 20) %>%
  mutate(player_label = paste0(player_name, " (", team, ")"))

check_rows(depth_top, "Plot 3 -- depth profile", min_rows = 3L)

depth_long <- depth_top %>%
  select(player_label, total_targets, depth_profile,
         short_pct, medium_pct, deep_pct) %>%
  pivot_longer(
    cols      = c(short_pct, medium_pct, deep_pct),
    names_to  = "bucket",
    values_to = "pct"
  ) %>%
  mutate(
    bucket = case_when(
      bucket == "short_pct"  ~ "Short\n(<10 air yds)",
      bucket == "medium_pct" ~ "Medium\n(10-19 air yds)",
      bucket == "deep_pct"   ~ "Deep\n(20+ air yds)"
    ),
    bucket = factor(bucket, levels = c(
      "Short\n(<10 air yds)",
      "Medium\n(10-19 air yds)",
      "Deep\n(20+ air yds)"
    )),
    player_label = fct_reorder(player_label, total_targets),
    # White text on dark tiles, dark text on light tiles
    text_color   = ifelse(pct >= 0.35, "white", "gray30")
  )

p3 <- ggplot(depth_long, aes(x = bucket, y = player_label, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label      = paste0(round(pct * 100, 0), "%"),
                color      = text_color),
            size = 3.0, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradient2(
    low      = "#F0F4F8",
    mid      = "#3498DB",
    high     = "#1A5276",
    midpoint = 0.40,
    labels   = percent_format(accuracy = 1),
    name     = "% of\nTargets"
  ) +
  labs(
    title    = glue("Receiving Depth Profile: Top 20 Receivers by Volume, 2025"),
    subtitle = "% of targets by air yards depth | Ordered by total targets",
    x        = "Depth Bucket",
    y        = NULL,
    caption  = paste0(
      "Data: nflfastR | Analysis: NFL Analytics Toolkit\n",
      "Air yards on all targets including incompletions -- ",
      "captures intended route depth, not just catches"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 10, color = "gray40"),
    plot.caption    = element_text(size = 8,  color = "gray50", hjust = 0),
    panel.grid      = element_blank(),
    axis.text.x     = element_text(size = 10),
    axis.text.y     = element_text(size = 9),
    legend.position = "right"
  )

ggsave(here("output", "plots", "week6_depth_profile.png"),
       plot = p3, width = 10, height = 9, dpi = 300)
message("  Saved: output/plots/week6_depth_profile.png")
print(p3)


# ==============================================================================
# PLOT 4: RED ZONE OPPORTUNITY CONCENTRATION
# NFL Context: Separates "red zone volume" players from "goal line" specialists.
# Fantasy implication:
#   High rz20 + high rz10 = true red zone focal point (TDs + yardage)
#   High rz20 + low rz10  = perimeter red zone target (fade routes, fewer TDs)
#   Low  rz20 + high rz10 = goal line specialist (RB or TE, pure TD scorer)
# min_redzone_opps lowered to 5 to retain players in potential mid-season snapshots
# ==============================================================================
message("\nBuilding Plot 4: Red zone opportunity concentration scatter...")

rz_combined <- rz %>%
  mutate(
    rz20_opps    = rz20_targets + rz20_rushes,
    rz10_opps    = rz10_targets + rz10_rushes,
    player_label = paste0(player_name, " (", team, ")")
  ) %>%
  filter(rz20_opps >= 5)

check_rows(rz_combined, "Plot 4 -- red zone", min_rows = 5L)

avg_rz20 <- mean(rz_combined$rz20_opps, na.rm = TRUE)
avg_rz10 <- mean(rz_combined$rz10_opps, na.rm = TRUE)

top_rz <- rz_combined %>%
  arrange(desc(rz10_opps)) %>%
  slice_head(n = 12)

p4 <- ggplot(rz_combined,
             aes(x = rz20_opps, y = rz10_opps)) +
  geom_hline(yintercept = avg_rz10, color = "gray55",
             linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = avg_rz20, color = "gray55",
             linetype = "dashed", linewidth = 0.7) +
  annotate(
    "text",
    x     = max(rz_combined$rz20_opps, na.rm = TRUE) * 0.97,
    y     = max(rz_combined$rz10_opps, na.rm = TRUE) * 0.97,
    label = "Elite Red Zone\nFocal Point",
    hjust = 1, color = "gray35", size = 2.8, fontface = "italic"
  ) +
  geom_point(
    aes(color = position_group,
        size  = rz20_opps),
    alpha = 0.75
  ) +
  ggrepel::geom_text_repel(
    data          = top_rz,
    aes(label     = player_label),
    size          = 2.8,
    max.overlaps  = 12,
    segment.color = "gray60",
    box.padding   = 0.3
  ) +
  scale_color_manual(
    values = c("receiver" = "#2980B9", "rusher" = "#27AE60"),
    name   = "Position"
  ) +
  scale_size_continuous(range = c(2, 7), name = "RZ20\nOpps") +
  scale_x_continuous(expand = c(0.05, 0)) +
  scale_y_continuous(expand = c(0.05, 0)) +
  labs(
    title    = glue("Red Zone Opportunity Concentration, 2025"),
    subtitle = glue(
      "Inside-20 vs inside-10 opportunities | Min 5 RZ opps | ",
      "{nrow(rz_combined)} players | Size = total RZ20 opportunities"
    ),
    x       = "Opportunities Inside Opponent 20",
    y       = "Opportunities Inside Opponent 10",
    caption = paste0(
      "Data: nflfastR | Analysis: NFL Analytics Toolkit\n",
      "Dashed lines = group averages | Excludes QB kneels and scrambles"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    plot.caption     = element_text(size = 8,  color = "gray50", hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )

if (!requireNamespace("ggrepel", quietly = TRUE)) {
  p4 <- p4 +
    geom_text(data = top_rz, aes(label = player_label),
              size = 2.7, hjust = -0.1)
  message("  ggrepel not installed -- using geom_text (labels may overlap)")
}

ggsave(here("output", "plots", "week6_redzone_concentration.png"),
       plot = p4, width = 10, height = 8, dpi = 300)
message("  Saved: output/plots/week6_redzone_concentration.png")
print(p4)


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================
cat("\n", strrep("=", 60), "\n")
cat("WEEK 6 VISUALIZATION SUMMARY\n")
cat(strrep("=", 60), "\n\n")

cat(glue("2025 season: {n_weeks} weeks loaded, {n_pbp} plays\n\n"))

cat("Target Share (Plot 1):\n")
cat(glue("  Qualified receivers: {nrow(target_shares_season)}\n"))
cat(glue("  Top-25 avg target share: {round(league_avg_target_share * 100, 1)}%\n"))
if (nrow(target_shares_season) > 0) {
  top_player <- target_shares_season$player_label[
    which.max(target_shares_season$avg_target_share)
  ]
  top_share <- max(target_shares_season$avg_target_share, na.rm = TRUE)
  cat(glue("  Leader: {round(top_share * 100, 1)}% ({top_player})\n\n"))
}

cat("Opportunity Quality (Plot 2):\n")
cat(glue("  Qualified receivers: {nrow(opportunity_quality)}\n"))
cat(glue("  Median target share:    {round(med_ts  * 100, 1)}%\n"))
cat(glue("  Median air yards share: {round(med_ays * 100, 1)}%\n\n"))

cat("Depth Profile (Plot 3):\n")
if (nrow(depth) > 0 && "depth_profile" %in% names(depth)) {
  profile_counts <- depth %>% count(depth_profile, name = "n")
  for (i in seq_len(nrow(profile_counts))) {
    cat(glue("  {profile_counts$depth_profile[i]}: {profile_counts$n[i]} receivers\n"))
  }
}
cat("\n")

cat("Red Zone (Plot 4):\n")
cat(glue("  Players with 5+ RZ20 opps: {nrow(rz_combined)}\n"))
cat(glue("  Avg RZ20 opportunities:    {round(avg_rz20, 1)}\n"))
cat(glue("  Avg RZ10 opportunities:    {round(avg_rz10, 1)}\n"))
if (nrow(rz_combined) > 0) {
  top_rz_player <- rz_combined$player_label[which.max(rz_combined$rz10_opps)]
  cat(glue("  RZ10 leader: {top_rz_player}\n"))
}

cat("\nPlots saved:\n")
cat("  output/plots/week6_target_share.png\n")
cat("  output/plots/week6_opportunity_quality.png\n")
cat("  output/plots/week6_depth_profile.png\n")
cat("  output/plots/week6_redzone_concentration.png\n")
cat(strrep("=", 60), "\n")
