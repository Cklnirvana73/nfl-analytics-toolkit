# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 1
# Visualization Script
# File: examples/create_season2_week1_visual.R
#
# Purpose: Generate 4 publication-quality validation plots confirming the
#          16-season multi-year data pipeline is working correctly.
#
# Plots:
#   1. EPA density overlay across eras (2010-2014, 2015-2020, 2021-2025)
#   2. Season-level play count bar chart with 17-game breakpoint
#   3. Schema coverage heatmap (columns x seasons)
#   4. Team game count validation tile plot (teams x seasons)
#
# Prerequisites:
#   - R/15_multi_season_pbp.R sourced
#   - All 16 seasons cached via load_multi_season_pbp() or run_week1_pipeline()
#
# Output: output/plots/ directory (4 PNG files, 300 dpi)
#
# Production-grade for portfolio display
# Built for personal analytics use
# ==============================================================================

library(here)
library(dplyr)
library(ggplot2)
library(glue)
library(tidyr)

# Source production file
source(here::here("R", "15_multi_season_pbp.R"))

# Output directory
PLOT_DIR <- here::here("output", "plots")
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

DATA_ATTRIBUTION <- "Data: nflfastR | Analysis: NFL Analytics Toolkit"


# ==============================================================================
# PLOT 1: EPA Density Overlay Across Eras
# ==============================================================================
# Shows EPA distribution for pass/run plays (garbage time excluded) across
# three era groupings. Validates that EPA is centered near zero in all eras
# and surfaces any shifts in nflfastR's underlying EP model.
#
# Chart type: density (distribution of one continuous variable)
# Reference: visualization-patterns.md > Chart Type Selection Framework
# ==============================================================================

cat("Plot 1: EPA density overlay across eras...\n")

# Sample from each era to keep density plot manageable
# Full 16 seasons of pass/run plays would be millions of rows
set.seed(42)
era_sample_size <- 50000L

era_data <- list()

for (s in SEASON_RANGE_DEFAULT) {
  pbp <- load_normalized_season(s)

  clean <- pbp %>%
    filter(
      !is.na(epa),
      !is.na(posteam),
      play_type %in% c("pass", "run"),
      !is.na(wp),
      wp >= 0.10,
      wp <= 0.90
    ) %>%
    select(season, epa, play_type)

  era_data[[as.character(s)]] <- clean
  rm(pbp, clean)
  gc(verbose = FALSE)
}

all_epa <- bind_rows(era_data)
rm(era_data)
gc(verbose = FALSE)

# Assign era groupings
all_epa <- all_epa %>%
  mutate(
    era = case_when(
      season >= 2010 & season <= 2014 ~ "2010-2014",
      season >= 2015 & season <= 2020 ~ "2015-2020",
      season >= 2021 & season <= 2025 ~ "2021-2025",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(era))

# Sample within each era for plotting performance
era_sampled <- all_epa %>%
  group_by(era) %>%
  slice_sample(n = era_sample_size) %>%
  ungroup()

# Compute era means for annotation
era_means <- all_epa %>%
  group_by(era) %>%
  summarise(
    mean_epa = mean(epa, na.rm = TRUE),
    n_plays = n(),
    .groups = "drop"
  )

cat("  Era means:\n")
for (i in seq_len(nrow(era_means))) {
  cat(glue("    {era_means$era[i]}: mean EPA = {round(era_means$mean_epa[i], 4)}, ",
           "n = {format(era_means$n_plays[i], big.mark = ',')}"), "\n")
}

p1 <- ggplot(era_sampled, aes(x = epa, fill = era, color = era)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.6) +
  scale_fill_manual(
    values = c("2010-2014" = "#1b9e77", "2015-2020" = "#d95f02", "2021-2025" = "#7570b3"),
    name = "Era"
  ) +
  scale_color_manual(
    values = c("2010-2014" = "#1b9e77", "2015-2020" = "#d95f02", "2021-2025" = "#7570b3"),
    name = "Era"
  ) +
  coord_cartesian(xlim = c(-6, 6)) +
  labs(
    title = "EPA Distribution Across 16 Seasons of NFL Data",
    subtitle = "Pass and run plays, garbage time excluded (WP 10%-90%)",
    x = "EPA (Expected Points Added)",
    y = "Density",
    caption = glue("{DATA_ATTRIBUTION}\n",
                   "EPA = EP_after - EP_before | Centered near zero confirms valid EP model\n",
                   "Sampled {format(era_sample_size, big.mark = ',')} plays per era for plotting")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(color = "gray50", size = 8),
    legend.position = "top"
  )

ggsave(
  file.path(PLOT_DIR, "season2_week1_epa_density_by_era.png"),
  plot = p1, width = 10, height = 6, dpi = 300
)
cat("  Saved: season2_week1_epa_density_by_era.png\n\n")

rm(all_epa, era_sampled, era_means, p1)
gc(verbose = FALSE)


# ==============================================================================
# PLOT 2: Season-Level Play Count Bar Chart with 17-Game Breakpoint
# ==============================================================================
# Horizontal bar chart showing total offensive plays (pass + run) per season.
# Color break at 2021 marks the 17-game transition.
# Validates expected volume increase and flags suspicious seasons.
#
# Chart type: bar chart (comparison across categories)
# Reference: visualization-patterns.md > Chart Type Selection Framework
# ==============================================================================

cat("Plot 2: Season-level play count bar chart...\n")

season_counts <- list()

for (s in SEASON_RANGE_DEFAULT) {
  pbp <- load_normalized_season(s)

  counts <- pbp %>%
    filter(
      !is.na(posteam),
      play_type %in% c("pass", "run")
    ) %>%
    summarise(
      season = first(season),
      n_plays = n(),
      n_games = length(unique(game_id)),
      .groups = "drop"
    )

  season_counts[[as.character(s)]] <- counts
  rm(pbp, counts)
  gc(verbose = FALSE)
}

season_df <- bind_rows(season_counts) %>%
  mutate(
    schedule_era = ifelse(season >= 2021, "17-Game (2021+)", "16-Game (2010-2020)"),
    plays_per_game = round(n_plays / n_games, 1)
  )

rm(season_counts)

cat("  Play counts by season:\n")
for (i in seq_len(nrow(season_df))) {
  cat(glue("    {season_df$season[i]}: {format(season_df$n_plays[i], big.mark = ',')} plays ",
           "({season_df$n_games[i]} games, {season_df$plays_per_game[i]} per game)"), "\n")
}

p2 <- ggplot(season_df, aes(x = factor(season), y = n_plays, fill = schedule_era)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = which(season_df$season == 2021) - 0.5,
             linetype = "dashed", color = "gray30", linewidth = 0.6) +
  annotate("text",
           x = which(season_df$season == 2021) - 0.5,
           y = max(season_df$n_plays) * 1.02,
           label = "17-game season begins",
           hjust = 1.05, size = 3.2, color = "gray30", fontface = "italic") +
  scale_fill_manual(
    values = c("16-Game (2010-2020)" = "#4575b4", "17-Game (2021+)" = "#d73027"),
    name = "Schedule Era"
  ) +
  scale_y_continuous(labels = scales::comma_format(), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Total Offensive Plays Per Season (2010-2025)",
    subtitle = "Pass and run plays only, all teams combined",
    x = "Season",
    y = "Total Plays",
    caption = glue("{DATA_ATTRIBUTION}\n",
                   "NFL expanded to 17-game regular season in 2021 (272 games vs 256)")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(color = "gray50", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

ggsave(
  file.path(PLOT_DIR, "season2_week1_plays_per_season.png"),
  plot = p2, width = 10, height = 6, dpi = 300
)
cat("  Saved: season2_week1_plays_per_season.png\n\n")

rm(season_df, p2)
gc(verbose = FALSE)


# ==============================================================================
# PLOT 3: Schema Coverage Heatmap
# ==============================================================================
# Key analytical columns on y-axis, seasons on x-axis. Filled by
# presence/absence. Shows which features are available for the full
# 16-year span versus only recent seasons.
#
# Uses get_schema_differences() output directly.
# Sorted by coverage count (fewest seasons at top) per
# faceted heatmap ordering rule in visualization-patterns.md.
#
# Chart type: heatmap (presence matrix)
# Reference: visualization-patterns.md > Heatmap Row Selection Rule
# ==============================================================================

cat("Plot 3: Schema coverage heatmap...\n")

schema_diffs <- get_schema_differences()

# Select analytically important columns to display
# Full schema has 300+ columns -- showing all would be unreadable
# Focus on columns that matter for downstream modeling
key_columns <- c(
  # Core EPA and efficiency
  "epa", "success", "wp", "cpoe", "pass_oe", "xpass",
  "xyac_epa", "xyac_mean_yardage", "comp_air_epa", "comp_yac_epa",
  # Player IDs
  "passer_player_id", "rusher_player_id", "receiver_player_id",
  # Play classification
  "play_type", "qb_dropback", "shotgun", "no_huddle",
  # Yardage detail
  "air_yards", "yards_after_catch", "passing_yards", "rushing_yards",
  # Game context
  "down", "ydstogo", "yardline_100",
  # Binary outcomes
  "touchdown", "interception", "fumble", "sack", "complete_pass",
  # Structural
  "game_id", "play_id", "season", "week", "posteam", "defteam"
)

# Filter to key columns that exist in schema_diffs
plot_schema <- schema_diffs %>%
  filter(column_name %in% key_columns)

# Reshape to long format for heatmap
season_cols <- names(plot_schema)[grepl("^s\\d{4}$", names(plot_schema))]

schema_long <- plot_schema %>%
  pivot_longer(
    cols = all_of(season_cols),
    names_to = "season_label",
    values_to = "present"
  ) %>%
  mutate(
    season = as.integer(gsub("^s", "", season_label))
  )

# Count seasons per column for ordering (fewest at top)
col_coverage <- schema_long %>%
  group_by(column_name) %>%
  summarise(n_present = sum(present), .groups = "drop") %>%
  arrange(n_present)

# Order columns: fewest seasons at top (ascending coverage)
# ggplot2 y-axis draws bottom-to-top, so reverse for visual
schema_long <- schema_long %>%
  mutate(column_name = factor(column_name, levels = rev(col_coverage$column_name)))

cat("  Key columns tracked: ", nrow(col_coverage), "\n")
cat("  Columns available in all 16 seasons: ",
    sum(col_coverage$n_present == length(SEASON_RANGE_DEFAULT)), "\n")
cat("  Columns available in fewer than 16 seasons: ",
    sum(col_coverage$n_present < length(SEASON_RANGE_DEFAULT)), "\n")

p3 <- ggplot(schema_long, aes(x = factor(season), y = column_name, fill = present)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = c("TRUE" = "#2166ac", "FALSE" = "#f4a582"),
    labels = c("TRUE" = "Available", "FALSE" = "Missing"),
    name = "Column Status"
  ) +
  labs(
    title = "Schema Coverage: Key Analytical Columns Across 16 Seasons",
    subtitle = "Sorted by coverage (fewest seasons at top). Missing = added as NA by normalize_schema().",
    x = "Season",
    y = NULL,
    caption = glue("{DATA_ATTRIBUTION}\n",
                   "Showing {nrow(col_coverage)} key columns out of {nrow(schema_diffs)} total\n",
                   "Optional columns (cpoe, pass_oe, xyac_*) added as NA for earlier seasons")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 9),
    plot.caption = element_text(color = "gray50", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 8, family = "mono"),
    legend.position = "top",
    panel.grid = element_blank()
  )

ggsave(
  file.path(PLOT_DIR, "season2_week1_schema_coverage.png"),
  plot = p3, width = 12, height = 8, dpi = 300
)
cat("  Saved: season2_week1_schema_coverage.png\n\n")

rm(schema_diffs, plot_schema, schema_long, col_coverage, p3)
gc(verbose = FALSE)


# ==============================================================================
# PLOT 4: Team Game Count Validation Tile Plot
# ==============================================================================
# 32 teams on y-axis, seasons on x-axis, tile color = games played.
# Expected to be uniform (16 or 17 per team per season).
# Instantly surfaces data gaps, relocation mapping errors, or missing data.
#
# Chart type: tile/heatmap (matrix validation)
# Reference: visualization-patterns.md > Heatmap Row Selection Rule
# ==============================================================================

cat("Plot 4: Team game count validation tile plot...\n")

team_games <- list()

for (s in SEASON_RANGE_DEFAULT) {
  pbp <- load_normalized_season(s)

  tg <- pbp %>%
    filter(
      !is.na(posteam), posteam != "",
      # Regular season only: week <= 18 for 2021+, week <= 17 for pre-2021
      !is.na(week),
      week <= ifelse(s >= 2021, 18L, 17L)
    ) %>%
    distinct(game_id, posteam) %>%
    count(posteam, name = "games_played") %>%
    mutate(season = s)

  team_games[[as.character(s)]] <- tg
  rm(pbp, tg)
  gc(verbose = FALSE)
}

team_df <- bind_rows(team_games)
rm(team_games)

# Check for relocation artifacts
old_teams <- c("OAK", "SD", "STL")
relo_check <- team_df %>% filter(posteam %in% old_teams)
if (nrow(relo_check) > 0) {
  cat("  WARNING: Old team abbreviations found after normalization:\n")
  print(relo_check)
} else {
  cat("  Relocation mapping clean: no OAK, SD, or STL found.\n")
}

# Expected games per team per season
team_df <- team_df %>%
  mutate(
    expected_games = ifelse(season >= 2021, 17L, 16L),
    deviation = games_played - expected_games,
    status = case_when(
      deviation == 0 ~ "Expected",
      deviation > 0  ~ "Over",
      deviation < 0  ~ "Under"
    )
  )

# Sort teams alphabetically for clean display
team_order <- sort(unique(team_df$posteam))
team_df <- team_df %>%
  mutate(posteam = factor(posteam, levels = rev(team_order)))

n_deviations <- sum(team_df$deviation != 0, na.rm = TRUE)
cat("  Total team-seasons: ", nrow(team_df), "\n")
cat("  Deviations from expected: ", n_deviations, "\n")

p4 <- ggplot(team_df, aes(x = factor(season), y = posteam, fill = games_played)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#f7f7f7",
    high = "#4575b4",
    midpoint = 16.5,
    name = "Games\nPlayed",
    breaks = c(14, 15, 16, 17, 18)
  ) +
  labs(
    title = "Team Game Counts Across 16 Seasons (2010-2025)",
    subtitle = glue("Expected: 16 games (2010-2020), 17 games (2021-2025) | ",
                    "{n_deviations} deviations found"),
    x = "Season",
    y = NULL,
    caption = glue("{DATA_ATTRIBUTION}\n",
                   "Team abbreviations normalized: OAK->LV (2020), SD->LAC (2017), STL->LA (2016)\n",
                   "Deviations may reflect cancelled/rescheduled games or data edge cases")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 9),
    plot.caption = element_text(color = "gray50", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 7),
    legend.position = "right",
    panel.grid = element_blank()
  )

ggsave(
  file.path(PLOT_DIR, "season2_week1_team_game_counts.png"),
  plot = p4, width = 13, height = 9, dpi = 300
)
cat("  Saved: season2_week1_team_game_counts.png\n\n")

rm(team_df, p4)
gc(verbose = FALSE)


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("================================================================\n")
cat("Season 2, Week 1 Visualization Script Complete\n")
cat("================================================================\n")
cat("Files saved to: ", PLOT_DIR, "\n")
cat("  1. season2_week1_epa_density_by_era.png\n")
cat("     EPA distribution across three eras, garbage time excluded\n")
cat("  2. season2_week1_plays_per_season.png\n")
cat("     Total offensive plays per season with 17-game breakpoint\n")
cat("  3. season2_week1_schema_coverage.png\n")
cat("     Key column availability across 16 seasons\n")
cat("  4. season2_week1_team_game_counts.png\n")
cat("     32-team game count validation with relocation check\n")
cat("================================================================\n")
