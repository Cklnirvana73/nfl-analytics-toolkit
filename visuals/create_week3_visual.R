# ==============================================================================
# WEEK 3 VISUALIZATIONS FOR LINKEDIN POST
# ==============================================================================
#
# Creates 3 publication-quality visualizations:
# 1. Fantasy Points Distribution by Position
# 2. Rolling Average Trends (Featured Player)
# 3. Consistency Scatter Plot (Mean vs Variance)
#
# Output: output/plots/week3_*.png files
#
# Data Science Best Practices Applied:
# - Coefficient of variation for consistency measurement
# - Rolling averages visualize trends vs season averages
# - Clear statistical annotations (zones, thresholds)
#
# ==============================================================================

library(dplyr)
library(ggplot2)
library(scales)

source("R/01_data_loading.R")
source("R/05_consistency_metrics.R")

# ==============================================================================
# SETUP: Load Data & Calculate Fantasy Points
# ==============================================================================

cat("\n================================================================================\n")
cat("WEEK 3 VISUALIZATION GENERATION\n")
cat("================================================================================\n\n")

cat("Loading 2024 NFL data...\n")
pbp_2024 <- load_and_validate_pbp(2024)
roster_2024 <- get_roster_data(2024)

cat("Calculating fantasy points with your default settings...\n")
cat("  (Tiered PPR, TE Premium, 6pt Pass TDs, 0.25 Rush Bonus)\n")
fantasy_ppr <- calculate_fantasy_points(
  pbp_data = pbp_2024,
  roster_data = roster_2024,
  season = 2024,
  week_max = 10  # First 10 weeks of season
)

cat("Adding rolling averages...\n")
fantasy_rolling <- calculate_rolling_fantasy(fantasy_ppr)

cat("Data preparation complete!\n\n")

# Create output directory if it doesn't exist
if (!dir.exists("output/plots")) {
  dir.create("output/plots", recursive = TRUE)
}

# ==============================================================================
# VISUALIZATION 1: Fantasy Points Distribution by Position
# ==============================================================================

cat("Creating Visualization 1: Fantasy Points Distribution...\n")

viz1_data <- fantasy_ppr %>%
  filter(position %in% c("QB", "RB", "WR")) %>%
  group_by(player_id, player_name, position) %>%
  summarise(avg_points = mean(total_fantasy_points), .groups = "drop")

p1 <- ggplot(viz1_data, aes(x = position, y = avg_points, fill = position)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("QB" = "#E74C3C", "RB" = "#3498DB", "WR" = "#2ECC71")) +
  labs(
    title = "Fantasy Points Distribution by Position",
    subtitle = "2024 Season (Weeks 1-10) | Tiered PPR + TE Premium + 6pt Pass TDs",
    x = "Position",
    y = "Average Fantasy Points per Game",
    caption = "Data: nflfastR | Analysis: NFL Analytics Toolkit"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave("output/plots/week3_fantasy_distribution.png", p1, width = 10, height = 6, dpi = 300)
cat("  ✓ Saved: output/plots/week3_fantasy_distribution.png\n\n")

# ==============================================================================
# VISUALIZATION 2: Rolling Average Trends (Featured Player)
# ==============================================================================

cat("Creating Visualization 2: Rolling Average Trends...\n")

# Select top WR by average fantasy points for visualization
featured_player <- fantasy_rolling %>%
  filter(position == "WR", !is.na(total_fantasy_points_roll3)) %>%
  group_by(player_name) %>%
  summarise(avg_pts = mean(total_fantasy_points), .groups = "drop") %>%
  arrange(desc(avg_pts)) %>%
  slice(1) %>%
  pull(player_name)

viz2_data <- fantasy_rolling %>%
  filter(player_name == featured_player)

p2 <- ggplot(viz2_data, aes(x = week)) +
  geom_line(aes(y = total_fantasy_points), color = "gray70", linewidth = 1) +
  geom_point(aes(y = total_fantasy_points), color = "gray50", size = 3) +
  geom_line(aes(y = total_fantasy_points_roll3, color = "3-Game Avg"), 
            linewidth = 1.2) +
  geom_line(aes(y = total_fantasy_points_roll6, color = "6-Game Avg"), 
            linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("3-Game Avg" = "#E74C3C", "6-Game Avg" = "#3498DB")) +
  labs(
    title = paste0(featured_player, " - Rolling Fantasy Point Averages"),
    subtitle = "Gray = actual weekly performance | Colored = rolling averages (excludes current game)",
    x = "Week",
    y = "Fantasy Points (Tiered PPR)",
    color = NULL,
    caption = "Data: nflfastR | Analysis: NFL Analytics Toolkit | Feature leakage prevented"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave("output/plots/week3_rolling_trends.png", p2, width = 10, height = 6, dpi = 300)
cat("  ✓ Saved: output/plots/week3_rolling_trends.png\n\n")

# ==============================================================================
# VISUALIZATION 3: Consistency Scatter Plot
# ==============================================================================

cat("Creating Visualization 3: Consistency Analysis...\n")

# Calculate coefficient of variation (CV) for consistency metric
# CV = SD / Mean (normalized measure of variability)
# Low CV = consistent "high floor" player
# High CV = boom/bust "high ceiling" player
viz3_data <- fantasy_ppr %>%
  filter(position %in% c("RB", "WR")) %>%
  group_by(player_id, player_name, position) %>%
  summarise(
    games = n(),
    avg_points = mean(total_fantasy_points),
    sd_points = sd(total_fantasy_points),
    cv = sd_points / avg_points,  # Coefficient of variation
    .groups = "drop"
  ) %>%
  filter(games >= 6) %>%  # Minimum 6 games for statistical reliability
  mutate(
    consistency_category = case_when(
      cv < 0.4 ~ "High Floor (Consistent)",
      cv > 0.7 ~ "Boom/Bust (Volatile)",
      TRUE ~ "Moderate Variance"
    )
  )

p3 <- ggplot(viz3_data, aes(x = avg_points, y = cv, color = position)) +
  geom_point(aes(size = games), alpha = 0.6) +
  geom_hline(yintercept = 0.4, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_hline(yintercept = 0.7, linetype = "dashed", color = "gray50", alpha = 0.5) +
  scale_color_manual(values = c("RB" = "#3498DB", "WR" = "#2ECC71")) +
  scale_size_continuous(range = c(2, 8)) +
  labs(
    title = "Fantasy Player Consistency: Floor vs Ceiling",
    subtitle = "Coefficient of Variation (CV) = SD / Mean | Lower CV = More Consistent",
    x = "Average Fantasy Points per Game",
    y = "Coefficient of Variation (Consistency Metric)",
    color = "Position",
    size = "Games Played",
    caption = "Data: nflfastR | Analysis: NFL Analytics Toolkit | Min 6 games"
  ) +
  annotate("text", x = 5, y = 0.35, label = "High Floor Zone", 
           color = "darkgreen", size = 3.5, fontface = "italic") +
  annotate("text", x = 5, y = 0.75, label = "Boom/Bust Zone", 
           color = "darkred", size = 3.5, fontface = "italic") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

ggsave("output/plots/week3_consistency_scatter.png", p3, width = 12, height = 8, dpi = 300)
cat("  ✓ Saved: output/plots/week3_consistency_scatter.png\n\n")

# ==============================================================================
# SUMMARY STATISTICS FOR LINKEDIN POST
# ==============================================================================

cat("================================================================================\n")
cat("VISUALIZATION SUMMARY\n")
cat("================================================================================\n\n")

cat("Generated 3 visualizations for LinkedIn post:\n")
cat("  1. Fantasy points distribution by position (boxplots)\n")
cat("  2. Rolling average trends for", featured_player, "\n")
cat("  3. Consistency scatter plot (CV analysis)\n\n")

cat("Key Statistics:\n")
cat(sprintf("  • Total player-weeks analyzed: %s\n", format(nrow(fantasy_ppr), big.mark = ",")))
cat(sprintf("  • Unique players: %s\n", format(n_distinct(fantasy_ppr$player_id), big.mark = ",")))
cat(sprintf("  • Weeks covered: %d\n", max(fantasy_ppr$week)))

# Show top 5 most consistent players
cat("\nTop 5 Most Consistent Players (Lowest CV):\n")
top_consistent <- viz3_data %>%
  arrange(cv) %>%
  head(5) %>%
  select(player_name, position, avg_points, cv)
print(top_consistent)

cat("\nTop 5 Most Volatile Players (Highest CV):\n")
top_volatile <- viz3_data %>%
  arrange(desc(cv)) %>%
  head(5) %>%
  select(player_name, position, avg_points, cv)
print(top_volatile)

cat("\n================================================================================\n")
cat("WEEK 3 VISUALIZATIONS COMPLETE\n")
cat("================================================================================\n")
cat("\nFiles saved to output/plots/:\n")
cat("  • week3_fantasy_distribution.png\n")
cat("  • week3_rolling_trends.png\n")
cat("  • week3_consistency_scatter.png\n\n")
cat("Ready for LinkedIn post!\n\n")

# ==============================================================================
# DATA SCIENCE NOTES
# ==============================================================================

# Coefficient of Variation (CV) Interpretation:
# - CV < 0.4: High floor player (consistent, reliable for lineup)
# - CV 0.4-0.7: Moderate variance (typical fantasy player)
# - CV > 0.7: Boom/bust player (high ceiling, low floor)
#
# Why CV matters:
# - Normalizes variability across different scoring levels
# - A RB averaging 15 pts with SD=3 (CV=0.2) is more consistent than
#   a WR averaging 15 pts with SD=9 (CV=0.6)
# - Fantasy managers trade consistency for upside based on team needs
#
# Rolling Averages:
# - Feature leakage prevented: uses lag() to exclude current game
# - Shows TRENDS not just HISTORY (season average hides recent performance)
# - 3-game window = recent form, 6-game window = sustained production
