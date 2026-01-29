# Week 1 Visualization
# Shows data quality metrics for LinkedIn post

library(tidyverse)
library(nflfastR)

# Source our functions
source("R/01_data_loading.R")

# Load data for multiple seasons
pbp <- load_and_validate_pbp(2020:2025, validate = FALSE)

# Calculate plays per season
plays_by_season <- pbp %>%
  group_by(season) %>%
  summarise(
    total_plays = n(),
    games = n_distinct(game_id),
    avg_plays_per_game = total_plays / games
  )

# Create visualization
plot <- ggplot(plays_by_season, aes(x = season, y = total_plays)) +
  geom_col(fill = "#013369", alpha = 0.8) +
  geom_text(aes(label = format(total_plays, big.mark = ",")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::comma, 
                     limits = c(0, max(plays_by_season$total_plays) * 1.1)) +
  scale_x_continuous(breaks = 2020:2025) +
  labs(
    title = "NFL Play-by-Play Data Quality Check",
    subtitle = "Total plays loaded and validated per season (2020-2025)",
    x = "Season",
    y = "Total Plays",
    caption = "Data: nflfastR | Validation: load_and_validate_pbp()"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray50"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save plot
ggsave("output/plots/week1_data_validation.png", 
       plot = plot, 
       width = 10, 
       height = 6, 
       dpi = 300)

cat("\n✓ Visualization saved to: output/plots/week1_data_validation.png\n")

# Print summary table
cat("\nData Summary:\n")
print(plays_by_season)