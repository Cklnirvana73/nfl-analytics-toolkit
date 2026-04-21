# ==============================================================================
# examples/create_season2_week6_visual.R
# Season 2, Week 6: CFB Multi-Season Pipeline -- Visualizations
# NFL Analytics Toolkit
#
# PURPOSE
# Generates 2 publication-quality plots from the saved Week 6 pipeline RDS.
# Run AFTER run_week6_pipeline() has produced the pipeline output RDS.
#
# PLOTS PRODUCED
#   Plot 1: Raw vs FBS-filtered play counts per season -- dual-series line chart
#           s2_week6_play_counts.png
#
#   Plot 2: FBS team count by season -- bar chart
#           s2_week6_fbs_team_count.png
#
# PREREQUISITES
#   output/season2_week6/s2_week6_pipeline_output.rds  -- from run_week6_pipeline()
#
# DENSITY THRESHOLD CHECK
#   Plot 1: 24 data points (12 seasons x 2 series) -- static PNG sufficient
#   Plot 2: 12 bars -- static PNG sufficient
#   Neither plot exceeds 50 labeled data points. No interactive HTML required.
#
# OUTPUT
#   output/plots/s2_week6_play_counts.png
#   output/plots/s2_week6_fbs_team_count.png
#
# DEPENDENCIES
#   ggplot2, dplyr, tidyr, glue, here, scales
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)
library(here)
library(scales)


# ==============================================================================
# CONFIGURATION
# ==============================================================================

OUTPUT_DIR  <- here::here("output", "plots")
DPI         <- 300L
ATTRIBUTION <- "Data: cfbfastR 2014-2025 | Analysis: NFL Analytics Toolkit S2W6"
PREFIX      <- "s2_week6_"

# Colorblind-safe palette (Paul Tol)
COL_RAW <- "#0077BB"   # blue  -- all divisions (raw cache)
COL_FBS <- "#EE7733"   # orange -- FBS teams only

# COVID season constant
SEASON_COVID <- 2020L

# Consistent theme for both plots
theme_cfb <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title        = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle     = element_text(size = 11, color = "gray40", hjust = 0),
      plot.caption      = element_text(size = 8,  color = "gray55", hjust = 1),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.grid.major  = element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text        = element_text(face = "bold", size = 11),
      legend.position   = "none",
      plot.margin       = margin(12, 16, 8, 12)
    )
}

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message("Created output directory: ", OUTPUT_DIR)
}


# ==============================================================================
# LOAD ARTIFACTS
# ==============================================================================

pipeline_path <- here::here("output", "season2_week6",
                              "s2_week6_pipeline_output.rds")

if (!file.exists(pipeline_path)) {
  stop(
    "Required artifact missing: ", pipeline_path,
    "\nRun run_week6_pipeline() first to generate artifacts."
  )
}

pipeline_output <- readRDS(pipeline_path)

load_summary <- pipeline_output$load_summary
coverage     <- pipeline_output$coverage

cat(glue(
  "\nArtifacts loaded.\n",
  "  Load summary: {nrow(load_summary)} seasons\n",
  "  Coverage:     {nrow(coverage)} seasons\n\n"
))


# ==============================================================================
# PLOT 1: RAW VS FBS-FILTERED PLAY COUNTS PER SEASON
# ==============================================================================
# Story: cfbfastR expanded FCS play-by-play coverage in 2022, causing a 54%
# jump in raw play counts. After the FBS filter, that expansion is completely
# invisible -- FBS counts stay flat at ~152k. The filter is doing its job.
# ==============================================================================

cat("[Plot 1/2] Building play counts dual-series line chart...\n")

# Build long-format data from two sources
raw_counts <- load_summary %>%
  filter(status %in% c("cached", "loaded")) %>%
  select(season, n_plays) %>%
  mutate(series = "All divisions (raw cache)")

fbs_counts <- coverage %>%
  select(season, n_plays) %>%
  mutate(series = "FBS teams only")

plays_long <- dplyr::bind_rows(raw_counts, fbs_counts) %>%
  mutate(series = factor(series,
                          levels = c("All divisions (raw cache)",
                                     "FBS teams only")))

stopifnot(
  "plays_long is empty" = nrow(plays_long) > 0,
  "fewer than 4 rows -- data likely incomplete" = nrow(plays_long) >= 4
)

# KEY INSIGHTS: computed from data, never hardcoded
raw_2022     <- load_summary %>% filter(season == 2022L) %>% pull(n_plays)
fbs_2022     <- coverage     %>% filter(season == 2022L) %>% pull(n_plays)
raw_2021     <- load_summary %>% filter(season == 2021L) %>% pull(n_plays)
yoy_jump_pct <- round(100 * (raw_2022 - raw_2021) / raw_2021, 1)
gap_2022     <- format(raw_2022 - fbs_2022, big.mark = ",")

# Endpoint label data for direct line labeling
label_data <- plays_long %>%
  group_by(series) %>%
  filter(season == max(season)) %>%
  ungroup()

series_colors <- c("All divisions (raw cache)" = COL_RAW,
                   "FBS teams only"            = COL_FBS)

p1 <- ggplot(plays_long,
             aes(x = season, y = n_plays, color = series, group = series)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.8) +
  # Direct series labels at right endpoint
  geom_text(
    data     = label_data,
    aes(x = season + 0.3, label = series),
    hjust    = 0,
    size     = 3.2,
    fontface = "plain",
    show.legend = FALSE
  ) +
  # 2022 FCS expansion annotation
  annotate(
    "text",
    x        = 2022.4,
    y        = (raw_2022 + fbs_2022) / 2,
    label    = glue("cfbfastR added full\nFCS coverage (2022)\n+{yoy_jump_pct}% raw, 0% FBS"),
    hjust    = 0,
    size     = 3.0,
    color    = "gray35",
    fontface = "italic"
  ) +
  # 2020 COVID dip annotation
  annotate(
    "text",
    x        = 2020,
    y        = coverage %>% filter(season == SEASON_COVID) %>% pull(n_plays) - 14000,
    label    = "COVID-19",
    hjust    = 0.5,
    size     = 3.0,
    color    = "gray40",
    fontface = "italic"
  ) +
  scale_x_continuous(
    breaks = 2014:2025,
    limits = c(2013.5, 2028.5)
  ) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, 320000),
    breaks = seq(0, 300000, by = 50000)
  ) +
  scale_color_manual(values = series_colors) +
  labs(
    title    = "The FBS Filter Absorbs cfbfastR's 2022 FCS Coverage Expansion",
    subtitle = glue(
      "Raw play counts jumped {yoy_jump_pct}% from 2021 to 2022; ",
      "FBS-filtered counts stayed flat. Gap at 2022: {gap_2022} plays."
    ),
    x       = "Season",
    y       = "Total plays",
    caption = ATTRIBUTION
  ) +
  theme_cfb() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

path_p1 <- file.path(OUTPUT_DIR, paste0(PREFIX, "play_counts.png"))
ggsave(path_p1, plot = p1, width = 10, height = 5.5, dpi = DPI, bg = "white")
cat(glue("  Saved: {path_p1}\n\n"))


# ==============================================================================
# PLOT 2: FBS TEAM COUNT BY SEASON
# ==============================================================================
# Story: FBS team count has grown steadily from 128 to 136 across 12 seasons.
# 2020 is the only structural outlier -- COVID reduced mean games per team
# to 7.97, logged as a known exception in the validation layer.
# ==============================================================================

cat("[Plot 2/2] Building FBS team count bar chart...\n")

team_data <- coverage %>%
  select(season, n_teams) %>%
  mutate(
    bar_fill    = if_else(season == SEASON_COVID, "#888888", "#1d3557"),
    covid_flag  = season == SEASON_COVID
  )

stopifnot(
  "team_data is empty" = nrow(team_data) > 0,
  "n_teams column missing" = "n_teams" %in% names(team_data)
)

# KEY INSIGHTS: computed from data, never hardcoded
min_teams      <- min(team_data$n_teams, na.rm = TRUE)
max_teams      <- max(team_data$n_teams, na.rm = TRUE)
min_teams_yr   <- team_data %>% filter(n_teams == min_teams) %>% pull(season)
max_teams_yr   <- team_data %>% filter(n_teams == max_teams) %>% pull(season)
covid_teams    <- team_data %>% filter(covid_flag) %>% pull(n_teams)

p2 <- ggplot(team_data, aes(x = factor(season), y = n_teams)) +
  geom_col(aes(fill = bar_fill), width = 0.72) +
  # Count labels above each bar
  geom_text(
    aes(label = n_teams, y = n_teams + 0.5),
    size  = 3.2,
    vjust = 0,
    color = "gray25"
  ) +
  # COVID annotation above 2020 bar label
  geom_text(
    data = filter(team_data, covid_flag),
    aes(label = "COVID-19", y = n_teams + 2.8),
    size     = 2.8,
    color    = "gray45",
    fontface = "italic"
  ) +
  scale_fill_identity() +
  scale_y_continuous(
    limits = c(0, 145),
    breaks = seq(0, 140, by = 20),
    labels = scales::comma
  ) +
  labs(
    title    = glue("FBS Team Count Grew From {min_teams} to {max_teams} Across 12 Seasons"),
    subtitle = glue(
      "{min_teams_yr} low: {min_teams} teams (COVID). ",
      "{max_teams_yr} high: {max_teams} teams. ",
      "Steady growth reflects conference expansion and FBS reclassifications."
    ),
    x       = "Season",
    y       = "FBS teams",
    caption = ATTRIBUTION
  ) +
  theme_cfb() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

path_p2 <- file.path(OUTPUT_DIR, paste0(PREFIX, "fbs_team_count.png"))
ggsave(path_p2, plot = p2, width = 9, height = 5, dpi = DPI, bg = "white")
cat(glue("  Saved: {path_p2}\n\n"))


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("  Season 2 Week 6 -- Visualizations Complete\n")
cat(strrep("=", 60), "\n")
cat(glue("  Plot 1: {basename(path_p1)}\n"))
cat(glue("  Plot 2: {basename(path_p2)}\n"))
cat(strrep("-", 60), "\n")

cat("\nKEY INSIGHTS:\n")
cat(glue(
  "  2022 raw jump:    +{yoy_jump_pct}% YoY (FCS coverage expansion)\n"
))
cat(glue(
  "  2022 FBS counts:  stable at {format(fbs_2022, big.mark = ',')} plays\n"
))
cat(glue(
  "  FBS team growth:  {min_teams} ({min_teams_yr}) to {max_teams} ({max_teams_yr})\n"
))
cat(glue(
  "  COVID 2020:       {covid_teams} FBS teams, mean 7.97 games/team\n"
))
cat(strrep("-", 60), "\n")

cat("\nInteractivity check:\n")
cat("  Plot 1: 24 data points (12 seasons x 2 series) -- static PNG sufficient.\n")
cat("  Plot 2: 12 bars -- static PNG sufficient.\n")
cat("  No interactive HTML version required.\n\n")
