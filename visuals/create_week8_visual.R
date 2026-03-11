# ==============================================================================
# WEEK 8 VISUALIZATIONS: OPPONENT-ADJUSTED METRICS & MATCHUP FEATURES
# ==============================================================================
#
# Generates 3 publication-quality plots:
#   1. week8_schedule_difficulty_vs_raw_epa.png
#      Scatter: raw EPA vs opponent-adjusted EPA by position group.
#      Reveals which players benefited from/were hurt by schedule.
#
#   2. week8_defensive_style_epa_matrix.png
#      Dot plot: each defense's pass EPA allowed vs rush EPA allowed.
#      Color = defensive_style, size = total plays. Reference lines at
#      league averages. Identifies pass-funnel and run-funnel defenses.
#
#   3. week8_matchup_archetype_heatmap.png
#      Heatmap: player (top N by position) x defensive archetype -> EPA.
#      Grays out cells where min_plays_per_archetype not met (NA).
#      Shows which players are most matchup-sensitive.
#
# Output directory: output/plots/
# Resolution: 300 dpi
# Attribution: "Data: nflfastR | Analysis: NFL Analytics Toolkit"
#
# ==============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(glue)
library(here)

source(here("R", "01_data_loading.R"))
source(here("R", "10_opponent_features.R"))

# --- Parameters ---------------------------------------------------------------
SEASON          <- 2025
WEEK_MIN        <- 1
WEEK_MAX        <- 18
MIN_PLAYS       <- 50
MIN_DEF_PLAYS   <- 100
TOP_N_PLAYERS   <- 10   # players per position in archetype heatmap
OUTPUT_DIR      <- here("output", "plots")
DPI             <- 300

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Load data ----------------------------------------------------------------
cat("Loading play-by-play data...\n")
pbp <- load_and_validate_pbp(seasons = SEASON)

# --- Build function outputs ---------------------------------------------------
cat("Computing opponent adjustments...\n")
opp_adj <- calculate_opponent_adjustments(
  pbp, season = SEASON, week_min = WEEK_MIN, week_max = WEEK_MAX,
  min_plays = MIN_PLAYS
)

cat("Classifying defensive styles...\n")
def_styles <- classify_defensive_style(
  pbp, season = SEASON, week_min = WEEK_MIN, week_max = WEEK_MAX,
  min_def_plays = MIN_DEF_PLAYS
)

cat("Computing matchup history...\n")
arch_history <- calculate_matchup_history(
  pbp, def_styles,
  season = SEASON, week_min = WEEK_MIN, week_max = WEEK_MAX,
  min_plays_per_archetype = 15
)

# --- Summary stats to console -------------------------------------------------
cat("\n", strrep("=", 60), "\n")
cat("WEEK 8 VISUALIZATION SUMMARY STATS\n")
cat(strrep("=", 60), "\n")

cat(glue("\nOpponent adjustments: {nrow(opp_adj)} player-position rows\n"))
cat(glue("  Mean opp_adjustment: {round(mean(opp_adj$opp_adjustment), 4)}\n"))
cat(glue("  Max |adjustment|: {round(max(abs(opp_adj$opp_adjustment)), 4)}\n"))
cat(glue("  League avg EPA allowed: {round(opp_adj$league_avg_epa_allowed[1], 4)}\n"))

cat(glue("\nDefensive styles: {nrow(def_styles)} teams\n"))
def_styles %>%
  count(defensive_style) %>%
  mutate(pct = round(n / sum(n) * 100)) %>%
  print()

cat(glue("\nMatchup history: {nrow(arch_history)} rows\n"))
n_with_range <- sum(!is.na(arch_history$archetype_epa_range))
cat(glue("  With archetype range: {n_with_range}\n"))
if (n_with_range > 0) {
  cat(glue(
    "  Max archetype EPA range: ",
    "{round(max(arch_history$archetype_epa_range, na.rm = TRUE), 4)}\n"
  ))
}

# ==============================================================================
# PLOT 1: SCHEDULE DIFFICULTY vs RAW EPA (SCATTER)
# ==============================================================================
cat("\nGenerating Plot 1: Schedule difficulty vs raw EPA...\n")

# All three position groups
plot1_data <- opp_adj %>%
  filter(position_group %in% c("passer", "receiver", "rusher"),
         total_plays >= MIN_PLAYS) %>%
  mutate(
    above_diagonal = opp_adjusted_epa > raw_epa_per_play,
    label_player   = position_group == "passer" |
      abs(opp_adjustment) > quantile(abs(opp_adjustment), 0.75)
  )

# Add league average reference for diagonal
diag_range <- range(c(plot1_data$raw_epa_per_play,
                       plot1_data$opp_adjusted_epa), na.rm = TRUE)

p1 <- ggplot(plot1_data,
             aes(x = raw_epa_per_play, y = opp_adjusted_epa,
                 color = opp_adjustment, size = total_plays)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_point(alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "gray40", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted",
             color = "gray40", linewidth = 0.5) +
  ggrepel::geom_text_repel(
    data = plot1_data %>% filter(label_player),
    aes(label = player_name),
    size = 2.8, color = "gray20", max.overlaps = 12,
    show.legend = FALSE
  ) +
  scale_color_gradient2(
    low     = "#d73027",
    mid     = "gray70",
    high    = "#4575b4",
    midpoint = 0,
    name   = "Opp. Adjustment\n(adj - raw)"
  ) +
  scale_size_continuous(range = c(2, 6), name = "Total Plays", guide = "none") +
  facet_wrap(~position_group, scales = "free") +
  labs(
    title    = "Raw vs Opponent-Adjusted EPA per Play",
    subtitle = glue(
      "Diagonal = no adjustment | Blue = rewarded (tough schedule) | ",
      "Red = penalized (easy schedule)"
    ),
    x        = "Raw EPA per Play",
    y        = "Opponent-Adjusted EPA per Play",
    caption  = glue(
      "Data: nflfastR | Analysis: NFL Analytics Toolkit\n",
      "Adjustment = player_EPA - (opponent_avg_EPA_allowed - league_avg)\n",
      "Garbage time excluded (WP < 10% or > 90% in Q4) | {SEASON} season | ",
      "Min {MIN_PLAYS} plays"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title     = element_text(face = "bold", size = 14),
    plot.subtitle  = element_text(color = "gray40", size = 10),
    plot.caption   = element_text(color = "gray50", size = 7.5, hjust = 1),
    strip.text     = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# Handle optional ggrepel dependency gracefully
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  p1 <- p1 +
    geom_text(
      data = plot1_data %>% filter(label_player),
      aes(label = player_name),
      size = 2.5, vjust = -0.8, color = "gray20", show.legend = FALSE
    )
}

ggsave(
  filename = file.path(OUTPUT_DIR, "week8_schedule_difficulty_vs_raw_epa.png"),
  plot     = p1,
  width    = 12, height = 6, dpi = DPI
)
cat("  Saved: week8_schedule_difficulty_vs_raw_epa.png\n")

# --- Interactive version (HTML) -----------------------------------------------
# The static version is too dense to read at full scale. The interactive version
# allows the reader to:
#   - Hover any point for player name, raw EPA, adjusted EPA, adjustment delta,
#     total plays, and position group
#   - Filter to a single position group by clicking its legend entry
#   - Isolate a region by zooming/panning
#   - Double-click legend to reset
#
# Saved as self-contained HTML -- no server or R session required to view.
# Open in any browser. Suitable for sharing alongside LinkedIn posts.
#
# NOTE: plotly and htmlwidgets are required. Both are standard CRAN packages.
# Install if needed: install.packages(c("plotly", "htmlwidgets"))

if (requireNamespace("plotly", quietly = TRUE) &&
    requireNamespace("htmlwidgets", quietly = TRUE)) {

  library(plotly)
  library(htmlwidgets)

  # Build tooltip text with all relevant context
  plot1_interactive <- plot1_data %>%
    mutate(
      tooltip = paste0(
        "<b>", player_name, "</b><br>",
        "Position: ", position_group, "<br>",
        "Raw EPA/play: ", round(raw_epa_per_play, 3), "<br>",
        "Adj EPA/play: ", round(opp_adjusted_epa, 3), "<br>",
        "Opp adjustment: ", ifelse(opp_adjustment >= 0, "+", ""),
                           round(opp_adjustment, 3), "<br>",
        "Total plays: ", total_plays
      )
    )

  # Diagonal reference line range
  diag_min <- min(c(plot1_interactive$raw_epa_per_play,
                    plot1_interactive$opp_adjusted_epa), na.rm = TRUE)
  diag_max <- max(c(plot1_interactive$raw_epa_per_play,
                    plot1_interactive$opp_adjusted_epa), na.rm = TRUE)

  p1_plotly <- plot_ly() %>%

    # Diagonal reference line (no adjustment)
    add_segments(
      x = diag_min, xend = diag_max,
      y = diag_min, yend = diag_max,
      line = list(color = "gray60", dash = "dash", width = 1),
      showlegend = FALSE, hoverinfo = "none"
    ) %>%

    # Zero reference lines
    add_segments(x = 0, xend = 0, y = diag_min, yend = diag_max,
                 line = list(color = "gray70", dash = "dot", width = 0.8),
                 showlegend = FALSE, hoverinfo = "none") %>%
    add_segments(x = diag_min, xend = diag_max, y = 0, yend = 0,
                 line = list(color = "gray70", dash = "dot", width = 0.8),
                 showlegend = FALSE, hoverinfo = "none") %>%

    # One trace per position group so legend entries toggle visibility
    add_trace(
      data = plot1_interactive %>% filter(position_group == "passer"),
      type = "scatter", mode = "markers",
      x = ~raw_epa_per_play, y = ~opp_adjusted_epa,
      color = ~opp_adjustment,
      colors = c("#d73027", "gray70", "#4575b4"),
      marker = list(size = ~sqrt(total_plays) * 1.2,
                    sizemode = "diameter",
                    opacity = 0.85,
                    line = list(color = "white", width = 0.5)),
      text = ~tooltip, hoverinfo = "text",
      name = "Passer", legendgroup = "passer"
    ) %>%
    add_trace(
      data = plot1_interactive %>% filter(position_group == "receiver"),
      type = "scatter", mode = "markers",
      x = ~raw_epa_per_play, y = ~opp_adjusted_epa,
      color = ~opp_adjustment,
      colors = c("#d73027", "gray70", "#4575b4"),
      marker = list(size = ~sqrt(total_plays) * 1.2,
                    sizemode = "diameter",
                    opacity = 0.85,
                    symbol = "circle-open",
                    line = list(color = NULL, width = 1.5)),
      text = ~tooltip, hoverinfo = "text",
      name = "Receiver", legendgroup = "receiver",
      showlegend = TRUE
    ) %>%
    add_trace(
      data = plot1_interactive %>% filter(position_group == "rusher"),
      type = "scatter", mode = "markers",
      x = ~raw_epa_per_play, y = ~opp_adjusted_epa,
      color = ~opp_adjustment,
      colors = c("#d73027", "gray70", "#4575b4"),
      marker = list(size = ~sqrt(total_plays) * 1.2,
                    sizemode = "diameter",
                    opacity = 0.85,
                    symbol = "diamond",
                    line = list(color = "white", width = 0.5)),
      text = ~tooltip, hoverinfo = "text",
      name = "Rusher", legendgroup = "rusher",
      showlegend = TRUE
    ) %>%

    layout(
      title = list(
        text = "<b>Raw vs Opponent-Adjusted EPA per Play</b><br><sup>Hover for details | Click legend to filter by position | Scroll to zoom</sup>",
        font = list(size = 15)
      ),
      xaxis = list(
        title = "Raw EPA per Play",
        zeroline = FALSE,
        gridcolor = "rgba(200,200,200,0.3)"
      ),
      yaxis = list(
        title = "Opponent-Adjusted EPA per Play",
        zeroline = FALSE,
        gridcolor = "rgba(200,200,200,0.3)"
      ),
      legend = list(
        title = list(text = "<b>Position</b><br><sup>(click to filter)</sup>"),
        orientation = "v"
      ),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      annotations = list(
        list(
          text = paste0(
            "Data: nflfastR | Analysis: NFL Analytics Toolkit | ",
            "Adjustment = player_EPA - (opp_avg_EPA_allowed - league_avg) | ",
            SEASON, " season | Min ", MIN_PLAYS, " plays"
          ),
          xref = "paper", yref = "paper",
          x = 1, y = -0.12, xanchor = "right", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 9, color = "gray50")
        )
      )
    ) %>%
    colorbar(title = "Opp. Adjustment<br>(adj - raw)")

  html_path <- file.path(OUTPUT_DIR,
                         "week8_schedule_difficulty_vs_raw_epa_interactive.html")
  saveWidget(p1_plotly, file = html_path, selfcontained = TRUE)
  cat("  Saved: week8_schedule_difficulty_vs_raw_epa_interactive.html\n")
  cat("  Open in any browser -- no R session required.\n")

} else {
  cat("  Skipping interactive version: plotly or htmlwidgets not installed.\n")
  cat("  Install with: install.packages(c('plotly', 'htmlwidgets'))\n")
}


# ==============================================================================
# PLOT 2: DEFENSIVE STYLE EPA MATRIX (DOT PLOT)
# ==============================================================================
cat("\nGenerating Plot 2: Defensive style EPA matrix...\n")

# Compute league averages for reference lines
league_pass_epa <- mean(def_styles$pass_epa_allowed, na.rm = TRUE)
league_rush_epa <- mean(def_styles$rush_epa_allowed, na.rm = TRUE)

# Style colors
style_colors <- c(
  "pass_funnel" = "#d73027",
  "run_funnel"  = "#4575b4",
  "balanced"    = "gray55"
)

style_labels <- c(
  "pass_funnel" = "Pass Funnel\n(pass-vulnerable)",
  "run_funnel"  = "Run Funnel\n(run-vulnerable)",
  "balanced"    = "Balanced"
)

p2 <- ggplot(def_styles,
             aes(x = rush_epa_allowed, y = pass_epa_allowed,
                 color = defensive_style, size = pass_plays_allowed + rush_plays_allowed)) +
  # Quadrant shading
  annotate("rect", xmin = -Inf, xmax = league_rush_epa,
           ymin = league_pass_epa, ymax = Inf,
           fill = "#d73027", alpha = 0.04) +
  annotate("rect", xmin = league_rush_epa, xmax = Inf,
           ymin = -Inf, ymax = league_pass_epa,
           fill = "#4575b4", alpha = 0.04) +
  geom_vline(xintercept = league_rush_epa,
             linetype = "dashed", color = "gray50", linewidth = 0.7) +
  geom_hline(yintercept = league_pass_epa,
             linetype = "dashed", color = "gray50", linewidth = 0.7) +
  geom_point(alpha = 0.85) +
  geom_text(
    aes(label = team),
    size = 2.5, vjust = -0.9, color = "gray20", show.legend = FALSE
  ) +
  scale_color_manual(
    values = style_colors,
    labels = style_labels,
    name   = "Defensive Style"
  ) +
  scale_size_continuous(range = c(2, 7), name = "Total Plays Faced",
                        guide = "none") +
  annotate("text", x = league_rush_epa + 0.001, y = Inf,
           label = "League avg rush", hjust = 0, vjust = 1.5,
           size = 2.8, color = "gray45") +
  annotate("text", x = -Inf, y = league_pass_epa + 0.001,
           label = "League avg pass", hjust = -0.1, vjust = -0.5,
           size = 2.8, color = "gray45") +
  labs(
    title    = "Defensive Style: EPA Allowed by Play Type",
    subtitle = glue(
      "Top-left = pass-funnel (better vs rush) | ",
      "Bottom-right = run-funnel (better vs pass)"
    ),
    x        = "Rush EPA Allowed per Play",
    y        = "Pass EPA Allowed per Play",
    caption  = glue(
      "Data: nflfastR | Analysis: NFL Analytics Toolkit\n",
      "EPA allowed uses EPA > 0 = offense succeeded (higher = worse defense)\n",
      "Garbage time excluded (WP < 10% or > 90% in Q4) | {SEASON} season | ",
      "Min {MIN_DEF_PLAYS} plays"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "gray40", size = 10),
    plot.caption    = element_text(color = "gray50", size = 7.5, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(OUTPUT_DIR, "week8_defensive_style_epa_matrix.png"),
  plot     = p2,
  width    = 9, height = 8, dpi = DPI
)
cat("  Saved: week8_defensive_style_epa_matrix.png\n")


# ==============================================================================
# PLOT 3: MATCHUP ARCHETYPE HEATMAP
# ==============================================================================
cat("\nGenerating Plot 3: Matchup archetype heatmap...\n")

# Select top 5 and bottom 5 players per position for best-vs-worst contrast.
# Three filters applied before selection:
#   1. min 100 plays against classified defenses -- excludes injured/backup
#      players whose archetype samples are too sparse to be meaningful.
#      Computed as sum of archetype play buckets (plays against defenses that
#      met min_def_plays in classify_defensive_style). This undercounts vs
#      raw season plays, which is intentional -- we want classified-defense
#      sample size, not just total snaps.
#   2. at least one non-NA archetype cell -- player faced enough plays against
#      at least one archetype style to meet min_plays_per_archetype.
#   3. position_group in the three standard groups.
MIN_CLASSIFIED_PLAYS <- 100L

qualified <- arch_history %>%
  filter(position_group %in% c("passer", "receiver", "rusher")) %>%
  mutate(
    total_classified_plays = vs_pass_funnel_plays + vs_run_funnel_plays +
                             vs_balanced_plays
  ) %>%
  filter(
    total_classified_plays >= MIN_CLASSIFIED_PLAYS,
    !is.na(vs_pass_funnel_epa) |
    !is.na(vs_run_funnel_epa)  |
    !is.na(vs_balanced_epa)
  )

top_players <- bind_rows(
  qualified %>%
    group_by(position_group) %>%
    slice_max(order_by = overall_epa_per_play, n = 5, with_ties = FALSE) %>%
    mutate(.selection = "top"),
  qualified %>%
    group_by(position_group) %>%
    slice_min(order_by = overall_epa_per_play, n = 5, with_ties = FALSE) %>%
    mutate(.selection = "bottom")
) %>%
  # Remove any player who appears in both top and bottom (can happen with
  # small position groups where top 5 + bottom 5 exhausts all qualifiers)
  distinct(position_group, player_id, .keep_all = TRUE) %>%
  ungroup()

# Reshape to long format for heatmap
heatmap_data <- top_players %>%
  select(player_name, position_group,
         vs_pass_funnel_epa, vs_run_funnel_epa, vs_balanced_epa) %>%
  pivot_longer(
    cols      = c(vs_pass_funnel_epa, vs_run_funnel_epa, vs_balanced_epa),
    names_to  = "archetype",
    values_to = "epa"
  ) %>%
  mutate(
    archetype = case_when(
      archetype == "vs_pass_funnel_epa" ~ "vs Pass Funnel",
      archetype == "vs_run_funnel_epa"  ~ "vs Run Funnel",
      archetype == "vs_balanced_epa"    ~ "vs Balanced"
    ),
    archetype = factor(archetype,
                       levels = c("vs Pass Funnel", "vs Balanced",
                                  "vs Run Funnel")),
    has_data  = !is.na(epa),
    # For display, NA appears as gray; non-NA uses EPA color scale
    epa_display = epa
  ) %>%
  arrange(position_group, desc(player_name))

if (nrow(heatmap_data) == 0) {
  cat("  Insufficient data for archetype heatmap -- no players met thresholds. Skipping.\n")
  cat(glue("  Thresholds: min_classified_plays={MIN_CLASSIFIED_PLAYS}, min_plays_per_archetype=15\n"))
} else {
  # Warn if any position group has fewer than 10 qualifying players (5 top + 5 bottom)
  position_counts <- top_players %>% count(position_group)
  sparse_positions <- position_counts %>% filter(n < 10)
  if (nrow(sparse_positions) > 0) {
    cat(glue(
      "  NOTE: Some positions have fewer than 10 qualifying players: ",
      "{paste(paste(sparse_positions$position_group, sparse_positions$n, sep='='), collapse=', ')}\n",
      "  Consider lowering MIN_CLASSIFIED_PLAYS (currently {MIN_CLASSIFIED_PLAYS}) or min_plays_per_archetype.\n"
    ))
  }

  # Order: best EPA at top, worst at bottom within each position.
  # ggplot y axis reads factor levels bottom-to-top, so arrange worst-to-best
  # here so that best naturally appears at the top of each facet panel.
  player_order <- top_players %>%
    arrange(position_group, overall_epa_per_play) %>%
    pull(player_name)

  heatmap_data <- heatmap_data %>%
    mutate(player_name = factor(player_name,
                                levels = unique(player_order)))

  p3 <- ggplot(heatmap_data,
               aes(x = archetype, y = player_name, fill = epa_display)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_tile(data = heatmap_data %>% filter(!has_data),
              fill = "gray88", color = "white", linewidth = 0.5) +
    geom_text(
      data = heatmap_data %>% filter(has_data),
      aes(label = round(epa_display, 3)),
      size = 2.8, color = "gray10"
    ) +
    geom_text(
      data = heatmap_data %>% filter(!has_data),
      label = "n/a", size = 2.5, color = "gray65"
    ) +
    scale_fill_gradient2(
      low      = "#d73027",
      mid      = "white",
      high     = "#4575b4",
      midpoint = 0,
      na.value = "gray88",
      name     = "EPA per Play"
    ) +
    facet_wrap(~position_group, scales = "free_y", ncol = 3) +
    labs(
      title    = "Player EPA by Defensive Archetype",
      subtitle = glue(
        "Blue = better performance against that style | Gray = insufficient sample ",
        "(< 15 plays)"
      ),
      x        = NULL,
      y        = NULL,
      caption  = glue(
        "Data: nflfastR | Analysis: NFL Analytics Toolkit\n",
        "Top 5 and bottom 5 per position by overall EPA/play | ",
        "Min {MIN_CLASSIFIED_PLAYS} plays vs classified defenses | ",
        "Garbage time excluded | {SEASON} season | Experimental metric"
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "gray40", size = 9),
      plot.caption  = element_text(color = "gray50", size = 7.5, hjust = 1),
      strip.text    = element_text(face = "bold"),
      axis.text.x   = element_text(size = 9, angle = 15, hjust = 1),
      axis.text.y   = element_text(size = 9),
      panel.grid    = element_blank(),
      legend.position = "right"
    )

  ggsave(
    filename = file.path(OUTPUT_DIR, "week8_matchup_archetype_heatmap.png"),
    plot     = p3,
    width    = 14, height = 8, dpi = DPI
  )
  cat("  Saved: week8_matchup_archetype_heatmap.png\n")
}

cat("\nWeek 8 visualizations complete.\n")
cat(glue("  Output directory: {OUTPUT_DIR}\n"))
