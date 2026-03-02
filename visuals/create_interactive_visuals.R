# ==============================================================================
# NFL ANALYTICS TOOLKIT - INTERACTIVE VISUALIZATIONS
# ==============================================================================
#
# Three interactive HTML plots built with native plotly (no ggplotly conversion).
# Each plot exceeds the 50-point density threshold and would be unreadable as
# a static PNG. Interactive HTML is the primary artifact here; no static
# fallback is generated because interactivity IS the analytical value.
#
# PLOTS:
#   1. Pass/Rush EPA Efficiency Quadrant          (Week 2 -- Team Stats)
#   2. Fantasy Scoring System Comparator           (Week 3 -- Fantasy Scoring)
#   3. Consistency vs Ceiling Scatter              (Week 3 -- Consistency Metrics)
#
# COLUMN VERIFICATION (completed against source files before writing):
#   Week 2 team stats  : pass_epa_per_play, rush_epa_per_play (aggregated
#                        from pbp; no pre-split columns in team stats module)
#   Week 3 fantasy     : player_id, player_name, position, team,
#                        total_fantasy_points, receptions, games (via n_distinct)
#
# NFL CONTEXT:
#   EPA = Expected Points Added (EP_after - EP_before)
#   Success rate = proportion of plays with EPA > 0 (NFL standard)
#   Garbage time excluded: win probability <10% or >90% in Q4
#   QB kneels and spikes excluded from all efficiency metrics
#   League average: rush EPA ~0.0, pass EPA ~+0.08, success rate ~45%
#
# HOVER FIX (v2): customdata uses pre-built hover_text string column.
#   cbind() coerces mixed types to character matrix, breaking %{customdata[N]}
#   indexing in plotly. Fix: build hover_text via paste0() in the data frame,
#   pass as single customdata column, use %{customdata}<extra></extra>.
#
# DEPENDENCIES:
#   nflfastR, nflreadr, dplyr, glue, here, plotly, htmlwidgets
#
# USAGE:
#   source(here::here("examples", "create_interactive_visuals.R"))
#   Outputs saved to: output/plots/interactive/
#
# DATA SOURCE: nflfastR | Analysis: NFL Analytics Toolkit
# ==============================================================================

library(dplyr)
library(glue)
library(here)
library(plotly)
library(htmlwidgets)

# Source required toolkit modules
source(here("R", "01_data_loading.R"))
source(here("R", "02_player_stats.R"))
source(here("R", "03_team_stats.R"))
source(here("R", "05_consistency_metrics.R"))

# ==============================================================================
# CONFIGURATION
# ==============================================================================

SEASON            <- 2025
OUTPUT_DIR        <- here("output", "plots", "interactive")
MIN_ATTEMPTS      <- 100L  # QB minimum dropbacks for inclusion
MIN_RUSHES        <- 50L   # RB minimum rushes for inclusion
MIN_TARGETS       <- 40L   # WR/TE minimum targets for inclusion
DENSITY_THRESHOLD <- 50L   # Points above which interactive HTML is mandatory

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message(glue("Created output directory: {OUTPUT_DIR}"))
}

# Shared color palette: position groups
POSITION_COLORS <- list(
  QB = "#003f88",
  RB = "#00843d",
  WR = "#e4002b",
  TE = "#f5a623"
)

# Common hover label style
HOVER_STYLE <- list(
  bgcolor     = "white",
  bordercolor = "#cccccc",
  font        = list(family = "Arial", size = 12)
)

# Common layout defaults
# FIX: plotly axis titles require list(text = ...) not bare string
base_layout <- function(title, xlab, ylab, ...) {
  list(
    title         = list(text = title, font = list(size = 16, family = "Arial Black")),
    xaxis         = list(title = list(text = xlab),
                         gridcolor = "#eeeeee", zerolinecolor = "#bbbbbb"),
    yaxis         = list(title = list(text = ylab),
                         gridcolor = "#eeeeee", zerolinecolor = "#bbbbbb"),
    paper_bgcolor = "white",
    plot_bgcolor  = "white",
    font          = list(family = "Arial"),
    legend        = list(title      = list(text = ""),
                         bgcolor    = "rgba(255,255,255,0.8)",
                         bordercolor = "#dddddd",
                         borderwidth = 1),
    hoverlabel    = HOVER_STYLE,
    ...
  )
}

cat("\n")
cat("================================================================\n")
cat("  NFL Analytics Toolkit -- Interactive Visualizations\n")
cat(glue("  Season: {SEASON}\n"))
cat("================================================================\n\n")

# ==============================================================================
# LOAD DATA (shared across all plots)
# ==============================================================================

cat("Loading play-by-play data...\n")
pbp <- load_and_validate_pbp(SEASON)

cat("Loading roster data...\n")
roster <- get_roster_data(SEASON)

# ==============================================================================
# PLOT 1: PASS/RUSH EPA EFFICIENCY QUADRANT (Week 2 -- Team Stats)
# ==============================================================================

cat("Building Plot 1: Pass/Rush EPA Efficiency Quadrant...\n")

pbp_clean <- pbp %>%
  filter(
    !is.na(epa),
    !is.na(posteam),
    play_type %in% c("pass", "run"),
    !is.na(down),
    !(qtr == 4 & !is.na(wp) & (wp < 0.10 | wp > 0.90)),
    qb_kneel == 0 | is.na(qb_kneel),
    qb_spike  == 0 | is.na(qb_spike)
  )

team_pass_epa <- pbp_clean %>%
  filter(play_type == "pass") %>%
  group_by(posteam) %>%
  summarise(
    pass_epa_per_play = mean(epa, na.rm = TRUE),
    pass_plays        = n(),
    .groups = "drop"
  )

team_rush_epa <- pbp_clean %>%
  filter(play_type == "run") %>%
  group_by(posteam) %>%
  summarise(
    rush_epa_per_play = mean(epa, na.rm = TRUE),
    rush_plays        = n(),
    .groups = "drop"
  )

team_quadrant <- team_pass_epa %>%
  inner_join(team_rush_epa, by = "posteam") %>%
  left_join(
    nflreadr::load_teams() %>%
      select(team_abbr, team_conf, team_division) %>%
      rename(posteam = team_abbr),
    by = "posteam"
  ) %>%
  mutate(
    overall_epa  = (pass_epa_per_play * pass_plays + rush_epa_per_play * rush_plays) /
                   (pass_plays + rush_plays),
    overall_rank = rank(-overall_epa)
  )

# Pre-build hover string -- avoids cbind() type coercion
team_quadrant <- team_quadrant %>%
  mutate(
    hover_text = paste0(
      "<b>", posteam, "</b> (", team_division, ")<br>",
      "Pass EPA/play: ", round(pass_epa_per_play, 3), "<br>",
      "Rush EPA/play: ", round(rush_epa_per_play, 3), "<br>",
      "Pass plays: ",    pass_plays, "<br>",
      "Rush plays: ",    rush_plays, "<br>",
      "Overall rank: #", as.integer(overall_rank), " of 32"
    )
  )

lg_pass_epa  <- mean(team_quadrant$pass_epa_per_play)
lg_rush_epa  <- mean(team_quadrant$rush_epa_per_play)
conferences  <- sort(unique(team_quadrant$team_conf))
conf_colors  <- c("AFC" = "#003f88", "NFC" = "#e4002b")

p1 <- plot_ly()

for (conf in conferences) {
  conf_data <- team_quadrant %>% filter(team_conf == conf)

  p1 <- p1 %>% add_trace(
    data          = conf_data,
    type          = "scatter",
    mode          = "markers+text",
    x             = ~pass_epa_per_play,
    y             = ~rush_epa_per_play,
    text          = ~posteam,
    textposition  = "top center",
    textfont      = list(size = 10),
    name          = conf,
    marker        = list(
      color = conf_colors[[conf]],
      size  = 12,
      line  = list(color = "white", width = 1.5)
    ),
    customdata    = ~hover_text,
    hovertemplate = "%{customdata}<extra></extra>"
  )
}

# Reference lines -- FIX: use hex color strings, not named colors
p1 <- p1 %>%
  add_segments(
    x    = lg_pass_epa, xend = lg_pass_epa,
    y    = min(team_quadrant$rush_epa_per_play) - 0.02,
    yend = max(team_quadrant$rush_epa_per_play) + 0.02,
    line = list(color = "#999999", dash = "dash", width = 1),
    showlegend = FALSE, hoverinfo = "none"
  ) %>%
  add_segments(
    x    = min(team_quadrant$pass_epa_per_play) - 0.02,
    xend = max(team_quadrant$pass_epa_per_play) + 0.02,
    y    = lg_rush_epa, yend = lg_rush_epa,
    line = list(color = "#999999", dash = "dash", width = 1),
    showlegend = FALSE, hoverinfo = "none"
  ) %>%
  add_annotations(
    x = max(team_quadrant$pass_epa_per_play),
    y = max(team_quadrant$rush_epa_per_play),
    text = "Elite both phases",
    showarrow = FALSE, xanchor = "right", yanchor = "top",
    font = list(size = 10, color = "#888888")
  ) %>%
  add_annotations(
    x = min(team_quadrant$pass_epa_per_play),
    y = min(team_quadrant$rush_epa_per_play),
    text = "Struggling both",
    showarrow = FALSE, xanchor = "left", yanchor = "bottom",
    font = list(size = 10, color = "#888888")
  ) %>%
  layout(
    do.call(base_layout, list(
      title = glue("Pass vs Rush EPA Efficiency by Team -- {SEASON}"),
      xlab  = "Pass EPA per Play (garbage time excluded)",
      ylab  = "Rush EPA per Play (garbage time excluded)"
    ))
  ) %>%
  config(
    displaylogo            = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
  )

out1 <- file.path(OUTPUT_DIR, "plot1_team_epa_quadrant_interactive.html")
saveWidget(p1, out1, selfcontained = TRUE)
cat(glue("  Saved: {basename(out1)} ({nrow(team_quadrant)} teams)\n\n"))


# ==============================================================================
# PLOT 2: FANTASY SCORING SYSTEM COMPARATOR (Week 3 -- Fantasy Scoring)
# ==============================================================================

cat("Building Plot 2: Fantasy Scoring System Comparator...\n")
cat("  Calculating fantasy points under 4 scoring systems...\n")

calc_fp <- function(ppr_val, tiered, te_prem, label) {
  calculate_fantasy_points(
    pbp_data       = pbp,
    roster_data    = roster,
    season         = SEASON,
    ppr            = ppr_val,
    use_tiered_ppr = tiered,
    te_premium     = te_prem
  ) %>%
    group_by(player_id, player_name, position, team) %>%
    summarise(
      pts   = sum(total_fantasy_points, na.rm = TRUE),
      games = n_distinct(week),
      .groups = "drop"
    ) %>%
    mutate(system = label)
}

fp_all <- bind_rows(
  calc_fp(0,   FALSE, FALSE, "Standard"),
  calc_fp(0.5, FALSE, FALSE, "Half PPR"),
  calc_fp(1,   FALSE, FALSE, "PPR"),
  calc_fp(1,   TRUE,  TRUE,  "Tiered PPR + TE Premium")
) %>%
  filter(
    position %in% c("QB", "RB", "WR", "TE"),
    games >= 8L
  ) %>%
  group_by(system, position) %>%
  mutate(rank_in_system = rank(-pts, ties.method = "min")) %>%
  ungroup()

# Diagnostic: how many players per position qualify?
pos_counts <- fp_all %>%
  filter(system == "PPR") %>%
  group_by(position) %>%
  summarise(n = n(), .groups = "drop")
cat("  Players qualifying per position (PPR, >=8 games):\n")
for (i in seq_len(nrow(pos_counts))) {
  cat(glue("    {pos_counts$position[i]}: {pos_counts$n[i]}\n"))
}

# Top 15 per position by PPR rank
top_ids <- fp_all %>%
  filter(system == "PPR") %>%
  group_by(position) %>%
  slice_min(order_by = rank_in_system, n = 15) %>%
  pull(player_id)

plot2_data <- fp_all %>%
  filter(player_id %in% top_ids) %>%
  mutate(
    system     = factor(system, levels = c("Standard", "Half PPR", "PPR",
                                           "Tiered PPR + TE Premium")),
    system_num = as.integer(system)
  )

# Pre-build hover string per row
plot2_data <- plot2_data %>%
  mutate(
    hover_text = paste0(
      "<b>", player_name, "</b> (", team, ")<br>",
      "Scoring: ",  as.character(system), "<br>",
      "Points: ",   round(pts, 1), "<br>",
      "Rank among ", position, "s: #", rank_in_system, "<br>",
      "Games: ",    games
    )
  )

n_plot2_points <- n_distinct(plot2_data$player_id) * 4L
cat(glue("  {n_distinct(plot2_data$player_id)} players x 4 systems = {n_plot2_points} points\n"))

positions <- c("QB", "RB", "WR", "TE")

p2 <- plot_ly()

# FIX: track which positions have already added a legend entry separately
legend_added <- c(QB = FALSE, RB = FALSE, WR = FALSE, TE = FALSE)

for (pos in positions) {
  pos_data   <- plot2_data %>% filter(position == pos)
  player_ids <- unique(pos_data$player_id)

  if (length(player_ids) == 0) {
    cat(glue("  WARNING: No qualifying players for position {pos}\n"))
    next
  }

  for (pid in player_ids) {
    pdata <- pos_data %>%
      filter(player_id == pid) %>%
      arrange(system)

    show_leg <- !legend_added[[pos]]
    if (show_leg) legend_added[[pos]] <- TRUE

    p2 <- p2 %>% add_trace(
      data          = pdata,
      type          = "scatter",
      mode          = "lines+markers",
      x             = ~system_num,
      y             = ~rank_in_system,
      line          = list(color = POSITION_COLORS[[pos]], width = 1.5),
      marker        = list(color = POSITION_COLORS[[pos]], size = 7),
      name          = pos,
      legendgroup   = pos,
      showlegend    = show_leg,
      customdata    = ~hover_text,
      hovertemplate = "%{customdata}<extra></extra>"
    )
  }
}

p2 <- p2 %>%
  layout(
    title  = list(text = glue("Fantasy Rank Volatility Across Scoring Systems -- {SEASON}"),
                  font = list(size = 16, family = "Arial Black")),
    xaxis  = list(
      title    = list(text = "Scoring System"),
      tickvals = 1:4,
      ticktext = c("Standard", "Half PPR", "PPR", "Tiered PPR + TE Premium"),
      gridcolor = "#eeeeee"
    ),
    yaxis  = list(
      title     = list(text = "Rank within Position (1 = best)"),
      autorange = "reversed",
      gridcolor = "#eeeeee"
    ),
    paper_bgcolor = "white",
    plot_bgcolor  = "white",
    font          = list(family = "Arial"),
    legend        = list(title       = list(text = "Position"),
                         bgcolor     = "rgba(255,255,255,0.8)",
                         bordercolor = "#dddddd",
                         borderwidth = 1),
    hoverlabel    = HOVER_STYLE,
    annotations   = list(list(
      text      = "Toggle positions via legend | Hover for player details | Steep lines = high rank volatility",
      xref      = "paper", yref = "paper", x = 0, y = -0.12,
      showarrow = FALSE,
      font      = list(size = 10, color = "#888888"),
      xanchor   = "left"
    ))
  ) %>%
  config(
    displaylogo            = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d")
  )

out2 <- file.path(OUTPUT_DIR, "plot2_fantasy_system_comparator_interactive.html")
saveWidget(p2, out2, selfcontained = TRUE)
cat(glue("  Saved: {basename(out2)} ({n_distinct(plot2_data$player_id)} players)\n\n"))


# ==============================================================================
# PLOT 3: CONSISTENCY VS CEILING SCATTER (Week 3 -- Consistency Metrics)
# ==============================================================================

cat("Building Plot 3: Consistency vs Ceiling Scatter...\n")

fp_weekly <- calculate_fantasy_points(
  pbp_data       = pbp,
  roster_data    = roster,
  season         = SEASON,
  ppr            = 1,
  use_tiered_ppr = FALSE,
  te_premium     = FALSE
) %>%
  filter(
    position %in% c("QB", "RB", "WR", "TE"),
    !is.na(total_fantasy_points)
  )

# Season-level stats per player
consistency_stats <- fp_weekly %>%
  group_by(player_id, player_name, position, team) %>%
  summarise(
    games    = n(),
    mean_pts = mean(total_fantasy_points, na.rm = TRUE),
    sd_pts   = sd(total_fantasy_points,   na.rm = TRUE),
    min_pts  = min(total_fantasy_points,  na.rm = TRUE),
    max_pts  = max(total_fantasy_points,  na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    coef_var = ifelse(mean_pts > 0, sd_pts / mean_pts, NA_real_)
  )

# Boom/bust rates: join season mean back to weekly data
boom_bust <- fp_weekly %>%
  left_join(
    consistency_stats %>% select(player_id, mean_pts),
    by = "player_id"
  ) %>%
  group_by(player_id) %>%
  summarise(
    boom_rate = mean(total_fantasy_points > 2.0 * mean_pts, na.rm = TRUE),
    bust_rate = mean(total_fantasy_points < 0.5 * mean_pts, na.rm = TRUE),
    .groups   = "drop"
  )

consistency_final <- consistency_stats %>%
  left_join(boom_bust, by = "player_id") %>%
  filter(
    games >= 8L,
    !is.na(coef_var),
    mean_pts >= 5.0
  )

n_plot3_points <- nrow(consistency_final)
cat(glue("  {n_plot3_points} players qualify (games >= 8, mean_pts >= 5)\n"))

# Pre-build hover string -- numeric formatting done here, not in hovertemplate
consistency_final <- consistency_final %>%
  mutate(
    hover_text = paste0(
      "<b>", player_name, "</b> (", team, ") -- ", position, "<br>",
      "Mean pts/game: ",             round(mean_pts, 1), "<br>",
      "Consistency (CV): ",          round(coef_var, 2),
        " (lower = more consistent)<br>",
      "Weekly range: ",              round(min_pts, 1), " -- ",
                                     round(max_pts, 1), " pts<br>",
      "Boom rate (>2x avg): ",       scales::percent(boom_rate, accuracy = 1), "<br>",
      "Bust rate (<0.5x avg): ",     scales::percent(bust_rate, accuracy = 1), "<br>",
      "Games: ",                     games
    )
  )

med_cv  <- median(consistency_final$coef_var,  na.rm = TRUE)
med_pts <- median(consistency_final$mean_pts, na.rm = TRUE)

p3 <- plot_ly()

for (pos in positions) {
  pos_data <- consistency_final %>% filter(position == pos)
  if (nrow(pos_data) == 0) next

  p3 <- p3 %>% add_trace(
    data          = pos_data,
    type          = "scatter",
    mode          = "markers",
    x             = ~coef_var,
    y             = ~mean_pts,
    name          = pos,
    marker        = list(
      color   = POSITION_COLORS[[pos]],
      size    = 10,
      opacity = 0.8,
      line    = list(color = "white", width = 1)
    ),
    customdata    = ~hover_text,
    hovertemplate = "%{customdata}<extra></extra>"
  )
}

p3 <- p3 %>%
  add_segments(
    x = med_cv, xend = med_cv,
    y = 0, yend = max(consistency_final$mean_pts, na.rm = TRUE) * 1.05,
    line = list(color = "#999999", dash = "dot", width = 1),
    showlegend = FALSE, hoverinfo = "none"
  ) %>%
  add_segments(
    x = 0, xend = max(consistency_final$coef_var, na.rm = TRUE) * 1.05,
    y = med_pts, yend = med_pts,
    line = list(color = "#999999", dash = "dot", width = 1),
    showlegend = FALSE, hoverinfo = "none"
  ) %>%
  add_annotations(
    x = max(consistency_final$coef_var, na.rm = TRUE) * 0.95,
    y = max(consistency_final$mean_pts, na.rm = TRUE),
    text = "High ceiling\nHigh variance\n(boom/bust)",
    showarrow = FALSE, xanchor = "right", yanchor = "top",
    font = list(size = 9, color = "#888888"), align = "right"
  ) %>%
  add_annotations(
    x = min(consistency_final$coef_var, na.rm = TRUE),
    y = max(consistency_final$mean_pts, na.rm = TRUE),
    text = "High ceiling\nHigh floor\n(elite consistent)",
    showarrow = FALSE, xanchor = "left", yanchor = "top",
    font = list(size = 9, color = "#888888"), align = "left"
  ) %>%
  layout(
    do.call(base_layout, list(
      title = glue("Consistency vs Ceiling: PPR Fantasy {SEASON} (min 8 games)"),
      xlab  = "Coefficient of Variation (lower = more consistent)",
      ylab  = "Mean PPR Points per Game"
    ))
  ) %>%
  config(
    displaylogo            = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d")
  )

out3 <- file.path(OUTPUT_DIR, "plot3_consistency_vs_ceiling_interactive.html")
saveWidget(p3, out3, selfcontained = TRUE)
cat(glue("  Saved: {basename(out3)} ({n_plot3_points} players)\n\n"))


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("================================================================\n")
cat("  Interactive Visualizations -- Complete\n")
cat("================================================================\n")
cat(glue("  Output directory: output/plots/interactive/\n\n"))
cat("  Files generated:\n")
cat("  1. plot1_team_epa_quadrant_interactive.html\n")
cat("     Pass vs Rush EPA by team | Toggle AFC/NFC | 32 teams\n\n")
cat("  2. plot2_fantasy_system_comparator_interactive.html\n")
cat("     Rank volatility across 4 scoring systems | Toggle by position\n\n")
cat("  3. plot3_consistency_vs_ceiling_interactive.html\n")
cat("     Mean pts vs coefficient of variation | Boom/bust hover detail\n\n")
cat("  All files are self-contained HTML (no server required).\n")
cat("  Open directly in any browser.\n")
cat("  These are exploration tools -- static PNGs remain the shareable artifacts.\n")
cat("================================================================\n")
