# ==============================================================================
# examples/create_season2_week8_visuals.R
# Visualizations for R/22_sos_reconciliation.R
#
# Execution order (project standard): example -> tests -> visuals
# Run example_season2_week8.R before this script.
#
# Plots:
#
#   Plot 1 (s2_week8_qb_sos_vs_efficiency.png):
#     QB pass EPA per attempt vs mean opponent defensive EPA allowed.
#     Story: Which QBs outperformed their schedule difficulty?
#     Points above the regression line = efficiency above what schedule predicts.
#     Interactive HTML saved when labeled points exceed density threshold (50).
#
#   Plot 2 (s2_week8_sos_adjusted_efficiency.png):
#     Dumbbell chart: raw efficiency vs SOS-adjusted efficiency.
#     Faceted by position: QB (pass EPA/att), RB (rush EPA/att), WR_TE (rec EPA/target).
#     Story: Which players look better or worse once schedule difficulty is removed?
#     SOS adjustment: adjusted = raw - sos_opp_def_epa_per_play
#     Blue connector = player faced harder schedule (adjusted > raw).
#     Red connector  = player faced easier schedule (adjusted < raw).
#     Top 20 players per position by SOS-adjusted efficiency shown.
#
# Output directory: output/plots/
# Season scope: 2014-2025 (full 12 seasons)
# ==============================================================================

library(ggplot2)
library(dplyr)
library(glue)
library(here)
library(purrr)

if (!requireNamespace("ggrepel", quietly = TRUE)) {
  stop("Package 'ggrepel' is required. Install with: install.packages('ggrepel')")
}
library(ggrepel)

if (!requireNamespace("plotly", quietly = TRUE)) {
  stop("Package 'plotly' is required. Install with: install.packages('plotly')")
}
if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
  stop("Package 'htmlwidgets' is required. Install with: install.packages('htmlwidgets')")
}
library(plotly)
library(htmlwidgets)

source(here::here("R", "22_sos_reconciliation.R"))


# ==============================================================================
# CONFIGURATION
# ==============================================================================

OUTPUT_DIR <- here::here("output", "plots")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

MIN_QB_ATTEMPTS <- 100L
MIN_RB_ATTEMPTS <- 50L
MIN_WR_TARGETS  <- 30L
TOP_N_PER_POS   <- 20L
DENSITY_THRESHOLD <- 50L

# cfbfastR play text bleeds into player name field -- filter these out
BAD_NAME_PATTERN <- "(?i)(shotgun|huddle|incomplete|#\\d|^no |^o huddle)"

theme_w8 <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11, color = "gray40"),
    plot.caption     = element_text(size = 9,  color = "gray50"),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 12)
  )


# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading SOS panel...\n")

sos_path <- here::here("data", "season2_cfb_cache", "s2_week8_cfb_sos_panel.rds")
ex_path  <- here::here("data", "season2_cfb_cache", "s2_week8_cfb_sos_panel_example.rds")

if (file.exists(sos_path)) {
  panel_sos <- readRDS(sos_path)
  cat(glue("Full SOS panel: {format(nrow(panel_sos), big.mark=',')} rows\n"))
} else if (file.exists(ex_path)) {
  panel_sos <- readRDS(ex_path)
  cat(glue("Example SOS panel (subset): {format(nrow(panel_sos), big.mark=',')} rows\n"))
} else {
  stop(glue(
    "No SOS panel found. Run example_season2_week8.R first.\n",
    "Expected: {sos_path}"
  ))
}


# ==============================================================================
# PLOT 1: QB EPA per attempt vs schedule difficulty (scatter)
# ==============================================================================

cat("\nBuilding Plot 1: QB efficiency vs schedule difficulty...\n")

plot1_data <- panel_sos %>%
  dplyr::filter(
    position_group == "QB",
    !low_volume,
    sos_computed,
    sos_n_opponents >= 6L,
    pass_attempts >= MIN_QB_ATTEMPTS,
    !is.na(pass_epa_per_attempt),
    !is.na(sos_opp_def_epa_per_play),
    !grepl(BAD_NAME_PATTERN, player_name, perl = TRUE)
  ) %>%
  dplyr::mutate(season_chr = as.character(season))

n_qb <- nrow(plot1_data)
cat(glue("  QB points after filters: {n_qb}\n"))

if (n_qb == 0L) {
  cat("  No QB data meets thresholds. Skipping Plot 1.\n")
} else {

  lm_fit <- lm(pass_epa_per_attempt ~ sos_opp_def_epa_per_play, data = plot1_data)
  r2_val <- round(summary(lm_fit)$r.squared, 3)

  plot1_data <- plot1_data %>%
    dplyr::mutate(
      fitted   = predict(lm_fit),
      residual = pass_epa_per_attempt - fitted,
      is_label = abs(residual) > sd(residual, na.rm = TRUE)
    )

  p1 <- ggplot(
    plot1_data,
    aes(x = sos_opp_def_epa_per_play, y = pass_epa_per_attempt)
  ) +
    geom_smooth(method = "lm", se = TRUE, color = "gray50",
                fill = "gray85", linewidth = 0.8) +
    geom_point(aes(color = season_chr), alpha = 0.65, size = 2.2) +
    geom_text_repel(
      data  = dplyr::filter(plot1_data, is_label),
      aes(label = glue("{player_name} ({season})")),
      size  = 2.8, max.overlaps = 12, color = "gray20"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "gray60", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray60", linewidth = 0.4) +
    scale_color_viridis_d(name = "Season", option = "D") +
    labs(
      title    = "QB Efficiency vs Schedule Difficulty",
      subtitle = glue(
        "Pass EPA per attempt vs mean opponent defensive EPA allowed | ",
        "Min {MIN_QB_ATTEMPTS} attempts | R\u00b2 = {r2_val}"
      ),
      caption  = glue(
        "Points above the regression line outperformed their schedule difficulty. ",
        "Labeled: QBs > 1 SD from fitted line (n={sum(plot1_data$is_label)})."
      ),
      x = "Opponent Def EPA Allowed / Play (SOS -- higher = easier schedule)",
      y = "Pass EPA per Attempt"
    ) +
    theme_w8

  p1_path <- file.path(OUTPUT_DIR, "s2_week8_qb_sos_vs_efficiency.png")
  ggsave(filename = p1_path, plot = p1, width = 10, height = 7, dpi = 150)
  cat(glue("  Saved: {p1_path}\n"))

  if (n_qb >= DENSITY_THRESHOLD) {
    cat(glue("  {n_qb} points -- saving interactive HTML...\n"))

    p1_interactive <- plot_ly(
      data      = plot1_data,
      x         = ~sos_opp_def_epa_per_play,
      y         = ~pass_epa_per_attempt,
      type      = "scatter",
      mode      = "markers",
      color     = ~season_chr,
      colors    = "viridis",
      text      = ~glue(
        "<b>{player_name}</b><br>",
        "Season: {season}<br>",
        "Team: {primary_team}<br>",
        "Pass EPA/att: {round(pass_epa_per_attempt, 3)}<br>",
        "Opp Def EPA: {round(sos_opp_def_epa_per_play, 3)}<br>",
        "Opponents: {sos_n_opponents}<br>",
        "Pass attempts: {format(pass_attempts, big.mark=',')}"
      ),
      hoverinfo = "text",
      marker    = list(size = 7, opacity = 0.7)
    ) %>%
      layout(
        title  = list(text = "QB Efficiency vs Schedule Difficulty (Interactive)"),
        xaxis  = list(title = "Opponent Def EPA Allowed / Play (SOS)"),
        yaxis  = list(title = "Pass EPA per Attempt"),
        legend = list(title = list(text = "Season"))
      )

    html_path <- file.path(OUTPUT_DIR,
                           "s2_week8_qb_sos_vs_efficiency_interactive.html")
    saveWidget(p1_interactive, file = html_path, selfcontained = TRUE)
    cat(glue("  Saved interactive: {html_path}\n"))
  }
}


# ==============================================================================
# PLOT 2: Dumbbell -- raw vs SOS-adjusted efficiency, three position facets
# ==============================================================================

cat("\nBuilding Plot 2: SOS-adjusted efficiency dumbbell...\n")

# SOS adjustment formula:
#   adjusted = raw_efficiency - sos_opp_def_epa_per_play
# Higher opponent EPA allowed = weaker defense = easier schedule.
# Subtracting penalizes players who faced weak defenses and
# rewards those who faced strong ones.

pos_specs <- list(
  QB    = list(
    metric  = "pass_epa_per_attempt",
    vol_col = "pass_attempts",
    min_vol = MIN_QB_ATTEMPTS,
    label   = "QB\nPass EPA / Attempt"
  ),
  RB    = list(
    metric  = "rush_epa_per_attempt",
    vol_col = "rush_attempts",
    min_vol = MIN_RB_ATTEMPTS,
    label   = "RB\nRush EPA / Attempt"
  ),
  WR_TE = list(
    metric  = "rec_epa_per_target",
    vol_col = "targets",
    min_vol = MIN_WR_TARGETS,
    label   = "WR / TE\nRec EPA / Target"
  )
)

dumbbell_list <- purrr::map(names(pos_specs), function(pos) {
  spec <- pos_specs[[pos]]

  df <- panel_sos %>%
    dplyr::filter(
      position_group == pos,
      !low_volume,
      sos_computed,
      sos_n_opponents >= 6L,
      !is.na(.data[[spec$metric]]),
      !is.na(sos_opp_def_epa_per_play),
      .data[[spec$vol_col]] >= spec$min_vol,
      !grepl(BAD_NAME_PATTERN, player_name, perl = TRUE)
    ) %>%
    dplyr::mutate(
      raw_efficiency = .data[[spec$metric]],
      sos_adjusted   = raw_efficiency - sos_opp_def_epa_per_play,
      player_label   = glue("{player_name} ({season})")
    ) %>%
    # Best season per player by SOS-adjusted efficiency to avoid repetition
    dplyr::group_by(player_name) %>%
    dplyr::slice_max(order_by = sos_adjusted, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(order_by = sos_adjusted, n = TOP_N_PER_POS,
                     with_ties = FALSE) %>%
    dplyr::mutate(
      pos_label  = spec$label,
      moved_up   = sos_adjusted >= raw_efficiency,
      # Worst-to-best so ggplot renders best at top of each facet
      player_label = factor(
        player_label,
        levels = player_label[order(sos_adjusted)]
      )
    )

  df
})

plot2_data <- dplyr::bind_rows(dumbbell_list) %>%
  dplyr::mutate(
    pos_label = factor(pos_label, levels = c(
      pos_specs$QB$label,
      pos_specs$RB$label,
      pos_specs$WR_TE$label
    ))
  )

if (nrow(plot2_data) == 0L) {
  cat("  No data meets dumbbell thresholds. Skipping Plot 2.\n")
} else {
  n_by_pos <- table(plot2_data$pos_label)
  cat(glue(
    "  Dumbbell rows by position: ",
    "{paste(names(n_by_pos), n_by_pos, sep='=', collapse=' | ')}\n"
  ))

  p2 <- ggplot(plot2_data) +
    geom_segment(
      aes(
        x     = raw_efficiency,
        xend  = sos_adjusted,
        y     = player_label,
        yend  = player_label,
        color = moved_up
      ),
      linewidth = 0.9, alpha = 0.75
    ) +
    # Open dot = raw efficiency
    geom_point(
      aes(x = raw_efficiency, y = player_label),
      shape = 21, fill = "white", color = "gray40", size = 2.8
    ) +
    # Filled dot = SOS-adjusted efficiency
    geom_point(
      aes(x = sos_adjusted, y = player_label, color = moved_up),
      size = 3.2
    ) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray60", linewidth = 0.4) +
    scale_color_manual(
      values = c("TRUE" = "#2166ac", "FALSE" = "#d73027"),
      labels = c(
        "TRUE"  = "Faced harder schedule (adjusted up)",
        "FALSE" = "Faced easier schedule (adjusted down)"
      ),
      name = NULL
    ) +
    facet_wrap(~ pos_label, scales = "free_y", ncol = 3L) +
    labs(
      title    = "CFB Player Efficiency: Raw vs Schedule-Adjusted",
      subtitle = glue(
        "Top {TOP_N_PER_POS} per position by SOS-adjusted efficiency | ",
        "Seasons 2014-2025 | Best season per player shown\n",
        "Open dot = raw efficiency | Filled dot = adjusted for opponent defense quality"
      ),
      caption  = glue(
        "Adjustment: SOS-adjusted = raw EPA metric minus mean opponent defensive EPA allowed. ",
        "Min attempts: QB {MIN_QB_ATTEMPTS}, RB {MIN_RB_ATTEMPTS}, WR/TE {MIN_WR_TARGETS}."
      ),
      x = "EPA per Attempt / Target",
      y = NULL
    ) +
    theme_w8 +
    theme(
      legend.position = "bottom",
      axis.text.y     = element_text(size = 8),
      panel.spacing   = unit(1.5, "lines")
    )

  p2_path <- file.path(OUTPUT_DIR, "s2_week8_sos_adjusted_efficiency.png")
  ggsave(filename = p2_path, plot = p2, width = 14, height = 10, dpi = 150)
  cat(glue("  Saved: {p2_path}\n"))
}


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n=== Week 8 Visuals Complete ===\n")
cat(glue("Output directory: {OUTPUT_DIR}\n"))
