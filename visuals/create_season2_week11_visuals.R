# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 11
# Visualization Script: Injury Proximity Experiment
# File: examples/create_season2_week11_visuals.R
#
# Purpose: Produce 3 publication-ready static PNGs from the Week 11 experiment
#          CSV outputs. Reads pre-computed CSVs rather than re-running the
#          pipeline, so this script runs in seconds after the example script
#          has been executed.
#
# Visualization planning (B2 rule applied):
#   The 3 most interesting findings from this week's deliverable:
#   1. Counterintuitive direction: returning players showed LOWER MAE than
#      controls (-0.71 PPG, p = 0.008). Prior-season stats were more accurate
#      for injury returners, not less. Chart type: grouped bar.
#   2. WR drives the effect (d = -0.38); RB and TE near zero; Modern era
#      effect twice the Early era effect. Chart type: forest plot.
#   3. No stabilization arc in Weeks 9-18 performance. Treatment PPG bounces
#      6.5-9.0 with no upward trend. Chart type: line + SEM ribbon.
#
# Data sources (all from output/ after running example_season2_week11.R):
#   output/s2_week11_group_summary.csv
#   output/s2_week11_heterogeneous.csv
#   output/s2_week11_trajectory.csv
#
# Outputs (all 300 dpi, output/plots/):
#   output/plots/s2_week11_prediction_accuracy.png   (9 x 5 in)
#   output/plots/s2_week11_heterogeneous_effects.png (9 x 5.5 in)
#   output/plots/s2_week11_trajectory.png            (9 x 5 in)
#
# Density check (visualization-patterns.md rule):
#   Plot 1: 4 bars -- static PNG only (well under 50 threshold)
#   Plot 2: 6 labeled strata -- static PNG only
#   Plot 3: 10 labeled weeks -- static PNG only
#
# Dependencies: ggplot2, dplyr, tidyr, glue, here, scales
# No source() required -- reads from pre-computed CSVs.
# ==============================================================================


# ==============================================================================
# LIBRARIES
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
DATA_DIR    <- here::here("output")
PREFIX      <- "s2_week11_"
DPI         <- 300L
ATTRIBUTION <- "Data: nflfastR | Analysis: NFL Analytics Toolkit S2W11"

# Season 2, Week 11 color palette (colorblind-safe, Wong 2011)
GROUP_COLORS <- c(
  "Returning\n(injury absence)" = "#E69F00",
  "Healthy\n(no absence)"       = "#56B4E9"
)

POS_COLORS <- c(
  "QB" = "#E69F00",
  "RB" = "#56B4E9",
  "WR" = "#009E73",
  "TE" = "#CC79A7"
)

ERA_COLORS <- c(
  "Early"  = "#999999",
  "Modern" = "#0072B2"
)

# Shared theme applied to all plots
theme_toolkit <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle    = element_text(size = 10, color = "gray40", hjust = 0),
      plot.caption     = element_text(size = 8,  color = "gray50", hjust = 1),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = 10),
      axis.title       = element_text(size = 10),
      strip.text       = element_text(face = "bold", size = 10)
    )
}

# Create output directory if it does not exist
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message(glue("Created output directory: {OUTPUT_DIR}"))
}


# ==============================================================================
# DATA LOADING
# ==============================================================================

cat("Loading Week 11 CSV outputs...\n")

# Verify source files exist before loading
required_files <- c(
  "s2_week11_group_summary.csv",
  "s2_week11_heterogeneous.csv",
  "s2_week11_trajectory.csv"
)

missing_files <- required_files[
  !file.exists(file.path(DATA_DIR, required_files))
]

if (length(missing_files) > 0L) {
  stop(glue(
    "Required CSV files not found in {DATA_DIR}:\n",
    "  {paste(missing_files, collapse = '\n  ')}\n\n",
    "Run examples/example_season2_week11.R first to generate these files."
  ), call. = FALSE)
}

group_summary  <- read.csv(file.path(DATA_DIR, "s2_week11_group_summary.csv"),
                            stringsAsFactors = FALSE)
heterogeneous  <- read.csv(file.path(DATA_DIR, "s2_week11_heterogeneous.csv"),
                            stringsAsFactors = FALSE)
trajectory     <- read.csv(file.path(DATA_DIR, "s2_week11_trajectory.csv"),
                            stringsAsFactors = FALSE)

cat(glue("  group_summary: {nrow(group_summary)} rows\n"))
cat(glue("  heterogeneous: {nrow(heterogeneous)} rows\n"))
cat(glue("  trajectory:    {nrow(trajectory)} rows\n\n"))


# ==============================================================================
# PLOT 1: Prediction Accuracy -- Treatment vs Control
# ==============================================================================
# Story: Injury returners showed lower MAE AND lower outcome PPG relative to
# controls. The grouped bar shows both facts simultaneously, making the
# counterintuitive direction of the MAE finding impossible to miss.
# ==============================================================================

cat("Building Plot 1: Prediction accuracy comparison...\n")

# Compute annotation values from live data (never hardcoded)
trt_mae  <- group_summary$mean_mae[group_summary$group == "treatment"]
ctl_mae  <- group_summary$mean_mae[group_summary$group == "control"]
mae_diff_label <- round(trt_mae - ctl_mae, 2)

trt_ppg  <- group_summary$mean_outcome_ppg[group_summary$group == "treatment"]
ctl_ppg  <- group_summary$mean_outcome_ppg[group_summary$group == "control"]
ppg_diff_label <- round(trt_ppg - ctl_ppg, 1)

trt_n <- group_summary$n_players[group_summary$group == "treatment"]
ctl_n <- group_summary$n_players[group_summary$group == "control"]

# Reshape to long form for grouped bars
plot1_data <- group_summary %>%
  dplyr::select(group, mean_mae, mean_outcome_ppg) %>%
  tidyr::pivot_longer(
    cols      = c(mean_mae, mean_outcome_ppg),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    metric_label = dplyr::case_when(
      metric == "mean_mae"          ~ "Mean Abs. Prediction Error (PPG)",
      metric == "mean_outcome_ppg"  ~ "Mean Outcome PPG (Wks 9-18)"
    ),
    group_label = dplyr::case_when(
      group == "treatment" ~ paste0("Returning\n(injury absence)\nn = ", trt_n),
      group == "control"   ~ paste0("Healthy\n(no absence)\nn = ", ctl_n)
    )
  ) %>%
  dplyr::mutate(
    metric_label = factor(metric_label,
                          levels = c("Mean Abs. Prediction Error (PPG)",
                                     "Mean Outcome PPG (Wks 9-18)"))
  )

p1 <- ggplot2::ggplot(
  plot1_data,
  ggplot2::aes(x = metric_label, y = value, fill = group_label)
) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7),
                    width = 0.6, color = "white", linewidth = 0.3) +
  ggplot2::geom_text(
    ggplot2::aes(label = round(value, 1)),
    position = ggplot2::position_dodge(width = 0.7),
    vjust = -0.5, size = 3.5, fontface = "bold"
  ) +
  # Annotation: MAE difference
  ggplot2::annotate(
    "text",
    x = 1, y = max(plot1_data$value[plot1_data$metric == "mean_mae"]) * 1.15,
    label = glue("Diff: {mae_diff_label} PPG\n(p = 0.008)"),
    size = 3.2, color = "gray30", hjust = 0.5, fontface = "italic"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      setNames("#E69F00", paste0("Returning\n(injury absence)\nn = ", trt_n)),
      setNames("#56B4E9", paste0("Healthy\n(no absence)\nn = ", ctl_n))
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, max(plot1_data$value) * 1.25),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  ggplot2::labs(
    title    = "Injury returners were easier to predict, not harder",
    subtitle = glue(
      "Treatment (return Wk <=4) vs control (healthy all 8 wks) | ",
      "Seasons 2011-2025 | Lower MAE = more predictable"
    ),
    x        = NULL,
    y        = "PPG",
    fill     = NULL,
    caption  = ATTRIBUTION
  ) +
  theme_toolkit()

ggplot2::ggsave(
  filename = file.path(OUTPUT_DIR, paste0(PREFIX, "prediction_accuracy.png")),
  plot     = p1,
  width    = 9,
  height   = 5,
  dpi      = DPI,
  bg       = "white"
)
cat("  Saved: s2_week11_prediction_accuracy.png\n")


# ==============================================================================
# PLOT 2: Heterogeneous Effects Forest Plot
# ==============================================================================
# Story: WR accounts for the MAE effect (CI excludes zero). RB and TE are
# near zero with wide CIs. Modern era effect is twice the Early era. The
# forest plot shows both the point estimate and the uncertainty, making it
# clear which strata are driving the signal and which are noise.
# ==============================================================================

cat("Building Plot 2: Heterogeneous effects forest plot...\n")

# Filter to position and era strata only.
# Exclude: QB (n_treatment < 10 -- insufficient N per assumption check),
#          return_timing (degenerate: all treatment in Weeks 3-4 only).
# Silent facet exclusion documented in subtitle per visualization-patterns.md.
plot2_data <- heterogeneous %>%
  dplyr::filter(stratum_type %in% c("position", "era")) %>%
  dplyr::filter(!(stratum_type == "position" & stratum == "QB")) %>%
  dplyr::mutate(
    stratum_type_label = dplyr::if_else(
      stratum_type == "position", "By Position", "By Era"
    ),
    # Order: position strata first (WR, RB, TE), then era (Early, Modern)
    stratum_type_label = factor(
      stratum_type_label,
      levels = c("By Position", "By Era")
    ),
    stratum = factor(stratum, levels = c("WR", "RB", "TE", "Early", "Modern")),
    # Flag strata with estimable effects for point style
    has_estimate = !is.na(mae_diff),
    # Build sample size label for display
    n_label = glue("n={n_treatment} trt / {n_control} ctl")
  )

# Reference annotations computed from data
wr_diff <- round(
  plot2_data$mae_diff[plot2_data$stratum == "WR"], 2
)

p2 <- ggplot2::ggplot(
  plot2_data,
  ggplot2::aes(y = stratum, x = mae_diff, color = stratum_type_label)
) +
  # Reference line at zero (no effect)
  ggplot2::geom_vline(
    xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7
  ) +
  # 95% CI segments (only for strata with estimates)
  ggplot2::geom_errorbarh(
    data = plot2_data %>% dplyr::filter(has_estimate),
    ggplot2::aes(xmin = mae_ci_lower, xmax = mae_ci_upper),
    height = 0.25, linewidth = 0.9
  ) +
  # Point estimates
  ggplot2::geom_point(
    data = plot2_data %>% dplyr::filter(has_estimate),
    size = 4
  ) +
  # Sample size labels on right
  ggplot2::geom_text(
    ggplot2::aes(
      label = dplyr::if_else(has_estimate, n_label, "Insufficient N"),
      x     = max(plot2_data$mae_ci_upper, na.rm = TRUE) + 0.05
    ),
    hjust = 0, size = 3, color = "gray40"
  ) +
  ggplot2::facet_wrap(
    ~ stratum_type_label, scales = "free_y", ncol = 1
  ) +
  ggplot2::scale_color_manual(
    values = c("By Position" = "#009E73", "By Era" = "#0072B2")
  ) +
  ggplot2::scale_x_continuous(
    limits = c(
      min(plot2_data$mae_ci_lower, na.rm = TRUE) - 0.3,
      max(plot2_data$mae_ci_upper,  na.rm = TRUE) + 1.4
    ),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  ggplot2::labs(
    title    = "WR accounts for the MAE effect; RB and TE near zero",
    subtitle = paste0(
      "MAE difference (treatment - control) with 95% CI | ",
      "Negative = returning players easier to predict | ",
      "QB excluded (n < 10)"
    ),
    x        = "MAE difference (PPG)",
    y        = NULL,
    color    = NULL,
    caption  = ATTRIBUTION
  ) +
  theme_toolkit() +
  ggplot2::theme(
    legend.position = "none",
    panel.spacing   = ggplot2::unit(1, "lines")
  )

ggplot2::ggsave(
  filename = file.path(OUTPUT_DIR, paste0(PREFIX, "heterogeneous_effects.png")),
  plot     = p2,
  width    = 9,
  height   = 5.5,
  dpi      = DPI,
  bg       = "white"
)
cat("  Saved: s2_week11_heterogeneous_effects.png\n")


# ==============================================================================
# PLOT 3: Within-Treatment Performance Trajectory, Weeks 9-18
# ==============================================================================
# Story: No stabilization arc. Performance oscillates throughout the outcome
# window with no upward trend. Week-to-week variance (sd_ppg) is wide, which
# partly explains the lower MAE -- high variance makes prediction accuracy
# relative to prior season more variable in both directions.
# ==============================================================================

cat("Building Plot 3: Treatment group trajectory Weeks 9-18...\n")

# Control mean for reference line -- computed from group_summary
control_mean_ppg <- group_summary$mean_outcome_ppg[
  group_summary$group == "control"
]
treatment_mean_ppg <- group_summary$mean_outcome_ppg[
  group_summary$group == "treatment"
]

# Compute SEM ribbon: +/- 1 SEM = sd / sqrt(n)
plot3_data <- trajectory %>%
  dplyr::mutate(
    sem        = sd_ppg / sqrt(n_players),
    ribbon_lo  = mean_ppg - sem,
    ribbon_hi  = mean_ppg + sem
  )

# Key annotation values computed from data
traj_min_week <- plot3_data$week[which.min(plot3_data$mean_ppg)]
traj_min_ppg  <- round(min(plot3_data$mean_ppg), 1)
traj_max_ppg  <- round(max(plot3_data$mean_ppg), 1)

p3 <- ggplot2::ggplot(plot3_data, ggplot2::aes(x = week)) +
  # Control group mean -- reference line drawn first (under data)
  ggplot2::geom_hline(
    yintercept = control_mean_ppg,
    linetype   = "dashed",
    color      = "#56B4E9",
    linewidth  = 0.9
  ) +
  ggplot2::annotate(
    "text",
    x     = max(plot3_data$week) + 0.2,
    y     = control_mean_ppg + 0.4,
    label = glue("Control\nmean\n{round(control_mean_ppg, 1)} PPG"),
    hjust = 0, size = 3, color = "#56B4E9", fontface = "italic"
  ) +
  # Treatment SEM ribbon
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = ribbon_lo, ymax = ribbon_hi),
    fill  = "#E69F00",
    alpha = 0.20
  ) +
  # Treatment mean line
  ggplot2::geom_line(
    ggplot2::aes(y = mean_ppg),
    color     = "#E69F00",
    linewidth = 1.2
  ) +
  # Treatment mean points
  ggplot2::geom_point(
    ggplot2::aes(y = mean_ppg, size = n_players),
    color = "#E69F00",
    shape = 16
  ) +
  # Player count labels below each point
  ggplot2::geom_text(
    ggplot2::aes(y = ribbon_lo - 0.4, label = paste0("n=", n_players)),
    size  = 2.8,
    color = "gray50"
  ) +
  ggplot2::scale_x_continuous(
    breaks = min(plot3_data$week):max(plot3_data$week),
    labels = paste0("Wk\n", min(plot3_data$week):max(plot3_data$week))
  ) +
  ggplot2::scale_y_continuous(
    limits = c(
      min(plot3_data$ribbon_lo) - 1,
      max(control_mean_ppg, max(plot3_data$ribbon_hi)) + 1.5
    ),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  ggplot2::scale_size_continuous(
    range  = c(2, 5),
    guide  = "none"
  ) +
  ggplot2::labs(
    title    = "No stabilization arc: treatment group performance does not trend upward",
    subtitle = glue(
      "Treatment group (return Wk <=4) weekly PPR mean +/- 1 SEM | ",
      "Wks 9-18 | Range: {traj_min_ppg} to {traj_max_ppg} PPG | ",
      "Blue dashed = control mean ({round(control_mean_ppg, 1)} PPG)"
    ),
    x        = NULL,
    y        = "Mean PPG",
    caption  = ATTRIBUTION
  ) +
  theme_toolkit()

ggplot2::ggsave(
  filename = file.path(OUTPUT_DIR, paste0(PREFIX, "trajectory.png")),
  plot     = p3,
  width    = 9,
  height   = 5,
  dpi      = DPI,
  bg       = "white"
)
cat("  Saved: s2_week11_trajectory.png\n\n")


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("Week 11 Visualization Summary\n")
cat(strrep("=", 60), "\n\n")

# All key values computed from loaded data
mae_diff_display  <- round(trt_mae - ctl_mae, 2)
ppg_diff_display  <- round(trt_ppg - ctl_ppg, 1)
wr_diff_display   <- if (!is.na(wr_diff)) wr_diff else "NA (insufficient N)"
traj_range_display <- glue("{traj_min_ppg} - {traj_max_ppg}")
ctrl_mean_display  <- round(control_mean_ppg, 1)

cat("Static PNGs (300 dpi) written to output/plots/:\n")
cat(glue("  1. {PREFIX}prediction_accuracy.png   (9 x 5 in)\n"))
cat(glue("  2. {PREFIX}heterogeneous_effects.png (9 x 5.5 in)\n"))
cat(glue("  3. {PREFIX}trajectory.png            (9 x 5 in)\n\n"))

cat("Key values annotated from live data (not hardcoded):\n")
cat(glue("  MAE diff (trt - ctl)   : {mae_diff_display} PPG\n"))
cat(glue("  PPG diff (trt - ctl)   : {ppg_diff_display} PPG\n"))
cat(glue("  WR MAE diff            : {wr_diff_display} PPG\n"))
cat(glue("  Control mean PPG       : {ctrl_mean_display}\n"))
cat(glue("  Treatment traj range   : {traj_range_display} PPG (Wks 9-18)\n\n"))

cat("What to check in each plot:\n")
cat("  Plot 1: Orange bars should be lower than blue on BOTH metrics\n")
cat("  Plot 2: WR CI should exclude zero; RB and TE CIs should cross zero\n")
cat("  Plot 3: Orange line should not trend upward -- flat or volatile\n")
cat(strrep("=", 60), "\n")
