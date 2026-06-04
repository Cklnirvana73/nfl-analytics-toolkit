# ==============================================================================
# Season 2 Week 16 -- Start/Sit Uncertainty Visuals (R/36)
# File: visuals/create_season2_week16_visual_r36.R
# ==============================================================================
#
# WHAT THIS DOES
# --------------
# Two publication-quality plots from the R/36 output:
#
#   Plot 1  s2_week16_r36_decision_map.png
#           The start/sit decision map. Each started slot placed by P(start is
#           correct) against expected regret, colored by stakes. This is the
#           graded view the R/35 binary close/confident flag cannot give: it
#           shows probability and magnitude together, so the genuinely risky
#           starts (low probability AND high regret) separate from the rest.
#
#   Plot 2  s2_week16_r36_def_streaming.png
#           The DEF streaming shortlist. Top available defenses by
#           matchup-adjusted projection, colored by pick clarity. Near-equal
#           bar heights are the point: when the projections cluster, streaming
#           is a coin flip and the matchup is the tiebreaker.
#
# Both are static PNG at 300 dpi. Under the project's interactive standard, an
# HTML version is only warranted at 50+ labeled points; with 8 starters and a
# 5-defense shortlist, static is the right call.
#
# INPUTS (written by the R/36 example with save_output = TRUE)
# ------------------------------------------------------------
#   data/season2_cache/s2_week16_r36_start_sit_uq.rds    (required)
#   data/season2_cache/s2_week16_r36_def_streaming.rds   (optional; Plot 2)
#
# RUN
# ---
#   source(here::here("visuals", "create_season2_week16_visual_r36.R"))
# ==============================================================================

library(here)
library(dplyr)
library(ggplot2)
library(scales)

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

SEASON     <- 2026L
WEEK       <- 6L
DPI        <- 300

CACHE_DIR  <- here::here("data", "season2_cache")
UQ_PATH    <- file.path(CACHE_DIR, "s2_week16_r36_start_sit_uq.rds")
DEF_PATH   <- file.path(CACHE_DIR, "s2_week16_r36_def_streaming.rds")

OUTPUT_DIR <- here::here("output", "plots")
PLOT1_PATH <- file.path(OUTPUT_DIR, "s2_week16_r36_decision_map.png")
PLOT2_PATH <- file.path(OUTPUT_DIR, "s2_week16_r36_def_streaming.png")
PLOT3_PATH <- file.path(OUTPUT_DIR, "s2_week16_r36_probability_spread.png")

CAPTION <- "Data: nflfastR + Sleeper | Analysis: NFL Analytics Toolkit"

# Okabe-Ito colorblind-safe palette (visualization-patterns.md). Stakes are
# ordered low -> high, so the ramp goes cool (calm) to warm (alarm).
STAKES_COLORS <- c(low = "#0072B2", medium = "#E69F00", high = "#D55E00")
# Pick clarity ordered tossup -> clear: clear is the confident "go" (green),
# tossup the neutral gray.
CLARITY_COLORS <- c("clear" = "#009E73", "slight edge" = "#E69F00",
                    "tossup" = "#999999")

# ------------------------------------------------------------------------------
# LOAD
# ------------------------------------------------------------------------------

if (!file.exists(UQ_PATH)) {
  stop(glue::glue(
    "Missing {UQ_PATH}. Run the R/36 example with save_output = TRUE first."),
    call. = FALSE)
}
starters <- readRDS(UQ_PATH)
def_streaming <- if (file.exists(DEF_PATH)) readRDS(DEF_PATH) else NULL

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ==============================================================================
# PLOT 1: START/SIT DECISION MAP
# ==============================================================================
# Question answered: "Which starts are both uncertain and costly to get wrong?"
# Scored slots only (a no_alt slot has no decision to plot).

scored <- starters %>%
  dplyr::filter(!is.na(.data$p_start_correct),
                !is.na(.data$expected_regret)) %>%
  dplyr::mutate(stakes = factor(.data$stakes,
                                levels = c("low", "medium", "high")))

p1 <- ggplot(scored,
             aes(x = .data$p_start_correct, y = .data$expected_regret,
                 color = .data$stakes)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60") +
  annotate("text", x = 0.5, y = max(scored$expected_regret) * 1.02,
           label = "coin flip", hjust = -0.05, vjust = 1,
           color = "grey50", size = 3) +
  geom_point(size = 4, alpha = 0.85) +
  geom_text(aes(label = .data$player_name), vjust = -0.9, size = 3,
            color = "grey20", check_overlap = TRUE) +
  scale_color_manual(values = STAKES_COLORS, drop = TRUE, name = "Stakes") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(min(scored$p_start_correct) - 0.05, 1)) +
  scale_y_continuous(limits = c(0, max(scored$expected_regret) * 1.15)) +
  labs(
    title = "Start/Sit Decision Confidence (Week 16)",
    subtitle = paste("Each started slot by probability the start is correct",
                     "and expected points at risk if it is not"),
    x = "P(started player outscores best alternative)",
    y = "Expected regret (points)",
    caption = CAPTION
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey50", size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

ggsave(PLOT1_PATH, p1, width = 10, height = 6, dpi = DPI)
message(glue::glue("  Saved: {PLOT1_PATH}"))

# ==============================================================================
# PLOT 2: DEF STREAMING SHORTLIST
# ==============================================================================
# Question answered: "Which defense do I stream, and is the choice clear-cut?"

if (!is.null(def_streaming) && nrow(def_streaming) > 0L) {

  def_plot <- def_streaming %>%
    dplyr::mutate(
      # The bottom-ranked defense has no next option to compare against, so it
      # is a tossup like the rest. Fill it as one rather than as a phantom NA
      # color that would read as meaningfully worse.
      pick_clarity = factor(dplyr::coalesce(.data$pick_clarity, "tossup"),
                            levels = c("clear", "slight edge", "tossup")),
      bar_label = ifelse(
        is.na(.data$opponent),
        format(round(.data$adj_proj, 1), nsmall = 1),
        paste0("vs ", .data$opponent, "   ",
               format(round(.data$adj_proj, 1), nsmall = 1)))
    )

  p2 <- ggplot(def_plot,
               aes(x = stats::reorder(.data$def_team, .data$adj_proj),
                   y = .data$adj_proj, fill = .data$pick_clarity)) +
    geom_col(width = 0.7, alpha = 0.95) +
    geom_text(aes(label = .data$bar_label), hjust = -0.07, size = 3.4,
              color = "grey20") +
    coord_flip() +
    scale_fill_manual(values = CLARITY_COLORS, drop = TRUE,
                      name = "Pick clarity") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(
      title = "DEF Streaming Shortlist (Week 16)",
      subtitle = paste("Top available defenses by matchup-adjusted projection;",
                       "near-equal bars mean the choice is a tossup"),
      x = NULL,
      y = "Matchup-adjusted projected points",
      caption = CAPTION
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "grey40", size = 10),
      plot.caption = element_text(color = "grey50", size = 8),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )

  ggsave(PLOT2_PATH, p2, width = 10, height = 5, dpi = DPI)
  message(glue::glue("  Saved: {PLOT2_PATH}"))
} else {
  message("  No def_streaming artifact found; skipping Plot 2. ",
          "Run the example with save_output = TRUE and an open DEF slot.")
}

# ==============================================================================
# PLOT 3: PROBABILITY SPREAD (the binary-to-graded upgrade story)
# ==============================================================================
# Question answered: "R/35 flagged all 8 slots 'close' -- what does that
# actually mean, quantitatively?"
# A lollipop from the coin-flip line to each slot's P(start correct), ordered
# by probability, colored by stakes. The point: 'close' spans 58-89%; they are
# not the same decision.

player_levels <- scored %>%
  dplyr::arrange(.data$p_start_correct) %>%
  dplyr::pull(.data$player_name)

p3_data <- scored %>%
  dplyr::mutate(player_name = factor(.data$player_name, levels = player_levels))

p3 <- ggplot(p3_data,
             aes(y = .data$player_name, x = .data$p_start_correct,
                 color = .data$stakes)) +
  geom_vline(xintercept = 0.50, linetype = "dashed", color = "grey60",
             alpha = 0.8) +
  geom_vline(xintercept = 0.75, linetype = "dotted", color = "grey70",
             alpha = 0.8) +
  geom_segment(aes(xend = .data$p_start_correct,
                   y    = .data$player_name,
                   yend = .data$player_name),
               x = 0.50, linewidth = 1, alpha = 0.6) +
  geom_point(size = 4.5) +
  scale_color_manual(values = STAKES_COLORS, drop = TRUE, name = "Stakes") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.40, 1.0)) +
  labs(
    title = "Probability Spread Across Started Slots (Week 16)",
    subtitle = paste("All 8 slots flagged 'close' by R/35 binary overlap;",
                     "R/36 reveals they range from 58% to 89%",
                     "-- dashed = coin flip, dotted = 75%"),
    x = "P(started player outscores best alternative)",
    y = NULL,
    caption = CAPTION
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "right"
  )

ggsave(PLOT3_PATH, p3, width = 10, height = 6, dpi = DPI)
message(glue::glue("  Saved: {PLOT3_PATH}"))

# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

n_scored <- sum(!is.na(starters$p_start_correct))
week_risk <- sum(starters$expected_regret, na.rm = TRUE)

cat("\n")
cat("==============================================================\n")
cat("  Season 2 Week 16 Visualization Summary (R/36)\n")
cat("==============================================================\n")
cat("  Plot 1: s2_week16_r36_decision_map.png\n")
cat("          start/sit decision map (probability vs expected regret)\n")
if (!is.null(def_streaming) && nrow(def_streaming) > 0L) {
  cat("  Plot 2: s2_week16_r36_def_streaming.png\n")
  cat("          DEF streaming shortlist (projection x pick clarity)\n")
} else {
  cat("  Plot 2: skipped (no def_streaming artifact)\n")
}
cat("  Plot 3: s2_week16_r36_probability_spread.png\n")
cat("          probability spread (binary 'close' vs graded R/36 view)\n")
cat("  Output: output/plots/\n")
cat("  Resolution:", DPI, "dpi (static; under the 50-point interactive bar)\n")
cat("  Slots scored:", n_scored, "of", nrow(starters), "\n")
cat("  Week risk score:", format(round(week_risk, 1), nsmall = 1),
    "pts at risk\n")
cat("==============================================================\n")
