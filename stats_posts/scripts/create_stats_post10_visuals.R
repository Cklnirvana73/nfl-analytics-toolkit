# ==============================================================================
# create_stats_post10_visuals.R
# Statistics Series -- Post 10, Bayes' Theorem
# ==============================================================================
#
# PURPOSE
# -------
# Two-panel figure for Post 10:
#   Panel 1: The real precision-weighted blend for one demo player (QB,
#            3 NFL seasons). Shows the translation estimate, the history
#            estimate, and the blended result, each with its own uncertainty
#            (error bars = sigma). A red dashed reference line marks the
#            simple average of the two inputs, so the reader can see the
#            blend is NOT a midpoint -- it favors the more precise source.
#   Panel 2: The contrast mechanism. The fixed week-based prior weight
#            schedule (compute_prior_weight() from R/14, carried forward
#            unchanged into R/29) plotted across all 18 weeks. This is the
#            OTHER "prior/posterior" mechanism in the same codebase -- driven
#            entirely by calendar week, not by precision. The crossover point
#            (~week 9, where the schedule crosses 0.5) is marked for scale.
#
# DATA SOURCE
# -----------
# Panel 1 values are pulled directly from the verified console output of
# pull_stats_post10_bayes_demo_data.R (this session), player 00-0037834,
# QB, years_exp 3, n_nfl_seasons 3. Recomputed prior_mu/prior_sigma matched
# R/29's own output within tolerance in that script's self-check.
#
# Panel 2 values are computed directly from the R/14 formula (PRIOR_WEIGHT_START
# = 1.0, PRIOR_WEIGHT_FLOOR = 0.05, linear decay across weeks 1-18), which R/29
# carries forward unchanged. No pull needed -- this is a closed-form formula,
# not a data query.
#
# NAMESPACE NOTE
# ---------------
# This session already sourced 15/16/17/19/23/29 from the NFL toolkit into
# the global environment, which produced a confirmed collision earlier this
# session (purrr's %||% got masked by a global-environment function). Every
# ggplot2 call below is explicitly namespaced with ggplot2:: to eliminate the
# risk of a same-named helper elsewhere in the sourced toolkit (most likely
# candidate: R/23's aging-curve plotting code) silently intercepting a call
# meant for ggplot2.
#
# CONFIGURATION
# -------------

OUTPUT_DIR   <- "stats_posts/output/plots"
FILE_PREFIX  <- "stats_post10_"
DPI          <- 300
LIGHT_GRAY   <- "#F5F5F5"

NAVY   <- "#1B3A5C"   # actual / correct / final result
GRAY   <- "#9AA5B1"   # component inputs (neither naive nor final)
RED    <- "#C1443C"   # reference line (simple average, for contrast)

# Panel 1 demo values (pulled and self-verified this session)
MU_TRANSLATION    <- 8.618
SIGMA_TRANSLATION <- 6.655
MU_HISTORY        <- 16.517
SIGMA_HISTORY     <- 8.062
PRIOR_MU          <- 13.922
PRIOR_SIGMA       <- 3.814
SIMPLE_AVERAGE    <- (MU_TRANSLATION + MU_HISTORY) / 2

# Panel 2 schedule constants (from R/14, carried forward exactly into R/29)
PRIOR_WEIGHT_START <- 1.0
PRIOR_WEIGHT_FLOOR <- 0.05
NFL_REGULAR_SEASON_MAX_WEEK <- 18L

# ==============================================================================
# LIBRARIES
# ==============================================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Install with: install.packages('ggplot2')")
}
if (!requireNamespace("scales", quietly = TRUE)) {
  stop("Install with: install.packages('scales')")
}
library(dplyr)  # needed for %>% and dplyr::filter/slice/pull below

patchwork_available <- requireNamespace("patchwork", quietly = TRUE)
if (!patchwork_available) {
  message("patchwork not installed -- will save two separate PNGs instead ",
          "of one combined figure. Install with: install.packages('patchwork')")
}

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ==============================================================================
# PANEL 1: THE REAL PRECISION-WEIGHTED BLEND
# ==============================================================================

panel1_data <- tibble::tibble(
  label = factor(
    c("Translation\nmodel", "NFL\nhistory", "Blended\nestimate"),
    levels = c("Translation\nmodel", "NFL\nhistory", "Blended\nestimate")
  ),
  estimate = c(MU_TRANSLATION, MU_HISTORY, PRIOR_MU),
  sigma    = c(SIGMA_TRANSLATION, SIGMA_HISTORY, PRIOR_SIGMA),
  kind     = c("input", "input", "result")
)

p1 <- ggplot2::ggplot(panel1_data, ggplot2::aes(x = label, y = estimate, fill = kind)) +
  ggplot2::geom_hline(
    yintercept = SIMPLE_AVERAGE,
    linetype = "dashed", color = RED, linewidth = 0.7
  ) +
  ggplot2::geom_col(width = 0.55) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = estimate - sigma, ymax = estimate + sigma),
    width = 0.15, linewidth = 0.6, color = "gray30"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = round(estimate, 1)),
    vjust = -0.6, size = 4, fontface = "bold"
  ) +
  ggplot2::scale_fill_manual(values = c("input" = GRAY, "result" = NAVY), guide = "none") +
  ggplot2::coord_cartesian(ylim = c(0, 28), clip = "off") +
  ggplot2::labs(
    title = "The blend favors the more precise estimate, not the midpoint",
    subtitle = paste0(
      "One QB, 3rd NFL season. Error bars = sigma. ",
      "Red dashed line = simple average (", round(SIMPLE_AVERAGE, 1), ")"
    ),
    x = NULL,
    y = "Points per game"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 12.5),
    plot.subtitle = ggplot2::element_text(size = 10, color = "gray30"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank()
  )

# ==============================================================================
# PANEL 2: THE CONTRAST -- WEEK-BASED SCHEDULE, NOT PRECISION
# ==============================================================================

compute_prior_weight_demo <- function(week) {
  slope <- (PRIOR_WEIGHT_START - PRIOR_WEIGHT_FLOOR) /
             (NFL_REGULAR_SEASON_MAX_WEEK - 1)
  weight <- PRIOR_WEIGHT_START - slope * (week - 1)
  pmax(weight, PRIOR_WEIGHT_FLOOR)
}

panel2_data <- tibble::tibble(
  week = 1:18,
  prior_weight = compute_prior_weight_demo(1:18)
)

# Crossover: first week where prior_weight drops at/below 0.5
crossover_week <- panel2_data %>%
  dplyr::filter(prior_weight <= 0.5) %>%
  dplyr::slice(1) %>%
  dplyr::pull(week)

p2 <- ggplot2::ggplot(panel2_data, ggplot2::aes(x = week, y = prior_weight)) +
  ggplot2::geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray60") +
  ggplot2::geom_vline(
    xintercept = crossover_week, linetype = "dashed",
    color = RED, linewidth = 0.6
  ) +
  ggplot2::geom_line(color = NAVY, linewidth = 1.1) +
  ggplot2::geom_point(color = NAVY, size = 1.8) +
  ggplot2::annotate(
    "text", x = crossover_week + 0.4, y = 0.9, hjust = 0,
    label = paste0("crossover: week ", crossover_week),
    color = RED, size = 3.2, fontface = "italic"
  ) +
  ggplot2::scale_x_continuous(breaks = c(1, 5, 9, 13, 18)) +
  ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(
    title = "The other update: weight set by week number, not by confidence",
    subtitle = "Same file, one level up. Weight on the prior depends only on the calendar",
    x = "NFL week",
    y = "Weight on prior",
    caption = "NFL Analytics Toolkit | Statistics Series, Post 10"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 12.5),
    plot.subtitle = ggplot2::element_text(size = 10, color = "gray30"),
    panel.grid.minor = ggplot2::element_blank()
  )

# ==============================================================================
# COMBINE AND SAVE
# ==============================================================================

if (patchwork_available) {
  # Stack the two panels with wrap_plots(). Do NOT chain
  # `+ patchwork::plot_annotation(...)` onto the result: in this session's
  # load order that `+` was dispatching to ggplot2's method and folding
  # panel 2's continuous x-scale into panel 1's discrete x, throwing
  # "Discrete value supplied to a continuous scale". The figure-level caption
  # is instead carried on panel 2's own labs(caption = ...), which is a plain
  # string on a single ggplot and cannot merge scales.
  combined <- patchwork::wrap_plots(p1, p2, ncol = 1)

  out_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "bayes_theorem.png"))
  ggplot2::ggsave(out_path, combined, width = 7, height = 9, dpi = DPI, bg = "white")
  cat("Saved combined figure:", out_path, "\n")
} else {
  p1_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "panel1_precision_blend.png"))
  p2_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "panel2_week_schedule.png"))
  ggplot2::ggsave(p1_path, p1, width = 6, height = 5, dpi = DPI, bg = "white")
  ggplot2::ggsave(p2_path, p2, width = 6, height = 5, dpi = DPI, bg = "white")
  cat("Saved panel 1:", p1_path, "\n")
  cat("Saved panel 2:", p2_path, "\n")
}

# ---- CONSOLE SUMMARY ---------------------------------------
cat("\n--- POST 10 VISUAL SUMMARY ---\n")
cat("Panel 1: Precision-weighted blend (gray inputs, navy result, red dashed = simple average)\n")
cat("  Translation model :", MU_TRANSLATION, " (sigma", SIGMA_TRANSLATION, ")\n")
cat("  NFL history       :", MU_HISTORY,     " (sigma", SIGMA_HISTORY, ")\n")
cat("  Simple average     :", round(SIMPLE_AVERAGE, 3), "\n")
cat("  Blended estimate   :", PRIOR_MU,       " (sigma", PRIOR_SIGMA, ")\n")
cat("\nPanel 2: Week-based prior weight schedule (navy line, red dashed = crossover)\n")
cat("  Week 1 weight  :", round(compute_prior_weight_demo(1), 3), "\n")
cat("  Crossover week :", crossover_week, "\n")
cat("  Week 18 weight :", round(compute_prior_weight_demo(18), 3), "\n")
