# ==============================================================================
# create_stats_post13_visuals.R
#
# Post 13: Continuous Distributions (uniform and exponential)
#
# Two-panel figure. All numbers are embedded from the verified console output of
# pull_stats_post13_continuous_distributions_demo_data.R (2025 regular season).
# Nothing is re-pulled at plot time.
#
#   PANEL 1 (EXPONENTIAL): observed share of scoring-gap waiting times vs the
#           share a fitted exponential predicts, across five time ranges. The
#           under-2-minute deficit is the "floor" that the exponential lacks.
#   PANEL 2 (UNIFORM): observed share of drives per 10-yard starting band vs the
#           flat 10%-per-band a uniform distribution predicts (red reference).
#
# Output: stats_posts/output/plots/stats_post13_continuous_distributions.png at 300 DPI.
# Degrades gracefully to two separate PNGs if patchwork is not installed.
# ==============================================================================

# ------------------------------- CONFIGURATION --------------------------------
SEASON      <- 2025L
OUTPUT_DIR  <- "stats_posts/output/plots"
DPI         <- 300L
NAVY        <- "#13294B"   # observed data
RED         <- "#C8102E"   # model prediction / reference
COMBINED_PNG <- file.path(OUTPUT_DIR, "stats_post13_continuous_distributions.png")
PANEL1_PNG   <- file.path(OUTPUT_DIR, "stats_post13_exponential_gaps.png")
PANEL2_PNG   <- file.path(OUTPUT_DIR, "stats_post13_uniform_drivestart.png")

# --- Embedded verified figures (from console output) --------------------------
# Exponential: shares per time-range bin, derived from the verified CDF marks
#   observed CDF:  120->0.113, 300->0.439, 600->0.831, 900->0.949
#   exp CDF:       120->0.265, 300->0.537, 600->0.785, 900->0.901
exp_bins <- c("Under 2 min", "2 to 5 min", "5 to 10 min", "10 to 15 min", "Over 15 min")
exp_observed    <- c(0.113, 0.326, 0.392, 0.118, 0.051) * 100   # % of gaps
exp_predicted   <- c(0.265, 0.272, 0.248, 0.116, 0.099) * 100   # % of gaps

# Uniform: observed share of drives per 10-yard own-yard-line band
uni_bands    <- c("1-10","11-20","21-30","31-40","41-50",
                  "51-60","61-70","71-80","81-90","91-99")
uni_observed <- c(8.12, 17.93, 27.56, 29.14, 7.28, 4.54, 2.46, 1.58, 0.88, 0.51)
UNIFORM_LINE <- 10.0   # a uniform distribution puts 10% in every band
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
})

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# --- Shared theme -------------------------------------------------------------
theme_stats <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11, color = "grey30",
                                   margin = margin(b = 8)),
      plot.caption  = element_text(size = 9, color = "grey45"),
      axis.title    = element_text(size = 11),
      legend.position = "top",
      legend.title  = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

# ==============================================================================
# PANEL 1: EXPONENTIAL - observed vs predicted share per time range
# ==============================================================================
p1_data <- data.frame(
  bin       = factor(rep(exp_bins, 2), levels = exp_bins),
  source    = factor(rep(c("Observed", "Exponential prediction"), each = length(exp_bins)),
                     levels = c("Observed", "Exponential prediction")),
  share_pct = c(exp_observed, exp_predicted)
)

p1 <- ggplot(p1_data, aes(x = bin, y = share_pct, fill = source)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", share_pct)),
            position = position_dodge(width = 0.75),
            vjust = -0.4, size = 3.1, color = "grey25") +
  scale_fill_manual(values = c("Observed" = NAVY,
                               "Exponential prediction" = RED)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Time between scores is right-skewed, but not exponential",
    subtitle = paste0(SEASON, " regular season, 2,319 gaps between scoring plays. ",
                      "SD divided by mean = 0.70, where a true exponential needs 1.0."),
    x = "Waiting time until the next score",
    y = "Share of gaps",
    caption = "Exponential expects mostly short gaps; real scoring has a floor (kickoff plus drive)."
  ) +
  theme_stats()

# ==============================================================================
# PANEL 2: UNIFORM - observed share per field-position band vs flat 10%
# ==============================================================================
p2_data <- data.frame(
  band         = factor(uni_bands, levels = uni_bands),
  observed_pct = uni_observed
)

p2 <- ggplot(p2_data, aes(x = band, y = observed_pct)) +
  geom_col(fill = NAVY, width = 0.72) +
  geom_hline(yintercept = UNIFORM_LINE, color = RED,
             linetype = "dashed", linewidth = 0.9) +
  geom_text(aes(label = sprintf("%.0f%%", observed_pct)),
            vjust = -0.4, size = 3.1, color = "grey25") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Drive start is nowhere near uniform",
    subtitle = paste0(SEASON, " regular season, 5,700 drives. Mean start: own 30.7, ",
                      "not the 50 a uniform distribution would center on."),
    x = "Drive-starting own yard line (10-yard bands)",
    y = "Share of drives",
    caption = "Red line: uniform's flat 10% per band. The 35 (2025 touchback) drives the 31-40 spike."
  ) +
  theme_stats()

# ==============================================================================
# COMBINE AND SAVE (graceful degradation if patchwork is unavailable)
# ==============================================================================
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  combined <- (p1 / p2) +
    plot_annotation(
      title = "Two continuous distributions, tested against 2025 NFL data",
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
  ggsave(COMBINED_PNG, combined, width = 9, height = 10, dpi = DPI, bg = "white")
  cat("Saved combined figure:", COMBINED_PNG, "\n")
} else {
  ggsave(PANEL1_PNG, p1, width = 9, height = 5.5, dpi = DPI, bg = "white")
  ggsave(PANEL2_PNG, p2, width = 9, height = 5.5, dpi = DPI, bg = "white")
  cat("patchwork not found. Saved two separate panels:\n")
  cat("  ", PANEL1_PNG, "\n")
  cat("  ", PANEL2_PNG, "\n")
}

# ------------------------------------------------------------------------------
# CONSOLE SUMMARY
# ------------------------------------------------------------------------------
cat("================================================================\n")
cat("  POST 13 VISUALS BUILT\n")
cat("================================================================\n")
cat("Panel 1 (exponential): observed vs predicted share across 5 time bins\n")
cat("  under-2-min: observed 11.3% vs exponential 26.5% (the missing short gaps)\n")
cat("Panel 2 (uniform): observed share per 10-yard band vs flat 10% reference\n")
cat("  modal band 31-40: 29.1% vs uniform 10%\n")
cat("Output directory:", OUTPUT_DIR, "| DPI:", DPI, "\n")
cat("================================================================\n")
