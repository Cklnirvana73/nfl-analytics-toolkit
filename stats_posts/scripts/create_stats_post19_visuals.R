# ==============================================================================
# Statistics Series - Post 19: Standard Error
# Visual script
#
# PURPOSE
# -------
# Two panels supporting the post:
#   Panel 1: spread stays flat across age while the standard error climbs.
#            The finding is the divergence between the two lines.
#   Panel 2: the counterfactual. Age 34's standard error with its own sample
#            size versus with age 25's sample size, spread held fixed.
#
# DATA PROVENANCE
# ---------------
# Every number below is hardcoded from the verified console output of
# pull_stats_post19_standard_error_data.R (run 2026-08-06). Nothing is
# re-simulated or recomputed from the pipeline. Do not reseed, do not
# re-derive: if these numbers change, the post is wrong.
#
# SCOPE
# -----
# Ages 22 through 34 only. Ages 35, 36, and 37 have 3 or fewer transitions
# and are flagged sparse by MIN_AGE_OBS in R/23. Their exclusion is stated
# in the Panel 1 subtitle rather than left silent.
#
# OUTPUT
# ------
#   stats_posts/output/plots/stats_post19_standard_error.png  (300 DPI)
#   Falls back to two separate PNGs if patchwork is unavailable.
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(glue)
library(here)


# ==============================================================================
# CONFIGURATION
# ==============================================================================

OUTPUT_DIR  <- here::here("stats_posts", "output", "plots")
FILE_PREFIX <- "stats_post19_"
DPI         <- 300

# Color convention:
#   navy  = the actual, observed value
#   gold  = a neutral alternative or counterfactual, NOT a wrong value
# Red is reserved for naive or incorrect values and is deliberately unused here.
COL_NAVY <- "#1B3A6B"
COL_GOLD <- "#C8951E"
COL_GRAY <- "gray45"

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}


# ==============================================================================
# THEME
# ==============================================================================

theme_stats_post <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = rel(1.15), hjust = 0),
      plot.subtitle   = element_text(color = "gray40", size = rel(0.85), hjust = 0),
      plot.caption    = element_text(color = "gray55", size = rel(0.7),  hjust = 1),
      panel.grid.minor = element_blank(),
      axis.title      = element_text(color = "gray30"),
      axis.text       = element_text(color = "gray40"),
      legend.position = "none",
      plot.margin     = margin(12, 16, 12, 12)
    )
}


# ==============================================================================
# DATA (hardcoded from verified console output)
# ==============================================================================

# Per-age summary, running backs, ages 22-34 (non-sparse groups only).
# Source: pull script Step 5, PER-AGE SUMMARY: RB
age_data <- tibble::tribble(
  ~age, ~n_obs, ~sd_delta, ~se_delta,
   22L,    39L,     4.139,     0.663,
   23L,   121L,     5.093,     0.463,
   24L,   178L,     4.314,     0.323,
   25L,   189L,     4.238,     0.308,
   26L,   178L,     4.324,     0.324,
   27L,   148L,     3.825,     0.314,
   28L,   120L,     4.240,     0.387,
   29L,    90L,     4.223,     0.445,
   30L,    51L,     5.246,     0.735,
   31L,    29L,     4.237,     0.787,
   32L,    21L,     4.917,     1.073,
   33L,    11L,     2.919,     0.880,
   34L,     5L,     2.867,     1.282
)

# Counterfactual, from pull script Step 6.
# Age 34 spread (2.867) with age 25 sample size (189) = 0.209
CF_SE_ACTUAL       <- 1.282
CF_SE_COUNTERFACT  <- 0.209
CF_SE_AGE25        <- 0.308


# ==============================================================================
# PRE-BUILD ASSERTIONS
# ==============================================================================
# These run before any plot is constructed. A failure here means the
# hardcoded values drifted from the arithmetic they claim to represent.

stopifnot(
  "age_data has fewer than 2 rows" =
    nrow(age_data) >= 2,
  "age_data contains NA" =
    all(!is.na(age_data)),
  "se_delta does not equal sd_delta / sqrt(n_obs)" =
    all(abs(age_data$se_delta - age_data$sd_delta / sqrt(age_data$n_obs)) < 0.001),
  "se_delta should never exceed sd_delta" =
    all(age_data$se_delta <= age_data$sd_delta),
  "counterfactual does not reconcile: 2.867 / sqrt(189)" =
    abs(CF_SE_COUNTERFACT - 2.867 / sqrt(189)) < 0.001,
  "age 34 must have the smallest spread in the panel" =
    age_data$sd_delta[age_data$age == 34L] == min(age_data$sd_delta),
  "age 34 must have the widest standard error in the panel" =
    age_data$se_delta[age_data$age == 34L] == max(age_data$se_delta)
)

cat("Pre-build assertions passed.\n")


# ==============================================================================
# PANEL 1: spread flat, standard error climbing
# ==============================================================================

p1_data <- age_data %>%
  tidyr::pivot_longer(
    cols      = c(sd_delta, se_delta),
    names_to  = "measure",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    measure = factor(
      measure,
      levels = c("sd_delta", "se_delta"),
      labels = c("Spread of players", "Standard error of the average")
    )
  )

# Fixed literal titles. Never assembled with paste0; length asserted below.
P1_TITLE    <- "The spread holds steady. The error bar does not."
P1_SUBTITLE <- "Running back season-to-season change in fantasy points per game, 2011 to 2025. Ages 35 and up excluded (3 or fewer players each)."

stopifnot(
  "Panel 1 title too long for one line" = nchar(P1_TITLE) <= 60,
  "Panel 1 subtitle too long"           = nchar(P1_SUBTITLE) <= 145
)

# Endpoint labels replace a legend (direct labels beat legends).
p1_labels <- p1_data %>%
  dplyr::filter(age == 34L) %>%
  dplyr::mutate(
    label = c("Spread of players", "Standard error"),
    vjust = c(-1.1, -1.1)
  )

p1 <- ggplot(p1_data, aes(x = age, y = value, group = measure, color = measure)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.4) +
  geom_text(
    data        = p1_labels,
    aes(label = label),
    hjust       = 1.05,
    vjust       = -1.3,
    size        = 3.6,
    fontface    = "bold",
    show.legend = FALSE
  ) +
  annotate(
    "text", x = 25, y = 0.308,
    label = "n = 189", vjust = 2.2, size = 3.2, color = COL_GRAY
  ) +
  annotate(
    "text", x = 34, y = 1.282,
    label = "n = 5", vjust = 2.4, hjust = 0.9, size = 3.2, color = COL_GRAY
  ) +
  scale_color_manual(values = c(COL_GRAY, COL_NAVY)) +
  scale_x_continuous(breaks = seq(22, 34, by = 2)) +
  scale_y_continuous(limits = c(0, 6.4), breaks = seq(0, 6, by = 1)) +
  labs(
    title    = P1_TITLE,
    subtitle = P1_SUBTITLE,
    x        = "Age",
    y        = "Fantasy points per game"
  ) +
  theme_stats_post()


# ==============================================================================
# PANEL 2: the counterfactual
# ==============================================================================

p2_data <- tibble::tibble(
  label = factor(
    c("Age 25\n(n = 189)", "Age 34\n(n = 5)", "Age 34 spread\nwith n = 189"),
    levels = c("Age 25\n(n = 189)", "Age 34\n(n = 5)", "Age 34 spread\nwith n = 189")
  ),
  se   = c(CF_SE_AGE25, CF_SE_ACTUAL, CF_SE_COUNTERFACT),
  kind = c("actual", "actual", "counterfactual")
)

P2_TITLE    <- "Same spread, different sample size"
P2_SUBTITLE <- "Standard error of the average change. Age 34 has the smallest spread of any group shown."

stopifnot(
  "Panel 2 title too long for one line" = nchar(P2_TITLE) <= 60,
  "Panel 2 subtitle too long"           = nchar(P2_SUBTITLE) <= 145,
  "Panel 2 bars must be non-negative"   = all(p2_data$se >= 0)
)

p2 <- ggplot(p2_data, aes(x = label, y = se, fill = kind)) +
  geom_col(width = 0.62) +
  geom_text(
    aes(label = sprintf("%.3f", se)),
    vjust    = -0.6,
    size     = 4,
    fontface = "bold",
    color    = "gray20"
  ) +
  scale_fill_manual(values = c("actual" = COL_NAVY, "counterfactual" = COL_GOLD)) +
  scale_y_continuous(limits = c(0, 1.55), breaks = seq(0, 1.5, by = 0.5)) +
  labs(
    title    = P2_TITLE,
    subtitle = P2_SUBTITLE,
    x        = NULL,
    y        = "Standard error (points per game)",
    caption  = "Data: nflfastR via nflreadr | Aging curve module, R/23"
  ) +
  theme_stats_post()


# ==============================================================================
# ASSEMBLE AND SAVE
# ==============================================================================

out_combined <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "standard_error.png"))

if (requireNamespace("patchwork", quietly = TRUE)) {

  library(patchwork)
  combined <- p1 / p2 + patchwork::plot_layout(heights = c(1.15, 1))

  ggsave(
    filename = out_combined,
    plot     = combined,
    width    = 9,
    height   = 10.5,
    dpi      = DPI
  )
  cat(glue("Saved: {out_combined}\n"))

} else {

  message("patchwork not installed. Saving panels as separate files.")

  out_p1 <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "panel1_spread_vs_se.png"))
  out_p2 <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "panel2_counterfactual.png"))

  ggsave(filename = out_p1, plot = p1, width = 9, height = 5.6, dpi = DPI)
  ggsave(filename = out_p2, plot = p2, width = 9, height = 5.2, dpi = DPI)

  cat(glue("Saved: {out_p1}\n"))
  cat(glue("Saved: {out_p2}\n"))
}


# ==============================================================================
# CONSOLE SUMMARY AND RENDER WATCH ITEMS
# ==============================================================================

cat("\n")
cat("==============================================================\n")
cat("  Post 19 visuals built. Check these in the rendered PNG:\n")
cat("==============================================================\n")
cat("  1. Panel 1 title fits on ONE line (this was the Post 18 v2 failure).\n")
cat("  2. The two endpoint labels at age 34 do not collide with each\n")
cat("     other or with the 'n = 5' annotation.\n")
cat("  3. The gray spread line reads as roughly flat while the navy\n")
cat("     standard error line clearly climbs after age 29.\n")
cat("  4. Panel 2: all three value labels sit above their bars and\n")
cat("     none is clipped by the 1.55 y-axis ceiling.\n")
cat("  5. Panel 2 x-axis labels wrap to two lines without overlapping.\n")
cat("==============================================================\n\n")
