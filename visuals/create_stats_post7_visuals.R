# ==============================================================================
# Statistics Series | Post 7: Visualizing Distributions
# One distribution of 2025 EPA-per-play shown three ways: histogram, density,
# box plot. A teaching panel for how each plot type surfaces a different
# feature of the SAME data.
#
# Outputs (300 DPI, output/plots/):
#   stats_post7_epa_three_views.png          (combined teaching panel)
#   stats_post7_epa_histogram.png            (fallback if patchwork absent)
#   stats_post7_epa_density.png              (fallback if patchwork absent)
#   stats_post7_epa_boxplot.png              (fallback if patchwork absent)
#
# Requires: ggplot2, dplyr
#   plus the project loader sourced below (nflfastR, nflreadr, glue, here are
#   loaded by R/15):
#   R/15_multi_season_pbp.R -> load_normalized_season(), validate_epa_distribution()
#   Optional: patchwork (combines the three panels into one figure; if not
#             installed the script saves the three panels separately instead).
#
# REUSED PROJECT INFRASTRUCTURE (nothing re-implemented):
#   The EPA play population is selected with the EXACT filter that
#   validate_epa_distribution() applies in R/15_multi_season_pbp.R, on the same
#   normalized cache file (data/season2_cache/pbp_normalized_2025.rds) that the
#   validator reads. The trio therefore plots the same plays the validator
#   checks, and the script self-verifies the count against the validator.
#
# TWO FILTER FACTS, MATCHED ON PURPOSE (both affect the play count):
#   1. The validator's garbage-time clause applies the win-probability window
#      (wp < 0.10 | wp > 0.90) to ALL quarters, not only the 4th. This is
#      broader than the textbook 4th-quarter garbage-time definition. Matched
#      verbatim so the count reconciles with the validator.
#   2. The normalized cache is NOT filtered to regular season (R/15 loads
#      nflfastR::load_pbp() whole; the validator has no season_type clause), so
#      the count includes postseason pass/run plays. REG_ONLY below defaults to
#      FALSE to preserve the exact match; set TRUE for regular-season-only.
#
# Run from project root. No download needed (reads the Season 2 cache).
# ==============================================================================

# CONFIGURATION ----------------------------------------------------------------
POST_NUM    <- 7L
CONCEPT     <- "Visualizing Distributions"
SEASON      <- 2025L
BINWIDTH    <- 0.25            # histogram binwidth for EPA (points)
REG_ONLY    <- FALSE           # FALSE = match validator exactly (incl. postseason)
CROSS_CHECK <- TRUE            # assert vector length == validate_epa_distribution() n_plays
R_DIR       <- "R"             # where the project loader lives
OUTPUT_DIR  <- "output/plots"
DPI         <- 300L
WIDTH_IN    <- 9
HEIGHT_IN   <- 11              # tall: three stacked panels share the EPA x-axis

# Colors (locked stats-series palette) -----------------------------------------
FILL_DENSITY <- "#DCE8F5"
FILL_HIST    <- "#DCE8F5"
FILL_BOX     <- "#DCE8F5"
LINE_MEAN    <- "#C0392B"     # red  -- solid, mean
LINE_MEDIAN  <- "#1A5276"     # navy -- dashed, median
COL_OUTLINE  <- "#89A9C8"     # geom outline
COL_TEXT     <- "#1C1C1A"
COL_SUBTEXT  <- "#5F5E5A"
COL_BG       <- "#FAFAF8"
COL_PANEL    <- "#FFFFFF"
COL_GRID     <- "#E8E6DF"
COL_STRIP    <- "#ECEAE3"

# LIBRARIES --------------------------------------------------------------------
library(ggplot2)
library(dplyr)

HAS_PATCHWORK <- requireNamespace("patchwork", quietly = TRUE)

# Reuse the project's loader and validator (do not re-implement) ---------------
source(file.path(R_DIR, "15_multi_season_pbp.R"))   # load_normalized_season(),
                                                     # validate_epa_distribution()

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# SHARED THEME (locked stats-series standard) ----------------------------------
theme_stats_series <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = COL_BG,    color = NA),
      panel.background = element_rect(fill = COL_PANEL, color = NA),
      panel.grid.major = element_line(color = COL_GRID, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = COL_STRIP, color = NA),
      strip.text       = element_text(color = COL_TEXT, face = "bold", size = 11),
      plot.title       = element_text(color = COL_TEXT, face = "bold", size = 14),
      plot.subtitle    = element_text(color = COL_SUBTEXT, size = 10),
      plot.caption     = element_text(color = COL_SUBTEXT, size = 8, hjust = 0),
      axis.title       = element_text(color = COL_TEXT, size = 11),
      axis.text        = element_text(color = COL_SUBTEXT, size = 10),
      legend.position  = "none"
    )
}

# SKEWNESS ESTIMATOR (base R, no extra dependency) -----------------------------
# Moment coefficient of skewness g1 = m3 / m2^(3/2), where m2 and m3 are the
# population (divide-by-n) second and third central moments. This is identical
# to moments::skewness() with its default and to e1071::skewness(type = 1).
# Implemented in base R so the script needs no skewness package.
moment_skewness_g1 <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3L) return(NA_real_)
  m  <- mean(x)
  m2 <- mean((x - m)^2)
  m3 <- mean((x - m)^3)
  if (m2 <= 0) return(NA_real_)
  m3 / (m2^(3 / 2))
}

# ==============================================================================
# STEP 1: Load the normalized season (same cache file the validator reads)
# ==============================================================================
pbp <- load_normalized_season(SEASON)

# ==============================================================================
# STEP 2: Column presence check (defensive against any nflverse rename)
# The filter touches only play-level PBP columns, which were not affected by the
# nflverse player-stats rename wave. Fail loudly if any are missing rather than
# silently dropping plays.
# ==============================================================================
required_cols <- c("epa", "posteam", "play_type", "down", "wp",
                   "desc", "two_point_attempt", "season_type")
missing_cols  <- setdiff(required_cols, names(pbp))
if (length(missing_cols) > 0) {
  stop(
    "Cache for season ", SEASON, " is missing required column(s): ",
    paste(missing_cols, collapse = ", "),
    ". Filter cannot match validate_epa_distribution() without them.",
    call. = FALSE
  )
}

# ==============================================================================
# STEP 3: EPA play population -- EXACT filter from validate_epa_distribution()
# (R/15_multi_season_pbp.R). Copied line-for-line so the trio plots the same
# plays the validator checks. Do not edit without editing the validator.
# ==============================================================================
clean <- pbp %>%
  filter(
    !is.na(epa),
    !is.na(posteam),
    play_type %in% c("pass", "run"),
    # Exclude garbage time (validator applies this to ALL quarters, not just Q4)
    !(down >= 1 & !is.na(wp) & (wp < 0.10 | wp > 0.90)),
    # Exclude kneels and spikes
    !grepl("kneel|spike", tolower(desc), fixed = FALSE),
    # Exclude 2-point attempts
    is.na(two_point_attempt) | two_point_attempt != 1
  )

# Optional regular-season-only narrowing (default FALSE = exact validator match)
if (REG_ONLY) {
  clean <- clean %>% filter(season_type == "REG")
  message("REG_ONLY = TRUE: restricted to regular season. ",
          "Count will NOT match the validator's per-season figure.")
}

epa <- clean$epa
n_plays <- length(epa)

if (n_plays == 0L) {
  stop("No plays survived the filter for season ", SEASON,
       ". Check the cache contents.", call. = FALSE)
}

# Free the large PBP object before plotting
rm(pbp)
gc(verbose = FALSE)

# ==============================================================================
# STEP 4: Summary statistics for the LinkedIn prose (computed from data)
# ==============================================================================
mean_epa   <- mean(epa)
median_epa <- median(epa)
sd_epa     <- sd(epa)                      # sample SD (divide by n-1)
fivenum_v  <- fivenum(epa)                 # min, lower-hinge, median, upper-hinge, max
q1_epa     <- as.numeric(quantile(epa, 0.25, type = 7))
q3_epa     <- as.numeric(quantile(epa, 0.75, type = 7))
iqr_epa    <- q3_epa - q1_epa              # IQR on type-7 quartiles
min_epa    <- min(epa)
max_epa    <- max(epa)
skew_epa   <- moment_skewness_g1(epa)      # g1, = moments::skewness() default

# ==============================================================================
# STEP 5: Self cross-check against validate_epa_distribution() for SEASON
# Proves the trio's play count equals the validator's per-season count.
# ==============================================================================
if (CROSS_CHECK && !REG_ONLY) {
  vcheck <- tryCatch(
    validate_epa_distribution(seasons = SEASON, save_plots = FALSE),
    error = function(e) {
      message("Cross-check skipped: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(vcheck)) {
    validator_n <- vcheck$n_plays[vcheck$season == SEASON]
    if (length(validator_n) == 1L && !is.na(validator_n) &&
        validator_n == n_plays) {
      message("Cross-check PASSED: filtered play count (",
              format(n_plays, big.mark = ","),
              ") matches validate_epa_distribution() for ", SEASON, ".")
    } else {
      warning("Cross-check MISMATCH: script n = ",
              format(n_plays, big.mark = ","),
              " vs validator n = ",
              format(validator_n, big.mark = ","),
              ". Investigate before publishing the count.",
              call. = FALSE)
    }
  }
} else if (CROSS_CHECK && REG_ONLY) {
  message("Cross-check skipped: REG_ONLY = TRUE diverges from the validator ",
          "by design (validator does not filter season_type).")
}

# ==============================================================================
# VISUALS: three views of the same EPA vector, shared x-axis
# ==============================================================================
plot_df <- tibble::tibble(epa = epa)

# Shared x-range so all three panels align (full data range, nothing hidden)
x_range <- range(epa, na.rm = TRUE)

center_caption <- sprintf(
  "Red solid = mean (%.3f)   |   Navy dashed = median (%.3f)",
  mean_epa, median_epa
)
src_caption <- paste0(
  "Source: nflfastR via load_normalized_season(", SEASON, ") | ",
  "filter matches validate_epa_distribution() | n = ",
  format(n_plays, big.mark = ","), " plays"
)

# Panel 1: Histogram -- shows the binned shape and where the mass piles up
p_hist <- ggplot(plot_df, aes(x = epa)) +
  geom_histogram(binwidth = BINWIDTH, fill = FILL_HIST,
                 color = COL_OUTLINE, linewidth = 0.2) +
  geom_vline(xintercept = mean_epa,   color = LINE_MEAN,
             linewidth = 0.9, linetype = "solid") +
  geom_vline(xintercept = median_epa, color = LINE_MEDIAN,
             linewidth = 0.9, linetype = "dashed") +
  coord_cartesian(xlim = x_range) +
  labs(
    title    = "Histogram",
    subtitle = sprintf("Binned counts (binwidth = %.2f): where plays pile up",
                       BINWIDTH),
    x = "EPA per play", y = "Count"
  ) +
  theme_stats_series()

# Panel 2: Density -- smooths the shape, makes skew and tails easier to read
p_density <- ggplot(plot_df, aes(x = epa)) +
  geom_density(fill = FILL_DENSITY, color = COL_OUTLINE,
               alpha = 0.85, linewidth = 0.6) +
  geom_vline(xintercept = mean_epa,   color = LINE_MEAN,
             linewidth = 0.9, linetype = "solid") +
  geom_vline(xintercept = median_epa, color = LINE_MEDIAN,
             linewidth = 0.9, linetype = "dashed") +
  coord_cartesian(xlim = x_range) +
  labs(
    title    = "Density",
    subtitle = "Smoothed shape: the gap between mean and median signals skew",
    x = "EPA per play", y = "Density"
  ) +
  theme_stats_series()

# Panel 3: Box plot -- compresses to five-number summary; center line = median,
# diamond = mean (their separation is the same skew, shown a third way)
p_box <- ggplot(plot_df, aes(x = epa, y = "")) +
  geom_boxplot(fill = FILL_BOX, color = COL_OUTLINE,
               outlier.alpha = 0.15, outlier.size = 0.8, width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3.5,
               color = LINE_MEAN) +
  geom_vline(xintercept = median_epa, color = LINE_MEDIAN,
             linewidth = 0.7, linetype = "dashed") +
  coord_cartesian(xlim = x_range) +
  labs(
    title    = "Box plot",
    subtitle = "Five-number summary: box = IQR, line = median, diamond = mean",
    x = "EPA per play", y = NULL
  ) +
  theme_stats_series()

# Combine into one teaching panel (patchwork), or save separately if unavailable
combined_path <- file.path(OUTPUT_DIR, "stats_post7_epa_three_views.png")

if (HAS_PATCHWORK) {
  panel <- patchwork::wrap_plots(p_hist, p_density, p_box, ncol = 1) +
    patchwork::plot_annotation(
      title    = sprintf("One Distribution, Three Views | %d EPA per Play", SEASON),
      subtitle = paste0(
        "Same plays, three chart types. ", center_caption
      ),
      caption  = src_caption,
      theme    = theme(
        plot.background = element_rect(fill = COL_BG, color = NA),
        plot.title      = element_text(color = COL_TEXT, face = "bold", size = 16),
        plot.subtitle   = element_text(color = COL_SUBTEXT, size = 11),
        plot.caption    = element_text(color = COL_SUBTEXT, size = 8, hjust = 0)
      )
    )
  ggsave(combined_path, panel, width = WIDTH_IN, height = HEIGHT_IN,
         dpi = DPI, bg = COL_BG)
  cat(sprintf("\nSaved: %s\n", combined_path))
} else {
  message("patchwork not installed: saving the three panels separately.")
  ggsave(file.path(OUTPUT_DIR, "stats_post7_epa_histogram.png"), p_hist,
         width = WIDTH_IN, height = HEIGHT_IN / 3, dpi = DPI, bg = COL_BG)
  ggsave(file.path(OUTPUT_DIR, "stats_post7_epa_density.png"), p_density,
         width = WIDTH_IN, height = HEIGHT_IN / 3, dpi = DPI, bg = COL_BG)
  ggsave(file.path(OUTPUT_DIR, "stats_post7_epa_boxplot.png"), p_box,
         width = WIDTH_IN, height = HEIGHT_IN / 3, dpi = DPI, bg = COL_BG)
  cat("\nSaved three separate panels to ", OUTPUT_DIR,
      " (install patchwork for the combined figure).\n", sep = "")
}

# ==============================================================================
# CONSOLE SUMMARY (labeled lines, easy to paste into the LinkedIn draft)
# ==============================================================================
cat("\n")
cat("====================================================================\n")
cat(sprintf("  STATS POST %d | %s\n", POST_NUM, CONCEPT))
cat(sprintf("  %d EPA per play (filter matches validate_epa_distribution)\n",
            SEASON))
cat(sprintf("  Population: %s\n",
            if (REG_ONLY) "regular season only" else
              "all plays in cache, incl. postseason (validator-exact)"))
cat("====================================================================\n")
cat(sprintf("  n plays (after filtering) : %s\n", format(n_plays, big.mark = ",")))
cat(sprintf("  mean EPA                  : %.4f\n", mean_epa))
cat(sprintf("  median EPA                : %.4f\n", median_epa))
cat(sprintf("  standard deviation        : %.4f\n", sd_epa))
cat("  --------------------------------------------------------------\n")
cat("  Five-number summary (Tukey fivenum):\n")
cat(sprintf("    min                     : %.4f\n", fivenum_v[1]))
cat(sprintf("    Q1 (lower hinge)        : %.4f\n", fivenum_v[2]))
cat(sprintf("    median                  : %.4f\n", fivenum_v[3]))
cat(sprintf("    Q3 (upper hinge)        : %.4f\n", fivenum_v[4]))
cat(sprintf("    max                     : %.4f\n", fivenum_v[5]))
cat("  Quartiles (type-7, used for IQR):\n")
cat(sprintf("    Q1                      : %.4f\n", q1_epa))
cat(sprintf("    Q3                      : %.4f\n", q3_epa))
cat(sprintf("    IQR (Q3 - Q1)           : %.4f\n", iqr_epa))
cat("  --------------------------------------------------------------\n")
cat(sprintf("  skewness (g1, moment)     : %.4f\n", skew_epa))
cat("    estimator: base-R moment coefficient g1 = m3 / m2^(3/2),\n")
cat("    identical to moments::skewness() and e1071::skewness(type = 1)\n")
cat("====================================================================\n")
