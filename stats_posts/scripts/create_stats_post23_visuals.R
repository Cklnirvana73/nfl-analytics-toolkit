# ==============================================================================
# create_stats_post23_visuals.R
# Statistics Series, Post 23: The Significance Threshold
#
# CENTRAL FINDING (must be the biggest visible thing in the panel):
#   The data requirement climbs as the tolerance tightens. Quarterback data
#   clears the requirement at a tolerance of 0.05 and falls short of the
#   requirement at 0.001. The strictest threshold was never affordable.
#
# DATA PROVENANCE
#   Every number is hardcoded from the verified console output of
#   pull_stats_post23_alpha_data.R (section Q5). Nothing is re-simulated.
#   These figures depend only on the tolerance, the power target, and the
#   effect-size floor. They carry NO dependency on the player-week panel,
#   which is why they are safe to publish while the panel row counts are
#   unreconciled against Posts 21 and 22.
#
#   The quarterback player-week count is deliberately NOT plotted. That figure
#   differs between panel runs (1,242 vs 1,344). Both values sit above 788 and
#   below 1,711, so the band is drawn between those two bounds and the exact
#   position inside it is left unresolved. Drawing a line would assert a
#   precision the reconciliation does not support.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(glue)
  library(scales)
  library(tibble)
})

# ------------------------------------------------------------ CONFIGURATION

OUTPUT_DIR  <- file.path("stats_posts", "output", "plots")
FILE_PREFIX <- "stats_post23_"
PLOT_WIDTH  <- 12
PLOT_HEIGHT <- 8
PLOT_DPI    <- 300

# Okabe-Ito, colorblind safe. Series convention:
#   navy = the value that held, gold = neither value is wrong
COL_NAVY <- "#0072B2"
COL_GOLD <- "#E69F00"

# Verified from pull Q5. Held constant: power 0.80, effect-size floor 0.10.
TARGET_POWER <- 0.80
MDE_Q        <- 0.10

req <- tibble::tribble(
  ~alpha,  ~required_n,
  0.200,   454L,
  0.100,   622L,
  0.050,   788L,
  0.010,   1171L,
  0.005,   1335L,
  0.001,   1711L
)

# The two bounds the post names. Both verified in Q5.
BOUND_CLEARED <- 788L    # requirement at alpha 0.05, quarterbacks clear this
BOUND_SHORT   <- 1711L   # requirement at alpha 0.001, quarterbacks fall short

# ------------------------------------------------------- PRE-BUILD ASSERTIONS

stopifnot(
  "six tolerance levels present"                     = nrow(req) == 6L,
  "no missing values"                                = !any(is.na(req)),
  "requirement rises strictly as tolerance tightens" =
    all(diff(req$required_n[order(-req$alpha)]) > 0),
  "alpha 0.05 requirement is 788"                    =
    req$required_n[req$alpha == 0.050] == 788L,
  "alpha 0.001 requirement is 1,711"                 =
    req$required_n[req$alpha == 0.001] == 1711L,
  "cleared bound matches the alpha 0.05 requirement" =
    BOUND_CLEARED == req$required_n[req$alpha == 0.050],
  "short bound matches the alpha 0.001 requirement"  =
    BOUND_SHORT == req$required_n[req$alpha == 0.001],
  "band has positive width"                          = BOUND_SHORT > BOUND_CLEARED,
  "the strictest tolerance sits outside the band"    =
    max(req$required_n) >= BOUND_SHORT,
  "the loosest tolerance sits below the band"        =
    min(req$required_n) < BOUND_CLEARED,
  "requirement roughly quadruples across the grid"   =
    round(max(req$required_n) / min(req$required_n), 1) == 3.8
)

message("All 11 pre-build assertions passed.")

# ------------------------------------------------------------------ PREP

plot_df <- req %>%
  mutate(
    alpha_lbl = factor(
      format(alpha, nsmall = 3, trim = TRUE),
      levels = format(sort(alpha, decreasing = TRUE), nsmall = 3, trim = TRUE)
    ),
    # Gold marks the tolerance the study could not afford. Nothing here is
    # wrong, so red is not the right encoding.
    bar_col = ifelse(required_n >= BOUND_SHORT, COL_GOLD, COL_NAVY)
  )

# ------------------------------------------------------------------ PLOT

p <- ggplot(plot_df, aes(x = required_n, y = alpha_lbl)) +

  # Band drawn first so bars sit on top of it.
  annotate(
    "rect",
    xmin = BOUND_CLEARED, xmax = BOUND_SHORT,
    ymin = -Inf, ymax = Inf,
    fill = COL_GOLD, alpha = 0.16
  ) +
  annotate(
    "segment",
    x = c(BOUND_CLEARED, BOUND_SHORT), xend = c(BOUND_CLEARED, BOUND_SHORT),
    y = -Inf, yend = Inf,
    color = COL_GOLD, linewidth = 0.6, linetype = "dashed"
  ) +

  geom_col(aes(fill = bar_col), width = 0.62, show.legend = FALSE) +
  scale_fill_identity() +

  geom_text(
    aes(label = comma(required_n)),
    hjust = -0.18, size = 5.1, fontface = "bold", color = "grey20"
  ) +

  # Annotation sits above the geometry it describes, per the collision rule.
  annotate(
    "text",
    x = (BOUND_CLEARED + BOUND_SHORT) / 2,
    y = 6.62,
    label = "Quarterback data lands somewhere in here",
    size = 4.6, fontface = "bold", color = "grey25"
  ) +
  annotate(
    "text",
    x = (BOUND_CLEARED + BOUND_SHORT) / 2,
    y = 6.28,
    label = "Above the requirement at 0.05, below the requirement at 0.001",
    size = 3.9, color = "grey40"
  ) +

  scale_x_continuous(
    labels = comma,
    breaks = c(0, 500, 1000, 1500, 2000),
    expand = expansion(mult = c(0, 0.13))
  ) +
  # Discrete axis: expand, never coord_cartesian(ylim = ).
  scale_y_discrete(expand = expansion(add = c(0.6, 1.35))) +

  coord_cartesian(xlim = c(0, 1900)) +

  labs(
    title    = "A stricter threshold would have cost more data than I had",
    subtitle = glue(
      "Player-weeks required per position group to detect the same size of effect, ",
      "holding the detection target fixed.\n",
      "Gold marks the tolerance the quarterback group could not afford."
    ),
    x = "Player-weeks required per position group",
    y = "Tolerance for a false positive (alpha)",
    caption = "Requirements computed from the tolerance, the power target, and the effect-size floor. Data: nflfastR, 2010 to 2025."
  ) +

  theme_minimal(base_size = 15) +
  theme(
    plot.title       = element_text(face = "bold", size = 20, margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 12.5, color = "grey30", margin = margin(b = 16)),
    plot.caption     = element_text(size = 9.5, color = "grey50", hjust = 0,
                                    margin = margin(t = 14)),
    axis.title.x     = element_text(size = 12.5, color = "grey30", margin = margin(t = 10)),
    axis.title.y     = element_text(size = 12.5, color = "grey30", margin = margin(r = 10)),
    axis.text.y      = element_text(size = 13, face = "bold", color = "grey20"),
    axis.text.x      = element_text(size = 11, color = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey92"),
    plot.margin      = margin(22, 26, 16, 22)
  )

# ------------------------------------------------------------------ OUTPUT

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

png_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "alpha_data_cost.png"))
svg_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "alpha_data_cost.svg"))

ggsave(png_path, p, width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")

# SVG needs the svglite device package. Guard it: a missing suggested package
# must degrade to a message, never to an interactive install prompt, because
# a prompt inside a sourced script swallows every line that follows it.
svg_written <- FALSE
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, p, width = PLOT_WIDTH, height = PLOT_HEIGHT,
         bg = "white", device = svglite::svglite)
  svg_written <- TRUE
} else {
  message("svglite not installed. PNG written, SVG skipped.")
  message('  To enable SVG output: install.packages("svglite")')
}

# ------------------------------------------------------------ CONSOLE SUMMARY

message("\n", strrep("=", 70))
message("POST 23 VISUAL BUILD COMPLETE")
message(strrep("=", 70))
message(glue("  Panel        : requirement by tolerance, single panel"))
message(glue("  Tolerances   : {paste(format(req$alpha, nsmall = 3, trim = TRUE), collapse = ', ')}"))
message(glue("  Requirements : {paste(comma(req$required_n), collapse = ', ')}"))
message(glue("  Band         : {comma(BOUND_CLEARED)} to {comma(BOUND_SHORT)} player-weeks"))
message(glue("  Held fixed   : power {TARGET_POWER}, effect-size floor {MDE_Q}"))
message(glue("  Ratio        : {round(max(req$required_n) / min(req$required_n), 2)}x across the grid"))
message(glue("  PNG          : {png_path}"))
message(glue("  SVG          : {if (svg_written) svg_path else 'skipped'}"))
message(strrep("=", 70), "\n")
