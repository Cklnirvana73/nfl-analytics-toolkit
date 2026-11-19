# ==============================================================================
# create_stats_post28_visuals.R
#
# Statistics Series, Post 28: The One-Sample t-Test
#
# Central finding the panel must carry:
#   The fixed allowance of 0.05 points passed all sixteen seasons.
#   The 95% interval separated four of them from zero.
#
# ALL NUMBERS ARE HARDCODED from the verified console output of
# pull_stats_post28_epa_center_data_v2.R (run 2026-09-07 15:14:15 CDT).
# Nothing is re-simulated. Do not recompute these from a fresh pull; if the
# pull is rerun, update this block and the post together or they will drift.
#
# Filters behind these numbers mirror validate_epa_distribution() lines 665-675:
# pass/run plays, garbage time removed across all quarters, kneels and spikes
# removed by desc regex, two-point attempts removed, NO season_type filter
# (postseason plays are included, 4.32% of rows).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# ------------------------------------------------------------- constants ----

OUTPUT_DIR <- file.path("stats_posts", "output", "plots")
PREFIX     <- "stats_post28_"

TOLERANCE  <- 0.05        # epa_tolerance default, 15_multi_season_pbp.R line 638
NAVY       <- "#0072B2"   # Okabe-Ito. Actual values, interval contains zero
GOLD       <- "#E69F00"   # Okabe-Ito. Neither value is wrong, both checks valid
GREY_BAND  <- "#BBBBBB"

# Red is deliberately NOT used. Per the series palette rule red marks a naive or
# wrong value. Neither check is wrong here, so the four separated seasons are
# gold, not red. This is the per-post judgment call the rule leaves open.

# ------------------------------------------------- verified console values ---

seasons <- tibble::tribble(
  ~season, ~n_plays, ~mean_epa,     ~ci_lo,        ~ci_hi,
  2010L,   25202L,   -0.01132340,   -0.028164149,  0.005517356,
  2011L,   24983L,    0.00594461,   -0.011076401,  0.022965622,
  2012L,   25256L,    0.00880737,   -0.008249030,  0.025863779,
  2013L,   26118L,    0.00567124,   -0.011206837,  0.022549325,
  2014L,   25085L,    0.00300524,   -0.013929012,  0.019939488,
  2015L,   25820L,   -0.01376001,   -0.030786916,  0.003266901,
  2016L,   25691L,    0.01311920,   -0.003453881,  0.029692273,
  2017L,   25695L,   -0.01920446,   -0.035806440, -0.002602478,
  2018L,   24661L,    0.01819490,    0.000812801,  0.035576996,
  2019L,   25315L,    0.00751727,   -0.009719735,  0.024754270,
  2020L,   25831L,    0.04497184,    0.028134525,  0.061809147,
  2021L,   26650L,    0.02203347,    0.005591867,  0.038475080,
  2022L,   27879L,    0.00582358,   -0.010200693,  0.021847863,
  2023L,   27323L,   -0.01570274,   -0.032042259,  0.000636784,
  2024L,   26878L,    0.01586125,   -0.000570610,  0.032293118,
  2025L,   26887L,    0.01245615,   -0.003961029,  0.028873324
)

plot_data <- seasons %>%
  mutate(
    within_tolerance = abs(mean_epa) < TOLERANCE,
    excludes_zero    = (ci_lo > 0) | (ci_hi < 0),
    ci_half          = (ci_hi - ci_lo) / 2,
    verdict          = if_else(excludes_zero,
                               "Interval separates it from zero",
                               "Interval still contains zero"),
    season_f         = factor(season, levels = rev(sort(unique(season))))
  )

# --------------------------------------------------- PRE-BUILD ASSERTIONS ----
# A01-A08 validate the data block. A09-A16 validate the post's actual claims.
# If any fire, the post and the panel disagree and one of them is wrong.

stopifnot(
  # --- data integrity ---
  "A01 sixteen seasons present"            = nrow(plot_data) == 16L,
  "A02 seasons are 2010:2025 with no gaps" = identical(sort(plot_data$season), 2010:2025),
  "A03 no missing values anywhere"         = !any(is.na(plot_data)),
  "A04 ci_lo strictly below mean"          = all(plot_data$ci_lo < plot_data$mean_epa),
  "A05 ci_hi strictly above mean"          = all(plot_data$ci_hi > plot_data$mean_epa),
  "A06 intervals are near-symmetric about the mean" =
    all(abs((plot_data$ci_hi - plot_data$mean_epa) -
            (plot_data$mean_epa - plot_data$ci_lo)) < 1e-6),
  "A07 play counts in plausible range"     = all(plot_data$n_plays > 20000 &
                                                 plot_data$n_plays < 30000),
  "A08 tolerance is the source default"    = TOLERANCE == 0.05,

  # --- the post's claims, checked in code ---
  "A09 POST CLAIM: the allowance passed all sixteen seasons" =
    all(plot_data$within_tolerance),
  "A10 POST CLAIM: exactly four seasons are separated from zero" =
    sum(plot_data$excludes_zero) == 4L,
  "A11 POST CLAIM: those four are 2017, 2018, 2020, 2021" =
    identical(sort(plot_data$season[plot_data$excludes_zero]),
              c(2017L, 2018L, 2020L, 2021L)),
  "A12 POST CLAIM: resolution is roughly 0.017 points in every season" =
    all(abs(plot_data$ci_half - 0.017) <= 0.001),
  "A13 POST CLAIM: allowance is about three times the resolution, all sixteen" =
    all(TOLERANCE / plot_data$ci_half > 2.5 & TOLERANCE / plot_data$ci_half < 3.5),
  "A14 POST CLAIM: 2020 is the sharpest case, largest absolute average of 16" =
    which.max(abs(plot_data$mean_epa)) == which(plot_data$season == 2020L),
  "A15 POST CLAIM: 2020's interval top runs past the allowance line" =
    plot_data$ci_hi[plot_data$season == 2020L] > TOLERANCE,
  "A16 POST CLAIM: 2025's interval contains zero" =
    plot_data$ci_lo[plot_data$season == 2025L] < 0 &
    plot_data$ci_hi[plot_data$season == 2025L] > 0,

  # --- 5-second rule precondition ---
  "A17 the two verdict groups are both non-empty, so color carries meaning" =
    dplyr::n_distinct(plot_data$verdict) == 2L
)

message("All 17 pre-build assertions passed.")

# ------------------------------------------------------------------ plot ----
# Layer order: tolerance band, then zero line, then intervals, then points.
# Reference geometry sits UNDER the data.

x_lo <- -0.06
x_hi <-  0.075

p <- ggplot(plot_data, aes(x = mean_epa, y = season_f)) +

  # tolerance band, drawn first so data sits on top of it
  annotate("rect",
           xmin = -TOLERANCE, xmax = TOLERANCE,
           ymin = -Inf, ymax = Inf,
           fill = GREY_BAND, alpha = 0.25) +
  annotate("segment",
           x = c(-TOLERANCE, TOLERANCE), xend = c(-TOLERANCE, TOLERANCE),
           y = -Inf, yend = Inf,
           color = "gray45", linetype = "dotted", linewidth = 0.5) +

  # the reference value the test is run against
  geom_vline(xintercept = 0, color = "gray25", linewidth = 0.6) +

  # 95% intervals
  geom_linerange(aes(xmin = ci_lo, xmax = ci_hi, color = verdict),
                 linewidth = 1.5) +
  geom_point(aes(color = verdict), size = 2.6) +

  scale_color_manual(values = c(
    "Interval still contains zero"    = NAVY,
    "Interval separates it from zero" = GOLD
  )) +

  # discrete axis: expansion(add = ), never coord_cartesian(ylim = )
  scale_y_discrete(expand = expansion(add = 0.9)) +
  scale_x_continuous(breaks = seq(-0.06, 0.06, by = 0.02)) +

  # continuous axis: coord_cartesian, does not drop data
  coord_cartesian(xlim = c(x_lo, x_hi)) +

  labs(
    title    = "The allowance passed all sixteen seasons. The interval separated four.",
    subtitle = paste0(
      "Average EPA per play with its 95% interval, 2010 to 2025.\n",
      "Shaded band is the pipeline's fixed allowance of 0.05 points either side of zero.\n",
      "The allowance is about three times wider than what a season of plays can resolve."
    ),
    x        = "Average EPA per play (points)",
    y        = NULL,
    color    = NULL,
    caption  = paste0(
      "Pass and run plays, garbage time and kneels, spikes and two-point ",
      "attempts removed. Postseason included.\nData: nflfastR"
    )
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = rel(1.15), hjust = 0),
    plot.subtitle    = element_text(color = "gray35", size = rel(0.85), hjust = 0,
                                    margin = margin(b = 12)),
    plot.caption     = element_text(color = "gray55", size = rel(0.65), hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y      = element_text(face = "bold", color = "gray25"),
    axis.title.x     = element_text(color = "gray30", margin = margin(t = 8)),
    legend.position  = "top",
    legend.justification = "left",
    plot.margin      = margin(12, 18, 10, 12)
  )

# Annotation placed ABOVE the geometry it refers to, to avoid collision.
# 2020 is the top-of-panel row, so the label goes above its interval.
p <- p +
  annotate("text",
           x = 0.0618, y = which(levels(plot_data$season_f) == "2020") + 0.42,
           label = "2020 clears the allowance\nbut its interval does not reach zero",
           hjust = 1, vjust = 0, size = 2.9, color = "gray25", lineheight = 0.95)

# ------------------------------------------------- POST-BUILD RENDER CHECKS --

built <- ggplot_build(p)

stopifnot(
  "R01 at least four layers built (rect, allowance, vline, intervals, points)" =
    length(built$data) >= 4L,
  "R02 the interval layer holds exactly sixteen rows" =
    identical(
      max(vapply(built$data,
                 function(d) if ("xmin" %in% names(d)) nrow(d) else 0L,
                 integer(1))),
      16L
    ),
  "R02b the point layer holds exactly sixteen rows" =
    identical(
      max(vapply(built$data,
                 function(d) if (all(c("x", "y", "shape") %in% names(d))) nrow(d) else 0L,
                 integer(1))),
      16L
    ),
  "R03 both colors present in the rendered plot" =
    length(intersect(c(NAVY, GOLD),
                     unlist(lapply(built$data, function(d) unique(d$colour))))) == 2L,
  "R04 x range covers every interval bound" =
    x_lo <= min(plot_data$ci_lo) && x_hi >= max(plot_data$ci_hi),
  "R05 x range covers both allowance lines" =
    x_lo <= -TOLERANCE && x_hi >= TOLERANCE,
  "R06 no data dropped by the coordinate system" =
    all(plot_data$ci_lo >= x_lo) && all(plot_data$ci_hi <= x_hi)
)

message("All 7 post-build render checks passed.")

# ---------------------------------------------------------------- export ----

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# Size note: series instructions say 1200x800. The code-review standard says
# 300 DPI. Posts 24 through 27 used 8 x 5.333 in at 300 DPI, which is 2400x1600.
# Following that precedent. This conflict still needs a series-wide ruling.
png_path <- file.path(OUTPUT_DIR, paste0(PREFIX, "season_intervals.png"))
ggsave(png_path, plot = p, width = 8, height = 5.333, dpi = 300)
message("Wrote: ", png_path)

# svglite is not installed locally. Guarded so the script does not fail.
svg_path <- file.path(OUTPUT_DIR, paste0(PREFIX, "season_intervals.svg"))
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, plot = p, width = 8, height = 5.333)
  message("Wrote: ", svg_path)
} else {
  message("svglite not installed. SVG skipped, PNG only.")
}

print(p)
