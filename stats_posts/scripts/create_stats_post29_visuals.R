# ==============================================================================
# create_stats_post29_visuals.R
#
# Post 29: The Independent Two-Sample t-Test
#
# Central finding the panel must deliver in 5 seconds:
#   The 115-player group contributes eleven times what the 1,830-player group
#   does to the standard error. The smaller group sets the precision.
#
# All values hardcoded from the verified console output of
# pull_stats_post29_two_sample_ttest_data.R. Nothing is re-simulated here,
# so the panel cannot drift from the prose.
#
# COLOR JUDGMENT (per-post call, red-vs-gold not yet decided series-wide):
#   Neither contribution is a naive or wrong value, so red is not used.
#   Navy  #0072B2 -> treatment contribution, the bar carrying the finding.
#   Gold  #E69F00 -> control contribution.
#
# OUTPUT SIZE: 8 x 5.333 in at 300 DPI, matching Posts 24 through 28.
#   Series instructions say 1200x800; the code-review standard says 300 DPI.
#   Conflict is still unresolved and flagged in the handoff.
# ==============================================================================

suppressPackageStartupMessages({
  library(grid)
  library(ggplot2)
  library(dplyr)
  library(here)
})

OUT_DIR <- here::here("stats_posts", "output", "plots")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

PLOT_TITLE   <- "The smaller group carries eleven times\nmore of the uncertainty"
PLOT_WIDTH   <- 8
PLOT_HEIGHT  <- 5.333

NAVY <- "#0072B2"
GOLD <- "#E69F00"
GREY <- "gray40"

# ------------------------------------------------------------------ verified
# Source: console output, injury proximity comparison, abs_error outcome.

N_TRT        <- 115L
N_CTL        <- 1830L
VAR_TRT      <- 7.40447
VAR_CTL      <- 10.34069
MEAN_TRT     <- 3.140398
MEAN_CTL     <- 3.849710
SE_REPORTED  <- 0.26464570
T_REPORTED   <- 2.680231

CONTRIB_TRT <- VAR_TRT / N_TRT
CONTRIB_CTL <- VAR_CTL / N_CTL

# ------------------------------------------------------- PRE-BUILD ASSERTIONS
# A01 to A08 validate the arithmetic. A09 to A14 validate the claims the post
# makes in words, in code, so a prose claim cannot silently drift from data.

stopifnot("A01 treatment n"      = N_TRT == 115L)
stopifnot("A02 control n"        = N_CTL == 1830L)
stopifnot("A03 trt contribution" = abs(CONTRIB_TRT - 0.0644) < 0.0001)
stopifnot("A04 ctl contribution" = abs(CONTRIB_CTL - 0.0057) < 0.0001)
stopifnot("A05 se reconstructs"  =
            abs(sqrt(CONTRIB_TRT + CONTRIB_CTL) - SE_REPORTED) < 1e-6)
stopifnot("A06 se rounds to .26" = round(SE_REPORTED, 2) == 0.26)
stopifnot("A07 gap"              =
            abs((MEAN_CTL - MEAN_TRT) - 0.709312) < 1e-6)
stopifnot("A08 t reconstructs"   =
            abs((MEAN_CTL - MEAN_TRT) / SE_REPORTED - T_REPORTED) < 1e-5)

# Post claim: "contributed eleven times what the 1,830-player group did"
stopifnot("A09 eleven times" =
            round(CONTRIB_TRT / CONTRIB_CTL) == 11)

# Post claim: "sixteen times more players" is NOT in the final draft, but the
# size ratio still governs the panel subtitle. Verify before using it.
stopifnot("A10 size ratio near 16" =
            abs(N_CTL / N_TRT - 15.9) < 0.1)

# Post claim: "Divide the gap by the standard error and the answer is 2.68"
stopifnot("A11 stated division" =
            abs((MEAN_CTL - MEAN_TRT) / SE_REPORTED - 2.68) < 0.005)

# Post claim: "another 200 players changes the standard error by about 0.001"
SE_PLUS_200 <- sqrt(CONTRIB_TRT + VAR_CTL / (N_CTL + 200L))
stopifnot("A12 200-player delta" =
            abs((SE_REPORTED - SE_PLUS_200) - 0.001) < 0.0005)

# Post claim: returning group landed CLOSER, so its mean error is lower
stopifnot("A13 direction" = MEAN_TRT < MEAN_CTL)

# Panel integrity: treatment bar must be the longer one or the 5-second
# reading inverts
stopifnot("A14 trt bar longer" = CONTRIB_TRT > CONTRIB_CTL)

message("Pre-build assertions passed: 14/14")

# ------------------------------------------------------------------- panel

plot_df <- tibble::tibble(
  group = factor(
    c("Returned\nfrom absence", "Never missed\na week"),
    levels = c("Never missed\na week", "Returned\nfrom absence")
  ),
  n            = c(N_TRT, N_CTL),
  contribution = c(CONTRIB_TRT, CONTRIB_CTL),
  fill_col     = c(NAVY, GOLD)
)

plot_df <- plot_df %>%
  dplyr::mutate(
    bar_label = sprintf("%.4f", contribution),
    n_label   = sprintf("%s players", format(n, big.mark = ","))
  )

X_MAX <- max(plot_df$contribution) * 1.32

p <- ggplot(plot_df, aes(x = contribution, y = group, fill = fill_col)) +
  geom_col(width = 0.58) +
  # Value label sits outside the bar end, never overlapping geometry.
  geom_text(
    aes(label = bar_label),
    hjust = -0.18, size = 5.4, fontface = "bold", color = GREY
  ) +
  # Group size annotated above each bar, clear of the bar body.
  geom_text(
    aes(label = n_label, y = as.numeric(group) + 0.40),
    x = 0, hjust = 0, size = 3.9, color = GREY
  ) +
  scale_fill_identity() +
  scale_y_discrete(expand = expansion(add = c(0.55, 0.75))) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.02)),
    breaks = seq(0, 0.06, by = 0.02)
  ) +
  coord_cartesian(xlim = c(0, X_MAX)) +
  labs(
    title    = PLOT_TITLE,
    subtitle = paste0(
      "Each group's share comes from its variance divided by its own size.",
      "\nThe larger group barely moves the total."
    ),
    x = "Share of the uncertainty in the comparison",
    y = NULL,
    caption = paste0(
      "Absolute error between second-half scoring average and prior-season ",
      "average, points per game.\nStandard error of the difference: 0.26. ",
      "Observed gap: 0.71."
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 15.5,
                                   lineheight = 1.1,
                                   margin = margin(b = 6)),
    plot.subtitle   = element_text(size = 11.5, color = GREY,
                                   margin = margin(b = 16), lineheight = 1.15),
    plot.caption    = element_text(size = 9, color = GREY, hjust = 0,
                                   margin = margin(t = 14), lineheight = 1.15),
    axis.text.y     = element_text(size = 11, color = "black",
                                   lineheight = 1.05),
    axis.title.x    = element_text(size = 11, color = GREY,
                                   margin = margin(t = 10)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin     = margin(18, 22, 14, 18)
  )

# ---------------------------------------------------------- POST-BUILD CHECKS
b <- ggplot_build(p)

# R01: exactly two bars drawn
col_layers <- which(vapply(
  b$data, function(d) all(c("xmin", "xmax") %in% names(d)), logical(1)
))
stopifnot("R01 one col layer" = length(col_layers) >= 1L)
n_bars <- max(vapply(b$data[col_layers], nrow, integer(1)))
stopifnot("R02 two bars" = n_bars == 2L)

# R03: the navy bar is the longer one as drawn, not just in the data frame
bar_data <- b$data[[col_layers[1]]]
navy_row <- bar_data[bar_data$fill == NAVY, ]
gold_row <- bar_data[bar_data$fill == GOLD, ]
stopifnot("R03 navy drawn longer" = navy_row$xmax > gold_row$xmax)

# R04: value labels present, one per bar
text_layers <- which(vapply(
  b$data, function(d) "label" %in% names(d), logical(1)
))
stopifnot("R04 text layers exist" = length(text_layers) >= 2L)

# R05: x axis starts at zero, so bar lengths are proportional
stopifnot("R05 x starts at 0" = b$layout$panel_params[[1]]$x.range[1] <= 0)

# R06: longer bar is not clipped by the x limit
stopifnot("R06 no clipping" =
            b$layout$panel_params[[1]]$x.range[2] >= CONTRIB_TRT)

# R07: no accidental legend, tested on the rendered gtable rather than on
# plot$guides, whose type varies by ggplot2 version. A suppressed legend
# leaves the guide-box cells present but empty (zeroGrob).
g <- ggplot2::ggplotGrob(p)
guide_cells <- grep("guide-box", g$layout$name)
legend_drawn <- length(guide_cells) > 0 && any(vapply(
  guide_cells, function(i) !inherits(g$grobs[[i]], "zeroGrob"), logical(1)
))
stopifnot("R07 no legend drawn" = !legend_drawn)

# R08: every title line must physically fit the device width. This is the
# check that would have caught the truncated title on the first render;
# no arithmetic or layer assertion can see it.
title_lines <- strsplit(PLOT_TITLE, "\n", fixed = TRUE)[[1]]
title_w_in <- max(vapply(title_lines, function(s) {
  grid::convertWidth(
    grid::grobWidth(grid::textGrob(
      s, gp = grid::gpar(fontsize = 15.5, fontface = "bold")
    )), "in", valueOnly = TRUE
  )
}, numeric(1)))
usable_in <- PLOT_WIDTH - 40 / 72
stopifnot("R08 title fits device" = title_w_in <= usable_in)

# R09: subtitle lines must fit too
sub_lines <- strsplit(b$plot$labels$subtitle, "\n", fixed = TRUE)[[1]]
sub_w_in <- max(vapply(sub_lines, function(s) {
  grid::convertWidth(
    grid::grobWidth(grid::textGrob(s, gp = grid::gpar(fontsize = 11.5))),
    "in", valueOnly = TRUE
  )
}, numeric(1)))
stopifnot("R09 subtitle fits device" = sub_w_in <= usable_in)

message("Post-build render checks passed: 9/9")

# ------------------------------------------------------------------- export

png_path <- file.path(OUT_DIR, "stats_post29_standard_error_contributions.png")
svg_path <- file.path(OUT_DIR, "stats_post29_standard_error_contributions.svg")

ggsave(png_path, p, width = PLOT_WIDTH, height = PLOT_HEIGHT,
       dpi = 300, bg = "white")
message("Wrote: ", png_path)

if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, p, width = PLOT_WIDTH, height = PLOT_HEIGHT,
         device = svglite::svglite, bg = "white")
  message("Wrote: ", svg_path)
} else {
  message("svglite not installed. SVG export skipped.")
}

message("done")
