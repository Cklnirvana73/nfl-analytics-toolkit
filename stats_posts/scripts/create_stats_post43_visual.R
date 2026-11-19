# ==============================================================================
# create_stats_post43_visual.R
# Statistics Series Post 43: Confounding Variables
#
# Two panels sharing one position axis (QB, RB, WR, TE, highest to lowest
# scoring). Left: each group's position mix. Right: weeks 9 to 18 fantasy
# points per game by position in the comparison group.
#
# Concept test: the chart shows the post's core claim, both confounder
# conditions at once: position differs between the groups (left) and goes
# with scoring on its own, among players who were all used in week 3 (right).
# Visual purpose (NFL-38): reading across a row shows the unused group
# stacked toward the lowest-scoring position (TE) and away from the highest
# (QB), which the post can only state as separate numbers.
# Salience: TE and QB rows at full strength; RB and WR muted.
# Color: gold = unused group, navy = comparison group. Neither value is wrong.
#
# Numbers are HARDCODED from the verified console output of stats_post43_pull.R
# (2026-11-05, cache dated 2026-09-17), sections 1 (position mix) and 3B
# (control scoring by position). Never re-simulated. A reproduction gate checks
# them against the CSVs that pull script wrote.
#
# Run from the nfl-analytics-toolkit project root:
#   source("stats_posts/scripts/create_stats_post43_visual.R")
# Outputs: stats_posts/output/plots/stats_post43_position_confounder.png and .svg (1200x800)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(glue)
})

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------
OUT_DIR    <- here::here("stats_posts", "output", "plots")
DATA_DIR   <- here::here("stats_posts", "output", "data")
FILE_STEM  <- "stats_post43_position_confounder"
WIDTH_PX   <- 1200
HEIGHT_PX  <- 800
DPI        <- 300
TOL_PCT    <- 0.05    # shares hardcoded to 1 decimal
TOL_PPG    <- 1e-3    # PPG hardcoded to 4 significant digits

COL_UNUSED <- "#E69F00"   # Okabe-Ito gold: unused in week 3
COL_COMP   <- "#0072B2"   # Okabe-Ito navy: comparison group
COL_TEXT   <- "gray20"
ALPHA_MUTE <- 0.40        # RB and WR rows

POSITIONS  <- c("QB", "RB", "WR", "TE")   # plotted top to bottom
ROW_Y      <- c(QB = 4, RB = 3, WR = 2, TE = 1)
EMPHASIS   <- c("QB", "TE")

# Verified values: position share of each group, % (pull section 1, pos25)
PCT_UNUSED <- c(QB = 1.7,  RB = 25.2, WR = 35.7, TE = 37.4)
PCT_COMP   <- c(QB = 16.0, RB = 26.1, WR = 41.1, TE = 16.7)
# Verified values: weeks 9-18 PPG by position, comparison group (pull 3B)
PPG_COMP   <- c(QB = 20.0747, RB = 12.9378, WR = 10.3631, TE = 7.9457)
N_UNUSED     <- 115L
N_COMPARISON <- 1829L

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

gate <- function(ok, msg) {
  if (!isTRUE(ok)) stop("GATE FAILED: ", msg, call. = FALSE)
  cat("  [PASS] ", msg, "\n", sep = "")
}

# ------------------------------------------------------------------------------
# REPRODUCTION GATE: hardcoded values must match the pull script's CSVs
# ------------------------------------------------------------------------------
cat("\nReproduction gate\n")
f_pos <- file.path(DATA_DIR, "stats_post43_r25_position.csv")
f_out <- file.path(DATA_DIR, "stats_post43_r25_pos_outcome.csv")
gate(file.exists(f_pos) && file.exists(f_out),
     "stats_post43 position CSVs exist (run stats_post43_pull.R first)")

pos <- utils::read.csv(f_pos, stringsAsFactors = FALSE)
out <- utils::read.csv(f_out, stringsAsFactors = FALSE)
gate(all(c("group", "position", "n", "pct") %in% names(pos)),
     "position CSV has group, position, n, pct")
gate(all(c("group", "position", "n", "mean_outcome_ppg") %in% names(out)),
     "position-outcome CSV has group, position, n, mean_outcome_ppg")

pct_of <- function(g, p) pos$pct[pos$group == g & pos$position == p]
for (p in POSITIONS) {
  gate(length(pct_of("treatment", p)) == 1L &&
         abs(pct_of("treatment", p) - PCT_UNUSED[[p]]) < TOL_PCT &&
         abs(pct_of("control", p)   - PCT_COMP[[p]])   < TOL_PCT,
       glue("{p} shares match ({PCT_UNUSED[[p]]}% unused, {PCT_COMP[[p]]}% comparison)"))
  ppg_csv <- out$mean_outcome_ppg[out$group == "control" & out$position == p]
  gate(length(ppg_csv) == 1L && abs(ppg_csv - PPG_COMP[[p]]) < TOL_PPG,
       glue("{p} comparison weeks 9-18 PPG matches ({round(PPG_COMP[[p]], 2)})"))
}
gate(sum(pos$n[pos$group == "treatment"]) == N_UNUSED &&
       sum(pos$n[pos$group == "control"]) == N_COMPARISON,
     glue("group sizes {N_UNUSED} and {N_COMPARISON}"))

# ------------------------------------------------------------------------------
# PRE-BUILD ASSERTIONS: the post's claims, and the chart's reading, in code
# ------------------------------------------------------------------------------
cat("\nPost claim assertions\n")
gate(PCT_UNUSED[["TE"]] == 37.4 && PCT_COMP[["TE"]] == 16.7,
     "TE 37.4% vs 16.7% (post wording)")
gate(PCT_UNUSED[["QB"]] == 1.7 && PCT_COMP[["QB"]] == 16.0,
     "QB 1.7% vs 16.0% (post wording)")
gate(round(PPG_COMP[["QB"]], 2) == 20.07 && round(PPG_COMP[["TE"]], 2) == 7.95,
     "QB 20.07 and TE 7.95 PPG (post wording)")
gate(names(which.max(PPG_COMP)) == "QB" && names(which.min(PPG_COMP)) == "TE",
     "QB highest and TE lowest of the four (post: 'the highest and lowest')")
gate(identical(names(sort(PPG_COMP, decreasing = TRUE)), POSITIONS),
     "rows run highest to lowest scoring, top to bottom")
gate(PCT_UNUSED[["TE"]] > PCT_COMP[["TE"]] && PCT_UNUSED[["QB"]] < PCT_COMP[["QB"]],
     "unused group heavier on TE and lighter on QB (the chart's reading)")
gate(abs(sum(PCT_UNUSED) - 100) < 0.2 && abs(sum(PCT_COMP) - 100) < 0.2,
     "each group's shares sum to 100%")

# ------------------------------------------------------------------------------
# PLOT DATA
# ------------------------------------------------------------------------------
BAR_H   <- 0.17   # half-height of each share bar (two per row)
BAR_H1  <- 0.22   # half-height of each scoring bar (one per row)
OFFSET  <- 0.19   # vertical offset of the two share bars within a row

PANEL_L <- "Share of each group"
PANEL_R <- "Weeks 9 to 18 points per game,\ncomparison group"
panel_f <- function(x) factor(x, levels = c(PANEL_L, PANEL_R))

alpha_of <- function(p) ifelse(p %in% EMPHASIS, 1, ALPHA_MUTE)

share_df <- dplyr::bind_rows(
  dplyr::tibble(position = POSITIONS, series = "Unused in week 3",
                value = unname(PCT_UNUSED[POSITIONS]),
                y = unname(ROW_Y[POSITIONS]) + OFFSET),
  dplyr::tibble(position = POSITIONS, series = "Comparison group",
                value = unname(PCT_COMP[POSITIONS]),
                y = unname(ROW_Y[POSITIONS]) - OFFSET)
) %>%
  dplyr::mutate(panel = panel_f(PANEL_L),
                alpha = alpha_of(position),
                label = sprintf("%.1f%%", value),
                series = factor(series, levels = c("Unused in week 3", "Comparison group")))

ppg_df <- dplyr::tibble(position = POSITIONS,
                        series   = factor("Comparison group",
                                          levels = c("Unused in week 3", "Comparison group")),
                        value    = unname(PPG_COMP[POSITIONS]),
                        y        = unname(ROW_Y[POSITIONS])) %>%
  dplyr::mutate(panel = panel_f(PANEL_R),
                alpha = alpha_of(position),
                label = sprintf("%.2f", value))

stopifnot("Plot data has NA values" = !anyNA(share_df$value) && !anyNA(ppg_df$value),
          nrow(share_df) == 8L, nrow(ppg_df) == 4L)

# Invisible points widen each panel so value labels fit (checked in Python)
X_MAX_L <- 52
X_MAX_R <- 25
blank_df <- dplyr::tibble(panel = panel_f(c(PANEL_L, PANEL_R)),
                          x = c(X_MAX_L, X_MAX_R), y = c(2.5, 2.5))

# ------------------------------------------------------------------------------
# BUILD
# ------------------------------------------------------------------------------
p <- ggplot() +
  # Layer 1: invisible range setters
  geom_blank(data = blank_df, aes(x = x, y = y)) +
  # Layer 2: position mix, two bars per row
  geom_rect(data = share_df,
            aes(xmin = 0, xmax = value, ymin = y - BAR_H, ymax = y + BAR_H,
                fill = series, alpha = alpha)) +
  # Layer 3: comparison-group scoring, one bar per row
  geom_rect(data = ppg_df,
            aes(xmin = 0, xmax = value, ymin = y - BAR_H1, ymax = y + BAR_H1,
                fill = series, alpha = alpha)) +
  # Layer 4 and 5: value labels just past each bar end
  geom_text(data = share_df,
            aes(x = value + 1.2, y = y, label = label, alpha = alpha),
            hjust = 0, size = 2.1, color = COL_TEXT, fontface = "bold") +
  geom_text(data = ppg_df,
            aes(x = value + 0.6, y = y, label = label, alpha = alpha),
            hjust = 0, size = 2.3, color = COL_TEXT, fontface = "bold") +
  facet_wrap(~ panel, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = c("Unused in week 3" = COL_UNUSED,
                               "Comparison group" = COL_COMP),
                    breaks = c("Unused in week 3", "Comparison group"),
                    name = NULL) +
  scale_alpha_identity() +
  scale_y_continuous(breaks = unname(ROW_Y), labels = names(ROW_Y),
                     expand = expansion(add = 0.1)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  coord_cartesian(ylim = c(0.55, 4.45), clip = "off") +
  labs(
    title    = "The unused group leaned toward the lowest-scoring position",
    subtitle = glue("Players unused in week 3 ({N_UNUSED}) vs players used every game of ",
                    "weeks 1 to 8 ({format(N_COMPARISON, big.mark = ',')}),\n",
                    "2011 to 2025 player-seasons. Rows run from highest to lowest scoring."),
    x        = NULL,
    y        = NULL,
    caption  = "Right panel uses only the comparison group, where every player was used in week 3. Toolkit fantasy scoring."
  ) +
  theme_minimal(base_size = 7) +
  theme(
    plot.title            = element_text(face = "bold", size = 9, hjust = 0, color = COL_TEXT),
    plot.title.position   = "plot",
    plot.subtitle         = element_text(size = 6.2, hjust = 0, color = "gray35"),
    plot.caption          = element_text(size = 5.2, hjust = 1, color = "gray50"),
    plot.caption.position = "plot",
    strip.text            = element_text(size = 6.5, face = "bold", color = COL_TEXT, hjust = 0),
    axis.text.y           = element_text(size = 7, face = "bold", color = COL_TEXT),
    axis.text.x           = element_blank(),
    panel.grid            = element_blank(),
    panel.spacing.x       = unit(10, "pt"),
    legend.position       = "top",
    legend.justification  = "left",
    legend.text           = element_text(size = 6.2, color = COL_TEXT),
    legend.key.size       = unit(7, "pt"),
    legend.margin         = margin(0, 0, 0, 0),
    plot.margin           = margin(8, 10, 6, 8),
    plot.background       = element_rect(fill = "white", color = NA)
  )

# ------------------------------------------------------------------------------
# POST-BUILD ASSERTIONS
# ------------------------------------------------------------------------------
cat("\nPost-build assertions\n")
gate(inherits(p, "ggplot"), "plot object built")
# Layer order: 1 blank, 2 share bars, 3 scoring bars, 4 share labels, 5 scoring labels.
# Indexed by number, not p$layers, so it runs on ggplot2 3.x and 4.0 alike.
share_built <- ggplot2::layer_data(p, i = 2L)
ppg_built   <- ggplot2::layer_data(p, i = 3L)
gate(nrow(share_built) == 8L && !anyNA(share_built$xmax), "8 share bars, none NA")
gate(nrow(ppg_built) == 4L && !anyNA(ppg_built$xmax), "4 scoring bars, none NA")
gate(all(abs(sort(share_built$xmax) - sort(c(PCT_UNUSED, PCT_COMP))) < 1e-9),
     "share bar lengths equal the eight verified shares")
gate(all(abs(sort(ppg_built$xmax) - sort(PPG_COMP)) < 1e-9),
     "scoring bar lengths equal the four verified PPG values")
gate(max(share_df$value) + 1.2 < X_MAX_L && max(ppg_df$value) + 0.6 < X_MAX_R,
     "every value label starts inside its panel")
gate(all(share_built$PANEL == 1L) && all(ppg_built$PANEL == 2L),
     "shares in the left panel, scoring in the right panel")

# ------------------------------------------------------------------------------
# SAVE
# ------------------------------------------------------------------------------
png_path <- file.path(OUT_DIR, paste0(FILE_STEM, ".png"))
svg_path <- file.path(OUT_DIR, paste0(FILE_STEM, ".svg"))

ggsave(png_path, plot = p, width = WIDTH_PX, height = HEIGHT_PX,
       units = "px", dpi = DPI, bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(svg_path, plot = p, width = WIDTH_PX, height = HEIGHT_PX,
         units = "px", dpi = DPI, bg = "white")
} else {
  warning("svglite not installed; SVG not written. install.packages('svglite')")
}

# ------------------------------------------------------------------------------
# CONSOLE SUMMARY
# ------------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("Stats Post 43 visual\n")
cat(glue("  TE: {PCT_UNUSED[['TE']]}% unused vs {PCT_COMP[['TE']]}% comparison, ",
         "{round(PPG_COMP[['TE']], 2)} PPG | QB: {PCT_UNUSED[['QB']]}% vs ",
         "{PCT_COMP[['QB']]}%, {round(PPG_COMP[['QB']], 2)} PPG"), "\n")
cat("  PNG: ", png_path, "\n", sep = "")
cat("  SVG: ", if (file.exists(svg_path)) svg_path else "not written", "\n", sep = "")
cat(strrep("=", 70), "\n", sep = "")
