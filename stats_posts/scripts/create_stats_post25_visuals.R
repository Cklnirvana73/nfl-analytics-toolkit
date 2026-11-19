# =====================================================================
# create_stats_post25_visuals.R
# Statistics Series Post 25: Type II Error
#
# Single panel. Required sample size decomposed into the false-alarm
# floor (constant, anchored at zero) and the portion bought to lower
# the silence rate.
#
# All values hardcoded from verified console output of
# pull_stats_post25_type2_data.R. Nothing is re-simulated here.
#
# Render confirmed by Christian. Defects found in the first render and
# fixed below: inverted stack order, clipped title, annotation collision.
# =====================================================================

library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(here)
library(glue)

# ---- CONFIGURATION --------------------------------------------------
OUT_DIR     <- here("stats_posts", "output", "plots")
FILE_PREFIX <- "stats_post25_"
PLOT_W      <- 8
PLOT_H      <- 5.333        # 8 x 5.333 at 300 dpi = 2400 x 1600
PLOT_DPI    <- 300

NAVY  <- "#0072B2"   # portion bought to lower the silence rate
GRAY  <- "#999999"   # false-alarm floor, constant across every row
GOLD  <- "#E69F00"   # the study's actual setting

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# ---- VERIFIED DATA (hardcoded from console output) ------------------
# Section 2 sweep: alpha 0.05, mde_q 0.10 held fixed in every row.
sweep <- tibble::tibble(
  miss_rate  = c(0.50, 0.30, 0.20, 0.10, 0.05, 0.01),
  required_n = c(388L, 621L, 788L, 1054L, 1303L, 1841L)
)

FLOOR_N   <- 388L    # miss rate 0.50, z_beta = 0.000000
PROJECT_N <- 788L    # miss rate 0.20, the study's setting

plot_data <- sweep %>%
  mutate(
    floor_n     = FLOOR_N,
    bought_n    = required_n - FLOOR_N,
    label_miss  = paste0(format(miss_rate * 100, trim = TRUE), "%"),
    is_project  = miss_rate == 0.20
  )

# ---- PRE-BUILD ASSERTIONS -------------------------------------------
# Minimum pre-delivery visualization assertion
stopifnot(
  "Chart has no non-NA cells" =
    sum(!is.na(plot_data$required_n)) > 0,
  "Chart has fewer than 2 data points" =
    nrow(plot_data) >= 2
)

# Code-level checks validating the post's central claims
stopifnot(
  "Floor row must have zero bought portion" =
    plot_data$bought_n[plot_data$miss_rate == 0.50] == 0L,
  "Floor must equal the verified 388" =
    plot_data$required_n[plot_data$miss_rate == 0.50] == 388L,
  "Project setting must equal the verified 788" =
    plot_data$required_n[plot_data$miss_rate == 0.20] == 788L,
  "Post claims 400 player-weeks buy the silence reduction" =
    PROJECT_N - FLOOR_N == 400L,
  "Post claims slightly more than half the sample is the bought portion" =
    (PROJECT_N - FLOOR_N) / PROJECT_N > 0.50,
  "Floor must be constant across every row" =
    length(unique(plot_data$floor_n)) == 1L,
  "Required n must increase as miss rate falls" =
    all(diff(rev(plot_data$required_n)) < 0),
  "Bought portion must be non-negative everywhere" =
    all(plot_data$bought_n >= 0L),
  "Stack components must reconcile to the total" =
    all(plot_data$floor_n + plot_data$bought_n == plot_data$required_n),
  "Exactly one row is flagged as the project setting" =
    sum(plot_data$is_project) == 1L,
  "No missing values in any plotted column" =
    !any(is.na(plot_data %>% select(miss_rate, floor_n, bought_n)))
)
message("All pre-build assertions passed.")

# ---- RESHAPE FOR STACKING -------------------------------------------
stack_data <- plot_data %>%
  select(label_miss, miss_rate, floor_n, bought_n) %>%
  pivot_longer(
    cols      = c(floor_n, bought_n),
    names_to  = "component",
    values_to = "n"
  ) %>%
  mutate(
    component = factor(
      component,
      levels = c("floor_n", "bought_n"),
      labels = c("Buys false alarm protection",
                 "Buys a lower silence rate")
    ),
    # ascending miss rate -> ggplot draws lowest at bottom, so
    # reverse for a top-down read from 50% down to 1%
    label_miss = factor(label_miss,
                        levels = rev(plot_data$label_miss))
  )

stopifnot(
  "Reshape lost or duplicated rows" =
    nrow(stack_data) == nrow(plot_data) * 2L
)

# ---- BUILD PANEL ----------------------------------------------------
p <- ggplot(stack_data, aes(x = n, y = label_miss, fill = component)) +
  # reverse = TRUE puts the first factor level (the floor) at x = 0
  geom_col(width = 0.68, position = position_stack(reverse = TRUE)) +
  geom_vline(xintercept = FLOOR_N, linetype = "dashed",
             color = "gray35", linewidth = 0.5) +
  scale_fill_manual(values = c(
    "Buys false alarm protection" = GRAY,
    "Buys a lower silence rate"   = NAVY
  )) +
  scale_x_continuous(
    labels = scales::comma,
    breaks = seq(0, 2000, by = 400),
    expand = expansion(mult = c(0, 0.18))
  ) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.35))) +
  geom_text(
    data = plot_data %>%
      mutate(label_miss = factor(label_miss,
                                 levels = rev(plot_data$label_miss))),
    aes(x = required_n, y = label_miss,
        label = scales::comma(required_n)),
    inherit.aes = FALSE,
    hjust = -0.22, size = 3.4, color = "gray25"
  ) +
  # Placed fully above the top bar, clear of the 388 total label
  annotate("text", x = 640, y = 6.95,
           label = "388 is the floor. At a 50% silence rate nothing\nis bought beyond false alarm protection.",
           hjust = 0, vjust = 1, size = 3.2, color = "gray25") +
  annotate("text", x = 900, y = 4.55,
           label = "The study set 20%. 400 of its 788 player-weeks,\nslightly more than half, exist only to move the\nsilence rate from one in two to one in five.",
           hjust = 0, vjust = 1, size = 3.2, color = GOLD,
           fontface = "bold") +
  labs(
    title    = "Half the sample buys one thing: a lower chance of missing a real difference",
    subtitle = "Player-weeks per position required to detect a difference of the pre-set size, false alarm rate held at 0.05 in every row",
    x        = "Required player-weeks per position",
    y        = "Silence rate (chance of missing a real difference)",
    fill     = NULL,
    caption  = "Sample size formula from the NFL predictive validity study, evaluated across silence rates the study does not use"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = rel(0.98), hjust = 0),
    plot.subtitle    = element_text(color = "gray40", size = rel(0.80), hjust = 0),
    plot.caption     = element_text(color = "gray60", size = rel(0.68), hjust = 1),
    axis.title       = element_text(color = "gray30", size = rel(0.85)),
    axis.text        = element_text(color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position  = "top",
    legend.text      = element_text(size = rel(0.82)),
    plot.margin      = margin(12, 22, 10, 12)
  )

print(p)

# ---- POST-BUILD DRAW-ORDER CHECK ------------------------------------
# Inspects rendered geometry, not input data. Catches the inverted-stack
# defect that data-level assertions cannot see.
built      <- ggplot_build(p)$data[[1]]
floor_bars <- built[built$fill == GRAY, ]
stopifnot(
  "Floor segment must start at x = 0 in every bar" =
    all(abs(floor_bars$xmin) < 1e-8),
  "Floor segment must end at 388 in every bar" =
    all(abs(floor_bars$xmax - FLOOR_N) < 1e-8)
)
message("Draw-order check passed: floor anchored at zero in all bars.")

# ---- EXPORT ---------------------------------------------------------
png_path <- file.path(OUT_DIR, paste0(FILE_PREFIX, "sample_size_split.png"))
ggsave(png_path, plot = p, width = PLOT_W, height = PLOT_H, dpi = PLOT_DPI)
cat("Saved PNG:", png_path, "\n")

if (requireNamespace("svglite", quietly = TRUE)) {
  svg_path <- file.path(OUT_DIR, paste0(FILE_PREFIX, "sample_size_split.svg"))
  ggsave(svg_path, plot = p, width = PLOT_W, height = PLOT_H)
  cat("Saved SVG:", svg_path, "\n")
} else {
  cat("svglite not installed. SVG skipped, PNG written.\n")
}

# ---- CONSOLE SUMMARY ------------------------------------------------
cat("\n---- POST 25 VISUAL SUMMARY ----\n")
cat(glue("Rows plotted        : {nrow(plot_data)}\n"))
cat(glue("Floor (n)           : {FLOOR_N} at a 50% silence rate\n"))
cat(glue("Project setting (n) : {PROJECT_N} at a 20% silence rate\n"))
cat(glue("Bought portion      : {PROJECT_N - FLOOR_N} player-weeks ",
         "({round(100 * (PROJECT_N - FLOOR_N) / PROJECT_N, 1)}% of the sample)\n"))
cat(glue("Output size         : {PLOT_W * PLOT_DPI} x {round(PLOT_H * PLOT_DPI)} px\n"))
cat("--------------------------------\n")
