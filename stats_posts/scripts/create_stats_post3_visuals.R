# ==============================================================================
# Statistics Series | Post 3: Data Types
# Visuals for LinkedIn post
#
# Outputs:
#   stats_posts/output/plots/stats_post3_demo_output.png
#   stats_posts/output/plots/stats_post3_reference_hierarchy.png
#
# Requires: ggplot2, dplyr, tibble
# ==============================================================================

# CONFIGURATION ----------------------------------------------------------------
POST_NUM   <- 3L
CONCEPT    <- "Data Types"
OUTPUT_DIR <- "stats_posts/output/plots"
DPI        <- 300L

# Color palette (light mode, WCAG-accessible ramps)
COL_RED_BG  <- "#FCEBEB"; COL_RED_TXT  <- "#A32D2D"; COL_RED_BD  <- "#E24B4A"
COL_AMB_BG  <- "#FAEEDA"; COL_AMB_TXT  <- "#854F0B"; COL_AMB_BD  <- "#BA7517"
COL_GRN_BG  <- "#E1F5EE"; COL_GRN_TXT  <- "#085041"; COL_GRN_BD  <- "#0F6E56"
COL_BLU_BG  <- "#E6F1FB"; COL_BLU_TXT  <- "#0C447C"; COL_BLU_BD  <- "#185FA5"
COL_GRY_BG  <- "#F1EFE8"; COL_GRY_TXT  <- "#5F5E5A"; COL_GRY_BD  <- "#888780"

COL_HDR_BG  <- "#F1EFE8"
COL_DIVIDER <- "#D3D1C7"
COL_PRI     <- "#1C1C1A"
COL_SEC     <- "#5F5E5A"
COL_MUT     <- "#888780"

# LIBRARIES --------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tibble)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# VISUAL 1: Three Means Demo Output
# Three rows, one per variable type. Left accent border colors validity.
# Canvas: x in [0, 10], y in [0, 5]. ggsave: 10in x 5in at 300 DPI.
# ==============================================================================

# Row geometry and content
v1 <- tibble(
  y_top    = c(3.8, 2.7, 1.6),
  y_bot    = c(2.7, 1.6, 0.5),
  y_mid    = c(3.25, 2.15, 1.05),
  var_name = c("position", "draft round", "PPR points / game"),
  subtext  = c("QB, WR, RB, TE", "rounds 1 through 7", "season average"),
  type_lbl = c("nominal", "ordinal", "ratio"),
  mean_val = c("2.92", "3.05", "8.90"),
  interp   = c(
    "Falls between RB and TE.\nThat position does not exist.",
    "Rank is real. Equal spacing between\nrounds is not guaranteed.",
    "Zero is real, intervals are equal.\nThis number is interpretable."
  ),
  bd_col   = c(COL_RED_BD, COL_AMB_BD, COL_GRN_BD),
  bg_col   = c(COL_RED_BG, COL_AMB_BG, COL_GRN_BG),
  txt_col  = c(COL_RED_TXT, COL_AMB_TXT, COL_GRN_TXT)
)

p1 <- ggplot() +
  # --- Structure: outer border, header background, row dividers --------------
  annotate("rect",
    xmin = 0.14, xmax = 9.95, ymin = 0.5, ymax = 3.8,
    fill = "white", color = COL_DIVIDER, linewidth = 0.4
  ) +
  annotate("rect",
    xmin = 0.14, xmax = 9.95, ymin = 3.8, ymax = 4.45,
    fill = COL_HDR_BG, color = COL_DIVIDER, linewidth = 0.4
  ) +
  annotate("segment",
    x = 0.14, xend = 9.95, y = 1.6, yend = 1.6,
    color = COL_DIVIDER, linewidth = 0.3
  ) +
  annotate("segment",
    x = 0.14, xend = 9.95, y = 2.7, yend = 2.7,
    color = COL_DIVIDER, linewidth = 0.3
  ) +
  # --- Left accent borders (per row) -----------------------------------------
  geom_rect(
    data = v1,
    aes(xmin = 0, xmax = 0.14, ymin = y_bot, ymax = y_top, fill = bd_col),
    color = NA
  ) +
  # --- Badge backgrounds -------------------------------------------------------
  geom_rect(
    data = v1,
    aes(xmin = 2.35, xmax = 3.65, ymin = y_mid - 0.28, ymax = y_mid + 0.28,
        fill = bg_col),
    color = NA
  ) +
  scale_fill_identity() +
  # --- Header labels -----------------------------------------------------------
  annotate("text", x = 0.35, y = 4.125,
    label = "variable",
    hjust = 0, size = 2.7, color = COL_SEC, fontface = "bold"
  ) +
  annotate("text", x = 3.0, y = 4.125,
    label = "type",
    hjust = 0.5, size = 2.7, color = COL_SEC, fontface = "bold"
  ) +
  annotate("text", x = 4.75, y = 4.125,
    label = "mean()",
    hjust = 0.5, size = 2.7, color = COL_SEC, fontface = "bold"
  ) +
  annotate("text", x = 5.7, y = 4.125,
    label = "what the number means",
    hjust = 0, size = 2.7, color = COL_SEC, fontface = "bold"
  ) +
  # --- Variable names (bold) ---------------------------------------------------
  geom_text(
    data = v1,
    aes(x = 0.35, y = y_mid + 0.2, label = var_name),
    hjust = 0, size = 3.4, fontface = "bold", color = COL_PRI
  ) +
  # --- Subtexts (muted, smaller) -----------------------------------------------
  geom_text(
    data = v1,
    aes(x = 0.35, y = y_mid - 0.2, label = subtext),
    hjust = 0, size = 2.5, color = COL_MUT
  ) +
  # --- Type badge text ----------------------------------------------------------
  geom_text(
    data = v1,
    aes(x = 3.0, y = y_mid, label = type_lbl, color = txt_col),
    hjust = 0.5, size = 2.8, fontface = "bold"
  ) +
  scale_color_identity() +
  # --- Mean values (large, bold) -----------------------------------------------
  geom_text(
    data = v1,
    aes(x = 4.75, y = y_mid, label = mean_val),
    hjust = 0.5, size = 7.5, fontface = "bold", color = COL_PRI
  ) +
  # --- Interpretation text -----------------------------------------------------
  geom_text(
    data = v1,
    aes(x = 5.7, y = y_mid, label = interp),
    hjust = 0, size = 2.8, color = COL_SEC, lineheight = 1.35
  ) +
  # --- Footer note -------------------------------------------------------------
  annotate("text", x = 0.35, y = 0.22,
    label = "R printed all three without complaint. The type constraint is yours to carry.",
    hjust = 0, size = 2.4, color = COL_MUT, fontface = "italic"
  ) +
  # --- Coordinate system and theme ---------------------------------------------
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 5), expand = FALSE) +
  labs(
    title    = "Three calls to mean(). Three different answers about what that means.",
    subtitle = "2024 NFL skill position players  \u00b7  min. 8 regular season games"
  ) +
  theme_void() +
  theme(
    plot.title      = element_text(size = 10.5, face = "bold", color = COL_PRI,
                                   margin = margin(b = 3), hjust = 0),
    plot.subtitle   = element_text(size = 9, color = COL_MUT,
                                   margin = margin(b = 10), hjust = 0),
    plot.margin     = margin(14, 14, 14, 14),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  p1,
  filename = file.path(OUTPUT_DIR,
    paste0("stats_post", POST_NUM, "_demo_output.png")),
  width = 10, height = 5, dpi = DPI, bg = "white"
)


# ==============================================================================
# VISUAL 2: Measurement Level Reference Hierarchy
# Four rows, one per level. Left accent color and badge color build from
# neutral (nominal) to green (ratio). Each row adds a property.
# Canvas: x in [0, 10], y in [0, 7]. ggsave: 12in x 7in at 300 DPI.
# ==============================================================================

# Row geometry and content. Strings pre-broken at natural wrap points to
# guarantee clean two-line layout regardless of font rendering.
v2 <- tibble(
  y_top    = c(5.6, 4.2, 2.8, 1.4),
  y_bot    = c(4.2, 2.8, 1.4, 0.0),
  y_mid    = c(4.9, 3.5, 2.1, 0.7),
  type_lbl = c("nominal", "ordinal", "interval", "ratio"),
  property = c(
    "named categories,\nno natural order",
    "+ meaningful order\nbetween values",
    "+ equal spacing\nbetween all values",
    "+ true zero (makes\nratios meaningful)"
  ),
  example  = c(
    "position\n(QB, WR, RB, TE)",
    "draft round\n(round 1 > round 2)",
    "temperature in \u00b0F\n(no true zero)",
    "PPR pts/game,\ncompletion %"
  ),
  ops      = c(
    "count, mode,\nfrequency",
    "+ rank, median,\npercentile",
    "+ mean, standard\ndeviation",
    "+ multiply, divide,\nall operations"
  ),
  bd_col   = c(COL_GRY_BD, COL_BLU_BD, COL_AMB_BD, COL_GRN_BD),
  bg_col   = c(COL_GRY_BG, COL_BLU_BG, COL_AMB_BG, COL_GRN_BG),
  txt_col  = c(COL_GRY_TXT, COL_BLU_TXT, COL_AMB_TXT, COL_GRN_TXT)
)

p2 <- ggplot() +
  # --- Structure ---------------------------------------------------------------
  annotate("rect",
    xmin = 0.14, xmax = 9.95, ymin = 0.0, ymax = 5.6,
    fill = "white", color = COL_DIVIDER, linewidth = 0.4
  ) +
  annotate("rect",
    xmin = 0.14, xmax = 9.95, ymin = 5.6, ymax = 6.35,
    fill = COL_HDR_BG, color = COL_DIVIDER, linewidth = 0.4
  ) +
  annotate("segment",
    x = 0.14, xend = 9.95, y = c(1.4, 2.8, 4.2), yend = c(1.4, 2.8, 4.2),
    color = COL_DIVIDER, linewidth = 0.3
  ) +
  # --- Left accent borders -----------------------------------------------------
  geom_rect(
    data = v2,
    aes(xmin = 0, xmax = 0.14, ymin = y_bot, ymax = y_top, fill = bd_col),
    color = NA
  ) +
  # --- Badge backgrounds -------------------------------------------------------
  geom_rect(
    data = v2,
    aes(xmin = 0.22, xmax = 1.88, ymin = y_mid - 0.3, ymax = y_mid + 0.3,
        fill = bg_col),
    color = NA
  ) +
  scale_fill_identity() +
  # --- Header labels -----------------------------------------------------------
  annotate("text", x = 1.05, y = 5.975,
    label = "type",
    hjust = 0.5, size = 2.7, color = COL_SEC, fontface = "bold"
  ) +
  annotate("text", x = 2.1, y = 5.975,
    label = "what it adds",
    hjust = 0, size = 2.7, color = COL_SEC, fontface = "bold"
  ) +
  annotate("text", x = 6.2, y = 5.975,
    label = "example",
    hjust = 0, size = 2.7, color = COL_SEC, fontface = "bold"
  ) +
  annotate("text", x = 8.1, y = 5.975,
    label = "valid operations",
    hjust = 0, size = 2.7, color = COL_SEC, fontface = "bold"
  ) +
  # --- Type badge text ----------------------------------------------------------
  geom_text(
    data = v2,
    aes(x = 1.05, y = y_mid, label = type_lbl, color = txt_col),
    hjust = 0.5, size = 2.9, fontface = "bold"
  ) +
  scale_color_identity() +
  # --- Property text (bold, the building-block property) -----------------------
  geom_text(
    data = v2,
    aes(x = 2.1, y = y_mid, label = property),
    hjust = 0, size = 3.0, fontface = "bold", color = COL_PRI, lineheight = 1.35
  ) +
  # --- Example text (muted) ----------------------------------------------------
  geom_text(
    data = v2,
    aes(x = 6.2, y = y_mid, label = example),
    hjust = 0, size = 2.8, color = COL_SEC, lineheight = 1.35
  ) +
  # --- Operations text (muted) -------------------------------------------------
  geom_text(
    data = v2,
    aes(x = 8.1, y = y_mid, label = ops),
    hjust = 0, size = 2.8, color = COL_SEC, lineheight = 1.35
  ) +
  # --- Footer ------------------------------------------------------------------
  annotate("text", x = 0.35, y = -0.28,
    label = "Each level inherits all valid operations from the levels above it.",
    hjust = 0, size = 2.4, color = COL_MUT, fontface = "italic"
  ) +
  # --- Coordinate system and theme ---------------------------------------------
  coord_cartesian(xlim = c(0, 10), ylim = c(-0.5, 6.8), expand = FALSE) +
  labs(
    title = "Measurement levels and what each one permits"
  ) +
  theme_void() +
  theme(
    plot.title      = element_text(size = 10.5, face = "bold", color = COL_PRI,
                                   margin = margin(b = 10), hjust = 0),
    plot.margin     = margin(14, 14, 14, 14),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  p2,
  filename = file.path(OUTPUT_DIR,
    paste0("stats_post", POST_NUM, "_reference_hierarchy.png")),
  width = 12, height = 7, dpi = DPI, bg = "white"
)


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================
cat("\n========================================\n")
cat("Statistics Series | Post", POST_NUM, "|", CONCEPT, "\n")
cat("========================================\n")
cat("Output directory:", OUTPUT_DIR, "\n\n")
cat("Files saved:\n")
cat("  stats_post", POST_NUM, "_demo_output.png",
    "  [10in x 5in, ", DPI, "DPI]\n", sep = "")
cat("  stats_post", POST_NUM, "_reference_hierarchy.png",
    "  [12in x 7in, ", DPI, "DPI]\n", sep = "")
cat("\nCheck column widths and text wrap on both files.\n")
cat("Adjust x positions or size args if any text clips.\n")
cat("========================================\n\n")
