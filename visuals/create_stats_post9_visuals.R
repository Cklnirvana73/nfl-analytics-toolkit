# ============================================================
# create_stats_post9_visuals.R
# Statistics Series -- Post 9: Conditional Probability
# Output: output/plots/stats_post9_conditional_probability.png
# Style: navy bars, red reference line (unconditional rate)
# ============================================================

library(ggplot2)
library(dplyr)

# ---- CONFIGURATION -----------------------------------------
OUTPUT_DIR  <- "output/plots"
FILE_PREFIX <- "stats_post9_"
DPI         <- 300
NAVY        <- "#1a3a5c"
RED         <- "#c0392b"
LIGHT_GRAY  <- "#f5f5f5"

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- DATA (sourced from demo console output) ----------------
UNCONDITIONAL_RATE <- 40.2

conditional <- data.frame(
  bucket = factor(
    c("1-2 yards", "3-4 yards", "5-7 yards", "8+ yards"),
    levels = c("8+ yards", "5-7 yards", "3-4 yards", "1-2 yards")  # bottom-to-top render
  ),
  rate         = c(63.1, 49.4, 40.0, 24.0),
  n_plays      = c(1430, 1157, 1553, 2661),
  pct_of_total = c(21.0, 17.0, 22.8, 39.1)
)

# ---- PANEL 1: Conversion rate by distance ------------------
p1 <- ggplot(conditional, aes(x = rate, y = bucket)) +
  geom_col(fill = NAVY, width = 0.6) +
  geom_vline(
    xintercept = UNCONDITIONAL_RATE,
    color      = RED,
    linewidth  = 0.9,
    linetype   = "dashed"
  ) +
  geom_text(
    aes(label = paste0(rate, "%")),
    hjust  = -0.15,
    size   = 3.8,
    color  = "gray20"
  ) +
  annotate(
    "text",
    x      = UNCONDITIONAL_RATE + 0.8,
    y      = 4.6,
    label  = paste0("Overall: ", UNCONDITIONAL_RATE, "%"),
    color  = RED,
    size   = 3.3,
    hjust  = 0
  ) +
  scale_x_continuous(
    limits = c(0, 80),
    expand = expansion(mult = c(0, 0.05)),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Conversion rate by yards to go",
    subtitle = "Dashed line = unconditional rate (40.2%)",
    x        = "3rd-down conversion rate",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 10, color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = LIGHT_GRAY, color = NA),
    panel.background   = element_rect(fill = LIGHT_GRAY, color = NA)
  )

# ---- PANEL 2: Share of 3rd downs by distance ---------------
p2 <- ggplot(conditional, aes(x = pct_of_total, y = bucket)) +
  geom_col(fill = NAVY, width = 0.6) +
  geom_text(
    aes(label = paste0(pct_of_total, "%")),
    hjust  = -0.15,
    size   = 3.8,
    color  = "gray20"
  ) +
  scale_x_continuous(
    limits = c(0, 55),
    expand = expansion(mult = c(0, 0.05)),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Share of all 3rd-down plays",
    subtitle = "8+ yards accounts for nearly 4 in 10 plays",
    x        = "% of all 3rd-down plays",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 10, color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = LIGHT_GRAY, color = NA),
    panel.background   = element_rect(fill = LIGHT_GRAY, color = NA)
  )

# ---- COMBINE AND SAVE --------------------------------------
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)

  combined <- p1 + p2 +
    plot_annotation(
      title   = "Conditional Probability: 3rd-Down Conversion, 2025 NFL Season",
      caption = "Data: nflfastR 2025 regular season | Pass and run plays only | n = 6,801",
      theme   = theme(
        plot.title      = element_text(face = "bold", size = 13, hjust = 0),
        plot.caption    = element_text(size = 9, color = "gray50"),
        plot.background = element_rect(fill = LIGHT_GRAY, color = NA)
      )
    )

  out_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "conditional_probability.png"))
  ggsave(out_path, combined, width = 11, height = 5, dpi = DPI, bg = LIGHT_GRAY)
  cat("Saved:", out_path, "\n")

} else {
  message("patchwork not installed -- saving panels separately.")
  message("Install with: install.packages('patchwork')")

  p1_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "panel1_conversion_rate.png"))
  p2_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "panel2_distribution.png"))
  ggsave(p1_path, p1, width = 6, height = 5, dpi = DPI, bg = LIGHT_GRAY)
  ggsave(p2_path, p2, width = 6, height = 5, dpi = DPI, bg = LIGHT_GRAY)
  cat("Saved panel 1:", p1_path, "\n")
  cat("Saved panel 2:", p2_path, "\n")
}

# ---- CONSOLE SUMMARY ---------------------------------------
cat("\n--- POST 9 VISUAL SUMMARY ---\n")
cat("Panel 1: Conversion rate by yards to go (navy bars, red dashed reference)\n")
cat("Panel 2: Share of 3rd-down plays by distance bucket (navy bars)\n")
cat("\nKey figures:\n")
cat("  Unconditional rate:  40.2%\n")
cat("  1-2 yards:  63.1%  (21.0% of plays)\n")
cat("  3-4 yards:  49.4%  (17.0% of plays)\n")
cat("  5-7 yards:  40.0%  (22.8% of plays)\n")
cat("  8+ yards:   24.0%  (39.1% of plays)\n")
cat("  Total plays: 6,801 | Season: 2025 REG\n")
