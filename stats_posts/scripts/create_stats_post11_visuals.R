# =============================================================================
# Stats Post 11: Random Variables and Expected Value
# Visual script -- field goal attempts as a discrete random variable
# =============================================================================
# Panel 1: E(X) by kick distance bucket, navy bars, red dashed reference line
#          at the overall E(X) so the distance-dependence is visible against
#          a single fixed baseline.
# Panel 2: The two-path convergence -- probability-weighted sum (stacked gray
#          make/miss contributions) vs. direct average (single navy bar) --
#          both landing on the same value, connected by a red dashed line.
#
# Convention: navy = actual/correct/final values, gray = component inputs,
# red dashed = reference or contrast line. 300 DPI. stats_posts/output/plots/. Prefix
# stats_post11_.
# =============================================================================

# ---- CONFIGURATION ----------------------------------------------------------
OUTPUT_DIR <- "stats_posts/output/plots"
FILE_PREFIX <- "stats_post11_"
DPI <- 300

NAVY <- "#1B2A4A"
GRAY <- "#9CA3AF"
RED  <- "#C0392B"

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

library(ggplot2)
library(dplyr)

has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
if (has_patchwork) library(patchwork)

# ---- DATA (verified console output from pull_stats_post11_expected_value_demo_data.R) ----

overall_ex <- 2.5671

by_bucket <- tibble::tibble(
  distance_bucket = factor(
    c("Under 30", "30 to 39", "40 to 49", "50 plus"),
    levels = c("Under 30", "30 to 39", "40 to 49", "50 plus")
  ),
  expected_value = c(2.94, 2.79, 2.53, 2.06)
)

convergence <- tibble::tibble(
  method = factor(
    c("Weighted sum", "Direct average"),
    levels = c("Weighted sum", "Direct average")
  ),
  make_component = c(2.5671, NA_real_),
  miss_component = c(0, NA_real_),
  direct_value = c(NA_real_, 2.5671)
)

# ---- PANEL 1: E(X) by distance bucket ---------------------------------------

p1 <- ggplot(by_bucket, aes(x = distance_bucket, y = expected_value)) +
  geom_hline(yintercept = overall_ex, linetype = "dashed", color = RED, linewidth = 0.7) +
  geom_col(fill = NAVY, width = 0.6) +
  geom_text(
    aes(label = sprintf("%.2f", expected_value)),
    vjust = -0.6, size = 4.2, color = NAVY, fontface = "bold"
  ) +
  annotate(
    "text", x = 4, y = overall_ex + 0.18,
    label = sprintf("Overall E(X) = %.4f", overall_ex),
    hjust = 1, size = 3.4, color = RED
  ) +
  scale_y_continuous(limits = c(0, 3.3), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Expected value depends on which random variable you defined",
    subtitle = "E(X) for a field goal attempt, by kick distance (2025 regular season)",
    x = NULL,
    y = "E(X) in points"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# ---- PANEL 2: two-path convergence -------------------------------------------

stack_df <- tibble::tibble(
  method = factor("Weighted sum", levels = c("Weighted sum", "Direct average")),
  component = factor(c("Make: 0.8557 x 3", "Miss: 0.1443 x 0"),
                      levels = c("Miss: 0.1443 x 0", "Make: 0.8557 x 3")),
  value = c(2.5671, 0)
)

direct_df <- tibble::tibble(
  method = factor("Direct average", levels = c("Weighted sum", "Direct average")),
  value = 2.5671
)

p2 <- ggplot() +
  geom_hline(yintercept = overall_ex, linetype = "dashed", color = RED, linewidth = 0.7) +
  geom_col(
    data = stack_df, aes(x = method, y = value, fill = component),
    width = 0.5
  ) +
  geom_col(
    data = direct_df, aes(x = method, y = value),
    fill = NAVY, width = 0.5
  ) +
  geom_text(
    data = data.frame(method = c("Weighted sum", "Direct average"), value = c(2.5671, 2.5671)),
    aes(x = method, y = value, label = sprintf("%.4f", value)),
    vjust = -0.6, size = 4.2, fontface = "bold", color = NAVY
  ) +
  scale_fill_manual(values = c("Make: 0.8557 x 3" = GRAY, "Miss: 0.1443 x 0" = "gray90"), guide = "none") +
  scale_y_continuous(limits = c(0, 3.3), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Two ways to compute the same number",
    subtitle = "Probability-weighted sum vs. plain average of 1,088 real attempts",
    x = NULL,
    y = "E(X) in points"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# ---- COMBINE AND EXPORT -------------------------------------------------------

if (has_patchwork) {
  combined <- p1 / p2 +
    plot_annotation(
      caption = "Source: 2025 nflfastR play-by-play, 1,088 field goal attempts, regular season only."
    )

  out_path <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "field_goal_expected_value.png"))
  ggsave(out_path, combined, width = 7.5, height = 9, dpi = DPI, bg = "white")
  cat("Saved combined two-panel figure to:", out_path, "\n")
} else {
  message("patchwork not installed -- saving panels separately.")

  out_path_1 <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "field_goal_expected_value_by_distance.png"))
  ggsave(out_path_1, p1, width = 7.5, height = 4.5, dpi = DPI, bg = "white")
  cat("Saved panel 1 to:", out_path_1, "\n")

  out_path_2 <- file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "field_goal_expected_value_convergence.png"))
  ggsave(out_path_2, p2, width = 7.5, height = 4.5, dpi = DPI, bg = "white")
  cat("Saved panel 2 to:", out_path_2, "\n")
}

# ---- CONSOLE SUMMARY ----------------------------------------------------------
cat("\n=== VISUAL SCRIPT SUMMARY ===\n")
cat("Panel 1: E(X) by distance bucket, navy bars, red dashed line at overall E(X) =",
    overall_ex, "\n")
cat("Panel 2: Weighted sum (stacked gray make/miss) vs. direct average (navy),",
    "both at", overall_ex, "\n")
cat("Output directory:", OUTPUT_DIR, "\n")
cat("DPI:", DPI, "\n")
