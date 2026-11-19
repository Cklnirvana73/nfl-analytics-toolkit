# ============================================================
# Stats Post 12: Discrete Distributions (Binomial + Poisson)
# Visual script
# ============================================================
# Two-panel figure:
#   Panel 1 -- Binomial(n=53, p=0.9588) PMF for Buffalo's PAT
#              attempts, with the observed value (48) marked
#   Panel 2 -- Poisson(lambda=0.6985) predicted vs actual
#              interceptions per team-game, by count bucket
#
# Numbers embedded below are verified from this session's
# nflreadr::load_pbp(2025) pull (pull_stats_post12_discrete_
# distributions_demo_data.R console output). No re-pull needed.
#
# Color convention: navy = actual/observed values, red =
# fitted/predicted model values.
# ============================================================

library(ggplot2)
library(dplyr)

has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
if (has_patchwork) library(patchwork)

# ---- CONFIGURATION ----
OUTPUT_DIR <- "stats_posts/output/plots"
FILE_PREFIX <- "stats_post12_"
DPI <- 300
NAVY <- "#1f3864"
RED <- "#c00000"

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ------------------------------------------------------------
# Panel 1 data: Binomial(n = 53, p = 0.9588) PMF, k = 40 to 53
# ------------------------------------------------------------
n_trials <- 53
p_success <- 0.9588
observed_makes <- 48

k_vals <- 40:53
pmf_vals <- dbinom(k_vals, size = n_trials, prob = p_success)

binom_df <- data.frame(
  k = k_vals,
  pmf = pmf_vals
) %>%
  mutate(is_observed = k == observed_makes)

z_score <- round((observed_makes - n_trials * p_success) /
                    sqrt(n_trials * p_success * (1 - p_success)), 2)
cum_prob <- round(pbinom(observed_makes, n_trials, p_success) * 100, 1)

panel1 <- ggplot(binom_df, aes(x = factor(k), y = pmf, fill = is_observed)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("FALSE" = NAVY, "TRUE" = RED), guide = "none") +
  labs(
    title = "Binomial PMF: Buffalo's PAT makes",
    subtitle = paste0(
      "n = 53, p = 0.9588 | observed = 48 (red) | z = ", z_score,
      " | P(X <= 48) = ", cum_prob, "%"
    ),
    x = "Makes (k)",
    y = "Probability"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------------
# Panel 2 data: Poisson(lambda = 0.6985) predicted vs actual
# interceptions per team-game, verified counts
# ------------------------------------------------------------
lambda_hat <- 0.6985

pois_df <- data.frame(
  bucket = factor(c("0", "1", "2", "3", "4+"), levels = c("0", "1", "2", "3", "4+")),
  actual = c(271, 189, 64, 17, 3),
  predicted = c(270.5, 189.0, 66.0, 15.4, 3.1)
)

pois_long <- pois_df %>%
  tidyr::pivot_longer(cols = c(actual, predicted), names_to = "type", values_to = "count")

panel2 <- ggplot(pois_long, aes(x = bucket, y = count, fill = type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(
    values = c("actual" = NAVY, "predicted" = RED),
    labels = c("actual" = "Actual", "predicted" = "Poisson predicted"),
    name = NULL
  ) +
  labs(
    title = "Poisson fit: interceptions per team-game",
    subtitle = paste0("lambda = ", lambda_hat, " | 544 team-game observations, 2025 season"),
    x = "Interceptions in game",
    y = "Team-games"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------------
# Combine and export
# ------------------------------------------------------------
if (has_patchwork) {
  combined <- panel1 / panel2 +
    plot_annotation(
      title = "Discrete Distributions: Binomial vs Poisson",
      caption = "2025 NFL regular season | nflfastR play-by-play",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )

  ggsave(
    filename = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "discrete_distributions.png")),
    plot = combined,
    width = 8, height = 9, dpi = DPI, bg = "white"
  )
} else {
  message("patchwork not available, saving panels separately")
  ggsave(
    filename = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "binomial_pat.png")),
    plot = panel1, width = 8, height = 4.5, dpi = DPI, bg = "white"
  )
  ggsave(
    filename = file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "poisson_interceptions.png")),
    plot = panel2, width = 8, height = 4.5, dpi = DPI, bg = "white"
  )
}

# ------------------------------------------------------------
# Console summary
# ------------------------------------------------------------
cat("=====================================================\n")
cat("Stats Post 12 visuals complete\n")
cat("=====================================================\n")
cat("Panel 1 (Binomial): n =", n_trials, "| p =", p_success,
    "| observed =", observed_makes, "| z =", z_score,
    "| P(X<=48) =", cum_prob, "%\n")
cat("Panel 2 (Poisson): lambda =", lambda_hat,
    "| team-games =", sum(pois_df$actual), "\n")
if (has_patchwork) {
  cat("Output: ", file.path(OUTPUT_DIR, paste0(FILE_PREFIX, "discrete_distributions.png")), "\n")
} else {
  cat("Output (degraded, no patchwork): two separate panel PNGs in", OUTPUT_DIR, "\n")
}
cat("=====================================================\n")
