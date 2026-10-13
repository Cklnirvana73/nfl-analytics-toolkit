# ==============================================================================
# Statistics Series | Post 8: Basic Probability Rules
# Visual for LinkedIn post
#
# Output (300 DPI):
#   output/plots/stats_post8_three_rules.png
#   (degrades to three separate PNGs if patchwork is not installed)
#
# Requires: nflfastR, ggplot2, dplyr; patchwork optional for the combined panel
# Run from project root. Internet access required for nflfastR on first pull.
#
# SCOPE NOTE
#   Every probability below is recomputed live from 2025 nflfastR play by play,
#   so the figure cannot drift from the numbers in the post. The three panels
#   illustrate the three textbook rules on the same season of real plays.
# ==============================================================================

# CONFIGURATION ----------------------------------------------------------------
POST_NUM   <- 8L
CONCEPT    <- "Basic Probability Rules"
SEASON     <- 2025L
OUTPUT_DIR <- "output/plots"
DPI        <- 300L
WIDTH_IN   <- 9
HEIGHT_IN  <- 11

# Colors (locked series palette) -----------------------------------------------
COL_ACTUAL  <- "#1A5276"   # navy, the correct or actual value
COL_NAIVE   <- "#C0392B"   # red, the naive or assumption based value
COL_NEUTRAL <- "#89A9C8"   # steel, second outcome where nothing misleads
COL_TEXT    <- "#1C1C1A"
COL_SUBTEXT <- "#5F5E5A"
COL_BG      <- "#FAFAF8"
COL_PANEL   <- "#FFFFFF"
COL_GRID    <- "#E8E6DF"
COL_STRIP   <- "#ECEAE3"

# LIBRARIES --------------------------------------------------------------------
library(nflfastR)
library(ggplot2)
library(dplyr)

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# SHARED THEME -----------------------------------------------------------------
theme_stats_series <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = COL_BG,    color = NA),
      panel.background = element_rect(fill = COL_PANEL, color = NA),
      panel.grid.major = element_line(color = COL_GRID,  linewidth = 0.4),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = COL_STRIP,  color = NA),
      strip.text       = element_text(face = "bold", size = 11, color = COL_TEXT),
      plot.title       = element_text(face = "bold", size = 13, color = COL_TEXT,
                                      margin = margin(b = 4)),
      plot.subtitle    = element_text(size = 9.5, color = COL_SUBTEXT,
                                      margin = margin(b = 8)),
      plot.caption     = element_text(size = 9,  color = COL_SUBTEXT,
                                      margin = margin(t = 8)),
      axis.title       = element_text(size = 9.5, color = COL_SUBTEXT),
      axis.text        = element_text(size = 9,  color = COL_SUBTEXT),
      legend.position  = "none",
      plot.margin      = margin(14, 18, 10, 16)
    )
}

pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)

# ==============================================================================
# COMPUTE (same logic as the demo script; recomputed live)
# ==============================================================================
pbp <- load_pbp(SEASON)

plays <- pbp %>%
  filter(
    season_type == "REG",
    play_type %in% c("pass", "run"),
    two_point_attempt == 0,
    !is.na(yardline_100)
  ) %>%
  mutate(
    off_td    = as.integer((coalesce(pass_touchdown, 0) + coalesce(rush_touchdown, 0)) > 0),
    red_zone  = as.integer(yardline_100 <= 20),
    pass_td   = coalesce(pass_touchdown, 0),
    explosive = as.integer(coalesce(yards_gained, 0) >= 20)
  )

N <- nrow(plays)

# Complement (4th down go for it) ----------------------------------------------
fourth <- plays %>%
  filter(down == 4,
         coalesce(fourth_down_converted, 0) == 1 |
         coalesce(fourth_down_failed, 0) == 1)
n_fourth  <- nrow(fourth)
p_convert <- sum(coalesce(fourth$fourth_down_converted, 0) == 1) / n_fourth
p_fail    <- 1 - p_convert

# Addition (pass TD or explosive) ----------------------------------------------
p_A  <- sum(plays$pass_td == 1) / N
p_B  <- sum(plays$explosive == 1) / N
p_AB <- sum(plays$pass_td == 1 & plays$explosive == 1) / N
p_or_correct <- p_A + p_B - p_AB
p_or_naive   <- p_A + p_B

# Multiplication (red zone and offensive TD) -----------------------------------
n_rz      <- sum(plays$red_zone == 1)
pA_rz     <- n_rz / N
pB_td     <- sum(plays$off_td == 1) / N
pAB_joint <- sum(plays$red_zone == 1 & plays$off_td == 1) / N
pAB_naive <- pA_rz * pB_td
p_td_rz   <- sum(plays$red_zone == 1 & plays$off_td == 1) / n_rz
p_td_out  <- sum(plays$red_zone == 0 & plays$off_td == 1) / sum(plays$red_zone == 0)
lift      <- pAB_joint / pAB_naive

# ==============================================================================
# PANEL 1: COMPLEMENT  (the two outcomes tile the whole, sum to 1)
# ==============================================================================
df_c <- data.frame(
  bar     = "4th-down\ngo-for-it",
  outcome = factor(c("Converted", "Failed"), levels = c("Failed", "Converted")),
  share   = c(p_convert, p_fail)
)

p1 <- ggplot(df_c, aes(x = bar, y = share, fill = outcome)) +
  geom_col(width = 0.45) +
  geom_text(aes(label = paste0(outcome, "  ", pct(share)), color = outcome),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 3.6) +
  scale_fill_manual(values = c("Converted" = COL_ACTUAL, "Failed" = COL_NEUTRAL)) +
  scale_color_manual(values = c("Converted" = "white", "Failed" = COL_TEXT)) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  coord_flip() +
  labs(
    title    = "Complement rule: the outcomes tile the whole",
    subtitle = sprintf("P(fail) is just 1 minus P(convert). Two outcomes, must sum to 100%%. (%s attempts)",
                       format(n_fourth, big.mark = ",")),
    x = NULL, y = "Share of 4th-down go-for-it attempts"
  ) +
  theme_stats_series()

# ==============================================================================
# PANEL 2: ADDITION  (the overlap gets counted twice)
# ==============================================================================
# One-decimal, self-consistent shares so the figure reconciles under subtraction
# and matches the post prose exactly (2.5 + 5.8 = 8.3, minus 0.8 overlap = 7.5).
pAB1     <- round(p_AB * 100, 1) / 100          # 0.008  (0.8%)
naive1   <- (round(p_A * 100, 1) + round(p_B * 100, 1)) / 100  # 0.083 (8.3%)
correct1 <- naive1 - pAB1                        # 0.075  (7.5%)

df_b <- data.frame(
  approach  = factor(c("Add P(A) + P(B)", "Add P(A) + P(B)", "Subtract the overlap"),
                     levels = c("Add P(A) + P(B)", "Subtract the overlap")),
  component = factor(c("Correct total", "Overlap counted twice", "Correct total"),
                     levels = c("Overlap counted twice", "Correct total")),
  value     = c(correct1, pAB1, correct1)
)
df_b_tot <- data.frame(
  approach = factor(c("Add P(A) + P(B)", "Subtract the overlap"),
                    levels = c("Add P(A) + P(B)", "Subtract the overlap")),
  total    = c(naive1, correct1)
)

p2 <- ggplot(df_b, aes(x = approach, y = value, fill = component)) +
  geom_col(width = 0.5) +
  geom_text(data = df_b_tot, aes(x = approach, y = total, label = pct(total, 1)),
            inherit.aes = FALSE, vjust = -0.6, color = COL_TEXT,
            fontface = "bold", size = 3.6) +
  annotate("text", x = 1, y = correct1 + pAB1 / 2,
           label = paste0("+", pct(pAB1, 1), "\ncounted twice"),
           color = "white", fontface = "bold", size = 3.0) +
  scale_fill_manual(values = c("Correct total" = COL_ACTUAL,
                               "Overlap counted twice" = COL_NAIVE)) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Addition rule: the overlap gets counted twice",
    subtitle = "P(pass TD or 20+ yard play). Adding the two alone double-counts the plays that were both.",
    x = NULL, y = "Share of all plays"
  ) +
  theme_stats_series()

# ==============================================================================
# PANEL 3: MULTIPLICATION  (independence underpredicts; the rule's assumption breaks)
# ==============================================================================
df_a <- data.frame(
  scenario = factor(c("If independent\nP(A) x P(B)", "Actual joint\nP(A and B)"),
                    levels = c("If independent\nP(A) x P(B)", "Actual joint\nP(A and B)")),
  value    = c(pAB_naive, pAB_joint),
  kind     = c("naive", "actual")
)

p3 <- ggplot(df_a, aes(x = scenario, y = value, fill = kind)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = pct(value, 2)), vjust = -0.6, color = COL_TEXT,
            fontface = "bold", size = 3.6) +
  annotate("text", x = 1.5, y = pAB_joint * 0.92,
           label = sprintf("%.1fx higher\nthan independence predicts", lift),
           color = COL_NAIVE, fontface = "bold", size = 3.2) +
  scale_fill_manual(values = c("naive" = COL_NAIVE, "actual" = COL_ACTUAL)) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%"),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Multiplication rule: independence is the assumption that breaks",
    subtitle = sprintf("P(red zone and offensive TD). TD rate was %s inside the 20 and %s outside it.",
                       pct(p_td_rz), pct(p_td_out)),
    x = NULL, y = "Share of all plays"
  ) +
  theme_stats_series()

# ==============================================================================
# COMBINE AND SAVE
# ==============================================================================
have_patchwork <- requireNamespace("patchwork", quietly = TRUE)

if (have_patchwork) {
  library(patchwork)
  combined <- (p1 / p2 / p3) +
    plot_annotation(
      title    = "Three probability rules on one season of real plays",
      subtitle = sprintf("2025 NFL regular season | %s pass and run plays (no two-point tries)",
                         format(N, big.mark = ",")),
      caption  = "Source: nflfastR | Stats Series Post 8: Basic Probability Rules",
      theme = theme(
        plot.background = element_rect(fill = COL_BG, color = NA),
        plot.title      = element_text(face = "bold", size = 16, color = COL_TEXT),
        plot.subtitle   = element_text(size = 11, color = COL_SUBTEXT,
                                       margin = margin(b = 6)),
        plot.caption    = element_text(size = 9, color = COL_SUBTEXT)
      )
    )
  out_file <- file.path(OUTPUT_DIR,
                        paste0("stats_post", POST_NUM, "_three_rules.png"))
  ggsave(out_file, combined, width = WIDTH_IN, height = HEIGHT_IN,
         dpi = DPI, bg = COL_BG)
  cat(sprintf("Saved: %s\n", out_file))
} else {
  message("patchwork not installed. Saving three separate PNGs instead.")
  message("Install with: install.packages('patchwork') for the combined figure.")
  for (obj in list(list(p = p1, s = "complement"),
                   list(p = p2, s = "addition"),
                   list(p = p3, s = "multiplication"))) {
    f <- file.path(OUTPUT_DIR,
                   paste0("stats_post", POST_NUM, "_", obj$s, ".png"))
    ggsave(f, obj$p, width = WIDTH_IN, height = HEIGHT_IN / 3 + 1,
           dpi = DPI, bg = COL_BG)
    cat(sprintf("Saved: %s\n", f))
  }
}

# CONSOLE SUMMARY --------------------------------------------------------------
cat("\n=== POST 8 VISUAL BUILD COMPLETE ===\n")
cat(sprintf("Post:    %d | %s\n", POST_NUM, CONCEPT))
cat(sprintf("Season:  %d regular season | Universe: %s plays\n",
            SEASON, format(N, big.mark = ",")))
cat(sprintf("Complement: P(convert) %s, P(fail) %s\n", pct(p_convert), pct(p_fail)))
cat(sprintf("Addition:   naive %s, correct %s, overlap %s (one-decimal, reconciles)\n",
            pct(naive1, 1), pct(correct1, 1), pct(pAB1, 1)))
cat(sprintf("Multiply:   actual %s, naive %s, lift %.2fx\n",
            pct(pAB_joint, 2), pct(pAB_naive, 2), lift))
cat(sprintf("DPI: %d | %g x %g in\n", DPI, WIDTH_IN, HEIGHT_IN))
