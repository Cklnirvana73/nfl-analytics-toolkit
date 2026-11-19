# ==============================================================================
# Statistics Series: Post 15 data pull
# Log-Normal and Heavy-Tailed Distributions (Concept 15)
#
# PURPOSE
# -------
# Show what the normal assumption costs when weekly fantasy scoring is
# right-skewed. Mirrors the exact spread-recovery step from the start/sit
# uncertainty layer (R/36):
#
#     sigma = (upper_80 - lower_80) / (2 * qnorm(0.9))
#
# That recovery is exact for a symmetric normal. This script recovers a sigma
# the same way from the EMPIRICAL 10th/90th percentiles of real 2025 weekly PPR,
# rebuilds the symmetric-normal 80% band, and measures how far it misses the
# true ceiling (P90) and floor (P10) once the distribution is right-skewed.
#
# It computes, per group (pooled, by position, by volatility tier):
#   - mean, median, mean minus median (a plain skew signal)
#   - sd, skewness (g1)
#   - empirical P10 / P90 / P95 / P99
#   - sigma_sym   : sigma recovered exactly as R/36 does
#   - sym_ceiling : normal(mean, sigma_sym) 90th percentile
#   - ceiling_gap : empirical P90 minus sym_ceiling  (>0 = model ceiling too low)
#   - sym_floor   : normal(mean, sigma_sym) 10th percentile
#   - floor_gap   : empirical P10 minus sym_floor    (>0 = model floor too low)
#   - p95_gap / p99_gap : empirical minus normal(mean, sd) high percentiles
#                         (>0 = real upper tail heavier than normal)
#
# NOTE ON FAIRNESS: R/36 centers each player's normal on adj_proj (the
# projection mean). Here we substitute the empirical mean for adj_proj so the
# comparison isolates distribution SHAPE, not forecast accuracy.
#
# HONESTY NOTE: weekly PPR has a floor near zero and occasional slight
# negatives, so it is not a textbook log-normal (which needs support above
# zero). This script demonstrates right-skew and heavy tails and treats
# log-normal as the reference lens, not a fitted claim.
#
# HOW TO RUN
# ----------
#   source("stats_posts/scripts/pull_stats_post15_lognormal_heavy_tailed_demo_data.R")
# Requires internet (nflreadr downloads the 2025 weekly stats). Prints only;
# writes no files. Paste the console output back for verification.
# ==============================================================================

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------
SEASON    <- 2025L
MIN_GAMES <- 8L
POSITIONS <- c("QB", "RB", "WR", "TE")
Z80       <- stats::qnorm(0.9)        # ~1.2816, exactly the R/36 constant

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------
library(nflreadr)
library(dplyr)
library(tibble)

# ------------------------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------------------------

# Sample skewness (g1): third standardized moment using the sample sd.
sample_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3L) return(NA_real_)
  m <- mean(x)
  s <- stats::sd(x)
  if (is.na(s) || s == 0) return(NA_real_)
  (sum((x - m)^3) / n) / (s^3)
}

# Exactly the R/36 recovery: an 80% interval reduced to one symmetric sigma.
recover_sigma <- function(lower_80, upper_80) {
  (upper_80 - lower_80) / (2 * Z80)
}

# One summary row for a vector of weekly PPR scores.
summarize_group <- function(x, label) {
  x <- x[is.finite(x)]
  n <- length(x)
  q <- stats::quantile(x, probs = c(0.10, 0.50, 0.90, 0.95, 0.99),
                       names = FALSE, type = 7)
  mu <- mean(x)
  md <- q[2]
  s  <- stats::sd(x)

  sigma_sym   <- recover_sigma(q[1], q[3])        # from empirical P10, P90
  sym_ceiling <- mu + Z80 * sigma_sym             # normal(mu, sigma_sym) P90
  sym_floor   <- mu - Z80 * sigma_sym             # normal(mu, sigma_sym) P10
  norm_p95    <- mu + stats::qnorm(0.95) * s      # normal(mu, s) high pcts
  norm_p99    <- mu + stats::qnorm(0.99) * s

  tibble::tibble(
    group       = label,
    n_weeks     = n,
    mean        = mu,
    median      = md,
    mean_md_gap = mu - md,
    sd          = s,
    skew        = sample_skewness(x),
    p10         = q[1],
    p90         = q[3],
    p95         = q[4],
    p99         = q[5],
    sigma_sym   = sigma_sym,
    sym_ceiling = sym_ceiling,
    ceiling_gap = q[3] - sym_ceiling,             # >0 = symmetric ceiling too low
    sym_floor   = sym_floor,
    floor_gap   = q[1] - sym_floor,               # >0 = symmetric floor too low
    p95_gap     = q[4] - norm_p95,                # >0 = heavier upper tail
    p99_gap     = q[5] - norm_p99
  )
}

# Round numeric columns of a data frame for readable printing.
round_df <- function(df, digits = 2) {
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], round, digits)
  df
}

# ------------------------------------------------------------------------------
# LOAD (nflreadr signature has shifted across versions; try the common forms)
# ------------------------------------------------------------------------------
load_weekly <- function(season) {
  out <- tryCatch(
    nflreadr::load_player_stats(seasons = season, stat_type = "offense"),
    error = function(e) NULL
  )
  if (is.null(out)) {
    out <- tryCatch(nflreadr::load_player_stats(seasons = season),
                    error = function(e) NULL)
  }
  if (is.null(out)) {
    stop("Could not load weekly player stats. Check nflreadr version/install.")
  }
  out
}

raw <- load_weekly(SEASON)
nm  <- names(raw)

# Detect the columns we need, tolerant of naming drift.
id_col   <- if ("player_id" %in% nm) "player_id" else
            if ("gsis_id" %in% nm) "gsis_id" else NA_character_
name_col <- if ("player_display_name" %in% nm) "player_display_name" else
            if ("player_name" %in% nm) "player_name" else NA_character_
pos_col  <- if ("position" %in% nm) "position" else
            if ("position_group" %in% nm) "position_group" else NA_character_
ppr_col  <- if ("fantasy_points_ppr" %in% nm) "fantasy_points_ppr" else NA_character_

if (any(is.na(c(id_col, name_col, pos_col, ppr_col)))) {
  stop("Missing an expected column. Available columns:\n",
       paste(nm, collapse = ", "))
}

work <- data.frame(
  player_id   = raw[[id_col]],
  player_name = raw[[name_col]],
  position    = raw[[pos_col]],
  week        = raw[["week"]],
  ppr         = raw[[ppr_col]],
  season_type = if ("season_type" %in% nm) raw[["season_type"]] else NA_character_,
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# FILTER: regular season, skill positions, finite PPR, 8+ games
# ------------------------------------------------------------------------------
work <- work[is.finite(work$ppr), ]

if (any(!is.na(work$season_type))) {
  work <- work[work$season_type == "REG", ]
} else {
  work <- work[work$week <= 18L, ]   # 2025 REG is weeks 1-18; POST is 19+
}

work <- work[work$position %in% POSITIONS, ]

games <- aggregate(week ~ player_id, data = work,
                   FUN = function(w) length(unique(w)))
names(games)[2] <- "games"
keep_ids <- games$player_id[games$games >= MIN_GAMES]
weekly   <- work[work$player_id %in% keep_ids, ]

# ------------------------------------------------------------------------------
# PER-PLAYER (for volatility tiers and illustrative extremes)
# ------------------------------------------------------------------------------
per_player <- weekly %>%
  dplyr::group_by(player_id, player_name, position) %>%
  dplyr::summarise(
    games    = dplyr::n(),
    mean_ppr = mean(ppr),
    sd_ppr   = stats::sd(ppr),
    skew_ppr = sample_skewness(ppr),
    .groups  = "drop"
  )

med_sd   <- stats::median(per_player$sd_ppr, na.rm = TRUE)
low_ids  <- per_player$player_id[per_player$sd_ppr <  med_sd]
high_ids <- per_player$player_id[per_player$sd_ppr >= med_sd]

# ------------------------------------------------------------------------------
# SUMMARIES
# ------------------------------------------------------------------------------
pooled <- summarize_group(weekly$ppr, "POOLED (all skill)")

by_pos <- dplyr::bind_rows(lapply(POSITIONS, function(p) {
  summarize_group(weekly$ppr[weekly$position == p], p)
}))

by_vol <- dplyr::bind_rows(
  summarize_group(weekly$ppr[weekly$player_id %in% low_ids],  "LOW volatility"),
  summarize_group(weekly$ppr[weekly$player_id %in% high_ids], "HIGH volatility")
)

top_skew <- per_player %>%
  dplyr::arrange(dplyr::desc(skew_ppr)) %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::mutate(across(c(mean_ppr, sd_ppr, skew_ppr), ~ round(.x, 2)))

low_skew <- per_player %>%
  dplyr::arrange(skew_ppr) %>%
  dplyr::slice_head(n = 5) %>%
  dplyr::mutate(across(c(mean_ppr, sd_ppr, skew_ppr), ~ round(.x, 2)))

# ------------------------------------------------------------------------------
# PRINT
# ------------------------------------------------------------------------------
line <- function() cat(strrep("=", 78), "\n")

cat("\n"); line()
cat(sprintf("POST 15 PULL  |  %d regular season  |  weekly PPR  |  %d+ games\n",
            SEASON, MIN_GAMES))
line()
cat(sprintf("Players kept:      %d\n", length(keep_ids)))
cat(sprintf("Player-weeks kept: %d\n", nrow(weekly)))
cat(sprintf("Z80 constant:      %.5f  (qnorm(0.9), the R/36 divisor half)\n", Z80))
cat("Positive ceiling_gap = symmetric normal understates the true ceiling.\n")
cat("Positive floor_gap   = symmetric normal invents a floor below reality.\n")

cat("\n"); line(); cat("POOLED\n"); line()
print(round_df(as.data.frame(pooled)))

cat("\n"); line(); cat("BY POSITION\n"); line()
print(round_df(as.data.frame(by_pos)))

cat("\n"); line(); cat("BY VOLATILITY TIER (per-player sd split at the median)\n"); line()
print(round_df(as.data.frame(by_vol)))

cat("\n"); line()
cat("ILLUSTRATIVE PLAYERS  (per-player skew over ~17 games is NOISY;\n")
cat("use as illustration only, never as a ranking)\n"); line()
cat("Most right-skewed:\n")
print(as.data.frame(top_skew))
cat("\nLeast skewed (near-symmetric):\n")
print(as.data.frame(low_skew))

cat("\n"); line(); cat("PLAIN-LANGUAGE READOUT (pooled)\n"); line()
cat(sprintf("Mean %.2f sits %.2f above the median %.2f (right lean).\n",
            pooled$mean, pooled$mean_md_gap, pooled$median))
cat(sprintf("Skewness (g1): %.3f\n", pooled$skew))
cat(sprintf("R/36-recovered sigma_sym: %.2f\n", pooled$sigma_sym))
cat(sprintf("True ceiling P90 %.2f vs symmetric ceiling %.2f  -> understated by %.2f pts\n",
            pooled$p90, pooled$sym_ceiling, pooled$ceiling_gap))
cat(sprintf("True floor P10 %.2f vs symmetric floor %.2f  -> floor set %.2f pts too low\n",
            pooled$p10, pooled$sym_floor, pooled$floor_gap))
cat(sprintf("Upper-tail heaviness: P95 gap %.2f, P99 gap %.2f (empirical minus normal)\n",
            pooled$p95_gap, pooled$p99_gap))
line(); cat("\n")
