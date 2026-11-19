# ==============================================================================
# Statistics Series: Post 40 data pull
# Spearman and Kendall correlation (Concept 40, tag [GAP], hands-on demo)
#
# Demo question: does a player's first-half fantasy ranking hold in the second
# half of the season? Same players, two rankings, compared three ways:
#   Pearson r     (straight-line fit on the raw points per game)
#   Spearman rho  (Pearson computed on the ranks)
#   Kendall tau-b (concordant pairs minus discordant pairs, tie-adjusted)
#
# Rules fixed BEFORE any result is seen (do not change after the run):
#   Season        2025 regular season (season_type == "REG"), weeks 1 to 18
#   Halves        weeks 1 to 9 and weeks 10 to 18
#   Positions     QB, RB, WR, TE, each ranked within its own position
#   Metric        mean PPR fantasy points per game in each half, using
#                 nflreadr's fantasy_points_ppr column (standard full PPR as
#                 nflverse publishes it, NOT the toolkit's scoring rules)
#   "Game"        a week in which the player has a row in the weekly stats
#   Qualifier     at least 5 games in EACH half (main); 4 and 6 run as
#                 sensitivity checks only
#   Robustness    leave-one-out: drop each player once, record the largest
#                 change in each coefficient. Applied to every player, so no
#                 outlier is hand-picked.
#
# Gates (script stops before writing anything if any fails):
#   G1 structure: required columns, REG weeks are exactly 1 to 18, one row
#      per player-week, numeric scoring column
#   G2 Spearman identity: cor(method = "spearman") equals Pearson on the
#      average ranks, to 1e-10
#   G3 Kendall identity: hand-counted tau-b from concordant / discordant /
#      tied pairs equals cor(method = "kendall"), to 1e-10, and the pair
#      bookkeeping sums to n(n-1)/2
#
# Writes (only after all gates pass), to stats_posts/output/data/:
#   stats_post40_half_pairs.csv        one row per qualifying player (main rule)
#   stats_post40_position_summary.csv  coefficients and pair counts by position
#   stats_post40_sensitivity.csv       same, at 4 / 5 / 6 game qualifiers
#   stats_post40_loo.csv               leave-one-out maximum change by position
#
# HOW TO RUN: source("stats_posts/scripts/pull_stats_post40_rank_correlation_data.R") from any
# directory. Paste the full console output back into the chat.
# Assumed versions: R 4.3+, dplyr 1.1+, nflreadr 1.4+ (printed at the end).
# ==============================================================================

# ---- CONFIGURATION -----------------------------------------------------------
TOOLKIT_ROOT <- here::here()
if (!dir.exists(TOOLKIT_ROOT)) stop("TOOLKIT_ROOT not found: ", TOOLKIT_ROOT)
OUT_DIR <- file.path(TOOLKIT_ROOT, "stats_posts", "output", "data")
if (!dir.exists(OUT_DIR)) stop("Output folder not found: ", OUT_DIR)

SEASON          <- 2025L
HALF1_WEEKS     <- 1:9
HALF2_WEEKS     <- 10:18
MIN_GAMES_MAIN  <- 5L
MIN_GAMES_SENS  <- c(4L, 5L, 6L)
POSITIONS       <- c("QB", "RB", "WR", "TE")
TOL             <- 1e-10
MIN_N_WARN      <- 20L

library(dplyr)
library(nflreadr)

# ---- HELPERS -----------------------------------------------------------------

# All three coefficients plus the pair bookkeeping behind Kendall.
rank_stats <- function(x, y) {
  stopifnot(length(x) == length(y))
  n <- length(x)
  if (n < 3L) stop("Fewer than 3 players; coefficients undefined")
  if (sd(x) == 0 || sd(y) == 0) stop("Zero spread in a half; coefficients undefined")

  rx <- rank(x, ties.method = "average")
  ry <- rank(y, ties.method = "average")

  sx <- sign(outer(x, x, "-"))
  sy <- sign(outer(y, y, "-"))
  ut <- upper.tri(sx)
  px <- sx[ut]
  py <- sy[ut]
  prod_xy <- px * py

  n_pairs    <- n * (n - 1) / 2
  concordant <- sum(prod_xy > 0)
  discordant <- sum(prod_xy < 0)
  tied_x     <- sum(px == 0)
  tied_y     <- sum(py == 0)
  tied_both  <- sum(px == 0 & py == 0)

  denom <- sqrt((n_pairs - tied_x) * (n_pairs - tied_y))
  tau_b_hand <- if (denom > 0) (concordant - discordant) / denom else NA_real_
  untied <- concordant + discordant

  data.frame(
    n                = n,
    n_pairs          = n_pairs,
    concordant       = concordant,
    discordant       = discordant,
    tied_x           = tied_x,
    tied_y           = tied_y,
    tied_both        = tied_both,
    pairs_check      = concordant + discordant + tied_x + tied_y - tied_both,
    pearson          = cor(x, y),
    spearman         = cor(x, y, method = "spearman"),
    spearman_hand    = cor(rx, ry),
    kendall          = cor(x, y, method = "kendall"),
    kendall_hand     = tau_b_hand,
    tau_a            = (concordant - discordant) / n_pairs,
    share_same_order = if (untied > 0) concordant / untied else NA_real_
  )
}

# Largest change in a coefficient when any single player is removed.
loo_max <- function(x, y, method) {
  full <- cor(x, y, method = method)
  ch <- vapply(seq_along(x),
               function(i) cor(x[-i], y[-i], method = method) - full,
               numeric(1))
  i <- which.max(abs(ch))
  data.frame(method = method, full = full, max_abs_change = abs(ch[i]),
             signed_change = ch[i], row = i)
}

skew_hand <- function(v) {
  s <- sd(v)
  if (s == 0) return(NA_real_)
  n <- length(v)
  (sum((v - mean(v))^3) / n) / ((sum((v - mean(v))^2) / n)^1.5)
}

# ---- LOAD --------------------------------------------------------------------
raw <- tryCatch(
  nflreadr::load_player_stats(seasons = SEASON, summary_level = "week"),
  error = function(e) {
    message("summary_level call failed (", conditionMessage(e),
            "); retrying without it")
    nflreadr::load_player_stats(seasons = SEASON)
  }
)
raw <- dplyr::as_tibble(raw)

cat("==== SECTION 1: STRUCTURE ====\n")
cat("Columns returned (", ncol(raw), "):\n", sep = "")
print(names(raw))

name_col <- intersect(c("player_display_name", "player_name"), names(raw))[1]
team_col <- intersect(c("team", "recent_team"), names(raw))[1]
need <- c("player_id", "position", "season", "week", "season_type",
          "fantasy_points_ppr")
miss <- setdiff(need, names(raw))
if (length(miss) > 0) stop("G1 FAIL: missing columns: ", paste(miss, collapse = ", "))
if (is.na(name_col)) stop("G1 FAIL: no player name column")
if (is.na(team_col)) stop("G1 FAIL: no team column")
cat("Using name column:", name_col, "| team column:", team_col, "\n")
stopifnot(is.numeric(raw$fantasy_points_ppr), is.character(raw$position))

cat("\nRows loaded:", nrow(raw), "\nseason_type counts:\n")
print(table(raw$season_type, useNA = "ifany"))

reg <- raw %>% filter(season == SEASON, season_type == "REG")
cat("\nREG rows:", nrow(reg), "\n")
reg_weeks <- sort(unique(reg$week))
cat("REG weeks present:", paste(reg_weeks, collapse = " "), "\n")
if (!setequal(reg_weeks, c(HALF1_WEEKS, HALF2_WEEKS))) {
  stop("G1 FAIL: REG weeks are not exactly ", min(HALF1_WEEKS), " to ", max(HALF2_WEEKS))
}

dups <- reg %>% count(player_id, week) %>% filter(n > 1)
cat("Duplicate player-week rows:", nrow(dups), "\n")
if (nrow(dups) > 0) stop("G1 FAIL: more than one row per player-week")

n_na <- sum(is.na(reg$fantasy_points_ppr))
cat("NA fantasy_points_ppr rows (dropped):", n_na, "\n")
cat("Negative fantasy_points_ppr rows (kept, valid):",
    sum(reg$fantasy_points_ppr < 0, na.rm = TRUE), "\n")

cat("\nPlayer-weeks by position (all positions, before filter):\n")
print(sort(table(reg$position), decreasing = TRUE))

multi_pos <- reg %>%
  filter(position %in% POSITIONS) %>%
  group_by(player_id) %>%
  summarise(n_pos = n_distinct(position), .groups = "drop") %>%
  filter(n_pos > 1)
cat("Players listed at more than one of QB/RB/WR/TE:", nrow(multi_pos), "\n")

reg <- reg %>%
  filter(position %in% POSITIONS, !is.na(fantasy_points_ppr)) %>%
  arrange(player_id, week)
cat("Rows kept for QB/RB/WR/TE:", nrow(reg), "\n")
cat("G1 PASS\n")

# ---- BUILD HALVES ------------------------------------------------------------
build_halves <- function(df, min_games) {
  per <- df %>%
    mutate(half = if_else(week %in% HALF1_WEEKS, "h1", "h2")) %>%
    group_by(player_id, half) %>%
    summarise(
      games       = n(),
      ppg         = mean(fantasy_points_ppr),
      position    = last(position),
      player_name = last(.data[[name_col]]),
      team        = last(.data[[team_col]]),
      .groups     = "drop"
    )
  h1 <- per %>% filter(half == "h1") %>%
    select(player_id, position_h1 = position, player_name,
           games_h1 = games, ppg_h1 = ppg)
  h2 <- per %>% filter(half == "h2") %>%
    select(player_id, position_h2 = position, team,
           games_h2 = games, ppg_h2 = ppg)
  inner_join(h1, h2, by = "player_id", relationship = "one-to-one") %>%
    filter(games_h1 >= min_games, games_h2 >= min_games) %>%
    mutate(position = position_h2) %>%
    select(-position_h1, -position_h2) %>%
    group_by(position) %>%
    mutate(rank_h1 = rank(-ppg_h1, ties.method = "average"),
           rank_h2 = rank(-ppg_h2, ties.method = "average")) %>%
    ungroup()
}

stats_by_position <- function(pairs) {
  out <- list()
  for (pos in POSITIONS) {
    d <- pairs[pairs$position == pos, ]
    if (nrow(d) < MIN_N_WARN) {
      warning(pos, ": only ", nrow(d), " qualifying players")
    }
    s <- rank_stats(d$ppg_h1, d$ppg_h2)
    s$position <- pos
    out[[pos]] <- s
  }
  bind_rows(out) %>% relocate(position)
}

# ---- SECTION 2 AND 3: MAIN RULE, IDENTITY GATES -----------------------------
cat("\n==== SECTION 2: MAIN RULE (", MIN_GAMES_MAIN,
    "+ games in each half) ====\n", sep = "")
pairs_main <- build_halves(reg, MIN_GAMES_MAIN)
cat("Qualifying players by position:\n")
print(table(pairs_main$position))

summ_main <- stats_by_position(pairs_main)

g2 <- max(abs(summ_main$spearman - summ_main$spearman_hand))
g3 <- max(abs(summ_main$kendall - summ_main$kendall_hand))
g3_pairs <- all(summ_main$pairs_check == summ_main$n_pairs)
cat("\nG2 max |spearman - Pearson on ranks|:", format(g2, digits = 3), "\n")
cat("G3 max |kendall - hand tau-b|:", format(g3, digits = 3), "\n")
cat("G3 pair bookkeeping sums to n(n-1)/2 at every position:", g3_pairs, "\n")
if (!(g2 < TOL)) stop("G2 FAIL: Spearman identity. Nothing written.")
if (!(g3 < TOL) || !g3_pairs) stop("G3 FAIL: Kendall identity. Nothing written.")
cat("G2 PASS, G3 PASS\n")

cat("\n==== SECTION 3: COEFFICIENTS AND PAIR COUNTS (main rule) ====\n")
print(as.data.frame(summ_main %>%
  select(position, n, n_pairs, concordant, discordant, tied_x, tied_y,
         pearson, spearman, kendall, tau_a, share_same_order)),
  digits = 6)
cat("\nPearson minus Spearman, by position:\n")
print(as.data.frame(summ_main %>%
  transmute(position, pearson_minus_spearman = pearson - spearman)), digits = 6)

# ---- SECTION 4: DISTRIBUTION SHAPE OF EACH HALF ------------------------------
cat("\n==== SECTION 4: POINTS PER GAME SHAPE (main rule) ====\n")
shape <- pairs_main %>%
  group_by(position) %>%
  summarise(
    n            = n(),
    h1_mean = mean(ppg_h1), h1_median = median(ppg_h1),
    h1_min = min(ppg_h1), h1_max = max(ppg_h1), h1_skew = skew_hand(ppg_h1),
    h2_mean = mean(ppg_h2), h2_median = median(ppg_h2),
    h2_min = min(ppg_h2), h2_max = max(ppg_h2), h2_skew = skew_hand(ppg_h2),
    .groups = "drop"
  )
print(as.data.frame(shape), digits = 4)

# ---- SECTION 5: LEAVE-ONE-OUT ROBUSTNESS -------------------------------------
cat("\n==== SECTION 5: LEAVE-ONE-OUT (every player dropped once) ====\n")
loo_list <- list()
for (pos in POSITIONS) {
  d <- pairs_main[pairs_main$position == pos, ]
  for (m in c("pearson", "spearman", "kendall")) {
    l <- loo_max(d$ppg_h1, d$ppg_h2, m)
    l$position    <- pos
    l$player_name <- d$player_name[l$row]
    l$ppg_h1      <- d$ppg_h1[l$row]
    l$ppg_h2      <- d$ppg_h2[l$row]
    l$rank_h1     <- d$rank_h1[l$row]
    l$rank_h2     <- d$rank_h2[l$row]
    loo_list[[length(loo_list) + 1L]] <- l
  }
}
loo <- bind_rows(loo_list) %>% select(-row) %>% relocate(position, method)
print(as.data.frame(loo), digits = 5)

# ---- SECTION 6: SENSITIVITY TO THE GAMES QUALIFIER ---------------------------
cat("\n==== SECTION 6: SENSITIVITY (4 / 5 / 6 games per half) ====\n")
sens_list <- list()
for (g in MIN_GAMES_SENS) {
  s <- stats_by_position(build_halves(reg, g))
  s$min_games <- g
  sens_list[[length(sens_list) + 1L]] <- s
}
sens <- bind_rows(sens_list) %>%
  select(min_games, position, n, pearson, spearman, kendall, share_same_order)
print(as.data.frame(sens), digits = 5)

# ---- SECTION 7: TOP OF EACH FIRST-HALF RANKING (preview) ---------------------
cat("\n==== SECTION 7: TOP 5 BY FIRST-HALF POINTS PER GAME ====\n")
for (pos in POSITIONS) {
  cat("\n", pos, "\n", sep = "")
  print(as.data.frame(pairs_main %>%
    filter(position == pos) %>%
    arrange(rank_h1) %>%
    slice_head(n = 5) %>%
    select(player_name, team, games_h1, ppg_h1, rank_h1,
           games_h2, ppg_h2, rank_h2)),
    digits = 4, row.names = FALSE)
}

# ---- WRITE -------------------------------------------------------------------
write.csv(pairs_main %>% arrange(position, rank_h1),
          file.path(OUT_DIR, "stats_post40_half_pairs.csv"), row.names = FALSE)
write.csv(summ_main, file.path(OUT_DIR, "stats_post40_position_summary.csv"),
          row.names = FALSE)
write.csv(sens, file.path(OUT_DIR, "stats_post40_sensitivity.csv"), row.names = FALSE)
write.csv(loo, file.path(OUT_DIR, "stats_post40_loo.csv"), row.names = FALSE)

cat("\n==== WRITTEN to", OUT_DIR, "====\n")
cat("stats_post40_half_pairs.csv        rows:", nrow(pairs_main), "\n")
cat("stats_post40_position_summary.csv  rows:", nrow(summ_main), "\n")
cat("stats_post40_sensitivity.csv       rows:", nrow(sens), "\n")
cat("stats_post40_loo.csv               rows:", nrow(loo), "\n")

cat("\n==== VERSIONS ====\n")
cat(R.version.string, "| dplyr", as.character(packageVersion("dplyr")),
    "| nflreadr", as.character(packageVersion("nflreadr")), "\n")
cat("Season:", SEASON, "| halves: weeks", min(HALF1_WEEKS), "to", max(HALF1_WEEKS),
    "and", min(HALF2_WEEKS), "to", max(HALF2_WEEKS), "\n")
