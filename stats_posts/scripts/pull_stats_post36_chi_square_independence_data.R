# pull_stats_post36_chi_square_independence_data.R
# Statistics Series, Post 36 (Concept 36, chi-square test of independence). Verification pull.
#
# Units   : the Post 35 players (2025, QB/RB/WR, 8+ forecast games, 197 of the
#           202 Post 32 players), ONE randomly drawn eligible week per player,
#           the exact Post 35 headline draw (seed 20261006). One player per row,
#           so the counts in the table are independent.
# Question: is where a player's tested game landed against his season-to-date
#           forecast (fantasy_pts_std) related to his position?
# Table   : position (QB / RB / WR) x direction (below forecast / above forecast).
# Test    : chi-square test of independence. Expected count per cell =
#           row total x column total / grand total. df = (3 - 1)(2 - 1) = 2.
#           No continuity correction (it applies only to 2 x 2 tables; the
#           argument is set explicitly anyway).
#
# Rules fixed BEFORE any numbers exist (2026-10-12 session):
#   - Headline: the Post 35 headline draw, alpha 0.05, not changed after
#     seeing results.
#   - "Below" = actual < forecast; "above" = actual > forecast. An exact tie is
#     counted and printed, then left out of the table.
#   - Robustness only: the same test on each player's earliest and latest
#     eligible week, and on the 200 further Post 35 draws (seeds 20261007 to
#     20261206). Fisher's exact test on the headline table as a cross-check.
#   - Section 8 pools every eligible player-week by position. Those rows are
#     NOT independent (one player contributes many). Shares only, no p-value.
#
# Gates: section 2 reproduces Post 32's verified one-way numbers; section 5
# reproduces Post 35's verified headline (counts, statistic, eligible rows,
# share below). The script stops if either fails, because then the draw is not
# the Post 35 draw. Sections 1 to 4 copy the Post 32 gate and the Post 35 v3
# rebuild rules.
#
# Read-only. Writes nothing to disk. Run from the toolkit root and paste the full
# console output back verbatim.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
})
options(digits = 7, width = 120)

# ---- CONFIGURATION -----------------------------------------------------------
SEASON          <- 2025L
PANEL_PATH      <- here::here("data", "season2_cache", "s2_week4_ab_panel.rds")
POSITIONS       <- c("QB", "RB", "WR")
DIRECTIONS      <- c("below", "above")
N_ROWS_EXPECTED <- 2922L
MAIN_FLOOR      <- 8L
MIN_PRIOR       <- 4L
K_P35           <- 5L          # Post 35 slice count, used only for the reproduction gate
ALPHA           <- 0.05
SEED            <- 20261006L
N_DRAWS         <- 200L
B_MC            <- 100000L
MEAN_TOL        <- 1e-6
MAX_PLAYERS_LOST <- 20L

# Post 32 verified values (mae_std | floor 8)
P32_N    <- c(QB = 26L, RB = 70L, WR = 106L)
P32_MEAN <- c(QB = 8.272975, RB = 5.645086, WR = 5.157343)
P32_SSB  <- 203.399412
P32_SSW  <- 944.218806
P32_F    <- 21.433847

# Post 35 verified headline values
P35_OBS        <- c(45L, 37L, 42L, 27L, 46L)
P35_STAT       <- 6.121827
P35_N_ELIG     <- 2146L
P35_N_PLAYERS  <- 197L
P35_POS        <- c(QB = 26L, RB = 70L, WR = 101L)
P35_N_BELOW    <- 106L

f6 <- function(x) sprintf("%.6f", x)
banner <- function(s) cat("\n====================", s, "====================\n")

# ---- 1. LOAD PANEL -----------------------------------------------------------
banner("1. PANEL")
if (!file.exists(PANEL_PATH)) stop("Panel not found at: ", PANEL_PATH)
panel_all <- readRDS(PANEL_PATH)
need <- c("season", "week", "player_id", "player_name", "position", "team",
          "fantasy_pts_actual", "fantasy_pts_roll3", "fantasy_pts_std",
          "games_played_prior")
miss <- setdiff(need, names(panel_all))
if (length(miss)) stop("Panel missing columns: ", paste(miss, collapse = ", "))

p25 <- panel_all %>% filter(season == SEASON)
cat("Rows in", SEASON, ":", nrow(p25), "(expected", N_ROWS_EXPECTED, ")\n")
stopifnot(nrow(p25) == N_ROWS_EXPECTED)
stopifnot(setequal(unique(p25$position), POSITIONS))
dup <- p25 %>% count(player_id, week) %>% filter(n > 1)
cat("Duplicate player-week rows:", nrow(dup), "\n")
stopifnot(nrow(dup) == 0)
stopifnot(is.numeric(p25$games_played_prior), !anyNA(p25$games_played_prior))
stopifnot(is.numeric(p25$fantasy_pts_actual), is.numeric(p25$fantasy_pts_std),
          is.numeric(p25$fantasy_pts_roll3))

p25 <- p25 %>%
  mutate(err_std   = abs(fantasy_pts_actual - fantasy_pts_std),
         err_roll3 = abs(fantasy_pts_actual - fantasy_pts_roll3))
stopifnot(!anyNA(p25$err_std), !anyNA(p25$err_roll3))

players <- p25 %>%
  arrange(player_id, week) %>%
  group_by(player_id) %>%
  summarise(player_name = last(player_name),
            n_pos       = n_distinct(position),
            position    = first(position),
            n_weeks     = n(),
            mae_std     = mean(err_std),
            .groups = "drop")
stopifnot(all(players$n_pos == 1))
cat("Players:", nrow(players), "\n")

# ---- 2. REPRODUCE POST 32 (GATE) ---------------------------------------------
banner("2. POST 32 REPRODUCTION GATE")
pl8   <- players %>% filter(n_weeks >= MAIN_FLOOR)
n_by  <- as.integer(table(factor(pl8$position, levels = POSITIONS)))
names(n_by) <- POSITIONS
m_by  <- tapply(pl8$mae_std, factor(pl8$position, levels = POSITIONS), mean)
grand <- mean(pl8$mae_std)
ssb   <- sum(n_by * (m_by - grand)^2)
ssw   <- sum((pl8$mae_std - ave(pl8$mae_std, pl8$position))^2)
f_one <- (ssb / (length(POSITIONS) - 1)) / (ssw / (nrow(pl8) - length(POSITIONS)))
cat("n:", paste(names(n_by), n_by, collapse = " / "), "\n")
cat("SSB", f6(ssb), " SSW", f6(ssw), " F", f6(f_one), "\n")
gate32 <- all(n_by == P32_N) &&
  max(abs(m_by - P32_MEAN)) < 1e-5 &&
  abs(ssb - P32_SSB) < 1e-4 &&
  abs(ssw - P32_SSW) < 1e-4 &&
  abs(f_one - P32_F) < 1e-4
cat("Post 32 reproduced:", gate32, "\n")
if (!gate32) stop("Post 32 numbers not reproduced. Stop and report; do not continue.")

# ---- 3. REBUILD EARLIER GAMES (Post 35 v3 rules) -----------------------------
# games_played_prior == 3 on every first forecast row, so three games precede the
# panel. With a = actual scores on forecast rows 1..3:
#   g2 + g3 = 3 * roll3_2 - a1;  g3 = 3 * roll3_3 - a1 - a2;  g1 = 3 * std_1 - (g2 + g3)
# Checks on rows the algebra does not use:
#   (i)   games_played_prior rises by exactly 1 per forecast row (player-level);
#   (ii)  roll3_1 == std_1, and roll3 == mean of the last three actual scores on
#         every row from 4 on (player-level);
#   (iii) std == mean of all rebuilt earlier games on the row (row-level).
banner("3. REBUILD AND VERIFY EARLIER GAMES")
hist25 <- p25 %>% filter(player_id %in% pl8$player_id) %>% arrange(player_id, week)
cat("Forecast rows for the", nrow(pl8), "players:", nrow(hist25), "\n")

rebuild_one <- function(d) {
  d   <- d[order(d$week), ]
  act <- d$fantasy_pts_actual
  r3  <- d$fantasy_pts_roll3
  st  <- d$fantasy_pts_std
  nr  <- nrow(d)
  ok_step <- nr >= 3 && d$games_played_prior[1] == 3 &&
    all(diff(d$games_played_prior) == 1)
  ok_roll <- ok_step && abs(r3[1] - st[1]) < MEAN_TOL &&
    (nr < 4 || all(vapply(4:nr, function(i) abs(r3[i] - mean(act[(i - 3):(i - 1)])) < MEAN_TOL,
                          logical(1))))
  pre <- NA_real_
  if (ok_step) {
    s23 <- 3 * r3[2] - act[1]
    g3  <- 3 * r3[3] - act[1] - act[2]
    pre <- c(3 * st[1] - s23, s23 - g3, g3)
  }
  out <- vector("list", nr)
  for (i in seq_len(nr)) {
    earlier <- c(pre, act[seq_len(i - 1)])
    ok_len  <- ok_step && ok_roll && !anyNA(earlier) &&
      length(earlier) == d$games_played_prior[i]
    e_mean  <- if (ok_len) mean(earlier) else NA_real_
    e_sd    <- if (ok_len && length(earlier) >= 2) sd(earlier) else NA_real_
    out[[i]] <- data.frame(
      player_id   = d$player_id[i],
      player_name = d$player_name[i],
      position    = d$position[i],
      week        = d$week[i],
      actual      = act[i],
      forecast    = st[i],
      n_prior     = if (ok_len) length(earlier) else NA_integer_,
      prior_mean  = e_mean,
      prior_sd    = e_sd,
      ok_mean     = ok_len && abs(e_mean - st[i]) < MEAN_TOL,
      stringsAsFactors = FALSE
    )
  }
  bind_rows(out)
}
rb <- bind_rows(lapply(split(hist25, hist25$player_id), rebuild_one))
stopifnot(nrow(rb) == nrow(hist25))
cat("Rows passing all rebuild checks:", sum(rb$ok_mean), "of", nrow(rb), "\n")

# ---- 4. ELIGIBLE WEEKS -------------------------------------------------------
banner(paste0("4. ELIGIBLE WEEKS (verified, >= ", MIN_PRIOR, " earlier games, earlier SD > 0)"))
elig <- rb %>%
  filter(ok_mean, n_prior >= MIN_PRIOR, !is.na(prior_sd), prior_sd > 0) %>%
  mutate(z = (actual - prior_mean) / prior_sd)
stopifnot(!anyNA(elig$z), all(is.finite(elig$z)))
per_pl <- elig %>% count(player_id, name = "n_elig")
lost <- setdiff(pl8$player_id, per_pl$player_id)
cat("Eligible player-weeks:", nrow(elig), "\n")
cat("Players with at least one eligible week:", nrow(per_pl), "of", nrow(pl8), "\n")
if (length(lost)) print(pl8 %>% filter(player_id %in% lost) %>% count(position))
if (length(lost) > MAX_PLAYERS_LOST) stop("Too many players lost. Stop and report.")

pick_random <- function(df) {
  df %>% arrange(player_id, week) %>% group_by(player_id) %>%
    slice_sample(n = 1) %>% ungroup()
}

# ---- 5. REPRODUCE POST 35 HEADLINE (GATE) ------------------------------------
banner("5. POST 35 REPRODUCTION GATE")
set.seed(SEED)
main <- pick_random(elig)
stopifnot(nrow(main) == nrow(per_pl), !any(duplicated(main$player_id)))
sl35   <- findInterval(main$z, qnorm(seq_len(K_P35 - 1) / K_P35)) + 1L
obs35  <- tabulate(sl35, nbins = K_P35)
stat35 <- sum((obs35 - nrow(main) / K_P35)^2 / (nrow(main) / K_P35))
pos35  <- as.integer(table(factor(main$position, levels = POSITIONS)))
names(pos35) <- POSITIONS
n_below_z <- sum(main$z < 0)
cat("Slice counts:", paste(obs35, collapse = " / "), "  chi-square", f6(stat35), "\n")
cat("Eligible rows:", nrow(elig), "  players:", nrow(main),
    "  positions:", paste(names(pos35), pos35, collapse = " / "), "\n")
cat("Tested games below forecast (z < 0):", n_below_z, "\n")
gate35 <- all(obs35 == P35_OBS) && abs(stat35 - P35_STAT) < 1e-5 &&
  nrow(elig) == P35_N_ELIG && nrow(main) == P35_N_PLAYERS &&
  all(pos35 == P35_POS) && n_below_z == P35_N_BELOW
cat("Post 35 headline reproduced:", gate35, "\n")
if (!gate35) stop("Post 35 draw not reproduced. Stop and report; do not continue.")

# ---- 6. INDEPENDENCE ENGINE --------------------------------------------------
# direction from actual vs forecast directly; ties reported and dropped
direction_of <- function(actual, forecast) {
  ifelse(actual < forecast, "below", ifelse(actual > forecast, "above", "tie"))
}

indep <- function(df) {
  df  <- df %>% mutate(direction = direction_of(actual, forecast))
  n_tie <- sum(df$direction == "tie")
  df  <- df %>% filter(direction != "tie")
  tab <- table(factor(df$position, levels = POSITIONS),
               factor(df$direction, levels = DIRECTIONS))
  n   <- sum(tab)
  stopifnot(n > 0)
  expd <- outer(rowSums(tab), colSums(tab)) / n
  stopifnot(all(expd > 0))
  piece <- (tab - expd)^2 / expd
  stat  <- sum(piece)
  dfree <- (nrow(tab) - 1L) * (ncol(tab) - 1L)
  list(tab = tab, expd = expd, piece = piece, n = n, n_tie = n_tie,
       stat = stat, df = dfree,
       p = pchisq(stat, dfree, lower.tail = FALSE),
       crit = qchisq(1 - ALPHA, dfree))
}

print_indep <- function(g) {
  cat("Observed (rows = position, columns = direction):\n"); print(g$tab)
  cat("Row totals:", paste(names(rowSums(g$tab)), rowSums(g$tab), collapse = " / "),
      "  Column totals:", paste(names(colSums(g$tab)), colSums(g$tab), collapse = " / "),
      "  Grand total:", g$n, "  Ties dropped:", g$n_tie, "\n")
  cat("Expected (row total x column total / grand total):\n")
  print(round(g$expd, 6))
  cat("Observed minus expected:\n"); print(round(g$tab - g$expd, 6))
  cat("Pieces (O - E)^2 / E:\n"); print(round(g$piece, 6))
  cat("Share below forecast by position:",
      paste(POSITIONS, f6(g$tab[, "below"] / rowSums(g$tab)), collapse = " / "),
      "  overall", f6(sum(g$tab[, "below"]) / g$n), "\n")
  cat("chi-square ", f6(g$stat), "  df ", g$df, "  critical (alpha ", ALPHA, ") ",
      f6(g$crit), "  p ", signif(g$p, 6), "  smallest expected ", f6(min(g$expd)), "\n", sep = "")
}

# ---- 7. MAIN: position x direction on the Post 35 headline draw --------------
banner("7. MAIN: position x direction, Post 35 headline draw")
g_main <- indep(main)
print_indep(g_main)

ct <- chisq.test(g_main$tab, correct = FALSE)
cat("chisq.test() statistic", f6(unname(ct$statistic)), " df", unname(ct$parameter),
    " p", signif(ct$p.value, 6), "\n")
stopifnot(abs(unname(ct$statistic) - g_main$stat) < 1e-8)
stopifnot(max(abs(ct$expected - g_main$expd)) < 1e-8)
set.seed(SEED)
ct_mc <- chisq.test(g_main$tab, simulate.p.value = TRUE, B = B_MC)
cat("Monte Carlo p (B =", B_MC, "):", signif(ct_mc$p.value, 6), "\n")
fe <- fisher.test(g_main$tab)
cat("Fisher's exact test p:", signif(fe$p.value, 6), "\n")

cat("\nOne cell sets the rest? Observed minus expected must sum to 0 across each row",
    "and each column:\n")
cat("  row sums:", paste(f6(rowSums(g_main$tab - g_main$expd)), collapse = " / "),
    "  column sums:", paste(f6(colSums(g_main$tab - g_main$expd)), collapse = " / "), "\n")

# ---- 8. ROBUSTNESS -----------------------------------------------------------
banner("8. ROBUSTNESS")

cat("\n-- 8a. Earliest eligible week per player --\n")
early <- elig %>% arrange(player_id, week) %>% group_by(player_id) %>% slice(1) %>% ungroup()
print_indep(indep(early))

cat("\n-- 8b. Latest eligible week per player --\n")
late <- elig %>% arrange(player_id, week) %>% group_by(player_id) %>% slice(n()) %>% ungroup()
print_indep(indep(late))

cat("\n-- 8c.", N_DRAWS, "further random draws (seeds", SEED + 1L, "to", SEED + N_DRAWS, ") --\n")
draws <- lapply(seq_len(N_DRAWS), function(i) {
  set.seed(SEED + i)
  g <- indep(pick_random(elig))
  c(stat = g$stat, p = g$p, ties = g$n_tie,
    setNames(g$tab[, "below"] / rowSums(g$tab), paste0("below_", POSITIONS)))
})
draws <- as.data.frame(do.call(rbind, draws))
cat("Share of draws with p < ", ALPHA, ": ", f6(mean(draws$p < ALPHA)),
    " (", sum(draws$p < ALPHA), " of ", N_DRAWS, ")\n", sep = "")
cat("p quantiles (0, 0.1, 0.5, 0.9, 1):",
    paste(signif(quantile(draws$p, c(0, .1, .5, .9, 1)), 4), collapse = " / "), "\n")
cat("Statistic quantiles (0, 0.1, 0.5, 0.9, 1):",
    paste(f6(quantile(draws$stat, c(0, .1, .5, .9, 1))), collapse = " / "), "\n")
cat("Median share below forecast across draws:",
    paste(POSITIONS, f6(sapply(paste0("below_", POSITIONS), function(s) median(draws[[s]]))),
          collapse = " / "), "\n")
cat("Total ties across draws:", sum(draws$ties), "\n")

# ---- 9. CONTEXT ONLY: every eligible player-week (NOT independent) -----------
banner("9. CONTEXT ONLY: all eligible player-weeks pooled (not independent, not a test)")
pooled <- elig %>% mutate(direction = direction_of(actual, forecast))
print(table(factor(pooled$position, levels = POSITIONS),
            factor(pooled$direction, levels = c(DIRECTIONS, "tie"))))
pb <- pooled %>% filter(direction != "tie") %>%
  group_by(position) %>% summarise(share_below = mean(direction == "below"), n = n(), .groups = "drop")
print(as.data.frame(pb %>% mutate(share_below = f6(share_below))), row.names = FALSE)
cat("Shares only; no p-value is reported because rows from one player are not independent.\n")

cat("\nDone. Copy the full console output back verbatim.\n")
