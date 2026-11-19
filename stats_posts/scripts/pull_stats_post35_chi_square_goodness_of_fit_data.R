# pull_stats_post35_chi_square_goodness_of_fit_data.R
# Statistics Series, Post 35 (Concept 35, chi-square goodness of fit). Verification pull.
#
# Units   : the Post 32 players (2025, QB/RB/WR, 8+ forecast games), ONE tested
#           week per player, so the counts are independent across players.
# Claim   : the season-to-date forecast's own range. For a tested week, a band is
#           built from the player's earlier 2025 games: centred on their average
#           (fantasy_pts_std, the toolkit's season-to-date forecast) with their
#           standard deviation as the width, assuming a normal shape. The band is
#           cut into K slices of equal probability. If the band describes the
#           player's games, each slice should catch 1/K of tested weeks.
# Test    : chi-square goodness of fit, observed slice counts vs n/K each.
#           df = K - 1. The band uses only EARLIER games, never the tested game,
#           so no parameter is fitted to the tested data and no df is deducted.
#
# Rules fixed BEFORE any numbers exist (2026-10-06 session):
#   - Headline: K = 5 slices, one randomly drawn eligible week per player
#     (seed 20261006), alpha 0.05. Not changed after seeing results.
#   - Eligible week: the reconstructed earlier games pass verification (section 3),
#     at least MIN_PRIOR earlier games, and a nonzero earlier standard deviation.
#   - Robustness only: K = 10; earliest and latest eligible week instead of a
#     random one; 200 further random draws; a rank version with no normal
#     assumption (where the game ranks among the player's own earlier games).
#   - Section 7 pools every eligible player-week. Those rows are NOT independent
#     (one player contributes many). It is printed for context only, never as a
#     test result.
#
# Earlier games are rebuilt from the panel itself (v2, after run 1 showed every
# player's first forecast row has 3 earlier games). The 3 pre-panel games come
# from the 3-game rolling average on forecast rows 2 and 3 plus the season-to-date
# average on row 1. The definitions this relies on are verified on rows the
# algebra does not use; rows or players that fail are excluded and counted.
#
# Gate: section 2 reproduces Post 32's verified one-way numbers and stops if they
# do not match. The load and aggregation blocks are copied from the Post 34 pull
# script, which passed this gate.
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
N_ROWS_EXPECTED <- 2922L
MAIN_FLOOR      <- 8L
MIN_PRIOR       <- 4L          # earlier games needed before a week can be tested
K_MAIN          <- 5L
K_ALT           <- 10L
ALPHA           <- 0.05
SEED            <- 20261006L
N_DRAWS         <- 200L        # further random week draws (robustness)
B_MC            <- 100000L     # Monte Carlo replicates for chisq.test cross-check
MEAN_TOL        <- 1e-6        # rebuild must match fantasy_pts_std to this tolerance
MAX_PLAYERS_LOST <- 20L        # stop if more than this many of the 202 have no eligible week

# Post 32 verified values (mae_std | floor 8), used as a reproduction gate
P32_N    <- c(QB = 26L, RB = 70L, WR = 106L)
P32_MEAN <- c(QB = 8.272975, RB = 5.645086, WR = 5.157343)
P32_SSB  <- 203.399412
P32_SSW  <- 944.218806
P32_F    <- 21.433847

f6 <- function(x) sprintf("%.6f", x)
banner <- function(s) cat("\n====================", s, "====================\n")

# ---- 1. LOAD PANEL (copied from Post 34) -------------------------------------
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
            team        = last(team),
            n_teams     = n_distinct(team),
            n_weeks     = n(),
            mae_std     = mean(err_std),
            mae_roll3   = mean(err_roll3),
            mean_actual = mean(fantasy_pts_actual),
            .groups = "drop")
stopifnot(all(players$n_pos == 1))
cat("Players:", nrow(players), " Multi-team players:", sum(players$n_teams > 1), "\n")

# ---- 2. REPRODUCE POST 32 (GATE, copied from Post 34) ------------------------
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
cat("means:", paste(names(m_by), f6(m_by), collapse = " / "), "\n")
cat("SSB", f6(ssb), " SSW", f6(ssw), " F", f6(f_one), "\n")
gate_ok <- all(n_by == P32_N) &&
  max(abs(m_by - P32_MEAN)) < 1e-5 &&
  abs(ssb - P32_SSB) < 1e-4 &&
  abs(ssw - P32_SSW) < 1e-4 &&
  abs(f_one - P32_F) < 1e-4
cat("Post 32 reproduced:", gate_ok, "\n")
if (!gate_ok) stop("Post 32 numbers not reproduced. Stop and report; do not continue.")

# ---- 3. REBUILD EACH PLAYER'S EARLIER GAMES ----------------------------------
# Run 1 showed games_played_prior == 3 on every player's first forecast row, so
# three games (g1, g2, g3) precede the panel. With a = actual scores on forecast
# rows 1, 2, 3 and the forecast definitions
#   std_r   = mean of ALL earlier games
#   roll3_r = mean of the LAST THREE earlier games
# the three are exact:
#   g2 + g3 = 3 * roll3_2 - a1
#   g3      = 3 * roll3_3 - a1 - a2
#   g2      = (g2 + g3) - g3
#   g1      = 3 * std_1 - (g2 + g3)
# The algebra assumes those two definitions and that forecast rows are
# consecutive games. All three are checked on rows the algebra does NOT use:
#   (i)   games_played_prior rises by exactly 1 per forecast row;
#   (ii)  roll3 == mean of the last three actual scores on every row from 4 on,
#         and roll3_1 == std_1 (both are the mean of g1..g3);
#   (iii) std == mean of all rebuilt earlier games on every row from 2 on.
# A player failing (i) or (ii) on any row is excluded; a row failing (iii) is excluded.
banner("3. REBUILD AND VERIFY EARLIER GAMES")
hist25 <- p25 %>% filter(player_id %in% pl8$player_id) %>% arrange(player_id, week)
cat("Forecast rows for the", nrow(pl8), "players:", nrow(hist25), "\n")
first_gp <- hist25 %>% group_by(player_id) %>% slice(1) %>% ungroup()
cat("games_played_prior on each player's FIRST forecast row:\n")
print(table(first_gp$games_played_prior))

rebuild_one <- function(d) {
  d <- d[order(d$week), ]
  a  <- d$fantasy_pts_actual
  r3 <- d$fantasy_pts_roll3
  st <- d$fantasy_pts_std
  nr <- nrow(d)
  ok_step <- nr >= 3 && d$games_played_prior[1] == 3 &&
    all(diff(d$games_played_prior) == 1)
  ok_roll <- ok_step && abs(r3[1] - st[1]) < MEAN_TOL &&
    (nr < 4 || all(vapply(4:nr, function(r) abs(r3[r] - mean(a[(r - 3):(r - 1)])) < MEAN_TOL,
                          logical(1))))
  pre <- NA_real_
  if (ok_step) {
    s23 <- 3 * r3[2] - a[1]
    g3  <- 3 * r3[3] - a[1] - a[2]
    pre <- c(3 * st[1] - s23, s23 - g3, g3)
  }
  rows <- vector("list", nr)
  for (r in seq_len(nr)) {
    prior  <- c(pre, a[seq_len(r - 1)])
    ok_len <- ok_step && ok_roll && !anyNA(prior) && length(prior) == d$games_played_prior[r]
    pm     <- if (ok_len && length(prior) > 0) mean(prior) else NA_real_
    psd    <- if (ok_len && length(prior) >= 2) sd(prior) else NA_real_
    act    <- a[r]   # v3: was `a <- ...`, which overwrote the score vector after row 1
    rows[[r]] <- data.frame(
      player_id  = d$player_id[r],
      player_name = d$player_name[r],
      position   = d$position[r],
      week       = d$week[r],
      actual     = act,
      forecast   = d$fantasy_pts_std[r],
      n_prior    = if (ok_len) length(prior) else NA_integer_,
      prior_mean = pm,
      prior_sd   = psd,
      mean_gap   = if (is.na(pm)) NA_real_ else abs(pm - d$fantasy_pts_std[r]),
      n_below    = if (ok_len) sum(prior < act) else NA_integer_,
      n_tied     = if (ok_len) sum(prior == act) else NA_integer_,
      ok_len     = ok_len,
      ok_step    = ok_step,
      ok_roll    = ok_roll,
      g1 = pre[1], g2 = if (length(pre) == 3) pre[2] else NA_real_,
      g3 = if (length(pre) == 3) pre[3] else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  bind_rows(rows)
}
rb <- bind_rows(lapply(split(hist25, hist25$player_id), rebuild_one))
stopifnot(nrow(rb) == nrow(hist25))
rb <- rb %>% mutate(ok_mean = ok_len & !is.na(mean_gap) & mean_gap < MEAN_TOL)
pl_chk <- rb %>% group_by(player_id) %>%
  summarise(ok_step = first(ok_step), ok_roll = first(ok_roll),
            g1 = first(g1), g2 = first(g2), g3 = first(g3), .groups = "drop")
cat("(i)  players whose games_played_prior starts at 3 and rises by 1 per row:",
    sum(pl_chk$ok_step), "of", nrow(pl_chk), "\n")
cat("(ii) players whose roll3 matches its definition on every checkable row:",
    sum(pl_chk$ok_roll), "of", nrow(pl_chk), "\n")
cat("(iii) rows whose rebuilt mean matches fantasy_pts_std (tol", MEAN_TOL, "):",
    sum(rb$ok_mean), "of", nrow(rb), "\n")
gaps <- rb$mean_gap[!is.na(rb$mean_gap)]
cat("Largest |rebuilt mean - fantasy_pts_std| among rebuilt rows:",
    if (length(gaps)) f6(max(gaps)) else "none rebuilt", "\n")
rec <- unlist(pl_chk %>% filter(ok_step, ok_roll) %>% select(g1, g2, g3))
if (length(rec)) {
  cat("Sanity on", length(rec), "recovered pre-panel games: min", f6(min(rec)),
      " max", f6(max(rec)), " mean", f6(mean(rec)), "\n")
  cat("Recovered games below -5 points:", sum(rec < -5), "\n")
  cat("Recovered games within 1e-6 of a multiple of 0.01:",
      sum(abs(rec * 100 - round(rec * 100)) < 1e-4), "of", length(rec), "\n")
  cat("Same check on the panel's own actual scores:",
      sum(abs(hist25$fantasy_pts_actual * 100 - round(hist25$fantasy_pts_actual * 100)) < 1e-4),
      "of", nrow(hist25), "\n")
}

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
cat("Players with none:", length(lost), "\n")
if (length(lost)) print(pl8 %>% filter(player_id %in% lost) %>% count(position))
print(summary(per_pl$n_elig))
cat("Eligible weeks per player, distribution:\n"); print(table(per_pl$n_elig))
stopifnot(max(per_pl$n_elig) > 1)   # v3 guard: one week per player means the rebuild is broken again
cat("Rows failing (iii) among players who passed (i) and (ii):",
    sum(rb$ok_step & rb$ok_roll & !rb$ok_mean), "\n")
if (length(lost) > MAX_PLAYERS_LOST) {
  stop("More than ", MAX_PLAYERS_LOST, " players have no eligible week. Stop and report.")
}

# ---- 5. GOODNESS-OF-FIT ENGINE -----------------------------------------------
slice_of <- function(z, K) findInterval(z, qnorm(seq_len(K - 1) / K)) + 1L

gof <- function(slices, K) {
  stopifnot(length(slices) > 0, all(slices %in% seq_len(K)))
  obs     <- tabulate(slices, nbins = K)
  n       <- sum(obs)
  expd    <- rep(n / K, K)
  contrib <- (obs - expd)^2 / expd
  stat    <- sum(contrib)
  df      <- K - 1L
  list(obs = obs, expd = expd, contrib = contrib, n = n, stat = stat, df = df,
       p = pchisq(stat, df, lower.tail = FALSE),
       crit = qchisq(1 - ALPHA, df))
}

slice_labels <- function(K) {
  cuts <- c(-Inf, qnorm(seq_len(K - 1) / K), Inf)
  sprintf("slice %d (z %s to %s)", seq_len(K),
          ifelse(is.infinite(cuts[-(K + 1)]), "-Inf", sprintf("%.3f", cuts[-(K + 1)])),
          ifelse(is.infinite(cuts[-1]), "+Inf", sprintf("%.3f", cuts[-1])))
}

print_gof <- function(g, K) {
  print(data.frame(slice = slice_labels(K), observed = g$obs, expected = f6(g$expd),
                   obs_minus_exp = f6(g$obs - g$expd), contribution = f6(g$contrib)),
        row.names = FALSE)
  cat("n", g$n, " chi-square", f6(g$stat), " df", g$df, " critical (alpha ", ALPHA, ") ",
      f6(g$crit), " p ", signif(g$p, 6), "\n", sep = "")
}

pick_random <- function(df) {
  df %>% arrange(player_id, week) %>% group_by(player_id) %>%
    slice_sample(n = 1) %>% ungroup()
}

# ---- 6. MAIN: K = 5, one random eligible week per player ---------------------
banner(paste0("6. MAIN: K = ", K_MAIN, ", one random eligible week per player, seed ", SEED))
set.seed(SEED)
main <- pick_random(elig)
stopifnot(nrow(main) == nrow(per_pl), !any(duplicated(main$player_id)))
main <- main %>% mutate(sl = slice_of(z, K_MAIN))
g_main <- gof(main$sl, K_MAIN)
print_gof(g_main, K_MAIN)

ct <- chisq.test(g_main$obs, p = rep(1 / K_MAIN, K_MAIN))
cat("chisq.test() statistic", f6(unname(ct$statistic)), " p", signif(ct$p.value, 6), "\n")
stopifnot(abs(unname(ct$statistic) - g_main$stat) < 1e-8)
set.seed(SEED)
ct_mc <- chisq.test(g_main$obs, p = rep(1 / K_MAIN, K_MAIN),
                    simulate.p.value = TRUE, B = B_MC)
cat("Monte Carlo p (B =", B_MC, "):", signif(ct_mc$p.value, 6), "\n")

cat("\nTested weeks by position:", paste(names(table(main$position)),
                                         table(main$position), collapse = " / "), "\n")
cat("Earlier games per tested week:\n"); print(summary(main$n_prior))
cat("Tested week number:\n"); print(summary(main$week))
cat("Share of tested games BELOW the forecast (z < 0):", f6(mean(main$z < 0)),
    " (", sum(main$z < 0), " of ", nrow(main), ")\n", sep = "")
bt <- binom.test(sum(main$z < 0), nrow(main), p = 0.5)
cat("Exact binomial p vs 0.5:", signif(bt$p.value, 6), "\n")
cat("z summary:\n"); print(summary(main$z))
cat("Share with |z| > 2 (normal shape puts 4.55% there):", f6(mean(abs(main$z) > 2)),
    " (", sum(abs(main$z) > 2), ")\n", sep = "")

cat("\nSlice counts by position (descriptive only, K = 5):\n")
print(table(factor(main$position, levels = POSITIONS),
            factor(main$sl, levels = seq_len(K_MAIN))))

cat("\nWorked examples (per position: the tested week with the most earlier games,",
    "ties broken by name):\n")
ex <- main %>% group_by(position) %>% arrange(desc(n_prior), player_name, .by_group = TRUE) %>%
  slice(1) %>% ungroup()
cuts5 <- qnorm(seq_len(K_MAIN - 1) / K_MAIN)
for (i in seq_len(nrow(ex))) {
  e <- ex[i, ]
  edges <- e$prior_mean + cuts5 * e$prior_sd
  cat(sprintf("%s %s | week %d | %d earlier games | average %s | SD %s | actual %s | z %s | slice %d\n",
              e$position, e$player_name, as.integer(e$week), as.integer(e$n_prior),
              f6(e$prior_mean), f6(e$prior_sd), f6(e$actual), f6(e$z), e$sl))
  cat("   slice edges in fantasy points:", paste(f6(edges), collapse = " / "), "\n")
}

# ---- 7. ROBUSTNESS -----------------------------------------------------------
banner("7. ROBUSTNESS")

cat("\n-- 7a. Same draw, K =", K_ALT, "--\n")
g_alt <- gof(slice_of(main$z, K_ALT), K_ALT)
print_gof(g_alt, K_ALT)

cat("\n-- 7b. Earliest eligible week per player, K =", K_MAIN, "--\n")
early <- elig %>% arrange(player_id, week) %>% group_by(player_id) %>% slice(1) %>% ungroup()
print_gof(gof(slice_of(early$z, K_MAIN), K_MAIN), K_MAIN)

cat("\n-- 7c. Latest eligible week per player, K =", K_MAIN, "--\n")
late <- elig %>% arrange(player_id, week) %>% group_by(player_id) %>% slice(n()) %>% ungroup()
print_gof(gof(slice_of(late$z, K_MAIN), K_MAIN), K_MAIN)

cat("\n-- 7d.", N_DRAWS, "further random draws (seeds", SEED + 1L, "to", SEED + N_DRAWS, "), K =",
    K_MAIN, "--\n")
draws <- lapply(seq_len(N_DRAWS), function(i) {
  set.seed(SEED + i)
  g <- gof(slice_of(pick_random(elig)$z, K_MAIN), K_MAIN)
  c(stat = g$stat, p = g$p, setNames(g$obs, paste0("s", seq_len(K_MAIN))))
})
draws <- as.data.frame(do.call(rbind, draws))
cat("Share of draws with p < ", ALPHA, ": ", f6(mean(draws$p < ALPHA)), "\n", sep = "")
cat("p quantiles (0, 0.1, 0.5, 0.9, 1):", paste(signif(quantile(draws$p, c(0, .1, .5, .9, 1)), 4),
                                                collapse = " / "), "\n")
cat("Statistic quantiles (0, 0.1, 0.5, 0.9, 1):",
    paste(f6(quantile(draws$stat, c(0, .1, .5, .9, 1))), collapse = " / "), "\n")
cat("Median and range of each slice count across draws:\n")
print(data.frame(slice = seq_len(K_MAIN),
                 median = sapply(paste0("s", seq_len(K_MAIN)), function(s) median(draws[[s]])),
                 min = sapply(paste0("s", seq_len(K_MAIN)), function(s) min(draws[[s]])),
                 max = sapply(paste0("s", seq_len(K_MAIN)), function(s) max(draws[[s]]))),
      row.names = FALSE)

cat("\n-- 7e. Rank version, no normal assumption, same draw, K =", K_MAIN, "--\n")
# If the tested game behaves like one more draw from the same pool as the
# player's earlier games, its rank among all n_prior + 1 games is equally likely
# to be any position. u = (n_below + U * (n_tied + 1)) / (n_prior + 1), U ~ Unif(0,1),
# is then exactly Uniform(0, 1). Ties are split at random (seeded).
set.seed(SEED)
main <- main %>%
  mutate(u = (n_below + runif(n()) * (n_tied + 1)) / (n_prior + 1))
stopifnot(all(main$u > 0 & main$u < 1))
g_rank <- gof(findInterval(main$u, seq_len(K_MAIN - 1) / K_MAIN) + 1L, K_MAIN)
print(data.frame(slice = paste0("rank fifth ", seq_len(K_MAIN)), observed = g_rank$obs,
                 expected = f6(g_rank$expd), contribution = f6(g_rank$contrib)), row.names = FALSE)
cat("n", g_rank$n, " chi-square", f6(g_rank$stat), " df", g_rank$df, " p ",
    signif(g_rank$p, 6), "\n", sep = "")
cat("Tested games that beat or tie the player's BEST earlier game:",
    sum(main$n_below + main$n_tied == main$n_prior), "  that fall at or below the WORST:",
    sum(main$n_below == 0), "\n")

# ---- 8. CONTEXT ONLY: every eligible player-week (NOT independent) -----------
banner("8. CONTEXT ONLY: all eligible player-weeks pooled (not independent, not a test)")
g_all <- gof(slice_of(elig$z, K_MAIN), K_MAIN)
print(data.frame(slice = slice_labels(K_MAIN), observed = g_all$obs,
                 share = f6(g_all$obs / g_all$n)), row.names = FALSE)
cat("n", g_all$n, "player-weeks from", n_distinct(elig$player_id), "players.",
    "Shares only; no p-value is reported because rows from one player are not independent.\n")

cat("\nDone. Copy the full console output back verbatim.\n")
