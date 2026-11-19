# pull_stats_post33_two_way_anova_data.R
# Statistics Series, Post 33 (Concept 33, two-way ANOVA). Verification pull.
#
# Units   : 2025 players, one row each, from s2_week4_ab_panel.rds.
# Outcome : mae_std, the player's mean absolute miss of the season-to-date
#           forecast across his forecast games (same definition as Post 32).
# Factor 1: position (QB, RB, WR).
# Factor 2: career stage from nflreadr 2025 rosters, years_exp 0-2 = "early",
#           3 or more = "veteran".
#
# Rules fixed BEFORE any numbers exist (2026-09-28 session):
#   - Career-stage cutoff EARLY_MAX_EXP = 2. Not tuned after seeing results.
#   - Cell-size fallback: if any 3x2 cell at the main floor has fewer than
#     MIN_CELL players, the design drops to RB and WR only (2x2).
#   - Same season, 8-game floor, and outcome as Post 32.
#
# Gate: section 2 reproduces Post 32's verified one-way numbers and stops if
# they do not match. The aggregation block is copied from the Post 32 script.
#
# Read-only. Writes nothing to disk. Run from the toolkit root and paste the
# full console output back verbatim.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(nflreadr)
})
options(digits = 7, width = 120)

# ---- CONFIGURATION -----------------------------------------------------------
SEASON             <- 2025L
PANEL_PATH         <- here::here("data", "season2_cache", "s2_week4_ab_panel.rds")
POSITIONS          <- c("QB", "RB", "WR")
FALLBACK_POSITIONS <- c("RB", "WR")
N_ROWS_EXPECTED    <- 2922L
MAIN_FLOOR         <- 8L
FLOORS             <- c(4L, 8L, 12L)
EARLY_MAX_EXP      <- 2L          # pre-committed
CUTOFF_SENS        <- c(1L, 3L)   # sensitivity only, never the headline
MIN_CELL           <- 5L          # pre-committed
ALPHA              <- 0.05

# Post 32 verified values (mae_std | floor 8), used as a reproduction gate
P32_N    <- c(QB = 26L, RB = 70L, WR = 106L)
P32_MEAN <- c(QB = 8.272975, RB = 5.645086, WR = 5.157343)
P32_SSB  <- 203.399412
P32_SSW  <- 944.218806
P32_F    <- 21.433847

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

p25 <- p25 %>%
  mutate(err_std   = abs(fantasy_pts_actual - fantasy_pts_std),
         err_roll3 = abs(fantasy_pts_actual - fantasy_pts_roll3))
stopifnot(!anyNA(p25$err_std), !anyNA(p25$err_roll3))

# Per-player table: copied from the Post 32 pull script
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

# ---- 2. REPRODUCE POST 32 (GATE) ---------------------------------------------
banner("2. POST 32 REPRODUCTION GATE")
pl8 <- players %>% filter(n_weeks >= MAIN_FLOOR)
n_by   <- as.integer(table(factor(pl8$position, levels = POSITIONS)))
names(n_by) <- POSITIONS
m_by   <- tapply(pl8$mae_std, factor(pl8$position, levels = POSITIONS), mean)
grand  <- mean(pl8$mae_std)
ssb    <- sum(n_by * (m_by - grand)^2)
ssw    <- sum((pl8$mae_std - ave(pl8$mae_std, pl8$position))^2)
f_one  <- (ssb / (length(POSITIONS) - 1)) / (ssw / (nrow(pl8) - length(POSITIONS)))
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

# ---- 3. ROSTER JOIN ----------------------------------------------------------
banner("3. ROSTER EXPERIENCE JOIN")
ros <- nflreadr::load_rosters(seasons = SEASON)
need_r <- c("season", "gsis_id", "full_name", "position", "years_exp",
            "rookie_year", "entry_year")
miss_r <- setdiff(need_r, names(ros))
if (length(miss_r)) stop("Roster missing columns: ", paste(miss_r, collapse = ", "))
cat("Roster rows:", nrow(ros), "\n")
ros <- ros %>% filter(!is.na(gsis_id), gsis_id != "")
cat("Roster rows with a gsis_id:", nrow(ros), " distinct gsis_id:",
    n_distinct(ros$gsis_id), "\n")

exp_lookup <- ros %>%
  group_by(gsis_id) %>%
  summarise(n_roster_rows = n(),
            n_exp_values  = n_distinct(years_exp, na.rm = TRUE),
            years_exp     = if (n_distinct(years_exp, na.rm = TRUE) == 1L)
                              as.integer(first(na.omit(years_exp))) else NA_integer_,
            rookie_year   = as.integer(first(rookie_year)),
            entry_year    = as.integer(first(entry_year)),
            roster_pos    = first(position),
            .groups = "drop")
cat("gsis_id with conflicting years_exp across roster rows:",
    sum(exp_lookup$n_exp_values > 1), "\n")

n_before <- nrow(players)
players <- players %>% left_join(exp_lookup, by = c("player_id" = "gsis_id"))
stopifnot(nrow(players) == n_before)

report_missing <- function(df, lbl) {
  unm  <- df %>% filter(is.na(n_roster_rows))
  noex <- df %>% filter(!is.na(n_roster_rows), is.na(years_exp))
  cat(lbl, ": unmatched to roster", nrow(unm), " matched but years_exp missing or conflicting",
      nrow(noex), "\n")
  bad <- bind_rows(unm, noex)
  if (nrow(bad)) print(as.data.frame(bad %>% select(player_id, player_name, position,
                                                    n_weeks, n_roster_rows, n_exp_values)))
}
report_missing(players, "All 2025 players")
report_missing(players %>% filter(n_weeks >= MAIN_FLOOR), paste("Floor", MAIN_FLOOR))

chk <- players %>% filter(n_weeks >= MAIN_FLOOR, !is.na(years_exp))
cat("\nFloor", MAIN_FLOOR, "matched players:", nrow(chk), "\n")
cat("years_exp == SEASON - rookie_year:", sum(chk$years_exp == SEASON - chk$rookie_year, na.rm = TRUE),
    " == SEASON - entry_year:", sum(chk$years_exp == SEASON - chk$entry_year, na.rm = TRUE),
    " rookie_year NA:", sum(is.na(chk$rookie_year)), " entry_year NA:", sum(is.na(chk$entry_year)), "\n")
disagree <- chk %>% filter(years_exp != SEASON - rookie_year | years_exp != SEASON - entry_year)
if (nrow(disagree)) {
  cat("Rows where years_exp disagrees with rookie_year or entry_year:\n")
  print(as.data.frame(disagree %>% select(player_name, position, years_exp, rookie_year, entry_year)))
}
cat("Roster position differs from panel position:", sum(chk$roster_pos != chk$position, na.rm = TRUE), "\n")
cat("years_exp distribution at floor", MAIN_FLOOR, ":\n")
print(table(chk$position, chk$years_exp))

# ---- 4. CELLS AND DESIGN DECISION --------------------------------------------
banner("4. CELL COUNTS AND DESIGN DECISION")
add_stage <- function(df, early_max) {
  df %>% mutate(stage = factor(if_else(years_exp <= early_max, "early", "veteran"),
                               levels = c("early", "veteran")))
}
analysis_base <- players %>% filter(!is.na(years_exp))

cell_table <- function(df, positions) {
  table(factor(df$position, levels = positions), df$stage)
}
for (fl in FLOORS) {
  d <- analysis_base %>% filter(n_weeks >= fl) %>% add_stage(EARLY_MAX_EXP)
  cat("\nFloor", fl, "3x2 cell counts (cutoff years_exp <=", EARLY_MAX_EXP, "):\n")
  print(cell_table(d, POSITIONS))
}
main3 <- analysis_base %>% filter(n_weeks >= MAIN_FLOOR) %>% add_stage(EARLY_MAX_EXP)
min3  <- min(cell_table(main3, POSITIONS))
design_positions <- if (min3 >= MIN_CELL) POSITIONS else FALLBACK_POSITIONS
cat("\nSmallest 3x2 cell at floor", MAIN_FLOOR, ":", min3, " Rule threshold:", MIN_CELL, "\n")
cat("DESIGN SELECTED:", paste(design_positions, collapse = "/"), "x career stage",
    if (identical(design_positions, POSITIONS)) "(3x2, rule not triggered)" else "(2x2 fallback, rule triggered)", "\n")
min_sel <- min(cell_table(main3 %>% filter(position %in% design_positions), design_positions))
if (min_sel < MIN_CELL) stop("Selected design still has a cell below MIN_CELL. Stop and report.")

# ---- 5. TWO-WAY ANOVA --------------------------------------------------------
two_way <- function(df, outcome, positions, verbose = TRUE) {
  d <- df %>%
    filter(position %in% positions) %>%
    mutate(position = factor(position, levels = positions),
           y = .data[[outcome]])
  stopifnot(nrow(d) > 0, !anyNA(d$y))
  cells <- d %>%
    group_by(position, stage, .drop = FALSE) %>%
    summarise(n = n(), mean_actual = mean(mean_actual),
              mean = mean(y), var = var(y), .groups = "drop")
  if (any(cells$n < 2)) stop("A cell has fewer than 2 players; variance undefined.")

  a <- nlevels(d$position); b <- nlevels(d$stage); N <- nrow(d)
  grand <- mean(d$y)
  sst   <- sum((d$y - grand)^2)
  ssw   <- sum((d$y - ave(d$y, d$position, d$stage))^2)
  rss   <- function(f) sum(resid(lm(f, data = d))^2)
  rA    <- rss(y ~ position)
  rB    <- rss(y ~ stage)
  rAB   <- rss(y ~ position + stage)
  rfull <- rss(y ~ position * stage)
  stopifnot(abs(rfull - ssw) < 1e-8 * max(1, sst))

  df_A <- a - 1; df_B <- b - 1; df_AB <- (a - 1) * (b - 1); df_W <- N - a * b
  msw  <- ssw / df_W

  typeI_pos_first   <- c(position = sst - rA, stage = rA - rAB, interaction = rAB - rfull)
  typeI_stage_first <- c(stage = sst - rB, position = rB - rAB, interaction = rAB - rfull)
  typeII            <- c(position = rB - rAB, stage = rA - rAB, interaction = rAB - rfull)

  fit3 <- lm(y ~ position * stage, data = d,
             contrasts = list(position = "contr.sum", stage = "contr.sum"))
  X    <- model.matrix(fit3); asgn <- attr(X, "assign")
  labs <- attr(terms(fit3), "term.labels")
  t3   <- vapply(seq_along(labs), function(k)
            sum(lm.fit(X[, asgn != k, drop = FALSE], d$y)$residuals^2) - rfull, numeric(1))
  typeIII <- c(position = t3[1], stage = t3[2], interaction = t3[3])

  dfs  <- c(position = df_A, stage = df_B, interaction = df_AB)
  ftab <- function(ss) {
    Fv <- (ss / dfs) / msw
    data.frame(term = names(ss), SS = f6(ss), df = dfs, F = f6(Fv),
               p = signif(pf(Fv, dfs, df_W, lower.tail = FALSE), 6), row.names = NULL)
  }
  f_int  <- (typeII[["interaction"]] / df_AB) / msw
  p_int  <- pf(f_int, df_AB, df_W, lower.tail = FALSE)
  share_int <- typeII[["interaction"]] / sst

  if (verbose) {
    add_fit <- lm(y ~ position + stage, data = d)
    grid <- expand.grid(position = levels(d$position), stage = levels(d$stage))
    grid$additive <- predict(add_fit, newdata = grid)
    cells_out <- cells %>%
      left_join(grid, by = c("position", "stage")) %>%
      mutate(departure = mean - additive,
             rel_miss  = if_else(mean_actual > 0, mean / mean_actual, NA_real_))
    cat("\nCells (mean = average of player misses; additive = what position + stage alone predict):\n")
    print(as.data.frame(cells_out), digits = 7)
    cat("Balanced (equal n in every cell):", length(unique(cells$n)) == 1, "\n")

    gaps <- cells %>% group_by(position) %>%
      summarise(early = mean[stage == "early"], veteran = mean[stage == "veteran"],
                early_minus_veteran = early - veteran, .groups = "drop")
    cat("\nEarly-minus-veteran gap by position:\n"); print(as.data.frame(gaps), digits = 7)
    if (a == 2) cat("Difference of gaps (", positions[1], "gap minus", positions[2], "gap):",
                    f6(gaps$early_minus_veteran[1] - gaps$early_minus_veteran[2]), "\n")

    wm_pos <- tapply(d$y, d$position, mean); um_pos <- tapply(cells$mean, cells$position, mean)
    wm_stg <- tapply(d$y, d$stage, mean);    um_stg <- tapply(cells$mean, cells$stage, mean)
    cat("\nMarginal means, weighted by n vs unweighted mean of cell means:\n")
    print(data.frame(level = c(names(wm_pos), names(wm_stg)),
                     weighted = f6(c(wm_pos, wm_stg)), unweighted = f6(c(um_pos, um_stg))))

    cat("\nN", N, " grand mean", f6(grand), " SST", f6(sst), " SSW", f6(ssw),
        " df_W", df_W, " MSW", f6(msw), "\n")
    cat("\nType I, position entered first:\n");  print(ftab(typeI_pos_first[names(dfs)]))
    cat("\nType I, stage entered first:\n");     print(ftab(typeI_stage_first[names(dfs)]))
    cat("\nType II:\n");                         print(ftab(typeII))
    cat("\nType III (sum-to-zero contrasts):\n"); print(ftab(typeIII))
    cat("\nInteraction SS identical in both Type I orders and Type II:",
        isTRUE(all.equal(typeI_pos_first[["interaction"]], typeI_stage_first[["interaction"]])) &&
        isTRUE(all.equal(typeI_pos_first[["interaction"]], typeII[["interaction"]])), "\n")
    cat("Critical F for interaction at alpha", ALPHA, ":", f6(qf(1 - ALPHA, df_AB, df_W)), "\n")
    cat("Interaction share of total (Type II SS / SST):", f6(share_int), "\n")
    cat("Type II pieces + SSW:", f6(sum(typeII) + ssw), " vs SST:", f6(sst),
        " gap:", f6(sst - sum(typeII) - ssw), "\n")

    cat("\naov() cross-check, both orders (Type I):\n")
    print(summary(aov(y ~ position * stage, data = d)))
    print(summary(aov(y ~ stage * position, data = d)))
    if (requireNamespace("car", quietly = TRUE)) {
      cat("\ncar::Anova cross-check:\n")
      print(car::Anova(lm(y ~ position * stage, data = d), type = 2))
      print(car::Anova(fit3, type = 3))
    } else cat("\ncar not installed; Type II and III above are computed by hand only.\n")

    v <- cells$var
    cat("\nLargest-to-smallest cell variance ratio:", f6(max(v) / min(v)), "\n")
    r <- resid(fit3)
    if (length(r) >= 3 && length(r) <= 5000)
      cat("Shapiro-Wilk on full-model residuals: W", f6(shapiro.test(r)$statistic),
          " p", signif(shapiro.test(r)$p.value, 6), "\n")
  }
  list(N = N, min_cell = min(cells$n), f_int = f_int, p_int = p_int,
       share_int = share_int, df_AB = df_AB, df_W = df_W)
}

banner(paste0("5. MAIN: mae_std | floor ", MAIN_FLOOR, " | cutoff <= ", EARLY_MAX_EXP))
main_res <- two_way(main3, "mae_std", design_positions)

# ---- 6. ROBUSTNESS (design held fixed) ---------------------------------------
banner("6. ROBUSTNESS: interaction test, same design")
run_row <- function(label, df, outcome) {
  out <- tryCatch(two_way(df, outcome, design_positions, verbose = FALSE),
                  error = function(e) { cat(label, "-> not computable:", conditionMessage(e), "\n"); NULL })
  if (is.null(out)) return(NULL)
  data.frame(check = label, N = out$N, min_cell = out$min_cell,
             F_int = f6(out$f_int), df = paste(out$df_AB, out$df_W, sep = ","),
             p_int = signif(out$p_int, 6), share_int = f6(out$share_int))
}
rows <- list(run_row(paste("MAIN floor", MAIN_FLOOR), main3, "mae_std"))
for (fl in setdiff(FLOORS, MAIN_FLOOR)) {
  rows[[length(rows) + 1]] <- run_row(paste("floor", fl),
    analysis_base %>% filter(n_weeks >= fl) %>% add_stage(EARLY_MAX_EXP), "mae_std")
}
rows[[length(rows) + 1]] <- run_row("rolling 3-game predictor (mae_roll3)", main3, "mae_roll3")
for (cut in CUTOFF_SENS) {
  rows[[length(rows) + 1]] <- run_row(paste("cutoff years_exp <=", cut),
    analysis_base %>% filter(n_weeks >= MAIN_FLOOR) %>% add_stage(cut), "mae_std")
}

# Confound check: scoring level. Miss divided by the player's own average points.
rel <- main3 %>% mutate(rel_miss = if_else(mean_actual > 0, mae_std / mean_actual, NA_real_))
cat("Players dropped from relative-miss check (average points <= 0):",
    sum(is.na(rel$rel_miss[rel$position %in% design_positions])), "\n")
rows[[length(rows) + 1]] <- run_row("relative miss (mae_std / mean_actual)",
                                    rel %>% filter(!is.na(rel_miss)), "rel_miss")
print(bind_rows(rows))

cat("\nDone. Copy the full console output back verbatim.\n")
