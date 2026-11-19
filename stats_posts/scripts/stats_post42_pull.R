# ==============================================================================
# stats_post42_pull.R
# Statistics Series Post 42 (Correlation vs. causation) -- verified data pull
#
# READ-ONLY against data/season2_cache/ (the 2026-09-17 rerun).
# Writes only stats_posts/output/data/stats_post42_*.csv. Edits no toolkit file.
# Run from the nfl-analytics-toolkit project root:  source("stats_posts/scripts/stats_post42_pull.R")
#
# Sections
#   0  File inventory and structure dump (column verification before use)
#   1  R/18 stored A/B results (prediction study, read as stored)
#   2  R/25 injury study: reproduce, then pre-period gap, within-player change,
#      and a stratified comparison on prior-season PPG
#   3  R/26 usage ramp study: reproduce outcome, then weeks 1-8 PPG by group
#   4  CSV export
#   5  Key findings, every number and direction computed from this run
#
# Assumes dplyr >= 1.1 (relationship = argument on joins).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(glue)
})

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------
CACHE_DIR         <- here::here("data", "season2_cache")
OUT_DIR           <- here::here("stats_posts", "output", "data")
TOL               <- 1e-4      # reproduction tolerance against stored values
SEED              <- 42L
B_BOOT            <- 2000L     # bootstrap resamples for new quantities
N_BINS            <- 5L        # strata for the stratified comparison
OUTCOME_WEEKS     <- 9L:18L    # matches OUTCOME_WEEKS_W11 / _W12
TRAINING_WEEKS    <- 1L:8L     # matches TRAINING_WEEKS_W11 / _W12
MIN_OUTCOME_GAMES <- 3L        # matches MIN_OUTCOME_GAMES_W12

# Stored values from the 2026-11-02 read-only pull report (current cache)
EXP_R18 <- dplyr::tribble(
  ~position, ~n_player_weeks, ~r_std, ~r_roll3,
  "QB",       6566L,          0.3770, 0.3370,
  "RB",      15199L,          0.5816, 0.5653,
  "WR",      23021L,          0.4649, 0.4192
)
EXP_R25 <- list(n_trt = 115L, n_ctl = 1829L,
                prior_trt = 8.292955, prior_ctl = 12.109796,
                out_trt = 7.654706,   out_ctl = 12.187294,
                effect_ppg = -4.53258786)
EXP_R26 <- list(n_trt = 1085L, n_ctl = 630L,
                out_trt = 9.522617, out_ctl = 10.878057,
                effect_ppg = -1.35544061)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------------------------
banner <- function(x) {
  cat("\n", strrep("=", 74), "\n", x, "\n", strrep("=", 74), "\n", sep = "")
}

gate <- function(ok, msg) {
  if (!isTRUE(ok)) stop("GATE FAILED: ", msg, call. = FALSE)
  cat("  [PASS] ", msg, "\n", sep = "")
}

near <- function(a, b, tol = TOL) {
  length(a) == 1L && length(b) == 1L && !is.na(a) && !is.na(b) &&
    abs(as.numeric(a) - as.numeric(b)) < tol
}

need_cols <- function(df, cols, label) {
  miss <- setdiff(cols, names(df))
  if (length(miss) > 0L) {
    cat("\n", label, " columns actually present:\n", sep = "")
    print(names(df))
    stop(label, " is missing column(s): ", paste(miss, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

describe <- function(obj, label) {
  cat("\n-- ", label, " --\n", sep = "")
  if (is.data.frame(obj)) {
    cat("data.frame, ", nrow(obj), " rows\n", sep = "")
    print(names(obj))
  } else if (is.list(obj)) {
    cat("list with elements: ", paste(names(obj), collapse = ", "), "\n", sep = "")
    for (nm in names(obj)) {
      el <- obj[[nm]]
      if (is.data.frame(el)) {
        cat("  $", nm, ": data.frame ", nrow(el), " rows | ",
            paste(names(el), collapse = ", "), "\n", sep = "")
      } else if (is.list(el)) {
        cat("  $", nm, ": list | ", paste(names(el), collapse = ", "), "\n", sep = "")
      } else {
        cat("  $", nm, ": ", paste(class(el), collapse = "/"), "\n", sep = "")
      }
    }
  } else {
    cat(paste(class(obj), collapse = "/"), "\n")
  }
}

# Weighted stratified difference (treatment minus control within each stratum,
# weighted by the treatment count in that stratum). Breaks are FIXED from the
# full sample and passed in, so bootstrap draws reuse the same strata.
strat_diff <- function(trt_x, trt_y, ctl_x, ctl_y, breaks) {
  b_t <- cut(trt_x, breaks, include.lowest = TRUE)
  b_c <- cut(ctl_x, breaks, include.lowest = TRUE)
  lv  <- levels(b_t)
  num <- 0; den <- 0
  for (l in lv) {
    nt <- sum(b_t == l, na.rm = TRUE)
    if (nt == 0L) next
    nc <- sum(b_c == l, na.rm = TRUE)
    if (nc == 0L) return(NA_real_)      # treatment stratum with no control
    d   <- mean(trt_y[b_t == l & !is.na(b_t)]) - mean(ctl_y[b_c == l & !is.na(b_c)])
    num <- num + nt * d
    den <- den + nt
  }
  if (den == 0) NA_real_ else num / den
}

# Percentile bootstrap, groups resampled independently, seeded.
boot_pct <- function(trt, ctl, stat_fn, B = B_BOOT, seed = SEED) {
  est <- stat_fn(trt, ctl)
  set.seed(seed)
  draws <- vapply(seq_len(B), function(i) {
    stat_fn(trt[sample.int(nrow(trt), replace = TRUE), , drop = FALSE],
            ctl[sample.int(nrow(ctl), replace = TRUE), , drop = FALSE])
  }, numeric(1))
  c(estimate = est,
    ci_lower = unname(stats::quantile(draws, 0.025, na.rm = TRUE)),
    ci_upper = unname(stats::quantile(draws, 0.975, na.rm = TRUE)),
    n_na     = sum(is.na(draws)))
}

bin_table <- function(df, xvar, yvar, breaks) {
  df %>%
    mutate(stratum = cut(.data[[xvar]], breaks, include.lowest = TRUE)) %>%
    group_by(stratum, group) %>%
    summarise(n = n(),
              mean_x = mean(.data[[xvar]]),
              mean_y = mean(.data[[yvar]]),
              .groups = "drop") %>%
    arrange(stratum, group)
}

# ==============================================================================
# SECTION 0: FILE INVENTORY AND STRUCTURE
# ==============================================================================
banner("SECTION 0: FILE INVENTORY AND STRUCTURE")

files <- c(
  r18_results  = "s2_week4_ab_results.rds",
  r25_groups   = "s2_week11_groups.rds",
  r25_effects  = "s2_week11_effects.rds",
  r26_groups   = "s2_week12_groups.rds",
  r26_effects  = "s2_week12_effects.rds",
  r26_fantasy  = "s2_week12_weekly_fantasy.rds"
)
paths <- file.path(CACHE_DIR, files)
names(paths) <- names(files)

gate(all(file.exists(paths)), "all six cache files exist")
inv <- file.info(unname(paths))      # row names are full paths, so index by position
for (i in seq_along(paths)) {
  cat(sprintf("  %-12s %10s B  %s  %s\n", names(paths)[i],
              format(inv$size[i], big.mark = ","),
              format(inv$mtime[i], "%Y-%m-%d %H:%M"), files[[i]]))
}
stale <- names(paths)[as.Date(inv$mtime) < as.Date("2026-09-17")]
gate(length(stale) == 0L,
     "no file predates the 2026-09-17 rerun (pre-rerun results are invalidated)")

r18  <- readRDS(paths[["r18_results"]])
g25  <- readRDS(paths[["r25_groups"]])
e25  <- readRDS(paths[["r25_effects"]])
g26  <- readRDS(paths[["r26_groups"]])
e26  <- readRDS(paths[["r26_effects"]])
wf26 <- readRDS(paths[["r26_fantasy"]])

describe(r18,  "r18_results")
describe(g25,  "r25_groups")
describe(e25,  "r25_effects")
describe(g26,  "r26_groups")
describe(e26,  "r26_effects")
describe(wf26, "r26_weekly_fantasy")

# ==============================================================================
# SECTION 1: R/18 STORED RESULTS (prediction study)
# ==============================================================================
banner("SECTION 1: R/18 A/B PREDICTIVE VALIDITY (stored values)")

need_cols(r18, c("position", "n_player_weeks", "r_std", "r_roll3",
                 "p_value_adj", "cohens_q", "better_predictor"), "r18_results")

r18_chk <- r18 %>% inner_join(EXP_R18, by = "position", suffix = c("", "_exp"))
gate(nrow(r18_chk) == 3L, "R/18 has QB, RB, WR rows")
gate(all(r18_chk$n_player_weeks == r18_chk$n_player_weeks_exp),
     "R/18 n_player_weeks match the pull report")
gate(all(abs(r18_chk$r_std - r18_chk$r_std_exp) < TOL) &&
       all(abs(r18_chk$r_roll3 - r18_chk$r_roll3_exp) < TOL),
     "R/18 r_std and r_roll3 match the pull report")

r18_out <- r18 %>%
  transmute(position, n_player_weeks,
            r_season_avg = r_std, r_last3 = r_roll3,
            r_diff_computed_here = r_std - r_roll3,
            p_value_adj, cohens_q, better_predictor)
print(as.data.frame(r18_out), digits = 6)
cat("  Note: r_diff_computed_here is not a stored column; it is r_std - r_roll3.\n")

# ==============================================================================
# SECTION 2: R/25 INJURY PROXIMITY STUDY
# ==============================================================================
banner("SECTION 2: R/25 INJURY PROXIMITY STUDY")

need_cols(e25, c("group_summary", "effect_ppg", "player_level"), "r25_effects")
pl <- e25$player_level
need_cols(pl, c("player_id", "season", "group", "prior_ppg", "outcome_ppg"),
          "r25_effects$player_level")

gate(setequal(unique(pl$group), c("treatment", "control")),
     "player_level groups are exactly treatment and control")
gate(!anyDuplicated(pl[, c("player_id", "season")]),
     "player_level is unique on player_id + season")
gate(!anyNA(pl$prior_ppg) && !anyNA(pl$outcome_ppg),
     "no NA in prior_ppg or outcome_ppg")

s25 <- pl %>%
  group_by(group) %>%
  summarise(n = n(),
            mean_prior_ppg   = mean(prior_ppg),
            mean_outcome_ppg = mean(outcome_ppg),
            mean_change      = mean(outcome_ppg - prior_ppg),
            .groups = "drop")
trt <- s25[s25$group == "treatment", ]
ctl <- s25[s25$group == "control", ]

cat("\n2A. Reproduction gates\n")
gate(trt$n == EXP_R25$n_trt && ctl$n == EXP_R25$n_ctl,
     glue("effect sample n = {trt$n} treatment / {ctl$n} control"))
gate(near(trt$mean_prior_ppg, EXP_R25$prior_trt) &&
       near(ctl$mean_prior_ppg, EXP_R25$prior_ctl),
     "prior-season PPG means reproduce group_summary")
gate(near(trt$mean_outcome_ppg, EXP_R25$out_trt) &&
       near(ctl$mean_outcome_ppg, EXP_R25$out_ctl),
     "weeks 9-18 PPG means reproduce group_summary")
gs25 <- e25$group_summary
need_cols(gs25, c("group", "mean_prior_ppg", "mean_outcome_ppg"), "r25 group_summary")
gate(near(gs25$mean_outcome_ppg[gs25$group == "treatment"], trt$mean_outcome_ppg) &&
       near(gs25$mean_prior_ppg[gs25$group == "control"], ctl$mean_prior_ppg),
     "recomputed means match the stored group_summary object")

out_gap   <- trt$mean_outcome_ppg - ctl$mean_outcome_ppg
prior_gap <- trt$mean_prior_ppg   - ctl$mean_prior_ppg
chg_gap   <- trt$mean_change      - ctl$mean_change
gate(near(out_gap, e25$effect_ppg$estimate),
     glue("outcome gap {round(out_gap, 4)} equals stored effect_ppg estimate"))
gate(near(chg_gap, out_gap - prior_gap, 1e-9),
     "identity: change gap = outcome gap - prior gap")

cat("\n2B. Group means (PPG = fantasy points per game, toolkit scoring)\n")
print(as.data.frame(s25), digits = 6)

share_prior <- if (out_gap != 0) prior_gap / out_gap else NA_real_
cat(sprintf("\n  Weeks 9-18 gap (treatment - control):        %8.4f\n", out_gap))
cat(sprintf("  Prior-season gap (treatment - control):      %8.4f\n", prior_gap))
cat(sprintf("  Prior gap as share of outcome gap:           %8.4f\n", share_prior))
cat(sprintf("  Within-player change gap (trt - ctl):        %8.4f\n", chg_gap))

cat("\n2C. Bootstrap CIs on the new quantities (percentile, B = ", B_BOOT,
    ", seed ", SEED, ", groups resampled independently)\n", sep = "")
trt_df <- pl %>% filter(group == "treatment")
ctl_df <- pl %>% filter(group == "control")

ci_prior <- boot_pct(trt_df, ctl_df,
                     function(t, c) mean(t$prior_ppg) - mean(c$prior_ppg))
ci_chg   <- boot_pct(trt_df, ctl_df,
                     function(t, c) mean(t$outcome_ppg - t$prior_ppg) -
                                    mean(c$outcome_ppg - c$prior_ppg))
cat(sprintf("  Prior-season gap: %8.4f  [%8.4f, %8.4f]\n",
            ci_prior["estimate"], ci_prior["ci_lower"], ci_prior["ci_upper"]))
cat(sprintf("  Change gap:       %8.4f  [%8.4f, %8.4f]\n",
            ci_chg["estimate"], ci_chg["ci_lower"], ci_chg["ci_upper"]))
cat(sprintf("  Stored outcome gap CI (basic bootstrap, B = 1000): [%8.4f, %8.4f]\n",
            e25$effect_ppg$ci_lower, e25$effect_ppg$ci_upper))

cat("\n2D. Stratified comparison: compare players with similar prior-season PPG\n")
breaks25 <- stats::quantile(pl$prior_ppg, probs = seq(0, 1, length.out = N_BINS + 1L),
                            names = FALSE)
gate(!anyDuplicated(breaks25), "prior-season PPG quintile breaks are distinct")
cat("  Breaks (combined sample): ", paste(round(breaks25, 3), collapse = " | "), "\n")

bins25 <- bin_table(pl, "prior_ppg", "outcome_ppg", breaks25)
print(as.data.frame(bins25), digits = 5)

ci_strat25 <- boot_pct(trt_df, ctl_df, function(t, c)
  strat_diff(t$prior_ppg, t$outcome_ppg, c$prior_ppg, c$outcome_ppg, breaks25))
cat(sprintf("\n  Stratified gap (weighted by treatment n): %8.4f  [%8.4f, %8.4f]  NA draws: %d\n",
            ci_strat25["estimate"], ci_strat25["ci_lower"], ci_strat25["ci_upper"],
            as.integer(ci_strat25["n_na"])))

cat("\n2E. Treatment-group composition (groups file)\n")
need_cols(g25, c("group", "first_absent_week", "return_week",
                 "n_absent_weeks", "n_prior_games"), "r25_groups")
g25_trt <- g25 %>% filter(group == "treatment")
combo25 <- g25_trt %>%
  count(first_absent_week, return_week, n_absent_weeks, n_prior_games, name = "n")
print(as.data.frame(combo25))
cat("  Groups file counts: ", paste(names(table(g25$group)), table(g25$group),
                                   sep = " = ", collapse = " | "), "\n")

pos25 <- NULL
if ("position" %in% names(g25) &&
    all(c("player_id", "season") %in% names(g25))) {
  g25_tc <- g25 %>%
    filter(group %in% c("treatment", "control")) %>%
    distinct(player_id, season, position)
  if (anyDuplicated(g25_tc[, c("player_id", "season")])) {
    cat("  Position skipped: groups file has >1 position per player-season.\n")
  } else {
    pos25 <- pl %>%
      left_join(g25_tc, by = c("player_id", "season"), relationship = "many-to-one") %>%
      count(group, position) %>%
      group_by(group) %>%
      mutate(pct = round(100 * n / sum(n), 1)) %>%
      ungroup()
    gate(sum(pos25$n) == nrow(pl), "position join kept every player-season")
    print(as.data.frame(pos25))
  }
} else {
  cat("  Position skipped: no position column in the groups file.\n")
}

# ==============================================================================
# SECTION 3: R/26 USAGE RAMP STUDY
# ==============================================================================
banner("SECTION 3: R/26 USAGE RAMP STUDY")

need_cols(g26, c("player_id", "season", "group", "early_avg", "late_avg",
                 "relative_change"), "r26_groups")
need_cols(e26, c("group_summary", "effect_ppg"), "r26_effects")
need_cols(wf26, c("player_id", "season", "week", "total_fantasy_points"),
          "r26_weekly_fantasy")

n_na_pts <- sum(is.na(wf26$total_fantasy_points))
cat("  NA total_fantasy_points rows in weekly fantasy: ", n_na_pts, "\n", sep = "")
gate(n_na_pts == 0L, "no NA fantasy points in the R/26 weekly fantasy file")
dup_wf <- sum(duplicated(wf26[, c("player_id", "season", "week")]))
gate(dup_wf == 0L, "weekly fantasy is unique on player_id + season + week")

if ("season_type" %in% names(wf26)) {
  n_post <- sum(wf26$week %in% OUTCOME_WEEKS & wf26$season_type != "REG", na.rm = TRUE)
  cat("  season_type present; non-REG rows inside weeks 9-18: ", n_post, "\n", sep = "")
} else {
  cat("  season_type not present in weekly fantasy.\n")
}

outcome26 <- wf26 %>%
  filter(week %in% OUTCOME_WEEKS) %>%
  group_by(player_id, season) %>%
  summarise(outcome_ppg = mean(total_fantasy_points), n_out = n(), .groups = "drop") %>%
  filter(n_out >= MIN_OUTCOME_GAMES)

first26 <- wf26 %>%
  filter(week %in% TRAINING_WEEKS) %>%
  group_by(player_id, season) %>%
  summarise(h1_ppg = mean(total_fantasy_points), n_h1 = n(), .groups = "drop")

g26_tc <- g26 %>% filter(group %in% c("treatment", "control"))
gate(!anyDuplicated(g26_tc[, c("player_id", "season")]),
     "R/26 treatment/control rows are unique on player_id + season")

a26 <- g26_tc %>%
  inner_join(outcome26, by = c("player_id", "season"), relationship = "one-to-one") %>%
  left_join(first26, by = c("player_id", "season"), relationship = "one-to-one")

s26 <- a26 %>%
  group_by(group) %>%
  summarise(n = n(),
            mean_early_usage = mean(early_avg),
            mean_late_usage  = mean(late_avg),
            n_missing_h1     = sum(is.na(h1_ppg)),
            mean_h1_ppg      = mean(h1_ppg, na.rm = TRUE),
            mean_outcome_ppg = mean(outcome_ppg),
            .groups = "drop")
t26 <- s26[s26$group == "treatment", ]
c26 <- s26[s26$group == "control", ]

cat("\n3A. Reproduction gates\n")
gate(t26$n == EXP_R26$n_trt && c26$n == EXP_R26$n_ctl,
     glue("effect sample n = {t26$n} treatment / {c26$n} control"))
gate(near(t26$mean_outcome_ppg, EXP_R26$out_trt) &&
       near(c26$mean_outcome_ppg, EXP_R26$out_ctl),
     "weeks 9-18 PPG means reproduce the stored group_summary")
out26_gap <- t26$mean_outcome_ppg - c26$mean_outcome_ppg
gate(near(out26_gap, e26$effect_ppg$estimate),
     glue("outcome gap {round(out26_gap, 4)} equals stored effect_ppg estimate"))
gate(t26$n_missing_h1 == 0L && c26$n_missing_h1 == 0L,
     "every analysis player-season has weeks 1-8 games")

cat("\n3B. Group means\n")
print(as.data.frame(s26), digits = 6)

h1_gap  <- t26$mean_h1_ppg - c26$mean_h1_ppg
a26     <- a26 %>% mutate(change = outcome_ppg - h1_ppg)
chg26   <- mean(a26$change[a26$group == "treatment"]) -
           mean(a26$change[a26$group == "control"])
usage_gap <- t26$mean_early_usage - c26$mean_early_usage
cat(sprintf("\n  Weeks 1-4 usage gap (trt - ctl, signed):   %8.4f\n", usage_gap))
cat(sprintf("  Weeks 1-8 PPG gap (trt - ctl):             %8.4f\n", h1_gap))
cat(sprintf("  Weeks 9-18 PPG gap (trt - ctl):            %8.4f\n", out26_gap))
cat(sprintf("  Within-player change gap (trt - ctl):      %8.4f\n", chg26))

trt26 <- a26 %>% filter(group == "treatment")
ctl26 <- a26 %>% filter(group == "control")
ci_h1 <- boot_pct(trt26, ctl26, function(t, c) mean(t$h1_ppg) - mean(c$h1_ppg))
ci_c26 <- boot_pct(trt26, ctl26, function(t, c) mean(t$change) - mean(c$change))
cat(sprintf("  Weeks 1-8 gap CI:     [%8.4f, %8.4f]\n", ci_h1["ci_lower"], ci_h1["ci_upper"]))
cat(sprintf("  Change gap CI:        [%8.4f, %8.4f]\n", ci_c26["ci_lower"], ci_c26["ci_upper"]))

cat("\n3C. Stratified on weeks 1-8 PPG (DESCRIPTIVE ONLY: weeks 1-8 is the window\n",
    "    that defines the ramp, so this conditions on a concurrent variable)\n", sep = "")
breaks26 <- stats::quantile(a26$h1_ppg, probs = seq(0, 1, length.out = N_BINS + 1L),
                            names = FALSE)
gate(!anyDuplicated(breaks26), "weeks 1-8 PPG quintile breaks are distinct")
bins26 <- bin_table(a26, "h1_ppg", "outcome_ppg", breaks26)
print(as.data.frame(bins26), digits = 5)
ci_strat26 <- boot_pct(trt26, ctl26, function(t, c)
  strat_diff(t$h1_ppg, t$outcome_ppg, c$h1_ppg, c$outcome_ppg, breaks26))
cat(sprintf("\n  Stratified gap: %8.4f  [%8.4f, %8.4f]  NA draws: %d\n",
            ci_strat26["estimate"], ci_strat26["ci_lower"], ci_strat26["ci_upper"],
            as.integer(ci_strat26["n_na"])))

# ==============================================================================
# SECTION 4: CSV EXPORT
# ==============================================================================
banner("SECTION 4: CSV EXPORT")

summary_tbl <- dplyr::tibble(
  study    = c(rep("R25", 4), rep("R26", 4)),
  quantity = c("prior_season_gap", "outcome_gap", "change_gap", "stratified_gap",
               "early_usage_gap", "h1_ppg_gap", "outcome_gap", "change_gap"),
  estimate = c(prior_gap, out_gap, chg_gap, ci_strat25["estimate"],
               usage_gap, h1_gap, out26_gap, chg26),
  ci_lower = c(ci_prior["ci_lower"], e25$effect_ppg$ci_lower, ci_chg["ci_lower"],
               ci_strat25["ci_lower"], NA, ci_h1["ci_lower"],
               e26$effect_ppg$ci_lower, ci_c26["ci_lower"]),
  ci_upper = c(ci_prior["ci_upper"], e25$effect_ppg$ci_upper, ci_chg["ci_upper"],
               ci_strat25["ci_upper"], NA, ci_h1["ci_upper"],
               e26$effect_ppg$ci_upper, ci_c26["ci_upper"]),
  ci_method = c("percentile B2000", "stored basic B1000", "percentile B2000",
                "percentile B2000", "none", "percentile B2000",
                "stored basic B1000", "percentile B2000")
)

out_files <- list(
  stats_post42_r18_results.csv  = r18_out,
  stats_post42_r25_groups.csv   = s25,
  stats_post42_r25_strata.csv   = bins25,
  stats_post42_r25_trt_combo.csv = combo25,
  stats_post42_r26_groups.csv   = s26,
  stats_post42_r26_strata.csv   = bins26,
  stats_post42_summary.csv      = summary_tbl
)
if (!is.null(pos25)) out_files$stats_post42_r25_position.csv <- pos25
for (f in names(out_files)) {
  utils::write.csv(out_files[[f]], file.path(OUT_DIR, f), row.names = FALSE)
  cat("  wrote ", file.path(OUT_DIR, f), " (", nrow(out_files[[f]]), " rows)\n", sep = "")
}

# ==============================================================================
# SECTION 5: KEY FINDINGS (computed from this run)
# ==============================================================================
banner("SECTION 5: KEY FINDINGS (all values from this run)")

dir25 <- if (prior_gap < 0) "lower" else "higher"
dir26 <- if (h1_gap < 0) "lower" else "higher"
cat(glue(
  "R/18: season average r minus last-3 r = ",
  "{paste(r18_out$position, round(r18_out$r_diff_computed_here, 4), sep = ' ', collapse = ', ')}.\n",
  "R/25: treatment scored {round(abs(out_gap), 2)} PPG {if (out_gap < 0) 'less' else 'more'} ",
  "in weeks 9-18. The same players were already {round(abs(prior_gap), 2)} PPG {dir25} ",
  "the prior season ({round(100 * share_prior, 1)}% of the outcome gap). ",
  "Within-player change gap = {round(chg_gap, 2)} ",
  "[{round(ci_chg['ci_lower'], 2)}, {round(ci_chg['ci_upper'], 2)}]. ",
  "Stratified on prior PPG = {round(ci_strat25['estimate'], 2)} ",
  "[{round(ci_strat25['ci_lower'], 2)}, {round(ci_strat25['ci_upper'], 2)}].\n",
  "R/26: ramp group was {round(abs(h1_gap), 2)} PPG {dir26} in weeks 1-8 and ",
  "{round(abs(out26_gap), 2)} PPG {if (out26_gap < 0) 'lower' else 'higher'} in weeks 9-18. ",
  "Change gap = {round(chg26, 2)} [{round(ci_c26['ci_lower'], 2)}, {round(ci_c26['ci_upper'], 2)}].\n"
))
cat("\nAll gates passed. Paste the full console output back into the session.\n")
