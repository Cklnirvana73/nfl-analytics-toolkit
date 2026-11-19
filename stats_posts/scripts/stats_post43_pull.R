# ==============================================================================
# stats_post43_pull.R
# Statistics Series Post 43 (Confounding variables) -- verified data pull
#
# READ-ONLY against data/season2_cache/ (the 2026-09-17 rerun).
# Writes only stats_posts/output/data/stats_post43_*.csv. Edits no toolkit file.
# Run from the nfl-analytics-toolkit project root:  source("stats_post43_pull.R")
#
# Built on stats_post42_pull.R: same cache files, same group reconstruction,
# same reproduction gates, so every Post 43 number sits on the Post 42 sample.
#
# Sections
#   0  File inventory: cache listing, any stored balance objects, any cached
#      game-script / opponent / garbage-time outputs (listed, not gated)
#   1  R/25 rebuild + reproduction gates (115 / 1,829), join position and
#      n_prior_games from the groups file
#   2  R/25 balance: SMDs (prior-season PPG, n_prior_games, position shares)
#   3  Confounder condition 2: does each candidate predict scoring on its own?
#      Checked in the CONTROL group only (no week 3 miss)
#   4  R/25 adjusted gaps: raw, by prior band (reproduces Post 42), by
#      position, by position x prior band
#   5  R/26 rebuild + gates (1,085 / 630) and its balance SMDs
#   6  CSV export
#   7  Key findings, every number and direction computed from this run
#
# SMD convention (matches R/25 check_balance_injury_groups, lines 976-1002):
#   (mean_trt - mean_ctl) / sqrt((var_trt + var_ctl) / 2), signed.
# R/26 check_balance_ramp_groups reports abs() of the same quantity; both are
# printed for R/26. Binary shares use the standard proportion form
#   (p_trt - p_ctl) / sqrt((p_trt(1 - p_trt) + p_ctl(1 - p_ctl)) / 2),
# which is computed HERE, not by any toolkit function.
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
TOL               <- 1e-4
SEED              <- 42L
B_BOOT            <- 2000L
N_BINS            <- 5L
OUTCOME_WEEKS     <- 9L:18L
TRAINING_WEEKS    <- 1L:8L
MIN_OUTCOME_GAMES <- 3L
SMD_FLAG_R25      <- 0.20     # R/25 line 1007
SMD_FLAG_R26      <- 0.25     # R/26 line 1082
POSITIONS         <- c("QB", "RB", "WR", "TE")

# Stored values carried from stats_post42_pull.R (same cache)
EXP_R25 <- list(n_trt = 115L, n_ctl = 1829L,
                prior_trt = 8.292955, prior_ctl = 12.109796,
                out_trt = 7.654706,   out_ctl = 12.187294,
                effect_ppg = -4.53258786,
                strat_gap_2dp = -2.28)
EXP_POS25 <- dplyr::tribble(        # stats_post42_r25_position.csv (pct, 1 dp)
  ~group,      ~position, ~pct,
  "treatment", "QB",       1.7,
  "control",   "QB",      16.0,
  "treatment", "TE",      37.4,
  "control",   "TE",      16.7
)
EXP_R26 <- list(n_trt = 1085L, n_ctl = 630L,
                out_trt = 9.522617, out_ctl = 10.878057,
                effect_ppg = -1.35544061)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# HELPERS (banner, gate, near, need_cols, describe, boot_pct, strat_diff
# copied from stats_post42_pull.R unchanged)
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

no_suffix_cols <- function(df, label) {
  bad <- grep("\\.(x|y)$", names(df), value = TRUE)
  gate(length(bad) == 0L,
       glue("{label}: no .x/.y columns after join ({paste(bad, collapse = ', ')})"))
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

# Signed SMD, pooled-variance denominator (R/25 convention)
smd_cont <- function(x_trt, x_ctl) {
  x_trt <- x_trt[!is.na(x_trt)]
  x_ctl <- x_ctl[!is.na(x_ctl)]
  if (length(x_trt) < 2L || length(x_ctl) < 2L) return(NA_real_)
  pooled_sd <- sqrt((stats::var(x_trt) + stats::var(x_ctl)) / 2)
  if (pooled_sd == 0) return(0)
  (mean(x_trt) - mean(x_ctl)) / pooled_sd
}

# Signed SMD for a binary share (computed here, not a toolkit function)
smd_bin <- function(p_trt, p_ctl) {
  denom <- sqrt((p_trt * (1 - p_trt) + p_ctl * (1 - p_ctl)) / 2)
  if (is.na(denom) || denom == 0) return(NA_real_)
  (p_trt - p_ctl) / denom
}

strat_diff <- function(trt_x, trt_y, ctl_x, ctl_y, breaks) {
  b_t <- cut(trt_x, breaks, include.lowest = TRUE)
  b_c <- cut(ctl_x, breaks, include.lowest = TRUE)
  lv  <- levels(b_t)
  num <- 0; den <- 0
  for (l in lv) {
    nt <- sum(b_t == l, na.rm = TRUE)
    if (nt == 0L) next
    nc <- sum(b_c == l, na.rm = TRUE)
    if (nc == 0L) return(NA_real_)
    d   <- mean(trt_y[b_t == l & !is.na(b_t)]) - mean(ctl_y[b_c == l & !is.na(b_c)])
    num <- num + nt * d
    den <- den + nt
  }
  if (den == 0) NA_real_ else num / den
}

# Same weighting as strat_diff, but strata are given as a categorical key
# (position, or position x prior band). Weighted by treatment n per stratum.
strat_diff_cat <- function(trt_key, trt_y, ctl_key, ctl_y) {
  num <- 0; den <- 0
  for (k in unique(trt_key)) {
    nt <- sum(trt_key == k)
    nc <- sum(ctl_key == k)
    if (nc == 0L) return(NA_real_)
    d   <- mean(trt_y[trt_key == k]) - mean(ctl_y[ctl_key == k])
    num <- num + nt * d
    den <- den + nt
  }
  if (den == 0) NA_real_ else num / den
}

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

# ==============================================================================
# SECTION 0: FILE INVENTORY
# ==============================================================================
banner("SECTION 0: FILE INVENTORY")

files <- c(
  r25_groups   = "s2_week11_groups.rds",
  r25_effects  = "s2_week11_effects.rds",
  r26_groups   = "s2_week12_groups.rds",
  r26_effects  = "s2_week12_effects.rds",
  r26_fantasy  = "s2_week12_weekly_fantasy.rds"
)
paths <- file.path(CACHE_DIR, files)
names(paths) <- names(files)

gate(all(file.exists(paths)), "all five cache files exist")
inv <- file.info(unname(paths))
for (i in seq_along(paths)) {
  cat(sprintf("  %-12s %10s B  %s  %s\n", names(paths)[i],
              format(inv$size[i], big.mark = ","),
              format(inv$mtime[i], "%Y-%m-%d %H:%M"), files[[i]]))
}
stale <- names(paths)[as.Date(inv$mtime) < as.Date("2026-09-17")]
gate(length(stale) == 0L,
     "no file predates the 2026-09-17 rerun (pre-rerun results are invalidated)")

cat("\n0B. Full season2_cache listing (week 11 and week 12 files)\n")
cache_all <- list.files(CACHE_DIR)
print(grep("week1[12]", cache_all, value = TRUE))

cat("\n0C. Stored balance objects in season2_cache (printed if present, not gated)\n")
bal_files <- grep("balance", cache_all, value = TRUE, ignore.case = TRUE)
if (length(bal_files) == 0L) {
  cat("  none found; SMDs below are recomputed with the R/25 / R/26 formulas.\n")
} else {
  for (bf in bal_files) {
    obj <- readRDS(file.path(CACHE_DIR, bf))
    describe(obj, bf)
    for (el in c("smd_table", "usage_smd", "balance_flags", "balance_summary",
                 "summary", "position_table")) {
      if (is.list(obj) && !is.null(obj[[el]])) {
        cat("  $", el, ":\n", sep = "")
        print(obj[[el]])
      }
    }
  }
}

cat("\n0D. Cached outputs that may hold game-script, opponent or garbage-time\n",
    "    results (names only, for the next pull; nothing here is used below)\n", sep = "")
adj_hits <- c(
  list.files(here::here("data"),   pattern = "script|opp|adjust|garbage|leverage",
             recursive = TRUE, ignore.case = TRUE),
  list.files(here::here("output"), pattern = "script|opp|adjust|garbage|leverage",
             recursive = TRUE, ignore.case = TRUE)
)
if (length(adj_hits) == 0L) cat("  none found\n") else print(adj_hits)

g25  <- readRDS(paths[["r25_groups"]])
e25  <- readRDS(paths[["r25_effects"]])
g26  <- readRDS(paths[["r26_groups"]])
e26  <- readRDS(paths[["r26_effects"]])
wf26 <- readRDS(paths[["r26_fantasy"]])

describe(g25,  "r25_groups")
describe(e25,  "r25_effects")
describe(g26,  "r26_groups")
describe(e26,  "r26_effects")

# ==============================================================================
# SECTION 1: R/25 REBUILD
# ==============================================================================
banner("SECTION 1: R/25 REBUILD AND REPRODUCTION GATES")

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

# Pull only the keys and the two covariates from the groups file before the
# join, so no non-key column can collide (Post 42 post-mortem #1).
need_cols(g25, c("player_id", "season", "group", "position", "n_prior_games"),
          "r25_groups")
g25_keys <- g25 %>%
  filter(group %in% c("treatment", "control")) %>%
  distinct(player_id, season, position, n_prior_games)
gate(!anyDuplicated(g25_keys[, c("player_id", "season")]),
     "groups file has one position and one n_prior_games per player-season")

a25 <- pl %>%
  select(player_id, season, group, prior_ppg, outcome_ppg) %>%
  left_join(g25_keys, by = c("player_id", "season"), relationship = "many-to-one")
no_suffix_cols(a25, "a25")
gate(nrow(a25) == nrow(pl), "join kept every effect-sample player-season")
gate(!anyNA(a25$position) && !anyNA(a25$n_prior_games),
     "every effect-sample player-season has position and n_prior_games")
gate(all(a25$position %in% POSITIONS), "positions are QB / RB / WR / TE only")

s25 <- a25 %>%
  group_by(group) %>%
  summarise(n = n(),
            mean_prior_ppg   = mean(prior_ppg),
            mean_outcome_ppg = mean(outcome_ppg),
            .groups = "drop")
trt <- s25[s25$group == "treatment", ]
ctl <- s25[s25$group == "control", ]

gate(trt$n == EXP_R25$n_trt && ctl$n == EXP_R25$n_ctl,
     glue("effect sample n = {trt$n} treatment / {ctl$n} control (Post 42)"))
gate(near(trt$mean_prior_ppg, EXP_R25$prior_trt) &&
       near(ctl$mean_prior_ppg, EXP_R25$prior_ctl),
     "prior-season PPG means reproduce Post 42")
gate(near(trt$mean_outcome_ppg, EXP_R25$out_trt) &&
       near(ctl$mean_outcome_ppg, EXP_R25$out_ctl),
     "weeks 9-18 PPG means reproduce Post 42")
out_gap <- trt$mean_outcome_ppg - ctl$mean_outcome_ppg
gate(near(out_gap, e25$effect_ppg$estimate),
     glue("outcome gap {round(out_gap, 4)} equals stored effect_ppg estimate"))

pos25 <- a25 %>%
  count(group, position) %>%
  group_by(group) %>%
  mutate(share = n / sum(n), pct = round(100 * share, 1)) %>%
  ungroup()
pos_chk <- EXP_POS25 %>%
  left_join(pos25 %>% select(group, position, pct_now = pct),
            by = c("group", "position"), relationship = "one-to-one")
gate(all(!is.na(pos_chk$pct_now)) && all(abs(pos_chk$pct_now - pos_chk$pct) < 0.05),
     "QB and TE shares reproduce Post 42 (1.7 / 16.0, 37.4 / 16.7)")
cat("\n")
print(as.data.frame(pos25), digits = 6)

# ==============================================================================
# SECTION 2: R/25 BALANCE (SMD)
# ==============================================================================
banner("SECTION 2: R/25 BALANCE (STANDARDIZED MEAN DIFFERENCES)")

t25 <- a25 %>% filter(group == "treatment")
c25 <- a25 %>% filter(group == "control")

share_of <- function(df, p) mean(df$position == p)

smd25 <- dplyr::bind_rows(
  dplyr::tibble(
    covariate = c("prior_season_ppg", "n_prior_games"),
    type      = "continuous",
    mean_trt  = c(mean(t25$prior_ppg), mean(t25$n_prior_games)),
    mean_ctl  = c(mean(c25$prior_ppg), mean(c25$n_prior_games)),
    sd_trt    = c(sd(t25$prior_ppg),   sd(t25$n_prior_games)),
    sd_ctl    = c(sd(c25$prior_ppg),   sd(c25$n_prior_games)),
    smd       = c(smd_cont(t25$prior_ppg, c25$prior_ppg),
                  smd_cont(t25$n_prior_games, c25$n_prior_games)),
    source    = "R/25 formula, effect sample"
  ),
  dplyr::tibble(
    covariate = paste0("share_", POSITIONS),
    type      = "binary",
    mean_trt  = vapply(POSITIONS, function(p) share_of(t25, p), numeric(1)),
    mean_ctl  = vapply(POSITIONS, function(p) share_of(c25, p), numeric(1)),
    sd_trt    = NA_real_,
    sd_ctl    = NA_real_,
    smd       = vapply(POSITIONS, function(p)
                  smd_bin(share_of(t25, p), share_of(c25, p)), numeric(1)),
    source    = "proportion SMD, computed here"
  )
) %>%
  mutate(abs_smd = abs(smd), flag_0.20 = abs_smd > SMD_FLAG_R25)

print(as.data.frame(smd25), digits = 4)
gate(near(smd25$mean_trt[1] - smd25$mean_ctl[1],
          EXP_R25$prior_trt - EXP_R25$prior_ctl),
     "prior-season PPG mean difference in the SMD table matches Post 42")

cat("\n2B. Groups-file sample (every treatment/control row, before the outcome\n",
    "    filter that produces the 115 / 1,829 effect sample)\n", sep = "")
g25_tc <- g25 %>% filter(group %in% c("treatment", "control"))
print(table(g25_tc$group))
smd_nprior_groups <- smd_cont(g25_tc$n_prior_games[g25_tc$group == "treatment"],
                              g25_tc$n_prior_games[g25_tc$group == "control"])
cat(sprintf("  n_prior_games SMD on the groups file: %8.4f\n", smd_nprior_groups))

# ==============================================================================
# SECTION 3: CONFOUNDER CONDITION 2 (CONTROL GROUP ONLY)
# ==============================================================================
banner("SECTION 3: DOES EACH CANDIDATE PREDICT SCORING ON ITS OWN? (CONTROL ONLY)")
cat("  Control = no week 3 miss, so week 3 cannot produce these relationships.\n")

ct <- stats::cor.test(c25$prior_ppg, c25$outcome_ppg, method = "pearson")
fit <- stats::lm(outcome_ppg ~ prior_ppg, data = c25)
cat(sprintf("\n3A. Prior-season PPG vs weeks 9-18 PPG, control n = %d\n", nrow(c25)))
cat(sprintf("  Pearson r = %.4f  95%% CI [%.4f, %.4f]\n",
            unname(ct$estimate), ct$conf.int[1], ct$conf.int[2]))
cat(sprintf("  Slope = %.4f weeks 9-18 PPG per 1 prior PPG (intercept %.4f)\n",
            unname(coef(fit)[2]), unname(coef(fit)[1])))

cat("\n3B. Weeks 9-18 PPG by position, control group\n")
pos_out_ctl <- c25 %>%
  group_by(position) %>%
  summarise(n = n(),
            mean_prior_ppg   = mean(prior_ppg),
            mean_outcome_ppg = mean(outcome_ppg),
            sd_outcome_ppg   = sd(outcome_ppg),
            .groups = "drop") %>%
  arrange(desc(mean_outcome_ppg))
print(as.data.frame(pos_out_ctl), digits = 5)
gate(sum(pos_out_ctl$n) == nrow(c25), "position table covers every control player")

cat("\n3C. Same table, treatment group (small cells; descriptive only)\n")
pos_out_trt <- t25 %>%
  group_by(position) %>%
  summarise(n = n(),
            mean_prior_ppg   = mean(prior_ppg),
            mean_outcome_ppg = mean(outcome_ppg),
            .groups = "drop") %>%
  arrange(desc(mean_outcome_ppg))
print(as.data.frame(pos_out_trt), digits = 5)

aov_pos <- stats::anova(stats::lm(outcome_ppg ~ position, data = c25))
eta_sq  <- aov_pos[["Sum Sq"]][1] / sum(aov_pos[["Sum Sq"]])
cat(sprintf("\n  Share of control weeks 9-18 PPG variance explained by position (eta^2): %.4f\n",
            eta_sq))

# ==============================================================================
# SECTION 4: R/25 ADJUSTED GAPS
# ==============================================================================
banner("SECTION 4: R/25 GAP BEFORE AND AFTER COMPARING LIKE WITH LIKE")

breaks25 <- stats::quantile(a25$prior_ppg, probs = seq(0, 1, length.out = N_BINS + 1L),
                            names = FALSE)
gate(!anyDuplicated(breaks25), "prior-season PPG quintile breaks are distinct")
a25 <- a25 %>%
  mutate(prior_band = cut(prior_ppg, breaks25, include.lowest = TRUE),
         pos_band   = paste(position, prior_band, sep = " | "))
t25 <- a25 %>% filter(group == "treatment")
c25 <- a25 %>% filter(group == "control")

ci_band <- boot_pct(t25, c25, function(t, c)
  strat_diff(t$prior_ppg, t$outcome_ppg, c$prior_ppg, c$outcome_ppg, breaks25))
gate(abs(ci_band["estimate"] - EXP_R25$strat_gap_2dp) < 0.005,
     glue("prior-band gap {round(ci_band['estimate'], 4)} reproduces Post 42 (-2.28)"))

ci_pos <- boot_pct(t25, c25, function(t, c)
  strat_diff_cat(t$position, t$outcome_ppg, c$position, c$outcome_ppg))
ci_both <- boot_pct(t25, c25, function(t, c)
  strat_diff_cat(t$pos_band, t$outcome_ppg, c$pos_band, c$outcome_ppg))

cell_tbl <- a25 %>%
  group_by(pos_band) %>%
  summarise(n_trt = sum(group == "treatment"),
            n_ctl = sum(group == "control"),
            .groups = "drop") %>%
  filter(n_trt > 0L)
empty_cells <- cell_tbl %>% filter(n_ctl == 0L)
cat("\n4A. Position x prior-band cells holding treatment players\n")
print(as.data.frame(cell_tbl))
gate(nrow(empty_cells) == 0L,
     "every position x prior-band cell with a treatment player has controls")

adj25 <- dplyr::tibble(
  comparison = c("raw (no adjustment)",
                 "within prior-season PPG quintile",
                 "within position",
                 "within position x prior-season PPG quintile"),
  estimate   = c(out_gap, ci_band["estimate"], ci_pos["estimate"], ci_both["estimate"]),
  ci_lower   = c(e25$effect_ppg$ci_lower, ci_band["ci_lower"],
                 ci_pos["ci_lower"], ci_both["ci_lower"]),
  ci_upper   = c(e25$effect_ppg$ci_upper, ci_band["ci_upper"],
                 ci_pos["ci_upper"], ci_both["ci_upper"]),
  n_na_draws = c(NA, ci_band["n_na"], ci_pos["n_na"], ci_both["n_na"]),
  ci_method  = c("stored basic B1000", rep("percentile B2000", 3))
) %>%
  mutate(share_of_raw_remaining = estimate / out_gap)
cat("\n4B. Gap (treatment - control, weeks 9-18 PPG), weighted by treatment n\n")
print(as.data.frame(adj25), digits = 4)

# ==============================================================================
# SECTION 5: R/26 REBUILD AND BALANCE
# ==============================================================================
banner("SECTION 5: R/26 USAGE RAMP REBUILD AND BALANCE")

need_cols(g26, c("player_id", "season", "group", "early_avg", "late_avg"), "r26_groups")
need_cols(e26, c("group_summary", "effect_ppg"), "r26_effects")
need_cols(wf26, c("player_id", "season", "week", "total_fantasy_points"),
          "r26_weekly_fantasy")
gate(sum(is.na(wf26$total_fantasy_points)) == 0L, "no NA fantasy points in R/26 weekly file")
gate(sum(duplicated(wf26[, c("player_id", "season", "week")])) == 0L,
     "weekly fantasy is unique on player_id + season + week")

outcome26 <- wf26 %>%
  filter(week %in% OUTCOME_WEEKS) %>%
  group_by(player_id, season) %>%
  summarise(outcome_ppg = mean(total_fantasy_points), n_out = n(), .groups = "drop") %>%
  filter(n_out >= MIN_OUTCOME_GAMES)
first26 <- wf26 %>%
  filter(week %in% TRAINING_WEEKS) %>%
  group_by(player_id, season) %>%
  summarise(h1_ppg = mean(total_fantasy_points), .groups = "drop")

has_pos26 <- "position" %in% names(g26)
g26_tc <- g26 %>%
  filter(group %in% c("treatment", "control")) %>%
  select(any_of(c("player_id", "season", "group", "early_avg", "late_avg", "position")))
gate(!anyDuplicated(g26_tc[, c("player_id", "season")]),
     "R/26 treatment/control rows are unique on player_id + season")

a26 <- g26_tc %>%
  inner_join(outcome26, by = c("player_id", "season"), relationship = "one-to-one") %>%
  left_join(first26,   by = c("player_id", "season"), relationship = "one-to-one")
no_suffix_cols(a26, "a26")

t26 <- a26 %>% filter(group == "treatment")
c26 <- a26 %>% filter(group == "control")
gate(nrow(t26) == EXP_R26$n_trt && nrow(c26) == EXP_R26$n_ctl,
     glue("effect sample n = {nrow(t26)} treatment / {nrow(c26)} control (Post 42)"))
gate(near(mean(t26$outcome_ppg), EXP_R26$out_trt) &&
       near(mean(c26$outcome_ppg), EXP_R26$out_ctl),
     "weeks 9-18 PPG means reproduce Post 42")
out26_gap <- mean(t26$outcome_ppg) - mean(c26$outcome_ppg)
gate(near(out26_gap, e26$effect_ppg$estimate),
     glue("outcome gap {round(out26_gap, 4)} equals stored effect_ppg estimate"))
gate(!anyNA(a26$h1_ppg), "every R/26 analysis player-season has weeks 1-8 games")

smd26 <- dplyr::tibble(
  covariate = c("early_usage (weeks 1-4)", "weeks 1-8 PPG"),
  sample    = "effect sample",
  mean_trt  = c(mean(t26$early_avg), mean(t26$h1_ppg)),
  mean_ctl  = c(mean(c26$early_avg), mean(c26$h1_ppg)),
  smd       = c(smd_cont(t26$early_avg, c26$early_avg),
                smd_cont(t26$h1_ppg,    c26$h1_ppg)),
  note      = c("R/26 balance covariate",
                "DESCRIPTIVE ONLY: concurrent with the ramp window")
)
smd26_groups <- smd_cont(g26_tc$early_avg[g26_tc$group == "treatment"],
                         g26_tc$early_avg[g26_tc$group == "control"])
smd26 <- dplyr::bind_rows(
  smd26,
  dplyr::tibble(covariate = "early_usage (weeks 1-4)", sample = "groups file",
                mean_trt = mean(g26_tc$early_avg[g26_tc$group == "treatment"], na.rm = TRUE),
                mean_ctl = mean(g26_tc$early_avg[g26_tc$group == "control"],   na.rm = TRUE),
                smd = smd26_groups,
                note = "the sample check_balance_ramp_groups() sees")
) %>%
  mutate(abs_smd = abs(smd), flag_0.25 = abs_smd > SMD_FLAG_R26)
cat("\n5A. R/26 SMDs (R/26 itself reports abs(SMD) and flags above 0.25)\n")
print(as.data.frame(smd26), digits = 4)

pos26 <- NULL
if (has_pos26) {
  pos26 <- a26 %>%
    count(group, position) %>%
    group_by(group) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup()
  cat("\n5B. R/26 position mix, effect sample\n")
  print(as.data.frame(pos26))
} else {
  cat("\n5B. No position column in the R/26 groups file; position mix skipped.\n")
}

# ==============================================================================
# SECTION 6: CSV EXPORT
# ==============================================================================
banner("SECTION 6: CSV EXPORT")

out_files <- list(
  stats_post43_r25_smd.csv          = smd25,
  stats_post43_r25_position.csv     = pos25,
  stats_post43_r25_pos_outcome.csv  = dplyr::bind_rows(
                                        pos_out_ctl %>% mutate(group = "control"),
                                        pos_out_trt %>% mutate(group = "treatment")),
  stats_post43_r25_cells.csv        = cell_tbl,
  stats_post43_r25_adjusted.csv     = adj25,
  stats_post43_r26_smd.csv          = smd26,
  stats_post43_ctl_prior_cor.csv    = dplyr::tibble(
                                        n = nrow(c25),
                                        r = unname(ct$estimate),
                                        r_ci_lower = ct$conf.int[1],
                                        r_ci_upper = ct$conf.int[2],
                                        slope = unname(coef(fit)[2]),
                                        eta_sq_position = eta_sq)
)
if (!is.null(pos26)) out_files$stats_post43_r26_position.csv <- pos26
for (f in names(out_files)) {
  utils::write.csv(out_files[[f]], file.path(OUT_DIR, f), row.names = FALSE)
  cat("  wrote ", file.path(OUT_DIR, f), " (", nrow(out_files[[f]]), " rows)\n", sep = "")
}

# ==============================================================================
# SECTION 7: KEY FINDINGS (computed from this run)
# ==============================================================================
banner("SECTION 7: KEY FINDINGS (all values from this run)")

n_flag25  <- sum(smd25$flag_0.20, na.rm = TRUE)
top_pos   <- pos_out_ctl$position[1]
bot_pos   <- pos_out_ctl$position[nrow(pos_out_ctl)]
cat(glue(
  "R/25 balance: {n_flag25} of {nrow(smd25)} covariates exceed |SMD| 0.20. ",
  "Prior-season PPG SMD = {round(smd25$smd[1], 3)}; ",
  "TE share SMD = {round(smd25$smd[smd25$covariate == 'share_TE'], 3)}; ",
  "QB share SMD = {round(smd25$smd[smd25$covariate == 'share_QB'], 3)}.\n",
  "Condition 2 (control only): prior vs later PPG r = {round(ct$estimate, 3)} ",
  "[{round(ct$conf.int[1], 3)}, {round(ct$conf.int[2], 3)}]; ",
  "highest-scoring position {top_pos} ",
  "({round(pos_out_ctl$mean_outcome_ppg[1], 2)} PPG), lowest {bot_pos} ",
  "({round(pos_out_ctl$mean_outcome_ppg[nrow(pos_out_ctl)], 2)} PPG).\n",
  "Gap: raw {round(adj25$estimate[1], 2)}; within prior band {round(adj25$estimate[2], 2)}; ",
  "within position {round(adj25$estimate[3], 2)} ",
  "[{round(adj25$ci_lower[3], 2)}, {round(adj25$ci_upper[3], 2)}]; ",
  "within both {round(adj25$estimate[4], 2)} ",
  "[{round(adj25$ci_lower[4], 2)}, {round(adj25$ci_upper[4], 2)}].\n",
  "R/26: early-usage SMD = {round(smd26$smd[1], 3)} (effect sample), ",
  "{round(smd26_groups, 3)} (groups file).\n"
))
cat("\nAll gates passed. Paste the full console output back into the session.\n")
