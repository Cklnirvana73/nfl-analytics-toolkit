# ==============================================================================
# stats_post28_pull_v2.R
#
# Statistics Series, Post 28: one-sample t-test.
#
# PURPOSE: run the project's own EPA-centered check and a one-sample t-test
# on THE SAME ROWS, so the two can be compared honestly.
#
# The filter block below is a line-for-line mirror of validate_epa_distribution()
# at 15_multi_season_pbp.R lines 665-675. Do not "improve" it. Any change breaks
# the comparison. Notably it includes postseason plays and applies garbage-time
# exclusion across all quarters, because that is what the source does.
#
# Tolerance 0.05 is the function's default argument (line 638), never overridden
# at the only call site (line 849).
#
# Reference value mu = 0 is fixed by construction: EPA is a change in expected
# points, so zero is the break-even play.
#
# Run this and return the ENTIRE console output.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(nflreadr)
})

MAIN_SEASON   <- 2025
SEASON_RANGE  <- 2010:2025    # SEASON_RANGE_DEFAULT, 15_multi_season_pbp.R line 48
EPA_TOLERANCE <- 0.05         # epa_tolerance default,   line 638
ALPHA         <- 0.05

cat("\n")
cat(strrep("=", 78), "\n")
cat("POST 28 PULL v2 :: project tolerance check vs one-sample t-test\n")
cat("filters mirror validate_epa_distribution() lines 665-675\n")
cat("run at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
cat(strrep("=", 78), "\n\n")

# ---------------------------------------------------------------- 1. load ----
# normalize_schema() does not subset rows (its roxygen says so at lines 297-298),
# so load_pbp() output is row-equivalent to the cached pbp_normalized_{s}.rds.

pbp_raw <- nflreadr::load_pbp(SEASON_RANGE)

cat("Raw rows loaded:", format(nrow(pbp_raw), big.mark = ","), "\n")
cat("Seasons present:", paste(range(pbp_raw$season), collapse = " to "), "\n")
cat("season_type values present:",
    paste(sort(unique(pbp_raw$season_type)), collapse = ", "), "\n\n")

required_cols <- c("season", "season_type", "game_id", "posteam", "play_type",
                   "epa", "wp", "down", "desc", "two_point_attempt")
missing_cols <- setdiff(required_cols, names(pbp_raw))
stopifnot("Required columns missing" = length(missing_cols) == 0)
cat("Column check: all", length(required_cols), "required columns present.\n\n")

# ------------------------------------------- 2. mirrored filter block --------
# Mirror of 15_multi_season_pbp.R:665-675. Six predicates, same order.

clean_as_source <- function(df) {
  df %>%
    filter(
      !is.na(epa),
      !is.na(posteam),
      play_type %in% c("pass", "run"),
      !(down >= 1 & !is.na(wp) & (wp < 0.10 | wp > 0.90)),
      !grepl("kneel|spike", tolower(desc), fixed = FALSE),
      is.na(two_point_attempt) | two_point_attempt != 1
    )
}

pbp <- clean_as_source(pbp_raw)

cat("--- FILTER LADDER, mirrored block, all seasons ---\n")
step <- function(lbl, n) cat(sprintf("  %-48s %10s\n", lbl, format(n, big.mark = ",")))
f1 <- filter(pbp_raw, !is.na(epa), !is.na(posteam))
f2 <- filter(f1, play_type %in% c("pass", "run"))
f3 <- filter(f2, !(down >= 1 & !is.na(wp) & (wp < 0.10 | wp > 0.90)))
f4 <- filter(f3, !grepl("kneel|spike", tolower(desc), fixed = FALSE))
step("raw rows", nrow(pbp_raw))
step("epa and posteam non-missing", nrow(f1))
step("+ play_type in pass/run", nrow(f2))
step("+ garbage time removed (all quarters)", nrow(f3))
step("+ kneel/spike removed by desc regex", nrow(f4))
step("+ two-point attempts removed", nrow(pbp))
cat("\n")

cat("--- POSTSEASON CHECK (source applies no season_type filter) ---\n")
pt <- pbp %>% count(season_type)
print(as.data.frame(pt), row.names = FALSE)
cat(sprintf("  postseason share of the analysis set: %.4f\n",
            sum(pbp$season_type != "REG") / nrow(pbp)))
cat("\n")

# ------------------------------- 3. both checks, every season ----------------

season_tbl <- pbp %>%
  group_by(season) %>%
  summarise(
    n_plays  = n(),
    mean_epa = mean(epa, na.rm = TRUE),
    sd_epa   = sd(epa, na.rm = TRUE),
    med_epa  = median(epa, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    # project's check, exactly as source computes it (line 687)
    within_tolerance = abs(mean_epa) < EPA_TOLERANCE,
    # one-sample t-test against mu = 0
    se       = sd_epa / sqrt(n_plays),
    t_stat   = mean_epa / se,
    df       = n_plays - 1,
    p_value  = 2 * pt(-abs(t_stat), df = df),
    ci_lo    = mean_epa - qt(1 - ALPHA / 2, df) * se,
    ci_hi    = mean_epa + qt(1 - ALPHA / 2, df) * se,
    reject   = p_value < ALPHA,
    disagree = within_tolerance != (!reject)
  )

cat("--- PROJECT CHECK vs t-TEST, EVERY SEASON ---\n")
print(as.data.frame(season_tbl %>%
  select(season, n_plays, mean_epa, sd_epa, within_tolerance,
         t_stat, p_value, ci_lo, ci_hi, reject, disagree)),
  digits = 6, row.names = FALSE)
cat("\n")

cat("--- AGREEMENT SUMMARY ---\n")
cat(sprintf("  seasons tested                        : %d\n", nrow(season_tbl)))
cat(sprintf("  pass project tolerance (|mean| < %.2f) : %d\n",
            EPA_TOLERANCE, sum(season_tbl$within_tolerance)))
cat(sprintf("  rejected by t-test at alpha = %.2f     : %d\n",
            ALPHA, sum(season_tbl$reject)))
cat(sprintf("  the two checks disagree on            : %d season(s)\n",
            sum(season_tbl$disagree)))
if (any(season_tbl$disagree)) {
  cat("  disagreement seasons:\n")
  print(as.data.frame(season_tbl %>%
    filter(disagree) %>%
    select(season, n_plays, mean_epa, within_tolerance, p_value, ci_lo, ci_hi)),
    digits = 6, row.names = FALSE)
}
cat("\n")

# --------------------------------------------- 4. how wide is 0.05 -----------
# The tolerance is a fixed distance. The CI half-width is what the data can
# actually resolve. Reporting the ratio makes the difference concrete.

cat("--- FIXED TOLERANCE vs WHAT THE DATA RESOLVES ---\n")
ratio_tbl <- season_tbl %>%
  mutate(ci_half = qt(1 - ALPHA / 2, df) * se,
         tol_over_ci = EPA_TOLERANCE / ci_half) %>%
  select(season, n_plays, ci_half, tol_over_ci)
print(as.data.frame(ratio_tbl), digits = 6, row.names = FALSE)
cat("\n")

# ------------------------------------------------ 5. 2025 focus block --------

main <- pbp %>% filter(season == MAIN_SEASON)
stopifnot("No rows for MAIN_SEASON" = nrow(main) > 0)

x  <- main$epa
n  <- length(x)
m  <- mean(x)
s  <- sd(x)
se <- s / sqrt(n)
tt <- t.test(x, mu = 0)

cat("--- 2025 FOCUS ---\n")
cat(sprintf("  n plays                 : %s\n", format(n, big.mark = ",")))
cat(sprintf("  games                   : %d\n", n_distinct(main$game_id)))
cat(sprintf("  teams                   : %d\n", n_distinct(main$posteam)))
cat(sprintf("  mean EPA per play       : %.6f\n", m))
cat(sprintf("  rounded as source does  : %.4f\n", round(m, 4)))
cat(sprintf("  sd                      : %.6f\n", s))
cat(sprintf("  median                  : %.6f\n", median(x)))
cat(sprintf("  share of plays EPA > 0  : %.4f\n", mean(x > 0)))
cat(sprintf("  standard error          : %.6f\n", se))
cat(sprintf("  t                       : %.4f\n", unname(tt$statistic)))
cat(sprintf("  df                      : %.0f\n", unname(tt$parameter)))
cat(sprintf("  p-value                 : %.6g\n", tt$p.value))
cat(sprintf("  95%% CI                  : [%.6f, %.6f]\n",
            tt$conf.int[1], tt$conf.int[2]))
cat(sprintf("  project check passes    : %s\n", abs(m) < EPA_TOLERANCE))
cat(sprintf("  t-test rejects          : %s\n", tt$p.value < ALPHA))
cat("\n")

ci_half <- qt(1 - ALPHA / 2, n - 1) * se
ppg <- n / (n_distinct(main$game_id) * 2)
cat("--- 2025 SCALED TO FOOTBALL ---\n")
cat(sprintf("  plays per team-game            : %.2f\n", ppg))
cat(sprintf("  observed mean per team-game    : %.4f points\n", m * ppg))
cat(sprintf("  CI half-width per team-game    : %.4f points\n", ci_half * ppg))
cat(sprintf("  CI upper bound per team-game   : %.4f points\n", tt$conf.int[2] * ppg))
cat(sprintf("  tolerance 0.05 per team-game   : %.4f points\n", EPA_TOLERANCE * ppg))
cat(sprintf("  observed mean over 17 games    : %.4f points\n", m * ppg * 17))
cat(sprintf("  tolerance 0.05 over 17 games   : %.4f points\n",
            EPA_TOLERANCE * ppg * 17))
cat("\n")

# ------------------------------------------------------------ 6. asserts -----

stopifnot(
  "season count is not 16" = nrow(season_tbl) == 16,
  "2025 team count is not 32" = n_distinct(main$posteam) == 32,
  "2025 mean outside plausible range" = abs(m) < 0.25,
  "tolerance is not 0.05" = EPA_TOLERANCE == 0.05,
  "postseason rows were dropped; they should be retained" =
    any(pbp$season_type != "REG")
)
cat("All assertions passed.\n\n")

cat(strrep("=", 78), "\n")
cat("END OF PULL v2\n")
cat(strrep("=", 78), "\n")
