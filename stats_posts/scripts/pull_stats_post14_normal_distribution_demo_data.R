# ==============================================================================
# pull_stats_post14_normal_distribution_demo_data.R
# ==============================================================================
#
# PURPOSE
#   Statistics Series Post 14 (The Normal Distribution and Z-Scores).
#   Captures the R/23 Shapiro-Wilk normality result on year-over-year PPG
#   deltas, ages 24-30, per position group.
#
#   validate_aging_assumptions() computes these p-values and emits them only
#   through message(); normality_results is never returned and never written
#   to disk. This script replicates check 5 exactly as written in R/23 so the
#   p-values are captured as citable values.
#
# WHAT THIS RUNS
#   Step 1  build_player_season_panel()   (R/16)
#   Step 2  compute_player_ages()         (R/23)
#   Step 3  compute_season_ppg()          (R/23, legacy path)
#   Step 3b split_wr_te_positions()       (R/23)
#   Step 4  compute_age_deltas()          (R/23)
#   Step 7  validate_aging_assumptions()  (R/23, ngs_panel = NULL)
#   Then    independent replication of check 5 to expose shapiro_p
#
# WHAT THIS SKIPS
#   Step 5 / 6  NGS panel load and NGS deltas. check 5 does not consume them,
#               and ngs_panel = NULL cleanly skips the NGS coverage check.
#   Step 8+     Curve fitting, plotting, OOS gate. Not needed.
#
# WHAT THIS MODIFIES
#   Nothing. Read-only against the cache. No files written, no scripts edited.
#
# HOW TO RUN
#   From the project root:  source("stats_posts/scripts/pull_stats_post14_normal_distribution_demo_data.R")
#
# ==============================================================================

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

R16_PATH <- here::here("R", "16_player_season_panel.R")
R23_PATH <- here::here("R", "23_aging_curves.R")

# Mirrors validate_aging_assumptions() check 5 exactly
NORMALITY_AGE_MIN <- 24L
NORMALITY_AGE_MAX <- 30L
SHAPIRO_MIN_N     <- 8L
SHAPIRO_MAX_N     <- 5000L
ALPHA             <- 0.05

# ------------------------------------------------------------------------------
# SOURCE
# ------------------------------------------------------------------------------
# R/16 first, then R/23, so R/23's constants (PANEL_SEASONS, CACHE_DIR,
# CURVE_POSITIONS, AGE_MIN, AGE_MAX, MIN_CAREER_SEASONS) are authoritative.
# R/23 carries the library() calls for nflreadr, dplyr, purrr, tidyr,
# ggplot2, glue, here.

stopifnot(file.exists(R16_PATH), file.exists(R23_PATH))

message("Sourcing R/16_player_season_panel.R ...")
source(R16_PATH)

message("Sourcing R/23_aging_curves.R ...")
source(R23_PATH)

message(glue::glue("\nSEASON range from R/23: PANEL_SEASONS = ",
                   "{min(PANEL_SEASONS)}:{max(PANEL_SEASONS)}"))
message(glue::glue("CURVE_POSITIONS = {paste(CURVE_POSITIONS, collapse = ', ')}"))
message(glue::glue("MIN_CAREER_SEASONS = {MIN_CAREER_SEASONS}, ",
                   "AGE_MIN = {AGE_MIN}, AGE_MAX = {AGE_MAX}\n"))

# ------------------------------------------------------------------------------
# STEP 1: Player-season panel
# ------------------------------------------------------------------------------

message("\n=== STEP 1: Building player-season panel ===")

panel <- build_player_season_panel(
  seasons   = PANEL_SEASONS,
  cache_dir = CACHE_DIR,
  verbose   = TRUE
)

message(glue::glue("  Panel rows: {format(nrow(panel), big.mark = ',')}"))

# ------------------------------------------------------------------------------
# STEP 2: Player ages
# ------------------------------------------------------------------------------

message("\n=== STEP 2: Computing player ages ===")

panel <- compute_player_ages(panel, verbose = TRUE)

# ------------------------------------------------------------------------------
# STEP 3: Fantasy points per game (legacy path)
# ------------------------------------------------------------------------------
# Matches run_aging_curve_pipeline()'s default scoring_settings = NULL branch.

message("\n=== STEP 3: Computing PPR points per game (legacy path) ===")

panel <- compute_season_ppg(panel, ppr_value = 1)

message(glue::glue(
  "  Player-seasons with fp_per_game: ",
  "{format(sum(!is.na(panel$fp_per_game)), big.mark = ',')}"
))

# ------------------------------------------------------------------------------
# STEP 3b: Split WR_TE
# ------------------------------------------------------------------------------

message("\n=== STEP 3b: Splitting WR_TE into WR and TE ===")

panel <- split_wr_te_positions(panel, verbose = TRUE)

# ------------------------------------------------------------------------------
# STEP 4: Box score deltas
# ------------------------------------------------------------------------------
# Argument values copied verbatim from run_aging_curve_pipeline() step 4.

message("\n=== STEP 4: Computing year-over-year PPG deltas ===")

delta_boxscore <- compute_age_deltas(
  panel_with_ages    = panel,
  metric_col         = "fp_per_game",
  positions          = CURVE_POSITIONS,
  age_min            = AGE_MIN,
  age_max            = AGE_MAX,
  min_career_seasons = MIN_CAREER_SEASONS,
  filter_low_volume  = TRUE
)

message(glue::glue(
  "  Transitions: {format(nrow(delta_boxscore), big.mark = ',')}"
))

# ------------------------------------------------------------------------------
# STEP 7: Assumption validation (official)
# ------------------------------------------------------------------------------
# ngs_panel = NULL skips the NGS coverage check. check 5 (normality) is
# unaffected: it reads only delta_data.

message("\n=== STEP 7: Validating assumptions (official R/23 output) ===")

validation <- validate_aging_assumptions(
  panel_with_ages = panel,
  delta_data      = delta_boxscore,
  ngs_panel       = NULL,
  verbose         = TRUE
)

# ------------------------------------------------------------------------------
# CHECK 5 REPLICATION: capture the discarded p-values
# ------------------------------------------------------------------------------
# Identical logic to validate_aging_assumptions() check 5. The only difference
# is that this version keeps the result instead of discarding it.

message("\n=== CHECK 5 REPLICATION: Shapiro-Wilk p-values ===")

normality_results <- purrr::map_dfr(CURVE_POSITIONS, function(pos) {

  test_deltas <- delta_boxscore %>%
    dplyr::filter(
      position_group      == pos,
      age_at_season_start >= NORMALITY_AGE_MIN,
      age_at_season_start <= NORMALITY_AGE_MAX
    ) %>%
    dplyr::pull(delta)

  n_tested <- length(test_deltas)

  if (n_tested < SHAPIRO_MIN_N) {
    return(tibble::tibble(
      position   = pos,
      n_deltas   = n_tested,
      n_to_test  = NA_integer_,
      shapiro_W  = NA_real_,
      shapiro_p  = NA_real_,
      normal     = NA
    ))
  }

  n_to_test <- min(n_tested, SHAPIRO_MAX_N)

  sw <- tryCatch(
    shapiro.test(test_deltas[seq_len(n_to_test)]),
    error = function(e) list(p.value = NA_real_, statistic = NA_real_)
  )

  tibble::tibble(
    position   = pos,
    n_deltas   = n_tested,
    n_to_test  = n_to_test,
    shapiro_W  = round(unname(sw$statistic), 5),
    shapiro_p  = round(sw$p.value, 6),
    normal     = is.na(sw$p.value) || sw$p.value >= ALPHA
  )
})

# ------------------------------------------------------------------------------
# DELTA DISTRIBUTION SHAPE (context for the post)
# ------------------------------------------------------------------------------
# Descriptive stats on the same filtered deltas the test consumed. Gives the
# post a way to say what the distribution looks like, not just whether it
# passed a test.

delta_shape <- delta_boxscore %>%
  dplyr::filter(
    age_at_season_start >= NORMALITY_AGE_MIN,
    age_at_season_start <= NORMALITY_AGE_MAX,
    position_group %in% CURVE_POSITIONS
  ) %>%
  dplyr::group_by(position_group) %>%
  dplyr::summarise(
    n        = dplyr::n(),
    mean     = round(mean(delta, na.rm = TRUE), 4),
    median   = round(stats::median(delta, na.rm = TRUE), 4),
    sd       = round(stats::sd(delta, na.rm = TRUE), 4),
    min      = round(min(delta, na.rm = TRUE), 3),
    max      = round(max(delta, na.rm = TRUE), 3),
    pct_within_1sd = round(
      mean(abs(delta - mean(delta, na.rm = TRUE)) <=
             stats::sd(delta, na.rm = TRUE), na.rm = TRUE) * 100, 2
    ),
    pct_within_2sd = round(
      mean(abs(delta - mean(delta, na.rm = TRUE)) <=
             2 * stats::sd(delta, na.rm = TRUE), na.rm = TRUE) * 100, 2
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n\n")
cat("==============================================================\n")
cat("  POST 14 DEMO DATA. COPY EVERYTHING BELOW THIS LINE\n")
cat("==============================================================\n\n")

cat("--- SEASON / CONFIG ---\n")
cat(sprintf("PANEL_SEASONS      : %d:%d\n", min(PANEL_SEASONS), max(PANEL_SEASONS)))
cat(sprintf("CURVE_POSITIONS    : %s\n", paste(CURVE_POSITIONS, collapse = ", ")))
cat(sprintf("AGE_MIN / AGE_MAX  : %d / %d\n", AGE_MIN, AGE_MAX))
cat(sprintf("MIN_CAREER_SEASONS : %d\n", MIN_CAREER_SEASONS))
cat(sprintf("Normality age band : %d to %d inclusive\n",
            NORMALITY_AGE_MIN, NORMALITY_AGE_MAX))
cat(sprintf("Alpha              : %.2f\n", ALPHA))
cat(sprintf("Panel rows         : %s\n", format(nrow(panel), big.mark = ",")))
cat(sprintf("Total transitions  : %s\n", format(nrow(delta_boxscore), big.mark = ",")))

cat("\n--- OFFICIAL R/23 NORMALITY CHECK ROW ---\n")
norm_row <- validation$report %>%
  dplyr::filter(grepl("normality", check, ignore.case = TRUE))
if (nrow(norm_row) > 0L) {
  cat(sprintf("Status : %s\n", if (norm_row$result[1]) "PASS" else "FLAG"))
  cat(sprintf("Note   : %s\n", norm_row$note[1]))
} else {
  cat("Normality check row not found in report.\n")
}

cat("\n--- SHAPIRO-WILK PER POSITION (the discarded values) ---\n")
print(as.data.frame(normality_results), row.names = FALSE)

cat("\n--- DELTA DISTRIBUTION SHAPE (ages 24-30) ---\n")
print(as.data.frame(delta_shape), row.names = FALSE)

cat("\n--- FULL VALIDATION REPORT ---\n")
print(as.data.frame(
  validation$report %>% dplyr::select(check, critical, result)
), row.names = FALSE)

cat("\n==============================================================\n")
cat("  END. Paste everything above back into the session\n")
cat("==============================================================\n\n")
