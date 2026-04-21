# ==============================================================================
# NFL Analytics Toolkit - Season 2, Week 6
# Example Script: Multi-Season CFB Play-by-Play Loading
# File: examples/example_season2_week6.R
#
# Purpose: Walk through the full Week 6 pipeline on real cfbfastR data.
#          Populates the cache at data/season2_cfb_cache/ and surfaces
#          column names, schema drift, and validation results that will
#          inform the visualization script and downstream Week 7+ code.
#
# Run this BEFORE the assumptions script and BEFORE the visualization
# script. Subsequent runs use cached data (fast).
#
# Execution time (first run): estimated 3-10 minutes for 16 seasons,
#   depending on network speed. cfbfastR's load_cfb_pbp() pulls from
#   GitHub release RDS files, not the CFBD API, so there is no rate limit.
#
# Dependencies: cfbfastR, dplyr, tibble, glue, here
# Source files: R/20_multi_season_cfb_pbp.R
# ==============================================================================

# --- Load source and libraries ---
library(here)
library(dplyr)
library(tibble)
library(glue)

# Install cfbfastR if not already present
if (!requireNamespace("cfbfastR", quietly = TRUE)) {
  message("cfbfastR not installed. Installing from CRAN...")
  install.packages("cfbfastR")
}

library(cfbfastR)

# Source the Week 6 production file
source(here::here("R", "20_multi_season_cfb_pbp.R"))


# ==============================================================================
# STEP 1: Smoke test with a single recent season
# ==============================================================================
#
# Before loading 16 seasons, verify a single recent season loads and
# normalizes correctly. This catches package install issues, network
# problems, and schema assumptions before committing to the full load.

cat(strrep("=", 70), "\n", sep = "")
cat("STEP 1: Smoke test with a single recent season\n")
cat(strrep("=", 70), "\n\n", sep = "")

SMOKE_SEASON <- 2024L

smoke_summary <- load_multi_season_cfb_pbp(
  seasons = SMOKE_SEASON,
  force_reload = FALSE,
  verbose = TRUE
)

print(smoke_summary)

# Inspect the first cached season: what columns did we actually get?
cat("\n--- Column inventory from smoke test ---\n")
smoke_pbp <- load_normalized_cfb_season(
  season = SMOKE_SEASON,
  division = "all"
)

cat(glue("Total columns: {ncol(smoke_pbp)}\n"))
cat(glue("Total rows:    {format(nrow(smoke_pbp), big.mark = ',')}\n"))
cat(glue("\nFirst 30 column names:\n"))
print(head(names(smoke_pbp), 30))

cat(glue("\nKey column presence check (core columns expected):\n"))
key_cols <- c("game_id", "season", "week", "pos_team", "def_pos_team",
              "play_type", "yards_gained", "down", "distance")
for (col in key_cols) {
  present <- col %in% names(smoke_pbp)
  marker <- if (present) "OK  " else "MISS"
  cat(glue("  [{marker}] {col}\n"))
}

cat(glue("\nNFL-style alias presence check:\n"))
alias_cols <- c("posteam", "defteam")
for (col in alias_cols) {
  present <- col %in% names(smoke_pbp)
  marker <- if (present) "OK  " else "MISS"
  cat(glue("  [{marker}] {col}\n"))
}

# Free memory
rm(smoke_pbp)
gc(verbose = FALSE)


# ==============================================================================
# STEP 2: FBS filter sanity check
# ==============================================================================
#
# Verify load_normalized_cfb_season() correctly applies the FBS filter.
# Compare row counts: "all" should be >= "fbs" row count.

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 2: FBS filter sanity check\n")
cat(strrep("=", 70), "\n\n", sep = "")

smoke_all <- load_normalized_cfb_season(SMOKE_SEASON, division = "all")
smoke_fbs <- load_normalized_cfb_season(SMOKE_SEASON, division = "fbs")

cat(glue("Season {SMOKE_SEASON}:\n"))
cat(glue("  All divisions: {format(nrow(smoke_all), big.mark = ',')} plays, ",
         "{length(unique(smoke_all$pos_team))} teams\n"))
cat(glue("  FBS only:      {format(nrow(smoke_fbs), big.mark = ',')} plays, ",
         "{length(unique(smoke_fbs$pos_team))} teams\n"))

if (nrow(smoke_fbs) > nrow(smoke_all)) {
  warning("FBS-filtered rows EXCEEDS 'all' rows. Filter logic error.",
          call. = FALSE)
} else if (nrow(smoke_fbs) == nrow(smoke_all)) {
  cat(glue("  NOTE: FBS row count equals all-division row count. ",
           "Either cache is FBS-only, or classification join failed.\n"))
}

n_fbs_teams <- length(unique(smoke_fbs$pos_team))
if (n_fbs_teams < CFB_FBS_TEAMS_MIN || n_fbs_teams > CFB_FBS_TEAMS_MAX) {
  cat(glue("  WARNING: FBS team count {n_fbs_teams} outside expected ",
           "range [{CFB_FBS_TEAMS_MIN}, {CFB_FBS_TEAMS_MAX}]\n"))
} else {
  cat(glue("  FBS team count within expected range.\n"))
}

rm(smoke_all, smoke_fbs)
gc(verbose = FALSE)


# ==============================================================================
# STEP 3: Full 16-season pipeline
# ==============================================================================
#
# Load all 16 seasons (2010-2025). First run downloads everything;
# subsequent runs skip cached seasons.

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 3: Full 16-season pipeline\n")
cat(strrep("=", 70), "\n\n", sep = "")

pipeline_output <- run_week6_pipeline(
  seasons = CFB_SEASON_RANGE_DEFAULT,
  force_reload = FALSE
)


# ==============================================================================
# STEP 4: Inspect load summary
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 4: Load summary\n")
cat(strrep("=", 70), "\n\n", sep = "")

print(pipeline_output$load_summary)

n_error <- sum(pipeline_output$load_summary$status == "error")
if (n_error > 0) {
  cat(glue("\nWARNING: {n_error} season(s) failed to load.\n"))
  failed_seasons <- pipeline_output$load_summary %>%
    filter(status == "error") %>%
    pull(season)
  cat(glue("Failed: {paste(failed_seasons, collapse = ', ')}\n"))
}


# ==============================================================================
# STEP 5: Inspect coverage validation
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 5: Coverage validation\n")
cat(strrep("=", 70), "\n\n", sep = "")

print(pipeline_output$coverage)

n_coverage_fail <- sum(!pipeline_output$coverage$all_pass)
if (n_coverage_fail > 0) {
  cat(glue("\n{n_coverage_fail} season(s) failed coverage checks. Details:\n"))
  failed <- pipeline_output$coverage %>%
    filter(!all_pass) %>%
    select(season, failures)
  print(failed)
}


# ==============================================================================
# STEP 6: Inspect schema differences
# ==============================================================================

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 6: Schema differences across seasons\n")
cat(strrep("=", 70), "\n\n", sep = "")

diffs <- pipeline_output$schema_diffs

# Universal columns: present in ALL seasons
universal_cols <- diffs %>%
  filter(pct_seasons_present == 1) %>%
  pull(column)

cat(glue("Universal columns (present in all seasons): ",
         "{length(universal_cols)}\n"))
cat(glue("First 20: {paste(head(universal_cols, 20), collapse = ', ')}\n\n"))

# Partial columns: present in some seasons but not others (schema drift)
partial_cols <- diffs %>%
  filter(pct_seasons_present < 1, pct_seasons_present > 0)

cat(glue("Partial-coverage columns (schema drift): {nrow(partial_cols)}\n"))

if (nrow(partial_cols) > 0) {
  cat("Top 20 partial columns by presence:\n")
  print(
    partial_cols %>%
      arrange(desc(pct_seasons_present)) %>%
      select(column, n_seasons_present, pct_seasons_present) %>%
      head(20)
  )
}


# ==============================================================================
# STEP 7: Cross-season play count sanity
# ==============================================================================
#
# Total play counts per season. Year-over-year drops or spikes may
# indicate partial downloads or cfbfastR data issues.

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 7: Play counts per season\n")
cat(strrep("=", 70), "\n\n", sep = "")

play_counts <- pipeline_output$load_summary %>%
  filter(status %in% c("cached", "loaded")) %>%
  select(season, n_plays, n_games) %>%
  arrange(season)

print(play_counts)

# Year-over-year change
if (nrow(play_counts) >= 2) {
  play_counts_yoy <- play_counts %>%
    mutate(
      prev_n_plays = lag(n_plays),
      yoy_pct_change = round(100 * (n_plays - prev_n_plays) / prev_n_plays, 1)
    )

  cat("\nYear-over-year % change in play counts:\n")
  print(play_counts_yoy %>% select(season, n_plays, yoy_pct_change))

  # Flag large drops
  large_drops <- play_counts_yoy %>% filter(yoy_pct_change < -15)
  if (nrow(large_drops) > 0) {
    cat("\nWARNING: Seasons with >15% play count drop YoY:\n")
    print(large_drops)
  }
}


# ==============================================================================
# STEP 8: Export artifacts for downstream scripts
# ==============================================================================
#
# Save the pipeline output as an RDS so the assumptions script and
# visualization script can load it directly without re-running.

cat("\n", strrep("=", 70), "\n", sep = "")
cat("STEP 8: Export pipeline output\n")
cat(strrep("=", 70), "\n\n", sep = "")

output_dir <- here::here("output", "season2_week6")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

pipeline_rds <- file.path(output_dir, "s2_week6_pipeline_output.rds")
saveRDS(pipeline_output, pipeline_rds)
cat(glue("Saved pipeline output: {pipeline_rds}\n"))

# Also export the schema diff as a CSV for easy inspection
schema_csv <- file.path(output_dir, "s2_week6_schema_diffs.csv")
write.csv(pipeline_output$schema_diffs, schema_csv, row.names = FALSE)
cat(glue("Saved schema diff CSV:  {schema_csv}\n"))

# And the coverage report
coverage_csv <- file.path(output_dir, "s2_week6_coverage.csv")
write.csv(pipeline_output$coverage, coverage_csv, row.names = FALSE)
cat(glue("Saved coverage CSV:     {coverage_csv}\n"))

cat("\n", strrep("=", 70), "\n", sep = "")
cat("Example script complete.\n")
cat("Next: run tests/assumptions_season2_week6.R\n")
cat(strrep("=", 70), "\n", sep = "")
