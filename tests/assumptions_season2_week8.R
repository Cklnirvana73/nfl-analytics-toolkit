# ==============================================================================
# examples/assumptions_season2_week8.R
# Assumption validation for R/22_sos_reconciliation.R
#
# Execution order (project standard): example -> assumptions -> tests -> visuals
# Run example_season2_week8.R before this script.
#
# Five assumptions tested:
#   1. Linearity of SOS-efficiency relationship (per position)
#   2. Garbage time filter consistency between R/21 and R/22
#   3. SOS independence from position within team-season
#   4. Opponent sample adequacy (>= 6 opponents per player-season)
#   5. low_volume flag coverage (above-threshold rows have usable stats)
#
# All numbers in KEY INSIGHTS are computed from data -- never hardcoded.
# ==============================================================================

library(dplyr)
library(glue)
library(here)

source(here::here("R", "22_sos_reconciliation.R"))


# ==============================================================================
# LOAD SOS PANEL
# ==============================================================================

cat("Loading SOS panel...\n")

sos_path <- here::here("data", "season2_cfb_cache", "s2_week8_cfb_sos_panel.rds")
ex_path  <- here::here("data", "season2_cfb_cache", "s2_week8_cfb_sos_panel_example.rds")

if (file.exists(sos_path)) {
  panel_sos <- readRDS(sos_path)
  cat(glue("Full SOS panel: {format(nrow(panel_sos), big.mark=',')} rows | ",
           "seasons {min(panel_sos$season)}-{max(panel_sos$season)}\n"))
} else if (file.exists(ex_path)) {
  panel_sos <- readRDS(ex_path)
  cat(glue("Example SOS panel (subset): {format(nrow(panel_sos), big.mark=',')} rows | ",
           "seasons {min(panel_sos$season)}-{max(panel_sos$season)}\n"))
  cat("NOTE: Run full pipeline for complete 2014-2025 assumption checks.\n")
} else {
  stop(glue(
    "No SOS panel found. Run example_season2_week8.R first.\n",
    "Expected: {sos_path}"
  ))
}


# ==============================================================================
# RUN ASSUMPTION CHECKS
# ==============================================================================

cat("\n=== Running Week 8 Assumption Checks ===\n")

ar <- validate_week8_assumptions(
  sos_panel      = panel_sos,
  min_n_for_corr = 30L
)


# ==============================================================================
# PRINT SUMMARY TABLE
# ==============================================================================

cat("\n--- Assumption Summary ---\n")
print(ar$summary[, c("assumption", "severity", "passed", "detail")])


# ==============================================================================
# KEY INSIGHTS: computed from results, never hardcoded
# ==============================================================================

cat("\n--- KEY INSIGHTS ---\n")

# Linearity results
cat("\nAssumption 1: SOS-Efficiency Linearity\n")
for (pos in names(ar$linearity)) {
  res <- ar$linearity[[pos]]
  if (is.na(res$r)) {
    cat(glue("  {pos}: insufficient data (n={res$n})\n"))
  } else {
    direction <- if (res$slope > 0) "positive" else "negative"
    sig_label <- if (!is.na(res$p) && res$p < 0.05) "significant" else "not significant"
    cat(glue(
      "  {pos}: r={res$r}, R\u00b2={res$r2}, slope={res$slope} ({direction}), ",
      "p={res$p} ({sig_label}), n={res$n}\n"
    ))
  }
}

# Interpretation of linearity
cat("\nInterpretation:\n")
cat(paste(
  "  A positive slope means players facing harder defenses (lower opponent EPA allowed)",
  "  tend to have lower raw efficiency -- which is the expected direction.",
  "  R\u00b2 < 0.10 is expected given the many other factors driving CFB efficiency.",
  "  The adjustment is small but directionally correct.",
  sep = "\n"
), "\n")

# Filter consistency
filter_row <- ar$summary[ar$summary$assumption ==
                           "Garbage time filter consistency (R/21 vs R/22)", ]
if (nrow(filter_row) > 0L) {
  status <- if (filter_row$passed) "CONFIRMED" else "MISMATCH -- INVESTIGATE"
  cat(glue("\nAssumption 2: Filter Consistency -- {status}\n"))
  cat(glue("  {filter_row$detail}\n"))
}

# SOS position independence
ind_row <- ar$summary[ar$summary$assumption ==
                        "SOS independent of position within team-season", ]
if (nrow(ind_row) > 0L) {
  status <- if (ind_row$passed) "CONFIRMED" else "VIOLATION DETECTED"
  cat(glue("\nAssumption 3: Position Independence -- {status}\n"))
  cat(glue("  {ind_row$detail}\n"))
}

# Opponent sample
opp_row <- ar$summary[ar$summary$assumption ==
                        "Opponent sample adequacy (>= 6 opponents per player-season)", ]
if (nrow(opp_row) > 0L) {
  cat(glue("\nAssumption 4: Opponent Sample Adequacy\n"))
  cat(glue("  {opp_row$detail}\n"))

  if (!is.null(ar$low_n_opponents) && nrow(ar$low_n_opponents) > 0L) {
    cat(glue("  Top 5 low-opponent rows:\n"))
    ar$low_n_opponents %>%
      dplyr::arrange(sos_n_opponents) %>%
      dplyr::select(player_name, season, primary_team,
                    position_group, sos_n_opponents) %>%
      head(5L) %>%
      print()
  }
}

# low_volume coverage
lv_row <- ar$summary[
  ar$summary$assumption ==
    "low_volume == FALSE rows have at least one non-NA efficiency column", ]
if (nrow(lv_row) > 0L) {
  status <- if (lv_row$passed) "CONFIRMED" else "COVERAGE GAP DETECTED"
  cat(glue("\nAssumption 5: low_volume Coverage -- {status}\n"))
  cat(glue("  {lv_row$detail}\n"))
}


# ==============================================================================
# PHASE 3 READINESS STATEMENT
# ==============================================================================

cat("\n--- Phase 3 Readiness ---\n")

n_critical_failed <- sum(
  !ar$summary$passed[ar$summary$severity == "critical"],
  na.rm = TRUE
)
n_warnings <- sum(
  !ar$summary$passed[ar$summary$severity == "warning"],
  na.rm = TRUE
)

if (n_critical_failed == 0L) {
  cat(glue(
    "SOS panel CLEARED for Phase 3 use.\n",
    "Critical assumptions: all passed.\n",
    "Warnings: {n_warnings} (see summary above for details).\n"
  ))
} else {
  cat(glue(
    "SOS panel NOT cleared for Phase 3 use.\n",
    "{n_critical_failed} critical assumption(s) failed.\n",
    "Resolve before using SOS features in modeling.\n"
  ))
}

cat("\n=== assumptions_season2_week8.R complete ===\n")
