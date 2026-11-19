# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Phase 5
# Bundle-Prep Script for the shinyapps.io Deploy
# File: scripts/deploy/prepare_bundle.R   (run from the PROJECT ROOT)
#
# PURPOSE
# -------
# Makes shiny_app/ self-contained before deploy. shinyapps.io uploads only the
# shiny_app/ directory and cannot reach any file outside it, so the two things
# the app reads from outside the bundle have to be copied in first:
#   1. The canonical Sleeper API script R/19_sleeper_api.R  -> shiny_app/R/
#   2. The six Phase 4 cache files  data/season2_cache/*    -> shiny_app/data/season2_cache/
#
# Run this from the project root every time those canonical files change (for
# example after the R/19 season bump, or after regenerating a cache), then
# deploy. This keeps the bundle copies from drifting away from the originals.
#
# HOW TO RUN
# ----------
#   From the project root, in any R session:
#     source("scripts/deploy/prepare_bundle.R")
#   or from a shell:
#     Rscript scripts/deploy/prepare_bundle.R
#
# DESIGN NOTES
# ------------
# - No packages are loaded, not even `here`. Loading `here` would pin its root
#   for the session and could misdirect the app's own paths if you launched the
#   app afterward in the same session. This script is pure base-R file copying
#   off the working directory, so it has no such side effect.
# - The cache filename list below is maintained BY HAND. It MUST stay in sync
#   with the PATH_* constants in shiny_app/R/data_layer.R. If you add a cache
#   there, add it here. The app's startup "N of N data sources loaded" count is
#   the backstop: a drift shows up immediately as a missing slot.
# - A missing source cache is reported as a warning, not a stop. In particular
#   s2_week16_optimal_lineup.rds may be intentionally absent (pending the
#   optimal_lineup A/B decision); the app degrades that slot gracefully.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Locate and verify the project layout
# ------------------------------------------------------------------------------

project_root <- getwd()
app_dir      <- file.path(project_root, "shiny_app")

canonical_r19 <- file.path(project_root, "R", "19_sleeper_api.R")
app_entry     <- file.path(app_dir, "app.R")

if (!file.exists(app_entry)) {
  stop(
    "Could not find shiny_app/app.R under the current working directory:\n  ",
    project_root, "\n",
    "Run this script from the project root (the folder that contains both ",
    "R/ and shiny_app/).",
    call. = FALSE
  )
}
if (!file.exists(canonical_r19)) {
  stop(
    "Could not find the canonical R/19_sleeper_api.R at:\n  ", canonical_r19,
    "\nRun this script from the project root.",
    call. = FALSE
  )
}

# Warn (do not stop) if the here() anchor sentinel is missing: without it the
# bundle will not resolve its paths on the server.
sentinel <- file.path(app_dir, ".here")
if (!file.exists(sentinel)) {
  warning(
    "shiny_app/.here is missing. The app will not resolve its paths on the ",
    "deployed server without it. Create it with file.create(\"shiny_app/.here\") ",
    "before deploying.",
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 1. Cache filename list -- KEEP IN SYNC WITH shiny_app/R/data_layer.R
# ------------------------------------------------------------------------------

cache_files <- c(
  "s2_week15_vorp_rankings.rds",        # vorp          (R/33)
  "s2_week16_r36_start_sit_uq.rds",     # start_sit_uq  (R/36)
  "s2_week16_r36_def_streaming.rds",    # def_streaming (R/36)
  "s2_week16_optimal_lineup.rds",       # optimal_lineup(R/35) -- may be absent
  "s2_week14_final_prospect_scores.csv",# prospects     (R/28)
  "s2_week17_trade_values.rds"          # trade_values  (R/37)
)

# Runtime caches the app reads through R/19 but that are NOT data-layer slots.
# The Sleeper players list backs the Tab 4 lookup. Bundling it means the server
# only ever reads it; without it the server falls back to a live ~5MB pull on
# each cold start and may try to write to a read-only filesystem.
runtime_caches <- c(
  "sleeper_players_nfl.rds"             # R/19 get_all_sleeper_players cache
)

src_cache_dir  <- file.path(project_root, "data", "season2_cache")
dest_cache_dir <- file.path(app_dir, "data", "season2_cache")
dest_r_dir     <- file.path(app_dir, "R")

# ------------------------------------------------------------------------------
# 2. Ensure destination directories exist
# ------------------------------------------------------------------------------

dir.create(dest_r_dir,     recursive = TRUE, showWarnings = FALSE)
dir.create(dest_cache_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 3. Copy the canonical R/19 into the bundle
# ------------------------------------------------------------------------------

cat(strrep("=", 70), "\n", sep = "")
cat("Bundle prep: shiny_app/\n")
cat(strrep("=", 70), "\n", sep = "")

r19_ok <- file.copy(canonical_r19,
                    file.path(dest_r_dir, "19_sleeper_api.R"),
                    overwrite = TRUE)
cat(sprintf("  R/19_sleeper_api.R -> shiny_app/R/   : %s\n",
            if (r19_ok) "copied" else "FAILED"))

# ------------------------------------------------------------------------------
# 4. Copy the caches, reporting each
# ------------------------------------------------------------------------------

cat("\n  Caches (", src_cache_dir, " -> shiny_app/data/season2_cache/):\n",
    sep = "")

copied  <- character(0)
missing <- character(0)

for (f in cache_files) {
  src <- file.path(src_cache_dir, f)
  if (!file.exists(src)) {
    missing <- c(missing, f)
    cat(sprintf("    [ missing ] %s\n", f))
    next
  }
  ok <- file.copy(src, file.path(dest_cache_dir, f), overwrite = TRUE)
  if (ok) {
    copied <- c(copied, f)
    cat(sprintf("    [ copied  ] %s\n", f))
  } else {
    missing <- c(missing, f)
    cat(sprintf("    [ FAILED  ] %s\n", f))
  }
}

# Runtime caches (Tab 4 players list). A miss here is a soft warning: the app
# still works on the server, it just does a live pull on cold start.
players_ok <- TRUE
cat("\n  Runtime caches (Tab 4 Sleeper players):\n")
for (f in runtime_caches) {
  src <- file.path(src_cache_dir, f)
  if (!file.exists(src)) {
    players_ok <- FALSE
    cat(sprintf("    [ missing ] %s  (server will pull live on cold start)\n", f))
    next
  }
  ok <- file.copy(src, file.path(dest_cache_dir, f), overwrite = TRUE)
  cat(sprintf("    [ %s ] %s\n", if (ok) "copied " else "FAILED ", f))
  if (!ok) players_ok <- FALSE
}

# ------------------------------------------------------------------------------
# 5. Summary
# ------------------------------------------------------------------------------

cat("\n", strrep("-", 70), "\n", sep = "")
cat(sprintf("  R/19 bundled:        %s\n", if (r19_ok) "yes" else "NO"))
cat(sprintf("  Data-layer caches:   %d of %d copied\n",
            length(copied), length(cache_files)))
cat(sprintf("  Players cache:       %s\n",
            if (players_ok) "bundled" else "absent (live pull on server)"))
if (length(missing) > 0L) {
  cat(sprintf("  Missing/failed:      %s\n", paste(missing, collapse = ", ")))
  cat("  The app will report these as missing data slots at startup.\n")
}
cat(strrep("-", 70), "\n", sep = "")
cat("  Done. Verify in a FRESH R session, then deploy from the project root:\n")
cat("    source(\"deploy.R\"); deploy_now(\"nfl-analytics-toolkit\")\n")
cat(strrep("=", 70), "\n", sep = "")

# Surface missing caches as a warning too, so it is visible in non-interactive
# (Rscript) runs that may not scroll the console.
if (length(missing) > 0L) {
  warning("Bundle prep finished with missing caches: ",
          paste(missing, collapse = ", "), call. = FALSE)
}
