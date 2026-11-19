# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Phase 5
# Deploy pre-flight + publish helper for shinyapps.io
# File: scripts/deploy/deploy.R   (run from the PROJECT ROOT)
#
# PURPOSE
# -------
# Sourcing this script runs a PRE-FLIGHT CHECK ONLY and prints GO or NO-GO. It
# does not publish anything. Publishing is a separate, explicit action you take:
#   deploy_now("nfl-analytics-toolkit")
# which wraps rsconnect::deployApp(). Keeping publish behind a function call,
# rather than running it on source, means sourcing this file is always safe.
#
# PREREQUISITES (one-time, done by you):
#   1. install.packages("rsconnect")
#   2. rsconnect::setAccountInfo(...) with your shinyapps.io token + secret
#      (you have completed both; account name: cklnirvana73)
#
# BEFORE DEPLOYING, in order:
#   1. From the project root: source("scripts/deploy/prepare_bundle.R")   (copies R/19, the
#      caches, and the players cache into shiny_app/)
#   2. In a FRESH R session, launch the app and confirm the banner
#      (shiny::runApp("shiny_app") or RStudio Run App). Do not run analytics
#      scripts in that session; the here() root is pinned to shiny_app.
#   3. In another FRESH session from the project root: source("scripts/deploy/deploy.R")
#      and read the GO/NO-GO, then deploy_now("nfl-analytics-toolkit").
#
# NOTES
# -----
# - No here() is used here, to avoid pinning the session root. Paths are taken
#   relative to the working directory, so run this from the project root.
# - The deployed URL will be:
#       https://cklnirvana73.shinyapps.io/nfl-analytics-toolkit
# - shinyapps.io has no durable writable storage. The app is cache-read by
#   design; everything it needs must be inside shiny_app/ at deploy time.
# ==============================================================================

# ------------------------------------------------------------------------------
# Pre-flight check
# ------------------------------------------------------------------------------

deploy_preflight <- function() {

  project_root <- getwd()
  app_dir      <- file.path(project_root, "shiny_app")

  results <- list()
  record  <- function(label, ok, detail = "") {
    ok <- isTRUE(ok)
    results[[length(results) + 1L]] <<- list(label = label, ok = ok)
    cat(sprintf("  [%s] %s%s\n",
                if (ok) "GO   " else "NO-GO",
                label,
                if (nzchar(detail)) paste0("  (", detail, ")") else ""))
    invisible(ok)
  }

  cat(strrep("=", 70), "\n", sep = "")
  cat("Deploy pre-flight: shiny_app/ -> cklnirvana73.shinyapps.io\n")
  cat(strrep("=", 70), "\n", sep = "")

  # rsconnect installed and an account configured.
  has_rsconnect <- requireNamespace("rsconnect", quietly = TRUE)
  record("rsconnect package installed", has_rsconnect)

  acct_ok <- FALSE
  if (has_rsconnect) {
    accts <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
    acct_ok <- !is.null(accts) && nrow(accts) > 0L
    record("a shinyapps.io account is configured", acct_ok,
           if (acct_ok) paste(accts$name, collapse = ", ") else "run setAccountInfo")
  } else {
    record("a shinyapps.io account is configured", FALSE, "rsconnect missing")
  }

  # The app entry point.
  record("shiny_app/app.R exists", file.exists(file.path(app_dir, "app.R")))

  # Bundled code (prepare_bundle.R must have run).
  needed_code <- c(
    file.path("R", "19_sleeper_api.R"),
    file.path("R", "data_layer.R"),
    file.path("R", "trade_helpers.R"),
    file.path("tabs", "projections.R"),
    file.path("tabs", "waiver.R"),
    file.path("tabs", "trade.R"),
    file.path("tabs", "rookie.R")
  )
  code_missing <- needed_code[!file.exists(file.path(app_dir, needed_code))]
  record("bundled code present (R/19, data_layer, helpers, tabs)",
         length(code_missing) == 0L,
         if (length(code_missing)) paste(code_missing, collapse = ", ")
         else "run scripts/deploy/prepare_bundle.R if this is NO-GO")

  # Required data-layer caches in the bundle (optimal_lineup is allowed absent).
  cache_dir <- file.path(app_dir, "data", "season2_cache")
  required_caches <- c(
    "s2_week15_vorp_rankings.rds",
    "s2_week16_r36_start_sit_uq.rds",
    "s2_week16_r36_def_streaming.rds",
    "s2_week14_final_prospect_scores.csv",
    "s2_week17_trade_values.rds"
  )
  cache_missing <- required_caches[!file.exists(file.path(cache_dir, required_caches))]
  record("required caches bundled (5 of 6; optimal_lineup optional)",
         length(cache_missing) == 0L,
         if (length(cache_missing)) paste(cache_missing, collapse = ", ")
         else "run scripts/deploy/prepare_bundle.R if this is NO-GO")

  # Soft, informational only: these do not block deploy.
  lineup_here  <- file.exists(file.path(cache_dir, "s2_week16_optimal_lineup.rds"))
  players_here <- file.exists(file.path(cache_dir, "sleeper_players_nfl.rds"))
  cat(sprintf("  [info ] optimal_lineup cache: %s\n",
              if (lineup_here) "bundled (will read 6 of 6)"
              else "absent (will read 5 of 6, fine for Version 1)"))
  cat(sprintf("  [info ] players cache: %s\n",
              if (players_here) "bundled (Tab 4 fast)"
              else "absent (Tab 4 pulls live on cold start)"))

  # app.R self-anchors with here::i_am.
  app_txt <- tryCatch(readLines(file.path(app_dir, "app.R"), warn = FALSE),
                      error = function(e) character(0))
  record("app.R self-anchors with here::i_am(\"app.R\")",
         any(grepl('here::i_am\\("app.R"\\)', app_txt)))

  n_go  <- sum(vapply(results, function(r) r$ok, logical(1)))
  n_tot <- length(results)
  cat(strrep("-", 70), "\n", sep = "")
  if (n_go == n_tot) {
    cat(sprintf("  PRE-FLIGHT: GO  (%d of %d). Publish with:\n", n_go, n_tot))
    cat("    deploy_now(\"nfl-analytics-toolkit\")\n")
  } else {
    cat(sprintf("  PRE-FLIGHT: NO-GO  (%d of %d). Resolve the NO-GO items above.\n",
                n_go, n_tot))
  }
  cat(strrep("=", 70), "\n", sep = "")

  invisible(n_go == n_tot)
}

# ------------------------------------------------------------------------------
# Publish (explicit; you call this)
# ------------------------------------------------------------------------------

deploy_now <- function(app_name = "nfl-analytics-toolkit") {

  if (!grepl("^[a-z0-9][a-z0-9-]{2,61}[a-z0-9]$", app_name)) {
    stop("app_name must be 4-63 chars, lowercase letters/numbers/dashes, ",
         "not starting or ending with a dash. Got: ", app_name, call. = FALSE)
  }

  if (!isTRUE(deploy_preflight())) {
    stop("Pre-flight is NO-GO. Resolve the flagged items before deploying.",
         call. = FALSE)
  }

  app_dir <- file.path(getwd(), "shiny_app")
  cat("\nDeploying ", app_dir, "\n", sep = "")
  cat("Target: https://cklnirvana73.shinyapps.io/", app_name, "\n\n", sep = "")

  rsconnect::deployApp(
    appDir      = app_dir,
    appName     = app_name,
    appTitle    = "NFL Analytics Toolkit",
    account     = "cklnirvana73",
    forceUpdate = TRUE,
    launch.browser = TRUE
  )
}

# Sourcing runs the pre-flight only. It never publishes.
deploy_preflight()
