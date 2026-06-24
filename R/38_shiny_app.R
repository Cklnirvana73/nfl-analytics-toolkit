# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Week 18
# R/38: Shiny App Launcher
# File: R/38_shiny_app.R
#
# PURPOSE
# -------
# Thin launcher for the Phase 5 Shiny app. All application logic lives in
# shiny_app/app.R. This file preserves the Season 2 R/NN_ numbering convention
# while keeping the app in the shinyapps.io-deployable shiny_app/ directory.
# It is a development convenience and is NOT part of the deployed bundle.
#
# RUN LOCALLY (read this -- the app must start in a FRESH R session)
# ------------------------------------------------------------------
# From the project root, in a clean R session that has NOT already loaded the
# `here` package:
#   source("R/38_shiny_app.R")
# Or in RStudio, click "Run App". RStudio launches the app in its own fresh
# process, which is the simplest way to get this right.
#
# WHY A FRESH SESSION MATTERS
# ---------------------------
# The `here` package fixes its project root ONCE, the first time it loads in a
# session, from the working directory at that moment. The app relies on here()
# anchoring to the shiny_app/ bundle root via the shiny_app/.here sentinel, so
# that every source() and cache path resolves the same locally as on the
# deployed server. If `here` was already loaded earlier in the session (for
# example by running an analytics script such as R/33 from the project root),
# its root is pinned to the project root, and the app's here::here("R", ...)
# calls would look in project_root/R/ instead of shiny_app/R/. A fresh session
# avoids that. This launcher deliberately does NOT load `here`, so that app.R
# loads it after runApp() has set the working directory into shiny_app/.
#
# DEPLOY TO SHINYAPPS.IO
# ----------------------
#   From the project root: rsconnect::deployApp("shiny_app")
#   The deploy target is the shiny_app/ directory, not this launcher.
#
# SCHEMA TAG: s2_w18_shiny_v1
# ==============================================================================

library(shiny)

# Locate the app directory relative to the project root, which is the working
# directory when this launcher is sourced. We intentionally do NOT use
# here::here() to find it: loading `here` now would pin its root to the project
# root and misdirect the app's own path resolution. See WHY A FRESH SESSION
# MATTERS above.
app_dir <- file.path(getwd(), "shiny_app")

if (!dir.exists(app_dir)) {
  stop(
    "Could not find a 'shiny_app/' directory under the current working ",
    "directory:\n  ", getwd(), "\n",
    "Run this launcher from the project root (the folder that contains ",
    "shiny_app/), in a fresh R session.",
    call. = FALSE
  )
}

shiny::runApp(app_dir, launch.browser = TRUE)
