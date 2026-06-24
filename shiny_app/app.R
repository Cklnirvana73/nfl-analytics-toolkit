# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Phase 5
# Shiny App: Architecture and Data Layer
# File: shiny_app/app.R
#
# PURPOSE
# -------
# Week 18 scope: data loading foundation and four-tab skeleton.
# All Phase 4 cached RDS/CSV outputs are loaded once at startup outside
# server(). The app does NOT re-run the projection pipeline -- it reads only
# from cache. Tab content is built in Weeks 19-21.
#
# FOUR TABS
# ---------
#   Tab 1: Projections      -- R/29-R/33 outputs   (built Season 2 Week 19)
#   Tab 2: Waiver/Start-Sit -- R/35-R/36 outputs   (built Season 2 Week 20)
#   Tab 3: Trade Evaluator  -- R/37 output          (built Season 2 Week 21)
#   Tab 4: Rookie Tracker   -- R/28 + R/19 outputs  (built Season 2 Week 21)
#
# LIVE API CALLS (not at startup -- reactive on user input)
# ---------------------------------------------------------
#   Tab 2: cache-read in Week 20 (start/sit + DEF streaming). The live
#          suggest_waiver_adds() button is deferred to a follow-on build.
#   Tab 4: get_sleeper_rosters() from R/19 fires on player search (Week 21)
#   Tab 3: evaluate_trade() from trade_helpers.R fires on input (Week 21)
#
# DEPLOY
# ------
#   From the project root: rsconnect::deployApp("shiny_app")
#
# RUN LOCALLY
# -----------
# Launch in a FRESH R session, from the project root, and do NOT call here() in
# the console first. Calling here::here() before the app starts pins here's root
# to the project root and breaks the app's own path resolution. Use either:
#   shiny::runApp("shiny_app")
# or RStudio's "Run App" button, which runs in its own fresh process. Once the
# app starts, runApp has set the working dir to the app root, so app.R's own
# library(here) anchors to the .here sentinel inside shiny_app.
#
# SCHEMA TAG: s2_w21_shiny_v6
# ==============================================================================

# ------------------------------------------------------------------------------
# LIBRARIES
# ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(glue)
library(here)
library(httr)

# Force the here() root to this app's own directory (shiny_app), regardless of
# the working directory, any .here sentinel, or whether here was already loaded
# earlier in the session. i_am() locates the directory containing this app.R and
# pins the root there, so every source() and cache path below resolves inside
# the bundle both locally and on the deployed server. This makes the app
# self-correcting: it no longer depends on a .here file being placed by hand.
# Local note: this pins here to shiny_app for the session, so run the app in a
# fresh session and do not run project-level analytics scripts in the same one.
here::i_am("app.R")

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

SEASON <- 2026L

# Default Sleeper username for the Tab 4 lookup input (wired in Week 21).
# Empty by default so no personal username ships in the public app -- the user
# types their own at runtime. Set this locally if you want it pre-filled for
# your own use, but leave it empty in anything deployed publicly.
SLEEPER_USERNAME_DEFAULT <- ""

# Cache file paths and load_shiny_data() live in shiny_app/R/data_layer.R,
# sourced below. Kept separate so the data layer is unit-testable in isolation.

# ------------------------------------------------------------------------------
# SOURCE DEPENDENCIES
# ------------------------------------------------------------------------------

# Sleeper API: get_user_leagues(), get_sleeper_rosters(), match_sleeper_players()
# Reactive calls wired in Week 21 (Tab 4 Player League Lookup).
# Bundled copy at shiny_app/R/19_sleeper_api.R. The canonical file is the
# project-level R/19; the bundle-prep step copies it in so the app is
# self-contained on deploy. here() anchors to shiny_app via the .here sentinel.
source(here::here("R", "19_sleeper_api.R"))

# Data layer: PATH constants + load_shiny_data(). Pure definitions, no
# execution. Extracted so the data layer can be unit-tested without launching
# the app. See shiny_app/R/data_layer.R.
source(here::here("R", "data_layer.R"))

# evaluate_trade() extracted from R/37. Pure: no I/O, no side effects.
# Sourced here rather than via R/37 directly to avoid pulling R/37's source()
# chain (R/19, R/23) into app startup twice. See shiny_app/R/trade_helpers.R.
source(here::here("R", "trade_helpers.R"))

# Tab modules. Pure definitions (projectionsUI() / projectionsServer() etc.),
# sourced before ui and server are built. One file per tab in shiny_app/tabs/.
#   Tab 1: Projections (Season 2 Week 19)
source(here::here("tabs", "projections.R"))
#   Tab 2: Waiver / Start-Sit (Season 2 Week 20)
source(here::here("tabs", "waiver.R"))
#   Tab 3: Trade Evaluator (Season 2 Week 21)
source(here::here("tabs", "trade.R"))
#   Tab 4: Rookie Tracker + Player League Lookup (Season 2 Week 21)
source(here::here("tabs", "rookie.R"))

# ==============================================================================
# SECTION 1: DATA LAYER STARTUP
# ==============================================================================
# load_shiny_data() and the PATH constants are defined in
# shiny_app/R/data_layer.R, sourced above. Here we run it once at startup,
# before ui and server are defined, so the loaded data is shared across all
# sessions on the worker process.

message(glue(
  "\n{strrep('=', 60)}\n",
  "NFL Analytics Toolkit | Shiny App (Season {SEASON})\n",
  "{strrep('=', 60)}"
))
message("Loading cached Phase 4 outputs...")

app_data <- load_shiny_data()

n_loaded <- sum(vapply(app_data, Negate(is.null), logical(1)))
n_total  <- length(app_data)
message(glue("  {n_loaded} of {n_total} data sources loaded."))

if (n_loaded < n_total) {
  missing_nm <- names(app_data)[vapply(app_data, is.null, logical(1))]
  message(glue("  Missing slots: {paste(missing_nm, collapse = ', ')}"))
}
message(glue("{strrep('=', 60)}\n"))

# ==============================================================================
# SECTION 2: THEME
# ==============================================================================

app_theme <- bslib::bs_theme(
  version   = 5,
  primary   = "#013369",   # NFL blue
  secondary = "#D50A0A"    # NFL red
)

# ==============================================================================
# SECTION 3: UI HELPERS
# ==============================================================================

#' Build a data status card for a placeholder tab
#'
#' Confirms whether a Phase 4 cache file loaded successfully, shows the row
#' count, and notes which Season 2 week will replace the placeholder with real
#' tab content. Used in all four tabs during the Week 18 skeleton phase.
#'
#' @param label Character. Human-readable name for this data source.
#' @param data_obj Tibble or NULL. The loaded data object, or NULL if missing.
#' @param src_script Character. The source script (e.g. "R/33").
#' @param coming_week Integer. Season 2 week when this tab gets real content.
#' @return A bslib card.
make_status_card <- function(label, data_obj, src_script, coming_week) {

  if (is.null(data_obj)) {
    hdr_class   <- "bg-danger text-white"
    status_line <- glue("Cache not found. Run {src_script} first.")
    row_line    <- "No data loaded"
  } else {
    hdr_class   <- "bg-success text-white"
    status_line <- glue("Loaded from {src_script}")
    row_line    <- glue("{format(nrow(data_obj), big.mark = ',')} rows")
  }

  bslib::card(
    bslib::card_header(label, class = hdr_class),
    bslib::card_body(
      shiny::tags$p(shiny::tags$strong("Status: "), status_line),
      shiny::tags$p(shiny::tags$strong("Rows: "),   row_line),
      shiny::tags$hr(),
      shiny::tags$p(
        class = "text-muted fst-italic",
        glue("Full tab content builds in Season 2 Week {coming_week}.")
      )
    )
  )
}

# ==============================================================================
# SECTION 4: UI
# ==============================================================================

ui <- bslib::page_navbar(

  title           = "NFL Analytics Toolkit",
  theme           = app_theme,
  navbar_options  = bslib::navbar_options(bg = "#013369"),
  fillable        = FALSE,

  # ---- Tab 1: Projections (built Season 2 Week 19) --------------------------

  bslib::nav_panel(
    title = "Projections",
    projectionsUI("proj")
  ),

  # ---- Tab 2: Waiver / Start-Sit (built Season 2 Week 20) -------------------

  bslib::nav_panel(
    title = "Waiver / Start-Sit",
    waiverUI("waiver")
  ),

  # ---- Tab 3: Trade Evaluator -----------------------------------------------

  bslib::nav_panel(
    title = "Trade Evaluator",
    tradeUI("trade")
  ),

  # ---- Tab 4: Rookie Tracker ------------------------------------------------

  bslib::nav_panel(
    title = "Rookie Tracker",
    rookieUI("rookie")
  ),

  # ---- Right-side navbar text -----------------------------------------------
  bslib::nav_spacer(),
  bslib::nav_item(
    shiny::tags$span(
      style = "color: rgba(255,255,255,0.65); font-size: 0.875rem;",
      glue("Season {SEASON} | Phase 5")
    )
  )
)

# ==============================================================================
# SECTION 5: SERVER
# ==============================================================================

server <- function(input, output, session) {
  # app_data is loaded at startup (above) and available here via closure.
  # All six data objects are static for the life of the session -- no reactive
  # reload from disk.
  #
  # REACTIVE ACCESSORS -- the single seam for the multi-user upgrade. Every tab
  # module reads through these, never from app_data directly. For the current
  # single-user build they return the cached objects loaded at startup. The
  # post-launch multi-user upgrade changes ONLY these definitions (e.g. to scope
  # by the logged-in user's leagues); the tab modules are untouched.
  current_vorp          <- shiny::reactive(app_data$vorp)
  current_start_sit     <- shiny::reactive(app_data$start_sit_uq)
  current_def_streaming <- shiny::reactive(app_data$def_streaming)
  current_trade_values  <- shiny::reactive(app_data$trade_values)
  current_prospects     <- shiny::reactive(app_data$prospects)

  # ---- Tab 1: Projections (Season 2 Week 19) ----
  projectionsServer("proj", current_vorp, current_start_sit)

  # ---- Tab 2: Waiver / Start-Sit (Season 2 Week 20) ----
  # Reads the enriched lineup through current_start_sit (the same accessor wired
  # into Tab 1's seam) and the DEF streaming table through current_def_streaming.
  # Cache-read only: no live suggest_waiver_adds() call. The live waiver-add path
  # is deferred to a follow-on build. current_optimal_lineup is intentionally not
  # wired: start_sit_uq is a superset of the lineup, so Tab 2 reads the richer
  # table and never the bare lineup.
  waiverServer("waiver", current_start_sit, current_def_streaming)

  # ---- Tab 3: Trade Evaluator (Season 2 Week 21) ----
  # Reads the R/37 trade value table through current_trade_values and calls the
  # pure evaluate_trade() (sourced from shiny_app/R/trade_helpers.R) on the
  # evaluate button. Cache-read only: no trade-value re-derivation in the app.
  tradeServer("trade", current_trade_values, SEASON)

  # ---- Tab 4: Rookie Tracker + Player League Lookup (Season 2 Week 21) ----
  # Reads the R/28 prospect scores through current_prospects for the tracker and
  # comparables. The Player League Lookup makes live Sleeper calls on its own
  # Search button (get_user_leagues + get_sleeper_rosters per league); SEASON is
  # passed so the lookup queries the same season as the rest of the app.
  rookieServer("rookie", current_prospects, SEASON)
}

# ==============================================================================
# SECTION 6: LAUNCH
# ==============================================================================

shiny::shinyApp(ui = ui, server = server)
