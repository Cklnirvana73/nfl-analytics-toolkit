# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Phase 5
# Shiny Tab 1: Projection Dashboard
# File: shiny_app/tabs/projections.R
#
# PURPOSE
# -------
# Tab 1 of the Phase 5 Shiny app. Renders the Bayesian player projections from
# R/33 (VORP rankings, schema s2_w15_vorp_v2) as a filterable DT table with the
# posterior point estimate, the 80% interval (floor and ceiling), boom/bust
# probabilities, and VORP. Filters by league and position.
#
# SOURCED BY
# ----------
# shiny_app/app.R. This file contains PURE DEFINITIONS ONLY -- sourcing it just
# defines projectionsUI() and projectionsServer(). The libraries (shiny, bslib,
# DT, dplyr) are attached by app.R; calls here are namespaced so the file is
# also sourceable in isolation for inspection.
#
# REACTIVE ACCESSOR DISCIPLINE (non-negotiable)
# ---------------------------------------------
# projectionsServer() reads data ONLY through the reactive accessors passed in
# (current_vorp, current_start_sit), never from app_data directly. For the
# current single-user build these accessors return the cached objects. The
# post-launch multi-user upgrade changes only the accessor definitions in
# app.R's server(); nothing in this file changes. current_start_sit is wired in
# now to establish the seam even though Tab 1 does not yet surface start/sit
# data.
#
# DATA CONTRACT (app_data$vorp, from R/33 schema s2_w15_vorp_v2)
# -------------------------------------------------------------
#   nfl_gsis_id, player_name, team, position, league_name, league_format,
#   league_teams, r32_posterior_mu, r32_projection_lower_80,
#   r32_projection_upper_80, boom_probability, bust_probability,
#   replacement_ppg, vorp_base, boom_modifier, bust_modifier, ceiling_modifier,
#   adjusted_vorp, overall_rank, position_rank, schema_tag
#
# SCHEMA TAG: s2_w19_projtab_v2
# ==============================================================================

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

# Position filter choices. "All" passes everything through.
PROJTAB_POSITIONS <- c("All", "QB", "RB", "WR", "TE")

# ------------------------------------------------------------------------------
# projectionsUI
# ------------------------------------------------------------------------------

#' Tab 1 UI: projection dashboard
#'
#' Returns the tab body: a short intro, a league/position filter sidebar, and
#' the projection table output. All inputs and outputs are namespaced with the
#' module id so multiple instances never collide.
#'
#' @param id Character. Module namespace id (matched in projectionsServer()).
#' @return A Shiny UI tag list for use inside a bslib::nav_panel().
projectionsUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::h3("Player Projections"),
    shiny::p(
      class = "lead text-muted",
      paste(
        "Bayesian projections with an 80% interval, boom/bust probabilities,",
        "and VORP, scoped to a single league's configuration.",
        "Powered by R/29-R/33."
      )
    ),
    shiny::hr(),

    # Calibration notice. The app and ranking logic are final; the preseason
    # priors feeding the projections are still being tuned. Placed full-width at
    # the top of the tab (not buried in the sidebar) so it is unmissable.
    shiny::div(
      class = "alert alert-warning",
      role  = "alert",
      shiny::tags$strong("Projections are being calibrated. "),
      paste(
        "The dashboard and ranking logic are final. Preseason priors are still",
        "being tuned, so some rookies, high-target veterans, and the boom/bust",
        "probabilities are currently mispriced. Known and tracked."
      )
    ),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Filters",
        width = "260px",
        shiny::selectInput(
          ns("league"), "League",
          choices  = NULL,          # populated server-side from the data
          selected = NULL
        ),
        shiny::selectInput(
          ns("position"), "Position",
          choices  = PROJTAB_POSITIONS,
          selected = "All"
        ),
        shiny::hr(),
        shiny::helpText(
          "Proj is the posterior point estimate. Floor and Ceiling are the",
          "lower and upper bounds of the 80% interval. Use the search box to",
          "find a player by name."
        )
      ),
      bslib::card(
        full_screen = TRUE,
        DT::DTOutput(ns("table"))
      )
    )
  )
}

# ------------------------------------------------------------------------------
# projectionsServer
# ------------------------------------------------------------------------------

#' Tab 1 server: filter and render the projection table
#'
#' Populates the league selector from the available leagues, filters the vorp
#' data by the selected league and position, and renders the DT table. Reads
#' data only through the reactive accessors passed in.
#'
#' @param id Character. Module namespace id (matched in projectionsUI()).
#' @param current_vorp Reactive returning the vorp tibble (R/33 output), or NULL
#'   if the cache is missing. The single source of player projections for Tab 1.
#' @param current_start_sit Reactive returning the start/sit UQ tibble (R/36
#'   output), or NULL. Accepted to establish the reactive-accessor seam for the
#'   multi-user upgrade. Intentionally unused in Tab 1 -- do not remove.
#' @return Invisibly NULL. Called for its side effect of wiring the module.
projectionsServer <- function(id, current_vorp, current_start_sit) {
  shiny::moduleServer(id, function(input, output, session) {

    # current_start_sit is part of the accessor seam only. Tab 1 does not
    # surface start/sit data (its home is the Waiver / Start-Sit tab). Keeping
    # the argument here means the multi-user upgrade touches app.R's accessor
    # definitions, not this module. Force the reference so linters and future
    # readers see the seam is deliberate.
    force(current_start_sit)

    # ---- Populate the league selector from the user's available leagues ------
    # Fires when current_vorp changes. For single-user this runs once at init.
    # For multi-user it re-fires when the logged-in user's data changes, so the
    # dropdown always reflects exactly the leagues that user has.
    shiny::observe({
      df <- current_vorp()
      if (is.null(df) || nrow(df) == 0L) {
        return()
      }
      leagues <- sort(unique(df$league_name))
      shiny::updateSelectInput(
        session, "league",
        choices  = leagues,
        selected = leagues[1]
      )
    })

    # ---- Filtered data -------------------------------------------------------
    filtered <- shiny::reactive({
      df <- current_vorp()
      shiny::req(df)
      shiny::req(input$league)

      df <- dplyr::filter(df, .data$league_name == input$league)

      if (!is.null(input$position) && input$position != "All") {
        df <- dplyr::filter(df, .data$position == input$position)
      }

      # Drop rows without a projection so the table never shows blank Proj cells.
      dplyr::filter(df, !is.na(.data$r32_posterior_mu))
    })

    # ---- Render the projection table -----------------------------------------
    output$table <- DT::renderDT({

      df0 <- current_vorp()
      shiny::validate(
        shiny::need(
          !is.null(df0),
          "VORP rankings are not loaded. Run R/33 to build the cache, then relaunch."
        )
      )

      df <- filtered()
      shiny::validate(
        shiny::need(
          nrow(df) > 0L,
          "No players match these filters. Try another league or position."
        )
      )

      display <- df |>
        dplyr::arrange(.data$overall_rank) |>
        dplyr::transmute(
          Overall    = .data$overall_rank,
          `Pos Rank` = .data$position_rank,
          Player     = .data$player_name,
          Team       = .data$team,
          Pos        = .data$position,
          Proj       = .data$r32_posterior_mu,
          Floor      = .data$r32_projection_lower_80,
          Ceiling    = .data$r32_projection_upper_80,
          `Boom %`   = .data$boom_probability,
          `Bust %`   = .data$bust_probability,
          VORP       = .data$adjusted_vorp
        )

      # Center every column except Player (index 2, 0-indexed).
      center_targets <- c(0, 1, 3, 4, 5, 6, 7, 8, 9, 10)

      DT::datatable(
        display,
        rownames = FALSE,
        class    = "stripe hover compact",
        options  = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100),
          order      = list(list(0, "asc")),   # default sort: Overall ascending
          searching  = TRUE,
          scrollX    = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = center_targets)
          )
        )
      ) |>
        DT::formatRound(c("Proj", "Floor", "Ceiling", "VORP"), digits = 1) |>
        DT::formatPercentage(c("Boom %", "Bust %"), digits = 0)
    })

    invisible(NULL)
  })
}
