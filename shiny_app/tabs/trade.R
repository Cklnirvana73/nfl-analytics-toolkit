# ==============================================================================
# NFL Analytics Toolkit | Season 2, Phase 5
# Shiny Tab 3: Trade Evaluator
# File: shiny_app/tabs/trade.R
#
# PURPOSE
# -------
# Tab 3 of the Phase 5 Shiny app. The user enters their Sleeper username and
# loads their leagues; the league selector then offers only the leagues that
# have cached R/37 trade values behind them. The user assembles a proposed
# trade (players to give, players to receive) within a chosen league and the
# tab reports each side's summed trade value, the net delta from the receiving
# side's perspective, and a verdict (WIN / LOSS / ROUGHLY EVEN), with a
# per-player breakdown. The core evaluator is evaluate_trade() from R/37
# (extracted into shiny_app/R/trade_helpers.R), a pure function.
#
# SOURCED BY
# ----------
# shiny_app/app.R. This file contains PURE DEFINITIONS ONLY. Sourcing it just
# defines tradeUI() and tradeServer(). The libraries (shiny, bslib, DT, dplyr,
# glue), evaluate_trade(), and the Sleeper API functions (get_user_leagues(),
# the `%||%` helper) are attached/sourced by app.R; calls here are namespaced
# where a namespace exists.
#
# DATA + LIVE-CALL CONTRACT
# -------------------------
# Trade VALUES are cache-read only. They come through current_trade_values()
# (app_data$trade_values, R/37 build_trade_value_table() output, cached at
# data/season2_cache/s2_week17_trade_values.rds), columns:
#   league_name, nfl_gsis_id, player_name, position, trade_value, tv_tier
#
# The ONE live call in this tab is get_user_leagues(username, season), made
# only when the user clicks Load, used solely to decide which leagues to offer.
# It never computes trade value. A Sleeper league is selectable only if its name
# matches
# a league_name present in the cache (case- and whitespace-insensitive); the
# cache spelling is what evaluate_trade() filters on. Leagues with no cached
# values are reported as not selectable rather than shown with empty results.
#
# evaluate_trade(give_ids, receive_ids, trade_values, league_name) returns a
#   named list: give_value, receive_value, net_delta, give_detail,
#   receive_detail (tibbles of player_name, position, trade_value, tv_tier),
#   and verdict. net_delta = receive_value - give_value, so a positive delta
#   favors the side receiving the players selected on the right.
#
# SCHEMA TAG: s2_w21_tradetab_v7
# ==============================================================================

# ------------------------------------------------------------------------------
# tradeUI
# ------------------------------------------------------------------------------

#' Tab 3 UI: trade evaluator
#'
#' Returns the tab body: a short intro, a calibration note, a Sleeper username
#' box with a Load button and a status line, a league selector and a position
#' filter, two multi-select player pickers (give and receive) that search as you
#' type, an evaluate button, a verdict summary, and the two per-side detail
#' tables. All inputs and outputs are namespaced with the module id so multiple
#' instances never collide.
#'
#' @param id Character. Module namespace id (matched in tradeServer()).
#' @return A Shiny UI tag list for use inside a bslib::nav_panel().
tradeUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::h3("Trade Evaluator"),
    shiny::p(
      class = "lead text-muted",
      paste(
        "Enter your Sleeper username to load your leagues, pick the players you",
        "would give and receive, then evaluate the trade. Each side is summed",
        "on R/37 trade value and the net delta is reported from your receiving",
        "side's perspective. Powered by R/37 and R/19."
      )
    ),
    shiny::hr(),

    # Calibration note. Same caveat as the other tabs: trade value is built on
    # the preseason projection priors that are still being tuned.
    shiny::div(
      class = "alert alert-warning",
      role  = "alert",
      shiny::tags$strong("Trade values are being calibrated. "),
      paste(
        "Trade value derives from the same preseason projection priors as the",
        "Projections tab, which are still being tuned. Use the verdict as a",
        "directional read, not a precise price. Known and tracked."
      )
    ),

    # Sleeper account connection. One live get_user_leagues() call on Load.
    bslib::card(
      bslib::card_header("Connect your Sleeper account"),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(8, 4),
          shiny::textInput(
            ns("username"),
            label       = NULL,
            placeholder = "Sleeper username (not display name)"
          ),
          shiny::actionButton(
            ns("load_leagues"),
            label = "Load my leagues",
            class = "btn-secondary"
          )
        ),
        shiny::uiOutput(ns("league_status"))
      )
    ),

    # League selector and position filter, side by side. The position filter
    # narrows both pickers; leaving it empty shows all positions.
    bslib::layout_columns(
      col_widths = c(8, 4),
      shiny::selectInput(
        ns("league"),
        label   = "League",
        choices = NULL
      ),
      shiny::selectInput(
        ns("pos_filter"),
        label    = "Filter by position",
        choices  = c("QB", "RB", "WR", "TE"),
        selected = NULL,
        multiple = TRUE,
        selectize = TRUE
      )
    ),

    # Give / receive pickers, stacked full width so selected players have room
    # to display and wrap. Both search as you type (server-side selectize),
    # matching on the player name and the position in each label. The scoped
    # style caps the open option list at five roomy rows (the rest scroll) so it
    # never becomes one long crowded list, and gives the input a little height
    # for selected tags.
    shiny::tags$style(shiny::HTML(sprintf(
      paste0(
        "#%s .selectize-input, #%s .selectize-input ",
        "{ min-height: 48px; align-items: flex-start; } ",
        "#%s .selectize-dropdown-content, #%s .selectize-dropdown-content ",
        "{ max-height: 200px; overflow-y: auto; } ",
        "#%s .selectize-dropdown .option, #%s .selectize-dropdown .option ",
        "{ padding-top: 9px; padding-bottom: 9px; }"
      ),
      ns("give"), ns("receive"),
      ns("give"), ns("receive"),
      ns("give"), ns("receive")
    ))),

    bslib::layout_columns(
      col_widths = c(12, 12),

      bslib::card(
        bslib::card_header("Give"),
        bslib::card_body(
          shiny::selectizeInput(
            ns("give"),
            label    = NULL,
            choices  = NULL,
            multiple = TRUE,
            width    = "100%",
            options  = list(placeholder = "Type to search players to give")
          )
        )
      ),

      bslib::card(
        bslib::card_header("Receive"),
        bslib::card_body(
          shiny::selectizeInput(
            ns("receive"),
            label    = NULL,
            choices  = NULL,
            multiple = TRUE,
            width    = "100%",
            options  = list(placeholder = "Type to search players to receive")
          )
        )
      )
    ),

    shiny::actionButton(
      ns("evaluate"),
      label = "Evaluate trade",
      class = "btn-primary"
    ),

    shiny::hr(),

    # Verdict summary, rendered after the evaluate button is pressed.
    shiny::uiOutput(ns("verdict_summary")),

    # Per-side detail tables.
    bslib::layout_columns(
      col_widths = c(6, 6),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Give"),
        DT::DTOutput(ns("give_table"))
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Receive"),
        DT::DTOutput(ns("receive_table"))
      )
    )
  )
}

# ------------------------------------------------------------------------------
# tradeServer
# ------------------------------------------------------------------------------

#' Tab 3 server: load leagues, populate pickers, and evaluate the trade
#'
#' On Load, calls get_user_leagues() once for the entered username and offers
#' only the leagues that have cached R/37 trade values. Within the selected
#' league it populates the give/receive pickers, and on the evaluate button it
#' calls the pure evaluate_trade() and renders the verdict and the two per-side
#' detail tables. Trade values are read only through current_trade_values().
#'
#' @param id Character. Module namespace id (matched in tradeUI()).
#' @param current_trade_values Reactive returning the R/37 trade value tibble,
#'   or NULL if the cache is missing. The single source for player choices and
#'   the values summed by evaluate_trade(), and the set a Sleeper league must
#'   match to be selectable.
#' @param app_season Integer. The NFL season the analytics stack and the R/37
#'   cache were built for (app.R's SEASON constant). Passed to get_user_leagues()
#'   so the league lookup queries the same season as the cached values, rather
#'   than falling back to R/19's CURRENT_NFL_SEASON default.
#' @return Invisibly NULL. Called for its side effect of wiring the module.
tradeServer <- function(id, current_trade_values, app_season) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- Live league lookup from the entered username ------------------------
    # State is held in reactiveValues rather than an eventReactive: the whole
    # load happens in one button observer, and updateSelectInput is called in
    # the same handler. This avoids the lazy-eventReactive-read-inside-observer
    # pattern, which can leave the dropdown unpopulated even when the value
    # behind it is correct.
    rv <- shiny::reactiveValues(leagues = NULL, loaded = FALSE)

    # ---- Leagues that have cached trade values (the selectable set) ----------
    # Returns the CACHE spelling of league_name (what evaluate_trade() filters
    # on) for any of the user's Sleeper leagues whose name matches a cached
    # league, compared case- and whitespace-insensitively.
    matched_leagues <- shiny::reactive({
      lg <- rv$leagues
      tv <- current_trade_values()
      if (is.null(lg) || nrow(lg) == 0L || is.null(tv) || nrow(tv) == 0L) {
        return(character(0))
      }
      cache_names <- unique(tv$league_name)
      norm <- function(x) tolower(trimws(x))
      cache_names[norm(cache_names) %in% norm(lg$name)]
    })

    # ---- Load button: fetch leagues and populate the selector ----------------
    # One live Sleeper call, gated behind the button. get_user_leagues() warns
    # and returns NULL on an unknown user or no leagues; that NULL is stored and
    # surfaced by the status line below.
    shiny::observeEvent(input$load_leagues, {
      username <- trimws(input$username %||% "")
      if (nchar(username) == 0L) {
        rv$leagues <- NULL
        rv$loaded  <- TRUE
        shiny::updateSelectInput(session, "league",
                                 choices = character(0), selected = character(0))
        return(invisible(NULL))
      }

      lg <- shiny::withProgress(
        message = "Looking up your Sleeper leagues...",
        value   = 0.5,
        {
          get_user_leagues(username, season = app_season)
        }
      )
      rv$leagues <- lg
      rv$loaded  <- TRUE

      matched <- matched_leagues()
      shiny::updateSelectInput(
        session, "league",
        choices  = matched,
        selected = if (length(matched) > 0L) matched[[1]] else character(0)
      )
    }, ignoreInit = TRUE)

    # ---- Status line under the username box ----------------------------------
    output$league_status <- shiny::renderUI({
      if (!rv$loaded) return(NULL)
      lg <- rv$leagues
      if (is.null(lg)) {
        return(shiny::div(
          class = "alert alert-danger",
          paste(
            "Could not find that user, or no leagues for the current season.",
            "Use your Sleeper username, not your display name."
          )
        ))
      }
      n_found <- nrow(lg)
      n_match <- length(matched_leagues())
      if (n_match > 0L) {
        shiny::div(
          class = "alert alert-success",
          sprintf(
            "Found %d league(s). %d have cached trade values and are selectable below.",
            n_found, n_match
          )
        )
      } else {
        shiny::div(
          class = "alert alert-warning",
          sprintf(
            paste(
              "Found %d league(s), but none have cached trade values yet.",
              "Build the R/37 trade value cache for them, then reload."
            ),
            n_found
          )
        )
      }
    })

    # ---- Player choices for the selected league ------------------------------
    # Named character vectors: names are the display labels, values are the
    # nfl_gsis_id that evaluate_trade() consumes. Position is folded into the
    # label to disambiguate same-name players and to let the type-ahead match on
    # position as well as name.

    # Every player in the selected league, all positions. Used to recover the
    # label for an already-selected player that the position filter hides.
    all_league_choices <- shiny::reactive({
      tv <- current_trade_values()
      shiny::req(tv, input$league)
      slice <- tv |>
        dplyr::filter(.data$league_name == input$league) |>
        dplyr::arrange(dplyr::desc(.data$trade_value))
      stats::setNames(
        slice$nfl_gsis_id,
        paste0(slice$player_name, " (", slice$position, ")")
      )
    })

    # The same set narrowed to the selected positions. An empty filter means
    # all positions.
    filtered_league_choices <- shiny::reactive({
      tv <- current_trade_values()
      shiny::req(tv, input$league)
      slice <- dplyr::filter(tv, .data$league_name == input$league)
      pos <- input$pos_filter
      if (length(pos) > 0L) {
        slice <- dplyr::filter(slice, .data$position %in% pos)
      }
      slice <- dplyr::arrange(slice, dplyr::desc(.data$trade_value))
      stats::setNames(
        slice$nfl_gsis_id,
        paste0(slice$player_name, " (", slice$position, ")")
      )
    })

    # Update one picker to the filtered choices while keeping any already-
    # selected players selectable, so a position filter never silently drops a
    # player the user already put in the trade.
    update_picker <- function(input_id) {
      choices  <- filtered_league_choices()
      selected <- input[[input_id]]
      if (length(selected) > 0L) {
        missing <- selected[!(selected %in% choices)]
        if (length(missing) > 0L) {
          all_ch  <- all_league_choices()
          choices <- c(choices, all_ch[all_ch %in% missing])
        }
      }
      shiny::updateSelectizeInput(
        session, input_id,
        choices  = choices,
        selected = selected,
        server   = TRUE
      )
    }

    # On a league change, clear both pickers (a player from a prior league must
    # never carry over as an unknown id) and set the choices to the new league
    # under the current position filter.
    shiny::observeEvent(input$league, {
      choices <- filtered_league_choices()
      shiny::updateSelectizeInput(
        session, "give",
        choices = choices, selected = character(0), server = TRUE
      )
      shiny::updateSelectizeInput(
        session, "receive",
        choices = choices, selected = character(0), server = TRUE
      )
    })

    # On a position-filter change, re-narrow both pickers but preserve the
    # current selections.
    shiny::observeEvent(input$pos_filter, {
      update_picker("give")
      update_picker("receive")
    }, ignoreNULL = FALSE)

    # ---- Evaluate the trade on button press ----------------------------------
    result <- shiny::eventReactive(input$evaluate, {
      tv <- current_trade_values()
      shiny::validate(
        shiny::need(
          !is.null(tv),
          paste(
            "Trade values are not loaded. Run R/37 with save_output = TRUE to",
            "build the cache, then relaunch."
          )
        )
      )
      shiny::validate(
        shiny::need(
          shiny::isTruthy(input$league),
          "Load your leagues and select one before evaluating."
        )
      )
      shiny::validate(
        shiny::need(
          length(input$give) > 0L || length(input$receive) > 0L,
          "Select at least one player on either side, then evaluate."
        )
      )

      evaluate_trade(
        give_ids     = input$give    %||% character(0),
        receive_ids  = input$receive %||% character(0),
        trade_values = tv,
        league_name  = input$league
      )
    })

    # ---- Verdict summary -----------------------------------------------------
    output$verdict_summary <- shiny::renderUI({
      res <- result()

      verdict_class <- switch(
        res$verdict,
        "WIN"          = "alert alert-success",
        "LOSS"         = "alert alert-danger",
        "ROUGHLY EVEN" = "alert alert-secondary",
        "alert alert-secondary"
      )

      headline <- switch(
        res$verdict,
        "WIN"          = "This trade is a WIN for you.",
        "LOSS"         = "This trade is a LOSS for you.",
        "ROUGHLY EVEN" = "This trade is roughly even.",
        "This trade is roughly even."
      )

      gap <- abs(res$net_delta)
      detail_line <- switch(
        res$verdict,
        "WIN"  = sprintf(
          "You give up %.1f in value and get back %.1f, coming out %.1f ahead.",
          res$give_value, res$receive_value, gap
        ),
        "LOSS" = sprintf(
          "You give up %.1f in value and get back %.1f, coming out %.1f behind.",
          res$give_value, res$receive_value, gap
        ),
        sprintf(
          "You give up %.1f in value and get back %.1f, within %.1f of even.",
          res$give_value, res$receive_value, gap
        )
      )

      shiny::tagList(
        shiny::div(
          class = verdict_class,
          role  = "alert",
          shiny::tags$strong(headline)
        ),
        shiny::div(
          class = "text-muted",
          detail_line
        )
      )
    })

    # ---- Per-side detail tables ----------------------------------------------
    render_side <- function(detail) {
      display <- detail |>
        dplyr::arrange(dplyr::desc(.data$trade_value)) |>
        dplyr::transmute(
          Player = .data$player_name,
          Pos    = .data$position,
          Value  = .data$trade_value,
          Tier   = .data$tv_tier
        )

      # Center every column except Player (0-indexed column 0).
      center_targets <- c(1, 2, 3)

      DT::datatable(
        display,
        rownames = FALSE,
        class    = "stripe hover compact",
        options  = list(
          paging     = FALSE,
          searching  = FALSE,
          info       = FALSE,
          ordering   = FALSE,
          scrollX    = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = center_targets)
          )
        )
      ) |>
        DT::formatRound("Value", digits = 1)
    }

    output$give_table <- DT::renderDT({
      render_side(result()$give_detail)
    })

    output$receive_table <- DT::renderDT({
      render_side(result()$receive_detail)
    })

    invisible(NULL)
  })
}
