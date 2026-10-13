# ==============================================================================
# shiny_dfs/app.R
#
# Daily (DFS) lineup app, v1: FanDuel classic.  [2026-10-09]
#
# A local Shiny surface over R/65. It recomputes nothing upstream: it reads the
# salary CSV and the chain output that run_dfs_chain() already wrote, and calls
# R/65's own parser, matcher, eligibility rule, lock check and solver. The
# console optimizer (run_dfs_optimizer) and this app always agree because they
# run the same functions.
#
# DESIGN (forks locked 2026-09-30)
#   5 C  Locks and excludes are clicked in the app (select rows, press Lock or
#        Exclude). The lock set is checked live for legality before Solve.
#   9 B  Questionable players are excluded by default; one checkbox includes
#        them. A locked questionable player is always allowed.
#  12 B  Standalone folder. On start it loads the newest CSV in
#        data/dfs_salaries/ (name_overrides.csv excluded); an upload button
#        uses a different file for this session only. Nothing is written to
#        your folders by the app; the lineup downloads through the browser.
#
# THE WEEK
#   Taken from the salary file: the app finds the one 2026 regular-season week
#   that contains every game on the slate and stops if there is not exactly
#   one. The chain output for that week must already exist
#   (run_dfs_chain(week = N)); if it does not, the app says so.
#
# NEEDS
#   shiny, DT (plus everything R/65 needs: dplyr, tidyr, purrr, tibble, readr,
#   glue, here, nflreadr, lpSolve).
#
# RUN
#   rm(list = ls())
#   shiny::runApp(here::here("shiny_dfs"))
# ==============================================================================

library(shiny)
library(DT)
library(dplyr)
library(glue)
library(here)
library(readr)

source(here::here("R", "65_daily_optimizer.R"))

APP_SITE   <- "fd"
APP_SEASON <- 2026L
APP_CFG    <- .r65_site(APP_SITE)
APP_POSITIONS <- c("QB", "RB", "WR", "TE", "DEF")

# ------------------------------------------------------------------------------
# Helpers (app-only; everything model-side comes from R/65)
# ------------------------------------------------------------------------------

# The one REG week containing every slate game; stops otherwise.
.app_infer_week <- function(sal, season) {
  sch <- .r65_load_schedules(season) %>%
    dplyr::filter(.data$game_type == "REG") %>%
    dplyr::mutate(game = paste(.r65_clean_abbr(away_team), "@", .r65_clean_abbr(home_team)))
  games <- unique(sal$game)
  wk <- sch %>%
    dplyr::filter(game %in% games) %>%
    dplyr::count(week) %>%
    dplyr::filter(n == length(games)) %>%
    dplyr::pull(week)
  if (length(wk) != 1L) {
    stop(glue("The salary file's {length(games)} games do not all fall in one ",
              "{season} regular-season week. Check that this is a classic main-slate file."),
         call. = FALSE)
  }
  as.integer(wk)
}

.app_age <- function(t) {
  h <- as.numeric(difftime(Sys.time(), t, units = "hours"))
  if (!is.finite(h)) return("unknown")
  if (h < 1) return(glue("{round(h * 60)} min ago"))
  glue("{round(h, 1)} h ago")
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(glue("Daily optimizer: {APP_CFG$name} classic")),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Slate"),
      uiOutput("slate_info"),
      fileInput("upload", "Use a different salary CSV (this session only)",
                accept = ".csv"),
      hr(),
      h4("Pool"),
      checkboxInput("include_q", "Include questionable players", value = FALSE),
      checkboxGroupInput("pos", "Positions", choices = APP_POSITIONS,
                         selected = APP_POSITIONS, inline = TRUE),
      selectInput("team", "Team", choices = "All", selected = "All"),
      radioButtons("show", "Show",
                   choices = c("Eligible" = "eligible",
                               "All matched" = "matched",
                               "Everything" = "all"),
                   selected = "eligible", inline = TRUE),
      hr(),
      h4("Locks and excludes"),
      helpText("Select rows in the Pool tab, then:"),
      actionButton("lock", "Lock"),
      actionButton("exclude", "Exclude"),
      actionButton("clear_sel", "Clear selected"),
      br(), br(),
      actionButton("clear_all", "Clear all locks and excludes"),
      br(), br(),
      uiOutput("lock_summary"),
      uiOutput("feasibility"),
      hr(),
      actionButton("solve", "Solve lineup", class = "btn-primary", width = "100%")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel("Pool", br(), DTOutput("pool_tbl")),
        tabPanel("Lineup", br(), uiOutput("lineup_msg"), DTOutput("lineup_tbl"),
                 br(), uiOutput("lineup_totals"),
                 downloadButton("dl_lineup", "Download lineup CSV")),
        tabPanel("Unmatched", br(),
                 helpText(glue("Priced players with no projection. Add any that ",
                               "should be in play to data/dfs_salaries/{basename(R65_OVERRIDE_PATH)} ",
                               "(columns site, site_name, team, nfl_gsis_id) and restart the app.")),
                 DTOutput("unmatched_tbl")),
        tabPanel("Excluded", br(),
                 helpText("Matched players the solver will not use, and why."),
                 DTOutput("excluded_tbl"))
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(locks = character(0), excl = character(0),
                       result = NULL, problems = character(0), stale = TRUE)

  salary_path <- reactive({
    if (!is.null(input$upload)) input$upload$datapath else newest_salary_file()
  })
  salary_label <- reactive({
    if (!is.null(input$upload)) paste0(input$upload$name, " (uploaded)")
    else basename(salary_path())
  })

  # Salary file + chain output + matching. Independent of the Q toggle.
  base <- reactive({
    path <- salary_path()
    sal  <- read_fd_salaries(path)
    wk   <- .app_infer_week(sal, APP_SEASON)
    bands_rds <- file.path(R65_OUTPUT_DIR,
                           sprintf("%sr59_weekly_bands_w%02d.rds", APP_CFG$prefix, wk))
    def_rds   <- .r65_path(APP_CFG, "def_st", wk, "rds")
    for (f in c(bands_rds, def_rds)) {
      if (!file.exists(f)) {
        stop(glue("Chain output for week {wk} not found ({basename(f)}). ",
                  "Run run_dfs_chain(week = {wk}) first."), call. = FALSE)
      }
    }
    bands <- readRDS(bands_rds)
    def   <- readRDS(def_rds)
    ov    <- .r65_read_overrides(APP_SITE)
    list(week = wk,
         pool = .r65_match(sal, bands, def, ov),
         n_games = dplyr::n_distinct(sal$game),
         bands_built = max(bands$built_at),
         lines_pulled = max(def$lines_pulled_at, na.rm = TRUE),
         n_overrides = nrow(ov))
  })

  # Eligibility depends on the Q toggle.
  pool <- reactive({
    .r65_eligibility(base()$pool, include_q = isTRUE(input$include_q))
  })

  # New slate: reset everything.
  observeEvent(base(), {
    rv$locks <- character(0); rv$excl <- character(0)
    rv$result <- NULL; rv$problems <- character(0); rv$stale <- TRUE
    teams <- sort(unique(base()$pool$team))
    updateSelectInput(session, "team", choices = c("All", teams), selected = "All")
  })

  # Anything that changes the problem makes a solved lineup stale.
  observeEvent(list(pool(), rv$locks, rv$excl), { rv$stale <- TRUE }, ignoreInit = TRUE)

  # Rows shown in the Pool tab (selection indices refer to these rows).
  shown <- reactive({
    P <- pool() %>%
      dplyr::mutate(status = dplyr::case_when(
        !is.na(player_uid) & player_uid %in% rv$locks ~ "LOCK",
        !is.na(player_uid) & player_uid %in% rv$excl  ~ "EXCLUDE",
        TRUE ~ ""))
    P <- switch(input$show,
                eligible = P %>% dplyr::filter(eligible | status != ""),
                matched  = P %>% dplyr::filter(match_status %in% c("matched", "override", "dst")),
                all      = P)
    if (length(input$pos) > 0) {
      P <- P %>% dplyr::filter(is.na(position) | position %in% input$pos)
    }
    if (!is.null(input$team) && input$team != "All") {
      P <- P %>% dplyr::filter(team == input$team)
    }
    P %>% dplyr::arrange(dplyr::desc(status != ""), dplyr::desc(proj))
  })

  # ---- sidebar text ----
  output$slate_info <- renderUI({
    b <- base()
    tagList(
      tags$div(tags$b("File: "), salary_label()),
      tags$div(tags$b("Week: "), b$week, glue(" ({b$n_games} games)")),
      tags$div(tags$b("Projections built: "), format(b$bands_built, "%a %m-%d %H:%M"),
               glue(" ({.app_age(b$bands_built)})")),
      tags$div(tags$b("Vegas lines pulled: "), format(b$lines_pulled, "%a %m-%d %H:%M")),
      tags$div(tags$b("Name overrides: "), b$n_overrides),
      tags$div(tags$b("Cap: "), paste0("$", format(APP_CFG$cap, big.mark = ",")))
    )
  })

  output$lock_summary <- renderUI({
    P <- pool()
    nm <- function(ids) {
      if (length(ids) == 0) return("none")
      paste(P$site_name[match(ids, P$player_uid)], collapse = ", ")
    }
    tagList(tags$div(tags$b("Locked: "), nm(rv$locks)),
            tags$div(tags$b("Excluded: "), nm(rv$excl)))
  })

  lock_idx <- reactive({ i <- match(rv$locks, pool()$player_uid); i[!is.na(i)] })
  excl_idx <- reactive({ i <- match(rv$excl,  pool()$player_uid); i[!is.na(i)] })

  output$feasibility <- renderUI({
    probs <- .r65_lock_problems(pool(), lock_idx(), APP_CFG)
    if (length(probs) == 0) {
      if (length(rv$locks) == 0) return(NULL)
      return(tags$div(style = "color:#2e7d32;", "Locks are legal."))
    }
    tags$div(style = "color:#c62828;",
             tags$b("Locks cannot form a legal lineup:"),
             tags$ul(lapply(probs, tags$li)))
  })

  # ---- lock / exclude buttons ----
  selected_ids <- reactive({
    s <- input$pool_tbl_rows_selected
    if (length(s) == 0) return(character(0))
    ids <- shown()$player_uid[s]
    if (any(is.na(ids))) {
      showNotification("Rows with no projection cannot be locked or excluded; they were skipped.",
                       type = "warning")
    }
    ids[!is.na(ids)]
  })
  observeEvent(input$lock, {
    ids <- selected_ids()
    rv$locks <- union(rv$locks, ids); rv$excl <- setdiff(rv$excl, ids)
  })
  observeEvent(input$exclude, {
    ids <- selected_ids()
    rv$excl <- union(rv$excl, ids); rv$locks <- setdiff(rv$locks, ids)
  })
  observeEvent(input$clear_sel, {
    ids <- selected_ids()
    rv$locks <- setdiff(rv$locks, ids); rv$excl <- setdiff(rv$excl, ids)
  })
  observeEvent(input$clear_all, { rv$locks <- character(0); rv$excl <- character(0) })

  # ---- solve ----
  observeEvent(input$solve, {
    P <- pool()
    li <- lock_idx(); ei <- excl_idx()
    probs <- .r65_lock_problems(P, li, APP_CFG)
    if (length(probs) > 0) {
      rv$problems <- probs; rv$result <- NULL; rv$stale <- FALSE
    } else {
      res <- solve_dfs_lineup(P, APP_CFG, li, ei)
      if (res$status != 0) {
        rv$problems <- paste("No legal lineup exists with these locks and excludes.",
                             "Loosen a lock, remove an exclude, or include questionable players.")
        rv$result <- NULL
      } else {
        rv$problems <- character(0); rv$result <- res
      }
      rv$stale <- FALSE
    }
    updateTabsetPanel(session, "tabs", selected = "Lineup")
  })

  # ---- tables ----
  output$pool_tbl <- renderDT({
    d <- shown() %>%
      dplyr::transmute(status, name = site_name, pos = position, team, opp = opponent,
                       salary, proj = round(proj, 2), value = round(value, 2),
                       availability, excluded = excluded_reason, match = match_status,
                       site_proj = file_proj)
    datatable(d, rownames = FALSE, selection = "multiple",
              options = list(pageLength = 25, scrollX = TRUE, order = list())) %>%
      formatCurrency("salary", digits = 0) %>%
      formatStyle("status", target = "row",
                  backgroundColor = styleEqual(c("LOCK", "EXCLUDE"), c("#e3f2fd", "#fdecea")))
  })

  output$lineup_msg <- renderUI({
    if (length(rv$problems) > 0) {
      return(tags$div(style = "color:#c62828;", tags$b("Not solved: "),
                      tags$ul(lapply(rv$problems, tags$li))))
    }
    if (is.null(rv$result)) return(helpText("Press Solve lineup."))
    if (rv$stale) {
      return(tags$div(style = "color:#ef6c00;",
                      "Locks, excludes or the pool changed since this lineup was solved. Press Solve lineup again."))
    }
    NULL
  })

  lineup_df <- reactive({
    req(rv$result)
    rv$result$lineup %>%
      dplyr::transmute(slot, name = site_name, pos = position, team, opp = opponent,
                       salary, proj = round(proj, 2), value = round(value, 2),
                       locked, availability)
  })

  output$lineup_tbl <- renderDT({
    datatable(lineup_df(), rownames = FALSE, selection = "none",
              options = list(dom = "t", pageLength = 9, ordering = FALSE)) %>%
      formatCurrency("salary", digits = 0)
  })

  output$lineup_totals <- renderUI({
    req(rv$result)
    r <- rv$result
    tags$p(tags$b(glue("Projected {round(r$total_proj, 2)}")),
           glue(" | salary ${format(r$total_salary, big.mark = ',')} of ",
                "${format(APP_CFG$cap, big.mark = ',')} ",
                "(${format(APP_CFG$cap - r$total_salary, big.mark = ',')} left) | ",
                "teams {dplyr::n_distinct(r$lineup$team)}"))
  })

  output$dl_lineup <- downloadHandler(
    filename = function() sprintf("%s_lineup_w%02d.csv", APP_SITE, base()$week),
    content  = function(file) readr::write_csv(lineup_df(), file)
  )

  output$unmatched_tbl <- renderDT({
    d <- pool() %>%
      dplyr::filter(match_status %in% c("no_projection", "ambiguous",
                                        "override_not_found", "dst_no_projection")) %>%
      dplyr::arrange(dplyr::desc(salary)) %>%
      dplyr::transmute(name = site_name, team, opp = opponent, salary,
                       match = match_status, site_proj = file_proj)
    datatable(d, rownames = FALSE, selection = "none",
              options = list(pageLength = 25)) %>%
      formatCurrency("salary", digits = 0)
  })

  output$excluded_tbl <- renderDT({
    d <- pool() %>%
      dplyr::filter(match_status %in% c("matched", "override", "dst"), !eligible) %>%
      dplyr::arrange(excluded_reason, dplyr::desc(proj)) %>%
      dplyr::transmute(name = site_name, pos = position, team, salary,
                       proj = round(proj, 2), reason = excluded_reason, availability)
    datatable(d, rownames = FALSE, selection = "none",
              options = list(pageLength = 25)) %>%
      formatCurrency("salary", digits = 0)
  })
}

shinyApp(ui, server)
