# ==============================================================================
# 60_projection_explorer_app.R
#
# Local Shiny surface over R/59's weekly projection bands.  [2026-09-18]
#
# Same contract as R/57 over R/56: reads what R/59 wrote, recomputes nothing.
# Every s3_r59_weekly_bands_wNN.csv in data/season3_cache is loaded, so a
# player's band can be followed across weeks as the files accumulate.
#
# TABS
#   Table    filtered rows, sortable, every R/59 column available
#   Bands    dot-and-range chart of the current filter: low (p10) to p95 with
#            the mean marked, ordered by mean, colored by availability
#   Trend    one or more players' mu / low / high across weeks
#   Summary  counts and medians by position for the current filter
#   Dictionary  every R/59 column: type, source, meaning; searchable table
#            embedded below (R60_DICTIONARY), no external file
#
# FILTERS (all combine; sidebar)
#   week, position, team, opponent, home/away, availability, playable only,
#   avail_source, prior_source, ytd_weight_mode, ytd_n range, mu / low / high /
#   p95 / sigma ranges, band width range, name search, pasted name list,
#   top-N-per-position cap
#
# USAGE
#   source(here::here("R", "60_projection_explorer_app.R"))
#   run_projection_explorer()
#
# Needs: shiny, DT, ggplot2 (plus dplyr, readr, glue).
# Brand palette per project memory: green #1f6f3f, dark #13311d, cream
# #f4ead0, gold #e6b53c.
# ==============================================================================

library(dplyr)
library(readr)
library(glue)

R60_INPUT_DIR <- here::here("data", "season3_cache")
R60_FILE_GLOB <- "^s3_r59_weekly_bands_w[0-9]{2}\\.csv$"
R60_POSITIONS <- c("QB", "RB", "WR", "TE")

R60_GREEN <- "#1f6f3f"
R60_DARK  <- "#13311d"
R60_CREAM <- "#f4ead0"
R60_GOLD  <- "#e6b53c"

# ------------------------------------------------------------------------------
# Data dictionary for the R/59 band files (schema s3_r59_bands_v3)
# ------------------------------------------------------------------------------
R60_DICTIONARY <- tibble::tribble(
  ~group, ~column, ~type, ~source, ~meaning,
  "Identity", "season", "int", "R/59 argument", "NFL season.",
  "Identity", "week", "int", "R/59 argument", "Week the band is for. Filename carries the same number.",
  "Identity", "player_uid", "chr", "R/29", "Pipeline-wide never-NA identity key. Real gsis id where one exists, otherwise a synthetic rookie key (rk_<name>_<year>_<pos>). Join on this, not on name.",
  "Identity", "nfl_gsis_id", "chr", "R/29", "nflfastR / nflreadr player id. NA for the id-less rookie cohort.",
  "Identity", "player_name", "chr", "R/29 roster", "Display name from the current-season roster.",
  "Identity", "position", "chr", "R/29", "QB, RB, WR or TE. Other positions are not in this file.",
  "Identity", "team", "chr", "R/29 roster", "Current team abbreviation.",

  "Schedule", "opponent", "chr", "nflreadr schedule", "This week's opponent. NA on a bye or if the schedule could not load.",
  "Schedule", "home", "lgl", "nflreadr schedule", "TRUE at home, FALSE away, NA on a bye. Shown in the app as home_away.",
  "Schedule", "on_bye", "lgl", "nflreadr schedule", "TRUE when the team has no regular-season game this week. Every band column is set to 0 for bye rows.",

  "Availability", "availability", "chr", "derived", "Final call for the week: active, questionable, out_week, out_for_season, bye, or status:<value> for a Sleeper status the file does not map (carried verbatim on purpose so unknown designations are visible).",
  "Availability", "playable", "lgl", "derived", "TRUE when availability is active or questionable. The gate to apply before treating a row as a candidate.",
  "Availability", "avail_source", "chr", "derived", "Where the call came from: manual_override (override CSV), inferred (Sleeper and/or injury report), schedule (bye), no_feed (nothing matched; defaults to active).",
  "Availability", "availability_inferred", "chr", "derived", "What the feeds said before any manual override. Equals availability unless an override applied.",
  "Availability", "sleeper_status", "chr", "Sleeper player DB", "Sleeper roster status, e.g. Active, Inactive. NA if the player did not match.",
  "Availability", "sleeper_injury", "chr", "Sleeper player DB", "Sleeper injury designation: Questionable, Doubtful, Out, IR. NA when none.",
  "Availability", "sleeper_match", "chr", "derived", "How the Sleeper row was found: gsis (id join), name_position (fallback; Sleeper's gsis field is sparse), NA (unmatched).",
  "Availability", "report_status", "chr", "nflreadr injuries", "Official weekly injury report status: Out, Doubtful, Questionable. NA when not listed or not matched.",
  "Availability", "practice_status", "chr", "nflreadr injuries", "Practice participation from the same report, when present.",
  "Availability", "override_return_week", "int", "override CSV", "return_week from the manual override row, if given.",
  "Availability", "(precedence)", "", "derived", "Highest first: bye; manual override; Sleeper out-for-season status; Sleeper Inactive with IR/PUP/NFI/Sus; any other non-Active Sleeper status (verbatim); Sleeper Out/IR/Doubtful; report Out/Doubtful; Questionable from either feed; else active.",
  "Availability", "(override file)", "", "data/season3_cache/availability_overrides.csv", "Columns gsis_id, status (active / out_week / out_for_season), optional return_week. Same contract as R/35 load_availability(). Final word.",

  "Provenance", "prior_source", "chr", "R/29", "Branch that built the preseason prior: translation_active (college translation + history), translation_capped_rookie (rookie, translation only), veteran_history_only, phased_out_history_only, phaseout_stale_history (history older than the normal window), phaseout_no_history_fallback (no usable history; position-average prior, the weakest branch).",
  "Provenance", "ytd_n", "int", "R/29", "Games played this season that entered the season-to-date update. 0 = still the preseason prior. Played games only; a DNP is not a zero.",
  "Provenance", "ytd_weight_mode", "chr", "R/29", "Rule that blended prior with season-to-date for this row. games: empirical-Bayes weight ytd_n / (ytd_n + k), k gated by R/58 (currently RB and WR). linear: calendar-week schedule (QB, TE, and any position where the gate did not pass).",

  "Band", "mu", "dbl", "R/32 r32_posterior_mu", "Expected points per game. R/29 posterior reconciled with R/31 volume allocation; QB1s anchored to R/30 team volume. NOT opponent-adjusted (R/35 offense matchup factor is neutralized on gate evidence).",
  "Band", "sigma_wk", "dbl", "R/32 r32_sigma_week", "One-week predictive SD, sqrt(tau^2 + omega^2): uncertainty in the mean plus week-to-week dispersion. Omega carries most of it.",
  "Band", "low", "dbl", "R/50 quantile at p_low", "Floor, 10th percentile: mu + sigma_wk * z_position(0.10) on the empirical per-position shape.",
  "Band", "q1", "dbl", "R/50 quantile at 0.25", "25th percentile.",
  "Band", "q3", "dbl", "R/50 quantile at 0.75", "75th percentile. q1 to q3 is the middle half of outcomes.",
  "Band", "high", "dbl", "R/50 quantile at p_high", "90th percentile. Not the ceiling: under the measured right skew the p90 sits close to the Normal p90 and the tail opens above it.",
  "Band", "p95", "dbl", "R/50 quantile at 0.95", "Ceiling, 95th percentile. Use for best ball or 'what does a big week look like'.",
  "Band", "band_width", "dbl", "derived", "high - low. Width of the 80 percent range.",
  "Band", "p_low / p_high", "dbl", "R/59 constants", "Probabilities low and high were taken at (0.10 and 0.90). Carried so the file is self-describing.",
  "Band", "(shape)", "", "R/50 shape table", "z-quantiles from 7,678 pooled within-player standardized weekly residuals, 2023-2024. Skew ~1.39 RB/WR, 1.35 TE, 0.45 QB. Method empirical at all four positions.",

  "Normal diagnostic", "low_norm", "dbl", "derived", "mu + sigma_wk * qnorm(p_low). The floor under a symmetric Normal (what R/36 still assumes).",
  "Normal diagnostic", "high_norm", "dbl", "derived", "mu + sigma_wk * qnorm(p_high). The p90 under a Normal.",
  "Normal diagnostic", "delta_low", "dbl", "derived", "low - low_norm. Positive = real floor is higher than the Normal says (typical at RB/WR/TE, about +1 to +1.4).",
  "Normal diagnostic", "delta_high", "dbl", "derived", "high - high_norm. Near zero or slightly negative at p90 because the skew shows above p90, not at it.",
  "Normal diagnostic", "band_width_norm", "dbl", "derived", "high_norm - low_norm.",

  "Lineage", "recon_schema", "chr", "R/32", "Schema tag of the reconciled file the band was built from (e.g. s2_w15_reconciled_v6).",
  "Lineage", "shape_built", "chr", "R/50", "Date the shape table was built.",
  "Lineage", "schema_tag", "chr", "R/59", "This file's schema (s3_r59_bands_v3).",
  "Lineage", "built_at", "dttm", "R/59", "When the file was written.",
  "Lineage", "source_file", "chr", "R/60", "Which weekly CSV the row was loaded from (added by the app).",

  "Not in this file", "(opponent strength)", "", "", "Offense matchup path is neutralized; nothing shifts mu for the defense faced.",
  "Not in this file", "(Vegas)", "", "", "No totals or spreads.",
  "Not in this file", "(usage)", "", "", "Route participation and snap share are R/56, keyed on the week just played.",
  "Not in this file", "(league context)", "", "", "No league-specific scoring or roster. R/59 is league-agnostic."
)

R60_AVAIL_COLORS <- c(
  active          = R60_GREEN,
  questionable    = R60_GOLD,
  out_week        = "#b3502a",
  out_for_season  = "#7a1f1f",
  bye             = "#8c8c8c"
)

.r60_avail_color <- function(x) {
  out <- unname(R60_AVAIL_COLORS[x])
  out[is.na(out)] <- "#5a5a5a"   # status:<value>, no_feed and anything unmapped
  out
}

# ------------------------------------------------------------------------------
# load_projection_bands: every R/59 week file on disk
# ------------------------------------------------------------------------------
load_projection_bands <- function(input_dir = R60_INPUT_DIR) {
  files <- list.files(input_dir, pattern = R60_FILE_GLOB, full.names = TRUE)
  if (length(files) == 0) {
    stop("R/60: no s3_r59_weekly_bands_wNN.csv files in ", input_dir,
         ". Run R/59 first.", call. = FALSE)
  }
  df <- purrr::map_dfr(files, function(f) {
    readr::read_csv(f, show_col_types = FALSE, guess_max = 5000) %>%
      dplyr::mutate(source_file = basename(f))
  })
  need <- c("season", "week", "player_name", "position", "team", "opponent",
            "availability", "playable", "mu", "low", "q1", "q3", "high", "p95",
            "sigma_wk", "band_width", "ytd_n", "prior_source")
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) stop("R/60: band files missing columns: ",
                             paste(miss, collapse = ", "), call. = FALSE)
  for (opt in c("avail_source", "ytd_weight_mode", "home", "sleeper_injury",
                "report_status", "low_norm", "high_norm", "delta_low", "delta_high",
                "recon_schema", "schema_tag")) {
    if (!opt %in% names(df)) df[[opt]] <- NA
  }
  df %>%
    dplyr::mutate(
      week      = as.integer(week),
      ytd_n     = as.integer(ytd_n),
      playable  = as.logical(playable),
      home      = as.logical(home),
      home_away = dplyr::case_when(is.na(home) ~ "unknown", home ~ "home", TRUE ~ "away"),
      avail_source    = as.character(avail_source),
      ytd_weight_mode = as.character(ytd_weight_mode)
    ) %>%
    dplyr::arrange(dplyr::desc(week), dplyr::desc(mu))
}

.r60_choices <- function(x) sort(unique(x[!is.na(x)]))
.r60_range   <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(0, 1))
  c(floor(min(x)), ceiling(max(x)))
}
.r60_parse_names <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(character())
  x <- unlist(strsplit(txt, "[,;\n]+"))
  x <- trimws(x)
  tolower(x[nzchar(x)])
}

# ------------------------------------------------------------------------------
# run_projection_explorer
# ------------------------------------------------------------------------------
run_projection_explorer <- function(input_dir = R60_INPUT_DIR, launch = TRUE) {

  for (pkg in c("shiny", "DT", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("R/60: package '", pkg, "' is required. install.packages('", pkg, "')",
           call. = FALSE)
    }
  }

  bands <- load_projection_bands(input_dir)
  weeks <- .r60_choices(bands$week)
  message(glue("R/60: {nrow(bands)} rows across week(s) {paste(weeks, collapse = ', ')}"))

  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML(glue(
      ".well {{ background-color: {R60_CREAM}; border-color: {R60_DARK}; }}
       h2, h4 {{ color: {R60_DARK}; }}
       .nav-tabs > li.active > a {{ color: {R60_DARK}; font-weight: bold; }}"
    )))),
    shiny::titlePanel("Weekly projection bands (R/59)"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::selectInput("week", "Week", choices = weeks, selected = max(weeks)),
        shiny::checkboxGroupInput("position", "Position", choices = R60_POSITIONS,
                                  selected = R60_POSITIONS, inline = TRUE),
        shiny::selectizeInput("team", "Team", choices = .r60_choices(bands$team),
                              multiple = TRUE),
        shiny::selectizeInput("opponent", "Opponent", choices = .r60_choices(bands$opponent),
                              multiple = TRUE),
        shiny::checkboxGroupInput("home_away", "Home / away",
                                  choices = c("home", "away", "unknown"),
                                  selected = c("home", "away", "unknown"), inline = TRUE),
        shiny::checkboxInput("playable_only", "Playable only", value = TRUE),
        shiny::selectizeInput("availability", "Availability",
                              choices = .r60_choices(bands$availability), multiple = TRUE),
        shiny::selectizeInput("avail_source", "Availability source",
                              choices = .r60_choices(bands$avail_source), multiple = TRUE),
        shiny::selectizeInput("prior_source", "Prior source",
                              choices = .r60_choices(bands$prior_source), multiple = TRUE),
        shiny::selectizeInput("weight_mode", "YTD weight mode",
                              choices = .r60_choices(bands$ytd_weight_mode), multiple = TRUE),
        shiny::sliderInput("ytd_n", "Observed games (ytd_n)",
                           min = 0, max = max(bands$ytd_n, na.rm = TRUE),
                           value = c(0, max(bands$ytd_n, na.rm = TRUE)), step = 1),
        shiny::sliderInput("mu",  "Mean (mu)",  min = .r60_range(bands$mu)[1],  max = .r60_range(bands$mu)[2],
                           value = .r60_range(bands$mu),  step = 0.5),
        shiny::sliderInput("low", "Floor (low, p10)", min = .r60_range(bands$low)[1], max = .r60_range(bands$low)[2],
                           value = .r60_range(bands$low), step = 0.5),
        shiny::sliderInput("high", "High (p90)", min = .r60_range(bands$high)[1], max = .r60_range(bands$high)[2],
                           value = .r60_range(bands$high), step = 0.5),
        shiny::sliderInput("p95", "Ceiling (p95)", min = .r60_range(bands$p95)[1], max = .r60_range(bands$p95)[2],
                           value = .r60_range(bands$p95), step = 0.5),
        shiny::sliderInput("sigma", "Weekly sigma", min = .r60_range(bands$sigma_wk)[1], max = .r60_range(bands$sigma_wk)[2],
                           value = .r60_range(bands$sigma_wk), step = 0.25),
        shiny::sliderInput("width", "Band width (p90 - p10)", min = .r60_range(bands$band_width)[1], max = .r60_range(bands$band_width)[2],
                           value = .r60_range(bands$band_width), step = 0.5),
        shiny::numericInput("top_n", "Top N per position (0 = all)", value = 0, min = 0, step = 1),
        shiny::textInput("search", "Name contains"),
        shiny::textAreaInput("name_list", "Only these players (one per line, or comma-separated)",
                             rows = 4),
        shiny::actionButton("reset", "Reset filters")
      ),
      shiny::mainPanel(
        width = 9,
        shiny::textOutput("status_line"),
        shiny::tabsetPanel(
          id = "tabs",
          shiny::tabPanel("Table",
            shiny::br(),
            shiny::checkboxGroupInput("cols", "Columns", inline = TRUE,
              choices  = c("opponent", "home_away", "availability", "avail_source",
                           "sleeper_injury", "report_status", "prior_source",
                           "ytd_n", "ytd_weight_mode", "mu", "low", "q1", "q3",
                           "high", "p95", "sigma_wk", "band_width",
                           "low_norm", "high_norm", "delta_low", "delta_high"),
              selected = c("opponent", "availability", "ytd_n", "mu", "low",
                           "high", "p95", "sigma_wk")),
            DT::DTOutput("table"),
            shiny::downloadButton("dl", "Download filtered CSV")
          ),
          shiny::tabPanel("Bands",
            shiny::br(),
            shiny::fluidRow(
              shiny::column(3, shiny::numericInput("plot_n", "Rows to plot (by mean)",
                                                   value = 30, min = 5, max = 120, step = 5)),
              shiny::column(3, shiny::checkboxInput("plot_facet", "Facet by position", value = TRUE)),
              shiny::column(3, shiny::checkboxInput("plot_norm", "Show Normal band (p10 to p90) for comparison",
                                                    value = FALSE))
            ),
            shiny::plotOutput("band_plot", height = "auto")
          ),
          shiny::tabPanel("Trend",
            shiny::br(),
            shiny::selectizeInput("trend_players", "Players (any week)",
                                  choices = .r60_choices(bands$player_name), multiple = TRUE),
            shiny::plotOutput("trend_plot", height = "420px"),
            DT::DTOutput("trend_table")
          ),
          shiny::tabPanel("Summary",
            shiny::br(),
            DT::DTOutput("summary_table")
          ),
          shiny::tabPanel("Dictionary",
            shiny::br(),
            shiny::p("How to read a row: mu is the expected points; low to high is the range to expect 80 percent of the time; p95 is the ceiling one week in twenty; playable says whether the row is even a candidate this week."),
            DT::DTOutput("dictionary")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    shiny::observeEvent(input$reset, {
      shiny::updateCheckboxGroupInput(session, "position", selected = R60_POSITIONS)
      shiny::updateSelectizeInput(session, "team", selected = character())
      shiny::updateSelectizeInput(session, "opponent", selected = character())
      shiny::updateCheckboxGroupInput(session, "home_away", selected = c("home", "away", "unknown"))
      shiny::updateCheckboxInput(session, "playable_only", value = TRUE)
      shiny::updateSelectizeInput(session, "availability", selected = character())
      shiny::updateSelectizeInput(session, "avail_source", selected = character())
      shiny::updateSelectizeInput(session, "prior_source", selected = character())
      shiny::updateSelectizeInput(session, "weight_mode", selected = character())
      shiny::updateSliderInput(session, "ytd_n", value = c(0, max(bands$ytd_n, na.rm = TRUE)))
      shiny::updateSliderInput(session, "mu",    value = .r60_range(bands$mu))
      shiny::updateSliderInput(session, "low",   value = .r60_range(bands$low))
      shiny::updateSliderInput(session, "high",  value = .r60_range(bands$high))
      shiny::updateSliderInput(session, "p95",   value = .r60_range(bands$p95))
      shiny::updateSliderInput(session, "sigma", value = .r60_range(bands$sigma_wk))
      shiny::updateSliderInput(session, "width", value = .r60_range(bands$band_width))
      shiny::updateNumericInput(session, "top_n", value = 0)
      shiny::updateTextInput(session, "search", value = "")
      shiny::updateTextAreaInput(session, "name_list", value = "")
    })

    filtered <- shiny::reactive({
      d <- bands %>% dplyr::filter(week == as.integer(input$week))
      if (length(input$position) > 0) d <- d %>% dplyr::filter(position %in% input$position)
      if (length(input$team) > 0)     d <- d %>% dplyr::filter(team %in% input$team)
      if (length(input$opponent) > 0) d <- d %>% dplyr::filter(opponent %in% input$opponent)
      if (length(input$home_away) > 0) d <- d %>% dplyr::filter(home_away %in% input$home_away)
      if (isTRUE(input$playable_only)) d <- d %>% dplyr::filter(playable %in% TRUE)
      if (length(input$availability) > 0) d <- d %>% dplyr::filter(availability %in% input$availability)
      if (length(input$avail_source) > 0) d <- d %>% dplyr::filter(avail_source %in% input$avail_source)
      if (length(input$prior_source) > 0) d <- d %>% dplyr::filter(prior_source %in% input$prior_source)
      if (length(input$weight_mode) > 0)  d <- d %>% dplyr::filter(ytd_weight_mode %in% input$weight_mode)
      d <- d %>% dplyr::filter(
        dplyr::between(dplyr::coalesce(ytd_n, 0L), input$ytd_n[1], input$ytd_n[2]),
        dplyr::between(mu,         input$mu[1],    input$mu[2]),
        dplyr::between(low,        input$low[1],   input$low[2]),
        dplyr::between(high,       input$high[1],  input$high[2]),
        dplyr::between(p95,        input$p95[1],   input$p95[2]),
        dplyr::between(sigma_wk,   input$sigma[1], input$sigma[2]),
        dplyr::between(band_width, input$width[1], input$width[2])
      )
      if (nzchar(trimws(input$search))) {
        d <- d %>% dplyr::filter(grepl(trimws(input$search), player_name, ignore.case = TRUE))
      }
      nl <- .r60_parse_names(input$name_list)
      if (length(nl) > 0) d <- d %>% dplyr::filter(tolower(player_name) %in% nl)
      if (!is.na(input$top_n) && input$top_n > 0) {
        d <- d %>% dplyr::group_by(position) %>%
          dplyr::arrange(dplyr::desc(mu), .by_group = TRUE) %>%
          dplyr::slice_head(n = input$top_n) %>% dplyr::ungroup()
      }
      d %>% dplyr::arrange(dplyr::desc(mu))
    })

    output$status_line <- shiny::renderText({
      d <- filtered()
      wk <- bands %>% dplyr::filter(week == as.integer(input$week))
      glue("Week {input$week}: {nrow(d)} of {nrow(wk)} rows | schema {paste(unique(wk$schema_tag), collapse = ',')} ",
           "| reconciled {paste(unique(wk$recon_schema), collapse = ',')} ",
           "| YTD mode {paste(names(table(wk$ytd_weight_mode)), table(wk$ytd_weight_mode), sep = '=', collapse = ', ')}")
    })

    output$table <- DT::renderDT({
      d <- filtered()
      cols <- c("player_name", "position", "team", intersect(input$cols, names(d)))
      ord <- if ("mu" %in% cols) list(list(which(cols == "mu") - 1L, "desc")) else list()
      DT::datatable(
        d %>% dplyr::select(dplyr::all_of(cols)),
        rownames = FALSE, filter = "top",
        options = list(pageLength = 25, scrollX = TRUE, order = ord)
      )
    })

    output$dl <- shiny::downloadHandler(
      filename = function() sprintf("r59_bands_w%02d_filtered.csv", as.integer(input$week)),
      content  = function(file) readr::write_csv(filtered(), file)
    )

    band_data <- shiny::reactive({
      d <- filtered()
      n <- if (is.na(input$plot_n)) 30 else input$plot_n
      if (isTRUE(input$plot_facet)) {
        d <- d %>% dplyr::group_by(position) %>%
          dplyr::arrange(dplyr::desc(mu), .by_group = TRUE) %>%
          dplyr::slice_head(n = n) %>% dplyr::ungroup()
      } else {
        d <- d %>% dplyr::slice_head(n = n)
      }
      d %>%
        dplyr::mutate(label = paste0(player_name, " (", team, ")"),
                      label = factor(label, levels = rev(unique(label))))
    })

    output$band_plot <- shiny::renderPlot({
      d <- band_data()
      shiny::validate(shiny::need(nrow(d) > 0, "No rows match the current filters."))
      vals <- unique(d$availability)
      cols <- stats::setNames(.r60_avail_color(vals), vals)
      p <- ggplot2::ggplot(d, ggplot2::aes(y = label)) +
        ggplot2::geom_segment(ggplot2::aes(x = low, xend = p95, yend = label, color = availability),
                              linewidth = 2.2, alpha = 0.55, lineend = "round") +
        ggplot2::geom_segment(ggplot2::aes(x = q1, xend = q3, yend = label, color = availability),
                              linewidth = 4.2, lineend = "butt") +
        ggplot2::geom_point(ggplot2::aes(x = mu), color = R60_DARK, size = 2.4) +
        ggplot2::geom_point(ggplot2::aes(x = high), color = R60_DARK, shape = 124, size = 3) +
        ggplot2::scale_color_manual(values = cols, name = "Availability") +
        ggplot2::labs(
          x = "Fantasy points (bar: p10 to p95, thick: p25 to p75, dot: mean, tick: p90)",
          y = NULL,
          title = glue("Week {input$week} projection bands")
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = R60_CREAM, color = NA),
          panel.background = ggplot2::element_rect(fill = R60_CREAM, color = NA),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          text = ggplot2::element_text(color = R60_DARK),
          legend.position = "bottom"
        )
      if (isTRUE(input$plot_norm)) {
        p <- p + ggplot2::geom_errorbar(
          ggplot2::aes(xmin = low_norm, xmax = high_norm),
          width = 0.35, color = R60_GOLD, linewidth = 0.6, orientation = "y"
        )
      }
      if (isTRUE(input$plot_facet)) {
        p <- p + ggplot2::facet_wrap(~ position, scales = "free_y", ncol = 2)
      }
      p
    }, height = function() {
      d <- band_data()
      n_rows <- if (isTRUE(input$plot_facet)) {
        max(table(d$position), 1) * ceiling(length(unique(d$position)) / 2)
      } else nrow(d)
      max(420, 24 * n_rows + 160)
    })

    trend_data <- shiny::reactive({
      shiny::req(length(input$trend_players) > 0)
      bands %>% dplyr::filter(player_name %in% input$trend_players) %>%
        dplyr::arrange(player_name, week)
    })

    output$trend_plot <- shiny::renderPlot({
      d <- trend_data()
      shiny::validate(shiny::need(nrow(d) > 0, "Pick one or more players."))
      ggplot2::ggplot(d, ggplot2::aes(x = week, group = player_name)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = high, fill = player_name), alpha = 0.18) +
        ggplot2::geom_line(ggplot2::aes(y = mu, color = player_name), linewidth = 1.1) +
        ggplot2::geom_point(ggplot2::aes(y = mu, color = player_name), size = 2.2) +
        ggplot2::scale_x_continuous(breaks = .r60_choices(bands$week)) +
        ggplot2::labs(x = "Week", y = "Fantasy points (line: mean, band: p10 to p90)",
                      color = NULL, fill = NULL, title = "Band by week") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = R60_CREAM, color = NA),
          panel.background = ggplot2::element_rect(fill = R60_CREAM, color = NA),
          text = ggplot2::element_text(color = R60_DARK),
          legend.position = "bottom"
        )
    })

    output$trend_table <- DT::renderDT({
      d <- trend_data() %>%
        dplyr::select(player_name, week, team, opponent, availability, ytd_n,
                      mu, low, high, p95, sigma_wk, ytd_weight_mode)
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
    })

    output$dictionary <- DT::renderDT({
      DT::datatable(
        R60_DICTIONARY, rownames = FALSE, filter = "top",
        options = list(pageLength = 60, dom = "ft", scrollX = TRUE,
                       columnDefs = list(list(width = "45%", targets = 4)))
      )
    })

    output$summary_table <- DT::renderDT({
      d <- filtered() %>%
        dplyr::group_by(position) %>%
        dplyr::summarise(
          n            = dplyr::n(),
          playable     = sum(playable %in% TRUE),
          med_mu       = round(stats::median(mu), 2),
          med_low      = round(stats::median(low), 2),
          med_high     = round(stats::median(high), 2),
          med_p95      = round(stats::median(p95), 2),
          med_sigma    = round(stats::median(sigma_wk), 2),
          med_width    = round(stats::median(band_width), 2),
          with_ytd     = sum(dplyr::coalesce(ytd_n, 0L) > 0),
          .groups = "drop"
        )
      DT::datatable(d, rownames = FALSE, options = list(dom = "t"))
    })
  }

  app <- shiny::shinyApp(ui, server)
  if (launch) shiny::runApp(app, launch.browser = TRUE) else app
}
