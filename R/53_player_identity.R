# ==============================================================================
# R/53: PLAYER IDENTITY RESOLUTION
# ==============================================================================
#
# PURPOSE. Map placeholder NFL gsis ids onto real ones as soon as the real id
# exists, so a rookie stops carrying a dummy key the moment the league issues
# him a real one.
#
# ------------------------------------------------------------------------------
# THE DEFECT THIS EXISTS FOR  [2026-08-10]
# ------------------------------------------------------------------------------
# nflreadr publishes two tables that disagree about the same players:
#
#   load_draft_picks(2026)   gsis_id = "LOV121782"   placeholder, 230 of 257
#                                                    (27 rows carry no id at all)
#   load_depth_charts(2026)  gsis_id = "00-0041027"  real, issued later
#
# R/28 reads the draft table and is right to. R/31 reads the depth chart and is
# right to. Neither stage invented anything. The two are simply keyed on ids of
# different vintages, and nothing re-links them when the real id appears.
#
# MEASURED CONSEQUENCES on the 2026-08-10 board:
#
#   74 of 599 board players carry a placeholder id. Every one of them is known
#   to R/31 under a real id. All 74 fail the R/32 join and reach the board with
#   blend_weight_effective = 0, meaning no volume input at all. Two sit inside
#   the top 200 (board ranks 111 and 173).
#
#   R/31's draft-capital join matches 0 of 230 picks, because it joins the
#   draft table's placeholder against the depth chart's real id. So
#   ROOKIE_CAPITAL_MULTIPLIERS has been inert for every player all season, and
#   .correct_rookie_depth_ranks() has never fired. Both report as clean lines
#   in the console ("Rookies in pool: 0", "no eligible players"), which is how
#   a total join failure passed for a normal result.
#
# ------------------------------------------------------------------------------
# WHY DERIVED AND NOT PERSISTED
# ------------------------------------------------------------------------------
# A stored crosswalk would go stale the same way every other cache in this
# pipeline has gone stale this season. Resolution is recomputed from the
# current roster on every run, so the moment nflreadr issues a real id the next
# run adopts it and the placeholder stops being used. No file to maintain and
# nothing to forget to refresh. The audit CSV this module writes is a RECORD of
# what was resolved, never an INPUT to resolution.
#
# ------------------------------------------------------------------------------
# WHY ADDITIVE (PHASE 1)
# ------------------------------------------------------------------------------
# R/39 builds player_uid from whatever nfl_gsis_id it receives (R/39:299-305),
# so for these rookies R/39's uid IS the placeholder. Overwriting the id in one
# stage would break the translation-prediction join for exactly the players
# being repaired, and the failure would present as an empty prior rather than
# as an id change.
#
# This module therefore NEVER overwrites nfl_gsis_id. It adds three columns and
# consumers opt in one at a time:
#
#   gsis_raw        what the source supplied
#   gsis_resolved   the real id when one was found, else gsis_raw
#   id_source       native | recovered_by_name | ambiguous | unresolved
#
# Phase 1 changes no consumer. Phases 2 and 3 (R/31 draft capital, R/32 blend
# join) adopt gsis_resolved. Phase 4 moves player_uid itself and requires R/39
# to be regenerated, which is why it is last and alone.
#
# ------------------------------------------------------------------------------
# WHY A SHARED MODULE AND NOT A FOURTH INLINE MATCHER
# ------------------------------------------------------------------------------
# This codebase already contains three name normalizers that disagree:
# .normalize_name_for_match_r31() in R/31, .uid_normalize_name() in R/39, and
# the normalizer inlined inside .r29_build_player_uid(). R/29:3597-3603
# documents that two functions named .normalize_player_name() exist in R/24 and
# R/29 with different behaviour, and whichever file sources last wins. A fourth
# copy is not the answer.
#
# NAMING. Everything here is prefixed pidr_. R/29 already defines a function
# called .resolve_player_identity() that resolves DISPLAY NAMES, which is a
# different job. Reusing that name would recreate the exact collision the
# paragraph above describes.
# ==============================================================================

library(dplyr)
library(readr)
library(here)
library(glue)
library(nflreadr)

# A real NFL gsis id. Everything else is a placeholder, whatever its shape.
# Observed placeholder forms in the 2026 feed: "LOV121782", "MEN516487",
# "WAS569019". Three uppercase letters and six digits, but the pattern is NOT
# matched on positively: anything that is not a real id is treated as a
# placeholder, so a new placeholder format needs no code change.
PIDR_REAL_GSIS_PATTERN <- "^00-0[0-9]{6}$"

PIDR_SCHEMA_TAG <- "s3_pidr_v1"

PIDR_AUDIT_PATH <- here::here("data", "season3_cache",
                              "s3_identity_resolution_audit.csv")


# ------------------------------------------------------------------------------
# .pidr_normalize_name
# ------------------------------------------------------------------------------
#' Normalize a player name for identity matching
#'
#' Inlined rather than delegating, for the reason documented in the header: the
#' shared-name normalizers in this codebase disagree with each other and the
#' winner depends on source order. This one is the module's own and nothing
#' else may redefine it.
#'
#' Suffixes are stripped because the draft table, the roster and Sleeper
#' disagree about them constantly (Josh vs Joshua Palmer, Jr. present or
#' absent). That raises collision risk, which is why every caller restricts to
#' unique matches.
#'
#' @param x Character vector.
#' @return Character vector, lowercase, no punctuation, no generational suffix.
#' @keywords internal
.pidr_normalize_name <- function(x) {
  out <- tolower(as.character(x))
  out <- gsub("[.']", "", out)
  out <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", out, ignore.case = TRUE)
  out <- gsub("[^a-z ]", "", out)
  out <- gsub("\\s+", " ", out)
  trimws(out)
}


#' Is this a real NFL gsis id?
#' @param x Character vector.
#' @return Logical vector. NA ids return FALSE, not NA.
#' @keywords internal
.pidr_is_real_gsis <- function(x) {
  !is.na(x) & grepl(PIDR_REAL_GSIS_PATTERN, as.character(x))
}


# ------------------------------------------------------------------------------
# pidr_build_authority
# ------------------------------------------------------------------------------
#' Build the authority table resolution matches against
#'
#' The authority is the current roster: nflreadr::load_rosters() refreshed with
#' Sleeper, which is where real ids for newly-signed rookies first appear. This
#' is the same source R/29 already loads, so no new dependency is introduced.
#'
#' AMBIGUOUS KEYS ARE EXCLUDED, NOT RESOLVED. A name+position appearing more
#' than once cannot be matched safely, and guessing between two players is
#' worse than leaving one unresolved. Same rule as R/29:5030-5037 and the
#' unique-match restriction in R/31's Sleeper merge.
#'
#' @param season Integer.
#' @param roster Optional pre-loaded roster, to avoid a second network call
#'   when the caller already has one. Must carry gsis_id, full_name, position.
#' @param verbose Logical.
#' @return Tibble: name_key, gsis_authority, authority_name, authority_team.
#' @export
pidr_build_authority <- function(season, roster = NULL, verbose = TRUE) {

  if (is.null(roster)) {
    roster <- tryCatch(
      nflreadr::load_rosters(seasons = as.integer(season)),
      error = function(e) {
        warning(glue("pidr_build_authority(): load_rosters({season}) failed: ",
                     "{conditionMessage(e)}. Returning an empty authority; ",
                     "every id will be reported unresolved."), call. = FALSE)
        NULL
      }
    )
  }

  empty <- tibble::tibble(name_key       = character(),
                          gsis_authority = character(),
                          authority_name = character(),
                          authority_team = character())

  if (is.null(roster) || nrow(roster) == 0L) return(empty)

  id_col <- intersect(c("gsis_id", "nfl_gsis_id"), names(roster))[1]
  nm_col <- intersect(c("full_name", "player_name", "football_name"),
                      names(roster))[1]
  ps_col <- intersect(c("position", "depth_chart_position"), names(roster))[1]
  tm_col <- intersect(c("team", "club_code"), names(roster))[1]

  if (any(is.na(c(id_col, nm_col, ps_col)))) {
    warning(glue("pidr_build_authority(): roster missing an id, name or ",
                 "position column. Columns present: ",
                 "{paste(names(roster), collapse = ', ')}"), call. = FALSE)
    return(empty)
  }

  base <- roster %>%
    dplyr::filter(.pidr_is_real_gsis(.data[[id_col]]),
                  !is.na(.data[[nm_col]]),
                  !is.na(.data[[ps_col]])) %>%
    dplyr::transmute(
      name_key       = paste0(.pidr_normalize_name(.data[[nm_col]]), "|",
                              toupper(as.character(.data[[ps_col]]))),
      gsis_authority = as.character(.data[[id_col]]),
      authority_name = as.character(.data[[nm_col]]),
      authority_team = if (!is.na(tm_col)) as.character(.data[[tm_col]])
                       else NA_character_
    ) %>%
    dplyr::filter(nchar(.data$name_key) > 1L)

  # A key mapping to more than one DISTINCT id is ambiguous. A key repeated
  # with the SAME id is just a duplicate roster row and is safe to collapse.
  key_ids <- base %>%
    dplyr::group_by(.data$name_key) %>%
    dplyr::summarise(n_ids = dplyr::n_distinct(.data$gsis_authority),
                     .groups = "drop")

  ambiguous <- key_ids$name_key[key_ids$n_ids > 1L]

  out <- base %>%
    dplyr::filter(!.data$name_key %in% ambiguous) %>%
    dplyr::distinct(.data$name_key, .keep_all = TRUE)

  if (isTRUE(verbose)) {
    message(glue("  pidr authority: {nrow(out)} usable key(s) from ",
                 "{nrow(roster)} roster row(s); ",
                 "{length(ambiguous)} key(s) excluded as ambiguous"))
  }

  attr(out, "ambiguous_keys") <- ambiguous
  out
}


# ------------------------------------------------------------------------------
# pidr_resolve
# ------------------------------------------------------------------------------
#' Resolve placeholder gsis ids against an authority table
#'
#' IDEMPOTENT. A real id in returns the same real id out, so this may be called
#' in any stage in any order without ordering risk. Calling it twice on the
#' same frame changes nothing.
#'
#' ADDITIVE. nfl_gsis_id (or whatever id_col names) is NEVER modified. Three
#' columns are added. See the header for why.
#'
#' @param df Tibble to resolve.
#' @param authority Tibble from pidr_build_authority().
#' @param id_col,name_col,pos_col Column names in df.
#' @param verbose Logical.
#' @return df plus gsis_raw, gsis_resolved, id_source.
#' @export
pidr_resolve <- function(df,
                          authority,
                          id_col   = "nfl_gsis_id",
                          name_col = "player_name",
                          pos_col  = "position",
                          verbose  = TRUE) {

  need <- c(id_col, name_col, pos_col)
  miss <- setdiff(need, names(df))
  if (length(miss) > 0L) {
    stop("pidr_resolve(): df is missing column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }

  ambiguous <- attr(authority, "ambiguous_keys")
  if (is.null(ambiguous)) ambiguous <- character(0)

  out <- df %>%
    dplyr::mutate(
      gsis_raw      = as.character(.data[[id_col]]),
      .pidr_is_real = .pidr_is_real_gsis(.data[[id_col]]),
      .pidr_key     = paste0(.pidr_normalize_name(.data[[name_col]]), "|",
                             toupper(as.character(.data[[pos_col]])))
    ) %>%
    dplyr::left_join(
      authority %>% dplyr::select(name_key, gsis_authority),
      by = c(".pidr_key" = "name_key"),
      relationship = "many-to-one",
      na_matches = "never"
    ) %>%
    dplyr::mutate(
      id_source = dplyr::case_when(
        .data$.pidr_is_real                    ~ "native",
        !is.na(.data$gsis_authority)           ~ "recovered_by_name",
        .data$.pidr_key %in% ambiguous         ~ "ambiguous",
        TRUE                                   ~ "unresolved"
      ),
      # A native id is never overwritten, which is what makes this idempotent.
      gsis_resolved = dplyr::if_else(
        .data$id_source == "recovered_by_name",
        .data$gsis_authority,
        .data$gsis_raw
      )
    ) %>%
    dplyr::select(-.pidr_is_real, -.pidr_key, -gsis_authority)

  # A resolution that maps two different players onto the same real id would be
  # silently destructive downstream, so it fails loudly here instead.
  recov <- out$gsis_resolved[out$id_source == "recovered_by_name"]
  clash <- intersect(recov, out$gsis_raw[out$id_source == "native"])
  if (length(clash) > 0L) {
    stop(glue(
      "pidr_resolve(): {length(clash)} recovered id(s) collide with an id ",
      "already native in this frame: {paste(utils::head(clash, 5), collapse = ', ')}. ",
      "Resolving would merge two rows into one player."
    ), call. = FALSE)
  }

  if (isTRUE(verbose)) {
    tally <- table(out$id_source)
    message(glue("  pidr resolve: ",
                 "{paste(sprintf('%s %d', names(tally), as.integer(tally)), collapse = ' | ')}"))
  }

  out
}


# ------------------------------------------------------------------------------
# pidr_audit
# ------------------------------------------------------------------------------
#' Report and optionally record what was resolved
#'
#' @param resolved Tibble from pidr_resolve().
#' @param label Character. Which stage produced it.
#' @param write_csv Logical. Append to PIDR_AUDIT_PATH.
#' @return Tibble of the recovered rows, invisibly.
#' @export
pidr_audit <- function(resolved, label = "unnamed", write_csv = FALSE) {

  recovered <- resolved %>%
    dplyr::filter(.data$id_source == "recovered_by_name") %>%
    dplyr::transmute(
      stage       = label,
      run_date    = as.character(Sys.Date()),
      player_name = .data$player_name,
      position    = .data$position,
      gsis_raw    = .data$gsis_raw,
      gsis_resolved = .data$gsis_resolved,
      schema_tag  = PIDR_SCHEMA_TAG
    )

  unresolved <- resolved %>%
    dplyr::filter(.data$id_source %in% c("unresolved", "ambiguous"),
                  !.pidr_is_real_gsis(.data$gsis_raw))

  message(glue("\n  IDENTITY AUDIT [{label}]"))
  message(glue("    rows                : {nrow(resolved)}"))
  message(glue("    recovered by name   : {nrow(recovered)}"))
  message(glue("    still unresolved    : {nrow(unresolved)}"))

  if (nrow(unresolved) > 0L) {
    message("    UNRESOLVED PLACEHOLDERS (these keep a dummy id):")
    print(as.data.frame(
      unresolved %>%
        dplyr::select(dplyr::any_of(c("player_name", "position", "team",
                                      "gsis_raw", "id_source"))) %>%
        utils::head(20)
    ))
  }

  if (isTRUE(write_csv) && nrow(recovered) > 0L) {
    dir.create(dirname(PIDR_AUDIT_PATH), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(recovered, PIDR_AUDIT_PATH,
                     append = file.exists(PIDR_AUDIT_PATH))
    message(glue("    audit appended: {basename(PIDR_AUDIT_PATH)}"))
  }

  invisible(recovered)
}


# ------------------------------------------------------------------------------
# pidr_phase1_report
# ------------------------------------------------------------------------------
#' Phase 1 report: what WOULD resolve, changing nothing
#'
#' Runs the resolver against the current R/29 and R/31 outputs and reports the
#' result without modifying any pipeline stage. This is the number that decides
#' whether Phases 2 and 3 are worth building: if a large share of the
#' placeholder cohort lands in "ambiguous", the resolver helps less than the
#' raw count of 74 suggests and the gap needs a different answer.
#'
#' @param season Integer.
#' @param r29_path,r31_path Character paths to the current cached outputs.
#' @param write_csv Logical. Record the crosswalk.
#' @return List of the two resolved frames, invisibly.
#' @export
pidr_phase1_report <- function(season   = 2026L,
                                r29_path = here::here("data", "season2_cache",
                                                      "dk6_week15_player_projections.csv"),
                                r31_path = here::here("data", "season2_cache",
                                                      "s2_week15_player_volume_allocation.csv"),
                                write_csv = TRUE) {

  message(glue("\n{strrep('=', 70)}"))
  message(glue("R/53 PHASE 1: identity resolution report, season {season}"))
  message("No pipeline stage is modified. Nothing downstream reads these")
  message("columns yet.")
  message(glue("{strrep('=', 70)}"))

  message("\nBuilding authority table from the current roster")
  auth <- pidr_build_authority(season = season)

  if (nrow(auth) == 0L) {
    stop("pidr_phase1_report(): empty authority table. Cannot resolve.",
         call. = FALSE)
  }

  out <- list()

  for (nm in c("r29", "r31")) {
    pth <- if (nm == "r29") r29_path else r31_path
    if (!file.exists(pth)) {
      message(glue("\n  {toupper(nm)}: not found at {basename(pth)}, skipped"))
      next
    }
    message(glue("\n{strrep('-', 70)}"))
    message(glue("{toupper(nm)}: {basename(pth)}"))
    d <- readr::read_csv(pth, show_col_types = FALSE)
    r <- pidr_resolve(d, auth)
    pidr_audit(r, label = nm, write_csv = write_csv)
    out[[nm]] <- r
  }

  # THE QUESTION PHASE 1 EXISTS TO ANSWER: after resolution, how many R/29
  # players can reach R/31? Compare against the id join as it stands today.
  if (!is.null(out$r29) && !is.null(out$r31)) {
    message(glue("\n{strrep('-', 70)}"))
    message("CROSS-STAGE EFFECT")
    before <- sum(out$r29$gsis_raw %in% out$r31$gsis_raw, na.rm = TRUE)
    after  <- sum(out$r29$gsis_resolved %in% out$r31$gsis_resolved, na.rm = TRUE)
    message(glue("  R/29 players matching R/31 BEFORE resolution: {before}"))
    message(glue("  R/29 players matching R/31 AFTER  resolution: {after}"))
    message(glue("  RECOVERED BY THIS MODULE                    : {after - before}"))
    message("\n  That last number is what Phases 2 and 3 would deliver. If it")
    message("  is far below 74, the ambiguous bucket is eating the cohort and")
    message("  the design needs revisiting before those phases are built.")
  }

  invisible(out)
}
