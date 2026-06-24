# ==============================================================================
# NFL Analytics Toolkit -- Season 2, Phase 5
# Shiny Data Layer
# File: shiny_app/R/data_layer.R
#
# PURPOSE
# -------
# Defines the cache file paths and load_shiny_data(), the function that reads
# all Phase 4 cached outputs at app startup. Extracted from app.R so the data
# layer can be unit-tested in isolation without launching the app or sourcing
# the full dependency chain (R/19, etc.).
#
# This file contains PURE DEFINITIONS ONLY. Sourcing it has no side effects
# beyond defining the path constants and the function. app.R calls
# load_shiny_data() itself after sourcing this file.
#
# DEPENDENCIES (namespaced so this file is sourceable in isolation)
# -----------------------------------------------------------------
#   here, readr, glue
# ==============================================================================

# ------------------------------------------------------------------------------
# CACHE FILE PATHS
# ------------------------------------------------------------------------------
# All produced by Phase 4 scripts. Changing a path here propagates to the data
# layer and to every tab that reads the corresponding slot.

PATH_VORP           <- here::here("data", "season2_cache",
                                  "s2_week15_vorp_rankings.rds")
PATH_START_SIT_UQ   <- here::here("data", "season2_cache",
                                  "s2_week16_r36_start_sit_uq.rds")
PATH_DEF_STREAMING  <- here::here("data", "season2_cache",
                                  "s2_week16_r36_def_streaming.rds")
PATH_OPTIMAL_LINEUP <- here::here("data", "season2_cache",
                                  "s2_week16_optimal_lineup.rds")
PATH_PROSPECTS      <- here::here("data", "season2_cache",
                                  "s2_week14_final_prospect_scores.csv")
PATH_TRADE_VALUES   <- here::here("data", "season2_cache",
                                  "s2_week17_trade_values.rds")

# ------------------------------------------------------------------------------
# load_shiny_data
# ------------------------------------------------------------------------------

#' Load all Phase 4 cached outputs
#'
#' Returns a named list with one slot per data source. Any slot is NULL if its
#' file is missing or fails to load -- the affected tab shows an informative
#' status message rather than crashing the app. Called once at app startup
#' (outside server) so the loaded data is shared across all sessions on the
#' worker process. No session ever triggers a reload from disk.
#'
#' Missing or failed slots are set with single-bracket assignment
#' (out[nm] <- list(NULL)), which preserves the named slot as NULL. Double-
#' bracket NULL assignment (out[[nm]] <- NULL) would REMOVE the slot from the
#' list entirely, corrupting the loaded/total count downstream.
#'
#' @return Named list with slots: vorp, start_sit_uq, def_streaming,
#'   optimal_lineup, prospects, trade_values.
load_shiny_data <- function() {

  loaders <- list(
    vorp = list(
      path  = PATH_VORP,
      type  = "rds",
      label = "VORP rankings",
      src   = "R/33"
    ),
    start_sit_uq = list(
      path  = PATH_START_SIT_UQ,
      type  = "rds",
      label = "Start/sit UQ",
      src   = "R/36"
    ),
    def_streaming = list(
      path  = PATH_DEF_STREAMING,
      type  = "rds",
      label = "DEF streaming",
      src   = "R/36"
    ),
    optimal_lineup = list(
      path  = PATH_OPTIMAL_LINEUP,
      type  = "rds",
      label = "Optimal lineup",
      src   = "R/35"
    ),
    prospects = list(
      path  = PATH_PROSPECTS,
      type  = "csv",
      label = "Prospect scores",
      src   = "R/28"
    ),
    trade_values = list(
      path  = PATH_TRADE_VALUES,
      type  = "rds",
      label = "Trade values",
      src   = "R/37"
    )
  )

  out <- vector("list", length(loaders))
  names(out) <- names(loaders)

  for (nm in names(loaders)) {
    spec <- loaders[[nm]]

    if (!file.exists(spec$path)) {
      warning(
        glue::glue("Cache file missing for '{nm}' ({spec$src}): ",
                   "{basename(spec$path)}"),
        call. = FALSE
      )
      out[nm] <- list(NULL)   # single-bracket assignment preserves the slot;
      next                    # [[<- NULL would remove it from the list entirely
    }

    out[nm] <- list(tryCatch(
      {
        if (spec$type == "rds") {
          readRDS(spec$path)
        } else {
          readr::read_csv(spec$path, show_col_types = FALSE)
        }
      },
      error = function(e) {
        warning(
          glue::glue("Failed to load '{nm}': {conditionMessage(e)}"),
          call. = FALSE
        )
        NULL
      }
    ))
  }

  out
}
