# ==============================================================================
# 50_weekly_predictive_shape.R
# Season 3 Wave A2 -- R5: the shape of the weekly predictive distribution
# NFL Analytics Toolkit
#
# NAVIGATION GUIDE
# Line  135 : .weekly_residuals_one_target() -- z for every player-week
# Line  205 : build_weekly_residuals()       -- pool z across target seasons
# Line  265 : .fit_shifted_gamma()           -- moment-matched fallback form
# Line  340 : build_shape_table()            -- the artifact R/32 and R/6 consume
# Line  440 : shape_quantile()               -- z at probability p, per position
# Line  495 : shape_draw()                   -- sample z, for simulation
# Line  545 : validate_shape_oos()           -- fit on A, score on B
# Line  650 : diagnose_exchangeability()     -- is one z pool per position valid
# Line  720 : run_shape_build()              -- build + validate + report
#
# DESCRIPTION
# R2, R2b, R3 and R3b fixed the SIZE of the weekly predictive distribution.
# R/49 confirmed it: weekly sd(z) = 0.997, 80 percent coverage 84.5, 95 percent
# coverage 95.5. What remains is SHAPE, and the same gate showed it is badly
# wrong: skew 1.242, with PIT deciles running 5.5 / 13.9 / 16.7 / 14.0 across
# the bottom half against 10 expected and 6 to 7 through bins 7 to 9.
#
# That pattern is a floor-bounded, right-skewed distribution. A weekly fantasy
# score cannot fall far below zero, so the left tail compresses and mass piles
# up just below the middle, while touchdowns produce a long right tail. No
# rescaling of a symmetric normal reproduces it, which is why this is a
# separate module rather than another constant.
#
# APPROACH (option C, with B as the fallback)
# C. EMPIRICAL. Pool standardized residuals z = (actual - mu) / sigma_week by
#    position and use their observed distribution directly. No functional form
#    to be wrong about, the floor and the tail come out exactly as observed,
#    and R6 simulates by drawing z and scaling by sigma_week.
# B. SHIFTED GAMMA, moment-matched on mean, variance and skew, used only where
#    a position has too few weeks for the empirical pool to be trustworthy.
#    Bounded below and genuinely right-skewed, so it fails in the right
#    direction when it is used at all.
#
# Cornish-Fisher (Chain B items 5a/5b) was considered and rejected here: its
# quantile function becomes non-monotone at the skew levels measured (RB 1.41,
# TE 1.57), so it can return an 80 percent interval wider than the 95 percent
# one. That is a silent, board-corrupting failure and not worth the reuse.
#
# WHY THE OUT-OF-SAMPLE SPLIT IS NOT OPTIONAL
# The residuals are computed from predictions. Fitting a shape on the same
# season used to score it would report a calibration that cannot fail, which is
# precisely the "small sample theater" trap. build_shape_table() therefore
# takes explicit fit seasons, and validate_shape_oos() scores on a season the
# table never saw.
#
# ADDITIVE. This module is standalone. It writes a cached artifact and edits no
# pipeline file. Nothing in the managed-league path reads it.
#
# DEPENDENCIES
# library(dplyr), library(purrr), library(glue), library(here)
# Upstream: R/49_uncertainty_calibration_gate.R (residual machinery, which in
#           turn sources R/15 and R/17)
# ==============================================================================

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(glue)

source(here::here("R", "49_uncertainty_calibration_gate.R"))


# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------

SHAPE_CACHE_PATH <- here::here("data", "season2_cache",
                                "s3_weekly_shape_table.rds")

# Probability grid the empirical quantile function is stored on. Dense in the
# tails because that is where best-ball value is decided and where a symmetric
# normal was worst.
SHAPE_PROB_GRID <- c(
  seq(0.001, 0.01, by = 0.001),
  seq(0.02, 0.10, by = 0.01),
  seq(0.125, 0.875, by = 0.025),
  seq(0.90, 0.98, by = 0.01),
  seq(0.99, 0.999, by = 0.001)
)

# Below this many pooled player-weeks a position falls back to the fitted
# shifted gamma. An empirical 0.1 percent quantile off 400 weeks is one
# observation, which is not an estimate.
SHAPE_MIN_WEEKS <- 800L

# Skew below this is treated as symmetric: the gamma fit degenerates as skew
# approaches zero (shape = 4 / skew^2 diverges), and a normal is the honest
# answer there anyway. QB measured 0.28.
SHAPE_MIN_SKEW <- 0.10

SHAPE_POSITIONS <- c("QB", "RB", "WR", "TE")

# Method per position. [2026-07-30, R5b]
#
# The pooled standardized-z table (R5) beat a normal decisively out of sample,
# mean_abs_dev 0.48 against 3.16. But diagnose_exchangeability() showed the
# shape is NOT scale-invariant: within every skill position, skew falls
# monotonically as projection rises (RB 2.07 / 1.23 / 0.74 across terciles) and
# low-projection RBs produced no weekly z below -1 at all.
#
# That is the zero floor binding. A weekly score cannot go below roughly zero,
# so for a 5 PPG player the floor sits about one sigma down and truncates the
# left side, forcing right skew. For a 15 PPG player it is three sigma away and
# stops mattering. One pooled z per position therefore misprices exactly the
# late-round low-projection players whose spike weeks are the entire best-ball
# case.
#
# "gamma_points" fixes this without bins and without new parameters. A gamma
# with mean mu and SD sigma has shape = (mu/sigma)^2 and skew = 2*sigma/mu, so
# the declining-skew pattern is PREDICTED by the form rather than fitted to it.
# Checked against the measured terciles: RB tercile 3 skew 0.735 implies sigma
# 5.66, tercile 1 skew 2.074 implies 5.25. Both are real weekly dispersions.
#
# QB is deliberately excluded. skew = 2*sigma/mu would imply about 0.82 for a
# typical starter against a measured 0.29 to 0.52, because a quarterback's
# weekly floor is nowhere near zero and the constraint generating the skew is
# simply not binding for him. The empirical pool is the honest description
# there, and this is a principled exception rather than a patch.
# GATE RESULT [2026-07-30]: gamma_points was TESTED AND REJECTED.
# The skew relation it predicts is correct. Backing sigma out of the measured
# tercile skews via sigma = skew * mu / 2 gives RB 5.25 and 5.66, WR 5.06 and
# 6.64, TE 3.10 and 5.26 across low and high terciles: all plausible weekly
# dispersions, so the declining-skew pattern really is the zero floor and the
# form really does explain it.
#
# It still lost on held-out 2025, and badly:
#   mean_abs_dev   pooled_z 0.48   gamma_points 1.32   normal 3.16
#   PIT bin 1      pooled_z 9.32   gamma_points 14.76  (expected 10)
#   low terciles   worse for RB and TE, better only for WR
#
# The reason is an ATOM AT ZERO. A player who is active but untargeted scores
# exactly 0.00, and for a 5 PPG back those weeks are a large share of the
# sample. A gamma is continuous and cannot place mass at a point, so it spreads
# that probability across a smooth interval and puts its 10th percentile too
# high, which is precisely the bin 1 overflow.
#
# The correct parametric form is therefore ZERO-INFLATED, not gamma. The
# empirical pool already absorbs the atom for free because it resamples
# observed weeks with their zeros intact.
#
# Machinery kept intact and revivable, per the neutralize-do-not-delete
# pattern: set a position to "gamma_points" here to re-enable it, and
# validate_shape_oos() will score it against the pool automatically.
SHAPE_METHOD_DEFAULT <- c(QB = "empirical", RB = "empirical",
                           WR = "empirical", TE = "empirical")

# Guards for the points-space gamma. A projection at or below zero, or a
# sigma at or below zero, has no gamma; those rows fall back to the
# standardized pool. SHAPE_GAMMA_MIN_SHAPE stops a near-zero projection
# producing an absurd skew from a shape approaching zero.
SHAPE_GAMMA_MIN_SHAPE <- 0.05
SHAPE_GAMMA_MIN_MU    <- 0.10

utils::globalVariables(c(
  "player_id", "position", "game_id", "game_points", "season",
  "pred_mu", "sigma_week", "z", "n_weeks", "mean_z", "sd_z", "skew_z",
  "p", "q", "src", "bin", "pct", "dev", "mu_tercile", "target",
  "u_norm", "u_shp", "u_cfg", "dev_pooled", "dev_config",
  "pooled_method", "normal", "pooled_z", "configured",
  "team", "week", "pid_a", "pid_b", "pos_a", "pos_b", "z_a", "z_b",
  "p1", "p2", "n_pairs", "obs_cor", "fit_cor", "err_model", "err_indep",
  "oos_cor", "n_oos", "n_fit", "target", "realized", "gap"
))


# ==============================================================================
# .weekly_residuals_one_target()
#
# Standardized weekly residuals for one target season.
#
# Predictions are built from the three seasons before the target using the SAME
# estimators production uses, via R/49's helpers. Reusing them rather than
# reimplementing matters: a shape fitted to residuals from a different
# estimator would describe a distribution the board never produces.
#
# @param target_season Integer.
# @param scoring_settings List.
# @param k Numeric. Omega shrinkage constant. Defaults to the R/49 gate value.
# @param cache_dir Character.
# @return Tibble: player_id, position, season, game_points, pred_mu,
#   sigma_week, z. NULL if the season cannot be built.
# ==============================================================================

.weekly_residuals_one_target <- function(target_season,
                                         scoring_settings,
                                         k = 12,
                                         cache_dir = GATE_CACHE_DIR) {

  target_season   <- as.integer(target_season)
  history_seasons <- (target_season - 3L):(target_season - 1L)

  hist_pts <- purrr::map_dfr(history_seasons, function(s) {
    .season_game_points(s, scoring_settings, cache_dir = cache_dir)
  })
  if (nrow(hist_pts) == 0) {
    message(glue("  target {target_season}: no history seasons available"))
    return(NULL)
  }

  target_pts <- .season_game_points(target_season, scoring_settings,
                                     cache_dir = cache_dir)
  if (is.null(target_pts) || nrow(target_pts) == 0) {
    message(glue("  target {target_season}: no target cache"))
    return(NULL)
  }

  hist_est <- .build_history_estimates(hist_pts, target_season = target_season)
  gf <- .apply_position_shrinkage(
    .assemble_gate_frame(hist_est, target_pts, k = k), k
  )

  if (nrow(gf) == 0) return(NULL)

  target_pts %>%
    dplyr::inner_join(
      gf %>% dplyr::select(player_id, position, pred_mu, sigma_week),
      by = c("player_id", "position")
    ) %>%
    dplyr::filter(!is.na(sigma_week), sigma_week > 0, !is.na(game_points)) %>%
    dplyr::mutate(
      z      = (game_points - pred_mu) / sigma_week,
      target = target_season
    )
}


# ==============================================================================
# build_weekly_residuals()
#
# Pools standardized residuals across several target seasons.
#
# Each target season gets its own three-season history window, so no residual
# is ever computed from a prediction that saw the outcome. Pooling several
# targets is what makes a per-position empirical tail estimable at all.
#
# @param target_seasons Integer vector.
# @param scoring_settings List.
# @param k Numeric.
# @param cache_dir Character.
# @return Tibble of residuals across all usable target seasons.
# ==============================================================================

build_weekly_residuals <- function(target_seasons,
                                   scoring_settings = DK_BEST_BALL_SCORING,
                                   k = 12,
                                   cache_dir = GATE_CACHE_DIR) {

  out <- purrr::map_dfr(sort(unique(as.integer(target_seasons))), function(ts) {
    message(glue("  residuals for target season {ts}..."))
    .weekly_residuals_one_target(ts, scoring_settings, k = k,
                                  cache_dir = cache_dir)
  })

  if (nrow(out) == 0) {
    stop("build_weekly_residuals(): no residuals could be built from ",
         "target seasons ", paste(target_seasons, collapse = ", "),
         ". Check the normalized pbp caches.", call. = FALSE)
  }

  out %>% dplyr::filter(position %in% SHAPE_POSITIONS, is.finite(z))
}


# ==============================================================================
# .fit_shifted_gamma()
#
# Moment-matches a shifted gamma to a residual vector. This is fallback B.
#
# For X = shift + Gamma(shape, rate):
#   mean  = shift + shape / rate
#   var   = shape / rate^2
#   skew  = 2 / sqrt(shape)
# so three observed moments determine all three parameters:
#   shape = 4 / skew^2,  rate = sqrt(shape / var),  shift = mean - shape / rate
#
# The two-parameter gamma cannot choose its skew independently of its shape,
# which is exactly why the shift is needed: without it, matching variance would
# force a skew the data does not have.
#
# Returns NULL when skew is below SHAPE_MIN_SKEW. The fit degenerates there
# (shape diverges) and a normal is the correct description anyway.
#
# @param z Numeric vector of standardized residuals.
# @return List(shift, shape, rate) or NULL.
# ==============================================================================

.fit_shifted_gamma <- function(z) {

  z <- z[is.finite(z)]
  if (length(z) < 30L) return(NULL)

  m <- mean(z)
  v <- stats::var(z)
  s <- stats::sd(z)
  if (!is.finite(v) || v <= 0) return(NULL)

  g <- mean((z - m)^3) / s^3
  if (!is.finite(g) || g < SHAPE_MIN_SKEW) return(NULL)

  shape <- 4 / (g^2)
  rate  <- sqrt(shape / v)
  shift <- m - shape / rate

  if (!is.finite(shape) || !is.finite(rate) || !is.finite(shift) ||
      shape <= 0 || rate <= 0) {
    return(NULL)
  }

  list(shift = shift, shape = shape, rate = rate, skew_matched = g)
}


# ==============================================================================
# build_shape_table()
#
# Builds the artifact R/32 and R6 consume.
#
# Per position:
#   method      "empirical" when the pooled week count clears SHAPE_MIN_WEEKS,
#               otherwise "gamma", otherwise "normal" if neither is usable
#   quantiles   z at each probability in SHAPE_PROB_GRID
#   gamma       moment-matched parameters, always fitted when possible so the
#               fallback is available and comparable even when unused
#   moments     n_weeks, mean, sd, skew
#
# The z pool is standardized, so the same table applies at any projection size:
# a prediction becomes mu + sigma_week * z. That is the whole reason for
# working in standardized space rather than fitting a distribution per player.
#
# @param resid Tibble from build_weekly_residuals().
# @param prob_grid Numeric vector of probabilities.
# @return List keyed by position, plus a meta element.
# ==============================================================================

build_shape_table <- function(resid, prob_grid = SHAPE_PROB_GRID,
                              methods = SHAPE_METHOD_DEFAULT) {

  .skew <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 3L) return(NA_real_)
    s <- stats::sd(x)
    if (!is.finite(s) || s == 0) return(NA_real_)
    mean((x - mean(x))^3) / s^3
  }

  tbl <- purrr::map(SHAPE_POSITIONS, function(pos) {

    zp <- resid$z[resid$position == pos]
    zp <- zp[is.finite(zp)]
    n  <- length(zp)

    if (n == 0L) {
      return(list(position = pos, method = "normal", n_weeks = 0L,
                  quantiles = NULL, gamma = NULL,
                  mean_z = NA_real_, sd_z = NA_real_, skew_z = NA_real_))
    }

    gam <- .fit_shifted_gamma(zp)

    # Configured method, downgraded if the data cannot support it. The
    # standardized pool is ALWAYS retained regardless of method, so a
    # gamma_points position still carries a usable fallback and the two can be
    # scored against each other on held-out data.
    want <- unname(methods[pos])
    if (is.na(want)) want <- "empirical"

    method <- if (identical(want, "gamma_points")) {
      "gamma_points"
    } else if (n >= SHAPE_MIN_WEEKS) {
      "empirical"
    } else if (!is.null(gam)) {
      "gamma"
    } else {
      "normal"
    }

    # Records what the pooled-z route would have been, so validation can put
    # the two side by side without rebuilding the table.
    pooled_method <- if (n >= SHAPE_MIN_WEEKS) "empirical" else
      if (!is.null(gam)) "gamma" else "normal"

    qs <- if (method %in% c("empirical", "gamma_points")) {
      as.numeric(stats::quantile(zp, probs = prob_grid, names = FALSE,
                                  type = 7))
    } else if (method == "gamma") {
      gam$shift + stats::qgamma(prob_grid, shape = gam$shape, rate = gam$rate)
    } else {
      stats::qnorm(prob_grid)
    }

    list(
      position      = pos,
      method        = method,
      pooled_method = pooled_method,
      n_weeks       = n,
      probs         = prob_grid,
      quantiles     = qs,
      # Always retained: gamma_points needs no pool, but validation and any
      # fallback do.
      z_pool        = if (n >= SHAPE_MIN_WEEKS) zp else NULL,
      gamma         = gam,
      mean_z        = mean(zp),
      sd_z          = stats::sd(zp),
      skew_z        = .skew(zp)
    )
  })

  names(tbl) <- SHAPE_POSITIONS

  tbl$meta <- list(
    built_at        = Sys.time(),
    n_weeks_total   = nrow(resid),
    target_seasons  = sort(unique(resid$target)),
    min_weeks_rule  = SHAPE_MIN_WEEKS,
    prob_grid_n     = length(prob_grid)
  )

  tbl
}


# ==============================================================================
# shape_quantile()
#
# z at probability p for a position, from the shape table.
#
# Falls back to the standard normal for an unknown position or a table entry
# that could not be built, and warns rather than failing silently, because a
# silent normal fallback here would reintroduce exactly the symmetric-tail bug
# this module exists to remove.
#
# @param position Character vector.
# @param p Numeric vector of probabilities, recycled against position.
# @param shape_table List from build_shape_table().
# @return Numeric vector of z values.
# ==============================================================================

shape_quantile <- function(position, p, shape_table) {

  n <- max(length(position), length(p))
  position <- rep_len(as.character(position), n)
  p        <- rep_len(as.numeric(p), n)

  warned <- FALSE
  out <- vapply(seq_len(n), function(i) {
    ent <- shape_table[[position[i]]]
    if (is.null(ent) || is.null(ent$quantiles) || ent$method == "normal") {
      if (!warned) {
        warning("shape_quantile(): falling back to a NORMAL tail for ",
                "position ", position[i], ". The weekly distribution is ",
                "right-skewed and a normal understates the upper tail.",
                call. = FALSE)
        warned <<- TRUE
      }
      return(stats::qnorm(p[i]))
    }
    stats::approx(x = ent$probs, y = ent$quantiles, xout = p[i],
                   rule = 2)$y
  }, numeric(1))

  out
}


# ==============================================================================
# shape_draw()
#
# Draws standardized residuals for simulation. This is what R6 consumes.
#
# For an empirical entry it resamples the observed z pool, which preserves the
# floor, the pile-up below the middle, and the long right tail exactly as
# measured, with no interpolation artefacts. For a gamma entry it samples the
# fitted form.
#
# @param position Character. One position.
# @param n Integer. Number of draws.
# @param shape_table List.
# @return Numeric vector of length n.
# ==============================================================================

shape_draw <- function(position, n, shape_table) {

  ent <- shape_table[[as.character(position)]]

  if (is.null(ent) || ent$method == "normal") {
    warning("shape_draw(): NORMAL fallback for position ", position,
            ". Best-ball value priced off a symmetric tail is understated.",
            call. = FALSE)
    return(stats::rnorm(n))
  }

  if (ent$method == "empirical" && !is.null(ent$z_pool)) {
    return(sample(ent$z_pool, size = n, replace = TRUE))
  }

  ent$gamma$shift + stats::rgamma(n, shape = ent$gamma$shape,
                                   rate = ent$gamma$rate)
}


# ==============================================================================
# POINTS-SPACE PREDICTIVE INTERFACE
#
# These three are what R6 and any interval consumer should call. They hide the
# method entirely: a caller supplies position, mu and sigma_week and gets
# points back, whether the position is priced off a standardized pool or off a
# points-space gamma.
#
# The gamma parameterisation is the whole point of R5b:
#   shape = (mu / sigma)^2      rate = mu / sigma^2
#   mean  = mu                  sd   = sigma      skew = 2 * sigma / mu
# So mean and variance are matched exactly and the skew is DERIVED from them
# rather than fitted. The floor at zero is respected by construction, which is
# the property the pooled-z table could not have at every projection size.
# ==============================================================================

#' Gamma parameters in points space, or NULL when the guards refuse
#' @keywords internal
.gamma_points_params <- function(mu, sigma) {
  if (!is.finite(mu) || !is.finite(sigma) || mu <= SHAPE_GAMMA_MIN_MU ||
      sigma <= 0) {
    return(NULL)
  }
  shp <- (mu / sigma)^2
  if (!is.finite(shp) || shp < SHAPE_GAMMA_MIN_SHAPE) return(NULL)
  list(shape = shp, rate = mu / (sigma^2))
}

#' Predictive quantile in POINTS for one position
#'
#' @param position Character vector.
#' @param p Numeric vector of probabilities.
#' @param mu Numeric vector of projections.
#' @param sigma_week Numeric vector of weekly predictive SDs.
#' @param shape_table List from build_shape_table().
#' @return Numeric vector of points.
#' @export
predictive_quantile <- function(position, p, mu, sigma_week, shape_table) {

  n <- max(length(position), length(p), length(mu), length(sigma_week))
  position   <- rep_len(as.character(position), n)
  p          <- rep_len(as.numeric(p), n)
  mu         <- rep_len(as.numeric(mu), n)
  sigma_week <- rep_len(as.numeric(sigma_week), n)

  vapply(seq_len(n), function(i) {
    ent <- shape_table[[position[i]]]
    if (!is.null(ent) && identical(ent$method, "gamma_points")) {
      gp <- .gamma_points_params(mu[i], sigma_week[i])
      if (!is.null(gp)) {
        return(stats::qgamma(p[i], shape = gp$shape, rate = gp$rate))
      }
      # Guards refused: fall through to the standardized pool rather than
      # emitting a value from a distribution that does not exist.
    }
    mu[i] + sigma_week[i] * shape_quantile(position[i], p[i], shape_table)
  }, numeric(1))
}

#' Predictive CDF in POINTS. Used by the out-of-sample PIT check.
#' @keywords internal
predictive_cdf <- function(position, x, mu, sigma_week, shape_table) {

  n <- max(length(position), length(x), length(mu), length(sigma_week))
  position   <- rep_len(as.character(position), n)
  x          <- rep_len(as.numeric(x), n)
  mu         <- rep_len(as.numeric(mu), n)
  sigma_week <- rep_len(as.numeric(sigma_week), n)

  vapply(seq_len(n), function(i) {
    ent <- shape_table[[position[i]]]
    if (!is.null(ent) && identical(ent$method, "gamma_points")) {
      gp <- .gamma_points_params(mu[i], sigma_week[i])
      if (!is.null(gp)) {
        return(stats::pgamma(x[i], shape = gp$shape, rate = gp$rate))
      }
    }
    z <- (x[i] - mu[i]) / sigma_week[i]
    if (is.null(ent) || is.null(ent$quantiles)) return(stats::pnorm(z))
    stats::approx(x = ent$quantiles, y = ent$probs, xout = z,
                   rule = 2, ties = "ordered")$y
  }, numeric(1))
}

#' Draw predictive weekly POINTS. This is what R6 simulates from.
#'
#' @param position Character. One position.
#' @param n Integer. Draws.
#' @param mu Numeric. Projection.
#' @param sigma_week Numeric. Weekly predictive SD.
#' @param shape_table List.
#' @return Numeric vector of length n, floored at zero.
#' @export
predictive_draw <- function(position, n, mu, sigma_week, shape_table) {

  ent <- shape_table[[as.character(position)]]

  if (!is.null(ent) && identical(ent$method, "gamma_points")) {
    gp <- .gamma_points_params(mu, sigma_week)
    if (!is.null(gp)) {
      return(stats::rgamma(n, shape = gp$shape, rate = gp$rate))
    }
  }

  # Standardized route. Floored at zero because a weekly fantasy score below
  # zero is possible but rare (fumbles, negative yardage), and the pool already
  # carries whatever negative mass actually occurs; the floor only catches
  # tail draws the scaling would push implausibly far down.
  pmax(mu + sigma_week * shape_draw(position, n, shape_table), 0)
}


# ==============================================================================
# TEAM CORRELATION
#
# Best ball is played with correlated players. A quarterback and his top
# receiver share a good week; a run-heavy game script lifts the back and
# suppresses the pass catchers. Drawing every player independently would price
# a stack exactly like two unrelated players, which would put a hole in the
# middle of any best-ball analysis, because stacking is one of the format's
# central ideas.
#
# MODEL. One shared shock per team-week, with a loading per position:
#   z_i = rho_pos(i) * S_team,week + sqrt(1 - rho_pos(i)^2) * e_i
# so for two players on the same team, cor(z_i, z_j) = rho_i * rho_j.
#
# That is four parameters instead of a 586-by-586 covariance matrix, which is
# not estimable from the weeks available. It is a real restriction: it forces
# every within-team correlation to factor through one common driver, so it
# cannot represent, say, two backs splitting a workload being NEGATIVELY
# correlated with each other while both are positively correlated with the QB.
# The gate below reports where that restriction fails rather than hiding it.
#
# The idiosyncratic part e_i is drawn from the R/50 empirical pool, so the
# marginal shape survives. Mixing a normal shock in does pull the marginal
# slightly toward normal, but at the rho levels typically measured the shock
# carries only rho^2 of the variance, so the empirical shape dominates.
# ==============================================================================

# Minimum player-pairs in a position-pair cell before its observed correlation
# is trusted enough to fit against.
CORR_MIN_PAIRS <- 200L

# CELL POLICY [2026-07-30]. Only the QB cells survived out-of-sample
# replication, so everything else is neutralized to zero.
#
# THE EVIDENCE. Cells measured on 2023-24, then re-measured on held-out 2025:
#     QB-WR   +0.245 -> +0.261   replicates, strengthens
#     QB-TE   +0.180 -> +0.210   replicates, strengthens
#     QB-RB   +0.075 -> +0.093   replicates, strengthens
#     TE-TE   -0.080 -> +0.008   SIGN FLIP
#     WR-WR   +0.016 -> -0.001   both indistinguishable from zero
#     RB-RB   -0.017 -> +0.002   both indistinguishable from zero
#     RB-TE   -0.004 -> +0.011   both indistinguishable from zero
#
# The negative within-position values were noise. TE-TE at -0.080 came off 696
# pairs, which is roughly two standard errors from zero BEFORE accounting for
# the fact that pairs sharing a player are not independent observations, so the
# true interval is wider still. The "target competition drives teammates
# negative" reading was an interpretation of that noise and it did not survive
# contact with a second season.
#
# What IS real: a quarterback and his pass catchers move together, in every
# cell, in both seasons, and more strongly in the held-out one. Two receivers
# from the same offense are effectively independent. The offense-level lift and
# the target competition do appear to cancel, but they cancel to zero rather
# than to a negative.
#
# Neutralized rather than deleted, per the standing pattern: set
# CORR_NEUTRALIZE_NON_QB to FALSE to restore every measured cell, and
# validate_pair_correlations_oos() will re-run the same comparison.
CORR_NEUTRALIZE_NON_QB <- TRUE
CORR_QB_CELLS <- c("QB")

#' Observed within-team-week correlations by position pair
#'
#' @param resid Tibble from build_weekly_residuals(), carrying team and week.
#' @return Tibble: pos_a, pos_b, n_pairs, obs_cor.
#' @keywords internal
.observed_pair_correlations <- function(resid) {

  need <- c("team", "week", "season", "z", "position", "player_id")
  miss <- setdiff(need, names(resid))
  if (length(miss) > 0) {
    stop(".observed_pair_correlations(): residual panel is missing ",
         paste(miss, collapse = ", "), ". Rebuild it with the R/49 that ",
         "carries week and team.", call. = FALSE)
  }

  d <- resid %>%
    dplyr::filter(!is.na(team), nchar(as.character(team)) > 0, is.finite(z))

  a <- d %>% dplyr::select(season, week, team, pid_a = player_id,
                            pos_a = position, z_a = z)
  b <- d %>% dplyr::select(season, week, team, pid_b = player_id,
                            pos_b = position, z_b = z)

  dplyr::inner_join(a, b, by = c("season", "week", "team"),
                     relationship = "many-to-many") %>%
    dplyr::filter(pid_a < pid_b) %>%
    dplyr::mutate(
      p1 = pmin(pos_a, pos_b),
      p2 = pmax(pos_a, pos_b)
    ) %>%
    dplyr::group_by(pos_a = p1, pos_b = p2) %>%
    dplyr::summarise(
      n_pairs = dplyr::n(),
      obs_cor = suppressWarnings(stats::cor(z_a, z_b, use = "complete.obs")),
      .groups = "drop"
    ) %>%
    dplyr::filter(is.finite(obs_cor))
}

#' Estimate per-position loadings on the shared team-week shock
#'
#' Fits rho by least squares against the observed pairwise correlations,
#' weighted by pair count, subject to cor(i, j) = rho_i * rho_j.
#'
#' rho is bounded to [0, 0.95]. Negative loadings are excluded deliberately:
#' with a single shared factor a negative rho would be indistinguishable from a
#' sign flip on the shock, so the parameter is not identified there, and the
#' genuinely negative relationships (a backfield committee splitting work) are
#' outside what a one-factor model can express at all.
#'
#' @param resid Tibble from build_weekly_residuals().
#' @return List: rho (named numeric), observed (tibble), fitted (tibble).
#' @export
estimate_team_correlation <- function(resid) {

  obs <- .observed_pair_correlations(resid) %>%
    dplyr::filter(n_pairs >= CORR_MIN_PAIRS)

  if (nrow(obs) < 3L) {
    warning("estimate_team_correlation(): only ", nrow(obs), " usable ",
            "position-pair cells. Returning zero correlation, which makes ",
            "every draw independent and understates roster variance.",
            call. = FALSE)
    return(list(rho = stats::setNames(rep(0, length(SHAPE_POSITIONS)),
                                       SHAPE_POSITIONS),
                observed = obs, fitted = NULL))
  }

  pos <- SHAPE_POSITIONS
  ia  <- match(obs$pos_a, pos)
  ib  <- match(obs$pos_b, pos)
  w   <- obs$n_pairs
  y   <- obs$obs_cor

  sse <- function(par) {
    r <- pmin(pmax(par, 0), 0.95)
    sum(w * (r[ia] * r[ib] - y)^2)
  }

  fit <- stats::optim(par = rep(0.3, length(pos)), fn = sse,
                       method = "L-BFGS-B",
                       lower = rep(0, length(pos)),
                       upper = rep(0.95, length(pos)))

  rho <- stats::setNames(pmin(pmax(fit$par, 0), 0.95), pos)

  fitted <- obs %>%
    dplyr::mutate(
      fit_cor  = round(rho[pos_a] * rho[pos_b], 4),
      obs_cor  = round(obs_cor, 4),
      residual = round(obs_cor - rho[pos_a] * rho[pos_b], 4)
    )

  list(rho = rho, observed = obs, fitted = fitted, converged = fit$convergence)
}

#' Score fitted loadings against a held-out season
#'
#' A one-factor model that reproduces its own fit data proves nothing. This
#' recomputes the observed pairwise correlations on a season the loadings never
#' saw and reports the gap, plus what independence (rho = 0) would have implied,
#' so the comparison is against a real alternative rather than against nothing.
#'
#' @param rho Named numeric from estimate_team_correlation().
#' @param score_season Integer.
#' @param scoring_settings List.
#' @param k Numeric.
#' @return Tibble.
#' @export
validate_correlation_oos <- function(rho, score_season,
                                      scoring_settings = DK_BEST_BALL_SCORING,
                                      k = 12) {

  resid <- .weekly_residuals_one_target(score_season, scoring_settings, k = k)
  if (is.null(resid) || nrow(resid) == 0) {
    stop("validate_correlation_oos(): no residuals for ", score_season,
         call. = FALSE)
  }

  .observed_pair_correlations(resid) %>%
    dplyr::filter(n_pairs >= CORR_MIN_PAIRS) %>%
    dplyr::mutate(
      obs_cor    = round(obs_cor, 4),
      fit_cor    = round(rho[pos_a] * rho[pos_b], 4),
      err_model  = round(abs(obs_cor - rho[pos_a] * rho[pos_b]), 4),
      err_indep  = round(abs(obs_cor - 0), 4),
      model_wins = err_model < err_indep
    )
}

#' Draw correlated standardized residuals for one team-week
#'
#' @param position Character vector, one entry per player.
#' @param rho Named numeric loadings.
#' @param shape_table List from build_shape_table().
#' @param shock Numeric scalar. The shared team-week draw. Supplied by the
#'   caller so that every player on a team receives the SAME shock, which is
#'   the entire point; generating it inside would silently decorrelate them.
#' @return Numeric vector of z, same length as position.
#' @export
draw_correlated_z <- function(position, rho, shape_table, shock = stats::rnorm(1)) {

  n <- length(position)
  r <- as.numeric(rho[as.character(position)])
  r[!is.finite(r)] <- 0

  # Idiosyncratic part from the empirical pool, standardized so the shared and
  # idiosyncratic pieces combine to unit variance.
  e <- vapply(seq_len(n), function(i) shape_draw(position[i], 1L, shape_table),
              numeric(1))
  ent_sd <- vapply(seq_len(n), function(i) {
    sd_i <- shape_table[[position[i]]]$sd_z
    if (is.null(sd_i) || !is.finite(sd_i) || sd_i <= 0) 1 else sd_i
  }, numeric(1))
  e <- e / ent_sd

  r * shock + sqrt(pmax(1 - r^2, 0)) * e
}


# ==============================================================================
# GAUSSIAN COPULA CORRELATION  [2026-07-30, replaces the one-factor model]
#
# WHY THE FACTOR MODEL WAS DROPPED
# A single shared team-week shock forces cor(i, j) = rho_i * rho_j, which makes
# same-position teammates the MOST correlated pairs on a roster. The data say
# the opposite, and not marginally:
#     QB-WR   +0.245   a QB and his receiver share the same touchdown
#     WR-WR   +0.016   two receivers COMPETE for targets, and that
#                      cannibalization cancels the offense-level lift
#     TE-TE   -0.080   same effect, stronger in a one-TE offense
# Those two cells cannot coexist under one factor: WR-WR of 0.0158 implies
# rho_WR = 0.126, which then requires rho_QB = 1.94 to reach QB-WR of 0.2447.
# The optimizer pinned rho_QB at its 0.95 bound, which was the tell. Out of
# sample it lost exactly the three within-position cells and won the six that
# do not need a negative term.
#
# Target competition is the whole question behind stacking two receivers from
# one offense, so a model that cannot express it is not usable for the thing it
# was added for.
#
# WHAT REPLACES IT
# Use the nine measured position-pair correlations directly as an exchangeable
# block matrix, and draw through a Gaussian copula so the R/50 empirical
# marginals survive untouched. Every cell fits by construction, including the
# negative ones, and there is no identification problem. The cells carry 380 to
# 5,968 pairs, which is ample.
#
# This is the same lesson the shape work taught: the direct empirical object
# beat the parametric one. It is now two for two in this chain.
#
# LIMITS, stated rather than discovered later
#  - Exchangeable within a position pair: every WR-WR pair on a team gets the
#    same correlation, so a true WR1/WR2 asymmetry is not represented.
#  - A copula's Gaussian correlation is not exactly the Pearson correlation of
#    the transformed variables. gate_copula_recovery() measures that gap
#    instead of assuming it away.
#  - A position pair never observed falls back to zero, which is independence,
#    not an error. That is deliberate and logged.
# ==============================================================================

#' Measured position-pair correlations, before policy
#' @param resid Tibble from build_weekly_residuals().
#' @param min_pairs Integer.
#' @return Tibble: pos_a, pos_b, n_pairs, obs_cor.
#' @export
estimate_pair_correlations <- function(resid, min_pairs = CORR_MIN_PAIRS) {
  .observed_pair_correlations(resid) %>%
    dplyr::filter(n_pairs >= min_pairs) %>%
    dplyr::mutate(obs_cor = round(obs_cor, 5))
}

#' Apply the cell policy: keep what replicated, neutralize what did not
#'
#' Returns the measured value alongside the shipped one so the neutralization
#' is visible in the object rather than only in this file's comments.
#'
#' @param pair_cor Tibble from estimate_pair_correlations().
#' @param neutralize Logical. FALSE restores every measured cell.
#' @return Tibble with obs_cor_measured, obs_cor, neutralized.
#' @export
apply_correlation_policy <- function(pair_cor,
                                      neutralize = CORR_NEUTRALIZE_NON_QB) {

  out <- pair_cor %>%
    dplyr::mutate(
      obs_cor_measured = obs_cor,
      is_qb_cell       = pos_a %in% CORR_QB_CELLS | pos_b %in% CORR_QB_CELLS,
      neutralized      = neutralize & !is_qb_cell,
      obs_cor          = dplyr::if_else(neutralized, 0, obs_cor)
    )

  if (any(out$neutralized)) {
    message(glue("  correlation policy: {sum(out$neutralized)} non-QB cell(s) ",
                 "neutralized to zero (failed OOS replication); ",
                 "{sum(!out$neutralized)} QB cell(s) retained"))
  }
  out
}

#' Position-by-position correlation lookup
#'
#' The diagonal holds the WITHIN-position pair correlation (two different
#' receivers), NOT 1. The 1s go on the player-level diagonal later. Conflating
#' the two is the easy mistake here and would silently erase the target
#' competition effect this whole change exists to capture.
#'
#' @param pair_cor Tibble from estimate_pair_correlations().
#' @return Named POSxPOS numeric matrix.
#' @export
pair_correlation_matrix <- function(pair_cor) {
  pos <- SHAPE_POSITIONS
  m <- matrix(0, length(pos), length(pos), dimnames = list(pos, pos))
  for (i in seq_len(nrow(pair_cor))) {
    a <- pair_cor$pos_a[i]; b <- pair_cor$pos_b[i]
    if (a %in% pos && b %in% pos) {
      m[a, b] <- pair_cor$obs_cor[i]
      m[b, a] <- pair_cor$obs_cor[i]
    }
  }
  # QB-QB is structurally unobservable rather than merely unmeasured: one
  # quarterback starts per team-week, so two rostered QBs are almost never on
  # the same NFL team and the cell has no football meaning. Zero is correct
  # there, not a fallback.
  if (m["QB", "QB"] == 0) {
    message("  pair_correlation_matrix(): QB-QB left at zero ",
            "(structurally unobservable, not a missing estimate)")
  }
  m
}

#' Nearest positive-definite correlation matrix
#'
#' An exchangeable block matrix built from independently estimated cells is not
#' guaranteed positive definite, especially with negative within-position
#' entries and several players per position. Clipping eigenvalues and
#' renormalising is the standard repair. Without it chol() fails outright,
#' which at least is loud, or succeeds on a matrix that is not a correlation
#' matrix, which is not.
#'
#' @param R Numeric matrix.
#' @param eps Numeric. Eigenvalue floor.
#' @return Repaired matrix.
#' @keywords internal
.nearest_pd <- function(R, eps = 1e-6) {
  R <- (R + t(R)) / 2
  e <- eigen(R, symmetric = TRUE)
  if (min(e$values) >= eps) return(R)
  vals <- pmax(e$values, eps)
  R2 <- e$vectors %*% diag(vals, nrow = length(vals)) %*% t(e$vectors)
  d <- sqrt(diag(R2))
  d[!is.finite(d) | d <= 0] <- 1
  R2 <- R2 / outer(d, d)
  diag(R2) <- 1
  (R2 + t(R2)) / 2
}

#' Prepare a reusable copula factorisation for one roster composition
#'
#' The Cholesky depends only on the POSITION VECTOR, not on the players or the
#' week, so a simulation should build it once per composition and reuse it
#' across every week and every draw. Recomputing an eigen decomposition inside
#' the sampling loop is the difference between a simulation that runs and one
#' that does not.
#'
#' @param positions Character vector, one per player.
#' @param pc Named POSxPOS matrix from pair_correlation_matrix().
#' @return List: positions, L (upper Cholesky factor), repaired (logical).
#' @export
prepare_copula <- function(positions, pc) {
  positions <- as.character(positions)
  known <- positions %in% rownames(pc)
  if (!all(known)) {
    stop("prepare_copula(): unknown position(s): ",
         paste(unique(positions[!known]), collapse = ", "), call. = FALSE)
  }
  R <- pc[positions, positions, drop = FALSE]
  diag(R) <- 1
  R_pd <- .nearest_pd(R)
  list(positions = positions,
       L = chol(R_pd),
       repaired = !isTRUE(all.equal(R, R_pd, tolerance = 1e-8)))
}

#' Draw correlated standardized residuals through the copula
#'
#' Marginals come from the R/50 empirical quantile curves, so the shape work is
#' preserved exactly. Only the dependence comes from the Gaussian layer.
#'
#' @param prep List from prepare_copula().
#' @param shape_table List from build_shape_table().
#' @param n_draws Integer. Independent team-weeks to generate.
#' @return Numeric matrix, n_draws rows by length(positions) columns.
#' @export
draw_copula_z <- function(prep, shape_table, n_draws = 1L) {
  k <- length(prep$positions)
  g <- matrix(stats::rnorm(n_draws * k), nrow = n_draws, ncol = k) %*% prep$L
  u <- stats::pnorm(g)
  out <- matrix(NA_real_, nrow = n_draws, ncol = k)
  for (j in seq_len(k)) {
    out[, j] <- shape_quantile(rep(prep$positions[j], n_draws), u[, j],
                                shape_table)
  }
  out
}

#' Calibrate Gaussian inputs so the copula RETURNS the measured correlations
#'
#' A Gaussian copula's correlation parameter is not the Pearson correlation of
#' the transformed variables. With the skewed empirical marginals used here the
#' transform ATTENUATES, and the loss scales with the correlation: the recovery
#' gate measured QB-WR coming back at 0.2353 against a target of 0.2509 and
#' QB-TE at 0.1767 against 0.1905, about 6 to 7 percent light, while QB-RB at
#' 0.0812 was exact and every zero cell was clean.
#'
#' Six percent is small but it is one-directional and it lands on precisely the
#' cells that carry any stacking conclusion, so it is inverted rather than
#' carried. For each cell this solves numerically for the Gaussian input whose
#' transformed correlation equals the measured one.
#'
#' COMMON RANDOM NUMBERS. The base normals are drawn ONCE and reused across
#' every candidate value, which makes the mapping deterministic in g and lets a
#' plain bisection converge. Redrawing inside the loop would make the objective
#' noisy and the search would chase Monte Carlo error instead of the answer.
#'
#' Pairwise calibration is an approximation in the joint case: solving each
#' cell separately ignores the others. gate_copula_recovery() is run on the
#' full composition afterwards to confirm it holds jointly.
#'
#' @param pc Named POSxPOS matrix of TARGET correlations.
#' @param shape_table List from build_shape_table().
#' @param n_draws Integer. Sample size for the deterministic mapping.
#' @param tol Numeric. Absolute correlation tolerance.
#' @param max_iter Integer.
#' @return Named POSxPOS matrix of calibrated Gaussian inputs.
#' @export
calibrate_copula_targets <- function(pc, shape_table, n_draws = 60000L,
                                      tol = 0.0015, max_iter = 25L) {

  pos <- rownames(pc)
  x1 <- stats::rnorm(n_draws)
  x2 <- stats::rnorm(n_draws)

  realized_for <- function(a, b, g) {
    ga <- x1
    gb <- g * x1 + sqrt(pmax(1 - g^2, 0)) * x2
    za <- shape_quantile(rep(a, n_draws), stats::pnorm(ga), shape_table)
    zb <- shape_quantile(rep(b, n_draws), stats::pnorm(gb), shape_table)
    stats::cor(za, zb)
  }

  out <- pc
  for (i in seq_along(pos)) {
    for (j in seq_along(pos)) {
      if (j < i) next
      target <- pc[i, j]
      if (!is.finite(target) || abs(target) < 1e-9) next

      lo <- max(-0.99, target - 0.25)
      hi <- min(0.99, target + 0.25)
      g  <- target
      for (it in seq_len(max_iter)) {
        r <- realized_for(pos[i], pos[j], g)
        if (!is.finite(r)) break
        if (abs(r - target) <= tol) break
        if (r < target) lo <- g else hi <- g
        g <- (lo + hi) / 2
      }
      out[i, j] <- g
      out[j, i] <- g
      message(sprintf("  calibrated %s-%s: target %.4f -> gaussian input %.4f",
                      pos[i], pos[j], target, g))
    }
  }
  out
}

#' Does the copula actually reproduce the correlations it was given?
#'
#' A Gaussian copula's correlation parameter is not identical to the Pearson
#' correlation of the transformed variables, and the empirical marginals here
#' are skewed, which widens that gap. This simulates and measures it rather
#' than asserting it is negligible.
#'
#' @param pc Named POSxPOS matrix.
#' @param shape_table List.
#' @param composition Character vector of positions to simulate.
#' @param n_draws Integer.
#' @return Tibble: pos_a, pos_b, target, realized, gap.
#' @export
gate_copula_recovery <- function(pc, shape_table,
                                  composition = c("QB", "RB", "RB", "WR",
                                                   "WR", "WR", "TE", "TE"),
                                  n_draws = 20000L) {

  prep <- prepare_copula(composition, pc)
  z <- draw_copula_z(prep, shape_table, n_draws = n_draws)

  res <- list()
  for (i in seq_along(composition)) {
    for (j in seq_along(composition)) {
      if (j <= i) next
      a <- composition[i]; b <- composition[j]
      res[[length(res) + 1L]] <- tibble::tibble(
        pos_a    = min(a, b),
        pos_b    = max(a, b),
        target   = pc[a, b],
        realized = stats::cor(z[, i], z[, j])
      )
    }
  }

  dplyr::bind_rows(res) %>%
    dplyr::group_by(pos_a, pos_b) %>%
    dplyr::summarise(
      target   = round(mean(target), 4),
      realized = round(mean(realized), 4),
      gap      = round(mean(realized) - mean(target), 4),
      .groups  = "drop"
    )
}

#' Stability of the measured pair correlations on a held-out season
#'
#' For the copula the relevant question is not whether a functional form fits,
#' it is whether last year's measured cells predict this year's. That is what
#' actually breaks if the estimate is noise.
#'
#' @param pair_cor Tibble estimated on the fit seasons.
#' @param score_season Integer.
#' @param scoring_settings List.
#' @param k Numeric.
#' @return Tibble.
#' @export
validate_pair_correlations_oos <- function(pair_cor, score_season,
                                            scoring_settings = DK_BEST_BALL_SCORING,
                                            k = 12) {

  resid <- .weekly_residuals_one_target(score_season, scoring_settings, k = k)
  if (is.null(resid) || nrow(resid) == 0) {
    stop("validate_pair_correlations_oos(): no residuals for ", score_season,
         call. = FALSE)
  }

  .observed_pair_correlations(resid) %>%
    dplyr::filter(n_pairs >= CORR_MIN_PAIRS) %>%
    dplyr::rename(oos_cor = obs_cor, n_oos = n_pairs) %>%
    dplyr::inner_join(
      pair_cor %>% dplyr::select(pos_a, pos_b, fit_cor = obs_cor,
                                  n_fit = n_pairs),
      by = c("pos_a", "pos_b")
    ) %>%
    dplyr::mutate(
      oos_cor    = round(oos_cor, 4),
      err_model  = round(abs(oos_cor - fit_cor), 4),
      err_indep  = round(abs(oos_cor), 4),
      model_wins = err_model < err_indep
    )
}


# ==============================================================================
# validate_shape_oos()
#
# Fits nothing. Scores an existing shape table against a season it never saw.
#
# Reports PIT deciles under the NORMAL assumption and under the shape table,
# side by side. A correct shape drives every decile toward 10 percent. This is
# the only number that establishes R5 worked, and it is meaningless unless
# score_season is absent from the table's target_seasons.
#
# @param shape_table List.
# @param score_season Integer. Held out from the fit.
# @param scoring_settings List.
# @param k Numeric.
# @return List: deciles tibble, summary tibble.
# ==============================================================================

validate_shape_oos <- function(shape_table, score_season,
                                scoring_settings = DK_BEST_BALL_SCORING,
                                k = 12) {

  score_season <- as.integer(score_season)
  fit_targets  <- shape_table$meta$target_seasons

  if (score_season %in% fit_targets) {
    stop("validate_shape_oos(): season ", score_season, " was used to BUILD ",
         "this table. Scoring it here would report a calibration that cannot ",
         "fail. Rebuild the table without it.", call. = FALSE)
  }

  resid <- .weekly_residuals_one_target(score_season, scoring_settings, k = k)
  if (is.null(resid) || nrow(resid) == 0) {
    stop("validate_shape_oos(): no residuals for season ", score_season,
         call. = FALSE)
  }

  # PIT under the shape table: find the probability whose z matches the
  # observed z. Inverted by interpolating the stored quantile curve.
  u_shape <- vapply(seq_len(nrow(resid)), function(i) {
    ent <- shape_table[[resid$position[i]]]
    if (is.null(ent) || is.null(ent$quantiles) || ent$method == "normal") {
      return(stats::pnorm(resid$z[i]))
    }
    stats::approx(x = ent$quantiles, y = ent$probs, xout = resid$z[i],
                   rule = 2, ties = "ordered")$y
  }, numeric(1))

  # Configured methods, in points space. This is what actually ships.
  u_cfg <- predictive_cdf(resid$position, resid$game_points,
                           resid$pred_mu, resid$sigma_week, shape_table)

  d <- resid %>%
    dplyr::mutate(u_norm = stats::pnorm(z), u_shp = u_shape, u_cfg = u_cfg)

  .dec <- function(u, label) {
    tibble::tibble(u = u) %>%
      dplyr::mutate(bin = cut(u, breaks = seq(0, 1, 0.1),
                               include.lowest = TRUE, labels = FALSE)) %>%
      dplyr::count(bin, name = "n") %>%
      dplyr::mutate(pct = round(100 * n / sum(n), 2),
                     dev = round(pct - 10, 2),
                     src = label)
  }

  deciles <- dplyr::bind_rows(.dec(d$u_norm, "normal"),
                               .dec(d$u_shp, "pooled_z"),
                               .dec(d$u_cfg, "configured"))

  # THE decisive table. Overall PIT was already acceptable for the pooled
  # route; the defect diagnose_exchangeability() found was at TERCILE level,
  # so a fix has to be judged there or it is not being judged at all.
  by_tercile <- d %>%
    dplyr::group_by(position) %>%
    dplyr::mutate(mu_tercile = dplyr::ntile(pred_mu, 3)) %>%
    dplyr::group_by(position, mu_tercile) %>%
    dplyr::summarise(
      n_weeks = dplyr::n(),
      med_mu  = round(stats::median(pred_mu), 2),
      # Mean absolute decile deviation within the cell, per route.
      dev_pooled = round(mean(abs(
        as.numeric(table(cut(u_shp, seq(0, 1, 0.1), include.lowest = TRUE))) /
          dplyr::n() * 100 - 10)), 2),
      dev_config = round(mean(abs(
        as.numeric(table(cut(u_cfg, seq(0, 1, 0.1), include.lowest = TRUE))) /
          dplyr::n() * 100 - 10)), 2),
      .groups = "drop"
    )

  summ <- deciles %>%
    dplyr::group_by(src) %>%
    dplyr::summarise(
      max_abs_dev  = round(max(abs(dev), na.rm = TRUE), 2),
      mean_abs_dev = round(mean(abs(dev), na.rm = TRUE), 2),
      .groups = "drop"
    )

  list(deciles = deciles, summary = summ, by_tercile = by_tercile,
       n_weeks = nrow(d), score_season = score_season)
}


# ==============================================================================
# diagnose_exchangeability()
#
# Pooling one z distribution per position assumes standardized residuals are
# exchangeable across players within that position. That assumption is doing
# real work and is easy to get wrong: if a 3 PPG backup and an 18 PPG starter
# have different SHAPES after standardizing, one pooled table misprices both.
#
# Splits each position's residuals by projection tercile and reports skew and
# tail mass. Materially different skew across terciles means the pool should be
# split further, most likely by role.
#
# @param resid Tibble from build_weekly_residuals().
# @return Tibble.
# ==============================================================================

diagnose_exchangeability <- function(resid) {

  .skew <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 3L) return(NA_real_)
    s <- stats::sd(x)
    if (!is.finite(s) || s == 0) return(NA_real_)
    mean((x - mean(x))^3) / s^3
  }

  resid %>%
    dplyr::group_by(position) %>%
    dplyr::mutate(mu_tercile = dplyr::ntile(pred_mu, 3)) %>%
    dplyr::group_by(position, mu_tercile) %>%
    dplyr::summarise(
      n_weeks  = dplyr::n(),
      med_mu   = round(stats::median(pred_mu, na.rm = TRUE), 2),
      sd_z     = round(stats::sd(z, na.rm = TRUE), 3),
      skew_z   = round(.skew(z), 3),
      pct_gt_2 = round(100 * mean(z > 2, na.rm = TRUE), 2),
      pct_lt_1 = round(100 * mean(z < -1, na.rm = TRUE), 2),
      .groups = "drop"
    )
}


# ==============================================================================
# run_shape_build()
#
# Builds the shape table, validates it out of sample, and reports.
#
# @param fit_seasons Integer vector of target seasons used to FIT.
# @param score_season Integer. Held out.
# @param scoring_settings List.
# @param k Numeric.
# @param save_output Logical. Writes SHAPE_CACHE_PATH.
# @return Invisible list: table, validation, exchangeability, residuals.
# ==============================================================================

run_shape_build <- function(fit_seasons      = c(2023L, 2024L),
                            score_season     = 2025L,
                            scoring_settings = DK_BEST_BALL_SCORING,
                            k                = 12,
                            save_output      = TRUE) {

  if (score_season %in% fit_seasons) {
    stop("run_shape_build(): score_season cannot appear in fit_seasons.",
         call. = FALSE)
  }

  message(glue("\n{strrep('=', 70)}"))
  message("R/50 WEEKLY PREDICTIVE SHAPE")
  message(glue("Fit target seasons : {paste(fit_seasons, collapse = ', ')}"))
  message(glue("Held-out season    : {score_season}"))
  message(glue("Omega shrinkage k  : {k}"))
  message(glue("{strrep('=', 70)}"))

  message("\nSTEP 1/4: Building standardized weekly residuals")
  resid <- build_weekly_residuals(fit_seasons, scoring_settings, k = k)
  message(glue("  pooled player-weeks: {nrow(resid)}"))

  message("\nSTEP 2/4: Building shape table")
  tbl <- build_shape_table(resid)

  message("\nSTEP 3/4: Exchangeability diagnostic")
  exch <- diagnose_exchangeability(resid)

  message("\nSTEP 4/4: Out-of-sample validation")
  val <- validate_shape_oos(tbl, score_season, scoring_settings, k = k)

  cat("\n", strrep("-", 70), "\n", sep = "")
  cat("SHAPE TABLE\n")
  cat(strrep("-", 70), "\n", sep = "")
  print(as.data.frame(purrr::map_dfr(SHAPE_POSITIONS, function(p) {
    e <- tbl[[p]]
    tibble::tibble(position = p, method = e$method, n_weeks = e$n_weeks,
                   mean_z = round(e$mean_z, 3), sd_z = round(e$sd_z, 3),
                   skew_z = round(e$skew_z, 3),
                   gamma_shape = if (is.null(e$gamma)) NA_real_ else
                     round(e$gamma$shape, 2))
  })))

  cat("\n", strrep("-", 70), "\n", sep = "")
  cat("EXCHANGEABILITY: is one pooled z per position defensible?\n")
  cat("Materially different skew across terciles means split further.\n")
  cat(strrep("-", 70), "\n", sep = "")
  print(as.data.frame(exch))

  cat("\n", strrep("-", 70), "\n", sep = "")
  cat(glue("OUT-OF-SAMPLE PIT on {val$score_season} ",
            "({val$n_weeks} player-weeks)"), "\n")
  cat("Perfect calibration = 10.00 percent in every bin.\n")
  cat(strrep("-", 70), "\n", sep = "")
  print(as.data.frame(
    val$deciles %>%
      dplyr::select(bin, src, pct) %>%
      tidyr::pivot_wider(names_from = src, values_from = pct) %>%
      dplyr::mutate(
        dev_normal = round(normal - 10, 2),
        dev_pooled = round(pooled_z - 10, 2),
        dev_config = round(configured - 10, 2)
      )
  ))
  cat("\n")
  print(as.data.frame(val$summary))

  cat("\n", strrep("-", 70), "\n", sep = "")
  cat("BY TERCILE: the defect R5b targets. Lower is better.\n")
  cat("dev_pooled = pooled standardized z. dev_config = shipped methods.\n")
  cat(strrep("-", 70), "\n", sep = "")
  print(as.data.frame(val$by_tercile))

  if (save_output) {
    dir.create(dirname(SHAPE_CACHE_PATH), recursive = TRUE,
                showWarnings = FALSE)
    saveRDS(tbl, SHAPE_CACHE_PATH)
    message(glue("\nSaved shape table: {SHAPE_CACHE_PATH}"))
  }

  invisible(list(table = tbl, validation = val,
                  exchangeability = exch, residuals = resid))
}
