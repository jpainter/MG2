# Outlier detection functions for MG2.
# Migrated from R/originals/Cleaning.R

# Shared mutable state for time-throttled Shiny progress updates.
# Initialised by mad_outliers() / seasonal_outliers() before each scan;
# read/written by extremely_mad() and the seasonal group_modify loop.
.mg2_scan_state <- new.env(parent = emptyenv())
.mg2_scan_state$n        <- 0L    # series processed so far
.mg2_scan_state$total    <- 0L    # total series in this scan
.mg2_scan_state$last_t   <- 0     # proc.time elapsed at last UI update
.mg2_scan_state$interval <- 10    # minimum seconds between UI updates

#' Flag Values Exceeding a MAD-Based Threshold
#'
#' For a numeric vector `x`, computes the median and median absolute deviation
#' (MAD), then flags values that deviate from the median by more than
#' `deviation × MAD`. When MAD is near zero, falls back to a trimmed SD.
#' Returns `NA` for all values when the series is entirely below
#' `smallThreshold`.
#'
#' @param x Numeric vector.
#' @param deviation Numeric. Multiple of MAD used as threshold (default: `15`).
#' @param smallThreshold Numeric. Series with all values ≤ this are skipped
#'   (default: `50`).
#' @param key_entry_error Logical vector. Pre-flagged key-entry errors (unused
#'   internally; kept for API compatibility).
#' @param over_max Logical vector. Pre-flagged over-max values (unused
#'   internally; kept for API compatibility).
#' @param maximum_allowed Numeric. Pre-flagged maximum cap (unused internally).
#' @param logical Logical. When `TRUE` return a logical flag vector; when
#'   `FALSE` return values with non-outliers set to `NA` (default: `FALSE`).
#' @param .progress Logical. Update Shiny progress bar (default: `FALSE`).
#' @param total Integer. Total series count for progress increment calculation.
#'
#' @return Logical or numeric vector the same length as `x`.
#' @export
extremely_mad <- function(x,
                          deviation       = 15,
                          smallThreshold  = 50,
                          key_entry_error = NA,
                          over_max        = NA,
                          maximum_allowed = NA,
                          logical         = FALSE,
                          .progress       = FALSE,
                          total           = NA) {
  if (.progress) {
    .mg2_scan_state$n <- .mg2_scan_state$n + 1L
    now <- proc.time()[["elapsed"]]
    if (now - .mg2_scan_state$last_t >= .mg2_scan_state$interval) {
      .mg2_scan_state$last_t <- now
      pct <- min(.mg2_scan_state$n / max(.mg2_scan_state$total, 1L), 1)
      shiny::setProgress(
        value  = pct,
        detail = sprintf("%d of %d series (%.0f%%)",
                         .mg2_scan_state$n, .mg2_scan_state$total, pct * 100)
      )
    }
  }

  y <- x

  # All-small series: skip
  if (!is.null(smallThreshold) && all(y <= smallThreshold | is.na(y))) {
    return(if (logical) rep(NA, length(y)) else y)
  }

  medianVal          <- median(y, na.rm = TRUE)
  medianAbsDeviation <- mad(y, na.rm = TRUE)

  # When MAD is negligible, fall back to trimmed SD × 0.6745
  if (medianAbsDeviation < .01 * medianVal) {
    q01 <- quantile(x, .01, na.rm = TRUE)
    q99 <- quantile(x, .99, na.rm = TRUE)
    medianAbsDeviation <- 0.6745 * sd(x[x > q01 & x < q99], na.rm = TRUE)
  }

  extreme <- y > (medianVal + deviation * medianAbsDeviation) |
             y < (medianVal - deviation * medianAbsDeviation)

  if (logical) return(extreme)

  y[!extreme] <- NA
  return(y)
}


#' Detect Seasonally-Adjusted Outliers
#'
#' Uses `forecast::tsclean()` to produce a seasonally-adjusted forecast for
#' each series and flags values that deviate from the forecast by more than
#' `deviation × MAD`.
#'
#' @param x Numeric vector (one time series).
#' @param smallThreshold Numeric. Skip series with all values ≤ this
#'   (default: `100`).
#' @param deviation Numeric. MAD multiplier for outlier threshold (default: `3`).
#' @param logical Logical. Return logical flag when `TRUE`, cleaned series when
#'   `FALSE` (default: `FALSE`).
#' @param interpolate Logical. Interpolate missing values (default: `FALSE`).
#' @param .lambda Numeric. Box-Cox lambda passed to `tsclean` (default: `0.5`).
#' @param .progress Logical. Update Shiny progress bar (default: `FALSE`).
#' @param total Integer. Total series count for progress increment calculation.
#'
#' @return Logical or numeric vector the same length as `x`.
#' @export
unseasonal <- function(x,
                       smallThreshold = 100,
                       deviation      = 3,
                       logical        = FALSE,
                       interpolate    = FALSE,
                       .lambda        = 0.5,
                       .progress      = FALSE,
                       total          = NA) {
  if (.progress && !is.na(total)) {
    shiny::setProgress(detail = "Searching for seasonal outliers within each orgUnit")
    shiny::incProgress(amount = 1 / total)
  }

  if (all(is.na(x))) {
    return(if (logical) rep(NA, length(x)) else x)
  }

  if (!is.null(smallThreshold) && all(x <= smallThreshold | is.na(x))) {
    return(if (logical) rep(NA, length(x)) else x)
  }

  if (!requireNamespace("forecast", quietly = TRUE)) {
    warning("Package 'forecast' is required for seasonal outlier detection.")
    return(if (logical) rep(NA, length(x)) else x)
  }

  x.ts       <- stats::ts(x, frequency = 12)
  x.forecast <- as.integer(forecast::tsclean(x.ts,
                                             replace.missing = interpolate,
                                             lambda          = .lambda))

  if (!logical) return(x.forecast)

  MAD     <- mad(x, na.rm = TRUE)
  outlier <- abs((x.forecast - x.ts) / MAD) >= deviation

  return(outlier)
}


#' Flag MAD-Based Outliers Across All Series in a tsibble
#'
#' Applies [extremely_mad()] across every org unit × data element series,
#' flagging key-entry errors, over-max values, and MAD extremes at 15×, 10×,
#' and 5× thresholds.
#'
#' @param d A tsibble (output of [data_1()]).
#' @param .total Integer. Number of series (used for progress). If `NULL`,
#'   computed automatically from `tsibble::n_keys(d)`.
#' @param .threshold Numeric. All-small threshold; series with all values ≤
#'   this are skipped (default: `50`).
#' @param key_entry_errors Numeric vector. Pre-specified key-entry error values.
#'   If `NULL`, detected automatically.
#' @param progress Logical. Show Shiny progress increments (default: `TRUE`).
#'
#' @return The input tsibble with additional logical flag columns:
#'   `key_entry_error`, `over_max`, `mad15`, `mad10`, `mad5`.
#' @export
mad_outliers <- function(d,
                         .total          = NULL,
                         .threshold      = 50,
                         key_entry_errors = NULL,
                         progress        = TRUE) {
  if (is.null(.total)) {
    .total <- if (tsibble::is_tsibble(d)) tsibble::n_keys(d) else
      data.table::uniqueN(data.table::as.data.table(d), by = c("orgUnit", "data.id"))
  }

  # Initialise time-throttled progress state for this scan
  .mg2_scan_state$n      <- 0L
  .mg2_scan_state$total  <- .total
  .mg2_scan_state$last_t <- proc.time()[["elapsed"]]

  if (is.null(key_entry_errors)) {
    large_values <- dplyr::count(
      dplyr::ungroup(tibble::as_tibble(
        dplyr::filter(d, nchar(as.character(original)) > 3, effectiveLeaf)
      )),
      original
    ) |>
      dplyr::arrange(-n)

    top10_median <- median(
      dplyr::pull(dplyr::filter(large_values, dplyr::row_number() < 11), n),
      na.rm = TRUE
    )

    key_entry_errors <- dplyr::pull(
      dplyr::filter(large_values, n > 3 * top10_median),
      original
    )

    if (rlang::is_empty(key_entry_errors)) key_entry_errors <- NA
  }

  # ── data.table fast path ─────────────────────────────────────────────────
  if (requireNamespace("data.table", quietly = TRUE)) {
    if (tsibble::is_tsibble(d)) {
      key_vars <- tsibble::key_vars(d)
      idx_var  <- tsibble::index_var(d)
    } else {
      key_vars <- c("orgUnit", "data.id")
      idx_var  <- if ("Month" %in% names(d)) "Month" else "Week"
    }
    dt       <- data.table::as.data.table(tibble::as_tibble(d))

    # Row-level (no grouping): .max cap, key_entry_error, over_max
    dt[, .max := dplyr::if_else(
      grepl("stock", data, ignore.case = TRUE) &
        grepl("out|rupture", data, ignore.case = TRUE) &
        effectiveLeaf,
      31, NA_real_
    )]

    if (!all(is.na(key_entry_errors))) {
      dt[, key_entry_error := dplyr::if_else(!is.na(original),
                                              original %in% key_entry_errors, NA)]
    } else {
      dt[, key_entry_error := NA]
    }

    dt[, over_max := dplyr::if_else(!is.na(.max) & !is.na(original),
                                    original > .max, NA)]

    # Group-level: AllSmall (scalar per group, broadcast to rows)
    dt[, AllSmall := !is.null(.threshold) &&
         all(is.na(original) | original <= .threshold),
       by = key_vars]

    # Row-level: not_key_or_over_under (uses AllSmall already set above)
    dt[, not_key_or_over_under := dplyr::if_else(
      (is.na(over_max) | !over_max) &
        (is.na(key_entry_error) | !key_entry_error) &
        !AllSmall,
      original, NA_real_
    )]

    # Group-level: MAD outlier flags via extremely_mad()
    dt[, mad15 := extremely_mad(
      not_key_or_over_under,
      deviation       = 15,
      smallThreshold  = .threshold,
      key_entry_error = key_entry_error,
      over_max        = over_max,
      maximum_allowed = .max,
      logical         = TRUE,
      .progress       = progress,
      total           = .total
    ), by = key_vars]

    dt[, mad10 := extremely_mad(
      not_key_or_over_under,
      deviation       = 10,
      smallThreshold  = .threshold,
      maximum_allowed = .max,
      logical         = TRUE,
      .progress       = FALSE
    ), by = key_vars]

    dt[, mad5 := extremely_mad(
      not_key_or_over_under,
      deviation       = 5,
      smallThreshold  = .threshold,
      maximum_allowed = .max,
      logical         = TRUE,
      .progress       = FALSE
    ), by = key_vars]

    m_o <- tsibble::as_tsibble(
      tibble::as_tibble(dt),
      key   = dplyr::all_of(key_vars),
      index = dplyr::all_of(idx_var)
    )

    return(m_o)
  }

  # ── Fallback: original tsibble grouped-mutate path ────────────────────────
  m_o <- d |>
    tsibble::group_by_key() |>
    dplyr::mutate(
      .max = dplyr::if_else(
        grepl("stock", data, ignore.case = TRUE) &
          grepl("out|rupture", data, ignore.case = TRUE) &
          effectiveLeaf,
        31, NA_real_
      ),
      key_entry_error = dplyr::if_else(
        !all(is.na(key_entry_errors)) & !is.na(original),
        original %in% key_entry_errors,
        NA
      ),
      over_max = dplyr::if_else(
        !is.na(.max) & !is.na(original),
        original > .max,
        NA
      ),
      AllSmall = !is.null(.threshold) &&
        all(is.na(original) | original <= .threshold),
      not_key_or_over_under = dplyr::if_else(
        (is.na(over_max) | !over_max) &
          (is.na(key_entry_error) | !key_entry_error) &
          !AllSmall,
        original, NA_real_
      ),
      mad15 = extremely_mad(
        not_key_or_over_under,
        deviation       = 15,
        smallThreshold  = .threshold,
        key_entry_error = key_entry_error,
        over_max        = over_max,
        maximum_allowed = .max,
        logical         = TRUE,
        .progress       = progress,
        total           = .total
      ),
      mad10 = extremely_mad(
        not_key_or_over_under,
        deviation       = 10,
        smallThreshold  = .threshold,
        maximum_allowed = .max,
        logical         = TRUE,
        .progress       = FALSE
      ),
      mad5 = extremely_mad(
        not_key_or_over_under,
        deviation       = 5,
        smallThreshold  = .threshold,
        maximum_allowed = .max,
        logical         = TRUE,
        .progress       = FALSE
      )
    )

  return(m_o)
}


#' Flag Seasonal Outliers Across All Series in a tsibble
#'
#' Applies [unseasonal()] to every series, flagging values that deviate from a
#' seasonal forecast by more than 5× MAD (`seasonal5`) and 3× MAD
#' (`seasonal3`).
#'
#' @param d A tsibble (output of [mad_outliers()]).
#' @param .total Integer. Number of series (used for progress). If `NULL`,
#'   computed from `tsibble::n_keys(d)`.
#' @param .threshold Numeric. All-small threshold (default: `50`).
#' @param mad Character. Name of MAD flag column to exclude before seasonal
#'   detection (default: `"mad10"`).
#' @param tests Character vector. Which seasonal tests to run; subset of
#'   `c("seasonal5", "seasonal3")` (default: both).
#' @param progress Logical. Use `progressr` progress bar (default: `FALSE`).
#'
#' @return The input tsibble with `not_mad`, `seasonal5`, and `seasonal3`
#'   columns added.
#' @export
seasonal_outliers <- function(d,
                              .total         = NULL,
                              .threshold     = 50,
                              mad            = "mad10",
                              tests          = c("seasonal5", "seasonal3"),
                              progress       = FALSE,
                              shiny_progress = FALSE) {

  if (is.null(.total)) {
    .total <- if (tsibble::is_tsibble(d)) tsibble::n_keys(d) else
      data.table::uniqueN(data.table::as.data.table(d), by = c("orgUnit", "data.id"))
  }
  if (tsibble::is_tsibble(d)) {
    key_vars <- tsibble::key_vars(d)
    idx_var  <- tsibble::index_var(d)
  } else {
    key_vars <- c("orgUnit", "data.id")
    idx_var  <- if ("Month" %in% names(d)) "Month" else "Week"
  }

  # Determine whether parallel execution is available
  use_parallel <- .total > 1L &&
    requireNamespace("furrr",  quietly = TRUE) &&
    requireNamespace("future", quietly = TRUE)

  n_workers <- if (use_parallel) {
    max(1L, parallelly::availableCores() - 1L)
  } else 1L

  use_parallel <- use_parallel && n_workers > 1L

  # Initialise Shiny progress
  if (shiny_progress) {
    .mg2_scan_state$n      <- 0L
    .mg2_scan_state$total  <- .total
    .mg2_scan_state$last_t <- proc.time()[["elapsed"]]
    shiny::setProgress(
      value  = 0,
      detail = if (use_parallel)
        sprintf("%d series on %d cores...", .total, n_workers)
      else
        "starting"
    )
  }

  # Capture unseasonal() explicitly so furrr workers can find it regardless
  # of whether the package was loaded via install or devtools::load_all()
  .unseasonal <- unseasonal

  # Per-series worker — closed over mad, .threshold, tests, .unseasonal
  .process_one <- function(s) {
    s$not_mad   <- dplyr::if_else(!s[[mad]], s$original, NA_real_)
    s$seasonal5 <- if ("seasonal5" %in% tests)
      .unseasonal(s$not_mad, smallThreshold = .threshold, deviation = 5, logical = TRUE)
    else rep(NA, nrow(s))
    s$seasonal3 <- if ("seasonal3" %in% tests)
      .unseasonal(s$not_mad, smallThreshold = .threshold, deviation = 3, logical = TRUE)
    else rep(NA, nrow(s))
    s
  }

  # Split tsibble into a list of per-key tibbles
  d_tbl       <- tibble::as_tibble(d)
  series_list <- dplyr::group_split(
    d_tbl, dplyr::across(dplyr::all_of(key_vars)), .keep = TRUE
  )

  if (use_parallel) {
    # ── Parallel path ───────────────────────────────────────────────────────
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = n_workers)

    results <- furrr::future_map(
      series_list,
      .process_one,
      .options = furrr::furrr_options(seed = NULL, packages = "forecast")
    )

    if (shiny_progress)
      shiny::setProgress(value = 1, detail = sprintf("%d series complete", .total))

  } else {
    # ── Sequential path with time-throttled progress ─────────────────────
    results <- vector("list", length(series_list))
    for (i in seq_along(series_list)) {
      results[[i]] <- .process_one(series_list[[i]])

      if (shiny_progress) {
        .mg2_scan_state$n <- .mg2_scan_state$n + 1L
        now <- proc.time()[["elapsed"]]
        if (now - .mg2_scan_state$last_t >= .mg2_scan_state$interval) {
          .mg2_scan_state$last_t <- now
          pct <- min(.mg2_scan_state$n / max(.mg2_scan_state$total, 1L), 1)
          shiny::setProgress(
            value  = pct,
            detail = sprintf("%d of %d series (%.0f%%)",
                             .mg2_scan_state$n, .mg2_scan_state$total, pct * 100)
          )
        }
      }
    }
  }

  # Reconstruct tsibble from list of per-key tibbles
  data1.seasonal <- dplyr::bind_rows(results) |>
    tsibble::as_tsibble(
      key   = dplyr::all_of(key_vars),
      index = dplyr::all_of(idx_var)
    )

  return(data1.seasonal)
}
