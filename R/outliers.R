# Outlier detection functions for MG2.
# Migrated from R/originals/Cleaning.R

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
  if (.progress && !is.na(total)) {
    shiny::setProgress(detail = "Searching for extreme values within each orgUnit")
    shiny::incProgress(amount = 1 / total)
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
  if (is.null(.total)) .total <- tsibble::n_keys(d)

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
                              .total    = NULL,
                              .threshold = 50,
                              mad       = "mad10",
                              tests     = c("seasonal5", "seasonal3"),
                              progress  = FALSE) {
  mad_sym <- rlang::sym(mad)

  if (is.null(.total)) .total <- tsibble::n_keys(d)

  data1.seasonal <- d |>
    tsibble::group_by_key() |>
    dplyr::group_modify(~ {
      .x <- dplyr::mutate(.x,
        not_mad = dplyr::if_else(!{{ mad_sym }}, original, NA_real_)
      )
      .x <- dplyr::mutate(.x,
        seasonal5 = if ("seasonal5" %in% tests) {
          unseasonal(not_mad, smallThreshold = .threshold,
                     deviation = 5, logical = TRUE)
        } else NA,
        seasonal3 = if ("seasonal3" %in% tests) {
          unseasonal(not_mad, smallThreshold = .threshold,
                     deviation = 3, logical = TRUE)
        } else NA
      )
      return(.x)
    })

  return(data1.seasonal)
}
