# Data Quality Assessment functions for MG2.
# Migrated from R/originals/dqa_functions.R

# Reporting -------------------------------------------------------------------

#' Extract Unique Org Units from DQA Data
#' @noRd
data_ous <- function(dqa_data) {
  dqa_data |>
    tibble::as_tibble() |>
    dplyr::ungroup() |>
    dplyr::count(orgUnit, orgUnitName, level) |>
    dplyr::select(-n)
}

#' Extract Years Present in DQA Data
#' @noRd
dqa_years <- function(dqa_data) {
  dqa_data |>
    tibble::as_tibble() |>
    dplyr::ungroup() |>
    dplyr::count(Year = lubridate::year(Month)) |>
    dplyr::select(-n)
}

#' Count Consistently Reporting Facilities per Year
#'
#' @param dqa_data tsibble of prepared data.
#' @param missing_reports Integer. Allowed missing months (default: `0`).
#' @param count.any Logical. Count facilities reporting any data element
#'   (default: `TRUE`).
#' @param .cat Logical. Print progress messages (default: `FALSE`).
#'
#' @return Numeric vector with count of consistently reporting facilities per year.
#' @export
dqa_reporting <- function(dqa_data, missing_reports = 0, count.any = TRUE,
                          .cat = FALSE, ...) {
  startingMonth <- tsibble::yearmonth(
    paste0(dqa_years(dqa_data)$Year, "Jan")
  )

  endingMonth <- dqa_data |>
    tibble::as_tibble() |>
    dplyr::ungroup() |>
    dplyr::group_by(year = lubridate::year(Month)) |>
    dplyr::summarize(latest_month = max(Month), .groups = "drop") |>
    dplyr::pull(latest_month)

  endingMonth[length(endingMonth)] <- endingMonth[length(endingMonth)] - 1

  if (.cat) cat("\n *dqa_reporting - mostFrequentReportingOUs")

  reportingOUS <- purrr::map(
    seq_along(startingMonth),
    ~ mostFrequentReportingOUs(
      data           = dqa_data,
      startingMonth  = startingMonth[.x],
      endingMonth    = endingMonth[.x],
      missing_reports = missing_reports,
      count.any      = count.any,
      .cat           = FALSE
    )
  )

  purrr::map_dbl(reportingOUS, length)
}


#' Compute Percent of Facilities Consistently Reporting per Year
#'
#' @param dqa_data tsibble of prepared data.
#' @param .cat Logical. Print progress messages (default: `FALSE`).
#'
#' @return A tibble with columns `Year`, `n_frequently_reporting`,
#'   `n_facilities`, `pr`, `label`.
#' @export
dqaPercentReporting <- function(dqa_data, .cat = FALSE) {
  if (.cat) cat('\n*  dqa_functions.R dqaPercentReporting')

  year <- dqa_years(dqa_data)
  if (.cat) cat('\n -  years:', paste(year$Year, collapse = ","))

  n_frequently_reporting <- dqa_reporting(dqa_data, missing_reports = 0)
  n_facilities           <- length(unique(dqa_data$orgUnit))
  pr                     <- n_frequently_reporting / n_facilities

  if (.cat) cat('\n -  pr:', paste(pr, collapse = ","))

  data <- tibble::tibble(
    year,
    n_frequently_reporting,
    n_facilities,
    pr,
    label = scales::percent(pr, 0.1)
  )

  if (.cat) print(data)
  return(data)
}


#' Plot Percent of Facilities Consistently Reporting per Year
#'
#' @param data Output of [dqaPercentReporting()].
#' @param text_size Numeric. Base text size for the plot (default: `18`).
#'
#' @return A ggplot object.
#' @export
dqa_reporting_plot <- function(data, text_size = 18) {
  n_facilities <- max(data$n_facilities)

  ggplot2::ggplot(
    data = data,
    ggplot2::aes(x = as.character(Year), y = pr, label = label, group = 1)
  ) +
    ggplot2::geom_line(linewidth = 1.25) +
    ggplot2::geom_text(vjust = -1, size = text_size / 3) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0.0, 0.2))
    ) +
    ggplot2::labs(
      x        = "Year",
      y        = "Percent",
      title    = "Percent of facilities consistently reporting each year",
      subtitle = paste(
        'Out of the number of facilities that have ever reported (',
        scales::comma(n_facilities), ")"
      )
    ) +
    ggplot2::theme_minimal(base_size = text_size)
}


# Outliers --------------------------------------------------------------------

#' Summarise Yearly Outlier Flags from DQA Data
#'
#' @param yearly.outlier.summary Output of `yearly.outlier.summary()`.
#'
#' @return A tibble with percent-no-error columns added.
#' @export
dqa_outliers <- function(yearly.outlier.summary) {
  yearly.outlier.summary |>
    dplyr::filter(data %in% "All") |>
    dplyr::mutate(
      percent_no_error             = 1 - pn,
      percent_no_error_chr         = scales::percent(percent_no_error, 0.1),
      percent_value_no_error       = pe,
      percent_value_no_error_chr   = scales::percent(percent_value_no_error, 0.1)
    )
}


#' Plot Yearly Outlier Summary
#'
#' @param data Output of [dqa_outliers()].
#' @param text_size Numeric. Base text size (default: `18`).
#' @param label_size Numeric. Geom text size (default: `6`).
#'
#' @return A ggplot object.
#' @export
yearly.outlier.summary_plot <- function(data, text_size = 18, label_size = 6) {
  data <- data |>
    dplyr::select(year, dplyr::starts_with("percent") & !dplyr::ends_with("chr")) |>
    tidyr::pivot_longer(-year) |>
    dplyr::mutate(label = scales::percent(value, 0.1))

  ggplot2::ggplot(
    data = data,
    ggplot2::aes(x = as.character(year), y = value, color = name,
                 label = label, group = name)
  ) +
    ggplot2::geom_line(linewidth = 1.25) +
    ggplot2::geom_text(vjust = -1.5, size = label_size) +
    ggplot2::annotate(
      "text", x = -1,
      y = max(data[grepl("value", data$name), ]$value),
      label = "Magnitude of flagged values relative to sum of all values",
      vjust = -3.5, hjust = -0.5, size = label_size
    ) +
    ggplot2::annotate(
      "text", x = -1,
      y = min(data[!grepl("value", data$name), ]$value),
      label = "Percentage of values with NO error flags",
      vjust = 2, hjust = -1, size = label_size
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0.1, 0.2))
    ) +
    ggplot2::scale_color_hue(l = 40, c = 35) +
    ggplot2::guides(color = "none") +
    ggplot2::labs(
      x        = "Year",
      y        = "Percent",
      title    = "Potential Reporting Errors",
      subtitle = "Data flagged as incorrect through outlier algorithms"
    ) +
    ggplot2::theme_minimal(base_size = text_size)
}


# MASE ------------------------------------------------------------------------

#' Absolute Error Helper
#' @noRd
abs_ae <- function(actual, predicted) abs(actual - predicted)

#' Mean Absolute Scaled Error
#' @noRd
mase <- function(actual, predicted, step_size = 1) {
  if (all(is.na(predicted))) return(NA_real_)
  n           <- length(actual)
  naive_start <- step_size + 1
  naive_end   <- n - step_size
  sum_errors  <- sum(abs_ae(actual, predicted), na.rm = TRUE)
  naive_errors <- sum(abs_ae(actual[naive_start:n], actual[1:naive_end]),
                      na.rm = TRUE)
  sum_errors / (n * naive_errors / naive_end)
}

#' Compute MASE for a Single Year
#' @noRd
mase_year <- function(dqa_data, .year) {
  d_all <- data.table::setDT(tibble::as_tibble(dqa_data))[
    lubridate::year(Month) <= .year,
    .(
      expected = sum(expected, na.rm = TRUE),
      original = sum(original, na.rm = TRUE)
    ),
    by = c("orgUnit", "orgUnitName", "Month")
  ] |>
    tibble::as_tibble() |>
    dplyr::group_by(orgUnit, orgUnitName)

  d.mase <- data.table::setDT(d_all)[,
    .(
      MASE           = mase(actual = original, predicted = expected),
      n              = sum(!is.na(original)),
      total_expected = sum(expected, na.rm = TRUE)
    ),
    by = c("orgUnit", "orgUnitName")
  ] |>
    tibble::as_tibble()

  mean.mase <- weighted.mean(
    d.mase$MASE[d.mase$MASE < Inf],
    w  = d.mase$total_expected[d.mase$MASE < Inf],
    na.rm = TRUE
  )

  tibble::tibble(
    Year        = .year,
    Facilities  = nrow(d.mase),
    Mean_MASE   = mean.mase,
    label       = scales::percent(mean.mase, 0.1)
  )
}


#' Compute MASE Across All Years in DQA Data
#'
#' Requires an `expected` column (from seasonal cleaning). Returns `NULL`
#' with a message when the column is absent.
#'
#' @param dqa_data tsibble of prepared data (must have `expected` column).
#'
#' @return A tibble with one row per year, or `NULL` if `expected` is absent.
#' @export
dqa_mase <- function(dqa_data) {
  if (!"expected" %in% names(dqa_data)) {
    message("dqa_mase: 'expected' column not found — MASE plot requires seasonal cleaning first.")
    return(NULL)
  }
  years <- dqa_years(dqa_data)$Year
  result <- purrr::map_df(years, ~ mase_year(dqa_data, .x))
  result[1:2, 3:ncol(result)] <- NA
  return(result)
}


#' Plot MASE Summary
#'
#' @param data Output of [dqa_mase()].
#'
#' @return A ggplot object.
#' @export
dqa_mase_plot <- function(data) {
  if (is.null(data)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "MASE requires seasonal cleaning (expected column not available)",
                          size = 5) +
        ggplot2::theme_void()
    )
  }
  mase_txt <- paste(
    "Estimated as 2x the mean absolute scaled error (MASE) of the previous values\n",
    "- The smaller this value, the more accurate the data is\n",
    "- Year to year change less than this is likely due to random variation"
  )

  ggplot2::ggplot(
    data = data,
    ggplot2::aes(x = as.character(Year), y = Mean_MASE, label = label, group = 1)
  ) +
    ggplot2::geom_line(linewidth = 1.25) +
    ggplot2::geom_text(vjust = -1, size = 6) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1.5 * max(data$Mean_MASE, na.rm = TRUE))
    ) +
    ggplot2::labs(
      x        = "Year",
      y        = "Percent",
      title    = "Minimum Detectable Change for Program Evaluation",
      subtitle = mase_txt,
      caption  = "NOTE: MASE calculated beginning with 3rd year of data"
    ) +
    ggplot2::theme_minimal(base_size = 18)
}
