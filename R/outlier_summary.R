# Outlier summary functions for MG2.
# Migrated from R/originals/cleaning_functions.R and R/originals/Cleaning.R

#' Summarise Outlier Flags as a Formatted Tibble
#'
#' Builds a display table showing counts, totals, and percentages for each
#' outlier flag column.  Columns absent from `data` are silently dropped from
#' `cols`.
#'
#' @param data A data frame / tsibble containing an `original` value column and
#'   logical outlier-flag columns (e.g. `mad15`, `seasonal5`).
#' @param cols Character vector of flag column names to include.
#' @param .print Logical. Print a progress message (default: `FALSE`).
#'
#' @return A character tibble with one row per flag, formatted for display.
#' @export
outlier.summary.tibble <- function(
  data = NULL,
  cols = c(
    "AllSmall",
    "key_entry_error",
    "over_max",
    "missing_numerator",
    "missing_denominator",
    "mad15",
    "mad10",
    "mad5",
    "seasonal5",
    "seasonal3"
  ),
  .print = FALSE
) {
  if (.print) cat("\n * outlier.summary.tibble")

  warn <- options(warn = -1)  # suppress divide-by-zero warnings
  on.exit(options(warn))

  # Drop cols not present in data
  if ("expected" %in% cols) cols <- setdiff(cols, "expected")
  if (!"mad5"     %in% names(data)) cols <- setdiff(cols, "mad5")
  if (!"AllSmall" %in% names(data)) cols <- setdiff(cols, "AllSmall")
  cols <- intersect(cols, names(data))

  dt <- data.table::as.data.table(data)

  # For ratio variables, N = max(non-missing numerator count, non-missing
  # denominator count) — i.e. the count of facility-months where either side
  # of the ratio existed.  For other variables, N = non-missing original count.
  has_ratio_cols <- all(c("numerator", "denominator") %in% names(dt))
  os.total <- if (has_ratio_cols) {
    dt[, .(Total = sum(original, na.rm = TRUE),
           N     = max(sum(!is.na(numerator)), sum(!is.na(denominator))))]
  } else {
    dt[, .(Total = sum(original, na.rm = TRUE),
           N     = sum(!is.na(original)))]
  }

  if ("AllSmall" %in% names(data)) {
    os.notSmall <- dt[AllSmall == FALSE,
                      .(Total.notSmall = sum(original, na.rm = TRUE),
                        N.notSmall     = sum(!is.na(original)))]
  } else {
    os.notSmall <- dt[, .(Total.notSmall = sum(original, na.rm = TRUE),
                           N.notSmall     = sum(!is.na(original)))]
  }

  os <- dt[, .(
    n      = sum(!is.na(original)),
    total  = sum(original, na.rm = TRUE),
    Min    = min(original,    na.rm = TRUE),
    Median = median(original, na.rm = TRUE),
    Max    = max(original,    na.rm = TRUE)
  ), by = cols] |> tibble::as_tibble()

  # Rows with no error flag at all
  flag_cols <- if ("AllSmall" %in% cols) setdiff(cols, "AllSmall") else cols
  no.err.rows <- purrr::map(
    dplyr::select(os, dplyr::all_of(flag_cols)),
    ~ which(is.na(.x) | .x == FALSE)
  ) |> purrr::reduce(intersect)

  os.no.err <- os[no.err.rows, ] |>
    dplyr::summarise(
      n      = sum(n,     na.rm = TRUE),
      total  = sum(total, na.rm = TRUE),
      Min    = min(Min,   na.rm = TRUE),
      Median = max(Median, na.rm = TRUE),
      Max    = max(Max,   na.rm = TRUE)
    ) |>
    dplyr::mutate(err = "No Error Flags")

  os.errs <- purrr::map_df(cols, ~ {
    r <- dplyr::filter(os, !!rlang::sym(.x) == TRUE)
    if (nrow(r) == 0) r <- tibble::add_row(os[0, ])
    dplyr::bind_cols(tibble::tibble(err = .x), r)
  }) |>
    dplyr::mutate(err = factor(err, levels = unique(err))) |>
    dplyr::group_by(err) |>
    dplyr::summarise(
      n      = sum(n,      na.rm = TRUE),
      total  = sum(total,  na.rm = TRUE),
      Min    = min(Min,    na.rm = TRUE),
      Median = median(Median, na.rm = TRUE),
      Max    = max(Max,    na.rm = TRUE),
      .groups = "drop"
    )

  out <- dplyr::bind_rows(os.errs, os.no.err) |>
    dplyr::bind_cols(os.total, os.notSmall) |>
    dplyr::mutate(
      `%N` = ifelse(
        n > 0,
        scales::percent(n / N, accuracy = 0.01),
        as.character(-Inf)
      ),
      `%Total Value` = ifelse(
        n > 0,
        scales::percent(total / Total, accuracy = 0.01),
        as.character(-Inf)
      ),
      N              = scales::comma(n),
      `Total Value`  = scales::comma(total),
      `Smallest Value` = scales::comma(Min),
      `Median Value`   = scales::comma(Median),
      `Largest Value`  = scales::comma(Max)
    ) |>
    dplyr::select(
      err, N, `%N`, `Total Value`, `%Total Value`,
      `Smallest Value`, `Median Value`, `Largest Value`
    ) |>
    dplyr::rename(`Error Flag` = err) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  out[out == "-Inf"] <- "-"
  out
}

#' Summarise Outlier Flags by Month, Data Element, and Algorithm
#'
#' Computes monthly counts and proportions of flagged values across all
#' error-flag columns (`mad15`, `mad10`, `seasonal5`, `seasonal3`) plus a
#' combined flag.
#'
#' @param df.ts A tsibble (output of [data_1()] after outlier detection).
#' @param err.cols Tidy-select expression. Error flag columns to summarise
#'   (default: `c(mad15, mad10, seasonal5, seasonal3)`).
#' @param .cat Logical. Print progress messages (default: `TRUE`).
#'
#' @return A tsibble keyed by `data × Alg` with monthly error summaries.
#' @export
monthly.outlier.summary <- function(df.ts,
                                    err.cols = c(mad15, mad10, seasonal5, seasonal3),
                                    .cat     = TRUE) {
  if (.cat) cat("\n * monthly.outlier.summary")

  base <- tibble::as_tibble(df.ts) |> dplyr::ungroup() |>
    dplyr::filter(effectiveLeaf) |>
    dplyr::select(Month, data.id, data, value, {{ err.cols }}, original)

  if (.cat) cat("\n - errors_by_data_and_algorithm")
  errors_by_data <- base |>
    dplyr::mutate(
      Combined = purrr::reduce(dplyr::across({{ err.cols }}), pmax, .init = -Inf) > 0
    ) |>
    tidyr::pivot_longer(
      cols      = c({{ err.cols }}, Combined),
      names_to  = "Alg",
      values_to = "val"
    ) |>
    dplyr::filter(val == TRUE) |>
    dplyr::group_by(Month, data, Alg) |>
    dplyr::summarise(e = sum(original, na.rm = TRUE), n.e = dplyr::n(),
                     .groups = "keep")

  if (.cat) cat("\n - data.totals_by_data")
  data_totals <- base |>
    dplyr::group_by(Month, data) |>
    dplyr::summarise(t = sum(original, na.rm = TRUE), n.t = dplyr::n(),
                     .groups = "keep")

  if (.cat) cat("\n - totals for all data combined")
  errors_all <- errors_by_data |>
    dplyr::group_by(Month, Alg) |>
    dplyr::summarise(dplyr::across(c(e, n.e), sum, na.rm = TRUE),
                     .groups = "keep") |>
    dplyr::mutate(data = "All")

  totals_all <- data_totals |>
    dplyr::group_by(Month) |>
    dplyr::summarise(dplyr::across(c(t, n.t), sum, na.rm = TRUE),
                     .groups = "keep") |>
    dplyr::mutate(data = "All")

  if (.cat) cat("\n - grand.data.error.totals")
  grand <- dplyr::bind_rows(
    dplyr::left_join(data_totals,  errors_by_data, by = c("data", "Month")) |>
      dplyr::filter(!is.na(Alg)),
    dplyr::left_join(totals_all, errors_all,     by = c("data", "Month"))
  ) |>
    dplyr::mutate(
      pe    = e / t,
      pn    = n.e / n.t,
      year  = lubridate::year(Month),
      month = lubridate::month(Month)
    ) |>
    tsibble::as_tsibble(index = Month, key = c(data, Alg))

  return(grand)
}


#' Summarise Outlier Flags by Year
#'
#' Aggregates [monthly.outlier.summary()] output to yearly totals for the
#' combined flag.
#'
#' @param grand.data.error.totals Output of [monthly.outlier.summary()].
#' @param .cat Logical. Print progress messages (default: `TRUE`).
#'
#' @return A tibble with yearly error summaries.
#' @export
yearly.outlier.summary <- function(grand.data.error.totals, .cat = TRUE) {
  if (.cat) cat("\n * yearly.outlier.summary")

  grand.data.error.totals |>
    tibble::as_tibble() |>
    dplyr::filter(Alg %in% "Combined") |>
    dplyr::group_by(year, data) |>
    dplyr::summarise(dplyr::across(c(e, n.e, t, n.t), sum, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(pe = e / t, pn = n.e / n.t)
}


#' Plot Monthly Outlier Summaries by Year and Algorithm
#'
#' @param grand.data.error.totals Output of [monthly.outlier.summary()].
#'
#' @return A ggplot object.
#' @export
outlier.summary.chart <- function(grand.data.error.totals) {
  yearly <- yearly.outlier.summary(grand.data.error.totals)

  ggplot2::ggplot(grand.data.error.totals) +
    ggplot2::geom_line(
      ggplot2::aes(x = as.factor(month), y = pe, color = Alg, group = Alg)
    ) +
    ggplot2::facet_grid(year ~ data, labeller = ggplot2::label_wrap_gen()) +
    ggplot2::geom_text(
      data  = yearly,
      ggplot2::aes(
        x     = 8,
        y     = 0.65,
        label = paste(
          "%Err(n) =",     scales::percent(pn, accuracy = 0.1),
          "\n",
          "%Err(value) =", scales::percent(pe, accuracy = 1)
        )
      ),
      size = 3
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title    = "Potential errors (outliers) among reported values",
      subtitle = "by year, data, and error detection algorithm",
      x        = "Month",
      y        = "Percent\n",
      caption  = paste(
        "%Err(n) = percent of values flagged as potential errors\n",
        "%Err(value) = percent of total value flagged as potential errors"
      )
    )
}
