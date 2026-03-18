# Outlier summary functions for MG2.
# Migrated from R/originals/cleaning_functions.R

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
