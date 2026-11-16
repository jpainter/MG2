# Champion totals: the reporting-bias-adjusted series built from champion facilities.

#' Sum champion facilities' values to modelling units
#'
#' Totals per modelling unit and period over the facilities chosen by
#' [mostFrequentReportingOUs()]. Missing values are dropped, not imputed, so pass
#' censored values as `NA` (e.g. `value` from `apply_combine_cleaning()`): a unit-period
#' is the sum of its champions' remaining values, and a unit-period with none has no
#' row. Units with no champions have no rows.
#'
#' @param data data frame or data.table with `orgUnit`, the period column and `value`.
#' @param champions character vector of champion org units.
#' @param unit_map data frame with `orgUnit` and `unit`: the modelling unit of each
#'   facility. Facilities absent from it are ignored.
#' @param value character; name of the value column (default `"original"`).
#' @param period character; period column name.
#'
#' @return data.table with `unit`, the period column and `value`, ordered by unit and
#'   period.
#' @export
aggregate_champions <- function(data, champions, unit_map, value = "original", period = "Month") {
  dt <- if (data.table::is.data.table(data)) data else data.table::as.data.table(data)
  map <- data.table::as.data.table(unit_map)[, .(orgUnit, unit)]
  period_class <- class(dt[[period]])
  # Columns by name outside [.data.table: `value` and `period` may also be column names
  d <- data.table::data.table(orgUnit = dt$orgUnit, .p = unclass(dt[[period]]), .v = dt[[value]])[
    orgUnit %chin% champions & !is.na(.v)]
  d <- map[d, on = "orgUnit", nomatch = NULL]
  out <- d[, .(value = sum(.v)), by = .(unit, .p)]
  data.table::setorder(out, unit, .p)
  out[, .p := structure(.p, class = period_class)]
  data.table::setnames(out, ".p", period)
  out[]
}
