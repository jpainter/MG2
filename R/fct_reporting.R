# Reporting: which facility-periods count as reported, and the counts the reporting
# page charts. One definition shared by mostFrequentReportingOUs(), the reporting
# widget and the mg2-dqa service, so the charts describe the same reports the champion
# selection used.

#' Facility-periods counted as reported
#'
#' The reporting rule as applied by [mostFrequentReportingOUs()]: a facility has
#' reported in a period when it has a non-missing `original` value that satisfies
#' `reporting_rule`.
#'
#' - `"any_data"`: any value at all (also used when `data_categories` is empty).
#' - `"any_selected"`: a value for any of `data_categories`.
#' - `"all_elements"`: a value for at least one category of every selected data
#'   element (the element is the part of `data` before the last `"_"`).
#' - `"all_categories"`: a value for every one of `data_categories`.
#'
#' @param data data frame or data.table with `orgUnit`, `data`, `original` and the
#'   period column.
#' @param period character; period column name (`"Month"` or `"Week"`).
#' @param reporting_rule character; see Details.
#' @param data_categories character vector of `data` values the rule refers to.
#'
#' @return A data.table of distinct `orgUnit` x period pairs, with the period column
#'   keeping its class.
#' @export
facility_reports <- function(data, period = "Month", reporting_rule = "any_selected",
                             data_categories = NULL) {
  dt <- if (data.table::is.data.table(data)) data else data.table::as.data.table(data)
  period_class <- class(dt[[period]])
  # Integer surrogate for the period: grouping on vctrs classes falls back to R-level
  # comparison (~10x slower)
  # The period column by name outside [.data.table: `period` may also be a column name
  dt <- data.table::data.table(orgUnit = dt$orgUnit, data = dt$data, .p = unclass(dt[[period]]),
                               original = dt$original)[!is.na(original), .(orgUnit, data, .p)]

  has_cats <- !is.null(data_categories) && length(data_categories) > 0
  if (reporting_rule == "any_data" || !has_cats) {
    rep <- unique(dt[, .(orgUnit, .p)])
  } else if (reporting_rule == "any_selected") {
    rep <- unique(dt[data %chin% data_categories, .(orgUnit, .p)])
  } else if (reporting_rule == "all_elements") {
    sel <- dt[data %chin% data_categories]
    sel[, .base_elem := sub("_[^_]*$", "", data)]
    n_elems <- data.table::uniqueN(sel$.base_elem)
    rep <- sel[, .(n = data.table::uniqueN(.base_elem)), by = .(orgUnit, .p)][n >= n_elems, .(orgUnit, .p)]
  } else { # "all_categories" (and, as before, any other value)
    n_cats <- length(unique(data_categories))
    sel <- dt[data %chin% data_categories]
    rep <- sel[, .(n = data.table::uniqueN(data)), by = .(orgUnit, .p)][n >= n_cats, .(orgUnit, .p)]
  }
  rep[, .p := structure(.p, class = period_class)]
  data.table::setnames(rep, ".p", period)
  rep[]
}

.report_year <- function(x) data.table::year(data.table::as.IDate(unclass(x), origin = "1970-01-01"))

#' Facilities reporting in each period
#'
#' Counts from [facility_reports()] output, as the reporting page's "Number of
#' facilities reporting each period" chart.
#'
#' @param reports output of [facility_reports()].
#' @param period character; period column name.
#' @param champions optional character vector of champion org units; adds a
#'   `champions` count column.
#'
#' @return data.table with `year`, the period column, `all` and (if `champions` is
#'   given) `champions`, ordered by period.
#' @export
reports_by_period <- function(reports, period = "Month", champions = NULL) {
  period_class <- class(reports[[period]])
  r <- data.table::data.table(orgUnit = reports$orgUnit, .p = unclass(reports[[period]]))
  out <- r[, .(all = data.table::uniqueN(orgUnit)), by = .p]
  if (!is.null(champions)) {
    ch <- r[orgUnit %chin% champions, .(champions = data.table::uniqueN(orgUnit)), by = .p]
    out <- merge(out, ch, by = ".p", all.x = TRUE)
    out[is.na(champions), champions := 0L]
  }
  data.table::setorder(out, .p)
  out[, year := .report_year(.p)]
  out[, .p := structure(.p, class = period_class)]
  data.table::setnames(out, ".p", period)
  data.table::setcolorder(out, c("year", period))
  out[]
}

#' Facilities by number of periods reported in each year
#'
#' Counts from [facility_reports()] output, as the reporting page's "Histogram of
#' annual number of months reported".
#'
#' @inheritParams reports_by_period
#'
#' @return data.table with `year`, `n_periods` (periods reported in the year), `all`
#'   (facilities) and (if `champions` is given) `champions`, ordered by year and
#'   `n_periods`.
#' @export
reports_by_year <- function(reports, period = "Month", champions = NULL) {
  per_facility <- data.table::data.table(year = .report_year(reports[[period]]), orgUnit = reports$orgUnit)[
    , .(n_periods = .N), by = .(year, orgUnit)]
  out <- per_facility[, .(all = .N), by = .(year, n_periods)]
  if (!is.null(champions)) {
    ch <- per_facility[orgUnit %chin% champions, .(champions = .N), by = .(year, n_periods)]
    out <- merge(out, ch, by = c("year", "n_periods"), all.x = TRUE)
    out[is.na(champions), champions := 0L]
  }
  data.table::setorder(out, year, n_periods)
  out[]
}
