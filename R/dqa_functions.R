# Data Quality Assessment functions for MG2.
# Migrated from R/originals/dqa_functions.R


# Validation Rule Consistency -------------------------------------------------

#' Extract data.id values referenced in a raw rule expression
#'
#' Handles two formats:
#'   \code{#\{de.coc\}} -- standard DHIS2 format; returns \code{"de_coc"}
#'   \code{#\{de\}}     -- bare UID (data without COC disaggregation); returns \code{"de"}
#' @noRd
.extract_vr_uid_pairs <- function(raw_expr) {
  if (is.null(raw_expr) || is.na(raw_expr) || !nzchar(raw_expr))
    return(character(0))
  uid11 <- "[A-Za-z][A-Za-z0-9]{10}"
  # #{de.coc} -- standard form
  m1 <- gregexpr(
    paste0("#\\{(", uid11, ")\\.(", uid11, ")\\}"),
    raw_expr, perl = TRUE
  )
  matches1 <- regmatches(raw_expr, m1)[[1]]
  ids1 <- if (length(matches1) > 0)
    gsub(paste0("#\\{(", uid11, ")\\.(", uid11, ")\\}"),
         "\\1_\\2", matches1, perl = TRUE)
  else character(0)
  # #{de} -- bare UID (aggregated data, no COC)
  m2 <- gregexpr(paste0("#\\{(", uid11, ")\\}"), raw_expr, perl = TRUE)
  matches2 <- regmatches(raw_expr, m2)[[1]]
  bare <- matches2[!grepl("\\.", matches2, fixed = TRUE)]
  ids2 <- if (length(bare) > 0)
    gsub(paste0("#\\{(", uid11, ")\\}"), "\\1", bare, perl = TRUE)
  else character(0)
  unique(c(ids1, ids2))
}

#' Convert a raw DHIS2 rule expression to an evaluatable R string
#'
#' Replaces \code{#\{de.coc\}} with \code{val_de_coc} and bare \code{#\{de\}}
#' with \code{val_de} (for aggregated data where \code{data.id} has no COC part).
#' @noRd
.vr_expr_to_r <- function(raw_expr) {
  if (is.null(raw_expr) || is.na(raw_expr)) return(as.character(raw_expr))
  uid11  <- "[A-Za-z][A-Za-z0-9]{10}"
  # #{de.coc} first (more specific) -> val_de_coc
  result <- gsub(
    paste0("#\\{(", uid11, ")\\.(", uid11, ")\\}"),
    "val_\\1_\\2", raw_expr, perl = TRUE
  )
  # Then bare #{de} -> val_de
  result <- gsub(
    paste0("#\\{(", uid11, ")\\}"),
    "val_\\1", result, perl = TRUE
  )
  result
}

#' Apply a DHIS2 validation-rule operator to two numeric vectors
#' @noRd
.apply_vr_operator <- function(ls, rs, op) {
  result <- switch(toupper(op),
    "EQUAL_TO"                  = ls == rs,
    "NOT_EQUAL"                 = ls != rs,
    "LESS_THAN"                 = ls < rs,
    "GREATER_THAN"              = ls > rs,
    "LESS_THAN_OR_EQUAL_TO"     = ls <= rs,
    "GREATER_THAN_OR_EQUAL_TO"  = ls >= rs,
    "COMPULSORY_PAIR"           = (is.na(ls) == is.na(rs)),
    rep(NA, length(ls))
  )
  result[is.na(ls) | is.na(rs)] <- NA
  result
}

#' Cast long-format data to wide for rule evaluation
#' @noRd
.vr_wide_data <- function(data) {
  period_col <- if ("Month" %in% names(data)) "Month" else "Week"
  dt <- data.table::as.data.table(data)
  dt[, year_col := data.table::year(as.IDate(unclass(get(period_col)), origin = "1970-01-01"))]

  # Only leaf-level rows; need orgUnit, period, year, data.id, original
  dt_leaf <- dt[effectiveLeaf == TRUE,
    .(orgUnit, orgUnitName, period = get(period_col), year = year_col,
      data.id, original)]

  # Pre-aggregate to one row per orgUnit+period+data.id before pivoting.
  # This avoids passing a custom R fun.aggregate to dcast (which is called
  # once per group in R -- very slow on large datasets).
  # Rule: if any value is non-NA, sum them; if all NA, keep NA.
  # Pre-aggregate: keep only non-NA rows, sum within group.
  # Groups where all values were NA are simply absent; dcast fills them with NA_real_.
  # This avoids an R function call per group (43s -> <1s on large datasets).
  dt_agg <- dt_leaf[!is.na(original),
    .(original = sum(original)),
    by = .(orgUnit, orgUnitName, period, year, data.id)
  ]

  # Simple dcast -- one value per cell, no aggregation function needed
  wide <- data.table::dcast(
    dt_agg,
    orgUnit + orgUnitName + period + year ~ data.id,
    value.var = "original",
    fill = NA_real_
  )

  # Rename data.id columns to val_{data_id} for safe eval()
  id_cols <- setdiff(names(wide), c("orgUnit", "orgUnitName", "period", "year"))
  data.table::setnames(wide, id_cols, paste0("val_", id_cols))

  # For bare DE UID references (rules written against pre-aggregated totals):
  # add val_{de_uid} columns by summing all val_{de_uid}_* disaggregations.
  # This lets rules like #{de} evaluate correctly when data is COC-disaggregated.
  de_uids <- unique(sub("_.*$", "", id_cols))  # extract UID prefix from each data.id
  for (de in de_uids) {
    bare_col  <- paste0("val_", de)
    if (bare_col %in% names(wide)) next  # already exists (no-COC element)
    agg_cols  <- grep(paste0("^val_", de, "_"), names(wide), value = TRUE)
    if (length(agg_cols) == 0) next
    wide[, (bare_col) := {
      mat <- as.matrix(.SD)
      ifelse(rowSums(!is.na(mat)) == 0L, NA_real_, rowSums(mat, na.rm = TRUE))
    }, .SDcols = agg_cols]
  }

  wide
}

#' Evaluate All Validation Rules Against Data
#'
#' For each rule x facility x period, determines whether the rule passed,
#' failed, or could not be evaluated (missing data elements).
#'
#' @param data Processed dataset (output of `data_1()`).
#' @param validation_rules Tibble from `metadata_widget_output$validationRules()`.
#' @param filter_data_ids Character vector of `data.id` values (format
#'   `"deUID_cocUID"`) for the currently selected elements.  When supplied,
#'   only rules whose expressions reference at least one of those data element
#'   UIDs are evaluated; all other rules are silently ignored.
#'
#' @return A tibble with columns `rule_id`, `rule_name`, `year`, `n_evaluated`,
#'   `n_passed`, `n_failed`, `incomplete` (TRUE if required elements not in data).
#' @export
dqa_consistency <- function(data, validation_rules, filter_data_ids = NULL) {
  if (is.null(data) || nrow(data) == 0)          return(NULL)
  if (is.null(validation_rules) || nrow(validation_rules) == 0) return(NULL)
  if (!"data.id" %in% names(data))               return(NULL)

  # Require translated expression columns -- absent when validation rules come
  # directly from the raw API (not processed by fetch_validation_rules())
  if (!all(c("leftSide_expression_raw", "rightSide_expression_raw") %in%
           names(validation_rules)))              return(NULL)

  # Filter to rules that reference at least one selected element (by DE UID)
  if (!is.null(filter_data_ids) && length(filter_data_ids) > 0) {
    filter_de_uids <- unique(sub("_.*$", "", filter_data_ids))
    uid_pattern    <- paste(filter_de_uids, collapse = "|")
    validation_rules <- validation_rules[
      grepl(uid_pattern, validation_rules$leftSide_expression_raw,  fixed = FALSE) |
      grepl(uid_pattern, validation_rules$rightSide_expression_raw, fixed = FALSE),
    ]
    if (nrow(validation_rules) == 0) return(NULL)
  }

  # Filter data to only the data.id values referenced by the remaining rules
  # before the wide pivot -- avoids pivoting thousands of irrelevant columns.
  rule_ids <- unique(c(
    unlist(lapply(validation_rules$leftSide_expression_raw,  .extract_vr_uid_pairs)),
    unlist(lapply(validation_rules$rightSide_expression_raw, .extract_vr_uid_pairs))
  ))
  if (length(rule_ids) > 0 && "data.id" %in% names(data)) {
    rule_de_uids <- unique(sub("_.*$", "", rule_ids))
    # Pre-compute outside [.data.table to avoid its NSE parser choking on
    # a nested data[[...]] call inside the i expression.
    .keep <- sub("_.*$", "", as.character(data[["data.id"]])) %in% rule_de_uids
    data  <- data[.keep]
  }

  wide <- .vr_wide_data(data)
  wl   <- as.list(wide)  # evaluate expressions against this list

  results <- purrr::map_df(seq_len(nrow(validation_rules)), function(i) {
    rule  <- validation_rules[i, ]
    ls_r  <- .vr_expr_to_r(rule$leftSide_expression_raw[1])
    rs_r  <- .vr_expr_to_r(rule$rightSide_expression_raw[1])
    op    <- rule$operator[1]

    required_ids  <- c(
      .extract_vr_uid_pairs(rule$leftSide_expression_raw[1]),
      .extract_vr_uid_pairs(rule$rightSide_expression_raw[1])
    )
    needed_cols   <- paste0("val_", required_ids)
    missing_cols  <- setdiff(needed_cols, names(wide))
    incomplete    <- length(missing_cols) > 0

    pass <- if (incomplete) {
      rep(NA, nrow(wide))
    } else {
      tryCatch({
        ls_vals <- eval(parse(text = ls_r), envir = wl)
        rs_vals <- eval(parse(text = rs_r), envir = wl)
        .apply_vr_operator(ls_vals, rs_vals, op)
      }, error = function(e) rep(NA, nrow(wide)))
    }

    tibble::tibble(
      rule_id     = rule$id,
      rule_name   = rule$name,
      year        = wide$year,
      pass        = pass,
      incomplete  = incomplete
    )
  }, .id = NULL)

  results %>%
    dplyr::group_by(rule_id, rule_name, year, incomplete) %>%
    dplyr::summarise(
      n_evaluated = sum(!is.na(pass)),
      n_passed    = sum(pass == TRUE,  na.rm = TRUE),
      n_failed    = sum(pass == FALSE, na.rm = TRUE),
      .groups     = "drop"
    )
}


#' Annual Consistency Chart
#'
#' @param consistency_data Output of [dqa_consistency()].
#' @param text_size Numeric. Base text size (default: `18`).
#'
#' @return A ggplot object.
#' @export
dqa_consistency_plot <- function(consistency_data, text_size = 18) {
  if (is.null(consistency_data) || nrow(consistency_data) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, size = 5,
          label = "No validation rules could be evaluated.\nCheck that metadata has been fetched.") +
        ggplot2::scale_x_continuous(breaks = NULL, name = NULL) +
        ggplot2::scale_y_continuous(breaks = NULL, name = NULL) +
        ggplot2::theme_void()
    )
  }

  annual <- consistency_data %>%
    dplyr::filter(!incomplete) %>%
    dplyr::group_by(Year = year) %>%
    dplyr::summarise(
      n_evaluated = sum(n_evaluated),
      n_passed    = sum(n_passed),
      pct_passed  = dplyr::if_else(
        n_evaluated > 0, n_passed / n_evaluated, NA_real_
      ),
      label = scales::percent(pct_passed, 0.1),
      .groups = "drop"
    )

  if (nrow(annual) == 0 || all(is.na(annual$pct_passed))) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, size = 5,
          label = "No evaluatable rule-checks found.") +
        ggplot2::scale_x_continuous(breaks = NULL, name = NULL) +
        ggplot2::scale_y_continuous(breaks = NULL, name = NULL) +
        ggplot2::theme_void()
    )
  }

  ggplot2::ggplot(annual,
    ggplot2::aes(x = as.character(Year), y = pct_passed, label = label, group = 1)
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
      title    = "Validation Rule Consistency",
      subtitle = "Percent of facility-period evaluations where rules passed"
    ) +
    ggplot2::theme_minimal(base_size = text_size)
}


#' Per-Rule Summary Table
#'
#' @param consistency_data Output of [dqa_consistency()].
#'
#' @return A tibble with one row per rule.
#' @export
dqa_consistency_table <- function(consistency_data) {
  if (is.null(consistency_data) || nrow(consistency_data) == 0)
    return(tibble::tibble())

  consistency_data %>%
    dplyr::group_by(`Rule ID` = rule_id, `Rule Name` = rule_name) %>%
    dplyr::summarise(
      Status              = dplyr::if_else(any(incomplete), "Incomplete (missing elements)", "Evaluated"),
      `Facility-Periods`  = sum(n_evaluated),
      Passed              = sum(n_passed),
      Failed              = sum(n_failed),
      `% Passed`          = scales::percent(
        dplyr::if_else(sum(n_evaluated) > 0, sum(n_passed) / sum(n_evaluated), NA_real_),
        0.1
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(`% Passed`)
}


#' Drilldown: Failed Facility-Periods for One Rule
#'
#' Re-evaluates a single rule and returns rows where the rule failed or
#' could not be evaluated.
#'
#' @param data Processed dataset (output of `data_1()`).
#' @param validation_rules Tibble from metadata.
#' @param rule_id Character. ID of the rule to drilldown.
#' @param max_rows Integer. Maximum rows returned (default `2000`).
#'
#' @return A tibble of failed facility-periods.
#' @export
dqa_consistency_detail_rule <- function(data, validation_rules, rule_id,
                                        max_rows = 2000L) {
  if (is.null(data) || is.null(validation_rules)) return(tibble::tibble())
  rule <- dplyr::filter(validation_rules, id == rule_id)
  if (nrow(rule) == 0) return(tibble::tibble())

  # Extract the DE UIDs the rule references and filter data to just those
  # elements before pivoting -- avoids a full-dataset wide pivot
  required_ids <- c(
    .extract_vr_uid_pairs(rule$leftSide_expression_raw[1]),
    .extract_vr_uid_pairs(rule$rightSide_expression_raw[1])
  )
  required_de_uids <- unique(sub("_.*$", "", required_ids))
  dt_all <- data.table::as.data.table(data)
  data_filtered <- dt_all[sub("_.*$", "", data.id) %in% required_de_uids]

  wide <- .vr_wide_data(data_filtered)
  wl   <- as.list(wide)

  ls_r <- .vr_expr_to_r(rule$leftSide_expression_raw[1])
  rs_r <- .vr_expr_to_r(rule$rightSide_expression_raw[1])
  op   <- rule$operator[1]

  needed_cols  <- paste0("val_", required_ids)
  missing_cols <- setdiff(needed_cols, names(wide))

  if (length(missing_cols) > 0) {
    missing_ids <- sub("^val_", "", missing_cols)
    return(tibble::tibble(
      Note = paste0(
        "Rule cannot be evaluated: missing data element(s): ",
        paste(missing_ids, collapse = ", ")
      )
    ))
  }

  ls_vals <- tryCatch(
    eval(parse(text = ls_r), envir = wl),
    error = function(e) rep(NA_real_, nrow(wide))
  )
  rs_vals <- tryCatch(
    eval(parse(text = rs_r), envir = wl),
    error = function(e) rep(NA_real_, nrow(wide))
  )
  pass <- .apply_vr_operator(ls_vals, rs_vals, op)

  wide[, `:=`(ls_val = ls_vals, rs_val = rs_vals, pass = pass)]

  result <- wide[pass == FALSE | is.na(pass), ] %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      `Org Unit`   = orgUnitName,
      Period       = as.character(period),
      Year         = year,
      `Left Side`  = ls_val,
      `Right Side` = rs_val,
      Result       = dplyr::if_else(is.na(pass), "Incomplete", "Failed")
    ) %>%
    dplyr::arrange(dplyr::desc(Year), Period, `Org Unit`)

  if (!is.null(max_rows) && nrow(result) > max_rows) {
    attr(result, "truncated") <- nrow(result)
    result <- result[seq_len(max_rows), ]
  }
  result
}

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

# Extract calendar year from a Month value regardless of encoding.
# Handles both months-since-epoch (~360-840 for 2000-2040, correct yearmonth)
# and days-since-epoch (~10k-25k, legacy encoding from older MG2 builds).
.month_to_year <- function(x) {
  raw <- c(unclass(x))
  if (length(raw) == 0) return(integer(0))
  if (median(raw, na.rm = TRUE) > 5000) {
    as.integer(format(as.Date(as.integer(raw), origin = "1970-01-01"), "%Y"))
  } else {
    as.integer(raw) %/% 12L + 1970L
  }
}

#' Extract Years Present in DQA Data
#' @noRd
dqa_years <- function(dqa_data) {
  tibble::tibble(Year = sort(unique(.month_to_year(dqa_data$Month))))
}

#' Count Consistently Reporting Facilities per Year
#'
#' @param dqa_data tsibble of prepared data.
#' @param missing_reports Integer. Allowed missing months (default: `0`).
#' @param count.any Logical. Count facilities reporting any data element
#'   (default: `TRUE`).
#' @param .cat Logical. Print progress messages (default: `FALSE`).
#' @param .progress Optional function `(i, n)` called after processing each
#'   year, for Shiny progress feedback.
#' @param ... additional arguments passed to `mostFrequentReportingOUs()`
#'
#' @return Numeric vector with count of consistently reporting facilities per year.
#' @export
dqa_reporting <- function(dqa_data, missing_reports = 0, count.any = TRUE,
                          .cat = FALSE, .progress = NULL, ...) {
  startingMonth <- tsibble::yearmonth(
    paste0(dqa_years(dqa_data)$Year, "Jan")
  )

  endingMonth <- dqa_data |>
    tibble::as_tibble() |>
    dplyr::ungroup() |>
    dplyr::group_by(year = .month_to_year(Month)) |>
    dplyr::summarize(latest_month = max(Month), .groups = "drop") |>
    dplyr::pull(latest_month)

  endingMonth[length(endingMonth)] <- endingMonth[length(endingMonth)] - 1

  if (.cat) cat("\n *dqa_reporting - mostFrequentReportingOUs")

  n_years      <- length(startingMonth)
  reportingOUS <- vector("list", n_years)
  for (i in seq_len(n_years)) {
    reportingOUS[[i]] <- mostFrequentReportingOUs(
      data            = dqa_data,
      startingMonth   = startingMonth[i],
      endingMonth     = endingMonth[i],
      missing_reports = missing_reports,
      count.any       = count.any,
      .cat            = FALSE
    )
    if (is.function(.progress)) .progress(i, n_years)
  }

  purrr::map_dbl(reportingOUS, length)
}


#' Compute Percent of Facilities Consistently Reporting per Year
#'
#' @param dqa_data tsibble of prepared data.
#' @param .cat Logical. Print progress messages (default: `FALSE`).
#' @param .progress Optional function `(i, n)` passed through to
#'   [dqa_reporting()] for Shiny progress feedback.
#'
#' @return A tibble with columns `Year`, `n_frequently_reporting`,
#'   `n_facilities`, `pr`, `label`.
#' @export
dqaPercentReporting <- function(dqa_data, .cat = FALSE, .progress = NULL) {
  if (.cat) cat('\n*  dqa_functions.R dqaPercentReporting')

  year <- dqa_years(dqa_data)
  if (.cat) cat('\n -  years:', paste(year$Year, collapse = ","))

  n_frequently_reporting <- dqa_reporting(dqa_data, missing_reports = 0,
                                          .progress = .progress)
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
      title    = "Percent of facilities consistently reporting selected data elements each year",
      subtitle = paste(
        'Out of the number of facilities that have ever reported (',
        scales::comma(n_facilities), ")"
      )
    ) +
    ggplot2::theme_minimal(base_size = text_size)
}


#' Compute Reporting Completeness per Year per Region
#'
#' Calls [mostFrequentReportingOUs()] for each calendar year and groups
#' results by an org-unit hierarchy column (typically level 2 / province).
#'
#' @param dqa_data Processed dataset from `data_1()`.
#' @param level_col Character. Name of the column that identifies the region
#'   (e.g. `"Province (DPS)"`).  Must be a column in `dqa_data`.
#' @param missing_reports Integer. Allowed missing months (default `0`).
#' @param .progress Optional function `(i, n)` called after each year.
#'
#' @return A tibble with columns `Year`, `region_name`, `n_reporting`,
#'   `n_total`, `pr` (proportion 0-1).
#' @export
dqa_reporting_by_region <- function(dqa_data, level_col,
                                    missing_reports = 0L,
                                    .progress = NULL) {
  if (!level_col %in% names(dqa_data)) return(NULL)

  # region lookup: orgUnit -> region name
  region_map <- dqa_data |>
    tibble::as_tibble() |>
    dplyr::distinct(orgUnit, region_name = .data[[level_col]])

  # total distinct facilities per region (denominator is all-time, matching
  # dqaPercentReporting which uses length(unique(dqa_data$orgUnit)))
  n_by_region <- region_map |>
    dplyr::filter(!is.na(region_name)) |>
    dplyr::group_by(region_name) |>
    dplyr::summarise(n_total = dplyr::n(), .groups = "drop")

  # Use IDENTICAL year windows to dqa_reporting() so chart and map are consistent:
  # startingMonth = yearmonth("YYYYJan"), endingMonth = max(Month) per year,
  # with the last year's end reduced by 1 month (partial year).
  years_df    <- dqa_years(dqa_data)
  startMonths <- tsibble::yearmonth(paste0(years_df$Year, "Jan"))
  endMonths   <- dqa_data |>
    tibble::as_tibble() |>
    dplyr::ungroup() |>
    dplyr::group_by(year = .month_to_year(Month)) |>
    dplyr::summarize(latest_month = max(Month), .groups = "drop") |>
    dplyr::pull(latest_month)
  endMonths[length(endMonths)] <- endMonths[length(endMonths)] - 1L
  n <- length(startMonths)

  purrr::map_df(seq_len(n), function(i) {
    yr <- years_df$Year[i]
    reporting_ous <- mostFrequentReportingOUs(
      data            = dqa_data,
      startingMonth   = startMonths[i],
      endingMonth     = endMonths[i],
      missing_reports = missing_reports
    )
    if (is.function(.progress)) .progress(i, n)

    region_map |>
      dplyr::filter(!is.na(region_name)) |>
      dplyr::mutate(reporting = orgUnit %in% reporting_ous) |>
      dplyr::group_by(region_name) |>
      dplyr::summarise(n_reporting = sum(reporting), .groups = "drop") |>
      dplyr::left_join(n_by_region, by = "region_name") |>
      dplyr::mutate(Year = yr, pr = n_reporting / n_total)
  })
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
  if (is.null(data) || nrow(data) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, size = 6,
          label = "No outlier data available.\nRun outlier detection in the Outliers tab first.") +
        ggplot2::scale_x_continuous(breaks = NULL, name = NULL) +
        ggplot2::scale_y_continuous(breaks = NULL, name = NULL) +
        ggplot2::theme_void()
    )
  }

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


# SWAPE -----------------------------------------------------------------------

#' Compute SWAPE for a Single Year
#' @noRd
swape_year <- function(dqa_data, .year) {
  dt <- data.table::setDT(tibble::as_tibble(dqa_data))[
    .month_to_year(Month) <= .year & !is.na(original) & !is.na(expected)
  ]

  n_facilities <- dt[, data.table::uniqueN(orgUnit)]

  # SWAPE = 200 * sum(|actual - expected|) / (sum(actual) + sum(expected))
  # Same formula as model_metrics(); result is 0-200 scale expressed as fraction
  denom <- dt[, sum(original, na.rm = TRUE) + sum(expected, na.rm = TRUE)]
  swape_val <- if (denom > 0) {
    dt[, 2 * sum(abs(original - expected), na.rm = TRUE) / denom]
  } else NA_real_
  if (!is.finite(swape_val)) swape_val <- NA_real_

  tibble::tibble(
    Year       = .year,
    Facilities = n_facilities,
    Mean_MASE  = swape_val,          # keep column name for plot compatibility
    label      = scales::percent(swape_val, 0.1)
  )
}


#' Compute SWAPE Across All Years in DQA Data
#'
#' Requires an `expected` column (from seasonal cleaning). Returns `NULL`
#' with a message when the column is absent.
#'
#' @param dqa_data tsibble of prepared data (must have `expected` column).
#'
#' @return A tibble with one row per year, or `NULL` if `expected` is absent.
#' @export
dqa_swape <- function(dqa_data) {
  if (!"expected" %in% names(dqa_data)) {
    message("dqa_swape: 'expected' column not found - SWAPE plot requires seasonal cleaning first.")
    return(NULL)
  }
  years <- dqa_years(dqa_data)$Year
  result <- purrr::map_df(years, ~ swape_year(dqa_data, .x))
  result[1:2, 3:ncol(result)] <- NA
  return(result)
}

#' @rdname dqa_swape
#' @export
dqa_mase <- dqa_swape   # backwards-compatible alias


#' Plot SWAPE Summary (Minimum Detectable Change)
#'
#' @param data Output of [dqa_swape()].
#'
#' @return A ggplot object.
#' @export
dqa_swape_plot <- function(data) {
  if (is.null(data)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "SWAPE requires seasonal cleaning (expected column not available)",
                          size = 5) +
        ggplot2::theme_void()
    )
  }
  swape_txt <- paste(
    "Symmetric Weighted Absolute Percentage Error (SWAPE) across ALL facilities --",
    "200 \u00d7 sum(|actual \u2212 expected|) / (sum(actual) + sum(expected))\n",
    "- The smaller this value, the more predictable the data trend\n",
    "- Year-to-year changes smaller than this value may reflect data variability rather than a real change\n",
    "- Note: selecting champion facilities in the Evaluation tab may show lower error than seen here"
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
      subtitle = swape_txt,
      caption  = "NOTE: SWAPE calculated beginning with 3rd year of data"
    ) +
    ggplot2::theme_minimal(base_size = 18)
}

#' @rdname dqa_swape_plot
#' @export
dqa_mase_plot <- dqa_swape_plot   # backwards-compatible alias
