# Data Quality Assessment functions for MG2.
# Migrated from R/originals/dqa_functions.R


# Validation Rule Consistency -------------------------------------------------

#' Extract data.id values (de_uid_coc_uid) referenced in a raw rule expression
#' @noRd
.extract_vr_uid_pairs <- function(raw_expr) {
  if (is.null(raw_expr) || is.na(raw_expr) || !nzchar(raw_expr))
    return(character(0))
  m <- gregexpr(
    "#\\{([A-Za-z][A-Za-z0-9]{10})\\.([A-Za-z][A-Za-z0-9]{10})\\}",
    raw_expr, perl = TRUE
  )
  matches <- regmatches(raw_expr, m)[[1]]
  if (length(matches) == 0) return(character(0))
  unique(gsub(
    "#\\{([A-Za-z][A-Za-z0-9]{10})\\.([A-Za-z][A-Za-z0-9]{10})\\}",
    "\\1_\\2", matches, perl = TRUE
  ))
}

#' Convert a raw DHIS2 rule expression to an evaluatable R string
#' Replaces \code{#\{de.coc\}} with \code{val_de_coc}
#' @noRd
.vr_expr_to_r <- function(raw_expr) {
  if (is.null(raw_expr) || is.na(raw_expr)) return(as.character(raw_expr))
  gsub(
    "#\\{([A-Za-z][A-Za-z0-9]{10})\\.([A-Za-z][A-Za-z0-9]{10})\\}",
    "val_\\1_\\2", raw_expr, perl = TRUE
  )
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
  dt[, year_col := lubridate::year(get(period_col))]

  # Only leaf-level rows; need orgUnit, period, year, data.id, original
  dt_leaf <- dt[effectiveLeaf == TRUE,
    .(orgUnit, orgUnitName, period = get(period_col), year = year_col,
      data.id, original)]

  # Cast wide: one column per data.id
  wide <- data.table::dcast(
    dt_leaf,
    orgUnit + orgUnitName + period + year ~ data.id,
    value.var = "original",
    fun.aggregate = function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE),
    fill = NA_real_
  )

  # Rename data.id columns to val_{data_id} for safe eval()
  id_cols <- setdiff(names(wide), c("orgUnit", "orgUnitName", "period", "year"))
  data.table::setnames(wide, id_cols, paste0("val_", id_cols))
  wide
}

#' Evaluate All Validation Rules Against Data
#'
#' For each rule × facility × period, determines whether the rule passed,
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
#'
#' @return A tibble of failed facility-periods.
#' @export
dqa_consistency_detail_rule <- function(data, validation_rules, rule_id) {
  if (is.null(data) || is.null(validation_rules)) return(tibble::tibble())
  rule <- dplyr::filter(validation_rules, id == rule_id)
  if (nrow(rule) == 0) return(tibble::tibble())

  wide <- .vr_wide_data(data)
  wl   <- as.list(wide)

  ls_r <- .vr_expr_to_r(rule$leftSide_expression_raw[1])
  rs_r <- .vr_expr_to_r(rule$rightSide_expression_raw[1])
  op   <- rule$operator[1]

  required_ids <- c(
    .extract_vr_uid_pairs(rule$leftSide_expression_raw[1]),
    .extract_vr_uid_pairs(rule$rightSide_expression_raw[1])
  )
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

  wide[pass == FALSE | is.na(pass), ] %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      `Org Unit`   = orgUnitName,
      Period       = as.character(period),
      Year         = year,
      `Left Side`  = ls_val,
      `Right Side` = rs_val,
      Result       = dplyr::if_else(is.na(pass), "Incomplete", "Failed")
    ) %>%
    dplyr::arrange(Year, Period, `Org Unit`)
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
#' @param ... additional arguments passed to `mostFrequentReportingOUs()`
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
    message("dqa_mase: 'expected' column not found - MASE plot requires seasonal cleaning first.")
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
