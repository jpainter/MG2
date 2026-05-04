# Helpers that summarise MG2 session state into compact text for AI system prompts.
# Each function returns a character string or NULL (when data are unavailable).
# NULL sections are silently omitted from the prompt.

.fmt_period_values <- function(period_vec, value_vec, n_per_row = 6L) {
  ord   <- order(as.character(period_vec))
  pairs <- paste0(as.character(period_vec[ord]), ": ",
                  formatC(value_vec[ord], format = "fg", big.mark = ","))
  # wrap into rows of n_per_row so the prompt stays readable
  rows  <- split(pairs, ceiling(seq_along(pairs) / n_per_row))
  vapply(rows, paste, character(1), collapse = "  |  ")
}


#' Reporting completeness summary for AI prompt
#'
#' Computes the percentage of facilities with a non-missing value in each
#' period, per data element. Uses raw `data1` — no Reporting tab visit needed.
#'
#' @param data1 tsibble/tibble from `data_widget_output$data1()`
#' @param max_periods most-recent periods to include
#' @return character string or NULL
#' @export
summarize_completeness_for_prompt <- function(data1, max_periods = 24L) {
  if (is.null(data1) || nrow(data1) == 0) return(NULL)

  period_col <- intersect(c("Month", "Week"), names(data1))[1]
  if (is.na(period_col) || !"data" %in% names(data1)) return(NULL)

  val_col <- intersect(c("original", "total", "SUM"), names(data1))[1]
  if (is.na(val_col)) return(NULL)

  tryCatch({
    d <- as.data.frame(data1)
    d$..period  <- as.character(d[[period_col]])
    d$..element <- as.character(d[["data"]])
    d$..val     <- d[[val_col]]

    periods <- sort(unique(d$..period), decreasing = TRUE)
    if (length(periods) > max_periods) periods <- periods[seq_len(max_periods)]
    d <- d[d$..period %in% periods, ]

    # n_present / n_total per element × period
    n_total   <- stats::aggregate(rep(1L,    nrow(d)), list(d$..period, d$..element), sum)
    n_present <- stats::aggregate(!is.na(d$..val),     list(d$..period, d$..element), sum)
    names(n_total)   <- c("period", "element", "n")
    names(n_present) <- c("period", "element", "present")

    m <- merge(n_total, n_present, by = c("period", "element"))
    m$pct <- round(100 * m$present / pmax(m$n, 1L))

    elements <- sort(unique(m$element))
    section  <- c(
      "## Within-Dataset Completeness (% of periods with a non-missing value, per facility in the downloaded data)",
      "IMPORTANT CAVEAT: The denominator here is only facilities that appear in the",
      "downloaded dataset (i.e. submitted at least one value ever). Facilities that",
      "never submitted any data are excluded entirely, so these percentages will",
      "overestimate true reporting completeness. The Reporting tab shows the",
      "authoritative completeness estimate against all expected facilities."
    )

    for (el in elements) {
      rows <- m[m$element == el, ]
      section <- c(section, paste0("### ", el),
                   .fmt_period_values(rows$period, rows$pct))
    }

    paste(section, collapse = "\n")
  }, error = function(e) NULL)
}


#' Reporting-adjusted national totals for AI prompt
#'
#' Summarises `data.total()` from the Reporting widget — champion-facility
#' adjusted aggregates (unadjusted for incomplete periods).
#' Returns NULL if the Reporting tab has not been visited yet.
#'
#' @param data_total tibble from `reporting_widget_output$data.total()`
#' @param period_col "Month" or "Week"
#' @param max_periods most-recent periods to include
#' @return character string or NULL
#' @export
summarize_reporting_for_prompt <- function(data_total, period_col = "Month",
                                            max_periods = 36L) {
  if (is.null(data_total) || nrow(data_total) == 0) return(NULL)
  if (!period_col %in% names(data_total))
    period_col <- intersect(c("Month", "Week"), names(data_total))[1]
  if (is.na(period_col) || !"data" %in% names(data_total)) return(NULL)
  if (!"total" %in% names(data_total)) return(NULL)

  tryCatch({
    d <- as.data.frame(data_total)
    d$..period  <- as.character(d[[period_col]])
    d$..element <- as.character(d[["data"]])

    periods <- sort(unique(d$..period), decreasing = TRUE)
    if (length(periods) > max_periods) periods <- periods[seq_len(max_periods)]
    d <- d[d$..period %in% periods, ]

    agg <- stats::aggregate(d$total, list(d$..period, d$..element),
                            function(x) sum(x, na.rm = TRUE))
    names(agg) <- c("period", "element", "total")

    elements <- sort(unique(agg$element))
    section  <- c(
      "## Reporting-Adjusted Totals (champion facilities only, NOT adjusted for incomplete reporting)",
      "These are the totals shown in the Reporting tab charts."
    )

    for (el in elements) {
      rows <- agg[agg$element == el, ]
      section <- c(section, paste0("### ", el),
                   .fmt_period_values(rows$period, rows$total))
    }

    paste(section, collapse = "\n")
  }, error = function(e) NULL)
}


#' Outlier detection summary for AI prompt
#'
#' Summarises flagged values from `cleaning_widget_output$data2()` —
#' counts by element and algorithm, plus most-affected periods.
#' Returns NULL if the Outliers tab has not been visited yet.
#'
#' @param data2 tibble from `cleaning_widget_output$data2()`
#' @return character string or NULL
#' @export
summarize_outliers_for_prompt <- function(data2) {
  if (is.null(data2) || nrow(data2) == 0) return(NULL)
  if (!"data" %in% names(data2)) return(NULL)

  flag_cols <- intersect(
    c("key_entry_error", "over_max", "mad15", "mad10", "seasonal5", "seasonal3"),
    names(data2)
  )
  if (length(flag_cols) == 0) return(NULL)

  period_col <- intersect(c("Month", "Week"), names(data2))[1]

  tryCatch({
    d <- as.data.frame(data2)

    # combined flag: any algorithm
    d$..any <- rowSums(d[flag_cols], na.rm = TRUE) > 0

    elements <- sort(unique(as.character(d$data)))
    section  <- "## Outlier Detection Summary"

    for (el in elements) {
      rows      <- d[as.character(d$data) == el, ]
      n_total   <- nrow(rows)
      n_flagged <- sum(rows$..any, na.rm = TRUE)
      pct       <- round(100 * n_flagged / max(n_total, 1L), 1)

      algo_counts <- vapply(flag_cols, function(col)
        sum(rows[[col]], na.rm = TRUE), integer(1))
      algo_counts <- algo_counts[algo_counts > 0]

      section <- c(section,
        paste0("### ", el),
        paste0("Total values: ", formatC(n_total, format = "fg", big.mark = ","),
               "  |  Flagged: ", n_flagged, " (", pct, "%)"))

      if (length(algo_counts) > 0)
        section <- c(section,
          paste0("  By algorithm: ",
                 paste(names(algo_counts), algo_counts, sep = "=", collapse = "  ")))

      # most-affected periods (top 5)
      if (!is.na(period_col) && n_flagged > 0) {
        flagged_rows  <- rows[rows$..any & !is.na(rows$..any), ]
        period_counts <- sort(table(as.character(flagged_rows[[period_col]])),
                              decreasing = TRUE)
        top_n <- min(5L, length(period_counts))
        top   <- period_counts[seq_len(top_n)]
        section <- c(section,
          paste0("  Most flagged periods: ",
                 paste(names(top), top, sep = " (", collapse = " flags), "),
                 " flags)"))
      }
    }

    paste(section, collapse = "\n")
  }, error = function(e) NULL)
}


#' Build MG2 system prompt for the AI data assistant
#'
#' Constructs a context-rich system prompt from the current session state.
#' All parameters are optional; omitted sections degrade gracefully.
#'
#' @param formula_elements tibble from `data_widget_output$formula_elements()`
#' @param num_facilities integer from `reporting_widget_output$num_facilities()`
#' @param starting_month yearmonth/character start of data period
#' @param ending_month yearmonth/character end of data period
#' @param validation_rules tibble from `metadata_widget_output$validationRules()`
#' @param completeness_summary output of [summarize_completeness_for_prompt()]
#' @param raw_totals_summary output of [summarize_data_for_prompt()]
#' @param reporting_summary output of [summarize_reporting_for_prompt()]
#' @param outlier_summary output of [summarize_outliers_for_prompt()]
#' @return single character string suitable for an ellmer system prompt
#' @export
build_mg2_system_prompt <- function(
    formula_elements    = NULL,
    num_facilities      = NULL,
    starting_month      = NULL,
    ending_month        = NULL,
    validation_rules    = NULL,
    completeness_summary = NULL,
    raw_totals_summary  = NULL,
    reporting_summary   = NULL,
    outlier_summary     = NULL
) {

  lines <- c(
    "You are a data analysis assistant embedded in MagicGlasses2 (MG2), an",
    "epidemiological analysis tool for routine health data from DHIS2 health",
    "information systems. You help analysts understand data quality, trends,",
    "and anomalies in facility-reported health data.",
    "",
    "## App tabs (use these exact names when directing the user)",
    "Welcome | Setup | Metadata | Regions | Data (sub-tabs: Formula, Download, Combine)",
    "DQA | Reporting | Outliers | Evaluation | Assistant | About",
    "There is NO tab called 'Trends', 'Analysis', or 'Charts'.",
    "",
    "## Your role",
    "- Interpret data quality findings, trends, and anomalies",
    "- Suggest related data elements that may help contextualise findings",
    "- Explain relevant epidemiological concepts for the current data elements",
    "- Flag when comparisons are unreliable due to data quality issues",
    "- Keep answers concise and actionable for a public health analyst",
    ""
  )

  # --- Dataset overview ------------------------------------------------------
  dataset_lines <- "## Current Dataset"

  if (!is.null(formula_elements) && nrow(formula_elements) > 0) {
    fe <- formula_elements
    if (is.null(fe$role)) fe$role <- "primary"

    primary   <- fe[fe$role %in% "primary",   , drop = FALSE]
    secondary <- fe[fe$role %in% "secondary", , drop = FALSE]

    dataset_lines <- c(dataset_lines,
      paste0("Formula elements (", nrow(fe), " total):"))

    fmt_element <- function(row) {
      nm  <- if (!is.null(row$name) && !is.na(row$name)) row$name else row$dataElement
      cat <- if (!is.null(row$categories) && !is.na(row$categories) && nzchar(row$categories))
        paste0(" [", row$categories, "]") else ""
      paste0("  - ", nm, cat)
    }

    if (nrow(primary) > 0) {
      dataset_lines <- c(dataset_lines, "Primary elements:")
      dataset_lines <- c(dataset_lines,
        vapply(seq_len(nrow(primary)), function(i) fmt_element(primary[i, ]), character(1)))
    }
    if (nrow(secondary) > 0) {
      dataset_lines <- c(dataset_lines, "Secondary (supporting) elements:")
      dataset_lines <- c(dataset_lines,
        vapply(seq_len(nrow(secondary)), function(i) fmt_element(secondary[i, ]), character(1)))
    }
  } else {
    dataset_lines <- c(dataset_lines, "No formula elements loaded yet.")
  }

  if (!is.null(num_facilities) && !is.na(num_facilities))
    dataset_lines <- c(dataset_lines, paste0("Reporting facilities: ", num_facilities))

  if (!is.null(starting_month) && !is.null(ending_month))
    dataset_lines <- c(dataset_lines,
      paste0("Data period: ", as.character(starting_month),
             " to ", as.character(ending_month)))

  lines <- c(lines, dataset_lines, "")

  # --- Data sections (include whichever are available) ----------------------
  for (section in list(completeness_summary, reporting_summary,
                        raw_totals_summary, outlier_summary)) {
    if (!is.null(section)) lines <- c(lines, section, "")
  }

  # --- Validation rules (summarised) ----------------------------------------
  if (!is.null(validation_rules) && nrow(validation_rules) > 0) {
    vr_lines <- c(
      "## Validation Rules (defined in DHIS2)",
      paste0(nrow(validation_rules), " validation rules in this DHIS2 instance.")
    )
    show_n <- min(12L, nrow(validation_rules))
    for (i in seq_len(show_n)) {
      nm <- if (!is.null(validation_rules$name) && !is.na(validation_rules$name[i]))
        validation_rules$name[i] else paste("Rule", i)
      vr_lines <- c(vr_lines, paste0("  - ", nm))
    }
    if (nrow(validation_rules) > show_n)
      vr_lines <- c(vr_lines,
        paste0("  ... and ", nrow(validation_rules) - show_n, " more."))
    lines <- c(lines, vr_lines, "")
  }

  # --- Caveats ---------------------------------------------------------------
  has_data <- !is.null(reporting_summary) || !is.null(raw_totals_summary)

  lines <- c(lines,
    "## Important caveats for DHIS2 routine data",
    "- Reporting completeness is often incomplete and varies by facility and period.",
    "- The within-dataset completeness figures above overestimate true completeness — always defer to the Reporting tab for the authoritative completeness estimate.",
    "- Outliers may be data entry errors OR genuine epidemiological signals.",
    "- Trends may reflect changes in reporting practice, not true disease burden.",
    "- Comparisons across periods are only valid for consistently reporting facilities.",
    "- Population denominators (for rates) may be inaccurate at facility level.",
    "- Seasonality is common and should not be confused with real trends.",
    if (has_data)
      "- Reporting-adjusted totals use only champion (consistently reporting) facilities."
    else
      "- Visit the Reporting and Outliers tabs first, then click 'New Conversation' for richer context.",
    "",
    "When you lack enough context, ask a clarifying question rather than guessing."
  )

  paste(lines, collapse = "\n")
}
