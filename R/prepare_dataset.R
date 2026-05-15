# Functions for preparing downloaded DHIS2 data for time-series analysis.
# Migrated from R/originals/prepareDataset.R and R/originals/TS_Modeling_Functions.R

# Internal helpers ------------------------------------------------------------

is_null_or_empty <- function(x) {
  is.null(x) || length(x) == 0 || (is.character(x) && nchar(x) == 0)
}


# Dataset translation ---------------------------------------------------------

#' Match Downloaded Data to Formula Elements (simple match)
#'
#' @noRd
translate_dataset <- function(data, formula_elements, .verbose = FALSE) {
  if (.verbose) message("translate_dataset:")

  if (any(is.na(data$categoryOptionCombo))) {
    element_match <- match(
      paste(data$dataElement, data$categoryOptionCombo),
      paste(
        formula_elements$dataElement.id,
        ifelse(
          is.na(formula_elements$n_categoryOptions),
          NA,
          formula_elements$categoryOptionCombo.ids
        )
      )
    )
  } else {
    element_match <- match(
      paste(data$dataElement, data$categoryOptionCombo),
      paste(formula_elements$dataElement.id, formula_elements$categoryOptionCombo.ids)
    )
  }

  formula_data <- formula_elements[element_match, ] |>
    dplyr::select(-dataElement.id, -categoryOptionCombo.ids)

  data |>
    dplyr::rename(
      dataElement.id          = dataElement,
      categoryOptionCombo.ids = categoryOptionCombo
    ) |>
    dplyr::bind_cols(formula_data)
}


#' Match Downloaded Data to Formula Elements (separate DE and category lists)
#'
#' @noRd
translate_dataset_2 <- function(data, dataElements, categories, .verbose = FALSE) {
  if (.verbose) message("translate_dataset_2:")

  if (.verbose) message("- expanding categories table")
  categories_expanded <- categories |>
    dplyr::mutate(
      cat_list = stringr::str_split(Categories, " ;\\n "),
      opt_list = stringr::str_split(categoryOptionCombo.ids, " ;\\n ")
    ) |>
    dplyr::mutate(
      paired = purrr::map2(
        cat_list, opt_list,
        ~ tibble::tibble(
          Category               = trimws(.x),
          categoryOptionCombo.id = trimws(.y)
        )
      )
    ) |>
    dplyr::select(categoryCombo.id, categoryCombo, paired) |>
    tidyr::unnest(paired)

  if (.verbose) message("- matching dataElement and categoryOptionCombo")
  dataElement_match  <- match(data$dataElement, dataElements$dataElement.id)
  categories_match   <- match(data$categoryOptionCombo,
                              categories_expanded$categoryOptionCombo.id)

  data |>
    dplyr::rename(
      dataElement.id          = dataElement,
      categoryOptionCombo.ids = categoryOptionCombo
    ) |>
    dplyr::bind_cols(
      dataElements[dataElement_match, "dataElement"],
      categories_expanded[categories_match, "Category"]
    )
}


# Leaf determination ----------------------------------------------------------

#' Determine Effective Reporting Leaf Status per Org Unit × Data Element
#'
#' @details
#' **Deprecated for modern downloads.** The DHIS2 download API (post ~2022) returns
#' only org units where data was directly entered, so `COUNT` is always 1 and
#' `effectiveLeaf` is always `TRUE`. `data_1()` skips this function and sets
#' `effectiveLeaf = TRUE` directly when `max(COUNT) <= 1`. This function is
#' retained only for backward compatibility with legacy "All levels" datasets
#' where aggregate rows with `COUNT > 1` were included.
#'
#' @noRd
data_leaves <- function(d, .verbose = FALSE) {
  if (.verbose) message("determining effective leaf")

  d |>
    tibble::as_tibble() |>
    dplyr::group_by(orgUnit, dataElement.id, leaf) |>
    dplyr::summarise(
      n = max(as.integer(COUNT), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(effectiveLeaf = n == 1 | leaf == TRUE) |>
    dplyr::select(orgUnit, dataElement.id, leaf, effectiveLeaf)
}


# Time-series preparation -----------------------------------------------------

#' Prepare a Data Frame for Time-Series Analysis
#'
#' Converts raw downloaded data into a tidy format with `Month` or `Week`
#' columns and unified `data` / `data.id` labels.
#'
#' @param df Data frame from [data_1()].
#' @param period `"Month"` or `"Week"`.
#' @param missing.value Value to fill for missing periods (default: `NA`).
#' @param .verbose Logical. Print progress messages (default: `FALSE`).
#'
#' @return A tibble with time and identifier columns added.
#' @export
df_pre_ts <- function(df, period = "Month", missing.value = NA, .verbose = FALSE) {
  if (.verbose) message("df_pre_ts")

  .period <- if (period %in% c("Week", "Weekly")) "Week" else "Month"

  # Remove rows with no count or a malformed period value.
  # Use df$period (explicit) rather than dplyr NSE — the function argument is
  # also named 'period', which shadows the column name in tidy evaluation and
  # causes the filter to silently misbehave.
  # DHIS2 monthly periods are exactly 6 digits (YYYYMM); weekly are YYYYWww.
  # Quarterly (2026Q1), financial-year (2026Oct), empty, and NA strings are all
  # rejected here to prevent yearmonth() from erroring downstream.
  .valid_period <- if (.period == "Month")
    !is.na(df$period) & grepl("^[0-9]{6}$", df$period)
  else
    !is.na(df$period) & grepl("^[0-9]{4}W[0-9]+$", df$period)
  df <- df[!is.na(df$COUNT) & .valid_period, , drop = FALSE]

  if (.verbose) message("- unite data and data.id")

  if (.period == "Month") {
    ym <- tsibble::yearmonth(
      paste0(substr(df$period, 1, 4), "-", substr(df$period, 5, 6))
    )

    dt <- data.table::as.data.table(df)
    dt[, `:=`(
      Month = ym,
      COUNT = as.integer(COUNT),
      SUM   = as.numeric(SUM)
    )]
    dt[, data := fifelse(
      is.na(dataElement) & is.na(Category), NA_character_,
      paste0(
        fifelse(is.na(dataElement), "", dataElement),
        fifelse(is.na(Category),    "", paste0("_", Category))
      )
    )]
    dt[, data.id := fifelse(
      is.na(dataElement) & is.na(Category), NA_character_,
      paste0(
        fifelse(is.na(dataElement.id),          "", dataElement.id),
        fifelse(is.na(categoryOptionCombo.ids), "",
                paste0("_", categoryOptionCombo.ids))
      )
    )]
    return(dt)

  } else {
    dt <- data.table::as.data.table(df)
    dt[, `:=`(
      Week       = tsibble::yearweek(period),
      COUNT      = as.integer(COUNT),
      SUM        = as.numeric(SUM),
      Categories = fifelse(is.na(Categories), "", Categories)
    )]
    result <- tibble::as_tibble(dt) |>
      tidyr::unite("data",    dataElement, Categories, remove = FALSE) |>
      tidyr::unite("data.id", dataElement.id, categoryOptionCombo.ids, remove = FALSE)
    return(result)
  }
}


#' Convert Prepared Data to a tsibble Time-Series Object
#'
#' @param df.pre.ts Output of [df_pre_ts()].
#' @param period `"Month"` or `"Week"`.
#' @param fill.gaps Logical. Fill implicit gaps in the time series (default: `FALSE`).
#' @param missing.value Value to fill for gaps (default: `NA`).
#' @param .verbose Logical. Print progress messages (default: `FALSE`).
#'
#' @return A `tsibble` keyed by `orgUnit` × `data.id`.
#' @export
df_ts <- function(df.pre.ts, period = "Month", fill.gaps = FALSE,
                  missing.value = NA, .verbose = FALSE) {
  if (.verbose) message("df_ts: period = ", period)

  if (.verbose) message("- dedup")
  dt <- data.table::as.data.table(df.pre.ts)
  dt <- unique(dt, by = c("orgUnit", "data.id", period))

  if (.verbose) message("- as_tsibble")
  ts <- tibble::as_tibble(dt) |>
    tsibble::as_tsibble(
      key      = c(orgUnit, data.id),
      index    = !!rlang::sym(period),
      validate = FALSE
    )

  if (fill.gaps) {
    if (.verbose) message("- fill gaps")
    ts <- ts |>
      tsibble::fill_gaps(value = missing.value, .full = TRUE) |>
      tidyr::fill(c(effectiveLeaf, orgUnit, data.id), .direction = "down")
  }

  if (.verbose) message("- done")
  return(ts)
}


# Main preparation wrapper ----------------------------------------------------

#' Prepare Downloaded DHIS2 Data for Analysis
#'
#' Translates raw downloaded data (output of [api_data()]) into a tidy
#' time-series tibble with org unit hierarchy, data element labels, and
#' outlier-detection flags pre-computed.
#'
#' @param data Data frame returned by [api_data()].
#' @param dataSets Data sets metadata from the metadata widget.
#' @param formula_elements Formula elements table.
#' @param dataElements Data elements metadata.
#' @param categories Categories metadata.
#' @param ousTree Org unit hierarchy table from [ous_tree()].
#' @param .scan_outliers Logical. Run MAD and seasonal outlier scans per element
#'   inside `data_1()` (default: `TRUE`). Set to `FALSE` to skip outlier
#'   detection and produce a "raw processed" tsibble (useful for fast previews
#'   or when outliers will be re-run later via the Outliers tab).
#' @param .progress Optional callback `function(i, n, element_name, phase)`
#'   called at the start and after each scan phase for each element. Useful for
#'   driving a Shiny `withProgress` / `setProgress` UI. `i` is the 1-based
#'   element index, `n` is the total number of elements, `element_name` is the
#'   human-readable element label (first value of the `dataElement` column in
#'   the chunk), and `phase` is a short string describing the current stage
#'   (`"preparing"`, `"MAD scan — X series"`, `"seasonal — X series"`,
#'   `"combining"`).
#' @param .shiny_progress Logical. When `TRUE` and `.scan_outliers = TRUE`,
#'   passes `shiny_progress = TRUE` to [seasonal_outliers()] so that per-series
#'   completion counts appear in the `withProgress` detail line while the
#'   seasonal scan is running (default: `FALSE`).
#' @param .verbose Logical. Print progress messages (default: `FALSE`).
#'
#' @return A `tsibble` ready for DQA and analysis widgets.
#' @export
data_1 <- function(data,
                   dataSets        = NULL,
                   formula_elements = NULL,
                   dataElements    = NULL,
                   categories      = NULL,
                   ousTree         = NULL,
                   .scan_outliers  = TRUE,
                   .progress       = NULL,
                   .shiny_progress = FALSE,
                   .verbose        = FALSE) {
  if (.verbose) message("data_1: preparing dataset")

  if (!'COUNT' %in% names(data)) {
    if (.verbose) message("- no COUNT column, returning NULL")
    return(NULL)
  }

  # Determine period type
  ptype <- if (!"periodType" %in% names(formula_elements)) {
    "Monthly"
  } else {
    min(formula_elements$periodType, na.rm = TRUE)
  }
  if (is_null_or_empty(ptype) || is.na(ptype)) ptype <- "Monthly"
  if (.verbose) message("- ptype is ", ptype)

  p <- if (grepl("weekly", ptype, ignore.case = TRUE)) "Week" else "Month"

  # Ensure categoryOptionCombo column exists
  if (!'categoryOptionCombo' %in% names(data)) {
    if (.verbose) message("- adding missing categoryOptionCombo column")
    data <- dplyr::mutate(data, categoryOptionCombo = NA_character_)
  }

  # Build dataset → data element lookup
  dataSetElements <- dataSets |>
    tidyr::unnest(dataSetElements.id, names_sep = "_") |>
    dplyr::select(dataSet.id, dataSet, periodType, dataSetElements.id_dataElement) |>
    dplyr::rename(dataElement.id = dataSetElements.id_dataElement) |>
    dplyr::mutate(dataElement.id = as.character(dataElement.id$id)) |>
    dplyr::group_by(dataElement.id) |>
    dplyr::summarise(
      n_datasets  = dplyr::n(),
      dataSet.ids = paste(dataSet.id, collapse = " ;\n"),
      dataSet     = paste(dataSet, collapse = " ;\n"),
      .groups     = "drop"
    ) |>
    dplyr::arrange(-n_datasets)

  if (is.function(.progress)) .progress(0L, 0L, "", "Filtering records...")
  message("data_1: filter NA SUM (", nrow(data), " rows)")
  d. <- tibble::as_tibble(data)
  d. <- dplyr::filter(d., !is.na(SUM))
  message("data_1: ", nrow(d.), " rows after NA filter")

  if (is.function(.progress)) .progress(0L, 0L, "", "Matching data elements to formula...")
  if (.verbose) message("- translate_dataset_2 + join hierarchy")
  message("data_1: translate_dataset_2")
  d. <- translate_dataset_2(d., dataElements, categories, .verbose = .verbose)

  # Use data.table merges for the two large joins — much faster than dplyr on
  # 10M+ rows because data.table uses a radix sort-merge rather than hash join.
  if (is.function(.progress)) .progress(0L, 0L, "", "Joining dataset metadata...")
  message("data_1: join dataSetElements")
  dt.  <- data.table::as.data.table(d.)
  dse. <- data.table::as.data.table(dataSetElements)
  ous. <- data.table::as.data.table(ousTree)
  data.table::setkeyv(dt.,  "dataElement.id")
  data.table::setkeyv(dse., "dataElement.id")
  dt. <- dse.[dt., on = "dataElement.id"]

  if (is.function(.progress)) .progress(0L, 0L, "", sprintf("Joining org unit hierarchy (%d units)...", nrow(ous.)))
  message("data_1: join ousTree (", nrow(ous.), " org units)")
  data.table::setkeyv(dt.,  "orgUnit")
  data.table::setkeyv(ous., "orgUnit")
  dt. <- ous.[dt., on = "orgUnit"]

  # Skip the expensive leaf-detection join when every row has COUNT == 1.
  # The new DHIS2 download API returns only org units where data was entered,
  # so COUNT is always 1 for modern downloads.  Old "All levels" downloads
  # included aggregate rows with COUNT > 1; those still need data_leaves().
  if (isTRUE(max(as.integer(dt.$COUNT), na.rm = TRUE) <= 1L)) {
    if (.verbose) message("- effectiveLeaf: all COUNT == 1, skipping data_leaves()")
    message("data_1: effectiveLeaf = TRUE (COUNT always 1)")
    dt.$effectiveLeaf <- TRUE
  } else {
    message("data_1: effectiveLeaf via data_leaves()")
    d_tmp <- tibble::as_tibble(dt.)
    data.leaves <- data_leaves(d_tmp, .verbose = .verbose)
    dt. <- data.table::as.data.table(
      dplyr::left_join(
        d_tmp,
        dplyr::select(data.leaves, orgUnit, dataElement.id, effectiveLeaf),
        by = c("orgUnit", "dataElement.id")
      )
    )
  }

  # --------------------------------------------------------------------------
  # Per-element loop: df_pre_ts → df_ts → mutate(original, value) → outliers
  # --------------------------------------------------------------------------
  element_ids <- unique(dt.$dataElement.id)
  n_elements  <- length(element_ids)
  idx_var     <- p   # "Month" or "Week"
  key_vars    <- c("orgUnit", "data.id")

  message("data_1: processing ", n_elements, " element(s) per-element")
  if (is.function(.progress)) .progress(0L, n_elements, "", sprintf("Starting per-element scan (%d elements)...", n_elements))

  results <- vector("list", n_elements)

  for (i in seq_along(element_ids)) {
    eid   <- element_ids[[i]]
    chunk <- dt.[dataElement.id == eid]

    # Human-readable element name (first non-NA value of dataElement column)
    element_name <- if ("dataElement" %in% names(chunk)) {
      el <- chunk$dataElement[!is.na(chunk$dataElement)]
      if (length(el) > 0) el[[1L]] else eid
    } else {
      eid
    }

    message(sprintf("data_1: element %d/%d — %s", i, n_elements, element_name))
    .t0 <- proc.time()[["elapsed"]]
    if (is.function(.progress)) .progress(i, n_elements, element_name, "parsing periods...")

    # df_pre_ts expects a tibble
    chunk_tbl <- tibble::as_tibble(chunk)
    chunk_ts  <- df_pre_ts(chunk_tbl, period = p, .verbose = FALSE)

    if (is.function(.progress)) .progress(i, n_elements, element_name, "building time series...")
    chunk_ts  <- df_ts(chunk_ts, period = p, .verbose = FALSE)

    if (is.function(.progress)) .progress(i, n_elements, element_name, "adding value flags...")
    chunk_ts  <- dplyr::mutate(chunk_ts, original = SUM, value = !is.na(SUM))

    n_series <- tsibble::n_keys(chunk_ts)

    if (.scan_outliers) {
      message(sprintf("data_1:   MAD scan — %d series", n_series))
      if (is.function(.progress)) {
        .progress(i, n_elements, element_name,
                  sprintf("MAD scan \u2014 %d series", n_series))
      }
      .mad_fn <- if (.shiny_progress && is.function(.progress)) {
        local({
          .i <- i; .n <- n_elements; .nm <- element_name
          function(phase_str) .progress(.i, .n, .nm, phase_str)
        })
      } else NULL
      chunk_ts <- mad_outliers(chunk_ts, .total = n_series, progress = FALSE,
                               .progress_fn = .mad_fn)

      message(sprintf("data_1:   seasonal scan — %d series", n_series))
      if (is.function(.progress)) {
        .progress(i, n_elements, element_name,
                  sprintf("seasonal \u2014 %d series", n_series))
      }
      # Build a callback that threads per-series % back through .progress
      .seas_fn <- if (.shiny_progress && is.function(.progress)) {
        local({
          .i <- i; .n <- n_elements; .nm <- element_name
          function(phase_str) .progress(.i, .n, .nm, phase_str)
        })
      } else NULL
      chunk_ts <- seasonal_outliers(chunk_ts, .total = n_series,
                                    .progress_fn = .seas_fn)
    }

    results[[i]] <- tibble::as_tibble(chunk_ts)
    message(sprintf("data_1:   done in %.0f s", proc.time()[["elapsed"]] - .t0))
  }

  # --------------------------------------------------------------------------
  # Combine all element chunks
  # --------------------------------------------------------------------------
  message("data_1: combining ", n_elements, " element chunk(s)")
  if (is.function(.progress)) {
    .progress(n_elements, n_elements, "", "combining")
  }

  combined_dt <- data.table::rbindlist(results, fill = TRUE)
  # Restore the yearmonth/yearweek S3 class that rbindlist may strip from the
  # index column (data.table concatenates raw doubles without preserving class).
  if (idx_var %in% names(combined_dt)) {
    idx_cls <- if (idx_var == "Month") "yearmonth" else "yearweek"
    if (!inherits(combined_dt[[idx_var]], idx_cls)) {
      data.table::set(combined_dt, j = idx_var,
                      value = structure(as.numeric(combined_dt[[idx_var]]),
                                        class = idx_cls))
    }
  }
  data.table::setkeyv(combined_dt, c("orgUnit", "data.id", idx_var))

  d.. <- tsibble::as_tsibble(
    tibble::as_tibble(combined_dt),
    key      = dplyr::all_of(key_vars),
    index    = !!rlang::sym(idx_var),
    validate = FALSE
  )

  message("data_1: done — ", nrow(d..), " rows, ", tsibble::n_keys(d..), " series")

  # Drop columns that were only needed during processing.
  # SUM/COUNT: used to build original/value and effectiveLeaf; not needed downstream.
  # dataElement.id, categoryOptionCombo.ids, Category: encoded in data/data.id.
  # categoryCombo, categoryCombo.id, n_datasets, dataSet.ids: metadata not used downstream.
  # leaf: redundant with effectiveLeaf.
  # period: raw DHIS2 string replaced by Month/Week.
  drop_cols <- intersect(
    names(d..),
    c("SUM", "COUNT", "dataElement.id", "categoryOptionCombo.ids",
      "Category", "categoryCombo", "categoryCombo.id",
      "n_datasets", "dataSet.ids", "leaf", "period")
  )
  if (length(drop_cols) > 0) {
    if (.verbose) message("- dropping columns: ", paste(drop_cols, collapse = ", "))
    d.. <- dplyr::select(d.., -dplyr::all_of(drop_cols))
  }

  return(d..)
}
