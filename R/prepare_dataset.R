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
translate_dataset <- function(data, formula_elements) {
  cat('\n* translate_dataset:')

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
translate_dataset_2 <- function(data, dataElements, categories) {
  cat('\n* translate_dataset_2:')

  cat("\n- expanding categories table")
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

  cat("\n- matching dataElement and categoryOptionCombo")
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
#' @noRd
data_leaves <- function(d) {
  cat('\n* determining effective leaf')

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
#'
#' @return A tibble with time and identifier columns added.
#' @export
df_pre_ts <- function(df, period = "Month", missing.value = NA) {
  cat("\n * df_pre_ts")

  .period <- if (period %in% c("Week", "Weekly")) "Week" else "Month"

  # Remove rows with no count
  df <- dplyr::filter(df, !is.na(COUNT))

  cat("\n - unite data and data.id")

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
#'
#' @return A `tsibble` keyed by `orgUnit` × `data.id`.
#' @export
df_ts <- function(df.pre.ts, period = "Month", fill.gaps = FALSE,
                  missing.value = NA) {
  cat('\n * df_ts')
  cat('\n - .period is', period)

  cat('\n - dedup')
  dt <- data.table::as.data.table(df.pre.ts)
  dt <- unique(dt, by = c("orgUnit", "data.id", period))

  cat('\n - as_tsibble')
  ts <- tibble::as_tibble(dt) |>
    tsibble::as_tsibble(
      key      = c(orgUnit, data.id),
      index    = !!rlang::sym(period),
      validate = FALSE
    )

  if (fill.gaps) {
    cat('\n - fill gaps')
    ts <- ts |>
      tsibble::fill_gaps(value = missing.value, .full = TRUE) |>
      tidyr::fill(c(effectiveLeaf, orgUnit, data.id), .direction = "down")
  }

  cat('\n - done')
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
#' @param dataSets Data sets metadata from [metadata_widget_server()].
#' @param formula_elements Formula elements table.
#' @param dataElements Data elements metadata.
#' @param categories Categories metadata.
#' @param ousTree Org unit hierarchy table from [ous_tree()].
#'
#' @return A `tsibble` ready for DQA and analysis widgets.
#' @export
data_1 <- function(data,
                   dataSets        = NULL,
                   formula_elements = NULL,
                   dataElements    = NULL,
                   categories      = NULL,
                   ousTree         = NULL) {
  cat('\n* data_1: preparing dataset')

  if (!'COUNT' %in% names(data)) {
    cat('\n - no COUNT column, returning NULL')
    return(NULL)
  }

  # Determine period type
  ptype <- if (!"periodType" %in% names(formula_elements)) {
    "Monthly"
  } else {
    min(formula_elements$periodType, na.rm = TRUE)
  }
  if (is_null_or_empty(ptype)) ptype <- "Monthly"
  cat('\n - ptype is', ptype)

  p <- if (grepl("weekly", ptype, ignore.case = TRUE)) "Week" else "Month"

  # Ensure categoryOptionCombo column exists
  if (!'categoryOptionCombo' %in% names(data)) {
    cat("\n - adding missing categoryOptionCombo column")
    data <- dplyr::mutate(data, categoryOptionCombo = NA_character_)
  }

  # Build dataset → data element lookup
  dataSetElements <- dataSets |>
    tidyr::unnest(dataSetElements.id, names_sep = "_") |>
    dplyr::select(1:3, 5) |>
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

  cat('\n - translate_dataset_2 + join hierarchy')
  d. <- data |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(SUM)) |>
    translate_dataset_2(dataElements, categories) |>
    dplyr::left_join(dataSetElements, by = "dataElement.id") |>
    dplyr::left_join(ousTree,         by = "orgUnit")

  data.leaves <- data_leaves(d.)

  cat('\n - df_pre_ts + df_ts')
  d.. <- d. |>
    dplyr::left_join(
      dplyr::select(data.leaves, orgUnit, dataElement.id, effectiveLeaf),
      by = c("orgUnit", "dataElement.id")
    ) |>
    df_pre_ts(period = p) |>
    df_ts(period = p) |>
    dplyr::mutate(original = SUM, value = !is.na(SUM))

  return(d..)
}
