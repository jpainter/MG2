# fct_output_dhis2.R
# Backend functions for writing MG2 outputs to a local DHIS2 instance.
#
# Workflow:
#   1. Spin up a local d2 cluster matching the source DHIS2 version
#   2. Import org units (from saved metadata .rds) + data elements
#      (from the currently loaded formula — dynamic, never hardcoded) +
#      4 fixed MG2 output data elements
#   3. Write raw data values and/or model outputs via dataValueSets API
#
# DHIS2 fixed defaults (same uid across all blank instances):
.OUT_DEFAULT_CATCOMBO <- "bjDvmb4bfuf"   # "default" category combination
.OUT_DEFAULT_COC      <- "HllvX50cXC0"   # "default" category option combo

# ---- Type inference from observed values ------------------------------------

#' Infer DHIS2 valueType from a numeric vector
#'
#' Returns `"INTEGER_ZERO_OR_POSITIVE"` when all finite values are non-negative
#' whole numbers, otherwise `"NUMBER"`.  Deliberately avoids `"PERCENTAGE"`
#' — storing percentages as NUMBER is safe and avoids false positives.
#'
#' @param x Numeric vector of observed data values.
#' @return Single character string.
#' @keywords internal
mg2_infer_valuetype <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return("NUMBER")
  if (min(x) >= 0 && all(x == floor(x))) "INTEGER_ZERO_OR_POSITIVE" else "NUMBER"
}

#' Infer DHIS2 aggregationType from observed values
#'
#' Integer data -> `"SUM"` (routine counts: cases, tests, deaths).
#' Data with decimal values -> `"AVERAGE"`.
#' The formula `role` argument is reserved for future refinement (e.g. a
#' denominator that is itself a rate).
#'
#' @param x Numeric vector of observed values.
#' @param role Character; one of `"numerator"`, `"denominator"`, `"unknown"`.
#' @return `"SUM"` or `"AVERAGE"`.
#' @keywords internal
mg2_infer_aggregationtype <- function(x, role = "unknown") {
  x <- x[is.finite(x)]
  if (!length(x)) return("SUM")
  if (any(x != floor(x))) "AVERAGE" else "SUM"
}

# ---- Period conversion ------------------------------------------------------

# Convert a yearmonth / yearweek / Date / numeric vector to DHIS2 "YYYYMM"
# (or "YYYYWnn" for weekly data).
.out_period_to_dhis2 <- function(x) {
  if (inherits(x, "yearmonth")) {
    format(as.Date(x), "%Y%m")
  } else if (inherits(x, "yearweek")) {
    # tsibble yearweek -> ISO week "YYYYWnn"
    d <- as.Date(x)
    paste0(format(d, "%Y"), "W", formatC(as.integer(format(d, "%V")), width = 2, flag = "0"))
  } else if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    format(as.Date(x), "%Y%m")
  } else if (is.numeric(x)) {
    # MG2 stores yearmonth as days-since-epoch after unclass()
    format(as.Date(as.integer(x), origin = "1970-01-01"), "%Y%m")
  } else {
    as.character(x)
  }
}

# ---- Org unit payload -------------------------------------------------------

#' Build DHIS2 organisationUnits list for metadata import
#'
#' Reshapes the org-unit data frame stored in the MG2 metadata `.rds` into
#' the JSON structure expected by `POST /api/metadata`.  Sorts by level so
#' parents are always imported before children.
#'
#' @param org_units Data frame from `metadata_widget_output$orgUnits()`.
#'   Required columns: `id`, `name`, `level`; optional: `shortName`,
#'   `openingDate`, `parent.id`.
#' @return Named list suitable for `jsonlite::toJSON()` as
#'   `payload$organisationUnits`.
#' @noRd
mg2_build_org_units_payload <- function(org_units) {
  if (is.null(org_units) || !nrow(org_units))
    stop("org_units is empty \u2014 load metadata first.")

  org_units <- org_units[order(org_units$level), ]

  lapply(seq_len(nrow(org_units)), function(i) {
    r      <- org_units[i, , drop = FALSE]
    short  <- trimws(.nullish(r$shortName, ""))
    if (!nzchar(short) || short == "NA") short <- substr(trimws(r$name), 1L, 50L)
    open   <- trimws(.nullish(r$openingDate, ""))
    if (!nzchar(open) || open == "NA") open <- "1970-01-01"

    ou <- list(
      id          = r$id,
      name        = trimws(r$name),
      shortName   = short,
      openingDate = open,
      level       = r$level
    )
    pid <- trimws(.nullish(r$parent.id, ""))
    if (nzchar(pid) && pid != "NA") ou$parent <- list(id = pid)
    ou
  })
}

# ---- Source data element payload (dynamic) ----------------------------------

#' Build DHIS2 dataElements list from the currently loaded MG2 dataset
#'
#' Data elements are derived entirely from whatever formula is loaded —
#' nothing is hardcoded.  Types are inferred from the observed values.
#'
#' @param data_df Data frame from `data_widget_output$data1()`.  Expected
#'   columns: `data.id` (UID + optional COC suffix), `dataElement` (human
#'   name), `original`.  `dataElement.id` is dropped by `data_1()` —
#'   the UID is recovered by stripping the category suffix from `data.id`.
#' @return Named list of data element objects for `POST /api/metadata`.
#' @noRd
mg2_build_source_de_payload <- function(data_df) {
  if (is.null(data_df) || !nrow(data_df))
    stop("No data loaded \u2014 select a formula and load data first.")

  required <- c("data.id", "dataElement", "original")
  missing  <- setdiff(required, names(data_df))
  if (length(missing))
    stop("data_df missing columns: ", paste(missing, collapse = ", "))

  # Work on a small projection — avoids copying all 60M+ rows of the full dataset.
  keep_cols <- intersect(c("data.id", "dataElement", "original", "role"), names(data_df))
  dt <- data.table::as.data.table(data_df)[, ..keep_cols]

  # Extract UID from data.id (format "UID" or "UID_catOptionComboID")
  dt[, dataElement.id := sub("_.*$", "", data.id)]

  # Per-UID summary: first name + all original values (needed for type inference)
  de_ids <- unique(dt$dataElement.id)
  de_ids <- de_ids[!is.na(de_ids) & nzchar(de_ids)]

  lapply(de_ids, function(uid) {
    rows  <- dt[dataElement.id == uid]
    name  <- rows$dataElement[!is.na(rows$dataElement)][1L]
    vals  <- rows$original

    # Role from formula if present, else "unknown"
    role  <- if ("role" %in% names(rows)) {
      r <- rows$role[!is.na(rows$role)]
      if (length(r)) r[1L] else "unknown"
    } else "unknown"

    list(
      id              = uid,
      name            = name,
      shortName       = substr(name, 1L, 50L),
      domainType      = "AGGREGATE",
      valueType       = mg2_infer_valuetype(vals),
      aggregationType = mg2_infer_aggregationtype(vals, role),
      categoryCombo   = list(id = .OUT_DEFAULT_CATCOMBO)
    )
  })
}

# ---- MG2 output data elements (fixed) --------------------------------------

# These four data elements are always created in the local instance to hold
# MG2 model outputs.  UIDs are fixed 11-character strings that will not
# collide with source instance UIDs (which are random alphanumeric).
.OUT_ELEMENT_UIDS <- c(
  trend    = "MG2trendEst",
  adjusted = "MG2adjEstim",
  forecast = "MG2forecast",
  wpe      = "MG2wpeEffct"
)

#' Build the four fixed MG2 model-output data elements
#'
#' @param prefix Label prefix prepended to each element name (default `"MG2"`).
#' @return List of four data element objects for `POST /api/metadata`.
#' @noRd
mg2_build_output_de_payload <- function(prefix = "MG2") {
  defs <- list(
    list(key = "trend",    label = "Trend estimate"),
    list(key = "adjusted", label = "Reporting-adjusted estimate"),
    list(key = "forecast", label = "Forecast"),
    list(key = "wpe",      label = "WPE intervention effect")
  )
  lapply(defs, function(d) {
    nm <- paste(prefix, d$label)
    list(
      id              = .OUT_ELEMENT_UIDS[[d$key]],
      name            = nm,
      shortName       = substr(nm, 1L, 50L),
      domainType      = "AGGREGATE",
      valueType       = "NUMBER",
      aggregationType = "AVERAGE",
      categoryCombo   = list(id = .OUT_DEFAULT_CATCOMBO)
    )
  })
}

# ---- Metadata POST ----------------------------------------------------------

#' POST org units and data elements to a local DHIS2 instance
#'
#' @param base_url Base URL of the target instance, e.g. `"http://localhost:8090"`.
#' @param username,password DHIS2 credentials (default `admin` / `district`).
#' @param org_units List from `mg2_build_org_units_payload()`.
#' @param data_elements Combined list from source + output payload builders.
#' @return List: `ok` (logical), `status` (HTTP code), `message` (summary).
#' @noRd
mg2_post_metadata <- function(base_url,
                               username      = "admin",
                               password      = "district",
                               org_units     = NULL,
                               data_elements = NULL) {
  payload <- list()
  if (length(org_units))     payload$organisationUnits <- org_units
  if (length(data_elements)) payload$dataElements      <- data_elements
  if (!length(payload)) return(list(ok = FALSE, message = "Nothing to import."))

  url  <- paste0(
    gsub("/$", "", base_url),
    "/api/metadata?importMode=COMMIT&strategy=CREATE_AND_UPDATE&atomicMode=NONE"
  )
  body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")

  resp <- tryCatch(
    httr::POST(
      url,
      httr::authenticate(username, password),
      httr::content_type_json(),
      body   = body,
      encode = "raw",
      httr::timeout(120L)
    ),
    error = function(e) list(error = conditionMessage(e))
  )
  if (!is.null(resp$error))
    return(list(ok = FALSE, status = NA_integer_, message = resp$error))

  code <- httr::status_code(resp)
  body_out <- tryCatch(
    httr::content(resp, as = "parsed", type = "application/json"),
    error = function(e) NULL
  )
  # DHIS2 2.36+ wraps stats under response.stats; older versions at top-level stats
  st <- body_out$response$stats %||% body_out$stats
  msg <- if (!is.null(st)) {
    paste0("Created: ", .nullish(st$created, 0),
           " | Updated: ", .nullish(st$updated, 0),
           " | Ignored: ", .nullish(st$ignored, 0))
  } else paste("HTTP", code)

  list(ok = code %in% c(200L, 201L), status = code, message = msg, detail = body_out)
}

# ---- Data value formatting + POST ------------------------------------------

#' Reshape MG2 raw data to DHIS2 dataValueSets format
#'
#' Aggregates across category option combos (sums integer data, averages
#' numeric data) before writing, since the local instance uses the default
#' (no-disaggregation) category combo for all data elements.
#'
#' @param data_df Data frame from `data_widget_output$data1()`.
#' @param period_range Optional length-2 character vector of DHIS2 period
#'   strings `c("202101", "202312")` to filter rows.
#' @return Data frame with columns `dataElement`, `period`, `orgUnit`,
#'   `categoryOptionCombo`, `value`.
#' @noRd
mg2_format_raw_data_values <- function(data_df, period_range = NULL) {
  if (is.null(data_df) || !nrow(data_df)) return(data.frame())

  period_col <- if ("Month" %in% names(data_df)) "Month"
                else if ("Week" %in% names(data_df)) "Week"
                else stop("Cannot find period column (Month or Week) in data.")

  # data_1() drops dataElement.id from the final dataset; the UID is recovered
  # by stripping the category-option-combo suffix from data.id.
  if (!"data.id" %in% names(data_df)) {
    stop(sprintf(
      "Column 'data.id' not found in data. Available columns: %s",
      paste(names(data_df), collapse = ", ")
    ))
  }

  de_uid <- sub("_.*$", "", data_df$data.id)

  dt <- data.table::data.table(
    dataElement = de_uid,
    period      = .out_period_to_dhis2(data_df[[period_col]]),
    orgUnit     = data_df$orgUnit,
    value_raw   = as.numeric(data_df$original)
  )
  dt <- dt[!is.na(dataElement) & nzchar(dataElement) &
            !is.na(period) & !is.na(orgUnit) & !is.na(value_raw)]

  if (!nrow(dt)) return(data.frame())

  # Optional period filter
  if (!is.null(period_range) && length(period_range) == 2L) {
    dt <- dt[period >= period_range[1L] & period <= period_range[2L]]
  }

  # Aggregate across COCs per (dataElement, period, orgUnit) using data.table
  agg <- dt[, .(value_raw = sum(value_raw, na.rm = TRUE)),
              by = .(dataElement, period, orgUnit)]

  data.frame(
    dataElement         = agg$dataElement,
    period              = agg$period,
    orgUnit             = agg$orgUnit,
    categoryOptionCombo = .OUT_DEFAULT_COC,
    value               = as.character(round(agg$value_raw, 4L)),
    stringsAsFactors    = FALSE
  )
}

#' POST data values to a DHIS2 instance
#'
#' @param base_url Target instance base URL.
#' @param username,password DHIS2 credentials.
#' @param data_values Data frame from `mg2_format_raw_data_values()` or
#'   equivalent.
#' @return List: `ok`, `status`, `message`, `n_written`.
#' @noRd
mg2_post_data_values <- function(base_url,
                                  username    = "admin",
                                  password    = "district",
                                  data_values) {
  if (!nrow(data_values)) return(list(ok = FALSE, message = "No data values to write.", n_written = 0L))

  url  <- paste0(gsub("/$", "", base_url), "/api/dataValueSets")
  body <- jsonlite::toJSON(list(dataValues = data_values), auto_unbox = TRUE)

  resp <- tryCatch(
    httr::POST(
      url,
      httr::authenticate(username, password),
      httr::content_type_json(),
      body   = body,
      encode = "raw",
      httr::timeout(120L)
    ),
    error = function(e) list(error = conditionMessage(e))
  )
  if (!is.null(resp$error))
    return(list(ok = FALSE, status = NA_integer_, message = resp$error, n_written = 0L))

  code     <- httr::status_code(resp)
  body_out <- tryCatch(
    httr::content(resp, as = "parsed", type = "application/json"),
    error = function(e) NULL
  )
  ic  <- body_out$importCount
  n   <- .nullish(ic$imported, 0L) + .nullish(ic$updated, 0L)
  msg <- paste0("Imported: ", .nullish(ic$imported, 0L),
                " | Updated: ", .nullish(ic$updated,  0L),
                " | Ignored: ", .nullish(ic$ignored,  0L))

  list(ok = code %in% c(200L, 201L), status = code, message = msg, n_written = n)
}

#' Check what metadata is already present in a DHIS2 instance
#'
#' Performs three lightweight API queries to determine how much metadata has
#' already been imported, without loading large payloads.
#'
#' @param base_url Target base URL.
#' @param username,password Credentials.
#' @return List: `ok` (logical), `n_org_units` (integer), `n_data_elements`
#'   (integer), `mg2_installed` (logical — TRUE if all 4 MG2 output elements
#'   exist), `message` (summary string).
#' @noRd
mg2_check_instance_metadata <- function(base_url,
                                         username = "admin",
                                         password = "district") {
  base <- gsub("/$", "", base_url)
  auth <- httr::authenticate(username, password)
  tmo  <- httr::timeout(10L)

  .get_count <- function(path) {
    r <- tryCatch(
      httr::GET(paste0(base, path), auth, tmo),
      error = function(e) NULL
    )
    if (is.null(r) || httr::status_code(r) != 200L) return(NA_integer_)
    b <- tryCatch(httr::content(r, as = "parsed", type = "application/json"),
                  error = function(e) NULL)
    as.integer(b$pager$total %||% NA_integer_)
  }

  n_ou <- .get_count("/api/organisationUnits.json?fields=id&pageSize=0")
  n_de <- .get_count("/api/dataElements.json?fields=id&pageSize=0")

  # Check for MG2 output elements — just test the first one
  mg2_r <- tryCatch(
    httr::GET(paste0(base, "/api/dataElements/", .OUT_ELEMENT_UIDS[["trend"]], ".json"),
              auth, tmo),
    error = function(e) NULL
  )
  mg2_installed <- !is.null(mg2_r) && httr::status_code(mg2_r) == 200L

  list(
    ok             = !is.na(n_ou),
    n_org_units    = n_ou,
    n_data_elements = n_de,
    mg2_installed  = mg2_installed,
    message        = sprintf("%s org units, %s data elements%s",
                             if (is.na(n_ou)) "?" else format(n_ou, big.mark = ","),
                             if (is.na(n_de)) "?" else format(n_de, big.mark = ","),
                             if (mg2_installed) " (MG2 elements present)" else "")
  )
}

#' Test connectivity to a local DHIS2 instance
#'
#' @param base_url Target base URL.
#' @param username,password Credentials.
#' @return List: `ok` (logical), `version` (character or NA), `message`.
#' @noRd
mg2_test_dhis2_connection <- function(base_url,
                                       username = "admin",
                                       password = "district") {
  url  <- paste0(gsub("/$", "", base_url), "/api/system/info.json")
  resp <- tryCatch(
    httr::GET(url, httr::authenticate(username, password), httr::timeout(8L)),
    error = function(e) list(error = conditionMessage(e))
  )
  if (!is.null(resp$error)) return(list(ok = FALSE, version = NA_character_, message = resp$error))
  code <- httr::status_code(resp)
  if (code != 200L) return(list(ok = FALSE, version = NA_character_, message = paste("HTTP", code)))
  info <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"), error = function(e) NULL)
  list(ok = TRUE, version = info$version %||% "unknown", message = "Connected")
}
