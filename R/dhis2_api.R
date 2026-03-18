# DHIS2 API helpers
# Functions for retrieving data and metadata from a DHIS2 server.

#' GET a DHIS2 API Endpoint and Parse JSON
#'
#' Sends an authenticated GET request to a DHIS2 API URL and parses the JSON
#' response. This is the core HTTP helper used by all metadata retrieval
#' functions.
#'
#' @param source_url Character. Full API URL to request (e.g.,
#'   `"https://play.dhis2.org/2.39/api/dataElements.json?fields=id,name&paging=false"`).
#' @param username Character or `NULL`. DHIS2 username. When `NULL` the request
#'   is sent without credentials (useful for public instances).
#' @param password Character or `NULL`. DHIS2 password.
#' @param .print Logical. Print the URL being requested (default: `FALSE`).
#'
#' @return A parsed list (from `jsonlite::fromJSON()`), `NULL` on HTTP error,
#'   or the raw text if the response is not valid JSON.
#' @export
#'
#' @examples
#' \dontrun{
#'   result <- dhis2_get(
#'     "https://play.dhis2.org/2.39/api/system/info",
#'     username = "admin",
#'     password = "district"
#'   )
#' }
dhis2_get <- function(source_url, username = NULL, password = NULL, .print = FALSE) {
  # Many DHIS2 deployments use self-signed certificates
  httr::set_config(httr::config(ssl_verifypeer = 0L))

  if (.print) message("Requesting: ", source_url)

  response <- if (!is.null(username) && nchar(username) > 0) {
    httr::GET(source_url, httr::authenticate(username, password))
  } else {
    httr::GET(source_url)
  }

  if (response$status_code != 200L) {
    message("HTTP ", response$status_code, " for: ", source_url)
    return(NULL)
  }

  content_text <- httr::content(response, "text", encoding = "UTF-8")

  if (!jsonlite::validate(content_text)) {
    return(content_text)
  }

  result <- jsonlite::fromJSON(content_text)

  if (length(result) == 0) return(NA)

  return(result)
}


#' Build a DHIS2 Analytics API URL
#'
#' Constructs the full URL for a DHIS2 data values request. When `childOnly`
#' is `TRUE` the `dataValueSets` endpoint is used (raw entered values);
#' when `FALSE` the analytics `dataValueSet` endpoint is used (aggregated).
#'
#' @param baseurl Character. Base URL of the DHIS2 server (with trailing `/`).
#' @param de Character. Data element UID, optionally with category option combo
#'   separated by a dot (e.g. `"pikOziyCXbM.abc123"`).
#' @param periods Character. Semicolon-separated DHIS2 period codes
#'   (e.g. `"202401;202402"`).
#' @param orgUnits Character. Semicolon-separated org unit UIDs.
#' @param aggregationType Character. Aggregation type (e.g. `"SUM"`, `"COUNT"`).
#' @param childOnly Logical. Use data entry (`TRUE`) or analytics (`FALSE`)
#'   endpoint.
#'
#' @return Character. Full API URL string.
#' @noRd
api_url <- function(baseurl, de, periods, orgUnits, aggregationType,
                    childOnly = TRUE) {
  if (childOnly) {
    paste0(
      baseurl, "api/dataValueSets.json?",
      "orgUnit=",     orgUnits,
      "&period=",     periods,
      "&dataElement=", de,
      "&children=",   childOnly
    )
  } else {
    paste0(
      baseurl, "api/analytics/dataValueSet.json?",
      "dimension=ou:", orgUnits,
      "&dimension=pe:", periods,
      "&dimension=dx:", de,
      "&displayProperty=NAME",
      "&aggregationType=", aggregationType
    )
  }
}


#' Fetch a Single DHIS2 Data Request
#'
#' Builds a URL with [api_url()], fetches data with [dhis2_get()], and returns
#' a tidy tibble. Returns a one-row NA tibble when the server returns no data.
#'
#' @param baseurl. Character. Base URL of the DHIS2 server.
#' @param username Character. DHIS2 username.
#' @param password Character. DHIS2 password.
#' @param de. Character. Data element UID (with optional `.categoryOptionCombo`).
#' @param periods. Character. Period code(s) (semicolon-separated).
#' @param orgUnits. Character. Org unit UID(s) (semicolon-separated).
#' @param aggregationType. Character. `"SUM"` or `"COUNT"`.
#' @param get.print Logical. Print progress messages (default: `FALSE`).
#' @param childOnly Logical. Use data entry endpoint (default: `TRUE`).
#'
#' @return A tibble of data values, or a one-row tibble with `value = NA`
#'   when no data are returned.
#' @noRd
fetch_get <- function(baseurl., username = NULL, password = NULL,
                      de., periods., orgUnits., aggregationType.,
                      get.print = FALSE, childOnly = TRUE) {
  # Coerce arguments to plain character (callers may pass data frame columns)
  de.        <- as.character(de.)
  periods.   <- as.character(periods.)
  orgUnits.  <- as.character(orgUnits.)

  # Strip category option combo from de. when using childOnly endpoint
  de_api <- if (childOnly && grepl(".", de., fixed = TRUE)) {
    sub("\\..*", "", de.)
  } else {
    de.
  }

  url <- api_url(baseurl., de_api, periods., orgUnits., aggregationType.,
                 childOnly)

  result <- dhis2_get(url, username = username, password = password,
                      .print = get.print)

  # dataValueSets returns list with $dataValues; analytics returns $dataValues too
  fetch <- if (!is.null(result) && is.list(result) && !is.null(result[["dataValues"]])) {
    result[["dataValues"]]
  } else if (is.data.frame(result)) {
    result
  } else {
    NULL
  }

  if (!is.null(fetch) && is.data.frame(fetch) && nrow(fetch) > 0) {
    drop_cols <- intersect(
      colnames(fetch),
      c("storedBy", "created", "lastUpdated", "comment")
    )
    data.return <- dplyr::select(fetch, -dplyr::all_of(drop_cols)) |>
      dplyr::as_tibble()

    if (get.print) cat(nrow(data.return), "records\n")
  } else {
    de_parts <- strsplit(de., ".", fixed = TRUE)[[1]]
    data.return <- tibble::tibble(
      dataElement         = de_parts[1],
      categoryOptionCombo = if (length(de_parts) > 1) de_parts[2] else NA_character_,
      period              = periods.,
      orgUnit             = orgUnits.,
      aggregationType     = aggregationType.,
      value               = NA_character_
    )

    if (get.print) cat(de., periods., orgUnits., aggregationType., ": no records\n")
  }

  return(data.return)
}


#' Build an Org Unit Hierarchy Table
#'
#' Converts a flat org unit list (from the DHIS2 API) and a levels table into
#' a wide-format hierarchy table. Each row represents one org unit and columns
#' give the names of its ancestors at each administrative level. Uses
#' `data.tree` to traverse the parent-child network.
#'
#' @param ous Data frame of org units as returned by the DHIS2
#'   `organisationUnits` API endpoint. Must contain columns `id`, `name`,
#'   `parent.id`, `level`, and `leaf`.
#' @param ouLevels Data frame of org unit levels with columns `level` (integer)
#'   and `levelName` (character).
#'
#' @return A tibble with one row per org unit. Columns: `orgUnit` (id),
#'   `orgUnitName`, one column per admin level named by `levelName`,
#'   `level`, and `leaf`.
#' @export
#'
#' @examples
#' \dontrun{
#'   tree <- ous_tree(ous = org_units_df, ouLevels = levels_df)
#' }
ous_tree <- function(ous, ouLevels) {
  cat("\n* ous_tree: building org unit hierarchy\n")

  # Remove known-bad phantom parents (e.g. Benin "UO_supprimé")
  bad_parent <- dplyr::filter(ous, parent == "UO_supprim\u00e9") |> dplyr::pull(id)
  if (length(bad_parent) > 0) {
    bad2 <- dplyr::filter(ous, parent.id %in% bad_parent) |> dplyr::pull(id)
    bad3 <- dplyr::filter(ous, parent.id %in% bad2)       |> dplyr::pull(id)
    bad4 <- dplyr::filter(ous, parent.id %in% bad3)       |> dplyr::pull(id)
    all_bad <- c(bad_parent, bad2, bad3, bad4)
    cat(" - removing", length(all_bad), "phantom org units\n")
    ous <- dplyr::filter(ous, !id %in% all_bad)
  }

  # Build id -> parent_id edge list
  if (is.data.frame(ous$parent)) {
    ous_edges <- ous |>
      dplyr::mutate(parent = ous$parent$id) |>
      dplyr::filter(!is.na(parent)) |>
      dplyr::select(id, parent)
  } else {
    ous_edges <- ous |>
      dplyr::filter(!is.na(parent.id)) |>
      dplyr::arrange(level) |>
      dplyr::mutate(parent = parent.id) |>
      dplyr::select(id, parent)
  }

  cat(" - building tree from", nrow(ous_edges), "edges\n")
  ous_tree_obj <- data.tree::FromDataFrameNetwork(ous_edges)
  dti           <- data.tree::as.igraph.Node(ous_tree_obj)
  ids           <- names(igraph::V(dti))

  n_levels <- nrow(ouLevels)

  # Build named list of path-extractor functions for ToDataFrameTree
  path_fns <- lapply(seq_len(n_levels), function(i) {
    force(i)
    function(x) x$path[i]
  })
  names(path_fns) <- paste0("lvl", seq_len(n_levels))

  all_args <- c(
    list(ous_tree_obj, orgUnit = ids),
    path_fns,
    list(level = function(x) as.integer(x$level))
  )

  dft <- do.call(data.tree::ToDataFrameTree, all_args)

  # Remove first column (tree-structure level indicator added by data.tree)
  dft <- dft[, -1] |>
    dplyr::as_tibble() |>
    dplyr::select(orgUnit, dplyr::starts_with("lvl"), level)

  # Rename lvl1..lvlN to level names
  current_names <- names(dft)
  lvl_cols      <- grep("^lvl", current_names, value = TRUE)
  names(dft)[match(lvl_cols, current_names)] <- ouLevels$levelName

  # Replace org unit IDs in level columns with human-readable names
  id_name_lookup <- dplyr::select(ous, id, name)

  dft_translated <- dft |>
    tidyr::pivot_longer(
      cols      = c(-orgUnit, -level),
      names_to  = "Level"
    ) |>
    dplyr::left_join(id_name_lookup, by = c("value" = "id")) |>
    tidyr::pivot_wider(
      id_cols     = -value,
      names_from  = Level,
      values_from = name
    ) |>
    dplyr::left_join(
      dplyr::select(ous, id, name, leaf),
      by = c("orgUnit" = "id")
    ) |>
    dplyr::rename(orgUnitName = name) |>
    dplyr::arrange(level) |>
    dplyr::select(orgUnit, orgUnitName, dplyr::everything())

  cat(" - tree complete:", nrow(dft_translated), "org units\n")
  return(dft_translated)
}
