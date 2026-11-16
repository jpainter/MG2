# DHIS2 API helpers
# Functions for retrieving data and metadata from a DHIS2 server.

# Session-level flags for one-time notifications
.mg2_state <- new.env(parent = emptyenv())

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
  # Many DHIS2 deployments use self-signed certificates; scope the bypass to
  # this request only via with_config() rather than the session-wide set_config().
  if (!isTRUE(.mg2_state$ssl_bypass_warned)) {
    message(
      "Note: SSL certificate verification is disabled for DHIS2 requests. ",
      "This accommodates self-signed certificates common in DHIS2 deployments."
    )
    .mg2_state$ssl_bypass_warned <- TRUE
  }

  if (.print) message("Requesting: ", source_url)

  ssl_cfg  <- httr::config(ssl_verifypeer = 0L)
  response <- httr::with_config(ssl_cfg, {
    if (!is.null(username) && nchar(username) > 0) {
      httr::GET(source_url, httr::authenticate(username, password), httr::timeout(300))
    } else {
      httr::GET(source_url, httr::timeout(300))
    }
  })

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
#' Builds a URL with `api_url()`, fetches data with [dhis2_get()], and returns
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

    # DHIS2 dataValueSets treats period/orgUnit as filter parameters and may
    # omit them from individual dataValues records. Inject from the request
    # parameters when the column is absent or entirely NA.
    if (!"period" %in% names(data.return) || all(is.na(data.return$period))) {
      data.return[["period"]] <- periods.
    }
    if (!"orgUnit" %in% names(data.return) || all(is.na(data.return$orgUnit))) {
      data.return[["orgUnit"]] <- orgUnits.
    }

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
#' @param .verbose Logical. Print progress messages (default: `FALSE`).
#'
#' @return A tibble with one row per org unit. Columns: `orgUnit` (id),
#'   `orgUnitName`, one column per admin level named by `levelName`,
#'   `level`, and `leaf`.
#' @export
#'
#' @examples
#' tree <- ous_tree(ous = mg2_demo_meta$orgUnits,
#'                  ouLevels = mg2_demo_meta$orgUnitLevels)
#' head(tree)
ous_tree <- function(ous, ouLevels, .verbose = FALSE) {
  if (.verbose) message("ous_tree: building org unit hierarchy")

  # Remove known-bad phantom parents (e.g. Benin "UO_supprimé").
  # The parent column may be:
  #   (a) a nested data.frame (raw jsonlite output from the app), or
  #   (b) absent (replaced by parent.id / parent.name after flattening), or
  #   (c) a plain character vector.
  # Only apply the name-based phantom check when a character parent.name column
  # is available; skip silently otherwise (Sierra Leone / most instances are fine).
  phantom_name <- "UO_supprim\u00e9"
  parent_names <- if ("parent.name" %in% names(ous) && is.character(ous$parent.name)) {
    ous$parent.name
  } else if ("parent" %in% names(ous) && is.data.frame(ous$parent)) {
    ous$parent$name
  } else if ("parent" %in% names(ous) && is.character(ous$parent)) {
    ous$parent
  } else {
    character(0)
  }

  if (length(parent_names) > 0) {
    bad_parent <- ous$id[!is.na(parent_names) & parent_names == phantom_name]
    if (length(bad_parent) > 0) {
      id_col <- ous$id
      pid_col <- if ("parent.id" %in% names(ous)) ous$parent.id else ous$parent$id
      bad2 <- id_col[!is.na(pid_col) & pid_col %in% bad_parent]
      bad3 <- id_col[!is.na(pid_col) & pid_col %in% bad2]
      bad4 <- id_col[!is.na(pid_col) & pid_col %in% bad3]
      all_bad <- c(bad_parent, bad2, bad3, bad4)
      if (.verbose) message("- removing ", length(all_bad), " phantom org units")
      ous <- ous[!ous$id %in% all_bad, ]
    }
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

  if (.verbose) message("- building tree from ", nrow(ous_edges), " edges")
  dft <- .ous_paths(ous_edges$id, ous_edges$parent, n_levels = nrow(ouLevels))

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

  if (.verbose) message("- tree complete: ", nrow(dft_translated), " org units")
  return(dft_translated)
}

#' Ancestor path of every node in an org unit tree
#'
#' Base-R replacement for data.tree's FromDataFrameNetwork() +
#' ToDataFrameTree(): for each node, the ids of its ancestors at depth
#' 1..n_levels (`lvl1` = root), its depth (`level`, root = 1), with rows in
#' pre-order (each node followed by its subtree, siblings in edge order).
#' @param id,parent Character vectors: the edge list (child id, parent id).
#' @param n_levels Number of `lvl` columns to return.
#' @return A tibble: `orgUnit`, `lvl1`..`lvl<n_levels>`, `level`.
#' @keywords internal
#' @noRd
.ous_paths <- function(id, parent, n_levels) {
  roots  <- setdiff(unique(parent), id)
  nodes  <- c(roots, id)
  p_idx  <- match(c(rep(NA_character_, length(roots)), parent), nodes)
  n      <- length(nodes)

  # chain[[s + 1]][i] = index of the ancestor s steps above node i
  chain <- list(seq_len(n))
  cur   <- p_idx
  while (any(!is.na(cur))) {
    if (length(chain) > n) stop("ous_tree: org unit hierarchy contains a cycle", call. = FALSE)
    chain[[length(chain) + 1L]] <- cur
    cur <- p_idx[cur]
  }
  depth <- Reduce(`+`, lapply(chain, function(a) !is.na(a)))   # root = 1

  anc_at <- function(j) {               # ancestor index at depth j (NA if deeper than node)
    step <- depth - j
    out  <- rep(NA_integer_, n)
    for (s in unique(step[step >= 0L])) {
      k <- which(step == s)
      out[k] <- chain[[s + 1L]][k]
    }
    out
  }
  max_depth <- max(depth)
  anc <- lapply(seq_len(max(n_levels, max_depth)), anc_at)

  # Pre-order: sort by ancestor ranks, a parent (NA below it) before its children
  ord <- do.call(order, c(anc[seq_len(max_depth)], list(na.last = FALSE)))

  lvl <- lapply(anc[seq_len(n_levels)], function(a) nodes[a][ord])
  names(lvl) <- paste0("lvl", seq_len(n_levels))
  tibble::as_tibble(c(list(orgUnit = nodes[ord]), lvl, list(level = as.integer(depth[ord]))))
}
