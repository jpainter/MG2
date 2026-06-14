# Utility functions for MG2 package
# General-purpose helpers used throughout the package and Shiny app.

# Shiny UI helpers ---------------------------------------------------------

#' Step hint bar for Shiny tab navigation
#'
#' Renders a small blue hint bar pointing users to the next step.
#' Used at the top of tab UI functions to guide new users.
#'
#' @param text Character. The hint text to display.
#' @return A `div` tag.
#' @export
.mg2_step_hint <- function(text) {
  shiny::div(
    style = paste0(
      "margin-top:24px; padding:8px 16px; background:#f0f4ff;",
      " border-left:4px solid #4a90d9; border-radius:3px; color:#555; font-size:13px;"
    ),
    shiny::icon("circle-info", style = "color:#4a90d9; margin-right:6px;"),
    text
  )
}

# Date conversion ----------------------------------------------------------

#' Convert a Date String to yearmonth
#'
#' Converts a character string (or existing `yearmonth`) to a
#' [tsibble::yearmonth()] object.  Multiple formats are tried automatically:
#' * `"January2020"` / `"Jan2020"` (full or abbreviated month + year)
#' * `"2025 Jan"` (tsibble's `as.character()` output)
#' * `"202501"` / `"2025-01"` (DHIS2 / ISO numeric)
#' * Any format recognised by [zoo::as.yearmon()] or [lubridate::ym()]
#'
#' An explicit `fmt` can be supplied to skip auto-detection.
#'
#' @param date.string Character vector of date strings, or a `yearmonth` vector.
#' @param fmt Character. Optional format string passed to [zoo::as.yearmon()].
#'   When `NULL` (default) several common formats are tried in order.
#'
#' @return A `tsibble::yearmonth` vector.
#' @export
#'
#' @examples
#' as.yearmonth("January2020")
#' as.yearmonth("2025 Jan")
#' as.yearmonth("201901")
as.yearmonth <- function(date.string, fmt = NULL) {
  # Already the right type  -  return as-is
  if (tsibble::is_yearmonth(date.string)) return(date.string)

  # Explicit format supplied
  if (!is.null(fmt)) {
    return(zoo::as.yearmon(date.string, fmt) |> tsibble::yearmonth())
  }

  # Try formats in order of likelihood
  fmts <- c("%B%Y", "%b%Y", "%Y %b", "%b %Y", "%Y%m", "%Y-%m")
  for (f in fmts) {
    result <- tryCatch(
      zoo::as.yearmon(date.string, f) |> tsibble::yearmonth(),
      warning = function(w) NULL,
      error   = function(e) NULL
    )
    if (!is.null(result) && !anyNA(result)) return(result)
  }

  # Final fallback: zoo generic (tries its own set of formats)
  result <- tryCatch(
    zoo::as.yearmon(date.string) |> tsibble::yearmonth(),
    warning = function(w) NULL,
    error   = function(e) NULL
  )
  if (!is.null(result) && !anyNA(result)) return(result)

  # lubridate fallback
  tsibble::yearmonth(lubridate::ym(date.string))
}

#' Convert a DHIS2 Monthly Period Code to yearmonth
#'
#' Parses DHIS2 monthly period codes (format `"YYYYMM"`, e.g., `"201901"`)
#' to [tsibble::yearmonth()] objects.
#'
#' @param x Character vector of DHIS2 monthly period codes.
#'
#' @return A `tsibble::yearmonth` vector.
#' @export
#'
#' @examples
#' Month_Year("201901")
Month_Year <- function(x) {
  tsibble::yearmonth(zoo::as.yearmon(x, "%Y%m"))
}

#' Convert a DHIS2 Weekly Period Code to yearweek
#'
#' Parses DHIS2 weekly period codes to [tsibble::yearweek()] objects.
#'
#' @param x Character vector of DHIS2 weekly period codes.
#'
#' @return A `tsibble::yearweek` vector.
#' @export
#'
#' @examples
#' Week_Year("2019W01")
Week_Year <- function(x) {
  tsibble::yearweek(x)
}

# File I/O ------------------------------------------------------------------

#' Read an RDS, QS2, or FST File
#'
#' Reads a data file in `.rds`, `.qs`, or `.fst` format, selecting the
#' appropriate reader based on the file extension. `.qs` is the preferred
#' format: fast I/O with ZSTD compression and full R object preservation
#' (no class-stripping). `.fst` is supported for legacy files.
#'
#' @param filename Character. Path to the file.
#'
#' @return A data frame or tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- read_file("data/malaria_2023-01-15.qs")
#' }
read_file <- function(filename) {
  ext <- tools::file_ext(filename)

  if (tolower(ext) == "qs") {
    return(qs2::qs_read(filename))
  }

  if (tolower(ext) == "fst") {
    if (!requireNamespace("fst", quietly = TRUE)) {
      stop("Package 'fst' is required to read .fst files. Install with: install.packages('fst')")
    }
    df <- fst::read_fst(filename)

    # FST stores yearmonth/yearweek as plain numeric  -  restore the S3 class
    # so downstream tsibble operations work correctly.
    # Must include "vctrs_vctr" in the class vector; without it as.Date()
    # falls through to as.Date.default and throws "do not know how to convert".
    #
    # Guard: tsibble yearmonth integers for 2000-2040 are ~360-840.
    # Days-since-1970-01-01 for the same range are ~10,000-25,000.
    # If the median value exceeds 5000 the column is a Date integer, not a
    # yearmonth integer  -  leave it alone to avoid corrupting the values.
    if ("Month" %in% names(df) && !inherits(df[["Month"]], "yearmonth")) {
      m_vals <- as.numeric(df[["Month"]])
      if (median(m_vals, na.rm = TRUE) <= 5000) {
        # Months-since-epoch encoding (old tsibble/FST): convert to days-since-epoch.
        # e.g. Jan 2019 stored as 588 months → needs to become 17897 days.
        yr   <- 1970L + as.integer(m_vals) %/% 12L
        mo   <- as.integer(m_vals) %% 12L + 1L
        days <- as.double(as.Date(paste0(yr, "-", sprintf("%02d", mo), "-01")) -
                            as.Date("1970-01-01"))
        df[["Month"]] <- structure(days, class = c("yearmonth", "vctrs_vctr"))
      } else {
        # Days-since-epoch encoding (current): values are correct, just restore class.
        df[["Month"]] <- structure(m_vals, class = c("yearmonth", "vctrs_vctr"))
      }
    }
    if ("Week" %in% names(df) && !inherits(df[["Week"]], "yearweek")) {
      w_vals <- as.numeric(df[["Week"]])
      if (median(w_vals, na.rm = TRUE) <= 5000)
        df[["Week"]] <- structure(w_vals, class = c("yearweek", "vctrs_vctr"))
    }

    # If this looks like a processed MG2 dataset (has orgUnit + data.id + index),
    # rebuild the tsibble so it behaves identically to one loaded from .rds.
    idx_var  <- if ("Month" %in% names(df)) "Month" else if ("Week" %in% names(df)) "Week" else NULL
    key_vars <- intersect(c("orgUnit", "data.id"), names(df))
    if (!is.null(idx_var) && length(key_vars) == 2L) {
      df <- tryCatch(
        tsibble::as_tsibble(df, index = idx_var, key = dplyr::all_of(key_vars),
                            validate = FALSE),
        error = function(e) df   # fall back to plain data.frame on any error
      )
    }

    return(df)
  }

  if (tolower(ext) == "rds") {
    return(readRDS(filename))
  }

  stop(
    "Unsupported file extension: '", ext, "'. Supported: .qs, .rds, .fst."
  )
}


#' Return the Preferred Data File Extension
#'
#' Returns `"qs2"` when the `qs2` package is installed (fast I/O, ZSTD
#' compressed, full R object preservation), otherwise `"rds"`. Use this to
#' build filenames for [save_file()].
#'
#' @return `"qs2"` or `"rds"`.
#' @export
mg2_data_ext <- function() "qs"


#' Save a Data Frame to Disk
#'
#' Writes a data frame to a `.qs`, `.rds`, or `.fst` file based on the file
#' extension in `filename`. `.qs` is the preferred format: ZSTD-compressed,
#' fast, and preserves all R object classes (including `yearmonth`) without
#' any post-read restoration. `.rds` and legacy `.fst` are also supported.
#'
#' @param x A data frame or data.table.
#' @param filename Character. Destination path. Extension must be `".qs"`,
#'   `".rds"`, or `".fst"`.
#' @param compress Integer (0-100). Compression level for `.fst` files (default
#'   `100`). Ignored for `.qs` and `.rds`.
#'
#' @return `filename`, invisibly.
#' @export
save_file <- function(x, filename, compress = 100) {
  ext <- tolower(tools::file_ext(filename))

  if (ext == "qs") {
    qs2::qs_save(x, filename)
    return(invisible(filename))
  }

  if (ext == "fst") {
    if (!requireNamespace("fst", quietly = TRUE)) {
      stop("Package 'fst' required to write .fst files. Install with: install.packages('fst')")
    }
    fst::write_fst(as.data.frame(x), filename, compress = compress)
    return(invisible(filename))
  }

  if (ext == "rds") {
    saveRDS(x, filename, compress = FALSE)
    return(invisible(filename))
  }

  stop("Unsupported file extension '", ext, "'. Use .qs, .rds, or .fst.")
}

# Directory helpers ---------------------------------------------------------

#' List Files in a Directory Matching a Pattern
#'
#' Returns filenames (not full paths) in `dir` that match all of `search`,
#' `type`, and `other` patterns. Results are sorted in reverse alphabetical
#' order so the most recently dated filename (by convention `_YYYY-MM-DD`)
#' appears first.
#'
#' @param search Character. Case-insensitive substring the filename must
#'   contain (default: `"All"` matches everything).
#' @param type Character. Regex matched against the file extension, anchored
#'   at end of string (default: `"xlsx"`). Use `"xlsx|rds"` for multiple types.
#' @param other Character. Additional case-insensitive substring the filename
#'   must contain (default: `""` matches everything).
#' @param dir Character. Directory to search. Defaults to the current working
#'   directory if `NULL`.
#'
#' @return Character vector of matching filenames, sorted reverse
#'   alphabetically. `character(0)` if none match or `dir` does not exist.
#' @export
#'
#' @examples
#' list_dir_files(search = "Formulas_", type = "xlsx|rds", dir = tempdir())
list_dir_files <- function(search = "All", type = "xlsx", other = "", dir = NULL) {
  if (is.null(dir)) dir <- getwd()

  if (!dir.exists(dir)) {
    message("list_dir_files: directory not found: ", dir)
    return(character(0))
  }

  all_files <- list.files(dir)

  keep <- stringr::str_detect(all_files, stringr::fixed(search, ignore_case = TRUE)) &
    grepl(paste0(type, "$"), all_files, ignore.case = TRUE) &
    grepl(other, all_files, ignore.case = TRUE)

  matched <- all_files[keep]
  if (length(matched) == 0) return(matched)
  mtimes <- file.info(file.path(dir, matched))$mtime
  return(matched[order(mtimes, decreasing = TRUE)])
}

# DHIS2 period code generators ---------------------------------------------

#' Generate DHIS2 Monthly Period Codes
#'
#' Returns a semicolon-separated string of DHIS2 monthly period codes
#' (e.g. `"202001;202002;..."`), covering a range of months ending at the
#' current month.
#'
#' @param years Integer vector of years. If `NULL`, computed from `YrsPrevious`.
#' @param months Integer vector of months (1-12). Ignored when `years` is `NULL`.
#' @param startPeriod Character. Explicit start period in `"YYYYMM"` format.
#' @param YrsPrevious Integer. Number of full years prior to the current year
#'   to start from (default: `1`).
#' @param monthsPrevious Integer. Number of months prior to today to start from.
#' @param currentMonth Logical. Include the current month (default: `TRUE`).
#'
#' @return A single character string of period codes separated by `";"`.
#' @export
date_code <- function(years = NULL,
                      months = NULL,
                      startPeriod = NULL,
                      YrsPrevious = 1,
                      monthsPrevious = NULL,
                      currentMonth = TRUE) {
  endMonth <- zoo::as.yearmon(Sys.Date())

  if (!is.null(startPeriod)) {
    startMonth <- zoo::as.yearmon(startPeriod, "%Y%m")
  } else if (!is.null(monthsPrevious)) {
    startMonth <- endMonth - monthsPrevious / 12
  } else {
    this.year  <- lubridate::year(Sys.Date())
    start.year <- this.year - YrsPrevious
    startMonth <- zoo::as.yearmon(start.year)
  }

  month_seq <- seq(
    from = zoo::as.Date.yearmon(startMonth, frac = 0),
    to   = zoo::as.Date.yearmon(endMonth,   frac = 1),
    by   = "month"
  )
  codes <- format(zoo::as.yearmon(month_seq), "%Y%m")

  if (!currentMonth) {
    codes <- codes[-length(codes)]
  }

  paste(codes, collapse = ";")
}

#' Generate DHIS2 Weekly Period Codes
#'
#' Returns a semicolon-separated string of DHIS2 ISO weekly period codes
#' (e.g. `"2023W01;2023W02;..."`), covering a range of weeks ending at the
#' current week.
#'
#' @param startPeriod Character. Explicit start period in `"YYYYMM"` format.
#' @param YrsPrevious Integer. Number of full years prior to the current year
#'   to start from (default: `1`).
#' @param monthsPrevious Integer. Number of months prior to today to start from.
#' @param currentWeek Logical. Include the current week (default: `TRUE`).
#'
#' @return A single character string of period codes separated by `";"`.
#' @export
date_code_weekly <- function(startPeriod = NULL,
                             YrsPrevious = 1,
                             monthsPrevious = NULL,
                             currentWeek = TRUE) {
  endMonth <- zoo::as.yearmon(Sys.Date())

  if (!is.null(startPeriod)) {
    startMonth <- zoo::as.yearmon(startPeriod, "%Y%m")
  } else if (!is.null(monthsPrevious)) {
    startMonth <- endMonth - monthsPrevious / 12
  } else {
    this.year  <- lubridate::year(Sys.Date())
    start.year <- this.year - YrsPrevious
    startMonth <- zoo::as.yearmon(start.year)
  }

  week_seq <- seq(
    from = zoo::as.Date.yearmon(startMonth, frac = 0),
    to   = zoo::as.Date.yearmon(endMonth,   frac = 1),
    by   = "1 week"
  )
  codes <- format(tsibble::yearweek(week_seq), "%YW%V")

  if (!currentWeek) {
    codes <- codes[-length(codes)]
  }

  paste(codes, collapse = ";")
}

# Internal helpers ----------------------------------------------------------

#' Extract a Date from a Filename
#'
#' Parses a date embedded in filenames using the pattern `"_YYYYMMDD.ext"`.
#'
#' @param x Character. Filename (with or without directory path).
#'
#' @return A `Date` object.
#' @noRd
get_date_part <- function(x) {
  x_parts <- stringr::str_split(x, "_", simplify = TRUE)
  date_part <- x_parts[length(x_parts)]
  date <- stringr::str_split(date_part, stringr::fixed("."), simplify = TRUE)[1]
  lubridate::ymd(date)
}

#' Count the Length of the Intersection of Two Vectors
#'
#' @param x A vector.
#' @param y A vector.
#'
#' @return Integer. Number of elements in common between `x` and `y`.
#' @noRd
intersect_length <- function(x, y) length(intersect(x, y))


# Dependency checking ---------------------------------------------------------

#' Check MG2 Runtime Dependencies
#'
#' Returns a list of messages describing the current R and package environment,
#' flagging known incompatibilities. Intended for display on app startup.
#'
#' @return A list with elements `info` (character vector of informational lines)
#'   and `warnings` (character vector of warning lines, empty if all OK).
#' @export
check_mg2_dependencies <- function() {
  info  <- character(0)
  warns <- character(0)

  r_ver <- getRversion()
  info  <- c(info, paste0("R ", r_ver))

  if (r_ver < "4.2.0")
    warns <- c(warns, paste0("R >= 4.2 required (running ", r_ver, ")"))

  pkgs <- c("shiny", "bslib", "leaflet", "leaflet.providers", "sf",
            "DT", "dplyr", "tsibble", "fable", "data.table")

  for (p in pkgs) {
    if (requireNamespace(p, quietly = TRUE)) {
      v    <- as.character(utils::packageVersion(p))
      info <- c(info, paste0(p, " ", v))
    } else if (p == "leaflet.providers") {
      warns <- c(warns,
        "leaflet.providers not installed  -  only OpenStreetMap tiles available. ",
        "Install with: install.packages('leaflet.providers')")
    }
  }

  known_issues <- list(
    list(pkg = "bslib",   min = "0.9.0",
         msg = "bslib >= 0.9.0 changed page_navbar()  -  use navbar_options() for bg colour."),
    list(pkg = "leaflet", min = "2.2.0",
         msg = "leaflet < 2.2 has sf polygon rendering bugs. Update with install.packages('leaflet').")
  )
  for (chk in known_issues) {
    if (requireNamespace(chk$pkg, quietly = TRUE)) {
      v <- utils::packageVersion(chk$pkg)
      if (v < chk$min)
        warns <- c(warns, chk$msg)
    }
  }

  list(info = info, warnings = warns)
}
