# Utility functions for MG2 package
# General-purpose helpers used throughout the package and Shiny app.

# Date conversion ----------------------------------------------------------

#' Convert a Date String to yearmonth
#'
#' Converts a character string to a [tsibble::yearmonth()] object using
#' [zoo::as.yearmon()] for flexible date parsing.
#'
#' @param date.string Character vector of date strings.
#' @param fmt Character. Format string passed to [zoo::as.yearmon()]
#'   (default: `"%B%Y"`, e.g., `"January2020"`).
#'
#' @return A `tsibble::yearmonth` vector.
#' @export
#'
#' @examples
#' as.yearmonth("January2020")
#' as.yearmonth("201901", fmt = "%Y%m")
as.yearmonth <- function(date.string, fmt = "%B%Y") {
  zoo::as.yearmon(date.string, fmt) |> tsibble::yearmonth()
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

#' Read an RDS or FST File
#'
#' Reads a data file in either `.rds` or `.fst` format, selecting the
#' appropriate reader based on the file extension. The `.fst` format provides
#' faster I/O for large datasets but requires the `fst` package.
#'
#' @param filename Character. Path to the file.
#'
#' @return A data frame or tibble.
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- read_file("data/malaria_2023-01-15.rds")
#' }
read_file <- function(filename) {
  ext <- tools::file_ext(filename)

  if (tolower(ext) == "fst") {
    if (!requireNamespace("fst", quietly = TRUE)) {
      stop("Package 'fst' is required to read .fst files. Install with: install.packages('fst')")
    }
    return(fst::read_fst(filename))
  }

  if (tolower(ext) == "rds") {
    return(readRDS(filename))
  }

  stop(
    "Unsupported file extension: '", ext, "'. Only .rds and .fst files are supported."
  )
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
  return(matched[rev(order(matched))])
}

# DHIS2 period code generators ---------------------------------------------

#' Generate DHIS2 Monthly Period Codes
#'
#' Returns a semicolon-separated string of DHIS2 monthly period codes
#' (e.g. `"202001;202002;..."`), covering a range of months ending at the
#' current month.
#'
#' @param years Integer vector of years. If `NULL`, computed from `YrsPrevious`.
#' @param months Integer vector of months (1–12). Ignored when `years` is `NULL`.
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
