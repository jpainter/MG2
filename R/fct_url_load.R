#' Normalise a user-supplied share URL to a direct download URL
#'
#' Handles Google Drive, Dropbox, and OneDrive share links.
#' Plain URLs (ending in .qs, .rds, .xlsx, etc.) are returned unchanged.
#'
#' @param url Character. A share or direct-download URL.
#' @return Character. A URL suitable for `httr::GET()`.
#' @keywords internal
.mg2_normalize_url <- function(url) {
  url <- trimws(url)
  if (!nzchar(url)) return(url)

  # Google Sheets: docs.google.com/spreadsheets/d/ID/edit?...
  # → export as xlsx directly
  if (grepl("docs\\.google\\.com/spreadsheets", url, ignore.case = TRUE)) {
    id <- regmatches(url, regexpr("(?<=/d/)([A-Za-z0-9_-]+)", url, perl = TRUE))
    if (length(id) && nzchar(id))
      return(paste0("https://docs.google.com/spreadsheets/d/", id,
                    "/export?format=xlsx"))
    return(url)
  }

  # Google Drive: file/d/ID/view  or  open?id=ID  →  uc?export=download&id=ID
  if (grepl("drive\\.google\\.com", url, ignore.case = TRUE)) {
    id <- regmatches(url, regexpr("(?<=/d/)([A-Za-z0-9_-]+)", url, perl = TRUE))
    if (!length(id) || !nzchar(id))
      id <- regmatches(url, regexpr("(?<=id=)([A-Za-z0-9_-]+)", url, perl = TRUE))
    if (length(id) && nzchar(id))
      return(paste0("https://drive.google.com/uc?export=download&confirm=t&id=", id))
    return(url)
  }

  # Dropbox: replace dl=0 → dl=1; or www.dropbox.com → dl.dropboxusercontent.com
  if (grepl("dropbox\\.com", url, ignore.case = TRUE)) {
    url <- sub("\\?dl=0", "?dl=1", url)
    url <- sub("&dl=0",   "&dl=1", url)
    if (!grepl("dl=1", url)) url <- paste0(url, if (grepl("\\?", url)) "&dl=1" else "?dl=1")
    return(url)
  }

  # OneDrive personal share link: add download=1
  if (grepl("1drv\\.ms|onedrive\\.live\\.com", url, ignore.case = TRUE)) {
    url <- sub("&download=0", "", url)
    url <- if (grepl("\\?", url)) paste0(url, "&download=1") else paste0(url, "?download=1")
    return(url)
  }

  url
}


#' Download MG2 data files from shared URLs to a local directory
#'
#' Downloads one to three files (processed data, formula xlsx, metadata rds)
#' to `destdir` and returns the path. The directory can then be set as the
#' MG2 working directory so the normal file-discovery logic picks up the files.
#'
#' Share links from Google Drive, Dropbox, and OneDrive are converted to
#' direct download URLs automatically.
#'
#' @param data_url Character. URL to a `.qs` or `.rds` processed dataset (required).
#' @param formula_url Character or `NULL`. URL to the formula `.xlsx` file.
#' @param meta_url Character or `NULL`. URL to the metadata `.rds` file.
#' @param destdir Character. Directory to write files into (created if needed).
#'   Defaults to a `mg2_url_load/` subdirectory inside `tempdir()`.
#' @param timeout Integer. Download timeout in seconds (default 120).
#' @return The path to `destdir`, invisibly.
#' @export
mg2_load_from_urls <- function(data_url,
                                formula_url = NULL,
                                meta_url    = NULL,
                                destdir     = file.path(tempdir(), "mg2_url_load"),
                                timeout     = 120L) {

  if (!nzchar(trimws(data_url %||% "")))
    stop("data_url is required.")

  if (!dir.exists(destdir))
    dir.create(destdir, recursive = TRUE)

  .download_one <- function(url, label, allowed_ext) {
    url  <- .mg2_normalize_url(url)
    resp <- httr::GET(url, httr::timeout(timeout),
                      httr::user_agent("Mozilla/5.0 (MG2)"))

    if (httr::http_error(resp))
      stop(label, " download failed (HTTP ", httr::status_code(resp), "): ", url)

    # Guard: Google Drive may return an HTML confirmation page instead of the
    # file (virus-scan prompt for larger files). Detect via Content-Type and
    # follow the embedded download link if present.
    ct <- httr::headers(resp)[["content-type"]] %||% ""
    if (grepl("text/html", ct, ignore.case = TRUE)) {
      html <- httr::content(resp, "text", encoding = "UTF-8")
      m <- regmatches(html, regexpr(
        "/uc\\?export=download[^\"'>]*confirm=[A-Za-z0-9_-]+[^\"'>]*",
        html, perl = TRUE
      ))
      if (length(m) && nzchar(m)) {
        confirm_url <- paste0("https://drive.google.com",
                              gsub("&amp;", "&", m[1]))
        resp <- httr::GET(confirm_url, httr::timeout(timeout),
                          httr::user_agent("Mozilla/5.0 (MG2)"))
        if (httr::http_error(resp))
          stop(label, " download failed after Drive confirmation (HTTP ",
               httr::status_code(resp), ")")
      } else {
        stop(
          label, " download returned an HTML page instead of a file.\n",
          "For Google Drive: open the file \u2192 Share \u2192 ",
          "'Anyone with the link' \u2192 Copy link.\n",
          "URL tried: ", url
        )
      }
    }

    # Detect extension from Content-Disposition → URL → allowed_ext[1]
    cd   <- httr::headers(resp)[["content-disposition"]]
    ext  <- if (!is.null(cd) && grepl("filename", cd, ignore.case = TRUE)) {
      m <- regmatches(cd, regexpr("\\.([A-Za-z0-9]+)(?=[\"\\s;]|$)", cd, perl = TRUE))
      if (length(m)) m else allowed_ext[1]
    } else {
      url_path <- sub("\\?.*$", "", url)
      m <- regmatches(url_path, regexpr("\\.([A-Za-z0-9]+)$", url_path))
      if (length(m) && tolower(m) %in% allowed_ext) m else allowed_ext[1]
    }

    fname <- paste0(label, "_", format(Sys.Date(), "%Y-%m-%d"), ext)
    dest  <- file.path(destdir, fname)
    writeBin(httr::content(resp, "raw"), dest)
    message(label, " saved: ", basename(dest))
    dest
  }

  .download_one(data_url, "data",     c(".qs", ".rds"))

  if (!is.null(formula_url) && nzchar(trimws(formula_url)))
    .download_one(formula_url, "Formulas", c(".xlsx"))

  if (!is.null(meta_url) && nzchar(trimws(meta_url)))
    .download_one(meta_url, "metadata", c(".rds"))

  message("Files ready in: ", destdir)
  invisible(destdir)
}
