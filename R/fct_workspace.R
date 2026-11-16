#' MG2 workspace abstraction
#'
#' A thin S3 layer that lets the Shiny app read and write files without knowing
#' whether storage is local, Google Drive, or OneDrive.  Phase 1 implemented
#' the local backend; Phase 2 adds Google Drive via a per-session OAuth token
#' (see `R/fct_gdrive_oauth.R` and the "Connect Google Drive" link in
#' `directory_widget.R` — a web-application redirect flow, not
#' `googledrive::drive_auth()`'s console/local-browser flow, so it works
#' identically locally and on hosted deploys). OneDrive (Phase 3) is still
#' stubbed.
#'
#' @section Constructor:
#' ```r
#' ws <- mg2_workspace("/path/to/dir/")          # local
#' ws <- mg2_workspace("gdrive:FOLDER_ID")       # Google Drive (Phase 2)
#' ws <- mg2_workspace("onedrive:PATH")          # OneDrive (Phase 3)
#' ```
#'
#' @section Generic methods:
#' * `ws_list(ws, pattern)`          — character vector of filenames
#' * `ws_list_files(ws, ...)`        — like `list_dir_files()`, pattern-filtered
#' * `ws_read_file(ws, filename)`    — read .qs/.rds/.fst via `read_file()`
#' * `ws_save_file(ws, data, filename)` — save via `save_file()`
#' * `ws_read_rds(ws, filename)`     — `readRDS()`
#' * `ws_save_rds(ws, obj, filename)` — `saveRDS()`
#' * `ws_read_xlsx(ws, filename)`    — `openxlsx::loadWorkbook()`
#' * `ws_save_xlsx(ws, wb, filename)` — `openxlsx::saveWorkbook()`
#' * `ws_file_exists(ws, filename)`  — `file.exists()`
#' * `ws_file_info(ws, filename)`    — `file.info()`
#' * `ws_file_rename(ws, from, to)`  — `file.rename()`
#' * `ws_path(ws, filename)`         — full local path (escape hatch, local only)
#' * `ws_local_copy(ws, filename)`   — real local path for any backend (escape hatch)
#' * `ws_save_local_copy(ws, local_path, filename)` — land a local file in any backend (escape hatch)
#' * `ws_is_local(ws)`               — TRUE for local backend
#'
#' @name mg2_workspace
#' @noRd
NULL


# ── Constructor ───────────────────────────────────────────────────────────────

#' Create an MG2 workspace object
#'
#' @param path Local directory path, `"gdrive:FOLDER_ID"`, or
#'   `"onedrive:PATH"`.
#' @param token For `"gdrive:"` paths: an `httr::Token2.0` from
#'   `mg2_gdrive_exchange_code()`, scoped to the current Shiny session.
#' @return An `mg2_workspace` S3 object.
#' @noRd
mg2_workspace <- function(path, token = NULL) {
  if (startsWith(path, "gdrive:")) {
    ws <- list(path = path, folder_id = sub("^gdrive:", "", path), token = token)
    class(ws) <- c("mg2_ws_gdrive", "mg2_workspace")
  } else if (startsWith(path, "onedrive:")) {
    ws <- list(path = path, folder_path = sub("^onedrive:", "", path))
    class(ws) <- c("mg2_ws_onedrive", "mg2_workspace")
  } else {
    # Normalize: strip trailing slash so file.path() works cleanly.
    # ws$path for local workspaces is always a bare directory path.
    path <- sub("/$", "", path)
    ws <- list(path = path)
    class(ws) <- c("mg2_ws_local", "mg2_workspace")
  }
  ws
}

#' @export
print.mg2_workspace <- function(x, ...) {
  cat("<mg2_workspace>", class(x)[1], "\n  path:", x$path, "\n")
  invisible(x)
}


# ── Google Drive helpers (internal) ───────────────────────────────────────────
#
# Each mg2_ws_gdrive object carries its own httr::Token2.0 (ws$token), obtained
# via the redirect flow in R/fct_gdrive_oauth.R -- NOT a global
# googledrive::drive_auth() session. googledrive itself has no per-call
# token-scoping mechanism (only global package state), so .ws_gdrive_run()
# points that global state at ws$token immediately before each Drive
# operation. This is safe under Shiny's single-threaded-per-process request
# model (nothing else executes between the drive_auth() call and the
# operation that follows it in the same synchronous call stack), but would
# NOT be safe across multiple worker processes sharing no memory.
#
# Files are matched by name within ws$folder_id; reads download to a session
# temp file and dispatch to the existing local reader, writes do the reverse.

.ws_gdrive_authorize <- function(ws) {
  if (is.null(ws$token)) {
    stop("Google Drive workspace is not authenticated. Reconnect via the Setup tab.")
  }
  googledrive::drive_auth(token = ws$token)
  invisible(ws)
}

# Runs `fn` with ws$token as the active googledrive credential. On error,
# refreshes the token once (access tokens expire after ~1hr; httr::Token2.0
# does not auto-refresh) and retries -- if that also fails, the retry's error
# propagates normally.
.ws_gdrive_run <- function(ws, fn) {
  .ws_gdrive_authorize(ws)
  tryCatch(
    fn(),
    error = function(e) {
      if (isTRUE(ws$token$can_refresh())) {
        ws$token$refresh()
        googledrive::drive_auth(token = ws$token)
        fn()
      } else {
        stop(e)
      }
    }
  )
}

.ws_gdrive_ls <- function(ws) {
  .ws_gdrive_run(ws, function() googledrive::drive_ls(path = googledrive::as_id(ws$folder_id)))
}

.ws_gdrive_find <- function(ws, filename) {
  files <- .ws_gdrive_ls(ws)
  match <- files[files$name == filename, ]
  if (nrow(match) == 0) return(NULL)
  match[1, ]
}

.ws_gdrive_download_temp <- function(ws, filename) {
  match <- .ws_gdrive_find(ws, filename)
  if (is.null(match)) stop("File not found in Google Drive workspace: ", filename)
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(filename)))
  googledrive::drive_download(match, path = tmp, overwrite = TRUE)
  tmp
}

.ws_gdrive_upload_temp <- function(ws, tmp, filename) {
  existing <- .ws_gdrive_find(ws, filename)
  if (!is.null(existing)) {
    googledrive::drive_update(existing, media = tmp)
  } else {
    googledrive::drive_upload(tmp, path = googledrive::as_id(ws$folder_id), name = filename)
  }
  invisible(filename)
}


# ── ws_list ───────────────────────────────────────────────────────────────────

#' List files in a workspace
#'
#' @param ws An `mg2_workspace` object.
#' @param pattern Optional regex pattern passed to `grep()`.
#' @return Character vector of filenames (not full paths).
#' @noRd
ws_list <- function(ws, pattern = NULL) UseMethod("ws_list")

#' @export
ws_list.mg2_ws_local <- function(ws, pattern = NULL) {
  files <- list.files(ws$path)
  if (!is.null(pattern)) files <- files[grepl(pattern, files)]
  files
}

#' @export
ws_list.mg2_ws_gdrive <- function(ws, pattern = NULL) {
  files <- .ws_gdrive_ls(ws)$name
  if (!is.null(pattern)) files <- files[grepl(pattern, files)]
  files
}

#' @export
ws_list.mg2_ws_onedrive <- function(ws, pattern = NULL)
  stop("OneDrive workspace not yet implemented. Coming in Phase 3.")


# ── ws_list_files ─────────────────────────────────────────────────────────────

#' List files matching a search pattern (like list_dir_files)
#'
#' @param ws An `mg2_workspace` object.
#' @param search Substring to match (default `"All"`).
#' @param type File extension pattern (default `"xlsx"`).
#' @param other Additional substring filter (default `""`).
#' @return Character vector of matching filenames.
#' @noRd
ws_list_files <- function(ws, search = "All", type = "xlsx", other = "")
  UseMethod("ws_list_files")

#' @export
ws_list_files.mg2_ws_local <- function(ws, search = "All", type = "xlsx",
                                        other = "") {
  list_dir_files(search = search, type = type, other = other, dir = ws$path)
}

#' @export
ws_list_files.mg2_ws_gdrive <- function(ws, search = "All", type = "xlsx",
                                         other = "") {
  files <- .ws_gdrive_ls(ws)
  if (nrow(files) == 0) return(character(0))
  keep <- stringr::str_detect(files$name, stringr::fixed(search, ignore_case = TRUE)) &
    grepl(paste0(type, "$"), files$name, ignore.case = TRUE) &
    grepl(other, files$name, ignore.case = TRUE)
  files <- files[keep, ]
  if (nrow(files) == 0) return(character(0))
  mtimes <- vapply(files$drive_resource,
                    function(r) r$modifiedTime %||% NA_character_,
                    character(1))
  files$name[order(mtimes, decreasing = TRUE)]
}

#' @export
ws_list_files.mg2_ws_onedrive <- function(ws, search = "All", type = "xlsx",
                                           other = "")
  stop("OneDrive workspace not yet implemented.")


# ── ws_read_file / ws_save_file ───────────────────────────────────────────────

#' Read a data file (.qs, .rds, .fst) from a workspace
#'
#' @param ws An `mg2_workspace` object.
#' @param filename Filename (not a full path).
#' @return R object.
#' @noRd
ws_read_file <- function(ws, filename) UseMethod("ws_read_file")

#' @export
ws_read_file.mg2_ws_local <- function(ws, filename)
  read_file(file.path(ws$path, filename))

#' @export
ws_read_file.mg2_ws_gdrive <- function(ws, filename) {
  tmp <- .ws_gdrive_download_temp(ws, filename)
  on.exit(unlink(tmp))
  read_file(tmp)
}

#' @export
ws_read_file.mg2_ws_onedrive <- function(ws, filename)
  stop("OneDrive workspace not yet implemented.")

#' Save a data file to a workspace
#'
#' @param ws An `mg2_workspace` object.
#' @param data R object to save.
#' @param filename Filename (not a full path).
#' @return `filename` invisibly.
#' @noRd
ws_save_file <- function(ws, data, filename) UseMethod("ws_save_file")

#' @export
ws_save_file.mg2_ws_local <- function(ws, data, filename) {
  save_file(data, file.path(ws$path, filename))
  invisible(filename)
}

#' @export
ws_save_file.mg2_ws_gdrive <- function(ws, data, filename) {
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(filename)))
  save_file(data, tmp)
  on.exit(unlink(tmp))
  .ws_gdrive_upload_temp(ws, tmp, filename)
}

#' @export
ws_save_file.mg2_ws_onedrive <- function(ws, data, filename)
  stop("OneDrive workspace not yet implemented.")


# ── ws_read_rds / ws_save_rds ─────────────────────────────────────────────────

#' Read an RDS file from a workspace
#' @param ws An `mg2_workspace` object.
#' @param filename Filename (not a full path).
#' @return R object.
#' @noRd
ws_read_rds <- function(ws, filename) UseMethod("ws_read_rds")

#' @export
ws_read_rds.mg2_ws_local <- function(ws, filename)
  readRDS(file.path(ws$path, filename))

#' @export
ws_read_rds.mg2_ws_gdrive <- function(ws, filename) {
  tmp <- .ws_gdrive_download_temp(ws, filename)
  on.exit(unlink(tmp))
  readRDS(tmp)
}

#' @export
ws_read_rds.mg2_ws_onedrive <- function(ws, filename)
  stop("OneDrive workspace not yet implemented.")

#' Save an RDS file to a workspace
#' @param ws An `mg2_workspace` object.
#' @param obj R object to save.
#' @param filename Filename (not a full path).
#' @return `filename` invisibly.
#' @noRd
ws_save_rds <- function(ws, obj, filename) UseMethod("ws_save_rds")

#' @export
ws_save_rds.mg2_ws_local <- function(ws, obj, filename) {
  saveRDS(obj, file.path(ws$path, filename))
  invisible(filename)
}

#' @export
ws_save_rds.mg2_ws_gdrive <- function(ws, obj, filename) {
  tmp <- tempfile(fileext = ".rds")
  saveRDS(obj, tmp)
  on.exit(unlink(tmp))
  .ws_gdrive_upload_temp(ws, tmp, filename)
}

#' @export
ws_save_rds.mg2_ws_onedrive <- function(ws, obj, filename)
  stop("OneDrive workspace not yet implemented.")


# ── ws_read_xlsx / ws_save_xlsx ───────────────────────────────────────────────

#' Read an xlsx workbook from a workspace
#' @param ws An `mg2_workspace` object.
#' @param filename Filename (not a full path).
#' @return An `openxlsx` Workbook object.
#' @noRd
ws_read_xlsx <- function(ws, filename) UseMethod("ws_read_xlsx")

#' @export
ws_read_xlsx.mg2_ws_local <- function(ws, filename)
  openxlsx::loadWorkbook(file.path(ws$path, filename))

#' @export
ws_read_xlsx.mg2_ws_gdrive <- function(ws, filename) {
  tmp <- .ws_gdrive_download_temp(ws, filename)
  on.exit(unlink(tmp))
  openxlsx::loadWorkbook(tmp)
}

#' @export
ws_read_xlsx.mg2_ws_onedrive <- function(ws, filename)
  stop("OneDrive workspace not yet implemented.")

#' Save an xlsx workbook to a workspace
#' @param ws An `mg2_workspace` object.
#' @param wb An `openxlsx` Workbook object.
#' @param filename Filename (not a full path).
#' @return `filename` invisibly.
#' @noRd
ws_save_xlsx <- function(ws, wb, filename) UseMethod("ws_save_xlsx")

#' @export
ws_save_xlsx.mg2_ws_local <- function(ws, wb, filename) {
  openxlsx::saveWorkbook(wb, file.path(ws$path, filename), overwrite = TRUE)
  invisible(filename)
}

#' @export
ws_save_xlsx.mg2_ws_gdrive <- function(ws, wb, filename) {
  tmp <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  on.exit(unlink(tmp))
  .ws_gdrive_upload_temp(ws, tmp, filename)
}

#' @export
ws_save_xlsx.mg2_ws_onedrive <- function(ws, wb, filename)
  stop("OneDrive workspace not yet implemented.")


# ── ws_file_exists ────────────────────────────────────────────────────────────

#' Test whether a file exists in a workspace
#' @param ws An `mg2_workspace` object.
#' @param filename Filename (not a full path).
#' @return Logical.
#' @noRd
ws_file_exists <- function(ws, filename) UseMethod("ws_file_exists")

#' @export
ws_file_exists.mg2_ws_local <- function(ws, filename) {
  if (length(filename) == 0) return(FALSE)
  file.exists(file.path(ws$path, filename))
}

#' @export
ws_file_exists.mg2_ws_gdrive <- function(ws, filename) {
  if (length(filename) == 0) return(FALSE)
  # Handle vectors (e.g. ws_file_exists(ws, ff) where ff is a character vector)
  vapply(filename, function(fn) !is.null(.ws_gdrive_find(ws, fn)), logical(1))
}

#' @export
ws_file_exists.mg2_ws_onedrive <- function(ws, filename)
  stop("OneDrive workspace not yet implemented.")


# ── ws_file_info ──────────────────────────────────────────────────────────────

#' Get file metadata (mtime etc.) for a file in a workspace
#' @param ws An `mg2_workspace` object.
#' @param filename Filename or character vector of filenames.
#' @return A `file.info()` data frame.
#' @noRd
ws_file_info <- function(ws, filename) UseMethod("ws_file_info")

#' @export
ws_file_info.mg2_ws_local <- function(ws, filename)
  file.info(file.path(ws$path, filename))

#' @export
ws_file_info.mg2_ws_gdrive <- function(ws, filename) {
  files <- .ws_gdrive_ls(ws)
  idx   <- match(filename, files$name)
  found <- !is.na(idx)
  raw_mtime <- rep(NA_character_, length(filename))
  raw_mtime[found] <- vapply(
    files$drive_resource[idx[found]],
    function(r) r$modifiedTime %||% NA_character_,
    character(1)
  )
  data.frame(
    mtime = as.POSIXct(raw_mtime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    row.names = filename
  )
}

#' @export
ws_file_info.mg2_ws_onedrive <- function(ws, filename)
  stop("OneDrive workspace not yet implemented.")


# ── ws_file_rename ────────────────────────────────────────────────────────────

#' Rename a file within a workspace
#' @param ws An `mg2_workspace` object.
#' @param from Old filename.
#' @param to New filename.
#' @return Logical (TRUE on success), invisibly.
#' @noRd
ws_file_rename <- function(ws, from, to) UseMethod("ws_file_rename")

#' @export
ws_file_rename.mg2_ws_local <- function(ws, from, to)
  invisible(file.rename(file.path(ws$path, from), file.path(ws$path, to)))

#' @export
ws_file_rename.mg2_ws_gdrive <- function(ws, from, to) {
  match <- .ws_gdrive_find(ws, from)
  if (is.null(match)) stop("File not found in Google Drive workspace: ", from)
  googledrive::drive_rename(match, name = to)
  invisible(TRUE)
}

#' @export
ws_file_rename.mg2_ws_onedrive <- function(ws, from, to)
  stop("OneDrive workspace not yet implemented.")


# ── ws_path ───────────────────────────────────────────────────────────────────

#' Return the full local path for a workspace file (escape hatch)
#'
#' Use only where a genuine local path is required (e.g. passing `dir=` to
#' `api_data()`).  Will error for non-local backends.
#'
#' @param ws An `mg2_workspace` object.
#' @param filename Optional filename to append.
#' @return Character path.
#' @noRd
ws_path <- function(ws, filename = NULL) UseMethod("ws_path")

#' @export
ws_path.mg2_ws_local <- function(ws, filename = NULL) {
  # With no filename: return path with trailing slash (drop-in for data.folder()).
  # With filename: use file.path() for clean joining.
  if (is.null(filename)) paste0(ws$path, "/") else file.path(ws$path, filename)
}

#' @export
ws_path.mg2_ws_gdrive <- function(ws, filename = NULL)
  stop("ws_path() not available for Google Drive workspace. ",
       "Use ws_read_file() / ws_save_file() instead.")

#' @export
ws_path.mg2_ws_onedrive <- function(ws, filename = NULL)
  stop("ws_path() not available for OneDrive workspace. ",
       "Use ws_read_file() / ws_save_file() instead.")


# ── ws_local_copy ─────────────────────────────────────────────────────────────

#' Return a real, readable local path to a workspace file (escape hatch)
#'
#' Unlike [ws_path()], this works for every backend: local workspaces return
#' the real path directly (no copy); non-local backends download the file to
#' a session temp file first and return that path. Use this only where a
#' function insists on a genuine on-disk path (e.g. `readxl::read_excel()`,
#' or `api_data()`'s `previous_dataset_file`) and a `ws_read_*()` method
#' won't do. The returned path is read-only from the caller's perspective —
#' it is not synced back to the source; use `ws_save_*()` to persist changes.
#'
#' @param ws An `mg2_workspace` object.
#' @param filename Filename (not a full path). Required for non-local
#'   backends.
#' @return Character path to a real local file.
#' @noRd
ws_local_copy <- function(ws, filename = NULL) UseMethod("ws_local_copy")

#' @export
ws_local_copy.mg2_ws_local <- function(ws, filename = NULL) ws_path(ws, filename)

#' @export
ws_local_copy.mg2_ws_gdrive <- function(ws, filename = NULL) {
  if (is.null(filename))
    stop("ws_local_copy() requires `filename` for Google Drive workspaces.")
  .ws_gdrive_download_temp(ws, filename)
}

#' @export
ws_local_copy.mg2_ws_onedrive <- function(ws, filename = NULL)
  stop("OneDrive workspace not yet implemented.")


# ── ws_save_local_copy ────────────────────────────────────────────────────────

#' Land an already-written local file in a workspace (escape hatch)
#'
#' Inverse of [ws_local_copy()]. Use this when a file was written with a
#' function that doesn't go through `save_file()`/`ws_save_*()` (e.g.
#' `write.csv()`, or a `downloadHandler`'s temp file) -- write it to a local
#' path first, then hand that path here to land it in the connected
#' workspace: a copy for local backends, an upload for Google Drive.
#'
#' @param ws An `mg2_workspace` object.
#' @param local_path Path to the already-written local file.
#' @param filename Destination filename (not a full path).
#' @return `filename`, invisibly.
#' @noRd
ws_save_local_copy <- function(ws, local_path, filename) UseMethod("ws_save_local_copy")

#' @export
ws_save_local_copy.mg2_ws_local <- function(ws, local_path, filename) {
  file.copy(local_path, file.path(ws$path, filename), overwrite = TRUE)
  invisible(filename)
}

#' @export
ws_save_local_copy.mg2_ws_gdrive <- function(ws, local_path, filename)
  .ws_gdrive_upload_temp(ws, local_path, filename)

#' @export
ws_save_local_copy.mg2_ws_onedrive <- function(ws, local_path, filename)
  stop("OneDrive workspace not yet implemented.")


# ── ws_is_local ───────────────────────────────────────────────────────────────

#' Test whether a workspace uses local file storage
#' @param ws An `mg2_workspace` object.
#' @return Logical.
#' @noRd
ws_is_local <- function(ws) inherits(ws, "mg2_ws_local")
