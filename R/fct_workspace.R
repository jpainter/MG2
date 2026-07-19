#' MG2 workspace abstraction
#'
#' A thin S3 layer that lets the Shiny app read and write files without knowing
#' whether storage is local, Google Drive, or OneDrive.  Phase 1 implements
#' only the local backend; cloud backends are stubbed and will be added in
#' Phase 2/3.
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
#' * `ws_path(ws, filename)`         — full local path (escape hatch)
#' * `ws_is_local(ws)`               — TRUE for local backend
#'
#' @name mg2_workspace
NULL


# ── Constructor ───────────────────────────────────────────────────────────────

#' Create an MG2 workspace object
#'
#' @param path Local directory path, `"gdrive:FOLDER_ID"`, or
#'   `"onedrive:PATH"`.
#' @return An `mg2_workspace` S3 object.
#' @export
mg2_workspace <- function(path) {
  if (startsWith(path, "gdrive:")) {
    ws <- list(path = path, folder_id = sub("^gdrive:", "", path))
    class(ws) <- c("mg2_ws_gdrive", "mg2_workspace")
  } else if (startsWith(path, "onedrive:")) {
    ws <- list(path = path, folder_path = sub("^onedrive:", "", path))
    class(ws) <- c("mg2_ws_onedrive", "mg2_workspace")
  } else {
    if (!endsWith(path, "/")) path <- paste0(path, "/")
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


# ── ws_list ───────────────────────────────────────────────────────────────────

#' List files in a workspace
#'
#' @param ws An `mg2_workspace` object.
#' @param pattern Optional regex pattern passed to `grep()`.
#' @return Character vector of filenames (not full paths).
#' @export
ws_list <- function(ws, pattern = NULL) UseMethod("ws_list")

#' @export
ws_list.mg2_ws_local <- function(ws, pattern = NULL) {
  files <- list.files(ws$path)
  if (!is.null(pattern)) files <- files[grepl(pattern, files)]
  files
}

#' @export
ws_list.mg2_ws_gdrive <- function(ws, pattern = NULL)
  stop("Google Drive workspace not yet implemented. Coming in Phase 2.")

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
#' @export
ws_list_files <- function(ws, search = "All", type = "xlsx", other = "")
  UseMethod("ws_list_files")

#' @export
ws_list_files.mg2_ws_local <- function(ws, search = "All", type = "xlsx",
                                        other = "") {
  list_dir_files(search = search, type = type, other = other, dir = ws$path)
}

#' @export
ws_list_files.mg2_ws_gdrive <- function(ws, search = "All", type = "xlsx",
                                         other = "")
  stop("Google Drive workspace not yet implemented.")

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
#' @export
ws_read_file <- function(ws, filename) UseMethod("ws_read_file")

#' @export
ws_read_file.mg2_ws_local <- function(ws, filename)
  read_file(file.path(ws$path, filename))

#' @export
ws_read_file.mg2_ws_gdrive <- function(ws, filename)
  stop("Google Drive workspace not yet implemented.")

#' @export
ws_read_file.mg2_ws_onedrive <- function(ws, filename)
  stop("OneDrive workspace not yet implemented.")

#' Save a data file to a workspace
#'
#' @param ws An `mg2_workspace` object.
#' @param data R object to save.
#' @param filename Filename (not a full path).
#' @return `filename` invisibly.
#' @export
ws_save_file <- function(ws, data, filename) UseMethod("ws_save_file")

#' @export
ws_save_file.mg2_ws_local <- function(ws, data, filename) {
  save_file(data, file.path(ws$path, filename))
  invisible(filename)
}

#' @export
ws_save_file.mg2_ws_gdrive <- function(ws, data, filename)
  stop("Google Drive workspace not yet implemented.")

#' @export
ws_save_file.mg2_ws_onedrive <- function(ws, data, filename)
  stop("OneDrive workspace not yet implemented.")


# ── ws_read_rds / ws_save_rds ─────────────────────────────────────────────────

#' Read an RDS file from a workspace
#' @param ws An `mg2_workspace` object.
#' @param filename Filename (not a full path).
#' @return R object.
#' @export
ws_read_rds <- function(ws, filename) UseMethod("ws_read_rds")

#' @export
ws_read_rds.mg2_ws_local <- function(ws, filename)
  readRDS(file.path(ws$path, filename))

#' @export
ws_read_rds.mg2_ws_gdrive <- function(ws, filename)
  stop("Google Drive workspace not yet implemented.")

#' @export
ws_read_rds.mg2_ws_onedrive <- function(ws, filename)
  stop("OneDrive workspace not yet implemented.")

#' Save an RDS file to a workspace
#' @param ws An `mg2_workspace` object.
#' @param obj R object to save.
#' @param filename Filename (not a full path).
#' @return `filename` invisibly.
#' @export
ws_save_rds <- function(ws, obj, filename) UseMethod("ws_save_rds")

#' @export
ws_save_rds.mg2_ws_local <- function(ws, obj, filename) {
  saveRDS(obj, file.path(ws$path, filename))
  invisible(filename)
}

#' @export
ws_save_rds.mg2_ws_gdrive <- function(ws, obj, filename)
  stop("Google Drive workspace not yet implemented.")

#' @export
ws_save_rds.mg2_ws_onedrive <- function(ws, obj, filename)
  stop("OneDrive workspace not yet implemented.")


# ── ws_read_xlsx / ws_save_xlsx ───────────────────────────────────────────────

#' Read an xlsx workbook from a workspace
#' @param ws An `mg2_workspace` object.
#' @param filename Filename (not a full path).
#' @return An `openxlsx` Workbook object.
#' @export
ws_read_xlsx <- function(ws, filename) UseMethod("ws_read_xlsx")

#' @export
ws_read_xlsx.mg2_ws_local <- function(ws, filename)
  openxlsx::loadWorkbook(file.path(ws$path, filename))

#' @export
ws_read_xlsx.mg2_ws_gdrive <- function(ws, filename)
  stop("Google Drive workspace not yet implemented.")

#' @export
ws_read_xlsx.mg2_ws_onedrive <- function(ws, filename)
  stop("OneDrive workspace not yet implemented.")

#' Save an xlsx workbook to a workspace
#' @param ws An `mg2_workspace` object.
#' @param wb An `openxlsx` Workbook object.
#' @param filename Filename (not a full path).
#' @return `filename` invisibly.
#' @export
ws_save_xlsx <- function(ws, wb, filename) UseMethod("ws_save_xlsx")

#' @export
ws_save_xlsx.mg2_ws_local <- function(ws, wb, filename) {
  openxlsx::saveWorkbook(wb, file.path(ws$path, filename), overwrite = TRUE)
  invisible(filename)
}

#' @export
ws_save_xlsx.mg2_ws_gdrive <- function(ws, wb, filename)
  stop("Google Drive workspace not yet implemented.")

#' @export
ws_save_xlsx.mg2_ws_onedrive <- function(ws, wb, filename)
  stop("OneDrive workspace not yet implemented.")


# ── ws_file_exists ────────────────────────────────────────────────────────────

#' Test whether a file exists in a workspace
#' @param ws An `mg2_workspace` object.
#' @param filename Filename (not a full path).
#' @return Logical.
#' @export
ws_file_exists <- function(ws, filename) UseMethod("ws_file_exists")

#' @export
ws_file_exists.mg2_ws_local <- function(ws, filename)
  file.exists(file.path(ws$path, filename))

#' @export
ws_file_exists.mg2_ws_gdrive <- function(ws, filename)
  stop("Google Drive workspace not yet implemented.")

#' @export
ws_file_exists.mg2_ws_onedrive <- function(ws, filename)
  stop("OneDrive workspace not yet implemented.")


# ── ws_file_info ──────────────────────────────────────────────────────────────

#' Get file metadata (mtime etc.) for a file in a workspace
#' @param ws An `mg2_workspace` object.
#' @param filename Filename or character vector of filenames.
#' @return A `file.info()` data frame.
#' @export
ws_file_info <- function(ws, filename) UseMethod("ws_file_info")

#' @export
ws_file_info.mg2_ws_local <- function(ws, filename)
  file.info(file.path(ws$path, filename))

#' @export
ws_file_info.mg2_ws_gdrive <- function(ws, filename)
  stop("Google Drive workspace not yet implemented.")

#' @export
ws_file_info.mg2_ws_onedrive <- function(ws, filename)
  stop("OneDrive workspace not yet implemented.")


# ── ws_file_rename ────────────────────────────────────────────────────────────

#' Rename a file within a workspace
#' @param ws An `mg2_workspace` object.
#' @param from Old filename.
#' @param to New filename.
#' @return Logical (TRUE on success), invisibly.
#' @export
ws_file_rename <- function(ws, from, to) UseMethod("ws_file_rename")

#' @export
ws_file_rename.mg2_ws_local <- function(ws, from, to)
  invisible(file.rename(file.path(ws$path, from), file.path(ws$path, to)))

#' @export
ws_file_rename.mg2_ws_gdrive <- function(ws, from, to)
  stop("Google Drive workspace not yet implemented.")

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
#' @export
ws_path <- function(ws, filename = NULL) UseMethod("ws_path")

#' @export
ws_path.mg2_ws_local <- function(ws, filename = NULL) {
  if (is.null(filename)) ws$path else file.path(ws$path, filename)
}

#' @export
ws_path.mg2_ws_gdrive <- function(ws, filename = NULL)
  stop("ws_path() not available for Google Drive workspace. ",
       "Use ws_read_file() / ws_save_file() instead.")

#' @export
ws_path.mg2_ws_onedrive <- function(ws, filename = NULL)
  stop("ws_path() not available for OneDrive workspace. ",
       "Use ws_read_file() / ws_save_file() instead.")


# ── ws_is_local ───────────────────────────────────────────────────────────────

#' Test whether a workspace uses local file storage
#' @param ws An `mg2_workspace` object.
#' @return Logical.
#' @export
ws_is_local <- function(ws) inherits(ws, "mg2_ws_local")
