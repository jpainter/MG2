#' Set Up Demo Data Directory
#'
#' Writes the MG2 demo datasets (Sierra Leone malaria, 5 elements, 72 months)
#' to a local directory in the format the Shiny app expects: a formula `.xlsx`,
#' a metadata `.rds`, and a processed dataset `.rds`.
#'
#' After running this function, open the app with [run_mg2()], set the
#' directory to the path returned, and skip the Login step — the demo data
#' is ready to explore from the Data tab onward.
#'
#' @param dir Path to the directory to create (or reuse). If `NULL` (default),
#'   an interactive directory chooser opens: a folder-picker dialog in
#'   RStudio/Positron, or a `readline()` prompt in the console.
#'   Pass an explicit path to skip the prompt, e.g. `dir = "~/mg2_demo"`.
#' @param overwrite Logical. If `TRUE`, overwrites existing files in `dir`
#'   (default `FALSE` — skips files that already exist).
#' @param all_levels Logical. Also write the all-levels dataset (national,
#'   district, chiefdom and facility rows; about 0.4 MB). It is downloaded from
#'   the MG2 GitHub release `demo-data` on first use and cached in `cache_dir`;
#'   if the download fails, the rest of the demo is still written.
#' @param cache_dir Where downloaded files are kept between calls.
#'
#' @return The path to `dir`, invisibly.
#'
#' @examples
#' # Offline: facility-level demo only
#' dir <- mg2_demo_setup(file.path(tempdir(), "mg2_demo"), all_levels = FALSE)
#' list.files(dir)
#'
#' if (interactive()) {
#'   mg2_demo_setup()   # choose a folder interactively
#'   run_mg2()          # open the app, paste the returned path
#' }
#' @export
mg2_demo_setup <- function(dir = NULL, overwrite = FALSE, all_levels = TRUE,
                           cache_dir = tools::R_user_dir("MG2", which = "cache")) {

  # --- resolve directory interactively if not supplied ----------------------
  if (is.null(dir)) {
    # Try the IDE folder-picker; fall back to readline() if unavailable
    dir <- tryCatch(
      if (requireNamespace("rstudioapi", quietly = TRUE) &&
          rstudioapi::isAvailable() &&
          exists("selectDirectory", envir = asNamespace("rstudioapi")))
        rstudioapi::selectDirectory(
          caption = "Choose a folder for MG2 demo data",
          label   = "Select",
          path    = path.expand("~")
        ),
      error = function(e) NULL
    )

    if (is.null(dir) || !nzchar(dir)) {
      default_path <- file.path(path.expand("~"), "mg2_demo")
      answer <- readline(
        prompt = paste0("Directory for demo data [", default_path, "]: ")
      )
      dir <- if (nzchar(trimws(answer))) trimws(answer) else default_path
    }
  }

  dir <- path.expand(dir)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message("Created directory: ", dir)
  } else {
    message("Using existing directory: ", dir)
  }

  formula_name <- "Sierra Leone Malaria Demo"
  today        <- Sys.Date()

  # -------------------------------------------------------------------------
  # 1. Formula xlsx
  # -------------------------------------------------------------------------
  xlsx_path <- file.path(dir, paste0("Formulas_SierraLeone_", format(today, "%Y_%b%d"), ".xlsx"))

  if (!file.exists(xlsx_path) || overwrite) {
    fe <- .mg2_dataset("mg2_demo_formula")

    # Build formula string (same logic as .formula_string_from_df in formula_widget)
    fe_rows <- tryCatch(
      tidyr::separate_rows(fe, Categories, categoryOptionCombo.ids, sep = ";") |>
        dplyr::mutate(
          dataElement = trimws(dataElement),
          Categories  = trimws(ifelse(is.na(Categories), "", Categories))
        ),
      error = function(e) fe
    )
    a. <- paste0("[", format(fe_rows$dataElement), "]")
    b. <- paste0("[", format(fe_rows$Categories),  "]")
    formula_str <- paste(paste(a., b., sep = "."), collapse = " + ")

    formula_sheet <- tibble::tibble(
      Formula.Name = formula_name,
      Formula      = formula_str
    )

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Formula")
    openxlsx::addWorksheet(wb, "Formula Elements")
    openxlsx::writeDataTable(wb, 1, formula_sheet,  rowNames = FALSE)
    openxlsx::writeDataTable(wb, 2, fe,             rowNames = FALSE)
    openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    message("Formula file written:  ", basename(xlsx_path))
  } else {
    message("Formula file exists (skipped): ", basename(xlsx_path))
  }

  # -------------------------------------------------------------------------
  # 2. Metadata rds
  # -------------------------------------------------------------------------
  meta_path <- file.path(dir, paste0("metadata_", today, ".rds"))

  if (!file.exists(meta_path) || overwrite) {
    # Inject district-level polygons into the metadata so the DQA map,
    # climate widget, and burden map all have geometry for Sierra Leone.
    meta_out <- .mg2_dataset("mg2_demo_meta")
    meta_out$geoFeatures <- .mg2_dataset("mg2_demo_geo")
    saveRDS(meta_out, meta_path)
    message("Metadata saved:        ", basename(meta_path))
  } else {
    message("Metadata exists (skipped): ", basename(meta_path))
  }

  # -------------------------------------------------------------------------
  # 3. Processed dataset rds  (pre-built — no pipeline run needed)
  # -------------------------------------------------------------------------
  n_years   <- round(length(unique(.mg2_dataset("mg2_demo")$period)) / 12)
  data_path <- file.path(
    dir,
    paste0(formula_name, "_Facility_", n_years, "yrs_", today, ".rds")
  )

  if (!file.exists(data_path) || overwrite) {
    saveRDS(.mg2_dataset("mg2_demo_processed"), data_path, compress = TRUE)
    message("Dataset saved:         ", basename(data_path))
  } else {
    message("Dataset exists (skipped): ", basename(data_path))
  }

  # -------------------------------------------------------------------------
  # 4. All-levels processed dataset (national / district / chiefdom / facility)
  #    Downloaded from the `demo-data` release (kept out of the package to
  #    stay under CRAN's size limit). Optional: a failed download only skips it.
  # -------------------------------------------------------------------------
  if (all_levels) {
    existing <- list.files(dir, pattern = paste0("^", formula_name, "_All-levels_"))
    if (length(existing) && !overwrite) {
      message("All-levels data exists (skipped): ", existing[1])
    } else {
      alllevels <- tryCatch(
        .mg2_demo_fetch("mg2_demo_alllevels", cache_dir),
        error = function(e) {
          message("All-levels data skipped: ", conditionMessage(e))
          NULL
        }
      )
      if (!is.null(alllevels)) {
        n_years_all <- round(length(unique(alllevels$Month)) / 12)
        alllevels_path <- file.path(
          dir, paste0(formula_name, "_All-levels_", n_years_all, "yrs_", today, ".rds")
        )
        saveRDS(alllevels, alllevels_path, compress = FALSE)
        message("All-levels data saved: ", basename(alllevels_path))
      }
    }
  }

  message(
    "\nDemo data ready. In the MG2 app, paste this path into the Directory box:\n",
    "  ", dir
  )

  invisible(dir)
}


#' Set Up PDR Lao Demo Data Directory
#'
#' Writes the PDR Lao malaria demo datasets (82 data elements, ~5 years)
#' to a local directory in the format the Shiny app expects.
#'
#' The metadata and processed dataset (about 3 MB together) are too large to
#' ship inside the package. They are downloaded from the MG2 GitHub release
#' `demo-data` the first time this function runs and cached under
#' `tools::R_user_dir("MG2", "cache")`; later calls work offline.
#'
#' @param dir Path to the directory. If `NULL`, uses an interactive chooser.
#' @param overwrite Logical. Overwrite existing files (default `FALSE`).
#' @param cache_dir Where downloaded files are kept between calls.
#' @return The path to `dir`, invisibly.
#' @examples
#' \donttest{
#' # Downloads ~3 MB; needs an internet connection
#' tmp <- tempdir()
#' dir <- tryCatch(
#'   mg2_pdrlao_setup(file.path(tmp, "mg2_pdrlao_demo"), cache_dir = tmp),
#'   error = function(e) message(conditionMessage(e))
#' )
#' }
#' @export
mg2_pdrlao_setup <- function(dir = NULL, overwrite = FALSE,
                             cache_dir = tools::R_user_dir("MG2", which = "cache")) {

  if (is.null(dir)) {
    dir <- tryCatch(
      if (requireNamespace("rstudioapi", quietly = TRUE) &&
          rstudioapi::isAvailable() &&
          exists("selectDirectory", envir = asNamespace("rstudioapi")))
        rstudioapi::selectDirectory(
          caption = "Choose a folder for PDR Lao demo data",
          label   = "Select",
          path    = path.expand("~")
        ),
      error = function(e) NULL
    )
    if (is.null(dir) || !nzchar(dir)) {
      default_path <- file.path(path.expand("~"), "mg2_pdrlao_demo")
      answer <- readline(prompt = paste0("Directory [", default_path, "]: "))
      dir <- if (nzchar(trimws(answer))) trimws(answer) else default_path
    }
  }

  dir <- path.expand(dir)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message("Created directory: ", dir)
  }

  formula_name <- "PDR Lao Malaria Demo"
  today        <- Sys.Date()

  # 1. Formula xlsx
  xlsx_path <- file.path(dir, paste0("Formulas_PDRLao_", format(today, "%Y_%b%d"), ".xlsx"))
  if (!file.exists(xlsx_path) || overwrite) {
    fe <- .mg2_dataset("mg2_pdrlao_formula")
    fe$Formula.Name <- formula_name   # rename from "malaria" to "PDR Lao Malaria Demo"
    fe_rows <- tryCatch(
      tidyr::separate_rows(fe, Categories, categoryOptionCombo.ids, sep = ";") |>
        dplyr::mutate(
          dataElement = trimws(dataElement),
          Categories  = trimws(ifelse(is.na(Categories), "", Categories))
        ),
      error = function(e) fe
    )
    a. <- paste0("[", format(fe_rows$dataElement), "]")
    b. <- paste0("[", format(fe_rows$Categories),  "]")
    formula_str <- paste(paste(a., b., sep = "."), collapse = " + ")
    formula_sheet <- tibble::tibble(Formula.Name = formula_name, Formula = formula_str)
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Formula")
    openxlsx::addWorksheet(wb, "Formula Elements")
    openxlsx::writeDataTable(wb, 1, formula_sheet, rowNames = FALSE)
    openxlsx::writeDataTable(wb, 2, fe,            rowNames = FALSE)
    openxlsx::saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    message("Formula file written:  ", basename(xlsx_path))
  } else {
    message("Formula file exists (skipped): ", basename(xlsx_path))
  }

  # 2. Metadata rds
  meta_path <- file.path(dir, paste0("metadata_", today, ".rds"))
  if (!file.exists(meta_path) || overwrite) {
    saveRDS(.mg2_demo_fetch("mg2_pdrlao_meta", cache_dir), meta_path)
    message("Metadata saved:        ", basename(meta_path))
  } else {
    message("Metadata exists (skipped): ", basename(meta_path))
  }

  # 3. Processed dataset (save_file() writes the default qs format)
  processed <- .mg2_demo_fetch("mg2_pdrlao_processed", cache_dir)
  n_months  <- length(unique(processed$Month))
  n_years   <- round(n_months / 12)
  ext       <- paste0(".", mg2_data_ext())   # e.g. ".rds"
  data_path <- file.path(dir, paste0(formula_name, "_Facility_", n_years, "yrs_", today, ext))
  if (!file.exists(data_path) || overwrite) {
    save_file(processed, data_path)
    message("Dataset saved:         ", basename(data_path))
  } else {
    message("Dataset exists (skipped): ", basename(data_path))
  }

  message(
    "\nPDR Lao demo data ready. In the MG2 app, paste this path into the Directory box:\n",
    "  ", dir
  )

  invisible(dir)
}


# -- Demo data download ---------------------------------------------------------
# Large demo objects are hosted on the GitHub release `demo-data` rather than
# shipped in data/ (CRAN tarball limit 5 MB). The MD5 pins the exact file:
# never replace these assets in place -- upload new files under new names and
# update this table instead.

.mg2_demo_assets <- c(
  mg2_pdrlao_meta      = "84c16967cd8cb7768903e1e6fff83300",
  mg2_pdrlao_processed = "b6b51a539d220f65305f06775f05bf86",
  mg2_demo_alllevels   = "a0096881e8cdf39bc2d07a7a916344b9"
)

.mg2_demo_url <- "https://github.com/jpainter/MG2/releases/download/demo-data"

#' Download (once) and read a demo object from the `demo-data` release
#' @param name One of `names(.mg2_demo_assets)`.
#' @param dir Cache directory.
#' @return The R object.
#' @keywords internal
#' @noRd
.mg2_demo_fetch <- function(name, dir = tools::R_user_dir("MG2", which = "cache")) {
  md5  <- .mg2_demo_assets[[name]]
  path <- file.path(dir, paste0(name, ".rds"))

  if (!file.exists(path) || unname(tools::md5sum(path)) != md5) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    url <- paste0(.mg2_demo_url, "/", name, ".rds")
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    message("Downloading ", name, " ...")
    old_timeout <- options(timeout = max(300, getOption("timeout")))
    on.exit(options(old_timeout), add = TRUE)
    ok <- tryCatch(
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE) == 0,
      error = function(e) FALSE, warning = function(w) FALSE
    )
    if (!ok) {
      stop("Could not download the demo data from\n  ", url,
           "\nCheck your internet connection and try again.", call. = FALSE)
    }
    if (unname(tools::md5sum(tmp)) != md5) {
      stop("Downloaded ", name, " failed its checksum; please try again.",
           call. = FALSE)
    }
    file.copy(tmp, path, overwrite = TRUE)
  }
  readRDS(path)
}


#' Fetch one of MG2's bundled datasets
#'
#' Lazy-loaded datasets are not visible by bare name from package code unless
#' MG2 is attached, so `MG2::mg2_demo_setup()` (without `library(MG2)`) used to
#' fail with "object 'mg2_demo_formula' not found".
#' @param name Dataset name, e.g. `"mg2_demo"`.
#' @return The dataset.
#' @keywords internal
#' @noRd
.mg2_dataset <- function(name) getExportedValue("MG2", name)
