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
#'
#' @return The path to `dir`, invisibly.
#'
#' @examples
#' \dontrun{
#' mg2_demo_setup()              # interactive directory chooser
#' mg2_demo_setup("~/mg2_demo")  # explicit path, no prompt
#' run_mg2()                     # open the app, paste the returned path
#' }
#' @export
mg2_demo_setup <- function(dir = NULL, overwrite = FALSE) {

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
    fe <- mg2_demo_formula

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
    saveRDS(mg2_demo_meta, meta_path)
    message("Metadata saved:        ", basename(meta_path))
  } else {
    message("Metadata exists (skipped): ", basename(meta_path))
  }

  # -------------------------------------------------------------------------
  # 3. Processed dataset rds  (pre-built — no pipeline run needed)
  # -------------------------------------------------------------------------
  n_years   <- round(length(unique(mg2_demo$period)) / 12)
  data_path <- file.path(
    dir,
    paste0(formula_name, "_Facility_", n_years, "yrs_", today, ".rds")
  )

  if (!file.exists(data_path) || overwrite) {
    saveRDS(mg2_demo_processed, data_path, compress = TRUE)
    message("Dataset saved:         ", basename(data_path))
  } else {
    message("Dataset exists (skipped): ", basename(data_path))
  }

  # -------------------------------------------------------------------------
  # 4. Raw 12-month real data (one year from DHIS2 before bootstrapping)
  # -------------------------------------------------------------------------
  raw_path <- file.path(dir, paste0(formula_name, "_raw_1yr_", today, ".rds"))

  if (!file.exists(raw_path) || overwrite) {
    saveRDS(mg2_demo_raw, raw_path, compress = TRUE)
    message("Raw 1-yr data saved:   ", basename(raw_path))
  } else {
    message("Raw 1-yr data exists (skipped): ", basename(raw_path))
  }

  message(
    "\nDemo data ready. In the MG2 app, paste this path into the Directory box:\n",
    "  ", dir
  )

  invisible(dir)
}
