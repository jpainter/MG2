#' Path to the MG2 demo data directory
#'
#' Returns the path to the bundled demo dataset directory inside the installed
#' package.  The directory contains three files that mirror a real MG2 data
#' folder and can be pointed at directly from the Shiny app's Setup tab or
#' used in standalone R scripts via [read_file()].
#'
#' **Contents of the demo directory**
#'
#' | File | Description |
#' |---|---|
#' | `malaria_demo.rds` | 6-year Lao PDR malaria surveillance data (Aug 2021 – May 2026), 18 columns, ~867 k facility-month rows |
#' | `metadata_demo.rds` | DHIS2 metadata: org unit tree, geographic features (sf), validation rules, data element dictionary |
#' | `Formulas_demo.xlsx` | Formula file listing 67 malaria data elements across 18 categories |
#'
#' **Data source**
#'
#' Data were downloaded from the public DHIS2 demonstration server at
#' <https://demos.dhis2.org/hmis_data> (Lao PDR national malaria programme
#' configuration).  This is demonstration data intended for training and
#' software testing; it is not the official data of the Lao Ministry of Health.
#'
#' **Columns in `malaria_demo.rds`**
#'
#' | Column | Type | Description |
#' |---|---|---|
#' | `orgUnit` | character | DHIS2 org unit UID |
#' | `orgUnitName` | character | Facility / district / province name |
#' | `level` | integer | 2 = province, 3 = district, 4 = facility |
#' | `Country`, `Province`, `District`, `Facility` | character | Geographic hierarchy labels |
#' | `effectiveLeaf` | logical | `TRUE` for the lowest level that reported data |
#' | `Month` | yearmonth | Reporting month (tsibble `yearmonth` class) |
#' | `data` | character | Data element name (human-readable, from formula) |
#' | `original` | numeric | Raw reported value from DHIS2 |
#' | `key_entry_error` | logical | Flagged as mobile-phone key-entry error |
#' | `over_max` | logical | Exceeds theoretical maximum (e.g. > 31/month) |
#' | `mad15` | logical | > 15 × median absolute deviation |
#' | `mad10` | logical | > 10 × MAD (after mad15 removed) |
#' | `mad5` | logical | > 5 × MAD (after mad10 removed) |
#' | `seasonal5` | logical | > 5 × seasonal decomposition residual / MAD |
#' | `seasonal3` | logical | > 3 × seasonal decomposition residual / MAD |
#'
#' @return Character scalar — absolute path to `inst/extdata/demo/` in the
#'   installed package.
#'
#' @examples
#' demo <- mg2_demo_dir()
#' list.files(demo)
#'
#' # Load the processed data
#' d <- read_file(file.path(demo, "malaria_demo.rds"))
#'
#' # Load the metadata
#' meta <- readRDS(file.path(demo, "metadata_demo.rds"))
#' names(meta)
#'
#' @export
mg2_demo_dir <- function() {
  system.file("extdata", "demo", package = "MG2", mustWork = TRUE)
}
