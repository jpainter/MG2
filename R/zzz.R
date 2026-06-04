# Package-level declarations

# data.table: importing ':=' and other special symbols makes them available
# throughout the package without the "not data.table-aware" error.
# See vignette('datatable-importing').
#' @import data.table
#'
# Shiny ecosystem packages are used exclusively in inst/shiny/. One @importFrom
# per package is needed so DESCRIPTION Imports are considered "used" by R CMD check.
#' @importFrom assertthat assert_that
#' @importFrom bslib page_navbar
#' @importFrom DT datatable
#' @importFrom magrittr %>%
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinyFiles shinyDirButton
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets pickerInput
NULL
