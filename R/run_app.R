#' Launch the Magic Glasses 2 Shiny Application
#'
#' Opens the Magic Glasses 2 interactive Shiny application for exploring and
#' analyzing DHIS2 routine health data. The application guides users through
#' data quality assessment, reporting bias analysis, outlier detection, and
#' time-series trend evaluation.
#'
#' @param ... Additional arguments passed to [shiny::runApp()], such as
#'   `port`, `launch.browser`, or `display.mode`.
#'
#' @return Called for its side effect of launching the Shiny app.
#'   Invisible `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#'   MG2::run_mg2()
#'
#'   # Run on a specific port without opening a browser
#'   MG2::run_mg2(port = 4321, launch.browser = FALSE)
#' }
run_mg2 <- function(...) {
  app_dir <- system.file("shiny", package = "MG2")

  if (app_dir == "") {
    stop(
      "Could not find the MG2 Shiny app directory. ",
      "Try reinstalling MG2 with: devtools::install_github('your-repo/MG2')",
      call. = FALSE
    )
  }

  on.exit(try(httpuv::stopAllServers(), silent = TRUE), add = TRUE)

  tryCatch(
    shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE, ...),
    interrupt = function(e) message("App stopped.")
  )
}
