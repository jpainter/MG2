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

  args <- utils::modifyList(
    list(appDir = app_dir, display.mode = "normal", launch.browser = TRUE, port = 8100L),
    list(...)
  )

  tryCatch(
    do.call(shiny::runApp, args),
    interrupt = function(e) message("App stopped.")
  )
}

# State that must survive across run_mg2() calls in one R session (the app's
# pending-stop timer handle and live session count). Each run_mg2() sources
# inst/shiny/app.R into a fresh environment, so this lives in the package
# namespace instead -- CRAN policy forbids writing to .GlobalEnv. The
# environment object is fixed at load time; only its contents change.
.mg2_app_state <- new.env(parent = emptyenv())
.mg2_app_state$pending_stop  <- NULL
.mg2_app_state$session_count <- 0L
