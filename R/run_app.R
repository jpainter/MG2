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
#' if (interactive()) {
#'   run_mg2()
#'
#'   # Run on a specific port without opening a browser
#'   run_mg2(port = 4321, launch.browser = FALSE)
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

  if (requireNamespace("httpuv", quietly = TRUE)) {   # always present via shiny
    on.exit(try(httpuv::stopAllServers(), silent = TRUE), add = TRUE)
  }

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

#' Environment for evaluating the bundled Shiny app
#'
#' Returns a fresh environment whose parent holds every function in the MG2
#' namespace (exported or not), and whose grandparent is the global
#' environment. The app's widgets therefore resolve MG2 helpers by bare name
#' without MG2 exporting them, while other packages still resolve through the
#' search path exactly as before.
#'
#' The parent is deliberately *not* the namespace itself: that would put the
#' namespace's imports (all of data.table) ahead of the packages app_main.R
#' attaches, silently turning lubridate::month() / dplyr::first() etc. into
#' their data.table namesakes inside the widgets.
#' @return An environment.
#' @keywords internal
#' @noRd
.mg2_app_env <- function() {
  ns  <- asNamespace("MG2")
  fns <- new.env(parent = globalenv())
  for (nm in ls(ns, all.names = TRUE)) {
    obj <- get(nm, envir = ns, inherits = FALSE)
    if (is.function(obj)) assign(nm, obj, envir = fns)
  }
  fns$.mg2_app_state <- ns$.mg2_app_state
  new.env(parent = fns)
}
