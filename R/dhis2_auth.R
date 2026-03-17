# DHIS2 Authentication
# Functions for connecting to and authenticating with a DHIS2 server.

#' Login to a DHIS2 Server
#'
#' Authenticates with a DHIS2 server using HTTP Basic Authentication by
#' sending a GET request to the `/api/me` endpoint. Returns `TRUE` invisibly
#' on success; stops with an error if authentication fails.
#'
#' @param baseurl Character. Base URL of the DHIS2 server, including trailing
#'   slash (e.g., `"https://play.dhis2.org/2.39/"`).
#' @param username Character. DHIS2 username.
#' @param password Character. DHIS2 password.
#' @param timeout Numeric. Connection timeout in seconds (default: `30`).
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisible `TRUE` on success.
#' @export
#'
#' @examples
#' \dontrun{
#'   loginDHIS2(
#'     baseurl  = "https://play.dhis2.org/2.39/",
#'     username = "admin",
#'     password = "district"
#'   )
#' }
loginDHIS2 <- function(baseurl, username, password, timeout = 30, ...) {
  url <- paste0(baseurl, "api/me")

  r <- httr::GET(
    url,
    httr::authenticate(username, password),
    config = httr::config(connecttimeout = timeout)
  )

  if (r$status_code != 200L) {
    message("Login failed. HTTP status code: ", r$status_code)
    return(FALSE)
  }

  invisible(TRUE)
}

#' Retry a Failed Expression
#'
#' Evaluates `expr` and retries up to `maxErrors` times if the result is an
#' error. Useful for intermittent network failures when querying DHIS2.
#'
#' @param expr An R expression to evaluate.
#' @param isError Function that returns `TRUE` when the result should be
#'   retried. Default: checks for class `"try-error"`.
#' @param maxErrors Integer. Maximum number of retry attempts (default: `5`).
#' @param sleep Numeric. Seconds to wait between retries (default: `1`).
#'
#' @return The value of `expr` on success.
#' @noRd
retry <- function(
  expr,
  isError = function(x) "try-error" %in% class(x),
  maxErrors = 5,
  sleep = 1
) {
  attempts <- 1
  retval <- try(eval(expr))

  while (isError(retval)) {
    message("Request error on attempt ", attempts, " of ", maxErrors)

    if (attempts >= maxErrors) {
      stop("retry: too many failed attempts (", maxErrors, ")")
    }

    if (sleep > 0) Sys.sleep(sleep)
    attempts <- attempts + 1
    retval <- try(eval(expr))
  }

  return(retval)
}
