# DHIS2 Authentication
# Functions for connecting to and authenticating with a DHIS2 server.

#' Login to a DHIS2 Server
#'
#' Authenticates with a DHIS2 server using HTTP Basic Authentication by
#' sending a GET request to the `/api/me` endpoint.
#'
#' A `200` status alone is *not* treated as success. DHIS2 redirects
#' unauthenticated API requests to its HTML login form, which also answers
#' `200`, so a wrong password against a reachable server would otherwise look
#' like a successful login and only fail later on every subsequent API call.
#' The response must therefore be JSON *and* contain a user `id`.
#'
#' @param baseurl Character. Base URL of the DHIS2 server (e.g.
#'   `"https://play.dhis2.org/40/"`). A trailing slash is added if missing.
#'   Must include an `http://` or `https://` scheme and a host.
#' @param username Character. DHIS2 username.
#' @param password Character. DHIS2 password.
#' @param timeout Numeric. Connection timeout in seconds (default: `30`).
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisible `TRUE` on success, `FALSE` if the server rejected the
#'   credentials or did not answer with a DHIS2 user object. Errors if
#'   `baseurl` is empty or malformed.
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
  # Validate up front. Previously an empty baseurl fell through to
  # httr::GET("api/me"), which fails with an opaque curl error.
  if (!is.character(baseurl) || length(baseurl) != 1L || is.na(baseurl) ||
      !nzchar(trimws(baseurl))) {
    stop(
      "baseurl must be a single non-empty string, e.g. ",
      "\"https://play.dhis2.org/40/\".",
      call. = FALSE
    )
  }
  baseurl <- trimws(baseurl)
  if (!grepl("^https?://[^/]+", baseurl)) {
    stop(
      "baseurl must start with http:// or https:// followed by a host; got: ",
      baseurl,
      call. = FALSE
    )
  }
  if (!grepl("^https://", baseurl)) {
    warning(
      "Connecting over http:// - credentials will be sent in plain text.",
      call. = FALSE
    )
  }

  # Tolerate a missing trailing slash rather than building ".../40api/me".
  url <- paste0(sub("/?$", "/", baseurl), "api/me")

  r <- httr::GET(
    url,
    httr::authenticate(username, password),
    config = httr::config(connecttimeout = timeout)
  )

  status <- httr::status_code(r)
  if (status != 200L) {
    message("Login failed. HTTP status code: ", status)
    return(FALSE)
  }

  # A 200 does not mean authenticated: DHIS2 redirects unauthenticated API
  # requests to dhis-web-commons/security/login.action, which returns 200 with
  # an HTML body. Require JSON before believing it.
  ctype <- httr::headers(r)[["content-type"]]
  if (is.null(ctype)) ctype <- ""
  if (!grepl("application/json", tolower(ctype), fixed = TRUE)) {
    message(
      "Login failed: ", url, " returned '", ctype,
      "' instead of JSON (ended at ", r$url,
      "). Check the base URL and credentials."
    )
    return(FALSE)
  }

  # ...and that the JSON is actually an /api/me user object.
  me <- tryCatch(
    httr::content(r, as = "parsed", type = "application/json"),
    error = function(e) NULL
  )
  if (!is.list(me) || is.null(me[["id"]])) {
    message("Login failed: ", url, " did not return a DHIS2 user object.")
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
