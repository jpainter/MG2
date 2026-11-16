#' Google Drive OAuth — web-application redirect flow
#'
#' Implements the OAuth 2.0 authorization-code flow entirely inside the Shiny
#' app: a link sends the user's own browser to Google, Google redirects back
#' to the app's own URL with a `code`, and the server exchanges that code for
#' a token directly via HTTP. This works identically whether MG2 runs
#' locally, on shinyapps.io, or on Posit Connect Cloud — unlike
#' `googledrive::drive_auth()`'s default flow, which opens a local browser
#' and prompts in the R console, both of which assume the R process and the
#' user are on the same machine.
#'
#' Requires a **"Web application"** OAuth client in Google Cloud Console
#' (distinct from the "Desktop app" client used during early Phase 2
#' testing), with every deploy target's exact redirect URI registered.
#' `GOOGLE_CLIENT_ID` / `GOOGLE_CLIENT_SECRET` env vars hold that client's
#' credentials.
#'
#' @name mg2_gdrive_oauth
NULL

## drive.file only grants access to files the app created or that the user
## picked via Google's own Picker widget -- it can't see a folder just
## because its share link was pasted into a plain text box. Since MG2's UX is
## "paste a folder link," the broader `drive` scope is required.
.mg2_gdrive_scope <- "https://www.googleapis.com/auth/drive"

.mg2_gdrive_oauth_app <- function() {
  id     <- Sys.getenv("GOOGLE_CLIENT_ID")
  secret <- Sys.getenv("GOOGLE_CLIENT_SECRET")
  if (!nzchar(id) || !nzchar(secret)) {
    stop("GOOGLE_CLIENT_ID / GOOGLE_CLIENT_SECRET env vars are not set.")
  }
  httr::oauth_app("mg2-gdrive", key = id, secret = secret)
}


# ── CSRF state nonce (in-memory, per R process) ───────────────────────────────
#
# Deliberately NOT tied to a Shiny session: the browser does a full-page
# navigation to Google and back, which starts a *new* Shiny session on
# return. A package-level registry lets the returning request prove it
# corresponds to a state this process handed out, without needing session
# continuity. Assumes a single R process serves all sessions (the default
# for local runs, shinyapps.io, and Connect Cloud) — would not work behind a
# multi-process load balancer.

.mg2_gdrive_states <- new.env(parent = emptyenv())

#' Generate and register a CSRF state nonce
#'
#' Called once per render of the "Connect Google Drive" link, so also prunes
#' any unredeemed nonces older than 10 minutes -- bounds memory growth in a
#' long-lived hosted process where the link may render many times without
#' ever being clicked.
#'
#' @return Character nonce to embed in the authorization URL.
#' @noRd
mg2_gdrive_new_state <- function() {
  now   <- Sys.time()
  stale <- ls(.mg2_gdrive_states)
  stale <- stale[vapply(
    stale,
    function(s) difftime(now, get(s, envir = .mg2_gdrive_states, inherits = FALSE),
                          units = "mins") >= 10,
    logical(1)
  )]
  if (length(stale) > 0) rm(list = stale, envir = .mg2_gdrive_states)

  state <- paste0(
    as.integer(now), "-",
    paste(sample(c(letters, LETTERS, 0:9), 24, replace = TRUE), collapse = "")
  )
  assign(state, now, envir = .mg2_gdrive_states)
  state
}

#' Verify (and consume) a CSRF state nonce from the OAuth redirect
#' @param state The `state` query parameter Google sent back.
#' @return `TRUE` if valid and unexpired (10 min), `FALSE` otherwise.
#' @noRd
mg2_gdrive_verify_state <- function(state) {
  if (is.null(state) || !nzchar(state) ||
      !exists(state, envir = .mg2_gdrive_states, inherits = FALSE)) {
    return(FALSE)
  }
  issued <- get(state, envir = .mg2_gdrive_states, inherits = FALSE)
  rm(list = state, envir = .mg2_gdrive_states)
  difftime(Sys.time(), issued, units = "mins") < 10
}


# ── Authorization URL ─────────────────────────────────────────────────────────

#' Build the Google OAuth authorization URL
#'
#' Render this as a plain link's `href` — clicking it navigates the user's
#' own browser to Google's consent screen; no popup, no local server.
#'
#' @param redirect_uri Must exactly match a URI registered on the OAuth
#'   client (see `session$clientData` in `directory_widget.R` for how this
#'   is computed at runtime).
#' @param state CSRF nonce from `mg2_gdrive_new_state()`.
#' @return Character URL.
#' @noRd
mg2_gdrive_auth_url <- function(redirect_uri, state) {
  app <- .mg2_gdrive_oauth_app()
  httr::modify_url(
    "https://accounts.google.com/o/oauth2/v2/auth",
    query = list(
      client_id     = app$key,
      redirect_uri  = redirect_uri,
      response_type = "code",
      scope         = .mg2_gdrive_scope,
      access_type   = "offline",
      prompt        = "consent",
      state         = state
    )
  )
}


# ── Code exchange ─────────────────────────────────────────────────────────────

#' Exchange an authorization code for an access/refresh token
#'
#' @param code Authorization code from Google's redirect (`?code=...`).
#' @param redirect_uri Must exactly match the one used to obtain `code`.
#' @return An `httr::Token2.0` object, suitable for
#'   `googledrive::drive_auth(token = )` and for `mg2_workspace(..., token = )`.
#' @noRd
mg2_gdrive_exchange_code <- function(code, redirect_uri) {
  app <- .mg2_gdrive_oauth_app()

  resp <- httr::POST(
    "https://oauth2.googleapis.com/token",
    body = list(
      code          = code,
      client_id     = app$key,
      client_secret = app$secret,
      redirect_uri  = redirect_uri,
      grant_type    = "authorization_code"
    ),
    encode = "form"
  )

  if (httr::http_error(resp)) {
    stop("Google Drive token exchange failed: ",
         httr::content(resp, "text", encoding = "UTF-8"))
  }

  cred <- httr::content(resp, "parsed")

  httr::Token2.0$new(
    app         = app,
    endpoint    = httr::oauth_endpoints("google"),
    credentials = cred,
    params      = list(scope = .mg2_gdrive_scope, as_header = TRUE),
    cache_path  = FALSE
  )
}
