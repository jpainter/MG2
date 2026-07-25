directory_widget_ui = function(id) {
  ns <- NS(id)

  tagList(
    hr(),

    # Local directory picker — hidden when a Drive folder is connected
    div(
      id = ns("local_dir_section"),
      h3('Step 1.  Provide directory for data files:'),
      fluidRow(
        column(
          12,
          selectizeInput(
            ns("data.directory"),
            label   = NULL,
            choices = NULL,
            width   = "100%",
            options = list(
              create       = TRUE,
              createOnBlur = TRUE,
              placeholder  = "Select a recent directory or paste/type a path..."
            )
          )
        )
      ),
      fluidRow(
        column(
          3,
          shinyFiles::shinyDirButton(
            ns("folder"),
            label     = "Browse...",
            title     = "Select data directory",
            icon      = icon("folder-open"),
            style     = "width: 100%"
          )
        ),
      ),
      br(),
      h4('Or connect a cloud drive:')
    ),

    uiOutput(ns("gdrive_connect_ui")),

    br(),

    uiOutput(ns("folder_contents_heading")),

    tableOutput(ns('folderInfo')),

    # hr()
  ) # end tagList
} # ui


directory_widget_server <- function(id, demo_dir = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      config_path <- path.expand("~/.mg2_config.rds")

      # Load directory history (up to 10); support both old single-dir and new list format
      .is_temp_dir <- function(path) {
        tryCatch(
          startsWith(normalizePath(path, mustWork = FALSE),
                     normalizePath(tempdir(), mustWork = FALSE)),
          error = function(e) FALSE
        )
      }

      load_config <- function() {
        tryCatch(readRDS(config_path), error = function(e) list())
      }

      load_dir_history <- function() {
        cfg <- load_config()
        dirs <- if (!is.null(cfg$directories)) cfg$directories else if (!is.null(cfg$directory)) cfg$directory else character(0)
        dirs[nchar(dirs) > 0 & dir.exists(dirs) & !vapply(dirs, .is_temp_dir, logical(1))]
      }

      save_dir_history <- function(new_dir, existing) {
        # Never persist temp directories (e.g. URL-load sessions)
        if (.is_temp_dir(new_dir)) return(existing)
        # Normalize: strip trailing slash before storing
        new_dir  <- sub("/$", "", new_dir)
        existing <- sub("/$", "", existing)
        updated  <- unique(c(new_dir, existing[nchar(existing) > 0]))
        # Skip disk write in demo mode (server filesystem is ephemeral)
        if (!nzchar(Sys.getenv("MG2_DEMO_MODE"))) {
          tryCatch({
            cfg <- load_config()
            cfg$directories <- head(updated, 10)
            saveRDS(cfg, config_path)
          }, error = function(e) NULL)
        }
        head(updated, 10)
      }

      # Persist/restore Google Drive folder ID (not the token — tokens are
      # always session-only). Lets the user re-auth with one click instead of
      # re-pasting the folder link every session.
      load_gdrive_folder_id <- function() {
        if (nzchar(Sys.getenv("MG2_DEMO_MODE"))) return(NULL)
        cfg <- load_config()
        fid <- cfg$gdrive_folder_id
        if (!is.null(fid) && nzchar(fid)) fid else NULL
      }

      save_gdrive_folder <- function(folder_id, folder_name) {
        if (nzchar(Sys.getenv("MG2_DEMO_MODE"))) return(invisible(NULL))
        tryCatch({
          cfg <- load_config()
          cfg$gdrive_folder_id   <- folder_id
          cfg$gdrive_folder_name <- folder_name
          saveRDS(cfg, config_path)
        }, error = function(e) NULL)
      }

      clear_gdrive_folder <- function() {
        if (nzchar(Sys.getenv("MG2_DEMO_MODE"))) return(invisible(NULL))
        tryCatch({
          cfg <- load_config()
          cfg$gdrive_folder_id   <- NULL
          cfg$gdrive_folder_name <- NULL
          saveRDS(cfg, config_path)
        }, error = function(e) NULL)
      }

      dir_history <- reactiveVal(load_dir_history())

      # Populate selectize with history on startup
      isolate({
        dirs <- dir_history()
        updateSelectizeInput(session, "data.directory",
          choices  = dirs,
          selected = if (length(dirs) > 0) dirs[1] else NULL
        )
      })

      # Volumes available to the folder picker — use only known-accessible roots
      # to avoid macOS Full Disk Access blocks from shinyFiles::getVolumes()
      volumes <- c(Home = path.expand("~"))

      icloud <- file.path(path.expand("~"), "Library/Mobile Documents/com~apple~CloudDocs")
      if (dir.exists(icloud)) volumes <- c("iCloud Drive" = icloud, volumes)

      onedrive <- file.path(path.expand("~"), "OneDrive")
      if (dir.exists(onedrive)) volumes <- c("OneDrive" = onedrive, volumes)

      shinyFiles::shinyDirChoose(input, "folder", roots = volumes, session = session)

      # When user picks a folder via Browse, add to selectize and select it
      observeEvent(input$folder, {
        if (!is.integer(input$folder)) {
          path <- shinyFiles::parseDirPath(volumes, input$folder)
          if (length(path) > 0 && nchar(path) > 0) {
            cat('\n directory_widget - folder chosen:', path, '\n')
            new_choices <- unique(c(path, dir_history()))
            updateSelectizeInput(session, "data.directory",
              choices  = new_choices,
              selected = path
            )
          }
        }
      })


      # Watch for demo_dir signal from login_widget ####
      observeEvent(demo_dir(), {
        req(!is.null(demo_dir))
        d <- demo_dir()
        if (!is.null(d) && nzchar(d)) {
          new_choices <- unique(c(d, dir_history()))
          updateSelectizeInput(session, "data.directory",
            choices  = new_choices,
            selected = d
          )
        }
      }, ignoreNULL = TRUE, ignoreInit = FALSE)

      data.folder = reactive({
        req(input$data.directory)

        data.dir <- input$data.directory

        # Ensure path ends with a separator
        if (!endsWith(data.dir, "/") && !endsWith(data.dir, "\\")) {
          data.dir <- paste0(data.dir, "/")
        }

        return(data.dir)
      })

      # Persist chosen directory and update history
      observeEvent(data.folder(), {
        new_history <- save_dir_history(data.folder(), dir_history())
        dir_history(new_history)
      })

      data.dir.files = reactive({
        ws <- workspace()
        req(ws)
        if (ws_is_local(ws) && !dir.exists(ws_path(ws))) return(NULL)
        ws_list(ws)
      })

      folderInfo = reactive({
        ws <- workspace()
        req(ws)

        data.dir.files = ws_list(ws)

        if (length(data.dir.files) == 0) {
          return(tibble(`File type:` = c("Metadata", "Formula", "Data"),
                        Number = 0L, `Most Recent` = "—"))
        }

        finf = ws_file_info(ws, data.dir.files)

        metadata.files = grepl("metadata", data.dir.files, ignore.case = TRUE)
        formula.files = grepl("formulas", data.dir.files, ignore.case = TRUE)
        data.files = grepl("rds", data.dir.files, ignore.case = TRUE) &
          !grepl("metadata", data.dir.files, ignore.case = TRUE)

        max_date <- function(times) {
          if (length(times) == 0 || all(is.na(times))) return("—")
          as.character(date(max(times, na.rm = TRUE)))
        }

        info = tibble(
          `File type:` = c("Metadata", 'Formula', 'Data'),
          Number = c(sum(metadata.files), sum(formula.files), sum(data.files)),
          `Most Recent` = c(
            max_date(finf$mtime[metadata.files]),
            max_date(finf$mtime[formula.files]),
            max_date(finf$mtime[data.files])
          )
        )

        return(info)
      })

      output$folderInfo = renderTable(folderInfo())

      metadata.files = reactive({
        ws <- workspace()
        req(ws)
        mf <- ws_list_files(ws, search = "metadata", type = "rds")
        if (length(mf) == 0) return(NULL)
        mf
      })

      geofeatures.files = reactive({
        ws <- workspace()
        req(ws)
        gf <- ws_list_files(ws, search = "geofeatures", type = "rds")
        if (length(gf) == 0) return(NULL)
        gf
      })

      # Update list of data files
      observe({
        cat('\n directory_widget -updating metadata file list \n')
        updateSelectInput(
          session,
          'metadataFiles',
          choices = metadata.files(),
          selected = 1
        )
      })

      observe({
        cat('\n directory_widget -updating geofeatures file list \n')
        updateSelectInput(
          session,
          'geofeturesFiles',
          choices = geofeatures.files()
        )
      })

      # ── Google Drive connection (Phase 2) ────────────────────────────────
      # Redirect-based web-application OAuth flow (R/fct_gdrive_oauth.R) --
      # not googledrive::drive_auth(), whose default flow assumes a local
      # browser + R console on the same machine and breaks on hosted deploys
      # (shinyapps.io / Connect Cloud). The token lives only in this
      # session's reactiveVal -- never written to disk.
      # Folder ID and name are restored from config on startup so the user can
      # reconnect with one click instead of re-pasting the folder link.
      gdrive_token     <- reactiveVal(NULL)
      gdrive_folder_id <- reactiveVal(load_gdrive_folder_id())
      gdrive_folder_nm <- reactiveVal({
        cfg <- load_config()
        if (!is.null(cfg$gdrive_folder_name) && nzchar(cfg$gdrive_folder_name))
          cfg$gdrive_folder_name
        else
          NULL
      })

      .parse_gdrive_folder_id <- function(x) {
        x <- trimws(x)
        m <- regmatches(x, regexpr("(?<=folders/)[A-Za-z0-9_-]+", x, perl = TRUE))
        if (length(m) > 0 && nzchar(m)) return(m)
        m <- regmatches(x, regexpr("(?<=[?&]id=)[A-Za-z0-9_-]+", x, perl = TRUE))
        if (length(m) > 0 && nzchar(m)) return(m)
        x  # assume a bare folder ID was pasted
      }

      # Must match exactly what's registered on the OAuth client. Computed
      # from this session's own URL so it's correct for local dev, shinyapps.io,
      # and Connect Cloud alike -- as long as each target's real URL is
      # registered as an authorized redirect URI in Google Cloud Console.
      redirect_uri <- reactive({
        cd <- session$clientData
        paste0(
          cd$url_protocol, "//", cd$url_hostname,
          if (nzchar(cd$url_port)) paste0(":", cd$url_port) else "",
          cd$url_pathname
        )
      })

      # Handle the redirect back from Google (?code=...&state=...). Runs once
      # per session load; the browser did a full-page navigation to get here,
      # so this is a brand-new Shiny session -- mg2_gdrive_verify_state()
      # validates the state nonce without needing session continuity.
      observeEvent(session$clientData$url_search, {
        qs <- session$clientData$url_search
        if (!nzchar(qs)) return()
        params <- shiny::parseQueryString(qs)
        if (is.null(params$code) || is.null(params$state)) return()

        if (!mg2_gdrive_verify_state(params$state)) {
          showNotification(
            "Google Drive sign-in failed a security check (invalid or expired link). Please try again.",
            type = "error"
          )
          shiny::updateQueryString("?", mode = "replace", session = session)
          return()
        }

        tok <- tryCatch(
          mg2_gdrive_exchange_code(params$code, redirect_uri()),
          error = function(e) {
            showNotification(paste("Google Drive sign-in failed:", conditionMessage(e)),
                              type = "error")
            NULL
          }
        )

        # Scrub the code from the URL/browser history either way -- a spent
        # code can't be redeemed twice, so leaving it there just risks a
        # confusing error on refresh.
        shiny::updateQueryString("?", mode = "replace", session = session)

        if (!is.null(tok)) {
          gdrive_token(tok)
          # If no folder is saved, prompt the user to paste one.
          # If a folder IS saved, observeEvent(gdrive_token()) above will
          # auto-verify it and set gdrive_folder_nm().
          if (is.null(gdrive_folder_id())) {
            showNotification("Google Drive connected. Paste a folder link or ID below.",
                              type = "message")
          }
        }
      }, once = TRUE, ignoreInit = FALSE)

      output$gdrive_connect_ui <- renderUI({
        fid <- gdrive_folder_id()
        fnm <- gdrive_folder_nm()
        tok <- gdrive_token()

        if (!is.null(fid) && !is.null(fnm) && !is.null(tok)) {
          # Fully connected (token + folder)
          tagList(
            div(icon("check-circle"), paste0(" Using Drive folder: ", fnm)),
            actionLink(ns("gdrive_disconnect"), "Disconnect / use local directory instead")
          )
        } else if (!is.null(fid) && !is.null(fnm) && is.null(tok)) {
          # Folder saved from a previous session but token expired — one-click reconnect
          state <- mg2_gdrive_new_state()
          tagList(
            div(
              style = "margin-bottom: 6px; color: #555;",
              icon("folder"), paste0(" Previous folder: ", fnm)
            ),
            tags$a(
              href  = mg2_gdrive_auth_url(redirect_uri(), state),
              class = "btn btn-outline-primary",
              style = "width: 100%; display: inline-block; text-align: center;",
              icon("rotate-right"), " Reconnect Google Drive"
            ),
            actionLink(ns("gdrive_disconnect"), "Clear saved folder")
          )
        } else if (!is.null(tok) && is.null(fnm)) {
          # Authenticated but no folder chosen yet
          fluidRow(
            column(
              8,
              textInput(
                ns("gdrive_folder_input"),
                label = NULL,
                placeholder = "Paste a Google Drive folder link or folder ID..."
              )
            ),
            column(
              4,
              actionButton(ns("gdrive_set_folder"), "Use this folder", class = "btn-primary")
            )
          )
        } else {
          # Not connected at all
          state <- mg2_gdrive_new_state()
          tags$a(
            href  = mg2_gdrive_auth_url(redirect_uri(), state),
            class = "btn btn-outline-primary",
            style = "width: 100%; display: inline-block; text-align: center;",
            icon("cloud"), " Connect Google Drive"
          )
        }
      })

      observeEvent(input$gdrive_set_folder, {
        req(input$gdrive_folder_input, gdrive_token())
        fid <- .parse_gdrive_folder_id(input$gdrive_folder_input)

        googledrive::drive_auth(token = gdrive_token())
        info <- tryCatch(googledrive::drive_get(googledrive::as_id(fid)), error = function(e) NULL)

        if (is.null(info) || nrow(info) == 0) {
          showNotification(
            "Could not find that Google Drive folder. Check the link/ID and sharing permissions.",
            type = "error"
          )
          return()
        }

        is_folder <- identical(info$drive_resource[[1]]$mimeType,
                                "application/vnd.google-apps.folder")
        if (!is_folder) {
          showNotification("That points to a file, not a folder. Paste a folder link instead.",
                            type = "error")
          return()
        }

        gdrive_folder_id(fid)
        gdrive_folder_nm(info$name[[1]])
        save_gdrive_folder(fid, info$name[[1]])
        showNotification(paste("Using Google Drive folder:", info$name[[1]]), type = "message")
      })

      # After OAuth redirect: if a folder ID was already saved, auto-verify it
      # with the fresh token so the user doesn't need to paste the link again.
      observeEvent(gdrive_token(), {
        req(gdrive_token())
        fid <- gdrive_folder_id()
        if (is.null(fid) || !is.null(gdrive_folder_nm())) return()

        cat("* gdrive: token arrived, verifying saved folder_id", fid, "\n")
        googledrive::drive_auth(token = gdrive_token())
        info <- tryCatch(googledrive::drive_get(googledrive::as_id(fid)), error = function(e) NULL)
        if (!is.null(info) && nrow(info) > 0) {
          gdrive_folder_nm(info$name[[1]])
          showNotification(paste("Reconnected to Drive folder:", info$name[[1]]), type = "message")
        } else {
          cat("* gdrive: saved folder not accessible, clearing\n")
          showNotification(
            "Could not access the previously connected Drive folder. Please reconnect.",
            type = "warning"
          )
          gdrive_folder_id(NULL)
          gdrive_folder_nm(NULL)
          clear_gdrive_folder()
        }
      })

      observeEvent(input$gdrive_disconnect, {
        gdrive_token(NULL)
        gdrive_folder_id(NULL)
        gdrive_folder_nm(NULL)
        clear_gdrive_folder()
      })

      # Hide the local directory picker only when fully connected to Drive
      # (both folder ID and token present). A saved folder ID without a token
      # means "reconnect pending" — the user is still in local mode.
      # shinyjs auto-prepends the module namespace, so pass the bare id (no ns()).
      observe({
        drive_active <- !is.null(gdrive_folder_id()) && !is.null(gdrive_token())
        if (drive_active) {
          shinyjs::hide("local_dir_section")
        } else {
          shinyjs::show("local_dir_section")
        }
      })

      # Dynamic heading: "This Drive folder contains" vs "This directory contains"
      output$folder_contents_heading <- renderUI({
        drive_active <- !is.null(gdrive_folder_id()) && !is.null(gdrive_token())
        if (drive_active) {
          h4(paste0("Drive folder \u201c", gdrive_folder_nm(), "\u201d contains:"))
        } else {
          h4("This directory contains:")
        }
      })

      # Workspace object — wraps data.folder() (or a connected Google Drive
      # folder + its session token) in an mg2_workspace S3 object. Consumers
      # can use ws_*() methods instead of building paths manually. data.folder
      # kept for backward compat during migration.
      workspace <- reactive({
        fid <- gdrive_folder_id()
        tok <- gdrive_token()
        # Only switch to gdrive backend when authenticated — a saved folder ID
        # without a token (session just started, not yet reconnected) falls
        # through to the local workspace so the app remains usable.
        if (!is.null(fid) && !is.null(tok)) {
          return(mg2_workspace(paste0("gdrive:", fid), token = tok))
        }
        req(data.folder())
        mg2_workspace(data.folder())
      })

      return(list(
        workspace = workspace,
        directory = data.folder,
        metadata.files = metadata.files,
        geofeatures.files = geofeatures.files,
        drive_connected = reactive({ !is.null(gdrive_token()) && !is.null(gdrive_folder_id()) })
      ))
    }
  )
}
