directory_widget_ui = function(id) {
  ns <- NS(id)

  tagList(
    hr(),

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

    h4('Or connect a cloud drive:'),

    uiOutput(ns("gdrive_connect_ui")),

    br(),

    h4('This directory Contains:'),

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

      load_dir_history <- function() {
        cfg <- tryCatch(readRDS(config_path), error = function(e) list())
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
          tryCatch(
            saveRDS(list(directories = head(updated, 10)), config_path),
            error = function(e) NULL
          )
        }
        head(updated, 10)
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
        cat('\n* data.folder:\n')
        req(input$data.directory)

        data.dir <- input$data.directory

        # Ensure path ends with a separator
        if (!endsWith(data.dir, "/") && !endsWith(data.dir, "\\")) {
          data.dir <- paste0(data.dir, "/")
        }

        cat('\n - data.folder is ', data.dir, '\n')
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
      gdrive_token     <- reactiveVal(NULL)
      gdrive_folder_id <- reactiveVal(NULL)
      gdrive_folder_nm <- reactiveVal(NULL)

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
        cat("\n* gdrive redirect observer fired\n")
        qs <- session$clientData$url_search
        cat("- url_search:", qs, "\n")
        if (!nzchar(qs)) { cat("- empty query string, skipping\n"); return() }
        params <- shiny::parseQueryString(qs)
        if (is.null(params$code) || is.null(params$state)) {
          cat("- no code/state in query string, skipping\n")
          return()
        }
        cat("- found code + state, verifying state\n")

        if (!mg2_gdrive_verify_state(params$state)) {
          cat("- state verification FAILED\n")
          showNotification(
            "Google Drive sign-in failed a security check (invalid or expired link). Please try again.",
            type = "error"
          )
          shiny::updateQueryString("?", mode = "replace", session = session)
          return()
        }
        cat("- state verified OK, exchanging code\n")

        tok <- tryCatch(
          mg2_gdrive_exchange_code(params$code, redirect_uri()),
          error = function(e) {
            cat("- code exchange FAILED:", conditionMessage(e), "\n")
            showNotification(paste("Google Drive sign-in failed:", conditionMessage(e)),
                              type = "error")
            NULL
          }
        )
        cat("- code exchange step complete, token is", if (is.null(tok)) "NULL" else "present", "\n")

        # Scrub the code from the URL/browser history either way -- a spent
        # code can't be redeemed twice, so leaving it there just risks a
        # confusing error on refresh.
        shiny::updateQueryString("?", mode = "replace", session = session)
        cat("- query string scrubbed\n")

        if (!is.null(tok)) {
          gdrive_token(tok)
          showNotification("Google Drive connected. Paste a folder link or ID below.",
                            type = "message")
        }
        cat("* gdrive redirect observer finished\n")
      }, once = TRUE, ignoreInit = FALSE)

      output$gdrive_connect_ui <- renderUI({
        if (is.null(gdrive_token())) {
          state <- mg2_gdrive_new_state()
          tags$a(
            href  = mg2_gdrive_auth_url(redirect_uri(), state),
            class = "btn btn-outline-primary",
            style = "width: 100%; display: inline-block; text-align: center;",
            icon("cloud"), " Connect Google Drive"
          )
        } else if (is.null(gdrive_folder_nm())) {
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
          tagList(
            div(icon("check-circle"), paste0(" Using Drive folder: ", gdrive_folder_nm())),
            actionLink(ns("gdrive_disconnect"), "Disconnect / use local directory instead")
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
        showNotification(paste("Using Google Drive folder:", info$name[[1]]), type = "message")
      })

      observeEvent(input$gdrive_disconnect, {
        gdrive_token(NULL)
        gdrive_folder_id(NULL)
        gdrive_folder_nm(NULL)
      })

      # Workspace object — wraps data.folder() (or a connected Google Drive
      # folder + its session token) in an mg2_workspace S3 object. Consumers
      # can use ws_*() methods instead of building paths manually. data.folder
      # kept for backward compat during migration.
      workspace <- reactive({
        if (!is.null(gdrive_folder_id())) {
          return(mg2_workspace(paste0("gdrive:", gdrive_folder_id()), token = gdrive_token()))
        }
        req(data.folder())
        mg2_workspace(data.folder())
      })

      return(list(
        workspace = workspace,
        directory = data.folder,
        metadata.files = metadata.files,
        geofeatures.files = geofeatures.files
      ))
    }
  )
}
