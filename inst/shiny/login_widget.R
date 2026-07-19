login_widget_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinybusy::add_busy_spinner(
      spin     = "fading-circle",
      position = 'bottom-right'
    ),

    fluidPage(
      hr(),

      h3('Step 2. Login to DHIS2 server'),

      tags$p(
        tags$small(
          "Once data has been downloaded, the app works fully offline without a server connection."
        ),
        style = "color: #666; margin-bottom: 12px;"
      ),

      # Saved credentials dropdown (shown when directory has saved credentials)
      uiOutput(ns("saved_creds_ui")),

      h4('Enter address and credentials:'),

      inputPanel(
        textInput(ns("baseurl"),  label = "DHIS2 URL:",  NULL),
        textInput(ns("username"), label = "User name:",  NULL),
        passwordInput(ns("password"), label = "Password:", NULL)
      ),

      fluidRow(
        column(4, uiOutput(ns("status_ui"))),
        column(4, uiOutput(ns("save_creds_ui")))
      ),

      fluidRow(
        column(12, tableOutput(ns('systemInfo')))
      ),

      fluidRow(
        column(
          6,
          hr(),
          h4("Or connect to a DHIS2 demo instance:"),
          actionButton(
            ns("demo_picker"),
            label = "Choose Demo Instance",
            icon  = icon("globe"),
            class = "btn-info"
          )
        ),
        column(
          6,
          hr(),
          if (nzchar(Sys.getenv("MG2_DEMO_MODE"))) {
            # Demo mode: use actionButtons with fixed server-side temp paths.
            # shinyDirButton cannot browse a server filesystem; actionButtons
            # call the setup functions directly — no file picker needed.
            tagList(
              h4("Demo data:"),
              tags$p(
                tags$small("Pre-built datasets — no server connection needed."),
                style = "color: #555; margin-bottom: 8px;"
              ),
              fluidRow(
                column(
                  6,
                  actionButton(
                    ns("load_sl_demo"),
                    label = "Sierra Leone Malaria",
                    icon  = icon("play-circle"),
                    class = "btn-success btn-block"
                  ),
                  tags$p(tags$small("5 elements, 6 years"),
                         style = "color:#555; margin-top:4px;")
                ),
                column(
                  6,
                  actionButton(
                    ns("load_pdrlao_demo"),
                    label = "PDR Lao Malaria",
                    icon  = icon("play-circle"),
                    class = "btn-info btn-block"
                  ),
                  tags$p(tags$small("22 elements, ~5 years"),
                         style = "color:#555; margin-top:4px;")
                )
              )
            )
          } else {
            tagList(
              h4("Or load demo data:"),
              tags$p(
                tags$small("Pre-built datasets — no server connection needed."),
                style = "color: #555; margin-bottom: 8px;"
              ),
              fluidRow(
                column(
                  6,
                  shinyFiles::shinyDirButton(
                    ns("demo_folder"),
                    label = "Sierra Leone Malaria",
                    title = "Choose folder for Sierra Leone demo data",
                    icon  = icon("play-circle"),
                    class = "btn-success btn-block"
                  ),
                  tags$p(tags$small("5 elements, 6 years"),
                         style = "color:#555; margin-top:4px;")
                ),
                column(
                  6,
                  shinyFiles::shinyDirButton(
                    ns("demo_pdrlao_folder"),
                    label = "PDR Lao Malaria",
                    title = "Choose folder for PDR Lao demo data",
                    icon  = icon("play-circle"),
                    class = "btn-info btn-block"
                  ),
                  tags$p(tags$small("22 elements, ~5 years"),
                         style = "color:#555; margin-top:4px;")
                )
              )
            )
          }
        )
      ),

      # ── Load from shared URLs ────────────────────────────────────────────────
      fluidRow(
        column(
          12,
          hr(),
          h4("Or load from shared links:"),
          tags$p(
            tags$small(
              "Paste direct download links from Google Drive, Dropbox, or OneDrive.",
              "Formula and metadata links are optional but enable mapping and formula features."
            ),
            style = "color:#555; margin-bottom:8px;"
          ),
          fluidRow(
            column(6,
              textInput(ns("url_data"),    "Data file (.qs or .rds):",
                        placeholder = "https://drive.google.com/file/d/…"),
              textInput(ns("url_formula"), "Formula file (.xlsx, optional):",
                        placeholder = "drive.google.com/file/d/… or docs.google.com/spreadsheets/…"),
              textInput(ns("url_meta"),    "Metadata file (.rds, optional):",
                        placeholder = "https://drive.google.com/file/d/…")
            ),
            column(6,
              br(),
              actionButton(ns("load_from_url"), "Download & Load",
                           icon  = icon("cloud-download-alt"),
                           class = "btn-primary"),
              br(), br(),
              tags$p(tags$small(tags$strong("Google Drive:"),
                " right-click file → Share → 'Anyone with link' → Copy link"),
                style = "color:#666;"),
              tags$p(tags$small(tags$strong("Dropbox:"),
                " Share → Copy link (dl=0 is converted automatically)"),
                style = "color:#666;"),
              tags$p(tags$small(tags$strong("OneDrive:"),
                " Share → Copy link → Paste here"),
                style = "color:#666;")
            )
          )
        )
      )
    )
  )
} # ui

# Server function ####
login_widget_server <- function(id, directory_widget_output = NULL, demo_dir = NULL, nav_goto = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      data.folder = reactive({
        directory_widget_output$directory()
      })

      add_busy_spinner(spin = "fading-circle", position = "top-right")

      # State ----
      login         <- reactiveVal(FALSE)
      loginAttempted <- reactiveVal(FALSE)
      loginError    <- reactiveVal(FALSE)
      loginTrigger  <- reactiveVal(0L)  # increment to force a login attempt

      # Saved credentials ----

      cred_file <- reactive({
        req(data.folder())
        file.path(data.folder(), "credentials.rds")
      })

      saved_credentials <- reactive({
        req(cred_file())
        if (file.exists(cred_file())) {
          tryCatch({
            creds <- readRDS(cred_file())
            # Drop password column if present (migration from earlier format)
            creds[, setdiff(names(creds), "password"), drop = FALSE]
          }, error = function(e) NULL)
        } else {
          NULL
        }
      })

      # Dropdown UI: only shown when saved credentials exist for this directory
      output$saved_creds_ui <- renderUI({
        creds <- saved_credentials()
        if (is.null(creds) || nrow(creds) == 0) return(NULL)

        # Build display labels: "username" or "username (domain)" if duplicates
        hosts  <- sub("^https?://([^/]+).*", "\\1", creds$url)
        labels <- if (anyDuplicated(creds$username)) {
          paste0(creds$username, " (", hosts, ")")
        } else {
          creds$username
        }
        choices <- setNames(seq_len(nrow(creds)), labels)

        tagList(
          h4("Select saved credentials:"),
          fluidRow(
            column(
              6,
              selectizeInput(
                ns("saved_cred_select"),
                label   = NULL,
                choices = c("-- select --" = "", choices),
                width   = "100%"
              )
            )
          ),
          tags$hr(style = "border-top: 1px dashed #ccc;")
        )
      })

      # Populate fields when saved credential is chosen; password is not stored
      # so the user must re-enter it to complete the login.
      observeEvent(input$saved_cred_select, {
        req(input$saved_cred_select != "")
        creds <- saved_credentials()
        idx   <- as.integer(input$saved_cred_select)
        row   <- creds[idx, ]
        updateTextInput(session,    "baseurl",  value = row$url)
        updateTextInput(session,    "username", value = row$username)
        updateTextInput(session,    "password", value = "")
      })

      # Save credentials button: shown after a new successful login
      output$save_creds_ui <- renderUI({
        req(login())
        creds <- saved_credentials()
        already_saved <- !is.null(creds) && nrow(creds) > 0 &&
          any(creds$url == baseurl() & creds$username == input$username)
        if (already_saved) return(NULL)
        tagList(
          actionButton(
            ns("save_credentials"),
            label = "Save URL & username",
            icon  = icon("floppy-disk"),
            class = "btn-success btn-sm",
            style = "margin-top: 25px;"
          ),
          tags$p(
            tags$small(icon("lock"), " Password is not saved."),
            style = "color: #666; margin-top: 4px; font-size: 11px;"
          )
        )
      })

      observeEvent(input$save_credentials, {
        req(login(), data.folder())
        # Only url + username are stored; passwords are never written to disk.
        new_row <- tibble::tibble(
          url      = baseurl(),
          username = input$username
        )
        existing <- saved_credentials()
        if (!is.null(existing) && nrow(existing) > 0) {
          # Replace any existing entry for same url + username
          existing <- dplyr::filter(
            existing,
            !(url == baseurl() & username == input$username)
          )
          all_creds <- dplyr::bind_rows(existing, new_row)
        } else {
          all_creds <- new_row
        }
        saveRDS(all_creds, cred_file())
        showNotification(
          paste0("URL and username saved for ", input$username,
                 ". Password is not stored."),
          type     = "message",
          duration = 4
        )
      })

      # Core login logic ----

      attempt_login <- function(confirmed = FALSE) {
        if (
          is_empty(baseurl()) ||
            is_empty(input$username) ||
            is_empty(input$password)
        ) {
          login(FALSE)
          return()
        }

        # Warn before connecting over plain HTTP
        if (!confirmed && grepl("^http://", baseurl())) {
          showModal(modalDialog(
            title = "Insecure connection — confirm?",
            tags$p(
              icon("triangle-exclamation", style = "color: #f0ad4e; margin-right: 6px;"),
              "This URL uses ", tags$strong("http://"), " (not https://)."
            ),
            tags$p(
              "Your username and password will be transmitted in ",
              tags$strong("plain text"), " and could be intercepted on the network."
            ),
            tags$p("Are you sure you want to continue?"),
            footer    = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_http_login"), "Connect anyway",
                           class = "btn-warning")
            ),
            easyClose = TRUE,
            fade      = FALSE
          ))
          return()
        }

        cat('\n* login attempt:', baseurl(), input$username)
        loginAttempted(TRUE)
        l <- try(loginDHIS2(baseurl(), input$username, input$password, timeout = 45))
        result <- isTRUE(l)
        login(result)
        loginError(!result)
        cat('\n - login result:', result)
      }

      observeEvent(input$confirm_http_login, {
        removeModal()
        attempt_login(confirmed = TRUE)
      })

      # Auto-login when all fields become non-empty
      credentialsProvided <- reactive({
        !is_empty(baseurl()) &&
          !is_empty(input$username) &&
          !is_empty(input$password)
      })

      observeEvent(credentialsProvided(), {
        if (credentialsProvided()) attempt_login()
      })

      # Force re-attempt from saved credential selector
      observeEvent(loginTrigger(), {
        req(loginTrigger() > 0L)
        attempt_login()
      }, ignoreInit = TRUE)

      # Status display ----
      output$status_ui <- renderUI({
        if (login()) {
          tags$h3(
            style = "color: #3c763d;",
            icon("circle-check"), " Logged in"
          )
        } else if (loginAttempted() && loginError()) {
          tags$div(
            tags$h3(style = "color: #a94442;", icon("circle-xmark"), " Login failed"),
            tags$p(
              style = "color: #a94442; font-size: 13px;",
              "Check the URL, username, and password and try again."
            )
          )
        } else {
          tags$h3(style = "color: #888;", "Not logged in")
        }
      })

      # instance label and base reactives ----
      instance <- reactive({ input$baseurl })

      baseurl <- reactive({
        req(input$baseurl)
        suffix.part <- "dhis-web"
        strsplit(input$baseurl, suffix.part)[[1]][1]
      })

      username <- reactive({ input$username })
      password <- reactive({ input$password })

      # Load Demo Data — shinyDirButton ----
      demo_volumes <- c(Home = path.expand("~"))
      icloud_path  <- file.path(path.expand("~"),
                                "Library/Mobile Documents/com~apple~CloudDocs")
      if (dir.exists(icloud_path))
        demo_volumes <- c("iCloud Drive" = icloud_path, demo_volumes)
      onedrive_path <- file.path(path.expand("~"), "OneDrive")
      if (dir.exists(onedrive_path))
        demo_volumes <- c("OneDrive" = onedrive_path, demo_volumes)

      shinyFiles::shinyDirChoose(input, "demo_folder",
                                 roots = demo_volumes, session = session)
      shinyFiles::shinyDirChoose(input, "demo_pdrlao_folder",
                                 roots = demo_volumes, session = session)

      # Holds the prepared directory path until the user dismisses the modal.
      # demo_dir() is only set when OK is clicked — avoids reactive updates
      # triggered by demo_dir() replacing the modal before the user reads it.
      .pending_demo_dir <- reactiveVal(NULL)

      .run_demo_setup <- function(chosen_dir, setup_fn, label) {
        req(!is.null(demo_dir))
        showModal(modalDialog(
          title     = paste0("Setting up ", label, " demo"),
          tags$p("Preparing metadata, formula, and dataset files..."),
          easyClose = FALSE, footer = NULL, fade = FALSE
        ))
        tryCatch({
          result_dir <- setup_fn(dir = chosen_dir, overwrite = TRUE)
          .pending_demo_dir(result_dir)   # store result; do NOT set demo_dir yet
          showModal(modalDialog(
            title     = paste0(label, " demo ready"),
            tags$p(
              icon("circle-check", style = "color:#3c763d; margin-right:6px;"),
              tags$strong("Metadata and Regions map are ready to browse.")
            ),
            tags$p(
              icon("arrow-right", style = "margin-right:6px;"),
              "To load sample malaria data, go to the ",
              tags$strong("Data"), " tab."
            ),
            easyClose = FALSE, fade = FALSE,
            footer = actionButton(session$ns("demo_ok_go"),
                                  "OK — Browse Metadata",
                                  class = "btn-primary",
                                  icon  = icon("arrow-right"))
          ))
        }, error = function(e) {
          showModal(modalDialog(
            title     = "Demo setup failed",
            tags$p(conditionMessage(e)),
            easyClose = TRUE, fade = FALSE,
            footer = modalButton("Close")
          ))
        })
      }

      # Modal OK: NOW set demo_dir (after user reads the message) and navigate
      observeEvent(input$demo_ok_go, {
        removeModal()
        d <- .pending_demo_dir()
        if (!is.null(d)) {
          demo_dir(d)
          .pending_demo_dir(NULL)
        }
        if (!is.null(nav_goto)) nav_goto("Metadata")
      })

      observeEvent(input$demo_folder, {
        if (is.integer(input$demo_folder)) return()
        chosen_dir <- shinyFiles::parseDirPath(demo_volumes, input$demo_folder)
        if (length(chosen_dir) == 0 || !nzchar(chosen_dir)) return()
        .run_demo_setup(chosen_dir, mg2_demo_setup, "Sierra Leone")
      })

      observeEvent(input$demo_pdrlao_folder, {
        if (is.integer(input$demo_pdrlao_folder)) return()
        chosen_dir <- shinyFiles::parseDirPath(demo_volumes, input$demo_pdrlao_folder)
        if (length(chosen_dir) == 0 || !nzchar(chosen_dir)) return()
        .run_demo_setup(chosen_dir, mg2_pdrlao_setup, "PDR Lao")
      })

      # Demo mode actionButtons (Connect Cloud / server deployments where
      # shinyDirButton cannot browse the server filesystem)
      observeEvent(input$load_sl_demo, {
        .run_demo_setup(
          file.path(tempdir(), "mg2_demo_sl"),
          mg2_demo_setup,
          "Sierra Leone"
        )
      })

      observeEvent(input$load_pdrlao_demo, {
        .run_demo_setup(
          file.path(tempdir(), "mg2_demo_pdrlao"),
          mg2_pdrlao_setup,
          "PDR Lao"
        )
      })

      # Demo instance picker ----
      observeEvent(input$demo_picker, {
        instances_data <- tryCatch({
          resp <- GET("https://api.im.dhis2.org/instances/public")
          jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)
        }, error = function(e) NULL)

        if (is.null(instances_data)) {
          showModal(modalDialog(
            title     = "Could not load demo instances",
            "Check your internet connection and try again.",
            footer    = modalButton("Close"),
            easyClose = TRUE
          ))
          return()
        }

        cats    <- instances_data[[1]]$categories
        api_choices <- lapply(cats, function(cat) {
          urls   <- sapply(cat$instances, `[[`, "hostname")
          labels <- sapply(cat$instances, `[[`, "description")
          labels <- sub("^DHIS2? 2? ?", "", labels)
          setNames(urls, labels)
        })
        names(api_choices) <- sapply(cats, `[[`, "label")

        pdr_choices <- list(
          "PDR Lao" = c("PDR Lao HMIS (demo_en / District1#)" =
                          "https://demos.dhis2.org/hmis_dev/")
        )
        choices <- c(pdr_choices, api_choices)

        showModal(modalDialog(
          title = "Choose a DHIS2 Demo Instance",
          size  = "m",
          selectInput(
            ns("demo_instance_url"),
            label   = "Instance:",
            choices = c("-- select an instance --" = "", choices),
            width   = "100%"
          ),
          tags$p(
            tags$small(
              icon("info-circle"),
              " Most instances: admin / district.",
              tags$br(),
              " PDR Lao instance: demo_en / District1#"
            ),
            style = "color:#666; margin-top:4px;"
          ),
          footer    = tagList(
            modalButton("Cancel"),
            actionButton(ns("demo_connect"), "Connect", class = "btn-primary")
          ),
          easyClose = TRUE
        ))
      })

      observeEvent(input$demo_connect, {
        req(input$demo_instance_url, input$demo_instance_url != "")
        url <- input$demo_instance_url
        if (!endsWith(url, "/")) url <- paste0(url, "/")
        if (grepl("demos\\.dhis2\\.org", url)) {
          demo_user <- "demo_en"
          demo_pass <- "District1#"
        } else {
          demo_user <- "admin"
          demo_pass <- "district"
        }
        updateTextInput(session, "baseurl",  value = url)
        updateTextInput(session, "username", value = demo_user)
        updateTextInput(session, "password", value = demo_pass)
        removeModal()
      })

      # System info table ----
      system.info <- reactive({
        req(login())
        if (login()) {
          cat('\n *login_widget system.info')
          url     <- paste0(baseurl(), "api/system/info")
          getInfo <- GET(url, authenticate(username(), password()))
          getInfo.content <- content(getInfo, "text", encoding = "UTF-8")
          info <- tryCatch(
            jsonlite::fromJSON(getInfo.content),
            error = function(e) {
              cat('\n - system.info: could not parse JSON (HTTP', status_code(getInfo), ')')
              return(NULL)
            }
          )
          if (is.null(info)) return(tibble(Attribute = "systemInfo", Value = "Could not retrieve"))
          info[map_dbl(info, length) == 1] %>%
            as_tibble() %>%
            select(any_of(c(
              "systemName", "version",
              "lastAnalyticsTableSuccess",
              "intervalSinceLastAnalyticsTableSuccess",
              "lastAnalyticsTableRuntime",
              "buildTime", "serverDate", "contextPath",
              "calendar", "dateFormat"
            ))) %>%
            gather(Attribute, Value)
        } else {
          tibble(connection = "Waiting for login")
        }
      })

      output$systemInfo <- renderTable(
        if (is.null(system.info())) {} else { system.info() },
        align = "ll"
      )

      # ── Load from URL ─────────────────────────────────────────────────────────
      observeEvent(input$load_from_url, {
        url_data <- trimws(input$url_data %||% "")
        if (!nzchar(url_data)) {
          showModal(modalDialog(
            title     = "Data URL required",
            tags$p("Please paste a link to your processed data file (.qs or .rds)."),
            easyClose = TRUE, fade = FALSE, footer = modalButton("OK")
          ))
          return()
        }

        showModal(modalDialog(
          title     = "Downloading files…",
          tags$p("Fetching your data from the shared links. This may take a moment."),
          easyClose = FALSE, footer = NULL, fade = FALSE
        ))

        result <- tryCatch({
          destdir <- file.path(tempdir(), paste0("mg2_url_", format(Sys.time(), "%H%M%S")))
          mg2_load_from_urls(
            data_url    = url_data,
            formula_url = trimws(input$url_formula %||% ""),
            meta_url    = trimws(input$url_meta    %||% ""),
            destdir     = destdir
          )
        }, error = function(e) {
          removeModal()
          showModal(modalDialog(
            title     = "Download failed",
            tags$p(conditionMessage(e)),
            tags$p(tags$small("Check that the link is a direct download URL and that sharing permissions allow access.")),
            easyClose = TRUE, fade = FALSE, footer = modalButton("Close")
          ))
          NULL
        })

        if (is.null(result)) return()

        .pending_demo_dir(result)
        has_meta    <- length(list.files(result, pattern = "^metadata.*\\.rds$")) > 0
        has_formula <- length(list.files(result, pattern = "\\.xlsx$")) > 0

        showModal(modalDialog(
          title     = "Files ready",
          tags$p(icon("circle-check", style = "color:#3c763d; margin-right:6px;"),
                 tags$strong("Data downloaded successfully.")),
          if (!has_formula) tags$p(tags$small(icon("triangle-exclamation", style="color:#e67e00;"),
            " No formula file — go to Data tab and select elements manually.")),
          if (!has_meta) tags$p(tags$small(icon("triangle-exclamation", style="color:#e67e00;"),
            " No metadata file — mapping and org unit features will be limited.")),
          tags$p(icon("arrow-right", style = "margin-right:6px;"),
                 "Go to the ", tags$strong("Data"), " tab to select and load your dataset."),
          easyClose = FALSE, fade = FALSE,
          footer = actionButton(session$ns("url_load_ok"), "OK — Go to Data",
                                class = "btn-primary", icon = icon("arrow-right"))
        ))
      })

      observeEvent(input$url_load_ok, {
        removeModal()
        d <- .pending_demo_dir()
        if (!is.null(d)) {
          demo_dir(d)
          .pending_demo_dir(NULL)
        }
        if (!is.null(nav_goto)) nav_goto("Data")
      })

      # Return ####
      return(list(
        login       = login,
        baseurl     = baseurl,
        username    = username,
        password    = password,
        instance    = instance,
        system.info = system.info
      ))
    }
  )
}
