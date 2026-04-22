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
          12,
          hr(),
          h4("Or connect to a DHIS2 demo instance:"),
          actionButton(
            ns("demo_picker"),
            label = "Choose Demo Instance",
            icon  = icon("globe"),
            class = "btn-info"
          )
        )
      )
    )
  )
} # ui

# Server function ####
login_widget_server <- function(id, directory_widget_output = NULL) {
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
          tryCatch(readRDS(cred_file()), error = function(e) NULL)
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

      # Populate fields and trigger login when saved credential is chosen
      observeEvent(input$saved_cred_select, {
        req(input$saved_cred_select != "")
        creds <- saved_credentials()
        idx   <- as.integer(input$saved_cred_select)
        row   <- creds[idx, ]
        updateTextInput(session, "baseurl",  value = row$url)
        updateTextInput(session, "username", value = row$username)
        updateTextInput(session, "password", value = row$password)
        # Force login attempt regardless of whether fields were already populated
        loginTrigger(loginTrigger() + 1L)
      })

      # Save credentials button: shown after a new successful login
      output$save_creds_ui <- renderUI({
        req(login())
        creds <- saved_credentials()
        already_saved <- !is.null(creds) && nrow(creds) > 0 &&
          any(creds$url == baseurl() & creds$username == input$username)
        if (already_saved) return(NULL)
        actionButton(
          ns("save_credentials"),
          label = "Save credentials",
          icon  = icon("floppy-disk"),
          class = "btn-success btn-sm",
          style = "margin-top: 25px;"
        )
      })

      observeEvent(input$save_credentials, {
        req(login(), data.folder())
        new_row <- tibble::tibble(
          url      = baseurl(),
          username = input$username,
          password = input$password
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
          paste0("Credentials saved for ", input$username, "."),
          type     = "message",
          duration = 4
        )
      })

      # Core login logic ----

      attempt_login <- function() {
        if (
          is_empty(baseurl()) ||
            is_empty(input$username) ||
            is_empty(input$password)
        ) {
          login(FALSE)
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
                          "https://demos.dhis2.org/hmis_data/")
        )
        choices <- c(pdr_choices, api_choices)

        showModal(modalDialog(
          title = "Choose a DHIS2 Demo Instance",
          size  = "s",
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
