login_widget_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # fillCol( height = 600, flex = c(NA ) ,
  tagList(
    shinybusy::add_busy_spinner(
      spin = "fading-circle", # "self-building-square",
      position = 'bottom-right'
      # , margins = c(70, 1200)
    ),

    fluidPage(
      hr(),

      h3('Step 2. Login to DHIS2 server*'),

      h4('Enter address and credentials:'),

      inputPanel(
        textInput(ns("baseurl"), label = "DHIS2 URL:", NULL), # "https://play.dhis2.org/2.33.1/"

        textInput(ns("username"), label = "User name:", NULL), # "admin"

        passwordInput(ns("password"), label = "Password:", NULL)
      ),

      fluidRow(
        column(4, h2(textOutput(ns("Status")))),

        column(8, tableOutput(ns('systemInfo')))
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
          ),
          tags$p(
            tags$small(
              "Once data has been downloaded, the app works fully offline without a server connection."
            ),
            style = "margin-top: 8px; color: #666;"
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

      # reactives to toggle login status
      login = reactiveVal(FALSE)

      # Demo instance picker ####
      # Fetches live list from https://api.im.dhis2.org/instances/public
      # and shows a modal with instances grouped by category (Stable / Dev).
      observeEvent(input$demo_picker, {
        instances_data <- tryCatch({
          resp <- GET("https://api.im.dhis2.org/instances/public")
          jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)
        }, error = function(e) NULL)

        if (is.null(instances_data)) {
          showModal(modalDialog(
            title = "Could not load demo instances",
            "Check your internet connection and try again.",
            footer = modalButton("Close"),
            easyClose = TRUE
          ))
          return()
        }

        # Build named list for selectInput: list(Category = c(label = url, ...))
        cats <- instances_data[[1]]$categories
        choices <- lapply(cats, function(cat) {
          urls   <- sapply(cat$instances, `[[`, "hostname")
          labels <- sapply(cat$instances, `[[`, "description")
          # Trim "DHIS 2 " / "DHIS2 " prefix for brevity
          labels <- sub("^DHIS2? 2? ?", "", labels)
          setNames(urls, labels)
        })
        names(choices) <- sapply(cats, `[[`, "label")

        showModal(modalDialog(
          title = "Choose a DHIS2 Demo Instance",
          size  = "s",
          selectInput(
            ns("demo_instance_url"),
            label   = "Instance:",
            choices = choices,
            width   = "100%"
          ),
          tags$p(
            tags$small(icon("info-circle"), " Login: admin / district"),
            style = "color:#666; margin-top:4px;"
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("demo_connect"), "Connect", class = "btn-primary")
          ),
          easyClose = TRUE
        ))
      })

      observeEvent(input$demo_connect, {
        req(input$demo_instance_url)
        url <- input$demo_instance_url
        if (!endsWith(url, "/")) url <- paste0(url, "/")
        updateTextInput(session, "baseurl",   value = url)
        updateTextInput(session, "username",  value = "admin")
        updateTextInput(session, "password",  value = "district")
        removeModal()
      })

      # instance label for status display
      instance = reactive({
        input$baseurl
      })

      baseurl = reactive({
        req(input$baseurl)
        # if url is from login or dashboard url, trimto get baseurl
        # possible.suffixes:
        suffix.part = "dhis-web"

        strsplit(input$baseurl, suffix.part)[[1]][1]
      })

      username = reactive({
        input$username
      })

      password = reactive({
        input$password
      })

      credentialsProvided <- reactive({
        req(baseurl())
        req(input$username)

        credentialsProvided = !is_empty(baseurl()) &&
          !is_empty(input$username) &&
          !is_empty(input$password)

        print(paste('toLogin', credentialsProvided))

        return(credentialsProvided)
      })

      # Login Status
      observeEvent(credentialsProvided(), {
        print(paste('login', baseurl(), input$username, "...")) #input$password

        if (
          is_empty(baseurl()) |
            is_empty(input$username) |
            is_empty(input$password)
        ) {
          login(FALSE)
        }

        l = try(loginDHIS2(
          baseurl(),
          input$username,
          input$password,
          timeout = 45
        ))

        print(paste('try loginDHIS2 is', l, baseurl(), input$username, "...")) #

        login(isTRUE(l))

        print(paste('observe event input$password, login() is', login()))
      })

      # Update logged in status
      observeEvent(login(), {
        if (login()) {
          output$Status = renderText(paste('Logged in', instance()))
        } else {
          output$Status = renderText('Not logged in')
        }
      })

      # system info  ####
      system.info = reactive({
        req(login())
        if (login()) {
          cat('\n *login_widget system.info')

          url = paste0(baseurl(), "api/system/info")

          getInfo = GET(url, authenticate(username(), password()))

          getInfo.content = content(getInfo, "text", encoding = "UTF-8")

          info = tryCatch(
            jsonlite::fromJSON(getInfo.content),
            error = function(e) {
              cat('\n - system.info: could not parse response as JSON (HTTP', status_code(getInfo), ')')
              return(NULL)
            }
          )

          if (is.null(info)) return(tibble(Attribute = "systemInfo", Value = "Could not retrieve"))

          info[map_dbl(info, length) == 1] %>%
            as_tibble() %>%
            select(any_of(c(
              "systemName",
              "version",
              "lastAnalyticsTableSuccess",
              "intervalSinceLastAnalyticsTableSuccess",
              "lastAnalyticsTableRuntime",
              "buildTime",
              "serverDate",
              "contextPath",
              "calendar",
              "dateFormat"
            ))) %>%
            gather(Attribute, Value)
        } else {
          tibble(connection = "Waiting for login")
        }
      })

      # Status/connection  ####
      output$connection = renderText({
        req(baseurl())
        # req( login() )
        paste0(baseurl(), "api/system/info")
      })

      output$systemInfo = renderTable(
        if (is.null(system.info())) {} else { system.info() }
      )

      # Return ####
      return(list(
        login = login,
        baseurl = baseurl,
        username = username,
        password = password,
        instance = instance,
        system.info = system.info
        # uploaded_OrgUnitLevels = uploaded_OrgUnitLevels ,
        # uploaded_OrgUnits = uploaded_OrgUnits ,
        # uploaded_DataElements = uploaded_DataElements ,
        # uploaded_DataElementGroups = uploaded_DataElementGroups ,
        # uploaded_Categories = uploaded_Categories ,
        # uploaded_DataSets = uploaded_DataSets ,
        # uploaded_Indicators = uploaded_Indicators ,
        # uploaded_dataDictionary = uploaded_dataDictionary
      ))
    }
  )
}
