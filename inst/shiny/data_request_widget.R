data_request_widget_ui = function(id) {
  ns <- NS(id)

  tagList(
    shinybusy::add_busy_spinner(
      spin = "fading-circle", # "self-building-square",
      position = 'bottom-right'
      # , margins = c(70, 1200)
    ),

    fluidPage(
      div(
        h5("Request data from DHIS2 instance"), # Header text for the top of the page
        style = "font-weight: bold; text-align: center; text-decoration: underline;" # Center alignment and spacing
      ),

      fluidRow(
        column(2, h2("Formula: ")),
        column(10, h2(textOutput(ns("formulaName"))))
      ),

      p("\n"),

      fluidRow(
        column(
          6,
          offset = 0,
          selectInput(
            ns("level"),
            label = "OrgUnit Levels:",
            width = '90%',
            choices = "Load metadata to get values",
            selected = 1,
            multiple = FALSE,
            selectize = FALSE,
            size = 4 ##needed for `selected = FALSE` to work )
          )
        ),

        column(
          3,
          offset = 0,

          selectInput(
            ns("period"),
            label = "Period:",
            width = '90%',
            choices = c('Monthly', 'Weekly'),
            selected = 1,
            multiple = FALSE,
            selectize = FALSE,
            # size = 4  ##needed for `selected = FALSE` to work )
          )
        ),

        column(
          2,
          offset = 0,
          selectInput(
            ns("years"),
            label = "Years:",
            width = '90%',
            choices = 1:20,
            selected = 1,
            multiple = FALSE,
            selectize = FALSE,
            size = 10
          )
        )
      ),

      fluidRow(
        # height = '25%' ,

        column(
          2,
          actionButton(
            ns("requestDataButton"),
            height = "10%",
            "Request data",
            style = 'margin-top:25px'
          ),
        ),
        column(
          10,
          div(
            p(
              "**After download complete, use refresh button (Formula tab) and then \nre-select the formula to see the download file."
            ),
            style = "font-weight: bold; text-align: center;"
          )
        )
      )
    )
  )
}

data_request_widget_server <- function(
  id,
  loginDetails = list(),
  dataDirectory = NULL,
  metadata_widget_output = NULL,
  data_widget_output = NULL,
  regions_widget_output = NULL
) {
  moduleServer(
    id,
    function(
      input,
      output,
      session
      # loginInfo = loginDetails ,
      # data.folder = dataDirectory,
      # formula.details = data_widget_output ,
      # metadata = metadata_widget_output
    ) {
      login = reactive({
        loginDetails$login()
      })
      baseurl = reactive({
        loginDetails$baseurl()
      })
      username = reactive({
        loginDetails$username()
      })
      password = reactive({
        loginDetails$password()
      })
      data.folder = reactive({
        dataDirectory$directory()
      })
      indicator = reactive({
        data_widget_output$indicator()
      })
      formulas = reactive({
        data_widget_output$formulas()
      })
      formulaName = reactive({
        data_widget_output$formulaName()
      })
      formula_elements = reactive({
        data_widget_output$formula_elements()
      })
      dataset.file = reactive({
        data_widget_output$dataset.file()
      })
      dataset = reactive({
        data_widget_output$dataset()
      })
      orgUnitLevels = reactive({
        metadata_widget_output$orgUnitLevels()
      })
      orgUnits = reactive({
        metadata_widget_output$orgUnits()
      })
      selected_regions = reactive({
        regions_widget_output$selected_regions()
      })

      dataElements = reactive({
        metadata_widget_output$dataElements()
      })
      dataSets = reactive({
        metadata_widget_output$dataSets()
      })
      categories = reactive({
        metadata_widget_output$categories()
      })
      ousTree = reactive({
        metadata_widget_output$ousTree()
      })

      # Display formula name, or a prompt when none is selected
      output$formulaName <- renderText({
        fn <- formulaName()
        if (is.null(fn) || trimws(fn) == "") {
          "— please select a formula —"
        } else {
          cat("\n* data_request_widget sees formula:", fn)
          fn
        }
      })

      # Update level names
      observeEvent(!is.null(orgUnitLevels()), {
        cat('\n* data_request_widget: updating levels')

        # testing
        # saveRDS( orgUnitLevels(), 'orgUnitLevels.rds')

        oulvls = orgUnitLevels() %>% pull(levelName)
        # oulvls = c( 'All-levels' , oulvls )
        ## disable choice of other levels
        oulvls = c('All-levels')
        updateSelectInput(
          session,
          'level',
          choices = oulvls,
          selected = 'All-levels'
        )
        cat("...done")
      })

      # Update period: choose largest value of month or week
      observe({
        cat('\n* updating period')
        fe <- formula_elements()
        if (!is.null(fe) && nrow(fe) > 0 && "periodType" %in% names(fe)) {
          p = min(fe$periodType, na.rm = TRUE)
          updateSelectInput(session, 'period', selected = p)
        }
      })

      # Request data ####
      request = reactiveVal(FALSE)

      observeEvent(input$requestDataButton, {
        # Collect any missing prerequisites and report them all at once
        problems <- character(0)

        if (!isTRUE(login()))
          problems <- c(problems, "You are not logged in to a DHIS2 server (Setup tab).")

        if (is_empty(orgUnitLevels()))
          problems <- c(problems, "Metadata has not been loaded (Metadata tab).")

        fn <- tryCatch(formulaName(), error = function(e) NULL)
        if (is.null(fn) || !nzchar(trimws(fn)))
          problems <- c(problems, "No formula is selected. Choose a formula from the Formula tab.")

        fe <- tryCatch(formula_elements(), error = function(e) NULL)
        if (is.null(fe) || nrow(fe) == 0)
          problems <- c(problems,
            "The selected formula has no data elements. Add elements on the Formula tab before requesting data.")

        if (length(problems) > 0) {
          showModal(modalDialog(
            title = "Cannot start data request",
            tags$ul(lapply(problems, tags$li)),
            easyClose = TRUE, fade = FALSE, size = "m", footer = modalButton("OK")
          ))
          return()
        }

        # Warn if a region filter is active — only selected org units will download
        sr <- tryCatch(selected_regions(), error = function(e) NULL)
        active_regions <- unlist(sr[c("level2", "level3", "level4", "level5", "level6")])
        if (length(active_regions) > 0) {
          showModal(modalDialog(
            title = "Region filter is active",
            tags$p(
              icon("triangle-exclamation", style = "color: #f0ad4e; margin-right: 6px;"),
              "A region filter is selected in the Regions tab. ",
              tags$strong("Only the filtered org units will be downloaded"),
              " — not the full country."
            ),
            tags$p(
              "Selected: ",
              tags$strong(paste(active_regions, collapse = ", "))
            ),
            tags$p(
              tags$small(
                "To download all org units, go to the Regions tab,",
                " clear the selection, and return here."
              ),
              style = "color: #666;"
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(session$ns("confirm_region_download"), "Download filtered region",
                           class = "btn-warning")
            ),
            easyClose = TRUE, fade = FALSE, size = "m"
          ))
          return()
        }

        request(TRUE)
        cat('\n * data_request_widget requestData Button', request(), '\n')
        formula.request()
      })

      observeEvent(input$confirm_region_download, {
        removeModal()
        request(TRUE)
        cat('\n * data_request_widget requestData Button (region confirmed)', request(), '\n')
        formula.request()
      })

      orgUnitRequest = reactive({
        req(input$level)
        cat('\n* data_request_widget orgUnits reactive')

        # Update Feb 20225
        # if all_levels (national), then null and api will pull from all child values below highest level.
        # if Regions has selection and sub-national selected, then return the selected  ou

        # .orgUnitLevels = orgUnitLevels()

        # ou = case_when(
        #
        #   input$level %in% 'All-levels' ~
        #     list( .orgUnitLevels %>% arrange( level ) %>% pull( level ) %>%
        #     paste0( "LEVEL-" , .  ) )  ,
        #
        #   # input$level %in% 'Leaf-only' ~
        #   #   list(
        #   #   # split orgunit ids into chunks of 100
        #   #   orgUnits  %>%
        #   #     filter( leaf == TRUE ) %>% pull( id ) %>%
        #   #           split( . , ceiling(seq_along( . )/100) ) %>%
        #   #           map_chr( . , ~paste( .x , collapse = ";" ) )
        #   #   )  ,
        #
        #   TRUE ~ list(
        #     .orgUnitLevels %>%
        #       filter( levelName %in% input$level ) %>%
        #       pull( level ) %>%  paste0( "LEVEL-" , .  )  )
        #
        # ) %>% unlist #nb: each case evaluated as list, otherwise always returns vector of max length
        #
        # cat( '\n - data_request_widget orgUnits:' , ou )

        # if regions selected...
        cat("\n - selected_regions:")
        # selected_regions = selected_regions()
        sr = selected_regions()
        # sr = list( level2 = selected_regions$level2 ,
        #          level3 = selected_regions$level3 ,
        #          level4 = selected_regions$level4 ,
        #          level5 = selected_regions$level5 )

        cat("\n - selected_regions:", unlist(sr))
        # cat( "\n - observer(selected_regions):" , unlist( isolate(selected_regions() ) ) )

        if (!is.null(sr[[1]])) {
          cat("\n - data_request_widget orgUnitRequest selected_regions")

          level = find_lowest_nonnull(sr)
          # ou = paste( sr[[ level ]] , collapse = ";" )
          ou = sr[[level]]
          cat("\n\n -- ", level, ou)
        } else {
          ou = NULL
        }

        return(ou)
      })

      completedRequest = reactiveVal(0)

      formula.request = reactive({
        # cat( '\n Are orgUnitLevels() available?:' ,  !is_empty( orgUnitLevels() ) )

        if (is_empty(orgUnitLevels())) {
          showModal(
            modalDialog(
              title = "Please load metadata before requesting data",
              easyClose = TRUE,
              fade = FALSE,
              size = 'm',
              footer = NULL
            )
          )
          return()
        }

        if (login() & request()) {
          cat('\n* formula.request reactive')

          .dir = data.folder()
          .baseurl = baseurl()
          .username = username()
          .password = password()
          .period = input$period
          .years = input$years
          .level = input$level
          # .orgUnitLevels = orgUnitLevels()
          # .orgUnits = orgUnitRequest()

          .formula.name = indicator()

          # To use jim Grace api/dataSets, need to set .orgunits to the highest level
          # If orgUnits specified, then use them

          # Testing
          # saveRDS( orgUnits(), 'orgUnits.rds')
          # saveRDS( orgUnitRequest() , "orgUnitRequest.rds" )

          if (is.null(orgUnitRequest())) {
            cat("\n - National data request")
            .orgUnits = orgUnits() %>%
              filter(level %in% min(orgUnits()$level, na.rm = T)) %>%
              pull(id)
          } else {
            cat("\n - subNational data request")
            sr <- selected_regions()
            if (!is.null(sr$ids) && length(sr$ids) > 0) {
              # Table row selection: use IDs directly to avoid duplicate-name ambiguity
              cat("\n - using", length(sr$ids), "org unit IDs from table selection")
              .orgUnits <- sr$ids
            } else {
              .orgUnits = orgUnits() %>%
                filter(name %in% orgUnitRequest()) %>%
                pull(id)
            }
          }

          # If categoryOption is NA, then omit from request.  This will return 'total' with no categories.
          # NB this happens if any category is missing
          # TODO: revise so there can be a mix of categoryOptionCombo and no categoryOptionCombo

          #        .elements = formula_elements() %>%
          # mutate( )
          # unite( id , dataElement.id, categoryOptionCombo.ids , sep="." ) %>%
          # unite( name , dataElement, Categories , sep="." ) %>%
          # select( id , name )

          if (any(!is.na(formula_elements()$categoryOptionCombo.ids))) {
            .elements = formula_elements() %>%
              filter(!dataElement.id == "aaaaaaaaaaa") %>%
              mutate(
                categoryOptionCombo.ids = ifelse(
                  is.na(categoryCombo.id),
                  NA,
                  categoryOptionCombo.ids
                ),
                Categories = ifelse(is.na(categoryCombo.id), NA, Categories)
              ) %>%
              unite(
                id,
                dataElement.id,
                categoryOptionCombo.ids,
                sep = ".",
                na.rm = TRUE
              ) %>%
              unite(name, dataElement, Categories, sep = ".", na.rm = TRUE) %>%
              select(id, name)
          } else {
            .elements = formula_elements() %>%
              unite(id, dataElement.id, sep = ".") %>%
              unite(name, dataElement, sep = ".") %>%
              select(id, name)
          }

          # TEsting
          # saveRDS( .elements, "dataRequestElements.rds" )

          .level1.id = orgUnits() %>% filter(level == 1) %>% pull(id)

          cat('\n - .level1.id:', .level1.id)

          cat(
            '\n - data_request elements:',
            length(.elements$name),
            ':\n',
            .elements$name
          )

          if (length(.elements$name) == 0) {
            showModal(modalDialog(
              title = "No formula elements",
              "The selected formula has no data elements. Please add elements on the Data tab before requesting data.",
              easyClose = TRUE,
              fade = FALSE,
              size = 's',
              footer = NULL
            ))
            return(NULL)
          }

          cat('\n - dataset():', dataset.file())
          file = paste0(data.folder(), dataset.file())

          if (file_test("-f", file)) {
            .previous_dataset_file = file
            .update = TRUE
            cat('\n - previous file exists:', file)
            # Previous dataset file:
            .dataset = dataset()
          } else {
            cat('\n - no previous dataset file:')
            .update = FALSE
            .previous_dataset_file = ''
            .dataset = NA
          }

          cat("\n - .period is", input$period)
          # If missing, assume period is monthly
          if (is.null(.period)) {
            .period = "Monthly"
          }

          x = api_data(
            update = .update,
            baseurl = .baseurl,
            username = .username,
            password = .password,
            elements = .elements,
            orgUnits = .orgUnits,
            # periods = .periods ,
            periodType = .period,
            YrsPrevious = as.integer(.years),
            formula = .formula.name,
            previous_dataset_file = .previous_dataset_file,
            prev.data = .dataset,
            level1.id = .level1.id,
            dir = data.folder(),
            shinyApp = TRUE,
            parallel = FALSE,
            childOnly = TRUE
          )

          .periods = paste0(.years, 'yrs')

          saveAs = paste0(
            .dir,
            .formula.name,
            "_",
            .level,
            "_",
            .periods,
            "_",
            Sys.Date(),
            ".", mg2_data_ext()
          )

          cat('\n saving formula.request as', saveAs)

          .partial <- isTRUE(attr(x, "partial_download"))

          showModal(
            modalDialog(
              title = if (.partial) "Download interrupted -- partial data saved" else "Finished downloading.  Now saving the raw data download",
              easyClose = TRUE,
              fade = FALSE,
              size = if (.partial) "m" else "s",
              if (.partial) tagList(
                tags$p(paste0(
                  nrow(x), " records collected before the connection failed."
                )),
                tags$p(tags$b("Error: "), attr(x, "partial_message")),
                tags$p(
                  "The partial data has been saved. ",
                  "To resume: select this file as your dataset, then re-run the download — ",
                  "it will automatically fetch only the missing periods."
                )
              ),
              footer = modalButton("OK")
            )
          )

          save_file(x, saveAs)
          if (.partial) {
            cat('\n* partial download saved — stopping early\n')
            return()
          }
          removeModal()

          # showModal(
          #   modalDialog( title = "New file is saved",
          #                easyClose = TRUE ,
          #                size = 's'
          #                # footer= '(To refresh data in the app: use the refresh button (to left) and then re-select formula file and formula. '
          #                )
          # )

          cat('\n* finished downloading', .formula.name, '\n')
          completedRequest(completedRequest() + 1)

          cat('\n -- preparing data1 + outlier scans')

          # Prepare + scan in one pass with per-element progress notification ####
          n_fe <- if (!is.null(formula_elements())) nrow(formula_elements()) else 0L

          d1 <- withProgress(
            message = sprintf("Preparing dataset: element 1 of %d...", n_fe),
            detail  = "starting",
            value   = 0,
            tryCatch(
              data_1(
                x,
                formula_elements = formula_elements(),
                dataSets         = dataSets(),
                dataElements     = dataElements(),
                categories       = categories(),
                ousTree          = ousTree(),
                .scan_outliers   = TRUE,
                .shiny_progress  = TRUE,
                .progress        = function(i, n, element_name, phase) {
                  if (i == 0L) {
                    setProgress(value = 0, message = "Preparing dataset...", detail = phase)
                  } else if (startsWith(phase, "MAD:") || startsWith(phase, "seasonal:")) {
                    scan_label <- if (startsWith(phase, "MAD:")) "MAD outliers" else "Seasonal outliers"
                    pct_str    <- sub("^(MAD|seasonal):\\s*", "", phase)
                    setProgress(
                      message = sprintf("Element %d of %d \u2014 %s: %s", i, n, scan_label, pct_str),
                      detail  = substr(element_name, 1, 55)
                    )
                  } else {
                    setProgress(
                      message = sprintf(
                        "Preparing dataset: element %d of %d \u2014 %s",
                        i, n, phase
                      ),
                      detail = substr(element_name, 1, 55)
                    )
                  }
                }
              ),
              error = function(e) {
                showModal(modalDialog(
                  title = "Error preparing dataset",
                  conditionMessage(e),
                  easyClose = TRUE, fade = FALSE
                ))
                NULL
              }
            )
          )
        }

        if (is.null(d1)) return()

        # Search for duplicated rows ####
        nrow1 = nrow(d1)

        cat("\n - d1 has", nrow1, "rows")

        if (nrow1 == 0) {
          cat('\n - nrow1 = 0')
          return()
        }

        # remove duplicate rows because downloads may create duplicates
        u = d1 %>% as.data.table() %>% unique
        nrow2 = nrow(u)
        cat('\n - There were', nrow1 - nrow2, 'duplicates')

        showModal(
          modalDialog(
            title = "Saving data — please wait",
            easyClose = FALSE,
            fade = FALSE,
            size = 's',
            footer = NULL
          )
        )

        # Saving ####
        cat('\n - saving dataset to', saveAs)

        save_file(u, saveAs)
        removeModal()
      })

      return(list(
        completedRequest = reactive({
          completedRequest
        })
      ))
    }
  ) #end moduleServer
} #end server
