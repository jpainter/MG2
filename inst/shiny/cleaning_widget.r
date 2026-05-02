cleaning_widget_ui = function(id) {
  ns <- NS(id)

  tagList(
    shinybusy::add_busy_spinner(
      spin = "fading-circle", # "self-building-square",
      position = 'bottom-left'
      # , margins = c(70, 1200)
    ),

    tabsetPanel(
      type = "tabs",

      tabPanel(
        "Summary",

        sidebarLayout(
          sidebarPanel(
            width = 3,

            actionButton(ns('update_dataElement'), label = "Update"),

            checkboxInput(ns("select_all_dataElement"), "Select / deselect all", value = TRUE),

            div(
              style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 4px; border-radius: 4px;",
              checkboxGroupInput(
                ns("dataElement"),
                label = "DataElement_Category:",
                choices = NULL,
                selected = NULL,
                width = "100%"
              )
            ),

            uiOutput(ns("reporting_mismatch_warning")),

            selectInput(
              ns("selectOrgType"),
              label = "Filter results",
              choices = c('Facilities only', 'Admin only', 'All'),
              selected = 'Facilities only'
            )
          ),

          mainPanel(
            width = 9,

            # h5( 'Use the buttons to search for extreme values using median absolute deviation (MAD), and then seasonally adjusted outliers') ,
            #
            # actionButton( ns("determineExtremeValues") ,
            #               "Search for Extreme Values" , style='margin-top:25px'
            # )   ,

            # actionButton( ns("determineSeasonalOutliers") ,
            #               "Search for Seasonal Outliers" , style='margin-top:25px'
            # )  ,

            htmlOutput(ns("region_filter_status")),

            inputPanel(
              selectizeInput(
                ns("startingMonth"),
                label = "Begining with",
                choices = NULL,
                selected = NULL
              ),

              selectizeInput(
                ns("endingMonth"),
                label = "Ending with",
                choices = NULL,
                selected = NULL
              ),

              selectizeInput(
                ns("reporting"),
                label = "Reporting frequency",
                choices = c(
                  "All",
                  "Reporting Each Period",
                  "Inconsistent Reporting"
                ),
                selected = "All"
              ),

              sliderInput(
                ns('maxMASE'),
                "Select facilities with MASE less than or equal to -",
                min = 0.1,
                max = 1.0,
                value = .4,
                step = .05
              )
            ),

            tabsetPanel(
              type = "tabs",

              tabPanel(
                "Outlier Table",
                textOutput(ns("outlierSummaryText")),
                tableOutput(ns("outlier.summary.table")),
                br(),
                htmltools::HTML(
                  "For information on the outlier procedure and algorithms, see the <b>About</b> tab."
                )
              ),

              tabPanel(
                "Outlier Chart",
                style = "height:60vh;",
                fluidPage(
                  fluidRow(
                    style = "height:50vh;",
                    # h5( 'Select orgUnit having error')
                    # textOutput( ns("outlierSummaryText")) ,
                    plotOutput(ns("outlier.summary.chart"), height = "auto")
                  )
                )
              ),

              tabPanel(
                "Monthly Summary",
                style = "height:60vh;",
                fluidPage(
                  fluidRow(
                    style = "height:50vh;",
                    # h5( 'Select orgUnit having error')
                    # textOutput( ns("outlierSummaryText")) ,
                    plotOutput(ns("monthly_summary_chart"))
                  )
                )
              ),

              tabPanel(
                "Mean Absolute Scaled Error",
                fluidRow(style = "height:50vh;", plotOutput(ns("mase.summary")))
              ),

              tabPanel(
                "Inspect",
                style = "height:60vh;",
                fluidPage(
                  fluidRow(
                    style = "height:10vh;",
                    # h5( 'Select orgUnit having error')     ,

                    selectInput(
                      ns('Error'),
                      'Error Type',
                      choices = c(
                        'mad15',
                        'mad10',
                        'mad5',
                        'seasonal5',
                        'seasonal3'
                      )
                    ),

                    # selectInput( ns('seasonalError'), 'Seasonally Adjusted error' ,
                    #              choices = c( 'seasonal5', 'seasonal3' ) ) ,

                    selectizeInput(
                      ns('flaggedOrgUnit'),
                      'Select orgUnit having this error',
                      choices = ""
                    ),

                    checkboxInput(ns('showAllData'), 'Show all data elements')
                  ),

                  fluidRow(
                    style = "height:40vh;",
                    plotOutput(
                      ns("inspect")
                      # , hover = ns("plot_hover") ,
                      # click = ns("plot_click") )
                      # , uiOutput( ns("dynamic")
                    )
                  )

                  # , fluidRow( style = "height:5vh;",
                  #          verbatimTextOutput( ns("info") ))
                )
              )
            )
          )
        )
      ),

      tabPanel(
        "Data View",
        div(
          style = "padding: 6px 12px 2px 12px;",
          tags$small(style = "color:#555;",
            "Flagged values only — sorted by champion facilities first, then ratio (value ÷ median) descending. ",
            "Click a row to view the full time series for that facility and indicator."
          )
        ),
        DT::DTOutput(ns("flagged_dt")),
        plotly::plotlyOutput(ns("drill_chart"), height = "380px")
      )

      # tabPanel( "Summary (under construction)",
      #                 html("<div style='display:flex;'>") ,
      #                   htmlOutput( ns("profileSummary") ) ,
      #                 html("<div>") ,
      #
      #       )
    )
  )
} # ui


cleaning_widget_server <- function(
  id,
  directory_widget_output = NULL,
  metadata_widget_output = NULL,
  data_widget_output = NULL,
  reporting_widget_output = NULL,
  regions_widget_output = NULL,
  current_tab = NULL
) {
  moduleServer(
    id,
    function(input, output, session) {
      # Dependencies ####
      data.folder = reactive({
        directory_widget_output$directory()
      })
      indicator = reactive({
        data_widget_output$indicator()
      })
      formulas = reactive({
        data_widget_output$formulas()
      })
      dataset.file = reactive({
        data_widget_output$dataset.file()
      })
      # dataset = reactive({ data_widget_output$dataset() })

      ## Set reactive data only when tab is active
      data1 = reactive({
        # req( input$tabs == "outliers" )
        # cat( "\n*** Outlier tab is active" )
        data_widget_output$data1()
      })
      selected_data = reactive({
        # req( input$tabs == "outliers" )
        # cat( "\n*** Outlier tab is active" )
        reporting_widget_output$selected_data()
      })

      formula_elements = reactive({
        data_widget_output$formula_elements()
      })
      dataElements = reactive({
        metadata_widget_output$dataElements()
      })
      categories = reactive({
        metadata_widget_output$categories()
      })
      orgUnits = reactive({
        metadata_widget_output$orgUnits()
      })
      ousTree = reactive({
        metadata_widget_output$ousTree()
      })
      orgUnitLevels = reactive({
        metadata_widget_output$orgUnitLevels()
      })

      reportingSelectedOUs = reactive({
        req(input$tabs == "evaluation")
        reporting_widget_output$reportingSelectedOUs()
      })

      dates = reactive({
        reporting_widget_output$dates()
      })
      data.hts = reactive({
        reporting_widget_output$data.hts()
      })
      levelNames = reactive({
        reporting_widget_output$levelNames()
      })
      period = reactive({
        reporting_widget_output$period()
      })
      split = reactive({
        reporting_widget_output$split()
      })
      startingMonth = reactive({
        reporting_widget_output$startingMonth()
      })
      endingMonth = reactive({
        reporting_widget_output$endingMonth()
      })
      num_datasets = reactive({
        reporting_widget_output$num_datasets()
      })
      num_facilities = reactive({
        reporting_widget_output$num_facilities()
      })
      plotData = reactive({
        reporting_widget_output$plotData()
      })
      caption.text = reactive({
        reporting_widget_output$caption.text()
      })

      # data = reactive({ reporting_widget_output$d() })
      # data.total = reactive({ reporting_widget_output$data.total() })

      # outlierData ####

      outlierData <- reactiveValues(df_data = NULL) # use observeEvent levelNames to set to data1()

      # Region filter — driven by the Regions page ####

      regions_selected = reactive({
        if (!is.null(regions_widget_output)) regions_widget_output$selected_regions() else list()
      })

      output$region_filter_status <- renderUI({
        sr <- regions_selected()
        parts <- Filter(function(x) !is.null(x) && length(x) > 0,
                        list(sr$level2, sr$level3, sr$level4, sr$level5))
        label <- if (length(parts) == 0) "National" else
          paste(sapply(parts, paste, collapse = ", "), collapse = " / ")
        div(
          style = paste0(
            "background:#e8f4fd; padding:8px 14px;",
            " border-left:4px solid #2196F3; margin:6px 0 10px 0; border-radius:3px;"
          ),
          tags$strong(style = "color:#1565C0; font-size:1.05em;",
                      paste("Region:", label))
        )
      })

      region_caption_text = reactive({
        sr <- regions_selected()
        parts <- Filter(function(x) !is.null(x) && length(x) > 0,
                        list(sr$level2, sr$level3, sr$level4, sr$level5))
        if (length(parts) == 0) "National" else
          paste(sapply(parts, paste, collapse = ", "), collapse = " / ")
      })

      selected_data_cats = reactiveValues(elements = NULL)

      reporting_selected = reactive({
        tryCatch(
          reporting_widget_output$selected_data_categories(),
          error = function(e) NULL
        )
      })

      observeEvent(input$update_dataElement, {
        selected_data_cats$elements = input$dataElement
      })

      output$reporting_mismatch_warning <- renderUI({
        checked   <- selected_data_cats$elements
        rep_elems <- reporting_selected()

        if (is.null(checked) || length(checked) == 0) return(NULL)
        if (is.null(rep_elems)) return(NULL)

        missing <- setdiff(checked, rep_elems)
        if (length(missing) == 0) return(NULL)

        div(
          style = paste0(
            "background:#fff3cd; padding:7px 10px;",
            " border-left:4px solid #ffc107; border-radius:3px;",
            " margin-top:6px; font-size:0.85em;"
          ),
          tags$strong(style = "color:#856404;", "⚠ Not selected in Reporting:"),
          tags$ul(
            style = "margin:4px 0 0 0; padding-left:14px;",
            lapply(missing, tags$li)
          ),
          tags$span(
            style = "color:#856404;",
            "These elements have no data flowing through — check them in the Reporting tab to include them."
          )
        )
      })

      observeEvent(input$select_all_dataElement, {
        req(data1()$data)
        choices = sort(unique(data1()$data))
        updateCheckboxGroupInput(
          session,
          "dataElement",
          selected = if (input$select_all_dataElement) choices else character(0)
        )
      }, ignoreInit = TRUE)

      # Shared helpers (same pattern as reporting_widget)
      .cleaning_split_by_role <- function(choices, fe) {
        if (is.null(fe) || nrow(fe) == 0 || !"role" %in% names(fe))
          return(list(primary = choices, secondary = character(0)))
        fe_exp <- tryCatch(
          tidyr::separate_rows(fe, Categories, categoryOptionCombo.ids, sep = ";") %>%
            dplyr::mutate(
              Categories = trimws(Categories),
              data_name  = dplyr::if_else(
                is.na(Categories) | !nzchar(trimws(Categories)),
                dataElement,
                paste(dataElement, trimws(Categories), sep = "_")
              )
            ) %>%
            dplyr::select(data_name, role) %>%
            dplyr::distinct(data_name, .keep_all = TRUE),
          error = function(e) NULL
        )
        if (is.null(fe_exp)) return(list(primary = choices, secondary = character(0)))
        role_map  <- setNames(fe_exp$role, fe_exp$data_name)
        secondary <- choices[!is.na(role_map[choices]) & role_map[choices] == "secondary"]
        primary   <- setdiff(choices, secondary)
        list(primary = primary, secondary = secondary)
      }

      .cleaning_choice_names <- function(choices, secondary) {
        lapply(choices, function(ch) {
          if (ch %in% secondary)
            tags$span(ch, tags$em(" (secondary)", style = "color:#999; font-size:0.85em;"))
          else
            ch
        })
      }

      # data names — populate from data1() so ALL formula elements (primary and
      # secondary) appear in the list.  selected_data() only contains elements
      # that were checked in the reporting widget, so using it here would hide
      # secondary elements entirely.
      observeEvent(list(data1(), selected_data()), {
        req(data1())
        cat('\n* cleaning_widget observe data1() / selected_data()')

        choices <- sort(unique(data1()$data))
        fe      <- formula_elements()
        roles   <- .cleaning_split_by_role(choices, fe)

        updateCheckboxGroupInput(
          session, "dataElement",
          choiceNames  = .cleaning_choice_names(choices, roles$secondary),
          choiceValues = choices,
          selected     = roles$primary
        )
        selected_data_cats$elements <- roles$primary
      })

      # Dates
      observeEvent(dates(), {
        cat('\n* cleaning_widget observeEvent dates()')
        cat('\n - observeEvent dates() update startingMonth-')
        dates = dates()
        updateSelectizeInput(
          session,
          'startingMonth',
          choices = dates %>% as.character(),
          selected = min(dates, na.rm = TRUE) %>% as.character(),
          server = TRUE
        )

        cat('\n- cleaning_widget observeEvent dates() update endingMonth-')
        updateSelectizeInput(
          session,
          'endingMonth',
          choices = dates %>% as.character(),
          selected = max(dates, na.rm = TRUE) %>% as.character(),
          server = TRUE
        )

        cat('\n  -done')
      })

      # scan for MAD outliers  ####
      searchForMAD = reactiveVal(FALSE)
      scanForMAD = reactiveVal(FALSE)
      afterMAD = reactiveVal(FALSE)

      searchForSeasonalOutliers = reactiveVal(FALSE)
      scanForSeasonal = reactiveVal(FALSE)
      afterSeasonal = reactiveVal(FALSE)

      # Option to rerun MAD
      rerunMadModal <- function() {
        ns <- NS(id)
        modalDialog(
          title = "MAD flags already present in data",
          # easyClose = TRUE ,
          size = 'm',
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("rerunMAD"), "Re-Run scan for MAD Outliers")
          )
        )
      }

      observeEvent(input$rerunMAD, {
        req(data1())

        searchForMAD(FALSE)
        searchForMAD(TRUE)
        cat('\n * rerun MAD outliers button:', searchForMAD())
        removeModal()
        # outlierData$df_data = data1.mad()
      })

      # observeEvent( input$determineExtremeValues  , {
      observeEvent(data1(), {
        # req( outlierData$df_data )
        # req( data1() )
        # No tab guard here: the scan should trigger automatically whenever
        # data1() changes to data without mad15 flags — whether the user is on
        # the Outliers tab or the Data tab (rescan checkbox).  The withProgress
        # modal makes the scan visible on any tab.
        cat(
          '\n* cleaning_widget observeEvent determineExtremeValues.  searchForMAD():',
          searchForMAD()
        )

        #1. Scan for extreme values (MAD)
        x = data1()
        # cat( "\n - determineExtremeValues.  mad15 %in% names( outlierData$df_data ):" , 'mad15' %in% names( outlierData$df_data ))
        cat(
          "\n - determineExtremeValues.  mad15 %in% names( outlierData$df_data ):",
          'mad15' %in% names(x)
        )
        # if ( 'mad15' %in% names( outlierData$df_data ) ){
        if ('mad15' %in% names(x)) {
          # showModal( rerunMadModal() )
          # cat('\n - reRun extreme values button:' , searchForMAD() )
        } else {
          searchForMAD(FALSE)
          searchForMAD(TRUE)
          cat('\n - determine extreme values button:', searchForMAD())
        }

        # cat( "\n - outlierData$df_data = data1.mad()")
        # outlierData$df_data = data1.mad()
      })

      observeEvent(searchForMAD(), {
        # req( outlierData$df_data )

        cat('\n* observeEvent searchForMAD: ', searchForMAD())

        if (searchForMAD()) {
          scanForMAD(TRUE)

          # cat('\n - cleaning_widget outlierData$df_data class:' , class( outlierData$df_data ) )
          cat('\n - cleaning_widget data1():', class(data1()))

          # outlierData$df_data
          x = data1.mad_seasonal()
          cat("\n - cleaning_widget searchForMAD class(x):", class(x))
        }

        # cat( '\n - searchForMAD names(outlierData$df_data) ' , names(outlierData$df_data) )

        scanForMAD(FALSE)
        afterMAD(FALSE)
        afterMAD(TRUE)
      })

      data1.mad_seasonal = reactive({
        # req( outlierData$df_data )
        req(data1())

        cat('\n* cleaning_widget data1.mad_seasonal')
        cat('\n - scanForMAD:', scanForMAD())

        # cat( '\n - class(outlierData$df_data):' , class( outlierData$df_data ))
        cat('\n - cleaning_widget data1():', class(data1()))

        if (scanForMAD()) {
          cat('\n - data1.mad search')

          # d = outlierData$df_data
          d = data1()
          cat('\n - cleaning_widget data1.mad_seasonal class(d):', class(d))

          nrow1 = nrow(d)
          if (nrow1 == 0) {
            cat('\n - nrow1 = 0')
            return()
          } else {
            cat('\n - outlierData has', nrow1, 'rows')
          }

          # remove duplicate rows because downloads may create duplicates
          u = d %>% as.data.table() %>% unique
          nrow2 = nrow(u)
          cat('\n - There are', nrow1 - nrow2, 'duplicates')

          #### section on repetitive key erros moved to mad_outliers function in Cleaning.R
          #  cat( '\n - Scanning for repetive key entry errors')
          #  key_entry_errors =
          #    count( as_tibble( d %>%
          #                    filter( nchar(original)>3 ,
          #                           effectiveLeaf ) ) ,
          #          original ) %>%
          #    arrange(-n)
          #
          # # Default: values where the number happens at least 3* > than
          #  # median of the top 10 rows
          #  key_entry_errors = key_entry_errors %>%
          #    filter(  n > 3 * median(
          #      key_entry_errors %>% filter( row_number()<11 )  %>%
          #        pull( n ) )
          #      ) %>% pull( original )
          #
          #  # print( head( key_entry_errors ) )
          #  if ( is_empty( key_entry_errors )  ) key_entry_errors = NA

          cat('\n - scanning for MAD outliers')

          .threshold <- 50
          .total     <- if (tsibble::is_tsibble(d))
            tsibble::n_keys(d)
          else
            data.table::uniqueN(data.table::as.data.table(d), by = c("orgUnit", "data.id"))

          withProgress(
            message = sprintf("Scanning %d series for extreme values (MAD)...", .total),
            detail  = "starting",
            value   = 0,
            {
              data.mad <- mad_outliers(d, .total = .total, .threshold = .threshold)
            }
          )

          cat('\n - scanning for Seasonal outliers')

          withProgress(
            message = sprintf("Scanning %d series for seasonal outliers...", .total),
            detail  = "starting",    # updated by seasonal_outliers() with core count
            value   = 0,
            {
              data1.seasonal <- seasonal_outliers(
                data.mad,
                .total         = .total,
                .threshold     = .threshold,
                shiny_progress = TRUE
              )
            }
          )

          removeModal()
          showModal(
            modalDialog(
              title = "Finished scanning for seasonal values; saving data",
              easyClose = TRUE,
              size = 'm',
              footer = "(click anywhere to close dialog box)"
            )
          )

          cat('\n - saving data1.seasonal to replace dataset')
          cat('\n - names(data1.seasonal):', names(data1.seasonal))

          saveRDS(data1.seasonal, paste0(data.folder(), dataset.file()))

          # Signal data_widget to re-read the saved file (clears rescan_val so
          # dataset() picks up the freshly-scanned file on next access)
          if (!is.null(data_widget_output$scan_done_counter)) {
            data_widget_output$scan_done_counter(
              data_widget_output$scan_done_counter() + 1L
            )
          }

          removeModal()
        } # end if scan for mad
        return(data1.seasonal)
      })

      # data1.mad = reactive({
      #     req( outlierData$df_data )
      #     cat('\n* data1.mad' )
      #     cat('\n - scanForMAD:' , scanForMAD() )
      #
      #     if ( scanForMAD() ){
      #       cat('\n - data1.mad search')
      #
      #       d = outlierData$df_data
      #       cat( '\n - class(d)' , class(d) )
      #
      #       nrow1 = nrow( d )
      #       if ( nrow1 == 0 ){
      #         cat('\n - nrow1 = 0')
      #         return()
      #       } else { cat('\n - outlierData has' , nrow1 , 'rows')}
      #
      #       # remove duplicate rows because downloads may create duplicates
      #       u = d %>% as.data.table() %>% unique
      #       nrow2 = nrow( u )
      #       cat('\n - There were', nrow1-nrow2, 'duplicates' )
      #
      #
      #     #### This section -- repetivie key errors-- moved to mad_outliers function (Cleaning.R)
      #     #  cat( '\n - Scanning for repetive key entry errors')
      #     #  key_entry_errors =
      #     #    count( as_tibble( d %>%
      #     #                    filter( nchar(original)>3 ,
      #     #                           effectiveLeaf ) ) ,
      #     #          original ) %>%
      #     #    arrange(-n)
      #     #
      #     # # Default: values where the number happens at least 3 > than
      #     #  # median of the top 10 rows
      #     #  key_entry_errors = key_entry_errors %>%
      #     #    filter(  n > 3 * median(
      #     #      key_entry_errors %>% filter( row_number()<11 )  %>%
      #     #        pull( n ) )
      #     #      ) %>% pull( original )
      #     #
      #     #  # print( head( key_entry_errors ) )
      #     #  if ( is_empty( key_entry_errors )  ) key_entry_errors = NA
      #
      #
      #     cat( '\n - scanning for MAD outliers')
      #     .total = length( key_size( d ) )
      #
      #     .threshold = 50
      #
      #     withProgress(     message = "Searchng",
      #                         detail = "starting ...",
      #                         value = 0, {
      #
      #
      #        data.mad = mad_outliers( d )
      #     })
      #
      #     outlierData$df_data = data1.mad
      #
      #     showModal(
      #           modalDialog( title = "Saving results of scan for extreme values",
      #                        easyClose = TRUE ,
      #                        size = 'm' ,
      #                        footer = "(click anywhere to close dialog box)"
      #                        )
      #           )
      #
      #     # Save data for next time...
      #     cat('\n - saving data1.mad to replace dataset')
      #     saveRDS( data1.mad , paste0( data.folder(), dataset.file() ) )
      #     removeModal()
      #
      #     }
      #
      #
      #     return( data1.mad )
      # })

      # Option to rerun seasonal outliers
      rerunSeasonalModal <- function() {
        ns <- NS(id)
        modalDialog(
          title = "Seasonal outlier flags already present in data",
          # easyClose = TRUE ,
          size = 'm',
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("rerunSeasonal"), "Re-Run Seasonal Outliers")
          )
        )
      }

      observeEvent(input$rerunSeasonal, {
        req(outlierData$df_data)

        searchForSeasonalOutliers(TRUE)
        cat('\n * rerun seasonal outliers button:', searchForSeasonalOutliers())
        removeModal()
      })

      observeEvent(afterMAD(), {
        req(outlierData$df_data)
        cat("\n* observeEvent afterMAD():", afterMAD())
        cat(
          '\n - afterMAD names(outlierData$df_data) ',
          names(outlierData$df_data)
        )

        if (afterMAD()) {
          #   # 2. Scan for seasonally adjusted outliers
          #   if ( 'seasonal5' %in% names( outlierData$df_data ) ){
          #      showModal( rerunSeasonalModal() )
          #
          #   } else {
          # searchForSeasonalOutliers( FALSE )
          # searchForSeasonalOutliers( TRUE )
          # cat('\n - determine seasonal outliers button:' , searchForSeasonalOutliers() )
          # }
        }
      })

      observeEvent(searchForSeasonalOutliers(), {
        req(outlierData$df_data)
        cat(
          '\n* observeEvent searchForSeasonalOutliers: ',
          searchForSeasonalOutliers()
        )

        if (searchForSeasonalOutliers()) {
          # scanForSeasonal( FALSE )
          # scanForSeasonal( TRUE )
          # cat( '\n - searchForSeasonalOutliers names(outlierData$df_data) ' , names(outlierData$df_data) )
          # outlierData$df_data = data1.seasonal()
        }
      })

      # data1.seasonal = reactive({
      #   req( outlierData$df_data )
      #   cat('\n* data1.seasonal')
      #
      #   # if data1 already has seasonal columns, return data1
      #   # if ( !rerunSeasonalOutliers() & 'seasonal3' %in% names( outlierData$df_data ) ){
      #   #   cat('\n - seasonal cols already in data1' )
      #   #   return( outlierData$df_data )
      #   # }
      #
      #
      #   if ( scanForSeasonal()  ){
      #   cat('\n* data1.seasonal search')
      #
      #   # outlierData$df_data = data1.mad()
      #
      #   # Stop if mad10 not in dataset
      #
      #   cat('\n - names(outlierData$df_data)' , names(outlierData$df_data) )
      #   if ( !'mad10' %in% names( outlierData$df_data ) ){
      #
      #     showModal(
      #           modalDialog( title = "Please search for extreme values first",
      #                        easyClose = TRUE ,
      #                        size = 'm' ,
      #                        footer = "(click anywhere to close dialog box)"
      #                        )
      #           )
      #
      #     searchForSeasonalOutliers( FALSE )
      #     return( outlierData$df_data )
      #   }
      #
      #    cat( '\n - scanning for Seasonal outliers')
      #    d = outlierData$df_data
      #   .total = length( key_size( d ) )
      #    cat( '\n - .total' , .total )
      #
      #   .threshold = 50
      #
      #   withProgress(  message = "Seasonal Outliers",
      #                     detail = "starting ...",
      #                     value = 0, {
      #
      #           data1.seasonal = data1.seasonal( d )
      #     })
      #
      #     showModal(
      #           modalDialog( title = "Finished scanning for seasonal values; saving data",
      #                        easyClose = TRUE ,
      #                        size = 'm' ,
      #                        footer = "Click anywhere to close dialog box.  To see results, go to Data page and refresh data selection."
      #                        )
      #           )
      #
      # cat('\n - saving data1.seasonal to replace dataset')
      # cat('\n - names(data1.seasonal):', names(data1.seasonal) )
      #
      #
      # saveRDS( data1.seasonal , paste0( data.folder(), dataset.file() ) )
      # removeModal()
      #
      # # outlierData$df_data = data1.seasonal
      #
      # searchForSeasonalOutliers( FALSE )
      # afterSeasonal( TRUE )
      #   }
      #
      # # return( outlierData$df_data )
      # return( data1.seasonal )
      # })

      # Summary ####

      output$mase.summary <- renderPlot({
        req(selected_data())
        cat('\n* output$mase.summary')

        # d = data1()
        d = selected_data()

        if (!'expected' %in% names(d)) {
          cat('\n - "expected" column not found')
          return()
        }

        reportingSelectedOUs = reportingSelectedOUs()

        # Testing
        # saveRDS( d , 'd.rds')
        # saveRDS( reportingSelectedOUs,  'reportingSelectedOUs.rds' )

        data_mase = d.mase(d, reportingSelectedOUs)

        mase.summary.plot(data_mase, mase.cutpoint = input$maxMASE)

        # }
      })

      # ── Data View: flagged values table + drill-down chart ──────────────────

      flagged_table_data = reactive({
        req(outlier.dataset())
        req(levelNames())

        d_full = as.data.table(outlier.dataset())

        flag_cols = intersect(
          c("key_entry_error", "over_max", "mad15", "mad10", "seasonal5", "seasonal3"),
          names(d_full)
        )
        if (length(flag_cols) == 0) return(NULL)

        # Any-flag and per-series stats computed on the full filtered dataset
        d_full[, any_flag := Reduce(`|`, lapply(.SD, function(x) !is.na(x) & x == TRUE)),
               .SDcols = flag_cols]
        d_full[, series_median   := median(original, na.rm = TRUE), by = .(orgUnit, data.id)]
        d_full[, flags_in_series := sum(any_flag,   na.rm = TRUE),  by = .(orgUnit, data.id)]

        flagged = d_full[any_flag == TRUE]
        if (nrow(flagged) == 0) return(NULL)

        # Ratio: original ÷ series median
        flagged[, ratio := ifelse(
          !is.na(series_median) & series_median > 0,
          round(original / series_median, 1),
          NA_real_
        )]

        # Most-severe flag present in priority order
        fp = intersect(
          c("key_entry_error", "over_max", "mad15", "mad10", "seasonal5", "seasonal3"),
          names(flagged)
        )
        flag_mat = as.matrix(flagged[, ..fp])
        flag_mat[is.na(flag_mat)] = FALSE
        flagged[, flag_type := apply(flag_mat, 1, function(row) {
          idx = which(row)
          if (length(idx) == 0L) NA_character_ else fp[idx[1L]]
        })]

        # Champion indicator — safe: returns NULL if reactive fails/not yet computed
        champion_ous = tryCatch(
          reporting_widget_output$reportingSelectedOUs(),
          error = function(e) NULL
        )
        flagged[, champion := orgUnit %in% champion_ous]

        # Readable period label
        period_col = if ("Month" %in% names(flagged)) "Month" else "Week"
        flagged[, period_label := as.character(get(period_col))]

        # Hierarchy columns for this country instance
        lvl = levelNames()
        hier_cols = intersect(lvl[seq(2, min(5L, length(lvl)))], names(flagged))

        # Sort: champions first, then ratio descending
        setorder(flagged, -champion, -ratio, na.last = TRUE)

        # Build display frame; orgUnit + data.id kept but will be hidden in DT
        display_cols = unique(c(
          hier_cols, "orgUnitName", "data", "period_label",
          "original", "series_median", "ratio",
          "flag_type", "champion", "flags_in_series",
          "orgUnit", "data.id"
        ))
        display_cols = intersect(display_cols, names(flagged))
        as.data.frame(flagged[, ..display_cols])
      })

      output$flagged_dt = DT::renderDT({
        req(flagged_table_data())
        d         = flagged_table_data()
        col_names = names(d)

        # 0-indexed hidden columns (orgUnit, data.id are the last two)
        hidden = which(col_names %in% c("orgUnit", "data.id")) - 1L
        right  = which(col_names %in% c("original", "series_median", "ratio", "flags_in_series")) - 1L

        # Human-readable column headers
        labels = col_names
        labels[labels == "orgUnitName"]     = "Facility"
        labels[labels == "data"]            = "Indicator"
        labels[labels == "period_label"]    = "Period"
        labels[labels == "original"]        = "Value"
        labels[labels == "series_median"]   = "Median"
        labels[labels == "ratio"]           = "Ratio"
        labels[labels == "flag_type"]       = "Flag"
        labels[labels == "champion"]        = "Champion"
        labels[labels == "flags_in_series"] = "# Flags"

        DT::datatable(
          d,
          rownames  = FALSE,
          colnames  = labels,
          selection = "single",
          filter    = "top",
          options   = list(
            scrollY    = "42vh",
            scrollX    = TRUE,
            paging     = TRUE,
            pageLength = 25,
            lengthMenu = list(c(25L, 50L, 100L, -1L), list("25", "50", "100", "All")),
            dom        = "ftip",
            columnDefs = list(
              list(visible = FALSE, targets = as.list(hidden)),
              list(className = "dt-right", targets = as.list(right))
            )
          )
        ) %>%
          DT::formatRound(
            columns = intersect(c("ratio", "series_median"), col_names),
            digits  = 1
          ) %>%
          DT::formatStyle(
            "champion",
            target          = "row",
            backgroundColor = DT::styleEqual(TRUE, "#f0fff4")
          )
      })

      output$drill_chart = plotly::renderPlotly({
        sel = input$flagged_dt_rows_selected
        if (is.null(sel) || length(sel) == 0L) {
          return(
            plotly::plot_ly() %>%
              plotly::layout(
                title = list(
                  text = "Select a row above to view the full time series",
                  font = list(size = 13, color = "#888")
                ),
                xaxis = list(visible = FALSE),
                yaxis = list(visible = FALSE)
              )
          )
        }

        req(data1())
        tbl = flagged_table_data()
        row = tbl[sel, , drop = FALSE]
        ou  = row$orgUnit
        di  = row$data.id

        d_series = as.data.table(data1())[orgUnit == ou & data.id == di]
        if (nrow(d_series) == 0L) return(plotly::plot_ly())

        flag_cols = intersect(
          c("key_entry_error", "over_max", "mad15", "mad10", "seasonal5", "seasonal3"),
          names(d_series)
        )

        period_col = if ("Month" %in% names(d_series)) "Month" else "Week"
        d_series[, t := as.Date(get(period_col))]

        if (length(flag_cols) > 0L) {
          d_series[, any_flag := Reduce(`|`, lapply(.SD, function(x) !is.na(x) & x == TRUE)),
                   .SDcols = flag_cols]
          fm = as.matrix(d_series[, ..flag_cols])
          fm[is.na(fm)] = FALSE
          d_series[, flag_type := apply(fm, 1, function(row) {
            idx = which(row)
            if (length(idx) == 0L) "none" else flag_cols[idx[1L]]
          })]
        } else {
          d_series[, any_flag  := FALSE]
          d_series[, flag_type := "none"]
        }

        med_val = median(d_series$original, na.rm = TRUE)
        d_series[, ratio := ifelse(
          !is.na(med_val) & med_val > 0,
          round(original / med_val, 1),
          NA_real_
        )]
        d_series[, point_color := ifelse(any_flag, "Flagged", "OK")]
        d_series[, tooltip := paste0(
          as.character(get(period_col)),
          "<br>Value: ", original,
          "<br>Ratio: ", ratio,
          "<br>Flag: ", flag_type
        )]

        # Median reference line values
        med_line = data.frame(
          t = range(d_series$t, na.rm = TRUE),
          y = med_val
        )

        plotly::plot_ly() %>%
          # Median reference line
          plotly::add_lines(
            data       = med_line,
            x          = ~t, y = ~y,
            line       = list(color = "steelblue", dash = "dot", width = 1),
            name       = paste0("Median (", round(med_val, 1), ")"),
            hoverinfo  = "skip"
          ) %>%
          # Series line
          plotly::add_lines(
            data       = d_series,
            x          = ~t, y = ~original,
            line       = list(color = "grey70", width = 1),
            showlegend = FALSE,
            hoverinfo  = "skip"
          ) %>%
          # Points coloured by flag status
          plotly::add_markers(
            data      = d_series,
            x         = ~t, y = ~original,
            color     = ~point_color,
            colors    = c("OK" = "#2ecc71", "Flagged" = "#e74c3c"),
            text      = ~tooltip,
            hoverinfo = "text",
            marker    = list(size = 8)
          ) %>%
          plotly::layout(
            title  = list(
              text = paste0(row$orgUnitName, " \u2014 ", row$data),
              font = list(size = 13)
            ),
            xaxis  = list(title = ""),
            yaxis  = list(title = "Value"),
            legend = list(orientation = "h", x = 0, y = -0.2),
            margin = list(t = 40)
          )
      })

      output$profileSummary <- renderUI({
        #describeData()
        cat('\n* profileSummary')
        out <- print(
          dfSummary(data1(), graph.magnif = 0.75),
          style = "grid",
          method = 'render',
          omit.headings = TRUE,
          bootstrap.css = FALSE
        )
        out
      })

      outlier.summary = reactive({
        req(outlier.dataset())

        cat("\n * outlier.summary")

        outlier_dataset = outlier.dataset()

        cat("\n - outlier.dataset has", nrow(outlier_dataset), 'rows')

        # Testing
        # saveRDS( data1 , 'outlier.dataset.rds')

        # data.table?

        d = outlier_dataset %>%
          as_tibble() %>%
          group_by(Month, seasonal3) %>%
          summarise(original = sum(original, na.rm = T)) %>%
          mutate(clean = seasonal3 %in% TRUE) %>%
          group_by(Month, clean) %>%
          summarise(original = sum(original, na.rm = T)) %>%
          pivot_wider(names_from = clean, values_from = original) %>%
          mutate(Raw = `TRUE` + `FALSE`) %>%
          rename(Clean = `TRUE`) %>%
          select(-`FALSE`) %>%
          pivot_longer(cols = c(Clean, Raw))

        cat("\n - outlier.summary has", nrow(d), 'rows')

        # Testing
        # saveRDS( d , 'outlier.summary.rds')

        return(d)
      })

      output$outlier.summary.chart <- renderPlot({
        req(outlier.dataset())

        d = outlier.summary()

        cat('\n * outlier.summary.chart')

        g = d %>%
          ggplot(aes(x = Month, y = value, group = name, color = name)) +
          scale_color_brewer(type = 'qual') +
          geom_line() +
          labs(caption = region_caption_text()) +
          theme_minimal()

        g
      })

      output$monthly_summary_chart <- renderPlot({
        req(outlier.dataset())

        df.ts = data1()

        # Testing
        # saveRDS(df.ts, 'df.ts.rds')

        cat('\n * cleaning_widget.r monthly_summary_chart')

        d = monthly.outlier.summary(df.ts)
        g = outlier.summary.chart(d) +
          labs(caption = region_caption_text())
        g
      })

      # Inspect Outliers #####
      # observeEvent( afterSeasonal() ,{
      #   req( outlierData$df_data )
      #   cat( '\n* observeEvent afterSeasonal')
      #   x = outlier.dataset()
      # })

      outlier.dataset = reactive({
        # Only compute when the Outliers tab is active — this is a heavy 2M+ row
        # operation and there is no need to run it eagerly on data load.
        req(!is.null(current_tab) && current_tab() == "Outliers")
        # req( outlierData$df_data )
        # req( data1() )
        req(selected_data())
        req(input$reporting)
        req(input$startingMonth)
        req(input$endingMonth)
        req(period())

        d = selected_data()

        cat('\n* cleaning_widget outlier.dataset():')
        # if ( is.null( outlierData$df_data ) ){
        #   cat( '\n - is.null( outlierData$df_data )' )
        #   outlierData$df_data  = data1()
        # }

        # d = outlierData$df_data

        # filter date
        # d = d %>%
        #   filter(
        #     Month >= yearmonth( input$startingMonth ) ,
        #     Month <= yearmonth(input$endingMonth )
        #     )
        # d. = as.data.table( outlierData$df_data )

        # d. = as.data.table( data1() )
        d. = as.data.table(selected_data())

        if (!input$reporting %in% "All") {
          d. = d. %>% filter(Selected %in% input$reporting)
        }

        cat('\n - period():', period())
        # cat( "\n - d. class/cols: \n -- ", class( d. ) , "\n -- ", names( d. ))

        # testing
        # saveRDS( d. , "d..rds" )

        if (period() %in% 'Month') {
          # d = d.[ which(
          #   Month >= yearmonth( input$startingMonth ) & Month <= yearmonth(input$endingMonth ) ) ,] %>%
          #   as_tibble

          d = d. %>%
            filter(
              Month >= yearmonth(input$startingMonth) &
                Month <= yearmonth(input$endingMonth)
            ) %>%
            as_tibble

          cat('\n - period is month')
        }

        if (period() %in% 'Week') {
          d = d.[
            which(
              Week >= yearweek(input$startingMonth) &
                Week <= yearweek(input$endingMonth)
            ),
          ] %>%
            as_tibble

          cat('\n - period is week')
        }

        if ('mad10' %in% names(d)) {
          cat('\n - data has mad10')
        }
        if ('seasonal3' %in% names(d)) {
          cat('\n - data has seasonal3')
        }

        # effectiveLeaf is always TRUE for modern DHIS2 downloads; these filters
        # are no-ops on new datasets and are retained for legacy backward compatibility.
        if (
          'effectiveLeaf' %in%
            names(d) &&
            input$selectOrgType %in% 'Facilities only'
        ) {
          cat('\n - data has effectiveLeaf; facilities only')

          d = setDT(d)[effectiveLeaf == TRUE, ]
        } else if (input$selectOrgType %in% 'Admin only') {
          cat('\n - Admin only')
          d = setDT(d)[effectiveLeaf != TRUE, ]
        }

        # Filter by region — driven by the Regions page
        sr <- regions_selected()

        if (!is_empty(sr$level2)) {
          cat('\n - filtering outlier data by', levelNames()[2], "=", sr$level2)
          d = setDT(d)[base::get(levelNames()[2]) %in% sr$level2, , ]
        }

        if (!is_empty(sr$level3)) {
          cat('\n - filtering outlier data by', levelNames()[3], "=", sr$level3)
          d = setDT(d)[base::get(levelNames()[3]) %in% sr$level3, , ]
        }

        if (!is_empty(sr$level4)) {
          cat('\n - filtering outlier data by', levelNames()[4], "=", sr$level4)
          d = setDT(d)[base::get(levelNames()[4]) %in% sr$level4, , ]
        }

        if (!is_empty(sr$level5)) {
          cat('\n - filtering outlier data by', levelNames()[5], "=", sr$level5)
          d = setDT(d)[base::get(levelNames()[5]) %in% sr$level5, , ]
        }

        # filter dataElement
        if (!is.null(selected_data_cats$elements) && length(selected_data_cats$elements) > 0) {
          d = setDT(d)[data %in% selected_data_cats$elements, ] %>% as_tibble()
        } else {
          d = d %>% as_tibble()
        }

        cat('\n - done')
        return(d)
      })

      outlier.summary.cols = reactive({
        req(outlier.dataset())
        cat('\n* outlier.summary.cols():')

        d = outlier.dataset()

        if ('seasonal3' %in% names(d)) {
          cols = c('mad15', 'mad10', 'mad5', 'seasonal5', 'seasonal3')
        } else if ('mad5' %in% names(d)) {
          cols = c('mad15', 'mad10', 'mad5')
        } else {
          cat('\n - no outlier cols found')
          output$outlierSummaryText = renderText({
            'No outlier flags found. Please run the outlier detection algorithms'
          })
          return()
        }

        cat('\n - ', cols)
        output$outlierSummaryText = renderText({
          ''
        })

        return(cols)
      })

      outlier.summary.table = reactive({
        req(outlier.dataset())
        req(outlier.summary.cols())
        req(selected_data_cats$elements)

        cat('\n* cleaning_widget outlier.summary')
        d = outlier.dataset()

        # if not latest outliers set -- please re-run
        if (!'key_entry_error' %in% names(d)) {
          modalDialog(
            title = "The outlier flags in the data are based on older algorithms.\n
                       Please re-run the outlier detection.  ",
            # easyClose = TRUE ,
            size = 'm'
          )
          return()
        }

        os = outlier.summary.tibble(data = d)

        cat('\n - summary has', nrow(os), 'rows')
        return(os)
      })

      output$outlier.summary.table = renderTable(outlier.summary.table())

      ## Visualize cleaning (Inspect )  ####
      errorFlag = reactive({
        req(outlier.dataset())
        req(input$Error)
        req(selected_data_cats$elements)
        cat('\n* errorFlag():')

        # print( head( data1() ) )
        d = outlier.dataset()

        # testing
        # saveRDS( d , 'outlier.dataset.rds')

        # MAD Error
        cat("\n - input$Error:", input$Error)
        if (input$Error %in% names(d)) {
          if (!is.null(selected_data_cats$elements) && length(selected_data_cats$elements) > 0) {
            d = d %>% filter(data %in% selected_data_cats$elements)
          }
          flag = unique(
            as_tibble(d) %>%
              filter(
                !!rlang::sym(input$Error) == FALSE
              ) %>%
              distinct(orgUnit, orgUnitName)
          )
          cat('\n -  nrow errorFlag() :', nrow(flag))
        } else {
          return()
        }

        return(flag)
      })

      observeEvent(errorFlag(), {
        if (nrow(errorFlag()) > 0) {
          updateSelectizeInput(
            session,
            "flaggedOrgUnit",
            choices = paste0(errorFlag()$orgUnitName),
            server = TRUE
          )
        }
      })

      output$ouErrorTable =
        DT::renderDT(
          DT::datatable(
            outlier.dataset() %>%
              as_tibble() %>%
              select(
                data,
                period,
                orgUnitName,
                level,
                original,
                !!rlang::syms(outlier.summary.cols())
              ) %>%
              filter(
                orgUnitName %in% errorFlag()$orgUnitName,
                data %in% selected_data_cats$elements
              ),

            rownames = FALSE,
            filter = 'top',
            options = list(
              # bPaginate = FALSE,
              scrollY = "60vh",
              info = TRUE,
              lengthMenu = list(
                c(-1, 1, 5, 10, 25, 100),
                list('All', '1', '5', '10', '25', '100')
              ),
              server = TRUE
            ),
            fillContainer = TRUE
          )
          # options = DToptions_no_buttons()
        )

      plot.single.data.series = reactive({
        req(outlier.dataset())

        cat('\n* plot.single.data.series')

        if (nrow(errorFlag()) == 0) {
          return()
        }

        inspectOrgUnitData = outlier.dataset() %>%
          as_tibble() %>%
          filter(
            orgUnitName %in% input$flaggedOrgUnit
          )

        if (!input$showAllData && !is.null(selected_data_cats$elements) && length(selected_data_cats$elements) > 0) {
          inspectOrgUnitData = inspectOrgUnitData %>%
            filter(data %in% selected_data_cats$elements)
        }

        cat('\n* inspectOrgUnitData points:', nrow(inspectOrgUnitData))

        g = inspectOrgUnitData %>%
          ggplot(aes(x = Month, y = original, group = data)) +
          geom_line(alpha = .25) +
          geom_point(aes(
            color = !!rlang::sym(input$Error)
            # , shape = seasonal3
          )) +
          labs(
            title = paste(
              unique(inspectOrgUnitData$orgUnitName),
              collapse = ","
            ) +
              theme_minimal()
          )

        cat('\n -done')
        return(g)
      })

      output$inspect = renderPlot({
        plot.single.data.series()
      })

      output$dynamic <- renderUI({
        req(input$plot_hover)
        verbatimTextOutput("vals")
      })

      output$vals <- renderPrint({
        hover <- input$plot_hover
        # print(str(hover)) # list
        y <- nearPoints(outlier.dataset(), input$plot_hover)["original"]
        req(nrow(y) != 0)
        y
      })

      output$info <- renderPrint({
        req(input$plot_hover)
        x <- input$plot_hover$x
        y <- input$plot_hover$y
        # group =
        cat("[", x, ", ", y, "]", sep = "")
      })

      # Return ####
      return(list(
        data2 = reactive({
          outlier.dataset()
        })
      ))
    }
  )
}
