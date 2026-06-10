cleaning_widget_ui = function(id) {
  ns <- NS(id)

  tagList(
    shinybusy::add_busy_spinner(spin = "fading-circle", position = 'bottom-left'),

    sidebarLayout(
      sidebarPanel(
        width = 3,

        tabsetPanel(
          type = "tabs",

          tabPanel(
            "Data Elements",

            tags$p(
              style = "font-size:0.85em; color:#555; margin: 6px 0 4px 0;",
              "Only elements checked in ",
              tags$strong("Reporting → Data Elements"),
              " appear here."
            ),

            actionButton(ns('update_dataElement'), label = "Update",
                         class = "btn-info btn-sm", style = "margin-top:2px;"),

            checkboxInput(ns("collapse_cleaning"), "One row per data element", value = TRUE),

            checkboxInput(ns("select_all_dataElement"), "Select / deselect all", value = TRUE),

            div(
              style = "max-height: calc(100vh - 360px); overflow-y: auto; border: 1px solid #ddd; padding: 4px; border-radius: 4px;",
              checkboxGroupInput(
                ns("dataElement"),
                label = NULL,
                choices = NULL,
                selected = NULL,
                width = "100%"
              )
            ),

            uiOutput(ns("reporting_mismatch_warning"))
          ),

          tabPanel(
            "Reporting Period",

            selectizeInput(
              ns("startingMonth"),
              label = "Beginning with",
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
              choices = c("All", "Champion", "Non-champion"),
              selected = "All"
            )
          )
        )
      ),

      mainPanel(
        width = 9,

        htmlOutput(ns("region_filter_status")),

        tabsetPanel(
          type = "tabs",

          tabPanel(
            "Summary",
            br(),
            textOutput(ns("outlierSummaryText")),
            DT::dataTableOutput(ns("outlier.summary.table")),
            tags$p(style = "font-size:0.9em; color:#555; margin-top:4px;",
                   htmltools::HTML("For outlier procedure details see the <b>About</b> tab.")),
            hr(style = "margin: 6px 0;"),
            plotly::plotlyOutput(ns("outlier_cleaning_chart"), height = "calc(50vh - 80px)")
          ),

          tabPanel(
            "Data View",
            div(
              style = "padding: 6px 12px 2px 12px;",
              tags$small(
                style = "color:#555;",
                "Flagged values only — sorted by champion facilities first, then ratio (value \u00f7 median) descending. ",
                "Click a row to view the full time series for that facility and indicator."
              )
            ),
            div(
              style = "max-height: calc(45vh - 80px); overflow-y: auto;",
              DT::DTOutput(ns("flagged_dt"))
            ),
            hr(style = "margin: 4px 0;"),
            plotly::plotlyOutput(ns("drill_chart"), height = "calc(45vh - 80px)")
          )
        )
      )
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
        ct <- tryCatch(caption.text(), error = function(e) NULL)
        label <- if (!is.null(ct) && nzchar(ct)) ct else {
          sr <- regions_selected()
          parts <- Filter(function(x) !is.null(x) && length(x) > 0,
                          list(sr$level2, sr$level3, sr$level4, sr$level5))
          if (length(parts) == 0) "National" else
            paste(sapply(parts, paste, collapse = ", "), collapse = " / ")
        }
        div(
          style = paste0(
            "background:#e8f4fd; padding:8px 14px;",
            " border-left:4px solid #2196F3; margin:6px 0 10px 0; border-radius:3px;"
          ),
          tags$strong(style = "color:#1565C0; font-size:1.05em;", label)
        )
      })

      region_caption_text = reactive({
        # Use caption.text from reporting_widget — already includes region,
        # facility count, and reporting criteria (X/12 months, date range).
        ct <- tryCatch(caption.text(), error = function(e) NULL)
        if (!is.null(ct) && nzchar(ct)) return(ct)
        # Fallback: region only
        sr <- regions_selected()
        parts <- Filter(function(x) !is.null(x) && length(x) > 0,
                        list(sr$level2, sr$level3, sr$level4, sr$level5))
        if (length(parts) == 0) "National" else
          paste(sapply(parts, paste, collapse = ", "), collapse = " / ")
      })

      selected_data_cats   = reactiveValues(elements = NULL)
      cleaning_elem_map_rv = reactiveVal(list())

      reporting_selected = reactive({
        tryCatch(
          reporting_widget_output$selected_data_categories(),
          error = function(e) NULL
        )
      })

      observeEvent(input$update_dataElement, {
        if (isTRUE(input$collapse_cleaning)) {
          map <- cleaning_elem_map_rv()
          selected_data_cats$elements <- unique(unlist(map[input$dataElement], use.names = FALSE))
        } else {
          selected_data_cats$elements <- input$dataElement
        }
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
          tags$strong(style = "color:#856404;",
            "⚠ Checked here but not enabled in the Reporting tab:"),
          tags$ul(
            style = "margin:4px 0 0 0; padding-left:14px;",
            lapply(missing, tags$li)
          ),
          tags$span(
            style = "color:#856404;",
            "These elements are selected above but were not checked in Reporting, so no data flows through for them. ",
            "Enable them in the Reporting → Data Elements tab to include them here."
          )
        )
      })

      observeEvent(input$select_all_dataElement, {
        req(data1()$data)
        choices <- stringr::str_sort(unique(data1()$data), numeric = TRUE)
        if (isTRUE(input$collapse_cleaning)) {
          map <- cleaning_elem_map_rv()
          updateCheckboxGroupInput(
            session, "dataElement",
            selected = if (input$select_all_dataElement) stringr::str_sort(names(map), numeric = TRUE) else character(0)
          )
        } else {
          updateCheckboxGroupInput(
            session, "dataElement",
            selected = if (input$select_all_dataElement) choices else character(0)
          )
        }
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

      # Build dataElement → data_names map using UID-based matching via data.id
      .cleaning_element_map <- function(choices, fe, d) {
        tryCatch({
          req_cols <- c("data", "data.id")
          fe_cols  <- c("dataElement.id", "dataElement")
          if (!is.null(d) && all(req_cols %in% names(d)) &&
              !is.null(fe) && nrow(fe) > 0 && all(fe_cols %in% names(fe))) {
            id_name <- as.data.frame(d)[, req_cols, drop = FALSE] %>%
              dplyr::distinct() %>%
              dplyr::filter(data %in% choices) %>%
              dplyr::mutate(dataElement.id = sub("_.*$", "", data.id))
            de_map  <- fe %>%
              dplyr::select(dataElement.id, dataElement) %>%
              dplyr::distinct()
            result  <- dplyr::inner_join(id_name, de_map, by = "dataElement.id") %>%
              dplyr::select(dataElement, data) %>%
              dplyr::distinct()
            if (nrow(result) > 0) {
              map <- base::split(result$data, result$dataElement)
              for (ch in setdiff(choices, unlist(map))) map[[ch]] <- ch
              return(map)
            }
          }
        }, error = function(e) NULL)
        setNames(as.list(choices), choices)
      }

      # Central population function for the element checkbox group
      .update_cleaning_checkbox <- function(choices, fe, collapsed, d) {
        map <- .cleaning_element_map(choices, fe, d)
        cleaning_elem_map_rv(map)

        if (!collapsed) {
          roles <- .cleaning_split_by_role(choices, fe)
          updateCheckboxGroupInput(
            session, "dataElement",
            choiceNames  = .cleaning_choice_names(choices, roles$secondary),
            choiceValues = choices,
            selected     = roles$primary
          )
          selected_data_cats$elements <- roles$primary
        } else {
          elems     <- stringr::str_sort(names(map), numeric = TRUE)
          fe_roles  <- if (!is.null(fe) && nrow(fe) > 0 && "role" %in% names(fe))
            setNames(fe$role[match(elems, fe$dataElement)], elems)
          else character(0)
          sec_elems  <- elems[!is.na(fe_roles) & fe_roles == "secondary"]
          prim_elems <- setdiff(elems, sec_elems)
          updateCheckboxGroupInput(
            session, "dataElement",
            choiceNames  = .cleaning_choice_names(elems, sec_elems),
            choiceValues = elems,
            selected     = prim_elems
          )
          selected_data_cats$elements <- unique(unlist(map[prim_elems], use.names = FALSE))
        }
      }

      # data names — populate from data1() so ALL formula elements (primary and
      # secondary) appear in the list.  selected_data() only contains elements
      # that were checked in the reporting widget, so using it here would hide
      # secondary elements entirely.
      # NOTE: do NOT include selected_data() in the trigger — it is not used in
      # the body and its presence forces the entire reporting computation chain
      # to run eagerly whenever data loads.
      observeEvent(data1(), {
        req(data1())
        cat('\n* cleaning_widget observe data1()')
        choices <- stringr::str_sort(unique(data1()$data), numeric = TRUE)
        .update_cleaning_checkbox(choices, formula_elements(), isTRUE(input$collapse_cleaning),
                                  data1())
      })

      observeEvent(input$collapse_cleaning, {
        req(data1())
        choices <- stringr::str_sort(unique(data1()$data), numeric = TRUE)
        .update_cleaning_checkbox(choices, formula_elements(), isTRUE(input$collapse_cleaning),
                                  data1())
      }, ignoreInit = TRUE)

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

      # Auto-scan threshold: datasets larger than this many series are too slow
      # to scan synchronously (R blocks the event loop, so withProgress never
      # renders and the user sees only a spinning wheel).  Large datasets get a
      # notification directing the user to the Outliers tab instead.
      .auto_scan_series_limit <- 200000L

      # observeEvent( input$determineExtremeValues  , {
      observeEvent(data1(), {
        cat(
          '\n* cleaning_widget observeEvent determineExtremeValues.  searchForMAD():',
          searchForMAD()
        )

        x <- data1()
        cat(
          "\n - determineExtremeValues.  mad15 %in% names( outlierData$df_data ):",
          'mad15' %in% names(x)
        )

        if ('mad15' %in% names(x)) {
          # Already scanned — nothing to do
        } else {
          # Count series to decide whether to auto-scan or prompt the user
          n_series <- if (tsibble::is_tsibble(x))
            tsibble::n_keys(x)
          else
            data.table::uniqueN(data.table::as.data.table(x),
                                by = c("orgUnit", "data.id"))

          cat('\n - n_series:', n_series,
              ' limit:', .auto_scan_series_limit)

          if (n_series > .auto_scan_series_limit) {
            # Large dataset: skip auto-scan to avoid blocking the UI for minutes.
            # Notify the user and let them trigger the scan from the Outliers tab.
            showNotification(
              ui = tags$span(
                tags$b(format(n_series, big.mark = ","), " series loaded."),
                " Outlier scan skipped (dataset too large for auto-scan). ",
                "Go to the ", tags$b("Outliers"), " tab to start scanning."
              ),
              type     = "warning",
              duration = 12
            )
            cat('\n - large dataset: skipping auto-scan')
          } else {
            searchForMAD(FALSE)
            searchForMAD(TRUE)
            cat('\n - determine extreme values button:', searchForMAD())
          }
        }
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

          cat('\n - saving data1.seasonal to replace dataset')
          cat('\n - names(data1.seasonal):', names(data1.seasonal))

          saveRDS(data1.seasonal, paste0(data.folder(), dataset.file()))

          showNotification(
            "Outlier scan complete — data saved.",
            type = "message",
            duration = 4
          )

          # Signal data_widget to re-read the saved file (clears rescan_val so
          # dataset() picks up the freshly-scanned file on next access)
          if (!is.null(data_widget_output$scan_done_counter)) {
            data_widget_output$scan_done_counter(
              data_widget_output$scan_done_counter() + 1L
            )
          }
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

      # ── Data View: flagged values table + drill-down chart ──────────────────

      flagged_table_data = reactive({
        req(outlier.dataset())
        req(levelNames())

        d_full = outlier.dataset()

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
                  font = list(size = 16, color = "#888")
                ),
                xaxis = list(visible = FALSE),
                yaxis = list(visible = FALSE),
                paper_bgcolor = "white",
                plot_bgcolor  = "white"
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
              font = list(size = 18, color = "#222")
            ),
            font   = list(size = 16, family = "sans-serif"),
            xaxis  = list(title = "", tickfont = list(size = 14)),
            yaxis  = list(title = "Value", titlefont = list(size = 15),
                          tickfont = list(size = 14)),
            legend = list(orientation = "h", x = 0, y = -0.18,
                          font = list(size = 14)),
            paper_bgcolor = "white",
            plot_bgcolor  = "white",
            margin = list(t = 50, b = 60)
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

        cat('\n* cleaning_widget outlier.dataset():')

        d. = as.data.table(selected_data())

        if (input$reporting != "All") {
          champion_ous <- tryCatch(
            reporting_widget_output$reportingSelectedOUs(),
            error = function(e) NULL
          )
          if (!is.null(champion_ous) && length(champion_ous) > 0) {
            if (input$reporting == "Champion") {
              d. <- d.[orgUnit %in% champion_ous]
            } else if (input$reporting == "Non-champion") {
              d. <- d.[!orgUnit %in% champion_ous]
            }
          }
        }

        cat('\n - period():', period())
        # cat( "\n - d. class/cols: \n -- ", class( d. ) , "\n -- ", names( d. ))

        # testing
        # saveRDS( d. , "d..rds" )

        if (period() %in% 'Month') {
          # Use unclass() to compare as plain integers — avoids vctrs dispatch on
          # yearmonth, which is ~5-10x slower for large vectors.
          .sm_int <- unclass(yearmonth(input$startingMonth))
          .em_int <- unclass(yearmonth(input$endingMonth))
          d <- d.[unclass(Month) >= .sm_int & unclass(Month) <= .em_int]
          cat('\n - period is month')
        }

        if (period() %in% 'Week') {
          .sw_int <- unclass(yearweek(input$startingMonth))
          .ew_int <- unclass(yearweek(input$endingMonth))
          d <- d.[unclass(Week) >= .sw_int & unclass(Week) <= .ew_int]
          cat('\n - period is week')
        }

        if ('mad10' %in% names(d)) {
          cat('\n - data has mad10')
        }
        if ('seasonal3' %in% names(d)) {
          cat('\n - data has seasonal3')
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

        # filter dataElement — stay as data.table; callers use it directly
        if (!is.null(selected_data_cats$elements) && length(selected_data_cats$elements) > 0) {
          d = setDT(d)[data %chin% selected_data_cats$elements]
        } else {
          d = setDT(d)
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

      output$outlier.summary.table = DT::renderDataTable({
        dt <- outlier.summary.table()
        req(nrow(dt) > 0)
        DT::datatable(
          dt,
          rownames = FALSE,
          options = list(
            dom       = 't',
            paging    = FALSE,
            ordering  = FALSE,
            autoWidth = FALSE,
            rowCallback = DT::JS(
              "function(row, data) {",
              "  if (data[0] === 'No Error Flags') {",
              "    $(row).css({'background-color': '#f0f9f0', 'font-weight': 'bold'});",
              "  }",
              "}"
            )
          )
        )
      })

      # Outlier cleaning chart ####
      output$outlier_cleaning_chart <- plotly::renderPlotly({
        d <- tryCatch(outlier.dataset(), error = function(e) NULL)
        if (is.null(d) || nrow(d) == 0) {
          return(plotly::plot_ly() %>%
            plotly::layout(
              title = list(text = "No data — run outlier detection first",
                           font = list(size = 16, color = "#888")),
              paper_bgcolor = "white", plot_bgcolor = "white",
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
            ))
        }

        period_col <- if ("Month" %in% names(d)) "Month" else "Week"
        dt <- d  # outlier.dataset() already returns data.table
        dt[, t := as.Date(get(period_col))]

        # Build cumulative cleaning levels
        flag_levels <- list(
          "Original"         = character(0),
          "Without MAD 15×"  = c("key_entry_error", "over_max", "mad15"),
          "Without MAD 10×"  = c("key_entry_error", "over_max", "mad15", "mad10"),
          "Without Season 5×" = c("key_entry_error", "over_max", "mad15", "mad10", "seasonal5"),
          "Without Season 3×" = c("key_entry_error", "over_max", "mad15", "mad10", "seasonal5", "seasonal3")
        )

        colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")

        p <- plotly::plot_ly()
        for (i in seq_along(flag_levels)) {
          nm    <- names(flag_levels)[i]
          flags <- intersect(flag_levels[[i]], names(dt))
          if (length(flags) == 0) {
            agg <- dt[, .(total = sum(original, na.rm = TRUE)), by = t]
          } else {
            keep <- !Reduce(`|`, lapply(flags, function(f) !is.na(dt[[f]]) & dt[[f]] == TRUE))
            agg  <- dt[keep, .(total = sum(original, na.rm = TRUE)), by = t]
          }
          data.table::setorder(agg, t)
          p <- p %>% plotly::add_lines(
            data       = agg,
            x          = ~t, y = ~total,
            name       = nm,
            line       = list(color = colors[i],
                               width = if (i == 1) 2 else 1.5,
                               dash  = if (i == 1) "solid" else "solid")
          )
        }

        p %>% plotly::layout(
          title    = list(text = "National total — effect of progressive outlier removal",
                          font = list(size = 20, color = "#222")),
          font     = list(size = 17),
          xaxis    = list(title = "", tickfont = list(size = 16)),
          yaxis    = list(title = "Total", titlefont = list(size = 17),
                          tickfont = list(size = 16)),
          legend   = list(orientation = "h", x = 0, y = -0.22,
                          font = list(size = 16)),
          paper_bgcolor = "white",
          plot_bgcolor  = "white",
          margin = list(t = 50, b = 70)
        )
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
