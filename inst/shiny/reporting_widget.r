reporting_widget_ui = function(id) {
  ns <- NS(id)
  # fillCol( height = 600, flex = c(NA ) ,
  tagList(
    shinybusy::add_busy_spinner(
      spin = "fading-circle", # "self-building-square",
      position = 'bottom-right'
      # , margins = c(70, 1200)
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3,
        tabsetPanel(
          type = "tabs",
          selected = "Data Elements",
          tabPanel(
            "Display",

            inputPanel(
              selectInput(
                ns("source"),
                label = "Original/Cleaned",
                choices = c('Original', 'Cleaned'),
                selected = 'Original'
              ),

              selectInput(
                ns("split"),
                label = "Split Data By:",
                choices = "None",
                selected = "None"
              )
            ),

            h5('Filter display dates'),

            inputPanel(
              selectizeInput(
                ns("startDisplayMonth"),
                label = "begining",
                choices = NULL,
                selected = NULL
              ),

              selectizeInput(
                ns("endDisplayMonth"),
                label = "ending",
                choices = NULL,
                selected = NULL
              )
            )
          ),

          tabPanel(
            "Reporting Consistency",

            h5(
              'Find *Champion* facilities  -- the ones that reported the most each year'
            ),

            br(),

            checkboxInput(
              ns("mostReports"),
              label = 'Stratify by consistently reporting facilities (champion)',
              value = TRUE
            ),

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

              checkboxInput(
                ns("exclude_recent_month"),
                label = 'Exclude most recent period? (e.g. current month if data expected to be incomplete)',
                value = TRUE
              ),

              selectizeInput(
                ns("missing_reports"),
                label = "Number of missing reports allowed/yr",
                choices = 0:2,
                selected = 0
              )
            )
          ),

          tabPanel(
            "Datasets",

            h5("Datasets (forms used to enter data)"),

            checkboxInput(
              ns("dataset_merge"),
              label = 'Merge data from all datasets (or choose datasets below)',
              value = FALSE
            ),

            checkboxInput(
              ns("dataset_merge_average"),
              label = 'When merging, average values reported by same facility to multiple datasets',
              value = FALSE
            ),

            selectInput(
              ns("dataSets"),
              label = 'Merge selected datasets with selected dataElements/Categories',
              choices = NULL,
              selected = 1,
              width = "100%",
              multiple = TRUE,
              selectize = TRUE
            )
          ),

          tabPanel(
            "Data Elements",

            p(
              style = "font-size:0.85em; color:#444; margin-top:6px; margin-bottom:2px;",
              "Checked elements count as reporting. ",
              tags$em("See About for details.")
            ),

            radioButtons(
              ns("reporting_rule"),
              label = "Count a facility as reporting when:",
              choices = c(
                "All selected elements — every category present"   = "all_categories",
                "All selected elements — at least one category"    = "all_elements",
                "Any selected element present"                     = "any_selected",
                "Any data element present (including unchecked)"   = "any_data"
              ),
              selected = "any_selected"
            ),

            uiOutput(ns("reporting_rule_hint")),

            div(
              style = "display:flex; align-items:center; gap:6px; margin-bottom:2px;",
              actionButton(ns('update_data_categories'), label = "Update",
                           class = "btn-info btn-sm"),
              checkboxInput(ns("collapse_reporting"), "One row per element", value = TRUE),
              checkboxInput(ns("select_all_categories"), "Select / deselect all", value = FALSE)
            ),

            div(
              style = "max-height: calc(100vh - 580px); min-height: 150px; overflow-y: auto; border: 1px solid #ddd; padding: 4px; border-radius: 4px;",
              checkboxGroupInput(
                ns("data_categories"),
                label = NULL,
                choices = NULL,
                selected = 1,
                width = "100%"
              )
            )
          ) # end tabPanel
        ) # end tabset panel
      ), # end sidebar panel

      mainPanel(
        width = 9,
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Summary",
            style = "height:90vh;",

            fluidPage(
              htmlOutput(ns("region_filter_status")),

              fluidRow(
                style = "height:55vh;",

                column(
                  6,

                  # h5( 'Number of Facilties Reporting Each Period') ,

                  ### Number of Facilties Reporting each Period (plot_reporting_by_month)
                  chartModuleUI(
                    ns('plot_reporting_by_month'),
                    height  = "90%", overlay = TRUE,
                    click = "plot_click", dblclick = "plot_dblclick",
                    hover = "plot_hover", brush = "plot_brush"
                  )
                ),

                column(
                  6,
                  # htmlOutput("x_value") ,

                  # h5( 'Histogram of Periods Reported Each Year') ,

                  ### Histogram of Annual Number of Months Reported (plot_reports_in_a_year)
                  # miniContentPanel(

                  chartModuleUI(
                    ns('plot_reports_in_a_year'),
                    height  = "90%", overlay = TRUE,
                    click = "plot_click", dblclick = "plot_dblclick",
                    hover = "plot_hover", brush = "plot_brush"
                  )

                  # , scrollable = TRUE
                )
                # )

                # , h6( 'Red indicates the facilities that reported each month (*Champions*)' )
              ),

              # h5( "Data Values: On Right, from facilities that reported each month (*Champions*); on left, from all others")  ,

              # fluidRow( style = "height:10vh;",
              #
              #           column(6, h5( "Data Values from Inconsistenly Reporting Facilities" ) ) ,
              #
              #           column(6, h5( "Data Values from *Champion* Facilities" ) )
              #
              # ) ,

              fluidRow(
                column(
                  3,
                  selectInput(
                    ns("series_by"),
                    label = "Color series by:",
                    choices = c("None", "Dataset", "Category"),
                    selected = "None"
                  )
                ),
                column(
                  3,
                  selectInput(
                    ns("facet_by"),
                    label = "Facet chart by:",
                    choices = c("Champion/Non-Champion", "All Facilities"),
                    selected = "Champion/Non-Champion"
                  )
                )
              ),

              fluidRow(
                style = "height:40vh;",

                column(
                  12,
                  chartModuleUI(
                    ns('plot_values'),
                    height  = "80%", overlay = TRUE,
                    hover = "plot_hover", brush = "plot_brush"
                  )
                )
              )
            )
          ),

          tabPanel(
            "Facilities",

            # tableOutput( ns('orgUnitReportingTable'))

            # miniPage(
            # gadgetTitleBar( "Shiny gadget example" ),

            # miniTabstripPanel(
            tabsetPanel(
              type = "tabs",

              tabPanel(
                "Chart",
                style = "height:80vh;",
                # miniTabPanel( "Visualize" , icon = icon("area-chart"),
                # miniContentPanel(
                chartModuleUI(ns("facility_chart"), height = "100%", overlay = TRUE)
                # )
              ),
              tabPanel(
                "Map",
                style = "height:90vh;",
                # miniTabPanel( ns("Map") ,
                # icon = icon("map-o"),

                # miniContentPanel(padding = 0,
                # leafletOutput( ns("map") , height = "100%")
                # ),
                # miniButtonBlock(
                # actionButton( ns("resetMap") , "Reset" )
                # )

                fluidRow(
                  style = "height:80vh;",
                  column(12, leafletOutput(ns("facility_map"), height = "100%"))
                )
              ),
              tabPanel(
                "Data",
                style = "height:80vh; overflow: hidden;",
                DT::dataTableOutput(ns("facility_table"))
              )
            )
          )
        )
      ) # end main panel
    ) # end sidbar layout
  ) # end tagset
}

reporting_widget_server <- function(
  id,
  dataDirectory = NULL,
  metadata_widget_output = NULL,
  data_widget_output = NULL,
  cleaning_widget_output = NULL,
  regions_widget_output = NULL,
  current_tab = NULL
) {
  moduleServer(
    id,
    function(
      input,
      output,
      session
      # dataDirectory = dataDirectory,
      # metadata = metadata_widget_output ,
      # data.details = data_widget_output
    ) {
      options(shiny.trace = FALSE)
      options(shiny.reactlog = FALSE)
      options(dplyr.summarise.inform = FALSE)

      # ── Reporting-rule contextual hint ──────────────────────────────────────
      output$reporting_rule_hint <- renderUI({
        rule <- input$reporting_rule
        if (is.null(rule)) return(NULL)

        txt <- switch(rule,
          all_categories = tagList(
            tags$strong("Use for ratios"), " (e.g. test positivity rate). ",
            "Both numerator and denominator must be present; a facility missing ",
            "either component produces an undefined ratio and should not ",
            "anchor the champion distribution."
          ),
          all_elements = tagList(
            tags$strong("Use with disaggregated elements"), " where partial ",
            "category reporting is acceptable — e.g. age/sex sub-totals may not ",
            "always be complete but the element-level total is present."
          ),
          any_selected = tagList(
            tags$strong("General-purpose default."),
            " A facility that submitted any one of the checked elements is ",
            "credited as reporting. Suitable when all checked elements are ",
            "meaningful indicators of facility activity."
          ),
          any_data = tagList(
            tags$strong("Use when zero cases may be stored as missing."),
            " A facility submitting no case entries but reporting attendance or ",
            "fever counts is still active. Including a surrogate element ",
            "(e.g. outpatient attendance) ensures these facilities are ",
            "classified as champions."
          )
        )

        div(
          style = paste0(
            "background:#f0f4ff; border-left:3px solid #6c8ebf;",
            " padding:4px 7px; margin:2px 0 6px 0;",
            " font-size:0.80em; color:#333; border-radius:0 3px 3px 0;",
            " max-height:90px; overflow-y:auto;"
          ),
          txt,
          tags$small(
            style = "display:block; margin-top:3px; color:#666;",
            tags$em("See About tab for full explanation and examples.")
          )
        )
      })

      # cat('\n**Starting Reporting Widget\n')

      data.folder = reactive({
        dataDirectory$directory()
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
      dataset = reactive({
        data_widget_output$dataset()
      })
      data1 = reactive({
        data_widget_output$data1()
      })
      formula_elements = reactive({
        data_widget_output$formula_elements()
      })
      orgUnits = reactive({
        metadata_widget_output$orgUnits()
      })
      orgUnitLevels = reactive({
        metadata_widget_output$orgUnitLevels()
      })
      geoFeatures = reactive({
        metadata_widget_output$geoFeatures()
      })
      data2 = reactive({
        cleaning_widget_output$data2()
      })

      # Debounced versions of the champion-window date inputs so that rapid
      # selectize interactions don't fire mostFrequentReportingOUs() repeatedly.
      # These are also exported from the module so evaluation_widget benefits.
      startingMonth_debounced = reactive(input$startingMonth) |> debounce(600)
      endingMonth_debounced   = reactive(input$endingMonth)   |> debounce(600)

      # Flag: has the user visited the Reporting tab at least once?
      # selected_data() and reportingSelectedOUs() are expensive; this flag
      # keeps them dormant during initial data load.  Once TRUE it never goes
      # back to FALSE within a dataset session, so it does NOT create a
      # per-tab-switch invalidation loop.
      # Reset to FALSE on dataset file change so loading a new dataset does not
      # immediately trigger expensive computations from a prior session visit.
      hasVisitedReporting = reactiveVal(FALSE)
      observeEvent(current_tab(), {
        if (isTRUE(current_tab() == "Reporting")) hasVisitedReporting(TRUE)
      }, ignoreInit = TRUE)
      observeEvent(data_widget_output$dataset.file(), {
        hasVisitedReporting(FALSE)
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      # see https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
      shift_legend3 <- function(p) {
        pnls <- cowplot::plot_to_gtable(p) %>%
          gtable::gtable_filter("panel") %>%
          with(setNames(grobs, layout$name)) %>%
          purrr::keep(~ identical(.x, zeroGrob()))

        if (length(pnls) == 0) {
          return(p)
        }

        lemon::reposition_legend(p, "center", panel = names(pnls))
      }

      dates = reactive({
        req(data1())
        req(period())
        cat('\n* reporting_widget dates:')

        cat('\n -data1 class:', class(data1()))
        .period = period()

        if (!.period %in% names(data1())) {
          cat('\n -- dataset does not contain column:', .period)
          cat('\n - dataset columns are:', names(data1()))
          return()
        }

        # data.table?
        dates = data1() %>%
          ungroup %>%
          distinct(!!rlang::sym(.period)) %>%
          arrange(!!rlang::sym(.period)) %>%
          pull(!!rlang::sym(.period))

        cat('\n - min:', min(dates), ', max:', max(dates))

        cat('\n - done')
        return(dates)
      })

      # reporting_month selection
      observeEvent(dates(), {
        cat('\n - reporting_widget observeEvent dates() update startingMonth-')
        dates = dates()
        updateSelectizeInput(
          session,
          'startingMonth',
          choices = dates %>% as.character(),
          selected = min(dates, na.rm = TRUE) %>% as.character(),
          server = TRUE
        )

        updateSelectizeInput(
          session,
          'startDisplayMonth',
          choices = dates %>% as.character(),
          selected = min(dates, na.rm = TRUE) %>% as.character(),
          server = TRUE
        )

        cat('\n - reporting_widget observeEvent dates() update endingMonth-')
        updateSelectizeInput(
          session,
          'endingMonth',
          choices = dates %>% as.character(),
          selected = (max(dates, na.rm = TRUE) - 1) %>% as.character(),
          server = TRUE
        )

        updateSelectizeInput(
          session,
          'endDisplayMonth',
          choices = dates %>% as.character(),
          selected = max(dates, na.rm = TRUE) %>% as.character(),
          server = TRUE
        )

        cat('-done\n')
      })

      # Update data
      observe({
        req(dataSets())
        cat('\n* updating merge dataSets input')
        cat('\n - dataSets()', dataSets())

        if (any(nchar(dataSets() > 0))) {
          updateSelectInput(session, 'dataSets', choices = dataSets())
        }
      })

      observeEvent(input$dataset_merge, {
        cat('\n* updating merge dataSets to all')
        req(dataSets())
        cat('\n - dataSets:', dataSets())

        if (input$dataset_merge == TRUE) {
          if (any(nchar(dataSets() > 0))) {
            updateSelectInput(
              session,
              'merge',
              choices = dataSets(),
              selected = dataSets()
            )
          }
        } else {
          updateSelectInput(
            session,
            'merge',
            choices = dataSets(),
            selected = NULL
          )
          cat('\n - done')
        }
      })

      #   observEvent({
      #     cat( '\n - updating data_choices' )
      #     updateSelectInput( session, 'data_categories' ,
      #                        choices =   unique( data1()$data ) ,
      #                        selected = 1 )
      #     cat( '\n - done' )
      # } )

      selected_data_categories = reactiveValues()

      selected_org_levels = reactiveValues(
        level2 = NULL,
        level3 = NULL,
        level4 = NULL,
        level5 = NULL
      )

      # Sync region selection from the Regions widget
      regions_selected = reactive({
        if (!is.null(regions_widget_output)) regions_widget_output$selected_regions() else list()
      })

      observeEvent(regions_selected(), {
        sr <- regions_selected()
        selected_org_levels$level2 <- sr$level2
        selected_org_levels$level3 <- sr$level3
        selected_org_levels$level4 <- sr$level4
        selected_org_levels$level5 <- sr$level5
        cat('\n* reporting_widget synced region filter from Regions page')
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

      # Internal helpers
      .split_by_role <- function(choices, fe) {
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

      .make_choice_names <- function(choices, secondary) {
        lapply(choices, function(ch) {
          if (ch %in% secondary)
            tags$span(ch, tags$em(" (secondary)", style = "color:#999; font-size:0.85em;"))
          else
            ch
        })
      }

      # Build dataElement → data_names map using UID-based matching via data.id
      .reporting_element_map <- function(choices, fe, d) {
        tryCatch({
          req_cols <- c("data", "data.id")
          fe_cols  <- c("dataElement.id", "dataElement")
          if (!is.null(d) && all(req_cols %in% names(d)) &&
              !is.null(fe) && nrow(fe) > 0 && all(fe_cols %in% names(fe))) {
            id_name <- data.table::as.data.table(d)[
                data %in% choices, .(data, data.id)
              ] |>
              unique() |>
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

      reporting_elem_map_rv <- reactiveVal(list())

      # Auto-select primary elements only when there are few (≤ .max_auto_select).
      # With many elements (e.g. PDR Lao: 82) start empty so the user chooses
      # deliberately — same threshold logic as the DQA page.
      .max_auto_select <- 5L

      .update_data_categories <- function(all_choices, fe, d) {
        map <- .reporting_element_map(all_choices, fe, d)
        reporting_elem_map_rv(map)

        has_secondary <- !is.null(fe) && nrow(fe) > 0 && "role" %in% names(fe) &&
          any(fe$role == "secondary", na.rm = TRUE)

        if (!isTRUE(input$collapse_reporting)) {
          roles <- .split_by_role(all_choices, fe)
          sel   <- if (length(roles$primary) > .max_auto_select) character(0) else roles$primary
          updateCheckboxGroupInput(
            session, 'data_categories',
            choiceNames  = .make_choice_names(all_choices, roles$secondary),
            choiceValues = all_choices,
            selected     = sel
          )
          selected_data_categories$elements <- sel
        } else {
          elems     <- stringr::str_sort(names(map), numeric = TRUE)
          fe_roles  <- if (!is.null(fe) && nrow(fe) > 0 && "role" %in% names(fe))
            setNames(fe$role[match(elems, fe$dataElement)], elems)
          else character(0)
          sec_elems  <- elems[!is.na(fe_roles) & fe_roles == "secondary"]
          prim_elems <- setdiff(elems, sec_elems)
          sel        <- if (length(prim_elems) > .max_auto_select) character(0) else prim_elems
          updateCheckboxGroupInput(
            session, 'data_categories',
            choiceNames  = .make_choice_names(elems, sec_elems),
            choiceValues = elems,
            selected     = sel
          )
          selected_data_categories$elements <- unique(unlist(map[sel], use.names = FALSE))
        }

        if (has_secondary) {
          updateRadioButtons(session, 'reporting_rule', selected = 'any_selected')
          showNotification(
            "Secondary element(s) detected — reporting rule set to 'Any selected element present'.",
            type = "message", duration = 5
          )
        }
      }

      observeEvent('data' %in% names(data1()), {
        req(data1()$data)
        cat('\n* updating data_categories with primary/secondary distinction')
        .update_data_categories(stringr::str_sort(unique(data1()$data), numeric = TRUE), formula_elements(), data1())
        cat('\n - done')
      })

      observeEvent(formula_elements(), {
        req(data1()$data)
        req(formula_elements())
        cat('\n* reporting_widget: formula_elements changed, refreshing data_categories')
        .update_data_categories(stringr::str_sort(unique(data1()$data), numeric = TRUE), formula_elements(), data1())
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      observeEvent(input$collapse_reporting, {
        req(data1()$data)
        .update_data_categories(stringr::str_sort(unique(data1()$data), numeric = TRUE), formula_elements(), data1())
      }, ignoreInit = TRUE)

      observeEvent(input$update_data_categories, {
        if (isTRUE(input$collapse_reporting)) {
          map <- reporting_elem_map_rv()
          selected_data_categories$elements <- unique(unlist(map[input$data_categories], use.names = FALSE))
        } else {
          selected_data_categories$elements <- input$data_categories
        }
      })

      observeEvent(input$select_all_categories, {
        req(data1()$data)
        choices <- stringr::str_sort(unique(data1()$data), numeric = TRUE)
        if (isTRUE(input$collapse_reporting)) {
          map <- reporting_elem_map_rv()
          updateCheckboxGroupInput(
            session, 'data_categories',
            selected = if (input$select_all_categories) stringr::str_sort(names(map), numeric = TRUE) else character(0)
          )
        } else {
          updateCheckboxGroupInput(
            session, 'data_categories',
            selected = if (input$select_all_categories) choices else character(0)
          )
        }
      }, ignoreInit = TRUE)

      # update split
      observeEvent(data1(), {
        req(data1())
        updateSelectInput(session, 'split', choices = c('None', names(data1())))
      })

      dataSets = reactive({
        req(data1())
        cat('\n* reporting_widget dataSets:')
        # cat('\n - data1 class:' , class( data1()) )

        if (is_empty(data1())) {
          cat('\n - data1() is empty')
          return()
        }

        if (!'dataSet' %in% names(data1())) {
          message('dataSet not in names( dataset) ')
          cat('\n names( data1()) :', names(data1()))
          return()
        }

        # x = setDT( data1() )[ !is.na( dataSet ) , dataSet , ] %>%
        # unique

        #testing
        # saveRDS( data1() , "data1.rds" )

        x = data1()[!is.na(data1()$dataSet), ]$dataSet %>% unique

        cat('\n - there are', length(x), 'dataSets')
        cat('\n - \n, ', x)

        return(x)
      })

      # Months and periods
      period = reactive({
        req(data1())

        cat('\n* reporting_widget period():')

        period = dataPeriod(data1())

        cat('\n - period: ', period)
        return(period)
      })

      most_recent_period = reactive({
        req(data1())
        req(period())
        cat('\n* reporting_widget Looking for most recent', period())
        # cat('\n - data1 class:' , class( data1()) )

        # data.table?
        mrp = max(
          data1() %>%
            pull(!!rlang::sym(period())),
          na.rm = TRUE
        )
        # mrp = max( data1()[ , 'Month'] , na.rm = TRUE )
        # mrp = max( data1()$Month , na.rm = TRUE )
        cat('\n - most recent', period(), 'is', mrp)

        if (input$exclude_recent_month) {
          cat('\n - exclude most recent period')
          if (period() == "Month") {
            mrp = mrp - month(1)
          }
          if (period() == "Week") mrp = mrp - 1
        }

        cat('\n - mrp:', mrp)
        return(mrp)
      })

      # d Filter data1 to selected level ####
      d = reactive({
        req(data1())
        req(period())
        cat('\n* reporting_widget d:')
        .t0_d <- proc.time()["elapsed"]

        # Testing
        # saveRDS( data1() , 'dataset.rds' )
        # cat( "\n - reporting_widget data1() class/cols:" , class( data1() ) )

        if (nrow(data1()) == 0) {
          cat('\n - data1() has zero rows')
          return()
        }

        # data1() is now a data.table (converted once in data_widget).
        # Using it directly avoids a ~300ms copy on every d() evaluation.
        data = data1()
        cat(sprintf('\n - data1() assigned: %.1f sec', proc.time()["elapsed"] - .t0_d))

        cat('\n - data (d) is data.table')

        .period = period()
        cat('\n - period is', .period)

        data = data[, period := base::get(.period), ]
        # data = data1()  %>% mutate( period = !!rlang::sym( .period ))

        # cat( "\n - reporting_widget data class/cols:" ,class( data ) )
        # cat( "\n - reporting_widget data1() class/cols:" , class( data1() ) )

        if (!is_empty(selected_org_levels$level2)) {
          cat(
            '\n - filtering data by',
            levelNames()[2],
            "=",
            selected_org_levels$level2
          )

          # data = data %>%
          #   filter( !! rlang::sym( levelNames()[2])  %in%   input$level2  )

          # cat(  '\n - data was' , class(data) )

          data = setDT(data)[
            base::get(levelNames()[2]) %in% selected_org_levels$level2,
            ,
          ]

          # cat(  '\n - and now is' , class(data) )

          #print( paste( 'data filtered by level2 has' , nrow( data ), 'rows' ))
          # glimpse( data )
        }

        if (!is_empty(selected_org_levels$level3)) {
          cat(
            '\n - filtering data by',
            levelNames()[3],
            "=",
            selected_org_levels$level3
          )

          data = setDT(data)[
            base::get(levelNames()[3]) %in% selected_org_levels$level3,
            ,
          ]

          # data = data %>%
          #   filter( !! rlang::sym( levelNames()[3])  %in%   input$level3  )

          #print( paste( 'data filtered by level3 has' , nrow( data ), 'rows' ))
          # glimpse( data )
        }

        if (!is_empty(selected_org_levels$level4)) {
          cat(
            '\n - filtering data by',
            levelNames()[4],
            "=",
            selected_org_levels$level4
          )

          data = setDT(data)[
            base::get(levelNames()[4]) %in% selected_org_levels$level4,
            ,
          ]

          # data = data %>%
          #   filter( !! rlang::sym( levelNames()[4])  %in%   input$level4  )

          #print( paste( 'data filtered by level4 has' , nrow( data ), 'rows' ))
          # glimpse( data )
        }

        if (!is_empty(selected_org_levels$level5)) {
          cat(
            '\n - filtering data by',
            levelNames()[5],
            "=",
            selected_org_levels$level5
          )

          data = setDT(data)[
            base::get(levelNames()[5]) %in% selected_org_levels$level5,
            ,
          ]

          # data = data %>%
          #   filter( !! rlang::sym( levelNames()[5])  %in%   input$level5  )

          #print( paste( 'data filtered by level4 has' , nrow( data ), 'rows' ))
          # glimpse( data )
        }

        cat('\n - nrow( d ):', nrow(data))

        # Always filter to leaf (facility) level.
        # effectiveLeaf is always TRUE for modern DHIS2 downloads; the all()
        # check is a fast no-op in that case. Filter retained for legacy files.
        if (!isTRUE(all(data$effectiveLeaf))) {
          data = setDT(data)[effectiveLeaf == TRUE, , ]
        }

        # if ( input$exclude_recent_month ) data = data %>%
        #   filter( !! rlang::sym( period() ) <= most_recent_period() )

        if (input$source %in% 'Original') {
          cat('\n - d() source is original')

          # data = data %>% mutate( dataCol = original )
          .t0_dc <- proc.time()["elapsed"]
          data = setDT(data)[, dataCol := as.numeric(original), ]
          cat(sprintf('\n - dataCol := as.numeric(original): %.1f sec', proc.time()["elapsed"] - .t0_dc))
        }

        if (input$source %in% 'Cleaned' & 'seasonal3' %in% names(data)) {
          cat(
            '\n -',
            paste(
              'Cleaning removes',
              sum(data$value, na.rm = T) - sum(data$seasonal3, na.rm = T),
              'data points'
            )
          )

          # data = data %>%
          #   mutate( dataCol = ifelse( seasonal3, original, NA  ) )

          data = setDT(data)[,
            dataCol := fifelse(seasonal3, original, NA_real_),
          ]

          # Modify variables used for cleaning data so that FALSE when NA -- meaning it failed prior cleaning step, and TRUE means data is ok
          if ('mad15' %in% names(data)) {
            # data = data %>% mutate( mad15 = ifelse( value & is.na( mad15)|!mad15, FALSE, TRUE ) )
            data = setDT(data)[,
              mad15 := fifelse(value & is.na(mad15) | !mad15, FALSE, TRUE),
            ]
          }

          if ('mad10' %in% names(data)) {
            # data = data %>% mutate( mad10 = ifelse( value & is.na( mad10)|!mad10, FALSE, TRUE ) )
            data = setDT(data)[,
              mad10 := fifelse(value & is.na(mad10) | !mad10, FALSE, TRUE),
            ]
          }

          if ('mad5' %in% names(data)) {
            # data = data %>% mutate( mad5 = ifelse( value & is.na( mad5)|!mad5, FALSE, TRUE ) )
            data = setDT(data)[,
              mad5 := fifelse(value & is.na(mad5) | !mad5, FALSE, TRUE),
            ]
          }

          if ('seasonal5' %in% names(data)) {
            # data = data %>% mutate( seasonal5 = ifelse( value & is.na( seasonal5)|!seasonal5, FALSE, TRUE ) )
            data = setDT(data)[,
              seasonal5 := fifelse(
                value & is.na(seasonal5) | !seasonal5,
                FALSE,
                TRUE
              ),
            ]
          }

          if ('seasonal3' %in% names(data)) {
            # data = data %>% mutate( seasonal3 = ifelse( value & is.na( seasonal3)|!seasonal3, FALSE, TRUE ) )
            data = setDT(data)[,
              seasonal3 := fifelse(
                value & is.na(seasonal3) | !seasonal3,
                FALSE,
                TRUE
              ),
            ]
          }

          cat(
            '\n -',
            paste(
              'cleaning changes total by',
              sum(data$original, na.rm = T) - sum(data$dataCol, na.rm = T)
            )
          )
        }

        # #print( 'd: max period ' ); #print( max( d$period ))

        cat('\n - d: max period: ', max(data$period, na.rm = TRUE))
        # #print( max( data$Month , na.rm = TRUE ))

        cat(sprintf('\n - end d()  %.1f sec  %d rows', proc.time()["elapsed"] - .t0_d, nrow(data)))
        # cat( "\n - reporting_widget d() class/cols: \n -- " , class( data ) , "\n -- " , names( data ))

        # testing — uncomment to capture d() for profvis; re-comment after
        # if (!file.exists('reporting_widget_d.rds')) saveRDS(data, 'reporting_widget_d.rds')

        return(data)
      })

      #  Reports ####

      orgunit.reports = reactive({
        req(selected_data_categories$elements)
        req(most_recent_period())
        req(period())

        cat('\n* reporting_widget orgunit.reports()')
        .t0_orgunit_reports <- proc.time()["elapsed"]

        mrm = most_recent_period()

        year_var = 'calendar_year' # ifelse( input$calendar_year , 'calendar_year' , 'months12' )

        data = d()

        if (input$reporting_rule != "any_data") {
          data = setDT(data)[data %chin% selected_data_categories$elements, , ]
        }

        .period = period()

        # Fast path: add year column, take unique (year, orgUnit, period) triples,
        # then count .N per (year, orgUnit).  Avoids creating per-group .SD objects
        # (which takes ~10s for 191K groups on 5.2M rows).
        # Add year in-place on d()'s cached data.table — avoids a ~530ms full copy.
        # The year column persists in the cache across calls, which is safe and saves
        # recomputation when both orgunit.reports and orgunit.monthly.reports run.
        if (!'year' %in% names(data)) data[,
          year := data.table::year(as.IDate(unclass(.SD[[1L]]), origin = "1970-01-01")),
          .SDcols = .period]
        o.r. = data

        # Use integer key for period to bypass vctrs dispatch in unique() —
        # yearmonth/yearweek stored as integers but unique.data.table falls
        # back to R-level comparison on vctrs objects (~10x slower).
        o.r. = unique(o.r.[, .(year, orgUnit, .period_int = unclass(get(.period)))])[
          , .(n_periods = .N), by = c('year', 'orgUnit')
        ][,
          `:=`(n_periods = factor(n_periods), year = factor(year))
        ] |> as_tibble()

        cat(sprintf('\n - end orgunit.reports  %.1f sec', proc.time()["elapsed"] - .t0_orgunit_reports))
        return(o.r.)
      })

      annual.reports = reactive({
        req(orgunit.reports())
        #print( 'annual reports()' )

        or = orgunit.reports()
        # #print( 'annual reports() or:' ); #print( names(or))

        ar = setDT(or)[,
          .(n = uniqueN(orgUnit)),
          by = c('year', 'n_periods')
        ] %>%
          as_tibble()
        # %>%
        #   group_by( year ,  n_periods ) %>%
        #   summarise( n = n() )

        #print( 'end annual reports' )
        return(ar)
      })

      orgunit.monthly.reports = reactive({
        req(selected_data_categories$elements)
        cat('\n* reporting_widget orgunit.monthly.reports():')
        .t0_omr <- proc.time()["elapsed"]

        # mrp = most_recent_period()
        .period = period()

        year_var = 'calendar_year' # ifelse( input$calendar_year , 'calendar_year' , 'months12' )

        data = d()
        cat('\n - o.m.r data has', nrow(data), 'rows')

        if (input$reporting_rule != "any_data") {
          data = data[data %chin% selected_data_categories$elements]
        }

        # Add year in-place — avoids ~530ms copy (same pattern as orgunit.reports).
        if (!'year' %in% names(data)) data[,
          year := data.table::year(as.IDate(unclass(.SD[[1L]]), origin = "1970-01-01")),
          .SDcols = .period]
        o.m.r = data
        # %>%
        # mutate( year = factor( year ) )

        cat(sprintf('\n - end orgunit.monthly.reports  %.1f sec  %d rows', proc.time()["elapsed"] - .t0_omr, nrow(o.m.r)))
        return(o.m.r)
      })

      monthly.reports = reactive({
        req(orgunit.monthly.reports())
        req(period())
        #print( 'monthly reports():' )

        .period = period()

        o.m.r = orgunit.monthly.reports()

        # #print('monthly.reports() o.m.r'); #print( names(o.m.r) )

        # Use integer surrogate for period column to avoid vctrs dispatch in groupby.
        # yearmonth/yearweek are stored as integers under a vctrs class; restore the
        # class after aggregation so the plot axis ticks render correctly.
        .period_class = class(o.m.r[[.period]])
        m.r = o.m.r[, .(year, orgUnit, .pint = unclass(get(.period)))][
          , .(n = uniqueN(orgUnit)), by = c("year", ".pint")
        ]
        setnames(m.r, ".pint", .period)
        m.r[[.period]] = structure(m.r[[.period]], class = .period_class)
        m.r = as_tibble(m.r)
        # group_by( year , !! rlang::sym( .period )   ) %>%
        # summarise( n = n_distinct( orgUnit ) )

        # #print('m.r') ; glimpse(m.r)
        #print( 'end monthly reports()' )
        # glimpse( m.r)
        return(m.r)
      })

      facilities = reactive({
        #print( 'facilities' )
        req(orgunit.reports())

        f = orgunit.reports() %>%
          ungroup()

        f = setDT(f)[, .(Total = uniqueN(orgUnit)), ] %>%
          as_tibble() %>%
          # summarise( Total = n_distinct( orgUnit ) ) %>%
          pull(Total)

        #print( 'end facilities' )
        return(f)
      })

      # Levels ####
      observe({
        updateSelectInput(
          session,
          'agg_level',
          choices = levelNames(),
          selected = levelNames()[1]
        )

        updateSelectInput(session, 'level', choices = c('leaf', levelNames()))
      })

      # level 2
      observeEvent(data1(), {
        if (nrow(data1()) > 0 && 'level' %in% names(data1())) {
          cat('\n* reporting_widget updating level2')
          # Use data.table for fast unique extraction on large datasets
          choices <- data.table::setDT(data1())[[levelNames()[2]]] |>
            unique() |>
            stringr::str_sort()
          updateSelectInput(session, 'level2', choices = choices, selected = NULL)
        }
      })

      # level 3
      observe({
        #Event( data1()  , {
        req(selected_org_levels$level2)
        if (nrow(data1()) > 0 && 'level' %in% names(data1())) {
          cat('\n* reporting_widget updating level3')

          ls = setDT(data1())[
            base::get(levelNames()[2]) %in% selected_org_levels$level2,
            base::get(levelNames()[3]),
          ] %>%
            unique %>%
            str_sort()

          cat("\n - level3 update to:", paste(ls, collapse = ""))

          updateSelectInput(
            session,
            'level3',

            choices = ls,
            # data1() %>%
            #   filter(
            #   !! rlang::sym( levelNames()[2] ) %in% input$level2 ) %>%
            #   pull( !! rlang::sym( levelNames()[3]  ) ) %>%
            #   unique %>% str_sort() ,

            selected = NULL
          )
        }
      })

      # level 4
      observe({
        #Event( data1()  , {
        req(input$level3)
        if (nrow(data1()) > 0 && 'level' %in% names(data1())) {
          cat('\n* reporting_widget updating level4')

          ls = setDT(data1())[
            base::get(levelNames()[3]) %in% input$level3,
            base::get(levelNames()[4]),
          ] %>%
            unique %>%
            str_sort()

          updateSelectInput(
            session,
            'level4',
            choices = ls,
            # data1() %>%
            #   filter(
            #   !! rlang::sym( levelNames()[3] ) %in% input$level3 ) %>%
            #   pull( !! rlang::sym( levelNames()[4]  ) ) %>%
            #           unique %>% str_sort(),
            selected = NULL
          )
        }
      })

      level5 = reactive({
        req(input$level4)
        req(levelNames())
        req(data1())
        cat('\n* reporting_widget level5:')
        # cat('\n - data1 class:' , class( data1()) )

        if (is_empty(data1())) {
          return(NA)
        }
        if (is.na(levelNames()[5])) {
          return(NA)
        }

        ls = setDT(data1())[
          base::get(levelNames()[4]) %in% input$level4,
          base::get(levelNames()[5]),
        ] %>%
          unique %>%
          str_sort()

        return(ls)
        # data1() %>%
        #     filter(
        #         !! rlang::sym( levelNames()[4] ) %in%
        #                    input$level4 ) %>%
        #     pull( !! rlang::sym( levelNames()[5]  ) ) %>%
        #     unique %>% str_sort()
      })

      observe({
        if (nrow(data1()) > 0 && 'level' %in% names(data1())) {
          updateSelectInput(
            session,
            'level5',
            choices = level5(),
            selected = NULL
          )
        }
      })

      levelNames = reactive({
        req(orgUnits())
        cat('\n* reporting_widget levelNames():')

        l = getLevelNames(orgUnits())

        cat('\n - ', l)
        return(l)
      })

      levels = reactive({
        req(orgUnits())
        cat('\n* reporting_widget levels():')
        levels =
          count(orgUnits() %>% as_tibble, level, levelName) %>%
          arrange(level)
        cat('\n - end levels():')
        return(levels)
      })

      # reportingSelectedOUs ####

      selected <- reactiveValues(x = NULL, panel = NULL, chart = NULL)

      observeEvent(r_year$brush(), {
        selected$chart = 1
        b <- r_year$brush()
        selected$x = round(seq(b$xmin, b$xmax, by = .5)) %>% unique
        selected$panel = b$panelvar1
        return(selected)
      })

      observeEvent(r_month$brush(), {
        selected$chart = 2
        b <- r_month$brush()
        selected$x = round(seq(b$xmin, b$xmax, by = .5)) %>% unique %>% as.integer()
        selected$panel = b$panelvar1
        cat("plot_2_selected$x:", selected$x)
        return(selected)
      })

      reportingSelectedOUs <- reactive({
        #print( 'reportingSelectedOUs()' )
        req(hasVisitedReporting())  # dormant until user first opens Reporting tab
        req(endingMonth_debounced())
        req(startingMonth_debounced())
        req(d())
        req(period())
        req(selected_data_categories$elements)

        cat("\n* reporting_widget: reportingSelectedOUs")

        if (length(selected_data_categories$elements) == 0) {
          cat("\n - no data elements selected")
          return()
        }

        # Testing
        # d = d()
        # endingMonth = input$endingMonth
        # startingMonth = input$startingMonth
        # .period = period()
        # missing_reports = as.integer( input$missing_reports )
        # count.any = input$count.any
        # all_categories = input$all_categories
        # data_categories = input$data_categories
        # .cat = FALSE
        # save( d , endingMonth, startingMonth, .period, missing_reports, count.any,
        #       all_categories, data_categories, .cat, file = 'reportingSelectedOUs_data.rda')

        if (input$mostReports & nrow(d()) > 0) {
          cat(
            "\n - determining most frequently reported facilities...",
            startingMonth_debounced(),
            endingMonth_debounced()
          )
          .t0_rous <- proc.time()["elapsed"]

          # cat( "\n - selected_data_categories:" ,
          #      paste( selected_data_categories$elements , collapse = ", " )
          #      )

          # Pre-filter to selected elements before passing to mostFrequentReportingOUs.
          # Reduces date-window scan from ~34M rows (all elements) to ~3M (selected),
          # saving ~10-12 sec. The function's internal %chin% filter becomes a no-op.
          # "any_data" rule intentionally uses all elements, so skip pre-filter then.
          .d_rous <- if (input$reporting_rule != "any_data" &&
                         length(selected_data_categories$elements) > 0)
            d()[get("data") %chin% selected_data_categories$elements]
          else
            d()

          sf = mostFrequentReportingOUs(
            d = .d_rous,
            endingMonth = endingMonth_debounced(),
            startingMonth = startingMonth_debounced(),
            missing_reports = as.integer(input$missing_reports),
            reporting_rule  = input$reporting_rule,
            data_categories = selected_data_categories$elements,
            .cat = TRUE
          )

          cat(sprintf("\n - mostFrequentReportingOUs: %d orgUnits  %.1f sec", length(sf), proc.time()["elapsed"] - .t0_rous))

          if (length(sf) == 0L) {
            showNotification(
              paste0(
                "No champion facilities found with the current reporting rule ",
                "(\"", input$reporting_rule, "\"). ",
                "All facilities will be classified as Non-Champion. ",
                "Try a less strict rule or reduce the number of selected elements."
              ),
              type = "warning", duration = 10
            )
          }
        } else {
          sf = NULL
        }

        cat("\n - end reportingSelectedOUs:")

        # Testing
        # saveRDS( sf, 'reportingSelectedOUs.rds')

        return(sf)
      })

      x.annual = reactive({
        cat('\n* reporting_widget x.annual()')

        # x.a = orgunit.reports() %>%
        #       filter( orgUnit %in% reportingSelectedOUs() )   # %>%
        # group_by( year , n_periods ) %>%
        #   summarise( n =  n_distinct( orgUnit ) )

        # data.table speed up over dplyr
        x.a = setDT(orgunit.reports())[
          orgUnit %in% reportingSelectedOUs(),
          .(n = uniqueN(orgUnit)),
          by = c('year', 'n_periods')
        ] %>%
          as_tibble()

        #print('end x.annual:') ; toc();  # #print( x.a )
        cat('\n - x.annual done')
        return(x.a)
      })

      x.months = reactive({
        cat('\n* reporting_widget x.months()')
        # req( keeprows() )
        # tic()
        #print( 'x.months()' )

        .period = period()

        # x.m = orgunit.monthly.reports() %>%
        #       filter( orgUnit %in% reportingSelectedOUs() )

        x.m = setDT(orgunit.monthly.reports())[
          orgUnit %in% reportingSelectedOUs(),
          .(n = uniqueN(orgUnit)),
          by = c('year', .period)
        ] %>%
          as_tibble()

        # %>%
        #       group_by( year , !! rlang::sym( period() )  ) %>%
        #       summarise( n = n_distinct( orgUnit ) )

        #print('end x.months:') ; toc() ; # glimpse( x.m )
        cat('\n - x.months done')
        return(x.m)
      })

      # plot_reporting_by_month ####

      plot2 = reactive({
        req(monthly.reports())
        req(period())

        cat('\n* reporting_widget plot2():')
        .period = period()
        cat('\n - period():', .period)

        # save data for testing ggplot options
        # cat('\n - saving plot2_data.rds')
        # saveRDS( monthly.reports(), 'monthly.reports.rds' )

        if (length(monthly.reports()$year) > 0) {
          if (.period == "Month") {
            .breaks = 1:12
          } else {
            .breaks = seq(2, 53, 4)
          }

          cat('\n - plot2: ggplot( monthly.reports() ... ')
          g = ggplot(
            monthly.reports() %>% mutate(facilities = 'All'),
            aes(
              x = !!rlang::sym(.period),
              y = n,
              group = facilities,
              color = facilities
            )
          ) +
            # geom_col() +
            geom_point() +
            geom_line() +
            geom_hline(yintercept = facilities()) +
            facet_wrap(~year, scales = 'free_x') +
            # scale_x_discrete( .period
            #                     , breaks = .breaks
            #                     # , labels  = as.character( .breaks )
            #                     )  +
            ylim(0, NA) +
            scale_color_manual(
              values = c('All' = 'black', 'Champion' = 'brown')
            )

          if (!is.null(reportingSelectedOUs())) {
            cat('\n - g + selected facilities ')
            g = g +
              # geom_col(  data = x.months() %>% mutate( facilities = 'Champion' )  )
              geom_point(
                data = x.months() %>% mutate(facilities = 'Champion')
              ) +
              geom_line(data = x.months() %>% mutate(facilities = 'Champion'))
          }

          # return( shift_legend3(g) )
          return(g)
        }

        #print('end plot2')
        return(g)
      })

      r_month <- chartModuleServer(
        "plot_reporting_by_month",
        reactive({ plot2() + theme_bw(base_size = 16) })
      )

      verbatimTextOutput("info")

      output$info <- renderText({
        xy_str <- function(e) {
          if (is.null(e)) {
            return("NULL\n")
          }
          paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
        }

        xy_range_str <- function(e) {
          if (is.null(e)) {
            return("NULL\n")
          }
          paste0(
            "xmin=",
            round(e$xmin, 1),
            " xmax=",
            round(e$xmax, 1),
            " ymin=",
            round(e$ymin, 1),
            " ymax=",
            round(e$ymax, 1)
          )
        }

        paste0(
          "click: ",    xy_str(r_year$click()),
          "dblclick: ", xy_str(r_year$dblclick()),
          "hover: ",    xy_str(r_year$hover()),
          "brush: ",    xy_range_str(r_year$brush())
        )
      })

      output$x_value <- renderText({
        if (input$mostReports) {
          HTML(
            "You've selected <code>",
            comma(length(reportingSelectedOUs())),
            "facilities",
            "</code>"
          )
        }

        if (is.null(selected$x)) {
          return("")
        } else {
          lvls <- levels(annual.reports()$n_periods)
          panel = selected$panel

          name <- lvls[round(selected$x)]
          HTML(
            "You've selected <code>",
            comma(length(reportingSelectedOUs())),
            "</code>",
            "facilities that submitted data for <code>",
            paste(name, collapse = ","),
            "months during",
            panel,
            "</code>"
          )
        }
      })

      # Histogram of Annual Number of Months Reported (plot_reports_in_a_year) ####

      plot1 = reactive({
        #print( 'plot1():' )
        req(annual.reports())
        req(period())
        .period = period()

        # save data for testing ggplot options
        # saveRDS( annual.reports() , 'plot1_data.rds' )

        if (length(annual.reports()$year) > 0) {
          if (.period == "Month") {
            .breaks = 1:12
          } else {
            .breaks = seq(2, 53, 4)
          }

          cat('\n - plot1: ggplot( annual.reports() ... ')

          # Testing
          # saveRDS( annual.reports(), "annual.reports.rds" )
          # saveRDS( reportingSelectedOUs() , "reportingSelectedOUs.rds" )
          # saveRDS( facilities() , "facilities.rds" )
          # saveRDS( x.annual() , "x.annual.rds" )

          g = ggplot(annual.reports(), aes(x = n_periods, y = n)) +
            geom_col() +
            scale_x_discrete(
              'Number Months Reported',
              breaks = .breaks,
              labels = .breaks,
              drop = FALSE
            ) +
            geom_hline(yintercept = facilities()) +
            facet_wrap(~year, scales = 'free_x')

          if (!is.null(reportingSelectedOUs())) {
            g = g + geom_col(data = x.annual(), fill = 'brown', width = .9)
          }

          #print( 'end plot1' )
          return(g)
        }
      })

      r_year <- chartModuleServer(
        "plot_reports_in_a_year",
        reactive({ plot1() + theme_bw(base_size = 16) })
      )

      # Cache the last successfully-computed selected_data so that downstream
      # widgets (cleaning, evaluation, dqa) always have a stable value even if
      # selected_data() is mid-computation or blocked by req().
      cached_selected_data = reactiveValues(value = NULL)
      observeEvent(selected_data(), ignoreNULL = TRUE, {
        cached_selected_data$value <- selected_data()
      })

      # selected_data. select ous and data element categories ####

      selected_data = reactive({
        #print( 'selected_data():')
        req(hasVisitedReporting())  # dormant until user first opens Reporting tab
        req(data1())
        req(selected_data_categories$elements)

        cat("\n* reporting_widget selected_data(): ")

        cat(
          "\n - levels ",
          selected_org_levels$level2,
          selected_org_levels$level3,
          selected_org_levels$level4,
          selected_org_levels$level5
        )

        cat(
          "\n - reporting_widget selected_data_categories(): ",
          paste(selected_data_categories$elements, collapse = ", ")
        )

        # Pre-filter to selected elements before cleanedData — reduces input from
        # ~34M rows (all elements) to ~3M rows (selected elements only), saving ~4 sec.
        # cleanedData only does per-row operations (effectiveLeaf, dataCol, NA_ filter)
        # so pre-filtering is safe and produces identical output.
        .t0_sd <- proc.time()["elapsed"]
        .d1_input <- if (length(selected_data_categories$elements) > 0)
          as.data.table(data1())[get("data") %chin% selected_data_categories$elements]
        else
          data1()
        .cleanedData = cleanedData(
          .d1_input,
          .effectiveLeaf = TRUE,
          source = input$source,
          error = NULL,
          algorithm = 'seasonal3',
          .cat = TRUE
        )
        cat(sprintf('\n - cleanedData: %.1f sec  %d rows', proc.time()["elapsed"] - .t0_sd, nrow(.cleanedData)))

        # When there are no leaf orgUnits (e.g. National or District data only)
        if (nrow(.cleanedData) == 0) {
          .cleanedData = cleanedData(
            data1(),
            .effectiveLeaf = FALSE,
            source = input$source,
            error = NULL,
            algorithm = 'seasonal3',
            .cat = TRUE
          )

          cat(
            "\n - .cleanedData with .effectiveLeaf FALSE, nrow = ",
            nrow(.cleanedData)
          )
        }

        rous = reportingSelectedOUs()

        cat("\n - selected_data:")
        .t0_sdata <- proc.time()["elapsed"]
        selected_data = selectedData(
          data = .cleanedData, # data1() ,
          levelNames = levelNames(),
          data_categories = selected_data_categories$elements,
          # all_categories = input$all_categories ,
          alwaysReporting = input$mostReports,
          reportingSelectedOUs = rous,
          startingMonth = NULL,
          endingMonth = NULL,
          # source = 'Original' ,
          level = 'leaf',
          level2 = selected_org_levels$level2,
          level3 = selected_org_levels$level3,
          level4 = selected_org_levels$level4,
          level5 = selected_org_levels$level5,
          .cat = TRUE
        )

        # data = data1() ,
        # levelNames = levelNames() ,
        # data_categories = selected_data_categories$elements ,
        # # all_categories = input$all_categories ,
        # alwaysReporting = input$mostReports ,
        # reportingSelectedOUs = reportingSelectedOUs() ,
        # source = input$source ,
        # level2 = input$level2 ,
        # level3 = input$level3 ,
        # level4 = input$level4 ,
        # level5 = input$level5 ,
        # .cat = TRUE )

        #  data = setDT( d() )[ , Selected := 'All', ]
        #  # data = d() %>% mutate( Selected = 'All' )
        #
        #
        #  # filter to selected category
        #  cat( '\n - selected_data filtered by' # , input$data_categories
        #       )
        #
        #  if ( !input$all_categories )
        #
        #    # data = data %>% filter( data %in% input$data_categories )
        #
        #    data = setDT( data )[ data %in% input$data_categories ,, ]
        #
        #  # Add var for selected ous
        #  cat( '\n - selected_data length( reportingSelectedOUs()): ' , length( reportingSelectedOUs())  )
        #
        # if ( length( reportingSelectedOUs()) > 0 ){
        #
        #   data = setDT( data )[ , Selected := fifelse( orgUnit %in% reportingSelectedOUs() ,
        #                                                    'Reporting Each Period',
        #                                                    'Inconsistent Reporting') ]
        #   # data  = data %>%
        #   #   mutate( Selected = ifelse(
        #   #     orgUnit %in% reportingSelectedOUs() ,
        #   #     'Reporting Each Period',
        #   #     'Inconsistent Reporting' )
        #   #   )
        # }
        #
        #  cat( '\n - end  selected_data()')

        cat(sprintf('\n - selectedData: %.1f sec  %d rows', proc.time()["elapsed"] - .t0_sdata, nrow(selected_data)))
        cat(sprintf('\n - selected_data total: %.1f sec', proc.time()["elapsed"] - .t0_sd))

        return(selected_data)
      })

      group_by_cols = reactive({
        req(period())
        req(levelNames())

        cat("\n* reporting_widget group_by_cols():")

        adms = levelNames()
        cat("\n - adms:", adms)

        # When coloring by category, add "data" as the split so dataTotal()
        # preserves the data-element column instead of summing across it
        effective_split <- if (isTRUE(input$series_by == "Category")) "data" else input$split

        group_by_cols = groupByCols(
          period = period(),
          levelNames = levelNames(),
          agg_level = adms[1],
          split = effective_split
        )

        cat("\n - group_by_cols:", group_by_cols)

        return(group_by_cols)
      })

      # data.total: merges data across multiple set ####
      data.total = reactive({
        req(selected_data())
        # req( group_by_cols() )
        req(period())

        cat('\n* reporting_widget data.total()')
        .t0_dt <- proc.time()["elapsed"]

        # Testing
        # saveRDS( selected_data()  , 'selected_data.rds')
        # saveRDS( group_by_cols()  , 'group_by_cols.rds')

        data.total = dataTotal(
          data = selected_data(),
          period = period(),
          group_by_cols = group_by_cols(),
          startMonth = NULL,
          endMonth = NULL,
          dataSets = input$dataSets,
          mean.merge = input$dataset_merge_average,
          .cat = TRUE
        )

        cat(sprintf('\n - end data.total  %.1f sec  %d rows', proc.time()["elapsed"] - .t0_dt, nrow(data.total)))

        # Testing
        # saveRDS( data.total , 'data.total.rds')
        return(data.total)
      })

      num_facilities = reactive({
        req(data.total())
        #print('num_facilities()')
        .d = data.total()
        l = length(unique(.d$Selected))
        #print( paste( 'number of Facilities', l ) )
        return(l)
      })

      num_datasets = reactive({
        req(data.total())
        #print('num_datasets()')
        .d = data.total()
        l = length(unique(.d$dataSet))
        #print( paste( 'number of dataSets', l ) )
        return(l)
      })

      backtick <- function(x) paste0("`", x, "`")

      aggregateDataKey = reactive({
        req(num_facilities())
        req(num_datasets())

        cat('\n* reporting_widget aggregateDataKey():')

        adms = backtick(levelNames())

        hts = paste("(", adms[1], ")")

        # Facet by champion/non-champion only when explicitly requested
        if (num_facilities() > 1 && isTRUE(input$facet_by == "Champion/Non-Champion")) {
          hts = paste('Selected *', hts)
        }

        # Dataset series: only when explicitly requested
        if (isTRUE(input$series_by == "Dataset") && num_datasets() > 1) {
          hts = paste('dataSet *', hts)
        }

        # Category series: split by data-element column
        effective_split <- if (isTRUE(input$series_by == "Category")) "data" else input$split
        if (!effective_split %in% 'None') {
          hts = paste(backtick(effective_split), '*', hts)
        }

        cat('\n - done:', hts)
        return(hts)
      })

      aggregateselected_data = reactive({
        # req( data.hts() )
        req(data.total())
        cat('\n* reporting_widget aggregateselected_data():')
        .t0_agg <- proc.time()["elapsed"]

        # testing
        # saveRDS(levelNames(), 'levelNames.rds')
        # saveRDS(aggregateDataKey(), 'aggregateDataKey.rds')

        .d = data.total()
        cat('\n - data.total():')

        # testing
        # saveRDS(.d, 'data.total.rds')

        if (!is_tsibble(.d)) {
          cat('\n - preparing data.total as tsibble')

          key.cols = intersect(setdiff(group_by_cols(), period()), names(.d))

          cat('\n - key.cols:', key.cols)

          # testing
          # saveRDS( key.cols , 'key.cols.rds' )

          .d = .d %>%
            as_tsibble(
              index = !!rlang::sym(period()),
              key = all_of({{ key.cols }})
            )
        }

        cat('\n - preparing aggregate_key')

        has_ratio_cols <- all(c("numerator", "denominator") %in% names(.d))

        if (has_ratio_cols) {
          # Enforce ratio validity rules before aggregating:
          #   - Missing numerator + valid denominator → numerator = 0 (zero positives
          #     is plausible; reporters often leave blank rather than enter 0).
          #   - Present numerator but missing/zero denominator → exclude both
          #     (orphaned numerator: cannot have positive tests without total tests).
          #   - numerator > denominator → exclude both (impossible ratio).
          .d <- .d %>%
            mutate(
              .valid_den  = !is.na(denominator) & denominator > 0,
              .impossible = .valid_den & !is.na(numerator) & numerator > denominator,
              numerator = dplyr::case_when(
                !.valid_den | .impossible ~ NA_real_,
                is.na(numerator)          ~ 0,
                TRUE                      ~ numerator
              ),
              denominator = dplyr::if_else(.valid_den & !.impossible, denominator, NA_real_)
            ) %>%
            select(-.valid_den, -.impossible)

          .d = .d %>%
            aggregate_key(
              .spec       = !!rlang::parse_expr(aggregateDataKey()),
              total       = sum(total,       na.rm = TRUE),
              numerator   = sum(numerator,   na.rm = TRUE),
              denominator = sum(denominator, na.rm = TRUE)
            )
          # Recompute total from aggregated components for ratio rows.
          # Include-step rows have denominator == 0 after na.rm sum of NAs,
          # so the condition correctly leaves them untouched.
          .d = .d %>%
            mutate(total = dplyr::if_else(
              !is.na(denominator) & denominator > 0,
              numerator / denominator,
              total
            ))
        } else {
          .d = .d %>%
            aggregate_key(
              .spec = !!rlang::parse_expr(aggregateDataKey()),
              total = sum(total, na.rm = TRUE)
            )
        }

        indexVar = index_var(.d)
        keyVars = key_vars(.d)

        # testing
        # saveRDS( .d , 'agg.d1.rds' )

        .d = .d %>%
          filter(
            !is.na(!!rlang::sym(levelNames()[1])),
            is_aggregated(!!rlang::sym(levelNames()[1]))
          ) %>%
          mutate(grouping_var = 'Total')

        if (isTRUE(input$series_by == "Dataset") && num_datasets() > 1) {
          .d = .d %>%
            filter(!is_aggregated(dataSet)) %>%
            mutate(
              dataSet      = as.character(dataSet) %>% str_remove_all("<aggregated>"),
              grouping_var = dataSet
            )
        }

        if (num_facilities() > 1 && isTRUE(input$facet_by == "Champion/Non-Champion")) {
          .d = .d %>%
            filter(!is_aggregated(Selected)) %>%
            mutate(Selected = as.character(Selected) %>% str_remove_all("<aggregated>"))
        }

        # testing
        # saveRDS( .d , 'agg.d2.rds' )

        # If split (or series_by == Category maps to "data"), remove aggregate grouping
        effective_split <- if (isTRUE(input$series_by == "Category")) "data" else input$split
        if (!effective_split %in% 'None') {
          .d = .d %>%
            filter(!is_aggregated(!!rlang::sym(effective_split))) %>%
            mutate(grouping_var = as.character(!!rlang::sym(effective_split)))
        }

        # testing
        # saveRDS( .d , 'agg.d3.rds' )

        # ensure output is tbl_ts
        if (!'tbl_ts' %in% class(.d)) {
          cat('\n - convert .d to tsibble ')
          .d = .d %>% as_tsibble(key = all_of(keyVars), index = indexVar)
        }

        # testing
        # saveRDS( .d, 'aggregateselected_data.rds')

        cat(sprintf('\n -  end aggregateselected_data  %.1f sec  %d rows', proc.time()["elapsed"] - .t0_agg, nrow(.d)))
        return(.d)
      })

      caption.text = reactive({
        rous = reportingSelectedOUs()

        parts <- Filter(
          function(x) !is.null(x) && length(x) > 0,
          list(
            selected_org_levels$level2,
            selected_org_levels$level3,
            selected_org_levels$level4,
            selected_org_levels$level5
          )
        )
        region_label <- if (length(parts) == 0) "National" else
          paste(sapply(parts, paste, collapse = ", "), collapse = " / ")

        # Reporting criteria: "N/12 months, Jan 2020–Dec 2025"
        months_required <- 12L - as.integer(input$missing_reports %||% 0L)
        start_str <- startingMonth_debounced()
        end_str   <- endingMonth_debounced()
        date_range <- if (!is.null(start_str) && !is.null(end_str) &&
                          nzchar(start_str) && nzchar(end_str))
          paste0(start_str, "\u2013", end_str)
        else NULL
        criteria <- paste0(
          months_required, "/12 months",
          if (!is.null(date_range)) paste0(", ", date_range) else ""
        )

        facility_text <- if (isTRUE(input$facet_by == "Champion/Non-Champion")) {
          if (length(rous) > 0)
            paste0(comma(length(rous)), " consistently reporting facilities (", criteria, ")")
          else ""
        } else {
          # All Facilities: show total facility count, no champion criteria
          n_tot <- tryCatch(sum(n_selected()$n, na.rm = TRUE), error = function(e) 0L)
          if (n_tot > 0) paste0(comma(n_tot), " facilities") else ""
        }

        paste(c(region_label, facility_text), collapse = " \u2014 ")
      })

      n_selected = reactive({
        req(selected_data())

        cat("\n* n_selected(): ")

        # testing
        # saveRDS( reportingSelectedOUs(), "reportingSelectedOUs.rds")
        # saveRDS( selected_data(),"selected_data.rds" )
        # saveRDS( data1(), "data1.rds" )
        # saveRDS( levelNames() , "levelNames.rds" )
        # saveRDS(  selected_data_categories$elements , "selected_data_categories.rds" )

        x = selected_data() %>%
          as_tibble %>%
          ungroup %>%
          distinct(Selected, orgUnit) %>%
          group_by(Selected) %>%
          summarise(n = n())

        # cat( x )
        return(x)
      })

      plotAgregateValue = reactive({
        req(aggregateselected_data())
        req(input$split)
        cat('\n* reporting_widget plotAgregateValue():')

        .d = aggregateselected_data()

        # testing
        # saveRDS(.d, 'plot3_data.rds')

        cats = unique(selected_data()$data)
        max_show = 5
        data.text = if (length(cats) <= max_show) {
          paste(cats, collapse = " + ")
        } else {
          paste0(paste(cats[seq_len(max_show)], collapse = " + "),
                 sprintf(" ... and %d more", length(cats) - max_show))
        }
        cat("\n - data.text:", data.text)

        # #print( 'data.text'); #print( data.text )

        .limits = c(0, NA)
        .period = period()

        #print('plotting aggregate data');

        # Apply display-date filter here rather than in data.total() so that
        # adjusting the display window doesn't re-run dataTotal()/aggregate_key().
        if (!is.null(input$startDisplayMonth) && !is.null(input$endDisplayMonth)) {
          if (.period %in% 'Month') {
            .d = .d %>% filter(
              Month >= yearmonth(input$startDisplayMonth),
              Month <= yearmonth(input$endDisplayMonth)
            )
          } else if (.period %in% 'Week') {
            .d = .d %>% filter(
              Week >= yearweek(input$startDisplayMonth),
              Week <= yearweek(input$endDisplayMonth)
            )
          }
        }

        cat('\n - fill_gaps')
        .d = .d %>%
          fill_gaps(.full = TRUE)

        cat('\n - plot')
        g = .d %>%
          # autoplot( vars( total , grouping_var ) )
          ggplot(aes(
            x = !!rlang::sym(.period),
            y = total,
            group = grouping_var,
            colour = grouping_var
          )) +
          geom_line(na.rm = TRUE)

        # Legend / color scale based on series_by selection
        if (isTRUE(input$series_by == "Dataset") && num_datasets() > 1) {
          dataSet_breaks <- unique(.d$dataSet)
          dataSet_labels <- unique(.d$dataSet)
          dataSet_labels[dataSet_labels == ""] <- "Combined"
          g = g +
            scale_color_discrete(breaks = dataSet_breaks, labels = dataSet_labels, drop = TRUE) +
            guides(color = guide_legend(title = "Dataset", nrow = 3, byrow = TRUE))
        } else if (isTRUE(input$series_by == "Category")) {
          # Truncate long category names so the legend doesn't dominate;
          # max 3 rows so entries spread across columns rather than stacking.
          g = g +
            scale_color_discrete(labels = function(x) stringr::str_trunc(x, 40)) +
            guides(color = guide_legend(title = "Category", nrow = 3, byrow = TRUE))
        } else if (!input$split %in% 'None') {
          g = g + guides(color = guide_legend(title = input$split, nrow = 3, byrow = TRUE))
        } else {
          g = g + guides(color = "none")
        }

        facet_labeller = function(x) {
          y = n_selected() %>% filter(Selected %in% x) %>% pull(n)

          paste0(x, " ( n= ", comma(y), " )")
        }

        # facet when selected > 0 and Selected column is present with values
        # (aggregate_key marginalises Selected when it is not in the key formula,
        # so the column may be absent or all-NA after aggregation)
        rous = reportingSelectedOUs()

        has_selected <- "Selected" %in% names(.d) &&
          length(unique(na.omit(.d[["Selected"]]))) > 0

        if (isTRUE(input$facet_by == "Champion/Non-Champion") && length(rous) > 0 &&
            has_selected) {
          g = g +
            facet_wrap(
              vars(Selected),
              labeller = as_labeller(facet_labeller),
              ncol = 3
            )
        }

        # Time scales
        if (period() %in% 'Month') {
          g = g +
            scale_x_yearmonth(date_breaks = "1 year")
        }

        if (period() %in% 'Week') {
          g = g +
            scale_x_yearweek(date_breaks = "1 year")
        }

        g = g +
          scale_y_continuous(label = comma, limits = .limits) +
          labs(
            y = "",
            x = "",
            title = str_wrap(input$indicator, 200),
            subtitle = str_wrap(data.text, 200),
            caption = str_wrap(caption.text(), 200)
          ) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            legend.text     = element_text(size = 8),
            legend.key.size = unit(0.35, "cm"),
            legend.spacing.x = unit(0.2, "cm"),
            strip.text = element_text(size = 20) # facet label text size
          )

        #print( ' end plotAgregateValue()' )
        cat('\n - done')
        return(g)
      })

      chartModuleServer("plot_values", reactive({ plotAgregateValue() }))

      # Champions Map and Table####

      # Avg Value by facility
      avgValues = reactive({
        req(selected_data())
        cat("\n * avgValues:")

        # Include the level-2 (province/region) column so the Facilities table
        # can be filtered by province, allowing the user to see which facilities
        # belong to a specific region.
        ln <- levelNames()
        lvl2_col <- if (length(ln) >= 2L) ln[2L] else NULL

        if (!is.null(lvl2_col) && lvl2_col %in% names(selected_data())) {
          avgValues <-
            selected_data() %>%
            group_by(orgUnit, !! rlang::sym(lvl2_col), Month) %>%
            summarise(dataCol = sum(dataCol, na.rm = TRUE), .groups = "drop") %>%
            group_by(orgUnit, !! rlang::sym(lvl2_col)) %>%
            summarise(medianValue = median(dataCol, na.rm = TRUE), .groups = "drop")
        } else {
          avgValues <-
            selected_data() %>%
            group_by(orgUnit, Month) %>%
            summarise(dataCol = sum(dataCol, na.rm = TRUE), .groups = "drop") %>%
            group_by(orgUnit) %>%
            summarise(medianValue = median(dataCol, na.rm = TRUE), .groups = "drop")
        }

        cat("\n - done avgValues:")
        return(avgValues)
      })

      champion_facilities = reactive({
        req(avgValues())
        req(reportingSelectedOUs())
        cat("\n * champion_facilities:")

        sou = reportingSelectedOUs()
        gf = geoFeatures()
        req(inherits(gf, "sf"))   # geoFeatures not available for this instance
        avgValues = avgValues()

        # testing
        # saveRDS(gf, 'gf.rds')
        # saveRDS(sou, 'sou.rds')
        # saveRDS(avgValues(), 'avgValues.rds')

        cat("\n - quartile values:")
        quartileValues = quantile(
          avgValues$medianValue,
          probs = c(0, 0.25, 0.5, 0.75, 1)
        )

        cat("\n - champion column:")
        # Try POINT features first; if none exist (facilities stored as polygons)
        # fall back to the lowest-level features using their polygon centroids.
        facilities_sf <- gf %>% filter(st_geometry_type(.) == 'POINT') %>%
          filter(!st_is_empty(.))

        if (nrow(facilities_sf) == 0) {
          cat("\n - no POINT features; using lowest-level polygon centroids")
          lowest_level <- max(gf$level, na.rm = TRUE)
          facilities_sf <- gf %>%
            filter(level == lowest_level, !st_is_empty(.)) %>%
            sf::st_centroid()
        }

        # Restrict to facilities present in avgValues, which is already filtered
        # to the selected region via selected_data(). Without this, all facilities
        # in the full geoFeatures dataset are shown and only the champion subset
        # get labelled correctly — the rest appear as "Inconsistent".
        facilities_sf <- facilities_sf %>%
          filter(id %in% avgValues$orgUnit)

        champion_facilities = facilities_sf %>%
          mutate(
            champion = ifelse(
              id %in% sou,
              "Consistent Reporting (Champion)",
              "Inconstent Reporting"
            )
          )

        cat("\n - join avgvalues:")
        champion_facilities = champion_facilities %>%
          # filter( id %in% "a08881Oz98k" ) %>%
          left_join(avgValues, by = c("id" = "orgUnit")) %>%
          mutate(
            medianValueRange = cut(
              medianValue,
              breaks = unique(quartileValues),
              ordered_result = TRUE
            ),
            medianValueRangeSize = medianValueRange %>% as.numeric()
          )

        cat("\n - done champion_facilities:")
        return(champion_facilities)
      })

      facility_chart = reactive({
        req(champion_facilities())
        cat("\n * facility_chart:")

        champion_facilities = champion_facilities() %>% st_drop_geometry()

        summary = champion_facilities %>%
          select(-parentGraph, groups) %>%
          group_by(champion) %>%
          summarise(n = n(), mean = mean(medianValue, na.rm = TRUE))

        annotation = paste0(
          "mean = ",
          round(summary$mean),
          " (n=",
          scales::comma(summary$n),
          ")"
        )

        facility_chart =
          champion_facilities %>%
          ggplot(aes(champion, medianValue)) +
          geom_boxplot() +
          stat_summary(fun = mean, geom = "point", shape = 5, size = 4) +
          geom_jitter(width = .35) +
          labs(x = "", y = "Median Value") +
          scale_y_log10() +
          annotate('text', x = 1:2, y = 1.5, label = annotation) +
          labs(
            title = "Comparison of Values Repoted by Consistent and Inconsistent Reporting Facilities"
          )

        cat("\n - done facility_chart:")
        return(facility_chart)
      })

      base.map = reactive({
        cat("\n * reporting_widget: base.map")
        gf = geoFeatures()

        cat('\n - split geofeatures')
        split_geofeatures = base::split(gf, f = gf[['levelName']])

        levels = bind_rows(gf %>% st_drop_geometry()) %>%
          filter(!is.na(level)) %>%
          distinct(level, levelName)

        cat('\n - levels:', levels$levelName, '\n')

        # reorder levels
        split_geofeatures = split_geofeatures[levels$levelName]

        # test for empty geometry
        not_all_empty_geo = map_lgl(
          split_geofeatures,
          ~ !all(is.na(st_dimension(.x)))
        )

        n_levels = sum(not_all_empty_geo) #

        cat(
          paste(
            '\n - geoFeatures split into',
            n_levels,
            'levels',
            paste(names(split_geofeatures), collapse = ','),
            sep = " "
          ),
          '\n'
        )

        level.colors = RColorBrewer::brewer.pal(n_levels, 'Set2')
        names(level.colors) = levels[not_all_empty_geo, 'levelName']

        split_gf = split_geofeatures[not_all_empty_geo]

        admins = gf %>%
          filter(st_geometry_type(.) != 'POINT') %>%
          filter(!st_is_empty(.))

        admin.levels = admins$levelName %>% unique

        # pal <- colorNumeric( palette = "YlGnBu", domain = avgValues$medianValue  )
        cat("\n - base.map")
        base.map =
          leaflet() %>%
          addTiles(group = "OpenStreetMap") %>%
          addTiles(group = "No Background", options = providerTileOptions(opacity = 0))

        admins_wgs84 <- sf::st_transform(admins, crs = 4326)
        for (i in seq_along(admin.levels)) {
          base.map = base.map %>%
            addPolygons(
              data = admins_wgs84 %>% filter(levelName == admin.levels[i]),
              group = admin.levels[i],
              label = ~ paste(
                name,
                ifelse(level < 3, '', paste('in', parentName))
              ),
              color = "black",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0,
              fillColor = "lightblue",
              highlightOptions = highlightOptions(
                color = "white",
                weight = 2,
                bringToFront = TRUE
              )
            ) %>%
            hideGroup(admin.levels[i])
        }

        cat("\n - addLayersControls")
        base.map = base.map %>%
          # Layers control
          addLayersControl(
            baseGroups = c("OpenStreetMap", "No Background"),
            overlayGroups = c(admin.levels, "Facility"),
            options = layersControlOptions(collapsed = TRUE)
          )

        return(base.map)
      })

      facility_map = reactive({
        cat("\n * reporting_widget: facility map")
        gf = geoFeatures()
        facilities = champion_facilities()
        cat("\n - facilities nrow:", nrow(facilities),
            " POINT rows:", sum(sf::st_geometry_type(facilities) == "POINT", na.rm = TRUE),
            " medianValue non-NA:", sum(!is.na(facilities$medianValue)))
        avgValues = avgValues()
        base.map = base.map()

        cat("\n - admin.levels")
        admins = gf %>%
          filter(st_geometry_type(.) != 'POINT') %>%
          filter(!st_is_empty(.))
        admin.levels = admins$levelName %>% unique

        cat('\n - factpal')

        factpal <- colorFactor(c("red4", "grey20"), facilities$champion)

        cat('\n - add markers')

        # Helper: cluster icon JS
        # - solid-color outer ring so the group is identifiable even at low counts
        # - fill alpha scales with count (density encoding)
        cluster_icon_js <- function(r, g, b) {
          JS(sprintf(
            "function(cluster) {
              var n = cluster.getChildCount();
              var a = Math.min(0.25 + n / 120, 0.85);
              var sz = n < 10 ? 34 : n < 100 ? 42 : 50;
              return new L.DivIcon({
                html: '<div style=\"' +
                  'background:rgba(%d,%d,%d,' + a + ');' +
                  'border:3px solid rgba(%d,%d,%d,1);' +
                  'border-radius:50%%;' +
                  'width:' + sz + 'px;height:' + sz + 'px;' +
                  'box-sizing:border-box;' +
                  'display:flex;align-items:center;justify-content:center;' +
                  'color:white;font-weight:bold;font-size:12px;' +
                  'text-shadow:0 0 3px rgba(0,0,0,0.8);' +
                  '\">' + n + '</div>',
                className: '',
                iconSize: new L.Point(sz, sz)
              });
            }", r, g, b, r, g, b
          ))
        }

        champion_cluster_opts    <- markerClusterOptions(
          maxClusterRadius = 40,
          iconCreateFunction = cluster_icon_js(139, 0, 0)   # red4  = #8B0000
        )
        nonchampion_cluster_opts <- markerClusterOptions(
          maxClusterRadius = 40,
          iconCreateFunction = cluster_icon_js(32, 32, 32)  # grey20 = #202020
        )

        # Split into champion / non-champion for separate coloured cluster layers
        champ    <- facilities %>% filter(champion == "Consistent Reporting (Champion)")
        nonchamp <- facilities %>% filter(champion != "Consistent Reporting (Champion)")

        mk_radius <- function(fac) ifelse(
          is.na(fac$medianValue) | fac$medianValue == 0,
          4,
          pmax(4, pmin(20, log1p(fac$medianValue)))
        )

        # Cluster markers so the browser renders ~dozens of bubbles on load
        # rather than thousands of individual SVG circles (which crashes tabs
        # with large datasets like DRC).  Clusters expand automatically on zoom.
        gf.map = base.map

        if (nrow(champ) > 0) {
          gf.map = gf.map %>%
            addCircleMarkers(
              lng            = sf::st_coordinates(champ)[, 1],
              lat            = sf::st_coordinates(champ)[, 2],
              group          = "Champions",
              radius         = mk_radius(champ),
              fillColor      = "red4",
              color          = "red4",
              stroke         = TRUE,
              weight         = 1,
              fillOpacity    = 0.8,
              opacity        = 1,
              label          = paste0(champ$name, " (Champion)"),
              clusterOptions = champion_cluster_opts
            )
        }

        if (nrow(nonchamp) > 0) {
          gf.map = gf.map %>%
            addCircleMarkers(
              lng            = sf::st_coordinates(nonchamp)[, 1],
              lat            = sf::st_coordinates(nonchamp)[, 2],
              group          = "Non-Champions",
              radius         = mk_radius(nonchamp),
              fillColor      = "grey20",
              color          = "grey20",
              stroke         = TRUE,
              weight         = 1,
              fillOpacity    = 0.8,
              opacity        = 1,
              label          = paste0(nonchamp$name, " (Non-Champion)"),
              clusterOptions = nonchampion_cluster_opts
            )
        }

        gf.map = gf.map %>%
          addLayersControl(
            baseGroups    = c("OpenStreetMap", "No Background"),
            overlayGroups = c(admin.levels, "Champions", "Non-Champions"),
            options = layersControlOptions(collapsed = TRUE)
          )

        cat('\n - add legend')

        gf.map = gf.map %>%
          addLegend(
            "bottomright",
            colors  = c("#8B0000", "#333333"),
            labels  = c("Consistent Reporting (Champion)", "Inconsistent Reporting"),
            title   = 'Reporting Consistency',
            opacity = 0.8
          )

        # Zoom to facility bounding box so markers are visible on load
        fac_bbox <- tryCatch(sf::st_bbox(facilities), error = function(e) NULL)
        if (!is.null(fac_bbox)) {
          gf.map <- gf.map %>%
            fitBounds(lng1 = unname(fac_bbox["xmin"]), lat1 = unname(fac_bbox["ymin"]),
                      lng2 = unname(fac_bbox["xmax"]), lat2 = unname(fac_bbox["ymax"]))
        }

        #   options = popupOptions(closeButton = FALSE)

        cat('\n - done')

        return(gf.map)
      })

      chartModuleServer("facility_chart", reactive({ facility_chart() }))

      output$facility_map <- renderLeaflet({
        facility_map()
      })

      output$facility_table <- DT::renderDT({
        ln      <- levelNames()
        prov_col <- if (length(ln) >= 2L) ln[2L] else NULL

        drop_cols <- c("parentGraph", "groups", "medianValueRange",
                       "medianValueRangeSize", "level")

        df <- champion_facilities() %>%
          st_drop_geometry() %>%
          select(-any_of(drop_cols))

        # Put province column first so table is easy to sort/filter by region
        lead_cols <- c(prov_col, "name", "champion", "medianValue")
        lead_cols <- lead_cols[lead_cols %in% names(df)]
        rest_cols <- setdiff(names(df), lead_cols)
        df <- df[, c(lead_cols, rest_cols), drop = FALSE]

        # Rename for display
        rename_map <- c(
          name         = "Facility",
          champion     = "Reporting",
          medianValue  = "Median Value"
        )
        if (!is.null(prov_col)) rename_map[[prov_col]] <- "Province"
        names(df)[names(df) %in% names(rename_map)] <-
          rename_map[names(df)[names(df) %in% names(rename_map)]]

        num_cols <- names(df)[sapply(df, is.numeric)]

        DT::datatable(
          df,
          rownames  = FALSE,
          filter    = "top",
          options   = list(
            DToptions_no_buttons(),
            scrollX    = TRUE,
            scrollY    = "60vh",
            paging     = FALSE,
            columnDefs = list(
              list(className = "dt-left",   targets = "_all"),
              list(className = "dt-right",
                   targets  = which(names(df) %in% num_cols) - 1L)
            )
          )
        )
      })

      # Return ####
      split = reactive({
        input$split
      })
      startingMonth = reactive({
        input$startingMonth
      })
      endingMonth = reactive({
        input$endingMonth
      })
      missing_reports = reactive({
        as.integer(input$missing_reports)
      })

      return(
        list(
          dates = dates,
          # dataset = dataset ,
          d = d,
          # data.hts = data.hts ,
          data.total = data.total,
          aggregateselected_data = aggregateselected_data,
          period = period,
          group_by_cols = group_by_cols,
          levelNames = levelNames,
          split = split,
          startingMonth = startingMonth_debounced,
          endingMonth = endingMonth_debounced,
          missing_reports = missing_reports,
          num_datasets = num_datasets,
          num_facilities = num_facilities,
          selected_data = reactive({ cached_selected_data$value }),
          caption.text = caption.text,
          reportingSelectedOUs = reportingSelectedOUs,
          selected_data_categories = reactive({ selected_data_categories$elements })
        )
      )
    }
  )
}
