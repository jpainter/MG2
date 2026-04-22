evaluation_widget_ui = function(id) {
  ns <- NS(id)
  # fillCol( height = 600, flex = c(NA ) ,

  tagList(
    shinybusy::add_busy_spinner(
      spin = "fading-circle", # "self-building-square",
      position = 'bottom-right'
      # , margins = c(70, 1200)
    ),

    fillPage(
      tabsetPanel(
        type = "tabs",
        # add_busy_spinner(spin = "fading-circle", position = "bottom-right") ,

        tabPanel(
          "",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              # width = "25%" ,

              tabsetPanel(
                tabPanel(
                  "Data",

                  selectInput(
                    ns("reporting"),
                    label = "Select facilities based on reporting",
                    choices = c("All", "Champion", "Non-Champion"),
                    selected = 1
                  ),

                  selectInput(
                    ns("outliers"),
                    # label = "Filter out data flagged by outlier algorithm" ,
                    label = "Keep original data or remove data with following error flags:",
                    choices = c(
                      "Original",
                      "mad15",
                      "mad10",
                      "seasonal5",
                      "seasonal3"
                    ),
                    selected = 1
                  ),

                  selectInput(
                    ns("split_data"),
                    label = "Split data by:",
                    choices = 'None',
                    selected = 1
                  )
                ),

                tabPanel(
                  "Models",
                  inputPanel(
                    selectInput(
                      ns("model"),
                      label = "Time-series model:",
                      choices = c(
                        'TSLM (trend)',
                        'TSLM (trend+season)',
                        'ETS',
                        'ARIMA'

                        # 'SNAIVE'
                        # , 'NNETAR' , 'Prophet'
                      ),

                      selected = 'ETS'
                    ),

                    checkboxInput(
                      ns("pre_evaluation"),
                      label = 'Pre-intervention model fit',
                      value = FALSE
                    ),

                    checkboxInput(
                      ns("evaluation"),
                      label = 'Post-intervention evaluation',
                      value = FALSE
                    ),

                    textInput(
                      ns('model.formula'),
                      'Model Formula',
                      value = "total ~ error() + trend() + season()"
                    ),

                    textInput(
                      ns('covariates'),
                      'Model covariates',
                      value = NULL
                    ),

                    checkboxInput(
                      ns("transform"),
                      label = 'Log transform count data',
                      value = TRUE
                    ),

                    checkboxInput(
                      ns("smooth"),
                      label = 'Show smoothed trend line (loess)',
                      value = FALSE
                    ),

                    checkboxInput(
                      ns("scale"),
                      label = 'Scale values (x-mean)/sd + 1)',
                      value = FALSE
                    ),

                    checkboxInput(
                      ns('components'),
                      label = 'Visualize trend',
                      value = FALSE
                    ),

                    checkboxInput(
                      ns("forecast_ci"),
                      label = 'Prediction interval',
                      value = TRUE
                    ),

                    # checkboxInput( ns( "annualChange" ) , label ='Show legend',
                    #              value = FALSE  )

                    # checkboxInput( ns( "bootstrap" ) , label ='Bootstrap estimate',
                    #                value = FALSE  ) ,

                    # checkboxInput( ns( "autoModel" ) , label ='Automatic nmodel selection',
                    #                value = FALSE  ) ,

                    checkboxInput(
                      ns("ensemble"),
                      label = 'Use ensemble models',
                      value = TRUE
                    ),

                    selectInput(
                      ns("replicates"),
                      label = "Forecasting replicates:",
                      choices = c(100, 500, 1000, 5000),
                      selected = 1
                    )
                  )
                ),

                tabPanel(
                  "Stratifications",
                  # checkboxInput( ns('hts'), label = "hts across full admin hierarchy",
                  #        value = FALSE ) ,

                  # selectInput( ns("hts_level") , label = "Aggregate only from level:" ,
                  #       choices = 1:6 ,
                  #       selected = 1 ) ,

                  selectInput(
                    ns("agg_level"),
                    label = "Aggregate to admin level:",
                    choices = NULL,
                    selected = 1
                  ),

                  # selectInput( ns( "agg_method") , label = "Aggregate (regression) method:" ,
                  #     choices = c( "None", "Bottum up", "MINT(ols)" , "MINT(cov)") ,
                  #     selected = 1  ) ,

                  checkboxInput(
                    ns("facet_admin"),
                    label = "Facet by admin",
                    value = TRUE
                  ),

                  checkboxInput(
                    ns("facet_split"),
                    label = "Facet by split",
                    value = FALSE
                  ),

                  checkboxInput(
                    ns("label"),
                    label = 'Show labels',
                    value = FALSE
                  ),

                  checkboxInput(
                    ns("pe"),
                    label = 'Show mean percent difference from expected',
                    value = TRUE
                  ),

                  checkboxInput(
                    ns("legend"),
                    label = 'Show legend',
                    value = FALSE
                  )
                )
              )
            ),

            mainPanel(
              width = 9,

              htmlOutput(ns("region_filter_status")),

              inputPanel(
                selectizeInput(
                  ns("evaluation_month"),
                  label = "Evaluation Start",
                  choices = NULL,
                  selected = NULL
                ),

                # div(id = "expr-container",
                selectInput(
                  ns("horizon"),
                  label = "Months of Evaluation:",
                  choices = c(3, 6, 12, 18, 24, 36),
                  selected = 12
                ),

                actionButton(ns("forecast"), "Calculate Percent Change"),

                selectInput(
                  ns("selected_model"),
                  label = "Model:",
                  choices = character(0),
                  selected = NULL
                )
              ),

              tabsetPanel(
                tabPanel(
                  "Time-Series Chart",

                  fluidPage(
                    fluidRow(
                      style = "height:60vh;",

                      chartModuleUI(ns('plotOutput'), "Trend Analysis")
                    ),

                    fluidRow(tableOutput(ns("forecastResult")))
                  )
                ),

                tabPanel("Annual Change", uiOutput(ns("annualTable"))),

                tabPanel(
                  "Validation table",
                  h5(
                    "Symmetric Weighted Absolute Percent Error (SWAPE) of difference between the test forecasts and the actual values"
                  ),
                  htmlOutput(ns("modelLegend")),

                  fluidRow(
                    style = "height:55vh;",
                    column(
                      12,
                      div(
                        DT::dataTableOutput(ns('wpeValidationTable')),
                        style = "font-size: 60%; width: 100%"
                      )
                    )
                  )
                ),
                tabPanel(
                  "Posterior Distribution of Change",
                  h5(
                    "Weighted Absolute Percent Error (WAPE) of difference between predicted values and the actual values"
                  ),

                  fluidRow(
                    style = "height:60vh;",
                    column(
                      12,
                      chartModuleUI(ns("wpeHistogram"), "WPE Histogram")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

evaluation_widget_server <- function(
  id,
  directory_widget_output = NULL,
  metadata_widget_output = NULL,
  data_widget_output = NULL,
  reporting_widget_output = NULL,
  cleaning_widget_output = NULL,
  regions_widget_output = NULL,
  current_tab = NULL
) {
  moduleServer(
    id,
    function(input, output, session) {
      # Force all future operations to run sequentially
      plan(sequential)

      testing = FALSE

      options(shiny.trace = FALSE)
      options(shiny.reactlog = FALSE)
      options(dplyr.summarise.inform = FALSE)

      # cat('\n**Starting Reporting Widget\n')

      # Region filter status — data is already filtered upstream via reporting/cleaning
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
      # dataset = reactive({ data_widget_output$data1() })

      data1 = reactive({
        data_widget_output$data1()
      })

      data.total = reactive({
        reporting_widget_output$data.total()
      })
      selected_data = reactive({
        reporting_widget_output$selected_data()
      })

      # Apply region filter to selected_data before it enters mable_data()
      region_filtered_selected_data = reactive({
        req(selected_data())
        d  <- selected_data()
        sr <- regions_selected()
        ln <- levelNames()

        if (!is_empty(sr$level2) && length(ln) >= 2 && ln[2] %in% names(d))
          d <- d[d[[ln[2]]] %in% sr$level2, ]
        if (!is_empty(sr$level3) && length(ln) >= 3 && ln[3] %in% names(d))
          d <- d[d[[ln[3]]] %in% sr$level3, ]
        if (!is_empty(sr$level4) && length(ln) >= 4 && ln[4] %in% names(d))
          d <- d[d[[ln[4]]] %in% sr$level4, ]
        if (!is_empty(sr$level5) && length(ln) >= 5 && ln[5] %in% names(d))
          d <- d[d[[ln[5]]] %in% sr$level5, ]
        d
      })

      data2 = reactive({
        cleaning_widget_output$data2()
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

      # dates = reactive({ reporting_widget_output$dates() })
      # dataset = reactive({ reporting_widget_output$data1() })
      # data.hts = reactive({ reporting_widget_output$data.hts() })
      levelNames = reactive({
        reporting_widget_output$levelNames()
      })
      period = reactive({
        reporting_widget_output$period()
      })
      group_by_cols = reactive({
        reporting_widget_output$group_by_cols()
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
      missing_reports = reactive({
        reporting_widget_output$missing_reports()
      })
      num_datasets = reactive({
        reporting_widget_output$num_datasets()
      })
      num_facilities = reactive({
        reporting_widget_output$num_facilities()
      })

      caption.text = reactive({
        reporting_widget_output$caption.text()
      })

      aggregateselected_data = reactive({
        reporting_widget_output$aggregateselected_data()
      })
      reportingSelectedOUs = reactive({
        reporting_widget_output$reportingSelectedOUs()
      })

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

      # Dates

      dates = reactive({
        req(!is.null(current_tab) && current_tab() == "Evaluation")
        req(selected_data())

        cat('\n* evaluation_widget dates():')
        .period = period()

        dates = data1() %>% pull(!!rlang::sym(.period)) %>% unique

        dates = dates[order(dates)]

        # dates = setDT( data1() )[ , base::get( .period ) ] %>%
        #   unique

        # print( dates )
        # print( max( dates ))
        print('end dates()')
        return(dates)
      })

      observeEvent(
        dates(),
        {
          cat('\n* evaluation_widget update evaluation_month:')
          updateSelectInput(
            session,
            'evaluation_month',
            choices = dates(),
            # selected = dates()[ round(length(dates())/2) ]
            selected = max(dates()) - 12
            # ifelse( period() %in% 'Month' ,
            #                  dates()[12],
            #                  dates()[52] )
            # length(  dates()  ) - 12  ,
            # length(  dates()  ) - 12            )
          )
        }
      )

      # Model ####
      # level names
      observeEvent(
        levelNames(),
        {
          cat('\n* evaluation_widget update agg_level:')
          updateSelectInput(
            session,
            'agg_level',

            choices = levelNames(),
            selected = levelNames()[1]
          ) # 12 months before latest date
        }
      )

      # Impact ####

      # Var_y
      observeEvent(
        data.total(),
        {
          req(!is.null(current_tab) && current_tab() == "Evaluation")
          cat('\n* evaluation_widget update var_y:')
          updateSelectInput(session, 'var_y', choices = names(data.total()))
        }
      )

      # evaluation date
      observeEvent(
        dates(),
        {
          cat('\n* evaluation_widget update evaluation_month:')
          updateSelectInput(
            session,
            'evaluation_month',
            choices = dates(),
            # selected = dates()[ round(length(dates())/2) ]
            selected = max(dates()) - 11
            # ifelse( period() %in% 'Month' ,
            #                  dates()[12],
            #                  dates()[52] )
            # length(  dates()  ) - 12  ,
            # length(  dates()  ) - 12            )
          )
        }
      )

      # Split plot
      observeEvent(
        split(),
        {
          cat('\n* evaluation_widget update split:')
          if (!split() %in% 'None') {
            print("split():")
            print(split())
            # print( "data.total():" ); # glimpse( data.total() )

            # splits = data.total() %>% pull( .data[[ split() ]] ) %>% unique

            splits = data.total()[, split()] %>% unique

            print(paste('splits: ', splits))

            updateSelectInput(
              session,
              'split_data',
              choices = c('None', splits)
            )

            updateSelectInput(
              session,
              'filter_display',
              choices = c('None', splits)
            )
          } else {
            updateSelectInput(session, 'split_data', choices = c('None'))
            updateSelectInput(session, 'filter_display', choices = c('None'))
          }
        }
      )

      backtick <- function(x) paste0("`", x, "`")

      levelNames = reactive({
        req(orgUnits())
        cat('\n* evaluation_widget levelNames():')
        l = count(orgUnits() %>% as_tibble, level, levelName) %>%
          arrange(level) %>%
          pull(levelName)

        # l = setDT( orgUnits )[ , .(n = uniqueN(order_no)), by = c("level", "levelName") ]

        l = l[!is.na(l)]
        cat('\n - end levelNames():')
        return(l)
      })

      levels = reactive({
        req(orgUnits())
        cat('\n* evaluation_widget levels():')
        levels =
          count(orgUnits() %>% as_tibble, level, levelName) %>%
          arrange(level)
        cat('\n - end levels():')
        return(levels)
      })

      sub_agg_level = reactive({
        req(levels())
        cat('\n* evaluation_widget sub_agg_level:')
        x = levels() %>%
          mutate(parent = dplyr::lag(levelName)) %>%
          filter(parent == input$agg_level) %>%
          pull(levelName)
        cat('\n - done:', x)
        if (is.na(x)) {
          return(NULL)
        }
        return(x)
      })

      MAPE = reactive({
        req(tsPreForecast())
        cat('\n* evaluation_widget MAPE()')

        predicted = tsPreForecast() %>% as_tibble() %>% select(-total)
        actual = mable_Data()
        d = predicted %>%
          inner_join(actual, by = period())

        e = d %>%
          as_tibble() %>%
          # group_by( orgUnit , data  )  %>%
          summarise(
            mape = ifelse(
              mean(total, na.rm = T) > 0,
              mean(abs(total - .mean), na.rm = T) /
                mean(total, na.rm = T),
              NA
            )
          )

        cat('\n* - ', e$mape)
        return(scales::percent(e$mape))
      })

      key.mape = reactive({
        req(tsPreForecast())
        req(mable_Data())

        cat('\n* evaluation_widget key.mape()')

        predicted = tsPreForecast() %>%
          rename(pred = .mean)

        actual = mable_Data() %>%
          rename(actual = total)

        keyvars = key_vars(actual)
        cat('\n - keyvars', keyvars)

        truth = predicted %>%
          inner_join(actual, by = c(period(), keyvars))

        cat('\n - truth') #print( truth )

        mid_point = round(as.integer(input$horizon) / 2)

        e = truth %>%
          group_by_key() %>%
          index_by(1) %>%
          summarise(
            mape = ifelse(
              mean(pred, na.rm = T) > 0,
              mean(abs(actual - pred), na.rm = T) /
                mean(pred, na.rm = T),
              NA
            ),
            !!rlang::sym(period()) := nth(!!rlang::sym(period()), mid_point),
            actual = ifelse(
              mape >= 0,
              max(actual, na.rm = TRUE),
              min(actual, na.rm = TRUE)
              #nth( actual , mid_point )
            ),
            just = ifelse(mape >= 0, 2, -2)
          ) %>%
          as_tibble() %>%
          mutate(
            !!input$agg_level := as.character(!!rlang::sym(input$agg_level))
          )

        if (!split() %in% 'None') {
          cat('\n - key.mape grouping_var', split())
          e = e %>%
            mutate(
              grouping_var = as.character(!!rlang::sym(split()))
            )
        } else {
          e = e %>%
            mutate(grouping_var = 'Total')
        }

        # print( "end key.mape"); #glimpse(e )
        return(e)
      })

      MPE = reactive({
        req(tsForecast())

        cat('\n* evaluation_widget MPE()')

        predicted = tsForecast() %>% as_tibble() %>% select(-total)
        actual = mable_Data()

        d = predicted %>%
          inner_join(actual, by = period())

        e = d %>%
          as_tibble() %>%
          # group_by( orgUnit , data  )  %>%
          summarise(
            mpe = ifelse(
              mean(.mean, na.rm = T) > 0,
              mean(total - .mean, na.rm = T) /
                mean(.mean, na.rm = T),
              NA
            )
          )

        cat("\n - ", e$mpe)
        return(scales::percent(e$mpe))
      })

      key.mpe = reactive({
        req(tsForecast())
        req(mable_Data())

        cat('\n* evaluation_widget key.mpe()')

        predicted = tsForecast() %>%
          rename(pred = .mean)

        actual = mable_Data() %>%
          rename(actual = total)

        keyvars = key_vars(actual)
        cat('\n - keyvars', keyvars)

        truth = predicted %>%
          inner_join(actual, by = c(period(), keyvars))

        # print( 'truth'); #print( truth )

        mid_point = round(as.integer(input$horizon) / 2)

        e = truth %>%
          group_by_key() %>%
          index_by(1) %>%
          summarise(
            mpe = ifelse(
              mean(pred, na.rm = T) > 0,
              mean(actual - pred, na.rm = T) /
                mean(pred, na.rm = T),
              NA
            ),
            !!period() := nth(!!rlang::sym(period()), mid_point),
            actual = ifelse(
              mpe >= 0,
              max(actual, na.rm = TRUE),
              min(actual, na.rm = TRUE)
              #nth( actual , mid_point )
            ),
            just = ifelse(mpe >= 0, 1, -1)
          ) %>%
          as_tibble() %>%
          mutate(
            !!input$agg_level := as.character(!!rlang::sym(input$agg_level))
          )

        if (!split() %in% 'None') {
          cat('\n - key.mape grouping_var', split())
          e = e %>%
            mutate(
              grouping_var = as.character(!!rlang::sym(split()))
            )
        } else {
          e = e %>%
            mutate(grouping_var = 'Total')
        }

        cat("\n - mpe") #glimpse(e )
        return(e)
      })

      pi_levels = reactive({
        # req( input$forecast_ci )
        cat('\n* pi_levels:', input$forecast_ci)
        if (!input$forecast_ci) {
          return(NULL)
        }
        return(90)
      })

      # Model forecasts ####

      evaluationParameters <- reactiveValues(Month = NULL)

      model_formula = reactive({
        req(input$model.formula)
        cat('\n* evaluation_widget model_formula')

        formula.string = input$model.formula

        # if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  )'
        #
        # f = as.formula(  formula.string )
        #
        #
        # if (input$model %in% 'ARIMA' ){
        #   cat("\n - input$model = ARIMA")
        #
        #   # formula.string = paste( 'fabletools::box_cox( total , lambda = .5  ) ~ ',
        #   #                         ' pdq() ' )
        #
        #   formula.string = ' total ~  pdq() '
        #   if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~  pdq() '
        #
        #   if ( period() %in% "Month" ) formula.string = paste0( formula.string ,
        #                                                      '+ PDQ( period = "1 year" )'   )
        #
        #   if ( period() %in% "Week" ) formula.string = paste0( formula.string ,
        #                                                      '+ PDQ( period = 52 )'   )
        #
        #
        #    if ( nchar( input$covariates ) > 0 ) formula.string =
        #        paste( formula.string , '+ xreg(' , input$covariates , ' ) '  )
        #
        #    cat( '\n - ARIMA formula string:', formula.string )
        #    f = as.formula( formula.string )
        # }
        #
        # if (input$model %in% 'BSTS' ){
        #   cat("\n - input$model = BSTS")
        #
        #    f = as.formula( paste( 'total ~ season("year")' ) )
        #
        #   if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ season("year")'
        #
        # }
        #
        # if (input$model %in% 'ETS' ){
        #   cat("\n - input$model = ETS")
        #
        #   f = as.formula( paste( 'total ~ season("year")' ) )
        #
        #   if ( input$transform ) formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ error() + trend() + season()'
        #
        # }

        cat('\n - end model_formula:', formula.string)
        return(formula.string)
      })

      # Models formulas
      modelSpecs <- reactive({
        req(input$model)
        specs = list(input$model, input$transform)
        return(specs)
      })

      observeEvent(modelSpecs(), {
        if (input$model %in% 'TSLM (trend+season)') {
          cat("\n - input$model = TSLM")

          formula.string = "total ~ trend() + season()"

          if (input$transform) {
            formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ trend() + season()'
          }
        }

        if (input$model %in% 'TSLM (trend)') {
          cat("\n - input$model = TSLM (trend)")

          formula.string = "total ~ trend()"

          if (input$transform) {
            formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ trend()'
          }
        }

        if (input$model %in% 'ARIMA') {
          cat("\n - input$model = ARIMA")

          # formula.string = paste( 'fabletools::box_cox( total , lambda = .5  ) ~ ',
          #                         ' pdq() ' )

          formula.string = ' total ~  pdq() '

          if (input$transform) {
            formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~  pdq() '
          }

          if (period() %in% "Month") {
            formula.string = paste0(
              formula.string,
              '+ PDQ( period = "1 year" )'
            )
          }

          if (period() %in% "Week") {
            formula.string = paste0(formula.string, '+ PDQ( period = 52 )')
          }

          if (nchar(input$covariates) > 0) {
            formula.string =
              paste(formula.string, '+ xreg(', input$covariates, ' ) ')
          }
        }

        if (input$model %in% 'BSTS') {
          cat("\n - input$model = BSTS")

          formula.string = 'total ~ season("year")'

          if (input$transform) {
            formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ season()'
          }
        }

        if (input$model %in% 'ETS') {
          cat("\n - input$model = ETS")

          formula.string = 'total ~ error() + trend() + season()'

          if (input$transform) {
            formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ error() + trend() + season()'
          }
        }

        if (input$model %in% 'NNETAR') {
          cat("\n - input$model = NNETAR")

          formula.string = 'total'

          if (input$transform) {
            formula.string = 'fabletools::box_cox( total , lambda = .5  )'
          }
        }

        if (input$model %in% 'SNAIVE') {
          cat("\n - input$model = SNAIVE")

          formula.string = 'total ~ lag("year")'

          if (input$transform) {
            formula.string = 'fabletools::box_cox( total , lambda = .5  ) ~ lag("year")'
          }
        }

        updateTextInput(session, "model.formula", value = formula.string)
      })

      # observeEvent( input$forecast , { cat("\n * observed button push") })

      # Auto Model ####

      # Set up parallel processing with proper RNG
      # plan( multisession, workers = min(4, availableCores() - 1) )
      plan(sequential)
      options(future.rng.onMisuse = "warning") # Keep warnings enabled

      # Use reactiveValues to store results across both phases
      auto_model_values <- reactiveValues(
        computing        = FALSE,
        validation_done  = FALSE,
        done             = FALSE,   # TRUE only after evaluation completes
        model_output     = NULL,
        # Phase-2 inputs stored after validation, used to run evaluation for chosen model:
        modeling_data    = NULL,
        n_forecasts_val  = NULL,
        type_val         = NULL,
        ensemble_val     = NULL,
        ntestmonths_val  = NULL
      )

      # Tracks whether the model dropdown has been populated for the current validation run.
      # Prevents wpeValidationTable() changes (caused by Phase 2 updating model_output)
      # from resetting the dropdown back to models[1] every time Phase 2 completes.
      dropdown_initialized <- reactiveVal(FALSE)

      # Helper: clear predictions only (keep validation table visible)
      clear_predictions <- function() {
        current <- isolate(auto_model_values$model_output)
        if (!is.null(current)) {
          current$predicted <- NULL
          auto_model_values$model_output <- current
        }
        auto_model_values$done <- FALSE
      }

      # When underlying data changes (region, reporting filter, outlier filter,
      # agg level, split), erase any predictions so stale forecasts are not shown.
      observeEvent(mable_Data(), ignoreNULL = TRUE, ignoreInit = TRUE, {
        clear_predictions()
      })

      # When model/validation parameters change, also erase predictions and
      # invalidate validation so user must re-run "Calculate Percent Change".
      observeEvent(
        list(input$evaluation_month, input$horizon, input$replicates,
             input$ensemble, input$transform),
        ignoreNULL = TRUE, ignoreInit = TRUE, {
          clear_predictions()
          auto_model_values$validation_done <- FALSE
          dropdown_initialized(FALSE)
        }
      )

      # Phase 1: Validation — fit all models on train/test data ####
      observeEvent(input$forecast, {
        req(mable_Data())
        req(input$evaluation_month)

        cat("\n* observeEvent(input$forecast: validation phase)")
        showModal(modalDialog("Running model validation...", footer = NULL))

        auto_model_values$computing       <- TRUE
        auto_model_values$validation_done <- FALSE
        auto_model_values$done            <- FALSE
        auto_model_values$model_output    <- NULL
        dropdown_initialized(FALSE)   # allow dropdown to be repopulated after new validation

        withProgress(message = 'Validating models...', value = 0, {
          data             = isolate(mable_Data())
          model_data       = as_tsibble(data)
          startMonth       = min(model_data$Month, na.rm = T)
          evaluation_month = yearmonth(isolate(input$evaluation_month))
          numberTestMonths = as.integer(isolate(input$horizon))
          endEvalMonth     = evaluation_month + numberTestMonths
          ensemble         = input$ensemble
          n_forecasts      = as.integer(isolate(input$replicates))
          .type = switch(as.character(input$transform),
                         "TRUE" = 'transform', "FALSE" = NA)

          incProgress(0.1, detail = "Preparing data")

          modelingData = dataset(
            data = model_data,
            startMonth = startMonth,
            startEvalMonth = evaluation_month,
            numberTestMonths = 12,
            endEvalMonth = endEvalMonth,
            unadjusted = TRUE,
            grouping = FALSE
          )

          # Store for Phase 2 (run before future so they're accessible in the observer)
          auto_model_values$modeling_data   <- modelingData
          auto_model_values$n_forecasts_val <- n_forecasts
          auto_model_values$type_val        <- .type
          auto_model_values$ensemble_val    <- ensemble
          auto_model_values$ntestmonths_val <- numberTestMonths

          train_data = modelingData$pre.intervention.train
          test_data  = modelingData$pre.intervention.test

          incProgress(0.2, detail = "Training models")

          fut <- future(seed = TRUE, {
            test.forecasts = tsmodels(
              train_data, test_data,
              n_forecasts = n_forecasts,
              .var = 'total', numberForecastMonths = 12,
              type = .type, covariate = NULL, ensemble = ensemble,
              msg = TRUE, .set.seed = TRUE
            )

            validations    = model_metrics(test.forecasts, test_data, .var = 'total')
            model_selection = modelSelection(validations, type = 'synchronize')
            cat("\n * model_selection:", model_selection$.model)

            list(
              actual         = modelingData$fable.data,
              train_data     = modelingData$pre.intervention.train,
              test_data      = modelingData$pre.intervention.test,
              test.forecasts = test.forecasts,
              validations    = validations,
              model_selection = model_selection,
              predicted      = NULL   # filled by Phase 2
            )
          })

          observe({
            if (resolved(fut)) {
              result <- tryCatch(value(fut), error = function(e) {
                cat("\n-- Validation failed:", e$message)
                showModal(modalDialog(title = "Error",
                  paste("Validation failed:", e$message), footer = modalButton("OK")))
                NULL
              })
              auto_model_values$model_output    <- result
              auto_model_values$validation_done <- TRUE
              auto_model_values$computing       <- FALSE
              removeModal()
            } else {
              invalidateLater(100)
            }
          })
        })
      })

      # Phase 2: Evaluation — run only the selected model on the full pre-intervention data ####
      # Fires whenever the model selector changes (including first population after validation).
      observeEvent(input$selected_model, ignoreInit = TRUE, ignoreNULL = TRUE, {
        req(auto_model_values$validation_done)
        req(!auto_model_values$computing)
        req(!is.null(auto_model_values$modeling_data))
        req(nchar(input$selected_model) > 0)

        sel_model    <- tolower(input$selected_model)
        modelingData <- auto_model_values$modeling_data
        n_forecasts  <- auto_model_values$n_forecasts_val
        .type        <- auto_model_values$type_val
        ensemble     <- auto_model_values$ensemble_val
        ntestmonths  <- auto_model_values$ntestmonths_val

        # Primary model abbreviations used in tsmodels()
        primary_model_names <- c("a", "e", "n", "t", "p1", "p4", "p8")

        # If the selected model is a primary model, fit only that one.
        # Otherwise it is a combination (e.g. "aentp1") — fit all primary models
        # with ensemble = TRUE so combination_forecasts() rebuilds it, then filter.
        if (sel_model %in% primary_model_names) {
          base_names   <- sel_model
          run_ensemble <- FALSE
        } else {
          base_names   <- NULL   # tsmodels will use all primary models
          run_ensemble <- TRUE
        }

        cat("\n* Phase 2 evaluation for model:", sel_model,
            " base_models:", paste(base_names, collapse = ","))
        showModal(modalDialog(
          paste("Estimating prediction with", input$selected_model, "model..."),
          footer = NULL
        ))

        auto_model_values$computing <- TRUE
        auto_model_values$done      <- FALSE

        fut2 <- future(seed = TRUE, {
          evaluation.forecasts <- tsmodels(
            train_data           = modelingData$pre.intervention,
            test_data            = modelingData$post.intervention,
            n_forecasts          = n_forecasts,
            .var                 = 'total',
            numberForecastMonths = ntestmonths,
            type                 = .type,
            covariate            = NULL,
            ensemble             = run_ensemble,
            base_models          = base_names,
            msg                  = TRUE,
            .set.seed            = TRUE
          ) %>% dplyr::filter(.model == sel_model)

          evaluation.forecasts
        })

        observe({
          if (resolved(fut2)) {
            pred <- tryCatch(value(fut2), error = function(e) {
              cat("\n-- Evaluation failed:", e$message)
              showModal(modalDialog(title = "Error",
                paste("Evaluation failed:", e$message), footer = modalButton("OK")))
              NULL
            })
            cat("\n* Phase 2 inner observer: resolved. pred is",
                if (is.null(pred)) "NULL" else paste0(nrow(pred), " rows"))
            if (!is.null(pred) && nrow(pred) > 0)
              cat("\n  .model values:", paste(unique(pred$.model), collapse = ", "))
            # isolate() prevents reading model_output from creating a reactive
            # dependency that would re-trigger this observer (infinite loop)
            current <- isolate(auto_model_values$model_output)
            if (!is.null(pred) && !is.null(current) && nrow(pred) > 0) {
              current$predicted          <- pred
              auto_model_values$model_output <- current
              auto_model_values$done         <- TRUE
              cat("\n* Phase 2: done = TRUE, predicted stored")
            } else if (is.null(pred)) {
              cat("\n-- Evaluation returned NULL; charts will remain empty")
            } else if (is.null(current)) {
              cat("\n-- current model_output is NULL; phase 1 result missing")
            } else {
              cat("\n-- Evaluation returned 0 rows; model name may not match filter")
            }
            auto_model_values$computing <- FALSE
            removeModal()
          } else {
            invalidateLater(100)
          }
        })
      })

      # Access results with reactive
      auto_model <- reactive({
        auto_model_values$model_output
      })

      # Check if computing
      is_computing <- reactive({
        auto_model_values$computing
      })

      # Annual Table ####
      output$annualTable <- renderUI({
        req(mable_Data())

        cat('\n* evaluation_widget annualTable():')

        mable_Data = mable_Data()

        # cat("\n - yearly_summary_table " )
        ft = yearly_summary_table(data = mable_Data)

        cat("\n - class(ft):", class(ft))
        ft %>% htmltools_value()
      })

      # forecastResult table ####
      output$forecastResult <- renderTable({
        req(auto_model_values$done)
        req(auto_model())
        req(selected_predicted())
        cat("\n* output$forecastResult: impactSummary")
        wpe_summary(
          actual    = auto_model()$actual,
          predicted = selected_predicted(),
          .var      = 'total'
        )
      })

      wpeHistogram <- reactive({
        req(auto_model_values$done)
        req(auto_model())
        req(selected_predicted())
        req(nrow(selected_predicted()) > 0)
        cat("\n* wpeHistogram")

        auto_model = auto_model()
        pred <- selected_predicted() %>% mutate(Intervention = input$reporting)
        act  <- auto_model$actual    %>% mutate(Intervention = input$reporting)

        stats <- tryCatch(
          wpe_summary(actual = act, predicted = pred, .var = 'total'),
          error = function(e) NULL
        )

        h <- diffHistogram(actual = act, predicted = pred, .var = 'total') +
          labs(caption = paste0(region_caption_text(), " | Blue bar = median"))

        # Upper-left annotation: model name and WPE summary statistics
        if (!is.null(stats) && nrow(stats) > 0) {
          annotation_text <- paste(
            paste("Model:", input$selected_model),
            paste("Mean:",   round(stats$mean[1],   1), "%"),
            paste("SD:",     round(stats$sd[1],     1), "%"),
            paste("Median:", round(stats$median[1], 1), "%"),
            sep = "\n"
          )
          h <- h + annotate(
            "label",
            x = -Inf, y = Inf,
            label    = annotation_text,
            hjust    = -0.05, vjust = 1.1,
            size     = 3,
            alpha    = 0.85,
            label.size = 0.3
          )
        }

        return(h)
      })

      wpeValidationTable = reactive({
        req(auto_model())
        auto_model = auto_model()
        auto_model$validations %>%
          arrange(swape) %>%
          rename(SWAPE = swape, Model = .model) %>%
          mutate(Model = toupper(Model))
      })

      # Populate the model selector from the validation table (best model first).
      # Only fires once per validation run — dropdown_initialized() prevents
      # Phase 2 updates to model_output (which invalidate wpeValidationTable)
      # from resetting the user's model selection back to models[1].
      observeEvent(wpeValidationTable(), {
        if (!dropdown_initialized()) {
          vt <- wpeValidationTable()
          models <- vt$Model  # already uppercased, sorted by SWAPE ascending
          updateSelectInput(session, "selected_model",
            choices  = models,
            selected = models[1]
          )
          dropdown_initialized(TRUE)
        }
      })

      output$modelLegend <- renderUI({
        div(
          style = "font-size:0.82em; color:#555; margin:2px 0 6px 0;",
          tags$strong("Abbreviations: "),
          "A = ARIMA | E = ETS | N = NNETAR | T = TSLM | P1 = Prophet (1 seasonal) | ",
          "P4 = Prophet (4 seasonals) | P8 = Prophet (8 seasonals) | ",
          "COMBINATION_X_Y = Ensemble of models X and Y",
          tags$br(),
          tags$strong("Full names: "),
          "ARIMA = AutoRegressive Integrated Moving Average | ",
          "ETS = Exponential Smoothing | ",
          "NNETAR = Neural Network Autoregression | ",
          "TSLM = Time Series Linear Model | ",
          "Prophet = Additive decomposition (Meta/Facebook)"
        )
      })

      # Forecasts filtered to the user-selected model (only valid after phase 2)
      selected_predicted = reactive({
        req(auto_model_values$done)   # ensures predicted is populated, not NULL
        req(auto_model())
        req(input$selected_model)
        req(nchar(input$selected_model) > 0)
        auto_model()$predicted %>%
          dplyr::filter(.model == tolower(input$selected_model))
      })

      # WPE summary statistics for the selected model (used for chart annotations)
      selected_wpe_stats = reactive({
        req(auto_model())
        req(selected_predicted())
        pred <- selected_predicted()
        req(nrow(pred) > 0)
        tryCatch(
          wpe_summary(
            actual    = auto_model()$actual,
            predicted = pred,
            .var      = 'total'
          ),
          error = function(e) NULL
        )
      })

      output$wpeValidationTable =
        DT::renderDT(
          server = TRUE,
          DT::datatable(
            wpeValidationTable(),
            rownames = FALSE,
            filter = 'top',
            options = list(
              scrollY = "50vh",
              scrollX = TRUE,
              scrollCollapse = TRUE,
              paging = FALSE,
              searching = TRUE,
              info = TRUE,
              dom = 'ti'
            )
          )
        )

      ############### tsModel and tsPre-Model #####
      tsModel = reactive({
        req(mable_Data())
        req(model_formula())
        req(input$evaluation_month)

        mable_data = mable_Data()
        model_formula = model_formula()
        evaluation_month = input$evaluation_month
        period = period()
        model = input$model
        transform = input$transform

        #Testing
        # save( mable_data ,  model_formula , evaluation_month ,  period ,
        #       file = 'tsModelTesting.rda' )

        if (!input$evaluation) {
          return(NULL)
        }

        cat('\n* evaluation_widget tsModel():')
        cat(
          '\n - ',
          paste('available vars:', paste(names(mable_data), collapse = ','))
        )

        if (!exists("model.formula")) {
          model.formula = model_formula
        }
        cat('\n* evaluation_widget tsModel():', as.character(model.formula))

        # Dickey-Fuller test for stationary series
        # Null hypothese is non-stationary.
        # Evidence that series is stationary when p-v < .05
        # dickeyFuller = tseries::adf.test( mable_data()$total )
        # print( dickeyFuller )

        # Filter data to period just before evaluation start
        cat("\n -evaluation_month:", evaluation_month)
        eval_month = evaluation_month
        if (period %in% "Month") {
          time_period = yearmonth(eval_month)
        } # - month(1)
        if (period %in% "Week") {
          time_period = yearweek(eval_month)
        }

        # index( mable_data )
        # fit model with pre-intervention data
        fit.data = mable_data %>%
          filter(Month < time_period)

        if (grepl("~", model.formula, fixed = TRUE)) {
          model.formula = as.formula(model.formula)
        }

        if (model %in% 'TSLM (trend)') {
          fit = fit.data %>% model(l = TSLM(model.formula))

          cat('\n - end tsModel():')
          return(fit)
        }

        if (model %in% 'TSLM (trend+season)') {
          fit = fit.data %>% model(l = TSLM(model.formula))

          cat('\n - end tsModel():')
          return(fit)
        }

        if (model %in% 'ARIMA') {
          fit = fit.data %>%
            model(
              arima = ARIMA(model.formula)
            )
          # if ( input$reconcile ) fit = fit %>%
          #       reconcile(
          #         mint = min_trace(a, method = "mint_shrink")
          #         )

          cat('\n - end tsModel(): arima fit')
          # glimpse( fit )
          # testing model fit for forecasts

          # if ( input$covariates %in% c('ipti', 'doses') ) saveRDS( fit , 'arima.rds' )

          return(fit)
        }

        if (model %in% 'NNETAR') {
          fit = fit.data %>%
            model(
              nnetar = NNETAR(!!rlang::sym(model.formula)),
              times = 10
            )

          cat('\n - end tsModel():')
          return(fit)
        }

        if (model %in% 'BSTS') {
          fit = fit.data %>%
            model(
              # b = BSTS( model_formula() )
              bsts = BSTS(model.formula)
            )

          cat('\n - end tsModel():')
          return(fit)
        }

        if (model %in% 'ETS') {
          # if ( input$transform ){
          #   fit = fit.data %>% model( a = ETS( fabletools::box_cox( total , lambda = .5  ) )  )
          # } else {

          # fit = fit.data %>% model( a = ETS( total )  )
          #
          # if ( input$transform ) fit = fit.data %>% model( a = ETS( fabletools::box_cox( total , lambda = .5  )  ) )

          # }

          fit = fit.data %>% model(ets = ETS(!!model.formula))

          cat('\n - end ETS tsModel():')

          # if ( input$reconcile ) fit = fit %>%
          #       reconcile(
          #         mint = min_trace(a, method = "mint_shrink")
          #         )
          cat('\n - end tsModel():')
          return(fit)
        }

        if (model %in% 'SNAIVE') {
          fit = fit.data %>% model(snaive = SNAIVE(model.formula))

          cat('\n - end tsModel():')
          return(fit)
        }

        if (model %in% 'Prophet') {
          if (transform) {
            fit = fit.data %>%
              model(
                prophet = prophet(
                  fabletools::box_cox(total, lambda = .5) ~
                    growth(
                      type = 'linear',
                      changepoint_range = 1,
                      changepoint_prior_scale = 1,
                      # capacity = 1e5 ,
                      # floor = 0
                    ) +
                      season(period = 12, order = 4, type = 'multiplicative'),
                  seed = TRUE
                )
              )
          } else {
            fit = fit.data %>%
              model(
                prophet = prophet(
                  total ~
                    growth(
                      type = 'linear',
                      changepoint_range = 1,
                      changepoint_prior_scale = 1,
                      # capacity = 1e5 ,
                      # floor = 0
                    ) +
                      season(period = 12, order = 4, type = 'multiplicative'),
                  seed = TRUE
                )
              )
          }

          cat('\n - end tsPreModel() Prophet:')
          return(fit)
        }
      })

      tsPreModel = reactive({
        req(mable_Data())
        req(input$evaluation_month)
        req(model_formula())

        mable_data = mable_Data()
        model_formula = model_formula()
        evaluation_month = input$evaluation_month
        period = period()
        model = input$model
        transform = input$transform

        if (!input$pre_evaluation) {
          return(NULL)
        }
        cat('\n* evaluation_widget tsPreModel():', as.character(model_formula))

        eval_month = input$evaluation_month
        if (period() %in% "Month") {
          time_period = yearmonth(eval_month) - 12
        }
        if (period() %in% "Week") {
          time_period = yearweek(eval_month) - 52
        }

        cat("\n - time_period:", time_period)

        fit.data = mable_data %>% filter(Month < time_period)

        cat("\n - nrow(mable_data):", nrow(mable_data))
        cat("\n - nrow(fit.data:", nrow(fit.data))

        # Testing:
        # saveRDS( mable_data() , 'mable_data.rds' )
        # saveRDS( fit.data , 'fit.data.rds' )

        model.formula = model_formula
        if (grepl("~", model.formula, fixed = TRUE)) {
          model.formula = as.formula(model.formula)
        }

        if (input$model %in% 'TSLM (trend)') {
          fit = fit.data %>% model(l = TSLM(model.formula))

          cat('\n - end tsPreModel() TSLM(trend):')
          return(fit)
        }

        if (model %in% 'TSLM (trend+season)') {
          fit = fit.data %>% model(l = TSLM(model.formula))

          cat('\n - end tsPreModel() TSLM(trend + season):')
          return(fit)
        }

        if (model %in% 'ARIMA') {
          fit = fit.data %>%
            model(
              arima = ARIMA(model.formula)
            )
          # if ( input$reconcile ) fit = fit %>%
          #       reconcile(
          #         mint = min_trace(a, method = "mint_shrink")
          #         )

          cat('\n - end tsPreModel(): arima fit')
          # glimpse( fit )
          # testing model fit for forecasts

          # if ( input$covariates %in% c('ipti', 'doses') ) saveRDS( fit , 'arima.rds' )

          return(fit)
        }

        if (model %in% 'NNETAR') {
          fit = fit.data %>%
            model(
              nnetar = NNETAR(total)
            )

          if (input$transform) {
            fit = fit.data %>%
              model(nnetar = NNETAR(fabletools::box_cox(total, lambda = .5)))
          }

          cat('\n - end tsModel():')
          return(fit)
        }

        if (model %in% 'BSTS') {
          fit = fit.data %>%
            model(
              # b = BSTS( model_formula() )
              bsts = BSTS(model.formula)
            )

          cat('\n - end tsPreModel() BSTS:')
          return(fit)
        }

        if (model %in% 'ETS') {
          fit = fit.data %>% model(ets = ETS(!!model.formula))

          # if ( input$reconcile ) fit = fit %>%
          #       reconcile(
          #         mint = min_trace(a, method = "mint_shrink")
          #         )
          cat('\n - end tsModel():')
          return(fit)
        }

        if (model %in% 'SNAIVE') {
          fit = fit.data %>% model(ets = SNAIVE(model.formula))

          cat('\n - end tsModel():')
          return(fit)
        }

        if (model %in% 'Prophet') {
          if (transform) {
            fit = fit.data %>%
              model(
                prophet = prophet(
                  fabletools::box_cox(total, lambda = .5) ~
                    growth(
                      type = 'linear',
                      changepoint_range = 1,
                      changepoint_prior_scale = 1,
                      # capacity = 1e5 ,
                      # floor = 0
                    ) +
                      season(period = 12, order = 4, type = 'multiplicative'),
                  seed = TRUE
                )
              )
          } else {
            fit = fit.data %>%
              model(
                prophet = prophet(
                  total ~
                    growth(
                      type = 'linear',
                      changepoint_range = 1,
                      changepoint_prior_scale = 1,
                      # capacity = 1e5 ,
                      # floor = 0
                    ) +
                      season(period = 12, order = 4, type = 'multiplicative'),
                  seed = TRUE
                )
              )
          }

          cat('\n - end tsPreModel() Prophet:')
          return(fit)
        }
      })

      tsForecast = reactive({
        req(tsModel())
        req(input$horizon)
        cat('\n* evaluation_widget tsForecast()')

        mable_data = mable_Data()
        model_formula = model_formula()
        evaluation_month = input$evaluation_month
        period = period()
        model = input$model
        transform = input$transform
        horizon = input$horizon
        bootstrap = FALSE # input$bootstrap
        Reps = input$Reps
        covariates = input$covariates
        tsModel = tsModel()

        eval_month = yearmonth(evaluation_month)
        time_period = seq(
          eval_month,
          eval_month + as.integer(horizon) - 1,
          by = 1
        )

        if (period() %in% "Month") {
          time_period = yearmonth(evaluation_month)
        } # - month(1)
        if (period() %in% "Week") {
          time_period = yearweek(evaluation_month)
        }

        forecast.fit.data = mable_data %>%
          select(-total) %>%
          # filter_index( as.character( time_period ) ~ . ,
          #             .preserve = TRUE ) %>%
          filter(Month %in% time_period)

        # if ( input$bootstrap ){
        #
        #   fcast = model %>%
        #     fabletools::forecast( h = as.integer( horizon ) ,
        #               bootstrap = TRUE,
        #               times = as.integer( Reps )
        #     )
        # } else {
        #
        #   # if ( nchar( covariates ) > 0 ){
        #   #   cat( '\n - covariates')
        #   #
        #   #   fcast = tsModel %>% fabletools::forecast( new_data = forecast.fit.data )
        #   #
        #   # } else {
        #   #
        #   #   fcast = tsModel %>% fabletools::forecast(  h = as.integer( horizon ) )
        #   # }
        #   #   fcast = tsModel %>% fabletools::forecast(  h = as.integer( horizon ) )
        # }

        # fcast = tsModel %>% fabletools::forecast( h=14 )
        fcast = tsModel %>% fabletools::forecast(h = as.integer(horizon))
        #   fcast = tsModel %>% fabletools::forecast(  new_data = fd )
        # fcast = tsModel %>% fabletools::forecast( new_data = forecast.fit.data )

        # preserve tsibble key and index,
        indexVar = index_var(fcast)
        keyVars = key_vars(fcast)

        fcast = fcast %>%
          mutate(
            !!input$agg_level := as.character(!!rlang::sym(input$agg_level))
          )

        if (!split() %in% 'None') {
          cat('\n - tsForecast grouping_var', split())
          fcast = fcast %>%
            mutate(
              grouping_var = as.character(!!rlang::sym(split()))
            )
        } else {
          fcast = fcast %>%
            mutate(grouping_var = 'Total')
        }

        # Ensure result is tstiblle
        # fcast = fcast %>%
        #        as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
        #        fill_gaps( .full = TRUE  )

        # Reconcile
        # if ( input$agg_method %in% "None" ){
        #   if ( input$agg_method %in% 'Bottom up' ){
        #       fcast = fcast %>%
        #         reconcile( bu = bottom_up(base) )
        #   }
        #   if ( input$agg_method %in% 'MINT(ols)' ){
        #     fcast = fcast %>%
        #         reconcile( ols = min_trace(base, method = "ols") )
        #   }
        #   if ( input$agg_method %in% 'MINT(cov)' ){
        #     fcast = fcast %>%
        #         reconcile( mint = min_trace(base, method = "mint_cov") )
        #   }
        # }

        # saveRDS( fcast , 'tsForecast.rds')
        cat('\n - fcast end:') #glimpse( fcast )
        # print( names( fcast ) )
        return(fcast)
      })

      tsPreForecast = reactive({
        req(tsPreModel())
        req(input$horizon)
        req(input$evaluation_month)

        eval_month = yearmonth(input$evaluation_month)
        time_period = seq(eval_month - 11, eval_month, by = 1)

        cat('\n* evaluation_widget tsPreForecast')
        # if ( input$covariates %in% "avg_mm"){

        cat('\n - evaluation_widget test.data')
        # testing
        # saveRDS( mable_data() , 'mable_data.rds')
        # saveRDS( time_period , 'time_period.rds')
        # saveRDS( input$horizon , 'horizon.rds')

        test.data = mable_Data() %>%
          select(-total) %>%
          filter(Month %in% time_period)

        # fcast= getForecast( test_data = test.data , model = tsPreModel() ,
        #          bootstrap = FALSE , Reps = 1000 )

        if (nchar(input$covariates) > 0) {
          cat("\n - input$covariates:", input$covariates)
          if (period() %in% "Month") {
            time_period = yearmonth(eval_month)
          } # - month(1)
          if (period() %in% "Week") {
            time_period = yearweek(eval_month)
          }

          fcast = tsPreModel() %>% fabletools::forecast(new_data = test.data)
        } else {
          cat('\n - evaluation_widget tsPreModel() %>% fabletools::forecast')
          # Testing
          # saveRDS( tsPreModel() , 'tsPreModel.rds')
          # saveRDS( pi_levels() , 'pi_levels.rds' )

          if (period() %in% 'Month') {
            fcast = tsPreModel() %>%
              fabletools::forecast(h = "12 months", level = pi_levels())
          }
          if (period() %in% 'Week') {
            fcast = tsPreModel() %>%
              fabletools::forecast(h = "52 weeks", level = pi_levels())
          }
        }

        # preserve tsibble key and index,
        indexVar = index_var(fcast)
        keyVars = key_vars(fcast)

        cat('\n - tsPreForecast done.  Adding agg_level')

        fcast = fcast %>%
          mutate(
            !!input$agg_level := as.character(!!rlang::sym(input$agg_level))
          )

        cat('\n - tsPreForecast grouping_var', split())
        if (!split() %in% 'None') {
          fcast = fcast %>%
            mutate(
              grouping_var = as.character(!!rlang::sym(split()))
            )
        } else {
          fcast = fcast %>%
            mutate(grouping_var = 'Total')
        }
        cat(
          '\n - tsPreForecast grouping_var values:',
          unique(fcast$grouping_var)
        )

        # Ensure result is tsibble
        # fcast = fcast %>%
        #        as_tsibble( key = all_of(keyVars) , index = indexVar  ) %>%
        #        fill_gaps( .full = TRUE  )

        cat('\n - tsPreForecast done.')
        # print( names( fcast ) )

        # Testing:
        # saveRDS( fcast , 'tsPreForecast.rds' )

        return(fcast)
      })

      # Mable data ####

      mable_Data = reactive({
        req(region_filtered_selected_data())
        req(input$outliers)
        req(input$reporting)

        cat("\n* evaluation_widget mable_Data")

        cat("\n missing_reports: ", missing_reports())
        cat("\n covariates: ", input$covariates)
        cat("\n split: ", split())
        cat("\n agg_level: ", input$agg_level)

        selected_data = region_filtered_selected_data()

        # Testing
        # saveRDS( selected_data , "selected_data.rds")

        # Reporting
        reporting = input$reporting
        cat("\n Reporting: ", reporting)
        if (!reporting == "All") {
          selected_data = selected_data %>%
            filter(Selected %in% reporting)

          if (nrow(selected_data) == 0) {
            cat("\n - no data for this level of reporting!")
            return()
          }
        }

        # Outliers
        error = NULL
        if (!input$outliers == "Original") {
          error = input$outliers
        }
        cat("\n outlier filter: ", error)

        levelNames = orgUnitLevels()$levelName
        cat("\n - levelNames", levelNames)

        cat("\n - mable.data")

        startingMonth = startingMonth()
        endingMonth = endingMonth()
        agg_level = input$agg_level
        # missing_reports = missing_reports()

        split = NULL
        selected_split = split()
        if (!input$split_data == 'None') {
          split = input$split
        }

        if (testing) {
          save(
            selected_data,
            selected_split,
            startingMonth,
            endingMonth,
            error,
            missing_reports,
            split,
            agg_level,
            levelNames,
            file = "mable_Data_inputs.rda"
          )
        }

        mable.data = mable_data(
          tibble.data = selected_data,
          .orgUnit = FALSE, # group by orgunit
          .startingMonth = startingMonth,
          .endingMonth = endingMonth,
          # .missing_reports = missing_reports ,
          selected.only = TRUE, #reporting already in selected_data
          # alwaysReporting = input$selected ,
          # reportingSelectedOUs = reportingSelectedOUs() ,
          covariates = input$covariates,
          .split = split,
          .error = error,
          agg_level = agg_level,
          levelNames = levelNames,
          remove.aggregate = TRUE,
          .cat = TRUE,
          # Testing:
          testing = FALSE
        )

        # # testing
        if (testing) {
          saveRDS(mable.data, "mable.data.rds")
        }
        return(mable.data)
      })

      # Plot ####
      plotTrends = reactive({
        req(mable_Data())

        cat('\n* evaluation_widget plotTrends():')

        # Eval Date
        cat('\n - evaluation date', input$evaluation_month)
        .period = period()
        if (.period %in% 'Month') {
          eval_date = yearmonth(input$evaluation_month)
        }
        if (.period %in% 'Week') {
          eval_date = yearweek(input$evaluation_month)
        }
        cat('\n - eval_date:', eval_date)

        .limits =
          if (input$scale) {
            c(NA, NA)
          } else {
            c(0, NA)
          }

        # Testing
        # saveRDS( mable_Data(), 'mable_Data.rds')

        cat('\n - ploTrends mable_Data():')
        mable_Data = mable_Data()
        cat('\n - ploTrends .d:') #glimpse(.d)

        data.text = paste(unique(mable_Data$data), collapse = " + ")

        tic()

        ## Main plot ####
        cat("\n - main plot")

        g = mable_Data %>%
          filter(!is.na(total)) %>%
          autoplot(total) +
          # ggplot( aes( x = !! rlang::sym( .period ), y = total
          #
          #            , group =  as.character( !! rlang::sym( input$agg_level  ) )
          #
          #            , color =  as.character( !! rlang::sym( input$agg_level  ) )
          #           ) )  +
          # geom_line() +
          theme_minimal() +

          geom_vline(
            xintercept = as.Date(eval_date),
            color = 'brown',
            alpha = .25
          )

        cat('\n - basic plot done')
        toc()

        if (!input$legend) {
          g = g +
            theme(legend.position = "none")
        }

        if (input$label) {
          g = g +
            geom_label_repel(
              data = mable_Data %>%
                filter(
                  !!rlang::sym(.period) ==
                    max(mable_Data %>% pull(.period), na.rm = T)
                ),
              aes(label = grouping_var, group = grouping_var)
            )
        }

        # Determine number of agg levels available
        # If only one, do not facet (causes error, perhaps because of autoplot?)

        num_agg_levels = count(
          mable_Data %>% as_tibble,
          !!rlang::sym(input$agg_level)
        ) %>%
          nrow()

        # if ( input$agg_level != levelNames()[1] & input$facet_admin ){
        if (num_agg_levels > 1 & input$facet_admin) {
          cat('\n -  admin facets')

          if (input$facet_split) {
            cat('\n -  facet admin - split')

            g = g +
              facet_grid(
                rows = vars(as.character(!!rlang::sym(input$agg_level))),
                cols = grouping_var,
                scales = "free_y"
              )
          } else {
            g = g +
              facet_wrap(
                vars(as.character(!!rlang::sym(input$agg_level))),
                scales = "free_y"
              )
          }
        } else {
          if (input$facet_split) {
            cat('\n - facet_split')
            g = g +
              facet_wrap(~grouping_var, scales = "free_y")
          }
        }

        # Time scale
        cat('\n - Evaluation: setting x axis time scale', period())
        if (.period %in% 'Month') {
          g = g + scale_x_yearmonth("", date_breaks = "1 year")
        }
        # Default for weeks seems ok - 6 months
        # if ( .period %in% 'Week') g = g + scale_x_yearweek("", date_breaks = "1 year" )

        g = g +
          scale_y_continuous(label = comma, limits = .limits) +
          scale_color_discrete(drop = TRUE) +
          labs(
            y = "",
            x = "",
            title = str_wrap(input$indicator, 200),
            subtitle = str_wrap(data.text, 200),
            caption = str_wrap(caption.text(), 200)
          )

        cat('\n - axis scales and labs done')

        ## Pre-Evaluation Trend #####
        if (input$pre_evaluation) {
          cat('\n - evaluation line.  ')
          cat('\n - pi_levels:', pi_levels())

          cat('\n - pre-evaluation date')
          if (.period %in% 'Month') {
            pre_eval_date = yearmonth(input$evaluation_month) - 12
          }
          if (.period %in% 'Week') {
            pre_eval_date = yearweek(input$evaluation_month) - 25
          }
          cat('\n - pre_eval_date:', pre_eval_date)

          g = g +
            fabletools::autolayer(
              tsPreForecast(),
              level = ifelse(input$forecast_ci, 89, FALSE),
              color = 'steelblue',
              linetype = 'dotted',
              linewidth = 2,
              alpha = .75
            ) +
            # geom_line( data = tsPreForecast(), aes(  y = .mean )
            #   # ,   color = 'light blue'
            #   , alpha = .75
            #   , linetype = 'dotted'  , size = 2
            # ) +
            # geom_vline( xintercept = as.Date( pre_eval_date ) ,
            #             color = 'brown' ,
            #             alpha = .25 ) +
            geom_vline(
              xintercept = as.Date(eval_date),
              color = 'black',
              alpha = 1
            )

          if (input$pe) {
            g = g +
              geom_label_repel(
                data = key.mape(),
                aes(
                  x = !!rlang::sym(period()),
                  y = actual,
                  label = paste("MAPE:", percent(mape, accuracy = 1.0)),
                  hjust = just
                ),
                # force_pull = 0 ,
                segment.colour = NA
              )
          }
          cat('\n - evaluation line done')
        }

        ## Evaluation Trend  ####
        if (input$evaluation) {
          cat('\n - predicted trend  ')
          cat('\n - evaluation line.  ', 'pi_levels:', pi_levels())

          g = g +
            fabletools::autolayer(
              tsForecast(),
              color = 'darkorange',
              level = ifelse(input$forecast_ci, 89, FALSE),
              linetype = 'dashed',
              linewidth = 1,
              alpha = .5
            ) +

            geom_vline(
              xintercept = as.Date(eval_date),
              color = 'blue',
              alpha = 1
            )

          # annotate( "text" ,
          #           x = as.Date( eval_date ) ,
          #           y = Inf ,
          #           hjust = 0 , vjust = 1 ,
          #           label = paste( "MPE:\n" )
          #           ) +

          if (input$pe) {
            g = g +
              geom_label_repel(
                data = key.mpe(),
                aes(
                  x = !!rlang::sym(period()),
                  y = actual,
                  label = paste("MPE:", percent(mpe, accuracy = 1.0)),
                  hjust = just
                ),
                # force_pull = 0 ,
                segment.colour = NA
              )
          }
        }

        ## Auto Model Prediction  ####
        if (auto_model_values$done) {
          cat("\n - auto_model_values$done = ", auto_model_values$done)
          auto_model = auto_model()

          sel_predicted  <- tryCatch(selected_predicted(),  error = function(e) NULL)
          test.forecasts <- if (!is.null(auto_model)) auto_model$test.forecasts else NULL
          sel_model_name <- if (!is.null(input$selected_model) && nchar(input$selected_model) > 0)
            tolower(input$selected_model) else NULL

          cat('\n - auto predicted trend  ')
          cat("\n - nrow(sel_predicted) = ", if (!is.null(sel_predicted)) nrow(sel_predicted) else "NULL")

          if (!is.null(sel_predicted) && nrow(sel_predicted) > 0 && !is.null(test.forecasts)) {
            g = g +
              # Post-intervention evaluation forecast (strip samples list-col before autolayer)
              fabletools::autolayer(
                sel_predicted %>% dplyr::select(-dplyr::any_of("samples")),
                color    = 'darkorange',
                level    = ifelse(input$forecast_ci, 80, FALSE),
                linetype = 'dashed',
                linewidth = 1,
                alpha    = .75
              ) +
              # Test-period forecast for the selected model (pre-intervention validation)
              fabletools::autolayer(
                test.forecasts %>%
                  dplyr::filter(.model == sel_model_name) %>%
                  dplyr::select(-dplyr::any_of("samples")),
                color    = 'steelblue',
                level    = ifelse(input$forecast_ci, 80, FALSE),
                linetype = 'dotted',
                linewidth = 1,
                alpha    = .5
              )

            # Lower-right annotation: selected model + WPE stats
            stats <- tryCatch(selected_wpe_stats(), error = function(e) NULL)
            if (!is.null(stats) && nrow(stats) > 0) {
              annotation_text <- paste(
                paste("Model:", input$selected_model),
                paste("Mean:",   round(stats$mean[1],   1), "%"),
                paste("SD:",     round(stats$sd[1],     1), "%"),
                paste("Median:", round(stats$median[1], 1), "%"),
                sep = "\n"
              )
              g <- g + annotate(
                "label",
                x = Inf, y = -Inf,
                label    = annotation_text,
                hjust    = 1.05, vjust = -0.1,
                size     = 3,
                alpha    = 0.85,
                label.size = 0.3
              )
            }
          } else {
            cat('\n - selected predicted or test.forecasts is NULL; skipping autolayer')
          }
        }

        ## prediction Interval
        # if ( ){
        #
        #   g = g +
        #     geom_line( data = tsForecast() , aes( y = .mean )
        #              # ,   color = 'light blue'
        #              , alpha = .75
        #              , linetype = 'dotted'  , size = 2
        #   ) +
        # }

        ## Smooth line #####
        if (input$smooth) {
          cat('\n - agg level', input$agg_level)
          .d. = .d %>%
            as_tibble %>%
            mutate(
              !!input$agg_level := as.character(!!rlang::sym(input$agg_level))
            )

          cat('\n - smooth .d.') #glimpse(.d. )
          g = g +
            geom_smooth(data = .d., alpha = .75)
        }

        ## End ####
        cat('\n - end plotTrends():')

        # Testing
        # saveRDS( g, 'plotTrends.rds')

        return(g)
      })

      plotComponents = reactive({
        req(tsModel())
        req(input$evaluation_month)
        cat('\n* evaluation_widget plotComponenets():')

        g = tsModel() %>% fabletools::components() %>% autoplot

        cat('\n - end plotComponents():')

        return(g)
      })

      plotOutput = reactive({
        # req( input$components )
        cat('\n*  evaluation_widget plotOutput')
        cat('\n - input$components:', input$components)

        if (input$components) {
          cat('\n - components')
          g = plotComponents()
        } else {
          cat('\n - plotTrends')
          g = plotTrends()
        }
        return(g)
      })

      # output$plotlyOutput <- renderPlotly({
      # plotly::ggplotly( plotOutput() )  })

      # output$plotOutput <-  renderPlot({ plotOutput()  })

      chartModuleServer(
        "plotOutput",
        reactive({
          plotOutput()
        })
      )

      chartModuleServer(
        "wpeHistogram",
        reactive({
          wpeHistogram()
        })
      )

      # output$dynamic <- renderUI({
      #     req(input$plot_hover)
      #     verbatimTextOutput("vals")
      # })
      #
      # output$vals <- renderPrint({
      #       hover <- input$plot_hover
      #       # print(str(hover)) # list
      #       y <- nearPoints( mable_data() , input$plot_hover)[input$var_y]
      #       req(nrow(y) != 0)
      #       y
      # })

      # Return ####
      return()
    }
  )
}
