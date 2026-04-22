dqa_widget_ui = function(id) {
  ns <- NS(id)
  # fillCol( height = 600, flex = c(NA ) ,

  tagList(
    shinybusy::add_busy_spinner(
      spin = "fading-circle", # "self-building-square",
      position = 'bottom-right'
      # , margins = c(70, 1200)
    ),

    fluidPage(
      titlePanel("Evolution of Data Quality"),
      htmlOutput(ns("region_filter_status")),
      # tags$style(".row{height: 80vh;} "),
      navlistPanel(
        "Metric",
        widths = c(2, 10),

        tabPanel(
          "Reporting",

          # fluidPage(
          #   fluidRow(  style = "height:80vh;",
          plotOutput(ns("dqaReportingOutput"))
        ),

        # ) )
        tabPanel(
          "Outliers",

          fluidPage(
            fluidRow(
              style = "height:90vh;",
              plotOutput(ns("dqaNoErrorsOutput"))
            )
          )
        ),

        tabPanel(
          "Time-series Quality",

          fluidPage(
            fluidRow(style = "height:100vh;", plotOutput(ns("dqaMaseOutput")))
          )
        )
      )
    )
  )
}

dqa_widget_server <- function(
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
      options(shiny.trace = FALSE)
      options(shiny.reactlog = FALSE)
      options(dplyr.summarise.inform = FALSE)

      # cat('\n**Starting Reporting Widget\n')

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

      aggregateselected_data = reactive({
        reporting_widget_output$aggregateselected_data()
      })
      data.total = reactive({
        reporting_widget_output$data.total()
      })
      selected_data = reactive({
        reporting_widget_output$selected_data()
      })
      reportingSelectedOUs = reactive({
        reporting_widget_output$reportingSelectedOUs()
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

      dates = reactive({
        reporting_widget_output$dates()
      })
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

      # data1 filtered to selected region for DQA plots
      region_filtered_data1 = reactive({
        req(data1())
        d <- data1()
        sr <- regions_selected()
        ln <- reporting_widget_output$levelNames()

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

      plotDqaReporting = reactive({
        # req( input$components )
        cat('\n*  dqa_widget plotDqaReporting')

        dqa_data = region_filtered_data1()
        # TESTING
        # saveRDS( dqa_data , 'dqa_data.rds')
        dqa_data %>% dqaPercentReporting() %>% dqa_reporting_plot()
      })

      output$dqaReportingOutput <- renderPlot({
        plotDqaReporting() + labs(caption = region_caption_text())
      })

      plotDqaNoError = reactive({
        # req( input$components )
        cat('\n*  dqa_widget plotDqaNoError')

        dqa_data = region_filtered_data1()
        dqa_data %>%
          monthly.outlier.summary() %>%
          yearly.outlier.summary() %>%
          dqa_outliers %>%
          yearly.outlier.summary_plot()
      })

      output$dqaNoErrorsOutput <- renderPlot({
        plotDqaNoError() + labs(caption = region_caption_text())
      })

      plotDqaMASE = reactive({
        cat('\n*  dqa_widget plotDqaMASE')
        dqa_data = region_filtered_data1()
        dqa_data %>% dqa_mase %>% dqa_mase_plot
      })

      output$dqaMaseOutput <- renderPlot({
        plotDqaMASE() + labs(caption = region_caption_text())
      })

      # Return ####
      return()
    }
  )
}
