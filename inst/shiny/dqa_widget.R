dqa_widget_ui = function(id) {
  ns <- NS(id)

  tagList(
    shinybusy::add_busy_spinner(
      spin     = "fading-circle",
      position = 'bottom-right'
    ),

    fluidPage(
      titlePanel("Evolution of Data Quality"),
      htmlOutput(ns("region_filter_status")),

      sidebarLayout(
        sidebarPanel(
          width = 2,

          h5("Data Elements"),

          actionButton(ns('update_dqa_elements'), "Update", class = "btn-info btn-sm"),

          checkboxInput(ns("collapse_dqa"), "One row per data element", value = TRUE),

          checkboxInput(ns("select_all_dqa"), "Select / deselect all", value = TRUE),

          div(
            style = paste0(
              "max-height: calc(60vh - 80px); overflow-y: auto;",
              " border: 1px solid #ddd; padding: 4px; border-radius: 4px;"
            ),
            checkboxGroupInput(
              ns("dqa_elements"),
              label    = NULL,
              choices  = NULL,
              selected = NULL,
              width    = "100%"
            )
          )
        ),

        mainPanel(
          width = 10,

          uiOutput(ns("dqa_no_elements_hint")),

          bslib::navset_tab(
            id = ns("dqa_tab"),

            bslib::nav_panel(
              "Reporting",
              bslib::navset_pill(
                id = ns("dqa_reporting_subtab"),
                bslib::nav_panel(
                  "Chart",
                  chartModuleUI(ns("dqaReportingOutput"), height = "calc(70vh - 50px)", overlay = TRUE)
                ),
                bslib::nav_panel(
                  "Map",
                  div(
                    style = "padding: 6px 0 2px 0;",
                    uiOutput(ns("dqa_map_year_ui"))
                  ),
                  leaflet::leafletOutput(ns("dqaReportingMap"), height = "72vh")
                ),
                bslib::nav_panel(
                  "Facility Heatmap",
                  chartModuleUI(ns("dqaFacilityHeatmap"),
                                height = "calc(75vh - 50px)", overlay = TRUE)
                )
              )
            ),

            bslib::nav_panel(
              "Outliers",
              chartModuleUI(ns("dqaNoErrorsOutput"), height = "calc(70vh - 50px)", overlay = TRUE)
            ),

            bslib::nav_panel(
              "Consistency",
              bslib::navset_tab(
                bslib::nav_panel(
                  "Chart",
                  br(),
                  uiOutput(ns("consistency_status")),
                  chartModuleUI(ns("dqaConsistencyChart"), height = "calc(70vh - 50px)", overlay = TRUE)
                ),
                bslib::nav_panel(
                  "Summary Table",
                  br(),
                  uiOutput(ns("consistency_summary_note")),
                  DT::DTOutput(ns("dqaConsistencyTable"))
                ),
                bslib::nav_panel(
                  "Detail Drilldown",
                  br(),
                  selectInput(
                    ns("consistency_rule_select"),
                    "Select rule to drilldown:",
                    choices  = NULL,
                    width    = "100%"
                  ),
                  DT::DTOutput(ns("dqaConsistencyDetail"))
                )
              )
            ),

            bslib::nav_panel(
              "MASE",
              chartModuleUI(ns("dqaMaseOutput"), height = "calc(70vh - 50px)", overlay = TRUE)
            ),

            bslib::nav_panel(
              "Facilities",
              bslib::navset_tab(
                bslib::nav_panel(
                  "Chart",
                  br(),
                  uiOutput(ns("facilities_status_note")),
                  chartModuleUI(ns("dqaFacilitiesPlot"),
                                height = "calc(70vh - 50px)", overlay = TRUE)
                ),
                bslib::nav_panel(
                  "Table",
                  br(),
                  DT::DTOutput(ns("dqaFacilitiesTable"))
                )
              )
            )
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
      options(shiny.trace    = FALSE)
      options(shiny.reactlog = FALSE)
      options(dplyr.summarise.inform = FALSE)

      data.folder  = reactive({ directory_widget_output$directory() })
      indicator    = reactive({ data_widget_output$indicator() })
      formulas     = reactive({ data_widget_output$formulas() })
      dataset.file = reactive({ data_widget_output$dataset.file() })
      geoFeatures  = reactive({ metadata_widget_output$geoFeatures() })

      data1 = reactive({ data_widget_output$data1() })

      aggregateselected_data = reactive({ reporting_widget_output$aggregateselected_data() })
      data.total    = reactive({ reporting_widget_output$data.total() })
      selected_data = reactive({ reporting_widget_output$selected_data() })
      reportingSelectedOUs = reactive({ reporting_widget_output$reportingSelectedOUs() })

      data2 = reactive({ cleaning_widget_output$data2() })

      formula_elements = reactive({ data_widget_output$formula_elements() })

      validationRules = reactive({
        if (!is.null(metadata_widget_output)) metadata_widget_output$validationRules()
      })

      orgUnits      = reactive({ metadata_widget_output$orgUnits() })
      orgUnitLevels = reactive({ metadata_widget_output$orgUnitLevels() })

      dates       = reactive({ reporting_widget_output$dates() })
      levelNames  = reactive({ reporting_widget_output$levelNames() })
      period      = reactive({ reporting_widget_output$period() })
      group_by_cols = reactive({ reporting_widget_output$group_by_cols() })
      split       = reactive({ reporting_widget_output$split() })
      startingMonth = reactive({ reporting_widget_output$startingMonth() })
      endingMonth   = reactive({ reporting_widget_output$endingMonth() })
      missing_reports = reactive({ reporting_widget_output$missing_reports() })
      num_datasets  = reactive({ reporting_widget_output$num_datasets() })
      num_facilities = reactive({ reporting_widget_output$num_facilities() })
      caption.text  = reactive({ reporting_widget_output$caption.text() })

      # Region filter ####
      regions_selected = reactive({
        if (!is.null(regions_widget_output)) regions_widget_output$selected_regions() else list()
      })

      output$region_filter_status <- renderUI({
        sr    <- regions_selected()
        parts <- Filter(function(x) !is.null(x) && length(x) > 0,
                        list(sr$level2, sr$level3, sr$level4, sr$level5, sr$level6))
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
        sr    <- regions_selected()
        parts <- Filter(function(x) !is.null(x) && length(x) > 0,
                        list(sr$level2, sr$level3, sr$level4, sr$level5, sr$level6))
        region <- if (length(parts) == 0) "National" else
          paste(sapply(parts, paste, collapse = ", "), collapse = " / ")

        # Add date range from reporting settings
        start_str <- tryCatch(reporting_widget_output$startingMonth(), error = function(e) NULL)
        end_str   <- tryCatch(reporting_widget_output$endingMonth(),   error = function(e) NULL)
        date_range <- if (!is.null(start_str) && !is.null(end_str) &&
                          nzchar(start_str) && nzchar(end_str))
          paste0(start_str, "\u2013", end_str)
        else NULL

        paste(c(region, date_range), collapse = " | ")
      })

      region_filtered_data1 = reactive({
        req(data1())
        d  <- data1()
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
        if (!is_empty(sr$level6) && length(ln) >= 6 && ln[6] %in% names(d))
          d <- d[d[[ln[6]]] %in% sr$level6, ]
        d
      })

      # Element selector ####

      selected_dqa_elements = reactiveValues(elements = NULL)
      dqa_elem_map_rv       = reactiveVal(list())

      # Internal helpers
      .dqa_split_by_role <- function(choices, fe) {
        if (is.null(fe) || nrow(fe) == 0 || !"role" %in% names(fe))
          return(list(primary = choices, secondary = character(0)))
        fe_exp <- tryCatch(
          tidyr::separate_rows(fe, Categories, categoryOptionCombo.ids, sep = ";") %>%
            dplyr::mutate(
              Categories = trimws(Categories),
              data_name  = dplyr::if_else(
                is.na(Categories) | !nzchar(trimws(Categories)),
                dataElement, paste(dataElement, trimws(Categories), sep = "_")
              )
            ) %>%
            dplyr::select(data_name, role) %>%
            dplyr::distinct(data_name, .keep_all = TRUE),
          error = function(e) NULL
        )
        if (is.null(fe_exp)) return(list(primary = choices, secondary = character(0)))
        role_map  <- setNames(fe_exp$role, fe_exp$data_name)
        secondary <- choices[!is.na(role_map[choices]) & role_map[choices] == "secondary"]
        list(primary = setdiff(choices, secondary), secondary = secondary)
      }

      .dqa_choice_names <- function(choices, secondary) {
        lapply(choices, function(ch) {
          if (ch %in% secondary)
            tags$span(ch, tags$em(" (secondary)", style = "color:#999; font-size:0.85em;"))
          else ch
        })
      }

      # Build dataElement → data_names map using UID-based matching via data.id.
      # Falls back to the dataset's own dataElement column for elements not in
      # the formula (e.g. when an all-levels file is loaded with a smaller formula).
      .dqa_element_map <- function(choices, fe, d) {
        # Helper: group choices by dataElement column in the data
        .group_by_de_col <- function(ch, d) {
          if (!is.null(d) && "dataElement" %in% names(d) && "data" %in% names(d)) {
            tryCatch({
              lkp <- as.data.frame(d)[, c("data", "dataElement"), drop = FALSE] %>%
                dplyr::distinct() %>%
                dplyr::filter(data %in% ch)
              if (nrow(lkp) > 0) return(base::split(lkp$data, lkp$dataElement))
            }, error = function(e) NULL)
          }
          setNames(as.list(ch), ch)
        }

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
              map      <- base::split(result$data, result$dataElement)
              unmatched <- setdiff(choices, unlist(map))
              if (length(unmatched) > 0)
                map <- c(map, .group_by_de_col(unmatched, d))
              return(map)
            }
          }
        }, error = function(e) NULL)
        # No formula match at all — group entirely by dataElement column
        .group_by_de_col(choices, d)
      }

      # Populate checkboxGroupInput in either expanded or collapsed mode.
      # When there are more than 5 primary elements, default to none selected
      # so a large dataset doesn't silently trigger a long computation on load.
      .dqa_max_auto_select <- 5L

      .update_dqa_checkbox <- function(choices, fe, collapsed, d) {
        map <- .dqa_element_map(choices, fe, d)
        dqa_elem_map_rv(map)

        if (!collapsed) {
          roles  <- .dqa_split_by_role(choices, fe)
          sel    <- if (length(roles$primary) > .dqa_max_auto_select) character(0) else roles$primary
          updateCheckboxGroupInput(
            session, "dqa_elements",
            choiceNames  = .dqa_choice_names(choices, roles$secondary),
            choiceValues = choices,
            selected     = sel
          )
          selected_dqa_elements$elements <- sel
        } else {
          elems     <- stringr::str_sort(names(map), numeric = TRUE)
          fe_roles  <- if (!is.null(fe) && nrow(fe) > 0 && "role" %in% names(fe))
            setNames(fe$role[match(elems, fe$dataElement)], elems)
          else character(0)
          sec_elems  <- elems[!is.na(fe_roles) & fe_roles == "secondary"]
          prim_elems <- setdiff(elems, sec_elems)
          sel        <- if (length(prim_elems) > .dqa_max_auto_select) character(0) else prim_elems
          updateCheckboxGroupInput(
            session, "dqa_elements",
            choiceNames  = .dqa_choice_names(elems, sec_elems),
            choiceValues = elems,
            selected     = sel
          )
          selected_dqa_elements$elements <- unique(unlist(map[sel], use.names = FALSE))
        }
      }

      observeEvent(region_filtered_data1(), {
        req(region_filtered_data1()$data)
        choices <- stringr::str_sort(unique(region_filtered_data1()$data), numeric = TRUE)
        .update_dqa_checkbox(choices, formula_elements(), isTRUE(input$collapse_dqa),
                             region_filtered_data1())
      })

      observeEvent(input$collapse_dqa, {
        req(region_filtered_data1()$data)
        choices <- stringr::str_sort(unique(region_filtered_data1()$data), numeric = TRUE)
        .update_dqa_checkbox(choices, formula_elements(), isTRUE(input$collapse_dqa),
                             region_filtered_data1())
      }, ignoreInit = TRUE)

      observeEvent(input$update_dqa_elements, {
        if (isTRUE(input$collapse_dqa)) {
          map <- dqa_elem_map_rv()
          selected_dqa_elements$elements <- unique(unlist(map[input$dqa_elements], use.names = FALSE))
        } else {
          selected_dqa_elements$elements <- input$dqa_elements
        }
        # Show notification NOW — this observer flushes to the client before
        # the blocking render fires, so the user sees the message during computation
        if (length(selected_dqa_elements$elements) > 0)
          showNotification(
            "DQA: computing reporting completeness — please wait...",
            id          = "dqa_reporting_computing",
            type        = "message",
            duration    = NULL,
            closeButton = FALSE
          )
      })

      observeEvent(input$select_all_dqa, {
        req(region_filtered_data1()$data)
        choices <- stringr::str_sort(unique(region_filtered_data1()$data), numeric = TRUE)
        if (isTRUE(input$collapse_dqa)) {
          map <- dqa_elem_map_rv()
          updateCheckboxGroupInput(
            session, "dqa_elements",
            selected = if (input$select_all_dqa) stringr::str_sort(names(map), numeric = TRUE) else character(0)
          )
        } else {
          updateCheckboxGroupInput(
            session, "dqa_elements",
            selected = if (input$select_all_dqa) choices else character(0)
          )
        }
      }, ignoreInit = TRUE)

      # DQA data filtered to selected elements
      dqa_data = reactive({
        req(region_filtered_data1())
        req(length(selected_dqa_elements$elements) > 0)
        d     <- region_filtered_data1()
        elems <- selected_dqa_elements$elements
        d <- d[d$data %in% elems, ]
        cat('\n* dqa_data: selected', length(elems), 'series |',
            nrow(d), 'rows | elements:', paste(unique(elems), collapse = '; '))
        d
      })

      output$dqa_no_elements_hint <- renderUI({
        if (length(selected_dqa_elements$elements) > 0) return(NULL)
        div(
          style = paste0(
            "background:#fff3cd; padding:10px 16px;",
            " border-left:4px solid #ffc107; border-radius:3px; margin:10px 0;"
          ),
          tags$strong("No elements selected."),
          " Select one or more elements in the sidebar and press ",
          tags$strong("Update"), " to compute DQA."
        )
      })

      # Existing DQA plots (now use dqa_data() so element filter applies) ####

      plotDqaReporting = reactive({
        cat('\n*  dqa_widget plotDqaReporting |', nrow(dqa_data()), 'rows |',
            length(selected_dqa_elements$elements), 'series')
        withProgress(
          message = "DQA: reporting completeness — year 1...",
          value   = 0,
          {
            dqa_data() %>%
              dqaPercentReporting(
                .progress = function(i, n) {
                  setProgress(
                    value   = i / n,
                    message = sprintf(
                      "DQA: reporting completeness — year %d of %d", i, n
                    )
                  )
                  # Force httpuv to flush queued WebSocket sends so the
                  # browser receives the progress update before R blocks
                  # again on the next year's computation.
                  tryCatch(
                    httpuv::run_now(timeoutMs = 0L),
                    error = function(e) NULL
                  )
                }
              ) %>%
              dqa_reporting_plot()
          }
        )
      })

      chartModuleServer("dqaReportingOutput", reactive({
        plotDqaReporting() + labs(caption = region_caption_text())
      }))

      # Remove the "computing completeness" notification once the chart is ready.
      # on.exit() in a reactive only fires on re-execution, not cache hits,
      # so use an observer that invalidates whenever plotDqaReporting() does.
      observe({
        plotDqaReporting()
        removeNotification("dqa_reporting_computing")
      })

      plotDqaNoError = reactive({
        cat('\n*  dqa_widget plotDqaNoError')
        withProgress(message = "DQA: computing outlier summary...", value = NULL, {
          dqa_data() %>%
            monthly.outlier.summary() %>%
            yearly.outlier.summary() %>%
            dqa_outliers %>%
            yearly.outlier.summary_plot()
        })
      })

      chartModuleServer("dqaNoErrorsOutput", reactive({
        plotDqaNoError() + labs(caption = region_caption_text())
      }))

      plotDqaMASE = reactive({
        cat('\n*  dqa_widget plotDqaMASE')
        withProgress(message = "DQA: computing SWAPE stability...", value = NULL, {
          dqa_data() %>% dqa_swape %>% dqa_swape_plot
        })
      })

      chartModuleServer("dqaMaseOutput", reactive({
        plotDqaMASE() + labs(caption = region_caption_text())
      }))

      # Reporting Map ####

      dqa_region_reporting = reactive({
        cat('\n* dqa_region_reporting: dqa_data rows=', if(!is.null(dqa_data())) nrow(dqa_data()) else 'NULL')
        cat('\n* dqa_region_reporting: levelNames=', paste(levelNames(), collapse=','))
        cat('\n* dqa_region_reporting: geoFeatures rows=', if(!is.null(geoFeatures())) nrow(geoFeatures()) else 'NULL')
        req(dqa_data())
        req(levelNames())
        # Note: geoFeatures is NOT required here — reporting aggregation runs even
        # without geo data; the map renderer checks geoFeatures separately.

        # Default to level 2 when geoFeatures is unavailable
        if (!is.null(geoFeatures())) {
          gf_levels <- sort(unique(geoFeatures()$level))
          map_level <- min(gf_levels[gf_levels > 1L], na.rm = TRUE)
        } else {
          map_level <- 2L
        }
        # levelNames()[1]=National, [2]=level2, [3]=level3, ...
        level_col  <- if (length(levelNames()) >= map_level) levelNames()[map_level] else levelNames()[2L]
        req(level_col %in% names(dqa_data()))

        notif_id <- showNotification("DQA map: computing per-region reporting…",
                                     duration = NULL, type = "message", closeButton = FALSE)
        on.exit(removeNotification(notif_id), add = TRUE)

        dqa_reporting_by_region(
          dqa_data(),
          level_col       = level_col,
          missing_reports = 0L,
          .progress       = NULL
        )
      })

      output$dqa_map_year_ui <- renderUI({
        res <- dqa_region_reporting()
        req(res)
        years <- sort(unique(res$Year))
        sliderInput(
          session$ns("dqa_map_year"),
          label   = NULL,
          min     = min(years),
          max     = max(years),
          value   = max(years),
          step    = 1L,
          sep     = "",
          width   = "60%",
          animate = animationOptions(interval = 2000L, loop = FALSE)
        )
      })

      output$dqaReportingMap <- leaflet::renderLeaflet({
        res <- dqa_region_reporting()
        req(res)
        req(geoFeatures())

        # Use slider year if available; otherwise default to the most recent year
        yr <- if (!is.null(input$dqa_map_year)) input$dqa_map_year else max(res$Year)

        rep_yr <- res |> dplyr::filter(Year == yr)

        # Use the lowest non-national level present in geoFeatures.
        # Some demo datasets only have chiefdom (level 3) shapes, not district (level 2).
        gf_levels <- sort(unique(geoFeatures()$level))
        map_level <- min(gf_levels[gf_levels > 1L], na.rm = TRUE)
        gf_filtered <- geoFeatures() |> dplyr::filter(level == map_level)
        cat('\n* dqaReportingMap: gf names=', paste(sort(gf_filtered$name), collapse=', '))
        cat('\n* dqaReportingMap: rep_yr region_names=', paste(sort(unique(rep_yr$region_name)), collapse=', '))
        gf2 <- dplyr::left_join(gf_filtered, rep_yr, by = c("name" = "region_name"))
        n_matched <- sum(!is.na(gf2$pr))
        cat('\n* dqaReportingMap: matched', n_matched, 'of', nrow(gf2), 'polygons')
        req(nrow(gf2) > 0)

        pal  <- leaflet::colorNumeric("RdYlGn", domain = c(0, 1), na.color = "#cccccc")
        bbox <- sf::st_bbox(gf2)

        leaflet::leaflet(gf2) |>
          leaflet::addTiles() |>
          leaflet::fitBounds(
            lng1 = unname(bbox["xmin"]), lat1 = unname(bbox["ymin"]),
            lng2 = unname(bbox["xmax"]), lat2 = unname(bbox["ymax"])
          ) |>
          leaflet::addPolygons(
            fillColor        = ~pal(pr),
            fillOpacity      = 0.75,
            color            = "white",
            weight           = 1,
            label            = ~paste0(
              name, ": ",
              dplyr::if_else(is.na(pr), "no data", paste0(round(pr * 100, 1), "%"))
            ),
            highlightOptions = leaflet::highlightOptions(
              weight = 2, color = "#444", bringToFront = TRUE
            )
          ) |>
          leaflet::addLegend(
            pal       = pal,
            values    = c(0, 1),
            title     = paste0("% Reporting<br>", yr),
            labFormat = leaflet::labelFormat(
              suffix    = "%",
              transform = function(x) round(x * 100)
            )
          )
      })

      outputOptions(output, "dqaReportingMap", suspendWhenHidden = TRUE)
      outputOptions(output, "dqa_map_year_ui", suspendWhenHidden = TRUE)

      # Consistency tab ####

      consistency_results = reactive({
        if (!is.null(current_tab)) req(current_tab() == "DQA")
        req(region_filtered_data1())
        req(validationRules())
        req(length(selected_dqa_elements$elements) > 0)
        cat('\n*  dqa_widget consistency_results')

        d     <- region_filtered_data1()
        elems <- selected_dqa_elements$elements

        # Resolve selected element names → data.ids → DE UIDs for rule filtering
        filter_ids <- NULL
        if (!is.null(elems) && length(elems) > 0 && "data.id" %in% names(d)) {
          filter_ids <- unique(d$data.id[d$data %in% elems & !is.na(d$data.id)])
        }

        dqa_consistency(d, validationRules(), filter_data_ids = filter_ids)
      })

      output$consistency_status <- renderUI({
        res <- consistency_results()
        if (is.null(res)) {
          return(div(
            style = "background:#fff3cd; padding:8px 14px; border-left:4px solid #ffc107; border-radius:3px; margin-bottom:8px;",
            "No validation rules available or data not loaded."
          ))
        }
        n_rules <- dplyr::n_distinct(res$rule_id)
        n_inc   <- res %>% dplyr::filter(incomplete) %>% dplyr::distinct(rule_id) %>% nrow()
        div(
          style = "background:#e8f4fd; padding:8px 14px; border-left:4px solid #2196F3; border-radius:3px; margin-bottom:8px;",
          paste0(n_rules, " rule(s) found. ",
                 if (n_inc > 0) paste0(n_inc, " rule(s) incomplete (elements not in formula). ") else "",
                 n_rules - n_inc, " rule(s) fully evaluated.")
        )
      })

      chartModuleServer("dqaConsistencyChart", reactive({
        withProgress(message = "DQA: evaluating validation rules...", value = NULL, {
          res <- consistency_results()
          rule_choices <- if (!is.null(res) && nrow(res) > 0)
            res |> dplyr::filter(!incomplete) |> dplyr::distinct(rule_id, rule_name)
          else NULL
          if (!is.null(rule_choices) && nrow(rule_choices) > 0)
            updateSelectInput(session, "consistency_rule_select",
              choices  = setNames(rule_choices$rule_id, rule_choices$rule_name),
              selected = rule_choices$rule_id[1])
          dqa_consistency_plot(res) + labs(caption = region_caption_text())
        })
      }))

      output$consistency_summary_note <- renderUI({
        res <- consistency_results()
        if (is.null(res) || nrow(res) == 0) return(NULL)
        n_inc <- res %>% dplyr::filter(incomplete) %>% dplyr::distinct(rule_id) %>% nrow()
        if (n_inc == 0) return(NULL)
        div(
          style = "background:#fff3cd; padding:6px 12px; border-left:4px solid #ffc107; border-radius:3px; margin-bottom:8px; font-size:0.9em;",
          paste0(n_inc, " rule(s) marked Incomplete: the data elements they reference are not in ",
                 "the current formula. Add them to the formula to evaluate these rules.")
        )
      })

      output$dqaConsistencyTable <- DT::renderDT({
        tbl <- dqa_consistency_table(consistency_results())
        DT::datatable(
          tbl,
          rownames  = FALSE,
          selection = "single",
          options   = list(
            scrollX    = TRUE,
            scrollY    = "55vh",
            paging     = FALSE,
            searching  = TRUE,
            dom        = 'ti'
          )
        ) %>%
          DT::formatStyle(
            "% Passed",
            backgroundColor = DT::styleInterval(
              c(0.5, 0.8),
              c("#ffcccc", "#fff3cd", "#d4edda")
            )
          )
      })

      # Update when user clicks a row in summary table
      observeEvent(input$dqaConsistencyTable_rows_selected, {
        tbl <- dqa_consistency_table(consistency_results())
        sel <- input$dqaConsistencyTable_rows_selected
        if (is.null(sel) || length(sel) == 0) return()
        rule_id_sel <- tbl$`Rule ID`[sel]
        updateSelectInput(session, "consistency_rule_select", selected = rule_id_sel)
      })

      consistency_detail_data <- reactive({
        req(input$consistency_rule_select)
        dqa_consistency_detail_rule(
          region_filtered_data1(),
          validationRules(),
          input$consistency_rule_select,
          max_rows = 2000L
        )
      })

      output$dqaConsistencyDetail <- DT::renderDT({
        detail <- consistency_detail_data()

        # Show notification only when this table actually renders (lazy — only
        # fires when the Detail Drilldown tab is visible)
        n            <- attr(detail, "truncated")
        n_incomplete <- if (!is.null(detail) && "Result" %in% names(detail))
          sum(detail$Result == "Incomplete", na.rm = TRUE) else 0L
        lines <- character(0)
        if (!is.null(n))
          lines <- c(lines, paste0("Showing most-recent 2,000 of ", n,
            " records — use column filters to narrow results."))
        if (n_incomplete > 0)
          lines <- c(lines, paste0(n_incomplete,
            " row(s) marked ‘Incomplete’: one element had no data",
            " for that facility-period (Left/Right Side is blank)."))
        if (length(lines) > 0)
          showNotification(HTML(paste(lines, collapse = "<br>")),
                           type = "warning", duration = NULL, closeButton = TRUE)
        DT::datatable(
          detail,
          rownames = FALSE,
          filter   = "top",
          options  = list(
            scrollX        = TRUE,
            scrollY        = "calc(100vh - 480px)",
            scrollCollapse = TRUE,
            paging         = TRUE,
            pageLength     = 25,
            lengthMenu     = list(c(10, 25, 50, 100), list("10", "25", "50", "100")),
            dom            = 'ltipr'
          )
        )
      })

      # Facility Status tab ####

      facility_status_rv <- reactive({
        req(region_filtered_data1())
        ln <- levelNames()
        req(length(ln) >= 2)
        level_col <- ln[2]
        req(level_col %in% names(region_filtered_data1()))
        tryCatch(
          dqa_facility_status(region_filtered_data1(), level_col),
          error = function(e) { message("facility_status: ", e$message); NULL }
        )
      })

      output$facilities_status_note <- renderUI({
        fs <- facility_status_rv()
        req(!is.null(fs) && nrow(fs) > 0)
        n_total  <- nrow(fs)
        n_active <- sum(fs$status == "Active")
        pct      <- round(100 * n_active / n_total, 1)
        div(
          style = paste0(
            "background:#e8f4fd; padding:8px 14px;",
            " border-left:4px solid #2196F3; border-radius:3px; margin-bottom:8px;"
          ),
          paste0(n_total, " facilities total: ", n_active,
                 " Active (", pct, "%), ", n_total - n_active, " Inactive.")
        )
      })

      chartModuleServer("dqaFacilitiesPlot", reactive({
        fs <- facility_status_rv()
        req(!is.null(fs) && nrow(fs) > 0)
        dqa_facility_status_plot(fs) +
          ggplot2::labs(caption = region_caption_text())
      }))

      output$dqaFacilitiesTable <- DT::renderDT({
        fs <- facility_status_rv()
        req(!is.null(fs) && nrow(fs) > 0)
        DT::datatable(
          dqa_facility_status_table(fs),
          rownames = FALSE,
          options  = list(scrollX = TRUE, paging = FALSE, dom = "ti")
        )
      })

      outputOptions(output, "dqaFacilitiesTable", suspendWhenHidden = TRUE)

      # Facility Heatmap (Reporting sub-tab) ####

      facility_heatmap_rv <- reactive({
        req(region_filtered_data1())
        ln <- levelNames()
        req(length(ln) >= 2)
        level_col <- ln[2]
        req(level_col %in% names(region_filtered_data1()))
        cat("\n* facility_heatmap: computing, level_col=", level_col,
            "nrow=", nrow(region_filtered_data1()), "\n")
        result <- tryCatch(
          dqa_facility_heatmap_data(region_filtered_data1(), level_col),
          error = function(e) {
            cat("\n! facility_heatmap error:", e$message, "\n")
            NULL
          }
        )
        cat("\n* facility_heatmap: result rows=",
            if (is.null(result)) "NULL" else nrow(result), "\n")
        result
      })

      chartModuleServer("dqaFacilityHeatmap", reactive({
        hm <- facility_heatmap_rv()
        req(!is.null(hm) && nrow(hm) > 0)
        # Scale height: 12px per facility, capped at the viewport limit
        n_fac <- length(unique(hm$orgUnit))
        h     <- min(max(n_fac * 12L, 300L), 2400L)
        dqa_facility_heatmap_plot(hm) +
          ggplot2::labs(caption = region_caption_text())
      }))

      # Return ####
      return()
    }
  )
}
