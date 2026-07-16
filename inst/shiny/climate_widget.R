# climate_widget.R — CHIRPS satellite rainfall analysis module
#
# Primary boundary source: DHIS2 org unit polygons from metadata_widget_output.
# Fallback: custom shapefile upload.
# CHIRPS rasters are Africa-wide; no country code is required for download.
#
# UI entry point:     climate_widget_ui(id)
# Server entry point: climate_widget_server(id, directory_widget_output,
#                                               metadata_widget_output)
#
# Requires: sf, terra, httr, openxlsx, ggplot2
# Optional: leaflet (Interactive tab degrades gracefully if absent)

# ── UI ────────────────────────────────────────────────────────────────────────

climate_widget_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(

    # ── Sidebar ──────────────────────────────────────────────────────────────
    sidebarPanel(
      width = 3,

      h5(icon("cloud-rain"), " CHIRPS Rainfall Analysis"),

      # Boundary status + level selector — populated from metadata
      uiOutput(ns("geo_status")),
      uiOutput(ns("ou_level_ui")),

      # Custom shapefile fallback — always available, collapsed by default
      tags$details(
        tags$summary(
          style = "cursor:pointer; font-size:0.9em; color:#555; margin-top:6px;",
          icon("folder-open"), " Use custom shapefile instead"
        ),
        div(
          style = "margin-top:6px;",
          fileInput(ns("shp_file"), "Geometry (.shp)",       accept = ".shp"),
          fileInput(ns("shx_file"), "Shape index (.shx)",    accept = ".shx"),
          fileInput(ns("dbf_file"), "Attribute table (.dbf)", accept = ".dbf"),
          fileInput(ns("prj_file"), "Projection (.prj) — optional", accept = ".prj")
        )
      ),

      hr(),

      radioButtons(
        ns("year_mode"), NULL,
        choices  = c("Single year" = "single", "Year range" = "range"),
        selected = "single", inline = TRUE
      ),

      # Single-year selector
      conditionalPanel(
        condition = "input.year_mode == 'single'",
        ns = ns,
        selectInput(
          ns("year"), "Year",
          choices  = rev(seq(1981L, as.integer(format(Sys.Date(), "%Y")) - 1L)),
          selected = as.integer(format(Sys.Date(), "%Y")) - 1L
        )
      ),

      # Year-range selectors
      conditionalPanel(
        condition = "input.year_mode == 'range'",
        ns = ns,
        fluidRow(
          column(6,
            selectInput(
              ns("year_start"), "From",
              choices  = rev(seq(1981L, as.integer(format(Sys.Date(), "%Y")) - 1L)),
              selected = as.integer(format(Sys.Date(), "%Y")) - 4L
            )
          ),
          column(6,
            selectInput(
              ns("year_end"), "To",
              choices  = rev(seq(1981L, as.integer(format(Sys.Date(), "%Y")) - 1L)),
              selected = as.integer(format(Sys.Date(), "%Y")) - 1L
            )
          )
        ),
        helpText(
          style = "font-size:0.82em; color:#666; margin-top:-4px;",
          icon("circle-info"), " Cached data loads instantly."
        )
      ),

      tags$label("Months"),
      div(
        style = "display:flex; gap:6px; margin-bottom:4px;",
        actionLink(ns("sel_all"),  "All"),
        actionLink(ns("sel_none"), "None"),
        actionLink(ns("sel_wet"),  "Wet (May–Oct)")
      ),
      checkboxGroupInput(
        ns("months"), label = NULL,
        choices  = stats::setNames(
          1:12,
          c("Jan","Feb","Mar","Apr","May","Jun",
            "Jul","Aug","Sep","Oct","Nov","Dec")
        ),
        selected = 1:12,
        inline   = TRUE
      ),

      hr(),

      selectInput(
        ns("palette"), "Colour Palette",
        choices  = c("Blues", "viridis", "plasma", "YlOrRd"),
        selected = "Blues"
      ),

      actionButton(
        ns("run"), "Run Analysis",
        icon  = icon("play"),
        class = "btn-primary",
        style = "width:100%; margin-top:6px;"
      ),
      br(), br(),
      actionLink(ns("clear_cache"), tagList(icon("trash"), " Clear cache")),
      uiOutput(ns("cache_status"))
    ),

    # ── Main panel ───────────────────────────────────────────────────────────
    mainPanel(
      width = 9,
      uiOutput(ns("results_panel"))
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

climate_widget_server <- function(id,
                                   directory_widget_output = NULL,
                                   metadata_widget_output  = NULL) {
  moduleServer(id, function(input, output, session) {

    # ── Cache directory ──────────────────────────────────────────────────────
    cache_dir <- reactive({
      d <- tryCatch(directory_widget_output$directory(), error = function(e) NULL)
      base <- if (!is.null(d) && nzchar(d %||% "")) d else tempdir()
      file.path(base, "climate_cache")
    })

    # ── Month helpers ────────────────────────────────────────────────────────
    observeEvent(input$sel_all,  updateCheckboxGroupInput(session, "months", selected = 1:12))
    observeEvent(input$sel_none, updateCheckboxGroupInput(session, "months", selected = character(0)))
    observeEvent(input$sel_wet,  updateCheckboxGroupInput(session, "months", selected = 5:10))

    # ── Cache management ─────────────────────────────────────────────────────
    cache_stamp <- reactiveVal(0)

    observeEvent(input$clear_cache, {
      d <- cache_dir()
      if (dir.exists(d)) {
        unlink(d, recursive = TRUE)
        showNotification("Climate cache cleared.", type = "message", duration = 4)
      } else {
        showNotification("No cache found.", type = "message", duration = 3)
      }
      cache_stamp(cache_stamp() + 1)
    })

    output$cache_status <- renderUI({
      cache_stamp()
      d    <- cache_dir()
      tifs <- list.files(d, pattern = "\\.tif$", full.names = TRUE)
      rdss <- list.files(d, pattern = "^results_.*\\.rds$", full.names = TRUE)

      if (length(tifs) == 0 && length(rdss) == 0)
        return(tags$small(style = "color:#888;", br(), "No cached files yet."))

      lines <- character(0)
      if (length(tifs) > 0) {
        mb     <- round(sum(file.info(tifs)$size, na.rm = TRUE) / 1e6, 1)
        n_crop <- sum(grepl("_x[0-9]", basename(tifs)))
        n_full <- length(tifs) - n_crop
        parts  <- character(0)
        if (n_crop > 0) parts <- c(parts, sprintf("%d cropped", n_crop))
        if (n_full > 0) parts <- c(parts, sprintf("%d full-region", n_full))
        lines <- c(lines, sprintf("%s TIF(s) (%.1f MB)", paste(parts, collapse = " + "), mb))
      }
      if (length(rdss) > 0) {
        mb_r  <- round(sum(file.info(rdss)$size, na.rm = TRUE) / 1e6, 1)
        lines <- c(lines, sprintf("%d result cache(s) (%.1f MB)", length(rdss), mb_r))
      }
      tags$small(style = "color:#555;", br(), paste(lines, collapse = "; "))
    })

    # ── Metadata boundary info ───────────────────────────────────────────────
    # geo_info() summarises what polygon levels are available in geoFeatures.
    geo_info <- reactive({
      gf <- tryCatch(metadata_widget_output$geoFeatures(), error = function(e) NULL)
      empty <- list(available = FALSE, levels_df = NULL,
                    country_name = NULL, sf_by_level = list())
      if (is.null(gf) || nrow(gf) == 0) return(empty)
      if (!inherits(gf, "sf"))          return(empty)

      ld <- chirps_geo_levels(gf)
      if (is.null(ld) || nrow(ld) == 0) return(empty)

      # Country name: name of the single feature at the lowest level
      min_lvl  <- min(ld$level)
      lvl1_sf  <- chirps_geo_from_metadata(gf, min_lvl)
      country  <- if (!is.null(lvl1_sf) && nrow(lvl1_sf) == 1) lvl1_sf$name[1] else NULL

      # Cache sf for each available level
      sf_by_level <- lapply(as.character(ld$level), function(lvl) {
        chirps_geo_from_metadata(gf, as.integer(lvl))
      })
      names(sf_by_level) <- as.character(ld$level)

      list(available    = TRUE,
           levels_df    = ld,
           country_name = country,
           sf_by_level  = sf_by_level)
    })

    # ── Boundary status banner ───────────────────────────────────────────────
    output$geo_status <- renderUI({
      gi <- geo_info()

      if (!gi$available) {
        return(div(
          class = "alert alert-warning",
          style = "padding:8px 10px; margin-bottom:6px; font-size:0.9em;",
          icon("triangle-exclamation"), " No DHIS2 boundary data found.",
          tags$small(br(),
            "Load metadata first. Until then, use a custom shapefile (below).")
        ))
      }

      ld <- gi$levels_df
      country_line <- if (!is.null(gi$country_name))
        tagList(strong(gi$country_name), " — DHIS2 boundaries", br())
      else
        tagList("DHIS2 boundaries loaded", br())

      level_items <- lapply(seq_len(nrow(ld)), function(i) {
        tags$li(sprintf("Level %s — %s (%d polygons)",
                        ld$level[i], ld$levelName[i], as.integer(ld$n_polygons[i])))
      })

      div(
        class = "alert alert-success",
        style = "padding:8px 10px; margin-bottom:6px; font-size:0.9em;",
        icon("circle-check"), " ", country_line,
        tags$ul(style = "margin:2px 0 0 0; padding-left:16px;", level_items)
      )
    })

    # ── Level selector (populated from metadata) ─────────────────────────────
    output$ou_level_ui <- renderUI({
      gi <- geo_info()
      if (!gi$available) return(NULL)

      ld      <- gi$levels_df
      choices <- stats::setNames(
        as.character(ld$level),
        paste0(ld$levelName, " (", ld$n_polygons, " polygons)")
      )
      # Default to level 2 when available; otherwise the second-finest level
      all_lvls  <- as.character(sort(ld$level))
      default   <- if ("2" %in% all_lvls) "2" else all_lvls[min(2, length(all_lvls))]

      selectInput(
        session$ns("ou_level"),
        "Org Unit Level for Analysis",
        choices  = choices,
        selected = default
      )
    })

    # ── Results state ────────────────────────────────────────────────────────
    rv <- reactiveValues(
      # single-year
      results      = NULL,
      year         = NULL,
      # multi-year
      multi_results = NULL,
      years         = NULL,
      # shared
      year_mode    = "single",
      area_label   = NULL,
      level_label  = NULL,
      source_label = NULL,
      flat_df      = NULL,   # combined across years for download
      stats_df     = NULL,   # combined across years for display
      error_msg    = NULL
    )

    # ── Run analysis ─────────────────────────────────────────────────────────
    observeEvent(input$run, {

      if (length(input$months) == 0) {
        showNotification("Select at least one month.", type = "warning")
        return()
      }

      # Determine boundary source
      use_custom <- !is.null(input$shp_file) &&
                    !is.null(input$shx_file) &&
                    !is.null(input$dbf_file)

      gi <- geo_info()

      if (!use_custom && !gi$available) {
        showNotification(
          "No boundaries available. Load metadata or upload a shapefile.",
          type = "warning", duration = 8
        )
        return()
      }

      # Reset state
      rv$results       <- NULL
      rv$multi_results <- NULL
      rv$error_msg     <- NULL
      rv$flat_df       <- NULL
      rv$stats_df      <- NULL

      year_mode <- input$year_mode %||% "single"
      months    <- sort(as.integer(input$months))
      cd        <- cache_dir()
      n_mon     <- length(months)

      withProgress(message = "CHIRPS Analysis", value = 0, {
        tryCatch({

          # ── Step 1: resolve boundary sf (common to both modes) ────────────
          incProgress(0.03, detail = "Loading boundaries…")

          if (use_custom) {
            sf_obj       <- chirps_load_custom_shapefile(
              input$shp_file$datapath, input$shx_file$datapath,
              input$dbf_file$datapath,
              if (!is.null(input$prj_file)) input$prj_file$datapath else NULL
            )
            source_label <- "Custom upload"
            level_label  <- "Custom"
            area_label   <- "Custom Area"
            lvl_str      <- "custom"
          } else {
            lvl_str  <- input$ou_level
            sf_obj   <- gi$sf_by_level[[lvl_str]]
            if (is.null(sf_obj) || nrow(sf_obj) == 0)
              stop("No polygon boundaries found for the selected level.")
            ld           <- gi$levels_df
            lvl_int      <- as.integer(lvl_str)
            level_label  <- ld$levelName[ld$level == lvl_int][1] %||% paste("Level", lvl_str)
            source_label <- "DHIS2 metadata"
            area_label   <- gi$country_name %||% "DHIS2"
          }

          # Helper: process one year through cache + chirps_process_months
          .process_year <- function(yr, prog_offset, prog_total) {
            rds_name <- chirps_results_cache_key(sf_obj, yr, months, lvl_str)
            rds_path <- file.path(cd, rds_name)
            if (file.exists(rds_path)) {
              incProgress(n_mon / prog_total * 0.85,
                          detail = paste0("Loading ", yr, " from cache…"))
              return(readRDS(rds_path))
            }
            dl <- chirps_process_months(
              sf_obj, year = yr, months = months, cache_dir = cd,
              on_progress = function(i, n, detail) {
                incProgress(0.85 / prog_total,
                            detail = paste0(detail, " (", prog_offset + i,
                                            "/", prog_total, ")…"))
              },
              on_error = function(msg)
                showNotification(msg, type = "warning", duration = 10)
            )
            if (length(dl) > 0) saveRDS(dl, rds_path)
            dl
          }

          # Helper: save flat rainfall file to data directory
          .save_flat <- function(flat_df, yr) {
            data_dir <- tryCatch(directory_widget_output$directory(),
                                 error = function(e) NULL)
            if (!is.null(data_dir) && nzchar(data_dir %||% ""))
              tryCatch(
                chirps_save_flat(flat_df, data_dir, yr, lvl_str),
                error = function(e)
                  showNotification(paste0("Rainfall file not saved: ", conditionMessage(e)),
                                   type = "warning", duration = 6)
              )
          }

          # ── Step 2a: single year ───────────────────────────────────────────
          if (year_mode == "single") {
            year      <- as.integer(input$year)
            data_list <- .process_year(year, 0L, n_mon)

            if (length(data_list) == 0)
              stop("No months could be processed. Check date and internet connection.")

            incProgress(0.05, detail = "Building outputs…")
            flat_df  <- chirps_combine_data(data_list, year, source_label, level_label)
            stats_df <- chirps_stats_table(data_list, year)
            .save_flat(flat_df, year)

            rv$results      <- data_list
            rv$year         <- year

          # ── Step 2b: year range ────────────────────────────────────────────
          } else {
            year_start <- as.integer(input$year_start)
            year_end   <- as.integer(input$year_end)

            if (year_start >= year_end)
              stop("'From' year must be earlier than 'To' year.")
            years <- seq(year_start, year_end)

            n_total       <- length(years) * n_mon
            multi_results <- list()
            all_flats     <- list()
            all_stats     <- list()
            offset        <- 0L

            for (yr in years) {
              dl <- .process_year(yr, offset, n_total)
              offset <- offset + n_mon
              multi_results[[as.character(yr)]] <- dl
              if (length(dl) > 0) {
                flat_yr <- chirps_combine_data(dl, yr, source_label, level_label)
                all_flats[[as.character(yr)]] <- flat_yr
                all_stats[[as.character(yr)]] <- chirps_stats_table(dl, yr)
                .save_flat(flat_yr, yr)
              }
            }

            incProgress(0.05, detail = "Building outputs…")
            flat_df  <- if (length(all_flats) > 0) do.call(rbind, all_flats) else NULL
            stats_df <- if (length(all_stats) > 0) do.call(rbind, all_stats) else NULL

            rv$multi_results <- multi_results
            rv$years         <- years
          }

          rv$year_mode    <- year_mode
          rv$area_label   <- area_label
          rv$level_label  <- level_label
          rv$source_label <- source_label
          rv$flat_df      <- flat_df
          rv$stats_df     <- stats_df
          cache_stamp(cache_stamp() + 1)

        }, error = function(e) {
          rv$error_msg <- conditionMessage(e)
        })
      })
    })

    # ── Results panel ────────────────────────────────────────────────────────
    output$results_panel <- renderUI({

      if (!is.null(rv$error_msg)) {
        return(div(
          class = "alert alert-danger", style = "margin:16px;",
          icon("circle-exclamation", style = "margin-right:6px;"),
          strong("Error: "), rv$error_msg
        ))
      }

      # ── Empty state ─────────────────────────────────────────────────────────
      if (is.null(rv$results) && is.null(rv$multi_results)) {
        return(div(
          class = "alert alert-info", style = "margin:16px;",
          icon("circle-info", style = "margin-right:6px;"),
          "Select an org unit level and year (or year range), then click ",
          strong("Run Analysis"), ".",
          br(), br(),
          tags$small(
            "Rainfall: ",
            tags$a("CHIRPS v2.0", href = "https://www.chc.ucsb.edu/data/chirps",
                   target = "_blank"),
            " (UCSB CHC) — global, monthly, ~5 km, 1981–present.",
            br(),
            "Only the area covered by the selected boundaries is downloaded and cached.",
            br(),
            "Boundaries: DHIS2 org unit polygons from loaded metadata."
          )
        ))
      }

      # ── Multi-year results ───────────────────────────────────────────────────
      if (rv$year_mode == "range" && !is.null(rv$multi_results)) {
        yrs       <- rv$years
        title_str <- paste0(rv$area_label %||% "", " \u2014 ", rv$level_label %||% "",
                            " \u2014 ", min(yrs), "\u2013", max(yrs))
        n_yrs     <- length(yrs)
        # ncol=3 in both facet_wrap calls; 300px/row + 120px for title/caption/legend
        map_h     <- paste0(max(400, 300 * ceiling(n_yrs / 3) + 120), "px")

        return(tabsetPanel(
          id = session$ns("result_tabs"),

          tabPanel(
            "Annual Maps", br(),
            p(tags$small(style = "color:#666;", title_str)),
            plotOutput(session$ns("multi_map"), height = map_h)
          ),

          tabPanel(
            "Multi-year Chart", br(),
            p(tags$small(style = "color:#666;", title_str)),
            fluidRow(
              column(5,
                radioButtons(
                  session$ns("multi_chart_view"), NULL,
                  choices  = c("Annual totals" = "annual",
                               "Monthly breakdown" = "monthly"),
                  selected = "annual", inline = TRUE
                )
              )
            ),
            plotOutput(session$ns("multi_chart"), height = "420px")
          ),

          tabPanel(
            "Anomaly", br(),
            p(tags$small(style = "color:#666;", title_str)),
            if (n_yrs >= 2) {
              tagList(
                fluidRow(
                  column(5,
                    radioButtons(
                      session$ns("anomaly_method"), "Units:",
                      choices  = c("mm deviation" = "mm",
                                   "Z-score (SD units)" = "zscore"),
                      selected = "mm", inline = TRUE
                    )
                  )
                ),
                div(
                  style = paste0("background:#f0f4f8; border-left:4px solid #5b9bd5;",
                                 " padding:8px 12px; margin:4px 0 10px 0;",
                                 " border-radius:3px; font-size:0.85em; color:#444;"),
                  icon("circle-info"), " ",
                  "Each panel shows one year's mean rainfall minus the ",
                  n_yrs, "-year average per polygon. ",
                  tags$strong("Blue = wetter than average; red = drier.")
                ),
                plotOutput(session$ns("anomaly_map"), height = map_h)
              )
            } else {
              div(class = "alert alert-warning",
                  "Need at least 2 years to compute anomalies.")
            }
          ),

          tabPanel(
            "Statistics", br(),
            p(tags$small(style = "color:#666;", title_str)),
            DTOutput(session$ns("stats_table"))
          ),

          tabPanel(
            "Data & Download", br(),
            fluidRow(
              column(6, downloadButton(session$ns("dl_csv"),
                                       "Download CSV")),
              column(6, downloadButton(session$ns("dl_excel"),
                                       "Download Excel"))
            ),
            br(),
            p(tags$small(style = "color:#666;",
                         paste0("First 200 rows of ",
                                if (is.null(rv$flat_df)) 0 else nrow(rv$flat_df),
                                " total records (all years combined)"))),
            DTOutput(session$ns("data_preview"))
          )
        ))
      }

      # ── Single-year results ──────────────────────────────────────────────────
      title_str <- paste0(rv$area_label %||% "", " — ", rv$level_label %||% "",
                          " — ", rv$year)

      tabsetPanel(
        id = session$ns("result_tabs"),

        tabPanel(
          "Map Grid", br(),
          p(tags$small(style = "color:#666;", title_str)),
          plotOutput(session$ns("map_grid"),
                     height = paste0(max(300, 200 * ceiling(length(rv$results) / 3)), "px"))
        ),

        tabPanel(
          "Chart", br(),
          p(tags$small(style = "color:#666;", title_str)),
          {
            # Annotation vars — computed once inside renderUI where rv$ is accessible
            n_units    <- tryCatch(nrow(rv$results[[1]]), error = function(e) NA_integer_)
            n_parents  <- tryCatch({
              first_sf <- rv$results[[1]]
              if ("parentName" %in% names(first_sf))
                length(unique(stats::na.omit(first_sf$parentName)))
              else 0L
            }, error = function(e) 0L)
            lvl_lbl  <- rv$level_label %||% "org unit"
            area_lbl <- rv$area_label  %||% ""
            yr       <- rv$year

            tagList(
              # ── Controls row ──────────────────────────────────────────────
              fluidRow(
                column(4,
                  if (n_parents >= 2L && n_parents <= 8L)
                    checkboxInput(session$ns("chart_group_parent"),
                                  paste0("Group by parent unit (", n_parents, ")"),
                                  value = FALSE)
                )
              ),

              # ── Explanatory annotation ────────────────────────────────────
              div(
                style = paste0(
                  "background:#f0f4f8; border-left:4px solid #5b9bd5;",
                  " padding:10px 14px; margin:8px 0 12px 0;",
                  " border-radius:3px; font-size:0.88em; color:#333;"
                ),
                tags$strong("What this chart shows:"), br(),
                tags$ul(
                  style = "margin:4px 0 0 0; padding-left:18px; line-height:1.7;",
                  tags$li(
                    "Each bar is the ", tags$strong("mean CHIRPS rainfall (mm)"),
                    " across all ",
                    if (!is.na(n_units)) paste0(n_units, " ") else "",
                    tags$em(paste0(lvl_lbl, "s")),
                    if (nzchar(area_lbl)) paste0(" in ", area_lbl) else "",
                    " for that month."
                  ),
                  tags$li(
                    "Error bars show ", tags$strong("\u00b11 SD"),
                    " across ", tags$em(paste0(lvl_lbl, "s")),
                    " — reflecting ", tags$strong("geographic spread"),
                    " of rainfall within the area, not variability over time."
                  ),
                  tags$li(
                    "Data shown are for ", tags$strong(yr), " only.",
                    tags$span(
                      style = "color:#666;",
                      " Run the Climate tab for other years to build a multi-year archive",
                      " (see the Data & Download tab for the full record)."
                    )
                  ),
                  tags$li(
                    "Boundaries used: ",
                    tags$strong(paste0(lvl_lbl, " level")),
                    if (!is.na(n_units))
                      paste0(" (", n_units, " polygons).")
                    else "."
                  )
                )
              ),

              plotOutput(session$ns("rain_chart"), height = "420px")
            )
          }
        ),

        tabPanel(
          "Interactive", br(),
          if (!requireNamespace("leaflet", quietly = TRUE)) {
            div(class = "alert alert-warning",
                icon("triangle-exclamation", style = "margin-right:6px;"),
                "Install the ", code("leaflet"), " package for interactive maps.")
          } else {
            tagList(
              fluidRow(
                column(4,
                  selectInput(session$ns("map_month"), "Month",
                              choices  = names(rv$results),
                              selected = names(rv$results)[1])
                )
              ),
              leaflet::leafletOutput(session$ns("map_interactive"), height = "550px")
            )
          }
        ),

        tabPanel(
          "Statistics", br(),
          p(tags$small(style = "color:#666;", title_str)),
          DTOutput(session$ns("stats_table"))
        ),

        tabPanel(
          "Data & Download", br(),
          fluidRow(
            column(6, downloadButton(session$ns("dl_csv"),   "Download CSV")),
            column(6, downloadButton(session$ns("dl_excel"), "Download Excel"))
          ),
          br(),
          p(tags$small(style = "color:#666;",
                       paste0("First 200 rows of ",
                              if (is.null(rv$flat_df)) 0 else nrow(rv$flat_df),
                              " total records"))),
          DTOutput(session$ns("data_preview"))
        )
      )
    })

    # ── Map Grid ─────────────────────────────────────────────────────────────
    output$map_grid <- renderPlot({
      req(rv$results)
      chirps_make_map(rv$results,
                      title_prefix = paste0(rv$area_label %||% "", " — ",
                                            rv$level_label %||% ""),
                      year         = rv$year,
                      palette      = input$palette)
    })

    # ── Bar chart ─────────────────────────────────────────────────────────────
    output$rain_chart <- renderPlot({
      req(rv$results)
      chirps_make_chart(
        rv$results,
        year            = rv$year,
        title_prefix    = paste0(rv$area_label %||% "", " \u2014 ", rv$level_label %||% ""),
        group_by_parent = isTRUE(input$chart_group_parent)
      )
    })

    # ── Multi-year map ────────────────────────────────────────────────────────
    output$multi_map <- renderPlot({
      req(rv$multi_results)
      chirps_multi_year_map(
        rv$multi_results,
        title_prefix = paste0(rv$area_label %||% "", " \u2014 ", rv$level_label %||% ""),
        months       = sort(as.integer(input$months)),
        palette      = input$palette
      )
    })

    # ── Multi-year chart ──────────────────────────────────────────────────────
    output$multi_chart <- renderPlot({
      req(rv$multi_results)
      chirps_multi_year_chart(
        rv$multi_results,
        title_prefix = paste0(rv$area_label %||% "", " \u2014 ", rv$level_label %||% ""),
        view         = input$multi_chart_view %||% "annual"
      )
    })

    # ── Anomaly map ───────────────────────────────────────────────────────────
    output$anomaly_map <- renderPlot({
      req(rv$multi_results)
      req(length(rv$multi_results) >= 2)
      chirps_anomaly_map(
        rv$multi_results,
        title_prefix = paste0(rv$area_label %||% "", " \u2014 ", rv$level_label %||% ""),
        method       = input$anomaly_method %||% "mm"
      )
    })

    # ── Interactive leaflet ───────────────────────────────────────────────────
    # Stable base map — no data dependency so the div is never grey on init.
    # All data layers are pushed via leafletProxy in the observer below.
    output$map_interactive <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
    })
    outputOptions(output, "map_interactive", suspendWhenHidden = FALSE)

    # Push polygon layers whenever results or the selected month change.
    observeEvent(list(rv$results, input$map_month), {
      req(rv$results)
      req(!is.null(input$map_month) && input$map_month %in% names(rv$results))

      sf_month <- rv$results[[input$map_month]]
      vals     <- sf_month$mean_rain

      # Label column: prefer DHIS2 'name', fall back to first NAME_ column
      label_col <- if ("name" %in% names(sf_month)) "name" else {
        nc <- sort(grep("^NAME_", names(sf_month), value = TRUE))
        if (length(nc) > 0) tail(nc, 1) else NULL
      }

      pal <- leaflet::colorNumeric(input$palette, domain = vals, na.color = "#b0b0b0")

      is_centroid <- !is.null(sf_month$fill_method) &
                     !is.na(sf_month$fill_method) &
                     sf_month$fill_method == "centroid"
      method_note <- ifelse(is_centroid, " <i>(centroid est.)</i>", "")
      popup_txt <- if (!is.null(label_col))
        paste0("<b>", sf_month[[label_col]], "</b><br>Rainfall: ",
               round(vals, 1), " mm", method_note)
      else
        paste0("Rainfall: ", round(vals, 1), " mm", method_note)

      leaflet::leafletProxy(session$ns("map_interactive"), session) |>
        leaflet::clearShapes() |>
        leaflet::clearControls() |>
        leaflet::addPolygons(
          data        = sf_month,
          fillColor   = ~pal(mean_rain),
          fillOpacity = 0.75,
          color       = "white", weight = 0.6,
          popup       = popup_txt,
          highlightOptions = leaflet::highlightOptions(
            weight = 2, color = "#555", fillOpacity = 0.9, bringToFront = TRUE
          )
        ) |>
        leaflet::addLegend(
          data     = sf_month,
          pal      = pal,
          values   = ~mean_rain,
          title    = paste0(input$map_month, " ", rv$year, "<br>mm"),
          position = "bottomright",
          na.label = "No data"
        )
    }, ignoreNULL = TRUE)

    # ── Statistics table ──────────────────────────────────────────────────────
    output$stats_table <- DT::renderDT({
      req(rv$stats_df)
      DT::datatable(rv$stats_df, rownames = FALSE,
                    options = list(pageLength = 12, dom = "t"))
    })

    # ── Data preview ──────────────────────────────────────────────────────────
    output$data_preview <- DT::renderDT({
      req(rv$flat_df)
      DT::datatable(head(rv$flat_df, 200), rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    # ── Downloads ─────────────────────────────────────────────────────────────
    output$dl_csv <- downloadHandler(
      filename = function() {
        sprintf("chirps_%s_%s.csv",
                gsub("[^A-Za-z0-9]", "_", rv$area_label %||% "data"),
                rv$year %||% "")
      },
      content = function(file) {
        req(rv$flat_df)
        utils::write.csv(rv$flat_df, file, row.names = FALSE)
      }
    )

    output$dl_excel <- downloadHandler(
      filename = function() {
        sprintf("chirps_%s_%s.xlsx",
                gsub("[^A-Za-z0-9]", "_", rv$area_label %||% "data"),
                rv$year %||% "")
      },
      content = function(file) {
        req(rv$flat_df, rv$stats_df)
        bytes <- chirps_write_excel(
          flat_df      = rv$flat_df,
          stats_df     = rv$stats_df,
          year         = rv$year,
          area_label   = rv$area_label   %||% "",
          months       = sort(as.integer(input$months)),
          source_label = rv$source_label %||% "",
          level_label  = rv$level_label  %||% ""
        )
        writeBin(bytes, file)
      }
    )

  })
}
