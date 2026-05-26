# chart_module.R
#
# Two layout modes:
#   overlay = FALSE (default, original)
#     Title centred on one line, Customize and Download buttons inline,
#     then the plot below.  Used by the Evaluation tab.
#
#   overlay = TRUE
#     No extra row.  A small gear icon and download icon are absolutely
#     positioned in the top-right corner of the plot area so they do not
#     consume any vertical space.  Used everywhere else.

# Extra plotOutput arguments (click, hover, brush, dblclick) are forwarded via ...
chartModuleUI <- function(id, label = "Chart", height = "400px",
                          overlay = FALSE, ...) {
  ns <- NS(id)

  btn_css <- "
    .chart-overlay-controls {
      position: absolute; top: 6px; right: 8px;
      z-index: 100; display: flex; gap: 4px;
      opacity: 0.25; transition: opacity 0.15s;
    }
    .chart-overlay-wrapper:hover .chart-overlay-controls { opacity: 1; }
    .chart-overlay-controls .btn {
      padding: 2px 6px; font-size: 12px; line-height: 1.4;
      background: rgba(255,255,255,0.85); border: 1px solid #ccc;
    }
    .compact-button .btn {
      font-size: 14px; padding: 2px 8px;
      height: 30px; line-height: 1.2;
    }
  "

  if (overlay) {
    tagList(
      tags$style(HTML(btn_css)),
      div(
        class = "chart-overlay-wrapper",
        style = "position: relative;",
        plotOutput(ns("plot"), height = height, ...),
        div(
          class = "chart-overlay-controls",
          actionButton(
            ns("open_modal"), label = NULL,
            icon  = shiny::icon("gear"),
            class = "btn-sm btn-default",
            title = "Customize / Download"
          ),
          downloadButton(
            ns("download_plot"),
            label = NULL,
            class = "btn-sm btn-default",
            style = "padding:2px 6px; font-size:12px;",
            title = "Download chart"
          )
        )
      )
    )
  } else {
    tagList(
      tags$style(HTML(btn_css)),
      div(
        style = "text-align: center;",
        div(style = "display: inline-block; margin-right: 20px;",
            h3(style = "font-size: 16px; margin: 0;", label)),
        div(class = "compact-button",
            style = "display: inline-block; margin-right: 10px;",
            actionButton(ns("open_modal"), "Customize")),
        div(class = "compact-button",
            style = "display: inline-block;",
            downloadButton(ns("download_plot"), label = NULL))
      ),
      plotOutput(ns("plot"), height = height, ...)
    )
  }
}


chartModuleServer <- function(id, plot_expr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      filename       = "chart",
      title          = NULL,
      subtitle       = NULL,
      caption        = NULL,
      base.size      = 14,
      legend.position = "bottom",
      format         = "png",
      width          = 8,
      height         = 5,
      dpi            = 300,
      transparent    = FALSE,
      theme          = "bw",
      ready          = TRUE
    )

    observeEvent(input$open_modal, {
      showModal(modalDialog(
        title = "Download Options",
        fade  = FALSE,
        textInput(ns("filename"),   "Filename (no extension)", rv$filename),
        textInput(ns("title"),      "Plot title:",    rv$title    %||% ""),
        textInput(ns("subtitle"),   "Subtitle:",      rv$subtitle %||% ""),
        textInput(ns("caption"),    "Caption:",       rv$caption  %||% ""),
        fluidRow(
          column(6, numericInput(ns("width"),  "Width (in):",  rv$width)),
          column(6, numericInput(ns("height"), "Height (in):", rv$height))
        ),
        fluidRow(
          column(6, numericInput(ns("base.size"), "Font size:", rv$base.size)),
          column(6, numericInput(ns("dpi"),       "DPI:",       rv$dpi))
        ),
        fluidRow(
          column(6,
            selectInput(ns("format"), "Format:",
                        choices = c("png", "pdf", "svg"), selected = rv$format)),
          column(6,
            selectInput(ns("legend.position"), "Legend:",
                        choices = c("bottom", "top", "right", "none"),
                        selected = rv$legend.position))
        ),
        selectInput(ns("theme"), "Theme:",
          choices = c("Black & White" = "bw", "Minimal" = "minimal",
                      "Classic" = "classic", "Light" = "light",
                      "Gray (default)" = "gray", "Dark" = "dark"),
          selected = rv$theme),
        checkboxInput(ns("transparent"), "Transparent background (PNG/SVG only)",
                      rv$transparent),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_modal"), "Apply", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_modal, {
      rv$filename        <- input$filename
      rv$title           <- input$title
      rv$subtitle        <- input$subtitle
      rv$caption         <- input$caption
      rv$base.size       <- input$base.size
      rv$legend.position <- input$legend.position
      rv$format          <- input$format
      rv$width           <- input$width
      rv$height          <- input$height
      rv$dpi             <- input$dpi
      rv$transparent     <- input$transparent
      rv$theme           <- input$theme
      rv$ready           <- TRUE
      removeModal()
    })

    get_theme <- function(theme_key = "bw", base_size = 14) {
      switch(theme_key,
        "minimal" = theme_minimal(base_size = base_size),
        "classic" = theme_classic(base_size = base_size),
        "light"   = theme_light(base_size  = base_size),
        "dark"    = theme_dark(base_size   = base_size),
        "bw"      = theme_bw(base_size     = base_size),
        theme_gray(base_size = base_size)
      )
    }

    build_plot <- function() {
      p <- plot_expr() +
        get_theme(rv$theme, rv$base.size) +
        theme(
          text             = element_text(size = rv$base.size),
          legend.position  = rv$legend.position,
          strip.text       = element_text(face = "bold", size = rv$base.size),
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA)
        )
      overrides <- Filter(function(x) !is.null(x) && nzchar(x),
                          list(title    = rv$title,
                               subtitle = rv$subtitle,
                               caption  = rv$caption))
      if (length(overrides) > 0) p <- p + do.call(labs, overrides)
      p
    }

    output$plot <- renderPlot(res = 96, { build_plot() })

    output$download_plot <- downloadHandler(
      filename = function() paste0(rv$filename, "_", Sys.Date(), ".", rv$format),
      content  = function(file) {
        ggsave(
          filename = file,
          plot     = build_plot(),
          device   = rv$format,
          width    = rv$width,
          height   = rv$height,
          dpi      = rv$dpi,
          bg       = if (rv$transparent && rv$format %in% c("png", "svg"))
                       "transparent" else "white"
        )
      }
    )

    # Return plot interaction events so the parent server can observe them
    # (e.g. brush selections, hover tooltips).  Access via module_return$brush()
    list(
      click    = reactive(input$plot_click),
      dblclick = reactive(input$plot_dblclick),
      hover    = reactive(input$plot_hover),
      brush    = reactive(input$plot_brush)
    )
  })
}
