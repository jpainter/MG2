# chart_module.R

chartModuleUI <- function(id, label = "Chart") {
  ns <- NS(id)
  tagList(
    # CSS to make buttons smaller
    tags$style(HTML(
      "
      .compact-button .btn {
        font-size: 14px;
        padding: 2px 8px;
        height: 30px;
        line-height: 1.2;
      }
    "
    )),

    div(
      style = "text-align: center;",
      div(
        style = "display: inline-block; margin-right: 20px;",
        h3(style = "font-size: 16px; margin: 0;", label)
      ),
      div(
        class = "compact-button",
        style = "display: inline-block; margin-right: 10px;",
        actionButton(ns("open_modal"), "Customize")
      ),
      div(
        class = "compact-button",
        style = "display: inline-block;",
        downloadButton(ns("download_plot"), label = NULL)
      )
    ),

    plotOutput(ns("plot"), height = "90%")
  )
}


chartModuleServer <- function(id, plot_expr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      filename = "chart",
      title = NULL,
      subtitle = NULL,
      caption = NULL,
      base.size = 20,
      legend.position = "bottom",
      format = "png",
      width = 6,
      height = 4,
      dpi = 300,
      transparent = FALSE,
      theme = "bw",
      ready = TRUE
    )

    # Show modal
    observeEvent(input$open_modal, {
      showModal(modalDialog(
        title = "Download Options",
        textInput(ns("filename"), "Filename (no extension)", "filename"),
        textInput(ns("title"), "Plot title:", ""),
        textInput(ns("subtitle"), "Subtitle:", ""),
        textInput(ns("caption"), "Caption:", ""),
        numericInput(ns("base.size"), "Font size:", 14),
        selectInput(
          ns("legend.position"),
          "Legend position:",
          choices = c("none", "bottom", "top", "right")
        ),
        selectInput(ns("format"), "Format:", choices = c("png", "pdf", "svg")),
        numericInput(ns("width"), "Width (inches):", 6),
        numericInput(ns("height"), "Height (inches):", 4),
        numericInput(ns("dpi"), "DPI:", 300),
        checkboxInput(
          ns("transparent"),
          "Transparent background (PNG/SVG only)",
          FALSE
        ),
        selectInput(
          ns("theme"),
          "Plot theme:",
          choices = c(
            "Gray (default)" = "gray",
            "Minimal" = "minimal",
            "Classic" = "classic",
            "Light" = "light",
            "Dark" = "dark",
            "Black & White" = "bw"
          ),
          selected = 1
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_modal"), "Apply")
        ),
        easyClose = TRUE
      ))
    })

    # Confirm and prepare download
    observeEvent(input$confirm_modal, {
      rv$filename <- input$filename
      rv$title <- input$title
      rv$subtitle <- input$subtitle
      rv$caption <- input$caption
      rv$base.size <- input$base.size
      rv$legend.position <- input$legend.position
      rv$format <- input$format
      rv$width <- input$width
      rv$height <- input$height
      rv$dpi <- input$dpi
      rv$transparent <- input$transparent
      rv$theme <- input$theme
      rv$ready <- TRUE
      removeModal()
    })

    # Theme selector — pass base_size so all text scales correctly
    get_theme <- function(theme_key = 'default', base_size = 20) {
      switch(
        theme_key,
        "minimal"  = theme_minimal(base_size = base_size),
        "classic"  = theme_classic(base_size = base_size),
        "light"    = theme_light(base_size = base_size),
        "dark"     = theme_dark(base_size = base_size),
        "bw"       = theme_bw(base_size = base_size),
        theme_gray(base_size = base_size)
      )
    }

    # Display plot — res = 96 prevents HiDPI double-scaling of text
    output$plot <- renderPlot(res = 96, {
      p <- plot_expr() +
        get_theme(rv$theme, rv$base.size) +
        theme(
          text           = element_text(size = rv$base.size),
          legend.position = rv$legend.position,
          strip.text     = element_text(face = "bold", size = rv$base.size),
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA)
        )
      # Only override labs that the user explicitly set — passing NULL clears them
      overrides <- Filter(function(x) !is.null(x) && nzchar(x),
                          list(title = rv$title, subtitle = rv$subtitle, caption = rv$caption))
      if (length(overrides) > 0) p <- p + do.call(labs, overrides)
      p
    })

    # Download handler
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0(rv$filename, "_", Sys.Date(), ".", rv$format)
      },
      content = function(file) {
        plot <- plot_expr() +
          get_theme(rv$theme, rv$base.size) +
          theme(
            text             = element_text(size = rv$base.size),
            legend.position  = rv$legend.position,
            plot.background  = element_rect(fill = "white", colour = NA),
            panel.background = element_rect(fill = "white", colour = NA)
          )
        overrides <- Filter(function(x) !is.null(x) && nzchar(x),
                            list(title = rv$title, subtitle = rv$subtitle, caption = rv$caption))
        if (length(overrides) > 0) plot <- plot + do.call(labs, overrides)

        ggsave(
          filename = file,
          plot = plot,
          device = rv$format,
          width = rv$width,
          height = rv$height,
          dpi = rv$dpi,
          bg = if (rv$transparent && rv$format %in% c("png", "svg")) {
            "transparent"
          } else {
            "white"
          }
        )
      }
    )
  })
}
