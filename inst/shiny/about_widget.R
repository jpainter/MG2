# about_widget.R — Static About / Help tab
#
# Renders the package README.md as in-app documentation.
# No server function needed — UI only.
#
# Path resolution works for both:
#   - devtools::load_all()  (source directory)
#   - installed package     (installation directory)
#
# When README.md changes, this tab updates automatically on next app launch.

about_widget_ui <- function(id) {
  ns <- NS(id)

  readme_path <- system.file("README.md", package = "MG2")
  if (!nzchar(readme_path) || !file.exists(readme_path))
    readme_path <- file.path(getwd(), "..", "..", "README.md")

  tagList(
    tags$div(
      style = "max-width: 860px; margin: 0 auto; padding: 24px 32px;",

      # Live dependency summary
      uiOutput(ns("dep_summary")),

      if (file.exists(readme_path)) {
        shiny::includeMarkdown(readme_path)
      } else {
        tagList(
          p("Documentation not found locally. Please see the README on GitHub:"),
          tags$a("github.com/jpainter/MG2",
                 href = "https://github.com/jpainter/MG2", target = "_blank")
        )
      }
    )
  )
}

about_widget_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$dep_summary <- renderUI({
      deps <- check_mg2_dependencies()

      warn_ui <- if (length(deps$warnings) > 0)
        div(
          style = "background:#fff3cd; padding:10px 14px; border-left:4px solid #ffc107; border-radius:3px; margin-bottom:12px;",
          tags$strong("Compatibility warnings:"),
          tags$ul(lapply(deps$warnings, tags$li))
        )

      info_ui <- div(
        style = "background:#e8f4fd; padding:10px 14px; border-left:4px solid #2196F3; border-radius:3px; margin-bottom:20px;",
        tags$strong("Environment:"),
        tags$ul(style = "margin:4px 0 0 0; padding-left:18px; columns:2;",
                lapply(deps$info, tags$li))
      )

      tagList(warn_ui, info_ui)
    })
  })
}
