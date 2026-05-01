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

  # Locate README.md --------------------------------------------------------
  readme_path <- system.file("README.md", package = "MG2")

  # Fallback for development environments where system.file may not resolve
  if (!nzchar(readme_path) || !file.exists(readme_path)) {
    readme_path <- file.path(getwd(), "..", "..", "README.md")
  }

  # UI ----------------------------------------------------------------------
  tagList(
    tags$div(
      style = "max-width: 860px; margin: 0 auto; padding: 24px 32px;",

      if (file.exists(readme_path)) {
        shiny::includeMarkdown(readme_path)
      } else {
        tagList(
          p("Documentation not found locally. Please see the README on GitHub:"),
          tags$a("github.com/jpainter/MG2",
                 href   = "https://github.com/jpainter/MG2",
                 target = "_blank")
        )
      }
    )
  )
}
