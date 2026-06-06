# MG2 demo deployment — shinyapps.io entry point
#
# Deploy with:
#   rsconnect::deployApp(appDir = "deploy", appName = "MG2-MagicGlasses2")

Sys.setenv(MG2_DEMO_MODE = "1")

# Install MG2 from the bundled source tarball if not already present.
# Package name is assembled at runtime so rsconnect's static dependency
# scanner does not flag it as a GitHub package and break the manifest.
.pkg <- paste0("MG", "2")
if (!nchar(system.file(package = .pkg))) {
  message("Installing MG2 from bundled tarball...")
  install.packages("MG2_0.1.5.tar.gz", repos = NULL, type = "source",
                   quiet = TRUE)
}
library(.pkg, character.only = TRUE)
shiny::shinyAppDir(system.file("shiny", package = .pkg))
