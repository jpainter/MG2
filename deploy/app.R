# MG2 demo deployment — shinyapps.io entry point
#
# Deploy with:
#   rsconnect::deployApp(appDir = "deploy", appName = "MG2-MagicGlasses2")

Sys.setenv(MG2_DEMO_MODE = "1")

# Install or upgrade MG2 from the bundled source tarball.
# Checks installed version so a re-deploy with a new tarball always upgrades.
.pkg     <- paste0("MG", "2")
.tarball <- "MG2_0.1.6.tar.gz"
.target  <- "0.1.6"
.needs_install <- !nchar(system.file(package = .pkg)) ||
  tryCatch(
    utils::packageVersion(.pkg) < .target,
    error = function(e) TRUE
  )
if (.needs_install) {
  message("Installing MG2 ", .target, " from bundled tarball...")
  install.packages(.tarball, repos = NULL, type = "source", quiet = TRUE)
}

library(.pkg, character.only = TRUE)
shiny::shinyAppDir(system.file("shiny", package = .pkg))
