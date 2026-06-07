# MG2 demo deployment
#
# Connect Cloud: deploy via GitHub at connectcloud.posit.co
# shinyapps.io:  rsconnect::deployApp(appDir="deploy", appName="MG2-MagicGlasses2")

Sys.setenv(MG2_DEMO_MODE = "1")

# Install MG2 if not present.
# Connect Cloud clones the entire jpainter/MG2 repo, so the package
# source is at ../ relative to this deploy/ directory — no download needed.
# shinyapps.io only bundles deploy/ contents, so fall back to the tarball.
.pkg <- paste0("MG", "2")
if (!nchar(system.file(package = .pkg))) {
  local_src <- tryCatch(normalizePath(".."), error = function(e) "")
  if (nzchar(local_src) && file.exists(file.path(local_src, "DESCRIPTION"))) {
    message("Installing MG2 from local repo source...")
    install.packages(local_src, repos = NULL, type = "source",
                     INSTALL_opts = "--no-build-vignettes --no-manual",
                     quiet = TRUE)
  } else if (file.exists("MG2_0.1.6.tar.gz")) {
    message("Installing MG2 from bundled tarball...")
    install.packages("MG2_0.1.6.tar.gz", repos = NULL, type = "source",
                     quiet = TRUE)
  }
}

library(.pkg, character.only = TRUE)
shiny::shinyAppDir(system.file("shiny", package = .pkg))
