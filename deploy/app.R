# MG2 demo deployment
#
# Connect Cloud: deploy via GitHub at connectcloud.posit.co
# shinyapps.io:  rsconnect::deployApp(appDir="deploy", appName="MG2-MagicGlasses2")

Sys.setenv(MG2_DEMO_MODE = "1")

# qs2 is not in Connect Cloud's managed package list — install it first
# so MG2 (which lists qs2 in Imports) can be installed successfully.
if (!requireNamespace("qs2", quietly = TRUE)) {
  message("Installing qs2...")
  install.packages("qs2", repos = "https://cloud.r-project.org", quiet = TRUE)
}

# Install or upgrade MG2.
# Connect Cloud clones the entire jpainter/MG2 repo, so the package
# source is at ../ relative to this deploy/ directory — no download needed.
# shinyapps.io only bundles deploy/ contents, so fall back to the tarball.
.pkg     <- paste0("MG", "2")
.target  <- "0.1.7"
.needs_install <- !nchar(system.file(package = .pkg)) ||
  tryCatch(utils::packageVersion(.pkg) < .target, error = function(e) TRUE)

if (.needs_install) {
  local_src <- tryCatch(normalizePath(".."), error = function(e) "")
  if (nzchar(local_src) && file.exists(file.path(local_src, "DESCRIPTION"))) {
    message("Installing MG2 ", .target, " from local repo source...")
    install.packages(local_src, repos = NULL, type = "source",
                     INSTALL_opts = "--no-build-vignettes --no-manual",
                     quiet = TRUE)
  } else if (file.exists("MG2_0.1.7.tar.gz")) {
    message("Installing MG2 ", .target, " from bundled tarball...")
    install.packages("MG2_0.1.7.tar.gz", repos = NULL, type = "source",
                     quiet = TRUE)
  }
}

library(.pkg, character.only = TRUE)
shiny::shinyAppDir(system.file("shiny", package = .pkg))
