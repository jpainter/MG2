# MG2 demo deployment
#
# Connect Cloud: deploy via GitHub at connectcloud.posit.co
# shinyapps.io:  rsconnect::deployApp(appDir="deploy", appName="MG2-MagicGlasses2")

Sys.setenv(MG2_DEMO_MODE = "1")

# Packages not in Connect Cloud's managed list — install from Posit Package Manager
# for pre-built Linux binaries (no compilation → installs in seconds).
# Auto-detect the OS codename so binaries match the server's GLIBC version.
.os <- tryCatch({
  lines <- readLines("/etc/os-release", warn = FALSE)
  sub("^VERSION_CODENAME=", "", grep("^VERSION_CODENAME=", lines, value = TRUE))
}, error = function(e) character(0))
.os <- if (length(.os) == 1L && nzchar(.os)) .os else "jammy"  # safe default
message("PPM target OS: ", .os)
.ppm <- paste0("https://packagemanager.posit.co/cran/__linux__/", .os, "/latest")
for (.p in c("qs2", "future.apply", "feasts", "ggtime")) {
  if (!requireNamespace(.p, quietly = TRUE)) {
    message("Installing ", .p, " from Posit Package Manager (pre-built binary)...")
    install.packages(.p, repos = .ppm, quiet = TRUE)
  }
}

# Install or upgrade MG2.
# Connect Cloud clones the entire jpainter/MG2 repo, so the package source is
# at ../ relative to this deploy/ directory.  Re-install whenever the repo HEAD
# differs from the SHA recorded at the last install — no version bumps needed.
.pkg      <- paste0("MG", "2")
.local    <- tryCatch(normalizePath(".."), error = function(e) "")
.has_src  <- nzchar(.local) && file.exists(file.path(.local, "DESCRIPTION"))

# Current HEAD of the cloned repo (empty string if git unavailable).
.head_sha <- if (.has_src) {
  tryCatch(
    system2("git", c("-C", .local, "rev-parse", "HEAD"),
            stdout = TRUE, stderr = FALSE)[1L],
    error = function(e) ""
  )
} else ""

# SHA recorded from the previous install (persists in the package dir).
.sha_file      <- file.path(path.expand("~"), ".mg2_deploy_sha")
.installed_sha <- if (file.exists(.sha_file))
  tryCatch(readLines(.sha_file, warn = FALSE)[1L], error = function(e) "")
else ""

.needs_install <-
  !nzchar(system.file(package = .pkg)) ||          # not installed at all
  (nzchar(.head_sha) && .head_sha != .installed_sha) || # code changed
  (!nzchar(.head_sha) &&                           # git unavailable: fall back
     tryCatch(utils::packageVersion(.pkg) < "0.1.8",
              error = function(e) TRUE))

if (.needs_install && .has_src) {
  message("Installing MG2 from local repo source (SHA: ",
          if (nzchar(.head_sha)) substr(.head_sha, 1, 7) else "unknown", ")...")
  install.packages(.local, repos = NULL, type = "source",
                   INSTALL_opts = "--no-build-vignettes --no-manual",
                   quiet = TRUE)
  if (nzchar(.head_sha))
    writeLines(.head_sha, .sha_file)
}

library(.pkg, character.only = TRUE)
shiny::shinyAppDir(system.file("shiny", package = .pkg))
