# fct_cluster_manager.R
# Dev tool: Shiny gadget for managing local DHIS2 d2 cluster instances.
# Launcher: manage_clusters()
#
# Prerequisites (not hard package deps — checked at runtime):
#   processx, rappdirs, httr2
# Docker Desktop (with Compose v2) and the d2 CLI must be in PATH.
#   Install d2: npm install -g @dhis2/cli
#   Apple Silicon: use an ARM64 Node.js (nvm or Homebrew).

# ---- Internal helpers -------------------------------------------------------

.nullish <- function(x, default) if (is.null(x) || (length(x) == 1L && is.na(x))) default else x




# Run a command synchronously; returns list(status, stdout, stderr, timed_out).
.cm_run <- function(cmd, args = character(), timeout = 30) {
  tryCatch(
    processx::run(cmd, args, timeout = timeout, error_on_status = FALSE),
    error = function(e) list(status = -1L, stdout = "", stderr = conditionMessage(e), timed_out = FALSE)
  )
}

.cm_which <- function(cmd) {
  r <- .cm_run("which", cmd, timeout = 5)
  if (r$status == 0L) trimws(r$stdout) else NULL
}

# ---- Environment checks -----------------------------------------------------

#' Report whether Docker is usable by `d2 cluster`
#'
#' `d2 cluster up` drives docker-compose, so a missing or stopped Docker
#' surfaces as a wall of compose output ending in `exit code: 1`. Call this
#' first to get a usable diagnosis instead.
#'
#' Note the `compose_v1` field: `d2 cluster` invokes the standalone
#' `docker-compose` binary, **not** Docker Desktop's built-in `docker compose`
#' plugin, so Docker can be installed and running and Step 2 still fail.
#'
#' @return A list with `installed`, `running`, `compose_v1`, `compose_v2`
#'   (all logical), `version` (character) and `msg` (a human-readable reason,
#'   or `NULL` when everything needed is present).
#' @noRd
mg2_check_docker <- function() {
  # .cm_run() needs processx, which is a Suggests. Say so rather than erroring.
  if (!requireNamespace("processx", quietly = TRUE)) {
    return(list(
      installed = FALSE, running = FALSE, version = NA_character_,
      compose_v1 = FALSE, compose_v2 = FALSE,
      msg = paste("The 'processx' package is required to check Docker.",
                  'Install with: install.packages("processx")')
    ))
  }
  if (is.null(.cm_which("docker"))) {
    return(list(
      installed = FALSE, running = FALSE,
      version = NA_character_, compose_v1 = FALSE, compose_v2 = FALSE,
      msg = paste(
        "Docker not found in PATH.",
        "Install Docker Desktop from https://www.docker.com/products/docker-desktop/"
      )
    ))
  }
  info <- .cm_run("docker", "info", timeout = 15)
  if (info$status != 0L) {
    return(list(
      installed = TRUE, running = FALSE,
      version = NA_character_, compose_v1 = FALSE, compose_v2 = FALSE,
      msg = paste("Docker daemon not running:", trimws(info$stderr))
    ))
  }
  ver <- .cm_run("docker", c("version", "--format", "{{.Server.Version}}"), timeout = 10)
  version <- if (ver$status == 0L) trimws(ver$stdout) else "unknown"

  # d2 cluster calls the standalone `docker-compose` v1 binary — NOT the
  # `docker compose` v2 plugin. Check for both and report separately.
  compose_v1_ok <- !is.null(.cm_which("docker-compose"))
  compose_v2_ok <- .cm_run("docker", c("compose", "version"), timeout = 10)$status == 0L

  list(
    installed    = TRUE, running = TRUE,
    version      = version,
    compose_v1   = compose_v1_ok,   # needed by d2 cluster
    compose_v2   = compose_v2_ok,   # Docker Desktop built-in plugin
    msg = if (!compose_v1_ok)
      "docker-compose (v1 binary) not found. d2 cluster requires it. Fix: brew install docker-compose"
    else NULL
  )
}



# ---- Cluster list + status --------------------------------------------------

# d2 cluster list has no JSON output mode; parse the table and supplement with
# docker ps for accurate running status.


# ---- Port suggestion --------------------------------------------------------



# ---- Version match from live instance ---------------------------------------


# ---- Async process management -----------------------------------------------

# Background processes are stored here: id -> list(proc, log, logged_count)
.cm_procs <- new.env(parent = emptyenv())


# Poll a running process; returns list(alive, new_lines, exit_status).

# ---- UI ---------------------------------------------------------------------


# ---- Server -----------------------------------------------------------------


# ---- Exported launcher ------------------------------------------------------

