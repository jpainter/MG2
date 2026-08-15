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

.cm_config_path <- function() {
  dir <- rappdirs::user_config_dir("mg2-cluster")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, "config.json")
}

.cm_read_config <- function() {
  path <- .cm_config_path()
  default <- list(seeds_dir = "", source_instances = list())
  if (!file.exists(path)) return(default)
  tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) default
  )
}

.cm_write_config <- function(cfg) {
  jsonlite::write_json(cfg, .cm_config_path(), auto_unbox = TRUE, pretty = TRUE)
}

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

.cm_check_docker <- function() {
  if (is.null(.cm_which("docker"))) {
    return(list(
      installed = FALSE, running = FALSE,
      version = NA_character_, compose_v2 = FALSE,
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
      version = NA_character_, compose_v2 = FALSE,
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

.cm_check_d2 <- function() {
  if (is.null(.cm_which("d2"))) {
    return(list(
      installed = FALSE, version = NA_character_,
      msg = paste(
        "d2 CLI not found in PATH.",
        "Install with: npm install -g @dhis2/cli",
        "On Apple Silicon: ensure Node.js is ARM64 (use nvm or Homebrew).",
        sep = "\n"
      )
    ))
  }
  ver <- .cm_run("d2", "--version", timeout = 10)
  list(
    installed = TRUE,
    version = if (ver$status == 0L) trimws(ver$stdout) else "unknown",
    msg = NULL
  )
}

.cm_docker_df <- function() {
  r <- .cm_run("docker", c("system", "df"), timeout = 20)
  if (r$status == 0L) trimws(r$stdout) else paste("Error:", trimws(r$stderr))
}

# ---- Cluster list + status --------------------------------------------------

# d2 cluster list has no JSON output mode; parse the table and supplement with
# docker ps for accurate running status.
.cm_docker_ps_map <- function() {
  # Returns a named list: cluster_name -> status string
  r <- .cm_run("docker",
    c("ps", "-a", "--format", "{{.Names}}\t{{.Status}}", "--filter", "name=d2-cluster"),
    timeout = 15)
  if (r$status != 0L || !nzchar(trimws(r$stdout))) return(list())
  lines <- strsplit(trimws(r$stdout), "\n")[[1]]
  lines <- lines[nzchar(lines)]
  result <- list()
  for (l in lines) {
    parts <- strsplit(l, "\t")[[1]]
    if (length(parts) < 2L) next
    name <- parts[[1]]
    status <- parts[[2]]
    # Container names: d2-cluster-<cluster>-core-1 or similar
    m <- regmatches(name, regexpr("d2-cluster-(.+?)-(core|db|gateway)", name, perl = TRUE))
    if (!length(m)) next
    cluster_name <- gsub("^d2-cluster-|-core$|-db$|-gateway$", "", m)
    # Aggregate: prefer Running over Exited
    existing <- result[[cluster_name]]
    if (is.null(existing) || grepl("^Up", status, ignore.case = TRUE)) {
      result[[cluster_name]] <- status
    }
  }
  result
}

.cm_list_clusters <- function() {
  empty <- data.frame(
    name = character(), version = character(), port = character(),
    channel = character(), db_version = character(), status = character(),
    stringsAsFactors = FALSE
  )
  r <- .cm_run("d2", c("cluster", "list"), timeout = 20)
  if (r$status != 0L || !nzchar(trimws(r$stdout))) return(empty)

  lines <- strsplit(trimws(r$stdout), "\n")[[1]]
  # Drop header row(s) and separator lines
  lines <- lines[
    !grepl("^[-=\u2500]+$", lines) &
    !grepl("(?i)^\\s*name\\s+port", lines)
  ]
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(empty)

  # Split on 2+ consecutive spaces (table column separator)
  parse_row <- function(l) {
    parts <- strsplit(trimws(l), "\\s{2,}")[[1]]
    if (length(parts) < 3L) return(NULL)
    list(
      name       = .nullish(parts[1L], ""),
      port       = .nullish(parts[2L], ""),
      channel    = .nullish(parts[3L], ""),
      version    = if (length(parts) >= 4L) parts[4L] else "",
      db_version = if (length(parts) >= 5L) parts[5L] else "",
      status     = if (length(parts) >= 6L) parts[6L] else "Unknown"
    )
  }
  rows <- Filter(Negate(is.null), lapply(lines, parse_row))
  if (!length(rows)) return(empty)

  df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))

  # Override status with live docker ps info (more reliable than d2's parser)
  ps <- .cm_docker_ps_map()
  if (length(ps)) {
    for (i in seq_len(nrow(df))) {
      nm <- df$name[i]
      if (!is.null(ps[[nm]])) {
        raw <- ps[[nm]]
        df$status[i] <- if (grepl("^Up", raw, ignore.case = TRUE)) "Running"
                        else if (grepl("Exited", raw, ignore.case = TRUE)) "Exited"
                        else if (grepl("Paused", raw, ignore.case = TRUE)) "Paused"
                        else raw
      }
    }
  }
  df
}

# ---- Port suggestion --------------------------------------------------------

.cm_used_ports <- function() {
  r <- .cm_run("docker", c("ps", "--format", "{{.Ports}}"), timeout = 10)
  if (r$status != 0L || !nzchar(trimws(r$stdout))) return(integer(0))
  m <- gregexpr(":(\\d+)->", r$stdout, perl = TRUE)
  raw <- regmatches(r$stdout, m)[[1]]
  if (!length(raw)) return(integer(0))
  as.integer(gsub("[^0-9]", "", raw))
}

.cm_suggest_port <- function(start = 8081L) {
  used <- .cm_used_ports()
  p <- start
  while (p %in% used) p <- p + 1L
  p
}

# ---- Version match from live instance ---------------------------------------

.cm_fetch_instance_version <- function(base_url,
                                        username = NULL,
                                        password = NULL) {
  url <- paste0(gsub("/$", "", base_url), "/api/system/info.json")
  req <- httr2::request(url) |> httr2::req_timeout(10)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  if (!is.null(username) && nzchar(username)) {
    req <- httr2::req_auth_basic(req, username, .nullish(password, ""))
  }
  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp)) return(list(ok = FALSE, msg = "Connection failed."))
  code <- httr2::resp_status(resp)
  if (code != 200L) return(list(ok = FALSE, msg = paste("HTTP", code)))
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(body)) return(list(ok = FALSE, msg = "Could not parse JSON response."))
  ver <- body$version
  if (is.null(ver)) return(list(ok = FALSE, msg = "No 'version' field in system/info.json."))
  list(ok = TRUE, version = ver)
}

# ---- Async process management -----------------------------------------------

# Background processes are stored here: id -> list(proc, log, logged_count)
.cm_procs <- new.env(parent = emptyenv())

.cm_start_proc <- function(id, cmd, args) {
  # Kill any existing process with same id
  if (!is.null(.cm_procs[[id]]) && isTRUE(.cm_procs[[id]]$proc$is_alive())) {
    try(.cm_procs[[id]]$proc$kill(), silent = TRUE)
  }
  p <- processx::process$new(cmd, args, stdout = "|", stderr = "2>&1", cleanup = TRUE)
  .cm_procs[[id]] <- list(proc = p, log = character(0), logged_count = 0L)
  invisible(id)
}

# Poll a running process; returns list(alive, new_lines, exit_status).
.cm_poll_proc <- function(id) {
  entry <- .cm_procs[[id]]
  if (is.null(entry)) return(list(alive = FALSE, new_lines = character(0), exit_status = NA_integer_))

  new_out <- tryCatch(entry$proc$read_output_lines(), error = function(e) character(0))
  entry$log <- c(entry$log, new_out)
  prev_count <- entry$logged_count
  new_lines <- entry$log[seq_len(length(entry$log) - prev_count) + prev_count]
  entry$logged_count <- length(entry$log)
  .cm_procs[[id]] <- entry

  alive <- isTRUE(entry$proc$is_alive())
  list(
    alive      = alive,
    new_lines  = new_lines,
    exit_status = if (!alive) tryCatch(entry$proc$get_exit_status(), error = function(e) NA_integer_) else NA_integer_
  )
}

# ---- UI ---------------------------------------------------------------------

.cm_ui <- function() {
  bslib::page_navbar(
    title = "DHIS2 Cluster Manager",
    theme = bslib::bs_theme(version = 5L, bootswatch = "flatly"),
    window_title = "DHIS2 Cluster Manager",

    # ---- Clusters tab -------------------------------------------------------
    bslib::nav_panel(
      "Clusters",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 260L,
          shiny::actionButton(
            "refresh_clusters", "Refresh list",
            icon = shiny::icon("rotate"), class = "btn-sm btn-outline-primary w-100 mb-2"
          ),
          shiny::hr(),
          shiny::uiOutput("cluster_sidebar_detail")
        ),
        shiny::div(
          DT::DTOutput("cluster_table"),
          shiny::br(),
          shiny::uiOutput("cluster_action_bar")
        )
      )
    ),

    # ---- Create tab ---------------------------------------------------------
    bslib::nav_panel(
      "Create",
      bslib::layout_columns(
        col_widths = c(5L, 7L),
        bslib::card(
          bslib::card_header("New cluster"),
          shiny::textInput("new_name",    "Cluster name",    placeholder = "e.g. test-262"),
          shiny::textInput("new_version", "DHIS2 version",   placeholder = "e.g. 2.40.4"),
          shiny::numericInput("new_port", "Port", value = 8081L, min = 1024L, max = 65535L),
          shiny::selectInput("new_channel", "Channel", choices = c("stable", "dev"), selected = "stable"),
          shiny::checkboxInput("new_seed", "Seed database on first start", value = FALSE),
          shiny::conditionalPanel(
            "input.new_seed",
            shiny::fileInput("new_seed_file", "SQL dump (.sql.gz)", accept = c(".gz", ".sql"))
          ),
          shiny::actionButton("btn_create", "Create cluster",
            class = "btn-success mt-2", icon = shiny::icon("plus"))
        ),
        bslib::card(
          bslib::card_header("Match version from a live instance"),
          shiny::textInput("match_url",  "Base URL",  placeholder = "https://dhis2.example.org"),
          shiny::textInput("match_user", "Username (optional)"),
          shiny::passwordInput("match_pass", "Password (optional)"),
          shiny::div(
            class = "d-flex gap-2",
            shiny::actionButton("btn_match_version", "Fetch version",
              class = "btn-outline-secondary btn-sm"),
            shiny::uiOutput("btn_use_version_ui", inline = TRUE)
          ),
          shiny::uiOutput("match_version_result"),
          shiny::hr(),
          bslib::card_header("Saved source instances"),
          shiny::uiOutput("source_instances_ui"),
          shiny::tags$div(
            class = "d-flex gap-2 mt-2",
            shiny::textInput("save_inst_name", NULL, placeholder = "Label", width = "120px"),
            shiny::textInput("save_inst_url",  NULL, placeholder = "URL"),
            shiny::actionButton("btn_save_instance", "Save",
              class = "btn-sm btn-outline-secondary")
          )
        )
      )
    ),

    # ---- Cleanup tab --------------------------------------------------------
    bslib::nav_panel(
      "Cleanup",
      bslib::layout_columns(
        col_widths = c(6L, 6L),
        bslib::card(
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            "Docker disk usage",
            shiny::actionButton("btn_refresh_df", "Refresh",
              class = "btn-sm btn-outline-primary")
          ),
          shiny::pre(shiny::textOutput("docker_df_text", inline = FALSE))
        ),
        shiny::tagList(
          bslib::card(
            class = "border-warning mb-3",
            bslib::card_body(
              shiny::tags$strong("Volume prune"),
              shiny::tags$p(class = "text-muted small mb-1",
                "Removes volumes not attached to any running container.",
                "Stopped clusters' databases will be lost."),
              shiny::uiOutput("stopped_clusters_warn"),
              shiny::actionButton("btn_volume_prune", "Prune volumes",
                class = "btn-warning btn-sm mt-1")
            )
          ),
          bslib::card(
            class = "border-danger",
            bslib::card_body(
              shiny::tags$strong("Image prune"),
              shiny::tags$p(class = "text-muted small mb-1",
                "Removes ALL unused images, including DHIS2 images.",
                "Re-pulling is needed before next cluster start."),
              shiny::actionButton("btn_image_prune", "Prune all images",
                class = "btn-danger btn-sm mt-1")
            )
          )
        )
      )
    ),

    # ---- Environment tab ----------------------------------------------------
    bslib::nav_panel(
      "Environment",
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          "Preflight checks",
          shiny::actionButton("btn_recheck_env", "Re-check",
            class = "btn-sm btn-outline-primary")
        ),
        shiny::uiOutput("env_ui")
      )
    ),

    # ---- Log bar (navbar right) + clear button ------------------------------
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::actionButton("btn_clear_log", "Clear log",
        class = "btn-sm btn-outline-secondary navbar-btn")
    ),

    # Clipboard JS
    shiny::tags$head(shiny::tags$script(shiny::HTML(
      "Shiny.addCustomMessageHandler('cm_copy', function(txt) {
         navigator.clipboard
           ? navigator.clipboard.writeText(txt)
           : (function(){
               var el=document.createElement('textarea');
               el.value=txt; document.body.appendChild(el);
               el.select(); document.execCommand('copy');
               document.body.removeChild(el);
             })();
       });"
    ))),

    # Sticky log pane at the bottom
    shiny::tags$div(
      id = "cm_log_pane",
      style = paste(
        "position:fixed; bottom:0; left:0; right:0; height:130px;",
        "background:#1a1a2e; color:#a8d8a8; font-family:monospace;",
        "font-size:11px; overflow-y:auto; padding:6px 10px;",
        "border-top:2px solid #3a3a5c; z-index:9999;"
      ),
      shiny::verbatimTextOutput("global_log", placeholder = TRUE)
    ),
    shiny::tags$style(
      "body { padding-bottom: 140px !important; }",
      "#global_log { background:transparent; border:none; color:inherit;",
      "  white-space:pre-wrap; margin:0; padding:0; }"
    )
  )
}

# ---- Server -----------------------------------------------------------------

.cm_server <- function(input, output, session) {

  rv <- shiny::reactiveValues(
    clusters       = data.frame(),
    env            = list(),
    log            = character(0),
    active_proc_id = NULL,
    matched_ver    = NULL,
    docker_df      = "",
    config         = .cm_read_config()
  )

  ts_log <- function(...) {
    rv$log <- c(rv$log,
      paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", paste(..., sep = " ")))
  }

  # ---- Environment ----------------------------------------------------------
  do_env_check <- function() {
    ts_log("Running environment checks...")
    docker <- .cm_check_docker()
    d2     <- .cm_check_d2()
    disk_r <- .cm_run("df", c("-h", "."), timeout = 5L)
    disk   <- if (disk_r$status == 0L) trimws(disk_r$stdout) else "unavailable"
    list(docker = docker, d2 = d2, disk = disk)
  }

  shiny::observe({ rv$env <- do_env_check() })
  shiny::observeEvent(input$btn_recheck_env, { rv$env <- do_env_check() })

  output$env_ui <- shiny::renderUI({
    e <- rv$env
    if (!length(e)) return(shiny::p("Checking environment..."))

    # Docker card
    docker_card <- if (isTRUE(e$docker$running)) {
      compose_note <- if (isTRUE(e$docker$compose_v1)) {
        shiny::span(class = "text-success small", shiny::icon("check"), " docker-compose (v1) available")
      } else {
        shiny::tagList(
          shiny::span(class = "text-danger small d-block",
            shiny::icon("triangle-exclamation"),
            " docker-compose (v1) NOT found \u2014 required by d2 cluster."),
          shiny::tags$code(class = "small", "brew install docker-compose")
        )
      }
      bslib::value_box(
        title = "Docker", value = paste0("v", e$docker$version),
        showcase = shiny::icon("circle-check"), theme = "success",
        compose_note
      )
    } else if (isTRUE(e$docker$installed)) {
      bslib::value_box(title = "Docker", value = "Daemon not running",
        showcase = shiny::icon("circle-xmark"), theme = "danger",
        shiny::tags$small("Start Docker Desktop, then click Re-check."))
    } else {
      bslib::value_box(title = "Docker", value = "Not installed",
        showcase = shiny::icon("circle-xmark"), theme = "danger",
        shiny::tags$small(e$docker$msg))
    }

    # d2 card
    d2_card <- if (isTRUE(e$d2$installed)) {
      bslib::value_box(title = "d2 CLI", value = e$d2$version,
        showcase = shiny::icon("circle-check"), theme = "success")
    } else {
      bslib::value_box(title = "d2 CLI", value = "Not installed",
        showcase = shiny::icon("circle-xmark"), theme = "danger",
        shiny::tags$div(
          shiny::tags$code("npm install -g @dhis2/cli"),
          shiny::tags$p(class = "small mt-1 mb-0",
            "Apple Silicon: use an ARM64 Node.js (nvm or Homebrew).",
            "Verify with: node --version; arch")
        ))
    }

    # Seeds card
    seeds_dir <- .nullish(rv$config$seeds_dir, "")
    seeds_card <- bslib::value_box(
      title = "Seeds directory",
      value = if (nzchar(seeds_dir)) "Configured" else "Not set",
      showcase = shiny::icon(if (nzchar(seeds_dir)) "folder-open" else "folder"),
      theme = if (nzchar(seeds_dir)) "secondary" else "light",
      shiny::tags$small(if (nzchar(seeds_dir)) seeds_dir else "Set in config YAML or via the picker below.")
    )

    shiny::tagList(
      bslib::layout_columns(col_widths = c(4L, 4L, 4L), docker_card, d2_card, seeds_card),
      bslib::card(
        bslib::card_header("Local disk space"),
        shiny::pre(class = "small mb-0", e$disk)
      )
    )
  })

  # ---- Clusters -------------------------------------------------------------
  do_load_clusters <- function() {
    tryCatch(
      { rv$clusters <- .cm_list_clusters() },
      error = function(e) ts_log("Error listing clusters:", conditionMessage(e))
    )
  }

  shiny::observe({ do_load_clusters() })
  shiny::observeEvent(input$refresh_clusters, {
    do_load_clusters()
    ts_log("Cluster list refreshed.")
  })

  output$cluster_table <- DT::renderDT({
    df <- rv$clusters
    if (nrow(df) == 0L) {
      return(DT::datatable(
        data.frame(Message = "No clusters found. Use the Create tab to add one."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }
    status_colours <- c(
      Running = "#d4edda", Exited = "#f8d7da",
      Paused  = "#fff3cd", Unknown = "#e2e3e5"
    )
    DT::datatable(
      df,
      selection = "single",
      rownames  = FALSE,
      colnames  = c("Name", "Port", "Channel", "DHIS2 Version", "DB Version", "Status"),
      options   = list(dom = "tp", pageLength = 25L, scrollX = TRUE)
    ) |>
      DT::formatStyle(
        "status",
        backgroundColor = DT::styleEqual(
          names(status_colours), unname(status_colours)
        )
      )
  })

  selected_row <- shiny::reactive({
    sel <- input$cluster_table_rows_selected
    if (is.null(sel) || !length(sel)) return(NULL)
    rv$clusters[sel, , drop = FALSE]
  })

  # Sidebar detail for selected cluster
  output$cluster_sidebar_detail <- shiny::renderUI({
    row <- selected_row()
    if (is.null(row)) {
      return(shiny::p(class = "text-muted small", "Select a row to see actions."))
    }
    is_running <- grepl("running", tolower(.nullish(row$status, "")), ignore.case = TRUE)
    port <- .nullish(row$port, NA_character_)
    url  <- if (!is.na(port) && nzchar(port)) paste0("http://localhost:", port) else NULL

    shiny::tagList(
      shiny::tags$p(shiny::tags$strong(row$name), class = "mb-0"),
      shiny::tags$p(class = "text-muted small mb-2", row$version),
      if (is_running)
        shiny::span(class = "badge bg-success", "Running")
      else
        shiny::span(class = "badge bg-secondary", "Stopped"),
      if (is_running && !is.null(url)) shiny::tagList(
        shiny::hr(),
        shiny::tags$p(class = "small mb-1", shiny::tags$strong("Connect")),
        shiny::tags$code(url), shiny::tags$br(),
        shiny::tags$small(class = "text-muted", "admin / district"),
        shiny::div(
          class = "d-flex flex-column gap-1 mt-2",
          shiny::actionButton("btn_copy_url",  "Copy URL",        class = "btn-sm btn-outline-secondary w-100"),
          shiny::actionButton("btn_test_conn", "Test connection", class = "btn-sm btn-outline-info w-100")
        ),
        shiny::uiOutput("conn_test_result")
      )
    )
  })

  # Action bar below table
  output$cluster_action_bar <- shiny::renderUI({
    row <- selected_row()
    if (is.null(row)) return(NULL)
    is_running <- grepl("running", tolower(.nullish(row$status, "")), ignore.case = TRUE)

    shiny::div(
      class = "d-flex gap-2 flex-wrap mt-2",
      if (!is_running)
        shiny::actionButton("btn_start",   paste("Start",   row$name), class = "btn-success",          icon = shiny::icon("play"))
      else
        shiny::actionButton("btn_stop",    paste("Stop",    row$name), class = "btn-warning",          icon = shiny::icon("stop")),
      shiny::actionButton("btn_restore",   "Restore DB",               class = "btn-outline-secondary", icon = shiny::icon("database")),
      shiny::actionButton("btn_destroy",   "Destroy\u2026",            class = "btn-danger",            icon = shiny::icon("trash"))
    )
  })

  # Start
  shiny::observeEvent(input$btn_start, {
    row <- selected_row(); if (is.null(row)) return()
    ts_log("Starting cluster:", row$name, "(d2 cluster up) — may take several minutes on first run (image pull).")
    id <- paste0("up_", row$name)
    .cm_start_proc(id, "d2", c("cluster", "up", row$name))
    rv$active_proc_id <- id
  })

  # Stop
  shiny::observeEvent(input$btn_stop, {
    row <- selected_row(); if (is.null(row)) return()
    ts_log("Stopping cluster:", row$name)
    id <- paste0("down_", row$name)
    .cm_start_proc(id, "d2", c("cluster", "down", row$name))
    rv$active_proc_id <- id
  })

  # Restore — modal
  shiny::observeEvent(input$btn_restore, {
    row <- selected_row(); if (is.null(row)) return()

    seeds_dir  <- .nullish(rv$config$seeds_dir, "")
    seed_files <- if (nzchar(seeds_dir) && dir.exists(seeds_dir))
      list.files(seeds_dir, pattern = "\\.sql(\\.gz)?$", full.names = FALSE)
    else
      character(0)

    shiny::showModal(shiny::modalDialog(
      title = shiny::tagList(shiny::icon("triangle-exclamation"), " Restore database"),
      shiny::p(
        shiny::tags$strong("Warning:"), "This destroys the current DB state for",
        shiny::tags$code(row$name), "and replaces it with the selected dump."
      ),
      shiny::fileInput("restore_file_input", "Upload SQL dump (.sql.gz)",
        accept = c(".gz", ".sql")),
      if (length(seed_files)) shiny::selectInput(
        "restore_seed_choice", "Or pick from seeds directory:",
        choices = c("\u2014 use uploaded file \u2014", seed_files)
      ),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("btn_restore_go", "Restore", class = "btn-danger")
      ),
      fade = FALSE
    ))
  })

  shiny::observeEvent(input$btn_restore_go, {
    row <- selected_row()
    if (is.null(row)) { shiny::removeModal(); return() }

    file_path <- NULL
    seed_choice <- input$restore_seed_choice
    upload      <- input$restore_file_input

    if (!is.null(seed_choice) &&
        !grepl("^\u2014", seed_choice) &&
        nzchar(seed_choice)) {
      file_path <- file.path(.nullish(rv$config$seeds_dir, ""), seed_choice)
    } else if (!is.null(upload)) {
      file_path <- upload$datapath
    }

    if (is.null(file_path) || !file.exists(file_path)) {
      shiny::showNotification("No valid file selected.", type = "warning")
      return()
    }
    shiny::removeModal()
    ts_log("Restoring DB for", row$name, "from", basename(file_path))
    id <- paste0("restore_", row$name)
    .cm_start_proc(id, "d2", c("cluster", "db", "restore", row$name, file_path))
    rv$active_proc_id <- id
  })

  # Destroy — modal with name-confirmation
  shiny::observeEvent(input$btn_destroy, {
    row <- selected_row(); if (is.null(row)) return()
    shiny::showModal(shiny::modalDialog(
      title = shiny::tagList(shiny::icon("skull"), " Destroy cluster"),
      shiny::p(
        shiny::tags$strong("This permanently destroys"), shiny::tags$code(row$name),
        "and all associated volumes."
      ),
      shiny::p("Note: uses", shiny::tags$code("d2 cluster down --clean"),
        "(there is no", shiny::tags$code("d2 cluster destroy"), "command)."),
      shiny::textInput("destroy_name_confirm", "Type the cluster name to confirm:",
        placeholder = row$name),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("btn_destroy_go", "Destroy permanently", class = "btn-danger")
      ),
      fade = FALSE
    ))
  })

  shiny::observeEvent(input$btn_destroy_go, {
    row <- selected_row()
    if (is.null(row)) { shiny::removeModal(); return() }
    if (!identical(trimws(input$destroy_name_confirm), row$name)) {
      shiny::showNotification("Name does not match. Cancelled.", type = "warning")
      return()
    }
    shiny::removeModal()
    ts_log("Destroying cluster:", row$name, "(d2 cluster down --clean)")
    id <- paste0("destroy_", row$name)
    .cm_start_proc(id, "d2", c("cluster", "down", "--clean", row$name))
    rv$active_proc_id <- id
  })

  # Copy URL
  shiny::observeEvent(input$btn_copy_url, {
    row <- selected_row(); if (is.null(row)) return()
    port <- .nullish(row$port, NA_character_)
    if (is.na(port) || !nzchar(port)) return()
    url <- paste0("http://localhost:", port)
    session$sendCustomMessage("cm_copy", url)
    shiny::showNotification(paste("Copied:", url), type = "message", duration = 2L)
  })

  # Test connection
  shiny::observeEvent(input$btn_test_conn, {
    row <- selected_row(); if (is.null(row)) return()
    port <- .nullish(row$port, NA_character_)
    if (is.na(port) || !nzchar(port)) return()
    url <- paste0("http://localhost:", port)
    ts_log("Testing connection to", url, "...")
    res <- .cm_fetch_instance_version(url, "admin", "district")
    output$conn_test_result <- shiny::renderUI({
      if (isTRUE(res$ok))
        shiny::div(class = "text-success small mt-1",
          shiny::icon("circle-check"), " DHIS2", res$version)
      else
        shiny::div(class = "text-danger small mt-1",
          shiny::icon("circle-xmark"), " ", res$msg)
    })
  })

  # ---- Create ---------------------------------------------------------------
  shiny::observe({
    shiny::updateNumericInput(session, "new_port", value = .cm_suggest_port())
  })

  shiny::observeEvent(input$btn_match_version, {
    url <- trimws(input$match_url)
    if (!nzchar(url)) {
      shiny::showNotification("Enter a URL first.", type = "warning")
      return()
    }
    user <- if (nzchar(trimws(.nullish(input$match_user, "")))) input$match_user else NULL
    pass <- if (!is.null(user)) input$match_pass else NULL
    ts_log("Fetching version from", url)
    res <- .cm_fetch_instance_version(url, user, pass)
    if (isTRUE(res$ok)) {
      rv$matched_ver <- res$version
      ts_log("Remote version:", res$version)
    } else {
      rv$matched_ver <- NULL
      ts_log("Version fetch failed:", res$msg)
    }
    output$match_version_result <- shiny::renderUI({
      if (isTRUE(res$ok))
        shiny::div(class = "text-success small mt-1",
          shiny::icon("circle-check"), " Detected version:", shiny::tags$strong(res$version))
      else
        shiny::div(class = "text-danger small mt-1",
          shiny::icon("circle-xmark"), " ", res$msg)
    })
  })

  output$btn_use_version_ui <- shiny::renderUI({
    if (is.null(rv$matched_ver)) return(NULL)
    shiny::actionButton("btn_use_version",
      paste("Use", rv$matched_ver),
      class = "btn-sm btn-outline-success")
  })

  shiny::observeEvent(input$btn_use_version, {
    if (!is.null(rv$matched_ver))
      shiny::updateTextInput(session, "new_version", value = rv$matched_ver)
  })

  # Saved source instances
  output$source_instances_ui <- shiny::renderUI({
    insts <- .nullish(rv$config$source_instances, list())
    if (!length(insts))
      return(shiny::p(class = "text-muted small", "No saved instances."))
    lapply(seq_along(insts), function(i) {
      inst <- insts[[i]]
      shiny::div(
        class = "d-flex align-items-center gap-2 mb-1",
        shiny::actionButton(paste0("load_inst_", i),
          .nullish(inst$name, inst$url),
          class = "btn-sm btn-outline-secondary"),
        shiny::tags$small(class = "text-muted", inst$url)
      )
    })
  })

  shiny::observeEvent(input$btn_save_instance, {
    name <- trimws(.nullish(input$save_inst_name, ""))
    url  <- trimws(.nullish(input$save_inst_url,  ""))
    if (!nzchar(url)) {
      shiny::showNotification("URL is required.", type = "warning")
      return()
    }
    cfg <- rv$config
    cfg$source_instances <- c(
      .nullish(cfg$source_instances, list()),
      list(list(name = name, url = url))
    )
    .cm_write_config(cfg)
    rv$config <- cfg
    shiny::updateTextInput(session, "save_inst_name", value = "")
    shiny::updateTextInput(session, "save_inst_url",  value = "")
  })

  # Dynamically wire load_inst_N buttons (up to 30 saved instances)
  for (idx in seq_len(30L)) {
    local({
      i <- idx
      shiny::observeEvent(input[[paste0("load_inst_", i)]], {
        insts <- .nullish(rv$config$source_instances, list())
        if (i <= length(insts))
          shiny::updateTextInput(session, "match_url", value = .nullish(insts[[i]]$url, ""))
      }, ignoreInit = TRUE)
    })
  }

  shiny::observeEvent(input$btn_create, {
    name    <- trimws(.nullish(input$new_name,    ""))
    version <- trimws(.nullish(input$new_version, ""))
    port    <- input$new_port
    channel <- input$new_channel

    if (!nzchar(name))    { shiny::showNotification("Cluster name is required.", type = "warning"); return() }
    if (!nzchar(version)) { shiny::showNotification("DHIS2 version is required.", type = "warning"); return() }

    args <- c("cluster", "up", name,
      "--dhis2Version", version,
      "--port",         as.character(port),
      "--channel",      channel)

    if (isTRUE(input$new_seed) && !is.null(input$new_seed_file))
      args <- c(args, "--seedFile", input$new_seed_file$datapath)

    ts_log("Creating cluster:", name, "| version:", version, "| port:", port,
      "| (first run may take several minutes for image pull)")
    .cm_start_proc(paste0("create_", name), "d2", args)
    rv$active_proc_id <- paste0("create_", name)
    shiny::showNotification(
      paste0("Cluster creation started for '", name, "' \u2014 see log pane."),
      type = "message", duration = 6L
    )
  })

  # ---- Cleanup --------------------------------------------------------------
  do_refresh_df <- function() {
    rv$docker_df <- .cm_docker_df()
  }

  shiny::observe({ do_refresh_df() })
  shiny::observeEvent(input$btn_refresh_df, { do_refresh_df() })

  output$docker_df_text <- shiny::renderText({ rv$docker_df })

  output$stopped_clusters_warn <- shiny::renderUI({
    df <- rv$clusters
    if (!nrow(df)) return(NULL)
    stopped <- df$name[!grepl("running", tolower(df$status), ignore.case = TRUE)]
    if (!length(stopped)) return(NULL)
    shiny::div(
      class = "alert alert-warning small py-1 mb-2",
      shiny::tags$strong("Stopped clusters (volumes at risk):"),
      shiny::tags$ul(class = "mb-0", lapply(stopped, shiny::tags$li))
    )
  })

  shiny::observeEvent(input$btn_volume_prune, {
    shiny::showModal(shiny::modalDialog(
      title = "Prune volumes?",
      shiny::p("Removes all Docker volumes not in use. Stopped cluster databases will be permanently deleted."),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("btn_volume_prune_go", "Yes, prune volumes", class = "btn-warning")
      ), fade = FALSE
    ))
  })
  shiny::observeEvent(input$btn_volume_prune_go, {
    shiny::removeModal()
    ts_log("Running: docker volume prune -f")
    r <- .cm_run("docker", c("volume", "prune", "-f"), timeout = 60L)
    ts_log(if (r$status == 0L) trimws(r$stdout) else paste("Error:", trimws(r$stderr)))
    do_refresh_df()
  })

  shiny::observeEvent(input$btn_image_prune, {
    shiny::showModal(shiny::modalDialog(
      title = "Prune all images?",
      shiny::p("Removes ALL unused Docker images. DHIS2 images will need to be re-downloaded before next cluster start."),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("btn_image_prune_go", "Yes, prune images", class = "btn-danger")
      ), fade = FALSE
    ))
  })
  shiny::observeEvent(input$btn_image_prune_go, {
    shiny::removeModal()
    ts_log("Running: docker image prune -a -f")
    r <- .cm_run("docker", c("image", "prune", "-a", "-f"), timeout = 120L)
    ts_log(if (r$status == 0L) trimws(r$stdout) else paste("Error:", trimws(r$stderr)))
    do_refresh_df()
  })

  # ---- Background process polling -------------------------------------------
  shiny::observe({
    id <- rv$active_proc_id
    if (is.null(id)) return()
    shiny::invalidateLater(500L, session)

    state <- .cm_poll_proc(id)
    for (l in state$new_lines) ts_log(l)

    if (!isTRUE(state$alive)) {
      code <- .nullish(state$exit_status, NA_integer_)
      if (!is.na(code) && code == 0L) {
        ts_log("Process finished successfully.")
      } else {
        ts_log("Process finished with exit code:", .nullish(code, "?"))
      }
      rv$active_proc_id <- NULL
      do_load_clusters()
      do_refresh_df()
    }
  })

  # ---- Log pane -------------------------------------------------------------
  output$global_log <- shiny::renderText({
    paste(tail(rv$log, 200L), collapse = "\n")
  })

  shiny::observeEvent(input$btn_clear_log, { rv$log <- character(0) })
}

# ---- Exported launcher ------------------------------------------------------

#' Launch the DHIS2 Cluster Manager gadget
#'
#' A developer tool for managing local DHIS2 d2 cluster instances used for
#' integration testing.  Provides a GUI for creating, starting, stopping,
#' restoring, and destroying d2 clusters, with environment preflight checks
#' and Docker disk-usage management.
#'
#' @section Prerequisites:
#' - **Docker Desktop** (>= 4.x) with the Compose v2 plugin
#'   (`docker compose version` should succeed).
#' - **d2 CLI**: `npm install -g @@dhis2/cli`
#'   On Apple Silicon, ensure Node.js is ARM64 (install via `nvm` or Homebrew).
#'
#' @section Typical workflow:
#' 1. **Create** a cluster once per source instance.  Use the "Match version"
#'    helper to pull the exact version from a live URL so you match prod.
#' 2. **Start / Stop** the cluster between test sessions (`d2 cluster up/down`).
#' 3. **Restore** to reset the database to a known SQL dump state.
#' 4. **Destroy** (`d2 cluster down --clean`) only when the cluster is no
#'    longer needed — volumes are deleted and cannot be recovered.
#'
#' @section Configuration:
#' Lightweight config (seeds directory, saved source-instance URLs) is stored
#' in `rappdirs::user_config_dir("mg2-cluster")/config.json` and persists
#' across sessions.
#'
#' @section Non-interactive flags:
#' `d2 cluster up/down` can prompt interactively in some error conditions.
#' Pass `--quiet` if you encounter hanging prompts; this is logged in the
#' log pane so you can diagnose the issue.
#'
#' @param port Port for the gadget's own Shiny server (`0L` = OS-assigned).
#' @param launch.browser Passed to [shiny::runApp()].  `TRUE` opens a system
#'   browser window; use `getOption("shiny.launch.browser")` default.
#' @return Called for its side effect (launches a Shiny application).
#' @export
manage_clusters <- function(
    port           = 0L,
    launch.browser = getOption("shiny.launch.browser", interactive())) {

  pkgs <- c("processx", "rappdirs", "httr2", "DT", "bslib", "jsonlite")
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing_pkgs)) {
    stop(
      "manage_clusters() requires these packages: ",
      paste(missing_pkgs, collapse = ", "),
      "\nInstall with: install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "), "))",
      call. = FALSE
    )
  }

  app <- shiny::shinyApp(ui = .cm_ui(), server = .cm_server)
  shiny::runApp(app, port = port, launch.browser = launch.browser)
}
