# output_widget.R — Tab: Output
#
# Four-step pipeline for writing MG2 outputs to a local DHIS2 instance:
#
#   Step 1 — Source info     : read from loaded metadata (no connection needed)
#   Step 2 — Local instance  : create a d2 cluster matching the source version
#   Step 3 — Import metadata : org units + formula data elements + MG2 outputs
#   Step 4 — Write values    : raw data and/or model outputs
#
# Data elements in step 3 are derived entirely from the currently loaded
# formula — they are never hardcoded.

# ---- Helpers ----------------------------------------------------------------

.out_step_card <- function(number, title, status_id, body_id, ns) {
  # Wrapper card with a coloured left border driven by a server-side CSS class.
  shiny::div(
    id    = ns(paste0("step_card_", number)),
    class = "card mb-3",
    style = "border-left: 4px solid #ced4da;",   # overridden by JS/server
    shiny::div(
      class = "card-header d-flex justify-content-between align-items-center py-2",
      shiny::div(
        shiny::span(
          number,
          class = "badge bg-secondary me-2",
          style = "font-size:0.85rem; width:1.6rem;"
        ),
        shiny::tags$strong(title)
      ),
      shiny::uiOutput(ns(status_id))
    ),
    shiny::div(class = "card-body py-3", shiny::uiOutput(ns(body_id)))
  )
}

.out_status_badge <- function(state) {
  switch(state,
    pending  = shiny::span(class = "badge bg-secondary",  "Pending"),
    running  = shiny::span(class = "badge bg-primary",    shiny::icon("spinner", class = "fa-spin"), " Running"),
    complete = shiny::span(class = "badge bg-success",    shiny::icon("circle-check"),    " Done"),
    error    = shiny::span(class = "badge bg-danger",     shiny::icon("circle-xmark"),    " Error"),
    shiny::span(class = "badge bg-light text-dark", state)
  )
}

# ---- UI ---------------------------------------------------------------------

output_widget_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    style = "overflow-y: auto; height: 100%;",
  shiny::div(
    style = "max-width:900px; margin:20px auto; padding:0 16px 40px;",
    # Force the log pre element to use the dark-pane colours, overriding
    # Bootstrap's default light pre background.
    shiny::tags$style(sprintf(
      "#%s { background: transparent !important; color: #a8d8a8 !important;
              border: none !important; margin: 0; padding: 0;
              font-size: 11px; white-space: pre-wrap; }",
      ns("cluster_log_out")
    )),

    shiny::div(
      class = "alert alert-warning d-flex align-items-start gap-2 mb-3",
      shiny::icon("triangle-exclamation"),
      shiny::div(
        shiny::tags$strong("Under development."),
        " Steps 1\u20133 are functional. Step 4 (model outputs) is a placeholder."
      )
    ),

    # Step 1 ----------------------------------------------------------------
    .out_step_card(1L, "Source instance", "s1_badge", "s1_body", ns),

    # Step 2 ----------------------------------------------------------------
    .out_step_card(2L, "Local DHIS2 instance", "s2_badge", "s2_body", ns),

    # Step 3 ----------------------------------------------------------------
    .out_step_card(3L, "Import metadata", "s3_badge", "s3_body", ns),

    # Step 4 ----------------------------------------------------------------
    .out_step_card(4L, "Write data values", "s4_badge", "s4_body", ns)
  )  # inner max-width div
  )  # outer scroll wrapper
}

# ---- Server -----------------------------------------------------------------

output_widget_server <- function(id,
                                  metadata_widget_output   = NULL,
                                  data_widget_output       = NULL,
                                  evaluation_widget_output = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      s1 = "pending", s2 = "pending", s3 = "pending", s4 = "pending",
      target_url      = NULL,   # confirmed local instance URL
      instance_meta   = NULL,   # result of mg2_check_instance_metadata()
      cluster_proc    = NULL,   # processx process for d2 cluster up
      cluster_log     = character(0),
      cluster_error   = NULL,   # structured error: list(title, body, fix) or NULL
      startup_url     = NULL,   # URL being polled while DHIS2 initialises
      startup_attempt = 0L,     # retry counter
      import_result   = NULL,
      write_result    = NULL
    )

    # ── Shared helpers ──────────────────────────────────────────────────────

    # Note: do NOT wrap these in tryCatch — req() failures must bubble up
    # so Shiny can suppress dependent outputs correctly. A bare tryCatch
    # catches req() and returns NULL, making data appear unloaded.
    sys_info <- shiny::reactive({
      if (is.null(metadata_widget_output)) return(NULL)
      metadata_widget_output$systemInfo()
    })

    org_units <- shiny::reactive({
      if (is.null(metadata_widget_output)) return(NULL)
      metadata_widget_output$orgUnits()
    })

    raw_data <- shiny::reactive({
      if (is.null(data_widget_output)) return(NULL)
      data_widget_output$data1()
    })

    .sysval <- function(attr) {
      info <- sys_info()
      if (is.null(info)) return(NULL)
      rows <- info[tolower(info$Attribute) == tolower(attr), , drop = FALSE]
      if (!nrow(rows)) return(NULL)
      trimws(as.character(rows$Value[[1L]]))
    }

    # ── Step 1: Source info ─────────────────────────────────────────────────

    shiny::observe({
      info <- sys_info()
      rv$s1 <- if (!is.null(info) && is.data.frame(info) && nrow(info)) "complete" else "pending"
    })

    output$s1_badge <- shiny::renderUI(.out_status_badge(rv$s1))

    output$s1_body <- shiny::renderUI({
      info <- sys_info()
      if (is.null(info) || !nrow(info)) {
        return(shiny::div(
          class = "text-muted",
          shiny::icon("circle-info"), " No metadata loaded.",
          " Go to the ", shiny::tags$strong("Metadata"), " tab and load a metadata file."
        ))
      }
      ous <- org_units()
      dat <- raw_data()

      version  <- .sysval("version")   %||% "\u2014"
      sysname  <- .sysval("systemName") %||% "\u2014"
      ctx      <- .sysval("contextPath") %||% .sysval("instanceBaseUrl") %||% "\u2014"
      n_ou     <- if (!is.null(ous) && nrow(ous) > 0L) nrow(ous) else "\u2014"
      n_de     <- if (!is.null(dat) && "data.id" %in% names(dat)) {
                    uids <- sub("_.*$", "", dat$data.id)
                    length(unique(uids[!is.na(uids) & nzchar(uids)]))
                  } else "\u2014"

      shiny::tags$table(
        class = "table table-sm table-borderless mb-0",
        shiny::tags$tbody(
          .out_row("System",       sysname),
          .out_row("Version",      shiny::tags$strong(version)),
          .out_row("URL",          shiny::tags$code(ctx)),
          .out_row("Org units",    n_ou),
          .out_row("Data elements (loaded formula)", n_de)
        )
      )
    })

    # ── Step 2: Local instance ──────────────────────────────────────────────

    output$s2_badge <- shiny::renderUI(.out_status_badge(rv$s2))

    # Suggest a cluster name from source system name
    suggested_name <- shiny::reactive({
      nm  <- .sysval("systemName")
      ver <- .sysval("version")
      # Abbreviate version: "2.40.9" -> "240" (drop patch if zero, keep if not)
      ver_slug <- if (!is.null(ver)) {
        parts <- strsplit(ver, "\\.")[[1L]]
        paste0(parts[1L], parts[2L],
               if (length(parts) >= 3L && parts[3L] != "0") parts[3L] else "")
      } else NULL
      base <- if (!is.null(nm))
        gsub("[^a-z0-9]+", "-", tolower(nm)) |> gsub("-+$", "", x = _) |> substr(1L, 18L)
      else "mg2"
      paste0(base, if (!is.null(ver_slug)) paste0("-", ver_slug), "-out")
    })

    output$s2_body <- shiny::renderUI({
      ver <- .sysval("version")

      shiny::tagList(
        # d2 create section
        bslib::card(
          class = "mb-3",
          bslib::card_body(
            class = "pb-2",
            shiny::tags$p(class = "small text-muted mb-2",
              "Create a new local d2 cluster at the matching DHIS2 version.",
              if (!is.null(ver)) shiny::span(class = "fw-semibold", paste0(" (", ver, ")"))
            ),
            shiny::fluidRow(
              shiny::column(6,
                shiny::textInput(ns("cluster_name"), "Cluster name",
                  value = suggested_name(), placeholder = "mg2-output")
              ),
              shiny::column(3,
                shiny::numericInput(ns("cluster_port"), "Port",
                  value = 8090L, min = 1024L, max = 65535L)
              ),
              shiny::column(3,
                shiny::div(style = "margin-top:1.9rem;",
                  shiny::actionButton(ns("btn_create_cluster"), "Create",
                    icon = shiny::icon("play"), class = "btn-success w-100")
                )
              )
            ),
            # Log pane — only shown once cluster creation starts
            shiny::conditionalPanel(
              condition = sprintf("output['%s']", ns("show_log")),
              shiny::div(
                style = paste(
                  "background:#1a1a2e; color:#a8d8a8; font-family:monospace;",
                  "font-size:11px; border-radius:4px; padding:8px;",
                  "height:200px; overflow-y:auto; margin-top:8px;"
                ),
                shiny::verbatimTextOutput(ns("cluster_log_out"), placeholder = FALSE)
              ),
              # Structured error alert — shown separately below the raw log
              shiny::uiOutput(ns("cluster_error_ui"))
            )
          )
        ),
        # Manual URL override
        bslib::card(
          bslib::card_body(
            class = "py-2",
            shiny::tags$p(class = "small text-muted mb-2",
              "Already have a running instance? Enter its URL directly:"),
            shiny::fluidRow(
              shiny::column(8,
                shiny::textInput(ns("manual_url"), NULL,
                  placeholder = "http://localhost:8090", width = "100%")
              ),
              shiny::column(4,
                shiny::actionButton(ns("btn_test_url"), "Test & use",
                  class = "btn-outline-primary btn-sm w-100", style = "margin-top:0;")
              )
            ),
            shiny::uiOutput(ns("manual_url_status"))
          )
        ),
        # Confirmed URL display
        shiny::uiOutput(ns("confirmed_instance_ui"))
      )
    })

    # Create cluster via d2
    shiny::observeEvent(input$btn_create_cluster, {
      name <- trimws(.nullish(input$cluster_name, ""))
      port <- input$cluster_port
      ver  <- .sysval("version")

      if (!nzchar(name)) {
        shiny::showNotification("Enter a cluster name.", type = "warning")
        return()
      }

      # Pre-flight Docker. `d2 cluster up` shells out to docker-compose, so
      # without a running daemon it emits ~20 lines of compose output ending in
      # "exit code: 1" and never names the actual problem. Diagnose it here.
      dchk <- MG2::mg2_check_docker()
      .no_docker_fix <- paste(
        "Or skip Step 2 entirely: use \"Already have a running instance?\"",
        "below to point at an existing DHIS2."
      )
      if (!isTRUE(dchk$installed) || !isTRUE(dchk$running) ||
          !isTRUE(dchk$compose_v1)) {
        rv$s2          <- "error"
        rv$cluster_log <- character(0)
        rv$target_url  <- NULL
        rv$cluster_error <-
          if (!isTRUE(dchk$installed)) list(
            title = "Docker is not available",
            body  = .nullish(dchk$msg, "No docker binary was found on PATH."),
            fix   = paste(
              "Install Docker Desktop:",
              "  https://www.docker.com/products/docker-desktop/",
              "Then also install the v1 compose binary, which d2 requires:",
              "  brew install docker-compose",
              .no_docker_fix, sep = "\n")
          ) else if (!isTRUE(dchk$running)) list(
            title = "Docker is installed but not running",
            body  = .nullish(dchk$msg, "The Docker daemon did not respond."),
            fix   = paste(
              "Start Docker Desktop, wait for it to report Running, then retry.",
              .no_docker_fix, sep = "\n")
          ) else list(
            title = "docker-compose (v1) not found",
            body  = .nullish(
              dchk$msg,
              paste("Docker is running, but d2 cluster calls the standalone",
                    "docker-compose binary, not Docker Desktop's built-in",
                    "`docker compose` plugin.")),
            fix   = paste("  brew install docker-compose", .no_docker_fix, sep = "\n")
          )
        cat("\n[Output tab] Docker pre-flight failed \u2014",
            rv$cluster_error$title, "\n")
        return()
      }

      rv$s2            <- "running"
      rv$cluster_log   <- character(0)
      rv$cluster_error <- NULL
      rv$target_url    <- NULL

      args <- c("cluster", "up", name, "--port", as.character(port))
      if (!is.null(ver)) args <- c(args, "--dhis2Version", ver)

      rv$cluster_proc <- tryCatch(
        processx::process$new("d2", args, stdout = "|", stderr = "2>&1", cleanup = TRUE),
        error = function(e) {
          msg <- conditionMessage(e)
          err <- list(
            title = "Could not start d2",
            body  = msg,
            fix   = if (grepl("ENOENT|not found", msg, ignore.case = TRUE))
              "npm install -g @dhis2/cli\nOn Apple Silicon: use an ARM64 Node.js (nvm or Homebrew)."
            else
              "Check that d2 is installed and on your PATH:\n  which d2\n  d2 --version"
          )
          rv$cluster_error <- err
          rv$cluster_log   <- c(rv$cluster_log, paste("[error]", msg))
          rv$s2            <- "error"
          cat("\n[Output tab] ERROR \u2014", err$title, "\n ", msg, "\n  Fix:", err$fix, "\n")
          NULL
        }
      )
    }, ignoreInit = TRUE)

    # Poll cluster process
    shiny::observe({
      if (is.null(rv$cluster_proc)) return()
      shiny::invalidateLater(600L, session)

      new_lines <- tryCatch(rv$cluster_proc$read_output_lines(), error = function(e) character(0))
      if (length(new_lines)) rv$cluster_log <- c(rv$cluster_log, new_lines)

      if (!rv$cluster_proc$is_alive()) {
        code <- tryCatch(rv$cluster_proc$get_exit_status(), error = function(e) NA_integer_)
        rv$cluster_proc <- NULL
        if (!is.na(code) && code == 0L) {
          port <- input$cluster_port
          url  <- paste0("http://localhost:", port)
          rv$cluster_log <- c(rv$cluster_log,
            paste0("[done] Cluster started. Testing ", url, " ..."))
          test <- mg2_test_dhis2_connection(url)
          if (isTRUE(test$ok)) {
            rv$target_url  <- url
            rv$s2          <- "complete"
            rv$cluster_log <- c(rv$cluster_log, paste("[ok] Connected \u2014 DHIS2", test$version))
            cat("\n[Output tab] d2 cluster ready:", url, "\n")
          } else {
            # 502/503 = gateway up but DHIS2 still initialising — start polling
            rv$cluster_log   <- c(rv$cluster_log,
              paste0("[wait] DHIS2 initialising (", test$message,
                     ") \u2014 will retry every 15 s for up to 5 min..."))
            rv$startup_url     <- url
            rv$startup_attempt <- 1L
            rv$s2              <- "running"
          }
        } else {
          rv$s2    <- "error"
          full_log <- paste(rv$cluster_log, collapse = "\n")

          err <- if (grepl("docker-compose.*not found|docker-compose: command", full_log,
                           ignore.case = TRUE)) {
            list(
              title = "docker-compose not found",
              body  = paste0(
                "d2 cluster calls the standalone docker-compose v1 binary, which is ",
                "separate from Docker Compose v2 (the \u2018docker compose\u2019 plugin ",
                "bundled with Docker Desktop)."
              ),
              fix = paste0(
                "Install the v1 binary:\n",
                "  brew install docker-compose\n\n",
                "Then retry, or enter the URL of an already-running instance below."
              )
            )
          } else if (grepl("d2.*not found|d2: command", full_log, ignore.case = TRUE) ||
                     grepl("ENOENT|spawn d2", full_log)) {
            list(
              title = "d2 CLI not found",
              body  = "The d2 command was not found in PATH.",
              fix   = "npm install -g @dhis2/cli\nOn Apple Silicon: use an ARM64 Node.js (nvm or Homebrew)."
            )
          } else if (grepl("Cannot connect|connection refused", full_log, ignore.case = TRUE)) {
            list(
              title = "Docker daemon not running",
              body  = "d2 cluster could not connect to Docker.",
              fix   = "Start Docker Desktop, then retry."
            )
          } else {
            list(
              title = paste("Cluster creation failed (exit code:", code, ")"),
              body  = "See the log above for details.",
              fix   = "Check the full log output. You can also enter an existing instance URL below."
            )
          }

          rv$cluster_error <- err
          rv$cluster_log   <- c(rv$cluster_log, paste("[error]", err$title))
          cat(
            "\n[Output tab] ERROR \u2014", err$title,
            "\n  ", err$body,
            "\n  Fix:", err$fix, "\n"
          )
        }
      }
    })

    # Poll DHIS2 startup after containers are up (502 = still initialising)
    shiny::observe({
      url <- rv$startup_url
      if (is.null(url)) return()
      shiny::invalidateLater(15000L, session)   # retry every 15 s

      max_attempts <- 20L   # 20 × 15 s = 5 min
      attempt      <- rv$startup_attempt
      if (attempt > max_attempts) {
        rv$startup_url   <- NULL
        rv$s2            <- "error"
        rv$cluster_error <- list(
          title = "DHIS2 did not start within 5 minutes",
          body  = paste("Cluster containers are running but", url, "is still not responding."),
          fix   = paste0("Check Docker logs: docker logs d2-cluster-nigeria_mg2-core-1\n",
                         "Or enter the URL manually below once it comes up.")
        )
        rv$cluster_log <- c(rv$cluster_log, "[error] Startup timed out after 5 min.")
        return()
      }

      test <- mg2_test_dhis2_connection(url)
      if (isTRUE(test$ok)) {
        rv$startup_url <- NULL
        rv$target_url  <- url
        rv$s2          <- "complete"
        rv$cluster_log <- c(rv$cluster_log,
          paste0("[ok] Connected after ", attempt, " attempts \u2014 DHIS2 ", test$version))
        cat("\n[Output tab] DHIS2 ready after", attempt, "attempts:", url, "\n")
      } else {
        rv$startup_attempt <- attempt + 1L
        rv$cluster_log <- c(rv$cluster_log,
          paste0("[wait] Attempt ", attempt, "/", max_attempts,
                 " \u2014 ", test$message))
      }
    })

    output$show_log <- shiny::reactive({ length(rv$cluster_log) > 0L })
    shiny::outputOptions(output, "show_log", suspendWhenHidden = FALSE)

    output$cluster_log_out <- shiny::renderText({
      paste(tail(rv$cluster_log, 200L), collapse = "\n")
    })

    output$cluster_error_ui <- shiny::renderUI({
      err <- rv$cluster_error
      if (is.null(err)) return(NULL)
      shiny::div(
        class = "alert alert-danger mt-2 mb-0",
        shiny::tags$strong(shiny::icon("circle-xmark"), " ", err$title),
        shiny::tags$p(class = "mb-2 mt-1 small", err$body),
        shiny::tags$strong(class = "small", "Fix:"),
        shiny::tags$pre(
          class = "mb-0 mt-1 small",
          style = "background:#f8d7da; border:none; padding:6px 10px;",
          err$fix
        )
      )
    })

    # Test manual URL
    manual_test_result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$btn_test_url, {
      url <- trimws(.nullish(input$manual_url, ""))
      if (!nzchar(url)) {
        shiny::showNotification("Enter a URL first.", type = "warning")
        return()
      }
      manual_test_result(list(testing = TRUE, url = url))
      test <- mg2_test_dhis2_connection(url)
      if (isTRUE(test$ok)) {
        rv$target_url <- url
        rv$s2         <- "complete"
      } else {
        # Don't flip to "error" for 502/503 — those just mean still initialising
        still_starting <- grepl("502|503|Gateway|Bad Gateway", test$message, ignore.case = TRUE) ||
                          grepl("timed out|timeout|connection refused", test$message, ignore.case = TRUE)
        rv$s2 <- if (still_starting) rv$s2 else "error"   # keep current state if still starting
      }
      manual_test_result(list(testing = FALSE, ok = test$ok,
                              version = test$version, message = test$message))
    })

    output$manual_url_status <- shiny::renderUI({
      res <- manual_test_result()
      if (is.null(res)) return(NULL)
      if (isTRUE(res$testing))
        return(shiny::div(class = "text-muted small mt-1",
          shiny::icon("spinner", class = "fa-spin"), " Testing..."))
      if (isTRUE(res$ok))
        shiny::div(class = "text-success small mt-1",
          shiny::icon("circle-check"), " DHIS2 ", res$version, " \u2014 connected")
      else {
        still_starting <- grepl("502|503|Gateway|Bad Gateway", res$message, ignore.case = TRUE) ||
                          grepl("timed out|timeout|connection refused", res$message, ignore.case = TRUE)
        if (still_starting)
          shiny::div(class = "text-warning small mt-1",
            shiny::icon("hourglass-half"), " DHIS2 still initialising (", res$message,
            ") \u2014 wait ~1 min and retry")
        else
          shiny::div(class = "text-danger small mt-1",
            shiny::icon("circle-xmark"), " ", res$message)
      }
    })

    output$confirmed_instance_ui <- shiny::renderUI({
      url <- rv$target_url
      if (is.null(url)) return(NULL)
      shiny::div(
        class = "alert alert-success d-flex align-items-center justify-content-between py-2 mt-2 mb-0",
        shiny::div(
          class = "d-flex align-items-center gap-2",
          shiny::icon("circle-check"),
          "Target: ", shiny::tags$strong(url), " \u2014 admin / district"
        ),
        shiny::tags$a(
          href   = url,
          target = "_blank",
          class  = "btn btn-sm btn-outline-success",
          shiny::icon("arrow-up-right-from-square"), " Open"
        )
      )
    })

    # ── Step 3: Import metadata ─────────────────────────────────────────────

    # Check what metadata is already in the instance whenever target_url changes
    shiny::observeEvent(rv$target_url, {
      url <- rv$target_url
      if (is.null(url)) { rv$instance_meta <- NULL; return() }
      rv$instance_meta <- mg2_check_instance_metadata(url)
      cat("[Output tab] Instance check:", rv$instance_meta$message, "\n")
    }, ignoreNULL = FALSE)

    output$s3_badge <- shiny::renderUI(.out_status_badge(rv$s3))

    # Summarise what will be imported
    import_summary <- shiny::reactive({
      ous  <- org_units()
      dat  <- raw_data()
      n_ou <- if (!is.null(ous) && nrow(ous)) nrow(ous) else 0L
      n_de_source <- if (!is.null(dat) && "data.id" %in% names(dat)) {
        uids <- sub("_.*$", "", dat$data.id)
        length(unique(uids[!is.na(uids) & nzchar(uids)]))
      } else 0L
      list(n_ou = n_ou, n_de_source = n_de_source, n_de_output = 4L)
    })

    output$s3_body <- shiny::renderUI({
      url <- rv$target_url
      sm  <- import_summary()

      if (is.null(url)) {
        return(shiny::div(class = "text-muted small",
          shiny::icon("arrow-up"), " Complete Step 2 first."))
      }

      # Instance current-state badge
      im <- rv$instance_meta
      instance_status_ui <- if (!is.null(im)) {
        if (isTRUE(im$mg2_installed) && !is.na(im$n_org_units) && im$n_org_units > 0L) {
          shiny::div(class = "alert alert-success small py-2 mb-3 d-flex gap-2 align-items-center",
            shiny::icon("circle-check"),
            shiny::div(
              shiny::tags$strong("Already imported:"),
              sprintf(" %s org units, %s data elements (incl. MG2 output elements).",
                      format(im$n_org_units, big.mark = ","),
                      format(im$n_data_elements, big.mark = ",")),
              " Re-importing will update existing records."
            )
          )
        } else if (!is.na(im$n_org_units) && im$n_org_units > 0L) {
          shiny::div(class = "alert alert-warning small py-2 mb-3 d-flex gap-2 align-items-center",
            shiny::icon("triangle-exclamation"),
            sprintf("Instance has %s org units but MG2 output elements are missing — import to add them.",
                    format(im$n_org_units, big.mark = ","))
          )
        } else {
          shiny::div(class = "alert alert-secondary small py-2 mb-3 d-flex gap-2 align-items-center",
            shiny::icon("database"),
            "Instance appears blank — import will create all org units and data elements."
          )
        }
      } else NULL

      shiny::tagList(
        instance_status_ui,
        shiny::tags$p(class = "small mb-2", "Will import into", shiny::tags$code(url), ":"),
        shiny::tags$ul(
          class = "small mb-3",
          shiny::tags$li(shiny::tags$strong(sm$n_ou), " org units (from metadata file)"),
          shiny::tags$li(
            shiny::tags$strong(sm$n_de_source),
            " data elements from the loaded formula",
            shiny::tags$span(class = "text-muted", " (valueType + aggregationType inferred from data)")
          ),
          shiny::tags$li(
            shiny::tags$strong(sm$n_de_output),
            " MG2 output elements (trend, adjusted, forecast, WPE)"
          )
        ),
        if (sm$n_de_source == 0L)
          shiny::div(class = "alert alert-warning small py-2 mb-2",
            shiny::icon("triangle-exclamation"),
            " No formula data loaded \u2014 org units and MG2 output elements will still be imported.",
            " Load a dataset in the Data tab to also import source data elements."),
        if (sm$n_ou > 5000L)
          shiny::div(class = "alert alert-info small py-2 mb-2",
            shiny::icon("circle-info"),
            sprintf(" %s org units — the import may take 1\u20132 minutes.", format(sm$n_ou, big.mark = ","))),
        shiny::div(
          class = "d-flex align-items-center gap-2",
          shiny::actionButton(ns("btn_import_meta"), "Import metadata",
            icon = shiny::icon("upload"), class = "btn-primary"),
          shiny::uiOutput(ns("import_spinner_ui"))
        ),
        shiny::uiOutput(ns("import_result_ui"))
      )
    })

    output$import_spinner_ui <- shiny::renderUI(NULL)  # placeholder; withProgress handles feedback

    shiny::observeEvent(input$btn_import_meta, {
      url <- rv$target_url
      ous <- org_units()
      dat <- raw_data()

      if (is.null(url)) {
        shiny::showNotification("No target instance. Complete Step 2.", type = "warning")
        return()
      }

      n_ou <- if (!is.null(ous)) nrow(ous) else 0L
      cat("\n[Output tab] Starting metadata import to", url,
          "\n  Org units:", n_ou,
          "\n  Data loaded:", !is.null(dat) && nrow(dat) > 0L, "\n")

      rv$s3            <- "running"
      rv$import_result <- NULL

      result <- tryCatch({
        # --- Build payloads -------------------------------------------------
        ou_payload <- mg2_build_org_units_payload(ous)   # sorted by level
        de_source  <- if (!is.null(dat) && nrow(dat) > 0L &&
                          "data.id" %in% names(dat))
          mg2_build_source_de_payload(dat) else list()
        de_output  <- mg2_build_output_de_payload()
        de_payload <- c(de_source, de_output)

        # Batch size: small enough to avoid nginx 504 (~500 org units / POST)
        batch_size <- 500L
        ou_batches <- split(ou_payload,
                            ceiling(seq_along(ou_payload) / batch_size))
        n_batches  <- length(ou_batches)
        total_steps <- n_batches + 1L   # +1 for data elements

        totals <- list(created = 0L, updated = 0L, ignored = 0L)

        shiny::withProgress(
          message = sprintf("Importing metadata to %s", url),
          value   = 0,
          {
            # Step 0: data elements (fast, few)
            shiny::setProgress(0, detail = sprintf(
              "Data elements (%d items)", length(de_payload)))
            cat("  [1/", total_steps, "] Posting data elements...\n", sep = "")
            r0 <- mg2_post_metadata(url, data_elements = de_payload)
            if (!isTRUE(r0$ok))
              stop(paste("Data element import failed:", r0$message))
            cat("  Data elements:", r0$message, "\n")

            # Steps 1..n: org units in batches
            for (i in seq_along(ou_batches)) {
              shiny::setProgress(
                i / total_steps,
                detail = sprintf("Org units: batch %d of %d", i, n_batches)
              )
              cat("  [", i + 1L, "/", total_steps, "] Org unit batch ",
                  i, " (", length(ou_batches[[i]]), " units)...\n", sep = "")
              ri <- mg2_post_metadata(url, org_units = ou_batches[[i]])
              if (!isTRUE(ri$ok))
                stop(sprintf("Org unit batch %d/%d failed: %s", i, n_batches, ri$message))
              # Accumulate counts — DHIS2 2.36+ puts stats under response.stats
              d <- ri$detail$response$stats %||% ri$detail$stats
              if (!is.null(d)) {
                totals$created <- totals$created + (.nullish(d$created, 0L))
                totals$updated <- totals$updated + (.nullish(d$updated, 0L))
                totals$ignored <- totals$ignored + (.nullish(d$ignored, 0L))
              }
            }

            shiny::setProgress(1, detail = "Done")
          }
        )

        list(
          ok      = TRUE,
          message = sprintf(
            "Created: %d | Updated: %d | Ignored: %d  (%d batches)",
            totals$created, totals$updated, totals$ignored, n_batches
          )
        )
      }, error = function(e) {
        list(ok = FALSE, message = conditionMessage(e))
      })

      rv$import_result <- result
      rv$s3 <- if (isTRUE(result$ok)) "complete" else "error"
      if (isTRUE(result$ok)) {
        cat("[Output tab] Import done:", result$message, "\n")
        # Refresh instance state so the badge updates
        rv$instance_meta <- mg2_check_instance_metadata(rv$target_url)
      } else {
        cat("[Output tab] Import FAILED:", result$message, "\n")
      }
    })

    output$import_result_ui <- shiny::renderUI({
      res <- rv$import_result
      if (is.null(res)) return(NULL)
      shiny::div(
        class = if (isTRUE(res$ok)) "alert alert-success small py-2 mt-2 mb-0"
                else                "alert alert-danger  small py-2 mt-2 mb-0",
        shiny::icon(if (isTRUE(res$ok)) "circle-check" else "circle-xmark"),
        " ", res$message
      )
    })

    # ── Step 4: Write data values ───────────────────────────────────────────

    output$s4_badge <- shiny::renderUI(.out_status_badge(rv$s4))

    output$s4_body <- shiny::renderUI({
      url <- rv$target_url
      s3  <- rv$s3
      dat <- raw_data()

      if (s3 != "complete") {
        return(shiny::div(class = "text-muted small",
          shiny::icon("arrow-up"), " Complete Step 3 first."))
      }

      # Estimate value count
      n_raw <- if (!is.null(dat) && "original" %in% names(dat))
        sum(!is.na(dat$original)) else 0L

      period_col <- if (!is.null(dat)) {
        if ("Month" %in% names(dat)) "Month" else if ("Week" %in% names(dat)) "Week" else NULL
      } else NULL

      periods <- if (!is.null(period_col) && !is.null(dat)) {
        p <- sort(unique(dat[[period_col]]))
        list(
          min = .out_period_to_dhis2(min(p, na.rm = TRUE)),
          max = .out_period_to_dhis2(max(p, na.rm = TRUE))
        )
      } else list(min = "", max = "")

      shiny::tagList(
        shiny::tags$p(class = "small text-muted mb-3",
          "Write values to ", shiny::tags$code(url), ":"),

        # Raw data
        bslib::card(
          class = "mb-3",
          bslib::card_body(
            class = "py-2",
            shiny::checkboxInput(ns("write_raw"), "Raw data (from loaded formula)", value = TRUE),
            shiny::conditionalPanel(
              paste0("input['", ns("write_raw"), "']"),
              shiny::div(class = "d-flex align-items-center gap-2 ms-3 small",
                shiny::tags$span("Period:"),
                shiny::textInput(ns("period_from"), NULL, value = periods$min,
                  placeholder = "YYYYMM", width = "100px"),
                shiny::tags$span("to"),
                shiny::textInput(ns("period_to"), NULL, value = periods$max,
                  placeholder = "YYYYMM", width = "100px"),
                shiny::tags$span(class = "text-muted",
                  paste0("(~", format(n_raw, big.mark = ","), " values)"))
              )
            )
          )
        ),

        # Model outputs
        bslib::card(
          class = "mb-3",
          bslib::card_body(
            class = "py-2",
            shiny::checkboxInput(ns("write_outputs"), "Model outputs (trend, adjusted, forecast, WPE)",
              value = FALSE),
            shiny::conditionalPanel(
              paste0("input['", ns("write_outputs"), "']"),
              shiny::div(class = "text-muted small ms-3",
                shiny::icon("wrench"), " Model output writing coming soon.")
            )
          )
        ),

        if (n_raw > 1e6)
          shiny::div(class = "alert alert-warning small py-2 mb-2",
            shiny::icon("triangle-exclamation"),
            sprintf(" %s raw values — writing will take several minutes.",
                    format(n_raw, big.mark = ","))),

        shiny::actionButton(ns("btn_write"), "Write to DHIS2",
          icon = shiny::icon("upload"), class = "btn-primary"),
        shiny::uiOutput(ns("write_result_ui"))
      )
    })

    shiny::observeEvent(input$btn_write, {
      url <- rv$target_url
      dat <- raw_data()

      if (!isTRUE(input$write_raw)) {
        shiny::showNotification("Select at least one data type to write.", type = "warning")
        return()
      }

      cat("\n[Output tab] Write data values — data columns:", paste(names(dat), collapse = ", "), "\n")

      rv$s4        <- "running"
      rv$write_result <- NULL

      period_range <- if (nzchar(.nullish(input$period_from, "")) &&
                          nzchar(.nullish(input$period_to,   "")))
        c(input$period_from, input$period_to) else NULL

      batch_size <- 2000L

      result <- tryCatch({
        cat("[Output tab] Formatting data values...\n")
        dv <- mg2_format_raw_data_values(dat, period_range = period_range)
        n_total <- nrow(dv)
        cat("[Output tab] Formatted", format(n_total, big.mark = ","), "data values\n")

        if (n_total == 0L)
          return(list(ok = FALSE, message = "No data values after formatting.", n_written = 0L))

        n_batches <- ceiling(n_total / batch_size)
        cat("[Output tab] Writing in", n_batches, "batches of", batch_size, "\n")

        totals <- list(written = 0L, updated = 0L, ignored = 0L)

        shiny::withProgress(
          message = sprintf("Writing data values to %s", url),
          value   = 0,
          {
            for (i in seq_len(n_batches)) {
              idx_from <- (i - 1L) * batch_size + 1L
              idx_to   <- min(i * batch_size, n_total)
              shiny::setProgress(
                i / n_batches,
                detail = sprintf("Batch %d of %d (%s values)", i, n_batches,
                                 format(n_total, big.mark = ","))
              )
              ri <- mg2_post_data_values(url, data_values = dv[idx_from:idx_to, ])
              if (!isTRUE(ri$ok))
                stop(sprintf("Batch %d/%d failed: %s", i, n_batches, ri$message))
              totals$written  <- totals$written  + ri$n_written
              if (i %% 10L == 0L)
                cat("  Batch", i, "/", n_batches, "— written so far:", totals$written, "\n")
            }
            shiny::setProgress(1, detail = "Done")
          }
        )

        list(
          ok      = TRUE,
          message = sprintf("Written: %s values in %d batches",
                            format(totals$written, big.mark = ","), n_batches),
          n_written = totals$written
        )
      }, error = function(e) {
        list(ok = FALSE, message = conditionMessage(e), n_written = 0L)
      })

      rv$write_result <- result
      rv$s4 <- if (isTRUE(result$ok)) "complete" else "error"
      cat("[Output tab] Write", if (isTRUE(result$ok)) "done:" else "FAILED:", result$message, "\n")
    })

    output$write_result_ui <- shiny::renderUI({
      res <- rv$write_result
      if (is.null(res)) return(NULL)
      shiny::div(
        class = if (isTRUE(res$ok)) "text-success small mt-2" else "text-danger small mt-2",
        shiny::icon(if (isTRUE(res$ok)) "circle-check" else "circle-xmark"),
        " ", res$message
      )
    })

    # ── Card border colours (JS-free approach via inline styles) ────────────
    # Re-render the step cards' left-border colour via shinyjs when state changes.
    .border_colour <- function(state) {
      switch(state,
        pending  = "#ced4da",
        running  = "#0d6efd",
        complete = "#198754",
        error    = "#dc3545",
        "#ced4da"
      )
    }

    shiny::observe({
      for (sn in 1:4) {
        state <- switch(sn, rv$s1, rv$s2, rv$s3, rv$s4)
        col   <- .border_colour(state)
        shinyjs::runjs(sprintf(
          'var el = document.getElementById("%s");
           if (el) el.style.borderLeft = "4px solid %s";',
          ns(paste0("step_card_", sn)), col
        ))
      }
    })

  })
}

# ---- Internal UI helpers ----------------------------------------------------

.out_row <- function(label, value) {
  shiny::tags$tr(
    shiny::tags$td(
      class = "text-muted small fw-semibold pe-3",
      style = "white-space:nowrap; width:1%;",
      label
    ),
    shiny::tags$td(class = "small", value)
  )
}
