# combine_widget — build derived datasets from multiple processed .rds files
#
# Source selection mirrors data_widget: formula file → formula name → dataset
# file.  Users add sources one at a time; each appears in the sources list and
# its data values become available for step definitions.
#
# Companion definition files are named Combinations_[name]_[date].rds and
# stored in the data directory alongside the source datasets.

# Cleaning level choices (displayed label = value passed to apply_combine_cleaning)
.CLEAN_CHOICES <- c(
  "None (original values)"                          = "none",
  "Moderate — remove extreme values (mad15)"        = "mad15",
  "Standard — remove probable outliers (mad10)"     = "mad10",
  "Aggressive — remove seasonal outliers (seasonal5)" = "seasonal5",
  "Most aggressive — all detected outliers (seasonal3)" = "seasonal3"
)

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────

combine_widget_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      p("Build new derived datasets (e.g. Test Positivity Rate) by combining
        variables from existing processed datasets."),
      style = "font-weight:bold; margin-bottom:16px;"
    ),

    fluidRow(

      # ── Left sidebar: add sources + load definition ───────────────────────
      column(3,

        h5("Add source"),

        selectInput(
          ns("src_formula_file"),
          label     = "Formula file:",
          choices   = NULL,
          selectize = FALSE,
          size      = 3
        ),

        selectInput(
          ns("src_formula_name"),
          label     = "Indicator / formula:",
          choices   = NULL,
          selectize = FALSE,
          size      = 4
        ),

        selectInput(
          ns("src_dataset_file"),
          label     = "Dataset file:",
          choices   = NULL,
          selectize = FALSE,
          size      = 3
        ),

        actionButton(ns("add_source"), "Add as source",
                     class = "btn-sm btn-primary"),

        hr(),

        h5("Sources in use"),
        uiOutput(ns("source_list_ui")),
        div(
          selectInput(ns("source_to_remove"), NULL, choices = NULL,
                      selectize = FALSE, size = 3),
          actionButton(ns("remove_source"), "Remove selected",
                       class = "btn-sm btn-danger"),
          style = "font-size:85%;"
        ),

        hr(),

        h5("Load saved definition"),
        selectInput(
          ns("combo_def_file"),
          label     = NULL,
          choices   = NULL,
          selectize = FALSE,
          size      = 3
        ),
        actionButton(ns("load_combo_def"), "Load",
                     class = "btn-sm btn-default")
      ),

      # ── Main panel: Define / Preview / Build tabs ─────────────────────────
      column(9,

        tabsetPanel(
          id = ns("combo_tabs"),

          # ── Define ────────────────────────────────────────────────────────
          tabPanel("Define steps",
            br(),
            fluidRow(
              column(12,
                actionButton(ns("add_step"),    "Add step", class = "btn-sm btn-primary"),
                actionButton(ns("edit_step"),   "Edit",     class = "btn-sm btn-default"),
                actionButton(ns("delete_step"), "Delete",   class = "btn-sm btn-danger"),
                style = "margin-bottom:10px;"
              )
            ),
            DT::dataTableOutput(ns("steps_table"))
          ),

          # ── Preview ───────────────────────────────────────────────────────
          tabPanel("Preview",
            br(),
            fluidRow(
              column(5,
                selectInput(ns("preview_step_sel"),
                            "Show output through step:", choices = NULL)
              ),
              column(3,
                br(),
                actionButton(ns("run_preview"), "Refresh preview",
                             class = "btn-sm btn-default")
              )
            ),
            DT::dataTableOutput(ns("preview_table")),
            hr(),
            div(style = "max-height:250px; overflow-y:auto;",
                verbatimTextOutput(ns("build_status")))
          ),

          # ── Build & Save ──────────────────────────────────────────────────
          tabPanel("Build & Save",
            br(),
            fluidRow(
              column(5, textInput(ns("combo_name"),     "Combination name:", value = "")),
              column(7, textInput(ns("combo_filename"), "Output filename:",  value = ""))
            ),
            fluidRow(
              column(7,
                selectInput(ns("reg_formula_file"),
                            "Register in formula file (so it appears in Data → Formula):",
                            choices = NULL, selectize = FALSE, size = 3)
              )
            ),
            fluidRow(
              column(12,
                actionButton(ns("build_combo"), "Build & Save dataset",
                             class = "btn-success"),
                style = "margin-bottom:10px;"
              )
            ),
            div(style = "max-height:250px; overflow-y:auto;",
                verbatimTextOutput(ns("build_status")))
          )
        )
      )
    )
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Server
# ─────────────────────────────────────────────────────────────────────────────

combine_widget_server <- function(id,
                                  directory_widget_output = NULL,
                                  metadata_widget_output  = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive dependencies ───────────────────────────────────────────────
    data.folder <- reactive({ directory_widget_output$directory() })
    ousTree     <- reactive({ metadata_widget_output$ousTree() })

    # ── Step and source state ───────────────────────────────────────────────
    combo_steps   <- reactiveVal(list())  # list of step definitions
    editing_index <- reactiveVal(NULL)    # NULL = new; integer = editing

    # Sources: named list keyed by formula_name
    # Each entry: list(formula_name, dataset_file, meta)
    sources <- reactiveVal(list())

    # ── Formula file picker ─────────────────────────────────────────────────
    formula_files <- reactive({
      req(data.folder())
      list_dir_files(search = "Formulas_", dir = data.folder(), type = "xlsx|rds")
    })

    observe({
      ff <- formula_files()
      updateSelectInput(session, "src_formula_file",  choices = ff)
      updateSelectInput(session, "reg_formula_file",  choices = ff)
    })

    # ── Formula names from selected formula file ────────────────────────────
    formula_names_src <- reactive({
      req(input$src_formula_file, data.folder())
      file <- paste0(data.folder(), input$src_formula_file)
      if (!file.exists(file)) return(character(0))

      tryCatch({
        if (grepl("\\.xlsx$", file, ignore.case = TRUE)) {
          readxl::read_excel(file, sheet = "Formula") |>
            dplyr::filter(!is.na(Formula.Name)) |>
            dplyr::pull(Formula.Name) |>
            unique() |>
            sort()
        } else {
          d <- readRDS(file)
          sort(unique(d$Formula.Name))
        }
      }, error = function(e) character(0))
    })

    observe({
      updateSelectInput(session, "src_formula_name", choices = formula_names_src())
    })

    # ── Find matching dataset files — same logic as data_widget rds_data_file
    src_rds_files <- reactive({
      req(input$src_formula_name, data.folder())

      dir.files <- list.files(data.folder())

      # Filter to .rds files, excluding Update_ and Combinations_ files
      data.files <- dir.files[
        grepl("rds", dir.files, ignore.case = TRUE) &
        !grepl("Update_",       dir.files, ignore.case = TRUE) &
        !grepl("^Combinations_",dir.files)
      ]

      matches <- data.files[grepl(input$src_formula_name, data.files, fixed = TRUE)]

      if (length(matches) == 0) return(data.files)   # show all if no match

      # Sort by modification time, most recent first
      mtimes  <- file.info(file.path(data.folder(), matches))$mtime
      matches[rev(order(mtimes))]
    })

    observe({
      updateSelectInput(session, "src_dataset_file", choices = src_rds_files())
    })

    # ── Add / remove sources ────────────────────────────────────────────────
    observeEvent(input$add_source, {
      req(input$src_formula_name, input$src_dataset_file)
      dfile <- input$src_dataset_file
      if (is.null(dfile) || nchar(dfile) == 0) {
        showNotification("Select a dataset file first.", type = "warning")
        return()
      }

      fname <- input$src_formula_name
      fp    <- paste0(data.folder(), dfile)

      showModal(modalDialog(
        title = paste("Loading metadata for", fname, "…"),
        easyClose = TRUE, size = "s", footer = NULL
      ))

      meta <- tryCatch(read_combine_meta(fp),
                       error = function(e) {
                         showNotification(paste("Could not read file:", e$message),
                                          type = "error")
                         NULL
                       })
      removeModal()
      if (is.null(meta)) return()

      s <- sources()
      s[[fname]] <- list(formula_name = fname, dataset_file = dfile, meta = meta)
      sources(s)
    })

    observeEvent(input$remove_source, {
      req(input$source_to_remove)
      s <- sources()
      s[[input$source_to_remove]] <- NULL
      sources(s)
    })

    # Keep remove picker in sync with sources list
    observe({
      s <- sources()
      updateSelectInput(session, "source_to_remove",
                        choices = if (length(s)) names(s) else character(0))
    })

    # ── Display added sources ────────────────────────────────────────────────
    output$source_list_ui <- renderUI({
      s <- sources()
      if (length(s) == 0) return(p("(none)", style = "color:#999; font-size:85%;"))

      lapply(names(s), function(fname) {
        m <- s[[fname]]$meta
        tagList(
          tags$details(
            tags$summary(
              tags$b(fname),
              if (!is.null(m))
                tags$span(paste0(" (", m$n_rows, " rows, ", m$period_type, ")"),
                          style = "font-size:82%; color:#666;")
            ),
            if (!is.null(m))
              tags$ul(
                style = "font-size:80%; max-height:120px; overflow-y:auto; margin:3px 0;",
                lapply(m$data_values, tags$li)
              )
          ),
          br()
        )
      })
    })

    # ── Available Combinations_*.rds definition files ────────────────────────
    observe({
      req(data.folder())
      defs <- list_dir_files(search = "Combinations_", type = "rds",
                             dir = data.folder())
      updateSelectInput(session, "combo_def_file",
                        choices = if (length(defs)) defs else character(0))
    })

    # ── Load definition file ────────────────────────────────────────────────
    observeEvent(input$load_combo_def, {
      req(input$combo_def_file, data.folder())
      fp <- paste0(data.folder(), input$combo_def_file)
      if (!file.exists(fp)) {
        showNotification("Definition file not found.", type = "error"); return()
      }

      # Corner notification so the rest of the UI stays visible
      notif_id <- showNotification(
        ui = tagList(
          tags$span(class = "fa fa-spinner fa-spin",
                    style = "margin-right:6px;"),
          paste("Loading", input$combo_def_file, "…")
        ),
        duration = NULL, closeButton = FALSE, type = "message"
      )
      on.exit(removeNotification(notif_id), add = TRUE)

      def <- tryCatch(load_combine_definition(fp),
                      error = function(e) {
                        showNotification(paste("Load failed:", e$message), type = "error")
                        NULL
                      })
      if (is.null(def)) return()
      combo_steps(def$steps)

      # Reconstruct sources from the step definitions so Edit works immediately.
      # Collect every unique dataset file referenced across all steps.
      src_files <- unique(unlist(lapply(def$steps, function(s) {
        if (identical(s$operation, "Ratio"))
          c(s$numerator$source_file, s$denominator$source_file)
        else
          s$source_file
      })))
      new_sources <- list()
      for (dfile in src_files) {
        fp_src <- paste0(data.folder(), dfile)
        if (!file.exists(fp_src)) next
        meta  <- tryCatch(read_combine_meta(fp_src), error = function(e) NULL)
        fname <- tools::file_path_sans_ext(dfile)
        new_sources[[fname]] <- list(formula_name = fname,
                                     dataset_file  = dfile,
                                     meta          = meta)
      }
      sources(new_sources)

      if (!is.null(def$combo_name))
        updateTextInput(session, "combo_name", value = def$combo_name)
      showNotification(paste("Loaded:", input$combo_def_file), type = "message")
    })

    # ── Helpers used in step modal ──────────────────────────────────────────

    # Choices for source picker in modal: formula_name → dataset_file
    source_choices <- reactive({
      s <- sources()
      if (length(s) == 0) return(character(0))
      setNames(sapply(s, `[[`, "dataset_file"), names(s))
    })

    values_for_file <- function(dataset_file) {
      s <- sources()
      entry <- Filter(function(x) x$dataset_file == dataset_file, s)
      if (length(entry) == 0 || is.null(entry[[1]]$meta)) return(character(0))
      entry[[1]]$meta$data_values
    }

    cleaning_for_file <- function(dataset_file) {
      s <- sources()
      entry <- Filter(function(x) x$dataset_file == dataset_file, s)
      if (length(entry) == 0 || is.null(entry[[1]]$meta)) return(.CLEAN_CHOICES)
      avail <- entry[[1]]$meta$cleaning_levels
      .CLEAN_CHOICES[.CLEAN_CHOICES %in% c("none", avail)]
    }

    # ── Steps table ─────────────────────────────────────────────────────────
    steps_df <- reactive({
      steps <- combo_steps()
      if (length(steps) == 0) {
        return(data.frame(`#` = integer(), Operation = character(),
                          `Output name` = character(), `Max` = character(),
                          Source = character(), Cleaning = character(),
                          check.names = FALSE))
      }
      do.call(rbind, lapply(seq_along(steps), function(i) {
        s   <- steps[[i]]
        src <- if (s$operation == "Ratio") {
          paste0("Num: ", s$numerator$source_file,
                 " [", paste(s$numerator$data_values, collapse = ", "), "]  /  ",
                 "Den: ", s$denominator$source_file,
                 " [", paste(s$denominator$data_values, collapse = ", "), "]")
        } else {
          paste0(s$source_file, " [", paste(s$data_values, collapse = ", "), "]")
        }
        clean <- if (s$operation == "Ratio") {
          paste0("Num:", s$numerator$cleaning_level,
                 " / Den:", s$denominator$cleaning_level)
        } else { s$cleaning_level }
        mv <- if (!is.null(s$max_value) && !is.na(s$max_value))
          as.character(s$max_value) else "—"

        data.frame(`#` = i, Operation = s$operation, `Max` = mv,
                   `Output name` = s$output_name, Source = src,
                   Cleaning = clean, check.names = FALSE, stringsAsFactors = FALSE)
      }))
    })

    output$steps_table <- DT::renderDT({
      DT::datatable(steps_df(), selection = "single", rownames = FALSE,
                    options = list(dom = "t", pageLength = 20, scrollX = TRUE))
    })

    observe({
      steps   <- combo_steps()
      choices <- if (length(steps) == 0) character(0) else
        setNames(seq_along(steps),
                 paste0(seq_along(steps), ": ", sapply(steps, `[[`, "output_name")))
      updateSelectInput(session, "preview_step_sel", choices = choices,
                        selected = if (length(choices)) tail(choices, 1) else NULL)
    })

    # ── Step modal ──────────────────────────────────────────────────────────
    step_modal <- function(prefill = NULL) {
      srcs <- source_choices()
      if (length(srcs) == 0) {
        showNotification("Add at least one source dataset before defining steps.",
                         type = "warning")
        return()
      }

      is_ratio   <- !is.null(prefill) && prefill$operation == "Ratio"
      default_op <- if (is_ratio) "Ratio" else "Include"
      def_name   <- if (!is.null(prefill)) prefill$output_name else ""
      def_max    <- if (!is.null(prefill) && !is.null(prefill$max_value) &&
                        !is.na(prefill$max_value)) prefill$max_value else NA

      # Include defaults
      inc_src   <- if (!is_ratio && !is.null(prefill)) prefill$source_file  else srcs[1]
      inc_vals  <- if (!is_ratio && !is.null(prefill)) prefill$data_values  else character(0)
      inc_clean <- if (!is_ratio && !is.null(prefill)) prefill$cleaning_level else "seasonal3"

      # Ratio defaults
      r_nsrc   <- if (is_ratio) prefill$numerator$source_file    else srcs[1]
      r_nvals  <- if (is_ratio) prefill$numerator$data_values    else character(0)
      r_nclean <- if (is_ratio) prefill$numerator$cleaning_level else "seasonal3"
      r_dsrc   <- if (is_ratio) prefill$denominator$source_file  else srcs[1]
      r_dvals  <- if (is_ratio) prefill$denominator$data_values  else character(0)
      r_dclean <- if (is_ratio) prefill$denominator$cleaning_level else "seasonal3"

      showModal(modalDialog(
        title     = if (is.null(prefill)) "Add step" else "Edit step",
        easyClose = TRUE, size = "l",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_step"), "OK", class = "btn-primary")
        ),

        fluidRow(
          column(4, textInput(ns("step_output_name"), "Output variable name:",
                              value = def_name)),
          column(3, numericInput(ns("step_max_value"),
                                 "Max valid value (optional):",
                                 value = def_max, min = 0, step = 0.01)),
          column(5, br(),
            radioButtons(ns("step_operation"), NULL,
                         choices = c("Include", "Ratio"),
                         selected = default_op, inline = TRUE))
        ),

        # ── Include ──────────────────────────────────────────────────────
        conditionalPanel(
          condition = paste0("input['", ns("step_operation"), "'] == 'Include'"),
          hr(),
          fluidRow(
            column(4,
              selectInput(ns("inc_source"), "Source (formula name):",
                          choices = srcs, selected = inc_src)
            ),
            column(4,
              selectizeInput(ns("inc_values"), "Data values:",
                             choices = values_for_file(inc_src),
                             selected = inc_vals, multiple = TRUE)
            ),
            column(4,
              selectInput(ns("inc_cleaning"), "Cleaning:",
                          choices = cleaning_for_file(inc_src),
                          selected = inc_clean)
            )
          )
        ),

        # ── Ratio ─────────────────────────────────────────────────────────
        conditionalPanel(
          condition = paste0("input['", ns("step_operation"), "'] == 'Ratio'"),
          hr(),
          h5("Numerator"),
          fluidRow(
            column(4,
              selectInput(ns("ratio_num_source"), "Source (formula name):",
                          choices = srcs, selected = r_nsrc)
            ),
            column(4,
              selectizeInput(ns("ratio_num_values"),
                             "Data values (summed if multiple):",
                             choices = values_for_file(r_nsrc),
                             selected = r_nvals, multiple = TRUE)
            ),
            column(4,
              selectInput(ns("ratio_num_cleaning"), "Cleaning:",
                          choices = cleaning_for_file(r_nsrc),
                          selected = r_nclean)
            )
          ),
          h5("Denominator"),
          fluidRow(
            column(4,
              selectInput(ns("ratio_den_source"), "Source (formula name):",
                          choices = srcs, selected = r_dsrc)
            ),
            column(4,
              selectizeInput(ns("ratio_den_values"),
                             "Data values (summed if multiple):",
                             choices = values_for_file(r_dsrc),
                             selected = r_dvals, multiple = TRUE)
            ),
            column(4,
              selectInput(ns("ratio_den_cleaning"), "Cleaning:",
                          choices = cleaning_for_file(r_dsrc),
                          selected = r_dclean)
            )
          ),
          p("Multiple values on either side are summed before dividing.",
            style = "font-size:85%; color:#666;")
        )
      ))
    }

    # Update data value choices when source changes inside modal.
    # When editing a saved step, the source observer fires on modal open with
    # the prefilled source value — in that case restore saved selections
    # instead of clearing them.
    prefill_for_edit <- function() {
      idx <- editing_index()
      if (is.null(idx)) NULL else combo_steps()[[idx]]
    }

    observeEvent(input$inc_source, {
      pf <- prefill_for_edit()
      if (!is.null(pf) && identical(pf$source_file, input$inc_source)) {
        updateSelectizeInput(session, "inc_values",
                             choices  = values_for_file(input$inc_source),
                             selected = pf$data_values)
        updateSelectInput(session, "inc_cleaning",
                          choices  = cleaning_for_file(input$inc_source),
                          selected = pf$cleaning_level)
      } else {
        updateSelectizeInput(session, "inc_values",
                             choices = values_for_file(input$inc_source), selected = NULL)
        updateSelectInput(session, "inc_cleaning",
                          choices = cleaning_for_file(input$inc_source))
      }
    })
    observeEvent(input$ratio_num_source, {
      pf <- prefill_for_edit()
      if (!is.null(pf) && identical(pf$numerator$source_file, input$ratio_num_source)) {
        updateSelectizeInput(session, "ratio_num_values",
                             choices  = values_for_file(input$ratio_num_source),
                             selected = pf$numerator$data_values)
        updateSelectInput(session, "ratio_num_cleaning",
                          choices  = cleaning_for_file(input$ratio_num_source),
                          selected = pf$numerator$cleaning_level)
      } else {
        updateSelectizeInput(session, "ratio_num_values",
                             choices = values_for_file(input$ratio_num_source), selected = NULL)
        updateSelectInput(session, "ratio_num_cleaning",
                          choices = cleaning_for_file(input$ratio_num_source))
      }
    })
    observeEvent(input$ratio_den_source, {
      pf <- prefill_for_edit()
      if (!is.null(pf) && identical(pf$denominator$source_file, input$ratio_den_source)) {
        updateSelectizeInput(session, "ratio_den_values",
                             choices  = values_for_file(input$ratio_den_source),
                             selected = pf$denominator$data_values)
        updateSelectInput(session, "ratio_den_cleaning",
                          choices  = cleaning_for_file(input$ratio_den_source),
                          selected = pf$denominator$cleaning_level)
      } else {
        updateSelectizeInput(session, "ratio_den_values",
                             choices = values_for_file(input$ratio_den_source), selected = NULL)
        updateSelectInput(session, "ratio_den_cleaning",
                          choices = cleaning_for_file(input$ratio_den_source))
      }
    })

    # ── Add / Edit / Delete steps ───────────────────────────────────────────
    observeEvent(input$add_step,    { editing_index(NULL); step_modal() })

    observeEvent(input$edit_step, {
      sel <- input$steps_table_rows_selected
      if (is.null(sel)) {
        showNotification("Select a step row to edit.", type = "warning"); return()
      }
      editing_index(sel)
      step_modal(prefill = combo_steps()[[sel]])
    })

    observeEvent(input$delete_step, {
      sel <- input$steps_table_rows_selected
      if (is.null(sel)) {
        showNotification("Select a step row to delete.", type = "warning"); return()
      }
      steps         <- combo_steps()
      steps[[sel]]  <- NULL
      combo_steps(steps)
    })

    observeEvent(input$confirm_step, {
      op   <- input$step_operation
      name <- trimws(input$step_output_name)

      if (nchar(name) == 0) {
        showNotification("Enter an output variable name.", type = "warning"); return()
      }

      mv <- suppressWarnings(as.numeric(input$step_max_value))
      mv <- if (length(mv) == 0 || is.na(mv)) NA_real_ else mv

      step <- if (op == "Include") {
        if (length(input$inc_values) == 0) {
          showNotification("Select at least one data value.", type = "warning"); return()
        }
        list(operation = "Include", output_name = name, max_value = mv,
             source_file    = input$inc_source,
             data_values    = input$inc_values,
             cleaning_level = input$inc_cleaning)
      } else {
        if (length(input$ratio_num_values) == 0 || length(input$ratio_den_values) == 0) {
          showNotification("Select numerator and denominator values.", type = "warning")
          return()
        }
        list(
          operation   = "Ratio",
          output_name = name,
          max_value   = mv,
          numerator   = list(source_file    = input$ratio_num_source,
                             data_values    = input$ratio_num_values,
                             cleaning_level = input$ratio_num_cleaning),
          denominator = list(source_file    = input$ratio_den_source,
                             data_values    = input$ratio_den_values,
                             cleaning_level = input$ratio_den_cleaning)
        )
      }

      steps <- combo_steps()
      idx   <- editing_index()
      if (is.null(idx)) steps <- c(steps, list(step)) else steps[[idx]] <- step
      combo_steps(steps)
      removeModal()
    })

    # ── Shared build state ───────────────────────────────────────────────────
    # built_result: full dataset from the last successful build (any step count)
    # built_n_steps: how many steps that result covers
    # build_log: captured cat() output shown to the user
    built_result  <- reactiveVal(NULL)
    built_n_steps <- reactiveVal(0L)
    build_log     <- reactiveVal("")

    # Run build_combined_dataset inside withProgress so stage labels appear
    # in real-time.  cat() output is still captured for the post-build log box.
    # Returns the result data.table, or NULL on error.
    do_build <- function(steps_to_use, label = "Building") {
      name <- if (nchar(trimws(input$combo_name)) > 0)
                trimws(input$combo_name) else "Combined"

      result    <- NULL
      log_lines <- character(0)

      withProgress(
        message = paste(label, name, "…"), value = 0,
        {
          log_lines <- capture.output({
            result <- tryCatch(
              build_combined_dataset(
                steps        = steps_to_use,
                data_folder  = data.folder(),
                ousTree      = if (!is.null(ousTree)) ousTree() else NULL,
                formula_name = name,
                .progress    = function(value, message, detail = "")
                                 setProgress(value = value, message = message,
                                             detail = detail)
              ),
              error = function(e) e
            )
          }, type = "output")
        }
      )

      build_log(paste(log_lines, collapse = "\n"))

      if (inherits(result, "error")) {
        build_log(paste(build_log(), "\nERROR:", result$message))
        showNotification(paste(label, "failed:", result$message), type = "error")
        return(NULL)
      }
      result
    }

    # ── Preview ──────────────────────────────────────────────────────────────
    preview_data <- reactiveVal(NULL)

    observeEvent(input$run_preview, {
      req(length(combo_steps()) > 0, data.folder())
      through   <- as.integer(input$preview_step_sel)
      n_all     <- length(combo_steps())
      steps_sub <- combo_steps()[seq_len(min(through, n_all))]

      result <- do_build(steps_sub, label = "Preview")

      if (!is.null(result)) {
        # Cache the full result when preview covers all steps — reused by Save
        built_result(result)
        built_n_steps(length(steps_sub))
        # Preview table shows first 500 rows with yearmonth → character
        d <- head(as.data.frame(result), 500)
        for (col in names(d)) {
          if (tsibble::is_yearmonth(d[[col]]) || tsibble::is_yearweek(d[[col]]))
            d[[col]] <- as.character(d[[col]])
        }
        preview_data(d)
      }
    })

    output$preview_table <- DT::renderDT({
      req(preview_data())
      DT::datatable(preview_data(), rownames = FALSE,
                    options = list(scrollX = TRUE, pageLength = 15, dom = "tip"))
    })

    # ── Build & Save ─────────────────────────────────────────────────────────
    observeEvent(input$combo_name, {
      name <- trimws(input$combo_name)
      if (nchar(name) > 0)
        updateTextInput(session, "combo_filename",
                        value = suggest_combine_filename(name))
    })

    observeEvent(input$build_combo, {
      req(length(combo_steps()) > 0, data.folder())
      name <- trimws(input$combo_name)
      fn   <- trimws(input$combo_filename)
      if (nchar(name) == 0) {
        showNotification("Enter a combination name first.", type = "warning"); return()
      }
      if (nchar(fn) == 0) fn <- suggest_combine_filename(name)
      out_path <- paste0(data.folder(), fn)

      # Reuse previewed result if it covers all steps; otherwise build fresh
      n_all  <- length(combo_steps())
      result <- if (!is.null(built_result()) && built_n_steps() == n_all) {
        build_log(paste0(build_log(),
                         "\n[Save] Reusing previewed dataset (", n_all,
                         " step(s)) — skipping rebuild."))
        built_result()
      } else {
        r <- do_build(combo_steps(), label = "Build")
        if (!is.null(r)) {
          built_result(r)
          built_n_steps(n_all)
        }
        r
      }

      if (is.null(result)) return()

      saveRDS(result, out_path)

      def_fn   <- paste0("Combinations_", gsub("[^A-Za-z0-9_-]", "_", name),
                         "_", format(Sys.Date(), "%Y-%m-%d"), ".rds")
      def_path <- paste0(data.folder(), def_fn)
      save_combine_definition(
        list(steps = combo_steps(), combo_name = name, output_file = fn,
             created = Sys.time()),
        def_path
      )

      # Register combo_name in the selected formula xlsx so data_widget can find it
      reg_file <- trimws(if (is.null(input$reg_formula_file)) "" else input$reg_formula_file)
      reg_msg  <- ""
      if (length(reg_file) > 0 && nchar(reg_file) > 0) {
        reg_path <- paste0(data.folder(), reg_file)
        reg_err  <- NULL
        tryCatch(
          register_combined_formula(name, reg_path),
          error = function(e) { reg_err <<- e$message }
        )
        reg_msg <- if (is.null(reg_err))
          paste0("\nRegistered '", name, "' in ", reg_file)
        else
          paste0("\nREGISTRATION FAILED: ", reg_err, "\n  (file: ", reg_file, ")")
      } else {
        reg_msg <- "\nNo formula file selected — name not registered."
      }

      save_msg <- paste0(
        "\nSaved: ", fn,
        "\nDefinition: ", def_fn,
        reg_msg, "\n",
        nrow(result), " rows  |  ",
        length(unique(result$data)), " variable(s): ",
        paste(sort(unique(result$data)), collapse = ", ")
      )
      build_log(paste0(build_log(), save_msg))
      showNotification("Dataset saved successfully.", type = "message", duration = 6)
    })

    output$build_status <- renderText({ build_log() })

    return(invisible(NULL))
  })
}
