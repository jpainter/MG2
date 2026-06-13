data_widget_ui = function(id) {
  ns <- NS(id)

  tagList(
    shinybusy::add_busy_spinner(
      spin = "fading-circle", # "self-building-square",
      position = 'bottom-left',
      timeout = 100,
      onstart = FALSE
      # , margins = c(70, 1200)
    ),

    div(
      # Header text for the top of the page
      p(
        "Create groupings (formulas) of data by subject (e.g. confirmed malaria cases) with a file that indicates which DHIS2 data elements or indicators are included"
      ),
      style = "font-weight: bold; text-align: left; margin-bottom: 20px;" # Center alignment and spacing
    ),

    actionButton(ns("refresh"), "Refresh"),

    selectInput(
      ns("formula.file"),
      label = "Formula Files:",
      width = '95%',
      choices = NULL,
      selected = FALSE,
      multiple = FALSE,
      selectize = FALSE,
      size = 4 ##needed for `selected = FALSE` to work )
    ),

    selectizeInput(
      ns("formula"),
      label = "Select/Add Formula:",
      width = '95%',
      choices = "",
      options = list(create = TRUE),
      selected = FALSE,
      multiple = FALSE,
      # selectize = FALSE,
      size = 4 ##needed for `selected = FALSE` to work )
    ),

    div(
      selectInput(
        ns("dataset"),
        label = div(
          "Data previously downloaded from DHIS2:",
          style = "font-size: 100%"
        ),
        width = '95%',
        choices = NULL,
        selected = FALSE,
        multiple = FALSE,
        selectize = FALSE,
        size = 4 ##needed for `selected = FALSE` to work )
      ),
      style = "overflow-y: auto; font-size: 66%; width: 100%"
    ),

    actionButton(ns("rescan"), "Rescan dataset (optional)", class = "btn-warning btn-sm"),

    br(),

    tags$hr(style = "margin-top: 18px; margin-bottom: 12px;"),

    div(
      style = "font-size: 12px; color: #555; line-height: 1.6;",

      tags$p(tags$strong("How this tab works:"),
             style = "margin-bottom: 8px; color: #333; font-size: 13px;"),

      tags$p(
        tags$span("1.", style = "font-weight:bold; color:#4a90d9; margin-right:5px;"),
        tags$strong("Select or create a formula file"), " — a formula groups DHIS2 data elements
        by subject (e.g. malaria cases = confirmed + suspected)."
      ),
      tags$p(
        style = "margin-left: 16px; margin-top: -4px;",
        icon("folder-open", style = "color:#888; margin-right:4px; font-size:0.9em;"),
        tags$em("Have a file?"), " Select it from the Formula Files list above.",
        tags$br(),
        icon("plus", style = "color:#888; margin-right:4px; font-size:0.9em;"),
        tags$em("Starting fresh?"), " Type a new name in ",
        tags$strong("Select/Add Formula"), ", then use the ",
        tags$strong("Select"), " tab on the right to search for and add data elements.
        Click ", tags$strong("Save Changes"), " when done."
      ),

      tags$p(
        tags$span("2.", style = "font-weight:bold; color:#4a90d9; margin-right:5px;"),
        "Go to ", tags$strong("Data → Download"), " to fetch data from DHIS2 for your selected
        formula, org units, and time period. Downloaded files appear in the",
        tags$em("Data previously downloaded"), " list above."
      ),

      tags$p(
        tags$span("3.", style = "font-weight:bold; color:#4a90d9; margin-right:5px;"),
        "Select a downloaded dataset above to load it — then proceed to",
        tags$strong("DQA, Reporting,"), " and ", tags$strong("Outliers"), " for analysis."
      ),

      tags$hr(style = "margin: 10px 0;"),

      tags$p(
        icon("layer-group", style = "color:#4a90d9; margin-right:5px;"),
        tags$strong("Combine tab:"),
        " Build derived datasets by merging or ratioing two existing downloaded files
        (e.g. test positivity rate = positives ÷ tests performed).",
        style = "margin-bottom: 0;"
      )
    )

  )
  # ) # end fillColl
} # ui


data_widget_server <- function(
  id,
  metadata_widget_output = NULL,
  directory_widget_output = NULL,
  data_request_output = NULL,
  formulaSaved = NULL
) {
  moduleServer(
    id,
    function(input, output, session) {
      # Rescan state: holds reconstructed+processed data until scan saves the file
      rescan_val <- reactiveVal(NULL)
      # Incremented by cleaning_widget after saving; dataset() depends on it
      scan_done_counter <- reactiveVal(0L)
      # Cache for dataset(): skip re-read (and "Reading data" modal) when
      # input$dataset is re-sent by Shiny with the same file path.
      .dw_cache <- list(path = NULL, data = NULL, sc = -1L)

      # Reactive dependecies
      data.folder = reactive({
        directory_widget_output$directory()
      })
      dataElements = reactive({
        metadata_widget_output$dataElements()
      })
      dataSets = reactive({
        metadata_widget_output$dataSets()
      })
      categories = reactive({
        metadata_widget_output$categories()
      })
      ousTree = reactive({
        metadata_widget_output$ousTree()
      })
      completedRequest = reactive({
        data_request_output$completedRequest()
      })

      # completedRequest = reactive({ data_request_output$completedRequest() })

      formula.files = reactive({
        if (!is.null(formulaSaved)) formulaSaved()  # re-scan after save creates a new file
        req(data.folder())
        cat('\n* data_widget looking for formula files in', data.folder(), '\n')

        ff = list_dir_files(search = 'Formulas_', dir = data.folder(), type = 'xlsx|rds')
        if (is_empty(ff)) {
          cat('\n - no forumula files in directory')
          return()
        }

        # cat( '\n - formula.files:' , ff  )
        if (!any(file.exists(paste0(data.folder(), ff)))) {
          return()
        }

        return(ff)
      })

      # When the directory changes, reset both formula file and formula dropdowns
      observeEvent(data.folder(), {
        updateSelectInput(session, 'formula.file', choices = character(0), selected = NULL)
        updateSelectizeInput(session, 'formula',   choices = character(0), selected = NULL)
        updateSelectInput(session, 'dataset',      choices = character(0), selected = NULL)
      }, ignoreInit = TRUE)

      # After a save, remember the formula name so we can re-select it once
      # the file list refreshes (which clears the formula dropdown)
      formula_to_restore <- reactiveVal(NULL)

      observeEvent(formulaSaved(), {
        req(formulaSaved() > 0)
        formula_to_restore(input$formula)
      }, ignoreInit = TRUE, priority = 1000)

      # trigger refresh after completed download
      observeEvent(input$refresh, {
        cat('\n* Update data widget text boxes')

        aa = data.folder()

        cat('\n - looking for formula files in', aa, '\n')

        ff = list_dir_files(search = 'Formulas_', dir = aa, type = 'xlsx|rds')
        if (is_empty(ff)) {
          cat('\n - no forumula files in directory')
          return()
        }

        cat('\n - Update data formula files')
        updateSelectInput(session, 'formula.file', choices = ff, selected = 1)

        b = formula.names()
        cat('\n - Update data formula.names')
        updateSelectInput(session, 'formula', choices = "", selected = NULL)

        cat('\n - Update rds_data_file')
        updateSelectInput(session, 'dataset', choices = "", selected = NULL) # rds_data_file()[1] )            }
      })

      observeEvent(completedRequest(), {
        cat('\n* data_widget completedRequest():')
        a = formula.files()
      })

      observe({
        cat('\n* updating directory \n')
        # updateTextOutput( session, 'directory' , value = data.folder()  )
        output$directory = renderText({
          data.folder()
        })
      })

      observe({
        cat('\n* updating formula.files input ')
        updateSelectInput(
          session,
          'formula.file',
          choices = formula.files(),
          selected = 1
        )
      })

      formulas = reactive({
        if (!is.null(formulaSaved)) formulaSaved()  # invalidate when formula is saved
        cat('\n* formulas:')

        if (is.null(input$formula.file)) {
          return(NULL)
        }

        file = paste0(data.folder(), input$formula.file)
        cat('\n - formula file:', file)

        if (!any(file.exists(file))) {
          return(NULL)
        }

        if (grepl(fixed('.xlsx'), file)) {
          cat('\n - read xls file', file)
          formulas = read_excel(file, sheet = 'Formula') %>%
            filter(!is.na(Formula.Name)) %>%
            arrange(desc(Formula.Name))
        } else {
          cat('\n - reading formula rds file', file)
          formulas = readRDS(file)
        }

        cat('\n - formula.Name:', unique(formulas$Formula.Name))

        return(formulas)
      })

      formula.names = reactive({
        formulas()$Formula.Name
      })

      all_formula_elements = reactive({
        if (!is.null(formulaSaved)) formulaSaved()  # invalidate when formula is saved
        req(input$formula.file)
        cat('\n* all_formula_elements:')

        file = paste0(data.folder(), input$formula.file)
        cat('\n - formula file', input$formula.file)

        if (!any(file.exists(file))) {
          return(NULL)
        }

        if (grepl(fixed('.xlsx'), file)) {
          cat('\n - read xls file', file)
          formulas = read_excel(
            file,
            sheet = 'Formula Elements',
            guess_max = 1e6
          )
        } else {
          cat('\n - reading formula elements from formula rds file', file)
          formulas = readRDS(file)
        }

        # Legacy files without a role column: treat all elements as primary
        if (!"role" %in% names(formulas)) formulas[["role"]] <- "primary"

        formulas
      })

      formula_elements = reactive({
        req(all_formula_elements())
        req(input$formula.file)
        req(input$formula)
        cat('\n* formula_elements:')

        fe <- all_formula_elements()

        # Guard: empty or malformed formula file (no elements saved yet)
        if (!is.data.frame(fe) || nrow(fe) == 0 || !"Formula.Name" %in% names(fe)) {
          cat('\n - formula elements empty or missing Formula.Name column')
          return(tibble::tibble())
        }

        cat('\n - selecting formula')
        fe %>% filter(Formula.Name %in% input$formula)
      })

      data.dir_files = reactive({
        req(input$formula.file)
        completedRequest()  # re-scan directory after each completed download
        if (!dir.exists(data.folder())) {
          return(NULL)
        }
        dir.files = list.files(data.folder())
        cat("\n - number of dir.files :", length(dir.files))
        return(dir.files)
      })

      rds_data_file = reactive({
        req(data.dir_files())
        req(data.folder())
        req(input$formula)

        dir.files = data.dir_files()

        formula = input$formula # paste0( input$formula , "_" )
        cat('\n* rds_data_file formula: ', input$formula, '\n')

        file.type = 'rds|fst|qs'
        file.keywords = 'rds|fst|qs'

        data.files = dir.files[
          grepl(file.type, dir.files, ignore.case = T) &
            grepl(file.keywords, dir.files, ignore.case = T) &
            !grepl("Update_", dir.files, ignore.case = T)
        ]

        cat('\n- all levels data files:', length(data.files))

        f.formula = grepl(formula, data.files, fixed = TRUE)

        cat("\n- f.formula:", sum(f.formula, na.rm = TRUE))

        if (sum(f.formula) == 0) {
          cat('\n - no data files for this formula')
          return("")
        }

        if (!dir.exists(data.folder())) {
          cat('\n - no folder matching data.folder()')
          return("")
        }

        data_files = data.files[f.formula] # %>% most_recent_file()

        # cat( '\n data_files are:\n' , data_files )

        # Arrange by modified date
        data_file.mdate = file.info(paste0(data.folder(), data_files))$mtime
        data_files = data_files[rev(order(data_file.mdate))]

        cat('\n - done:', length(data_files), 'files')
        return(data_files)
      })

      # update formulas
      observeEvent(input$formula.file, {
        cat('\n* updating formula list')
        choices <- formula.names()
        desired <- formula_to_restore()

        if (!is.null(desired) && desired != "" && desired %in% choices) {
          cat('\n - restoring formula selection:', desired)
          updateSelectizeInput(session, 'formula', choices = choices, selected = desired)
          formula_to_restore(NULL)
        } else {
          updateSelectInput(session, 'formula', choices = choices, selected = 1)
        }

        updateSelectInput(session, 'dataset', choices = NULL, selected = NULL)
      })

      # Update list of data files
      observe({
        cat('\n* updating dataset list')
        updateSelectInput(
          session,
          'dataset',
          choices = rds_data_file(),
          selected = NULL
        ) # rds_data_file()[1] )
      })

      dataset.file = reactive({
        req(input$dataset)
        req(data.folder())

        cat('\n* data_widget  dataset.file():', input$dataset)

        data.folder = isolate(data.folder())
        file = paste0(data.folder, input$dataset)

        cat('\n - ', file)
        return(file)
      })

      # Rescan button: reconstruct raw format → data_1() → store in rescan_val()
      # ignoreInit = TRUE prevents firing on app open (button counter starts at 0)
      observeEvent(input$rescan, {
        req(dataset())
        req(formula_elements())
        req(ousTree())

        cat('\n* data_widget rescan button clicked')

        if (!'original' %in% names(dataset())) {
          showModal(modalDialog(
            title = "Cannot rescan: no 'original' column found in dataset",
            easyClose = TRUE, size = 's',
            footer = '(click anywhere to continue)'
          ))
          return()
        }

        # Reset any previously cached rescan result
        rescan_val(NULL)

        idx_var <- if ('Month' %in% names(dataset())) 'Month' else 'Week'
        data = dataset() %>%
          as_tibble() %>%
          ungroup() %>%
          mutate(
            dataElement = sub("_.*$", "", data.id),
            categoryOptionCombo = ifelse(
              grepl("_", data.id),
              sub("^[^_]*_", "", data.id),
              NA_character_
            ),
            period = if (idx_var == 'Month') {
              ym_int <- as.integer(floor(as.numeric(Month)))
              ifelse(is.na(ym_int), NA_character_,
                     sprintf("%04d%02d",
                             1970L + ym_int %/% 12L,
                             ym_int %% 12L + 1L))
            } else
              format(Week, "%YW%V"),
            COUNT = 1L,
            SUM   = original
          ) %>%
          select(dataElement, categoryOptionCombo, orgUnit, period, COUNT, SUM) %>%
          mutate(COUNT = as.integer(COUNT), SUM = as.numeric(SUM)) %>%
          mutate_all(as.character)

        withProgress(
          message = "Rescanning dataset: element 1 of ...",
          detail  = "starting",
          value   = 0, {
            d1 = data_1(
              data,
              formula_elements = formula_elements(),
              dataSets         = dataSets(),
              dataElements     = dataElements(),
              categories       = categories(),
              ousTree          = ousTree(),
              .progress        = function(i, n, element_name, phase) {
                if (startsWith(phase, "MAD:") || startsWith(phase, "seasonal:")) {
                  scan_label <- if (startsWith(phase, "MAD:")) "MAD outliers" else "Seasonal outliers"
                  pct_str    <- sub("^(MAD|seasonal):\\s*", "", phase)
                  setProgress(
                    value   = (i - 1) / max(n, 1L),
                    message = sprintf("Rescan element %d of %d \u2014 %s: %s", i, n, scan_label, pct_str),
                    detail  = substr(element_name, 1, 55)
                  )
                } else {
                  setProgress(
                    value   = (i - 1) / n,
                    message = sprintf("Rescanning dataset: element %d of %d", i, n),
                    detail  = paste0(
                      substr(element_name, 1, 45),
                      if (nzchar(phase)) paste0(" \u2014 ", phase) else ""
                    )
                  )
                }
              }
            )
          }
        )

        cat('\n - rescan data_1 done:', nrow(d1), 'rows')

        save_file(d1, dataset.file())
        cat('\n - rescan dataset saved to', dataset.file())

        if (!is.data.table(d1)) setDT(d1)
        rescan_val(d1)

      }, ignoreInit = TRUE)

      # When scan_done_counter increments (set by cleaning_widget after saving),
      # clear rescan_val so dataset() re-reads from file on next access.
      observeEvent(scan_done_counter(), {
        if (scan_done_counter() > 0L) {
          cat('\n* data_widget scan_done_counter incremented — clearing rescan_val')
          rescan_val(NULL)
        }
      }, ignoreInit = TRUE)

      dataset = reactive({
        # req( input$dataset ) # file name from data_widget (on Dictionary tab)
        cat('\n* data_widget  dataset():')

        # Depend on scan_done_counter so dataset() re-reads after scan saves file
        sc <- scan_done_counter()

        req(input$dataset)

        file = isolate(dataset.file())

        if (!is.null(input$dataset) && file_test("-f", file)) {
          # Return cached result when file path and scan counter are unchanged.
          # This prevents "Reading data" from flashing (and replacing other modals)
          # when Shiny re-sends input$dataset with the same value.
          if (!is.null(.dw_cache$path) &&
              .dw_cache$path == file &&
              .dw_cache$sc   == sc) {
            cat('\n - returning cached dataset (same file, same scan counter)\n')
            return(.dw_cache$data)
          }

          showModal(
            modalDialog(
              title = "Reading data",
              easyClose = TRUE,
              size = 's',
              footer = NULL
            )
          )

          cat('\n - reading selected file', file)
          d = read_file(file)

          removeModal()

          # Auto-convert legacy .rds files to .qs on first load.
          # Saves a .qs copy alongside the .rds; the .rds is kept as backup.
          # On the next session the .qs will appear in the dataset list and
          # load ~10x faster with ~10x less disk space.
          if (tolower(tools::file_ext(file)) == "rds") {
            qs_file <- sub("\\.rds$", ".qs", file, ignore.case = TRUE)
            if (!file.exists(qs_file)) {
              tryCatch({
                MG2:::save_file(d, qs_file)
                showNotification(
                  paste0("Saved as ", basename(qs_file),
                         " for faster future loading. ",
                         "You can delete the .rds file."),
                  type     = "message",
                  duration = 8
                )
                cat('\n - auto-converted to .qs:', qs_file)
              }, error = function(e) {
                cat('\n - auto-convert to .qs failed:', conditionMessage(e))
              })
            }
          }

          cat('\n - done: dataset has', nrow(d), 'rows')
          .dw_cache <<- list(path = file, data = d, sc = sc)

          return(d)
        } else {
          cat('\n - dataset.file() not selected or not found')
        }
      })

      data1 = reactive({
        req(dataset.file())
        req(dataset())
        req(formula_elements())
        req(ousTree())
        cat('\n* data_widget data1')

        cat('\n -  data_widget data1() class( dataset() )', class(dataset()))

        # If rescan button was clicked, rescan_val() holds the processed result —
        # use it directly without re-running data_1().
        if (!is.null(rescan_val())) {
          cat('\n -- data1 using rescan_val()')
          return(rescan_val())
        }

        # A valid file is either:
        #   (a) a raw download — has COUNT and SUM (needs data_1() processing), or
        #   (b) an already-processed file — has effectiveLeaf and original.
        # COUNT is dropped from processed files, so checking COUNT alone is no longer
        # sufficient.  Reject only when neither signature is present.
        is_raw       <- 'COUNT' %in% names(dataset())
        is_processed <- 'effectiveLeaf' %in% names(dataset())

        if (!is_raw && !is_processed) {
          showModal(
            modalDialog(
              title = "Data is the wrong type and will not be used",
              easyClose = TRUE,
              size = 's',
              footer = '(click anywhere to continue)'
            )
          )
          return()
        }

        if (!is_processed) {
          cat('\n -- preparing data1 from raw download')

          data = dataset()

          if ('categoryOptionCombo.ids' %in% names(data)) {
            data = data %>%
              rename(categoryOptionCombo = categoryOptionCombo.ids)
          }

          # when there is no category option combo, as when downloading totals with no dissagregations,
          # the column does not exist in download and need to add it...
          if (!'categoryOptionCombo' %in% names(data)) {
            data = data %>%
              mutate(categoryOptionCombo = NA)
          }

          if ('dataElement.id' %in% names(data)) {
            data = data %>%
              select(-dataElement) %>%
              rename(dataElement = dataElement.id)
          }

          withProgress(
            message = "Preparing dataset: element 1 of ...",
            detail  = "starting",
            value   = 0, {
              d1 <- data_1(
                data,
                formula_elements = formula_elements(),
                dataSets         = dataSets(),
                dataElements     = dataElements(),
                categories       = categories(),
                ousTree          = ousTree(),
                .shiny_progress  = TRUE,
                .progress        = function(i, n, element_name, phase) {
                  if (i == 0L) {
                    setProgress(
                      value   = 0,
                      message = "Preparing dataset...",
                      detail  = phase
                    )
                  } else if (startsWith(phase, "MAD:") || startsWith(phase, "seasonal:")) {
                    scan_label <- if (startsWith(phase, "MAD:")) "MAD outliers" else "Seasonal outliers"
                    pct_str    <- sub("^(MAD|seasonal):\\s*", "", phase)
                    setProgress(
                      value   = (i - 1) / max(n, 1L),
                      message = sprintf("Element %d of %d \u2014 %s: %s", i, n, scan_label, pct_str),
                      detail  = substr(element_name, 1, 55)
                    )
                  } else {
                    setProgress(
                      value   = (i - 1) / max(n, 1L),
                      message = sprintf("Preparing dataset: element %d of %d", i, n),
                      detail  = paste0(
                        substr(element_name, 1, 45),
                        if (nzchar(phase)) paste0(" \u2014 ", phase) else ""
                      )
                    )
                  }
                }
              )
            }
          )

          cat('\n - data1 names:', names(d1))
          cat('\n - data1 rows:', nrow(d1))

          # Save the processed result so subsequent loads skip data_1() entirely
          cat('\n - saving processed dataset to', dataset.file())
          save_file(d1, dataset.file())
          showNotification("Dataset processed and saved.", type = "message", duration = 3)
        } else {
          cat('\n -- data1 already prepared')
          # as.data.frame() forces a copy so the subsequent setDT() does not
          # mutate the reactive's cached object in-place via data.table reference
          # semantics (which would corrupt dataset() on the next call).
          d1 = as.data.frame(dataset())
        }

        cat('\n - d1 class:', class(d1))

        # Add value column if missing (now added in data_1 function)
        if (!'value' %in% names(d1)) {
          cat('\n - data_widget adding value column')
          d1 = d1 %>% mutate(value = !is.na(SUM))
        }

        # Convert to data.table once here so all downstream consumers
        # (d(), cleanedData, selectedData, etc.) skip individual conversions.
        if (!is.data.table(d1)) setDT(d1)

        # Ensure Month is a properly encoded yearmonth (months-since-epoch).
        # Three cases arise in practice:
        #   (a) class stripped by setDT(), values are months-since-epoch (~360-840)
        #   (b) class stripped by setDT(), values are days-since-epoch (~10k-25k)
        #   (c) class present (preserved by RDS) but values are days-since-epoch
        #       — happens when datasets were built before yearmonth encoding was
        #       standardised (e.g. mg2_demo_processed, Mike's DRC/Zambia data)
        # The > 5000 threshold distinguishes days from months encoding.
        # Month encoding is now fixed upstream in read_file() for FST files.
        # For RDS files, yearmonth class is preserved by saveRDS/readRDS.
        # setDT() can silently drop the S3 class; restore it if missing.
        if ("Month" %in% names(d1)) {
          m_raw <- c(unclass(d1[["Month"]]))
          if (length(m_raw) > 0) {
            med <- median(m_raw, na.rm = TRUE)
            needs_fix <- !inherits(d1[["Month"]], "yearmonth") || med <= 5000
            if (needs_fix) {
              if (med <= 5000) {
                # Months-since-epoch (old encoding, e.g. from qs/fst with wrong values):
                # convert to days-since-epoch so tsibble formats dates correctly.
                yr   <- 1970L + as.integer(m_raw) %/% 12L
                mo   <- as.integer(m_raw) %% 12L + 1L
                days <- as.double(as.Date(paste0(yr, "-", sprintf("%02d", mo), "-01")) -
                                    as.Date("1970-01-01"))
                data.table::set(d1, j = "Month",
                                value = structure(days, class = c("yearmonth", "vctrs_vctr")))
              } else {
                # Days-since-epoch, class just needs restoring.
                data.table::set(d1, j = "Month",
                                value = structure(m_raw, class = c("yearmonth", "vctrs_vctr")))
              }
            }
          }
        }

        cat("\n - end d1  class/cols:\n -- ", class(d1), "\n")

        return(d1)
      })

      # dt1 = reactive({
      #   req( data1() )
      #   cat( "\n* dt1 " )
      #   cat( "\n - data1() class:" , class( data1()  ) )
      #   d1 = data1()
      #   cat( "\n - dt1 class:" , class( d1 ) )
      #   return( d1 )
      # })

      # Return ####
      return(list(
        indicator = reactive({
          input$formula
        }),
        formulas = formulas,
        formulaName = reactive({
          input$formula
        }),
        all_formula_elements = all_formula_elements,
        formula_elements = formula_elements,
        formulaFile = reactive({
          req(data.folder())
          if (is.null(input$formula.file) || input$formula.file == "") {
            paste0(data.folder(), "Formulas_", format(Sys.Date(), "%Y_%b%d"), ".xlsx")
          } else {
            paste0(data.folder(), input$formula.file)
          }
        }),
        dataset.file = reactive({
          input$dataset
        }),
        dataset = dataset,
        data1 = data1,
        scan_done_counter = scan_done_counter
        # dt1 = dt1
      ))
    }
  )
}
