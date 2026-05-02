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

    br()
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

        cat('\n - selecting formula')
        all_formula_elements() %>%
          filter(Formula.Name %in% input$formula)
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

        file.type = 'rds' # input$file.type
        # file.other = ifelse( input$cleaned %in% "Cleaned" , '_Seasonal' , "" )  # input$file.other

        # file.keywords = input$file.keywords # '_formulaData|Seasonal|dts|rds'
        file.keywords = 'rds'

        data.files = dir.files[
          # grepl( 'All levels' , dir.files ) &
          grepl(file.type, dir.files, ignore.case = T) &
            # grepl( file.other, dir.files, fixed = TRUE  ) &
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

        showModal(modalDialog(
          title = "Preparing raw data for rescan.  Just a moment...",
          easyClose = TRUE, size = 's',
          footer = '(click anywhere to continue)'
        ))

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
            period = if (idx_var == 'Month')
              format(as.Date(Month), "%Y%m")
            else
              format(Week, "%YW%V"),
            COUNT = 1L,
            SUM   = original
          ) %>%
          select(dataElement, categoryOptionCombo, orgUnit, period, COUNT, SUM) %>%
          mutate(COUNT = as.integer(COUNT), SUM = as.numeric(SUM)) %>%
          mutate_all(as.character)

        d1 = data_1(
          data,
          formula_elements = formula_elements(),
          dataSets         = dataSets(),
          dataElements     = dataElements(),
          categories       = categories(),
          ousTree          = ousTree()
        )

        cat('\n - rescan data_1 done:', nrow(d1), 'rows')
        removeModal()

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
        scan_done_counter()

        req(input$dataset)

        file = isolate(dataset.file())

        if (!is.null(input$dataset) && file_test("-f", file)) {
          showModal(
            modalDialog(
              title = "Reading data",
              easyClose = TRUE,
              size = 's',
              footer = NULL
            )
          )

          cat('\n - reading selected rds file', file)
          d = readRDS(file)

          removeModal()

          cat('\n - done: dataset has', nrow(d), 'rows')

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
          showModal(
            modalDialog(
              title = "Preparing raw data for analysis.  Just a moment...",
              easyClose = TRUE,
              size = 's',
              footer = '(click anywhere to continue)'
            )
          )

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

          d1 = data_1(
            data,
            formula_elements = formula_elements(),
            dataSets = dataSets(),
            dataElements = dataElements(),
            categories = categories(),
            ousTree = ousTree()
          )

          cat('\n - data1 names:', names(d1))
          cat('\n - data1 rows:', nrow(d1))

          removeModal()
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
          req(input$formula)
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
