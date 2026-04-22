formula_widget_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # fillCol( height = 600, flex = c(NA ) ,

  tagList(
    # add_busy_spinner(spin = "fading-circle",
    #                position = "top-right") ,

    fluidPage(
      # div(
      #         # Header text for the top of the page
      #         p( "Data elements (or indicators) associated with formula (left panel)"),
      #         style = "font-weight: bold; text-align: left; margin-bottom: 10px;" # Center alignment and spacing
      #       ),
      # div(
      #         # Header text for the top of the page
      #         p( "Use the 'Select' and 'Review' tabs here:"),
      #         style = "font-weight: bold; text-align: left; margin-bottom: 5px;" # Center alignment and spacing
      #       ),

      tabsetPanel(
        type = "tabs",

        tabPanel(
          "Review",
          # h5( "List of selected elements with a row for each disaggregation" ) ,
          fluidRow(
            column(
              7,
              div(p(
                'Use this button to delete unwanted selection',
                style = "font-size: 16px"
              ))
            ),
            column(
              2,
              actionButton(
                ns('deleteRows'),
                'Delete Selected Rows',
                width = '200px'
              )
            )
            # column( 2, actionButton( ns("saveData"), "Save Changes") )
          ),
          fluidRow(
            column(
              7,
              div(p(
                'Use this button to save changes',
                style = "font-size: 16px"
              ))
            ),
            column(
              2,
              actionButton(ns("saveData"), "Save Changes", width = '200px')
            )
          ),
          fluidRow(
            column(
              12,
              checkboxInput(
                ns("collapse_to_element"),
                "One row per data element (collapse categories)",
                value = TRUE
              )
            )
          ),
          fluidRow(
            column(
              12,
              div(
                DT::dataTableOutput(ns('forumlaDictionaryTable')),
                style = "font-size: 60%; width: 100%"
              )
            )
          ),

          selected = "Review"
        ),

        tabPanel(
          "Select",
          fluidRow(
            column(
              8,
              br(),
              p("Click on a row below to select each element:"),
              style = "font-weight: bold; text-align: left; margin-bottom: 20px;"
            ),
            column(
              4,
              selectInput(
                ns('element_indicator_choice'),
                "",
                choices = c('data elements', 'indicators'),
                selected = 'data elements'
              )
            )
          ),

          fluidRow(
            column(12, verbatimTextOutput(ns("selected")))
          ),

          fluidRow(
            column(
              12,
              div(
                DT::dataTableOutput(ns('dataElementDictionaryTable')),
                style = "font-size: 60%; width: 100%"
              )
            )
          )
        )
      )
    )
  )
} # ui

# Server function ####
formula_widget_server <- function(
  id,
  metadata_widget_output = NULL,
  data_Widget_output = NULL,
  directory_widget_output = NULL,
  formulaSaved = NULL
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      # cat('\n **** Starting formula_widget_server \n')

      # reactives to toggle login status
      dataElementDictionary = reactive({
        metadata_widget_output$dataElements()
      })
      indicators = reactive({
        metadata_widget_output$indicators()
      })
      categories = reactive({
        metadata_widget_output$categories()
      })
      formulas = reactive({
        data_Widget_output$formulas()
      })
      all_formula_elements = reactive({
        data_Widget_output$all_formula_elements()
      })
      formula_elements = reactive({
        data_Widget_output$formula_elements()
      })
      formulaName = reactive({
        data_Widget_output$formulaName()
      })
      formulaFile = reactive({
        data_Widget_output$formulaFile()
      })
      dir = reactive({
        directory_widget_output$directory()
      })

      ## Browse and Select Elements/Indicators Here ####
      formulaChoices = reactive({
        req(dataElementDictionary())

        cat('\n * formulaChoices ')

        if (input$element_indicator_choice %in% 'data elements') {
          return(dataElementDictionary())
        } else {
          # testing:
          # saveRDS( indicators()[1:5, ], "indicators.rds" )
          # saveRDS( dataElementDictionary()[0, ] , "dataElementDictionary.rds" )

          # indicators() %>% bind_cols( dataElementDictionary()[0,  c( "Categories" , "categoryOptionCombo.ids" ) ] )

          indicators() %>%
            mutate(dataElement.id = id, dataElement = name) %>%
            full_join(
              dataElementDictionary()[0, ],
              by = c("dataElement.id", "dataElement", "displayName")
            )
        }
      })

      output$dataElementDictionaryTable =
        DT::renderDT(DT::datatable(
          formulaChoices(),

          selection = 'multiple',
          rownames = FALSE,
          filter = 'top',
          options = list(
            scrollY = "55vh",
            scrollX = TRUE,
            scrollCollapse = TRUE,
            paging = TRUE,
            searching = TRUE,
            info = TRUE,
            lengthMenu = list(
              c(10, 25, 100, -1),
              list('10', '25', '100', 'All')
            ),
            pageLength = 10,
            server = TRUE,
            dom = 'tirp'
          )
        ))

      # Clear row selection when switching between data elements and indicators
      # so stale row numbers from the previous table don't go out of bounds
      observeEvent(input$element_indicator_choice, {
        DT::selectRows(DT::dataTableProxy("dataElementDictionaryTable"), NULL)
      }, ignoreInit = TRUE)

      selected_elements = reactive({
        req(input$dataElementDictionaryTable_rows_selected)

        cat('\n* formula_widget selected_elements():')

        row_selected = input$dataElementDictionaryTable_rows_selected
        cat('\n - row number selected is', row_selected)

        if (length(row_selected) >= 1) {
          selected_elements = formulaChoices()[row_selected, , drop = FALSE]
          return(selected_elements)
        } else {
          cat("\n- No formula row selected")
          return()
        }
      })

      selectedElementNames = reactive({
        # cat('\n* selectedElementNames')

        selected_categories = selected_elements() %>%
          separate_rows(Categories, categoryOptionCombo.ids, sep = ";") %>%
          mutate(
            Categories = Categories %>% str_trim,
            dataElement = dataElement %>% str_trim
          )

        a = selected_categories %>% pull(dataElement)
        b = selected_categories %>% pull(Categories)

        a. = paste0("[", format(unlist(a)), "]")
        b. = paste0("[", format(unlist(b)), "]")

        x = paste(a., b., sep = '.', collapse = ' + ')

        # cat('\n - done ')
        return(x)
      })

      output$selected = renderPrint({
        # req( selectedElementNames() )
        cat(selectedElementNames())
      })

      ## Review Selected ELements ####

      # Save once for both DT and selection reference
      # review_table_data <- updated_formula_elements$df
      #
      updated_formula_elements = reactiveValues(df = tibble())
      #
      observeEvent(updated_formula_elements$df, {
        cat(
          "\n* updated_formula_elements: there are",
          nrow(updated_formula_elements$df),
          "elements"
        )
      })

      # Clear the review table immediately when the formula name changes,
      # before formula_elements() repopulates it from the file.
      observeEvent(formulaName(), {
        req(formulaName())
        cat('\n* formulaName changed — clearing review table')
        updated_formula_elements$df <- dataElementDictionary()[0, ]
      }, ignoreInit = TRUE)

      observeEvent(formula_elements(), {
        cat("\n* updated_formula_elements = formula_elements()")

        if (nrow(formula_elements()) == 0) {
          # Testing
          # saveRDS( dataElementDictionary()[0, ]  , 'emptyDataDictionary.rds')

          cat("\n - formula_elements() is empty")

          updated_formula_elements$df = dataElementDictionary()[0, ]
        } else {
          cat("\n - setting formula_elements()")

          updated_formula_elements$df = formula_elements()
        }
      })

      observeEvent(selected_elements(), {
        cat('\n* observe selected_elements()')

        selected_categories <- selected_elements() %>%
          separate_rows(Categories, categoryOptionCombo.ids, sep = ";") %>%
          mutate(
            Categories              = Categories %>% str_trim,
            categoryOptionCombo.ids = categoryOptionCombo.ids %>% str_trim,
            Formula.Name            = formulaName()
          )

        # Merge into existing elements: drop any rows for the same dataElement.id
        # (in case the user is replacing a previous selection), then append.
        existing <- updated_formula_elements$df
        if (nrow(existing) > 0 && "dataElement.id" %in% names(existing)) {
          existing <- existing %>%
            dplyr::filter(!dataElement.id %in% selected_categories$dataElement.id)
        }

        updated_formula_elements$df <- dplyr::bind_rows(existing, selected_categories) %>%
          dplyr::arrange(dataElement) %>%
          dplyr::select(Formula.Name, everything()) %>%
          dplyr::distinct()

        cat('\n - updated_formula_elements now has', nrow(updated_formula_elements$df), 'rows')
      })

      # Collapsed view of the formula table (shared by renderDT and deleteRows)
      review_df <- reactive({
        df <- updated_formula_elements$df
        if (isTRUE(input$collapse_to_element) && nrow(df) > 0 &&
            all(c("dataElement.id", "Categories", "categoryOptionCombo.ids") %in% names(df))) {
          collapse_cols <- setdiff(
            names(df),
            c("Categories", "categoryOptionCombo.ids", "n_categoryOptions", "categoryCombo", "categoryCombo.id")
          )
          df <- df %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(collapse_cols))) %>%
            dplyr::summarise(
              Categories              = paste(trimws(Categories), collapse = " ;\n "),
              categoryOptionCombo.ids = paste(trimws(categoryOptionCombo.ids), collapse = " ;\n "),
              n_categoryOptions       = dplyr::n(),
              .groups = "drop"
            )
        }
        df
      })

      observeEvent(input$deleteRows, {
        sel <- input$forumlaDictionaryTable_rows_selected
        if (is.null(sel) || length(sel) == 0) return()

        cat("\n* deleteRows: selected rows:", sel)

        displayed <- review_df()

        if (isTRUE(input$collapse_to_element) &&
            "dataElement.id" %in% names(displayed)) {
          # Collapsed view: delete all underlying rows for each selected element
          ids_to_remove <- displayed$dataElement.id[sel]
          cat("\n - removing dataElement.ids:", ids_to_remove)
          updated_formula_elements$df <- updated_formula_elements$df %>%
            dplyr::filter(!dataElement.id %in% ids_to_remove)
        } else {
          # Expanded view: delete by row index directly
          updated_formula_elements$df <- updated_formula_elements$df[-as.numeric(sel), ]
        }
      })

      output$forumlaDictionaryTable =
        DT::renderDT({
          DT::datatable(
            review_df(),
            rownames = FALSE,
            filter = 'top',
            options = list(
              scrollY = "55vh",
              scrollX = TRUE,
              scrollCollapse = TRUE,
              paging = TRUE,
              searching = TRUE,
              info = TRUE,
              lengthMenu = list(c(5, 10, 25, -1), list('5', '10', '25', 'All')),
              pageLength = 10,
              server = TRUE,
              dom = 'tirp'
            )
          )
          # options = DToptions_no_buttons()
        })

      output$formulaName = renderPrint({
        formulaName()
      })

      # Save Formula ####

      pending_save_path <- reactiveVal(NULL)

      # Generate a unique dated filename that does not yet exist on disk
      new_dated_formula_path <- function(data_dir) {
        base <- paste0(data_dir, "Formulas_", format(Sys.Date(), "%Y_%b%d"))
        path <- paste0(base, ".xlsx")
        counter <- 1L
        while (file.exists(path)) {
          counter <- counter + 1L
          path <- paste0(base, "_", counter, ".xlsx")
        }
        path
      }

      # Core save logic — reads existing content from target_file directly
      # so it works correctly whether saving to the selected file or a new one
      perform_save <- function(target_file) {
        cat('\n* perform_save: saving formula to', target_file)

        showModal(modalDialog(
          title = "Saving formula...",
          easyClose = FALSE,
          footer = NULL,
          fade = FALSE
        ))

        tryCatch({
          wb     <- openxlsx::createWorkbook()
          sheet1 <- openxlsx::addWorksheet(wb, sheetName = "Formula")
          sheet2 <- openxlsx::addWorksheet(wb, sheetName = "Formula Elements")

          # Read existing content from the *target* file (not the currently selected one)
          existing_formulas  <- NULL
          existing_elements  <- NULL
          if (file.exists(target_file)) {
            tryCatch({
              existing_formulas <- readxl::read_excel(target_file, sheet = "Formula") %>%
                dplyr::filter(!is.na(Formula.Name))
              existing_elements <- readxl::read_excel(
                target_file, sheet = "Formula Elements", guess_max = 1e6
              )
            }, error = function(e) {
              cat('\n - could not read existing file:', conditionMessage(e))
            })
          }

          no_existing <- is.null(existing_formulas) || nrow(existing_formulas) == 0
          cat('\n - existing formulas in target:', if (no_existing) 0 else nrow(existing_formulas))

          if (no_existing) {
            new.Formula.Name     <- formulaName()
            new.Formula          <- selectedElementNames()
            new.formula_elements <- updated_formula_elements$df
          } else {
            original_formula <- existing_formulas %>%
              dplyr::filter(!Formula.Name %in% formulaName())
            original_elements <- existing_elements %>%
              dplyr::filter(!Formula.Name %in% formulaName())

            cat('\n - new formula already in file?', formulaName() %in% existing_formulas$Formula.Name)

            new.Formula.Name     <- c(formulaName(), original_formula$Formula.Name)
            new.Formula          <- c(selectedElementNames(), original_formula$Formula)
            new.formula_elements <- rbind(updated_formula_elements$df, original_elements)
          }

          openxlsx::writeDataTable(
            wb, sheet1,
            tibble::tibble(Formula.Name = new.Formula.Name, Formula = new.Formula),
            rowNames = FALSE
          )
          openxlsx::writeDataTable(wb, sheet2, new.formula_elements, rowNames = FALSE)
          openxlsx::saveWorkbook(wb, target_file, overwrite = TRUE)

          if (!is.null(formulaSaved)) formulaSaved(formulaSaved() + 1)

          removeModal()
          showNotification(
            paste0("Saved to: ", basename(target_file)),
            type = "message",
            duration = 4
          )
          cat('\n - done saving to', target_file)

        }, error = function(e) {
          removeModal()
          showNotification(
            paste0("Save failed: ", conditionMessage(e)),
            type = "error",
            duration = 8
          )
          cat('\n - save error:', conditionMessage(e))
        })
      }

      observeEvent(input$saveData, {
        if (is.null(formulaName()) || trimws(formulaName()) == "") {
          showNotification(
            "Please enter a formula name in the 'Select/Add Formula' box before saving.",
            type = "warning",
            duration = 5
          )
          return()
        }
        req(formulaFile())

        target <- formulaFile()

        if (file.exists(target)) {
          # File exists — ask whether to add to it or start a new file
          pending_save_path(target)
          showModal(modalDialog(
            title = "Save Formula",
            tags$p("This formula will be added to the currently selected file:"),
            tags$p(tags$strong(basename(target)), style = "margin-left: 10px;"),
            tags$p(
              "Would you like to add it to this file, or save to a new file?",
              style = "margin-top: 10px;"
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("save_new_file"),  "Save to New File",      class = "btn-default"),
              actionButton(ns("save_same_file"), "Add to Current File",   class = "btn-primary")
            ),
            easyClose = TRUE,
            fade = FALSE
          ))
        } else {
          # New file — save immediately with the dated default name
          perform_save(target)
        }
      })

      observeEvent(input$save_same_file, {
        removeModal()
        req(pending_save_path())
        perform_save(pending_save_path())
        pending_save_path(NULL)
      })

      observeEvent(input$save_new_file, {
        removeModal()
        req(dir())
        perform_save(new_dated_formula_path(dir()))
        pending_save_path(NULL)
      })

      # Return ####
      return(list(
        updated_formula_elements = updated_formula_elements
      ))
    }
  )
}
