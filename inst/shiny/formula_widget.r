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
        cat('\n* observe selected_elements() ')

        selected_categories = selected_elements() %>%
          separate_rows(Categories, categoryOptionCombo.ids, sep = ";") %>%
          mutate(
            Categories = Categories %>% str_trim,
            categoryOptionCombo.ids = categoryOptionCombo.ids %>% str_trim,
            Formula.Name = formulaName()
            # ,zeroIsSignificant = as.logical( zeroIsSignificant )
          )

        # if ( nrow( updated_formula_elements$df ) == 0 ){

        cat(
          '\n- updated_formula_elements == ',
          nrow(selected_categories),
          "elements"
        )

        updated_formula_elements$df = selected_categories %>%
          arrange(dataElement) %>%
          select(Formula.Name, everything()) %>%
          distinct()

        # } else {
        #
        #   # if selectected elements in updated_formula_elements, remove
        #   if ( any( selected_categories$dataElement %in% updated_formula_elements$df$dataElement ) ){
        #
        #       cat('\n - removing' , nrow( selected_categories ), 'selected elements' )
        #
        #       updated_formula_elements$df = anti_join( updated_formula_elements$df , selected_categories,
        #                                                by = "dataElement" )
        #
        #   } else {. # or else add them
        #       cat('\n - adding' , nrow( selected_categories ), 'selected elements' )
        #
        #       updated_formula_elements$df = bind_rows( updated_formula_elements$df , selected_categories ) %>%
        #         arrange( dataElement ) %>%
        #         select( Formula.Name, everything() ) %>%
        #         distinct()
        #     }

        # }
      })

      observeEvent(input$deleteRows, {
        cat("\n* delete row:")

        if (!is.null(input$forumlaDictionaryTable_rows_selected)) {
          cat("\n - delete row:", input$forumlaDictionaryTable_rows_selected)

          updated_formula_elements$df = updated_formula_elements$df[
            -as.numeric(input$forumlaDictionaryTable_rows_selected),
          ]
        }
      })

      output$forumlaDictionaryTable =
        DT::renderDT({
          df = updated_formula_elements$df

          if (isTRUE(input$collapse_to_element) && nrow(df) > 0 &&
              all(c("dataElement.id", "Categories", "categoryOptionCombo.ids") %in% names(df))) {
            collapse_cols = setdiff(
              names(df),
              c("Categories", "categoryOptionCombo.ids", "n_categoryOptions", "categoryCombo", "categoryCombo.id")
            )
            df = df %>%
              dplyr::group_by(dplyr::across(dplyr::all_of(collapse_cols))) %>%
              dplyr::summarise(
                Categories            = paste(trimws(Categories), collapse = " ;\n "),
                categoryOptionCombo.ids = paste(trimws(categoryOptionCombo.ids), collapse = " ;\n "),
                n_categoryOptions     = dplyr::n(),
                .groups = "drop"
              )
          }

          DT::datatable(
            df,
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

      observeEvent(input$saveData, {
        req(formulaFile())
        req(formulaName())

        cat('\n* saveData: saving formula to', formulaFile())

        showModal(modalDialog(
          title = "Saving formula...",
          easyClose = FALSE,
          footer = NULL,
          fade = FALSE
        ))

        tryCatch({
          wb <- openxlsx::createWorkbook()
          sheet1 <- openxlsx::addWorksheet(wb, sheetName = "Formula")
          sheet2 <- openxlsx::addWorksheet(wb, sheetName = "Formula Elements")

          no_existing_formulas = (is.null(formulas()) || nrow(formulas()) == 0)

          cat('\n - original formulas have', if (no_existing_formulas) 0 else nrow(formulas()), 'rows')

          if (no_existing_formulas) {
            cat('\n - preparing new formula')
            new.Formula.Name = formulaName()
            new.Formula     = selectedElementNames()
            new.formula_elements = updated_formula_elements$df
          } else {
            cat('\n - adding/replacing formula in existing file')

            original_formula = formulas() %>%
              filter(!Formula.Name %in% formulaName())

            cat(
              '\n - new formula already in file?',
              formulaName() %in% formulas()$Formula.Name
            )

            original_formula_elements = all_formula_elements() %>%
              filter(!Formula.Name %in% formulaName())

            cat('\n - original formula elements have', nrow(all_formula_elements()), 'rows')

            new.Formula.Name = c(formulaName(), original_formula$Formula.Name)
            new.Formula      = c(selectedElementNames(), original_formula$Formula)
            new.formula_elements = rbind(updated_formula_elements$df, original_formula_elements)
          }

          cat('\n - writing Formula sheet')
          openxlsx::writeDataTable(
            wb, sheet1,
            tibble(Formula.Name = new.Formula.Name, Formula = new.Formula),
            rowNames = FALSE
          )

          cat('\n - writing Formula Elements sheet')
          openxlsx::writeDataTable(wb, sheet2, new.formula_elements, rowNames = FALSE)

          openxlsx::saveWorkbook(wb, formulaFile(), overwrite = TRUE)

          if (!is.null(formulaSaved)) formulaSaved(formulaSaved() + 1)

          removeModal()
          showNotification(
            paste0("Saved to: ", basename(formulaFile())),
            type = "message",
            duration = 4
          )
          cat('\n - done saving to', formulaFile())

        }, error = function(e) {
          removeModal()
          showNotification(
            paste0("Save failed: ", conditionMessage(e)),
            type = "error",
            duration = 8
          )
          cat('\n - save error:', conditionMessage(e))
        })
      })

      # Return ####
      return(list(
        updated_formula_elements = updated_formula_elements
      ))
    }
  )
}
