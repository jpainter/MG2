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

          # Formula name banner
          fluidRow(
            column(12,
              uiOutput(ns("formula_name_banner"))
            )
          ),

          # Row-level actions
          fluidRow(
            column(3,
              actionButton(ns('deleteRows'),  'Delete Selected Rows', width = '180px')
            ),
            column(2,
              actionButton(ns("saveData"), "Save Changes", width = '150px', class = "btn-primary")
            ),
            column(7,
              div(
                style = "display:flex; gap:6px; justify-content:flex-end;",
                actionButton(ns("rename_formula"), "Rename Formula", class = "btn-default btn-sm"),
                actionButton(ns("rename_file"),    "Rename File",    class = "btn-default btn-sm"),
                actionButton(ns("delete_formula"), "Delete Formula", class = "btn-danger  btn-sm")
              )
            )
          ),

          br(),

          fluidRow(
            column(6,
              checkboxInput(
                ns("collapse_to_element"),
                "One row per data element (collapse categories)",
                value = TRUE
              )
            ),
            column(6,
              div(
                style = "display:flex; justify-content:flex-end; align-items:center;",
                actionButton(
                  ns("check_related"),
                  "Check for Related Elements",
                  class = "btn-info btn-sm"
                )
              )
            )
          ),

          fluidRow(column(12, uiOutput(ns("secondary_note")))),

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
      validationRules = reactive({
        if (!is.null(metadata_widget_output)) metadata_widget_output$validationRules()
      })
      data1 = reactive({
        data_Widget_output$data1()
      })

      # Counts per data column value (element_category) in the current dataset
      element_row_counts = reactive({
        d <- tryCatch(data1(), error = function(e) NULL)
        if (is.null(d) || nrow(d) == 0 || !"data" %in% names(d)) return(NULL)
        dt <- data.table::as.data.table(d)
        counts <- dt[!is.na(data), .N, by = data]
        tibble::tibble(data_name = counts$data, n_rows = counts$N)
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
          cat("\n - formula_elements() is empty")
          updated_formula_elements$df = dataElementDictionary()[0, ]
        } else {
          cat("\n - setting formula_elements()")
          df <- formula_elements()
          if (!"role" %in% names(df)) df[["role"]] <- "primary"
          updated_formula_elements$df = df
        }
      })

      observeEvent(selected_elements(), {
        cat('\n* observe selected_elements()')

        selected_categories <- selected_elements() %>%
          separate_rows(Categories, categoryOptionCombo.ids, sep = ";") %>%
          mutate(
            Categories              = Categories %>% str_trim,
            categoryOptionCombo.ids = categoryOptionCombo.ids %>% str_trim,
            Formula.Name            = formulaName(),
            role                    = "primary"
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
        # Sort by role → element name → category
        sort_cols <- intersect(c("role", "dataElement", "Categories"), names(df))
        if (length(sort_cols) > 0)
          df <- dplyr::arrange(df, dplyr::across(dplyr::all_of(sort_cols)))

        # role first; Formula.Name hidden from display (kept in underlying df)
        if ("role" %in% names(df)) df <- dplyr::select(df, role, dplyr::everything())
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
          df <- review_df()

          # Hide Formula.Name from display (it's in the underlying df for save logic)
          display_df <- dplyr::select(df, -dplyr::any_of("Formula.Name"))

          # Join row counts from the current dataset when available,
          # then put "Rows in dataset" first and "role" second
          counts <- element_row_counts()
          if (!is.null(counts) && all(c("dataElement", "Categories") %in% names(display_df))) {
            display_df <- tryCatch({
              display_df %>%
                dplyr::mutate(
                  .key = dplyr::if_else(
                    is.na(Categories) | !nzchar(trimws(sub(" ;.*", "", Categories))),
                    trimws(dataElement),
                    paste(trimws(dataElement), trimws(sub(" ;.*", "", Categories)), sep = "_")
                  )
                ) %>%
                dplyr::left_join(
                  dplyr::rename(counts, .key = data_name, `Rows in dataset` = n_rows),
                  by = ".key"
                ) %>%
                dplyr::select(-.key) %>%
                dplyr::select(
                  dplyr::any_of("Rows in dataset"),
                  dplyr::any_of("role"),
                  dplyr::everything()
                )
            }, error = function(e) {
              cat('\n - element_row_counts join error:', conditionMessage(e))
              display_df
            })
          }

          role_col_idx <- if ("role" %in% names(display_df)) which(names(display_df) == "role") - 1L else NULL

          dt <- DT::datatable(
            display_df,
            rownames = FALSE,
            filter = 'top',
            editable = if (!is.null(role_col_idx))
              list(target = "cell", disable = list(columns = setdiff(seq_along(names(display_df)) - 1L, role_col_idx)))
            else
              FALSE,
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
          # Colour-code the role column
          if (!is.null(role_col_idx)) {
            dt <- DT::formatStyle(
              dt, "role",
              backgroundColor = DT::styleEqual(
                c("primary", "secondary"),
                c("#e8f5e9", "#fff3e0")
              )
            )
          }
          dt
        })

      # Handle in-cell role edits (column index is relative to display_df, not full df)
      observeEvent(input$forumlaDictionaryTable_cell_edit, {
        info     <- input$forumlaDictionaryTable_cell_edit
        df       <- review_df()
        disp_df  <- dplyr::select(df, -dplyr::any_of("Formula.Name"))
        if (!"role" %in% names(disp_df)) return()
        role_col_idx <- which(names(disp_df) == "role") - 1L
        if (info$col != role_col_idx) return()

        new_val <- trimws(as.character(info$value))
        if (!new_val %in% c("primary", "secondary")) {
          showNotification("Role must be 'primary' or 'secondary'.", type = "warning", duration = 3)
          return()
        }

        # Map the displayed row back to updated_formula_elements$df rows
        full_df <- updated_formula_elements$df
        if (isTRUE(input$collapse_to_element) && "dataElement.id" %in% names(df)) {
          # Collapsed view: update all category rows for the selected element
          element_id <- df$dataElement.id[info$row]
          full_df[full_df$dataElement.id == element_id, "role"] <- new_val
        } else {
          full_df[info$row, "role"] <- new_val
        }
        updated_formula_elements$df <- full_df
      })

      output$formulaName = renderPrint({
        formulaName()
      })

      # Formula name banner above the table ####
      output$formula_name_banner <- renderUI({
        nm <- formulaName()
        if (is.null(nm) || !nzchar(trimws(nm))) return(NULL)
        div(
          style = paste0(
            "background:#e8f4fd; padding:8px 14px;",
            " border-left:4px solid #2196F3; margin:0 0 8px 0; border-radius:3px;"
          ),
          tags$strong(style = "color:#1565C0; font-size:1.1em;",
                      paste0("Formula: ", nm))
        )
      })

      # Secondary note banner ####
      output$secondary_note <- renderUI({
        df <- updated_formula_elements$df
        if (is.null(df) || nrow(df) == 0 || !"role" %in% names(df)) return(NULL)
        n_sec <- sum(df$role == "secondary", na.rm = TRUE)
        if (n_sec == 0) return(NULL)
        div(
          style = paste0(
            "background:#fff3cd; padding:8px 14px;",
            " border-left:4px solid #ffc107; margin:6px 0 6px 0; border-radius:3px;"
          ),
          tags$strong(style = "color:#856404;",
            paste0(n_sec, " secondary category row(s) detected. ",
                   "Secondary elements inform reporting completeness ",
                   "but are excluded from totals by default."))
        )
      })

      # Check for Related Elements ####
      # Stores candidate rows from dataElementDictionary pending user confirmation
      related_candidates <- reactiveVal(NULL)

      observeEvent(input$check_related, {
        vr  <- validationRules()
        fe  <- updated_formula_elements$df

        if (is.null(vr) || nrow(vr) == 0) {
          showNotification(
            "No validation rules available. Fetch metadata first.",
            type = "warning", duration = 4
          )
          return()
        }
        if (is.null(fe) || nrow(fe) == 0 || !"dataElement.id" %in% names(fe)) {
          showNotification("No elements in formula yet.", type = "warning", duration = 3)
          return()
        }

        fe_de_uids <- unique(na.omit(fe$dataElement.id))
        uid_pattern <- paste(fe_de_uids, collapse = "|")

        rule_hits <- vr[
          grepl(uid_pattern, vr$leftSide_expression_raw,  fixed = FALSE) |
          grepl(uid_pattern, vr$rightSide_expression_raw, fixed = FALSE),
        ]

        if (nrow(rule_hits) == 0) {
          showNotification(
            "No validation rules link formula elements to other elements.",
            type = "message", duration = 4
          )
          return()
        }

        uid_re <- "[A-Za-z][A-Za-z0-9]{10}"
        all_exprs <- na.omit(c(rule_hits$leftSide_expression_raw,
                                rule_hits$rightSide_expression_raw))
        raw_uids <- unlist(regmatches(all_exprs, gregexpr(uid_re, all_exprs)))
        new_de_uids <- setdiff(unique(raw_uids), fe_de_uids)

        de_dict <- dataElementDictionary()
        candidates <- de_dict[de_dict$dataElement.id %in% new_de_uids, , drop = FALSE]

        if (nrow(candidates) == 0) {
          showNotification(
            "All related elements are already in the formula.",
            type = "message", duration = 3
          )
          return()
        }

        related_candidates(candidates)

        # Force-expand the review table
        updateCheckboxInput(session, "collapse_to_element", value = FALSE)

        # Build rule context labels
        rule_labels <- purrr::map_chr(seq_len(nrow(candidates)), function(i) {
          uid <- candidates$dataElement.id[i]
          matched_rules <- rule_hits[
            grepl(uid, rule_hits$leftSide_expression_raw,  fixed = TRUE) |
            grepl(uid, rule_hits$rightSide_expression_raw, fixed = TRUE),
          ]
          if (nrow(matched_rules) == 0) return("")
          paste0(" (rule: ", paste(matched_rules$name, collapse = "; "), ")")
        })

        choice_labels <- paste0(candidates$dataElement, rule_labels)

        showModal(modalDialog(
          title = "Related Elements via Validation Rules",
          tags$p(
            paste0("Found ", nrow(rule_hits), " rule(s) linking formula elements to ",
                   nrow(candidates), " additional element(s). ",
                   "Select elements to add as secondary:")
          ),
          checkboxGroupInput(
            ns("related_elements_select"),
            label = NULL,
            choiceNames  = choice_labels,
            choiceValues = candidates$dataElement.id,
            selected     = candidates$dataElement.id
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("add_related"), "Add Selected as Secondary", class = "btn-primary")
          ),
          easyClose = TRUE,
          fade = FALSE
        ))
      })

      observeEvent(input$add_related, {
        removeModal()
        sel_ids <- input$related_elements_select
        if (is.null(sel_ids) || length(sel_ids) == 0) return()

        candidates  <- related_candidates()
        if (is.null(candidates)) return()

        new_elements <- candidates[candidates$dataElement.id %in% sel_ids, , drop = FALSE]

        new_rows <- new_elements %>%
          tidyr::separate_rows(Categories, categoryOptionCombo.ids, sep = ";") %>%
          dplyr::mutate(
            Categories              = trimws(Categories),
            categoryOptionCombo.ids = trimws(categoryOptionCombo.ids),
            Formula.Name            = formulaName(),
            role                    = "secondary"
          )

        existing <- updated_formula_elements$df
        if (nrow(existing) > 0 && "dataElement.id" %in% names(existing)) {
          existing <- dplyr::filter(existing, !dataElement.id %in% new_rows$dataElement.id)
        }

        updated_formula_elements$df <- dplyr::bind_rows(existing, new_rows) %>%
          dplyr::arrange(role, dataElement) %>%
          dplyr::select(Formula.Name, dplyr::everything()) %>%
          dplyr::distinct()

        related_candidates(NULL)
        showNotification(
          paste0("Added ", nrow(new_rows), " secondary row(s) to formula."),
          type = "message", duration = 3
        )
      })

      # Save Formula ####

      # Build the Formula string from the current review df, independent of
      # browse-table row selection (fixing the "Save failed" bug where
      # selectedElementNames() required a row to be selected in the browse table)
      .formula_string_from_df <- function(df) {
        if (is.null(df) || nrow(df) == 0) return("")
        rows <- tryCatch(
          df %>%
            tidyr::separate_rows(Categories, categoryOptionCombo.ids, sep = ";") %>%
            dplyr::mutate(
              dataElement = trimws(dataElement),
              Categories  = trimws(ifelse(is.na(Categories), "", Categories))
            ),
          error = function(e) df
        )
        if (nrow(rows) == 0) return("")
        a. <- paste0("[", format(rows$dataElement), "]")
        b. <- paste0("[", format(rows$Categories),  "]")
        paste(paste(a., b., sep = "."), collapse = " + ")
      }

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

          current_formula_str <- .formula_string_from_df(updated_formula_elements$df)

          if (no_existing) {
            new.Formula.Name     <- formulaName()
            new.Formula          <- current_formula_str
            new.formula_elements <- updated_formula_elements$df
          } else {
            original_formula <- existing_formulas %>%
              dplyr::filter(!Formula.Name %in% formulaName())
            original_elements <- existing_elements %>%
              dplyr::filter(!Formula.Name %in% formulaName())

            # Legacy files may not have a role column — default to primary
            if (!"role" %in% names(original_elements))
              original_elements[["role"]] <- "primary"

            cat('\n - new formula already in file?', formulaName() %in% existing_formulas$Formula.Name)

            new.Formula.Name     <- c(formulaName(), original_formula$Formula.Name)
            new.Formula          <- c(current_formula_str, original_formula$Formula)
            new.formula_elements <- dplyr::bind_rows(updated_formula_elements$df, original_elements)
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

      # Delete Formula ####
      observeEvent(input$delete_formula, {
        nm <- formulaName()
        if (is.null(nm) || !nzchar(trimws(nm))) {
          showNotification("No formula selected.", type = "warning", duration = 3)
          return()
        }
        showModal(modalDialog(
          title = "Delete Formula",
          tags$p(paste0("Delete formula '", nm, "'?")),
          tags$p("All its elements will be removed from the file. This cannot be undone.",
                 style = "color:#c0392b;"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_delete_formula"), "Delete", class = "btn-danger")
          ),
          easyClose = TRUE, fade = FALSE
        ))
      })

      observeEvent(input$confirm_delete_formula, {
        removeModal()
        nm <- formulaName()
        req(formulaFile())
        target <- formulaFile()

        tryCatch({
          existing_formulas <- readxl::read_excel(target, sheet = "Formula") %>%
            dplyr::filter(!is.na(Formula.Name), !Formula.Name %in% nm)
          existing_elements <- readxl::read_excel(
            target, sheet = "Formula Elements", guess_max = 1e6
          ) %>% dplyr::filter(!Formula.Name %in% nm)

          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Formula")
          openxlsx::addWorksheet(wb, "Formula Elements")
          openxlsx::writeDataTable(wb, 1, existing_formulas, rowNames = FALSE)
          openxlsx::writeDataTable(wb, 2, existing_elements, rowNames = FALSE)
          openxlsx::saveWorkbook(wb, target, overwrite = TRUE)

          updated_formula_elements$df <- dataElementDictionary()[0, ]
          if (!is.null(formulaSaved)) formulaSaved(formulaSaved() + 1)

          showNotification(paste0("Formula '", nm, "' deleted."),
                           type = "message", duration = 4)
        }, error = function(e) {
          showNotification(paste0("Delete failed: ", conditionMessage(e)),
                           type = "error", duration = 6)
        })
      })

      # Rename Formula ####
      observeEvent(input$rename_formula, {
        nm <- formulaName()
        if (is.null(nm) || !nzchar(trimws(nm))) {
          showNotification("No formula selected.", type = "warning", duration = 3)
          return()
        }
        showModal(modalDialog(
          title = "Rename Formula",
          textInput(ns("rename_formula_input"), "New name:", value = nm, width = "100%"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_rename_formula"), "Rename & Save", class = "btn-primary")
          ),
          easyClose = TRUE, fade = FALSE
        ))
      })

      # Store old/new name across the dataset-rename confirmation step
      pending_formula_rename <- reactiveValues(old = NULL, new = NULL)

      observeEvent(input$confirm_rename_formula, {
        removeModal()
        old_nm  <- formulaName()
        new_nm  <- trimws(input$rename_formula_input)
        if (!nzchar(new_nm) || new_nm == old_nm) return()
        req(formulaFile())
        target <- formulaFile()

        tryCatch({
          existing_formulas <- readxl::read_excel(target, sheet = "Formula") %>%
            dplyr::filter(!is.na(Formula.Name)) %>%
            dplyr::mutate(Formula.Name = dplyr::if_else(Formula.Name == old_nm, new_nm, Formula.Name))
          existing_elements <- readxl::read_excel(
            target, sheet = "Formula Elements", guess_max = 1e6
          ) %>%
            dplyr::mutate(Formula.Name = dplyr::if_else(Formula.Name == old_nm, new_nm, Formula.Name))

          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Formula")
          openxlsx::addWorksheet(wb, "Formula Elements")
          openxlsx::writeDataTable(wb, 1, existing_formulas, rowNames = FALSE)
          openxlsx::writeDataTable(wb, 2, existing_elements, rowNames = FALSE)
          openxlsx::saveWorkbook(wb, target, overwrite = TRUE)

          df <- updated_formula_elements$df
          if ("Formula.Name" %in% names(df)) df$Formula.Name <- new_nm
          updated_formula_elements$df <- df

          if (!is.null(formulaSaved)) formulaSaved(formulaSaved() + 1)

          # Check for dataset files containing the old formula name
          data_dir <- dir()
          matching_files <- character(0)
          if (!is.null(data_dir) && nzchar(data_dir)) {
            all_rds <- list.files(data_dir, pattern = "\\.rds$", full.names = FALSE)
            matching_files <- all_rds[grepl(old_nm, all_rds, fixed = TRUE)]
          }

          if (length(matching_files) > 0) {
            pending_formula_rename$old <- old_nm
            pending_formula_rename$new <- new_nm
            showModal(modalDialog(
              title = "Rename Associated Datasets?",
              tags$p(paste0(
                "Found ", length(matching_files),
                " dataset file(s) whose name contains '", old_nm, "':"
              )),
              tags$ul(lapply(matching_files, tags$li)),
              tags$p("Rename them to use '", new_nm, "' instead?"),
              footer = tagList(
                actionButton(ns("skip_dataset_rename"),  "No, keep file names", class = "btn-default"),
                actionButton(ns("confirm_dataset_rename"), "Yes, rename datasets", class = "btn-primary")
              ),
              easyClose = FALSE, fade = FALSE
            ))
          } else {
            showNotification(paste0("Renamed to '", new_nm, "'. Select it in the dropdown."),
                             type = "message", duration = 5)
          }
        }, error = function(e) {
          showNotification(paste0("Rename failed: ", conditionMessage(e)),
                           type = "error", duration = 6)
        })
      })

      observeEvent(input$skip_dataset_rename, {
        removeModal()
        nm <- pending_formula_rename$new
        pending_formula_rename$old <- NULL
        pending_formula_rename$new <- NULL
        showNotification(paste0("Formula renamed to '", nm, "'. Select it in the dropdown."),
                         type = "message", duration = 5)
      })

      observeEvent(input$confirm_dataset_rename, {
        removeModal()
        old_nm   <- pending_formula_rename$old
        new_nm   <- pending_formula_rename$new
        data_dir <- dir()
        pending_formula_rename$old <- NULL
        pending_formula_rename$new <- NULL

        if (is.null(old_nm) || is.null(data_dir)) return()

        all_rds <- list.files(data_dir, pattern = "\\.rds$", full.names = FALSE)
        to_rename <- all_rds[grepl(old_nm, all_rds, fixed = TRUE)]

        n_ok  <- 0L
        n_err <- 0L
        for (f in to_rename) {
          new_f <- gsub(old_nm, new_nm, f, fixed = TRUE)
          tryCatch({
            file.rename(file.path(data_dir, f), file.path(data_dir, new_f))
            n_ok <- n_ok + 1L
          }, error = function(e) {
            n_err <- n_err + 1L
          })
        }

        if (!is.null(formulaSaved)) formulaSaved(formulaSaved() + 1)
        msg <- paste0("Formula renamed to '", new_nm, "'. ",
                      n_ok, " dataset file(s) renamed",
                      if (n_err > 0) paste0("; ", n_err, " failed") else "",
                      ". Select the formula from the dropdown.")
        showNotification(msg, type = "message", duration = 7)
      })

      # Rename Formula File ####
      observeEvent(input$rename_file, {
        req(formulaFile())
        old_base <- tools::file_path_sans_ext(basename(formulaFile()))
        showModal(modalDialog(
          title = "Rename Formula File",
          textInput(ns("rename_file_input"), "New file name (no extension):",
                    value = old_base, width = "100%"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_rename_file"), "Rename File", class = "btn-primary")
          ),
          easyClose = TRUE, fade = FALSE
        ))
      })

      observeEvent(input$confirm_rename_file, {
        removeModal()
        req(formulaFile())
        new_base <- trimws(input$rename_file_input)
        if (!nzchar(new_base)) return()

        old_path <- formulaFile()
        new_path <- file.path(dirname(old_path), paste0(new_base, ".xlsx"))

        if (old_path == new_path) return()
        if (file.exists(new_path)) {
          showNotification("A file with that name already exists.", type = "error", duration = 5)
          return()
        }

        tryCatch({
          file.rename(old_path, new_path)
          if (!is.null(formulaSaved)) formulaSaved(formulaSaved() + 1)
          showNotification(
            paste0("File renamed to '", basename(new_path), "'. Select it from the file list."),
            type = "message", duration = 6
          )
        }, error = function(e) {
          showNotification(paste0("File rename failed: ", conditionMessage(e)),
                           type = "error", duration = 6)
        })
      })

      # Return ####
      return(list(
        updated_formula_elements = updated_formula_elements
      ))
    }
  )
}
