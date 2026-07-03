# DT table options... ####
buttonList = function(file_name = paste('downloaded_', Sys.Date()), columns = NULL) {
  export_opts <- list(modifier = list(page = 'all'))
  if (!is.null(columns)) export_opts$columns <- columns
  list(
    'copy',
    'print',
    list(
      extend = 'collection',
      buttons = list(
        list(extend = 'csv',   filename = file_name, exportOptions = export_opts),
        list(extend = 'excel', filename = file_name, exportOptions = export_opts),
        list(extend = 'pdf',   filename = file_name, exportOptions = export_opts)
      ),
      text = 'Download'
    )
  )
}


DToptions_with_buttons = function(...) {
  list(
    autoWidth = FALSE,   # TRUE breaks column widths with Bootstrap 5 (bslib)
    scrollX = TRUE,
    lengthMenu = list(
      c(15, 25, 50, 100, -1),
      list('15', '25', '50', '100', 'All')
    ),
    pageLength = 15,
    columnDefs = list(list(className = 'dt-right', targets = "_all")),
    dom = 'l<"col-sm-6"B>fiprt',
    buttons = buttonList(...)
  )
}


DToptions_no_buttons = function(...) {
  list(
    scrollX = TRUE,
    dom = 'l<"col-sm-6"i>fprt',
    lengthMenu = list(
      c(15, 25, 50, 100, -1),
      list('15', '25', '50', '100', 'All')
    ),
    pageLength = 15,
    rownames = FALSE,
    escape = FALSE,
    selection = list(mode = 'single')
  )
}
