directory_widget_ui = function(id) {
  ns <- NS(id)

  tagList(
    hr(),

    h3('Step 1.  Provide directory for data files:'),

    fluidRow(
      column(
        12,
        selectizeInput(
          ns("data.directory"),
          label   = NULL,
          choices = NULL,
          width   = "100%",
          options = list(
            create       = TRUE,
            createOnBlur = TRUE,
            placeholder  = "Select a recent directory or paste/type a path..."
          )
        )
      )
    ),

    fluidRow(
      column(
        3,
        shinyFiles::shinyDirButton(
          ns("folder"),
          label     = "Browse...",
          title     = "Select data directory",
          icon      = icon("folder-open"),
          style     = "width: 100%"
        )
      )
    ),

    # hr() ,
    h4('This directory Contains:'),

    tableOutput(ns('folderInfo')),

    # hr()
  ) # end tagList
} # ui


directory_widget_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      config_path <- path.expand("~/.mg2_config.rds")

      # Load directory history (up to 10); support both old single-dir and new list format
      load_dir_history <- function() {
        cfg <- tryCatch(readRDS(config_path), error = function(e) list())
        dirs <- if (!is.null(cfg$directories)) cfg$directories else if (!is.null(cfg$directory)) cfg$directory else character(0)
        dirs[nchar(dirs) > 0 & dir.exists(dirs)]
      }

      save_dir_history <- function(new_dir, existing) {
        # Normalize: strip trailing slash before storing
        new_dir  <- sub("/$", "", new_dir)
        existing <- sub("/$", "", existing)
        updated  <- unique(c(new_dir, existing[nchar(existing) > 0]))
        tryCatch(
          saveRDS(list(directories = head(updated, 10)), config_path),
          error = function(e) NULL
        )
        head(updated, 10)
      }

      dir_history <- reactiveVal(load_dir_history())

      # Populate selectize with history on startup
      isolate({
        dirs <- dir_history()
        updateSelectizeInput(session, "data.directory",
          choices  = dirs,
          selected = if (length(dirs) > 0) dirs[1] else NULL
        )
      })

      # Volumes available to the folder picker — use only known-accessible roots
      # to avoid macOS Full Disk Access blocks from shinyFiles::getVolumes()
      volumes <- c(Home = path.expand("~"))

      icloud <- file.path(path.expand("~"), "Library/Mobile Documents/com~apple~CloudDocs")
      if (dir.exists(icloud)) volumes <- c("iCloud Drive" = icloud, volumes)

      onedrive <- file.path(path.expand("~"), "OneDrive")
      if (dir.exists(onedrive)) volumes <- c("OneDrive" = onedrive, volumes)

      shinyFiles::shinyDirChoose(input, "folder", roots = volumes, session = session)

      # When user picks a folder via Browse, add to selectize and select it
      observeEvent(input$folder, {
        if (!is.integer(input$folder)) {
          path <- shinyFiles::parseDirPath(volumes, input$folder)
          if (length(path) > 0 && nchar(path) > 0) {
            cat('\n directory_widget - folder chosen:', path, '\n')
            new_choices <- unique(c(path, dir_history()))
            updateSelectizeInput(session, "data.directory",
              choices  = new_choices,
              selected = path
            )
          }
        }
      })


      data.folder = reactive({
        cat('\n* data.folder:\n')
        req(input$data.directory)

        data.dir <- input$data.directory

        # Ensure path ends with a separator
        if (!endsWith(data.dir, "/") && !endsWith(data.dir, "\\")) {
          data.dir <- paste0(data.dir, "/")
        }

        cat('\n - data.folder is ', data.dir, '\n')
        return(data.dir)
      })

      # Persist chosen directory and update history
      observeEvent(data.folder(), {
        new_history <- save_dir_history(data.folder(), dir_history())
        dir_history(new_history)
      })

      data.dir.files = reactive({
        req(data.folder())
        if (!dir.exists(data.folder())) {
          return(NULL)
        }
        dir.files = list.files(data.folder())
        return(dir.files)
      })

      folderInfo = reactive({
        req(data.folder())
        # there are a couple forms of metadata in the api.  This code tests the format, then gets metadata
        # if available, use resources method
        cat('\n folder info : ', data.folder())

        d = data.folder()
        data.dir.files = base::dir(d)

        if (length(data.dir.files) == 0) {
          return(tibble(`File type:` = c("Metadata", "Formula", "Data"),
                        Number = 0L, `Most Recent` = "—"))
        }

        finf = map_df(data.dir.files, ~ file.info(paste(d, .x, sep = "/")))

        # Testing
        # save(d, data.dir.files, finf, file = "finf.rda")

        metadata.files = grepl("metadata", data.dir.files, ignore.case = TRUE)
        formula.files = grepl("formulas", data.dir.files, ignore.case = TRUE)
        data.files = grepl("rds", data.dir.files, ignore.case = TRUE) &
          !grepl("metadata", data.dir.files, ignore.case = TRUE)

        max_date <- function(times) {
          if (length(times) == 0 || all(is.na(times))) return("—")
          as.character(date(max(times, na.rm = TRUE)))
        }

        info = tibble(
          `File type:` = c("Metadata", 'Formula', 'Data'),
          Number = c(sum(metadata.files), sum(formula.files), sum(data.files)),
          `Most Recent` = c(
            max_date(finf$mtime[metadata.files]),
            max_date(finf$mtime[formula.files]),
            max_date(finf$mtime[data.files])
          )
        )

        cat('\n directory widget folderInfo completed')
        return(info)
      })

      output$folderInfo = renderTable(folderInfo())

      metadata.files = reactive({
        req(data.folder())
        cat('- looking for metadata in:', data.folder(), '\n')

        dir.files = data.dir.files()

        # file.type = '.xl' # input$file.type
        file.type = '.rds'
        file.other = "metadata"

        search.index =
          grepl(file.type, dir.files, ignore.case = TRUE) &
          grepl(file.other, dir.files, ignore.case = TRUE)

        if (!any(search.index)) {
          cat('- no metadata files in directory \n')
          return(NULL)
        }

        mf = dir.files[search.index]

        # Arrange by modified date
        mf.mdate = file.info(paste0(data.folder(), mf))$mtime
        mf = mf[rev(order(mf.mdate))]

        cat('-', length(mf), 'metadata files \n')

        cat('mf:', mf, '\n')

        return(mf)
      })

      geofeatures.files = reactive({
        req(data.folder())
        cat(
          '\n* geofeatures.files: looking for geoFeatures in:',
          data.folder(),
          '\n'
        )

        dir.files = data.dir.files()

        file.type = '.rds' # input$file.type
        file.other = "geofeatures"

        search.index =
          grepl(file.type, dir.files, ignore.case = TRUE) &
          grepl(file.other, dir.files, ignore.case = TRUE)

        if (!any(search.index)) {
          cat('- no geoFeatures files in directory \n')
          return(NULL)
        }

        gf = dir.files[search.index]

        # Arrange by modified date
        gf.mdate = file.info(paste0(data.folder(), gf))$mtime
        gf = gf[rev(order(gf.mdate))]

        cat('-', length(gf), 'geofeatures files \n')

        cat('gf:', gf, '\n')

        return(gf)
      })

      rds_data_files = reactive({
        req(data.dir.files())
        cat('- looking for rds files in:', data.folder(), '\n')

        dir.files = data.dir.files()

        file.type = 'rds' # input$file.type
        file.other = "cleaned|seasonal|data"

        search.index =
          grepl(file.type, dir.files, ignore.case = TRUE) &
          grepl(file.other, dir.files, ignore.case = TRUE)

        data.files = paste0(data.folder(), dir.files[search.index])

        if (!any(file.exists(data.files))) {
          return(NULL)
        }

        cat("-data file exists: \n")
        return(data.files)
      })

      # Update list of data files
      observe({
        cat('\n directory_widget -updating metadata file list \n')
        updateSelectInput(
          session,
          'metadataFiles',
          choices = metadata.files(),
          selected = 1
        )
      })

      observe({
        cat('\n directory_widget -updating geofeatures file list \n')
        updateSelectInput(
          session,
          'geofeturesFiles',
          choices = geofeatures.files()
        )
      })

      observe({
        cat('\n directory_widget -updating .rds file list \n')
        updateSelectInput(session, 'datasetFiles', choices = rds_data_files())
      })

      return(list(
        directory = data.folder,
        metadata.files = metadata.files,
        geofeatures.files = geofeatures.files
      ))
    }
  )
}
