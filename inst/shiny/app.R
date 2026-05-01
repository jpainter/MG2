# Magic Glasses 2 — Shiny Application
# Launch with: MG2::run_mg2()
#
# This file is the entry point for the Shiny app bundled in inst/shiny/.
# All analysis functions are provided by the MG2 package (loaded via run_mg2).
# This file sources only the Shiny module (widget) files.

# Core Shiny packages -------------------------------------------------------
library(shiny)
library(shinyjs)
library(shinybusy)
library(shinyFiles)
library(shinyWidgets)
library(bslib)
library(DT)

# Data manipulation ---------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(lubridate)
library(magrittr)
library(rlang)

# Date/time -----------------------------------------------------------------
library(tsibble)
library(zoo)

# File I/O and API ----------------------------------------------------------
library(readxl)
library(httr)
library(jsonlite)

# Visualization (required by multiple widgets) ------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) library(ggplot2)
if (requireNamespace("plotly",  quietly = TRUE)) library(plotly)

# Optional packages for full functionality ----------------------------------
# These are loaded silently; widgets that need them will show errors if missing.
optional_pkgs <- c(
  "flextable", "officer", "openxlsx",        # document generation
  "sf", "leaflet", "leaflegend", "mapview",   # geospatial
  "feasts", "fable", "fabletools",            # time-series modeling
  "fable.prophet", "prophet", "forecast",    # forecasting
  "future", "furrr", "promises",             # async/parallel
  "data.table", "tidyfast",                  # performance
  "patchwork", "dygraphs", "GGally",         # visualization
  "slider", "sugrrants",                     # rolling windows / calendars
  "data.tree", "igraph",                     # hierarchical data
  "progress", "progressr", "tictoc",         # progress reporting
  "hrbrthemes", "RColorBrewer", "scales",    # plot themes/colors
  "assertthat", "digest"                     # utilities
)
for (pkg in optional_pkgs) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, warn.conflicts = FALSE)
    )
  }
}

# Re-attach dplyr after optional packages to ensure its functions are not masked
# (some optional packages, e.g. forecast, can mask dplyr::filter with stats::filter)
suppressPackageStartupMessages(library(dplyr))

# Options -------------------------------------------------------------------
options(dplyr.summarise.inform = FALSE)
options(future.globals.maxSize = 30 * 1024^3)

# Source Shiny modules ------------------------------------------------------
# Each file defines a *_ui() and *_server() function for one app tab/panel.

source("DToptions.R")          # DataTable styling helpers (used by many widgets)
source("chart_module.R")       # Reusable ggplot chart module

source("directory_widget.R")   # Tab: Setup — data directory
source("login_widget.R")       # Tab: Setup — DHIS2 login
source("metadata_widget.R")    # Tab: Metadata
source("data_widget.r")        # Tab: Data — formula/dataset selector
source("formula_widget.r")     # Tab: Data — formula builder
source("data_request_widget.R")# Tab: Data — download from DHIS2
source("combine_widget.R")     # Tab: Data — combine/derive datasets
source("dqa_widget.R")         # Tab: DQA
source("reporting_widget.r")   # Tab: Reporting
source("cleaning_widget.r")    # Tab: Outliers
source("evaluation_widget_2.R")# Tab: Evaluation
source("regions_widget.R")     # Tab: Regions
source("about_widget.R")       # Tab: About
# map_widget.R and facilities_widget.r are not standalone modules;
# they are embedded within other widgets and sourced there when needed.

# User Interface ------------------------------------------------------------
ui <- bslib::page_navbar(
  title = span("Magic Glasses 2", style = "color:#61A1FA; font-weight:bold;"),
  id    = "tabs",
  theme = bslib::bs_theme(bootswatch = "yeti"),
  bg    = "#222222",
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(
      "body { min-width: 1100px; }
       table.dataTable td { vertical-align: top !important; }"
    )),
    shinyWidgets::setBackgroundColor(color = "#F5F5F5")
  ),

  bslib::nav_panel(
    "Welcome",
    br(),
    h2("An epidemiological look at DHIS2 data"),
    br(),
    h4("The goal is to make the analysis of routine data accessible, transparent, and repeatable."),
    br(),
    p("The layout follows a research path:"),
    p("- Understand which data are available ", span("(Metadata)", style = "color:blue")),
    p("- Request data ", span("(Data)", style = "color:blue")),
    p("- Get an overview of data quality ", span("(DQA)", style = "color:blue")),
    p("- Adjust for reporting bias ", span("(Reporting)", style = "color:blue")),
    p("- Scan for outliers ", span("(Outliers)", style = "color:blue")),
    p("- Evaluate trends and estimate intervention effectiveness ",
      span("(Evaluation)", style = "color:blue")),
    br(),
    p("(Adjust screen layout and text size with Ctrl- or Ctrl+)"),
    p(paste("MG2 package version", utils::packageVersion("MG2")))
  ),

  bslib::nav_panel(
    "Setup",
    fluidRow(
      column(6, directory_widget_ui("directory1")),
      column(6, login_widget_ui("login1"))
    )
  ),

  bslib::nav_panel("Metadata", metadata_widget_ui("metadata1")),

  bslib::nav_panel("Regions", regions_widget_ui("regions1")),

  bslib::nav_menu(
    "Data",
    bslib::nav_panel(
      "Formula",
      fluidRow(
        column(5, data_widget_ui("data1")),
        column(7, formula_widget_ui("formula1"))
      )
    ),
    bslib::nav_panel("Download", data_request_widget_ui("data_request1")),
    bslib::nav_panel("Combine",  combine_widget_ui("combine1"))
  ),

  bslib::nav_panel("DQA",        dqa_widget_ui("dqa1")),
  bslib::nav_panel("Reporting",  reporting_widget_ui("reporting1")),
  bslib::nav_panel("Outliers",   cleaning_widget_ui("cleaning1")),
  bslib::nav_panel("Evaluation", evaluation_widget_ui("evaluation1")),
  bslib::nav_spacer(),
  bslib::nav_panel("About",      about_widget_ui("about"))
)

# Server --------------------------------------------------------------------
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)

  directory_widget_output <- directory_widget_server("directory1")

  login_widget_output <- login_widget_server(
    "login1",
    directory_widget_output = directory_widget_output
  )

  metadata_widget_output <- metadata_widget_server(
    "metadata1",
    login_widget_output      = login_widget_output,
    directory_widget_output  = directory_widget_output
  )

  # data1_Widget_output must be defined before regions_widget_output because
  # regions_widget_server receives it as a lazy argument evaluated at reactive time.
  formulaSaved <- reactiveVal(0)

  data1_Widget_output <- data_widget_server(
    "data1",
    directory_widget_output  = directory_widget_output,
    metadata_widget_output   = metadata_widget_output,
    data_request_output      = data_request_output,
    formulaSaved             = formulaSaved
  )

  data_request_output <- data_request_widget_server(
    "data_request1",
    loginDetails             = login_widget_output,
    dataDirectory            = directory_widget_output,
    metadata_widget_output   = metadata_widget_output,
    data_widget_output       = data1_Widget_output,
    regions_widget_output    = regions_widget_output
  )

  regions_widget_output <- regions_widget_server(
    "regions1",
    directory_widget_output  = directory_widget_output,
    metadata_widget_output   = metadata_widget_output,
    data_widget_output       = data1_Widget_output
  )

  combine_widget_server(
    "combine1",
    directory_widget_output = directory_widget_output,
    metadata_widget_output  = metadata_widget_output
  )

  formula1_Widget_output <- formula_widget_server(
    "formula1",
    metadata_widget_output   = metadata_widget_output,
    data_Widget_output       = data1_Widget_output,
    directory_widget_output  = directory_widget_output,
    formulaSaved             = formulaSaved
  )

  current_tab <- reactive({ input$tabs })

  reporting_widget_output <- reporting_widget_server(
    "reporting1",
    dataDirectory            = directory_widget_output,
    metadata_widget_output   = metadata_widget_output,
    data_widget_output       = data1_Widget_output,
    cleaning_widget_output   = cleaning_widget_output,
    regions_widget_output    = regions_widget_output,
    current_tab              = current_tab
  )

  cleaning_widget_output <- cleaning_widget_server(
    "cleaning1",
    directory_widget_output  = directory_widget_output,
    metadata_widget_output   = metadata_widget_output,
    data_widget_output       = data1_Widget_output,
    reporting_widget_output  = reporting_widget_output,
    regions_widget_output    = regions_widget_output,
    current_tab              = current_tab
  )

  dqa_widget_output <- dqa_widget_server(
    "dqa1",
    directory_widget_output  = directory_widget_output,
    metadata_widget_output   = metadata_widget_output,
    data_widget_output       = data1_Widget_output,
    reporting_widget_output  = reporting_widget_output,
    cleaning_widget_output   = cleaning_widget_output,
    regions_widget_output    = regions_widget_output,
    current_tab              = current_tab
  )

  evaluation_widget_output <- evaluation_widget_server(
    "evaluation1",
    directory_widget_output  = directory_widget_output,
    metadata_widget_output   = metadata_widget_output,
    data_widget_output       = data1_Widget_output,
    reporting_widget_output  = reporting_widget_output,
    cleaning_widget_output   = cleaning_widget_output,
    regions_widget_output    = regions_widget_output,
    current_tab              = current_tab
  )
}

# Run -----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
