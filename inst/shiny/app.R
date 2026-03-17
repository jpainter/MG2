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
source("dqa_widget.R")         # Tab: DQA
source("reporting_widget.r")   # Tab: Reporting
source("cleaning_widget.r")    # Tab: Outliers
source("evaluation_widget_2.R")# Tab: Evaluation
source("regions_widget.R")     # Tab: Regions
source("map_widget.R")         # Auxiliary: map display
source("facilities_widget.r")  # Auxiliary: facility table

shinyjs::useShinyjs()

# User Interface ------------------------------------------------------------
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "yeti"),
  shinyWidgets::setBackgroundColor(color = "#F5F5F5"),

  titlePanel(h1(
    "Magic Glasses 2",
    style = "background-color:#61A1FA; padding-left:15px;"
  )),

  navlistPanel(
    widths = c(1, 11),
    id = "tabs",

    tabPanel(
      "Welcome",
      br(),
      h2("An epidemiological look at DHIS2 data"),
      br(),
      h4("The goal is to make the analysis of routine data accessible, transparent, and repeatable."),
      br(),
      p("The layout follows a research path using the pages (navigation at left) to:"),
      p("- Understand which data are available ", span("(Metadata)", style = "color:blue")),
      p("- Request data ", span("(Data)", style = "color:blue")),
      p("- Get an overview of data quality ", span("(DQA)", style = "color:blue")),
      p("- Adjust for reporting bias ", span("(Reporting)", style = "color:blue")),
      p("- Scan for outliers ", span("(Outliers)", style = "color:blue")),
      p("- Evaluate trends and estimate intervention effectiveness ",
        span("(Evaluation)", style = "color:blue")),
      br(),
      p("(Note: The layout of each page depends on your browser.",
        "Adjust screen layout and text size with Ctrl- or Ctrl+)"),
      p(paste("MG2 package version", utils::packageVersion("MG2")))
    ),

    tabPanel(
      "Setup",
      fluidPage(fluidRow(
        column(6, directory_widget_ui("directory1")),
        column(6, login_widget_ui("login1"))
      ))
    ),

    tabPanel("Metadata", metadata_widget_ui("metadata1")),

    tabPanel("Regions", regions_widget_ui("regions1")),

    tabPanel(
      "Data",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Formula",
          fluidRow(
            column(5, data_widget_ui("data1")),
            column(7, formula_widget_ui("formula1"))
          )
        ),
        tabPanel("Download", data_request_widget_ui("data_request1"))
      )
    ),

    tabPanel("DQA",       dqa_widget_ui("dqa1")),
    tabPanel("Reporting", reporting_widget_ui("reporting1"),    value = "reporting"),
    tabPanel("Outliers",  cleaning_widget_ui("cleaning1"),      value = "outliers"),
    tabPanel("Evaluation",evaluation_widget_ui("evaluation1"),  value = "evaluation")
  )
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
  data1_Widget_output <- data_widget_server(
    "data1",
    directory_widget_output  = directory_widget_output,
    metadata_widget_output   = metadata_widget_output,
    data_request_output      = data_request_output
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

  formula1_Widget_output <- formula_widget_server(
    "formula1",
    metadata_widget_output   = metadata_widget_output,
    data_Widget_output       = data1_Widget_output,
    directory_widget_output  = directory_widget_output
  )

  reporting_trigger <- reactive({ input$tabs == "reporting" })

  reporting_widget_output <- reporting_widget_server(
    "reporting1",
    trigger                  = reporting_trigger,
    dataDirectory            = directory_widget_output,
    metadata_widget_output   = metadata_widget_output,
    data_widget_output       = data1_Widget_output,
    cleaning_widget_output   = cleaning_widget_output
  )

  cleaning_widget_output <- cleaning_widget_server(
    "cleaning1",
    directory_widget_output  = directory_widget_output,
    metadata_widget_output   = metadata_widget_output,
    data_widget_output       = data1_Widget_output,
    reporting_widget_output  = reporting_widget_output
  )

  dqa_widget_output <- dqa_widget_server(
    "dqa1",
    directory_widget_output  = directory_widget_output,
    metadata_widget_output   = metadata_widget_output,
    data_widget_output       = data1_Widget_output,
    reporting_widget_output  = reporting_widget_output,
    cleaning_widget_output   = cleaning_widget_output
  )

  evaluation_widget_output <- evaluation_widget_server(
    "evaluation1",
    directory_widget_output  = directory_widget_output,
    metadata_widget_output   = metadata_widget_output,
    data_widget_output       = data1_Widget_output,
    reporting_widget_output  = reporting_widget_output,
    cleaning_widget_output   = cleaning_widget_output
  )
}

# Run -----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
