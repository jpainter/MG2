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
  "sf", "leaflet", "leaflegend",              # geospatial
  "fable", "fabletools",                     # time-series modeling
  "fable.prophet", "prophet", "forecast",    # forecasting
  "future", "furrr", "promises",             # async/parallel
  "data.table",                              # performance
  "slider",                                  # rolling windows
  "data.tree",                               # hierarchical data
  "progress", "progressr",                   # progress reporting
  "RColorBrewer", "scales",                  # plot themes/colors
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

# UI helper: small next-step hint bar at the bottom of a tab
.mg2_step_hint <- function(text) {
  div(
    style = paste0(
      "margin-top:24px; padding:8px 16px; background:#f0f4ff;",
      " border-left:4px solid #4a90d9; border-radius:3px; color:#555; font-size:13px;"
    ),
    icon("circle-info", style = "color:#4a90d9; margin-right:6px;"),
    text
  )
}

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
source("chat_widget.R")        # Tab: Assistant (AI chat, requires ellmer + shinychat)
source("burden_widget.R")      # Tab: Burden Estimate (under development)
# map_widget.R and facilities_widget.r are not standalone modules;
# they are embedded within other widgets and sourced there when needed.

# User Interface ------------------------------------------------------------
ui <- bslib::page_navbar(
  title = span("Magic Glasses 2", style = "color:#61A1FA; font-weight:bold;"),
  id    = "tabs",
  theme = bslib::bs_theme(bootswatch = "yeti", base_font_size = "16px"),
  bg    = "#222222",
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$style(
        "body { min-width: 1100px; }
         table.dataTable td { vertical-align: top !important; }
         .leaflet-container svg,
         .leaflet-pane svg,
         .leaflet-overlay-pane svg { overflow: visible !important; }
         @keyframes mg2-glow {
           0%,100% { box-shadow: 0 0 0 0 rgba(74,144,217,0); }
           50%      { box-shadow: 0 0 6px 3px rgba(74,144,217,0.6); }
         }
         .next-step-glow {
           animation: mg2-glow 2s ease-in-out infinite;
           border-radius: 4px;
           font-weight: 600 !important;
           color: #4a90d9 !important;
         }"
      )
    ),
    shinyWidgets::setBackgroundColor(color = "#F5F5F5")
  ),

  bslib::nav_panel(
    "Welcome",
    br(),
    h2("An epidemiological look at DHIS2 data"),
    br(),
    h4("The goal is to make the analysis of routine health data accessible, transparent, and repeatable."),
    br(),
    p(tags$strong("Getting started:"), " Go to ",
      tags$strong(tags$span("Setup", style = "color:blue")),
      " to connect to a DHIS2 country instance or load a demo dataset."),
    br(),
    p(tags$strong("The analysis follows a research path:")),
    p(tags$span("→", style = "color:#888; margin-right:4px;"),
      span("Metadata", style = "color:blue; font-weight:bold"),
      " — explore available data elements, org units, and validation rules"),
    p(tags$span("→", style = "color:#888; margin-right:4px;"),
      span("Regions", style = "color:blue; font-weight:bold"),
      " — filter analysis to selected regions or org units"),
    p(tags$span("→", style = "color:#888; margin-right:4px;"),
      span("Data", style = "color:blue; font-weight:bold"),
      " — build a formula and download data from DHIS2"),
    p(tags$span("→", style = "color:#888; margin-right:4px;"),
      span("DQA", style = "color:blue; font-weight:bold"),
      " — assess reporting completeness, outliers, and consistency"),
    p(tags$span("→", style = "color:#888; margin-right:4px;"),
      span("Reporting", style = "color:blue; font-weight:bold"),
      " — adjust for reporting bias using champion facilities"),
    p(tags$span("→", style = "color:#888; margin-right:4px;"),
      span("Outliers", style = "color:blue; font-weight:bold"),
      " — detect and review flagged data values"),
    p(tags$span("→", style = "color:#888; margin-right:4px;"),
      span("Evaluation", style = "color:blue; font-weight:bold"),
      " — model trends and estimate intervention effectiveness"),
    br(),
    p(tags$em("Under development:"),
      span(" Burden Estimate", style = "color:#e67e00; font-weight:bold"),
      " — population-level burden estimation from routine data;",
      span(" Assistant", style = "color:#e67e00; font-weight:bold"),
      " — AI-powered analysis assistant (requires API key)."),
    br(),
    p(tags$small("Adjust screen layout and text size with Ctrl− / Ctrl+")),
    p(paste0("MG2 package version ", utils::packageVersion("MG2"),
             local({
               # Prefer git commit date (works when running from source repo);
               # fall back to DESCRIPTION Date field when installed without git.
               git_date <- tryCatch(
                 trimws(system2("git", c("log", "-1", "--format=%cd", "--date=short"),
                                stdout = TRUE, stderr = FALSE)),
                 error = function(e) character(0)
               )
               d <- if (length(git_date) == 1 && nzchar(git_date)) git_date
                    else tryCatch(utils::packageDescription("MG2")$Date, error = function(e) NULL)
               if (!is.null(d) && nzchar(d %||% "")) paste0(" (", d, ")") else ""
             })))
  ),

  bslib::nav_panel(
    "Setup",
    fluidRow(
      column(6, directory_widget_ui("directory1")),
      column(6, login_widget_ui("login1"))
    ),
    uiOutput("setup_next_btn")
  ),

  bslib::nav_panel(
    "Metadata",
    metadata_widget_ui("metadata1"),
    .mg2_step_hint("→ Next: go to Regions — choose to analyse data nationally or sub-nationally — then go to Data to select and load a dataset.")
  ),

  bslib::nav_panel(
    "Regions",
    regions_widget_ui("regions1"),
    .mg2_step_hint("→ Next: go to Data to select and load a dataset for analysis.")
  ),

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

  bslib::nav_panel("DQA",        uiOutput("dqa_empty_hint"),       dqa_widget_ui("dqa1")),
  bslib::nav_panel("Reporting",  uiOutput("reporting_empty_hint"), reporting_widget_ui("reporting1")),
  bslib::nav_panel("Outliers",   cleaning_widget_ui("cleaning1")),
  bslib::nav_panel("Evaluation", evaluation_widget_ui("evaluation1")),
  bslib::nav_panel(
    tagList(
      "Burden Estimate",
      tags$span("dev", style = paste0(
        "background:#ffc107; color:#000; font-size:0.55em; font-weight:600;",
        " padding:2px 6px; border-radius:3px; vertical-align:middle; margin-left:5px;"
      ))
    ),
    value = "Burden",
    burden_widget_ui("burden1")
  ),
  bslib::nav_panel(
    tagList(
      "Assistant",
      tags$span("dev", style = paste0(
        "background:#ffc107; color:#000; font-size:0.55em; font-weight:600;",
        " padding:2px 6px; border-radius:3px; vertical-align:middle; margin-left:5px;"
      ))
    ),
    value = "Assistant",
    chat_widget_ui("chat1")
  ),
  bslib::nav_spacer(),
  bslib::nav_panel("About",      about_widget_ui("about"))
)

# Server --------------------------------------------------------------------
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)

  # Dependency check — show notifications for warnings on startup
  local({
    deps <- check_mg2_dependencies()
    for (w in deps$warnings) {
      showNotification(w, type = "warning", duration = 12)
    }
  })

  # Shared signal: login_widget sets this when "Load Demo Data" is clicked;
  # directory_widget watches it to update the directory input automatically.
  demo_dir <- reactiveVal(NULL)

  # nav_goto: login_widget sets this to a tab name to trigger navigation.
  nav_goto <- reactiveVal(NULL)
  observeEvent(nav_goto(), {
    req(!is.null(nav_goto()))
    updateNavbarPage(session, "tabs", selected = nav_goto())
    nav_goto(NULL)
  })

  directory_widget_output <- directory_widget_server("directory1", demo_dir = demo_dir)

  login_widget_output <- login_widget_server(
    "login1",
    directory_widget_output = directory_widget_output,
    demo_dir                = demo_dir,
    nav_goto                = nav_goto
  )

  # Setup tab: "Next →" button once a data directory is set (#2)
  output$setup_next_btn <- renderUI({
    req(directory_widget_output$directory())
    div(
      style = "margin-top:24px; padding:8px 16px;",
      actionButton(
        "setup_go_next",
        "→ Browse Metadata",
        icon  = icon("arrow-right"),
        class = "btn-primary"
      )
    )
  })
  observeEvent(input$setup_go_next, {
    updateNavbarPage(session, "tabs", selected = "Metadata")
  })

  # Empty-state hints for DQA and Reporting (#5)
  .no_data_hint <- function(next_tab) {
    div(
      class = "alert alert-info",
      style = "margin:16px;",
      icon("circle-info", style = "margin-right:6px;"),
      "No dataset loaded yet. Go to the ",
      tags$strong(next_tab), " tab to select and load a dataset."
    )
  }
  output$dqa_empty_hint <- renderUI({
    d <- tryCatch(data1_Widget_output$data1(), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) .no_data_hint("Data") else NULL
  })
  output$reporting_empty_hint <- renderUI({
    d <- tryCatch(data1_Widget_output$data1(), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) .no_data_hint("Data") else NULL
  })

  # Navbar highlight: pulse the next recommended tab via CSS (#4)
  # Highlights "Data" when directory is set but no data loaded yet.
  observe({
    has_dir  <- !is.null(directory_widget_output$directory()) &&
                nzchar(directory_widget_output$directory())
    has_data <- tryCatch(
      !is.null(data1_Widget_output$data1()) && nrow(data1_Widget_output$data1()) > 0,
      error = function(e) FALSE
    )
    css_id <- if (has_dir && !has_data) "tab-Data" else if (has_data) "tab-DQA" else NULL
    shinyjs::runjs(sprintf('
      document.querySelectorAll(".nav-link.next-step-glow").forEach(
        function(el){ el.classList.remove("next-step-glow"); }
      );
      if ("%s" !== "null") {
        var els = document.querySelectorAll(".nav-link");
        els.forEach(function(el){
          if (el.textContent.trim().startsWith("%s"))
            el.classList.add("next-step-glow");
        });
      }
    ', ifelse(is.null(css_id), "null", css_id),
       ifelse(is.null(css_id), "null",
              if (!is.null(css_id) && css_id == "tab-Data") "Data" else "DQA")))
  })

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

  burden_widget_server(
    "burden1",
    directory_widget_output = directory_widget_output,
    metadata_widget_output  = metadata_widget_output,
    data_widget_output      = data1_Widget_output,
    reporting_widget_output = reporting_widget_output,
    regions_widget_output   = regions_widget_output,
    current_tab             = current_tab
  )

  chat_widget_server(
    "chat1",
    data_widget_output      = data1_Widget_output,
    reporting_widget_output = reporting_widget_output,
    cleaning_widget_output  = cleaning_widget_output,
    metadata_widget_output  = metadata_widget_output,
    current_tab             = current_tab
  )

  about_widget_server("about")
}

# Run -----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
