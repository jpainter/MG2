# Burden Estimate module
# UI: burden_widget_ui(id)
# Server: burden_widget_server(id, ...)

# ── UI ────────────────────────────────────────────────────────────────────────

burden_widget_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinybusy::add_busy_spinner(spin = "fading-circle", position = "bottom-right"),

    # Under-development banner (no emoji — clean text only)
    div(
      style = paste0(
        "background:#fff3cd; border:1px solid #ffc107; border-radius:4px;",
        " padding:7px 14px; margin-bottom:10px;"
      ),
      tags$strong("Under Development"),
      tags$span(
        style = "font-size:0.88em; color:#555; margin-left:8px;",
        "Methods and outputs are being refined.  Results are experimental."
      )
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3,

        # ── Sidebar tabs: Data | Model ──────────────────────────────────
        tabsetPanel(
          type = "tabs",

          tabPanel(
            "Data",
            br(),
            selectInput(
              ns("target_elements"),
              label   = "Target element(s):",
              choices = NULL, multiple = TRUE, selectize = TRUE, width = "100%"
            ),
            selectInput(
              ns("attendance_elements"),
              label   = "Attendance element(s) — B & E:",
              choices = NULL, multiple = TRUE, selectize = TRUE, width = "100%"
            ),
            selectInput(
              ns("tested_elements"),
              label   = "Patients tested — E:",
              choices = NULL, multiple = TRUE, selectize = TRUE, width = "100%"
            ),
            selectInput(
              ns("population_element"),
              label   = "Population (optional, per-100k):",
              choices = NULL, multiple = FALSE, selectize = TRUE, width = "100%"
            ),
            uiOutput(ns("cat_map_ui")),
            uiOutput(ns("date_range_display"))
          ),

          tabPanel(
            "Model",
            br(),
            checkboxGroupInput(
              ns("methods"),
              label    = NULL,
              choices  = c(
                "A — Champion multiple"        = "A",
                "B — Attendance-based"         = "B",
                "C1 — Linear imputation"       = "C1",
                "C2 — ARIMA imputation"        = "C2",
                "E — Adj. Corrected Incidence" = "E"
              ),
              selected = "A",
              width    = "100%"
            ),
            conditionalPanel(
              condition = sprintf("input['%s'].indexOf('E') >= 0", ns("methods")),
              div(
                style = "padding:6px; background:#e8f4e8; border-radius:4px; margin-bottom:6px; font-size:0.85em;",
                tags$small(tags$em("Method E: Thwing et al. (2020) AJTMH 102:811-820")),
                br(),
                tags$small(tags$strong("β"), " fever proportion:"),
                fluidRow(
                  column(6, numericInput(ns("beta_young"), "< 5 yrs", value = 0.73,
                                         min = 0.1, max = 0.99, step = 0.01, width = "100%")),
                  column(6, numericInput(ns("beta_old"),   "≥ 5 yrs", value = 0.57,
                                         min = 0.1, max = 0.99, step = 0.01, width = "100%"))
                ),
                tags$small(tags$strong("α"), " TPR ratio:"),
                numericInput(ns("alpha_e"), NULL, value = 0.48,
                             min = 0.1, max = 1.0, step = 0.01, width = "100%"),
                tags$small(tags$strong("γ"), " NMF episodes/person/yr:"),
                fluidRow(
                  column(6, numericInput(ns("gamma_young"), "< 5 yrs", value = 2.0,
                                         min = 0.1, max = 10, step = 0.1, width = "100%")),
                  column(6, numericInput(ns("gamma_old"),   "≥ 5 yrs", value = 1.0,
                                         min = 0.1, max = 10, step = 0.1, width = "100%"))
                ),
                tags$small(tags$strong("λmin"), ":"),
                numericInput(ns("lambdamin"), NULL, value = 0.75,
                             min = 0.1, max = 1.0, step = 0.05, width = "100%")
              )
            ),
            checkboxInput(ns("run_d"), "D — Care-seeking adjustment", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("run_d")),
              div(
                style = "padding-left:10px; font-size:0.9em;",
                p(style = "margin-bottom:4px; color:#555;", "Care-seeking proportions:"),
                fluidRow(
                  column(6, numericInput(ns("cs_young"), "Younger (%)", value = 90,
                                         min = 1, max = 100, step = 1, width = "100%")),
                  column(6, numericInput(ns("cs_old"),   "Older (%)",   value = 70,
                                         min = 1, max = 100, step = 1, width = "100%"))
                )
              )
            )
          )
        ),

        hr(),

        actionButton(ns("run"), "Run Estimates",
                     class = "btn-primary btn-block", style = "width:100%;"),
        br(),
        div(
          style = paste0(
            "background:#f8f9fa; border:1px solid #dee2e6; border-radius:4px;",
            " padding:6px; max-height:180px; overflow-y:auto;",
            " font-size:0.78em; font-family:monospace;"
          ),
          verbatimTextOutput(ns("progress_log"), placeholder = TRUE)
        )
      ),

      mainPanel(
        width = 9,

        bslib::navset_tab(
          id = ns("burden_tab"),

          # ── Results: single combined table ──────────────────────────
          bslib::nav_panel(
            "Results",
            br(),
            uiOutput(ns("results_hint")),
            DT::DTOutput(ns("combined_table"))
          ),

          # ── Map (own tab) ────────────────────────────────────────────
          bslib::nav_panel(
            "Map",
            fluidRow(
              column(3,
                selectInput(ns("map_method"), "Method:",
                            choices = NULL, width = "100%")
              ),
              column(3,
                selectInput(ns("map_category"), "Category:",
                            choices = NULL, width = "100%")
              ),
              column(3,
                sliderInput(
                  ns("sig_threshold"),
                  label  = "Significance threshold:",
                  min = 0.80, max = 0.99, value = 0.90, step = 0.01,
                  width = "100%", ticks = FALSE
                )
              ),
              column(3,
                checkboxInput(ns("show_groups"), "Color by group", value = FALSE),
                conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns("show_groups")),
                  sliderInput(
                    ns("n_groups"), label = NULL,
                    min = 2, max = 5, value = 3, step = 1,
                    width = "100%", ticks = FALSE,
                    post = " groups"
                  )
                )
              )
            ),
            div(
              style = "display:flex; align-items:center; gap:10px; margin-bottom:4px;",
              div(
                style = "font-size:0.82em; color:#666; flex:1;",
                "Click a region to compare against all others (red = higher, green = lower)."
              ),
              actionButton(
                ns("reset_map"), "Reset map",
                class = "btn-sm btn-default",
                icon  = shiny::icon("rotate-left")
              )
            ),
            leaflet::leafletOutput(ns("burden_map"), height = "70vh"),
            uiOutput(ns("idiopleth_label"))
          ),

          bslib::nav_panel(
            "Methods",

            div(
              style = "max-width:820px; padding:16px; font-size:0.95em; line-height:1.6;",

              h4("Burden Estimation Methods"),

              p("Routine facility data are incomplete for three reasons: some facilities
                never report, reporting facilities under-count cases, and some persons
                never seek care. These methods estimate true burden by correcting each
                source in turn — from most approximate (A) to most precise (C2)
                — with an optional care-seeking adjustment (D)."),

              p(tags$strong("Champion facilities"), " are high-quality reporters
                designated in the Reporting module and serve as the reference
                standard throughout. All estimates target annual totals stratified
                by category (e.g. age group).  Each method generates B = 1,000
                bootstrap samples; the mean and 2.5th / 97.5th percentiles
                are reported as the point estimate and 95% uncertainty interval.
                Final values are integers."),

              hr(),

              h5("Method A — Champion Multiple"),

              p("Assume every non-champion facility has the same annual total as
                a typical champion facility in the same region."),

              tags$ul(
                tags$li("Let C = champion facilities in region r, T", tags$sub("c"),
                  " = annual total at champion c, N", tags$sub("nc"),
                  " = number of non-champion facilities."),
                tags$li("Draw B = 1,000 samples {s₁, …, s₂} from
                  the empirical distribution of {T", tags$sub("c"), "}."),
                tags$li("For each sample b:  ",
                  tags$strong("Êₛ = Σ Tᶜ + N₏ᶜ × sₛ")),
                tags$li("Report mean(Ê), 2.5th and 97.5th percentiles."),
                tags$li("If no champions in region r, champion distribution is drawn
                  from all national champions.")
              ),

              hr(),

              h5("Method B — Attendance-Based Champion Multiple"),

              p("Use each facility’s outpatient attendance as a covariate
                to predict its case count."),

              tags$ul(
                tags$li("Fit a log–log regression on champion facilities: ",
                  tags$strong("log(Yᶜ) = α + β·log(Aᶜ) + ε"),
                  ", ε ~ N(0, σ²)."),
                tags$li("For each non-champion facility, draw B predicted values: ",
                  tags$strong("ŷ₏ᶜ,ₛ = exp(α̂ + β̂·log(A₏ᶜ) + εₛ)")),
                tags$li("Region total: ",
                  tags$strong("Êₛ = Σ Yᶜ + Σ ŷ₏ᶜ,ₛ")),
                tags$li("The log–log model ensures all predictions are positive.
                  The regression residual SD σ̂ propagates into every sample."),
                tags$li("If attendance categories differ from target categories,
                  specify the mapping using the controls in the sidebar."),
                tags$li("Facilities without attendance data fall back to Method A
                  resampling.")
              ),

              hr(),

              h5("Method C1 — Facility Imputation (Linear)"),

              p("Impute missing months within each facility’s own time
                series using linear regression, then sum to annual totals."),

              tags$ul(
                tags$li("For each non-champion facility, fit: ",
                  tags$strong("yₘ = α + β·ḿₘ + ε"),
                  ", where ḿₘ is the monthly mean across champion
                  facilities in the same region.  The model is fit on all
                  observed months across the full history of the facility."),
                tags$li("Missing months in the target year are replaced by
                  B draws from N(ŷₘ, σ̂²), truncated at zero."),
                tags$li("Annual total per sample: ",
                  tags$strong("Tₛ = Σₘ yₘ,ₛ"),
                  " (observed months use actual values)."),
                tags$li("Facilities with fewer than 3 observed months cannot be
                  modeled and are flagged for fallback to Method B.")
              ),

              hr(),

              h5("Method C2 — Facility Imputation (ARIMA)"),

              p("Same as C1 but replaces the linear model with a seasonal ARIMA
                to capture monthly patterns."),

              tags$ul(
                tags$li("Fits ", tags$strong("auto.arima"),
                  " on the facility’s full monthly history
                  (requires ≥ 24 months) with the regional champion mean
                  as an external regressor."),
                tags$li("Missing months are imputed by simulation from the model’s
                  forecast distribution (truncated at zero)."),
                tags$li("Fallback to C1 (linear) when the series is too short or
                  the ARIMA fit fails."),
                tags$li("C2 is preferred when a facility has a clear seasonal
                  pattern; C1 is the recommended fallback.")
              ),

              hr(),

              h5("Method E — Adjusted Corrected Incidence"),

              p("Corrects confirmed case counts for incomplete testing coverage — ",
                "febrile patients who attended but were not tested for malaria."),

              p(tags$em(
                "Reference: Plucinski et al. (2018).  How Far Are We from ",
                "Reaching Universal Malaria Testing of All Fever Cases?  ",
                tags$strong("Am J Trop Med Hyg 99(3):670-679."),
                " doi:10.4269/ajtmh.18-0312"
              )),

              tags$ul(
                tags$li("Applied ", tags$strong("only to champion facilities"),
                  " that consistently report all three required elements ",
                  "(confirmed cases, total attendance, patients tested) for ≥ 10 of 12 months."),
                tags$li("The three elements are summed across qualifying champions ",
                  "within each region, then the ACI correction is applied once to ",
                  "the regional aggregate."),
                tags$li("For each region: ",
                  tags$strong("Adjusted = confirmed + (β·(A−confirmed) − tested_neg) × TPR"),
                  ", where A = total attendance, tested_neg = tested − confirmed, ",
                  "TPR = confirmed / tested."),
                tags$li("If observed testing rate ≥ β, no adjustment is applied."),
                tags$li(tags$strong("β defaults from Plucinski 2018 Table 1:"),
                  " β (< 5 yrs) = 0.75 [range 0.56–0.89];  ",
                  "β (≥ 5 yrs) = 0.589 [range 0.40–0.75].  ",
                  "β is drawn from Uniform(range) across 1,000 bootstrap samples."),
                tags$li("Non-champion facilities are excluded and do not influence ",
                  "the Method E estimate.")
              ),

              div(
                style = paste0(
                  "background:#fff8e1; border-left:4px solid #ffc107;",
                  " padding:8px 12px; margin:10px 0; border-radius:0 4px 4px 0;"
                ),
                tags$strong("Interpretation note:"),
                " Method E reports the testing-corrected case count for the ",
                tags$em("champion facility sector only"),
                " — it is not a total-burden estimate.  Compare it with Methods A, B, ",
                "or C (which extrapolate to all facilities) to understand how much of the ",
                "total burden is attributable to incomplete testing versus non-reporting.  ",
                "The ", tags$code("n_champions"), " column in the results table shows how ",
                "many facilities contributed to each regional estimate."
              ),

              hr(),

              h5("Population-Adjusted Rates"),

              p("When a population element is selected, all estimates are also ",
                "expressed as cases per 100,000 population.  Region-level population ",
                "is obtained by summing the selected element by region and year; ",
                "facility-level catchment populations are summed to region automatically."),

              hr(),

              h5("Method D — Care-Seeking Adjustment"),

              p("Adjusts any of the above estimates for persons who never seek
                facility care."),

              tags$ul(
                tags$li("Let pₖ = care-seeking proportion for category k.
                  Defaults: ",
                  tags$strong("pₚₚ = 0.90 (younger), pₒₗₗ = 0.70 (older)"),
                  " — placeholders to be updated from DHS / MIS surveys."),
                tags$li("For each available estimate Êₛ from A, B, C1, or C2: ",
                  tags$strong("Ê*ₛ = Êₛ / pₖ")),
                tags$li("Output is a table showing each base method with and
                  without the care-seeking adjustment, plus 95% uncertainty
                  intervals propagated through the division.")
              )
            )
          ),

          # ── References tab ────────────────────────────────────────────────
          bslib::nav_panel(
            "References",
            div(
              style = "max-width:820px; padding:16px; font-size:0.92em; line-height:1.7;",

              h4("Literature Search"),

              p("To identify methods for estimating malaria burden or comparing ",
                "subnational incidence using routine HMIS data, a structured search ",
                "was conducted in May 2026.  Search terms included combinations of: ",
                tags$em("malaria burden estimation, corrected incidence, adjusted ",
                  "incidence, routine health data, HMIS, subnational comparison, ",
                  "health facility surveillance"),
                ", restricted to 2020 onwards.  Databases: PubMed/PMC, Malaria Journal, ",
                "AJTMH, bioRxiv/medRxiv."),

              p("Inclusion criteria for MG2:"),
              tags$ol(
                tags$li("Uses only routinely collected HMIS data (confirmed cases, ",
                  "attendance, patients tested, population) — no environmental ",
                  "rasters, individual records, or sentinel surveys required."),
                tags$li("Provides a formula applicable to district-level annual or ",
                  "monthly aggregates."),
                tags$li("Validated or applied in sub-Saharan Africa.")
              ),

              p(tags$strong("Result:"), " One method met all three criteria and is ",
                "implemented as Method E.  Three method families were excluded ",
                "as requiring data beyond standard HMIS."),

              hr(),
              h5("Included"),

              tags$dl(
                tags$dt(
                  tags$strong("Thwing J, Plucinski MM, Painter JA, et al. (2020)")
                ),
                tags$dd(
                  tags$em("A Robust Estimator of Malaria Incidence from Routine ",
                    "Health Facility Data."),
                  " Am J Trop Med Hyg 102(4):811-820. ",
                  tags$a("doi:10.4269/ajtmh.19-0600",
                    href = "https://doi.org/10.4269/ajtmh.19-0600", target = "_blank"),
                  tags$br(),
                  "Five-equation algebraic correction for incomplete testing and ",
                  "low facility utilisation. Implemented as Method E.  Parameters: ",
                  "β (fever proportion among non-malaria consults), ",
                  "α (TPR ratio untested/tested febrile), ",
                  "γ (standard non-malaria fever incidence per person), ",
                  "λmin (minimum malaria-attributable fraction).  ",
                  "Excel tool: ",
                  tags$a("github.com/MateuszPlucinski/Corrected-Incidence",
                    href = "https://github.com/MateuszPlucinski/Corrected-Incidence",
                    target = "_blank"), "."
                ),

                tags$dt(
                  tags$strong("Plucinski MM, Guilavogui T, Camara A, et al. (2018)")
                ),
                tags$dd(
                  tags$em("How Far Are We from Reaching Universal Malaria Testing ",
                    "of All Fever Cases?"),
                  " Am J Trop Med Hyg 99(3):670-679. ",
                  tags$a("doi:10.4269/ajtmh.18-0312",
                    href = "https://doi.org/10.4269/ajtmh.18-0312", target = "_blank"),
                  tags$br(),
                  "Defines the corrected fever testing proportion B/(B+C) and provides ",
                  "gold-standard β estimates from 12 health facility surveys (Table 1).  ",
                  "Informs the testing-sufficiency check and default β ranges in Method E."
                )
              ),

              hr(),
              h5("Identified but Not Included"),

              tags$dl(
                tags$dt("Spatiotemporal GAM — Uganda sentinel sites (2023, PMC10201255)"),
                tags$dd(
                  tags$em("Excluded:"),
                  " requires individual-level patient data, travel-time surfaces, ",
                  "and environmental covariates from 74 sentinel facilities. ",
                  "Not applicable to national HMIS aggregates."
                ),

                tags$dt("Bayesian spatiotemporal model — Tanzania routine data ",
                  "(2023, Sci Rep doi:10.1038/s41598-023-37669-x)"),
                tags$dd(
                  tags$em("Excluded:"),
                  " geostatistical model requiring environmental raster inputs ",
                  "and specialist software beyond standard HMIS capability."
                ),

                tags$dt("Subnational tailoring process papers (2024-2025, ",
                  "PMC12118815, PMC12619253, PMC11720682)"),
                tags$dd(
                  tags$em("No new estimation formula."),
                  " Qualitative guidance on how national programs stratify using ",
                  "adjusted incidence; validates the regional comparison map ",
                  "approach already in MG2."
                )
              )
            )
          )
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

burden_widget_server <- function(
  id,
  directory_widget_output  = NULL,
  metadata_widget_output   = NULL,
  data_widget_output       = NULL,
  reporting_widget_output  = NULL,
  regions_widget_output    = NULL,
  current_tab              = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── reactive sources ──────────────────────────────────────────────────────

    data_folder <- reactive({ directory_widget_output$directory() })

    selected_data <- reactive({
      req(reporting_widget_output)
      reporting_widget_output$selected_data()
    })

    level_names <- reactive({
      req(reporting_widget_output)
      reporting_widget_output$levelNames()
    })

    geo_features <- reactive({
      req(metadata_widget_output)
      metadata_widget_output$geoFeatures()
    })

    # ── populate input choices ────────────────────────────────────────────────

    observeEvent(selected_data(), {
      d <- selected_data()
      req(!is.null(d) && nrow(d) > 0)

      elements <- sort(unique(d$data))
      none_choice <- c("(none)" = "")
      updateSelectInput(session, "target_elements",
                        choices = elements, selected = elements[1])
      updateSelectInput(session, "attendance_elements",
                        choices = c(none_choice, elements), selected = "")
      updateSelectInput(session, "tested_elements",
                        choices = c(none_choice, elements), selected = "")
      updateSelectInput(session, "population_element",
                        choices = c(none_choice, elements), selected = "")
    })

    # Date range from Reporting widget (champion window dates)
    start_month <- reactive({
      req(reporting_widget_output)
      reporting_widget_output$startingMonth()
    })
    end_month <- reactive({
      req(reporting_widget_output)
      reporting_widget_output$endingMonth()
    })

    # Safely coerce anything (character, yearmonth, Date) to yearmonth.
    # The reporting widget's startingMonth/endingMonth are character strings
    # (selectizeInput values); yearmonth objects can't compare with character.
    .to_ym <- function(x) {
      if (is.null(x)) return(NULL)
      if (inherits(x, "yearmonth")) return(x)
      ym <- tryCatch(tsibble::yearmonth(as.character(x)), error = function(e) NULL)
      if (is.null(ym) || length(ym) == 0 || is.na(ym)) return(NULL)
      ym
    }

    # yearmonth → Date (first day of month)
    .ym_date <- function(ym) {
      ym <- .to_ym(ym)
      if (is.null(ym)) return(NULL)
      as.Date(paste0(format(ym, "%Y-%m"), "-01"))
    }

    output$date_range_display <- renderUI({
      sm <- start_month()
      em <- end_month()
      if (is.null(sm) || is.null(em)) {
        div(style = "font-size:0.85em; color:#888;",
            "Period: set in Reporting tab")
      } else {
        sm_d <- tryCatch(.ym_date(sm), error = function(e) NULL)
        em_d <- tryCatch(.ym_date(em), error = function(e) NULL)
        if (is.null(sm_d) || is.null(em_d)) {
          div(style = "font-size:0.85em; color:#888;",
              paste0("Period: ", as.character(sm), " – ", as.character(em)))
        } else {
          n_mo <- as.integer(round(
            as.numeric(difftime(em_d, sm_d, units = "days")) / 30.4
          )) + 1L
          div(style = "font-size:0.85em; color:#444; margin-top:4px;",
              tags$strong("Period: "),
              paste0(format(sm_d, "%b %Y"), " – ",
                     format(em_d, "%b %Y"),
                     " (", n_mo, " months)"))
        }
      }
    })

    # ── category mapping UI (for Method B) ───────────────────────────────────

    output$cat_map_ui <- renderUI({
      att <- input$attendance_elements
      tgt <- input$target_elements
      req(length(att) > 0, att[1] != "", length(tgt) > 0)

      # Show mapping only when attendance categories differ from target
      if (length(att) == length(tgt) &&
          all(sort(att) == sort(tgt))) return(NULL)

      d <- selected_data()
      req(!is.null(d))
      att_cats <- sort(unique(d[d$data %in% att, ]$data))

      div(
        style = "padding:6px; background:#f0f4ff; border-radius:4px; margin-bottom:8px;",
        tags$small(tags$em("Attendance → target category mapping:")),
        lapply(tgt, function(tc) {
          selectInput(
            ns(paste0("map_cat_", make.names(tc))),
            label   = tc,
            choices = att_cats,
            width   = "100%"
          )
        })
      )
    })

    cat_map_reactive <- reactive({
      tgt <- input$target_elements
      att <- input$attendance_elements
      req(length(tgt) > 0)
      if (length(att) == 0 || att[1] == "") return(NULL)

      map <- stats::setNames(
        vapply(tgt, function(tc) {
          id <- paste0("map_cat_", make.names(tc))
          if (!is.null(input[[id]])) input[[id]] else att[1]
        }, character(1)),
        tgt
      )
      map
    })

    # ── progress log ─────────────────────────────────────────────────────────

    log_lines <- reactiveVal(character(0))

    add_log <- function(msg) {
      ts  <- format(Sys.time(), "%H:%M:%S")
      log_lines(c(log_lines(), paste0("[", ts, "] ", msg)))
    }

    output$progress_log <- renderText({
      l <- log_lines()
      if (length(l) == 0) return("(Press Run to start)")
      paste(l, collapse = "\n")
    })

    # ── results store ─────────────────────────────────────────────────────────

    results <- reactiveValues(
      A  = NULL,
      B  = NULL,
      C1 = NULL,
      C2 = NULL,
      E  = NULL,
      D  = NULL
    )

    # ── run button ────────────────────────────────────────────────────────────

    observeEvent(input$run, {
      req(selected_data())
      req(length(input$target_elements) > 0)

      log_lines(character(0))
      add_log("Starting burden estimation...")

      d_all      <- selected_data()
      tgt        <- input$target_elements
      methods    <- input$methods
      ln         <- level_names()
      region_col <- if (length(ln) >= 2) ln[2] else ln[1]

      # Filter data to the champion-window date range from Reporting widget.
      # Convert to yearmonth so data.table comparisons work correctly.
      sm_ym <- .to_ym(start_month())
      em_ym <- .to_ym(end_month())
      d  <- data.table::as.data.table(d_all)
      if (!is.null(sm_ym)) d <- d[Month >= sm_ym]
      if (!is.null(em_ym)) d <- d[Month <= em_ym]
      if (nrow(d) == 0) { add_log("ERROR: no data in the selected date range."); return() }
      sm_d <- .ym_date(sm_ym)
      em_d <- .ym_date(em_ym)
      period_label <- if (!is.null(sm_d) && !is.null(em_d))
        paste0(format(sm_d, "%b %Y"), " – ", format(em_d, "%b %Y"))
      else "all available data"
      add_log(paste0("  Period: ", period_label))

      # Clear previous results
      results$A  <- NULL; results$B  <- NULL
      results$C1 <- NULL; results$C2 <- NULL
      results$E  <- NULL; results$D  <- NULL

      n_boot <- 1000L

      if ("A" %in% methods) {
        add_log("Method A: Champion multiple...")
        res <- tryCatch(
          burden_a(d, tgt, region_col, n_bootstrap = n_boot),
          error = function(e) { add_log(paste("  ERROR:", e$message)); NULL }
        )
        results$A <- res
        if (!is.null(res))
          add_log(sprintf("  Done. %d region-year-category combinations.",
                          nrow(res$subnational)))
        else
          add_log("  No results (check champion facilities are defined).")
      }

      if ("B" %in% methods) {
        att <- input$attendance_elements
        if (length(att) == 0 || att[1] == "") {
          add_log("Method B skipped: no attendance element selected.")
        } else {
          add_log("Method B: Attendance-based...")
          cmap <- cat_map_reactive()
          res <- tryCatch(
            burden_b(d, tgt, att, cat_map = cmap, region_col,
                     n_bootstrap = n_boot),
            error = function(e) { add_log(paste("  ERROR:", e$message)); NULL }
          )
          results$B <- res
          if (!is.null(res))
            add_log(sprintf("  Done. %d region-year-category combinations.",
                            nrow(res$subnational)))
          else
            add_log("  No results (need ≥3 champion facilities with attendance data).")
        }
      }

      if ("C1" %in% methods) {
        add_log("Method C1: Linear imputation...")
        res <- tryCatch(
          burden_c1(d, tgt, region_col, n_bootstrap = n_boot),
          error = function(e) { add_log(paste("  ERROR:", e$message)); NULL }
        )
        results$C1 <- res
        if (!is.null(res)) {
          nm <- if (!is.null(res$not_modeled)) nrow(res$not_modeled) else 0L
          add_log(sprintf("  Done. %d not modeled (flagged for fallback).", nm))
        } else {
          add_log("  No results.")
        }
      }

      if ("C2" %in% methods) {
        add_log("Method C2: ARIMA imputation (may be slow)...")
        res <- tryCatch(
          burden_c2(d, tgt, region_col, n_bootstrap = n_boot),
          error = function(e) { add_log(paste("  ERROR:", e$message)); NULL }
        )
        results$C2 <- res
        if (!is.null(res)) {
          nm <- if (!is.null(res$not_modeled)) nrow(res$not_modeled) else 0L
          add_log(sprintf("  Done. %d facilities fell back to linear.", nm))
        } else {
          add_log("  No results.")
        }
      }

      if ("E" %in% methods) {
        att  <- input$attendance_elements
        test <- input$tested_elements
        if (length(att) == 0 || att[1] == "" || length(test) == 0 || test[1] == "") {
          add_log("Method E skipped: attendance and/or tested elements not selected.")
        } else {
          add_log("Method E: Robust Estimator (Thwing/Plucinski/Painter 2020)...")
          pop_elem <- input$population_element
          res <- tryCatch(
            burden_aci(d, tgt, att, test,
                       population_element = if (nchar(pop_elem) > 0) pop_elem else NULL,
                       beta_young    = input$beta_young,
                       beta_old      = input$beta_old,
                       alpha         = input$alpha_e,
                       gamma_young   = input$gamma_young,
                       gamma_old     = input$gamma_old,
                       lambdamin     = input$lambdamin,
                       years         = yrs,
                       region_col    = region_col,
                       n_bootstrap   = n_boot),
            error = function(e) { add_log(paste("  ERROR:", e$message)); NULL }
          )
          results$E <- res
          if (!is.null(res))
            add_log(sprintf("  Done. %d region-year-category combinations.",
                            nrow(res$subnational)))
          else
            add_log("  No results (check attendance and tested elements).")
        }
      }

      # Population rate (applies to all completed methods)
      pop_elem <- input$population_element
      if (!is.null(pop_elem) && nchar(pop_elem) > 0) {
        add_log("Adding population rates (per 100,000)...")
        for (nm in c("A", "B", "C1", "C2", "E")) {
          if (!is.null(results[[nm]])) {
            results[[nm]] <- tryCatch(
              add_population_rate(results[[nm]], d, pop_elem, region_col),
              error = function(e) results[[nm]]
            )
          }
        }
        add_log("  Population rates added.")
      }

      if (isTRUE(input$run_d)) {
        res_list <- Filter(Negate(is.null),
                           list(A = results$A, B = results$B,
                                C1 = results$C1, C2 = results$C2))
        if (length(res_list) == 0L) {
          add_log("Method D: no base estimates available, skipping.")
        } else {
          add_log("Method D: Care-seeking adjustment...")
          # Build per-category care-seeking from user inputs
          cs <- c(
            stats::setNames(
              rep(input$cs_young / 100, sum(grepl("<|young|under|inf", tgt, ignore.case = TRUE))),
              tgt[grepl("<|young|under|inf", tgt, ignore.case = TRUE)]
            ),
            stats::setNames(
              rep(input$cs_old / 100, sum(!grepl("<|young|under|inf", tgt, ignore.case = TRUE))),
              tgt[!grepl("<|young|under|inf", tgt, ignore.case = TRUE)]
            )
          )
          if (length(cs) == 0) cs <- c("default" = input$cs_young / 100)
          res_d <- tryCatch(
            burden_d_adjust(res_list, care_seeking = cs, n_bootstrap = n_boot),
            error = function(e) { add_log(paste("  ERROR:", e$message)); NULL }
          )
          results$D <- res_d
          if (!is.null(res_d))
            add_log(sprintf("  Done. %d rows.", nrow(res_d)))
        }
      }

      add_log("All methods complete.")

      # Update map selectors
      available <- names(Filter(Negate(is.null),
                                list(A = results$A, B = results$B,
                                     C1 = results$C1, C2 = results$C2,
                                     E = results$E)))
      updateSelectInput(session, "map_method",  choices = available)
      updateSelectInput(session, "map_category", choices = tgt, selected = tgt[1])
    })

    # ── results hint ──────────────────────────────────────────────────────────

    output$results_hint <- renderUI({
      all_null <- is.null(results$A) && is.null(results$B) &&
                  is.null(results$C1) && is.null(results$C2)
      if (all_null)
        div(style = "color:#888; font-style:italic; padding:10px;",
            "Select elements and years, choose methods, then press Run.")
    })

    # ── combined results table (national rows first, then sub-national) ──────

    combined_df <- reactive({
      res_list <- Filter(Negate(is.null),
                         list(A = results$A, B = results$B,
                              C1 = results$C1, C2 = results$C2,
                              E = results$E))
      if (length(res_list) == 0L) return(NULL)
      has_pop <- !is.null(input$population_element) &&
                 nchar(input$population_element) > 0

      nat <- burden_summary_table(res_list, "national",    show_rate = has_pop)
      sub <- burden_summary_table(res_list, "subnational", show_rate = has_pop)
      if (is.null(nat) && is.null(sub)) return(NULL)

      if (!is.null(nat)) names(nat)[names(nat) == "region"] <- "Area"
      if (!is.null(sub)) names(sub)[names(sub) == "region"] <- "Area"

      df <- rbind(nat, sub)

      # Add Group column when groups have been computed
      grps <- region_groups()
      if (!is.null(grps) && nrow(df) > 0) {
        df$Group <- grps[df$Area]
        df$Group <- ifelse(is.na(df$Group), "—",
                           paste0("G", df$Group))
        # Move Group column to position 2 (after Area)
        df <- df[, c("Area", "Group",
                     setdiff(names(df), c("Area", "Group"))),
                 drop = FALSE]
      }

      df
    })

    output$combined_table <- DT::renderDT({
      req(combined_df())
      DT::datatable(
        combined_df(),
        rownames = FALSE,
        filter   = "top",
        options  = list(
          dom        = "ftp",
          scrollX    = TRUE,
          scrollY    = "70vh",
          paging     = FALSE,
          columnDefs = list(list(className = "dt-left", targets = "_all"))
        )
      )
    })

    # ── map ───────────────────────────────────────────────────────────────────

    # Reactive that returns the current map data (gf2) for idiopleth reuse
    map_gf2 <- reactive({
      req(geo_features())
      method  <- input$map_method
      cat_sel <- input$map_category
      req(method, cat_sel)

      res <- switch(method,
        A  = results$A, B = results$B,
        C1 = results$C1, C2 = results$C2,
        E  = results$E
      )
      req(!is.null(res), !is.null(res$subnational))

      map_data <- as.data.frame(res$subnational)
      map_data <- map_data[map_data$category == cat_sel &
                           map_data$region != "National", ]
      req(nrow(map_data) > 0)

      gf_all <- geo_features()

      # Identify which level in geoFeatures best matches the region names
      # from the burden results (avoids hardcoding level == 2).
      region_names <- unique(map_data$region)
      poly_levels  <- sort(unique(gf_all$level[!sf::st_is_empty(gf_all)]))
      poly_levels  <- poly_levels[poly_levels > 1L]   # exclude national (level 1)
      best_level   <- if (length(poly_levels) > 0L) poly_levels[1L] else 2L
      for (lev in poly_levels) {
        names_at <- gf_all$name[gf_all$level == lev]
        if (mean(region_names %in% names_at, na.rm = TRUE) > 0.3) {
          best_level <- lev
          break
        }
      }

      gf_all |>
        dplyr::filter(level == best_level) |>
        dplyr::left_join(map_data, by = c("name" = "region"))
    })

    # Track which region is selected for idiopleth comparison
    selected_region <- reactiveVal(NULL)
    region_groups   <- reactiveVal(NULL)

    # Build base map (standard choropleth)
    output$burden_map <- leaflet::renderLeaflet({
      gf2     <- map_gf2()
      method  <- input$map_method
      yr      <- NULL
      cat_sel <- input$map_category

      selected_region(NULL)   # reset comparison on any map rebuild
      region_groups(NULL)     # reset grouping on any map rebuild

      est_vals <- gf2$estimate[is.finite(gf2$estimate)]
      if (length(est_vals) == 0L) {
        return(
          leaflet::leaflet(gf2) |>
            leaflet::addTiles() |>
            leaflet::fitBounds(
              lng1 = unname(sf::st_bbox(gf2)["xmin"]),
              lat1 = unname(sf::st_bbox(gf2)["ymin"]),
              lng2 = unname(sf::st_bbox(gf2)["xmax"]),
              lat2 = unname(sf::st_bbox(gf2)["ymax"])
            )
        )
      }
      pal  <- leaflet::colorNumeric("YlOrRd",
                                    domain = range(est_vals),
                                    na.color = "#cccccc")
      bbox <- sf::st_bbox(gf2)

      leaflet::leaflet(gf2) |>
        leaflet::addTiles() |>
        leaflet::fitBounds(
          lng1 = unname(bbox["xmin"]), lat1 = unname(bbox["ymin"]),
          lng2 = unname(bbox["xmax"]), lat2 = unname(bbox["ymax"])
        ) |>
        leaflet::addPolygons(
          fillColor        = ~pal(estimate),
          fillOpacity      = 0.75,
          color            = "white",
          weight           = 1,
          layerId          = ~name,
          label            = ~paste0(
            name, ": ",
            dplyr::if_else(
              is.na(estimate), "no data",
              format_burden_estimate(estimate, lower, upper)
            )
          ),
          highlightOptions = leaflet::highlightOptions(
            weight = 2, color = "#666", bringToFront = TRUE
          )
        ) |>
        leaflet::addLegend(
          pal       = pal,
          values    = ~estimate,
          title     = paste0("Method ", method, "<br>", cat_sel),
          layerId   = "legend_standard",
          labFormat = leaflet::labelFormat(big.mark = ",", digits = 0)
        )
    })

    # Toggle selected region on click; clicking same region deselects
    observeEvent(input$burden_map_shape_click, {
      clicked <- input$burden_map_shape_click$id
      if (!is.null(clicked) && !is.null(selected_region()) &&
          selected_region() == clicked) {
        selected_region(NULL)
      } else {
        selected_region(clicked)
      }
    })

    # Reset button: return to standard choropleth, clear groups
    observeEvent(input$reset_map, {
      selected_region(NULL)
      region_groups(NULL)
      updateCheckboxInput(session, "show_groups", value = FALSE)
    })

    # ── Group coloring ────────────────────────────────────────────────────────
    # Reacts to checkbox + slider; computes groups and redraws map.
    observe({
      req(isTRUE(input$show_groups))
      gf2     <- map_gf2()
      n_grps  <- input$n_groups
      cat_sel <- input$map_category
      req(gf2, n_grps, cat_sel)

      sub_df <- as.data.frame(gf2)[, intersect(
        names(as.data.frame(gf2)),
        c("name", "estimate", "lower", "upper", "category")
      )]
      names(sub_df)[names(sub_df) == "name"] <- "region"
      if (!"category" %in% names(sub_df)) sub_df$category <- cat_sel

      grps <- tryCatch(
        compute_burden_groups(sub_df, n_grps, category = cat_sel),
        error = function(e) NULL
      )
      region_groups(grps)
    })

    # When groups are set, redraw with categorical group palette
    observe({
      grps    <- region_groups()
      gf2     <- isolate(map_gf2())
      method  <- isolate(input$map_method)
      cat_sel <- isolate(input$map_category)
      n_grps  <- isolate(input$n_groups)
      if (is.null(grps) || is.null(gf2)) return()

      gf2$grp <- as.character(grps[gf2$name])
      gf2$grp[is.na(gf2$grp)] <- "?"

      n_pal  <- max(3L, n_grps)
      colors <- RColorBrewer::brewer.pal(n_pal, "RdYlGn")[seq_len(n_grps)]
      grp_pal <- leaflet::colorFactor(
        palette = colors,
        levels  = as.character(seq_len(n_grps)),
        na.color = "#cccccc"
      )

      grp_labels <- paste0("Group ", seq_len(n_grps),
                           " (low → high)")

      leaflet::leafletProxy(ns("burden_map"), data = gf2) |>
        leaflet::removeControl("legend_standard") |>
        leaflet::removeControl("legend_comparison") |>
        leaflet::addLegend(
          colors  = colors,
          labels  = paste0("Group ", seq_len(n_grps)),
          title   = paste0(n_grps, " groups — ", cat_sel,
                           "<br><small>1 = lowest burden</small>"),
          layerId = "legend_groups",
          opacity = 0.85
        ) |>
        leaflet::addPolygons(
          fillColor   = ~grp_pal(grp),
          fillOpacity = 0.80,
          color       = "white",
          weight      = 1,
          layerId     = ~name,
          label       = ~paste0(
            name, ": Group ", grp, " — ",
            dplyr::if_else(is.na(estimate), "no data",
                           format_burden_estimate(estimate, lower, upper))
          ),
          highlightOptions = leaflet::highlightOptions(
            weight = 2, color = "#333", bringToFront = TRUE
          )
        )
    })

    # When groups are cleared, remove group legend
    observeEvent(region_groups(), {
      req(is.null(region_groups()))
      leaflet::leafletProxy(ns("burden_map")) |>
        leaflet::removeControl("legend_groups")
    })

    # Reset to standard choropleth when region is deselected
    observeEvent(selected_region(), {
      req(is.null(selected_region()))
      gf2    <- isolate(map_gf2())
      method <- isolate(input$map_method)
      yr     <- NULL
      cat_s  <- isolate(input$map_category)
      if (is.null(gf2)) return()

      pal <- leaflet::colorNumeric("YlOrRd",
                                   domain = range(gf2$estimate, na.rm = TRUE),
                                   na.color = "#cccccc")
      leaflet::leafletProxy(ns("burden_map"), data = gf2) |>
        leaflet::removeControl("legend_comparison") |>
        leaflet::addLegend(
          pal     = pal, values = ~estimate,
          title   = paste0("Method ", method, "<br>", cat_s),
          layerId = "legend_standard",
          labFormat = leaflet::labelFormat(big.mark = ",", digits = 0)
        ) |>
        leaflet::addPolygons(
          fillColor   = ~pal(estimate), fillOpacity = 0.75,
          color = "white", weight = 1, layerId = ~name,
          label = ~paste0(name, ": ",
            dplyr::if_else(is.na(estimate), "no data",
                           format_burden_estimate(estimate, lower, upper))),
          highlightOptions = leaflet::highlightOptions(
            weight = 2, color = "#666", bringToFront = TRUE)
        )
    })

    # Idiopleth: significance-based comparison coloring.
    # Reacts to both region selection AND threshold slider.
    observe({
      reg       <- selected_region()
      threshold <- input$sig_threshold
      req(!is.null(reg), !is.null(threshold))

      gf2    <- isolate(map_gf2())
      method <- isolate(input$map_method)
      yr     <- NULL
      cat_s  <- isolate(input$map_category)
      if (is.null(gf2)) return()

      ref_row <- gf2[gf2$name == reg, ]
      if (nrow(ref_row) == 0 || is.na(ref_row$estimate[1])) return()

      ref_est   <- ref_row$estimate[1]
      ref_lower <- ref_row$lower[1]
      ref_upper <- ref_row$upper[1]

      # ── Significance via Gaussian approximation ─────────────────────────
      # SE approximated from 95% CI width (2.5th / 97.5th percentiles).
      # P(region_i > reference) computed as P(Z > 0) for Z ~ N(diff, se_diff²).
      z975    <- stats::qnorm(0.975)
      se_ref  <- (ref_upper  - ref_lower)  / (2 * z975)
      gf2$se  <- (gf2$upper  - gf2$lower)  / (2 * z975)
      gf2$se  <- pmax(gf2$se, 1)           # floor: avoid division by zero
      se_ref  <- max(se_ref, 1)

      gf2$diff     <- gf2$estimate - ref_est
      gf2$se_diff  <- sqrt(gf2$se^2 + se_ref^2)
      gf2$p_higher <- stats::pnorm(gf2$diff / gf2$se_diff)

      # ── Three-category significance classification ───────────────────────
      gf2$sig_cat <- dplyr::case_when(
        is.na(gf2$p_higher)              ~ "No data",
        gf2$name == reg                  ~ "Reference",
        gf2$p_higher >  threshold        ~ "Significantly higher",
        gf2$p_higher < (1 - threshold)   ~ "Significantly lower",
        TRUE                             ~ "Not significantly different"
      )

      sig_levels <- c("Significantly higher", "Not significantly different",
                      "Significantly lower",  "Reference", "No data")
      sig_colors <- c("#d73027", "#d9d9d9", "#1a9641", "#ffffff", "#cccccc")

      cat_pal <- leaflet::colorFactor(
        palette = sig_colors,
        levels  = sig_levels,
        na.color = "#cccccc"
      )

      # ── Labels ───────────────────────────────────────────────────────────
      fmt_label <- function(nm, est, lo, hi, diff, p, is_ref) {
        if (is.na(est)) return(paste0(nm, ": no data"))
        base <- format_burden_estimate(as.integer(est), as.integer(lo), as.integer(hi))
        if (is_ref) return(paste0(nm, ": ", base, " (reference)"))
        sign_str  <- ifelse(diff >= 0, "+", "")
        diff_str  <- formatC(as.integer(diff), format = "d", big.mark = ",")
        p_str     <- formatC(round(p, 2), format = "f", digits = 2)
        paste0(nm, ": ", base,
               " — ", ifelse(p > threshold, "significantly higher",
                           ifelse(p < 1 - threshold, "significantly lower",
                                  "not significantly different")),
               " (", sign_str, diff_str, "; p=", p_str, ")")
      }

      labels <- mapply(fmt_label,
        nm     = gf2$name,
        est    = gf2$estimate,
        lo     = gf2$lower,
        hi     = gf2$upper,
        diff   = gf2$diff,
        p      = gf2$p_higher,
        is_ref = gf2$name == reg,
        SIMPLIFY = TRUE, USE.NAMES = FALSE
      )

      leaflet::leafletProxy(ns("burden_map"), data = gf2) |>
        leaflet::removeControl("legend_standard") |>
        leaflet::addLegend(
          colors  = sig_colors[seq_along(sig_levels)],
          labels  = sig_levels,
          title   = paste0("vs. ", reg,
                           "<br><small>threshold: p≥", threshold, "</small>"),
          layerId = "legend_comparison",
          opacity = 0.85
        ) |>
        leaflet::addPolygons(
          fillColor   = ~cat_pal(sig_cat),
          fillOpacity = 0.80,
          color       = ~ifelse(name == reg, "#000000", "white"),
          weight      = ~ifelse(name == reg, 3L, 1L),
          layerId     = ~name,
          label       = labels,
          highlightOptions = leaflet::highlightOptions(
            weight = 2, color = "#333", bringToFront = TRUE
          )
        )
    })

    # Status label below map
    output$idiopleth_label <- renderUI({
      reg       <- selected_region()
      threshold <- input$sig_threshold
      if (!is.null(reg)) {
        div(
          style = paste0(
            "margin-top:4px; padding:4px 10px; background:#fff3cd;",
            " border-radius:4px; font-size:0.82em; color:#555;"
          ),
          tags$strong("Reference: "), reg,
          tags$span(style = "margin-left:8px;",
            paste0("p ≥ ", threshold, " = significantly higher (red);  ",
                   "p ≤ ", round(1 - threshold, 2), " = significantly lower (green)")),
          tags$span(style = "margin-left:10px; color:#888;",
                    "— click region again to reset")
        )
      }
    })

    outputOptions(output, "burden_map",     suspendWhenHidden = TRUE)
    outputOptions(output, "combined_table", suspendWhenHidden = TRUE)
    outputOptions(output, "idiopleth_label", suspendWhenHidden = TRUE)
  })
}
