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

            # Target element — element-level by default
            selectInput(
              ns("target_base"),
              label   = "Target data element(s):",
              choices = NULL, multiple = TRUE, selectize = TRUE, width = "100%"
            ),
            checkboxInput(ns("by_category"), "Select by category", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("by_category")),
              selectInput(
                ns("target_elements"),
                label   = "Categories:",
                choices = NULL, multiple = TRUE, selectize = TRUE, width = "100%"
              )
            ),
            uiOutput(ns("category_conflict_msg")),

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
            uiOutput(ns("date_range_display")),
            selectInput(
              ns("value_source"),
              label = "Values to use:",
              choices = c(
                "Original (no censoring)"               = "original",
                "Censor: key entry errors"              = "key_entry_error",
                "Censor: + over-max values"             = "over_max",
                "Censor: + MAD 15x outliers"            = "mad15",
                "Censor: + MAD 10x outliers"            = "mad10",
                "Censor: + seasonal 5x outliers"        = "seasonal5",
                "Censor all (through seasonal 3x)"      = "value"
              ),
              selected = "original",
              width = "100%"
            )
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
            div(
              style = "display:flex; align-items:center; gap:16px; padding:8px 0 4px 0;",
              uiOutput(ns("period_display")),
              uiOutput(ns("results_hint")),
              checkboxInput(ns("show_by_category"),
                            "Show by category", value = FALSE)
            ),
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
            ),
            fluidRow(
              column(6,
                div(
                  style = "display:flex; align-items:center; gap:12px;",
                  checkboxInput(ns("show_groups"), "Color by significance group",
                                value = FALSE),
                  conditionalPanel(
                    condition = sprintf("input['%s'] == true", ns("show_groups")),
                    div(style = "margin-top:-4px;",
                      sliderInput(
                        ns("n_groups"), label = NULL,
                        min = 2, max = 5, value = 3, step = 1,
                        width = "180px", ticks = FALSE, post = " groups"
                      )
                    )
                  )
                )
              ),
              column(6,
                div(
                  style = "display:flex; align-items:center; gap:10px; justify-content:flex-end;",
                  div(
                    style = "font-size:0.82em; color:#666;",
                    "Click region to compare. "
                  ),
                  actionButton(
                    ns("reset_map"), "Reset map",
                    class = "btn-sm btn-default",
                    icon  = shiny::icon("rotate-left")
                  )
                )
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

    # ── element parsing helpers ───────────────────────────────────────────────

    # Extract base element name (text before last underscore)
    .base_elem <- function(x) sub("_[^_]*$", "", x)
    # Extract category  (text after last underscore)
    .cat_elem  <- function(x) sub("^.*_([^_]*)$", "\\1", x)

    # Mapping: base element name → vector of full data values (all categories)
    element_map <- reactive({
      d <- selected_data()
      req(!is.null(d) && nrow(d) > 0)
      data_vals <- sort(unique(d$data))
      split(data_vals, .base_elem(data_vals))
    })

    # ── populate input choices ────────────────────────────────────────────────

    observeEvent(element_map(), {
      em <- element_map()
      none_choice <- c("(none)" = "")
      base_names  <- sort(names(em))
      all_vals    <- sort(unlist(em, use.names = FALSE))

      # Base-element selector (deduplicated) for all inputs
      updateSelectInput(session, "target_base",
                        choices = base_names, selected = base_names[1])
      updateSelectInput(session, "attendance_elements",
                        choices = c(none_choice, base_names), selected = "")
      updateSelectInput(session, "tested_elements",
                        choices = c(none_choice, base_names), selected = "")
      updateSelectInput(session, "population_element",
                        choices = c(none_choice, base_names), selected = "")
    })

    # When base element(s) selected, populate the by-category selector
    observeEvent(input$target_base, {
      em   <- element_map()
      tgt  <- input$target_base
      req(length(tgt) > 0)
      cats <- sort(unique(unlist(em[tgt], use.names = FALSE)))
      updateSelectInput(session, "target_elements",
                        choices = cats, selected = cats)
    }, ignoreNULL = TRUE)

    # Effective target elements used by all burden functions
    eff_target <- reactive({
      em  <- element_map()
      tgt <- input$target_base
      req(length(tgt) > 0)
      if (isTRUE(input$by_category) && length(input$target_elements) > 0) {
        input$target_elements          # user-selected categories
      } else {
        sort(unique(unlist(em[tgt], use.names = FALSE)))  # all categories for selected elements
      }
    })

    # ── category conflict detection ───────────────────────────────────────────

    output$category_conflict_msg <- renderUI({
      tgt <- input$target_base
      req(length(tgt) > 1)
      em <- element_map()

      # Get category sets per selected element
      cats_per <- lapply(tgt, function(e) {
        vals <- em[[e]]
        if (is.null(vals)) return(character(0))
        sort(.cat_elem(vals))
      })
      names(cats_per) <- tgt

      # Check if all elements have identical categories
      ref <- cats_per[[1]]
      all_same <- all(vapply(cats_per[-1],
                             function(c) identical(c, ref), logical(1)))
      if (all_same) return(NULL)

      # Try auto-mapping: strip sex suffix and compare age groups
      strip_sex <- function(x) trimws(gsub(",\\s*(Male|Female)$", "", x,
                                           ignore.case = TRUE, perl = TRUE))
      stripped_per <- lapply(cats_per, function(c) sort(unique(strip_sex(c))))
      auto_ok <- all(vapply(stripped_per[-1],
                            function(c) identical(c, stripped_per[[1]]),
                            logical(1)))

      if (auto_ok) {
        div(
          style = "background:#fff3cd; border-left:3px solid #ffc107; padding:6px 10px; font-size:0.85em; margin-bottom:6px;",
          tags$strong("Category mapping applied: "),
          "Selected elements have sex-split categories that will be ",
          "aggregated to age groups automatically before estimation."
        )
      } else {
        div(
          style = "background:#f8d7da; border-left:3px solid #dc3545; padding:6px 10px; font-size:0.85em; margin-bottom:6px;",
          tags$strong("Category mismatch: "),
          "The selected elements have incompatible categories. ",
          "Use ", tags$strong("Data → Combine"), " to create a merged ",
          "dataset with aligned categories before running burden estimates."
        )
      }
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

    # Category mapping UI — not needed when elements are selected at base level;
    # burden_b / burden_aci receive all categories and use default 1:1 or
    # single-element mapping automatically.
    output$cat_map_ui <- renderUI(NULL)
    cat_map_reactive  <- reactive(NULL)

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
      A            = NULL,
      B            = NULL,
      C1           = NULL,
      C2           = NULL,
      E            = NULL,
      D            = NULL,
      reported     = NULL,
      fac_counts   = NULL,
      period_label = NULL
    )

    # ── run button ────────────────────────────────────────────────────────────

    observeEvent(input$run, {
      req(selected_data())
      req(length(input$target_base) > 0)

      log_lines(character(0))
      add_log("Starting burden estimation...")

      d_all      <- selected_data()
      tgt        <- eff_target()   # all categories for selected base element(s)
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
      results$reported <- NULL

      # Apply value-source censoring based on cumulative outlier algorithm selection
      val_choice <- if (is.null(input$value_source)) "original" else input$value_source
      algo_order <- c("key_entry_error", "over_max", "mad15", "mad10", "seasonal5", "seasonal3")
      d <- data.table::copy(data.table::as.data.table(d_all))
      if (val_choice == "original") {
        if ("original" %in% names(d)) d[, value := original]
      } else if (val_choice %in% algo_order) {
        # Censor through the selected algorithm (and all more severe ones)
        censor_idx <- which(algo_order == val_choice)
        apply_algs <- algo_order[seq_len(censor_idx)]
        if ("original" %in% names(d)) d[, value := original]
        for (alg in apply_algs) {
          if (alg %in% names(d)) d[get(alg) == TRUE, value := NA_real_]
        }
      }
      # val_choice == "value" → keep d as-is (fully cleaned from selected_data)

      # Expand base element names to full element+category strings
      em <- element_map()
      .expand <- function(base_names) {
        if (length(base_names) == 0 || base_names[1] == "") return(NULL)
        sort(unique(unlist(em[base_names[base_names != ""]], use.names = FALSE)))
      }
      att_full  <- .expand(input$attendance_elements)
      test_full <- .expand(input$tested_elements)
      pop_full  <- .expand(input$population_element)

      # Store period label for display on Results tab
      results$period_label <- period_label

      # Period-filtered data kept for the "Reported" map layer
      d_period <- data.table::copy(d)
      if (!is.null(sm_ym)) d_period <- d_period[Month >= sm_ym]
      if (!is.null(em_ym)) d_period <- d_period[Month <= em_ym]
      results$d_period        <- d_period
      results$tgt_used        <- tgt
      results$region_col_used <- region_col

      # Reported actuals and per-facility average (within the estimate period)
      d_champ <- d_period[get("data") %in% tgt & !is.na(value)]
      rep_sub <- d_champ[, .(Reported = as.integer(sum(value, na.rm = TRUE))),
                          by = region_col]
      # Average = Reported / number of unique facilities that submitted any value
      n_rep_sub <- d_champ[, .(n_rep = data.table::uniqueN(orgUnit)), by = region_col]
      rep_sub <- merge(rep_sub, n_rep_sub, by = region_col, all.x = TRUE)
      rep_sub[, Avg := round(Reported / n_rep)]
      rep_sub[, n_rep := NULL]
      data.table::setnames(rep_sub, region_col, "region")
      rep_nat <- data.frame(
        region   = "National",
        Reported = as.integer(sum(rep_sub$Reported)),
        Avg      = round(sum(rep_sub$Reported) /
                         data.table::uniqueN(d_champ$orgUnit))
      )
      results$reported <- rbind(rep_nat, as.data.frame(rep_sub))

      # Facility universe: all elements (not filtered to target).
      # A champion identified via suspects-tested is still a champion even if
      # they never submitted a value for the target element — 0 is the true count.
      # The total denominator is every facility in the region, not just those
      # that appear in target-element data.
      fac_univ <- unique(d[!is.na(get(region_col)),
                           c("orgUnit", region_col, "Selected"), with = FALSE])
      champ_n <- fac_univ[Selected == "Champion",
                          .(n_champ = data.table::uniqueN(orgUnit)), by = region_col]
      total_n <- fac_univ[, .(n_total = data.table::uniqueN(orgUnit)), by = region_col]
      fac_counts <- merge(champ_n, total_n, by = region_col, all = TRUE)
      fac_counts[is.na(n_champ), n_champ := 0L]
      fac_counts[, champ_total := paste0(n_champ, " / ", n_total)]
      data.table::setnames(fac_counts, region_col, "region")
      nat_fac <- data.frame(
        region     = "National",
        champ_total = paste0(sum(fac_counts$n_champ), " / ", sum(fac_counts$n_total, na.rm=TRUE))
      )
      fac_df <- rbind(nat_fac, as.data.frame(fac_counts[, .(region, champ_total)]))
      names(fac_df)[names(fac_df) == "champ_total"] <- "Champ/Total"
      results$fac_counts <- fac_df

      n_boot <- 1000L

      # Build province adjacency for Method A neighbour fallback.
      # Uses sf::st_touches() on the region-level polygons so that when a
      # province has no usable local champions, the closest geographic peers
      # are tried before falling back to the national distribution.
      neighbor_list <- tryCatch({
        gf <- geo_features()
        if (!inherits(gf, "sf")) stop("no sf")
        region_names <- unique(d[[region_col]])
        poly_levels  <- sort(unique(gf$level[!sf::st_is_empty(gf) & gf$level > 1L]))
        best_lev     <- poly_levels[1L]
        for (lev in poly_levels) {
          if (mean(region_names %in% gf$name[gf$level == lev], na.rm = TRUE) > 0.3) {
            best_lev <- lev; break
          }
        }
        prov_sf <- gf[gf$level == best_lev & !sf::st_is_empty(gf), ]
        adj <- sf::st_touches(prov_sf, sparse = FALSE)
        nms <- prov_sf$name
        stats::setNames(lapply(seq_len(nrow(prov_sf)),
                               function(i) nms[adj[i, ]]), nms)
      }, error = function(e) NULL)

      if ("A" %in% methods) {
        add_log("Method A: Champion multiple...")
        res <- tryCatch(
          burden_a(d, tgt, region_col,
                   period_start = sm_ym, period_end = em_ym,
                   neighbor_list = neighbor_list,
                   n_bootstrap = n_boot),
          error = function(e) { add_log(paste("  ERROR:", e$message)); NULL }
        )
        results$A <- add_category_totals(res)
        if (!is.null(res))
          add_log(sprintf("  Done. %d region-year-category combinations.",
                          nrow(res$subnational)))
        else
          add_log("  No results (check champion facilities are defined).")
      }

      if ("B" %in% methods) {
        att <- att_full
        if (is.null(att) || length(att) == 0) {
          add_log("Method B skipped: no attendance element selected.")
        } else {
          add_log("Method B: Attendance-based...")
          cmap <- cat_map_reactive()
          res <- tryCatch(
            burden_b(d, tgt, att, cat_map = cmap, region_col,
                     n_bootstrap = n_boot),
            error = function(e) { add_log(paste("  ERROR:", e$message)); NULL }
          )
          results$B <- add_category_totals(res)
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
          burden_c1(d, tgt, region_col,
                    period_start = sm_ym, period_end = em_ym,
                    n_bootstrap = n_boot),
          error = function(e) { add_log(paste("  ERROR:", e$message)); NULL }
        )
        results$C1 <- add_category_totals(res)
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
          burden_c2(d, tgt, region_col,
                    period_start = sm_ym, period_end = em_ym,
                    n_bootstrap = n_boot),
          error = function(e) { add_log(paste("  ERROR:", e$message)); NULL }
        )
        results$C2 <- add_category_totals(res)
        if (!is.null(res)) {
          nm <- if (!is.null(res$not_modeled)) nrow(res$not_modeled) else 0L
          add_log(sprintf("  Done. %d facilities fell back to linear.", nm))
        } else {
          add_log("  No results.")
        }
      }

      if ("E" %in% methods) {
        att  <- att_full
        test <- test_full
        if (is.null(att) || is.null(test) || length(att) == 0 || length(test) == 0) {
          add_log("Method E skipped: attendance and/or tested elements not selected.")
        } else {
          add_log("Method E: Robust Estimator (Thwing/Plucinski/Painter 2020)...")
          res <- tryCatch(
            burden_aci(d, tgt, att, test,
                       population_element = if (length(pop_full) > 0) pop_full[1] else NULL,
                       beta_young    = input$beta_young,
                       beta_old      = input$beta_old,
                       alpha         = input$alpha_e,
                       gamma_young   = input$gamma_young,
                       gamma_old     = input$gamma_old,
                       lambdamin     = input$lambdamin,
                       region_col    = region_col,
                       n_bootstrap   = n_boot),
            error = function(e) { add_log(paste("  ERROR:", e$message)); NULL }
          )
          results$E <- add_category_totals(res)
          if (!is.null(res))
            add_log(sprintf("  Done. %d region-year-category combinations.",
                            nrow(res$subnational)))
          else
            add_log("  No results (check attendance and tested elements).")
        }
      }

      # Population rate (applies to all completed methods)
      if (length(pop_full) > 0) {
        add_log("Adding population rates (per 100,000)...")
        for (nm in c("A", "B", "C1", "C2", "E")) {
          if (!is.null(results[[nm]])) {
            results[[nm]] <- tryCatch(
              add_population_rate(results[[nm]], d, pop_full[1], region_col),
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
          # Build per-category care-seeking.
          # "younger" pattern covers common age-group notations: 0-4, <5, under, infant.
          # "Total" and unmatched categories use the older (more conservative) proportion.
          young_pat <- "0-4|<5|<4|under|young|infant|<five"
          is_young  <- grepl(young_pat, c(tgt, "Total"), ignore.case = TRUE)
          p_young   <- input$cs_young / 100
          p_old     <- input$cs_old   / 100
          cs <- stats::setNames(
            ifelse(is_young, p_young, p_old),
            c(tgt, "Total")
          )
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
      # "Reported" is always available after a run — prepend it
      available <- c("Reported", available)
      updateSelectInput(session, "map_method",  choices = available)
      # Map category: always include "Total" first, then individual categories
      map_cats <- c("Total", tgt)
      updateSelectInput(session, "map_category",
                        choices = map_cats, selected = "Total")
    })

    # ── results hint ──────────────────────────────────────────────────────────

    output$period_display <- renderUI({
      lbl <- results$period_label
      if (!is.null(lbl) && nchar(lbl) > 0) {
        div(
          style = "font-size:0.85em; color:#555; padding-top:6px;",
          tags$strong("Period: "), lbl
        )
      }
    })

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

      # Method D: burden_d_adjust() returns a flat table with a `level` column.
      # Reshape into the list($subnational, $national) format so
      # burden_summary_table() can handle it alongside A/B/C1/C2/E.
      d_flat <- results$D
      if (!is.null(d_flat) && nrow(d_flat) > 0) {
        d_flat <- as.data.frame(d_flat)
        for (bm in unique(d_flat$base_method)) {
          key <- paste0(bm, "+D")
          sub_d <- d_flat[d_flat$base_method == bm & d_flat$level == "subnational",
                          c("region", "category", "estimate", "lower", "upper"),
                          drop = FALSE]
          nat_d <- d_flat[d_flat$base_method == bm & d_flat$level == "national",
                          c("region", "category", "estimate", "lower", "upper"),
                          drop = FALSE]
          sub_d$method <- key;  nat_d$method <- key
          res_list[[key]] <- list(subnational = sub_d, national = nat_d)
        }
      }

      if (length(res_list) == 0L) return(NULL)
      has_pop <- !is.null(input$population_element) &&
                 nchar(input$population_element) > 0

      # Filter to Total-only or all categories based on toggle
      show_cats <- isTRUE(input$show_by_category)
      res_filtered <- lapply(res_list, function(res) {
        lapply(res[c("subnational", "national")], function(df) {
          if (is.null(df)) return(NULL)
          df <- as.data.frame(df)
          if ("category" %in% names(df)) {
            if (!show_cats) {
              df <- df[df$category == "Total", , drop = FALSE]
            }
          }
          df
        })
      })
      names(res_filtered) <- names(res_list)

      nat <- burden_summary_table(res_filtered, "national",    show_rate = has_pop)
      sub <- burden_summary_table(res_filtered, "subnational", show_rate = has_pop)
      if (is.null(nat) && is.null(sub)) return(NULL)

      if (!is.null(nat)) names(nat)[names(nat) == "region"] <- "Area"
      if (!is.null(sub)) names(sub)[names(sub) == "region"] <- "Area"

      # Drop "category" column when showing totals only (it just says "Total")
      if (!show_cats && "category" %in% names(nat)) nat$category <- NULL
      if (!show_cats && "category" %in% names(sub)) sub$category <- NULL

      df <- rbind(nat, sub)

      # Add Reported, Avg, Champ/Total columns (actual totals, no estimation)
      if (!is.null(results$reported) && nrow(df) > 0) {
        df <- merge(df, results$reported, by.x = "Area", by.y = "region", all.x = TRUE)
        df$Reported <- formatC(df$Reported, format = "d", big.mark = ",")
        df$Reported[is.na(df$Reported)] <- "—"
        df$Avg <- formatC(df$Avg, format = "d", big.mark = ",")
        df$Avg[is.na(df$Avg)] <- "—"
      }
      if (!is.null(results$fac_counts) && nrow(df) > 0) {
        df <- merge(df, results$fac_counts, by.x = "Area", by.y = "region", all.x = TRUE)
        df[["Champ/Total"]][is.na(df[["Champ/Total"]])] <- "—"
      }
      # Reorder: Area, [Group], Reported, Avg, Champ/Total, then method columns
      first_cols  <- intersect(c("Area", "Group"), names(df))
      anchor_cols <- intersect(c("Reported", "Avg", "Champ/Total"), names(df))
      df <- df[, c(first_cols, anchor_cols,
                   setdiff(names(df), c(first_cols, anchor_cols))),
               drop = FALSE]

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
      df <- combined_df()
      req(df)

      dt <- DT::datatable(
        df,
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

      # Color rows by group when groups are active
      grps <- region_groups()
      if (!is.null(grps) && "Group" %in% names(df)) {
        n_grps   <- if (is.null(isolate(input$n_groups))) 3L else isolate(input$n_groups)
        all_cols <- RColorBrewer::brewer.pal(max(3L, n_grps), "YlOrRd")
        idx      <- round(seq(1, max(3L, n_grps), length.out = n_grps))
        colors   <- all_cols[idx]
        grp_labels <- paste0("G", seq_len(n_grps))
        dt <- DT::formatStyle(
          dt, "Group",
          target          = "row",
          backgroundColor = DT::styleEqual(
            c(grp_labels, "—"),
            c(colors,     "white")
          )
        )
      }

      dt
    })

    # ── map ───────────────────────────────────────────────────────────────────

    # Reactive that returns the current map data (gf2) for idiopleth reuse
    map_gf2 <- reactive({
      req(geo_features())
      method  <- input$map_method
      cat_sel <- input$map_category
      req(method, cat_sel)

      if (method == "Reported") {
        d_p  <- results$d_period
        tgt_u <- results$tgt_used
        rc    <- results$region_col_used
        req(!is.null(d_p), !is.null(tgt_u), !is.null(rc))

        d_champ <- d_p[get("data") %in% tgt_u & !is.na(value)]
        if (cat_sel == "Total") {
          map_data <- d_champ[, .(estimate = as.integer(sum(value, na.rm = TRUE))),
                               by = rc]
        } else {
          map_data <- d_champ[get("data") == cat_sel,
                               .(estimate = as.integer(sum(value, na.rm = TRUE))),
                               by = rc]
        }
        req(nrow(map_data) > 0)
        data.table::setnames(map_data, rc, "region")
        map_data[, lower := NA_integer_]
        map_data[, upper := NA_integer_]
        map_data <- as.data.frame(map_data)
      } else {
        res <- switch(method,
          A  = results$A, B = results$B,
          C1 = results$C1, C2 = results$C2,
          E  = results$E
        )
        req(!is.null(res), !is.null(res$subnational))

        map_data <- as.data.frame(res$subnational)
        if (!"category" %in% names(map_data)) map_data$category <- "Total"
        map_data <- map_data[map_data$category == cat_sel &
                             map_data$region != "National", ]
        req(nrow(map_data) > 0)
      }

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
    selected_region  <- reactiveVal(NULL)
    region_groups    <- reactiveVal(NULL)
    choropleth_reset <- reactiveVal(0L)   # incremented to force choropleth redraw

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
          label            = if (method == "Reported") {
            ~paste0(name, ": ",
                    dplyr::if_else(is.na(estimate), "no data",
                                   formatC(estimate, format = "d", big.mark = ",")))
          } else {
            ~paste0(name, ": ",
                    dplyr::if_else(is.na(estimate), "no data",
                                   format_burden_estimate(estimate, lower, upper)))
          },
          highlightOptions = leaflet::highlightOptions(
            weight = 2, color = "#666", bringToFront = TRUE
          )
        ) |>
        leaflet::addLegend(
          pal       = pal,
          values    = ~estimate,
          title     = if (method == "Reported")
                        paste0("Reported<br>(all facilities)<br>", cat_sel)
                      else
                        paste0("Method ", method, "<br>", cat_sel),
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

    # Reset button: clear idiopleth + groups, force choropleth redraw.
    # selected_region may already be NULL (groups shown without idiopleth),
    # so we can't rely on it changing — use a separate counter to force redraw.
    observeEvent(input$reset_map, {
      selected_region(NULL)
      region_groups(NULL)
      updateCheckboxInput(session, "show_groups", value = FALSE)
      choropleth_reset(choropleth_reset() + 1L)
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

      # YlOrRd: light yellow (low burden) → dark red (high burden),
      # matching the standard choropleth direction.
      n_pal     <- max(3L, n_grps)
      all_cols  <- RColorBrewer::brewer.pal(n_pal, "YlOrRd")
      idx       <- round(seq(1, n_pal, length.out = n_grps))
      colors    <- all_cols[idx]
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
                           "<br><small>light = lowest, dark = highest</small>"),
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

    # Reset to standard choropleth: triggered by deselecting a region OR by
    # the reset button (via choropleth_reset counter).
    .redraw_choropleth <- function() {
      gf2    <- isolate(map_gf2())
      method <- isolate(input$map_method)
      cat_s  <- isolate(input$map_category)
      if (is.null(gf2) || is.null(method)) return()

      est_vals <- gf2$estimate[is.finite(gf2$estimate)]
      if (length(est_vals) == 0L) return()
      pal <- leaflet::colorNumeric("YlOrRd", domain = range(est_vals),
                                   na.color = "#cccccc")
      leaflet::leafletProxy(ns("burden_map"), data = gf2) |>
        leaflet::removeControl("legend_comparison") |>
        leaflet::removeControl("legend_groups") |>
        leaflet::addLegend(pal = pal, values = ~estimate,
                           title = paste0("Method ", method, "<br>", cat_s),
                           layerId = "legend_standard",
                           labFormat = leaflet::labelFormat(big.mark = ",", digits = 0)) |>
        leaflet::addPolygons(
          fillColor = ~pal(estimate), fillOpacity = 0.75,
          color = "white", weight = 1, layerId = ~name,
          label = ~paste0(name, ": ",
            dplyr::if_else(is.na(estimate), "no data",
                           format_burden_estimate(estimate, lower, upper))),
          highlightOptions = leaflet::highlightOptions(
            weight = 2, color = "#666", bringToFront = TRUE)
        )
    }

    observeEvent(choropleth_reset(), {
      req(choropleth_reset() > 0L)
      .redraw_choropleth()
    })

    observeEvent(selected_region(), {
      req(is.null(selected_region()))
      .redraw_choropleth()
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
