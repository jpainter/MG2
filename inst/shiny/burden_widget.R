# Burden Estimate module
# UI: burden_widget_ui(id)
# Server: burden_widget_server(id, ...)

# ── UI ────────────────────────────────────────────────────────────────────────

burden_widget_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinybusy::add_busy_spinner(spin = "fading-circle", position = "bottom-right"),

    # Under-development banner
    div(
      style = paste0(
        "background:#fff3cd; border:1px solid #ffc107; border-radius:6px;",
        " padding:8px 14px; margin-bottom:12px; display:flex;",
        " align-items:center; gap:10px;"
      ),
      tags$span("\U0001F6A7",
                style = "font-size:1.4em; line-height:1;"),
      div(
        tags$strong("Under Development"),
        tags$span(
          style = "font-size:0.85em; color:#555; margin-left:6px;",
          "Methods and outputs are being refined.  Results are experimental."
        )
      )
    ),

    sidebarLayout(
      sidebarPanel(
        width = 3,

        h5("Data"),

        selectInput(
          ns("target_elements"),
          label   = "Target data element(s):",
          choices = NULL, multiple = TRUE, selectize = TRUE, width = "100%"
        ),

        selectInput(
          ns("attendance_elements"),
          label   = "Attendance element(s) — for Methods B & E:",
          choices = NULL, multiple = TRUE, selectize = TRUE, width = "100%"
        ),

        selectInput(
          ns("tested_elements"),
          label   = "Patients tested element(s) — for Method E:",
          choices = NULL, multiple = TRUE, selectize = TRUE, width = "100%"
        ),

        selectInput(
          ns("population_element"),
          label   = "Population element (optional — for per-100k rates):",
          choices = NULL, multiple = FALSE, selectize = TRUE, width = "100%"
        ),

        uiOutput(ns("cat_map_ui")),

        selectInput(
          ns("years"),
          label    = "Year(s):",
          choices  = NULL, multiple = TRUE, selectize = TRUE, width = "100%"
        ),

        hr(),
        h5("Methods"),

        checkboxGroupInput(
          ns("methods"),
          label    = NULL,
          choices  = c(
            "A — Champion multiple"       = "A",
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
            tags$small(tags$em(
              "Method E: Thwing, Plucinski, Painter et al. (2020) AJTMH 102:811-820"
            )),
            br(),
            tags$small(tags$strong("β"), " — fever proportion among non-malaria consults:"),
            fluidRow(
              column(6,
                numericInput(ns("beta_young"), "< 5 yrs", value = 0.73,
                             min = 0.1, max = 0.99, step = 0.01, width = "100%")
              ),
              column(6,
                numericInput(ns("beta_old"), "≥ 5 yrs", value = 0.57,
                             min = 0.1, max = 0.99, step = 0.01, width = "100%")
              )
            ),
            tags$small(tags$strong("α"), " — TPR ratio (untested / tested febrile):"),
            numericInput(ns("alpha_e"), NULL, value = 0.48,
                         min = 0.1, max = 1.0, step = 0.01, width = "100%"),
            tags$small(tags$strong("γ"), " — expected NMF episodes / person / year:"),
            fluidRow(
              column(6,
                numericInput(ns("gamma_young"), "< 5 yrs", value = 2.0,
                             min = 0.1, max = 10, step = 0.1, width = "100%")
              ),
              column(6,
                numericInput(ns("gamma_old"), "≥ 5 yrs", value = 1.0,
                             min = 0.1, max = 10, step = 0.1, width = "100%")
              )
            ),
            tags$small(tags$strong("λmin"), " — min. malaria-attributable fraction:"),
            numericInput(ns("lambdamin"), NULL, value = 0.75,
                         min = 0.1, max = 1.0, step = 0.05, width = "100%")
          )
        ),

        checkboxInput(
          ns("run_d"),
          label = "D — Care-seeking adjustment",
          value = FALSE
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("run_d")),
          div(
            style = "padding-left:10px; font-size:0.9em;",
            p(style = "margin-bottom:4px; color:#555;",
              "Default proportions (editable):"),
            fluidRow(
              column(6,
                numericInput(ns("cs_young"), "Younger (%)", value = 90,
                             min = 1, max = 100, step = 1, width = "100%")
              ),
              column(6,
                numericInput(ns("cs_old"), "Older (%)", value = 70,
                             min = 1, max = 100, step = 1, width = "100%")
              )
            )
          )
        ),

        hr(),

        actionButton(ns("run"), "Run Estimates",
                     class = "btn-primary btn-block",
                     style = "width:100%;"),

        br(),

        div(
          style = paste0(
            "background:#f8f9fa; border:1px solid #dee2e6; border-radius:4px;",
            " padding:6px; max-height:200px; overflow-y:auto; font-size:0.78em;",
            " font-family:monospace;"
          ),
          verbatimTextOutput(ns("progress_log"), placeholder = TRUE)
        )
      ),

      mainPanel(
        width = 9,

        bslib::navset_tab(
          id = ns("burden_tab"),

          bslib::nav_panel(
            "Results",

            br(),

            uiOutput(ns("results_hint")),

            h5("National totals"),
            DT::DTOutput(ns("national_table")),

            br(),

            h5("Sub-national totals"),
            DT::DTOutput(ns("subnational_table")),

            br(),

            fluidRow(
              column(4,
                selectInput(ns("map_method"), "Color map by method:",
                            choices = NULL, width = "100%")
              ),
              column(4,
                selectInput(ns("map_year"), "Year:",
                            choices = NULL, width = "100%")
              ),
              column(4,
                selectInput(ns("map_category"), "Category:",
                            choices = NULL, width = "100%")
              )
            ),

            div(
              style = "font-size:0.83em; color:#555; margin-bottom:4px;",
              tags$em(
                "\U0001F4CC Click a region to compare it with all others ",
                "(red = higher burden, green = lower burden). ",
                "Click again to return to standard view."
              )
            ),

            leaflet::leafletOutput(ns("burden_map"), height = "55vh"),

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

      yrs <- sort(unique(lubridate::year(d$Month)))
      updateSelectInput(session, "years",
                        choices = yrs, selected = max(yrs))
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
      req(length(input$years) > 0)

      log_lines(character(0))
      add_log("Starting burden estimation...")

      d          <- selected_data()
      tgt        <- input$target_elements
      yrs        <- as.integer(input$years)
      methods    <- input$methods
      ln         <- level_names()
      region_col <- if (length(ln) >= 2) ln[2] else ln[1]

      # Clear previous results
      results$A  <- NULL; results$B  <- NULL
      results$C1 <- NULL; results$C2 <- NULL
      results$E  <- NULL; results$D  <- NULL

      n_boot <- 1000L

      if ("A" %in% methods) {
        add_log("Method A: Champion multiple...")
        res <- tryCatch(
          burden_a(d, tgt, yrs, region_col, n_bootstrap = n_boot),
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
            burden_b(d, tgt, att, cat_map = cmap, yrs, region_col,
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
          burden_c1(d, tgt, yrs, region_col, n_bootstrap = n_boot),
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
          burden_c2(d, tgt, yrs, region_col, n_bootstrap = n_boot),
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
      updateSelectInput(session, "map_year",    choices = as.character(yrs),
                        selected = as.character(max(yrs)))
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

    # ── national table ────────────────────────────────────────────────────────

    national_df <- reactive({
      res_list <- Filter(Negate(is.null),
                         list(A = results$A, B = results$B,
                              C1 = results$C1, C2 = results$C2))
      if (length(res_list) == 0L) return(NULL)
      has_pop <- !is.null(input$population_element) &&
                 nchar(input$population_element) > 0
      burden_summary_table(res_list, level = "national", show_rate = has_pop)
    })

    output$national_table <- DT::renderDT({
      req(national_df())
      DT::datatable(
        national_df(),
        rownames = FALSE,
        options  = list(
          dom      = "t",
          paging   = FALSE,
          scrollX  = TRUE,
          columnDefs = list(list(className = "dt-left", targets = "_all"))
        )
      )
    })

    # ── subnational table ─────────────────────────────────────────────────────

    subnational_df <- reactive({
      res_list <- Filter(Negate(is.null),
                         list(A = results$A, B = results$B,
                              C1 = results$C1, C2 = results$C2))
      if (length(res_list) == 0L) return(NULL)
      burden_summary_table(res_list, level = "subnational")
    })

    output$subnational_table <- DT::renderDT({
      req(subnational_df())
      DT::datatable(
        subnational_df(),
        rownames = FALSE,
        filter   = "top",
        options  = list(
          dom     = "ftp",
          scrollX = TRUE,
          scrollY = "35vh",
          paging  = FALSE,
          columnDefs = list(list(className = "dt-left", targets = "_all"))
        )
      )
    })

    # ── map ───────────────────────────────────────────────────────────────────

    # Reactive that returns the current map data (gf2) for idiopleth reuse
    map_gf2 <- reactive({
      req(geo_features())
      method  <- input$map_method
      yr      <- as.integer(input$map_year)
      cat_sel <- input$map_category
      req(method, yr, cat_sel)

      res <- switch(method,
        A  = results$A, B = results$B,
        C1 = results$C1, C2 = results$C2,
        E  = results$E
      )
      req(!is.null(res), !is.null(res$subnational))

      map_data <- as.data.frame(res$subnational)
      map_data <- map_data[map_data$year == yr &
                           map_data$category == cat_sel &
                           map_data$region != "National", ]
      req(nrow(map_data) > 0)

      geo_features() |>
        dplyr::filter(level == 2L) |>
        dplyr::left_join(map_data, by = c("name" = "region"))
    })

    # Track which region is selected for idiopleth comparison
    selected_region <- reactiveVal(NULL)

    # Build base map (standard choropleth)
    output$burden_map <- leaflet::renderLeaflet({
      gf2     <- map_gf2()
      method  <- input$map_method
      yr      <- input$map_year
      cat_sel <- input$map_category

      selected_region(NULL)  # reset comparison on any map rebuild

      pal  <- leaflet::colorNumeric("YlOrRd",
                                    domain = range(gf2$estimate, na.rm = TRUE),
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
          title     = paste0("Method ", method, "<br>", cat_sel, "<br>", yr),
          layerId   = "legend_standard",
          labFormat = leaflet::labelFormat(
            transform = function(x) formatC(as.integer(x), format = "d", big.mark = ",")
          )
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

    # Idiopleth: redraw polygons colored by comparison to selected region
    observeEvent(selected_region(), {
      gf2    <- isolate(map_gf2())
      method <- isolate(input$map_method)
      yr     <- isolate(input$map_year)
      cat_s  <- isolate(input$map_category)
      if (is.null(gf2)) return()

      reg <- selected_region()

      if (is.null(reg)) {
        # Return to standard choropleth
        pal <- leaflet::colorNumeric("YlOrRd",
                                     domain = range(gf2$estimate, na.rm = TRUE),
                                     na.color = "#cccccc")
        leaflet::leafletProxy(ns("burden_map"), data = gf2) |>
          leaflet::removeControl("legend_comparison") |>
          leaflet::addLegend(
            pal       = pal,
            values    = ~estimate,
            title     = paste0("Method ", method, "<br>", cat_s, "<br>", yr),
            layerId   = "legend_standard",
            labFormat = leaflet::labelFormat(
              transform = function(x) formatC(as.integer(x), format = "d", big.mark = ",")
            )
          ) |>
          leaflet::addPolygons(
            fillColor   = ~pal(estimate),
            fillOpacity = 0.75,
            color       = "white",
            weight      = 1,
            layerId     = ~name,
            label       = ~paste0(
              name, ": ",
              dplyr::if_else(is.na(estimate), "no data",
                             format_burden_estimate(estimate, lower, upper))
            ),
            highlightOptions = leaflet::highlightOptions(
              weight = 2, color = "#666", bringToFront = TRUE
            )
          )
        return()
      }

      # Compute idiopleth comparison values: other - selected
      ref_val <- gf2$estimate[gf2$name == reg]
      if (length(ref_val) == 0 || is.na(ref_val[1])) return()
      ref_val <- ref_val[1]

      gf2$comparison <- gf2$estimate - ref_val

      # Symmetric domain for diverging palette
      max_abs <- max(abs(gf2$comparison), na.rm = TRUE)
      if (max_abs == 0) max_abs <- 1
      sym_domain <- c(-max_abs, max_abs)

      cmp_pal <- leaflet::colorNumeric(
        palette   = c("#1a9641", "#ffffbf", "#d7191c"),
        domain    = sym_domain,
        na.color  = "#cccccc"
      )

      leaflet::leafletProxy(ns("burden_map"), data = gf2) |>
        leaflet::removeControl("legend_standard") |>
        leaflet::addLegend(
          pal     = cmp_pal,
          values  = sym_domain,
          title   = paste0("vs. ", reg, "<br><small>red = higher burden</small>"),
          layerId = "legend_comparison",
          labFormat = leaflet::labelFormat(
            transform = function(x) formatC(as.integer(x), format = "d", big.mark = ",")
          )
        ) |>
        leaflet::addPolygons(
          fillColor   = ~cmp_pal(comparison),
          fillOpacity = 0.80,
          color       = ~ifelse(name == reg, "#000000", "white"),
          weight      = ~ifelse(name == reg, 3, 1),
          layerId     = ~name,
          label       = ~paste0(
            name, ": ",
            dplyr::if_else(is.na(estimate), "no data",
              paste0(
                format_burden_estimate(estimate, lower, upper),
                dplyr::if_else(
                  name == reg, " (reference)",
                  paste0(" (", ifelse(comparison >= 0, "+", ""),
                         formatC(as.integer(comparison), format = "d", big.mark = ","),
                         " vs ", reg, ")")
                )
              )
            )
          ),
          highlightOptions = leaflet::highlightOptions(
            weight = 2, color = "#333", bringToFront = TRUE
          )
        )
    }, ignoreNULL = FALSE)

    # Show/hide idiopleth status label below map
    output$idiopleth_label <- renderUI({
      reg <- selected_region()
      if (!is.null(reg)) {
        div(
          style = paste0(
            "margin-top:4px; padding:4px 10px; background:#fff3cd;",
            " border-radius:4px; font-size:0.82em; color:#555;"
          ),
          tags$strong("Comparing to: "), reg,
          tags$span(style = "margin-left:10px; color:#888;",
                    "— click region again to reset")
        )
      }
    })

    outputOptions(output, "burden_map",        suspendWhenHidden = TRUE)
    outputOptions(output, "national_table",    suspendWhenHidden = TRUE)
    outputOptions(output, "subnational_table", suspendWhenHidden = TRUE)
    outputOptions(output, "idiopleth_label",   suspendWhenHidden = TRUE)
  })
}
