# CLAUDE.md — Magic Glasses 2 (MG2) Package

## Project Overview

**MagicGlasses2 (MG2)** is an R package that wraps an interactive Shiny application for
epidemiological analysis of routine health data from DHIS2 information systems. It also
exposes functions for standalone analyses outside of the Shiny app.

**Author:** John Painter (painter.ja@gmail.com)
**License:** GPL-3
**Version:** 0.1.0 (in development)
**Reference:** [R Packages (2e)](https://r-pkgs.org)

---

## Purpose

Given a DHIS2 database connection or locally downloaded data, MG2 guides analysts through:

1. **Setup** — Configure a local data directory and authenticate with a DHIS2 server
2. **Metadata** — Explore data elements, categories, org units, and indicators
3. **Data** — Build a formula (data dictionary) and download data via the DHIS2 API
4. **DQA** — Assess data quality: reporting completeness, outliers, time-series stability
5. **Reporting** — Adjust for reporting bias; identify consistently reporting facilities
6. **Outliers** — Detect and flag anomalous values using sequential outlier algorithms
7. **Evaluation** — Fit time-series models, decompose trends, and generate forecasts

Functions are also usable in standalone R scripts and notebooks.

---

## Architecture

### Package Structure

```
MG2/
├── R/                         # Exported package functions
│   ├── utils.R                # Date conversion, file I/O, helpers
│   ├── dhis2_auth.R           # DHIS2 authentication (loginDHIS2)
│   ├── run_app.R              # run_mg2() — Shiny app launcher
│   ├── data_functions.R       # Core data manipulation (error_factor, cleanedData, etc.)
│   └── api_data.R             # DHIS2 API data retrieval (api_data)
│
├── inst/shiny/                # Shiny application (sourced by run_mg2)
│   ├── app.R                  # Main entry point
│   ├── directory_widget.R     # Module 1: Data directory selection
│   ├── login_widget.R         # Module 2: DHIS2 authentication
│   ├── metadata_widget.R      # Module 3: Metadata exploration
│   ├── data_widget.r          # Module 4a: Formula/dataset selection
│   ├── formula_widget.r       # Module 4b: Formula builder
│   ├── data_request_widget.R  # Module 4c: Data download
│   ├── dqa_widget.R           # Module 5: Data quality assessment
│   ├── reporting_widget.r     # Module 6: Reporting bias analysis
│   ├── cleaning_widget.r      # Module 7: Outlier detection
│   ├── evaluation_widget_2.R  # Module 8: Trend analysis & forecasting
│   ├── map_widget.R           # Auxiliary: Geographic visualization
│   ├── regions_widget.R       # Auxiliary: Regional analysis
│   ├── facilities_widget.r    # Auxiliary: Facility data table
│   ├── DToptions.R            # DataTable styling helpers
│   ├── chart_module.R         # Reusable chart module
│   └── Instances.xlsx         # Demo DHIS2 instance list
│
├── R/originals/               # Reference copies of original code (not built into package)
├── man/                       # Auto-generated documentation (roxygen2)
├── tests/testthat/            # Unit tests
├── DESCRIPTION                # Package metadata and dependencies
├── NAMESPACE                  # Auto-generated (do not edit)
└── CLAUDE.md                  # This file
```

### Key Design Decisions

- **`R/` functions** are available both inside the Shiny app and in standalone scripts.
- **`inst/shiny/` modules** are Shiny-specific; they use `::` calls to reference package functions.
- **`R/originals/`** is excluded from the package build (listed in `.Rbuildignore`). It contains
  reference copies of code that was progressively refactored into `R/`.
- **No `source()` calls for functions** — the app.R sources only widget module files; all
  analysis functions are loaded via the package namespace.

### Shiny Module Architecture

Each module follows the standard Shiny module pattern:
- `widget_name_ui(id)` — returns UI elements
- `widget_name_server(id, ...)` — returns a reactive list of outputs for downstream modules

Reactive dependency chain:
```
directory_widget → login_widget
               ↘ metadata_widget → data_widget ←→ data_request_widget
                                              ↘ formula_widget
                 regions_widget ←────────────────┘
data_widget → dqa_widget
           → reporting_widget → cleaning_widget → evaluation_widget
```

---

## Outlier Detection Algorithms

Applied sequentially to each time series (each facility × data element):

| Algorithm     | Description                                                   |
|---------------|---------------------------------------------------------------|
| key_entry_error | Values matching mobile phone entry codes (4+ digit patterns) |
| over_max      | Values exceeding a theoretical maximum (e.g., > 31 days/month) |
| mad15         | Values > 15× the median absolute deviation                   |
| mad10         | Values > 10× MAD (applied after mad15 is removed)            |
| seasonal5     | Values > 5× seasonal decomposition residual / MAD            |
| seasonal3     | Values > 3× seasonal decomposition residual / MAD            |

---

## renv Decision

**renv is NOT initialized** for this project. Rationale:

- **renv is for developers, not package users.** End users install MG2 via
  `devtools::install_github()` and get dependencies from `DESCRIPTION` automatically.
- **Previous user issues** were likely from users who encountered renv during development
  builds, not during normal package installation.
- **Current approach:** `DESCRIPTION` declares all package dependencies. The dev environment
  uses standard R package management.
- **Revisit:** If multiple developers need reproducibility guarantees, initialize renv with
  `renv::init()` and add `renv/` to `.gitignore` (or track `renv.lock` only).

---

## Development Workflow

1. Edit functions in `R/`
2. Run `devtools::document()` to regenerate `NAMESPACE` and `man/`
3. Run `devtools::load_all()` to load the package
4. Test with `MG2::run_mg2()` to launch the app
5. Write tests in `tests/testthat/`
6. Run `devtools::check()` before committing

---

## Progress Log

### Phase 1 — Package Foundation (2026-03-17)

**Status:** ✅ Complete

**Changes:**
- Created package structure following R Packages (2e) conventions
- Moved Shiny app from `incl/shiny/` to `inst/shiny/` (correct R package location)
- Created `R/utils.R`: `as.yearmonth()`, `Month_Year()`, `Week_Year()`, `read_file()`,
  `get_date_part()`, `intersect_length()`
- Created `R/dhis2_auth.R`: `loginDHIS2()`, internal `retry()` helper
- Created `R/run_app.R`: `run_mg2()` — the main entry point for users
- Created `inst/shiny/app.R`: clean entry point replacing `MagicGlasses2_ShinyApp.R`;
  removes all `source()` calls for function files; sources only widget modules
- Updated `inst/shiny/directory_widget.R`: removed dependency on `file.dir()`;
  uses input directly
- Updated `DESCRIPTION`: proper title/description, declared all core dependencies
- Added `R/originals/` to `.Rbuildignore` so reference files are not included in the build

**App state:** App opens and UI renders. Directory and Login widgets are functional.
Remaining widgets (Metadata through Evaluation) require additional function migration
in subsequent phases.

**To launch the app:**
```r
devtools::load_all()
run_mg2()
```

---

### Phase 2 — Metadata Module (2026-03-17)

**Status:** ✅ Complete

**Changes:**
- Created `R/dhis2_api.R`:
  - `dhis2_get()` — authenticated DHIS2 API GET with JSON parsing (replaces
    the generic `get()` in originals that conflicted with base R)
  - `ous_tree()` — builds wide-format org unit hierarchy from a flat API list
    (migrated from `R/originals/prepareDataset.R`)
- Created `R/metadata.R`:
  - `clean_invalid_characters()` — strips invisible Unicode from data frames
    before Excel export (migrated from `R/originals/metadata_functions.R`)
  - `truncate_df_chars()` — truncates overlong strings for Excel compatibility
    (migrated from `R/originals/metadata_functions.R`)
- Updated `inst/shiny/metadata_widget.R`:
  - Replaced all 9 bare `get(source_url = url)` calls with
    `dhis2_get(source_url = url, username = username(), password = password())`
  - Fixed `systemInfo` to use `dhis2_get()` instead of raw `GET()`
  - Fixed `geoFeatures` to use explicit `httr::authenticate()` credentials
- Updated `DESCRIPTION`: added `data.table`, `data.tree`, `igraph`, `stringi`
  to `Imports`
- Added tests in `tests/testthat/test-metadata.R` (11 passing)
- Added tests in `tests/testthat/test-dhis2_auth.R`

**Fixed bug:** `clean_invalid_characters()` now uses `which(bad)` instead of
`bad` as subscript index, preventing NA-in-subscript errors on columns with
missing values.

---

### Phase 3 — Setup UX, Data Download, and Core Processing (2026-03-18)

**Status:** ✅ Complete

**Changes:**

*Setup widgets:*
- `inst/shiny/directory_widget.R`: Added `shinyFiles` browse button (cross-platform,
  iCloud + OneDrive aware); text box now starts blank with placeholder; fixed empty-dir
  and `max()` on empty vector warnings
- `inst/shiny/login_widget.R`: Replaced static `Instances.xlsx` picker with live API
  call to `https://api.im.dhis2.org/instances/public`; fixed login check bug
  (`isTRUE(l)` not `class(l) == "logical"`); fixed `system.info` GET to include
  credentials and handle non-JSON responses; added `ns <- session$ns`
- Added `shinyFiles` to `DESCRIPTION` Imports

*Modal dialog fix (metadata + data download widgets):*
- Added `fade = FALSE` to all progress `modalDialog()` calls in `metadata_widget.R`
  (15 modals) and `data_request_widget.R` (6 modals) — prevents Bootstrap CSS fade
  animation queue from leaving modals stuck on screen

*Namespace/filter conflict fix:*
- Removed `@importFrom stats filter` from `api_data_function_revision.R`
- Added `dplyr::filter()` explicit calls in `data_Functions.R` and `regions_widget.R`
- Added `@import data.table` in new `R/zzz.R` (required for `:=` inside package)
- Re-attach dplyr after optional packages in `app.R`

*Migrated to `R/utils.R`:*
- `date_code()` — DHIS2 monthly period code generator (fixed `zoo::as.Date.yearmon()`
  dispatch with `frac=` argument for package namespace)
- `date_code_weekly()` — DHIS2 weekly period code generator (fixed original bugs:
  `currentMonth` → `currentWeek`, `months` → `weeks` in paste)

*New `R/dhis2_api.R` functions:*
- `api_url()` — builds DHIS2 dataValueSets / analytics API URL
- `fetch_get()` — single data request with credential passing and NA fallback;
  coerces `pmap.df[i,]` data frame columns to character

*New `R/prepare_dataset.R`:*
- `translate_dataset_2()` — matches downloaded data to formula elements
- `data_leaves()` — determines effective reporting leaf per org unit × data element
- `data_1()` — main wrapper: translates + joins hierarchy + calls `df_pre_ts`/`df_ts`
- `df_pre_ts()` — adds `Month`/`Week` and `data`/`data.id` columns (data.table fast path)
- `df_ts()` — converts to tsibble, deduplicates

*New `R/outliers.R`:*
- `extremely_mad()` — MAD-based outlier flag with trimmed-SD fallback
- `unseasonal()` — seasonal outlier flag via `forecast::tsclean()`
- `mad_outliers()` — applies `extremely_mad` at 15×, 10×, 5× across all series
- `seasonal_outliers()` — applies `unseasonal` at 5× and 3× per series

*New `R/outlier_summary.R`:*
- `monthly.outlier.summary()` — monthly error counts/proportions by algorithm
- `yearly.outlier.summary()` — annual rollup of combined flag
- `outlier.summary.chart()` — faceted ggplot of monthly error rates

*New `R/dqa_functions.R`:*
- `dqa_reporting()`, `dqaPercentReporting()`, `dqa_reporting_plot()`
- `dqa_outliers()`, `yearly.outlier.summary_plot()`
- `abs_ae()`, `mase()` — custom MASE implementation (not `forecast::accuracy()`)
- `mase_year()`, `dqa_mase()`, `dqa_mase_plot()` — MASE trend across years;
  guards against missing `expected` column

*Regions widget:*
- Moved `regions_widget.R` from `R/originals/` to `inst/shiny/`
- Replaced `stri_trim_both(stri_trans_tolower())` with `trimws(tolower())`

**App state:** End-to-end flow working through DQA tab. Setup, Metadata, Regions,
Data (formula + download), and DQA tabs all functional.

---

### UI Responsiveness (2026-03-18)

**Status:** ✅ Complete

**Changes:**
- `R/run_app.R`: Added `launch.browser = TRUE` default so app always opens in
  system browser rather than the cramped Positron/RStudio viewer pane
- `inst/shiny/app.R`: Replaced `fluidPage` + `navlistPanel` with
  `bslib::page_navbar()` — full-width top nav bar with hamburger collapse on
  small screens; "Data" tab converted to `bslib::nav_menu()` dropdown;
  `shinyjs::useShinyjs()` moved to `header =` argument; `min-width: 1100px`
  (reduced from 1200px since sidebar space is recovered)
- `inst/shiny/metadata_widget.R`: Replaced `fillCol(height=600)` wrapper with
  `tagList()` — removes 600px height cap
- `inst/shiny/directory_widget.R`: Same `fillCol` → `tagList` fix
- `inst/shiny/levels_widget.R`: Same `fillCol` → `tagList` fix

---

### Phase 5 — Reporting, Outliers, and Evaluation Modules (2026-03-19)

**Status:** ✅ Substantially complete (some warnings remain, no blocking errors)

**Changes:**

*`as.yearmonth` consolidation:*
- Removed duplicate `as.yearmonth` definition from `R/data_Functions.R`
- Updated `R/utils.R` version to handle all formats automatically:
  `"%B%Y"` (January2020), `"%b%Y"` (Jan2020), `"%Y %b"` (2025 Jan — tsibble
  `as.character()` output), `"%Y%m"` (DHIS2 format), `"%Y-%m"`, plus zoo generic
  and lubridate fallbacks
- **Fixed bug:** `mostFrequentReportingOUs` returned 0 champion facilities because
  `input$startingMonth` from `selectizeInput` uses tsibble's `"2025 Jan"` format
  which the old `"%B%Y"`-only parser could not handle

*New `R/outlier_summary.R` function:*
- `outlier.summary.tibble()` — formatted display table of outlier flag counts,
  totals, and percentages per detection algorithm; called by `cleaning_widget`

*New functions in `R/data_Functions.R`:*
- `mable_data()` — prepares aggregated time-series data for fable model fitting
  (migrated from `R/originals/TS_Modeling_Functions.R`)
- `dataset()` — splits a tsibble into pre/post-intervention windows (training,
  test, post-yr1/2/3) for forecast evaluation
- `yearly_summary_table()` — colour-coded flextable of yearly totals and
  year-on-year percent change
- `combination_forecasts()` — builds all pairwise/higher-order ensemble model
  combinations from primary forecasts
- `best_fables_accuracy()` — ranks forecast models by SWAPE or other metric
- `tsmodels()` — fits ARIMA/ETS/NNETAR/TSLM/Prophet models and generates
  sample-based forecast paths; optionally includes ensemble combinations
- `model_metrics()` — computes out-of-sample SWAPE per model per replicate
- `modelSelection()` — selects best model by `"synchronize"` or `"optimize"`
- `forecast_diff()` — computes WPE (weighted percent error) per replicate
  (renamed from `diff()` to avoid base R namespace conflict)
- `diff.summary()` — summarises WPE across replicates (mean, SD, median)
- `diffHistogram()` — histogram of WPE distribution with median bin highlighted

*Bug fixes:*
- `tsmodels()`: removed `fabletools::` and `fable.prophet::` qualifiers from
  `trend()` and `season()` inside `model()` formulas — these are "model specials"
  resolved by the fable fitting environment and fail with `::` qualification
- Added `distributional` to `DESCRIPTION` Suggests (used by `combination_forecasts`)

**App state:** End-to-end flow working through Evaluation tab. All major tabs
functional: Setup, Metadata, Regions, Data, DQA, Reporting, Outliers, Evaluation.
Forecast model fitting (ARIMA, ETS, NNETAR) working; TSLM and Prophet models
degrade gracefully with `.safely = TRUE`.

---

### Phase 6 — Validation Rules, README, and Metadata UX (2026-03-26)

**Status:** ✅ Complete

**Changes:**

*Validation Rules tab (new):*
- `R/metadata.R`: Added `fetch_validation_rules()` — fetches all rules from DHIS2
  API with paging, translates UID references (`#{uid}`, `I{uid}`) in left/right
  side expressions to human-readable names using a combined data element + indicator
  + category option combo lookup; stores both translated and raw expressions
- `R/metadata.R`: Added `.translate_vr_expression()` internal UID→name helper
- `inst/shiny/metadata_widget.R`: Added `validationRules` reactive — downloads on
  first fetch, loads preferentially from `.rds` on subsequent sessions
- Added `element_rules_lookup` reactive — maps every UID in rule expressions to its
  rule ID for badge lookup
- Added **Validation Rules** tab with full DT display (all fields, raw expression
  columns hidden)
- Added **Rules badge** to Data Elements tab: elements with validation rules show a
  "View rules" button; clicking opens a modal with all matching rules
- Added **Rules badge** to Indicators tab: same pattern, matching on `I{uid}` references
  in rule expressions (indicators use the same `element_rules_lookup` since all UIDs are
  extracted regardless of `#{}` vs `I{}` wrapper)
- Validation rules saved to metadata `.rds` list and to a new `ValidationRules`
  sheet in the Excel export

*API Resources tab fixes:*
- Resources reactive now fetches on `login()` alone (no longer requires clicking
  "Request Metadata") — fast single call, no modal
- Falls back to saved `.rds` metadata when available (offline-capable)
- Switched from `renderTable` to `DT::renderDT` with clickable `<a>` links
- Resources saved to `.rds` metadata list for future sessions
- Updated explanatory text

*Metadata table display fixes:*
- Removed `fillContainer = TRUE` from all `DT::datatable()` calls in
  `metadata_widget.R` — was conflicting with `scrollY` and preventing proper sizing
- Removed `scrollY` + `scrollCollapse` from all hardcoded DT options; switched to
  `scrollY = "calc(100vh - 300px)"` with `paging = FALSE` so tables fill the
  available viewport height without pagination overflow
- Removed `autoWidth = TRUE` from `DToptions_no_buttons()` — was causing header/
  column misalignment when `scrollX = TRUE` is set (DataTables known bug)
- Standardised `dom = 'ti'` (table + info only) across all metadata tables

*README:*
- Full rewrite following R package GitHub conventions: installation section,
  per-module descriptions, standalone usage example, badges, license

---

### Future Phases (planned)

- Phase 7: Map module
- Phase 8: Full `devtools::check()` pass, vignettes, CRAN prep
