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

### Cross-tab Reactive Safety: the `cached_rous` Pattern

`reportingSelectedOUs()` in `reporting_widget` has a `req(current_tab() == "Reporting")`
guard. Any reactive that calls it **directly** will fail silently (blank output, no error)
when the user is on any other tab — and the silent failure propagates to every downstream
reactive, including plots in `cleaning_widget` and `evaluation_widget`.

**The fix:** a `cached_rous` reactiveValues stores the last successfully-computed value.
An `observeEvent(reportingSelectedOUs(), ...)` updates the cache; because `observeEvent`
handlers do not fire when the event expression fails via `req()`, the cache — and anything
that reads it — stays stable across tab switches.

```r
# In reporting_widget_server:
cached_rous = reactiveValues(value = NULL)
observeEvent(reportingSelectedOUs(), ignoreNULL = FALSE, {
  cached_rous$value = reportingSelectedOUs()
})
```

**Rule:** Never call `reportingSelectedOUs()` directly inside a reactive or in a function
called from one. Always read `cached_rous$value` instead. Reactives that violated this
rule and were fixed: `selected_data()`, `caption.text()`.

`caption.text()` also had a secondary bug: it compared `reportingSelectedOUs() > 0`
(character vector vs. numeric — alphabetical comparison against "0") instead of
`length(rous) > 0`.

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

### Bug Fixes — Missing NAMESPACE Exports (2026-04-02)

**Status:** ✅ Complete

**Problem:** Functions defined in `R/` but lacking `@export` tags were inaccessible
to installed-package users (i.e. anyone who installed via `devtools::install_github()`
rather than `devtools::load_all()`). The Shiny widgets call these functions as bare
names, which requires them to be exported.

**Changes:**
- Added `@export` + minimal roxygen stub to 22 previously unexported functions:
  - `R/data_Functions.R`: `cleanedData`, `dataPeriod`, `mostFrequentReportingOUs`,
    `groupByCols`, `selectedData`, `dataTotal`, `htsFormula`, `htsData`, `aggData`,
    `trendData`, `model_formula`, `tsPreModel`, `tsPreForecast`, `MAPE`, `key.mape`,
    `pre_impact_fit`, `tsModel`, `tsForecast`, `impact_fit`, `getForecast`, `key.mpe`,
    `plotTrends`
  - `R/api_data_function_revision.R`: `find_lowest_nonnull`
- Regenerated `NAMESPACE` and `man/` (22 new `.Rd` files)

**Note:** `backtick` was intentionally left unexported — it is redefined locally
inside widget function bodies and does not need to be in the package namespace.

**Note:** `aggData`, `pre_impact_fit`, and `impact_fit` were subsequently changed
from `@export` to `@noRd` — they are not called directly from widget files, so
they do not need to be exported.

---

### Bug Fix — Formula file sort order (2026-04-02)

**Status:** ✅ Complete

**Changes:**
- `R/utils.R`: `list_dir_files()` now sorts results by file modification time
  (most recent first) instead of reverse alphabetical name order — ensures
  recently saved/edited files surface first in all file picker inputs
- `inst/shiny/data_widget.r`: Removed duplicate mtime sort that was previously
  applied after `list_dir_files()` (now redundant)

---

### devtools::check() Clean Baseline (2026-04-02)

**Status:** ✅ Complete — 0 ERRORs · 0 WARNINGs · 2 NOTEs

**Changes:**
- `R/api_data_function_revision.R`: Wrapped `@examples api_data()` in `\dontrun{}`
  (was causing check ERROR); changed `dir = country.dir` default to `dir = NULL`
- `DESCRIPTION`: Removed `data.table`, `igraph`, `lubridate` from `Suggests`
  (duplicates of `Imports`); added `URL:` and `BugReports:` GitHub links
- `inst/shiny/`: Deleted 11 leftover session `.rds` files, `~$Instances.xlsx`,
  and old `MagicGlasses2_ShinyApp.R`
- `.Rbuildignore`: Added `.DS_Store`, `.claude`, and `inst/shiny/forecasting schemas.txt`
- Added `@param` entries to all 20 newly exported functions; `dqa_reporting` also fixed
- `R/dqa_functions.R`: Replaced non-ASCII em dash with ASCII hyphen in message string
- `R/data_Functions.R`: Renamed `diff.summary` → `wpe_summary` (R was interpreting
  it as an S3 method for base `diff()` with incompatible signature)
- `R/data_Functions.R`: Fixed two stale `key.mpe()` call sites with wrong argument
  names (`split` → `.split`; `forecastData`/`actualData` → `Forecast_data`/`test_data`)
- `R/prepare_dataset.R`: Removed broken `[metadata_widget_server()]` roxygen cross-reference
- `inst/shiny/evaluation_widget_2.R`: Updated `diff.summary` → `wpe_summary` call

**Remaining NOTEs (Phase D):**
- Shiny packages in `Imports` without `@importFrom` (expected pattern for Shiny apps)
- Bare `dplyr`/`ggplot2` calls in `R/data_Functions.R` without `::` qualification

**Version bumped to 0.1.1** and `NEWS.md` created.

---

### Bug Fixes — Formula Widget Save & Data Request (2026-04-04)

**Status:** ✅ Complete

**Changes:**

*Formula save — replaced download handler with direct server-side save:*
- `inst/shiny/formula_widget.r`: `observeEvent(input$saveData, ...)` now calls
  `openxlsx::saveWorkbook()` directly to overwrite the selected formula xlsx in the
  data directory, instead of showing a modal with a browser `downloadButton`; shows
  "Saving formula..." modal during write, green notification on success, error
  notification on failure
- `inst/shiny/data_widget.r`: Added `formulaFile` to return list (full path of the
  currently selected formula xlsx, used by `formula_widget` as the save target)
- `inst/shiny/data_widget.r`: Renamed label "Select Formula:" → "Select/Add Formula:"

*Auto-refresh after save — no manual Refresh needed:*
- `inst/shiny/app.R`: Created shared `formulaSaved <- reactiveVal(0)` passed to both
  `data_widget_server` and `formula_widget_server`
- `inst/shiny/formula_widget.r`: Increments `formulaSaved` after a successful save
- `inst/shiny/data_widget.r`: `formulas` and `all_formula_elements` reactives depend
  on `formulaSaved` — they invalidate and re-read the xlsx automatically after each
  save, so `formula_elements()` is populated before the user clicks Request Data

*Auto-refresh after download — newly saved rds appears in dataset list automatically:*
- `inst/shiny/data_widget.r`: `data.dir_files()` now depends on `completedRequest()`;
  re-scans the data directory after each completed download without requiring Refresh

*Guard against data request with empty formula:*
- `inst/shiny/data_request_widget.R`: Added early-return guard when
  `length(.elements$name) == 0` — shows informative modal instead of proceeding to
  `api_data()` with no elements

*Fix crash when requesting data with 0 elements (R `1:0` footgun):*
- `R/api_data_function_revision.R`: Changed `for (i in 1:nrow(pmap.df))` →
  `for (i in seq_len(nrow(pmap.df)))` — `1:0` in R yields `c(1, 0)` (two iterations
  with invalid NA indices); `seq_len(0)` correctly yields no iterations

*Fix subscript-out-of-bounds when switching between data elements and indicators:*
- `inst/shiny/formula_widget.r`: Added `observeEvent(input$element_indicator_choice)`
  that calls `DT::selectRows(proxy, NULL)` to clear stale row selections before
  `selected_elements()` fires with indices that exceed the new table's row count

---

### Evaluation Module — Charts, Model Selection, and Performance (2026-04-22)

**Status:** ✅ Complete

**Changes:**

*Performance:*
- `inst/shiny/reporting_widget.r`: Added `cached_selected_data` pattern to
  `selected_data()` — tab guard (`req(current_tab() == "Reporting")`) prevents
  eager recomputation; cache updated via `observeEvent`; downstream widgets read
  `cached_selected_data$value`
- `inst/shiny/cleaning_widget.r`: Added tab guard to `outlier.dataset()` so
  the 2.6M-row outlier pipeline only runs when the Outliers tab is active
- `R/data_Functions.R`: Pre-filter to `orgUnit/Month/data/original` columns
  before `mostFrequentReportingOUs()` to avoid passing unnecessary columns
- Commented out active `saveRDS()` debug calls in reporting and cleaning widgets

*Evaluation chart fixes:*
- `inst/shiny/evaluation_widget_2.R`: Strip `samples` list-column from fable
  before `fabletools::autolayer()` — list columns cause silent ggplot2 failures
- Fixed combination model detection in Phase 2: check `sel_model %in%
  primary_model_names` (`a/e/n/t/p1/p4/p8`) instead of a `"combination_"` prefix
  that does not exist; combination names are concatenated abbreviations (e.g. `aentp1`)
- Replaced `eval_trigger` reactive with direct `observeEvent(input$selected_model)`
  for reliable Phase 2 triggering after the dropdown is first populated
- Added `dropdown_initialized` flag: prevents `wpeValidationTable()` changes
  (from Phase 2 updating `model_output`) from resetting the model selector back to
  `models[1]` every time a model evaluation completes
- Phase 2 modal message: "Estimating prediction with X model..."
- Added diagnostic `cat()` logging to Phase 2 inner observer

*Region filter in evaluation:*
- Added `region_filtered_selected_data()` reactive — applies level-by-level OrgUnit
  filter from `regions_widget_output` to `selected_data()` before it enters
  `mable_data()`; mirrors the pattern used in `dqa_widget.R`

*Stale prediction clearing:*
- `clear_predictions()` helper zeros `model_output$predicted` and sets `done = FALSE`
- Triggered by `mable_Data()` changes (region, reporting, outlier, agg level)
  and by model parameters (`evaluation_month`, `horizon`, `replicates`, `ensemble`,
  `transform`); keeps validation table visible but clears charts

*Annual Change table:*
- `R/data_Functions.R` — `yearly_summary_table()`: when most recent year is partial
  (< 12 months), all previous years show full-year totals and full-year % change;
  the most recent year shows its partial total and its % change is computed against
  the same calendar months of the prior year only; year label and column header
  note the restricted period (e.g. "2024 (Jan–Aug)")
- Fixed `&&` scalar coercion error: `as_tibble()` before filter+sum drops tsibble
  key grouping; `isTRUE()` for safe scalar logical test
- Fixed "Unsupported index type: character" error: `as_tibble()` inserted after
  `index_by/summarise` so `Year` (former tsibble index) can be safely relabeled

---

### Dataset Combiner — New Module (2026-04-30)

**Status:** ✅ Complete

**New files:**
- `R/combine_functions.R` — all pipeline logic (no Shiny dependency)
- `inst/shiny/combine_widget.R` — Shiny module (`combine_widget_ui` / `combine_widget_server`)
- Accessible via **Data → Combine** tab

**What it does:**
Lets users construct new derived datasets (e.g. Test Positivity Rate) from existing
processed `.rds` files without writing code.  Each step is either an **Include**
(filter + optional rename) or a **Ratio** (numerator ÷ denominator, full outer join).
Results are saved as standard MG2 processed datasets compatible with the full
Reporting → Outliers → Evaluation pipeline.

**Key functions in `R/combine_functions.R`:**
- `read_combine_meta()` — reads `data_values`, `cleaning_levels`, `period_type`
  from a source file without loading the full dataset (used to populate UI pickers)
- `apply_combine_cleaning()` — applies cumulative outlier cleaning at a user-chosen
  severity level (`none` / `mad15` / `mad10` / `seasonal5` / `seasonal3`)
- `execute_include_step()` — filters + renames one source variable; carries outlier
  flag columns through from source data (Phase 1 inheritance)
- `execute_ratio_step()` — full outer join of numerator and denominator; sums
  components per orgUnit × period before dividing (never sums ratio values);
  produces `missing_numerator` and `missing_denominator` flag columns; carries and
  combines (OR) outlier flags from both sides
- `build_combined_dataset()` — orchestrates all steps, initialises flag columns,
  applies per-step `max_value` → `over_max`, runs two-phase outlier detection,
  joins ousTree; accepts optional `.progress` callback for Shiny `withProgress`
- `register_combined_formula()` — appends the combo name to a `Formulas_*.xlsx`
  "Formula" sheet so `data_widget` can find the output file; handles Excel Table
  objects (removes before `writeData`)
- `suggest_combine_filename()` — sanitises combo name to a valid filename while
  preserving special characters (`>=` etc.) so `grepl()` matching in `data_widget` works
- `save_combine_definition()` / `load_combine_definition()` — persist step
  definitions to `Combinations_[name]_[date].rds` companion files

**Two-phase outlier detection for combined datasets:**
- *Phase 1 (inheritance)*: `execute_include_step` and `execute_ratio_step` carry
  flag columns (`over_max`, `mad15`, `mad10`, `seasonal5`, `seasonal3`) from source
  data.  For Ratio steps, flags are aggregated with `any()` per orgUnit × period on
  each side, then combined with OR after the merge.  Inherited `TRUE` is never cleared.
- *Phase 2 (direct scan)*: `build_combined_dataset` calls `.scan_series_outliers()`
  (internal, via `data.table` `by`) on the combined values using `smallThreshold = 0`
  (the default of 50 would skip all ratio series in [0,1]).  Respects hierarchy:
  `over_max` → stop; `mad15` → skip `mad10`; `mad10` → skip seasonal.
  Results are OR-ed with Phase 1 inherited flags.

**Pipeline compatibility fixes (required by combine output):**
- `R/data_Functions.R` — `error_factor()`: safety-initialises all 8 flag columns
  (`key_entry_error`, `over_max`, `mad15`, `mad10`, `seasonal5`, `seasonal3`,
  `missing_numerator`, `missing_denominator`) to `FALSE` if absent — prevents crash
  when a combined dataset hasn't been through the Outliers tab
- `R/data_Functions.R` — `dataTotal()`: drops `group_by_cols` not present in
  the data (combined datasets don't have `dataSet`); detects ratio variables via
  `numerator`/`denominator` columns and uses `sum(num)/sum(den)` aggregation instead
  of `sum(ratio)` — correct rollup across facilities
- `R/outlier_summary.R` — `outlier.summary.tibble()`: adds `missing_numerator` and
  `missing_denominator` to default `cols`; computes N as
  `max(non-missing numerator count, non-missing denominator count)` for ratio data
- `inst/shiny/reporting_widget.r` — `aggregateselected_data`: intersects
  `group_by_cols` with `names(.d)` before tsibble key construction; when ratio
  columns present, aggregates `numerator` and `denominator` separately via
  `aggregate_key()` then recomputes `total = numerator / denominator`

**UX:**
- Step definitions persist to `Combinations_*.rds` (load with spinner notification
  in lower-right corner, non-blocking)
- Build-once: preview caches the result; Build & Save reuses it if step count matches
- `withProgress()` + `.progress` callback: stage labels (loading, scanning, complete)
  appear in real-time in the Shiny progress dialog during build
- Post-build log shown in a scrollable `verbatimTextOutput` box (max-height 250px)
- Formula registration appended to selected `Formulas_*.xlsx` so output appears
  in Data → Formula immediately

**Version bumped to 0.1.4.**

---

---

### Primary/Secondary Elements, DQA Consistency, and Map Fixes (2026-05-02)

**Status:** ✅ Complete

#### Formula: Primary/Secondary element roles

- `inst/shiny/data_widget.r`: `all_formula_elements()` adds `role = "primary"` fallback for legacy xlsx files lacking the column
- `inst/shiny/formula_widget.r`:
  - `role` column persisted to xlsx; defaults to `"primary"` on load and when adding new elements
  - **"Check for Related Elements"** button: scans validation rules for UIDs linked to formula elements → modal with checkboxGroupInput → selected elements added as `"secondary"`
  - Review table DT: editable `role` column (green = primary, orange = secondary); force-expands when related elements are added
  - `Formula.Name` column hidden from display (kept internally for save logic)
  - `role` moves to first column; elements sorted by role → dataElement → Categories
  - **Save bug fixed:** `selectedElementNames()` (which required a browse-table row selection) replaced by `.formula_string_from_df()` that builds the formula string from `updated_formula_elements$df` directly
  - **Delete Formula**, **Rename Formula**, **Rename File** buttons with confirmation modals
  - Rename Formula: offers to rename matching `.rds` dataset files with a second confirmation modal
  - `Rows in dataset` column: joined from `data1()` counts per element-category when a dataset is loaded
- `R/combine_functions.R`: combined datasets default all elements to `role = "primary"`

#### Selector consistency across tabs

- `inst/shiny/reporting_widget.r`:
  - `data_categories` checkboxes split by role: primary pre-checked, secondary unchecked with `(secondary)` label
  - Auto-checks `count.any` with notification when secondary elements detected
  - Re-initialises when formula changes; exported `selected_data_categories` reactive to return list
  - `count.any` UI default changed to `TRUE`
  - Label text updated to explain primary/secondary reporting logic
- `inst/shiny/cleaning_widget.r`:
  - Element selector populated from `data1()` (not `selected_data()`) so all elements — including secondary — are visible
  - Primary pre-checked, secondary unchecked; **mismatch warning banner** if outlier-tab element is not checked in reporting
- `inst/shiny/evaluation_widget_2.R`: read-only element display panel in the Data sidebar (primary/secondary labels)

#### DQA: element selector + Consistency tab

- `inst/shiny/dqa_widget.R`: full rewrite — `sidebarLayout` with element selector (primary default); new **Consistency** tab
- `R/dqa_functions.R`: new functions:
  - `.extract_vr_uid_pairs`, `.vr_expr_to_r`, `.apply_vr_operator` — validation rule expression utilities
  - `dqa_consistency()` — evaluates rules per facility × period; accepts `filter_data_ids` to restrict to selected elements; marks rules as `incomplete` when referenced elements are absent from formula
  - `dqa_consistency_plot()` — annual % of rule evaluations passing (same style as other DQA charts)
  - `dqa_consistency_table()` — per-rule summary with traffic-light % colouring
  - `dqa_consistency_detail_rule()` — drilldown of failed facility-periods for one rule

#### Bug fixes

- `R/api_data_function_revision.R`:
  - **Update download on processed file:** detected processed `.rds` files (have `Month`/`Week`, no `period`) and reconstructed raw `period`/`COUNT`/`SUM` format before the update merge
  - **Character SUM:** `sum(as.numeric(SUM), ...)` in the period-comparison aggregation
- `R/outliers.R` (`seasonal_outliers`):
  - Seasonal outlier furrr workers no longer try to load MG2 package: explicit `globals = list(...)` in `furrr_options`; `environment(.unseasonal) <- baseenv()`; `mad()` → `stats::mad()`

#### README / About tab

- `README.md`: expanded Reporting section explaining primary/secondary elements and the two counting modes
- `inst/shiny/reporting_widget.r`: updated label and checkbox text for `count.any`
- `inst/shiny/about_widget.R`: added server component; live **Environment** panel shows R + key package versions; yellow warning panel for known compatibility issues
- `R/utils.R`: `check_mg2_dependencies()` — checks R version and key package versions, returns info/warnings for startup notifications and About tab display

#### Map fixes (bslib 0.9.0 + leaflet 2.2.x compatibility)

- **Root cause:** leaflet-providers plugin JS (`L.tileLayer.provider`) fails with `module` error in newer leaflet 2.2.x; crashing all page JavaScript (no polygons, no DT tables, no Shiny UI updates)
- **Fix:** removed all `addProviderTiles()` calls from `regions_widget.R` and `reporting_widget.r`; maps now use plain `addTiles()` (OpenStreetMap) only. Provider tiles return automatically once `install.packages("leaflet.providers")` is run — `check_mg2_dependencies()` warns if the package is absent
- `inst/shiny/regions_widget.R`:
  - `fillPage` → `tagList`; single-tab `tabsetPanel` removed (Bootstrap 5 / bslib 0.9.0 hid tab content)
  - `inputPanel` → `div(style="display:flex...")` (Bootstrap 5 removed `well` CSS class)
  - `geoFeatures.ous()` semi_join restored to original structure
  - `fitBounds` added using `sf::st_bbox`; `names(level.colors)` tibble-subscript bug fixed
- `inst/shiny/app.R`: startup call to `check_mg2_dependencies()`; CSS override for leaflet SVG overflow retained for safety

---

### Future Phases (planned)

- Phase 7: Map module
- Phase B: Tests (~95 tests across 6 files, no live DHIS2 server needed)
- Phase C: Demo data (`data/mg2_demo.rda`) + vignettes
- Phase D: `cat()` → `message()` sweep; `@importFrom` for dplyr/ggplot2; final CRAN polish
