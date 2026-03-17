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

### Future Phases (planned)

- Phase 3: Data / Formula / Download modules
- Phase 4: DQA module
- Phase 5: Reporting module
- Phase 6: Outlier/Cleaning module
- Phase 7: Evaluation/Forecasting module
- Phase 8: Map / Regions modules
- Phase 9: Full `devtools::check()` pass, vignettes, CRAN prep
