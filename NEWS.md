# MG2 0.1.5

* Added **DQA Consistency tab**: evaluates DHIS2 validation rules against formula
  elements per facility × period; shows annual pass-rate chart, per-rule summary
  table with traffic-light colouring, and facility-period drilldown for failed rules
* Added **primary/secondary element roles** to formula management: formula xlsx
  tracks role per element; secondary elements are pre-unchecked in Reporting and
  Outliers tabs with mismatch warning banner
* Formula widget: **Check for Related Elements** button links validation rules to
  formula elements; editable role column in review table; Delete/Rename Formula and
  Rename File buttons with confirmation modals; save no longer requires a row selection
* Map fixes for bslib 0.9.0 + leaflet 2.2.x: removed `addProviderTiles()` calls
  that crashed page JavaScript; maps use `addTiles()` (OpenStreetMap) by default
* Added **About tab** with live environment panel showing R and key package versions,
  and warnings for known compatibility issues (`check_mg2_dependencies()`)
* Reporting widget: element selector split by role; `count.any` auto-enabled when
  secondary elements are present
* Fixed seasonal outlier furrr workers: explicit `globals` list prevents workers
  from attempting to load the MG2 package
* Fixed `api_data()` update-download for processed `.rds` files and character SUM
  coercion

# MG2 0.1.4

* Added **Dataset Combiner** module (Data → Combine tab): build derived datasets
  (e.g. test positivity rate) from existing processed `.rds` files using Include
  and Ratio steps; output is compatible with the full Reporting → Outliers →
  Evaluation pipeline
* Two-phase outlier detection for combined datasets: Phase 1 inherits flags from
  source data; Phase 2 scans combined values directly
* `error_factor()` safety-initialises all 8 flag columns to prevent crashes on
  combined datasets not yet processed through the Outliers tab
* `dataTotal()` detects ratio variables and uses `sum(num)/sum(den)` aggregation
* `outlier.summary.tibble()` handles `missing_numerator` / `missing_denominator` flags
* Reporting widget aggregation handles ratio columns correctly

# MG2 0.1.3

* Fixed incorrect join behavior in `api_data()` when a formula contains a mix
  of default-category and disaggregated data elements. DHIS2 omits
  `categoryOptionCombo` from the API response for elements using the default
  category combo, leaving `NA` after `bind_rows()`. In the mixed case this
  caused default-element rows to never match in the update comparison (NA ≠ NA
  in dplyr joins), so those elements were always re-downloaded unnecessarily.
  Worse, dropping `categoryOptionCombo` from the join entirely would collapse
  disaggregated elements into a many-to-many match. Fix: replace `NA` with the
  sentinel `"default"` in `current.counts`, `current.values`, and `prev.data`
  before any joins, so all element types match correctly.

# MG2 0.1.2

* Fixed crash when downloading more than one year of data: `api_data()` was
  calling `group_by(categoryOptionCombo)` unconditionally, but that column is
  absent when data has no category disaggregation. Now uses `by_cols` (already
  correctly conditional) for the subsequent `group_by` as well.

# MG2 0.1.1

* Exported 22 package functions that were missing `@export` tags and inaccessible
  to installed-package users (`dataPeriod`, `cleanedData`, `mostFrequentReportingOUs`,
  `groupByCols`, `selectedData`, `dataTotal`, `htsFormula`, `htsData`, `trendData`,
  `model_formula`, `tsPreModel`, `tsPreForecast`, `MAPE`, `key.mape`, `tsModel`,
  `tsForecast`, `getForecast`, `key.mpe`, `plotTrends`, `find_lowest_nonnull`,
  `wpe_summary`, `diffHistogram`)
* Renamed `diff.summary()` to `wpe_summary()` to avoid S3 generic conflict with
  base R `diff()`
* Fixed `key.mpe()` call sites with stale argument names
* Formula files and data files now sorted by modification time (most recent first)
* Fixed `devtools::check()` to 0 ERRORs, 0 WARNINGs

# MG2 0.1.0

* Initial package release
* Shiny app with full workflow: Setup, Metadata, Data, DQA, Reporting,
  Outliers, and Evaluation tabs
* Standalone R functions for data quality assessment, outlier detection,
  reporting bias analysis, and time-series forecasting
