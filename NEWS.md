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
