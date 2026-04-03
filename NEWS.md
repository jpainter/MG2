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
