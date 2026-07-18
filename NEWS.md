# MG2 0.1.9

* **DQA: Active/Inactive Facility tab** — new "Facilities" top-level tab within DQA shows
  a stacked horizontal bar chart (% active vs inactive per district) and a summary table.
  A facility is "active" if it reported at least one non-zero value anywhere in the dataset.
* **DQA: Facility Reporting Heatmap** — new sub-tab under Reporting shows a per-district
  facility × month grid; blue = reported, grey = no report. District dropdown keeps the plot
  readable regardless of facility count; height scales automatically to the number of facilities.
* **DQA: Reporting Map now works with demo data** — Sierra Leone district polygons
  (`mg2_demo_geo`, sourced from GADM) are shipped with the package and injected into the demo
  metadata at setup time, so the DQA map and Climate map render without a live DHIS2 connection.
* **DQA: Reporting map no longer blocked by missing geo** — `dqa_region_reporting()` no longer
  requires `geoFeatures()` to compute the reporting aggregation; the map renderer guards geometry
  separately, so the reporting table is available even when polygons are absent.
* **Combine: negative-value clipping** — `execute_ratio_step()` clips negative derived values
  to 0 and emits a warning with the count; prevents impossible values propagating downstream.
* **Burden Estimate UX** — "Run Estimates" button and progress log moved into the Model tab
  (no longer visible while browsing Data); "Years to include" replaced with inline checkboxes
  (all years pre-checked, easy to deselect individual years); fixed blank Results table and Map
  for single-category runs where no "Total" row is added.

# MG2 0.1.8

* **Climate tab** — new CHIRPS rainfall module: download, extract, and map monthly rainfall
  for any DHIS2 org unit hierarchy. Downloads only the bounding box region (~3–5 MB for Africa;
  auto-selects Africa vs global). Two-level cache (TIF + extracted sf) means repeat runs are instant.
  Outputs: faceted choropleth, interactive leaflet, bar chart, statistics table, CSV/Excel download.
* **Climate: multi-year and anomaly modes** — year-range selector runs multiple years sequentially;
  Anomaly tab shows diverging RdBu choropleth (mm deviation or z-score vs multi-year mean).
* **Climate: rainfall covariate export** — after every run, writes `rainfall_{year}_lvl{level}.rds`
  to the data directory for use as a covariate in Evaluation models.
* **Climate overlay in Evaluation** — sidebar toggle adds rainfall bars to forecast charts when
  rainfall files are present in the data directory; national average fallback when district join fails.
* **Burden Estimate tab (dev preview)** — new tab for burden estimation: Method A (champion
  multiple), B (attendance-based), C1/C2/CA (linear/ARIMA/cascade imputation), D (care-seeking
  adjustment), E (Thwing et al. adjusted incidence). Results by region, year, and category.
* **QS serialization** — `qs2` replaces FST as the default file format (`mg2_data_ext()` returns
  `"qs"`); `save_file()` / `read_file()` support `.qs`, `.rds`, and legacy `.fst`. QS preserves
  all R classes (including `yearmonth`) without post-read fixup; FST had persistent class-stripping bugs.
* **Evaluation: per-region model fits** — when stratified, each region gets its own model fit and
  faceted chart; adaptive x-axis breaks; default champion pre-selected.
* **Reporting: sub-tabs** — Summary split into Reports Received and Total Value Reported sub-tabs;
  element selector reordered (primary elements first).
* **Outliers: Facilities tab** — champions and non-champions listed with filter; alert banner when
  no champion is found.
* **DQA: SWAPE metric** — MASE tab replaced with SWAPE (Symmetric Weighted Absolute Percentage
  Error) for a more interpretable model fit score.
* **DQA: Consistency speedup** — pre-aggregate + filter before wide pivot; large datasets no longer
  stall the browser.
* **Metadata: DT export buttons** — Copy/Print/Download buttons added to all metadata tables;
  large tables use server-side paging for fast rendering.
* **Regions: Level 6 support** — Level 6 `selectInput` cascades from Level 5; all downstream
  widgets (DQA, Reporting, Outliers, Evaluation) filter and display level-6 selections.
* **PDR Lao demo data** — `mg2_pdrlao_*` datasets (82 elements, ~57 months) and
  `mg2_pdrlao_setup()` added; Login tab has dual demo buttons (Sierra Leone / PDR Lao).
* **Deploy: Connect Cloud** — `deploy/app.R` auto-detects OS for PPM binary URL; installs
  `qs2`, `future.apply`, `feasts`, `ggtime` from PPM; SHA-based reinstall check means no
  version bumps are needed to trigger redeployment.

# MG2 0.1.7

* **Evaluation: adaptive x-axis** — breaks chosen automatically based on series length; avoids
  label crowding for long time series.
* **Evaluation: default Champion** — Champion facility pre-selected when Evaluation tab opens,
  matching the most common workflow.
* **UX guided workflow** — blue step-hint bars on Metadata and Regions tabs point to the next
  step; empty-state banners on DQA and Reporting when no data is loaded; Welcome page expanded
  with full tab descriptions and dev-tab callouts.
* **Setup: "Browse Metadata" button** — appears once a directory is set, giving one-click
  navigation to the Metadata tab.
* **Demo modal redesign** — two-stage modal (setup → ready); OK button navigates directly to
  Metadata tab.
* **DQA: Reporting Map fixes** — blank map and persistent notification resolved.

# MG2 0.1.6

* **File format: RDS replaces FST** — default serialization switched from FST to uncompressed
  RDS to fix persistent `yearmonth` class-stripping bugs in FST; `read_file()` still reads
  legacy `.fst` files.
* **shinyapps.io deployment** — `deploy/` directory with `app.R` and manifest; demo mode via
  `MG2_DEMO_MODE=1` environment variable; shinyFiles picker replaced with demo-button UI in
  deployed instances.
* **Connect Cloud deployment** — `deploy/app.R` installs MG2 from the GitHub repo at startup;
  SHA-based check avoids reinstalling on every page load.
* **PDR Lao demo** — first version of `mg2_pdrlao_*` demo data added.
* **yearmonth encoding robustness** — `read_file()` detects legacy months-since-epoch encoding
  and converts to current days-since-epoch format; guards added throughout DQA and data widget.

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
