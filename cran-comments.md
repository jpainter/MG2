## Submission

This is a new submission.

MG2 ('Magic Glasses 2') is a 'Shiny' application, with the underlying functions
also usable from scripts, for assessing and analysing routine health data from
'DHIS2' <https://dhis2.org> health information systems: data quality
assessment, outlier detection, adjustment for incomplete reporting, and
time-series evaluation.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for the reviewer

* **Number of imported packages (31).** MG2 ships a complete 'Shiny'
  application in `inst/shiny/`; roughly half of the imports are the 'Shiny'
  stack the app needs at startup ('shiny', 'bslib', 'DT', 'shinyjs',
  'shinyWidgets', 'shinyFiles', 'shinybusy') and the rest are the data and
  time-series packages every analysis tab uses ('dplyr', 'data.table',
  'tsibble', 'fable', 'fabletools', 'ggplot2', 'sf', ...). Packages needed by
  only one optional feature ('terra', 'googledrive', 'ggiraph', 'ellmer',
  'leaflet', ...) are in Suggests and are checked with `requireNamespace()`
  before use, with an install hint or a static fallback in the app.

* **`\dontrun{}` examples.** Only `loginDHIS2()`, `dhis2_get()` and
  `api_data()` use `\dontrun{}`: they require a live 'DHIS2' server and
  credentials. All other examples run against the bundled demo data and write
  only to `tempdir()`.

* **Internet access.** `mg2_pdrlao_setup()` downloads an optional demo dataset
  (about 3 MB) from the package's GitHub release. Its example is in
  `\donttest{}`, caches to `tempdir()`, and catches download failures. Tests
  that contact a third-party server use `skip_on_cran()`. Vignettes do not
  access the internet; code that queries 'DHIS2' is shown with `eval = FALSE`.

* **Files written.** Outside examples and tests, the app stores its settings
  (recently used data folders) under `tools::R_user_dir("MG2", "config")` and
  caches downloads under `tools::R_user_dir("MG2", "cache")`.

* **Console output.** Exported functions print progress only when their
  existing verbosity argument is set (`print`, `verbose`, `msg`, `progress`,
  `.cat`), and report problems with `message()` / `warning()`.

## Test environments

* local macOS (aarch64), R 4.6.2
* Ubuntu 26.04 LTS (x86_64, Docker `rocker/geospatial`), R 4.6.2, `--as-cran --run-donttest`
* win-builder: R-devel (2026-11-14 r90890 ucrt), Windows x86_64
