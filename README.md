<img src="logo.png" align="right" width="140"/>

# MG2: Magic Glasses 2

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

**MG2** is an R package that wraps an interactive Shiny application for epidemiological analysis of routine health data from [DHIS2](https://dhis2.org/) information systems. It guides analysts through the full cycle of data quality assessment, outlier detection, reporting bias adjustment, and time-series evaluation. All analysis functions are also available for use in standalone R scripts and notebooks.

---

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("https://github.com/jpainter/MG2")
```

### System requirements

The core app requires the packages listed in `DESCRIPTION` under `Imports`. For full functionality — including time-series modeling, forecasting, and map visualization — also install the suggested packages:

```r
remotes::install_github("https://github.com/jpainter/MG2", dependencies = TRUE)
```

---

## Getting started

Launch the Shiny app with:

```r
library(MG2)
run_mg2()
```

The app opens in your system browser. Work through the tabs from left to right — each tab builds on the output of the previous one.

---

## Workflow overview

```
Setup → Metadata → Data → DQA → Reporting → Outliers → Evaluation
```

---

## Modules

### 1. Setup

Configure a local data directory and authenticate with a DHIS2 server.

- Select a local folder where metadata and data files will be stored. The folder must exist before it is used.
- Enter DHIS2 credentials manually or pick from a list of saved instances. By default the app loads `Instances.xlsx`, which includes the public DHIS2 demo servers. To add your own instances, copy the file as `_Instances.xlsx` and add credentials there — if the app finds `_Instances.xlsx`, it loads that instead.
- Login is only required when actively downloading metadata or data. Previously downloaded files load automatically so you can work offline.

### 2. Metadata

Browse and download the DHIS2 metadata needed to define your analysis.

- Press the download button (login required) to retrieve data elements, categories, organisation units, indicators, and other metadata from the connected DHIS2 instance.
- Download progress is shown in real time.
- Save metadata to your local folder. It is stored as a multi-sheet Excel file that can be reviewed outside the app.
- On subsequent sessions the most recent metadata file is loaded automatically; re-downloading is only needed when the server metadata changes.
- Use the per-column search boxes and the global search box together to locate specific items — for example, set the period column to "monthly" and the data element column to "confirmed" when searching for monthly confirmed malaria cases.

### 3. Data

Define the data elements for analysis and download data from the DHIS2 server.

**Formula builder** — Before downloading data, build a formula (data dictionary) that specifies which data elements and category-option combinations to include. Formulas are saved as dated Excel files and can be extended over time.

**Data download** — Select an existing formula file, then request data. The app compares national-level totals in the local file against the server for each time period and only re-downloads periods where values have changed, saving substantial time on large datasets.

### 4. Data Quality Assessment (DQA)

Assess three dimensions of data quality before proceeding to analysis.

- **Reporting completeness** — The proportion of expected facility-months that have at least one report. Shown as a time trend and by organisation unit level.
- **Outliers** — A summary of flagged values by detection algorithm and time period, giving an early view of data irregularities before detailed cleaning.
- **Forecast accuracy (MASE)** — Mean Absolute Scaled Error of a naive seasonal forecast, used as a signal of time-series stability. A rising MASE over time may indicate structural changes or data quality deterioration.

### 5. Reporting

Identify facilities that report consistently and adjust analyses for reporting bias.

- Facilities are classified by their reporting regularity over the selected time window.
- The "champion facilities" subset — those reporting in every period — can be used to compute bias-adjusted national totals.
- Outputs include a reporting regularity table and adjusted trend charts.

### 6. Outliers

Detect and flag anomalous values using a sequential set of algorithms, each applied to the residuals left after the previous step. Algorithms are applied separately to each facility × data element series.

| Algorithm | Description |
|---|---|
| `key_entry_error` | Values matching mobile phone entry code patterns (4+ repeated digits occurring more often than chance) |
| `over_max` | Values exceeding a domain-specific maximum (e.g., > 31 days/month for stock-out data) |
| `mad15` | Values more than 15× the median absolute deviation (MAD) from the series median |
| `mad10` | Values more than 10× MAD, applied after `mad15` values are removed |
| `seasonal5` | Values where \|observed − expected\| / MAD ≥ 5, using a seasonal decomposition expected value |
| `seasonal3` | Same metric ≥ 3, applied after `seasonal5` values are removed |

MAD-based detection is preferred over mean ± SD because it is robust when multiple outliers inflate the standard deviation. Seasonal detection uses `forecast::tsclean()` to compute expected values, catching values that are within the overall range of the series but anomalous for their time of year.

### 7. Evaluation

Fit time-series models to evaluate trends and measure the effect of programmatic interventions.

The evaluation module follows the interrupted time-series framework described by [Linden (2015)](https://doi.org/10.1097/MLR.0000000000000216). Models are fit with the [fable](https://fable.tidyverts.org/) ecosystem (`tsibble`, `feasts`, `fable`).

- **Models** — ARIMA, ETS, NNETAR, TSLM, and Prophet are fit to each series. Ensemble combinations are built from the primary models and ranked by out-of-sample accuracy.
- **Model selection** — Choose between `"synchronize"` (same model type across all facilities for comparability) or `"optimize"` (best model per facility).
- **Intervention evaluation** — The pre-intervention window is used as training data. Forecast accuracy over the post-intervention window is measured as Weighted Percent Error (WPE), summarised across replicates and displayed as a histogram.
- **Hierarchical forecasting** — When data are structured by organisation unit hierarchy and/or categories (age, sex), models can be fit at each level and reconciled upward using `fabletools::reconcile()`.

---

## Standalone use

All analysis functions are available outside the Shiny app:

```r
library(MG2)

# Authenticate with a DHIS2 server
session <- loginDHIS2(url = "https://play.dhis2.org/40/",
                      username = "admin",
                      password = "district")

# Retrieve data via the API
df <- api_data(url = session$url,
               username = session$username,
               password = session$password,
               formula = my_formula,
               periods = c("202301", "202302", "202303"))
```

---

## Contributing

Bug reports and feature requests are welcome via [GitHub Issues](https://github.com/johnpainter/MG2/issues). Pull requests are also welcome — please open an issue first to discuss proposed changes.

---

## License

GPL-3 © John Painter
