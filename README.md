<img src="logo.png" align="right" width="140"/>

# MG2: Magic Glasses 2

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

**MG2** is an open-source R package and Shiny application for epidemiological analysis of routine health data from [DHIS2](https://dhis2.org/) information systems.

National health information systems like DHIS2 generate rich longitudinal records of cases, tests, treatments, and commodities across thousands of facilities. But these data, like all raw operational data, reflect the circumstances under which they were collected: facilities with incomplete or missing reports, commodity stockouts that temporarily suppress case counts, data entry errors, health worker strikes, and periodic data recategorizations. These are expected features of any large-scale reporting system — not failures — and like any operational data system, they require processing before analysis. The [WHO Data Quality App](https://dhis2.org/who-data-quality-tool/) instructs analysts to adjust for incomplete reporting before trend analysis, but provides no tool to do so.  Yet most country programs skip this step entirely, drawing conclusions directly from raw data. Converting operational records into reliable indicators is a distinct technical step — one that routine DHIS2 workflows still lack.

**MG2 fills that gap.** It identifies consistently-reporting facilities, removes data entry artifacts, adjusts indicators for missing reports, and fits confounder-adjusted time-series models with percent-change estimates and confidence intervals. All analysis functions are also available for use in standalone R scripts and notebooks.

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("https://github.com/jpainter/MG2")
```

For full functionality — including time-series modeling, forecasting, and map visualization — also install suggested packages:

```r
remotes::install_github("https://github.com/jpainter/MG2", dependencies = TRUE)
```

---

## Data file storage

MG2 saves processed datasets as **`.fst`** files when the [`fst`](https://www.fstpackage.org/) package is installed, falling back to **`.rds`** otherwise. FST is a binary columnar format that reads and writes 5–10× faster than compressed RDS — important when working with national-level datasets that can reach several hundred megabytes.

`fst` is installed automatically with `dependencies = TRUE` (see above). To install it separately:

```r
install.packages("fst")
```

MG2 selects the format at runtime — no configuration needed. New downloads and processed files are saved with the `.fst` extension when `fst` is available.

**Older `.rds` files continue to work.** The app detects the extension and reads either format transparently via `read_file()`. Files downloaded before `fst` was installed load without any changes.

**Metadata files always remain `.rds`** — FST only handles flat data frames. The metadata objects (`metadata_*.rds`, `geoFeatures_*.rds`) contain spatial objects and nested lists that FST cannot serialize.

---

## Getting started

```r
library(MG2)
run_mg2()
```

The app opens in your system browser. Work through the tabs from left to right — each tab builds on the output of the previous one.

---

## Workflow

```
Setup → Metadata → Data → DQA → Reporting → Outliers → Evaluation
                  (Formula / Download / Combine)
```

---

## Modules

### 1. Setup

Configure a local data directory and authenticate with a DHIS2 server.

- Select a local folder where metadata and data files will be stored.
- Enter DHIS2 credentials manually or pick from a list of saved instances. By default the app loads `Instances.xlsx`, which includes the public DHIS2 demo servers. To add your own instances, copy the file as `_Instances.xlsx` and add credentials there — if the app finds `_Instances.xlsx`, it loads that instead.
- Login is only required when actively downloading metadata or data. Previously downloaded files load automatically so you can work offline.

### 2. Metadata

Browse and download the DHIS2 metadata needed to define your analysis.

- Press the download button (login required) to retrieve data elements, categories, organisation units, indicators, validation rules, and other metadata from the connected DHIS2 instance.
- Save metadata to your local folder as a multi-sheet Excel file that can be reviewed outside the app.
- On subsequent sessions the most recent metadata file is loaded automatically; re-downloading is only needed when the server metadata changes.
- Data elements and indicators that appear in validation rules display a **View rules** badge — click to see all matching rules in a modal.

### 3. Data

Define the data elements for analysis, download data from the DHIS2 server, and build derived datasets.

**Formula builder** — Build a formula (data dictionary) specifying which data elements and category-option combinations to include. Formulas are saved as dated Excel files and can be extended over time.

**Data download** — Select a formula file, then request data. The app compares national-level totals in the local file against the server for each period and only re-downloads periods where values have changed, saving time on large datasets.

**Combine** — Build derived datasets from existing downloaded data — without writing code. Construct new indicators (e.g. test positivity rate, stock-out rate) by composing steps over one or more source `.rds` files. Each step is either an **Include** (filter and optionally rename a source variable) or a **Ratio** (numerator ÷ denominator, joined across sources). The resulting dataset is a standard MG2 processed file, fully compatible with the DQA → Reporting → Outliers → Evaluation pipeline.

- **Two-phase outlier inheritance** — Outlier flags from source datasets carry forward into the combined output. For ratio steps, flags from numerator and denominator are merged with OR logic. A second direct scan then runs on the combined values.
- **Correct ratio aggregation** — When rolling up across facilities, the module sums numerator and denominator separately before dividing, rather than averaging ratio values — avoiding the aggregation bias common in naive implementations.
- **Missing component flags** — `missing_numerator` and `missing_denominator` columns are added automatically wherever one component was absent for a given facility × period.
- **Persistent step definitions** — Step configurations are saved to a companion `.rds` file so a pipeline can be reloaded and re-run as new data are downloaded.
- **Auto-registration** — The combined dataset name is appended to the selected `Formulas_*.xlsx` so it appears immediately in the Data tab without restarting the app.

### 4. Data Quality Assessment (DQA)

Assess four dimensions of data quality before proceeding to analysis.

- **Reporting completeness** — The proportion of expected facility-months with at least one report, shown as a time trend and by organisation unit level.
- **Outliers** — A summary of flagged values by detection algorithm and time period.
- **Forecast accuracy (MASE)** — Mean Absolute Scaled Error of a naive seasonal forecast, used as a signal of time-series stability. A rising MASE over time may indicate structural changes or data quality deterioration.
- **Consistency** — Evaluates DHIS2 validation rules against the formula elements for each facility × period. Displays an annual pass-rate chart, a per-rule summary table with traffic-light colouring, and a facility-period drilldown for failed rules. Rules that reference elements not present in the formula are flagged as incomplete rather than failed, so the consistency score reflects only rules that can actually be evaluated with the current data.

### 5. Reporting

Identify facilities that report consistently and adjust analyses for reporting bias.

- Facilities are classified by their reporting regularity over the selected time window.
- The **"champion facilities"** subset — those reporting in every period — is used to compute bias-adjusted national totals.
- Outputs include a reporting regularity table and adjusted trend charts.

#### What counts as "reported"?

A facility-period is counted as *reported* when it satisfies the selected criterion.
The criterion is set in the **Reporting Consistency** sidebar under *"Count a facility as reporting when:"*.
Choosing the right rule matters because it determines which facilities become **champion facilities**
— the consistently-reporting subset used to adjust for reporting bias throughout the pipeline.

| Rule | Description | When to use |
|---|---|---|
| **All selected elements — every category present** *(default)* | Every selected data element *and* every one of its category-option combinations must have a non-missing value in the period. | Use for indicators analysed as **ratios**, such as test positivity rate (confirmed cases ÷ persons tested). If either component is missing, the ratio is undefined, so only facilities that reported every required piece of data should be considered "reporting." |
| **All selected elements — at least one category** | Every selected data element must have at least one non-missing value, but not all category-option combinations are required. | Use when your data element has sex- or age-disaggregated categories and partial disaggregation is acceptable — e.g. total malaria cases where male + female sub-totals may not always sum to the reported total. |
| **Any selected element present** | A facility-period counts as reported if *any* of the checked data elements has a non-missing value. | Suitable when all checked elements are meaningful indicators and a facility that submitted any one of them should be credited with reporting. |
| **Any data element present (including unchecked)** | A facility-period counts as reported if *any* data element in the dataset — including those that are unchecked in the element selector — has a non-missing value. | Use when the primary data element of interest may legitimately be zero and zeros are stored as missing rather than as `0`. For example, in a low-transmission setting, districts with no confirmed malaria cases may submit nothing for the cases element but still submit outpatient attendance or fever counts. Using attendance as the reporting surrogate ensures that low-burden but active facilities are classified as champions. Also appropriate when your formula includes **secondary** elements (see below) that reliably indicate facility activity even when the primary element is absent. |

**Practical examples:**

- *Test positivity rate (confirmed cases ÷ persons tested)*: choose **All selected elements — every category present** so that only facilities with both numerator and denominator are classified as consistently reporting. A facility that submitted case counts but not testing numbers produces an undefined ratio and should not anchor the champion distribution.

- *Confirmed malaria cases in a setting where "zero cases" is reported as missing*: choose **Any data element present** and include outpatient attendance as a secondary element in your formula. A facility submitting attendance but no case entry is still an active reporter; excluding it would under-count champions and inflate the reporting-bias adjustment.

- *A single case-count element without disaggregation*: the default **Any selected element** is appropriate.

#### Primary and secondary elements

Formula elements can be designated **primary** (the indicators you are analysing and counting toward totals) or **secondary** (supporting context elements — e.g. a denominator or a related indicator pulled in via a validation rule).

- Only **primary** elements are checked by default in the element selector and counted in totals.
- **Secondary** elements appear in the selector unchecked and can be included in the "any data" reporting check without inflating the primary total.
- When secondary elements are detected, "Count facility as reporting if any data submitted" is enabled automatically.

To assign roles, go to **Data → Formula → Review tab** and edit the `role` column (primary / secondary), or use the **Check for Related Elements** button to find elements linked via DHIS2 validation rules.

### 6. Outliers

Detect and flag anomalous values using a sequential set of algorithms, each applied to the residuals left after the previous step. Algorithms are applied separately to each facility × data element series.

| Algorithm | Description |
|---|---|
| `key_entry_error` | Values matching mobile phone entry code patterns (4+ repeated digits) |
| `over_max` | Values exceeding a domain-specific maximum (e.g. > 31 days/month for stock-out data) |
| `mad15` | Values more than 15× the median absolute deviation from the series median |
| `mad10` | Values more than 10× MAD, applied after `mad15` values are removed |
| `seasonal5` | \|observed − expected\| / MAD ≥ 5, using a seasonal decomposition expected value |
| `seasonal3` | Same metric ≥ 3, applied after `seasonal5` values are removed |

MAD-based detection is preferred over mean ± SD because it is robust when multiple outliers inflate the standard deviation. Seasonal detection uses `forecast::tsclean()` to compute expected values, catching values that are within the overall range of the series but anomalous for their time of year.

### 7. Evaluation

Fit time-series models to evaluate trends and measure the effect of programmatic interventions.

The evaluation module follows the interrupted time-series framework described by [Linden (2015)](https://doi.org/10.1097/MLR.0000000000000216). Models are fit with the [fable](https://fable.tidyverts.org/) ecosystem (`tsibble`, `feasts`, `fable`).

- **Models** — ARIMA, ETS, NNETAR, TSLM, and Prophet are fit to each series. Ensemble combinations are built from the primary models and ranked by out-of-sample accuracy.
- **Model selection** — Choose between `"synchronize"` (same model type across all facilities for comparability) or `"optimize"` (best model per facility).
- **Intervention evaluation** — The pre-intervention window is used as training data. Forecast accuracy over the post-intervention window is measured as Weighted Percent Error (WPE), summarised across replicates and displayed as a histogram.
- **Annual change table** — A colour-coded summary of yearly totals and year-on-year percent change. When the most recent year is partial, percent change is computed against the same calendar months of the prior year.

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

## AI Assistant

The **Assistant** tab provides a conversational AI interface powered by [Claude](https://www.anthropic.com/) (Anthropic) or [GPT-4o](https://openai.com/) (OpenAI). It has read access to your current session context — formula elements, date range, facility count, and validation rules — so it can answer questions specific to your data, suggest related indicators, explain caveats, or help interpret findings.

Individual facility records are never sent to the AI. Only element names and aggregate summaries are transmitted.

### API key setup

The assistant requires an API key from your chosen provider.

**Option 1 — Recommended: store in `~/.Renviron`**

Add one or both of the following lines to `~/.Renviron` (run `usethis::edit_r_environ()` to open it):

```
ANTHROPIC_API_KEY=sk-ant-...
OPENAI_API_KEY=sk-...
```

Restart R. The key is then available in every session automatically — no code needed.

**Option 2 — Set for the current session only**

```r
Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-...")
run_mg2()
```

**Option 3 — Paste at runtime**

If no environment variable is found, a password input appears in the Assistant tab sidebar. Paste your key there — it is used only for the current session and is never saved to disk.

### Getting an API key

| Provider | Where to get a key |
|---|---|
| Anthropic (Claude) | [console.anthropic.com](https://console.anthropic.com) → API Keys |
| OpenAI (GPT) | [platform.openai.com](https://platform.openai.com) → API Keys |

Both providers charge per use (token-based pricing). Conversational assistant usage at this scale is inexpensive — typically a few cents per session.

### Required packages

```r
install.packages(c("ellmer", "shinychat"))
```

---

## Demo data

MG2 ships with a ready-to-use demo dataset so you can explore the full app without a DHIS2 server connection.

```r
library(MG2)
mg2_demo_setup()   # writes demo files to ~/mg2_demo  (~1 second)
run_mg2()          # open the app → paste ~/mg2_demo → skip Login
```

In the app, click **Load Demo Data** on the Setup tab (right column) instead of logging in.

### What the demo contains

| Object | Description |
|---|---|
| `mg2_demo_raw` | The 12 real months (Jan–Dec 2025) as downloaded from the DHIS2 instance — unmodified. |
| `mg2_demo` | Full 60-month dataset: real 2025 data plus 48 bootstrapped prior months with trends, gaps, and outliers. |
| `mg2_demo_formula` | Formula elements table — the 5 data elements below with UIDs and category option combos. |
| `mg2_demo_meta` | Full metadata list: org units, hierarchy, data element dictionary, validation rules, etc. |
| `mg2_demo_processed` | Pre-processed tsibble from `data_1()` — org unit hierarchy joined, time index built, ready for DQA onward. |

**Country:** Sierra Leone · **Org units:** 1,095 facilities across 19 districts (4-level hierarchy)

**Data elements:**

| UID | Name |
|---|---|
| `p4K11MFEWtw` | Inpatient malaria cases |
| `wWy5TE9cQ0V` | Inpatient malaria deaths |
| `wZwzzRnr9N4` | RDT positive |
| `Qk9nnX0i7lZ` | RDT negative |
| `AFM5H0wNq3t` | Malaria treated at PHU with ACT < 24 hrs |

**Time range:** 72 months — 12 real months (Jan–Dec 2025) downloaded from the public [DHIS2 Sierra Leone demo instance](https://play.im.dhis2.org/stable-2-41-8/) plus 60 months of prior history generated by a seasonal bootstrap with five features designed to exercise the full MG2 pipeline:

- **Higher facility-level noise** (lognormal SD ≈ 35%) — realistic spread across facilities.
- **Element-specific year-on-year trends** — ACT treatment (`AFM5H0wNq3t`) shows a step increase from 2023, simulating a scale-up programme the Evaluation tab can detect; inpatient cases and deaths decline; RDT testing grows gradually.
- **Reporting completeness gaps** — earlier years have fewer facility-months present (~65% in 2020 rising to ~93% by 2024), so the Reporting tab shows a real completeness trend rather than flat 100%.
- **Outlier cluster** — RDT-positive values (`wZwzzRnr9N4`) have a spike in Jun–Aug 2022 (~5% of facilities report 3–4× their expected value), giving the Outliers tab detectable signals.
- **2025 malaria reduction** — inpatient cases, deaths, RDT positive, and ACT treated are reduced ~15% in 2025 (more so in peak-season months); RDT negative increases ~15%, simulating an intervention effect detectable by the Evaluation tab.

The raw 12 real months are stored separately as `mg2_demo_raw`.

### Regenerating the demo data

The build script is at `data-raw/generate_demo.R`. It requires a live internet connection to the DHIS2 Sierra Leone demo instance and takes a few minutes to run:

```r
source("data-raw/generate_demo.R")
```

This re-downloads all metadata and data, rebuilds the seasonal bootstrap, re-runs `data_1()`, and overwrites the four `data/*.rda` files. Run it when the demo instance is refreshed or you want to update the time window.

---

## Contributing

Bug reports and feature requests are welcome via [GitHub Issues](https://github.com/jpainter/MG2/issues). Pull requests are also welcome — please open an issue first to discuss proposed changes.

---

## License

GPL-3 © John Painter
