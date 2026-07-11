# Magic Glasses 2 (MG2) — Epidemiological Analysis of DHIS2 Routine Health Data

*Announcing a new app for epidemiologists and health data analysts*

![MG2 Logo](https://raw.githubusercontent.com/jpainter/MG2/main/logo.png)  
---

## What is MG2?

Every dataset has noise — inconsistent reporting, outliers, gaps. The epidemiological
signal is there. Getting to it is the hard part.

**Magic Glasses 2 (MG2)** is a free, open-source R/Shiny application that guides
epidemiologists and health data analysts through a complete routine data analysis
workflow — directly from a DHIS2 connection, without ever leaving the app.

MG2 is a standalone R/Shiny application that connects to DHIS2 via the API — not a native DHIS2 app, but built specifically around the DHIS2 data model and workflow.

It is not a dashboard. It is step-by-step workflow: from raw DHIS2
data to a cleaned, adjusted, modelled impact estimate. The sequence matters, and MG2
makes it explicit.

---

## Who is it for?

MG2 is designed for **epidemiologists and surveillance analysts** working with routine
health data in DHIS2 — particularly in national malaria programs, HMIS units, and
similar public health contexts. It assumes some familiarity with data analysis but
does not require programming skills to use.

---

## What does it do?

MG2 walks users through eight analytical steps:

**1. Setup & Authentication**
Connect to any live DHIS2 instance via the API, or load local data files. A demo
mode is available with synthetic Sierra Leone malaria data and real PDR Lao malaria
data — no login required.

**2. Metadata Explorer**
Browse data elements, indicators, category option combinations, organisation units,
and validation rules. Inspect the actual DHIS2 dataset forms as they appear to health
workers. Useful on its own for understanding what data your instance is collecting.

**3. Data**
Select data elements of interest and download via the DHIS2 API with one click.

**4. Data Quality Assessment (DQA)**
- Percent of facilities submitting the selected data elements each and every month
- Percent of values with no indication of being an outlier 
- Perecent of values passing all DHIS2 validation rules
- Maps showing which districts are missing data

**5. Reporting Adjustment**
Adjust for incomplete reporting using the champion-facility method, by identifying facilities that consistently report data values and removing bias of facilities that do not.

**6. Outlier Censoring**
A series of six sequential algorithms to identify potential outliers that may skew analysis results.  

**7. Evaluation & Forecasting**
Time-series models (ARIMA, ETS, NNETAR, Prophet, and ensemble combinations) to assess the likelihood there was a real change. This is the same workflow that was used to assess the impact of the new malaria vaccine in Kenya using routine data. 

---

## Try it out

**Live demo (no install required):**
[https://connect.posit.cloud/magicglasses](https://connect.posit.cloud/magicglasses)

No DHIS2 account needed. On the Login tab, click **"Load Sierra Leone Demo"** or **"Load PDR Lao Demo"** to get started immediately with real-looking data.

To explore real data and have full functionality, you need to install MG2 locally (below).

**Install locally (R required):**

```r
# Install from GitHub
devtools::install_github("https://github.com/jpainter/MG2")

# Launch the app
MG2::run_mg2()
```

**Source code:** [github.com/jpainter/MG2](https://github.com/jpainter/MG2)

---

## Background

MG2 grew out of field work supporting national malaria programs in sub-Saharan Africa.
The challenge was always the same: facilities report inconsistently, outliers contaminate
trends, and turning DHIS2 data into defensible analytical outputs required a patchwork
of disconnected tools and a lot of re-work every time something changed upstream.

The goal was to make that full pipeline reproducible, auditable, and accessible — so
that this kind of epidemiological work doesn't require a specialist every time.

*Routine data, extraordinary insight.  See what the data is really saying*

---

We'd love to hear from you — questions, feedback, or ideas for how MG2 could be
more useful in your context. Happy to discuss in this thread or connect directly.

*— John Painter*
