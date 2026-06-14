# CLAUDE.md — Magic Glasses 2 (MG2) Package

## Project Overview

**MagicGlasses2 (MG2)** is an R package wrapping an interactive Shiny application for
epidemiological analysis of routine health data from DHIS2 systems. Functions are also
usable in standalone R scripts.

**Author:** John Painter (painter.ja@gmail.com) | **License:** GPL-3 | **Version:** 0.1.7
**Reference:** [R Packages (2e)](https://r-pkgs.org)

---

## Purpose

Given a DHIS2 connection or local data, MG2 guides analysts through:

1. **Setup** — Configure data directory and authenticate with DHIS2
2. **Metadata** — Explore data elements, categories, org units, indicators, validation rules, and dataset forms
3. **Data** — Build a formula (data dictionary) and download via DHIS2 API
4. **DQA** — Reporting completeness, outliers, MASE stability, and consistency (validation rules)
5. **Reporting** — Adjust for reporting bias; champion-facility method
6. **Outliers** — Sequential outlier detection and flagging
7. **Evaluation** — Time-series models, forecasts, and WPE assessment
8. **Combine** — Build derived datasets (ratios, subsets) from existing processed files
9. **Assistant** — AI chat (Claude / GPT-4o) with session-context system prompt

---

## Architecture

### Key files

```
R/
  utils.R                  # Date helpers, file I/O, date_code*, check_mg2_dependencies
  dhis2_auth.R             # loginDHIS2(), retry()
  dhis2_api.R              # dhis2_get(), api_url(), fetch_get(), ous_tree()
  metadata.R               # clean_invalid_characters(), fetch_validation_rules(),
                           #   .build_dhis2_form_html() (dataset form viewer)
  api_data_function_revision.R  # api_data() — main download
  prepare_dataset.R        # data_1(), translate_dataset_2(), data_leaves(), df_pre_ts(), df_ts()
  data_Functions.R         # cleanedData(), selectedData(), dataTotal(), tsmodels(),
                           #   mable_data(), yearly_summary_table(), wpe_summary(), …
  outliers.R               # mad_outliers(), seasonal_outliers(), extremely_mad(), unseasonal()
  outlier_summary.R        # outlier.summary.tibble(), monthly/yearly summaries
  dqa_functions.R          # dqa_reporting(), dqa_consistency(), dqa_mase(), …
  combine_functions.R      # build_combined_dataset(), execute_include/ratio_step(), …
  chat_functions.R         # build_mg2_system_prompt(), summarize_*_for_prompt()
  run_app.R                # run_mg2()
  zzz.R                    # @import data.table (required for := in package)

inst/shiny/
  app.R                    # bslib::page_navbar() entry point
  directory_widget.R       # Module 1: data directory (shinyFiles)
  login_widget.R           # Module 2: DHIS2 auth (live instance list)
  metadata_widget.R        # Module 3: metadata + validation rules + form viewer
  data_widget.r            # Module 4a: formula/dataset picker
  formula_widget.r         # Module 4b: formula builder (primary/secondary roles)
  data_request_widget.R    # Module 4c: data download
  dqa_widget.R             # Module 5: DQA (sidebarLayout, Consistency tab)
  reporting_widget.r       # Module 6: reporting bias
  cleaning_widget.r        # Module 7: outlier detection
  evaluation_widget_2.R    # Module 8: forecasting
  combine_widget.R         # Module 9: dataset combiner
  chat_widget.R            # Module 10: AI assistant
  regions_widget.R         # Auxiliary: regional filter + map
  map_widget.R             # Auxiliary: geographic viz
  about_widget.R           # About tab + environment check
  chart_module.R           # Reusable chart module
  DToptions.R              # DataTable styling helpers
```

### Reactive dependency chain

```
directory_widget → login_widget
               ↘ metadata_widget → data_widget ←→ data_request_widget
                                              ↘ formula_widget
                 regions_widget ←────────────────┘
data_widget → dqa_widget
           → reporting_widget → cleaning_widget → evaluation_widget
```

### Critical pattern: `cached_rous`

`reportingSelectedOUs()` has a `req(current_tab() == "Reporting")` guard. Calling it
directly inside any reactive causes silent failure on other tabs, propagating blanks
downstream. **Always read `cached_rous$value` instead.**

```r
cached_rous = reactiveValues(value = NULL)
observeEvent(reportingSelectedOUs(), ignoreNULL = FALSE, {
  cached_rous$value = reportingSelectedOUs()
})
```

Same pattern applied to `selected_data()` via `cached_selected_data`.

### Other design rules

- `R/` functions load via package namespace — no `source()` calls for functions.
- `inst/shiny/` modules use `::` to call package functions.
- `R/originals/` excluded from build via `.Rbuildignore`.
- `fable` model specials (`trend()`, `season()`) must NOT use `::` inside `model()` — they are resolved by fable's fitting environment.
- `stats::filter` vs `dplyr::filter` conflict resolved via `@import data.table` in `zzz.R` + explicit `dplyr::filter()` calls.
- All progress `modalDialog()` calls use `fade = FALSE` to prevent stuck Bootstrap animation queues.

---

## Outlier Detection Algorithms

Applied sequentially per facility × data element:

| Algorithm       | Description                                               |
|-----------------|-----------------------------------------------------------|
| key_entry_error | Values matching mobile-phone entry codes (4+ digit patterns) |
| over_max        | Values exceeding a theoretical maximum (e.g. > 31/month) |
| mad15           | Values > 15× MAD                                          |
| mad10           | Values > 10× MAD (after mad15 removed)                    |
| seasonal5       | Values > 5× seasonal decomposition residual / MAD         |
| seasonal3       | Values > 3× seasonal decomposition residual / MAD         |

Combined datasets add `missing_numerator` and `missing_denominator` flags.

---

## renv

**Not initialized.** End users install via `devtools::install_github()`; dependencies
come from `DESCRIPTION`. Revisit only if multiple developers need reproducibility locks.

---

## Development Workflow

```r
devtools::document()   # regenerate NAMESPACE + man/
devtools::load_all()   # load package
run_mg2()              # launch app
devtools::check()      # must pass before committing (baseline: 0E · 0W · 2N)
```

---

## Current State (as of 2026-06-08)

**Version:** 0.1.9 | **check() baseline:** 0 ERRORs · 0 WARNINGs · 1 NOTE

**All tabs functional end-to-end:**
Setup → Metadata (incl. validation rules + form viewer) → Regions → Data (formula +
download + combine) → DQA (incl. Consistency + Reporting Map) → Reporting → Outliers →
Evaluation → Burden Estimate (dev) → AI Assistant (dev)

**Map tab:** widget file exists (`map_widget.R`), not yet integrated.

**Remaining NOTE (Phase D — deferred):**
- Bare `dplyr`/`ggplot2` calls in `data_Functions.R` without `::` qualification

**Demo data:**
- `mg2_demo_*` — Sierra Leone malaria (5 elements, 72 months synthetic, yearmonth correct)
- `mg2_pdrlao_*` — PDR Lao malaria (82 elements, ~57 months, Aug 2021–May 2026)
- `mg2_demo_setup()` / `mg2_pdrlao_setup()` — write demo files to disk
- Login tab has two demo buttons (Sierra Leone / PDR Lao) using `actionButton` not `shinyFiles`

**Deployments:**
- GitHub releases: automated via `.github/workflows/release.yml` (push `git tag vX.Y.Z`)
- shinyapps.io: `rsconnect::deployApp(appDir="deploy", appName="MG2-MagicGlasses2")`
- Connect Cloud: republish via https://connect.posit.cloud/magicglasses (installs MG2 from `../` local clone)
- Both deployments use `MG2_DEMO_MODE=1` env var; demo buttons replace shinyFiles picker

**File format (QS default, RDS fallback):**
`mg2_data_ext()` returns `"qs"`. `save_file()` uses `qs2::qs_save()`; `read_file()` reads
`.qs`, `.rds`, and legacy `.fst`. QS replaced RDS (2026-06-07) because FST had persistent
yearmonth class-stripping bugs. QS uses ZSTD compression and preserves all R classes
including `yearmonth` without post-read fixup. Do NOT revert to FST — the vctrs dispatch
issue where `data.table::set()` converts yearmonth days back to months was never solved.

**yearmonth encoding (critical):**
- Internal storage: days-since-epoch (~18000-20000 for 2020-2025); `unclass(yearmonth("2024 Dec")) = 20058`
- Old tsibble (pre-stored data): months-since-epoch (~360-840); `median > 5000` in `read_file()` detects legacy days encoding
- `data_widget.r` restores yearmonth class after `setDT()` strips it via `structure(as.double(m_raw), class=c("yearmonth","vctrs_vctr"))`
- `dqa_functions.R` uses `.month_to_year()` helper that handles both encodings
- Evaluation widget: `selectInput` stores `unclass(yearmonth)` = days-since-epoch ("20058"); parse via `yearmonth(as.Date(as.integer(x), origin="1970-01-01"))`
- DO NOT use `yearmonth(as.integer(x))` on stored unclass values — treats days as months → year ~3656 AD

**UX improvements (2026-06-08):**
- Welcome page: getting-started prompt, full tab descriptions, dev tab callouts
- Setup tab: "Browse Metadata" next-step button appears once directory is set
- Metadata/Regions tabs: blue step-hint bars pointing to next tab
- DQA/Reporting: empty-state banners when no data loaded
- Demo modal: two-stage (setup → ready), OK navigates to Metadata tab

---

## Nigeria DHIS2 Indicator Finding (cross-project)

Nigeria's national DHIS2 instance contains two indicators nominally measuring RDT test
positivity rate (TPR). They use different formulas and produce divergent results (Kebbi State:
Indicator A ~76%, Indicator B ~26% for the same post-transition period — ~50pp gap).

| Label | UID | Formula | Issues |
|-------|-----|---------|--------|
| **Indicator A** — `% of Fever cases tested positive with RDT` | `l2eIpdluuTI` | Positives (default) / Tested (default) | Denominator may include untested fever cases → TPR deflated |
| **Indicator B** — `Test Positivity Rate(TPR) (RDT)` | `uiA07P72s2Z` | Positives (4 groups) / Positives (total) + Tested (3 new-scheme groups) | Positives added to denominator; COC scheme mix; missing new-scheme `<5yrs` |

Key data elements: `GEd2F6skCpT` (RDT positives), `r6WOvUlcQm6` (RDT tested).

**Indicator B bias directions:**
- **Post-transition (visible):** denominator inflated ~2× by adding positives → TPR deflated ~50 pp
- **Pre-transition (suppressed):** new-scheme tested = 0 → denominator collapses to positives → TPR ≈ 100%, suppressed by DHIS2 → no rows returned for pre-transition periods

**Indicator A denominator concern:** "Persons presenting with fever & tested by RDT" may in
practice include untested fever attendees if health workers record total fever cases — also
deflating TPR.

**NHMIS form transition (Jan 2021):** Kebbi data shows a sharp, coordinated cutover from
`Age(Malaria)` (old, 2-group) to `NH19_Malaria Testing` (new, 3-group incl. Preg Women).
Indicator B's formula mixes both COC schemes; its denominator covers only new-scheme COCs.

**Source files:**
- Analysis/report: `reports/Nigeria/kebbi_tpr_rdt.Rmd`
- Cross-project write-up: `../DHIS2 Best Practices/Drafts/nigeria_tpr_indicator_error.qmd`
