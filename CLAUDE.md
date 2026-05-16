# CLAUDE.md — Magic Glasses 2 (MG2) Package

## Project Overview

**MagicGlasses2 (MG2)** is an R package wrapping an interactive Shiny application for
epidemiological analysis of routine health data from DHIS2 systems. Functions are also
usable in standalone R scripts.

**Author:** John Painter (painter.ja@gmail.com) | **License:** GPL-3 | **Version:** 0.1.4
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

## Current State (as of 2026-05-16)

**Version:** 0.1.4 | **check() baseline:** 0 ERRORs · 0 WARNINGs · 2 NOTEs

**All tabs functional end-to-end:**
Setup → Metadata (incl. validation rules + form viewer) → Regions → Data (formula +
download + combine) → DQA (incl. Consistency) → Reporting → Outliers → Evaluation →
AI Assistant

**Map tab:** widget file exists (`map_widget.R`), not yet integrated.

**Remaining NOTEs (Phase D — deferred):**
- Shiny packages in `Imports` without `@importFrom`
- Bare `dplyr`/`ggplot2` calls in `data_Functions.R` without `::` qualification

**Next planned phases:**
- Phase B: Tests (~95 tests, no live DHIS2 server needed)
- Phase C: Demo data (`data/mg2_demo.rda`) + vignettes
- Phase D: `cat()` → `message()` sweep; `@importFrom` polish; CRAN submission prep
- Phase 7: Map module integration

**FST note:** Processed datasets (`.fst`) are supported — `read_file()` restores
`yearmonth`/`yearweek` class and rebuilds tsibble on load. Metadata files stay `.rds`
(mixed object types: sf, nested lists — FST cannot handle them; also LZ4 compresses
string-heavy DHIS2 data worse than gzip).
