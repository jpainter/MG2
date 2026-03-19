# MG2 CRAN Preparation Plan

Created: 2026-03-19
Status: Not started

---

## Phase A — Quick Blocking Fixes (~1 hour)
*Do these first to establish a clean `devtools::check()` baseline.*

| # | Task | File | Detail |
|---|---|---|---|
| A1 | Fix `api_data` docs | `R/api_data_function_revision.R` | Replace `#' Title` placeholder; wrap `@examples api_data()` in `\dontrun{}`; change `dir = country.dir` default to `dir = NULL` |
| A2 | Fix DESCRIPTION | `DESCRIPTION` | Remove `data.table`, `igraph`, `lubridate` from `Suggests` (all three are in `Imports` too); add `URL:` and `BugReports:` GitHub links |
| A3 | Delete bundled `.rds` files | `inst/shiny/*.rds` | 11 leftover session data files; also delete `~$Instances.xlsx` and the old `MagicGlasses2_ShinyApp.R` |
| A4 | `cat()` → `message()` in core functions | `R/prepare_dataset.R`, `R/dhis2_api.R` | Add `.verbose = FALSE` parameter to `df_pre_ts`, `df_ts`, `data_1`, `ous_tree`; wrap `cat()` calls — CRAN expects exported functions to be silent by default |
| A5 | Run `devtools::check()` | — | Establish clean baseline before writing tests |

---

## Phase B — Tests (~4–5 hours)
*Target: ~95 tests across 6 files. All testable without a live DHIS2 server.*

**Fixture strategy:** Build minimal synthetic tsibbles inline — no `.rds` fixtures needed.

| File | Functions covered | ~Tests |
|---|---|---|
| `tests/testthat/test-utils.R` | `as.yearmonth` (all 5 input formats), `Month_Year`, `Week_Year`, `date_code`, `date_code_weekly`, `list_dir_files` | 25 |
| `tests/testthat/test-outliers.R` | `extremely_mad` (spike detection, all-NA, zero-MAD fallback), `unseasonal`, `mad_outliers`, `seasonal_outliers` | 20 |
| `tests/testthat/test-prepare_dataset.R` | `df_pre_ts` (Month/Week columns, NA removal), `df_ts` (tsibble output, deduplication, gap-filling) | 15 |
| `tests/testthat/test-dqa_functions.R` | `dqaPercentReporting` (pr between 0–1, all-reporting = 1.0), `dqa_mase` (missing `expected` col returns NULL) | 12 |
| `tests/testthat/test-metadata.R` | Extend existing 8 tests with factor-column and wide-column edge cases | 13 |
| `tests/testthat/test-dhis2_api.R` | `ous_tree` (synthetic tree, no network needed), `dhis2_get` (skip_if_offline), `api_url` | 10 |

### Key test cases by function

**`as.yearmonth`:** "January2020", "2025 Jan" (tsibble format), "201901" (DHIS2), "2019-01", already-yearmonth input, explicit `fmt` arg, vector input

**`date_code`:** semicolon-separated string output, `YrsPrevious=1` starts at Jan last year, `startPeriod="202401"` starts at 2024-01, `currentMonth=FALSE` excludes current month

**`extremely_mad`:** single spike flagged, all-NA returns NA, zero-MAD falls back to trimmed SD, `logical=FALSE` returns cleaned values

**`df_pre_ts`:** adds `Month` column of class yearmonth, removes NA COUNT rows, `data` column = `dataElement_Category`, NA Category produces no trailing underscore

**`df_ts`:** returns tsibble, correct key columns, gap-filling, deduplication

**`dqaPercentReporting`:** pr between 0–1, all-reporting year = 1.0, label is a percent string

**What to skip for now:** `data_1` (complex wrapper), `cleanedData`/`mostFrequentReportingOUs` (need namespace cleanup in Phase D first)

---

## Phase C — Vignettes (~3–4 hours)
*Two-track: one fully executable, two documentation-style.*

### Create demo data first
Create `data/mg2_demo.rda` — a small synthetic tsibble (3 facilities × 2 data elements × 36 months) mirroring `data_1()` output. Document with roxygen in `R/data-mg2_demo.R`. This powers the standalone vignette and test fixtures.

### Vignettes

| File | Title | Executable? | Contents |
|---|---|---|---|
| `vignettes/standalone-analysis.Rmd` | "Standalone Analysis of DHIS2 Data" | ✅ Fully runs on CRAN | Load demo data → outlier detection → DQA reporting completeness plot → outlier summary table → interpretation |
| `vignettes/dhis2-workflow.Rmd` | "Connecting to DHIS2 and Downloading Data" | API calls use `eval=FALSE` | Authentication → metadata → `date_code()` → download → `data_1()` → hands off to demo data |
| `vignettes/shiny-app.Rmd` | "Running the MG2 Shiny Application" | No code execution | Tab-by-tab walkthrough; `run_mg2()` in `eval=FALSE` |

### DESCRIPTION additions for vignettes
Add to `Suggests`: `knitr`, `rmarkdown`, `withr`

---

## Phase D — Polish (~2 hours)
*Not blocking, but required for a credible submission.*

1. **`cat()` sweep in `R/data_Functions.R`** — `translate_dataset`, `getLevelNames` have unconditional `cat()` calls; add `.verbose` guards or convert to `message()`
2. **Namespace hygiene in `data_Functions.R`** — bare `count()`, `arrange()`, `pull()`, `%>%` without `::` or `@importFrom` generate R CMD check NOTEs about "no visible binding for global variable". Use `dplyr::count()` etc. throughout, or add `@importFrom` tags.
3. **Roxygen on undocumented internal functions** — `getLevelNames`, `error_factor`, `cleanedData` have no `@description`; add `@noRd` or proper docs
4. **`NEWS.md`** — create with a standard 0.1.0 entry
5. **Final `devtools::check(cran = TRUE)`** — target 0 ERRORs, 0 WARNINGs, ≤ 2 NOTEs

---

## Recommended Session Order

```
A3 → A2 → A1 → A4 → A5 (check baseline)
          ↓
    Create mg2_demo data
          ↓
    B1 → B2 → B3 → B4 (tests)
          ↓
    C1 → C2 → C3 (vignettes)
          ↓
    D1 → D2 → D3 → D4 → D5 (polish + final check)
```

---

## How to Run the Check

```r
devtools::load_all()
devtools::document()
devtools::check(cran = TRUE, remote = TRUE, manual = TRUE)
```
