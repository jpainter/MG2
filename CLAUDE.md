# CLAUDE.md — Magic Glasses 2 (MG2) Package

## Project Overview

**MagicGlasses2 (MG2)** is an R package wrapping an interactive Shiny application for
epidemiological analysis of routine health data from DHIS2 systems. Functions are also
usable in standalone R scripts.

**Author:** John Painter (painter.ja@gmail.com) | **License:** GPL-3 | **Version:** 0.2.0
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
9. **Climate** — Monthly CHIRPS rainfall extraction and mapping using DHIS2 org unit boundaries
10. **Output** — Write MG2 results to a local DHIS2 instance (spin up via d2 cluster, import metadata, write values)
11. **Assistant** — AI chat (Claude / GPT-4o) with session-context system prompt

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
  climate_functions.R      # chirps_download_tif(), chirps_extract_rainfall(),
                           #   chirps_process_months(), chirps_make_map(),
                           #   chirps_geo_levels(), chirps_geo_from_metadata(),
                           #   chirps_combine_data(), chirps_write_excel(),
                           #   chirps_save_flat(), chirps_load_rainfall(),
                           #   chirps_make_chart(), chirps_annual_means_sf(),
                           #   chirps_multi_year_map(), chirps_multi_year_chart(),
                           #   chirps_anomaly_map()
  fct_output_dhis2.R       # mg2_build_org_units_payload(), mg2_build_source_de_payload(),
                           #   mg2_build_output_de_payload(), mg2_post_metadata(),
                           #   mg2_format_raw_data_values(), mg2_post_data_values(),
                           #   mg2_check_instance_metadata(), mg2_test_dhis2_connection()
  fct_cluster_manager.R    # mg2_check_docker() (manage_clusters() gadget moved to dev/unused_R/)
  chat_functions.R         # build_mg2_system_prompt(), summarize_*_for_prompt()
  run_app.R                # run_mg2()
  zzz.R                    # @import data.table (required for := in package)

inst/shiny/
  app.R                    # 1-line launcher: sources app_main.R in MG2:::.mg2_app_env()
  app_main.R               # bslib::page_navbar() UI + server; sources widgets (local = TRUE)
  directory_widget.R       # Module 1: data directory (shinyFiles)
  login_widget.R           # Module 2: DHIS2 auth (live instance list)
  metadata_widget.R        # Module 3: metadata + validation rules + form viewer
  data_widget.r            # Module 4a: formula/dataset picker
  formula_widget.r         # Module 4b: formula builder (primary/secondary roles)
  data_request_widget.R    # Module 4c: data download
  dqa_widget.R             # Module 5: DQA (sidebarLayout, Consistency tab)
  reporting_widget.r       # Module 6: reporting bias
  cleaning_widget.r        # Module 7: outlier detection
  evaluation_widget.R      # Module 8: forecasting
  combine_widget.R         # Module 9: dataset combiner
  climate_widget.R         # Module 10: CHIRPS rainfall (dev) — uses metadata geoFeatures
  output_widget.R          # Module 10: write results to local DHIS2 instance (Steps 1–3 done, Step 4 WIP)
  chat_widget.R            # Module 11: AI assistant
  regions_widget.R         # Auxiliary: regional filter + map
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

### Critical pattern: `geoFeatures$level` is integer-normalised on read

`geojsonsf::geojson_sf()` returns every GeoJSON property as a **string**, and metadata
saved to disk preserves that, so `geoFeatures$level` arrives as character (`"2"`, `"3"`).
The `geoFeatures()` reactive in `metadata_widget.R` now coerces it to integer before
returning, so **consumers can rely on `level` being integer**. Do not undo that, and do
not reintroduce string levels from a new source.

Why it matters: character levels survive comparison unnoticed but break indexing.

```r
lv <- c("2", "3", "4")          # what geojson_sf gives you
min(lv[lv > 1L])                # "2"  — string compare, "works"
levelNames[["2"]]               # NA   — NAME lookup on an unnamed vector!
req(NA %in% names(data))        # req(FALSE) → silent abort, blank output
```

`level == 2`, `level > 1L` and `sort()`/`max()` all appear to work because R coerces to
character and lexicographic order matches numeric order **for single digits**. What fails
is using the value as a *numeric index*: `levelNames[map_level]` with a character
`map_level` is a name lookup, silently yielding `NA`. This blanked the DQA reporting map
(and its year slider, which shares the reactive) for months — see `98d9cce`.

Two lessons, both of which cost real debugging time here:

- **A bare `req()` on a derived condition is a silent blank.** Prefer returning `NULL`
  plus a diagnostic the renderer can display (`dqa_map_diag` in `dqa_widget.R`) over
  `req(x %in% names(y))`, which aborts with no console error and no UI message.
- **Tab-visibility guards must fail open.** `req(identical(input$dqa_tab, "Reporting"))`
  blanks the panel outright if the input value is ever not what you assumed. Gate on the
  panels you *positively know* are hidden instead, so a wrong guess costs CPU rather than
  the whole output — see `.dqa_map_hidden()` in `dqa_widget.R`.

### Other design rules

- **Never pick a `future` backend from `.Platform$OS.type`, and don't trust
  `parallelly::supportsMulticore()` either.** macOS reports `"unix"` but cannot fork
  inside RStudio or Positron, and `plan(multicore)` there dies with *"Can't fork the
  R session in Positron"*. `supportsMulticore()` recognises RStudio but (as of
  parallelly 1.48.0) knows nothing about Positron, so it returns `TRUE` there.
  `.mg2_supports_fork()` in `R/outliers.R` therefore *probes* `parallel::mcparallel()`
  once per session and caches the answer — deliberately the raw primitive, not a test
  future, because a failed multicore future stays in future's registry and the next
  `future::plan()` trips over it. The
  same distinction governs worker counts: forked workers share the parent heap, so
  separate processes need the free-memory cap that forks do not.
  Any `furrr_options()` on a non-fork backend must also declare `packages =` —
  fresh processes do not inherit the parent's attached packages.
- **`'inherits(job, "parallelJob")' is not TRUE` never means what it says.** It comes
  from `future:::resolved.MulticoreFuture()` finding no job on a future that never
  launched, and it is usually raised by `plan_cleanup()` from inside an `on.exit`
  plan restore — where it *replaces* the real error (typically "Can't fork"). Restore
  plans via `.mg2_restore_plan()`, which retries and never throws, so the
  original error survives. If it appears at app startup, the session is polluted from
  an earlier crash: restart R.

- `R/` functions load via package namespace — no `source()` calls for functions.
- **Optional packages (Suggests) must be checked before use.** `terra`, `googledrive`,
  `ggiraph`, `rstudioapi` and others are Suggests: call `requireNamespace()` first and
  give an install hint or a fallback (see `.chirps_sf_layer()` / `.chirps_girafe()`,
  `.ws_gdrive_authorize()`). Don't add a package to Imports without a reason it can't be
  optional; the count is at 31.
- **The app sees every MG2 function, exported or not.** `inst/shiny/app.R` evaluates
  `app_main.R` in `MG2:::.mg2_app_env()`: a child of an environment holding all namespace
  functions, whose parent is `globalenv()` (so the search path still resolves `dplyr`,
  `lubridate`, …). Widgets call MG2 helpers by bare name; app-only helpers stay unexported
  (`@noRd`). Widget files must be sourced with `local = TRUE` — plain `source()` writes
  them into `.GlobalEnv`. Don't make the namespace itself the parent: its imports (all of
  `data.table`) would shadow `lubridate::month()`, `dplyr::first()` etc. in widget code.
- **Global variables:** a name flagged "no visible binding" goes in `R/globals.R` only
  after checking it is a column / NSE name. The 2026-11-16 review found five real
  undefined-variable bugs hiding in that NOTE. Bare function calls go in
  `R/imports.R`. Lazy-loaded datasets must be fetched with `.mg2_dataset("name")`
  from package code — a bare `mg2_demo` only works when MG2 is attached.
- **Dead code:** `codetools::findGlobals()` does not look inside formulas
  (`purrr::map(x, ~ f(.x))`), so a call-graph "unreachable" list needs a text-search
  confirmation before anything is moved (`swape_year()` was a false positive).
- **Exports are the standalone-script API only** (67 as of 2026-11-16). Export a new
  function only if a script user would call it; otherwise `@noRd`.
- `R/originals/` excluded from build via `.Rbuildignore`.
- `fable` model specials (`trend()`, `season()`) must NOT use `::` inside `model()` — they are resolved by fable's fitting environment.
- `stats::filter` vs `dplyr::filter` conflict resolved via `@import data.table` in `zzz.R` + explicit `dplyr::filter()` calls.
- All progress `modalDialog()` calls use `fade = FALSE` to prevent stuck Bootstrap animation queues.
- New `R/` files follow golemverse naming: `fct_` prefix for feature/business-logic functions, `utils_` prefix for generic helpers (existing files are not renamed).

### Golemverse assessment (2026-07-11)

MG2 already implements ~80% of what golem provides (R package structure, `run_mg2()`, modular widgets, DESCRIPTION dependency management). **Migration not recommended** — moving modules from `inst/shiny/` to `R/` would expose widget code to `R CMD CHECK`, add golem as a runtime dependency, and rewire `system.file()` paths for zero user-facing gain. Golem is designed for commercial multi-developer shops deploying to Docker/enterprise servers.

**Adopted without migration:**
- `fct_` / `utils_` file-naming convention for new `R/` files
- `dev/deploy.md` — step-by-step deployment checklist (see `dev/` directory)

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
devtools::check()      # must pass before committing (baseline: 0E · 0W · 1N)
```

**When bumping the version** (do all four together):
1. Update `Version:` in `DESCRIPTION`
2. Update `Date:` in `DESCRIPTION` to today (`date +%Y-%m-%d`)
3. Add `# MG2 X.Y.Z` entry to `NEWS.md`
4. Update version in `CLAUDE.md` header + Current State section
5. `git tag vX.Y.Z && git push origin main --tags`

---

## Current State (as of 2026-09-07)

**Version:** 0.2.0 | **check() baseline:** 0 ERRORs · 0 WARNINGs · 2 NOTEs

Verified 2026-09-07. The previously recorded 0E · 0W · 1N was stale — the real
state at the start of that session was 1 ERROR · 2 WARNINGs · 2 NOTEs. Both
remaining NOTEs are expected:

- **NOTE — unable to verify current time.** Environmental, not a code issue.
- **NOTE — no visible binding for global variable.** Cleared on the `cran` branch
  (2026-11-16): `R/imports.R` (`@importFrom` for bare calls) + `R/globals.R`
  (reviewed column names).

**Non-ASCII characters:** R CMD check inspects *parsed* code, so the
"except perhaps in comments" line in that warning is literal — comments are
genuinely exempt. `data_Functions.R` has carried ~30 non-ASCII comment lines
for ages and was never flagged. If the warning appears, look only for non-ASCII
inside string literals and identifiers; use `\uXXXX` escapes there (see
`fct_output_dhis2.R`'s `stop()` messages). Do not mass-rewrite em-dashes or
`──────` box rules in comments — it changes nothing in the check.

**Note on running `check()`:** the repo lives in iCloud Drive, so `R CMD build`
spends many minutes hydrating evicted files before it does any work (0.2% CPU, no
progress output). Export the tree to local disk first:
`mkdir -p /tmp/mg2check/MG2 && git archive HEAD | tar -x -C /tmp/mg2check/MG2`,
then run `devtools::check()` there — seconds instead of minutes.

**All tabs functional end-to-end:**
Setup → Metadata (incl. validation rules + form viewer) → Regions → Data (formula +
download + combine) → DQA (incl. Consistency + Reporting Map + Facilities + Heatmap) →
Reporting → Outliers → Evaluation → Burden Estimate (dev) → Climate →
Output (dev, Steps 1–3) → AI Assistant (dev)

Navbar order as of 0.2.0: the four dev-ish tabs follow Evaluation as
Burden Estimate → Climate → Output → Assistant. Climate no longer carries the
`dev` badge and is listed among the working tabs on the Welcome page.

**Output tab (dev):** wired into the navbar as of 0.2.0 (`84b726d` — `output_widget.R`
was added in `98e822b` but never sourced or served, so the tab was unreachable).
Steps 1–3 functional — source info display, spin up local DHIS2 via
`d2 cluster up` (matching source version, processx, polls for readiness), import org units +
dynamically-inferred source data elements + 4 fixed MG2 output UIDs (`MG2trendEst`,
`MG2adjEstim`, `MG2forecast`, `MG2wpeEffct`). Step 4 (model output writing) is a placeholder.
Data elements are inferred from the loaded formula — nothing is hardcoded.

**Map tab:** not integrated. `map_widget.R` (and the unused `facilities_widget.r`, `levels_widget.R`, `trend_widget.r`) moved to `dev/unused_widgets/` on 2026-11-16 — nothing sourced them, and `inst/shiny/` ships in the package.

**Version history:** `NEWS.md` — documents user-facing changes for every release.

**Active branches (as of 2026-07-19):**

| Branch | Status | Purpose |
|---|---|---|
| `main` | stable | All 0.1.9 features; evaluation autoplot deprecation fix |
| `feature/url-load` | ready to test | Load data from shared Google Drive / Dropbox / OneDrive URLs (Login tab); read-only, single-session stopgap |
| `feature/cloud-workspace` | Phase 2 complete (Google Drive) | Cloud storage workspace abstraction (Option B); see below |

**`feature/url-load` — "Load from shared links":**
- New section on Login tab: paste share URLs for data (.qs/.rds), formula (.xlsx or Google Sheets), metadata (.rds)
- `R/fct_url_load.R`: `mg2_load_from_urls()` + `.mg2_normalize_url()` handle Google Drive, Google Sheets, Dropbox, OneDrive URLs
- Normalizes Google Sheets `/edit` links → `/export?format=xlsx`; follows Google Drive virus-scan HTML confirmation pages automatically
- Downloaded files go to a session temp dir; temp dirs are never saved to `~/.mg2_config.rds` directory history
- Read-only, single-session — files are not written back to the cloud source

**`feature/cloud-workspace` — Option B workspace abstraction (Phase 2 complete: Google Drive):**

The long-term cloud storage solution. Replaces raw path strings with an S3 workspace object that dispatches file I/O to the correct backend (local, Google Drive, OneDrive).

*Architecture:*
- `R/fct_workspace.R` — S3 generics; local backend (Phase 1) + Google Drive backend (Phase 2); OneDrive still stubbed (Phase 3)
- `R/fct_gdrive_oauth.R` — Google OAuth as a **web-application redirect flow**, not `googledrive::drive_auth()`. That function's default flow opens a local browser + prompts in the R console — both assume the R process and the user share one machine, which breaks on hosted deploys (shinyapps.io/Connect Cloud) and is confusing even locally. Instead: a plain link sends the user's browser to Google; Google redirects back to the app's own URL with `?code=&state=`; the server exchanges the code directly via `httr::POST()` (no `gargle` orchestration) and stores the resulting `httr::Token2.0` in a per-session `reactiveVal` — never written to disk.
- `directory_widget.R` — "Connect Google Drive" UI + OAuth redirect handling; returns `workspace` reactive (the single local/cloud switch — local path or `gdrive:FOLDER_ID` + token) alongside `directory` alias (raw local path only, backward compat, still used by a few display-only/local-cache call sites)
- All file-I/O widgets migrated to consume `workspace()` (not just the `directory` alias) — see "Known gotcha" below

*Workspace API:*
```r
ws_list(ws, pattern)          ws_list_files(ws, search, type, other)
ws_read_file(ws, filename)    ws_save_file(ws, data, filename)
ws_read_rds(ws, filename)     ws_save_rds(ws, obj, filename)
ws_read_xlsx(ws, filename)    ws_save_xlsx(ws, wb, filename)
ws_file_exists(ws, filename)  ws_file_info(ws, filename)
ws_file_rename(ws, from, to)  ws_path(ws, filename=NULL)          # local only, errors on gdrive
ws_local_copy(ws, filename)   ws_save_local_copy(ws, local_path, filename)  # escape hatches, any backend
ws_is_local(ws)
```
`ws_path()` only ever worked for local workspaces (deliberately errors for gdrive). `ws_local_copy()`/`ws_save_local_copy()` are the general escape hatches for code that insists on a real on-disk path either direction (e.g. `readxl::read_excel()`, `api_data()`'s `previous_dataset_file`, `write.csv()`) — local backends pass through with no copy; gdrive downloads/uploads via a session temp file.

*Known gotcha (root-caused 2026-07-20, fixed):* several widgets built their **own** local-only `ws <- reactive({ mg2_workspace(dir()) })` instead of using `directory_widget_output$workspace()` directly. This was invisible in Phase 1 (both approaches were equivalent when only the local backend existed) but silently discarded the Drive connection in Phase 2 — e.g. DHIS2 metadata downloads kept landing in the local directory even with Drive connected. Fixed in `metadata_widget.R`, `combine_widget.R`, `data_request_widget.R`. Also fixed: ~13 places in `metadata_widget.R` that checked file existence via a raw local path instead of `ws_file_exists()`; a facilities-CSV export and formula-registration write (`register_combined_formula()`, now takes `ws =`) that bypassed the workspace; `chirps_save_flat()`/`chirps_load_rainfall()` (now take `ws =`) for the Climate tab's persistent rainfall output (the CHIRPS TIF *cache* itself deliberately stays local-only — disposable/re-derivable, no benefit to routing through Drive); and `app.R`'s "Next →" button, which only checked the local path and so never appeared for Drive-only users. **If a new widget reads/writes user data, it must use `workspace()`/`ws_*()`, never reconstruct its own `mg2_workspace()` from the raw directory path.**

*Deploy-target limitation:* the redirect flow currently only works for local runs — it needs a fixed local port (`run_mg2(port = 8100)`, matching whatever redirect URI is registered on the OAuth client) and Google Cloud Console redirect URIs are per-exact-URL. Extending to shinyapps.io/Connect Cloud needs their public URLs registered as additional redirect URIs on the same "Web application" OAuth client (`fct_gdrive_oauth.R`'s `redirect_uri` is already computed dynamically from `session$clientData`, so no code change needed there — just GCP config).

*Google Cloud Console setup (one-time, done for the `dhis2jp` project):* Drive API enabled; OAuth consent screen in Testing mode with test users added (must include every account that will connect Drive); **two** OAuth clients exist — a "Desktop app" client (unused now, was for the abandoned `drive_auth()` console flow) and the "Web application" client actually in use, with `http://127.0.0.1:8100/` as an authorized redirect URI; `GOOGLE_CLIENT_ID`/`GOOGLE_CLIENT_SECRET` in `.Renviron` hold the Web application client's credentials; scope is the full `https://www.googleapis.com/auth/drive` (not `drive.file` — that scope only covers files the app created or the user picked via Google's own Picker widget, which doesn't support MG2's "paste a folder link" UX).

*`app.R` session-end grace period (root-caused 2026-07-20):* `session$onSessionEnded(stopApp)` used to fire immediately, a convenience so closing the browser tab frees the R console for local single-user runs. That's fatal to the OAuth redirect flow: clicking "Connect Google Drive" navigates the tab away to Google (closing the WebSocket = "session ended") and back — `stopApp()` was tearing down the whole server before the redirect could land, with no error printed (a clean intentional shutdown, not a crash — very confusing to debug). Fixed with a 90-second grace period (`later::later()`) before actually stopping, canceled if a new session (the redirect landing back) connects in the meantime. Normal "close tab and walk away" behavior is unchanged, just delayed.

**File rename (2026-07-20):** `evaluation_widget_2.R` → `evaluation_widget.R` (the old dead `evaluation_widget.R`, never sourced by `app.R`, was deleted first). Also removed unused `data.folder`/`data_folder` reactives (defined, never called) from `burden_widget.R`, `dqa_widget.R`, `regions_widget.R`.

**Climate tab:**
- `R/climate_functions.R` — pure R functions (no Shiny); `terra` + `sf` based
- `inst/shiny/climate_widget.R` — Shiny module; reads `geoFeatures` from metadata widget
- Downloads only the bounding box region (Africa TIF ~3–5 MB; global ~20 MB) — not the full
  continental raster. Auto-selects Africa vs global based on bbox via `.chirps_in_africa()`
- Two-level cache: (1) cropped CHIRPS TIFs in `climate_cache/` keyed by bbox+month+year;
  (2) extracted sf results as `results_*.rds` in `climate_cache/` — repeat runs load from
  RDS and skip extraction entirely. Progress shows "Extracting" (cached TIF) vs "Downloading".
- Polygons smaller than one CHIRPS pixel (~5 km) get centroid-based fallback; flagged in
  orange on the ggplot map grid and marked "(centroid est.)" in the leaflet popup
- All returned sf objects are WGS84 (leaflet-ready); no downstream reprojection needed
- Outputs: faceted ggplot choropleth, interactive leaflet, bar chart, statistics table, CSV + Excel download
- After every run, writes `rainfall_{year}_lvl{level}.rds` to the **data directory** (not
  `climate_cache/`) — a plain data frame (`id`, `name`, `parentName`, `year`, `month`,
  `mean_rain`) for use as a covariate in other analyses. Load with `chirps_load_rainfall(data_dir)`;
  join to ts data by `id + year + month`. See README § Climate for full code examples.
- Multi-year mode: "Year range" selector runs each year sequentially; results in Annual Maps /
  Multi-year Chart / Anomaly / Statistics / Data & Download tabs. Anomaly tab (≥2 years):
  diverging RdBu choropleth showing mm-deviation or z-score vs multi-year mean.
- Interactive leaflet: stable base map via `renderLeaflet`; data layers pushed via `leafletProxy`
  so the map never goes grey. `outputOptions(suspendWhenHidden = FALSE)` keeps it alive when tab hidden.
- Rainfall overlay in Evaluation tab: sidebar toggle appears when files exist; national average
  fallback when district join produces all NAs.
- Inspired by Mohamed Sillah Kanu's Python/Streamlit CHIRPS app (Apache-2.0); see README for attribution.

**DQA tab (0.1.9 additions):**
- Facilities sub-tab: active/inactive bar chart + summary table (`dqa_facility_status()`)
- Facility Heatmap sub-tab: facility × month reporting grid, filtered by region dropdown,
  height scales to facility count (`dqa_facility_heatmap_data()` / `dqa_facility_heatmap_plot()`)
- Reporting Map: works with demo data via `mg2_demo_geo`; `dqa_region_reporting()` no longer
  requires `geoFeatures` (map renderer guards separately)
- `dqa_facility_heatmap_data()`: Month kept as plain integer (days-since-epoch) throughout —
  `data.table::CJ()` and `merge()` strip yearmonth class; plot converts via
  `as.Date(as.integer(unclass(x)), origin="1970-01-01")`

**Demo data:**
- `mg2_demo_*` — Sierra Leone malaria (5 elements, 72 months synthetic)
- `mg2_demo_geo` — Sierra Leone district polygons (GADM level-2; Western Rural+Urban merged to
  Western Area); injected into metadata at `mg2_demo_setup()` write time
- `mg2_pdrlao_*` — PDR Lao malaria (82 elements, ~57 months, Aug 2021–May 2026)
- `mg2_demo_setup()` / `mg2_pdrlao_setup()` — write demo files to disk
- Login tab has two demo buttons (Sierra Leone / PDR Lao) using `actionButton` not `shinyFiles`

**Global-variable NOTE:** cleared on the `cran` branch (see CRAN_PLAN.md #7).

**Deployments:**
- GitHub releases: automated via `.github/workflows/release.yml` (push `git tag vX.Y.Z`)
- shinyapps.io: `rsconnect::deployApp(appDir="deploy", appName="MG2-MagicGlasses2")`
- Connect Cloud: republish via https://connect.posit.cloud/magicglasses (installs MG2 from `../` local clone)
- Both deployments use `MG2_DEMO_MODE=1` env var; demo buttons replace shinyFiles picker
- `deploy/app.R` auto-detects OS from `/etc/os-release` for PPM binary URL; installs `qs2`, `future.apply`, `feasts`, `ggtime` from PPM if missing; uses SHA-based check to reinstall MG2 only when repo HEAD changes — no version bumps needed
- `tsmodels()` forces `future::plan(sequential)` before fable `model()` to prevent fabletools 0.7.0 from requiring `future.apply` for nested parallelism

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

## Pending changes

### Default "Exclude most recent period" from the submission deadline (noted 2026-10-18)

The Reporting page checkbox `exclude_recent_month` (`inst/shiny/reporting_widget.r`,
`checkboxInput` ~L91, applied ~L885) defaults to `TRUE` always. Make the default follow
the data set's submission deadline instead:

- Deadline for the last month = end of that month + the data set's `timelyDays` (DHIS2's
  "days after period end to qualify for timely submission"; varies by country and data
  set). Use the latest deadline among the monthly data sets that contain the selected
  elements; fall back to 15 (DHIS2's default) when none has a value.
- Exclude if today is on or before the deadline (reports still arriving); include after.
- Show why beside the checkbox, e.g. "Reports for 2026-08 were due 2026-09-15 (NHMIS
  Monthly Summary, 15 days after the month ends); today is 2026-10-18, so included."
  A user's own tick/untick wins over the default.
- `timelyDays` is already fetched in the data set metadata (`metadata_widget.R` ~L526),
  so this needs no new API call. Weekly data: same rule on the week's end date.

Same change made in the Chap fork on 2026-10-18 (`chap-frontend`, branch
`feat/mg2-champion-selection`, `features/mg2-selection/recipe.ts`:
`submissionDeadline()`, `isBeforeDeadline()`, with tests). Nigeria's NHMIS data sets
carry `timelyDays = 15`. Possible refinement for both: also compare the last month's
facilities reporting with the median of the previous 12, for countries where reports
routinely arrive after the deadline.

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
- Analysis/report: `private/Nigeria/kebbi_tpr_rdt.Rmd` (moved 2026-07-13 — private, local-only
  git repo, excluded from MG2's repo/build; see "Private/local-only content" section below)
- Cross-project write-up: `../DHIS2 Best Practices/Drafts/nigeria_tpr_indicator_error.qmd`

---

## External Python reference code (2026-07-13)

Mohamed Sillah Kanu (Informatics Consultancy Firm – Sierra Leone; also Research Data Analyst
Associate, Northwestern University) built a set of Python/Streamlit tools covering the same
domain as MG2 — CHIRPS rainfall, DHIS2/NMDR dashboards, DQA, outlier detection, TPR, and
WHO Sub-National Tailoring (SNT). His GitHub (`mohamedsillahkanu`, 306 public repos) is mostly
disposable single-`index.html` demo exports with no inspectable logic, but four repos have
real Python source and were cloned to `dev/reference/` (gitignored + `.Rbuildignore`-excluded,
not part of the MG2 package — see `dev/reference/*/LICENSE`, all Apache-2.0, one-way
compatible with MG2's GPL-3 provided attribution is kept when porting logic):

| Local directory | Source repo | Contents | Relevance |
|---|---|---|---|
| `chirps-rainfall-app-python` | `blank-app` | `streamlit_app.py` — CHIRPS raster download/clip via `geopandas`+`rasterio`, GADM boundaries | Potential R port target (`terra`/`sf`) for a rainfall-covariate feature |
| `nmcp-analysis-app-python` | `blank-app-3` | `outlier_detection_and_correction.py` (32KB), `reporting.py`, `active_and_inactive_hf_adm2/3.py`, `combine_multiple_files.py`, `compute_new_variables.py`, `rename_variables.py` | Directly overlaps `R/outliers.R`, `dqa_functions.R`, `combine_functions.R` — worth diffing his outlier/reporting-rate logic against MG2's |
| `snt-package-python` | `snt-package` | Installable package: `setup.py` + `snt/core.py` (132KB) | Likely his consolidated/cleaned library version of the above — check first before the sprawling demo repos |
| `nmcp-sl-snt-python` | `nmcp-sl-snt` | `01Name Matching of Health Facilities.py` | Fuzzy facility-name matching — relevant to MG2 metadata/duplicate-detection work |

**Not cloned (checked, confirmed no real source — pure `index.html` demo exports):**
`dhis2-dqa`, `dqa-nmdr`, `dqa-updated`, `Final-DQA`, `Data-quality-assessment*`, `tpr`,
`tpr-heatmap`, `testing-rate`, `treatment-rate`, `presumptive-treatment-rate`,
`confirmed-not-treated`, `outlier-winsorisation-method`, `retrospective-analysis`,
`interupted-time-series-analysis`, `time-series-visualization1`, `nmdr-dashboard-test`,
`new-nmdr-set-up`, `nmdr-setup`, `introduction-nmdr`, `malaria-dashboard-nmdr`,
`dhis2-bulk-upload-v3`, `metadata-download`, `download-dataset2`, `dashboard-dhis2-copy`,
`auto-dhis2-instance-setup`.

**Look at later — forked SNT training libraries (not his original work):**
- `SNT-Code-Library` and `snt-library` — forked **NuMalariaModeling Faculty Enrichment
  Program** materials. Actually **R-based** (`.Rprofile`, `renv.lock`, Quarto site), not
  Python — potentially more directly reusable in MG2 than his Python work, since it's already
  R. Contains shapefile-overlay, health-facility mapping, and testing-rate walkthroughs
  (`README_Penta1_Python.md` is 3.1MB — likely a Jupyter export despite the Python name).
  Not yet cloned/inspected.
- `SNT` (his own repo) — `streamlit_app.py`/`HOME.py` are nearly empty (~3KB combined); the
  44MB repo size is almost entirely raw Sierra Leone district-level malaria XLS data
  (2015–2023), not code. Worth a look if MG2 ever needs Sierra Leone historical raw data for
  testing, but skip for code-porting purposes.

**Directory convention:** external reference clones live under `dev/reference/<name>/`,
gitignored (`.gitignore`) and excluded from `R CMD check` (`.Rbuildignore: ^dev$`) — see
`dev/deploy.md` for the sibling convention of non-package developer docs under `dev/`.

---

## Private/local-only content (2026-07-13)

The `jpainter/MG2` GitHub repo is **public**. `reports/Nigeria/*` (Nigeria TPR indicator
analysis) and `reports/uio_security_response.txt` (UiO security review response) had
already been pushed to `origin/main` before this was noticed. Both were moved to
`private/` at the MG2 repo root, which:

- Is fully excluded from MG2's git (`.gitignore: /private/`) and `R CMD check`
  (`.Rbuildignore: ^private$`) — MG2 will never track or push anything under it again.
- Has its **own independent local git repo** (`private/.git`, initialized 2026-07-13,
  `git init` run *inside* `private/`) — full commit history locally, **no remote
  configured**, so there is nothing to accidentally push.

**Still outstanding:** removing the files from MG2's tracked tree and pushing that removal
only stops them showing in the current GitHub browse view — the content remains fetchable
from `origin/main`'s prior commits (and any existing forks/clones) unless the public repo's
git history is actually rewritten (`git filter-repo` + force-push) or the repo is made
private. That is a deliberately separate, not-yet-taken decision — force-pushing a rewritten
history on a public repo is destructive (breaks existing clones/forks) and should only be
done with explicit sign-off, not as a routine cleanup step.
