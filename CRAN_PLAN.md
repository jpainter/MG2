# MG2 CRAN Preparation Plan

Created: 2026-03-19 · Rewritten: 2026-11-14 (replaces the April plan; Phases A–C
of that plan are done or superseded — demo data, NEWS.md and four vignettes exist)

## Branches

- `main` — stays at 0.1.9 for `install_github()` users. Not touched until CRAN accepts.
- `cran` — cut from `feature/cloud-workspace` (which already contains every fix on
  main, cherry-picked). All CRAN work happens here.
- After acceptance: bring `main` up to the released version, tag it, and resume normal
  feature-branch work. `main` should then always equal the latest CRAN release.

## Baseline (2026-11-14, `devtools::check(cran = TRUE)`, R 4.6.2, macOS)

0 ERRORs · 0 WARNINGs · 2 NOTEs, but the check misses several CRAN-policy problems
(below). Source tarball was **8.6 MB**; CRAN's limit is ~5 MB.

Running check locally: export the tree to local disk first (iCloud hydration is very
slow — see CLAUDE.md). After an R minor upgrade, reinstall compiled packages: the 4.5
builds of units/anytime/etc. segfault on load and kill the check at "lazy loading".

## Check status (2026-11-16, after steps 1–4)

`devtools::check(cran = TRUE)` on a local-disk copy: **0 errors · 0 warnings · 0 NOTEs** (after #7). Vignettes build; examples and tests pass.
Run with `_R_CHECK_EXCESSIVE_IMPORTS_=20` to match CRAN: the Imports count (31) is
reported as INFO, not a NOTE.

## 1. Blockers — CRAN rejects without these

| # | Item | Status |
|---|---|---|
| 1 | **Tarball size.** 8.6 → **4.1 MB** (built without vignettes). `mg2_pdrlao_meta`/`_processed` moved to the GitHub release `demo-data`; `mg2_pdrlao_setup()` downloads them once (MD5-checked, cached in `R_user_dir("MG2","cache")`). `inst/extdata/demo` is *not* a duplicate: it is Lao data read by the vignettes. Remaining big items (`malaria_demo.rds` 1.4 MB, `metadata_demo.rds` 0.6 MB) are used by 3 vignettes — candidates if headroom is needed later. | ✅ |
| 2 | **No writes to home dir.** `~/.mg2_config.rds` → `tools::R_user_dir("MG2","config")`; legacy file still read once. | ✅ `.mg2_config_path()` / `.mg2_load_config()` / `.mg2_save_config()` in `utils.R` |
| 3 | **No writes to `.GlobalEnv`.** App session count / pending-stop handle. | ✅ `MG2:::.mg2_app_state` (in `run_app.R`) |
| 4 | **Restore options and future plans.** `tsmodels()`, `pre_impact_fit()`, `impact_fit()`, `api_data()`. | ✅ `on.exit()` added |
| 5 | **No `:::` into other packages.** `future:::FutureRegistry` in `.mg2_restore_plan()`. | ✅ removed (was a no-op under future 1.82) |
| 6 | `.Rbuildignore`: `MG2.Rcheck`, `*.tar.gz`, `vignettes/*.html`. | ✅ |

## 2. Reviewer pushback — fix before submitting

| # | Item | Size of job |
|---|---|---|
| 7 | **Global-variable NOTE — cleared (0 notes).** 742 entries → 0: (a) 38 unreachable functions (call-graph check from exports + app/vignette/test references) moved to `dev/unused_R/`; (b) bare calls pinned with `@importFrom` in `R/imports.R`; (c) reviewed column names in `R/globals.R`. Review found real bugs, fixed rather than declared: `mg2_demo_setup()` without `library(MG2)`, `api_data()` without progressr, `selectedData(level=)`, `htsFormula()`, `api_data` shiny+parallel `progress$`. | ✅ |
| 8 | **Console output — done.** Call-graph walk found 97 `cat()`/`print()` reachable from exports not behind a verbosity flag (most output was already behind `.cat`). Gated on each function's existing flag (`print` in api_data, `verbose` in combine, `msg` in tsmodels, `progress` in the outlier scans, `.cat`); error-like ones became `warning()` (failed combine step, Prophet fallback, bad `tsmodels(type=)`); `fetch_validation_rules` page counter → `message()`; three trace lines in `dataset()` removed. Remaining `cat()` is app-only code or behind a flag. | ✅ |
| 9 | **`\value`** — all exported functions have one (3 were missing: `dataTotal`, `mostFrequentReportingOUs`, `selectedData`). | ✅ |
| 10 | **`\dontrun{}`** — only `loginDHIS2`, `dhis2_get`, `api_data` keep it (need a live DHIS2 server). Dataset, `read_file`, `ous_tree`, `mg2_demo_setup` examples now run (write only to `tempdir()`); `run_mg2` uses `if (interactive())`. | ✅ |
| 11 | **162 exports → 67.** App-only functions are `@noRd` internals. Prerequisite done first: `inst/shiny/app.R` now evaluates `app_main.R` in `MG2:::.mg2_app_env()` (all namespace functions, parent = globalenv), so widgets see internals by bare name and nothing is sourced into `.GlobalEnv`. | ✅ |
| 12 | **Imports 42 → 31 non-base.** Dropped: assertthat, data.tree (`ous_tree()` → base R `.ous_paths()`, identical output), stringi (base regex), tictoc (`.mg2_elapsed()`). To Suggests: terra, googledrive, ggiraph (static-plot fallback), rstudioapi, plus httpuv / parallelly / distributional (always installed via shiny / future / fabletools; guarded anyway). With `_R_CHECK_EXCESSIVE_IMPORTS_=20`, R 4.6 reports the count as **INFO**, not a NOTE — still worth one line in cran-comments. | ✅ |
| 13 | **`later`, `readxl` → Imports** (both used only in `inst/shiny`; declared via `@importFrom` in `zzz.R` so check counts them as used). | ✅ |
| 14 | **DESCRIPTION wording** — `'DHIS2'`/`'Shiny'` quoted in Title and Description, `<https://dhis2.org>` added. | ✅ |
| 15 | **Examples / tests / vignettes — audited.** Vignettes: network chunks are `eval = FALSE`, no file writes. Tests: the one test that contacts play.dhis2.org now has `skip_on_cran()`. `mg2_pdrlao_setup()` gained `cache_dir =`; its `\donttest` example caches to `tempdir()` and catches download failure. | ✅ |

## 3. Quality — not required, strongly advised

- Tests: 18 `test_that` blocks today. Priorities from the April plan still apply
  (`as.yearmonth`, `date_code`, outlier functions, `df_pre_ts`/`df_ts`, DQA reporting).
- `urlchecker::url_check()`, `spelling::spell_check_package()`.
- `rhub::rhub_check()` / win-builder (`devtools::check_win_devel()`) for Linux/Windows.
- `cran-comments.md`.

## Section 3 — done 2026-11-16

- `urlchecker::url_check()`: 5 dead/moved URLs fixed (README, `mg2_demo_dir`, `mg2_demo_geo`).
- `spelling::spell_check_package()`: one typo fixed; 154 words in `inst/WORDLIST`.
- Rd: `≤ ≈ →` replaced in roxygen — pdflatex could not typeset them (only seen with `manual = TRUE`).
- `cran-comments.md` written. DESCRIPTION `Date:` updated.
- `check(cran = TRUE, remote = TRUE, manual = TRUE, run_dont_test = TRUE)`:
  **0 errors · 0 warnings · 2 NOTEs** — "New submission" and local HTML Tidy too old
  (environmental; CRAN has a current Tidy).

## Final step

```r
devtools::check(cran = TRUE, remote = TRUE, manual = TRUE)   # 0 E · 0 W · ≤1 NOTE ("New submission")
devtools::check_win_devel()
devtools::submit_cran()
```
