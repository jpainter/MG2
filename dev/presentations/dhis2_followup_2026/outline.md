# DHIS2 Follow-up Presentation — Outline
## "Beyond Raw Data: Where Analysis Happens, and Where Do the Results Go?"

**Format:** ~10 slides, PDF (Quarto source), ~1 hour Zoom  
**Time split:** ~20 min slides, 10 min structured Q&A on open questions, 30 min open floor  
**Audience:** DHIS2 CoP, HISP staff, core developers  
**Goal:** Raise awareness of the output-routing problem and invite collaboration on design  

---

## Background / Context

- Follow-up to DHIS2 Annual Conference demo (movies produced; this is a developer-audience zoom)
- Original presentation was mostly a demo + rationale
- This session: go deeper on one specific development challenge

---

## Slide 1 — Title / Context

- Brief recap of MG2 and the conference demo
- This session: a developer conversation, not a demo
- Central question: *"DHIS2 is where the data lives — but what if the analysis it needs can't be done inside DHIS2?"*

---

## Slide 2 — Why Build Outside DHIS2?

The case for external statistical environments (R or Python):

- DHIS2 predictors and indicators cover a narrow range: simple math, basic regressions
- What epidemiologists actually need: time-series decomposition, outlier detection algorithms,
  reporting-bias adjustment, forecast models (ETS/ARIMA/MASE), burden estimation with
  statistical uncertainty
- R's `fable`/`feasts` ecosystem — not replicable in JS/Java analytics
- Same argument applies to Python (`statsmodels`, `prophet`, `scikit-learn`)
- Key tension: the richer the analysis, the further you are from where the data lives and
  where users expect to see results

*This establishes the problem before touching MG2 specifically.*

---

## Slide 3 — What MG2 Produces (the output inventory)

Quick inventory of what comes *out* of a typical MG2 analysis session — makes the
"where does it go?" question concrete:

- Reporting-adjusted case counts (per facility, per month)
- Outlier flags (6 algorithms: key-entry-error, over-max, MAD×15, MAD×10, seasonal×5, seasonal×3)
- Time-series model outputs: trend, seasonal decomposition, forecasts (multiple model types)
- WPE intervention effect estimates
- Burden estimates (5 methods, subnational)
- Climate covariates (CHIRPS rainfall, extracted by org unit boundary)

~10 output types, some with parameter variants. Sets up contrast with CHAP later.

---

## Slide 4 — The Gap: Results Trapped in the Session

Current state:

- Analyst runs MG2, explores parameters, runs models
- Results live in R session and local files
- Standard country users (district health officers, program managers) have no path to see results
- "The person who needs the output is not the person running the analysis"
- Fine for exploration; bad for routine use

**Diagram idea:** Analyst laptop → results → dead end  
vs.  
Analyst laptop → results → ??? → program dashboard

---

## Slide 5 — Positioning Diagram

2×2 (or axis diagram) showing where existing tools sit and where the gap is:

```
              FIXED OUTPUTS <————————————> MANY OUTPUTS
                    |                            |
ANALYST-ONLY        |   SNT (today)              |   MG2 (today)
                    |                            |
DHIS2-VISIBLE       |   CHAP (today)             |   ??? (unsolved)
```

The bottom-right quadrant is the honest gap. This presentation is about why it is hard to
get there and what the options are.

---

## Slide 6 — Three (Four) Architectural Options

Present as a genuine design question, not a solved problem:

| Option | Where results go | Pros | Cons |
|--------|-----------------|------|------|
| A. Keep in app | External app session / files | Fast iteration, no DHIS2 config | Not accessible to standard users |
| B. Push to source instance | Original production DHIS2 | Users already have access, existing dashboards | Namespace pollution, governance burden, risk to production |
| B2. Aggregate Data Exchange API | Source or separate instance (DHIS2 v2.39+) | Reusable exchange configs, scheduled jobs, cross-instance PAT auth | Still requires pre-created data elements |
| C. Separate output instance | New DHIS2 instance (local or hosted) | Clean separation, full DHIS2 API | Another system to maintain |
| D. Non-DHIS2 store | PostgreSQL + Superset / Grafana | Maximum flexibility | Leaves the DHIS2 ecosystem entirely |

No right answer — depends on deployment context. MG2 is currently building Option C.

**Note:** The Aggregate Data Exchange API (`/api/aggregateDataExchanges`) was introduced in
DHIS2 v2.39 and is a better foundation for systematic/recurring cross-instance pushes than
calling `dataValueSets` directly. Worth raising with the group.

---

## Slide 7 — CHAP as a Reference Model

CHAP (Climate Health Analytics Platform, University of Oslo / HISP):
- Python/FastAPI backend + React frontend; runs as Docker Compose sidecar alongside DHIS2
- Pulls DHIS2 disease data + climate covariates, runs forecast models, pushes predictions back
- Model interface: plug-in MLproject specs; input always standardized CSV
  (`time_period, rainfall, mean_temperature, disease_cases, location`)
- **Output schema is fixed: 5 quantile bands** (10th, 25th, 50th, 75th, 90th percentile)
  + optional outbreak alert flag

**How the push back works (important detail):**
1. User runs prediction in the Modeling App
2. User opens "Import prediction" form
3. User must select 5 **pre-existing** DHIS2 data elements (one per quantile) — CHAP does not create them
4. Dry-run validation → optional clear of ±12-month window → POST to `/api/dataValueSets`

**What this means:** The admin must pre-create the mapping before the first import.
The "clear previous values" sweep is a real gotcha — it can overwrite legitimate data if
data elements are shared with other purposes.

**Within its scope this is elegant.** For forecasting one disease with one model shape,
5 quantile bands is exactly the right output. The simplicity is a feature.

**Framing question:** Does that simplicity assumption hold for general health data analysis?

---

## Slide 8 — The "Many Outputs" Problem

Where the CHAP model starts to strain for richer analysis:

| Output type | DHIS2 representation | Problem |
|---|---|---|
| Adjusted case count | NUMBER, SUM | Straightforward — the easy one |
| Forecast median | NUMBER, AVERAGE | Fine, but which model? which parameters? |
| Outlier flag | BOOLEAN or NUMBER | Loses the *reason* (algorithm, threshold) |
| WPE effect | NUMBER, AVERAGE | Unitless, meaningless without model context |
| Reporting rate | Needs numerator + denominator | Two data elements + calculated indicator |
| Burden estimate | NUMBER, but 5 methods → 5 data elements | Which one does a health officer look at? |

**Root cause:** DHIS2 data elements are optimized for data entry, not model metadata.
No concept of provenance, version, or parameter set.

**Practical problems:**
- Each output variant needs its own data element — potentially dozens or hundreds
- Data element naming becomes a governance problem
- Seasonal parameters change the outputs — same name, different values depending on analyst choices
- Outlier flags are analyst judgment calls, not measurements — semantically different

**Question for the group:** Is there a DHIS2 metadata pattern — attribute option combos,
data store API, custom attributes — that could carry model provenance alongside values?

---

## Slide 9 — What We Built: The Output Tab

Four-step pipeline (functional today except Step 4 model outputs):

1. **Source info** — read from loaded metadata, no new config needed; shows source system
   name, version, URL, org unit count, data element count
2. **Spin up a local DHIS2 instance** — `d2 cluster up` with matching DHIS2 version;
   polls until ready; structured error handling for missing `docker-compose`,
   Docker not running, d2 CLI not found
3. **Import metadata** — org units (batched to avoid nginx 504), source data elements
   **dynamically inferred from the loaded formula** (nothing hardcoded; valueType and
   aggregationType inferred from observed values), + 4 fixed MG2 output data elements
4. **Write values** — raw data via `dataValueSets` API in batches of 2000; model outputs
   (placeholder — Step 4 is in progress)

**Key design choice to highlight:** Data elements in Step 3 come from whatever formula is
loaded — they are never hardcoded. The output instance mirrors the analyst's current dataset.
This is different from CHAP's approach where the admin pre-creates a fixed mapping.

**The 4 fixed MG2 output UIDs:** `MG2trendEst`, `MG2adjEstim`, `MG2forecast`, `MG2wpeEffct`
— analogous to CHAP's 5 quantile slots, the "curated small set" for initial model output push.
Step 4 is a placeholder: *what exactly should go there?* — good discussion point.

**Demo opportunity:** Screenshots of the 4-step card UI, green Done badges, DHIS2 instance
opening in browser with imported data visible.

---

## Slide 10 — Open Design Questions / Call to Action

Honest list of unsolved problems — invite collaboration:

**Technical questions:**
- Step 4 (model outputs): what granularity? facility-level or district-level only?
  All months or forecast horizon only? One value per output type or full quantile bands?
- Pushing to source instance: is there a safe path? Separate attribute option combo?
  Custom namespace? DHIS2 data store API for model metadata alongside values?
- Versioning: if the analyst re-runs with different parameters, old values are overwritten silently —
  is that acceptable?
- Production deployment: Option C (separate instance) needs hosting — what is the right
  companion infrastructure?

**Design philosophy questions:**
- Should MG2 reduce what it writes to a small curated set (CHAP style) or expose everything?
- Is there a shared standard worth proposing for how external statistical apps return
  results to DHIS2?

**Call to action:**
- Has anyone else solved the "many model outputs" problem within DHIS2?
- Is there a DHIS2 metadata pattern for provenance / versioning of computed values?
- Interest in collaborating on a standard for external-analytics-to-DHIS2 output routing?
- MG2 is open source (GPL-3): [github.com/jpainter/MG2](https://github.com/jpainter/MG2)

---

## Research Notes

### CHAP sources consulted (2026-08-15)
- GitHub: `dhis2-chap/chap-core`, `dhis2-chap/chap-frontend`
- Key files: `usePostPredictionData.ts`, `predictionImportDataValues.ts`
- Architecture: FastAPI + PostgreSQL + Redis/Celery + MLproject model sidecars
- License: AGPL-3, University of Oslo / HISP, Wellcome funded
- Requires DHIS2 v2.40.7+
- Climate data pre-loaded via separate DHIS2 Climate App (Google Earth Engine)

### SNT (MACEPA/PATH) finding
- SNT is fundamentally document-output, not DHIS2-integrated
- Outputs are Excel/PDF/maps given to national programs — results never pushed back to DHIS2
- The gap MG2 addresses is real: no existing tool has solved rich epidemiological analysis output routing

### DHIS2 Aggregate Data Exchange API
- Introduced v2.39: `/api/aggregateDataExchanges`
- Reusable exchange configs, scheduled jobs, cross-instance PAT auth, ID scheme mapping
- Better foundation than raw `dataValueSets` for systematic/recurring pushes
- Worth raising with the developer audience as a potential building block

### MG2 output module (as of 2026-08-15)
- `R/fct_output_dhis2.R` — backend functions
- `inst/shiny/output_widget.R` — 4-step Shiny UI
- Steps 1–3 functional; Step 4 (model output writing) is a placeholder
- Uses `processx` to spawn `d2 cluster up`; polls for DHIS2 readiness; structured error UI
