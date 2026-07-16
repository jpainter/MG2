# Climate Widget Development Notes

**Module:** `inst/shiny/climate_widget.R` + `R/climate_functions.R`  
**Status:** Dev (functional end-to-end)  
**Completed:** July 2026

---

## Inspiration

The Climate tab was inspired by a Python/Streamlit CHIRPS rainfall tool built by
**Mohamed Sillah Kanu** (Informatics Consultancy Firm – Sierra Leone; Research Data
Analyst Associate, Northwestern University). His code is cloned locally at
`dev/reference/chirps-rainfall-app-python/` (Apache-2.0 licence, one-way compatible
with MG2's GPL-3 provided attribution is kept when porting logic).

His tool demonstrated the core workflow:

- Fetch monthly CHIRPS rasters from the UCSB/CHC server
- Clip the raster to the study area bounding box rather than downloading the full
  continental file
- Compute zonal means over administrative polygons using `geopandas` + `rasterio`
- Visualise per-polygon rainfall on a choropleth map

---

## What was preserved

| Concept | Python original | MG2 R implementation |
|---|---|---|
| CHIRPS data source | CHC/UCSB `.tif` files via direct URL | Same URL scheme (`chirps_download_tif()`) |
| Bounding-box crop | `rasterio` window read from bbox | `terra::crop()` to bbox of loaded polygons |
| Africa vs global dataset | Checked bbox against Africa extent | `.chirps_in_africa()` — same bbox thresholds |
| Zonal statistics | `rasterstats.zonal_stats()` mean | `terra::extract(..., fun = mean)` |
| Choropleth output | Static matplotlib figure | ggplot2 `geom_sf` faceted choropleth |

---

## What was dropped

- **GADM boundaries** — the Python tool required external GADM shapefiles downloaded
  separately. MG2 reads org unit polygon boundaries (`geoFeatures`) directly from the
  DHIS2 metadata already loaded in the Metadata tab. No separate shapefile needed for
  standard use.
- **Streamlit UI patterns** — replaced entirely by a Shiny module.
- **geopandas / rasterio / Python stack** — replaced by `terra` + `sf`.
- **Country-level selector** — the Python tool let users pick a country from a dropdown.
  MG2 works with whatever boundaries are loaded in the session (DHIS2 or custom upload),
  so there is no separate country concept.
- **Per-admin-level hardcoding** — the Python tool assumed fixed GADM levels (0–3).
  MG2 reads level names and polygon counts from `orgUnitLevels()` and lets the user
  choose which level to analyse.

---

## What is new in MG2

### Boundary integration
- Reads `geoFeatures` polygons directly from the Metadata widget — no external file needed.
- **Small polygon fallback**: org units smaller than one CHIRPS cell (~5 km) have no pixel
  centroid inside them. These are handled by centroid-based point extraction and flagged with
  an orange border on the map grid and *(centroid est.)* in the leaflet popup.
- **Optional custom shapefile**: an `actionLink` in the sidebar allows loading an external
  `.shp` / `.gpkg` when DHIS2 boundaries are not available.

### Two-level cache
1. **TIF cache** (`climate_cache/` in the data directory): cropped CHIRPS rasters keyed by
   bounding box + month + year. A cached TIF is reused without re-downloading; progress
   shows "Extracting" vs "Downloading" accordingly.
2. **Results cache** (`climate_cache/results_*.rds`): extracted sf objects keyed by year,
   months, level, and bbox. When a matching RDS exists the entire extraction step is skipped.

### Flat rainfall files for cross-widget use
After every successful run the widget writes `rainfall_{year}_lvl{level}.rds` to the **data
directory** (not the cache). These are plain data frames with columns
`id | name | parentName | year | month | mean_rain` — no geometry, no package dependencies.

Load all years with `chirps_load_rainfall(data_dir)`, which scans the directory, combines
all files, and de-duplicates on `id × year × month`. Use as a covariate or for lag
analysis directly in R scripts or TSLM models (see README § Climate).

### Outputs (single-year mode)
| Tab | Content |
|---|---|
| Map Grid | Faceted ggplot choropleth, one panel per selected month |
| Chart | Monthly bar chart — mean ± SD across org units; toggle to group bars by parent unit |
| Interactive | Leaflet map with per-polygon popup; month selector; stable base map via `renderLeaflet` + `leafletProxy` (map never goes grey) |
| Statistics | Summary table: month, N polygons, mean, SD, min, max |
| Data & Download | Flat data preview; CSV and Excel download (Excel has three sheets: data, statistics, metadata) |

### Multi-year mode
Select a year range (no upper limit; cached years load instantly). Runs each year
sequentially, saving flat files and results cache as it goes. Results appear in:

| Tab | Content |
|---|---|
| Annual Maps | Faceted choropleth, one panel per year (`ncol = 3`) |
| Multi-year Chart | Toggle between **Annual totals** and **Monthly breakdown** views (see below) |
| Anomaly | Diverging RdBu choropleth — each year vs the multi-year mean (see below) |
| Statistics | Combined stats table across all years |
| Data & Download | Combined flat data for all years; CSV and Excel |

### Multi-year chart
**Annual totals view**: one bar per year; horizontal dashed line = multi-year mean;
light grey band = ±1 SD across years; darker grey band = ±2 SD across years.

**Monthly breakdown view**: dodged bars, one colour per year; dashed line = multi-year
monthly mean (across all selected years); light grey ribbon = ±1 SD; darker grey ribbon =
±2 SD. Colour palette is a curated 12-colour qualitative set (`.chirps_year_palette()`),
interpolated via `colorRampPalette()` for ranges longer than 12 years so no year is ever
assigned white or an indistinguishable colour.

### Anomaly map
Requires ≥ 2 years. For each polygon, computes the per-year mean rainfall then
subtracts the multi-year mean. Two display modes:

- **mm deviation** — raw difference in millimetres; data-driven symmetric scale.
- **Z-score** — deviation divided by the multi-year SD; **fixed scale −3 to +3** with
  0.5-unit breaks regardless of actual data range, so minor anomalies (z ≈ 1) do not
  appear visually extreme. Values beyond ±3 are squished to the palette limits.

### Rainfall overlay in the Evaluation tab
Once at least one `rainfall_*.rds` file is present in the data directory, a "Rainfall
overlay" toggle appears in the Evaluation tab's Data sidebar. When enabled:

- Blue semi-transparent bars are drawn behind the time-series trend line.
- Bars are scaled to the primary y-axis (`k = primary_max / rain_max`).
- At national level, rainfall is averaged across all org units in the rainfall file.
  At sub-national level, a name-based join is attempted first; if it produces all NAs
  (e.g. the admin column holds a national name, not district names), it falls back to the
  national average.

---

## Key technical decisions

| Decision | Rationale |
|---|---|
| `terra` + `sf` rather than `stars` | `terra` is faster for single-band TIF crop+extract; `sf` for polygon handling and leaflet; `stars` would add complexity for no gain here |
| Stable leaflet base map + `leafletProxy` | `renderLeaflet` inside `renderUI` recreates the DOM node on each reactive update, causing a grey map. Separating the base map (static `renderLeaflet`) from data layers (`leafletProxy`) fixes this. |
| `outputOptions(suspendWhenHidden = FALSE)` | Keeps the leaflet output alive when the Interactive tab is hidden, preventing the grey-on-first-visit problem. |
| Flat `.rds` files in data dir, not cache | Cache files are keyed by bbox and may be cleared; flat rainfall files are keyed by year+level and are stable outputs intended for downstream use across analysis sessions. |
| `ncol = 3` explicit in `facet_wrap` | Consistent 3-column layout for both Annual Maps and Anomaly; height formula `300px × rows + 120px` matches exactly, preventing bottom-row clipping. |
| Z-score fixed at ±3 | A data-driven scale makes any variation look extreme. Fixing at ±3 gives a statistically meaningful reference: values beyond ±2 SD are already unusual in a normal distribution. |
