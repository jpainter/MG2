# data-raw/mg2_demo.R
#
# Prepares the MG2 demo dataset from source files obtained from the public
# DHIS2 demonstration server at https://demos.dhis2.org/hmis_data.
#
# Source files (local, not committed to repo):
#   ~/Library/Mobile Documents/com~apple~CloudDocs/Projects/Formulas/PDRLao/
#     malaria_All-levels_6yrs_2026-05-12.rds
#     metadata_2026-04-22.rds
#     Formulas_2026_May12.xlsx
#
# Output (committed to repo in inst/extdata/demo/):
#   malaria_demo.rds       — processed data, 18 columns, xz-compressed
#   metadata_demo.rds      — org tree, geo features, validation rules
#   Formulas_demo.xlsx     — formula / element list
#
# The directory widget detects these files by name pattern:
#   "metadata" in name  → metadata file
#   "formulas" in name  → formula file
#   .rds without "metadata" → data file
#
# Run this script to regenerate the demo files after source data changes.
# Re-run devtools::document() and devtools::check() afterwards.

library(data.table)
library(tsibble)

src_dir  <- path.expand(
  "~/Library/Mobile Documents/com~apple~CloudDocs/Projects/Formulas/PDRLao"
)
dest_dir <- here::here("inst/extdata/demo")   # or set manually
# dest_dir <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path),
#                       "..", "inst", "extdata", "demo")

stopifnot(dir.exists(src_dir), dir.exists(dest_dir))

# ── 1. Processed data ────────────────────────────────────────────────────────

message("Reading source data...")
d <- MG2::read_file(file.path(src_dir, "malaria_All-levels_6yrs_2026-05-12.rds"))
setDT(d)

message("  ", nrow(d), " rows, ", ncol(d), " columns, ",
        format(min(d$Month)), " – ", format(max(d$Month)))

# Columns to keep:
#   Geographic hierarchy  — needed by all widgets
#   Month                 — time index (yearmonth class restored by read_file)
#   data                  — human-readable element name from formula
#   original              — raw numeric value from DHIS2
#   Outlier flags         — needed by Outliers / Cleaning widget
#     key_entry_error, over_max, mad15, mad10, mad5, seasonal5, seasonal3
#
# Columns dropped:
#   value          — legacy logical presence flag (all TRUE), no longer used
#   .max           — outlier threshold used to compute over_max; not needed after
#   AllSmall, not_key_or_over_under, not_mad  — intermediate pipeline flags
#   expected       — combine-tab replacement value; not critical for demo
#   aggregationType, attributeOptionCombo, followup  — DHIS2 API internals
#   dataElement    — raw API element name superseded by human-readable `data`
#   dataSet        — dataset label, not used in analysis
#   data.id        — DHIS2 element UID, not needed after translation

keep_cols <- c(
  "orgUnit", "orgUnitName", "level",
  "Country", "Province", "District", "Facility",
  "effectiveLeaf",
  "Month",
  "data", "original",
  "key_entry_error", "over_max", "mad15", "mad10", "mad5",
  "seasonal5", "seasonal3"
)

missing <- setdiff(keep_cols, names(d))
if (length(missing) > 0) stop("Columns missing from source: ", paste(missing, collapse = ", "))

d_demo <- d[, ..keep_cols]

message("  Saving ", ncol(d_demo), " columns...")
out_data <- file.path(dest_dir, "malaria_demo.rds")
saveRDS(d_demo, out_data, compress = "xz")
message("  -> ", out_data, " (", round(file.size(out_data) / 1e6, 1), " MB)")

# ── 2. Metadata ──────────────────────────────────────────────────────────────

message("Reading metadata...")
meta <- readRDS(file.path(src_dir, "metadata_2026-04-22.rds"))
message("  Keys: ", paste(names(meta), collapse = ", "))

# The systemInfo shows demos.dhis2.org — fine to distribute publicly.
# No credentials or private URLs present.

out_meta <- file.path(dest_dir, "metadata_demo.rds")
saveRDS(meta, out_meta, compress = "xz")
message("  -> ", out_meta, " (", round(file.size(out_meta) / 1e6, 1), " MB)")

# ── 3. Formula ───────────────────────────────────────────────────────────────

message("Copying formula...")
out_formula <- file.path(dest_dir, "Formulas_demo.xlsx")
file.copy(
  file.path(src_dir, "Formulas_2026_May12.xlsx"),
  out_formula,
  overwrite = TRUE
)
message("  -> ", out_formula, " (", round(file.size(out_formula) / 1e3, 0), " KB)")

# ── Summary ──────────────────────────────────────────────────────────────────

total_mb <- sum(file.size(c(out_data, out_meta, out_formula))) / 1e6
message("\nDemo data written to: ", dest_dir)
message("Total size: ", round(total_mb, 1), " MB")
message("Run devtools::document() then devtools::check() to verify.")
