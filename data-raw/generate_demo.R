# data-raw/generate_demo.R
#
# Generates three demo datasets shipped with the MG2 package:
#   mg2_demo         — raw API-style data frame (matches api_data() output)
#   mg2_demo_formula — formula elements table (matches formula_widget output)
#   mg2_demo_meta    — metadata list (matches metadata_YYYY-MM-DD.rds)
#
# Source: Sierra Leone DHIS2 demo instance
#   URL:      https://play.im.dhis2.org/stable-2-41-8/
#   Username: admin  |  Password: district
# Requires live internet access. Run once; outputs are committed to data/.
# Re-run when the demo instance is refreshed or data elements change.
#
# Usage:
#   source("data-raw/generate_demo.R")

devtools::load_all() # for dhis2_get(), ous_tree(), date_code()
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(purrr)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

BASEURL <- "https://play.im.dhis2.org/stable-2-41-8/"
USERNAME <- "admin"
PASSWORD <- "district"

# 5 malaria data elements (TV5zBFFkZLu / Malaria cases confirmed excluded:
# analytics table not built on this demo instance — returns HTTP 409)
DE_IDS <- c(
  "p4K11MFEWtw", # Inpatient malaria cases
  "wWy5TE9cQ0V", # Inpatient malaria deaths
  "wZwzzRnr9N4", # RDT positive
  "Qk9nnX0i7lZ", # RDT negative
  "AFM5H0wNq3t"  # Malaria treated at PHU with ACT < 24 hrs new
)

DE_NAMES <- c(
  "Inpatient malaria cases",
  "Inpatient malaria deaths",
  "RDT positive",
  "RDT negative",
  "Malaria treated at PHU with ACT < 24 hrs new"
)

FORMULA_NAME <- "Sierra Leone Malaria"

get_sl <- function(path, ...) {
  dhis2_get(
    paste0(BASEURL, path),
    username = USERNAME,
    password = PASSWORD,
    ...
  )
}

# ---------------------------------------------------------------------------
# 1. Metadata
# ---------------------------------------------------------------------------

message("\n=== Fetching metadata ===")

message("- system info")
systemInfo <- get_sl("api/system/info")

message("- org unit levels")
ouLevels_raw <- get_sl(
  "api/organisationUnitLevels.json?fields=id,name,level,lastUpdated,created,displayName&paging=false&order=level:asc"
)
orgUnitLevels <- ouLevels_raw$organisationUnitLevels |>
  as_tibble() |>
  mutate(level = as.integer(level)) |>
  rename(levelName = name) |>
  select(id, level, levelName, lastUpdated, created, displayName)

message("- org units (all levels; may take a moment)")
ous_raw <- get_sl(
  "api/organisationUnits.json?fields=id,name,level,leaf,parent[id,name]&paging=false"
)
orgUnits_raw <- ous_raw$organisationUnits

# Flatten parent column (API returns list-column)
orgUnits <- orgUnits_raw %>%
  as_tibble() %>%
  mutate(
    parent.id = if (is.data.frame(parent)) {
      parent$id
    } else {
      map_chr(parent, ~ .x$id %||% NA_character_)
    },
    parent.name = if (is.data.frame(parent)) {
      parent$name
    } else {
      map_chr(parent, ~ .x$name %||% NA_character_)
    }
  ) %>%
  select(-parent)

# Join levelName onto orgUnits (matches what metadata_widget does during live fetch)
orgUnits <- orgUnits |>
  dplyr::left_join(
    dplyr::select(orgUnitLevels, level, levelName),
    by = "level"
  )

message("- building org unit tree")
ousTree <- ous_tree(orgUnits, orgUnitLevels)

message("- data elements")
de_raw <- get_sl(
  paste0(
    "api/dataElements.json?fields=id,name,shortName,displayName,displayShortName,",
    "categoryCombo[id,name],periodType,zeroIsSignificant&paging=false&",
    "filter=id:in:[",
    paste(DE_IDS, collapse = ","),
    "]"
  )
)
dataElements_raw <- de_raw$dataElements %>%
  as_tibble() %>%
  mutate(
    categoryCombo.id = categoryCombo$id,
    categoryCombo.name = categoryCombo$name
  ) %>%
  select(-categoryCombo)

message("- data element groups")
deg_raw <- get_sl(
  "api/dataElementGroups.json?fields=id,name,dataElements[id]&paging=false"
)
dataElementGroups <- deg_raw$dataElementGroups %>%
  as_tibble() %>%
  tidyr::unnest(dataElements, names_sep = "_") %>%
  rename(
    dataElement.id = dataElements_id,
    dataElementGroups.id = id,
    dataElementGroup = name
  )

message("- data sets")
ds_raw <- get_sl(
  paste0(
    "api/dataSets.json?fields=id,name,periodType,dataSetElements[dataElement[id]]&paging=false&",
    "filter=dataSetElements.dataElement.id:in:[",
    paste(DE_IDS, collapse = ","),
    "]"
  )
)
dataSets_raw <- ds_raw$dataSets %>% as_tibble()

# dataSets. (with dataSetElements expanded — used by data_1)
# After unnest(), dataSetElements_dataElement is a nested data.frame column
# (not a list-column), so we extract $id directly rather than via map().
dataSets_dot <- dataSets_raw %>%
  unnest(dataSetElements, names_sep = "_") %>%
  rename(
    dataSet.id = id,
    dataSet = name
  ) %>%
  mutate(
    # Extract the DE id from the nested data.frame column
    de_id = dataSetElements_dataElement$id,
    # Rebuild as a list-column of tibbles in the shape data_1() expects:
    # unnest(dataSetElements.id, names_sep="_") → dataSetElements.id_dataElement
    # with a nested data.frame containing $id
    dataSetElements.id = map(de_id, ~ tibble(dataElement = tibble(id = .x)))
  )

message("- category combos")
cc_raw <- get_sl(
  "api/categoryCombos.json?fields=id,name,categoryOptionCombos[id,name]&paging=false"
)
categoryCombos <- cc_raw$categoryCombos %>% as_tibble()

message("- category option combos")
coc_raw <- get_sl(
  "api/categoryOptionCombos.json?fields=id,name,categoryCombo[id,name]&paging=false"
)
categoryOptionCombos_raw <- coc_raw$categoryOptionCombos %>% as_tibble()

# Build categories table (same logic as metadata_widget categories reactive)
cc_flat <- categoryCombos %>%
  select(id, name, categoryOptionCombos) %>%
  rename(categoryCombo.id = id, categoryCombo = name) %>%
  unnest(categoryOptionCombos) %>%
  rename(categoryOptionCombo.id = id)

coc_flat <- categoryOptionCombos_raw %>%
  rename(categoryOptionCombo.id = id, categoryOptionCombo = name) %>%
  unnest(categoryCombo, names_sep = "_") %>%
  rename(categoryCombo.id = categoryCombo_id)

cc_coc <- coc_flat %>%
  left_join(
    categoryCombos %>%
      select(id, name) %>%
      rename(categoryCombo.id = id, categoryCombo = name),
    by = "categoryCombo.id"
  )

categories <- cc_coc %>%
  group_by(categoryCombo.id, categoryCombo) %>%
  summarise(
    n_categoryOptions = n(),
    Categories = paste(categoryOptionCombo, collapse = " ;\n "),
    categoryOptionCombo.ids = paste(categoryOptionCombo.id, collapse = " ;\n "),
    .groups = "drop"
  )

message("- indicators (fetching subset for demo)")
ind_raw <- get_sl(
  "api/indicators.json?fields=id,name,displayName,numerator,denominator,indicatorType[name]&paging=false&pageSize=200"
)
indicatorDictionary <- ind_raw$indicators %>% as_tibble()

message("- validation rules (may be slow)")
vr_raw <- tryCatch(
  get_sl(
    "api/validationRules.json?fields=id,name,description,leftSide,rightSide,operator&paging=false"
  ),
  error = function(e) NULL
)
validationRules <- if (!is.null(vr_raw)) {
  vr <- vr_raw$validationRules %>% as_tibble()
  # Extract raw expression strings from nested leftSide/rightSide objects so
  # dqa_consistency() can evaluate rules without fetch_validation_rules()
  # Extract raw expression strings so dqa_consistency() works without
  # fetch_validation_rules(). The API returns leftSide/rightSide as nested
  # data frames when parsed by jsonlite; $expression is a plain character column.
  if ("leftSide" %in% names(vr) && is.data.frame(vr$leftSide) &&
      "expression" %in% names(vr$leftSide)) {
    vr$leftSide_expression_raw  <- vr$leftSide$expression
    vr$rightSide_expression_raw <- vr$rightSide$expression
  }
  vr
} else {
  tibble()
}

message("- adding demo-specific malaria validation rules")
# The demo dataset has no COC disaggregation (analytics API returns totals),
# so data.id = element UID only.  Rules use bare #{uid} (no .coc suffix) so
# dqa_consistency() can evaluate them against val_<uid> wide columns.
.make_vr_side <- function(expr, desc) {
  df <- data.frame(expression = expr, description = desc, slidingWindow = FALSE,
    missingValueStrategy = "NEVER_SKIP", displayDescription = desc,
    stringsAsFactors = FALSE)
  df$translations <- list(list())
  df
}
.make_vr_row <- function(vr_template, name, desc, op, ls_expr, ls_desc,
                         rs_expr, rs_desc, id) {
  ls_df <- .make_vr_side(ls_expr, ls_desc)
  rs_df <- .make_vr_side(rs_expr, rs_desc)
  row <- vr_template[0L, ]
  row[1L, "name"]                    <- name
  row[1L, "description"]             <- desc
  row[1L, "operator"]                <- op
  row[1L, "id"]                      <- id
  row[1L, "leftSide_expression_raw"] <- ls_expr
  row[1L, "rightSide_expression_raw"]<- rs_expr
  row[1L, "leftSide"]                <- list(ls_df)
  row[1L, "rightSide"]               <- list(rs_df)
  row
}

# Convert existing leftSide/rightSide nested-df columns to list-columns so
# bind_rows() can combine them with the new rows.
validationRules$leftSide  <- lapply(seq_len(nrow(validationRules)),
                                    function(i) validationRules$leftSide[i, ])
validationRules$rightSide <- lapply(seq_len(nrow(validationRules)),
                                    function(i) validationRules$rightSide[i, ])

demo_vr_rows <- dplyr::bind_rows(
  .make_vr_row(validationRules,
    "Inpatient malaria deaths <= inpatient malaria cases",
    "Deaths from malaria cannot exceed total inpatient malaria cases",
    "less_than_or_equal_to",
    "#{wWy5TE9cQ0V}", "Inpatient malaria deaths",
    "#{p4K11MFEWtw}", "Inpatient malaria cases",
    "MG2DemoVR001"),
  .make_vr_row(validationRules,
    "ACT treatment <= RDT positive",
    "ACT treatments given should not exceed RDT-positive cases",
    "less_than_or_equal_to",
    "#{AFM5H0wNq3t}", "ACT treatment",
    "#{wZwzzRnr9N4}", "RDT positive",
    "MG2DemoVR003")
)
validationRules <- dplyr::bind_rows(validationRules, demo_vr_rows)
message("  Total validation rules: ", nrow(validationRules))

message("- resources")
res_raw <- tryCatch(
  get_sl("api/resources.json"),
  error = function(e) NULL
)
resources <- if (!is.null(res_raw) && "resources" %in% names(res_raw)) {
  res_raw$resources %>% as_tibble()
} else {
  tibble()
}

message("- geo features (district level)")
# Level 3 = district in Sierra Leone
geoFeatures <- tryCatch(
  {
    level3_ids <- orgUnits %>%
      filter(level == 3) %>%
      pull(id) %>%
      paste(collapse = ";")
    get_sl(paste0(
      "api/geoFeatures.json?ou=ou:",
      level3_ids,
      "&displayProperty=NAME"
    ))
  },
  error = function(e) NULL
)

# ---------------------------------------------------------------------------
# 2. Build dataElementDictionary (same structure as metadata_widget output)
# ---------------------------------------------------------------------------

message("\n=== Building dataElementDictionary ===")

# dataSets joined by data element
# de_id was extracted from the nested dataSetElements_dataElement column above
ds_by_de <- dataSets_dot %>%
  group_by(dataElement.id = de_id) %>%
  summarise(
    n_datasets = n(),
    dataSet.ids = paste(dataSet.id, collapse = " ;\n"),
    dataSet = paste(dataSet, collapse = " ;\n"),
    periodType = first(periodType),
    .groups = "drop"
  )

# data element groups joined by data element
deg_by_de <- dataElementGroups %>%
  group_by(dataElement.id) %>%
  summarise(
    dataElementGroup = paste(dataElementGroup, collapse = " ;\n"),
    dataElementGroups.id = paste(dataElementGroups.id, collapse = " ;\n"),
    .groups = "drop"
  )

dataElementDictionary <- dataElements_raw %>%
  rename(
    dataElement.id = id,
    dataElement = name,
    categoryCombo.id = categoryCombo.id
  ) %>%
  left_join(ds_by_de, by = "dataElement.id") %>%
  left_join(deg_by_de, by = "dataElement.id") %>%
  left_join(
    categories %>%
      select(
        categoryCombo.id,
        categoryCombo,
        n_categoryOptions,
        Categories,
        categoryOptionCombo.ids
      ),
    by = "categoryCombo.id"
  ) %>%
  select(
    dataElement,
    Categories,
    dataElementGroup,
    dataSet,
    periodType,
    zeroIsSignificant,
    shortName,
    displayShortName,
    displayName,
    dataElement.id,
    categoryCombo.id,
    categoryOptionCombo.ids,
    dataSet.ids,
    dataElementGroups.id,
    n_categoryOptions,
    categoryCombo
  ) %>%
  mutate(
    dataElement = str_trim(dataElement),
    Categories = str_trim(Categories)
  )

# meta_variables: distinct data elements + category combos
meta_variables <- dataElementDictionary %>%
  select(dataElement, dataElement.id, Categories, categoryOptionCombo.ids) %>%
  distinct()

# ---------------------------------------------------------------------------
# 3. Build mg2_demo_formula
# ---------------------------------------------------------------------------

message("\n=== Building mg2_demo_formula ===")

mg2_demo_formula <- dataElementDictionary %>%
  filter(dataElement.id %in% DE_IDS) %>%
  separate_rows(Categories, categoryOptionCombo.ids, sep = " ;\n ") %>%
  mutate(
    Categories = str_trim(Categories),
    categoryOptionCombo.ids = str_trim(categoryOptionCombo.ids),
    Formula.Name = FORMULA_NAME
  ) %>%
  select(Formula.Name, everything()) %>%
  arrange(dataElement) %>%
  distinct()

# ---------------------------------------------------------------------------
# 4. Download real data (most recent 12 months at LEVEL-4 = facility)
# ---------------------------------------------------------------------------

message("\n=== Downloading real data (12 months) ===")

# Use Jan–Dec 2025.
# Adjust END_YEAR if the instance has been refreshed.
END_YEAR <- 2025L
real_periods <- sprintf("%04d%02d", END_YEAR, 1:12)
real_periods_str <- paste(real_periods, collapse = ";")

# Determine facility level (maximum org unit level = leaves)
FACILITY_LEVEL <- max(orgUnitLevels$level)
message("  Facility level: LEVEL-", FACILITY_LEVEL)

message("  Periods: ", paste(real_periods, collapse = ", "))

# Fetch SUM and COUNT for each data element × period
# Use analytics endpoint (aggregated) for all facilities at once
fetch_one_de <- function(de_id, periods_str) {
  message("  - fetching ", de_id)

  # SUM
  sum_result <- dhis2_get(
    paste0(
      BASEURL,
      "api/analytics/dataValueSet.json?",
      "dimension=dx:",
      de_id,
      "&dimension=pe:",
      periods_str,
      "&dimension=ou:LEVEL-",
      FACILITY_LEVEL,
      "",
      "&displayProperty=NAME&aggregationType=SUM"
    ),
    username = USERNAME,
    password = PASSWORD
  )

  # COUNT
  count_result <- dhis2_get(
    paste0(
      BASEURL,
      "api/analytics/dataValueSet.json?",
      "dimension=dx:",
      de_id,
      "&dimension=pe:",
      periods_str,
      "&dimension=ou:LEVEL-",
      FACILITY_LEVEL,
      "",
      "&displayProperty=NAME&aggregationType=COUNT"
    ),
    username = USERNAME,
    password = PASSWORD
  )

  sum_df <- if (
    !is.null(sum_result) &&
      is.list(sum_result) &&
      !is.null(sum_result$dataValues)
  ) {
    as_tibble(sum_result$dataValues)
  } else {
    tibble()
  }
  count_df <- if (
    !is.null(count_result) &&
      is.list(count_result) &&
      !is.null(count_result$dataValues)
  ) {
    as_tibble(count_result$dataValues)
  } else {
    tibble()
  }

  if (nrow(sum_df) == 0) {
    return(tibble())
  }

  by_cols <- intersect(
    c("dataElement", "period", "orgUnit", "categoryOptionCombo"),
    names(sum_df)
  )

  sum_df %>%
    rename(SUM = value) %>%
    mutate(SUM = as.numeric(SUM)) %>%
    left_join(
      count_df %>%
        rename(COUNT = value) %>%
        mutate(COUNT = as.integer(COUNT)),
      by = by_cols
    )
}

real_data_list <- map(DE_IDS, fetch_one_de, periods_str = real_periods_str)
real_data_raw <- bind_rows(real_data_list)

if (nrow(real_data_raw) == 0 || !"dataElement" %in% names(real_data_raw)) {
  stop(
    "No data returned for any data element. ",
    "Check that END_YEAR (",
    END_YEAR,
    ") has analytics data on this instance ",
    "and that LEVEL-",
    FACILITY_LEVEL,
    " org units have data."
  )
}

# Report which elements returned data
returned_des <- unique(real_data_raw$dataElement)
missing_des <- setdiff(DE_IDS, returned_des)
if (length(missing_des) > 0) {
  message(
    "  WARNING: no data for ",
    length(missing_des),
    " element(s): ",
    paste(missing_des, collapse = ", ")
  )
}

real_data <- real_data_raw %>%
  select(
    dataElement,
    period,
    orgUnit,
    any_of("categoryOptionCombo"),
    COUNT,
    SUM
  ) %>%
  filter(!is.na(SUM))

message(
  "  Real data: ",
  nrow(real_data),
  " rows, ",
  n_distinct(real_data$orgUnit),
  " facilities, ",
  n_distinct(real_data$period),
  " periods"
)

# ---------------------------------------------------------------------------
# 4b. Save raw real data (12 months) before any modification
# ---------------------------------------------------------------------------

mg2_demo_raw <- real_data

# ---------------------------------------------------------------------------
# 4c. Apply 2025 malaria-reduction pattern
#
# In the demo's final year (2025), a programme intervention reduces malaria:
#   - Inpatient cases, deaths, RDT positive, ACT treated: down ~15%
#   - RDT negative: up ~15%
# The reduction is seasonally amplified — months that are normally the
# highest (rainy-season peak) show a proportionally larger change, making
# the effect visible in both level and seasonal charts.
# ---------------------------------------------------------------------------

message("  Applying 2025 malaria-reduction pattern to real_data")

MALARIA_DOWN_ELEMENTS <- c("p4K11MFEWtw", "wWy5TE9cQ0V", "wZwzzRnr9N4", "AFM5H0wNq3t")
MALARIA_UP_ELEMENT    <- "Qk9nnX0i7lZ"
REDUCTION_AMPLITUDE   <- 0.05   # extra ±reduction per unit of seasonal_index above 1

# Seasonal index: normalised mean SUM per month (mean over 12 months = 1.0)
seasonal_idx_2025 <- real_data %>%
  filter(dataElement %in% c(MALARIA_DOWN_ELEMENTS, MALARIA_UP_ELEMENT)) %>%
  mutate(month = as.integer(substr(period, 5, 6))) %>%
  group_by(dataElement, month) %>%
  summarise(mean_sum = mean(SUM, na.rm = TRUE), .groups = "drop") %>%
  group_by(dataElement) %>%
  mutate(
    grand_mean     = mean(mean_sum, na.rm = TRUE),
    seasonal_index = ifelse(grand_mean > 0, mean_sum / grand_mean, 1)
  ) %>%
  ungroup() %>%
  select(dataElement, month, seasonal_index)

real_data <- real_data %>%
  mutate(month = as.integer(substr(period, 5, 6))) %>%
  left_join(seasonal_idx_2025, by = c("dataElement", "month")) %>%
  mutate(
    seasonal_index = replace_na(seasonal_index, 1),
    adjust_factor  = case_when(
      dataElement %in% MALARIA_DOWN_ELEMENTS ~
        pmax(0.50, 1 - (0.15 + REDUCTION_AMPLITUDE * pmax(0, seasonal_index - 1))),
      dataElement == MALARIA_UP_ELEMENT ~
        1 + (0.15 + REDUCTION_AMPLITUDE * pmax(0, seasonal_index - 1)),
      TRUE ~ 1.0
    ),
    SUM = round(SUM * adjust_factor)
  ) %>%
  select(-month, -seasonal_index, -adjust_factor)

# ---------------------------------------------------------------------------
# 5. Seasonal bootstrap: generate 48 months of prior history  (Option D)
#
# Four features added over the original simple tile:
#   1. Higher per-facility noise  (sdlog 0.35 vs 0.12) — more realistic spread
#   2. Element-specific year trends — visible national-level change over time
#        AFM5H0wNq3t (ACT treatment) : step-up from 2023 (scale-up program)
#        Qk9nnX0i7lZ / wZwzzRnr9N4  : gradual testing/positivity growth
#        p4K11MFEWtw / wWy5TE9cQ0V  : declining inpatient cases/deaths
#   3. Reporting gaps for multi-facility elements — earlier years have more
#      missing facility-months (programme roll-out improving over time)
#   4. One outlier year — wZwzzRnr9N4 (RDT positive) Jun–Aug 2022:
#      ~5 % of facilities report 3–4× their expected value, leaving a
#      detectable cluster of outliers for the Outliers tab to catch
# ---------------------------------------------------------------------------

message("\n=== Bootstrapping prior 60 months (Option D) ===")

period_to_ym <- function(p) {
  as.integer(substr(p, 1, 4)) * 12 + as.integer(substr(p, 5, 6)) - 1
}

ym_to_period <- function(ym) {
  yr <- ym %/% 12
  mo <- ym %% 12 + 1
  sprintf("%04d%02d", yr, mo)
}

set.seed(20260404)

# --- Option D parameters ---------------------------------------------------

NOISE_SD <- 0.35   # facility-level lognormal noise (was 0.12)

# Year-trend multipliers: how large is each prior year relative to modified 2025?
# Multipliers < 1 → the programme grew to reach 2025 level
# Multipliers > 1 → the outcome declined to reach 2025 level (e.g. deaths)
# 2025 already has the malaria-reduction baked in; prior years reflect
# higher burden (cases/deaths) or lower coverage (ACT, testing).
YEAR_TRENDS <- list(
  AFM5H0wNq3t = c(`2024` = 0.88, `2023` = 0.72, `2022` = 0.60, `2021` = 0.55, `2020` = 0.46),
  Qk9nnX0i7lZ = c(`2024` = 0.93, `2023` = 0.87, `2022` = 0.80, `2021` = 0.75, `2020` = 0.68),
  p4K11MFEWtw  = c(`2024` = 1.04, `2023` = 1.08, `2022` = 1.15, `2021` = 1.20, `2020` = 1.26),
  wWy5TE9cQ0V  = c(`2024` = 1.08, `2023` = 1.15, `2022` = 1.25, `2021` = 1.35, `2020` = 1.45),
  wZwzzRnr9N4  = c(`2024` = 0.95, `2023` = 0.90, `2022` = 0.84, `2021` = 0.78, `2020` = 0.72)
)

# Multi-facility elements: reporting gaps applied (single-facility inpatient
# elements p4K11MFEWtw and wWy5TE9cQ0V are excluded — dropping their one
# facility would delete the series entirely)
MULTI_FAC_ELEMENTS <- c("AFM5H0wNq3t", "Qk9nnX0i7lZ", "wZwzzRnr9N4")

# Per-facility reporting trajectories (replaces flat annual probability).
# Champions (all 12 months in 2025) reported well historically: start prob
# 65–88% in 2020 (earliest year). Non-champions start at 12–50% of their
# 2025 rate and improve linearly each year, reflecting a maturing system.
fac_completeness_2025 <- real_data %>%
  dplyr::filter(dataElement %in% MULTI_FAC_ELEMENTS) %>%
  dplyr::group_by(orgUnit) %>%
  dplyr::summarise(months_2025 = dplyr::n_distinct(period), .groups = "drop") %>%
  dplyr::mutate(
    rate_2025   = months_2025 / 12,
    is_champion = months_2025 == 12,
    start_prob  = ifelse(
      is_champion,
      runif(dplyr::n(), 0.65, 0.88),
      pmax(0.08, rate_2025 * runif(dplyr::n(), 0.12, 0.50))
    )
  )

fac_year_probs <- fac_completeness_2025 %>%
  tidyr::crossing(tile_year = 2020L:2024L) %>%
  dplyr::mutate(
    alpha = (tile_year - 2020L) / 5L,
    prob  = pmin(1, start_prob + alpha * (rate_2025 - start_prob))
  ) %>%
  dplyr::select(orgUnit, tile_year, prob)

fac_year_probs_by_ou <- split(fac_year_probs, fac_year_probs$orgUnit)

# Outlier cluster: ~5 % of facilities report 3–4× expected value
OUTLIER_ELEMENT <- "wZwzzRnr9N4"
OUTLIER_YEAR    <- 2022L
OUTLIER_MONTHS  <- 6:8
OUTLIER_FRAC    <- 0.05
OUTLIER_FACTOR  <- c(3, 4)

# ---------------------------------------------------------------------------

bootstrap_series <- function(df, key) {
  df <- df %>% arrange(period)
  n  <- nrow(df)
  if (n == 0) return(tibble())

  de_id        <- key$dataElement[1]
  is_multi_fac <- de_id %in% MULTI_FAC_ELEMENTS
  trends       <- YEAR_TRENDS[[de_id]]
  min_ym       <- period_to_ym(min(df$period))

  prior_rows <- vector("list", 5)
  for (tile in 5:1) {
    tile_year  <- 2025L - as.integer(tile)   # tile 5→2020, 4→2021, 3→2022, 2→2023, 1→2024
    noise      <- rlnorm(n, meanlog = 0, sdlog = NOISE_SD)
    multiplier <- trends[as.character(tile_year)]

    prior_df <- df %>%
      mutate(
        ym     = period_to_ym(period),
        offset = min_ym - (tile * 12L) + (ym - min(ym)),
        period = ym_to_period(offset),
        SUM    = round(SUM * noise * multiplier),
        COUNT  = COUNT
      ) %>%
      select(-ym, -offset)

    # Reporting gaps — facility-specific probability from trajectory table
    if (is_multi_fac) {
      fac_tbl <- fac_year_probs_by_ou[[key$orgUnit[1]]]
      prob    <- if (!is.null(fac_tbl))
        fac_tbl$prob[fac_tbl$tile_year == tile_year]
      else 0.80
      if (length(prob) == 0) prob <- 0.80
      keep <- as.logical(rbinom(nrow(prior_df), 1, prob))
      prior_df <- prior_df[keep, , drop = FALSE]
    }

    # Outlier spike
    if (de_id == OUTLIER_ELEMENT && tile_year == OUTLIER_YEAR) {
      spike_months <- as.integer(substr(prior_df$period, 5, 6)) %in% OUTLIER_MONTHS
      spike_facs   <- runif(nrow(prior_df)) < OUTLIER_FRAC
      spike_idx    <- which(spike_months & spike_facs)
      if (length(spike_idx) > 0) {
        mult <- runif(length(spike_idx),
                      min = OUTLIER_FACTOR[1], max = OUTLIER_FACTOR[2])
        prior_df$SUM[spike_idx] <- round(prior_df$SUM[spike_idx] * mult)
      }
    }

    prior_rows[[tile]] <- prior_df
  }

  bind_rows(prior_rows)
}

prior_data <- real_data %>%
  group_by(across(-c(period, SUM, COUNT))) %>%
  group_modify(bootstrap_series) %>%
  ungroup()

message("  Prior data: ", nrow(prior_data), " rows")

# Combine and clean
mg2_demo <- bind_rows(prior_data, real_data) %>%
  arrange(dataElement, orgUnit, period) %>%
  mutate(
    SUM = pmax(SUM, 0, na.rm = TRUE), # no negative values
    COUNT = as.integer(COUNT)
  ) %>%
  distinct()

message(
  "  Total mg2_demo: ",
  nrow(mg2_demo),
  " rows, ",
  n_distinct(mg2_demo$period),
  " periods (",
  min(mg2_demo$period),
  "-",
  max(mg2_demo$period),
  ")"
)

# ---------------------------------------------------------------------------
# 6. Assemble mg2_demo_meta
# ---------------------------------------------------------------------------

message("\n=== Assembling mg2_demo_meta ===")

mg2_demo_meta <- list(
  systemInfo = systemInfo,
  meta_variables = meta_variables,
  orgUnitLevels = orgUnitLevels,
  orgUnits = orgUnits,
  dataElementDictionary = dataElementDictionary,
  indicatorDictionary = indicatorDictionary,
  dataSets. = dataSets_dot,
  dataSets = dataSets_raw,
  categories = categories,
  dataElementGroups = dataElementGroups,
  ousTree = ousTree,
  geoFeatures = geoFeatures,
  validationRules = validationRules,
  resources = resources
)

# ---------------------------------------------------------------------------
# 7. Save to data/
# ---------------------------------------------------------------------------

message("\n=== Saving to data/ ===")

usethis::use_data(mg2_demo_raw, overwrite = TRUE)
usethis::use_data(mg2_demo, overwrite = TRUE)
usethis::use_data(mg2_demo_formula, overwrite = TRUE)
usethis::use_data(mg2_demo_meta, overwrite = TRUE)

# ---------------------------------------------------------------------------
# 8. Pre-process through data_1() and save as mg2_demo_processed
# ---------------------------------------------------------------------------

message("\n=== Running data_1() to build mg2_demo_processed ===")

mg2_demo_processed <- data_1(
  data             = mg2_demo,
  dataSets         = mg2_demo_meta[["dataSets."]],
  formula_elements = mg2_demo_formula,
  dataElements     = mg2_demo_meta$dataElementDictionary,
  categories       = mg2_demo_meta$categories,
  ousTree          = mg2_demo_meta$ousTree
)

usethis::use_data(mg2_demo_processed, overwrite = TRUE)

message("\nDone. Objects saved:")
message("  data/mg2_demo.rda           — ", nrow(mg2_demo), " rows")
message(
  "  data/mg2_demo_formula.rda   — ",
  nrow(mg2_demo_formula),
  " rows, ",
  n_distinct(mg2_demo_formula$dataElement),
  " data elements"
)
message(
  "  data/mg2_demo_meta.rda      — ",
  length(mg2_demo_meta),
  " metadata components"
)
message("  data/mg2_demo_processed.rda — ", nrow(mg2_demo_processed), " rows (tsibble)")
