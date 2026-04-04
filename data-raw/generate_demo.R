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
#
# Requires live internet access. Run once; outputs are committed to data/.
# Re-run when the demo instance is refreshed or data elements change.
#
# Usage:
#   source("data-raw/generate_demo.R")

library(MG2) # for dhis2_get(), ous_tree(), date_code()
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

# 6 malaria data elements
DE_IDS <- c(
  "TV5zBFFkZLu", # Malaria cases confirmed
  "p4K11MFEWtw", # Inpatient malaria cases
  "wWy5TE9cQ0V", # Inpatient malaria deaths
  "wZwzzRnr9N4", # RDT positive
  "Qk9nnX0i7lZ", # RDT negative / tested
  "AFM5H0wNq3t" # Malaria treated at PHU with ACT < 24 hrs new
)

DE_NAMES <- c(
  "Malaria cases confirmed",
  "Inpatient malaria cases",
  "Inpatient malaria deaths",
  "RDT positive",
  "RDT negative / tested",
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
  "api/organisationUnitLevels.json?fields=id,name,level&paging=false&order=level:asc"
)
orgUnitLevels <- tibble(
  id = ouLevels_raw$organisationUnitLevels$id,
  level = as.integer(ouLevels_raw$organisationUnitLevels$level),
  levelName = ouLevels_raw$organisationUnitLevels$name
)

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

message("- building org unit tree")
ousTree <- ous_tree(orgUnits, orgUnitLevels, .verbose = TRUE)

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
    dataSet    = name
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
  vr_raw$validationRules %>% as_tibble()
} else {
  tibble()
}

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

real_periods_str <- date_code(YrsPrevious = 1)
real_periods <- strsplit(real_periods_str, ";", fixed = TRUE)[[1]]

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
      "&dimension=ou:LEVEL-4",
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
      "&dimension=ou:LEVEL-4",
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
real_data <- bind_rows(real_data_list) %>%
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
# 5. Seasonal bootstrap: generate 48 months of prior history
# ---------------------------------------------------------------------------

message("\n=== Bootstrapping prior 48 months ===")

# Period helpers
period_to_ym <- function(p) {
  as.integer(substr(p, 1, 4)) * 12 + as.integer(substr(p, 5, 6)) - 1
}

ym_to_period <- function(ym) {
  yr <- ym %/% 12
  mo <- ym %% 12 + 1
  sprintf("%04d%02d", yr, mo)
}

set.seed(20260404)

# For each facility × data element × categoryOptionCombo, tile the 12 real
# months backward 4 times with lognormal multiplicative noise (SD ≈ 15%).
bootstrap_series <- function(df) {
  df <- df %>% arrange(period)
  n <- nrow(df)
  if (n == 0) {
    return(tibble())
  }

  min_ym <- period_to_ym(min(df$period))

  # Key columns (everything except period / SUM / COUNT)
  key_cols <- setdiff(names(df), c("period", "SUM", "COUNT"))

  prior_rows <- vector("list", 4)
  for (tile in 4:1) {
    noise <- rlnorm(n, meanlog = 0, sdlog = 0.12)
    prior_df <- df %>%
      mutate(
        ym = period_to_ym(period),
        offset = min_ym - (tile * 12) + (ym - min(ym)),
        period = ym_to_period(offset),
        SUM = round(SUM * noise),
        COUNT = COUNT # keep count the same (same facilities)
      ) %>%
      select(-ym, -offset)
    prior_rows[[tile]] <- prior_df
  }

  bind_rows(prior_rows)
}

prior_data <- real_data %>%
  group_by(across(-c(period, SUM, COUNT))) %>%
  group_modify(~ bootstrap_series(.x)) %>%
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

usethis::use_data(mg2_demo, overwrite = TRUE)
usethis::use_data(mg2_demo_formula, overwrite = TRUE)
usethis::use_data(mg2_demo_meta, overwrite = TRUE)

message("\nDone. Objects saved:")
message("  data/mg2_demo.rda         — ", nrow(mg2_demo), " rows")
message(
  "  data/mg2_demo_formula.rda — ",
  nrow(mg2_demo_formula),
  " rows, ",
  n_distinct(mg2_demo_formula$dataElement),
  " data elements"
)
message(
  "  data/mg2_demo_meta.rda    — ",
  length(mg2_demo_meta),
  " metadata components"
)
