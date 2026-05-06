# data-raw/rebuild_demo_bootstrap.R
#
# Re-runs ONLY the bootstrap and data_1() steps using the existing saved objects.
# Use this when:
#   - The demo instance is offline / unavailable (avoids a full re-download), OR
#   - You only changed bootstrap parameters (YEAR_TRENDS, noise, 2025 reduction, etc.)
#
# Requires:
#   data/mg2_demo_raw.rda       — real 12-month data (source for bootstrap)
#   data/mg2_demo_meta.rda      — metadata (for data_1 and ousTree)
#   data/mg2_demo_formula.rda   — formula elements
#
# Outputs (overwrites):
#   data/mg2_demo.rda
#   data/mg2_demo_processed.rda
#
# Usage:
#   source("data-raw/rebuild_demo_bootstrap.R")

devtools::load_all()
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# ---------------------------------------------------------------------------
# 0. Load existing saved objects
# ---------------------------------------------------------------------------

message("Loading existing saved objects...")
load("data/mg2_demo_raw.rda")          # → mg2_demo_raw
load("data/mg2_demo_meta.rda")         # → mg2_demo_meta
load("data/mg2_demo_formula.rda")      # → mg2_demo_formula
message("  mg2_demo_raw: ", nrow(mg2_demo_raw), " rows, ", n_distinct(mg2_demo_raw$orgUnit), " facilities")

# ---------------------------------------------------------------------------
# 1. Apply 2025 malaria-reduction pattern to real_data
#
# In 2025, a programme intervention reduces malaria:
#   - Inpatient cases, deaths, RDT positive, ACT treated: down ~15%
#   - RDT negative: up ~15%
# The effect is seasonally amplified — peak-season months (Jul-Oct) show
# proportionally larger changes.
# mg2_demo_raw is preserved as the unmodified original.
# ---------------------------------------------------------------------------

message("\nApplying 2025 malaria-reduction pattern...")

MALARIA_DOWN_ELEMENTS <- c("p4K11MFEWtw", "wWy5TE9cQ0V", "wZwzzRnr9N4", "AFM5H0wNq3t")
MALARIA_UP_ELEMENT    <- "Qk9nnX0i7lZ"
REDUCTION_AMPLITUDE   <- 0.05   # extra ±reduction per unit of seasonal_index above 1

# Seasonal index from the raw real data (before modification)
seasonal_idx_2025 <- mg2_demo_raw %>%
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

real_data <- mg2_demo_raw %>%
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

# Quick check: national monthly totals before/after for inpatient cases
check <- dplyr::bind_rows(
  mg2_demo_raw %>% filter(dataElement == "p4K11MFEWtw") %>%
    mutate(month = substr(period, 5, 6)) %>%
    group_by(month) %>% summarise(sum = sum(SUM, na.rm=TRUE), .groups="drop") %>%
    mutate(version = "raw"),
  real_data %>% filter(dataElement == "p4K11MFEWtw") %>%
    mutate(month = substr(period, 5, 6)) %>%
    group_by(month) %>% summarise(sum = sum(SUM, na.rm=TRUE), .groups="drop") %>%
    mutate(version = "modified")
)
message("  Inpatient cases before/after by month:")
print(tidyr::pivot_wider(check, names_from = version, values_from = sum) %>%
        mutate(pct_change = round(100 * (modified / raw - 1), 1)))

# ---------------------------------------------------------------------------
# 2. Seasonal bootstrap: 60 months of prior history (2020-2024)
# ---------------------------------------------------------------------------

message("\n=== Bootstrapping prior 60 months ===")

period_to_ym <- function(p) {
  as.integer(substr(p, 1, 4)) * 12 + as.integer(substr(p, 5, 6)) - 1
}

ym_to_period <- function(ym) {
  yr <- ym %/% 12
  mo <- ym %% 12 + 1
  sprintf("%04d%02d", yr, mo)
}

set.seed(20260404)

NOISE_SD <- 0.35

# Year-trend multipliers relative to modified 2025
YEAR_TRENDS <- list(
  AFM5H0wNq3t = c(`2024` = 0.88, `2023` = 0.72, `2022` = 0.60, `2021` = 0.55, `2020` = 0.46),
  Qk9nnX0i7lZ = c(`2024` = 0.93, `2023` = 0.87, `2022` = 0.80, `2021` = 0.75, `2020` = 0.68),
  p4K11MFEWtw  = c(`2024` = 1.04, `2023` = 1.08, `2022` = 1.15, `2021` = 1.20, `2020` = 1.26),
  wWy5TE9cQ0V  = c(`2024` = 1.08, `2023` = 1.15, `2022` = 1.25, `2021` = 1.35, `2020` = 1.45),
  wZwzzRnr9N4  = c(`2024` = 0.95, `2023` = 0.90, `2022` = 0.84, `2021` = 0.78, `2020` = 0.72)
)

MULTI_FAC_ELEMENTS <- c("AFM5H0wNq3t", "Qk9nnX0i7lZ", "wZwzzRnr9N4")

# Per-facility reporting trajectories: linear from start_prob (2020) to rate_2025
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

# Outlier cluster: ~5% of facilities report 3-4x expected in Jun-Aug 2022
OUTLIER_ELEMENT <- "wZwzzRnr9N4"
OUTLIER_YEAR    <- 2022L
OUTLIER_MONTHS  <- 6:8
OUTLIER_FRAC    <- 0.05
OUTLIER_FACTOR  <- c(3, 4)

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
    tile_year  <- 2025L - as.integer(tile)   # 5→2020, 4→2021, 3→2022, 2→2023, 1→2024
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

    # Reporting gaps — facility-specific probability
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

message("  Prior data: ", nrow(prior_data), " rows, ",
        n_distinct(prior_data$period), " periods (",
        min(prior_data$period), "-", max(prior_data$period), ")")

# Combine
mg2_demo <- bind_rows(prior_data, real_data) %>%
  arrange(dataElement, orgUnit, period) %>%
  mutate(
    SUM   = pmax(SUM, 0, na.rm = TRUE),
    COUNT = as.integer(COUNT)
  ) %>%
  distinct()

message("  Total mg2_demo: ", nrow(mg2_demo), " rows, ",
        n_distinct(mg2_demo$period), " periods (",
        min(mg2_demo$period), "-", max(mg2_demo$period), ")")

# Completeness check
comp_by_year <- mg2_demo %>%
  filter(dataElement == "wZwzzRnr9N4") %>%
  mutate(year = substr(period, 1, 4)) %>%
  group_by(year) %>%
  summarise(facility_months = n(), .groups = "drop")
message("  RDT positive facility-months by year:")
print(comp_by_year)

# ---------------------------------------------------------------------------
# 3. Save mg2_demo
# ---------------------------------------------------------------------------

message("\nSaving mg2_demo...")
usethis::use_data(mg2_demo, overwrite = TRUE)

# ---------------------------------------------------------------------------
# 4. Build mg2_demo_processed
# ---------------------------------------------------------------------------

message("\nRunning data_1()...")
mg2_demo_processed <- data_1(
  data             = mg2_demo,
  dataSets         = mg2_demo_meta[["dataSets."]],
  formula_elements = mg2_demo_formula,
  dataElements     = mg2_demo_meta$dataElementDictionary,
  categories       = mg2_demo_meta$categories,
  ousTree          = mg2_demo_meta$ousTree
)

message("  mg2_demo_processed: ", nrow(mg2_demo_processed), " rows")
usethis::use_data(mg2_demo_processed, overwrite = TRUE)

message("\nDone.")
message("  data/mg2_demo.rda           — ", nrow(mg2_demo), " rows, ",
        n_distinct(mg2_demo$period), " periods")
message("  data/mg2_demo_processed.rda — ", nrow(mg2_demo_processed), " rows (tsibble)")
