# Use fakir to generate synthetic data for malaria data

library(dplyr)
library(tidyr)
library(fakir) # Loaded to inject missing data


# 1. Define foundational time-series framework
historical_years <- 2023:2025
months <- 1:12
key_var1 <- c("North", "South", "East", "West")
key_var2 <- c("Widget_A", "Widget_B")

# Create the skeleton grid
preceding_data <- expand_grid(
  year = historical_years,
  month = months,
  region = key_var1,
  product = key_var2
)

# 2. Define your seasonal and trend multipliers
# Seasonality: Index values relative to 1.0 (e.g., Dec = +30%, Jan = -20%)
seasonal_weights <- tibble(
  month = 1:12,
  month_multiplier = c(
    0.8,
    0.85,
    1.0,
    1.05,
    1.1,
    1.15,
    1.0,
    0.95,
    1.1,
    1.15,
    1.2,
    1.3
  )
)

# Trend: Index values for yearly growth (e.g., 2023 was smaller than 2025)
yearly_trends <- tibble(
  year = c(2023, 2024, 2025),
  year_multiplier = c(0.85, 0.92, 1.0) # 2023 starts at 85% of baseline strength
)

# 3. Build metrics with compounding patterns
set.seed(42)
total_rows <- nrow(preceding_data)

historical_dataset <- preceding_data %>%
  # Join multipliers onto the data frame grid
  left_join(seasonal_weights, by = "month") %>%
  left_join(yearly_trends, by = "year") %>%
  mutate(
    # Create an initial randomized base number
    base_sales = rnorm(total_rows, mean = 5000, sd = 400),

    # Compound formula: Base * Seasonality * Trend
    sales = round(base_sales * month_multiplier * year_multiplier, 2)
  ) %>%
  # Drop calculations to clean up schema
  select(-base_sales, -month_multiplier, -year_multiplier)

head(historical_dataset)


# Inject missing data using fakir
historical_with_nas <- historical_dataset %>%
  mutate(
    # Replace 10% of sales numbers with NA
    sales = miss_fill(sales, p = 0.10),

    # You can also add missing data to your key variables if needed
    region = miss_fill(region, p = 0.05)
  )

# Verify the missing values exist
sum(is.na(historical_with_nas$sales))
