# 📌 Load necessary libraries
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

# 📌 Set Census API Key (Replace with your own key)
census_api_key("63217a192c5803bfc72aab537fe4bf19f6058326", overwrite = TRUE)

# 📌 Define ACS variables to retrieve
acs_vars <- c(
  "B19013_001",  # Median Household Income
  "B25070_001",  # Households paying 30%+ income on rent
  "B25003_002",  # Owner-occupied housing
  "B25003_003",  # Renter-occupied housing
  "B17001_002"   # Individuals below poverty line
)

# 📌 Get ACS data for all counties in California (WITHOUT geometry)
housing_data <- get_acs(
  geography = "county",
  variables = acs_vars,
  state = "CA",
  year = 2023,
  survey = "acs5"
)

# 📌 Retrieve California County Shapefiles using `tigris`
ca_counties <- counties(state = "CA", class = "sf")  # Get geographic shapes

# 📌 Reshape ACS data: Pivot variables into columns
housing_cleaned <- housing_data %>%
  select(NAME, variable, estimate) %>%  # Keep relevant columns
  pivot_wider(names_from = variable, values_from = estimate)

# 📌 Check column names before renaming
print(colnames(housing_cleaned))

# 📌 Rename columns dynamically
housing_cleaned <- housing_cleaned %>%
  rename_with(~ case_when(
    . == "B19013_001" ~ "median_income",
    . == "B25070_001" ~ "rent_burden",
    . == "B25003_002" ~ "homeowners",
    . == "B25003_003" ~ "renters",
    . == "B17001_002" ~ "poverty",
    TRUE ~ .
  )) %>%
  mutate(
    homeownership_rate = homeowners / (homeowners + renters) * 100,
    rent_burden_rate = rent_burden / (homeowners + renters) * 100
  )

housing_cleaned <- housing_cleaned %>%
  mutate(NAME = str_remove(NAME, " County, California$"))

# 📌 Merge ACS data with county geometry using a Left Join
housing_map_data <- left_join(ca_counties, housing_cleaned, by = c("NAME" = "NAME"))

# 📌 Check if median_income and geometry exist before plotting
glimpse(housing_map_data)

# 📌 Plot the California Map - Median Income by County
ggplot(housing_map_data) +
  geom_sf(aes(fill = median_income), color = "white", lwd = 0.2) +
  scale_fill_viridis_c(option = "magma", name = "Median Income ($)") +
  labs(title = "Median Household Income by County in California (2023)",
       subtitle = "Source: American Community Survey 2023 (5-Year Estimates)") +
  theme_minimal()
