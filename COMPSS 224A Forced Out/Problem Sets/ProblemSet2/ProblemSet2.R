
rm(list = ls())

# AI Usage
# https://www.perplexity.ai/search/acs-data-cleaned-acs-data-muta-ybTnQDoRSCy_diNjwbYPmA

# Set Census API Key (Replace with your own key)
census_api_key("63217a192c5803bfc72aab537fe4bf19f6058326", overwrite = TRUE)

library(tidyverse)
library(tidycensus)
library(tigris)
library(ggplot2)
library(sf)

### IMPORT ACS

acs_vars <- c(
  "B25003_003",   # Total renters
  "B25003_002",   # Total owners
  # Groups
  "B02001_002",   # White alone
  "B02001_003",   # Black alone
  "B02001_005",   # Asian alone
  "B03002_012",   # Hispanic or Latino
  "B01001_020",   # Population 65+
  "B01001_003",   # Population under 18
  "B25003A_003",  # White renters
  "B25003B_003",  # Black renters
  "B25003D_003",  # Asian renters
  "B25003I_003",  # Hispanic renters
  "B25007_012",   # Senior renters
  "B25008_003",   # Children renters
  # Displacement Risk Factor
  "B06007_005",   # Limited English Proficiency
  "B19013_001",   # Median Household Income
  "B17001_002",   # Poverty Population
  "B25070_001"    # Housing Cost Burden
)

acs_data <- get_acs(
  geography = "county",
  state = "WI",
  variables = acs_vars,
  year = 2022,
  survey = "acs5"
)

acs_data <- acs_data %>%
  mutate(NAME = str_remove(NAME, " County, Wisconsin")) %>%
  select(NAME, variable, estimate) %>%pivot_wider(names_from = variable, values_from = estimate) %>%
  rename(
    county = NAME,
    total_renters = B25003_003,
    total_owners = B25003_002,
    white_population = B02001_002,
    black_population = B02001_003,
    asian_population = B02001_005,
    hispanic_population = B03002_012,
    seniors_population = B01001_020,
    children_population = B01001_003,
    white_renters = B25003A_003,
    black_renters = B25003B_003,
    asian_renters = B25003D_003,
    hispanic_renters = B25003I_003,
    seniors_renters = B25007_012,
    children_renters = B25008_003,
    limited_english = B06007_005,
    median_income = B19013_001,
    poverty_population = B17001_002,
    rent_burden = B25070_001
  )

acs_data_cleaned <- acs_data %>%
  mutate(
    # Racial minority % (non-white pop)
    pct_nonwhite = 1 - (white_population / total_owners + total_renters),
    # Rent burden rate
    rent_burden_rate = rent_burden / (total_owners + total_renters),
    # Poverty rate
    poverty_rate = poverty_population / total_owners + total_renters,
    # Renter rate
    renter_rate = total_renters / (total_owners + total_renters),
    # Normalize each indicator to 0-1 range
    pct_nonwhite_norm = (pct_nonwhite - min(pct_nonwhite, na.rm = TRUE)) / 
      (max(pct_nonwhite, na.rm = TRUE) - min(pct_nonwhite, na.rm = TRUE)),
    rent_burden_norm = (rent_burden_rate - min(rent_burden_rate, na.rm = TRUE)) / 
      (max(rent_burden_rate, na.rm = TRUE) - min(rent_burden_rate, na.rm = TRUE)),
    poverty_norm = (poverty_rate - min(poverty_rate, na.rm = TRUE)) / 
      (max(poverty_rate, na.rm = TRUE) - min(poverty_rate, na.rm = TRUE)),
    renter_norm = (renter_rate - min(renter_rate, na.rm = TRUE)) / 
      (max(renter_rate, na.rm = TRUE) - min(renter_rate, na.rm = TRUE)),
    limited_english_norm = (limited_english - min(limited_english, na.rm = TRUE)) / 
      (max(limited_english, na.rm = TRUE) - min(limited_english, na.rm = TRUE)),
    median_income_norm = 1 - ((median_income - min(median_income, na.rm = TRUE)) / 
                                (max(median_income, na.rm = TRUE) - min(median_income, na.rm = TRUE)))
  )

weights <- c(
  pct_nonwhite = 0.20,
  rent_burden = 0.20,
  poverty = 0.20,
  renter_rate = 0.20,
  limited_english = 0.10,
  median_income = 0.10
)

acs_data_cleaned <- acs_data_cleaned %>%
  mutate(
    displacement_risk_factor = (
      pct_nonwhite_norm * weights["pct_nonwhite"] +
        rent_burden_norm * weights["rent_burden"] +
        poverty_norm * weights["poverty"] +
        renter_norm * weights["renter_rate"] +
        limited_english_norm * weights["limited_english"] +
        median_income_norm * weights["median_income"]
    )
  )

acs_data_cleaned <- acs_data_cleaned %>%
  mutate(
    displacement_risk_normalized = (displacement_risk_factor - min(displacement_risk_factor, na.rm = TRUE)) / 
      (max(displacement_risk_factor, na.rm = TRUE) - min(displacement_risk_factor, na.rm = TRUE))
  )

print(acs_data_cleaned)

### IMPORT EVICTION DATA

eviction_data <- read_csv("wisconsin_monthly_2020_2021.csv")

wi_county_fips <- tribble(
  ~county_fips, ~county,
  "55001", "Adams",
  "55003", "Ashland",
  "55005", "Barron",
  "55007", "Bayfield",
  "55009", "Brown",
  "55011", "Buffalo",
  "55013", "Burnett",
  "55015", "Calumet",
  "55017", "Chippewa",
  "55019", "Clark",
  "55021", "Columbia",
  "55023", "Crawford",
  "55025", "Dane",
  "55027", "Dodge",
  "55029", "Door",
  "55031", "Douglas",
  "55033", "Dunn",
  "55035", "Eau Claire",
  "55037", "Florence",
  "55039", "Fond du Lac",
  "55041", "Forest",
  "55043", "Grant",
  "55045", "Green",
  "55047", "Green Lake",
  "55049", "Iowa",
  "55051", "Iron",
  "55053", "Jackson",
  "55055", "Jefferson",
  "55057", "Juneau",
  "55059", "Kenosha",
  "55061", "Kewaunee",
  "55063", "La Crosse",
  "55065", "Lafayette",
  "55067", "Langlade",
  "55069", "Lincoln",
  "55071", "Manitowoc",
  "55073", "Marathon",
  "55075", "Marinette",
  "55077", "Marquette",
  "55078", "Menominee",
  "55079", "Milwaukee",
  "55081", "Monroe",
  "55083", "Oconto",
  "55085", "Oneida",
  "55087", "Outagamie",
  "55089", "Ozaukee",
  "55091", "Pepin",
  "55093", "Pierce",
  "55095", "Polk",
  "55097", "Portage",
  "55099", "Price",
  "55101", "Racine",
  "55103", "Richland",
  "55105", "Rock",
  "55107", "Rusk",
  "55109", "St. Croix",
  "55111", "Sauk",
  "55113", "Sawyer",
  "55115", "Shawano",
  "55117", "Sheboygan",
  "55119", "Taylor",
  "55121", "Trempealeau",
  "55123", "Vernon",
  "55125", "Vilas",
  "55127", "Walworth",
  "55129", "Washburn",
  "55131", "Washington",
  "55133", "Waukesha",
  "55135", "Waupaca",
  "55137", "Waushara",
  "55139", "Winnebago",
  "55141", "Wood"
)

eviction_data <- eviction_data %>%
  mutate(county_fips = str_sub(GEOID, 1, 5)) %>%
  left_join(wi_county_fips, by = "county_fips")

eviction_data_cleaned <- eviction_data %>%
  group_by(county) %>%
  summarise(avg_eviction_filings = mean(filings_avg, na.rm = TRUE))

print(eviction_data_cleaned)

### MERGE

merged_data <- acs_data_cleaned %>%
  left_join(eviction_data_cleaned, by = "county")

merged_data <- merged_data %>%
  mutate(
    avg_eviction_filings_normalized = (avg_eviction_filings - min(avg_eviction_filings, na.rm = TRUE)) / 
      (max(avg_eviction_filings, na.rm = TRUE) - min(avg_eviction_filings, na.rm = TRUE))
  )

print(merged_data)

top10_risk_counties_acs <- merged_data %>%
  arrange(desc(displacement_risk_factor)) %>%
  select(county, displacement_risk_factor) %>%
  head(10)

print(top10_risk_counties_acs)

top10_risk_counties_evi <- merged_data %>%
  arrange(desc(avg_eviction_filings)) %>%
  select(county, avg_eviction_filings) %>%
  head(10)

print(top10_risk_counties_evi)

wi_counties <- counties(state = "WI", year = 2022, cb = TRUE) %>% 
  select(NAME, geometry) %>% 
  rename(county = NAME)

map_data <- wi_counties %>%
  left_join(merged_data, by = "county")

ggplot(map_data) +
  geom_sf(aes(fill = displacement_risk_normalized), color = "black", size = 0.1) + 
  scale_fill_viridis_c(option = "magma", name = "Normalized Displacement Risk") + 
  theme_minimal() +
  labs(
    title = "Displacement Risk by County in Wisconsin",
    subtitle = "Normalized Displacement Risk Factor",
    caption = "Source: ACS & Eviction Lab"
  )

ggplot(map_data) +
  geom_sf(aes(fill = avg_eviction_filings_normalized), color = "black", size = 0.1) + 
  scale_fill_viridis_c(option = "magma", name = "Normalized Eviction Filings") + 
  theme_minimal() +
  labs(
    title = "Average Eviction Filings by County in Wisconsin",
    subtitle = "Normalized 2020-2021 Monthly Eviction Data",
    caption = "Source: Eviction Lab"
  )

correlation_value <- cor(
  merged_data$displacement_risk_normalized, 
  merged_data$avg_eviction_filings_normalized, 
  use = "complete.obs"
)

ggplot(merged_data, aes(x = displacement_risk_normalized, y = avg_eviction_filings_normalized)) +
  geom_point(color = "#A833B9", alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", color = "#FF69B4", se = TRUE) +
  theme_minimal(base_size = 15) +
  labs(
    title = "Correlation between Displacement Risk and Eviction Filings",
    x = "Normalized Displacement Risk Factor",
    y = "Normalized Eviction Filings",
    caption = paste("Pearson Correlation:", round(correlation_value, 3))
  )

### PROPORTION

acs_risk_threshold <- quantile(merged_data$displacement_risk_factor, 0.75, na.rm = TRUE)
eviction_risk_threshold <- quantile(merged_data$avg_eviction_filings, 0.75, na.rm = TRUE)

merged_data <- merged_data %>%
  mutate(
    eviction_risk_norm = (avg_eviction_filings - min(avg_eviction_filings, na.rm = TRUE)) / 
      (max(avg_eviction_filings, na.rm = TRUE) - min(avg_eviction_filings, na.rm = TRUE)),
    acs_risk_norm = (displacement_risk_factor - min(displacement_risk_factor, na.rm = TRUE)) / 
      (max(displacement_risk_factor, na.rm = TRUE) - min(displacement_risk_factor, na.rm = TRUE)),
    combined_risk = (eviction_risk_norm + acs_risk_norm) / 2
  )

combined_risk_threshold <- quantile(merged_data$combined_risk, 0.75, na.rm = TRUE)

high_risk_acs <- merged_data %>% filter(displacement_risk_factor >= acs_risk_threshold)
high_risk_eviction <- merged_data %>% filter(avg_eviction_filings >= eviction_risk_threshold)
high_risk_combined <- merged_data %>% filter(combined_risk >= combined_risk_threshold)

calculate_high_risk_proportion <- function(high_risk_data, total_data) {
  high_risk_summary <- high_risk_data %>%
    summarise(
      total_renters_high_risk = sum(total_renters, na.rm = TRUE),
      total_owners_high_risk = sum(total_owners, na.rm = TRUE),
      white_population_high_risk = sum(white_population, na.rm = TRUE),
      black_population_high_risk = sum(black_population, na.rm = TRUE),
      asian_population_high_risk = sum(asian_population, na.rm = TRUE),
      hispanic_population_high_risk = sum(hispanic_population, na.rm = TRUE),
      seniors_population_high_risk = sum(seniors_population, na.rm = TRUE),
      children_population_high_risk = sum(children_population, na.rm = TRUE)
    )
  
  total_summary <- total_data %>%
    summarise(
      total_renters = sum(total_renters, na.rm = TRUE),
      total_owners = sum(total_owners, na.rm = TRUE),
      white_population = sum(white_population, na.rm = TRUE),
      black_population = sum(black_population, na.rm = TRUE),
      asian_population = sum(asian_population, na.rm = TRUE),
      hispanic_population = sum(hispanic_population, na.rm = TRUE),
      seniors_population = sum(seniors_population, na.rm = TRUE),
      children_population = sum(children_population, na.rm = TRUE)
    )
  
  proportion_table <- high_risk_summary / total_summary
  proportion_table <- proportion_table %>%
    mutate(Risk_Type = NA) %>%
    select(Risk_Type, everything())
  
  return(proportion_table)
}

proportion_acs_risk <- calculate_high_risk_proportion(high_risk_acs, merged_data) %>% mutate(Risk_Type = "ACS Displacement Risk")
proportion_eviction_risk <- calculate_high_risk_proportion(high_risk_eviction, merged_data) %>% mutate(Risk_Type = "Eviction Risk")
proportion_combined_risk <- calculate_high_risk_proportion(high_risk_combined, merged_data) %>% mutate(Risk_Type = "Combined Risk")

proportion_table <- bind_rows(proportion_acs_risk, proportion_eviction_risk, proportion_combined_risk)
proportion_table_transposed <- as.data.frame(t(proportion_table))
colnames(proportion_table_transposed) <- proportion_table_transposed[1, ]
proportion_table_transposed <- proportion_table_transposed[-1, ]

print(proportion_table_transposed)

custom_order <- c("asian", "black", "hispanic", "white", "children", "seniors", "owners", "renters")

proportion_long <- proportion_table %>%
  pivot_longer(cols = -Risk_Type, names_to = "Group", values_to = "Proportion") %>%
  mutate(Group = str_replace(Group, "_population_high_risk", "")) %>%
  mutate(Group = str_replace(Group, "_high_risk", "")) %>%
  mutate(Group = str_replace(Group, "total_", ""))

proportion_long <- proportion_long %>%
  mutate(Group = factor(Group, levels = custom_order))

ggplot(proportion_long, aes(x = Group, y = Proportion, fill = Risk_Type)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#A833B9", "#C42D92", "#FF69B4")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  labs(
    title = "Proportion of Groups at High Displacement Risk",
    x = "Group",
    y = "Proportion",
    fill = "Risk Measure",
    caption = "Data: ACS & Eviction Lab"
  )