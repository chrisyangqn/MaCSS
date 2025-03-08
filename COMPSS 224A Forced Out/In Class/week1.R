install.packages('tidyverse')
install.packages("tidycensus", type = "binary")

library('tidyverse')
library('tidycensus')

# Read a table of census data
vars_acs5_2023 <- load_variables(2023, 'acs5', cache = TRUE)
View(vars_acs5_2023)

# Get acs
males <-
  get_acs(
    geography = 'tract',
    state = 'California',
    county = 'Alameda',
    variables = 'B01002_002'
  )