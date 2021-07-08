# data visualization deaths by suicide in Maricopa County
# rene dario 
# 3 July 2021 

# set up
# packages 
library(here)
library(tidyverse)
library(tidycensus)

# Source: U.S. Census Bureau, 2019 American Community Survey 1-Year Estimates

# load variables 
var_acs_2019_acs1 <- load_variables(2019, "acs1", cache = TRUE)
var_acs_2019_acs1_sub <- load_variables(2019, "acs1/subject", cache = TRUE)

# get data for Maricopa county 
# demographics
maricopa_total_pop <- get_acs(
  geography = "county",
  year = 2019,
  state = "04",
  county = "Maricopa",
  variables = c("total_pop" = "B01003_001",
                "total_male" = "B01001_002",
                "total_female" = "B01001_026",
                "total_ai" = "B02001_004",
                "total_asian" = "B02001_005",
                "total_black" = "B02001_003",
                "total_hispanic" = "B03003_003",
                "total_white" = "B02001_002"),
  survey = "acs1",
  cache_table = TRUE)

# age
maricopa_age <- get_acs(
  geography = "county",
  year = 2019,
  state = "04",
  county = "Maricopa",
  variables = c(
    "total" = "S0101_C01_001",
    "age<5" = "S0101_C01_002",
    "age5-9" = "S0101_C01_003",
    "age10-14" = "S0101_C01_004",
    "age15-19" = "S0101_C01_005",
    "age20-24" = "S0101_C01_006",
    "age25-29" = "S0101_C01_007",
    "age30-34" = "S0101_C01_008",
    "age35-39" = "S0101_C01_009",
    "age40-44" = "S0101_C01_010",
    "age45-49" = "S0101_C01_011",
    "age50-54" = "S0101_C01_012",
    "age55-59" = "S0101_C01_013",
    "age60-64" = "S0101_C01_014",
    "age65-69" = "S0101_C01_015",
    "age70-74" = "S0101_C01_016",
    "age75-79" = "S0101_C01_017",
    "age80-84" = "S0101_C01_018",
    "age85<" = "S0101_C01_019"
  ),
  survey = "acs1",
  cache_table = TRUE
)

# proportion of age group in population 
maricopa_age %>%
  filter(variable != "total") %>%
  mutate(percent_of_total = estimate / 4485414) %>%
  arrange(desc(percent_of_total))
