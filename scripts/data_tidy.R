# data visualization deaths by suicide in Maricopa County
# rene dario
# 3 July 2021

# set up
# packages
library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

# read data
s_data <- read_excel("data/raw/Suicide_Data_Data_Viz_a.xlsx") %>%
  clean_names()

# inspect
s_data
glimpse(s_data)
str(s_data)

# create date from deathyear, deathmonth, deathday
s_data$date <- str_c(
  s_data$deathyear, s_data$deathmonth, s_data$deathday,
  sep = "-"
)

s_data$date <- ymd(s_data$date)

# recode suicide as binary 0 or 1
s_data <- s_data %>%
  mutate(suicide = recode(manner_of_death,
    "ACCIDENT" = 0,
    "HOMICIDE" = 0,
    "NATURAL DEATH" = 0,
    "PENDING INVESTIGATION" = 0,
    "SUICIDE" = 1,
    "UNDETERMINED" = 0,
    .missing = 0
  )) %>%
  mutate(suicide_factor = as_factor(suicide)) %>%
  mutate(suicide_factor = recode(suicide_factor,
    "0" = "no",
    "1" = "yes"
  ))

# recode race_white as binary 0 or 1
s_data <- s_data %>%
  mutate(race_white = if_else(race == "WHITE", 1, 0, missing = 0)) %>%
  mutate(race_white_fct = as_factor(race_white)) %>%
  mutate(race_white_fct = recode(race_white_fct,
    "0" = "no",
    "1" = "yes"
  ))

# recode race_hispanic as binary 0 or 1
s_data <- s_data %>%
  mutate(race_hispanic = if_else(race == "HISPANIC", 1, 0, missing = 0)) %>%
  mutate(race_hispanic_fct = as_factor(race_hispanic)) %>%
  mutate(race_hispanic_fct = recode(race_hispanic_fct,
    "0" = "no",
    "1" = "yes"
  ))

# recode race_asian as binary 0 or 1
s_data <- s_data %>%
  mutate(race_asian = if_else(race == "ASIAN", 1, 0, missing = 0)) %>%
  mutate(race_asian_fct = as_factor(race_asian)) %>%
  mutate(race_asian_fct = recode(race_asian_fct,
    "0" = "no",
    "1" = "yes"
  ))

# recode race_black as binary 0 or 1
s_data <- s_data %>%
  mutate(race_black = if_else(race == "BLACK", 1, 0, missing = 0)) %>%
  mutate(race_black_fct = as_factor(race_black)) %>%
  mutate(race_black_fct = recode(race_black_fct,
    "0" = "no",
    "1" = "yes"
  ))

# recode race_ai as binary 0 or 1
s_data <- s_data %>%
  mutate(race_ai = if_else(race == "AMERICAN INDIAN", 1, 0, missing = 0)) %>%
  mutate(race_ai_fct = as_factor(race_ai)) %>%
  mutate(race_ai_fct = recode(race_ai_fct,
    "0" = "no",
    "1" = "yes"
  ))

# recode age to be in groups
s_data <- s_data %>%
  mutate(
    age_group =
      case_when(
        age < 5 ~ "<5 years",
        age > 5 & age < 10 ~ "5-9 years",
        age > 10 & age < 15 ~ "10-14 years",
        age > 15 & age < 20 ~ "15-19 years",
        age > 20 & age < 25 ~ "20-24 years",
        age > 25 & age < 30 ~ "25-29 years",
        age > 30 & age < 35 ~ "30-34 years",
        age > 35 & age < 40 ~ "35-39 years",
        age > 40 & age < 45 ~ "40-44 years",
        age > 45 & age < 50 ~ "45-49 years",
        age > 50 & age < 55 ~ "50-54 years",
        age > 55 & age < 50 ~ "55-59 years",
        age > 60 & age < 65 ~ "60-64 years",
        age > 65 & age < 70 ~ "65-69 years",
        age > 70 & age < 75 ~ "70-74 years",
        age > 75 & age < 80 ~ "75-79 years",
        age > 80 & age < 85 ~ "80-84 years",
        age > 85 ~ ">=85 years"
      )
  )

s_data <- s_data %>%
  mutate(age_group = factor(age_group,
    levels = c(
      "<5 years",
      "5-9 years",
      "10-14 years",
      "15-19 years",
      "20-24 years",
      "25-29 years",
      "30-34 years",
      "35-39 years",
      "40-44 years",
      "45-49 years",
      "50-54 years",
      "55-59 years",
      "60-64 years",
      "65-69 years",
      "70-74 years",
      "75-79 years",
      "80-84 years",
      ">=85 years"
    ),
    ordered = TRUE
  ))

# inspect
levels(s_data$age_group)

# save the data to disk
write_rds(s_data, "data/tidy/death_data.rds")

# comparative crude rates of suicide
# source: https://www.cdc.gov/nchs/fastats/suicide.htm
# USA: 14.5 suicide deaths per 100,000
# AZ: 19.5 suicide deaths per 100,000
# Maricopa County: 16.3

# citation:
# Centers for Disease Control and Prevention, National Center for Health Statistics. Underlying Cause of Death 1999-2019 on CDC WONDER Online Database, released in 2020. Data are from the Multiple Cause of Death Files, 1999-2019, as compiled from data provided by the 57 vital statistics jurisdictions through the Vital Statistics Cooperative Program. Accessed at http://wonder.cdc.gov/ucd-icd10.html on Jul 7, 2021 3:16:49 PM

death_crude_rate_cdc_usa <- 14.5
death_crude_rate_cdc_az <- 19.5
death_crude_rate_cdc_mar <- 16.3

# write_rds(crude_suicide_death_rates, "data/tidy/crude_suicide_death_rates.rds")

# import data from CDC Wonder
# age
cdc_age <- read_tsv(
  file = "data/raw/cdc_cause_of_death_age.txt",
  n_max = 45,
  na = c("", "NA", "Not Applicable", "Unreliable")
) %>%
  clean_names() %>%
  filter(county_code == "04013")

cdc_age <- cdc_age %>%
  filter(five_year_age_groups != "NA") %>%
  select(county, five_year_age_groups, deaths, population, crude_rate)

# race
cdc_race_hisp <- read_tsv(
  file = "data/raw/cdc_cause_of_death_race_hisp.txt",
  n_max = 21,
  na = c("", "NA", "Not Applicable", "Unreliable")
) %>%
  clean_names() %>%
  filter(county_code == "04013")

cdc_race_hisp <- cdc_race_hisp %>%
  select(county, race, hispanic_origin, deaths, population, crude_rate)

# sex
cdc_sex <- read_tsv(
  file = "data/raw/cdc_cause_of_death_gender.txt",
  n_max = 17,
  na = c("", "NA", "Not Applicable", "Unreliable")
) %>%
  clean_names() %>%
  filter(county_code == "04013")

cdc_sex <- cdc_sex %>%
  select(county, gender, deaths, population, crude_rate)

# sex, hispanic, race, age
cdc_sex_hisp_race_age <- read_tsv(
  file = "data/raw/cdc_cause_of_death_sex_hisp_race_age.txt",
  n_max = 109,
  na = c("", "NA", "Not Applicable", "Unreliable")
) %>%
  clean_names() %>%
  filter(county_code == "04013")

cdc_sex_hisp_race_age <- cdc_sex_hisp_race_age %>%
  select(county, gender, hispanic_origin, race, five_year_age_groups, deaths, population, crude_rate)

# combine
cdc_pop_denominators <- bind_rows(
  cdc_age, cdc_race_hisp, cdc_sex, cdc_sex_hisp_race_age
)

# create variables for use in processing
# total
pop_total <- 4485414

# sex
pop_fem <- 2268184
pop_mal <- 2217230

# race
pop_white <- 2497373
pop_hisp <- 1285543
pop_black <- 279060
pop_asian <- 218862
pop_ai <- 81264

# sex & race
pop_fem_white <- 1564971
pop_male_white <- 1511588
pop_fem_hisp <- 641889
pop_male_hisp <- 643654
pop_fem_black <- 139005
pop_male_black <- 140055
pop_male_ai <- 38423

# age
pop_age_15_19 <- 300164
pop_age_20_24 <- 296160
pop_age_25_29 <- 340955
pop_age_30_34 <- 317424
pop_age_35_39 <- 303640
pop_age_40_44 <- 280702
pop_age_45_49 <- 285146
pop_age_50_54 <- 270492
pop_age_55_59 <- 273909
pop_age_60_64 <- 248439
pop_age_65_69 <- 215853
pop_age_70_74 <- 186626
pop_age_75_79 <- 130836
pop_age_80_84 <- 82694

# standard population for age adjustment 
standard_pop <- read_csv(file = "data/raw/standard_population.csv") %>%
  clean_names()

standard_pop <- standard_pop %>%
  mutate(age = if_else(age == "01-04 years", "<5 years", age),
         age = if_else(age == "85+ years", ">=85 years", age),
         age = if_else(age == "05-09 years", "5-9 years", age),
         x2000_u_s_standard_population_census_p25_1130 = if_else(age == "<5 years", 18986520, x2000_u_s_standard_population_census_p25_1130)) %>%
  filter(age != "00 years")

write_rds(standard_pop, "data/tidy/standard_pop.rds")
