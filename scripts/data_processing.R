# data visualization deaths by suicide in Maricopa County
# rene dario
# 3 July 2021

# set up
# packages
library(here)
library(tidyverse)
library(lubridate)
library(epitools)
# library(zoo) how does this work?

# source data tidy script
source(file = "scripts/data_tidy.R")

# read data 
# not needed because sourcing file above
# s_data <- read_rds("data/tidy/death_data.rds")

# inspect
glimpse(s_data)
str(s_data)

# type of death ####
# grouped by type of death
death_crude_rates <- s_data %>%
  group_by(manner_of_death) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(
    population = pop_total,
    crude_rate = (n / population) * 100000
  )

# deaths by manner of death type
# raw numbers
# death by suicide?
s_data %>%
  group_by(suicide_factor) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(
    population = pop_total,
    crude_rate = (n / population) * 100000
  )

# as a proportion of total deaths
death_by_type_proportions <- s_data %>%
  group_by(manner_of_death) %>%
  count() %>%
  summarise_all(sum) %>%
  mutate(
    total_deaths = sum(n),
    prop_of_total_deaths = n / total_deaths
  ) %>%
  mutate(
    population = pop_total,
    crude_rate = (n / population) * 100000
  )

death_by_type_proportions %>%
  arrange(desc(prop_of_total_deaths))

death_by_type_proportions

# explore total deaths
# sex ####
# grouped by sex
s_data %>%
  group_by(gender) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(
    population = if_else(gender == "MALE", pop_mal, pop_fem),
    crude_rate = (n / population) * 100000
  )

# deaths by suicide by gender, crude rates
death_crude_rates_sex <- s_data %>%
  filter(manner_of_death == "SUICIDE") %>%
  group_by(gender) %>%
  count() %>%
  mutate(
    population = if_else(gender == "MALE", pop_mal, pop_fem),
    crude_rate = (n / population) * 100000
  )

# crude rates and suicide as proportion of total death
# female
death_type_fem <- s_data %>%
  filter(gender == "FEMALE") %>%
  group_by(manner_of_death) %>%
  count() %>%
  summarise_all(sum) %>%
  mutate(
    total_deaths = sum(n),
    prop_of_total_deaths = n / total_deaths,
    population = pop_fem,
    crude_rate = (n / population) * 100000
  )

death_type_fem %>%
  arrange(desc(prop_of_total_deaths))

# male
death_type_male <- s_data %>%
  filter(gender == "MALE") %>%
  group_by(manner_of_death) %>%
  count() %>%
  summarise_all(sum) %>%
  mutate(
    total_deaths = sum(n),
    prop_of_total_deaths = n / total_deaths,
    population = pop_mal,
    crude_rate = (n / population) * 100000
  )

death_type_male %>%
  arrange(desc(prop_of_total_deaths))

# 2 x 2 contingency table
table(s_data$gender,
  s_data$suicide_factor,
  dnn = c("Gender", "Death by Suicide")
) %>%
  rateratio(verbose = TRUE,
            rev = "columns")

# race ####
# total deaths grouped by race
s_data %>%
  group_by(race) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(
    population = if_else(
      race == "WHITE", pop_white,
      if_else(race == "HISPANIC", pop_hisp,
        if_else(race == "BLACK", pop_black,
          if_else(race == "ASIAN", pop_asian,
            if_else(race == "AMERICAN INDIAN", pop_ai, NULL)
          )
        )
      )
    ),
    crude_rate = (n / population) * 100000
  ) %>%
  arrange(desc(crude_rate))

# suicide as a proportion of total death by race 
suicide_death_by_race_rate_prop <- s_data %>%
  group_by(race, manner_of_death) %>%
  count() %>%
  summarise_all(sum) %>%
  mutate(
    total_deaths = sum(n),
    prop_of_total_deaths = n / total_deaths,
    population = if_else(
      race == "WHITE", pop_white,
      if_else(race == "HISPANIC", pop_hisp,
              if_else(race == "BLACK", pop_black,
                      if_else(race == "ASIAN", pop_asian,
                              if_else(race == "AMERICAN INDIAN", pop_ai, NULL)
                      )
              )
      )
    ),
    crude_rate = (n / population) * 100000
  )
  
# deaths by suicide by race, proportions
# AMERICAN INDIAN
death_type_ai <- s_data %>%
  filter(race == "AMERICAN INDIAN") %>%
  group_by(manner_of_death) %>%
  count() %>%
  summarise_all(sum) %>%
  mutate(
    total_deaths = sum(n),
    prop_of_total_deaths = n / total_deaths,
    population = pop_ai,
    crude_rate = (n / population) * 100000
  )

death_type_ai %>%
  arrange(desc(prop_of_total_deaths))

# 2 x 2 contingency table
table(s_data$race_ai_fct,
  s_data$suicide_factor,
  dnn = c("American Indian", "Death by Suicide")
) %>%
  rateratio(verbose = TRUE,
            rev = "rows")

# ASIAN
death_type_asian <- s_data %>%
  filter(race == "ASIAN") %>%
  group_by(manner_of_death) %>%
  count() %>%
  summarise_all(sum) %>%
  mutate(
    total_deaths = sum(n),
    prop_of_total_deaths = n / total_deaths,
    population = pop_asian,
    crude_rate = (n / population) * 100000
  )

death_type_asian %>%
  arrange(desc(prop_of_total_deaths))

# 2 x 2 contingency table
table(s_data$race_asian_fct,
  s_data$suicide_factor,
  dnn = c("Asian", "Death by Suicide")
) %>%
  rateratio(verbose = TRUE,
            rev = "columns")

# BLACK
death_type_black <- s_data %>%
  filter(race == "BLACK") %>%
  group_by(manner_of_death) %>%
  count() %>%
  summarise_all(sum) %>%
  mutate(
    total_deaths = sum(n),
    prop_of_total_deaths = n / total_deaths,
    population = pop_black,
    crude_rate = (n / population) * 100000
  )

death_type_black %>%
  arrange(desc(prop_of_total_deaths))

# 2 x 2 contingency table
table(s_data$race_black_fct,
  s_data$suicide_factor,
  dnn = c("Black", "Death by Suicide")
) %>%
  rateratio(verbose = TRUE,
            rev = "columns")

# HISPANIC
death_type_hispanic <- s_data %>%
  filter(race == "HISPANIC") %>%
  group_by(manner_of_death) %>%
  count() %>%
  summarise_all(sum) %>%
  mutate(
    total_deaths = sum(n),
    prop_of_total_deaths = n / total_deaths,
    population = pop_hisp,
    crude_rate = (n / population) * 100000
  )

death_type_hispanic %>%
  arrange(desc(prop_of_total_deaths))

# 2 x 2 contingency table
table(s_data$race_hispanic_fct,
  s_data$suicide_factor,
  dnn = c("Hispanic", "Death by Suicide")
) %>%
  rateratio(verbose = TRUE,
            rev = "neither")

# OTHER/UNKNOWN
death_type_other <- s_data %>%
  filter(race == "OTHER/UNKNOWN") %>%
  group_by(manner_of_death) %>%
  count() %>%
  summarise_all(sum) %>%
  mutate(
    total_deaths = sum(n),
    prop_of_total_deaths = n / total_deaths
  ) %>%
  select(
    manner_of_death,
    "prop_of_total_deaths"
  )

death_type_other %>%
  arrange(desc(prop_of_total_deaths))

# WHITE
death_type_white <- s_data %>%
  filter(race == "WHITE") %>%
  group_by(manner_of_death) %>%
  count() %>%
  summarise_all(sum) %>%
  mutate(
    total_deaths = sum(n),
    prop_of_total_deaths = n / total_deaths,
    population = pop_white,
    crude_rate = (n / population) * 100000
  )

death_type_white %>%
  arrange(desc(prop_of_total_deaths))

# 2 x 2 contingency table
table(s_data$race_white_fct,
  s_data$suicide_factor,
  dnn = c("White", "Death by Suicide")
) %>%
  rateratio(verbose = TRUE,
            rev = "columns")

# plot suicide binary grouped by race
# raw numbers
suicide_death_by_race <- s_data %>%
  group_by(race, suicide_factor) %>%
  count() %>%
  mutate(
    population = if_else(
      race == "WHITE", pop_white,
      if_else(race == "HISPANIC", pop_hisp,
        if_else(race == "BLACK", pop_black,
          if_else(race == "ASIAN", pop_asian,
            if_else(race == "AMERICAN INDIAN", pop_ai, NULL)
          )
        )
      )
    ),
    crude_rate = (n / population) * 100000
  )

death_crude_rates_race <- suicide_death_by_race %>%
  filter(suicide_factor == "yes") %>%
  arrange(desc(crude_rate))

# age ####
# summary statistics for age
s_data %>%
  summarise(
    age_min = min(age),
    age_average = mean(age),
    age_median = median(age),
    age_max = max(age)
  )

# stem and leaf plot for age
stem(s_data$age)

# age
s_data %>%
  filter(manner_of_death == "SUICIDE") %>%
  summarise(
    age_min = min(age),
    age_average = mean(age),
    age_median = median(age),
    age_max = max(age)
  )

# suicide by age group
# first need to reverse the factor sort level for the plot
s_data_rev_age <- mutate(s_data, age_group = fct_rev(s_data$age_group))

# age groups with highest raw numbers of suicide
s_data_rev_age <- s_data_rev_age %>%
  drop_na() %>%
  filter(manner_of_death == "SUICIDE") %>%
  group_by(age_group)

# crude rate for each age group 
suicide_death_rate_by_age <- s_data_rev_age %>%
  count() %>%
  #filter(n >= 10) %>%
  mutate(population = if_else(
    age_group == "15-19 years", pop_age_15_19,
    if_else(age_group == "20-24 years", pop_age_20_24,
      if_else(age_group == "25-29 years", pop_age_25_29,
        if_else(age_group == "30-34 years", pop_age_30_34,
          if_else(age_group == "35-39 years", pop_age_35_39,
            if_else(age_group == "40-44 years", pop_age_40_44,
              if_else(age_group == "45-49 years", pop_age_45_49,
                if_else(age_group == "50-54 years", pop_age_50_54,
                  if_else(age_group == "55-59 years", pop_age_55_59,
                    if_else(age_group == "60-64 years", pop_age_60_64,
                      if_else(age_group == "65-69 years", pop_age_65_69,
                        if_else(age_group == "70-74 years", pop_age_70_74,
                          if_else(age_group == "75-79 years", pop_age_75_79,
                            if_else(age_group == "80-84 years", pop_age_80_84, NULL)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  crude_rate = (n / population) * 100000) %>%
  #drop_na() %>%
  arrange(desc(crude_rate))

suicide_death_rate_by_age

# attempt to age adjust 
suicide_rate_by_age_adj <- full_join(suicide_death_rate_by_age,
          standard_pop,
          by = c("age_group" ="age")) %>%
  select(1:4, 6)

suicide_rate_by_age_adj %>%
  filter(age_group != "Total") %>%
  summarise(n = sum(n),
            population = sum(population),
            crude_rate = sum(crude_rate),
            us_standard_pop = sum(x2000_u_s_standard_population_census_p25_1130)) %>%
  mutate(age_dist = us_standard_pop / sum(us_standard_pop),
         age_adj_rate = crude_rate * age_dist) %>%
  drop_na() %>%
  summarise(age_adj_rate = sum(age_adj_rate))

s_data %>%
  filter(manner_of_death == "SUICIDE") %>%
  drop_na() %>%
  mutate(age_group_lump = fct_lump(age_group, n = 12)) %>%
  count(age_group_lump, sort = TRUE)

# over time ####
# deaths by year
s_data %>%
  group_by(deathmonth) %>%
  count() %>%
  arrange(desc(n))

# time series
# daily
s_data %>%
  filter(suicide_factor == "yes") %>%
  group_by(date) %>%
  count() %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = n))

# monthly
s_data %>%
  filter(suicide_factor == "yes") %>%
  mutate(deathmonth = month(date, label = TRUE)) %>%
  count(deathmonth) %>%
  ggplot() +
  geom_bar(mapping = aes(x = deathmonth, y = n), stat = "identity")

