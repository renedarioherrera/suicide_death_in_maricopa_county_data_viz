# data visualization deaths by suicide in Maricopa County
# rene dario 
# 3 July 2021 

# set up
# packages 
library(here)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(knitr)
library(janitor)
library(gt)

# source data processing script
source(file = "scripts/data_processing.R")

# read data 
# s_data <- read_rds("data/tidy/death_data.rds")

# inspect
glimpse(s_data)
str(s_data)

# comparing which cause of death has highest rate 
death_crude_rates %>%
  drop_na() %>%
  ggplot(mapping = aes(x = reorder(manner_of_death, crude_rate), y = crude_rate)) +
  geom_bar(stat = "identity", alpha = 0.9, color = "black") +
  coord_flip() +
  geom_label(
    label="15.2", 
    x=4,
    y=45.2,
    label.padding = unit(.35, "lines"), # Rectangle size around label
    label.size = 0.25,
    color = "black",
    fill="white"
  ) +
  labs(title = "All Cause Mortality Death Rate for Maricopa County, AZ in 2019",
       subtitle = "Year: 2019",
       caption = "",
       x = "Cause of Death",
       y = "Crude Rate per 100,000") +
  theme_linedraw() +
  theme(
    plot.background = element_rect(fill = "#F7F5FC"),
    panel.border = element_rect(color = "black")
  )
  
# same thing but without death by natural cause
# comparing which cause of death has highest rate 
# visualization 1 ####
all_cause_mortality <- death_crude_rates %>%
  filter(manner_of_death != "NATURAL DEATH") %>%
  drop_na() %>%
  ungroup() %>%
  ggplot() +
  geom_bar(mapping = aes(x = reorder(manner_of_death, crude_rate), y = crude_rate), stat = "identity", alpha = 0.8, color = "black", fill = "#0E081B") +
  geom_hline(yintercept = death_crude_rate_cdc_usa, color = death_crude_rate_cdc_usa, show.legend = TRUE, size = 3, alpha = 0.8) +
  geom_hline(yintercept = death_crude_rate_cdc_az, color = death_crude_rate_cdc_az, show.legend = TRUE, size = 3, alpha = 0.8) +
  coord_flip() +
  geom_label(
    label="15.2", 
    x=4,
    y=10,
    label.padding = unit(.35, "lines"), # Rectangle size around label
    label.size = 0.25,
    color = "black",
    fill="white",
    size = 12
  ) +
  geom_text(
    label = "US*: 14.5",
    x = 1,
    y = 9,
    color = death_crude_rate_cdc_usa,
    size = 12
  ) +
  geom_text(
    label = "AZ*: 19.5",
    x = 1,
    y = 25,
    color = death_crude_rate_cdc_az,
    size = 12
  ) +
  labs(title = "Death Rate for Maricopa County, AZ in 2019",
       subtitle = "Excluding deaths from natural causes",
       caption = "*Source: Centers for Disease Control and Prevention, National Center for Health Statistics.",
       x = "Cause of Death",
       y = "Crude Rate* per 100,000") +
  theme_linedraw() +
  theme(
    plot.background = element_rect(fill = "#F7F5FC"),
    panel.border = element_rect(color = "black"),
    text = element_text(size = 32)
  )

all_cause_mortality

ggsave(filename = "graphics/plot_all_cause_mortality.png",
       plot = all_cause_mortality,
       device = "png",
       scale = 1,
       width = 1920,
       height = 1080,
       units = "px",
       dpi = "screen")

write_rds(all_cause_mortality, "data/tidy/plot_all_cause_mortality.rds")

# crude rates by race 
# visualization 2 #### 
plot_death_crude_rates_race <- death_crude_rates_race %>%
  drop_na() %>%
  ggplot(mapping = aes(x = reorder(race, crude_rate), y = crude_rate)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "black", fill = "#0E081B") +
  geom_hline(yintercept = 15.2, color = "blue", show.legend = TRUE, size = 3, alpha = 0.8) +
  coord_flip()  +
  geom_text(
    label="County: 15.2", 
    x=1,
    y=17,
    color = "blue",
    size = 12
  ) +
  labs(title = "Suicide Death Rate by Race in Maricopa County, AZ in 2019",
       subtitle = "",
       caption = "*Source: Centers for Disease Control and Prevention, National Center for Health Statistics.",
       x = "Race",
       y = "Crude Rate* per 100,000") +
  theme_linedraw() +
  theme(
    plot.background = element_rect(fill = "#F7F5FC"),
    panel.border = element_rect(color = "black"),
    text = element_text(size = 32)
  )

plot_death_crude_rates_race

ggsave(filename = "graphics/plot_suicide_death_by_race.png",
       plot = plot_death_crude_rates_race,
       device = "png",
       scale = 1,
       width = 1920,
       height = 1080,
       units = "px",
       dpi = "screen")

write_rds(plot_death_crude_rates_race, "data/tidy/plot_suicide_rate_by_race.rds")

# visualization 2 comparison #### 
plot_suicide_death_by_race_rate_prop <- suicide_death_by_race_rate_prop %>%
  filter(manner_of_death == "SUICIDE") %>%
  drop_na() %>%
  ggplot(mapping = aes(x = reorder(race, prop_of_total_deaths), y = prop_of_total_deaths)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "black", fill = "#0E081B") +
  #geom_hline(yintercept = 15.2, color = "blue", show.legend = TRUE, size = 3, alpha = 0.8) +
  coord_flip()  +
  # geom_text(
  #   label="County: 15.2", 
  #   x=1,
  #   y=17,
  #   color = "blue",
  #   size = 12
  # ) +
  labs(title = "Proportion of Death by Suicide in Maricopa County, AZ in 2019",
       subtitle = "Grouped by Race",
       caption = "",
       x = "Race",
       y = "Proportion of death by suicide") +
  theme_linedraw() +
  theme(
    plot.background = element_rect(fill = "#F7F5FC"),
    panel.border = element_rect(color = "black"),
    text = element_text(size = 32)
  )

plot_suicide_death_by_race_rate_prop

ggsave(filename = "graphics/plot_suicide_death_by_race_proportion.png",
       plot = plot_suicide_death_by_race_rate_prop,
       device = "png",
       scale = 1,
       width = 1920,
       height = 1080,
       units = "px",
       dpi = "screen")
  
# crude rates by sex
death_crude_rates_sex %>%
  drop_na() %>%
  ggplot(mapping = aes(x = reorder(gender, crude_rate), y = crude_rate)) +
  geom_bar(stat = "identity", alpha = 0.75, color = "black") +
  geom_hline(yintercept = 15.2, color = "blue", show.legend = TRUE) +
  coord_flip() +
  geom_text(
    label="County: 15.2", 
    x=1,
    y=17,
    color = "blue"
  ) +
  geom_label(mapping = aes(label = round(crude_rate, digits = 2))) +
  labs(title = "Death Rates in Maricopa County",
       subtitle = "Year: 2019",
       caption = "",
       x = "Gender",
       y = "Crude Rate per 100,000") +
  theme_linedraw()

# but better as a table 
table_death_crude_rates_sex <- death_crude_rates_sex %>%
  mutate(gender = if_else(gender == "FEMALE", "Female", "Male")) %>%
  adorn_totals() %>%
  mutate(crude_rate = if_else(crude_rate >= 30, 15.2, crude_rate)) %>%
  kable(digits = 1,
        row.names = FALSE,
        col.names = c("Gender", "Deaths by Suicide", "Number in Category*", "Crude Death Rate per 100,000"),
        caption = "*Source: Centers for Disease Control and Prevention, National Center for Health Statistics. Underlying Cause of Death 1999-2019 on CDC WONDER Online Database, released in 2020.")

table_death_crude_rates_sex

write_rds(table_death_crude_rates_sex, "data/tidy/table_suicide_rate_by_sex.rds")

# visualization 3 ####
death_crude_rates_sex %>%
  mutate(gender = if_else(gender == "FEMALE", "Female", "Male")) %>%
  adorn_totals() %>%
  mutate(crude_rate = if_else(crude_rate >= 30, 15.2, crude_rate),
         crude_rate = round(crude_rate, digits = 1)) %>%
  select(Gender = gender,
         "Deaths by Suicide" = n,
         "Number in Category*" = population,
         "Crude Death Rate* per 100,000" = crude_rate) %>%
  gt() %>%
  tab_header(title = "Suicide Death Rate by Sex in Maricopa County, AZ in 2019") %>%
  tab_source_note(source_note = "*Source: Centers for Disease Control and Prevention, National Center for Health Statistics. Underlying Cause of Death 1999-2019 on CDC WONDER Online Database, released in 2020.") %>%
  tab_options(
    table.width = 1080,
    heading.background.color = "#F7F5FC",
    table.font.size = 32
  )

# suicide death by race 
suicide_death_by_race %>%
  
  
# suicide versus other causes of death 
# rank order 
# deaths by manner of death type
# raw numbers 
s_data %>%
  group_by(manner_of_death) %>%
  drop_na() %>%
  count() %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(manner_of_death, n), y = n)) +
  coord_flip() +
  labs(title = "Most Common Cause of Death in Maricopa County",
       subtitle = "For Year 2019",
       caption = "Values removed where cause of death is not documented",
       x = "Cause of Death",
       y = "Count")

# by sex
# suicide as a proportion of total deaths 
suicide_by_sex_rate_prop <- s_data %>%
  group_by(gender, manner_of_death) %>%
  count() %>%
  summarise_all(sum) %>%
  mutate(
    total_deaths = sum(n),
    prop_of_total_deaths = n / total_deaths,
    population = if_else(gender == "FEMALE", pop_fem, pop_mal),
    crude_rate = (n / population) * 100000
  ) %>%
  filter(manner_of_death == "SUICIDE") %>%
  mutate(gender = if_else(gender == "FEMALE", "Female", "Male")) %>%
  adorn_totals() %>%
  mutate(crude_rate = if_else(crude_rate >= 30, 15.2, crude_rate),
         crude_rate = round(crude_rate, digits = 1),
         prop_of_total_deaths = if_else(gender == "Total", 0.0224, prop_of_total_deaths),
         prop_of_total_deaths = round(prop_of_total_deaths, digits = 3),) %>%
select(Gender = gender,
       "Number of suicide deaths" = n,
       "Total deaths in category" = total_deaths,
       "Proportion of suicide deaths" = prop_of_total_deaths,
       "Total population in category*" = population,
       "Crude death rate per 100,000" = crude_rate)
  
suicide_by_sex_rate_prop %>%
  gt() %>%
  tab_header(title = "Suicide Death Rate by Sex in Maricopa County, AZ in 2019") %>%
  tab_source_note(source_note = "*Source: Centers for Disease Control and Prevention, National Center for Health Statistics. Underlying Cause of Death 1999-2019 on CDC WONDER Online Database, released in 2020.") %>%
  tab_options(
    table.width = 1080,
    heading.background.color = "#F7F5FC",
    table.font.size = 32
  )

# by race
s_data %>%
  filter(race != "OTHER/UNKNOWN") %>%
  group_by(manner_of_death, race) %>%
  drop_na() %>%
  count() %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(manner_of_death, n), y = n)) +
  coord_flip() +
  facet_wrap(~race) +
  labs(title = "Most Common Cause of Death in Maricopa County",
       subtitle = "For Year 2019",
       caption = "Values removed where race or cause of death are not documented",
       x = "Cause of Death",
       y = "Count")

# by race and sex
s_data %>%
  filter(race != "OTHER/UNKNOWN") %>%
  group_by(manner_of_death, race, gender) %>%
  drop_na() %>%
  count() %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(manner_of_death, n), y = n, fill = gender), position = "dodge") +
  coord_flip() +
  facet_wrap(~race) +
  labs(title = "Most Common Cause of Death in Maricopa County",
       subtitle = "For Year 2019",
       caption = "Values removed where race or cause of death is not documented",
       x = "Cause of Death",
       y = "Count")
  
# suicide as a proportion of all deaths 
# death by suicide?
# sex
s_data %>%
  drop_na() %>%
  ggplot(mapping = aes(x = gender, fill = suicide_factor)) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Suicide as Proportion of Total Deaths in Maricopa County",
       subtitle = "For Year 2019",
       caption = "Values removed where cause of death is not documented",
       x = "Gender",
       y = "%")

# race 
s_data %>%
  drop_na() %>%
  filter(race != "OTHER/UNKNOWN") %>%
  ggplot(mapping = aes(x = race, fill = suicide_factor)) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Suicide as Proportion of Total Deaths in Maricopa County",
       subtitle = "For Year 2019",
       caption = "Values removed where cause of death or race are not documented",
       x = "Race",
       y = "%")

# age
s_data %>%
  drop_na() %>%
  ggplot(mapping = aes(x = age_group, fill = suicide_factor)) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Suicide as Proportion of Total Deaths in Maricopa County",
       subtitle = "For Year 2019",
       caption = "Values removed where cause of death is not documented",
       x = "Age Group",
       y = "%")

# boxplot of age for each group 
# sex
s_data %>%
  filter(suicide_factor == "yes") %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = gender, y = age)) +
  labs(title = "Boxplot of Age of Suicide Deaths for Maricopa County",
       subtitle = "For Year 2019",
       caption = "",
       x = "Gender",
       y = "Age")

# race 
s_data %>%
  drop_na() %>%
  filter(race != "OTHER/UNKNOWN") %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = race, y = age)) +
  labs(title = "Boxplot of Age of Suicide Deaths for Maricopa County",
       subtitle = "For Year 2019",
       caption = "",
       x = "Race",
       y = "Age")

# race and sex 
s_data %>%
  drop_na() %>%
  filter(race != "OTHER/UNKNOWN") %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = race, y = age, fill = gender)) +
  labs(title = "Boxplot of Age of Suicide Deaths for Maricopa County",
       subtitle = "For Year 2019",
       caption = "",
       x = "Race",
       y = "Age")

# plot suicide binary grouped by race 
# raw numbers 
suicide_death_by_race <- s_data %>%
  group_by(race, suicide_factor) %>%
  count()

suicide_death_by_race %>%
  filter(race != "OTHER/UNKNOWN",
         suicide_factor == "yes") %>%
  ggplot() +
  geom_bar(mapping = aes(x = reorder(suicide_factor, n), y = n, fill = race), position = "dodge", stat = "identity") +
  coord_flip()

# age 
s_data %>%
  filter(manner_of_death == "SUICIDE") %>%
  summarise(
    age_min = min(age),
    age_average = mean(age),
    age_median = median(age),
    age_max = max(age)
  ) 

# age by race
s_data %>%
  filter(manner_of_death == "SUICIDE",
         race != "OTHER/UNKNOWN") %>%
  group_by(race, gender) %>%
  summarise(
    age_min = min(age),
    age_average = mean(age),
    age_median = median(age),
    age_max = max(age)
  ) %>%
  kable

# suicide by age group
# first need to reverse the factor sort level
s_data_rev_age <- mutate(s_data, age_group = fct_rev(s_data$age_group))

# now the plot 
s_data_rev_age %>%
  filter(manner_of_death == "SUICIDE") %>%
  group_by(age_group) %>%
  count() %>%
  drop_na() %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(age_group, n), y = n)) +
  coord_flip()

# visualization 4 ####
plot_suicide_death_rate_by_age <- suicide_death_rate_by_age %>%
  drop_na() %>%
  ggplot(mapping = aes(x = reorder(age_group, crude_rate), y = crude_rate)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "black", fill = "#0E081B") +
  geom_hline(yintercept = 15.2, color = "blue", show.legend = TRUE, size = 3, alpha = 0.8) +
  coord_flip() +
  geom_text(
    label="County: 15.2", 
    x=1,
    y=19,
    color = "blue",
    size = 12
  ) +
  labs(title = "Death Rate by Age Group for Maricopa County, AZ in 2019",
       subtitle = "",
       caption = "*Source: Centers for Disease Control and Prevention, National Center for Health Statistics.",
       x = "Age group",
       y = "Crude Rate* per 100,000") +
  theme_linedraw() +
  theme(
    plot.background = element_rect(fill = "#F7F5FC"),
    panel.border = element_rect(color = "black"),
    text = element_text(size = 32)
  )

plot_suicide_death_rate_by_age

ggsave(filename = "graphics/plot_suicide_death_rate_by_age.png",
       plot = plot_suicide_death_rate_by_age,
       device = "png",
       scale = 1,
       width = 1920,
       height = 1080,
       units = "px",
       dpi = "screen")

write_rds(plot_suicide_death_rate_by_age, "data/tidy/plot_suicide_rate_by_age.rds")

# time series
# daily 
s_data %>%
  filter(suicide_factor == "yes") %>%
  group_by(date) %>%
  count() %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = n))

s_data %>%
  filter(suicide_factor == "yes",
         race != "OTHER/UNKNOWN") %>%
  group_by(date, race) %>%
  count() %>%
  ggplot() +
  geom_density(mapping = aes(x = date, fill = race), alpha = 0.5)

s_data %>%
  filter(suicide_factor == "yes",
         race != "OTHER/UNKNOWN") %>%
  group_by(date, race) %>%
  count() %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = n, color = race))

# monthly
s_data %>%
  filter(suicide_factor == "yes",
         race != "OTHER/UNKNOWN") %>%
  group_by(deathmonth, race) %>%
  count() %>%
  ggplot() +
  geom_line(mapping = aes(x = deathmonth, y = n, color = race))

s_data %>%
  filter(suicide_factor == "yes",
         race != "OTHER/UNKNOWN") %>%
  group_by(deathmonth, race) %>%
  count() %>%
  ggplot() +
  geom_density(mapping = aes(x = deathmonth, fill = race), alpha = 0.5)

