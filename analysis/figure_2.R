######################################

# This script 
# - produces a table with the number of tests taken in initial priority groups, 
# - and plots a histogram of these

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('lubridate')
library('reshape2')
library('here')
library('gt')

## Create output directory
dir.create(here::here("output", "figures"), showWarnings = FALSE, recursive=TRUE)

## Import data
data <- read_rds(here::here("output", "data", "data_processed.rds"))
data_trial <- read_rds(here::here("output", "data", "data_processed_trial.rds"))

## Format groups
data <- data %>%
  mutate(group = ifelse(care_home_65plus == 1, 1, NA),
         group = ifelse(is.na(group) & ageband == 3, 2, group),
         group = ifelse(is.na(group) & hscworker == 1, 3, group),
         group = ifelse(is.na(group) & ageband == 2, 4, group),
         group = ifelse(is.na(group) & shielded == 1, 5, group),
         group = ifelse(is.na(group) & age >=50 & age <70, 6, group),
         group = ifelse(is.na(group), 7, group)) %>%
  group_by(patient_id) %>%
  mutate(follow_up_time =  min((follow_up_time_vax2 - 14), 
                               time_to_positive_test,
                               time_to_hospitalisation,
                               time_to_covid_death)) %>%
  ungroup()

data_trial <- data_trial %>%
  mutate(group = 8) %>%
  group_by(patient_id) %>%
  mutate(follow_up_time =  min((follow_up_time_vax2 - 14), 
                               time_to_positive_test,
                               time_to_hospitalisation,
                               time_to_covid_death)) %>%
  ungroup()

data_processed = rbind(data, data_trial) %>%
  mutate(group = as.factor(group))


# Counts of tests ----
population = rbind(data_processed %>%
                     count() %>%
                     mutate(group = 0),
                   data_processed %>%
                     group_by(group) %>%
                     count()) %>%
  mutate(Group = factor(group, labels = c("All", 
                                          "Care home (priority group 1)",
                                          "80+ (priority group 2)",
                                          "Health / care workers (priority groups 1-2)", 
                                          "70-79 (priority groups 3-4)",
                                          "Shielding (age 16-69) (priority group 4)",
                                          "50-69 (priority groups 5-9)",
                                          "Others not in the above groups",
                                          "Trial")))

test_counts_all <- data_processed %>%
  select(patient_id, group, tests_conducted_any, tests_conducted_positive) %>%
  mutate(tests_conducted_any = ifelse(is.na(tests_conducted_any), 0, tests_conducted_any),
         tests_conducted_positive = ifelse(is.na(tests_conducted_positive), 0, tests_conducted_positive)) %>%
  mutate(group = 0)

test_counts_groups <- data_processed %>%
  select(patient_id, group, tests_conducted_any, tests_conducted_positive) %>%
  mutate(tests_conducted_any = ifelse(is.na(tests_conducted_any), 0, tests_conducted_any),
         tests_conducted_positive = ifelse(is.na(tests_conducted_positive), 0, tests_conducted_positive))

test_counts_any <- rbind(test_counts_all, test_counts_groups) %>%
  mutate(Group = factor(group, labels = c("All", 
                                          "Care home (priority group 1)",
                                          "80+ (priority group 2)",
                                          "Health / care workers (priority groups 1-2)", 
                                          "70-79 (priority groups 3-4)",
                                          "Shielding (age 16-69) (priority group 4)",
                                          "50-69 (priority groups 5-9)",
                                          "Others not in the above groups",
                                          "Trial"))) %>%
  group_by(Group) %>%
  count(tests_conducted_any) %>%
  left_join(population, by = "Group") %>%
  mutate(n.x = ifelse(n.x < 8, NA, n.x),
         n.x = plyr::round_any(n.x, 5),
         n.y = plyr::round_any(n.y, 5),
         Test = "Positive or negative") %>%
  mutate(perc = round(n.x/n.y*100, digits = 1))


# Histogram of tests ----
test_hist_any <- ggplot(test_counts_any, aes(x = tests_conducted_any, y = perc, fill = Group)) +
  geom_bar(stat="identity") +
  facet_wrap(~Group) +
  scale_fill_discrete(guide="none") +
  theme_bw() +
  labs(x = "Number of (positive and/or negative) SARS-CoV-2 swab tests taken since being fully vaccinated", y = "%")


## Save plot
ggsave(
  here::here("output", "figures", "figure2.png"),
  test_hist_any,
  units = "cm", width = 20, height = 20
)

