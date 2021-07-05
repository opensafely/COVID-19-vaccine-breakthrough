######################################

# This script 
# - produces a table with the number of patients fully vaccinated (2 doses + 2 weeks) in initial priority groups, 
# - and the number of patients with each outcome.
# - saves table as html

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('lubridate')
library('reshape2')
library('here')
library('gt')

## Create output directory
dir.create(here::here("output", "tables"), showWarnings = FALSE, recursive=TRUE)

## Import data
data_processed <- read_rds(here::here("output", "data", "data_all.rds"))


# Table 1 shell ----
results.table <- data.frame(matrix(nrow = 7, ncol = 6))
colnames(results.table) <- c("Group","Fully vaccinated", "Positive COVID test", "Hospitalised with COVID", "Critical care with COVID", "COVID Deaths")
results.table[1:7,1] <- c("All", 
                           "Care home (priority group 1)",
                           "80+ (priority group 2)",
                           "Health / care workers (priority group 1-2)", 
                           "70-79 (priority group 3-4)",
                           "Shielding (age 16-69) (priority group 4)",
                           "Others not in the above groups")

# Fill in table ----

## Fully vaccinated
data <- data_processed
results.table[1,2] <- nrow(data)
results.table[2,2] <- nrow(data %>% filter(care_home_65plus == 1))
results.table[3,2] <- nrow(data %>% filter(ageband == 3))
results.table[4,2] <- nrow(data %>% filter(hscworker == 1))
results.table[5,2] <- nrow(data %>% filter(ageband == 2))
results.table[6,2] <- nrow(data %>% filter(shielded == 1))
results.table[7,2] <- results.table[1,2] - nrow(data %>% filter(care_home_65plus == 0,
                                                                shielded == 0,
                                                                !ageband %in% c(2,3),
                                                                hscworker == 0))

## Positive COVID test
data <- data_processed %>% filter(covid_positive_post_2vacc == 1)
results.table[1,3] <- nrow(data)
results.table[2,3] <- nrow(data %>% filter(care_home_65plus == 1))
results.table[3,3] <- nrow(data %>% filter(ageband == 3))
results.table[4,3] <- nrow(data %>% filter(hscworker == 1))
results.table[5,3] <- nrow(data %>% filter(ageband == 2))
results.table[6,3] <- nrow(data %>% filter(shielded == 1))
results.table[7,3] <- nrow(data %>% filter(care_home_65plus == 0,
                                                 shielded == 0,
                                                 !ageband %in% c(2,3),
                                                 hscworker == 0))

## Hospitalised with COVID
data <- data_processed %>% filter(covid_hospital_admission == 1)
results.table[1,4] <- nrow(data)
results.table[2,4] <- nrow(data %>% filter(care_home_65plus == 1))
results.table[3,4] <- nrow(data %>% filter(ageband == 3))
results.table[4,4] <- nrow(data %>% filter(hscworker == 1))
results.table[5,4] <- nrow(data %>% filter(ageband == 2))
results.table[6,4] <- nrow(data %>% filter(shielded == 1))
results.table[7,4] <- nrow(data %>% filter(care_home_65plus == 0,
                                                 shielded == 0,
                                                 !ageband %in% c(2,3),
                                                 hscworker == 0))

## Critical care with COVID
data <- data_processed %>% filter(covid_hospitalisation_critical_care == 1)
results.table[1,5] <- nrow(data)
results.table[2,5] <- nrow(data %>% filter(care_home_65plus == 1))
results.table[3,5] <- nrow(data %>% filter(ageband == 3))
results.table[4,5] <- nrow(data %>% filter(hscworker == 1))
results.table[5,5] <- nrow(data %>% filter(ageband == 2))
results.table[6,5] <- nrow(data %>% filter(shielded == 1))
results.table[7,5] <- nrow(data %>% filter(care_home_65plus == 0,
                                                 shielded == 0,
                                                 !ageband %in% c(2,3),
                                                 hscworker == 0))

## COVID Deaths
data <- data_processed %>% filter(covid_death == 1)
results.table[1,6] <- nrow(data)
results.table[2,6] <- nrow(data %>% filter(care_home_65plus == 1))
results.table[3,6] <- nrow(data %>% filter(ageband == 3))
results.table[4,6] <- nrow(data %>% filter(hscworker == 1))
results.table[5,6] <- nrow(data %>% filter(ageband == 2))
results.table[6,6] <- nrow(data %>% filter(shielded == 1))
results.table[7,6] <- nrow(data %>% filter(care_home_65plus == 0,
                                           shielded == 0,
                                           !ageband %in% c(2,3),
                                           hscworker == 0))


# Save as html ----
gt::gtsave(gt(results.table), here::here("output","tables", "table1.html"))






