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

## Import data
data_processed <- read_rds(here::here("output", "data", "data_all.rds"))

## Create output directory
dir.create(here::here("output", "tables"), showWarnings = FALSE, recursive=TRUE)

# Table 2 shell ----
results.table <- data.frame(matrix(nrow = 26, ncol = 6))
colnames(results.table) <- c("Group","Fully vaccinated", "Positive COVID test", "Hospitalised with COVID", "Critical care with COVID", "COVID Deaths")
results.table[1:26,1] <- c("All", 
                          "Immunocompromised",
                          "Time since 2nd dose (2-4 weeks)",
                          "Time since 2nd dose (4-8 weeks)",
                          "Time since 2nd dose (8+ weeks)",
                          "Region - London", 
                          "Region - East of England", 
                          "Region - East Midlands", 
                          "Region - North East", 
                          "Region - North West", 
                          "Region - South East", 
                          "Region - South West", 
                          "Region - West Midlands", 
                          "Region - Yorkshire and The Humber", 
                          "Ethnicity - White",
                          "Ethnicity - Asian or Asian British",
                          "Ethnicity - Black or Black British",
                          "Ethnicity - Mixed/Other ethnic groups/Unknown",
                          "Learning disability",
                          "Organ transplant",
                          "Dialysis / Kidney disease",
                          "Age - <80 ",
                          "Age - 80 - 84",
                          "Age - 85 - 89",
                          "Age - 90 - 94",
                          "Age - 95+")

# Fill in table ----

## Fully vaccinated
data <- data_processed
print((data %>% group_by(region, .drop = FALSE) %>% tally())$n)
unique(data$region)

results.table[1,2] <- nrow(data)
results.table[2,2] <- nrow(data %>% filter(immunosuppression == 1))
results.table[3,2] <- nrow(data %>% filter(follow_up_time >= 14 & follow_up_time < 31))
results.table[4,2] <- nrow(data %>% filter(follow_up_time >= 31 & follow_up_time < 62))
results.table[5,2] <- nrow(data %>% filter(follow_up_time >= 62))
results.table[6:14,2] <- (data %>% group_by(region, .drop = FALSE) %>% tally())$n
results.table[15,2] <- nrow(data %>% filter(ethnicity == "White"))
results.table[16,2] <- nrow(data %>% filter(ethnicity == "Asian or Asian British"))
results.table[17,2] <- nrow(data %>% filter(ethnicity == "Black or Black British"))
results.table[18,2] <- nrow(data %>% filter(ethnicity %in% c("Mixed", "Other ethnic groups", "Unknown")))
results.table[19,2] <- nrow(data %>% filter(learning_disability == 1))
results.table[20,2] <- nrow(data %>% filter(organ_transplant == 1))
results.table[21,2] <- nrow(data %>% filter(ckd == 1))
results.table[22,2] <- nrow(data %>% filter(ageband2 == "16-79"))
results.table[23,2] <- nrow(data %>% filter(ageband2 == "80-84"))
results.table[24,2] <- nrow(data %>% filter(ageband2 == "85-89"))
results.table[25,2] <- nrow(data %>% filter(ageband2 == "90-94"))
results.table[26,2] <- nrow(data %>% filter(ageband2 == "95+"))

## Positive COVID test
data <- data_processed %>% filter(covid_positive_post_2vacc == 1)
results.table[1,3] <- nrow(data)
results.table[2,3] <- nrow(data %>% filter(immunosuppression == 1))
results.table[3,3] <- nrow(data %>% filter(follow_up_time >= 14 & follow_up_time < 31))
results.table[4,3] <- nrow(data %>% filter(follow_up_time >= 31 & follow_up_time < 62))
results.table[5,3] <- nrow(data %>% filter(follow_up_time >= 62))
results.table[6:14,3] <- (data %>% group_by(region, .drop = FALSE) %>% tally())$n
results.table[15,3] <- nrow(data %>% filter(ethnicity == "White"))
results.table[16,3] <- nrow(data %>% filter(ethnicity == "Asian or Asian British"))
results.table[17,3] <- nrow(data %>% filter(ethnicity == "Black or Black British"))
results.table[18,3] <- nrow(data %>% filter(ethnicity %in% c("Mixed", "Other ethnic groups", "Unknown")))
results.table[19,3] <- nrow(data %>% filter(learning_disability == 1))
results.table[20,3] <- nrow(data %>% filter(organ_transplant == 1))
results.table[21,3] <- nrow(data %>% filter(ckd == 1))
results.table[22,3] <- nrow(data %>% filter(ageband2 == "16-79"))
results.table[23,3] <- nrow(data %>% filter(ageband2 == "80-84"))
results.table[24,3] <- nrow(data %>% filter(ageband2 == "85-89"))
results.table[25,3] <- nrow(data %>% filter(ageband2 == "90-94"))
results.table[26,3] <- nrow(data %>% filter(ageband2 == "95+"))

## Hospitalised with COVID
data <- data_processed %>% filter(covid_hospital_admission == 1)
results.table[1,4] <- nrow(data)
results.table[2,4] <- nrow(data %>% filter(immunosuppression == 1))
results.table[3,4] <- nrow(data %>% filter(follow_up_time >= 14 & follow_up_time < 31))
results.table[4,4] <- nrow(data %>% filter(follow_up_time >= 31 & follow_up_time < 62))
results.table[5,4] <- nrow(data %>% filter(follow_up_time >= 62))
results.table[6:14,4] <- (data %>% group_by(region, .drop = FALSE) %>% tally())$n
results.table[15,4] <- nrow(data %>% filter(ethnicity == "White"))
results.table[16,4] <- nrow(data %>% filter(ethnicity == "Asian or Asian British"))
results.table[17,4] <- nrow(data %>% filter(ethnicity == "Black or Black British"))
results.table[18,4] <- nrow(data %>% filter(ethnicity %in% c("Mixed", "Other ethnic groups", "Unknown")))
results.table[19,4] <- nrow(data %>% filter(learning_disability == 1))
results.table[20,4] <- nrow(data %>% filter(organ_transplant == 1))
results.table[21,4] <- nrow(data %>% filter(ckd == 1))
results.table[22,4] <- nrow(data %>% filter(ageband2 == "16-79"))
results.table[23,4] <- nrow(data %>% filter(ageband2 == "80-84"))
results.table[24,4] <- nrow(data %>% filter(ageband2 == "85-89"))
results.table[25,4] <- nrow(data %>% filter(ageband2 == "90-94"))
results.table[26,4] <- nrow(data %>% filter(ageband2 == "95+"))

## Critical care with COVID
data <- data_processed %>% filter(covid_hospitalisation_critical_care == 1)
results.table[1,5] <- nrow(data)
results.table[2,5] <- nrow(data %>% filter(immunosuppression == 1))
results.table[3,5] <- nrow(data %>% filter(follow_up_time >= 14 & follow_up_time < 31))
results.table[4,5] <- nrow(data %>% filter(follow_up_time >= 31 & follow_up_time < 62))
results.table[5,5] <- nrow(data %>% filter(follow_up_time >= 62))
results.table[6:14,5] <- (data %>% group_by(region, .drop = FALSE) %>% tally())$n
results.table[15,5] <- nrow(data %>% filter(ethnicity == "White"))
results.table[16,5] <- nrow(data %>% filter(ethnicity == "Asian or Asian British"))
results.table[17,5] <- nrow(data %>% filter(ethnicity == "Black or Black British"))
results.table[18,5] <- nrow(data %>% filter(ethnicity %in% c("Mixed", "Other ethnic groups", "Unknown")))
results.table[19,5] <- nrow(data %>% filter(learning_disability == 1))
results.table[20,5] <- nrow(data %>% filter(organ_transplant == 1))
results.table[21,5] <- nrow(data %>% filter(ckd == 1))
results.table[22,5] <- nrow(data %>% filter(ageband2 == "16-79"))
results.table[23,5] <- nrow(data %>% filter(ageband2 == "80-84"))
results.table[24,5] <- nrow(data %>% filter(ageband2 == "85-89"))
results.table[25,5] <- nrow(data %>% filter(ageband2 == "90-94"))
results.table[26,5] <- nrow(data %>% filter(ageband2 == "95+"))

## COVID Deaths
data <- data_processed %>% filter(covid_death == 1)
results.table[1,6] <- nrow(data)
results.table[2,6] <- nrow(data %>% filter(immunosuppression == 1))
results.table[3,6] <- nrow(data %>% filter(follow_up_time >= 14 & follow_up_time < 31))
results.table[4,6] <- nrow(data %>% filter(follow_up_time >= 31 & follow_up_time < 62))
results.table[5,6] <- nrow(data %>% filter(follow_up_time >= 62))
results.table[6:14,6] <- (data %>% group_by(region, .drop = FALSE) %>% tally())$n
results.table[15,6] <- nrow(data %>% filter(ethnicity == "White"))
results.table[16,6] <- nrow(data %>% filter(ethnicity == "Asian or Asian British"))
results.table[17,6] <- nrow(data %>% filter(ethnicity == "Black or Black British"))
results.table[18,6] <- nrow(data %>% filter(ethnicity %in% c("Mixed", "Other ethnic groups", "Unknown")))
results.table[19,6] <- nrow(data %>% filter(learning_disability == 1))
results.table[20,6] <- nrow(data %>% filter(organ_transplant == 1))
results.table[21,6] <- nrow(data %>% filter(ckd == 1))
results.table[22,6] <- nrow(data %>% filter(ageband2 == "16-79"))
results.table[23,6] <- nrow(data %>% filter(ageband2 == "80-84"))
results.table[24,6] <- nrow(data %>% filter(ageband2 == "85-89"))
results.table[25,6] <- nrow(data %>% filter(ageband2 == "90-94"))
results.table[26,6] <- nrow(data %>% filter(ageband2 == "95+"))

# Save as html ----
gt::gtsave(gt(results.table), here::here("output","tables", "table2.html"))






