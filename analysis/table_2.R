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
results.table <- data.frame(matrix(nrow = 27, ncol = 6))
colnames(results.table) <- c("Group","Fully vaccinated", "Positive COVID test", "Hospitalised with COVID", "Critical care with COVID", "COVID Deaths")
results.table[1:27,1] <- c("All", 
                          "Immunocompromised",
                          "Time since 2nd dose (2-4 weeks)",
                          "Time since 2nd dose (4-6 weeks)",
                          "Time since 2nd dose (6-8 weeks)",
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
datasets <- list(data_processed, 
                 data_processed %>% filter(covid_positive_post_2vacc == 1),
                 data_processed %>% filter(covid_hospital_admission == 1),
                 data_processed %>% filter(covid_hospitalisation_critical_care == 1),
                 data_processed %>% filter(covid_death == 1))

for (i in 1:length(datasets)) {
  
  data <- datasets[[i]]
  results.table[1,i+1] <- nrow(data)
  results.table[2,i+1] <- nrow(data %>% filter(immunosuppression == 1))
  results.table[3,i+1] <- nrow(data %>% filter(follow_up_time >= 14 & follow_up_time < 24))
  results.table[4,i+1] <- nrow(data %>% filter(follow_up_time >= 24 & follow_up_time < 42))
  results.table[5,i+1] <- nrow(data %>% filter(follow_up_time >= 42 & follow_up_time < 56))
  results.table[6,i+1] <- nrow(data %>% filter(follow_up_time >= 56))
  results.table[7,i+1] <- nrow(data %>% filter(region == "London"))
  results.table[8,i+1] <- nrow(data %>% filter(region == "East of England"))
  results.table[9,i+1] <- nrow(data %>% filter(region == "East Midlands"))
  results.table[10,i+1] <- nrow(data %>% filter(region == "North East"))
  results.table[11,i+1] <- nrow(data %>% filter(region == "North West"))
  results.table[12,i+1] <- nrow(data %>% filter(region == "South East"))
  results.table[13,i+1] <- nrow(data %>% filter(region == "South West"))
  results.table[14,i+1] <- nrow(data %>% filter(region == "West Midlands"))
  results.table[15,i+1] <- nrow(data %>% filter(region == "Yorkshire and the Humber"))
  results.table[16,i+1] <- nrow(data %>% filter(ethnicity == "White"))
  results.table[17,i+1] <- nrow(data %>% filter(ethnicity == "Asian or Asian British"))
  results.table[18,i+1] <- nrow(data %>% filter(ethnicity == "Black or Black British"))
  results.table[19,i+1] <- nrow(data %>% filter(ethnicity %in% c("Mixed", "Other ethnic groups", "Unknown")))
  results.table[20,i+1] <- nrow(data %>% filter(learning_disability == 1))
  results.table[21,i+1] <- nrow(data %>% filter(organ_transplant == 1))
  results.table[22,i+1] <- nrow(data %>% filter(ckd == 1))
  results.table[23,i+1] <- nrow(data %>% filter(ageband2 == "16-79"))
  results.table[24,i+1] <- nrow(data %>% filter(ageband2 == "80-84"))
  results.table[25,i+1] <- nrow(data %>% filter(ageband2 == "85-89"))
  results.table[26,i+1] <- nrow(data %>% filter(ageband2 == "90-94"))
  results.table[27,i+1] <- nrow(data %>% filter(ageband2 == "95+"))
}

# Save as html ----
gt::gtsave(gt(results.table), here::here("output","tables", "table2.html"))




