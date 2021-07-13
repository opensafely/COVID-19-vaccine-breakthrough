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

## Format groups
data_processed <- data_processed %>%
  mutate(group = ifelse(care_home_65plus == 1, 1, NA),
         group = ifelse(is.na(group) & ageband == 3, 2, group),
         group = ifelse(is.na(group) & hscworker == 1, 3, group),
         group = ifelse(is.na(group) & ageband == 2, 4, group),
         group = ifelse(is.na(group) & shielded == 1, 5, group),
         group = ifelse(is.na(group), 6, group))


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
datasets <- list(data_processed, 
                 data_processed %>% filter(covid_positive_post_2vacc == 1),
                 data_processed %>% filter(covid_hospital_admission == 1),
                 data_processed %>% filter(covid_hospitalisation_critical_care == 1),
                 data_processed %>% filter(covid_death == 1))

for (i in 1:length(datasets)) {
  
  data <- datasets[[i]]
  results.table[1,i+1] <- nrow(data)
  results.table[2,i+1] <- nrow(data %>% filter(group == 1))
  results.table[3,i+1] <- nrow(data %>% filter(group == 2))
  results.table[4,i+1] <- nrow(data %>% filter(group == 3))
  results.table[5,i+1] <- nrow(data %>% filter(group == 4))
  results.table[6,i+1] <- nrow(data %>% filter(group == 5))
  results.table[7,i+1] <- nrow(data %>% filter(group == 6))
  
}

# Redaction ----

## Redact values <=5
results.table_redacted <- results.table %>% 
  mutate_all(~na_if(., 0)) %>%
  mutate_all(~na_if(., 1)) %>%
  mutate_all(~na_if(., 2)) %>%
  mutate_all(~na_if(., 3)) %>%
  mutate_all(~na_if(., 4)) %>%
  mutate_all(~na_if(., 5)) %>%
  mutate_all(~na_if(., 6)) %>%
  mutate_all(~na_if(., 7))

## Round to nearest 5
results.table_redacted <- results.table_redacted %>%
  select(-Group) %>%
  mutate_all(~plyr::round_any(., 5)) %>%
  mutate(Group = c("All", 
                   "Care home (priority group 1)",
                   "80+ (priority group 2)",
                   "Health / care workers (priority group 1-2)", 
                   "70-79 (priority group 3-4)",
                   "Shielding (age 16-69) (priority group 4)",
                   "Others not in the above groups")) %>%
  select(Group, "Fully vaccinated", "Positive COVID test", "Hospitalised with COVID", "COVID Deaths")

## Recalculate column totals
results.table_redacted[1, "Positive COVID test"] <- sum(results.table_redacted[-1,]$`Positive COVID test`, na.rm = T)
results.table_redacted[1, "Hospitalised with COVID"] <- sum(results.table_redacted[-1,]$`Hospitalised with COVID`, na.rm = T)
results.table_redacted[1, "COVID Deaths"] <- sum(results.table_redacted[-1,]$`COVID Deaths`, na.rm = T)

## Replace na with [REDACTED]
results.table_redacted <- results.table_redacted %>% 
  replace(is.na(.), "[REDACTED]")

# Save as html ----
gt::gtsave(gt(results.table), here::here("output","tables", "table1.html"))
gt::gtsave(gt(results.table_redacted), here::here("output","tables", "table1_redacted.html"))



