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
         group = ifelse(is.na(group) & age >=50 & age <70, 6, group),
         group = ifelse(is.na(group), 7, group))


# Table 1 shell ----
results.table <- data.frame(matrix(nrow = 8, ncol = 22))
colnames(results.table) <- c("Group","Fully vaccinated", 
                             "Positive COVID test", "count_1", "rate_1", "lci_1", "uci_1", 
                             "Hospitalised with COVID", "count_2", "rate_2", "lci_2", "uci_2",
                             "Critical care with COVID", "count_3", "rate_3", "lci_3", "uci_3",
                             "COVID Deaths", "count_4", "rate_4", "lci_4", "uci_4")
results.table[1:8,1] <- c("All", 
                          "Care home (priority group 1)",
                          "80+ (priority group 2)",
                          "Health / care workers (priority groups 1-2)", 
                          "70-79 (priority groups 3-4)",
                          "Shielding (age 16-69) (priority group 4)",
                          "50-69 (priority groups 5-9)",
                          "Others not in the above groups")

# Fill in table ----
datasets <- list(data_processed %>% filter(covid_positive_post_2vacc == 1),
                 data_processed %>% filter(covid_hospital_admission == 1),
                 data_processed %>% filter(covid_hospitalisation_critical_care == 1),
                 data_processed %>% filter(covid_death == 1))

## Fully vaccinated
results.table[1,2] <- nrow(data_processed)
results.table[2,2] <- nrow(data_processed %>% filter(group == 1))
results.table[3,2] <- nrow(data_processed %>% filter(group == 2))
results.table[4,2] <- nrow(data_processed %>% filter(group == 3))
results.table[5,2] <- nrow(data_processed %>% filter(group == 4))
results.table[6,2] <- nrow(data_processed %>% filter(group == 5))
results.table[7,2] <- nrow(data_processed %>% filter(group == 6))
results.table[8,2] <- nrow(data_processed %>% filter(group == 7))

## Other outcomes
for (i in 1:length(datasets)) {
  
  # Select dataset
  data <- datasets[[i]]
  
  # Counts
  results.table[1,(5*i - 2)] <- nrow(data)
  results.table[2,(5*i - 2)] <- nrow(data %>% filter(group == 1))
  results.table[3,(5*i - 2)] <- nrow(data %>% filter(group == 2))
  results.table[4,(5*i - 2)] <- nrow(data %>% filter(group == 3))
  results.table[5,(5*i - 2)] <- nrow(data %>% filter(group == 4))
  results.table[6,(5*i - 2)] <- nrow(data %>% filter(group == 5))
  results.table[7,(5*i - 2)] <- nrow(data %>% filter(group == 6))
  results.table[8,(5*i - 2)] <- nrow(data %>% filter(group == 7))
  
  # Counts (as %)
  results.table[1:8,(5*i - 1)] <- round((results.table[1:8,(5*i - 2)]/results.table[1:8,2])*100, digits = 2)
  
  # Rates
  Y = 100000
  dig = 2
  
  results.table[1,((5*i):(5*i + 2))] <- data %>%
    summarise(
      n_postest = ifelse(i == 1, sum(covid_positive_post_2vacc), 
                         ifelse(i == 2, sum(covid_hospital_admission),
                                ifelse(i == 3, sum(covid_hospitalisation_critical_care), 
                                       sum(covid_death)))),
      person_time = ifelse(i == 1, sum(time_to_positive_test), 
                           ifelse(i == 2, sum(time_to_hospitalisation),
                                  ifelse(i == 3, sum(time_to_itu), 
                                         sum(time_to_covid_death))))
    ) %>% 
    ungroup() %>%
    mutate(rate = n_postest/person_time,
           lower = ifelse(rate - qnorm(0.975)*(sqrt(n_postest/(person_time^2))) < 0, 0, 
                          rate - qnorm(0.975)*(sqrt(n_postest/(person_time^2)))),
           upper = ifelse(rate + qnorm(0.975)*(sqrt(n_postest/(person_time^2))) < 0, 0, 
                          rate + qnorm(0.975)*(sqrt(n_postest/(person_time^2)))),
           Rate_py = round(rate/365.25*Y, digits = 2),
           lower_py = round(lower/365.25*Y, digits = 2),
           upper_py = round(upper/365.25*Y, digits = 2)) %>%
    select(Rate_py, lower_py, upper_py)
  
  print(results.table[2:8,(5*i):(5*i + 2)])
  
  print(data %>%
          group_by(group) %>%
          summarise(
            n_postest = ifelse(i == 1, sum(covid_positive_post_2vacc), 
                               ifelse(i == 2, sum(covid_hospital_admission),
                                      ifelse(i == 3, sum(covid_hospitalisation_critical_care), 
                                             sum(covid_death)))),
            person_time = ifelse(i == 1, sum(time_to_positive_test), 
                                 ifelse(i == 2, sum(time_to_hospitalisation),
                                        ifelse(i == 3, sum(time_to_itu), 
                                               sum(time_to_covid_death))))
          ) %>% 
          ungroup() %>%
          mutate(rate = n_postest/person_time,
                 lower = ifelse(rate - qnorm(0.975)*(sqrt(n_postest/(person_time^2))) < 0, 0, 
                                rate - qnorm(0.975)*(sqrt(n_postest/(person_time^2)))),
                 upper = ifelse(rate + qnorm(0.975)*(sqrt(n_postest/(person_time^2))) < 0, 0, 
                                rate + qnorm(0.975)*(sqrt(n_postest/(person_time^2)))),
                 Rate_py = round(rate/365.25*Y, digits = 2),
                 lower_py = round(lower/365.25*Y, digits = 2),
                 upper_py = round(upper/365.25*Y, digits = 2)) %>%
          select(n_postest, person_time, Rate_py, lower_py, upper_py))
  
  # results.table[2:8,(5*i):(5*i + 2)] <- data %>%
  #   group_by(group) %>%
  #   summarise(
  #     n_postest = ifelse(i == 1, sum(covid_positive_post_2vacc), 
  #                        ifelse(i == 2, sum(covid_hospital_admission),
  #                               ifelse(i == 3, sum(covid_hospitalisation_critical_care), 
  #                                      sum(covid_death)))),
  #     person_time = ifelse(i == 1, sum(time_to_positive_test), 
  #                          ifelse(i == 2, sum(time_to_hospitalisation),
  #                                 ifelse(i == 3, sum(time_to_itu), 
  #                                        sum(time_to_covid_death))))
  #   ) %>% 
  #   ungroup() %>%
  #   mutate(rate = n_postest/person_time,
  #          lower = ifelse(rate - qnorm(0.975)*(sqrt(n_postest/(person_time^2))) < 0, 0, 
  #                         rate - qnorm(0.975)*(sqrt(n_postest/(person_time^2)))),
  #          upper = ifelse(rate + qnorm(0.975)*(sqrt(n_postest/(person_time^2))) < 0, 0, 
  #                         rate + qnorm(0.975)*(sqrt(n_postest/(person_time^2)))),
  #          Rate_py = round(rate/365.25*Y, digits = 2),
  #          lower_py = round(lower/365.25*Y, digits = 2),
  #          upper_py = round(upper/365.25*Y, digits = 2)) %>%
  #   select(Rate_py, lower_py, upper_py)
  
  
  print(i)
  
  
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
                   "50-69 (priority groups 5-9)",
                   "Others not in the above groups")) %>%
  select(Group, "Fully vaccinated", 
         "Positive COVID test", "count_1", "rate_1", "lci_1", "uci_1",
         "Hospitalised with COVID", "count_2", "rate_2", "lci_2", "uci_2",
         "COVID Deaths", "count_4", "rate_4", "lci_4", "uci_4")

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



