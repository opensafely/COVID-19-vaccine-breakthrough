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


# Table 1 shell ----
results.table <- data.frame(matrix(nrow = 9, ncol = 22))
colnames(results.table) <- c("Group","Fully vaccinated", 
                             "Positive COVID test", "PYs_1", "rate_1", "lci_1", "uci_1", 
                             "Hospitalised with COVID", "PYs_2", "rate_2", "lci_2", "uci_2",
                             "Critical care with COVID", "PYs_3", "rate_3", "lci_3", "uci_3",
                             "COVID Deaths", "PYs_4", "rate_4", "lci_4", "uci_4")
results.table[1:9,1] <- c("All", 
                          "Care home (priority group 1)",
                          "80+ (priority group 2)",
                          "Health / care workers (priority groups 1-2)", 
                          "70-79 (priority groups 3-4)",
                          "Shielding (age 16-69) (priority group 4)",
                          "50-69 (priority groups 5-9)",
                          "Others not in the above groups",
                          "Trial")

# Fill in table ----

## Fully vaccinated
results.table[1,2] <- nrow(data_processed)
results.table[2,2] <- nrow(data_processed %>% filter(group == 1))
results.table[3,2] <- nrow(data_processed %>% filter(group == 2))
results.table[4,2] <- nrow(data_processed %>% filter(group == 3))
results.table[5,2] <- nrow(data_processed %>% filter(group == 4))
results.table[6,2] <- nrow(data_processed %>% filter(group == 5))
results.table[7,2] <- nrow(data_processed %>% filter(group == 6))
results.table[8,2] <- nrow(data_processed %>% filter(group == 7))
results.table[9,2] <- nrow(data_processed %>% filter(group == 8))

## Other outcomes
for (i in 1:4) {
  
  # Counts and rates
  Y = 1000
  dig = 2
  
  results.table[1,((5*i - 2):(5*i + 2))] <- data %>%
    summarise(
      n_postest = ifelse(i == 1, sum(covid_positive_test), 
                         ifelse(i == 2, sum(covid_hospital_admission),
                                ifelse(i == 3, sum(covid_hospitalisation_critical_care), 
                                       sum(covid_death)))),
      person_time = ifelse(i == 1, sum(time_to_positive_test), 
                           ifelse(i == 2, sum(time_to_hospitalisation),
                                  ifelse(i == 3, sum(time_to_itu), 
                                         sum(time_to_covid_death))))
    ) %>% 
    ungroup() %>%
    mutate(person_time = person_time/365.25,
           rate = n_postest/person_time,
           lower = ifelse(rate - qnorm(0.975)*(sqrt(n_postest/(person_time^2))) < 0, 0, 
                          rate - qnorm(0.975)*(sqrt(n_postest/(person_time^2)))),
           upper = ifelse(rate + qnorm(0.975)*(sqrt(n_postest/(person_time^2))) < 0, 0, 
                          rate + qnorm(0.975)*(sqrt(n_postest/(person_time^2)))),
           Rate_py = round(rate*Y, digits = 2),
           lower_py = round(lower*Y, digits = 2),
           upper_py = round(upper*Y, digits = 2),
           person_time = round(person_time, digits = 0)) %>%
    select(n_postest, person_time, Rate_py, lower_py, upper_py)
  
  results.table[2:9,(5*i - 2):(5*i + 2)] <- data_processed %>%
    group_by(group, .drop=FALSE) %>%
    summarise(
      n_postest = ifelse(i == 1, sum(covid_positive_test),
                         ifelse(i == 2, sum(covid_hospital_admission),
                                ifelse(i == 3, sum(covid_hospitalisation_critical_care),
                                       sum(covid_death)))),
      person_time = ifelse(i == 1, sum(time_to_positive_test),
                           ifelse(i == 2, sum(time_to_hospitalisation),
                                  ifelse(i == 3, sum(time_to_itu),
                                         sum(time_to_covid_death))))
    ) %>%
    ungroup() %>%
    mutate(person_time = person_time/365.25,
           rate = n_postest/person_time,
           lower = ifelse(rate - qnorm(0.975)*(sqrt(n_postest/(person_time^2))) < 0, 0,
                          rate - qnorm(0.975)*(sqrt(n_postest/(person_time^2)))),
           upper = ifelse(rate + qnorm(0.975)*(sqrt(n_postest/(person_time^2))) < 0, 0,
                          rate + qnorm(0.975)*(sqrt(n_postest/(person_time^2)))),
           Rate_py = round(rate*Y, digits = 2),
           lower_py = round(lower*Y, digits = 2),
           upper_py = round(upper*Y, digits = 2),
           person_time = round(person_time, digits = 0)) %>%
    select(n_postest, person_time, Rate_py, lower_py, upper_py)
  
}

## Counts of tests and positivity rate
test_counts_all <- data_processed %>%
  select(patient_id, group, tests_conducted_any, tests_conducted_positive) %>%
  summarise(n = n(),
            test_0 = sum(is.na(tests_conducted_any)),
            test_1 = sum(tests_conducted_any == 1, na.rm = T),
            test_2 = sum(tests_conducted_any  == 2, na.rm = T),
            test_3 = sum(tests_conducted_any > 3, na.rm = T),
            tests_conducted_any = sum(tests_conducted_any, na.rm = TRUE),
            tests_conducted_positive = sum(tests_conducted_positive, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(positivy = tests_conducted_positive/tests_conducted_any*100) %>%
  mutate(group = 0)

test_counts_groups <- data_processed %>%
  select(patient_id, group, tests_conducted_any, tests_conducted_positive) %>%
  group_by(group) %>%
  summarise(n = n(),
            test_0 = sum(is.na(tests_conducted_any)),
            test_1 = sum(tests_conducted_any == 1, na.rm = T),
            test_2 = sum(tests_conducted_any == 2, na.rm = T),
            test_3 = sum(tests_conducted_any > 2, na.rm = T),
            tests_conducted_any = sum(tests_conducted_any, na.rm = TRUE),
            tests_conducted_positive = sum(tests_conducted_positive, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(positivy = tests_conducted_positive/tests_conducted_any*100)

test_counts <- rbind(test_counts_all, test_counts_groups) %>%
  mutate(Group = results.table$Group) %>%
  select(Group, test_0, test_1, test_2, test_3, tests_conducted_any, positivy)

## Follow-up time
follow_up_all <- data_processed %>%
  summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),
            fu = quantile(follow_up_time, c(0.25, 0.5, 0.75))) %>%
  mutate(fu = round(fu, digits = 0)) %>%
  pivot_wider(names_from = quantile, values_from = fu) %>%
  mutate(group = 0,
         fu = paste(`50%`, " (", `25%`, "-", `75%`, ")", sep = ""))

follow_up_groups <- data_processed %>%
  group_by(group) %>%
  summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),
            fu = quantile(follow_up_time, c(0.25, 0.5, 0.75))) %>%
  mutate(fu = round(fu, digits = 0)) %>%
  pivot_wider(id_cols = c("group"), names_from = quantile, values_from = fu) %>%
  mutate(fu = paste(`50%`, " (", `25%`, "-", `75%`, ")", sep = ""))

follow_up <- rbind(follow_up_all, follow_up_groups) %>%
  mutate(Group = results.table$Group) %>%
  select(Group, fu)

## Combine tables
table1 <- left_join(results.table, test_counts, by = "Group") %>%
  left_join(follow_up) %>%
  mutate(test_0 = round(test_0/`Fully vaccinated`*100, digits = 0),
         test_1 = round(test_1/`Fully vaccinated`*100, digits = 0),
         test_2 = round(test_2/`Fully vaccinated`*100, digits = 0),
         test_3 = round(test_3/`Fully vaccinated`*100, digits = 0),
         positivy = round(positivy, digits = 2)) %>%
  select("Group", "Fully vaccinated", "fu", "test_0", "test_1", "test_2", "test_3",
         "Positive COVID test", "positivy", "PYs_1", "rate_1", "lci_1", "uci_1",
         "Hospitalised with COVID", "PYs_2", "rate_2", "lci_2", "uci_2",
         "COVID Deaths", "PYs_4", "rate_4", "lci_4", "uci_4")


# Redaction ----

## Redact values < 8
threshold = 8

results.table_redacted <- table1 %>%
  mutate(`Fully vaccinated` = ifelse(`Fully vaccinated` < threshold, NA, as.numeric(`Fully vaccinated`)),
         fu = ifelse(is.na(`Fully vaccinated`), NA, fu),
         test_0 = ifelse(is.na(`Fully vaccinated`), NA, test_0),
         test_1 = ifelse(is.na(`Fully vaccinated`), NA, test_1),
         test_2 = ifelse(is.na(`Fully vaccinated`), NA, test_2),
         test_3 = ifelse(is.na(`Fully vaccinated`), NA, test_3),
         `Positive COVID test` = ifelse(`Positive COVID test` < threshold, NA, `Positive COVID test`),
         positivy = ifelse(`Positive COVID test` < threshold, NA, positivy),
         PYs_1 = ifelse(`Positive COVID test` < threshold, NA, PYs_1),
         rate_1 = ifelse(`Positive COVID test` < threshold, NA, rate_1),
         lci_1 = ifelse(`Positive COVID test` < threshold, NA, lci_1),
         uci_1 = ifelse(`Positive COVID test` < threshold, NA, uci_1),
         `Hospitalised with COVID` = ifelse(`Hospitalised with COVID` < threshold, NA, `Hospitalised with COVID`),
         PYs_2 = ifelse(`Hospitalised with COVID` < threshold, NA, PYs_2),
         rate_2 = ifelse(`Hospitalised with COVID` < threshold, NA, rate_2),
         lci_2 = ifelse(`Hospitalised with COVID` < threshold, NA, lci_2),
         uci_2 = ifelse(`Hospitalised with COVID` < threshold, NA, uci_2),
         `COVID Deaths` = ifelse(`COVID Deaths` < threshold, NA, `COVID Deaths`),
         PYs_4 = ifelse(is.na(`COVID Deaths`), NA, PYs_4),
         rate_4 = ifelse(is.na(`COVID Deaths`), NA, rate_4),
         lci_4 = ifelse(is.na(`COVID Deaths`), NA, lci_4),
         uci_4 = ifelse(is.na(`COVID Deaths`), NA, uci_4)) %>%
  mutate(Group = c("All",
                   "Care home (priority group 1)",
                   "80+ (priority group 2)",
                   "Health / care workers (priority group 1-2)",
                   "70-79 (priority group 3-4)",
                   "Shielding (age 16-69) (priority group 4)",
                   "50-69 (priority groups 5-9)",
                   "Others not in the above groups",
                   "Trial")) %>%
  mutate(test_0 = ifelse(test_0/100*`Fully vaccinated` < threshold, NA, test_0),
         test_1 = ifelse(test_1/100*`Fully vaccinated` < threshold, NA, test_1),
         test_2 = ifelse(test_2/100*`Fully vaccinated` < threshold, NA, test_2),
         test_3 = ifelse(test_3/100*`Fully vaccinated` < threshold, NA, test_3))
  

## Round to nearest 5
results.table_redacted <- results.table_redacted %>%
  mutate(`Fully vaccinated` = plyr::round_any(`Fully vaccinated`, 5),
         `Positive COVID test` = plyr::round_any(`Positive COVID test`, 5),
         `Hospitalised with COVID` = plyr::round_any(`Hospitalised with COVID`, 5),
         `COVID Deaths` = plyr::round_any(`COVID Deaths`, 5))


## Recalculate column totals
results.table_redacted[1, "Fully vaccinated"] <- sum(results.table_redacted[-1,]$`Fully vaccinated`, na.rm = T)
results.table_redacted[1, "Positive COVID test"] <- sum(results.table_redacted[-1,]$`Positive COVID test`, na.rm = T)
results.table_redacted[1, "Hospitalised with COVID"] <- sum(results.table_redacted[-1,]$`Hospitalised with COVID`, na.rm = T)
results.table_redacted[1, "COVID Deaths"] <- sum(results.table_redacted[-1,]$`COVID Deaths`, na.rm = T)

## Replace na with [REDACTED]
results.table_redacted <- results.table_redacted %>%
  replace(is.na(.), "[REDACTED]")

## Formatting
results.table_redacted <- results.table_redacted %>%
  mutate(Fully_vaccinated_count =`Fully vaccinated`,
         Positive_test_count = paste(`Positive COVID test`, " (", PYs_1, ")", sep = ""),
         Positive_test_rate = paste(rate_1, " (", lci_1, "-", uci_1, ")", sep = ""),
         Hospitalised_count = paste(`Hospitalised with COVID`, " (", PYs_2, ")", sep = ""),
         Hospitalised_rate = paste(rate_2, " (", lci_2, "-", uci_2, ")", sep = ""),
         Death_count = paste(`COVID Deaths`, " (", PYs_4, ")", sep = ""),
         Death_rate = paste(rate_4, " (", lci_4, "-", uci_4, ")", sep = "")) %>%
  select(Group, Fully_vaccinated_count, Follow_up = fu, test_0, test_1, test_2, test_3, Positive_test_count, Positivy = positivy, Positive_test_rate, Hospitalised_count, Hospitalised_rate,
         Death_count,  Death_rate)

# Save as html ----
gt::gtsave(gt(table1), here::here("output","tables", "table1.html"))
gt::gtsave(gt(results.table_redacted), here::here("output","tables", "table1_redacted.html"))



