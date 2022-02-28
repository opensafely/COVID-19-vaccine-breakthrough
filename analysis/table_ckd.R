######################################

# This script 
# - produces a table with the number of patients fully vaccinated (2 doses + 2 weeks) in 
#   selected clinical and demographic groups
# - saves table as html

######################################

# Preliminaries ----

## Import libraries
library('tidyverse')
library('here')
library('glue')
library('gt')
library('gtsummary')
library('reshape2')
library('survival')

## Import custom user functions
source(here("analysis", "functions.R"))

## Create output directory
fs::dir_create(here::here("output", "tables"))

## Import data
data_processed <- read_rds(here::here("output", "data", "data_processed.rds"))

## Format data
data_processed <- data_processed %>%
  group_by(patient_id) %>%
  mutate(follow_up_time =  (follow_up_time_vax2 - 14)) %>%
  ungroup()


# Table 2 ----

## Counts
counts0 <- data_processed %>%
  select(chronic_kidney_disease) %>%
  tbl_summary()

counts0$inputs$data <- NULL

counts <- counts0$table_body %>%
  select(group = variable, variable = label, count = stat_0) %>%
  separate(count, c("count","perc"), sep = "([(])") %>%
  # mutate(count = as.numeric(count),
  #        perc = gsub('.{2}$', '', perc)) %>%
  mutate(count = gsub(" ", "", count),
         count = as.numeric(gsub(",", "", count))) %>%
  filter(!(is.na(count))) %>%
  filter(!(group == "chronic_kidney_disease" & variable == "Unknown"))


## Positive test rates
mod.cox <- coxph(Surv(time_to_positive_test, covid_positive_test) ~ chronic_kidney_disease, data = data_processed)
mod.cox.adj <- coxph(Surv(time_to_positive_test, covid_positive_test) ~ chronic_kidney_disease + age, data = data_processed)

positive_test_table <- rbind(tidy_wald(mod.cox, exponentiate = TRUE) %>% 
                               mutate(outcome = "covid_positive_test", model = "unadjusted"),
                             tidy_wald(mod.cox.adj, exponentiate = TRUE) %>%
                               mutate(outcome = "covid_positive_test", model = "age adjusted")) %>%
  filter(!term == "age")

## Hospitalisation rates
mod.cox <- coxph(Surv(time_to_hospitalisation, covid_hospital_admission) ~ chronic_kidney_disease, data = data_processed)
mod.cox.adj <- coxph(Surv(time_to_hospitalisation, covid_hospital_admission) ~ chronic_kidney_disease + age, data = data_processed)

hospitilisation_table <- rbind(tidy_wald(mod.cox, exponentiate = TRUE) %>% 
                               mutate(outcome = "covid_hospital_admission", model = "unadjusted"),
                             tidy_wald(mod.cox.adj, exponentiate = TRUE) %>%
                               mutate(outcome = "covid_hospital_admission", model = "age adjusted")) %>%
  filter(!term == "age")

## Critical care rates
mod.cox <- coxph(Surv(time_to_itu, covid_hospitalisation_critical_care) ~ chronic_kidney_disease, data = data_processed)
mod.cox.adj <- coxph(Surv(time_to_itu, covid_hospitalisation_critical_care) ~ chronic_kidney_disease + age, data = data_processed)

criticalcare_table <- rbind(tidy_wald(mod.cox, exponentiate = TRUE) %>% 
                                 mutate(outcome = "covid_hospitalisation_critical_care", model = "unadjusted"),
                               tidy_wald(mod.cox.adj, exponentiate = TRUE) %>%
                                 mutate(outcome = "covid_hospitalisation_critical_care", model = "age adjusted")) %>%
  filter(!term == "age")

## Death rates
mod.cox <- coxph(Surv(time_to_covid_death, covid_death) ~ chronic_kidney_disease, data = data_processed)
mod.cox.adj <- coxph(Surv(time_to_covid_death, covid_death) ~ chronic_kidney_disease + age, data = data_processed)

death_table <- rbind(tidy_wald(mod.cox, exponentiate = TRUE) %>% 
                               mutate(outcome = "covid_death", model = "unadjusted"),
                             tidy_wald(mod.cox.adj, exponentiate = TRUE) %>%
                               mutate(outcome = "covid_death", model = "age adjusted")) %>%
  filter(!term == "age")


## Combine tables
table_ckd <- rbind(positive_test_table,
                   hospitilisation_table,
                   criticalcare_table,
                   death_table) %>%
  left_join(data_processed %>%
              select(chronic_kidney_disease, 
                     covid_positive_test, 
                     time_to_positive_test,
                     covid_hospital_admission,
                     time_to_hospitalisation,
                     covid_hospitalisation_critical_care,
                     time_to_itu,
                     covid_death,
                     time_to_covid_death) %>%
              group_by(chronic_kidney_disease) %>%
              summarise(covid_positive_test = sum(covid_positive_test, na.rm = T),
                        covid_hospital_admission = sum(covid_hospital_admission, na.rm = T),
                        covid_hospitalisation_critical_care = sum(covid_hospitalisation_critical_care, na.rm = T),
                        covid_death = sum(covid_death, na.rm = T)) %>%
              melt(id.var = "chronic_kidney_disease") %>%
              mutate(chronic_kidney_disease = paste("chronic_kidney_disease", chronic_kidney_disease, sep="")),
            by = c("outcome" = "variable", "term" = "chronic_kidney_disease")) %>%
  select(outcome, model, term, events = value, estimate, `conf.low`, `conf.high`, `p.value`)
  

# Redaction ----

## Redact values < 8
threshold = 8

table_ckd_redacted <- table_ckd %>%
  mutate(estimate = ifelse(events < threshold, NA, estimate),
         `conf.low` = ifelse(events < threshold, NA, `conf.low`),
         `conf.high` = ifelse(events < threshold, NA, `conf.high`),
         `p.value` = ifelse(events < threshold, NA, `p.value`))

## Round to nearest 5
table_ckd_redacted <- table_ckd_redacted %>%
  mutate(events = plyr::round_any(events, 5))

# Save as html ----
gt::gtsave(gt(table_ckd), here::here("output","tables", "table_ckd.html"))
gt::gtsave(gt(table_ckd_redacted), here::here("output","tables", "table_ckd_redacted.html"))


#########################################################################################


variables <- c("chronic_kidney_disease")

counts0 <- data_processed %>%
  mutate(time_since_fully_vaccinated = cut(follow_up_time_vax2 - 14,
                                           breaks = c(0, 28, 56, 84, Inf),
                                           labels = c("0-4 weeks", "4-8 weeks", "8-12 weeks", "12+ weeks"),
                                           right = FALSE),
         
         time_between_vaccinations = cut(tbv,
                                         breaks = c(0, 42, 84, Inf),
                                         labels = c("6 weeks or less", "6-12 weeks", "12 weeks or more"),
                                         right = FALSE),
         
         smoking_status = ifelse(is.na(smoking_status), "M", smoking_status)) %>%
  select(all_of(variables)) %>%
  tbl_summary()

counts0$inputs$data <- NULL

counts <- counts0$table_body %>%
  select(group = variable, variable = label, count = stat_0) %>%
  separate(count, c("count","perc"), sep = "([(])") %>%
  # mutate(count = as.numeric(count),
  #        perc = gsub('.{2}$', '', perc)) %>%
  mutate(count = gsub(" ", "", count),
         count = as.numeric(gsub(",", "", count))) %>%
  filter(!(is.na(count))) %>%
  select(-perc) %>%
  filter(!(group == "prior_covid_cat" & variable == "Unknown"),
         !(group == "chronic_kidney_disease" & variable == "Unknown"),
         !(group == "dialysis" & variable == "Unknown"),
         !(group == "transplant" & variable == "Unknown"))

## Positive test rates
positive_test_rates <- calculate_rates(group = "covid_positive_test",
                                       follow_up = "time_to_positive_test",
                                       data = data_processed,
                                       Y = 1000, 
                                       dig = 0,
                                       variables = variables)

table2 <- left_join(counts, positive_test_rates, by = c("group", "variable"))

## Hospitalisation rates
hospitalisation_rates <- calculate_rates(group = "covid_hospital_admission",
                                         follow_up = "time_to_hospitalisation",
                                         data = data_processed,
                                         Y = 1000, 
                                         dig = 0,
                                         variables = variables)

table2 <- left_join(table2, hospitalisation_rates, by = c("group", "variable"))


## Critical care with COVID rates
critial_care_rates <- calculate_rates(group = "covid_hospitalisation_critical_care",
                                      follow_up = "time_to_itu",
                                      data = data_processed,
                                      Y = 1000, 
                                      dig = 0,
                                      variables = variables)

table2 <- left_join(table2, critial_care_rates, by = c("group", "variable"))

## Death rates
death_rates <- calculate_rates(group = "covid_death",
                               follow_up = "time_to_covid_death",
                               data = data_processed,
                               Y = 1000, 
                               dig = 0,
                               variables = variables)

table2 <- left_join(table2, death_rates, by = c("group", "variable"))

colnames(table2) = c("Variable", "level",
                     "Fully vaccinated",
                     "Positive COVID test", "PYs_1", "rate_1", "lci_1", "uci_1", 
                     "Hospitalised with COVID", "PYs_2", "rate_2", "lci_2", "uci_2",
                     "Critical care with COVID", "PYs_3", "rate_3", "lci_3", "uci_3",
                     "COVID Deaths", "PYs_4", "rate_4", "lci_4", "uci_4")

## Counts of tests and positivity rate
test_counts <- data_processed %>%
  select(tests_conducted_any,
         tests_conducted_positive,
         all_of(variables)) %>%
  melt(id.var = c("tests_conducted_any", "tests_conducted_positive")) %>%
  group_by(variable, value) %>%
  summarise(n = n(),
            test_0 = sum(is.na(tests_conducted_any)),
            test_1 = sum(tests_conducted_any == 1, na.rm = T),
            test_2 = sum(tests_conducted_any  == 2, na.rm = T),
            test_3 = sum(tests_conducted_any > 2, na.rm = T),
            tests_conducted_any = sum(tests_conducted_any, na.rm = TRUE),
            tests_conducted_positive = sum(tests_conducted_positive, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(positivy = tests_conducted_positive/tests_conducted_any*100,
         value = ifelse(is.na(value), "Unknown", value)) %>%
  select(Variable = variable, level = value, test_0, test_1, test_2, test_3, tests_conducted_any, positivy)

## Follow-up time
follow_up <- data_processed %>%
  select(follow_up_time,
         all_of(variables)) %>%
  melt(id.var = c("follow_up_time")) %>%
  group_by(variable, value) %>%
  summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),
            fu = quantile(follow_up_time, c(0.25, 0.5, 0.75))) %>%
  mutate(fu = round(fu, digits = 0)) %>%
  pivot_wider(id_cols = c("variable", "value"), names_from = quantile, values_from = fu) %>%
  mutate(fu = paste(`50%`, " (", `25%`, "-", `75%`, ")", sep = ""),
         value = ifelse(is.na(value), "Unknown", value)) %>%
  select(Variable = variable, level = value, fu)

## Combine tables
table2 <- left_join(table2, test_counts, by = c("Variable", "level")) %>%
  left_join(follow_up, by = c("Variable", "level")) %>%
  mutate(test_0 = round(test_0/`Fully vaccinated`*100, digits = 0),
         test_1 = round(test_1/`Fully vaccinated`*100, digits = 0),
         test_2 = round(test_2/`Fully vaccinated`*100, digits = 0),
         test_3 = round(test_3/`Fully vaccinated`*100, digits = 0),
         positivy = round(positivy, digits = 2)) %>%
  mutate() %>%
  select("Variable", "level", "Fully vaccinated", "fu", "test_0", "test_1", "test_2", "test_3",
         "Positive COVID test", "positivy", "PYs_1", "rate_1", "lci_1", "uci_1",
         "Hospitalised with COVID", "PYs_2", "rate_2", "lci_2", "uci_2",
         "Critical care with COVID", "PYs_3", "rate_3", "lci_3", "uci_3",
         "COVID Deaths", "PYs_4", "rate_4", "lci_4", "uci_4")

# Redaction ----

## Redact values < 8
threshold = 8

table2_redacted <- table2 %>%
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
         `Critical care with COVID` = ifelse(`Critical care with COVID` < threshold, NA, `Critical care with COVID`),
         PYs_3 = ifelse(`Critical care with COVID` < threshold, NA, PYs_3),
         rate_3 = ifelse(`Critical care with COVID` < threshold, NA, rate_3),
         lci_3 = ifelse(`Critical care with COVID` < threshold, NA, lci_3),
         uci_3 = ifelse(`Critical care with COVID` < threshold, NA, uci_3),
         `COVID Deaths` = ifelse(`COVID Deaths` < threshold, NA, `COVID Deaths`),
         PYs_4 = ifelse(is.na(`COVID Deaths`), NA, PYs_4),
         rate_4 = ifelse(is.na(`COVID Deaths`), NA, rate_4),
         lci_4 = ifelse(is.na(`COVID Deaths`), NA, lci_4),
         uci_4 = ifelse(is.na(`COVID Deaths`), NA, uci_4)) %>%
  mutate(test_0 = ifelse(test_0/100*`Fully vaccinated` < threshold, NA, test_0),
         test_1 = ifelse(test_1/100*`Fully vaccinated` < threshold, NA, test_1),
         test_2 = ifelse(test_2/100*`Fully vaccinated` < threshold, NA, test_2),
         test_3 = ifelse(test_3/100*`Fully vaccinated` < threshold, NA, test_3))

## Round to nearest 5
table2_redacted <- table2_redacted %>%
  mutate(`Fully vaccinated` = plyr::round_any(`Fully vaccinated`, 5),
         `Positive COVID test` = plyr::round_any(`Positive COVID test`, 5),
         `Hospitalised with COVID` = plyr::round_any(`Hospitalised with COVID`, 5),
         `Critical care with COVID` = plyr::round_any(`Critical care with COVID`, 5),
         `COVID Deaths` = plyr::round_any(`COVID Deaths`, 5))

## Formatting
table2_redacted <- table2_redacted %>%
  mutate(Positive_test_count = paste(`Positive COVID test`, " (", PYs_1, ")", sep = ""),
         Positive_test_rate = paste(rate_1, " (", lci_1, "-", uci_1, ")", sep = ""),
         Hospitalised_count = paste(`Hospitalised with COVID`, " (", PYs_2, ")", sep = ""),
         Hospitalised_rate = paste(rate_2, " (", lci_2, "-", uci_2, ")", sep = ""),
         Critial_Care_count = paste(`Critical care with COVID`, " (", PYs_3, ")", sep = ""),
         Critial_Care_rate = paste(rate_3, " (", lci_3, "-", uci_3, ")", sep = ""),
         Death_count = paste(`COVID Deaths`, " (", PYs_4, ")", sep = ""),
         Death_rate = paste(rate_4, " (", lci_4, "-", uci_4, ")", sep = "")) %>%
  select(Variable, level, "Fully vaccinated", Follow_up = fu, test_0, test_1, test_2, test_3, 
         Positive_test_count, Positivy = positivy, Positive_test_rate, Hospitalised_count, Hospitalised_rate,
         Critial_Care_count, Critial_Care_rate, Death_count, Death_rate) 

# Save as html ----
gt::gtsave(gt(table2), here::here("output","tables", "table2_ckd.html"))
gt::gtsave(gt(table2_redacted), here::here("output","tables", "table2_ckd_redacted.html"))


