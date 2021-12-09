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
                   death_table) %>%
  left_join(data_processed %>%
              select(chronic_kidney_disease, 
                     covid_positive_test, 
                     time_to_positive_test,
                     covid_hospital_admission,
                     time_to_hospitalisation,
                     covid_death,
                     time_to_covid_death) %>%
              group_by(chronic_kidney_disease) %>%
              summarise(covid_positive_test = sum(covid_positive_test, na.rm = T),
                        covid_hospital_admission = sum(covid_hospital_admission, na.rm = T),
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

