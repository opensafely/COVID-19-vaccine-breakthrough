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
  mutate(group = ifelse(care_home_65plus == 1, 1, NA),
         group = ifelse(is.na(group) & ageband == 3, 2, group),
         group = ifelse(is.na(group) & hscworker == 1, 3, group),
         group = ifelse(is.na(group) & ageband == 2, 4, group),
         group = ifelse(is.na(group) & shielded == 1, 5, group),
         group = ifelse(is.na(group) & age >=50 & age <70, 6, group),
         group = ifelse(is.na(group), 7, group),
         group = factor(group),
         
         ageband3 = cut(
           age,
           breaks = c(16, 50, 60, 70, 80, Inf),
           labels = c("16-50", "50-59", "60-69", "70-79", "80+"),
           right = FALSE),
         
         imd = as.character(imd),
         imd = ifelse(imd %in% c("1 most deprived", 2:4, "5 least deprived"), imd, "Unknown"),
         
         imd = fct_case_when(
           imd == "1 most deprived" ~ "1 most deprived",
           imd == 2 ~ "2",
           imd == 3 ~ "3",
           imd == 4 ~ "4",
           imd == "5 least deprived" ~ "5 least deprived",
           imd == "Unknown" ~ "Unknown",
           #TRUE ~ "Unknown",
           TRUE ~ NA_character_
         ),
         
         region = as.character(region),
         
         region = fct_case_when(
           region == "London" ~ "London",
           region == "East of England" ~ "East of England",
           region == "East Midlands" ~ "East Midlands",
           region == "North East" ~ "North East",
           region == "North West" ~ "North West",
           region == "South East" ~ "South East",
           region == "South West" ~ "South West",
           region == "West Midlands" ~ "West Midlands",
           region == "Yorkshire and the Humber" ~ "Yorkshire and the Humber",
           #TRUE ~ "Unknown"
           TRUE ~ NA_character_),
         
         bpcat = as.character(bpcat),
         bpcat = ifelse(bpcat == "Normal", "Normal", ifelse(bpcat %in% c("Elevated", "High"), "Elevated/high", "Unknown")),
         
         bpcat = fct_case_when(
           bpcat == "Normal" ~ "Normal",
           bpcat == "Elevated/high" ~ "Elevated/high",
           bpcat == "Unknown" ~ "Unknown",
           #TRUE ~ "Unknown",
           TRUE ~ NA_character_),
         
         organ_transplant_old = organ_transplant,
         end_stage_renal_old = end_stage_renal,
         
         organ_transplant = ifelse(organ_transplant_old == 1 & end_stage_renal_old == 0, "without RRT", 
                                   ifelse(organ_transplant_old == 1 & end_stage_renal_old == 1, "with RRT", NA)),
         
         
         end_stage_renal = ifelse(organ_transplant_old == 0 & end_stage_renal_old == "without organ transplant", 1, 
                                  ifelse(organ_transplant_old == 1 & end_stage_renal_old == 1, "with organ transplant", NA))
  ) %>%
  group_by(patient_id) %>%
  mutate(follow_up_time =  (follow_up_time_vax2 - 14)) %>%
  ungroup()


# Table 2 ----

## Counts
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
  select(-perc) %>%
  filter(!(group == "prior_covid_cat" & variable == "Unknown"),
         !(group == "organ_transplant" & variable == "Unknown"),
         !(group == "end_stage_renal" & variable == "Unknown"))


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
              select(covid_positive_test, 
                     time_to_positive_test,
                     covid_hospital_admission,
                     time_to_hospitalisation,
                     covid_death,
                     time_to_covid_death) %>%
              ungroup() %>%
              summarise(covid_positive_test = sum(covid_positive_test, na.rm = T),
                        covid_hospital_admission = sum(covid_hospital_admission, na.rm = T),
                        covid_death = sum(covid_death, na.rm = T)) %>%
              melt(), by = c("outcome" = "variable")) %>%
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

