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

## Import custom user functions
source(here("analysis", "functions.R"))

## Create output directory
fs::dir_create(here::here("output", "tables"))

## Import data
data_processed <- read_rds(here::here("output", "data", "data_all.rds"))

## Format data
data_processed <- data_processed %>%
  mutate(group = ifelse(care_home_65plus == 1, 1, NA),
         group = ifelse(is.na(group) & ageband == 3, 2, group),
         group = ifelse(is.na(group) & hscworker == 1, 3, group),
         group = ifelse(is.na(group) & ageband == 2, 4, group),
         group = ifelse(is.na(group) & shielded == 1, 5, group),
         group = ifelse(is.na(group) & age >=50 & age <70, 6, group),
         group = ifelse(is.na(group), 7, group),
         group = factor(group)) %>%
  mutate(time_since_2nd_dose = cut(follow_up_time_vax2,
                                   breaks = c(14, 28, 42, 56, 84, Inf),
                                   labels = c("2-4 weeks", "4-6 weeks", "6-8 weeks", "8-12 weeks", "12+ weeks"),
                                   right = FALSE),
         
         time_between_vaccinations = cut(tbv,
                                         breaks = c(0, 42, 98, Inf),
                                         labels = c("6 weeks or less", "6-14 weeks", "14 weeks or more"),
                                         right = FALSE),
         
         prior_covid = ifelse(latest_positive_test_date > (covid_vax_1_date + 14), "After 1st dose (+ 2 weeks)", NA),
         prior_covid = ifelse(latest_positive_test_date < (covid_vax_1_date + 14), "Anytime previously", prior_covid),
         
         smoking_status = ifelse(is.na(smoking_status), "M", smoking_status)) 


# Table 3 ----

## Counts
table_3_counts <- data_processed %>%
  select(group,
         covid_positive_post_2vacc,
         sex,
         bmi,
         smoking_status,
         ethnicity,
         imd,
         region,
         asthma,
         asplenia,
         blood_pressure = bpcat,
         chd,
         chronic_neuro_dis_inc_sig_learn_dis,
         chronic_resp_dis,
         chronic_kidney_disease,
         end_stage_renal, 
         cld, 
         diabetes, 
         immunosuppression, 
         learning_disability, 
         sev_mental_ill, 
         organ_transplant,
         time_since_2nd_dose,
         time_between_vaccinations,
         prior_covid) %>%
  filter(covid_positive_post_2vacc == 1) %>%
  select(-covid_positive_post_2vacc) %>%
  tbl_summary(by = group) %>%
  add_overall()

table_3_counts$inputs$data <- NULL

## Rates
table_3_rates <- calculate_rates(group = "covid_positive_post_2vacc",
                                 follow_up = "time_to_positive_test",
                                 data = data_processed,
                                 Y = 100000, 
                                 dig = 2,
                                 variables = c("sex", "bmi", "smoking_status", "ethnicity",
                                               "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                               "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                               "chronic_kidney_disease",  "end_stage_renal","cld", 
                                               "diabetes", "immunosuppression", "learning_disability", 
                                               "sev_mental_ill", "organ_transplant", "time_since_2nd_dose",
                                               "time_between_vaccinations", "prior_covid"))


# Save as html ----
gtsave(as_gt(table_3_counts), here::here("output", "tables", "table3_counts.html"))


