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


# Format data ----

## Filter on 80+ group
data_cohort_over80 <- data_processed %>%
  filter(care_home_65plus == 0 & ageband == 3)


# Counts tables ----

## All
table2_counts_all <- data_processed %>%
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
         
         smoking_status = ifelse(is.na(smoking_status), "M", smoking_status)) %>%
  select(covid_positive_post_2vacc,
         covid_hospital_admission,
         covid_hospitalisation_critical_care,
         covid_death,
         agegroup = ageband2,
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
  pivot_longer(
    cols = c("covid_positive_post_2vacc", "covid_hospital_admission", "covid_hospitalisation_critical_care", "covid_death"),
    names_to = "Outcome",
    values_to = "flag",
    values_drop_na = TRUE
  ) %>%
  filter(flag == 1) %>%
  select(-flag) %>%
  tbl_summary(by = Outcome) %>%
  add_overall()

table2_counts_all$inputs$data <- NULL

## Over 80s
table2_counts_over80 <- data_cohort_over80 %>%
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
         
         smoking_status = ifelse(is.na(smoking_status), "M", smoking_status)) %>%
  select(covid_positive_post_2vacc,
         covid_hospital_admission,
         covid_hospitalisation_critical_care,
         covid_death,
         agegroup = ageband2,
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
  pivot_longer(
    cols = c("covid_positive_post_2vacc", "covid_hospital_admission", "covid_hospitalisation_critical_care", "covid_death"),
    names_to = "Outcome",
    values_to = "flag",
    values_drop_na = TRUE
  ) %>%
  filter(flag == 1) %>%
  select(-flag) %>%
  tbl_summary(by = Outcome) %>%
  add_overall()

table2_counts_over80$inputs$data <- NULL


# Rates tables ----

## Shell 
names <-  data_processed %>%
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
         
         smoking_status = ifelse(is.na(smoking_status), "M", smoking_status),
         asthma = ifelse(asthma == 1, "astma", NA),
         asplenia = ifelse(asplenia == 1, "asplenia", NA),
         chd = ifelse(chd == 1, "chd", NA),
         chronic_neuro_dis_inc_sig_learn_dis = ifelse(chronic_neuro_dis_inc_sig_learn_dis == 1, "chronic_neuro_dis_inc_sig_learn_dis", NA),
         chronic_resp_dis = ifelse(chronic_resp_dis == 1, "chronic_resp_dis", NA),
         chronic_kidney_disease = ifelse(chronic_kidney_disease == 1, "chronic_kidney_disease", NA),
         end_stage_renal = ifelse(end_stage_renal == 1, "end_stage_renal", NA),
         cld = ifelse(cld == 1, "cld", NA),
         diabetes = ifelse(diabetes == 1, "diabetes", NA),
         immunosuppression = ifelse(immunosuppression == 1, "immunosuppression", NA),
         learning_disability = ifelse(learning_disability == 1, "learning_disability", NA),
         sev_mental_ill = ifelse(sev_mental_ill == 1, "sev_mental_ill", NA),
         organ_transplant = ifelse(organ_transplant == 1, "organ_transplant", NA)) %>%
  select(patient_id, 
         ageband2,
         sex,
         bmi,
         smoking_status,
         ethnicity,
         imd,
         region,
         asthma,
         asplenia,
         bpcat,
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
  reshape2::melt(id.var = "patient_id") %>%
  select(-patient_id) %>%
  distinct() %>%
  filter(value != 0) %>%
  select(group = variable,
         variable = value)

## All
rates1_all <- calculate_rates(group = "covid_positive_post_2vacc",
                                 follow_up = "time_to_positive_test",
                                 data = data_processed,
                                 Y = 1000, 
                                 dig = 2,
                                 variables = c("ageband2", "sex", "bmi", "smoking_status", "ethnicity",
                                               "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                               "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                               "chronic_kidney_disease",  "end_stage_renal","cld", 
                                               "diabetes", "immunosuppression", "learning_disability", 
                                               "sev_mental_ill", "organ_transplant", "time_since_2nd_dose",
                                               "time_between_vaccinations", "prior_covid"))

rates2_all <- calculate_rates(group = "covid_hospital_admission",
                                 follow_up = "time_to_hospitalisation",
                                 data = data_processed,
                                 Y = 1000, 
                                 dig = 2,
                                 variables = c("ageband2", "sex", "bmi", "smoking_status", "ethnicity",
                                               "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                               "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                               "chronic_kidney_disease",  "end_stage_renal","cld", 
                                               "diabetes", "immunosuppression", "learning_disability", 
                                               "sev_mental_ill", "organ_transplant", "time_since_2nd_dose",
                                               "time_between_vaccinations", "prior_covid"))

rates3_all <- calculate_rates(group = "covid_death",
                                 follow_up = "time_to_covid_death",
                                 data = data_processed,
                                 Y = 1000, 
                                 dig = 2,
                                 variables = c("ageband2", "sex", "bmi", "smoking_status", "ethnicity",
                                               "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                               "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                               "chronic_kidney_disease",  "end_stage_renal","cld", 
                                               "diabetes", "immunosuppression", "learning_disability", 
                                               "sev_mental_ill", "organ_transplant", "time_since_2nd_dose",
                                               "time_between_vaccinations", "prior_covid"))  

rates_all <- left_join(names, rates1_all, by = c("group", "variable")) %>%
  left_join(rates2_all, by = c("group", "variable")) %>%
  left_join(rates3_all, by = c("group", "variable"))

colnames(rates_all) = c("Variable", "level", 
                        "covid_positive_post_2vacc", "Rate1", "LCI1", "UCI1",
                        "covid_hospital_admission", "Rate2", "LCI2", "UCI2",
                        "covid_death", "Rate3", "LCI3", "UCI3")

## Over 80s
rates1_over80 <- calculate_rates(group = "covid_positive_post_2vacc",
                                                   follow_up = "time_to_positive_test",
                                                   data = data_cohort_over80,
                                                   Y = 1000, 
                                                   dig = 2,
                                                   variables = c("ageband2", "sex", "bmi", "smoking_status", "ethnicity",
                                                                 "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                                                 "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                                                 "chronic_kidney_disease",  "end_stage_renal","cld", 
                                                                 "diabetes", "immunosuppression", "learning_disability", 
                                                                 "sev_mental_ill", "organ_transplant", "time_since_2nd_dose",
                                                                 "time_between_vaccinations", "prior_covid"))

rates2_over80 <- calculate_rates(group = "covid_hospital_admission",
                                 follow_up = "time_to_hospitalisation",
                                 data = data_cohort_over80,
                                 Y = 1000, 
                                 dig = 2,
                                 variables = c("ageband2", "sex", "bmi", "smoking_status", "ethnicity",
                                               "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                               "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                               "chronic_kidney_disease",  "end_stage_renal","cld", 
                                               "diabetes", "immunosuppression", "learning_disability", 
                                               "sev_mental_ill", "organ_transplant", "time_since_2nd_dose",
                                               "time_between_vaccinations", "prior_covid"))

rates3_over80 <- calculate_rates(group = "covid_death",
                                 follow_up = "time_to_covid_death",
                                 data = data_cohort_over80,
                                 Y = 1000, 
                                 dig = 2,
                                 variables = c("ageband2", "sex", "bmi", "smoking_status", "ethnicity",
                                               "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                               "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                               "chronic_kidney_disease",  "end_stage_renal","cld", 
                                               "diabetes", "immunosuppression", "learning_disability", 
                                               "sev_mental_ill", "organ_transplant", "time_since_2nd_dose",
                                               "time_between_vaccinations", "prior_covid"))  

rates_over80s <- left_join(names, rates1_over80, by = c("group", "variable")) %>%
  left_join(rates2_over80, by = c("group", "variable")) %>%
  left_join(rates3_over80, by = c("group", "variable"))

colnames(rates_over80s) = c("Variable", "level", 
                            "covid_positive_post_2vacc", "Rate1", "LCI1", "UCI1",
                            "covid_hospital_admission", "Rate2", "LCI2", "UCI2",
                            "covid_death", "Rate3", "LCI3", "UCI3")



# Save as html ----
gtsave(as_gt(table2_counts_all), here::here("output", "tables", "table2_counts_all.html"))
gtsave(as_gt(table2_counts_over80), here::here("output", "tables", "table2_counts_over80.html"))

gt::gtsave(gt(rates_all), here::here("output","tables", "table2_rates_all.html"))
gt::gtsave(gt(rates_over80s), here::here("output","tables", "table2_rates_over80.html"))

#gt::gtsave(gt(results.table_redacted), here::here("output","tables", "table2_redacted.html"))




