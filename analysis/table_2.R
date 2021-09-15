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
           right = FALSE)) %>%
  group_by(patient_id) %>%
  mutate(follow_up_time =  min((follow_up_time_vax2 - 14), 
                               time_to_positive_test,
                               time_to_hospitalisation,
                               time_to_covid_death)) %>%
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
  select(ageband3, 
         sex,
         bmi,
         smoking_status,
         ethnicity,
         imd,
         region,
         asthma,
         asplenia,
         bpcat,
         cancer,
         diabetes,
         chd,
         haem_cancer,
         immunosuppression,
         chronic_kidney_disease,
         learning_disability,
         cld,
         chronic_neuro_dis_inc_sig_learn_dis,
         chronic_resp_dis,
         end_stage_renal, 
         sev_mental_ill, 
         organ_transplant,
         time_since_fully_vaccinated,
         time_between_vaccinations,
         prior_covid_cat) %>%
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
  filter(!(group == "prior_covid_cat" & variable == "Unknown"))


## Positive test rates
positive_test_rates <- calculate_rates(group = "covid_positive_test",
                          follow_up = "time_to_positive_test",
                          data = data_processed,
                          Y = 1, 
                          dig = 2,
                          variables = c("ageband3", "sex", "bmi", "smoking_status", "ethnicity",
                                        "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                        "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                        "chronic_kidney_disease",  "end_stage_renal","cld", 
                                        "diabetes", "immunosuppression", "learning_disability", 
                                        "sev_mental_ill", "organ_transplant", "time_since_fully_vaccinated",
                                        "time_between_vaccinations", "prior_covid_cat", "cancer", "haem_cancer"))

table2 <- left_join(counts, positive_test_rates, by = c("group", "variable"))

## Hospitalisation rates
hospitalisation_rates <- calculate_rates(group = "covid_hospital_admission",
                                       follow_up = "time_to_hospitalisation",
                                       data = data_processed,
                                       Y = 1, 
                                       dig = 2,
                                       variables = c("ageband3", "sex", "bmi", "smoking_status", "ethnicity",
                                                     "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                                     "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                                     "chronic_kidney_disease",  "end_stage_renal","cld", 
                                                     "diabetes", "immunosuppression", "learning_disability", 
                                                     "sev_mental_ill", "organ_transplant", "time_since_fully_vaccinated",
                                                     "time_between_vaccinations", "prior_covid_cat", "cancer", "haem_cancer"))

table2 <- left_join(table2, hospitalisation_rates, by = c("group", "variable"))

## Death rates
death_rates <- calculate_rates(group = "covid_death",
                                         follow_up = "time_to_covid_death",
                                         data = data_processed,
                                         Y = 1, 
                                         dig = 2,
                                         variables = c("ageband3", "sex", "bmi", "smoking_status", "ethnicity",
                                                       "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                                       "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                                       "chronic_kidney_disease",  "end_stage_renal","cld", 
                                                       "diabetes", "immunosuppression", "learning_disability", 
                                                       "sev_mental_ill", "organ_transplant", "time_since_fully_vaccinated",
                                                       "time_between_vaccinations", "prior_covid_cat", "cancer", "haem_cancer"))

table2 <- left_join(table2, death_rates, by = c("group", "variable"))


colnames(table2) = c("Variable", "level",
                         "Fully vaccinated",
                         "Positive COVID test", "PYs_1", "rate_1", "lci_1", "uci_1", 
                         "Hospitalised with COVID", "PYs_2", "rate_2", "lci_2", "uci_2",
                         "COVID Deaths", "PYs_4", "rate_4", "lci_4", "uci_4")

## Counts of tests and positivity rate
test_counts <- data_processed %>%
  mutate(time_since_fully_vaccinated = cut(follow_up_time_vax2 - 14,
                                           breaks = c(0, 28, 56, 84, Inf),
                                           labels = c("0-4 weeks", "4-8 weeks", "8-12 weeks", "12+ weeks"),
                                           right = FALSE),
         
         time_between_vaccinations = cut(tbv,
                                         breaks = c(0, 42, 84, Inf),
                                         labels = c("6 weeks or less", "6-12 weeks", "12 weeks or more"),
                                         right = FALSE),
         
         smoking_status = ifelse(is.na(smoking_status), "M", smoking_status),
         smoking_status = ifelse(is.na(smoking_status), "N&M", smoking_status),
         asthma = ifelse(asthma == 1, "asthma", NA),
         asplenia = ifelse(asplenia == 1, "asplenia", NA),
         cancer = ifelse(cancer == 1, "cancer", NA),
         haem_cancer = ifelse(haem_cancer == 1, "haem_cancer", NA),
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
  select(tests_conducted_any,
         tests_conducted_positive,
         ageband3, 
         sex,
         bmi,
         smoking_status,
         ethnicity,
         imd,
         region,
         asthma,
         asplenia,
         bpcat,
         cancer,
         diabetes,
         chd,
         haem_cancer,
         immunosuppression,
         chronic_kidney_disease,
         learning_disability,
         cld,
         chronic_neuro_dis_inc_sig_learn_dis,
         chronic_resp_dis,
         end_stage_renal, 
         sev_mental_ill, 
         organ_transplant,
         time_since_fully_vaccinated,
         time_between_vaccinations,
         prior_covid_cat) %>%
  filter(tests_conducted_any > 0) %>%
  melt(id.var = c("tests_conducted_any", "tests_conducted_positive")) %>%
  group_by(variable, value) %>%
  summarise(n = n(),
            n_test = sum(!is.na(tests_conducted_any)),
            tests_conducted_any = sum(tests_conducted_any, na.rm = TRUE),
            tests_conducted_positive = sum(tests_conducted_positive, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(positivy = tests_conducted_positive/tests_conducted_any*100) %>%
  select(Variable = variable, level = value, tests_conducted_any, positivy)

## Follow-up time
follow_up <- data_processed %>%
  mutate(time_since_fully_vaccinated = cut(follow_up_time_vax2 - 14,
                                           breaks = c(0, 28, 56, 84, Inf),
                                           labels = c("0-4 weeks", "4-8 weeks", "8-12 weeks", "12+ weeks"),
                                           right = FALSE),
         
         time_between_vaccinations = cut(tbv,
                                         breaks = c(0, 42, 84, Inf),
                                         labels = c("6 weeks or less", "6-12 weeks", "12 weeks or more"),
                                         right = FALSE),
         
         smoking_status = ifelse(is.na(smoking_status), "M", smoking_status),
         smoking_status = ifelse(is.na(smoking_status), "N&M", smoking_status),
         asthma = ifelse(asthma == 1, "asthma", NA),
         asplenia = ifelse(asplenia == 1, "asplenia", NA),
         cancer = ifelse(cancer == 1, "cancer", NA),
         haem_cancer = ifelse(haem_cancer == 1, "haem_cancer", NA),
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
  select(follow_up_time,
         ageband3, 
         sex,
         bmi,
         smoking_status,
         ethnicity,
         imd,
         region,
         asthma,
         asplenia,
         bpcat,
         cancer,
         diabetes,
         chd,
         haem_cancer,
         immunosuppression,
         chronic_kidney_disease,
         learning_disability,
         cld,
         chronic_neuro_dis_inc_sig_learn_dis,
         chronic_resp_dis,
         end_stage_renal, 
         sev_mental_ill, 
         organ_transplant,
         time_since_fully_vaccinated,
         time_between_vaccinations,
         prior_covid_cat) %>%
  melt(id.var = c("follow_up_time")) %>%
  group_by(variable, value) %>%
  summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),
            fu = quantile(follow_up_time, c(0.25, 0.5, 0.75))) %>%
  mutate(fu = round(fu, digits = 0)) %>%
  pivot_wider(id_cols = c("variable", "value"), names_from = quantile, values_from = fu) %>%
  mutate(fu = paste(`50%`, " (", `25%`, "-", `75%`, ")", sep = "")) %>%
  select(Variable = variable, level = value, fu)

## Combine tables
table2 <- left_join(table2, test_counts, by = c("Variable", "level")) %>%
  left_join(follow_up, by = c("Variable", "level")) %>%
  mutate(test = round(tests_conducted_any/`Fully vaccinated`*100, digits = 0),
         test_count = paste(tests_conducted_any, " (", test, ")", sep = "")) %>%
  select("Variable", "level", "Fully vaccinated", "fu", "test_count", 
         "Positive COVID test", "positivy", "PYs_1", "rate_1", "lci_1", "uci_1",
         "Hospitalised with COVID", "PYs_2", "rate_2", "lci_2", "uci_2",
         "COVID Deaths", "PYs_4", "rate_4", "lci_4", "uci_4")


# Redaction ----

## Redact values < 8
threshold = 8

table2_redacted <- table2 %>%
  mutate(`Fully vaccinated` = ifelse(`Fully vaccinated` < threshold, NA, as.numeric(`Fully vaccinated`)),
         fu = ifelse(is.na(`Fully vaccinated`), NA, fu),
         test_count = ifelse(is.na(`Fully vaccinated`), NA, test_count),
         `Positive COVID test` = ifelse(`Positive COVID test` < threshold, NA, `Positive COVID test`),
         positivy = ifelse(`Positive COVID test` < threshold, NA, positivy),
         PYs_1 = ifelse(`Positive COVID test` < threshold, NA, PYs_1),
         rate_1 = ifelse(`Positive COVID test` < threshold, NA, rate_1),
         lci_1 = ifelse(`Positive COVID test` < threshold, NA, lci_1),
         uci_1 = ifelse(`Positive COVID test` < threshold, NA, uci_1),
         `Hospitalised with COVID` = ifelse(`Hospitalised with COVID` < threshold, NA, `Positive COVID test`),
         PYs_2 = ifelse(`Hospitalised with COVID` < threshold, NA, PYs_2),
         rate_2 = ifelse(`Hospitalised with COVID` < threshold, NA, rate_2),
         lci_2 = ifelse(`Hospitalised with COVID` < threshold, NA, lci_2),
         uci_2 = ifelse(`Hospitalised with COVID` < threshold, NA, uci_2),
         `COVID Deaths` = ifelse(`COVID Deaths` < threshold, NA, `COVID Deaths`),
         PYs_4 = ifelse(is.na(`COVID Deaths`), NA, PYs_4),
         rate_4 = ifelse(is.na(`COVID Deaths`), NA, rate_4),
         lci_4 = ifelse(is.na(`COVID Deaths`), NA, lci_4),
         uci_4 = ifelse(is.na(`COVID Deaths`), NA, uci_4))

## Round to nearest 5
table2_redacted <- table2_redacted %>%
  mutate(`Fully vaccinated` = plyr::round_any(`Fully vaccinated`, 5),
         `Positive COVID test` = plyr::round_any(`Positive COVID test`, 5),
         `Hospitalised with COVID` = plyr::round_any(`Hospitalised with COVID`, 5),
         `COVID Deaths` = plyr::round_any(`COVID Deaths`, 5))

## Formatting
table2_redacted <- table2_redacted %>%
  mutate(Positive_test_count = paste(`Positive COVID test`, " (", PYs_1, ")", sep = ""),
         Positive_test_rate = paste(rate_1, " (", lci_1, "-", uci_1, ")", sep = ""),
         Hospitalised_count = paste(`Hospitalised with COVID`, " (", PYs_2, ")", sep = ""),
         Hospitalised_rate = paste(rate_2, " (", lci_2, "-", uci_2, ")", sep = ""),
         Death_count = paste(`COVID Deaths`, " (", PYs_4, ")", sep = ""),
         Death_rate = paste(rate_4, " (", lci_4, "-", uci_4, ")", sep = "")) %>%
  select(Variable, level, "Fully vaccinated", Follow_up = fu, Test_count = test_count, 
         Positive_test_count, Positivy = positivy, Positive_test_rate, Hospitalised_count, Hospitalised_rate,
         Death_count, Death_rate) 

# Save as html ----
gt::gtsave(gt(table2), here::here("output","tables", "table2.html"))
gt::gtsave(gt(table2_redacted), here::here("output","tables", "table2_redacted.html"))

