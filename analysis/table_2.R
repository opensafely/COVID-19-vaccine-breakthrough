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
data_processed <- read_rds(here::here("output", "data", "data_processed.rds"))


# Format data ----

## Filter on 80+ group
data_cohort_over80 <- data_processed %>%
  mutate(group = ifelse(care_home_65plus == 1, 1, NA),
         group = ifelse(is.na(group) & ageband == 3, 2, group),
         group = ifelse(is.na(group) & hscworker == 1, 3, group),
         group = ifelse(is.na(group) & ageband == 2, 4, group),
         group = ifelse(is.na(group) & shielded == 1, 5, group),
         group = ifelse(is.na(group) & age >=50 & age <70, 6, group),
         group = ifelse(is.na(group), 7, group),
         group = factor(group)) %>%
  filter(group == 2)

# Table 2 ----
rates0_over80 <- data_cohort_over80 %>%
  mutate(time_since_fully_vaccinated = cut(follow_up_time_vax2 - 14,
                                   breaks = c(14, 28, 42, 56, 84, Inf),
                                   labels = c("2-4 weeks", "4-6 weeks", "6-8 weeks", "8-12 weeks", "12+ weeks"),
                                   right = FALSE),
         
         time_between_vaccinations = cut(tbv,
                                         breaks = c(0, 42, 84, Inf),
                                         labels = c("6 weeks or less", "6-12 weeks", "12 weeks or more"),
                                         right = FALSE),
         
         smoking_status = ifelse(is.na(smoking_status), "N&M", smoking_status)) %>%
  select(ageband2,
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
         time_since_fully_vaccinated,
         time_between_vaccinations) %>%
  tbl_summary()

rates0_over80 <- rates0_over80$table_body %>%
  select(group = variable, variable = label, count = stat_0) %>%
  separate(count, c("count","perc"), sep = "([(])") %>%
  #mutate(count = as.numeric(count),
  #       perc = gsub('.{2}$', '', perc)) %>%
  mutate(count = gsub(" ", "", count),
         count = as.numeric(gsub(",", "", count))) %>%
  filter(!(is.na(count))) %>%
  select(-perc)

rates1_over80 <- calculate_rates(group = "covid_positive_test",
                                                   follow_up = "time_to_positive_test",
                                                   data = data_cohort_over80,
                                                   Y = 1, 
                                                   dig = 2,
                                                   variables = c("ageband2", "sex", "bmi", "smoking_status", "ethnicity",
                                                                 "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                                                 "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                                                 "chronic_kidney_disease",  "end_stage_renal","cld", 
                                                                 "diabetes", "immunosuppression", "learning_disability", 
                                                                 "sev_mental_ill", "organ_transplant", "time_since_fully_vaccinated",
                                                                 "time_between_vaccinations"))

rates2_over80 <- calculate_rates(group = "covid_hospital_admission",
                                 follow_up = "time_to_hospitalisation",
                                 data = data_cohort_over80,
                                 Y = 1, 
                                 dig = 2,
                                 variables = c("ageband2", "sex", "bmi", "smoking_status", "ethnicity",
                                               "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                               "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                               "chronic_kidney_disease",  "end_stage_renal","cld", 
                                               "diabetes", "immunosuppression", "learning_disability", 
                                               "sev_mental_ill", "organ_transplant", "time_since_fully_vaccinated",
                                               "time_between_vaccinations"))

rates3_over80 <- calculate_rates(group = "covid_death",
                                 follow_up = "time_to_covid_death",
                                 data = data_cohort_over80,
                                 Y = 1, 
                                 dig = 2,
                                 variables = c("ageband2", "sex", "bmi", "smoking_status", "ethnicity",
                                               "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                               "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                               "chronic_kidney_disease",  "end_stage_renal","cld", 
                                               "diabetes", "immunosuppression", "learning_disability", 
                                               "sev_mental_ill", "organ_transplant", "time_since_fully_vaccinated",
                                               "time_between_vaccinations"))  

table2_over80s <- left_join(rates0_over80, rates1_over80, by = c("group", "variable")) %>%
  left_join(rates2_over80, by = c("group", "variable")) %>%
  left_join(rates3_over80, by = c("group", "variable")) %>%
  mutate(follow_up.x = round(follow_up.x, digits = 0),
         follow_up.y = round(follow_up.y, digits = 0),
         follow_up = round(follow_up, digits = 0))

colnames(table2_over80s) = c("Variable", "level",
                             "Fully vaccinated",
                            "covid_positive_test", "PYs_1", "Rate1", "LCI1", "UCI1",
                            "covid_hospital_admission", "PYs_2", "Rate2", "LCI2", "UCI2",
                            "covid_death", "PYs_3", "Rate3", "LCI3", "UCI3")


# Redaction ----

## Redact values < 8
threshold = 8

table2_over80s_redacted <- table2_over80s %>%
  mutate(`Fully vaccinated` = ifelse(`Fully vaccinated` < threshold, NA, `Fully vaccinated`),
         covid_positive_test = ifelse(covid_positive_test < threshold, NA, covid_positive_test),
         PYs_1 = ifelse(is.na(covid_positive_test), NA, PYs_1),
         Rate1 = ifelse(is.na(covid_positive_test), NA, Rate1),
         LCI1 = ifelse(is.na(covid_positive_test), NA, LCI1),
         UCI1 = ifelse(is.na(covid_positive_test), NA, UCI1),
         covid_hospital_admission = ifelse(covid_hospital_admission < threshold, NA, covid_hospital_admission),
         PYs_2 = ifelse(is.na(covid_hospital_admission), NA, PYs_2),
         Rate2 = ifelse(is.na(covid_hospital_admission), NA, Rate2),
         LCI2 = ifelse(is.na(covid_hospital_admission), NA, LCI2),
         UCI2 = ifelse(is.na(covid_hospital_admission), NA, UCI2),
         covid_death = ifelse(covid_death < threshold, NA, covid_death),
         PYs_3 = ifelse(is.na(covid_death), NA, PYs_3),
         Rate3 = ifelse(is.na(covid_death), NA, Rate3),
         LCI3 = ifelse(is.na(covid_death), NA, LCI3),
         UCI3 = ifelse(is.na(covid_death), NA, UCI3))


## Round to nearest 5
table2_over80s_redacted <- table2_over80s_redacted %>%
  mutate(`Fully vaccinated` = plyr::round_any(`Fully vaccinated`, 5),
         covid_positive_test = plyr::round_any(covid_positive_test, 5),
         covid_hospital_admission = plyr::round_any(covid_hospital_admission, 5),
         covid_death = plyr::round_any(covid_death, 5))


# ## Recalculate column totals
# results.table_redacted[1, "Fully vaccinated"] <- sum(results.table_redacted[-1,]$`Fully vaccinated`, na.rm = T)
# results.table_redacted[1, "Positive COVID test"] <- sum(results.table_redacted[-1,]$`Positive COVID test`, na.rm = T)
# results.table_redacted[1, "Hospitalised with COVID"] <- sum(results.table_redacted[-1,]$`Hospitalised with COVID`, na.rm = T)
# results.table_redacted[1, "COVID Deaths"] <- sum(results.table_redacted[-1,]$`COVID Deaths`, na.rm = T)

## Replace na with [REDACTED]
# table2_over80s_redacted <- table2_over80s_redacted %>%
#   replace(is.na(.), "[REDACTED]")

## Formatting
table2_over80s_redacted <- table2_over80s_redacted %>%
  mutate(Fully_vaccinated_count = `Fully vaccinated`,
         Positive_test_count = paste(covid_positive_test, " (", PYs_1, ")", sep = ""),
         Positive_test_rate = paste(Rate1, " (", LCI1, "-", UCI1, ")", sep = ""),
         Hospitalised_count = paste(covid_hospital_admission, " (", PYs_2, ")", sep = ""),
         Hospitalised_rate = paste(Rate2, " (", LCI2, "-", UCI2, ")", sep = ""),
         Death_count = paste(covid_death, " (", PYs_3, ")", sep = ""),
         Death_rate = paste(Rate3, " (", LCI3, "-", UCI3, ")", sep = "")) %>%
  select(Variable, level, Fully_vaccinated_count, Positive_test_count, Positive_test_rate, Hospitalised_count, Hospitalised_rate,
         Death_count,  Death_rate)


# Save as html ----
gt::gtsave(gt(table2_over80s), here::here("output","tables", "table2.html"))
gt::gtsave(gt(table2_over80s_redacted), here::here("output","tables", "table2_redacted.html"))



