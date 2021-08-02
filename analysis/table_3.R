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
         group = factor(group))


# Table 3 ----
table3 <- list()
table3_redacted <- list()

for (i in 1:7){
  
  ## Filter on group
  data_group <- data_processed %>%
    filter(group == i)
  
  ## Calculate rates
  rates0 <- data_group %>%
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
    select(sex,
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
    tbl_summary()
  
  rates0$inputs$data <- NULL
  
  rates0 <- rates0$table_body %>%
    select(group = variable, variable = label, count = stat_0) %>%
    separate(count, c("count","perc"), sep = "([(])") %>%
    mutate(count = as.numeric(count),
           perc = gsub('.{2}$', '', perc)) %>%
    filter(!(is.na(count))) %>%
    select(-perc)
  
  rates1 <- calculate_rates(group = "covid_positive_post_2vacc",
                                   follow_up = "time_to_positive_test",
                                   data = data_group,
                                   Y = 100000, 
                                   dig = 2,
                                   variables = c("sex", "bmi", "smoking_status", "ethnicity",
                                                 "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                                 "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                                 "chronic_kidney_disease",  "end_stage_renal","cld", 
                                                 "diabetes", "immunosuppression", "learning_disability", 
                                                 "sev_mental_ill", "organ_transplant", "time_since_2nd_dose",
                                                 "time_between_vaccinations", "prior_covid"))
  
  table3[[i]] <- left_join(rates0, rates1, by = c("group", "variable"))
  
  colnames(table3[[i]]) = c("Variable", "level",
                               "Fully vaccinated",
                               "covid_positive_post_2vacc", "Rate1", "LCI1", "UCI1",
                               "covid_hospital_admission", "Rate2", "LCI2", "UCI2",
                               "covid_death", "Rate3", "LCI3", "UCI3")
  
  ## Redact values < 8
  threshold = 8
  
  table3_redacted[[i]] <- table3[[i]] %>%
    mutate(`Fully vaccinated` = ifelse(`Fully vaccinated` < threshold, NA, as.numeric(`Fully vaccinated`)),
           covid_positive_post_2vacc = ifelse(covid_positive_post_2vacc < threshold, NA, covid_positive_post_2vacc),
           Rate1 = ifelse(is.na(covid_positive_post_2vacc), NA, Rate1),
           LCI1 = ifelse(is.na(covid_positive_post_2vacc), NA, LCI1),
           UCI1 = ifelse(is.na(covid_positive_post_2vacc), NA, UCI1))
  
  # ## Round to nearest 5
  # table3_redacted[[i]] <- table3_redacted[[i]] %>%
  #   mutate(`Fully vaccinated` = plyr::round_any(`Fully vaccinated`, 5),
  #          covid_positive_post_2vacc = plyr::round_any(covid_positive_post_2vacc, 5))
  
  ## Recalculate totals

  ## Replace na with [REDACTED]
  # table3_redacted[[i]] <- table3_redacted[[i]] %>%
  #   replace(is.na(.), "[REDACTED]")
  
}

# Single table
table3_single <- left_join(table3[[1]], table3[[2]], by = c("Variable", "level", "Fully vaccinated")) %>%
  left_join(table3[[3]], by = c("Variable", "level", "Fully vaccinated")) %>%
  left_join(table3[[4]], by = c("Variable", "level", "Fully vaccinated")) %>%
  left_join(table3[[5]], by = c("Variable", "level", "Fully vaccinated")) %>%
  left_join(table3[[6]], by = c("Variable", "level", "Fully vaccinated")) %>%
  left_join(table3[[7]], by = c("Variable", "level", "Fully vaccinated"))

table3_redacted_single <- left_join(table3_redacted[[1]], table3_redacted[[2]], by = c("Variable", "level", "Fully vaccinated")) %>%
  left_join(table3_redacted[[3]], by = c("Variable", "level", "Fully vaccinated")) %>%
  left_join(table3_redacted[[4]], by = c("Variable", "level", "Fully vaccinated")) %>%
  left_join(table3_redacted[[5]], by = c("Variable", "level", "Fully vaccinated")) %>%
  left_join(table3_redacted[[6]], by = c("Variable", "level", "Fully vaccinated")) %>%
  left_join(table3_redacted[[7]], by = c("Variable", "level", "Fully vaccinated"))

# Save as html ----
gt::gtsave(gt(table3_single), here::here("output","tables", "table3.html"))
gt::gtsave(gt(table3_redacted_single), here::here("output","tables", "table3_redacted.html"))

