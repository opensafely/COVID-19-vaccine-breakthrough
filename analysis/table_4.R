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
           breaks = c(16, 50, 60, 70, 80, 85, 90, 95, Inf),
           labels = c("16-50", "50-59", "60-69", "70-79", "80-84", "85-89", "90-94", "95+"),
           right = FALSE))

# Table 3 ----

## All
## Calculate rates
rates0 <- data_processed %>%
  mutate(time_since_fully_vaccinated = cut(follow_up_time_vax2 - 14,
                                   breaks = c(14, 28, 42, 56, 84, Inf),
                                   labels = c("2-4 weeks", "4-6 weeks", "6-8 weeks", "8-12 weeks", "12+ weeks"),
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
         time_between_vaccinations,
         prior_covid_cat) %>%
  tbl_summary()

rates0$inputs$data <- NULL

rates0 <- rates0$table_body %>%
  select(group = variable, variable = label, count = stat_0) %>%
  separate(count, c("count","perc"), sep = "([(])") %>%
  # mutate(count = as.numeric(count),
  #        perc = gsub('.{2}$', '', perc)) %>%
  mutate(count = gsub(" ", "", count),
         count = as.numeric(gsub(",", "", count))) %>%
  filter(!(is.na(count))) %>%
  select(-perc)

rates1 <- calculate_rates(group = "covid_death",
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
                                        "time_between_vaccinations", "prior_covid_cat"))

table4_base <- left_join(rates0, rates1, by = c("group", "variable"))

colnames(table4_base) = c("Variable", "level",
                         "Fully vaccinated",
                         "covid_death", "PYs", "Rate1", "LCI1", "UCI1")
table4_base$group = 0

# Groups
table4 <- list()

for (i in 1:7){
  
  ## Filter on group
  data_group <- data_processed %>%
    filter(group == i)
  
  ## Calculate rates
  rates0 <- data_group %>%
    mutate(time_since_fully_vaccinated = cut(follow_up_time_vax2 - 14,
                                     breaks = c(14, 28, 42, 56, 84, Inf),
                                     labels = c("2-4 weeks", "4-6 weeks", "6-8 weeks", "8-12 weeks", "12+ weeks"),
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
           time_between_vaccinations,
           prior_covid_cat) %>%
    tbl_summary()
  
  rates0$inputs$data <- NULL
  
  rates0 <- rates0$table_body %>%
    select(group = variable, variable = label, count = stat_0) %>%
    separate(count, c("count","perc"), sep = "([(])") %>%
    # mutate(count = as.numeric(count),
    #        perc = gsub('.{2}$', '', perc)) %>%
    mutate(count = gsub(" ", "", count),
           count = as.numeric(gsub(",", "", count))) %>%
    filter(!(is.na(count))) %>%
    select(-perc)
  
  rates1 <- calculate_rates(group = "covid_death",
                                   follow_up = "time_to_covid_death",
                                   data = data_group,
                                   Y = 1, 
                                   dig = 2,
                                   variables = c("ageband3", "sex", "bmi", "smoking_status", "ethnicity",
                                                 "imd", "region", "asthma", "asplenia", "bpcat",  "chd",
                                                 "chronic_neuro_dis_inc_sig_learn_dis", "chronic_resp_dis",
                                                 "chronic_kidney_disease",  "end_stage_renal","cld", 
                                                 "diabetes", "immunosuppression", "learning_disability", 
                                                 "sev_mental_ill", "organ_transplant", "time_since_fully_vaccinated",
                                                 "time_between_vaccinations", "prior_covid_cat"))
  
  table4_tmp <- left_join(rates0, rates1, by = c("group", "variable"))
  
  colnames(table4_tmp) = c("Variable", "level",
                               "Fully vaccinated",
                               "covid_death", "PYs", "Rate1", "LCI1", "UCI1")
  table4_tmp$group = i
  
  table4 <- rbind(table4, table4_tmp)
  
}

table4 <- rbind(table4_base, table4)


# Redaction ----

## Redact values < 8
threshold = 8

table4_redacted <- table4 %>%
  mutate(`Fully vaccinated` = ifelse(`Fully vaccinated` < threshold, NA, as.numeric(`Fully vaccinated`)),
         covid_death = ifelse(covid_death < threshold, NA, covid_death),
         Rate1 = ifelse(is.na(covid_death), NA, Rate1),
         LCI1 = ifelse(is.na(covid_death), NA, LCI1),
         UCI1 = ifelse(is.na(covid_death), NA, UCI1))

# ## Round to nearest 5
table4_redacted <- table4_redacted %>%
  mutate(`Fully vaccinated` = plyr::round_any(`Fully vaccinated`, 5),
         covid_death = plyr::round_any(covid_death, 5))

## Recalculate totals

## Replace na with [REDACTED]
# table4_redacted[[i]] <- table4_redacted[[i]] %>%
#   replace(is.na(.), "[REDACTED]")

## Formatting
table4_redacted <- table4_redacted %>%
  mutate(PYs = round(PYs, digits = 0),
         Fully_vaccinated_count = `Fully vaccinated`,
         death_count = paste(covid_death, " (", PYs, ")", sep = ""),
         death_rate = paste(Rate1, " (", LCI1, "-", UCI1, ")", sep = "")) %>%
  select(Variable, level, Fully_vaccinated_count, death_count, death_rate, group)


# Save as html ----
gt::gtsave(gt(table4), here::here("output","tables", "table4.html"))
gt::gtsave(gt(table4_redacted), here::here("output","tables", "table4_redacted.html"))

