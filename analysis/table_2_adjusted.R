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
positive_test_rates <- calculate_age_adjusted_rates(group = "covid_positive_test",
                                       follow_up = "time_to_positive_test",
                                       data = data_processed,
                                       Y = 10000, 
                                       dig = 0,
                                       variables = c("chronic_kidney_disease"))

table2 <- left_join(counts, positive_test_rates, by = c("group", "variable"))

## Hospitalisation rates
hospitalisation_rates <- calculate_age_adjusted_rates(group = "covid_hospital_admission",
                                         follow_up = "time_to_hospitalisation",
                                         data = data_processed,
                                         Y = 10000, 
                                         dig = 0,
                                         variables = c("chronic_kidney_disease"))

table2 <- left_join(table2, hospitalisation_rates, by = c("group", "variable"))


## Critical care with COVID rates
critial_care_rates <- calculate_age_adjusted_rates(group = "covid_hospitalisation_critical_care",
                                      follow_up = "time_to_hospitalisation",
                                      data = data_processed,
                                      Y = 10000, 
                                      dig = 0,
                                      variables = c("chronic_kidney_disease"))

table2 <- left_join(table2, critial_care_rates, by = c("group", "variable"))

## Death rates
death_rates <- calculate_age_adjusted_rates(group = "covid_death",
                               follow_up = "time_to_covid_death",
                               data = data_processed,
                               Y = 10000, 
                               dig = 0,
                               variables = c("chronic_kidney_disease"))

rates.adj <- data_processed %>%
  mutate(time_since_fully_vaccinated = cut(follow_up_time_vax2 - 14,
                                           breaks = c(0, 28, 56, 84, Inf),
                                           labels = c("0-4 weeks", "4-8 weeks", "8-12 weeks", "12+ weeks"),
                                           right = FALSE),
         
         time_between_vaccinations = cut(tbv,
                                         breaks = c(0, 42, 84, Inf),
                                         labels = c("6 weeks or less", "6-12 weeks", "12 weeks or more"),
                                         right = FALSE),
         
         smoking_status = ifelse(is.na(smoking_status), "N&M", smoking_status),
         asthma = ifelse(asthma == 1, "asthma", NA),
         asplenia = ifelse(asplenia == 1, "asplenia", NA),
         cancer = ifelse(cancer == 1, "cancer", NA),
         haem_cancer = ifelse(haem_cancer == 1, "haem_cancer", NA),
         chd = ifelse(chd == 1, "chd", NA),
         chronic_neuro_dis_inc_sig_learn_dis = ifelse(chronic_neuro_dis_inc_sig_learn_dis == 1, "chronic_neuro_dis_inc_sig_learn_dis", NA),
         chronic_resp_dis = ifelse(chronic_resp_dis == 1, "chronic_resp_dis", NA),
         #chronic_kidney_disease = ifelse(chronic_kidney_disease == 1, "chronic_kidney_disease", NA),
         #end_stage_renal = ifelse(end_stage_renal == 1, "end_stage_renal", NA),
         cld = ifelse(cld == 1, "cld", NA),
         diabetes = ifelse(diabetes == 1, "diabetes", NA),
         immunosuppression = ifelse(immunosuppression == 1, "immunosuppression", NA),
         learning_disability = ifelse(learning_disability == 1, "learning_disability", NA),
         #organ_transplant = ifelse(organ_transplant == 1, "organ_transplant", NA),
         sev_mental_ill = ifelse(sev_mental_ill == 1, "sev_mental_ill", NA)) %>%
  select(group = covid_death,
         age,
         person_time = time_to_covid_death,
         variable = chronic_kidney_disease) %>%
  filter(person_time > -1) %>%
  mutate(variable = as.character(variable),
         variable = ifelse(variable == "", "Unknown", variable),
         variable = ifelse(is.na(variable), "Unknown", variable)) %>%
  group_by(variable, age) %>%
  summarise(n_postest = sum(group==1, na.rm = T),
            n_postest = ifelse(is.na(n_postest), 0, n_postest),
            person_time = sum(person_time, na.rm = T),
            person_time = ifelse(is.na(person_time), 0, person_time))

print(rates.adj)

rates.adj.mod <- glm(n_postest ~ 1 + variable + age + offset(log(person_time/(365.25*1000))), 
                     family = "poisson", data = rates.adj)
print(rates.adj.mod)

table2 <- left_join(table2, death_rates, by = c("group", "variable"))

colnames(table2) = c("Variable", "level",
                     "Fully vaccinated",
                     "Positive COVID test", "PYs_1", "rate_1", "lci_1", "uci_1", "rate_1_adj", "lci_1_adj", "uci_1_adj",
                     "Hospitalised with COVID", "PYs_2", "rate_2", "lci_2", "uci_2", "rate_2_adj", "lci_2_adj", "uci_2_adj",
                     "Critical care with COVID", "PYs_3", "rate_3", "lci_3", "uci_3", "rate_3_adj", "lci_3_adj", "uci_3_adj",
                     "COVID Deaths", "PYs_4", "rate_4", "lci_4", "uci_4", "rate_4_adj", "lci_4_adj", "uci_4_adj")

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
         #chronic_kidney_disease = ifelse(chronic_kidney_disease == 1, "chronic_kidney_disease", NA),
         end_stage_renal = ifelse(end_stage_renal == 1, "end_stage_renal", NA),
         cld = ifelse(cld == 1, "cld", NA),
         diabetes = ifelse(diabetes == 1, "diabetes", NA),
         immunosuppression = ifelse(immunosuppression == 1, "immunosuppression", NA),
         learning_disability = ifelse(learning_disability == 1, "learning_disability", NA),
         sev_mental_ill = ifelse(sev_mental_ill == 1, "sev_mental_ill", NA),
         organ_transplant = ifelse(organ_transplant == 1, "organ_transplant", NA)) %>%
  select(tests_conducted_any,
         tests_conducted_positive,
         chronic_kidney_disease) %>%
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
  mutate(positivy = tests_conducted_positive/tests_conducted_any*100) %>%
  select(Variable = variable, level = value, test_0, test_1, test_2, test_3, tests_conducted_any, positivy)

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
         #chronic_kidney_disease = ifelse(chronic_kidney_disease == 1, "chronic_kidney_disease", NA),
         end_stage_renal = ifelse(end_stage_renal == 1, "end_stage_renal", NA),
         cld = ifelse(cld == 1, "cld", NA),
         diabetes = ifelse(diabetes == 1, "diabetes", NA),
         immunosuppression = ifelse(immunosuppression == 1, "immunosuppression", NA),
         learning_disability = ifelse(learning_disability == 1, "learning_disability", NA),
         sev_mental_ill = ifelse(sev_mental_ill == 1, "sev_mental_ill", NA),
         organ_transplant = ifelse(organ_transplant == 1, "organ_transplant", NA)) %>%
  select(follow_up_time,
         chronic_kidney_disease) %>%
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
  mutate(test_0 = round(test_0/`Fully vaccinated`*100, digits = 0),
         test_1 = round(test_1/`Fully vaccinated`*100, digits = 0),
         test_2 = round(test_2/`Fully vaccinated`*100, digits = 0),
         test_3 = round(test_3/`Fully vaccinated`*100, digits = 0),
         positivy = round(positivy, digits = 2)) %>%
  mutate() %>%
  select("Variable", "level", "Fully vaccinated", "fu", "test_0", "test_1", "test_2", "test_3",
         "Positive COVID test", "positivy", "PYs_1", "rate_1", "lci_1", "uci_1", "rate_1_adj", "lci_1_adj", "uci_1_adj",
         "Hospitalised with COVID", "PYs_2", "rate_2", "lci_2", "uci_2", "rate_2_adj", "lci_2_adj", "uci_2_adj",
         "Critical care with COVID", "PYs_3", "rate_3", "lci_3", "uci_3", "rate_3_adj", "lci_3_adj", "uci_3_adj",
         "COVID Deaths", "PYs_4", "rate_4", "lci_4", "uci_4", "rate_4_adj", "lci_4_adj", "uci_4_adj")


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
         rate_1_adj = ifelse(`Positive COVID test` < threshold, NA, rate_1_adj),
         lci_1_adj = ifelse(`Positive COVID test` < threshold, NA, lci_1_adj),
         uci_1_adj = ifelse(`Positive COVID test` < threshold, NA, uci_1_adj),
         `Hospitalised with COVID` = ifelse(`Hospitalised with COVID` < threshold, NA, `Hospitalised with COVID`),
         PYs_2 = ifelse(`Hospitalised with COVID` < threshold, NA, PYs_2),
         rate_2 = ifelse(`Hospitalised with COVID` < threshold, NA, rate_2),
         lci_2 = ifelse(`Hospitalised with COVID` < threshold, NA, lci_2),
         uci_2 = ifelse(`Hospitalised with COVID` < threshold, NA, uci_2),
         rate_2_adj = ifelse(`Hospitalised with COVID` < threshold, NA, rate_2_adj),
         lci_2_adj = ifelse(`Hospitalised with COVID` < threshold, NA, lci_2_adj),
         uci_2_adj = ifelse(`Hospitalised with COVID` < threshold, NA, uci_2_adj),
         `Critical care with COVID` = ifelse(`Critical care with COVID` < threshold, NA, `Critical care with COVID`),
         PYs_3 = ifelse(`Critical care with COVID` < threshold, NA, PYs_3),
         rate_3 = ifelse(`Critical care with COVID` < threshold, NA, rate_3),
         lci_3 = ifelse(`Critical care with COVID` < threshold, NA, lci_3),
         uci_3 = ifelse(`Critical care with COVID` < threshold, NA, uci_3),
         rate_3_adj = ifelse(`Critical care with COVID` < threshold, NA, rate_3_adj),
         lci_3_adj = ifelse(`Critical care with COVID` < threshold, NA, lci_3_adj),
         uci_3_adj = ifelse(`Critical care with COVID` < threshold, NA, uci_3_adj),
         `COVID Deaths` = ifelse(`COVID Deaths` < threshold, NA, `COVID Deaths`),
         PYs_4 = ifelse(is.na(`COVID Deaths`), NA, PYs_4),
         rate_4 = ifelse(is.na(`COVID Deaths`), NA, rate_4),
         lci_4 = ifelse(is.na(`COVID Deaths`), NA, lci_4),
         uci_4 = ifelse(is.na(`COVID Deaths`), NA, uci_4),
         rate_4_adj = ifelse(is.na(`COVID Deaths`), NA, rate_4_adj),
         lci_4_adj = ifelse(is.na(`COVID Deaths`), NA, lci_4_adj),
         uci_4_adj = ifelse(is.na(`COVID Deaths`), NA, uci_4_adj)) %>%
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
         Positive_test_rate_adj = paste(rate_1_adj, " (", lci_1_adj, "-", uci_1_adj, ")", sep = ""),
         Hospitalised_count = paste(`Hospitalised with COVID`, " (", PYs_2, ")", sep = ""),
         Hospitalised_rate = paste(rate_2, " (", lci_2, "-", uci_2, ")", sep = ""),
         Hospitalised_rate_adj = paste(rate_2_adj, " (", lci_2_adj, "-", uci_2_adj, ")", sep = ""),
         Critial_Care_count = paste(`Critical care with COVID`, " (", PYs_3, ")", sep = ""),
         Critial_Care_rate = paste(rate_3, " (", lci_3, "-", uci_3, ")", sep = ""),
         Critial_Care_rate_adj = paste(rate_3_adj, " (", lci_3_adj, "-", uci_3_adj, ")", sep = ""),
         Death_count = paste(`COVID Deaths`, " (", PYs_4, ")", sep = ""),
         Death_rate = paste(rate_4, " (", lci_4, "-", uci_4, ")", sep = ""),
         Death_rate_adj = paste(rate_4_adj, " (", lci_4_adj, "-", uci_4_adj, ")", sep = "")) %>%
  select(Variable, level, "Fully vaccinated", Follow_up = fu, test_0, test_1, test_2, test_3, 
         Positivy = positivy, Positive_test_count, Positive_test_rate, Positive_test_rate_adj,
         Hospitalised_count, Hospitalised_rate, Hospitalised_rate_adj,
         Critial_Care_count, Critial_Care_rate, Critial_Care_rate_adj,
         Death_count, Death_rate, Death_rate_adj) 

# Save as html ----
gt::gtsave(gt(table2), here::here("output","tables", "table2_adj.html"))
gt::gtsave(gt(table2_redacted), here::here("output","tables", "table2_adj_redacted.html"))

