######################################

# This script 
# - produces a cumulative incidence plot of follow-up-time
# - saves plot as svg

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('lubridate')
library('reshape2')
library('here')
library('survival')
library('survminer')

## Custom function
fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

## Import data
data_processed <- read_rds(here::here("output", "data", "data_processed.rds"))
data_trial <- read_rds(here::here("output", "data", "data_processed_trial.rds")) %>%
  filter(time_to_positive_test >= 0)

## Format groups
data_processed <- data_processed %>%
  mutate(group = ifelse(care_home_65plus == 1, 1, NA),
         group = ifelse(is.na(group) & ageband == 3, 2, group),
         group = ifelse(is.na(group) & hscworker == 1, 3, group),
         group = ifelse(is.na(group) & ageband == 2, 4, group),
         group = ifelse(is.na(group) & shielded == 1, 5, group),
         group = ifelse(is.na(group) & age >=50 & age <70, 6, group),
         group = ifelse(is.na(group), 7, group),
         
         group = fct_case_when(
           group == "1" ~ "Care home (priority group 1)",
           group == "2" ~ "80+ (priority group 2)",
           group == "3" ~ "Health/care workers (priority groups 1-2)",
           group == "4" ~ "70-79 (priority groups 3-4)",
           group == "5" ~ "Shielding (age 16-69) (priority group 4)",
           group == "6" ~ "50-69 (priority groups 5-9)",
           group == "7" ~ "Others not in the above groups (under 50)",
           #TRUE ~ "Unknown"
           TRUE ~ NA_character_)) %>%
  filter(time_to_positive_test >= 0)
  


# Plot ----
threshold <- 7

## Data
surv_data_all <- survfit(Surv(time = time_to_positive_test, event = covid_positive_test) ~ 1, 
                         data = data_processed)  %>% 
  broom::tidy() %>% 
  filter(estimate > 0) %>%
  mutate(
    estimate = pmin(1,plyr::round_any(estimate, threshold/max(n.risk)), na.rm=TRUE),
    conf.low = pmin(1, plyr::round_any(conf.low, threshold/max(n.risk)), na.rm=TRUE),
    conf.high = pmin(1, plyr::round_any(conf.high, threshold/max(n.risk)), na.rm=TRUE),
    cum.in = 1 - estimate,
    lci = 1- conf.high,
    uci = 1 - conf.low
  ) %>%
  select(time, cum.in, lci, uci)

surv_data_groups <- survfit(Surv(time = time_to_positive_test, event = covid_positive_test) ~ group, 
                            data = data_processed) %>% 
  broom::tidy() %>% 
  filter(estimate > 0) %>%
  mutate(
    estimate = pmin(1,plyr::round_any(estimate, threshold/max(n.risk)), na.rm=TRUE),
    conf.low = pmin(1, plyr::round_any(conf.low, threshold/max(n.risk)), na.rm=TRUE),
    conf.high = pmin(1, plyr::round_any(conf.high, threshold/max(n.risk)), na.rm=TRUE),
    cum.in = 1 - estimate,
    lci = 1- conf.high,
    uci = 1 - conf.low
  ) %>%
  mutate(group = gsub(".*=","", strata),
         group = factor(group, levels = c("All",
                                          "Care home (priority group 1)",
                                          "80+ (priority group 2)",
                                          "Health/care workers (priority groups 1-2)",
                                          "70-79 (priority groups 3-4)",
                                          "Shielding (age 16-69) (priority group 4)",
                                          "50-69 (priority groups 5-9)",
                                          "Others not in the above groups (under 50)"))) %>%
  select(time, cum.in, lci, uci)

surv_data_trial <- survfit(Surv(time = time_to_positive_test, event = covid_positive_test) ~ 1, 
                         data = data_trial)  %>% 
  broom::tidy() %>% 
  filter(estimate > 0) %>%
  mutate(
    estimate = pmin(1,plyr::round_any(estimate, threshold/max(n.risk)), na.rm=TRUE),
    conf.low = pmin(1, plyr::round_any(conf.low, threshold/max(n.risk)), na.rm=TRUE),
    conf.high = pmin(1, plyr::round_any(conf.high, threshold/max(n.risk)), na.rm=TRUE),
    cum.in = 1 - estimate,
    lci = 1- conf.high,
    uci = 1 - conf.low
  ) %>%
  select(time, cum.in, lci, uci)

surv_data_risk_table <- ggsurvplot(survfit(Surv(time = time_to_positive_test, event = covid_positive_test) ~ group, 
                   data = data_processed), risk.table = TRUE)$data.survtable %>%
  select(group, time, n.risk) %>%
  mutate(`n.risk` = ifelse(`n.risk` < 8, "<8", `n.risk`),
         group = factor(group, levels = c("Care home (priority group 1)",
                                          "80+ (priority group 2)",
                                          "Health/care workers (priority groups 1-2)",
                                          "70-79 (priority groups 3-4)",
                                          "Shielding (age 16-69) (priority group 4)",
                                          "50-69 (priority groups 5-9)",
                                          "Others not in the above groups (under 50)")))

surv_data_risk_table_trial <- ggsurvplot(survfit(Surv(time = time_to_positive_test, event = covid_positive_test) ~ 1, 
                                           data = data_trial), risk.table = TRUE)$data.survtable %>%
  select(time, n.risk) %>%
  mutate(`n.risk` = ifelse(`n.risk` < 8, "<8", `n.risk`),
         group = "Trial")
  



## Save data behind plots
write_csv(surv_data_all, here::here("output", "data", "surv_data_all.csv"))
write_csv(surv_data_groups, here::here("output", "data", "surv_data_groups.csv"))
write_csv(surv_data_trial, here::here("output", "data", "surv_data_trial.csv"))
write_csv(surv_data_risk_table, here::here("output", "data", "surv_data_risk_table.csv"))
write_csv(surv_data_risk_table_trial, here::here("output", "data", "surv_data_risk_table_trial.csv"))

