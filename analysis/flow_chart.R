# # # # # # # # # # # # # # # # # # # # #
# This script:
# imports processed data
# creates indicator variables for each potential outcome combination of interest
# creates a metadata df that describes the cohort
# # # # # # # # # # # # # # # # # # # # #


# Preliminaries ----

## Import libraries
library('tidyverse')
library('here')
library('glue')

## Import command-line arguments
args <- commandArgs(trailingOnly = TRUE)

## Create output directories ----
dir.create(here("output", "data"), showWarnings = FALSE, recursive = TRUE)

## Import processed data
data_processed <- read_rds(here("output", "data", "data_all.rds"))
data_processed_final <-  read_rds(here::here("output", "data", "data_processed.rds"))


# Exclusion criteria ----
data_criteria <- data_processed %>%
  filter(follow_up_time_vax2 >=14) %>%
  mutate(
    patient_id,
    has_age = (age >=16 & age < 110),
    has_sex = !is.na(sex) & !(sex %in% c("I", "U")),
    no_outcomes_within_2_weeks_post_vax2_1 = (covid_positive_test_within_2_weeks_post_vax2 == 0),
    no_outcomes_within_2_weeks_post_vax2_2 = (covid_hospitalisation_within_2_weeks_post_vax2 == 0),
    no_outcomes_within_2_weeks_post_vax2_3 = (covid_death_within_2_weeks_post_vax2 == 0),
    include = (
      has_age & 
        has_sex & 
        no_outcomes_within_2_weeks_post_vax2_1 & 
        no_outcomes_within_2_weeks_post_vax2_2 & 
        no_outcomes_within_2_weeks_post_vax2_3),
  )

# Flowchart data
data_flowchart <- data_criteria %>%
  transmute(
    c0_all = TRUE,
    c1_notmissing = c0_all & has_age & has_sex,
    c2_no_outcomes_within_2_weeks_post_vax2_1 = c0_all & has_age & has_sex & no_outcomes_within_2_weeks_post_vax2_1,
    c3_no_outcomes_within_2_weeks_post_vax2_2 = c0_all & has_age & has_sex & no_outcomes_within_2_weeks_post_vax2_1 & 
      no_outcomes_within_2_weeks_post_vax2_2,
    c4_no_outcomes_within_2_weeks_post_vax2_3 = c0_all & has_age & has_sex & no_outcomes_within_2_weeks_post_vax2_1 & 
      no_outcomes_within_2_weeks_post_vax2_2 & no_outcomes_within_2_weeks_post_vax2_3
  ) %>%
  summarise(
    across(.fns=sum, na.rm = T)
  ) %>%
  pivot_longer(
    cols=everything(),
    names_to="criteria",
    values_to="n"
  ) %>%
  mutate(
    n_exclude = lag(n) - n,
    pct_exclude = n_exclude/lag(n),
    pct_all = n / first(n),
    pct_step = n / lag(n),
  )


# Save dataset as .csv files ----
write_csv(data_flowchart, here("output", "data", "flowchart.csv"))