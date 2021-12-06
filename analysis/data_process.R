######################################

# This script:
# - imports data extracted by the cohort extractor
# - combines ethnicity columns
# - standardises some variables (eg convert to factor) and derives some new ones
# - saves processed dataset

######################################


# Preliminaries ----

## Import libraries
library('tidyverse')
library('lubridate')

## Custom functions
fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

tte <- function(origin_date, event_date, censor_date, na.censor=FALSE){
  # returns time-to-event date or time to censor date, which is earlier
  
  if (na.censor)
    time <- event_date-origin_date
  else
    time <- pmin(event_date-origin_date, censor_date-origin_date, na.rm=TRUE)
  as.numeric(time)
}

## Output processed data to rds
dir.create(here::here("output", "data"), showWarnings = FALSE, recursive=TRUE)


# Process data ----

## Print variable names
read_csv(here::here("output", "data", "input.csv"),
         n_max = 0,
         col_types = cols()) %>%
  names() %>%
  print()

## Read in data (don't rely on defaults)
data_extract0 <- read_csv(
  here::here("output", "data", "input.csv"),
  col_types = cols_only(
    
    # Identifier
    patient_id = col_integer(),
    
    # Outcomes
    covid_positive_test_date = col_date(format="%Y-%m-%d"),
    covid_positive_test_within_2_weeks_post_vax2 = col_logical(),
    
    covid_hospital_admission = col_logical(),
    covid_hospital_admission_date = col_date(format="%Y-%m-%d"),
    covid_hospitalisation_within_2_weeks_post_vax2 = col_logical(),
    
    covid_hospitalisation_critical_care = col_integer(),
    
    death_with_covid_on_the_death_certificate = col_logical(),
    death_with_28_days_of_covid_positive_test = col_logical(),
    covid_death_within_2_weeks_post_vax2 = col_logical(),
    
    # Censoring
    dereg_date = col_date(format="%Y-%m-%d"),
    death_date = col_date(format="%Y-%m-%d"),

    # Priority groups
    care_home = col_logical(),
    shielded = col_logical(),
    hscworker = col_logical(),
    age = col_integer(),
    
    # Clinical/demographic variables
    sex = col_character(),
    bmi = col_character(),
    smoking_status =  col_character(),
    ethnicity_6 = col_character(),
    ethnicity_6_sus = col_character(),
    imd = col_character(),
    region = col_character(),
    asthma = col_logical(),
    asplenia = col_logical(),
    bp_sys = col_double(),
    bp_dias = col_double(),
    cancer = col_logical(),
    chd = col_logical(),
    chronic_neuro_dis_inc_sig_learn_dis = col_logical(),
    chronic_resp_dis = col_logical(),
    chronic_kidney_disease_diagnostic = col_date(format="%Y-%m-%d"),
    chronic_kidney_disease_all_stages = col_date(format="%Y-%m-%d"),
    chronic_kidney_disease_all_stages_3_5 = col_date(format="%Y-%m-%d"),
    creatinine = col_double(),
    end_stage_renal = col_logical(), 
    cld = col_logical(),
    diabetes = col_logical(),
    haem_cancer = col_logical(),
    immunosuppression_diagnosis_date = col_date(format="%Y-%m-%d"),
    immunosuppression_medication_date = col_date(format="%Y-%m-%d"),
    learning_disability = col_logical(),
    sev_mental_ill = col_date(format="%Y-%m-%d"),
    organ_transplant = col_logical(),
    
    # Other
    covid_vax_1_date = col_date(format="%Y-%m-%d"),
    covid_vax_2_date = col_date(format="%Y-%m-%d"),
    prior_positive_test_date = col_date(format="%Y-%m-%d"),
    prior_primary_care_covid_case_date = col_date(format="%Y-%m-%d"),
    prior_covidadmitted_date = col_date(format="%Y-%m-%d"),
    tests_conducted_any = col_double(),
    tests_conducted_positive = col_double()
    
  ),
  na = character() # more stable to convert to missing later
)

## Parse NAs
data_extract <- data_extract0 %>%
  mutate(across(
    .cols = where(is.character),
    .fns = ~na_if(.x, "")
  )) %>%
  mutate(across(
    .cols = c(where(is.numeric), -ends_with("_id")), #convert numeric+integer but not id variables
    .fns = ~na_if(.x, 0)
  )) %>%
  arrange(patient_id) %>%
  select(all_of((names(data_extract0))))

## Censor dates for outcomes
cat("#### Latest date for positive test ####\n")
data_extract %>%
  filter(covid_positive_test_within_2_weeks_post_vax2 == 0) %>%
  group_by(covid_positive_test_date) %>%
  summarise(n = n()) %>%
  tail()

cat("#### Latest date for covid hospitilisation ####\n")
data_extract %>%
  filter(covid_hospitalisation_within_2_weeks_post_vax2 == 0,
         covid_hospital_admission == 1) %>%
  group_by(covid_hospital_admission_date) %>%
  summarise(n = n()) %>%
  tail()

cat("#### Latest date for deaths ####\n")
data_extract %>%
  filter(covid_death_within_2_weeks_post_vax2 == 0) %>%
  group_by(death_date) %>%
  summarise(n = n()) %>%
  tail()

censor_dates <- data_extract %>%
  filter(covid_positive_test_within_2_weeks_post_vax2 == 0,
         covid_hospitalisation_within_2_weeks_post_vax2 == 0,
         covid_death_within_2_weeks_post_vax2 == 0) %>%
  select(patient_id, covid_positive_test_date, covid_hospital_admission_date, death_date) %>%
  reshape2::melt(id.var = "patient_id") %>%
  select(-patient_id) %>%
  filter(!is.na(value)) %>%
  group_by(variable, value) %>%
  tally() %>%
  filter(n > 5) %>%
  group_by(variable) %>%
  filter(value == max(value, na.rm = T)) %>%
  mutate(end_date = ifelse(variable == "covid_hospital_admission_date", value, floor_date(value, "month")),
         end_date = as.Date(end_date, origin = "1970-01-01"))

print(censor_dates)

## Format columns (i.e, set factor levels)
data_processed <- data_extract %>%
  mutate(
    
    # Positive test
    covid_positive_test = ifelse(is.na(covid_positive_test_date), 0, 1),

    # COVID hospital admission
    covid_hospital_admission_date = ifelse(covid_hospital_admission == 1, covid_hospital_admission_date, NA),
    covid_hospital_admission_date = as.Date(covid_hospital_admission_date, origin = "1970-01-01"),
    
    # COVID-related ITU 
    covid_hospitalisation_critical_care = ifelse(covid_hospitalisation_critical_care > 0 & covid_hospital_admission == 1, 1, 0),
    
    # COVID-related death
    covid_death = ifelse(death_with_covid_on_the_death_certificate == 1 |
                           death_with_28_days_of_covid_positive_test == 1, 1, 0),
    covid_death_date = ifelse(death_with_covid_on_the_death_certificate == 1 |
                                death_with_28_days_of_covid_positive_test == 1, death_date, NA),
    covid_death_date = as.Date(covid_death_date, origin = "1970-01-01"),
    
    # End date
    end_date_1 = subset(censor_dates, variable == "covid_positive_test_date")$end_date,
    end_date_2 = subset(censor_dates, variable == "covid_hospital_admission_date")$end_date,
    
    # Censoring
    censor_date_1 = pmin(death_date, 
                       dereg_date, 
                       end_date_1, 
                       na.rm = TRUE),
    
    censor_date_2 = pmin(death_date, 
                         dereg_date, 
                         end_date_2, 
                         na.rm = TRUE),
    
    # Time since first dose
    follow_up_time_vax1 = tte(covid_vax_1_date,
                              end_date_1,
                              censor_date_1),
    
    # Time since second dose
    follow_up_time_vax2 = tte(covid_vax_2_date,
                              end_date_1,
                              censor_date_1),
    
    # Time to positive test
    time_to_positive_test = tte(covid_vax_2_date + 14,
                                covid_positive_test_date,
                                censor_date_1),
    
    # Time to hospitalisation
    time_to_hospitalisation = tte(covid_vax_2_date + 14,
                                  covid_hospital_admission_date,
                                  censor_date_2),
    
    # Time to hospitalisation critical care
    time_to_itu = tte(covid_vax_2_date + 14,
                      covid_hospital_admission_date,
                      censor_date_2),
    
    # Time to covid death
    time_to_covid_death = tte(covid_vax_2_date + 14,
                              covid_death_date,
                              censor_date_1),
    
    # Care home (65+)
    care_home_65plus = ifelse(care_home == 1 & age >=65, 1, 0),
    
    # Shielding
    shielded = ifelse(shielded == 1 & (age >=16 & age < 70), 1, 0),
    
    # Age
    ageband = cut(
      age,
      breaks = c(16, 70, 80, Inf),
      labels = c("16-69", "70-79", "80+"),
      right = FALSE
    ),
    
    ageband = ifelse(care_home == 1, NA, ageband),
    
    ageband2 = cut(
      age,
      breaks = c(16, 80, 85, 90, 95, Inf),
      labels = c("16-79", "80-84", "85-89", "90-94", "95+"),
      right = FALSE
    ),
    
    # Sex
    sex = fct_case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male",
      #sex == "I" ~ "Inter-sex",
      #sex == "U" ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
    # BMI
    bmi = ifelse(bmi == "Not obese","Not obese", "Obese"),
    
    # Smoking status
    smoking_status = ifelse(smoking_status == "E" | smoking_status == "S","S&E", smoking_status),
    smoking_status = ifelse(smoking_status == "S&E", "S&E", "N&M"),
    
    # Ethnicity
    ethnicity_filled = ifelse(is.na(ethnicity_6), ethnicity_6_sus, ethnicity_6),
    ethnicity = ifelse(is.na(ethnicity_filled), 6, ethnicity_filled),
    
    ethnicity = fct_case_when(
      ethnicity == "1" ~ "White",
      ethnicity == "2" ~ "Mixed",
      ethnicity == "3" ~ "Asian or Asian British",
      ethnicity == "4" ~ "Black or Black British",
      ethnicity == "5" ~ "Other ethnic groups",
      ethnicity == "6" ~ "Unknown",
      #TRUE ~ "Unknown"
      TRUE ~ NA_character_),
    
    # IMD
    imd = na_if(imd, "0"),
    imd = fct_case_when(
      imd == 1 ~ "1 most deprived",
      imd == 2 ~ "2",
      imd == 3 ~ "3",
      imd == 4 ~ "4",
      imd == 5 ~ "5 least deprived",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
    # Region
    region = fct_case_when(
      region == "London" ~ "London",
      region == "East" ~ "East of England",
      region == "East Midlands" ~ "East Midlands",
      region == "North East" ~ "North East",
      region == "North West" ~ "North West",
      region == "South East" ~ "South East",
      region == "South West" ~ "South West",
      region == "West Midlands" ~ "West Midlands",
      region == "Yorkshire and The Humber" ~ "Yorkshire and the Humber",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_),
    
    ## Blood pressure
    bpcat = ifelse(bp_sys < 120 &  bp_dias < 80, 1, NA),
    bpcat = ifelse(bp_sys >= 120 & bp_sys < 130 & bp_dias < 80, 2, bpcat),
    bpcat = ifelse(bp_sys >= 130 &  bp_dias >= 90, 3, bpcat),
    bpcat = ifelse(is.na(bpcat), 4, bpcat),
    
    bpcat = fct_case_when(
      bpcat == 1 ~ "Normal",
      bpcat == 2 ~ "Elevated",
      bpcat == 3 ~ "High",
      bpcat == 4 ~ "Unknown",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
    # CKD (as function of esrd and creatinine status)
    ## define variables needed for calculation
    creatinine = replace(creatinine, creatinine <20 | creatinine >3000, NA), # set implausible creatinine values to missing
    SCR_adj = creatinine/88.4 # divide by 88.4 (to convert umol/l to mg/dl)
    ) %>%
  rowwise() %>%
  mutate(
    min = case_when(sex == "Male" ~ min((SCR_adj/0.9), 1, na.rm = T)^-0.411, 
                    sex == "Female" ~ min(SCR_adj/0.7, 1, na.rm = T)^-0.329),
    max = case_when(sex == "Male" ~ max(SCR_adj/0.9, 1, na.rm = T)^-1.209, 
                    sex == "Female" ~ max(SCR_adj/0.7, 1, na.rm = T)^-1.209)) %>%
  ungroup() %>%
  mutate(
    egfr = (min*max*141)*(0.993^age),
    egfr = case_when(sex == "Female" ~ egfr*1.018, TRUE ~ egfr),
    
    ## categorise into stages
    ckd_egfr = case_when(  
      egfr < 15 ~ 5, 
      egfr >= 15 & egfr < 30 ~ 4, 
      egfr >= 30 & egfr < 45 ~ 3, 
      egfr >= 45 & egfr < 60 ~ 2, 
      egfr >= 60 ~ 0), 
    
    ## exclude those in end stage renal failure 
    chronic_kidney_disease = ifelse(end_stage_renal == 1 | !(ckd_egfr %in% c(0:5)), 0, ckd_egfr),
    chronic_kidney_disease = fct_case_when(
      chronic_kidney_disease == 0 ~ "No CKD",
      chronic_kidney_disease == 2 ~ "stage 3a",
      chronic_kidney_disease == 3 ~ "stage 3b",
      chronic_kidney_disease == 4 ~ "Stage 4",
      chronic_kidney_disease == 5 ~ "Stage 5",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_)
  ) %>%
  ungroup() %>%
  mutate(
    # Immunosuppression
    immunosuppression_diagnosis_date = !is.na(immunosuppression_diagnosis_date),
    immunosuppression_medication_date = !is.na(immunosuppression_medication_date),
    immunosuppression = immunosuppression_diagnosis_date | immunosuppression_medication_date,
    
    # Mental illness
    sev_mental_ill = !is.na(sev_mental_ill),
    
    # Time between vaccinations
    tbv = as.numeric(covid_vax_2_date - covid_vax_1_date),
    
    # Prior covid
    prior_covid_date = pmin(prior_positive_test_date, 
                           prior_primary_care_covid_case_date, 
                           prior_covidadmitted_date,
                           na.rm=TRUE), 
    
    prior_covid_cat = ifelse(prior_covid_date < covid_vax_2_date & prior_covid_date >= covid_vax_1_date, 1, NA),
    prior_covid_cat = ifelse(prior_covid_date < covid_vax_1_date, 2, prior_covid_cat),
    
    prior_covid_cat = fct_case_when(
      prior_covid_cat == 1 ~ "Between first and second dose",
      prior_covid_cat == 2 ~ "Prior to first dose",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
  ) %>%
  select(patient_id, 
         covid_positive_test_date, covid_hospital_admission_date, death_date, censor_date_1, censor_date_2,
         covid_vax_1_date, covid_vax_2_date, follow_up_time_vax1, follow_up_time_vax2, tbv,
         time_to_positive_test, time_to_hospitalisation, time_to_itu, time_to_covid_death,
         covid_positive_test, covid_positive_test_within_2_weeks_post_vax2, 
         covid_hospital_admission, covid_hospitalisation_critical_care, covid_hospitalisation_within_2_weeks_post_vax2,
         covid_death, death_with_covid_on_the_death_certificate, death_with_28_days_of_covid_positive_test, covid_death_within_2_weeks_post_vax2,
         care_home, care_home_65plus, shielded, hscworker, 
         age, ageband, ageband2, sex, bmi, smoking_status, ethnicity, imd, region,
         asthma, asplenia, bpcat, cancer, chd, chronic_neuro_dis_inc_sig_learn_dis, chronic_resp_dis, chronic_kidney_disease,
         end_stage_renal, cld, diabetes, haem_cancer, immunosuppression, learning_disability, sev_mental_ill, 
         organ_transplant, prior_covid_cat, tests_conducted_any, tests_conducted_positive) %>%
  droplevels() %>%
  mutate(
    across(
      where(is.logical),
      ~.x*1L
    )
  ) %>%
  filter(!is.na(covid_vax_1_date),
         !is.na(covid_vax_2_date),
         covid_vax_2_date > covid_vax_1_date)


## Exclusion criteria
data_processed_final <- data_processed %>%
  filter(follow_up_time_vax2 >=14,
         age >= 16,
         age < 110,
         !is.na(sex),
         covid_positive_test_within_2_weeks_post_vax2 == 0,
         covid_hospitalisation_within_2_weeks_post_vax2 == 0,
         covid_death_within_2_weeks_post_vax2 == 0) %>%
  select(-covid_positive_test_within_2_weeks_post_vax2, 
         -covid_hospitalisation_within_2_weeks_post_vax2,
         -covid_death_within_2_weeks_post_vax2,
         -covid_positive_test_date, -covid_hospital_admission_date, -death_date, -censor_date_1, censor_date_2) %>% 
  droplevels() 



# Save dataset as .rds files ----
write_rds(data_processed, here::here("output", "data", "data_all.rds"), compress = "gz")
write_rds(data_processed_final, here::here("output", "data", "data_processed.rds"), compress = "gz")









