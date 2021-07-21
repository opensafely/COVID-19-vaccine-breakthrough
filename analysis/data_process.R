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
    covid_hospital_admission_date = col_date(format="%Y-%m-%d"),
    covid_hospitalisation_critical_care_days = col_integer(),
    covid_death = col_logical(),
    
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
    chd = col_logical(),
    chronic_neuro_dis_inc_sig_learn_dis = col_logical(),
    chronic_resp_dis = col_logical(),
    chronic_kidney_disease_diagnostic = col_date(format="%Y-%m-%d"),
    chronic_kidney_disease_all_stages = col_date(format="%Y-%m-%d"),
    chronic_kidney_disease_all_stages_3_5 = col_date(format="%Y-%m-%d"),
    end_stage_renal = col_logical(), 
    cld = col_logical(),
    diabetes = col_logical(),
    immunosuppression_diagnosis_date = col_date(format="%Y-%m-%d"),
    immunosuppression_medication_date = col_date(format="%Y-%m-%d"),
    learning_disability = col_logical(),
    sev_mental_ill = col_date(format="%Y-%m-%d"),
    organ_transplant = col_logical(),
    
    # Other
    first_positive_test_date = col_date(format="%Y-%m-%d"),
    latest_positive_test_date = col_date(format="%Y-%m-%d"),
    covid_vax_1_date = col_date(format="%Y-%m-%d"),
    covid_vax_2_date = col_date(format="%Y-%m-%d")
    
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

## Format columns (i.e, set factor levels)
data_processed <- data_extract %>%
  mutate(
    
    # Positive test
    covid_positive_post_2vacc = ifelse(latest_positive_test_date > (covid_vax_2_date + 14), 1, 0),
    
    # COVID hospital admission
    covid_hospital_admission = ifelse(is.na(covid_hospital_admission_date), 0, 1),
    
    # End date
    end_date = as.Date("2021-03-17", format = "%Y-%m-%d"),
    
    # Censoring
    censor_date = pmin(death_date, 
                       dereg_date, 
                       as.Date(Sys.Date(), format = "%Y-%m-%d"), 
                       na.rm=TRUE),
    
    # Time since second dose
    follow_up_time_vax2 = tte(covid_vax_2_date,
                              as.Date(Sys.Date(), format = "%Y-%m-%d"),
                              censor_date),
    
    # Time since first dose
    follow_up_time_vax1 = tte(covid_vax_1_date,
                              as.Date(Sys.Date(), format = "%Y-%m-%d"),
                              censor_date),
    
    # Time to positive test
    time_to_positive_test = tte(covid_vax_2_date + 14,
                                latest_positive_test_date,
                                censor_date),
    
    # Time to hospitalisation
    time_to_hospitalisation = tte(covid_vax_2_date,
                                  covid_hospital_admission_date,
                                  censor_date),
    
    # Time to hospitalisation critical care
    time_to_itu = tte(covid_vax_2_date,
                      covid_hospital_admission_date,
                      censor_date),
    
    # Time to covid death
    time_to_covid_death = tte(covid_vax_2_date,
                              covid_death_date,
                              censor_date),
    
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
    bpcat = ifelse(bp_sys >= 130 & bp_sys < 140 & bp_dias >= 80 & bp_dias < 90, 3, bpcat),
    bpcat = ifelse(bp_sys >= 140 &  bp_dias >= 90, 4, bpcat),
    bpcat = ifelse(is.na(bpcat), 5, bpcat),
    
    bpcat = fct_case_when(
      bpcat == 1 ~ "Normal",
      bpcat == 2 ~ "Elevated",
      bpcat == 3 ~ "High, stage I",
      bpcat == 4 ~ "High, stage II",
      bpcat == 5 ~ "Unknown",
      #TRUE ~ "Unknown",
      TRUE ~ NA_character_
    ),
    
    # CKD
    chronic_kidney_disease = case_when(
      !is.na(chronic_kidney_disease_diagnostic) ~ TRUE,
      is.na(chronic_kidney_disease_all_stages) ~ FALSE,
      !is.na(chronic_kidney_disease_all_stages_3_5) & (chronic_kidney_disease_all_stages_3_5 >= chronic_kidney_disease_all_stages) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Immunosuppression
    immunosuppression_diagnosis_date = !is.na(immunosuppression_diagnosis_date),
    immunosuppression_medication_date = !is.na(immunosuppression_medication_date),
    immunosuppression = immunosuppression_diagnosis_date | immunosuppression_medication_date,
    
    # Mental illness
    sev_mental_ill = !is.na(sev_mental_ill),
    
    # Time between vaccinations
    tbv = as.numeric(covid_vax_2_date - covid_vax_1_date)
    
  ) %>%
  select(patient_id, covid_vax_1_date, covid_vax_2_date, follow_up_time_vax1, follow_up_time_vax2, tbv,
         time_to_positive_test, time_to_hospitalisation, time_to_itu, time_to_covid_death,
         covid_hospital_admission, covid_hospitalisation_critical_care, covid_death, covid_positive_post_2vacc,
         care_home, care_home_65plus, shielded, hscworker, 
         age, ageband, ageband2, sex, bmi, smoking_status, ethnicity, imd, region,
         asthma, asplenia, bpcat, chd, chronic_neuro_dis_inc_sig_learn_dis, chronic_resp_dis, chronic_kidney_disease,
         end_stage_renal, cld, diabetes, immunosuppression, learning_disability, sev_mental_ill, organ_transplant,
         first_positive_test_date, latest_positive_test_date) %>%
  droplevels() %>%
  mutate(
    across(
      where(is.logical),
      ~.x*1L
    )
  ) %>%
  filter(!is.na(covid_vax_1_date),
         !is.na(covid_vax_2_date),
         age >= 16)

# Save dataset as .rds files ----
write_rds(data_processed, here::here("output", "data", "data_all.rds"), compress="gz")









