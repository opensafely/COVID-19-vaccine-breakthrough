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
    death_date = col_date(format="%Y-%m-%d"),
    
    # Censoring
    dereg_date = col_date(format="%Y-%m-%d"),
    
    # Covariates
    care_home = col_logical(),
    shielded = col_logical(),
    age = col_integer(),
    hscworker = col_logical(),
    immunosuppression_diagnosis_date = col_date(format="%Y-%m-%d"),
    immunosuppression_medication_date = col_date(format="%Y-%m-%d"),
    first_positive_test_date = col_date(format="%Y-%m-%d"),
    latest_positive_test_date = col_date(format="%Y-%m-%d"),
    ethnicity_6 = col_character(),
    ethnicity_6_sus = col_character(),
    imd = col_character(),
    region = col_character(),

    # Other
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
    
    # COVID hospital admission
    covid_hospital_admission = ifelse(is.na(covid_hospital_admission_date), 0, 1),
    
    # Death
    death = ifelse(is.na(death_date), 0, 1),
    
    # COVID hospitalisation critical care
    covid_hospitalisation_critical_care = ifelse(covid_hospitalisation_critical_care_days > 0 , 1, 0),
    
    # Care home (65+)
    care_home_65plus = ifelse(care_home == 1 & age >=65, 1, 0),
    
    # Age
    ageband = cut(
      age,
      breaks = c(16, 70, 80, Inf),
      labels = c("16-69", "70-79", "80+"),
      right = FALSE
    ),
    
    ageband = ifelse(care_home == 1, NA, ageband),
    
    # Shielding
    shielded = ifelse(shielded == 1 & (age >=16 & age < 70), 1, 0),
    
    # Ethnicity
    ethnicity = ifelse(is.na(ethnicity_6), ethnicity_6_sus, ethnicity_6),
    ethnicity = ifelse(is.na(ethnicity), 6, ethnicity),
    
    ethnicity = fct_case_when(
      ethnicity == "1" ~ "White",
      ethnicity == "2" ~ "Mixed",
      ethnicity == "3" ~ "Asian or Asian British",
      ethnicity == "4" ~ "Black or Black British",
      ethnicity == "5" ~ "Other ethnic groups",
      ethnicity == "6" ~ "Unknown",
      #TRUE ~ "Unknown"
      TRUE ~ NA_character_),
    
    # Immunosuppression
    immunosuppression_diagnosis_date = !is.na(immunosuppression_diagnosis_date),
    immunosuppression_medication_date = !is.na(immunosuppression_medication_date),
    immunosuppression = immunosuppression_diagnosis_date | immunosuppression_medication_date,
    
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
    
    # Censoring
    censor_date = pmin(death_date, 
                       dereg_date, 
                       as.Date(Sys.Date(), format = "%Y-%m-%d"), 
                       na.rm=TRUE),
    
    # Time since second dose
    follow_up_time = tte(covid_vax_2_date,
                         as.Date(Sys.Date(), format = "%Y-%m-%d"),
                         censor_date)
    ) %>%
  select(patient_id, covid_vax_1_date, covid_vax_2_date, follow_up_time,
         covid_hospital_admission, covid_hospitalisation_critical_care, covid_death, death,
         care_home, care_home_65plus, shielded, age, ageband, hscworker, immunosuppression, 
         first_positive_test_date, latest_positive_test_date, 
         ethnicity, imd, region) %>%
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

    
    
  
  
  
  
  
  
  