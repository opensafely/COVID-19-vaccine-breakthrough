######################################

# This script provides the formal specification of the study data that will be extracted from 
# the OpenSAFELY database.

######################################


# --- IMPORT STATEMENTS ---

## Import code building blocks from cohort extractor package
from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
  Measure
)

## Import codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *
  
  
# --- DEFINE STUDY POPULATION ---
  
## Define study time variables
from datetime import datetime

start_date = "2019-01-01"
end_date = datetime.today().strftime('%Y-%m-%d')

## Define study population and variables
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": "1970-01-01", "latest": end_date},
    "rate": "uniform",
    "incidence": 0.2,
  },
  
  # Set index date to start date
  index_date = start_date,
  
  # Define the study population
  
  ## POPULATION ----
  population = patients.satisfying(
    """
        age >= 16
        AND
        covid_vax_1_date
        AND
        covid_vax_2_date
        AND
        registered
        """,
    
    registered = patients.registered_as_of("covid_vax_2_date + 14 days"),
  ),
  
  covid_vax_1_date = patients.with_vaccination_record(
    returning = "date",
    tpp = {"target_disease_matches": "SARS-2 CORONAVIRUS",},
    emis = {"procedure_codes": covid_vaccine_EMIS_codes,},
    find_first_match_in_period = True,
    on_or_after = "2020-12-08",
    date_format = "YYYY-MM-DD",
    return_expectations = {
      "date": {
        "earliest": "2020-12-08",
        "latest": end_date,
      }
    },
  ),
  
  covid_vax_2_date = patients.with_vaccination_record(
    returning = "date",
    tpp = {"target_disease_matches": "SARS-2 CORONAVIRUS",},
    emis = {"procedure_codes": covid_vaccine_EMIS_codes,},
    find_first_match_in_period = True,
    on_or_after = "covid_vax_1_date + 15 days",
    date_format = "YYYY-MM-DD",
    return_expectations = {
      "date": {
        "earliest": "2021-12-31",
        "latest": end_date,
      }
    },
  ),
  
  
  # OUTCOMES ----
  
  ## COVID-related hospitalisation 
  covid_primary_diagnosis_hospitalisation_date = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = covid_codes,
    with_admission_method = ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    on_or_after = "covid_vax_2_date + 14 days",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2021-05-01", "latest" : end_date},
      "rate": "uniform",
      "incidence": 0.05,
    },
  ),
  
  covid_any_diagnosis_hospitalisation_date = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = covid_codes,
    on_or_after = "covid_vax_2_date + 14 days",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2021-05-01", "latest" : end_date},
      "rate": "uniform",
      "incidence": 0.05,
    },
  ),
  
  ## Critical care days for COVID-related hospitalisation 
  covid_hospitalisation_critical_care_days = patients.admitted_to_hospital(
    returning = "days_in_critical_care",
    with_these_diagnoses = covid_codes,
    on_or_after = "covid_vax_2_date + 14 days",
    find_first_match_in_period = True,
    return_expectations = {
      "category": {"ratios": {"20": 0.5, "40": 0.5}},
      "incidence": 1,
    },
  ),
  
  ## COVID related death
  covid_death_date = patients.with_these_codes_on_death_certificate(
    covid_codes,
    returning = "date_of_death",
    date_format = "YYYY-MM-DD",
    on_or_after = "covid_vax_2_date + 14 days",
    return_expectations = {
      "date": {"earliest": "2021-05-01", "latest" : end_date},
      "rate": "uniform",
      "incidence": 0.02
    },
  ),
  
  ## Death of any cause
  death_date = patients.died_from_any_cause(
    returning = "date_of_death",
    date_format = "YYYY-MM-DD",
    on_or_after = "covid_vax_2_date + 14 days",
    return_expectations = {
      "date": {"earliest": "2021-05-01", "latest" : end_date},
      "rate": "uniform",
      "incidence": 0.02
    },
  ),
  
  
  # CENSORING ----
  
  ## De-registration
  dereg_date = patients.date_deregistered_from_all_supported_practices(
    on_or_after = "index_date + 1 day",
    date_format = "YYYY-MM-DD",
  ),
  
  
  # COVARIATES ----
  
  ## Care home
  care_home =  patients.with_these_clinical_events(
    carehome_primis_codes,
    on_or_before = "index_date",
    returning="binary_flag",
  ),
  
  ## PRIMIS overall flag for shielded group
  shielded = patients.satisfying(
      """
      severely_clinically_vulnerable
      AND 
      NOT less_vulnerable
      """, 
    return_expectations = {
      "incidence": 0.01,
    },
    
    ### SHIELDED GROUP - first flag all patients with "high risk" codes
    severely_clinically_vulnerable = patients.with_these_clinical_events(
      high_risk_codes, # note no date limits set
      find_last_match_in_period = True,
      return_expectations = {"incidence": 0.02,},
    ),
    
    # find date at which the high risk code was added
    date_severely_clinically_vulnerable = patients.date_of(
      "severely_clinically_vulnerable", 
      date_format = "YYYY-MM-DD",   
    ),
    
    ### NOT SHIELDED GROUP (medium and low risk) - only flag if later than 'shielded'
    less_vulnerable = patients.with_these_clinical_events(
      not_high_risk_codes, 
      on_or_after = "date_severely_clinically_vulnerable",
      return_expectations = {"incidence": 0.01,},
    ),
  ),
  
  age = patients.age_as_of(
    "2020-03-31",
    return_expectations = {
      "rate": "universal",
      "int": {"distribution": "population_ages"},
      "incidence" : 0.001
    },
  ),
  
  ## HCW (when available)
  
  ## Immunosuppression diagnosis
  immunosuppression_diagnosis = patients.with_these_clinical_events(
    immunosuppression_diagnosis_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "index_date",
    date_format = "YYYY-MM-DD",
  ),
  
  ## Immunosuppression medication
  immunosuppression_medication = patients.with_these_medications(
    immunosuppression_medication_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "index_date",
    on_or_after = "2020-07-01",
    date_format = "YYYY-MM-DD",
  ),
  
  ## First COVID positive test
  first_positive_test_date = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    returning = "date",
    date_format = "YYYY-MM-DD",
    on_or_before = end_date,
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01
    },
  ),
  
  ## Latest COVID positive test
  latest_positive_test_date=patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    returning = "date",
    date_format = "YYYY-MM-DD",
    on_or_before = end_date,
    find_last_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01
    },
  ),
  
  ## Ethnicity
  ethnicity = patients.with_these_clinical_events(
    ethnicity_codes,
    returning = "category",
    find_last_match_in_period = True,
    on_or_before = "index_date",
    return_expectations = {
      "category": {
        "ratios": {
          "1": 0.25,
          "2": 0.05,
          "3": 0.05,
          "4": 0.05,
          "5": 0.05,
          "6": 0.05,
          "7": 0.05,
          "8": 0.05,
          "9": 0.05,
          "10": 0.05,
          "11": 0.05,
          "12": 0.05,
          "13": 0.05,
          "14": 0.05,
          "15": 0.05,
          "16": 0.05,
        }
      },
      "incidence": 0.75,
    },
  ),
  
  ## Any other ethnicity code
  ethnicity_other = patients.with_these_clinical_events(
    ethnicity_other_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "index_date",
    date_format = "YYYY-MM-DD",
  ),
  
  ## Ethnicity not given - patient refused
  ethnicity_not_given = patients.with_these_clinical_events(
    ethnicity_not_given_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "index_date",
    date_format = "YYYY-MM-DD",
    return_expectations = {"incidence": 0.00000001},
  ),
  
  ## Ethnicity not stated
  ethnicity_not_stated = patients.with_these_clinical_events(
    ethnicity_not_stated_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "index_date",
    date_format = "YYYY-MM-DD",
  ),
  
  ## Ethnicity no record
  ethnicity_no_record = patients.with_these_clinical_events(
    ethnicity_no_record_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "index_date",
    date_format = "YYYY-MM-DD",
  ),  
  
  ## Index of multiple deprivation
  imd = patients.categorised_as(
    {"0": "DEFAULT",
      "1": """index_of_multiple_deprivation >=1 AND index_of_multiple_deprivation < 32844*1/5""",
      "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
      "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
      "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
      "5": """index_of_multiple_deprivation >= 32844*4/5 """,
    },
    index_of_multiple_deprivation = patients.address_as_of(
      "index_date",
      returning = "index_of_multiple_deprivation",
      round_to_nearest = 100,
    ),
    return_expectations = {
      "rate": "universal",
      "category": {
        "ratios": {
          "0": 0.01,
          "1": 0.20,
          "2": 0.20,
          "3": 0.20,
          "4": 0.20,
          "5": 0.19,
        }},
    },
  ),
  
  ## Region - NHS England 9 regions
  region = patients.registered_practice_as_of(
    "index_date",
    returning = "nuts1_region_name",
    return_expectations = {
      "rate": "universal",
      "category": {
        "ratios": {
          "North East": 0.1,
          "North West": 0.1,
          "Yorkshire and The Humber": 0.1,
          "East Midlands": 0.1,
          "West Midlands": 0.1,
          "East": 0.1,
          "London": 0.2,
          "South West": 0.1,
          "South East": 0.1,},},
    },
  ),
  
)



