######################################

# This script provides the formal specification of the study data that will be extracted from 
# the OpenSAFELY database.

######################################


# IMPORT STATEMENTS ----

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
  
  
# DEFINE STUDY POPULATION ----

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
        AND
        NOT COVID_positive_unvacc
        
        """,
    
    registered = patients.registered_as_of("covid_vax_2_date + 14 days"),
    
    COVID_positive_unvacc = patients.with_test_result_in_sgss(
      pathogen = "SARS-CoV-2",
      test_result = "positive",
      returning = "binary_flag",
      between = ["covid_vax_2_date", "covid_vax_2_date + 13 days"],
      return_expectations = {"incidence": 0.01},
    ),
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
  covid_hospital_admission_date = patients.admitted_to_hospital(
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
  covid_death = patients.satisfying(
    
    """
    covid_death_after_vacc_date
    AND 
    NOT covid_hospitalisation_pre_vacc
    """, 
    
    return_expectations = {
      "incidence": 0.01,
    },
    
    covid_death_after_vacc_date = patients.with_these_codes_on_death_certificate(
      covid_codes,
      returning = "date_of_death",
      date_format = "YYYY-MM-DD",
      on_or_after = "covid_vax_2_date + 14 days",
      return_expectations = {
        "date": {"earliest": "2021-01-01", "latest" : end_date},
        "rate": "uniform",
        "incidence": 0.02
      },
    ),
    
    covid_hospitalisation_pre_vacc = patients.admitted_to_hospital(
      returning = "binary_flag",
      with_these_diagnoses = covid_codes,
      between = ["covid_vax_2_date", "covid_vax_2_date + 13 days"],
      date_format = "YYYY-MM-DD",
      find_first_match_in_period = True,
      return_expectations = {"incidence": 0.05},
    ),
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
    on_or_after = "covid_vax_2_date",
    date_format = "YYYY-MM-DD",
  ),
  
  
  # COVARIATES ----
  
  ## Care home
  care_home =  patients.with_these_clinical_events(
    carehome_primis_codes,
    on_or_before = "covid_vax_2_date",
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
  
  ## Age
  age = patients.age_as_of(
    "2021-03-31",
    return_expectations = {
      "rate": "universal",
      "int": {"distribution": "population_ages"},
      "incidence" : 0.001
    },
  ),
  
  ## HCW
  hscworker = patients.with_healthcare_worker_flag_on_covid_vaccine_record(returning = "binary_flag"),
  
  ## Immunosuppression diagnosis
  immunosuppression_diagnosis_date = patients.with_these_clinical_events(
    immunosuppression_diagnosis_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "covid_vax_2_date",
    date_format = "YYYY-MM-DD",
  ),
  
  ## Immunosuppression medication
  immunosuppression_medication_date = patients.with_these_medications(
    immunosuppression_medication_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "covid_vax_2_date",
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
  ethnicity_6 = patients.with_these_clinical_events(
    ethnicity_6_codes,
    returning="category",
    find_last_match_in_period = True,
    include_date_of_match = False,
    return_expectations = {
      "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
      "incidence": 0.75,
    },
  ),
  
  ## New ethnicity variable that takes data from SUS
  ethnicity_6_sus = patients.with_ethnicity_from_sus(
    returning = "group_6",  
    use_most_frequent_code = True,
    return_expectations = {
      "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
      "incidence": 0.8,
    },
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
      "covid_vax_2_date",
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
    "covid_vax_2_date",
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




