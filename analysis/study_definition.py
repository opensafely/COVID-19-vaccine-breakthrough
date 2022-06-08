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
from datetime import datetime, date

campaign_start = "2020-12-07"
end_date = "2022-03-31"

## Define study population and variables
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": "1970-01-01", "latest": "today"},
    "rate": "uniform",
    "incidence": 0.2,
  },
  
  # Set index date to vaccination campaign start date
  index_date = campaign_start,
  
  # Define the study population
  
  # POPULATION ----
  population = patients.satisfying(
    
    """
    registered_at_start
    """,
    
    registered_at_start = patients.registered_as_of("index_date"),
    
  ),
  
  
  
  # COVID VACCINATION ----
  
  ## Date of first vaccination
  covid_vax_1_date = patients.with_tpp_vaccination_record(
    target_disease_matches = "SARS-2 CORONAVIRUS",
    between = ["2020-12-08", end_date],
    find_first_match_in_period = True,
    returning = "date",
    date_format = "YYYY-MM-DD",
    return_expectations = {
      "date": {"earliest": "2020-12-08", "latest": end_date},
      "incidence": 0.1,
    },
  ),
  
  ## Date of second vaccination
  covid_vax_2_date = patients.with_tpp_vaccination_record(
    target_disease_matches = "SARS-2 CORONAVIRUS",
    between = ["covid_vax_1_date + 19 days", end_date],
    find_first_match_in_period = True,
    returning = "date",
    date_format = "YYYY-MM-DD",
    return_expectations = {
      "date": {"earliest": "2020-12-31", "latest": end_date},
      "incidence": 0.8,
    },
  ),
  
  ## Date of third/booster vaccination
  covid_vax_3_date = patients.with_tpp_vaccination_record(
    target_disease_matches = "SARS-2 CORONAVIRUS",
    between = ["covid_vax_2_date + 56 days", end_date],
    find_first_match_in_period = True,
    returning = "date",
    date_format = "YYYY-MM-DD",
    return_expectations = {
      "date": {"earliest": "2021-09-24", "latest": end_date},
      "incidence": 0.8,
    },
  ),
  
  ## Vaccination declined
  covid_vax_declined = patients.with_these_clinical_events(
    covid_vaccine_declined_codes,
    returning = "binary_flag",
    on_or_before = end_date,
    return_expectations = {
      "incidence": 0.01,
    },
  ),
  
  ## Vaccination status
  vaccination_status = patients.categorised_as(
    {
      "Un-vaccinated": "DEFAULT",
      "Un-vaccinated (declined)": """ covid_vax_declined AND NOT (covid_vax_1_date OR covid_vax_2_date OR covid_vax_3_date)""",
      "One vaccination": """ covid_vax_1_date AND NOT covid_vax_2_date """,
      "Two vaccinations": """ covid_vax_2_date AND NOT covid_vax_3_date """,
      "Three or more vaccinations": """ covid_vax_3_date """
    },
    
    return_expectations = {
      "rate": "universal",
      "category": {
        "ratios": {
          "Un-vaccinated": 0.1,
          "Un-vaccinated (declined)": 0.1,
          "One vaccination": 0.1,
          "Two vaccinations": 0.2,
          "Three or more vaccinations": 0.5,
        }
      },
    },
  ),
  
  
  
  # OUTCOMES ----
  
  ## Latest vaccination date
  latest_vaccination_date = patients.maximum_of(
    "covid_vax_1_date",
    "covid_vax_2_date",
    "covid_vax_3_date",
  ),
  
  ## SARS-CoV-2 infection
  earliest_covid_positive_test_date = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    returning = "date",
    date_format = "YYYY-MM-DD",
    on_or_before = end_date,
    find_first_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.6
    },
  ),
  
  breakthrough_covid_positive_test_date = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    returning = "date",
    date_format = "YYYY-MM-DD",
    between = ["latest_vaccination_date + 14 days", end_date],
    find_first_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.6
    },
  ),
  
  covid_positive_test_within_2_weeks_post_vax = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    returning = "binary_flag",
    date_format = "YYYY-MM-DD",
    between = ["latest_vaccination_date", "latest_vaccination_date + 14 days"],
    find_first_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.1
    },
  ),
  
  ## COVID-19-related hospitalisation 
  earliest_covid_hospital_admission_date = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_primary_diagnoses = covid_codes,
    with_patient_classification = ["1"], # ordinary admissions only - exclude day cases and regular attenders
    # see https://docs.opensafely.org/study-def-variables/#sus for more info
    with_admission_method = ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"], # emergency admissions only to exclude incidental COVID
    on_or_before = end_date,
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2020-03-01", "latest" : end_date},
      "rate": "uniform",
      "incidence": 0.2,
    },
  ),
  
  breakthrough_covid_hospital_admission_date = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_primary_diagnoses = covid_codes,
    with_patient_classification = ["1"], # ordinary admissions only - exclude day cases and regular attenders
    # see https://docs.opensafely.org/study-def-variables/#sus for more info
    with_admission_method = ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"], # emergency admissions only to exclude incidental COVID
    between = ["latest_vaccination_date", end_date],
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2020-03-01", "latest" : end_date},
      "rate": "uniform",
      "incidence": 0.05,
    },
  ),
  
  covid_hospitalisation_within_2_weeks_post_vax = patients.admitted_to_hospital(
    returning = "binary_flag",
    with_these_primary_diagnoses = covid_codes,
    with_patient_classification = ["1"], # ordinary admissions only - exclude day cases and regular attenders
    # see https://docs.opensafely.org/study-def-variables/#sus for more info
    with_admission_method = ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"], # emergency admissions only to exclude incidental COVID
    between = ["latest_vaccination_date", "latest_vaccination_date + 14 days"],
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {"incidence": 0.05},
  ),
  
  ## Critical care days for breakthrough COVID-19-related hospitalisation 
  breakthrough_covid_hospitalisation_critical_care = patients.admitted_to_hospital(
    returning = "days_in_critical_care",
    with_these_primary_diagnoses = covid_codes,
    with_patient_classification = ["1"], # ordinary admissions only - exclude day cases and regular attenders
    # see https://docs.opensafely.org/study-def-variables/#sus for more info
    with_admission_method = ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"], # emergency admissions only to exclude incidental COVID
    between = ["latest_vaccination_date", end_date],
    find_first_match_in_period = True,
    return_expectations = {
      "category": {"ratios": {"20": 0.5, "40": 0.5}},
      "incidence": 0.2,
    },
  ),
  
  ## COVID-19 related death
  death_with_covid_on_the_death_certificate = patients.with_these_codes_on_death_certificate(
    covid_codes,
    returning = "binary_flag",
    date_format = "YYYY-MM-DD",
    between = [campaign_start, end_date],
    return_expectations = {
      "date": {"earliest": "2021-01-01", "latest" : end_date},
      "rate": "uniform",
      "incidence": 0.3},
  ),
  
  death_with_28_days_of_covid_positive_test = patients.satisfying(
    
    """
    death_date_post_start
    AND 
    positive_covid_test_prior_28_days
    """, 
    
    return_expectations = {
      "incidence": 0.2,
    },
    
    death_date_post_start = patients.died_from_any_cause(
      returning = "date_of_death",
      date_format = "YYYY-MM-DD",
      between = [campaign_start, end_date],
      return_expectations = {
        "date": {"earliest": "2020-02-01", "latest" : end_date},
        "rate": "uniform",
        "incidence": 0.02
      },
    ),
    
    positive_covid_test_prior_28_days = patients.with_test_result_in_sgss(
      pathogen = "SARS-CoV-2",
      test_result = "positive",
      returning = "binary_flag",
      date_format = "YYYY-MM-DD",
      between = ["death_date_post_start - 28 days", "death_date_post_start"],
      find_first_match_in_period = True,
      restrict_to_earliest_specimen_date = False,
      return_expectations = {
        "date": {"earliest": "2020-02-01"},
        "rate": "exponential_increase",
        "incidence": 0.1
      },
    ),
    
  ),
  

  
  # CENSORING ----
  
  ## Death date
  death_date = patients.died_from_any_cause(
    returning = "date_of_death",
    date_format = "YYYY-MM-DD",
    between = [campaign_start, end_date],
    return_expectations = {
      "date": {"earliest": "2021-05-01", "latest" : end_date},
      "rate": "uniform",
      "incidence": 0.02
    },
  ),
  
  ## De-registration
  dereg_date = patients.date_deregistered_from_all_supported_practices(
    between = [campaign_start, end_date],
    date_format = "YYYY-MM-DD",
    return_expectations ={
      "date": {"earliest": "2021-01-01", "latest": end_date},
      "incidence": 0.001
    }
  ),
  
  
  
  # PRIORITY GROUPS ----
  
  baseline_date = patients.maximum_of(
    campaign_start,
    "covid_vax_1_date",
    "covid_vax_2_date",
    "covid_vax_3_date",
  ),
  
  ## Care home
  care_home =  patients.with_these_clinical_events(
    carehome_primis_codes,
    on_or_before = "baseline_date",
    returning = "binary_flag",
  ),
  
  ## PRIMIS overall flag for shielded group
  shielded = patients.satisfying(
    """
      severely_clinically_vulnerable
      AND 
      NOT less_vulnerable
      """, 
    return_expectations = {
      "incidence": 0.3,
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
  
  ## HCW
  hscworker = patients.with_healthcare_worker_flag_on_covid_vaccine_record(returning = "binary_flag"),
  
  
  
  # CLINICAL/DEMOGRAPHIC COVARIATES ----
  
  ## Age
  age = patients.age_as_of(
    "2021-08-31",
    return_expectations = {
      "rate": "universal",
      "int": {"distribution": "population_ages"},
      "incidence" : 0.9
    },
  ),
  
  ## Sex
  sex = patients.sex(
    return_expectations = {
      "rate": "universal",
      "category": {"ratios": {"M": 0.49, "F": 0.51}},
    }
  ),
  
  ## BMI
  bmi = patients.categorised_as(
    {
      "Not obese": "DEFAULT",
      "Obese I (30-34.9)": """ bmi_value >= 30 AND bmi_value < 35""",
      "Obese II (35-39.9)": """ bmi_value >= 35 AND bmi_value < 40""",
      "Obese III (40+)": """ bmi_value >= 40 AND bmi_value < 100""",
      # set maximum to avoid any impossibly extreme values being classified as obese
    },
    bmi_value = patients.most_recent_bmi(
      on_or_after = "baseline_date - 5 years",
      minimum_age_at_measurement = 16
    ),
    return_expectations = {
      "rate": "universal",
      "category": {
        "ratios": {
          "Not obese": 0.7,
          "Obese I (30-34.9)": 0.1,
          "Obese II (35-39.9)": 0.1,
          "Obese III (40+)": 0.1,
        }
      },
    },
  ),
  
  ## Smoking
  smoking_status = patients.categorised_as(
    {
      "S": "most_recent_smoking_code = 'S'",
      "E": """
                 most_recent_smoking_code = 'E' OR (
                   most_recent_smoking_code = 'N' AND ever_smoked
                 )
            """,
      "N": "most_recent_smoking_code = 'N' AND NOT ever_smoked",
      "M": "DEFAULT",
    },
    
    return_expectations = {
      "category": {"ratios": {"S": 0.6, "E": 0.1, "N": 0.2, "M": 0.1}}
    },
    
    most_recent_smoking_code = patients.with_these_clinical_events(
      clear_smoking_codes,
      find_last_match_in_period = True,
      on_or_before = "baseline_date",
      returning="category",
    ),
    
    ever_smoked=patients.with_these_clinical_events(
      filter_codes_by_category(clear_smoking_codes, include=["S", "E"]),
      on_or_before = "baseline_date",
    ),
  ),
  
  ## Ethnicity
  ethnicity_6 = patients.with_these_clinical_events(
    ethnicity_6_codes,
    returning = "category",
    find_last_match_in_period = True,
    include_date_of_match = False,
    return_expectations = {
      "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
      "incidence": 0.75,
    },
  ),
  
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
      "baseline_date",
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
    "baseline_date",
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
  
  
  
  # COMORBIDITIES ----
  
  ## Asthma
  asthma = patients.with_these_clinical_events(
    asthma_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_last_match_in_period = True,
    on_or_before = "baseline_date",
  ),
  
  ## Asplenia or Dysfunction of the Spleen codes
  asplenia = patients.with_these_clinical_events(
    spln_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_last_match_in_period = True,
    on_or_before = "baseline_date",
  ),
  
  ## Blood pressure
  bp_sys = patients.mean_recorded_value(
    systolic_blood_pressure_codes,
    on_most_recent_day_of_measurement = True,
    on_or_before = "baseline_date",
    include_measurement_date = True,
    include_month = True,
    return_expectations = {
      "incidence": 0.6,
      "float": {"distribution": "normal", "mean": 80, "stddev": 10},
    },
  ),
  
  bp_dias = patients.mean_recorded_value(
    diastolic_blood_pressure_codes,
    on_most_recent_day_of_measurement = True,
    on_or_before="baseline_date",
    include_measurement_date = True,
    include_month = True,
    return_expectations ={
      "incidence": 0.6,
      "float": {"distribution": "normal", "mean": 120, "stddev": 10},
    },
  ),
  
  ## Cancer (non-haematological)
  cancer = patients.with_these_clinical_events(
    combine_codelists(
      lung_cancer_codes,
      other_cancer_codes
    ),
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_last_match_in_period = True,
    on_or_before = "baseline_date",
  ),
  
  ## Cancer (haematological)
  haem_cancer = patients.with_these_clinical_events(
    haem_cancer_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_last_match_in_period = True,
    on_or_before = "baseline_date",
  ),
  
  ### Creatinine (used to calculate CKD) 
  creatinine = patients.with_these_clinical_events(
    creatinine_codes,
    find_last_match_in_period = True,
    between = ["baseline_date - 1 year", "baseline_date"],
    returning = "numeric_value",
    return_expectations = {
      "float": {"distribution": "normal", "mean": 60.0, "stddev": 15},
      "incidence": 0.95,
    },
  ),
  
  creatinine_date = patients.with_these_clinical_events(
    creatinine_codes,
    find_last_match_in_period = True,
    between = ["baseline_date - 1 year", "baseline_date"],
    returning = "date",
    date_format = "YYYY-MM-DD",
  ),
  
  ## Diabetes
  diabetes = patients.with_these_clinical_events(
    diab_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_last_match_in_period = True,
    on_or_before = "baseline_date",
  ),
  
  ## Dialysis
  dialysis = patients.with_these_clinical_events(
    dialysis_codes,
    find_last_match_in_period = True,
    returning = "date",
    date_format = "YYYY-MM-DD",
    on_or_before = "baseline_date",
  ),
  
  ## Heart disease codes
  chd = patients.with_these_clinical_events(
    chd_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_last_match_in_period = True,
    on_or_before = "baseline_date",
  ),
  
  ## Immunosuppression diagnosis
  immunosuppression_diagnosis_date = patients.with_these_clinical_events(
    immunosuppression_diagnosis_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "baseline_date",
    date_format = "YYYY-MM-DD",
  ),
  
  ## Immunosuppression medication
  immunosuppression_medication_date = patients.with_these_medications(
    immunosuppression_medication_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "baseline_date",
    date_format = "YYYY-MM-DD",
  ),
  
  ## Learning disabilities
  learning_disability = patients.with_these_clinical_events(
    learning_disability_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_last_match_in_period = True,
    on_or_before = "baseline_date"
  ),
  
  ## Liver disease codes
  cld = patients.with_these_clinical_events(
    cld_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "baseline_date",
    date_format = "YYYY-MM-DD",
  ),
  
  ## Neurological disease (including Significant Learning Disorder)
  chronic_neuro_dis_inc_sig_learn_dis = patients.with_these_clinical_events(
    cnd_inc_sig_learn_dis_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_last_match_in_period = True,
    on_or_before = "baseline_date",
  ),
  
  ## Respiratory disease
  chronic_resp_dis = patients.with_these_clinical_events(
    crs_codes,
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_last_match_in_period = True,
    on_or_before = "baseline_date",
  ),
  
  ## Severe mental illness
  sev_mental_ill = patients.with_these_clinical_events(
    sev_mental_ill_codes,
    returning = "date",
    find_last_match_in_period = True,
    on_or_before = "baseline_date",
    date_format = "YYYY-MM-DD",
  ),
  
  ## Kidney transplant
  kidney_transplant = patients.with_these_clinical_events(
    kidney_transplant_codes, 
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_last_match_in_period = True,
    on_or_before = "baseline_date"
  ),
  
  ## Other organ transplant
  other_organ_transplant = patients.with_these_clinical_events(
    other_organ_transplant_codes, 
    returning = "date",
    date_format = "YYYY-MM-DD",
    find_last_match_in_period = True,
    on_or_before = "baseline_date"
  ),
  
  ## Last positive test date prior to vaccination
  prior_positive_test_date = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    returning = "date",
    date_format = "YYYY-MM-DD",
    on_or_before = "latest_vaccination_date + 14 days",
    find_first_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01
    },
  ),
  
  ## Count of positive tests prior to vaccination
  prior_positive_test_count = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    returning = "number_of_matches_in_period",
    on_or_after = "latest_vaccination_date + 14 days",
    find_first_match_in_period = True,
    restrict_to_earliest_specimen_date = False,
    return_expectations={
      "int": {"distribution": "normal", "mean": 2, "stddev": 1},
      "incidence": 0.05,
    },
  ),
  
  ## Positive case identification prior to vaccination
  prior_primary_care_covid_case_date = patients.with_these_clinical_events(
    combine_codelists(
      covid_primary_care_code,
      covid_primary_care_positive_test,
      covid_primary_care_sequalae,
    ),
    returning = "date",
    date_format = "YYYY-MM-DD",
    on_or_before = "latest_vaccination_date + 14 days",
    find_first_match_in_period=True,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01
    },
  ),
  
  ## Positive covid admission prior to vaccination
  prior_covidadmitted_date = patients.admitted_to_hospital(
    returning = "date_admitted",
    with_these_diagnoses = covid_icd10,
    on_or_before = "latest_vaccination_date + 14 days",
    date_format = "YYYY-MM-DD",
    find_first_match_in_period = True,
    return_expectations = {
      "date": {"earliest": "2020-02-01"},
      "rate": "exponential_increase",
      "incidence": 0.01,
    },
  ),
  
  ## Count of tests (any)
  tests_conducted_any = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "any",
    returning = "number_of_matches_in_period",
    between = ["baseline_date", end_date],
    restrict_to_earliest_specimen_date = False,
    return_expectations={
      "int": {"distribution": "normal", "mean": 4, "stddev": 1},
      "incidence": 0.05,
    },
  ),
  
  ## Count of tests (positive)
  tests_conducted_positive = patients.with_test_result_in_sgss(
    pathogen = "SARS-CoV-2",
    test_result = "positive",
    returning = "number_of_matches_in_period",
    between = ["baseline_date + 14 days", end_date],
    restrict_to_earliest_specimen_date = False,
    return_expectations={
      "int": {"distribution": "normal", "mean": 2, "stddev": 0.1},
      "incidence": 0.01,
    },
  ),
  
  
)




