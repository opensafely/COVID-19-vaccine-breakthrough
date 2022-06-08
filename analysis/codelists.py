######################################

# Some covariates used in the study are created from codelists of clinical conditions or 
# numerical values available on a patient's records.
# This script fetches all of the codelists identified in codelists.txt from OpenCodelists.

######################################


# --- IMPORT STATEMENTS ---

## Import code building blocks from cohort extractor package
from cohortextractor import (codelist, codelist_from_csv, combine_codelists)


# --- CODELISTS ---

## Vaccination declined
first_dose_declined = codelist_from_csv(
  "codelists/opensafely-covid-19-vaccination-first-dose-declined.csv",
  system = "snomed",
  column = "code",
)

second_dose_declined = codelist_from_csv(
  "codelists/opensafely-covid-19-vaccination-second-dose-declined.csv",
  system = "snomed",
  column = "code",
)

covid_vaccine_declined_codes = combine_codelists(
  first_dose_declined, second_dose_declined
)

## History of covid
covid_codes = codelist_from_csv(
  "codelists/opensafely-covid-identification.csv",
  system = "icd10",
  column = "icd10_code",
)

## Patients in long-stay nursing and residential care
carehome_primis_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-longres.csv",
  system = "snomed",
  column = "code",
)

## Shielding
high_risk_codes = codelist(
  ['1300561000000107'], system="snomed")

## Lower Risk from COVID-19 codes
not_high_risk_codes = codelist(
  ['1300591000000101', '1300571000000100'], system="snomed")

## Ethnicity
ethnicity_6_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-eth2001.csv",
  system = "snomed",
  column = "code",
  category_column="grouping_6_id",
)

## Smoking
clear_smoking_codes = codelist_from_csv(
  "codelists/opensafely-smoking-clear.csv",
  system="ctv3",
  column="CTV3Code",
  category_column="Category",
)
unclear_smoking_codes = codelist_from_csv(
  "codelists/opensafely-smoking-unclear.csv",
  system="ctv3",
  column="CTV3Code",
  category_column="Category",
)

## Asthma Diagnosis code
asthma_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ast.csv",
  system = "snomed",
  column = "code",
)

## Asplenia or Dysfunction of the Spleen codes
spln_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-spln_cov.csv",
  system = "snomed",
  column = "code",
)

## Blood pressure
systolic_blood_pressure_codes = codelist(["2469."], system = "ctv3")
diastolic_blood_pressure_codes = codelist(["246A."], system = "ctv3")

## Cancer
lung_cancer_codes = codelist_from_csv(
  "codelists/opensafely-lung-cancer.csv", 
  system="ctv3", 
  column="CTV3ID"
)

haem_cancer_codes = codelist_from_csv(
  "codelists/opensafely-haematological-cancer.csv", 
  system="ctv3", 
  column="CTV3ID"
)

other_cancer_codes = codelist_from_csv(
  "codelists/opensafely-cancer-excluding-lung-and-haematological.csv",
  system="ctv3",
  column="CTV3ID",
)

## Chronic heart disease codes
chd_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-chd_cov.csv",
  system = "snomed",
  column = "code",
)

## Chronic neurological disease including Significant Learning Disorder
cnd_inc_sig_learn_dis_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-cns_cov.csv",
  system = "snomed",
  column = "code",
)

## Chronic respiratory Disease
crs_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-resp_cov.csv",
  system = "snomed",
  column = "code",
)

## Creatine
creatinine_codes = codelist(["XE2q5"], system="ctv3")

## Dialysis
dialysis_codes = codelist_from_csv(
  "codelists/opensafely-dialysis.csv", 
  system = "ctv3", 
  column = "CTV3ID"
)

## Diabetes
diab_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-diab.csv",
  system = "snomed",
  column = "code",
)

## Immunosuppression diagnosis codes
immunosuppression_diagnosis_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immdx_cov.csv",
  system = "snomed",
  column = "code",
)

## Immunosuppression medication codes
immunosuppression_medication_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immrx.csv",
  system = "snomed",
  column = "code",
)

## Learning disabilities
learning_disability_codes = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-ld_cod.csv",
  system = "snomed",
  column = "code",
)

## Liver disease codes
cld_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-cld.csv",
  system = "snomed",
  column = "code",
)

## Severe mental illness
sev_mental_ill_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-sev_mental.csv",
  system = "snomed",
  column = "code",
)

## Kidney transplant
kidney_transplant_codes = codelist_from_csv(
  "codelists/opensafely-kidney-transplant.csv",
  system = "ctv3",
  column = "CTV3ID",
)

## Other organ transplant
other_organ_transplant_codes =  codelist_from_csv(
  "codelists/opensafely-other-organ-transplant.csv",
  system = "ctv3",
  column = "CTV3ID",
)

## Prior covid
covid_icd10 = codelist_from_csv(
  "codelists/opensafely-covid-identification.csv",
  system = "icd10",
  column = "icd10_code",
)

covid_primary_care_code = codelist_from_csv(
  "codelists/opensafely-covid-identification-in-primary-care-probable-covid-clinical-code.csv",
  system = "ctv3",
  column = "CTV3ID",
)

covid_primary_care_positive_test = codelist_from_csv(
  "codelists/opensafely-covid-identification-in-primary-care-probable-covid-positive-test.csv",
  system = "ctv3",
  column = "CTV3ID",
)

covid_primary_care_sequalae = codelist_from_csv(
  "codelists/opensafely-covid-identification-in-primary-care-probable-covid-sequelae.csv",
  system = "ctv3",
  column = "CTV3ID",
)



