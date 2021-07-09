######################################

# Some covariates used in the study are created from codelists of clinical conditions or 
# numerical values available on a patient's records.
# This script fetches all of the codelists identified in codelists.txt from OpenCodelists.

######################################


# --- IMPORT STATEMENTS ---

## Import code building blocks from cohort extractor package
from cohortextractor import (codelist, codelist_from_csv, combine_codelists)


# --- CODELISTS ---

## First COVID vaccination administration in EMIS
covid_vaccine_EMIS_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-covadm1.csv",
  system = "snomed",
  column = "code",
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

## Ethnicity
ethnicity_6_codes = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-eth2001.csv",
  system = "snomed",
  column = "code",
  category_column="grouping_6_id",
)

## Learning disabilities
learning_disability_codes = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-ld_cod.csv",
  system = "snomed",
  column = "code",
)

## Organ transplant
organ_transplant_codes = codelist_from_csv(
    "codelists/opensafely-solid-organ-transplantation-snomed.csv",
    system = "snomed",
    column = "id",
)

## Dialysis / kidney disease
ckd_codes = codelist_from_csv(
  "codelists/opensafely-chronic-kidney-disease.csv", 
  system = "ctv3", 
  column = "CTV3ID"
)
