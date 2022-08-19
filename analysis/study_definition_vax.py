## Set seed
import numpy as np
np.random.seed(123456)

# Cohort extractor
from tracemalloc import start
from cohortextractor import (
  StudyDefinition,
  patients,
  date_expressions,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
)

#study dates

## Variables for deriving JCVI groups
from grouping_variables import (
  
    study_dates
)

## Codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

## Datetime functions
from datetime import date

## Study definition helper
import study_definition_helper_functions as helpers

## Import common variables function
from common_variables import generate_common_variables
(
    dynamic_variables
) = generate_common_variables(index_date_variable="index_date_vax", end_date_variable="end_date_vax")


study = StudyDefinition(

    # Read in index date for study from the output of prelim.R file 
    index_date_vax = patients.with_value_from_file(
        f_path = 'output/index_dates.csv', 
        returning = 'index_vax', 
        returning_type = 'date', 
        date_format = 'YYYY-MM-DD',     
    ),

    end_date_vax = patients.with_value_from_file(
        f_path = 'output/index_dates.csv',
        returning = 'end_vax',
        returning_type = 'date', 
        date_format = 'YYYY-MM-DD',
    ),
    
 
    # Configure the expectations framework
    default_expectations={
        "date": {"earliest": study_dates["earliest_expec"], "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },
   
    # # Define the study population 
    # # NB: not all inclusions and exclusions are written into study definition
    population = patients.satisfying(
        """
            NOT has_died
            AND
            registered        
            """,
        
        has_died = patients.died_from_any_cause(
        on_or_before = "index_date_vax",
        returning="binary_flag",
        ),
        
        registered = patients.satisfying(
        "registered_at_start",
        registered_at_start = patients.registered_as_of("index_date_vax"),
        ),
    ),
    
    # Define sex 
    cov_cat_sex = patients.with_value_from_file(
        f_path = 'output/index_dates.csv',
        returning = 'cov_cat_sex',
        returning_type = 'str',  
        ),

    # Define fixed covariates other than sex

        ## 2019 consultation rate
        cov_num_consulation_rate=patients.with_gp_consultations(
                between=["2019-01-01", "2019-12-31"],
                returning="number_of_matches_in_period",
                return_expectations={
                    "int": {"distribution": "poisson", "mean": 5},
                },
            ),
    
        ## Healthcare worker    
        cov_bin_healthcare_worker=patients.with_healthcare_worker_flag_on_covid_vaccine_record(
            returning='binary_flag', 
            return_expectations={"incidence": 0.01},
        ),

# Define quality assurances

        ## Prostate cancer
            ### Primary care
        prostate_cancer_snomed=patients.with_these_clinical_events(
                prostate_cancer_snomed_clinical,
                returning='binary_flag',
                return_expectations={
                    "incidence": 0.03,
                },
            ),
            ### HES APC
        prostate_cancer_hes=patients.admitted_to_hospital(
                with_these_diagnoses=prostate_cancer_icd10,
                returning='binary_flag',
                return_expectations={
                    "incidence": 0.03,
                },
            ),
            ### ONS
        prostate_cancer_death=patients.with_these_codes_on_death_certificate(
                prostate_cancer_icd10,
                returning='binary_flag',
                return_expectations={
                    "incidence": 0.02
                },
            ),
            ### Combined
        qa_bin_prostate_cancer=patients.maximum_of(
                "prostate_cancer_snomed", "prostate_cancer_hes", "prostate_cancer_death"
            ),

        ## Pregnancy
        qa_bin_pregnancy=patients.with_these_clinical_events(
                pregnancy_snomed_clinical,
                returning='binary_flag',
                return_expectations={
                    "incidence": 0.03,
                },
            ),
        
        ## Year of birth
        qa_num_birth_year=patients.date_of_birth(
                date_format="YYYY",
                return_expectations={
                    "date": {"earliest": "1900-01-01", "latest": "today"},
                    "rate": "uniform",
                    "incidence": 1.0
                },
            ),

    has_follow_up_previous_6months=patients.registered_with_one_practice_between(
        start_date="index_date_vax - 6 months",
        end_date="index_date_vax",
        return_expectations={"incidence": 0.95},
    ),

    registered_as_of_6months_before_delta=patients.registered_with_one_practice_between(
        start_date="2020-12-15",
        end_date="2021-06-01",
        return_expectations={"incidence": 0.95},
    ),

    registered_as_of_pandemic_start=patients.registered_with_one_practice_between(
        start_date="2020-01-01",
        end_date="2020-01-01",
        return_expectations={"incidence": 0.95},
    ),

    registered_as_of_6months_before_pandemic_start=patients.registered_with_one_practice_between(
        start_date="2019-07-17",
        end_date="2020-01-01",
        return_expectations={"incidence": 0.95},
    ),

    dereg_date=patients.date_deregistered_from_all_supported_practices(
        on_or_after="2020-01-01", date_format = 'YYYY-MM-DD',
                        return_expectations={
                    "date": {"earliest": "2020-01-01", "latest": "today"},
                    "rate": "uniform",
                    "incidence": 0.01
                },
    ),

    # Define common variables (e.g., exposures, outcomes, covariates) that require dynamic dates

        **dynamic_variables
)