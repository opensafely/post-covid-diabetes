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
## Codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

## Datetime functions
from datetime import date

## Study definition helper
import study_definition_helper_functions as helpers

## Variables for deriving JCVI groups
from grouping_variables import (
    study_dates,
    jcvi_variables, 
    start_date,
    end_date,
    pandemic_start
)

study = StudyDefinition(

population = patients.all(),

# Get COVID-19 data (date and whether hospitalised or not) from stage 1 dataset

    sub_cat_covid19_hospital = patients.with_value_from_file(
        f_path = 'output/input_vax_stage1_diabetes.csv.gz', 
        returning = 'sub_cat_covid19_hospital', 
        returning_type = 'str', 
    ),

    exp_date_covid19_confirmed = patients.with_value_from_file(
        f_path = 'output/input_vax_stage1_diabetes.csv.gz', 
        returning = 'exp_date_covid19_confirmed', 
        returning_type = 'date', 
        date_format = 'YYYY-MM-DD',
    ), 

# DATES

    index_date_copy = patients.with_value_from_file(
        f_path = 'output/follow_up_end_dates_vax_diabetes.csv.gz', 
        returning = 'index_date', 
        returning_type = 'date', 
        date_format = 'YYYY-MM-DD',
    ),

    vax_date_covid_1 = patients.with_value_from_file(
        f_path = 'output/input_vax_stage1_diabetes.csv.gz', 
        returning = 'vax_date_covid_1', 
        returning_type = 'date', 
        date_format = 'YYYY-MM-DD',
    ),

    vax_date_eligible = patients.with_value_from_file(
        f_path = 'output/input_vax_stage1_diabetes.csv.gz', 
        returning = 'vax_date_eligible', 
        returning_type = 'date', 
        date_format = 'YYYY-MM-DD',
    ),

    t2dm_follow_up_end = patients.with_value_from_file(
        f_path = 'output/follow_up_end_dates_vax_diabetes.csv.gz', 
        returning = 't2dm_follow_up_end', 
        returning_type = 'date', 
        date_format = 'YYYY-MM-DD',
    ),

# Get t2dm diagnosis date from stage 1 dataset

    out_date_t2dm = patients.with_value_from_file(
        f_path = 'output/input_vax_stage1_diabetes.csv.gz', 
        returning = 'out_date_t2dm', 
        returning_type = 'date', 
        date_format = 'YYYY-MM-DD',
    ),

# Maximum HbA1c measure 4 months after Type 2 Diabetes diagnosis

    out_num_max_hba1c_mmol_4mnths=patients.max_recorded_value(
        hba1c_new_codes,
        on_most_recent_day_of_measurement=True, 
        on_or_after="out_date_t2dm + 4 months",
        date_format="YYYY-MM-DD",
        return_expectations={
            "float": {"distribution": "normal", "mean": 30.0, "stddev": 15},
            "date": {"earliest": "1980-02-01", "latest": "2021-05-31"},
            "incidence": 0.95,
        },
    ),
    out_num_max_hba1c_mmol_4mnths_date=patients.date_of("out_num_max_hba1c_mmol_4mnths", date_format="YYYY-MM-DD"),

# First and Second Prescriptions after 4 Months

    out_count_insulin_snomed_4mnths=patients.with_these_medications(
        insulin_snomed_clinical,
        on_or_after="out_date_t2dm + 4 months",
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 3, "stddev": 2},
            "date": {"earliest": "2021-02-01", "latest": "2021-05-31"},
            "incidence": 0.30,
        },
    ),

    out_count_antidiabetic_drugs_snomed_4mnths=patients.with_these_medications(
        antidiabetic_drugs_snomed_clinical,
        on_or_after="out_date_t2dm + 4 months",
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 3, "stddev": 2},
            "date": {"earliest": "2021-02-01", "latest": "2021-05-31"},
            "incidence": 0.30,
        },
    ),

    out_count_nonmetform_drugs_snomed_4mnths=patients.with_these_clinical_events(
        non_metformin_dmd,
        on_or_after="out_date_t2dm + 4 months",
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 3, "stddev": 2},
            "date": {"earliest": "2021-02-01", "latest": "2021-05-31"},
            "incidence": 0.30,
        },
    ),

# Define death date

    ## Primary care

    primary_care_death_date=patients.with_death_recorded_in_primary_care(
            on_or_after=study_dates["pandemic_start"],
            returning="date_of_death",
            date_format="YYYY-MM-DD",
            return_expectations={
                "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
                "rate": "uniform",
                "incidence": 0.01,
            },
        ),
    ## ONS

    ons_died_from_any_cause_date=patients.died_from_any_cause(
            on_or_after=study_dates["pandemic_start"],
            returning="date_of_death",
            date_format="YYYY-MM-DD",
            return_expectations={
                "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"},
                "rate": "uniform",
                "incidence": 0.01,
            },
        ),
    ## Combined

    death_date=patients.minimum_of(
            "primary_care_death_date", "ons_died_from_any_cause_date"
        ),

# DEREG

    dereg_date=patients.date_deregistered_from_all_supported_practices(
        on_or_after=study_dates["pandemic_start"], date_format = 'YYYY-MM-DD',
                        return_expectations={
                    "date": {"earliest": study_dates["pandemic_start"], "latest": "today"},
                    "rate": "uniform",
                    "incidence": 0.01
                },
    )
    
)