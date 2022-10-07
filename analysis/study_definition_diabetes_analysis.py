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

## Variables for deriving JCVI groups
from grouping_variables import (
    jcvi_variables, 
    study_dates,
    start_date,
    end_date,
    pandemic_start
)
import json
study = StudyDefinition(

population = patients.all(),

# Get t2dm from preprocess output
    out_date_t2dm = patients.with_value_from_file(
        f_path = 'output/input_prevax.csv', 
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

    out_count_insulin_snomed=patients.with_these_medications(
        insulin_snomed_clinical,
        on_or_after="out_date_t2dm + 4 months",
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 3, "stddev": 2},
            "date": {"earliest": "2021-02-01", "latest": "2021-05-31"},
            "incidence": 0.30,
        },
    ),

    out_count_antidiabetic_drugs_snomed=patients.with_these_medications(
        antidiabetic_drugs_snomed_clinical,
        on_or_after="out_date_t2dm + 4 months",
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 3, "stddev": 2},
            "date": {"earliest": "2021-02-01", "latest": "2021-05-31"},
            "incidence": 0.30,
        },
    ),

    out_count_nonmetform_drugs_snomed=patients.with_these_clinical_events(
        non_metformin_dmd,
        on_or_after="out_date_t2dm + 4 months",
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 3, "stddev": 2},
            "date": {"earliest": "2021-02-01", "latest": "2021-05-31"},
            "incidence": 0.30,
        },
    )
    
)