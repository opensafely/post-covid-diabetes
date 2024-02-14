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

## Datetime functions
from datetime import date

import json
study = StudyDefinition(

    # Define the study population 
    population = patients.all(
    ), 

    # Pull all available deregestration dates
    deregistered_date=patients.date_deregistered_from_all_supported_practices(
        date_format = 'YYYY-MM-DD',
        return_expectations={
            "date": {"earliest": "2000-01-01", "latest": "today"},
            "rate": "uniform",
            "incidence": 0.01
            },
            )
    
)