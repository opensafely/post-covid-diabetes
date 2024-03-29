# Import statements

## Set seed
import numpy as np
np.random.seed(123456)

## Cohort extractor
from cohortextractor import (
  StudyDefinition,
  patients,
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

## Import common variables function
from common_variables import generate_common_variables
(
    dynamic_variables
) = generate_common_variables(index_date_variable="index_date_prevax", end_date_variable="end_date_prevax")

## Variables for deriving JCVI groups
from grouping_variables import (
    study_dates,
    jcvi_variables, 
    start_date,
    end_date,
    pandemic_start
)


study = StudyDefinition(

    # Specify index date for study
    index_date_prevax = patients.with_value_from_file(
        f_path = 'output/index_dates.csv', 
        returning = 'index_prevax', 
        returning_type = 'date', 
        date_format = 'YYYY-MM-DD',
        
    ),
     end_date_prevax = patients.with_value_from_file(
        f_path = 'output/index_dates.csv', 
        returning = 'end_prevax', 
        returning_type = 'date', 
        date_format = 'YYYY-MM-DD',     
    ),

    # Configure the expectations framework
    default_expectations={
        "date": {"earliest": study_dates["earliest_expec"], "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },

    # Define the study population 
    # NB: all inclusions and exclusions are performed in stage 1
    population = patients.all(),
    
# Define sex 
    # NB: this is required for JCVI variables hence is defined here
    cov_cat_sex = patients.with_value_from_file(
        f_path = 'output/index_dates.csv',
        returning = 'cov_cat_sex',
        returning_type = 'str',  
        ),
    # Death date
    # death_date = patients.with_value_from_file(
    #     f_path = 'output/index_dates.csv',
    #     returning = 'death_date',
    #     returning_type = 'date', 

    # ),
    # Define vaccine eligibility variables

        ## Any covid vaccination, identified by target disease
    vax_date_covid_1 = patients.with_value_from_file(
        f_path = 'output/index_dates.csv',
        returning = 'vax_date_covid_1',
        returning_type = 'date'          
    ),

        **jcvi_variables, 

    # Define common variables (e.g., exposures, outcomes, covariates) that require dynamic dates

        **dynamic_variables
)
