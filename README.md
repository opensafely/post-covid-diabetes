# Diabetes following SARS-CoV-2 infection: Incidence, persistence and implications of COVID-19 vaccination: A cohort study of fifteen million people.

Abstract:

You can run this project via [Gitpod](https://gitpod.io) in a web browser by clicking on this badge: [![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-908a85?logo=gitpod)](https://gitpod.io/#https://github.com/opensafely/post-covid-vaccinated)

-   The preprint can be found
-   The project protocol which contains in depth details of the analysis plan can be found [`here`](https://uob.sharepoint.com/:w:/r/teams/grp-ehr/_layouts/15/Doc.aspx?sourcedoc=%7BEF4D8C0D-B811-4A56-9A03-030E6A32DCC5%7D&file=post-covid-vaccinated.docx&action=default&mobileredirect=true) (currently restricted access).
-   Developers and epidemiologists interested in the framework should review the [`OpenSAFELY documentation`](https://docs.opensafely.org/)

## Repository navigation

-   If you are interested in how we defined our code lists, look in the [`codelists`](./codelists) folder.

-   Analyses scripts are in the [`analysis`](./analysis) directory:

    -   If you are interested in how we defined our variables, we use study definition scripts to define three cohorts: pre-vaccination, vaccinated and unvaccinated. Study start dates (i.e., index) and end dates differ by cohort and are all described in the protocol. Hence, we have a study definition for each; these are written in `python`. Extracted data is then combined to create our final cohorts, in the [preprocess data script](analysis/preprocess_data.R).
    -   This directory also contains all the R scripts that process, describe, and analyse the extracted data.

-   The [`lib/`](./lib) directory contains a list of active analyses.

-   The [`project.yaml`](.project.yaml) defines run-order and dependencies for all the analysis scripts. This file should not be edited directly. To make changes to the yaml, edit and run the [`create_project.R`](./analysis/create_project.R) script which generates all the actions.

-   Descriptive and Model outputs, including figures and tables are in the [`released_outputs`](./release_outputs) directory.

## Manuscript

A first manuscript is being drafted.

### project.yaml

The [`project.yaml`](./project.yaml) defines project actions, run-order and dependencies for all analysis scripts. **This file should *not* be edited directly**. To make changes to the yaml, edit and run the [`create_project.R`](./analysis/create_project.R) script instead. Project actions are then run securely using [OpenSAFELY Jobs](https://jobs.opensafely.org/repo/https%253A%252F%252Fgithub.com%252Fopensafely%252Fpost-covid-vaccinated). Details of the purpose and any published outputs from this project can be found at this link as well.

Below is a description of each action in the [`project.yaml`](./project.yaml). Arguments are denoted by {arg} in the action name.

-   `vax_eligibility_inputs`
    -   Runs [`vax_eligibility_inputs.R`](./analysis/vax_eligibility_inputs.R) which creates metadata for aspects of the study design which are required for the `generate_study_population` actions.
    -   Creates dataframes that contain dates for each phase of vaccination and conditions for defining JCVI vacciantion groups
-   `generate_study_population_{cohort}`
    -   There are three `generate_study_population` scripts that are run to create the two study populations: vaccinated and unvaccinated. These are [`study_definition_vaccinated.py`](./analysis/study_definition_vaccinated.py), [`study_definition_electively_unvaccinated.py`](./analysis/study_definition_electively_unvaccinated.py) and [`study_definition_index.py`](./analysis/study_definition_index.py).
    -   These scripts are used to define JCVI groups, vaccine variables, variables used to apply eligibility criteria, outcome variables and covariate variables. Common variables used in all three scripts can be found in [`common_variables.py`](./analysis/common_variables.py).
-   `preprocess_data_{cohort}`
    -   Runs [`preprocess_data.R`](./analysis/preprocess/preprocess_data.R) to apply dataframe tidying to `input_{cohort}.feather` (generated by `generate_study_population_{cohort}`)
    -   Tidies vaccine variables, determines patient study index date, creates addtional variables (e.g. covid phenotype), tidies dataset and ensures all variables are in the correct format e.g. numeric, character etc.
-   `stage1_data_cleaning_both`
    -   Runs [`Stage1_data_cleaning.R`](./analysis/preprocess/Stage1_data_cleaning.R)
    -   Applies quality assurance rule and inclusion/exclusion criteria
    -   Outputted dataset is analysis ready
-   `stage1_end_date_table_{cohort}`
    -   Runs [`create_follow_up_end_date.R`](./analysis/preprocess/create_follow_up_end_date.R) which creates a dataframe of patient study end dates for each outcome of interest.
-   `stage2_missing_table1_both`
    -   Runs [`Stage2_missing_table1.R`](./analysis/descriptives/Stage2_missing_table1.R)
    -   Check for missing data within variables
    -   Creates the summary statistics for Table 1 of the manuscript which can then be outputted
-   `stage4_table_2_{cohort}`
    -   Runs [`table_2.R`](./analysis/descriptives/table_2.R) which calculates pre- and post-exposure event counts and person days of follow-up for all outcomes and subgroups.
    -   Used for Table 2 in the manuscript
-   `stage4_venn_diagram_both`
    -   Runs [`venn_diagram.R`](./anlaysis/descriptives/venn_diagram.R)
    -   Creates venn diagram data for all outcomes reporting where outcomes are sourced from i.e primary care, secondary care or deaths data
-   `Analysis_cox_{outcome}_{cohort}`
    -   Runs [`01_cox_pipeline.R`](./analysis/model/01_cox_pipeline.R)
    -   Each action runs all subgroups for the outcome and cohort of interest
    -   Detailed descriptions of each script used to fit the cox models can be in the model [`README`](./analysis/model/README_model_scripts.md) file
-   `stata_cox_model_{outcome}_{subgroup}_{cohort}_{time_periods}`
    -   Runs [`cox_model.do`](./analysis/cox_model.do) and [`cox_model_day0.do`](./analysis/cox_model_day0.do)
    -   When models do not fit successfully in R they are run in stata instead
-   `format_stata_output`
    -   Runs [`format_stata_output.R`](./analysis/format_stata_output.R) which combines all the stata results in one formatted .csv file
-   `format_R_output`
    -   Runs [`07_combine_HRs_to_one_file.R`](./analysis/model/07_combine_HRs_to_one_file.R) which combines all the R results in one formatted .csv file

### Creating the study population

In OpenSAFELY a study definition is a formal specification of the data that you want to extract from the OpenSAFELY database. This includes:

-   the patient population (dataset rows)
-   the variables (dataset columns)
-   the expected distributions of these variables for use in dummy data

Further details on creating the study population can be found in the [`OpenSAFELY documentation`](https://docs.opensafely.org/study-def/).

## About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic health records research in the NHS, with a focus on public accountability and research quality. Read more at [OpenSAFELY.org](https://opensafely.org).

## Licences

As standard, research projects have a MIT license.
