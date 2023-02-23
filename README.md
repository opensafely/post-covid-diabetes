# post-covid-diabetes

This is the code and configuration for the `post-covid-diabetes` repository.

## Protocol
The protocol is available [here](https://uob.sharepoint.com/:w:/r/teams/grp-ehr/_layouts/15/Doc.aspx?sourcedoc=%7BEF4D8C0D-B811-4A56-9A03-030E6A32DCC5%7D&file=post-covid-vaccinated.docx&action=default&mobileredirect=true) (currently restricted access).

## Repository navigation
* If you are interested in how we defined our code lists, look in the [codelists folder](./codelists/).

* Analyses scripts are in the `analysis/` directory:
    * If you are interested in how we defined our variables, we use study definition scripts to define three cohorts: pre-vaccination, vaccinated and unvaccinated. Study start dates (i.e., index) and end dates differ by cohort and are all described in the protocol. Hence, we have a study definition for each; these are written in `python`. Extracted data is then combined to create our final cohorts, in the [preprocess data script](analysis/preprocess_data.R).
    * This directory also contains all the R scripts that process, describe, and analyse the extracted data.

* The `lib/` directory contains a list of active analyses.

* The `project.yaml` defines run-order and dependencies for all the analysis scripts. This file should not be edited directly. To make changes to the yaml, edit and run the `create_project_actions.R` script (available in the `analysis/` directory) which generates all the actions of the `project.yaml`.

* Descriptive and Model outputs, including figures and tables are in the `released_outputs/` directory.

## Manuscript
A first manuscript is being drafted.

## About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality. Read more at [OpenSAFELY.org](https://opensafely.org).

Developers and epidemiologists interested in the framework should review [the OpenSAFELY documentation](https://docs.opensafely.org).

## Licences
As standard, research projects have a MIT license. 
