library(tidyverse)
library(yaml)
library(here)
library(glue)
library(readr)
#library(dplyr)


###########################
# Load information to use #
###########################

## defaults ----
defaults_list <- list(
  version = "3.0",
  expectations= list(population_size=200000L)
)

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- subset(active_analyses, active_analyses$active =="TRUE")

analyses_to_run <- active_analyses[,c("outcome_variable","cohort")]
analyses_to_run$cohort <- ifelse(analyses_to_run$cohort=="all","prevax;vax;unvax",analyses_to_run$cohort)
analyses_to_run <- tidyr::separate_rows(analyses_to_run, cohort, sep = ";")
analyses_to_run$outcome <- str_replace(analyses_to_run$outcome_variable,"out_date_", "")

# DATA ONLY ANALYSES WHERE MODELS ARE NOT CONVERGING

analyses_to_run <- analyses_to_run %>%
  mutate(data_only = 
           # ifelse(outcome_variable == "out_date_t2dm_extended_follow_up" & cohort == "prevax", "TRUE",
           ifelse(outcome_variable == "out_date_t2dm_obes_no" & cohort == "prevax", "TRUE",
                  ifelse(outcome_variable == "out_date_t2dm_obes_no" & cohort == "vax", "TRUE",
                         ifelse(outcome_variable == "out_date_t2dm_pd_no" & cohort == "prevax", "TRUE",
                                ifelse(outcome_variable == "out_date_t2dm_pd_no" & cohort == "vax", "TRUE",
                                       ifelse(outcome_variable == "out_date_t2dm_pd_no" & cohort == "unvax", "TRUE",
                                              ifelse(outcome_variable == "out_date_t2dm_pre_rec" & cohort == "prevax", "TRUE",
                                                     ifelse(outcome_variable == "out_date_t2dm_pre_rec" & cohort == "unvax", "TRUE",
                                                            ifelse(outcome_variable == "out_date_t2dm" & cohort == "prevax", "TRUE",
                                                                   ifelse(outcome_variable == "out_date_t2dm" & cohort == "unvax", "TRUE",
                                                                          ifelse(outcome_variable == "out_date_t2dm_unvax_sens" & cohort == "unvax", "TRUE",
                                                                                 ifelse(outcome_variable == "out_date_t2dm_extended_follow_up", "TRUE",  
                                                                          "FALSE"))))))))))))


cohort_to_run_all <- c("prevax", "vax", "unvax")

# data_only_all <- active_analyses_table_all$data_only

analyses <- c("main", "subgroups")

# ANALYSES TO RUN STATA

analyses_to_run_stata <- read.csv("lib/analyses_to_run_in_stata.csv")
analyses_to_run_stata$subgroup <- ifelse(analyses_to_run_stata$subgroup=="hospitalised","covid_pheno_hospitalised",analyses_to_run_stata$subgroup)
analyses_to_run_stata$subgroup <- ifelse(analyses_to_run_stata$subgroup=="non_hospitalised","covid_pheno_non_hospitalised",analyses_to_run_stata$subgroup)

# create action functions ----

############################
## generic action function #
############################
action <- function(
    name,
    run,
    dummy_data_file=NULL,
    arguments=NULL,
    needs=NULL,
    highly_sensitive=NULL,
    moderately_sensitive=NULL
){
  
  outputs <- list(
    moderately_sensitive = moderately_sensitive,
    highly_sensitive = highly_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL
  
  action <- list(
    run = paste(c(run, arguments), collapse=" "),
    dummy_data_file = dummy_data_file,
    needs = needs,
    outputs = outputs
  )
  action[sapply(action, is.null)] <- NULL
  
  action_list <- list(name = action)
  names(action_list) <- name
  
  action_list
}


## create comment function ----
comment <- function(...){
  list_comments <- list(...)
  comments <- map(list_comments, ~paste0("## ", ., " ##"))
  comments
}


## create function to convert comment "actions" in a yaml string into proper comments
convert_comment_actions <-function(yaml.txt){
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}


#################################################
## Function for typical actions to analyse data #
#################################################
# Updated to a typical action running Cox models for one outcome
apply_model_function <- function(outcome, cohort, data_only){
  splice(
    comment(glue("Cox model for {outcome} - {cohort}")),
    action(
      name = glue("Analysis_cox_{outcome}_{cohort}"),
      run = "r:latest analysis/model/01_cox_pipeline.R",
      arguments = c(outcome,cohort,data_only),
      needs = list("stage1_data_cleaning_prevax", "stage1_data_cleaning_vax", "stage1_data_cleaning_unvax", 
                   glue("stage1_end_date_table_{cohort}"),
                   glue("diabetes_post_hoc_{cohort}")),
      moderately_sensitive = list(
        analyses_not_run = glue("output/review/model/*/analyses_not_run_{outcome}_{cohort}.csv"),
        compiled_hrs_csv = glue("output/review/model/*/suppressed_compiled_HR_results_{outcome}_{cohort}.csv"),
        compiled_hrs_csv_to_release = glue("output/review/model/*/suppressed_compiled_HR_results_{outcome}_{cohort}_to_release.csv"),
        compiled_event_counts_csv = glue("output/review/model/*/suppressed_compiled_event_counts_{outcome}_{cohort}.csv"),
        compiled_event_counts_csv_non_supressed = glue("output/review/model/*/compiled_event_counts_{outcome}_{cohort}.csv"),
        describe_data_surv = glue("output/not-for-review/describe_data_surv_{outcome}_*_{cohort}_*_time_periods.txt")
      ),
      highly_sensitive = list(
        dataset = glue("output/input_{outcome}_*_{cohort}_*_time_periods.csv"),
        sampled_dataset = glue("output/input_sampled_data_{outcome}_*_{cohort}_*_time_periods.csv")
      )
    )
  )
}

# Updated to a typical action running Cox models for one outcome
# apply_model_function_covariate_testing <- function(outcome, cohort){
#   splice(
#     comment(glue("Cox model {outcome} - {cohort}, covariate_testing")),
#     action(
#       name = glue("Analysis_cox_{outcome}_{cohort}_covariate_testing"),
#       run = "r:latest analysis/model/01_cox_pipeline.R",
#       arguments = c(outcome,cohort,"test_all"),
#       needs = list("stage1_data_cleaning_prevax", "stage1_data_cleaning_vax", "stage1_data_cleaning_unvax", glue("stage1_end_date_table_{cohort}")),
#       moderately_sensitive = list(
#         analyses_not_run = glue("output/review/model/analyses_not_run_{outcome}_{cohort}_covariate_testing_test_all.csv"),
#         compiled_hrs_csv = glue("output/review/model/suppressed_compiled_HR_results_{outcome}_{cohort}_covariate_testing_test_all.csv"),
#         compiled_hrs_csv_to_release = glue("output/review/model/suppressed_compiled_HR_results_{outcome}_{cohort}_covariate_testing_test_all_to_release.csv"),
#         compiled_event_counts_csv = glue("output/review/model/suppressed_compiled_event_counts_{outcome}_{cohort}_covariate_testing_test_all.csv"),
#         compiled_event_counts_csv_non_supressed = glue("output/review/model/compiled_event_counts_{outcome}_{cohort}_covariate_testing_test_all.csv"),
#         describe_data_surv = glue("output/not-for-review/describe_data_surv_{outcome}_*_{cohort}_*_covariate_testing_test_all.txt")
#       )
#     )
#   )
# }
table2 <- function(cohort){
  splice(
    comment(glue("Stage 4 - Table 2 - {cohort} cohort")),
    action(
      name = glue("stage4_table_2_{cohort}"),
      run = "r:latest analysis/descriptives/table_2.R",
      arguments = c(cohort),
      needs = list("stage1_data_cleaning_prevax", "stage1_data_cleaning_vax", "stage1_data_cleaning_unvax",glue("stage1_end_date_table_{cohort}"),
                   "diabetes_post_hoc_prevax", "diabetes_post_hoc_vax", "diabetes_post_hoc_unvax", "diabetes_post_hoc_prevax_extended_follow_up"),
      moderately_sensitive = list(
        input_table_2 = glue("output/review/descriptives/table2_{cohort}_*.csv")
      )
    )
  )
}

stata_actions <- function(outcome, cohort, subgroup, time_periods, day0, extf){
  splice(
    action(
      name = glue("stata_cox_model_{outcome}_{subgroup}_{cohort}_{time_periods}_day0{day0}_extf{extf}"),
      run = glue("stata-mp:latest analysis/cox_model.do input_sampled_data_{outcome}_{subgroup}_{cohort}_{time_periods}_time_periods {day0} {extf}"),
      needs = list(glue("Analysis_cox_{outcome}_{cohort}")),
      moderately_sensitive = list(
        medianfup = glue("output/input_sampled_data_{outcome}_{subgroup}_{cohort}_{time_periods}_time_periods_stata_median_fup_day0{day0}_extf{extf}.csv"),
        stata_output = glue("output/input_sampled_data_{outcome}_{subgroup}_{cohort}_{time_periods}_time_periods_cox_model_day0{day0}_extf{extf}.txt")
      )
    )
  )
}

##########################################################
## Define and combine all actions into a list of actions #
##########################################################
actions_list <- splice(
  
  comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #",
          "DO NOT EDIT project.yaml DIRECTLY",
          "This file is created by create_project_actions.R",
          "Edit and run create_project_actions.R to update the project.yaml",
          "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
  ),
  
  #comment("Generate vaccination eligibility information"),
  action(
    name = glue("vax_eligibility_inputs"),
    run = "r:latest analysis/metadates.R",
    highly_sensitive = list(
      study_dates_json = glue("output/study_dates.json"),
      vax_jcvi_groups= glue("output/vax_jcvi_groups.csv"),
      vax_eligible_dates= ("output/vax_eligible_dates.csv")
    )
  ),
  #comment("Generate dummy data for study_definition - population_prelim"),
  action(
    name = "generate_study_population_prelim",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_prelim --output-format feather",
    needs = list("vax_eligibility_inputs"),
    highly_sensitive = list(
      cohort = glue("output/input_prelim.feather")
    )
  ),
  #comment("Generate dates for all study cohorts"),
  action(
    name = "generate_index_dates",
    run = "r:latest analysis/prelim.R",
    needs = list("vax_eligibility_inputs","generate_study_population_prelim"),
    highly_sensitive = list(
      index_dates = glue("output/index_dates.csv")
    )
  ),
  
  #comment("Generate dummy data for study_definition - prevax"),
  action(
    name = "generate_study_population_prevax",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_prevax --output-format feather",
    needs = list("vax_eligibility_inputs","generate_index_dates"),
    highly_sensitive = list(
      cohort = glue("output/input_prevax.feather")
    )
  ),
  
  #comment("Generate dummy data for study_definition - vax"),
  action(
    name = "generate_study_population_vax",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_vax --output-format feather",
    needs = list("generate_index_dates","vax_eligibility_inputs"),
    highly_sensitive = list(
      cohort = glue("output/input_vax.feather")
    )
  ),
  
  #comment("Generate dummy data for study_definition - unvax"),
  action(
    name = "generate_study_population_unvax",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_unvax --output-format feather",
    needs = list("vax_eligibility_inputs","generate_index_dates"),
    highly_sensitive = list(
      cohort = glue("output/input_unvax.feather")
    )
  ),
  
  #comment("Preprocess data - prevax"),
  action(
    name = "preprocess_data_prevax",
    run = "r:latest analysis/preprocess/preprocess_data.R prevax",
    needs = list("generate_index_dates", "generate_study_population_prelim", "generate_study_population_prevax", "generate_study_population_vax", "generate_study_population_unvax"),
    moderately_sensitive = list(
      describe = glue("output/not-for-review/describe_input_prevax_*.txt")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_prevax.rds"),
      venn = glue("output/venn_prevax.rds")
    )
  ), 
  
  #comment("Preprocess data - vax"),
  action(
    name = "preprocess_data_vax",
    run = "r:latest analysis/preprocess/preprocess_data.R vax",
    needs = list("generate_index_dates", "generate_study_population_prelim", "generate_study_population_prevax", "generate_study_population_vax", "generate_study_population_unvax"),
    moderately_sensitive = list(
      describe = glue("output/not-for-review/describe_input_vax_*.txt")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_vax.rds"),
      venn = glue("output/venn_vax.rds")
    )
  ), 
  
  #comment("Preprocess data - unvax"),
  action(
    name = "preprocess_data_unvax",
    run = "r:latest analysis/preprocess/preprocess_data.R unvax",
    needs = list("generate_index_dates", "generate_study_population_prelim", "generate_study_population_prevax", "generate_study_population_vax", "generate_study_population_unvax"),
    moderately_sensitive = list(
      describe = glue("output/not-for-review/describe_input_unvax_*.txt")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_unvax.rds"),
      venn = glue("output/venn_unvax.rds")
    )
  ), 
  
  #comment("Stage 1 - Data cleaning - PREVAX cohort"),
  action(
    name = "stage1_data_cleaning_prevax",
    run = "r:latest analysis/preprocess/Stage1_data_cleaning.R prevax",
    needs = list("preprocess_data_prevax","preprocess_data_vax", "preprocess_data_unvax","vax_eligibility_inputs"),
    moderately_sensitive = list(
      refactoring = glue("output/not-for-review/meta_data_factors_prevax.csv"),
      QA_rules = glue("output/review/descriptives/QA_summary_prevax_*.csv"),
      IE_criteria = glue("output/review/descriptives/Cohort_flow_prevax_*.csv"),
      histograms = glue("output/not-for-review/numeric_histograms_prevax_*.svg")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_prevax_*.rds"),
      cohort_csv = glue("output/input_prevax_*.csv.gz")
    )
  ),
  
  #comment("Stage 1 - Data cleaning - VAX cohort"),
  action(
    name = "stage1_data_cleaning_vax",
    run = "r:latest analysis/preprocess/Stage1_data_cleaning.R vax",
    needs = list("preprocess_data_prevax","preprocess_data_vax", "preprocess_data_unvax","vax_eligibility_inputs"),
    moderately_sensitive = list(
      refactoring = glue("output/not-for-review/meta_data_factors_vax.csv"),
      QA_rules = glue("output/review/descriptives/QA_summary_vax_*.csv"),
      IE_criteria = glue("output/review/descriptives/Cohort_flow_vax_*.csv"),
      histograms = glue("output/not-for-review/numeric_histograms_vax_*.svg")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_vax_*.rds"),
      cohort_csv = glue("output/input_vax_*.csv.gz")
    )
  ),
  
  #comment("Stage 1 - Data cleaning - UNVAX cohort"),
  action(
    name = "stage1_data_cleaning_unvax",
    run = "r:latest analysis/preprocess/Stage1_data_cleaning.R unvax",
    needs = list("preprocess_data_prevax","preprocess_data_vax", "preprocess_data_unvax","vax_eligibility_inputs"),
    moderately_sensitive = list(
      refactoring = glue("output/not-for-review/meta_data_factors_unvax.csv"),
      QA_rules = glue("output/review/descriptives/QA_summary_unvax_*.csv"),
      IE_criteria = glue("output/review/descriptives/Cohort_flow_unvax_*.csv"),
      histograms = glue("output/not-for-review/numeric_histograms_unvax_*.svg")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_unvax_*.rds"),
      cohort_csv = glue("output/input_unvax_*.csv.gz")
    )
  ),
  
  #comment("Stage 1 - End date table - prevax"),
  action(
    name = "stage1_end_date_table_prevax",
    run = "r:latest analysis/preprocess/create_follow_up_end_date.R prevax",
    needs = list("preprocess_data_prevax","preprocess_data_vax", "preprocess_data_unvax", "stage1_data_cleaning_prevax","vax_eligibility_inputs"),
    highly_sensitive = list(
      end_date_table = glue("output/follow_up_end_dates_prevax_*.rds"),
      end_date_table_csv = glue("output/follow_up_end_dates_prevax_*.csv.gz")
    )
  ),
  
  #comment("Stage 1 - End date table - vax"),
  action(
    name = "stage1_end_date_table_vax",
    run = "r:latest analysis/preprocess/create_follow_up_end_date.R vax",
    needs = list("preprocess_data_prevax","preprocess_data_vax", "preprocess_data_unvax", "stage1_data_cleaning_vax","vax_eligibility_inputs"),
    highly_sensitive = list(
      end_date_table = glue("output/follow_up_end_dates_vax_*.rds"),
      end_date_table_csv = glue("output/follow_up_end_dates_vax_*.csv.gz")
    )
  ),
  
  #comment("Stage 1 - End date table - unvax"),
  action(
    name = "stage1_end_date_table_unvax",
    run = "r:latest analysis/preprocess/create_follow_up_end_date.R unvax",
    needs = list("preprocess_data_prevax","preprocess_data_vax", "preprocess_data_unvax", "stage1_data_cleaning_unvax","vax_eligibility_inputs"),
    highly_sensitive = list(
      end_date_table = glue("output/follow_up_end_dates_unvax_*.rds"),
      end_date_table_csv = glue("output/follow_up_end_dates_unvax_*.csv.gz")
    )
  ),
  
  #comment("Generate dummy data for study_definition - PREVAX diabetes analysis"),
  action(
    name = "generate_study_population_prevax_diabetes_analyis",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_prevax_diabetes_analysis --output-format feather",
    needs = list("vax_eligibility_inputs", "stage1_data_cleaning_prevax", "stage1_end_date_table_prevax"),
    highly_sensitive = list(
      cohort = glue("output/input_prevax_diabetes_analysis.feather")
    )
  ),
  
  #comment("Generate dummy data for study_definition - VAX diabetes analysis"),
  action(
    name = "generate_study_population_vax_diabetes_analyis",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_vax_diabetes_analysis --output-format feather",
    needs = list("vax_eligibility_inputs", "stage1_data_cleaning_vax", "stage1_end_date_table_vax"),
    highly_sensitive = list(
      cohort = glue("output/input_vax_diabetes_analysis.feather")
    )
  ),
  
  #comment("Generate dummy data for study_definition - UNVAX diabetes analysis"),
  action(
    name = "generate_study_population_unvax_diabetes_analyis",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_unvax_diabetes_analysis --output-format feather",
    needs = list("vax_eligibility_inputs", "stage1_data_cleaning_unvax", "stage1_end_date_table_unvax"),
    highly_sensitive = list(
      cohort = glue("output/input_unvax_diabetes_analysis.feather")
    )
  ),
  
  #comment("Diabetes additional analysis - prevax"),
  action(
    name = "diabetes_post_hoc_prevax",
    run = "r:latest analysis/descriptives/diabetes-follow-up-analysis.R prevax",
    needs = list("generate_study_population_prevax_diabetes_analyis", "generate_study_population_vax_diabetes_analyis", "generate_study_population_unvax_diabetes_analyis", "vax_eligibility_inputs", "stage1_data_cleaning_prevax"),
    moderately_sensitive = list(
      res_table = glue("output/review/descriptives/diabetes_posthoc_analysis_res_*_prevax.csv"),
      histograms = glue("output/review/descriptives/days_*_prevax.png")
    ),
    highly_sensitive = list(
      cohort_new = glue("output/input_prevax_stage1_diabetes.rds")
    )
  ),
  
  #comment("Diabetes additional analysis - vax"),
  action(
    name = "diabetes_post_hoc_vax",
    run = "r:latest analysis/descriptives/diabetes-follow-up-analysis.R vax",
    needs = list("generate_study_population_prevax_diabetes_analyis", "generate_study_population_vax_diabetes_analyis", "generate_study_population_unvax_diabetes_analyis", "vax_eligibility_inputs", "stage1_data_cleaning_vax"),
    moderately_sensitive = list(
      res_table = glue("output/review/descriptives/diabetes_posthoc_analysis_res_*_vax.csv"),
      histograms = glue("output/review/descriptives/days_*_vax.png")
    ),
    highly_sensitive = list(
      cohort_new = glue("output/input_vax_stage1_diabetes.rds")
    )
  ),
  
  #comment("Diabetes additional analysis - unvax"),
  action(
    name = "diabetes_post_hoc_unvax",
    run = "r:latest analysis/descriptives/diabetes-follow-up-analysis.R unvax",
    needs = list("generate_study_population_prevax_diabetes_analyis", "generate_study_population_vax_diabetes_analyis", "generate_study_population_unvax_diabetes_analyis", "vax_eligibility_inputs", "stage1_data_cleaning_unvax"),
    moderately_sensitive = list(
      res_table = glue("output/review/descriptives/diabetes_posthoc_analysis_res_*_unvax.csv"),
      histograms = glue("output/review/descriptives/days_*_unvax.png")
    ),
    highly_sensitive = list(
      cohort_new = glue("output/input_unvax_stage1_diabetes.rds")
    )
  ),
  
  #comment("Diabetes additional analysis - prevax - extended follow up"),
  action(
    name = "diabetes_post_hoc_prevax_extended_follow_up",
    run = "r:latest analysis/descriptives/diabetes-follow-up-analysis-extended-follow-up.R prevax",
    needs = list("generate_study_population_prevax_diabetes_analyis", "generate_study_population_vax_diabetes_analyis", "generate_study_population_unvax_diabetes_analyis", "vax_eligibility_inputs", "stage1_data_cleaning_prevax"),
    moderately_sensitive = list(
      res_table = glue("output/review/descriptives/diabetes_posthoc_analysis_res_EXTENDED_*_prevax.csv")
    )
  ),
  
  #comment("Stage 2 - Missing - Table 1 - all cohorts"),
  action(
    name = "stage2_missing_table1_all",
    run = "r:latest analysis/descriptives/Stage2_missing_table1.R all",
    needs = list("stage1_data_cleaning_prevax", "stage1_data_cleaning_vax", "stage1_data_cleaning_unvax"),
    moderately_sensitive = list(
      Missing_RangeChecks = glue("output/not-for-review/Check_missing_range_*.csv"),
      DateChecks = glue("output/not-for-review/Check_dates_range_*.csv"),
      Descriptive_Table = glue("output/review/descriptives/Table1_*.csv")
    )
  ),
  
  # #comment("Format Table 1"),
  # action(
  #   name = "format_table1",
  #   run = "r:latest analysis/descriptives/format_table1.R",
  #   needs = list("stage2_missing_table1_all"),
  #   moderately_sensitive = list(
  #     formatted_tables = glue("output/review/descriptives/Table1_Formatted_To_Release_*.csv")
  #   )
  # ),
  
  
  #comment("Stage 3 - Diabetes flow - prevax"),  
  
  action(
    name = "stage3_diabetes_flow_prevax",
    run = "r:latest analysis/descriptives/diabetes_flowchart.R prevax",
    needs = list("stage1_data_cleaning_prevax", "stage1_data_cleaning_vax", "stage1_data_cleaning_unvax"),
    moderately_sensitive = list(
      flow_df = glue("output/review/figure-data/diabetes_flow_values_prevax_*.csv")
    ),
  ),
  
  #comment("Stage 3 - Diabetes flow - vax"),  
  
  action(
    name = "stage3_diabetes_flow_vax",
    run = "r:latest analysis/descriptives/diabetes_flowchart.R vax",
    needs = list("stage1_data_cleaning_prevax", "stage1_data_cleaning_vax", "stage1_data_cleaning_unvax"),
    moderately_sensitive = list(
      flow_df = glue("output/review/figure-data/diabetes_flow_values_vax_*.csv")
    ),
  ),
  
  #comment("Stage 3 - Diabetes flow - unvax"),  
  
  action(
    name = "stage3_diabetes_flow_unvax",
    run = "r:latest analysis/descriptives/diabetes_flowchart.R unvax",
    needs = list("stage1_data_cleaning_prevax", "stage1_data_cleaning_vax", "stage1_data_cleaning_unvax"),
    moderately_sensitive = list(
      flow_df = glue("output/review/figure-data/diabetes_flow_values_unvax_*.csv")
    ),
  ),
  
  
  #comment("Stage 4 - Create input for table2"),
  splice(
    # over outcomes
    unlist(lapply(cohort_to_run_all, function(x) table2(cohort = x)), recursive = FALSE)
  ),
  
  # #comment("Stage 4 - Venn diagrams - all cohorts"),
  # action(
  #   name = "stage4_venn_diagram_all",
  #   run = "r:latest analysis/descriptives/venn_diagram.R all",
  #   needs = list("preprocess_data_prevax","preprocess_data_vax", "preprocess_data_unvax", "stage1_data_cleaning_prevax", "stage1_data_cleaning_vax", "stage1_data_cleaning_unvax",
  #                "stage1_end_date_table_prevax", "stage1_end_date_table_vax", "stage1_end_date_table_unvax"),
  #   moderately_sensitive = list(
  #     venn_diagram = glue("output/review/venn-diagrams/venn_diagram_*")
  #     )
  # ),
  
  #comment("Stage 5 - Apply models - outcomes ran on all cohorts"),
  
  splice(unlist(lapply(1:nrow(analyses_to_run), 
                       function(i) apply_model_function(outcome = analyses_to_run[i, "outcome"],
                                                        cohort = analyses_to_run[i, "cohort"],
                                                        data_only = analyses_to_run[i, "data_only"])),
                recursive = FALSE)),
  
  # STATA ANALYSES
  
  splice(unlist(lapply(1:nrow(analyses_to_run_stata), 
                       function(i) stata_actions(outcome = analyses_to_run_stata[i, "outcome"],
                                                 subgroup = analyses_to_run_stata[i, "subgroup"],
                                                 cohort = analyses_to_run_stata[i, "cohort"],
                                                 time_periods = analyses_to_run_stata[i, "time_periods"],
                                                 day0 = analyses_to_run_stata[i, "day0"],
                                                 extf = analyses_to_run_stata[i, "extf"])),
                recursive = FALSE)),
  
  #comment("Format Stata output"),
  action(
    name = "format_stata_output",
    run = "r:latest analysis/format_stata_output.R",
    needs = as.list(paste0("stata_cox_model_",analyses_to_run_stata$outcome,"_",analyses_to_run_stata$subgroup,"_",analyses_to_run_stata$cohort,"_",analyses_to_run_stata$time_periods,"_day0",analyses_to_run_stata$day0,"_extf",analyses_to_run_stata$extf)),
    moderately_sensitive = list(
      stata_output = "output/stata_output.csv")
  ),
  
  #comment("Format hazard ratio output")
  action(
    name = "format_hazard_ratios",
    run = "r:latest analysis/model/format_hazard_ratio_outputs.R",
    needs = list("format_stata_output",
                 "stage4_table_2_prevax", "stage4_table_2_vax", "stage4_table_2_unvax",
                 "Analysis_cox_t1dm_prevax", "Analysis_cox_t1dm_vax", "Analysis_cox_t1dm_unvax",
                 "Analysis_cox_t2dm_prevax", "Analysis_cox_t2dm_vax", "Analysis_cox_t2dm_unvax",
                 "Analysis_cox_t2dm_pre_rec_prevax", "Analysis_cox_t2dm_pre_rec_vax", "Analysis_cox_t2dm_pre_rec_unvax",
                 "Analysis_cox_t2dm_pd_prevax", "Analysis_cox_t2dm_pd_vax", "Analysis_cox_t2dm_pd_unvax",
                 "Analysis_cox_t2dm_pd_no_prevax", "Analysis_cox_t2dm_pd_no_vax", "Analysis_cox_t2dm_pd_no_unvax",
                 "Analysis_cox_t2dm_obes_prevax", "Analysis_cox_t2dm_obes_vax", "Analysis_cox_t2dm_obes_unvax",
                 "Analysis_cox_t2dm_obes_no_prevax", "Analysis_cox_t2dm_obes_no_vax", "Analysis_cox_t2dm_obes_no_unvax",
                 "Analysis_cox_otherdm_prevax", "Analysis_cox_otherdm_vax", "Analysis_cox_otherdm_unvax",
                 "Analysis_cox_gestationaldm_prevax", "Analysis_cox_gestationaldm_vax", "Analysis_cox_gestationaldm_unvax",
                 "Analysis_cox_t2dm_rec_prevax",
                 "Analysis_cox_t2dm_post_rec_prevax",
                 "Analysis_cox_t2dm_follow_prevax",
                 "Analysis_cox_t1dm_extended_follow_up_prevax", "Analysis_cox_t2dm_extended_follow_up_prevax", "Analysis_cox_otherdm_extended_follow_up_prevax", "Analysis_cox_gestationaldm_extended_follow_up_prevax",
                 "Analysis_cox_t2dm_follow_extended_follow_up_prevax", "Analysis_cox_t2dm_pd_extended_follow_up_prevax", "Analysis_cox_t2dm_pd_no_extended_follow_up_prevax",
                 "Analysis_cox_t2dm_obes_extended_follow_up_prevax", "Analysis_cox_t2dm_obes_no_extended_follow_up_prevax",
                 "Analysis_cox_t2dm_unvax_sens_unvax", "Analysis_cox_t1dm_unvax_sens_unvax", "Analysis_cox_otherdm_unvax_sens_unvax", "Analysis_cox_gestationaldm_unvax_sens_unvax"),
    moderately_sensitive = list(
      hr_output = "output/review/model/hr_output_formatted.csv",
      hr_output_no_events = "output/review/model/hr_output_formatted_no_event_counts.csv",
      table2_output = "output/review/model/table2_output_formatted_no_hrs.csv",
      R_event_counts = "output/review/model/R_event_count_output.csv",
      R_event_counts_day_zero = "output/review/model/R_event_count_day_zero_output.csv")
  )
)

## combine everything ----
project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)

#####################################################################################
## convert list to yaml, reformat comments and white space, and output a .yaml file #
#####################################################################################
as.yaml(project_list, indent=2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1") %>%
  writeLines("project.yaml")
