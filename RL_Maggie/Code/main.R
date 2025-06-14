## main.R ----------------------------------------------------------------
## Run the entire analysis pipeline in order
source("./refined_r_code/00_setup.R")

## Optional – set to TRUE if you need fresh copies from the shared drive
RUN_COPY <- FALSE
if (RUN_COPY) source("./refined_r_code/01_copy_raw_files.R")

## Trial-level -----------------------------------------------------------
source("./refined_r_code/02_trial_etl_fibro.R")
source("./refined_r_code/03_trial_etl_stress.R")
source("./refined_r_code/04_trial_merge_clean.R")
source("./refined_r_code/05_trial_analysis.R")          # produces plots and ANOVA output

## Estimation-level ------------------------------------------------------
source("./refined_r_code/06_estimation_etl.R")
source("./refined_r_code/07_estimation_analysis.R")      # plots & ANOVA

## Questionnaire analysis (Fibro only) ----------------------------------
source("./refined_r_code/08_questionnaire_analysis.R")
