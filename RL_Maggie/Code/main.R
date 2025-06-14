## main.R ----------------------------------------------------------------
## Run the entire analysis pipeline in order
source("RL_Maggie/Code/00_setup.R")

## Optional – set to TRUE if you need fresh copies from the shared drive
RUN_COPY <- FALSE
if (RUN_COPY) source("RL_Maggie/Code/01_copy_raw_files.R")

## Trial-level -----------------------------------------------------------
source("RL_Maggie/Code/02_trial_etl_fibro.R")
source("RL_Maggie/Code/03_trial_etl_stress.R")
source("RL_Maggie/Code/04_trial_merge_clean.R")
source("RL_Maggie/Code/05_trial_analysis.R")          # produces plots and ANOVA output

## Estimation-level ------------------------------------------------------
source("RL_Maggie/Code/06_estimation_etl.R")
source("RL_Maggie/Code/07_estimation_analysis.R")      # plots & ANOVA

## Questionnaire analysis (Fibro only) ----------------------------------
tryCatch({
  source("RL_Maggie/Code/08_questionnaire_analysis.R")
}, error = function(e) {
  message("Questionnaire analysis failed: ", e$message)
  message("This is likely due to a missing file. Check that ", QUESTIONNAIRE_CSV, " exists.")
})
