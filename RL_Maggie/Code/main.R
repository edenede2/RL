## main.R ----------------------------------------------------------------
## Run the entire analysis pipeline in order

# Check if required packages are available
missing_packages <- c()
for (pkg in c("dplyr", "stringr", "tidyr", "emmeans", "here")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  message("WARNING: The following required packages are missing: ", paste(missing_packages, collapse=", "))
  message("Please run the install_packages.R script first:")
  message("Rscript RL_Maggie/Code/install_packages.R")
  message("\nContinuing with available packages...")
}

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
