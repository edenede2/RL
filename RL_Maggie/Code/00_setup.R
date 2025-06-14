## 00_setup.R ------------------------------------------------------------
## Load libraries and define global options / paths

# Function to check and install missing packages
check_and_install_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages) > 0) {
    message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages, repos = "https://cloud.r-project.org/")
  }
}

# List of required packages
required_packages <- c("dplyr", "stringr", "fs", "ggplot2", "tidyr", 
                       "tidyverse", "readxl", "emmeans", "here")

# Check and install missing packages
check_and_install_packages(required_packages)

# Load all packages
suppressPackageStartupMessages({
  library(dplyr);   library(stringr);   library(fs)
  library(ggplot2); library(tidyr);     library(tidyverse)
  library(readxl);  library(emmeans);   library(here)
})

## root-relative paths ---------------------------------------------------
RAW_SHARED_DIR   <- "G:/Shared drives/AdmonPsy - Fibro/Experiment/Data"
LOCAL_DATA_DIR   <- here("data")          # project/data
STRESS_DATA_DIR  <- here("data_healthy_Noa")
QUESTIONNAIRE_CSV <- here("merged_data_220125 - Maggie.csv")
