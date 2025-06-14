## 00_setup.R ------------------------------------------------------------
## Load libraries and define global options / paths

# Function to check and install missing packages
check_and_install_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages) > 0) {
    message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    # Try to install binary packages first with type="binary"
    tryCatch({
      install.packages(new_packages, repos = "https://cloud.r-project.org/", type="binary")
    }, error = function(e) {
      message("Error installing binary packages: ", e$message)
      message("Trying to install from source...")
      tryCatch({
        install.packages(new_packages, repos = "https://cloud.r-project.org/")
      }, error = function(e2) {
        message("Could not install packages: ", e2$message)
        message("Please manually install: ", paste(new_packages, collapse = ", "))
      })
    })
  }
}

# Safer library loading function
safe_library <- function(package_name) {
  tryCatch({
    # Try to load the package
    library(package_name, character.only = TRUE)
  }, error = function(e) {
    # If the package can't be loaded, print a helpful message but continue
    message("Warning: Could not load package '", package_name, "': ", e$message)
    message("Some functionality may be limited.")
  })
}

# List of required packages
required_packages <- c("dplyr", "stringr", "fs", "ggplot2", "tidyr", 
                       "tidyverse", "readxl", "emmeans", "here")

# Check and install missing packages
check_and_install_packages(required_packages)

# Load all packages with error handling
suppressPackageStartupMessages({
  for (pkg in required_packages) {
    safe_library(pkg)
  }
})

## root-relative paths ---------------------------------------------------
RAW_SHARED_DIR   <- "G:/Shared drives/AdmonPsy - Fibro/Experiment/Data"
LOCAL_DATA_DIR   <- here("data")          # project/data
STRESS_DATA_DIR  <- here("data_healthy_Noa")
QUESTIONNAIRE_CSV <- here("merged_data_220125 - Maggie.csv")
