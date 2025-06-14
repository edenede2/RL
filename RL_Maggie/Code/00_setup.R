## 00_setup.R ------------------------------------------------------------
## Load libraries and define global options / paths
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
