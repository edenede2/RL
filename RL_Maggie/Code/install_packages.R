## install_packages.R ----------------------------------------------------
## Run this script once to install all required packages for the analysis
## This script will try different methods to install packages

# List all packages needed for the analysis
packages_to_install <- c(
  # Main packages
  "dplyr", "stringr", "fs", "ggplot2", "tidyr", "tidyverse", 
  "readxl", "here",
  
  # Statistics packages
  "emmeans", 
  
  # Dependencies for emmeans
  "estimability", "mvtnorm", "numDeriv"
)

# Try to install pre-compiled binary packages
message("Attempting to install binary packages...")
install_result <- try(
  install.packages(
    packages_to_install, 
    repos = c("https://cloud.r-project.org/", "https://mac.r-project.org/bin/macosx/contrib/4.1/"),
    type = "binary"
  ),
  silent = TRUE
)

# Check which packages were successfully installed
installed <- packages_to_install[packages_to_install %in% installed.packages()[,"Package"]]
missing <- setdiff(packages_to_install, installed)

if (length(missing) > 0) {
  message("\nThe following packages could not be installed as binaries: ", paste(missing, collapse=", "))
  message("\nAttempting to install from source...")
  
  # Try to install from source one by one
  for (pkg in missing) {
    message("Installing ", pkg, "...")
    try(install.packages(pkg, repos = "https://cloud.r-project.org/"), silent = TRUE)
  }
}

# Final check
final_installed <- packages_to_install[packages_to_install %in% installed.packages()[,"Package"]]
final_missing <- setdiff(packages_to_install, final_installed)

message("\n=== Installation Summary ===")
message("Successfully installed: ", paste(final_installed, collapse=", "))
if (length(final_missing) > 0) {
  message("Failed to install: ", paste(final_missing, collapse=", "))
  message("\nFor macOS with Anaconda, the following may work to install mvtnorm:")
  message("  1. Run in Terminal: conda install -c conda-forge r-mvtnorm")
  message("  2. Then rerun this script to install the remaining packages")
  message("\nAlternatively, you can install manually using:")
  message("  - R CMD INSTALL for source packages")
  message("  - Downloading pre-compiled binaries from CRAN")
  message("  - Using alternative repositories")
} else {
  message("\nAll packages successfully installed!")
}
