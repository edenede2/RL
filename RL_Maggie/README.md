# RL Analysis Pipeline

This repository contains the R code for analyzing reinforcement learning data from multiple studies.

## Setup and Installation

Before running the analysis, you need to install the required R packages. The easiest way to do this is by running the installation script:

```bash
cd /path/to/your/project
Rscript RL_Maggie/Code/install_packages.R
```

### Package Installation Issues

If you encounter issues installing packages (especially on macOS with Anaconda), try these steps:

1. For `mvtnorm` package issues on macOS with Anaconda:
   ```bash
   conda install -c conda-forge r-mvtnorm
   ```

2. For other packages, you can install them individually:
   ```bash
   R -e "install.packages('packageName', repos='https://cloud.r-project.org/')"
   ```

## Running the Analysis

After installing the required packages, run the main analysis script:

```bash
cd /path/to/your/project
Rscript RL_Maggie/Code/main.R
```

## Missing Files and Paths

The analysis requires specific data files. If you encounter errors about missing files, make sure:

1. The `data` directory contains all participant data files (sub_*.csv)
2. The `data_healthy_Noa` directory contains all stress group data files (s_*.csv)
3. For questionnaire analysis, the file `merged_data_220125 - Maggie.csv` should be in the project root directory

## Error Handling

The scripts include robust error handling to work even if some data files are missing or if certain packages are not available. Error messages should provide guidance on what's missing or how to fix issues.

## Key Output

The analysis produces various outputs including:
- ANOVA results for trial data
- ANOVA results for estimation data
- Plots of learning curves and estimation differences
