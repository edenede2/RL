from pathlib import Path
import os
import json

# Set up root relative paths similar to the R script
PROJECT_ROOT = Path(__file__).resolve().parents[1]
RAW_SHARED_DIR = Path('G:/Shared drives/AdmonPsy - Fibro/Experiment/Data')

# Detect the correct data directory ("Data" or "data")
for cand in [PROJECT_ROOT / 'Data', PROJECT_ROOT / 'data']:
    if cand.exists():
        LOCAL_DATA_DIR = cand
        break
else:
    LOCAL_DATA_DIR = PROJECT_ROOT / 'Data'

STRESS_DATA_DIR = PROJECT_ROOT / 'data_healthy_Noa'
QUESTIONNAIRE_CSV = PROJECT_ROOT / 'merged_data_220125 - Maggie.csv'

# Add output directory for CSV exports
OUTPUT_DIR = PROJECT_ROOT / 'python_output'
OUTPUT_DIR.mkdir(exist_ok=True)

# Create subdirectories for different stages of the pipeline
TRIAL_DATA_DIR = OUTPUT_DIR / 'trial_data'
TRIAL_DATA_DIR.mkdir(exist_ok=True)

ESTIMATION_DATA_DIR = OUTPUT_DIR / 'estimation_data'
ESTIMATION_DATA_DIR.mkdir(exist_ok=True)

ANALYSIS_RESULTS_DIR = OUTPUT_DIR / 'analysis_results'
ANALYSIS_RESULTS_DIR.mkdir(exist_ok=True)

REPORTS_DIR = OUTPUT_DIR / 'reports'
REPORTS_DIR.mkdir(exist_ok=True)

LOCAL_DATA_DIR.mkdir(exist_ok=True)

# Helper function to save DataFrame to CSV
def save_to_csv(df, filename, directory=OUTPUT_DIR):
    """Save a DataFrame to CSV in the specified directory"""
    if df is None or df.empty:
        print(f"Warning: Empty DataFrame, not saving {filename}")
        return
    
    filepath = directory / filename
    df.to_csv(filepath, index=False)
    print(f"Saved: {filepath}")
    
# Helper function to save dictionary to JSON
def save_to_json(data, filename, directory=REPORTS_DIR):
    """Save a dictionary to JSON in the specified directory"""
    if not data:
        print(f"Warning: Empty data, not saving {filename}")
        return
    
    filepath = directory / filename
    with open(filepath, 'w') as f:
        json.dump(data, f, indent=2, default=str)
    print(f"Saved: {filepath}")
