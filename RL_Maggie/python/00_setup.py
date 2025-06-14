from pathlib import Path

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

LOCAL_DATA_DIR.mkdir(exist_ok=True)
