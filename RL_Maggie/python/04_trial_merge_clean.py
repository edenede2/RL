import pandas as pd
import importlib

# Import modules
setup_mod = importlib.import_module('00_setup')
fibro_mod = importlib.import_module('02_trial_etl_fibro')
stress_mod = importlib.import_module('03_trial_etl_stress')

# Setup export functions
TRIAL_DATA_DIR = setup_mod.TRIAL_DATA_DIR
save_to_csv = setup_mod.save_to_csv
save_to_json = setup_mod.save_to_json

# Get trial data for both groups
result_healthy_fibro = fibro_mod.get_trial_data_healthy_fibro()
result_healthy_stress = stress_mod.get_trial_data_healthy_stress()

trial_data_healthy_fibro = result_healthy_fibro['trial_data']
trial_data_healthy_stress = result_healthy_stress['trial_data']

# Merge trial data
full_trial_data = pd.concat([trial_data_healthy_fibro, trial_data_healthy_stress], ignore_index=True)
full_trial_data = full_trial_data.dropna().query("participant != ''")

# Save merged trial data
save_to_csv(full_trial_data, 'full_trial_data.csv', TRIAL_DATA_DIR)

# Identify non-learners
all_non_learners = result_healthy_fibro['non_learners'] + result_healthy_stress['non_learners']
save_to_json({'all_non_learners': all_non_learners}, 'all_non_learners.json')

# Create learners-only dataset
full_trial_data_learners = full_trial_data[~full_trial_data['participant'].isin(all_non_learners)]
save_to_csv(full_trial_data_learners, 'full_trial_data_learners.csv', TRIAL_DATA_DIR)

# Export participant information
participant_summary = {
    'total_participants': len(full_trial_data['participant'].unique()),
    'healthy_participants': len(result_healthy_fibro['healthy_participants']),
    'fibro_participants': len(result_healthy_fibro['fibro_participants']),
    'stress_participants': len(result_healthy_stress['participants_with_7_blocks']),
    'total_non_learners': len(all_non_learners),
    'learners': len(full_trial_data_learners['participant'].unique())
}
save_to_json(participant_summary, 'participant_summary.json')
