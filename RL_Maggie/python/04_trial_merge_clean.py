import pandas as pd
import importlib

fibro_mod = importlib.import_module('RL_Maggie.python.02_trial_etl_fibro')
stress_mod = importlib.import_module('RL_Maggie.python.03_trial_etl_stress')

result_healthy_fibro = fibro_mod.get_trial_data_healthy_fibro()
result_healthy_stress = stress_mod.get_trial_data_healthy_stress()

trial_data_healthy_fibro = result_healthy_fibro['trial_data']
trial_data_healthy_stress = result_healthy_stress['trial_data']

full_trial_data = pd.concat([trial_data_healthy_fibro, trial_data_healthy_stress], ignore_index=True)
full_trial_data = full_trial_data.dropna().query("participant != ''")

all_non_learners = result_healthy_fibro['non_learners'] + result_healthy_stress['non_learners']
full_trial_data_learners = full_trial_data[~full_trial_data['participant'].isin(all_non_learners)]
