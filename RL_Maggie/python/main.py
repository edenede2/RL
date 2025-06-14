import importlib

importlib.import_module('00_setup')
RUN_COPY = False
if RUN_COPY:
    importlib.import_module('01_copy_raw_files')

for mod in [
    '02_trial_etl_fibro',
    '03_trial_etl_stress',
    '04_trial_merge_clean',
    '05_trial_analysis',
    '06_estimation_etl',
    '07_estimation_analysis',
    '08_questionnaire_analysis']:
    importlib.import_module(f'{mod}')
