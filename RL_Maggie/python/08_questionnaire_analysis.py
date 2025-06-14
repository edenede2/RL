import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  # noqa: F401
import importlib

setup = importlib.import_module('RL_Maggie.python.00_setup')
QUESTIONNAIRE_CSV = setup.QUESTIONNAIRE_CSV
merge_mod = importlib.import_module('RL_Maggie.python.04_trial_merge_clean')
full_trial_data = merge_mod.full_trial_data

fibro_questionnaire_df = pd.read_csv(QUESTIONNAIRE_CSV)
rl_scores_df = (full_trial_data[full_trial_data['block'] == 6]
                [full_trial_data['correct'] == 1]
                .groupby('participant').size().reset_index(name='rl_score'))

fibro_merged = fibro_questionnaire_df.merge(rl_scores_df, on='participant', how='left')

lm_full = pd.concat([fibro_merged['rl_score'], fibro_merged[['Age', 'PCS_Total', 'FIQR_Total', 'PIPS_TOTAL']]], axis=1).dropna().corr()
lm_basic = pd.concat([fibro_merged['rl_score'], fibro_merged[['PIPS_TOTAL', 'FIQR_Total']]], axis=1).dropna().corr()
print(lm_full)
print(lm_basic)

plt.figure()
plt.scatter(fibro_merged['PIPS_TOTAL'], fibro_merged['FIQR_Total'], c=fibro_merged['rl_score'])
plt.title('Scatter Plot of PIPS_TOTAL vs FIQR_Total')
plt.xlabel('PIPS Total')
plt.ylabel('FIQR Total')
plt.colorbar(label='RL Score')
plt.savefig('questionnaire_scatter.png')

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(fibro_merged['PIPS_TOTAL'], fibro_merged['FIQR_Total'], fibro_merged['rl_score'])
ax.set_xlabel('PIPS Total')
ax.set_ylabel('FIQR Total')
ax.set_zlabel('RL Score')
plt.title('3D Scatter Plot of PIPS_TOTAL, FIQR_Total, and RL Score')
plt.savefig('questionnaire_3dscatter.png')
