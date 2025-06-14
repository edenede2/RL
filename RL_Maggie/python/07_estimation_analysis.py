import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.stats.anova import AnovaRM
import importlib

etl_mod = importlib.import_module('06_estimation_etl')
full_estimation_data = etl_mod.full_estimation_data
full_estimation_data_clean = etl_mod.full_estimation_data_clean

high_vs_low_df = (full_estimation_data_clean
    .groupby(['group', 'participant', 'block', 'pair_type', 'is_high_probe'])
    ['estimation_response'].mean().unstack('is_high_probe').reset_index())

# Convert boolean column names to strings for safer access
if True in high_vs_low_df.columns and False in high_vs_low_df.columns:
    high_vs_low_df = high_vs_low_df.rename(columns={True: 'high_probe', False: 'low_probe'})
    high_vs_low_df['choice_diff'] = high_vs_low_df['high_probe'] - high_vs_low_df['low_probe']
else:
    # Create a default column if the boolean columns don't exist
    print("Warning: Missing high/low probe columns in estimation data")
    high_vs_low_df['choice_diff'] = 0

summary_df = (high_vs_low_df
    .groupby(['group', 'block', 'pair_type'])['choice_diff']
    .agg(['mean', 'sem']).reset_index())

for (grp, pair_type), d in summary_df.groupby(['group', 'pair_type']):
    plt.figure()
    plt.errorbar(d['block'], d['mean'], yerr=d['sem'], label=pair_type)
    plt.axhline(0, ls='--')
    plt.title(f'{grp} - {pair_type}')
    plt.xlabel('Block')
    plt.ylabel('Mean Estimation Δ (high − low)')
    plt.legend()
    plt.savefig(f'estimation_plot_{grp}_{pair_type}.png')

anova_data = (full_estimation_data
    .groupby(['participant', 'block', 'pair_type', 'group', 'is_high_probe'])
    ['estimation_response'].mean().unstack('is_high_probe').reset_index())

# Convert boolean column names to strings for safer access
if True in anova_data.columns and False in anova_data.columns:
    anova_data = anova_data.rename(columns={True: 'high_probe', False: 'low_probe'})
    anova_data['estimation_diff'] = anova_data['high_probe'] - anova_data['low_probe']
    
    # Only run ANOVA if we have sufficient data
    if len(anova_data) > 5:
        try:
            anova = AnovaRM(anova_data, 'estimation_diff', 'participant', within=['block', 'pair_type', 'group'])
            print(anova.fit())
        except Exception as e:
            print(f"Error running ANOVA: {e}")
            print("This may be due to insufficient data or unbalanced design.")
    else:
        print("Insufficient data for ANOVA analysis")
else:
    print("Warning: Missing high/low probe columns in estimation data for ANOVA")
