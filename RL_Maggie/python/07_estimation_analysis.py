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

high_vs_low_df['choice_diff'] = high_vs_low_df[True] - high_vs_low_df[False]

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

anova_data['estimation_diff'] = anova_data[True] - anova_data[False]

anova = AnovaRM(anova_data, 'estimation_diff', 'participant', within=['block', 'pair_type', 'group'])
print(anova.fit())
