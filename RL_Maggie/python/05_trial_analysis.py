import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.stats.anova import AnovaRM
import importlib

merge_mod = importlib.import_module('RL_Maggie.python.04_trial_merge_clean')
full_trial_data_learners = merge_mod.full_trial_data_learners

anova_data = (full_trial_data_learners
              .groupby(['participant', 'block', 'pair_type', 'group'])
              ['choice_a'].mean().reset_index())

anova = AnovaRM(anova_data, 'choice_a', 'participant', within=['block', 'pair_type', 'group'])
print(anova.fit())

summary_df = (full_trial_data_learners
              .groupby(['block', 'pair_type', 'group'])
              ['choice_a'].mean().reset_index())

for key, grp_df in summary_df.groupby('group'):
    plt.figure()
    for pair_type, d in grp_df.groupby('pair_type'):
        plt.plot(d['block'], d['choice_a'], label=pair_type)
    plt.title(f'Group {key}')
    plt.xlabel('Block')
    plt.ylabel('Mean Choice A')
    plt.legend()
    plt.savefig(f'choice_plot_{key}.png')
