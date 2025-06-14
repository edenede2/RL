import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.stats.anova import AnovaRM
import importlib
import numpy as np
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm

merge_mod = importlib.import_module('04_trial_merge_clean')
full_trial_data_learners = merge_mod.full_trial_data_learners

# Check data balance before running ANOVA
anova_data = (full_trial_data_learners
              .groupby(['participant', 'block', 'pair_type', 'group'])
              ['choice_a'].mean().reset_index())

# Print information about the data structure
print("Data structure check:")
print(f"Number of participants: {anova_data['participant'].nunique()}")
print(f"Number of blocks: {anova_data['block'].nunique()}")
print(f"Number of pair types: {anova_data['pair_type'].nunique()}")
print(f"Number of groups: {anova_data['group'].nunique()}")

# Check if data is balanced
counts = anova_data.groupby(['participant']).size()
is_balanced = counts.min() == counts.max()
print(f"Is data balanced? {is_balanced}")
print(f"Min observations per participant: {counts.min()}")
print(f"Max observations per participant: {counts.max()}")

# Print participants with fewest observations
if not is_balanced:
    min_obs = counts.min()
    print("Participants with fewest observations:")
    print(counts[counts == min_obs])

# Run appropriate analysis based on data balance
try:
    # Try running the repeated measures ANOVA first
    print("\nAttempting Repeated Measures ANOVA:")
    # Only include within factors that exist for all participants
    within_factors = []
    for factor in ['block', 'pair_type']:
        if anova_data.groupby('participant')[factor].nunique().min() > 1:
            within_factors.append(factor)
    
    if len(within_factors) > 0:
        anova = AnovaRM(anova_data, 'choice_a', 'participant', within=within_factors)
        print(anova.fit())
    else:
        print("No suitable within-subjects factors found for RM-ANOVA")
except ValueError as e:
    print(f"RM-ANOVA failed: {e}")
    
    # Fallback to standard ANOVA
    print("\nFalling back to standard ANOVA:")
    formula = 'choice_a ~ C(block) + C(pair_type) + C(group)'
    model = ols(formula, data=anova_data).fit()
    anova_table = anova_lm(model)
    print(anova_table)

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
