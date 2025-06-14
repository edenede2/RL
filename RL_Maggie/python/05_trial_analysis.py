import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.stats.anova import AnovaRM
import importlib
import numpy as np
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm
import json

# Import setup module
setup_mod = importlib.import_module('00_setup')
merge_mod = importlib.import_module('04_trial_merge_clean')

# Get paths and functions
ANALYSIS_RESULTS_DIR = setup_mod.ANALYSIS_RESULTS_DIR
REPORTS_DIR = setup_mod.REPORTS_DIR
save_to_csv = setup_mod.save_to_csv
save_to_json = setup_mod.save_to_json

# Get trial data
full_trial_data_learners = merge_mod.full_trial_data_learners

# Check data balance before running ANOVA
anova_data = (full_trial_data_learners
              .groupby(['participant', 'block', 'pair_type', 'group'])
              ['choice_a'].mean().reset_index())

# Save ANOVA input data
save_to_csv(anova_data, 'anova_input_data.csv', ANALYSIS_RESULTS_DIR)

# Collect data structure information
data_structure = {
    "number_of_participants": anova_data['participant'].nunique(),
    "number_of_blocks": anova_data['block'].nunique(),
    "number_of_pair_types": anova_data['pair_type'].nunique(),
    "number_of_groups": anova_data['group'].nunique()
}

# Check if data is balanced
counts = anova_data.groupby(['participant']).size()
data_structure["is_balanced"] = counts.min() == counts.max()
data_structure["min_observations_per_participant"] = int(counts.min())
data_structure["max_observations_per_participant"] = int(counts.max())

# Print information about the data structure
print("Data structure check:")
print(f"Number of participants: {data_structure['number_of_participants']}")
print(f"Number of blocks: {data_structure['number_of_blocks']}")
print(f"Number of pair types: {data_structure['number_of_pair_types']}")
print(f"Number of groups: {data_structure['number_of_groups']}")
print(f"Is data balanced? {data_structure['is_balanced']}")
print(f"Min observations per participant: {data_structure['min_observations_per_participant']}")
print(f"Max observations per participant: {data_structure['max_observations_per_participant']}")

# Save data structure information
save_to_json(data_structure, 'data_structure.json', REPORTS_DIR)

# Print participants with fewest observations
if not data_structure["is_balanced"]:
    min_obs = counts.min()
    print("Participants with fewest observations:")
    min_obs_participants = counts[counts == min_obs]
    print(min_obs_participants)
    
    # Save to CSV
    min_obs_df = pd.DataFrame({
        'participant': min_obs_participants.index,
        'observation_count': min_obs_participants.values
    })
    save_to_csv(min_obs_df, 'participants_with_min_observations.csv', REPORTS_DIR)

# Run appropriate analysis based on data balance
anova_results = {}
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
        rm_anova_result = anova.fit()
        print(rm_anova_result)
        
        # Save RM-ANOVA results
        anova_results['rm_anova'] = {
            'within_factors': within_factors,
            'result': rm_anova_result.summary().as_csv()
        }
        
        # Also save as CSV
        anova_table_df = pd.read_csv(pd.StringIO(rm_anova_result.summary().as_csv()))
        save_to_csv(anova_table_df, 'rm_anova_results.csv', ANALYSIS_RESULTS_DIR)
    else:
        print("No suitable within-subjects factors found for RM-ANOVA")
        anova_results['rm_anova'] = {
            'error': "No suitable within-subjects factors found"
        }
except ValueError as e:
    print(f"RM-ANOVA failed: {e}")
    anova_results['rm_anova'] = {
        'error': str(e)
    }
    
    # Fallback to standard ANOVA
    print("\nFalling back to standard ANOVA:")
    formula = 'choice_a ~ C(block) + C(pair_type) + C(group)'
    model = ols(formula, data=anova_data).fit()
    anova_table = anova_lm(model)
    print(anova_table)
    
    # Save standard ANOVA results
    anova_results['standard_anova'] = {
        'formula': formula,
        'result': anova_table.to_csv()
    }
    
    # Also save as CSV
    anova_table_df = pd.DataFrame(anova_table)
    save_to_csv(anova_table_df, 'standard_anova_results.csv', ANALYSIS_RESULTS_DIR)

# Save all ANOVA results
save_to_json(anova_results, 'anova_results.json', REPORTS_DIR)

# Create summary dataframe for plotting
summary_df = (full_trial_data_learners
              .groupby(['block', 'pair_type', 'group'])
              ['choice_a'].mean().reset_index())

# Save summary data
save_to_csv(summary_df, 'choice_summary_by_group.csv', ANALYSIS_RESULTS_DIR)

# Create plots
for key, grp_df in summary_df.groupby('group'):
    plt.figure()
    for pair_type, d in grp_df.groupby('pair_type'):
        plt.plot(d['block'], d['choice_a'], label=pair_type)
    plt.title(f'Group {key}')
    plt.xlabel('Block')
    plt.ylabel('Mean Choice A')
    plt.legend()
    plt.savefig(f'{ANALYSIS_RESULTS_DIR}/choice_plot_{key}.png')
    print(f"Saved plot: choice_plot_{key}.png")
