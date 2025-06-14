import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.stats.anova import AnovaRM
import importlib
import json

# Import setup module for directories and helper functions
setup_mod = importlib.import_module('00_setup')
ANALYSIS_RESULTS_DIR = setup_mod.ANALYSIS_RESULTS_DIR
REPORTS_DIR = setup_mod.REPORTS_DIR
save_to_csv = setup_mod.save_to_csv
save_to_json = setup_mod.save_to_json

# Import estimation data
etl_mod = importlib.import_module('06_estimation_etl')
full_estimation_data = etl_mod.full_estimation_data
full_estimation_data_clean = etl_mod.full_estimation_data_clean

# Process the high vs low differences
high_vs_low_df = (full_estimation_data_clean
    .groupby(['group', 'participant', 'block', 'pair_type', 'is_high_probe'])
    ['estimation_response'].mean().unstack('is_high_probe').reset_index())

# Save the processed data
save_to_csv(high_vs_low_df, 'high_vs_low_df_raw.csv', ANALYSIS_RESULTS_DIR)

# Convert boolean column names to strings for safer access
if True in high_vs_low_df.columns and False in high_vs_low_df.columns:
    high_vs_low_df = high_vs_low_df.rename(columns={True: 'high_probe', False: 'low_probe'})
    high_vs_low_df['choice_diff'] = high_vs_low_df['high_probe'] - high_vs_low_df['low_probe']
else:
    # Create a default column if the boolean columns don't exist
    print("Warning: Missing high/low probe columns in estimation data")
    high_vs_low_df['choice_diff'] = 0

# Save the processed data with renamed columns
save_to_csv(high_vs_low_df, 'high_vs_low_df_processed.csv', ANALYSIS_RESULTS_DIR)

# Create summary dataframe
summary_df = (high_vs_low_df
    .groupby(['group', 'block', 'pair_type'])['choice_diff']
    .agg(['mean', 'sem']).reset_index())

# Save summary data
save_to_csv(summary_df, 'estimation_summary_by_group.csv', ANALYSIS_RESULTS_DIR)

# Create plots for each group and pair type
for (grp, pair_type), d in summary_df.groupby(['group', 'pair_type']):
    plt.figure()
    plt.errorbar(d['block'], d['mean'], yerr=d['sem'], label=pair_type)
    plt.axhline(0, ls='--')
    plt.title(f'{grp} - {pair_type}')
    plt.xlabel('Block')
    plt.ylabel('Mean Estimation Δ (high − low)')
    plt.legend()
    plt.savefig(f'{ANALYSIS_RESULTS_DIR}/estimation_plot_{grp}_{pair_type}.png')
    print(f"Saved plot: estimation_plot_{grp}_{pair_type}.png")

# Prepare data for ANOVA
anova_data = (full_estimation_data_clean
    .groupby(['participant', 'block', 'pair_type', 'group', 'is_high_probe'])
    ['estimation_response'].mean().unstack('is_high_probe').reset_index())

# Save ANOVA input data
save_to_csv(anova_data, 'estimation_anova_input.csv', ANALYSIS_RESULTS_DIR)

# Convert boolean column names to strings for safer access
if True in anova_data.columns and False in anova_data.columns:
    anova_data = anova_data.rename(columns={True: 'high_probe', False: 'low_probe'})
    anova_data['estimation_diff'] = anova_data['high_probe'] - anova_data['low_probe']
    
    # Save processed ANOVA data
    save_to_csv(anova_data, 'estimation_anova_processed.csv', ANALYSIS_RESULTS_DIR)
    
    # Only run ANOVA if we have sufficient data
    anova_results = {}
    if len(anova_data) > 5:
        try:
            # Try to run a repeated measures ANOVA
            print("\nAttempting Repeated Measures ANOVA on estimation data:")
            within_factors = []
            for factor in ['block', 'pair_type']:
                if anova_data.groupby('participant')[factor].nunique().min() > 1:
                    within_factors.append(factor)
            
            if len(within_factors) > 0:
                anova = AnovaRM(anova_data, 'estimation_diff', 'participant', within=within_factors)
                anova_result = anova.fit()
                print(anova_result)
                
                # Save RM-ANOVA results
                anova_results['rm_anova'] = {
                    'within_factors': within_factors,
                    'result': anova_result.summary().as_csv()
                }
                
                # Save as CSV
                anova_table_df = pd.read_csv(pd.StringIO(anova_result.summary().as_csv()))
                save_to_csv(anova_table_df, 'estimation_rm_anova_results.csv', ANALYSIS_RESULTS_DIR)
            else:
                print("No suitable within-subjects factors found for RM-ANOVA")
                anova_results['rm_anova'] = {
                    'error': "No suitable within-subjects factors found"
                }
                
        except Exception as e:
            print(f"Error running ANOVA: {e}")
            print("This may be due to insufficient data or unbalanced design.")
            anova_results['error'] = str(e)
            
        # Save ANOVA results
        save_to_json(anova_results, 'estimation_anova_results.json', REPORTS_DIR)
    else:
        print("Insufficient data for ANOVA analysis")
        save_to_json({'error': 'Insufficient data for ANOVA analysis'}, 
                     'estimation_anova_results.json', REPORTS_DIR)
else:
    print("Warning: Missing high/low probe columns in estimation data for ANOVA")
    save_to_json({'error': 'Missing high/low probe columns in estimation data'}, 
                 'estimation_anova_results.json', REPORTS_DIR)
