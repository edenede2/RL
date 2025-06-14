import pandas as pd
from pathlib import Path
import importlib

# Import setup module
setup = importlib.import_module('00_setup')
LOCAL_DATA_DIR = setup.LOCAL_DATA_DIR
STRESS_DATA_DIR = setup.STRESS_DATA_DIR
ESTIMATION_DATA_DIR = setup.ESTIMATION_DATA_DIR
REPORTS_DIR = setup.REPORTS_DIR
save_to_csv = setup.save_to_csv
save_to_json = setup.save_to_json

# Import trial data
merge_mod = importlib.import_module('04_trial_merge_clean')
full_trial_data = merge_mod.full_trial_data
all_non_learners = merge_mod.all_non_learners


def process_single_csv_file_for_estimation_data(file_path: Path) -> pd.DataFrame:
    """Process a single CSV file to extract estimation data"""
    try:
        sub_df = pd.read_csv(file_path)
        if 'estimation_rating.response' not in sub_df.columns:
            print(f"No estimation_rating.response column in {file_path}")
            return pd.DataFrame()
        sub_df = sub_df[sub_df['estimation_rating.response'].notna()]
        if sub_df.empty:
            print(f"No valid estimation responses in {file_path}")
            return pd.DataFrame()

        # Check if we have the image_file column
        if 'image_file' not in sub_df.columns:
            print(f"No image_file column in {file_path}")
            return pd.DataFrame()

        df = pd.DataFrame({
            'participant': sub_df['participant'],
            'image_file': sub_df['image_file'],
            'estimation_response': sub_df['estimation_rating.response']
        })
        df['block'] = (df.index // 4) + 1
        df.loc[df['estimation_response'] == 'None', 'estimation_response'] = '50'

        if df['block'].max() == 7:
            df = df[df['block'] != 1]
            df['block'] -= 1
        
        # Save individual participant data
        if not df.empty:
            participant_id = df['participant'].iloc[0]
            save_to_csv(df, f'estimation_raw_{participant_id}.csv', ESTIMATION_DATA_DIR)
            
        return df
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return pd.DataFrame()


def get_estimation_data_from_all_csvs(data_dir: Path = LOCAL_DATA_DIR) -> pd.DataFrame:
    files = list(data_dir.glob('sub_*.csv'))
    frames = [process_single_csv_file_for_estimation_data(f) for f in files if f.stat().st_size > 0]
    return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()


def create_estimation_data(data_dir: Path = STRESS_DATA_DIR) -> pd.DataFrame:
    files = list(data_dir.glob('s_*.csv'))
    df_list = []
    for file in files:
        if file.stat().st_size == 0:
            print(f"Empty file: {file}")
            continue
        sub_df = pd.read_csv(file)
        if sub_df.empty:
            print(f"Empty DataFrame for file: {file}")
            continue
            
        # Check if required columns exist
        required_columns = ['trial_num', 'pair_type', 'low_prob_image_file', 'high_prob_image_file']
        missing_columns = [col for col in required_columns if col not in sub_df.columns]
        if missing_columns:
            print(f"File {file} is missing columns: {missing_columns}")
            continue
            
        # Process only if we have the required data
        sub_df_filtered = sub_df.dropna(subset=['trial_num'])
        if 'pair_type' not in sub_df_filtered.columns:
            print(f"Missing pair_type column in {file}")
            continue
            
        # Get reversed trials or skip if none
        reversed_trials = sub_df_filtered[sub_df_filtered['pair_type'] == 'reversed']
        if reversed_trials.empty or 'low_prob_image_file' not in reversed_trials.columns:
            print(f"No reversed trials or missing low_prob_image_file in {file}")
            continue
            
        low_prob_rev = reversed_trials['low_prob_image_file']
        if low_prob_rev.empty:
            print(f"No low probability reversed images in {file}")
            continue
            
        # Get non-reversed trials or skip if none
        non_reversed_trials = sub_df_filtered[sub_df_filtered['pair_type'] == 'non-reversed']
        if non_reversed_trials.empty:
            print(f"No non-reversed trials in {file}")
            continue
            
        # Extract image choices safely
        try:
            choice_3 = low_prob_rev.iloc[-1]
            choice_4 = low_prob_rev.iloc[0]
            choice_2 = non_reversed_trials['low_prob_image_file'].unique()[0]
            choice_1 = non_reversed_trials['high_prob_image_file'].unique()[0]
        except (IndexError, KeyError) as e:
            print(f"Error extracting image choices from {file}: {e}")
            continue

        sub_df = sub_df.dropna(subset=['estimation_exp1.png'])
        sub_df = sub_df[['participant', 'estimation_exp1.png', 'estimation_exp2.png', 'estimation_exp3.png', 'estimation_exp4.png']]
        sub_df['block'] = range(1, len(sub_df) + 1)
        if sub_df['block'].max() == 7:
            sub_df = sub_df[sub_df['block'] > 1]
            sub_df['block'] -= 1

        sub_df.columns = ['participant', choice_1, choice_2, choice_3, choice_4, 'block']
        sub_df = sub_df.melt(id_vars=['participant', 'block'], var_name='choice', value_name='prob_estimation')
        df_list.append(sub_df)
    return pd.concat(df_list, ignore_index=True) if df_list else pd.DataFrame()


# Initialize default empty dataframes
full_estimation_data = pd.DataFrame(columns=['participant', 'block', 'estimation_response', 'pair_type', 'is_high_probe', 'group'])
full_estimation_data_clean = pd.DataFrame(columns=['participant', 'block', 'estimation_response', 'pair_type', 'is_high_probe', 'group'])

try:
    # Get estimation data for fibro/healthy participants
    estimation_data_df_fibro_healthy = get_estimation_data_from_all_csvs()

    # Check if required columns exist in the full_trial_data
    required_columns = ['participant', 'block', 'pair_type', 'high_prob_image_file', 'low_prob_image_file']
    missing_columns = [col for col in required_columns if col not in full_trial_data.columns]
    
    if missing_columns:
        print(f"Warning: full_trial_data is missing columns: {missing_columns}")
    else:
        # Filter to ensure we only include columns that exist
        image_per_block_description = full_trial_data.drop_duplicates(
            subset=['participant', 'block', 'pair_type'] + 
            [col for col in ['high_prob_image_file', 'low_prob_image_file'] if col in full_trial_data.columns]
        )

        # Only proceed with merge if we have data
        if not estimation_data_df_fibro_healthy.empty and 'image_file' in estimation_data_df_fibro_healthy.columns:
            # Perform first merge only if high_prob_image_file exists
            if 'high_prob_image_file' in image_per_block_description.columns:
                estimation_with_choices_high = estimation_data_df_fibro_healthy.merge(
                    image_per_block_description,
                    left_on=['participant', 'block', 'image_file'],
                    right_on=['participant', 'block', 'high_prob_image_file'],
                    how='left',
                    suffixes=('', '_high')
                )
            else:
                estimation_with_choices_high = estimation_data_df_fibro_healthy.copy()
                estimation_with_choices_high['pair_type'] = None

            # Perform second merge only if low_prob_image_file exists
            if 'low_prob_image_file' in image_per_block_description.columns:
                estimation_with_choices_fibro_healthy = estimation_with_choices_high.merge(
                    image_per_block_description,
                    left_on=['participant', 'block', 'image_file'],
                    right_on=['participant', 'block', 'low_prob_image_file'],
                    how='left',
                    suffixes=('_high', '_low')
                )
            else:
                estimation_with_choices_fibro_healthy = estimation_with_choices_high.copy()
                estimation_with_choices_fibro_healthy['pair_type_low'] = None

            # Process estimation data further
            if 'pair_type_high' in estimation_with_choices_fibro_healthy.columns and 'pair_type_low' in estimation_with_choices_fibro_healthy.columns:
                estimation_with_choices_fibro_healthy['pair_type'] = estimation_with_choices_fibro_healthy['pair_type_high'].combine_first(estimation_with_choices_fibro_healthy['pair_type_low'])
                
                if 'high_prob_image_file' in estimation_with_choices_fibro_healthy.columns:
                    estimation_with_choices_fibro_healthy['is_high_probe'] = estimation_with_choices_fibro_healthy['high_prob_image_file'].isna()
                else:
                    estimation_with_choices_fibro_healthy['is_high_probe'] = False
                
                estimation_with_choices_fibro_healthy['estimation_response'] = pd.to_numeric(estimation_with_choices_fibro_healthy['estimation_response'])
                
                # Add group information
                if 'group' in full_trial_data.columns:
                    group_info = full_trial_data[['participant', 'group']].drop_duplicates()
                    estimation_with_choices_fibro_healthy = estimation_with_choices_fibro_healthy.merge(
                        group_info,
                        on='participant',
                        how='left'
                    )
                else:
                    estimation_with_choices_fibro_healthy['group'] = 'Unknown'
                
                # Get stress estimation data
                stress_estimation_df = create_estimation_data()
                
                if not stress_estimation_df.empty and 'choice' in stress_estimation_df.columns:
                    estimation_with_choices_stress = stress_estimation_df.assign(
                        pair_type=lambda d: d['choice'].isin(['choice_1', 'choice_2']).map({True: 'non-reversed', False: 'reversed'}),
                        is_high_probe=lambda d: d['choice'].isin(['choice_1', 'choice_3']),
                        estimation_response=lambda d: pd.to_numeric(d['prob_estimation']),
                        group='Healthy Stress'
                    )[['participant', 'block', 'estimation_response', 'pair_type', 'is_high_probe', 'group']]
                    
                    # Combine datasets
                    full_estimation_data = pd.concat([
                        estimation_with_choices_fibro_healthy[['participant', 'block', 'estimation_response', 'pair_type', 'is_high_probe', 'group']],
                        estimation_with_choices_stress
                    ], ignore_index=True)
                    
                    # Clean the data
                    participant_variability = full_estimation_data.groupby('participant')['estimation_response'].std().reset_index(name='sd_estimation')
                    low_var_participants = participant_variability[participant_variability['sd_estimation'] < 1]['participant'].tolist()
                    full_estimation_data_clean = full_estimation_data[~full_estimation_data['participant'].isin(low_var_participants + all_non_learners)]
                    
                    # Save full and cleaned estimation data
                    save_to_csv(full_estimation_data, 'full_estimation_data.csv', ESTIMATION_DATA_DIR)
                    save_to_csv(full_estimation_data_clean, 'full_estimation_data_clean.csv', ESTIMATION_DATA_DIR)
                    
                    # Save participant variability data
                    save_to_csv(participant_variability, 'participant_estimation_variability.csv', ESTIMATION_DATA_DIR)
                    
                    # Save low variability participants list
                    save_to_json({'low_var_participants': low_var_participants}, 'low_var_participants.json')
                else:
                    print("Warning: No valid stress estimation data")
                    full_estimation_data = estimation_with_choices_fibro_healthy[['participant', 'block', 'estimation_response', 'pair_type', 'is_high_probe', 'group']]
                    
                    # Clean the data
                    participant_variability = full_estimation_data.groupby('participant')['estimation_response'].std().reset_index(name='sd_estimation')
                    low_var_participants = participant_variability[participant_variability['sd_estimation'] < 1]['participant'].tolist()
                    full_estimation_data_clean = full_estimation_data[~full_estimation_data['participant'].isin(low_var_participants + all_non_learners)]
                    
                    # Save data with fibro/healthy only
                    save_to_csv(full_estimation_data, 'full_estimation_data_fibro_healthy_only.csv', ESTIMATION_DATA_DIR)
                    save_to_csv(full_estimation_data_clean, 'full_estimation_data_clean_fibro_healthy_only.csv', ESTIMATION_DATA_DIR)
                    save_to_csv(participant_variability, 'participant_estimation_variability_fibro_healthy_only.csv', ESTIMATION_DATA_DIR)
            else:
                print("Warning: Missing pair_type columns in merged data")
        else:
            print("Warning: No valid estimation data for fibro/healthy participants")
            
except Exception as e:
    print(f"Error in estimation data processing: {e}")
    full_estimation_data = pd.DataFrame()
    full_estimation_data_clean = pd.DataFrame()
    
# Print summary
print(f"Full estimation data: {len(full_estimation_data) if 'full_estimation_data' in locals() else 0} rows")
print(f"Clean estimation data: {len(full_estimation_data_clean) if 'full_estimation_data_clean' in locals() else 0} rows")

# Save summary information
estimation_summary = {
    "full_estimation_data_rows": len(full_estimation_data) if 'full_estimation_data' in locals() else 0,
    "clean_estimation_data_rows": len(full_estimation_data_clean) if 'full_estimation_data_clean' in locals() else 0,
    "participants_removed_low_var": len(low_var_participants) if 'low_var_participants' in locals() else 0,
    "non_learners_removed": len(all_non_learners)
}
save_to_json(estimation_summary, 'estimation_summary.json', REPORTS_DIR)
