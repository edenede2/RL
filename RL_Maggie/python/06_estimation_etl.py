import pandas as pd
from pathlib import Path
import importlib

setup = importlib.import_module('00_setup')
LOCAL_DATA_DIR = setup.LOCAL_DATA_DIR
STRESS_DATA_DIR = setup.STRESS_DATA_DIR
merge_mod = importlib.import_module('04_trial_merge_clean')
full_trial_data = merge_mod.full_trial_data
all_non_learners = merge_mod.all_non_learners


def process_single_csv_file_for_estimation_data(file_path: Path) -> pd.DataFrame:
    sub_df = pd.read_csv(file_path)
    if 'estimation_rating.response' not in sub_df.columns:
        return pd.DataFrame()
    sub_df = sub_df[sub_df['estimation_rating.response'].notna()]
    if sub_df.empty:
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
    return df


def get_estimation_data_from_all_csvs(data_dir: Path = LOCAL_DATA_DIR) -> pd.DataFrame:
    files = list(data_dir.glob('sub_*.csv'))
    frames = [process_single_csv_file_for_estimation_data(f) for f in files if f.stat().st_size > 0]
    return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()


def create_estimation_data(data_dir: Path = STRESS_DATA_DIR) -> pd.DataFrame:
    files = list(data_dir.glob('s_*.csv'))
    df_list = []
    for file in files:
        if file.stat().st_size == 0:
            continue
        sub_df = pd.read_csv(file)
        if sub_df.empty:
            continue
        low_prob_rev = sub_df.dropna(subset=['trial_num']).query("pair_type == 'reversed'")['low_prob_image_file']
        if low_prob_rev.empty:
            continue
        choice_3 = low_prob_rev.iloc[-1]
        choice_4 = low_prob_rev.iloc[0]
        choice_2 = sub_df.dropna(subset=['trial_num']).query("pair_type == 'non-reversed'")['low_prob_image_file'].unique()[0]
        choice_1 = sub_df.dropna(subset=['trial_num']).query("pair_type == 'non-reversed'")['high_prob_image_file'].unique()[0]

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


estimation_data_df_fibro_healthy = get_estimation_data_from_all_csvs()
image_per_block_description = full_trial_data.drop_duplicates(subset=['participant', 'block', 'pair_type', 'high_prob_image_file', 'low_prob_image_file'])

estimation_with_choices_fibro_healthy = estimation_data_df_fibro_healthy.merge(
    image_per_block_description,
    left_on=['participant', 'block', 'image_file'],
    right_on=['participant', 'block', 'high_prob_image_file'],
    how='left'
).merge(
    image_per_block_description,
    left_on=['participant', 'block', 'image_file'],
    right_on=['participant', 'block', 'low_prob_image_file'],
    how='left',
    suffixes=('_high', '_low')
)

estimation_with_choices_fibro_healthy['pair_type'] = estimation_with_choices_fibro_healthy['pair_type_high'].combine_first(estimation_with_choices_fibro_healthy['pair_type_low'])

estimation_with_choices_fibro_healthy['is_high_probe'] = estimation_with_choices_fibro_healthy['high_prob_image_file'].isna()

estimation_with_choices_fibro_healthy['estimation_response'] = pd.to_numeric(estimation_with_choices_fibro_healthy['estimation_response'])

estimation_with_choices_fibro_healthy = estimation_with_choices_fibro_healthy.merge(
    full_trial_data[['participant', 'group']].drop_duplicates(),
    on='participant'
)

stress_estimation_df = create_estimation_data()

estimation_with_choices_stress = stress_estimation_df.assign(
    pair_type=lambda d: d['choice'].isin(['choice_1', 'choice_2']).map({True: 'non-reversed', False: 'reversed'}),
    is_high_probe=lambda d: d['choice'].isin(['choice_1', 'choice_3']),
    estimation_response=lambda d: pd.to_numeric(d['prob_estimation']),
    group='Healthy Stress'
)[['participant', 'block', 'estimation_response', 'pair_type', 'is_high_probe', 'group']]

full_estimation_data = pd.concat([
    estimation_with_choices_fibro_healthy[['participant', 'block', 'estimation_response', 'pair_type', 'is_high_probe', 'group']],
    estimation_with_choices_stress
], ignore_index=True)

participant_variability = full_estimation_data.groupby('participant')['estimation_response'].std().reset_index(name='sd_estimation')
low_var_participants = participant_variability[participant_variability['sd_estimation'] < 1]['participant'].tolist()

full_estimation_data_clean = full_estimation_data[~full_estimation_data['participant'].isin(low_var_participants + all_non_learners)]
