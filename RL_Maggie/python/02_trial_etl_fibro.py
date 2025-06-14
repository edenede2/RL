import pandas as pd
from pathlib import Path
from typing import List, Dict
from importlib import import_module

setup = import_module('RL_Maggie.python.00_setup')
LOCAL_DATA_DIR = setup.LOCAL_DATA_DIR


def get_trial_data_healthy_fibro(data_dir: Path = LOCAL_DATA_DIR) -> Dict[str, pd.DataFrame]:
    files = list(data_dir.glob('sub_*.csv'))
    df_list = []
    participants_with_7: List[str] = []

    for f in files:
        if f.stat().st_size == 0:
            print(f"Empty file: {f}")
            continue
        sub_df = pd.read_csv(f)
        sub_df = sub_df.dropna(subset=['trial_num'])
        if sub_df.empty:
            print(f"No trials in: {f}")
            continue

        sub_df = sub_df[['trial_num', 'participant', 'pair_type', 'choice_a', 'reward',
                         'correct', 'high_prob_image_file', 'low_prob_image_file']].copy()
        sub_df['block'] = (sub_df.index // 20) + 1

        cur_p = (sub_df.groupby('participant')['block'].max() == 7)
        participants_with_7.extend(sub_df.loc[cur_p[sub_df['participant']].index, 'participant'].unique())

        if sub_df['block'].max() == 7:
            sub_df = sub_df[sub_df['block'] != 1]
            sub_df['block'] -= 1
            sub_df['trial_num'] -= 20
        df_list.append(sub_df)

    if not df_list:
        return {'trial_data': pd.DataFrame(),
                'participants_with_7_blocks': [],
                'healthy_participants': [],
                'fibro_participants': [],
                'non_learners': []}
    df = pd.concat(df_list, ignore_index=True)
    df['correct'] = (df['correct'] == 'correct').astype(int)
    participants_with_7 = list(set(participants_with_7))

    if participants_with_7:
        non_learners = (df[df['participant'].isin(participants_with_7) & (df['block'] == 3)]
                        .groupby(['participant', 'pair_type'])['correct']
                        .mean().reset_index()
                        .pivot(index='participant', columns='pair_type', values='correct')
                        .reset_index())
        non_learners = non_learners[(non_learners.get('reversed', 0) < 0.7) |
                                    (non_learners.get('non-reversed', 0) < 0.7)]['participant'].tolist()
    else:
        non_learners = []

    p_nums = pd.to_numeric(df['participant'].str.replace('sub_', ''), errors='coerce')
    healthy = df.loc[p_nums.between(900, 1499), 'participant'].unique().tolist()
    fibro = [p for p in df['participant'].unique() if p not in healthy]

    df['group'] = df['participant'].apply(
        lambda p: 'Healthy' if p in healthy else ('Fibro' if p in fibro else None)
    )

    return {
        'trial_data': df,
        'participants_with_7_blocks': participants_with_7,
        'healthy_participants': healthy,
        'fibro_participants': fibro,
        'non_learners': non_learners
    }


if __name__ == '__main__':
    result_healthy_fibro = get_trial_data_healthy_fibro()
