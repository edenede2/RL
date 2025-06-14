import pandas as pd
from sklearn.cluster import KMeans
import importlib

merge_mod = importlib.import_module('04_trial_merge_clean')
full_trial_data_learners = merge_mod.full_trial_data_learners

acc_profile = (full_trial_data_learners[full_trial_data_learners['block'].between(1, 6)]
               .groupby(['participant', 'pair_type', 'block'])['correct']
               .mean().reset_index()
               .assign(cell=lambda d: d['pair_type'] + '_' + d['block'].astype(str))
               .pivot(index='participant', columns='cell', values='correct'))

acc_mat = acc_profile.fillna(acc_profile.mean()).to_numpy()
row_means = acc_mat.mean(axis=1, keepdims=True)
acc_mat_centred = acc_mat - row_means

km = KMeans(n_clusters=2, n_init=50, random_state=2025).fit(acc_mat_centred)

rev_cols = [c for c in acc_profile.columns if c.startswith('reversed_') and c.split('_')[1] in ['4', '5', '6']]
non_cols = [c for c in acc_profile.columns if c.startswith('non-reversed_') and c.split('_')[1] in ['4', '5', '6']]

gap_by_cluster = []
for cl in range(2):
    idx = km.labels_ == cl
    gap_by_cluster.append((acc_mat_centred[idx][:, [acc_profile.columns.get_loc(c) for c in rev_cols]].mean() -
                           acc_mat_centred[idx][:, [acc_profile.columns.get_loc(c) for c in non_cols]].mean()))
flex_cluster = int(pd.Series(gap_by_cluster).idxmax())

flex_labels = pd.DataFrame({
    'participant': acc_profile.index,
    'flex_label': ['flex' if lab == flex_cluster else 'non-flex' for lab in km.labels_]
})

full_trial_data_learners = full_trial_data_learners.merge(flex_labels, on='participant', how='left')
