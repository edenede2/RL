## ------------------------------------------------------------------
## 04b_flex_label.R  – Derive “flex” vs “non-flex” label
## ------------------------------------------------------------------
## Prerequisites:
##   • full_trial_data_learners   (from 04_trial_merge_clean.R)
##   • correct column is 1/0      (already enforced earlier)
## ------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)

# 1. accuracy profile: 6 blocks (1–6) × 2 pair types  --------------------
acc_profile <- full_trial_data_learners %>%
  filter(block %in% 1:6) %>%                     # discard extra learning trials (if any)
  group_by(participant, pair_type, block) %>%
  summarise(acc = mean(correct, na.rm = TRUE), .groups = "drop") %>%
  unite(cell, pair_type, block, sep = "_") %>%   # e.g. "reversed_4"
  pivot_wider(names_from = cell, values_from = acc)

# 2. person-mean centre ---------------------------------------------------
acc_mat          <- as.matrix(acc_profile[ , -1])              # strip participant id
row_means        <- rowMeans(acc_mat, na.rm = TRUE)
acc_mat_centred  <- sweep(acc_mat, 1, row_means)               # subtract

# 3. k-means (k = 2) ------------------------------------------------------
set.seed(2025)
km <- kmeans(acc_mat_centred, centers = 2, nstart = 50)

# 4. decide which cluster is “flex” --------------------------------------
#   Calculate mean (reversed – non-reversed) accuracy over blocks 4–6
gap_by_cluster <- sapply(1:2, function(cl){
  idx <- km$cluster == cl
  rev_cols  <- str_subset(colnames(acc_mat_centred), "^reversed_[4-6]$")
  non_cols  <- str_subset(colnames(acc_mat_centred), "^non-reversed_[4-6]$")
  mean(acc_mat_centred[idx , rev_cols] - acc_mat_centred[idx , non_cols], na.rm = TRUE)
})

flex_cluster <- which.max(gap_by_cluster)        # larger gap = more flexible

# 5. add label back to your master data frame ----------------------------
flex_labels <- acc_profile %>%
  mutate(flex_label = ifelse(km$cluster == flex_cluster, "flex", "non-flex")) %>%
  select(participant, flex_label)

# You can now merge with full_trial_data_learners, e.g.:
full_trial_data_learners <- full_trial_data_learners %>%
  left_join(flex_labels, by = "participant")

View(full_trial_data_learners)
# ------------------------------------------------------------------
# RESULT:  full_trial_data_learners now contains a column `flex_label`
#          (values: "flex" / "non-flex") exactly like the paper’s.
# ------------------------------------------------------------------
