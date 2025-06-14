## 04_trial_merge_clean.R -----------------------------------------------
trial_data_healthy_fibro  <- result_healthy_fibro$trial_data
trial_data_healthy_stress <- result_healthy_stress$trial_data

full_trial_data <- bind_rows(trial_data_healthy_fibro,
                             trial_data_healthy_stress) %>% 
  filter(if_all(everything(), ~ !is.na(.)),
         participant != "")

all_non_learners <- c(result_healthy_fibro$non_learners,
                      result_healthy_stress$non_learners)

full_trial_data_learners <- full_trial_data %>% 
  filter(!(participant %in% all_non_learners))
