## 03_trial_etl_stress.R -------------------------------------------------
get_trial_data_healthy_stress <- function(data_dir = STRESS_DATA_DIR) {
  files <- list.files(data_dir, "^s_.*\\.csv$", full.names = TRUE)
  df <- tibble();   participants_with_7 <- character()
  
  for (f in files) {
    if (file.info(f)$size == 0) { message("Empty file: ", f); next }
    
    sub_df <- read.csv(f) %>% filter(!is.na(trial_num))
    if (nrow(sub_df) == 0) { message("No trials in: ", f); next }
    
    sub_df <- sub_df %>% 
      select(trial_num, participant, pair_type, choice_a, reward,
             correct, high_prob_image_file, low_prob_image_file) %>% 
      mutate(block = rep(1:ceiling(n()/20), each = 20)[1:n()])
    
    cur_p <- sub_df %>% group_by(participant) %>% 
      summarise(total_blocks = max(block), .groups = "drop") %>% 
      filter(total_blocks == 7) %>% pull(participant)
    participants_with_7 <- c(participants_with_7, cur_p)
    
    if (max(sub_df$block) == 7) {
      sub_df <- sub_df %>% filter(block != 1) %>% 
        mutate(block = block - 1, trial_num = trial_num - 20)
    }
    df <- bind_rows(df, sub_df)
  }
  
  df <- df %>% mutate(correct = if_else(correct == "correct", 1, 0),
                      group   = "Healthy Stress")
  
  ## Non-learners ---------------------------------------------------------
  non_learners <- df %>% 
    filter(participant %in% unique(participants_with_7), block == 3) %>% 
    group_by(participant, pair_type) %>% 
    summarise(pct = mean(correct) * 100, .groups = "drop") %>% 
    pivot_wider(names_from = pair_type, values_from = pct) %>% 
    filter(reversed   < 70 | `non-reversed` < 70) %>% pull(participant)
  
  list(trial_data = df,
       participants_with_7_blocks = unique(participants_with_7),
       non_learners = non_learners)
}

result_healthy_stress <- get_trial_data_healthy_stress()
