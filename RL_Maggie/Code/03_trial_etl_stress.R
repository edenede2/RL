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
  # Check if df has data and required columns
  if (nrow(df) > 0) {
    # Print column names for debugging
    print(paste("Columns in df:", paste(colnames(df), collapse = ", ")))
    
    # Check if 'correct' column exists before processing
    if ("correct" %in% colnames(df)) {
      df <- df %>% 
        # Convert "correct"/"incorrect" to 1/0 if it's a character
        mutate(
          correct = ifelse(is.character(correct) & correct == "correct", 1,
                   ifelse(is.character(correct) & correct == "incorrect", 0, 
                   ifelse(is.numeric(correct), correct, NA_real_))),
          group = "Healthy Stress"
        )
    } else {
      # If 'correct' column doesn't exist, add it with NA values
      message("Warning: 'correct' column not found. Creating a default column with NA values.")
      df$correct <- NA_real_
      df$group <- "Healthy Stress"
    }
  } else {
    message("Warning: No data in df")
  }
    ## Non-learners ---------------------------------------------------------
  non_learners <- character()
  if (nrow(df) > 0 && !all(is.na(df$correct))) {
    # Only process if we have data and valid 'correct' values
    non_learners_df <- tryCatch({
      df %>% 
        filter(participant %in% unique(participants_with_7), block == 3) %>% 
        group_by(participant, pair_type) %>% 
        summarise(pct = mean(correct, na.rm = TRUE) * 100, .groups = "drop") %>% 
        pivot_wider(names_from = pair_type, values_from = pct)
    }, error = function(e) {
      message("Error calculating non-learners: ", e$message)
      return(data.frame(participant = character(0)))
    })
    
    if (nrow(non_learners_df) > 0) {
      # Check if both columns exist before filtering
      if ("reversed" %in% colnames(non_learners_df) && "non-reversed" %in% colnames(non_learners_df)) {
        non_learners <- non_learners_df %>% 
          filter(reversed < 70 | `non-reversed` < 70) %>% 
          pull(participant)
      } else {
        message("Warning: Missing required columns in non_learners_df")
      }
    } else {
      message("No data available for non-learners calculation")
    }
  } else {
    message("No valid 'correct' data for non-learners calculation")
  }
  
  list(trial_data = df,
       participants_with_7_blocks = unique(participants_with_7),
       non_learners = non_learners)
}

result_healthy_stress <- get_trial_data_healthy_stress()
