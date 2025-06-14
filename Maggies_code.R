#this section create a file with all RL data, sub_901 doesn't have RL data the file is empty needs to understand way
#create the RL path with all participant data:----
#install.packages("fs")
library(fs)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)

source_dir <- "G:\\Shared drives\\AdmonPsy - Fibro\\Experiment\\Data"
dest_dir <- "./Data"


# Ensure the destination directory exists
dir_create(dest_dir)

copy_files_from_rl <- function(source_dir, dest_dir) {
  # Get a list of all subdirectories in the source directory
  subdirs <- dir_ls(source_dir, recurse = TRUE, type = "directory")
  
  # Filter subdirectories to include only those containing 'RL'
  rl_subdirs <- subdirs[str_detect(subdirs, "/sub_\\d+/RL$")]
  
  # Iterate over each 'RL' subdirectory
  for (subdir in rl_subdirs) {
    # List all files in the current 'RL' subdirectory
    files <- dir_ls(subdir, type = "file")
    
    # Filter files that start with 'sub_'
    matching_files <- files[str_detect(path_file(files), "^sub_\\d+_Reversal_.*\\.csv$")]
    
    # Copy each matching file to the destination directory
    for (file in matching_files) {
      file_copy(file, path(dest_dir, path_file(file)), overwrite = TRUE)
    }
  }
}

# Call the function to copy the files
# copy_files_from_rl(source_dir, dest_dir) # This might be commented because already copied




# create data - aggregate sub_files (output of the RL task)----

get_trial_data_from_csvs <- function(data_dir) {
  files <- list.files(data_dir, pattern = "^sub_.*\\.csv$", full.names = TRUE) # Ensure only CSV files are selected
  nsubj <- length(files)
  
  df <- data.frame()
  
  for (i in 1:length(files)) {
    # Check if the file is not empty
    if (file.info(files[i])$size > 0) {
      # Read sub data file
      sub_df <- read.csv(files[i])
      
      # Filter only rows that contain trials (i.e. omit blank rows or rows with no relevant info)
      sub_df <- sub_df %>% filter(!is.na(trial_num))
      
      # Check if the filtered dataframe is not empty
      if (nrow(sub_df) > 0) {
        
        # Select relevant columns + add "block" variable (block = 20 trials)
        sub_df <- sub_df %>%
          select(trial_num, participant, pair_type, choice_a, reward, correct, high_prob_image_file, low_prob_image_file) %>%
          # mutate(block = rep(1:(nrow(sub_df) / 20), each = 20))
          mutate(block = rep(1:ceiling(nrow(sub_df) / 20), each = 20)[1:nrow(sub_df)])
        # Note to self: 7 blocks handeling 
        
        # Remove block 1 if there are exactly 7 blocks
        if (max(unique(sub_df$block)) == 7) {
          sub_df <- sub_df %>%
            filter(block != 1) %>%
            mutate(block = block - 1) %>%
            mutate(trial_num = trial_num - 20)
        }
        
        df <- rbind(df, sub_df)
      } else {
        message(paste("Filtered dataframe is empty for file:", files[i]))
      }
    } else {
      message(paste("File is empty:", files[i]))
    }
  }
  
  df$correct <- ifelse(df$correct == "correct", 1, 0)
  return(df)
  
}

trial_data_df <- get_trial_data_from_csvs(data_dir = dest_dir)

#until here only loading the trial part of the CSVs


# Summarize correct answers per block per participant
# Plot the summary
trial_data_summary_df <- trial_data_df %>%
  group_by(participant,pair_type, block) %>%
  summarize(choice_a_sum = sum(choice_a, na.rm = TRUE)) %>% 
  filter(participant != "")


plot_sum <- ggplot(trial_data_summary_df, 
               aes(x = block, y = choice_a_sum, color = pair_type)) +
  geom_line() +  # Add lines
  geom_point() +  # Add markers
  labs(title = "",
       x = "Block", y = "Choice A Sum") +
  scale_color_manual(values = c("blue", "red"), labels = c("Non-reversed", "Reversed")) +  # Customize colors and labels
  theme_minimal() +
  facet_wrap(~ participant)  # Facet by participant


plot_sum


# Get estimation data

process_single_csv_file_for_estimation_data <- function(file_path) {
  # Read the CSV file
  sub_df <- read.csv(file_path)
  
  # If the relevant column exists, proceed with processing
  if ("estimation_rating.response" %in% colnames(sub_df)) {
    if (nrow(sub_df) > 0) {
      # Filter relevant columns, rename, and add block variable
      sub_df <- sub_df %>%
        filter(!is.na(`estimation_rating.response`)) %>%
        rename(estimation_response = `estimation_rating.response`) %>%
        select(participant, image_file, estimation_response) %>%
        mutate(block = rep(1:ceiling(nrow(.) / 4), each = 4)[1:nrow(.)])  # Repeat each number 4 times
    }
  }
  else {
    return(data.frame())
  }
  
  return(sub_df)
}



get_estimation_data_from_all_csvs <- function(data_dir) {
  files <- list.files(data_dir, pattern = "^sub_.*\\.csv$", full.names = TRUE)  # Get all CSV files in directory
  nsubj <- length(files)
  
  df <- data.frame()
  
  for (i in 1:nsubj) {  # Loop over each file
    # Check if the file is not empty
    if (file.info(files[i])$size > 0) {
      # Read sub data file
      sub_df <- process_single_csv_file_for_estimation_data(files[i])  # Corrected to use the file path
      if (nrow(sub_df) > 0) {
        df <- rbind(df, sub_df)  
      }
      # Combine the data into the final dataframe
      
    } else {
      message(paste("Filtered dataframe is empty for file:", files[i]))
    }
  }
  
  return(df)
}

estimation_data_df <- get_estimation_data_from_all_csvs(data_dir)


image_per_block_description <- trial_data_df %>%
  group_by(pair_type, block, participant) %>%
  summarize(
    high_prob_image_file = first(high_prob_image_file),
    low_prob_image_file = first(low_prob_image_file),
    .groups = "drop"
  )

estimation_with_choices <- estimation_data_df %>%
  left_join(image_per_block_description, by = c("participant", "block", "image_file" = "high_prob_image_file")) %>%
  left_join(image_per_block_description, by = c("participant", "block", "image_file" = "low_prob_image_file")) %>%
  group_by(participant, image_file, estimation_response, block) %>%
  summarise(
    # Use coalesce to get the first non-null pair_type
    pair_type = coalesce(first(pair_type.x), first(pair_type.y)),
    
    # Use coalesce to determine if it's a high probe
    is_high_probe = ifelse(is.na(first(high_prob_image_file)), TRUE, FALSE),
    
    .groups = "drop"  # To avoid nesting of groups
  )

plot_estimations_per_block_facet_by_participant <- function(estimation_with_choices) {
  estimation_with_choices %>% 
    # Filtering out NAs in estimation_response and ensuring data is ordered correctly
    filter(!is.na(pair_type)) %>%
    filter(is_high_probe) %>%
    filter(!is.na(estimation_response)) %>%  # Remove rows where estimation_response is NA
    mutate(block = as.numeric(block), estimation_response = as.numeric(estimation_response)) %>%
    arrange(block) %>%
    
    ggplot(aes(x = block, y = estimation_response, color = pair_type)) +
    geom_line() +  # Add lines
    geom_point() +  # Add markers
    geom_hline(yintercept = 80, linetype = "dashed", color = "black") +  # Add a dotted line at y = 80
    labs(title = "",
         x = "Block", y = "Participant high image probability estimation in %") +
    scale_color_manual(values = c("blue", "red"), labels = c("Non-reversed", "Reversed")) +  # Customize colors and labels
    theme_minimal() +
    facet_wrap(~ participant) +  # Facet by participant
    ylim(0, 100)  # Ensure the y-axis range includes 80
}

#estimation_with_choices %>%
 # plot_estimations_per_block_facet_by_participant()
  

# Sample specific participants in order to have a good time
plot_estimation <- estimation_with_choices %>% 
  filter(participant == "sub_954") %>% 
  plot_estimations_per_block_facet_by_participant()

print(plot_estimation)




# Basic measures aggregations


aggregated_measures <- trial_data_df %>%
  filter(pair_type %in% c("reversed", "non-reversed")) %>% # There were 9 rows with NA in the pair type. Need to investigate - na participant
  group_by(participant, pair_type, block) %>%
  summarize(choice_a_sum = sum(choice_a, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = c(pair_type, block), values_from = choice_a_sum) %>%
  mutate(
    
    # Correct answers sum for non-reversed minus correct answers sum for reversed, at the end of the learning learning phase
    pair_delta_learning_phase = `non-reversed_3` - `reversed_3`,  
    # Correct answers sum for non-reversed minus correct answers sum for reversed, at the end of the reversal learning phase
    pair_delta_reversal_phase = `non-reversed_6` - `reversed_6`,
    # Correct answers sum for non-reversed minus correct answers sum for reversed, througth the whole learning phase (blocks 1-3)
    delta_learning_phase_above_pair_type =  (`non-reversed_1` + `non-reversed_2` + `non-reversed_3`) - 
      (`reversed_1` + `reversed_2` + `reversed_3`),
    # Correct answers sum for non-reversed minus correct answers sum for reversed, througth the whole reversal phase (blocks 4-6)
    delta_reversal_phase_above_pair_type =  (`non-reversed_4` + `non-reversed_5` + `non-reversed_6`) - 
      (`reversed_4` + `reversed_5` + `reversed_6`),
    # Correct sum through the whole learning phase (blocks 1-3, Reversed and non-reversed)
    total_correct_learning_phase = `non-reversed_1` + `non-reversed_2` + `non-reversed_3`+
      `reversed_1` + `reversed_2` + `reversed_3`,
    # Correct sum through the whole reversal phase (blocks 4-6, Reversed and non-reversed)
    total_correct_reversal_phase = `non-reversed_4` + `non-reversed_5` + `non-reversed_6` +
      `reversed_4` + `reversed_5` + `reversed_6`,
    # Correct sum in the reversed pair at the end of the learning phase minus at the end of the reversal phase
    delta_reversed_pair_end_of_phases =  `reversed_3` -  `reversed_6`,
    # Correct sum in the non-reversed pair at the end of the learning phase minus at the end of the reversal phase
    delta_non_reversed_pair_end_of_phases =  `non-reversed_3` -  `non-reversed_6`,
    # Correct sum within the switch point to the reversal phase, within the reversed pair
    delta_reversed_pair_within_switch_point =  `reversed_3` -  `reversed_4`,
    # Correct sum within the switch point to the reversal phase, within the non-reversed pair
    delta_non_reversed_pair_within_switch_point =  `non-reversed_3` -  `non-reversed_4`,
    # Correct sum within the switch point to the reversal phase, within the reversed pair
    delta_non_reversed_pair_within_switch_point =  `reversed_3` -  `reversed_4`,
    # Correct sum in the end of the reversal phase in comparison to the begining of the reversal phase, in the non reversed pair.
    # If positive - the pariticipant succeeded in understaing that there was a change withing the reversed pair.
    delta_reversed_pair_throughout_reversal_phase  = `reversed_6` -  `reversed_4`,
    # Correct sum in the end of the reversal phase in comparison to the begining of the reversal phase, in the non reversed pair.
    # If positive - the pariticipant succeeded in understanding that the change was only in the reversed pair
    delta_non_reversed_pair_throughout_reversal_phase  = `non-reversed_6` -  `non-reversed_4`,
    # Number of total correct answers per participant
    total_correct_per_participant = total_correct_reversal_phase + total_correct_learning_phase
    
    
    
  )
aggregated_measures_narrow <- aggregated_measures[, c(1, 14:ncol(aggregated_measures))]

# Print the table
print(aggregated_measures_narrow)







