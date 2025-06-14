rm(list= ls())
"yellow"
#this section create a file with all RL data, sub_901 doesn't have RL data the file is empty needs to understand way
#create the RL path with all participant data:----
#install.packages("fs")
library(fs)
library(dplyr)
library(stringr)

source_dir <- "G:/Shared drives/AdmonPsy - Fibro/Experiment/Data"
dest_dir <- "G:/Shared drives/AdmonPsy - Fibro/RL/Data"

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
copy_files_from_rl(source_dir, dest_dir)







# create data - aggregate sub_files (output of the RL task)----
library(dplyr)

create_data <- function(data_dir = '../data/') {
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
          select(trial_num, participant, pair_type, choice_a, reward, correct) %>%
         # mutate(block = rep(1:(nrow(sub_df) / 20), each = 20))
          mutate(block = rep(1:ceiling(nrow(sub_df) / 20), each = 20)[1:nrow(sub_df)])
        
        
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

dest_dir <- "G:/Shared drives/AdmonPsy - Fibro/RL/Data" # Update this to your actual data directory
#create_data(data_dir = dest_dir)

df <- create_data(data_dir = dest_dir)

library(ggplot2)
#ggplot(df) +
#  geom_line(aes(block, choice_a, color = pair_type)) +
#  facet_wrap(~participant)+
#  geom_line()+
#  geom_point()+
#  theme_classic() 

# Plot using ggplot2
library(ggplot2)
df_1 <- df %>%
  group_by(participant, pair_type) %>%
  mutate(cumulative_choice_a = cumsum(choice_a))
df_1 <- df %>%
  group_by(participant, pair_type) %>%
  mutate(cumulative_choice_a_percentile = cumsum(percent_rank(choice_a)))
df_1 <- df %>%
  group_by(participant, pair_type) %>%
  mutate(percentage_choice_a = cumsum(choice_a == 1) / row_number() * 100)
df_1 <- df %>%
  group_by(participant, pair_type,block) %>%
  mutate(cumulative_choice_a = cumsum(choice_a == 1))

df_summary <- df %>%
  group_by(participant,pair_type, block) %>%
  summarize(choice_a_sum = sum(choice_a, na.rm = TRUE)) #%>%
  #pivot_wider(names_from = block, values_from = choice_a_sum, names_prefix = "block_")

df_summary <- df_summary %>%
  filter(participant != "")

  ggplot(df_summary, aes(x = block, y = choice_a_sum, color = pair_type)) +
  geom_line() +
  labs(title = "",
       x = "Block", y = "Choice A Sum") +
  scale_color_manual(values = c("blue", "red"), labels = c("Non-reversed", "Reversed")) +  # Customize colors and labels
  theme_minimal() +
  facet_wrap(~ participant)  # Facet by participant with free y-axis scales

"yellow"
#Maggie continue from here
#Another func to create measures:
create_measures = function(data_dir = "G:/Shared drives/AdmonPsy - Fibro/RL/Data"){
  library(labelled) # to give SPSS style labeling to the data 
  files = list.files(path = data_dir,pattern = "^sub_", full.names = TRUE)
  nsubj = length(files)
  
  df = data.frame()
  
  for(i in 1:length(files)){
    sub_df = read.csv(files[i]) 
    extra_block = na.omit(sub_df$extra_block) 
    sub_df <- sub_df %>% filter(!is.na(trial_num)) 
    sub_df <- sub_df %>%
      select(trial_num, participant,pair_type,  choice_a, correct) %>%
      mutate(block = rep(1:(nrow(sub_df)/20), each = 20))
    
    sub_df<- sub_df%>% group_by(block, pair_type)%>% dplyr::summarise(correct  = sum(correct == "correct"), 
                                                               choice_a  = sum(choice_a), 
                                                               extra_block = mean(extra_block), 
                                                               participant =  unique(participant)) %>% ungroup()
    # if a subject preformed extra block, omit it and recode the block variable 
    #if(sub_df$extra_block == 1){
    if(extra_block == 1){
      sub_df = sub_df %>% filter(block != 1)
      sub_df$block = dplyr::recode(sub_df$block, `2`= 1, `3` =2, `4` =3, `5` =4, `6` = 5, `7` = 6)
    }
    df = rbind(df,sub_df)
  }
  
  #remove color blind participant:
  #df = df[df$participant != "s_061",]
  
  df$phase = ifelse(df$block < 4, "learning", "reversal")
  df$block = paste0("block_",as.character(df$block))
  
  #create measures df: 
  measures = df %>% pivot_wider(id_cols = participant, names_from = c(pair_type, block) ,values_from = c(choice_a, correct))
  
  #calculate measures:
  measures$delta_learning = measures$`choice_a_non-reversed_block_3` - measures$choice_a_reversed_block_3 
  measures$delta_reversal = measures$`choice_a_non-reversed_block_6` - measures$choice_a_reversed_block_6 
  measures$correct_all_learning = measures$`correct_non-reversed_block_3`+measures$correct_reversed_block_3 
  measures$correct_all_reversal = measures$`correct_non-reversed_block_6` + measures$correct_reversed_block_6  
  measures$delta_reversed_learning_reversal =  measures$correct_reversed_block_3 - measures$correct_reversed_block_6
  measures$delta_correct_reversed_block_3_block_4 =  measures$correct_reversed_block_3 - measures$correct_reversed_block_4
  measures$delta_correct_reversed_block_6_block_4 =  measures$correct_reversed_block_6 - measures$correct_reversed_block_4
  measures$delta_non_reversed_learning_reversal =  measures$`correct_non-reversed_block_3` - measures$`correct_non-reversed_block_6`
  measures$correct_all_reversal_4_5_6 = 
    measures$`correct_non-reversed_block_4`+
    measures$`correct_non-reversed_block_5`+
    measures$`correct_non-reversed_block_6`+
    measures$correct_reversed_block_4+
    measures$correct_reversed_block_5+
    measures$correct_reversed_block_6
  
  measures$correct_reversed_reversal_4_5_6 = 
    measures$correct_reversed_block_4+
    measures$correct_reversed_block_5+
    measures$correct_reversed_block_6
  
  
  measures$correct_non_reversed_reversal_4_5_6 = 
    measures$`correct_non-reversed_block_4`+
    measures$`correct_non-reversed_block_5`+
    measures$`correct_non-reversed_block_6` 
  

measures$correct_reversed_reversal_1_2_3 = 
  measures$correct_reversed_block_1+
  measures$correct_reversed_block_2+
  measures$correct_reversed_block_3


measures$correct_non_reversed_reversal_1_2_3 = 
  measures$`correct_non-reversed_block_1`+
  measures$`correct_non-reversed_block_2`+
  measures$`correct_non-reversed_block_3` 
  
  
  
  # label measures: 
  var_label(measures) <- list(
    delta_learning = "choice a non-reversed block 3 minus choice a reversed block 3",
    delta_reversal = "choice a non-reversed block 6 minus choice a reversed block 6",
    correct_all_learning = "correct block 3",
    correct_all_reversal = "correct block 6",
    delta_reversed_learning_reversal = "correct block 3 (reversal) minus correct block 6 (reversal)", 
    delta_correct_reversed_block_3_block_4 = "correct reversed block 3 minus correct reversed block 4", 
    delta_correct_reversed_block_6_block_4 = "correct reversed block 6 minus correct reversed block 4", 
    delta_non_reversed_learning_reversal = "correct non reversed block 3 minus correct non reversed block 6",
    correct_all_reversal_4_5_6 = "all the correct responses in the reversal phase (block 4+5+6)",
    correct_reversed_reversal_4_5_6 = "correct responses of the reversed pair in the reversal phase (block 4+5+6)", 
      correct_non_reversed_reversal_4_5_6 = "correct responses of the non-reversed pair in the reversal phase (block 4+5+6)" ,
    correct_reversed_reversal_1_2_3 = "correct responses of the reversed pair in the learning phase (block 1+2+3)", 
    correct_non_reversed_reversal_1_2_3 = "correct responses of the non-reversed pair in the learning phase (block 1+2+3)" 
    
  )
  
  return(list("measure" = measures, "df" = df))
}


# create data in the format compatible to  use code from "Learning to synchronize: Midfrontal theta dynamics during reversal learning". 
# data is then stored in "G:\Shared drives\AdmonPsy - Chronic Stress Project\RL analysis\code\PieterV_public-master\Reversal_learning\Reversal_Chronic_data" 

create_data_files = function(follow_up = FALSE){
  data_dir = '../data/'
  if (follow_up == TRUE){
    data_dir = '../RL_follow_up/data/'
  }
  files = list.files(data_dir,pattern = "^s_", full.names = TRUE)
  nsubj = length(files)
  
  for(i in 1:nsubj){
    sub_df = read.csv(files[i]) 
    extra_block = na.omit(sub_df$extra_block) 
    sub_df <- sub_df %>% filter(!is.na(trial_num))
    id = unique(sub_df$participant)
    print(paste0('converting subject ', id))
    
    sub_df <- sub_df %>%
      select(Stimulus = pair_type, Response = choice_a, reward) %>%
      mutate(block = rep(1:(nrow(sub_df)/20), each = 20))
    
    if(extra_block == 1){
      sub_df$phase = ifelse(sub_df$block < 5 , "learning", "reversal")
    }else{
      sub_df$phase = ifelse(sub_df$block < 4 , "learning", "reversal")
    }
    sub_df$Rule = 0 
    sub_df$Rule[sub_df$phase == "reversal" & sub_df$Stimulus == "reversed"] = 1

    sub_df$phase <- NULL
    sub_df$block <- NULL
    sub_df$Stimulus = ifelse(sub_df$Stimulus == "reversed", 1,0)
    file_name = paste0('G:/Shared drives/AdmonPsy - Chronic Stress Project/RL analysis/code/PieterV_public-master/Reversal_learning/Reversal_Chronic_data/',id, '_RL_task.csv')
    write.csv(sub_df, file = file_name)
  }
}


# declrative estimation of reward probability 
create_estimation_data = function(data_dir = '../data/', long = TRUE){
  files = list.files(data_dir,pattern = "^s_", full.names = TRUE) # e.g s_001_Reversal_2020_Nov_01_1450.csv
  nsubj = length(files)
  
  df = data.frame()
  
  for(i in 1:length(files)){
    #read sub data file 
    sub_df = read.csv(files[i]) 
    
    # stimuli = c("exp1.png", "exp2.png", "exp3.png", "exp4.png")
    # we have different orders, so we use the following code for stimuli.
    
    # 1 = high probability non reversed in the learning phase
    # 2 = low probability non reversed pair in the learning phase
    # 3 = high probability reversed pair in the learning phase
    # 4 = low probability reversed pair in the learning phase
    
    # for reversed pair: 
    low_prob = sub_df %>% filter(!is.na(trial_num) & pair_type == "reversed") %>% select(low_prob_image_file)
    choice_3  = low_prob$low_prob_image_file[nrow(low_prob)]
    choice_4 = low_prob$low_prob_image_file[1]
    choice_2 = sub_df %>% filter(!is.na(trial_num) & pair_type == "non-reversed") %>% select(low_prob_image_file)
    choice_2= unique(choice_2)$low_prob_image_file
    
    choice_1 = sub_df %>% filter(!is.na(trial_num) & pair_type == "non-reversed") %>% select(high_prob_image_file)
    choice_1= unique(choice_1)$high_prob_image_file
    
    

    # select relevant columns + add "block" variable (block = 20 trials)
    sub_df <- sub_df %>% filter(!is.na(estimation_exp1.png))  %>%
      select(participant, starts_with("estimation")) 
    sub_df$block = 1:nrow(sub_df)
    
    # omit first block for extra block participants: 
    if(max(sub_df$block == 7)){
      sub_df <- sub_df %>% filter(block>1) 
      sub_df$block = sub_df$block-1
    }
    
    colnames(sub_df)<- c("participant", "exp1.png", "exp2.png", "exp3.png", "exp4.png", "block")
    
    #change col names according to coding above
    names(sub_df)[names(sub_df) == choice_1] <- 'choice_1'
    names(sub_df)[names(sub_df) == choice_2] <- 'choice_2'
    names(sub_df)[names(sub_df) == choice_3] <- 'choice_3'
    names(sub_df)[names(sub_df) == choice_4] <- 'choice_4'
    
    df = rbind(df,sub_df)
    
  }
  if (long == T){
    df = pivot_longer(df, cols = starts_with("choice"), names_to = "choice", values_to = "prob_estimation")
  }
  
  return(df)
}



# create data - aggregate sub_files (output of the RL task)
create_data_times = function(data_dir = '../data/'){
  files = list.files(data_dir,pattern = "^s_", full.names = TRUE) # e.g s_001_Reversal_2020_Nov_01_1450.csv
  nsubj = length(files)
  
  df = data.frame()
  
  for(i in 1:length(files)){
    i
    #read sub data file and filter only rows that contain trials (i.e. omit blank rows or rows with no relevant info)
    sub_df = read.csv(files[i]) %>% filter(!is.na(trial_num)) 
    # select relevant columns + add "block" variable (block = 20 trials)
    sub_df <- sub_df %>%
      select(trial_num, participant,pair_type,  choice_a, reward, correct, overall_time_after_trial, total_trial_counter) %>%
      mutate(block = rep(1:(nrow(sub_df)/20), each = 20))
    sub_df$time_trial = c(0,diff(sub_df$overall_time_after_trial))
    
    
    df = rbind(df,sub_df)
    
  }
  
  df$correct = ifelse(df$correct == "correct",1,0)
  return(df)
}



