## ----------------------------------------------------------------------
## 06_estimation_etl.R
## ----------------------------------------------------------------------
## Builds the combined estimation dataset:
##   • Fibro + Healthy           (files that start with “sub_”)
##   • Healthy-Stress            (files that start with “s_”)
##
## Prerequisites (already created in earlier scripts):
##   • LOCAL_DATA_DIR      – where the “sub_*.csv” files live
##   • STRESS_DATA_DIR     – where the “s_*.csv”   files live
##   • full_trial_data     – trial-level tibble from 04_trial_merge_clean.R
##   • all_non_learners    – vector from 04_trial_merge_clean.R
## ----------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr);  library(tidyr);  library(stringr);  library(purrr)
})

# ----------------------------------------------------------------------
# 1.  Helper – read ONE “sub_*.csv” and return estimation rows
# ----------------------------------------------------------------------
process_single_csv_file_for_estimation_data <- function(file_path) {
  sub_df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Bail out if the column we need doesn’t exist
  if (!"estimation_rating.response" %in% names(sub_df)) return(tibble())
  
  # Keep rows that contain an estimation response
  if (nrow(sub_df) == 0) return(tibble())
  
  sub_df %>%
    filter(!is.na(estimation_rating.response),
           estimation_rating.response != "") %>%
    transmute(
      participant         = participant,
      image_file          = image_file,
      estimation_response = estimation_rating.response
    ) %>%
    mutate(
      block = rep(1:ceiling(n() / 4), each = 4)[1:n()],
      estimation_response = recode(estimation_response, "None" = "50")
    ) %>%
    # Drop practice block if exactly 7 blocks exist
    {
      if (max(.$block) == 7)
        dplyr::mutate(dplyr::filter(., block != 1), block = block - 1)
      else .
    }
}

# ----------------------------------------------------------------------
# 2.  Read ALL “sub_*.csv” (Fibro + Healthy)
# ----------------------------------------------------------------------
get_estimation_data_from_all_csvs <- function(data_dir = LOCAL_DATA_DIR) {
  files <- list.files(data_dir, "^sub_.*\\.csv$", full.names = TRUE)
  
  map_dfr(files, function(f) {
    if (file.info(f)$size == 0) {
      message("Filtered dataframe is empty for file: ", f)
      return(tibble())
    }
    process_single_csv_file_for_estimation_data(f)
  })
}

estimation_data_df_fibro_healthy <- get_estimation_data_from_all_csvs()

# ----------------------------------------------------------------------
# 3.  Attach pair-type & high/low probe flags for Fibro + Healthy
# ----------------------------------------------------------------------
image_per_block_description <- full_trial_data %>%
  distinct(participant, block, pair_type,
           high_prob_image_file, low_prob_image_file)

estimation_with_choices_fibro_healthy <- estimation_data_df_fibro_healthy %>%
  left_join(image_per_block_description,
            by = c("participant", "block",
                   "image_file" = "high_prob_image_file")) %>%
  left_join(image_per_block_description,
            by = c("participant", "block",
                   "image_file" = "low_prob_image_file")) %>%
  group_by(participant, image_file, estimation_response, block) %>%
  summarise(
    pair_type     = coalesce(first(pair_type.x), first(pair_type.y)),
    is_high_probe = is.na(first(high_prob_image_file)),
    .groups = "drop"
  ) %>%
  mutate(
    estimation_response = as.numeric(estimation_response)
  ) %>%
  left_join(full_trial_data %>% distinct(participant, group),
            by = "participant")

# ----------------------------------------------------------------------
# 4.  Helper – read ALL “s_*.csv” (Healthy-Stress)
# ----------------------------------------------------------------------
create_estimation_data <- function(data_dir = STRESS_DATA_DIR, long = TRUE) {
  files <- list.files(data_dir, "^s_.*\\.csv$", full.names = TRUE)
  df <- tibble()
  
  for (file in files) {
    if (file.info(file)$size == 0) {
      message("Filtered dataframe is empty for file: ", file)
      next
    }
    
    sub_df <- read.csv(file, stringsAsFactors = FALSE)
    if (nrow(sub_df) == 0) next
    
    # ----- Map image filenames to choice_1 … choice_4 ------------------
    low_prob_rev <- sub_df %>%
      filter(!is.na(trial_num), pair_type == "reversed") %>%
      pull(low_prob_image_file)
    
    choice_3 <- low_prob_rev[length(low_prob_rev)]
    choice_4 <- low_prob_rev[1]
    
    choice_2 <- sub_df %>%
      filter(!is.na(trial_num), pair_type == "non-reversed") %>%
      pull(low_prob_image_file) %>% unique()
    
    choice_1 <- sub_df %>%
      filter(!is.na(trial_num), pair_type == "non-reversed") %>%
      pull(high_prob_image_file) %>% unique()
    
    # ----- Keep only estimation columns --------------------------------
    sub_df <- sub_df %>%
      filter(!is.na(estimation_exp1.png)) %>%
      select(participant, starts_with("estimation"))
    
    sub_df$block <- seq_len(nrow(sub_df))
    
    if (max(sub_df$block) == 7) {
      sub_df <- sub_df %>% filter(block > 1)
      sub_df$block <- sub_df$block - 1
    }
    
    names(sub_df) <- c("participant", "exp1.png", "exp2.png",
                       "exp3.png", "exp4.png", "block")
    
    names(sub_df)[names(sub_df) == choice_1] <- "choice_1"
    names(sub_df)[names(sub_df) == choice_2] <- "choice_2"
    names(sub_df)[names(sub_df) == choice_3] <- "choice_3"
    names(sub_df)[names(sub_df) == choice_4] <- "choice_4"
    
    df <- bind_rows(df, sub_df)
  }
    if (long) {
    # Check if there are any 'choice' columns before pivoting
    choice_cols <- grep("^choice", names(df), value = TRUE)
    if (length(choice_cols) > 0) {
      df <- df %>%
        pivot_longer(
          cols      = all_of(choice_cols),
          names_to  = "choice",
          values_to = "prob_estimation"
        )
    } else {
      message("Warning: No 'choice' columns found for pivot_longer")
      # Return empty tibble with expected structure if no choice columns
      if (nrow(df) > 0) {
        df$choice <- NA
        df$prob_estimation <- NA
      } else {
        df <- tibble(participant = character(), block = numeric(), 
                    choice = character(), prob_estimation = numeric())
      }
    }
  }
  
  df
}

stress_estimation_df <- create_estimation_data()

# ----------------------------------------------------------------------
# 5.  Attach pair-type & high/low probe flags for Healthy-Stress
# ----------------------------------------------------------------------
estimation_with_choices_stress <- stress_estimation_df %>%
  mutate(
    pair_type = if_else(choice %in% c("choice_1", "choice_2"),
                        "non-reversed", "reversed"),
    is_high_probe       = choice %in% c("choice_1", "choice_3"),
    estimation_response = as.numeric(prob_estimation),
    group               = "Healthy Stress"
  ) %>%
  select(participant, block, estimation_response,
         pair_type, is_high_probe, group)

# ----------------------------------------------------------------------
# 6.  Combine both datasets and clean
# ----------------------------------------------------------------------
full_estimation_data <- bind_rows(
  estimation_with_choices_fibro_healthy,
  estimation_with_choices_stress
)

participant_variability <- full_estimation_data %>%
  group_by(participant) %>%
  summarise(sd_estimation = sd(estimation_response, na.rm = TRUE),
            .groups = "drop")

low_var_participants <- participant_variability %>%
  filter(sd_estimation < 1) %>% pull(participant)

full_estimation_data_clean <- full_estimation_data %>%
  filter(
    !(participant %in% low_var_participants),
    !(participant %in% all_non_learners)
  )

## ----------------------------------------------------------------------
## OBJECTS CREATED FOR THE NEXT SCRIPT:
##   • full_estimation_data
##   • full_estimation_data_clean
## ----------------------------------------------------------------------
