## 01_copy_raw_files.R ---------------------------------------------------
## Mirror RL csvs from the shared drive to LOCAL_DATA_DIR

copy_files_from_rl <- function(source_dir, dest_dir) {
  dir_create(dest_dir, recurse = TRUE)
  subdirs      <- dir_ls(source_dir, recurse = TRUE, type = "directory")
  rl_subdirs   <- subdirs[str_detect(subdirs, "/sub_\\d+/RL$")]
  
  for (subdir in rl_subdirs) {
    files <- dir_ls(subdir, type = "file")
    matching <- files[str_detect(path_file(files), "^sub_\\d+_Reversal_.*\\.csv$")]
    file_copy(matching, path(dest_dir, path_file(matching)), overwrite = TRUE)
  }
}

copy_files_from_rl(RAW_SHARED_DIR, LOCAL_DATA_DIR)
