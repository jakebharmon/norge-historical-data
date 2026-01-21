library(jsonlite)
library(tidyverse)
library(fs)

# --- CONFIGURATION ---
output_folder <- "02_rds" 

# --- EXECUTION ---
json_files <- dir_ls("01_json", glob = "*.json")

if(length(json_files) > 0) {
  
  # Use walk() instead of map_dfr() because we want to perform an action 
  # (saving a file) rather than combining them all into one object.
  walk(json_files, function(file) {
    
    message("Processing: ", path_file(file))
    
    # 1. Read and Parse
    raw_text <- read_file(file)
    parsed_data <- fromJSON(raw_text, flatten = TRUE)
    
    # 2. Standardize to Tibble
    # Initialize an empty tibble to hold the current file's data
    current_df <- NULL
    
    if (is.data.frame(parsed_data)) {
      current_df <- as_tibble(parsed_data)
    } else if (is.list(parsed_data)) {
      current_df <- map_dfr(parsed_data, as_tibble)
    }
    
    # 3. Clean Columns
    # We perform the cleaning on this specific file's data
    if (!is.null(current_df)) {
      clean_df <- current_df %>%
        unnest(cols = everything(), names_sep = "_") %>% 
        rename_with(~ str_remove(., "metadata_"), starts_with("metadata_")) %>%
        rename_with(~ str_remove(., "data_"), starts_with("data_")) %>%
        mutate(across(everything(), as.character))
      
      # 4. Generate Output Filename
      # This takes "json/myfile.json" and changes it to "json/myfile.rds"
      output_path <- file %>%
        path_file() %>%                # Extract just filename (myfile.json)
        path_ext_set("rds") %>%        # Change extension (myfile.rds)
        path(output_folder, .)         # Join with output folder
      
      # 5. Save Individual RDS
      message("Saving to: ", output_path)
      write_rds(clean_df, output_path)
    }
  })
  
  message("Batch processing complete.")
  
} else {
  message("No JSON files found.")
}