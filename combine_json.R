library(jsonlite)
library(tidyverse)

# 1. Define a function to process a single JSON file
process_asylum_json <- function(file_path) {
  
  # Read the JSON safely as a list
  raw_json <- fromJSON(file_path, simplifyVector = FALSE)
  
  # Extract the year from metadata and FORCE it to be an integer
  record_year <- as.integer(raw_json$metadata$year)
  
  # Map over the data_skeleton and build a clean tibble
  clean_df <- map_dfr(raw_json$data_skeleton, function(x) {
    tibble(
      year = record_year,
      institution = x$institution,
      category = x$category,
      patient_status = x$patient_status,
      sex = x$sex,
      
      # Safely extract cell_data, converting NULL to NA
      value = if (is.null(x$cell_data$value)) NA_real_ else as.numeric(x$cell_data$value),
      footnote_marker = if (is.null(x$cell_data$footnote_marker)) NA_character_ else as.character(x$cell_data$footnote_marker)
    )
  })
  
  return(clean_df)
}

# 2. Get a list of all your JSON files
json_files <- list.files(path = "03_json", pattern = "\\.json$", full.names = TRUE)

# 3. Process all files and combine them into one massive long dataframe
all_years_data <- map_dfr(json_files, process_asylum_json)


#################### CHECK ########################
## Read head of data
head(all_years_data)

# 1. Check Unique Institutions
institution_check <- all_years_data %>%
  count(institution, name = "total_records") %>%
  arrange(institution) # Sort alphabetically to spot near-matches easily

print("--- UNIQUE INSTITUTIONS ---")
print(institution_check, n = Inf) # n = Inf forces it to print all rows


# 2. Check Unique Diagnoses (Patient Status)
status_check <- all_years_data %>%
  count(patient_status, name = "total_records") %>%
  arrange(patient_status)

print("--- UNIQUE PATIENT STATUSES ---")
print(status_check, n = Inf)