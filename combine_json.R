library(jsonlite)
library(tidyverse)
library(stringr)

# 1. Define function with improved year handling (fixes 1923 metadata error)
process_asylum_json <- function(file_path) {
  raw_json <- fromJSON(file_path, simplifyVector = FALSE)
  
  # FIX: Prioritize filename for year because 1923 metadata is mislabeled as 1922
  record_year <- as.integer(str_extract(basename(file_path), "\\d{4}"))
  
  clean_df <- map_dfr(raw_json$data_skeleton, function(x) {
    tibble(
      year = record_year,
      institution = as.character(x$institution %||% NA_character_),
      category = as.character(x$category %||% NA_character_),
      patient_status = as.character(x$patient_status %||% NA_character_),
      sex = as.character(x$sex %||% NA_character_),
      value = if (is.null(x$cell_data$value)) NA_real_ else as.numeric(x$cell_data$value),
      footnote_marker = if (is.null(x$cell_data$footnote_marker)) NA_character_ else as.character(x$cell_data$footnote_marker)
    )
  })
  
  return(clean_df)
}

# 2. Load files and EXCLUDE 1913 (Census table error)
json_files <- list.files(path = "03_json", pattern = "\\.json$", full.names = TRUE)
json_files <- json_files[!grepl("1913", json_files)] # Exclude 1913

# 3. Process all files
all_years_data <- map_dfr(json_files, process_asylum_json)

# 4. Aggressive Cleaning and Filtering
cleaned_data <- all_years_data %>%
  # ==========================================
# STEP A: FILTER OUT AGGREGATES (Fixes Overcounting)
# ==========================================
# Remove "Tilsammen" and "Samtlige" rows and "Tils." sex columns
filter(!grepl("Tilsammen|Samtlige|Samtilge|Total", institution, ignore.case = TRUE)) %>%
  filter(!sex %in% c("Tils.", "Tilsammen", "Total")) %>%
  
  # Remove hierarchical parent rows like "Insania periodica" when sub-types exist
  # and combined categories like "Melancholia / Mania" found in 1907
  filter(!patient_status %in% c("Insania periodica", "Melancholia / Mania", "Melancholia & Mania")) %>%
  
  mutate(
    # ==========================================
    # STEP B: CLEAN INSTITUTIONS
    # ==========================================
    institution = str_squish(institution),
    institution = case_when(
      str_detect(institution, "(?i)blakst") ~ "Blakstad",
      str_detect(institution, "(?i)bratsberg") ~ "Bratsberg",
      str_detect(institution, "(?i)christians|kistiansand|kristiansand") ~ "Kristiansand",
      str_detect(institution, "(?i)kriminal") ~ "Kriminalasylet",
      str_detect(institution, "(?i)m[eoø]ll?en-?dal") ~ "Møllendal",
      str_detect(institution, "(?i)n[ee]+veng[aå]rden|newengaarden|neevegarden") ~ "Neevengården",
      str_detect(institution, "(?i)opd[eoø]l") ~ "Opdøl",
      str_detect(institution, "(?i)prests[aeæ]ter") ~ "Prestsæter",
      str_detect(institution, "(?i)r[eoø]nvik") ~ "Rønvik",
      str_detect(institution, "(?i)rosenberg") ~ "Rosenberg",
      str_detect(institution, "(?i)rotvol") ~ "Rotvoll",
      str_detect(institution, "(?i)sauderud") ~ "Sanderud",
      str_detect(institution, "(?i)t[ro]ndhjem") ~ "Trondhjem",
      str_detect(institution, "(?i)[oø]stmark") ~ "Østmarken",
      TRUE ~ institution
    ),
    
    # ==========================================
    # STEP C: CLEAN PATIENT STATUS (Punctuation Soup)
    # ==========================================
    # Remove ALL trailing punctuation and periods
    patient_status = str_remove_all(patient_status, "[[:punct:]]+$"),
    patient_status = str_squish(patient_status),
    
    patient_status = case_when(
      str_detect(patient_status, "(?i)^alch?olismus") ~ "Alcoholismus", 
      str_detect(patient_status, "(?i)idiotia \\(imbecillitas\\)") ~ "Idiotia (imbecillitas)",
      str_detect(patient_status, "(?i)idiotismus -? ?acq[u]?isitus") ~ "Idiotismus - acquisitus",
      str_detect(patient_status, "(?i)til observas|til observat|observation") ~ "Til observasjon",
      str_detect(patient_status, "(?i)vitia org.*cer") ~ "Vitia organica cerebri",
      str_detect(patient_status, "(?i)melancholia -? ?cum ?stup") ~ "Melancholia cum stupore",
      str_detect(patient_status, "(?i)ikke sin[nd]ssyk") ~ "Ikke sinnssyk",
      str_detect(patient_status, "(?i)hypocondria") ~ "Hypochondria",
      TRUE ~ patient_status
    )
  )

# Final validation print
cat("\n--- FINAL AUDIT ---")
cat("\nTotal Patients in Cleaned Dataset:", sum(cleaned_data$value, na.rm = TRUE))
cat("\nUnique Institutions:", length(unique(cleaned_data$institution)))
cat("\nUnique Diagnoses:", length(unique(cleaned_data$patient_status)))

write.csv(cleaned_data, "cleaned_long_dataset.csv")