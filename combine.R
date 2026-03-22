library(jsonlite)
library(tidyverse)
library(stringr)

# ==========================================
# REFERENCE TOTALS (for final audit)
# ==========================================
reference_totals <- tibble(
  year = 1872:1929,
  pdf_total = c(
    550, 561, 547, 570, 638, 598, 527, 520, 571, 700,
    715, 707, 761, 690, 690, 703, 750, 756, 795, 827,
    792, 810, 839, 771, 735, 767, 757, 768, 777, 858,
    1052, 932, 957, 1122, 994, 934, 1154, 1086, 1138, 1038,
    1112, 1427, 1537, 1399, 1392, 1187, 1206, 1537, 1275, 1271,
    1271, 1426, 1499, 1532, 2252, 1815, 1767, 1940
  )
)

# ==========================================
# LOAD FROM 03_json
# ==========================================
parse_json <- function(file_path) {
  raw_json    <- fromJSON(file_path, simplifyVector = FALSE)
  record_year <- as.integer(gsub("[^0-9]", "", basename(file_path)))

  map_dfr(raw_json$data_skeleton, function(x) {
    tibble(
      year           = record_year,
      institution    = as.character(x$institution    %||% NA_character_),
      patient_status = as.character(x$patient_status %||% NA_character_),
      sex            = as.character(x$sex            %||% NA_character_),
      value          = { v  <- x$cell_data$value
                         if (is.null(v)  || length(v)  == 0) NA_real_      else as.numeric(v) },
      footnote_marker= { fm <- x$cell_data$footnote_marker
                         if (is.null(fm) || length(fm) == 0) NA_character_ else as.character(fm) }
    )
  })
}

json_files <- list.files("03_json", pattern = "\\.json$", full.names = TRUE)
raw_data   <- map_dfr(json_files, parse_json)

# ==========================================
# FILTER AGGREGATES + STANDARDIZE NAMES
# ==========================================
cleaned_data <- raw_data %>%
  filter(!grepl("Tilsammen|Samtlige|Samtilge|Total", institution, ignore.case = TRUE)) %>%
  filter(!sex %in% c("Tils.", "Tilsammen", "Total")) %>%
  mutate(
    institution = str_squish(institution),
    institution = case_when(
      str_detect(institution, "(?i)blakst")                                  ~ "Blakstad",
      str_detect(institution, "(?i)bratsberg")                               ~ "Bratsberg",
      str_detect(institution, "(?i)christians|k[ir]istian-?sand")            ~ "Kristiansand",
      str_detect(institution, "(?i)krim[mi]+nnal-?asylet|kriminal-?asylet")  ~ "Kriminalasylet",
      str_detect(institution, "(?i)m[eoø]ll?en-?d[da]l")                    ~ "Møllendal",
      str_detect(institution, "(?i)n[ee]+veng[aå]rden|newengaarden|neevegarden") ~ "Neevengården",
      str_detect(institution, "(?i)opd[eoø]l")                              ~ "Opdøl",
      str_detect(institution, "(?i)prests[aeæ]ter|presteseter")              ~ "Prestsæter",
      str_detect(institution, "(?i)r[eoø]nvik")                             ~ "Rønvik",
      str_detect(institution, "(?i)rosenberg")                              ~ "Rosenberg",
      str_detect(institution, "(?i)rotvol")                                 ~ "Rotvoll",
      str_detect(institution, "(?i)sauderud|sanderud")                      ~ "Sanderud",
      str_detect(institution, "(?i)t[ro]ndhjems?")                          ~ "Trondhjem",
      str_detect(institution, "(?i)[oø]stmark")                             ~ "Østmarken",
      str_detect(institution, "(?i)fastings? minde|fastingsminde")           ~ "Fastings Minde",
      str_detect(institution, "(?i)kristiania|christiania")                  ~ "Kristiania",
      TRUE ~ institution
    ),
    patient_status = str_remove_all(patient_status, "[[:punct:]]+$"),
    patient_status = str_squish(patient_status),
    patient_status = case_when(
      str_detect(patient_status, "(?i)^alch?olismus")                        ~ "Alcoholismus",
      str_detect(patient_status, "(?i)idiotia \\(imbecillitas\\)")            ~ "Idiotia (imbecillitas)",
      str_detect(patient_status, "(?i)idiotismus -? ?acq[u]?isitus")          ~ "Idiotismus - acquisitus",
      str_detect(patient_status, "(?i)til observas|til observat|observation") ~ "Til observasjon",
      str_detect(patient_status, "(?i)vitia org.*cer")                        ~ "Vitia organica cerebri",
      str_detect(patient_status, "(?i)melancholia -? ?cum ?stup")             ~ "Melancholia cum stupore",
      str_detect(patient_status, "(?i)ikke sin[nd]ssyk")                      ~ "Ikke sinnssyk",
      str_detect(patient_status, "(?i)hypocondria")                           ~ "Hypochondria",
      TRUE ~ patient_status
    )
  ) %>%
  group_by(year, institution, patient_status, sex) %>%
  summarise(
    value           = if (all(is.na(value))) NA_real_ else sum(value, na.rm = TRUE),
    footnote_marker = first(na.omit(footnote_marker)),
    .groups = "drop"
  )

# ==========================================
# FINAL AUDIT
# ==========================================
audit <- cleaned_data %>%
  group_by(year) %>%
  summarise(computed_total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  full_join(reference_totals, by = "year") %>%
  mutate(difference = computed_total - pdf_total, match = difference == 0) %>%
  arrange(year)

cat("=== FINAL AUDIT ===\n")
print(audit, n = Inf)
cat("\nYears matching:", sum(audit$match, na.rm = TRUE), "/", sum(!is.na(audit$computed_total)), "\n")

failures <- filter(audit, !match | is.na(match))
if (nrow(failures) == 0) {
  cat("\nALL YEARS MATCH. Exporting final dataset...\n")
} else {
  cat("\nWARNING — mismatches found. Check before using:\n")
  print(failures)
}

# ==========================================
# EXPORT
# ==========================================
write_csv(cleaned_data, "cleaned_long_dataset.csv")
cat("Exported cleaned_long_dataset.csv\n")
cat("Rows:", nrow(cleaned_data), "\n")
cat("Years:", paste(sort(unique(cleaned_data$year)), collapse=", "), "\n")
cat("Institutions:", length(unique(cleaned_data$institution)), "\n")
cat("Diagnoses:", length(unique(cleaned_data$patient_status)), "\n")
