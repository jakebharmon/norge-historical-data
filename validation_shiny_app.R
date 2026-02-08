library(shiny)
library(tidyverse)
library(rhandsontable)
library(jsonlite)

# --- SETUP: DIRECTORY CREATION ---
if (!dir.exists("03_json")) {
  dir.create("03_json")
}

# --- UI ---
ui <- fluidPage(
  theme = NULL, 
  titlePanel("JSON Editor: JSON-Only Pathway"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3, 
      h4("1. Import"),
      fileInput("upload_file", "Upload File (.json)", accept = ".json"),
      br(),
      h4("2. Export"),
      actionButton("save_server", "Save to /03_json", icon = icon("save"), class = "btn-success btn-block"),
      hr(),
      helpText("Tip: Upload the 'Dimensions JSON' from the AI prompt to start a new grid, or a previously saved JSON to resume editing.")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(id = "main_tabs",
                  
                  # TAB 1: DATA
                  tabPanel("1. Data Values", 
                           br(),
                           rHandsontableOutput("value_grid", height = "750px")
                  ),
                  
                  # TAB 2: FOOTNOTES
                  tabPanel("2. Footnotes", 
                           br(),
                           rHandsontableOutput("note_grid", height = "750px")
                  ),
                  
                  # TAB 3: RENAMER
                  tabPanel("3. Global Dimensions",
                           br(),
                           fluidRow(
                             column(6, 
                                    div(class = "panel panel-default",
                                        div(class = "panel-heading", strong("Institutions")),
                                        div(class = "panel-body", rHandsontableOutput("edit_institutions"))
                                    )
                             ),
                             column(6, 
                                    div(class = "panel panel-default",
                                        div(class = "panel-heading", strong("Diagnoses")),
                                        div(class = "panel-body", rHandsontableOutput("edit_diagnoses"))
                                    )
                             )
                           )
                  ),
                  
                  # TAB 4: PREVIEW
                  tabPanel("4. JSON Preview", 
                           br(),
                           verbatimTextOutput("json_preview")
                  )
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # --- 1. STATE ---
  values <- reactiveValues(
    metadata = list(),
    df = NULL,          
    id_map = NULL,
    filename = "output"
  )
  
  # --- 2. LOAD JSON FILES ---
  observeEvent(input$upload_file, {
    req(input$upload_file)
    
    file_path <- input$upload_file$datapath
    ext <- tools::file_ext(file_path)
    values$filename <- tools::file_path_sans_ext(input$upload_file$name)
    
    if (ext != "json") {
      showNotification("Error: Invalid file type. Please upload a .json file.", type = "error")
      return()
    }
    
    tryCatch({
      in_json <- fromJSON(file_path, simplifyVector = FALSE)
      
      # PATH A: DIMENSIONS JSON (Auto-Expand)
      if (is.null(in_json$data_skeleton) && !is.null(in_json$institutions)) {
        values$metadata <- list(title = in_json$title, year = in_json$year)
        
        # Ensure factor levels for correct sorting later
        inst_levels <- unlist(in_json$institutions)
        sex_levels <- unlist(in_json$sex)
        
        full_grid <- expand.grid(
          institution = inst_levels,
          patient_status = unlist(in_json$diagnoses),
          sex = sex_levels,
          stringsAsFactors = FALSE
        )
        
        flat_df <- full_grid %>%
          mutate(
            category = in_json$title,
            value = NA_real_,
            footnote_marker = "",
            id = row_number(),
            # Create a sorting key to preserve order: Institution -> Sex
            sort_key = as.integer(factor(institution, levels = inst_levels)) * 100 + 
              as.integer(factor(sex, levels = sex_levels))
          ) %>%
          arrange(sort_key)
        
        values$df <- flat_df
        update_maps()
        showNotification("Dimensions JSON expanded successfully!", type = "message")
        
        # PATH B: FULL PROCESSED JSON (Resume Work)
      } else {
        values$metadata <- in_json$metadata
        
        # Extract original order from metadata if available, otherwise unique appearance
        inst_levels <- unique(unlist(lapply(in_json$data_skeleton, function(x) x$institution)))
        sex_levels <- unique(unlist(lapply(in_json$data_skeleton, function(x) x$sex)))
        
        flat_df <- map_dfr(in_json$data_skeleton, function(x) {
          tibble(
            patient_status = x$patient_status,
            institution = x$institution,
            sex = x$sex,
            category = x$category, 
            value = if(is.null(x$cell_data$value)) NA_real_ else as.numeric(x$cell_data$value),
            footnote_marker = if(is.null(x$cell_data$footnote_marker)) "" else x$cell_data$footnote_marker
          )
        }) %>% mutate(id = row_number())
        
        values$df <- flat_df
        update_maps()
        showNotification("Full JSON loaded successfully!", type = "message")
      }
      
    }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
  })
  
  # --- HELPER: MAP UPDATE ---
  update_maps <- function() {
    req(values$df)
    if(!all(c("institution", "sex") %in% names(values$df))) return()
    
    long <- values$df %>% mutate(col_key = paste(institution, sex, sep = " | "))
    wid <- long %>% select(patient_status, col_key, id) %>% 
      pivot_wider(names_from = col_key, values_from = id) %>% column_to_rownames("patient_status")
    values$id_map <- as.matrix(wid)
  }
  
  # --- HELPER: GET SORTED COLUMNS ---
  get_sorted_col_names <- function(df) {
    # Get unique institutions and sexes preserving their order in the dataframe
    # This assumes the dataframe was sorted upon loading/creation
    insts <- unique(df$institution)
    sexes <- unique(df$sex)
    
    # Create the desired column order: Inst1|Sex1, Inst1|Sex2, Inst2|Sex1...
    col_order <- c()
    for(i in insts) {
      for(s in sexes) {
        col_order <- c(col_order, paste(i, s, sep = " | "))
      }
    }
    return(col_order)
  }
  
  # --- 3. DATA MATRICES ---
  get_val_matrix <- reactive({
    req(values$df)
    if(!all(c("institution", "sex") %in% names(values$df))) return(NULL)
    
    long <- values$df %>% mutate(col_key = paste(institution, sex, sep = " | "))
    
    # pivot_wider will sort alphabetically by default, so we must reorder manually
    mat <- long %>% 
      select(patient_status, col_key, value) %>% 
      pivot_wider(names_from = col_key, values_from = value) %>% 
      column_to_rownames("patient_status")
    
    # Reorder columns based on Institution -> Sex hierarchy
    desired_order <- get_sorted_col_names(values$df)
    # Filter to only columns that actually exist in the matrix (safety)
    existing_cols <- intersect(desired_order, names(mat))
    mat <- mat[, existing_cols, drop = FALSE]
    
    as.data.frame(mat) %>% rownames_to_column("Diagnosis")
  })
  
  get_note_matrix <- reactive({
    req(values$df)
    if(!all(c("institution", "sex") %in% names(values$df))) return(NULL)
    
    long <- values$df %>% mutate(col_key = paste(institution, sex, sep = " | "))
    mat <- long %>% 
      select(patient_status, col_key, footnote_marker) %>% 
      pivot_wider(names_from = col_key, values_from = footnote_marker) %>% 
      column_to_rownames("patient_status")
    
    desired_order <- get_sorted_col_names(values$df)
    existing_cols <- intersect(desired_order, names(mat))
    mat <- mat[, existing_cols, drop = FALSE]
    
    as.data.frame(mat) %>% rownames_to_column("Diagnosis")
  })
  
  # --- 4. RENDER GRIDS ---
  output$value_grid <- renderRHandsontable({
    df <- get_val_matrix()
    req(df)
    
    hot <- rhandsontable(df, rowHeaders = TRUE, stretchH = "all") %>%
      hot_col("Diagnosis", readOnly = TRUE) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      # [FIX 1] Force integer-like display for numbers (no .00)
      hot_cols(renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          if (value !== null && value !== undefined && !isNaN(value)) {
             // If it's a whole number, show no decimals. Otherwise show default.
             if (value % 1 === 0) {
                td.innerHTML = value; 
             }
          }
        }")
    
    note_data <- values$df %>% filter(!is.na(footnote_marker) & footnote_marker != "")
    if(nrow(note_data) > 0 && !is.null(values$id_map)) {
      for(i in 1:nrow(note_data)) {
        target_id <- note_data$id[i]
        coords <- which(values$id_map == target_id, arr.ind = TRUE)
        if(length(coords) > 0) {
          # Use col_key to find the correct column index in the reordered matrix
          # This is tricky because id_map might not match the visual order anymore
          # Simplification: We rely on the column name matching the col_key
          
          # We need to map the ID to the specific visual column name
          rec <- values$df[values$df$id == target_id, ]
          col_name <- paste(rec$institution, rec$sex, sep = " | ")
          
          # Find column index in the displayed dataframe 'df'
          # col_index in 'df' (1-based)
          vis_col_idx <- which(names(df) == col_name)
          
          if(length(vis_col_idx) > 0) {
            hot <- hot %>% hot_cell(row = coords[1], col = vis_col_idx - 1, comment = note_data$footnote_marker[i])
          }
        }
      }
    }
    hot
  })
  
  output$note_grid <- renderRHandsontable({
    df <- get_note_matrix()
    req(df)
    rhandsontable(df, rowHeaders = TRUE, stretchH = "all") %>%
      hot_col("Diagnosis", readOnly = TRUE) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_cols(renderer = "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          if (value != null && value != '') {
            td.style.background = '#fff3cd'; 
            td.style.fontWeight = 'bold';
          }
        }")
  })
  
  # --- 5. HANDLE EDITS ---
  observeEvent(input$value_grid$changes$changes, {
    changes <- input$value_grid$changes$changes
    # We cannot use simple matrix coordinates anymore because columns are reordered.
    # We must map (row_name, col_name) back to the ID.
    
    req(changes, values$df)
    
    # Get current row names (Diagnoses) from the displayed grid
    # (Assuming the row order hasn't changed from get_val_matrix)
    current_display_df <- get_val_matrix()
    row_names <- current_display_df$Diagnosis
    col_names <- names(current_display_df)
    
    for(ch in changes) {
      r_vis <- ch[[1]] + 1 # visual row index (1-based)
      c_vis <- ch[[2]] + 1 # visual col index (1-based)
      new_val <- ch[[4]]
      
      # Column 1 is Diagnosis, skip it
      if(c_vis == 1) next 
      
      target_diagnosis <- row_names[r_vis]
      target_col_key <- col_names[c_vis]
      
      # Parse col_key back to Institution and Sex
      parts <- strsplit(target_col_key, " \\| ")[[1]]
      if(length(parts) != 2) next
      t_inst <- parts[1]
      t_sex <- parts[2]
      
      # Update the main dataframe
      # Find the specific row ID matching Diagnosis, Institution, and Sex
      row_idx <- which(values$df$patient_status == target_diagnosis & 
                         values$df$institution == t_inst & 
                         values$df$sex == t_sex)
      
      if(length(row_idx) == 1) {
        val_clean <- if(is.na(new_val) || new_val == "") NA_real_ else as.numeric(new_val)
        values$df$value[row_idx] <- val_clean
      }
    }
  })
  
  observeEvent(input$note_grid$changes$changes, {
    changes <- input$note_grid$changes$changes
    req(changes, values$df)
    
    current_display_df <- get_note_matrix()
    row_names <- current_display_df$Diagnosis
    col_names <- names(current_display_df)
    
    for(ch in changes) {
      r_vis <- ch[[1]] + 1 
      c_vis <- ch[[2]] + 1 
      new_val <- ch[[4]]
      
      if(c_vis == 1) next 
      
      target_diagnosis <- row_names[r_vis]
      target_col_key <- col_names[c_vis]
      
      parts <- strsplit(target_col_key, " \\| ")[[1]]
      if(length(parts) != 2) next
      t_inst <- parts[1]
      t_sex <- parts[2]
      
      row_idx <- which(values$df$patient_status == target_diagnosis & 
                         values$df$institution == t_inst & 
                         values$df$sex == t_sex)
      
      if(length(row_idx) == 1) {
        val_clean <- if(is.null(new_val) || new_val == "") "" else as.character(new_val)
        values$df$footnote_marker[row_idx] <- val_clean
      }
    }
  })
  
  # --- 6. RENAMERS ---
  output$edit_institutions <- renderRHandsontable({
    req(values$df)
    vec <- unique(values$df$institution)
    rhandsontable(data.frame(Name = vec, stringsAsFactors=F), rowHeaders=TRUE, stretchH = "all") %>% 
      hot_col("Name", allowInvalid=F)
  })
  
  observeEvent(input$edit_institutions, {
    req(values$df)
    raw <- hot_to_r(input$edit_institutions)
    if(is.null(raw)) return()
    new_vec <- raw$Name
    old_vec <- unique(values$df$institution)
    if(length(new_vec) == length(old_vec) && !identical(new_vec, old_vec)) {
      diffs <- which(new_vec != old_vec)
      for(i in diffs) {
        values$df$institution[values$df$institution == old_vec[i]] <- new_vec[i]
      }
      # Re-trigger UI updates implicitly by modifying values$df
      update_maps()
    }
  })
  
  output$edit_diagnoses <- renderRHandsontable({
    req(values$df)
    vec <- unique(values$df$patient_status)
    rhandsontable(data.frame(Name = vec, stringsAsFactors=F), rowHeaders=TRUE, stretchH = "all") %>% 
      hot_col("Name", allowInvalid=F)
  })
  
  observeEvent(input$edit_diagnoses, {
    req(values$df)
    raw <- hot_to_r(input$edit_diagnoses)
    if(is.null(raw)) return()
    new_vec <- raw$Name
    old_vec <- unique(values$df$patient_status)
    if(length(new_vec) == length(old_vec) && !identical(new_vec, old_vec)) {
      diffs <- which(new_vec != old_vec)
      for(i in diffs) {
        values$df$patient_status[values$df$patient_status == old_vec[i]] <- new_vec[i]
      }
      update_maps()
    }
  })
  
  # --- 7. EXPORT LOGIC ---
  reconstruct_json <- reactive({
    req(values$df)
    data_list <- values$df %>%
      rowwise() %>%
      mutate(cell_data = list(list(
        value = if(is.na(value)) NULL else value,
        footnote_marker = if(footnote_marker == "") NULL else footnote_marker
      ))) %>%
      ungroup() %>%
      select(institution, category, patient_status, sex, cell_data) %>%
      apply(1, as.list)
    
    clean_list <- lapply(data_list, function(x) {
      list(institution = x$institution, category = x$category, patient_status = x$patient_status, sex = x$sex, cell_data = x$cell_data)
    })
    
    list(
      metadata = values$metadata,
      dimensions = list(
        institutions = unique(values$df$institution),
        diagnoses = unique(values$df$patient_status),
        sex = unique(values$df$sex)
      ),
      data_skeleton = clean_list
    )
  })
  
  observeEvent(input$save_server, {
    req(values$df)
    out_dir <- file.path(getwd(), "03_json")
    if(!dir.exists(out_dir)) dir.create(out_dir)
    file_name <- paste0(values$filename, "_processed.json")
    full_path <- file.path(out_dir, file_name)
    write_json(reconstruct_json(), full_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
    showNotification(paste("Saved to:", full_path), type = "message", duration = 5)
  })
  
  output$json_preview <- renderText({
    req(values$df)
    toJSON(reconstruct_json(), pretty = TRUE, auto_unbox = TRUE)
  })
}

shinyApp(ui, server)