library(shiny)
library(rhandsontable)
library(tidyverse)
library(fs)
library(jsonlite)
library(arrow)

# --- CONFIGURATION ---
json_folder    <- "02_json"      
parquet_folder <- "03_parquet"   
ledger_file    <- "corrections_ledger.csv"

# Ensure output directory exists
dir_create(parquet_folder)

# --- UI ---
ui <- fluidPage(
  titlePanel("JSON to Parquet Validator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Select Source File"),
      selectInput("file_select", NULL, choices = NULL),
      
      hr(),
      h4("2. Configure View"),
      selectInput("row_col", "Row Label:", choices = NULL),
      selectInput("header_cols", "Header Columns:", choices = NULL, multiple = TRUE),
      
      hr(),
      h4("3. Actions"),
      actionButton("save_parquet_btn", "Save as Parquet", class = "btn-success"),
      p("Saves the current table state to '03_parquet/'", style = "font-size: 0.8em; color: #666;"),
      br(),
      actionButton("log_edit_btn", "Log Entry to Ledger", class = "btn-warning"),
      p("Records specific cell changes to CSV audit trail.", style = "font-size: 0.8em; color: #666;")
    ),
    
    mainPanel(
      width = 9,
      
      # --- TABS FOR DIFFERENT MODES ---
      tabsetPanel(
        
        # TAB 1: The Standard Table Editor
        tabPanel("Row-by-Row Editor", 
                 br(),
                 rHandsontableOutput("hot_table", height = "600px")
        ),
        
        # TAB 2: The Global Bulk Editor
        tabPanel("Global Cleaner (Bulk Edit)",
                 br(),
                 fluidRow(
                   column(4, 
                          selectInput("bulk_col", "Select Column to Clean:", choices = NULL)
                   ),
                   column(8,
                          p("Instructions: Edit the 'New_Value' column below to fix typos across the entire file. Click 'Apply' to update the main table."),
                          actionButton("apply_bulk_btn", "Apply Global Changes", class = "btn-primary")
                   )
                 ),
                 hr(),
                 rHandsontableOutput("bulk_table", height = "500px")
        )
      ),
      
      hr(),
      verbatimTextOutput("status_msg")
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  values <- reactiveValues(raw_data = NULL, msg = "Ready")
  
  # --- 1. Initialize File List ---
  observe({
    files <- dir_ls(json_folder, glob = "*.json")
    file_names <- path_file(files)
    updateSelectInput(session, "file_select", choices = file_names)
  })
  
  # --- 2. Load and Clean JSON ---
  observeEvent(input$file_select, {
    req(input$file_select)
    file_path <- path(json_folder, input$file_select)
    values$msg <- paste("Loading", input$file_select, "...")
    
    tryCatch({
      raw_text <- read_file(file_path)
      parsed_data <- fromJSON(raw_text, flatten = TRUE)
      
      current_df <- NULL
      if (is.data.frame(parsed_data)) {
        current_df <- as_tibble(parsed_data)
      } else if (is.list(parsed_data)) {
        current_df <- map_dfr(parsed_data, as_tibble)
      }
      
      if (!is.null(current_df)) {
        clean_df <- current_df %>%
          unnest(cols = everything(), names_sep = "_") %>%
          rename_with(~ str_remove(., "metadata_"), starts_with("metadata_")) %>%
          rename_with(~ str_remove(., "data_"), starts_with("data_")) %>%
          mutate(across(everything(), as.character))
        
        values$raw_data <- clean_df
        values$msg <- "File loaded and cleaned."
      }
      
    }, error = function(e) {
      values$msg <- paste("Error loading JSON:", e$message)
      values$raw_data <- NULL
    })
  })
  
  # --- 3. Dynamic Dropdowns ---
  observeEvent(values$raw_data, {
    req(values$raw_data)
    cols <- names(values$raw_data)
    valid_cols <- setdiff(cols, c("value", "note_symbol"))
    
    # Update Standard View Dropdowns
    updateSelectInput(session, "row_col", choices = valid_cols, selected = valid_cols[1])
    default_headers <- setdiff(valid_cols, valid_cols[1])
    updateSelectInput(session, "header_cols", choices = valid_cols, selected = default_headers)
    
    # Update Bulk Cleaner Dropdown
    updateSelectInput(session, "bulk_col", choices = valid_cols, selected = valid_cols[1])
  })
  
  # --- 4. Main View Logic (Row Editor) ---
  wide_view_data <- reactive({
    req(values$raw_data, input$row_col, input$header_cols)
    tryCatch({
      wide_df <- values$raw_data %>%
        mutate(across(all_of(input$header_cols), ~replace_na(., "Unknown"))) %>%
        unite("col_header", all_of(input$header_cols), sep = " | ") %>%
        select(all_of(input$row_col), col_header, value) %>%
        pivot_wider(
          names_from = col_header, 
          values_from = value, 
          values_fn = first, 
          values_fill = ""
        )
      names(wide_df) <- make.unique(names(wide_df))
      wide_df
    }, error = function(e) { NULL })
  })
  
  output$hot_table <- renderRHandsontable({
    df <- wide_view_data()
    if(is.null(df)) return(NULL)
    
    target_col <- input$row_col
    custom_renderer <- paste0("
       function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          if (instance.colToProp(col) === '", target_col, "') {
              td.style.background = '#eee';
              td.style.fontWeight = 'bold';
          }
       }")
    
    rhandsontable(df, rowHeaders = NULL, height = 600, stretchH = "all") %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE) %>% 
      hot_col(col = input$row_col, readOnly = TRUE) %>% 
      hot_cols(renderer = custom_renderer)
  })
  
  # --- 5. Global Cleaner Logic (Bulk Editor) ---
  
  # Generate unique values for the selected column
  output$bulk_table <- renderRHandsontable({
    req(values$raw_data, input$bulk_col)
    
    # Get distinct values
    uniques <- unique(values$raw_data[[input$bulk_col]])
    uniques <- uniques[!is.na(uniques)]
    
    # Create a mapping table: Original -> New
    df_map <- data.frame(
      Original_Value = uniques,
      New_Value = uniques, # Default to same
      stringsAsFactors = FALSE
    ) %>% arrange(Original_Value)
    
    rhandsontable(df_map, rowHeaders = NULL, height = 500, stretchH = "all") %>%
      hot_col("Original_Value", readOnly = TRUE, renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            td.style.background = '#eee';
            td.style.color = '#666';
         }") %>%
      hot_col("New_Value", renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            if (value !== instance.getDataAtRowProp(row, 'Original_Value')) {
               td.style.fontWeight = 'bold';
               td.style.color = 'blue';
            }
         }")
  })
  
  # Apply Bulk Changes
  observeEvent(input$apply_bulk_btn, {
    req(input$bulk_table, input$bulk_col)
    
    # 1. Get the mapping table from UI
    corrections <- hot_to_r(input$bulk_table)
    
    # 2. Filter only changed rows
    changes <- corrections %>% filter(Original_Value != New_Value)
    
    if(nrow(changes) > 0) {
      # 3. Apply changes to raw_data
      # We use match() to find positions and replace
      current_col_values <- values$raw_data[[input$bulk_col]]
      
      # Loop through changes (simple and robust for moderate data sizes)
      for(i in 1:nrow(changes)) {
        old_val <- changes$Original_Value[i]
        new_val <- changes$New_Value[i]
        
        # Replace in memory
        current_col_values[current_col_values == old_val] <- new_val
        
        # Log to ledger
        entry <- tibble(
          timestamp = Sys.time(),
          source_file = input$file_select,
          action = "Global Bulk Edit",
          column = input$bulk_col,
          old_value = old_val,
          new_value = new_val,
          reviewer = Sys.getenv("USER")
        )
        write_csv(entry, ledger_file, append = file_exists(ledger_file))
      }
      
      # 4. Update the reactive value (triggers UI refresh)
      values$raw_data[[input$bulk_col]] <- current_col_values
      values$msg <- paste("Updated", nrow(changes), "values globally.")
      
    } else {
      values$msg <- "No changes detected in Bulk Editor."
    }
  })
  
  # --- 6. Save Actions ---
  observeEvent(input$save_parquet_btn, {
    req(input$hot_table)
    # Note: We save the 'wide view' (the visual table) to Parquet
    final_df <- hot_to_r(input$hot_table)
    out_name <- path_file(input$file_select) %>% path_ext_set("parquet")
    out_path <- path(parquet_folder, out_name)
    write_parquet(final_df, out_path)
    values$msg <- paste("Saved successfully to:", out_path)
  })
  
  observeEvent(input$log_edit_btn, {
    entry <- tibble(
      timestamp = Sys.time(),
      source_file = input$file_select,
      action = "Manual Review Completed",
      reviewer = Sys.getenv("USER")
    )
    write_csv(entry, ledger_file, append = file_exists(ledger_file))
    values$msg <- "Review logged to ledger."
  })
  
  output$status_msg <- renderText({ values$msg })
}

shinyApp(ui, server)