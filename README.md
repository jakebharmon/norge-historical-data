# JSON to Parquet Validator: Historical Data Pipeline

The system transitions raw AI-extracted data into structured, research-ready Parquet datasets through a human-in-the-loop validation interface. 

---

## Project Architecture

| Directory | Purpose |
| :--- | :--- |
| **`01_pdf`** | Storage for original digitized source documents and scanned historical reports. |
| **`02_json`** | Landing zone for raw AI-extracted JSON data containing initial hierarchical structures. |
| **`03_parquet`** | Final repository for manually-verified datasets, stored in Parquet format. |

---

## Core Components
* **`ai_prompt.txt`**: The placeholder of the prompt given to Gemini in processing the initial pdfs into json files.
* **`validation_shiny_app.R`**: The primary validator tool. It features an integrated processing script that standardizes raw JSON on-the-fly, allowing for dynamic data pivoting and manual row-level editing.
* **`corrections_ledger.csv`**: An automated audit trail that timestamps and logs every manual intervention to ensure the reproducibility of the final dataset.

---

## Features
* **Dynamic Data Pivoting**: Reshape flat JSON extractions into document-accurate "Wide Views" by selecting custom row labels and header categories within the Shiny UI.
* **Interactive Editing**: Built on `rhandsontable`, the app supports direct cell updates and includes right-click context menus for adding or deleting rows and columns.
* **Data Integrity**: Finalized datasets are exported directly to Parquet format, preserving data types and complex structures more effectively than standard CSV files.
* **Audit Transparency**: Every "Manual Review Completed" action is logged to the ledger with a timestamp and reviewer ID.
